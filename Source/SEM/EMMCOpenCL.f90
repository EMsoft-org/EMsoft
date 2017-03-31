! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this
!        list of conditions and the following disclaimer in the documentation and/or
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names
!        of its contributors may be used to endorse or promote products derived from
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

program EMMCOpenCL

use local
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use json_module
use io

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(MCCLNameListType)                  :: mcnl
integer(kind=irg)                       :: res, error_cnt

nmldeffile = 'EMMCOpenCL.nml'
progname = 'EMMCOpenCL.f90'
progdesc = 'Monte Carlo backscattered electron simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 42 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call JSONreadMCCLNameList(mcnl, nmldeffile, error_cnt)
else
  call GetMCCLNameList(nmldeffile,mcnl)
end if


! perform a Monte Carlo simulation
call DoMCsimulation(mcnl, progname, nmldeffile)

end program EMMCOpenCL

!--------------------------------------------------------------------------
!
! SUBROUTINE:DoMCsimulation
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Perform the MC simulation
!
!> @param nmlfile namelist file name
!
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 07/23/13  MDG 3.0 complete rewrite
!> @date 07/30/13  MDG 3.1 added Patrick's code for double sample tilt (sigma, omega)
!> @date 09/25/13  MDG 3.2 added a few parameters to the output file
!> @date 03/17/14  MDG 3.3 added a few more for the IDL visualization program
!> @date 06/19/14  MDG 4.0 rewrite with name list handling removed
!> @date 07/23/14  SS  4.1 conversion to OpenCL
!> @date 03/26/15  MDG 5.0 all output now in HDF5 format 
!> @date 05/05/15  MDG 5.1 removed getenv() call; replaced by global path string
!> @date 09/01/15  MDG 5.2 modifications due to Lambert module changes
!> @date 09/09/15  MDG 5.3 added devid selector (GPU device ID) to namelist
!> @date 10/12/15  SS  5.4 added sample tilt series option
!> @date 12/04/15  MDG 5.5 added Ivol mode for display of interaction volume
!> @date 02/23/16  MDG 5.6 converted to CLFortran
!> @date 05/21/16  MDG 5.7 changes for HDF internal file reorganization
!--------------------------------------------------------------------------
subroutine DoMCsimulation(mcnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use crystal
use constants
use symmetry
use error
use io
use files
use timing
use diffraction, only:CalcWaveLength
use Lambert
use clfortran
use CLsupport
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING

IMPLICIT NONE

type(MCCLNameListType),INTENT(INOUT)    :: mcnl
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile


type(unitcell),pointer  :: cell
type(DynType)           :: Dyn
type(gnode)             :: rlp

integer(kind=irg)       :: numsy        ! number of Lambert map points along y
integer(kind=irg)       :: numEbins     ! number of energy bins
integer(kind=irg)       :: numzbins     ! number of depth bins
integer(kind=irg)       :: nx           ! no. of pixels
integer(kind=irg)       :: j,k,l,ip,istat
integer(kind=ill)       :: i, io_int(1), num_max, totnum_el_nml, multiplier
real(kind=4),target     :: Ze           ! average atomic number
real(kind=4),target     :: density      ! density in g/cm^3
real(kind=4),target     :: at_wt        ! average atomic weight in g/mole
logical                 :: verbose
real(kind=4)            :: dens, avA, avZ, io_real(3), dmin ! used with CalcDensity routine
real(kind=8) , parameter:: dtoR = 0.01745329251D0 !auxiliary variables
real(kind=4),target     :: EkeV, sig, omega ! input values to the kernel. Can only be real kind=4 otherwise values are not properly passed
integer(kind=ill)       :: totnum_el, bse     ! total number of electrons to simulate and no. of backscattered electrons
integer(kind=4)         :: prime ! input values to the kernel
integer(kind=4),target  :: globalworkgrpsz, num_el, steps ! input values to the kernel
integer(kind=8)         :: size_in_bytes,size_in_bytes_seeds ! size of arrays passed to kernel. Only accepts kind=8 integers by clCreateBuffer etc., so donot change
integer(kind=8),target  :: globalsize(2), localsize(2) ! size of global and local work groups. Again only kind=8 is accepted by clEnqueueNDRangeKernel
character(4)            :: mode
! results from kernel stored here
real(kind=4),allocatable, target :: Lamresx(:), Lamresy(:), Lamresz(:), depthres(:), energyres(:)

! final results stored here
integer(kind=4),allocatable :: accum_e(:,:,:), accum_z(:,:,:,:), accum_xyz(:,:,:), rnseeds(:)
integer(kind=ill),allocatable :: accum_e_ill(:,:,:)
integer(kind=4),allocatable,target  :: init_seeds(:)
integer(kind=4)         :: idxy(2), iE, px, py, iz, nseeds, hdferr, tstart, tstop ! auxiliary variables
real(kind=4)            :: cxyz(3), edis, xy(2), xs, ys, zs, sclf ! auxiliary variables
real(kind=8)            :: delta,rand
character(11)           :: dstr
character(15)           :: tstrb
character(15)           :: tstre
logical                 :: f_exists

integer(c_size_t),target       :: slocal(2), localout

! OpenCL variables
integer(c_intptr_t),allocatable, target  :: platform(:)
integer(c_intptr_t),allocatable, target  :: device(:)
integer(c_intptr_t),target     :: context
integer(c_intptr_t),target     :: command_queue
integer(c_intptr_t),target     :: prog
integer(c_intptr_t),target     :: kernel
integer(c_intptr_t),target     :: LamX, LamY, LamZ, depth, energy, seeds
type(c_ptr)                    :: event
integer(c_int32_t)             :: ierr, pcnt, ierr2
integer(c_size_t),target       :: slength
integer(c_intptr_t),target     :: ctx_props(3)
character(3),target            :: kernelname 
character(5),target            :: kernelname2
character(19),target           :: progoptions
character(fnlen),target        :: info ! info about the GPU
integer(c_int64_t)             :: cmd_queue_props

integer, parameter      :: iunit = 10
integer, parameter      :: source_length = 50000
character(len=source_length),target  :: source
character(len=source_length, KIND=c_char),TARGET :: csource
type(c_ptr), target :: psource
integer(c_int)         :: nump, numd, irec, val,val1 ! auxiliary variables
integer(c_size_t)      :: cnum, cnuminfo
character(fnlen)        :: groupname, dataset, instring, dataname, fname, sourcefile, datagroupname
integer(kind=irg)       :: numangle, iang

type(HDFobjectStackType),pointer  :: HDF_head

nullify(HDF_head)

call timestamp(datestring=dstr, timestring=tstrb)

numsy = mcnl%numsx
nullify(cell)
allocate(cell)

! get the crystal strucutre from the *.xtal file
verbose = .TRUE.
dmin = 0.05
val = 0
val1 = 0
call Initialize_Cell(cell,Dyn,rlp,mcnl%xtalname, dmin, sngl(mcnl%EkeV), verbose)

! then calculate density, average atomic number and average atomic weight
call CalcDensity(cell, dens, avZ, avA)
density = dble(dens)
Ze = dble(avZ)
at_wt = dble(avA)
io_real(1:3) = (/ dens, avZ, avA /)
call WriteValue('Density, avZ, avA = ',io_real,3,"(2f10.5,',',f10.5)")
mode = mcnl%mode

if (mode .eq. 'full') then
    steps = 300
else if (mode .eq. 'bse1') then
    steps = 1
else if (mode .eq. 'Ivol') then
    steps = 300
else
    stop 'Unknown mode specified in namelist file'
end if


EkeV = mcnl%EkeV
!sig = mcnl%sig*dtoR
omega = mcnl%omega*dtoR
globalworkgrpsz = mcnl%globalworkgrpsz
num_el = mcnl%num_el ! no. of electron simulation by one work item
num_max = globalworkgrpsz*globalworkgrpsz*num_el ! total simulation in one loop
totnum_el_nml = mcnl%totnum_el
multiplier =  mcnl%multiplier
totnum_el = totnum_el_nml * multiplier ! total number of electrons to simulate
globalsize = (/ mcnl%globalworkgrpsz, mcnl%globalworkgrpsz /)
numEbins =  int((mcnl%EkeV-mcnl%Ehistmin)/mcnl%Ebinsize)+1
numzbins =  int(mcnl%depthmax/mcnl%depthstep)+1
nx = (mcnl%numsx-1)/2


if (mode.eq.'Ivol') then 
  allocate(Lamresx(num_max), Lamresy(num_max), Lamresz(num_max), stat=istat)
  Lamresz = 0.0
else
  allocate(Lamresx(num_max), Lamresy(num_max), depthres(num_max), energyres(num_max), stat=istat)
  depthres = 0.0
  energyres = 0.0
end if
Lamresx = 0.0
Lamresy = 0.0
size_in_bytes = num_max*sizeof(EkeV)
size_in_bytes_seeds = 4*globalworkgrpsz*globalworkgrpsz*sizeof(EkeV)

if (mode .eq. 'bse1') then
    if (mcnl%sigstep .ne. 0.D0) then
       numangle = nint((mcnl%sigend - mcnl%sigstart)/mcnl%sigstep)+1
    else
       call FatalError('EMMCOpenCL:','zero step size for sigma values')
    end if
end if

if (mode .eq. 'full') then
   numangle = 1
   allocate(accum_e(numEbins,-nx:nx,-nx:nx),accum_z(numEbins,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)
   accum_e = 0
   accum_z = 0
else if (mode .eq. 'bse1') then
   allocate(accum_e(numangle,-nx:nx,-nx:nx),accum_z(numangle,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)
   accum_e = 0
   accum_z = 0
else if (mode .eq. 'Ivol') then
   numangle = 1
   allocate(accum_xyz(-nx:nx,-nx:nx,numzbins),stat=istat)
   accum_xyz = 0
else
   call FatalError('EMMCOpenCL:','Unknown mode specified in namelist file')
end if

! changed by MDG [09/01/15] after extensive modifications to Lambert routines
! old code delta = dble(nx)/LPs%sPio2
delta = dble(nx)

!=====================
! INITIALIZATION
!=====================
call CLinit_PDCCQ(platform, nump, mcnl%platid, device, numd, mcnl%devid, info, context, command_queue)


!=====================
! BUILD THE KERNEL
!=====================

! read the source file
if (mode .eq. 'Ivol') then 
  sourcefile = 'EMMCxyz.cl'
else
  sourcefile = 'EMMC.cl'
end if
write (*,*) 'OpenCL source file set to : ',trim(sourcefile)
call CLread_source_file(sourcefile, csource, slength)

! create the program
io_int(1) = slength
call WriteValue('Kernel source length (characters) : ',io_int,1)
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('DoMCsimulation:clCreateProgramWithSource', ierr)

! build the program
! progoptions = '-cl-no-signed-zeros'
! ierr = clBuildProgram(prog, numd, C_LOC(device), C_LOC(progoptions), C_NULL_FUNPTR, C_NULL_PTR)
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(mcnl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
if(len(trim(source)) > 0) call Message(trim(source(1:cnum)),frm='(A)')
call CLerror_check('DoMCsimulation:clBuildProgram', ierr)
call CLerror_check('DoMCsimulation:clGetProgramBuildInfo', ierr2)

! if we get here, then the program build was successful and we can proceed with the creation of the kernel
call Message('Program Build Successful... Creating kernel')

! finally get the kernel and release the program
if (mode.eq.'Ivol') then
  kernelname2 = 'MCxyz'
  kernel = clCreateKernel(prog, C_LOC(kernelname2), ierr)
  call CLerror_check('DoMCsimulation:clCreateKernel:MCxyz', ierr)
else
  kernelname = 'MC'//CHAR(0)
  kernel = clCreateKernel(prog, C_LOC(kernelname), ierr)
  call CLerror_check('DoMCsimulation:clCreateKernel:MC', ierr)
end if

ierr = clReleaseProgram(prog)
call CLerror_check('DoMCsimulation:clReleaseProgram', ierr)

open(unit = iunit, file = trim(EMsoft_toNativePath(EMsoft_getRandomseedfilename())), form='unformatted', status='old')
read(iunit) nseeds
allocate(rnseeds(nseeds))
read(iunit) rnseeds
close(unit=iunit,status='keep')

if (4*globalworkgrpsz**2 .gt. nseeds) then
  write (*,*) ' '
  write (*,*) 'Total number of prime number seeds available = ',nseeds
  write (*,*) 'Total number of prime number seeds needed    = ',4*globalworkgrpsz**2
  write (*,*) ' '
  write (*,*) 'Please reduce the globalworkgrpsz parameter or increase the number of seeds'
  write (*,*) 'in the ',trim(EMsoft_toNativePath(EMsoft_getRandomseedfilename())),' file. The total'
  write (*,*) 'number of prime seeds needed is equal to 4*globalworkgrpsz*globalworkgrpsz.'
  call FatalError('EMMCOpenCL:','insufficient prime number seeds')
end if

allocate(init_seeds(4*globalworkgrpsz*globalworkgrpsz),stat=istat)
init_seeds = 0
do i = 1,globalworkgrpsz
    do j = 1,globalworkgrpsz
        do k = 1,4
            init_seeds(4*((i-1)*globalworkgrpsz+(j-1))+k) = rnseeds(4*((i-1)*globalworkgrpsz+j)+k)
        end do
    end do
end do

! create device memory buffers

LamX = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:LamX', ierr)

LamY = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:LamY', ierr)

if (mode.eq.'Ivol') then 
  LamZ = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
  call CLerror_check('DoMCsimulation:clCreateBuffer:LamZ', ierr)
else
  depth = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
  call CLerror_check('DoMCsimulation:clCreateBuffer:depth', ierr)

  energy = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
  call CLerror_check('DoMCsimulation:clCreateBuffer:energy', ierr)
end if

seeds = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:seeds', ierr)

!call init_random_seed()
ierr = clEnqueueWriteBuffer(command_queue, seeds, CL_TRUE, 0_8, size_in_bytes_seeds, C_LOC(init_seeds(1)), &
                            0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('DoMCsimulation:clEnqueueWriteBuffer', ierr)

if (mode .eq. 'bse1') then
   call Message('Monte Carlo mode set to bse1. Calculating statistics for tilt series...',frm='(A/)')
else if (mode .eq. 'full') then
   call Message('Monte Carlo mode set to full. Performing full calculation...',frm='(A/)')
else if (mode .eq. 'Ivol') then 
   call Message('Monte Carlo mode set to Ivol. Performing full calculation...',frm='(A/)')
else
   call FatalError('DoMCSimulation','Unknown mode specified in namelist/json file')
end if


call Time_tick(tstart)

angleloop: do iang = 1,numangle

    if (mode .eq. 'bse1') then
        io_int(1) = iang
        call Writevalue('Angle loop #',io_int,1,'(I3)')
        sig = (mcnl%sigstart + (iang-1)*mcnl%sigstep)*dtoR
    else if (mode .eq. 'full') then
        sig = mcnl%sig*dtoR
    else if (mode .eq. 'Ivol') then
        sig = mcnl%sig*dtoR
    end if

    mainloop: do i = 1,(totnum_el/num_max+1)

! set the kernel arguments
if (mode.ne.'Ivol') then 
        ierr = clSetKernelArg(kernel, 0, sizeof(LamX), C_LOC(LamX))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamX', ierr)

        ierr = clSetKernelArg(kernel, 1, sizeof(LamY), C_LOC(LamY))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamY', ierr)

        ierr = clSetKernelArg(kernel, 2, sizeof(EkeV), C_LOC(EkeV))
        call CLerror_check('DoMCsimulation:clSetKernelArg:EkeV', ierr)

        ierr = clSetKernelArg(kernel, 3, sizeof(globalworkgrpsz), C_LOC(globalworkgrpsz))
        call CLerror_check('DoMCsimulation:clSetKernelArg:globalworkgrpsz', ierr)

        ierr = clSetKernelArg(kernel, 4, sizeof(Ze), C_LOC(Ze))
        call CLerror_check('DoMCsimulation:clSetKernelArg:Ze', ierr)

        ierr = clSetKernelArg(kernel, 5, sizeof(density), C_LOC(density))
        call CLerror_check('DoMCsimulation:clSetKernelArg:density', ierr)

        ierr = clSetKernelArg(kernel, 6, sizeof(at_wt), C_LOC(at_wt))
        call CLerror_check('DoMCsimulation:clSetKernelArg:at_wt', ierr)

        ierr = clSetKernelArg(kernel, 7, sizeof(num_el), C_LOC(num_el))
        call CLerror_check('DoMCsimulation:clSetKernelArg:num_el', ierr)

        ierr = clSetKernelArg(kernel, 8, sizeof(seeds), C_LOC(seeds))
        call CLerror_check('DoMCsimulation:clSetKernelArg:seeds', ierr)

        ierr = clSetKernelArg(kernel, 9, sizeof(sig), C_LOC(sig))
        call CLerror_check('DoMCsimulation:clSetKernelArg:sig', ierr)

        ierr = clSetKernelArg(kernel, 10, sizeof(omega), C_LOC(omega))
        call CLerror_check('DoMCsimulation:clSetKernelArg:omega', ierr)

        ierr = clSetKernelArg(kernel, 11, sizeof(depth), C_LOC(depth))
        call CLerror_check('DoMCsimulation:clSetKernelArg:depth', ierr)

        ierr = clSetKernelArg(kernel, 12, sizeof(energy), C_LOC(energy))
        call CLerror_check('DoMCsimulation:clSetKernelArg:energy', ierr)

        ierr = clSetKernelArg(kernel, 13, sizeof(steps), C_LOC(steps))
        call CLerror_check('DoMCsimulation:clSetKernelArg:steps', ierr)
else
        ierr = clSetKernelArg(kernel, 0, sizeof(LamX), C_LOC(LamX))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamX', ierr)

        ierr = clSetKernelArg(kernel, 1, sizeof(LamY), C_LOC(LamY))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamY', ierr)

        ierr = clSetKernelArg(kernel, 2, sizeof(LamZ), C_LOC(LamZ))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamZ', ierr)

        ierr = clSetKernelArg(kernel, 3, sizeof(EkeV), C_LOC(EkeV))
        call CLerror_check('DoMCsimulation:clSetKernelArg:EkeV', ierr)

        ierr = clSetKernelArg(kernel, 4, sizeof(globalworkgrpsz), C_LOC(globalworkgrpsz))
        call CLerror_check('DoMCsimulation:clSetKernelArg:globalworkgrpsz', ierr)

        ierr = clSetKernelArg(kernel, 5, sizeof(Ze), C_LOC(Ze))
        call CLerror_check('DoMCsimulation:clSetKernelArg:Ze', ierr)

        ierr = clSetKernelArg(kernel, 6, sizeof(density), C_LOC(density))
        call CLerror_check('DoMCsimulation:clSetKernelArg:density', ierr)

        ierr = clSetKernelArg(kernel, 7, sizeof(at_wt), C_LOC(at_wt))
        call CLerror_check('DoMCsimulation:clSetKernelArg:at_wt', ierr)

        ierr = clSetKernelArg(kernel, 8, sizeof(num_el), C_LOC(num_el))
        call CLerror_check('DoMCsimulation:clSetKernelArg:num_el', ierr)

        ierr = clSetKernelArg(kernel, 9, sizeof(seeds), C_LOC(seeds))
        call CLerror_check('DoMCsimulation:clSetKernelArg:seeds', ierr)

        ierr = clSetKernelArg(kernel, 10, sizeof(sig), C_LOC(sig))
        call CLerror_check('DoMCsimulation:clSetKernelArg:sig', ierr)

        ierr = clSetKernelArg(kernel, 11, sizeof(omega), C_LOC(omega))
        call CLerror_check('DoMCsimulation:clSetKernelArg:omega', ierr)

        ierr = clSetKernelArg(kernel, 12, sizeof(steps), C_LOC(steps))
        call CLerror_check('DoMCsimulation:clSetKernelArg:steps', ierr)
end if

! execute the kernel
!       ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), &
!                                     0, C_NULL_PTR, C_NULL_PTR)
        ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_NULL_PTR, &
                                      0, C_NULL_PTR, C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueNDRangeKernel', ierr)

! wait for the commands to finish
        ierr = clFinish(command_queue)
        call CLerror_check('DoMCsimulation:clFinish', ierr)

! read the resulting vector from device memory
if (mode.ne.'Ivol') then
        ierr = clEnqueueReadBuffer(command_queue,LamX,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresx(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:Lamresx', ierr)
        ierr = clEnqueueReadBuffer(command_queue,LamY,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresy(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:Lamresy', ierr)
        ierr = clEnqueueReadBuffer(command_queue,depth,CL_TRUE,0_8,size_in_bytes,C_LOC(depthres(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:depthres', ierr)
        ierr = clEnqueueReadBuffer(command_queue,energy,CL_TRUE,0_8,size_in_bytes,C_LOC(energyres(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:energyres', ierr)
else
        ierr = clEnqueueReadBuffer(command_queue,LamX,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresx(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:Lamresx', ierr)
        ierr = clEnqueueReadBuffer(command_queue,LamY,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresy(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:Lamresy', ierr)
        ierr = clEnqueueReadBuffer(command_queue,LamZ,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresz(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:Lamresz', ierr)
end if

!    call clEnqueueReadBuffer(command_queue, seeds, cl_bool(.true.), 0_8, size_in_bytes_seeds, init_seeds(1), ierr)
        if (mode .eq. 'full') then
           subloopfull: do j = 1, num_max

               if ((Lamresx(j) .ne. -10.0) .and. (Lamresy(j) .ne. -10.0) &
               .and. (depthres(j) .ne. 10.0) .and. (energyres(j) .ne. 0.0) &
               .and. .not.isnan(Lamresx(j)) .and. .not.isnan(Lamresy(j))) then
! and get the nearest pixel [ take into account reversal of coordinate frame (x,y) -> (y,-x) ]
                   if ((nint(delta*Lamresy(j)) .eq. 0.0) .and. (nint(-delta*Lamresx(j)) .eq. 0.0)) then
                       val1 = val1 + 1
                   end if

                   val = val + 1
                   idxy = (/ nint(delta*Lamresy(j)), nint(-delta*Lamresx(j)) /)

                   if (maxval(abs(idxy)).le.nx) then
! If Ec larger than Emin, then we should count this electron
                       if (energyres(j).gt.mcnl%Ehistmin) then

                           iE = nint((energyres(j)-mcnl%Ehistmin)/mcnl%Ebinsize)+1
! first add this electron to the correct exit distance vs. energy bin (coarser than the angular plot)
                           edis = abs(depthres(j))  ! distance from last scattering point to surface along trajectory
                           iz = nint(edis/mcnl%depthstep) +1
                           if ( (iz.gt.0).and.(iz.le.numzbins) ) then

                               px = nint(idxy(1)/10.0)
                               py = nint(idxy(2)/10.0)
                               accum_z(iE,iz,px,py) = accum_z(iE,iz,px,py) + 1

                           end if
! then add it to the modified Lambert accumulator array.
                           accum_e(iE,idxy(1),idxy(2)) = accum_e(iE,idxy(1),idxy(2)) + 1
                       end if
                   end if
               end if
           end do subloopfull
        end if
 
        if (mode .eq. 'bse1') then
           subloopbse1: do j = 1, num_max

               if ((Lamresx(j) .ne. -10.0) .and. (Lamresy(j) .ne. -10.0) &
               .and. (depthres(j) .ne. 10.0) .and. (energyres(j) .ne. 0.0) &
               .and. .not.isnan(Lamresx(j)) .and. .not.isnan(Lamresy(j))) then
! and get the nearest pixel [ take into account reversal of coordinate frame (x,y) -> (y,-x) ]
                   if ((nint(delta*Lamresy(j)) .eq. 0.0) .and. (nint(-delta*Lamresx(j)) .eq. 0.0)) then
                       val1 = val1 + 1
                   end if

                   val = val + 1
                   idxy = (/ nint(delta*Lamresy(j)), nint(-delta*Lamresx(j)) /)

                   if (maxval(abs(idxy)).le.nx) then
! first add this electron to the correct exit distance vs. sigma (coarser than the angular plot)
                       edis = abs(depthres(j))  ! distance from last scattering point to surface along trajectory
                       iz = nint(edis/mcnl%depthstep) +1
                       if ( (iz.gt.0).and.(iz.le.numzbins) ) then
                           px = nint(idxy(1)/10.0)
                           py = nint(idxy(2)/10.0)
                           accum_z(iang,iz,px,py) = accum_z(iang,iz,px,py) + 1

                       end if
! then add it to the modified Lambert accumulator array.
                       accum_e(iang,idxy(1),idxy(2)) = accum_e(iang,idxy(1),idxy(2)) + 1
                   end if
               end if
           end do subloopbse1
        end if

        if (mode .eq. 'Ivol') then
           sclf = 1.0E-1

           subloopIvol: do j = 1, num_max
               if ((Lamresx(j) .ne. -100000.0) .and. (Lamresy(j) .ne. -100000.0) &
               .and. (Lamresz(j) .ne. -100000.0) &
               .and. .not.isnan(Lamresx(j)) .and. .not.isnan(Lamresy(j)) .and. .not.isnan(Lamresz(j))) then
                  xs = Lamresx(j)*sclf
                  ys = Lamresy(j)*sclf
                  zs = Lamresz(j)*sclf/mcnl%depthstep
                  if ((maxval( (/ abs(xs), abs(ys) /)) .lt. nx) .and. (zs.lt.numzbins) ) then
                    accum_xyz(nint(xs),nint(ys),nint(zs)+1) = accum_xyz(nint(xs),nint(ys),nint(zs)+1) + 1
                  end if
               end if
           end do subloopIvol
        end if

        if (mod(i,50).eq.0) then
            io_int(1) = i*num_max
            call WriteValue(' Total number of electrons incident = ',io_int, 1, "(I15)")
            if (mode .eq. 'bse1') then
                io_int(1) = sum(accum_e(iang,:,:))
                call WriteValue(' Number of BSE1 electrons = ',io_int, 1, "(I15)")
            else if(mode .eq. 'full') then
                allocate(accum_e_ill(numEbins,-nx:nx,-nx:nx),stat=istat)
                accum_e_ill = accum_e
                io_int(1) = sum(accum_e_ill)
                deallocate(accum_e_ill)
                call WriteValue(' Number of BSE electrons = ',io_int, 1, "(I15)")
            else if(mode .eq. 'Ivol') then
                io_int(1) = sum(accum_xyz)
                call WriteValue(' Number of electrons in interaction volume = ',io_int, 1, "(I15)")
            else
                call FatalError('DoMCSimulations','Unknown mode specified in namelist/json file')
            end if

            if (i.eq.50) then

            end if

        end if


    end do mainloop
! and write some infgormation to the console

    io_int(1) = totnum_el
    call WriteValue('Total number of incident electrons = ',io_int,1,'(I15)')
    if (mode .eq. 'bse1') then
        io_int(1) = sum(accum_e(iang,:,:))
        call WriteValue('Total number of BSE1 electrons = ',io_int,1,'(I15)')
        bse = sum(accum_e(iang,:,:))
        io_real(1) = dble(bse)/dble(totnum_el)
        call WriteValue('Backscatter yield = ',io_real,1,'(F15.6)')
    else if (mode .eq. 'full') then
! note that we need to prevent integer overflows !
        allocate(accum_e_ill(numEbins,-nx:nx,-nx:nx),stat=istat)
        accum_e_ill = accum_e
        io_int(1) = sum(accum_e_ill)
        deallocate(accum_e_ill)
        call WriteValue('Total number of BSE electrons = ',io_int,1,'(I15)')
        io_real(1) = dble(io_int(1))/dble(totnum_el)
        call WriteValue('Backscatter yield = ',io_real,1,'(F15.6)')
    else if (mode .eq. 'Ivol') then
        io_int(1) = sum(accum_xyz)
        call WriteValue('Total number of electrons in interaction volume = ',io_int,1,'(I15)')
    else 
        call FatalError('DoMCSimulations','Unknown mode specified in namelist/json file')
    end if
 

end do angleloop

tstop = Time_tock(tstart)
io_int(1) = tstop
call WriteValue('Total execution time [s] = ',io_int,1)

io_int(1) = totnum_el/num_max

totnum_el = (io_int(1)+1)*num_max

! output in .h5 format.

! Initialize FORTRAN interface.
!
call h5open_EMsoft(hdferr)
call timestamp(datestring=dstr, timestring=tstre)

! get the filename; if it already exists, then delete it and create a new one
dataname = trim(EMsoft_getEMdatapathname())//trim(mcnl%dataname)
dataname = EMsoft_toNativePath(dataname)
inquire(file=trim(dataname), exist=f_exists)

if (f_exists) then
  open(unit=dataunit, file=trim(dataname), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

! Create a new file using the default properties.
hdferr =  HDF_createFile(dataname, HDF_head)

! write the EMheader to the file
datagroupname = 'MCOpenCL'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = "NMLfiles"
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = 'MCOpenCLNML'
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)
!call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = "NMLparameters"
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteMCCLNameList(HDF_head, mcnl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = 'EMData'
hdferr = HDF_createGroup(groupname, HDF_head)
hdferr = HDF_createGroup(datagroupname, HDF_head)

dataset = 'numzbins'
hdferr = HDF_writeDatasetInteger(dataset, numzbins, HDF_head)

! modified using multiplier
dataset = 'totnum_el'
hdferr = HDF_writeDatasetInteger(dataset, mcnl%totnum_el, HDF_head)

dataset = 'multiplier'
hdferr = HDF_writeDatasetInteger(dataset, mcnl%multiplier, HDF_head)

if (mode .eq. 'full') then

    dataset = 'numEbins'
    hdferr = HDF_writeDatasetInteger(dataset, numEbins, HDF_head)

!allocate(accum_e(numEbins,-nx:nx,-nx:nx),accum_z(numEbins,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)
    dataset = 'accum_e'
    hdferr = HDF_writeDatasetIntegerArray3D(dataset, accum_e, numEbins, 2*nx+1, 2*nx+1, HDF_head)

    dataset = 'accum_z'
    hdferr = HDF_writeDatasetIntegerArray4D(dataset, accum_z, numEbins, numzbins, 2*(nx/10)+1, 2*(nx/10)+1, HDF_head)

else if (mode .eq. 'bse1') then

    dataset = 'numangle'
    hdferr = HDF_writeDatasetInteger(dataset, numangle, HDF_head)

!allocate(accum_e(numangle,-nx:nx,-nx:nx),accum_z(numangle,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)
    dataset = 'accum_e'
    hdferr = HDF_writeDatasetIntegerArray3D(dataset, accum_e, numangle, 2*nx+1, 2*nx+1, HDF_head)

    dataset = 'accum_z'
    hdferr = HDF_writeDatasetIntegerArray4D(dataset, accum_z, numangle, numzbins, 2*(nx/10)+1, 2*(nx/10)+1, HDF_head)

else if (mode .eq. 'Ivol') then

    dataset = 'accum_xyz'
    hdferr = HDF_writeDatasetIntegerArray3D(dataset, accum_xyz, 2*nx+1, 2*nx+1, numzbins, HDF_head)

end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

!
!=====================
! RELEASE EVERYTHING
!=====================

ierr = clReleaseKernel(kernel)
call CLerror_check('DoMCsimulation:clReleaseKernel', ierr)
ierr = clReleaseCommandQueue(command_queue)
call CLerror_check('DoMCsimulation:clReleaseCommandQueue', ierr)
ierr = clReleaseContext(context)
call CLerror_check('DoMCsimulation:clReleaseContext', ierr)
ierr = clReleaseMemObject(LamX)
call CLerror_check('DoMCsimulation:clReleaseMemObject:LamX', ierr)
ierr = clReleaseMemObject(LamY)
call CLerror_check('DoMCsimulation:clReleaseMemObject:LamY', ierr)
if (mode.eq.'Ivol') then 
  ierr = clReleaseMemObject(LamZ)
  call CLerror_check('DoMCsimulation:clReleaseMemObject:LamZ', ierr)
else
  ierr = clReleaseMemObject(depth)
  call CLerror_check('DoMCsimulation:clReleaseMemObject:depth', ierr)
  ierr = clReleaseMemObject(energy)
  call CLerror_check('DoMCsimulation:clReleaseMemObject:energy', ierr)
end if
ierr = clReleaseMemObject(seeds)
call CLerror_check('DoMCsimulation:clReleaseMemObject:seeds', ierr)


end subroutine DoMCsimulation

