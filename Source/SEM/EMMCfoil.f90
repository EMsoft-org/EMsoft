! ###################################################################
! Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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

program EMMCfoil

use local
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use json_module
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(MCCLNameListType)                  :: mcnl
integer(kind=irg)                       :: res, error_cnt

nmldeffile = 'EMMCfoil.nml'
progname = 'EMMCfoil.f90'
progdesc = 'Monte Carlo backscattered electron simulation for thin foil'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 45 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call JSONreadMCCLNameList(mcnl, nmldeffile, error_cnt)
else
  call GetMCCLfoilNameList(nmldeffile,mcnl)
end if

! perform a Monte Carlo simulation
call DoMCsimulation(mcnl, progname, nmldeffile)

end program EMMCfoil

!--------------------------------------------------------------------------
!
! SUBROUTINE:DoMCsimulationfoil
!
!> @author Saransh Singh/MArc De Graef, Carnegie Mellon University
!
!> @brief Perform the MC simulation
!
!> @param nmlfile namelist file name
!
!> @date 01/14/17 MDG  1.0 modified EMMCsphere to EMMCfoil
!> @date 10/08/19 MDG  1.1 replace h5_open_f by EMsoft version
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
use stringconstants
use math

IMPLICIT NONE

type(MCCLNameListType),INTENT(INOUT)    :: mcnl
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile


type(unitcell)          :: cell
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
real(kind=4),target     :: EkeV, sig, omega, thickness ! input values to the kernel. Can only be real kind=4 otherwise values are not properly passed
integer(kind=ill)       :: totnum_el, bse     ! total number of electrons to simulate and no. of backscattered electrons
integer(kind=4)         :: prime ! input values to the kernel
integer(kind=4),target  :: globalworkgrpsz, num_el, steps ! input values to the kernel
integer(kind=8)         :: size_in_bytes,size_in_bytes_seeds ! size of arrays passed to kernel. Only accepts kind=8 integers by clCreateBuffer etc., so donot change
integer(kind=8),target  :: globalsize(2), localsize(2) ! size of global and local work groups. Again only kind=8 is accepted by clEnqueueNDRangeKernel
character(4)            :: mode
! results from kernel stored here
real(kind=4),allocatable, target :: depthres(:), energyres(:), LamresxSH(:), LamresySH(:)

! final results stored here
integer(kind=4),allocatable :: rnseeds(:)
integer(kind=4),allocatable :: accum_e_SH(:,:,:), accum_z_SH(:,:,:,:), accum_e_SP(:,:,:)
integer(kind=ill),allocatable :: accum_e_ill(:,:,:)
integer(kind=4),allocatable,target  :: init_seeds(:)
integer(kind=4)         :: idxy(2), iE, px, py, iz, nseeds, hdferr, tstart, tstop ! auxiliary variables
real(kind=4)            :: cxyz(3), edis, xy(2), xs, ys, zs, sclf, Radius ! auxiliary variables
real(kind=8)            :: delta,rand, xyz(3)
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
integer(c_intptr_t),target     :: LamX, LamY, LamZ, depth, energy, seeds, LamXSH, LamYSH
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
type(HDFobjectStackType)          :: HDF_head


nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)

numsy = mcnl%numsx

! get the crystal structure from the *.xtal file
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

steps = 150
EkeV = mcnl%EkeV
sig = mcnl%sig*dtoR
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
thickness = mcnl%thickness
delta = dble(nx)

allocate(depthres(num_max), energyres(num_max), stat=istat)
allocate(LamresxSH(num_max), LamresySH(num_max),stat=istat)
depthres = 0.0
energyres = 0.0
LamresxSH = 0.0
LamresySH = 0.0
size_in_bytes = num_max*sizeof(EkeV)
size_in_bytes_seeds = 4*globalworkgrpsz*globalworkgrpsz*sizeof(EkeV)

allocate(accum_e_SH(numEbins,-nx:nx,-nx:nx),accum_z_SH(numEbins,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)
accum_e_SH = 0
accum_z_SH = 0


!=====================
! INITIALIZATION
!=====================
call CLinit_PDCCQ(platform, nump, mcnl%platid, device, numd, mcnl%devid, info, context, command_queue)

!=====================
! BUILD THE KERNEL
!=====================

! read the source file
sourcefile = 'EMMCfoil.cl'
call Message('OpenCL source file set to : '//trim(sourcefile))
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
kernelname = 'MC'//CHAR(0)
write (*,*) 'creating kernelname : ',kernelname
kernel = clCreateKernel(prog, C_LOC(kernelname), ierr)
call CLerror_check('DoMCsimulation:clCreateKernel:MC', ierr)

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

LamXSH = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:LamXSH', ierr)

LamYSH = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:LamYSH', ierr)

depth = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:depth', ierr)

energy = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:energy', ierr)

seeds = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('DoMCsimulation:clCreateBuffer:seeds', ierr)

ierr = clEnqueueWriteBuffer(command_queue, seeds, CL_TRUE, 0_8, size_in_bytes_seeds, C_LOC(init_seeds(1)), &
                            0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('DoMCsimulation:clEnqueueWriteBuffer', ierr)

call Message('Monte Carlo mode set to full. Performing full calculation...',frm='(A/)')


call Time_tick(tstart)

    mainloop: do i = 1,(totnum_el/num_max+1)

! set the kernel arguments
        ierr = clSetKernelArg(kernel, 0, sizeof(EkeV), C_LOC(EkeV))
        call CLerror_check('DoMCsimulation:clSetKernelArg:EkeV', ierr)

        ierr = clSetKernelArg(kernel, 1, sizeof(globalworkgrpsz), C_LOC(globalworkgrpsz))
        call CLerror_check('DoMCsimulation:clSetKernelArg:globalworkgrpsz', ierr)

        ierr = clSetKernelArg(kernel, 2, sizeof(Ze), C_LOC(Ze))
        call CLerror_check('DoMCsimulation:clSetKernelArg:Ze', ierr)

        ierr = clSetKernelArg(kernel, 3, sizeof(density), C_LOC(density))
        call CLerror_check('DoMCsimulation:clSetKernelArg:density', ierr)

        ierr = clSetKernelArg(kernel, 4, sizeof(at_wt), C_LOC(at_wt))
        call CLerror_check('DoMCsimulation:clSetKernelArg:at_wt', ierr)

        ierr = clSetKernelArg(kernel, 5, sizeof(num_el), C_LOC(num_el))
        call CLerror_check('DoMCsimulation:clSetKernelArg:num_el', ierr)

        ierr = clSetKernelArg(kernel, 6, sizeof(seeds), C_LOC(seeds))
        call CLerror_check('DoMCsimulation:clSetKernelArg:seeds', ierr)

        ierr = clSetKernelArg(kernel, 7, sizeof(sig), C_LOC(sig))
        call CLerror_check('DoMCsimulation:clSetKernelArg:sig', ierr)

        ierr = clSetKernelArg(kernel, 8, sizeof(omega), C_LOC(omega))
        call CLerror_check('DoMCsimulation:clSetKernelArg:omega', ierr)

        ierr = clSetKernelArg(kernel, 9, sizeof(depth), C_LOC(depth))
        call CLerror_check('DoMCsimulation:clSetKernelArg:depth', ierr)

        ierr = clSetKernelArg(kernel, 10, sizeof(energy), C_LOC(energy))
        call CLerror_check('DoMCsimulation:clSetKernelArg:energy', ierr)

        ierr = clSetKernelArg(kernel, 11, sizeof(steps), C_LOC(steps))
        call CLerror_check('DoMCsimulation:clSetKernelArg:steps', ierr)

        ierr = clSetKernelArg(kernel, 12, sizeof(thickness), C_LOC(thickness))
        call CLerror_check('DoMCsimulation:clSetKernelArg:thickness', ierr)

        ierr = clSetKernelArg(kernel, 13, sizeof(LamXSH), C_LOC(LamXSH))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamXSH', ierr)

        ierr = clSetKernelArg(kernel, 14, sizeof(LamYSH), C_LOC(LamYSH))
        call CLerror_check('DoMCsimulation:clSetKernelArg:LamYSH', ierr)

! execute the kernel
        ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_NULL_PTR, &
                                      0, C_NULL_PTR, C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueNDRangeKernel', ierr)

! wait for the commands to finish
        ierr = clFinish(command_queue)
        call CLerror_check('DoMCsimulation:clFinish', ierr)

! read the resulting vector from device memory
        ierr = clEnqueueReadBuffer(command_queue,depth,CL_TRUE,0_8,size_in_bytes,C_LOC(depthres(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:depthres', ierr)
        ierr = clEnqueueReadBuffer(command_queue,energy,CL_TRUE,0_8,size_in_bytes,C_LOC(energyres(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:energyres', ierr)
        ierr = clEnqueueReadBuffer(command_queue,LamXSH,CL_TRUE,0_8,size_in_bytes,C_LOC(LamresxSH(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:LamresxSH', ierr)
        ierr = clEnqueueReadBuffer(command_queue,LamYSH,CL_TRUE,0_8,size_in_bytes,C_LOC(LamresySH(1)),0,C_NULL_PTR,C_NULL_PTR)
        call CLerror_check('DoMCsimulation:clEnqueueReadBuffer:LamresySH', ierr)

        subloopfull: do j = 1, num_max
!==============================================================================================================
!========================================SOUTHERN HEMISPHERE ONLY==============================================
!==============================================================================================================

               if ((LamresxSH(j) .ne. -10.0) .and. (LamresySH(j) .ne. -10.0) &
               .and. (depthres(j) .ne. 10.0) .and. (energyres(j) .ne. 0.0) &
               .and. .not.isnan(LamresxSH(j)) .and. .not.isnan(LamresySH(j))) then
! and get the nearest pixel [ take into account reversal of coordinate frame (x,y) -> (y,-x) ]
                   if ((nint(delta*LamresySH(j)) .eq. 0.0) .and. (nint(-delta*LamresxSH(j)) .eq. 0.0)) then
                       val1 = val1 + 1
                   end if

                   val = val + 1
                   idxy = (/ nint(delta*LamresySH(j)), nint(-delta*LamresxSH(j)) /)
                   
                   if (maxval(abs(idxy)).le.nx) then
! If Ec larger than Emin, then we should count this electron
                       if (energyres(j).gt.mcnl%Ehistmin) then

                           iE = nint((energyres(j)-mcnl%Ehistmin)/mcnl%Ebinsize)+1
! first add this electron to the correct exit distance vs. energy bin (coarser than the angular plot)
                           edis = depthres(j)  ! distance from last scattering point to bottom surface along trajectory
                           iz = nint(edis/mcnl%depthstep) +1
                           if ( (iz.gt.0).and.(iz.le.numzbins) ) then
                               px = nint(idxy(1)/10.0)
                               py = nint(idxy(2)/10.0)
                               accum_z_SH(iE,iz,px,py) = accum_z_SH(iE,iz,px,py) + 1

                           end if
! then add it to the modified Lambert accumulator array.
                           accum_e_SH(iE,idxy(1),idxy(2)) = accum_e_SH(iE,idxy(1),idxy(2)) + 1
                       end if
                   end if
               end if
        end do subloopfull        
 
        if (mod(i,50).eq.0) then
            io_int(1) = i*num_max
            call WriteValue(' Total number of electrons incident         = ',io_int, 1, "(I15)")
            allocate(accum_e_ill(numEbins,-nx:nx,-nx:nx),stat=istat)
            accum_e_ill = accum_e_SH
            io_int(1) = sum(accum_e_ill)
            deallocate(accum_e_ill)
            call WriteValue(' Number of electrons in southern hemisphere = ',io_int, 1, "(I15)")
        end if


    end do mainloop
! and write some information to the console

io_int(1) = totnum_el
call WriteValue('Total number of incident electrons = ',io_int,1,'(I15)')
! note that we need to prevent integer overflows !
allocate(accum_e_ill(numEbins,-nx:nx,-nx:nx),stat=istat)
accum_e_ill = accum_e_SH
io_int(1) = sum(accum_e_ill)
deallocate(accum_e_ill)
call WriteValue('Total number of electrons in Southern hemisphere = ',io_int,1,'(I15)')
io_real(1) = dble(io_int(1))/dble(totnum_el)
call WriteValue('Transmission yield = ',io_real,1,'(F15.6)')
 
tstop = Time_tock(tstart)
io_int(1) = tstop
call WriteValue('Total execution time [s] = ',io_int,1)

io_int(1) = totnum_el/num_max

totnum_el = (io_int(1)+1)*num_max

! output in .h5 format.

! Initialize FORTRAN interface.
!
call h5open_EMsoft(hdferr)
call timestamp(timestring=tstre)

! first of all, if the file exists, then delete it 
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
datagroupname = 'MCfoil'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_MCfoilNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteMCCLfoilNameList(HDF_head, mcnl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
hdferr = HDF_createGroup(datagroupname, HDF_head)

dataset = SC_numzbins
hdferr = HDF_writeDatasetInteger(dataset, numzbins, HDF_head)

! modified using multiplier
dataset = SC_totnumel
hdferr = HDF_writeDatasetInteger(dataset, mcnl%totnum_el, HDF_head)

dataset = SC_multiplier
hdferr = HDF_writeDatasetInteger(dataset, mcnl%multiplier, HDF_head)

dataset = SC_numEbins
hdferr = HDF_writeDatasetInteger(dataset, numEbins, HDF_head)

dataset = SC_accume
hdferr = HDF_writeDatasetIntegerArray3D(dataset, accum_e_SH, numEbins, 2*nx+1, 2*nx+1, HDF_head)

dataset = SC_accumz
hdferr = HDF_writeDatasetIntegerArray4D(dataset, accum_z_SH, numEbins, numzbins, 2*(nx/10)+1, 2*(nx/10)+1, HDF_head)

! add stereographic projection versions of the accum_e_SH array
allocate(accum_e_SP(numEbins,-nx:nx,-nx:nx),stat=istat)
accum_e_SP = 0

Radius = 1.0
do i=-nx,nx 
  do j=-nx,nx 
    xy = (/ float(i), float(j) /) / float(nx)
    xyz = StereoGraphicInverse( xy, ierr, Radius )
    xyz = xyz/vecnorm(xyz)
    if (ierr.ne.0) then 
      accum_e_SP(1:numEbins,i,j) = 0.0
    else
      accum_e_SP(1:numEbins,i,j) = InterpolateLambert(xyz, accum_e_SH, nx, numEbins)
    end if
  end do
end do

dataset = SC_accumeSP
hdferr = HDF_writeDatasetIntegerArray3D(dataset, accum_e_SP, numEbins, 2*nx+1, 2*nx+1, HDF_head)

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
ierr = clReleaseMemObject(LamXSH)
call CLerror_check('DoMCsimulation:clReleaseMemObject:LamXSH', ierr)
ierr = clReleaseMemObject(LamYSH)
call CLerror_check('DoMCsimulation:clReleaseMemObject:LamYSH', ierr)
ierr = clReleaseMemObject(depth)
call CLerror_check('DoMCsimulation:clReleaseMemObject:depth', ierr)
ierr = clReleaseMemObject(energy)
call CLerror_check('DoMCsimulation:clReleaseMemObject:energy', ierr)
ierr = clReleaseMemObject(seeds)
call CLerror_check('DoMCsimulation:clReleaseMemObject:seeds', ierr)


end subroutine DoMCsimulation
