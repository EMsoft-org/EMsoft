! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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

!--------------------------------------------------------------------------
! EMsoft:EMEBSDFull.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDFull
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief EMEBSDFull computes energy-weighted EBSD pattern
!> each pattern calculated separately, without the master patterns

!> @date  01/25/17  SS 1.0 original program
!--------------------------------------------------------------------------

! ###################################################################

program EMEBSDFull

use local
use files 
use NameListTypedefs
use NameListHandlers
use detectors
use EBSDmod
use JSONsupport
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                       :: nmldeffile, progname, progdesc
type(EBSDFullNameListType)             :: enl

type(EBSDAngleType),pointer            :: angles
type(EBSDDetectorType)                 :: EBSDdetector

integer(kind=irg)                      :: istat, numangles, numEbins, numzbins
logical                                :: verbose

interface
        subroutine ComputeFullEBSDPatterns(enl, numangles, angles, EBSDdetector, progname, nmldeffile)

            use local
            use typedefs
            use NameListTypedefs
            use EBSDmod
            use HDF5
            use HDFsupport
            use clfortran
            use CLsupport
            use timing
            use crystal
            use constants
            use io
            use InitializersHDF
            use, INTRINSIC :: ISO_C_BINDING

            IMPLICIT NONE

            type(EBSDFullNameListType),INTENT(IN)         :: enl
            integer(kind=irg),INTENT(IN)                  :: numangles
            type(EBSDAngleType),pointer                   :: angles
            type(EBSDDetectorType),INTENT(INOUT)          :: EBSDdetector
            character(fnlen),INTENT(IN)                   :: nmldeffile
            character(fnlen),INTENT(IN)                   :: progname

        end subroutine ComputeFullEBSDPatterns

end interface

nullify(angles)

nmldeffile = 'EMEBSDFull.nml'
progname = 'EMEBSDFull.f90'
progdesc = 'Dynamical EBSD patterns, WITHOUT using precomputed MC and master Lambert projections'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 46 /), progname)

! only nml file for now; JSON file will be implemented later
call GetEBSDFullNameList(nmldeffile,enl)

! 1. read the angle array from file
verbose = .TRUE.
nullify(angles)
allocate(angles)
call EBSDFullreadangles(enl, numangles, angles, verbose=.TRUE.)

! generate the detector
! all lambdaEZ arrays are allocated in this subroutine, so no need to reallocate them
allocate(EBSDdetector%detector(enl%numsx,enl%numsy))
numEbins =  int((enl%EkeV-enl%Ehistmin)/enl%Ebinsize)+1
numzbins =  int(enl%depthmax/enl%depthstep)+1
call EBSDFullGenerateDetector(enl, EBSDdetector, numEbins, numzbins, verbose=.TRUE.)

! compute full EBSD pattern
call ComputeFullEBSDPatterns(enl, numangles, angles, EBSDdetector, progname, nmldeffile)

end program EMEBSDFull

recursive subroutine ComputeFullEBSDPatterns(enl, numangles, angles, EBSDdetector, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use EBSDmod
use HDF5
use HDFsupport
use clfortran
use CLsupport
use timing
use crystal
use constants
use io
use InitializersHDF
use initializers
use gvectors
use error
use diffraction, only:CalcWavelength, CalcUcg
use quaternions
use MBmodule
use Lambert
use rotations
use omp_lib
use, INTRINSIC :: ISO_C_BINDING
use stringconstants

IMPLICIT NONE

type(EBSDFullNameListType),INTENT(IN)         :: enl
type(EBSDAngleType),pointer                   :: angles
integer(kind=irg),INTENT(IN)                  :: numangles
type(EBSDDetectorType),INTENT(INOUT)          :: EBSDdetector
character(fnlen),INTENT(IN)                   :: nmldeffile
character(fnlen),INTENT(IN)                   :: progname

! crystal structure variables
logical                                       :: verbose
type(unitcell)                                :: cell
type(DynType)                                 :: Dyn
type(gnode)                                   :: rlp, myrlp
type(BetheParameterType)                      :: BetheParameters

! MC and Dynamical simulation variables
character(fnlen)                              :: xtalname
real(kind=sgl)                                :: dmin 
real(kind=4)                                  :: dens, avA, avZ, io_real(3)! used with CalcDensity routine
real(kind=8) , parameter                      :: dtoR = 0.01745329251D0 !auxiliary variable
real(kind=dbl),parameter                      :: nAmpere = 6.241D+18   ! Coulomb per second
real(kind=dbl)                                :: prefactor
real(kind=4),target                           :: EkeV, omega, sig
real(kind=4),target                           :: Ze           ! average atomic number
real(kind=4),target                           :: density      ! density in g/cm^3
real(kind=4),target                           :: at_wt        ! average atomic weight in g/mole
integer(kind=4),target                        :: globalworkgrpsz, num_el, steps
integer(kind=8)                               :: size_in_bytes,size_in_bytes_seeds ! size of arrays passed to kernel. Only accepts kind=8 integers by clCreateBuffer etc., so donot change

integer(kind=8),target                        :: globalsize(2), localsize(2) ! size of global and local work groups. Again only kind=8 is accepted by clEnqueueNDRangeKernel
integer(kind=irg)                             :: mcnx, hkl(3)
! results from kernel stored here
real(kind=4),allocatable, target              :: Lamresx(:), Lamresy(:), Lamresz(:), depthres(:), energyres(:)
integer(kind=4),allocatable,target            :: init_seeds(:)
integer(kind=ill)                             :: io_int(3)

real(kind=8)                                  :: delta, rand, mRelcor, mPsihat, mLambda, temp1, temp2 
integer(kind=ill)                             :: i, j, k, num_max, totnum_el_nml, multiplier, totnum_el
integer(kind=4),allocatable                   :: rnseeds(:)
real(kind=4)                                  :: tstop
integer(kind=irg)                             :: idxy(2), iE, iz, nseeds, tick, tickstart, tock
integer(kind=irg)                             :: nix, niy, nixp, niyp, skip
real(kind=sgl)                                :: edis, xy(2), dx, dy, dxm, dym, alpha, om(3,3), xyz(3)
real(kind=sgl)                                :: tana, cota, sa, ca, r1, r2, r3, rho
! OpenCL variables
integer(c_intptr_t),allocatable, target       :: platform(:)
integer(c_intptr_t),allocatable, target       :: device(:)
integer(c_intptr_t),target                    :: context
integer(c_intptr_t),target                    :: command_queue
integer(c_intptr_t),target                    :: prog
integer(c_intptr_t),target                    :: kernel
integer(c_intptr_t),target                    :: LamX, LamY, LamZ, depth, energy, seeds
type(c_ptr)                                   :: event
integer(c_int32_t)                            :: ierr, pcnt, ierr2
integer(c_size_t),target                      :: slength
integer(c_intptr_t),target                    :: ctx_props(3)
character(3),target                           :: kernelname 
character(5),target                           :: kernelname2
character(19),target                          :: progoptions
character(fnlen),target                       :: info ! info about the GPU
integer(c_int64_t)                            :: cmd_queue_props

integer, parameter                            :: iunit = 10
integer, parameter                            :: source_length = 50000
character(len=source_length),target           :: source
character(len=source_length, KIND=c_char),TARGET :: csource
type(c_ptr), target                           :: psource
integer(c_int)                                :: nump, numd, irec, val,val1 ! auxiliary variables
integer(c_size_t)                             :: cnum, cnuminfo

! dynamical calculation variables
integer(kind=irg)                             :: nref, nns, nnw, iang, totstrong, totweak, numset, &
                                                 gzero, ix, nat(maxpasym), numEbins, numzbins
real(kind=sgl)                                :: kk(3), FN(3), kkk(3), qu(4), testval, nabsl
type(reflisttype),pointer                     :: reflist,firstw, rltmp
complex(kind=dbl)                             :: czero
complex(kind=dbl), allocatable                :: DynMat(:,:), Lgh(:,:), Sgh(:,:,:), Lghtmp(:,:,:)
real(kind=sgl),allocatable                    :: svals(:), lambdaZ(:), nabsfact(:)
real(kind=sgl),allocatable                    :: EBSDPatterns(:,:,:,:), eulerangles(:,:), EkeVs(:), lambdas(:,:,:,:)
real(kind=dbl),allocatable                    :: thick(:,:,:)
integer(kind=irg)                             :: izz, izzmax
real(kind=dbl)                                :: kn, lambdafile(201)

! time variables
character(11)                                 :: dstr
character(15)                                 :: tstrb
character(15)                                 :: tstre
logical                                       :: f_exists, overwrite=.TRUE.
character(fnlen,kind=c_char)                  :: line2(1)
integer(kind=irg)                             :: NTHREADS, TID
! HDF5 variables
integer(kind=irg)                             :: hdferr, dims4(4), istat
type(HDFobjectStackType)                      :: HDF_head
character(fnlen)                              :: groupname, dataset, instring, &
                                                 dataname, fname, sourcefile, datagroupname, datafile
interface
    recursive subroutine CalcLghSM(cell,DynMat,nn,lambdaZ,nt,dthick,Lgh) 

        use local
        use io
        use files
        use diffraction
        use constants
        use math

        IMPLICIT NONE

        type(unitcell)                      :: cell
        complex(kind=dbl),INTENT(IN)        :: DynMat(nn,nn)
        integer(kind=sgl),INTENT(IN)        :: nn
        real(kind=sgl),INTENT(IN)           :: lambdaZ(1:nt)
        integer(kind=sgl),INTENT(IN)        :: nt
        complex(kind=dbl),INTENT(OUT)       :: Lgh(nn,nn,nt)
        real(kind=sgl),INTENT(IN)           :: dthick

    end subroutine CalcLghSM

end interface


!!$OMP THREADPRIVATE(rlp,cell) 

call timestamp(datestring=dstr, timestring=tstrb)
tstre = tstrb

call Time_tick(tickstart)
call Time_tick(tick)

! nullify HDF pointer
nullify(HDF_head%next)

! generate xtal structure data
!nullify(cell)        
!allocate(cell)        

! get the crystal strucutre from the *.xtal file
verbose = .TRUE.
dmin = sngl(enl%dmin)
EkeV = sngl(enl%Ekev)
xtalname = trim(enl%xtalname)
call Initialize_Cell(cell,Dyn, rlp, xtalname, dmin, EkeV, verbose)

! open HDF5 interface
call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
datafile = EMsoft_toNativePath(datafile)

hdferr =  HDF_createFile(datafile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createFile ')

! write the EMheader to the file
datagroupname = 'EBSD'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLfiles')

! read the text file and write the array to the file
dataset = SC_EMEBSDFullNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetTextFile EMEBSDNML')

call HDF_pop(HDF_head)

groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EMData')
hdferr = HDF_createGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EBSD')

dataset = SC_numangles
hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger numangles')

! and add the Euler angles to the output file
allocate(eulerangles(3,numangles))
do i=1,numangles
  eulerangles(1:3,i) = qu2eu(angles%quatang(1:4,i))
end do
dataset = SC_Eulerangles
hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerangles, 3, numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloatArray2D Eulerangles')


! then calculate density, average atomic number and average atomic weight
call CalcDensity(cell, dens, avZ, avA)
density = dble(dens)
Ze = dble(avZ)
at_wt = dble(avA)
io_real(1:3) = (/ dens, avZ, avA /)
call WriteValue('Density, avZ, avA = ',io_real,3,"(2f10.5,',',f10.5)")
! number of steps pre-set to 300
steps = 300


! parameter used for binning MC results
alpha = 0.5 * cPi - (enl%sig - enl%thetac)*dtoR
tana = tan(alpha)
cota = 1.0/tana
sa = sin(alpha)
ca = cos(alpha)

sig = enl%sig*dtoR
omega = enl%omega*dtoR
globalworkgrpsz = enl%globalworkgrpsz
num_el = enl%num_el ! no. of electron simulation by one work item
num_max = globalworkgrpsz*globalworkgrpsz*num_el ! total simulation in one loop
totnum_el_nml = enl%totnum_el
multiplier =  enl%multiplier
totnum_el = totnum_el_nml * multiplier ! total number of electrons to simulate
globalsize = (/ enl%globalworkgrpsz, enl%globalworkgrpsz /)
numEbins =  int((enl%EkeV-enl%Ehistmin)/enl%Ebinsize)+1
numzbins =  int(enl%depthmax/enl%depthstep)+1

!===============================================================================
!========================ALLOCATE ARRAYS HERE===================================
!===============================================================================

allocate(Lamresx(num_max), Lamresy(num_max), depthres(num_max), energyres(num_max), stat=istat)
depthres = 0.0
energyres = 0.0
lamresx = 0.0
lamresy = 0.0

size_in_bytes = num_max*sizeof(EkeV)
size_in_bytes_seeds = 4*globalworkgrpsz*globalworkgrpsz*sizeof(EkeV)

!=====================
! INITIALIZATION
!=====================
call CLinit_PDCCQ(platform, nump, enl%platid, device, numd, enl%devid, info, context, command_queue)

!=====================
! BUILD THE KERNEL
!=====================

sourcefile = 'EMMC.cl'

write (*,*) 'OpenCL source file set to : ',trim(sourcefile)
call CLread_source_file(sourcefile, csource, slength)

! create the program
io_int(1) = slength
call WriteValue('Kernel source length (characters) : ',io_int,1)
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('ComputeFullEBSDPatterns:clCreateProgramWithSource', ierr)

! build the program
! progoptions = '-cl-no-signed-zeros'
! ierr = clBuildProgram(prog, numd, C_LOC(device), C_LOC(progoptions), C_NULL_FUNPTR, C_NULL_PTR)
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(enl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
if(len(trim(source)) > 0) call Message(trim(source(1:cnum)),frm='(A)')
call CLerror_check('ComputeFullEBSDPatterns:clBuildProgram', ierr)
call CLerror_check('ComputeFullEBSDPatterns:clGetProgramBuildInfo', ierr2)

! if we get here, then the program build was successful and we can proceed with the creation of the kernel
call Message('Program Build Successful... Creating kernel')

! finally get the kernel and release the program
kernelname = 'MC'//CHAR(0)
kernel = clCreateKernel(prog, C_LOC(kernelname), ierr)
call CLerror_check('ComputeFullEBSDPatterns:clCreateKernel:MC', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseProgram', ierr)

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
  call FatalError('EMEBSDFull:','insufficient prime number seeds')
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
call CLerror_check('ComputeFullEBSDPatterns:clCreateBuffer:LamX', ierr)

LamY = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('ComputeFullEBSDPatterns:clCreateBuffer:LamY', ierr)

depth = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('ComputeFullEBSDPatterns:clCreateBuffer:depth', ierr)

energy = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('ComputeFullEBSDPatterns:clCreateBuffer:energy', ierr)

seeds = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
call CLerror_check('ComputeFullEBSDPatterns:clCreateBuffer:seeds', ierr)

!call init_random_seed()
ierr = clEnqueueWriteBuffer(command_queue, seeds, CL_TRUE, 0_8, size_in_bytes_seeds, C_LOC(init_seeds(1)), &
                            0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('ComputeFullEBSDPatterns:clEnqueueWriteBuffer', ierr)

call Message('Monte Carlo mode set to full. Performing full calculation...',frm='(A/)')

!===============================================================================
!========================SET KERNEL ARGUMENTS===================================
!===============================================================================

ierr = clSetKernelArg(kernel, 0, sizeof(LamX), C_LOC(LamX))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:LamX', ierr)

ierr = clSetKernelArg(kernel, 1, sizeof(LamY), C_LOC(LamY))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:LamY', ierr)

ierr = clSetKernelArg(kernel, 2, sizeof(EkeV), C_LOC(EkeV))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:EkeV', ierr)

ierr = clSetKernelArg(kernel, 3, sizeof(globalworkgrpsz), C_LOC(globalworkgrpsz))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:globalworkgrpsz', ierr)

ierr = clSetKernelArg(kernel, 4, sizeof(Ze), C_LOC(Ze))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:Ze', ierr)

ierr = clSetKernelArg(kernel, 5, sizeof(density), C_LOC(density))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:density', ierr)

ierr = clSetKernelArg(kernel, 6, sizeof(at_wt), C_LOC(at_wt))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:at_wt', ierr)

ierr = clSetKernelArg(kernel, 7, sizeof(num_el), C_LOC(num_el))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:num_el', ierr)

ierr = clSetKernelArg(kernel, 8, sizeof(seeds), C_LOC(seeds))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:seeds', ierr)

ierr = clSetKernelArg(kernel, 9, sizeof(sig), C_LOC(sig))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:sig', ierr)

ierr = clSetKernelArg(kernel, 10, sizeof(omega), C_LOC(omega))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:omega', ierr)

ierr = clSetKernelArg(kernel, 11, sizeof(depth), C_LOC(depth))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:depth', ierr)

ierr = clSetKernelArg(kernel, 12, sizeof(energy), C_LOC(energy))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:energy', ierr)

ierr = clSetKernelArg(kernel, 13, sizeof(steps), C_LOC(steps))
call CLerror_check('ComputeFullEBSDPatterns:clSetKernelArg:steps', ierr)

! electron count variable
val = 0

mainloop: do i = 1,(totnum_el/num_max+1)

    ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_NULL_PTR, &
                                      0, C_NULL_PTR, C_NULL_PTR)
    call CLerror_check('ComputeFullEBSDPatterns:clEnqueueNDRangeKernel', ierr)

! wait for the commands to finish
    ierr = clFinish(command_queue)
    call CLerror_check('ComputeFullEBSDPatterns:clFinish', ierr)

    ierr = clEnqueueReadBuffer(command_queue,LamX,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresx(1)),0,C_NULL_PTR,C_NULL_PTR)
    call CLerror_check('ComputeFullEBSDPatterns:clEnqueueReadBuffer:Lamresx', ierr)

    ierr = clEnqueueReadBuffer(command_queue,LamY,CL_TRUE,0_8,size_in_bytes,C_LOC(Lamresy(1)),0,C_NULL_PTR,C_NULL_PTR)
    call CLerror_check('ComputeFullEBSDPatterns:clEnqueueReadBuffer:Lamresy', ierr)
        
    ierr = clEnqueueReadBuffer(command_queue,depth,CL_TRUE,0_8,size_in_bytes,C_LOC(depthres(1)),0,C_NULL_PTR,C_NULL_PTR)
    call CLerror_check('ComputeFullEBSDPatterns:clEnqueueReadBuffer:depthres', ierr)
        
    ierr = clEnqueueReadBuffer(command_queue,energy,CL_TRUE,0_8,size_in_bytes,C_LOC(energyres(1)),0,C_NULL_PTR,C_NULL_PTR)
    call CLerror_check('ComputeFullEBSDPatterns:clEnqueueReadBuffer:energyres', ierr)

! add the results to the MC detector arrays for the detector

    subloopfull: do j = 1, num_max
        
        if ((Lamresx(j) .ne. -10.0) .and. (Lamresy(j) .ne. -10.0) &
        .and. (depthres(j) .ne. 10.0) .and. (energyres(j) .ne. 0.0) &
        .and. .not.isnan(Lamresx(j)) .and. .not.isnan(Lamresy(j))) then

            xyz = LambertSquareToSphere((/Lamresx(j), Lamresy(j)/), ierr)

            if(ierr .eq. 0) then
                xyz = xyz/vecnorm(xyz)
            end if
 
            r1 = xyz(1)
            r2 = xyz(2)
            r3 = xyz(3)
            
            rho = (enl%L/enl%delta)*(tana + cota)/(r3/sa + r1/ca)

            r1 = r1*rho
            r2 = r2*rho

            idxy(1) = nint(enl%xpc - r2 + enl%numsx/2.0)
            idxy(2) = nint(enl%ypc - (r1 - (enl%L/enl%delta)*sa)/ca + enl%numsy/2.0)

            if ((idxy(1) .le. enl%numsx) .and. (idxy(1) .ge. 1) .and. (idxy(2) .le. enl%numsy) .and. (idxy(2) .ge. 1)) then
! If Ec larger than Emin, then we should count this electron
                if (energyres(j).gt.enl%Ehistmin) then

                    iE = nint((energyres(j)-enl%Ehistmin)/enl%Ebinsize)+1
! first add this electron to the correct exit distance vs. energy bin (coarser than the angular plot)
                    edis = abs(depthres(j))  ! distance from last scattering point to surface along trajectory
                    iz = nint(edis/enl%depthstep) +1
                    if ( (iz.gt.0).and.(iz.le.numzbins) ) then
                        val = val + 1

                        EBSDdetector%detector(idxy(1),idxy(2))%lambdaEZ(iE,iz) = &
                        EBSDdetector%detector(idxy(1),idxy(2))%lambdaEZ(iE,iz) + 1.0
                    end if
                end if
            end if
        end if
    end do subloopfull

    if (mod(i,50) .eq. 0) then
        io_int(1) = i*num_max
        call WriteValue('Total number of incident electrons = ',io_int,1,'(I15)')

! note that we need to prevent integer overflows !
        io_int(1) = dble(val)
        call WriteValue('Number of BSE electrons intercepted by detector = ',io_int,1,'(I15)')
    end if

end do mainloop

! print information about MC run

io_int(1) = totnum_el
call WriteValue('Total number of incident electrons = ',io_int,1,'(I15)')
io_int(1) = val
call WriteValue('Total number of BSE electrons intercepted by detector = ',io_int,1,'(I15)')
io_real(1) = dble(io_int(1))/dble(totnum_el)
call WriteValue('Backscatter yield on detector = ',io_real,1,'(F15.6)')

!
!=====================
! RELEASE EVERYTHING
!=====================

ierr = clReleaseKernel(kernel)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseKernel', ierr)

ierr = clReleaseCommandQueue(command_queue)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseCommandQueue', ierr)

ierr = clReleaseContext(context)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseContext', ierr)

ierr = clReleaseMemObject(LamX)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseMemObject:LamX', ierr)

ierr = clReleaseMemObject(LamY)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseMemObject:LamY', ierr)

ierr = clReleaseMemObject(depth)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseMemObject:depth', ierr)

ierr = clReleaseMemObject(energy)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseMemObject:energy', ierr)

ierr = clReleaseMemObject(seeds)
call CLerror_check('ComputeFullEBSDPatterns:clReleaseMemObject:seeds', ierr)

!===========================================================================================
!============================START DYNAMICAL CALCULATIONS===================================
!===========================================================================================

! some parameters
czero = (0.D0,0.D0)
gzero = 1  ! index of incident beam
numset = cell%ATOM_ntype  
verbose = .FALSE.

! intensity prefactor
prefactor = 0.25D0 * nAmpere * enl%beamcurrent * enl%dwelltime * 1.0D-15/ dble(totnum_el)

! normalize get weight factors for each pixel
do i = 1,enl%numsx
    do j = 1,enl%numsy
        EBSDdetector%detector(i,j)%lambdaEZ = &
          EBSDdetector%detector(i,j)%lambdaEZ/dble(totnum_el + num_max)
    end do
end do

!=============================================
!=============================================
! this is where we determine the value for the thickness integration limit for the CalcLgh3 routine...
allocate(EkeVs(numEbins),thick(numEbins,enl%numsx,enl%numsy))
allocate(lambdas(enl%numsx,enl%numsy,numEbins,numzbins),nabsfact(numzbins))
lambdas = 0.0
thick = 0.0
EkeVs = 0.0
nabsfact = 0.0

call CalcUcg(cell,rlp,(/0,0,0/))
nabsl = rlp%xgp

do i = 1,numzbins
    nabsfact(i) = exp(2.0*sngl(cPi)*(i-1)*enl%depthstep/nabsl)
end do


do i=1,numEbins
  EkeVs(i) = enl%Ehistmin + float(i-1)*enl%Ebinsize
end do

do i = 1,enl%numsx
    do j = 1,enl%numsy
       izzmax = 0

        do iE = 1,numEbins
            lambdas(i,j,iE,1:numzbins) = EBSDdetector%detector(i,j)%lambdaEZ(iE,1:numzbins)
            lambdas(i,j,iE,1:numzbins) = lambdas(i,j,iE,1:numzbins)*nabsfact
            izz = 1
            do while(sum(EBSDdetector%detector(i,j)%lambdaEZ(iE,izz:numzbins)) .gt. 0.0)
                izz = izz + 1
            end do
            !if(izz .gt. izzmax) izzmax = izz
            thick(iE,i,j) = izz
        end do
    end do
end do

! allocate main EBSD array
allocate(EBSDPatterns(enl%numsx,enl%numsy,numEbins,numangles))
EBSDPatterns = 0.0

! allocate and compute the Sgh loop-up table
call Initialize_SghLUT(cell, dmin, numset, nat, verbose)

! force dynamical matrix routine to read new Bethe parameters from file
! this will all be changed with the new version of the Bethe potentials
call Set_Bethe_Parameters(BetheParameters,.TRUE.)

write (*,*) 'Starting parallel section'
call OMP_SET_NUM_THREADS(enl%nthreads)
io_int(1) = nthreads
call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")
temp1 = 1.0D+9*cPlanck/dsqrt(2.D0*cRestmass*cCharge)



do iang = 1,numangles

    qu(1:4) = angles%quatang(1:4,iang)

    do iE = numEbins,1,-1

        EkeV = EkeVs(iE) !(iE - 1)*enl%Ebinsize + enl%Ehistmin

!$OMP PARALLEL default(SHARED) PRIVATE(TID, i, ix, j, k, kk, kkk, nref, lambdaZ, myrlp, temp2, mLambda) &
!$OMP& PRIVATE(FN, reflist, firstw, nns, nnw, DynMat, Sgh, Lghtmp, nat, kn, svals, mRelcor, mPsihat, hkl) 

        temp2 = cCharge*0.5D0*EkeV*1000.D0/cRestmass/(cLight**2)
! relativistic correction factor (known as gamma)      
        mRelcor = 1.0D0+2.0D0*temp2
! relativistic acceleration voltage
        mPsihat = EkeV*(1.D0+temp2)*1000.D0
! correct for refraction        
        hkl=(/0,0,0/)
        myrlp%method='WK'
        call CalcUcg(cell,myrlp,hkl) 
        mPsihat = mPsihat + dble(myrlp%Vmod)
        mLambda = temp1/dsqrt(mPsihat)

!       call CalcWaveLength(cell, myrlp, skip) 

        TID = OMP_GET_THREAD_NUM()
        if (iE .eq. numEbins) then
            if (TID.eq.0) then
              io_int(1) = OMP_GET_NUM_THREADS()
              call WriteValue(' Number of threads set to',io_int, 1, "(2I8)") 
              write (*,*) ' wavelength ', mLambda
            end if
        end if

        allocate(svals(numset),lambdaZ(numzbins),stat=istat)

!$OMP DO SCHEDULE(DYNAMIC)
        do k = 1,enl%numsx*enl%numsy

            i = mod(k,enl%numsy) + 1
            j = (k-1)/enl%numsy + 1

            lambdaZ = 0.0
            lambdaZ(1:numzbins) = EBSDdetector%detector(i,j)%lambdaEZ(iE,1:numzbins)
            lambdaZ = lambdaZ*nabsfact

! get the incident wavevector of the electron in xtal frame
            kk = EBSDdetector%detector(i,j)%dc(1:3)           
            kk = quat_Lp(conjg(qu), kk) 
            kk = kk/mLambda

            call TransSpace(cell,kk,kkk,'c','r')
            FN = kkk
            nullify(reflist)
            call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, kkk, dmin, nref, verbose)
                
            nullify(firstw)
            nns = 0
            nnw = 0
            call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, nref, nns, nnw)

! generate the dynamical matrix
            if(allocated(DynMat)) deallocate(DynMat)
            allocate(DynMat(nns,nns))
            DynMat = czero

            call GetDynMat(cell, reflist, firstw, myrlp, DynMat, nns, nnw)
               
! then we need to initialize the Sgh and Lgh arrays
            if (allocated(Sgh)) deallocate(Sgh)
            if (allocated(Lghtmp)) deallocate(Lghtmp)
            allocate(Sgh(nns,nns,numset),Lghtmp(nns,nns,numzbins))

            !if (allocated(Lgh)) deallocate(Lgh)
            !allocate(Sgh(nns,nns,numset),Lgh(nns,nns))
            !Sgh = czero
            !Lgh = czero
            Sgh = czero
            Lghtmp = czero

            call getSghfromLUT(cell,reflist,nns,numset,nat,Sgh)

! solve the dynamical eigenvalue equation for this beam direction  
            kn = 1.0/cell%mLambda
            call CalcLghSM(cell,DynMat,nns,lambdaZ,numzbins,sngl(enl%depthstep),Lghtmp)

            !call CalcLgh(DynMat,Lgh,enl%depthmax,kn,nns,gzero,enl%depthstep,&
            !lambdaZ(1:enl%numzbins),enl%numzbins)

! sum over the element-wise (Hadamard) product of the Lgh and Sgh arrays 
             svals = 0.0
             do ix=1,numset
                 !svals(ix) = real(sum(Lgh(1:nns,1:nns)*Sgh(1:nns,1:nns,ix)))
                 svals(ix) = real(sum(Lghtmp(1:nns,1:nns,numzbins)*Sgh(1:nns,1:nns,ix)))
             end do
             svals = svals/float(sum(nat(1:numset)))
             
             EBSDPatterns(i,j,iE,iang) = sum(svals)*prefactor
             call Delete_gvectorlist(reflist)

             if (mod(k,10000).eq.0) then 
               io_int(1) = k 
               io_int(2) = enl%numsx*enl%numsy
               call WriteValue(' completed ',io_int,2,"(I8,' of ',I8)")
             end if
         end do
!$OMP END DO
         deallocate(svals, lambdaZ)
!$OMP END PARALLEL
    io_int(1) = iE
    io_int(2) = iang
    call WriteValue('  completed energy bin for angle ',io_int, 2, "(2I8)") 
    end do
end do

tstop = Time_tock(tickstart) 
tock = Time_tock(tick)

io_real(1) = tstop
call WriteValue('Execution time [CPU_TIME()] = ',io_real, 1)

io_int(1) = tock
call WriteValue('Execution time [system_clock()] = ',io_int,1,"(I8,' [s]')")

! add data to the hyperslab
dataset = SC_EBSDPatterns
dims4 = (/  enl%numsx, enl%numsy, numEbins, numangles /)
hdferr = HDF_writeDatasetFloatArray4D(dataset, EBSDPatterns, dims4(1), dims4(2), dims4(3),&
dims4(4), HDF_head)

dataset = SC_Lambdas
dims4 = (/  enl%numsx, enl%numsy, numEbins, numzbins /)
hdferr = HDF_writeDatasetFloatArray4D(dataset, lambdas, dims4(1), dims4(2), dims4(3),&
dims4(4), HDF_head)

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

call timestamp(datestring=dstr, timestring=tstre)
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup EMheader')

datagroupname = "EBSD"
hdferr = HDF_openGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup EBSD')

! stop time /EMheader/StopTime 'character'
dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetStringArray StopTime')

dataset = SC_Duration
hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloat Duration')

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)


end subroutine ComputeFullEBSDPatterns

!--------------------------------------------------------------------------
!
! SUBROUTINE:CalcLghSM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief integrate the Bloch wave function over the foil thickness
!
!> @param cell unitcell pointer
!> @param DynMat Dynamical Matrix 
!> @param nn number of strong beams
!> @param nt number of thickness values
!> @param dthick integration step size
!> @param Lgh output array
!
!> @date 11/18/13  MDG 1.0 major rewrite from older ECP program; merged with ECPz
!> @date 03/03/14  MDG 2.0 version that works with the scattering matrix...
!> @date 02/02/17  SS  2.1 updated for cell and DynMat type
!--------------------------------------------------------------------------
recursive subroutine CalcLghSM(cell,DynMat,nn,lambdaZ,nt,dthick,Lgh) 

use local
use io
use files
use diffraction
use constants
use math

IMPLICIT NONE

type(unitcell)                      :: cell
complex(kind=dbl),INTENT(IN)        :: DynMat(nn,nn)
integer(kind=sgl),INTENT(IN)        :: nn
real(kind=sgl),INTENT(IN)           :: lambdaZ(1:nt)
integer(kind=sgl),INTENT(IN)        :: nt
complex(kind=dbl),INTENT(OUT)       :: Lgh(nn,nn,nt)
real(kind=sgl),INTENT(IN)           :: dthick

integer                             :: i
complex(kind=dbl),allocatable       :: Minp(:,:),Azz(:,:),ampl(:),ampl2(:)
  
allocate(Minp(nn,nn),Azz(nn,nn),ampl(nn),ampl2(nn))

Minp = DynMat * cmplx(0.D0,cPi * cell%mLambda)
call MatrixExponential(Minp, Azz, dble(dthick), 'Pade', nn)  

ampl = cmplx(0.D0,0.D0)
ampl(1) = cmplx(1.0D0,0.D0)
Lgh = cmplx(0.D0,0.D0)

! add some thickness handling here !!!
! thge integration uses a small step size, but there might
! be only a small number of thicknesses for which output is
! requested !

do i=1,nt
    ampl2 = matmul(Azz,ampl)
    if (i.eq.1) then 
        Lgh(1:nn,1:nn,i) = lambdaZ(i)*spread(ampl2(1:nn),dim=2,ncopies=nn)*&
        spread(conjg(ampl2(1:nn)),dim=1,ncopies=nn)
    else
        Lgh(1:nn,1:nn,i) = Lgh(1:nn,1:nn,i-1)+lambdaZ(i)*spread(ampl2(1:nn),dim=2,ncopies=nn)*&
        spread(conjg(ampl2(1:nn)),dim=1,ncopies=nn)
    end if
    ampl = ampl2
end do
  
deallocate(Minp,Azz,ampl,ampl2)

end subroutine CalcLghSM
