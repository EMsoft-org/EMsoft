! ###################################################################
! Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDmasterOpenCL.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDmasterOpenCL
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief ebsd master pattern on using scattering matrix GPU
!
!> @date 03/11/15 SS  1.0 original
!> @date 04/03/15 MDG 2.0 converted input/output to HDF format
!> @date 05/21/16 MDG 2.1 changes for new HDF internal organization
!> @date 12/10/16 MDG 2.2 updated for new OpenCL module
!--------------------------------------------------------------------------

program EMEBSDmasterOpenCL

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                                :: nmldeffile, progname, progdesc
type(EBSDMasterOpenCLNameListType)              :: emnl

nmldeffile = 'EMEBSDmasterOpenCL.nml'
progname = 'EMEBSDmasterOpenCL.f90'
progdesc = 'EBSD Energy-dependent Master Pattern Simulation using OpenCL on GPU'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 0, 28 /), progname)

! deal with the namelist stuff
call GetEBSDMasterOpenCLNameList(nmldeffile,emnl)

! perform the zone axis computations
call EBSDmasterpatternOpenCL(emnl, progname, nmldeffile)

end program EMEBSDmasterOpenCL

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDmasterpatternOpenCL
!
!> @author Marc De Graef/Saransh Singh, Carnegie Mellon University
!
!> @brief compute a master electron channeling pattern using scattering matrix on a GPU
!
!> @note Rewrite of the older ECPmaster program to perform the calculations on
!> a GPU. The bethe potential approximation combined with the GPU should really
!> things up a lot!
!
!> @param emnl name list structure
!> @param progname program name
!
!> @date 03/11/15  SS 1.0 original
!> @date 05/05/15 MDG 1.1 removed getenv() call; replaced by global path string
!> @date 04/18/16  SS 2.0 converted to clfortran instead of fortrancl
!> @date 12/10/16 MDG 2.1 updated for new OpenCL module
!> @date 03/04/21 MDG 3.0 revisions to bring code up to date with EMsoft 5.3 libraries
!> @date 03/07/21 MDG 3.1 debugging to fix error for large unit cells 
!--------------------------------------------------------------------------
subroutine EBSDmasterpatternOpenCL(emnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use crystal
use symmetry
use Lambert
use initializers
use initializersHDF
use constants
use gvectors
use kvectors
use error
use io
use math
use NameListHDFwriters
use EBSDmod
use files
use diffraction
use MBModule
use HDF5
use HDFsupport
use ISO_C_BINDING
use CLsupport
use omp_lib
use stringconstants
use multibeams
use timing

IMPLICIT NONE

type(EBSDMasterOpenCLNameListType),INTENT(INOUT):: emnl
character(fnlen),INTENT(IN)                     :: progname
character(fnlen),INTENT(IN)                     :: nmldeffile

real(kind=dbl)                                  :: frac
integer(kind=irg)                               :: gzero, istat

integer(kind=irg)                               :: numEbins, numzbins, nx, ny, totnum_el ! reading from MC file
real(kind=dbl)                                  :: EkeV, Ehistmin, Ebinsize, depthmax, depthstep, sig, omega, xyz(3), Radius  ! reading from MC file
integer(kind=irg), allocatable                  :: accum_e(:,:,:), accum_z(:,:,:,:), acc_z(:,:,:,:) ! reading from MC file

integer(kind=irg)                               :: io_int_sgl(1), io_int(6) ! integer output variable
real(kind=dbl)                                  :: io_real(5), xy(2), dx, dxm, dy, dym, dc(3), edge 

integer(kind=irg)                               :: i, j, isym, pgnum,ix,iy, nix, niy, nixp, niyp, numsites ! variables for point group and Laue group
integer(kind=irg),parameter                     :: LaueTest(11) = (/ 149, 151, 153, 156, 158, 160, 161, 164, 165, 166, 167 /)  ! space groups with 2 or mirror at 30 degrees
integer(kind=irg)                               :: npyhex, ijmax, numk, skip, timestart, timestop ! parameters for calckvectors and calcwavelength subroutine

integer(kind=irg)                               :: ga(3), gb(3) ! shortest reciprocal lattice vector for zone axis
real(kind=sgl), allocatable                     :: thick(:), svals(:), lambdaE(:,:),klist(:,:),knlist(:), masterSPNH(:,:,:), &
                                                   masterSPSH(:,:,:)
real(kind=sgl),allocatable                      :: mLPNH(:,:,:,:), mLPSH(:,:,:,:), auxNH(:,:,:,:), auxSH(:,:,:,:), Z2percent(:)
real(kind=dbl)                                  :: intthick
complex(kind=dbl),allocatable                   :: Lgh(:,:),Sgh(:,:),Sghtmp(:,:,:)
complex(kind=dbl),allocatable                   :: DynMat(:,:)
complex(kind=dbl)                               :: czero
real(kind=sgl)                                  :: tstart, tstop, bp(4), selE, dens, avA, avZ, etotal, scl
integer(kind=irg)                               :: nt, nns, nnw, tots, totw ! thickness array and BetheParameters strong and weak beams
real(kind=sgl)                                  :: FN(3), k(3), fnat, kn
integer(kind=irg)                               :: numset, nref, ipx, ipy, ipz, iequiv(3,48), nequiv, &
                                                   ip, jp, izz, iE, iz, one,ierr,ierr2,ss, Estart, lastEnergy
integer(kind=irg)                               :: izzmax, SamplingType, TID, NUMTHREADS
integer(kind=irg),allocatable                   :: kij(:,:) 
integer(kind=irg)                               :: nat(maxpasym)
real(kind=dbl)                                  :: res(2)

character(fnlen)                                :: oldprogname, energyfile, outname
character(fnlen)                                :: xtalname
character(8)                                    :: MCscversion
character(4)                                    :: MCmode
character(6)                                    :: projtype

logical                                         :: verbose, usehex, switchmirror
type(unitcell)                                  :: cell
type(gnode),save                                :: rlp
type(DynType)                                   :: Dyn
type(kvectorlist), pointer                      :: khead,ktmp,kheadtmp ! linked list for incident wave vectors for master list
type(kvectorlist), pointer                      :: kheadcone,ktmpcone ! linked list for incident wave vectors for individual pattern
real(kind=dbl),allocatable                      :: ecpattern(:,:)
type(BetheParameterType)                        :: BetheParameters
type(reflisttype),pointer                       :: reflist, firstw,rltmp

integer(kind=8)                                 :: size_in_bytes,size_in_bytes_coeff,size_in_bytes_lambda,&
size_in_bytes_dynmatsz,size_in_bytes_wavefn
integer, parameter                              :: iunit = 10
real(kind=4)                                    :: absmax,e,pf,eps
character(fnlen)                                :: info ! info about the GPU
integer(kind=8),TARGET                          :: globalsize(2),localsize(2)
integer, TARGET                                 :: source_length
character(len=50000, KIND=c_char),target        :: csource
integer(c_size_t),target                        :: slength

real(kind=4),allocatable,target                 :: lambdas(:),EkeVs(:)
integer(kind=4)                                 :: numd,nump,irec,ii,jj,kk,pp,qq,ll,mm,npx,npy,npiximgx,npiximgy

complex(kind=4),target                          :: coef(3,3),cvals(9)
integer(kind=4),target                          :: numdepth
integer(kind=4),allocatable,target              :: arrsize(:),arrsizesum(:),offset(:),ns(:)
complex(kind=4),allocatable,target              :: LghCumulative(:),SghCumulative(:,:),A(:)

integer(kind=4)                                 :: size1,size2

character(fnlen)                                :: sourcefile
integer(c_intptr_t),allocatable, target         :: platform(:)
integer(c_intptr_t),allocatable, target         :: device(:)
integer(c_intptr_t),target                      :: context
integer(c_intptr_t),target                      :: command_queue
type(c_ptr), target                             :: psource
integer(c_int32_t)                              :: pcnt
integer(c_intptr_t),target                      :: prog
integer(c_intptr_t),target                      :: kernel
integer(c_intptr_t),target                      :: cl_result
character(19),target                            :: progoptions
integer(c_size_t)                               :: cnum
character(len=50000),target                     :: source
integer(c_intptr_t),target                      :: cl_expA,cl_Amat,cl_AA,cl_AAA,cl_coeff,cl_T1,cl_T2,&
                                                   cl_wavefncoeff,cl_wavefncoeffintd,&
                                                   cl_offset,cl_arrsize,cl_arrsizesum,cl_sqrsize,cl_lambdas
character(7),target                             :: kernelname
character(8, KIND=c_char),target                :: ckernelname

type(EBSDMCdataType)                            :: EBSDMCdata
type(MCCLNameListType)                          :: mcnl

character(11)                                   :: dstr
character(15)                                   :: tstrb
character(15)                                   :: tstre
character(fnlen,kind=c_char)                    :: line2(1)
logical                                         :: overwrite=.TRUE., insert=.TRUE., g_exists
integer(HSIZE_T)                                :: dims4(4),cnt4(4),offset4(4)
integer(HSIZE_T)                                :: dims3(3), cnt3(3), offset3(3)
logical                                         :: f_exists, readonly
character(fnlen, KIND=c_char),allocatable,target:: stringarray(:)
character(fnlen)                                :: dataset, groupname, datagroupname
integer                                         :: hdferr, nlines, nsx, nsy, num_el

type(HDFobjectStackType)                        :: HDF_head

!$OMP THREADPRIVATE(rlp) 

! if copyfromenergyfile is different from 'undefined', then we need to 
! copy all the Monte Carlo data from that file into a new file, which 
! will then be read from and written to by the ComputeMasterPattern routine.
if (emnl%copyfromenergyfile.ne.'undefined') then
  call h5open_EMsoft(hdferr)
  call EBSDcopyMCdata(emnl%copyfromenergyfile, emnl%energyfile, emnl%h5copypath)
  call h5close_EMsoft(hdferr)
end if

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(timestart)

gzero = 1
frac = 0.05
eps = 1.0

source_length = 50000
globalsize = (/ 64, 64 /)
localsize = (/ 4, 4 /)

npiximgx = emnl%npx
npiximgy = emnl%npx
npx = globalsize(1)
npy = globalsize(2)

size_in_bytes_dynmatsz = npx*npy*sizeof(npx)
size_in_bytes_coeff = 9*sizeof(coef(1,1))


cvals(1) = cmplx(-3.3335514852690488032942739163345055,0.0)
cvals(2) = cmplx(-3.0386480729366970892124687564926859,-1.5868011957588383288038677051222921)
cvals(3) = cmplx(-3.0386480729366970892124687564926859,+1.5868011957588383288038677051222921)
cvals(4) = cmplx(-2.1108398003026547374987047865183922,-3.0899109287255009227777015426228801)
cvals(5) = cmplx(-2.1108398003026547374987047865183922,+3.0899109287255009227777015426228801)
cvals(6) = cmplx(-0.38106984566311299903129424501333242,-4.3846445331453979503692027283066828)
cvals(7) = cmplx(-0.38106984566311299903129424501333242,+4.3846445331453979503692027283066828)
cvals(8) = cmplx(2.6973334615369892273896047461916633,-5.1841620626494141778340870727109629)
cvals(9) = cmplx(2.6973334615369892273896047461916633,+5.1841620626494141778340870727109629)

coef(1,1) = cvals(1)+cvals(2)+cvals(3)
coef(2,1) = cvals(1)*cvals(2)+cvals(2)*cvals(3)+cvals(3)*cvals(1)
coef(3,1) = cvals(1)*cvals(2)*cvals(3)

coef(1,2) = cvals(4)+cvals(5)+cvals(6)
coef(2,2) = cvals(4)*cvals(5)+cvals(5)*cvals(6)+cvals(6)*cvals(4)
coef(3,2) = cvals(4)*cvals(5)*cvals(6)

coef(1,3) = cvals(7)+cvals(8)+cvals(9)
coef(2,3) = cvals(7)*cvals(8)+cvals(8)*cvals(9)+cvals(9)*cvals(7)
coef(3,3) = cvals(7)*cvals(8)*cvals(9)

!=============================================
!=============================================
! ---------- read Monte Carlo .h5 output file and extract necessary parameters
call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(emnl%energyfile, mcnl, hdferr, EBSDMCdata, getAccumz=.TRUE.)
call h5close_EMsoft(hdferr)

nsx = (mcnl%numsx - 1)/2
nsy = nsx
etotal = float(EBSDMCdata%totnum_el)

io_int(1) = EBSDMCdata%totnum_el
call WriteValue(' --> total number of BSE electrons in MC data set ', io_int, 1)
!=============================================
!=============================================



! print some information to the console
io_int(1:5) = (/ numEbins, numzbins, nsx, nsy, num_el /)
call WriteValue(' NumEbins, numzbins, nsx, nsy, num_el',io_int,5,"(4I8,', ',I14)")

io_real(1:5) = (/ EkeV, Ehistmin, Ebinsize, depthmax, depthstep /)
call WriteValue(' EkeV, Ehistmin, Ebinsize, depthmax, depthstep ',io_real,5,"(4F10.5,',',F10.5)")

call Message(' -> completed reading '//trim(emnl%energyfile), frm = "(A//)")

!=============================================
!=============================================
!=============================================
! should we create a new file or open an existing file?
!=============================================
  energyfile = trim(EMsoft_getEMdatapathname())//trim(emnl%energyfile)
  energyfile = EMsoft_toNativePath(energyfile)
!=============================================
!=============================================

! load the crystal structure and compute the Fourier coefficient lookup table
verbose = .TRUE.
if (emnl%restart.eqv..FALSE.) then 
  call Initialize_Cell(cell,Dyn,rlp, mcnl%xtalname, emnl%dmin, sngl(mcnl%EkeV), & 
                       nthreads=emnl%nthreads, verbose=verbose)
end if

! check the crystal system and setting; abort the program for trigonal with rhombohedral setting with
! an explanation for the user

if ((cell%xtal_system.eq.5).and.(cell%b.eq.cell%c)) then 
    call Message('')
    call Message(' ========Program Aborted========')
    call Message(' The EBSD master pattern simulation for rhombohedral/trigonal structures')
    call Message(' requires that the structure be described using the hexagonal reference')
    call Message(' frame.  Please re-enter the crystal structure in this setting and re-run')
    call Message(' the Monte Carlo calculation and this master pattern program.')
    call Message('')
    stop
end if

! then calculate density, average atomic number and average atomic weight
call CalcDensity(cell, dens, avZ, avA, Z2percent)


! determine the point group and Laue group number
j=0
do i=1,32
    if (SGPG(i).le.cell%SYM_SGnum) j=i
end do
isym = j
pgnum = j

SamplingType = PGSamplingType(isym)

! next, intercept the special cases (hexagonal vs. rhombohedral cases that require special treatment)
if ((SamplingType.eq.-1).or.(isym.eq.14).or.(isym.eq.26)) then 
  SamplingType = getHexvsRho(cell,isym)
end if 

! if the point group is trigonal or hexagonal, we need to switch usehex to .TRUE. so that
! the program will use the hexagonal sampling method
usehex = .FALSE.
if ((cell%xtal_system.eq.4).or.(cell%xtal_system.eq.5)) usehex = .TRUE.

! ---------- end of symmetry and crystallography section
!=============================================
!=============================================

write (*,*) ' Laue group # ',isym, PGTHD(j)

if(usehex)  npyhex = nint(2.0*float(emnl%npx)/sqrt(3.0))
ijmax = float(emnl%npx)**2   ! truncation value for beam directions

czero = cmplx(0.D0, 0.D0)
! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters,.TRUE.,emnl%BetheParametersFile)

nullify(reflist)
nullify(firstw)

nns = 0
nnw = 0
tots = 0
totw = 0

numset = cell % ATOM_ntype  ! number of special positions in the unit cell
izz = numzbins

!=============================================
!=============================================
! if the combinesites parameter is .TRUE., then we only need to 
! allocate a dimension of 1 in the master pattern array since we are adding 
! together the master patterns for all sites in the asymmetric unit.
  if (emnl%combinesites.eqv..TRUE.) then
    numsites = 1
  else
    numsites = numset
  end if

allocate(svals(numset),stat=istat)
numdepth = 0
svals = 0.0

allocate(mLPNH(-emnl%npx:emnl%npx,-emnl%npx:emnl%npx,1,1:numsites),stat=istat)
allocate(mLPSH(-emnl%npx:emnl%npx,-emnl%npx:emnl%npx,1,1:numsites),stat=istat)
allocate(masterSPNH(-emnl%npx:emnl%npx,-emnl%npx:emnl%npx,1))
allocate(masterSPSH(-emnl%npx:emnl%npx,-emnl%npx:emnl%npx,1))

mLPNH = 0.0
mLPSH = 0.0
masterSPNH = 0.0
masterSPSH = 0.0

allocate(EkeVs(EBSDMCdata%numEbins),thick(EBSDMCdata%numEbins))
numEbins = EBSDMCdata%numEbins

do i=1,EBSDMCdata%numEbins
  EkeVs(i) = mcnl%Ehistmin + float(i-1)*mcnl%Ebinsize
end do

! then, for each energy determine the 95% histogram thickness
izzmax = 0

do iE = 1,numEbins
    do ix=-nsx/10,nsx/10
        do iy=-nsy/10,nsy/10
            istat = sum(EBSDMCdata%accum_z(iE,:,ix,iy))
            izz = 1
            do while (sum(EBSDMCdata%accum_z(iE,1:izz,ix,iy)).lt.(0.95*istat))
                izz = izz+1
            end do
            if (izz.gt.izzmax) izzmax = izz
        end do
    end do
    thick(iE) = dble(izzmax) * mcnl%depthstep
end do

izz = nint(maxval(thick)/mcnl%depthstep)
depthstep = mcnl%depthstep

allocate(lambdaE(1:numEbins,1:izz),stat=istat)
allocate(lambdas(1:izz),stat=istat)

do iE=1,numEbins
    do iz=1,izz
        lambdaE(iE,iz) = float(sum(EBSDMCdata%accum_z(iE,iz,:,:)))/float(sum(EBSDMCdata%accum_z(:,:,:,:)))
    end do
end do

! and get rid of the EBSDMCdata%accum_z array
deallocate(EBSDMCdata%accum_z)

size_in_bytes_lambda = izz*sizeof(lambdas(1))

nat = 0
do ip=1,cell % ATOM_ntype
    nat(ip) = cell%numat(ip)
end do
fnat = 1.0/float(sum(nat(1:numset)))
intthick = dble(depthmax)

!================================
! INITIALIZATION OF OpenCL DEVICE
!================================
call CLinit_PDCCQ(platform, nump, emnl%platid, device, numd, emnl%devid, info, context, command_queue)
print*,'Name of device :',trim(info)

! read the cl source file
sourcefile = 'MBmoduleOpenCL.cl'
write (*,*) 'OpenCL source file set to : ',trim(sourcefile)
call CLread_source_file(sourcefile, csource, slength)
io_int(1) = slength
call WriteValue('Kernel source length (characters) : ',io_int,1)

!=====================
! BUILD THE KERNEL
!=====================

! create the program
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('EBSDmasterpatternOpenCL:clCreateProgramWithSource', ierr)

! build the program
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(1), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
if(cnum > 1) call Message(trim(source(1:cnum))//'test',frm='(A)')
call CLerror_check('EBSDmasterpatternOpenCL:clBuildProgram', ierr)
call CLerror_check('EBSDmasterpatternOpenCL:clGetProgramBuildInfo', ierr2)

! if we get here, then the program build was successful and we can proceed with the creation of the kernel
call Message('Program Build Successful... Creating kernel')

! finally get the kernel and release the program
kernelname = 'CalcLgh'
ckernelname = kernelname
ckernelname(8:8) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('EBSDmasterpatternOpenCL:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('EBSDmasterpatternOpenCL:clReleaseProgram', ierr)

allocate(arrsize(npx*npy),arrsizesum(npx*npy),offset(npx*npy),ns(npx*npy),stat=istat)

!=============================================
! should we create a new file or open an existing file?
!=============================================
lastEnergy = -1
outname = trim(EMsoft_getEMdatapathname())//trim(emnl%energyfile)
outname = EMsoft_toNativePath(outname)

if (emnl%restart.eqv..TRUE.) then
! in this case we need to check whether or not the file exists, then open
! it and read the value of the last energy level that was simulated and written
! to that file; if this level is different from the lowest energy level we 
! know that there is at least one more level to be simulated.  If it is equal,
! then we can abort the program here.

  inquire(file=trim(EMsoft_getEMdatapathname())//trim(emnl%energyfile), exist=f_exists)
  if (.not.f_exists) then 
    call FatalError('EBSDmasterpatternOpenCL','restart HDF5 file does not exist')
  end if
write(*,*) 'output file name = ',trim(outname)  
!=============================================
! open the existing HDF5 file 
!=============================================
  nullify(HDF_head%next)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(outname, HDF_head, readonly)

! all we need to get from the file is the lastEnergy parameter
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_EBSDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_lastEnergy
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, lastEnergy)

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

else
!=============================================
! create the HDF5 output file
!=============================================

    nullify(HDF_head%next)
! Initialize FORTRAN interface.
    call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
    hdferr =  HDF_openFile(outname, HDF_head)

! write the EMheader to the file
groupname = SC_EBSDmaster
    call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
    hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EBSDmasterNML
    hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
    call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
    hdferr = HDF_createGroup(groupname, HDF_head)
    call HDFwriteEBSDMasterOpenCLNameList(HDF_head, emnl)

! leave this group
    call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
    hdferr = HDF_createGroup(groupname, HDF_head)

groupname = SC_EBSDmaster
    hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_xtalname
    allocate(stringarray(1))
    stringarray(1)= trim(xtalname)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  end if

dataset = SC_numset
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head)
  end if

dataset = SC_BetheParameters
    bp = (/ BetheParameters%c1, BetheParameters%c2, BetheParameters%c3, BetheParameters%sgdbdiff /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head)
  end if

dataset = SC_lastEnergy
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, lastEnergy, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetInteger(dataset, lastEnergy, HDF_head)
  end if
  
   if (emnl%Esel.eq.-1) then
dataset = SC_numEbins
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, EBSDMCdata%numEbins, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, EBSDMCdata%numEbins, HDF_head)
    end if

dataset = SC_EkeVs
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, EBSDMCdata%numEbins, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, EBSDMCdata%numEbins, HDF_head)
    end if
  else
dataset = SC_numEbins
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, one, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, one, HDF_head)
    end if
  
dataset = SC_selE
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, selE, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, selE, HDF_head)
    end if
  end if  

dataset = SC_cellATOMtype
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, cell%ATOM_type(1:numset), numset, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, cell%ATOM_type(1:numset), numset, HDF_head)
  end if

! create the hyperslabs and write zeroes to them for now
dataset = SC_mLPNH
      dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, ebsdmcdata%numebins, numsites /)
      cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
      offset4 = (/ 0, 0, 0, 0 /)
      call h5lexists_f(hdf_head%next%objectid,trim(dataset),g_exists, hdferr)
      if (g_exists) then 
        hdferr = hdf_writehyperslabfloatarray4d(dataset, mlpnh, dims4, offset4, cnt4, hdf_head, insert)
      else
        hdferr = hdf_writehyperslabfloatarray4d(dataset, mlpnh, dims4, offset4, cnt4, hdf_head)
      end if

dataset = SC_mLPSH
      dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins, numsites /)
      cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
      offset4 = (/ 0, 0, 0, 0 /)
      call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
      if (g_exists) then 
        hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head, insert)
      else
        hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head)
      end if

dataset = SC_masterSPNH
      dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins /)
      cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
      offset3 = (/ 0, 0, 0 /)
      call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
      if (g_exists) then 
        hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)
      else
        hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head)
      end if

dataset = SC_masterSPSH
      dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins /)
      cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
      offset3 = (/ 0, 0, 0 /)
      call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
      if (g_exists) then 
        hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)
      else
        hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head)
      end if

    call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
    call h5close_EMsoft(hdferr)

end if

call Message('Data will be stored in file '//trim(emnl%energyfile), frm = "(A/)")

!=============================================
!=============================================
! figure out what the start energy value is for the energyloop
if (lastEnergy.ne.-1) then
  Estart = lastEnergy-1
  if (Estart.eq.0) then 
    call Message('All energy levels are present in the HDF5 file')
    call Message('No further computations needed.')
    stop 'Program halted.'
  end if
else
  Estart = numEbins
end if

call Time_tick(timestart)

energyloop: do iE = Estart,1,-1

    io_int(1)=iE
    call Message('Starting computation for energy bin (in reverse order)', frm = "(/A)",advance="no")
    call WriteValue(' ',io_int,1,"(I4)",advance="no")
    io_real(1) = EkeVs(iE)
    call WriteValue('; energy [keV] = ',io_real,1,"(F6.2/)")
    selE = EkeVs(iE)

! set the accelerating voltage
    skip = 3
    cell%voltage = dble(EkeVs(iE))
    ! set the accelerating voltage
    if(iE .ne. Estart) then
      verbose = .TRUE.
      call Initialize_Cell(cell,Dyn,rlp,mcnl%xtalname, emnl%dmin, EkeVs(iE), &
                           nthreads=emnl%nthreads, verbose=verbose, initLUT=.TRUE.)
    end if

!=============================================
! generating list of incident wave vectors
!=============================================

! determine all independent incident beam directions (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation;
    nullify(khead)

    if (usehex) then
       call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,emnl%npx,emnl%npx,numk, &
                SamplingType,ijmax,'RoscaLambert',usehex)
    else 
       call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,emnl%npx,emnl%npx,numk, &
                SamplingType,ijmax,'RoscaLambert',usehex)
    end if

    io_int(1)=numk
    call WriteValue('# independent beam directions to be considered = ', io_int, 1, "(I8)")

! allocate klist and put it in array form for OpenMP parallelization
    if (iE .eq. numEbins) then
        allocate(klist(3,numk),stat=istat)
        if(istat .ne. 0) stop 'Cannot allocate klist array'

        allocate(knlist(numk),stat=istat)
        if(istat .ne. 0) stop 'Cannot allocate kn array'

        allocate(kij(3,numk),stat=istat)
        if(istat .ne. 0) stop 'Cannot allocate kij array'

    end if

    klist = 0.0
    kn = 0.0
    kij = 0

    ktmp => khead
    do ii = 1,numk
        klist(1:3,ii) = ktmp%k
        knlist(ii) = ktmp%kn
        kij(1:3,ii) = (/ktmp%i,ktmp%j,ktmp%hs/)
        ktmp => ktmp%next
    end do
    lambdas = 0.0
    lambdas(:) = lambdaE(iE,:)
    numdepth = thick(iE)
!    ktmp => khead
!    kheadtmp => khead

    beamloop : do ii = 1,ceiling(float(numk)/float(npx*npy))
        arrsize = 0
        arrsizesum = 0
        offset = 0
        ns = 0

        if (ii .le. floor(float(numk)/float(npx*npy))) then

! set the number of OpenMP threads 
            call OMP_SET_NUM_THREADS(emnl%nthreads)
            if (ii .eq. 1) then
                io_int(1) = emnl%nthreads
                call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")
            end if

! $OMP PARALLEL DEFAULT(PRIVATE) &
! !$OMP& COPYIN(rlp) &
! $OMP& SHARED(ii,klist,knlist,kij,lambdas,numdepth,arrsize,arrsizesum,offset,ns,npx,npy,cell,BetheParameters) &
! $OMP& SHARED(emnl,SghCumulative,A,eps,numset)

! changed OMP section to default(shared) since it is easier to then identify the private variables
!$OMP PARALLEL DEFAULT(SHARED) COPYIN(rlp) &
!$OMP& PRIVATE(NUMTHREADS, TID, kk, k, FN, reflist, nref, nns, nnw, arrsize, SghCumulative, A, arrsizesum) &
!$OMP& PRIVATE(offset, DynMat, Sghtmp, ss, pp, qq, absmax, e, ns)

            NUMTHREADS = OMP_GET_NUM_THREADS()
            TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC)    
            do kk = 1,npx*npy
                k = klist(1:3,(ii-1)*npx*npy+kk)
                FN = k

                call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, k, emnl%dmin, nref)

! determine strong and weak reflections
                call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, nref, nns, nnw)
                arrsize(kk) = nns
                    
            end do
!$OMP END DO

!$OMP MASTER
            if (allocated(SghCumulative)) deallocate(SghCumulative)
            if (allocated(A)) deallocate(A)

            do kk = 1,npx*npy
                if (kk .lt. npx*npy) then
                    arrsizesum(kk+1) = sum(arrsize(1:kk))
                    offset(kk+1) = sum(arrsize(1:kk)**2)
                end if
            end do

            allocate(SghCumulative(1:offset(npx*npy)+arrsize(npx*npy)**2,numsites),&
            A(1:offset(npx*npy)+arrsize(npx*npy)**2),stat=istat)

!$OMP END MASTER
!$OMP BARRIER

!$OMP DO SCHEDULE(DYNAMIC)    

            do kk = 1,npx*npy

                k = klist(1:3,(ii-1)*npx*npy+kk)
                FN = k

                call Initialize_ReflectionList(cell, reflist, BetheParameters, FN,  k, emnl%dmin, nref)

! determine strong and weak reflections
                call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, nref, nns, nnw)

                if (allocated(DynMat)) deallocate(DynMat)

                allocate(DynMat(nns,nns),stat=istat)

                call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)
                
                if (allocated(Sghtmp)) deallocate(Sghtmp)

                allocate(Sghtmp(nns,nns,numset))

                Sghtmp = cmplx(0.D0,0.D0)
                call CalcSgh(cell,reflist,nns,numsites,Sghtmp,nat)

                do ss = 1,numsites
                    do pp = 1,nns
                        do qq = 1,nns
                           SghCumulative(offset(kk)+(pp-1)*nns+qq,ss) = Sghtmp(pp,qq,ss)
                        end do
                    end do
                end do

                DynMat = DynMat*cmplx(0.0,cPi*cell%mLambda)
                DynMat = DynMat*cmplx(eps,0.0)

                absmax = maxval(abs(DynMat))
                e = ceiling(log(absmax)/log(2.0))
                ns(kk) = e+1

                if (ns(kk) .le. 0) then
                    ns(kk) = 1
                else
                    DynMat = DynMat/2**ns(kk)
                end if

                do pp = 1,nns
                    do qq = 1,nns
                        A(offset(kk)+(pp-1)*nns+qq) = DynMat(pp,qq)
                    end do
                end do

            end do

!!$OMP END DO
!$OMP END PARALLEL

! allocate device memory
            size_in_bytes = (offset(npx*npy)+arrsize(npx*npy)**2)*sizeof(A(1))
            size_in_bytes_wavefn = (arrsizesum(npx*npy)+arrsize(npx*npy))*sizeof(A(1))

            if (allocated(LghCumulative)) deallocate(LghCumulative)
            allocate(LghCumulative(1:offset(npx*npy)+arrsize(npx*npy)**2),stat=istat)

            cl_expA = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:expA', ierr)

            cl_Amat = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:Amat', ierr)

            cl_AA = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:AA', ierr)

            cl_AAA = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:AAA', ierr)

            cl_T1 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:T1', ierr)

            cl_T2 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:T2', ierr)

            cl_wavefncoeff = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_wavefn, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:wavefncoeff', ierr)

            cl_wavefncoeffintd = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_wavefn, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:wavefncoeffintd', ierr)

            cl_coeff = clCreateBuffer(context, CL_MEM_READ_ONLY, size_in_bytes_coeff, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:coeff', ierr)

            cl_arrsize = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dynmatsz, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:arrsize', ierr)

            cl_offset = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dynmatsz, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:offset', ierr)

            cl_arrsizesum = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dynmatsz, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:arrsizesum', ierr)

            cl_sqrsize = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dynmatsz, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:sqrsize', ierr)

            cl_lambdas = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dynmatsz, C_NULL_PTR, ierr)
            call CLerror_check('EBSDmasterpatternOpenCL:clCreateBuffer:lambdas', ierr)

! write list of initial arrays to buffer

            ierr = clEnqueueWriteBuffer(command_queue, cl_Amat, CL_TRUE, 0_8, size_in_bytes, C_LOC(A(1)), &
                                        0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:Amat', ierr)

            ierr = clEnqueueWriteBuffer(command_queue, cl_arrsize, CL_TRUE, 0_8, size_in_bytes_dynmatsz, C_LOC(arrsize(1)), &
                                         0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:arrsize', ierr)

            ierr = clEnqueueWriteBuffer(command_queue, cl_offset, CL_TRUE, 0_8, size_in_bytes_dynmatsz, C_LOC(offset(1)), &
                                         0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:offset', ierr)

            ierr = clEnqueueWriteBuffer(command_queue, cl_arrsizesum, CL_TRUE, 0_8, size_in_bytes_dynmatsz, &
                                         C_LOC(arrsizesum(1)),  0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:arrsizesum', ierr)

            ierr = clEnqueueWriteBuffer(command_queue, cl_sqrsize, CL_TRUE, 0_8, size_in_bytes_dynmatsz, C_LOC(ns(1)), &
                                         0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:sqrsize', ierr)

            ierr = clEnqueueWriteBuffer(command_queue, cl_coeff, CL_TRUE, 0_8, size_in_bytes_coeff, C_LOC(coef(1,1)), &
                                         0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:coeff', ierr)

            ierr = clEnqueueWriteBuffer(command_queue, cl_lambdas, CL_TRUE, 0_8, size_in_bytes_lambda, C_LOC(lambdas(1)), &
                                         0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueWriteBuffer:lambdas', ierr)

! set kernel arguments


            ierr = clSetKernelArg(kernel, 0, sizeof(cl_expA), C_LOC(cl_expA))
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:expA', ierr)

            ierr = clSetKernelArg(kernel, 1, sizeof(cl_Amat), C_LOC(cl_Amat) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:Amat', ierr)

            ierr = clSetKernelArg(kernel, 2, sizeof(cl_AA), C_LOC(cl_AA) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:AA', ierr)

            ierr = clSetKernelArg(kernel, 3, sizeof(cl_AAA), C_LOC(cl_AAA) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:AAA', ierr)

            ierr = clSetKernelArg(kernel, 4, sizeof(cl_arrsize), C_LOC(cl_arrsize) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:arrsize', ierr)

            ierr = clSetKernelArg(kernel, 5, sizeof(cl_coeff), C_LOC(cl_coeff) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:coeff', ierr)

            ierr = clSetKernelArg(kernel, 6, sizeof(cl_T1), C_LOC(cl_T1) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:T1', ierr)

            ierr = clSetKernelArg(kernel, 7, sizeof(cl_T2), C_LOC(cl_T2))
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:T2', ierr)

            ierr = clSetKernelArg(kernel, 8, sizeof(cl_wavefncoeff), C_LOC(cl_wavefncoeff) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:wavefncoeff', ierr)

            ierr = clSetKernelArg(kernel, 9, sizeof(cl_wavefncoeffintd), C_LOC(cl_wavefncoeffintd))
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:wavefncoeffintd', ierr)

            ierr = clSetKernelArg(kernel, 10, sizeof(cl_sqrsize), C_LOC(cl_sqrsize) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:sqrsize', ierr)

            ierr = clSetKernelArg(kernel, 11, sizeof(cl_offset), C_LOC(cl_offset) )
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:offset', ierr)

            ierr = clSetKernelArg(kernel, 12, sizeof(cl_arrsizesum), C_LOC(cl_arrsizesum))
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:arrsizesum', ierr)

            ierr = clSetKernelArg(kernel, 13, sizeof(numdepth), C_LOC(numdepth))
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:numdepth', ierr)

            ierr = clSetKernelArg(kernel, 14, sizeof(cl_lambdas), C_LOC(cl_lambdas))
            call CLerror_check('EBSDmasterpatternOpenCL:clSetKernelArg:lambdas', ierr)

!execute the kernel
            ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize),&
                                        0, C_NULL_PTR, C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueNDRangeKernel', ierr)

! wait for the commands to finish
            ierr = clFinish(command_queue)
            call CLerror_check('EBSDmasterpatternOpenCL:clFinish', ierr)

! read the resulting vector from device memory
            ierr = clEnqueueReadBuffer(command_queue, cl_T1, CL_TRUE, 0_8, size_in_bytes, C_LOC(LghCumulative(1)),&
                                        0,C_NULL_PTR,C_NULL_PTR)
            call CLerror_check('EBSDmasterpatternOpenCL:clEnqueueReadBuffer:LghCumulative', ierr)

! divide by integration depth explicitly (OpenCL kernel giving some problems)
            LghCumulative = LghCumulative/float(izz-1)

            do pp = 1,npx*npy
                ipx = kij(1,(ii-1)*npx*npy+pp)
                ipy = kij(2,(ii-1)*npx*npy+pp)
                ipz = kij(3,(ii-1)*npx*npy+pp)
                size1 = arrsize(pp)
                size2 = offset(pp)
                do kk = 1,numset
                    svals(kk) = real(sum(SghCumulative(size2+1:size2+size1**2,kk)*&
                                      LghCumulative(size2+1:size2+size1**2)))
                end do
! apply 3d point group symmetry
                if (usehex) then 
                    call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv,usehex)
                else

                    if ((cell%SYM_SGnum.ge.195).and.(cell%SYM_SGnum.le.230)) then
                        call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv,cubictype=SamplingType)
                    else
                        call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv)
                    end if
                end if

!$OMP CRITICAL
                if (emnl%combinesites.eqv..FALSE.) then
                   do ix=1,nequiv
                     if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1,1:numset) = svals(1:numset)
                     if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1,1:numset) = svals(1:numset)
                   end do
                else
                   do ix=1,nequiv
                     if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1,1) = sum(svals)
                     if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1,1) = sum(svals)
                   end do
                end if
!$OMP END CRITICAL

            end do

            write(6,'(A,I8,A)') 'Completed ',(ii*npx*npy),' beams.'

            ierr = clReleaseMemObject(cl_expA)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:expA', ierr)

            ierr = clReleaseMemObject(cl_Amat)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:Amat', ierr)

            ierr = clReleaseMemObject(cl_AA)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:AA', ierr)

            ierr = clReleaseMemObject(cl_AAA)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:AAA', ierr)

            ierr = clReleaseMemObject(cl_T1)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:T1', ierr)

            ierr = clReleaseMemObject(cl_T2)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:T2', ierr)

            ierr = clReleaseMemObject(cl_wavefncoeff)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:wavefncoeff', ierr)

            ierr = clReleaseMemObject(cl_wavefncoeffintd)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:wavefncoeffintd', ierr)

            ierr = clReleaseMemObject(cl_coeff)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:coeff', ierr)

            ierr = clReleaseMemObject(cl_arrsize)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:arrsize', ierr)

            ierr = clReleaseMemObject(cl_arrsizesum)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:arrsizesum', ierr)

            ierr = clReleaseMemObject(cl_offset)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:offset', ierr)

            ierr = clReleaseMemObject(cl_lambdas)
            call CLerror_check('EBSDmasterpatternOpenCL:clReleaseMemObject:lambdas', ierr)

        else if (ii .eq. ceiling(float(numk)/float(npx*npy))) then
            write(6,'(A,I8,A)')'Performing computation of last ',MODULO(numk,npx*npy),' beam directions on the CPU'
! set the number of OpenMP threads 
            call OMP_SET_NUM_THREADS(emnl%nthreads)

! old version used default(private) and had some issues with intensities being 
! placed incorrectly in the master pattern arrays; new version uses default(shared)
! $OMP PARALLEL DEFAULT(PRIVATE) &
! $OMP& SHARED(cell,klist,npx,npy,numk,ii,BetheParameters,emnl,knlist,thick,gzero) &
! $OMP& SHARED(depthstep,lambdaE,kij,isym,mLPNH,mLPSH,iE,izz,numsites,nat) 
!$OMP PARALLEL COPYIN(rlp) DEFAULT(SHARED) &
!$OMP& PRIVATE(NUMTHREADS, TID, jj, k, FN, reflist, nref, firstw, nns, nnw, DynMat, istat) &
!$OMP& PRIVATE(Lgh, Sghtmp, kn, kk, svals, ipx, ipy, ipz, iequiv, nequiv, ix)

            NUMTHREADS = OMP_GET_NUM_THREADS()
            TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC)    

            do jj = 1,MODULO(numk,npx*npy)

                k = klist(1:3,(ii-1)*npx*npy+jj)
                FN = k

                call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, k, emnl%dmin, nref)

! determine strong and weak reflections
                nullify(firstw)
                nns = 0
                nnw = 0
                call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, nref, nns, nnw)

                if (allocated(DynMat)) deallocate(DynMat)
                allocate(DynMat(nns,nns),stat=istat)
                call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)

                if (allocated(Lgh)) deallocate(Lgh)
                if (allocated(Sghtmp)) deallocate(Sghtmp)

                allocate(Sghtmp(nns,nns,numsites),Lgh(nns,nns))

                Sghtmp = cmplx(0.D0,0.D0)
                call CalcSgh(cell,reflist,nns,numsites,Sghtmp,nat)

! solve the dynamical eigenvalue equation
                kn = knlist((ii-1)*npx*npy+jj)

                call CalcLgh(DynMat,Lgh,dble(thick(iE)),dble(kn),nns,gzero,depthstep,lambdaE(iE,:),izz)
                deallocate(DynMat)

                do kk = 1,numsites
                    svals(kk) = real(sum(Sghtmp(1:nns,1:nns,kk)*Lgh(1:nns,1:nns)))
                end do

! and store the resulting values
                ipx = kij(1,(ii-1)*npx*npy+jj)
                ipy = kij(2,(ii-1)*npx*npy+jj)
                ipz = kij(3,(ii-1)*npx*npy+jj)

                if (usehex) then 
                    call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv,usehex)
                else
                    if ((cell%SYM_SGnum.ge.195).and.(cell%SYM_SGnum.le.230)) then
                        call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv,cubictype=SamplingType)
                    else
                        call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv)
                    end if
                end if
!$OMP CRITICAL
                if (emnl%combinesites.eqv..FALSE.) then
                   do ix=1,nequiv
                     if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1,1:numsites) = svals(1:numsites)
                     if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1,1:numsites) = svals(1:numsites)
                   end do
                else
                   do ix=1,nequiv
                     if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1,1) = sum(svals)
                     if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1,1) = sum(svals)
                   end do
                end if
!$OMP END CRITICAL
            end do
!$OMP END DO
! and we end the parallel section here (all threads will synchronize).
!$OMP END PARALLEL


        end if
    !end do
    end do beamloop

    call Delete_kvectorlist(khead)

  if (usehex) then
! and finally, we convert the hexagonally sampled array to a square Lambert projection which will be used 
! for all EBSD pattern interpolations;  we need to do this for both the Northern and Southern hemispheres

! we begin by allocating auxiliary arrays to hold copies of the hexagonal data; the original arrays will
! then be overwritten with the newly interpolated data.
    allocate(auxNH(-emnl%npx:emnl%npx,-emnl%npx:emnl%npx,1,1:numsites),stat=istat)
    allocate(auxSH(-emnl%npx:emnl%npx,-emnl%npx:emnl%npx,1,1:numsites),stat=istat)
    auxNH = mLPNH
    auxSH = mLPSH

! 
    edge = 1.D0 / dble(emnl%npx)
    scl = float(emnl%npx) 
    do i=-emnl%npx,emnl%npx
      do j=-emnl%npx,emnl%npx
! determine the spherical direction for this point
        xy = (/ dble(i), dble(j) /) * edge
        dc = LambertSquareToSphere(xy, ierr)
! convert direction cosines to hexagonal Lambert projections
        xy = scl * LambertSphereToHex( dc, ierr )
! interpolate intensity from the neighboring points
        if (ierr.eq.0) then 
          nix = floor(xy(1))
          niy = floor(xy(2))
          nixp = nix+1
          niyp = niy+1
          if (nixp.gt.emnl%npx) nixp = nix
          if (niyp.gt.emnl%npx) niyp = niy
          dx = xy(1) - nix
          dy = xy(2) - niy
          dxm = 1.D0 - dx
          dym = 1.D0 - dy
          mLPNH(i,j,1,1:numsites) = auxNH(nix,niy,1,1:numsites)*dxm*dym + auxNH(nixp,niy,1,1:numsites)*dx*dym + &
                               auxNH(nix,niyp,1,1:numsites)*dxm*dy + auxNH(nixp,niyp,1,1:numsites)*dx*dy
          mLPSH(i,j,1,1:numsites) = auxSH(nix,niy,1,1:numsites)*dxm*dym + auxSH(nixp,niy,1,1:numsites)*dx*dym + &
                               auxSH(nix,niyp,1,1:numsites)*dxm*dy + auxSH(nixp,niyp,1,1:numsites)*dx*dy
        end if
      end do
    end do
    deallocate(auxNH, auxSH)
  end if

! make sure that the outer pixel rim of the mLPSH patterns is identical to
! that of the mLPNH array.
    mLPSH(-emnl%npx,-emnl%npx:emnl%npx,1,1:numsites) = mLPNH(-emnl%npx,-emnl%npx:emnl%npx,1,1:numsites)
    mLPSH( emnl%npx,-emnl%npx:emnl%npx,1,1:numsites) = mLPNH( emnl%npx,-emnl%npx:emnl%npx,1,1:numsites)
    mLPSH(-emnl%npx:emnl%npx,-emnl%npx,1,1:numsites) = mLPNH(-emnl%npx:emnl%npx,-emnl%npx,1,1:numsites)
    mLPSH(-emnl%npx:emnl%npx, emnl%npx,1,1:numsites) = mLPNH(-emnl%npx:emnl%npx, emnl%npx,1,1:numsites)

! get stereographic projections (summed over the atomic positions)
  Radius = 1.0
  do i=-emnl%npx,emnl%npx 
    do j=-emnl%npx,emnl%npx 
      xy = (/ float(i), float(j) /) / float(emnl%npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      xyz = xyz/vecnorm(xyz)
      if (ierr.ne.0) then 
        masterSPNH(i,j,1) = 0.0
        masterSPSH(i,j,1) = 0.0
      else
        masterSPNH(i,j,1) = InterpolateLambert(xyz, mLPNH, emnl%npx, numsites)
        masterSPSH(i,j,1) = InterpolateLambert(xyz, mLPSH, emnl%npx, numsites)
      end if
    end do
  end do

! and here is where the major changes are for version 3.1: all output now in HDF5 format
    call timestamp(datestring=dstr, timestring=tstre)

  datagroupname = 'EBSDmaster'

    nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
  call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
  hdferr =  HDF_openFile(outname, HDF_head)

! update the time string
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  tstop = Time_tock(timestart)
  io_int(1) = tstop
  call WriteValue('Execution time [s]: ',io_int,1)

dataset = SC_Duration
  if (iE.eq.EBSDMCdata%numEbins) then 
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then     
      hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
    end if
  else
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
  end if

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)
  
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

! update the current energy level counter, so that the restart option will function
dataset = SC_lastEnergy
  hdferr = HDF_writeDataSetInteger(dataset, iE, HDF_head, overwrite)

! add data to the hyperslab
dataset = SC_mLPNH
  dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins, numsites /)
  cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
  offset4 = (/ 0, 0, iE-1, 0 /)
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head, insert)

dataset = SC_mLPSH
  dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins, numsites /)
  cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
  offset4 = (/ 0, 0, iE-1, 0 /)
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head, insert)

dataset = SC_masterSPNH
  dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins /)
  cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
  offset3 = (/ 0, 0, iE-1 /)
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)

dataset = SC_masterSPSH
  dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, EBSDMCdata%numEbins /)
  cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
  offset3 = (/ 0, 0, iE-1 /)
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)    

    if ((emnl%Esel.eq.-1).and.(iE.ne.1)) then 
        call Message('Intermediate data stored in file '//trim(outname), frm = "(A/)")
    end if

    if ((emnl%Esel.eq.-1).and.(iE.eq.1)) then 
       call Message('Final data stored in file '//trim(outname), frm = "(A/)")
    end if

end do energyloop

timestop = Time_tock(timestart)
io_int(1) = timestop
call WriteValue('Total execution time [s] ',io_int,1)

if (emnl%Esel.ne.-1) then
  call Message('Final data stored in file '//trim(outname), frm = "(A/)")
end if

end subroutine EBSDmasterpatternOpenCL
