! ###################################################################
! Copyright (c) 2015-2016, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EBSDIndexing.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EBSDIndexing
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief Indexing of EBSD patterns using the dictionary approach. Later, this program
!> will be used for dynamic pattern center correction to hopefully make
!> the dictionary indexing even more robust.
!
!> @date 03/25/15  SS 1.0 original
!> @date 02/02/16 MDG 1.1 significant changes to optimize the code
!> @date 02/04/16 MDG 1.2 added circular mask code
!> @date 03/10/16 MDG 1.3 added h5ebsd formatted output
!> @date 11/14/16 MDG 1.4 added code to read dictionary patterns from h5 file
!--------------------------------------------------------------------------

program EBSDIndexing

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use EBSDmod
use EBSDDImod

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(EBSDIndexingNameListType)              :: ebsdnl
type(EBSDLargeAccumDIType),pointer          :: acc
type(EBSDMasterDIType),pointer              :: master
logical                                     :: verbose
integer(kind=irg)                           :: istat, res

interface
        subroutine MasterSubroutine(ebsdnl,acc,master,progname,nmldeffile)

        use local
        use typedefs
        use NameListTypedefs
        use NameListHandlers
        use files
        use dictmod
        use Lambert
        use others
        use crystal
        use initializersHDF
        use gvectors
        use filters
        use error
        use io
        use diffraction
        use symmetry
        use quaternions
        use constants
        use rotations
        use so3
        use math
        use EBSDmod
        use EBSDDImod
        use clfortran
        use CLsupport
        use omp_lib
        use HDF5
        use h5im
        use h5lt
        use HDFsupport
        use EMh5ebsd
        use NameListHDFwriters
        use ECPmod, only: GetPointGroup
        use Indexingmod
        use ISO_C_BINDING

        IMPLICIT NONE

        type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
        type(EBSDLargeAccumDIType),pointer,INTENT(IN)         :: acc
        type(EBSDMasterDIType),pointer,INTENT(IN)             :: master
        character(fnlen),INTENT(IN)                         :: progname
        character(fnlen),INTENT(IN)                         :: nmldeffile

        end subroutine MasterSubroutine
end interface

nmldeffile = 'EMEBSDDI.nml'
progname = 'EMEBSDDI.f90'
progdesc = 'Program to index EBSD patterns using a dynamically calculated pattern dictionary'
verbose = .TRUE.

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 80 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMEBSDIndexing','JSON input not yet implemented')
!  call JSONreadEBSDIndexingNameList(ebsdnl, nmldeffile, error_cnt)
else
  call GetEBSDIndexingNameList(nmldeffile,ebsdnl)
end if

! 1. read the Monte Carlo data file
allocate(acc)
call EBSDIndexingreadMCfile(ebsdnl, acc)

! 2. read EBSD master pattern file
allocate(master)
call EBSDIndexingreadMasterfile(ebsdnl, master)

! 3. generate detector arrays
allocate(master%rgx(ebsdnl%numsx,ebsdnl%numsy), master%rgy(ebsdnl%numsx,ebsdnl%numsy), &
         master%rgz(ebsdnl%numsx,ebsdnl%numsy), stat=istat)
allocate(acc%accum_e_detector(ebsdnl%numEbins,ebsdnl%numsx,ebsdnl%numsy), stat=istat)

call EBSDIndexingGenerateDetector(ebsdnl, acc, master)
deallocate(acc%accum_e)

! perform the dictionary indexing computations
call MasterSubroutine(ebsdnl,acc,master,progname, nmldeffile)

deallocate(master, acc)

end program EBSDIndexing

!--------------------------------------------------------------------------
!
! SUBROUTINE:MasterSubroutine
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Master subroutine to control dictionary generation, inner products computations, sorting values
!> and indexing of points, all in parallel using OpenCL/openMP
!
!> @param ebsdnl ped indexing namelist pointer
!> @param acc accumulator pointer containing MC results
!> @param master master pattern is read into this pointer
!> @param progname name of the program
!> @param nmldeffile namelist filename
!
!> @date 03/30/15  SS 1.0 original
!> @date 05/05/15 MDG 1.1 removed getenv() call; replaced by global path strings
!> @date 02/07/16 MDG 1.2 added image quality computation
!> @date 02/08/16 MDG 1.3 added confidence index output to HDF5 file
!> @date 02/10/16 MDG 1.4 added average dot product map to HDF5 file
!> @date 02/23/16 MDG 1.5 converted program to CLFortran instead of fortrancl.
!> @date 06/07/16 MDG 1.6 added tmpfile for variable temporary data file name
!> @date 11/14/16 MDG 1.7 added code to read h5 file instead of compute on-the-fly (static mode)
!> @date 07/24/17 MDG 1.8 temporary code to change the mask layout for fast DI tests
!> @date 08/30/17 MDG 1.9 added option to read custom mask from file
!> @date 11/13/17 MDG 2.0 moved OpenCL code from InnerProdGPU routine to main code
!> @date 01/09/18 MDG 2.1 first attempt at OpenMP for pattern pre-processing
!> @date 02/13/18 MDG 2.2 added support for multiple input formats for experimental patterns
!--------------------------------------------------------------------------

subroutine MasterSubroutine(ebsdnl,acc,master,progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use dictmod
use patternmod
use Lambert
use others
use crystal
use initializersHDF
use gvectors
use filters
use error
use io
use diffraction
use symmetry
use quaternions
use constants
use rotations
use so3
use math
use EBSDmod
use EBSDDImod
use clfortran
use CLsupport
use omp_lib
use HDF5
use h5im
use h5lt
use HDFsupport
use EMh5ebsd
use EBSDiomod
use NameListHDFwriters
use ECPmod, only: GetPointGroup
use Indexingmod
use ISO_C_BINDING
use notifications
use TIFF_f90

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
type(EBSDLargeAccumDIType),pointer,INTENT(IN)       :: acc
type(EBSDMasterDIType),pointer,Intent(IN)           :: master
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

type(unitcell),pointer                              :: cell
type(DynType)                                       :: Dyn
type(gnode)                                         :: rlp
logical                                             :: verbose

integer(c_intptr_t),allocatable, target             :: platform(:)
integer(c_intptr_t),allocatable, target             :: device(:)
integer(c_intptr_t),target                          :: context
integer(c_intptr_t),target                          :: command_queue
integer(c_intptr_t),target                          :: cl_expt,cl_dict
character(len = 50000), target                      :: source
integer(kind=irg), parameter                        :: source_length = 50000
integer(kind=irg), target                           :: source_l
character(len=source_length, KIND=c_char),TARGET    :: csource
type(c_ptr), target                                 :: psource
integer(c_int32_t)                                  :: ierr2, pcnt
integer(c_intptr_t),target                          :: prog
integer(c_intptr_t),target                          :: kernel
integer(c_size_t)                                   :: cnum
character(9),target                                 :: kernelname
character(10, KIND=c_char),target                   :: ckernelname

integer(kind=irg)                                   :: num,ierr,irec,istat, jpar(7)
integer(kind=irg),parameter                         :: iunit = 40
integer(kind=irg),parameter                         :: iunitexpt = 41
integer(kind=irg),parameter                         :: iunitdict = 42
character(fnlen)                                    :: info ! info about the GPU
real(kind=dbl),parameter                            :: nAmpere = 6.241D+18   ! Coulomb per second


integer(kind=irg)                                   :: Ne,Nd,L,totnumexpt,numdictsingle,numexptsingle,imght,imgwd,nnk, &
                                                       recordsize, fratio, cratio, fratioE, cratioE, iii, itmpexpt, hdferr,&
                                                       recordsize_correct, patsz
integer(kind=8)                                     :: size_in_bytes_dict,size_in_bytes_expt
real(kind=sgl),pointer                              :: dict(:), T0dict(:)
real(kind=sgl),allocatable,TARGET                   :: dict1(:), dict2(:)
!integer(kind=1),allocatable                         :: imageexpt(:),imagedict(:)
real(kind=sgl),allocatable                          :: imageexpt(:),imagedict(:), mask(:,:),masklin(:), exptIQ(:), &
                                                       exptCI(:), exptFit(:), exppatarray(:), tmpexppatarray(:)
real(kind=sgl),allocatable                          :: imageexptflt(:),binned(:,:),imagedictflt(:),imagedictfltflip(:), &
                                                       tmpimageexpt(:)
real(kind=sgl),allocatable, target                  :: results(:),expt(:),dicttranspose(:),resultarray(:),&
                                                       eulerarray(:,:),resultmain(:,:),resulttmp(:,:)
integer(kind=irg),allocatable                       :: acc_array(:,:), ppend(:), ppendE(:) 
integer*4,allocatable                               :: iexptCI(:,:), iexptIQ(:,:)
real(kind=sgl),allocatable                          :: meandict(:),meanexpt(:),wf(:),mLPNH(:,:,:),mLPSH(:,:,:),accum_e_MC(:,:,:)
real(kind=sgl),allocatable                          :: mLPNH_simple(:,:), mLPSH_simple(:,:), eangle(:)
real(kind=sgl),allocatable                          :: EBSDpattern(:,:), FZarray(:,:), dpmap(:), lstore(:,:), pstore(:,:)
real(kind=sgl),allocatable                          :: EBSDpatternintd(:,:), lp(:), cp(:), EBSDpat(:,:)
integer(kind=irg),allocatable                       :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:)
real(kind=dbl),allocatable                          :: rdata(:,:), fdata(:,:), rrdata(:,:), ffdata(:,:), ksqarray(:,:)
complex(kind=dbl),allocatable                       :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable               :: inp(:,:), outp(:,:)
real(kind=dbl)                                      :: w, Jres
integer(kind=irg)                                   :: dims(2)
character(11)                                       :: dstr
character(15)                                       :: tstrb
character(15)                                       :: tstre
character(3)                                        :: vendor
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, fname, clname, ename, sourcefile
integer(hsize_t)                                    :: expwidth, expheight
integer(hsize_t),allocatable                        :: iPhase(:), iValid(:)
integer(c_size_t),target                            :: slength
integer(c_int)                                      :: numd, nump
type(C_PTR)                                         :: planf, HPplanf, HPplanb
integer(HSIZE_T)                                    :: dims3(3), offset3(3)

integer(kind=irg)                                   :: i,j,ii,jj,kk,ll,mm,pp,qq
integer(kind=irg)                                   :: FZcnt, pgnum, io_int(3), ncubochoric, pc
type(FZpointd),pointer                              :: FZlist, FZtmp
integer(kind=irg),allocatable                       :: indexlist(:),indexarray(:),indexmain(:,:),indextmp(:,:)
real(kind=sgl)                                      :: dmin,voltage,scl,ratio, mi, ma, ratioE, io_real(2), tstart, tmp, &
                                                       totnum_el, vlen, tstop
real(kind=dbl)                                      :: prefactor
character(fnlen)                                    :: xtalname
integer(kind=irg)                                   :: binx,biny,TID,nthreads,Emin,Emax
real(kind=sgl)                                      :: sx,dx,dxm,dy,dym,rhos,x,projweight, dp
real(kind=sgl)                                      :: dc(3),quat(4),ixy(2),bindx
integer(kind=irg)                                   :: nix,niy,nixp,niyp
real(kind=sgl)                                      :: euler(3)
integer(kind=irg)                                   :: indx
integer(kind=irg)                                   :: correctsize
logical                                             :: f_exists, init

integer(kind=irg)                                   :: ipar(10)

character(fnlen),ALLOCATABLE                        :: MessageLines(:)
integer(kind=irg)                                   :: NumLines
character(fnlen)                                    :: TitleMessage, exectime
character(100)                                      :: c
character(1000)                                     :: charline

type(HDFobjectStackType),pointer                    :: HDF_head

call timestamp(datestring=dstr, timestring=tstrb)

verbose = .FALSE.
init = .TRUE.
Ne = ebsdnl%numexptsingle
Nd = ebsdnl%numdictsingle
L = ebsdnl%numsx*ebsdnl%numsy/ebsdnl%binning**2
totnumexpt = ebsdnl%ipf_wd*ebsdnl%ipf_ht
imght = ebsdnl%numsx/ebsdnl%binning
imgwd = ebsdnl%numsy/ebsdnl%binning
nnk = ebsdnl%nnk
xtalname = ebsdnl%MCxtalname
ncubochoric = ebsdnl%ncubochoric
recordsize = L*4
itmpexpt = 43
dims = (/imght, imgwd/)
w = ebsdnl%hipassw
source_l = source_length

! these will need to be read from an experimental data file but we''l set
! defaults values here.
ebsdnl%WD = 10.0

! nullify the dict  and T0dict pointers
nullify(dict,T0dict)

! make sure that correctsize is a multiple of 16; if not, make it so
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if

! determine the experimental and dictionary sizes in bytes
size_in_bytes_dict = Nd*correctsize*sizeof(correctsize)
size_in_bytes_expt = Ne*correctsize*sizeof(correctsize)
recordsize_correct = correctsize*4
patsz              = correctsize

! get the total number of electrons on the detector
totnum_el = sum(acc%accum_e_detector)

!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
pgnum = GetPointGroup(ebsdnl%MCxtalname)

!=====================================================
! make sure the minimum energy is set smaller than the maximum
!=====================================================
if (ebsdnl%energymin.gt.ebsdnl%energymax) then
    call Message('Minimum energy is larger than maximum energy; please correct input file')
    stop
end if
!=====================================================
! get the indices of the minimum and maximum energy
!=====================================================
Emin = nint((ebsdnl%energymin - ebsdnl%Ehistmin)/ebsdnl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.ebsdnl%numEbins)  Emin=ebsdnl%numEbins

Emax = nint((ebsdnl%energymax - ebsdnl%Ehistmin)/ebsdnl%Ebinsize) + 1
if (Emax .lt. 1) Emax = 1
if (Emax .gt. ebsdnl%numEbins) Emax = ebsdnl%numEbins

!====================================
! init a bunch of parameters
!====================================
! binned pattern array
binx = ebsdnl%numsx/ebsdnl%binning
biny = ebsdnl%numsy/ebsdnl%binning
bindx = 1.0/float(ebsdnl%binning)**2

! intensity prefactor
prefactor = 0.25D0 * nAmpere * ebsdnl%beamcurrent * ebsdnl%dwelltime * 1.0D-15/ totnum_el

! for dictionary computations, the patterns are usually rather small, so perhaps the explicit
! energy sums can be replaced by an averaged approximate approach, in which all the energy bins
! are added together from the start, and all the master patterns are totaled as well...
! this is a straightforward sum; we should probably do a weighted sum instead

! this code will be removed in a later version [post 3.1]
if (ebsdnl%energyaverage .eq. 0) then
        allocate(mLPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ebsdnl%nE))
        allocate(mLPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ebsdnl%nE))
        allocate(accum_e_MC(ebsdnl%numEbins,ebsdnl%numsx,ebsdnl%numsy),stat=istat)
        accum_e_MC = acc%accum_e_detector
        mLPNH = master%mLPNH
        mLPSH = master%mLPSH
else if (ebsdnl%energyaverage .eq. 1) then
        allocate(mLPNH_simple(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy))
        allocate(mLPSH_simple(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy))
        allocate(wf(ebsdnl%numEbins))
        allocate(acc_array(ebsdnl%numsx,ebsdnl%numsy))
        acc_array = sum(acc%accum_e_detector,1)
        wf = sum(sum(acc%accum_e_detector,2),2)
        wf = wf/sum(wf)
        do ii=Emin,Emax
            master%mLPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ii) = &
            master%mLPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ii) * wf(ii)

            master%mLPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ii) = &
            master%mLPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ii) * wf(ii)

        end do

        mLPNH_simple = sum(master%mLPNH,3)
        mLPSH_simple = sum(master%mLPNH,3)

else
        stop 'Invalid value of energyaverage parameter'
end if


!=====================================================
! SAMPLING OF RODRIGUES FUNDAMENTAL ZONE
!=====================================================
! if eulerfile is not defined, then we use the standard RFZ sampling;
! if it is defined, then we read the Eulerangle triplets from the file
! and generate the FZlist here... this can be useful to index patterns that
! have only a small misorientation range with respect to a known orientation,
! so that it is not necessary to scan all of orientation space.

nullify(FZlist)
FZcnt = 0
if (trim(ebsdnl%eulerfile).eq.'undefined') then
  call Message('Orientation space sampling mode set to RFZ')
  io_int(1) = pgnum
  io_int(2) = ncubochoric
  call WriteValue('Point group number and number of cubochoric sampling points : ',io_int,2,"(I4,',',I5)")

  call sampleRFZ(ncubochoric, pgnum, 0, FZcnt, FZlist)
else
! read the euler angle file and create the linked list
  call getEulersfromFile(ebsdnl%eulerfile, FZcnt, FZlist) 
  call Message('Orientation space sampling mode set to MIS')
  io_int(1) = pgnum
  io_int(2) = FZcnt
  call WriteValue('Point group number and number of sampling points : ',io_int,2,"(I4,',',I5)")
end if

! allocate and fill FZarray for OpenMP parallelization
allocate(FZarray(4,FZcnt),stat=istat)
FZarray = 0.0

FZtmp => FZlist
do ii = 1,FZcnt
    FZarray(1:4,ii) = FZtmp%rod(1:4)
    FZtmp => FZtmp%next
end do
io_int(1) = FZcnt
call WriteValue(' Number of unique orientations sampled =        : ', io_int, 1, "(I8)")

!================================
! INITIALIZATION OF OpenCL DEVICE
!================================
call Message('--> Initializing OpenCL device')

call CLinit_PDCCQ(platform, nump, ebsdnl%platid, device, numd, ebsdnl%devid, info, context, command_queue)

! read the cl source file
sourcefile = 'DictIndx.cl'
call CLread_source_file(sourcefile, csource, slength)

! allocate device memory for experimental and dictionary patterns
cl_expt = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_expt, C_NULL_PTR, ierr)
call CLerror_check('MasterSubroutine:clCreateBuffer', ierr)

cl_dict = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dict, C_NULL_PTR, ierr)
call CLerror_check('MasterSubroutine:clCreateBuffer', ierr)

!================================
! the following lines were originally in the InnerProdGPU routine, but there is no need
! to execute them each time that routine is called so we move them here...
!================================
! create the program
pcnt = 1
psource = C_LOC(csource)
!prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(source_l), ierr)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('InnerProdGPU:clCreateProgramWithSource', ierr)


! build the program
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(ebsdnl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
! if(cnum > 1) call Message(trim(source(1:cnum))//'test',frm='(A)')
call CLerror_check('InnerProdGPU:clBuildProgram', ierr)
call CLerror_check('InnerProdGPU:clGetProgramBuildInfo', ierr2)

! finally get the kernel and release the program
kernelname = 'InnerProd'
ckernelname = kernelname
ckernelname(10:10) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('InnerProdGPU:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('InnerProdGPU:clReleaseProgram', ierr)

! the remainder is done in the InnerProdGPU routine
!=========================================

!=========================================
! ALLOCATION AND INITIALIZATION OF ARRAYS
!=========================================
call Message('--> Allocating various arrays for indexing')

allocate(expt(Ne*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for experimental patterns'
expt = 0.0

allocate(dict1(Nd*correctsize),dict2(Nd*correctsize),dicttranspose(Nd*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for dictionary patterns'
dict1 = 0.0
dict2 = 0.0
dict => dict1
dicttranspose = 0.0

allocate(results(Ne*Nd),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for results'
results = 0.0

allocate(mask(binx,biny),masklin(L),stat=istat)
if (istat .ne. 0) stop 'Could not allocate arrays for masks'
mask = 1.0
masklin = 0.0

allocate(imageexpt(L),imageexptflt(correctsize),imagedictflt(correctsize),imagedictfltflip(correctsize),stat=istat)
allocate(tmpimageexpt(correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for reading experimental image patterns'
imageexpt = 0.0
imageexptflt = 0.0

allocate(meandict(correctsize),meanexpt(correctsize),imagedict(correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for mean dictionary and experimental patterns'
meandict = 0.0
meanexpt = 0.0

! allocate(EBSDpattern(ebsdnl%numsx,ebsdnl%numsy),binned(binx,biny),stat=istat)
allocate(EBSDpattern(binx,biny),binned(binx,biny),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for EBSD pattern'
EBSDpattern = 0.0
binned = 0.0

allocate(resultarray(1:Nd),stat=istat)
if (istat .ne. 0) stop 'could not allocate result arrays'

resultarray = 0.0

allocate(indexarray(1:Nd),stat=istat)
if (istat .ne. 0) stop 'could not allocate index arrays'

indexarray = 0

allocate(indexlist(1:Nd*(ceiling(float(FZcnt)/float(Nd)))),stat=istat)
if (istat .ne. 0) stop 'could not allocate indexlist arrays'

indexlist = 0

do ii = 1,Nd*ceiling(float(FZcnt)/float(Nd))
    indexlist(ii) = ii
end do

allocate(resultmain(nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate main result array'

resultmain = -2.0

allocate(indexmain(nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate main index array'

indexmain = 0

allocate(resulttmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate temporary result array'

resulttmp = -2.0

allocate(indextmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate temporary index array'

indextmp = 0

allocate(eulerarray(1:3,Nd*ceiling(float(FZcnt)/float(Nd))),stat=istat)
if (istat .ne. 0) stop 'could not allocate euler array'

allocate(exptIQ(totnumexpt), exptCI(totnumexpt), exptFit(totnumexpt), stat=istat)
if (istat .ne. 0) stop 'could not allocate exptIQ array'

allocate(rdata(binx,biny),fdata(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'
rdata = 0.D0
fdata = 0.D0


!=====================================================
! determine loop variables to avoid having to duplicate 
! large sections of mostly identical code
!=====================================================
ratio = float(FZcnt)/float(Nd)
cratio = ceiling(ratio)
fratio = floor(ratio)

ratioE = float(totnumexpt)/float(Ne)
cratioE = ceiling(ratioE)
fratioE = floor(ratioE)

allocate(ppend(cratio),ppendE(cratioE))
ppend = (/ (Nd, i=1,cratio) /)
if (fratio.lt.cratio) then
  ppend(cratio) = MODULO(FZcnt,Nd)
end if

ppendE = (/ (Ne, i=1,cratioE) /)
if (fratioE.lt.cratioE) then
  ppendE(cratioE) = MODULO(totnumexpt,Ne)
end if

!=====================================================
! define the circular mask if necessary and convert to 1D vector
!=====================================================

if (trim(ebsdnl%maskfile).ne.'undefined') then
! read the mask from file; the mask can be defined by a 2D array of 0 and 1 values
! that is stored in row form as strings, e.g.    
!    0000001110000000
!    0000011111000000
! ... etc
!
    f_exists = .FALSE.
    fname = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%maskfile)
    fname = EMsoft_toNativePath(fname)
    inquire(file=trim(fname), exist=f_exists)
    if (f_exists.eqv..TRUE.) then
      mask = 0.0
      open(unit=dataunit,file=trim(fname),status='old',form='formatted')
      do jj=biny,1,-1
        read(dataunit,"(A)") charline
        do ii=1,binx
          if (charline(ii:ii).eq.'1') mask(ii,jj) = 1.0
        end do
      end do
      close(unit=dataunit,status='keep')
    else
      call FatalError('MasterSubroutine','maskfile '//trim(fname)//' does not exist')
    end if
else
    if (ebsdnl%maskpattern.eq.'y') then
      do ii = 1,biny
          do jj = 1,binx
              if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. ebsdnl%maskradius**2) then
                  mask(jj,ii) = 0.0
              end if
          end do
      end do
    end if
end if

! convert the mask to a linear (1D) array
do ii = 1,biny
    do jj = 1,binx
        masklin((ii-1)*binx+jj) = mask(jj,ii)
    end do
end do


!=====================================================
! Preprocess all the experimental patterns and store
! them in a temporary file as vectors; also, create 
! an average dot product map to be stored in the h5ebsd output file
!
! this could become a separate routine in the EMEBSDmod module ...
!=====================================================

! first, make sure that this file does not already exist
f_exists = .FALSE.
fname = trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname), exist=f_exists)

call WriteValue('Creating temporary file :',trim(fname))

if (f_exists) then
  open(unit=itmpexpt,file=trim(fname),&
      status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
  close(unit=itmpexpt,status='delete')
end if

! open the temporary file
open(unit=itmpexpt,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)

! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(ebsdnl%exptfile, ebsdnl%ipf_wd, ebsdnl%inputtype, recordsize, iunitexpt, ebsdnl%HDFstrings)
if (istat.ne.0) then
    call patternmod_errormessage(istat)
    call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
end if

! also, allocate the arrays used to create the average dot product map; this will require 
! reading the actual EBSD HDF5 file to figure out how many rows and columns there
! are in the region of interest.  For now we get those from the nml until we actually 
! implement the HDF5 reading bit
! this portion of code was first tested in IDL.
allocate(EBSDpatterninteger(binx,biny))
EBSDpatterninteger = 0
allocate(EBSDpatternad(binx,biny),EBSDpatternintd(binx,biny))
EBSDpatternad = 0.0
EBSDpatternintd = 0.0


! this next part is done with OpenMP, with only thread 0 doing the reading and writing,
! Thread 0 reads one line worth of patterns from the input file, then all threads do 
! the work, and thread 0 writes to the output file; repeat until all patterns have been processed.

call OMP_SET_NUM_THREADS(ebsdnl%nthreads)
io_int(1) = ebsdnl%nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

! allocate the arrays that holds the experimental patterns from a single row of the region of interest
allocate(exppatarray(patsz * ebsdnl%ipf_wd),stat=istat)
if (istat .ne. 0) stop 'could not allocate exppatarray'


! prepare the fftw plan for this pattern size to compute pattern quality (pattern sharpness Q)
allocate(EBSDPat(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate arrays for EBSDPat filter'
EBSDPat = 0.0
allocate(ksqarray(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate ksqarray array'
Jres = 0.0
call init_getEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf)
deallocate(EBSDPat)

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask array'
call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
deallocate(inp, outp)

call Message('Starting processing of experimental patterns')
call cpu_time(tstart)

dims3 = (/ binx, biny, ebsdnl%ipf_wd /)

! we do one row at a time
prepexperimentalloop: do iii = 1,ebsdnl%ipf_ht

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat) &
!$OMP& PRIVATE(imageexpt, tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, vlen, tmp, inp, outp)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()
! initialize thread private variables
    tmpimageexpt = 0.0
    allocate(EBSDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(EBSDpint(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate EBSDpint array'

    allocate(inp(binx,biny),outp(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate inp, outp arrays'

    rrdata = 0.D0
    ffdata = 0.D0

! thread 0 reads the next row of patterns from the input file
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*ebsdnl%ipf_wd /)
        call getExpPatternRow(iii, ebsdnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                              ebsdnl%inputtype, ebsdnl%HDFstrings, exppatarray)
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER
    jj=0

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=1,ebsdnl%ipf_wd
! convert imageexpt to 2D EBSD Pattern array
        do kk=1,biny
          EBSDPat(1:binx,kk) = exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx)
        end do

! compute the pattern Image Quality 
        exptIQ((iii-1)*ebsdnl%ipf_wd + jj) = sngl(computeEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf))

! Hi-Pass filter
        rrdata = dble(EBSDPat)
        ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), w, hpmask, inp, outp, HPplanf, HPplanb)
        EBSDPat = sngl(ffdata)

! adaptive histogram equalization
        ma = maxval(EBSDPat)
        mi = minval(EBSDPat)
    
        EBSDpint = nint(((EBSDPat - mi) / (ma-mi))*255.0)
        EBSDPat = float(adhisteq(ebsdnl%nregions,binx,biny,EBSDpint))

! convert back to 1D vector
        do kk=1,biny
          exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx) = EBSDPat(1:binx,kk)
        end do

! apply circular mask and normalize for the dot product computation
        exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) * masklin(1:L)
        vlen = NORM2(exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L))
        if (vlen.ne.0.0) then
          exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L)/vlen
        else
          exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = 0.0
        end if
    end do
!$OMP END DO

! thread 0 writes the row of patterns to the output file
    if (TID.eq.0) then
      do jj=1,ebsdnl%ipf_wd
        write(itmpexpt,rec=(iii-1)*ebsdnl%ipf_wd + jj) exppatarray((jj-1)*patsz+1:jj*patsz)
      end do
    end if

deallocate(EBSDPat, rrdata, ffdata, EBSDpint, inp, outp)
!$OMP BARRIER
!$OMP END PARALLEL

! print an update of progress
    if (mod(iii,5).eq.0) then
        io_int(1:2) = (/ iii, ebsdnl%ipf_ht /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
    end if
end do prepexperimentalloop

call Message(' -> experimental patterns stored in tmp file')

call closeExpPatternFile(ebsdnl%inputtype, iunitexpt)

close(unit=itmpexpt,status='keep')

! print some timing information
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(ebsdnl%nthreads) * float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,/)")

!=====================================================
call Message(' -> computing Average Dot Product map (ADP)')
call Message(' ')

allocate(dpmap(totnumexpt))
! re-open the temporary file
open(unit=itmpexpt,file=trim(fname),&
     status='old',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
! use the getADPmap routine in the filters module
call getADPmap(itmpexpt, totnumexpt, L, ebsdnl%ipf_wd, ebsdnl%ipf_ht, dpmap)

! output the ADP map as a tiff file (for debugging purposes only)
if (1.eq.0) then
    TIFF_nx = ebsdnl%ipf_wd
    TIFF_ny = ebsdnl%ipf_ht
    TIFF_filename = "ADPmap.tiff"

    ! allocate memory for image
    allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))

    ! fill the image with whatever data you have (between 0 and 255)
    ma = maxval(dpmap)
    mi = minval(dpmap)

    do j=0,TIFF_ny-1
     do i=0,TIFF_nx-1
      ii = j * TIFF_nx + i + 1
      TIFF_image(i,j) = int(255 * (dpmap(ii)-mi)/(ma-mi))
     end do
    end do

    ! create the file
    call TIFF_Write_File
    deallocate(TIFF_image)
end if

! we will leave the itmpexpt file open, since we'll be reading from it again...

!=====================================================
! MAIN COMPUTATIONAL LOOP
!
! Some explanation is necessary here... the bulk of this code is 
! executed in OpenMP multithreaded mode, with nthreads threads.
! Thread 0 has a special role described below; threads 1 ... nthreads-1
! share the computation of the dictionary patterns, and wait for 
! thread 0 to finish, if necessary.
!
! Thread 0 takes the dictionary patterns computed by the other threads
! in the previous step in the dictionaryloop and sends them to the GPU,
! along with as many chunks of experimental data are to be handled (experimentalloop
! inside the thread 0 portion of the code); the experimental patterns 
! are then read from the temporary file (unit itmpexpt).  Once all dot
! products have been computed by the GPU, thread 0 will rank them largest
! to smallest and keep only the top nnk values along with their indices 
! into the array of Euler angle triplets.  If the other threads are still
! computing dictionary patterns, thread 0 will join them; otherwise
! thread 0 will immediately take the next batch of dictionary patterns 
! and start all over.
!
! The trick is for the user to determine the array chunk sizes so that 
! threads 1 ... nthreads-1 do not have to wait for thread 0 to finish;
! this requires a bit of experimenting and observing the load on all the 
! system cores.  The load should always be approximately 100% x nthreads-1
! for an efficient execution.  The appropriate number of threads will depend
! on how powerful the GPU card is...
!=====================================================

call cpu_time(tstart)

call OMP_SET_NUM_THREADS(ebsdnl%nthreads)
io_int(1) = ebsdnl%nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")


! define the jpar array of integer parameters
jpar(1) = ebsdnl%binning
jpar(2) = ebsdnl%numsx
jpar(3) = ebsdnl%numsy
jpar(4) = ebsdnl%npx
jpar(5) = ebsdnl%npy
jpar(6) = ebsdnl%numEbins
jpar(7) = ebsdnl%nE

call timestamp()

dictionaryloop: do ii = 1,cratio+1
    results = 0.0

! if ii is odd, then we use dict1 for the dictionary computation, and dict2 for the GPU
! (assuming ii>1); when ii is even we switch the two pointers 
    if (mod(ii,2).eq.1) then
      dict => dict1
      dict1 = 0.0
      T0dict => dict2   ! these are the patterns to be sent to the GPU
      if (verbose.eqv..TRUE.) call WriteValue('','dict => dict1; T0dict => dict2')
    else
      dict => dict2
      dict2 = 0.0
      T0dict => dict1   ! these are the patterns to be sent to the GPU
      if (verbose.eqv..TRUE.) call WriteValue('','dict => dict2; T0dict => dict1')
    end if

    if (verbose.eqv..TRUE.) then
      io_int(1) = ii
      call WriteValue('Dictionaryloop index = ',io_int,1)
    end if

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,iii,jj,ll,mm,pp,ierr,io_int) &
!$OMP& PRIVATE(binned, ma, mi, EBSDpatternintd, EBSDpatterninteger, EBSDpatternad, quat, imagedictflt,imagedictfltflip)

        TID = OMP_GET_THREAD_NUM()

      if ((ii.eq.1).and.(TID.eq.0)) write(*,*) ' actual number of OpenMP threads  = ',OMP_GET_NUM_THREADS()
      if ((ii.eq.1).and.(TID.eq.0)) write(*,*) ' maximum number of OpenMP threads = ',OMP_GET_MAX_THREADS()


! the master thread should be the one working on the GPU computation
!$OMP MASTER
    if (ii.gt.1) then
      iii = ii-1        ! the index ii is already one ahead, since the GPU thread lags one cycle behind the others...
      if (verbose.eqv..TRUE.) then 
        if (associated(T0dict,dict1)) then 
          write(*,"('   GPU thread is working on dict1')")
        else
          write(*,"('   GPU thread is working on dict2')")
        end if
      end if

      dicttranspose = 0.0

      do ll = 1,correctsize
        do mm = 1,Nd
            dicttranspose((ll-1)*Nd+mm) = T0dict((mm-1)*correctsize+ll)
        end do
      end do

      ierr = clEnqueueWriteBuffer(command_queue, cl_dict, CL_TRUE, 0_8, size_in_bytes_dict, C_LOC(dicttranspose(1)), &
                                  0, C_NULL_PTR, C_NULL_PTR)
      call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

      experimentalloop: do jj = 1,cratioE

        expt = 0.0

        do pp = 1,ppendE(jj)   ! Ne or MODULO(totnumexpt,Ne)
          read(itmpexpt,rec=(jj-1)*Ne+pp) tmpimageexpt
          expt((pp-1)*correctsize+1:pp*correctsize) = tmpimageexpt
        end do

        ierr = clEnqueueWriteBuffer(command_queue, cl_expt, CL_TRUE, 0_8, size_in_bytes_expt, C_LOC(expt(1)), &
                                    0, C_NULL_PTR, C_NULL_PTR)
        call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

        call InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,ebsdnl%devid,kernel,context,command_queue)

! this might be simplified later for the remainder of the patterns
        do qq = 1,ppendE(jj)
            resultarray(1:Nd) = results((qq-1)*Nd+1:qq*Nd)
            indexarray(1:Nd) = indexlist((iii-1)*Nd+1:iii*Nd)

            call SSORT(resultarray,indexarray,Nd,-2)
            resulttmp(nnk+1:2*nnk,(jj-1)*Ne+qq) = resultarray(1:nnk)
            indextmp(nnk+1:2*nnk,(jj-1)*Ne+qq) = indexarray(1:nnk)

            call SSORT(resulttmp(:,(jj-1)*Ne+qq),indextmp(:,(jj-1)*Ne+qq),2*nnk,-2)

            resultmain(1:nnk,(jj-1)*Ne+qq) = resulttmp(1:nnk,(jj-1)*Ne+qq)
            indexmain(1:nnk,(jj-1)*Ne+qq) = indextmp(1:nnk,(jj-1)*Ne+qq)
        end do
      end do experimentalloop

      io_real(1) = maxval(results)
      io_real(2) = float(iii)/float(cratio)*100.0
      call WriteValue('',io_real,2,"(' max. dot product = ',F10.6,';',F6.1,'% complete')")


      if (mod(iii,10) .eq. 0) then
        io_int(1:2) = (/iii,cratio/)
        call WriteValue('',io_int,2,"(' -> Completed cycle ',I5,' out of ',I5)")
      end if
    else
       if (verbose.eqv..TRUE.) call WriteValue('','        GPU thread is idling')
    end if  ! ii.gt.1

!$OMP END MASTER


! here we carry out the dictionary pattern computation, unless we are in the ii=cratio+1 step
    if (ii.lt.cratio+1) then
     if (verbose.eqv..TRUE.) then
       if (associated(dict,dict1)) then 
         write(*,"('    Thread ',I2,' is working on dict1')") TID
       else
         write(*,"('    Thread ',I2,' is working on dict2')") TID
       end if
     end if

!$OMP DO SCHEDULE(DYNAMIC)

     do pp = 1,ppend(ii)  !Nd or MODULO(FZcnt,Nd)
       binned = 0.0
       quat = ro2qu(FZarray(1:4,(ii-1)*Nd+pp))

       if (ebsdnl%energyaverage .eq. 0) then
         call CalcEBSDPatternSingleFull(jpar,quat,accum_e_MC,mLPNH,mLPSH,master%rgx,&
                                        master%rgy,master%rgz,binned,Emin,Emax,mask,prefactor)
       else if (ebsdnl%energyaverage .eq. 1) then 
         call CalcEBSDPatternSingleApprox(jpar,quat,acc_array,mLPNH_simple,mLPSH_simple,master%rgx,&
                                                   master%rgy,master%rgz,binned,mask,prefactor)
       else
         stop 'Invalid value of energyaverage'
       end if

       if (ebsdnl%scalingmode .eq. 'gam') then
         binned = binned**ebsdnl%gammavalue
       end if

! hi pass filtering
!      rdata = dble(binned)
!      fdata = HiPassFilter(rdata,dims,w)
!      binned = sngl(fdata)


! adaptive histogram equalization
       ma = maxval(binned)
       mi = minval(binned)
       
       EBSDpatternintd = ((binned - mi)/ (ma-mi))
       EBSDpatterninteger = nint(EBSDpatternintd*255.0)
       EBSDpatternad =  adhisteq(ebsdnl%nregions,binx,biny,EBSDpatterninteger)
       binned = float(EBSDpatternad)

       imagedictflt = 0.0
       imagedictfltflip = 0.0
       do ll = 1,biny
         do mm = 1,binx
           imagedictflt((ll-1)*binx+mm) = binned(mm,ll)
         end do
       end do

! normalize and apply circular mask 
       imagedictflt(1:L) = imagedictflt(1:L) * masklin(1:L)
       vlen = NORM2(imagedictflt(1:correctsize))
       if (vlen.ne.0.0) then
         imagedictflt(1:correctsize) = imagedictflt(1:correctsize)/vlen
       else
         imagedictflt(1:correctsize) = 0.0
       end if
       
       dict((pp-1)*correctsize+1:pp*correctsize) = imagedictflt(1:correctsize)

       eulerarray(1:3,(ii-1)*Nd+pp) = 180.0/cPi*ro2eu(FZarray(1:4,(ii-1)*Nd+pp))
     end do
!$OMP END DO

     if (verbose.eqv..TRUE.) then
       io_int(1) = TID
       call WriteValue('',io_int,1,"('       Thread ',I2,' is done')")
     end if
   else
     if (verbose.eqv..TRUE.) then
       io_int(1) = TID
       call WriteValue('',io_int,1,"('       Thread ',I2,' idling')")
     end if
   end if

! and we end the parallel section here (all threads will synchronize).
!$OMP END PARALLEL

end do dictionaryloop

close(itmpexpt,status='delete')

! release the OpenCL kernel
ierr = clReleaseKernel(kernel)
call CLerror_check('InnerProdGPU:clReleaseKernel', ierr)

! perform some timing stuff
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(totnumexpt)*float(FZcnt) / tstop
call WriteValue('Number of pattern comparisons per second : ',io_real,1,"(/,F10.2)")
io_real(1) = float(totnumexpt) / tstop
call WriteValue('Number of experimental patterns indexed per second : ',io_real,1,"(/,F10.2,/)")

! ===================
! MAIN OUTPUT SECTION
! ===================

! fill the ipar array with integer parameters that are needed to write the h5ebsd file
! (anything other than what is already in the ebsdnl structure)
ipar = 0
ipar(1) = nnk
ipar(2) = Ne*ceiling(float(totnumexpt)/float(Ne))
ipar(3) = totnumexpt
ipar(4) = Nd*ceiling(float(FZcnt)/float(Nd))
ipar(5) = FZcnt
ipar(6) = pgnum

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

if (ebsdnl%datafile.ne.'undefined') then 
  vendor = 'TSL'
  call h5ebsd_writeFile(vendor, ebsdnl, dstr, tstrb, ipar, resultmain, exptIQ, indexmain, eulerarray, &
                        dpmap, progname, nmldeffile)
  call Message('Data stored in h5ebsd file : '//trim(ebsdnl%datafile))
end if

if (ebsdnl%ctffile.ne.'undefined') then 
  call ctfebsd_writeFile(ebsdnl,ipar,indexmain,eulerarray,resultmain)
  call Message('Data stored in ctf file : '//trim(ebsdnl%ctffile))
end if

if (ebsdnl%angfile.ne.'undefined') then 
  write (*,*) 'ang format not available until Release 3.2'
  !call angebsd_writeFile(ebsdnl,ipar,indexmain,eulerarray,resultmain)
  !call Message('Data stored in ang file : '//trim(ebsdnl%angfile))
end if

! close the fortran HDF5 interface
call h5close_EMsoft(hdferr)

! if requested, we notify the user that this program has completed its run
if (trim(EMsoft_getNotify()).ne.'Off') then
  if (trim(ebsdnl%Notify).eq.'On') then 
    NumLines = 3
    allocate(MessageLines(NumLines))

    call hostnm(c)
 
    MessageLines(1) = 'EMEBSDDI program has ended successfully'
    MessageLines(2) = 'Indexed data stored in '//trim(ebsdnl%datafile)
    write (exectime,"(F15.0)") tstop  
    MessageLines(3) = 'Total execution time [s]: '//trim(exectime)
    TitleMessage = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, TitleMessage)
  end if
end if


end subroutine MasterSubroutine
