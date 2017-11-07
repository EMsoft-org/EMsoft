! ###################################################################
! Copyright (c) 2015-2017, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMTKDDI.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMTKDDI
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief Indexing of TKD patterns using the dictionary approach. 
!
!> @date 05/07/17 MDG 1.0 original, forked from EMTKDDI program
!--------------------------------------------------------------------------

program EMTKDDI

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use EBSDmod
use TKDmod
use EBSDDImod
use TKDDImod

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(TKDIndexingNameListType)               :: tkdnl
type(TKDLargeAccumDIType),pointer           :: acc
type(TKDMasterDIType),pointer               :: master
logical                                     :: verbose
integer(kind=irg)                           :: istat, res

interface
        subroutine MasterSubroutine(tkdnl,acc,master,progname,nmldeffile)

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
        use TKDmod
        use TKDDImod
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

        type(TKDIndexingNameListType),INTENT(INOUT)         :: tkdnl
        type(TKDLargeAccumDIType),pointer,INTENT(IN)        :: acc
        type(TKDMasterDIType),pointer,INTENT(IN)            :: master
        character(fnlen),INTENT(IN)                         :: progname
        character(fnlen),INTENT(IN)                         :: nmldeffile

        end subroutine MasterSubroutine
end interface

nmldeffile = 'EMTKDDI.nml'
progname = 'EMTKDDI.f90'
progdesc = 'Program to index TKD patterns using a dynamically calculated pattern dictionary'
verbose = .TRUE.

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 74 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMTKDIndexing','JSON input not yet implemented')
!  call JSONreadTKDIndexingNameList(tkdnl, nmldeffile, error_cnt)
else
  call GetTKDIndexingNameList(nmldeffile,tkdnl)
end if

! 1. read the Monte Carlo data file
allocate(acc)
call TKDIndexingreadMCfile(tkdnl, acc)

! 2. read EBSD master pattern file
allocate(master)
call TKDIndexingreadMasterfile(tkdnl, master)

! 3. generate detector arrays
allocate(master%rgx(tkdnl%numsx,tkdnl%numsy), master%rgy(tkdnl%numsx,tkdnl%numsy), &
         master%rgz(tkdnl%numsx,tkdnl%numsy), stat=istat)
allocate(acc%accum_e_detector(tkdnl%numEbins,tkdnl%numsx,tkdnl%numsy), stat=istat)

call TKDIndexingGenerateDetector(tkdnl, acc, master)
deallocate(acc%accum_e)
! perform the dictionary indexing computations
call MasterSubroutine(tkdnl,acc,master,progname, nmldeffile)

end program EMTKDDI

!--------------------------------------------------------------------------
!
! SUBROUTINE:MasterSubroutine
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Master subroutine to control dictionary generation, inner products computations, sorting values
!> and indexing of points, all in parallel using OpenCL/openMP
!
!> @param tkdnl ped indexing namelist pointer
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
!> @date 05/07/17 MDG 2.0 forked routine from original EBSD program; modified for TKD indexing
!--------------------------------------------------------------------------

subroutine MasterSubroutine(tkdnl,acc,master,progname, nmldeffile)

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
use TKDmod
use TKDDImod
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

IMPLICIT NONE

type(TKDIndexingNameListType),INTENT(INOUT)         :: tkdnl
type(TKDLargeAccumDIType),pointer,INTENT(IN)        :: acc
type(TKDMasterDIType),pointer,Intent(IN)            :: master
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

integer(kind=irg)                                   :: num,ierr,irec,istat, jpar(7)
integer(kind=irg),parameter                         :: iunit = 40
integer(kind=irg),parameter                         :: iunitexpt = 41
integer(kind=irg),parameter                         :: iunitdict = 42
character(fnlen)                                    :: info ! info about the GPU
integer, parameter                                  :: source_length = 50000
real(kind=dbl),parameter                            :: nAmpere = 6.241D+18   ! Coulomb per second

character(len = 50000)                              :: source

integer(kind=irg)                                   :: Ne,Nd,L,totnumexpt,numdictsingle,numexptsingle,imght,imgwd,nnk, &
                                                       recordsize, fratio, cratio, fratioE, cratioE, iii, itmpexpt, hdferr,&
                                                       recordsize_correct
integer(kind=8)                                     :: size_in_bytes_dict,size_in_bytes_expt
real(kind=sgl),pointer                              :: dict(:), T0dict(:)
real(kind=sgl),allocatable,TARGET                   :: dict1(:), dict2(:)
!integer(kind=1),allocatable                         :: imageexpt(:),imagedict(:)
real(kind=sgl),allocatable                          :: imageexpt(:),imagedict(:), mask(:,:),masklin(:), exptIQ(:), &
                                                       exptCI(:), exptFit(:)
real(kind=sgl),allocatable                          :: imageexptflt(:),binned(:,:),imagedictflt(:),imagedictfltflip(:), &
                                                       tmpimageexpt(:)
real(kind=sgl),allocatable, target                  :: results(:),expt(:),dicttranspose(:),resultarray(:),&
                                                       eulerarray(:,:),resultmain(:,:),resulttmp(:,:)
integer(kind=irg),allocatable                       :: acc_array(:,:), ppend(:), ppendE(:) 
integer*4,allocatable                               :: idpmap(:),iexptCI(:,:), iexptIQ(:,:)
real(kind=sgl),allocatable                          :: meandict(:),meanexpt(:),wf(:),mLPNH(:,:,:),mLPSH(:,:,:),accum_e_MC(:,:,:)
real(kind=sgl),allocatable                          :: mLPNH_simple(:,:), mLPSH_simple(:,:), eangle(:)
real(kind=sgl),allocatable                          :: TKDpattern(:,:), FZarray(:,:), dpmap(:), lstore(:,:), pstore(:,:)
real(kind=sgl),allocatable                          :: TKDpatternintd(:,:), lp(:), cp(:)
integer(kind=irg),allocatable                       :: TKDpatterninteger(:,:), TKDpatternad(:,:)
real(kind=dbl),allocatable                          :: rdata(:,:), fdata(:,:)
real(kind=dbl)                                      :: w
integer(kind=irg)                                   :: dims(2)
character(11)                                       :: dstr
character(15)                                       :: tstrb
character(15)                                       :: tstre
character(3)                                        :: vendor
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, fname, clname, ename, sourcefile
integer(hsize_t)                                    :: expwidth, expheight
integer(hsize_t),allocatable                        :: iPhase(:), iValid(:)
character(len=source_length, KIND=c_char),TARGET    :: csource
integer(c_size_t),target                            :: slength
integer(c_int)                                      :: numd, nump

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


type(HDFobjectStackType),pointer                    :: HDF_head

call timestamp(datestring=dstr, timestring=tstrb)

verbose = .FALSE.
init = .TRUE.
Ne = tkdnl%numexptsingle
Nd = tkdnl%numdictsingle
L = tkdnl%numsx*tkdnl%numsy/tkdnl%binning**2
totnumexpt = tkdnl%ipf_wd*tkdnl%ipf_ht
imght = tkdnl%numsx/tkdnl%binning
imgwd =  tkdnl%numsy/tkdnl%binning
nnk = tkdnl%nnk
xtalname = tkdnl%MCxtalname
ncubochoric = tkdnl%ncubochoric
recordsize = L*4
itmpexpt = 43
dims = (/imght, imgwd/)
w = tkdnl%hipassw


! these will need to be read from an experimental data file but we''l set
! defaults values here.
tkdnl%WD = 10.0

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


! get the total number of electrons on the detector
totnum_el = sum(acc%accum_e_detector)

!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
pgnum = GetPointGroup(tkdnl%MCxtalname)

!=====================================================
! make sure the minimum energy is set smaller than the maximum
!=====================================================
if (tkdnl%energymin.gt.tkdnl%energymax) then
    call Message('Minimum energy is larger than maximum energy; please correct input file')
    stop
end if
!=====================================================
! get the indices of the minimum and maximum energy
!=====================================================
Emin = nint((tkdnl%energymin - tkdnl%Ehistmin)/tkdnl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.tkdnl%numEbins)  Emin=tkdnl%numEbins

Emax = nint((tkdnl%energymax - tkdnl%Ehistmin)/tkdnl%Ebinsize) + 1
if (Emax .lt. 1) Emax = 1
if (Emax .gt. tkdnl%numEbins) Emax = tkdnl%numEbins

!====================================
! init a bunch of parameters
!====================================
! binned pattern array
binx = tkdnl%numsx/tkdnl%binning
biny = tkdnl%numsy/tkdnl%binning
bindx = 1.0/float(tkdnl%binning)**2

! intensity prefactor
prefactor = 0.25D0 * nAmpere * tkdnl%beamcurrent * tkdnl%dwelltime * 1.0D-15/ totnum_el

! for dictionary computations, the patterns are usually rather small, so perhaps the explicit
! energy sums can be replaced by an averaged approximate approach, in which all the energy bins
! are added together from the start, and all the master patterns are totaled as well...
! this is a straightforward sum; we should probably do a weighted sum instead

! this code will be removed in a later version [post 3.1]
if (tkdnl%energyaverage .eq. 0) then
        allocate(mLPNH(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy,tkdnl%nE))
        allocate(mLPSH(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy,tkdnl%nE))
        allocate(accum_e_MC(tkdnl%numEbins,tkdnl%numsx,tkdnl%numsy),stat=istat)
        accum_e_MC = acc%accum_e_detector
        mLPNH = master%mLPNH
        mLPSH = master%mLPSH
else if (tkdnl%energyaverage .eq. 1) then
        allocate(mLPNH_simple(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy))
        allocate(mLPSH_simple(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy))
        allocate(wf(tkdnl%numEbins))
        allocate(acc_array(tkdnl%numsx,tkdnl%numsy))
        acc_array = sum(acc%accum_e_detector,1)
        wf = sum(sum(acc%accum_e_detector,2),2)
        wf = wf/sum(wf)
        do ii=Emin,Emax
            master%mLPNH(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy,ii) = &
            master%mLPNH(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy,ii) * wf(ii)

            master%mLPSH(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy,ii) = &
            master%mLPSH(-tkdnl%npx:tkdnl%npx,-tkdnl%npy:tkdnl%npy,ii) * wf(ii)

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
if (trim(tkdnl%eulerfile).eq.'undefined') then
  call Message('Orientation space sampling mode set to RFZ')
  io_int(1) = pgnum
  io_int(2) = ncubochoric
  call WriteValue('Point group number and number of cubochoric sampling points : ',io_int,2,"(I4,',',I5)")

  call sampleRFZ(ncubochoric, pgnum, 0, FZcnt, FZlist)
else
! read the euler angle file and create the linked list
  call getEulersfromFile(tkdnl%eulerfile, FZcnt, FZlist) 
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
call CLinit_PDCCQ(platform, nump, tkdnl%platid, device, numd, tkdnl%devid, info, context, command_queue)

! read the cl source file
sourcefile = 'DictIndx.cl'
call CLread_source_file(sourcefile, csource, slength)

! allocate device memory for experimental and dictionary patterns
cl_expt = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_expt, C_NULL_PTR, ierr)
call CLerror_check('MasterSubroutine:clCreateBuffer', ierr)

cl_dict = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dict, C_NULL_PTR, ierr)
call CLerror_check('MasterSubroutine:clCreateBuffer', ierr)

! the remainder is done in the InnerProdGPU routine

!=========================================
! ALLOCATION AND INITIALIZATION OF ARRAYS
!=========================================

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

! allocate(TKDpattern(tkdnl%numsx,tkdnl%numsy),binned(binx,biny),stat=istat)
allocate(TKDpattern(binx,biny),binned(binx,biny),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for EBSD pattern'
TKDpattern = 0.0
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
if (tkdnl%maskpattern.eq.'y') then
  do ii = 1,biny
      do jj = 1,binx
          if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. tkdnl%maskradius**2) then
              mask(jj,ii) = 0.0
          end if
      end do
  end do
end if
  
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
! this could become a separate routine in the EMTKDmod module ...
!=====================================================

! first, make sure that this file does not already exist
f_exists = .FALSE.
fname = trim(EMsoft_getEMtmppathname())//trim(tkdnl%tmpfile)
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

ename = trim(EMsoft_getEMdatapathname())//trim(tkdnl%exptfile)
ename = EMsoft_toNativePath(ename)
open(unit=iunitexpt,file=trim(ename),&
    status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
call Message(' opened input file '//trim(ename))

! prepare the fftw plan for this pattern size
TKDpattern = 0.0
tmp = sngl(getEBSDIQ(binx, biny, TKDpattern, init))

! also, allocate the arrays used to create the dot product map; this will require 
! reading the actual EBSD HDF5 file to figure out how many rows and columns there
! are in the region of interest.  For now we get those from the nml until we actually 
! implement the HDF5 reading bit
! this portion of code was first tested in IDL.
allocate(lstore(L,tkdnl%ipf_wd),pstore(L,tkdnl%ipf_wd),dpmap(totnumexpt), &
         idpmap(totnumexpt),lp(L),cp(L))

allocate(TKDpatterninteger(binx,biny))
TKDpatterninteger = 0
allocate(TKDpatternad(binx,biny),TKDpatternintd(binx,biny))
TKDpatternad = 0.0
TKDpatternintd = 0.0
dpmap= 0.0
pstore = 0.0
lstore = 0.0
lp = 0.0
cp = 0.0

! initialize the HiPassFilter routine
rdata = dble(TKDpattern)
fdata = HiPassFilter(rdata,dims,w,init=.TRUE.)

!open(unit=45,file='hipassfilter.data',status='unknown',form='unformatted')
!write(45) fdata
!close(unit=45,status='keep')

call Message('Starting processing of experimental patterns')
call cpu_time(tstart)

pc = totnumexpt/10

prepexperimentalloop: do iii = 1,totnumexpt
    tmpimageexpt = 0.0
    read(iunitexpt,rec=iii) imageexpt

! convert imageexpt to 2D EBS Pattern array
    do kk=1,biny
      TKDpattern(1:binx,kk) = imageexpt((kk-1)*binx+1:kk*binx)
    end do

! compute the pattern Image Quality 
    exptIQ(iii) = sngl(getEBSDIQ(binx, biny, TKDpattern))

! Hi-Pass filter
    rdata = dble(TKDpattern)
    fdata = HiPassFilter(rdata,dims,w)
    TKDpattern = sngl(fdata)

! adaptive histogram equalization
    ma = maxval(TKDpattern)
    mi = minval(TKDpattern)
    
    TKDpatternintd = ((TKDpattern - mi)/ (ma-mi))
    TKDpatterninteger = nint(TKDpatternintd*255.0)
    TKDpatternad =  adhisteq(tkdnl%nregions,binx,biny,TKDpatterninteger)
    TKDpattern = float(TKDpatternad)

! convert back to 1D vector
    do kk=1,biny
      imageexpt((kk-1)*binx+1:kk*binx) = TKDpattern(1:binx,kk)
    end do

! normalize and apply circular mask
    imageexpt(1:L) = imageexpt(1:L) * masklin(1:L)
    vlen = NORM2(imageexpt(1:L))
    if (vlen.ne.0.0) then
      imageexpt(1:L) = imageexpt(1:L)/vlen
    else
      imageexpt(1:L) = 0.0
    end if
    tmpimageexpt(1:L) = imageexpt(1:L)

! and write this pattern into the temporary file
    write(itmpexpt,rec=iii) tmpimageexpt

! finally, handle the average dot product map stuff
    ii = mod(iii,tkdnl%ipf_wd)
    if (ii.eq.0) ii = tkdnl%ipf_wd
    jj = iii/tkdnl%ipf_wd+1
! do we need to copy pstore into lstore ?
    if ((ii.eq.1).and.(jj.gt.1)) lstore = pstore
! determine to which dpmap entries we need to add the dot product
    if (ii.eq.1) then
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
    else
      lp = cp
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
      dp = sum(lp(1:L)*cp(1:L))
      dpmap(iii-1) = dpmap(iii-1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
    if (jj.gt.1) then
      dp = sum(lstore(1:L,ii)*cp(1:L))
      dpmap(iii-tkdnl%ipf_wd+1) = dpmap(iii-tkdnl%ipf_wd+1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if

! print an update of progress
    if (iii.gt.pc) then
      io_int(1:2) = (/ iii, totnumexpt /)
      call WriteValue('Completed ',io_int,2,"(I12,' of ',I12,' experimental patterns')")
      pc = pc + totnumexpt/10
    end if
end do prepexperimentalloop

call Message(' -> experimental patterns stored in tmp file')

close(unit=iunitexpt,status='keep')

! print some timing information

call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,//)")

! we keep the temporary file open since we will be reading from it...

! correct the dot product map values depending on inside, edge, or corner pixels
! divide by 4
dpmap = dpmap*0.25

! correct the straight segments
dpmap(2:tkdnl%ipf_wd-1) = dpmap(2:tkdnl%ipf_wd-1) * 4.0/3.0
dpmap(totnumexpt-tkdnl%ipf_wd+2:totnumexpt-1) = dpmap(totnumexpt-tkdnl%ipf_wd+2:totnumexpt-1) * 4.0/3.0
do jj=1,tkdnl%ipf_ht-2
  dpmap(tkdnl%ipf_wd*jj+1) = dpmap(tkdnl%ipf_wd*jj+1) * 4.0/3.0
end do
do jj=2,tkdnl%ipf_ht-1
  dpmap(tkdnl%ipf_wd*jj) = dpmap(tkdnl%ipf_wd*jj) * 4.0/3.0
end do

! and the corners
dpmap(1) = dpmap(1) * 4.0
dpmap(tkdnl%ipf_wd) = dpmap(tkdnl%ipf_wd) * 2.0
dpmap(totnumexpt) = dpmap(totnumexpt) * 2.0
dpmap(totnumexpt-tkdnl%ipf_wd+1) = dpmap(totnumexpt-tkdnl%ipf_wd+1) * 4.0/3.0

! and we deallocate the auxiliary variables for the average dot product map
deallocate(lstore,pstore,lp,cp)

!=====================================================
! MAIN COMPUTATIONAL LOOP
!
! Some explanation is necessary here... the bulk of this code is 
! executed in OpenMP multithreaded mode, with nthreads threads.
! Thread 0 has a special role describe below; threads 1 ... nthreads-1
! share the computation of the dictionary patterns, and wait for 
! thread 0 to finish, if necessary.
!
! Thread 0 takes the dictionary patterns computed by the other threads
! in the previous step in the dictionaryloop and sends them to the GPU,
! along with as many chunks of experimental data are to be handled (experimentalloop
! inside the thread 0 portion of the code); the ! experimental patterns 
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


call OMP_SET_NUM_THREADS(tkdnl%nthreads)
io_int(1) = tkdnl%nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")


! define the ipar array
jpar(1) = tkdnl%binning
jpar(2) = tkdnl%numsx
jpar(3) = tkdnl%numsy
jpar(4) = tkdnl%npx
jpar(5) = tkdnl%npy
jpar(6) = tkdnl%numEbins
jpar(7) = tkdnl%nE

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
!$OMP& PRIVATE(binned, ma, mi, TKDpatternintd, TKDpatterninteger, TKDpatternad, quat, imagedictflt,imagedictfltflip)

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

        call InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,tkdnl%devid,csource,slength,platform, &
                          device,context,command_queue)

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

       if (tkdnl%energyaverage .eq. 0) then
         call CalcEBSDpatternSingleFull(jpar,quat,accum_e_MC,mLPNH,mLPSH,master%rgx,&
                                        master%rgy,master%rgz,binned,Emin,Emax,mask,prefactor)
       else if (tkdnl%energyaverage .eq. 1) then 
         call CalcEBSDpatternSingleApprox(jpar,quat,acc_array,mLPNH_simple,mLPSH_simple,master%rgx,&
                                                   master%rgy,master%rgz,binned,mask,prefactor)
       else
         stop 'Invalid value of energyaverage'
       end if

       if (tkdnl%scalingmode .eq. 'gam') then
         binned = binned**tkdnl%gammavalue
       end if

! hi pass filtering
!      rdata = dble(binned)
!      fdata = HiPassFilter(rdata,dims,w)
!      binned = sngl(fdata)


! adaptive histogram equalization
       ma = maxval(binned)
       mi = minval(binned)
       
       TKDpatternintd = ((binned - mi)/ (ma-mi))
       TKDpatterninteger = nint(TKDpatternintd*255.0)
       TKDpatternad =  adhisteq(tkdnl%nregions,binx,biny,TKDpatterninteger)
       binned = float(TKDpatternad)

       imagedictflt = 0.0
       imagedictfltflip = 0.0
       do ll = 1,biny
         do mm = 1,binx
           imagedictflt((ll-1)*binx+mm) = binned(mm,ll)
         end do
       end do

! normalize and apply circular mask 
       imagedictflt = imagedictflt * masklin
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
! (anything other than what is already in the tkdnl structure)
ipar = 0
ipar(1) = nnk
ipar(2) = Ne*ceiling(float(totnumexpt)/float(Ne))
ipar(3) = totnumexpt
ipar(4) = Nd*ceiling(float(FZcnt)/float(Nd))
ipar(5) = FZcnt
ipar(6) = pgnum

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

if (tkdnl%datafile.ne.'undefined') then 
  vendor = 'TSL'
  call h5tkd_writeFile(vendor, tkdnl, dstr, tstrb, ipar, resultmain, exptIQ, indexmain, eulerarray, &
                        dpmap, progname, nmldeffile)
  call Message('Data stored in h5tkd file : '//trim(tkdnl%datafile))
end if

if (tkdnl%ctffile.ne.'undefined') then 
  call ctftkd_writeFile(tkdnl,ipar,indexmain,eulerarray,resultmain)
  call Message('Data stored in ctf file : '//trim(tkdnl%ctffile))
end if

if (tkdnl%angfile.ne.'undefined') then 
  write (*,*) 'ang format not available until Release 3.2'
  !call angebsd_writeFile(tkdnl,ipar,indexmain,eulerarray,resultmain)
  !call Message('Data stored in ang file : '//trim(tkdnl%angfile))
end if

! close the fortran HDF5 interface
call h5close_EMsoft(hdferr)

end subroutine MasterSubroutine
