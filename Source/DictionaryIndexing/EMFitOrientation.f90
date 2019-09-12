! ###################################################################
! Copyright (c) 2015-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMFitOrientationPS.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMFitOrientationPS
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief Refine the orientation in a dot product file by searching orientation
!> space around the best indexed point and optionally include pseudosymmetric variants
!
!> @date 08/01/16  SS 1.0 original
!> @date 03/12/18 MDG 1.1 replaced HDF5 dot product file reading by subroutine call
!> @date 04/01/18 MDG 2.0 merged various versions of the orientation fit/refine into a single program
!> @date 11/19/18 MDG 2.1 correction of bug caused by incorrectly initialized CIlist array
!--------------------------------------------------------------------------
program EMFitOrientationPS

use local
use typedefs 
use crystal
use NameListTypedefs
use NameListHandlers
use initializersHDF
use patternmod
use HDF5
use h5im
use h5lt
use EMh5ebsd
use HDFsupport
use rotations
use so3
use constants
use detectors
use EBSDmod
use EBSDDImod
use omp_lib
use ECPmod, only: GetPointGroup
use filters
use timing
use error
use io
use EBSDiomod
use files
use dictmod
use bobyqa_refinement,only:bobyqa
use FitOrientations,only:EMFitOrientationcalfunEBSD
use stringconstants
use commonmod

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(RefineOrientationtype)             :: ronl
type(EBSDIndexingNameListType)          :: dinl
type(MCCLNameListType)                  :: mcnl
type(EBSDMasterNameListType)            :: mpnl
type(EBSDNameListType)                  :: ebsdnl
type(EBSDMCdataType)                    :: EBSDMCdata
type(EBSDMPdataType)                    :: EBSDMPdata
type(EBSDDetectorType)                  :: EBSDdetector
type(EBSDDIdataType)                    :: EBSDDIdata

logical                                 :: stat, readonly, noindex, ROIselected
character(fnlen)                        :: dpfile, masterfile, energyfile
integer(kind=irg)                       :: hdferr, ii, jj, kk, iii, istat, npy, jjj

real(kind=dbl)                          :: misang       ! desired misorientation angle (degrees)
integer(kind=irg)                       :: Nmis         ! desired number of sampling points along cube edge
integer(kind=irg)                       :: CMcnt        ! number of entries in linked list
type(FZpointd),pointer                  :: CMlist, CMtmp       ! pointer to start of linked list and temporary one
real(kind=dbl)                          :: rhozero(4), hipassw

real(kind=sgl),allocatable              :: euPS(:,:), euler_bestmatch(:,:,:), CIlist(:), CMarray(:,:,:)
integer(kind=irg),allocatable           :: indexmain(:,:) 
real(kind=sgl),allocatable              :: resultmain(:,:)                                         
integer(HSIZE_T)                        :: dims(1),dims2D(2),dims3(3),offset3(3) 

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                        :: dataset, groupname  
character(fnlen)                        :: ename, fname    
character(2)                            :: anglemode
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second

integer(c_size_t),allocatable           :: IPAR2(:)
real(kind=dbl),allocatable              :: X(:), XL(:), XU(:)
real(kind=sgl),allocatable              :: INITMEANVAL(:)
real(kind=dbl)                          :: RHOBEG, RHOEND
integer(kind=irg)                       :: NPT, N, IPRINT, NSTEP, NINIT
integer(kind=irg),parameter             :: MAXFUN = 10000
logical                                 :: verbose

logical                                 :: f_exists, init, g_exists, overwrite
integer(kind=irg),parameter             :: iunitexpt = 41, itmpexpt = 42
integer(kind=irg)                       :: binx, biny, recordsize, pos(2)
real(kind=sgl),allocatable              :: tmpimageexpt(:), EBSDPattern(:,:), imageexpt(:), mask(:,:), masklin(:)
real(kind=sgl),allocatable              :: imagedictflt(:), exppatarray(:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:), binned(:,:), euler_best(:,:)
real(kind=sgl),allocatable              :: epatterns(:,:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:), Rvar(:)
type(C_PTR)                             :: planf, HPplanf, HPplanb
integer(kind=8)                         :: size_in_bytes_dict,size_in_bytes_expt

real(kind=dbl)                          :: w, Jres, nel, emult 
real(kind=dbl)                          :: stpsz, cu0(3)
real(kind=dbl),allocatable              :: cubneighbor(:,:)
real(kind=sgl)                          :: quat(4), quat2(4), ma, mi, dp, tstart, tstop, io_real(4), tmp, &
                                           vlen, avec(3), dtor
real(kind=dbl)                          :: qu(4), rod(4) 
integer(kind=irg)                       :: ipar(10), Emin, Emax, nthreads, TID, io_int(2), tickstart, ierr, L, nvar, niter
integer(kind=irg)                       :: ll, mm, jpar(7), Nexp, pgnum, FZcnt, nlines, dims2(2), correctsize, totnumexpt

real(kind=dbl)                          :: prefactor, F, angleaxis(4)
real(kind=sgl),allocatable              :: quPS(:,:), axPS(:,:), dpPS(:,:), eulerPS(:,:,:)
real(kind=dbl)                          :: ratioE, eurfz(3), euinp(3)
integer(kind=irg)                       :: cratioE, fratioE, eindex, jjend, iiistart, iiiend, Nd, Ne, patsz, pp
integer(kind=irg),allocatable           :: ppendE(:)
real(kind=sgl),allocatable              :: exptpatterns(:,:)

real(kind=sgl),allocatable              :: STEPSIZE(:), OSMmap(:,:), IQmap(:)
type(dicttype),pointer                  :: dict
integer(kind=irg)                       :: FZtype, FZorder
character(fnlen)                        :: modalityname

type(HDFobjectStackType)                :: HDF_head
type(unitcell)                          :: cell

dtor = sngl(cPi) / 180.0

init = .TRUE.
overwrite = .TRUE.
verbose = .FALSE.
!nullify(cell)        

nmldeffile = 'EMFitOrientationPS.nml'
progname = 'EMFitOrientationPS.f90'
progdesc = 'Refine orientations by searching orientation space about a point including the pseudosymmetric variant(s)'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 91 /), progname)

! deal with the namelist stuff
call GetRefineOrientationNameList(nmldeffile,ronl)

modalityname = trim(ronl%modality)

!====================================
! read the relevant fields from the dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

!====================================
! read the relevant fields from the dot product HDF5 file
!====================================
if (trim(modalityname) .eq. 'EBSD') then
  if ( (ronl%matchdepth.eq.1).or.(trim(ronl%method).eq.'SUB') ) then 
    call readEBSDDotProductFile(ronl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                                getCI=.TRUE., &
                                getIQ=.TRUE., & 
                                getOSM=.TRUE., & 
                                getPhi1=.TRUE., &
                                getPhi=.TRUE., &
                                getPhi2=.TRUE.) 

    w = dinl%hipassw
    Nexp = EBSDDIdata%Nexp
    allocate(euler_bestmatch(3,1,Nexp),stat=istat)
    allocate(euler_best(3,Nexp),CIlist(Nexp),stat=istat)
    if (istat .ne. 0) then
        dpfile = 'Failed to allocate euler_bestmatch array'
        call FatalError('EMAverageOrient',dpfile)
    end if 
    euler_bestmatch = 0.0
    euler_best = 0.0
    CIlist = 0.0
    euler_bestmatch(1,1,1:Nexp) = EBSDDIdata%Phi1(1:Nexp)
    euler_bestmatch(2,1,1:Nexp) = EBSDDIdata%Phi(1:Nexp)
    euler_bestmatch(3,1,1:Nexp) = EBSDDIdata%Phi2(1:Nexp)
    deallocate(EBSDDIdata%Phi1,EBSDDIdata%Phi,EBSDDIdata%Phi2)
  else
    call readEBSDDotProductFile(ronl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                                getCI=.TRUE., &
                                getIQ=.TRUE., & 
                                getOSM=.TRUE., & 
                                getEulerAngles=.TRUE., &
                                getTopMatchIndices=.TRUE.) 

    w = dinl%hipassw
    Nexp = EBSDDIdata%Nexp
    allocate(euler_bestmatch(3,ronl%matchdepth,Nexp),stat=istat)
    allocate(euler_best(3,Nexp),CIlist(Nexp),stat=istat)
    if (istat .ne. 0) then
        dpfile = 'Failed to allocate euler_bestmatch array'
        call FatalError('EMAverageOrient',dpfile)
    end if 
    euler_bestmatch = 0.0
    euler_best = 0.0
    CIlist = 0.0
! read the appropriate set of Euler angles from the array of near matches 
    do ii=1,ronl%matchdepth
      do jj=1,Nexp
        euler_bestmatch(1:3,ii,jj) = EBSDDIdata%EulerAngles(1:3,EBSDDIdata%TopMatchIndices(ii,jj))
      end do 
    end do 
    euler_bestmatch = euler_bestmatch * dtor
    deallocate(EBSDDIdata%EulerAngles, EBSDDIdata%TopMatchIndices)
  end if

  CIlist(1:Nexp) = EBSDDIdata%CI(1:Nexp)

  deallocate(EBSDDIdata%CI)

! the following arrays are kept in the EBSDDIdata structure
!   OSMmap = EBSDDIdata%OSM
!   IQmap = EBSDDIdata%IQ

    call Message(' -> completed reading of dot product file')
else
   call FatalError('EMEBSDDIrefine','This program only handles EBSPs; use EMECPDIrefine for ECPs') 
end if

Ne = dinl%numexptsingle
Nd = dinl%numdictsingle
nthreads = ronl%nthreads

if (sum(dinl%ROI).ne.0) then
  ROIselected = .TRUE.
  iiistart = dinl%ROI(2)
  iiiend = dinl%ROI(2)+dinl%ROI(4)-1
  jjend = dinl%ROI(3)
else
  ROIselected = .FALSE.
  iiistart = 1
  iiiend = dinl%ipf_ht
  jjend = dinl%ipf_wd
end if

if (ROIselected.eqv..TRUE.) then 
    totnumexpt = dinl%ROI(3)*dinl%ROI(4)
else
    totnumexpt = dinl%ipf_wd*dinl%ipf_ht
end if

!===================================================================================
!===============READ MASTER AND MC FILE=============================================
!===================================================================================
!
! 1. read the Monte Carlo data file
call readEBSDMonteCarloFile(dinl%masterfile, mcnl, hdferr, EBSDMCdata, getAccume=.TRUE.)

! 2. read EBSD master pattern file (including HDF format)
call readEBSDMasterPatternFile(dinl%masterfile, mpnl, hdferr, EBSDMPdata, getmLPNH=.TRUE., getmLPSH=.TRUE.)

! 3. generate detector arrays
allocate(EBSDdetector%rgx(dinl%numsx,dinl%numsy), &
         EBSDdetector%rgy(dinl%numsx,dinl%numsy), &
         EBSDdetector%rgz(dinl%numsx,dinl%numsy), &
         EBSDdetector%accum_e_detector(EBSDMCdata%numEbins,dinl%numsx,dinl%numsy), stat=istat)

! 4. copy a few parameters from dinl to enl, which is the regular EBSDNameListType structure
! and then generate the detector arrays
ebsdnl%numsx = dinl%numsx
ebsdnl%numsy = dinl%numsy
ebsdnl%xpc = dinl%xpc
ebsdnl%ypc = dinl%ypc
ebsdnl%delta = dinl%delta
ebsdnl%thetac = dinl%thetac
ebsdnl%L = dinl%L
ebsdnl%energymin = dinl%energymin
ebsdnl%energymax = dinl%energymax
call GenerateEBSDDetector(ebsdnl, mcnl, EBSDMCdata, EBSDdetector, verbose)
deallocate(EBSDMCdata%accum_e)

! close the fortran HDF interface
! call h5close_EMsoft(hdferr)

!=====================================================
! get the indices of the minimum and maximum energy
!=====================================================
Emin = nint((dinl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

Emax = nint((dinl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) + 1
if (Emax .lt. 1) Emax = 1
if (Emax .gt. EBSDMCdata%numEbins) Emax = EBSDMCdata%numEbins

!=====================================================
!==========fill important parameters in namelist======
!=====================================================

binx = dinl%numsx/dinl%binning
biny = dinl%numsy/dinl%binning
recordsize = binx*biny*4
L = binx*biny
npy = mpnl%npx

! make sure that correctsize is a multiple of 16; if not, make it so
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if

! determine the experimental and dictionary sizes in bytes
size_in_bytes_dict = Nd*correctsize*sizeof(correctsize)
size_in_bytes_expt = Ne*correctsize*sizeof(correctsize)
patsz              = correctsize

allocate(IPAR2(10))
IPAR2 = 0

! define the jpar array
jpar(1) = dinl%binning
jpar(2) = dinl%numsx
jpar(3) = dinl%numsy
jpar(4) = mpnl%npx
jpar(5) = npy
jpar(6) = EBSDMCdata%numEbins
jpar(7) = EBSDMCdata%numEbins
!jpar(7) = dinl%nE

IPAR2(1:7) = jpar(1:7)
IPAR2(8) = Emin
IPAR2(9) = Emax
IPAR2(10)= dinl%nregions

dims2 = (/binx, biny/)

! intensity prefactor
! modified by MDG, 03/26/18
nel = float(mcnl%totnum_el) * float(EBSDMCdata%multiplier)
emult = nAmpere * 1e-9 / nel  ! multiplicative factor to convert MC data to an equivalent incident beam of 1 nanoCoulomb
! intensity prefactor  (redefined by MDG, 3/23/18)
prefactor = emult * dinl%beamcurrent * dinl%dwelltime * 1.0D-6

!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
pgnum = GetPointGroup(mcnl%xtalname,.FALSE.)
dinl%MCxtalname = trim(mcnl%xtalname)

!=====================================================
!=====================================================
! set up the equivalent pseudo-symmetric orientation quaternions
!=====================================================
!=====================================================
! do we have a PS variant file ?
if (trim(ronl%PSvariantfile).ne.'undefined') then
    ! we need to get the direct structure matrix to convert the axis-angle pair
    ! to the correct reference frame, so we'll read the complete xtal file
    ! and initialize all matrices as usual
    ! allocate (cell)
    cell%fname = trim(dinl%MCxtalname)
    call CrystalData(cell)

    call Message('Reading pseudo-symmetry variant operators: ')
    dpfile = trim(EMsoft_getEMdatapathname())//trim(ronl%PSvariantfile)
    dpfile = EMsoft_toNativePath(dpfile)

    ! this is a simple text file, similar to an euler angle file; the input should
    ! be in quaternion format, so abort when the file does not have quaternions...
    open(unit=53,file=trim(dpfile),status='old',action='read')
    read (53,*) anglemode
    if ((anglemode.ne.'ax').and.(anglemode.ne.'eu')) call FatalError('EMFitOrientationPS','angle type must be eu or ax')
    read (53,*) nvar
    nvar = nvar + 1     ! identity operation is first entry
    allocate(dpPS(ronl%matchdepth, nvar),eulerPS(3, ronl%matchdepth, nvar))

    if (anglemode.eq.'ax') then 
    ! allocate some arrays
        allocate(axPS(4,nvar), quPS(4,nvar))
        axPS = 0.0
        axPS(1:4,1) = (/ 0.0, 0.0, 1.0, 0.0 /)

        do ii = 2,nvar
            read(53,"(4F12.9)") axPS(1:4,ii)
        end do
    ! the axis should be given in crystal coordinates as a direction, so 
    ! we need to transform the axis first to the crystal cartesian frame
    ! using the direct structure matrix, and then normalize it...
        call Message(' -> Converting operators to cartesian reference frame...')
        do ii=1,nvar
            call TransSpace(cell,axPS(1:3,ii),avec,'d','c')
            call NormVec(cell,avec,'c')

            axPS(1:3,ii) = avec(1:3)
            axPS(4,ii) = axPS(4,ii) * cPi/180.D0
        end do

        quPS = 0.0

        call Message(' -> Final pseudo-symmetric quaternion operator(s): ')
        do ii = 1,nvar
            quPS(1:4,ii) = ax2qu(axPS(1:4,ii))
            if (quPS(1,ii).lt.0.0) quPS(1:4,ii) = -quPS(1:4,ii)
            io_real(1:4) = quPS(1:4,ii)
            call WriteValue('',io_real,4)
        end do
    else
        ! allocate some arrays
        allocate(euPS(3,nvar), quPS(4,nvar))
        euPS = 0.0
        euPS(1:3,1) = (/ 0.0, 0.0, 0.0 /)

        do ii = 2,nvar
            read(53,"(3F12.9)") euPS(1:3,ii)
        end do
        quPS = 0.0

        call Message(' -> Final pseudo-symmetric quaternion operator(s): ')
        do ii = 1,nvar
            quPS(1:4,ii) = eu2qu(euPS(1:3,ii))
            if (quPS(1,ii).lt.0.0) quPS(1:4,ii) = -quPS(1:4,ii)
            io_real(1:4) = quPS(1:4,ii)
            call WriteValue('',io_real,4)
        end do 
    end if
    close(52,status='keep')
    call Message('--------')
else  ! there are no pseudo-symmetric variants in this run
    nvar = 1     ! identity operation is the only entry
    allocate(dpPS(ronl%matchdepth, nvar),eulerPS(3, ronl%matchdepth, nvar), quPS(4,nvar))
    quPS(1:4,1) = (/  1.0, 0.0, 0.0, 0.0 /)
end if 
!=====================================================
!=====================================================

! allocate the dict structure
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

! initialize the symmetry matrices
call DI_Init(dict,'nil') 

!=====================================================
!==========ALLOCATE ALL ARRAYS HERE=================== 
!=====================================================
allocate(mask(binx,biny),masklin(binx*biny))
mask = 1.0
masklin = 0.0

allocate(EBSDPattern(binx,biny),tmpimageexpt(binx*biny),imageexpt(binx*biny),binned(binx,biny))
EBSDPattern = 0.0
tmpimageexpt = 0.0
imageexpt = 0.0
binned = 0.0


allocate(EBSDpatternintd(binx,biny),EBSDpatterninteger(binx,biny),EBSDpatternad(binx,biny))
EBSDpatternintd = 0.0
EBSDpatterninteger = 0
EBSDpatternad = 0.0

allocate(imagedictflt(binx*biny))
imagedictflt = 0.0

!===============================================================
! define the circular mask if necessary and convert to 1D vector
!===============================================================
if (dinl%maskpattern.eq.'y') then
  do ii = 1,biny
      do jj = 1,binx
          if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. dinl%maskradius**2) then
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

!===============================================================
!======== pre-process the experimental patterns=================
!===============================================================
! is the output to a temporary file or will it be kept in memory?
if (ronl%inRAM.eqv..TRUE.) then 
! allocate the array that will hold all the processed experimental patterns
  allocate(epatterns(correctsize,totnumexpt),stat=istat)
  if (istat .ne. 0) stop 'could not allocate array to hold processed experimental patterns'
  call PreProcessPatterns(ronl%nthreads, ronl%inRAM, dinl, binx, biny, masklin, correctsize, totnumexpt, epatterns=epatterns)
  io_real(1) = minval(epatterns)
  io_real(2) = maxval(epatterns)
  call WriteValue(' --> preprocessed patterns intensity range (kept in RAM) = ',io_real,2)
else
  ! get the tmp file name from the input name list instead of the dot product file
  ! to allow for multiple instantiations of this program to run simultaneously
  dinl%tmpfile = trim(ronl%tmpfile)
  call PreProcessPatterns(ronl%nthreads, ronl%inRAM, dinl, binx, biny, masklin, correctsize, totnumexpt)
end if

!=================================
!========LOOP VARIABLES===========
!=================================

ratioE = float(Nexp)/float(dinl%numexptsingle)
cratioE = ceiling(ratioE)
fratioE = floor(ratioE)

ppendE = (/ (dinl%numexptsingle, ii=1,cratioE) /)
if (fratioE.lt.cratioE) then
  ppendE(cratioE) = MODULO(Nexp,dinl%numexptsingle)
end if

!===================================================================================
! method = 'SUB' ... define necessary parameters
!===================================================================================
if (ronl%method.eq.'SUB') then 
    Nmis = ronl%nmis 
    niter = ronl%niter
    allocate(cubneighbor(1:3,(2*Nmis + 1)**3),stat=istat)
    if(istat .ne. 0) then
        call FatalError('EMRefineOrient','Failed to allocate cubneighbor array')
    end if
end if 

!===================================================================================
!===============BOBYQA VARIABLES====================================================
!===================================================================================
N = 3
allocate(X(N),XL(N),XU(N),INITMEANVAL(N),STEPSIZE(N))

XL = 0.D0
XU = 1.D0
RHOBEG = 0.1D0
RHOEND = 0.0001D0
IPRINT = 0
NPT = N + 6
STEPSIZE = ronl%step
verbose = .FALSE.

if (ronl%inRAM.eqv..FALSE.) then
   fname = trim(EMsoft_getEMtmppathname())//trim(dinl%tmpfile)
   fname = EMsoft_toNativePath(fname)
   open(unit=itmpexpt,file=trim(fname),&
   status='unknown',form='unformatted',access='direct',recl=correctsize*4,iostat=ierr)
end if

!===================================================================================
!===============MAIN COMPUTATION LOOP===============================================
!===================================================================================

io_int(1) = ronl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(ronl%nthreads)

call Time_tick(tickstart)

allocate(exptpatterns(binx*biny,dinl%numexptsingle),stat=istat)

! depending on the ronl%method, we perform the optimization with different routines...
if (ronl%method.eq.'FIT') then 

    do iii = 1,cratioE
        if (ronl%inRAM.eqv..FALSE.) then
            do jj = 1,ppendE(iii)
                eindex = (iii - 1)*Ne + jj
                read(itmpexpt,rec=eindex) tmpimageexpt
                exptpatterns(1:binx*biny,jj) = tmpimageexpt(1:binx*biny)
            end do
        end if

        
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,ii,tmpimageexpt,jj,quat,quat2,binned,ma,mi,eindex) &
    !$OMP& PRIVATE(EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt,kk,ll,mm) &
    !$OMP& PRIVATE(X,INITMEANVAL,dpPS,eulerPS,eurfz,euinp,pos)
         
          TID = OMP_GET_THREAD_NUM()

    !$OMP DO SCHEDULE(DYNAMIC)
          do jj = 1,ppendE(iii)

            eindex = (iii - 1)*Ne + jj
            if (ronl%inRAM.eqv..TRUE.) then
                tmpimageexpt(1:correctsize) = epatterns(1:correctsize,eindex)
                tmpimageexpt = tmpimageexpt/NORM2(tmpimageexpt)
            else
                tmpimageexpt = exptpatterns(:,jj)
            end if

    ! calculate the dot product for each of the orientations in the neighborhood of the best match
    ! including the pseudosymmetric variant; do this for all the selected top matches (matchdepth)
            do kk = 1, ronl%matchdepth    

                quat(1:4) = eu2qu(euler_bestmatch(1:3,kk,eindex))

                do ll = 1,nvar

                    quat2 = quat_mult(quPS(1:4,ll), quat)

                    euinp = qu2eu(quat2)

                    call ReduceOrientationtoRFZ(euinp, dict, FZtype, FZorder, eurfz)

                    quat2 = eu2qu(eurfz)

                    INITMEANVAL(1:3) = eu2ho(eurfz(1:3)) 
                    
                    X = 0.5D0
                    call bobyqa (IPAR2, INITMEANVAL, tmpimageexpt, N, NPT, X, XL,&
                             XU, RHOBEG, RHOEND, IPRINT, MAXFUN, EMFitOrientationcalfunEBSD, EBSDdetector%accum_e_detector,&
                             EBSDMPdata%mLPNH, EBSDMPdata%mLPSH, mask, prefactor, EBSDdetector%rgx, EBSDdetector%rgy, &
                             EBSDdetector%rgz, STEPSIZE, dinl%gammavalue, verbose)
                
                    eulerPS(1:3,kk,ll) = ho2eu((/X(1)*2.0*STEPSIZE(1) - STEPSIZE(1) + INITMEANVAL(1), &
                                                 X(2)*2.0*STEPSIZE(2) - STEPSIZE(2) + INITMEANVAL(2), &
                                                 X(3)*2.0*STEPSIZE(3) - STEPSIZE(3) + INITMEANVAL(3)/)) * 180.0/cPi

                    call EMFitOrientationcalfunEBSD(IPAR2, INITMEANVAL, tmpimageexpt, EBSDdetector%accum_e_detector, &
                                        EBSDMPdata%mLPNH, EBSDMPdata%mLPSH, N, X, F, mask, prefactor, &
                                        EBSDdetector%rgx, EBSDdetector%rgy, EBSDdetector%rgz, STEPSIZE, dinl%gammavalue, verbose)

                    dpPS(kk,ll) = 1.D0 - F
                end do
            end do

! updating the confidence index only if a better match is found
            dp = maxval(dpPS)
            if (dp .gt. CIlist(eindex)) then
              CIlist(eindex) = dp
              pos = maxloc(dpPS)
              euler_best(1:3,eindex) = eulerPS(1:3,pos(1),pos(2))
            else
              euler_best(1:3,eindex) = euler_bestmatch(1:3,1,eindex) * 180.0/cPi
            end if
            
            if (mod(eindex,250) .eq. 0) then
                io_int(1) = eindex
                io_int(2) = totnumexpt
                call Writevalue('completed refining pattern #',io_int,2,'(I8,'' of '',I8)')
            end if

        end do
    !$OMP END DO
    !$OMP END PARALLEL
     
    end do
else  ! sub-divide the cubochoric grid in half steps and determine for which gridpoint the dot product is largest
    do iii = 1,cratioE
        
        exptpatterns = 0.0
        stpsz = LPs%ap/2.D0/dinl%ncubochoric/2.D0

        if (ronl%inRAM.eqv..FALSE.) then
            do jj = 1,ppendE(iii)
                eindex = (iii - 1)*dinl%numexptsingle + jj
                read(itmpexpt,rec=eindex) tmpimageexpt
                exptpatterns(1:binx*biny,jj) = tmpimageexpt(1:binx*biny)
            end do
        end if

        do kk = 1,niter

            io_int(1) = kk
            io_int(2) = niter
            call Writevalue(' --> Starting cubochoric grid refinement ',io_int,2,'(I3,'' of '',I3)')

    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ii,tmpimageexpt,jj,quat,binned,ma,mi,eindex) &
    !$OMP& PRIVATE(EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt,ll,mm,dp) &
    !$OMP& PRIVATE(cubneighbor,cu0)
    !$OMP DO SCHEDULE(DYNAMIC)

            do ii = 1,ppendE(iii)

               eindex = (iii - 1)*dinl%numexptsingle + ii
               if (ronl%inRAM.eqv..TRUE.) then
                    tmpimageexpt(1:correctsize) = epatterns(1:correctsize,eindex)
                    tmpimageexpt = tmpimageexpt/NORM2(tmpimageexpt)
                else
                    tmpimageexpt = exptpatterns(:, ii)
                end if

                cu0 = eu2cu(euler_bestmatch(1:3,1,eindex))

                call CubochoricNeighbors(cubneighbor,Nmis,cu0,stpsz)

    ! calculate the dot product for each of the orientations in the neighborhood of the best match    
                do jj = 1,(2*Nmis + 1)**3

                    quat(1:4) = cu2qu(cubneighbor(1:3,jj)) !CMarray(1:4,jj,eindex)
            
                    call CalcEBSDPatternSingleFull(jpar,quat,EBSDdetector%accum_e_detector,EBSDMPdata%mLPNH,EBSDMPdata%mLPSH,&
                                                   EBSDdetector%rgx, EBSDdetector%rgy,EBSDdetector%rgz,binned,Emin,Emax,mask,&
                                                   prefactor)

                    ma = maxval(binned)
                    mi = minval(binned)
                  
                    EBSDpatternintd = ((binned - mi)/ (ma-mi))
                    EBSDpatterninteger = nint(EBSDpatternintd*255.0)
                    EBSDpatternad =  adhisteq(dinl%nregions,binx,biny,EBSDpatterninteger)
                    binned = float(EBSDpatternad)

                    imagedictflt = 0.0

                    do ll = 1,biny
                        do mm = 1,binx
                            imagedictflt((ll-1)*binx+mm) = binned(mm,ll)
                        end do
                    end do

                    imagedictflt = imagedictflt/NORM2(imagedictflt)

                    dp = DOT_PRODUCT(tmpimageexpt,imagedictflt)

    ! updating the confidence index if a better match is found
                    if(dp .gt. CIlist(eindex)) then
                        CIlist(eindex) = dp
                        euler_best(1:3,eindex) = qu2eu(quat) 
                    end if

                end do

                if (mod(eindex,250) .eq. 0) then
                    io_int(1) = eindex
                    io_int(2) = totnumexpt
                    call Writevalue('      completed refining pattern #',io_int,2,'(I8,'' of '',I8)')
                end if
            end do

    !$OMP END DO
    !$OMP END PARALLEL

        stpsz = stpsz/2.D0
        end do
        
    end do

end if

if (ronl%inRAM.eqv..FALSE.) then
   close(unit=itmpexpt, status='delete')
end if

!===========================================
! output section
!===========================================

! add fitted dot product values to HDF5 file
! open the fortran HDF interface
nullify(HDF_head%next)

dpfile = trim(EMsoft_getEMdatapathname())//trim(ronl%dotproductfile)
dpfile = EMsoft_toNativePath(dpfile)

! open the fortran HDF interface
!call h5open_EMsoft(hdferr)
hdferr =  HDF_openFile(dpfile, HDF_head)

! open the Scan 1/EBSD/Data group
groupname = 'Scan 1'
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSD
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_RefinedDotProducts
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray1D(dataset, CIlist, Nexp, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray1D(dataset, CIlist, Nexp, HDF_head)
end if
 
dataset = SC_RefinedEulerAngles
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(euler_best*cPi/180.0), 3, Nexp, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(euler_best*cPi/180.0), 3, Nexp, HDF_head)
end if
 
call HDF_pop(HDF_head,.TRUE.) 

! and generate the ctf output file as well... [.ang output to be added]
dinl%ctffile = ronl%ctffile

ipar = 0
ipar(1) = 1
ipar(2) = Nexp
ipar(3) = Nexp
ipar(4) = Nexp 
ipar(5) = EBSDDIdata%FZcnt
ipar(6) = pgnum
if (sum(dinl%ROI).ne.0) then
    ipar(7) = dinl%ROI(3)
    ipar(8) = dinl%ROI(4)
else
    ipar(7) = dinl%ipf_wd
    ipar(8) = dinl%ipf_ht
end if

allocate(indexmain(ipar(1),1:ipar(2)),resultmain(ipar(1),1:ipar(2)))
indexmain = 0
resultmain(1,1:ipar(2)) = CIlist(1:Nexp)

! this next command needs to be eliminated incorporation of mcnl%xtalname in the parameter list of the ctfebsd_writeFile routine
dinl%MCxtalname = trim(mcnl%xtalname)

if (dinl%ctffile.ne.'undefined') then 
  call ctfebsd_writeFile(dinl,mcnl%xtalname,ipar,indexmain,euler_best,resultmain,EBSDDIdata%OSM,EBSDDIdata%IQ,noindex=.TRUE.)
  call Message('Data stored in ctf file : '//trim(ronl%ctffile))
end if

tstop = Time_tock(tickstart) 

io_real(1) = tstop
call WriteValue('Execution time [system_clock()] = ',io_int,1,"(I8,' [s]')")

! close the fortran HDF interface
call h5close_EMsoft(hdferr)

end program EMFitOrientationPS
