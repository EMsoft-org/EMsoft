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
! EMsoft:EBSDmod.f90
!--------------------------------------------------------------------------
!
! MODULE: EBSDmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSD helper routines
!
!> @date  06/24/14  MDG 1.0 original, lifted from EMEBSD.f90 to simplify code
!> @date  09/01/15  MDG 1.1 modified EBSDMasterDIType definition to accommodate multiple Lambert maps
!> @date  09/15/15  SS  1.2 added accum_z to EBSDLargeAccumDIType
!--------------------------------------------------------------------------
module EBSDDImod

use math
use local
use typedefs
use stringconstants

IMPLICIT NONE


contains


!--------------------------------------------------------------------------
!
! SUBROUTINE:EMEBSDrefinement
!
!> @author Marc De Graef/Saransh Singh, Carnegie Mellon University
!
!> @brief pattern refinement routine 
!
!> @param progname name of the program
!> @param ronl name list
!> @param nmldeffile namelist filename
!
!> @date 09/23/19 MDG 1.0 reorganization of program structure
!> @date 04/27/20 MDG 1.1 move array (de)allocation inside parallel region (resolves issue on Windows)
!--------------------------------------------------------------------------
subroutine EMEBSDrefinement(progname, ronl, nmldeffile)
!DEC$ ATTRIBUTES DLLEXPORT :: EMEBSDrefinement

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

character(fnlen),INTENT(IN)               :: nmldeffile, progname
type(RefineOrientationtype),INTENT(INOUT) :: ronl

character(fnlen)                          :: progdesc
type(EBSDIndexingNameListType)            :: dinl
type(MCCLNameListType)                    :: mcnl
type(EBSDMasterNameListType)              :: mpnl
type(EBSDNameListType)                    :: ebsdnl, myebsdnl
type(EBSDMCdataType)                      :: EBSDMCdata
type(EBSDMPdataType)                      :: EBSDMPdata
type(EBSDDetectorType)                    :: EBSDdetector, myEBSDdetector
type(EBSDDIdataType)                      :: EBSDDIdata
  
logical                                   :: stat, readonly, noindex, ROIselected
character(fnlen)                          :: dpfile, masterfile, energyfile
integer(kind=irg)                         :: hdferr, ii, jj, kk, iii, istat, npy, jjj
  
real(kind=dbl)                            :: misang       ! desired misorientation angle (degrees)
integer(kind=irg)                         :: Nmis         ! desired number of sampling points along cube edge
integer(kind=irg)                         :: CMcnt        ! number of entries in linked list
type(FZpointd),pointer                    :: CMlist, CMtmp       ! pointer to start of linked list and temporary one
real(kind=dbl)                            :: rhozero(4), hipassw
  
real(kind=sgl),allocatable                :: euPS(:,:), euler_bestmatch(:,:,:), CIlist(:), CMarray(:,:,:)
integer(kind=irg),allocatable             :: indexmain(:,:) 
real(kind=sgl),allocatable                :: resultmain(:,:), DPCX(:), DPCY(:), DPCL(:) 
integer(HSIZE_T)                          :: dims(1),dims2D(2),dims3(3),offset3(3) 

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                          :: dataset, groupname  
character(fnlen)                          :: ename, fname    
character(2)                              :: anglemode
real(kind=dbl),parameter                  :: nAmpere = 6.241D+18   ! Coulomb per second
  
integer(c_size_t),allocatable             :: IPAR2(:)
real(kind=dbl),allocatable                :: X(:), XL(:), XU(:)
real(kind=sgl),allocatable                :: INITMEANVAL(:)
real(kind=dbl)                            :: RHOBEG, RHOEND
integer(kind=irg)                         :: NPT, N, IPRINT, NSTEP, NINIT
integer(kind=irg),parameter               :: MAXFUN = 10000
logical                                   :: verbose
  
logical                                   :: f_exists, init, g_exists, overwrite
integer(kind=irg),parameter               :: iunitexpt = 41, itmpexpt = 42
integer(kind=irg)                         :: binx, biny, recordsize, pos(2)
real(kind=sgl),allocatable                :: tmpimageexpt(:), mask(:,:), masklin(:)
real(kind=sgl),allocatable                :: imagedictflt(:), exppatarray(:)
real(kind=sgl),allocatable                :: EBSDpatternintd(:,:), binned(:,:), euler_best(:,:)
real(kind=sgl),allocatable                :: epatterns(:,:)
integer(kind=irg),allocatable             :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:), Rvar(:)
type(C_PTR)                               :: planf, HPplanf, HPplanb
integer(kind=8)                           :: size_in_bytes_dict,size_in_bytes_expt
  
real(kind=dbl)                            :: w, Jres, nel, emult 
real(kind=dbl)                            :: stpsz, cu0(3)
real(kind=dbl),allocatable                :: cubneighbor(:,:)
real(kind=sgl)                            :: quat(4), quat2(4), qq(4), ma, mi, dp, tstart, tstop, io_real(4), tmp, &
                                             vlen, avec(3), dtor, alpha, ca, sa, c2a, s2a, nn(3), omega, dx, dy, rho
real(kind=dbl)                            :: qu(4), rod(4) 
integer(kind=irg)                         :: ipar(10), Emin, Emax, nthreads, TID, io_int(2), tickstart, ierr, L, nvar, niter,i,j, &
                                             samplex, sampley, maxeindex, unchanged
integer(kind=irg)                         :: ll, mm, jpar(7), Nexp, pgnum, FZcnt, nlines, dims2(2), correctsize, totnumexpt, mystat
  
real(kind=dbl)                            :: prefactor, F, angleaxis(4)
real(kind=sgl),allocatable                :: quPS(:,:), axPS(:,:), dpPS(:,:), eulerPS(:,:,:)
real(kind=dbl)                            :: ratioE, eurfz(3), euinp(3)
integer(kind=irg)                         :: cratioE, fratioE, eindex, jjend, iiistart, iiiend, Nd, Ne, patsz, pp
integer(kind=irg),allocatable             :: ppendE(:)
real(kind=sgl),allocatable                :: exptpatterns(:,:)
  
real(kind=sgl),allocatable                :: STEPSIZE(:), OSMmap(:,:), IQmap(:)
type(dicttype),pointer                    :: dict
integer(kind=irg)                         :: FZtype, FZorder
character(fnlen)                          :: modalityname
  
type(HDFobjectStackType)                  :: HDF_head
type(unitcell)                            :: cell

dtor = sngl(cPi) / 180.0

init = .TRUE.
overwrite = .TRUE.
verbose = .FALSE.

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
                                getDictionaryEulerAngles=.TRUE., &
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
        euler_bestmatch(1:3,ii,jj) = EBSDDIdata%DictionaryEulerAngles(1:3,EBSDDIdata%TopMatchIndices(ii,jj))
      end do 
    end do 
    euler_bestmatch = euler_bestmatch * dtor
    deallocate(EBSDDIdata%DictionaryEulerAngles, EBSDDIdata%TopMatchIndices)
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
! and then generate the detector arrays; these are the generic arrays without pattern center 
! correction.
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
    allocate(quPS(4,nvar))
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
! account for the fact that the binning parameter may
! not be equal to 1 (used as of 5.0.3)
allocate(mask(dinl%exptnumsx,dinl%exptnumsy),masklin(dinl%exptnumsx*dinl%exptnumsy))
mask = 1.0
masklin = 0.0

!===============================================================
! define the circular mask if necessary and convert to 1D vector
!===============================================================
if (dinl%maskpattern.eq.'y') then
  do ii = 1,dinl%exptnumsy
      do jj = 1,dinl%exptnumsx
          if((ii-dinl%exptnumsy/2)**2 + (jj-dinl%exptnumsx/2)**2 .ge. dinl%maskradius**2) then
              mask(jj,ii) = 0.0
          end if
      end do
  end do
end if
  
do ii = 1,dinl%exptnumsy
    do jj = 1,dinl%exptnumsx
        masklin((ii-1)*dinl%exptnumsx+jj) = mask(jj,ii)
    end do
end do

!===============================================================
!======== pre-process the experimental patterns=================
!===============================================================
! is the output to a temporary file or will it be kept in memory?
dinl%similaritymetric = 'ndp'
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

! do we need to redefine the mask arrays ?
! remake the mask if the binning factor is not 1 
if ((dinl%binning.ne.1).and.(dinl%maskpattern.eq.'y') ) then 
  deallocate(mask, masklin) 
  allocate(mask(binx,biny), masklin(binx*biny))
  mask = 1.0
  masklin = 0.0

  do ii = 1,biny
    do jj = 1,binx
      if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. (dinl%maskradius/dinl%binning)**2) then
        mask(jj,ii) = 0.0
      end if
    end do
  end do

! convert the mask to a linear (1D) array
  masklin = reshape(mask, (/ binx*biny /) )
end if 

!===============================================================
!========Pattern center correction parameters===================
!===============================================================
if (trim(ronl%PCcorrection).eq.'on') then 
  alpha = 0.5 * sngl(cPi) - (mcnl%sig - dinl%thetac) * dtor  
  ca = cos(alpha)
  c2a = cos(2.0*alpha)
  sa = sin(alpha)
  s2a = sin(2.0*alpha)
! determine the shift vector for each sampling point (on the sample!) with respect to the 
! (initialx, initialy) position
  if (ROIselected.eqv..TRUE.) then 
    allocate(DPCX(dinl%ROI(3)), DPCY(dinl%ROI(4)), DPCL(dinl%ROI(4)) )
    do i=1,dinl%ROI(3)
      DPCX(i) = - ( ronl%initialx - (dinl%ROI(1)+(i-1)) ) * dinl%StepX
    end do 
    do j=1,dinl%ROI(4)
      DPCY(j) = - ( ronl%initialy - (dinl%ROI(2)+(j-1)) ) * dinl%StepY
    end do 
  else
    allocate(DPCX(dinl%ipf_wd), DPCY(dinl%ipf_ht), DPCL(dinl%ipf_ht) )
    do i=1,dinl%ipf_wd
      DPCX(i) = - ( ronl%initialx - i ) * dinl%StepX
    end do 
    do j=1,dinl%ipf_ht
      DPCY(j) = - ( ronl%initialy - j ) * dinl%StepY
    end do 
  end if
! convert these shifts to shifts in the detector reference frame 
! and put them in units of the detector pixel size 
  DPCX = - DPCX / dinl%delta
  DPCL = - DPCY * sa 
  DPCY = - DPCY * ca / dinl%delta
end if  

! write (*,*) 'DPCX : ', DPCX
! write (*,*) 'DPCY : ', DPCY
! write (*,*) 'DPCL : ', DPCL

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
! these are shared by all threads
N = 3
RHOBEG = 0.1D0
RHOEND = 0.0001D0
IPRINT = 0
NPT = N + 6
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
if (ROIselected.eqv..TRUE.) then 
  maxeindex = dinl%ROI(3) * dinl%ROI(4)
else 
  maxeindex = dinl%ipf_wd * dinl%ipf_ht
end if 

io_int(1) = ronl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(ronl%nthreads)


call Time_tick(tickstart)

allocate(exptpatterns(binx*biny,dinl%numexptsingle),stat=istat)
unchanged = 0 

! parameters for orientation correction
! if (trim(ronl%PCcorrection).eq.'on') then 
!   alpha = cPi/2.D0 - (ebsdnl%MCsig - ebsdnl%thetac)*cPi/180.0
!   ca = cos(alpha)
!   sa = sin(alpha)
!   drd = ebsdnl%stepY*ca
!   dtd = ebsdnl%stepX
!   zs = ebsdnl%L/ebsdnl%numsx/ebsdnl%delta
!   xs = 0.D0
!   ys = 0.D0
!   rho = sqrt(xs**2 + ys**2 + zs**2)
!   !r = (/(ys*ca + zs*sa)/rho, -xs/rho, (-ys*sa + zs*ca)/rho /)
!   r = (/sa, 0.D0, ca/)
! end if 

! depending on the ronl%method, we perform the optimization with different routines...
if (ronl%method.eq.'FIT') then 

    call Message(' --> Starting regular refinement loop')

    do iii = 1,cratioE
        if (ronl%inRAM.eqv..FALSE.) then
            allocate(tmpimageexpt(binx*biny))
            do jj = 1,ppendE(iii)
                eindex = (iii - 1)*Ne + jj
                read(itmpexpt,rec=eindex) tmpimageexpt
                exptpatterns(1:binx*biny,jj) = tmpimageexpt(1:binx*biny)
            end do
            deallocate(tmpimageexpt)
        end if

        
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,ii,tmpimageexpt,jj,qu,qq,quat,quat2,binned,ma,mi,eindex) &
    !$OMP& PRIVATE(EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt,kk,ll,mm, myebsdnl) &
    !$OMP& PRIVATE(X,XL,XU,STEPSIZE,INITMEANVAL,dpPS,eulerPS,eurfz,euinp,pos, mystat, myEBSDdetector) &
    !$OMP& PRIVATE(samplex, sampley, dx, dy, rho, nn, omega)
          
          allocate(X(N),XL(N),XU(N),INITMEANVAL(N),STEPSIZE(N))
          XL = 0.D0
          XU = 1.D0
          STEPSIZE = ronl%step         

          allocate(dpPS(ronl%matchdepth, nvar),eulerPS(3, ronl%matchdepth, nvar))

          allocate(tmpimageexpt(binx*biny),binned(binx,biny))
          allocate(EBSDpatternintd(binx,biny),EBSDpatterninteger(binx,biny),EBSDpatternad(binx,biny))
          allocate(imagedictflt(binx*biny))
          tmpimageexpt = 0.0
          binned = 0.0
          EBSDpatternintd = 0.0
          EBSDpatterninteger = 0
          EBSDpatternad = 0.0
          imagedictflt = 0.0

          if (trim(ronl%PCcorrection).eq.'on') then 
! allocate the necessary arrays 
            allocate(myEBSDdetector%rgx(dinl%numsx,dinl%numsy), &
                     myEBSDdetector%rgy(dinl%numsx,dinl%numsy), &
                     myEBSDdetector%rgz(dinl%numsx,dinl%numsy), &
                     myEBSDdetector%accum_e_detector(EBSDMCdata%numEbins,dinl%numsx,dinl%numsy), stat=mystat)
          end if 

          TID = OMP_GET_THREAD_NUM()

    !$OMP BARRIER

    !$OMP DO SCHEDULE(DYNAMIC)
          do jj = 1,ppendE(iii)

            eindex = (iii - 1)*Ne + jj
            if (eindex.gt.maxeindex) CYCLE 
            if (ronl%inRAM.eqv..TRUE.) then
                tmpimageexpt(1:correctsize) = epatterns(1:correctsize,eindex)
                tmpimageexpt = tmpimageexpt/vecnorm(tmpimageexpt)
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
                                                 X(3)*2.0*STEPSIZE(3) - STEPSIZE(3) + INITMEANVAL(3)/)) 

                    call EMFitOrientationcalfunEBSD(IPAR2, INITMEANVAL, tmpimageexpt, EBSDdetector%accum_e_detector, &
                                        EBSDMPdata%mLPNH, EBSDMPdata%mLPSH, N, X, F, mask, prefactor, &
                                        EBSDdetector%rgx, EBSDdetector%rgy, EBSDdetector%rgz, STEPSIZE, dinl%gammavalue, verbose)

                    dpPS(kk,ll) = 1.D0 - F

! do we need to perform a pattern center correction ?  This would be necessary for large area
! scans.  First, we apply the equivalent rotation to the refined orientation, then we create a 
! new set of detector arrays for this pattern center location, and we do another refinement step
! to get the final corrected orientation.  At the end, we make sure the new orientation falls in 
! the appropriate RFZ.
                    if ( (trim(ronl%PCcorrection).eq.'on') .and. (eindex.le.maxeindex) ) then 
! get the corrected pattern center coordinates
                      if (ROIselected.eqv..TRUE.) then 
                        samplex = mod(eindex-1, dinl%ROI(3))+1
                        sampley = (eindex-1)/dinl%ROI(3)+1
                      else 
                        samplex = mod(eindex-1, dinl%ipf_wd)+1
                        sampley = (eindex-1)/dinl%ipf_wd+1
                      end if 
                      myebsdnl = ebsdnl 
                      dx = DPCX(samplex)
                      dy = DPCY(sampley)
                      myebsdnl%xpc = ebsdnl%xpc - dx
                      myebsdnl%ypc = ebsdnl%ypc - dy
                      myebsdnl%L = ebsdnl%L - DPCL(sampley)
                      call GenerateEBSDDetector(myebsdnl, mcnl, EBSDMCdata, myEBSDdetector, verbose=.FALSE.)

! first undo the pattern center shift by an equivalent rotation (see J. Appl. Cryst. (2017). 50, 1664–1676, eq.15)
                      if ((dx.ne.0.0).or.(dy.ne.0.0)) then 
                        qu = eu2qu(eulerPS(1:3,kk,ll))
                        rho = dx**2+dy**2
                        nn = -(/dx*ca,-dy,-dx*sa/)/sqrt(rho)
                        omega = acos(ebsdnl%L/sqrt(ebsdnl%L**2 + dinl%delta**2 * rho))
                        qq = (/ cos(omega*0.5), sin(omega*0.5) * nn /)
                        quat2 = quat_mult(sngl(qu), qq)
                        INITMEANVAL(1:3) = qu2ho(quat2)
                      else
                        INITMEANVAL(1:3) = eu2ho(eulerPS(1:3,kk,ll))
                      end if 

! refine the orientation using the new detector array and initial orientation 
                      X = 0.5D0
                      call bobyqa (IPAR2, INITMEANVAL, tmpimageexpt, N, NPT, X, XL,&
                               XU, RHOBEG, RHOEND, IPRINT, MAXFUN, EMFitOrientationcalfunEBSD, myEBSDdetector%accum_e_detector,&
                               EBSDMPdata%mLPNH, EBSDMPdata%mLPSH, mask, prefactor, myEBSDdetector%rgx, myEBSDdetector%rgy, &
                               myEBSDdetector%rgz, STEPSIZE, dinl%gammavalue, verbose)
                  
                      eulerPS(1:3,kk,ll) = ho2eu((/X(1)*2.0*STEPSIZE(1) - STEPSIZE(1) + INITMEANVAL(1), &
                                                   X(2)*2.0*STEPSIZE(2) - STEPSIZE(2) + INITMEANVAL(2), &
                                                   X(3)*2.0*STEPSIZE(3) - STEPSIZE(3) + INITMEANVAL(3)/)) 

                      call EMFitOrientationcalfunEBSD(IPAR2, INITMEANVAL, tmpimageexpt, myEBSDdetector%accum_e_detector, &
                                          EBSDMPdata%mLPNH, EBSDMPdata%mLPSH, N, X, F, mask, prefactor, &
                                          myEBSDdetector%rgx, myEBSDdetector%rgy, myEBSDdetector%rgz, STEPSIZE, &
                                          dinl%gammavalue, verbose)

                      dpPS(kk,ll) = 1.D0 - F

! and return this orientation to the RFZ
                      euinp(1:3) = eulerPS(1:3,kk,ll)
                      call ReduceOrientationtoRFZ(euinp, dict, FZtype, FZorder, eurfz)
                      eulerPS(1:3,kk,ll) = eurfz(1:3)
                    end if 

                end do
            end do

! updating the confidence index only if a better match is found
            dp = maxval(dpPS)
            if (dp .gt. CIlist(eindex)) then
              CIlist(eindex) = dp
              pos = maxloc(dpPS)
              euler_best(1:3,eindex) = eulerPS(1:3,pos(1),pos(2)) * 180.0/cPi
            else
              euler_best(1:3,eindex) = euler_bestmatch(1:3,1,eindex) * 180.0/cPi
              !$OMP CRITICAL
                unchanged = unchanged + 1 
              !$OMP END CRITICAL
            end if
            
            if (mod(eindex,250) .eq. 0) then
                io_int(1) = eindex
                io_int(2) = totnumexpt
                call Writevalue('      completed refining pattern #',io_int,2,'(I8,'' of '',I8)')
            end if

        end do
    !$OMP END DO

        deallocate(tmpimageexpt,binned,EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt)
        deallocate(X,XL,XU,INITMEANVAL,STEPSIZE, eulerPS, dpPS)
        if (trim(ronl%PCcorrection).eq.'on') then
          deallocate(myEBSDdetector%rgx, myEBSDdetector%rgy)
          deallocate(myEBSDdetector%rgz, myEBSDdetector%accum_e_detector)
        end if

    !$OMP BARRIER

    !$OMP END PARALLEL
     
    end do
else  ! sub-divide the cubochoric grid in half steps and determine for which gridpoint the dot product is largest
    do iii = 1,cratioE
        
        exptpatterns = 0.0
        stpsz = LPs%ap/2.D0/dinl%ncubochoric/2.D0

        if (ronl%inRAM.eqv..FALSE.) then
            allocate(tmpimageexpt(binx*biny))
            do jj = 1,ppendE(iii)
                eindex = (iii - 1)*dinl%numexptsingle + jj
                read(itmpexpt,rec=eindex) tmpimageexpt
                exptpatterns(1:binx*biny,jj) = tmpimageexpt(1:binx*biny)
            end do
            deallocate(tmpimageexpt)
        end if

        do kk = 1,niter

            io_int(1) = kk
            io_int(2) = niter
            call Writevalue(' --> Starting cubochoric grid refinement ',io_int,2,'(I3,'' of '',I3)')

    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ii,tmpimageexpt,jj,quat,binned,ma,mi,eindex) &
    !$OMP& PRIVATE(EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt,ll,mm,dp) &
    !$OMP& PRIVATE(cubneighbor,cu0)

            allocate(tmpimageexpt(binx*biny),binned(binx,biny))
            allocate(EBSDpatternintd(binx,biny),EBSDpatterninteger(binx,biny),EBSDpatternad(binx,biny))
            allocate(imagedictflt(binx*biny))
            tmpimageexpt = 0.0
            binned = 0.0
            EBSDpatternintd = 0.0
            EBSDpatterninteger = 0
            EBSDpatternad = 0.0
            imagedictflt = 0.0

    !$OMP DO SCHEDULE(DYNAMIC)
            do ii = 1,ppendE(iii)

               eindex = (iii - 1)*dinl%numexptsingle + ii
               if (ronl%inRAM.eqv..TRUE.) then
                    tmpimageexpt(1:correctsize) = epatterns(1:correctsize,eindex)
                    tmpimageexpt = tmpimageexpt/vecnorm(tmpimageexpt)
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

                    imagedictflt = imagedictflt/vecnorm(imagedictflt)

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

            deallocate(tmpimageexpt,binned,EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt)

    !$OMP END PARALLEL

        stpsz = stpsz/2.D0
        end do
        
    end do

end if

if (ronl%inRAM.eqv..FALSE.) then
   close(unit=itmpexpt, status='delete')
end if

!===========================================
write (*,*) 'total number of unchanged points : ', unchanged,' out of ', maxeindex
!===========================================

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

end subroutine EMEBSDrefinement


!--------------------------------------------------------------------------
!
! FUNCTION: getExpEBSDpatterns
!
!> @author Marc De Graef
!
!> @brief Read an array of nx x ny x np experimental EBSD patterns from an HDF5 input file
!
!> @detail For now, the only HDF5 input file format is the one generated by the TSL software;
!> this will be updated at a later time to include potential other vendor formats.
!
!> @param enl EBSD cluster name list structure
!
!> @date 12/28/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getExpEBSDpatterns(enl) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: getExpEBSDpatterns

use local
use typedefs
use NameListTypedefs
use HDF5
use HDFsupport

type(EBSDclusterNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
real(kind=dbl),allocatable                      :: rdata(:,:,:)

character(len=1), allocatable                   :: cdata(:,:,:)
integer(kind=irg)                               :: hdferr, i, j, k
integer(HSIZE_T)                                :: rdims(3)
character(fnlen)                                :: datafile, groupname, dataset

type(HDFobjectStackType)                        :: HDF_head

! Initialize FORTRAN interface.
!
nullify(HDF_head%next)

! Open the HDF5 file  in readonly mode
datafile = trim(EMsoft_getEMdatapathname())//trim(enl%inputfilename)
datafile = EMsoft_toNativePath(datafile)

hdferr =  HDF_openFile(datafile, HDF_head, .TRUE.)

! open the correct group (only the Scan group should be entered in the namelist file)
groupname = trim(enl%groupname)//'/EBSD/Data'
hdferr = HDF_openGroup(groupname, HDF_head)

! open the correct dataset and read the data
dataset = trim(enl%datasetname)
call HDF_readDatasetCharArray3D(dataset, rdims, HDF_head, hdferr, cdata)

call HDF_pop(HDF_head)

! get the number of image columns and rows in the EBSD data set
groupname = trim(enl%groupname)//'/EBSD/Header'
hdferr = HDF_openGroup(groupname, HDF_head)

! open the correct dataset and read the data
dataset = SC_nColumns
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%NScanColumns)

dataset = SC_nRows
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%NScanRows)

! close the datafile
call HDF_pop(HDF_head,.TRUE.)

! convert to double precision intensities between 0 and 1
allocate(rdata(1:rdims(1),1:rdims(2),1:rdims(3)))
do k=1,rdims(3)
  do j=1,rdims(2)
    do i=1,rdims(1)
      rdata(i,j,k) = dble(ichar(cdata(i,j,k)))
    end do
  end do
end do
rdata = rdata/maxval(rdata)

deallocate(cdata)

end function getExpEBSDpatterns

!--------------------------------------------------------------------------
!
! SUBROUTINE: EBSDprepExpPatterns
!
!> @author Marc De Graef
!
!> @brief Read an array of nx x ny x np experimental EBSD patterns from an HDF5 input file
!
!> @param rdata experimental EBSD patterns; will be modified on output
!> @param dims dimensions of rdata array
!> @param w hi-pass filter mask semi-edge length (mask is square)
!> @param applymask OPTIONAL apply circular mask
!> @param normalize OPTIONAL normalize each pattern to unit total intensity
!
!> @date 12/28/15 MDG 1.0 original
!> @date 01/05/15 MDG 1.1 added w parameter to set size of hi-pass filter window
!--------------------------------------------------------------------------
recursive subroutine EBSDprepExpPatterns(enl,rdata,dims,w,applymask,normalize) 
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDprepExpPatterns

use local
use typedefs
use NameListTypedefs
use HDF5
use HDFsupport
use FFTW3mod
use io

type(EBSDclusterNameListType),INTENT(IN):: enl
integer(kind=irg),INTENT(IN)            :: dims(3)
integer(kind=irg),INTENT(IN)            :: w
real(kind=dbl),INTENT(INOUT)            :: rdata(dims(1),dims(2),dims(3))
!f2py intent(in,out) ::  rdata
logical,INTENT(IN),OPTIONAL             :: applymask
logical,INTENT(IN),OPTIONAL             :: normalize

complex(kind=dbl)                       :: hpmask(dims(1),dims(2)) 
real(kind=dbl)                          :: cmask(dims(1),dims(2)), pat(dims(1),dims(2)), r2, shx, shy, r
integer(kind=irg)                       :: i, j, k
complex(kind=dbl)                       :: cone = cmplx(1.D0,0.D0), czero = cmplx(0.D0,0.D0), cimag = cmplx(0.D0,1.D0)

! fftw variables
type(C_PTR)                             :: planf, planb
complex(C_DOUBLE_COMPLEX)               :: inp(dims(1),dims(2)), outp(dims(1),dims(2))

! first of all, we compute the high-pass filtering mask (will become variable size in future)
! we simply create a square array of complex zeroes centered on the origin
hpmask = cone
hpmask(1:w,1:w) = czero
hpmask(dims(1)-w:dims(1),1:w) = czero
hpmask(1:w,dims(2)-w:dims(2)) = czero
hpmask(dims(1)-w:dims(1),dims(2)-w:dims(2)) = czero

call Message('EBSDprepExpPatterns: performing hi-pass FFT filtering')

! then we set up the fftw plans for forward and reverse transforms
planf = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_FORWARD, FFTW_ESTIMATE)
planb = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_BACKWARD, FFTW_ESTIMATE)

! and we apply the hi-pass mask to each pattern
do i=1,dims(3)
  do j=1,dims(1)
   do k=1,dims(1)
    inp(j,k) = cmplx(rdata(j,k,i),0.D0)    
   end do
  end do
  call fftw_execute_dft(planf, inp, outp)
  inp = outp * hpmask
  call fftw_execute_dft(planb, inp, outp) 
  rdata(1:dims(1),1:dims(2),i) = real(outp)
end do

call fftw_destroy_plan(planf)
call fftw_destroy_plan(planb)
call fftw_cleanup()

! do we need to bin the patterns down ?
if (enl%binfactor.ne.1) then
  call Message('EBSDprepExpPatterns: binning patterns (to be implemented)')
end if

! do we need to apply a circular mask to these patterns ?
if (PRESENT(applymask)) then
  if (applymask) then
    call Message('EBSDprepExpPatterns: applying circular mask')
! initialize the circular mask first
    cmask = 0.D0
    r2 = ( dble(minval( (/ dims(1), dims(2) /) ))* 0.5D0 )**2
    shx = dble(dims(1))*0.5D0
    shy = dble(dims(2))*0.5D0
    do i=1,dims(1)
      do j=1,dims(2)
        r = (dble(i)-shx)**2 + (dble(j)-shy)**2
        if (r.le.r2) cmask(i,j) = 1.D0
      end do
    end do
! then apply it to all the patterns
    do k=1,dims(3)
      pat = rdata(1:dims(1),1:dims(2),k) * cmask
      rdata(1:dims(1),1:dims(2),k) = pat
    end do
  end if
end if

! do we need to normalize all the patterns ?
if (PRESENT(normalize)) then
  if (normalize) then
    call Message('EBSDprepExpPatterns: normalizing patterns')
    do k=1,dims(3)
      pat = rdata(1:dims(1),1:dims(2),k)
      r = dsqrt(sum(pat*pat))
      pat = pat / r
      rdata(1:dims(1),1:dims(2),k) = pat
    end do
  end if
end if

end subroutine EBSDprepExpPatterns


!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDIndexingreadMCfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file
!
!> @param enl EBSD name list structure
!> @param acc energy structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 11/18/14  MDG 1.1 removed enl%MCnthreads from file read
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 04/29/15  MDG 2.1 add optional parameter efile
!> @date 09/15/15  SS  2.2 added accum_z reading
!> @date 01/26/16  SS  2.3 adjusted for EBSDIndexing 
!> @date 10/11/16  MDG 2.4 conversion to new HDF5 file organization
!--------------------------------------------------------------------------
recursive subroutine EBSDIndexingreadMCfile(enl,acc,efile,verbose,NoHDFInterfaceOpen)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDIndexingreadMCfile

use local
use typedefs
use NameListTypedefs
use files
use io
use EBSDmod
use HDF5
use HDFsupport
use error

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(EBSDLargeAccumDIType),pointer              :: acc
character(fnlen),INTENT(IN),OPTIONAL            :: efile
logical,INTENT(IN),OPTIONAL                     :: verbose
logical,INTENT(IN),OPTIONAL                     :: NoHDFInterfaceOpen

type(MCCLNameListType)                          :: mcnl
type(EBSDMCdataType)                            :: EBSDMCdata
integer(kind=irg)                               :: istat, hdferr, nx, sz3(3), sz4(4)
logical                                         :: stat, HDFopen
character(fnlen)                                :: energyfile 

! is the efile parameter present? If so, use it as the filename, otherwise use the enl%energyfile parameter
if (PRESENT(efile)) then
  energyfile = efile
else
  energyfile = trim(EMsoft_getEMdatapathname())//trim(enl%energyfile)
end if
energyfile = EMsoft_toNativePath(energyfile)

HDFopen = .TRUE.
if (present(NoHDFInterfaceOpen)) then
  if (NoHDFInterfaceOpen.eqv..FALSE.) HDFopen = .FALSE.
end if 

allocate(acc)

if (HDFopen.eqv..TRUE.) call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(enl%energyfile, mcnl, hdferr, EBSDMCdata, getAccumz=.TRUE., getAccume=.TRUE.)
if (HDFopen.eqv..TRUE.) call h5close_EMsoft(hdferr)

! copy all the necessary variables from the mcnl namelist group
enl%MCxtalname = trim(mcnl%xtalname)
enl%MCmode = mcnl%MCmode
!if (enl%MCmode .ne. 'full') call FatalError('EBSDIndexingreadMCfile','File not in full mode. Please input correct HDF5 file')

enl%nsx = (mcnl%numsx - 1)/2
enl%nsy = enl%nsx

enl%EkeV = mcnl%EkeV
enl%Ehistmin = mcnl%Ehistmin

enl%Ebinsize = mcnl%Ebinsize
enl%depthmax = mcnl%depthmax
enl%depthstep = mcnl%depthstep
enl%MCsig = mcnl%sig
enl%MComega = mcnl%omega
enl%totnum_el = EBSDMCdata%totnum_el
enl%multiplier = EBSDMCdata%multiplier

! it is not clear whether or not these are really ever used ...  
! a grep of all the source code shows that they are not used at all
! dataset = SC_ProgramName
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%MCprogname = trim(stringarray(1))
!   deallocate(stringarray)

! dataset = SC_Version
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%MCscversion = trim(stringarray(1))
!   deallocate(stringarray)

enl%numEbins = EBSDMCdata%numEbins
enl%numzbins = EBSDMCdata%numzbins
enl%num_el = sum(EBSDMCdata%accum_e)
sz3 = shape(EBSDMCdata%accum_e)
nx = (sz3(2)-1)/2
allocate(acc%accum_e(1:sz3(1),-nx:nx,-nx:nx))
acc%accum_e = EBSDMCdata%accum_e
deallocate(EBSDMCdata%accum_e)
  
sz4 = shape(EBSDMCdata%accum_z)
allocate(acc%accum_z(1:sz4(1),1:sz4(2),1:sz4(3),1:sz4(4)))
acc%accum_z = EBSDMCdata%accum_z
deallocate(EBSDMCdata%accum_z)

if (present(verbose)) call Message(' -> completed reading Monte Carlo data from '//trim(enl%energyfile), frm = "(A)")

end subroutine EBSDIndexingreadMCfile

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDIndexingreadMasterfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read EBSD master pattern from file
!
!> @param enl EBSD name list structure
!> @param 
!
!> @date 06/24/14  MDG 1.0 original
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 09/01/15  MDG 3.0 changed Lambert maps to Northern + Southern maps; lots of changes...
!> @date 09/03/15  MDG 3.1 removed support for old file format (too difficult to maintain after above changes)
!> @date 01/26/16  SS  3.2 adjusted for EBSDIndexing 
!--------------------------------------------------------------------------
recursive subroutine EBSDIndexingreadMasterfile(enl, master, mfile, verbose, NoHDFInterfaceOpen)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDIndexingreadMasterfile

use local
use typedefs
use NameListTypedefs
use files
use io
use error
use HDF5
use HDFsupport


IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(EBSDMasterDIType),pointer                  :: master
character(fnlen),INTENT(IN),OPTIONAL            :: mfile
logical,INTENT(IN),OPTIONAL                     :: verbose
logical,OPTIONAL,INTENT(IN)                     :: NoHDFInterfaceOpen

real(kind=sgl),allocatable                      :: mLPNH(:,:,:) 
real(kind=sgl),allocatable                      :: mLPSH(:,:,:) 
real(kind=sgl),allocatable                      :: EkeVs(:) 
integer(kind=irg),allocatable                   :: atomtype(:)

real(kind=sgl),allocatable                      :: srtmp(:,:,:,:)
integer(kind=irg)                               :: istat

logical                                         :: stat, readonly, HDFopen 
integer(kind=irg)                               :: hdferr, nlines
integer(HSIZE_T)                                :: dims(1), dims4(4)
character(fnlen)                                :: groupname, dataset, masterfile
character(fnlen),allocatable                    :: stringarray(:)

type(HDFobjectStackType)                        :: HDF_head

HDFopen = .TRUE.
if (present(NoHDFInterfaceOpen)) then
  if (NoHDFInterfaceOpen.eqv..FALSE.) HDFopen = .FALSE.
end if 

! open the fortran HDF interface
if (HDFopen.eqv..TRUE.) call h5open_EMsoft(hdferr)

nullify(HDF_head%next)

! is the mfile parameter present? If so, use it as the filename, otherwise use the enl%masterfile parameter
if (PRESENT(mfile)) then
  masterfile = mfile
else
  masterfile = trim(EMsoft_getEMdatapathname())//trim(enl%masterfile)
end if
masterfile = EMsoft_toNativePath(masterfile)

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(masterfile), stat, hdferr)

if (stat) then 
! open the master file 
  readonly = .TRUE.
  hdferr =  HDF_openFile(masterfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_EBSDMasterNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_energyfile
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterenergyfile = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_npx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
  enl%npy = enl%npx

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_EBSDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)
! make sure that MC and Master results are compatible
  if ((enl%numEbins.ne.enl%nE).and.(.not.PRESENT(mfile))) then
    call Message('Energy histogram and Lambert stack have different energy dimension; aborting program', frm = "(A)")
    call HDF_pop(HDF_head,.TRUE.)
    stop
  end if

dataset = SC_numset
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numset)

! dataset = 'squhex'
! call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
! enl%sqorhe = trim(stringarray(1))
! deallocate(stringarray)

dataset = SC_mLPNH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPNH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_mLPSH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPSH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterxtalname = trim(stringarray(1))
  deallocate(stringarray)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_EBSDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_ProgramName
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterprogname = trim(stringarray(1))
  deallocate(stringarray)
  
dataset = SC_Version
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterscversion = trim(stringarray(1))
  deallocate(stringarray)
  
  call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
if (HDFopen.eqv..TRUE.)  call h5close_EMsoft(hdferr)

else
  masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
  call FatalError('EBSDIndexingreadMasterfile',masterfile)
end if
!====================================

if (present(verbose)) call Message(' -> completed reading dynamical scattering data from '//trim(enl%masterfile),frm="(A)")

end subroutine EBSDIndexingreadMasterfile

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDIndexingGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl EBSD name list structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS  1.1 added omega as the second tilt angle
!> @date 07/07/15   SS  1.2 correction to the omega tilt parameter; old version in the comments
!> @date 01/26/16   SS  1.3 adjusted for EBSDIndexing
!> @date 06/12/16  MDG  1.4 added correction for effetive detector pixel size w.r.t. equal area mapping
!> @date 02/19/19  MDG  2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine EBSDIndexingGenerateDetector(enl, acc, master, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDIndexingGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(EBSDLargeAccumDIType),pointer              :: acc
type(EBSDMasterDIType),pointer                  :: master
logical,INTENT(IN),OPTIONAL                     :: verbose

real(kind=sgl),allocatable                      :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                        :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                                  :: alp, ca, sa, cw, sw
real(kind=sgl)                                  :: L2, Ls, Lc     ! distances
real(kind=sgl),allocatable                      :: z(:,:)           
integer(kind=irg)                               :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nixp, niyp, elp  
real(kind=sgl)                                  :: dc(3), scl, pcvec(3), alpha, theta, gam, dp           ! direction cosine array
real(kind=sgl)                                  :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                                  :: ixy(2)


!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(enl%numsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i-1, i=1,enl%numsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (enl%MCsig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
elp = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   master%rgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   master%rgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   master%rgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(master%rgx*master%rgx+master%rgy*master%rgy+master%rgz*master%rgz)
  master%rgx = master%rgx*z
  master%rgy = master%rgy*z
  master%rgz = master%rgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(enl%nsx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.enl%numEbins)  Emin=enl%numEbins

  Emax = nint((enl%energymax - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.enl%numEbins)  Emax=enl%numEbins

! get an estimate of the cone opening angle for which the projected area at the pattern
! center is the same as delta**2
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))

! then get the direction cosines for the pattern center
  ipx = enl%numsx/2+nint(enl%xpc)
  ipy = enl%numsy/2+nint(enl%ypc)
  pcvec = (/ master%rgx(ipx,ipy), master%rgy(ipx,ipy), master%rgz(ipx,ipy) /)
  !pcvec = (/enl%ypc*enl%delta*ca + enl%xpc*enl%delta*sa*sw + enl%L*cw*sa, &
  !         enl%L*sw - enl%xpc*enl%delta*cw,&
  !         enl%L*ca*cw + enl%xpc*enl%delta*ca*sw - enl%ypc*enl%delta*sa/)
  !pcvec = pcvec/vecnorm(pcvec)

  do i=1,enl%numsx
    do j=1,enl%numsy
! do the coordinate transformation for this detector pixel
       dc = (/ master%rgx(i,j),master%rgy(i,j),master%rgz(i,j) /)

! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
       call LambertgetInterpolation(dc, scl, enl%nsx, enl%nsy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)

! do the area correction for this detector pixel
       dp = dot_product(pcvec,dc)
       theta = acos(dp)
       if ((i.eq.ipx).and.(j.eq.ipy)) then
          gam = 0.25 
       else
          gam = 2.0 * tan(alpha) * dp / ( tan(theta+alpha) - tan(theta-alpha) ) * 0.25
       end if
! interpolate the intensity 
       do k=Emin,Emax 
          acc%accum_e_detector(k,i,elp-j) = gam * ( acc%accum_e(k,nix,niy) * dxm * dym + &
                                                    acc%accum_e(k,nixp,niy) * dx * dym + &
                                                    acc%accum_e(k,nix,niyp) * dxm * dy + &
                                                    acc%accum_e(k,nixp,niyp) * dx * dy )
       end do
    end do
  end do 


! and finally, get rid of the original accum_e array which is no longer needed
! [we'll do that in the calling program ]
!  deallocate(accum_e)

!====================================
end subroutine EBSDIndexingGenerateDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDFastIndexingGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl EBSD name list structure
!
!> @date 06/24/14  MDG  1.0 original
!> @date 07/01/15   SS  1.1 added omega as the second tilt angle
!> @date 07/07/15   SS  1.2 correction to the omega tilt parameter; old version in the comments
!> @date 01/26/16   SS  1.3 adjusted for EBSDIndexing
!> @date 06/12/16  MDG  1.4 added correction for effetive detector pixel size w.r.t. equal area mapping
!> @date 07/06/17  MDG  2.0 split from regular routine for an N-line detector
!> @date 02/19/19  MDG  3.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine EBSDFastIndexingGenerateDetector(enl, acc, master, nlines, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDFastIndexingGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(EBSDLargeAccumDIType),pointer              :: acc
type(EBSDMasterDIType),pointer                  :: master
integer(kind=irg),INTENT(IN)                    :: nlines
logical,INTENT(IN),OPTIONAL                     :: verbose

real(kind=sgl),allocatable                      :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                        :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                                  :: alp, ca, sa, cw, sw
real(kind=sgl)                                  :: L2, Ls, Lc     ! distances
real(kind=sgl),allocatable                      :: z(:,:)           
integer(kind=irg)                               :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, ystep, nixp, &
                                                   niyp, elp   ! various parameters
real(kind=sgl)                                  :: dc(3), scl, pcvec(3), alpha, theta, gam, dp ! direction cosine array
real(kind=sgl)                                  :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                                  :: ixy(2)


!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(nlines),stat=istat)
! if (istat.ne.0) then ...
! we use nlines horizontal lines on the detector, equidistant from each other;
! the scin_x array remains unchanged from the regular detector definition
scin_x = - ( enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta

! this requires a change of the scin_y array definition to a larger vertical step size 
ystep = floor(float(enl%numsy)/float(nlines+1))
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i*ystep, i=1,nlines) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (enl%MCsig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
elp = nlines + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,nlines
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   master%rgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   master%rgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   master%rgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,nlines))
  z = 1.0/sqrt(master%rgx*master%rgx+master%rgy*master%rgy+master%rgz*master%rgz)
  master%rgx = master%rgx*z
  master%rgy = master%rgy*z
  master%rgz = master%rgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(enl%nsx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.enl%numEbins)  Emin=enl%numEbins

  Emax = nint((enl%energymax - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.enl%numEbins)  Emax=enl%numEbins

! get an estimate of the cone opening angle for which the projected area at the pattern
! center is the same as delta**2
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))

! then get the direction cosines for the pattern center
  ipx = enl%numsx/2+nint(enl%xpc)
  ipy = enl%numsy/2+nint(enl%ypc)
  !pcvec = (/ master%rgx(ipx,ipy), master%rgy(ipx,ipy), master%rgz(ipx,ipy) /)
  pcvec = (/enl%ypc*enl%delta*ca + enl%xpc*enl%delta*sa*sw + enl%L*cw*sa, &
           enl%L*sw - enl%xpc*enl%delta*cw,&
           enl%L*ca*cw + enl%xpc*enl%delta*ca*sw - enl%ypc*enl%delta*sa/)
  pcvec = pcvec/vecnorm(pcvec)

  do i=1,enl%numsx
    do j=1,nlines
! do the coordinate transformation for this detector pixel
       dc = (/ master%rgx(i,j),master%rgy(i,j),master%rgz(i,j) /)

! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
       call LambertgetInterpolation(dc, scl, enl%nsx, enl%nsy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)

! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          gam = 0.25 
        else
          gam = 2.0 * tan(alpha) * dp / ( tan(theta+alpha) - tan(theta-alpha) ) * 0.25
        end if
! interpolate the intensity 
        do k=Emin,Emax 
          acc%accum_e_detector(k,i,elp-j) = gam * ( acc%accum_e(k,nix,niy) * dxm * dym + &
                                                    acc%accum_e(k,nixp,niy) * dx * dym + &
                                                    acc%accum_e(k,nix,niyp) * dxm * dy + &
                                                    acc%accum_e(k,nixp,niyp) * dx * dy )
        end do
    end do
  end do 


! and finally, get rid of the original accum_e array which is no longer needed
! [we'll do that in the calling program ]
!  deallocate(accum_e)

!====================================
end subroutine EBSDFastIndexingGenerateDetector

!--------------------------------------------------------------------------
!
! FUNCTION: getEBSDIQ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the EBSD Image Quality using the second moment of the power spectrum
!
!> @details This is based on Krieger Lassen's pattern sharpness definition: Q = 1 - J / Jres wtot
!> more details page 93 of thesis of Farangis Ram.
!
!> @param dimx x pattern dimension
!> @param dimy y pattern dimension
!> @param pattern input EBSD pattern
!
!> @date 02/07/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getEBSDIQ(dimx, dimy, pattern, init) result(Q)
!DEC$ ATTRIBUTES DLLEXPORT :: getEBSDIQ

use local
use typedefs
use FFTW3mod

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dimx
integer(kind=irg),INTENT(IN)            :: dimy
real(kind=sgl),INTENT(IN)               :: pattern(dimx,dimy)
logical,INTENT(IN),OPTIONAL             :: init
real(kind=dbl)                          :: Q

real(kind=dbl),allocatable,SAVE         :: ksqarray(:,:)
real(kind=dbl),SAVE                     :: Jres
type(C_PTR),SAVE                        :: planf

real(kind=dbl)                          :: J, wtot
complex(C_DOUBLE_COMPLEX),pointer       :: inp(:,:)
complex(C_DOUBLE_COMPLEX),pointer       :: outp(:,:)
type(C_PTR)                             :: p, o
real(kind=dbl)                          :: w(dimx,dimy), linex(dimx), liney(dimy)
integer(kind=irg)                       :: i

p = fftw_alloc_complex(int(dimx*dimy,C_SIZE_T))
call c_f_pointer(p, inp, [dimx,dimy])

o = fftw_alloc_complex(int(dimx*dimy,C_SIZE_T))
call c_f_pointer(o, outp, [dimx,dimy])

if (present(init)) then
  if (init.eqv..TRUE.) then 
    allocate(ksqarray(dimx,dimy))

    inp = cmplx(0.D0,0D0)
    outp = cmplx(0.D0,0.D0)

! set up the fftw plan for the forward transform
    planf = fftw_plan_dft_2d(dimy,dimx,inp,outp, FFTW_FORWARD,FFTW_ESTIMATE)

! generate the parameter/array needed by the getEBSDIQ function
    ksqarray = 0.D0
    Jres = 0.D0

    linex = (/ (dble(i),i=0,dimx-1) /) 
    linex(dimx/2+1:dimx) = linex(dimx/2+1:dimx) - dble(dimx)
    linex = linex**2
    liney = (/ (dble(i),i=0,dimy-1) /) 
    liney(dimy/2+1:dimy) = liney(dimy/2+1:dimy) - dble(dimy)
    liney = liney**2
    
    do i=1,dimx
        ksqarray(i,1:dimy) = linex(i) + liney(1:dimy)
    end do
    Jres = sum(ksqarray) / dble(dimx) / dble(dimy)
  end if
else
  inp = cmplx(0.D0,0D0)
  inp = pattern
  outp = cmplx(0.D0,0.D0)

! compute the Fourier transform
  call fftw_execute_dft(planf, inp, outp)

  w = sqrt(real(outp)**2 + aimag(outp)**2)

! sum over the arrays
  J = sum(w*ksqarray)
  wtot = sum(w)

! and return the quality parametere
  Q = 1.0 - J/Jres/wtot
end if

call fftw_free(p)
call fftw_free(o)
call fftw_cleanup()

end function getEBSDIQ



!--------------------------------------------------------------------------
!
! SUBROUTINE: get_EBSDDI_memory_pattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief determine which large arrays to hold in memory, if any
!
!> @param ebsdnl EBSD namelist
!> @param holdexpt logical
!> @param holddict logical
!
!> @date 03/15/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine get_EBSDDI_memory_pattern(ebsdnl, holdexpt, holddict)
!DEC$ ATTRIBUTES DLLEXPORT :: get_EBSDDI_memory_pattern

use local
use typedefs
use NameListTypedefs
use HDF5
use h5im
use h5lt
use HDFsupport


IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
logical,INTENT(OUT)                                 :: holdexpt
logical,INTENT(OUT)                                 :: holddict

integer(kind=irg)                                   :: available_memory


! these will be returned to the calling program
holdexpt = .FALSE.
holddict = .FALSE.

! get the available memory in bytes
!available_memory = ebsdnl%sysmem * 1024 * 1024 * 1024

! open each of the HDF5 files for expt and dict patterns and find out
! the array sizes





end subroutine get_EBSDDI_memory_pattern



!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcEBSDPatternSingleApprox
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief compute an approximate single EBSD pattern, used in many programs
!
!> @param ebsdnl EBSD namelist
!> @param holdexpt logical
!> @param holddict logical
!
!> @date 03/17/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcEBSDPatternSingleApprox(ipar,qu,acc_array,mLPNH,mLPSH,rgx,rgy,rgz,binned,mask,prefactor)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcEBSDPatternSingleApprox

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use symmetry
use crystal
use constants
use io
use files
use diffraction
use Lambert
use quaternions
use rotations

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                    :: ipar(7)
real(kind=sgl),INTENT(IN)                       :: qu(4) 
real(kind=dbl),INTENT(IN)                       :: prefactor
integer(kind=irg),INTENT(IN)                    :: acc_array(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5))
real(kind=sgl),INTENT(IN)                       :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5))
real(kind=sgl),INTENT(IN)                       :: rgx(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgy(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgz(ipar(2),ipar(3))
real(kind=sgl),INTENT(OUT)                      :: binned(ipar(2)/ipar(1),ipar(3)/ipar(1))
real(kind=sgl),INTENT(IN)                       :: mask(ipar(2)/ipar(1),ipar(3)/ipar(1))

real(kind=sgl),allocatable                      :: EBSDpattern(:,:)
real(kind=sgl),allocatable                      :: wf(:)
real(kind=sgl)                                  :: dc(3),ixy(2),scl,bindx
real(kind=sgl)                                  :: dx,dy,dxm,dym
integer(kind=irg)                               :: ii,jj,kk,istat
integer(kind=irg)                               :: nix,niy,nixp,niyp

! ipar(1) = ebsdnl%binning
! ipar(2) = ebsdnl%numsx
! ipar(3) = ebsdnl%numsy
! ipar(4) = ebsdnl%npx
! ipar(5) = ebsdnl%npy
! ipar(6) = ebsdnl%numEbins
! ipar(7) = ebsdnl%nE


bindx = 1.0/float(ipar(1))**2

allocate(EBSDpattern(ipar(2),ipar(3)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(3)

        dc = quat_Lp(qu(1:4),  (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /) )

        dc = dc/sqrt(sum(dc**2))

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, ipar(4), ipar(5), nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
        if (dc(3) .ge. 0.0) then
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + acc_array(ii,jj) * ( mLPNH(nix,niy) * dxm * dym + &
                                               mLPNH(nixp,niy) * dx * dym + mLPNH(nix,niyp) * dxm * dy + &
                                               mLPNH(nixp,niyp) * dx * dy )
        else
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + acc_array(ii,jj) * ( mLPSH(nix,niy) * dxm * dym + &
                                               mLPSH(nixp,niy) * dx * dym + mLPSH(nix,niyp) * dxm * dy + &
                                               mLPSH(nixp,niyp) * dx * dy )

        end if
    end do
end do

EBSDpattern = prefactor * EBSDpattern

if (ipar(1) .ne. 1) then
    do ii=1,ipar(2),ipar(1)
        do jj=1,ipar(3),ipar(1)
            binned(ii/ipar(1)+1,jj/ipar(1)+1) = &
            sum(EBSDpattern(ii:ii+ipar(1)-1,jj:jj+ipar(1)-1))
        end do
    end do
! and divide by binning^2

    binned = binned * bindx
else
    binned = EBSDpattern
end if

binned = binned * mask

end subroutine CalcEBSDPatternSingleApprox

!--------------------------------------------------------------------------
!
! SUBROUTINE: readEBSDDotProductFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a Dot Product File from Dictionary Indexing into the correct namelist and data structure
!
!> @param dpfile filename of the EBSD dot product file
!> @param ebsdnl EBSDIndexingNamelist
!> @param hdferr error code
!
!> @date 03/12/18 MDG 1.0 started new routine
!> @date 02/05/19 MDG 1.1 added presentFolder optional keyword to turn off standard path handling
!--------------------------------------------------------------------------
recursive subroutine  readEBSDDotProductFile(dpfile, ebsdnl, hdferr, EBSDDIdata, getADP, getAverageOrientations, getCI, &
                                            getEulerAngles, getFit, getIQ, getKAM, getOSM, getPhase, getPhi1, &
                                            getPhi, getPhi2, getSEMsignal, getTopDotProductList, getTopMatchIndices, & 
                                            getValid, getXPosition, getYPosition, getRefinedDotProducts, &
                                            getRefinedEulerAngles, getDictionaryEulerAngles, presentFolder)
!DEC$ ATTRIBUTES DLLEXPORT :: readEBSDDotProductFile

use local
use typedefs
use NameListTypedefs
use error
use HDF5
use HDFsupport
use io
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                         :: dpfile
type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
integer(kind=irg),INTENT(OUT)                       :: hdferr
type(EBSDDIdataType),INTENT(INOUT)                  :: EBSDDIdata
!f2py intent(in,out) ::  EBSDDIdata
logical,INTENT(IN),OPTIONAL                         :: getADP
logical,INTENT(IN),OPTIONAL                         :: getAverageOrientations
logical,INTENT(IN),OPTIONAL                         :: getCI
logical,INTENT(IN),OPTIONAL                         :: getEulerAngles
logical,INTENT(IN),OPTIONAL                         :: getDictionaryEulerAngles
logical,INTENT(IN),OPTIONAL                         :: getFit
logical,INTENT(IN),OPTIONAL                         :: getIQ
logical,INTENT(IN),OPTIONAL                         :: getKAM
logical,INTENT(IN),OPTIONAL                         :: getOSM
logical,INTENT(IN),OPTIONAL                         :: getPhase
logical,INTENT(IN),OPTIONAL                         :: getPhi1
logical,INTENT(IN),OPTIONAL                         :: getPhi
logical,INTENT(IN),OPTIONAL                         :: getPhi2
logical,INTENT(IN),OPTIONAL                         :: getSEMsignal
logical,INTENT(IN),OPTIONAL                         :: getTopDotProductList
logical,INTENT(IN),OPTIONAL                         :: getTopMatchIndices
logical,INTENT(IN),OPTIONAL                         :: getValid
logical,INTENT(IN),OPTIONAL                         :: getXPosition
logical,INTENT(IN),OPTIONAL                         :: getYPosition
logical,INTENT(IN),OPTIONAL                         :: getRefinedDotProducts
logical,INTENT(IN),OPTIONAL                         :: getRefinedEulerAngles
logical,INTENT(IN),OPTIONAL                         :: presentFolder 

character(fnlen)                                    :: infile, groupname, dataset
logical                                             :: stat, readonly, g_exists, h_exists
type(HDFobjectStackType)                            :: HDF_head
integer(kind=irg)                                   :: ii, nlines
integer(kind=irg),allocatable                       :: iarray(:)
real(kind=sgl),allocatable                          :: farray(:)
integer(HSIZE_T)                                    :: dims(1), dims2(2), dims3(3), offset3(3) 
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

! we assume that the calling program has opened the HDF interface

! check to see if we are using the standard path handling or omitting the path completely
if (present(presentFolder)) then 
  infile = trim(dpfile)
else 
  infile = trim(EMsoft_getEMdatapathname())//trim(dpfile)
  infile = EMsoft_toNativePath(infile)
end if

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(infile), stat, hdferr)

!===================================================================================
!===============read dot product file===============================================
!===================================================================================

if (stat.eqv..FALSE.) then ! the file exists, so let's open it an first make sure it is an EBSD dot product file
   call FatalError('readEBSDDotProductFile','This is not a proper HDF5 file')
end if 
   
! open the dot product file 
nullify(HDF_head%next)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

! make sure this is an EBSD dot product file
groupname = SC_NMLfiles
    hdferr = HDF_openGroup(groupname, HDF_head)

dataset = 'EBSDDictionaryIndexingNML'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)

dataset = 'IndexEBSD'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),h_exists, hdferr)

if ((g_exists.eqv..FALSE.).and.(h_exists.eqv..FALSE.)) then
    call FatalError('readEBSDDotProductFile','this is not an EBSD dot product or SI file')
end if

if (g_exists) then 
  call Message(' --> EBSD dictionary indexing file found')
end if

if (h_exists) then 
  call Message(' --> EBSD spherical indexing file found')
end if

call HDF_pop(HDF_head)

! set this value to -1 initially to trigger steps in the calling routine 

EBSDDIdata%Nexp = -1

if (g_exists.eqv..TRUE.) then
!====================================
! read all NMLparameters group datasets
!====================================
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDIndexingNameListType
    hdferr = HDF_openGroup(groupname, HDF_head)

! we'll read these roughly in the order that the HDFView program displays them...
dataset = SC_HDFstrings
    call hdf_readdatasetstringarray(dataset, nlines, hdf_head, hdferr, stringarray)
    do ii=1,10
      ebsdnl%hdfstrings(ii) = trim(stringarray(ii))
    end do
    deallocate(stringarray)

dataset = SC_L
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%L)

dataset = SC_ncubochoric  
! There is an issue with the capitalization on this variable; needs to be resolved 
! [MDG 10/18/17]  We test to see if Ncubochoric exists; if it does not then we check
! for ncubochoric ...
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ncubochoric)
else
    dataset = 'ncubochoric'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ncubochoric)
end if

dataset = SC_ROI
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then
    call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, iarray)
    ebsdnl%ROI(1:4) = iarray(1:4)
    deallocate(iarray)
else
    ebsdnl%ROI = (/ 0, 0, 0, 0 /)
end if

dataset = SC_angfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%angfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_anglefile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%anglefile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_axisangle
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, farray)
    ebsdnl%axisangle(1:4) = farray(1:4)
    deallocate(farray)

dataset = SC_beamcurrent
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%beamcurrent)

dataset = SC_binning
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%binning)

dataset = SC_ctffile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%ctffile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_datafile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%datafile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_delta
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%delta)

dataset = SC_devid
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%devid)

dataset = SC_dwelltime
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%dwelltime)

dataset = SC_energyaverage
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%energyaverage)

dataset = SC_energyfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%energyfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_energymax
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%energymax)

dataset = SC_energymin
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%energymin)

dataset = SC_eulerfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%eulerfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_exptfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%exptfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_gammavalue
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%gammavalue)

dataset = SC_hipassw
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%hipassw)

dataset = SC_inputtype
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%inputtype = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_ipfht
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ipf_ht)

dataset = SC_ipfwd
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ipf_wd)

dataset = SC_maskfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%maskfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_maskpattern
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%maskpattern = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_maskradius
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%maskradius)

dataset = SC_masterfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%masterfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_nnav
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nnav)

dataset = SC_nnk
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nnk)

dataset = SC_nosm
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nosm)

dataset = SC_nregions
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nregions)

dataset = SC_nthreads
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nthreads)

dataset = SC_numdictsingle
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%numdictsingle)

dataset = SC_numexptsingle
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%numexptsingle)

dataset = SC_numsx
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%numsx)

dataset = SC_numsy
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%numsy)

!=====================================================
! check here for the exptnumsx(y) parameters that were introduced in 5.0.3
dataset = 'exptnumsx'
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%exptnumsx)
    else 
      ebsdnl%exptnumsx = ebsdnl%numsx 
    end if 

dataset = 'exptnumsy'
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%exptnumsy)
    else 
      ebsdnl%exptnumsy = ebsdnl%numsy 
    end if 
!=====================================================

dataset = SC_omega
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%omega)

dataset = SC_platid
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%platid)

dataset = SC_scalingmode
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%scalingmode = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_spatialaverage
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%spatialaverage = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_thetac
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%thetac)

dataset = SC_tmpfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%tmpfile = trim(stringarray(1))
    deallocate(stringarray)
    
dataset = SC_xpc
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%xpc)

dataset = SC_ypc
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%ypc)

! and close the NMLparameters group
    call HDF_pop(HDF_head)
    call HDF_pop(HDF_head)
end if 
!====================================
!====================================

! open the Scan 1/EBSD/Data group; dictionary indexing files only have one "scan" in them...
groupname = 'Scan 1'
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSD
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
    hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_FZcnt
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDDIdata%FZcnt)
    else
      call Message('  --> no FZcnt data set found ... continuing ... ')
    end if

dataset = SC_NumExptPatterns
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDDIdata%Nexp)
    else
      call Message('  --> no NumExptPatterns data set found ... continuing ... ')
    end if

dataset = SC_PointGroupNumber
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDDIdata%pgnum)
    else
      call Message('  --> no PointGroupNumber data set found ... continuing ... ')
    end if

! various optional arrays
if (present(getADP)) then
  if (getADP.eqv..TRUE.) then
   dataset = SC_AvDotProductMap
   call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
   if (g_exists.eqv..TRUE.) then
    if (sum(ebsdnl%ROI).ne.0) then 
      allocate(EBSDDIdata%ADP(ebsdnl%ROI(3), ebsdnl%ROI(4)))
      call HDF_read2DImage(dataset, EBSDDIdata%ADP, ebsdnl%ROI(3), ebsdnl%ROI(4), HDF_head)
    else
      allocate(EBSDDIdata%ADP(ebsdnl%ipf_wd, ebsdnl%ipf_ht))
      call HDF_read2DImage(dataset, EBSDDIdata%ADP, ebsdnl%ipf_wd, ebsdnl%ipf_ht, HDF_head)
    end if 
   else
        call Message('  --> no AvDotProductMap data set found ... continuing ... ')
   end if
  end if 
end if

if (present(getAverageOrientations)) then
  if (getAverageOrientations.eqv..TRUE.) then
    dataset = SC_AverageOrientations
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%AverageOrientations)
    else
      call Message('  --> no AverageOrientations data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getCI)) then
  if (getCI.eqv..TRUE.) then
! if this is a Spherical Indexing file, then we should look for the 'Metric' data set,
! otherwise the CI data set 
    dataset = SC_CI
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%CI)
    else
      call Message('  --> no CI data set found ... continuing ... ')
      dataset = 'Metric'
      call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
      if (g_exists.eqv..TRUE.) then
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%CI)
      else
        call Message('  --> no Metric data set found ... continuing ... ')
      end if
    end if
  end if 
end if

if (present(getEulerAngles)) then
  if (getEulerAngles.eqv..TRUE.) then
    dataset = SC_EulerAngles
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%EulerAngles)
   else
      call Message('  --> no EulerAngles data set found ... continuing ... ')
   end if
  end if 
end if

if (present(getDictionaryEulerAngles)) then
  if (getDictionaryEulerAngles.eqv..TRUE.) then
    dataset = 'DictionaryEulerAngles'
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%DictionaryEulerAngles)
    else
      call Message('  --> no DictionaryEulerAngles data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getFit)) then
  if (getFit.eqv..TRUE.) then
    dataset = SC_Fit
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Fit)
    else
      call Message('  --> no Fit data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getIQ)) then
  if (getIQ.eqv..TRUE.) then
    dataset = SC_IQ
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%IQ)
    else
      call Message('  --> no IQ data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getKAM)) then
  if (getKAM.eqv..TRUE.) then
    dataset = SC_KAM
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%KAM)
    else
      call Message('  --> no KAM data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getOSM)) then
  if (getOSM.eqv..TRUE.) then
    dataset = SC_OSM
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%OSM)
    else
      call Message('  --> no OSM data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getPhase)) then   ! this is a 1-byte integer, to be implemented 
  if (getPhase.eqv..TRUE.) then
!   dataset = SC_Phase
!   call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phase)
    call Message('Phase','reading the Phase variable is not yet implemented')
  end if 
end if

if (present(getPhi)) then
  if (getPhi.eqv..TRUE.) then
    dataset = SC_Phi
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phi)
    else
      call Message('  --> no Phi data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getPhi1)) then
  if (getPhi1.eqv..TRUE.) then
    dataset = SC_Phi1
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phi1)
    else
      call Message('  --> no Phi1 data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getPhi2)) then
  if (getPhi2.eqv..TRUE.) then
    dataset = SC_Phi2
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phi2)
    else
      call Message('  --> no Phi2 data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getSEMsignal)) then
  if (getSEMsignal.eqv..TRUE.) then
    dataset = SC_SEMsignal
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%SEMsignal)
    else
      call Message('  --> no SEMsignal data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getTopDotProductList)) then
  if (getTopDotProductList.eqv..TRUE.) then
    dataset = SC_TopDotProductList
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%TopDotProductList)
    else
      call Message('  --> no TopDotProductList data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getTopMatchIndices)) then
  if (getTopMatchIndices.eqv..TRUE.) then
    dataset = SC_TopMatchIndices
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetIntegerArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%TopMatchIndices)
    else
      call Message('  --> no TopMatchIndices data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getValid)) then
  if (getValid.eqv..TRUE.) then
!   dataset = SC_Valid
!   call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Valid)
    call Message('Valid','reading the Valid variable is not yet implemented')
  end if 
end if

if (present(getXPosition)) then
  if (getXPosition.eqv..TRUE.) then
    dataset = SC_XPosition
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%XPosition)
    else
      call Message('  --> no XPosition data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getYPosition)) then
  if (getYPosition.eqv..TRUE.) then
    dataset = SC_YPosition
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists.eqv..TRUE.) then
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%YPosition)
    else
      call Message('  --> no YPosition data set found ... continuing ... ')
    end if
  end if 
end if

if (present(getRefinedDotProducts)) then
  if (getRefinedDotProducts.eqv..TRUE.) then
    dataset = SC_RefinedDotProducts
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%RefinedDotProducts)
    else
      call Message('readEBSDDotProductFile','There is no RefinedDotProducts data set in this file')
    end if
  end if 
end if

if (present(getRefinedEulerAngles)) then
  if (getRefinedEulerAngles.eqv..TRUE.) then
    dataset = SC_RefinedEulerAngles
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%RefinedEulerAngles)
    else
      call Message('readEBSDDotProductFile','There is no RefinedEulerAngles data set in this file')
    end if
  end if 
end if

call HDF_pop(HDF_head)

groupname = SC_Header
    hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_StepX
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepX)
    else
      call Message('readEBSDDotProductFile','There is no StepX data set in this file')
    end if
 
dataset = SC_StepY
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepY)
    else
      call Message('readEBSDDotProductFile','There is no StepY data set in this file')
    end if

! if (present()) then
!   if (get.eqv..TRUE.) then

!   end if 
! end if

! and close the HDF5 dot product file
call HDF_pop(HDF_head,.TRUE.)
nullify(HDF_head%next)
 
end subroutine readEBSDDotProductFile

end module EBSDDImod
