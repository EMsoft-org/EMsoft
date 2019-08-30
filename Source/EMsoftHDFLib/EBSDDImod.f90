! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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


use local
use typedefs
use stringconstants

IMPLICIT NONE

type EBSDAngleDIType
        real(kind=sgl),allocatable      :: quatang(:,:)
end type EBSDAngleDIType

type EBSDLargeAccumDIType
        integer(kind=irg),allocatable   :: accum_e(:,:,:),accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
end type EBSDLargeAccumDIType

type EBSDMasterDIType
        real(kind=sgl),allocatable      :: mLPNH(:,:,:) , mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)          ! auxiliary detector arrays needed for interpolation
end type EBSDMasterDIType
        
type EBSDDIdataType
  integer(kind=irg)             :: FZcnt
  integer(kind=irg)             :: Nexp
  integer(kind=irg)             :: pgnum
  integer(kind=sgl),allocatable :: ADP(:,:)
  real(kind=sgl),allocatable    :: AverageOrientations(:,:)
  real(kind=sgl),allocatable    :: CI(:)
  real(kind=sgl),allocatable    :: EulerAngles(:,:)
  real(kind=sgl),allocatable    :: DictionaryEulerAngles(:,:)
  real(kind=sgl),allocatable    :: Fit(:)
  real(kind=sgl),allocatable    :: IQ(:)
  real(kind=sgl),allocatable    :: KAM(:,:)
  real(kind=sgl),allocatable    :: OSM(:,:)
  integer(kind=irg),allocatable :: Phase(:)
  real(kind=sgl),allocatable    :: Phi1(:)
  real(kind=sgl),allocatable    :: Phi(:)
  real(kind=sgl),allocatable    :: Phi2(:)
  integer(kind=irg),allocatable :: SEMsignal(:)
  real(kind=sgl),allocatable    :: TopDotProductList(:,:)
  integer(kind=irg),allocatable :: TopMatchIndices(:,:)
  integer(kind=irg),allocatable :: Valid(:)
  real(kind=sgl),allocatable    :: XPosition(:)
  real(kind=sgl),allocatable    :: YPosition(:)
  real(kind=sgl),allocatable    :: RefinedEulerAngles(:,:)
  real(kind=sgl),allocatable    :: RefinedDotProducts(:)
end type EBSDDIdataType


contains


!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDDISubroutine
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Master subroutine to control dictionary generation, inner products computations, sorting values
!> and indexing of points, all in parallel using OpenCL/openMP
!
!> @param dinl ped indexing namelist pointer
!> @param mcnl Monte Carlo namelist structure 
!> @param mpnl Master PAttern namelist structure 
!> @param EBSDMCdata Monte Carlo data arrays
!> @param EBSDMPdata master pattern data arrays
!> @param EBSDdetector detector arrays
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
!> @date 02/22/18 MDG 2.3 added support for Region-of-Interest (ROI) selection
!> @date 04/04/18 MDG 3.0 revised name list use as well as MC and MP data structures
!> @date 11/10/18 MDG 3.1 added EDAX/TSL .ang output format
!> @date 05/19/19 MDG 3.2 removes several averaging routines that were never used
!> @date 06/19/19 MDG 3.3 add support for overlap master patterns generated by EMEBSDoverlap.f90
!--------------------------------------------------------------------------
subroutine EBSDDISubroutine(dinl, mcnl, mpnl, EBSDMCdata, EBSDMPdata, EBSDdetector, progname, nmldeffile)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDDISubroutine
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
  !use EBSDDImod
  use clfortran
  use CLsupport
  use omp_lib
  use HDF5
  use h5im
  use h5lt
  use HDFsupport
  use EMh5ebsd
  use commonmod
  use EBSDiomod
  use NameListHDFwriters
  use ECPmod, only: GetPointGroup
  use Indexingmod
  use ISO_C_BINDING
  use notifications
  use TIFF_f90
  use timing
  
  IMPLICIT NONE
  
  type(EBSDIndexingNameListType),INTENT(INOUT)        :: dinl
!f2py intent(in,out) ::  dinl
  type(MCCLNameListType),INTENT(INOUT)                :: mcnl
!f2py intent(in,out) ::  mcnl
  type(EBSDMasterNameListType),INTENT(INOUT)          :: mpnl
!f2py intent(in,out) ::  mpnl
  type(EBSDMCdataType),INTENT(INOUT)                  :: EBSDMCdata
!f2py intent(in,out) ::  EBSDMCdata
  type(EBSDMPdataType),INTENT(INOUT)                  :: EBSDMPdata
!f2py intent(in,out) ::  EBSDMPdata
  type(EBSDDetectorType),INTENT(INOUT)                :: EBSDdetector
!f2py intent(in,out) ::  EBSDdetector
  character(fnlen),INTENT(IN)                         :: progname
  character(fnlen),INTENT(IN)                         :: nmldeffile
  
  type(unitcell)                                      :: cell
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
  
  integer(kind=irg)                                   :: num,ierr,irec,istat, jpar(7), SGnum, nlines
  integer(kind=irg),parameter                         :: iunit = 40
  integer(kind=irg),parameter                         :: iunitexpt = 41
  integer(kind=irg),parameter                         :: iunitdict = 42
  character(fnlen)                                    :: info ! info about the GPU
  real(kind=dbl),parameter                            :: nAmpere = 6.241D+18   ! Coulomb per second
  
  
  integer(kind=irg)                                   :: Ne,Nd,L,totnumexpt,numdictsingle,numexptsingle,imght,imgwd,nnk, &
                                                         recordsize, fratio, cratio, fratioE, cratioE, iii, itmpexpt, hdferr,&
                                                         recordsize_correct, patsz, tickstart, tock, npy, sz(3), jjj
  integer(kind=8)                                     :: size_in_bytes_dict,size_in_bytes_expt
  real(kind=sgl),pointer                              :: dict(:), T0dict(:)
  real(kind=sgl),allocatable,TARGET                   :: dict1(:), dict2(:)
  !integer(kind=1),allocatable                         :: imageexpt(:),imagedict(:)
  real(kind=sgl),allocatable                          :: imageexpt(:),imagedict(:), mask(:,:),masklin(:), exptIQ(:), &
                                                         exptCI(:), exptFit(:), exppatarray(:), tmpexppatarray(:)
  real(kind=sgl),allocatable                          :: imageexptflt(:),binned(:,:),imagedictflt(:),imagedictfltflip(:), &
                                                         tmpimageexpt(:), OSMmap(:,:)
  real(kind=sgl),allocatable, target                  :: results(:),expt(:),dicttranspose(:),resultarray(:),&
                                                         eulerarray(:,:),eulerarray2(:,:),resultmain(:,:),resulttmp(:,:)
  integer(kind=irg),allocatable                       :: acc_array(:,:), ppend(:), ppendE(:) 
  integer*4,allocatable                               :: iexptCI(:,:), iexptIQ(:,:)
  real(kind=sgl),allocatable                          :: meandict(:),meanexpt(:),wf(:),mLPNH(:,:,:),mLPSH(:,:,:),accum_e_MC(:,:,:)
  real(kind=sgl),allocatable                          :: mLPNH_simple(:,:), mLPSH_simple(:,:), eangle(:)
  real(kind=sgl),allocatable                          :: EBSDpattern(:,:), FZarray(:,:), dpmap(:), lstore(:,:), pstore(:,:)
  real(kind=sgl),allocatable                          :: EBSDpatternintd(:,:), lp(:), cp(:), EBSDpat(:,:)
  integer(kind=irg),allocatable                       :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:)
  character(kind=c_char),allocatable                  :: EBSDdictpat(:,:,:)
  real(kind=sgl),allocatable                          :: EBSDdictpatflt(:,:)
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
  character(fnlen)                                    :: groupname, dataset, fname, clname, ename, sourcefile, &
                                                         datagroupname, dictfile, attname
  integer(hsize_t)                                    :: expwidth, expheight
  integer(hsize_t),allocatable                        :: iPhase(:), iValid(:)
  integer(c_size_t),target                            :: slength
  integer(c_int)                                      :: numd, nump
  type(C_PTR)                                         :: planf, HPplanf, HPplanb
  integer(HSIZE_T)                                    :: dims2(2), offset2(2), dims3(3), offset3(3)
  
  integer(kind=irg)                                   :: i,j,ii,jj,kk,ll,mm,pp,qq
  integer(kind=irg)                                   :: FZcnt, pgnum, io_int(4), ncubochoric, pc
  type(FZpointd),pointer                              :: FZlist, FZtmp
  integer(kind=irg),allocatable                       :: indexlist(:),indexarray(:),indexmain(:,:),indextmp(:,:)
  real(kind=sgl)                                      :: dmin,voltage,scl,ratio, mi, ma, ratioE, io_real(2), tstart, tmp, &
                                                         totnum_el, vlen, tstop, ttime
  real(kind=dbl)                                      :: prefactor
  character(fnlen)                                    :: xtalname
  integer(kind=irg)                                   :: binx,biny,TID,nthreads,Emin,Emax, iiistart, iiiend, jjend
  real(kind=sgl)                                      :: sx,dx,dxm,dy,dym,rhos,x,projweight, dp, mvres, nel, emult
  real(kind=sgl)                                      :: dc(3),quat(4),ixy(2),bindx
  integer(kind=irg)                                   :: nix,niy,nixp,niyp
  real(kind=sgl)                                      :: euler(3)
  integer(kind=irg)                                   :: indx
  integer(kind=irg)                                   :: correctsize
  logical                                             :: f_exists, init, ROIselected 
  
  integer(kind=irg)                                   :: ipar(10)
  
  character(fnlen),ALLOCATABLE                        :: MessageLines(:)
  integer(kind=irg)                                   :: NumLines
  character(fnlen)                                    :: TitleMessage, exectime
  character(100)                                      :: c
  character(1000)                                     :: charline
  character(3)                                        :: stratt
  
  type(HDFobjectStackType)                            :: HDF_head
  
  call timestamp(datestring=dstr, timestring=tstrb)
  
  if (trim(dinl%indexingmode).eq.'static') then
      !
      ! Initialize FORTRAN interface.
      CALL h5open_EMsoft(hdferr)
  
      ! get the full filename
      dictfile = trim(EMsoft_getEMdatapathname())//trim(dinl%dictfile)
      dictfile = EMsoft_toNativePath(dictfile)
  
      call Message('-->  '//'Opening HDF5 dictionary file '//trim(dinl%dictfile))
      nullify(HDF_head%next)
  
      hdferr =  HDF_openFile(dictfile, HDF_head)
      if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
  
      ! we need the point group number (derived from the space group number)
      ! if EBSDMPdata%newSGnumber is set to 2, then pgnum must be set to 1 for 
      ! overlap master patterns  [ added by MDG, 06/19/19 ]
      if (EBSDMPdata%AveragedMP.eqv..TRUE.) then 
          pgnum = EBSDMPdata%newPGnumber
      else
          groupname = SC_CrystalData
          hdferr = HDF_openGroup(groupname, HDF_head)
          if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup:CrystalData')
  
          dataset = SC_SpaceGroupNumber
          call HDF_readDatasetInteger(dataset, HDF_head, hdferr, SGnum)
          if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetInteger:SpaceGroupNumber')
          call HDF_pop(HDF_head)
      ! get the point group number    
          if (SGnum.ge.221) then
            pgnum = 32
          else
            i=0
            do while (SGPG(i+1).le.SGnum) 
              i = i+1
            end do
            pgnum = i
          end if
      end if
  
      ! then read some more data from the EMData group
      groupname = SC_EMData
      hdferr = HDF_openGroup(groupname, HDF_head)
      if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup:EMData')
  
      datagroupname = 'EBSD'
      hdferr = HDF_openGroup(datagroupname, HDF_head)
      if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup:EBSD')
  
  ! test the HDF_FileVersion to make sure that the dictionary file is recent enough
      attname = 'HDF_FileVersion'
      hdferr = HDF_getStringAttributeFromGroup(attname, stratt, 3_SIZE_T, HDF_head)
  
      if (stratt.eq.'4.0') then
          call Message('The dictionary file was created with an older version of the EMEBSD program.')
          call Message('This file can not be used by the present program; must be version 4.1 or higher.')
          call Message('')
          call FatalError('EBSDDISubroutine','Incompatible dictionary file; please rerun the EMEBSD program.')
      end if
  
  
      ! we already have the xtalname string from the Monte Carlo name list
      xtalname = trim(mcnl%xtalname)
  
      ! number of Eulerangles numangles
      dataset = SC_numangles
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)
      if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetInteger:numangles')
  
      ! euler angle list Eulerangles
      dataset = SC_Eulerangles
      call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, eulerarray2)
      eulerarray2 = eulerarray2 * 180.0/sngl(cPi)
      if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetFloatArray2D:Eulerangles')
  
      ! we leave this file open since we still need to read all the patterns...
      !=====================================================
      call Message('-->  completed initial reading of dictionary file ')
  end if
  
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
  
  verbose = .FALSE.
  init = .TRUE.
  Ne = dinl%numexptsingle
  Nd = dinl%numdictsingle
  L = dinl%numsx*dinl%numsy/dinl%binning**2
  if (ROIselected.eqv..TRUE.) then 
      totnumexpt = dinl%ROI(3)*dinl%ROI(4)
  else
      totnumexpt = dinl%ipf_wd*dinl%ipf_ht
  end if
  imght = dinl%numsx/dinl%binning
  imgwd = dinl%numsy/dinl%binning
  dims = (/imght, imgwd/)
  nnk = dinl%nnk
  ncubochoric = dinl%ncubochoric
  recordsize = L*4
  itmpexpt = 43
  w = dinl%hipassw
  source_l = source_length
  
  ! these will eventually need to be read from an experimental data file but we'll set defaults values here.
  dinl%WD = 10.0
  
  ! nullify the dict and T0dict pointers
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
  
  
  if (trim(dinl%indexingmode).eq.'dynamic') then 
      !=====================================================
      ! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
      !=====================================================
      write (*,*) 'reading from xtalfile '//trim(mcnl%xtalname)

      if (EBSDMPdata%AveragedMP.eqv..TRUE.) then 
          pgnum = EBSDMPdata%newPGnumber
      else    
          pgnum = GetPointGroup(mcnl%xtalname)
      end if
  
      !=====================================================
      ! make sure the minimum energy is set smaller than the maximum
      !=====================================================
      if (dinl%energymin.gt.dinl%energymax) then
          call Message('Minimum energy is larger than maximum energy; please correct input file')
          stop
      end if
  
      !=====================================================
      ! get the indices of the minimum and maximum energy
      !=====================================================
      Emin = nint((dinl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
      if (Emin.lt.1)  Emin=1
      if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins
  
      Emax = nint((dinl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) + 1
      if (Emax .lt. 1) Emax = 1
      if (Emax .gt. EBSDMCdata%numEbins) Emax = EBSDMCdata%numEbins
  
      sz = shape(EBSDMPdata%mLPNH)
      dinl%nE = sz(3)
  
      ! intensity prefactor
      nel = float(mcnl%totnum_el) * float(EBSDMCdata%multiplier)
      emult = nAmpere * 1e-9 / nel  ! multiplicative factor to convert MC data to an equivalent incident beam of 1 nanoCoulomb
      ! intensity prefactor  (redefined by MDG, 3/23/18)
      prefactor = emult * dinl%beamcurrent * dinl%dwelltime * 1.0D-6
  end if
  
  !====================================
  ! init a bunch of parameters
  !====================================
  ! binned pattern array
  binx = dinl%numsx/dinl%binning
  biny = dinl%numsy/dinl%binning
  bindx = 1.0/float(dinl%binning)**2
  
  ! allocate the square-Lambert arrays
  npy = mpnl%npx
  if (trim(dinl%indexingmode).eq.'dynamic') then
    allocate(mLPNH(-mpnl%npx:mpnl%npx,-npy:npy,EBSDMCdata%numEbins))
    allocate(mLPSH(-mpnl%npx:mpnl%npx,-npy:npy,EBSDMCdata%numEbins))
    allocate(accum_e_MC(EBSDMCdata%numEbins,dinl%numsx,dinl%numsy),stat=istat)
    accum_e_MC = EBSDdetector%accum_e_detector
    mLPNH = EBSDMPdata%mLPNH
    mLPSH = EBSDMPdata%mLPSH
  end if
  
  !=====================================================
  ! SAMPLING OF RODRIGUES FUNDAMENTAL ZONE
  !=====================================================
  ! if eulerfile is not defined, then we use the standard RFZ sampling;
  ! if it is defined, then we read the Eulerangle triplets from the file
  ! and generate the FZlist here... this can be useful to index patterns that
  ! have only a small misorientation range with respect to a known orientation,
  ! so that it is not necessary to scan all of orientation space.
  if (trim(dinl%indexingmode).eq.'dynamic') then
      nullify(FZlist)
      FZcnt = 0
      if (trim(dinl%eulerfile).eq.'undefined') then
        call Message('Orientation space sampling mode set to RFZ')
        io_int(1) = pgnum
        io_int(2) = ncubochoric
        call WriteValue('Point group number and number of cubochoric sampling points : ',io_int,2,"(I4,',',I5)")
  
        call sampleRFZ(ncubochoric, pgnum, 0, FZcnt, FZlist)
      else
      ! read the euler angle file and create the linked list
        call getEulersfromFile(dinl%eulerfile, FZcnt, FZlist) 
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
  end if 
  
  
  !================================
  ! INITIALIZATION OF OpenCL DEVICE
  !================================
  call Message('--> Initializing OpenCL device')
  
  call CLinit_PDCCQ(platform, nump, dinl%platid, device, numd, dinl%devid, info, context, command_queue)
  
  ! read the cl source file
  sourcefile = 'DictIndx.cl'
  call CLread_source_file(sourcefile, csource, slength)
  
  ! allocate device memory for experimental and dictionary patterns
  cl_expt = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_expt, C_NULL_PTR, ierr)
  call CLerror_check('EBSDDISubroutine:clCreateBuffer', ierr)
  
  cl_dict = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dict, C_NULL_PTR, ierr)
  call CLerror_check('EBSDDISubroutine:clCreateBuffer', ierr)
  
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
  ierr2 = clGetProgramBuildInfo(prog, device(dinl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
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
  
  ! allocate(EBSDpattern(dinl%numsx,dinl%numsy),binned(binx,biny),stat=istat)
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
  eulerarray = 0.0
  if (trim(dinl%indexingmode).eq.'static') then
      eulerarray(1:3,1:FZcnt) = eulerarray2(1:3,1:FZcnt)
      deallocate(eulerarray2)
  end if
  
  allocate(exptIQ(totnumexpt), exptCI(totnumexpt), exptFit(totnumexpt), stat=istat)
  if (istat .ne. 0) stop 'could not allocate exptIQ array'
  
  allocate(rdata(binx,biny),fdata(binx,biny),stat=istat)
  if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'
  rdata = 0.D0
  fdata = 0.D0
  
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
  
  if (trim(dinl%maskfile).ne.'undefined') then
  ! read the mask from file; the mask can be defined by a 2D array of 0 and 1 values
  ! that is stored in row form as strings, e.g.    
  !    0000001110000000
  !    0000011111000000
  ! ... etc
  !
      f_exists = .FALSE.
      fname = trim(EMsoft_getEMdatapathname())//trim(dinl%maskfile)
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
        call FatalError('EBSDDISubroutine','maskfile '//trim(fname)//' does not exist')
      end if
  else
      if (dinl%maskpattern.eq.'y') then
        do ii = 1,biny
            do jj = 1,binx
                if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. dinl%maskradius**2) then
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
  !=====================================================
  call h5open_EMsoft(hdferr)
  call PreProcessPatterns(dinl%nthreads, .FALSE., dinl, binx, biny, masklin, correctsize, totnumexpt, exptIQ=exptIQ)
  call h5close_EMsoft(hdferr)
  
  !=====================================================
  call Message(' -> computing Average Dot Product map (ADP)')
  call Message(' ')
  
  ! re-open the temporary file
  fname = trim(EMsoft_getEMtmppathname())//trim(dinl%tmpfile)
  fname = EMsoft_toNativePath(fname)
  
  open(unit=itmpexpt,file=trim(fname),&
       status='old',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
  
  ! use the getADPmap routine in the filters module
  if (ROIselected.eqv..TRUE.) then
    allocate(dpmap(dinl%ROI(3)*dinl%ROI(4)))
    call getADPmap(itmpexpt, dinl%ROI(3)*dinl%ROI(4), L, dinl%ROI(3), dinl%ROI(4), dpmap)
    TIFF_nx = dinl%ROI(3)
    TIFF_ny = dinl%ROI(4)
  else
    allocate(dpmap(totnumexpt))
    call getADPmap(itmpexpt, totnumexpt, L, dinl%ipf_wd, dinl%ipf_ht, dpmap)
    TIFF_nx = dinl%ipf_wd
    TIFF_ny = dinl%ipf_ht
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
  call Time_tick(tickstart)
  
  if (trim(dinl%indexingmode).eq.'dynamic') then
      call OMP_SET_NUM_THREADS(dinl%nthreads)
      io_int(1) = dinl%nthreads
  else
      call OMP_SET_NUM_THREADS(2)
      io_int(1) = 2
  end if
  call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")
  
  ! define the jpar array of integer parameters
  jpar(1) = dinl%binning
  jpar(2) = dinl%numsx
  jpar(3) = dinl%numsy
  jpar(4) = mpnl%npx
  jpar(5) = npy
  jpar(6) = EBSDMCdata%numEbins
  jpar(7) = dinl%nE
  
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
  
  !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,iii,jj,ll,mm,pp,ierr,io_int, tock, ttime) &
  !$OMP& PRIVATE(binned, ma, mi, EBSDpatternintd, EBSDpatterninteger, EBSDpatternad, quat, imagedictflt,imagedictfltflip)
  
          TID = OMP_GET_THREAD_NUM()
  
        if ((ii.eq.1).and.(TID.eq.0)) write(*,*) ' actual number of OpenMP threads  = ',OMP_GET_NUM_THREADS()
  
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
        call CLerror_check('EBSDDISubroutine:clEnqueueWriteBuffer', ierr)
  
        mvres = 0.0
  
        experimentalloop: do jj = 1,cratioE
  
          expt = 0.0
  
          do pp = 1,ppendE(jj)   ! Ne or MODULO(totnumexpt,Ne)
            read(itmpexpt,rec=(jj-1)*Ne+pp) tmpimageexpt
            expt((pp-1)*correctsize+1:pp*correctsize) = tmpimageexpt
          end do
  
          ierr = clEnqueueWriteBuffer(command_queue, cl_expt, CL_TRUE, 0_8, size_in_bytes_expt, C_LOC(expt(1)), &
                                      0, C_NULL_PTR, C_NULL_PTR)
          call CLerror_check('EBSDDISubroutine:clEnqueueWriteBuffer', ierr)
  
          call InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,dinl%devid,kernel,context,command_queue)
  
          dp =  maxval(results)
          if (dp.gt.mvres) mvres = dp
  
  ! this might be simplified later for the remainder of the patterns
          do qq = 1,ppendE(jj)
              jjj = (jj-1)*Ne+qq
              resultarray(1:Nd) = results((qq-1)*Nd+1:qq*Nd)
              indexarray(1:Nd) = indexlist((iii-1)*Nd+1:iii*Nd)
  
              call SSORT(resultarray,indexarray,Nd,-2)
              resulttmp(nnk+1:2*nnk,jjj) = resultarray(1:nnk)
              indextmp(nnk+1:2*nnk,jjj) = indexarray(1:nnk)
  
              call SSORT(resulttmp(:,jjj),indextmp(:,jjj),2*nnk,-2)
  
              resultmain(1:nnk,jjj) = resulttmp(1:nnk,jjj)
              indexmain(1:nnk,jjj) = indextmp(1:nnk,jjj)
          end do
        end do experimentalloop
  
        io_real(1) = mvres
        io_real(2) = float(iii)/float(cratio)*100.0
        call WriteValue('',io_real,2,"(' max. dot product = ',F10.6,';',F6.1,'% complete')")
  
  
        if (mod(iii,10) .eq. 0) then
  ! do a remaining time estimate
  ! and print information
          if (iii.eq.10) then
              tock = Time_tock(tickstart)
              ttime = float(tock) * float(cratio) / float(iii)
              tstop = ttime
              io_int(1:4) = (/iii,cratio, int(ttime/3600.0), int(mod(ttime,3600.0)/60.0)/)
              call WriteValue('',io_int,4,"(' -> Completed cycle ',I5,' out of ',I5,'; est. total time ', &
                             I4,' hrs',I3,' min')")
          else
              ttime = tstop * float(cratio-iii) / float(cratio)
              io_int(1:4) = (/iii,cratio, int(ttime/3600.0), int(mod(ttime,3600.0)/60.0)/)
              call WriteValue('',io_int,4,"(' -> Completed cycle ',I5,' out of ',I5,'; est. remaining time ', &
                             I4,' hrs',I3,' min')")
          end if
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
  
  if (trim(dinl%indexingmode).eq.'dynamic') then
  !$OMP DO SCHEDULE(DYNAMIC)
  
       do pp = 1,ppend(ii)  !Nd or MODULO(FZcnt,Nd)
         binned = 0.0
         quat = ro2qu(FZarray(1:4,(ii-1)*Nd+pp))
  
         call CalcEBSDPatternSingleFull(jpar,quat,accum_e_MC,mLPNH,mLPSH,EBSDdetector%rgx,&
                                        EBSDdetector%rgy,EBSDdetector%rgz,binned,Emin,Emax,mask,prefactor)
  
         if (dinl%scalingmode .eq. 'gam') then
           binned = binned**dinl%gammavalue
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
         EBSDpatternad =  adhisteq(dinl%nregions,binx,biny,EBSDpatterninteger)
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
  
  else  ! we are doing static indexing, so only 2 threads in total
  
  ! get a set of patterns from the precomputed dictionary file... 
  ! we'll use a hyperslab to read a block of preprocessed patterns from file 
  
     if (TID .ne. 0) then
  ! read data from the hyperslab
       dataset = SC_EBSDpatterns
       dims2 = (/ correctsize, ppend(ii) /)
       offset2 = (/ 0, (ii-1)*Nd /)
  
       if(allocated(EBSDdictpatflt)) deallocate(EBSDdictpatflt)
       EBSDdictpatflt = HDF_readHyperslabFloatArray2D(dataset, offset2, dims2, HDF_head)
        
       do pp = 1,ppend(ii)  !Nd or MODULO(FZcnt,Nd)
         dict((pp-1)*correctsize+1:pp*correctsize) = EBSDdictpatflt(1:correctsize,pp)
       end do
     end if   
  
  end if
  
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
  
  if (dinl%keeptmpfile.eq.'n') then
      close(itmpexpt,status='delete')
  else
      close(itmpexpt,status='keep')
  end if
  
  ! release the OpenCL kernel
  ierr = clReleaseKernel(kernel)
  call CLerror_check('InnerProdGPU:clReleaseKernel', ierr)
  
  if (trim(dinl%indexingmode).eq.'static') then
  ! close file and nullify pointer
      call HDF_pop(HDF_head,.TRUE.)
      call h5close_EMsoft(hdferr)
  end if
  
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
  ! (anything other than what is already in the dinl structure)
  ipar = 0
  ipar(1) = nnk
  ipar(2) = Ne*ceiling(float(totnumexpt)/float(Ne))
  ipar(3) = totnumexpt
  ipar(4) = Nd*ceiling(float(FZcnt)/float(Nd))
  ipar(5) = FZcnt
  ipar(6) = pgnum
  if (ROIselected.eqv..TRUE.) then
    ipar(7) = dinl%ROI(3)
    ipar(8) = dinl%ROI(4)
  else
    ipar(7) = dinl%ipf_wd
    ipar(8) = dinl%ipf_ht
  end if 
  
  allocate(OSMmap(jjend, iiiend))
  
  ! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)
  
  if (dinl%datafile.ne.'undefined') then 
    vendor = 'TSL'
    call h5ebsd_writeFile(vendor, dinl, mcnl%xtalname, dstr, tstrb, ipar, resultmain, exptIQ, indexmain, eulerarray, &
                          dpmap, progname, nmldeffile, OSMmap)
    call Message('Data stored in h5ebsd file : '//trim(dinl%datafile))
  end if
  
  if (dinl%ctffile.ne.'undefined') then 
    call ctfebsd_writeFile(dinl,mcnl%xtalname,ipar,indexmain,eulerarray,resultmain, OSMmap, exptIQ)
    call Message('Data stored in ctf file : '//trim(dinl%ctffile))
  end if
  
  if (dinl%angfile.ne.'undefined') then 
      call angebsd_writeFile(dinl,mcnl%xtalname,ipar,indexmain,eulerarray,resultmain,exptIQ)
      call Message('Data stored in ang file : '//trim(dinl%angfile))
  end if
  
  ! close the fortran HDF5 interface
  call h5close_EMsoft(hdferr)
  
  ! if requested, we notify the user that this program has completed its run
  if (trim(EMsoft_getNotify()).ne.'Off') then
    if (trim(dinl%Notify).eq.'On') then 
      NumLines = 3
      allocate(MessageLines(NumLines))
  
      call hostnm(c)
   
      MessageLines(1) = 'EMEBSDDI program has ended successfully'
      MessageLines(2) = 'Indexed data stored in '//trim(dinl%datafile)
      write (exectime,"(F15.0)") tstop  
      MessageLines(3) = 'Total execution time [s]: '//trim(exectime)
      TitleMessage = 'EMsoft on '//trim(c)
      i = PostMessage(MessageLines, NumLines, TitleMessage)
    end if
  end if
  
  
  end subroutine EBSDDISubroutine

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

if (present(verbose)) call Message(' -> completed reading dynamical scattering data from &
'//trim(enl%masterfile), frm = "(A)")

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
integer(kind=irg)                               :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nixp, niyp, elp      ! various parameters
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
  !pcvec = pcvec/NORM2(pcvec)

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
  pcvec = pcvec/NORM2(pcvec)

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

        dc = sngl(quat_Lp(qu(1:4),  (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /) ))

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
logical                                             :: stat, readonly, g_exists
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
if (g_exists.eqv..FALSE.) then
    call FatalError('readEBSDDotProductFile','this is not an EBSD dot product file')
end if
call HDF_pop(HDF_head)

!====================================
! read all NMLparameters group datasets
!====================================
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDIndexingNameListType
    hdferr = HDF_openGroup(groupname, HDF_head)

! we'll read these roughly in the order that the HDFView program displays them...
dataset = SC_HDFstrings
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    do ii=1,10
      ebsdnl%HDFstrings(ii) = trim(stringarray(ii))
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
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDDIdata%FZcnt)

dataset = SC_NumExptPatterns
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDDIdata%Nexp)

dataset = SC_PointGroupNumber
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDDIdata%pgnum)

! various optional arrays
if (present(getADP)) then
  if (getADP.eqv..TRUE.) then
   dataset = SC_AvDotProductMap
   allocate(EBSDDIdata%ADP(ebsdnl%ipf_wd, ebsdnl%ipf_ht))
   call HDF_read2DImage(dataset, EBSDDIdata%ADP, ebsdnl%ipf_wd, ebsdnl%ipf_ht, HDF_head)
  end if 
end if

if (present(getAverageOrientations)) then
  if (getAverageOrientations.eqv..TRUE.) then
    dataset = SC_AverageOrientations
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%AverageOrientations)
  end if 
end if

if (present(getCI)) then
  if (getCI.eqv..TRUE.) then
    dataset = SC_CI
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%CI)
  end if 
end if

if (present(getEulerAngles)) then
  if (getEulerAngles.eqv..TRUE.) then
    dataset = SC_EulerAngles
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%EulerAngles)
  end if 
end if

if (present(getDictionaryEulerAngles)) then
  if (getDictionaryEulerAngles.eqv..TRUE.) then
    dataset = 'DictionaryEulerAngles'
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%DictionaryEulerAngles)
  end if 
end if

if (present(getFit)) then
  if (getFit.eqv..TRUE.) then
    dataset = SC_Fit
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Fit)
  end if 
end if

if (present(getIQ)) then
  if (getIQ.eqv..TRUE.) then
    dataset = SC_IQ
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%IQ)
  end if 
end if

if (present(getKAM)) then
  if (getKAM.eqv..TRUE.) then
    dataset = SC_KAM
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%KAM)
  end if 
end if

if (present(getOSM)) then
  if (getOSM.eqv..TRUE.) then
    dataset = SC_OSM
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%OSM)
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
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phi)
  end if 
end if

if (present(getPhi1)) then
  if (getPhi1.eqv..TRUE.) then
    dataset = SC_Phi1
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phi1)
  end if 
end if

if (present(getPhi2)) then
  if (getPhi2.eqv..TRUE.) then
    dataset = SC_Phi2
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%Phi2)
  end if 
end if

if (present(getSEMsignal)) then
  if (getSEMsignal.eqv..TRUE.) then
    dataset = SC_SEMsignal
    call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%SEMsignal)
  end if 
end if

if (present(getTopDotProductList)) then
  if (getTopDotProductList.eqv..TRUE.) then
    dataset = SC_TopDotProductList
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%TopDotProductList)
  end if 
end if

if (present(getTopMatchIndices)) then
  if (getTopMatchIndices.eqv..TRUE.) then
    dataset = SC_TopMatchIndices
    call HDF_readDatasetIntegerArray2D(dataset, dims2, HDF_head, hdferr, EBSDDIdata%TopMatchIndices)
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
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%XPosition)
  end if 
end if

if (present(getYPosition)) then
  if (getYPosition.eqv..TRUE.) then
    dataset = SC_YPosition
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDDIdata%YPosition)
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
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepX)
    
dataset = SC_StepY
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepY)

! if (present()) then
!   if (get.eqv..TRUE.) then

!   end if 
! end if

! and close the HDF5 dot product file
call HDF_pop(HDF_head,.TRUE.)
nullify(HDF_head%next)
 
end subroutine readEBSDDotProductFile

end module EBSDDImod
