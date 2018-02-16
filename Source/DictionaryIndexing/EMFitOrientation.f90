! ###################################################################
! Copyright (c) 2015-2016, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMFitOrientation.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMFitOrientation
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Refine the orientation in a dot product file by searching orientaions
!> space around that best indexed point. the search is perforems using the DFO
!> BOBYQA algorithm. The program can handle both the EBSD and ECP modality.
!> Future version may include PED and TKD modality as well.
!
!> @date 08/01/16 SS 1.0 original
!--------------------------------------------------------------------------
program EMFitOrientation

use local
use typedefs 
use NameListTypedefs
use NameListHandlers
use initializersHDF
use HDF5
use h5im
use h5lt
use EMh5ebsd
use HDFsupport
use rotations
use so3
use constants
use EBSDmod
use ECPiomod
use EBSDDImod
use omp_lib
use ECPmod
use filters
use timing
use error
use io
use EBSDiomod
use files
use bobyqa_refinement,only:bobyqa
use FitOrientations
use stringconstants
use patternmod

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(RefineOrientationtype)             :: enl
type(EBSDIndexingNameListType)          :: ebsdnl
type(ECPIndexingNameListType)           :: ecpnl

logical                                 :: stat, readonly, noindex, g_exists
character(fnlen)                        :: dpfile, masterfile, energyfile
integer(kind=irg)                       :: hdferr, ii, jj, kk, iii, istat

real(kind=dbl)                          :: misang       ! desired misorientation angle (degrees)
integer(kind=irg)                       :: Nmis         ! desired number of sampling points along cube edge
integer(kind=irg)                       :: CMcnt        ! number of entries in linked list
type(FZpointd),pointer                  :: CMlist, CMtmp       ! pointer to start of linked list and temporary one
real(kind=dbl)                          :: rhozero(4), hipassw

real(kind=sgl),allocatable              :: angles(:), euler_bestmatch(:,:), CIlist(:), CIlist_new(:), CMarray(:,:,:)
integer(kind=irg),allocatable           :: indexmain(:,:) 
real(kind=sgl),allocatable              :: resultmain(:,:)                                         
integer(HSIZE_T)                        :: dims(1) 

integer(kind=irg)                       :: numk, numdictsingle, numexptsingle

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                        :: dataset, groupname  
character(fnlen)                        :: ename, fname    
type(EBSDLargeAccumDIType),pointer      :: acc
type(EBSDMasterDIType),pointer          :: master
type(ECPLargeAccumType),pointer         :: accECP
type(ECPMasterType),pointer             :: masterECP
real(kind=sgl),allocatable              :: mLPNHECP(:,:,:),mLPSHECP(:,:,:),accum_e_detector(:,:,:)
type(IncidentListECP),pointer           :: khead, ktmp

real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second

integer(c_size_t),allocatable           :: IPAR2(:)
real(kind=dbl),allocatable              :: X(:), XL(:), XU(:)
real(kind=sgl),allocatable              :: INITMEANVAL(:)
real(kind=dbl)                          :: RHOBEG, RHOEND
integer(kind=irg)                       :: NPT, N, IPRINT, NSTEP, NINIT
integer(kind=irg),parameter             :: MAXFUN = 10000
logical                                 :: verbose

logical                                 :: f_exists, init, overwrite =.TRUE.
integer(kind=irg),parameter             :: iunitexpt = 41, itmpexpt = 42
integer(kind=irg)                       :: binx, biny, recordsize, patsz, totnumexpt
real(kind=sgl),allocatable              :: tmpimageexpt(:), EBSDPattern(:,:), imageexpt(:), mask(:,:), masklin(:), EBSDpat(:,:)
real(kind=sgl),allocatable              :: imagedictflt(:), exppatarray(:)
real(kind=dbl),allocatable              :: fdata(:,:), rdata(:,:), rrdata(:,:), ffdata(:,:), ksqarray(:,:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:), binned(:,:), euler_best(:,:), ECPattern(:,:),  ECpatternintd(:,:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:)
integer(kind=irg),allocatable           :: ECpatterninteger(:,:), ECpatternad(:,:), EBSDpint(:,:)
complex(kind=dbl),allocatable           :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable   :: inp(:,:), outp(:,:)
real(kind=sgl)                          :: quat(4), ma, mi, dp, tstart, tstop, io_real(1), tmp, totnum_el, genfloat, vlen
integer(kind=irg)                       :: ipar(10), Emin, Emax, nthreads, TID, io_int(2), tick, tock, ierr, L 
integer(kind=irg)                       :: ll, mm, jpar(7), Nexp, pgnum, FZcnt, nlines, dims2(2)
real(kind=dbl)                          :: prefactor, F

real(kind=dbl)                          :: ratioE
integer(kind=irg)                       :: cratioE, fratioE, eindex, niter, i
integer(kind=irg),allocatable           :: ppendE(:)
real(kind=sgl),allocatable              :: exptpatterns(:,:)

type(C_PTR)                             :: planf, HPplanf, HPplanb
real(kind=dbl)                          :: w, Jres
real(kind=sgl),allocatable              :: STEPSIZE(:)
character(fnlen)                        :: modalityname, Manufacturer
character(1)                            :: rchar    
integer(HSIZE_T)                        :: dims3(3), offset3(3)
type(HDFobjectStackType),pointer        :: HDF_head


init = .TRUE.

nmldeffile = 'EMFitOrientation.nml'
progname = 'EMFitOrientation.f90'
progdesc = 'Refine orientation by searching orientation space about a point using BOBYQA DFO'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 91 /), progname)

! deal with the namelist stuff
call GetRefineOrientationNameList(nmldeffile,enl)

nthreads = enl%nthreads

!====================================
! read the relevant fields from the dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

dpfile = trim(EMsoft_getEMdatapathname())//trim(enl%dotproductfile)
dpfile = EMsoft_toNativePath(dpfile)

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(dpfile), stat, hdferr)

!===================================================================================
!===============read dot product file===============================================
!===================================================================================

if (stat) then 
! open the dot product file 
    nullify(HDF_head)
    readonly = .TRUE.
    hdferr =  HDF_openFile(dpfile, HDF_head, readonly)

dataset = SC_Manufacturer
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    Manufacturer = trim(stringarray(1))

    if(trim(Manufacturer) .eq. 'EMEBSDDictionaryIndexing.f90') then
        modalityname = 'EBSD'
    else if(trim(Manufacturer) .eq. 'EMECPDictionaryIndexing.f90') then
        modalityname = 'ECP'
    else
        call FatalError('EMFitOrientation','Manufacturer unknown or not yet implemented')
    end if
    deallocate(stringarray)

    if (trim(modalityname) .eq. 'EBSD') then
! get the energyfile and masterfile parameters from NMLParameters
groupname = SC_NMLparameters
        hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDIndexingNameListType
        hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_energyfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ebsdnl%energyfile = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_masterfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ebsdnl%masterfile = trim(stringarray(1))
        deallocate(stringarray)
        
! here we read the datasets that are associated with the raw data input file type...
dataset = SC_inputtype
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ebsdnl%inputtype = trim(stringarray(1))
        deallocate(stringarray)  

dataset = SC_HDFstrings
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        do i=1,nlines 
            ebsdnl%HDFstrings(i) = trim(stringarray(i))
        end do
        deallocate(stringarray)  
!----

dataset = SC_ipfwd
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ipf_wd)

dataset = SC_ipfht
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ipf_ht)

dataset = SC_StepX
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepX)
    
dataset = SC_StepY
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepY)

dataset = SC_L
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%L)

dataset = SC_beamcurrent
        call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%beamcurrent)

dataset = SC_binning
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%binning)

dataset = SC_delta
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%delta)

dataset = SC_dwelltime
        call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%dwelltime)

dataset = SC_energyaverage
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%energyaverage)

dataset = SC_energymax
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%energymax)

dataset = SC_energymin
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%energymin)

dataset = SC_exptfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ebsdnl%exptfile = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_gammavalue
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%gammavalue)

dataset = SC_hipassw
        call HDF_readDatasetDouble(dataset, HDF_head, hdferr, hipassw)
w       = hipassw

dataset = SC_maskpattern
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ebsdnl%maskpattern = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_maskradius
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%maskradius)

dataset = SC_ncubochoric  
! There is an issue with the capitalization on this variable; needs to be resolved 
! [MDG 10/18/17]  Most likely we will need to put a test in to see if Ncubochoric exists; if it does not then we check
! for ncubochoric ...
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ncubochoric)
else
    dataset = 'ncubochoric'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ncubochoric)
end if

dataset = SC_nnk
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nnk)

dataset = SC_nregions
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%nregions)

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

! open the Scan 1/EBSD/Data group
        groupname = 'Scan 1'
        hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSD
        hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
        hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_FZcnt
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)

dataset = SC_NumExptPatterns
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, Nexp)

        allocate(euler_bestmatch(3,Nexp),CIlist_new(Nexp),stat=istat)
        if (istat .ne. 0) then
            dpfile = 'Failed to allocate CIlist_new and/or euler_bestmatch array'
            call FatalError('EMAverageOrient',dpfile)
        end if 
        euler_bestmatch = 0.0
        CIlist_new = 0.0

! arrays
dataset = SC_Phi1
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, angles)
    
        euler_bestmatch(1,1:Nexp) = angles(1:Nexp)
        deallocate(angles)

dataset = SC_Phi
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, angles)
    
        euler_bestmatch(2,1:Nexp) = angles(1:Nexp)
        deallocate(angles)

dataset = SC_Phi2
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, angles)
    
        euler_bestmatch(3,1:Nexp) = angles(1:Nexp)
        deallocate(angles)

dataset = SC_CI
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, CIlist)
    
        CIlist_new(1:Nexp) = CIlist(1:Nexp)
   
        call HDF_pop(HDF_head,.TRUE.)

        call Message('dot product EBSD HDF5 file read')

    else if (trim(modalityname) .eq. 'ECP') then
! fill ecpnl namelist structure here
groupname = SC_NMLparameters
        hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_ECPIndexingNameListType
        hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_energyfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ecpnl%energyfile = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_exptfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ecpnl%exptfile = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_masterfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ecpnl%masterfile = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_devid
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%devid)

dataset = SC_Rin
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ecpnl%Rin)

dataset = SC_Rout
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ecpnl%Rout)

dataset = SC_gammavalue
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ecpnl%gammavalue)

dataset = SC_maskpattern
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ecpnl%maskpattern = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_maskradius
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%maskradius)

dataset = SC_ncubochoric
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%ncubochoric)

dataset = SC_nnk
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%nnk)

dataset = SC_npix
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%npix)

dataset = SC_nregions
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%nregions)

dataset = SC_nthreads
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%nthreads)

dataset = SC_numdictsingle
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%numdictsingle)

dataset = SC_numexptsingle
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%numexptsingle)

dataset = SC_platid
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%platid)

dataset = SC_sampletilt
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, genfloat)
        ecpnl%sampletilt = dble(genfloat)

dataset = SC_thetac
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ecpnl%thetac)

dataset = SC_tmpfile
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ecpnl%tmpfile = trim(stringarray(1))
        deallocate(stringarray)

dataset = SC_totnumexpt
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ecpnl%totnumexpt)

dataset = SC_workingdistance
        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ecpnl%workingdistance)

dataset = SC_xtalname
        call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
        ecpnl%MCxtalname = trim(stringarray(1))
        deallocate(stringarray)

! and close the NMLparameters group
        call HDF_pop(HDF_head)
        call HDF_pop(HDF_head)

! open the Scan 1/EBSD/Data group
        groupname = 'Scan 1'
        hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_ECP
        hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
        hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_FZcnt
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)

dataset = SC_NumExptPatterns
        call HDF_readDatasetInteger(dataset, HDF_head, hdferr, Nexp)

        allocate(euler_bestmatch(3,Nexp),CIlist_new(Nexp),stat=istat)
        if (istat .ne. 0) then
            dpfile = 'Failed to allocate CIlist_new and/or euler_bestmatch array'
            call FatalError('EMAverageOrient',dpfile)
        end if 
        euler_bestmatch = 0.0
        CIlist_new = 0.0

! arrays
dataset = SC_Phi1
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, angles)
    
        euler_bestmatch(1,1:Nexp) = angles(1:Nexp)
        deallocate(angles)

dataset = SC_Phi
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, angles)
    
        euler_bestmatch(2,1:Nexp) = angles(1:Nexp)
        deallocate(angles)

dataset = SC_Phi2
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, angles)
    
        euler_bestmatch(3,1:Nexp) = angles(1:Nexp)
        deallocate(angles)

dataset = SC_CI
        call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, CIlist)
    
        CIlist_new(1:Nexp) = CIlist(1:Nexp)
   
        call HDF_pop(HDF_head,.TRUE.)

        call Message('dot product ECP HDF5 file read')

    end if
else
    dpfile = 'File '//trim(dpfile)//' is not an HDF5 file'
    call FatalError('EMFitOrientation:',dpfile)
end if



!===================================================================================
! ELECTRON BACKSCATTER DIFFRACTION PATTERNS
!===================================================================================
if (trim(modalityname) .eq. 'EBSD') then

! 1. read the Monte Carlo data file (including HDF format)
    allocate(acc)
    call EBSDIndexingreadMCfile(ebsdnl, acc, verbose=.TRUE.,NoHDFInterfaceOpen=.FALSE.)

! 2. read EBSD master pattern file (including HDF format)
    allocate(master)
    call EBSDIndexingreadMasterfile(ebsdnl, master, verbose=.TRUE.,NoHDFInterfaceOpen=.FALSE.)

! 3. generate detector arrays
    allocate(master%rgx(ebsdnl%numsx,ebsdnl%numsy), master%rgy(ebsdnl%numsx,ebsdnl%numsy), &
             master%rgz(ebsdnl%numsx,ebsdnl%numsy), stat=istat)
    allocate(acc%accum_e_detector(ebsdnl%numEbins,ebsdnl%numsx,ebsdnl%numsy), stat=istat)

    call EBSDIndexingGenerateDetector(ebsdnl, acc, master)
    deallocate(acc%accum_e)

!=====================================================
! get the indices of the minimum and maximum energy
!=====================================================
    Emin = nint((ebsdnl%energymin - ebsdnl%Ehistmin)/ebsdnl%Ebinsize) +1
    if (Emin.lt.1)  Emin=1
    if (Emin.gt.ebsdnl%numEbins)  Emin=ebsdnl%numEbins

    Emax = nint((ebsdnl%energymax - ebsdnl%Ehistmin)/ebsdnl%Ebinsize) + 1
    if (Emax .lt. 1) Emax = 1
    if (Emax .gt. ebsdnl%numEbins) Emax = ebsdnl%numEbins

!=====================================================
!==========fill important parameters in namelist======
!=====================================================

    binx          = ebsdnl%numsx/ebsdnl%binning
    biny          = ebsdnl%numsy/ebsdnl%binning
    recordsize    = binx*biny*4
    L             = binx*biny
    numdictsingle = ebsdnl%numdictsingle
    numexptsingle = ebsdnl%numexptsingle
    patsz         = L 
    allocate(IPAR2(10))
    IPAR2 = 0

! define the jpar array
    jpar(1) = ebsdnl%binning
    jpar(2) = ebsdnl%numsx
    jpar(3) = ebsdnl%numsy
    jpar(4) = ebsdnl%npx
    jpar(5) = ebsdnl%npy
    jpar(6) = ebsdnl%numEbins
    jpar(7) = ebsdnl%nE

    IPAR2(1:7) = jpar(1:7)
    IPAR2(8) = Emin
    IPAR2(9) = Emax
    IPAR2(10)= ebsdnl%nregions

    dims2 = (/binx, biny/)

! get the total number of electrons on the detector
    totnum_el = sum(acc%accum_e_detector)

! intensity prefactor
    prefactor = 0.25D0 * nAmpere * ebsdnl%beamcurrent * ebsdnl%dwelltime * 1.0D-15/ totnum_el

!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
    pgnum = GetPointGroup(ebsdnl%MCxtalname,.FALSE.)

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

    allocate(rdata(binx,biny),fdata(binx,biny))
    rdata = 0.D0
    fdata = 0.D0

    allocate(EBSDpatternintd(binx,biny),EBSDpatterninteger(binx,biny),EBSDpatternad(binx,biny))
    EBSDpatternintd = 0.0
    EBSDpatterninteger = 0
    EBSDpatternad = 0.0

    allocate(imagedictflt(binx*biny),euler_best(3,Nexp))
    imagedictflt = 0.0

    euler_best = 0.0
    euler_best(1:3,1:Nexp) = euler_bestmatch(1:3,1:Nexp)*180.0/cPi


!===============================================================
! define the circular mask if necessary and convert to 1D vector
!===============================================================
    if (ebsdnl%maskpattern.eq.'y') then
      do ii = 1,biny
          do jj = 1,binx
              if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. ebsdnl%maskradius**2) then
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

!=================================
!========LOOP VARIABLES===========
!=================================

    ratioE = float(Nexp)/float(ebsdnl%numexptsingle)
    cratioE = ceiling(ratioE)
    fratioE = floor(ratioE)

    ppendE = (/ (ebsdnl%numexptsingle, ii=1,cratioE) /)
    if (fratioE.lt.cratioE) then
      ppendE(cratioE) = MODULO(Nexp,ebsdnl%numexptsingle)
    end if

    dims3 = (/ binx, biny, ebsdnl%ipf_wd /)


call h5close_EMsoft(hdferr)

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
    inquire(file=trim(trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)), exist=f_exists)

    call WriteValue('Creating temporary file :',trim(fname))

    if (f_exists) then
      open(unit=itmpexpt,file=trim(fname),&
          status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
      close(unit=itmpexpt,status='delete')
    end if

! open the temporary file
    open(unit=itmpexpt,file=trim(fname),&
         status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

    istat = openExpPatternFile(ebsdnl%exptfile, ebsdnl%ipf_wd, ebsdnl%inputtype, recordsize, iunitexpt, ebsdnl%HDFstrings)
    if (istat.ne.0) then
        call patternmod_errormessage(istat)
        call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
    end if

    ! ename = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%exptfile)
    ! ename = EMsoft_toNativePath(ename)

    ! open(unit=iunitexpt,file=trim(ename),&
    !     status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

    allocate(exptpatterns(binx*biny,ebsdnl%numexptsingle),stat=istat)
    if(istat .ne. 0) then
        call FatalError('EMFitOrientation:','could not allocate exptpatterns array')
    end if
    exptpatterns = 0.0

! this next part is done with OpenMP, with only thread 0 doing the reading and writing,
! Thread 0 reads one line worth of patterns from the input file, then all threads do 
! the work, and thread 0 writes to the output file; repeat until all patterns have been processed.

    call OMP_SET_NUM_THREADS(enl%nthreads)
    io_int(1) = enl%nthreads
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
! convert imageexpt to 2D EBS Pattern array
        do kk=1,biny
          EBSDPat(1:binx,kk) = exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx)
        end do

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
! ELECTRON CHANNELING PATTERNS
!=====================================================
else if (trim(modalityname) .eq. 'ECP') then

! Prepare everything for the ECP modality
 
! 1. read the Monte Carlo data file
    allocate(accECP)
    call ECPIndexingreadMCfile(ecpnl, accECP, verbose=.TRUE., NoHDFInterfaceOpen=.FALSE.)

! 2. read EBSD master pattern file
    allocate(masterECP)
    call ECPIndexingreadMasterfile(ecpnl, masterECP, verbose=.TRUE., NoHDFInterfaceOpen=.FALSE.)
    
    allocate(accum_e_detector(1,ecpnl%npix,ecpnl%npix),mLPNHECP(-ecpnl%npx:ecpnl%npx,-ecpnl%npy:ecpnl%npy,1),&
             mLPSHECP(-ecpnl%npx:ecpnl%npx,-ecpnl%npy:ecpnl%npy,1))
    mLPNHECP(:,:,1) = masterECP%mLPNH(:,:)
    mLPSHECP(:,:,1) = masterECP%mLPSH(:,:)
    accum_e_detector = 1.0

!   deallocate(accECP%accum_e,acc%accum_z,masterECP%mLPNH,masterECP%mLPSH)

    allocate(masterECP%rgx(ecpnl%npix,ecpnl%npix), masterECP%rgy(ecpnl%npix,ecpnl%npix),&
             masterECP%rgz(ecpnl%npix,ecpnl%npix))

    masterECP%rgx = 0.0
    masterECP%rgy = 0.0
    masterECP%rgz = 0.0

! 4. generate list of incident vectors and put them in array for OpenMP
    numk = 0
    call GetVectorsConeIndexing(ecpnl, khead, numk)

    ktmp => khead
! converting to array for OpenMP parallelization
    do ii = 1,numk
       masterECP%rgx(ktmp%i,ktmp%j) = ktmp%k(1)
       masterECP%rgy(ktmp%i,ktmp%j) = ktmp%k(2)
       masterECP%rgz(ktmp%i,ktmp%j) = ktmp%k(3)
       ktmp => ktmp%next
    end do

!=====================================================
!==========fill important parameters in namelist======
!=====================================================

    binx = ecpnl%npix
    biny = ecpnl%npix
    recordsize = binx*biny*4
    L = binx*biny
    numdictsingle = ecpnl%numdictsingle
    numexptsingle = ecpnl%numexptsingle
    
    allocate(IPAR2(10))
    IPAR2 = 0

! define the jpar array
    jpar(1) = 1
    jpar(2) = ecpnl%npix
    jpar(3) = ecpnl%npix
    jpar(4) = ecpnl%npx
    jpar(5) = ecpnl%npy
    jpar(6) = 1
    jpar(7) = 1

    IPAR2(1:7) = jpar(1:7)
    IPAR2(8) = ecpnl%nregions

    IPAR2(9:10) = 0

    dims2 = (/binx, biny/)

    prefactor = 1.0
!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
    pgnum = GetPointGroup(ecpnl%MCxtalname,.FALSE.)

!=====================================================
!==========ALLOCATE ALL ARRAYS HERE=================== 
!=====================================================
    allocate(mask(binx,biny),masklin(binx*biny))
    mask = 1.0
    masklin = 0.0

    allocate(ECPattern(binx,biny),tmpimageexpt(binx*biny),imageexpt(binx*biny),binned(binx,biny))
    ECPattern = 0.0
    tmpimageexpt = 0.0
    imageexpt = 0.0
    binned = 0.0

    allocate(rdata(binx,biny),fdata(binx,biny))
    rdata = 0.D0
    fdata = 0.D0

    allocate(ECpatternintd(binx,biny),ECpatterninteger(binx,biny),ECpatternad(binx,biny))
    ECpatternintd = 0.0
    ECpatterninteger = 0
    ECpatternad = 0.0

    allocate(imagedictflt(binx*biny),euler_best(3,Nexp))
    imagedictflt = 0.0

    euler_best = 0.0
    euler_best(1:3,1:Nexp) = euler_bestmatch(1:3,1:Nexp)*180.0/cPi

!===============================================================
! define the circular mask if necessary and convert to 1D vector
!===============================================================
    if (ecpnl%maskpattern.eq.'y') then
      do ii = 1,biny
          do jj = 1,binx
              if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. ecpnl%maskradius**2) then
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

! first, make sure that this file does not already exist
    f_exists = .FALSE.
    fname = trim(EMsoft_getEMtmppathname())//trim(ecpnl%tmpfile)
    fname = EMsoft_toNativePath(fname)
    inquire(file=trim(trim(EMsoft_getEMtmppathname())//trim(ecpnl%tmpfile)), exist=f_exists)

    call WriteValue('Creating temporary file :',trim(fname))

    if (f_exists) then
      open(unit=itmpexpt,file=trim(fname),&
          status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
      close(unit=itmpexpt,status='delete')
    end if

! open the temporary file
    open(unit=itmpexpt,file=trim(fname),&
         status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

    write (*,*) 'input file ',trim(EMsoft_getEMdatapathname())//trim(ecpnl%exptfile)

    ename = trim(EMsoft_getEMdatapathname())//trim(ecpnl%exptfile)
    ename = EMsoft_toNativePath(ename)
    open(unit=iunitexpt,file=trim(ename),&
        status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

!=================================
!========LOOP VARIABLES===========
!=================================

    ratioE = float(Nexp)/float(ecpnl%numexptsingle)
    cratioE = ceiling(ratioE)
    fratioE = floor(ratioE)

    ppendE = (/ (ecpnl%numexptsingle, ii=1,cratioE) /)
    if (fratioE.lt.cratioE) then
      ppendE(cratioE) = MODULO(Nexp,ecpnl%numexptsingle)
    end if

    allocate(exptpatterns(binx*biny,ecpnl%numexptsingle),stat=istat)
    if(istat .ne. 0) then
        call FatalError('EMFitOrientation:','could not allocate exptpatterns array')
    end if
    exptpatterns = 0.0

    prepexperimentalloopECP: do iii = 1,Nexp

        tmpimageexpt = 0.0
        read(iunitexpt,rec=iii) imageexpt

! convert imageexpt to 2D EC Pattern array
        do kk=1,biny
          ECPattern(1:binx,kk) = imageexpt((kk-1)*binx+1:kk*binx)
        end do
     
! adaptive histogram equalization
        ma = maxval(ECPattern)
        mi = minval(ECPattern)
    
        ECpatternintd = ((ECPattern - mi)/ (ma-mi))
        ECpatterninteger = nint(ECpatternintd*255.0)
        ECpatternad =  adhisteq(ecpnl%nregions,binx,biny,ECpatterninteger)
        ECPattern = float(ECpatternad)

! convert back to 1D vector
        do kk=1,biny
          imageexpt((kk-1)*binx+1:kk*binx) = ECPattern(1:binx,kk)
        end do

! normalize and apply circular mask
        imageexpt(1:L) = imageexpt(1:L) * masklin(1:L)
        imageexpt(1:L) = imageexpt(1:L)/NORM2(imageexpt(1:L))
        tmpimageexpt(1:L) = imageexpt(1:L)

! and write this pattern into the temporary file
        write(itmpexpt,rec=iii) tmpimageexpt

        if (mod(iii,1000) .eq. 0) then
            io_int(1) = iii
            call Writevalue('completed pre-processing pattern #',io_int,1,'(I8)')
        end if 

    end do prepexperimentalloopECP

    call Message('experimental patterns stored in tmp file','(A)')

end if

!=====================================================
! INITIALIZATION DONE...START OPTIMIZATION ROUTINES 
!=====================================================

call Message('All initialization complete. Starting optimization routines','(A)')


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
STEPSIZE = enl%step
verbose = .FALSE.

!===================================================================================
!===============MAIN COMPUTATION LOOP===============================================
!===================================================================================
open(unit=itmpexpt,file=trim(fname),&
     status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)


io_int(1) = nthreads
call WriteValue(' Attempting to set number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(nthreads)

call CPU_TIME(tstart)
call Time_tick(tick)

do iii = 1,cratioE
    
    exptpatterns = 0.0
 
    do jj = 1,ppendE(iii)
        eindex = (iii - 1)*numexptsingle + jj
        read(itmpexpt,rec=eindex) tmpimageexpt
        exptpatterns(1:binx*biny,jj) = tmpimageexpt(1:binx*biny)
    end do

    if (iii .eq. 1) then
        io_int = numexptsingle
        call WriteValue('number of experimental patterns refined in one go = ',io_int,1,'(I8)')
    end if

    call Message('finished reading chunk of experimental data...refining now')
    
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ii,tmpimageexpt,jj,quat,binned,ma,mi,eindex) &
!$OMP& PRIVATE(EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt,ll,mm) &
!$OMP& PRIVATE(X,F,INITMEANVAL)
!$OMP DO SCHEDULE(DYNAMIC)

    do ii = 1,ppendE(iii)

        eindex = (iii - 1)*numexptsingle + ii
        tmpimageexpt = exptpatterns(:,ii)
        tmpimageexpt = tmpimageexpt/NORM2(tmpimageexpt)

! calculate the dot product for each of the orientations in the neighborhood of the best match    

        quat(1:4) = eu2qu(euler_best(1:3,eindex)*cPi/180.0) 
        INITMEANVAL(1:3) = euler_best(1:3,eindex)

        X = 0.5D0
        if(trim(modalityname) .eq. 'EBSD') then
            call bobyqa (IPAR2, INITMEANVAL, tmpimageexpt, N, NPT, X, XL,&
                     XU, RHOBEG, RHOEND, IPRINT, MAXFUN, EMFitOrientationcalfunEBSD, acc%accum_e_detector,&
                     master%mLPNH, master%mLPSH, mask, prefactor, master%rgx, master%rgy, master%rgz, &
                     STEPSIZE, ebsdnl%gammavalue, verbose) 
       
            if (mod(ii,100) .eq. 0) then
                io_int(1) = eindex
                call Writevalue('completed refining pattern #',io_int,1,'(I8,I8)')
            end if
 
        else if(trim(modalityname) .eq. 'ECP') then
            call bobyqa (IPAR2, INITMEANVAL, tmpimageexpt, N, NPT, X, XL,&
                     XU, RHOBEG, RHOEND, IPRINT, MAXFUN, EMFitOrientationcalfunECP, accum_e_detector,&
                     mLPNHECP, mLPSHECP, mask, prefactor, masterECP%rgx, masterECP%rgy, masterECP%rgz, &
                     STEPSIZE, ecpnl%gammavalue, verbose) 
       
            if (mod(ii,25) .eq. 0) then
                io_int(1) = eindex
                call Writevalue('completed refining pattern #',io_int,1,'(I8,I8)')
            end if
        end if
        
        euler_best(1:3,eindex) = (/X(1)*2.0*STEPSIZE(1) - STEPSIZE(1) + INITMEANVAL(1), &
        X(2)*2.0*STEPSIZE(2) - STEPSIZE(2)  + INITMEANVAL(2), &
        X(3)*2.0*STEPSIZE(3) - STEPSIZE(3) + INITMEANVAL(3)/)
       
       if(trim(modalityname) .eq. 'EBSD') then
           call EMFitOrientationcalfunEBSD(IPAR2, INITMEANVAL, tmpimageexpt, acc%accum_e_detector, &
                                master%mLPNH, master%mLPSH, N, X, F, mask, prefactor, &
                                master%rgx, master%rgy, master%rgz, STEPSIZE, ebsdnl%gammavalue, verbose)
       else if(trim(modalityname) .eq. 'ECP') then
           call EMFitOrientationcalfunECP(IPAR2, INITMEANVAL, tmpimageexpt, accum_e_detector, &
                                mLPNHECP, mLPSHECP, N, X, F, mask, prefactor, &
                                masterECP%rgx, masterECP%rgy, masterECP%rgz, STEPSIZE, ecpnl%gammavalue, verbose)
       end if

       CIlist(eindex) = 1.D0 - F

    end do
!$OMP END DO
!$OMP END PARALLEL
 
end do

! add fitted dot product values to HDF5 file
! open the fortran HDF interface
call h5open_EMsoft(hdferr)
nullify(HDF_head)
hdferr =  HDF_openFile(dpfile, HDF_head)

! open the Scan 1/EBSD/Data group
groupname = 'Scan 1'
hdferr = HDF_openGroup(groupname, HDF_head)
if(trim(modalityname) .eq. 'EBSD') then
groupname = SC_EBSD
else if(trim(modalityname) .eq. 'ECP') then
groupname = SC_ECP
end if
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_RefinedDotProducts
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray1D(dataset, CIlist, Nexp, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray1D(dataset, CIlist, Nexp, HDF_head)
end if
 
dataset = SC_RefinedEulerAngles
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(euler_best*cPi/180.0), 3, Nexp, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(euler_best*cPi/180.0), 3, Nexp, HDF_head)
end if
 
call HDF_pop(HDF_head,.TRUE.) 

if(modalityname .eq. 'EBSD') then
! generate new ctf file
    ebsdnl%ctffile = enl%ctffile

    ipar = 0
    ipar(1) = 1
    ipar(2) = Nexp
    ipar(3) = Nexp
    ipar(4) = FZcnt 
    ipar(5) = FZcnt
    ipar(6) = pgnum

    allocate(indexmain(ipar(1),1:ipar(2)),resultmain(ipar(1),1:ipar(2)))
    indexmain = 0
    resultmain(1,1:ipar(2)) = CIlist(1:Nexp)

    if (ebsdnl%ctffile.ne.'undefined') then 
      call ctfebsd_writeFile(ebsdnl,ipar,indexmain,euler_best,resultmain,noindex=.TRUE.)
      call Message('Data stored in ctf file : '//trim(enl%ctffile))
    end if

else if(modalityname .eq. 'ECP') then

    ecpnl%ctffile = enl%ctffile

    ipar = 0
    ipar(1) = 1
    ipar(2) = Nexp
    ipar(3) = Nexp
    ipar(4) = FZcnt 
    ipar(5) = FZcnt
    ipar(6) = pgnum

    allocate(indexmain(ipar(1),1:ipar(2)),resultmain(ipar(1),1:ipar(2)))
    indexmain = 0
    resultmain(1,1:ipar(2)) = CIlist(1:Nexp)

    if (ecpnl%ctffile.ne.'undefined') then 
      call ctfecp_writeFile(ecpnl,ipar,indexmain,euler_best,resultmain,noindex=.TRUE.)
      call Message('Data stored in ctf file : '//trim(enl%ctffile))
    end if

end if

call CPU_TIME(tstop) 
tock = Time_tock(tick)
tstop = tstop - tstart

io_real(1) = tstop
call WriteValue('Execution time [CPU_TIME()] = ',io_real, 1)

io_int(1) = tock
call WriteValue('Execution time [system_clock()] = ',io_int,1,"(I8,' [s]')")

! close the fortran HDF interface
call h5close_EMsoft(hdferr)

end program EMFitOrientation


