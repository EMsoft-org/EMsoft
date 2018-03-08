! ###################################################################
! Copyright (c) 2015-2018, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMgetCTF.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgetCTF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Extract a .ctf file from a dot product HDF5 file; uses the same template as EMFitOrientation
!
!> @date 03/07/18 MDG 1.0 original
!--------------------------------------------------------------------------
program EMgetCTF

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
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), ROI(:)
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
real(kind=sgl),allocatable              :: STEPSIZE(:), OSMmap(:,:), IQmap(:)
character(fnlen)                        :: modalityname, Manufacturer
character(1)                            :: rchar    
integer(HSIZE_T)                        :: dims3(3), offset3(3), dims1D(1), dims2D(2)
type(HDFobjectStackType),pointer        :: HDF_head


init = .TRUE.

nmldeffile = 'EMFitOrientation.nml'
progname = 'EMgetCTF.f90'
progdesc = 'Extract a .ctf file from a dot product HDF5 file'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 91 /), progname)

! deal with the namelist stuff
call GetRefineOrientationNameList(nmldeffile,enl)

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

dataset = SC_ROI
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then
    call HDF_readDatasetIntegerArray1D(dataset, dims1D, HDF_head, hdferr, ROI)
    ebsdnl%ROI = ROI
else
    ebsdnl%ROI(1:4) = (/ 0, 0, 0, 0 /)
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
dataset = SC_OSM
    call HDF_readDatasetFloatArray2D(dataset, dims2D, HDF_head, hdferr, OSMmap)

dataset = SC_IQ
    call HDF_readDatasetFloatArray1D(dataset, dims1D, HDF_head, hdferr, IQmap)

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

        call Message(' -> completed reading dot product file')

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


allocate(euler_best(3,Nexp))
euler_best = 0.0
euler_best(1:3,1:Nexp) = euler_bestmatch(1:3,1:Nexp)*180.0/cPi

! read the Monte Carlo data file to get the xtal file name
    allocate(acc)
    call EBSDIndexingreadMCfile(ebsdnl, acc, verbose=.TRUE.,NoHDFInterfaceOpen=.FALSE.)
    deallocate(acc)
    pgnum = GetPointGroup(ebsdnl%MCxtalname,.FALSE.)

! and prepare the .ctf output file 
    ebsdnl%ctffile = enl%ctffile

    ipar = 0
    ipar(1) = 1
    ipar(2) = Nexp
    ipar(3) = Nexp
    ipar(4) = FZcnt 
    ipar(5) = FZcnt
    ipar(6) = pgnum
    ipar(7) = ebsdnl%ipf_wd
    ipar(8) = ebsdnl%ipf_ht

    allocate(indexmain(ipar(1),1:ipar(2)),resultmain(ipar(1),1:ipar(2)))
    indexmain = 0
    resultmain(1,1:ipar(2)) = CIlist(1:Nexp)

    if (ebsdnl%ctffile.ne.'undefined') then 
      call ctfebsd_writeFile(ebsdnl,ipar,indexmain,euler_best,resultmain,OSMmap,IQmap,noindex=.TRUE.)
      call Message('Data stored in ctf file : '//trim(enl%ctffile))
    end if

! close the fortran HDF interface
call h5close_EMsoft(hdferr)

end program EMgetCTF


