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
! EMsoft:EMRefineOrientation.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMRefineOrientation
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Refine the orientation in a dot product file by searching orientaions
!> space around that best indexed point
!
!> @date 08/01/16 SS 1.0 original
!--------------------------------------------------------------------------
program EMRefineOrientation

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
use EBSDDImod
use omp_lib
use ECPmod, only: GetPointGroup
use filters
use timing
use error
use io
use EBSDiomod
use files
use ISO_C_BINDING
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(RefineOrientationtype)             :: enl
type(EBSDIndexingNameListType)          :: ebsdnl

logical                                 :: stat, readonly, noindex
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
integer(HSIZE_T)                        :: dims(1), dims2D(2)

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                        :: dataset, groupname  
character(fnlen)                        :: ename, fname    
type(EBSDLargeAccumDIType),pointer        :: acc
type(EBSDMasterDIType),pointer            :: master
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second


logical                                 :: f_exists, init, g_exists
integer(kind=irg),parameter             :: iunitexpt = 41, itmpexpt = 42
integer(kind=irg)                       :: binx, biny, recordsize
real(kind=sgl),allocatable              :: tmpimageexpt(:), EBSDPattern(:,:), imageexpt(:), mask(:,:), masklin(:)
real(kind=sgl),allocatable              :: imagedictflt(:)
real(kind=dbl),allocatable              :: fdata(:,:), rdata(:,:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:), binned(:,:), euler_best(:,:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:)
real(kind=sgl)                          :: quat(4), ma, mi, dp, tstart, tstop, io_real(1), tmp, totnum_el
integer(kind=irg)                       :: ipar(10), Emin, Emax, nthreads, TID, io_int(2), tick, tock, ierr, L
integer(kind=irg)                       :: ll, mm, jpar(7), Nexp, pgnum, FZcnt, nlines, dims2(2)
real(kind=dbl)                          :: prefactor

real(kind=dbl)                          :: ratioE
integer(kind=irg)                       :: cratioE, fratioE, eindex, niter
integer(kind=irg),allocatable           :: ppendE(:)
real(kind=sgl),allocatable              :: exptpatterns(:,:), OSMmap(:,:), IQmap(:)

real(kind=dbl)                          :: stepsize, cu0(3)
real(kind=dbl),allocatable              :: cubneighbor(:,:)

type(HDFobjectStackType),pointer        :: HDF_head


init = .TRUE.

nmldeffile = 'EMRefineOrientation.nml'
progname = 'EMRefineOrientation.f90'
progdesc = 'Refine orientation by searching orientation space about a point'

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

    !dataset = 'eulerconvention'
    !call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    !ebsdnl%eulerconvention = trim(stringarray(1))
    !deallocate(stringarray)

dataset = SC_exptfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%exptfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_gammavalue
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%gammavalue)

dataset = SC_hipassw
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, hipassw)

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
    !ebsdnl%tmpfile = 'EBSDDict_tmp.data'
    
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
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, IQmap)

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

    call Message('dot product HDF5 file read')

else
    dpfile = 'File '//trim(dpfile)//' is not an HDF5 file'
    call FatalError('EMRefineOrient',dpfile)
end if

!===================================================================================
!===============READ MASTER AND MC FILE=============================================
!===================================================================================
!
! 1. read the Monte Carlo data file (including HDF format)
allocate(acc)
call EBSDIndexingreadMCfile(ebsdnl, acc, verbose=.TRUE.)

! 2. read EBSD master pattern file (including HDF format)
allocate(master)
call EBSDIndexingreadMasterfile(ebsdnl, master, verbose=.TRUE.)

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



!########################################################################################
!########################################################################################
! TESTING MISORIENTATION SAMPLING v/s NEIGHBORHOOD SAMPLING
!########################################################################################
!########################################################################################

Nmis = enl%nmis 
niter = enl%niter

!=====================================================
! sample the misorientation space around origin 
!=====================================================
!misang = (0.03732D0 + 131.97049D0/dble(ebsdnl%ncubochoric))/2.D0

!nullify(CMlist)
!CMcnt = 0
!call sample_isoCubeFilled(misang, Nmis, CMcnt, CMlist)

!io_real(1) = misang
!call WriteValue('Maximum average Misorientation angle = ',io_real,1,'(F15.6)')

!io_int(1) = CMcnt
!call WriteValue('Number of points sampled in misorientation space = ',io_int,1,'(I8)')

!allocate(CMarray(4,CMcnt,Nexp),stat=istat)

!if (istat .ne. 0) then
!    dpfile = 'Failed to allocate CMarray'
!    call FatalError('EMRefineOrient',dpfile)
!end if 

!do ii = 1,Nexp

!    rhozero = eu2ro(euler_bestmatch(1:3,ii))
!    call SampleIsoMisorientation(rhozero, misang, CMcnt, CMlist)

!    CMtmp => CMlist    
!    do jj = 1,CMcnt
!        CMarray(1:4,jj,ii) = ro2qu(CMtmp%trod)
!        CMtmp => CMtmp%next
!    end do

!end do

!=====================================================
! sample the CUBOCHORIC space aroundpoint of interest
!=====================================================

allocate(cubneighbor(1:3,(2*Nmis + 1)**3),stat=istat)

if(istat .ne. 0) then
    call FatalError('EMRefineOrient','Failed to allocate cubneighbor array')
end if

! THIS PART IS DONE INSIDE MAIN LOOP

!########################################################################################
!########################################################################################
! END TESTING MISORIENTATION SAMPLING v/s NEIGHBORHOOD SAMPLING
!########################################################################################
!########################################################################################




!=====================================================
!==========fill important parameters in namelist======
!=====================================================

binx = ebsdnl%numsx/ebsdnl%binning
biny = ebsdnl%numsy/ebsdnl%binning
recordsize = binx*biny*4
L = binx*biny

! define the jpar array
jpar(1) = ebsdnl%binning
jpar(2) = ebsdnl%numsx
jpar(3) = ebsdnl%numsy
jpar(4) = ebsdnl%npx
jpar(5) = ebsdnl%npy
jpar(6) = ebsdnl%numEbins
jpar(7) = ebsdnl%nE

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
euler_best(1:3,1:Nexp) = euler_bestmatch(1:3,1:Nexp)


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

write (*,*) 'input file ',trim(EMsoft_getEMdatapathname())//trim(ebsdnl%exptfile)

ename = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%exptfile)
ename = EMsoft_toNativePath(ename)
open(unit=iunitexpt,file=trim(ename),&
    status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

! prepare the fftw plan for this pattern size
EBSDPattern = 0.0
fdata = HiPassFilter(rdata,dims2,hipassw,init)

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

allocate(exptpatterns(binx*biny,ebsdnl%numexptsingle),stat=istat)
if(istat .ne. 0) then
    call FatalError('EMRefineOrientation:','could not allocate exptpatterns array')
end if
exptpatterns = 0.0

prepexperimentalloop: do iii = 1,Nexp

    tmpimageexpt = 0.0
    read(iunitexpt,rec=iii) imageexpt

! convert imageexpt to 2D EBS Pattern array
    do kk=1,biny
      EBSDPattern(1:binx,kk) = imageexpt((kk-1)*binx+1:kk*binx)
    end do


! Hi-Pass filter
    rdata = dble(EBSDPattern)
    fdata = HiPassFilter(rdata,dims2,hipassw)
    EBSDPattern = sngl(fdata)
   
! adaptive histogram equalization
    ma = maxval(EBSDPattern)
    mi = minval(EBSDPattern)
    
    EBSDpatternintd = ((EBSDPattern - mi)/ (ma-mi))
    EBSDpatterninteger = nint(EBSDpatternintd*255.0)
    EBSDpatternad =  adhisteq(ebsdnl%nregions,binx,biny,EBSDpatterninteger)
    EBSDPattern = float(EBSDpatternad)

! convert back to 1D vector
    do kk=1,biny
      imageexpt((kk-1)*binx+1:kk*binx) = EBSDPattern(1:binx,kk)
    end do

! normalize and apply circular mask
    imageexpt(1:L) = imageexpt(1:L) * masklin(1:L)
    imageexpt(1:L) = imageexpt(1:L)/NORM2(imageexpt(1:L))
    tmpimageexpt(1:L) = imageexpt(1:L)

! and write this pattern into the temporary file
    write(itmpexpt,rec=iii) tmpimageexpt

    if (mod(iii,10000) .eq. 0) then
        io_int(1) = iii
        call Writevalue('completed pre-processing pattern #',io_int,1,'(I8)')
    end if 

end do prepexperimentalloop

call Message('experimental patterns stored in tmp file','(A)')

!===================================================================================
!===============MAIN COMPUTATION LOOP===============================================
!===================================================================================

io_int(1) = nthreads
call WriteValue(' Attempting to set number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(nthreads)

call CPU_TIME(tstart)
call Time_tick(tick)

do iii = 1,cratioE
    
    exptpatterns = 0.0

    stepsize = LPs%ap/2.D0/ebsdnl%ncubochoric/2.D0
    
    do jj = 1,ppendE(iii)
        eindex = (iii - 1)*ebsdnl%numexptsingle + jj
        read(itmpexpt,rec=eindex) tmpimageexpt
        exptpatterns(1:binx*biny,jj) = tmpimageexpt(1:binx*biny)
    end do

    if (iii .eq. 1) then
        io_int = ebsdnl%numexptsingle
        call WriteValue('number of experimental patterns refined in one go = ',io_int,1,'(I8)')
    end if

    call Message('finished reading chunk of experimental data...refining now')
    
    do kk = 1,niter

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ii,tmpimageexpt,jj,quat,binned,ma,mi,eindex) &
!$OMP& PRIVATE(EBSDpatternintd,EBSDpatterninteger,EBSDpatternad,imagedictflt,ll,mm,dp) &
!$OMP& PRIVATE(cubneighbor,cu0)
!$OMP DO SCHEDULE(DYNAMIC)

        do ii = 1,ppendE(iii)

            eindex = (iii - 1)*ebsdnl%numexptsingle + ii
            tmpimageexpt = exptpatterns(:,ii)
            cu0 = eu2cu(euler_best(1:3,eindex))

            call CubochoricNeighbors(cubneighbor,Nmis,cu0,stepsize)

! calculate the dot product for each of the orientations in the neighborhood of the best match    
            do jj = 1,(2*Nmis + 1)**3

                quat(1:4) = cu2qu(cubneighbor(1:3,jj)) !CMarray(1:4,jj,eindex)
        
                call CalcEBSDPatternSingleFull(jpar,quat,acc%accum_e_detector,master%mLPNH,master%mLPSH,master%rgx,&
                           master%rgy,master%rgz,binned,Emin,Emax,mask,prefactor)

                ma = maxval(binned)
                mi = minval(binned)
              
                EBSDpatternintd = ((binned - mi)/ (ma-mi))
                EBSDpatterninteger = nint(EBSDpatternintd*255.0)
                EBSDpatternad =  adhisteq(ebsdnl%nregions,binx,biny,EBSDpatterninteger)
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

            if (mod(ii,5) .eq. 0) then
                io_int(1) = kk
                io_int(2) = eindex
                call Writevalue('completed refining loop, pattern #',io_int,2,'(I8,I8)')
            end if 
        end do

!$OMP END DO
!$OMP END PARALLEL

    stepsize = stepsize/2.D0
    end do
    
end do

euler_best = euler_best*180.D0/cPi

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

call CPU_TIME(tstop) 
tock = Time_tock(tick)
tstop = tstop - tstart

io_real(1) = tstop
call WriteValue('Execution time [CPU_TIME()] = ',io_real, 1)

io_int(1) = tock
call WriteValue('Execution time [system_clock()] = ',io_int,1,"(I8,' [s]')")

! close the fortran HDF interface
call h5close_EMsoft(hdferr)

end program EMRefineOrientation

