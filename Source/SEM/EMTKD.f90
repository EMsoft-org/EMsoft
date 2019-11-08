! ###################################################################
! Copyright (c) 2016-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMTKD.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMTKD
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMTKD computes energy-weighted TKD patterns
!
!> @date  05/09/17  MDG 1.0 original, based on EMEBSD.f90
! ###################################################################

program EMTKD

use local
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use io
use TKDmod
use stringconstants
use HDF5
use HDFsupport

IMPLICIT NONE

character(fnlen)                       :: nmldeffile, progname, progdesc
type(TKDNameListType)                  :: enl
type(MCNameListType)                   :: mcnl

type(TKDAngleType),pointer             :: angles
type(TKDLargeAccumType),pointer        :: acc
type(TKDMasterType),pointer            :: master

integer(kind=irg)                      :: res, error_cnt, hdferr
integer(kind=irg)                      :: istat
logical                                :: verbose

interface
        subroutine ComputeTKDPatterns(enl, angles, acc, master, progname, nmldeffile)
        
        use local
        use typedefs
        use NameListTypedefs
        use symmetry
        use crystal
        use constants
        use io
        use files
        use diffraction
        use TKDmod
        use Lambert
        use quaternions
        use rotations
        use noise
        
        IMPLICIT NONE
        
        type(TKDNameListType),INTENT(INOUT)     :: enl
        type(TKDAngleType),pointer              :: angles
        type(TKDLargeAccumType),pointer         :: acc
        type(TKDMasterType),pointer             :: master
        character(fnlen),INTENT(IN)             :: progname
        character(fnlen),INTENT(IN)             :: nmldeffile
        end subroutine ComputeTKDPatterns
end interface

nullify(angles)
nullify(acc)
nullify(master)

nmldeffile = 'EMTKD.nml'
progname = 'EMTKD.f90'
progdesc = 'Dynamical TKD patterns, using precomputed MC and master Lambert projections'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 75 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  write (*,*) 'json input not yet implemented'
  stop
  ! call JSONreadTKDNameList(enl, nmldeffile, error_cnt)
else
  call GetTKDNameList(nmldeffile,enl)
end if

! this program needs a lot of data, and it also should be integrated 
! with Dream.3D, so we need to make sure that all data is loaded outside
! of the main computational routine, and passed in as pointers/arguments
! either by the fortran program or by Dream.3D calls.  

! 1. read the angle array from file
verbose = .TRUE.
allocate(angles)
call TKDreadangles(enl, angles, verbose=.TRUE.)

! 2. read the Monte Carlo data file (HDF format)
allocate(acc)
! open the fortran HDF interface
call h5open_EMsoft(hdferr)
call TKDreadMCfile(enl, acc, verbose=.TRUE.)

! 3. read TKD master pattern file (HDF format)
allocate(master)
call TKDreadMasterfile(enl, master, verbose=.TRUE.)
! close the fortran HDF interface
call h5close_EMsoft(hdferr)

! 3.1 twin the master pattern with equal weight (FZ == pg 622)
!call TwinCubicMasterPattern(enl,master)

! 4. generate detector arrays
allocate(master%rgx(enl%numsx,enl%numsy), master%rgy(enl%numsx,enl%numsy), master%rgz(enl%numsx,enl%numsy), stat=istat)
allocate(acc%accum_e_detector(enl%numEbins,enl%numsx,enl%numsy), stat=istat)
call TKDGenerateDetector(enl, acc, master, verbose)
deallocate(acc%accum_e)

! call TwinCubicMasterPattern(enl, master)
! perform the zone axis computations for the knl input parameters
call ComputeTKDpatterns(enl, angles, acc, master, progname, nmldeffile)
deallocate(master, acc, angles)

end program EMTKD

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeTKDpatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an energy-weighted TKD pattern
!
!> @param enl name list
!> @param angles angle structure
!> @param acc energy accumulator arrays
!> @param master structure with master and detector arrays
!> @param progname program name string
!> @param nmldeffile name of nml file
!
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 08/01/13  MDG 3.0 complete rewrite, eliminated old Lambert projection
!> @date 09/25/13  MDG 3.1 replaced k-vector code by kvectors module
!> @date 02/26/14  MDG 4.0 new version
!> @date 03/26/14  MDG 4.1 adapted to new input and out file formats
!> @date 05/22/14  MDG 4.2 slight modification of angle input file; update for new EMTKDMaster file format
!> @date 06/24/14  MDG 5.0 removal of global variables; removal of namelist stuff; 
!> @date 03/09/15  MDG 5.1 added OpenMP functionality for final loop
!> @date 03/10/15  MDG 5.2 added 'bin' and 'gui' outputformat; added mask support for 'bin' outputformat
!> @date 03/14/15  MDG 5.3 attempt at speeding up the program by performing approximate energy sums (energyaverage = 1)
!> @date 03/20/15  MDG 5.4 corrected out-of-bounds error in TKDpattern array
!> @date 04/07/15  MDG 5.5 added HDF-formatted output
!> @date 05/08/15  MDG 5.6 added support for hexagonal/trigonal master pattern interpolation
!> @date 09/01/15  MDG 6.0 changed Lambert patterns to Northern+Southern hemisphere...
!> @date 10/11/15  MDG 6.1 correction of error in saving of multiple patterns in GUI mode
!> @date 10/14/15  SS  6.2 bug fix in binning of pattern
!> @date 03/03/16  MDG 6.3 corrected error in energywf computation when energymin is not the lowest possible energy
!> @date 03/15/16  MDG 6.4 changed OpenMP do-loop mode to DYNAMIC; replaced CPU_TIME by omp_get_wtime().
!> @date 03/17/16  MDG 6.5 changed OpenMP mode again; added function call to TKDmod; added private variables to OpenMP section
!> @date 04/18/16  SS  6.6 removed outputformat .eq. bin check while writing hyperslab
!> @date 11/15/16  MDG 6.7 added CrystalData output to HDF file and removed unused entries.
!> @date 02/07/17  MDG 6.8 corrected bug when number of Euler angle triplets was smaller than requested number of threads
!> @date 05/09/17  MDG 7.0 forked program for TKD pattern simulations...
!> @date 02/19/19  MDG 7.1 updates HDFFileVersion to 4.1
!--------------------------------------------------------------------------
subroutine ComputeTKDpatterns(enl, angles, acc, master, progname, nmldeffile)

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
use TKDmod
use Lambert
use quaternions
use rotations
use noise
use HDF5
use HDFsupport
use ISO_C_BINDING
use omp_lib
use timing
use stringconstants

IMPLICIT NONE

type(TKDNameListType),INTENT(INOUT)     :: enl
type(TKDAngleType),pointer              :: angles
type(TKDLargeAccumType),pointer         :: acc
type(TKDMasterType),pointer             :: master
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile


! all geometrical parameters and filenames
real(kind=dbl)                          :: prefactor, qz(3)

! allocatable arrays
real(kind=sgl),allocatable              :: TKDpattern(:,:), binned(:,:)        ! array with TKD patterns
real(kind=sgl),allocatable              :: z(:,:)               ! used to store the computed patterns before writing to disk
real(kind=sgl),allocatable              :: energywf(:), eulerangles(:,:)
real(kind=sgl),allocatable              :: accum_e_MC(:,:,:)

! arrays for each OpenMP thread
real(kind=sgl),allocatable              :: tmLPNH(:,:,:) , tmLPSH(:,:,:)
real(kind=sgl),allocatable              :: trgx(:,:), trgy(:,:), trgz(:,:)          ! auxiliary detector arrays needed for interpolation
real(kind=sgl),allocatable              :: taccum(:,:,:)
integer(kind=HSIZE_T)                   :: dims2(2),dims3(3)

! quaternion variables
real(kind=dbl)                          :: qq(4), qq1(4), qq2(4), qq3(4)

! various items
integer(kind=irg)                       :: i, j, iang, jang, k, io_int(6), etotal, hdferr, tickstart          ! various counters
integer(kind=irg)                       :: istat, ipar(7), tick
integer(kind=irg)                       :: nix, niy, binx, biny,num_el, nixp, niyp, maxthreads,nextra,ninlastbatch,nlastremainder     ! various parameters
integer(kind=irg)                       :: NUMTHREADS, TID   ! number of allocated threads, thread ID
integer(kind=irg)                       :: ninbatch, nbatches,nremainder,ibatch,nthreads,maskradius,nlastbatches, totnumbatches
integer(kind=irg),allocatable           :: istart(:,:), istop(:,:), patinbatch(:)

real(kind=sgl)                          :: bindx, sig, ma, mi, tstop, io_real(1), tock
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second
integer(kind=irg),parameter             :: storemax = 20        ! number of TKD patterns stored in one output block
integer(kind=irg)                       :: Emin, Emax      ! various parameters
real(kind=dbl)                          :: dc(3), scl, nel           ! direction cosine array
real(kind=dbl)                          :: sx, dx, dxm, dy, dym, rhos, x         ! various parameters
real(kind=dbl)                          :: ixy(2), tmp

real(kind=sgl),allocatable              :: mask(:,:), lx(:), ly(:)
character(kind=c_char),allocatable      :: batchpatterns(:,:,:), bpat(:,:), threadbatchpatterns(:,:,:) 
integer(kind=irg),allocatable           :: acc_array(:,:)
real(kind=sgl),allocatable              :: master_arrayNH(:,:), master_arraySH(:,:), wf(:) 
character(len=3)                        :: outputformat

! parameter for random number generator
integer, parameter                      :: K4B=selected_int_kind(9)      ! used by ran function in math.f90
integer(K4B)                            :: idum

type(HDFobjectStackType)                :: HDF_head
type(unitcell)                          :: cell
integer(HSIZE_T), dimension(1:3)        :: hdims, offset 
integer(HSIZE_T)                        :: dim0, dim1, dim2
character(fnlen,kind=c_char)            :: line2(1)
character(fnlen)                        :: groupname, dataset, datagroupname, attributename, HDF_FileVersion
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
character(fnlen)                        :: datafile
logical                                 :: overwrite = .TRUE., insert = .TRUE., singlebatch

!====================================
! max number of OpenMP threads on this platform
maxthreads = omp_get_max_threads()

nullify(HDF_head%next)
!nullify(cell)        

!====================================
! what is the output format?  GUI or BIN ?
outputformat = enl%outputformat

! define some energy-related parameters derived from MC input parameters
!====================================
etotal = enl%num_el 
sig = enl%MCsig

! get the indices of the minimum and maximum energy
Emin = nint((enl%energymin - enl%Ehistmin)/enl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.enl%numEbins)  Emin=enl%numEbins

Emax = nint((enl%energymax - enl%Ehistmin)/enl%Ebinsize) +1
if (Emax.lt.1)  Emax=1
if (Emax.gt.enl%numEbins)  Emax=enl%numEbins

nel = sum(acc%accum_e_detector)
num_el = nint(nel)

allocate(energywf(Emin:Emax), wf(enl%numEbins),stat=istat)
energywf = 0.0
wf = 0.0

wf = sum(sum(acc%accum_e_detector,3),2)
energywf(Emin:Emax) = wf(Emin:Emax)
energywf = energywf/sum(energywf)
deallocate(wf)

!====================================

!====================================
! init a bunch of parameters
!====================================
! binned pattern array
  binx = enl%numsx/enl%binning
  biny = enl%numsy/enl%binning
  bindx = 1.0/float(enl%binning)**2

! intensity prefactor
  prefactor = 0.25D0 * nAmpere * enl%beamcurrent * enl%dwelltime * 1.0D-15/ nel
!====================================

!allocate(cell)        
cell%fname = trim(enl%MCxtalname)
call ReadDataHDF(cell)

!====================================
! ------ and open the output file for IDL visualization (only thread 0 can write to this file)
!====================================
! we need to write the image dimensions, and also how many of those there are...

! Initialize FORTRAN interface.
!
CALL h5open_EMsoft(hdferr)

call timestamp(datestring=dstr, timestring=tstrb)
tstre = tstrb

! Create a new file using the default properties.
datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
datafile = EMsoft_toNativePath(datafile)

hdferr =  HDF_createFile(datafile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createFile ')

! write the EMheader to the file
datagroupname = 'TKD'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLfiles')

! read the text file and write the array to the file
dataset = SC_EMTKDNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetTextFile EMTKDNML')

call HDF_pop(HDF_head)

! create a NMLparameters group to write all the namelist entries into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLparameters')

call HDFwriteTKDNameList(HDF_head, enl)

! and leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EMData')

! create the TKD group and add a HDF_FileVersion attribbute to it 
hdferr = HDF_createGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup TKD')
! before Feb. 19, 2019, an undetected error caused all patterns to be upside down in the Kikuchi bands only,
! not in the background intensity profile.  This was compensated by a pattern flip of all experimental 
! patterns in the dictionary indexing program, but when taking individual patterns from this program, they
! are actually upside down in all versions through HDF_FileVersion 4.0.  As of 4.1, the patterns are in the
! correct orientation.  This was detected by manually indexing a simulated pattern.
  HDF_FileVersion = '4.1'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0 and above
! =====================================================

dataset = SC_numangles
hdferr = HDF_writeDatasetInteger(dataset, enl%numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger numangles')

! and add the Euler angles to the output file
allocate(eulerangles(3,enl%numangles))
do i=1,enl%numangles
  eulerangles(1:3,i) = qu2eu(angles%quatang(1:4,i))
end do
dataset = SC_Eulerangles
hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerangles, 3, enl%numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloatArray2D Eulerangles')
! =====================================================
! end of HDF_FileVersion = 4.0 and above write statements
! =====================================================

! and we leave this group open for further data output from the main program loop ... 

!====================================
! ------ start the actual image computation loop
!====================================

!====================================
! to speed things up, we'll split the computation into batches of 1,024 patterns per thread; once those 
! are computed, we leave the OpenMP part to write them to a file 
!====================================


! and allocate space to store each batch; this requires some careful analysis
! since we are doing things in multiple threads
  nthreads = enl%nthreads
  if (nthreads.gt.maxthreads) then
    io_int(1) = maxthreads
    call WriteValue('# threads requested is larger than available number; resetting to ',io_int, 1)
    nthreads = maxthreads
  end if
  ninbatch = 1024
  nlastbatches = 0
  singlebatch = .FALSE.
  if (enl%numangles.ge.ninbatch*nthreads) then 
    nbatches = enl%numangles/(ninbatch*nthreads)
    nremainder = mod(enl%numangles,ninbatch*nthreads)
    nextra = 0
    if (nremainder.gt.0) then
      singlebatch = .TRUE.
      nextra = 1 
      ninlastbatch = nremainder/nthreads+1
      nlastremainder = nremainder - (nthreads-1)*ninlastbatch
    end if
  else
! if there are fewer patterns than ninbatch*nthreads we need to redefine ninbatch
    singlebatch = .TRUE.
    if (enl%numangles.le.nthreads) then
      nthreads = 1
    end if
    nbatches = 0
    ninlastbatch = enl%numangles/nthreads+1
    nlastremainder = enl%numangles - (nthreads-1)*ninlastbatch
    nlastbatches = 1
    nextra = 0
    if (nlastremainder.gt.0) nextra = 1 
end if
  if (nbatches.ne.0) then
    io_int(1) = enl%numangles 
    io_int(2) = ninbatch
    io_int(3) = nthreads
    io_int(4) = nbatches
    io_int(5) = nremainder
    call WriteValue('  OpenMP loop variables : ',io_int,5,"(I10,' = ',I4,' * ',I2,' * ',I4,' + ',I6)")
  end if
  if ((ninlastbatch.ne.0).and.(nextra.ne.0)) then
    io_int(1) = enl%numangles - nbatches * nthreads * ninbatch
    io_int(2) = ninlastbatch
    io_int(3) = nthreads-1
    io_int(4) = 1
    io_int(5) = nlastremainder
    call WriteValue('  Remainder loop variables : ',io_int,5,"(I10,' = ',I4,' * ',I2,' * ',I4,' + ',I6)")
  end if

! allocate the istart and istop arrays for all the separate runs
  totnumbatches = nbatches + nextra
  allocate(istart(0:nthreads-1,totnumbatches)) 
  allocate(istop(0:nthreads-1,totnumbatches)) 
  allocate(patinbatch(totnumbatches))
  do i=1,nbatches
    do j=0,nthreads-1
      istart(j,i) = 1 + ninbatch * ( j + nthreads*(i-1) )
      istop(j,i)  = ninbatch * ( j+1 + nthreads*(i-1) )
    end do
  end do
  if (nextra.eq.1) then
    i = nbatches+1
    do j=0,nthreads-1
      istart(j,i) = nthreads*ninbatch*nbatches + 1 + ninlastbatch * j
      if (j.ne.nthreads-1) then
         istop(j,i) = nthreads*ninbatch*nbatches + ninlastbatch * ( j + 1 )
      else
         istop(j,i) = nthreads*ninbatch*nbatches + ninlastbatch * ( nthreads - 1 ) + nlastremainder
      end if
    end do
  end if
  patinbatch = sum(istop-istart,1) + nthreads

! and allocate the batchpatterns array for hyperslab writing
  allocate(batchpatterns(binx,biny,ninbatch*nthreads),stat=istat)

!====================================
! here we also create a mask if necessary
  allocate(mask(binx,biny),stat=istat)
  mask = 1.0
  if (enl%maskpattern.eq.'y') then
! create the circular mask in a potentially rectangular array
    maskradius = (minval( (/ binx, biny /) ) / 2 )**2
    allocate(lx(binx), ly(biny), stat=istat)
    lx = (/ (float(i),i=1,binx) /) - float(binx/2)
    ly = (/ (float(i),i=1,biny) /) - float(biny/2)
    do i=1,binx
      do j=1,biny
        if ((lx(i)**2+ly(j)**2).gt.maskradius) mask(i,j) = 0.0
      end do
    end do
    deallocate(lx, ly)
  end if

!====================================
! for dictionary computations, the patterns are usually rather small, so perhaps the explicit 
! energy sums can be replaced by an averaged approximate approach, in which all the energy bins
! are added together from the start, and all the master patterns are totaled as well...
if (enl%energyaverage.eq.1) then
  allocate(acc_array(enl%numsx,enl%numsy))
  acc_array = sum(acc%accum_e_detector,1)
  allocate(wf(enl%numEbins))
  wf = sum(sum(acc%accum_e_detector,2),2)
  wf = wf/sum(wf)

! this is a straightforward sum; we should probably do a weighted sum instead
  allocate(master_arrayNH(-enl%npx:enl%npx,-enl%npy:enl%npy))
  allocate(master_arraySH(-enl%npx:enl%npx,-enl%npy:enl%npy))
  do k=Emin,Emax
    master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,k) = master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,k) * wf(k)
    master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,k) = master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,k) * wf(k)
  end do
  master_arrayNH = sum(master%mLPNH,3)
  master_arraySH = sum(master%mLPSH,3)

end if

allocate(accum_e_MC(enl%numEbins,enl%numsx,enl%numsy),stat=istat)
accum_e_MC = acc%accum_e_detector

!====================================
! determine the scale factor for the Lambert interpolation
scl = dble(enl%npx) 

!====================================
! define the integer parameter list for the CalcTKDPatternSingleFull call
ipar(1) = enl%binning
ipar(2) = enl%numsx
ipar(3) = enl%numsy
ipar(4) = enl%npx
ipar(5) = enl%npy
ipar(6) = enl%numEbins
ipar(7) = enl%nE

!====================================
! set the number of OpenMP threads 
io_int(1) = nthreads
call WriteValue(' Setting number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(nthreads)

call Time_tick(tickstart)
call Time_tick(tick)

!====================================
!====================================
do ibatch=1,totnumbatches

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL default(shared)  PRIVATE(TID,iang,i,j,istat,TKDpattern,binned,idum,bpat,ma,mi,threadbatchpatterns)&
!$OMP& PRIVATE(tmLPNH, tmLPSH, trgx, trgy, trgz, taccum, dims2, dims3)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

! initialize the random number generator for the Poison noise
!  idum = -1-TID               

! each thread needs a private copy of the master and accum arrays; not having
! those can produce poor scaling...
  dims2 = shape(master%rgx)
  allocate(trgx(dims2(1),dims2(2)), trgy(dims2(1),dims2(2)), trgz(dims2(1),dims2(2)))
  dims3 = shape(accum_e_MC)
  allocate(taccum(dims3(1),dims3(2),dims3(3)))
  dims3 = shape(master%mLPNH)
  allocate(tmLPNH(dims3(1),dims3(2),dims3(3)), tmLPSH(dims3(1),dims3(2),dims3(3)))
! and copy the data in
  trgx = master%rgx
  trgy = master%rgy
  trgz = master%rgz
  taccum = accum_e_MC
  tmLPNH = master%mLPNH
  tmLPSH = master%mLPSH

! allocate the arrays that will hold the computed pattern
  allocate(binned(binx,biny),stat=istat)
  allocate(bpat(binx,biny),stat=istat)

! this array requires some care in terms of its size parameters...
  if ((singlebatch.eqv..TRUE.).AND.(ibatch.eq.totnumbatches)) then 
    if (TID.eq.nthreads-1) then
      allocate(threadbatchpatterns(binx,biny,nlastremainder),stat=istat)
    else
      allocate(threadbatchpatterns(binx,biny,ninlastbatch),stat=istat)
    end if
  else
    allocate(threadbatchpatterns(binx,biny,ninbatch),stat=istat)
  end if

  threadbatchpatterns = ' '

  do iang=istart(TID,ibatch),istop(TID,ibatch)
! convert the direction cosines to quaternions, include the 
! sample quaternion orientation, and then back to direction cosines...
! then convert these individually to the correct TKD pattern location
    binned = 0.0
    bpat = ' '

    call CalcTKDPatternSingleFull(ipar,angles%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                   Emin,Emax,mask,prefactor)

    if (enl%scalingmode .eq. 'gam') then
        binned = binned**enl%gammavalue
    end if

    ma = maxval(binned)
    mi = minval(binned)
    binned = mask * ((binned - mi)/ (ma-mi))
    bpat = char(nint(255.0*binned))

    threadbatchpatterns(1:binx,1:biny, iang-istart(TID,ibatch)+1) = bpat

  end do ! end of iang loop

! and now we write the threadbatchpatterns arrays to the main batch patterns array; we 
! need to intercept the special case when there are remainder patterns!
!$OMP CRITICAL
  if ((singlebatch.eqv..TRUE.).AND.(ibatch.eq.totnumbatches)) then 
    if (TID.eq.nthreads-1) then
      batchpatterns(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
        threadbatchpatterns(1:binx,1:biny, 1:nlastremainder)
    else
      batchpatterns(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
        threadbatchpatterns(1:binx,1:biny, 1:ninlastbatch)
    end if
  else
    batchpatterns(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatterns(1:binx,1:biny, 1:ninbatch)
  end if
!$OMP END CRITICAL

!$OMP END PARALLEL

! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0
! =====================================================
! here we write all the entries in the batchpatterns array to the HDF file as a hyperslab
dataset = SC_TKDpatterns
 !if (outputformat.eq.'bin') then
   offset = (/ 0, 0, (ibatch-1)*ninbatch*enl%nthreads /)
   hdims = (/ binx, biny, enl%numangles /)
   dims3 = (/ binx, biny, patinbatch(ibatch) /)
   if (ibatch.eq.1) then
     hdferr = HDF_writeHyperslabCharArray3D(dataset, batchpatterns(1:binx,1:biny,1:dim2), hdims, offset, dims3, HDF_head)
     if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabCharArray3D TKDpatterns')
   else
     hdferr = HDF_writeHyperslabCharArray3D(dataset, batchpatterns(1:binx,1:biny,1:dim2), hdims, offset, dims3, HDF_head, insert)
     if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabCharArray3D TKDpatterns')
   end if
 !end if
! =====================================================
! end of HDF_FileVersion = 4.0 write statements
! =====================================================

 io_int(1) = ibatch
 io_int(2) = totnumbatches
 call WriteValue('Completed cycle ',io_int,2,"(I4,' of ',I4)")

end do
!====================================
!====================================

tstop = Time_tock(tickstart) 
tock = Time_tock(tick)

io_real(1) = tstop
call WriteValue('Execution time [CPU_TIME()] = ',io_real, 1)

io_int(1) = tock
call WriteValue('Execution time [system_clock()] = ',io_int,1,"(I8,' [s]')")

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! and update the end time
call timestamp(datestring=dstr, timestring=tstre)
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup EMheader')

datagroupname = "TKD"
hdferr = HDF_openGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup TKD')

! stop time /EMheader/StopTime 'character'
dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetStringArray StopTime')

dataset = SC_Duration
hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloat Duration')

! close the datafile
call HDF_pop(HDF_head,.TRUE.)

! close the Fortran interface
call h5close_EMsoft(hdferr)


end subroutine ComputeTKDpatterns

