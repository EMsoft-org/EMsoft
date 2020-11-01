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
! EMsoft:EMEBSD.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSD
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSD computes energy-weighted EBSD patterns
!
!> @date  08/01/12  MDG 1.0 EBSD extraction program for fundamental zone patterns
!> @date  08/17/12  MDG 1.1 generalized fundamental zone to other symmetries
!> @date  09/20/12  MDG 1.2 adapted for Lambert projection
!> @date  09/25/12  MDG 1.3 prepared for multithreaded version by separating computation steps
!> @date  12/11/12  MDG 2.0 new branch with energy-dependent Lambert projections (cubic only for now)
!> @date  02/26/14  MDG 3.0 incorporation into git and adapted to new libraries
!> @date  03/26/14  MDG 3.1 modification of file formats; made compatible with IDL visualization interface
!> @date  06/24/14  MDG 4.0 removal of all global variables; separation of nml from computation; OpenMP
!> @date  03/10/15  MDG 4.1 added output format selector
!> @date  04/02/15  MDG 5.0 changed program input & output to HDF format
!> @date  09/01/15  MDG 5.1 change from single to double Lambert master patterns (lots of changes)
!> @date  09/26/15  MDG 5.2 added json support for nml file
!> @date  10/07/15  MDG 5.3 minor clean up in preparation for release 3.0
!> @date  05/21/16  MDG 5.4 updated for new HDF file organization
!> @date  10/13/17  MDG 5.5 added code to compute "hybrid" deformed EBSD patterns...
!> @date  02/22/18  MDG 5.6 added option to include pattern center and deformation tensor in Euler input file
! ###################################################################

program EMEBSD

use local
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use io
use HDF5
use HDFsupport
use error
use detectors
use EBSDmod
use stringconstants

IMPLICIT NONE

character(fnlen)                       :: nmldeffile, progname, progdesc
type(EBSDNameListType)                 :: enl
type(MCCLNameListType)                 :: mcnl
type(EBSDMasterNameListType)           :: mpnl

type(EBSDAngleType),pointer            :: angles
type(EBSDAnglePCDefType),pointer       :: orpcdef
type(EBSDMCdataType)                   :: EBSDMCdata
type(EBSDMPdataType)                   :: EBSDMPdata
type(EBSDDetectorType)                 :: EBSDdetector

integer(kind=irg)                      :: res, error_cnt, hdferr, numangles
integer(kind=irg)                      :: istat
logical                                :: verbose

interface
        subroutine ComputeEBSDPatterns(enl, mcnl, mpnl, numangles, angles, EBSDMCdata, EBSDMPdata, EBSDdetector, &
          progname, nmldeffile)

        use local
        use typedefs
        use NameListTypedefs
        use NameListHDFwriters
        use symmetry
        use crystal
        use constants
        use io
        use files
        use filters
        use diffraction
        use EBSDmod
        use Lambert
        use quaternions
        use rotations
        use filters
        use HDF5
        use HDFsupport
        use ISO_C_BINDING
        use omp_lib
        use timing
        use stringconstants
        use math

        IMPLICIT NONE

        type(EBSDNameListType),INTENT(INOUT)    :: enl
        type(MCCLNameListType),INTENT(INOUT)    :: mcnl
        type(EBSDMasterNameListType),INTENT(INOUT) :: mpnl
        integer(kind=irg),INTENT(IN)            :: numangles
        type(EBSDAngleType),pointer             :: angles
        type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
        type(EBSDMPdataType),INTENT(INOUT)      :: EBSDMPdata
        type(EBSDDetectorType),INTENT(INOUT)    :: EBSDdetector
        character(fnlen),INTENT(IN)             :: progname
        character(fnlen),INTENT(IN)             :: nmldeffile
        end subroutine ComputeEBSDPatterns

        subroutine ComputedeformedEBSDPatterns(enl, mcnl, mpnl, numangles, orpcdef, EBSDMCdata, EBSDMPdata, progname, nmldeffile)

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
        use EBSDmod
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
        use math

        IMPLICIT NONE

        type(EBSDNameListType),INTENT(INOUT)    :: enl
        type(MCCLNameListType),INTENT(INOUT)    :: mcnl
        type(EBSDMasterNameListType),INTENT(INOUT) :: mpnl
        integer(kind=irg),INTENT(IN)            :: numangles
        type(EBSDAnglePCDefType),pointer        :: orpcdef
        type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
        type(EBSDMPdataType),INTENT(INOUT)      :: EBSDMPdata
        character(fnlen),INTENT(IN)             :: progname
        character(fnlen),INTENT(IN)             :: nmldeffile
        end subroutine ComputedeformedEBSDPatterns
end interface

nmldeffile = 'EMEBSD.nml'
progname = 'EMEBSD.f90'
progdesc = 'Dynamical EBSD patterns, using precomputed MC and master Lambert projections'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 22 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call JSONreadEBSDNameList(enl, nmldeffile, error_cnt)
else
  call GetEBSDNameList(nmldeffile,enl)
end if

! this program needs a lot of data, and it also should be integrated 
! with EMsoftWorkBench, so we need to make sure that all data is loaded outside
! of the main computational routine, and passed in as pointers/arguments
! either by the fortran program or by EMsoftWorkBench calls.  

! 1. read the angle array from file
verbose = .TRUE.
if (trim(enl%anglefiletype).eq.'orientations') then 
  nullify(angles)
  allocate(angles)
  call EBSDreadangles(enl, numangles, angles, verbose=.TRUE.)
else if (trim(enl%anglefiletype).eq.'orpcdef') then 
  nullify(orpcdef)
  allocate(orpcdef)
  call EBSDreadorpcdef(enl, numangles, orpcdef, verbose=.TRUE.)
else 
  call FatalError('EMEBSD','unknown anglefiletype')
end if 

! 2. read the Monte Carlo data file (HDF format)
call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(enl%energyfile, mcnl, hdferr, EBSDMCdata, getAccume=.TRUE.)

! 3. read EBSD master pattern file (HDF format)
call readEBSDMasterPatternFile(enl%masterfile, mpnl, hdferr, EBSDMPdata, getmLPNH=.TRUE., getmLPSH=.TRUE.)
call h5close_EMsoft(hdferr)

! for a regular Euler angle file, we precompute the detector arrays here; for the 'orpcdef' mode
! we compute them later (for each pattern separately)
if (trim(enl%anglefiletype).eq.'orientations') then
  allocate(EBSDdetector%rgx(enl%numsx,enl%numsy), &
           EBSDdetector%rgy(enl%numsx,enl%numsy), &
           EBSDdetector%rgz(enl%numsx,enl%numsy), &
           EBSDdetector%accum_e_detector(EBSDMCdata%numEbins,enl%numsx,enl%numsy), stat=istat)
! 4. generate detector arrays
  call GenerateEBSDDetector(enl, mcnl, EBSDMCdata, EBSDdetector, verbose)

  deallocate(EBSDMCdata%accum_e)

  ! perform the zone axis computations for the knl input parameters
  call ComputeEBSDpatterns(enl, mcnl, mpnl, numangles, angles, EBSDMCdata, EBSDMPdata, EBSDdetector, progname, nmldeffile)
  deallocate(angles)
end if

if (trim(enl%anglefiletype).eq.'orpcdef') then
  call ComputedeformedEBSDpatterns(enl, mcnl, mpnl, numangles, orpcdef, EBSDMCdata, EBSDMPdata, progname, nmldeffile)
  deallocate(orpcdef)
end if 
  
end program EMEBSD

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeEBSDPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an energy-weighted EBSD pattern
!
!> @param enl name list
!> @param mcnl Monte Carlo name list
!> @param mpnl master pattern name list
!> @param numangles number of angles to consider
!> @param angles angle structure
!> @param EBSDMCdata Monte Carlo arrays
!> @param EBSDMPdata Master Pattern arrays
!> @param EBSDdetector detector arrays
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
!> @date 05/22/14  MDG 4.2 slight modification of angle input file; update for new EMEBSDMaster file format
!> @date 06/24/14  MDG 5.0 removal of global variables; removal of namelist stuff; 
!> @date 03/09/15  MDG 5.1 added OpenMP functionality for final loop
!> @date 03/10/15  MDG 5.2 added 'bin' and 'gui' outputformat; added mask support for 'bin' outputformat
!> @date 03/14/15  MDG 5.3 attempt at speeding up the program by performing approximate energy sums (energyaverage = 1)
!> @date 03/20/15  MDG 5.4 corrected out-of-bounds error in EBSDpattern array
!> @date 04/07/15  MDG 5.5 added HDF-formatted output
!> @date 05/08/15  MDG 5.6 added support for hexagonal/trigonal master pattern interpolation
!> @date 09/01/15  MDG 6.0 changed Lambert patterns to Northern+Southern hemisphere...
!> @date 10/11/15  MDG 6.1 correction of error in saving of multiple patterns in GUI mode
!> @date 10/14/15  SS  6.2 bug fix in binning of pattern
!> @date 03/03/16  MDG 6.3 corrected error in energywf computation when energymin is not the lowest possible energy
!> @date 03/15/16  MDG 6.4 changed OpenMP do-loop mode to DYNAMIC; replaced CPU_TIME by omp_get_wtime().
!> @date 03/17/16  MDG 6.5 changed OpenMP mode again; added function call to EBSDmod; added private variables to OpenMP section
!> @date 04/18/16  SS  6.6 removed outputformat .eq. bin check while writing hyperslab
!> @date 11/15/16  MDG 6.7 added CrystalData output to HDF file and removed unused entries.
!> @date 02/07/17  MDG 6.8 corrected bug when number of Euler angle triplets was smaller than requested number of threads
!> @date 09/26/17  MDG 7.0 added ability to incorporate a deformation tensor in the pattern computation
!> @date 10/13/17  MDG 7.1 correction of deformation tensor code; tested for tetragonal, monoclinic and anorthic deformations
!> @date 12/20/17  MDG 7.2 added switch to turn off realistic background intensity profile
!> @date 04/03/18  MDG 8.0 rewrite with separated name lists and new more modular data structures
!> @date 02/19/19  MDG 8.1 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
subroutine ComputeEBSDPatterns(enl, mcnl, mpnl, numangles, angles, EBSDMCdata, EBSDMPdata, EBSDdetector, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use symmetry
use crystal
use constants
use io
use files
use filters
use diffraction
use EBSDmod
use Lambert
use quaternions
use rotations
use filters
use HDF5
use HDFsupport
use ISO_C_BINDING
use omp_lib
use timing
use stringconstants
use math

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
type(EBSDMasterNameListType),INTENT(INOUT) :: mpnl
integer(kind=irg),INTENT(IN)            :: numangles
type(EBSDAngleType),pointer             :: angles
type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
type(EBSDMPdataType),INTENT(INOUT)      :: EBSDMPdata
type(EBSDDetectorType),INTENT(INOUT)    :: EBSDdetector
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile

! all geometrical parameters and filenames
real(kind=dbl)                          :: prefactor, qz(3)

! allocatable arrays
real(kind=sgl),allocatable              :: EBSDpattern(:,:), binned(:,:)        ! array with EBSD patterns
real(kind=sgl),allocatable              :: z(:,:)               ! used to store the computed patterns before writing to disk
real(kind=sgl),allocatable              :: energywf(:), eulerangles(:,:)

! arrays for each OpenMP thread
real(kind=sgl),allocatable              :: tmLPNH(:,:,:) , tmLPSH(:,:,:)
real(kind=sgl),allocatable              :: trgx(:,:), trgy(:,:), trgz(:,:)          ! auxiliary detector arrays needed for interpolation
real(kind=sgl),allocatable              :: taccum(:,:,:)

! quaternion variables
real(kind=dbl)                          :: qq(4), qq1(4), qq2(4), qq3(4)

! various items
integer(kind=irg)                       :: i, j, iang, jang, k, io_int(6), hdferr, L, correctsize, dim1, dim2          ! various counters
integer(kind=irg)                       :: istat, ipar(7), tick, tock, tickstart
integer(kind=irg)                       :: nix, niy, binx, biny, nixp, niyp, maxthreads,nextra,ninlastbatch,nlastremainder, npy     ! various parameters
integer(kind=irg)                       :: NUMTHREADS, TID   ! number of allocated threads, thread ID
integer(kind=irg)                       :: ninbatch, nbatches,nremainder,ibatch,nthreads,maskradius,nlastbatches, totnumbatches
integer(kind=irg),allocatable           :: istart(:,:), istop(:,:), patinbatch(:)

real(kind=sgl)                          :: bindx, ma, mi, tstart, tstop, io_real(3)
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second
integer(kind=irg),parameter             :: storemax = 20        ! number of EBSD patterns stored in one output block
integer(kind=irg)                       :: Emin, Emax      ! various parameters
real(kind=dbl)                          :: dc(3), scl, nel, emult           ! direction cosine array
real(kind=dbl)                          :: sx, dx, dxm, dy, dym, rhos, x         ! various parameters
real(kind=dbl)                          :: ixy(2), tmp

real(kind=sgl),allocatable              :: mask(:,:), lx(:), ly(:), masklin(:), binnedvec(:)
character(kind=c_char),allocatable      :: batchpatterns(:,:,:), bpat(:,:), threadbatchpatterns(:,:,:) 
integer(kind=irg),allocatable           :: batchpatternsint(:,:,:), bpatint(:,:), threadbatchpatternsint(:,:,:) 
real(kind=sgl),allocatable              :: batchpatterns32(:,:,:), threadbatchpatterns32(:,:,:), threadbatchpatterns32lin(:,:) 
real(kind=sgl),allocatable              :: batchpatterns32lin(:,:)
integer(kind=irg),allocatable           :: acc_array(:,:)
real(kind=sgl),allocatable              :: master_arrayNH(:,:), master_arraySH(:,:), wf(:) 
character(len=3)                        :: outputformat
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)

! parameter for random number generator
integer, parameter                      :: K4B=selected_int_kind(9)      ! used by ran function in math.f90
integer(K4B)                            :: idum

type(HDFobjectStackType)                :: HDF_head
type(unitcell)                          :: cell
integer(HSIZE_T), dimension(1:3)        :: hdims, offset 
integer(HSIZE_T), dimension(1:2)        :: hdims2, offset2 
integer(HSIZE_T)                        :: dims2(2), dims3(3)
character(fnlen,kind=c_char)            :: line2(1)
character(fnlen)                        :: groupname, dataset, datagroupname, attributename, HDF_FileVersion
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
character(10)                           :: char10
character(fnlen)                        :: datafile
logical                                 :: overwrite = .TRUE., insert = .TRUE., singlebatch
character(5)                            :: bitmode
integer(kind=irg)                       :: numbits
real(kind=sgl)                          :: bitrange

! new stuff: deformation tensor
real(kind=dbl)                          :: Umatrix(3,3), Fmatrix(3,3), Smatrix(3,3), quF(4), Fmatrix_inverse(3,3), &
                                           Gmatrix(3,3), gs2c(3,3)
logical                                 :: includeFmatrix=.FALSE., noise

!====================================
! max number of OpenMP threads on this platform
maxthreads = omp_get_max_threads()

nullify(HDF_head%next)
!nullify(cell)        

!====================================
! what is the output format?  GUI or BIN ?
outputformat = enl%outputformat

!====================================
! bit depth and format of output
call get_bit_parameters(enl%bitdepth, numbits, bitrange, bitmode)

if (enl%makedictionary.eq.'y') then
  bitmode = 'dict'
  call Message('Program will work in dictionary generation mode')
end if

! define some energy-related parameters derived from MC input parameters
!====================================
noise = .FALSE.
if (enl%poisson.eq.'y') noise = .TRUE.

! make sure the requested energy range is within the range available from the Monte Carlo computation
if (enl%energymin.lt.mcnl%Ehistmin) enl%energymin = mcnl%Ehistmin
if (enl%energymax.gt.mcnl%EkeV) enl%energymax = mcnl%EkeV

! get the indices of the minimum and maximum energy
Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) + 1
if (Emin.lt.1)  Emin=1
if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) + 1
if (Emax.lt.1)  Emax=1
if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins

! modified by MDG, 03/26/18
!nel = sum(acc%accum_e_detector)
nel = float(mcnl%totnum_el) * float(EBSDMCdata%multiplier)
emult = nAmpere * 1e-9 / nel  ! multiplicative factor to convert MC data to an equivalent incident beam of 1 nanoCoulomb
write (*,*) ' Multiplicative factor to generate 1 nC of incident electrons ', emult
! intensity prefactor  (redefined by MDG, 3/23/18)
! prefactor = 0.25D0 * nAmpere * enl%beamcurrent * enl%dwelltime * 1.0D-15/ nel
prefactor = emult * enl%beamcurrent * enl%dwelltime * 1.0D-6
write (*,*) ' Intensity scaling prefactor = ', prefactor

allocate(energywf(Emin:Emax), wf(EBSDMCdata%numEbins),stat=istat)
energywf = 0.0
wf = 0.0

wf = sum(sum(EBSDdetector%accum_e_detector,3),2)
energywf(Emin:Emax) = wf(Emin:Emax)
energywf = energywf/sum(energywf)
deallocate(wf)

!====================================
! init a bunch of parameters
!====================================
! binned pattern array
  binx = enl%numsx/enl%binning
  biny = enl%numsy/enl%binning
  bindx = 1.0/float(enl%binning)**2
!====================================

!allocate(cell)        
cell%fname = trim(mcnl%xtalname)
call ReadDataHDF(cell)
call CalcMatrices(cell)

!====================================
! ------ and open the output file (only thread 0 can write to this file)
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

!====================================
! new in Release 4.3: add a Manufacturer string (null terminated)
dataset = SC_Manufacturer
line2(1) = ''
line2(1) = 'EMsoft'
i = len(trim(line2(1)))
i = i+1
line2(1)(i:i) = C_NULL_CHAR
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
!====================================

! write the EMheader to the file
datagroupname = 'EBSD'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLfiles')

! read the text file and write the array to the file
dataset = SC_EMEBSDNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetTextFile EMEBSDNML')

call HDF_pop(HDF_head)

! create a NMLparameters group to write all the namelist entries into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLparameters')

call HDFwriteEBSDNameList(HDF_head, enl)

! and leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EMData')

! create the EBSD group and add a HDF_FileVersion attribbute to it 
hdferr = HDF_createGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EBSD')
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
dataset = SC_xtalname
allocate(stringarray(1))
stringarray(1)= trim(mcnl%xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetStringArray xtalname')

dataset = SC_numangles
hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger numangles')

! and add the Euler angles to the output file
allocate(eulerangles(3,numangles))
do i=1,numangles
  eulerangles(1:3,i) = qu2eu(angles%quatang(1:4,i))
end do
dataset = SC_Eulerangles
hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerangles, 3, numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloatArray2D Eulerangles')

! =====================================================
! end of HDF_FileVersion = 4.0 and above write statements
! =====================================================


! and we leave this group open for further data output from the main program loop ... 

!====================================
! generate the deformation matrix and its polar decomposition
!====================================
includeFmatrix = .FALSE.
if (enl%applyDeformation.eq.'y') then
  includeFmatrix = .TRUE.
! importantly, we need to transpose the input deformation tensor for the 
! computations to be performed correctly
  Fmatrix = transpose(enl%Ftensor)

  if (numangles.eq.1.AND.enl%Fframe.eq.'samp') then
    call Message('Ftensor in the sample frame:')
    call PrintMatrixd('F = ',Fmatrix) 
    call Message('')
    call Message('Converting Sample Frame to Crystal Frame')
    call Message('')
    gs2c = qu2om(angles%quatang(1:4,1))
    Fmatrix=matmul(matmul(gs2c,Fmatrix),transpose(gs2c))
    call Message('Ftensor in the crystal frame:')
    call PrintMatrixd('F = ',Fmatrix) 
  end if

  if (enl%Fframe.eq.'crys'.OR.numangles.eq.1.) then
  ! perform the polar decomposition on the deformation tensor
    call getPolarDecomposition(Fmatrix, Umatrix, Smatrix)
    call Message('')

  ! and convert the unitary matrix to a quaternion
    quF = conjg(om2qu(Umatrix))

    call Message('Polar Decomposition')
    call Message('  --> Unitary Matrix')
    call PrintMatrixd('U = ',Umatrix) 
    char10 = ''
    call print_orientation(init_orientation(quF,'qu'),'qu',char10)
    write(*,*) 'rotation angle = ',2.0*acos(quF(1))*180.D0/cPi
    call Message('  --> Stretch Matrix')
    call PrintMatrixd('S = ',Smatrix) 

  ! compute the effective lattice parameters for the given deformation, based on the 
  ! undeformed unit cell
    Gmatrix = matmul(matmul(transpose(Fmatrix),cell%dsm), transpose(matmul(transpose(Fmatrix),cell%dsm)) )
    call Message('Metric tensor for distorted cell:')
    call PrintMatrixd('Gdis=',Gmatrix)
    io_real(1:3) = (/ sqrt(Gmatrix(1,1)), sqrt(Gmatrix(2,2)), sqrt(Gmatrix(3,3)) /)
    call WriteValue('(a, b, c) = ',io_real,3)
    
    io_real(1:3) = (/ acos(Gmatrix(2,3)/sqrt(Gmatrix(2,2))/sqrt(Gmatrix(3,3)))*180.D0/cPi  , &
                      acos(Gmatrix(1,3)/sqrt(Gmatrix(1,1))/sqrt(Gmatrix(3,3)))*180.D0/cPi  , &
                      acos(Gmatrix(1,2)/sqrt(Gmatrix(1,1))/sqrt(Gmatrix(2,2)))*180.D0/cPi  /)
    call WriteValue('(alpha, beta, gamma) = ',io_real,3)
    call Message('')
  else
    call Message('Metric tensor unavailable')
  end if
! invert the deformation tensor and pass it on to the pattern computation routine
!  call mInvert(Fmatrix, Fmatrix_inverse, .FALSE.)
end if

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
  if (numangles.ge.ninbatch*nthreads) then 
    nbatches = numangles/(ninbatch*nthreads)
    nremainder = mod(numangles,ninbatch*nthreads)
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
    if (numangles.le.nthreads) then
      nthreads = 1
    end if
    nbatches = 0
    ninlastbatch = numangles/nthreads+1
    nlastremainder = numangles - (nthreads-1)*ninlastbatch
    if (nlastremainder.le.0) then 
      ninlastbatch = numangles/nthreads
      nlastremainder = numangles - (nthreads-1)*ninlastbatch
    end if
    nlastbatches = 1
    nextra = 0
    if (nlastremainder.gt.0) nextra = 1 
end if
  if (nbatches.ne.0) then
    io_int(1) = numangles 
    io_int(2) = ninbatch
    io_int(3) = nthreads
    io_int(4) = nbatches
    io_int(5) = nremainder
    call WriteValue('  OpenMP loop variables : ',io_int,5,"(I10,' = ',I4,' * ',I2,' * ',I4,' + ',I6)")
  end if
  if ((ninlastbatch.ne.0).and.(nextra.ne.0)) then
    io_int(1) = numangles - nbatches * nthreads * ninbatch
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

! and allocate the batchpatterns array for hyperslab writing [modified 8/25/17 for different output formats]
L = binx*biny
! make sure that correctsize is a multiple of 16; if not, make it so
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if

if (trim(bitmode).eq.'char') then 
  allocate(batchpatterns(binx,biny,ninbatch*nthreads),stat=istat)
end if
if (trim(bitmode).eq.'int') then 
  allocate(batchpatternsint(binx,biny,ninbatch*nthreads),stat=istat)
end if
if (trim(bitmode).eq.'float') then 
  allocate(batchpatterns32(binx,biny,ninbatch*nthreads),stat=istat)
end if
if (trim(bitmode).eq.'dict') then 
  allocate(batchpatterns32lin(correctsize,ninbatch*nthreads),stat=istat)
end if

!====================================
! here we also create a mask if necessary
  allocate(mask(binx,biny),masklin(binx*biny),stat=istat)
  mask = 1.0
  masklin = 1.0
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
    if (trim(bitmode).eq.'dict') then
      do j = 1,biny
        do i = 1,binx
          masklin((j-1)*binx+i) = mask(i,j)
        end do
      end do 
    end if
  end if

!====================================
! for dictionary computations, the patterns are usually rather small, so perhaps the explicit 
! energy sums can be replaced by an averaged approximate approach, in which all the energy bins
! are added together from the start, and all the master patterns are totaled as well...

! this option is obsolete and will be removed in a later version of the code.  [MDG 02/19/2019]
if (enl%energyaverage.eq.1) then
  allocate(acc_array(enl%numsx,enl%numsy))
  acc_array = sum(EBSDdetector%accum_e_detector,1)
  allocate(wf(EBSDMCdata%numEbins))
  wf = sum(sum(EBSDdetector%accum_e_detector,2),2)
  wf = wf/sum(wf)

! this is a straightforward sum; we should probably do a weighted sum instead
  npy = mpnl%npx
  allocate(master_arrayNH(-mpnl%npx:mpnl%npx,-npy:npy))
  allocate(master_arraySH(-mpnl%npx:mpnl%npx,-npy:npy))
  do k=Emin,Emax
    EBSDMPdata%mLPNH(-mpnl%npx:mpnl%npx,-npy:npy,k) = EBSDMPdata%mLPNH(-mpnl%npx:mpnl%npx,-npy:npy,k) * wf(k)
    EBSDMPdata%mLPSH(-mpnl%npx:mpnl%npx,-npy:npy,k) = EBSDMPdata%mLPSH(-mpnl%npx:mpnl%npx,-npy:npy,k) * wf(k)
  end do
  master_arrayNH = sum(EBSDMPdata%mLPNH,3)
  master_arraySH = sum(EBSDMPdata%mLPSH,3)
end if

!====================================
! determine the scale factor for the Lambert interpolation
scl = dble(mpnl%npx) 

!====================================
! define the integer parameter list for the CalcEBSDPatternSingleFull call
ipar(1) = enl%binning
ipar(2) = enl%numsx
ipar(3) = enl%numsy
ipar(4) = mpnl%npx
ipar(5) = mpnl%npx
ipar(6) = EBSDMCdata%numEbins
ipar(7) = EBSDMCdata%numEbins

!====================================
! set the number of OpenMP threads 
io_int(1) = nthreads
call WriteValue(' Setting number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(nthreads)

call Time_tick(tickstart)
call Time_tick(tick)

write (*,*) 'applying noise ? ', noise

!====================================
!====================================
do ibatch=1,totnumbatches

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL default(shared)  PRIVATE(TID,iang,i,j,istat,EBSDpattern,binned,idum,bpat,ma,mi,threadbatchpatterns,bpatint)&
!$OMP& PRIVATE(tmLPNH, tmLPSH, trgx, trgy, trgz, taccum, dims2, dims3, threadbatchpatternsint, threadbatchpatterns32)&
!$OMP& PRIVATE(binnedvec, threadbatchpatterns32lin)&
!$OMP& PRIVATE(Fmatrix_inverse, Fmatrix, gs2c)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

! initialize the random number generator for the Poison noise
  if (noise.eqv..TRUE.) then 
    idum = -1-TID               
  else
    idum = 0_K4B
  end if

! each thread needs a private copy of the master and accum arrays; not having
! those can produce poor scaling...
  dims2 = shape(EBSDdetector%rgx)
  allocate(trgx(dims2(1),dims2(2)), trgy(dims2(1),dims2(2)), trgz(dims2(1),dims2(2)))
  dims3 = shape(EBSDdetector%accum_e_detector)
  allocate(taccum(dims3(1),dims3(2),dims3(3)))
  dims3 = shape(EBSDMPdata%mLPNH)
  allocate(tmLPNH(dims3(1),dims3(2),dims3(3)), tmLPSH(dims3(1),dims3(2),dims3(3)))
! and copy the data in
  trgx = EBSDdetector%rgx
  trgy = EBSDdetector%rgy
  trgz = EBSDdetector%rgz
  taccum = EBSDdetector%accum_e_detector
  tmLPNH = EBSDMPdata%mLPNH
  tmLPSH = EBSDMPdata%mLPSH

! allocate the arrays that will hold the computed pattern
  allocate(binned(binx,biny),stat=istat)
  if (trim(bitmode).eq.'char') then 
    allocate(bpat(binx,biny),stat=istat)
  end if
  if (trim(bitmode).eq.'int') then 
    allocate(bpatint(binx,biny),stat=istat)
  end if
  if (trim(bitmode).eq.'dict') then 
    allocate(bpatint(binx,biny),stat=istat)
    allocate(binnedvec(correctsize),stat=istat)
  end if

! this array requires some care in terms of its size parameters...
  if ((singlebatch.eqv..TRUE.).AND.(ibatch.eq.totnumbatches)) then 
     if (TID.eq.nthreads-1) then
      if (trim(bitmode).eq.'char') then 
        allocate(threadbatchpatterns(binx,biny,nlastremainder),stat=istat)
      end if
      if (trim(bitmode).eq.'int') then 
        allocate(threadbatchpatternsint(binx,biny,nlastremainder),stat=istat)
      end if
      if (trim(bitmode).eq.'float') then 
        allocate(threadbatchpatterns32(binx,biny,nlastremainder),stat=istat)
      end if
      if (trim(bitmode).eq.'dict') then 
        allocate(threadbatchpatterns32lin(correctsize,nlastremainder),stat=istat)
      end if
    else
      if (trim(bitmode).eq.'char') then 
        allocate(threadbatchpatterns(binx,biny,ninlastbatch),stat=istat)
      end if
      if (trim(bitmode).eq.'int') then 
        allocate(threadbatchpatternsint(binx,biny,ninlastbatch),stat=istat)
      end if
      if (trim(bitmode).eq.'float') then 
        allocate(threadbatchpatterns32(binx,biny,ninlastbatch),stat=istat)
      end if
      if (trim(bitmode).eq.'dict') then 
        allocate(threadbatchpatterns32lin(correctsize,ninlastbatch),stat=istat)
      end if
    end if
  else
    if (trim(bitmode).eq.'char') then 
      allocate(threadbatchpatterns(binx,biny,ninbatch),stat=istat)
    end if
    if (trim(bitmode).eq.'int') then 
      allocate(threadbatchpatternsint(binx,biny,ninbatch),stat=istat)
    end if
    if (trim(bitmode).eq.'float') then 
      allocate(threadbatchpatterns32(binx,biny,ninbatch),stat=istat)
    end if
    if (trim(bitmode).eq.'dict') then 
      allocate(threadbatchpatterns32lin(correctsize,ninbatch),stat=istat)
    end if
  end if

  if (trim(bitmode).eq.'char') then 
    threadbatchpatterns = ' '
  end if
  if (trim(bitmode).eq.'int') then 
    threadbatchpatternsint = 0_irg
  end if
  if (trim(bitmode).eq.'float') then 
    threadbatchpatterns32 = 0.0
  end if
  if (trim(bitmode).eq.'dict') then 
    threadbatchpatterns32lin = 0.0
  end if

  do iang=istart(TID,ibatch),istop(TID,ibatch)
! convert the direction cosines to quaternions, include the 
! sample quaternion orientation, and then back to direction cosines...
! then convert these individually to the correct EBSD pattern location
    binned = 0.0
    Fmatrix = transpose(enl%Ftensor)
    if (enl%Fframe.eq.'samp') then
      gs2c = qu2om(angles%quatang(1:4,iang))
      Fmatrix=matmul(matmul(gs2c,Fmatrix),transpose(gs2c))
    end if
    call mInvert(Fmatrix, Fmatrix_inverse, .FALSE.)

    if (includeFmatrix.eqv..TRUE.) then 
     if (enl%includebackground.eq.'y') then
      call CalcEBSDPatternSingleFull(ipar,angles%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,Fmatrix_inverse,applynoise=idum)
     else
      call CalcEBSDPatternSingleFull(ipar,angles%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,Fmatrix_inverse,removebackground='y',applynoise=idum)
     end if
    else
     if (enl%includebackground.eq.'y') then
      call CalcEBSDPatternSingleFull(ipar,angles%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,applynoise=idum)
     else
      call CalcEBSDPatternSingleFull(ipar,angles%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,removebackground='y',applynoise=idum)
     end if
    end if

    if (enl%scalingmode .eq. 'gam') then
        binned = binned**enl%gammavalue
    end if

    if (trim(bitmode).eq.'dict') then  ! pre-process the patterns for dictionary indexing
! this step includes adaptive histogram equalization, masking, and normalization

! adaptive histogram equalization
        ma = maxval(binned)
        mi = minval(binned)
        bpatint = nint(((binned - mi)/ (ma-mi))*255.0)
        binned =  float(adhisteq(enl%nregions,binx,biny,bpatint))

! linearize the array and apply the mask
        binnedvec = 0.0
        do j= 1,biny
          do i = 1,binx
            binnedvec((j-1)*binx+i) = binned(i,j)
          end do
        end do

! apply circular mask and normalize
        binnedvec(1:L) = binnedvec(1:L) * masklin(1:L)
        binnedvec(1:correctsize) = binnedvec(1:correctsize)/vecnorm(binnedvec(1:correctsize))

! store in array for hyperslab writing
        threadbatchpatterns32lin(1:correctsize, iang-istart(TID,ibatch)+1) = binnedvec

    else  ! don't make a dictionary so no preprocessing of patterns... just intensity scaling to the correct range
      if (trim(bitmode).eq.'char') then 
        ma = maxval(binned)
        mi = minval(binned)
        binned = mask * ((binned - mi)/ (ma-mi))
        bpat = char(nint(bitrange*binned))

        threadbatchpatterns(1:binx,1:biny, iang-istart(TID,ibatch)+1) = bpat
      end if

      if (trim(bitmode).eq.'int') then 
        ma = maxval(binned)
        mi = minval(binned)
        binned = mask * ((binned - mi)/ (ma-mi))
        bpatint = nint(bitrange*binned)

        threadbatchpatternsint(1:binx,1:biny, iang-istart(TID,ibatch)+1) = bpatint
      end if
      
      if (trim(bitmode).eq.'float') then 
        threadbatchpatterns32(1:binx,1:biny, iang-istart(TID,ibatch)+1) = binned
      end if
    end if

  end do ! end of iang loop

! and now we write the threadbatchpatterns arrays to the main batch patterns array; we 
! need to intercept the special case when there are remainder patterns!
!$OMP CRITICAL
  if ((singlebatch.eqv..TRUE.).AND.(ibatch.eq.totnumbatches)) then 
    if (TID.eq.nthreads-1) then
      if (trim(bitmode).eq.'char') then 
        batchpatterns(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatterns(1:binx,1:biny, 1:nlastremainder)
      end if
      
      if (trim(bitmode).eq.'int') then 
        batchpatternsint(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatternsint(1:binx,1:biny, 1:nlastremainder)
      end if
      
      if (trim(bitmode).eq.'float') then 
        batchpatterns32(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatterns32(1:binx,1:biny, 1:nlastremainder)
      end if
      
      if (trim(bitmode).eq.'dict') then 
        batchpatterns32lin(1:correctsize,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatterns32lin(1:correctsize, 1:nlastremainder)
      end if

    else
      if (trim(bitmode).eq.'char') then 
        batchpatterns(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
          threadbatchpatterns(1:binx,1:biny, 1:ninlastbatch)
      end if
      
      if (trim(bitmode).eq.'int') then 
        batchpatternsint(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
          threadbatchpatternsint(1:binx,1:biny, 1:ninlastbatch)
      end if
      
      if (trim(bitmode).eq.'float') then 
        batchpatterns32(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
          threadbatchpatterns32(1:binx,1:biny, 1:ninlastbatch)
      end if

      if (trim(bitmode).eq.'dict') then 
        batchpatterns32lin(1:correctsize,TID*ninlastbatch+1:(TID+1)*ninlastbatch)=&
          threadbatchpatterns32lin(1:correctsize, 1:ninlastbatch)
      end if
    end if
  else
    if (trim(bitmode).eq.'char') then 
      batchpatterns(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatterns(1:binx,1:biny, 1:ninbatch)
    end if
    
    if (trim(bitmode).eq.'int') then 
      batchpatternsint(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatternsint(1:binx,1:biny, 1:ninbatch)
    end if
    
    if (trim(bitmode).eq.'float') then 
      batchpatterns32(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatterns32(1:binx,1:biny, 1:ninbatch)
    end if

    if (trim(bitmode).eq.'dict') then 
      batchpatterns32lin(1:correctsize, TID*ninbatch+1:(TID+1)*ninbatch) = &
         threadbatchpatterns32lin(1:correctsize, 1:ninbatch)
    end if

  end if
!$OMP END CRITICAL

!$OMP END PARALLEL

! here we write all the entries in the batchpatterns array to the HDF file as a hyperslab
! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0
! =====================================================
dataset = SC_EBSDpatterns
 !if (outputformat.eq.'bin') then
   if (trim(bitmode).eq.'dict') then 
     offset2 = (/ 0, (ibatch-1)*ninbatch*enl%nthreads /)
     hdims2 = (/ correctsize, numangles /)
     dims2 = (/ correctsize, patinbatch(ibatch) /)
     dim1 = patinbatch(ibatch)
   else
     offset = (/ 0, 0, (ibatch-1)*ninbatch*enl%nthreads /)
     hdims = (/ binx, biny, numangles /)
     dims3 = (/ binx, biny, patinbatch(ibatch) /)
     dim2 = patinbatch(ibatch)
   end if
   if (ibatch.eq.1) then
     if (trim(bitmode).eq.'char') then 
       hdferr = HDF_writeHyperslabCharArray3D(dataset, batchpatterns(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabCharArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'int') then 
       hdferr = HDF_writeHyperslabIntegerArray3D(dataset, batchpatternsint(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabIntegerArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'float') then 
       hdferr = HDF_writeHyperslabFloatArray3D(dataset, batchpatterns32(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabFloatArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'dict') then 
       hdferr = HDF_writeHyperslabFloatArray2D(dataset, batchpatterns32lin(1:correctsize,1:dim1), hdims2, offset2, &
                                              dims2, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabFloatArray2D EBSDpatterns')
     end if
   else
     if (trim(bitmode).eq.'char') then 
       hdferr = HDF_writeHyperslabCharArray3D(dataset, batchpatterns(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabCharArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'int') then 
       hdferr = HDF_writeHyperslabIntegerArray3D(dataset, batchpatternsint(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabIntegerArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'float') then 
       hdferr = HDF_writeHyperslabFloatArray3D(dataset, batchpatterns32(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabFloatArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'dict') then 
       hdferr = HDF_writeHyperslabFloatArray2D(dataset, batchpatterns32lin(1:correctsize,1:dim1), hdims2, offset2, &
                                              dims2, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabFloatArray3D EBSDpatterns')
     end if
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

datagroupname = "EBSD"
hdferr = HDF_openGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup EBSD')

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


end subroutine ComputeEBSDPatterns


!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputedeformedEBSDPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a energy-weighted EBSD patterns with different pattern centers and deformation states
!
!> @param enl name list
!> @param orpcdef angle/orientation/deformation structure
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
!> @date 05/22/14  MDG 4.2 slight modification of angle input file; update for new EMEBSDMaster file format
!> @date 06/24/14  MDG 5.0 removal of global variables; removal of namelist stuff; 
!> @date 03/09/15  MDG 5.1 added OpenMP functionality for final loop
!> @date 03/10/15  MDG 5.2 added 'bin' and 'gui' outputformat; added mask support for 'bin' outputformat
!> @date 03/14/15  MDG 5.3 attempt at speeding up the program by performing approximate energy sums (energyaverage = 1)
!> @date 03/20/15  MDG 5.4 corrected out-of-bounds error in EBSDpattern array
!> @date 04/07/15  MDG 5.5 added HDF-formatted output
!> @date 05/08/15  MDG 5.6 added support for hexagonal/trigonal master pattern interpolation
!> @date 09/01/15  MDG 6.0 changed Lambert patterns to Northern+Southern hemisphere...
!> @date 10/11/15  MDG 6.1 correction of error in saving of multiple patterns in GUI mode
!> @date 10/14/15  SS  6.2 bug fix in binning of pattern
!> @date 03/03/16  MDG 6.3 corrected error in energywf computation when energymin is not the lowest possible energy
!> @date 03/15/16  MDG 6.4 changed OpenMP do-loop mode to DYNAMIC; replaced CPU_TIME by omp_get_wtime().
!> @date 03/17/16  MDG 6.5 changed OpenMP mode again; added function call to EBSDmod; added private variables to OpenMP section
!> @date 04/18/16  SS  6.6 removed outputformat .eq. bin check while writing hyperslab
!> @date 11/15/16  MDG 6.7 added CrystalData output to HDF file and removed unused entries.
!> @date 02/07/17  MDG 6.8 corrected bug when number of Euler angle triplets was smaller than requested number of threads
!> @date 09/26/17  MDG 7.0 added ability to incorporate a deformation tensor in the pattern computation
!> @date 10/13/17  MDG 7.1 correction of deformation tensor code; tested for tetragonal, monoclinic and anorthic deformations
!> @date 12/20/17  MDG 7.2 added switch to turn off realistic background intensity profile
!> @date 02/22/18  MDG 8.0 new version that incorporates different pattern center and deformation tensor for each pattern
!> @date 04/03/18  MDG 8.1 rewrite with separated name lists and new more modular data structures
!--------------------------------------------------------------------------
subroutine ComputedeformedEBSDPatterns(enl, mcnl, mpnl, numangles, orpcdef, EBSDMCdata, EBSDMPdata, progname, nmldeffile)

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
use detectors
use EBSDmod
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
use math

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
type(EBSDMasterNameListType),INTENT(INOUT) :: mpnl
integer(kind=irg),INTENT(IN)            :: numangles
type(EBSDAnglePCDefType),pointer        :: orpcdef
type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
type(EBSDMPdataType),INTENT(INOUT)      :: EBSDMPdata
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile

! all geometrical parameters and filenames
real(kind=dbl)                          :: prefactor, qz(3)

! allocatable arrays
real(kind=sgl),allocatable              :: EBSDpattern(:,:), binned(:,:)        ! array with EBSD patterns
real(kind=sgl),allocatable              :: z(:,:)               ! used to store the computed patterns before writing to disk
real(kind=sgl),allocatable              :: energywf(:), eulerangles(:,:)

! arrays for each OpenMP thread
real(kind=sgl),allocatable              :: tmLPNH(:,:,:) , tmLPSH(:,:,:)
real(kind=sgl),allocatable              :: trgx(:,:), trgy(:,:), trgz(:,:)          ! auxiliary detector arrays needed for interpolation
real(kind=sgl),allocatable              :: taccum(:,:,:)

! quaternion variables
real(kind=dbl)                          :: qq(4), qq1(4), qq2(4), qq3(4)

! various items
integer(kind=irg)                       :: i, j, iang, jang, k, io_int(6), hdferr, dim2          ! various counters
integer(kind=irg)                       :: istat, ipar(7), tick, tock, tickstart
integer(kind=irg)                       :: nix, niy, binx, biny, nixp, niyp, maxthreads,nextra,ninlastbatch,nlastremainder     ! various parameters
integer(kind=irg)                       :: NUMTHREADS, TID   ! number of allocated threads, thread ID
integer(kind=irg)                       :: ninbatch, nbatches,nremainder,ibatch,nthreads,maskradius,nlastbatches, totnumbatches
integer(kind=irg),allocatable           :: istart(:,:), istop(:,:), patinbatch(:)

real(kind=sgl)                          :: bindx, ma, mi, tstart, tstop, io_real(3)
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second
integer(kind=irg),parameter             :: storemax = 20        ! number of EBSD patterns stored in one output block
integer(kind=irg)                       :: Emin, Emax      ! various parameters
real(kind=dbl)                          :: dc(3), scl, nel, emult           ! direction cosine array
real(kind=dbl)                          :: sx, dx, dxm, dy, dym, rhos, x         ! various parameters
real(kind=dbl)                          :: ixy(2), tmp

real(kind=sgl),allocatable              :: mask(:,:), lx(:), ly(:)
character(kind=c_char),allocatable      :: batchpatterns(:,:,:), bpat(:,:), threadbatchpatterns(:,:,:) 
integer(kind=irg),allocatable           :: batchpatternsint(:,:,:), bpatint(:,:), threadbatchpatternsint(:,:,:) 
real(kind=sgl),allocatable              :: batchpatterns32(:,:,:), threadbatchpatterns32(:,:,:) 
integer(kind=irg),allocatable           :: acc_array(:,:)
real(kind=sgl),allocatable              :: master_arrayNH(:,:), master_arraySH(:,:), wf(:) 
character(len=3)                        :: outputformat
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)

! parameter for random number generator
integer, parameter                      :: K4B=selected_int_kind(9)      ! used by ran function in math.f90
integer(K4B)                            :: idum

type(HDFobjectStackType)                :: HDF_head
type(unitcell)                          :: cell
integer(HSIZE_T), dimension(1:3)        :: hdims, offset 
integer(HSIZE_T)                        :: dims2(2), dims3(3)
character(fnlen,kind=c_char)            :: line2(1)
character(fnlen)                        :: groupname, dataset, datagroupname
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
character(10)                           :: char10
character(fnlen)                        :: datafile
logical                                 :: overwrite = .TRUE., insert = .TRUE., singlebatch
character(5)                            :: bitmode
integer(kind=irg)                       :: numbits
real(kind=sgl)                          :: bitrange

! new stuff: deformation tensor
real(kind=dbl)                          :: Umatrix(3,3), Fmatrix(3,3), Smatrix(3,3), quF(4), Fmatrix_inverse(3,3), &
                                           Gmatrix(3,3), gs2c(3,3)
logical                                 :: includeFmatrix=.FALSE.

!====================================
! max number of OpenMP threads on this platform
maxthreads = omp_get_max_threads()

nullify(HDF_head%next)
!nullify(cell)        

!====================================
! what is the output format?  GUI or BIN ?
outputformat = enl%outputformat

!====================================
! bit depth and format of output
call get_bit_parameters(enl%bitdepth, numbits, bitrange, bitmode)

! define some energy-related parameters derived from MC input parameters
!====================================
! make sure the requested energy range is within the range available from the Monte Carlo computation
if (enl%energymin.lt.mcnl%Ehistmin) enl%energymin = mcnl%Ehistmin
if (enl%energymax.gt.mcnl%EkeV) enl%energymax = mcnl%EkeV

! get the indices of the minimum and maximum energy
Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) +1
if (Emax.lt.1)  Emax=1
if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins

! modified by MDG, 03/26/18
nel = float(mcnl%totnum_el) * float(EBSDMCdata%multiplier)
emult = nAmpere * 1e-9 / nel  ! multiplicative factor to convert MC data to an equivalent incident beam of 1 nanoCoulomb
write (*,*) ' multiplicative factor to generate 1 nC of incident electrons ', emult

!====================================
! init a bunch of parameters
!====================================
! binned pattern array
  binx = enl%numsx/enl%binning
  biny = enl%numsy/enl%binning
  bindx = 1.0/float(enl%binning)**2

!====================================

!allocate(cell)        
cell%fname = trim(mcnl%xtalname)
call ReadDataHDF(cell)
call CalcMatrices(cell)

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
datagroupname = 'EBSD'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLfiles')

! read the text file and write the array to the file
dataset = SC_EMEBSDNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetTextFile EMEBSDNML')

call HDF_pop(HDF_head)

! create a NMLparameters group to write all the namelist entries into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLparameters')

call HDFwriteEBSDNameList(HDF_head, enl)

! and leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EMData')
hdferr = HDF_createGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EBSD')

dataset = SC_xtalname
allocate(stringarray(1))
stringarray(1)= trim(mcnl%xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetStringArray xtalname')

dataset = SC_numangles
hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger numangles')


! and add the Euler angles to the output file
allocate(eulerangles(3,numangles))
do i=1,numangles
  eulerangles(1:3,i) = qu2eu(orpcdef%quatang(1:4,i))
end do
dataset = SC_Eulerangles
hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerangles, 3, numangles, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloatArray2D Eulerangles')

! and we leave this group open for further data output from the main program loop ... 

!====================================
! in this particular routine we always include the deformation tensors, no matter what the nml file says
!====================================
includeFmatrix = .TRUE.
!====================================

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
  if (numangles.ge.ninbatch*nthreads) then 
    nbatches = numangles/(ninbatch*nthreads)
    nremainder = mod(numangles,ninbatch*nthreads)
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
    if (numangles.le.nthreads) then
      nthreads = 1
    end if
    nbatches = 0
    ninlastbatch = numangles/nthreads+1
    nlastremainder = numangles - (nthreads-1)*ninlastbatch
    if (nlastremainder.le.0) then 
      ninlastbatch = numangles/nthreads
      nlastremainder = numangles - (nthreads-1)*ninlastbatch
    end if
    nlastbatches = 1
    nextra = 0
    if (nlastremainder.gt.0) nextra = 1 
end if
  if (nbatches.ne.0) then
    io_int(1) = numangles 
    io_int(2) = ninbatch
    io_int(3) = nthreads
    io_int(4) = nbatches
    io_int(5) = nremainder
    call WriteValue('  OpenMP loop variables : ',io_int,5,"(I10,' = ',I4,' * ',I2,' * ',I4,' + ',I6)")
  end if
  if ((ninlastbatch.ne.0).and.(nextra.ne.0)) then
    io_int(1) = numangles - nbatches * nthreads * ninbatch
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

! and allocate the batchpatterns array for hyperslab writing [modified 8/25/17 for different output formats]
if (trim(bitmode).eq.'char') then 
  allocate(batchpatterns(binx,biny,ninbatch*nthreads),stat=istat)
end if
if (trim(bitmode).eq.'int') then 
  allocate(batchpatternsint(binx,biny,ninbatch*nthreads),stat=istat)
end if
if (trim(bitmode).eq.'float') then 
  allocate(batchpatterns32(binx,biny,ninbatch*nthreads),stat=istat)
end if

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
! determine the scale factor for the Lambert interpolation
scl = dble(mpnl%npx) 

!====================================
! define the integer parameter list for the CalcEBSDPatternSingleFull call
ipar(1) = enl%binning
ipar(2) = enl%numsx
ipar(3) = enl%numsy
ipar(4) = mpnl%npx
ipar(5) = mpnl%npx
ipar(6) = EBSDMCdata%numEbins
ipar(7) = EBSDMCdata%numEbins

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
!$OMP PARALLEL default(shared)  PRIVATE(TID,iang,i,j,istat,EBSDpattern,binned,idum,bpat,ma,mi,threadbatchpatterns,bpatint)&
!$OMP& PRIVATE(tmLPNH, tmLPSH, trgx, trgy, trgz, taccum, threadbatchpatternsint, threadbatchpatterns32, prefactor)&
!$OMP& PRIVATE(Fmatrix_inverse, nel, Fmatrix, gs2c)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

! each thread needs a private copy of the master and accum arrays; not having
! those can produce poor scaling... in addition, they need to be recomputed for each pattern !
  allocate(trgx(enl%numsx,enl%numsy), trgy(enl%numsx,enl%numsy), trgz(enl%numsx,enl%numsy))
  allocate(taccum(EBSDMCdata%numEbins,enl%numsx,enl%numsy))
  allocate(tmLPNH(enl%numsx,enl%numsy,EBSDMCdata%numEbins), tmLPSH(enl%numsx,enl%numsy,EBSDMCdata%numEbins))
! and copy the data in
  tmLPNH = EBSDMPdata%mLPNH
  tmLPSH = EBSDMPdata%mLPSH

! allocate the arrays that will hold the computed pattern
  allocate(binned(binx,biny),stat=istat)
  if (trim(bitmode).eq.'char') then 
    allocate(bpat(binx,biny),stat=istat)
  end if
  if (trim(bitmode).eq.'int') then 
    allocate(bpatint(binx,biny),stat=istat)
  end if

! this array requires some care in terms of its size parameters...
  if ((singlebatch.eqv..TRUE.).AND.(ibatch.eq.totnumbatches)) then 
     if (TID.eq.nthreads-1) then
      if (trim(bitmode).eq.'char') then 
        allocate(threadbatchpatterns(binx,biny,nlastremainder),stat=istat)
      end if
      if (trim(bitmode).eq.'int') then 
        allocate(threadbatchpatternsint(binx,biny,nlastremainder),stat=istat)
      end if
      if (trim(bitmode).eq.'float') then 
        allocate(threadbatchpatterns32(binx,biny,nlastremainder),stat=istat)
      end if
    else
      if (trim(bitmode).eq.'char') then 
        allocate(threadbatchpatterns(binx,biny,ninlastbatch),stat=istat)
      end if
      if (trim(bitmode).eq.'int') then 
        allocate(threadbatchpatternsint(binx,biny,ninlastbatch),stat=istat)
      end if
      if (trim(bitmode).eq.'float') then 
        allocate(threadbatchpatterns32(binx,biny,ninlastbatch),stat=istat)
      end if
    end if
  else
    if (trim(bitmode).eq.'char') then 
      allocate(threadbatchpatterns(binx,biny,ninbatch),stat=istat)
    end if
    if (trim(bitmode).eq.'16bit') then 
      allocate(threadbatchpatternsint(binx,biny,ninbatch),stat=istat)
    end if
    if (trim(bitmode).eq.'float') then 
      allocate(threadbatchpatterns32(binx,biny,ninbatch),stat=istat)
    end if
  end if

  if (trim(enl%bitdepth).eq.'char') then 
    threadbatchpatterns = ' '
  end if
  if (trim(enl%bitdepth).eq.'int') then 
    threadbatchpatternsint = 0_irg
  end if
  if (trim(enl%bitdepth).eq.'float') then 
    threadbatchpatterns32 = 0.0
  end if

  do iang=istart(TID,ibatch),istop(TID,ibatch)
! invert the transposed deformation tensor for this pattern
    Fmatrix = transpose(orpcdef%deftensors(1:3,1:3,iang))
    if (enl%Fframe.eq.'samp') then
      gs2c = qu2om(orpcdef%quatang(1:4,iang))
      Fmatrix=matmul(matmul(gs2c,Fmatrix),transpose(gs2c))
    end if
    call mInvert(Fmatrix, Fmatrix_inverse, .FALSE.)

! for each pattern we need to compute the detector arrays 
    if (enl%includebackground.eq.'y') then
      call GeneratemyEBSDDetector(enl, mcnl, EBSDMCdata, enl%numsx, enl%numsy, EBSDMCdata%numEbins, trgx, trgy, trgz, taccum, &
                                  orpcdef%pcs(1:3,iang),bg=.TRUE.)
! intensity prefactor
      prefactor = emult * enl%beamcurrent * enl%dwelltime * 1.0D-6
    else
      call GeneratemyEBSDDetector(enl, mcnl, EBSDMCdata, enl%numsx, enl%numsy, EBSDMCdata%numEbins, trgx, trgy, trgz, taccum, &
                                  orpcdef%pcs(1:3,iang),bg=.FALSE.)
! we pick a reasonable value here ...
      prefactor = 3.D0 * enl%beamcurrent * enl%dwelltime * 1.0D-6
    end if

    binned = 0.0
    
!   write (*,*) TID, nel, maxval(trgx), maxval(taccum), maxval(Fmatrix_inverse)

    if (includeFmatrix.eqv..TRUE.) then 
     if (enl%includebackground.eq.'y') then
      call CalcEBSDPatternSingleFull(ipar,orpcdef%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,Fmatrix_inverse)
     else
      call CalcEBSDPatternSingleFull(ipar,orpcdef%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,Fmatrix_inverse,removebackground='y')
     end if
    else
     if (enl%includebackground.eq.'y') then
      call CalcEBSDPatternSingleFull(ipar,orpcdef%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor)
     else
      call CalcEBSDPatternSingleFull(ipar,orpcdef%quatang(1:4,iang),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                     Emin,Emax,mask,prefactor,removebackground='y')
     end if
    end if

! from here on everything is the same as before...

    if (enl%scalingmode .eq. 'gam') then
        binned = binned**enl%gammavalue
    end if

    if (trim(bitmode).eq.'char') then 
      ma = maxval(binned)
      mi = minval(binned)
      binned = mask * ((binned - mi)/ (ma-mi))
      bpat = char(nint(bitrange*binned))

      threadbatchpatterns(1:binx,1:biny, iang-istart(TID,ibatch)+1) = bpat
    end if

    if (trim(bitmode).eq.'int') then 
      ma = maxval(binned)
      mi = minval(binned)
      binned = mask * ((binned - mi)/ (ma-mi))
      bpatint = nint(bitrange*binned)

      threadbatchpatternsint(1:binx,1:biny, iang-istart(TID,ibatch)+1) = bpatint
    end if
    
    if (trim(bitmode).eq.'float') then 
      threadbatchpatterns32(1:binx,1:biny, iang-istart(TID,ibatch)+1) = binned
    end if

  end do ! end of iang loop

! and now we write the threadbatchpatterns arrays to the main batch patterns array; we 
! need to intercept the special case when there are remainder patterns!
!$OMP CRITICAL
  if ((singlebatch.eqv..TRUE.).AND.(ibatch.eq.totnumbatches)) then 
    if (TID.eq.nthreads-1) then
      if (trim(bitmode).eq.'char') then 
        batchpatterns(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatterns(1:binx,1:biny, 1:nlastremainder)
      end if
      
      if (trim(bitmode).eq.'int') then 
        batchpatternsint(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatternsint(1:binx,1:biny, 1:nlastremainder)
      end if
      
      if (trim(bitmode).eq.'float') then 
        batchpatterns32(1:binx,1:biny,TID*ninlastbatch+1:TID*ninlastbatch+nlastremainder)=&
          threadbatchpatterns32(1:binx,1:biny, 1:nlastremainder)
      end if
      
    else
      if (trim(bitmode).eq.'char') then 
        batchpatterns(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
          threadbatchpatterns(1:binx,1:biny, 1:ninlastbatch)
      end if
      
      if (trim(bitmode).eq.'int') then 
        batchpatternsint(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
          threadbatchpatternsint(1:binx,1:biny, 1:ninlastbatch)
      end if
      
      if (trim(bitmode).eq.'float') then 
        batchpatterns32(1:binx,1:biny, TID*ninlastbatch+1:(TID+1)*ninlastbatch) = &
          threadbatchpatterns32(1:binx,1:biny, 1:ninlastbatch)
      end if
    end if
  else
    if (trim(bitmode).eq.'char') then 
      batchpatterns(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatterns(1:binx,1:biny, 1:ninbatch)
    end if
    
    if (trim(bitmode).eq.'int') then 
      batchpatternsint(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatternsint(1:binx,1:biny, 1:ninbatch)
    end if
    
    if (trim(bitmode).eq.'float') then 
      batchpatterns32(1:binx,1:biny, TID*ninbatch+1:(TID+1)*ninbatch) = threadbatchpatterns32(1:binx,1:biny, 1:ninbatch)
    end if
  end if
!$OMP END CRITICAL

!$OMP END PARALLEL

! here we write all the entries in the batchpatterns array to the HDF file as a hyperslab
dataset = SC_EBSDpatterns
 !if (outputformat.eq.'bin') then
   offset = (/ 0, 0, (ibatch-1)*ninbatch*enl%nthreads /)
   hdims = (/ binx, biny, numangles /)
   dims3 = (/ binx, biny, patinbatch(ibatch) /)
   dim2 = patinbatch(ibatch)
   if (ibatch.eq.1) then
     if (trim(bitmode).eq.'char') then 
       hdferr = HDF_writeHyperslabCharArray3D(dataset, batchpatterns(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabCharArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'int') then 
       hdferr = HDF_writeHyperslabIntegerArray3D(dataset, batchpatternsint(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabIntegerArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'float') then 
       hdferr = HDF_writeHyperslabFloatArray3D(dataset, batchpatterns32(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabFloatArray3D EBSDpatterns')
     end if
   else
     if (trim(bitmode).eq.'char') then 
       hdferr = HDF_writeHyperslabCharArray3D(dataset, batchpatterns(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabCharArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'int') then 
       hdferr = HDF_writeHyperslabIntegerArray3D(dataset, batchpatternsint(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabIntegerArray3D EBSDpatterns')
     end if
     if (trim(bitmode).eq.'float') then 
       hdferr = HDF_writeHyperslabFloatArray3D(dataset, batchpatterns32(1:binx,1:biny,1:dim2), hdims, offset, &
                                              dims3, HDF_head, insert)
       if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeHyperslabFloatArray3D EBSDpatterns')
     end if
   end if
 !end if

 io_int(1) = ibatch
 io_int(2) = totnumbatches
 call WriteValue('Completed cycle ',io_int,2,"(I4,' of ',I4)")

end do
!====================================
!====================================

tstop = Time_tock(tickstart) 
tock = Time_tock(tick)
tstop = tstop - tstart

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

datagroupname = "EBSD"
hdferr = HDF_openGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup EBSD')

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


end subroutine ComputedeformedEBSDPatterns
