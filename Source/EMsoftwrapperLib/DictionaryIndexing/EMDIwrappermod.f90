! ###################################################################
! Copyright (c) 2013-2018, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMDIwrappermod.f90
!--------------------------------------------------------------------------
!
! MODULE: EMDIwrappermod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines that can be called by external code; 
!
!> @date  01/22/18 MDG 1.0 new C/C++ callable DI routines 
!--------------------------------------------------------------------------
! general information: the ipar and fpar arrays for all the routines that are C-callable
! are identical, so we document here their component definitions; to allow for future expansion, each
! array has 40 entries, of which about half are currently (April 2016) used.
!
! integer(kind=irg) :: ipar(40)  components 
! ipar(1) : nx  = (numsx-1)/2
! ipar(2) : globalworkgrpsz
! ipar(3) : num_el
! ipar(4) : totnum_el
! ipar(5) : multiplier
! ipar(6) : devid
! ipar(7) : platid
! ipar(8) : CrystalSystem
! ipar(9) : Natomtypes
! ipar(10): SpaceGroupNumber
! ipar(11): SpaceGroupSetting
! ipar(12): numEbins
! ipar(13): numzbins
! ipar(14): mcmode  ( 1 = 'full', 2 = 'bse1' )
! ipar(15): numangle
! ipar(16): nxten = nx/10
! the following are only used in the master routine
! ipar(17): npx
! ipar(18): nthreads
! the following are only used for EBSD patterns 
! ipar(19): numx of detector pixels
! ipar(20): numy of detector pixels
! ipar(21): number of orientation in quaternion set
! ipar(22): binning factor (0-3)
! ipar(23): binned x-dimension
! ipar(24): binned y-dimension
! ipar(25): anglemode  (0 for quaternions, 1 for Euler angles)
! ipar(26): ipf_wd
! ipar(27): ipf_ht
! ipar(28): nregions
! ipar(29): maskpattern
! ipar(30:40) : 0 (unused for now)

! real(kind=dbl) :: fpar(40)  components
! fpar(1) : sig
! fpar(2) : omega
! fpar(3) : EkeV
! fpar(4) : Ehistmin
! fpar(5) : Ebinsize
! fpar(6) : depthmax
! fpar(7) : depthstep
! fpar(8) : sigstart
! fpar(9) : sigend
! fpar(10): sigstep
! parameters only used in the master pattern routine
! fpar(11) : dmin
! fpar(12) : Bethe  c1
! fpar(13) : Bethe  c2
! fpar(14) : Bethe  c3
! parameters only used in the EBSD pattern routine
! fpar(15): pattern center x
! fpar(16): pattern center y
! fpar(17): scintillator pixel size
! fpar(18): detector tilt angle
! fpar(19): sample-scintillator distance
! fpar(20): beam current
! fpar(21): dwelltime
! fpar(22): gamma value
! fpar(23): maskradius
! fpar(24:40): 0 (unused for now)

! newly added in version 3.2, to facilitate passing EMsoft configuration
! strings back and forth to C/C++ programs that call EMdymod routines...
! character(fnlen)  :: spar(40)   configuration string components
! spar(1): EMsoftpathname
! spar(2): EMXtalFolderpathname
! spar(3): EMdatapathname
! spar(4): EMtmppathname
! spar(5): EMsoftLibraryLocation
! spar(6): EMSlackWebHookURL
! spar(7): EMSlackChannel
! spar(8): UserName
! spar(9): UserLocation
! spar(10): UserEmail
! spar(11): EMNotify
! spar(12): Develop
! spar(13): Release
! spar(14): h5copypath
! spar(15): EMsoftplatform
! spar(16): EMsofttestpath
! spar(17): EMsoftTestingPath
! spar(18): EMsoftversion
! spar(19): Configpath
! spar(20): Templatepathname
! spar(21): Resourcepathname
! spar(22): Homepathname
! spar(23): OpenCLpathname
! spar(24): Templatecodefilename
! spar(25): WyckoffPositionsfilename
! spar(26): Randomseedfilename
! spar(27): EMsoftnativedelimiter
! spar(28:40): '' (unused for now)


module EMDIwrappermod

! similar callback routine, with two integer arguments
ABSTRACT INTERFACE
   SUBROUTINE ProgressCallBackDI2(objAddress, loopCompleted, totalLoops) bind(C)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER(c_size_t),INTENT(IN), VALUE          :: objAddress
    INTEGER(KIND=4), INTENT(IN), VALUE           :: loopCompleted
    INTEGER(KIND=4), INTENT(IN), VALUE           :: totalLoops
   END SUBROUTINE ProgressCallBackDI2
END INTERFACE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:EMsoftCpreprocessEBSDPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This subroutine can be called by a C/C++ program as a standalone function to preprocess experimental EBSD patterns
!
!> @details This subroutine provides a method to preprocess a series of experimental EBSD patterns and
!> can be called from an external C/C++ program; the routine provides a callback mechanism to
!> update the calling program about computational progress, as well as a cancel option.
!> The routine is intended to be called form a C/C++ program, e.g., EMsoftWorkbench.  This routine is a portion
!> of the core of the EMEBSDDI program. 
!>
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param spar array with string input parameters
!> @param mask mask array (from file or circular)
!> @param exptIQ experimental pattern quality map
!> @param ADPmap average dot product map
!> @param cproc pointer to a C-function for the callback process
!> @param objAddress unique integer identifying the calling class in EMsoftWorkbench
!> @param cancel character defined by EMsoftWorkbench; when not equal to NULL (i.e., char(0)), the computation should be halted
!
!> @date 01/22/18 MDG 1.0 original extracted from EMEBSDDI program
!> @date 01/23/18 MDG 1.1 added callback routine and fixed compilation errors
!--------------------------------------------------------------------------
recursive subroutine EMsoftCpreprocessEBSDPatterns(ipar, fpar, spar, mask, exptIQ, ADPmap, cproc, objAddress, cancel) &
           bind(c, name='EMsoftCpreprocessEBSDPatterns')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCpreprocessEBSDPatterns

use local
use configmod
use constants
use typedefs
use iso_c_binding
use filters
use EBSDDImod
use omp_lib

IMPLICIT NONE

integer(c_int32_t),PARAMETER            :: nipar=40
integer(c_int32_t),PARAMETER            :: nfpar=40
integer(c_int32_t),PARAMETER            :: nspar=40
integer(c_int32_t),INTENT(IN)           :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
character(kind=c_char, len=1), target, INTENT(IN) :: spar(nspar*fnlen)
real(kind=sgl),INTENT(INOUT)            :: mask(ipar(19), ipar(20))
real(kind=sgl),INTENT(INOUT)            :: exptIQ(ipar(26)*ipar(27))
real(kind=sgl),INTENT(INOUT)            :: ADPmap(ipar(26)*ipar(27))
TYPE(C_FUNPTR), INTENT(IN), VALUE       :: cproc
integer(c_size_t),INTENT(IN), VALUE     :: objAddress
character(len=1),INTENT(IN)             :: cancel

PROCEDURE(ProgressCallBackDI2), POINTER :: proc
character(fnlen)                        :: maskfile, tmpfile, exptname
logical                                 :: f_exists
integer(kind=irg)                       :: ii, jj, kk, iii, maskpattern, numsx, numsy, L, binx, biny, binning, ipf_wd, ipf_ht, & 
                                           TID, itmpexpt, iunitexpt, recordsize_correct, ierr, correctsize, recordsize, patsz, &
                                           nregions, totnumexpt, istat, nthreads
real(kind=sgl)                          :: mi, ma, vlen, tmp, maskradius
real(kind=dbl)                          :: Jres, w
character(1000)                         :: charline
real(kind=sgl),allocatable              :: imageexpt(:), tmpimageexpt(:), imagedict(:), masklin(:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:), EBSDpat(:,:), exppatarray(:)
real(kind=dbl),allocatable              :: ksqarray(:,:), rrdata(:,:), ffdata(:,:)
complex(kind=dbl),allocatable           :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable   :: inp(:,:), outp(:,:)

type(C_PTR)                             :: planf, HPplanf, HPplanb


! parameters to deal with the input string array spar
type(ConfigStructureType)               :: CS



nullify(proc)

! link the proc procedure to the cproc argument
CALL C_F_PROCPOINTER (cproc, proc)


! outputs of this routine: all output arrays must be allocated in the calling program
!
! mask(binx,biny) (could be from file, or it is a simple circular mask with provided radius)
! IQ map
! ADP map
!

! required input parameters:
!
! binx, biny
! binning
! nthreads
! mask filename
! maskpattern           ---> ipar()
! maskfile              ---> CS%strvals(30)
! maskradius            ---> fpar()
! output file path      ---> CS%strvals(31)
! experimental file     ---> CS%strvals(32)
! nregions              ---> ipar()

! the calling program passes a c-string array spar that we need to convert to the 
! standard EMsoft config structure for use inside this routine
call C2F_configuration_strings(nspar, C_LOC(spar), CS)

! set up all the necessary variables and auxiliary arrays
maskfile = trim(CS%strvals(30))    ! calling program must ensure that this file exists if not 'undefined'
maskpattern = 0
if (ipar(29).eq.1) maskpattern = 1 
maskradius = fpar(23)**2
numsx = ipar(19)
numsy = ipar(20)
binning = ipar(22)
nthreads = ipar(18)
ipf_wd = ipar(26)
ipf_ht = ipar(27)
nregions = ipar(28)
binx = numsx/binning
biny = numsy/binning
patsz = numsx*numsy
totnumexpt = ipf_wd*ipf_ht
itmpexpt = 43

L = numsx*numsy/binning**2

! make sure that correctsize is a multiple of 16; if not, make it so
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if
recordsize_correct = correctsize*4
recordsize = L*4

! allocate auxiliary arrays
allocate(masklin(L))
allocate(EBSDpatterninteger(binx,biny))
EBSDpatterninteger = 0
allocate(EBSDpatternad(binx,biny),EBSDpatternintd(binx,biny))
EBSDpatternad = 0.0
EBSDpatternintd = 0.0

!=====================================================
! define the circular mask if necessary and convert to 1D vector
!=====================================================
if (trim(maskfile).ne.'undefined') then
! read the mask from file; the mask can be defined by a 2D array of 0 and 1 values
! that is stored in row form as strings, e.g.    
!    0000001110000000
!    0000011111000000
! ... etc
!
    f_exists = .FALSE.
    inquire(file=trim(maskfile), exist=f_exists)
!   if (f_exists.eqv..TRUE.) then  ! THIS TEST IS TO BE PERFORMED IN THE CALLING PROGRAM
      mask = 0.0
      open(unit=dataunit,file=trim(maskfile),status='old',form='formatted')
      do jj=biny,1,-1
        read(dataunit,"(A)") charline
        do ii=1,binx
          if (charline(ii:ii).eq.'1') mask(ii,jj) = 1.0
        end do
      end do
      close(unit=dataunit,status='keep')
!   else
!     call FatalError('MasterSubroutine','maskfile '//trim(fname)//' does not exist')
!   end if
else
    mask = 1.0
    if (maskpattern.eq.1) then
      do ii = 1,biny
          do jj = 1,binx
              if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. maskradius**2) then
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
! them in a temporary file as vectors; compute the IQ and ADP maps
! and make them available to the calling program 
!
!=====================================================
! first, make sure that this file does not already exist
f_exists = .FALSE.
tmpfile = trim(CS%strvals(31))
inquire(file=trim(tmpfile), exist=f_exists)

! if the file already exists, then we delete it
if (f_exists) then
  open(unit=itmpexpt,file=trim(tmpfile),&
      status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
  close(unit=itmpexpt,status='delete')
end if

! open the temporary file
open(unit=itmpexpt,file=trim(tmpfile),&
     status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)


! for the input data file with experimental images, we have either the standard binary format
! generate by a Matlb or IDL script, or we use HDF5 input; we need to determine automatically
! which flavor of input file we are dealing with.
!
! at this point in time, we only handle binary data; the full handling will first be tested in
! in the regular EMEBSDDI program before being implemented here...
exptname = trim(CS%strvals(32))

! datatype = getEBSDdataformat(exptname)

! we assume that this is a binary data file
open(unit=iunitexpt,file=trim(exptname),&
    status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)


! this next part is done with OpenMP, with only thread 0 doing the reading and writing,
! Thread 0 reads one line worth of patterns from the input file, then the threads do 
! the work, and thread 0 writes to the output file; repeat until all patterns have been processed.

call OMP_SET_NUM_THREADS(nthreads)

! allocate the arrays that hold the experimental patterns from a single row of the region of interest
allocate(exppatarray(numsx * numsy * ipf_wd),stat=istat)
!if (istat .ne. 0) stop 'could not allocate exppatarray'

! prepare the fftw plan for this pattern size to compute pattern quality (pattern sharpness Q)
allocate(EBSDPat(binx,biny),stat=istat)
!if (istat .ne. 0) stop 'could not allocate arrays for EBSDPat filter'
EBSDPat = 0.0
allocate(ksqarray(binx,biny),stat=istat)
!if (istat .ne. 0) stop 'could not allocate ksqarray array'
Jres = 0.0
call init_getEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf)
deallocate(EBSDPat)

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
!if (istat .ne. 0) stop 'could not allocate hpmask array'
call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
deallocate(inp, outp)

!=====================================================
! start the main preprocessing loop
!=====================================================
! we do one row at a time
prepexperimentalloop: do iii = 1,ipf_ht

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat) &
!$OMP& PRIVATE(imageexpt, tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, vlen, tmp, inp, outp)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()
! initialize thread private variables
    tmpimageexpt = 0.0
    allocate(EBSDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),stat=istat)
    !if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(EBSDpint(binx,biny),stat=istat)
    !if (istat .ne. 0) stop 'could not allocate EBSDpint array'

    allocate(inp(binx,biny),outp(binx,biny),stat=istat)
    !if (istat .ne. 0) stop 'could not allocate inp, outp arrays'

    rrdata = 0.D0
    ffdata = 0.D0

! thread 0 reads the next row of patterns from the input file
    if (TID.eq.0) then
      do jj=1,ipf_wd
        read(iunitexpt,rec=(iii-1)*ipf_wd + jj) imageexpt
        exppatarray((jj-1)*patsz+1:jj*patsz) = imageexpt(1:patsz)
      end do
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER
    jj=0

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=1,ipf_wd
! convert imageexpt to 2D EBS Pattern array
        do kk=1,biny
          EBSDPat(1:binx,kk) = exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx)
        end do

! compute the pattern Image Quality 
        exptIQ((iii-1)*ipf_wd + jj) = sngl(computeEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf))

! Hi-Pass filter
        rrdata = dble(EBSDPat)
        ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), w, hpmask, inp, outp, HPplanf, HPplanb)
        EBSDPat = sngl(ffdata)

! adaptive histogram equalization
        ma = maxval(EBSDPat)
        mi = minval(EBSDPat)
    
        EBSDpint = nint(((EBSDPat - mi) / (ma-mi))*255.0)
        EBSDPat = float(adhisteq(nregions,binx,biny,EBSDpint))

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
      do jj=1,ipf_wd
        write(itmpexpt,rec=(iii-1)*ipf_wd + jj) exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L)
      end do
    end if

deallocate(EBSDPat, rrdata, ffdata, EBSDpint, inp, outp)

!$OMP BARRIER
!$OMP END PARALLEL

! has the cancel flag been set by the calling program ?
  if (cancel.ne.char(0)) EXIT prepexperimentalloop

! update the progress counter and report it to the calling program via the proc callback routine
  if(objAddress.ne.0) then
    call proc(objAddress, iii, ipf_ht)
  end if

end do prepexperimentalloop

! close all the files
close(unit=iunitexpt,status='keep')
close(unit=itmpexpt,status='keep')

! and finally, compute the average dot product map from the pre-processed patterns
! re-open the temporary file
open(unit=itmpexpt,file=trim(exptname),&
     status='old',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
! use the getADPmap routine in the filters module
call getADPmap(itmpexpt, totnumexpt, L, ipf_wd, ipf_ht, ADPmap)
close(unit=itmpexpt,status='keep')

! deallocate anything that hasn't already been deallocated
deallocate(EBSDpatterninteger, EBSDpatternad, masklin, EBSDpatternintd, exppatarray, ksqarray, hpmask)

! that's it folks...
end subroutine EMsoftCpreprocessEBSDPatterns

!--------------------------------------------------------------------------
!
! SUBROUTINE:EMsoftCEBSDDI
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This subroutine can be called by a C/C++ program as a standalone function to perform dictionary indexing of experimental EBSD patterns
!
!> @details This subroutine provides a method to index experimental EBSD patterns and
!> can be called from an external C/C++ program; the routine provides a callback mechanism to
!> update the calling program about computational progress, as well as a cancel option.
!> The routine is intended to be called form a C/C++ program, e.g., EMsoftWorkbench.  This routine is a portion
!> of the core of the EMEBSDDI program. 
!>
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param spar array with string input parameters
!> @param mask mask array (from file or circular)
!> @param exptIQ experimental pattern quality map
!> @param ADPmap average dot product map
!> @param cproc pointer to a C-function for the callback process
!> @param objAddress unique integer identifying the calling class in DREAM.3D
!> @param cancel character defined by DREAM.3D; when not equal to NULL (i.e., char(0)), the computation should be halted
!
!> @date 01/23/18 MDG 1.0 original extracted from EMEBSDDI program
!--------------------------------------------------------------------------
recursive subroutine EMsoftCEBSDDI(ipar, fpar, spar, mask, exptIQ, ADPmap, cproc, objAddress, cancel) &
           bind(c, name='EMsoftCEBSDDI')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCEBSDDI

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
use EBSDiomod
use NameListHDFwriters
use ECPmod, only: GetPointGroup
use Indexingmod
use ISO_C_BINDING

IMPLICIT NONE

integer(c_int32_t),PARAMETER            :: nipar=40
integer(c_int32_t),PARAMETER            :: nfpar=40
integer(c_int32_t),PARAMETER            :: nspar=40
integer(c_int32_t),INTENT(IN)           :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
character(kind=c_char, len=1), target, INTENT(IN) :: spar(nspar*fnlen)
real(kind=sgl),INTENT(INOUT)            :: mask(ipar(19), ipar(20))
real(kind=sgl),INTENT(INOUT)            :: exptIQ(ipar(26)*ipar(27))
real(kind=sgl),INTENT(INOUT)            :: ADPmap(ipar(26)*ipar(27))
TYPE(C_FUNPTR), INTENT(IN), VALUE       :: cproc
integer(c_size_t),INTENT(IN), VALUE     :: objAddress
character(len=1),INTENT(IN)             :: cancel




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
patsz = ebsdnl%numsx*ebsdnl%numsy
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


! get the total number of electrons on the detector
totnum_el = sum(acc%accum_e_detector)

!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
pgnum = GetPointGroup(ebsdnl%MCxtalname)

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

allocate(mLPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ebsdnl%nE))
allocate(mLPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npy:ebsdnl%npy,ebsdnl%nE))
allocate(accum_e_MC(ebsdnl%numEbins,ebsdnl%numsx,ebsdnl%numsy),stat=istat)
accum_e_MC = acc%accum_e_detector
mLPNH = master%mLPNH
mLPSH = master%mLPSH

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
call sampleRFZ(ncubochoric, pgnum, 0, FZcnt, FZlist)

! allocate and fill FZarray for OpenMP parallelization
allocate(FZarray(4,FZcnt),stat=istat)
FZarray = 0.0

FZtmp => FZlist
do ii = 1,FZcnt
    FZarray(1:4,ii) = FZtmp%rod(1:4)
    FZtmp => FZtmp%next
end do

!================================
! INITIALIZATION OF OpenCL DEVICE
!================================
call CLinit_PDCCQ(platform, nump, platid, device, numd, devid, info, context, command_queue)

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

allocate(masklin(L),stat=istat)
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
! convert the circular mask to 1D vector
!=====================================================
! convert the mask to a linear (1D) array
do ii = 1,biny
    do jj = 1,binx
        masklin((ii-1)*binx+jj) = mask(jj,ii)
    end do
end do


! open the itmpexpt file, since we'll be reading preprocessed patterns from it...

!=====================================================
! MAIN COMPUTATIONAL LOOP
!=====================================================

call OMP_SET_NUM_THREADS(nthreads)

! define the jpar array of integer parameters
jpar(1) = binning
jpar(2) = numsx
jpar(3) = numsy
jpar(4) = npx
jpar(5) = npy
jpar(6) = numEbins
jpar(7) = nE


dictionaryloop: do ii = 1,cratio+1
    results = 0.0

! if ii is odd, then we use dict1 for the dictionary computation, and dict2 for the GPU
! (assuming ii>1); when ii is even we switch the two pointers 
    if (mod(ii,2).eq.1) then
      dict => dict1
      dict1 = 0.0
      T0dict => dict2   ! these are the patterns to be sent to the GPU
    else
      dict => dict2
      dict2 = 0.0
      T0dict => dict1   ! these are the patterns to be sent to the GPU
    end if

! start parallel processing
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,iii,jj,ll,mm,pp,ierr,io_int) &
!$OMP& PRIVATE(binned, ma, mi, EBSDpatternintd, EBSDpatterninteger, EBSDpatternad, quat, imagedictflt,imagedictfltflip)

      TID = OMP_GET_THREAD_NUM()

! the master thread should be the one working on the GPU computation
!$OMP MASTER
    if (ii.gt.1) then
      iii = ii-1        ! the index ii is already one ahead, since the GPU thread lags one cycle behind the others...

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

! here we need the cancel and callback routines !!!!!


    end if  ! ii.gt.1

!$OMP END MASTER


! here we carry out the dictionary pattern computation, unless we are in the ii=cratio+1 step
    if (ii.lt.cratio+1) then

!$OMP DO SCHEDULE(DYNAMIC)

     do pp = 1,ppend(ii)  !Nd or MODULO(FZcnt,Nd)
       binned = 0.0
       quat = ro2qu(FZarray(1:4,(ii-1)*Nd+pp))

       call CalcEBSDPatternSingleFull(jpar,quat,accum_e_MC,mLPNH,mLPSH,master%rgx,&
                                        master%rgy,master%rgz,binned,Emin,Emax,mask,prefactor)

       if (ebsdnl%scalingmode .eq. 'gam') then
         binned = binned**ebsdnl%gammavalue
       end if

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
! and we end the parallel section here (all threads will synchronize).
!$OMP END PARALLEL

end do dictionaryloop

! we may not need to delete the temporary file; depends on the user selection
close(itmpexpt,status='delete')

! release the OpenCL kernel
ierr = clReleaseKernel(kernel)
call CLerror_check('InnerProdGPU:clReleaseKernel', ierr)

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

! finally, deallocate everything that hasn't already been deallocated 
deallocate()

end subroutine EMsoftCEBSDDI



end module EMDIwrappermod