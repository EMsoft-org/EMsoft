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
! array has 80 entries, of which about half are currently (April 2016) used.
!
! integer(kind=irg) :: ipar(wraparraysize)  components 
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
! ipar(30): useROI   (1 or 0)
! ipar(31): ROI1
! ipar(32): ROI2
! ipar(33): ROI3
! ipar(34): ROI4
! ipar(35): inputtype
! ipar(36): uniform  ['1' = yes (background only), '0' = no ]
! ipar(37): numexptsingle  (multiple of 16; number of expt patterns in one dot product chunk)
! ipar(38): numdictsingle  (multiple of 16; number of dict patterns in one dot product chunk)
! ipar(39): nnk (number of top matches to keep)
! ipar(40): totnumexpt     (number of experimental patterns in current batch)
! ipar(41): numexptsingle*ceiling(float(totnumexpt)/float(numexptsingle))  
! ipar(42): 16*ceiling(float(numsx*numsy)/16.0)
! ipar(43): neulers  (number of Euler angle triplets in the dictionary)
! ipar(44): nvariants (number of variants for refinement wrapper)
! ipar(45): nregionsmin
! ipar(46): nregionsstepsize
! ipar(47): numav
! ipar(48): patx
! ipar(49): paty
! ipar(50): numw (number of hipass parameters)
! ipar(51): numr (number of regions parameters)
! ipar(52:wraparraysize) : 0 (unused for now)

! real(kind=dbl) :: fpar(wraparraysize)  components
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
! fpar(24): hipasswd
! refinement parameters
! fpar(25): step 
! fpar(26): hipasswmax (user by EBSDDIpreview wrapper)
! fpar(27:wraparraysize): 0 (unused for now)

! newly added in version 3.2, to facilitate passing EMsoft configuration
! strings back and forth to C/C++ programs that call EMdymod routines...
! character(fnlen)  :: spar(wraparraysize)   configuration string components
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
! spar(28:wraparraysize): '' (unused for now)


module EMDIwrappermod

use local

! similar callback routine, with two integer arguments
ABSTRACT INTERFACE
   SUBROUTINE ProgressCallBackDI2(objAddress, loopCompleted, totalLoops) bind(C)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER(c_size_t),INTENT(IN), VALUE          :: objAddress
    INTEGER(KIND=4), INTENT(IN), VALUE           :: loopCompleted
    INTEGER(KIND=4), INTENT(IN), VALUE           :: totalLoops
   END SUBROUTINE ProgressCallBackDI2

   SUBROUTINE ProgressCallBackDI3(objAddress, loopCompleted, totalLoops, timeRemaining) bind(C)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER(c_size_t),INTENT(IN), VALUE          :: objAddress
    INTEGER(KIND=4), INTENT(IN), VALUE           :: loopCompleted
    INTEGER(KIND=4), INTENT(IN), VALUE           :: totalLoops
    REAL(KIND=4),INTENT(IN), VALUE               :: timeRemaining
   END SUBROUTINE ProgressCallBackDI3

   SUBROUTINE OpenCLErrorCallBackDI2(objAddress, errorCode) bind(C)
    USE, INTRINSIC :: ISO_C_BINDING
    INTEGER(c_size_t),INTENT(IN), VALUE          :: objAddress
    INTEGER(KIND=4), INTENT(IN), VALUE           :: errorCode
   END SUBROUTINE OpenCLErrorCallBackDI2
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
!> @param [OUTPUT] mask mask array 
!> @param [OUTPUT] exptIQ experimental pattern quality map
!> @param [OUTPUT] ADPmap average dot product map
!> @param cproc pointer to a C-function for the callback process
!> @param objAddress unique integer identifying the calling class in EMsoftWorkbench
!> @param cancel character defined by EMsoftWorkbench; when not equal to NULL (i.e., char(0)), the computation should be halted
!
!> @date 01/22/18 MDG 1.0 original extracted from EMEBSDDI program
!> @date 01/23/18 MDG 1.1 added callback routine and fixed compilation errors
!> @date 05/31/18 MDG 2.0 updated to new routine from patternmod module, including ROI handling and various pattern file formats
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
use patternmod

IMPLICIT NONE

integer(c_int32_t),INTENT(IN)           :: ipar(wraparraysize)
real(kind=sgl),INTENT(IN)               :: fpar(wraparraysize)
character(kind=c_char, len=1), target, INTENT(IN) :: spar(wraparraysize*fnlen)
real(kind=sgl),INTENT(INOUT)            :: mask(ipar(19), ipar(20))
real(kind=sgl),INTENT(INOUT)            :: exptIQ(ipar(26)*ipar(27))
real(kind=sgl),INTENT(INOUT)            :: ADPmap(ipar(26)*ipar(27))
TYPE(C_FUNPTR), INTENT(IN), VALUE       :: cproc
integer(c_size_t),INTENT(IN), VALUE     :: objAddress
character(len=1),INTENT(IN)             :: cancel

PROCEDURE(ProgressCallBackDI2), POINTER :: proc
character(fnlen)                        :: maskfile='', tmpfile='', exptname=''
logical                                 :: f_exists=.FALSE., ROIselected=.FALSE.
integer(kind=irg)                       :: ii=0, jj=0, kk=0, iii=0, maskpattern=0, numsx=0, numsy=0, L=0, binx=0, biny=0, &
                                           binning=0, ipf_wd=0, ipf_ht=0, TID=0, itmpexpt=0, iunitexpt=0, recordsize_correct=0, &
                                           ierr=0, correctsize=0, recordsize=0, patsz=0, nregions=0, totnumexpt=0, istat=0, &
                                           nthreads=0, ROI(4), i=0, iiiend=0, iiistart=0, jjend=0, dn=0, cn=0, totn=0, wd=0, ht=0, &
                                           nexpt=0, pindex=0
real(kind=sgl)                          :: mi=0.0, ma=0.0, vlen=0.0, tmp=0.0, maskradius=0.0, dp=0.0
real(kind=dbl)                          :: Jres=0.D0, w=0.D0
real(kind=sgl),allocatable              :: imageexpt(:), tmpimageexpt(:), imagedict(:), masklin(:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:), EBSDpat(:,:), exppatarray(:)
real(kind=dbl),allocatable              :: ksqarray(:,:), rrdata(:,:), ffdata(:,:)
complex(kind=dbl),allocatable           :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable   :: inp(:,:), outp(:,:)
real(kind=sgl),allocatable              :: lstore(:,:), pstore(:,:), lp(:), cp(:)


type(C_PTR)                             :: planf, HPplanf, HPplanb
character(fnlen)                        :: HDFstrings(10), fname='', inputtype=''
integer(HSIZE_T)                        :: dims3(3), offset3(3)


! parameters to deal with the input string array spar
type(ConfigStructureType)               :: CS

nullify(proc)

! link the proc procedure to the cproc argument
CALL C_F_PROCPOINTER (cproc, proc)

! outputs of this routine: all output arrays must be allocated in the calling program
!
! mask
! IQ map
! ADP map
!

! required input parameters:
!
! integers:
! nthreads              ---> ipar(18)
! numsx                 ---> ipar(19)
! numsy                 ---> ipar(20)
! binning               ---> ipar(22)
! binx                  ---> ipar(23)
! biny                  ---> ipar(24)
! ipf_wd                ---> ipar(26)
! ipf_ht                ---> ipar(27)
! nregions              ---> ipar(28)
! maskpattern           ---> ipar(29)
! useROI                ---> ipar(30)
! ROI(1)                ---> ipar(31)
! ROI(2)                ---> ipar(32)
! ROI(3)                ---> ipar(33)
! ROI(4)                ---> ipar(34)
! inputtype             ---> ipar(35)     2 = up1, 3 = up2, 4 = h5ebsd 

! floats:
! maskradius            ---> fpar(23) [pixels]
! w                     ---> fpar(24)

! strings:
! output file path      ---> CS%strvals(31)
! experimental data file---> CS%strvals(32)
! HDFstrings(1)         ---> CS%strvals(41)
! HDFstrings(2)         ---> CS%strvals(42)
! HDFstrings(3)         ---> CS%strvals(43)
! HDFstrings(4)         ---> CS%strvals(44)
! HDFstrings(5)         ---> CS%strvals(45)
! HDFstrings(6)         ---> CS%strvals(46)
! HDFstrings(7)         ---> CS%strvals(47)
! HDFstrings(8)         ---> CS%strvals(48)
! HDFstrings(9)         ---> CS%strvals(49)
! HDFstrings(10)        ---> CS%strvals(50)

! the calling program passes a c-string array spar that we need to convert to the 
! standard EMsoft config structure for use inside this routine
call C2F_configuration_strings(C_LOC(spar), CS)

! set up all the necessary variables and auxiliary arrays
L = ipar(19) * ipar(20) / ipar(22)**2
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if
recordsize = correctsize*4

binx = ipar(19) / ipar(22)
biny = ipar(20) / ipar(22)
nthreads = ipar(18)
patsz = correctsize
w = fpar(24)
iunitexpt =  110
itmpexpt = 111

! get the HDF group and dataset names (in case the input file is HDF5 format)
do i=1,10
  HDFstrings(i) = trim(CS%strvals(40+i))
end do 

! set up the Region-of-Interest handling
if (ipar(30).ne.0) then
  ROIselected = .TRUE.
  iiistart = ipar(32)
  iiiend = ipar(32)+ipar(34)-1
  jjend = ipar(33)
  ROI = (/ ipar(31), ipar(32), ipar(33), ipar(34) /)
else
  ROIselected = .FALSE.
  iiistart = 1
  iiiend = ipar(27) ! ipf_ht
  jjend = ipar(26)  ! ipf_wd
  ROI = (/ 0, 0, 0, 0 /)
end if

! define the mask if necessary
if (ipar(29).eq.1) then
  do ii = 1,biny
      do jj = 1,binx
          if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. fpar(23)**2) then
              mask(jj,ii) = 0.0
          end if
      end do
  end do
end if

select case(ipar(35))
  case(1) 
    inputtype = "Binary"
  case(2) 
    inputtype = "TSLup1"
  case(3) 
    inputtype = "TSLup2"
  case(4) 
    inputtype = "TSLHDF"
  case(5) 
    inputtype = "OxfordBinary"
  case(6) 
    inputtype = "OxfordHDF"
  case(7) 
    inputtype = "EMEBSD"
  case(8) 
    inputtype = "BrukerHDF"
end select

! convert the mask to a linear (1D) array
do ii = 1,biny
    do jj = 1,binx
        masklin((ii-1)*binx+jj) = mask(jj,ii)
    end do
end do

! deal with the output file name ...
f_exists = .FALSE.
fname = trim(CS%EMtmppathname)//trim(CS%strvals(31))
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname), exist=f_exists)

! delete the file if it already exists
if (f_exists) then
  open(unit=itmpexpt,file=trim(fname),&
       status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
  close(unit=itmpexpt,status='delete')
end if

! now open the new file for direct access mode
! this file will be used by the main dictionary indexing routine as well as the refinement routine.
open(unit=itmpexpt,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

!===================================================================================
! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(CS%strvals(32), ipar(26), L, inputtype, recordsize, iunitexpt, HDFstrings)
! if (istat.ne.0) then
!     call patternmod_errormessage(istat)
!     call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
! end if

! this next part is done with OpenMP, with only thread 0 doing the reading;
! Thread 0 reads one line worth of patterns from the input file, then all threads do 
! the work, and thread 0 adds them to the epatterns array in RAM; repeat until all patterns have been processed.

call OMP_SET_NUM_THREADS(nthreads)

! allocate the arrays that holds the experimental patterns from a single row of the region of interest
allocate(exppatarray(patsz * ipar(26)),stat=istat)
! if (istat .ne. 0) stop 'could not allocate exppatarray'

! prepare the fftw plan for this pattern size to compute pattern quality (pattern sharpness Q)
allocate(EBSDPat(binx,biny),stat=istat)
! if (istat .ne. 0) stop 'could not allocate arrays for EBSDPat filter'
EBSDPat = 0.0
allocate(ksqarray(binx,biny),stat=istat)
! if (istat .ne. 0) stop 'could not allocate ksqarray array'
Jres = 0.0
call init_getEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf)
deallocate(EBSDPat)

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
! if (istat .ne. 0) stop 'could not allocate hpmask array'
call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
deallocate(inp, outp)

dims3 = (/ binx, biny, ipar(26) /)

! set the call-back parameters and allocate the arrays for the ADPmap computation
dn = 1
if (ipar(30).eq.1) then 
  totn = ROI(4)
  cn = iiistart
  wd = ROI(3)
  ht = ROI(4)
  nexpt = ROI(3) * ROI(4)
else
  totn = ipar(27)
  cn = 1
  wd = ipar(26)
  ht = ipar(27)
  nexpt = ipar(26) * ipar(27)
end if
allocate(lstore(L,wd), pstore(L,wd), lp(L), cp(L), imageexpt(L))
ADPmap= 0.0
pstore = 0.0
lstore = 0.0
lp = 0.0
cp = 0.0
pindex = 0

!===================================================================================
! we do one row at a time
prepexperimentalloop: do iii = iiistart,iiiend

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat) &
!$OMP& PRIVATE(imageexpt, tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, vlen, tmp, inp, outp)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()

! initialize thread private variables
    allocate(EBSDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),tmpimageexpt(binx*biny),stat=istat)
    ! if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(EBSDpint(binx,biny),stat=istat)
    ! if (istat .ne. 0) stop 'could not allocate EBSDpint array'

    allocate(inp(binx,biny),outp(binx,biny),stat=istat)
    ! if (istat .ne. 0) stop 'could not allocate inp, outp arrays'

    tmpimageexpt = 0.0
    rrdata = 0.D0
    ffdata = 0.D0

! thread 0 reads the next row of patterns from the input file
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*ipar(26)/)
        if (ROIselected.eqv..TRUE.) then
            call getExpPatternRow(iii, ipar(26), patsz, L, dims3, offset3, iunitexpt, &
                                  inputtype, HDFstrings, exppatarray, ROI)
        else
            call getExpPatternRow(iii, ipar(26), patsz, L, dims3, offset3, iunitexpt, &
                                  inputtype, HDFstrings, exppatarray)
        end if
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER
    jj=0

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=1,jjend
! convert imageexpt to 2D EBSD Pattern array
        do kk=1,biny
          EBSDPat(1:binx,kk) = exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx)
        end do

! compute the pattern Image Quality 
        exptIQ((iii-iiistart)*jjend + jj) = sngl(computeEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf))

! Hi-Pass filter
        rrdata = dble(EBSDPat)
        ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), w, hpmask, inp, outp, HPplanf, HPplanb)
        EBSDPat = sngl(ffdata)

! adaptive histogram equalization
        ma = maxval(EBSDPat)
        mi = minval(EBSDPat)
    
        EBSDpint = nint(((EBSDPat - mi) / (ma-mi))*255.0)
        EBSDPat = float(adhisteq(ipar(28), binx, biny, EBSDpint))

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

! thread 0 writes the row of patterns to the file, and computes the ADP map
    if (TID.eq.0) then
      do jj=1,jjend
        write(itmpexpt,rec=(iii-iiistart)*jjend + jj) exppatarray((jj-1)*patsz+1:jj*patsz)
        imageexpt = exppatarray((jj-1)*patsz+1:jj*patsz)

        pindex = pindex + 1
    ! do we need to copy pstore into lstore ? (starts with second line)
        if ((jj.eq.1).and.(iii.gt.iiistart)) lstore = pstore
    ! determine to which dpmap entries we need to add the dot product
        if (jj.eq.1) then
          cp(1:L) = imageexpt(1:L)
          pstore(1:L,jj) = cp(1:L)
        else
          lp = cp
          cp(1:L) = imageexpt(1:L)
          pstore(1:L,jj) = cp(1:L)
          dp = 0.25 * sum(lp(1:L)*cp(1:L))
          ADPmap(pindex-1) = ADPmap(pindex-1) + dp
          ADPmap(pindex) = ADPmap(pindex) + dp
        end if
        if (jj.gt.1) then
          dp = 0.25 * sum(lstore(1:L,jj)*cp(1:L))
          ADPmap(pindex-wd+1) = ADPmap(pindex-wd+1) + dp
          ADPmap(pindex) = ADPmap(pindex) + dp
        end if
      end do

      ! correct the lower corners and lower edge when we are done with the second row
      if ((iii-iiistart).eq.1) then
        ADPmap(1) = ADPmap(1) * 4.0
        ADPmap(wd) = ADPmap(wd) * 2.0
        ADPmap(2:wd-1) = ADPmap(2:wd-1) * 4.0/3.0
      end if
      ! once we've completed the third row, we can correct the edge points of the previous row
      if ((iii-iiistart).ge.2) then
        ADPmap(wd*(iii-iiistart-1)+1) = ADPmap(wd*(iii-iiistart-1)+1) * 4.0/3.0
        ADPmap(wd*(iii-iiistart)) = ADPmap(wd*(iii-iiistart)) * 4.0/3.0
      end if
    end if

deallocate(tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, inp, outp)
!$OMP BARRIER
!$OMP END PARALLEL

! has the cancel flag been set by the calling program ?
  if (cancel.ne.char(0)) EXIT prepexperimentalloop

! provide an update of progress via the call back routine (number of rows completed out of total number of rows)
  if(objAddress.ne.0) then
    call proc(objAddress, cn, totn)
    cn = cn+dn
  end if

end do prepexperimentalloop

! do final corrections to ADPmap
! edge points of next to last line from the top
ADPmap(wd*(ht-2)+1) = ADPmap(wd*(ht-2)+1) * 4.0/3.0
ADPmap(wd*(ht-1)) = ADPmap(wd*(ht-1)) * 4.0/3.0

! upper corners and edge
ADPmap(nexpt) = ADPmap(nexpt) * 2.0
ADPmap(nexpt-wd+1) = ADPmap(nexpt-wd+1) * 4.0/3.0
ADPmap(nexpt-wd+2:nexpt-1) = ADPmap(nexpt-wd+2:nexpt-1) * 4.0/3.0

! close both files
call closeExpPatternFile(inputtype, iunitexpt)
close(unit=itmpexpt,status='keep')

! that's it folks...
end subroutine EMsoftCpreprocessEBSDPatterns



!--------------------------------------------------------------------------
!
! SUBROUTINE:EMsoftCpreprocessSingleEBSDPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This subroutine can be called by a C/C++ program as a standalone function to preprocess a single experimental EBSD pattern
!
!> @details This subroutine provides a method to preprocess a series of experimental EBSD patterns and
!> can be called from an external C/C++ program; the routine provides a callback mechanism to
!> update the calling program about computational progress, as well as a cancel option.
!> The routine is intended to be called form a C/C++ program, e.g., EMsoftWorkbench.  This routine is a portion
!> of the core of the EMEBSDDI program. 
!>
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param inputpattern unprocessed pattern
!> @param outputpattern processed pattern
!
!> @date 01/22/18 MDG 1.0 original extracted from EMEBSDDI program
!> @date 05/31/18 MDG 2.0 rewrite based on EMsoftCpreprocessEBSDPatterns call.
!--------------------------------------------------------------------------
recursive subroutine EMsoftCpreprocessSingleEBSDPattern(ipar, fpar, inputpattern, outputpattern) &
           bind(c, name='EMsoftCpreprocessSingleEBSDPattern')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCpreprocessSingleEBSDPattern

use local
use configmod
use constants
use typedefs
use iso_c_binding
use filters
use EBSDDImod
use omp_lib
use patternmod

IMPLICIT NONE

integer(c_int32_t),INTENT(IN)           :: ipar(wraparraysize)
real(kind=sgl),INTENT(IN)               :: fpar(wraparraysize)
real(kind=sgl),INTENT(IN)               :: inputpattern(ipar(19),ipar(20))
real(kind=sgl),INTENT(OUT)              :: outputpattern(ipar(19),ipar(20))

integer(kind=irg)                       :: binx=0, biny=0, istat=0
real(kind=sgl)                          :: mi=0.0, ma=0.0
integer(kind=irg),allocatable           :: EBSDpint(:,:)
real(kind=sgl),allocatable              :: EBSDpat(:,:)
real(kind=dbl),allocatable              :: rrdata(:,:), ffdata(:,:)
complex(kind=dbl),allocatable           :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable   :: inp(:,:), outp(:,:)

type(C_PTR)                             :: HPplanf, HPplanb

HPplanf = C_NULL_PTR
HPplanb = C_NULL_PTR

! output of this routine: all output arrays must be allocated in the calling program
!
! outputpattern
!

! required input parameters:
!
! integers:
! numsx                 ---> ipar(19)
! numsy                 ---> ipar(20)
! nregions              ---> ipar(28)  [1 .. 20]

! floats:
! w                     ---> fpar(24)  [in range [0 .. 0.5]]

! set up all the necessary variables and auxiliary arrays
binx = ipar(19)
biny = ipar(20)

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
call init_HiPassFilter(dble(fpar(24)), (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 

! initialize thread private variables
allocate(EBSDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),stat=istat)

allocate(EBSDpint(binx,biny),stat=istat)

! Hi-Pass filter
rrdata = dble(inputpattern)
EBSDPat = sngl( applyHiPassFilter(rrdata, (/ binx, biny /), dble(fpar(24)), hpmask, inp, outp, HPplanf, HPplanb) )

! adaptive histogram equalization
ma = maxval(EBSDPat)
mi = minval(EBSDPat)
    
EBSDpint = nint(((EBSDPat - mi) / (ma-mi))*255.0)
outputpattern = float(adhisteq(ipar(28), binx, biny, EBSDpint))

deallocate(inp, outp, EBSDpint, EBSDPat, rrdata, ffdata, hpmask)
HPplanf = C_NULL_PTR
HPplanb = C_NULL_PTR

! that's it folks...
end subroutine EMsoftCpreprocessSingleEBSDPattern



!--------------------------------------------------------------------------
!
! SUBROUTINE:EMsoftCEBSDDIpreview
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This subroutine can be called by a C/C++ program as a standalone function to preprocess a single experimental EBSD pattern
!
!> @details This subroutine provides a method to generate a matrix of preprocessed
!> patterns for a selected pattern.  This can be used to determine the appropriate
!> preprocessing parameters 
!>
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param spar array with string input parameters
!> @param averagedpattern average pattern
!> @param patternarray array of processed patterns
!
!> @date 06/05/19 MDG 1.0 original extracted from EMEBSDDIpreview program
!--------------------------------------------------------------------------
recursive subroutine EMsoftCEBSDDIpreview(ipar, fpar, spar, averagedpattern, patternarray) &
           bind(c, name='EMsoftCEBSDDIpreview')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCEBSDDIpreview

use local
use configmod
use constants
use typedefs
use iso_c_binding
use filters
use EBSDDImod
use omp_lib
use patternmod
use HDF5

IMPLICIT NONE

integer(c_int32_t),INTENT(IN)           :: ipar(wraparraysize)
real(kind=sgl),INTENT(IN)               :: fpar(wraparraysize)
character(kind=c_char, len=1), target, INTENT(IN) :: spar(wraparraysize*fnlen)
real(kind=sgl),INTENT(OUT)              :: averagedpattern(ipar(19),ipar(20))
real(kind=sgl),INTENT(OUT)              :: patternarray(ipar(50)*ipar(19),ipar(51)*ipar(20))

integer(kind=irg)                       :: i, j, numsx, numsy, ipf_wd, ipf_ht, nregion, &
                                           hipasswnsteps, nregionsmin, nregionsmax, binx, biny, &
                                           nregionsstepsize, numav, istat, ii, jj, kk, hdferr, ierr, &
                                           numr, numw, nx, ny, xoffset, yoffset, iunitexpt, L, patsz, &
                                           patx, paty, recordsize
real(kind=sgl)                          :: mi, ma, hipasswmax
real(kind=dbl)                          :: x, y, v2, val
character(fnlen)                        :: HDFstrings(10), inputtype=''
real(kind=sgl),allocatable              :: expt(:), sumexpt(:), pattern(:,:), pcopy(:,:), hpvals(:)
integer(kind=irg),allocatable           :: nrvals(:), pint(:,:), ppp(:,:)
integer(HSIZE_T)                        :: dims3(3), offset3(3)
type(C_PTR)                             :: HPplanf, HPplanb
complex(kind=dbl),allocatable           :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable   :: inp(:,:), outp(:,:)
real(kind=dbl),allocatable              :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)

! parameters to deal with the input string array spar
type(ConfigStructureType)               :: CS

! output of this routine: all output arrays must be allocated in the calling program
!
! averagedpattern
! patternarray
!

! required input parameters:
!
! integers:
! numsx                 ---> ipar(19)
! numsy                 ---> ipar(20)
! ipf_wd                ---> ipar(26)
! ipf_ht                ---> ipar(27)
! inputtype             ---> ipar(35)  2 = up1, 3 = up2, 4 = h5ebsd, etc... (see below) 
! nregionsmin           ---> ipar(45)
! nregionsstepsize      ---> ipar(46)
! numav                 ---> ipar(47)
! patx                  ---> ipar(48)
! paty                  ---> ipar(49)
! numw                  ---> ipar(50)  
! numr                  ---> ipar(51)

! floats:
! w                     ---> fpar(24)  [in range [0 .. 0.5]]
! hipasswmax            ---> fpar(26)

! strings:
! output file path      ---> CS%strvals(31)
! experimental data file---> CS%strvals(32)
! HDFstrings(1)         ---> CS%strvals(41)
! HDFstrings(2)         ---> CS%strvals(42)
! HDFstrings(3)         ---> CS%strvals(43)
! HDFstrings(4)         ---> CS%strvals(44)
! HDFstrings(5)         ---> CS%strvals(45)
! HDFstrings(6)         ---> CS%strvals(46)
! HDFstrings(7)         ---> CS%strvals(47)
! HDFstrings(8)         ---> CS%strvals(48)
! HDFstrings(9)         ---> CS%strvals(49)
! HDFstrings(10)        ---> CS%strvals(50)

! the calling program passes a c-string array spar that we need to convert to the 
! standard EMsoft config structure for use inside this routine
call C2F_configuration_strings(C_LOC(spar), CS)

call h5open_EMsoft(hdferr)

binx = ipar(19)
biny = ipar(20)
L = binx * biny
recordsize = 4 * L
patsz = L
patx = ipar(48)
paty = ipar(49)

! dimensions of the output array of preprocessed patterns
numw = ipar(50)
numr = ipar(51)
nx = numw * binx
ny = numr * biny

hipasswmax = fpar(26)
nregionsmin = ipar(45)
nregionsstepsize = ipar(46)
iunitexpt = 100

! get the HDF group and dataset names (in case the input file is HDF5 format)
do i=1,10
  HDFstrings(i) = trim(CS%strvals(40+i))
end do 

select case(ipar(35))
  case(1) 
    inputtype = "Binary"
  case(2) 
    inputtype = "TSLup1"
  case(3) 
    inputtype = "TSLup2"
  case(4) 
    inputtype = "TSLHDF"
  case(5) 
    inputtype = "OxfordBinary"
  case(6) 
    inputtype = "OxfordHDF"
  case(7) 
    inputtype = "EMEBSD"
  case(8) 
    inputtype = "BrukerHDF"
end select

! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getSingleExpPattern() routine to read a 
! pattern into the expt variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(CS%strvals(32), ipf_wd, L, inputtype, recordsize, iunitexpt, HDFstrings)
if (istat.ne.0) then ! return an error message of some kind
    ! call patternmod_errormessage(istat)
    ! call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
end if

! should we average patterns?
allocate(expt(patsz))
dims3 = (/ binx, biny, 1 /)
if (numav.ge.0) then
  allocate(sumexpt(patsz))
  sumexpt = 0.0
  jj = 0 
  do i=-numav,numav
    if ((patx+i.gt.0).and.(patx+i.lt.ipf_wd)) then
      do j=-numav,numav
        if ((paty+j.gt.0).and.(paty+j.lt.ipf_ht)) then
          offset3 = (/ 0, 0, (paty+j) * ipf_wd + (patx+i) /)
          call getSingleExpPattern(paty, ipf_wd, patsz, L, dims3, offset3, iunitexpt, inputtype, &
                                   HDFstrings, expt)
          sumexpt = sumexpt + expt
          jj = jj+1
        end if 
      end do 
    end if 
  end do
  sumexpt = sumexpt / float(jj)
end if

! and read the center pattern (again)
offset3 = (/ 0, 0, paty * ipf_wd + patx /)
call getSingleExpPattern(paty, ipf_wd, patsz, L, dims3, offset3, iunitexpt, inputtype, HDFstrings, expt)

! and close the pattern file
call closeExpPatternFile(inputtype, iunitexpt)

! close the HDF interface
call h5close_EMsoft(hdferr)

! turn it into a 2D pattern
allocate(pattern(binx, biny), pcopy(binx, biny), pint(binx,biny), ppp(binx,biny), stat=ierr)
if (numav.gt.0) then 
  do kk=1,biny
    pcopy(1:binx,kk) = sumexpt((kk-1)*binx+1:kk*binx)
  end do
else
  do kk=1,biny
    pcopy(1:binx,kk) = expt((kk-1)*binx+1:kk*binx)
  end do
end if

! here we copy the averaged pattern into the output array 
! that was allocated by the calling program ...
averagedpattern = pcopy

! ================
! ================
! next we generate the array of preprocessed patterns
! define the high-pass filter width array and the nregions array
allocate(nrvals(numr))
nrvals = nregionsmin + (/ ((i-1)*nregionsstepsize, i=1,numr) /)

! the array for the hi pass filter parameter is a non-linear progression
allocate(hpvals(numw))
do ii=1,numw
    hpvals(ii) = 2.0**(float(ii-1-numw)) * 2.0 * hipasswmax
end do

! next we need to set up the high-pass filter fftw plans
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny))
allocate(rrdata(binx,biny),ffdata(binx,biny))
call init_HiPassFilter(dble(hpvals(1)), (/numsx, numsy /), hpmask, inp, outp, HPplanf, HPplanb) 

! the outer loop goes over the hipass filter width and is displayed horizontally in the final image
do ii=1,numw
! Hi-Pass filter
    pattern = pcopy
    rrdata = dble(pattern)
    ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), dble(hpvals(ii)), hpmask, inp, outp, HPplanf, HPplanb)
    pattern = sngl(ffdata)

    ma = maxval(pattern)
    mi = minval(pattern)

    pint = nint(((pattern - mi) / (ma-mi))*255.0)
    xoffset = (ii-1) * binx + 1
    do jj=1,numr
! adaptive histogram equalization
        if (nrvals(jj).eq.0) then
            ppp = pint
        else
            ppp = adhisteq(nrvals(jj),binx,biny,pint)
        end if 

! and store the pattern in the correct spot in the patternarray array (flipped upside down !!!)
! TO BE VERIFIED: DO WE STILL NEED TO FLIP THE PATTERN UPSIDE DOWN ???
        yoffset =  (numr-jj) * biny + 1
        do i=1,binx
          do j=1,biny
           patternarray(xoffset+i-1, yoffset+j-1) = ppp(i,biny-j+1)
          end do
        end do
    end do

! regenerate the complex inverted Gaussian mask with the next value of the mask width
    hpmask = cmplx(1.D0,0.D0)
    do i=1,binx/2 
      x = dble(i)**2
      do j=1,biny/2
        y = dble(j)**2
        v2 = hpvals(ii) * ( x+y )
        if (v2.lt.30.D0) then
          val = 1.D0-dexp(-v2)
          hpmask(i,j) = cmplx(val, 0.D0)
          hpmask(binx+1-i,j) = cmplx(val, 0.D0)
          hpmask(i,biny+1-j) = cmplx(val, 0.D0)
          hpmask(binx+1-i,biny+1-j) = cmplx(val, 0.D0)
        end if
      end do
    end do
end do

! clean up all auxiliary arrays
deallocate(hpmask, inp, outp, pcopy, rrdata, ffdata, hpvals, nrvals)
deallocate(pattern, pint, ppp, expt)
if (allocated(sumexpt)) deallocate(sumexpt)

! this completes the computation of the patternarray output array

end subroutine EMsoftCEBSDDIpreview

! !--------------------------------------------------------------------------
! !
! ! SUBROUTINE:EMsoftCEBSDDI
! !
! !> @author Marc De Graef, Carnegie Mellon University
! !
! !> @brief This subroutine can be called by a C/C++ program as a standalone function to perform dictionary indexing of experimental EBSD patterns
! !
! !> @details This subroutine provides a method to index experimental EBSD patterns and
! !> can be called from an external C/C++ program; the routine provides a callback mechanism to
! !> update the calling program about computational progress, as well as a cancel option.
! !> The routine is intended to be called from a C/C++ program, e.g., EMsoftWorkbench.  This routine is a portion
! !> of the core of the EMEBSDDI program. 
! !>
! !> @param ipar array with integer input parameters
! !> @param fpar array with float input parameters
! !> @param spar array with string input parameters
! !> @param dpatterns dictionary pattern array (pre-processed)
! !> @param epatterns experimental pattern array (pre-processed)
! !> @param resultmain dot product array for top N matches
! !> @param indexmain array with indices of matches into the euler angle array
! !> @param cproc pointer to a C-function for the callback process
! !> @param cerrorproc pointer to a C-function for the OpenCL error callback process
! !> @param objAddress unique integer identifying the calling class in DREAM.3D
! !> @param cancel character defined by DREAM.3D; when not equal to NULL (i.e., char(0)), the computation should be halted
! !
! !> @date 08/17/18 MDG 1.0 original extracted from EMEBSDDImem program
! !--------------------------------------------------------------------------
recursive subroutine EMsoftCEBSDDI(ipar, fpar, spar, dpatterns, epatterns, resultmain, indexmain, &
                                   cproc, cerrorproc, objAddress, cancel) &
          bind(c, name='EMsoftCEBSDDI')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCEBSDDI

use local
use typedefs
use constants
use configmod
use dictmod
use Indexingmod
use error
use io
use others
use clfortran
use omp_lib
use timing
use ISO_C_BINDING

! ipar, fpar, and spar variables used in this wrapper routine:
! integers
! ipar(6) : devid
! ipar(7) : platid
! ipar(18): nthreads
! ipar(37): numexptsingle  (multiple of 16; number of expt patterns in one dot product chunk)
! ipar(38): numdictsingle  (multiple of 16; number of dict patterns in one dot product chunk)
! ipar(39): nnk (number of top matches to keep)
! ipar(40): totnumexpt     (number of experimental patterns in current batch)
! ipar(41): numexptsingle*ceiling(float(totnumexpt)/float(numexptsingle))  
! ipar(42): 16*ceiling(float(numsx*numsy)/16.0)
! ipar(43): neulers  (number of Euler angle triplets in the dictionary)

! no floats

! strings
! spar(23):  OpenCLpathname
!

IMPLICIT NONE

integer(c_int32_t),INTENT(IN)             :: ipar(wraparraysize)
real(kind=sgl),INTENT(IN)                 :: fpar(wraparraysize)
character(kind=c_char, len=1), target, INTENT(IN) :: spar(wraparraysize*fnlen)
real(kind=sgl),INTENT(INOUT)              :: dpatterns(ipar(42),ipar(43))   ! correctsize x 
real(kind=sgl),INTENT(INOUT)              :: epatterns(ipar(42),ipar(40))   ! correctsize x totnumexpt
real(kind=sgl),INTENT(INOUT)              :: resultmain(ipar(39),ipar(41))
integer(c_int32_t),INTENT(INOUT)          :: indexmain(ipar(39),ipar(41)) 
TYPE(C_FUNPTR), INTENT(IN), VALUE         :: cproc
TYPE(C_FUNPTR), INTENT(IN), VALUE         :: cerrorproc
integer(c_size_t),INTENT(IN), VALUE       :: objAddress
character(len=1),INTENT(IN)               :: cancel

integer(kind=irg)                         :: i, ii, jj, cratio, fratio, cratioE, fratioE, FZcnt, Nd, ierr, totnumexpt, Ne, pp, ll, &
                                             mm, correctsize, TID, iii, irec, numsx, numsy, L, nnk, istat, devid, platid, qq, &
                                             tickstart, tock
real(kind=sgl)                            :: ratio, ratioE, tstop, ttime
integer(kind=irg),allocatable             :: ppend(:), ppendE(:)
real(kind=sgl),pointer                    :: dict(:), results(:), dpsort(:)
integer(kind=irg),pointer                 :: indexlist(:), dpindex(:)
real(kind=sgl),allocatable, target        :: res(:), results1(:), results2(:), expt(:), dicttranspose(:), resultarray(:), &
                                             resulttmp(:,:)
real(kind=sgl),allocatable                :: tmpimageexpt(:)                                             
integer(kind=irg),allocatable,target      :: indexlist1(:),indexlist2(:),indexarray(:),indextmp(:,:)
PROCEDURE(ProgressCallBackDI3), POINTER   :: proc
PROCEDURE(OpenCLErrorCallBackDI2), POINTER:: errorproc
logical                                   :: returnPending

! OpenCL related variables
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
character(fnlen)                                    :: info, sourcefile ! info about the GPU
integer(c_int)                                      :: numd, nump
integer(c_size_t),target                            :: slength
integer(kind=8)                                     :: size_in_bytes_dict,size_in_bytes_expt
integer(c_intptr_t),target                          :: ctx_props(3)
integer(c_int64_t)                                  :: cmd_queue_props

! parameters to deal with the input string array spar
type(ConfigStructureType)                           :: CS

returnPending = .FALSE.

! link the proc procedure to the cproc argument
nullify(proc, errorproc)
CALL C_F_PROCPOINTER (cproc, proc)
CALL C_F_PROCPOINTER (cerrorproc, errorproc)

! the calling program passes a c-string array spar that we need to convert to the 
! standard EMsoft config structure for use inside this routine
call C2F_configuration_strings(C_LOC(spar), CS)

!================================
! initialize a number of variables based on the ipar and fpar arrays
!================================
FZcnt = ipar(43)
Nd = ipar(38)
Ne = ipar(37)
totnumexpt = ipar(40)
platid = ipar(7)
devid = ipar(6)
L = ipar(19)*ipar(20)
nnk = ipar(39)
correctsize = ipar(42)
size_in_bytes_dict = Nd*correctsize*sizeof(correctsize)
size_in_bytes_expt = Ne*correctsize*sizeof(correctsize)

!================================
! set the sizes of the blocks used in the indexing loops
!================================
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

!================================
!================================
!================================
! INITIALIZATION OF OpenCL DEVICE
!================================
! to allow for the wrapper to return with a reasonable OpenCL error code, we explicitly 
! initialize the OpenCL device by duplicating the CLinit_PDCCQ code here, but with the 
! CLerror_check routine replaced by the error callback routine

! get the platform ID
ierr = clGetPlatformIDs(0, C_NULL_PTR, nump)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

allocate(platform(nump))
ierr = clGetPlatformIDs(nump, C_LOC(platform), nump)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

if ((platid.gt.nump).and.(objAddress.ne.0)) then
  ierr = -32
  call errorproc(objAddress, ierr)
  return
end if 

! get the device ID
ierr =  clGetDeviceIDs(platform(platid), CL_DEVICE_TYPE_GPU, 0, C_NULL_PTR, numd)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 
allocate(device(numd))

ierr =  clGetDeviceIDs(platform(platid), CL_DEVICE_TYPE_GPU, numd, C_LOC(device), numd)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

if ((devid.gt.numd).and.(objAddress.ne.0)) then
  ierr = -33
  call errorproc(objAddress, ierr)
  return
end if 

! create the context
ctx_props(1) = CL_CONTEXT_PLATFORM
ctx_props(2) = platform(platid)
ctx_props(3) = 0
context = clCreateContext(C_LOC(ctx_props), numd, C_LOC(device),C_NULL_FUNPTR, C_NULL_PTR, ierr)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if

! set up the command queue
cmd_queue_props = CL_QUEUE_PROFILING_ENABLE
command_queue = clCreateCommandQueue(context, device(devid), cmd_queue_props, ierr)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if

! read the cl source file
sourcefile = trim(CS%OpenCLpathname)//'/DictIndx.cl'
! read the source file from the opencl folder
open(unit = dataunit, file = trim(sourcefile), access='direct', status = 'old', &
     action = 'read', iostat = ierr, recl = 1)
!if (ierr /= 0) call FatalError("CLread_source_file: ",'Cannot open file '//fname)

source = ''
irec = 1
do
  read(unit = dataunit, rec = irec, iostat = ierr) source(irec:irec)
  if (ierr /= 0) exit
  irec = irec + 1
end do
close(unit=dataunit)

csource = trim(source)
csource(irec:irec) = C_NULL_CHAR
slength = irec

! allocate device memory for experimental and dictionary patterns
cl_expt = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_expt, C_NULL_PTR, ierr)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

cl_dict = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dict, C_NULL_PTR, ierr)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

!================================
! the following lines were originally in the InnerProdGPU routine, but there is no need
! to execute them each time that routine is called so we move them here...
!================================
! create the program
pcnt = 1
psource = C_LOC(csource)
!prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(source_l), ierr)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

! build the program
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
! if(cnum > 1) call Message(trim(source(1:cnum))//'test',frm='(A)')
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 
if ((ierr2.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr2)
  return
end if 

! finally get the kernel and release the program
kernelname = 'InnerProd'
ckernelname = kernelname
ckernelname(10:10) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 

ierr = clReleaseProgram(prog)
if ((ierr.ne.0).and.(objAddress.ne.0)) then
  call errorproc(objAddress, ierr)
  return
end if 
! the remainder is done in the InnerProdGPU routine
!================================
!================================
!================================

!=========================================
! ALLOCATION AND INITIALIZATION OF ARRAYS
!=========================================
allocate(results1(Ne*Nd*cratioE), results2(Ne*Nd*cratioE), res(Ne*Nd), stat=istat)
!if (istat .ne. 0) stop 'Could not allocate array for results'
results1 = 0.0
results2 = 0.0
res = 0.0

allocate(dict(Nd*correctsize),dicttranspose(Nd*correctsize),stat=istat)
!if (istat .ne. 0) stop 'Could not allocate array for dictionary patterns'
dict = 0.0
dicttranspose = 0.0

allocate(expt(Ne*correctsize),stat=istat)
!if (istat .ne. 0) stop 'Could not allocate array for experimental patterns'
expt = 0.0

allocate(tmpimageexpt(correctsize),stat=istat)
tmpimageexpt = 0.0

allocate(resulttmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
!if (istat .ne. 0) stop 'could not allocate temporary result array'
resulttmp = -2.0

allocate(indextmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
!if (istat .ne. 0) stop 'could not allocate temporary index array'
indextmp = 0

allocate(indexlist1(1:Nd*(ceiling(float(FZcnt)/float(Nd)))),stat=istat)
allocate(indexlist2(1:Nd*(ceiling(float(FZcnt)/float(Nd)))),stat=istat)
!if (istat .ne. 0) stop 'could not allocate indexlist arrays'

indexlist1 = 0
indexlist2 = 0

do ii = 1,Nd*ceiling(float(FZcnt)/float(Nd))
    indexlist1(ii) = ii
    indexlist2(ii) = ii
end do

!================================
! main computation section
!================================
nullify(results)
nullify(indexlist)

call OMP_SET_NUM_THREADS(ipar(18))

dictionaryloop: do ii = 1,cratio+1

! if ii is odd, then we use results1/indexlist1 for the dot products computation, and 
! results2/indexlist2 for the sorting (assuming ii>1); when ii is even we switch the two pointers
       if (mod(ii,2).eq.1) then
         results => results1
         results = 0.0
         indexlist => indexlist1
         dpsort => results2   
         dpindex => indexlist2
       else
         results => results2
         results = 0.0
         indexlist => indexlist2
         dpsort => results1   
         dpindex => indexlist1
       end if

! copy the dictionary pattern block into a transposed array used for dot product computation
      if (ii.le.cratio) then
         dicttranspose = 0.0

         do pp = 1,ppend(ii)  !Nd or MODULO(FZcnt,Nd)
           dict((pp-1)*correctsize+1:pp*correctsize) = dpatterns(1:correctsize, (ii-1)*Nd+pp)
         end do
   
         do ll = 1,correctsize
           do mm = 1,Nd
             dicttranspose((ll-1)*Nd+mm) = dict((mm-1)*correctsize+ll)
           end do
         end do
      end if
      
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,iii,jj,ll,mm,pp,qq,ierr,resultarray,indexarray)

      TID = OMP_GET_THREAD_NUM()
      allocate(resultarray(Nd))
      allocate(indexarray(Nd))

! the master thread should be the one working on the GPU computation
!$OMP MASTER
   if (ii.le.cratio) then

      ierr = clEnqueueWriteBuffer(command_queue, cl_dict, CL_TRUE, 0_8, size_in_bytes_dict, C_LOC(dicttranspose(1)), &
                                  0, C_NULL_PTR, C_NULL_PTR)
      if ((ierr.ne.0).and.(objAddress.ne.0)) then
        call errorproc(objAddress, ierr)
        returnPending = .TRUE.
      end if 

      if (returnPending.eqv..FALSE.) then
        experimentalloop: do jj = 1,cratioE

          expt = 0.0
          res = 0.0

          do pp = 1,ppendE(jj)   ! Ne or MODULO(totnumexpt,Ne)
            tmpimageexpt(1:correctsize) = epatterns(1:correctsize,(jj-1)*Ne+pp)

            expt((pp-1)*correctsize+1:pp*correctsize) = tmpimageexpt(1:correctsize)
          end do

          ierr = clEnqueueWriteBuffer(command_queue, cl_expt, CL_TRUE, 0_8, size_in_bytes_expt, C_LOC(expt(1)), &
                                      0, C_NULL_PTR, C_NULL_PTR)
          if ((ierr.ne.0).and.(objAddress.ne.0)) then
            call errorproc(objAddress, ierr)
            returnPending = .TRUE.
            exit experimentalloop
          end if 

          call InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,res,numd,devid,kernel,context,command_queue)

  ! we will do the sorting of the dot products in the other threads; we will just use results and indexlist
  ! directly, without copying anything...  Use pointers to swap back and forth between the two versions of the 
  ! arrays
          results((jj-1)*Ne*Nd+1:jj*Ne*Nd) = res(1:Nd*Ne)
        end do experimentalloop
      end if

      if ((objAddress.ne.0).and.(mod(ii,10).eq.0)) then
! do a remaining time estimate
! and send information via the callback routine
        if (ii.eq.10) then
            tock = Time_tock(tickstart)
            ttime = float(tock) * float(cratio) / float(ii)
            tstop = ttime
            call proc(objAddress, ii, cratio, ttime)
        else
            ttime = tstop * float(cratio-ii) / float(cratio)
            call proc(objAddress, ii, cratio, ttime)
        end if
      end if
    end if 

!$OMP END MASTER

! has the cancel flag been set by the calling program ?
    if(cancel.ne.char(0)) returnPending = .TRUE.

! here we carry out the sorting of dot products, unless we are in the ii=1 step
    if ((ii.gt.1).and.(returnPending.eqv..FALSE.)) then
!$OMP DO SCHEDULE(DYNAMIC)
        do qq = 1,totnumexpt
          resultarray(1:Nd) = dpsort((qq-1)*Nd+1:qq*Nd)
          indexarray(1:Nd) = dpindex((ii-2)*Nd+1:(ii-1)*Nd)

          call SSORT(resultarray,indexarray,Nd,-2)
          resulttmp(nnk+1:2*nnk,qq) = resultarray(1:nnk)
          indextmp(nnk+1:2*nnk,qq) = indexarray(1:nnk)

          call SSORT(resulttmp(:,qq),indextmp(:,qq),2*nnk,-2)

          resultmain(1:nnk,qq) = resulttmp(1:nnk,qq)
          indexmain(1:nnk,qq) = indextmp(1:nnk,qq)
        end do
!$OMP END DO
    end if

    deallocate(indexarray, resultarray)
! and we end the parallel section here (all threads will synchronize).
!$OMP END PARALLEL

  if (returnPending.eqv..TRUE.) EXIT dictionaryloop

! has the cancel flag been set by the calling program ?
  if(cancel.ne.char(0)) EXIT dictionaryloop

end do dictionaryloop

! release the OpenCL kernel
ierr = clReleaseKernel(kernel)

! and deallocate some arrays
deallocate(ppend, ppendE, res, results1, results2, resulttmp, expt, dicttranspose, tmpimageexpt)
deallocate(indexlist1, indexlist2, indexarray, indextmp)
nullify(dict, results, dpsort, indexlist, dpindex)

end subroutine EMsoftCEBSDDI

! !--------------------------------------------------------------------------
! !
! ! SUBROUTINE:EMsoftCEBSDRefine
! !
! !> @author Marc De Graef, Carnegie Mellon University
! !
! !> @brief This subroutine can be called by a C/C++ program as a standalone function to perform dictionary indexing of experimental EBSD patterns
! !
! !> @details This subroutine provides a method to refine indexed experimental EBSD patterns and
! !> can be called from an external C/C++ program; the routine provides a callback mechanism to
! !> update the calling program about computational progress, as well as a cancel option.
! !> The routine is intended to be called from a C/C++ program, e.g., EMsoftWorkbench.  This routine is a portion
! !> of the core of the EMFitOrientationPS program. 
! !>
! !> @param ipar array with integer input parameters
! !> @param fpar array with float input parameters
! !> @param accum_e array with Monte Carlo histogram
! !> @param mLPNH Northern hemisphere master pattern
! !> @param mLPSH Southern hemisphere master pattern
! !> @param variants array with quaternions defining the potential pseudosymmetry variants float(4,nvars)
! !> @param epatterns array with pre-processed experimental patterns float(correctsize, totnumexpt)
! !> @param startEulers array with initial Euler angle triplets in radians  float(3,totnumexpt)
! !> @param startdps array with initial dot product values float(totnumexpt)
! !> @param eumain array with refined Euler angle triplets in radians float(3,totnumexpt)
! !> @param dpmain array with refined dot product values float(totnumexpt)
! !> @param cproc pointer to a C-function for the callback process
! !> @param objAddress unique integer identifying the calling class in DREAM.3D
! !> @param cancel character defined by DREAM.3D; when not equal to NULL (i.e., char(0)), the computation should be halted
! !
! !> @date 01/10/19 MDG 1.0 original extracted from EFitOrientationPS program
! !--------------------------------------------------------------------------
recursive subroutine EMsoftCEBSDRefine(ipar, fpar, accum_e, mLPNH, mLPSH, variants, epatterns, startEulers, &
                                       startdps, eumain, dpmain, cproc, objAddress, cancel) &
          bind(c, name='EMsoftCEBSDRefine')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCEBSDRefine

use local
use typedefs 
use NameListTypedefs
use crystal
use patternmod
use rotations
use constants
use detectors
use EBSDmod
use EBSDDImod
use omp_lib
use dictmod
use bobyqa_refinement,only:bobyqa
use FitOrientations,only:EMFitOrientationcalfunEBSD
use stringconstants

use ISO_C_BINDING

! ipar, fpar, and spar variables used in this wrapper routine:
! integers
! ipar(1) : nx  = (numsx-1)/2
! ipar(4) : totnum_el
! ipar(5) : multiplier
! ipar(10): SpaceGroupNumber
! ipar(12): numEbins
! ipar(17): npx
! ipar(18): nthreads
! ipar(19): numx of detector pixels
! ipar(20): numy of detector pixels
! ipar(26): ipf_wd
! ipar(27): ipf_ht
! ipar(29): maskpattern
! ipar(37): numexptsingle  (multiple of 16; number of expt patterns in one dot product chunk)
! ipar(38): numdictsingle  (multiple of 16; number of dict patterns in one dot product chunk)
! ipar(40): totnumexpt     (number of experimental patterns in current batch)
! ipar(41): numexptsingle*ceiling(float(totnumexpt)/float(numexptsingle))  
! ipar(42): 16*ceiling(float(numsx*numsy)/16.0)
! ipar(44): nvariants (number of variants for refinement wrapper)

! floats
! fpar(1) : sig
! fpar(2) : omega
! fpar(3) : EkeV
! fpar(4) : Ehistmin
! fpar(5) : Ebinsize
! fpar(15): pattern center x
! fpar(16): pattern center y
! fpar(17): scintillator pixel size
! fpar(18): detector tilt angle
! fpar(19): sample-scintillator distance
! fpar(20): beam current
! fpar(21): dwelltime
! fpar(22): gamma value
! fpar(23): maskradius
! fpar(25): step 

! no strings
!

IMPLICIT NONE

integer(c_int32_t),INTENT(IN)           :: ipar(wraparraysize)
real(kind=sgl),INTENT(IN)               :: fpar(wraparraysize)
integer(c_int32_t),INTENT(IN)           :: accum_e(ipar(12),-ipar(1):ipar(1),-ipar(1):ipar(1))
real(kind=sgl),INTENT(IN)               :: mLPNH(-ipar(17):ipar(17), -ipar(17):ipar(17), ipar(12))
real(kind=sgl),INTENT(IN)               :: mLPSH(-ipar(17):ipar(17), -ipar(17):ipar(17), ipar(12))
real(kind=sgl),INTENT(IN)               :: variants(4,ipar(44))
real(kind=sgl),INTENT(IN)               :: epatterns(ipar(42),ipar(40))   ! correctsize x totnumexpt
real(kind=sgl),INTENT(IN)               :: startEulers(3,ipar(40)) 
real(kind=sgl),INTENT(IN)               :: startdps(ipar(40)) 
real(kind=sgl),INTENT(INOUT)            :: eumain(3,ipar(40)) 
real(kind=sgl),INTENT(INOUT)            :: dpmain(ipar(40))
TYPE(C_FUNPTR), INTENT(IN), VALUE       :: cproc
integer(c_size_t),INTENT(IN), VALUE     :: objAddress
character(len=1),INTENT(IN)             :: cancel

type(MCCLNameListType)                  :: mcnl
type(EBSDNameListType)                  :: ebsdnl
type(EBSDMCdataType)                    :: EBSDMCdata
type(EBSDDetectorType)                  :: EBSDdetector

logical                                 :: stat
integer(kind=irg)                       :: ii, jj, kk, iii, istat, npx, npy, jjj
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second

integer(c_size_t),allocatable           :: LOCALIPAR(:)
real(kind=dbl),allocatable              :: X(:), XL(:), XU(:)
real(kind=sgl),allocatable              :: INITMEANVAL(:)
real(kind=dbl)                          :: RHOBEG, RHOEND
integer(kind=irg)                       :: NPT, N, IPRINT, NSTEP, NINIT
integer(kind=irg),parameter             :: MAXFUN = 10000
logical                                 :: verbose

integer(kind=irg)                       :: binx, biny, recordsize, pos(1), globalcount, numexptsingle
real(kind=sgl),allocatable              :: tmpimageexpt(:), imageexpt(:), mask(:,:), masklin(:)
integer(kind=8)                         :: size_in_bytes_dict,size_in_bytes_expt

real(kind=dbl)                          :: emult 
real(kind=sgl)                          :: quat(4), quat2(4), ma, mi, dp, tmp, vlen, avec(3), dtor, nel
real(kind=dbl)                          :: qu(4), rod(4) 
integer(kind=irg)                       :: Emin, Emax, nthreads, TID, io_int(2), tick, tock, ierr, L, nvar, niter
integer(kind=irg)                       :: ll, mm, Nexp, pgnum, FZcnt, nlines, dims2(2), correctsize, totnumexpt

real(kind=dbl)                          :: prefactor, F
real(kind=sgl),allocatable              :: dpPS(:), eulerPS(:,:)
real(kind=dbl)                          :: ratioE, eurfz(3), euinp(3)
integer(kind=irg)                       :: cratioE, fratioE, eindex, jjend, iiistart, iiiend, Nd, Ne, patsz, pp
integer(kind=irg),allocatable           :: ppendE(:)

real(kind=sgl),allocatable              :: STEPSIZE(:)
type(dicttype),pointer                  :: dict
integer(kind=irg)                       :: FZtype, FZorder
PROCEDURE(ProgressCallBackDI2), POINTER :: proc

! link the proc procedure to the cproc argument
nullify(proc)
CALL C_F_PROCPOINTER (cproc, proc)

! get a few parameters from the input parameter arrays
Ne = ipar(37)
Nd = ipar(38)
nthreads = ipar(18)
iiistart = 1
iiiend = ipar(27)
jjend = ipar(26)
totnumexpt = ipar(26) * ipar(27)
numexptsingle = ipar(37)
nvar = ipar(44)

! generate the detector arrays
ebsdnl%numsx = ipar(19)
ebsdnl%numsy = ipar(20)
ebsdnl%xpc = fpar(15)
ebsdnl%ypc = fpar(16)
ebsdnl%delta = fpar(17)
ebsdnl%thetac = fpar(18)
ebsdnl%L = fpar(19)
ebsdnl%energymin = fpar(4)
ebsdnl%energymax = fpar(3)
mcnl%sig = fpar(1)
mcnl%omega = fpar(2)
mcnl%numsx = 2*ipar(1)+1
mcnl%Ehistmin = fpar(4)
mcnl%Ebinsize = fpar(5)
EBSDMCdata%numEbins = ipar(12)

allocate(EBSDdetector%rgx(ebsdnl%numsx,ebsdnl%numsy), &
         EBSDdetector%rgy(ebsdnl%numsx,ebsdnl%numsy), &
         EBSDdetector%rgz(ebsdnl%numsx,ebsdnl%numsy), &
         EBSDdetector%accum_e_detector(ipar(12),ebsdnl%numsx,ebsdnl%numsy), stat=istat)

call GenerateEBSDDetector(ebsdnl, mcnl, EBSDMCdata, EBSDdetector, verbose)

!=====================================================
! get the indices of the minimum and maximum energy
!=====================================================
Emin = nint((ebsdnl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

Emax = nint((ebsdnl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) + 1
if (Emax .lt. 1) Emax = 1
if (Emax .gt. EBSDMCdata%numEbins) Emax = EBSDMCdata%numEbins

!=====================================================
! determine important size parameters
!=====================================================
binx = ebsdnl%numsx
biny = ebsdnl%numsy
recordsize = binx*biny*4
L = binx*biny
npx = ipar(17)
npy = npx

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

! initialize the parameter arrays for the refinement routine
allocate(LOCALIPAR(10))
LOCALIPAR = 0

! define the LOCALIPAR array
LOCALIPAR(1) = 1
LOCALIPAR(2) = binx
LOCALIPAR(3) = biny
LOCALIPAR(4) = ipar(17)
LOCALIPAR(5) = ipar(17)
LOCALIPAR(6) = EBSDMCdata%numEbins
LOCALIPAR(7) = EBSDMCdata%numEbins
LOCALIPAR(8) = Emin
LOCALIPAR(9) = Emax
LOCALIPAR(10)= 0

dims2 = (/binx, biny/)

! intensity prefactor
nel = float(ipar(4)) * float(ipar(5))
emult = nAmpere * 1e-9 / nel  ! multiplicative factor to convert MC data to an equivalent incident beam of 1 nanoCoulomb
! intensity prefactor 
prefactor = emult * fpar(20)* fpar(21) * 1.0D-6

! point group number 
pgnum = 0
do ii=1,32
  if (SGPG(ii).le.ipar(10)) pgnum = ii
end do

! allocate the dict structure
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

! initialize the symmetry matrices
call DI_Init(dict,'nil') 

! allocate arrays for the results
allocate(dpPS(nvar),eulerPS(3, nvar))

!=====================================================
!==========ALLOCATE ALL ARRAYS HERE=================== 
!=====================================================
allocate(mask(binx,biny),masklin(binx*biny))
mask = 1.0
masklin = 0.0

allocate(tmpimageexpt(binx*biny),imageexpt(binx*biny))
tmpimageexpt = 0.0
imageexpt = 0.0

!===============================================================
! define the circular mask if necessary and convert to 1D vector
!===============================================================
if (ipar(29).eq.1) then
  do ii = 1,biny
      do jj = 1,binx
          if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. fpar(23)**2) then
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
ratioE = float(totnumexpt)/float(numexptsingle)
cratioE = ceiling(ratioE)
fratioE = floor(ratioE)

ppendE = (/ (numexptsingle, ii=1,cratioE) /)
if (fratioE.lt.cratioE) then
  ppendE(cratioE) = MODULO(totnumexpt,numexptsingle)
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
STEPSIZE = fpar(25)
verbose = .FALSE.

call OMP_SET_NUM_THREADS(nthreads)

globalcount = 0

do iii = 1,cratioE
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,ii,tmpimageexpt,jj,quat,quat2,ma,mi,eindex) &
!$OMP& PRIVATE(ll,mm, X,INITMEANVAL,dpPS,eulerPS,eurfz,euinp,pos)
         
  TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC)
  do jj = 1,ppendE(iii)

!$OMP CRITICAL
    globalcount = globalcount+1
!$OMP END CRITICAL

    eindex = (iii - 1)*Ne + jj
    tmpimageexpt(1:correctsize) = epatterns(1:correctsize,eindex)
    tmpimageexpt = tmpimageexpt/NORM2(tmpimageexpt)

! calculate the dot product for each of the orientations in the neighborhood of the best match
! including the pseudosymmetric variant; do this for all the selected top matches (matchdepth)
    quat(1:4) = eu2qu(startEulers(1:3,eindex))

    do ll = 1,nvar
      quat2 = quat_mult(variants(1:4,ll), quat)
      euinp = qu2eu(quat2)
      call ReduceOrientationtoRFZ(euinp, dict, FZtype, FZorder, eurfz)
      quat2 = eu2qu(eurfz)

      INITMEANVAL(1:3) = eu2ho(eurfz(1:3)) 
      X = 0.5D0
      call bobyqa (LOCALIPAR, INITMEANVAL, tmpimageexpt, N, NPT, X, XL, XU, RHOBEG, RHOEND, IPRINT, MAXFUN, &
                   EMFitOrientationcalfunEBSD, EBSDdetector%accum_e_detector, mLPNH, mLPSH, mask, prefactor, &
                   EBSDdetector%rgx, EBSDdetector%rgy, EBSDdetector%rgz, STEPSIZE, fpar(22), verbose)
  
      eulerPS(1:3,ll) = ho2eu((/X(1)*2.0*STEPSIZE(1) - STEPSIZE(1) + INITMEANVAL(1), &
                                X(2)*2.0*STEPSIZE(2) - STEPSIZE(2) + INITMEANVAL(2), &
                                X(3)*2.0*STEPSIZE(3) - STEPSIZE(3) + INITMEANVAL(3)/)) 

      call EMFitOrientationcalfunEBSD(LOCALIPAR, INITMEANVAL, tmpimageexpt, EBSDdetector%accum_e_detector, &
                                      mLPNH, mLPSH, N, X, F, mask, prefactor, EBSDdetector%rgx, &
                                      EBSDdetector%rgy, EBSDdetector%rgz, STEPSIZE, fpar(22), verbose)

      dpPS(ll) = 1.D0 - F
    end do

! updating the confidence index only if a better match is found
    dp = maxval(dpPS)
    if (dp .gt. startdps(eindex)) then
      dpmain(eindex) = dp
      pos = maxloc(dpPS)
      eumain(1:3,eindex) = eulerPS(1:3,pos(1))
    else
      dpmain(eindex) = startdps(eindex)
      eumain(1:3,eindex) = startEulers(1:3,eindex) 
    end if

! callback section
      if ((objAddress.ne.0).and.(mod(globalcount,10).eq.0)) then
! send information via the callback routine
        call proc(objAddress, globalcount, totnumexpt)
      end if
  end do
!$OMP END DO
!$OMP END PARALLEL
end do

end subroutine EMsoftCEBSDRefine

end module EMDIwrappermod
