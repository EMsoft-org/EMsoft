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


module EMDIwrappermod



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
!>
!> @param EBSDpattern output array
!> @param quats quaternion input array
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!> @param cproc pointer to a C-function for the callback process
!> @param objAddress unique integer identifying the calling class in DREAM.3D
!> @param cancel character defined by DREAM.3D; when not equal to NULL (i.e., char(0)), the computation should be halted
!
!> @date 01/22/18 MDG 1.0 original extracted from EMEBSDDI program
!--------------------------------------------------------------------------
recursive subroutine EMsoftCpreprocessEBSDPatterns(ipar, fpar, spar, EBSDpattern, quats, accum_e, mLPNH, mLPSH, cproc, objAddress, cancel) &
           bind(c, name='EMsoftCpreprocessEBSDPatterns')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: EMsoftCpreprocessEBSDPatterns

use local
use configmod
use constants
use typedefs
use iso_c_binding
use EBSDDImod
use filters
use omp_lib

IMPLICIT NONE

integer(c_int32_t),PARAMETER            :: nipar=40
integer(c_int32_t),PARAMETER            :: nfpar=40
integer(c_int32_t),PARAMETER            :: nspar=40
integer(c_int32_t),INTENT(IN)           :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
character(kind=c_char, len=1), target, INTENT(IN) :: spar(nspar*fnlen)
mask
exptIQ
ADPmap


character(fnlen)                        :: maskfile, tmpfile, exptname
logical                                 :: f_exists
integer(kind=irg)                       :: ii, jj, kk, iii, maskpattern, numsx, numsy, L, binx, biny, binning, ipf_wd, ipf_ht, TID 
integer(kind=irg)                       :: itmpexpt, iunitexpt, recordsize_correct, ierr, correctsize, recordsize, patsz, nregions
real(kind=sgl)                          :: mi, ma, vlen, tmp
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
if (ipar().eq.1) maskpattern = 1 
maskradius = fpar()**2
numsx = ipar()
numsy = ipar()
binning = ipar()
nthreads = ipar()
ipf_wd = ipar()
ipf_ht = ipar()
nregions = ipar()
binx = numsx/binning
biny = numsy/binning
patsz = numsx*numsy

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

! use a callback routine to provide some progress feedback
    if (mod(iii,5).eq.0) then
        io_int(1:2) = (/ iii, ebsdnl%ipf_ht /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
    end if
end do prepexperimentalloop

call Message(' -> experimental patterns stored in tmp file')

close(unit=iunitexpt,status='keep')
close(unit=itmpexpt,status='keep')

! print some timing information
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(ebsdnl%nthreads) * float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,/)")

!=====================================================
call Message(' -> computing Average Dot Product map (ADP)')
call Message(' ')

allocate(dpmap(totnumexpt))
! re-open the temporary file
open(unit=itmpexpt,file=trim(fname),&
     status='old',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
! use the getADPmap routine in the filters module
call getADPmap(itmpexpt, totnumexpt, L, ebsdnl%ipf_wd, ebsdnl%ipf_ht, dpmap)
close(unit=itmpexpt,status='keep')












end subroutine EMsoftCpreprocessEBSDPatterns


end module EMDIwrappermod