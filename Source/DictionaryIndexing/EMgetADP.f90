! ###################################################################
! Copyright (c) 2015-2018, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMgetADP.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgetADP
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute ADP map based on raw and filtered data 
!
!> @date 02/17/18 MDG 1.0 code extracted from EMEBSDDI program 
!--------------------------------------------------------------------------

program EMgetADP

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use EBSDmod
use EBSDDImod

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(ADPNameListType)                       :: adpnl
integer(kind=irg)                           :: istat, res

nmldeffile = 'EMgetADP.nml'
progname = 'EMgetADP.f90'
progdesc = 'Standalone program to compute Average Dot Product map'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 17 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMgetADP','JSON input not yet implemented')
else
  call GetADPNameList(nmldeffile,adpnl)
end if

call ADPSubroutine(adpnl, progname, nmldeffile)

end program EMgetADP

!--------------------------------------------------------------------------
!
! SUBROUTINE:ADPSubroutine
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Subroutine to compute the filtered patterns, and from that, the ADP map
!
!> @param adpnl  namelist pointer
!> @param progname name of the program
!> @param nmldeffile namelist filename
!
!> @date 02/17/18 MDG 1.1 extracted code from EMEBSDDI program
!--------------------------------------------------------------------------

subroutine ADPSubroutine(adpnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use patternmod
use initializersHDF
use filters
use error
use io
use constants
use math
use omp_lib
use HDF5
use h5im
use h5lt
use HDFsupport
use ISO_C_BINDING
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

type(ADPNameListType),INTENT(INOUT)                 :: adpnl
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

integer(kind=irg)                                   :: num,ierr,irec,istat
integer(kind=irg),parameter                         :: iunit = 40
integer(kind=irg),parameter                         :: iunitexpt = 41
integer(kind=irg),parameter                         :: iunitdict = 42
integer(kind=irg),parameter                         :: itmpexpt = 43

integer(kind=irg)                                   :: L,totnumexpt,imght,imgwd,nnk, recordsize, iii, hdferr,&
                                                       recordsize_correct, patsz, TIFF_nx, TIFF_ny
real(kind=sgl),allocatable                          :: imageexpt(:), mask(:,:),masklin(:), exppatarray(:), tmpexppatarray(:)
real(kind=sgl),allocatable                          :: imageexptflt(:),binned(:,:),imagedictflt(:),imagedictfltflip(:), &
                                                       tmpimageexpt(:)
real(kind=sgl),allocatable                          :: EBSDpattern(:,:), dpmap(:)
real(kind=sgl),allocatable                          :: EBSDpatternintd(:,:), EBSDpat(:,:)
integer(kind=irg),allocatable                       :: EBSDpatterninteger(:,:), EBSDpatternad(:,:), EBSDpint(:,:)
real(kind=dbl),allocatable                          :: rdata(:,:), fdata(:,:), rrdata(:,:), ffdata(:,:)
complex(kind=dbl),allocatable                       :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable               :: inp(:,:), outp(:,:)
character(11)                                       :: dstr
character(15)                                       :: tstrb
character(15)                                       :: tstre
real(kind=dbl)                                      :: w, Jres
integer(kind=irg)                                   :: dims(2)
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, fname, ename, sourcefile, TIFF_filename
integer(hsize_t)                                    :: expwidth, expheight
integer(c_size_t),target                            :: slength
integer(c_int)                                      :: numd, nump
type(C_PTR)                                         :: planf, HPplanf, HPplanb
integer(HSIZE_T)                                    :: dims3(3), offset3(3)

integer(kind=irg)                                   :: i,j,ii,jj,kk,ll,mm,pp,qq, io_int(3), iiistart, iiiend, jjend
real(kind=sgl)                                      :: mi, ma, io_real(2), tstart, tmp, vlen, tstop
integer(kind=irg)                                   :: binx,biny,TID,nthreads
integer(kind=irg)                                   :: correctsize
logical                                             :: f_exists, ROIselected
character(1000)                                     :: charline

type(HDFobjectStackType),pointer                    :: HDF_head

! declare variables for use in object oriented image module
integer                                             :: iostat
character(len=128)                                  :: iomsg
logical                                             :: isInteger
type(image_t)                                       :: im
integer(int8)                                       :: i8 (3,4)
integer(int8), allocatable                          :: TIFF_image(:,:)

call timestamp(datestring=dstr, timestring=tstrb)

L = adpnl%numsx*adpnl%numsy
totnumexpt = adpnl%ipf_wd*adpnl%ipf_ht
imght = adpnl%numsx
imgwd = adpnl%numsy
recordsize = L*4
dims = (/imght, imgwd/)
w = adpnl%hipassw
binx = adpnl%numsx
biny = adpnl%numsy

! make sure that correctsize is a multiple of 16; if not, make it so
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if

! determine the experimental and dictionary sizes in bytes
recordsize_correct = correctsize*4
patsz              = correctsize

!=========================================
! ALLOCATION AND INITIALIZATION OF ARRAYS
!=========================================
allocate(mask(binx,biny),masklin(L),stat=istat)
if (istat .ne. 0) stop 'Could not allocate arrays for masks'
mask = 1.0
masklin = 0.0

allocate(imageexpt(L),imageexptflt(correctsize),imagedictflt(correctsize),imagedictfltflip(correctsize),stat=istat)
allocate(tmpimageexpt(correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for reading experimental image patterns'
imageexpt = 0.0
imageexptflt = 0.0

allocate(EBSDpattern(binx,biny),binned(binx,biny),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for EBSD pattern'
EBSDpattern = 0.0
binned = 0.0

allocate(rdata(binx,biny),fdata(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'
rdata = 0.D0
fdata = 0.D0

!=====================================================
! define the circular mask if necessary and convert to 1D vector
!=====================================================

if (trim(adpnl%maskfile).ne.'undefined') then
! read the mask from file; the mask can be defined by a 2D array of 0 and 1 values
! that is stored in row form as strings, e.g.    
!    0000001110000000
!    0000011111000000
! ... etc
!
    f_exists = .FALSE.
    fname = trim(EMsoft_getEMdatapathname())//trim(adpnl%maskfile)
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
      call FatalError('MasterSubroutine','maskfile '//trim(fname)//' does not exist')
    end if
else
    if (adpnl%maskpattern.eq.'y') then
      do ii = 1,biny
          do jj = 1,binx
              if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. adpnl%maskradius**2) then
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
!
! this could become a separate routine in the EMEBSDmod module ...
!=====================================================

! first, make sure that this file does not already exist
f_exists = .FALSE.
fname = trim(EMsoft_getEMtmppathname())//trim(adpnl%tmpfile)
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname), exist=f_exists)

call WriteValue('Checking for temporary file ',trim(fname))

if ((adpnl%usetmpfile.eq.'y').and.(.not.f_exists)) then
  call FatalError('ADPSubroutine','tmp file does not exist ...')
end if

! if the file exists, and we do not want to keep it, then delete and recreate it
if (f_exists) then
  if (adpnl%usetmpfile.eq.'n') then   ! delete the file and open a new one
    open(unit=itmpexpt,file=trim(fname),&
      status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
    close(unit=itmpexpt,status='delete')
  end if
end if

call h5open_EMsoft(hdferr)

if (adpnl%usetmpfile.eq.'n') then

! open the temporary file
open(unit=itmpexpt,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)

! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(adpnl%exptfile, adpnl%ipf_wd, L, adpnl%inputtype, recordsize, iunitexpt, adpnl%HDFstrings)
if (istat.ne.0) then
    call patternmod_errormessage(istat)
    call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
end if

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


! this next part is done with OpenMP, with only thread 0 doing the reading and writing,
! Thread 0 reads one line worth of patterns from the input file, then all threads do 
! the work, and thread 0 writes to the output file; repeat until all patterns have been processed.

call OMP_SET_NUM_THREADS(adpnl%nthreads)
io_int(1) = adpnl%nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

! allocate the arrays that holds the experimental patterns from a single row of the region of interest
allocate(exppatarray(patsz * adpnl%ipf_wd),stat=istat)
if (istat .ne. 0) stop 'could not allocate exppatarray'

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask array'
call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
deallocate(inp, outp)

call Message('Starting processing of experimental patterns')
call cpu_time(tstart)

dims3 = (/ binx, biny, adpnl%ipf_wd /)

if (sum(adpnl%ROI).ne.0) then
  ROIselected = .TRUE.
  iiistart = adpnl%ROI(2)
  iiiend = adpnl%ROI(2)+adpnl%ROI(4)-1
  jjend = adpnl%ROI(3)
else
  ROIselected = .FALSE.
  iiistart = 1
  iiiend = adpnl%ipf_ht
  jjend = adpnl%ipf_wd
end if

! we do one row at a time
prepexperimentalloop: do iii = iiistart,iiiend

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
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*adpnl%ipf_wd /)
        if (ROIselected.eqv..TRUE.) then
          call getExpPatternRow(iii, adpnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                                adpnl%inputtype, adpnl%HDFstrings, exppatarray, adpnl%ROI)
        else
          call getExpPatternRow(iii, adpnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                                adpnl%inputtype, adpnl%HDFstrings, exppatarray)
        end if
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER
    jj=0

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=1,jjend
      if (adpnl%filterpattern == 'y') then
! convert imageexpt to 2D EBSD Pattern array
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
        EBSDPat = float(adhisteq(adpnl%nregions,binx,biny,EBSDpint))

! convert back to 1D vector
        do kk=1,biny
          exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx) = EBSDPat(1:binx,kk)
        end do
      end if

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
      do jj=1,jjend
        write(itmpexpt,rec=(iii-iiistart)*jjend + jj) exppatarray((jj-1)*patsz+1:jj*patsz)
      end do
    end if

deallocate(EBSDPat, rrdata, ffdata, EBSDpint, inp, outp)
!$OMP BARRIER
!$OMP END PARALLEL

! print an update of progress
    if (mod(iii-iiistart+1,5).eq.0) then
      if (ROIselected.eqv..TRUE.) then
        io_int(1:2) = (/ iii-iiistart+1, adpnl%ROI(4) /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
      else
        io_int(1:2) = (/ iii-iiistart+1, adpnl%ipf_ht /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
      end if
    end if
end do prepexperimentalloop

call Message(' -> experimental patterns stored in tmp file')

call closeExpPatternFile(adpnl%inputtype, iunitexpt)

close(unit=itmpexpt,status='keep')

! print some timing information
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(adpnl%nthreads) * float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,/)")

end if

!=====================================================
call Message(' -> computing Average Dot Product map (ADP)')
call Message(' ')


! re-open the temporary file
open(unit=itmpexpt,file=trim(fname),&
     status='old',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)

! use the getADPmap routine in the filters module
if (ROIselected.eqv..TRUE.) then
  allocate(dpmap(adpnl%ROI(3)*adpnl%ROI(4)))
  call getADPmap(itmpexpt, adpnl%ROI(3)*adpnl%ROI(4), L, adpnl%ROI(3), adpnl%ROI(4), dpmap)
  TIFF_nx = adpnl%ROI(3)
  TIFF_ny = adpnl%ROI(4)
else
  allocate(dpmap(totnumexpt))
  call getADPmap(itmpexpt, totnumexpt, L, adpnl%ipf_wd, adpnl%ipf_ht, dpmap)
  TIFF_nx = adpnl%ipf_wd
  TIFF_ny = adpnl%ipf_ht
end if

! output the ADP map as a tiff file 
fname = trim(EMsoft_getEMdatapathname())//trim(adpnl%tiffname)//'_ADP.tiff'
fname = EMsoft_toNativePath(fname)
TIFF_filename = trim(fname)

! allocate memory for image
allocate(TIFF_image(TIFF_nx,TIFF_ny))

! fill the image with whatever data you have (between 0 and 255)
ma = maxval(dpmap)
mi = minval(dpmap)

do j=1,TIFF_ny
 do i=1,TIFF_nx
  ii = j * TIFF_nx + i + 1
  TIFF_image(i,j) = int(255 * (dpmap(ii)-mi)/(ma-mi))
 end do
end do

! set up the image_t structure
im = image_t(TIFF_image)
if(im%empty()) call Message("EMgetADP","failed to convert array to image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message("failed to write image to file : "//iomsg)
else  
  call Message('ADP map written to '//trim(TIFF_filename))
end if 
deallocate(TIFF_image)

if (adpnl%keeptmpfile.eq.'n') then
  close(unit=itmpexpt, status = 'delete')
  call Message(' -> tmp file deleted')
else
  call Message(' -> keeping tmp file')
end if 

call h5close_EMsoft(hdferr)

end subroutine ADPSubroutine
