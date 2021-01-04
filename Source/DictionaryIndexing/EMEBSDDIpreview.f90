! ###################################################################
! Copyright (c) 2015-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDDIpreview.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDDIpreview
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Simple program to preview what the filtering parameters will result in 
!
!> @date 01/24/18 MDG 1.0 original 
!> @date 07/17/18 MDG 1.1 added option to extract single pattern from data set and scale
!--------------------------------------------------------------------------

program EMEBSDDIpreview

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(EBSDDIpreviewNameListType)             :: enl
logical                                     :: verbose
integer(kind=irg)                           :: res

nmldeffile = 'EMEBSDDIpreview.nml'
progname = 'EMEBSDDIpreview.f90'
progdesc = 'Program to preview which EBSD pattern filtering parameters work best'
verbose = .TRUE.

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 47 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMEBSDDIpreview','JSON input not yet implemented')
!  call JSONreadTKDIndexingNameList(tkdnl, nmldeffile, error_cnt)
else
  call GetEBSDDIpreviewNameList(nmldeffile,enl)
end if

call MasterSubroutine(enl, progname, nmldeffile)

end program EMEBSDDIpreview

!--------------------------------------------------------------------------
!
! SUBROUTINE:MasterSubroutine
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Master subroutine to generate tiff file with matrix of pre-processed patterns
!
!> @param enl namelist pointer
!> @param progname name of the program
!> @param nmldeffile namelist filename
!
!> @date 01/24/18 MDG 1.0 original
!> @date 02/19/18 MDG 1.1 modified input to new patternmod module options
!> @date 07/17/18 MDG 1.2 option to extract and scale up individual pattern
!> @date 04/26/19 MDG 1.3 added option to ouput an averaged pattern
!--------------------------------------------------------------------------

subroutine MasterSubroutine(enl, progname, nmldeffile)

use local
use error
use image
use, intrinsic :: iso_fortran_env
use io
use filters
use patternmod
use NameListTypedefs
use HDF5
use FFTW3mod

IMPLICIT NONE

type(EBSDDIpreviewNameListType),INTENT(INOUT)       :: enl
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

character(fnlen)                                    :: ename, image_filename
integer(kind=irg)                                   :: iunitexpt, recordsize, ierr, kk, ii, jj, i, j, numr, numw, binx, biny, &
                                                       xoffset, yoffset, io_int(2), istat, L, patsz , hdferr, nx, ny
integer(HSIZE_T)                                    :: dims3(3), offset3(3)
logical                                             :: f_exists
real(kind=sgl)                                      :: mi, ma, io_real(1)
real(kind=dbl)                                      :: x, y, val, v2
real(kind=sgl),allocatable                          :: expt(:), pattern(:,:), pcopy(:,:), hpvals(:), sumexpt(:)
integer(kind=irg),allocatable                       :: nrvals(:), pint(:,:), ppp(:,:)
type(C_PTR)                                         :: HPplanf, HPplanb
complex(kind=dbl),allocatable                       :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),pointer                   :: inp(:,:), outp(:,:)
type(c_ptr), allocatable                            :: ip, op
real(kind=dbl),allocatable                          :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)

! declare variables for use in object oriented image module
integer                                             :: iostat
character(len=128)                                  :: iomsg
logical                                             :: isInteger
type(image_t)                                       :: im, im2
integer(int8)                                       :: i8 (3,4), int8val
integer(int8), allocatable                          :: output_image(:,:)

call h5open_EMsoft(hdferr)

binx = enl%numsx
biny = enl%numsy
L = binx * biny
recordsize = 4 * L
patsz = L

! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getSingleExpPattern() routine to read a 
! pattern into the expt variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(enl%exptfile, enl%ipf_wd, L, enl%inputtype, recordsize, iunitexpt, enl%HDFstrings)
if (istat.ne.0) then
    call patternmod_errormessage(istat)
    call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
end if

! should we average patterns locally ?
allocate(expt(patsz))
dims3 = (/ binx, biny, 1 /)
if (enl%numav.ge.0) then
  io_int(1) = 2*enl%numav+1
  io_int(2) = 2*enl%numav+1
  call WriteValue(' Averaging patterns over ', io_int, 2, "(I3,' by ',I3,' area')")
  allocate(sumexpt(patsz))
  sumexpt = 0.0
  jj = 0 
  do i=-enl%numav,enl%numav
    if ((enl%patx+i.gt.0).and.(enl%patx+i.lt.enl%ipf_wd)) then
      do j=-enl%numav,enl%numav
        if ((enl%paty+j.gt.0).and.(enl%paty+j.lt.enl%ipf_ht)) then
          offset3 = (/ 0, 0, (enl%paty+j) * enl%ipf_wd + (enl%patx+i) /)
          call getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, enl%inputtype, &
                                   enl%HDFstrings, expt)
          sumexpt = sumexpt + expt
          jj = jj+1
        end if 
      end do 
    end if 
  end do
  sumexpt = sumexpt / float(jj)
end if

! and read the center pattern (again)
offset3 = (/ 0, 0, enl%paty * enl%ipf_wd + enl%patx /)
call getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, enl%inputtype, enl%HDFstrings, expt)

! and close the pattern file
call closeExpPatternFile(enl%inputtype, iunitexpt)


! turn it into a 2D pattern
allocate(pattern(binx, biny), pcopy(binx, biny), pint(binx,biny), ppp(binx,biny), stat=ierr)
if (enl%numav.gt.0) then 
  do kk=1,biny
    pcopy(1:binx,kk) = sumexpt((kk-1)*binx+1:kk*binx)
  end do
else
  do kk=1,biny
    pcopy(1:binx,kk) = expt((kk-1)*binx+1:kk*binx)
  end do
end if

! do we need to extract this pattern from the file and store it as an image file ?
if (trim(enl%patternfile).ne.'undefined') then
! allocate a byte array for the final output TIFF image that will contain all individual images
  allocate(output_image(binx,biny))
  image_filename = trim(EMsoft_getEMdatapathname())//trim(enl%patternfile)
  image_filename = EMsoft_toNativePath(image_filename)

  ma = maxval(pcopy)
  mi = minval(pcopy)

  do i=1,binx
    do j=1,biny
     int8val = int(255.0*(pcopy(i,biny-j+1)-mi)/(ma-mi))
     output_image(i,j) = int8val
    end do
  end do

 ! set up the image_t structure
  im = image_t(output_image)
  if(im%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

 ! create the file
  call im%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
  if(0.ne.iostat) then
    call Message("failed to write image to file : "//iomsg)
  else  
    call Message('  Selected pattern written to '//trim(image_filename))
  end if 
  deallocate(output_image)
end if


! define the high-pass filter width array and the nregions array
numr = (enl%nregionsmax - enl%nregionsmin) / enl%nregionsstepsize + 1
allocate(nrvals(numr))
nrvals = enl%nregionsmin + (/ ((i-1)*enl%nregionsstepsize, i=1,numr) /)

! the array for the hi pass filter parameter is a non-linear progression
numw = enl%hipasswnsteps
allocate(hpvals(numw))
do ii=1,numw
    hpvals(ii) = 2.0**(float(ii-1-numw)) * 2.0 * enl%hipasswmax
end do

! allocate a byte array for the final output TIFF image that will contain all individual images
nx = numw * binx
ny = numr * biny
allocate(output_image(nx,ny))
image_filename = trim(EMsoft_getEMdatapathname())//trim(enl%tifffile)
image_filename = EMsoft_toNativePath(image_filename)

! next we need to set up the high-pass filter fftw plans
allocate(hpmask(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask, inp, outp arrays'
allocate(rrdata(binx,biny),ffdata(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate rrdata, ffdata arrays'

! use the fftw_alloc routine to create the inp and outp arrays
! using a regular allocate can occasionally cause issues, in particular with 
! the ifort compiler. [MDG, 7/14/20]
ip = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
call c_f_pointer(ip, inp, [binx,biny])

op = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
call c_f_pointer(op, outp, [binx,biny])

inp = cmplx(0.D0,0D0)
outp = cmplx(0.D0,0.D0)

call init_HiPassFilter(dble(hpvals(1)), (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
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

! and store the pattern in the correct spot in the output_image array (flipped upside down !!!)
        yoffset =  (numr-jj) * biny + 1
        do i=1,binx
          do j=1,biny
!          output_image(xoffset+i-1, yoffset+j-1) = ppp(i,biny-j+1)
           output_image(xoffset+i-1, yoffset+j-1) = ppp(i,j)
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

call fftw_free(ip)
call fftw_free(op)
call fftw_cleanup()

! set up the image_t structure
im2 = image_t(output_image)
if(im2%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

! create the file
call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message("failed to write image to file : "//iomsg)
else  
  call Message('  Preprocessed pattern array written to '//trim(image_filename))
end if 
deallocate(output_image)

call Message('')
call Message(' High-pass filter parameter values along horizontal axis (L to R) :')
do ii=1,numw
    io_real(1) = hpvals(ii)
    call WriteValue('',io_real,1,"(F10.6)")
end do

call Message('')
call Message(' nregions values along vertical axis (B to T):')
do ii=1,numr
    io_int(1) = nrvals(ii)
    call WriteValue('',io_int,1)
end do

call h5close_EMsoft(hdferr)

end subroutine MasterSubroutine
