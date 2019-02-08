! ###################################################################
! Copyright (c) 2019-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMdpextract.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMdpextract
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extract a number of images from a dot product file and store them in a folder
!
!> @note all we need to run this program is the file name of the dot product file
!
!> @date 02/05/19 MDG 1.0 original
!--------------------------------------------------------------------------
program EMdpextract

use local
use HDF5
use h5im
use h5lt
use NameListTypedefs
use NameListHandlers
use HDFsupport
use io
use image
use, intrinsic :: iso_fortran_env
use EBSDDImod

IMPLICIT NONE

character(fnlen)                        :: progname, progdesc, dpfilebase, dpfile, dirname, image_filename
type(HDFobjectStackType),pointer        :: HDF_head
type(EBSDDIdataType)                    :: EBSDDIdata
type(EBSDIndexingNameListType)          :: dinl
integer(kind=irg)                       :: hdferr, shp(2), jj, nx, ny
real(kind=sgl)                          :: mi, ma
logical                                 :: fexists 

! declare variables for use in object oriented image module
integer                                 :: iostat
character(len=128)                      :: iomsg
logical                                 :: isInteger
type(image_t)                           :: im, im2
integer(int8)                           :: i8 (3,4), int8val
integer(int8), allocatable              :: output_image(:,:)

progname = 'EMdpextract.f90'
progdesc = 'Extract a number of images from a dictionary indexing dot product file'

! print some information
call EMsoft(progname, progdesc)


! ask the user for the dot product file name without the .h5 extension
call ReadValue('Enter the name of the dot product file without the .h5 extension:', dpfilebase)
dpfile = trim(dpfilebase)//'.h5'

call Message (' looking for file '//trim(dpfile))

! read all the image-type arrays from the file
call h5open_EMsoft(hdferr)
call readEBSDDotProductFile(dpfile, dinl, hdferr, EBSDDIdata, &
                            getADP=.TRUE., &
                            getKAM=.TRUE., &
                            getCI=.TRUE., &
                            getIQ=.TRUE., & 
                            getOSM=.TRUE., & 
                            presentFolder=.TRUE.) 
call h5close_EMsoft(hdferr)
call Message('   found file and read data arrays')

! take these arrays and generate image files for each of them; place them in a folder with the dpfilebase name 
dirname = trim(dpfilebase)
inquire(file=trim(dirname),exist=fexists)
if (.not.(fexists)) then
  call system('mkdir '//trim(dirname))
  write (*,*) trim(dirname),' folder has been created'
end if
call chdir(trim(dirname))

! ==============================
! ==============================
! ==============================
! ADP map
image_filename = trim(dpfilebase)//'_ADP.tiff'
shp = shape(EBSDDIdata%ADP)
nx = shp(1)
ny = shp(2)
allocate(output_image(nx,ny))
output_image = EBSDDIdata%ADP
im2 = image_t(output_image)
if(im2%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

! create the file
call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message(" failed to write image to file : "//iomsg)
else  
  call Message('  ADP array written to '//trim(image_filename))
end if 
deallocate(output_image)

! ==============================
! ==============================
! ==============================
! CI map
image_filename = trim(dpfilebase)//'_CI.tiff'
allocate(output_image(nx,ny))

mi = minval(EBSDDIdata%CI)
EBSDDIdata%CI = EBSDDIdata%CI - mi
ma = maxval(EBSDDIdata%CI)
output_image = 0

do jj = 1,ny
    output_image(1:nx,jj) = int(255.0*EBSDDIdata%CI((jj-1)*nx+1:jj*nx)/ma)
end do

im2 = image_t(output_image)
if(im2%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

! create the file
call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message(" failed to write image to file : "//iomsg)
else  
  call Message('  CI array written to '//trim(image_filename))
end if 
deallocate(output_image)

! ==============================
! ==============================
! ==============================
! IQ map
image_filename = trim(dpfilebase)//'_IQ.tiff'
allocate(output_image(nx,ny))

mi = minval(EBSDDIdata%IQ)
EBSDDIdata%IQ = EBSDDIdata%IQ - mi
ma = maxval(EBSDDIdata%IQ)
output_image = 0

do jj = 1,ny
    output_image(1:nx,jj) = int(255.0*EBSDDIdata%IQ((jj-1)*nx+1:jj*nx)/ma)
end do

im2 = image_t(output_image)
if(im2%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

! create the file
call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message(" failed to write image to file : "//iomsg)
else  
  call Message('  IQ array written to '//trim(image_filename))
end if 
deallocate(output_image)

! ==============================
! ==============================
! ==============================
! KAM map
image_filename = trim(dpfilebase)//'_KAM.tiff'
allocate(output_image(nx,ny))

mi = minval(EBSDDIdata%KAM)
EBSDDIdata%KAM = EBSDDIdata%KAM - mi
ma = maxval(EBSDDIdata%KAM)
output_image = 0

output_image = int(255.0*EBSDDIdata%KAM/ma)

im2 = image_t(output_image)
if(im2%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

! create the file
call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message(" failed to write image to file : "//iomsg)
else  
  call Message('  KAM array written to '//trim(image_filename))
end if 
deallocate(output_image)

! ==============================
! ==============================
! ==============================
! OSM map
image_filename = trim(dpfilebase)//'_OSM.tiff'
allocate(output_image(nx,ny))

mi = minval(EBSDDIdata%OSM)
EBSDDIdata%OSM = EBSDDIdata%OSM - mi
ma = maxval(EBSDDIdata%OSM)
output_image = 0

output_image = int(255.0*EBSDDIdata%OSM/ma)

im2 = image_t(output_image)
if(im2%empty()) call Message("EMEBSDDIpreview","failed to convert array to image")

! create the file
call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message(" failed to write image to file : "//iomsg)
else  
  call Message('  OSM array written to '//trim(image_filename))
end if 
deallocate(output_image)







end program EMdpextract
