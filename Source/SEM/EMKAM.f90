! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMKAM.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMKAM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a tiff file with an KAM map
!
!> @date 07/29/16 MDG 1.0 original
!> @date 03/12/18 MDG 1.1 replaced reading of dot product file by new subroutine, and simplified code
!> @date 03/14/18 MDG 1.2 replaced old TIF module by Will Lenthe's new object oriented module
!--------------------------------------------------------------------------
program EMKAM

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use constants
use dictmod
use io
use error
use ECPmod
use EBSDiomod
use EBSDmod
use EBSDDImod
use HDF5
use HDFsupport
!use TIFF_f90
use rotations
use stringconstants
use image
use, intrinsic :: iso_fortran_env
use commonmod


IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(KAMNameListType)                   :: enl
type(EBSDIndexingNameListType)          :: ebsdnl
type(EBSDDIdataType)                    :: EBSDDIdata

logical                                 :: stat, readonly, noindex
integer(kind=irg)                       :: hdferr, nlines, FZcnt, Nexp, nnm, nnk, Pmdims, i, j, k, olabel, Nd, Ne, ipar(10), &
                                           ipar2(6), pgnum, ipat, ipf_wd, ipf_ht, idims2(2), io_int(2)
character(fnlen)                        :: groupname, dataset, dpfile, energyfile, masterfile, efile, fname, image_filename
integer(HSIZE_T)                        :: dims2(2)
type(dicttype)                          :: dict
real(kind=sgl)                          :: testeu(3), eu1(3), eu2(3), ro1(4), ro2(4), s, da

character(fnlen),allocatable            :: stringarray(:)
integer(kind=irg),allocatable           :: tmi(:,:), tmitmp(:,:), indexmain(:,:)
real(kind=sgl),allocatable              :: kam(:,:), eulers(:,:), Eulerstmp(:,:), Eulervals(:,:), avEuler(:,:), &
                                           dplist(:,:), dplisttmp(:,:)

type(HDFobjectStackType)                :: HDF_head

! declare variables for use in object oriented image module
integer                                 :: iostat
character(len=128)                      :: iomsg
logical                                 :: isInteger
type(image_t)                           :: im
integer(int8)                           :: i8 (3,4)
integer(int8), allocatable              :: TIFF_image(:,:)


nullify(HDF_head%next)

nmldeffile = 'EMKAM.nml'
progname = 'EMKAM.f90'
progdesc = 'Generate a nearest-neighbor Kernel Average Misorientation (KAM) map'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 84 /), progname)

! deal with the namelist stuff
call GetKAMNameList(nmldeffile,enl)

!====================================
! read the relevant fields from the dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

call readEBSDDotProductFile(enl%dotproductfile, ebsdnl, hdferr, EBSDDIdata, &
                            getEulerAngles=.TRUE., &
                            getTopDotProductList=.TRUE., &
                            getTopMatchIndices=.TRUE.) 

write (*,*) 'completed reading data file '

Nexp = EBSDDIdata%Nexp
FZcnt = EBSDDIdata%FZcnt
dims2(1) = ebsdnl%nnk
allocate(Eulervals(3,FZcnt))
do i=1,FZcnt
  Eulervals(1:3,i) = EBSDDIdata%EulerAngles(1:3,i)
end do
deallocate(EBSDDIdata%EulerAngles)
Eulervals = Eulervals * sngl(cPi)/180.0

allocate(dplist(dims2(1),Nexp))
do i=1,Nexp
  dplist(1:dims2(1),i) = EBSDDIdata%TopDotProductList(1:dims2(1),i)
end do
deallocate(EBSDDIdata%TopDotProductList)
nnm = dims2(1)

allocate(tmi(nnm,Nexp))
do i=1,Nexp
  tmi(1:nnm,i) = EBSDDIdata%TopMatchIndices(1:nnm,i)
end do
deallocate(EBSDDIdata%TopMatchIndices)

! and next we compute the KAM map (kam)
allocate(kam(ebsdnl%ipf_wd,ebsdnl%ipf_ht),eulers(3,Nexp))

! do we need to do an orientation average first ?
if (enl%orav.ne.0) then
  ipar2(1) = EBSDDIdata%pgnum
  ipar2(2) = FZcnt
  ipar2(3) = Nexp
  ipar2(4) = ebsdnl%nnk
  ipar2(5) = Nexp*ceiling(float(ipf_wd*ipf_ht)/float(Nexp))
  ! to average we need at least two values so check the value of orav
  if (enl%orav.eq.1) then
    ipar2(6) = 2
  else
    ipar2(6) = enl%orav
  end if 
  call Message('Computing orientation averages ... ')
  call EBSDgetAverageOrientations(ipar2, Eulervals, tmi, dplist, eulers)
  eulers = eulers*sngl(cPi)/180.0
else
  do i=1,Nexp
    eulers(1:3,i) = Eulervals(1:3,tmi(1,i))
  end do
end if

! compute the Kernel Average Misorientation map
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = EBSDDIdata%pgnum
call DI_Init(dict,'nil') 

call Message('Computing KAM map... ')
call Message('')
call EBSDgetKAMMap(Nexp, eulers, ebsdnl%ipf_wd, ebsdnl%ipf_ht, dict, kam)
kam = kam*180.0/sngl(cPi)

where (kam.gt.enl%kamcutoff) kam = enl%kamcutoff
where (kam.lt.0.0) kam = 0.0
kam = kam/maxval(kam)
kam = kam*255.0


!==============================================
image_filename = trim(EMsoft_getEMdatapathname())//trim(enl%kamtiff)
image_filename = EMsoft_toNativePath(image_filename)

! allocate memory for image
allocate(TIFF_image(ebsdnl%ipf_wd,ebsdnl%ipf_ht))

! fill the image with whatever data you have (between 0 and 255)
 do i=1,ebsdnl%ipf_wd
  do j=1,ebsdnl%ipf_ht
   TIFF_image(i,j) = kam(i,j)
  end do
 end do

! set up the image_t structure
im = image_t(TIFF_image)
if(im%empty()) call Message("EMKAM","failed to convert array to image")

! create the file
call im%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message("failed to write image to file : "//iomsg)
else  
  call Message('KAM map written to '//trim(image_filename))
end if 

! print image information
call Message("Image Information:")
io_int(1) = size(im%dims)
call WriteValue("rank             : ",io_int,1,"(I3)")
io_int = im%dims
call WriteValue("dims             : ",io_int,2,"(I5,' x',I5)")
io_int(1) = im%samplesPerPixel
call WriteValue("samples per pixel: ",io_int,1,"(I3)") 
io_int(1) = im%size()
call WriteValue("total pixels     : ",io_int,1,"(I8)") 
select case(im%pixelType)
  case(pix_i8 )
    call Message("pixel type: 8  bit integer")
  case(pix_i16)
    call Message("pixel type: 16 bit integer")
  case(pix_i32)
    call Message("pixel type: 32 bit integer")
  case(pix_i64)
    call Message("pixel type: 64 bit integer")
  case(pix_r32)
    call Message("pixel type: float")
  case(pix_r64)
    call Message("pixel type: double")
  case(pix_unk)
    call Message("pixel type: unknown")
end select



end program EMKAM
