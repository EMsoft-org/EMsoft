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
! EMsoft:HDFhyperslabTest.f90
!--------------------------------------------------------------------------
!
! MODULE: HDFhyperslabTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief test module for writing and reading of hyperslabs to/from HDF5 files
!
!> @date 10/29/16   MDG 1.0 original
!--------------------------------------------------------------------------

module HDFhyperslabTest

use stringconstants

contains 

subroutine HDFhyperslabExecuteTest(res) &
           bind(c, name='HDFhyperslabExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFhyperslabExecuteTest

use,INTRINSIC :: ISO_C_BINDING
use local
use HDF5
use typedefs
use HDFsupport

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)  :: res

character(fnlen)                :: HDFfilename, tmppath, groupname, dataset, textfile

integer(kind=irg)               :: i1, i2, i3, i4, dim1, dim2, dim3, dim4, hdferr, isum 

integer(HSIZE_T)                :: dims2(2), dims3(3), dims4(4), cnt2(2), cnt3(3), cnt4(4), offset2(2), offset3(3), offset4(4), &
                                   rm2(2), rm3(3), rm4(4), rn2(2), rn3(3), rn4(4), olddims2(2), olddims3(3), olddims4(4)
logical                         :: overwrite

! character arrays
character(kind=c_char),allocatable      :: carr2(:,:), carr3(:,:,:), carr4(:,:,:,:)
character(kind=c_char),allocatable      :: carr2_save(:,:), carr3_save(:,:,:), carr4_save(:,:,:,:)

! integer arrays
integer(kind=irg),allocatable   :: iarr2(:,:), iarr3(:,:,:), iarr4(:,:,:,:)
integer(kind=irg),allocatable   :: iarr2_save(:,:), iarr3_save(:,:,:), iarr4_save(:,:,:,:)

! float arrays
real(kind=sgl),allocatable      :: farr2(:,:), farr3(:,:,:), farr4(:,:,:,:)
real(kind=sgl),allocatable      :: farr2_save(:,:), farr3_save(:,:,:), farr4_save(:,:,:,:)

! double arrays
real(kind=dbl),allocatable      :: darr2(:,:), darr3(:,:,:), darr4(:,:,:,:)
real(kind=dbl),allocatable      :: darr2_save(:,:), darr3_save(:,:,:), darr4_save(:,:,:,:)


type(HDFobjectStackType)        :: HDF_head
character(len=1)                :: EMsoftnativedelimiter


!====================================
! dimensions for the full arrays
dim1 = 10
dim2 = 15
dim3 = 20
dim4 = 25

ALLOCATE (carr2(dim1,dim2))
ALLOCATE (carr3(dim1,dim2,dim3))
ALLOCATE (carr4(dim1,dim2,dim3,dim4))
ALLOCATE (iarr2(dim1,dim2))
ALLOCATE (iarr3(dim1,dim2,dim3))
ALLOCATE (iarr4(dim1,dim2,dim3,dim4))
ALLOCATE (farr2(dim1,dim2))
ALLOCATE (farr3(dim1,dim2,dim3))
ALLOCATE (farr4(dim1,dim2,dim3,dim4))
ALLOCATE (darr2(dim1,dim2))
ALLOCATE (darr3(dim1,dim2,dim3))
ALLOCATE (darr4(dim1,dim2,dim3,dim4))

! initialize empty hyperslabs
carr2 = char(0)
carr3 = char(0)
carr4 = char(0)
iarr2 = 0
iarr3 = 0
iarr4 = 0
farr2 = 0.0
farr3 = 0.0
farr4 = 0.0
darr2 = 0.D0
darr3 = 0.D0
darr4 = 0.D0

!====================================
! nullify the push/pop stack pointer
nullify(HDF_head%next)

! determine the pathname delimiter character
EMsoftnativedelimiter = EMsoft_getEMsoftnativedelimiter()

! get the location of the Temporary folder inside the Build folder (it always exists)
tmppath = EMsoft_getEMsofttestpath()

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_hyperslab.h5'

write(*,*) 'writing filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_createFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 1
  return
end if

! char
dataset = SC_hypercarr2
dims2 = (/ dim1, dim2 /)
olddims2 = dims2
cnt2 = dims2
offset2 = (/ 0, 0 /)
hdferr = HDF_writeHyperslabCharArray2D(dataset, carr2, dims2, offset2, cnt2, HDF_head)
if (hdferr.ne.0) then
  res = 2
  return
end if

dataset = SC_hypercarr3
dims3 = (/ dim1, dim2, dim3 /)
olddims3 = dims3
cnt3 = dims3
offset3 = (/ 0, 0, 0 /)
hdferr = HDF_writeHyperslabCharArray3D(dataset, carr3, dims3, offset3, cnt3, HDF_head)
if (hdferr.ne.0) then
  res = 3
  return
end if

dataset = SC_hypercarr4
dims4 = (/ dim1, dim2, dim3, dim4 /)
olddims4 = dims4
cnt4 = dims4
offset4 = (/ 0, 0, 0, 0 /)
hdferr = HDF_writeHyperslabCharArray4D(dataset, carr4, dims4, offset4, cnt4, HDF_head)
if (hdferr.ne.0) then
  res = 4
  return
end if

! integer
dataset = SC_hyperiarr2
hdferr = HDF_writeHyperslabIntegerArray2D(dataset, iarr2, dims2, offset2, cnt2, HDF_head)
if (hdferr.ne.0) then
  res = 5
  return
end if

dataset = SC_hyperiarr3
hdferr = HDF_writeHyperslabIntegerArray3D(dataset, iarr3, dims3, offset3, cnt3, HDF_head)
if (hdferr.ne.0) then
  res = 6
  return
end if

dataset = SC_hyperiarr4
hdferr = HDF_writeHyperslabIntegerArray4D(dataset, iarr4, dims4, offset4, cnt4, HDF_head)
if (hdferr.ne.0) then
  res = 7
  return
end if

! float
dataset = SC_hyperfarr2
hdferr = HDF_writeHyperslabFloatArray2D(dataset, farr2, dims2, offset2, cnt2, HDF_head)
if (hdferr.ne.0) then
  res = 8
  return
end if

dataset = SC_hyperfarr3
hdferr = HDF_writeHyperslabFloatArray3D(dataset, farr3, dims3, offset3, cnt3, HDF_head)
if (hdferr.ne.0) then
  res = 9
  return
end if

dataset = SC_hyperfarr4
hdferr = HDF_writeHyperslabFloatArray4D(dataset, farr4, dims4, offset4, cnt4, HDF_head)
if (hdferr.ne.0) then
  res = 10
  return
end if

! double
dataset = SC_hyperdarr2
hdferr = HDF_writeHyperslabDoubleArray2D(dataset, darr2, dims2, offset2, cnt2, HDF_head)
if (hdferr.ne.0) then
  res = 11
  return
end if

dataset = SC_hyperdarr3
hdferr = HDF_writeHyperslabDoubleArray3D(dataset, darr3, dims3, offset3, cnt3, HDF_head)
if (hdferr.ne.0) then
  res = 12
  return
end if

dataset = SC_hyperdarr4
hdferr = HDF_writeHyperslabDoubleArray4D(dataset, darr4, dims4, offset4, cnt4, HDF_head)
if (hdferr.ne.0) then
  res = 13
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================

!====================================
! populate the arrays with data
do i1=1,dim1
  do i2=1,dim2
    carr2(i1,i2) = char(mod(i1 + i2,128))
    iarr2(i1,i2) = i1 * i2
    farr2(i1,i2) = real(i1 * i2)
    darr2(i1,i2) = dble(i1 * i2)
    do i3=1,dim3
      carr3(i1,i2,i3) = char(mod(i1 + i2 + i3,128))
      iarr3(i1,i2,i3) = i1 * i2 * i3
      farr3(i1,i2,i3) = real(i1 * i2 * i3)
      darr3(i1,i2,i3) = dble(i1 * i2 * i3)
      do i4=1,dim4
        carr4(i1,i2,i3,i4) = char(mod(i1 + i2 + i3 + i4,128))
        iarr4(i1,i2,i3,i4) = i1 * i2 * i3 * i4
        farr4(i1,i2,i3,i4) = real(i1 * i2 * i3 * i4)
        darr4(i1,i2,i3,i4) = dble(i1 * i2 * i3 * i4)
      end do
    end do
  end do
end do

! save the arrays
ALLOCATE (carr2_save(dim1,dim2))
ALLOCATE (carr3_save(dim1,dim2,dim3))
ALLOCATE (carr4_save(dim1,dim2,dim3,dim4))
ALLOCATE (iarr2_save(dim1,dim2))
ALLOCATE (iarr3_save(dim1,dim2,dim3))
ALLOCATE (iarr4_save(dim1,dim2,dim3,dim4))
ALLOCATE (farr2_save(dim1,dim2))
ALLOCATE (farr3_save(dim1,dim2,dim3))
ALLOCATE (farr4_save(dim1,dim2,dim3,dim4))
ALLOCATE (darr2_save(dim1,dim2))
ALLOCATE (darr3_save(dim1,dim2,dim3))
ALLOCATE (darr4_save(dim1,dim2,dim3,dim4))

carr2_save = carr2
carr3_save = carr3
carr4_save = carr4
iarr2_save = iarr2
iarr3_save = iarr3
iarr4_save = iarr4
farr2_save = farr2
farr3_save = farr3
farr4_save = farr4
darr2_save = darr2
darr3_save = darr3
darr4_save = darr4
!====================================


!====================================
! next, we overwrite portions of the existing arrays with partial hyperslabs
! nullify the push/pop stack pointer
nullify(HDF_head%next)

! determine the pathname delimiter character
EMsoftnativedelimiter = EMsoft_getEMsoftnativedelimiter()

! get the location of the Temporary folder inside the Build folder (it always exists)
tmppath = EMsoft_getEMsofttestpath()

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_hyperslab.h5'

write(*,*) 'writing filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_openFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 14
  return
end if

dim1 = 3
dim2 = 6
dim3 = 9
dim4 = 12

dims2 = (/ dim1, dim2 /)
offset2 = (/ 2, 4 /)
cnt2 = dims2
dims3 = (/ dim1, dim2, dim3 /)
offset3 = (/ 2, 4, 6 /)
cnt3 = dims3
dims4 = (/ dim1, dim2, dim3, dim4 /)
offset4 = (/ 2, 4, 6, 8 /)
cnt4 = dims4

rm2 = offset2
rm3 = offset3
rm4 = offset4
rn2 = offset2+dims2 - (/ 1, 1 /)
rn3 = offset3+dims3 - (/ 1, 1, 1 /)
rn4 = offset4+dims4 - (/ 1, 1, 1, 1 /)

write (*,*) rm2,'; ',rm3,'; ',rm4
write (*,*) rn2,'; ',rn3,'; ',rn4

overwrite = .TRUE.

! char
dataset = SC_hypercarr2
hdferr = HDF_writeHyperslabCharArray2D(dataset, carr2(rm2(1):rn2(1),rm2(2):rn2(2)), &
                                       olddims2, offset2, cnt2, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 15
  return
end if

dataset = SC_hypercarr3
hdferr = HDF_writeHyperslabCharArray3D(dataset, carr3(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3)), &
                                       olddims3, offset3, cnt3, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 16
  return
end if

dataset = SC_hypercarr4
hdferr = HDF_writeHyperslabCharArray4D(dataset, carr4(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4)), &
                                       olddims4, offset4, cnt4, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 15
  return
end if

! integer
dataset = SC_hyperiarr2
hdferr = HDF_writeHyperslabIntegerArray2D(dataset, iarr2(rm2(1):rn2(1),rm2(2):rn2(2)), &
                                       olddims2, offset2, cnt2, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 16
  return
end if

dataset = SC_hyperiarr3
hdferr = HDF_writeHyperslabIntegerArray3D(dataset, iarr3(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3)), &
                                       olddims3, offset3, cnt3, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 17
  return
end if

dataset = SC_hyperiarr4
hdferr = HDF_writeHyperslabIntegerArray4D(dataset, iarr4(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4)), &
                                       olddims4, offset4, cnt4, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 18
  return
end if

! float
dataset = SC_hyperfarr2
hdferr = HDF_writeHyperslabFloatArray2D(dataset, farr2(rm2(1):rn2(1),rm2(2):rn2(2)), &
                                       olddims2, offset2, cnt2, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 19
  return
end if

dataset = SC_hyperfarr3
hdferr = HDF_writeHyperslabFloatArray3D(dataset, farr3(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3)), &
                                       olddims3, offset3, cnt3, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 20
  return
end if

dataset = SC_hyperfarr4
hdferr = HDF_writeHyperslabFloatArray4D(dataset, farr4(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4)), &
                                       olddims4, offset4, cnt4, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 21
  return
end if

! double
dataset = SC_hyperdarr2
hdferr = HDF_writeHyperslabDoubleArray2D(dataset, darr2(rm2(1):rn2(1),rm2(2):rn2(2)), &
                                       olddims2, offset2, cnt2, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 22
  return
end if

dataset = SC_hyperdarr3
hdferr = HDF_writeHyperslabDoubleArray3D(dataset, darr3(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3)), &
                                       olddims3, offset3, cnt3, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 23
  return
end if

dataset = SC_hyperdarr4
hdferr = HDF_writeHyperslabDoubleArray4D(dataset, darr4(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4)), &
                                       olddims4, offset4, cnt4, HDF_head, overwrite)
if (hdferr.ne.0) then
  res = 24
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
! deallocate the arrays (they will be recreated upon reading)
deallocate( carr2, carr3, carr4, iarr2, iarr3, iarr4, farr2, farr3, farr4, darr2, darr3, darr4)
!====================================

!====================================
! next, we read the hyperslabs from the HDF5 file
! nullify the push/pop stack pointer
nullify(HDF_head%next)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_hyperslab.h5'

write(*,*) 'reading filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_openFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 25
  return
end if


! char
dataset = SC_hypercarr2
dims2 = (/ dim1, dim2 /)
offset2 = (/ 2, 4 /)
carr2 = HDF_readHyperslabCharArray2D(dataset, offset2, dims2, HDF_head)
if (hdferr.ne.0) then
  res = 26
  return
end if

dataset = SC_hypercarr3
dims3 = (/ dim1, dim2, dim3 /)
offset3 = (/ 2, 4, 6 /)
carr3 = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, HDF_head)
if (hdferr.ne.0) then
  res = 27
  return
end if

dataset = SC_hypercarr4
dims4 = (/ dim1, dim2, dim3, dim4 /)
offset4 = (/ 2, 4, 6, 8 /)
carr4 = HDF_readHyperslabCharArray4D(dataset, offset4, dims4, HDF_head)
if (hdferr.ne.0) then
  res = 28
  return
end if

! integer
dataset = SC_hyperiarr2
iarr2 = HDF_readHyperslabIntegerArray2D(dataset, offset2, dims2, HDF_head)
if (hdferr.ne.0) then
  res = 29
  return
end if

dataset = SC_hyperiarr3
iarr3 = HDF_readHyperslabIntegerArray3D(dataset, offset3, dims3, HDF_head)
if (hdferr.ne.0) then
  res = 30
  return
end if

dataset = SC_hyperiarr4
iarr4 = HDF_readHyperslabIntegerArray4D(dataset, offset4, dims4, HDF_head)
if (hdferr.ne.0) then
  res = 31
  return
end if

! float
dataset = SC_hyperfarr2
farr2 = HDF_readHyperslabFloatArray2D(dataset, offset2, dims2, HDF_head)
if (hdferr.ne.0) then
  res = 32
  return
end if

dataset = SC_hyperfarr3
farr3 = HDF_readHyperslabFloatArray3D(dataset, offset3, dims3, HDF_head)
if (hdferr.ne.0) then
  res = 33
  return
end if

dataset = SC_hyperfarr4
farr4 = HDF_readHyperslabFloatArray4D(dataset, offset4, dims4, HDF_head)
if (hdferr.ne.0) then
  res = 34
  return
end if

! double
dataset = SC_hyperdarr2
darr2 = HDF_readHyperslabDoubleArray2D(dataset, offset2, dims2, HDF_head)
if (hdferr.ne.0) then
  res = 35
  return
end if

dataset = SC_hyperdarr3
darr3 = HDF_readHyperslabDoubleArray3D(dataset, offset3, dims3, HDF_head)
if (hdferr.ne.0) then
  res = 36
  return
end if

dataset = SC_hyperdarr4
darr4 = HDF_readHyperslabDoubleArray4D(dataset, offset4, dims4, HDF_head)
if (hdferr.ne.0) then
  res = 37
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
! compare the partial slabs with the saved arrays
isum = 40
if (all(carr2.eq.carr2_save(rm2(1):rn2(1),rm2(2):rn2(2))).eqv..FALSE.) isum = isum + 1
if (all(carr3.eq.carr3_save(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3))).eqv..FALSE.) isum = isum + 2
if (all(carr4.eq.carr4_save(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4))).eqv..FALSE.) isum = isum + 4

if (all(iarr2.eq.iarr2_save(rm2(1):rn2(1),rm2(2):rn2(2))).eqv..FALSE.) isum = isum + 8
if (all(iarr3.eq.iarr3_save(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3))).eqv..FALSE.) isum = isum + 16
if (all(iarr4.eq.iarr4_save(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4))).eqv..FALSE.) isum = isum + 32

write (*,*) 'shape darr4 = ',shape(darr4)
write (*,*) 'shape darr4_save = ',shape(darr4_save(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4)))
write (*,*) 'max(abs(diff)) = ',maxval(darr4 - darr4_save(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4)))

if (all(farr2.eq.farr2_save(rm2(1):rn2(1),rm2(2):rn2(2))).eqv..FALSE.) isum = isum + 64
if (all(farr3.eq.farr3_save(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3))).eqv..FALSE.) isum = isum + 128
if (all(farr4.eq.farr4_save(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4))).eqv..FALSE.) isum = isum + 256

if (all(darr2.eq.darr2_save(rm2(1):rn2(1),rm2(2):rn2(2))).eqv..FALSE.) isum = isum + 512
if (all(darr3.eq.darr3_save(rm3(1):rn3(1),rm3(2):rn3(2),rm3(3):rn3(3))).eqv..FALSE.) isum = isum + 1024
if (all(darr4.eq.darr4_save(rm4(1):rn4(1),rm4(2):rn4(2),rm4(3):rn4(3),rm4(4):rn4(4))).eqv..FALSE.) isum = isum + 2048

if (isum.eq.40) then
  res = 0
else
  res = isum
end if

!====================================

end subroutine HDFhyperslabExecuteTest


end module HDFhyperslabTest
