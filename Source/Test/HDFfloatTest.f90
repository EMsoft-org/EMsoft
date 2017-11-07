! ###################################################################
! Copyright (c) 2016, Marc De Graef/Carnegie Mellon University
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
! EMsoft:HDFfloatTest.f90
!--------------------------------------------------------------------------
!
! MODULE: HDFfloatTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief test module for writing and reading of real(sgl) to/from HDF5 files
!
!> @date 10/29/16   MDG 1.0 original
!--------------------------------------------------------------------------

module HDFfloatTest

use stringconstants

contains 

subroutine HDFfloatExecuteTest(res) &
           bind(c, name='HDFfloatExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFfloatExecuteTest

use,INTRINSIC :: ISO_C_BINDING
use local
use HDF5
use typedefs
use HDFsupport

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)  :: res

character(fnlen)                :: HDFfilename, tmppath, groupname, dataset, textfile

integer(kind=irg)               :: i1, i2, i3, i4, dim1, dim2, dim3, dim4, hdferr, isum 
real(kind=sgl)                  :: fval, fval_save
integer(HSIZE_T)                :: dims1(1), dims2(2), dims3(3), dims4(4)
real(kind=sgl),allocatable      :: farr1(:), farr2(:,:), farr3(:,:,:), farr4(:,:,:,:)
real(kind=sgl),allocatable      :: farr1_save(:), farr2_save(:,:), farr3_save(:,:,:), farr4_save(:,:,:,:)

type(HDFobjectStackType),pointer:: HDF_head
character(len=1)                :: EMsoftnativedelimiter


!====================================
! generate the real arrays
dim1 = 5
dim2 = 10
dim3 = 15
dim4 = 20

ALLOCATE (farr1(dim1))
ALLOCATE (farr2(dim1,dim2))
ALLOCATE (farr3(dim1,dim2,dim3))
ALLOCATE (farr4(dim1,dim2,dim3,dim4))

fval = 123.0

do i1=1,dim1
  farr1(i1) = float(i1)
  do i2=1,dim2
    farr2(i1,i2) = float(i1 * i2)
    do i3=1,dim3
      farr3(i1,i2,i3) = float(i1 * i2 * i3)
      do i4=1,dim4
        farr4(i1,i2,i3,i4) = float(i1 * i2 * i3 * i4)
      end do
    end do
  end do
end do

ALLOCATE (farr1_save(dim1))
ALLOCATE (farr2_save(dim1,dim2))
ALLOCATE (farr3_save(dim1,dim2,dim3))
ALLOCATE (farr4_save(dim1,dim2,dim3,dim4))

fval_save = fval
farr1_save = farr1
farr2_save = farr2
farr3_save = farr3
farr4_save = farr4

!====================================
! nullify the push/pop stack pointer
nullify(HDF_head)

! determine the pathname delimiter character
EMsoftnativedelimiter = EMsoft_getEMsoftnativedelimiter()

! get the location of the Temporary folder inside the Build folder (it always exists)
tmppath = EMsoft_getEMsofttestpath()

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_float.h5'

write(*,*) 'writing filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_createFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 1
  return
end if

! write the float and float arrays to the file
dataset = SC_floatType
hdferr = HDF_writeDatasetFloat(dataset, fval, HDF_head)
if (hdferr.ne.0) then
  res = 2
  return
end if

dataset = SC_float1D
hdferr = HDF_writeDatasetFloatArray1D(dataset, farr1, dim1, HDF_head)
if (hdferr.ne.0) then
  res = 3
  return
end if

dataset = SC_float2D
hdferr = HDF_writeDatasetFloatArray2D(dataset, farr2, dim1, dim2, HDF_head)
if (hdferr.ne.0) then
  res = 4
  return
end if

dataset = SC_float3D
hdferr = HDF_writeDatasetFloatArray3D(dataset, farr3, dim1, dim2, dim3, HDF_head)
if (hdferr.ne.0) then
  res = 5
  return
end if

dataset = SC_float4D
hdferr = HDF_writeDatasetFloatArray4D(dataset, farr4, dim1, dim2, dim3, dim4, HDF_head)
if (hdferr.ne.0) then
  res = 6
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
! deallocate the integer arrays (they will be recreated upon reading)
fval = 0
deallocate( farr1, farr2, farr3, farr4)
!====================================

!====================================
! next, we read the data sets from the HDF5 file
! nullify the push/pop stack pointer
nullify(HDF_head)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_float.h5'

write(*,*) 'reading filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_openFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 7
  return
end if

! read the integer and arrays
dataset = SC_floatType
call HDF_readDatasetFloat(dataset, HDF_head, hdferr, fval)
if (hdferr.ne.0) then
  res = 8
  return
end if

dataset = SC_float1D
call HDF_readDatasetFloatArray1D(dataset, dims1, HDF_head, hdferr, farr1)
if (hdferr.ne.0) then
  res = 9
  return
end if

dataset = SC_float2D
call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, farr2)
if (hdferr.ne.0) then
  res = 10 
  return
end if

dataset = SC_float3D
call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, farr3)
if (hdferr.ne.0) then
  res = 11
  return
end if

dataset = SC_float4D
call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, farr4)
if (hdferr.ne.0) then
  res = 12
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
! compare the entries with the stored values
isum = 20
if (fval.ne.fval_save) isum = isum + 1
if (sum(farr1-farr1_save).ne.0) isum = isum + 2
if (sum(farr2-farr2_save).ne.0) isum = isum + 4
if (sum(farr3-farr3_save).ne.0) isum = isum + 8
if (sum(farr4-farr4_save).ne.0) isum = isum + 16

if (isum.eq.20) then
  res = 0
else
  res = isum
end if

!====================================

end subroutine HDFfloatExecuteTest


end module HDFfloatTest
