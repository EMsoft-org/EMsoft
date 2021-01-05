! ###################################################################
! Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
module HDFsupportTest
  

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFsupportTest_ExecuteTest
!
!> @author y
!
!> @brief 
!
!> @note 
!
!> @date
!--------------------------------------------------------------------------
subroutine HDFsupportTest_ExecuteTest(result) &
bind(c, name='HDFsupportTest_ExecuteTest') ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFsupportTest_ExecuteTest

use local
use HDF5
use typedefs
use HDFsupport
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)               :: result

result = 0


end subroutine HDFsupportTest_ExecuteTest

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFsupportTest_TestRWChar
!
!> @author y
!
!> @brief 
!
!> @note 
!
!> @date
!--------------------------------------------------------------------------
subroutine HDFsupportTest_TestRWChar(result) &
bind(c, name='HDFsupportTest_TestRWChar') ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFsupportTest_TestRWChar

use local
use HDF5
use typedefs
use HDFsupport
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)                :: result
character(fnlen)                              :: HDFfilename, tpn
character(len=1)                              :: EMsoftnativedelimiter
type(HDFobjectStackType)                      :: HDF_head
integer(kind=irg)                             :: hdferr
integer                                       :: dim1, dim2, dim3, dim4
integer(HSIZE_T)                              :: dims1(1), dims2(2), dims3(3), dims4(4)
integer*4                                     :: i1, i2, i3, i4
character(fnlen)                              :: dataset
character, DIMENSION(:), ALLOCATABLE          :: array1
character, DIMENSION(:, :), ALLOCATABLE       :: array2
character, DIMENSION(:, :, :), ALLOCATABLE    :: array3
character, DIMENSION(:, :, :, :), ALLOCATABLE :: array4

! determine the pathname delimiter character
EMsoftnativedelimiter = EMsoft_getEMsoftnativedelimiter()


! nullify the push/pop stack pointer
nullify(HDF_head%next)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

tpn = EMsoft_toNativePath(EMsoft_getEMsoftTestingPath())
HDFfilename = trim(tpn)//EMsoftnativedelimiter//'HDFsupport_Char.h5'//CHAR(0)
hdferr =  HDF_createFile(trim(HDFfilename), HDF_head)
if(hdferr.lt.0) then
  result = hdferr
  return
end if
dim1 = 10
dim2 = 10
dim3 = 10
dim4 = 20

dims1 = (/ dim1 /)
dims2 = (/ dim1, dim2 /)
dims3 = (/ dim1, dim2, dim3 /)
dims4 = (/ dim1, dim2, dim3, dim4 /)

write (*,*) "Before Allocate"
ALLOCATE (array1(dim1))
ALLOCATE (array2(dim1,dim2))
ALLOCATE (array3(dim1,dim2,dim3))
ALLOCATE (array4(dim1,dim2,dim3,dim4))

write (*,*) "Before Loops..."

do i1=0,dim1
  array1(i1+1) = char(i1)
  do i2=0,dim2
    array2(i1+1,i2+1) = char(i1 * i2)
    do i3=0,dim3
      array3(i1+1,i2+1,i3+1) = char(i1 * i2 * i3)
      do i4=0,dim4
        array4(i1+1,i2+1,i3+1,i4+1) = char(i1 * i2 * i3 * i4)
      end do
    end do
  end do
end do

write (*,*) "Before Writing"

dataset = 'char_1D'//CHAR(0)
hdferr = HDF_writeDatasetCharArray1D(dataset, array1, dims1, HDF_head)
if(hdferr.lt.0) then
  result = -1000
  DEALLOCATE (array1)
  DEALLOCATE (array2)
  DEALLOCATE (array3)
  DEALLOCATE (array4)
  return
end if

write (*,*) "Before DEALLOCATE"


DEALLOCATE (array1)
 DEALLOCATE (array2)
! DEALLOCATE (array3)
! DEALLOCATE (array4)

write (*,*) "Before HDF_pop"

call HDF_pop(HDF_head,.TRUE.)

write (*,*) "Before h5close_EMsoft"


! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
if(hdferr.lt.0) then
  result = hdferr
end if


end subroutine HDFsupportTest_TestRWChar




end module HDFsupportTest

!HDF_writeEMheader
!HDF_push
!HDF_pop
!HDF_stackdump
!HDF_handleError
!-HDF_createFile
!-HDF_openFile
!-HDF_createGroup
!-HDF_openGroup
!-HDF_openDataset
!-HDF_writeDatasetTextFile
!-HDF_readfromTextfile
!HDF_readDatasetStringArray
!-HDF_extractDatasetTextfile
!HDF_writeDatasetStringArray
!HDF_writeDatasetCharArray1D
!HDF_writeDatasetCharArray2D
!HDF_writeDatasetCharArray3D
!HDF_writeDatasetCharArray4D
!-HDF_writeDatasetInteger
!-HDF_writeDatasetInteger1byteArray1D
!-HDF_writeDatasetIntegerArray1D
!-HDF_writeDatasetIntegerArray2D
!-HDF_writeDatasetIntegerArray3D
!-HDF_writeDatasetIntegerArray4D
!-HDF_writeDatasetFloat
!-HDF_writeDatasetDouble
!-HDF_writeDatasetFloatArray1D
!-HDF_writeDatasetFloatArray2D
!-HDF_writeDatasetFloatArray3D
!-HDF_writeDatasetFloatArray4D
!-HDF_writeDatasetDoubleArray1D
!-HDF_writeDatasetDoubleArray2D
!-HDF_writeDatasetDoubleArray3D
!-HDF_writeDatasetDoubleArray4D

!HDF_readDatasetCharArray1D
!HDF_readDatasetCharArray2D
!HDF_readDatasetCharArray3D
!HDF_readDatasetCharArray4D
!-HDF_readDatasetInteger
!-HDF_readDatasetIntegerArray1D
!-HDF_readDatasetIntegerArray2D
!-HDF_readDatasetIntegerArray3D
!-HDF_readDatasetIntegerArray4D
!-HDF_readDatasetFloat
!-HDF_readDatasetFloatArray1D
!-HDF_readDatasetFloatArray2D
!-HDF_readDatasetFloatArray3D
!-HDF_readDatasetFloatArray4D
!-HDF_readDatasetDouble
!-HDF_readDatasetDoubleArray1D
!-HDF_readDatasetDoubleArray2D
!-HDF_readDatasetDoubleArray3D
!-HDF_readDatasetDoubleArray4D

!!!!! HyperSlab Routines






