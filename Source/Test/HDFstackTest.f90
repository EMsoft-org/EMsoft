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
! EMsoft:HDFstackTest.f90
!--------------------------------------------------------------------------
!
! MODULE: HDFcharTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief test module for the HDF5 push-pop stack
!
!> @date 10/30/16   MDG 1.0 original
!--------------------------------------------------------------------------

module HDFstackTest

use stringconstants

contains 

subroutine HDFstackExecuteTest(res) &
           bind(c, name='HDFstackExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFstackExecuteTest

use,INTRINSIC :: ISO_C_BINDING
use local
use HDF5
use typedefs
use HDFsupport

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)  :: res

character(fnlen)                :: HDFfilename, tmppath, groupname, dataset

integer(kind=irg)               :: ival, hdferr, i
real(kind=sgl)                  :: fval, fval_save

type(HDFobjectStackType),pointer:: HDF_head
character(len=1)                :: EMsoftnativedelimiter

character(kind=c_char)          :: cstr(fnlen)

! HDF_head components:
!  character(LEN=1)                      :: objectType  'f', 'g', 'd', 'a', 't', 's'
!  character(fnlen)                      :: objectName
!  integer(HID_T)                        :: objectID
!  type(HDFobjectStackType),pointer      :: next
!
! currently, the HDFsupport routines only use 'f' and 'g' entries so we only test those.

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
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_stack.h5'

write(*,*) 'writing filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_createFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 1
  return
end if

if (HDF_head%objectType.ne.'f') then
  res = 2
  return
end if

! create a group
groupname = SC_testgroup
hdferr = HDF_createGroup(groupname, HDF_head)

if (HDF_head%objectType.ne.'g') then
  res = 3
  return
end if

groupname = SC_test2group
hdferr = HDF_createGroup(groupname, HDF_head)

if (HDF_head%objectType.ne.'g') then
  res = 4
  return
end if

dataset = SC_integer
ival = 123
hdferr = HDF_writeDatasetInteger(dataset, ival, HDF_head)

! close inner group
call HDF_pop(HDF_head)

if (HDF_head%objectType.ne.'g') then
  res = 5
  return
end if

! close out group
call HDF_pop(HDF_head)

if (HDF_head%objectType.ne.'f') then
  res = 6
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================

end subroutine HDFstackExecuteTest


end module HDFstackTest
