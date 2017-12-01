! ###################################################################
! Copyright (c) 2014-2017, Marc De Graef/Carnegie Mellon University
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
! EMsoft:configmod.f90
!--------------------------------------------------------------------------
!
! MODULE: configmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief simple module to convert an array of C strings into the EMsoft configuration parameters
!
!> @date 10/28/17 MDG 1.0 original
!--------------------------------------------------------------------------
module configmod

use local
use io
use, intrinsic :: iso_c_binding

IMPLICIT NONE


public :: print_EMsoft_configuration_strings, C2F_configuration_strings

private :: cstrlen ! we keep this private to make sure there are no conflicts with other packages

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: print_EMsoft_configuration_strings
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief prints the entire ConfigStructureType content
!
!> @date 10/28/17 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine print_EMsoft_configuration_strings(CP, fname)
!DEC$ ATTRIBUTES DLLEXPORT :: print_EMsoft_configuration_strings

IMPLICIT NONE

type(ConfigStructureType),INTENT(IN)   :: CP
character(fnlen),OPTIONAL              :: fname

integer(kind=irg)                      :: ii, std

std = 6
if (PRESENT(fname)) then
    open(unit=23,file=trim(fname),status='unknown')
    std = 23
end if

call Message('EMsoft Configuration Strings',stdout=std)
call Message('----------------------------',stdout=std)

call Message(trim(ConfigStructureNames(1))//' : '//trim(CP%EMsoftpathname),stdout=std) 
call Message(trim(ConfigStructureNames(2))//' : '//trim(CP%EMXtalFolderpathname),stdout=std) 
call Message(trim(ConfigStructureNames(3))//' : '//trim(CP%EMdatapathname),stdout=std) 
call Message(trim(ConfigStructureNames(4))//' : '//trim(CP%EMtmppathname),stdout=std) 
call Message(trim(ConfigStructureNames(5))//' : '//trim(CP%EMsoftLibraryLocation),stdout=std) 
call Message(trim(ConfigStructureNames(6))//' : '//trim(CP%EMSlackWebHookURL),stdout=std) 
call Message(trim(ConfigStructureNames(7))//' : '//trim(CP%EMSlackChannel),stdout=std) 
call Message(trim(ConfigStructureNames(8))//' : '//trim(CP%UserName),stdout=std) 
call Message(trim(ConfigStructureNames(9))//' : '//trim(CP%UserLocation),stdout=std) 
call Message(trim(ConfigStructureNames(10))//' : '//trim(CP%UserEmail),stdout=std) 
call Message(trim(ConfigStructureNames(11))//' : '//trim(CP%EMNotify),stdout=std) 
call Message(trim(ConfigStructureNames(12))//' : '//trim(CP%Develop),stdout=std) 
call Message(trim(ConfigStructureNames(13))//' : '//trim(CP%Release),stdout=std) 
call Message(trim(ConfigStructureNames(14))//' : '//trim(CP%h5copypath),stdout=std) 
call Message(trim(ConfigStructureNames(15))//' : '//trim(CP%EMsoftplatform),stdout=std) 
call Message(trim(ConfigStructureNames(16))//' : '//trim(CP%EMsofttestpath),stdout=std) 
call Message(trim(ConfigStructureNames(17))//' : '//trim(CP%EMsoftTestingPath),stdout=std) 
call Message(trim(ConfigStructureNames(18))//' : '//trim(CP%EMsoftversion),stdout=std) 
call Message(trim(ConfigStructureNames(19))//' : '//trim(CP%Configpath),stdout=std) 
call Message(trim(ConfigStructureNames(20))//' : '//trim(CP%Templatepathname),stdout=std) 
call Message(trim(ConfigStructureNames(21))//' : '//trim(CP%Resourcepathname),stdout=std) 
call Message(trim(ConfigStructureNames(22))//' : '//trim(CP%Homepathname),stdout=std) 
call Message(trim(ConfigStructureNames(23))//' : '//trim(CP%OpenCLpathname),stdout=std) 
call Message(trim(ConfigStructureNames(24))//' : '//trim(CP%Templatecodefilename),stdout=std) 
call Message(trim(ConfigStructureNames(25))//' : '//trim(CP%WyckoffPositionsfilename),stdout=std) 
call Message(trim(ConfigStructureNames(26))//' : '//trim(CP%Randomseedfilename),stdout=std) 
call Message(trim(ConfigStructureNames(27))//' : '//trim(CP%EMsoftnativedelimiter),stdout=std) 

if (PRESENT(fname)) then
  close(unit=23,status='keep')
end if

end subroutine print_EMsoft_configuration_strings

!--------------------------------------------------------------------------
!
! SUBROUTINE: C2F_configuration_strings
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Converts string array to EMsoft ConfigStructureType 
!
!> @date 10/28/17 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine C2F_configuration_strings(nstring, cptr, CS) ! bind(C)
!DEC$ ATTRIBUTES DLLEXPORT :: C2F_configuration_strings

IMPLICIT NONE

integer(c_int), INTENT(IN), value     :: nstring
type(c_ptr), INTENT(IN), value        :: cptr
type(ConfigStructureType),INTENT(OUT) :: CS

character(kind=c_char), pointer       :: fptr(:,:)
integer(kind=irg)               	  :: ii, lenstr
character(fnlen)                      :: blank

call c_f_pointer(cptr, fptr, [ fnlen, nstring ])

CS%EMsoftpathname = cstrf(fptr(1:fnlen,1))
CS%EMXtalFolderpathname = cstrf(fptr(1:fnlen,2))
CS%EMdatapathname = cstrf(fptr(1:fnlen,3))
CS%EMtmppathname = cstrf(fptr(1:fnlen,4))
CS%EMsoftLibraryLocation = cstrf(fptr(1:fnlen,5))
CS%EMSlackWebHookURL = cstrf(fptr(1:fnlen,6))
CS%EMSlackChannel = cstrf(fptr(1:fnlen,7))
CS%UserName = cstrf(fptr(1:fnlen,8))
CS%UserLocation = cstrf(fptr(1:fnlen,9))
CS%UserEmail = cstrf(fptr(1:fnlen,10))
CS%EMNotify = cstrf(fptr(1:fnlen,11))
CS%Develop = cstrf(fptr(1:fnlen,12))
CS%Release = cstrf(fptr(1:fnlen,13))
CS%h5copypath = cstrf(fptr(1:fnlen,14))
CS%EMsoftplatform = cstrf(fptr(1:fnlen,15))
CS%EMsofttestpath = cstrf(fptr(1:fnlen,16))
CS%EMsoftTestingPath = cstrf(fptr(1:fnlen,17))
CS%EMsoftversion = cstrf(fptr(1:fnlen,18))
CS%Configpath = cstrf(fptr(1:fnlen,19))
CS%Templatepathname = cstrf(fptr(1:fnlen,20))
CS%Resourcepathname = cstrf(fptr(1:fnlen,21))
CS%Homepathname = cstrf(fptr(1:fnlen,22))
CS%OpenCLpathname = cstrf(fptr(1:fnlen,23))
CS%Templatecodefilename = cstrf(fptr(1:fnlen,24))
CS%WyckoffPositionsfilename = cstrf(fptr(1:fnlen,25))
CS%Randomseedfilename = cstrf(fptr(1:fnlen,26))
CS%EMsoftnativedelimiter = cstrf(fptr(1:fnlen,27))

end subroutine C2F_configuration_strings


!--------------------------------------------------------------------------
!
! FUNCTION: cstrlen
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the length of a null-terminated C string
!
!> @date 10/28/17 MDG 1.0 original
!--------------------------------------------------------------------------
function cstrlen(carray) result(res)

IMPLICIT NONE

character(kind=c_char), INTENT(IN) :: carray(:)
integer(kind=irg)                  :: res

integer(kind=irg)                  :: ii

res = size(carray)
do ii = 1, size(carray)
  if (carray(ii) == C_NULL_CHAR) then
    res = ii - 1
    return
  end if
end do

end function cstrlen

!--------------------------------------------------------------------------
!
! FUNCTION: cstrf
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert a c string into a correctly formatted f string
!
!> @date 11/30/17 MDG 1.0 original
!--------------------------------------------------------------------------
function cstrf(carray) result(res)

IMPLICIT NONE

character(kind=c_char), INTENT(IN) :: carray(:)
character(fnlen)                   :: res

integer(kind=irg)                  :: ii, lenstr

lenstr = cstrlen(carray)
res = ''
do ii = 1, lenstr
    res(ii:ii) = carray(ii)
end do

end function cstrf



end module configmod