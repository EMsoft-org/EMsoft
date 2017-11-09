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
subroutine print_EMsoft_configuration_strings(CP)
!DEC$ ATTRIBUTES DLLEXPORT :: print_EMsoft_configuration_strings

IMPLICIT NONE

type(ConfigStructureType),INTENT(IN)   :: CP
integer(kind=irg)                      :: ii

call Message('EMsoft Configuration Strings')
call Message('----------------------------')

call Message(trim(ConfigStructureNames(1))//' : '//trim(CP%EMsoftpathname)) 
call Message(trim(ConfigStructureNames(2))//' : '//trim(CP%EMXtalFolderpathname)) 
call Message(trim(ConfigStructureNames(3))//' : '//trim(CP%EMdatapathname)) 
call Message(trim(ConfigStructureNames(4))//' : '//trim(CP%EMtmppathname)) 
call Message(trim(ConfigStructureNames(5))//' : '//trim(CP%EMsoftLibraryLocation)) 
call Message(trim(ConfigStructureNames(6))//' : '//trim(CP%EMSlackWebHookURL)) 
call Message(trim(ConfigStructureNames(7))//' : '//trim(CP%EMSlackChannel)) 
call Message(trim(ConfigStructureNames(8))//' : '//trim(CP%UserName)) 
call Message(trim(ConfigStructureNames(9))//' : '//trim(CP%UserLocation)) 
call Message(trim(ConfigStructureNames(10))//' : '//trim(CP%UserEmail)) 
call Message(trim(ConfigStructureNames(11))//' : '//trim(CP%EMNotify)) 
call Message(trim(ConfigStructureNames(12))//' : '//trim(CP%Develop)) 
call Message(trim(ConfigStructureNames(13))//' : '//trim(CP%Release)) 
call Message(trim(ConfigStructureNames(14))//' : '//trim(CP%h5copypath)) 
call Message(trim(ConfigStructureNames(15))//' : '//trim(CP%EMsoftplatform)) 
call Message(trim(ConfigStructureNames(16))//' : '//trim(CP%EMsofttestpath)) 
call Message(trim(ConfigStructureNames(17))//' : '//trim(CP%EMsoftTestingPath)) 
call Message(trim(ConfigStructureNames(18))//' : '//trim(CP%EMsoftversion)) 
call Message(trim(ConfigStructureNames(19))//' : '//trim(CP%Configpath)) 
call Message(trim(ConfigStructureNames(20))//' : '//trim(CP%Templatepathname)) 
call Message(trim(ConfigStructureNames(21))//' : '//trim(CP%Resourcepathname)) 
call Message(trim(ConfigStructureNames(22))//' : '//trim(CP%Homepathname)) 
call Message(trim(ConfigStructureNames(23))//' : '//trim(CP%OpenCLpathname)) 
call Message(trim(ConfigStructureNames(24))//' : '//trim(CP%Templatecodefilename)) 
call Message(trim(ConfigStructureNames(25))//' : '//trim(CP%WyckoffPositionsfilename)) 
call Message(trim(ConfigStructureNames(26))//' : '//trim(CP%Randomseedfilename)) 
call Message(trim(ConfigStructureNames(27))//' : '//trim(CP%EMsoftnativedelimiter)) 

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

character(kind=c_char), pointer 	  :: fptr(:,:)
integer(kind=irg)               	  :: ii, lenstr

call c_f_pointer(cptr, fptr, [ fnlen, nstring ])

lenstr = cstrlen(fptr(:,1))
CS%EMsoftpathname = transfer(fptr(1:lenstr,1), CS%EMsoftpathname)
lenstr = cstrlen(fptr(:,2))
CS%EMXtalFolderpathname = transfer(fptr(1:lenstr,2), CS%EMXtalFolderpathname)
lenstr = cstrlen(fptr(:,3))
CS%EMdatapathname = transfer(fptr(1:lenstr,3), CS%EMdatapathname)
lenstr = cstrlen(fptr(:,4))
CS%EMtmppathname = transfer(fptr(1:lenstr,4), CS%EMtmppathname)
lenstr = cstrlen(fptr(:,5))
CS%EMsoftLibraryLocation = transfer(fptr(1:lenstr,5), CS%EMsoftLibraryLocation)
lenstr = cstrlen(fptr(:,6))
CS%EMSlackWebHookURL = transfer(fptr(1:lenstr,6), CS%EMSlackWebHookURL)
lenstr = cstrlen(fptr(:,7))
CS%EMSlackChannel = transfer(fptr(1:lenstr,7), CS%EMSlackChannel)
lenstr = cstrlen(fptr(:,8))
CS%UserName = transfer(fptr(1:lenstr,8), CS%UserName)
lenstr = cstrlen(fptr(:,9))
CS%UserLocation = transfer(fptr(1:lenstr,9), CS%UserLocation)
lenstr = cstrlen(fptr(:,10))
CS%UserEmail = transfer(fptr(1:lenstr,10), CS%UserEmail)
lenstr = cstrlen(fptr(:,11))
CS%EMNotify = transfer(fptr(1:lenstr,11), CS%EMNotify)
lenstr = cstrlen(fptr(:,12))
CS%Develop = transfer(fptr(1:lenstr,12), CS%Develop)
lenstr = cstrlen(fptr(:,13))
CS%Release = transfer(fptr(1:lenstr,13), CS%Release)
lenstr = cstrlen(fptr(:,14))
CS%h5copypath = transfer(fptr(1:lenstr,14), CS%h5copypath)
lenstr = cstrlen(fptr(:,15))
CS%EMsoftplatform = transfer(fptr(1:lenstr,15), CS%EMsoftplatform)
lenstr = cstrlen(fptr(:,16))
CS%EMsofttestpath = transfer(fptr(1:lenstr,16), CS%EMsofttestpath)
lenstr = cstrlen(fptr(:,17))
CS%EMsoftTestingPath = transfer(fptr(1:lenstr,17), CS%EMsoftTestingPath)
lenstr = cstrlen(fptr(:,18))
CS%EMsoftversion = transfer(fptr(1:lenstr,18), CS%EMsoftversion)
lenstr = cstrlen(fptr(:,19))
CS%Configpath = transfer(fptr(1:lenstr,19), CS%Configpath)
lenstr = cstrlen(fptr(:,20))
CS%Templatepathname = transfer(fptr(1:lenstr,20), CS%Templatepathname)
lenstr = cstrlen(fptr(:,21))
CS%Resourcepathname = transfer(fptr(1:lenstr,21), CS%Resourcepathname)
lenstr = cstrlen(fptr(:,22))
CS%Homepathname = transfer(fptr(1:lenstr,22), CS%Homepathname)
lenstr = cstrlen(fptr(:,23))
CS%OpenCLpathname = transfer(fptr(1:lenstr,23), CS%OpenCLpathname)
lenstr = cstrlen(fptr(:,24))
CS%Templatecodefilename = transfer(fptr(1:lenstr,24), CS%Templatecodefilename)
lenstr = cstrlen(fptr(:,25))
CS%WyckoffPositionsfilename = transfer(fptr(1:lenstr,25), CS%WyckoffPositionsfilename)
lenstr = cstrlen(fptr(:,26))
CS%Randomseedfilename = transfer(fptr(1:lenstr,26), CS%Randomseedfilename)
lenstr = cstrlen(fptr(:,27))
CS%EMsoftnativedelimiter = transfer(fptr(1:lenstr,27), CS%EMsoftnativedelimiter)

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

do ii = 1, size(carray)
  if (carray(ii) == C_NULL_CHAR) then
    res = ii - 1
    return
  end if
end do
res = ii

end function cstrlen


end module configmod