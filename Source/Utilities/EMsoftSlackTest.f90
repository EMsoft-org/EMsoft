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
!--------------------------------------------------------------------------
! EMsoft:EMsoftSlackTest.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMsoftSlackTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMsoftSlackTest sends a simple test message to a Slack channel
!
!> @date  08/19/17  MDG 1.0 Original version
!--------------------------------------------------------------------------
program EMsoftSlackTest

use local
use io
use notifications
use files

IMPLICIT NONE

character(fnlen),ALLOCATABLE      :: MessageLines(:)
integer(kind=irg)                 :: NumLines, i
character(fnlen)                  :: MessageTitle, line, progname
character(100)                    :: c

! deal with the command line arguments, if any
progname = 'EMsoftSlackTest'
call Interpret_Program_Arguments(1,(/ 922 /), progname)

if (trim(EMsoft_getNotify()).ne.'Off') then
    NumLines = 1
    allocate(MessageLines(NumLines))
    write (*,*) 'Enter a test sentence :' 
    read (*,"(A)") line

    call hostnm(c)
 
    MessageLines(1) = trim(line)
    MessageTitle = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, MessageTitle)
else
  call Message('Notifications are turned off in your EMsoftConfig.json configuration file')
end if

end program EMsoftSlackTest
