! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMlistSG.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMlistSG
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief List space group equivalent positions
!
!> @details  list general equivalent positions for a 
!> given space group.  This is a simple program
!> to illustrate how one can use the space group matrices
!>  
! 
!> @date 1/5/99   MDG 1.0 original
!> @date  5/29/01  MDG 2.0 f90
!> @date 4/16/13 MDG 3.0 rewrite
!--------------------------------------------------------------------------
program EMlistSG

use local
use constants
use io
use typedefs
use symmetry

IMPLICIT NONE

character(3)   				:: pos
integer(kind=irg)        		:: p(4),ii,jj,i, io_int(1)
real(kind=sgl)           		:: ppp, io_real(1)
character(fnlen)                      :: progname, progdesc
type(unitcell)        	               :: cell
character(fnlen)                      :: mess

 progname = 'EMlistSG.f90'
 progdesc = 'List equivalent positions for arbitrary space group'
 call EMsoft(progname, progdesc)

 allocate(cell)
 
 cell % SG% SYM_reduce=.TRUE.
 pos = 'xyz'
 call ReadValue(' Enter Space Group number : ', io_int, 1) 
 cell % SYM_SGnum = io_int(1)
 call GenerateSymmetry(cell,.TRUE.)

 call Message('  Space Group Symbol       : '//SYM_SGname(cell % SYM_SGnum), frm = "(A)")

 io_int(1) = cell%SG % SYM_MATnum
 call WriteValue(' number of operators      : ', io_int, 1, "(I3)")

! loop over all symmetry matrices
 do i=1,cell%SG % SYM_MATnum
  io_int(1) = i; 
  call WriteValue(' ', io_int, 1,"(1x,i3,2x,'-> (',$)")

! loop over all rows
  do ii=1,3

! loop over colums (get the numbers)
   p(1:3)=int(cell%SG % SYM_data(i,ii,1:3)) 
   ppp = sngl(cell%SG % SYM_data(i,ii,4))

! print each entry 
! first the part containing x, y, and/or z
   do jj=1,3
    if (p(jj).ne.0) then
     mess(1:1)='+'
     if (p(jj).eq.-1) mess(1:1)='-'
     mess(2:2) = pos(jj:jj)
     call Message(mess, frm = "(A2,$)")
    end if
   end do 

! if there is a translation component, print it
   if (ppp.ne.0.0) then
    mess(1:1)='+'
    if (ppp.lt.0.0) mess(1:1)='-'
    call Message(mess, frm = "(A1,$)");
    io_real(1) = abs(ppp); 
    call WriteValue('', io_real, 1, "(f5.3,$)")
   end if

! print a comma, or close the brackets and do a newline
   if (ii.ne.3) then 
     mess(1:1) = ','; call Message(mess, frm = "(A1,$)")
    else
     mess(1:1) = ')'; call Message(mess, frm = "(A1)")
   end if

  end do
 end do 

end program EMlistSG
