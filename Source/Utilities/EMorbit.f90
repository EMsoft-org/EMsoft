! ###################################################################
! Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMorbit.f90
!--------------------------------------------------------------------------
!
! PROGRAM:EMorbit 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print the orbit of a point (inside unit cell)
!! 
!> @date 1/5/99   MDG 1.0 original
!> @date  5/20/01  MDG 2.0 f90
!> @date 4/16/13 MDG 3.0 rewrite
!--------------------------------------------------------------------------
program EMorbit

use local
use typedefs
use HDFsupport
use symmetry
use files
use io

IMPLICIT NONE

real(kind=dbl)                  :: ctmp(192,3)
integer(kind=irg)               :: i,m,n,ans, io_int(1) 
real(kind=sgl)                  :: io_real(3)
character(fnlen)                :: progname, progdesc, gname
type(unitcell)                  :: cell

 progname = 'EMorbit.f90'
 progdesc = 'List the orbit of a given position'
 call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
 call Interpret_Program_Arguments(1,(/ 918 /), progname)
 
 !allocate(cell)        
 cell % SG % SYM_reduce=.TRUE.
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = gname
 call CrystalData(cell)
 ans = 1

 do while (ans.eq.1)
  call ReadValue(' Enter fractional coordinates of atom : ', io_real, 3)
  cell % ATOM_pos(1,1:3) = io_real(1:3)
  m=1
  call CalcOrbit(cell,m,n,ctmp)
  io_int(1) = n
  call WriteValue('   # equivalent atoms in orbit        : ', io_int, 1, "(I3)")

! spit them out 
  do i=1,n
   io_int(1) = i
   io_real(1:3) = ctmp(i,1:3)
   call WriteValue('', io_int, 1, "(1x,i3,' -> (')",advance="no")
   call WriteValue('', io_real, 3, "(f7.4,',',f7.4,',',f7.4,');   ')",advance="no") 
   if (mod(i+1,2).eq.1) then
     call Message(' ', frm = "(A)")
   end if
  end do
  call ReadValue(' Another orbit ? (1/0) ', io_int, 1)
  ans = io_int(1)
 end do 

end program EMorbit
