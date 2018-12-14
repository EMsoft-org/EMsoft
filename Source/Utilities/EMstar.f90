! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:EMstar.f90
!--------------------------------------------------------------------------
!
! PROGRAM: 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Prints the star of a reciprocal lattice position for a given space group;
!
!> @details  also lists the Fourier coefficients of the lattice potential
!>  
! 
!> @date 01/05/99 MDG 1.0 original
!> @date 05/18/01 MDG 2.0 f90
!> @date 03/07/02 MDG 2.1 added CalcUcg support
!> @date 04/16/13 MDG 3.0 rewrite
!> @date 06/13/14 MDG 4.0 rewrite without global variables
!--------------------------------------------------------------------------
program EMstar

use local
use typedefs
use HDFsupport
use io
use files
use gvectors
use symmetry
use diffraction

IMPLICIT NONE

integer(kind=irg)       :: g(3),gg(3),ans,n,i, io_int(3)
real(kind=dbl)          :: kk(3),stmp(0:47,3)
logical                 :: first, loadingfile
character(1)            :: space
character(fnlen)        :: progname, progdesc, gname
type(unitcell),pointer  :: cell
type(gnode)             :: rlp

 progname = 'EMstar.f90'
 progdesc = 'Computes the star of a reciprocal lattice vector'
 call EMsoft(progname, progdesc)

 allocate(cell)

! initialize crystal
 cell % SG % SYM_reduce=.FALSE.
 space = 'r'
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = gname
 call CrystalData(cell,.TRUE.)
 call GetVoltage(cell, rlp)
 call CalcPositions(cell,'v')

 if (cell % SG % SYM_centrosym) then 
  call Message('structure is centrosymmetric',frm = "(A/)") 
 else 
  call Message('structure is non-centrosymmetric', frm = "(A/)") 
 end if
 ans = 1

 do while (ans.eq.1)
  call Message(' ',"(/A)")
  call ReadValue(' Enter reciprocal lattice coordinates [I] : ', io_int, 3)
  g(1:3) = io_int(1:3)
  kk = dble(g)
  call CalcStar(cell,kk,n,stmp,space)
  io_int(1) = n
  call WriteValue(' Number of equivalent planes in star = ', io_int, 1, "(I3)")
! compute and display the structure factor
  first = .TRUE.
  do i=0,n-1
   gg(1:3) = nint(stmp(i,1:3))
   call CalcUcg(cell,rlp,gg)
   call Printrlp(rlp,first)
  end do
  call ReadValue(' Another star ? (1/0) ', io_int, 1)
  ans = io_int(1)
 end do

end program EMstar
