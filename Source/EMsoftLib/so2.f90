! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:so2.f90
!--------------------------------------------------------------------------
!
! MODULE: so2
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief everything that has to do with sampling on the 2-sphere
!
!> @date 07/28/20 MDG 1.0 original
!--------------------------------------------------------------------------
module so2 

use local
use constants 
use typedefs
use Lambert 

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: delete_SO2list
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief delete a linked list of vectors
!
!> @param top linked list
!
!> @date 07/28/20 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine delete_SO2list(top)
!DEC$ ATTRIBUTES DLLEXPORT :: delete_SO2list

IMPLICIT NONE

type(SO2pointd),pointer,INTENT(INOUT)  :: top
!f2py intent(in,out) ::  top

type(SO2pointd),pointer                :: ltail, ltmp

! deallocate the entire linked list before returning, to prevent memory leaks
ltail => top
ltmp => ltail % next
do 
  deallocate(ltail)
  if (.not. associated(ltmp)) EXIT
  ltail => ltmp
  ltmp => ltail % next
end do

end subroutine delete_SO2list

!--------------------------------------------------------------------------
!
! SUBROUTINE: getBSEDetectorGrid
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief delete a linked list of unit vectors that fall inside an annular BSE detector
!
!> @param nsteps number of points along semi-edge of 2D Lambert space
!> @param niz z-component of unit vector pointing to inner edge of BSE detector
!> @param noz z-component of unit vector pointing to outer edge of BSE detector
!> @param SO2list linked list of vectors
!> @param SO2cnt number of entries in the linked list 
!
!> @date 07/28/20 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getBSEDetectorGrid(nsteps, niz, noz, SO2list, SO2cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: getBSEDetectorGrid

IMPLICIT NONE 

integer(kind=irg),INTENT(IN)                :: nsteps 
real(kind=dbl),INTENT(IN)                   :: niz 
real(kind=dbl),INTENT(IN)                   :: noz 
type(SO2pointd),pointer,INTENT(INOUT)       :: SO2list
integer(kind=irg),INTENT(INOUT)             :: SO2cnt 

real(kind=dbl)                              :: ds, xy(2), xyz(3) 
integer(kind=irg)                           :: ierr, ix, iy 
type(SO2pointd), pointer                    :: SO2tmp, SO2tmp2 

! set the counter to zero
SO2cnt = 0

! make sure the linked lists are empty
if (associated(SO2list)) then
  SO2tmp => SO2list%next
  SO2tmp2 => SO2list
  do
    deallocate(SO2tmp2)  
    if (.not. associated(SO2tmp) ) EXIT
    SO2tmp2 => SO2tmp
    SO2tmp => SO2tmp%next
  end do
  nullify(SO2list)
else
  nullify(SO2list)
end if

ds = 1.D0/dble(nsteps)

! loop through the square Lambert grid and test each point to see whether or not 
! it falls inside the annular BSE detector; this is a simple test in which we 
! compute the angle between the unit vector on the 2-sphere and the optical axis.
! We are actually just comparing the third direction cosine.
do ix = -nsteps,nsteps
    do iy = -nsteps,nsteps
        xy = (/ dble(ix), dble(iy) /) * ds 
        xyz = LambertSquareToSphere(xy, ierr)
! does the z-component fall in the correct range ?
        if ((xyz(3).le.niz).and.(xyz(3).ge.noz)) then 
! add this point to the linked list 
          if (.not.associated(SO2list)) then
              allocate(SO2list)
              SO2tmp => SO2list
          else
              allocate(SO2tmp%next)
              SO2tmp => SO2tmp%next
          end if
          nullify(SO2tmp%next)
! and set the correct values 
          SO2tmp%sql = xy 
          SO2tmp%nvec = xyz
          SO2cnt = SO2cnt + 1 
        end if
    end do 
end do 

end subroutine getBSEDetectorGrid





end module so2
