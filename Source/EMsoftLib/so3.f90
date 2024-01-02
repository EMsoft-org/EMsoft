! ###################################################################
! Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:so3.f90
!--------------------------------------------------------------------------
!
! MODULE: so3
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief everything that has to do with sampling of rotation space SO(3)
!
!> @todo verify that this is correct for point groups with multiple settings, eg, 3m, 32, ...
!
!> @date 05/29/14 MDG 1.0 original
!> @date 10/02/14 MDG 2.0 removed globals + rewrite
!> @date 01/01/15 MDG 2.1 added IsinsideFZ function, also used in dictionary indexing approach
!> @date 01/17/15 MDG 2.2 added gridtype option to SampleRFZ
!> @date 03/03/16 MDG 2.3 added uniform sampling for constant misorientations
!> @date 04/01/17 MDG 3.0 start to add FZs for two-phase systems (e.g., cubic-hexagonal, etc.)
!> @date 02/21/19 MDG 3.1 adds MacKenzie distributions (theoretical expressions)
!--------------------------------------------------------------------------
module so3

use local

IMPLICIT NONE

! sampler routine
public :: SampleRFZ, IsinsideFZ, CubochoricNeighbors

! logical functions to determine if point is inside specific FZ
!private :: insideCyclicFZ, insideDihedralFZ, insideCubicFZ
public:: insideCyclicFZ, insideDihedralFZ, insideCubicFZ


contains

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! Routine to return the FZtype and FZorder parameters for single or two-phase
! fundamental zone (FZ) computations; this includes all the FZ types from the 
! following paper:
!
! "Representation of Orientation and Disorientation data for Cubic, Hexagonal, 
! Tetragonal, and Orthorhombic Crystals", A. Heinz and P. Neumann, Acta Cryst. A47, 
! 780-789 (1991)
!
! this routine also allows for icosahedral symmetry, although this is not part 
! of the paper above.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE: getFZtypeandorder
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside the relevant FZ
!
!> @param pgnum1 point group number for phase 1
!> @param FZtype FZ type
!> @param FZorder FZ order
!> @param pgnum2 point group number for phase 2 (optional)
!
!> @date 04/02/17 MDG 1.0 new routine, needed for two-phase disorientations
!--------------------------------------------------------------------------
recursive subroutine getFZtypeandorder(pgnum1,FZtype,FZorder,pgnum2) 
!DEC$ ATTRIBUTES DLLEXPORT :: getFZtypeandorder

use typedefs
use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)              :: pgnum1
integer(kind=irg),INTENT(OUT)             :: FZtype
integer(kind=irg),INTENT(OUT)             :: FZorder
integer(kind=irg),INTENT(IN),OPTIONAL     :: pgnum2

integer(kind=irg)                         :: thisFZType
logical                                   :: twophase

! 0 -> x  (no symmetry or unbounded FZ for the cyclic symmetries)
! 1 -> a  mixed cubic-hexagonal FZ  -> FZtype = 6
! 2 -> b  mixed FZ -> FZtype = 7
! 3 -> c  octahedral FZ
! 4 -> d  tetrahedral FZ
! 5 -> e  24-sided prismatic FZ -> FZtype = 8
! 6 -> f  622 hexagonal dihedral FZ
! 7 -> g  422 octagonal dihedral FZ
! 8 -> h  32 trigonal dihedral FZ
! 9 -> i  222 dihedral FZ

! we reserve FZtype = 5 for icosahedral symmetry, which is under development on a separate code branch

twophase = .FALSE.
if (present(pgnum2)) twophase = .TRUE.

if (twophase.eqv..TRUE.) then
  thisFZtype = FZtypeTable(pgnum1,pgnum2)

  select case(thisFZtype)
    case (0)
      ! this needs some more work since we need to properly handle the cyclic groups of FZtype = 1 ...
      FZtype = 0
      FZorder = 0
    case (1)
      FZtype = 6
      FZorder = 0
    case (2)
      FZtype = 7
      FZorder = 0
    case (3)
      FZtype = 4
      FZorder = 0
    case (4)
      FZtype = 3
      FZorder = 0
    case (5)
      FZtype = 8
      FZorder = 0
    case (6)
      FZtype = 2
      FZorder = 6
    case (7)
      FZtype = 2
      FZorder = 4
    case (8)
      FZtype = 2
      FZorder = 3
    case (9)
      FZtype = 2
      FZorder = 2
  end select
else  ! single phase so use the old way of doing things...
  FZtype = FZtarray(pgnum1)
  FZorder = FZoarray(pgnum1)
end if

end subroutine getFZtypeandorder

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! We define a number of logical routines, that decide whether or not 
! a point in Rodrigues representation lies inside the fundamental zone (FZ)
! for a given crystal symmetry. This follows the Morawiec@Field paper:
!
! A. Morawiec & D. P. Field (1996) Rodrigues parameterization for orientation 
! and misorientation distributions, Philosophical Magazine A, 73:4, 1113-1130, 
! DOI: 10.1080/01418619608243708
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! FUNCTION: IsinsideFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside the relevant FZ
!
!> @param rod Rodrigues coordinates  (double precision)
!> @param FZtype FZ type
!> @param FZorder FZ order
!> @param qFZ quaternion that rotates the FZ into a new orientation (optional)
!
!> @date 01/01/15 MDG 1.0 new routine, needed for dictionary indexing approach
!> @date 06/04/15 MDG 1.1 corrected infty to inftyd (double precision infinity)
!> @date 04/02/17 MDG 1.2 expanded FZ types to include misorientation FZs and icosahedral
!> @date 07/04/19 MDG 1.3 added optional parameter to rotate the FZ into an arbitrary orientation
!--------------------------------------------------------------------------
recursive function IsinsideFZ(rod,FZtype,FZorder,qFZ) result(insideFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: IsinsideFZ

use constants
use math
use quaternions
use rotations

IMPLICIT NONE

real(kind=dbl), INTENT(IN)              :: rod(4)
integer(kind=irg),INTENT(IN)            :: FZtype
integer(kind=irg),INTENT(IN)            :: FZorder
real(kind=dbl),INTENT(IN),OPTIONAL      :: qFZ(4)
logical                                 :: insideFZ

real(kind=dbl)                          :: newrod(4), qu(4)

! do we need to rotate the FZ ? (we do this by rotating rod in the opposite way)
if (present(qFZ)) then 
  qu = ro2qu(rod)
  qu = quat_mult( qFZ, quat_mult(qu, conjg(qFZ) )  )
  newrod = qu2ro(qu)
else
  newrod = rod
end if

insideFZ = .FALSE.

! dealing with 180 rotations is needed only for 
! FZtypes 0 and 1; the other FZs are always finite.
  select case (FZtype)
    case (0)
      insideFZ = .TRUE.   ! all points are inside the FZ
    case (1)
      insideFZ = insideCyclicFZ(newrod,FZtype,FZorder)        ! infinity is checked inside this function
    case (2)
      if (rod(4).ne.inftyd()) insideFZ = insideDihedralFZ(newrod,FZorder)
    case (3)
      if (rod(4).ne.inftyd()) insideFZ = insideCubicFZ(newrod,'tet')
    case (4)
      if (rod(4).ne.inftyd()) insideFZ = insideCubicFZ(newrod,'oct')
    case (5) ! icosahedral symmetry
      if (rod(4).ne.inftyd()) insideFZ = insideIcosahedralFZ(newrod)
    case (6) ! cubic-hexagonal misorientation FZ
      if (rod(4).ne.inftyd()) insideFZ = insideCubeHexFZ(newrod)
    case (7)
!     if (rod(4).ne.inftyd) insideFZ = insideCubicFZ(rod,'oct')
    case (8)
!     if (rod(4).ne.inftyd) insideFZ = insideCubicFZ(rod,'oct')
  end select

end function IsinsideFZ


!--------------------------------------------------------------------------
!
! FUNCTION: insideIcosahedralFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside cosahedral FZ?
!
!> @param rod Rodrigues coordinates  (double precision)
!
!> @date 03/24/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function insideIcosahedralFZ(rod) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideIcosahedralFZ

use constants

real(kind=dbl), INTENT(IN)        :: rod(4)
logical                           :: res

real(kind=dbl)                    :: dval, rv(3)
integer(kind=irg)                 :: i, j

res = .FALSE.

dval=0.32491969623290632616D0  ! sqrt(1-2/sqrt(5)))
rv(1:3) = rod(1:3)*rod(4)
j = 0
do i=1,12
  if (DOT_PRODUCT(IcoVertices(1:3,i),rv)+dval.ge.0.D0) j = j+1
end do
if (j.eq.12) res = .TRUE.

end function insideIcosahedralFZ

!--------------------------------------------------------------------------
!
! FUNCTION: insideCyclicFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside cyclic FZ (for 2, 3, 4, and 6-fold)?
!
!> @param rod Rodrigues coordinates  (double precision)
!> @param FZtype symmetry type
!> @param FZorder depending on main symmetry axis
!
!> @date 05/12/14 MDG 1.0 original
!> @date 10/02/14 MDG 2.0 rewrite
!> @date 06/04/15 MDG 2.1 corrected infty to inftyd (double precision infinity)
!--------------------------------------------------------------------------
recursive function insideCyclicFZ(rod,FZtype,FZorder) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideCyclicFZ

use constants
use math

IMPLICIT NONE

real(kind=dbl), INTENT(IN)                :: rod(4)
integer(kind=irg), INTENT(IN)             :: FZtype
integer(kind=irg), INTENT(IN)             :: FZorder

logical                                   :: res

res = .FALSE.

if (rod(4).ne.inftyd()) then
  if ((FZtype.eq.1.).and.(FZorder.eq.2)) then
! check the y-component vs. tan(pi/2n)
    res = dabs(rod(2)*rod(4)).le.LPs%BP(FZorder)
  else
! check the z-component vs. tan(pi/2n)
    res = dabs(rod(3)*rod(4)).le.LPs%BP(FZorder)
  end if
else
  if ((FZtype.eq.1.).and.(FZorder.eq.2)) then
    if(rod(2) .eq. 0.D0) res = .TRUE.
  else
    if (rod(3).eq.0.D0) res = .TRUE.
  end if
endif

end function insideCyclicFZ

!--------------------------------------------------------------------------
!
! FUNCTION: insideDihedralFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside dihedral FZ (for 2, 3, 4, and 6-fold)?
!
!> @param rod Rodrigues coordinates (double precision)
!> @param order depending on main symmetry axis
!
!> @todo for now, we ignore here the fact that, among others, the 3m point group can be oriented in two ways;
!> @todo this should be fixable in the future with an additional optional argument
!
!> @date 05/12/14  MDG 1.0 original
!> @date 10/02/14  MDG 2.0 rewrite
!> @date 02/21/19  MDG 2.1 correction of initial truncation factor from 1.5 to sqrt(3); only affected 222 point group
!--------------------------------------------------------------------------
recursive function insideDihedralFZ(rod,order) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideDihedralFZ

use constants

IMPLICIT NONE

real(kind=dbl), INTENT(IN)                :: rod(4)
integer(kind=irg), INTENT(IN)             :: order

logical                                   :: res, c1, c2
real(kind=dbl)                            :: r(3)
real(kind=dbl),parameter                  :: r1 = 1.00D0
real(kind=dbl),allocatable                :: polygonvertex(:,:)
integer(kind=irg)                         :: inout

if (rod(4).gt.sqrt(3.D0)) then 
  res = .FALSE.
else
  r(1:3) = rod(1:3) * rod(4)

  ! first, check the z-component vs. tan(pi/2n)  (same as insideCyclicFZ)
  c1 = dabs(r(3)).le.LPs%BP(order)
  res = .FALSE.

  ! check the square boundary planes if c1=.TRUE.
  if (c1) then
    select case (order)
      case (2)
        c2 = (dabs(r(1)).le.r1).and.(dabs(r(2)).le.r1)
      case (3)
        c2 =          dabs( LPs%srt*r(1)+0.5D0*r(2)).le.r1
        c2 = c2.and.( dabs( LPs%srt*r(1)-0.5D0*r(2)).le.r1 )
        c2 = c2.and.( dabs(r(2)).le.r1 )
      case (4)
        c2 = (dabs(r(1)).le.r1).and.(dabs(r(2)).le.r1)
        c2 = c2.and.((LPs%r22*dabs(r(1)+r(2)).le.r1).and.(LPs%r22*dabs(r(1)-r(2)).le.r1))
      case (6)
        c2 =          dabs( 0.5D0*r(1)+LPs%srt*r(2)).le.r1
        c2 = c2.and.( dabs( LPs%srt*r(1)+0.5D0*r(2)).le.r1 )
        c2 = c2.and.( dabs( LPs%srt*r(1)-0.5D0*r(2)).le.r1 )
        c2 = c2.and.( dabs( 0.5D0*r(1)-LPs%srt*r(2)).le.r1 )
        c2 = c2.and.( dabs(r(2)).le.r1 )
        c2 = c2.and.( dabs(r(1)).le.r1 )

      ! add the 2-D quasi crystal type for 822, 1022, and 1222 rotational groups
      case (8)
        c2 = .FALSE.
        allocate(polygonvertex(order*2, 2))
        polygonvertex = 0.D0
        call getVertex(order, polygonvertex)
        inout = PNPOLY(r(1),r(2),polygonvertex(1:2*order,1),polygonvertex(1:2*order,2),order*2)
        if(inout .ge. 0) c2 = .TRUE.

      case (10)
        c2 = .FALSE.
        allocate(polygonvertex(order*2, 2))
        polygonvertex = 0.D0
        call getVertex(order, polygonvertex)
        inout = PNPOLY(r(1),r(2),polygonvertex(1:2*order,1),polygonvertex(1:2*order,2),order*2)
        if(inout .ge. 0) c2 = .TRUE.

      case(12)
        c2 = .FALSE.
        allocate(polygonvertex(order*2, 2))
        polygonvertex = 0.D0
        call getVertex(order, polygonvertex)
        inout = PNPOLY(r(1),r(2),polygonvertex(1:2*order,1),polygonvertex(1:2*order,2),order*2)
        if(inout .ge. 0) c2 = .TRUE.

    end select
    res = c2
  end if
end if

end function insideDihedralFZ

!--------------------------------------------------------------------------
!
! FUNCTION: insideCubicFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside cubic FZ (octahedral or tetrahedral)?
!
!> @param rod Rodrigues coordinates  (double precision)
!> @param ot 'oct' or 'tet', depending on symmetry
!
!> @date 05/12/14 MDG 1.0 original
!> @date 10/02/14 MDG 2.0 rewrite
!> @date 01/03/15 MDG 2.1 correction of boundary error; simplification of octahedral planes
!> @date 06/04/15 MDG 2.2 simplified handling of components of r
!--------------------------------------------------------------------------
recursive function insideCubicFZ(rod,ot) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideCubicFZ

use constants

IMPLICIT NONE

real(kind=dbl), INTENT(IN)                :: rod(4)
character(3), INTENT(IN)                  :: ot

logical                                   :: res, c1, c2
real(kind=dbl)                            :: r(3)
real(kind=dbl),parameter                  :: r1  = 1.0D0
real(kind=dbl),parameter                  :: eps = 1.0D-8

r(1:3) = rod(1:3) * rod(4)

res = .FALSE.

! primary cube planes (only needed for octahedral case)
if (ot.eq.'oct') then
  c1 = (maxval(dabs(r)) - LPS%BP(4) .le. eps) 
else 
  c1 = .TRUE.
end if

! octahedral truncation planes, both for tetrahedral and octahedral point groups
c2 = ((dabs(r(1))+dabs(r(2))+dabs(r(3))) - r1 .le. eps)

! if both c1 and c2, then the point is inside
if (c1.and.c2) res = .TRUE.

end function insideCubicFZ

!--------------------------------------------------------------------------
!
! FUNCTION: insideCubeHexFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside combined cubic-hexagonal FZ?
!
!> @note For details on this test, see section 8 in "Representation of Orientation and
!> Disorientation data for Cubic, Hexagonal, Tetragonal, and Orthorhombic Crystals", A. Heinz
!> and P. Neumann, Acta Cryst. A47, 780-789 (1991)
!
!> @param rod Rodrigues coordinates  (double precision)
!
!> @date 04/01/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function insideCubeHexFZ(rod) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideCubeHexFZ

use constants

IMPLICIT NONE

real(kind=dbl), INTENT(IN)                :: rod(4)

logical                                   :: res
real(kind=dbl)                            :: r(3)
real(kind=dbl),parameter                  :: r1 = 0.414213562373095D0, r2 = 0.131652497587396D0, &
                                             alpha = 0.267949192431123D0, beta = 0.464101615137755D0
real(kind=dbl),parameter                  :: eps = 1.0D-6


r(1:3) = rod(1:3) * rod(4)

res = .FALSE.

if ( (r(2).ge.0.D0).and.(r(3).ge.0.D0) ) then 
  if ( ((alpha * (r(1)+r(3)) + r(2)) - beta .le. eps).and.( (alpha * (r(2)-r(3)) + r(1)) - beta .le. eps) ) then 
    if ( (r(1) - r1 .le. eps) .and. (r(2) - r1 .le. eps) .and. (r(3) - r2 .le. eps) ) res = .TRUE.
  end if
end if 

end function insideCubeHexFZ

!--------------------------------------------------------------------------
!
! FUNCTION: delete_FZlist
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief delete a linked list of rodrigues vectors
!
!> @param FZlist linked list
!
!> @date 02/20/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine delete_FZlist(top)
!DEC$ ATTRIBUTES DLLEXPORT :: delete_FZlist

use typedefs 

IMPLICIT NONE

type(FZpointd),pointer,INTENT(INOUT)  :: top
!f2py intent(in,out) ::  top

type(FZpointd),pointer                :: ltail, ltmp

! deallocate the entire linked list before returning, to prevent memory leaks
ltail => top
ltmp => ltail % next
do 
  deallocate(ltail)
  if (.not. associated(ltmp)) EXIT
  ltail => ltmp
  ltmp => ltail % next
end do

end subroutine delete_FZlist




!--------------------------------------------------------------------------
!
! FUNCTION: SampleRFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate a uniform sampling of a Rodriguess FZ
!
!> @note This routine fills in a linked list FZlist of Rodrigues points that 
!> are inside a specific fundamental zone determined by the sample point group;
!> this list can then be further dealt with in the calling program.  
!>
!> Here's how you would use this routine in a main program:
!>
!> use so3
!>
!> integer(kind=irg)       :: FZcnt, nsteps, pgnum
!> type(FZpointd),pointer  :: FZlist, FZtmp
!> 
!> nullify(FZlist)
!> FZcnt = 0
!> nsteps = 10
!> pgnum = 32
!> call sampleRFZ(nsteps, pgnum, FZcnt, FZlist)
!> 
!> Then you can access all the entries in the list and, for instance, convert them to Euler angles...
!>
!> FZtmp => FZlist                        ! point to the top of the list
!> do i = 1, FZcnt                        ! loop over all entries
!>   eu = ro2eu(FZtmp%rod)                ! convert to Euler angles
!>   do something with eu                 ! for instance, write eu to a file
!>   FZtmp => FZtmp%next                  ! point to the next entry
!> end do
!>
!> If you just want to look at the first 10 entries on the list and show all other orientation representations:
!>
!> type(orientationtyped):: ot
!> 
!> FZtmp => FZlist
!> do i = 1,10
!>   ot = init_orientation(FZtmp%rod,'ro')
!>   call print_orientation(ot)
!>   FZtmp => FZtmp%next
!> end do
!
!> @param nsteps number of steps along semi-edge in cubochoric grid
!> @param pgnum point group number to determine the appropriate Rodrigues fundamental zone
!> @param gridtype (input) 0 for origin-centered grid; 1 for grid with origin at box center
!> @param FZcnt (output) number of points inside fundamental zone
!> @param FZlist (output) linked list of points inside fundamental zone
!
!> @date 05/12/14 MDG 1.0 original
!> @date 10/02/14 MDG 2.0 rewrite, removed all globals, added function arguments
!> @date 09/15/15 MDG 2.1 removed explicit origin allocation; changed while to do loops.
!> @date 01/17/15 MDG 2.2 added gridtype option
!> @date 05/22/16 MDG 2.3 correction for monoclinic symmetry with twofold axis along b, not c !!!
!> @date 07/04/19 MDG 2.4 added option to rotate FZ before sampling
!--------------------------------------------------------------------------
recursive subroutine SampleRFZ(nsteps,pgnum,gridtype,FZcnt,FZlist, qFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: SampleRFZ

use typedefs
use constants
use rotations

IMPLICIT NONE

integer(kind=irg), INTENT(IN)        :: nsteps
integer(kind=irg), INTENT(IN)        :: pgnum
integer(kind=irg), INTENT(IN)        :: gridtype
integer(kind=irg),INTENT(OUT)        :: FZcnt                ! counts number of entries in linked list
type(FZpointd),pointer,INTENT(OUT)   :: FZlist               ! pointers
real(kind=dbl),INTENT(IN),OPTIONAL   :: qFZ(4)

real(kind=dbl)                       :: x, y, z, rod(4), delta, shift, sedge, ztmp
type(FZpointd), pointer              :: FZtmp, FZtmp2
integer(kind=irg)                    :: FZtype, FZorder, i, j, k
logical                              :: b, rotateFZ = .FALSE.


if (present(qFZ)) rotateFZ = .TRUE.

! cube semi-edge length s = 0.5D0 * LPs%ap
! step size for sampling of grid; total number of samples = (2*nsteps+1)**3
sedge = 0.5D0 * LPs%ap
delta = sedge / dble(nsteps)
if (gridtype.eq.0) then
  shift = 0.0D0
else
  shift = 0.5D0
end if

! set the counter to zero
FZcnt = 0

! make sure the linked lists are empty
if (associated(FZlist)) then
  FZtmp => FZlist%next
  FZtmp2 => FZlist
  do
    deallocate(FZtmp2)  
    if (.not. associated(FZtmp) ) EXIT
    FZtmp2 => FZtmp
    FZtmp => FZtmp%next
  end do
  nullify(FZlist)
else
  nullify(FZlist)
end if

! determine which function we should call for this point group symmetry
FZtype = FZtarray(pgnum)
FZorder = FZoarray(pgnum)

! note that when FZtype is cyclic (1) and FZorder is 2, then we must rotate the 
! rotation axis to lie along the b (y) direction, not z !!!!

! loop over the cube of volume pi^2; note that we do not want to include
! the opposite edges/facets of the cube, to avoid double counting rotations
! with a rotation angle of 180 degrees.  This only affects the cyclic groups.

 do i=-nsteps+1,nsteps
  x = (dble(i)+shift)*delta
  do j=-nsteps+1,nsteps
   y = (dble(j)+shift)*delta
   do k=-nsteps+1,nsteps
    z = (dble(k)+shift)*delta
! make sure that this point lies inside the cubochoric cell
    if (maxval( (/ abs(x), abs(y), abs(z) /) ).le.sedge) then

! convert to Rodrigues representation
      rod = cu2ro( (/ x, y, z /) )

! If insideFZ=.TRUE., then add this point to the linked list FZlist and keep
! track of how many points there are on this list
       if (rotateFZ.eqv..TRUE.) then 
         b = IsinsideFZ(rod,FZtype,FZorder,qFZ)
       else
         b = IsinsideFZ(rod,FZtype,FZorder)
       end if
       if (b) then 
        if (.not.associated(FZlist)) then
          allocate(FZlist)
          FZtmp => FZlist
! in DEBUG mode, there is a strange error here ... 
! if gridtype = 0 then we must add the identity rotation here
! currently, the identity is not automatically recognized since the 
! returns from several rotation routines also equal the identity when
! the point is actually invalid... 
!          if (gridtype.eq.0) then
!            FZcnt = 1
!            FZtmp%rod = (/ 0.D0, 0.D0, 1.D0, 0.D0 /)
!            allocate(FZtmp%next)
!            FZtmp => FZtmp%next
!            nullify(FZtmp%next)
!          end if 
        else
          allocate(FZtmp%next)
          FZtmp => FZtmp%next
        end if
        nullify(FZtmp%next)
! if monoclinic, then reorder the components !!!
!        if ((FZtype.eq.1).and.(FZorder.eq.2)) then
!          ztmp = rod(3)
!          rod(3) = rod(1)
!          rod(1) = rod(2)
!          rod(2) = ztmp
!        end if
        FZtmp%rod = rod
        FZtmp%gridpt(1:3) = (/i, j, k/)
        FZcnt = FZcnt + 1
       end if
    end if
  end do
 end do
end do

end subroutine SampleRFZ

!--------------------------------------------------------------------------
!
! SUBROUTINE: CubochoricNeighbors
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief find the nearest neighbors of a point in s03 space, given the point
!> and the step size used in the previous meshing. to be used in multi resolution
!> indexing programs, specifically the PED, ECP and EBSD indexing. we're not worrying
!> about keeping the neighbors in the FZ. that can just be done later.
!
!> @param cub cubochoric coordinates  (double precision)
!> @param stepsize stepsize of last mesh. the mesh will be stepsize/2
!
!> @date 04/07/15 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CubochoricNeighbors(cubneighbor,nn,cub,stepsize)
!DEC$ ATTRIBUTES DLLEXPORT :: CubochoricNeighbors

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nn ! number of nearest neighbor in each direction (should be an odd number for symmetric meshing)
real(kind=dbl),INTENT(OUT)              :: cubneighbor(3,(2*nn+1)**3)
real(kind=dbl),INTENT(IN)               :: cub(3)
real(kind=dbl),INTENT(IN)               :: stepsize

integer(kind=irg)                       :: ii,jj,kk,ll,idx

if (dabs(stepsize) .gt. LPs%ap) then
    write(*,*) "ERROR in subroutine CubochoricNeighbors: Step size is larger than edge length of the cube"
    stop
end if

do ii = -nn,nn
    do jj = -nn,nn
        do kk = -nn,nn
            idx  = (ii + nn)*(2*nn + 1)**2 + (jj + nn)*(2*nn + 1) + (kk + nn + 1)
            cubneighbor(1:3,idx) = cub + stepsize/2.D0*(/ii,jj,kk/)
            do ll = 1,3
                if (cubneighbor(ll,idx) .lt.  -0.5D0 * LPs%ap) then
                    cubneighbor(ll,idx) = cubneighbor(ll,idx) + LPs%ap
                else if (cubneighbor(ll,idx) .gt.  0.5D0 * LPs%ap) then
                    cubneighbor(ll,idx) = cubneighbor(ll,idx) - LPs%ap
                end if
            end do
        end do
    end do
end do


end subroutine CubochoricNeighbors

!--------------------------------------------------------------------------
!
! SUBROUTINE: SamplefcctwinRFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief uniformly sample the 32-based FZ for an fcc twin.
!
!> @param nsteps number of steps along semi-edge in cubochoric grid
!> @param FZcnt (output) number of points inside fundamental zone
!> @param FZlist (output) linked list of points inside fundamental zone
!
!> @date 09/17/16 MDG 1.0 fcc twin FZ for 60@[111] (also MacKenzie cell)
!--------------------------------------------------------------- -----------
recursive subroutine SamplefcctwinRFZ(nsteps,FZcnt,FZlist)
!DEC$ ATTRIBUTES DLLEXPORT :: SamplefcctwinRFZ

use local
use constants
use typedefs
use rotations
use quaternions

IMPLICIT NONE

integer(kind=irg), INTENT(IN)        :: nsteps
integer(kind=irg),INTENT(OUT)        :: FZcnt                ! counts number of entries in linked list
type(FZpointd),pointer,INTENT(OUT)   :: FZlist                ! pointers

real(kind=dbl),allocatable           :: vertex(:,:), normals(:,:)
integer(kind=irg),allocatable        :: faces(:,:)
real(kind=dbl)                       :: r21(3), r23(3), rv(3)
integer(kind=irg)                    :: i, j, nv, nf, nn

real(kind=dbl)                       :: x, y, z, s, rod(4), rodt(4), delta, rval, ro(3), dp
type(FZpointd), pointer              :: FZtmp, FZtmp2
logical                              :: inside


! a point is inside a polyhedron if the dot product of a vector from that
! point to any vertex of each face with the face normal is positive

! this routine is specifically written for the fcc twin 60°@[111]; the FZ in 
! that case has the following vertex positions with respect to the standard
! 432 rfz (determined with the EMRFZ program):
nv = 14
nf = 18
nn = 18
allocate(vertex(3,nv), faces(4,nf), normals(3,nn))
vertex(1:3, 1) = (/0.3333334, 0.3333333, 0.3333333/);
vertex(1:3, 2) = (/0.0906927, 0.0906927, 0.3333333/);
vertex(1:3, 3) = (/0.0098125, 0.2524531, 0.2524532/);
vertex(1:3, 4) = (/0.0906928, 0.3333333, 0.0906928/);
vertex(1:3, 5) = (/0.2524532, 0.2524531, 0.0098125/);
vertex(1:3, 6) = (/0.3333334, 0.0906927, 0.0906927/);
vertex(1:3, 7) = (/0.2524532, 0.0098125, 0.2524531/);
vertex(1:3, 8) = (/-0.2524531,-0.2524531,-0.0098124/);
vertex(1:3, 9) = (/-0.3333333,-0.0906927,-0.0906926/);
vertex(1:3,10) = (/-0.2524531,-0.0098124,-0.2524531/);
vertex(1:3,11) = (/-0.0906926,-0.0906926,-0.3333333/);
vertex(1:3,12) = (/-0.0098124,-0.2524531,-0.2524530/);
vertex(1:3,13) = (/-0.0906926,-0.3333333,-0.0906926/);
vertex(1:3,14) = (/-0.3333333,-0.3333333,-0.3333333/);


! next, get the face vertex numbers (triangles and squares) in counterclockwise
! order when looking towards the origin; triangle faces first, then the rectangles
faces(1:4,1) = (/ 1, 3, 2, 0 /)
faces(1:4,2) = (/ 1, 4, 3, 0 /)
faces(1:4,3) = (/ 1, 5, 4, 0 /)
faces(1:4,4) = (/ 1, 6, 5, 0 /)
faces(1:4,5) = (/ 1, 7, 6, 0 /)
faces(1:4,6) = (/ 1, 2, 7, 0 /)

faces(1:4,7) = (/ 14, 8, 9, 0 /)
faces(1:4,8) = (/ 14, 9,10, 0 /)
faces(1:4,9) = (/ 14,10,11, 0 /)
faces(1:4,10) = (/ 14,11,12, 0 /)
faces(1:4,11) = (/ 14,12,13, 0 /)
faces(1:4,12) = (/ 14,13, 8, 0 /)

faces(1:4,13) = (/ 2, 3, 9, 8 /)
faces(1:4,14) = (/ 3, 4,10, 9 /)
faces(1:4,15) = (/ 4, 5,11,10 /)
faces(1:4,16) = (/ 5, 6,12,11 /)
faces(1:4,17) = (/ 6, 7,13,12 /)
faces(1:4,18) = (/ 7, 2, 8,13 /)

! next we get the outward normal for each of the faces
! we'll get the outward normal by computing the cross product of 2-1 and 2-3
do i=1, nn
  r21(1:3) = vertex(1:3,faces(3,i)) - vertex(1:3,faces(2,i))
  r23(1:3) = vertex(1:3,faces(1,i)) - vertex(1:3,faces(2,i))
  normals(1:3,i) = (/ r21(2)*r23(3)-r21(3)*r23(2), r21(3)*r23(1)-r21(1)*r23(3), r21(1)*r23(2)-r21(2)*r23(1) /)
  write (*,*) i, dot_product(vertex(1:3,faces(3,i)),normals(1:3,i))
end do

! here we do the regular cubochoric sampling, but use the dot product test to 
! determine whether or not the point is inside the twin RFZ

! cube semi-edge length
s = 0.5D0 * LPs%ap

! step size for sampling of grid; total number of samples = (2*nsteps+1)**3
delta = s/dble(nsteps)

! set the counter to zero
FZcnt = 0

! make sure the linked lists are empty
if (associated(FZlist)) then
  FZtmp => FZlist%next
  FZtmp2 => FZlist
  do
    deallocate(FZtmp2)  
    if (.not. associated(FZtmp) ) EXIT
    FZtmp2 => FZtmp
    FZtmp => FZtmp%next
  end do
  nullify(FZlist)
end if

! we always want the identity rotation to be the first one on the list
! it is automatically skipped later on...
allocate(FZlist)
FZtmp => FZlist
nullify(FZtmp%next)
FZtmp%rod = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
FZcnt = 1

! loop over the cube of volume pi^2; note that we do not want to include
! the opposite edges/facets of the cube, to avoid double counting rotations
! with a rotation angle of 180 degrees.  This only affects the cyclic groups.
x = -s
do while (x.lt.s)
  y = -s
  do while (y.lt.s)
    z = -s
    do while (z.lt.s)

     if ((x.ne.0.D0).and.(y.ne.0.D0).and.(z.ne.0.D0)) then
! convert to Rodrigues representation
      rod = cu2ro( (/ x, y, z /) )
      ro(1:3) = rod(1:3)*rod(4)

! assume the point is inside and start the tests for each of the face normals
      inside = .TRUE.
      FACELOOP: do i=1,nn
         rv(1:3) = vertex(1:3,faces(1,i)) - ro(1:3)
         dp = DOT_PRODUCT(rv, normals(1:3,i)) 
         if (dp.lt.0.D0) then
           inside = .FALSE.
           exit FACELOOP
         end if
      end do FACELOOP

! If inside=.TRUE., then add this point to the linked list FZlist and keep
! track how many points there are on this list
       if (inside) then 
        allocate(FZtmp%next)
        FZtmp => FZtmp%next
        nullify(FZtmp%next)
        FZtmp%rod = rod
        FZcnt = FZcnt + 1
       end if
     
     end if
    z = z + delta
  end do
  y = y + delta
 end do
 x = x + delta
end do

! that's it.
write (*,*) 'nsteps, delta, s = ',nsteps,delta,s
write (*,*) 'FZcnt = ',FZcnt

end subroutine SamplefcctwinRFZ


!--------------------------------------------------------------------------
!
! SUBROUTINE: SampleRFZtwin
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief use a quaternion to rotate the fundamental zone, then sample it 
!> this is useful for twins etc, hence the name of the routine...
!
!> @param nsteps number of steps along semi-edge in cubochoric grid
!> @param pgnum point group number to determine the appropriate Rodrigues fundamental zone
!> @param qt rotation quaternion
!> @param FZcnt (output) number of points inside fundamental zone
!> @param FZlist (output) linked list of points inside fundamental zone

!
!> @date 04/07/15 SS  1.0 original
!--------------------------------------------------------------------------
recursive subroutine SampleRFZtwin(nsteps,pgnum,qt,FZcnt,FZlist)
!DEC$ ATTRIBUTES DLLEXPORT :: SampleRFZtwin

use local
use constants
use typedefs
use rotations
use quaternions

IMPLICIT NONE

integer(kind=irg), INTENT(IN)        :: nsteps
integer(kind=irg), INTENT(IN)        :: pgnum
real(kind=sgl),INTENT(IN)            :: qt(4)
integer(kind=irg),INTENT(OUT)        :: FZcnt                ! counts number of entries in linked list
type(FZpointd),pointer,INTENT(OUT)   :: FZlist               ! pointers

real(kind=dbl)                       :: x, y, z, s, rod(4), rodt(4), delta, rval, ro(3)
type(FZpointd), pointer              :: FZtmp, FZtmp2
integer(kind=irg)                    :: FZtype, FZorder

! cube semi-edge length
s = 0.5D0 * LPs%ap

! step size for sampling of grid; total number of samples = (2*nsteps+1)**3
delta = s/dble(nsteps)

! set the counter to zero
FZcnt = 0

! make sure the linked lists are empty
if (associated(FZlist)) then
  FZtmp => FZlist%next
  FZtmp2 => FZlist
  do
    deallocate(FZtmp2)  
    if (.not. associated(FZtmp) ) EXIT
    FZtmp2 => FZtmp
    FZtmp => FZtmp%next
  end do
  nullify(FZlist)
end if

! we always want the identity rotation to be the first one on the list
! it is automatically skipped later on...
allocate(FZlist)
FZtmp => FZlist
nullify(FZtmp%next)
FZtmp%rod = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
FZcnt = 1

! determine which function we should call for this point group symmetry
FZtype = FZtarray(pgnum)
FZorder = FZoarray(pgnum)

! loop over the cube of volume pi^2; note that we do not want to include
! the opposite edges/facets of the cube, to avoid double counting rotations
! with a rotation angle of 180 degrees.  This only affects the cyclic groups.
x = -s
do while (x.lt.s)
  y = -s
  do while (y.lt.s)
    z = -s
    do while (z.lt.s)

     if ((x.ne.0.D0).and.(y.ne.0.D0).and.(z.ne.0.D0)) then
! convert to Rodrigues representation
      rod = cu2ro( (/ x, y, z /) )
! convert to an actual vector
      ro(1:3) = rod(1:3)
! then apply the twinning quaternion
      ro = quat_Lp(dble(qt), ro)
! convert back to a Rodrigues vector
      rodt = rod
      rodt(1:3) = ro(1:3)

! If insideFZ=.TRUE., then add this point to the linked list FZlist and keep
! track of how many points there are on this list
       if (IsinsideFZ(rodt,FZtype,FZorder)) then 
        allocate(FZtmp%next)
        FZtmp => FZtmp%next
        nullify(FZtmp%next)
        FZtmp%rod = rod
        FZcnt = FZcnt + 1
       end if
     
     end if
    z = z + delta
  end do
  y = y + delta
 end do
 x = x + delta
end do

! that's it.
write (*,*) 'pgnum, nsteps, delta, s = ',pgnum, nsteps,delta,s
write (*,*) 'FZtype, FZorder = ',FZtype,FZorder
write (*,*) 'FZcnt = ',FZcnt

end subroutine SampleRFZtwin

!--------------------------------------------------------------------------
!
! SUBROUTINE: sample_isoCube
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief sample a centered cube surface inside the Cubochoric cube for a given misorientation angle
!
!> @details linked list wil have a length of 6(N-1)^2+2 entries
!
!> @param misang the misorientation angle [degrees]
!> @param N number of points along cube semi-edge
!> @param CMcnt the number of components in the returned linked list
!> @param CMlist (output) linked list of Rodrigues vectors
!
!> @date 03/03/16 MDG 1.0 original
!> @date 04/19/16 MDG 1.1 changed value of N to be along the semi-edge instead of the edge
!--------------------------------------------------------------------------
recursive subroutine sample_isoCube(misang, N, CMcnt, CMlist) ! CM = Constant Misorientation
!DEC$ ATTRIBUTES DLLEXPORT :: sample_isoCube

use constants
use typedefs
use rotations

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: misang       ! desired misorientation angle (degrees)
integer(kind=irg),INTENT(IN)            :: N            ! desired number of sampling points along cube edge
integer(kind=irg),INTENT(INOUT)         :: CMcnt        ! number of entries in linked list
!f2py intent(in,out) ::  CMcnt        ! number of entries in linked list
type(FZpointd),pointer                  :: CMlist       ! pointer to start of linked list

type(FZpointd),pointer                  :: CMtmp, CMtmp2
real(kind=dbl)                          :: edge, misangr, dx, cu(3), x, y, z
integer(kind=irg)                       :: i, j, k

! initialize parameters
CMcnt = 0

! convert the misorientation angle to radians
misangr = misang * cPi/180.D0

! make sure the linked list is empty
if (associated(CMlist)) then
  CMtmp => CMlist%next
  CMtmp2 => CMlist
  do
    deallocate(CMtmp2)  
    if (.not. associated(CMtmp) ) EXIT
    CMtmp2 => CMtmp
    CMtmp => CMtmp%next
  end do
  nullify(CMlist)
end if

! allocate the linked list
allocate(CMlist)
CMtmp => CMlist

! set the cube edge length based on the misorientation angle
edge = (cPi * (misangr-sin(misangr)))**(1.D0/3.D0)  * 0.5D0
dx = edge / dble(N)

write (*,*) ' edge = ', edge, '; delta = ', dx

! and generate the linked list of surface points

! do the x-y bottom and top planes first (each have N^2 points)
do i=-N,N
  x = dble(i)*dx
  do j=-N,N
    y = dble(j)*dx
! add the point to the list 
    cu = (/ x, y, edge /)
    CMtmp%rod = cu2ro(cu)
    CMcnt = CMcnt + 1
    allocate(CMtmp%next)
    CMtmp => CMtmp%next
    nullify(CMtmp%next)
! and its mirror image in the top plane
    cu = (/ x, y, -edge /)
    CMtmp%rod = cu2ro(cu)
    CMcnt = CMcnt + 1
    allocate(CMtmp%next)
    CMtmp => CMtmp%next
    nullify(CMtmp%next)
  end do
end do

! then we do the y-z planes; each have N*(N-2) points
do j=-N,N
  y =  dble(j)*dx
  do k=-N+1,N-1
    z = dble(k)*dx
! add the point to the list 
    cu = (/ edge, y, z /)
    CMtmp%rod = cu2ro(cu)
    CMcnt = CMcnt + 1
    allocate(CMtmp%next)
    CMtmp => CMtmp%next
    nullify(CMtmp%next)
! and its mirror image in the top plane
    cu = (/ -edge, y, z /)
    CMtmp%rod = cu2ro(cu)
    CMcnt = CMcnt + 1
    allocate(CMtmp%next)
    CMtmp => CMtmp%next
    nullify(CMtmp%next)
  end do
end do

! and finally the x-z planes, with (N-2)^2 points each
do i=-N+1,N-1
  x = dble(i)*dx
  do k=-N+1,N-1
    z = dble(k)*dx
! add the point to the list 
    cu = (/ x, edge, z /)
    CMtmp%rod = cu2ro(cu)
    CMcnt = CMcnt + 1
    allocate(CMtmp%next)
    CMtmp => CMtmp%next
    nullify(CMtmp%next)
! and its mirror image in the top plane
    cu = (/ x, -edge, z /)
    CMtmp%rod = cu2ro(cu)
    CMcnt = CMcnt + 1
    allocate(CMtmp%next)
    CMtmp => CMtmp%next
    nullify(CMtmp%next)
  end do
end do

end subroutine sample_isoCube

!--------------------------------------------------------------------------
!
! SUBROUTINE: sample_isoCubeFilled
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief sample a centered cube inside the Cubochoric cube for a given misorientation angle
!
!> @details This routine is different from the sample_isoCube routine in that it 
!> generates ALL the points inside the centered cube instead of just the points on
!> the outer surface.  This can be useful to uniformly sample a small volume of orientation
!> space around some point out to a given misorientation angle.  Since the sampling has concentric
!> cubes, all the samples can be subdivided into discrete misorientation classes.
!> The linked list wil have a length of N^3
!
!> @param misang the misorientation angle [degrees]
!> @param N number of points along cube edge
!> @param CMcnt the number of components in the returned linked list
!> @param CMlist (output) linked list of Rodrigues vectors
!
!> @date 04/08/16 MDG 1.0 original
!> @date 04/19/16 MDG 1.1 changed value of N to be along the semi-edge instead of the edge
!--------------------------------------------------------------------------
recursive subroutine sample_isoCubeFilled(misang, N, CMcnt, CMlist) ! CM = Constant Misorientation
!DEC$ ATTRIBUTES DLLEXPORT :: sample_isoCubeFilled

use constants
use typedefs
use rotations

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: misang       ! desired misorientation angle (degrees)
integer(kind=irg),INTENT(IN)            :: N            ! desired number of sampling points along cube edge
integer(kind=irg),INTENT(INOUT)         :: CMcnt        ! number of entries in linked list
!f2py intent(in,out) ::  CMcnt        ! number of entries in linked list
type(FZpointd),pointer                  :: CMlist       ! pointer to start of linked list

type(FZpointd),pointer                  :: CMtmp, CMtmp2
real(kind=dbl)                          :: edge, misangr, dx, cu(3), x, y, z, xc, yc, zc
integer(kind=irg)                       :: i, j, k

! initialize parameters
CMcnt = 0

! convert the misorientation angle to radians
misangr = misang * cPi/180.D0

! make sure the linked list is empty
if (associated(CMlist)) then
  CMtmp => CMlist%next
  CMtmp2 => CMlist
  do
    deallocate(CMtmp2)  
    if (.not. associated(CMtmp) ) EXIT
    CMtmp2 => CMtmp
    CMtmp => CMtmp%next
  end do
  nullify(CMlist)
end if

! allocate the linked list
allocate(CMlist)
CMtmp => CMlist

! set the cube edge length based on the misorientation angle
edge = (cPi * (misangr-sin(misangr)))**(1.D0/3.D0) * 0.5D0
dx = edge / dble(N)

! and generate the linked list of surface points
! loop over the (2N+1)^3 points
do i=-N,N
  x = dble(i)*dx
  do j=-N,N
    y = dble(j)*dx
    do k=-N,N
      z = dble(k)*dx
! add the point to the list 
      cu = (/ x, y, z /)
      CMtmp%rod = cu2ro(cu)
      CMcnt = CMcnt + 1
      allocate(CMtmp%next)
      CMtmp => CMtmp%next
      nullify(CMtmp%next)
    end do
  end do
end do

end subroutine sample_isoCubeFilled

!--------------------------------------------------------------------------
!
! SUBROUTINE: sample_Cone
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief sample a cone centered on a unit vector with apex in the origin and given opening angle
!
!> @param unitvector  unit vector describing the cone axis
!> @param dpmin = cosine of half the cone angle
!> @param N number of points along cube edge
!> @param CMcnt the number of components in the returned linked list
!> @param CMlist (output) linked list of Rodrigues vectors
!
!> @date 02/01/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine sample_Cone(unitvector, dpmin, N, FZtype, FZorder, cnt, list) 
!DEC$ ATTRIBUTES DLLEXPORT :: sample_Cone

use constants
use typedefs
use rotations

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: unitvector(3)! axis of cone
real(kind=dbl),INTENT(IN)               :: dpmin        ! maximum dot product
integer(kind=irg),INTENT(IN)            :: N            ! number of sampling points along cube semi edge
integer(kind=irg),INTENT(IN)            :: FZtype
integer(kind=irg),INTENT(IN)            :: FZorder
integer(kind=irg),INTENT(INOUT)         :: cnt          ! number of entries in linked list
!f2py intent(in,out) ::  cnt          ! number of entries in linked list
type(FZpointd),pointer                  :: list         ! pointer to start of linked list

type(FZpointd),pointer                  :: tmp, tmp2
real(kind=dbl)                          :: dx, rod(4), x, y, z, s, delta, dp, r(3)

! initialize parameters
cnt = 0
! cube semi-edge length
s = 0.5D0 * LPs%ap
! step size for sampling of grid; total number of samples = (2*nsteps+1)**3
delta = s/dble(N)

! make sure the linked list is empty
if (associated(list)) then
  tmp => list%next
  tmp2 => list
  do
    deallocate(tmp2)  
    if (.not. associated(tmp) ) EXIT
    tmp2 => tmp
    tmp => tmp%next
  end do
  nullify(list)
end if

! allocate the linked list and insert the origin
allocate(list)
tmp => list
nullify(tmp%next)
tmp%rod = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
cnt = 1

! and generate the linked list of points inside the cone
x = -s
do while (x.lt.s)
  y = -s
  do while (y.lt.s)
    z = -s
    do while (z.lt.s)

     if ((x.ne.0.D0).and.(y.ne.0.D0).and.(z.ne.0.D0)) then
! convert to Rodrigues representation
      rod = cu2ro( (/ x, y, z /) )
      r = rod(1:3)/sqrt(sum(rod(1:3)**2))
! compute the dot product of this vector and the unitvector
      dp = unitvector(1)*r(1)+unitvector(2)*r(2)+unitvector(3)*r(3)
! conditionally add the point to the list if it lies inside the cone (dpmax <= dp)
      if ((dp.ge.dpmin).and.(IsinsideFZ(rod,FZtype,FZorder))) then
        allocate(tmp%next)
        tmp => tmp%next
        nullify(tmp%next)
        tmp%trod = rod
        cnt = cnt + 1
      end if
     end if

    z = z + delta
  end do
  y = y + delta
 end do
 x = x + delta
end do

end subroutine sample_Cone

!--------------------------------------------------------------------------
!
! SUBROUTINE: sample_Fiber
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief sample a fiber texture with a given fiber axis and angular spread
!
!> @param itmp  array of equivalent unit vectors describing the fiber axis in the crystal frame
!> @param num  number of unique entries in itmp array
!> @param dpmin = cosine of half the cone angle
!> @param N number of points along cube edge
!> @param CMcnt the number of components in the returned linked list
!> @param CMlist (output) linked list of Rodrigues vectors
!
!> @date 08/16/17 MDG 1.0 original
!> @date 08/16/17 MDG 1.1 incorporate family symmetry for the input unitvector
!--------------------------------------------------------------------------
recursive subroutine sample_Fiber(itmp, num, dpmin, N, FZtype, FZorder, cnt, list) 
!DEC$ ATTRIBUTES DLLEXPORT :: sample_Fiber

use constants
use typedefs
use rotations

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: itmp(48,3)   ! equivalent fiber axis unit vectors
integer(kind=irg),INTENT(IN)            :: num
real(kind=dbl),INTENT(IN)               :: dpmin        ! maximum dot product
integer(kind=irg),INTENT(IN)            :: N            ! number of sampling points along cube semi edge
integer(kind=irg),INTENT(IN)            :: FZtype
integer(kind=irg),INTENT(IN)            :: FZorder
integer(kind=irg),INTENT(INOUT)         :: cnt          ! number of entries in linked list
!f2py intent(in,out) ::  cnt          ! number of entries in linked list
type(FZpointd),pointer                  :: list         ! pointer to start of linked list

type(FZpointd),pointer                  :: tmp, tmp2
real(kind=dbl)                          :: dx, qu(4), x, y, z, s, delta, dp, Fr(3), Fn(3), rod(4)
integer(kind=irg)                       :: j

! initialize parameters
cnt = 0
! cube semi-edge length
s = 0.5D0 * LPs%ap
! step size for sampling of grid; total number of samples = (2*nsteps+1)**3
delta = s/dble(N)

! make sure the linked list is empty
if (associated(list)) then
  tmp => list%next
  tmp2 => list
  do
    deallocate(tmp2)  
    if (.not. associated(tmp) ) EXIT
    tmp2 => tmp
    tmp => tmp%next
  end do
  nullify(list)
end if

! allocate the linked list and insert the origin
allocate(list)
tmp => list
nullify(tmp%next)
cnt = 0

! and generate the linked list of points inside the cone
x = -s
do while (x.lt.s)
  y = -s
  do while (y.lt.s)
    z = -s
    do while (z.lt.s)

! convert to Rodrigues representation
      qu = cu2qu( (/ x, y, z /) )
      rod = cu2ro( (/ x, y, z /) )

! loop over the equivalent fiber axis indices
      do j=1,num
        Fr = quat_Lp(conjg(qu), itmp(j,1:3))

! conditionally add the point to the list if it lies inside the cone (dpmax <= dp)
        if ((Fr(3).ge.dpmin).and.(IsinsideFZ(rod,FZtype,FZorder))) then
          tmp%trod = rod
          allocate(tmp%next)
          tmp => tmp%next
          nullify(tmp%next)
          cnt = cnt + 1
        end if
      end do

    z = z + delta
  end do
  y = y + delta
 end do
 x = x + delta
end do

end subroutine sample_Fiber

!--------------------------------------------------------------------------
!
! SUBROUTINE: SampleIsoMisorientation
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Constant Misorientation sampling routine; input list must be generated by sampleCubeSurface
!
!> @param rhozero central Rodrigues point around which constant misorientation sample is requested
!> @param misang the misorientation angle [degrees]
!> @param CMcnt the number of components in the returned linked list
!> @param CMlist (output) linked list of Rodrigues vectors, with transformed vectors in trod(4) entries
!
!> @date 03/03/16 SS/MDG 1.0 original (merged from two separate implementations)
!--------------------------------------------------------------------------
recursive subroutine SampleIsoMisorientation(rhozero, misang, CMcnt, CMlist) ! CM = Constant Misorientation
!DEC$ ATTRIBUTES DLLEXPORT :: SampleIsoMisorientation

use constants
use typedefs
use math

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: rhozero(4)   ! center Rodrigues vector
real(kind=dbl),INTENT(IN)               :: misang       ! desired misorientation angle (degrees)
integer(kind=irg),INTENT(INOUT)         :: CMcnt        ! number of entries in linked list
!f2py intent(in,out) ::  CMcnt        ! number of entries in linked list
type(FZpointd),pointer                  :: CMlist       ! pointer to start of linked list

type(FZpointd),pointer                  :: CMtmp
real(kind=dbl)                          :: rhovec(3), s, vv(3)
integer(kind=irg)                       :: i

! go through the list and transform all points to the spheroid misorientation surface
! the resulting Rodrigues vectors are stored in the trod(4) entry.

rhovec(1:3) = rhozero(1:3) * rhozero(4)

CMtmp => CMlist
do i=1,CMcnt
! get the actual Rodrigues vector
  vv(1:3) = CMtmp%rod(1:3) * CMtmp%rod(4)
! apply the Rodrigues transformation formula
  vv = (-vv + rhovec + cross3(rhovec,vv))/(1.D0 + DOT_PRODUCT(vv,rhovec))
! and convert back to the 4-component format
  s = dsqrt(sum(vv*vv))
  if (s.gt.0.D0) then 
    CMtmp%trod = (/ vv(1)/s, vv(2)/s, vv(3)/s, s /)
  else
    CMtmp%trod = (/ 0.D0, 0.D0, 1.D0, 0.D0 /)
  end if
  CMtmp=>CMtmp%next
end do

end subroutine SampleIsoMisorientation

!--------------------------------------------------------------------------
!
! SUBROUTINE: getEulersfromFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a list of Euler angles from a text file and insert them in a linked list
!
!> @param eulerfile name of the Euler angle file (with usual path handling)
!> @param FZcnt the number of components in the returned linked list
!> @param FZlist (output) linked list of Rodrigues vectors, with transformed vectors in trod(4) entries
!
!> @date 12/22/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getEulersfromFile(eulerfile, FZcnt, FZlist)
!DEC$ ATTRIBUTES DLLEXPORT :: getEulersfromFile

use constants
use typedefs
use rotations

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: eulerfile
integer(kind=irg),INTENT(INOUT)         :: FZcnt        ! number of entries in linked list
!f2py intent(in,out) ::  FZcnt        ! number of entries in linked list
type(FZpointd),pointer                  :: FZlist       ! pointer to start of linked list

character(fnlen)                        :: filepath
character(2)                            :: anglemode
integer(kind=irg)                       :: numang, i
real(kind=dbl)                          :: eu(3), dtor
type(FZpointd),pointer                  :: FZtmp, FZtmp2

dtor = cPi/180.D0

! set the file path
filepath = trim(EMsoft_getEMdatapathname())//trim(eulerfile)
filepath = EMsoft_toNativePath(filepath)

open(unit=53,file=trim(filepath),status='old',action='read')
read (53,*) anglemode
read (53,*) numang

! make sure the linked list is empty
if (associated(FZlist)) then
  FZtmp => FZlist%next
  FZtmp2 => FZlist
  do
    deallocate(FZtmp2)  
    if (.not. associated(FZtmp) ) EXIT
    FZtmp2 => FZtmp
    FZtmp => FZtmp%next
  end do
  nullify(FZlist)
end if

! allocate the linked list
allocate(FZlist)
FZtmp => FZlist

do i=1,numang
  read (53,*) eu(1:3)
  FZtmp%rod = eu2ro(eu*dtor)
  FZcnt = FZcnt + 1
  allocate(FZtmp%next)
  FZtmp => FZtmp%next
  nullify(FZtmp%next)
end do

close(unit=53,status='keep')

end subroutine getEulersfromFile

!--------------------------------------------------------------------------
!
! FUNCTION: IsinsideMFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside the relevant Mackenzie (disorientation) FZ
!
!> @param rod Rodrigues coordinates  (double precision)
!> @param MFZtype MFZ type
!> @param MFZorder MFZ order
!
!> @date 09/09/16 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive function IsinsideMFZ(rod,MFZtype,MFZorder) result(insideMFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: IsinsideMFZ

use constants
use math

IMPLICIT NONE

real(kind=dbl), INTENT(IN)              :: rod(4)
integer(kind=irg),INTENT(IN)            :: MFZtype
integer(kind=irg),INTENT(IN)            :: MFZorder
logical                                 :: insideMFZ

  select case (MFZtype)
    case (0)
      insideMFZ = .TRUE.   ! all points are inside the FZ
    case (1)
      insideMFZ = insideCyclicFZ(rod,MFZtype,MFZorder)        ! infinity is checked inside this function
    case (2)
      if (rod(4).ne.inftyd()) insideMFZ = insideDihedralMFZ(rod,MFZorder)
    case (3)
      if (rod(4).ne.inftyd()) insideMFZ = insideCubicMFZ(rod,'tet')
    case (4)
      if (rod(4).ne.inftyd()) insideMFZ = insideCubicMFZ(rod,'oct')
  end select

end function IsinsideMFZ

!--------------------------------------------------------------------------
!
! FUNCTION: insideCubicMFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside cubic MacKenzie FZ (octahedral or tetrahedral)?
!
!> @param rod Rodrigues coordinates  (double precision)
!> @param ot 'oct' or 'tet', depending on symmetry
!
!> @date 09/09/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function insideCubicMFZ(rod,ot) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideCubicMFZ

use constants

IMPLICIT NONE

real(kind=dbl), INTENT(IN)                :: rod(4)
character(3), INTENT(IN)                  :: ot

logical                                   :: res, c0, c1, c2, c3
real(kind=dbl)                            :: r(3)

res = .FALSE.

! first of all, we need to be inside the regular FZ
c0 = insideCubicFz(rod,ot)

r(1:3) = rod(1:3) * rod(4)

if (ot.eq.'oct') then
  c1 = (c0.and.(r(3).ge.0.D0))
  c2 = (c1.and.(r(2).ge.r(3)))
  c3 = (c2.and.(r(1).ge.r(2))) 
else 
  c1 = (c0.and.(minval(r).ge.0.D0))  ! in the first octant
  if (r(1).le.r(2)) then
    c3 = (c1.and.(r(3).le.r(1)))
  else
    c3 = (c1.and.(r(3).le.r(2)))
  end if
end if

res = c3

end function insideCubicMFZ

!--------------------------------------------------------------------------
!
! FUNCTION: insideDihedralMFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does Rodrigues point lie inside cubic MacKenzie FZ (octahedral or tetrahedral)?
!
!> @param rod Rodrigues coordinates  (double precision)
!> @param ot 'oct' or 'tet', depending on symmetry
!
!> @date 09/09/16 MDG 1.0 original
!> @date 09/15/16 MDG 1.0 completed all orders
!--------------------------------------------------------------------------
recursive function insideDihedralMFZ(rod,MFZorder) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: insideDihedralMFZ

use constants

IMPLICIT NONE

real(kind=dbl), INTENT(IN)                :: rod(4)
integer, INTENT(IN)                       :: MFZorder

logical                                   :: res, c0, c1, c2, c3
real(kind=dbl)                            :: r(3)
real(kind=dbl),parameter                  :: v = 0.57735026918962584D0

! v = 1.0/dsqrt(3.D0)

res = .FALSE.

! first of all, we need to be inside the regular FZ
c0 = insideDihedralFZ(rod,MFZorder)

r(1:3) = rod(1:3) * rod(4)

if (c0) then
select case (MFZorder)
    case (2)
      c2 = (minval(r).ge.0.D0)
    case (3)
      c1 = (minval( (/ r(1), r(3) /) ).ge.0.D0)
      c2 = (c1.and.(r(1).ge.dabs(r(2))*v)) 
    case (4)
      c1 = (minval(r).ge.0.D0)
      c2 = (c1.and.(r(1).ge.r(2))) 
    case (6)
      c1 = (minval(r).ge.0.D0)
      c2 = (c1.and.(r(1).ge.r(2)*v)) 
  end select
end if

res = c2

end function insideDihedralMFZ

!--------------------------------------------------------------------------
!
! SUBROUTINE: getVertex
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief get vertices of RFZ for quasicrystals (dihedral symmetries)
!
!> @param order name of the Euler angle file (with usual path handling)
!> @param vertex the number of components in the returned linked list
!
!> @date 06/18/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getVertex(order, vertex)
!DEC$ ATTRIBUTES DLLEXPORT :: getVertex

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: order
real(kind=dbl),INTENT(OUT)              :: vertex(2*order,2)

integer(kind=irg)                       :: ii
real(kind=dbl)                          :: th

do ii = 1,2*order
  th  = (dble(ii - 1)/dble(order) + 1.D0/2.D0/dble(order)) * cPi
  vertex(ii,1:2) = (/dcos(th), dsin(th)/)
end do

end subroutine getVertex
                                                                
!                                                                      
!     ..................................................................
!                                                                       
!        SUBROUTINE PNPOLY                                              
!                                                                       
!        PURPOSE                                                        
!           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON            
!                                                                       
!        USAGE                                                          
!           CALL PNPOLY (PX, PY, XX, YY, N, INOUT )                     
!                                                                       
!        DESCRIPTION OF THE PARAMETERS                                  
!           PX      - X-COORDINATE OF POINT IN QUESTION.                
!           PY      - Y-COORDINATE OF POINT IN QUESTION.                
!           XX      - N LONG VECTOR CONTAINING X-COORDINATES OF         
!                     VERTICES OF POLYGON.                              
!           YY      - N LONG VECTOR CONTAING Y-COORDINATES OF           
!                     VERTICES OF POLYGON.                              
!           N       - NUMBER OF VERTICES IN THE POLYGON.                
!           INOUT   - THE SIGNAL RETURNED:                              
!                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,        
!                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,     
!                      1 IF THE POINT IS INSIDE OF THE POLYGON.         
!                                                                       
!        REMARKS                                                        
!           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.      
!           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY           
!           OPTIONALLY BE INCREASED BY 1.                               
!           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING      
!           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX    
!           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING   
!           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.              
!           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.         
!           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM      
!           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.   
!                                                                       
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  
!           NONE                                                        
!                                                                       
!        METHOD                                                         
!           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT  
!           CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE        
!           POINT IS INSIDE OF THE POLYGON.                             
!                                                                       
!     ..................................................................
!                                                                       
      RECURSIVE FUNCTION PNPOLY(PX,PY,XX,YY,N) RESULT(INOUT)

      IMPLICIT NONE

      REAL(KIND=DBL) PX, PY
      INTEGER(KIND=IRG) N

      REAL(KIND=DBL) X(200),Y(200),XX(N),YY(N)                                    
      LOGICAL MX,MY,NX,NY                                         
      INTEGER O,INOUT,I,J,MAXDIM                                                         
!      OUTPUT UNIT FOR PRINTED MESSAGES                                 
      DATA O/6/                                                         
      MAXDIM=200                                                        
      IF(N.LE.MAXDIM)GO TO 6                                            
      WRITE(O,7)                                                        
7     FORMAT('0WARNING:',I5,' TOO GREAT FOR THIS VERSION OF PNPOLY. RESULTS INVALID')                                                 
      RETURN                                                            
6     DO 1 I=1,N                                                        
      X(I)=XX(I)-PX                                                     
1     Y(I)=YY(I)-PY                                                     
      INOUT=-1                                                          
      DO 2 I=1,N                                                        
      J=1+MOD(I,N)                                                      
      MX=X(I).GE.0.0                                                    
      NX=X(J).GE.0.0                                                    
      MY=Y(I).GE.0.0                                                    
      NY=Y(J).GE.0.0                                                    
      IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GO TO 2       
      IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GO TO 3  
      INOUT=-INOUT                                                      
      GO TO 2                                                           
3     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5                       
4     INOUT=0                                                           
      RETURN                                                            
5     INOUT=-INOUT                                                      
2     CONTINUE                                                          
      RETURN                                                            
      END FUNCTION PNPOLY


!--------------------------------------------------------------------------
!
! FUNCTION: MKCC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief auxiliary function for MacKenzie distributions
!
!> @note Uses the expressions from A. Morawiec, J.Appl.Cryst. (1995) 28:289-293
!
!> @param a
!> @param b
!> @param c
!
!> @date 02/22/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MKCC(a, b, c) result(CC)
!DEC$ ATTRIBUTES DLLEXPORT :: MKCC

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: a 
real(kind=dbl),INTENT(IN)       :: b 
real(kind=dbl),INTENT(IN)       :: c 
real(kind=dbl)                  :: CC 

CC =  acos( (cos(c)-cos(a)*cos(b)) / (sin(a)*sin(b)) )

end function MKCC

!--------------------------------------------------------------------------
!
! FUNCTION: MKS2
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief auxiliary function for MacKenzie distributions
!
!> @note Uses the expressions from A. Morawiec, J.Appl.Cryst. (1995) 28:289-293
!
!> @param a
!> @param b
!> @param c
!
!> @date 02/22/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MKS2(a, b, c) result(S2)
!DEC$ ATTRIBUTES DLLEXPORT :: MKS2

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: a 
real(kind=dbl),INTENT(IN)       :: b 
real(kind=dbl),INTENT(IN)       :: c 
real(kind=dbl)                  :: S2 

S2 = 2.D0 * ( cPi - MKCC(a,b,c) - cos(a) * MKCC(c,a,b) - cos(b) * MKCC(b,c,a) )

end function MKS2

!--------------------------------------------------------------------------
!
! SUBROUTINE: getMacKenzieDistribution
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief computes the theoretical MacKenzie distribution for a given rotational point group
!
!> @note Uses the expressions from A. Morawiec, J.Appl.Cryst. (1995) 28:289-293
!
!> @param pgnum
!> @param misor  array of misorientation angles for which the function values are requested
!> @param Nmisor number of misorientation values
!> @param MK MacKenzie distribution values
!
!> @date 02/22/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getMacKenzieDistribution(pgnum, Nmisor, misor, MK)
!DEC$ ATTRIBUTES DLLEXPORT :: getMacKenzieDistribution

use constants
use typedefs
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: pgnum
integer(kind=irg),INTENT(IN)        :: Nmisor
real(kind=dbl),INTENT(IN)           :: misor(0:Nmisor)
real(kind=dbl),INTENT(INOUT)        :: MK(0:Nmisor)
!f2py intent(in,out) ::  MK

real(kind=dbl),allocatable          :: pomega(:), chi(:), rr(:)
real(kind=dbl)                      :: h, h2, h3
integer(kind=irg)                   :: prot, pgrotOrder, i
real(kind=dbl),parameter            :: nn(4) = dble((/ 2, 4, 3, 6 /)), nnn(4) = dble((/ 4, 8, 6, 12 /))

! allocate array
allocate(pomega(Nmisor))

! first get the distribution pomega for the no-symmetry case 
prot = PGrot(pgnum)
pgrotOrder = PGTHDorder(prot)
pomega = (1.D0 / (2.D0 * cPi * cPi)) * sin(misor*0.5D0)**2

! if there is rotational symmetry, then compute the solid angle function chi
if (prot.gt.1) then 
  allocate(chi(Nmisor), rr(Nmisor))
  chi = 4.D0 * cPi
  rr = tan(misor*0.5D0)
  if ((prot.eq.3).or.(prot.eq.6)) h = nn(1)
  if ((prot.eq.9).or.(prot.eq.12)) h = nn(2)
  if ((prot.eq.16).or.(prot.eq.18)) h = nn(3)
  if ((prot.eq.21).or.(prot.eq.24)) h = nn(4)

  select case(prot) 
    case (3, 9, 16, 21)   ! cyclic point groups 
      chi = 4.D0 * cPi
      h2 = tan(cPi * 0.5D0 / h)
      do i = 0, Nmisor
        if (rr(i).ge.h2) then 
          chi(i) = chi(i) - 4.D0 * cPi * (1.D0 - cos( acos(h2/rr(i)) ) )
        end if
      end do
      MK = (cPi/180.D0) * h * pomega * chi

    case (6, 12, 18, 24)   ! dihedral groups 
      h2 = tan(cPi * 0.5D0 / h)
      h3 = 1.D0
      do i = 0, Nmisor
        if (rr(i).gt.h2) then 
          chi(i) = chi(i) - 4.D0 * cPi * (1.D0 - cos( acos(h2/rr(i)) ) )
        end if
        if (rr(i).gt.h3) then 
          chi(i) = chi(i) - 4.D0 * h * cPi * (1.D0 - cos( acos(h3/rr(i)) ) )
        end if
        if (rr(i).gt.sqrt(h3+h2*h2)) then 
          chi(i) = chi(i) + 4.D0 * h * MKS2( acos(h2/rr(i)), acos(h3/rr(i)), cPi/2.D0) &
                   + 2.D0 * h * MKS2( acos(h3/rr(i)), acos(h3/rr(i)), cPi/h)           
        end if
        if (rr(i).gt.sqrt(h3+2.D0*h2*h2)) then 
          chi(i) = 0.D0
        end if
      end do
      MK = (cPi/180.D0) * (2.D0 * h) * pomega * chi
 
     case (28)   ! tetrahedral group 
      h2 = 1.D0/sqrt(2.D0)
      h3 = 1.D0/sqrt(3.D0)
      do i = 0, Nmisor
        if (rr(i).gt.h3) then 
          chi(i) = chi(i) - 16.D0 * cPi * (1.D0 - cos( acos(h3/rr(i)) ) )
        end if
        if (rr(i).gt.h2) then 
          chi(i) = chi(i) + 12.D0 * MKS2( acos(h3/rr(i)),  acos(h3/rr(i)), acos(h3*h3) ) 
        end if
        if (rr(i).gt.1.D0) then 
          chi(i) = 0.D0
        end if
      end do
      MK = (cPi/180.D0) * 12.D0 * pomega * chi

     case (30)   ! octahedral group 
      h2 = sqrt(2.D0) - 1.D0
      h3 = 1.D0/sqrt(3.D0)
      do i = 0, Nmisor
        if (rr(i).gt.h2) then 
          chi(i) = chi(i) - 12.D0 * cPi * (1.D0 - cos( acos(h2/rr(i)) ) )
        end if
        if (rr(i).gt.h3) then 
          chi(i) = chi(i) - 16.D0 * cPi * (1.D0 - cos( acos(h3/rr(i)) ) )
        end if
        if (rr(i).gt.(2.D0-sqrt(2.D0))) then 
          chi(i) = chi(i) + 12.D0 * MKS2( acos(h2/rr(i)),  acos(h2/rr(i)), cPi*0.5D0 ) + &
                   24.D0 * MKS2( acos(h2/rr(i)),  acos(h3/rr(i)), acos(h3) ) 
        end if
        if (rr(i).gt.sqrt(23.0-16.D0*sqrt(2.D0))) then 
          chi(i) = 0.D0
        end if
      end do
      MK = (cPi/180.D0) * 24.D0 * pomega * chi

    case default
      call FatalError('getMacKenzieDistribution',' non-existent rotational point group number')
  end select
  deallocate(chi, rr)
! force the last point to zero  
  MK(Nmisor) = 0.D0
else 
  MK = (cPi/180.D0) * (4.D0 * cPi) * pomega
  deallocate(pomega)
end if

end subroutine getMacKenzieDistribution





end module so3
