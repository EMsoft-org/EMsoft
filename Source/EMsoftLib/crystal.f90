! ###################################################################
! Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:crystal.f90
!--------------------------------------------------------------------------
!
! MODULE: crystal
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Everything that has to do with crystallographic computations and input/output
!
!> @details  This includes distance and angle computations, coordinate transformations,
!> normalizations, dot and cross products, generation of asymmetric positions; also some
!> routines that deal with reading lattice parameters and atom coordinates and such.
! 
!> @date  1/5/99   MDG 1.0 original
!> @date    7/16/99 MDG 1.1 added error handling and TransCoor
!> @date    4/ 5/00 MDG 1.2 modified TransCoor to include mInvert
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 improved interface for single and double precision
!> @date   01/10/14 MDG 4.0 new version, suitable for multiphase calculations
!> @date   06/05/14 MDG 4.1 many modifications to remove/replace global variables, in particular "cell"
!> @date   06/23/14 MDG 4.2 added recursive statement to most functions
!--------------------------------------------------------------------------

module crystal

use local
use typedefs

public

interface TransSpace
  module procedure TransSpaceSingle
  module procedure TransSpaceDouble
end interface TransSpace  

interface CalcDot
  module procedure CalcDotSingle
  module procedure CalcDotDouble
end interface CalcDot

interface NormVec
  module procedure NormVecSingle
  module procedure NormVecDouble
end interface NormVec

interface CalcLength
  module procedure CalcLengthSingle
  module procedure CalcLengthDouble
end interface CalcLength

interface CalcAngle
  module procedure CalcAngleSingle
  module procedure CalcAngleDouble
end interface CalcAngle

interface CalcCross
  module procedure CalcCrossSingle
  module procedure CalcCrossDouble
end interface CalcCross

contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetEMsoftXtalSystem
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  convert a space group to the EMsoft xtal system numbering
! 
!> @date   07/30/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GetEMsoftXtalSystem(iSG) result(xs)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMsoftXtalSystem

IMPLICIT NONE

integer(kind=irg),INTENT(IN)       :: iSG
integer(kind=irg)                  :: xs

integer(kind=irg)                  :: i, j 
integer(kind=irg),parameter        :: icv(7) = (/ 7, 6, 3, 2, 5, 4, 1 /)

! first get the crystal system number in the international numbering scheme
j = 0
do i=1,7
  if (SGXsym(i).le.iSG) j = j+1
end do

! 1. Cubic
! 2. Tetragonal
! 3. Orthorhombic
! 4. Hexagonal
! 5. Trigonal
! 6. Monoclinic
! 7. Triclinic

xs = icv(j)

end function GetEMsoftXtalSystem

!--------------------------------------------------------------------------
!
! SUBROUTINE: ResetCell
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  reset all unit cell and symmetry variables to zero
! 
!> @date    1/ 5/99 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update after new cell type
!> @date   06/06/14 MDG 4.1 further update of various fields in cell pointer
!> @date   06/06/14 MDG 4.2 moved routine to crystal module
!--------------------------------------------------------------------------
recursive subroutine ResetCell(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: ResetCell

IMPLICIT NONE

type(unitcell)          :: cell

! initialize cell 
 cell%a = 0.0_dbl
 cell%b = 0.0_dbl
 cell%c = 0.0_dbl
 cell%alpha = 0.0_dbl
 cell%beta  = 0.0_dbl
 cell%gamma = 0.0_dbl
 cell%vol   = 0.0_dbl
 cell%dmt = 0.0_dbl
 cell%rmt = 0.0_dbl
 cell%dsm = 0.0_dbl
 cell%rsm = 0.0_dbl
 cell%trigmat = 0.0_dbl
 cell%ATOM_type = 0_irg
 cell%ATOM_ntype = 0_irg
 cell%SYM_SGnum = 0_irg
 cell%xtal_system = 0_irg
 cell%SYM_SGset = 0_irg
 cell%ATOM_pos = 0.0_dbl
 cell%fname = ''

! initialize all symmetry variables
 cell%SG%SYM_GENnum = 0_irg
 cell%SG%SYM_MATnum = 0_irg
 cell%SG%SYM_NUMpt  = 0_irg
 cell%SG%SYM_reduce = .FALSE.
 cell%SG%SYM_trigonal = .FALSE.
 cell%SG%SYM_second = .FALSE.
 cell%SG%SYM_centrosym = .FALSE. 
 cell%SG%SYM_c = 0.0_dbl
 cell%SG%SYM_data = 0.0_dbl
 cell%SG%SYM_direc = 0.0_dbl
 cell%SG%SYM_recip = 0.0_dbl
 cell%SG%SYM_name = ''

! and deallocate any arrays
 if (allocated(cell%LUT)) deallocate(cell%LUT)
 if (allocated(cell%dbdiff)) deallocate(cell%dbdiff)
 if (allocated(cell%apos)) deallocate(cell%apos)
 
! deallocate any linked lists 
 if (associated(cell%reflist)) nullify(cell%reflist)

end subroutine ResetCell

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcMatrices
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief computes important crystallographic matrices
!
!> @details  Computes the direct and reciprocal metric tensors and the direct
!>  and reciprocal structure matrices 
! 
!> @param cell unit cell pointer
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   01/10/14 MDG 3.0 removed Kronecker delta matrix
!--------------------------------------------------------------------------
recursive subroutine CalcMatrices(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcMatrices

use error
use constants

IMPLICIT NONE

type(unitcell)          :: cell

!> auxiliary variables for geometric computation
real(kind=dbl)          :: det,ca,cb,cg,sa,sb,sg,tg,pirad
real(kind=dbl)          :: Mx(3,3), My(3,3), x, y

! auxiliary variables for the various tensors
 pirad = cPi/180.0_dbl
 ca = dcos(pirad*cell%alpha)
 cb = dcos(pirad*cell%beta)
 cg = dcos(pirad*cell%gamma)
 sa = dsin(pirad*cell%alpha)
 sb = dsin(pirad*cell%beta)
 sg = dsin(pirad*cell%gamma)
 tg = dtan(pirad*cell%gamma)

 if (sg.eq.0.0_dbl) call FatalError('CalcMatrices',' Invalid gamma angle')

! [removed on 1/10/14; was not used anywhere in the package]
! define the Kronecker Delta
! cell%krdel = reshape( (/ 1.0_dbl,0.0_dbl,0.0_dbl,0.0_dbl,1.0_dbl,0.0_dbl,0.0_dbl,0.0_dbl,1.0_dbl /), (/3,3/) )

! compute the direct metric tensor [equation 1.5, page 6]
 cell%dmt(1,1) = cell%a**2
 cell%dmt(2,2) = cell%b**2
 cell%dmt(3,3) = cell%c**2
 cell%dmt(1,2) = cell%a*cell%b*cg
 cell%dmt(2,1) = cell%dmt(1,2)
 cell%dmt(1,3) = cell%a*cell%c*cb
 cell%dmt(3,1) = cell%dmt(1,3)
 cell%dmt(2,3) = cell%b*cell%c*ca
 cell%dmt(3,2) = cell%dmt(2,3)
! cell volume via the determinant of dmt 
 det = (cell%a*cell%b*cell%c)**2*(1.D0-ca**2-cb**2-cg**2+2.D0*ca*cb*cg)
 cell%vol = dsqrt(det)

 if (cell%vol.lt.1D-6) call FatalError('CalcMatrices',' Unit cell volume is zero or suspiciously small')

! compute the reciprocal metric tensor as the inverse of the direct
! metric tensor
 cell%rmt(1,1) = (cell%b*cell%c*sa)**2
 cell%rmt(2,2) = (cell%a*cell%c*sb)**2
 cell%rmt(3,3) = (cell%a*cell%b*sg)**2
 cell%rmt(1,2) = cell%a*cell%b*cell%c**2*(ca*cb-cg)
 cell%rmt(2,1) = cell%rmt(1,2)
 cell%rmt(1,3) = cell%a*cell%b**2*cell%c*(cg*ca-cb)
 cell%rmt(3,1) = cell%rmt(1,3)
 cell%rmt(2,3) = cell%a**2*cell%b*cell%c*(cb*cg-ca)
 cell%rmt(3,2) = cell%rmt(2,3)
 cell%rmt = cell%rmt/det

! compute the direct structure matrix [equation 1.64, page 57]
 cell%dsm(1,1) = cell%a
 cell%dsm(1,2) = cell%b*cg
 cell%dsm(1,3) = cell%c*cb
 cell%dsm(2,1) = 0.0_dbl
 cell%dsm(2,2) = cell%b*sg
 cell%dsm(2,3) = -cell%c*(cb*cg-ca)/sg
 cell%dsm(3,1) = 0.0_dbl
 cell%dsm(3,2) = 0.0_dbl
 cell%dsm(3,3) = cell%vol/(cell%a*cell%b*sg)

! compute the reciprocal structure matrix [equation 1.65, page 58]
 cell%rsm(1,1) = 1.0_dbl/cell%a
 cell%rsm(1,2) = 0.0_dbl
 cell%rsm(1,3) = 0.0_dbl
 cell%rsm(2,1) = -1.0_dbl/(cell%a*tg)
 cell%rsm(2,2) = 1.0_dbl/(cell%b*sg)
 cell%rsm(2,3) = 0.0_dbl
 cell%rsm(3,1) = cell%b*cell%c*(cg*ca-cb)/(cell%vol*sg)
 cell%rsm(3,2) = cell%a*cell%c*(cb*cg-ca)/(cell%vol*sg)
 cell%rsm(3,3) = (cell%a*cell%b*sg)/cell%vol

! finally, if we have the trigonal/rhombohedral case, we need a second direct structure matrix
! that can transform the three-fold [111] axis to the z-axis of a cartesian reference frame,
! while keeping the rhombohedral [100] direction in the x-z plane.  This is used for the k-vector
! sampling routines for master pattern calculations.
!
! added by MDG on 08/30/15; computations in Mathematica notebook rhombohedral.nb in manuals folder, validated.
if (cell%xtal_system.eq.5) then 
! Mx matrix
  x = 0.5D0/dcos(pirad*0.5D0*cell%alpha)
  y = dsqrt(1.D0-x*x)
  Mx = transpose(reshape( (/ 1.D0, 0.D0, 0.D0,  0.D0, x, -y,  0.D0, y, x /), (/ 3,3 /) ))
! My matrix
  x = 2.0D0*dsin(pirad*0.5D0*cell%alpha)/dsqrt(3.D0)
  y = dsqrt(1.D0-x*x)
  My = transpose(reshape( (/ x, 0.D0, -y,  0.D0, 1.0D0, 0.D0,  y, 0.D0, x /), (/ 3,3 /) ))
  cell%trigmat = matmul(matmul(My,Mx),cell%dsm)
end if

end subroutine CalcMatrices

!--------------------------------------------------------------------------
!
! SUBROUTINE: TransSpaceDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert vector components from one inspace to outspace (double precision)
!
!> @details  Converts vector components from one space to another, including
!> direct space, reciprocal space, and the standard cartesian reference frame.
!
!> @param cell unit cell pointer
!> @param t input vector 
!> @param d output vector 
!> @param inspace input space character ('d', 'r', 'c')
!> @param outspace output space character ('d', 'r', 'c') 
! 
!> @date    10/13/98 MDG 1.0 original
!> @date     5/19/01 MDG 2.0 f90 version
!> @date    11/27/01 MDG 2.1 added kind support
!> @date    03/19/13 MDG 3.0 changed using interface protocol
!> @date    01/10/14 MDG 4.0 checked for changes to unitcell type
!--------------------------------------------------------------------------
recursive subroutine TransSpaceDouble(cell,t,d,inspace,outspace)
!DEC$ ATTRIBUTES DLLEXPORT :: TransSpaceDouble

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=dbl),INTENT(IN)       :: t(3)                 !< input vector in inspace reference frame
real(kind=dbl),INTENT(OUT)      :: d(3)                 !< output vector in outspace reference frame 
character(1),INTENT(IN)         :: inspace              !< characters to label input space (d, r, or c)
character(1),INTENT(IN)         :: outspace             !< characters to label output space (d, r, or c)

! intercept the case where inspace and outspace are the same 
if (inspace.eq.outspace) then
  d = t
  return
end if

 if (inspace.eq.'d') then
! direct to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%dsm,t)
   return
  end if
! direct to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dmt)
   return
  end if
 end if

 if (inspace.eq.'r') then
! reciprocal to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%rsm,t)
   return
  end if
! reciprocal to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(t,cell%rmt)
   return
  end if
 end if

 if (inspace.eq.'c') then
! Cartesian to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(cell%rsm,t)
   return
  end if
! Cartesian to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dsm)
   return
  end if
 end if
 
end subroutine TransSpaceDouble

!--------------------------------------------------------------------------
!
! SUBROUTINE: TransSpaceSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert vector components from one inspace to outspace (single precision)
!
!
!> @details  Converts vector components from one space to another, including
!> direct space, reciprocal space, and the standard cartesian reference frame.
!
!> @param cell unit cell pointer
!> @param t input vector 
!> @param d output vector 
!> @param inspace input space character ('d', 'r', 'c')
!> @param outspace output space character ('d', 'r', 'c') 
! 
!> @date    10/13/98 MDG 1.0 original
!> @date     5/19/01 MDG 2.0 f90 version
!> @date    11/27/01 MDG 2.1 added kind support
!> @date    03/19/13 MDG 3.0 changed using interface protocol
!> @date    01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date    06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive subroutine TransSpaceSingle(cell, t, d, inspace, outspace)
!DEC$ ATTRIBUTES DLLEXPORT :: TransSpaceSingle

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: t(3)                 !< input vector in inspace reference frame
real(kind=sgl),INTENT(OUT)      :: d(3)                 !< output vector in outspace reference frame 
character(1),INTENT(IN)         :: inspace              !< characters to label input space (d, r, or c)
character(1),INTENT(IN)         :: outspace             !< characters to label output space (d, r, or c)

! intercept the case where inspace and outspace are the same 
if (inspace.eq.outspace) then
  d = t
  return
end if

 if (inspace.eq.'d') then
! direct to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%dsm,t)
   return
  end if
! direct to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dmt)
   return
  end if
 end if

 if (inspace.eq.'r') then
! reciprocal to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%rsm,t)
   return
  end if
! reciprocal to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(t,cell%rmt)
   return
  end if
 end if

 if (inspace.eq.'c') then
! Cartesian to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(cell%rsm,t)
   return
  end if
! Cartesian to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dsm)
   return
  end if
 end if
 
end subroutine  TransSpaceSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE: TransCoor
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert vector components from one frame to another
!
!> @details  convert vector components from one reference frame 
!> to another; this is a general coordinate transformation using the 
!> old-to-new matrix alpha.  The details of this routine are summarized in 
!> Table 1.6, page 51, of the textbook. The direction of the 
!> transformation is 'on' (old-to-new) or 'no' (new-to-old).
!
!> @param cell unit cell pointer
!> @param t input vector 
!> @param d output vector 
!> @param talpha transformation matrix
!> @param space input space character ('d', 'r', 'c')
!> @param direction transformation direction string ('on', 'no') 
! 
!> @todo This whole routine will need to be re-designed so that a more
!> general type of transformation can be carried out, including Euler angle
!> rotations and quaternion rotations.
!
!> @date    7/16/99 MDG 1.0 original
!> @date    4/ 5/00 MDG 1.1 added support for new mInvert
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 slight modification
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive subroutine TransCoor(cell, t, d, talpha, space, direction)
!DEC$ ATTRIBUTES DLLEXPORT :: TransCoor

use math, ONLY: mInvert 

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=dbl),INTENT(IN)       :: t(3)                 !< input vector w.r.t. input space reference frame
real(kind=dbl),INTENT(OUT)      :: d(3)                 !< transformed vector components
real(kind=dbl),INTENT(IN)       :: talpha(3,3)          !< transformation matrix
real(kind=dbl)                  :: alinv(3,3)           !< inverse of transformation matrix
character(1),INTENT(IN)         :: space                !< space in which to perform transformation ('d', 'r', 'c')
character(2),INTENT(IN)         :: direction            !< transformation direction (no=new-to-old, on=old-to-new)

logical                         :: uni                  !< logical to indicate unitary matrix (or not)

! these matrices are typically unitary, so inverse is simply the transpose
 uni = .TRUE.
 if (space.eq.'d') then 
  if (direction.eq.'on') then 
   call mInvert(talpha,alinv,uni)
   d = matmul(t,alinv)
  else
   d = matmul(t,talpha)
  end if
 else
  if (direction.eq.'on') then 
   d = matmul(talpha,t)
  else
   call mInvert(talpha,alinv,uni)
   d = matmul(alinv,t)
  end if
 end if

end subroutine TransCoor

!--------------------------------------------------------------------------
!
! FUNCTION: CalcDotSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief single precision dot product in arbitrary space
!
!> @details  computes the dot product between two vectors in
!> real, reciprocal, or Cartesian space; implements
!> equations 1.6 (page 7), and 1.16 (page 15).
!
!> @param cell unit cell pointer
!> @param p input vector 
!> @param q input vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive function CalcDotSingle(cell, p,q,space) result(cdot)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcDotSingle

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: p(3)         !< first input vector in space reference frame
real(kind=sgl),INTENT(IN)       :: q(3)         !< second input vector
character(1),INTENT(IN)         :: space        !< space in which to compute product ('d', 'r', or 'c')
real(kind=sgl)                  :: cdot         !< dot product p.q

 cdot = 0.0_sgl
 if (space.eq.'d') cdot = dot_product(p,matmul(cell%dmt,q))
 if (space.eq.'r') cdot = dot_product(p,matmul(cell%rmt,q))
 if (space.eq.'c') cdot = dot_product(p,q)

end function CalcDotSingle

!--------------------------------------------------------------------------
!
! FUNCTION: CalcDotDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief double precision dot product in arbitrary space
!
!> @details  computes the dot product between two vectors in
!> real, reciprocal, or Cartesian space; implements
!> equations 1.6 (page 7), and 1.16 (page 15).
!
!> @param cell unit cell pointer
!> @param p input vector 
!> @param q input vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive function CalcDotDouble(cell, p, q, space) result(cdot)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcDotDouble

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=dbl),INTENT(IN)       :: p(3)         !< first input vector in space reference frame
real(kind=dbl),INTENT(IN)       :: q(3)         !< second input vector
character(1),INTENT(IN)         :: space        !< space in which to compute product ('d', 'r', or 'c')
real(kind=dbl)                  :: cdot         !< dot product p.q

 cdot = 0.0_dbl
 if (space.eq.'d') cdot = dot_product(p,matmul(cell%dmt,q))
 if (space.eq.'r') cdot = dot_product(p,matmul(cell%rmt,q))
 if (space.eq.'c') cdot = dot_product(p,q)

end function CalcDotDouble

!--------------------------------------------------------------------------
!
! SUBROUTINE: NormVecSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief single precision vector normalization
!
!> @details  vector normalization in arbitrary space.  Note that it is not
!> necessarily so that the Cartesian length of a vector [sqrt(x^2+y^2+z^2)]
!> becomes unity after this normalization.  That is only the case for 
!> the cartesian metric; for all other metrics, the length of a normalized
!> vector is in general not equal to 1.0 !!!
!
!> @param cell unit cell pointer
!> @param p input.output vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date 10/20/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive subroutine NormVecSingle(cell, p, space)
!DEC$ ATTRIBUTES DLLEXPORT :: NormVecSingle

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=sgl),INTENT(INOUT)            :: p(3)         !< input/output vector components
!f2py intent(in,out) ::  p
character(1),INTENT(IN)                 :: space        !< space character ('d', 'r', or 'c')
real(kind=sgl)                          :: x            !< auxiliary variable

 x=CalcLength(cell, p, space)
 if (x.ne.0.0) then 
   p=p/x
 else
   p=(/0.0,0.0,0.0/)
 end if  

end subroutine NormVecSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE: NormVecDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief double precision vector normalization
!
!> @details  vector normalization in arbitrary space; see also comment in 
!> single precision version.
!
!> @param cell unit cell pointer
!> @param p input/output vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date 10/20/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive subroutine NormVecDouble(cell, p, space)
!DEC$ ATTRIBUTES DLLEXPORT :: NormVecDouble

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=dbl),INTENT(INOUT)            :: p(3)         !< input/output vector components
!f2py intent(in,out) ::  p
character(1),INTENT(IN)                 :: space        !< space character ('d', 'r', or 'c')
real(kind=dbl)                          :: x            !< auxiliary variable

 x=CalcLength(cell,p,space)
 if (x.ne.0.D0) then 
   p=p/x
 else
   p=(/0.D0,0.D0,0.D0/)
 end if  

end subroutine NormVecDouble

!--------------------------------------------------------------------------
!
! FUNCTION: CalcLengthSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief single precision vector length
!
!> @details  compute the length of a vector in real, reciprocal
!> or Cartesian space
!
!> @param cell unit cell pointer
!> @param p input/output vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive function CalcLengthSingle(cell, p, space) result(x)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcLengthSingle

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=sgl),INTENT(IN)               :: p(3)         !< input/output vector components
character(1),INTENT(IN)                 :: space        !< space character ('d', 'r', or 'c')
real(kind=sgl)                          :: x            !< auxiliary variable


 x = sqrt(CalcDot(cell, p, p, space))

end function CalcLengthSingle

!--------------------------------------------------------------------------
!
! FUNCTION: CalcLengthDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief double precision vector length
!
!> @details  compute the length of a vector in real, reciprocal
!> or Cartesian space
!
!> @param cell unit cell pointer
!> @param p input/output vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive function CalcLengthDouble(cell, p, space) result(x)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcLengthDouble

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=dbl),INTENT(IN)               :: p(3)         !< input/output vector components
character(1),INTENT(IN)                 :: space        !< space character ('d', 'r', or 'c')
real(kind=dbl)                          :: x            !< auxiliary variable

 x = dsqrt(CalcDot(cell, p, p, space))

end function CalcLengthDouble

!--------------------------------------------------------------------------
!
! FUNCTION: CalcAngleSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief single precision angle in arbitrary space
!
!> @details  compute the angle between vectors in real, reciprocal
!> or Cartesian space
!
!> @param cell unit cell pointer
!> @param p input vector 
!> @param q input vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date 10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive function CalcAngleSingle(cell, p, q, space) result(a)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcAngleSingle

use error
use constants

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=sgl),INTENT(IN)               :: p(3)         !< first vector components
real(kind=sgl),INTENT(IN)               :: q(3)         !< second vector components
character(1),INTENT(IN)                 :: space        !< space of the computation ('d', 'r', 'c')
real(kind=sgl)                          :: a            !< angle in radians
real(kind=sgl)                          :: x, y, z, t   !< auxiliary variables

 x = CalcDot(cell,p,q,space)
 y = CalcLength(cell,p,space)
 z = CalcLength(cell,q,space)

 if ((y.eq.0.0_sgl).or.(z.eq.0.0_sgl)) then
  call FatalError('CalcAngleSingle',' vector of zero length specified')
 end if

 t = x/(y*z)
 if (t.ge.1.0_sgl) then 
  a = 0.0_sgl
 else 
  if (t.le.-1.0_sgl) then 
   a = sngl(cPi)
  else 
   a = acos(t)
  end if
 end if

end function CalcAngleSingle

!--------------------------------------------------------------------------
!
! FUNCTION: CalcAngleDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief double precision angle in arbitrary space
!
!> @details  compute the angle between vectors in real, reciprocal
!> or Cartesian space
!
!> @param cell unit cell pointer
!> @param p input vector 
!> @param q input vector 
!> @param space input space character ('d', 'r', 'c')
!
!> @date 10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive function CalcAngleDouble(cell,p,q,space) result(a)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcAngleDouble

use error
use constants

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=dbl),INTENT(IN)               :: p(3)         !< first vector components
real(kind=dbl),INTENT(IN)               :: q(3)         !< second vector components
character(1),INTENT(IN)                 :: space        !< space of the computation ('d', 'r', 'c')
real(kind=dbl)                          :: a            !< angle in radians
real(kind=dbl)                          :: x, y, z, t   !< auxiliary variables


 x = CalcDot(cell,p,q,space)
 y = CalcLength(cell,p,space)
 z = CalcLength(cell,q,space)

 if ((y.eq.0.0_dbl).or.(z.eq.0.0_dbl)) then
  call FatalError('CalcAngleDouble',' vector of zero length specified')
 end if

 t = x/(y*z)
 if (t.ge.1.0_dbl) then 
  a = 0.0_dbl
 else 
  if (t.le.-1.0_dbl) then 
   a = cPi
  else 
   a = dacos(t)
  end if
 end if
 
end function CalcAngleDouble

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcCrossSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief vector cross product in arbitrary space
!
!> @details  computes the cross product between two vectors and
!> expresses it in either real space or reciprocal space.
!> The output can also be expressed in the standard
!> Cartesian reference frame.  The switch iv indicates
!> whether the result should be scaled by the unit cell
!> volume. More information in section 1.3.5, page 18.
!
!> @param cell unit cell pointer
!> @param p input vector 
!> @param q input vector 
!> @param r output vector 
!> @param inspace input space character ('d', 'r', 'c')
!> @param outspace output space character ('d', 'r', 'c')
!> @param iv logical switch for volume division (if TRUE)
!
!> @todo replace iv by a logical switch; replace if-statements by case statement
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 cell pointer argument
!--------------------------------------------------------------------------
recursive subroutine CalcCrossSingle(cell,p,q,r,inspace,outspace,iv)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcCrossSingle

use math

IMPLICIT NONE 

type(unitcell)                          :: cell
real(kind=sgl),INTENT(IN)               :: p(3)         !< first input vector (order is important here !)
real(kind=sgl),INTENT(IN)               :: q(3)         !< second input vector
real(kind=sgl),INTENT(OUT)              :: r(3)         !< output vector
character(1),INTENT(IN)                 :: inspace      !< inspace character ('d','r','c')
character(1),INTENT(IN)                 :: outspace     !< outspace character
integer(kind=irg),INTENT(IN)            :: iv           !< volume division switch
real(kind=sgl)                          :: x(3), vl     !< auxiliary variables

! divide by volume?
 if (iv.eq.1) then 
  vl = sngl(cell%vol)
 else
  vl = 1.0_sgl
 endif

! in direct space 
 if (inspace.eq.'d') then               ! so the output is in reciprocal space !!!
  r(1) = vl*(p(2)*q(3)-p(3)*q(2))
  r(2) = vl*(p(3)*q(1)-p(1)*q(3))
  r(3) = vl*(p(1)*q(2)-p(2)*q(1))
  if (outspace.eq.'d') then             ! output in direct space
   x = matmul(r,cell%rmt)
   r = x
  end if
  if (outspace.eq.'c') then             ! output in cartesian frame
   x = matmul(cell%rsm,r)
   r = x
  end if
 end if

! in reciprocal space 
 if (inspace.eq.'r') then               ! so the output is in direct space !!!
  r(1) = (p(2)*q(3)-p(3)*q(2))/vl
  r(2) = (p(3)*q(1)-p(1)*q(3))/vl
  r(3) = (p(1)*q(2)-p(2)*q(1))/vl
  if (outspace.eq.'r') then             ! output in reciprocal space
   x = matmul(r,cell%dmt)
   r = x
  end if
  if (outspace.eq.'c') then             ! output in cartesian frame
   x = matmul(cell%dsm,r)
   r = x
  end if
 end if

! in  cartesian
 if (inspace.eq.'c') then               ! so no conversion needed.
  r(1) = p(2)*q(3)-p(3)*q(2)
  r(2) = p(3)*q(1)-p(1)*q(3)
  r(3) = p(1)*q(2)-p(2)*q(1)
 end if
 
end subroutine CalcCrossSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcCrossDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief vector cross product in arbitrary space (double precision)
!
!> @details  computes the cross product between two vectors and
!> expresses it in either real space or reciprocal space.
!> The output can also be expressed in the standard
!> Cartesian reference frame.  The switch iv indicates
!> whether the result should be scaled by the unit cell
!> volume. More information in section 1.3.5, page 18.
!
!> @param cell unit cell pointer
!> @param p input vector 
!> @param q input vector 
!> @param r output vector 
!> @param inspace input space character ('d', 'r', 'c')
!> @param outspace output space character ('d', 'r', 'c')
!> @param iv logical switch for colume division (if TRUE)
!
!> @todo replace iv by a logical switch; replace if-statements by case statement
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!--------------------------------------------------------------------------
recursive subroutine CalcCrossDouble(cell,p,q,r,inspace,outspace,iv)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcCrossDouble

use math

IMPLICIT NONE 

type(unitcell)                          :: cell
real(kind=dbl),INTENT(IN)               :: p(3)         !< first input vector (order is important here !)
real(kind=dbl),INTENT(IN)               :: q(3)         !< second input vector
real(kind=dbl),INTENT(OUT)              :: r(3)         !< output vector
character(1),INTENT(IN)                 :: inspace      !< inspace character ('d','r','c')
character(1),INTENT(IN)                 :: outspace     !< outspace character
integer(kind=irg),INTENT(IN)            :: iv           !< volume division switch
real(kind=dbl)                          :: x(3), vl     !< auxiliary variables


 if (iv.eq.1) then 
  vl = cell%vol
 else
  vl = 1.0_dbl
 endif

! in direct space 
 if (inspace.eq.'d') then               ! so the output is in reciprocal space !!!
  r(1) = vl*(p(2)*q(3)-p(3)*q(2))
  r(2) = vl*(p(3)*q(1)-p(1)*q(3))
  r(3) = vl*(p(1)*q(2)-p(2)*q(1))
  if (outspace.eq.'d') then             ! output in direct space
   x = matmul(r,cell%rmt)
   r = x
  end if
  if (outspace.eq.'c') then             ! output in cartesian frame
   x = matmul(cell%rsm,r)
   r = x
  end if
 end if

! in reciprocal space 
 if (inspace.eq.'r') then               ! so the output is in direct space !!!
  r(1) = (p(2)*q(3)-p(3)*q(2))/vl
  r(2) = (p(3)*q(1)-p(1)*q(3))/vl
  r(3) = (p(1)*q(2)-p(2)*q(1))/vl
  if (outspace.eq.'r') then             ! output in reciprocal space
   x = matmul(r,cell%dmt)
   r = x
  end if
  if (outspace.eq.'c') then             ! output in cartesian frame
   x = matmul(cell%dsm,r)
   r = x
  end if
 end if

! in  cartesian
 if (inspace.eq.'c') then               ! so no conversion needed.
  r(1) = p(2)*q(3)-p(3)*q(2)
  r(2) = p(3)*q(1)-p(1)*q(3)
  r(3) = p(1)*q(2)-p(2)*q(1)
 end if
  
end subroutine CalcCrossDouble

!--------------------------------------------------------------------------
!
! SUBROUTINE: MilBrav
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief conversion 3->4 or 4->3 index notation
!
!> @details  conversion from Miller to Miller-Bravais indices for
!> directions.  The switch d is either '34' or '43'.
!> implements equations 1.31 and 1.32, pages 24-25.
!
!> @param p input vector 
!> @param q output vector 
!> @param d direction string ('34' or '43')
!!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!--------------------------------------------------------------------------
recursive subroutine MilBrav(p,q,d)
!DEC$ ATTRIBUTES DLLEXPORT :: MilBrav

IMPLICIT NONE

integer(kind=irg),INTENT(INOUT)         :: p(3)         !< input/output vector
!f2py intent(in,out) ::  p
integer(kind=irg),INTENT(INOUT)         :: q(4)         !< input/output vector
!f2py intent(in,out) ::  q
character(2),INTENT(IN)                 :: d            !< direction string ('34' or '43')
integer(kind=irg)                       :: i, j         !< auxiliary variables
real(kind=sgl)                          :: r(4), rm, tmp(4)     !< auxiliary variables  

 if (d.eq.'43') then 
! equation 1.31
! these will always be integers, so no reduction is required
  p(1) = q(1)-q(3)
  p(2) = q(2)-q(3)
  p(3) = q(4)
 else
! equation 1.32
! there is no need to divide by 3, since that would be taken out 
! by the reduction to integers in the next step
  r(1) = float(2*p(1)-p(2))
  r(2) = float(2*p(2)-p(1))
  r(3) = -float(p(1)+p(2))
  r(4) = float(3*p(3))

! next reduce to common integers
! first, find the non-zero minimum index
  rm = 100.0
  do i=1,4 
   if ((abs(r(i)).lt.rm).and.(r(i).ne.0.0)) rm = abs(r(i))
  end do

! then check if this index is a common divider of the others
  j = 0
  do i=1,4
   tmp(i) = r(i)/rm
   if ( ( abs(tmp(i))-int(abs(tmp(i))) ).eq.0.0) j=j+1
  end do
  if (j.eq.4) then
    q = tmp
  else  
    q = r
  end if
 end if

end subroutine MilBrav

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetLatParm
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief input of lattice parameters
!
!> @details  Input of crystal system followed by the appropriate set of lattice
!> parameters; all are stored in the cell type.
!>
!> For the trigonal and hexagonal crystal systems, the following switch settings are to be used:
!>              xtal_system   hexset    SYM_trigonal   SYM_second
!> hexagonal          4         T            F              F
!> trig/hex           5         T            T              F
!> trig/rhomb         5         F            T              T
!
!> @param cell unit cell pointer
!!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell 
!> @date   06/05/14 MDG 4.1 modified after elimination of global variables
!> @date   08/30/15 MDG 4.2 validated trigonal setting
!> @date   04/20/18 MDG 5.0 prepare code for 2D and 3D quasicrystals
!--------------------------------------------------------------------------
recursive subroutine GetLatParm(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetLatParm

use io

IMPLICIT NONE

type(unitcell),INTENT(INOUT)            :: cell
!f2py intent(in,out) ::  cell

integer(kind=irg)                       :: io_int(1)    !< integer input array
real(kind=dbl)                          :: io_real(1)   !< double precision real input array
integer(kind=irg)                       :: std

! this routine assumes that the cell pointer has been associated elsewhere

 call Message(' Select the crystal system : ', frm = "(A)")
 call Message('  1. Cubic ', frm = "(A)")
 call Message('  2. Tetragonal ', frm = "(A)")
 call Message('  3. Orthorhombic ', frm = "(A)")
 call Message('  4. Hexagonal ', frm = "(A)")
 call Message('  5. Trigonal ', frm = "(A)")
 call Message('  6. Monoclinic ', frm = "(A)")
 call Message('  7. Triclinic ', frm = "(A/)")
! call Message('  8. 2-D Quasi-Crystal', frm = "(A/)")
! call Message('  9. 3-D Quasi-Crystal', frm = "(A/)")

 call Message(' Note about the trigonal system:', frm = "(A)")
 call Message(' -------------------------------', frm = "(A)")
 call Message(' Primitive trigonal crystals are defined with respect to a HEXAGONAL', frm = "(A)")
 call Message(' reference frame.  Rhombohedral crystals can be referenced with', frm = "(A)")
 call Message(' respect to a HEXAGONAL basis (first setting), or with respect to', frm = "(A)")
 call Message(' a RHOMBOHEDRAL basis (second setting).  The default setting for ', frm = "(A)")
 call Message(' trigonal symmetry is the hexagonal setting.  When you select', frm = "(A)")
 call Message(' crystal system 5 above, you will be prompted for the setting. ', frm = "(A//)")
 call ReadValue(' crystal system ---> ', io_int, 1)
 cell%xtal_system = io_int(1)
 
!if (io_int(1).gt.7) then

!else

  ! make sure the symmetry operations will be reduced to the 
  ! fundamental unit cell
   cell%SG%SYM_reduce=.TRUE.
   cell%hexset=.FALSE.

  ! deal with the rhombohedral vs. hexagonal setting in the trigonal crystal system
  ! (the rhombohedral axes are considered as the second setting)
   cell%SG%SYM_trigonal=.FALSE.
   cell%SG%SYM_second=.FALSE.
   if (cell%xtal_system.eq.5) then
    cell%SG%SYM_trigonal=.TRUE.
    call Message('Enter 1 for rhombohedral setting ,', frm = "(/A)")
    call ReadValue('0 for hexagonal setting : ', io_int, 1)
    if (io_int(1).eq.0) then
     cell%xtal_system=4   ! this is set to 4 so that we ask for the correct lattice parameters below
    else
     cell%SG%SYM_second=.TRUE.
    end if
   end if

  ! get the lattice parameters
   call Message('Enter lattice parameters', frm = "(//A)")

  ! put default values based on cubic symmetry, then change them later
   call ReadValue('    a [nm] = ', io_real, 1)
   cell%a = io_real(1)
   cell%b = cell%a 
   cell%c = cell%a 
   cell%alpha = 90.0_dbl
   cell%beta = 90.0_dbl
   cell%gamma = 90.0_dbl

  ! now get the proper lattice parameters
   select case (cell%xtal_system)
    case (1)
  ! tetragonal
    case (2)
     call ReadValue('    c [nm] = ', io_real, 1)
     cell%c = io_real(1)
  ! orthorhombic
    case (3)
     call ReadValue('    b [nm] = ', io_real, 1)
     cell%b = io_real(1)
     call ReadValue('    c [nm] = ', io_real, 1)
     cell%c = io_real(1)
  ! hexagonal
    case (4)
     call ReadValue('    c [nm] = ', io_real, 1)
     cell%c = io_real(1)
     cell%gamma=120.0_dbl
  ! rhombohedral 
    case (5)
     call ReadValue('    alpha [deg] = ', io_real, 1)
     cell%alpha = io_real(1)
     cell%beta = cell%alpha
     cell%gamma = cell%alpha
  ! monoclinic   
    case (6)
     call ReadValue('    b [nm] = ', io_real, 1)
     cell%b = io_real(1)
     call ReadValue('    c [nm] = ', io_real, 1)
     cell%c = io_real(1)
     call ReadValue('    beta  [deg] = ', io_real, 1)
     cell%beta = io_real(1)
  ! triclinic    
    case (7) 
     call ReadValue('    b [nm] = ', io_real, 1)
     cell%b = io_real(1)
     call ReadValue('    c [nm] = ', io_real, 1)
     cell%c = io_real(1)
     call ReadValue('    alpha [deg] = ', io_real, 1)
     cell%alpha = io_real(1)
     call ReadValue('    beta  [deg] = ', io_real, 1)
     cell%beta = io_real(1)
     call ReadValue('    gamma [deg] = ', io_real, 1)
     cell%gamma = io_real(1)
   end select

  ! if trigonal symmetry was selected in the first setting,
  ! then the xtal_system must be reset to 5
   if (cell%SG%SYM_trigonal) then
    cell%xtal_system=5
   end if

  ! if hexagonal setting is used, then Miller-Bravais indices must be enabled
   if ((cell%xtal_system.eq.4).OR.((cell%xtal_system.eq.5).AND.(.not.cell%SG%SYM_second))) then
    cell%hexset = .TRUE.
   else 
    cell%hexset = .FALSE.
 end if
!end if 
  
end subroutine GetLatParm

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetAsymPos
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read the atom coordinates from standard input
!
!> @details ask the user for the atom type, coordinates, site occupation parameter
!> and Debye-Waller parameter for each atom type.
!
!> @param cell unit cell pointer
!!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!--------------------------------------------------------------------------
recursive subroutine GetAsymPos(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetAsymPos

use io

IMPLICIT NONE

type(unitcell),INTENT(INOUT)            :: cell
!f2py intent(in,out) ::  cell

logical                                 :: more                 !< logical to determine if more atoms need to be entered
character(1)                            :: ans, list(256)       !< used for IO
real(kind=sgl)                          :: pt(5), out_real(5)   !< used to read and write asymmetric position data
integer(kind=irg)                       :: i, j, io_int(1), std, sl   !< auxiliary variables
character(fnlen)                        :: instring
 
 more=.TRUE.
 cell%ATOM_ntype = 0
 call Message(' Enter atoms in asymmetric unit ', frm = "(/A)")
 call DisplayElements()

 do while (more)
  cell%ATOM_ntype = cell%ATOM_ntype + 1

! atomic number
  call ReadValue(' ->  Atomic number : ', io_int, 1)
  cell%ATOM_type(cell%ATOM_ntype) = io_int(1)

! general atom coordinate
  list = (/ (' ',j=1,256) /)
  call Message(' ->  Fractional coordinates, site occupation, and Debye-Waller Factor [nm^2] : ', frm = "(A,' ')",advance="no")
  read (5,"(A)") instring
! read (5,"(256A)") list
  sl = len(trim(instring))
  j = 0
  do i=1,sl
    if (instring(i:i).ne.' ') then
      j = j+1
      list(j) = instring(i:i)
    end if
  end do

! interpret this string and extract coordinates and such ...
  call extractposition(list,pt) 
  
! store in the appropriate component of the cell variable  
  cell%ATOM_pos(cell%ATOM_ntype,1:5) = pt(1:5)

! and write the coordinate back to the terminal  
  out_real = (/ (cell%ATOM_pos(cell%ATOM_ntype,j),j=1,5) /)
  call WriteValue('    -> ', out_real, 5, frm = "(1x,4(F10.7,2x),F10.7)") 

  call ReadValue(' ->  Another atom ? (y/n) ', ans, frm = "(A1)")
  if ((ans.eq.'y').or.(ans.eq.'Y')) then 
   more=.TRUE.
  else
   more=.FALSE.
  end if 

 end do

end subroutine GetAsymPos

!--------------------------------------------------------------------------
!
! SUBROUTINE: DisplayElements
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print the periodic table
!
!> @details display the periodic table so that the user can look up the atomic number
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 modified to a fixed size table
!--------------------------------------------------------------------------
recursive subroutine DisplayElements()
!DEC$ ATTRIBUTES DLLEXPORT :: DisplayElements

use io

IMPLICIT NONE

 call Message(' ------------------------------------ Periodic Table of the Elements --------------------------------------', & 
   frm ="(/A/)")
 call Message('1:H                                                                                                    2:He', &
   frm ="(A)")
 call Message('3:Li  4:Be                                                               5:B   6:C   7:N   8:O   9:F  10:Ne', &
   frm ="(A)")
 call Message('11:Na 12:Mg                                                             13:Al 14:Si 15:P  16:S  17:Cl 18:Ar', &
   frm ="(A)")
 call Message('19:K  20:Ca 21:Sc 22:Ti 23:V  24:Cr 25:Mn 26:Fe 27:Co 28:Ni 29:Cu 30:Zn 31:Ga 32:Ge 33:As 34:Se 35:Br 36:Kr', &
   frm ="(A)")
 call Message('37:Rb 38:Sr 39:Y  40:Zr 41:Nb 42:Mo 43:Tc 44:Ru 45:Rh 46:Pd 47:Ag 48:Cd 49:In 50:Sn 51:Sb 52:Te 53: I 54:Xe', &
   frm ="(A)")
 call Message('55:Cs 56:Ba ----- 72:Hf 73:Ta 74:W  75:Re 76:Os 77:Ir 78:Pt 79:Au 80:Hg 81:Tl 82:Pb 83:Bi 84:Po 85:At 86:Rn', &
   frm ="(A)")
 call Message('87:Fr 88:Ra -----', &
   frm ="(A)/")
 call Message('57:La 58:Ce 59:Pr 60:Nd 61:Pm 62:Sm 63:Eu 64:Gd 65:Tb 66:Dy 67:Ho 68:Er 69:Tm 70:Yb 71:Lu', &
   frm ="(A)")
 call Message('89:Ac 90:Th 91:Pa 92:U', &
   frm ="(A)")
 call Message(' ----------------------------------------------------------------------------------------------------------', &
   frm ="(A)")

end subroutine DisplayElements


!--------------------------------------------------------------------------
!
! SUBROUTINE: extractposition
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extract atom position data from a string
!
!> @details Extract the coordinates, site occupation, and DW factor from the 
!> input string; note that coordinates can be entered in decimal or in fractional
!> notation, hence the somewhat convoluted way of interpreting this string...
!
!> @param list  string typed in by the user
!> @param pt set of 5 reals returned to the calling routine
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!--------------------------------------------------------------------------
recursive subroutine extractposition(list,pt)
!DEC$ ATTRIBUTES DLLEXPORT :: extractposition

IMPLICIT NONE

character(1),INTENT(IN)                 :: list(256)                              !< input string
real(kind=sgl),INTENT(OUT)              :: pt(5)                                !< output real array
integer(kind=irg)                       :: comma(6),slash(5),period(5), &
                                           ccnt,scnt,pcnt,pp,i,j,hcnt, &
                                           ip,ipt,icnt,nd,n,k,ns                !< auxiliary variables
integer(kind=irg),parameter             :: nmb(48:57)=(/0,1,2,3,4,5,6,7,8,9/)   !< list of numbers
real(kind=dbl)                          :: nominator,denominator,x              !< used for fraction interpretation
logical                                 :: hasperiod                            !< used for decimal interpretation

! first, make sure all the spaces are removed from the list array

! initalize a few variables
 comma(1:6) = 0
 slash(1:5) = 0
 period(1:5) = 0
 ccnt = 0
 scnt = 0
 pcnt = 0
 j = 0
 hcnt = 0
 
! count characters and search for , . and /
 ccnt = ccnt+1
 comma(ccnt) = 0
 do i=1,256
  if (list(i)(1:1).ne.' ') j=j+1
  if (list(i)(1:1).eq.',') then 
   ccnt = ccnt+1
   comma(ccnt)=i
  end if
  if (list(i)(1:1).eq.'/') then 
   scnt = scnt+1
   slash(scnt)=i
  end if
  if (list(i)(1:1).eq.'.') then 
   pcnt = pcnt+1
   period(pcnt)=i
  end if
 end do 
 ccnt = ccnt+1
 comma(ccnt) = j+1
 do while (ccnt.lt.6) 
  ccnt = ccnt+1
  comma(ccnt) = comma(ccnt-1)+1
 end do

! interpret the string
 j = 1
 ip = 1
 icnt = 0
 ipt = 1
 pp = 1
 do i=1,ccnt-1
! is it a real number or a fraction ?
  if (((slash(j).lt.comma(i+1)).and.(scnt.gt.0)).and.(j.le.scnt)) then
! it is a fraction;  get the nominator
   nd = slash(j)-ip
   n = 0
   do k=0,nd-1
    n = 10*n+nmb(ichar(list(ip+k)(1:1)))
   end do
   nominator = dble(n)
   ip = slash(j)+1
! and then the denominator
   nd = comma(i+1)-ip
   n = 0
   do k=0,nd-1
    n = 10*n+nmb(ichar(list(ip+k)(1:1)))
   end do
   denominator = dble(n)
! and fill in the entire range
   pt(ipt) = sngl(nominator/denominator)
   ipt = ipt+1
   ip = comma(i+1)+1
   j=j+1
  else
! no, it is a real number, possibly without a period
! is there a period in this number ?
   if ((period(pp).gt.comma(i)).and.(period(pp).lt.comma(i+1))) then
     hasperiod = .TRUE.
   else
     hasperiod = .FALSE.
   endif
   nd = comma(i+1)-ip
   if (hasperiod) then 
    if (period(pp).eq.comma(i)+1) then
     x = 0.D0
     ns = 2
    else
     x = dble(nmb(ichar(list(ip)(1:1))))
     ns = 3
    end if 
    do k=ns,nd
     x = x + 10.D0**(ns-k-1)*dble(nmb(ichar(list(ip+k-1)(1:1))))
    end do
    pt(ipt)= sngl(x)
    ipt=ipt+1
    ip = comma(i+1)+1
    pp = pp+1
   else
    nd = comma(i+1)-ip
    n = 0
    do k=0,nd-1
     n = 10*n+nmb(ichar(list(ip+k)(1:1)))
    end do
    pt(ipt) = float(n)
    ipt=ipt+1
    ip = comma(i+1)+1
   end if
  end if
 end do 

! set default values
 if (pt(4).eq.0.0) pt(4) = 1.0

end subroutine extractposition

!--------------------------------------------------------------------------
!
! Subroutine: CalcDensity
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the theoretical density as well as average Z and A
!
!> @details this routine is used by the Monte Carlo program for EBSD, and also
!> by the HEDM-GFP program.
!
!> @param cell unit cell pointer
!> @param dens density in g/cm^3
!> @param avZ average atomic number
!> @param avA average atomic weight g/mol
!
!> @todo modify call in HEDM-GFP program
!
!> @date   03/19/13 MDG 1.0 original version
!> @date   07/23/13 MDG 1.1 converted to subroutine from function
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 added unit cell pointer argument
!> @date   08/10/18 MDG 4.2 correct weight factors for average atomic number...
!--------------------------------------------------------------------------
recursive subroutine CalcDensity(cell, dens, avZ, avA, Z2percent)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcDensity

use constants

IMPLICIT NONE

type(unitcell),INTENT(IN)               :: cell
real(kind=sgl),INTENT(OUT)              :: dens, avA, avZ
real(kind=sgl),OPTIONAL,INTENT(OUT),allocatable  :: Z2percent(:)

real(kind=sgl),allocatable              :: Z2list(:)
real(kind=sgl)                          :: AW, Z
integer(kind=irg)                       :: i

! compute the total atomic weight for the unit cell (g/mol)
! also compute the total atomic number
AW = 0.0
Z = 0.0
do i = 1, cell % ATOM_ntype
  AW = AW + cell%numat(i) * ATOM_weights(cell % ATOM_type(i)) * cell % ATOM_pos(i,4)
  Z = Z + cell%numat(i) * float(cell % ATOM_type(i))
end do
avA = AW/sum( cell%numat(1:cell % ATOM_ntype) * cell%ATOM_pos(1:cell%ATOM_ntype,4) )
avZ = Z/float(sum( cell%numat(1:cell % ATOM_ntype) ))

! and compute the density in gram/centimeter^3
dens = AW / sngl(cell % vol * 1.D-21 * cAvogadro)

! do we need to fill the Z2percent array?
! this estimates the percentage contribution of each atom type to the 
! Rutherford scattering process by simply taking Z^2 for each atom in the unit cell
if (present(Z2percent)) then
  allocate(Z2percent(cell%ATOM_ntype),Z2list(cell%ATOM_ntype))
  do i=1, cell%ATOM_ntype
    Z2list(i) = cell%numat(i) * cell%ATOM_pos(i,4) * cell%ATOM_type(i)**2
  end do
  Z = sum(Z2list)
  Z2percent = 100.0 * Z2list / Z
end if 

end subroutine CalcDensity


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetOR
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief ask for orientation relation between two crystals
!
!> @details ask for orientation relation between two crystals in terms of parallel planes
!> and parallel directions; 
!
!> @param orel output variable of type orientation
!
!> @todo Is this routine really necessary ? It is not called very often.
!
!> @date 10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/19/13 MDG 3.0 interface support
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 added stdout 
!> @date   03/29/18 MDG 4.2 removed stdout 
!--------------------------------------------------------------------------
recursive subroutine GetOR(orel)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOR

use io

IMPLICIT NONE

type(orientation),INTENT(OUT)                   :: orel                 !< orientation relation type

real(kind=sgl)                                  :: c1,c2                !< auxiliary variables
integer(kind=irg)                               :: io_int(6)            !< used for IO

 
 c1 = 1.0_sgl
 c2 = 1.0_sgl
 do while ((c1.ne.0.0_sgl).or.(c2.ne.0.0_sgl))
  call Message('Enter orientation relation in following form:', frm = "(A)")
  call Message('planes:     h_A,k_A,l_A,h_B,k_B,l_B ', frm = "(A)")
  call Message('directions: u_A,v_A,w_A,u_B,v_B,w_B ', frm = "(A)")
  call ReadValue('Plane normals :', io_int, 6) 
  orel%gA(1:3) = float(io_int(1:3))
  orel%gB(1:3) = float(io_int(4:6))
  call ReadValue('Directions    :', io_int, 6) 
  orel%tA(1:3) = float(io_int(1:3))
  orel%tB(1:3) = float(io_int(4:6))

! check for orthonormality using zone equation
  c1=sum(orel%tA*orel%gA)
  if (c1.ne.0.0_sgl) then
   call Message('Plane does not contain direction (crystal A)', frm ="(A)")
  end if
  c2=sum(orel%tB*orel%gB)
  if (c2.ne.0.0_sgl) then
   call Message('Plane does not contain direction (crystal B)', frm = "(A)")
  end if
 end do

end subroutine GetOR


!--------------------------------------------------------------------------
!
! SUBROUTINE: ComputeOR
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the orientation relation transformation matrix
!
!> @param orel output variable of type orientation
!> @param cellA unit cell A pointer
!> @param cellB unit cell B pointer
!> @param direction 'AB' for A new, B old; 'BA' for B new, A old
!
!> @date   12/20/13 MDG 1.0 first version, used for EMoverlap and EMorient
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 modification for cell pointers
!> @date   09/21/15 SS  4.2 correction in final step (buggy lines commented out and new lines marked)
!--------------------------------------------------------------------------
recursive function ComputeOR(orel, cellA, cellB, direction) result(TT)
!DEC$ ATTRIBUTES DLLEXPORT :: ComputeOR

use math
use io
use error

IMPLICIT NONE

type(orientation),INTENT(INOUT) :: orel         !< orientation relation type
!f2py intent(in,out) ::  orel         !< orientation relation type
type(unitcell)                  :: cellA, cellB 
character(2),INTENT(IN)         :: direction  !< direction of transformation (AB or BA)
real(kind=sgl)                  :: TT(3,3)

real(kind=sgl)                  :: r(3), p(3), Ep(3,3), E(3,3)
real(kind=dbl)                  :: dE(3,3)
integer(kind=irg)               :: i


! compute E matrix  [page 74]
 call TransSpace(cellA, orel % gA,r,'r','d')
 call NormVec(cellA, r,'d')
 call NormVec(cellA, orel % tA,'d')
 call CalcCross(cellA, orel % tA,r,p,'d','d',0)
 call NormVec(cellA, p,'d')
 E(1,1:3)=r(1:3)
 E(2,1:3)=p(1:3)
 E(3,1:3)=orel % tA(1:3)
 !if (direction.eq.'AB') then
 !  call mInvert(dble(E),dE,.FALSE.)
 !  E = sngl(dE)
 !end if

! compute E-prime matrix 
 call TransSpace(cellB, orel % gB,r,'r','d')
 call NormVec(cellB, r,'d')
 call NormVec(cellB, orel % tB,'d')
 call CalcCross(cellB, orel % tB,r,p,'d','d',0)
 call NormVec(cellB, p,'d')
 Ep(1,1:3)=r(1:3)
 Ep(2,1:3)=p(1:3)
 Ep(3,1:3)=orel % tB(1:3)
 !if (direction.eq.'BA') then
 !  call mInvert(dble(Ep),dE,.FALSE.)
 !  Ep = sngl(dE)
 !end if

! and multiply E^(-1)Ep or Ep^(-1)E matrices to get transformation matrix M
 if (direction.eq.'BA') then
!=============NEW==================
   call mInvert(dble(Ep),dE,.FALSE.)
   Ep = sngl(dE) 
!==================================
   TT = matmul(Ep,E)
 else if (direction .eq. 'AB') then
!=============NEW==================
   call mInvert(dble(E),dE,.FALSE.)
   E = sngl(dE)
!==================================   
   TT = matmul(E,Ep)
 else
   call FatalError('ComputeOR','Unknown direction specified')
 end if

end function ComputeOR

!--------------------------------------------------------------------------
! 
! FUNCTION:CalcsgHOLZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the excitation error including HOLZ and Laue Center information
!
!> @details  see chapter 3
!
!> @param cell unit cell pointer
!> @param HOLZdata HOLZ data structure
!> @param gg input g vector
!> @param kt tangential components of wave vector
!> @param lambda electron wavelength
! 
!> @date   10/16/13 MDG 1.0 new version, includes HOLZ stuff
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 added unit cell pointer argument and HOLZdata argument
!--------------------------------------------------------------------------
recursive function CalcsgHOLZ(cell,HOLZdata,gg,kt,lambda) result(exer)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcsgHOLZ

IMPLICIT NONE

type(unitcell)                          :: cell
type(HOLZentries),INTENT(INOUT)         :: HOLZdata
!f2py intent(in,out) ::  HOLZdata
real(kind=sgl),INTENT(IN)               :: gg(3), kt(3), lambda

real(kind=sgl)                          :: exer, g1len, g2len
real(kind=sgl)                          :: ll(3), lpg(3), glen, gplen, LC1, LC2, LC3, sgdenom


glen = CalcLength(cell,gg,'r')
g1len = CalcLength(cell,HOLZdata%g1,'r')
g2len = CalcLength(cell,HOLZdata%g2,'r')
if (glen.ne.0.0) then
  LC1 = CalcDot(cell,kt,HOLZdata%g1,'r')/g1len
  LC2 = CalcDot(cell,kt,HOLZdata%g2,'r')/g2len
  ll = LC1*HOLZdata%g1 + LC2*HOLZdata%g2
  lpg = ll + gg
  gplen = CalcLength(cell,lpg,'r')
  LC3 = sqrt(1.0-lambda**2*CalcLength(cell,ll,'r')**2)
  if (gplen.eq.0.0) then
    exer = -lambda*CalcDot(cell,gg,2.0*ll+gg,'r')/(2.0*LC3*CalcDot(cell,HOLZdata%g3,HOLZdata%FNr,'r'))
  else
    sgdenom = 2.0*CalcDot(cell,LC3*HOLZdata%g3-lambda*lpg,HOLZdata%FNr,'r')
    exer = (CalcDot(cell,lpg,2.0*LC3*HOLZdata%g3-lambda*gg,'r')-lambda*CalcDot(cell,gg,ll,'r'))/sgdenom
  end if
else
  exer = 10000.0
end if

end function CalcsgHOLZ




!--------------------------------------------------------------------------
! 
! SUBROUTINE:GetHOLZGeometry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize HOLZ geometrical data for a given zone axis
!
!> @details  see chapter 3
!
!> @param cell unit cell pointer
!> @param HOLZdata HOLZ data structure
!> @param g1 first ZOLZ vector
!> @param g2 second ZOLZ vector
!> @param uvw zone axis
!> @param FN foil normal
! 
!> @date 10/17/13 MDG 1.0 original
!> @date   01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 added unit cell pointer argument and HOLZdata argument
!--------------------------------------------------------------------------
recursive subroutine GetHOLZGeometry(cell,HOLZdata,g1,g2,uvw,fn)
!DEC$ ATTRIBUTES DLLEXPORT :: GetHOLZGeometry

use io
use error

IMPLICIT NONE

type(unitcell)                          :: cell
type(HOLZentries),INTENT(INOUT)         :: HOLZdata
!f2py intent(in,out) ::  HOLZdata
integer(kind=irg),INTENT(IN)            :: uvw(3), fn(3)
real(kind=sgl),INTENT(IN)               :: g1(3), g2(3)

real(kind=sgl)                          :: gmin,gam11,gam12,gam22, phi, glen, g3(3), c(3), gx(3), gy(3), gshort(3)
integer(kind=irg),parameter             :: inm = 8
integer(kind=irg)                       :: ih,ik,il,NN, oi_int(1)

! set some basic values
    HOLZdata%g1 = g1
    HOLZdata%g2 = g2
    HOLZdata%uvw = uvw
    HOLZdata%FN = fn
    
! distance between consecutive HOLZ layers in nm-1
    HOLZdata%H = 1.0/CalcLength(cell,float(uvw),'d')

! determine g3 basis vector
    call CalcCross(cell,HOLZdata%g1,HOLZdata%g2,g3,'r','r',1)
    call NormVec(cell,g3,'r')
    HOLZdata%g3 = HOLZdata%H * g3

! compute components of FN with respect to ga, gb, g3
    call TransSpace(cell,float(HOLZdata%FN),HOLZdata%FNr,'d','r')
    call NormVec(cell,HOLZdata%FNr,'r')
    HOLZdata%FNg = (/ CalcDot(cell,HOLZdata%FNr,HOLZdata%g1,'r'), CalcDot(cell,HOLZdata%FNr,HOLZdata%g2,'r'), &
                        CalcDot(cell,HOLZdata%FNr,g3,'r') /)

! look for the shortest reflection satisfying hu+kv+lw = 1
! This could be replaced by code from Jackson's paper (1987),
! but it does essentially the same thing.
 gmin = 100.0
 NN=1
 do while((gmin.eq.100.0).and.(NN.lt.4))
  do ih=-inm,inm
   do ik=-inm,inm
    do il=-inm,inm
! does this reflection lie in the plane NN ?
     if ((ih*uvw(1)+ik*uvw(2)+il*uvw(3)).eq.NN) then
      glen = CalcLength(cell,float((/ih,ik,il/)),'r')
      if (glen.lt.gmin) then
       gmin = glen
       gshort = float( (/ ih,ik,il /) )
      end if
     end if
    end do
   end do
  end do
  oi_int(1) = NN
  call WriteValue(' Could not find any reflections with hu+kv+lw = ', oi_int, 1, frm = "(I2)")
  NN = NN+1
 end do
 if (gmin.eq.100.0) then ! for some reason there is no reflection with N<=3 ...
  call FatalError('ShortestGFOLZ: ',' could not find any reflections with hu+kv+lw<=3 ...')
 end if
 HOLZdata%gshort = gshort

! projected components of G
 gam11 = CalcDot(cell,g1,g1,'r')
 gam12 = CalcDot(cell,g1,g2,'r')
 gam22 = CalcDot(cell,g2,g2,'r')
 gmin = 1.0/(gam11*gam22-gam12**2)
 HOLZdata%gp(1) = (CalcDot(cell,gshort,g1,'r')*gam22-CalcDot(cell,gshort,g2,'r')*gam12)*gmin
 HOLZdata%gp(2) = (CalcDot(cell,gshort,g2,'r')*gam11-CalcDot(cell,gshort,g1,'r')*gam12)*gmin

! coordinate transformation matrix for g1 along x (our standard orientation for all programs)
 phi = CalcAngle(cell,g1,g2,'r')
 glen = CalcLength(cell,g2,'r')
 HOLZdata%gtoc(1,1) = CalcLength(cell,g1,'r')
 HOLZdata%gtoc(1,2) = glen * cos(phi)
 HOLZdata%gtoc(2,1) = 0.0
 HOLZdata%gtoc(2,2) = glen * sin(phi)

! first normalize the zone axis in cartesian components; this is the z-axis
  call TransSpace(cell,float(uvw),c,'d','c')
  call NormVec(cell,c,'c')

! then make ga the x-axis
  call TransSpace(cell,g1,gx,'r','c')
  call NormVec(cell,gx,'c')
  HOLZdata%gx = gx

! compute the cross product between k and gx; this is the y-axis
  call CalcCross(cell,c,gx,gy,'c','c',0)
  HOLZdata%gy = gy


end subroutine GetHOLZGeometry

!--------------------------------------------------------------------------
! 
! FUNCTION:GetHOLZcoordinates
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief find the projected coordinates of an arbitrary HOLZ g-vector
!
!> @details  see chapter 3
!
!> @param gg input g vector
!> @param kt tangential wave vector component
!> @param lambda electron wavelength
! 
!> @date 1/29/02  MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 10/16/13 MDG 3.0 incorporation into LACBED code
!> @date 01/10/14 MDG 4.0 checked for changes to unitcell type
!> @date   06/05/14 MDG 4.1 added unit cell pointer argument and HOLZdata argument
!--------------------------------------------------------------------------
recursive function GetHOLZcoordinates(cell,HOLZdata,gg,kt,lambda) result(pxy)
!DEC$ ATTRIBUTES DLLEXPORT :: GetHOLZcoordinates

IMPLICIT NONE

type(unitcell)                          :: cell
type(HOLZentries),INTENT(INOUT)         :: HOLZdata
!f2py intent(in,out) ::  HOLZdata
real(kind=sgl),INTENT(IN)               :: gg(3), kt(3), lambda

real(kind=sgl)                          :: pxy(2), h1, h2, g11, g12, g22, z
real(kind=sgl)                          :: exer, correction, gxy(2), nx, ny, hh(3)
integer(kind=irg)                       :: N

! get the Laue zone number
        N = abs( HOLZdata%uvw(1)*gg(1) + HOLZdata%uvw(2)*gg(2) + HOLZdata%uvw(3)*gg(3) )

! get components of gg w.r.t. g1 and g2
        hh = gg - N * HOLZdata%gshort 
        h1 = CalcDot(cell,hh,HOLZdata%g1,'c')
        h2 = CalcDot(cell,hh,HOLZdata%g2,'c')
        g11 = CalcDot(cell,HOLZdata%g1,HOLZdata%g1,'c')
        g12 = CalcDot(cell,HOLZdata%g1,HOLZdata%g2,'c')
        g22 = CalcDot(cell,HOLZdata%g2,HOLZdata%g2,'c')
        z = 1.0/(g12**2-g11*g22)
        nx = (g12*h2-g22*h1)*z
        ny = (g12*h1-g11*h2)*z

! compute excitation error, including Laue center, foil normal, and HOLZ reflection.
        exer = CalcsgHOLZ(cell,HOLZdata,gg,kt,lambda)

! next, determine the drawing coordinates, first in terms of g1 and g2
        correction = 1.0/(1.0-lambda*HOLZdata%H*(float(N)+exer*HOLZdata%FNg(3)))
        gxy = (/ (nx+N*HOLZdata%gp(1)+exer*HOLZdata%FNg(1)), (ny+N*HOLZdata%gp(2)+exer*HOLZdata%FNg(2))  /) * correction

! convert to Cartesian drawing coordinates
        pxy = matmul(HOLZdata%gtoc,gxy)

end function GetHOLZcoordinates
 
 
!--------------------------------------------------------------------------
! 
! FUNCTION:Convert_kgs_to_Substrate
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert a film wave vector into substrate coordinates
!
!> @param cell film unit cell pointer
!> @param cellS substrate unit cell pointer
!> @param kg wave vector components in film
!> @param TTinv coordinate transformation matrix
!> @param lambdaS electron wavelength in substrate, corrected for refraction
!> @param FN foil normal in substrate frame
! 
!> @date 11/25/14 MDG 1.0 original
!> @date 08/19/15 SS  1.1 replaced FN by r in evaluation of tangential 
!> component and corrected calculation of kgS from p1
!> @date 09/23/15 commented out a few lines
!--------------------------------------------------------------------------
recursive function Convert_kgs_to_Substrate(cell, cellS, kg, TTinv, FN) result(kgS)
!DEC$ ATTRIBUTES DLLEXPORT :: Convert_kgs_to_Substrate

use local
use typedefs

IMPLICIT NONE

type(unitcell)                          :: cell, cellS
real(kind=sgl),INTENT(IN)               :: kg(3)
real(kind=sgl),INTENT(IN)               :: TTinv(3,3)
real(kind=sgl),INTENT(IN)               :: FN(3)
real(kind=sgl)                          :: kgS(3)
real(kind=sgl)                          :: dp,normal
real(kind=sgl)                          :: tangential(3)

real(kind=sgl)                          :: p(3), r(3), p1(3)

! convert to direct space for transforming to substrate frame
call TransSpace(cell,kg,p,'r','d')

! convert to substrate frame
p = matmul(TTinv,p)

! convert to cartesian frame and get correct length of wavevector
call TransSpace(cellS,p,p1,'d','c')

!call NormVec(cellS,p1,'c')
!p1 = p1/cell%mLambda

! convert foil normal to cartesian frame and get normal component of wavevector
call TransSpace(cellS,FN,r,'r','c')
call NormVec(cellS,r,'c')
dp = CalcDot(cellS,p1,r,'c')

! subtract out the normal component to get tangential component; this is conserved across the interface
tangential = p1 - dp*r

! get magnitude of normal component
normal = sqrt((1.0/cellS%mLambda)**2 - sum(tangential**2))

p1 = normal*r + tangential

call NormVec(cellS,p1,'c')

p1 = p1/cellS%mLambda

call TransSpace(cellS,p1,kgS,'c','r')

end function Convert_kgs_to_Substrate

end module crystal
