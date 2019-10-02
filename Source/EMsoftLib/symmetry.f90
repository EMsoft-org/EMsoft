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
! EMsoft:symmetry.f90
!--------------------------------------------------------------------------
!
! MODULE: symmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief all symmetry-related routines
!
!> @details routines to generate a space group based on the generator string; computation
!> of orbits and families; computation of all atoms in a single or multiple unit cells
! 
!> @date  01/05/99 MDG 1.0 original
!> @date  05/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/19/13 MDG 3.0 updated IO and such
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable 
!> @sdate 09/05/16 MDG 4.2 added Wyckoff Position routines
!--------------------------------------------------------------------------

module symmetry

use local
use typedefs

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: SYM_fillgen
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a generator matrix
!
!> @details  fills in a generator matrix based on an input 4-character code string
!
!> @param cell unit cell pointer
!> @param t input string (4 character block from generator string)
!> @param isgn switch to indicate forward or reverse translation component
!
!> @date  10/13/98 MDG 1.0 original
!> @date  05/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable 
!--------------------------------------------------------------------------
recursive subroutine SYM_fillgen(cell,t,isgn)
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_fillgen

IMPLICIT NONE

type(unitcell)          :: cell
character(1),INTENT(IN)         :: t(4) !< 4-character input string
integer(kind=irg),INTENT(IN)            :: isgn !< indicates forward or reverse translation

integer(kind=irg)                       :: j    !< auxiliary variable
real(kind=dbl)                          :: sgn  !< forward or reverse multiplier for translation components

! forward or reverse translation ?
 sgn=dble(isgn)

! first fill the array with zeroes and a 1 at 4,4
 cell%SG%SYM_c(1:4,1:4) = 0.0_dbl
 cell%SG%SYM_c(4,4) = 1.0_dbl

! then check for the particular matrix type
 select case (t(1))
  case ('a'); cell%SG%SYM_c(1,1) = 1.0_dbl; cell%SG%SYM_c(2,2) = 1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
  case ('b'); cell%SG%SYM_c(1,1) =-1.0_dbl; cell%SG%SYM_c(2,2) =-1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
  case ('c'); cell%SG%SYM_c(1,1) =-1.0_dbl; cell%SG%SYM_c(2,2) = 1.0_dbl; cell%SG%SYM_c(3,3) =-1.0_dbl
  case ('d'); cell%SG%SYM_c(1,3) = 1.0_dbl; cell%SG%SYM_c(2,1) = 1.0_dbl; cell%SG%SYM_c(3,2) = 1.0_dbl
  case ('e'); cell%SG%SYM_c(1,2) = 1.0_dbl; cell%SG%SYM_c(2,1) = 1.0_dbl; cell%SG%SYM_c(3,3) =-1.0_dbl
  case ('f'); cell%SG%SYM_c(1,2) =-1.0_dbl; cell%SG%SYM_c(2,1) =-1.0_dbl; cell%SG%SYM_c(3,3) =-1.0_dbl
  case ('g'); cell%SG%SYM_c(1,2) =-1.0_dbl; cell%SG%SYM_c(2,1) = 1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
  case ('h'); cell%SG%SYM_c(1,1) =-1.0_dbl; cell%SG%SYM_c(2,2) =-1.0_dbl; cell%SG%SYM_c(3,3) =-1.0_dbl
  case ('i'); cell%SG%SYM_c(1,1) = 1.0_dbl; cell%SG%SYM_c(2,2) = 1.0_dbl; cell%SG%SYM_c(3,3) =-1.0_dbl
  case ('j'); cell%SG%SYM_c(1,1) = 1.0_dbl; cell%SG%SYM_c(2,2) =-1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
  case ('k'); cell%SG%SYM_c(1,2) =-1.0_dbl; cell%SG%SYM_c(2,1) =-1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
  case ('l'); cell%SG%SYM_c(1,2) = 1.0_dbl; cell%SG%SYM_c(2,1) = 1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
  case ('m'); cell%SG%SYM_c(1,2) = 1.0_dbl; cell%SG%SYM_c(2,1) =-1.0_dbl; cell%SG%SYM_c(3,3) =-1.0_dbl
  case ('n'); cell%SG%SYM_c(1,2) =-1.0_dbl; cell%SG%SYM_c(2,1) = 1.0_dbl; cell%SG%SYM_c(2,2) =-1.0_dbl; 
              cell%SG%SYM_c(3,3) = 1.0_dbl
 end select

! then fill in the translational component
 do j=2,4 
  select case (t(j))
   case('A'); cell%SG%SYM_c(j-1,4) = sgn/6.0_dbl
   case('B'); cell%SG%SYM_c(j-1,4) = sgn/4.0_dbl
   case('C'); cell%SG%SYM_c(j-1,4) = sgn/3.0_dbl
   case('D'); cell%SG%SYM_c(j-1,4) = sgn/2.0_dbl
   case('E'); cell%SG%SYM_c(j-1,4) = sgn*2.0_dbl/3.0_dbl
   case('F'); cell%SG%SYM_c(j-1,4) = sgn*3.0_dbl/4.0_dbl
   case('G'); cell%SG%SYM_c(j-1,4) = sgn*5.0_dbl/6.0_dbl
   case('O'); cell%SG%SYM_c(j-1,4) = 0.0_dbl
   case('X'); cell%SG%SYM_c(j-1,4) = -sgn*3.0_dbl/8.0_dbl
   case('Y'); cell%SG%SYM_c(j-1,4) = -sgn/4.0_dbl
   case('Z'); cell%SG%SYM_c(j-1,4) = -sgn/8.0_dbl
  end select
 end do

end subroutine SYM_fillgen

!--------------------------------------------------------------------------
!
! SUBROUTINE: MakeGenerators
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief construct all generator matrices
!
!> @details interprets the generator string and initializes all generator matrices
!
!> @param cell unit cell pointer
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/19/13 MDG 3.0 general cleanup
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable 
!--------------------------------------------------------------------------!     
recursive subroutine MakeGenerators(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: MakeGenerators

use math

IMPLICIT NONE

type(unitcell)          :: cell

integer(kind=irg),parameter     :: QQ=48                        !< maximum number of point group symmetry elements
integer(kind=irg)               :: i,k,l,iset                   !< auxiliary variables
real(kind=dbl)                  :: SYM_d(4,4),SYM_e(4,4)        !< auxiliary 4x4 matrices
real(kind=dbl), parameter       :: eps=0.0005_dbl               !< constant used to decide whether or not elements are equal 
character(1)                    :: t(4)                         !< 4-character string etracted from generator string
character(40)                   :: genst                        !< full generator string

! fill in the space group name 
 cell%SG%SYM_name = SYM_SGname(cell%SYM_SGnum)

! initialize the encoded identity operator aOOO
 t = (/ 'a', 'O', 'O', 'O' /)
! compute its matrix
 call SYM_fillgen(cell,t,1)
! and put it first in the list of matrices
 cell%SG%SYM_data(1,:,:) = cell%SG%SYM_c(:,:)

! get the space group generator string 
 genst = SYM_GL(cell%SYM_SGnum)

! initialize the number of generators 
 cell%SG%SYM_GENnum = ichar(genst(2:2))-QQ

! create the generator matrices 
 do i=2,2+cell%SG%SYM_GENnum - 1
   do  k=1,4
     l=2+4*(i-2)+k
     t(k) = genst(l:l)
   end do
   call SYM_fillgen(cell,t,1)
   cell%SG%SYM_data(i,:,:) = cell%SG%SYM_c(:,:)
 end do

! this is where we are in the generator string
 i=2+4*cell%SG%SYM_GENnum+1

! if there is inversion symmetry, add the inversion to the generators 
 if (genst(1:1).eq.'1') then 
  cell%SG%SYM_centrosym=.TRUE.
  t = (/ 'h', 'O', 'O', 'O' /)
  call SYM_fillgen(cell,t,1)
  cell%SG%SYM_data(cell%SG%SYM_GENnum+2,:,:) = cell%SG%SYM_c(:,:)
  cell%SG%SYM_GENnum = cell%SG%SYM_GENnum+2
 else   
  cell%SG%SYM_GENnum = cell%SG%SYM_GENnum+1
 end if

! now check for special origin conditions (choices 1 and 2) 
 if (genst(i:i).ne.'0') then 
  if (cell%SYM_SGset.eq.0) then
   call GetSetting(cell,iset)
   cell%SYM_SGset=iset
  end if
  if (cell%SYM_SGset.eq.2) then 
! second setting: apply translation transformation to generators
   t(1)='a'
   do k=2,4
    l=i+k-1
    t(k) = genst(l:l)
   end do
   do l=2,cell%SG%SYM_GENnum 
! translate to first setting origin
    call SYM_fillgen(cell,t,-1)
    SYM_d(:,:)=cell%SG%SYM_data(l,:,:)
! apply generator
    SYM_e = matmul(SYM_d,cell%SG%SYM_c)
! translate back to second setting origin
    call SYM_fillgen(cell,t,1)
    SYM_d = matmul(cell%SG%SYM_c,SYM_e)
! reduce the translations to the fundamental unit cell
    do  k=1,3
     if (abs(SYM_d(k,4)).lt.eps) SYM_d(k,4)=0.0_dbl
     if (SYM_d(k,4).lt.0.0_dbl) then 
      do while (SYM_d(k,4).lt.0.0_dbl) 
       SYM_d(k,4)=SYM_d(k,4)+1.0_dbl
      end do
     end if
     if (SYM_d(k,4).ge.1.0_dbl) then 
      do while (SYM_d(k,4).ge.1.0_dbl) 
       SYM_d(k,4)=SYM_d(k,4)-1.0_dbl
      end do
     end if
     if (abs(SYM_d(k,4)-1.0_dbl).lt.eps) SYM_d(k,4)=0.0_dbl
    end do
! and store the result in the SYM_data array
    cell%SG%SYM_data(l,:,:)=SYM_d(:,:)
   end do
  end if ! if (SYM_SGset.eq.2)
 end if ! if (genst(i:i).ne.'0')

end subroutine MakeGenerators

!--------------------------------------------------------------------------
!
! SUBROUTINE: matrixmult
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief multiply 4x4 matrices and reduce translation component
!
!> @details multiplies two 4x4 symmetry matrices and brings
!> the translation component back to the fundamental unit cell.
!
!> @param cell unit cell pointer
!> @param k1 index of first input matrix
!> @param k2 index of second input matrix
!
!> @note there is no output since the result is stored in the SG structure SYM_c array
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/19/13 MDG 3.0 general cleanup
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable 
!--------------------------------------------------------------------------
recursive subroutine matrixmult(cell, k1, k2)
!DEC$ ATTRIBUTES DLLEXPORT :: matrixmult
   
IMPLICIT NONE

type(unitcell)          :: cell

integer(kind=irg),INTENT(IN)    :: k1                           !< index of first 4x4 input matrix
integer(kind=irg),INTENT(IN)    :: k2                           !< index of second 4x4 input matrix
integer(kind=irg)               :: i,j,k                        !< loop counters
real(kind=dbl),parameter        :: eps=0.0005_dbl               !< truncation constant

 do i=1,4
  do j=1,4
   cell%SG%SYM_c(i,j) = 0.0_dbl
   do k=1,4
    cell%SG%SYM_c(i,j)=cell%SG%SYM_c(i,j)+cell%SG%SYM_data(k1,i,k)*cell%SG%SYM_data(k2,k,j)
   end do
  end do
 end do

! bring the translational part of the matrix back to
! the first unit cell and correct possible rounding errors
 do  k=1,3
  if (abs(cell%SG%SYM_c(k,4)).lt.eps) cell%SG%SYM_c(k,4)=0.0_dbl
  if (cell%SG%SYM_c(k,4).lt.0.0_dbl) then 
   do while (cell%SG%SYM_c(k,4).lt.0.0_dbl) 
    cell%SG%SYM_c(k,4)=cell%SG%SYM_c(k,4)+1.0_dbl
   end do
  end if
  if (cell%SG%SYM_c(k,4).gt.1.0_dbl) then 
   do while (cell%SG%SYM_c(k,4).gt.1.0_dbl) 
    cell%SG%SYM_c(k,4)=cell%SG%SYM_c(k,4)-1.0_dbl
   end do
  end if
  if (abs(cell%SG%SYM_c(k,4)-1.0_dbl).lt.eps) cell%SG%SYM_c(k,4)=0.0_dbl
 end do

end subroutine matrixmult

!--------------------------------------------------------------------------
!
! FUNCTION: isitnew
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief is this a new symmetry operator?
!
!> @details check whether or not this is a new operator by simply comparing it 
!> with all existing operators
!
!> @param cell unit cell pointer
!> @param nsym index of matrix to be compared
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable 
!--------------------------------------------------------------------------
logical recursive function isitnew(cell,nsym)

IMPLICIT NONE

type(unitcell)          :: cell

integer(kind=irg),INTENT(IN)            :: nsym                 !< index of matrix to be compared
integer(kind=irg)                       :: i,j,k,n              !< loop counters
real(kind=dbl),parameter                :: eps=0.0005_dbl       !< comparison threshold

 k=0
 n=0
 do while ((k.le.nsym).and.(n.ne.12))
  n=0
  k=k+1
  do i=1,3
   do j=1,4
    if (abs(cell%SG%SYM_c(i,j)- cell%SG%SYM_data(k,i,j)).lt.eps) n=n+1
   end do
  end do
 end do
 
 if (n.ne.12) then 
  isitnew=.TRUE.
 else
  isitnew=.FALSE.
 end if

end function isitnew

!--------------------------------------------------------------------------
!
! SUBROUTINE: GenerateSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute all relevant symmetry operators
!
!> @details compute all symmetry operators and store them in cell%SG%SYM_data.
!               
!> @note These routines are based on a program written by G. Ceder (MIT).
!
!> @param cell unit cell pointer
!> @param dopg include point group matrices or not (logical)
!
!> @date  10/13/98 MDG/Ceder 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/19/13 MDG 3.0 clean up
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable 
!--------------------------------------------------------------------------
recursive subroutine GenerateSymmetry(cell,dopg)
!DEC$ ATTRIBUTES DLLEXPORT :: GenerateSymmetry

IMPLICIT NONE

type(unitcell)          :: cell
logical,INTENT(IN)              :: dopg                         !< logical to determine if point group matrices are to be computed as well

integer(kind=irg)               :: i,j,k,nsym,k1,k2,l1,l2       !< loop counters (mostly)
real(kind=dbl)                  :: q,sm                         !< auxiliary variables.

! create the space group generator matrices
 call MakeGenerators(cell)
 nsym = cell%SG%SYM_GENnum

! generate new elements from the squares of the generators 
 do k=1,cell%SG%SYM_GENnum 
  call matrixmult(cell,k,k)
  if (isitnew(cell,nsym).eqv..TRUE.) then 
   nsym=nsym+1
   cell%SG%SYM_data(nsym,:,:) = cell%SG%SYM_c(:,:)
  end if
 end do

! generate the remainder of the factorgroup
 k1=1
 do while (k1.le.nsym) 
  k2=k1+1
  do while (k2.le.nsym)
   call matrixmult(cell,k2,k1)
   if (isitnew(cell,nsym).eqv..TRUE.) then 
    nsym=nsym+1
    cell%SG%SYM_data(nsym,:,:) = cell%SG%SYM_c(:,:)
    if (nsym.ge.192) then 
     k2 = nsym
     k1 = nsym
    end if
   end if
   k2=k2+1
  end do
  k1=k1+1
 end do
 cell%SG%SYM_MATnum = nsym

! reduce the translation operators to the fundamental unit cell
 do i=1,cell%SG%SYM_MATnum
  do j=1,3
   cell%SG%SYM_data(i,j,4)=mod( cell%SG%SYM_data(i,j,4),1.0_dbl)
  end do
 end do

 if (dopg) then
! tag the point symmetry operators
! this is used to determine families of directions;
! for planes we must determine the transformed point symmetry
! operators SYM_recip() (this requires the metric tensors)
  cell%SG%SYM_NUMpt=0
  do i=1,cell%SG%SYM_MATnum 
   sm=cell%SG%SYM_data(i,1,4)**2+cell%SG%SYM_data(i,2,4)**2+cell%SG%SYM_data(i,3,4)**2
   if (sm.lt.0.1_dbl) then
    cell%SG%SYM_NUMpt=cell%SG%SYM_NUMpt+1

! direct space point group symmetry elements
    cell%SG%SYM_direc(cell%SG%SYM_NUMpt,:,:)=cell%SG%SYM_data(i,1:3,1:3)

! reciprocal space point group symmetry elements
    do j=1,3
     do k=1,3 
      q=0.0_dbl
      do l1=1,3
       do l2=1,3
        q=q+cell%dmt(j,l1)*cell%SG%SYM_data(i,l1,l2)*cell%rmt(l2,k)
       end do
      end do
      cell%SG%SYM_recip(cell%SG%SYM_NUMpt,j,k)=q
     end do
    end do
   end if  ! (sm.lt.0.1)
  end do
 end if ! if (dopg.eq..TRUE.)
! this completes generation of the factor group 

! and finally, let's determine whether or not this space group is non-symmorphic
 cell%nonsymmorphic = (minval(abs(SGsym - cell % SYM_SGnum)).ne.0) 

end subroutine GenerateSymmetry

!--------------------------------------------------------------------------
!
! SUBROUTINE: Calc2DFamily
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the indices of equivalent planes/directions
!
!> @details compute the indices of equivalent planes w.r.t. 2D symmetry and store them in the itmp array
!               
!> @param cell unit cell pointer
!> @param ind input index triplet
!> @param ksame logical list with symmetry operators to consider
!> @param numksame number of entries in ksame to be considered
!> @param nunique number of unique entries in itmp
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable; replaced itmp by argument
!--------------------------------------------------------------------------
recursive subroutine Calc2DFamily(cell,ind,ksame,numksame,nunique,itmp)
!DEC$ ATTRIBUTES DLLEXPORT :: Calc2DFamily
        
IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(IN)            :: ind(3)                       !< input triplet
logical,INTENT(IN)                      :: ksame(*)                     !< list of symmetry operators
integer(kind=irg),INTENT(IN)            :: numksame                     !< number on the input list
integer(kind=irg),INTENT(OUT)           :: nunique                      !< number of equivalent entries generated
integer(kind=irg),INTENT(OUT)           :: itmp(48,3)                   !< array used for family computations etc

integer(kind=irg)                       :: m,i,j                        !< loop counters and such
real(kind=sgl)                          :: h,k,l,ih,ik,il,idiff !< auxiliary variables
logical                                 :: newpoint                     !< is this a new point ?
real,parameter                          :: eps=0.0001_sgl               !< comparison threshold

! first take the identity
 j=1
 itmp(j,1:3)=ind(1:3)
 h=float(ind(1))
 k=float(ind(2))
 l=float(ind(3))

! multiply with all point group elements that have the value .TRUE. in ksame
 do i=2,cell%SG%SYM_NUMpt 
  if (ksame(i)) then 
    ih=cell%SG%SYM_direc(i,1,1)*h+cell%SG%SYM_direc(i,1,2)*k+cell%SG%SYM_direc(i,1,3)*l
    ik=cell%SG%SYM_direc(i,2,1)*h+cell%SG%SYM_direc(i,2,2)*k+cell%SG%SYM_direc(i,2,3)*l
    il=cell%SG%SYM_direc(i,3,1)*h+cell%SG%SYM_direc(i,3,2)*k+cell%SG%SYM_direc(i,3,3)*l

! is this a new point ?
   newpoint=.TRUE.
   do m=1,j+1
    idiff=(itmp(m,1)-ih)**2+(itmp(m,2)-ik)**2+(itmp(m,3)-il)**2
    if (idiff.lt.eps) newpoint=.FALSE.
   end do

   if (newpoint) then 
    j=j+1
    itmp(j,1)=nint(ih)
    itmp(j,2)=nint(ik)
    itmp(j,3)=nint(il)
   end if
  end if
 end do 
 nunique=j

end subroutine Calc2DFamily

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcFamily
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the indices of equivalent planes/directions
!
!> @details compute the indices of equivalent planes/directions and store them in the itmp array
!               
!> @param cell unit cell pointer
!> @param ind input index triplet
!> @param num output number of family members generated
!> @param space space in which to perform the computation (direct 'd' or reciprocal 'r')
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable; replaced itmp by argument
!--------------------------------------------------------------------------
recursive subroutine CalcFamily(cell,ind,num,space,itmp)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcFamily

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(OUT)           :: num                          !< number of equivalent entries generated
integer(kind=irg),INTENT(IN)            :: ind(3)                       !< input triplet
character(1),INTENT(IN)                 :: space                        !< 'd' or 'r'
integer(kind=irg),INTENT(OUT)           :: itmp(48,3)                   !< array used for family computations etc

integer(kind=irg)                       :: m,i,j                        !< loop counters and such
real(kind=sgl)                          :: h,k,l,ih,ik,il,idiff         !< auxiliary variables
logical                                 :: newpoint                     !< is this a new point ?
real,parameter                          :: eps=0.0001_sgl               !< comparison threshold

! first take the identity
 itmp = 0
 j=1
 itmp(j,1:3)=ind(1:3)
 h=float(ind(1))
 k=float(ind(2))
 l=float(ind(3))

! multiply with all point group elements
 do i=2,cell%SG%SYM_NUMpt 
  if (space.eq.'d') then
   ih=cell%SG%SYM_direc(i,1,1)*h+cell%SG%SYM_direc(i,1,2)*k+cell%SG%SYM_direc(i,1,3)*l
   ik=cell%SG%SYM_direc(i,2,1)*h+cell%SG%SYM_direc(i,2,2)*k+cell%SG%SYM_direc(i,2,3)*l
   il=cell%SG%SYM_direc(i,3,1)*h+cell%SG%SYM_direc(i,3,2)*k+cell%SG%SYM_direc(i,3,3)*l
  else
   ih=cell%SG%SYM_recip(i,1,1)*h+cell%SG%SYM_recip(i,1,2)*k+cell%SG%SYM_recip(i,1,3)*l
   ik=cell%SG%SYM_recip(i,2,1)*h+cell%SG%SYM_recip(i,2,2)*k+cell%SG%SYM_recip(i,2,3)*l
   il=cell%SG%SYM_recip(i,3,1)*h+cell%SG%SYM_recip(i,3,2)*k+cell%SG%SYM_recip(i,3,3)*l
  end if

! is this a new point ?
  newpoint=.TRUE.
  do m=1,j+1
   idiff=(itmp(m,1)-ih)**2+(itmp(m,2)-ik)**2+(itmp(m,3)-il)**2
   if (idiff.lt.eps) newpoint=.FALSE.
  end do

  if (newpoint) then 
   j=j+1
   itmp(j,1)=nint(ih)
   itmp(j,2)=nint(ik)
   itmp(j,3)=nint(il)
  endif

 end do 
 num=j

end subroutine CalcFamily

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcOrbit
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the orbit of a point
!
!> @details compute the orbit of a point and outputs it in an array
!
!> @param cell unit cell pointer
!> @param m input 
!> @param n output number of orbit members generated
!> @param ctmp output coordinate array
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable; replaced itmp by argument
!> @date  06/18/18 MDG 4.2 added code to intercept rare rounding problem (happened with alpha-quartz)
!--------------------------------------------------------------------------
recursive subroutine CalcOrbit(cell,m,n,ctmp)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcOrbit

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=dbl),INTENT(OUT)              :: ctmp(192,3)          !< output array with orbit coordinates
integer(kind=irg),INTENT(OUT)           :: n                    !< number of equivalent entries 
integer(kind=irg),INTENT(IN)            :: m                    !< index of input atom in asymmetric unit array cell%ATOM_pos

real(kind=dbl)                          :: r(3),s(3),diff       !< auxiliary variables
real(kind=dbl), parameter               :: eps = 1.0D-4         !< comparison threshold
real(kind=dbl), parameter               :: eps2= 1.0D-6         !< comparison threshold
integer(kind=irg)                       :: i,j,k,mm             !< auxiliary variables
logical                                 :: new                  !< is this a new point ?

! get the atom coordinates
! and store them in the temporary array
 n=1
 do i=1,3
  r(i)=cell%ATOM_pos(m,i)
  ctmp(n,i)=r(i)
 end do
 
! get all the equivalent atom positions
 do i=2,cell%SG%SYM_MATnum
  do j=1,3
   s(j)=cell%SG%SYM_data(i,j,4)
   do k=1,3
    s(j)=s(j)+cell%SG%SYM_data(i,j,k)*r(k)
   end do
  end do

! sometimes the code below produces an incorrect answer when one of the fractional coordinates
! is slightly negative... we intercept such issues here ... 
  do j=1,3
    if (abs(s(j)).lt.eps2) s(j) = 0.D0
  end do

! reduce to the fundamental unit cell if necessary
  if (cell%SG%SYM_reduce.eqv..TRUE.) then
   do j=1,3
    s(j) = mod(s(j)+100.0_dbl,1.0_dbl)
   end do
  end if

  do j=1,3
    if (abs(s(j)).lt.eps2) s(j) = 0.D0
  end do

! is this a new point ?
  new = .TRUE.
  do mm=1,n
   diff=0.0_dbl
   do j=1,3
    diff=diff+abs(ctmp(mm,j)-s(j))
   end do
   if (diff.lt.eps) then
     new = .FALSE.
   end if
  end do 

! yes, it is a new point
  if (new.eqv..TRUE.) then
   n=n+1
   do j=1,3
    ctmp(n,j)=s(j)
   end do
  end if

 end do

end subroutine CalcOrbit

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcStar
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the star of a vector
!
!> @details compute the star of a given reciprocal vector and outputs it in an array
!
!> @todo Since a star and an orbit are basically the same, but in different spaces,
!> it might be a good idea to merge CalcStar with CalcOrbit.
!
!> @param cell unit cell pointer
!> @param kk input vector 
!> @param n output number of orbit members generated
!> @param stmp output coordinate array
!> @param space space in which to carry out the computation
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable; replaced itmp by argument
!--------------------------------------------------------------------------
recursive subroutine CalcStar(cell,kk,n,stmp,space)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcStar

IMPLICIT NONE

type(unitcell)                          :: cell
real(kind=dbl),INTENT(IN)               :: kk(3)                !< input vector
integer(kind=irg),INTENT(OUT)           :: n                    !< number of entries in equivalent vector array
real(kind=dbl),INTENT(OUT)              :: stmp(48,3)           !< output array with equivalent vectors
character(1),INTENT(IN)                 :: space                !< 'd' or 'r'

integer(kind=irg)                       :: i,j,k,mm             !< various loop counters and such
real(kind=dbl)                          :: r(3),s(3),diff       !< auxiliary variables
real(kind=dbl),parameter                :: eps=1.0D-4           !< comparison threshold
logical                                 :: new                  !< logical (is this a new one?)

 n=1
 r=kk
 stmp(n,1:3)=r(1:3)

! get all the equivalent reciprocal/direct space vectors
 do i=2,cell%SG%SYM_NUMpt 
  do j=1,3
   s(j)=0.0_dbl
   do k=1,3
    if (space.eq.'r') then 
     s(j)=s(j)+cell%SG%SYM_recip(i,j,k)*r(k)
    else
     s(j)=s(j)+cell%SG%SYM_direc(i,j,k)*r(k)
    end if
   end do
  end do

! is this a new point ?
  new = .TRUE.
  do mm=1,n
   diff=0.0_dbl
   do j=1,3
    diff=diff+abs(stmp(mm,j)-s(j))
   end do
   if (diff.le.eps) then
     new  = .FALSE.
   endif
  end do

! yes, it is a new point
  if (new.eqv..TRUE.) then
   n=n+1
   stmp(n,1:3)=s(1:3)
  end if

 end do

end subroutine CalcStar

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcPositions
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute atom positions in one or more unit cells
!
!> @details Compute all atom positions in the fundamental unit
!> cell and translate to neighbouring cells if needed
!> (used for structure drawings and structure factor computations)
!!
!> @param cell unit cell pointer
!> @param switch input vector 
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable; replaced itmp by argument
!--------------------------------------------------------------------------
recursive subroutine CalcPositions(cell,switch)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcPositions

use io
use error
use crystal

IMPLICIT NONE

type(unitcell)                  :: cell
character(1),INTENT(IN)         :: switch                       !< if switch='m', then multiple unit cells, otherwise single cell

logical                         :: inside                       !< auxiliary logical
integer(kind=irg)               :: i,j,k,l,mm,icnt,celln(3),ncells,n,kk,ier, io_int(3)  !< various auxiliary variables
real(kind=dbl)                  :: ctmp(192,3),ff(3),sh(3)      !< auxiliary variables  
real(kind=sgl)                  :: r(3),g(3)                    !< auxiliary variables	

! make sure all coordinates are reduced to the fundamental unit cell
 cell%SG%SYM_reduce=.TRUE.

! multiple cells ?
 if (switch.eq.'m') then 
  call ReadValue('Number of unit cells in a, b and c direction ?: ', io_int,3)
  do j=1,3
   celln(j) = io_int(j)
   sh(j) = 0.5_dbl*celln(j)+1.0_dbl
  end do
  ncells = celln(1)*celln(2)*celln(3)
 else
! no, just one cell
  do j=1,3
   celln(j)=0
   sh(j)=1.0_dbl
  end do
  ncells = 1
 end if

! main loop
! first allocate the apos variable (contains CARTESIAN coordinates
! if switch is 'm', crystal coordinates otherwise)
 if (allocated(cell%apos)) deallocate(cell%apos)
 allocate (cell%apos(cell%ATOM_ntype, ncells * cell%SG%SYM_MATnum, 3),stat=ier)
 if (ier.ne.0) call FatalError('CalcPositions',' unable to allocate memory for array cell%apos')

 do i=1,cell%ATOM_ntype

! for each atom in the asymmetric unit
  call CalcOrbit(cell,i,n,ctmp)
  cell%numat(i)=n
  icnt=1

! replicate in all cells
  do j=1,celln(1)+1
   ff(1)=dble(j)
   do k=1,celln(2)+1
    ff(2)=dble(k)
    do l=1,celln(3)+1
     ff(3)=dble(l)
     do kk=1,cell%numat(i)
      do mm=1,3
       r(mm)=ctmp(kk,mm)+ff(mm)-sh(mm)
      end do 
      if (switch.eq.'m') then
! make sure the atom is actually inside the block of unit
! cells, or on one of the edges/faces/corners
       inside=.TRUE.
       do mm=1,3
        if ((r(mm)+sh(mm)).gt.(celln(mm)+1.0)) inside=.FALSE.
       end do
       if (inside) then
        call TransSpace(cell,r,g,'d','c')
        do mm=1,3
         cell%apos(i,icnt,mm)=g(mm)
        end do
        icnt=icnt+1
       end if
      else ! switch

! prepare for structure factor computation
       do mm=1,3
        cell%apos(i,icnt,mm)=r(mm)
       end do
       icnt=icnt+1
      end if  ! switch
     end do ! kk
    end do ! l 
   end do ! k
  end do ! j
  cell%numat(i)=icnt-1
 end do  ! cell%ATOM_type

end subroutine CalcPositions

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSetting
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  space group first or second setting
!
!> @details for the space groups with a second origin setting
!> this routine asks which of the two settings to use
!
!> @param cell unit cell pointer
!> @param iset  output value (1 or 2) 
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable
!> @date  08/14/15 MDG 4.2 minor change in handling of iset for space group with only one setting
!--------------------------------------------------------------------------
recursive subroutine GetSetting(cell, iset)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSetting

use io

IMPLICIT NONE

type(unitcell)                  :: cell
integer(kind=irg),INTENT(OUT)   :: iset                         !< output setting

integer(kind=irg)               :: i,isg, io_int(1)             !< auxiliary variables

! There are 24 space groups with two origin choices.
! The symmetry of both sites is stored in the array
! sitesym; the space group numbers are stored
! in tworig

!> numbers of the space groups with two settings
integer(kind=irg),parameter             :: tworig(24)=(/48,50,59,68,70,85,86,88,125,126,129,130,133,134,137,138,&
                                         141,142,201,203,222,224,227,228/)

!> site symmetry list
character(7),parameter          :: sitesym(48) = (/ '222    ',' -1    ','222/n  ',' -1    ','mm2/n  ',' -1    ', &
                                                    '222    ',' -1    ','222    ',' -1    ','-4     ',' -1    ', &
                                                    '-4     ',' -1    ','-4     ',' -1    ','422    ','2/m    ', &
                                                    '422/n  ',' -1    ','-4m2   ','2/m    ','-4/ncn ',' -1    ', &
                                                    '-4121/c',' -1    ','-42m   ','2/m    ','-4m2/n ',' -1    ', &
                                                    '-4cg   ','2/m    ','-4m2   ','2/m    ','-4c21  ',' -1    ', &
                                                    '23     ',' -3    ','23     ',' -3    ','432    ',' -3    ', &
                                                    '-43m   ','-3m    ','-43m   ','-3m    ','23     ',' -3    '/)

 isg = 0
 do i=1,24
  if (tworig(i).eq.cell%SYM_SGnum) isg=i
 end do
 
 if (isg.ne.0) then 
  iset = 0
  do while (iset.eq.0)
    call Message(' ---------------------------------------------', frm = "(A)")
    call Message(' This space group has two origin settings.', frm = "(A)")
    call Message(' The first setting has site symmetry    : '//sitesym(2*isg-1), frm = "(A)")
    call Message(' the second setting has site symmetry   : '//sitesym(2*isg), frm = "(A)")
    call ReadValue(' Which setting do you wish to use (1/2) : ', io_int, 1)
    call Message('---------------------------------------------', frm = "(A)")
    if ((io_int(1).eq.1).or.(io_int(1).eq.2)) then 
      iset = io_int(1)
    else
      call Message(' Value entered must be 1 or 2 !', frm = "(A)")
    end if
  end do
 else 
  iset = 1   ! setting for space group with only one origin setting...
 end if
 
end subroutine GetSetting

!--------------------------------------------------------------------------
!
! SUBROUTINE: ListPointGroups
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief list the crystallographic point groups in tabular form
!
!> @date  01/14/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ListPointGroups
!DEC$ ATTRIBUTES DLLEXPORT :: ListPointGroups

use io 
use typedefs

IMPLICIT NONE

integer(kind=irg)         :: i, j 

call Message('Crystallographic Point Groups')
call Message('-----------------------------')

do i=1,32
 if (mod(i,8).eq.0) then
  write (6,"(1x,i3,':',A5,5x)") i,PGTHD(i)
 else
  write (6,"(1x,i3,':',A5,5x)",advance="no") i,PGTHD(i)
 end if
end do

end subroutine ListPointGroups

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSpaceGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief asks the user for a space group number
!
!> @details This routines lists all the relevant space groups for 
!> the present crystal system, andasks the user to piok one.
!>
!> the following space groups have a hexagonal and rhombohedral
!> setting;  the generator matrices are different, and so are
!> their entries in the SYM_GL array.\n
!>  hexagonal setting 146: R 3           231\n
!>  hexagonal setting 148: R -3          232\n
!>  hexagonal setting 155: R 3 2         233\n
!>  hexagonal setting 160: R 3 m         234\n
!>  hexagonal setting 161: R 3 c         235\n
!>  hexagonal setting 166: R -3 m        23\n
!> hexagonal setting 167: R -3 c        237\n
!
!> @todo  the space group listing portion of this code uses simple write() 
!> statements, which in principle should only be used in the io module; so,
!> we may need to move this entire routine to the io.f90 file to be consistent.
!
!> @param cell unit cell pointer
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 made cell an argument instead of global variable
!--------------------------------------------------------------------------
recursive subroutine GetSpaceGroup(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSpaceGroup

use io

IMPLICIT NONE

type(unitcell)          :: cell

integer(kind=irg)       :: sgmin,sgmax,i,j,TRIG(7), io_int(1)           !< auxiliary variables  
logical                 :: skip                                         !< logical variable

 TRIG = (/ 146,148,155,160,161,166,167 /)
 skip = .FALSE.

 select case (cell%xtal_system)
 case (1); sgmin = 195; sgmax = 230
 case (2); sgmin =  75; sgmax = 142
 case (3); sgmin =  16; sgmax =  74
 case (4); sgmin = 168; sgmax = 194
 case (5); if (cell%SG%SYM_second) then
             call Message('The space groups below correspond to the ', frm = "(/A)")
             call Message('second (rhombohedral) setting.', frm = "(A/)")
             call Message('Please select one of these space groups.', frm = "(A/)")
             do i=1,7
              if ((mod(i,4).eq.0).or.(i.eq.7)) then
                write (6,"(1x,i3,':',A11,5x)") TRIG(i),SYM_SGname(TRIG(i))
              else
                write (6,"(1x,i3,':',A11,5x)",advance="no") TRIG(i),SYM_SGname(TRIG(i))
              end if
             end do 
             call Message(' -------------------------- ', frm = "(A)")
             call ReadValue(' Enter space group number : ', io_int, 1)
             cell%SYM_SGnum = io_int(1)

! check for rhombohedral settings of rhombohedral space groups
             if (cell%SG%SYM_second) then
               if (cell%SYM_SGnum.eq.146) cell%SYM_SGnum=231
               if (cell%SYM_SGnum.eq.148) cell%SYM_SGnum=232
               if (cell%SYM_SGnum.eq.155) cell%SYM_SGnum=233
               if (cell%SYM_SGnum.eq.160) cell%SYM_SGnum=234
               if (cell%SYM_SGnum.eq.161) cell%SYM_SGnum=235
               if (cell%SYM_SGnum.eq.166) cell%SYM_SGnum=236
               if (cell%SYM_SGnum.eq.167) cell%SYM_SGnum=237
             endif
             skip = .TRUE.
           else 
            sgmin = 143
            sgmax = 167
           end if
 case (6); sgmin =   3; sgmax =  15
 case (7); sgmin =   1; sgmax =   2
 end select

! print out all the relevant space group names and numbers        
 if (skip.eqv..FALSE.) then
  call Message(' ', frm = "(/A/)")
  do i=sgmin,sgmax
   j=i-sgmin+1
   if ((mod(j,4).eq.0).or.(i.eq.sgmax)) then
    write (6,"(1x,i3,':',A11,5x)") i,SYM_SGname(i)
   else
    write (6,"(1x,i3,':',A11,5x)",advance="no") i,SYM_SGname(i)
   end if
  end do
  cell%SYM_SGnum = sgmin-1
  do while ((cell%SYM_SGnum.lt.sgmin).or.(cell%SYM_SGnum.gt.sgmax)) 
   call Message(' -------------------------- ', frm = "(A)")
   call ReadValue(' Enter space group number : ', io_int, 1)
   cell%SYM_SGnum = io_int(1)
   if ((cell%SYM_SGnum.lt.sgmin).or.(cell%SYM_SGnum.gt.sgmax)) then
    call Message('Error in space group number ', frm = "(A)")
    call Message('Crystal system / space group mismatch ', frm = "(A)")
   end if
  end do
 end if 

end subroutine GetSpaceGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetOrder
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  determine order of a subfamily
!
!> @details determine the order of the subfamily of a reciprocal 
!> lattice family belonging to a zone
!
!> @param k input vector (zone axis) 
!> @param il  output list of indices that belong to the zone
!> @param num number of values to test
!> @param jcnt number of distinct entries in the output list
!> @param itmp coordinate array
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!--------------------------------------------------------------------------
recursive subroutine GetOrder(k,il,num,jcnt,itmp)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOrder

IMPLICIT NONE

real(kind=sgl),INTENT(IN)               :: k(3)         !< input vector (zone axis)
integer(kind=irg),INTENT(OUT)           :: il(48)       !< output index list
integer(kind=irg),INTENT(IN)            :: num          !< number of  entries to test
integer(kind=irg),INTENT(OUT)           :: jcnt         !< number of entries in output
integer(kind=irg),INTENT(IN)            :: itmp(48,3)   !< array used for family computations etc

integer(kind=irg)                       :: i            !< loop counter
real(kind=sgl)                          :: gn(3)        !< auxiliary variable
real(kind=sgl),parameter                :: eps=1.0E-5   !< threshold value

 jcnt = 0
 do i=1,num
  gn(1:3) = float(itmp(i,1:3))
  if (abs(sum(gn*k)).lt.eps) then
    jcnt = jcnt+1
    il(jcnt) = i
  end if
 end do

end subroutine GetOrder

!--------------------------------------------------------------------------
!
! SUBROUTINE: ShortestG
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  determine shortest vector pair for a given zone
!
!> @details determine the pair of shortest reciprocal lattice 
!> vectors for a given zone axis; used to draw diffraction 
!> patterns and in a bunch of other routines
!>
!> we look for 4 integer numbers which transform ga and gb
!> simultaneously into two shortest possible vectors of the same zone;
!> a range of -10:10 should be sufficient as a search space
!> 
!> If g1 and g2 are those shortest vectors, then we have
!> 
!>    ga  =  na*g1 + ma*g2\n
!>    gb  =  nb*g1 + mb*g2\n
!> 
!> Inversion of this relation gives
!> 
!>    g1  =  (mb*ga - ma*gb)/D\n
!>    g2  =  (-nb*ga + na*gb)/D\n
!> 
!> with D = na*mb - nb*ma.
!> 
!> The procedure below searches for the combination of 
!> (ma,mb,na,nb) which simultaneously minimizes the 
!> length of g1 and g2, and makes sure that both g1 and g2
!> are integer linear combinations of the reciprocal basis
!> vectors.
!
!> @param cell unit cell pointer
!> @param k input vector (zone axis) 
!> @param gone  output indices of first vector
!> @param gtwo output indices of second vector
!> @param isym used to resolve some potential ambiguities for 3-fold and 6-fold symmetries
!
!> @todo It may be possible to do this in a different (easier) way; it is a 
!> bit clumsy right now...
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 added unit cell pointer as argument
!--------------------------------------------------------------------------
recursive subroutine ShortestG(cell,k,gone,gtwo,isym)
!DEC$ ATTRIBUTES DLLEXPORT :: ShortestG

use error
use crystal
use constants

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(INOUT) :: isym                         !< used to resolve some potential ambiguities for 3-fold and 6-fold symmetries
!f2py intent(in,out) ::  isym                         !< used to resolve some potential ambiguities for 3-fold and 6-fold symmetries
integer(kind=irg),INTENT(IN)            :: k(3)                 !< input zone axis indices
integer(kind=irg),INTENT(OUT)           :: gone(3)              !< output first vector
integer(kind=irg),INTENT(OUT)           :: gtwo(3)              !< output second vector

integer(kind=irg)                       :: ga(3),gb(3),nzero(3),u,v,w,snz,ml(4),igsave(3)       !< auxiliary variables
integer(kind=irg)                       :: ima,imb,ina,inb,el(6),denom,minsum,inm,il(48),jcnt,num !< auxiliary variables
real(kind=sgl)                          :: fel(6),fit,gsave(3)  !< auxiliary variables
integer(kind=irg),allocatable           :: ifit(:,:,:,:)        !< array used to search
integer(kind=irg)                       :: itmp(48,3)           !< array used for family computations etc

 u = k(1)
 v = k(2)
 w = k(3)

! determine two arbitrary vectors normal to k 
! first count the zeroes in k
 nzero = (/0,0,0/)
 where (k.eq.0) nzero = 1
 snz = sum(nzero)
 
 if (snz.eq.0) then  ! make sure ga x gb is parallel to k
   ga = (/v,-u,0/)
   gb = (/w,0,-u/)
 else
  select case (snz)
  case(1);  ga = nzero; gb = 1 - nzero
            if (nzero(1).eq.1) gb = gb * (/0,w,-v/)
            if (nzero(2).eq.1) gb = gb * (/w,0,-u/)
            if (nzero(3).eq.1) gb = gb * (/v,-u,0/)
  case(2);  if ((nzero(1).eq.1).and.(nzero(2).eq.1)) then
             ga = (/1,0,0/); gb = (/0,1,0/)
            endif
            if ((nzero(1).eq.1).and.(nzero(3).eq.1)) then
             ga = (/0,0,1/); gb = (/1,0,0/)
            endif
            if ((nzero(2).eq.1).and.(nzero(3).eq.1)) then
             ga = (/0,1,0/); gb = (/0,0,1/)
            endif
  case(3); call FatalError('ShortestG',' beam direction cannot be [0,0,0]')
  end select
 end if 

! check linear combinations to see if there are any shorter ones
 inm = 10
 allocate(ifit(-inm:inm,-inm:inm,-inm:inm,-inm:inm))
 do ima=-inm,inm
  do imb=-inm,inm
   do ina=-inm,inm
    do inb=-inm,inm
     el(1) = imb*ga(1)-ima*gb(1) 
     el(2) = imb*ga(2)-ima*gb(2) 
     el(3) = imb*ga(3)-ima*gb(3) 
     el(4) = ina*gb(1)-inb*ga(1) 
     el(5) = ina*gb(2)-inb*ga(2) 
     el(6) = ina*gb(3)-inb*ga(3) 
     denom = ina*imb-inb*ima
     ifit(ima,imb,ina,inb)=100
     if (denom.ne.0) then
      fel = float(el)/float(denom)
      fit = sum(abs(float(int(fel))-fel))
! here is where we only keep the integer combinations
      if (fit.eq.0.0) then
        gone(1:3) = int(fel(1:3))
        gtwo(1:3) = int(fel(4:6))
! keep the sum of the squares of the lengths 
       ifit(ima,imb,ina,inb)=sum(gone**2)+sum(gtwo**2) 
      end if
     end if
    end do
   end do
  end do
 end do
 minsum = 50

! look for the minimum of ifit with the smallest and most
! positive coefficients; store them in ml
! [minloc does not work here because there may be multiple minima]
 do ima=-inm,inm
  do imb=-inm,inm
   do ina=-inm,inm
    do inb=-inm,inm
     if (ifit(ima,imb,ina,inb).le.minsum) then
      minsum = ifit(ima,imb,ina,inb)
      ml(1) = ima
      ml(2) = imb
      ml(3) = ina
      ml(4) = inb
     end if
    end do
   end do
  end do
 end do
 deallocate(ifit)

! transform ga and gb into g1 and g2 
 gone = (ml(2)*ga-ml(1)*gb)/(ml(3)*ml(2)-ml(4)*ml(1))
 gtwo = (ml(3)*gb-ml(4)*ga)/(ml(3)*ml(2)-ml(4)*ml(1))

! next rank these two vectors so that their cross product is along +k
 call CalcCross(cell,float(gone),float(gtwo),gsave,'r','r',0)
 fit = CalcDot(cell,gsave,float(k),'r')
 if (fit.lt.0.0) then
  igsave = gone
  gone = gtwo
  gtwo = igsave
 end if

! finally, if isym.ne.0 make sure that the selection of the 
! basis vectors for the 3-fold and 6-fold 2D point groups is
! correctly done.
!
! For isym < 7:   90 degrees between vectors (should not be a problem)
! For isym = 7:  120 degrees between vectors (should be ok too)
! distinguish between 3 coming from a cubic group
! vs. the same symmetries originating from a hexagonal setting
 if ((isym.eq.7).and.(cell%gamma.eq.120.0)) then
   isym=11
   fit = CalcAngle(cell,float(gone),float(gtwo),'r')*180.0/cPi 
   if (abs(fit-120.0).lt.1.0) then
     gtwo=gone+gtwo
   end if
 end if

! For isym = 8:  here we should distinguish between the settings 3m1 and 31m !!!
!                The angle should always be 120 degrees, so we must check that
!                this is the case for the selected gone and gtwo.
 if ((isym.eq.8).and.(cell%gamma.eq.120.0)) then
   isym=12
   fit = CalcAngle(cell,float(gone),float(gtwo),'r')*180.0/cPi 
   if (abs(fit-120.0).lt.1.0) then
     gtwo=gone+gtwo
   end if
 end if
!
 if (isym.eq.8) then
   fit = CalcAngle(cell,float(gone),float(gtwo),'r')*180.0/cPi 
   if (abs(fit-120.0).gt.1.0) then
     gtwo=gtwo-gone
   end if

! we assume it is the 31m setting;  if the order of gone is 6, then that is not true
   call CalcFamily(cell,gone,num,'r',itmp)
   call GetOrder(float(k),il,num,jcnt,itmp)
   if (jcnt.eq.6) then  ! it is the 3m1 setting
     isym = 13
   end if
 end if

! it could the 3m1 setting for the 3m hexagonal case
 if (isym.eq.12) then
! we assume it is the 31m setting;  if the order of gone is 6, then that is not true
   call CalcFamily(cell,gone,num,'r',itmp)
   call GetOrder(float(k),il,num,jcnt,itmp)
   if (jcnt.eq.6) then  ! it is the 3m1 setting
     isym = 14
   end if
 end if

! For isym = 9 or 10:   60 degrees between vectors (may not be the case)
 if ((isym.eq.9).or.(isym.eq.10)) then
   fit = CalcAngle(cell,float(gone),float(gtwo),'r')*180.0/cPi 
   if (abs(fit-120.0).lt.1.0) then
     gtwo=gone+gtwo
   end if
 end if

end subroutine ShortestG

!--------------------------------------------------------------------------
!
! FUNCTION: IsGAllowed
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  is a reflection allowed?
!
!> @details determine whether or not a given reflection is absent due to
!> lattice centering operations.
!
!> @param cell unit cell pointer
!> @param g input vector  
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 added unit cell pointer as argument
!--------------------------------------------------------------------------
recursive logical function IsGAllowed(cell,g)
!DEC$ ATTRIBUTES DLLEXPORT :: IsGAllowed

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(IN)            :: g(3)         !< input reciprocal lattice vector

integer(kind=irg)                       :: seo          !< auxiliary variable
character(1)                            :: lc           !< first letter of space group name

! Determine whether or not this vector is
! actually allowed by the lattice centering
 lc(1:1) =  cell%SG%SYM_name(2:2)
 IsGAllowed = .TRUE.
 select case (lc)
  case ('P'); ! all reflections allowed for a primitive lattice
  case ('F'); seo = sum(mod(g+100,2)); if ((seo.eq.1).or.(seo.eq.2)) IsGAllowed = .FALSE.
  case ('I'); seo = mod(sum(g)+100,2); if (seo.eq.1) IsGAllowed = .FALSE.
  case ('A'); seo = mod(g(2)+g(3)+100,2); if (seo.eq.1) IsGAllowed = .FALSE.
  case ('B'); seo = mod(g(1)+g(3)+100,2); if (seo.eq.1) IsGAllowed = .FALSE.
  case ('C'); seo = mod(g(1)+g(2)+100,2); if (seo.eq.1) IsGAllowed = .FALSE.
  case ('R'); if (cell%hexset) then
               seo = mod(-g(1)+g(2)+g(3)+90,3); if (seo.ne.0) IsGAllowed = .FALSE.
              endif ! otherwise reflections are all allowed
 end select
 
end function IsGAllowed

!--------------------------------------------------------------------------
!
! SUBROUTINE: BFsymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  What is the bright field symmetry?
!
!> @details routine to determine the symmetry of a bright field bend center [uses Table 4 in BESR paper]
!>
!> we must intercept the special cases where more than one symmetry of the same order can
!> occur in a given Laue group;  e.g.  Laue group 2/m has two bright field symmetries
!> of order 2:  2 and m
!> 
!> This happens for the following Laue groups:
!>   2/m     [010] -> 2    [u0w] -> m
!>   -3m     [11.0] -> 2   [u-u.w] -> m
!> 
!> The normal conversion from the reduced order ir to the actual Bright Field
!> symmetry uses the PGTWDinverse array to determine the 2D symmetry
!
!> @param cell unit cell pointer
!> @param uvw input vector (zone axis)
!> @param j index into inverse Laue group list
!> @param isym keeps track of special cases     
!> @param ir  index of point group
!
!> @date  10/13/98 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/21/13 MDG 3.0 clean up and updated IO
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 added unit cell pointer as argument
!--------------------------------------------------------------------------
recursive subroutine BFsymmetry(cell,uvw,j,isym,ir)
!DEC$ ATTRIBUTES DLLEXPORT :: BFsymmetry

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(IN)            :: uvw(3)       !< zone axis indices
integer(kind=irg),INTENT(IN)            :: j            !< index into Laue group list
integer(kind=irg),INTENT(OUT)           :: isym         !< keeps track of special cases
integer(kind=irg),INTENT(OUT)           :: ir           !< index of point group

integer(kind=irg)                       :: orderPG, Lauenum, ng         !< auxiliary variables
real(kind=dbl)                          :: kstar(48,3)                          !< star variable


 orderPG = cell%SG%SYM_NUMpt
 Lauenum = PGLaueinv(j)
 call CalcStar(cell,dble(uvw),ng,kstar,'d')
 ir = orderPG/ng

! take care of special cases
 isym = PGTWDinverse(ir,Lauenum)
 if ((Lauenum.eq.2).and.(ir.eq.2)) then   ! this deals with Laue Group 2/m
  if (uvw(2).eq.0) isym=3
 end if
 if ((Lauenum.eq.7).and.(ir.eq.2)) then   ! and this covers -3m
  if (uvw(1).eq.-uvw(2)) isym=3
 end if

end subroutine BFsymmetry

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetPatternSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Determine the diffraction group number and optionally produce output
!
!> @param cell unit cell pointer
!> @param uvw input vector (zone axis)
!> @param pgnum point group number
!> @param verbose (optional) produces output if present
!
!> @date  10/01/13 MDG 1.0 original
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 added unit cell pointer as argument
!--------------------------------------------------------------------------
recursive function GetPatternSymmetry(cell,uvw,pgnum,verbose) result(dgn)
!DEC$ ATTRIBUTES DLLEXPORT :: GetPatternSymmetry

use io

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(IN)            :: uvw(3)       !< zone axis indices
integer(kind=irg),INTENT(IN)            :: pgnum        !< point group number
logical,INTENT(IN),OPTIONAL             :: verbose      !< print output or not

integer(kind=irg)                       :: dgn          !< output diffraction group number
integer(kind=irg)                       :: io_int(3)

! get the diffraction group number
 dgn = GetDiffractionGroup(cell,uvw,pgnum)

! and print some general information if needed.
 if (present(verbose)) then
   call Message(' ',"(A)")
   io_int(1:3) = uvw(1:3)
   call WriteValue(' Zone Axis : ',io_int,3,"('[',I3,I3,I3,']')")
   io_int(1) = pgnum
   call Message(' Crystal point group           : '//PGTHD(pgnum), frm = "(/A)")
   call WriteValue(' Crystal point group number    : ', io_int, 1, "(I3)")
   call Message(' Laue group                    : '// PGTHD(PGLaue(pgnum)), frm = "(A)")
   io_int(1) = dgn
   call Message(' Diffraction group             : '//DG(dgn), frm = "(A)")
   call WriteValue(' Diffraction group number      : ', io_int, 1, "(I3)")
   call Message(' Projection diffraction group  : '// DG(PDG(dgn)), frm = "(A/)")

   call Message(' Bright Field symmetry         : '//PGTWD(BFPG(dgn)), frm = "(A)")
   call Message(' Whole Pattern symmetry        : '//PGTWD(WPPG(dgn)), frm = "(A)")
   call Message(' Dark Field general symmetry   : '//PGTWD(DFGN(dgn)), frm = "(A)")
   call Message(' Dark Field special symmetry   : '//PGTWD(DFSP(dgn)), frm = "(A/)")
 end if

end function GetPatternSymmetry

!--------------------------------------------------------------------------
!
! FUNCTION: GetDiffractionGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief get the diffraction group number for this zone axis orientation
!
!> @details This implements Table 4 in BESR76 or Table 7.2 in the EM book
!> This is a bit tricky and we want to do this as efficiently as possible...
!> In this first version, let's compute the order of the family of directions
!> and based on that and the PG number determine what the diffraction group is.
!> The data in this routine was painstakingly entered after staring at both the 
!> BESR table and the International Tables for Crystallography, point group section,
!> Table 10.2.2.
!
!> @param cell unit cell pointer
!> @param uvw zone axis direction 
!> @param pgn point group number
!
!> @todo verify that for point groups with multiple settings, things are still correct.
!> this would be for the following point groups  -6m2, -3m, 3m, 32, -42m, and the unique
!> axis settings for the monoclinic groups.
!
!> @date  10/01/13 MDG 1.0 original
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 added unit cell pointer as argument
!--------------------------------------------------------------------------
recursive function GetDiffractionGroup(cell,uvw,pgn) result(dgn)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDiffractionGroup

use io

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(IN)            :: uvw(3)                       !< zone axis indices
integer(kind=irg),INTENT(IN)            :: pgn                          !< point group number

real(kind=dbl)                          :: kstar(48,3)                  !< star variable
integer(kind=irg)                       :: ng                           !< number of members in star
integer(kind=irg)                       :: auvw(3), mina, nz, i, s      !< abs(uvw), min(auvw), number of zeroes
integer(kind=irg)                       :: dgn                          !< (output) diffraction group number
logical                                 :: found

! compute the order of the family of directions (in direct space)
 call CalcStar(cell,dble(uvw),ng,kstar,'d')

! determine some parameters that might be useful in deciding the correct diffraction group symmetry 
 auvw = iabs(uvw)
 mina = minval(auvw)
 nz = 0   ! how many zeroes are there in the index symbol ?
 do i=1,3 
   if (uvw(i).eq.0) nz = nz+1
 end do

! very long case statement to cover each of the 32 point groups
! we'll put them in reverse order since structures in materials
! science are more likely to have a higher symmetry...
!
! These statements were carefully entered, but it is certainly possible that
! there are errors remaining... Please report any potential errors to the author.
select case (pgn)
 case (32) ! m -3 m
 	select case (ng)
 		case (6)
 			dgn = 19 ! 4mm1R
 		case (8)
 			dgn = 24 ! 6RmmR
 		case (12)
 			dgn = 12 ! 2mm1R
 		case (24)
 			dgn = 11 ! 2RmmR
 		case (48)
 			dgn = 4  ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (31) ! -4 3 m
 	select case (ng)
 		case (4)
 			dgn = 23 ! 3m
 		case (6)
 			dgn = 18 !4RmmR
 		case (12)
 			if (mina.eq.0) then
				dgn = 8 ! m1R 
			else 
				dgn = 7 ! m
			end if 
 		case (24)
 			if (mina.eq.0) then
				dgn = 6 ! mR 
			else 
				dgn = 1 ! 1
			end if 
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (30) ! 432
 	select case (ng)
 		case (6)
 			dgn = 16 ! 4mRmR
 		case (8)
 			dgn = 22 ! 3mR 
 		case (12)
 			dgn = 9  ! 2mRmR
 		case (24)
  			if (mina.eq.0) then
  				dgn = 6 ! mR
  			else
 		  if ( (auvw(1).eq.auvw(2)).or.(auvw(1).eq.auvw(3)).or.(auvw(2).eq.auvw(3)) ) then
				dgn = 6 ! mR
			  else
				dgn = 1 ! 1
			  end if
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 !------------
 case (29) ! m3
 	select case (ng)
 		case (6)
 			dgn = 12 ! 2mm1R
 		case (8)
 			dgn = 21 ! 6R 
 		case (12)
 			dgn = 11 ! 2RmmR
 		case (24)
 			dgn = 4  ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (28) ! 23
 	select case (ng)
 		case (4)
 			dgn = 20 ! 3
 		case (6)
 			dgn = 9  ! 2mRmR 
		case (12)
  			if (mina.eq.0) then
  				dgn = 6 ! mR
  			else
 				dgn = 1 ! 1
 			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 !------------
 case (27) ! 6/mmm
 	select case (ng)
 		case (2)
 			dgn = 31 ! 6mm1R
 		case (6)
 			dgn = 12 ! 2mm1R
 		case (12)
 			dgn = 11 ! 2RmmR 
		case (24)
 			dgn = 4  ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (26) ! -6m2
 	select case (ng)
 		case (2)
 			dgn = 30 ! 3m1R
 		case (3)
 			dgn = 10 ! 2mm
 		case (6)
 			if (auvw(3).ne.0) then
				dgn = 7 ! m 
			else  ! check if [11.0] is part of kstar
				found = .FALSE.
				do i=1,ng
					s = sum( int(kstar(i,1:3)) - (/ 1, 1, 0 /) )
					if (s.eq.0) found=.TRUE.
				end do
				if (found) then 
					dgn = 8 ! m1R
				else
					dgn = 7 ! m
				end if 
			end if
		case (12)
 			found = .FALSE.
			do i=1,ng
				s = kstar(i,1)-kstar(i,2)
				if (s.eq.0) found=.TRUE.
			end do
			if (found) then
				dgn = 6  ! mR
			else
				dgn = 1  ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (25) ! 6mm
 	select case (ng)
 		case (1)
 			dgn = 29 ! 6mm
 		case (6)
 			if (auvw(3).eq.0) then  
				dgn = 8 ! m1R
			else
 				dgn = 7 ! m
			end if
 		case (12)
 			if (auvw(3).eq.0) then  
				dgn = 6 ! mR
			else
 				dgn = 1 ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (24) ! 622
 	select case (ng)
 		case (2)
 			dgn = 28 ! 6mRmR
 		case (6)
			dgn = 9  ! 2mRmR
 		case (12)   ! there are three cases that produce mR, and one that produces 1
 			found = .FALSE.
 			if (auvw(3).eq.0) found=.TRUE.
 			do i=1,ng  ! look for [hh.l]
				s = kstar(i,1)-kstar(i,2)
				if (s.eq.0) found=.TRUE.
			end do
 			do i=1,ng  ! look for [h -h.l]
				s = kstar(i,1)+kstar(i,2)
				if (s.eq.0) found=.TRUE.
			end do
 			if (found) then  
				dgn = 6 ! mR
			else
 				dgn = 1 ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 !------------
 case (23) ! 6/m
 	select case (ng)
 		case (2)
 			dgn = 27 ! 61R
 		case (6)
 			dgn = 11 ! 2RmmR 
 		case (12)
			dgn = 4  ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (22) ! -6
 	select case (ng)
 		case (2)
 			dgn = 26 ! 31R
 		case (3)
 			dgn = 7  ! m 
 		case (6)
			dgn = 1  ! 1
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (21) ! -6
 	select case (ng)
 		case (1)
 			dgn = 25 ! 6
 		case (6)
			if (auvw(3).eq.0) then
				dgn = 6  ! mR
			else
				dgn = 1  ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 !------------
 case (20) ! -3m
 	select case (ng)
 		case (2)
 			dgn = 24 ! 6RmmR
 		case (6)
			if (auvw(3).eq.0) then
				found = .FALSE.
				do i=1,ng
					s = kstar(i,1)+kstar(i,2)
					if (s.eq.0) found=.TRUE.
				end do
				if (found) then
					dgn = 11  ! 2RmmR
				else
					dgn = 5  ! 21R
				end if
			else
				dgn = 11  ! 2RmmR
			end if
  		case (12)
			dgn = 4  ! 2R
		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (19) ! -3m1
 	select case (ng)
 		case (1)
 			dgn = 23 ! 3m
 		case (3)
			if (auvw(3).eq.0) then
				dgn = 2  ! 1R
			else
				dgn = 7  ! m
			end if
  		case (6)
			dgn = 1  ! 1
		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (18) ! 321
 	select case (ng)
 		case (2)
 			dgn = 22 ! 3mR
 		case (3)
			dgn = 3  ! 2
  		case (6)
			found = .FALSE.
			do i=1,ng
				s = kstar(i,1)+kstar(i,2)
				if (s.eq.0) found=.TRUE.
			end do
			if (found) then
				dgn = 6  ! mR
			else
				dgn = 1  ! 1
			end if
		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 !------------
 case (17) ! -3
 	select case (ng)
 		case (2)
 			dgn = 21 ! 6R
 		case (6)
 			dgn = 4  ! 2R 
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (16) ! 3
 	select case (ng)
 		case (1)
 			dgn = 20 ! 3
 		case (3)
 			dgn = 1  ! 1 
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 !------------
 case (15) ! 4/mmm
 	select case (ng)
 		case (2)
 			dgn = 19 ! 4mm1R
 		case (4)
 			dgn = 12 ! 2mm1R 
 		case (8)
			dgn = 11 ! 2RmmR
 		case (16)
 			dgn = 4  ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (14) ! 4/mmm
 	select case (ng)
 		case (2)
 			dgn = 18 ! 4RmmR
 		case (4)
 			if (nz.eq.2) then
				dgn = 9 ! 2mRmR
			else
				if (nz.eq.1) then
					dgn = 8 ! m1R
				else
					dgn = 7 ! m
				end if
			end if 
 		case (8)
			if (nz.eq.1) then
				dgn = 6 ! mR
			else
				dgn = 1  ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
 	end select
 case (13) ! 4mmm
 	select case (ng)
 		case (1)
 			dgn = 17 ! 4mm
 		case (4)
 			if ( (nz.eq.2).or. ( (nz.eq.1).and.(auvw(3).eq.0))  ) then
				dgn = 8 ! m1R
			else
				dgn = 7 ! m
			end if 
 		case (8)
			if (nz.eq.1) then
				dgn = 6 ! mR
			else
				dgn = 1  ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (12) ! 422
 	select case (ng)
 		case (2)
 			dgn = 16 ! 4mRmR
 		case (4)
			dgn = 9  ! 2mRmR
 		case (8)
 			auvw = auvw - auvw(1)
			if ( (auvw(2).ne.0).and.(auvw(3).ne.0) ) then
				dgn = 1 ! 1
			else
				dgn = 6 ! mR
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
!------------
 case (11) ! 4/m
 	select case (ng)
 		case (2)
 			dgn = 15 ! 41R
 		case (4)
			dgn = 11 ! 2RmmR
 		case (8)
			dgn = 4  ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (10) ! -4
 	select case (ng)
 		case (2)
 			dgn = 14 ! 4R
 		case (4)
			if (auvw(3).eq.0) then 
				dgn = 6 ! mR
			else
				dgn = 1 ! 1
			end if				
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (9) ! 4
 	select case (ng)
 		case (1)
 			dgn = 13 ! 4
 		case (4)
			if (auvw(3).eq.0) then 
				dgn = 6 ! mR
			else
				dgn = 1 ! 1
			end if				
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
!------------
 case (8) ! mmm
 	select case (ng)
 		case (2)
 			dgn = 12 ! 2mm1R
 		case (4)
 			dgn = 11 ! 2RmmR
 		case (8)
			dgn = 4 ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (7) ! mm2
 	select case (ng)
 		case (1)
 			dgn = 10 ! 2mm
 		case (2)
 			if (nz.eq.1) then
 				dgn = 8 ! m1R
 			else
				dgn = 7 ! m
			end if
 		case (4)
 			if (auvw(3).eq.0) then
				dgn = 6 ! mR
			else
				dgn = 1 ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (6) ! 222
 	select case (ng)
 		case (2)
 			dgn = 9 ! 2mRmR
 		case (4)
 			if (nz.eq.1) then
				dgn = 6 ! mR
			else
				dgn = 1 ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
!------------
 case (5) ! 2/m
 	select case (ng)
 		case (2)
			if (sum(iabs( uvw - (/0, 1, 0/) )).eq.0) then
				dgn = 5  ! 21R
			else
				dgn = 11 ! 2RmmR
			end if
 		case (4)
 			dgn = 4 ! 2R
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (4) ! m
 	select case (ng)
 		case (1)
			dgn = 7 ! m 	
 		case (2)
			if (sum(iabs( uvw - (/0, 1, 0/) )).eq.0) then
				dgn = 2 ! 1R
			else
 				dgn = 1 ! 1
			end if
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
 case (3) ! 2
 	select case (ng)
  		case (1)
			if (sum(iabs( uvw - (/0, 1, 0/) )).eq.0) then
				dgn = 3 ! 2
			else
				dgn = 6 ! mR
			end if
 		case (2)
			dgn = 1 ! 1
 		case default
			call Message(' -> incorrect number of equivalent directions in point group '//PGTHD(pgn), frm = "(A)")
	end select
!------------
 case (2) ! -1
 	dgn = 4 ! 2R
 case (1) ! 1
 	dgn = 1 ! 1
 case default
	write (*,*) 'unknown point group'
	dgn = 0
end select

end function GetDiffractionGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE: Generate2DSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the symmetry matrices for one of the 2D planar point groups.
!
!> @details The output of this routine is stored in the TDPG structure that 
!> is defined at the end of the symmetryvars module.  Routine verified on 10/2/13.
!
!> @param TDPG output structure
!> @param pgn 2D point group number
!
!> @date  10/02/13 MDG 1.0 original
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!--------------------------------------------------------------------------
recursive subroutine Generate2DSymmetry(TDPG,pgn)
!DEC$ ATTRIBUTES DLLEXPORT :: Generate2DSymmetry

use error

IMPLICIT NONE

type (symdata2D),INTENT(INOUT)  :: TDPG
!f2py intent(in,out) ::  TDPG
integer(kind=irg),INTENT(IN)    :: pgn          !< point group number

! here we define all 8 possible 2x2 matrices (remember column major order !)
integer(kind=irg),parameter     :: mI(2,2) = reshape( (/ 1, 0, 0, 1 /), (/2,2/))        ! identity
integer(kind=irg),parameter     :: mA(2,2) = reshape( (/-1, 0, 0, 1 /), (/2,2/))        ! mirror
integer(kind=irg),parameter     :: mB(2,2) = reshape( (/ 0, 1,-1, 0 /), (/2,2/))        ! 4-fold
integer(kind=irg),parameter     :: mC(2,2) = reshape( (/ 0, 1, 1, 0 /), (/2,2/))        ! diagonal mirror
integer(kind=irg),parameter     :: mD(2,2) = reshape( (/-1, 1,-1, 0 /), (/2,2/))        ! 3-fold
integer(kind=irg),parameter     :: mE(2,2) = reshape( (/ 0,-1, 1,-1 /), (/2,2/))        ! 3-fold twice
integer(kind=irg),parameter     :: mF(2,2) = reshape( (/-1, 0,-1, 1 /), (/2,2/))        ! 6-fold
integer(kind=irg),parameter     :: mG(2,2) = reshape( (/ 1,-1, 0,-1 /), (/2,2/))        ! 6-fold twice


TDPG%SYM_pgnum = pgn

select case (pgn)
  case (1) ! 1
        TDPG%SYM_MATnum = 1
        TDPG%SYM_direc(1,1:2,1:2) = mI
!------------
  case (2) ! 2
        TDPG%SYM_MATnum = 2
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = -mI
!------------
  case (3) ! m
        TDPG%SYM_MATnum = 2
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = mA
!------------
  case (4) ! 2mm
        TDPG%SYM_MATnum = 4
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = -mI
        TDPG%SYM_direc(3,1:2,1:2) = mA
        TDPG%SYM_direc(4,1:2,1:2) = -mA
!------------
  case (5) ! 4
        TDPG%SYM_MATnum = 4
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = -mI
        TDPG%SYM_direc(3,1:2,1:2) = mB
        TDPG%SYM_direc(4,1:2,1:2) = -mB
!------------
  case (6) ! 4mm
        TDPG%SYM_MATnum = 8
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = -mI
        TDPG%SYM_direc(3,1:2,1:2) = mA
        TDPG%SYM_direc(4,1:2,1:2) = -mA
        TDPG%SYM_direc(5,1:2,1:2) = mB
        TDPG%SYM_direc(6,1:2,1:2) = -mB
        TDPG%SYM_direc(7,1:2,1:2) = mC
        TDPG%SYM_direc(8,1:2,1:2) = -mC
!------------
  case (7) ! 3
        TDPG%SYM_MATnum = 3
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = mD
        TDPG%SYM_direc(3,1:2,1:2) = mE
!------------
  case (8) ! 3m1
        TDPG%SYM_MATnum = 6
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = mD
        TDPG%SYM_direc(3,1:2,1:2) = mE
        TDPG%SYM_direc(4,1:2,1:2) = -mC
        TDPG%SYM_direc(5,1:2,1:2) = -mF
        TDPG%SYM_direc(6,1:2,1:2) = -mG
!------------
  case (9) ! 6
        TDPG%SYM_MATnum = 6
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = -mI
        TDPG%SYM_direc(3,1:2,1:2) = mD
        TDPG%SYM_direc(4,1:2,1:2) = -mD
        TDPG%SYM_direc(5,1:2,1:2) = mE
        TDPG%SYM_direc(6,1:2,1:2) = -mE
!------------
  case (10) ! 6mm
        TDPG%SYM_MATnum = 12
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = -mI
        TDPG%SYM_direc(3,1:2,1:2) = mD
        TDPG%SYM_direc(4,1:2,1:2) = -mD
        TDPG%SYM_direc(5,1:2,1:2) = mE
        TDPG%SYM_direc(6,1:2,1:2) = -mE
        TDPG%SYM_direc(7,1:2,1:2) = mF
        TDPG%SYM_direc(8,1:2,1:2) = -mF
        TDPG%SYM_direc(9,1:2,1:2) = mG
        TDPG%SYM_direc(10,1:2,1:2) = -mG
        TDPG%SYM_direc(11,1:2,1:2) = mC
        TDPG%SYM_direc(12,1:2,1:2) = -mC        
!------------
  case (11) ! 31m
        TDPG%SYM_MATnum = 6
        TDPG%SYM_direc(1,1:2,1:2) = mI
        TDPG%SYM_direc(2,1:2,1:2) = mC
        TDPG%SYM_direc(3,1:2,1:2) = mD
        TDPG%SYM_direc(4,1:2,1:2) = mE
        TDPG%SYM_direc(5,1:2,1:2) = mF
        TDPG%SYM_direc(6,1:2,1:2) = mG
!------------
  case default
        call FatalError('Generate2DSymmetry',' unknown 2D point group number')
        stop
end select


end subroutine Generate2DSymmetry


!--------------------------------------------------------------------------
!
! SUBROUTINE: CheckPatternSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief verify the relative orientation of the 2D point group with respect to the standard orientation.
!
!> @details For some 2D point groups, the orientation of the group's symmetry elements 
!> differs from the standard orientation in ITC A; for instance, for the [112] zone axis in fcc,
!> the calling program will select the (1,-1,0) vector to be the horizontal vector; the 
!> whole pattern m symmetry for this zone axis indicates the presence of a single mirror plane,
!> which, in its standard orientation, is also horizontal, whereas it should be normal to
!> (1,-1,0).  This routine tests for such cases and returns the correct relative orientation 
!> angle thetam if a correction is needed. If necessary, the 2D pint group number is also 
!> changed (for the 3m1 vs. 31m case).
!
!> @param cell unit cell pointer
!> @param k zone axis
!> @param ga horizontal g vector
!> @param isym 2D point group number for whole pattern symmetry
!> @param thetam (output) point group rotation angle, if needed
!
!> @date  10/14/13 MDG 1.0 original
!> @date  01/10/14 MDG 4.0 SG is now part of the unitcell type
!> @date  06/05/14 MDG 4.1 added unit cell pointer as argument
!--------------------------------------------------------------------------
recursive subroutine CheckPatternSymmetry(cell,k,ga,isym,thetam)
!DEC$ ATTRIBUTES DLLEXPORT :: CheckPatternSymmetry

use error
use crystal
use constants
use io

IMPLICIT NONE

type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: k(3)         !< zone axis
integer(kind=irg),INTENT(IN)            :: ga(3)        !< g-vector
integer(kind=irg),INTENT(INOUT) :: isym         !< 2D point group number
!f2py intent(in,out) ::  isym         !< 2D point group number
real(kind=sgl),INTENT(OUT)              :: thetam       !< rotation angle (degrees, CCW)

integer(kind=irg)                       :: num
real(kind=sgl)                          :: io_real(1)
integer(kind=irg)                       :: itmp(48,3)   !< array used for family computations etc

! no action is needed for the following 2D point groups: 1, 2, 2mm, 3, 4, 4mm, 6, 6mm
thetam = 0.0

! for the group m (isym=3), we need to determine the cardinality of the 
! family of ga; if equal to 1, then ga lies in the mirror plane and all
! is correct.  If the cardinality is 2, then we compute the angle between
! ga and ga' and set thetam to half of this angle.

if (isym.eq.3) then 
  call CalcFamily(cell, ga, num, 'r',itmp)
  if (num.ne.1) then
    thetam = 0.5 * CalcAngle(cell, float(ga),float(itmp(2,1:3)),'r') *180.0/cPi
  end if  
  io_real(1) = thetam
  call WriteValue('  --> Pattern symmetry m correction; point group rotation angle [deg]',io_real, 1, "(F6.3/)")
end if

! for the groups 3m1 and 31m, we need to check which one we have

if ((isym.eq.8).or.(isym.eq.11)) then 
  call CalcFamily(cell, ga, num, 'r',itmp)
  if (num.eq.3) then
    isym = 11
  else
    isym = 8
  end if  
  call Message('  --> Pattern symmetry verified to be '//PGTWD(isym), frm = "(A/)")
end if

end subroutine CheckPatternSymmetry

!--------------------------------------------------------------------------
!
! FUNCTION: getHexvsRho
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert a 3D point group number to a SamplingType for trigonal symmetry
!
!> @param cell unit cell pointer
!> @param pgnum point group number
!
!> @date  08/25/15 MDG 1.0 original
!> @date  08/26/15 MDG 1.1 added point group cases 14 and 26
!> @date  11/06/15 MDG 1.2 fixed special cases (errors for trigonal symmetry)
!> @date  07/30/18 MDG 1.3 correction of space groups 166 and 167 (after error in Calcite EBSD master)
!--------------------------------------------------------------------------
recursive function getHexvsRho(cell,pgnum) result(stnum)
!DEC$ ATTRIBUTES DLLEXPORT :: getHexvsRho

use local
use constants
use crystal

IMPLICIT NONE

type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: pgnum
integer(kind=irg)                       :: stnum, sg

sg = cell%SYM_SGnum
! Is this a trigonal group
if (cell%SG%SYM_trigonal.eqv..TRUE.) then ! yes, it is

! go through all the trigonal space groups from 143 to 167 and set the correct sampling type number stnum
! point group 3
  if ((sg.ge.143).and.(sg.le.146)) stnum = 10

! point group bar3 [corrected on 7/31/18 by MDG]
  if (sg.eq.147) stnum = 12 
  if (sg.eq.148) stnum = 12

! point group 32
  if ((sg.eq.149).or.(sg.eq.151).or.(sg.eq.153)) stnum = 12
  if ((sg.eq.150).or.(sg.eq.152).or.(sg.eq.154)) stnum = 12
  if (sg.eq.155) stnum = 12

! point group 3m
  if ((sg.eq.156).or.(sg.eq.158)) stnum = 14
  if ((sg.eq.157).or.(sg.eq.159)) stnum = 14 
  if ((sg.eq.160).or.(sg.eq.161)) stnum = 14
  
! point group bar3m
  if ((sg.eq.162).or.(sg.eq.163)) stnum = 17
  if ((sg.eq.164).or.(sg.eq.165)) stnum = 16 
  if ((sg.eq.166).or.(sg.eq.167)) stnum = 16 
else
! this must be either point group 14 or 26, each with two settings
  if (pgnum.eq.14) then
    if ((sg.ge.115).and.(sg.le.120)) then
      stnum = 6
    else
      stnum = 8
    end if
  end if
  if (pgnum.eq.26) then
    if ((sg.eq.187).or.(sg.eq.188)) then
      stnum = 16
    else
      stnum = 17
    end if
 end if
end if

end function getHexvsRho

!--------------------------------------------------------------------------
!
! FUNCTION: SYM_getmultiplicity
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert multiplicity letter into a number string
!
!> @param t single upper case character
!
!> @date  09/05/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function SYM_getmultiplicity(t) result(stmult)
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_getmultiplicity

use local
use constants
use crystal

IMPLICIT NONE

character(1),INTENT(IN)                 :: t
character(3)                            :: stmult

 select case (t(1:1))
  case ('A');  stmult = '1'
  case ('B');  stmult = '2'
  case ('C');  stmult = '3'
  case ('D');  stmult = '4'
  case ('E');  stmult = '6'
  case ('F');  stmult = '8'
  case ('G');  stmult = '9'
  case ('H');  stmult = '12'
  case ('I');  stmult = '16'
  case ('J');  stmult = '18'
  case ('K');  stmult = '24'
  case ('L');  stmult = '32'
  case ('M');  stmult = '36'
  case ('N');  stmult = '48'
  case ('O');  stmult = '64'
  case ('P');  stmult = '96'
  case ('Q');  stmult = '192'
 end select

end function SYM_getmultiplicity

!--------------------------------------------------------------------------
!
! FUNCTION: SYM_getposition
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert multiplicity letter into a number string
!
!> @param t single upper case character
!
!> @date  09/05/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function SYM_getposition(t) result(st)
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_getposition

use local
use constants
use crystal

IMPLICIT NONE

character(3),INTENT(IN)                 :: t
character(fnlen)                        :: st
integer(kind=irg)                       :: i

st = ' ( '
do i=1,3
 select case (t(i:i))
  case ('a');  st = trim(st)//' 0'
  case ('b');  st = trim(st)//' 1/2'
  case ('c');  st = trim(st)//' 1/4'
  case ('d');  st = trim(st)//' 3/4'
  case ('e');  st = trim(st)//' 1/6'
  case ('f');  st = trim(st)//' 1/3'
  case ('g');  st = trim(st)//' 2/3'
  case ('h');  st = trim(st)//' 5/6'
  case ('i');  st = trim(st)//' 1/8'
  case ('j');  st = trim(st)//' 3/8'
  case ('k');  st = trim(st)//' 5/8'
  case ('l');  st = trim(st)//' 7/8'
  case ('m');  st = trim(st)//' -x'
  case ('n');  st = trim(st)//' -y'
  case ('o');  st = trim(st)//' y+1/4'
  case ('p');  st = trim(st)//' x+1/4'
  case ('q');  st = trim(st)//' -y+1/4'
  case ('r');  st = trim(st)//' y+1/2'
  case ('s');  st = trim(st)//' -y+1/2'
  case ('t');  st = trim(st)//' x+1/4'
  case ('u');  st = trim(st)//' x+1/2'
  case ('v');  st = trim(st)//' -x+1/2'
  case ('w');  st = trim(st)//' 2x'
  case ('x');  st = trim(st)//' x'
  case ('y');  st = trim(st)//' y'
  case ('z');  st = trim(st)//' z'
 end select
 if (i.ne.3) then
   st = trim(st)//',' 
 else
   st = trim(st)//' )' 
 end if
end do

end function SYM_getposition

!--------------------------------------------------------------------------
!
! FUNCTION: interpretWyckoffletter
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert Wyckff letter into numerical value
!
!> @param t single upper case character
!> @param x, y, z  x, y, z coordinate values
!
!> @date  09/06/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function interpretWyckoffletter(t,x,y,z) result(st)
!DEC$ ATTRIBUTES DLLEXPORT :: interpretWyckoffletter

use local
use constants
use crystal

IMPLICIT NONE

character(1),INTENT(IN)                 :: t
real(kind=sgl),INTENT(IN)               :: x, y, z

real(kind=sgl)                          :: st

select case (t(1:1))
  case ('a');  st = 0.0
  case ('b');  st = 1.0/2.0
  case ('c');  st = 1.0/4.0
  case ('d');  st = 3.0/4.0
  case ('e');  st = 1.0/6.0
  case ('f');  st = 1.0/3.0
  case ('g');  st = 2.0/3.0
  case ('h');  st = 5.0/6.0
  case ('i');  st = 1.0/8.0
  case ('j');  st = 3.0/8.0
  case ('k');  st = 5.0/8.0
  case ('l');  st = 7.0/8.0
  case ('m');  st = -x
  case ('n');  st = -y
  case ('o');  st = y+1.0/4.0
  case ('p');  st = x+1.0/4.0
  case ('q');  st = -y+1.0/4.0
  case ('r');  st = y+1.0/2.0
  case ('s');  st = -y+1.0/2.0
  case ('t');  st = x+1.0/4.0
  case ('u');  st = x+1.0/2.0
  case ('v');  st = -x+1.0/2.0
  case ('w');  st = 2.0*x
  case ('x');  st = x
  case ('y');  st = y
  case ('z');  st = z
end select

end function interpretWyckoffletter


!--------------------------------------------------------------------------
!
! SUBROUTINE: SYM_getWPstring
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief retrieve the Wyckoff positions encoder string
!
!> @param sgnum space group number
!> @param wpstring Wyckoff Position encoder string
!
!> @date  09/06/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SYM_getWPstring(sgnum,wpstring) 
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_getWPstring

use local
use constants
use crystal
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: sgnum
character(fnlen),INTENT(INOUT)          :: wpstring
!f2py intent(in,out) ::  wpstring

character(fnlen)                        :: line, wpf
logical                                 :: fexists
integer(kind=irg)                       :: ios, i, j

wpf = trim(EMsoft_toNativePath(EMsoft_getWyckoffPositionsfilename()))
inquire(file=trim(wpf),exist=fexists)
if (.not.fexists) then 
  call FatalError('SYM_getWPstring','Wyckoff Positions file not found')
end if

open(UNIT=dataunit,FILE=trim(wpf), STATUS='old', FORM='formatted', ACCESS='sequential')
wpstring = ''
do i = 1, sgnum
 line = ''
 read(dataunit,'(I2,A)',iostat=ios) j, line
 if (ios.ne.0) then 
  exit
 end if
end do
CLOSE(UNIT=dataunit, STATUS='keep')

! wp contains the string of encoded special positions
wpstring = trim(line)

end subroutine SYM_getWPstring

!--------------------------------------------------------------------------
!
! SUBROUTINE: SYM_printWyckoffPositions
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Print a list of Wyckoff positions for the current space group
!
!> @param sgnum space group number
!> @param wpstring Wyckoff Position encoder string
!> @param WyckoffList (optional) list of all Wyckoff position strings
!
!> @date  09/05/16 MDG 1.0 original routine
!--------------------------------------------------------------------------
recursive subroutine SYM_printWyckoffPositions(sgnum,wpstring,WyckoffList) 
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_printWyckoffPositions

use local
use constants
use crystal
use error
use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: sgnum
character(fnlen),INTENT(INOUT)          :: wpstring
!f2py intent(in,out) ::  wpstring
character(6),INTENT(OUT),OPTIONAL       :: WyckoffList(27)

character(fnlen)                        :: line, wpf, mess, gpstring, spstring
logical                                 :: fexists
integer(kind=irg)                       :: ios, i, j, numsp, ipos, io_int(1) 
character(3)                            :: gpmul, spmul, pos
character(5)                            :: splett

call SYM_getWPstring(sgnum,wpstring)

! number of special positions encoded in string 
numsp = (len(trim(wpstring))-1)/4

io_int(1) = sgnum
call WriteValue('Wyckoff positions for space group ',io_int, 1, "(I4)")
mess = '-------------------------------------'
call Message(mess)

! next, loop through all the sets of 4 characters and interpret them
if (numsp.ne.0) then
  do i=1,numsp
    ipos = (i-1)*4+1
! get the multiplicity
    spmul = SYM_getmultiplicity(wpstring(ipos:ipos))
! and interpret the position symbols
    do j=1,3
      pos(j:j) = wpstring(ipos+j:ipos+j)
    end do
    spstring = SYM_getposition(pos)
! and finally the Wyckoff letter
    splett = char(96+i)
! and put them all together
    mess = trim(spmul)//trim(splett)//trim(spstring)
    call Message(mess)
! should we store this in the optional WyckoffList variable?
    if (PRESENT(WyckoffList)) then 
      WyckoffList(i) =  trim(spmul)//trim(splett)
    end if
  end do
end if

! print the general position
if (numsp.eq.26) then
  splett = 'alpha'
else
  splett = char(96+numsp+1)
end if
ipos = numsp*4+1
gpmul = SYM_getmultiplicity(wpstring(ipos:ipos))
mess = trim(gpmul)//trim(splett)//' ( x, y, z )'
! should we store this in the optional WyckoffList variable?
if (PRESENT(WyckoffList)) then 
  WyckoffList(numsp+1) =  trim(gpmul)//trim(splett)
end if
call Message(mess)

end subroutine SYM_printWyckoffPositions


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetAsymPosWyckoff
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read the atom coordinates from standard input in Wyckoff format
!
!> @details ask the user for the atom type, coordinates, site occupation parameter
!> and Debye-Waller parameter for each atom type; this uses the Wyckoff positions
!
!> @param cell unit cell pointer
!!
!> @date   09/06/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetAsymPosWyckoff(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetAsymPosWyckoff

use io
use crystal
use error

IMPLICIT NONE

type(unitcell),INTENT(INOUT)            :: cell
!f2py intent(in,out) ::  cell

logical                                 :: more, found                !< logical to determine if more atoms need to be entered
character(1)                            :: ans, list(6)        !< used for IO
real(kind=sgl)                          :: pt(3), out_real(5)  !< used to read and write asymmetric position data
integer(kind=irg)                       :: i, j, ipos, std, numsp  !< auxiliary variables
real(kind=sgl)                          :: io_real(3)
character(fnlen)                        :: wpstring
character(3)                            :: Wyckoffpos
character(6)                            :: Wyckoffstring
character(6)                            :: WyckoffList(27)


 more=.TRUE.
 cell%ATOM_ntype = 0
 call Message(' Enter atoms in asymmetric unit using Wyckoff positions', frm = "(/A)")
 call DisplayElements()
 call SYM_printWyckoffPositions(cell%SYM_SGnum,wpstring,WyckoffList)

! number of special positions encoded in string 
 numsp = (len(trim(wpstring))-1)/4+1

write (*,*) 'WPstring : ',trim(wpstring)

 do while (more)
  cell%ATOM_ntype = cell%ATOM_ntype + 1

! atomic number
  call ReadValue(' ->  Atomic number, site occupation, Debye-Waller factor : ', io_real, 3)
  cell%ATOM_type(cell%ATOM_ntype) = int(io_real(1))
  cell%ATOM_pos(cell%ATOM_ntype,4:5) = io_real(2:3)

! ask for the Wyckoff position and make sure it actually exists in the list
  found = .FALSE.
  do while (found.eqv..FALSE.)
! ask for the Wyckoff position
    list = (/ (' ',j=1,6) /)
    call Message(' ->  Wyckoff position : ', frm = "(A,' ')",advance="no")
    read (5,"(6A)") list

! find the corresponding encoded triplet
    do i=1,6
      Wyckoffstring(i:i) = list(i)
    end do
    do i=1,numsp
      if (trim(Wyckoffstring).eq.trim(WyckoffList(i))) then 
        found = .TRUE.
        if (i.eq.numsp) then 
          Wyckoffpos = 'xyz'
        else
          do j=1,3
            ipos = (i-1)*4+1+j
            Wyckoffpos(j:j) = wpstring(ipos:ipos)
          end do 
        end if
      end if
    end do 
    if (found.eqv..FALSE.) then
      call Message(' incorrect Wyckoff position; please try again ', frm = "(A,' ')",advance="no")
!   else
!     write (*,*) 'Found Wyckoff position '//Wyckoffpos
    end if
  end do

! interpret this encoded string and extract coordinates and such ...
  call extractWyckoffposition(Wyckoffpos, pt) 
  
! store in the appropriate component of the cell variable  
  cell%ATOM_pos(cell%ATOM_ntype,1:3) = pt(1:3)

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

end subroutine GetAsymPosWyckoff


!--------------------------------------------------------------------------
!
! SUBROUTINE: extractWyckoffposition
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extract atom position data 
!
!> @details Extract the coordinates by interpreting the Wyckoff encoded string
!
!> @param Wyckoffpos Wyckoff position string
!> @param pt set of 3 reals returned to the calling routine
!
!> @date   09/06/16 MDG 1.0 original
!> @date   10/07/16 MDG 1.1 corrected (added) case accum=0 which caused FatalError
!--------------------------------------------------------------------------
recursive subroutine extractWyckoffposition(Wyckoffpos, pt)
!DEC$ ATTRIBUTES DLLEXPORT :: extractWyckoffposition

use error 
use io

IMPLICIT NONE

character(3),INTENT(IN)                 :: Wyckoffpos
real(kind=sgl),INTENT(OUT)              :: pt(3)             !< output real array

integer(kind=irg)                       :: i, accum, findx, findy, findz
real(kind=sgl)                          :: io_real(3), xval, yval, zval

! first we need to figure out which coordinate values we need to ask for
accum = 0
findx = scan(Wyckoffpos,'mptuvwx')
if (findx.ne.0) accum = accum + 1
findy = scan(Wyckoffpos,'noqrsy')
if (findy.ne.0) accum = accum + 2
findz = scan(Wyckoffpos,'z')
if (findz.ne.0) accum = accum + 4

! then we do a case statement to ask for the correct values:
select case (accum)
   case (0)

   case (1)   
              call ReadValue(' Enter x value : ',io_real,1)
              xval = io_real(1)
   case (2)   
              call ReadValue(' Enter y value : ',io_real,1)
              yval = io_real(1)
   case (3)   
              call ReadValue(' Enter x, y values : ',io_real,2)
              xval = io_real(1)
              yval = io_real(2)
   case (4)   
              call ReadValue(' Enter z value : ',io_real,1)
              zval = io_real(1)
   case (5)   
              call ReadValue(' Enter x, z values : ',io_real,2)
              xval = io_real(1)
              zval = io_real(2)
   case (6)   
              call ReadValue(' Enter y, z values : ',io_real,2)
              yval = io_real(1)
              zval = io_real(2)
   case (7)   
              call ReadValue(' Enter x, y, z values : ',io_real,3)
              xval = io_real(1)
              yval = io_real(2)
              zval = io_real(3)
   case default
              call FatalError('extractWyckoffposition','Unknown Wyckoff symbol')
end select

do i=1,3
  pt(i) = interpretWyckoffletter(Wyckoffpos(i:i), xval, yval, zval)
end do

end subroutine extractWyckoffposition

!--------------------------------------------------------------------------
!
! SUBROUTINE: getLaueGroupNumber
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief get the Laue point group number for a given space group number
!
!> @param SGnum space group number
!
!> @date   10/18/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getLaueGroupNumber(SGnum) result(LGN)
!DEC$ ATTRIBUTES DLLEXPORT :: getLaueGroupNumber

use constants
use typedefs

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: SGnum
integer(kind=irg)               :: LGN, i

! Oxford software uses the following symmetry conversion table:
! LG_Triclinic = 1,
! LG_Monoclinic = 2,
! LG_Orthorhombic = 3,
! LG_Tetragonal_Low = 4,
! LG_Tetragonal_High = 5,
! LG_Trigonal_Low = 6,
! LG_Trigonal_High = 7,
! LG_Hexagonal_Low = 8,
! LG_Hexagonal_High = 9,
! LG_Cubic_Low = 10,
! LG_Cubic_High = 11,
! UnknownSymmetry = 12    -> this value is not used in EMsoft
! this function returns one of the above numbers for a given space group number

! find the rotational symmetry group number
if (SGnum.ge.221) then
  i = 32
else
  i=0
  do while (SGPG(i+1).le.SGnum) 
    i = i+1
  end do
end if
LGN = PGLaueinv(i)

end function getLaueGroupNumber 

end module
