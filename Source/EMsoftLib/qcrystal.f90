! ###################################################################
! Copyright (c) 2014-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:qcrystal.f90
!--------------------------------------------------------------------------
!
! MODULE: qcrystal
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Everything that has to do with crystallographic input/output for quasicrystals
!
!> @details  This includes only the qxtal file i/o for now. the crystallographic
!> computations might be moved here later 
! 
!> @date  05/22/18   SS 1.0 original
!--------------------------------------------------------------------------

module qcrystal

use local
use typedefs

private   :: matrixmult, isitnew, extractposition, isnewvector

public

interface GetQCLatParm
  module procedure Get2DQCLatParm
  module procedure Get3DQCLatParm
end interface GetQCLatParm

interface PrintSGTable
  module procedure Print2DQCSGTable
  module procedure Print3DQCSGTable
end interface PrintSGTable

interface SYM_fillgen_QC
  !module procedure SYM_fillgen_2DQC
  module procedure SYM_fillgen_3DQC
end interface SYM_fillgen_QC

interface GenerateQCSymmetry
  module procedure Generate2DQCSymmetry
  module procedure Generate3DQCSymmetry
end interface GenerateQCSymmetry

interface GetQCSpaceGroup
  module procedure Get2DQCSpaceGroup
  module procedure Get3DQCSpaceGroup
end interface GetQCSpaceGroup

interface GetQCAsymPos
  module procedure Get2DQCAsymPos
  module procedure Get3DQCAsymPos
end interface GetQCAsymPos

interface CalcQCPositions
  module procedure Calc2DQCPositions
  module procedure Calc3DQCPositions
end interface CalcQCPositions

interface isnewvector
  module procedure isnewvector2DQC_int
  module procedure isnewvector2DQC_dbl
  module procedure isnewvector3DQC_int
  module procedure isnewvector3DQC_dbl
end interface isnewvector

interface isitnew
  module procedure isitnew2DQC
  module procedure isitnew3DQC
end interface isitnew

interface matrixmult
  module procedure matrixmult2DQC
  module procedure matrixmult3DQC
end interface matrixmult

interface MakeQCGenerators
  module procedure MakeTDQCGenerators
  module procedure Make3DQCGenerators
end interface MakeQCGenerators

interface GetQCOrbit
  module procedure Get2DQCOrbit
  module procedure Get3DQCOrbit
end interface GetQCOrbit

interface GetQCPGsymmetry
  module procedure Get3DQCPGsymmetry
  module procedure Get2DQCPGsymmetry
end interface GetQCPGsymmetry

interface QC_getindex
	module procedure QC_get5Dindex
	module procedure QC_get6Dindex
end interface QC_getindex

interface QC_inverseIndex
	module procedure QC_invert5Dindex
	module procedure QC_invert6Dindex
end interface QC_inverseIndex

contains

! !--------------------------------------------------------------------------
! !
! ! SUBROUTINE: QCSYM_fillgen
! !
! !> @author Saransh Singh, Carnegie Mellon University
! !
! !> @brief create a generator matrix
! !
! !> @details  fills in a generator matrix based on an input 6-character code string
! !
! !> @param cell unit cell pointer
! !> @param t input string (4 character block from generator string)
! !> @param isgn switch to indicate forward or reverse translation component
! !
! !> @date  10/13/98 SS 1.0 original, adapted from symmetry.f90 module
! !--------------------------------------------------------------------------
! recursive subroutine QCSYM_fillgen(cell, t, isgn)
! !DEC$ ATTRIBUTES DLLEXPORT :: QCSYM_fillgen

! IMPLICIT NONE

! type(TDQCStructureType), pointer            :: TDQCcell
! character(1),INTENT(IN)                     :: t(6) !< 6-character input string
! integer(kind=irg),INTENT(IN)                :: isgn !< indicates forward or reverse translation

! integer(kind=irg)                           :: j    !< auxiliary variable
! real(kind=dbl)                              :: sgn  !< forward or reverse multiplier for translation components

! ! forward or reverse translation ?
!  sgn=dble(isgn)

! ! first fill the array with zeroes and a 1 at 4,4
!  TDQCcell%SG%SYM_c(1:6,1:6) = 0.0_dbl
!  TDQCcell%SG%SYM_c(6,6) = 1.0_dbl

! ! then check for the particular matrix type
!  select case (t(1))

!  end select

! ! then fill in the translational component
!  do j=2,4 
!   select case (t(j))
!    case('A'); cell%SG%SYM_c(j-1,4) = sgn/6.0_dbl
!    case('B'); cell%SG%SYM_c(j-1,4) = sgn/4.0_dbl
!    case('C'); cell%SG%SYM_c(j-1,4) = sgn/3.0_dbl
!    case('D'); cell%SG%SYM_c(j-1,4) = sgn/2.0_dbl
!    case('E'); cell%SG%SYM_c(j-1,4) = sgn*2.0_dbl/3.0_dbl
!    case('F'); cell%SG%SYM_c(j-1,4) = sgn*3.0_dbl/4.0_dbl
!    case('G'); cell%SG%SYM_c(j-1,4) = sgn*5.0_dbl/6.0_dbl
!    case('O'); cell%SG%SYM_c(j-1,4) = 0.0_dbl
!    case('X'); cell%SG%SYM_c(j-1,4) = -sgn*3.0_dbl/8.0_dbl
!    case('Y'); cell%SG%SYM_c(j-1,4) = -sgn/4.0_dbl
!    case('Z'); cell%SG%SYM_c(j-1,4) = -sgn/8.0_dbl
!   end select
!  end do

! end subroutine QCSYM_fillgen

!--------------------------------------------------------------------------
!
! SUBROUTINE: SYM_fillgen_3DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief create a generator matrix
!
!> @details  fills in a generator matrix based on an input 6-character code string
!
!> @param cell unit cell pointer
!> @param t input string (4 character block from generator string)
!> @param isgn switch to indicate forward or reverse translation component
!
!> @date  07/02/18 SS 1.0 original, adapted from symmetry.f90 module
!--------------------------------------------------------------------------
recursive subroutine SYM_fillgen_3DQC(cell, t)
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_fillgen_3DQC

IMPLICIT NONE

type(QCStructureType), pointer            	:: cell
character(1),INTENT(IN)                     :: t(7) !< 7-character input string
integer(kind=irg)                           :: j    !< auxiliary variable
real(kind=dbl)                              :: sgn  !< forward or reverse multiplier for translatio

! forward or reverse translation ?

! first fill the array with zeroes and a 1 at 4,4
 cell%SG%SYM_c(1:7,1:7) 	= 0.0_dbl
 cell%SG%SYM_c(7,7) 		= 1.0_dbl
! then check for the particular matrix type
 select case (t(1))
 case('a')
 	cell%SG%SYM_c(1,1) = 1.0_dbl; cell%SG%SYM_c(2,2) = 1.0_dbl; cell%SG%SYM_c(3,3) = 1.0_dbl
 	cell%SG%SYM_c(4,4) = 1.0_dbl; cell%SG%SYM_c(5,5) = 1.0_dbl; cell%SG%SYM_c(6,6) = 1.0_dbl
 case('b')
 	cell%SG%SYM_c(1,1) = 1.0_dbl; cell%SG%SYM_c(6,2) = 1.0_dbl; cell%SG%SYM_c(2,3) = 1.0_dbl
 	cell%SG%SYM_c(3,4) = 1.0_dbl; cell%SG%SYM_c(4,5) = 1.0_dbl; cell%SG%SYM_c(5,6) = 1.0_dbl
 case('c')
 	cell%SG%SYM_c(2,1) = 1.0_dbl; cell%SG%SYM_c(6,2) = 1.0_dbl; cell%SG%SYM_c(4,3) = -1.0_dbl
 	cell%SG%SYM_c(5,4) = -1.0_dbl; cell%SG%SYM_c(3,5) = 1.0_dbl; cell%SG%SYM_c(1,6) = 1.0_dbl
 case('d')
 	cell%SG%SYM_c(1,1) = -1.0_dbl; cell%SG%SYM_c(2,2) = -1.0_dbl; cell%SG%SYM_c(3,3) = -1.0_dbl
 	cell%SG%SYM_c(4,4) = -1.0_dbl; cell%SG%SYM_c(5,5) = -1.0_dbl; cell%SG%SYM_c(6,6) = -1.0_dbl
 end select

! then fill in the translational component
 do j=1,6 
  select case (t(j+1))
   case('A'); cell%SG%SYM_c(j,7) = 1.0_dbl/5.0_dbl
   case('B'); cell%SG%SYM_c(j,7) = 1.0_dbl/2.0_dbl
  end select
 end do

end subroutine SYM_fillgen_3DQC

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! FUNCTION:QC_get6Dindex
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Convert a 6-component Miller index to a single lookup index
!
!> @param QCcell QCStructureType variable
!> @param QCindex set of QC Miller indices
!
!> @date 03/17/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function QC_get5Dindex(QCcell, QCindex) result(gindex)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_get5Dindex


IMPLICIT NONE

type(TDQCStructureType),pointer        :: QCcell
integer(kind=irg),INTENT(IN)           :: QCindex(5)
integer(kind=irg)                      :: gindex, imax_qc, imax_p, isize_qc, isize_p, g(5)

imax_qc   = QCcell%imax_qc
imax_p 	  = QCcell%imax_p
isize_qc  = 2 * imax_qc + 1
isize_p   = 2 * imax_p  + 1
g(1:4)    = QCindex(1:4) + imax_qc
g(5) 	  = QCindex(5)   + imax_p 
gindex 	  = g(1) * isize_qc**3 * isize_p + g(2)*isize_qc**2 * isize_p + g(3)*isize_qc * isize_p + &
         	g(4)*isize_p + g(5) + 1

end function QC_get5Dindex
!--------------------------------------------------------------------------
!
! FUNCTION:QC_invert6Dindex
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Convert a single lookup index into the corresponding 6-component Miller index 
!
!> @details This relation was derived and verified with Mathematica
!
!> @param QCcell QCStructureType variable
!> @param gindex single lookup table index
!
!> @date 03/17/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function QC_invert5Dindex(QCcell, gindex) result(QCindex)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_invert5Dindex 

IMPLICIT NONE

type(TDQCStructureType),pointer        :: QCcell
integer(kind=irg),INTENT(IN)           :: gindex
integer(kind=irg)                      :: QCindex(5)

QCindex(1:5) = QCcell%inverseIndex(gindex,1:5)

end function QC_invert5Dindex

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! FUNCTION:QC_get6Dindex
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Convert a 6-component Miller index to a single lookup index
!
!> @param QCcell QCStructureType variable
!> @param QCindex set of QC Miller indices
!
!> @date 03/17/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function QC_get6Dindex(QCcell, QCindex) result(gindex)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_get6Dindex

use others, only: SSORT

IMPLICIT NONE

type(QCStructureType),pointer          :: QCcell
integer(kind=irg),INTENT(IN)           :: QCindex(6)
integer(kind=irg)                      :: gindex, imax, isize, g(6)

imax   = QCcell%imax
isize  = 2 * imax + 1
g      = QCindex + imax 
gindex = g(1)*isize**5 + g(2)*isize**4 + g(3)*isize**3 + &
         g(4)*isize**2 + g(5)*isize    + g(6) + 1

end function QC_get6Dindex
!--------------------------------------------------------------------------
!
! FUNCTION:QC_invert6Dindex
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Convert a single lookup index into the corresponding 6-component Miller index 
!
!> @details This relation was derived and verified with Mathematica
!
!> @param QCcell QCStructureType variable
!> @param gindex single lookup table index
!
!> @date 03/17/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function QC_invert6Dindex(QCcell, gindex) result(QCindex)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_invert6Dindex 

IMPLICIT NONE

type(QCStructureType),pointer          :: QCcell
integer(kind=irg),INTENT(IN)           :: gindex
integer(kind=irg)                      :: QCindex(6)

QCindex(1:6) = QCcell%inverseIndex(gindex,1:6)

end function QC_invert6Dindex

!--------------------------------------------------------------------------
! SUBROUTINE: Print3DQCSGTable
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate SG name and fill 3D quasicrystal structure
!
!> Daniel Rokhsar, David Wright, David Mermin, Scale equivalence of 
!> quasicrystallographic space groups, PHYS REV B, 37(14), 1987.
!
!--------------------------------------------------------------------------
recursive subroutine Print3DQCSGTable(QCcell,toprint)
!DEC$ ATTRIBUTES DLLEXPORT :: Print3DQCSGTable

use io
use error

IMPLICIT NONE

type(QCStructureType), pointer              :: QCcell
logical,INTENT(IN),OPTIONAL                 :: toprint

character(25)                               :: bspace, sgname1, sgname2
character(fnlen)                            :: str, str1
logical                                     :: pflag
integer(kind=irg)                           :: nsg, ii

pflag = .TRUE.
bspace = ''
if(present(toprint)) then
  if(.not.toprint) then
    pflag = .FALSE.
  end if
end if

nsg = 11
QCcell%SGname(1)  	= 'P 5 3 2'
QCcell%SG%SYM_GL(1) = '2b000000c000000'

QCcell%SGname(2)  	= 'P 5_1 3 2'
QCcell%SG%SYM_GL(2) = '2bA00000c000000'

QCcell%SGname(3)  	= 'I 5 3 2'
QCcell%SG%SYM_GL(3) = '3aBBBBBBb000000c000000'

QCcell%SGname(4)  	= 'I 5_1 3 2'
QCcell%SG%SYM_GL(4) = '3aBBBBBBbA00000c000000'

QCcell%SGname(5)  	= 'F 5 3 2'
QCcell%SG%SYM_GL(5) = '7aBB0000a0BB000a00BB00a000BB0a0000BBb000000c000000'

QCcell%SGname(6)  	= 'F 5_1 3 2'
QCcell%SG%SYM_GL(6) = '7aBB0000a0BB000a00BB00a000BB0a0000BBbA00000c000000'

QCcell%SGname(7)  	= 'P -5 -3 2/m'
QCcell%SG%SYM_GL(7) = '3b000000c000000d000000'

QCcell%SGname(8)  	= 'P -5 -3 2/q'
QCcell%SG%SYM_GL(8) = '3b000000c000000d000000'

QCcell%SGname(9)  	= 'I -5 -3 2/m'
QCcell%SG%SYM_GL(9) = '4aBBBBBBb000000c000000d000000'

QCcell%SGname(10) 	 = 'F -5 -3 2/m'
QCcell%SG%SYM_GL(10) = '8aBB0000a0BB000a00BB00a000BB0a0000BBb000000c000000d000000'

QCcell%SGname(11) 	 = 'F -5 -3 2/q'
QCcell%SG%SYM_GL(11) = '3b000000c000000d000000'

if(pflag) then
      if(mod(nsg,2) .ne. 0) then
        do ii = 1, floor(float(nsg)/2.0) + 1
          if(ii .lt. floor(float(nsg)/2.0) + 1) then
            write(str,'(I3,A)') 2*ii-1,':'
            write(str1,'(I3,A)') 2*ii,':'
            sgname1 = ''
            sgname2 = ''
            sgname1 = trim(QCcell%SGname(2*ii-1))
            sgname2 = trim(QCcell%SGname(2*ii))
            call Message(trim(str)//' '//sgname1//trim(str1)//' '//sgname2)
          else
            write(str,'(I3,A)')2*ii-1,':'
            sgname1 = ''
            sgname1 = trim(QCcell%SGname(2*ii-1))
            call Message(trim(str)//sgname1)
          end if
        end do
      else
        do ii = 1, nsg/2
          write(str,'(I3,A)')2*ii-1,':'
          write(str1,'(I3,A)')2*ii,': '
          sgname1 = ''
          sgname2 = ''
          sgname1 = trim(QCcell%SGname(2*ii-1))
          sgname2 = trim(QCcell%SGname(2*ii))
          call Message(trim(str)//' '//sgname1//trim(str1)//' '//sgname2)
        end do
      end if
    end if

end subroutine Print3DQCSGTable

!--------------------------------------------------------------------------
! SUBROUTINE: Print2DQCSGTable
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate SG name and fill 2D quasicrystal structure
!
!> David Rabson, David Mermin, Daniel Rokhsar, David Wright, The space groups
!> of axial crystals and quasicrystals, REVIEW OF MODERN PHYSICS, 63(3), 1991.
!
!--------------------------------------------------------------------------
recursive subroutine Print2DQCSGTable(TDQCcell,toprint)
!DEC$ ATTRIBUTES DLLEXPORT :: Print2DQCSGTable

use io
use error

IMPLICIT NONE

type(TDQCStructureType), pointer            :: TDQCcell
logical,INTENT(IN),OPTIONAL                 :: toprint

integer(kind=irg)                           :: n      ! axial rotational symmetry
character(1)                                :: lat    ! primitive or centered
character(fnlen)                            :: str, str1, str2    ! concatanate various symbols here
character(25)                               :: bspace, sgname1, sgname2
integer(kind=irg)                           :: nsg, styp, p, io_int(1), ii
logical                                     :: pflag

! n is the rotational symmetry of the highest symmetry axis
! styp is the categorization of n based on the following conditions
! 1. styp == 1 for n power of odd prime e.g. 3-fold, 7-fold, 9-fold etc.
! 2. styp == 1 for n power of two e.g. 2-fold, 4-fold, 8-fold etc.
! 3. styp == 1 for n twice the power of odd prime e.g. 6-fold, 10-fold etc.
! 4. styp == 1 for n even but not twice a prime power e.g. 12-fold, 20-fold etc.
! 5. styp == 1 for n odd but not a prime power e.g. 15-fold, 21-fold etc.

pflag = .TRUE.
bspace = ''
if(present(toprint)) then
  if(.not.toprint) then
    pflag = .FALSE.
  end if
end if

if(trim(TDQCcell%QCtype) .eq. 'Oct') then
  n     = 8
  styp  = 2
else if(trim(TDQCcell%QCtype) .eq. 'Dec') then
  n     = 10
  styp  = 3

else if(trim(TDQCcell%QCtype) .eq. 'DoD') then
  n     = 12
  styp  = 4 
else
  call FatalError('PrintSGTable:','only 8, 10 and 12 fold symmetries implemented till now.')

end if

select case(styp)
  case(1)
    !p = ! get odd prime number
    !nsg = 19 + 3 * (n - 1) + 2 * (n/p - 1)
    !io_int(1) = nsg
    !write(str,'(I2)') n
    !call WriteValue('Number of space groups for axial symmetry of '//trim(str)//' = ',io_int,1,'(I3,//)')
    !allocate(TDQCcell%SGname(nsg))
    call FatalError('PrintSGTable:','not implemented yet')

  case(2)
    nsg = 57 + 3*(n - 1) + 2*(n/2 - 1)
    io_int(1) = nsg
    write(str,'(I2)') n
    if(pflag) then
      call WriteValue('Number of space groups for axial symmetry of '//trim(str)//' = ',io_int,1,'(I3,//)')
    end if
    if(.not.(allocated(TDQCcell%SGname))) allocate(TDQCcell%SGname(nsg))

  case(3)
    nsg = 17 + 2*(n - 1)
    io_int(1) = nsg
    write(str,'(I2)') n
    if(pflag) then
      call WriteValue('Number of space groups for axial symmetry of '//trim(str)//' = ',io_int,1,'(I3,//)')
    end if
    if(.not.(allocated(TDQCcell%SGname))) allocate(TDQCcell%SGname(nsg))

    ! fill out the names and print on screen
    write(str1,'(I6)') n
    str1 = adjustl(str1)
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(1) = trim(str)


    write(str1,'(I6)') -n
    str1 = adjustl(str1)
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(2) = trim(str)

    do ii = 3, n+1
      write(str1,'(I6)') ii-2
      str1 = adjustl(str1)
      str1 = '_'//trim(str1)
      write(str2,'(I6,A)') n,trim(str1)
      str2 = adjustl(str2)
      write(str,'(A,A)')'P',trim(str2)
      TDQCcell%SGname(ii) = trim(str)
    end do

    write(str1,'(I6)') n
    str1 = adjustl(str1)
    str1 = trim(str1)//' 2 2'
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(n+2) = trim(str)

    do ii = n+3, 2*n+1
      write(str1,'(I6)') ii-n-2
      str1 = adjustl(str1)
      str1 = '_'//trim(str1)//' 2 2'
      write(str2,'(I6,A)') n,trim(str1)
      str2 = adjustl(str2)
      write(str,'(A,A)')'P',trim(str2)
      TDQCcell%SGname(ii) = trim(str)
    end do

    write(str1,'(I6)') -n
    str1 = adjustl(str1)
    TDQCcell%SGname(2*n+2) = 'P'//trim(str1)//' m 2'
    TDQCcell%SGname(2*n+3) = 'P'//trim(str1)//' c 2'
    TDQCcell%SGname(2*n+4) = 'P'//trim(str1)//' 2 m'
    TDQCcell%SGname(2*n+5) = 'P'//trim(str1)//' 2 c'

    write(str1,'(I6)') n
    str1 = adjustl(str1)
    write(str2,'(I6)') n/2
    str2 = adjustl(str2)

    TDQCcell%SGname(2*n+6) = 'P'//trim(str1)//' m m'
    TDQCcell%SGname(2*n+7) = 'P'//trim(str1)//' c c'
    TDQCcell%SGname(2*n+8) = 'P'//trim(str1)//'_'//trim(str2)//' m c'
    TDQCcell%SGname(2*n+9) = 'P'//trim(str1)//'_'//trim(str2)//' c m'

    TDQCcell%SGname(2*n+10) = 'P'//trim(str1)//' / m'
    TDQCcell%SGname(2*n+11) = 'P'//trim(str1)//'_'//trim(str2)//' / m'
    TDQCcell%SGname(2*n+12) = 'P'//trim(str1)//' / m 2 / m 2 / m'
    TDQCcell%SGname(2*n+13) = 'P'//trim(str1)//'/ m 2 / c 2 / c'
    TDQCcell%SGname(2*n+14) = 'P'//trim(str1)//'_'//trim(str2)//' / m 2 / m 2 / c'
    TDQCcell%SGname(2*n+15) = 'P'//trim(str1)//'_'//trim(str2)//' / m 2 / c 2 / m'

    if(pflag) then
      if(mod(nsg,2) .ne. 0) then
        do ii = 1, floor(float(nsg)/2.0) + 1
          if(ii .lt. floor(float(nsg)/2.0) + 1) then
            write(str,'(I3,A)') 2*ii-1,':'
            write(str1,'(I3,A)') 2*ii,':'
            sgname1 = ''
            sgname2 = ''
            sgname1 = trim(TDQCcell%SGname(2*ii-1))
            sgname2 = trim(TDQCcell%SGname(2*ii))
            call Message(trim(str)//' '//sgname1//trim(str1)//' '//sgname2)
          else
            write(str,'(I3,A)')2*ii-1,':'
            sgname1 = ''
            sgname1 = trim(TDQCcell%SGname(2*ii-1))
            call Message(trim(str)//sgname1)
          end if
        end do
      else
        do ii = 1, nsg/2
          write(str,'(I3,A)')2*ii-1,':'
          write(str1,'(I3,A)')2*ii,': '
          sgname1 = ''
          sgname2 = ''
          sgname1 = trim(TDQCcell%SGname(2*ii-1))
          sgname2 = trim(TDQCcell%SGname(2*ii))
          call Message(trim(str)//' '//sgname1//trim(str1)//' '//sgname2)
        end do
      end if
    end if

  case(4)
    nsg = 13 + 2*(n - 1)
    io_int(1) = nsg
    write(str,'(I2)') n
    if(pflag) then
      call WriteValue('Number of space groups for axial symmetry of '//trim(str)//' = ',io_int,1,'(I3,//)')
    end if
    if(.not.(allocated(TDQCcell%SGname))) allocate(TDQCcell%SGname(nsg))

    write(str1,'(I6)') n
    str1 = adjustl(str1)
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(1) = trim(str)

    write(str1,'(I6)') -n
    str1 = adjustl(str1)
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(2) = trim(str)

    do ii = 3, n+1
      write(str1,'(I6)') ii-2
      str1 = adjustl(str1)
      str1 = '_'//trim(str1)
      write(str2,'(I6,A)') n,trim(str1)
      str2 = adjustl(str2)
      write(str,'(A,A)')'P',trim(str2)
      TDQCcell%SGname(ii) = trim(str)
    end do

    write(str1,'(I6)') n
    str1 = adjustl(str1)
    str1 = trim(str1)//' 2 2'
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(n+2) = trim(str)

    do ii = n+3, 2*n+1
      write(str1,'(I6)') ii-n-2
      str1 = adjustl(str1)
      str1 = '_'//trim(str1)//' 2 2'
      write(str2,'(I6,A)') n,trim(str1)
      str2 = adjustl(str2)
      write(str,'(A,A)')'P',trim(str2)
      TDQCcell%SGname(ii) = trim(str)
    end do

    write(str1,'(I6)') -n
    str1 = adjustl(str1)
    TDQCcell%SGname(2*n+2) = 'P'//trim(str1)//' 2 m'
    TDQCcell%SGname(2*n+3) = 'P'//trim(str1)//' 2 c'

    write(str1,'(I6)') n
    str1 = adjustl(str1)
    write(str2,'(I6)') n/2
    str2 = adjustl(str2)

    TDQCcell%SGname(2*n+4) = 'P'//trim(str1)//' m m'
    TDQCcell%SGname(2*n+5) = 'P'//trim(str1)//' c c'
    TDQCcell%SGname(2*n+6) = 'P'//trim(str1)//'_'//trim(str2)//' c m'

    TDQCcell%SGname(2*n+7) = 'P'//trim(str1)//' / m'
    TDQCcell%SGname(2*n+8) = 'P'//trim(str1)//'_'//trim(str2)//' / m'
    TDQCcell%SGname(2*n+9) = 'P'//trim(str1)//' / m 2 / m 2 / m'
    TDQCcell%SGname(2*n+10) = 'P'//trim(str1)//'/ m 2 / c 2 / c'
    TDQCcell%SGname(2*n+11) = 'P'//trim(str1)//'_'//trim(str2)//' / m 2 / c 2 / m'

    if(pflag) then
      if(mod(nsg,2) .ne. 0) then
        do ii = 1, floor(float(nsg)/2.0) + 1
          if(ii .lt. floor(float(nsg)/2.0) + 1) then
            write(str,'(I3,A)') 2*ii-1,':'
            write(str1,'(I3,A)') 2*ii,':'
            sgname1 = ''
            sgname2 = ''
            sgname1 = trim(TDQCcell%SGname(2*ii-1))
            sgname2 = trim(TDQCcell%SGname(2*ii))
            call Message(trim(str)//' '//sgname1//trim(str1)//' '//sgname2)
          else
            write(str,'(I3,A)')2*ii-1,':'
            sgname1 = ''
            sgname1 = trim(TDQCcell%SGname(2*ii-1))
            call Message(trim(str)//sgname1)
          end if
        end do
      else
        do ii = 1, nsg/2
          write(str,'(I3,A)')2*ii-1,':'
          write(str1,'(I3,A)')2*ii,': '
          sgname1 = ''
          sgname2 = ''
          sgname1 = trim(TDQCcell%SGname(2*ii-1))
          sgname2 = trim(TDQCcell%SGname(2*ii))
          call Message(trim(str)//' '//sgname1//trim(str1)//' '//sgname2)
        end do
      end if
    end if

  case(5)
    nsg = 7 + 2*(n - 1)
    io_int(1) = nsg
    write(str,'(I2)') n
    if(pflag) then
      call WriteValue('Number of space groups for axial symmetry of '//trim(str)//' = ',io_int,1,'(I3,//)')
    end if
    if(.not.(allocated(TDQCcell%SGname))) allocate(TDQCcell%SGname(nsg))
    call FatalError('PrintSGTable:','not implemented yet');

  case DEFAULT
    call FatalError('PrintSGTable:','n-fold axis couldn''t be categorized into one of the 5 categories. &
      see: The space groups of axial crystals and quasicrystals, REVIEW OF MODERN PHYSICS, 63(3), 1991');

end select


end subroutine Print2DQCSGTable

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetQCType
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief 2D or 3D quasicrystal
!
!> @details  Input of quasicrystal type; decides what routines to call next
!
!> @param qcdim dimensionality of quasicrystal
!
!> @date   05/22/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetQCType(qcdim)
!DEC$ ATTRIBUTES DLLEXPORT :: GetQCType

use io

integer(kind=irg),INTENT(OUT)           :: qcdim
integer(kind=irg)                       :: io_int(1)

call Message(' Select the quasicrystal dimensionality : ', frm = "(A)")
call Message('  1. 2-dimensional (axial) quasicrystal', frm = "(A/)")
call Message('  2. 3-dimensional (icosahedral) quasicrystal', frm = "(A//)")
call ReadValue(' quasi-crystal dimensionality ---> ', io_int, 1)
qcdim = io_int(1) 

end subroutine GetQCType

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get2DQCLatParm
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief input of lattice parameters for 2D quasi-crystal
!
!> @details  Input of crystal system followed by the appropriate set of lattice
!> parameters; all are stored in the TDQCcell type.
!>
!
!> @param TDQCcell 2D quasicrystal unit cell pointer
!
!> @date   05/22/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Get2DQCLatParm(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get2DQCLatParm

use io

IMPLICIT NONE

type(TDQCStructureType), pointer        :: TDQCcell

integer(kind=irg)                       :: io_int(1)    !< integer input array
real(kind=dbl)                          :: io_real(1)   !< double precision real input array
integer(kind=irg)                       :: std

! this routine assumes that the cell pointer has been associated elsewhere

 call Message(' Select the 2D quasicrystal type (axial symmetry) : ', frm = "(//A)")
 call Message('  1. octagonal (8-fold) quasicrystal', frm = "(A/)")
 call Message('  2. decagonal (10-fold) quasicrystal', frm = "(A/)")
 call Message('  3. dodecagonal (12-fold) quasicrystal', frm = "(A//)")
 call ReadValue(' quasi-crystal type ---> ', io_int, 1)

 select case (io_int(1))
 case(1)
  ! 8-fold
  TDQCcell%QCtype         = 'Oct'
  TDQCcell%SG%N_Axial     =  8
  TDQCcell%alphaij        =  90.D0
  TDQCcell%alphastarij    =  90.D0
  TDQCcell%alphai5        =  90.D0

 case(2)
  ! 10-fold
  TDQCcell%QCtype         = 'Dec'
  TDQCcell%SG%N_Axial     =  10
  TDQCcell%alphaij        =  60.D0
  TDQCcell%alphastarij    =  104.5D0
  TDQCcell%alphai5        =  90.D0
 case(3)
  ! 12-fold
  TDQCcell%QCtype         = 'DoD'
  TDQCcell%SG%N_Axial     =  12
  TDQCcell%alphaij        =  120.D0
  TDQCcell%alphastarij    =  60.D0
  TDQCcell%alphai5        =  90.D0

 end select

 call Message(' Using the higher dimensional cut and project approach:', frm = "(A)")
 call Message(' -------------------------------', frm = "(A)")
 

  ! get the lattice parameters
   call Message('Enter lattice parameters', frm = "(//A)")

   ! in-plane lattice parameters
   call ReadValue('    a_i | i = {1,2,3,4} (quasicrystal plane) [nm] = ', io_real, 1)
   TDQCcell%QClatparm_a = io_real(1)

   ! out of plane lattice parameters
   call ReadValue('    a_5 (axial direction) [nm] = ', io_real, 1)
   TDQCcell%QClatparm_c = io_real(1)

  
end subroutine Get2DQCLatParm

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get3DQCLatParm
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief input of lattice parameters for 3D (icosahedral) quasi-crystal
!
!> @details  Input of crystal system followed by the appropriate set of lattice
!> parameters; all are stored in the QCcell type.
!>
!
!> @param QCcell unit cell pointer
!!
!> @date   05/22/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Get3DQCLatParm(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get3DQCLatParm

use io

IMPLICIT NONE

type(QCStructureType),pointer           :: QCcell

integer(kind=irg)                       :: io_int(1)    !< integer input array
real(kind=dbl)                          :: io_real(1)   !< double precision real input array
integer(kind=irg)                       :: std

! this routine assumes that the cell pointer has been associated elsewhere

 QCcell%QCtype         = 'Ico'
 QCcell%alphaij        =  90.D0
 QCcell%alphastarij    =  90.D0

 call Message(' Using the higher dimensional cut and project approach:', frm = "(A)")
 call Message(' -------------------------------', frm = "(A)")
 
  ! get the lattice parameters
 call Message('Enter lattice parameters', frm = "(//A)")

  ! in-plane lattice parameters
 call ReadValue('    a_i | i = {1,2,3,4,5,6} (hyper-cube) [nm] = ', io_real, 1)
 QCcell%QClatparm = io_real(1)
  
end subroutine Get3DQCLatParm

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get3DQCSpaceGroup
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief input of space group for 2D (axial) quasi-crystal
!
!> @details  Input of space group.
!
!> @param TDQCcell unit cell pointer
!!
!> @date   05/22/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Get3DQCSpaceGroup(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get3DQCSpaceGroup

use io

IMPLICIT NONE

type(QCStructureType),pointer             :: QCcell

integer(kind=irg)                         :: io_int(1)

 ! get the space group number
 call Message('Enter space group number', frm = "(//A)")

 call ReadValue('    Space group number --> ', io_int, 1)
 QCcell%SYM_SGnum = io_int(1)

end subroutine Get3DQCSpaceGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get2DQCSpaceGroup
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief input of space group for 2D (axial) quasi-crystal
!
!> @details  Input of space group.
!
!> @param TDQCcell unit cell pointer
!!
!> @date   05/22/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Get2DQCSpaceGroup(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get2DQCSpaceGroup

use io

IMPLICIT NONE

type(TDQCStructureType),pointer           :: TDQCcell

integer(kind=irg)                         :: io_int(1)

 ! get the space group number
 call Message('Enter space group number', frm = "(//A)")

 call ReadValue('    Space group number --> ', io_int, 1)
 TDQCcell%SYM_SGnum = io_int(1)

end subroutine Get2DQCSpaceGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get3DQCAsymPos
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read the atom coordinates from standard input
!
!> @details ask the user for the atom type, coordinates, site occupation parameter
!> and Debye-Waller parameter for each atom type.
!
!> @param 3D QC unit cell pointer
!!
!> @date   05/23/18 SS 1.0 original, adapted from symmetry module
!--------------------------------------------------------------------------
recursive subroutine Get3DQCAsymPos(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get3DQCAsymPos

use io
use crystal, only: DisplayElements

type(QCStructureType),pointer         :: QCcell

logical                                 :: more                 !< logical to determine if more atoms need to be entered
character(1)                            :: ans, list(256)       !< used for IO
real(kind=sgl)                          :: pt(10), out_real(10)   !< used to read and write asymmetric position data
integer(kind=irg)                       :: j, io_int(1) , std   !< auxiliary variables

more=.TRUE.
 QCcell%ATOM_ntype = 0
 call Message(' Enter atoms in asymmetric unit ', frm = "(/A)")
 call DisplayElements()

 do while (more)
  QCcell%ATOM_ntype = QCcell%ATOM_ntype + 1

! atomic number
  call ReadValue(' ->  Atomic number : ', io_int, 1)
  QCcell%ATOM_type(QCcell%ATOM_ntype) = io_int(1)

! general atom coordinate
  list = (/ (' ',j=1,256) /)
  call Message(' ->  Fractional coordinates, site occupation, Bpar [nm^2],'&
  'Bperp [nm^2], radial atomic size (fraction of a_i) : ',&
  frm = "(A,' ')",advance="no")
  read (*,"(256A)") list

! interpret this string and extract coordinates and such ...
  call extractposition(list,pt,iQC=.TRUE.) 
  
! store in the appropriate component of the cell variable  
  QCcell%ATOM_pos(QCcell%ATOM_ntype,1:10) = pt(1:10)

! and write the coordinate back to the terminal  
  out_real = (/ (QCcell%ATOM_pos(QCcell%ATOM_ntype,j),j=1,10) /)
  call WriteValue('    -> ', out_real, 10, frm = "(1x,6(F10.7,2x),3(F10.7,2x),F10.7)") 

  call ReadValue(' ->  Another atom ? (y/n) ', ans, frm = "(A1)")
  if ((ans.eq.'y').or.(ans.eq.'Y')) then 
   more=.TRUE.
  else
   more=.FALSE.
  end if 

 end do

end subroutine Get3DQCAsymPos


!--------------------------------------------------------------------------
!
! SUBROUTINE: Get2DQCAsymPos
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read the atom coordinates from standard input
!
!> @details ask the user for the atom type, coordinates, site occupation parameter
!> and Debye-Waller parameter for each atom type.
!
!> @param 2D QC unit cell pointer
!!
!> @date   05/23/18 SS 1.0 original, adapted from symmetry module
!--------------------------------------------------------------------------
recursive subroutine Get2DQCAsymPos(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get2DQCAsymPos

use io
use crystal, only: DisplayElements

IMPLICIT NONE

type(TDQCStructureType),pointer         :: TDQCcell

logical                                 :: more                 !< logical to determine if more atoms need to be entered
character(1)                            :: ans, list(256)       !< used for IO
real(kind=sgl)                          :: pt(10), out_real(10)   !< used to read and write asymmetric position data
integer(kind=irg)                       :: j, io_int(1) , std   !< auxiliary variables

 
 more=.TRUE.
 TDQCcell%ATOM_ntype = 0
 call Message(' Enter atoms in asymmetric unit ', frm = "(/A)")
 call DisplayElements()

 do while (more)
  TDQCcell%ATOM_ntype = TDQCcell%ATOM_ntype + 1

! atomic number
  call ReadValue(' ->  Atomic number : ', io_int, 1)
  TDQCcell%ATOM_type(TDQCcell%ATOM_ntype) = io_int(1)

! general atom coordinate
  list = (/ (' ',j=1,256) /)
  call Message(' ->  Fractional coordinates, site occupation, Bpar_11 [nm^2],'&
  ' Bpar_33 [nm^2], Bperp [nm^2], radial atomic size (fraction of a_i) : ',&
  frm = "(A,' ')",advance="no")
  read (*,"(256A)") list

! interpret this string and extract coordinates and such ...
  call extractposition(list,pt) 
  
! store in the appropriate component of the cell variable  
  TDQCcell%ATOM_pos(TDQCcell%ATOM_ntype,1:10) = pt(1:10)

! and write the coordinate back to the terminal  
  out_real = (/ (TDQCcell%ATOM_pos(TDQCcell%ATOM_ntype,j),j=1,10) /)
  call WriteValue('    -> ', out_real, 10, frm = "(1x,6(F10.7,2x),3(F10.7,2x),F10.7)") 

  call ReadValue(' ->  Another atom ? (y/n) ', ans, frm = "(A1)")
  if ((ans.eq.'y').or.(ans.eq.'Y')) then 
   more=.TRUE.
  else
   more=.FALSE.
  end if 

 end do

end subroutine Get2DQCAsymPos

!--------------------------------------------------------------------------
!
! SUBROUTINE: Generate3DQCSymmetry
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute all relevant symmetry operators
!
!> @details compute all symmetry operators and store them in QCcell%SG%SYM_data.
!               
!> @note These routines are based on a program written by G. Ceder (MIT).
!
!> @param QCcell QC unit cell pointer
!
!> @date  05/23/18 SS 1.0 original, adapted from symmetry module 
!--------------------------------------------------------------------------
recursive subroutine Generate3DQCSymmetry(QCcell, dopg)
!DEC$ ATTRIBUTES DLLEXPORT :: Generate3DQCSymmetry

use io

IMPLICIT NONE

type(QCStructureType),pointer             :: QCcell
logical,INTENT(IN)                        :: dopg

integer(kind=irg)                         :: i,j,k,nsym,k1,k2,l1,l2,sg(4)       !< loop counters (mostly)
real(kind=dbl)                            :: q,sm, eps                         !< auxiliary variables.

eps = 0.1D0

! create the space group generator matrices
 call MakeQCGenerators(QCcell)

sg = (/5,6,10,11/)
do k = 1,4
 if(QCcell%SYM_SGnum .eq. sg(k) ) then
 	call Message('--> Face-centered Icosahedral quasicrystal detected. Generating space group symmetry. This will take a while...')
 end if
end do

 nsym = QCcell%SG%SYM_GENnum

! generate new elements from the squares of the generators 
 do k=1,QCcell%SG%SYM_GENnum 
  call matrixmult(QCcell,k,k)
  if (isitnew(QCcell,nsym).eqv..TRUE.) then 
   nsym=nsym+1
   QCcell%SG%SYM_data(nsym,:,:) = QCcell%SG%SYM_c(:,:)
  end if
 end do

! generate the remainder of the factorgroup
 k1=1
 do while (k1.le.nsym) 
  k2=k1+1
  do while (k2.le.nsym)
   call matrixmult(QCcell,k2,k1)
   if (isitnew(QCcell,nsym).eqv..TRUE.) then 
    nsym=nsym+1
    QCcell%SG%SYM_data(nsym,:,:) = QCcell%SG%SYM_c(:,:)
   end if
   k2=k2+1
  end do
  k1=k1+1
 end do
 QCcell%SG%SYM_MATnum = nsym

! reduce the translation operators to the fundamental unit cell
 do i=1,QCcell%SG%SYM_MATnum
   QCcell%SG%SYM_data(i,6,7)=mod( QCcell%SG%SYM_data(i,6,7),1.0_dbl)
 end do 

! generate point group symmetry if flag is passed
if(dopg) then
  QCcell%SG%SYM_NUMpt = 0
  do i = 1,QCcell%SG%SYM_MATnum
  	sm = 0.D0
    do j = 1,6
      sm = sm + QCcell%SG%SYM_data(i,j,7)**2
    end do
    if(sm .lt. eps) then
      QCcell%SG%SYM_NUMpt = QCcell%SG%SYM_NUMpt + 1
      QCcell%SG%SYM_direc(QCcell%SG%SYM_NUMpt,1:6,1:6) = QCcell%SG%SYM_data(i,1:6,1:6)
    end if
  end do
end if

end subroutine Generate3DQCSymmetry

!--------------------------------------------------------------------------
!
! SUBROUTINE: Generate2DQCSymmetry
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute all relevant symmetry operators
!
!> @details compute all symmetry operators and store them in TDQCcell%SG%SYM_data.
!               
!> @note These routines are based on a program written by G. Ceder (MIT).
!
!> @param QCcell 2DQC unit cell pointer
!
!> @date  05/23/18 SS 1.0 original, adapted from symmetry module 
!--------------------------------------------------------------------------
recursive subroutine Generate2DQCSymmetry(TDQCcell, dopg)
!DEC$ ATTRIBUTES DLLEXPORT :: Generate2DQCSymmetry

IMPLICIT NONE

type(TDQCStructureType),pointer           :: TDQCcell
logical,INTENT(IN)                        :: dopg

integer(kind=irg)                         :: i,j,k,nsym,k1,k2,l1,l2       !< loop counters (mostly)
real(kind=dbl)                            :: q,sm, eps                         !< auxiliary variables.

eps = 0.1D0

! create the space group generator matrices
 call MakeQCGenerators(TDQCcell)
 nsym = TDQCcell%SG%SYM_GENnum

! generate new elements from the squares of the generators 
 do k=1,TDQCcell%SG%SYM_GENnum 
  call matrixmult(TDQCcell,k,k)
  if (isitnew(TDQCcell,nsym).eqv..TRUE.) then 
   nsym=nsym+1
   TDQCcell%SG%SYM_data(nsym,:,:) = TDQCcell%SG%SYM_c(:,:)
  end if
 end do

! generate the remainder of the factorgroup
 k1=1
 do while (k1.le.nsym) 
  k2=k1+1
  do while (k2.le.nsym)
   call matrixmult(TDQCcell,k2,k1)
   if (isitnew(TDQCcell,nsym).eqv..TRUE.) then 
    nsym=nsym+1
    TDQCcell%SG%SYM_data(nsym,:,:) = TDQCcell%SG%SYM_c(:,:)
   end if
   k2=k2+1
  end do
  k1=k1+1
 end do
 TDQCcell%SG%SYM_MATnum = nsym

! reduce the translation operators to the fundamental unit cell
 do i=1,TDQCcell%SG%SYM_MATnum
   TDQCcell%SG%SYM_data(i,5,6)=mod( TDQCcell%SG%SYM_data(i,5,6),1.0_dbl)
 end do 

! generate point group symmetry if flag is passed
if(dopg) then
  TDQCcell%SG%SYM_NUMpt = 0
  do i = 1,TDQCcell%SG%SYM_MATnum
  	sm = 0.D0
    do j = 1,5
      sm = sm + TDQCcell%SG%SYM_data(i,j,6)**2
    end do
    if(sm .lt. eps) then
      TDQCcell%SG%SYM_NUMpt = TDQCcell%SG%SYM_NUMpt + 1
      TDQCcell%SG%SYM_direc(TDQCcell%SG%SYM_NUMpt,1:5,1:5) = TDQCcell%SG%SYM_data(i,1:5,1:5)
    end if
  end do
end if

end subroutine Generate2DQCSymmetry

!--------------------------------------------------------------------------
!
! SUBROUTINE: Make3DQCGenerators
!
!> @author Saransh, Carnegie Mellon University
!
!> @brief get all generators for the 3D QC
!
!> @details fill all generators
!               
!> @param QCcell 3DQC unit cell pointer
!
!> @date  05/23/18 SS 1.0 original, adapted from symmetry module 
!--------------------------------------------------------------------------
recursive subroutine Make3DQCGenerators(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: Make3DQCGenerators

IMPLICIT NONE

type(QCStructureType), pointer              :: cell
real(kind=dbl)                              :: g1(6,6), g2(6,6), g3(6,6)
character(100)                   			:: genst                        !< full generator string
character(1)								:: t(7), ngen
integer(kind=irg)							:: i, k, l

genst = trim(cell%SG%SYM_GL(cell%SYM_SGnum))

ngen = genst(1:1)

read(ngen,*) cell%SG%SYM_GENnum

! create the generator matrices 
do i = 1,cell%SG%SYM_GENnum
	do  k = 1,7
    	l = 1 + 7*(i-1) + k
     	t(k) = genst(l:l)
   	end do
   	call SYM_fillgen_QC(cell,t)
   	cell%SG%SYM_data(i,:,:) = cell%SG%SYM_c(:,:)
 end do

! 5-fold symmetry
!g1(1,1:6) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
!g1(2,1:6) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
!g1(3,1:6) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
!g1(4,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
!g1(5,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)
!g1(6,1:6) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)

! 3-fold symmetry
!g2(1,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)
!g2(2,1:6) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
!g2(3,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
!g2(4,1:6) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
!g2(5,1:6) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
!g2(6,1:6) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)

! inversion symmetry
!g3(1,1:6) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
!g3(2,1:6) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
!g3(3,1:6) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
!g3(4,1:6) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
!g3(5,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
!g3(6,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)


!QCcell%SG%SYM_data(1,1:7,1:7) = 0.D0
!QCcell%SG%SYM_data(1,7,7)     = 1.D0

!QCcell%SG%SYM_data(1,1:6,1:6) = g1(1:6,1:6)
!QCcell%SG%SYM_data(1,6,7)     = 0.5D0

! generator h (primary m) + translation
!QCcell%SG%SYM_data(2,1:7,1:7) = 0.D0
!QCcell%SG%SYM_data(2,7,7)     = 1.D0

!QCcell%SG%SYM_data(2,1:6,1:6) = g2(1:6,1:6)
!QCcell%SG%SYM_data(2,5,6)     = 0.5D0

! generator m (secondary m)
!QCcell%SG%SYM_data(3,1:7,1:7) = 0.D0
!QCcell%SG%SYM_data(3,7,7)     = 1.D0

!QCcell%SG%SYM_data(3,1:6,1:6) = g3(1:6,1:6)

!QCcell%SG%SYM_GENnum = 3

end subroutine Make3DQCGenerators
!--------------------------------------------------------------------------
!
! SUBROUTINE: MakeTDQCGenerators
!
!> @author Saransh, Carnegie Mellon University
!
!> @brief get all generators for the 2D QC
!
!> @details fill all generators
!               
!> @param QCcell 2DQC unit cell pointer
!
!> @date  05/23/18 SS 1.0 original, adapted from symmetry module 
!--------------------------------------------------------------------------
!DEC$ ATTRIBUTES DLLEXPORT :: MakeTDQCGenerators
recursive subroutine MakeTDQCGenerators(TDQCcell)

IMPLICIT NONE

type(TDQCStructureType), pointer            :: TDQCcell
real(kind=dbl)                              :: g1(5,5), g2(5,5), g3(5,5)

! g1(1,1:5) = (/0.D0, 1.D0, -1.D0, 0.D0, 0.D0/)
! g1(2,1:5) = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
! g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(4,1:5) = (/-1.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g2(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g2(2,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
! g2(3,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
! g2(4,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g3(1,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g3(2,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g3(3,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
! g3(4,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
! g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)
if(trim(TDQCcell%QCtype) .eq. 'Dec') then
  g1(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
  g1(2,1:5) = (/1.D0, 1.D0, 1.D0, 1.D0, 0.D0/)
  g1(3,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g1(4,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
  g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  g2(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
  g2(2,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
  g2(3,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
  g2(4,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  g3(1,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g3(2,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
  g3(3,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
  g3(4,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
  g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)

  ! generator r^1/2
  TDQCcell%SG%SYM_data(1,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(1,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(1,1:5,1:5) = g1(1:5,1:5)
  TDQCcell%SG%SYM_data(1,5,6)     = 0.5D0

  ! generator h (primary m) + translation
  TDQCcell%SG%SYM_data(2,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(2,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(2,1:5,1:5) = g2(1:5,1:5)
  TDQCcell%SG%SYM_data(2,5,6)     = 0.5D0

  ! generator m (secondary m)
  TDQCcell%SG%SYM_data(3,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(3,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(3,1:5,1:5) = g3(1:5,1:5)

else if(trim(TDQCcell%QCtype) .eq. 'Oct') then
  g1(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
  g1(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
  g1(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
  g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  g2(1,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
  g2(2,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
  g2(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
  g2(4,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)

  g3(1,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g3(2,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
  g3(3,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
  g3(4,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
  g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)

  TDQCcell%SG%SYM_data(1,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(1,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(1,1:5,1:5) = g1(1:5,1:5)
  TDQCcell%SG%SYM_data(1,5,6)     = 0.5D0

  ! generator h (primary m) + translation
  TDQCcell%SG%SYM_data(2,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(2,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(2,1:5,1:5) = g2(1:5,1:5)
  TDQCcell%SG%SYM_data(2,5,6)     = 0.5D0

  ! generator m (secondary m)
  TDQCcell%SG%SYM_data(3,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(3,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(3,1:5,1:5) = g3(1:5,1:5)

else if(trim(TDQCcell%QCtype) .eq. 'DoD') then

  g1(1,1:5) = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
  g1(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
  g1(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
  g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  g2(1,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
  g2(2,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
  g2(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
  g2(4,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  g3(1,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
  g3(2,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
  g3(3,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
  g3(4,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
  g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)

  TDQCcell%SG%SYM_data(1,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(1,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(1,1:5,1:5) = g1(1:5,1:5)
  TDQCcell%SG%SYM_data(1,5,6)     = 0.0D0

  ! generator h (primary m) + translation
  TDQCcell%SG%SYM_data(2,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(2,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(2,1:5,1:5) = g2(1:5,1:5)
  TDQCcell%SG%SYM_data(2,5,6)     = 0.0D0

  ! generator m (secondary m)
  TDQCcell%SG%SYM_data(3,1:6,1:6) = 0.D0
  TDQCcell%SG%SYM_data(3,6,6)     = 1.D0

  TDQCcell%SG%SYM_data(3,1:5,1:5) = g3(1:5,1:5)

end if

TDQCcell%SG%SYM_GENnum = 3

end subroutine MakeTDQCGenerators

!--------------------------------------------------------------------------
!
! FUNCTION: isnew2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief check if its a new symmetry operator for 2D QC
!
!> @param  TDQCcell cell structure
!> @param  sym    symmetry operator to check
!> @param  nsym   number of independent symmetry operators already found
!
!> @date   03/21/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive function isnew2DQC(SYM_icos, sym, nsym) result(new_sym)
!DEC$ ATTRIBUTES DLLEXPORT :: isnew2DQC

use typedefs
use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)             :: SYM_icos(5,5,40)
real(kind=dbl),INTENT(IN)             :: sym(5,5)
integer(kind=irg),INTENT(IN)          :: nsym

integer(kind=irg)                     :: ii, jj
real(kind=dbl)                        :: eps = 1.0D-4
logical                               :: new_sym

new_sym = .TRUE.

do ii = 1,nsym
  if(sum(abs(SYM_icos(:,:,ii) - sym)) .lt. eps) then
    new_sym = .FALSE.
    EXIT
  end if
end do

end function isnew2DQC

!--------------------------------------------------------------------------
!
! FUNCTION: isnew3DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief check if its a new symmetry operator 
!
!> @param  QCcell cell structure
!> @param  sym    symmetry operator to check
!> @param  nsym   number of independent symmetry operators already found
!
!> @date   02/05/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function isnew3DQC(SYM_icos, sym, nsym) result(new_sym)
!DEC$ ATTRIBUTES DLLEXPORT :: isnew3DQC

use typedefs
use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)             :: SYM_icos(6,6,120)
real(kind=dbl),INTENT(IN)             :: sym(6,6)
integer(kind=irg),INTENT(IN)          :: nsym

integer(kind=irg)                     :: ii, jj
real(kind=dbl)                        :: eps = 1.0D-4
logical                               :: new_sym
new_sym = .TRUE.

do ii = 1,nsym
  if(sum(abs(SYM_icos(:,:,ii) - sym)) .lt. eps) then
    new_sym = .FALSE.
    EXIT
  end if
end do

end function isnew3DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: isnewvector2DQC_int
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief is this a new vector not on the list already?
!
!> @param  hkl    input reciprocal lattice vector to check
!> @param  orbit  list of exisitng vectors
!> @param  nn     number of symmetrically equivalent vectors found until now
!
!> @date   02/07/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from  QCmod to qcrystal
!--------------------------------------------------------------------------
recursive function isnewvector2DQC_int(QCcell, hkl, orbit, nn) result(isnew)
!DEC$ ATTRIBUTES DLLEXPORT :: isnewvector2DQC_int

use local

IMPLICIT NONE

type(TDQCStructureType), pointer      :: QCcell
real(kind=dbl),INTENT(IN)             :: hkl(5)
integer(kind=irg),INTENT(IN)          :: orbit(QCcell%SG%SYM_MATnum,5)
integer(kind=irg),INTENT(IN)          :: nn

logical                               :: isnew
integer(kind=irg)                     :: ii
real(kind=dbl),parameter              :: eps = 1.0D-6

isnew = .TRUE.

do ii = 1,nn
  if(sum(abs(hkl - dble(orbit(ii,1:5)))) .lt. eps) then
    isnew = .FALSE.
    EXIT
  end if
end do

end function isnewvector2DQC_int

!--------------------------------------------------------------------------
!
! SUBROUTINE: isnewvector2DQC_dbl
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief is this a new vector not on the list already?
!
!> @param  hkl    input reciprocal lattice vector to check
!> @param  orbit  list of exisitng vectors
!> @param  nn     number of symmetrically equivalent vectors found until now
!
!> @date   02/07/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive function isnewvector2DQC_dbl(QCcell, hkl, orbit, nn) result(isnew)
!DEC$ ATTRIBUTES DLLEXPORT :: isnewvector2DQC_dbl

use local

IMPLICIT NONE

type(TDQCStructureType), pointer      :: QCcell
real(kind=dbl),INTENT(IN)             :: hkl(5)
real(kind=dbl),INTENT(IN)             :: orbit(QCcell%SG%SYM_MATnum,5)
integer(kind=irg),INTENT(IN)          :: nn

logical                               :: isnew
integer(kind=irg)                     :: ii
real(kind=dbl),parameter              :: eps = 1.0D-6

isnew = .TRUE.

do ii = 1,nn
  if(sum(abs(hkl - dble(orbit(ii,1:5)))) .lt. eps) then
    isnew = .FALSE.
    EXIT
  end if
end do

end function isnewvector2DQC_dbl

!--------------------------------------------------------------------------
!
! SUBROUTINE: isnewvector3DQC_int
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief is this a new vector not on the list already?
!
!> @param  hkl    input reciprocal lattice vector to check
!> @param  orbit  list of exisitng vectors
!> @param  nn     number of symmetrically equivalent vectors found until now
!
!> @date   02/07/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive function isnewvector3DQC_int(QCcell, hkl, orbit, nn) result(isnew)
!DEC$ ATTRIBUTES DLLEXPORT :: isnewvector3DQC_int

use local

IMPLICIT NONE

type(QCStructureType), pointer        :: QCcell
real(kind=dbl),INTENT(IN)             :: hkl(6)
integer(kind=irg),INTENT(IN)          :: orbit(QCcell%SG%SYM_MATnum,6)
integer(kind=irg),INTENT(IN)          :: nn

logical                               :: isnew
integer(kind=irg)                     :: ii
real(kind=dbl),parameter              :: eps = 1.0D-6

isnew = .TRUE.

do ii = 1,nn
  if(sum(abs(hkl - dble(orbit(ii,1:6)))) .lt. eps) then
    isnew = .FALSE.
    EXIT
  end if
end do

end function isnewvector3DQC_int

!--------------------------------------------------------------------------
!
! SUBROUTINE: isnewvector3DQC_dbl
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief is this a new vector not on the list already?
!
!> @param  hkl    input reciprocal lattice vector to check
!> @param  orbit  list of exisitng vectors
!> @param  nn     number of symmetrically equivalent vectors found until now
!
!> @date   02/07/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive function isnewvector3DQC_dbl(QCcell, hkl, orbit, nn) result(isnew)
!DEC$ ATTRIBUTES DLLEXPORT :: isnewvector3DQC_dbl

use local

IMPLICIT NONE

type(QCStructureType), pointer        :: QCcell
real(kind=dbl),INTENT(IN)             :: hkl(6)
real(kind=dbl),INTENT(IN)             :: orbit(QCcell%SG%SYM_MATnum,6)
integer(kind=irg),INTENT(IN)          :: nn

logical                               :: isnew
integer(kind=irg)                     :: ii
real(kind=dbl),parameter              :: eps = 1.0D-6

isnew = .TRUE.

do ii = 1,nn
  if(sum(abs(hkl - orbit(ii,1:6))) .lt. eps) then
    isnew = .FALSE.
    EXIT
  end if
end do

end function isnewvector3DQC_dbl

!--------------------------------------------------------------------------
!
! FUNCTION: MatrixPower2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief raise 4x4 matrix to positive integer power, A^n (n >= 1)
!
!> @param  A    matrix
!> @param  n    exponent
!
!> @date   03/21/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive function MatrixPower2DQC(A, n) result(B)
!DEC$ ATTRIBUTES DLLEXPORT :: MatrixPower2DQC

use local
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)             :: A(5,5)
integer(kind=irg),INTENT(IN)          :: n
real(kind=dbl)                        :: B(5,5), tmp(5,5)
integer(kind=irg)                     :: ii

if(n .eq. 0) then

  B = 0.D0
  do ii = 1,5
    B(ii,ii) = 1.D0
  end do

else if(n .eq. 1) then

  B = A

else if(n .gt. 1) then

  tmp = A
  do ii = 1,n-1
    tmp = matmul(A, tmp)
  end do
  B = tmp

else

  call FatalError('MatrixPower:','Exponent n should be greater that or equal to 0.')

end if

end function MatrixPower2DQC

!--------------------------------------------------------------------------
!
! FUNCTION: MatrixPower
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief raise 6x6 matrix to positive integer power, A^n (n >= 1)
!
!> @param  A    matrix
!> @param  n    exponent
!
!> @date   02/05/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function MatrixPower3DQC(A, n) result(B)
!DEC$ ATTRIBUTES DLLEXPORT :: MatrixPower3DQC

use local
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)             :: A(6,6)
integer(kind=irg),INTENT(IN)          :: n
real(kind=dbl)                        :: B(6,6), tmp(6,6)
integer(kind=irg)                     :: ii

if(n .eq. 0) then

  B = 0.D0
  do ii = 1,6
    B(ii,ii) = 1.D0
  end do

else if(n .eq. 1) then

  B = A

else if(n .gt. 1) then

  tmp = A
  do ii = 1,n-1
    tmp = matmul(A, tmp)
  end do
  B = tmp

else

  call FatalError('MatrixPower:','Exponent n should be greater that or equal to 0.')

end if

end function MatrixPower3DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSymmetryOperatorsDoDecahedral
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate all 60 symmetry operators for point group D24
!> using the generators in 6 index notation. The generatos are borrowed from:
!> F. Gahler, CRYSTALLOGRAPHY OF DODECAGONAL QUASICRYSTALS
!
!> @param  TDQCcell 2-D QCcell structure
!
!> @date   03/21/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine GetSymmetryOperatorsDoDecahedral(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSymmetryOperatorsDoDecahedral

use typedefs
use local

IMPLICIT NONE

type(TDQCStructureType), pointer      :: TDQCcell

real(kind=dbl)                        :: g1(5,5), g2(5,5) !two generators 
real(kind=dbl)                        :: O1(5,5,12), O2(5,5,2), O(5,5)
integer(kind=irg)                     :: ii, jj, kk, nsym, nsym2


g1(1,1:5) = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
g1(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g1(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

g2(1,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
g2(2,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
g2(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g2(4,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g1(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g1(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 1.D0, 0.D0/)
! g1(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
! g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g2(1,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g2(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g2(3,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
! g2(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
! g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! generate all powers of the matrix
do ii = 1,12
  O1(:,:,ii) = MatrixPower2DQC(g1,ii)
  if(ii .le. 2) then
    O2(:,:,ii) = MatrixPower2DQC(g2,ii)
  end if
end do

! combine the rotations and mirrors to generate all 24 operators
! START WITH IDENTITY OPERATOR
nsym = 1
TDQCcell%SYM_icos = 0.D0
do ii = 1,5
  TDQCcell%SYM_icos(ii,ii,1) = 1.D0
end do

do ii = 1,12
  O = O1(:,:,ii)
  if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
  end if
end do

do ii = 1,2
  O = O2(:,:,ii)
  if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
  end if
end do

! go through all combinations two at a time (2 possible configurations)
do ii = 1,12
  do jj = 1,2
    O = matmul(O1(:,:,ii), O2(:,:,jj))
    if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
    end if

    O = matmul(O2(:,:,jj), O1(:,:,ii))
    if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
    end if

  end do
end do

TDQCcell%nsym = nsym

end subroutine GetSymmetryOperatorsDoDecahedral

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSymmetryOperatorsDecagonal
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate all 40 symmetry operators for point group 10/mmm
!> using the generators in 5 index notation. The generatos are borrowed from:
!> Decagonal quasicrystals higher dimensional description & structure determination

!
!> @param  TDQCcell 2-D QCcell structure
!
!> @date   03/21/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine GetSymmetryOperatorsDecagonal(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSymmetryOperatorsDecagonal

use typedefs
use local

IMPLICIT NONE

type(TDQCStructureType), pointer      :: TDQCcell

real(kind=dbl)                        :: g1(5,5), g2(5,5), g3(5,5) ! three generators 
real(kind=dbl)                        :: O1(5,5,10), O2(5,5,2), O3(5,5,2), O(5,5)
integer(kind=irg)                     :: ii, jj, kk, nsym, nsym2

! g1(1,1:5) = (/0.D0, 1.D0, -1.D0, 0.D0, 0.D0/)
! g1(2,1:5) = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
! g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(4,1:5) = (/-1.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g2(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g2(2,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
! g2(3,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
! g2(4,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g3(1,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g3(2,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
! g3(3,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
! g3(4,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)

! g1(1,:) = (/0.D0,  0.D0,  0.D0, 1.D0, 0.D0/)
! g1(2,:) = (/-1.D0, 0.D0,  0.D0, 1.D0, 0.D0/)
! g1(3,:) = (/0.D0, -1.D0,  0.D0, 1.D0, 0.D0/)
! g1(4,:) = (/0.D0,  0.D0, -1.D0, 1.D0, 0.D0/)
! g1(5,:) = (/0.D0,  0.D0,  0.D0, 0.D0, 1.D0/)

! g1(1,1:5) = (/0.D0, 1.D0, -1.D0, 0.D0, 0.D0/)
! g1(2,1:5) = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
! g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(4,1:5) = (/-1.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g2(1,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
! g2(2,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
! g2(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g2(4,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g3(1,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g3(2,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
! g3(3,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
! g3(4,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)

!g1 = transpose(g1)
!g2 = transpose(g2)
! g3 = transpose(g3)

g1(1,:) = (/0.D0,-1.D0,1.D0,0.D0,0.D0/)
g1(2,:) = (/0.D0,-1.D0,0.D0,1.D0,0.D0/)
g1(3,:) = (/0.D0,-1.D0,0.D0,0.D0,0.D0/)
g1(4,:) = (/1.D0,-1.D0,0.D0,0.D0,0.D0/)
g1(5,:) = (/0.D0,0.D0,0.D0,0.D0,1.D0/)


! generate all powers of the matrix
do ii = 1,4
  O1(:,:,ii) = MatrixPower2DQC(g1,ii)
end do

! combine the rotations and mirrors to generate all 24 operators
! START WITH IDENTITY OPERATOR
nsym = 1
TDQCcell%SYM_icos = 0.D0
do ii = 1,5
  TDQCcell%SYM_icos(ii,ii,1) = 1.D0
end do

do ii = 1,4
  O = O1(:,:,ii)
  if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
  end if
end do

! do ii = 1,2
!   O = O2(:,:,ii)
!   if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
!     nsym = nsym + 1
!     TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
!   end if
! end do

! ! go through all combinations two at a time (2 possible configurations)
! do ii = 1,10
!   do jj = 1,2
!     O = matmul(O1(:,:,ii), O2(:,:,jj))
!     if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
!       nsym = nsym + 1
!       TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
!     end if

!     O = matmul(O2(:,:,jj), O1(:,:,ii))
!     if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
!       nsym = nsym + 1
!       TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
!     end if

!   end do
! end do

TDQCcell%nsym = nsym

end subroutine GetSymmetryOperatorsDecagonal

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSymmetryOperatorsDoDecahedral
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate all 24 symmetry operators for point group D24
!> using the generators in 6 index notation. The generatos are borrowed from:
!> F. Gahler, CRYSTALLOGRAPHY OF DODECAGONAL QUASICRYSTALS
!
!> @param  TDQCcell 2-D QCcell structure
!
!> @date   03/21/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine GetSymmetryOperatorsOctagonal(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSymmetryOperatorsOctagonal

use typedefs
use local

IMPLICIT NONE

type(TDQCStructureType), pointer      :: TDQCcell

real(kind=dbl)                        :: g1(5,5), g2(5,5) !two generators 
real(kind=dbl)                        :: O1(5,5,12), O2(5,5,2), O(5,5)
integer(kind=irg)                     :: ii, jj, kk, nsym, nsym2

! g1(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g1(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g1(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
! g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! g2(1,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
! g2(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
! g2(3,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
! g2(4,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
! g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

g1(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
g1(2,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g1(4,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

g2(1,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
g2(2,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
g2(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g2(4,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

! generate all powers of the matrix
do ii = 1,8
  O1(:,:,ii) = MatrixPower2DQC(g1,ii)
  if(ii .le. 2) then
    O2(:,:,ii) = MatrixPower2DQC(g2,ii)
  end if
end do

! combine the rotations and mirrors to generate all 24 operators
! START WITH IDENTITY OPERATOR
nsym = 1
TDQCcell%SYM_icos = 0.D0
do ii = 1,5
  TDQCcell%SYM_icos(ii,ii,1) = 1.D0
end do

do ii = 1,8
  O = O1(:,:,ii)
  if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
  end if
end do

do ii = 1,2
  O = O2(:,:,ii)
  if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
  end if
end do

! go through all combinations two at a time (2 possible configurations)
do ii = 1,8
  do jj = 1,2
    O = matmul(O1(:,:,ii), O2(:,:,jj))
    if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
    end if

    O = matmul(O2(:,:,jj), O1(:,:,ii))
    if(isnew2DQC(TDQCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      TDQCcell%SYM_icos(:,:,nsym) = O(1:5,1:5)
    end if

  end do
end do

TDQCcell%nsym = nsym

end subroutine GetSymmetryOperatorsOctagonal

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSymmetryOperatorsIcosahedral
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate all 60 symmetry operators for point group 532
!> using the generators in 6 index notation. The generatos are borrowed from:
!> T. Janssen, Crystallography of Quasi-Crystals, Acta Cryst. (1986). A42, 261-271
!
!> @param  QCcell cell structure
!
!> @date   02/05/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetSymmetryOperatorsIcosahedral(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSymmetryOperatorsIcosahedral

use typedefs
use local

IMPLICIT NONE

type(QCStructureType), pointer        :: QCcell

real(kind=dbl)                        :: g1(6,6), g2(6,6), g3(6,6) !three generators 
real(kind=dbl)                        :: O1(6,6,5), O2(6,6,6), O3(6,6,2), O(6,6)
integer(kind=irg)                     :: ii, jj, kk, nsym, nsym2

g1(1,:) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g1(2,:) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g1(3,:) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
g1(4,:) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
g1(5,:) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)
g1(6,:) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)

g2(1,:) = (/0.D0, 0.D0, 0.D0,  0.D0, 0.D0, 1.D0/)
g2(2,:) = (/1.D0, 0.D0, 0.D0,  0.D0, 0.D0, 0.D0/)
g2(3,:) = (/0.D0, 0.D0, 0.D0,  0.D0, 1.D0, 0.D0/)
g2(4,:) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
g2(5,:) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
g2(6,:) = (/0.D0, 1.D0, 0.D0,  0.D0, 0.D0, 0.D0/)

! Inversion operator
g3 = 0.D0 
do ii = 1,6
  g3(ii,ii) = -1.D0
end do

do ii = 1,5
  O1(:,:,ii) = MatrixPower3DQC(G1, ii)
end do

do ii = 1,3
  O2(:,:,ii)  = MatrixPower3DQC(G2, ii)
end do

do ii = 1,2
  O3(:,:,ii) = MatrixPower3DQC(G3, ii)
end do

! START WITH IDENTITY OPERATOR
nsym = 1
QCcell%SYM_icos = 0.D0
do ii = 1,6
  QCcell%SYM_icos(ii,ii,1) = 1.D0
end do

! go through all combinations one at a time (2 possible configurations)

do ii = 1,5
  O = O1(:,:,ii)
  if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
  end if
end do

do ii = 1,3
  O = O2(:,:,ii)
  if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
    nsym = nsym + 1
    QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
  end if
end do

! go through all combinations two at a time (2 possible configurations)
do ii = 1,5
  do jj = 1,3
    O = matmul(O1(:,:,ii), O2(:,:,jj))
    if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
    end if

    O = matmul(O2(:,:,jj), O1(:,:,ii))
    if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
    end if
  end do
end do


! now multiply all the genrators with the already existing symmetry operators
! in the SYM_icos matrix
nsym2 = nsym
do ii = 1,5
  do jj = 1,nsym2
    O = matmul(O1(:,:,ii),QCcell%SYM_icos(:,:,jj))
    if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

      O = matmul(QCcell%SYM_icos(:,:,jj), O1(:,:,ii))
      if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

  end do
end do

nsym2 = nsym
do ii = 1,3
  do jj = 1,nsym2
    O = matmul(O2(:,:,ii),QCcell%SYM_icos(:,:,jj))
    if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

      O = matmul(QCcell%SYM_icos(:,:,jj), O2(:,:,ii))
      if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

  end do
end do

nsym2 = nsym
do ii = 1,5
  do jj = 1,nsym2
    O = matmul(O1(:,:,ii),QCcell%SYM_icos(:,:,jj))
    if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

      O = matmul(QCcell%SYM_icos(:,:,jj), O1(:,:,ii))
      if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

  end do
end do

nsym2 = nsym
do ii = 1,3
  do jj = 1,nsym2
    O = matmul(O2(:,:,ii),QCcell%SYM_icos(:,:,jj))
    if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

      O = matmul(QCcell%SYM_icos(:,:,jj), O2(:,:,ii))
      if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
        nsym = nsym + 1
        QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
      end if

  end do
end do

nsym2 = nsym
do ii = 1,nsym2
  O = matmul(g3,QCcell%SYM_icos(:,:,ii))
  if(isnew3DQC(QCcell%SYM_icos, O, nsym)) then
      nsym = nsym + 1
      QCcell%SYM_icos(:,:,nsym) = O(1:6,1:6)
  end if
end do

end subroutine GetSymmetryOperatorsIcosahedral

!--------------------------------------------------------------------------
!
! FUNCTION: Get2DQCPGsymmetry
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute and fill point group symmetry
!
!> @param QCcell cell pointer
!
!> @date   05/23/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine Get2DQCPGsymmetry(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get2DQCPGsymmetry

use error

IMPLICIT NONE

type(TDQCStructureType),pointer :: QCcell
character(fnlen)                :: QCtype

QCtype = trim(QCcell%QCtype)

if(trim(QCtype) .eq. 'DoD') then
  call GetSymmetryOperatorsDoDecahedral(QCcell)

else if(trim(QCtype) .eq. 'Dec') then
  call GetSymmetryOperatorsDecagonal(QCcell)

else if(trim(QCtype) .eq. 'Oct') then
  !call FatalError('Initialize_QCCell:','Octagonal symmetry not implemented yet.')
  call GetSymmetryOperatorsOctagonal(QCcell)
else
  call FatalError('Initialize_QCCell:','unknown symmetry of Quasi-crystal.')

end if

end subroutine Get2DQCPGsymmetry

!--------------------------------------------------------------------------
!
! FUNCTION: Get3DQCPGsymmetry
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute and fill point group symmetry
!
!> @param QCcell cell pointer
!
!> @date   05/23/18 SS 1.0 original
!> @date   05/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine Get3DQCPGsymmetry(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Get3DQCPGsymmetry

use error

IMPLICIT NONE

type(QCStructureType),pointer 	:: QCcell

call GetSymmetryOperatorsIcosahedral(QCcell)

end subroutine Get3DQCPGsymmetry

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
recursive subroutine matrixmult2DQC(cell, k1, k2)
!DEC$ ATTRIBUTES DLLEXPORT :: matrixmult2DQC
   
IMPLICIT NONE

type(TDQCStructureType),pointer  :: cell

integer(kind=irg),INTENT(IN)     :: k1                           !< index of first 6x6 input matrix
integer(kind=irg),INTENT(IN)     :: k2                           !< index of second 6x6 input matrix
integer(kind=irg)                :: i,j,k                        !< loop counters
real(kind=dbl),parameter         :: eps=0.0005_dbl               !< truncation constant

 do i=1,6
  do j=1,6
   cell%SG%SYM_c(i,j) = 0.0_dbl
   do k=1,6
    cell%SG%SYM_c(i,j)=cell%SG%SYM_c(i,j)+cell%SG%SYM_data(k1,i,k)*cell%SG%SYM_data(k2,k,j)
   end do
  end do
 end do

! bring the translational part of the matrix back to
! the first unit cell and correct possible rounding errors
! only a_5 is checked since the other are always 0
  if (abs(cell%SG%SYM_c(5,6)).lt.eps) cell%SG%SYM_c(5,6)=0.0_dbl
  if (cell%SG%SYM_c(5,6).lt.0.0_dbl) then 
   do while (cell%SG%SYM_c(5,6).lt.0.0_dbl) 
    cell%SG%SYM_c(5,6)=cell%SG%SYM_c(5,6)+1.0_dbl
   end do
  end if
  if (cell%SG%SYM_c(5,6).gt.1.0_dbl) then 
   do while (cell%SG%SYM_c(5,6).gt.1.0_dbl) 
    cell%SG%SYM_c(5,6)=cell%SG%SYM_c(5,6)-1.0_dbl
   end do
  end if
  if (abs(cell%SG%SYM_c(5,6)-1.0_dbl).lt.eps) cell%SG%SYM_c(5,6)=0.0_dbl

end subroutine matrixmult2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: matrixmult3DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief multiply 7x7 matrices and reduce translation component
!
!> @details multiplies two 7x7 symmetry matrices and brings
!> the translation component back to the fundamental unit cell.
!
!> @param cell unit cell pointer
!> @param k1 index of first input matrix
!> @param k2 index of second input matrix
!
!> @note there is no output since the result is stored in the SG structure SYM_c array
!
!> @date  06/24/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine matrixmult3DQC(cell, k1, k2)
!DEC$ ATTRIBUTES DLLEXPORT :: matrixmult3DQC
   
IMPLICIT NONE

type(QCStructureType),pointer  :: cell

integer(kind=irg),INTENT(IN)     :: k1                           !< index of first 6x6 input matrix
integer(kind=irg),INTENT(IN)     :: k2                           !< index of second 6x6 input matrix
integer(kind=irg)                :: i,j,k                        !< loop counters
real(kind=dbl),parameter         :: eps=0.0005_dbl               !< truncation constant

 do i=1,7
  do j=1,7
   cell%SG%SYM_c(i,j) = 0.0_dbl
   do k=1,7
    cell%SG%SYM_c(i,j)=cell%SG%SYM_c(i,j)+cell%SG%SYM_data(k1,i,k)*cell%SG%SYM_data(k2,k,j)
   end do
  end do
 end do

! bring the translational part of the matrix back to
! the first unit cell and correct possible rounding errors
do k = 1,6
  if (abs(cell%SG%SYM_c(k,7)).lt.eps) cell%SG%SYM_c(k,7)=0.0_dbl
  if (cell%SG%SYM_c(k,7).lt.0.0_dbl) then 
   do while (cell%SG%SYM_c(k,7).lt.0.0_dbl) 
    cell%SG%SYM_c(k,7)=cell%SG%SYM_c(k,7)+1.0_dbl
   end do
  end if
  if (cell%SG%SYM_c(k,7).gt.1.0_dbl) then 
   do while (cell%SG%SYM_c(k,7).gt.1.0_dbl) 
    cell%SG%SYM_c(k,7)=cell%SG%SYM_c(k,7)-1.0_dbl
   end do
  end if
  if (abs(cell%SG%SYM_c(k,7)-1.0_dbl).lt.eps) cell%SG%SYM_c(k,7)=0.0_dbl
end do
end subroutine matrixmult3DQC

!--------------------------------------------------------------------------
!
! FUNCTION: isitnew2DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief is this a new symmetry operator for 2D quasicrystal?
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
logical recursive function isitnew2DQC(cell,nsym)
!DEC$ ATTRIBUTES DLLEXPORT :: isitnew2DQC

IMPLICIT NONE

type(TDQCStructureType),pointer         :: cell

integer(kind=irg),INTENT(IN)            :: nsym                 !< index of matrix to be compared
integer(kind=irg)                       :: i,j,k,n              !< loop counters
real(kind=dbl),parameter                :: eps=0.0005_dbl       !< comparison threshold

 k=0
 n=0
 do while ((k.le.nsym).and.(n.ne.30))
  n=0
  k=k+1
  do i=1,5
   do j=1,6
    if (abs(cell%SG%SYM_c(i,j)- cell%SG%SYM_data(k,i,j)).lt.eps) n=n+1
   end do
  end do
 end do
 
 if (n.ne.30) then 
  isitnew2DQC = .TRUE.
 else
  isitnew2DQC = .FALSE.
 end if

end function isitnew2DQC

!--------------------------------------------------------------------------
!
! FUNCTION: isitnew3DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief is this a new symmetry operator for 3D quasicrystal?
!
!> @details check whether or not this is a new operator by simply comparing it 
!> with all existing operators
!
!> @param cell unit cell pointer
!> @param nsym index of matrix to be compared
!
!> @date  06/24/18 SS 1.0 original
!--------------------------------------------------------------------------
logical recursive function isitnew3DQC(cell,nsym)
!DEC$ ATTRIBUTES DLLEXPORT :: isitnew3DQC

IMPLICIT NONE

type(QCStructureType),pointer           :: cell

integer(kind=irg),INTENT(IN)            :: nsym                 !< index of matrix to be compared
integer(kind=irg)                       :: i,j,k,n              !< loop counters
real(kind=dbl),parameter                :: eps=0.0005_dbl       !< comparison threshold

 k=0
 n=0
 do while ((k.le.nsym).and.(n.ne.42))
  n=0
  k=k+1
  do i=1,6
   do j=1,7
    if (abs(cell%SG%SYM_c(i,j)- cell%SG%SYM_data(k,i,j)).lt.eps) n=n+1
   end do
  end do
 end do
 
 if (n.ne.42) then 
  isitnew3DQC = .TRUE.
 else
  isitnew3DQC = .FALSE.
 end if

end function isitnew3DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get2DQCOrbit
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate all symmetrically equivalent direct space vectors
!
!> @param  QCcell cell structure
!> @param  hkl    input reciprocal lattice vector
!> @param  orbit  symmetrically equivalent vectors
!> @param  nn     number of symmetrically equivalent vectors
!
!> @date   02/07/18 SS 1.0 original
!> @date   06/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine Get2DQCOrbit(QCcell, orbit, mm, nn)
!DEC$ ATTRIBUTES DLLEXPORT ::Get2DQCOrbit

use typedefs
use local

IMPLICIT NONE

type(TDQCStructureType), pointer      :: QCcell
integer(kind=irg),INTENT(IN)          :: mm
real(kind=dbl),INTENT(OUT)            :: orbit(QCcell%SG%SYM_MATnum,5)
integer(kind=irg),INTENT(OUT)         :: nn


integer(kind=irg)                     :: ii, jj, kk, Pmdims
real(kind=dbl)                        :: r(5), s(5)

nn = 1
orbit(1,1:5) = QCcell%ATOM_pos(mm,1:5)

r(1:5)       = orbit(1,1:5)


Pmdims = QCcell%SG%SYM_MATnum

! get all the equivalent atom positions
do ii = 1,Pmdims

    do jj = 1,5
      s(jj) = QCcell%SG%SYM_data(ii,jj,6)
      do kk = 1,5
        s(jj) = s(jj) + QCcell%SG%SYM_data(ii,jj,kk) * r(kk)
      end do
    end do

    do jj = 1,5

      if(s(jj) .lt. 0.D0) then

        do while(s(jj) .lt. 0.D0) 
          s(jj) = s(jj) + 1.D0
        end do

      else if(s(jj) .gt. 0.D0) then

        s(jj) = mod(s(jj),1.D0)

      end if

  end do

  if(isnewvector(QCcell, s, orbit, nn)) then
    nn = nn + 1
    orbit(nn,1:5) = s
  end if
end do


end subroutine Get2DQCOrbit

!--------------------------------------------------------------------------
!
! SUBROUTINE: Get3DQCOrbit
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate all symmetrically equivalent direct space vectors
!
!> @param  QCcell cell structure
!> @param  hkl    input reciprocal lattice vector
!> @param  orbit  symmetrically equivalent vectors
!> @param  nn     number of symmetrically equivalent vectors
!
!> @date   02/07/18 SS 1.0 original
!> @date   06/01/18 SS 1.1 moved from QCmod to qcrystal
!--------------------------------------------------------------------------
recursive subroutine Get3DQCOrbit(QCcell, orbit, mm, nn)
!DEC$ ATTRIBUTES DLLEXPORT ::Get2DQCOrbit

use typedefs
use local

IMPLICIT NONE

type(QCStructureType), pointer        :: QCcell
integer(kind=irg),INTENT(IN)          :: mm
real(kind=dbl),INTENT(OUT)            :: orbit(QCcell%SG%SYM_MATnum,6)
integer(kind=irg),INTENT(OUT)         :: nn


integer(kind=irg)                     :: ii, jj, kk, Pmdims
real(kind=dbl)                        :: r(6), s(6)

nn = 1
orbit(1,1:6) = QCcell%ATOM_pos(mm,1:6)

r(1:6)       = orbit(1,1:6)

Pmdims = QCcell%SG%SYM_MATnum

! get all the equivalent atom positions
do ii = 1,Pmdims

    do jj = 1,6
      s(jj) = QCcell%SG%SYM_data(ii,jj,7)
      do kk = 1,6
        s(jj) = s(jj) + QCcell%SG%SYM_data(ii,jj,kk) * r(kk)
      end do
    end do

    do jj = 1,6

      if(s(jj) .lt. 0.D0) then

        do while(s(jj) .lt. 0.D0) 
          s(jj) = s(jj) + 1.D0
        end do

      else if(s(jj) .gt. 0.D0) then

        s(jj) = mod(s(jj),1.D0)

      end if
     
  end do

  if(isnewvector(QCcell, s, orbit, nn)) then
    nn = nn + 1
    orbit(nn,1:6) = s
  end if

end do

end subroutine Get3DQCOrbit

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
!> @param pt set of 8 reals returned to the calling routine
!
!> @date   10/13/98 SS 1.0 original, adapted from crystal.f90
!--------------------------------------------------------------------------
recursive subroutine extractposition(list,pt,iQC)
!DEC$ ATTRIBUTES DLLEXPORT :: extractposition

IMPLICIT NONE

character(1),INTENT(IN)                 :: list(256)                              !< input string
real(kind=sgl),INTENT(OUT)              :: pt(10)                                !< output real array
logical,INTENT(IN),OPTIONAL				:: iQC
integer(kind=irg)                       :: comma(12),slash(12),period(12), &
                                           ccnt,scnt,pcnt,pp,i,j,hcnt, &
                                           ip,ipt,icnt,nd,n,k,ns,nocc                !< auxiliary variables
integer(kind=irg),parameter             :: nmb(48:57)=(/0,1,2,3,4,5,6,7,8,9/)   !< list of numbers
real(kind=dbl)                          :: nominator,denominator,x              !< used for fraction interpretation
logical                                 :: hasperiod                            !< used for decimal interpretation

nocc = 6
if(present(iQC)) then
	if(iQC) then
		nocc = 7
	end if
end if

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
 do while (ccnt.lt.8) 
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
 if (pt(nocc).eq.0.0) pt(nocc) = 1.0

end subroutine extractposition

!--------------------------------------------------------------------------
!
! SUBROUTINE: Calc2DQCPositions
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  compute atom positions, always reduced to unit cell
!!
!> @param cell unit cell pointer
!
!> @date  05/24/18 SS 1.0 original, based on CalcPosition
!--------------------------------------------------------------------------
recursive subroutine Calc2DQCPositions(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: Calc2DQCPositions

use QCmod
use error

IMPLICIT NONE

type(TDQCStructureType),pointer         :: cell

logical                         :: inside                       !< auxiliary logical
integer(kind=irg)               :: i,j,k,l,mm,icnt,celln(3),ncells,n,kk,ier, io_int(3)  !< various auxiliary variables
real(kind=dbl)                  :: ctmp(cell%SG%SYM_MATnum,5),ff(5),sh(5)      !< auxiliary variables  
real(kind=sgl)                  :: r(5),g(5)                    !< auxiliary variables  

! make sure all coordinates are reduced to the fundamental unit cell
 cell%SG%SYM_reduce=.TRUE.

! main loop
! first allocate the apos variable (contains CARTESIAN coordinates
! if switch is 'm', crystal coordinates otherwise)
 if (allocated(cell%apos)) deallocate(cell%apos)
 allocate (cell%apos(cell%ATOM_ntype, cell%SG%SYM_MATnum, 5),stat=ier)
 if (ier.ne.0) call FatalError('CalcPositions',' unable to allocate memory for array cell%apos')
 ctmp = 0.D0

 do i=1,cell%ATOM_ntype
! for each atom in the asymmetric unit
  call GetQCOrbit(cell, ctmp, i, n)
  cell%apos(i,1:n,1:5) = ctmp(1:n,1:5)
  cell%numat(i)        = n
 end do  ! cell%ATOM_type

end subroutine Calc2DQCPositions

!--------------------------------------------------------------------------
!
! SUBROUTINE: Calc3DQCPositions
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  compute atom positions, always reduced to unit cell
!!
!> @param cell unit cell pointer
!
!> @date  06/25/18 SS 1.0 original, based on CalcPosition
!--------------------------------------------------------------------------
recursive subroutine Calc3DQCPositions(cell)
!DEC$ ATTRIBUTES DLLEXPORT :: Calc3DQCPositions

use QCmod
use error

IMPLICIT NONE

type(QCStructureType),pointer         :: cell

logical                         :: inside                       !< auxiliary logical
integer(kind=irg)               :: i,j,k,l,mm,icnt,ncells,n,kk,ier, io_int(3)  !< various auxiliary variables
real(kind=dbl)                  :: ctmp(cell%SG%SYM_MATnum,6)      !< auxiliary variables  

! make sure all coordinates are reduced to the fundamental unit cell
 cell%SG%SYM_reduce=.TRUE.

! main loop
! first allocate the apos variable (contains CARTESIAN coordinates
! if switch is 'm', crystal coordinates otherwise)
 if (allocated(cell%apos)) deallocate(cell%apos)
 allocate (cell%apos(cell%ATOM_ntype, cell%SG%SYM_MATnum, 6),stat=ier)
 if (ier.ne.0) call FatalError('CalcPositions',' unable to allocate memory for array cell%apos')
 ctmp = 0.D0

 do i=1,cell%ATOM_ntype
! for each atom in the asymmetric unit
  call GetQCOrbit(cell, ctmp, i, n)
  cell%apos(i,1:n,1:6) = ctmp(1:n,1:6)
  cell%numat(i)        = n
 end do  ! cell%ATOM_type

end subroutine Calc3DQCPositions

!--------------------------------------------------------------------------
!
! FUNCTION: IsGAllowedQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  is a reflection allowed?
!
!> @details determine whether or not a given reflection is absent due to
!> lattice centering operations.
!
!> @param cell unit cell pointer
!> @param g input vector  
!
!> @date  07/02/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function IsGAllowedQC(cell,g) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: IsGAllowedQC

IMPLICIT NONE

type(QCStructureType),pointer  			:: cell
integer(kind=irg),INTENT(IN)            :: g(6)         	!< input reciprocal lattice vector

integer(kind=irg)                       :: seo,sgnum,icase  !< auxiliary variable
logical 								:: res
! Determine whether or not this vector is
! actually allowed by the lattice centering
sgnum = cell%SYM_SGnum
icase = 0

if(sgnum .eq. 1 .or. sgnum .eq. 2 .or. sgnum .eq. 7 .or. sgnum .eq. 8) 	 icase = 1 ! primitive

if(sgnum .eq. 3 .or. sgnum .eq. 4 .or. sgnum .eq. 9 ) 					 icase = 2 ! body-centered

if(sgnum .eq. 5 .or. sgnum .eq. 6 .or. sgnum .eq. 10 .or. sgnum .eq. 11) icase = 3 ! face-centered

res = .TRUE.
 select case (icase)
  case (1)
 																! all reflections allowed for a primitive lattice
  case (2)
   seo = mod(sum(g)+100,2); if (seo.eq.1) 	res = .FALSE.	   	! body-centered sum all even

  case (3)
   seo = sum(mod(g+100,2)); if (seo .le. 5) res = .FALSE.  		! face-centered all even or all odd
    
  case DEFAULT

 end select
 
end function IsGAllowedQC

end module
