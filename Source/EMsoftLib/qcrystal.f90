! ###################################################################
! Copyright (c) 2014, Marc De Graef/Carnegie Mellon University
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

private   :: matrixmult, isitnew, extractposition

public

interface GetQCLatParm
  module procedure Get2DQCLatParm
  module procedure Get3DQCLatParm
end interface GetQCLatParm

interface GenerateQCSymmetry
  module procedure Generate2DQCSymmetry
  !module procedure Generate3DQCSymmetry
end interface GenerateQCSymmetry

interface GetQCSpaceGroup
  module procedure Get2DQCSpaceGroup
  !module procedure Get3DQCSpaceGroup
end interface GetQCSpaceGroup

interface GetQCAsymPos
  module procedure Get2DQCAsymPos
  !module procedure Get3DQCAsymPos
end interface GetQCAsymPos

interface CalcQCPositions
  module procedure Calc2DQCPositions
  !module procedure Calc3DQCPositions
end interface CalcQCPositions



contains

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! SUBROUTINE: PrintSGTable
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate SG name and fill 2D quasicrystal structure
!
!> David Rabson, David Mermin, Daniel Rokhsar, David Wright, The space groups
!> of axial crystals and quasicrystals, REVIEW OF MODERN PHYSICS, 63(3), 1991.
!
!--------------------------------------------------------------------------
recursive subroutine PrintSGTable(TDQCcell,toprint)
!DEC$ ATTRIBUTES DLLEXPORT :: PrintSGTable

use io
use error

type(TDQCStructureType), pointer            :: TDQCcell
logical,INTENT(IN),OPTIONAL                 :: toprint

integer(kind=irg)                           :: n      ! axial rotational symmetry
character(1)                                :: lat    ! primitive or centered
character(fnlen)                            :: str, str1, str2    ! concatanate various symbols here
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
    nsg = 16 + 2*(n - 1)
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
    str1 = trim(str1)//'22'
    write(str,'(A,A)')'P',trim(str1)
    TDQCcell%SGname(n+2) = trim(str)

    do ii = n+2, 2*n
      write(str1,'(I6)') ii-n-2
      str1 = adjustl(str1)
      str1 = '_'//trim(str1)//' '//'22'
      write(str2,'(I6,A)') n,trim(str1)
      str2 = adjustl(str2)
      write(str,'(A,A)')'P',trim(str2)
      TDQCcell%SGname(ii) = trim(str)
    end do

    write(str1,'(I6)') -n
    str1 = adjustl(str1)
    TDQCcell%SGname(2*n+1) = 'P'//trim(str1)//'m2'
    TDQCcell%SGname(2*n+2) = 'P'//trim(str1)//'c2'
    TDQCcell%SGname(2*n+3) = 'P'//trim(str1)//'2m'
    TDQCcell%SGname(2*n+4) = 'P'//trim(str1)//'2c'

    write(str1,'(I6)') n
    str1 = adjustl(str1)
    write(str2,'(I6)') n/2
    str2 = adjustl(str2)

    TDQCcell%SGname(2*n+5) = 'P'//trim(str1)//'mm'
    TDQCcell%SGname(2*n+6) = 'P'//trim(str1)//'cc'
    TDQCcell%SGname(2*n+7) = 'P'//trim(str1)//'_'//trim(str2)//'mc'
    TDQCcell%SGname(2*n+8) = 'P'//trim(str1)//'_'//trim(str2)//'cm'
    TDQCcell%SGname(2*n+9) = 'P'//trim(str1)//'/m'
    TDQCcell%SGname(2*n+10) = 'P'//trim(str1)//'_'//trim(str2)//'/m'
    TDQCcell%SGname(2*n+11) = 'P'//trim(str1)//'/m 2/m 2/m'
    TDQCcell%SGname(2*n+12) = 'P'//trim(str1)//'/m 2/c 2/c'
    TDQCcell%SGname(2*n+13) = 'P'//trim(str1)//'_'//trim(str2)//'/m 2/m 2/c'
    TDQCcell%SGname(2*n+14) = 'P'//trim(str1)//'_'//trim(str2)//'/m 2/c 2/m'

    if(pflag) then
      do ii = 1, nsg
        write(str,'(I3,A)')ii,'. '
        call Message(trim(str)//trim(TDQCcell%SGname(ii)))
      end do
    end if

  case(4)
    nsg = 13 + 2*(n - 1)
    io_int(1) = nsg
    write(str,'(I2)') n
    if(pflag) then
      call WriteValue('Number of space groups for axial symmetry of '//trim(str)//' = ',io_int,1,'(I3,//)')
    end if
    if(.not.(allocated(TDQCcell%SGname))) allocate(TDQCcell%SGname(nsg))

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


end subroutine PrintSGTable

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
  TDQCcell%QCtype = 'Oct'
  TDQCcell%SG%N_Axial = 8

 case(2)
  ! 10-fold
  TDQCcell%QCtype = 'Dec'
  TDQCcell%SG%N_Axial = 10

 case(3)
  ! 12-fold
  TDQCcell%QCtype = 'DoD'
  TDQCcell%SG%N_Axial = 12

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
! SUBROUTINE: GetAsymPos
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
real(kind=sgl)                          :: pt(7), out_real(7)   !< used to read and write asymmetric position data
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
  call Message(' ->  Fractional coordinates, site occupation, and Debye-Waller Factor [nm^2] : ', frm = "(A,' ',$)")
  read (5,"(256A)") list

! interpret this string and extract coordinates and such ...
  call extractposition(list,pt) 
  
! store in the appropriate component of the cell variable  
  TDQCcell%ATOM_pos(TDQCcell%ATOM_ntype,1:7) = pt(1:7)

! and write the coordinate back to the terminal  
  out_real = (/ (TDQCcell%ATOM_pos(TDQCcell%ATOM_ntype,j),j=1,7) /)
  call WriteValue('    -> ', out_real, 7, frm = "(1x,6(F10.7,2x),F10.7)") 

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
! SUBROUTINE: Generate2DQCSymmetry
!
!> @author Saransh, Carnegie Mellon University
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
recursive subroutine Generate2DQCSymmetry(TDQCcell)
!DEC$ ATTRIBUTES DLLEXPORT :: Generate2DQCSymmetry

IMPLICIT NONE

type(TDQCStructureType),pointer           :: TDQCcell

integer(kind=irg)                         :: i,j,k,nsym,k1,k2,l1,l2       !< loop counters (mostly)
real(kind=dbl)                            :: q,sm                         !< auxiliary variables.

! create the space group generator matrices
 call MakeTDQCGenerators(TDQCcell)
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
    !if (nsym.ge.192) then 
     !k2 = nsym
     !k1 = nsym
    !end if
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

end subroutine Generate2DQCSymmetry

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

g1(1,1:5) = (/0.D0, 1.D0, -1.D0, 0.D0, 0.D0/)
g1(2,1:5) = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
g1(3,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g1(4,1:5) = (/-1.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g1(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

g2(1,1:5) = (/0.D0, 0.D0, 0.D0, -1.D0, 0.D0/)
g2(2,1:5) = (/0.D0, 0.D0, -1.D0, 0.D0, 0.D0/)
g2(3,1:5) = (/0.D0, -1.D0, 0.D0, 0.D0, 0.D0/)
g2(4,1:5) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g2(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

g3(1,1:5) = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
g3(2,1:5) = (/0.D0, 1.D0, 0.D0, 0.D0, 0.D0/)
g3(3,1:5) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/)
g3(4,1:5) = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
g3(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, -1.D0/)

! generator r^1/2
TDQCcell%SG%SYM_data(1,1:6,1:6) = 0.D0
TDQCcell%SG%SYM_data(1,6,6)     = 1.D0

TDQCcell%SG%SYM_data(1,1:5,1:5) = g1(1:5,1:5)
TDQCcell%SG%SYM_data(1,5,6)     = 0.5D0

! generator h (primary m)
TDQCcell%SG%SYM_data(2,1:6,1:6) = 0.D0
TDQCcell%SG%SYM_data(2,6,6)     = 1.D0

TDQCcell%SG%SYM_data(2,1:5,1:5) = g3(1:5,1:5)

! generator m (secondary m)
TDQCcell%SG%SYM_data(3,1:6,1:6) = 0.D0
TDQCcell%SG%SYM_data(3,6,6)     = 1.D0

TDQCcell%SG%SYM_data(3,1:5,1:5) = g2(1:5,1:5)

TDQCcell%SG%SYM_GENnum = 3

end subroutine MakeTDQCGenerators

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
  isitnew=.TRUE.
 else
  isitnew=.FALSE.
 end if

end function isitnew

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
!> @date   10/13/98 SS 1.0 original, adapted from crystal.f90
!--------------------------------------------------------------------------
recursive subroutine extractposition(list,pt)
!DEC$ ATTRIBUTES DLLEXPORT :: extractposition

IMPLICIT NONE

character(1),INTENT(IN)                 :: list(256)                              !< input string
real(kind=sgl),INTENT(OUT)              :: pt(7)                                !< output real array
integer(kind=irg)                       :: comma(8),slash(7),period(7), &
                                           ccnt,scnt,pcnt,pp,i,j,hcnt, &
                                           ip,ipt,icnt,nd,n,k,ns                !< auxiliary variables
integer(kind=irg),parameter             :: nmb(48:57)=(/0,1,2,3,4,5,6,7,8,9/)   !< list of numbers
real(kind=dbl)                          :: nominator,denominator,x              !< used for fraction interpretation
logical                                 :: hasperiod                            !< used for decimal interpretation

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
 if (pt(6).eq.0.0) pt(6) = 1.0

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

use TDQCmod
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
  call Get2DQCOrbit(cell, ctmp, i, n)
  cell%apos(i,1:n,1:5) = ctmp(1:n,1:5)
  cell%numat(i)        = n
 end do  ! cell%ATOM_type

end subroutine Calc2DQCPositions

end module