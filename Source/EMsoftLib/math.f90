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
! EMsoft:math.f90
!--------------------------------------------------------------------------
!
! MODULE: math
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief collection of mathematical/numerical routines that don't fit anywhere else
! 
!> @date 10/13/98 MDG 1.0 original
!> @date 05/19/01 MDG 2.0 f90
!> @date 11/27/01 MDG 2.1 added kind support
!> @date 03/19/13 MDG 3.0 updated all routines
!> @date 11/13/13 MDG 4.0 added MatrixExponential routine
!> @date 11/23/15 MDG 4.1 moved several routines from other mods into this one
!> @date 10/24/17 MDG 4.2 added infty()/inftyd() functions to return the IEEE infinity value
!> @date 08/23/19 MDG 4.3 removed spaces around "kind" statements to facilitate f90wrap python wrapper generation
!> @date 10/04/19 MDG 4.4 adds vecnorm to replace non-standard NORM2 calls  (F2003 compliance)
!> @date 10/04/19 MDG 4.5 adds nan() function, returning a single or double precision IEEE NaN value
!--------------------------------------------------------------------------
! ###################################################################
!  

module math

use local

public :: mInvert, cross3, infty, inftyd

interface mInvert
        module procedure mInvert
        module procedure mInvert_d
end interface

interface cross3
        module procedure cross3
        module procedure cross3_d
end interface 

interface vecnorm
        module procedure vecnorm 
        module procedure vecnorm_d
        module procedure vecnorm2 
        module procedure vecnorm2_d
end interface

contains


!--------------------------------------------------------------------------
!
! FUNCTION: vecnorm
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the single precision length of a 1D vector
!
!> @date  10/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function vecnorm(vec) result(veclen)
!DEC$ ATTRIBUTES DLLEXPORT :: vecnorm

real(kind=sgl),INTENT(IN)        :: vec(:)
real(kind=sgl)                   :: veclen

integer(kind=irg)                :: sz(1)

sz = size(vec)

veclen = sqrt(sum(vec(1:sz(1))*vec(1:sz(1))))

end function vecnorm

!--------------------------------------------------------------------------
!
! FUNCTION: vecnorm_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the double precision length of a 1D vector
!
!> @date  10/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function vecnorm_d(vec) result(veclen)
!DEC$ ATTRIBUTES DLLEXPORT :: vecnorm_d

real(kind=dbl),INTENT(IN)       :: vec(:)
real(kind=dbl)                  :: veclen

integer(kind=irg)               :: sz(1)

sz = size(vec)

veclen = sqrt(sum(vec(1:sz(1))*vec(1:sz(1))))

end function vecnorm_d

!--------------------------------------------------------------------------
!
! FUNCTION: vecnorm2
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the single precision length of a 2D array
!
!> @date  10/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function vecnorm2(vec) result(veclen)
!DEC$ ATTRIBUTES DLLEXPORT :: vecnorm2

real(kind=sgl),INTENT(IN)        :: vec(:,:)
real(kind=sgl)                   :: veclen

integer(kind=irg)                :: sz(2)

sz = size(vec)

veclen = sqrt(sum(vec(1:sz(1),1:sz(2))*vec(1:sz(1),1:sz(2))))

end function vecnorm2

!--------------------------------------------------------------------------
!
! FUNCTION: vecnorm2_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the double precision length of a 2D array
!
!> @date  10/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function vecnorm2_d(vec) result(veclen)
!DEC$ ATTRIBUTES DLLEXPORT :: vecnorm2_d

real(kind=dbl),INTENT(IN)        :: vec(:,:)
real(kind=dbl)                   :: veclen

integer(kind=irg)                :: sz(2)

sz = size(vec)

veclen = sqrt(sum(vec(1:sz(1),1:sz(2))*vec(1:sz(1),1:sz(2))))

end function vecnorm2_d


!--------------------------------------------------------------------------
!
! FUNCTION: infty
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the single precision IEEE value for infinity
!
!> @date  10/24/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function infty() result(infinity)
!DEC$ ATTRIBUTES DLLEXPORT :: infty

real(kind=sgl)      :: infinity
real(kind=sgl)      :: big 

big = HUGE(1.0)
infinity = big + HUGE(1.0)

end function infty


!--------------------------------------------------------------------------
!
! FUNCTION: inftyd
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the double precision IEEE value for infinity
!
!> @date  10/24/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function inftyd() result(infinity)
!DEC$ ATTRIBUTES DLLEXPORT :: inftyd

real(kind=dbl)      :: infinity
real(kind=dbl)      :: big 

big = HUGE(1.D0)
infinity = big + HUGE(1.D0)

end function inftyd

!--------------------------------------------------------------------------
!
! FUNCTION: nan
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the single precision IEEE value for nan
!
!> @date  10/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function nan() result(x)
!DEC$ ATTRIBUTES DLLEXPORT :: nan

 use, intrinsic :: iso_fortran_env
 use, intrinsic :: ieee_arithmetic

 IMPLICIT NONE 

real(kind=sgl)        :: x

x = ieee_value(x, ieee_quiet_nan)

end function nan

!--------------------------------------------------------------------------
!
! FUNCTION: nan_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the sngle precision IEEE value for nan
!
!> @date  10/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function nan_d() result(x)
!DEC$ ATTRIBUTES DLLEXPORT :: nan_d

 use, intrinsic :: iso_fortran_env
 use, intrinsic :: ieee_arithmetic

 IMPLICIT NONE 

real(kind=dbl)        :: x

x = ieee_value(x, ieee_quiet_nan)

end function nan_d


!--------------------------------------------------------------------------
!
! SUBROUTINE: getPolarDecomposition
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Use LAPACK routines to compute the polar decomposition of a real 3x3 matrix
!
!> @param F input matrix
!> @param Rmatrix (output) unitary matrix
!> @param Smatrix (output) symmetric stretch matrix
!
!> @date  09/29/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getPolarDecomposition(F, Rmatrix, Smatrix)
!DEC$ ATTRIBUTES DLLEXPORT :: getPolarDecomposition

use local
use error
use io

IMPLICIT NONE

real(kind=dbl),INTENT(IN)     :: F(3,3)           !< input matrix
real(kind=dbl),INTENT(OUT)    :: Rmatrix(3,3)     !< output unitary matrix
real(kind=dbl),INTENT(OUT)    :: Smatrix(3,3)     !< output symmetric stretch matrix

! various parameters needed by the LAPACK routine
integer(kind=irg)               :: INFO, LDA, LDU, LDVT, LWORK, M, N, i
integer(kind=irg),parameter     :: LWMAX = 100 
real(kind=dbl)                  :: A(3,3), WORK(LWMAX), S(3), U(3,3), VT(3,3), Sm(3,3)
character                       :: JOBU, JOBVT

! set initial LAPACK variables
JOBU = 'A'
JOBVT = 'A'
M = 3
N = 3
LDA = 3
A = F
LDU = 3
LDVT = 3

S = 0.D0
U = 0.D0
VT = 0.D0

LWORK = LWMAX

call dgesvd(JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO)
if (INFO.ne.0) call FatalError('Error in getPolarDecomposition: ','DGESVD return not zero')
Sm = 0.D0
Sm(1,1) = S(1)
Sm(2,2) = S(2)
Sm(3,3) = S(3)

! next use these matrices to compute the polar decomposition
Rmatrix = matmul(U, VT)
Smatrix = matmul(transpose(VT),matmul(Sm, VT))

end subroutine getPolarDecomposition

!--------------------------------------------------------------------------
!
! SUBROUTINE: get_bit_parameters
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief interpret a bitdepth string and return parameters
!
!> @param bd bitdepth string
!> @param numbits number of bits 
!> @param bitrange 2^(number of bits)-1 
!> @param bitmode char, int, or float
!
!> @date    8/25/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine get_bit_parameters(bd, numbits, bitrange, bitmode)
!DEC$ ATTRIBUTES DLLEXPORT :: get_bit_parameters

use io

character(5),INTENT(IN)         :: bd
integer(kind=irg),INTENT(OUT)   :: numbits
real(kind=sgl),INTENT(OUT)      :: bitrange
character(5),INTENT(OUT)        :: bitmode

character(2)                    :: bitval
integer(kind=irg)               :: io_int(1)

!====================================
! analyze the bitdepth parameter; if we have integers, then we need to analyze the 
! digits in the string to figure out how to scale the data.  '10int' means that we
! store the data as 32-bit integers, but the scaled values range from 0 to 2^(10)-1
if ((trim(bd).ne.'8bit').and.(trim(bd).ne.'float')) then
  bitval(1:1) = bd(1:1)
  if (bd(2:2).ne.'i') then
    bitval(2:2) = bd(2:2)
  else
    bitval(2:2) = ''
  end if
  read (bitval,*) numbits
  io_int(1) = numbits 
  call WriteValue(' ---> Integer format requested with bit depth ',io_int,1,"(I3)")
  bitrange = 2.0**numbits-1.0
  bitmode = 'int'
end if
if (trim(bd).eq.'8bit') then 
  numbits = 8
  io_int(1) = numbits 
  call WriteValue(' ---> character format requested with bit depth ',io_int,1,"(I3)")
  bitrange = 2.0**numbits-1.0
  bitmode = 'char'
end if
if (trim(bd).eq.'float') then
  call Message(' ---> 32-bit float format requested')
  numbits = 32
  bitrange = 0.0
  bitmode = 'float'
end if

end subroutine get_bit_parameters

!--------------------------------------------------------------------------
!
! SUBROUTINE:mInvert_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Invert a 3x3 matrix
!
!> @details  Invert a 3x3 matrix; if unitary, simply transpose
!
!> @param a input matrix
!> @param b output matrix
!> @param uni .TRUE. if unitary matrix, .FALSE. otherwise
!
!> @todo this should really be replaced by a BLAS call
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    4/ 5/00 MDG 1.1 added inverse of unitary matrix
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!
!--------------------------------------------------------------------------
recursive subroutine mInvert_d(a,b,uni)
!DEC$ ATTRIBUTES DLLEXPORT :: mInvert_d

use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: a(3,3)               !< input matrix
real(kind=dbl),INTENT(OUT)              :: b(3,3)               !< output matrix
logical,INTENT(IN)                      :: uni                  !< unitary logical
real(kind=dbl)                          :: d                    !< auxiliary variable

! it is a regular (non-unitary) matrix
 if (.not.uni) then 
  d = a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+ &
         a(1,3)*a(2,1)*a(3,2)-a(1,3)*a(2,2)*a(3,1)- &
         a(1,2)*a(2,1)*a(3,3)-a(1,1)*a(2,3)*a(3,2)
  if (d.ne.0.D0) then
   b(1,1)=a(2,2)*a(3,3)-a(2,3)*a(3,2)
   b(1,2)=a(1,3)*a(3,2)-a(1,2)*a(3,3)
   b(1,3)=a(1,2)*a(2,3)-a(1,3)*a(2,2)
   b(2,1)=a(2,3)*a(3,1)-a(2,1)*a(3,3)
   b(2,2)=a(1,1)*a(3,3)-a(1,3)*a(3,1)
   b(2,3)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
   b(3,1)=a(2,1)*a(3,2)-a(2,2)*a(3,1)
   b(3,2)=a(1,2)*a(3,1)-a(1,1)*a(3,2)
   b(3,3)=a(1,1)*a(2,2)-a(1,2)*a(2,1)
   b = b/d
  else
   call FatalError('mInvert','matrix has zero determinant')
  end if
 else
! it is a unitary matrix, so simply get the transpose
  b = transpose(a)
 endif

end subroutine mInvert_d
      
!--------------------------------------------------------------------------
!
! SUBROUTINE:mInvert
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Invert a 3x3 matrix
!
!> @details  Invert a 3x3 matrix; if unitary, simply transpose
!
!> @param a input matrix
!> @param b output matrix
!> @param uni .TRUE. if unitary matrix, .FALSE. otherwise
!
!> @todo this should really be replaced by a BLAS call
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    4/ 5/00 MDG 1.1 added inverse of unitary matrix
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!
!--------------------------------------------------------------------------
recursive subroutine mInvert(a,b,uni)
!DEC$ ATTRIBUTES DLLEXPORT :: mInvert

use error

IMPLICIT NONE

real(kind=sgl),INTENT(IN)               :: a(3,3)               !< input matrix
real(kind=sgl),INTENT(OUT)              :: b(3,3)               !< output matrix
logical,INTENT(IN)                      :: uni                  !< unitary logical
real(kind=sgl)                          :: d                    !< auxiliary variable

! it is a regular (non-unitary) matrix
 if (.not.uni) then 
  d = a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+ &
         a(1,3)*a(2,1)*a(3,2)-a(1,3)*a(2,2)*a(3,1)- &
         a(1,2)*a(2,1)*a(3,3)-a(1,1)*a(2,3)*a(3,2)
  if (d.ne.0.0) then
   b(1,1)=a(2,2)*a(3,3)-a(2,3)*a(3,2)
   b(1,2)=a(1,3)*a(3,2)-a(1,2)*a(3,3)
   b(1,3)=a(1,2)*a(2,3)-a(1,3)*a(2,2)
   b(2,1)=a(2,3)*a(3,1)-a(2,1)*a(3,3)
   b(2,2)=a(1,1)*a(3,3)-a(1,3)*a(3,1)
   b(2,3)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
   b(3,1)=a(2,1)*a(3,2)-a(2,2)*a(3,1)
   b(3,2)=a(1,2)*a(3,1)-a(1,1)*a(3,2)
   b(3,3)=a(1,1)*a(2,2)-a(1,2)*a(2,1)
   b = b/d
  else
   call FatalError('mInvert','matrix has zero determinant')
  end if
 else
! it is a unitary matrix, so simply get the transpose
  b = transpose(a)
 endif

end subroutine mInvert

!--------------------------------------------------------------------------
!
! SUBROUTINE:cInvert
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Invert a 3x3 complex matrix
!
!> @param a input matrix
!> @param b output matrix
!
!> @todo this should really be replaced by a BLAS call
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    4/ 5/00 MDG 1.1 added inverse of unitary matrix
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!
!--------------------------------------------------------------------------
recursive subroutine cInvert(a,b)
!DEC$ ATTRIBUTES DLLEXPORT :: cInvert

use error

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)            :: a(3,3)               !< input matrix
complex(kind=dbl),INTENT(OUT)           :: b(3,3)               !< output matrix
complex(kind=dbl)                                       :: d                    !< auxiliary variable

  d = a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+ &
      a(1,3)*a(2,1)*a(3,2)-a(1,3)*a(2,2)*a(3,1)- &
      a(1,2)*a(2,1)*a(3,3)-a(1,1)*a(2,3)*a(3,2)
  if (abs(d).ne.0.D0) then
   b(1,1)=a(2,2)*a(3,3)-a(2,3)*a(3,2)
   b(1,2)=a(1,3)*a(3,2)-a(1,2)*a(3,3)
   b(1,3)=a(1,2)*a(2,3)-a(1,3)*a(2,2)
   b(2,1)=a(2,3)*a(3,1)-a(2,1)*a(3,3)
   b(2,2)=a(1,1)*a(3,3)-a(1,3)*a(3,1)
   b(2,3)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
   b(3,1)=a(2,1)*a(3,2)-a(2,2)*a(3,1)
   b(3,2)=a(1,2)*a(3,1)-a(1,1)*a(3,2)
   b(3,3)=a(1,1)*a(2,2)-a(1,2)*a(2,1)
   b = b/d
  else
   call FatalError('cInvert','Matrix has complex zero determinant')
  end if

end subroutine cInvert


!--------------------------------------------------------------------------
!
! SUBROUTINE: MatrixExponential
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the exponential of a dynamical matrix
!
!> @details This routine uses two different methods, one based on the Taylor
!> expansion, the other on Pade approximants, both with "scaling & squaring".
!> Currently, the routine targets an accuracy level of 10^{-9}.
!> This routine uses table 1 in "Nineteen dubious ways to compute the exponential of a matrix,
!> twenty-five years later", C. Moler, C. Van Loan, SIAM Review, 45, 1 (2003)
!
!> @param A input matrix
!> @param E output matrix
!> @param z0 slice thickness in [nm]
!> @param TP 'Tayl' or 'Pade', to select method
!> @param nn number of row/column entries in A
!
!> @date 09/16/13 MDG 1.0 original, tested against analytical version for small array
!> @date 06/05/14 MDG 1.1 updated IO
!--------------------------------------------------------------------------
recursive subroutine MatrixExponential(A,E,z0,TP,nn)
!DEC$ ATTRIBUTES DLLEXPORT :: MatrixExponential

use io
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nn
complex(kind=dbl),INTENT(IN)            :: A(nn,nn)
complex(kind=dbl),INTENT(OUT)           :: E(nn,nn)
real(kind=dbl),INTENT(IN)               :: z0
character(4),INTENT(IN)         :: TP

real(kind=dbl)                          :: modA, pref, sgn
complex(kind=dbl),allocatable           :: B(:,:), add(:,:), Nqq(:,:), Dqq(:,:), C(:,:)

integer(kind=irg)                       :: i, k, j, icnt, q, istat, ilev

integer(kind=irg)                       :: INFO, LDA, MILWORK
integer(kind=irg),allocatable           :: JPIV(:)
complex(kind=dbl),allocatable           :: MIWORK(:)

integer(kind=irg),parameter             :: kTaylor(6) = (/ 3, 4, 6, 8, 7, 6 /)          ! from table 1 in reference above
integer(kind=irg),parameter             :: jTaylor(6) = (/ 1, 2, 3, 5, 9, 13 /)
integer(kind=irg),parameter             :: qPade(6) = (/ 2, 3, 4, 4, 4, 4 /)
integer(kind=irg),parameter             :: jPade(6) = (/ 0, 0, 1, 5, 8, 11 /)

! set output array to zero
E = cmplx(0.D0,0.D0)

!get the max( ||A|| ) value and determine index into (k,j) or (q,j) pairs
modA = maxval(abs(A)*z0)
ilev = nint(log10(modA))+3
if (ilev.le.0) ilev = 1         ! can not be smaller than 1

! if modA gets to be too large, abort with a message
if (modA.gt.10000.D0) call FatalError('MatrixExponential','  routine can not deal with ||A|| > 10000.0')

if (TP.eq.'Tayl') then ! use scaling and squaring for the Taylor expansion
        k = kTaylor(ilev)
        j = jTaylor(ilev)

        ! allocate an auxiliary array
        allocate( B(nn,nn), add(nn,nn), stat=istat )
        if (istat.ne.0) call FatalError('MatrixExponential',' Error allocating arrays for Taylor approximation')
        
        ! perform the scaling step
        B = (A * z0) / 2.0D0**j ! cmplx(2.0**j,0.0)
        
        ! initialize the diagonal of E
        forall (i=1:nn) E(i,i) = cmplx(1.0D0,0.D0)
        
        ! loop over the Taylor series
        add = B
        E = E + add
        do icnt=2,k
          add = matmul( add, B/cmplx(icnt,0) )
          E = E + add
        end do
        
        ! and deallocate the auxiliary arrays
        deallocate(add, B)

else ! Pade approximation for target accuracy 10^(-9)
        q = qPade(ilev)
        j = jPade(ilev)

        ! allocate auxiliary arrays
        allocate(B(nn,nn),C(nn,nn), Nqq(nn,nn), Dqq(nn,nn), stat=istat )
        if (istat.ne.0) call FatalError('MatrixExponential',' Error allocating arrays for Pade approximation')
        
        ! perform the scaling step
        B = (A * z0) / 2.D0**j  ! cmplx(2.0**j,0.0)
        C = B
                
        ! initialize the diagonal of both arrays
        Nqq = cmplx(0.D0,0.D0)
        forall (i=1:nn) Nqq(i,i) = cmplx(1.0D0,0.D0)
        Dqq = Nqq

        ! init some constants
        pref = 1.D0
        sgn = -1.D0
        
        ! and loop
        do icnt=1,q
          pref = pref * dble(q-icnt+1) / dble(icnt) / dble(2*q-icnt+1)
          Nqq = Nqq + pref * C
          Dqq = Dqq + sgn * pref * C
          sgn = -sgn
          C = matmul( C, B )
        end do
        
        ! get the inverse of Dqq using the LAPACK routines zgetrf and zgetri
        LDA = nn
        allocate( JPIV(nn) )
        call zgetrf(nn,nn,Dqq,LDA,JPIV,INFO)
        if (INFO.ne.0) call FatalError('Error in MatrixExponential: ',' ZGETRF return not zero')

        MILWORK = 64*nn 
        allocate(MIWORK(MILWORK))

        MIWORK = cmplx(0.0_dbl,0.0_dbl)
        call zgetri(nn,Dqq,LDA,JPIV,MIWORK,MILWORK,INFO)
        if (INFO.ne.0) call FatalError('Error in MatrixExponential: ',' ZGETRI return not zero')

        ! and compute E
        E = matmul( Dqq, Nqq )
        
        ! clean up
        deallocate(Nqq, Dqq, C, B, JPIV, MIWORK)
end if

! and finally compute the power 2^j of the matrix E (i.e. the squaring step)
do icnt = 1,j
  E = matmul( E, E )
end do

end subroutine MatrixExponential


!--------------------------------------------------------------------------
!
! FUNCTION BesselIn
!
!> @author Marc De Graef, Carnegie Mellon University/ J-P Moreau (www.jpmoreau.fr)
!
!> @brief compute the first order modified Bessel function I_n(x)
!
!> @details original source: http://jean-pierre.moreau.pagesperso-orange.fr/Fortran/tbessi_f90.txt (12/31/14)
!>* -------------------------------------------------------------------- *
!>*   Reference: From Numath Library By Tuan Dang Trong in Fortran 77.   *
!>*                                                                      *
!>*                               F90 Release 1.2 By J-P Moreau, Paris.  *
!>*                                        (www.jpmoreau.fr)             *
!> ----------------------------------------------------------------------
!> routine tested and compared with both Matlab and IDL output.
!
!> @param X input argument
!> @param N Bessel order
!
!> @date 12/31/14 MDG 1.0 original, rewritten with EMsoft module calls and renamed
!--------------------------------------------------------------------------
recursive function BesselIn(X,N) result(BESSI)
!DEC$ ATTRIBUTES DLLEXPORT :: BesselIn
! original comment:
!     This subroutine calculates the first kind modified Bessel function
!     of integer order N, for any REAL X. We use here the classical
!     recursion formula, when X > N. For X < N, the Miller's algorithm
!     is used to avoid overflows. 
!     REFERENCE:
!     C.W.CLENSHAW, CHEBYSHEV SERIES FOR MATHEMATICAL FUNCTIONS,
!     MATHEMATICAL TABLES, VOL.5, 1962.
!
use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: X
integer(kind=irg),INTENT(IN)    :: N

integer(kind=irg),parameter     :: IACC = 40
real(kind=dbl),parameter        :: BIGNO = 1.D10, BIGNI = 1.D-10
integer(kind=irg)               :: M, J
real(kind=dbl)                  :: BESSI,BESSI0,BESSI1,TOX,BIM,BI,BIP

! special cases
if (N.EQ.0) then
  BESSI = BesselI0(X)
  return
end if

if (N.EQ.1) then
  BESSI = BesselI1(X)
  return
endif

if(X.EQ.0.D0) then
  BESSI=0.D0
  return
endif

! set up the recursion 
TOX = 2.D0/X
BIP = 0.D0
BI  = 1.D0
BESSI = 0.D0
M = 2*((N+int(sqrt(float(IACC*N)))))

DO J = M,1,-1
   BIM = BIP+dble(J)*TOX*BI
   BIP = BI
   BI  = BIM
   if (dabs(BI).GT.BIGNO) then
     BI  = BI*BIGNI
     BIP = BIP*BIGNI
     BESSI = BESSI*BIGNI
   end if
   if (J.EQ.N) BESSI = BIP
end do
BESSI = BESSI*BesselI0(X)/BI

end function BesselIn

!--------------------------------------------------------------------------
!
! FUNCTION BesselI0
!
!> @author Marc De Graef, Carnegie Mellon University/ J-P Moreau (www.jpmoreau.fr)
!
!> @brief compute the first order modified Bessel function I_0(x)
!
!> @details original source: http://jean-pierre.moreau.pagesperso-orange.fr/Fortran/tbessi_f90.txt (12/31/14)
!>* -------------------------------------------------------------------- *
!>*   Reference: From Numath Library By Tuan Dang Trong in Fortran 77.   *
!>*                                                                      *
!>*                               F90 Release 1.2 By J-P Moreau, Paris.  *
!>*                                        (www.jpmoreau.fr)             *
!> ----------------------------------------------------------------------
!> routine tested and compared with both Matlab and IDL output.
!
!> @param X input argument
!
!> @date 12/31/14 MDG 1.0 original, rewritten with EMsoft module calls and renamed
!--------------------------------------------------------------------------
recursive function BesselI0(X) result(BESSI0)
!DEC$ ATTRIBUTES DLLEXPORT :: BesselI0

use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: X
real(kind=dbl)                  :: BESSI0

real(kind=dbl)                  :: Y,Ax,BX
real(kind=dbl),parameter        :: P1=1.D0, P2=3.5156229D0, P3=3.0899424D0, P4=1.2067492D0,  &
                                   P5=0.2659732D0, P6=0.360768D-1, P7=0.45813D-2, Q1=0.39894228D0, &
                                   Q2=0.1328592D-1, Q3=0.225319D-2, Q4=-0.157565D-2, Q5=0.916281D-2, &
                                   Q6=-0.2057706D-1,  Q7=0.2635537D-1, Q8=-0.1647633D-1, Q9=0.392377D-2

if(dabs(X).lt.3.75D0) then
  Y=(X/3.75D0)**2
  BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
else
  AX=dabs(X)
  Y=3.75D0/AX
  BX=dexp(AX)/dsqrt(AX)
  AX=Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9)))))))
  BESSI0=AX*BX
ENDIF

end function BesselI0


!--------------------------------------------------------------------------
!
! FUNCTION BesselI1
!
!> @author Marc De Graef, Carnegie Mellon University/ J-P Moreau (www.jpmoreau.fr)
!
!> @brief compute the first order modified Bessel function I_1(x)
!
!> @details original source: http://jean-pierre.moreau.pagesperso-orange.fr/Fortran/tbessi_f90.txt (12/31/14)
!>* -------------------------------------------------------------------- *
!>*   Reference: From Numath Library By Tuan Dang Trong in Fortran 77.   *
!>*                                                                      *
!>*                               F90 Release 1.2 By J-P Moreau, Paris.  *
!>*                                        (www.jpmoreau.fr)             *
!> ----------------------------------------------------------------------
!> routine tested and compared with both Matlab and IDL output.
!
!> @param X input argument
!
!> @date 12/31/14 MDG 1.0 original, rewritten with EMsoft module calls and renamed
!--------------------------------------------------------------------------
recursive function BesselI1(X) result(BESSI1)
!DEC$ ATTRIBUTES DLLEXPORT :: BesselI1

use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: X
real(kind=dbl)                  :: BESSI1

real(kind=dbl)                  :: Y,AX,BX
real(kind=dbl),parameter        :: P1=0.5D0, P2=0.87890594D0, P3=0.51498869D0, P4=0.15084934D0, &
                                   P5=0.2658733D-1, P6=0.301532D-2, P7=0.32411D-3, Q1=0.39894228D0, &
                                   Q2=-0.3988024D-1, Q3=-0.362018D-2, Q4=0.163801D-2, Q5=-0.1031555D-1, &
                                   Q6=0.2282967D-1, Q7=-0.2895312D-1, Q8=0.1787654D-1, Q9=-0.420059D-2


if(dabs(X).lt.3.75D0) then
      Y=(X/3.75D0)**2
      BESSI1=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
else
      AX=dabs(X)
      Y=3.75D0/AX
      BX=dexp(AX)/dsqrt(AX)
      AX=Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9)))))))
      BESSI1=AX*BX
end if

end function BesselI1

!*****************************************************************************80
!*****************************************************************************80
!*****************************************************************************80
! the functions below are taken from the normal.f90 file posted on 
! http://people.sc.fsu.edu/~jburkardt/f_src/normal/normal.html
!*****************************************************************************80
!*****************************************************************************80
!*****************************************************************************80


recursive function c4_normal_01 ( seed )
!DEC$ ATTRIBUTES DLLEXPORT :: c4_normal_01
!*****************************************************************************80
!
!! C4_NORMAL_01 returns a unit pseudonormal C4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, complex (kind=4) C4_NORMAL_01, a unit pseudonormal value.
!
  implicit none

  complex (kind=4) c4_normal_01
  real (kind=4), parameter :: r4_pi = 3.141592653589793E+00
! real (kind=4) r4_uniform_01
  integer (kind=4) seed
  real (kind=4) v1
  real (kind=4) v2
  real (kind=4) x_c
  real (kind=4) x_r

  v1 = r4_uniform_01 ( seed )
  v2 = r4_uniform_01 ( seed )

  x_r = sqrt ( - 2.0E+00 * log ( v1 ) ) * cos ( 2.0E+00 * r4_pi * v2 )
  x_c = sqrt ( - 2.0E+00 * log ( v1 ) ) * sin ( 2.0E+00 * r4_pi * v2 )

  c4_normal_01 = cmplx ( x_r, x_c )

  return
end
recursive function c8_normal_01 ( seed )
!DEC$ ATTRIBUTES DLLEXPORT :: c8_normal_01

!*****************************************************************************80
!
!! C8_NORMAL_01 returns a unit pseudonormal C8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer (kind=4) SEED, a seed for the random number
!    generator.
!
!    Output, complex (kind=8) C8_NORMAL_01, a sample of the PDF.
!
  implicit none

  complex (kind=8) c8_normal_01
  real (kind=8), parameter :: r8_pi = 3.141592653589793D+00
! real (kind=8) r8_uniform_01
  integer (kind=4) seed
  real (kind=8) v1
  real (kind=8) v2
  real (kind=8) x_c
  real (kind=8) x_r

  v1 = r8_uniform_01 ( seed )
  v2 = r8_uniform_01 ( seed )

  x_r = sqrt ( - 2.0D+00 * log ( v1 ) ) * cos ( 2.0D+00 * r8_pi * v2 )
  x_c = sqrt ( - 2.0D+00 * log ( v1 ) ) * sin ( 2.0D+00 * r8_pi * v2 )

  c8_normal_01 = cmplx ( x_r, x_c,kind=8)

  return
end
recursive function i4_huge ( )
!DEC$ ATTRIBUTES DLLEXPORT :: i4_huge

!*****************************************************************************80
!
!! I4_HUGE returns a "huge" I4.
!
!  Discussion:
!
!    On an IEEE 32 bit machine, I4_HUGE should be 2^31 - 1, and its
!    bit pattern should be
!
!     01111111111111111111111111111111
!
!    In this case, its numerical value is 2147483647.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer (kind=4) I4_HUGE, a "huge" I4.
!
  implicit none

  integer (kind=4) i4
  integer (kind=4) i4_huge

  i4_huge = 2147483647

  return
end
recursive function i4_normal_ab ( a, b, seed )
!DEC$ ATTRIBUTES DLLEXPORT :: i4_normal_ab

!*****************************************************************************80
!
!! I4_NORMAL_AB returns a scaled pseudonormal I4.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) is sampled,
!    with mean A and standard deviation B.
!
!    The result is then rounded to the nearest integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) A, the mean of the PDF.
!
!    Input, real (kind=4) B, the standard deviation of the PDF.
!
!    Input/output, integer (kind=4) SEED, a seed for the
!    random number generator.
!
!    Output, integer (kind=4) I4_NORMAL_AB, a sample of the normal PDF.
!
  implicit none

  real (kind=4) a
  real (kind=4) b
  integer (kind=4) i4_normal_ab
  real (kind=4) r1
  real (kind=4) r2
  real (kind=4), parameter :: r4_pi = 3.141592653589793E+00
! real (kind=4) r4_uniform_01
  integer (kind=4) seed
  real (kind=4) x

  r1 = r4_uniform_01 ( seed )
  r2 = r4_uniform_01 ( seed )
  x = sqrt ( - 2.0E+00 * log ( r1 ) ) * cos ( 2.0E+00 * r4_pi * r2 )

  i4_normal_ab = nint ( a + b * x )

  return
end
recursive function i8_normal_ab ( a, b, seed )
!DEC$ ATTRIBUTES DLLEXPORT :: i8_normal_ab

!*****************************************************************************80
!
!! I8_NORMAL_AB returns a scaled pseudonormal I8.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) is sampled,
!    with mean A and standard deviation B.
!
!    The result is then rounded to the nearest integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=8) A, the mean of the PDF.
!
!    Input, real (kind=8) B, the standard deviation of the PDF.
!
!    Input/output, integer (kind=8) SEED, a seed for the
!    random number generator.
!
!    Output, integer (kind=8) I8_NORMAL_AB, a sample of the normal PDF.
!
  implicit none

  real (kind=8) a
  real (kind=8) b
  integer (kind=8) i8_normal_ab
  real (kind=8) r1
  real (kind=8) r2
  real (kind=8), parameter :: r8_pi = 3.141592653589793D+00
! real (kind=8) r8_uniform_01
! integer (kind=8) seed     ! compiler error, type mismatch; corrected MDG 01/02/15
  integer (kind=4) seed
  real (kind=8) x

  r1 = r8_uniform_01 ( seed )
  r2 = r8_uniform_01 ( seed )
  x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * r8_pi * r2 )

  i8_normal_ab = nint ( a + b * x )

  return
end
recursive function r4_normal_01 ( seed )
!DEC$ ATTRIBUTES DLLEXPORT :: r4_normal_01

!*****************************************************************************80
!
!! R4_NORMAL_01 returns a unit pseudonormal R4.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=4) R4_NORMAL_01, a sample of the standard
!    normal PDF.
!
  implicit none

  real (kind=4) r1
  real (kind=4) r2
  real (kind=4) r4_normal_01
  real (kind=4), parameter :: r4_pi = 3.141592653589793E+00
! real (kind=4) r4_uniform_01
  integer (kind=4) seed
  real (kind=4) x

  r1 = r4_uniform_01 ( seed )
  r2 = r4_uniform_01 ( seed )
  x = sqrt ( - 2.0E+00 * log ( r1 ) ) * cos ( 2.0E+00 * r4_pi * r2 )

  r4_normal_01 = x

  return
end
recursive function r4_normal_ab ( a, b, seed )
!DEC$ ATTRIBUTES DLLEXPORT :: r4_normal_ab

!*****************************************************************************80
!
!! R4_NORMAL_AB returns a scaled pseudonormal R4.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) is sampled,
!    with mean A and standard deviation B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) A, the mean of the PDF.
!
!    Input, real (kind=4) B, the standard deviation of the PDF.
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=4) R4_NORMAL_AB, a sample of the normal PDF.
!
  implicit none

  real (kind=4) a
  real (kind=4) b
  real (kind=4) r1
  real (kind=4) r2
  real (kind=4) r4_normal_ab
  real (kind=4), parameter :: r4_pi = 3.141592653589793E+00
! real (kind=4) r4_uniform_01
  integer (kind=4) seed
  real (kind=4) x

  r1 = r4_uniform_01 ( seed )
  r2 = r4_uniform_01 ( seed )
  x = sqrt ( - 2.0E+00 * log ( r1 ) ) * cos ( 2.0E+00 * r4_pi * r2 )

  r4_normal_ab = a + b * x

  return
end

recursive function r4_uniform_01 ( seed )
!DEC$ ATTRIBUTES DLLEXPORT :: r4_uniform_01

!*****************************************************************************80
!
!! R4_UNIFORM_01 returns a unit pseudorandom R4.
!
!  Discussion:
!
!    An R4 is a real (kind=4) value.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r4_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R4_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input/output, integer (kind=4) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=4) R4_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer (kind=4), parameter :: i4_huge = 2147483647
  integer (kind=4) k
  integer (kind=4) seed
  real (kind=4) r4_uniform_01

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r4_uniform_01 = real ( seed,kind=4) * 4.656612875E-10

  return
end
recursive subroutine r4vec_uniform_01 ( n, seed, r )
!DEC$ ATTRIBUTES DLLEXPORT :: r4vec_uniform_01 

!*****************************************************************************80
!
!! R4VEC_UNIFORM_01 returns a unit pseudorandom R4VEC.
!
!  Discussion:
!
!    An R4VEC is an array of real (kind=4) values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer (kind=4) N, the number of entries in the vector.
!
!    Input/output, integer (kind=4) SEED, the "seed" value,
!    which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real (kind=4) R(N), the vector of pseudorandom values.
!
  implicit none

  integer (kind=4) n

  integer (kind=4) i
  integer (kind=4), parameter :: i4_huge = 2147483647
  integer (kind=4) k
  integer (kind=4) seed
  real (kind=4) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = real ( seed,kind=4) * 4.656612875E-10

  end do

  return
end
recursive subroutine r4vec_normal_ab ( n, a, b, seed, x )
!DEC$ ATTRIBUTES DLLEXPORT :: r4vec_normal_ab 

!*****************************************************************************80
!
!! R4VEC_NORMAL_AB returns a scaled pseudonormal R4VEC.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    An R4VEC is a vector of R4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) N, the number of values desired.
!
!    Input, real (kind=4) A, B, the mean and standard deviation.
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=4) X(N), a sample of the standard normal PDF.
!
!  Local parameters:
!
!    Local, real (kind=4) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    Local, integer (kind=4) X_LO_INDEX, X_HI_INDEX, records the range
!    of entries of X that we need to compute.
!
  implicit none

  integer (kind=4) n

  real (kind=4) a
  real (kind=4) b
  integer (kind=4) m
  real (kind=4) r(n+1)
  real (kind=4), parameter :: r4_pi = 3.141592653589793E+00
! real (kind=4) r4_uniform_01
  integer (kind=4) seed
  real (kind=4) x(n)
  integer (kind=4) x_hi_index
  integer (kind=4) x_lo_index
!
!  Record the range of X we need to fill in.
!
  x_lo_index = 1
  x_hi_index = n
!
!  If we need just one new value, do that here to avoid null arrays.
!
  if ( x_hi_index - x_lo_index + 1 == 1 ) then

    r(1) = r4_uniform_01 ( seed )

    if ( r(1) == 0.0E+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4VEC_NORMAL_AB - Fatal error!'
      write ( *, '(a)' ) '  R4_UNIFORM_01 returned a value of 0.'
      stop 1
    end if

    r(2) = r4_uniform_01 ( seed )

    x(x_hi_index) = &
      sqrt ( - 2.0E+00 * log ( r(1) ) ) * cos ( 2.0E+00 * r4_pi * r(2) )
!
!  If we require an even number of values, that's easy.
!
  else if ( mod ( x_hi_index - x_lo_index, 2 ) == 1 ) then

    m = ( x_hi_index - x_lo_index + 1 ) / 2

    call r4vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0E+00 * log ( r(1:2*m-1:2) ) ) &
      * cos ( 2.0E+00 * r4_pi * r(2:2*m:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0E+00 * log ( r(1:2*m-1:2) ) ) &
      * sin ( 2.0E+00 * r4_pi * r(2:2*m:2) )
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
  else

    x_hi_index = x_hi_index - 1

    m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

    call r4vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0E+00 * log ( r(1:2*m-3:2) ) ) &
      * cos ( 2.0E+00 * r4_pi * r(2:2*m-2:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0E+00 * log ( r(1:2*m-3:2) ) ) &
      * sin ( 2.0E+00 * r4_pi * r(2:2*m-2:2) )

    x(n) = sqrt ( - 2.0E+00 * log ( r(2*m-1) ) ) &
      * cos ( 2.0E+00 * r4_pi * r(2*m) )

  end if

  x(1:n) = a + b * x(1:n)

  return
end
recursive function r8_normal_01 ( seed )
!DEC$ ATTRIBUTES DLLEXPORT :: r8_normal_01

!*****************************************************************************80
!
!! R8_NORMAL_01 returns a unit pseudonormal R8.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=8) R8_NORMAL_01, a normally distributed
!    random value.
!
  implicit none

  real (kind=8) r1
  real (kind=8) r2
  real (kind=8) r8_normal_01
  real (kind=8), parameter :: r8_pi = 3.141592653589793D+00
! real (kind=8) r8_uniform_01
  integer (kind=4) seed
  real (kind=8) x

  r1 = r8_uniform_01 ( seed )
  r2 = r8_uniform_01 ( seed )
  x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * r8_pi * r2 )

  r8_normal_01 = x

  return
end
recursive function r8_normal_ab ( a, b, seed )
!DEC$ ATTRIBUTES DLLEXPORT :: r8_normal_ab

!*****************************************************************************80
!
!! R8_NORMAL_AB returns a scaled pseudonormal R8.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) is sampled,
!    with mean A and standard deviation B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=8) A, the mean of the PDF.
!
!    Input, real (kind=8) B, the standard deviation of the PDF.
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=8) R8_NORMAL_AB, a sample of the normal PDF.
!
  implicit none

  real (kind=8) a
  real (kind=8) b
  real (kind=8) r1
  real (kind=8) r2
  real (kind=8) r8_normal_ab
  real (kind=8), parameter :: r8_pi = 3.141592653589793D+00
! real (kind=8) r8_uniform_01
  integer (kind=4) seed
  real (kind=8) x

  r1 = r8_uniform_01 ( seed )
  r2 = r8_uniform_01 ( seed )
  x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * r8_pi * r2 )

  r8_normal_ab = a + b * x

  return
end
recursive function r8_uniform_01 ( seed )
!DEC$ ATTRIBUTES DLLEXPORT :: r8_uniform_01

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, 1969, pages 136-143.
!
!  Parameters:
!
!    Input/output, integer (kind=4) SEED, the "seed" value, which
!    should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real (kind=8) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer (kind=4) k
  real (kind=8) r8_uniform_01
  integer (kind=4) seed

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + 2147483647
  end if
!
!  Although SEED can be represented exactly as a 32 bit integer,
!  it generally cannot be represented exactly as a 32 bit real number!
!
  r8_uniform_01 = real ( seed,kind=8) * 4.656612875D-10

  return
end
recursive subroutine r8mat_normal_01 ( m, n, seed, r )
!DEC$ ATTRIBUTES DLLEXPORT :: r8mat_normal_01 

!*****************************************************************************80
!
!! R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer (kind=4) M, N, the number of rows and columns
!    in the array.
!
!    Input/output, integer (kind=4) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=8) R(M,N), the array of pseudonormal values.
!
  implicit none

  integer (kind=4) m
  integer (kind=4) n

  integer (kind=4) seed
  real (kind=8) r(m,n)

  call r8vec_normal_01 ( m * n, seed, r )

  return
end
recursive subroutine r8mat_normal_ab ( m, n, a, b, seed, r )
!DEC$ ATTRIBUTES DLLEXPORT :: r8mat_normal_ab 

!*****************************************************************************80
!
!! R8MAT_NORMAL_AB returns a scaled pseudonormal R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer (kind=4) M, N, the number of rows and columns
!    in the array.
!
!    Input, real (kind=8) A, B, the mean and standard deviation.
!
!    Input/output, integer (kind=4) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=8) R(M,N), the array of pseudonormal values.
!
  implicit none

  integer (kind=4) m
  integer (kind=4) n

  real (kind=8) a
  real (kind=8) b
  integer (kind=4) seed
  real (kind=8) r(m,n)

  call r8vec_normal_ab ( m * n, a, b, seed, r )

  return
end
recursive subroutine r8vec_normal_01 ( n, seed, x )
!DEC$ ATTRIBUTES DLLEXPORT :: r8vec_normal_01 

!*****************************************************************************80
!
!! R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
!
!  Discussion:
!
!    An R8VEC is an array of double precision real values.
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) N, the number of values desired.
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=8) X(N), a sample of the standard normal PDF.
!
!  Local parameters:
!
!    Local, real (kind=8) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    Local, integer (kind=4) X_LO_INDEX, X_HI_INDEX, records the range
!    of entries of X that we need to compute
!
  implicit none

  integer (kind=4) n

  integer (kind=4) m
  real (kind=8) r(n+1)
  real (kind=8), parameter :: r8_pi = 3.141592653589793D+00
! real (kind=8) r8_uniform_01
  integer (kind=4) seed
  real (kind=8) x(n)
  integer (kind=4) x_hi_index
  integer (kind=4) x_lo_index
!
!  Record the range of X we need to fill in.
!
  x_lo_index = 1
  x_hi_index = n
!
!  If we need just one new value, do that here to avoid null arrays.
!
  if ( x_hi_index - x_lo_index + 1 == 1 ) then

    r(1) = r8_uniform_01 ( seed )

    if ( r(1) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop 1
    end if

    r(2) = r8_uniform_01 ( seed )

    x(x_hi_index) = &
      sqrt ( - 2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * r8_pi * r(2) )
!
!  If we require an even number of values, that's easy.
!
  else if ( mod ( x_hi_index - x_lo_index, 2 ) == 1 ) then

    m = ( x_hi_index - x_lo_index + 1 ) / 2

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m:2) )
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
  else

    x_hi_index = x_hi_index - 1

    m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(n) = sqrt ( - 2.0D+00 * log ( r(2*m-1) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2*m) )

  end if

  return
end
recursive subroutine r8vec_normal_ab ( n, a, b, seed, x )
!DEC$ ATTRIBUTES DLLEXPORT :: r8vec_normal_ab 

!*****************************************************************************80
!
!! R8VEC_NORMAL_AB returns a scaled pseudonormal R8VEC.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) N, the number of values desired.
!
!    Input, real (kind=8) A, B, the mean and standard deviation.
!
!    Input/output, integer (kind=4) SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=8) X(N), a sample of the standard normal PDF.
!
!  Local parameters:
!
!    Local, real (kind=8) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    Local, integer (kind=4) X_LO_INDEX, X_HI_INDEX, records the range
!    of entries of X that we need to compute. 
!
  implicit none

  integer (kind=4) n

  real (kind=8) a
  real (kind=8) b
  integer (kind=4) m
  real (kind=8) r(n+1)
  real (kind=8), parameter :: r8_pi = 3.141592653589793D+00
! real (kind=8) r8_uniform_01
  integer (kind=4) seed
  real (kind=8) x(n)
  integer (kind=4) x_hi_index
  integer (kind=4) x_lo_index
!
!  Record the range of X we need to fill in.
!
  x_lo_index = 1
  x_hi_index = n
!
!  If we need just one new value, do that here to avoid null arrays.
!
  if ( x_hi_index - x_lo_index + 1 == 1 ) then

    r(1) = r8_uniform_01 ( seed )

    if ( r(1) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop 1
    end if

    r(2) = r8_uniform_01 ( seed )

    x(x_hi_index) = &
      sqrt ( - 2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * r8_pi * r(2) )
!
!  If we require an even number of values, that's easy.
!
  else if ( mod ( x_hi_index - x_lo_index, 2 ) == 1 ) then

    m = ( x_hi_index - x_lo_index + 1 ) / 2

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m:2) )
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
  else

    x_hi_index = x_hi_index - 1

    m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(n) = sqrt ( - 2.0D+00 * log ( r(2*m-1) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2*m) )

  end if

  x(1:n) = a + b * x(1:n)

  return
end
recursive subroutine r8vec_uniform_01 ( n, seed, r )
!DEC$ ATTRIBUTES DLLEXPORT :: r8vec_uniform_01 

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer (kind=4) N, the number of entries in the vector.
!
!    Input/output, integer (kind=4) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=8) R(N), the vector of pseudorandom values.
!
  implicit none

  integer (kind=4) n

  integer (kind=4) i
  integer (kind=4) k
  integer (kind=4) seed
  real (kind=8) r(n)

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = real ( seed,kind=8) * 4.656612875D-10

  end do

  return
end



!--------------------------------------------------------------------------
!
! SUBROUTINE: TransFourthRankTensor 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  transform a fourth rank tensor using a given transformation matrix
! 
!> @note This is one of the very few places in this package where we still use the 
!> good old-fashioned rotation matrix instead of quaternions... Note also that we
!> use the 6x6 notation for the tensors, so we need to convert them to real tensor
!> notation before carrying out the rotations.
!
!> @param al rotation matrix
!> @param cin unrotated tensor
!> @param cout rotated tensor
! 
!> @date 1/5/99   MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   06/04/13 MDG 3.0 rewrite
!--------------------------------------------------------------------------
recursive subroutine TransFourthRankTensor(al,cin,cout)
!DEC$ ATTRIBUTES DLLEXPORT :: TransFourthRankTensor

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: al(3,3)
real(kind=sgl),INTENT(IN)       :: cin(6,6)
real(kind=sgl),INTENT(OUT)      :: cout(6,6) 

real(kind=sgl)                          :: cold(3,3,3,3), cnew(3,3,3,3)
integer(kind=irg)                       :: i,j,k,l,p,q,r,s,delta(3,3),gamma(6,2)

! initalize a bunch of variables
cold = 0.0
cnew = 0.0
cout = 0.0
delta(1,1) = 1; delta(1,2) = 6; delta(1,3) = 5
delta(2,1) = 6; delta(2,2) = 2; delta(2,3) = 4
delta(3,1) = 5; delta(3,2) = 4; delta(3,3) = 3
gamma(1,1) = 1; gamma(1,2) = 1
gamma(2,1) = 2; gamma(2,2) = 2
gamma(3,1) = 3; gamma(3,2) = 3
gamma(4,1) = 2; gamma(4,2) = 3
gamma(5,1) = 1; gamma(5,2) = 3
gamma(6,1) = 1; gamma(6,2) = 2

! convert to real tensor indices
do i=1,3
 do j=1,3
  do k=1,3
   do l=1,3
    cold(i,j,k,l) = cin(delta(i,j),delta(k,l))
   end do
  end do
 end do
end do

! and transform
do i=1,3
 do j=1,3
  do k=1,3
   do l=1,3
    do p=1,3
     do q=1,3
      do r=1,3
       do s=1,3
        cnew(i,j,k,l) = cnew(i,j,k,l) + al(i,p)*al(j,q)*al(k,r)*al(l,s)*cold(p,q,r,s)
       end do
      end do
     end do
    end do
   end do
  end do
 end do
end do

! and convert to 6-index notation again
do i=1,6
 do j=1,6
  cout(i,j) = cnew(gamma(i,1),gamma(i,2),gamma(j,1),gamma(j,2))
 end do
end do
! That's it.

end subroutine TransFourthRankTensor


!--------------------------------------------------------------------------
!
! SUBROUTINE: laguer
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Laguerre polynomial, based on Numerical Recipes routine of the same name
!
!> @details but adapted to the sextic equation case considered here.
!
!> @param a
!> @param m
!> @param x
!> @param eps
!> @param polish
! 
!> @date 1/5/99   MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   06/04/13 MDG 3.0 rewrite
!--------------------------------------------------------------------------
recursive subroutine laguer(a,m,x,eps,polish)
!DEC$ ATTRIBUTES DLLEXPORT :: laguer

use local

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)    :: a(*)
integer(kind=irg),INTENT(IN)    :: m
complex(kind=dbl),INTENT(INOUT) :: x
!f2py intent(in,out) ::  x
real(kind=sgl),INTENT(IN)       :: eps
logical,INTENT(IN)              :: polish

complex(kind=dbl)               :: dx,x1,b,d,f,g,h,sq,gp,gm,g2,zero
real(kind=sgl),parameter        :: epss=6.E-8
integer(kind=irg),parameter     :: maxit=1000
integer(kind=irg)               :: iter,j
real(kind=dbl)                  :: dxold,errr,abx,cdx

      zero = cmplx(0.0,0.0,dbl)
      dxold = abs(x)
      
      do iter=1,maxit
        b=a(m+1)
        errr=abs(b)
        d=zero
        f=zero
        abx=abs(x)
        do j=m,1,-1
          f=x*f+d
          d=x*d+b
          b=x*b+a(j)
          errr=abs(b)+abx*errr
        end do
        errr=epss*errr
        if (abs(b).le.errr) then
          dx=zero
          return
        else
          g=d/b
          g2=g*g
          h=g2-2.D0*f/b
          sq=sqrt((m-1)*(m*h-g2))
          gp=g+sq
          gm=g-sq
          if (abs(gp).lt.abs(gm)) gp=gm
          dx=m/gp
        end if
        x1=x-dx
        if (x.eq.x1) return
        x=x1
        cdx=abs(dx)
        if ((iter.gt.6).and.(cdx.ge.dxold)) return
        dxold=cdx
        if (.NOT.polish) then
          if (abs(dx).le.eps*abs(x)) return
        end if
     end do
     STOP 'too many iterations'
     return
end subroutine laguer

!--------------------------------------------------------------------------
!
! SUBROUTINE: zroots
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Polynomial roots, based on Numerical Recipes routine of the same name
!
!> @details but adapted to the sextic equation case considered here.
!
!> @param a
!> @param roots
! 
!> @date 1/5/99   MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   06/04/13 MDG 3.0 rewrite
!--------------------------------------------------------------------------
recursive subroutine zroots(a,roots)
!DEC$ ATTRIBUTES DLLEXPORT :: zroots

use local

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)            :: a(7)
complex(kind=dbl),INTENT(OUT)           ::roots(6)
integer(kind=irg)                                       :: i,j,jj,m
real,parameter                                          :: eps = 1.E-6
complex(kind=dbl)                               :: ad(7), x, b, c, czero
 
 m=6
 czero = cmplx(0.0,0.0,dbl)
 do j=1,7
  ad(j) = a(j)
 end do

! get the roots
 do j=6,1,-1
  x = czero
  call laguer(ad,j,x,eps,.FALSE.)
  if (abs(aimag(x)).le.2.*eps**2*abs(real(x))) x=cmplx(real(x),0.D0,dbl)
  roots(j)=x
  b=ad(j+1)
  do jj=j,1,-1
    c=ad(jj)
    ad(jj)=b
    b=x*b+c
  end do
 end do

! polish the roots
 do j=1,6
  call laguer(a,m,roots(j),eps,.TRUE.)
 end do

! and prepare to return to the calling routine
 outerloop: do j=2,6
  x=roots(j)
  innerloop: do i=j-1,1,-1
   if (real(roots(i)).le.real(x)) then
     roots(i+1) = x
     cycle outerloop
   end if
   roots(i+1)=roots(i)
  end do innerloop
  i=0
  roots(i+1)=x
 end do outerloop
 return
end subroutine

!------------------------
! these following three routines should be replaced by something else in the io.f90 file
! using a proper interface for all possible formats
!------------------------
recursive subroutine PrintMatrix(s,a)
!DEC$ ATTRIBUTES DLLEXPORT :: PrintMatrix

use local
use io

IMPLICIT NONE

real(kind=sgl)   :: a(3,3)
integer(kind=irg):: i,j
character(4)     :: s

write (*,"(A/)") s
do i=1,3
  write (*,"(3(F12.5,2x))") (a(i,j),j=1,3)
end do
write (*,"(/)")

end subroutine

!--------------------------------------------------------------------------
!
! FUNCTION: point_inside_triangle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  determines whether or not a point lies inside or outside a triangle (in 2D)
!
!> @details based on http://www.blackpawn.com/texts/pointinpoly/default.html
! 
!> @param v0 vertex coordinates
!> @param v1 vertex coordinates
!> @param v2 vertex coordinates
! 
!> @date 1/5/99   MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
logical recursive function point_inside_triangle(v0,v1,v2)
!DEC$ ATTRIBUTES DLLEXPORT :: point_inside_triangle

IMPLICIT NONE

real(kind=sgl),INTENT(IN)       :: v0(2), v1(2), v2(2) 
real(kind=sgl)                  :: dot00, dot01, dot02, dot11, dot12, invdenom, u, v
logical                         :: inside

dot00 = v0(1)*v0(1) + v0(2)*v0(2) ! matmul(v0,v0)
dot01 = v0(1)*v1(1) + v0(2)*v1(2) ! matmul(v0,v1)
dot02 = v0(1)*v2(1) + v0(2)*v2(2) ! matmul(v0,v2)
dot11 = v1(1)*v1(1) + v1(2)*v1(2) ! matmul(v1,v1)
dot12 = v1(1)*v2(1) + v1(2)*v2(2) ! matmul(v1,v2)

! use barycentric coordinates
invdenom = 1.0 / (dot00*dot11 - dot01*dot01)
u = (dot11*dot02 - dot01*dot12)*invdenom
v = (dot00*dot12 - dot01*dot02)*invdenom

inside = .FALSE.
if ((u.ge.0.0).and.(v.ge.0.0).and.((u+v).le.1.0)) inside=.TRUE.

point_inside_triangle = inside

end function point_inside_triangle


!--------------------------------------------------------------------------
!
! SUBROUTINE: rank_points
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief rank points for inside/outside analysis
!
!> @details based on http://www.blackpawn.com/texts/pointinpoly/default.html
! 
!> @param p1 vertex coordinates
!> @param p2 vertex coordinates
!> @param p3 vertex coordinates
!> @param p4 vertex coordinates
!> @param xx sorted x coordinates
!> @param yy sorted y coordinates
! 
!> @date 1/5/99   MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine rank_points(p1,p2,p3,p4,xx,yy)
!DEC$ ATTRIBUTES DLLEXPORT :: rank_points

IMPLICIT NONE

real(kind=sgl),INTENT(IN)       :: p1(2), p2(2), p3(2), p4(2) 
real(kind=sgl),INTENT(OUT)      :: xx(4), yy(4) 
real(kind=sgl)                  ::a(6), ma 

! set the first point
xx(1) = p1(1)
yy(1) = p1(2)

! compute the areas of six permutations
a(1) = p1(1)*p2(2)-p1(2)*p2(1) + p2(1)*p3(2)-p2(2)*p3(1) + p3(1)*p4(2)-p3(2)*p4(1) + p4(1)*p1(2)-p4(2)*p1(1)
a(2) = p1(1)*p2(2)-p1(2)*p2(1) + p2(1)*p4(2)-p2(2)*p4(1) + p4(1)*p3(2)-p4(2)*p3(1) + p3(1)*p1(2)-p3(2)*p1(1)
a(3) = p1(1)*p3(2)-p1(2)*p3(1) + p3(1)*p2(2)-p3(2)*p2(1) + p2(1)*p4(2)-p2(2)*p4(1) + p4(1)*p1(2)-p4(2)*p1(1)
a(4) = p1(1)*p3(2)-p1(2)*p3(1) + p3(1)*p4(2)-p3(2)*p4(1) + p4(1)*p2(2)-p4(2)*p2(1) + p2(1)*p1(2)-p2(2)*p1(1)
a(5) = p1(1)*p4(2)-p1(2)*p4(1) + p4(1)*p2(2)-p4(2)*p2(1) + p2(1)*p3(2)-p2(2)*p3(1) + p3(1)*p1(2)-p3(2)*p1(1)
a(6) = p1(1)*p4(2)-p1(2)*p4(1) + p4(1)*p3(2)-p4(2)*p3(1) + p3(1)*p2(2)-p3(2)*p2(1) + p2(1)*p1(2)-p2(2)*p1(1)
a = abs(a)
ma = maxval(a)

if (a(1).eq.ma) then 
  xx(2:4) = (/ p2(1),p3(1),p4(1) /)
  yy(2:4) = (/ p2(2),p3(2),p4(2) /)
  return
end if
if (a(2).eq.ma) then 
  xx(2:4) = (/ p2(1),p4(1),p3(1) /)
  yy(2:4) = (/ p2(2),p4(2),p3(2) /)
  return
end if
if (a(3).eq.ma) then 
  xx(2:4) = (/ p3(1),p2(1),p4(1) /)
  yy(2:4) = (/ p3(2),p2(2),p4(2) /)
  return
end if
if (a(4).eq.ma) then 
  xx(2:4) = (/ p3(1),p4(1),p2(1) /)
  yy(2:4) = (/ p3(2),p4(2),p2(2) /)
  return
end if
if (a(5).eq.ma) then 
  xx(2:4) = (/ p4(1),p2(1),p3(1) /)
  yy(2:4) = (/ p4(2),p2(2),p3(2) /)
  return
end if
if (a(6).eq.ma) then 
  xx(2:4) = (/ p4(1),p3(1),p2(1) /)
  yy(2:4) = (/ p4(2),p3(2),p2(2) /)
end if

end subroutine rank_points



!--------------------------------------------------------------------------
!
! FUNCTION: point_inside_polygon
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  determines whether or not a point lies inside or outside a polygon (in 2D)
!
!> @details based on http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html (Fortran version)
! 
!> @note Copyright (c) 1970-2003, Wm. Randolph Franklin
!>
!> Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
!> and associated documentation files (the "Software"), to deal in the Software without restriction, 
!> including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, 
!> and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, 
!> subject to the following conditions:

!> Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers.
!> Redistributions in binary form must reproduce the above copyright notice in the documentation and/or 
!> other materials provided with the distribution.
!> The name of W. Randolph Franklin may not be used to endorse or promote products derived from this Software 
!> without specific prior written permission.
!> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
!> TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
!> THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
!> CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
!> DEALINGS IN THE SOFTWARE.
!
!> @param px point coordinate x
!> @param py point coordinate y
!> @param xx vertex coordinates
!> @param yy vertex coordinates
! 
!> @date 1/5/99   MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive function point_inside_polygon(px,py,xx,yy) result(inorout)
!DEC$ ATTRIBUTES DLLEXPORT :: point_inside_polygon

IMPLICIT NONE

real(kind=sgl),INTENT(IN)       :: px, py, xx(4), yy(4)
real(kind=sgl)                  :: x(4), y(4), z
integer(kind=irg)               :: i, j, inorout
logical                         :: mx,my,nx,ny

x = xx-px
y = yy-py
inorout = -1

do i=1,4
  j = 1+mod(i,4)
  mx = x(i).ge.0.0
  nx = x(j).ge.0.0
  my = y(i).ge.0.0
  ny = y(j).ge.0.0
  if (.not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)) cycle
  if (.not.(my.and.ny.AND.(mx.or.nx).and..not.(mx.and.nx))) then
    z  = (y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))
    if (z.lt.0.0) cycle
    if (z.eq.0.0) then 
      inorout = 0
      exit
    end if
    if (z.gt.0.0) inorout=-inorout
  else
    inorout = -inorout
    cycle
  endif
end do

end function point_inside_polygon


!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! the following routines are taken from Numerical Recipes and need to be 
! replaced by routines in the public domain ... 
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------

recursive FUNCTION EL2(X,QQC,AA,BB) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: EL2

use local
use constants
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)     :: X
real(kind=dbl),INTENT(IN)     :: QQC
real(kind=dbl),INTENT(IN)     :: AA
real(kind=dbl),INTENT(IN)     :: BB
real(kind=dbl)                :: res

real(kind=dbl),PARAMETER      :: CA=.0003D0, CB=1.D-9
real(kind=dbl)                :: QC, A, B, C, D, E, P, Z, EYE, Y, F, EM, G
integer(kind=irg)             :: L

if (X.EQ.0.D0) then
 res=0.D0
else if (QQC.NE.0.D0) then
  QC=QQC
  A=AA
  B=BB
  C=X**2
  D=1.D0+C
  P=DSQRT((1.D0+QC**2*C)/D)
  D=X/D
  C=D/(2.D0*P)
  Z=A-B
  EYE=A
  A=0.5D0*(B+A)
  Y=ABS(1.D0/X)
  F=0.D0
  L=0
  EM=1.D0
  QC=ABS(QC)
1 B=EYE*QC+B
  E=EM*QC
  G=E/P
  D=F*G+D
  F=C
  EYE=A
  P=G+P
  C=0.5D0*(D/P+C)
  G=EM
  EM=QC+EM
  A=0.5D0*(B/EM+A)
  Y=-E/Y+Y
  if (Y.EQ.0.) Y=DSQRT(E)*CB
  if (ABS(G-QC).GT.CA*G) then
    QC=DSQRT(E)*2.D0
    L=L+L
    if (Y.LT.0.D0) L=L+1
    go to 1
  endif
  if (Y.LT.0.) L=L+1
  E=(DATAN(EM/Y)+cPi*L)*A/EM
  if (X.LT.0.D0) E=-E
  res=E+C*Z
else
  call FatalError('EL2','Error in Elliptic Integral routine')
endif
end function EL2


recursive function el1k(phi,k) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: el1k
!
! Legendre elliptic integral of the first kind F(phi,k)
!
! conversion of parameters to standardized symbols

use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: phi
real(kind=dbl),INTENT(IN)       :: k
real(kind=dbl)                  :: res

real(kind=dbl)                  :: x, kc

x = dtan(phi)
kc = dsqrt(1.D0-k**2)

! call Numerical Recipes routine
res = el2(x,kc,1.D0,1.D0)
end function el1k

recursive function el2k(phi,k) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: el2k
!
! Legendre elliptic integral of the second kind E(phi,k)
!
! conversion of parameters to standardized symbols

use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: phi
real(kind=dbl),INTENT(IN)       :: k
real(kind=dbl)                  :: res

real(kind=dbl)                  :: x, kc

x=tan(phi)
kc = sqrt(1.D0-k**2)

! call Numerical Recipes routine
res = el2(x,kc,1.D0,kc**2)
end function el2k

!--------------------------------------------------------------------------
!
! FUNCTION: kdelta
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Kronecker delta 
!
!> @param i first integer subscript
!> @param j second integer subscript
! 
!> @date  12/27/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function kdelta(i,j) result(k)
!DEC$ ATTRIBUTES DLLEXPORT :: kdelta

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: i,j
integer(kind=irg)               :: k

k=0
if (i.eq.j) k=1 

end function kdelta

!--------------------------------------------------------------------------
!
! SUBROUTINE: cubicroots
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  all three roots of cubic polynomial with real coefficients 
!
!> @details the equations were taken from wikipedia article https://en.wikipedia.org/wiki/Cubic_function
!
!> @param co  coefficients in co1 x^3 + co2 x^2 + co3 x + co4 = 0
!> @param roots
!
!> @date 01/18/16   SS 1.0 original
!> @date 01/25/16  MDG 1.1 minor mods to make more efficient
!> @date 09/13/19  MDG 2.0 rewrite, verified against Mathematica for a couple of trial cases
!--------------------------------------------------------------------------
recursive subroutine cubicroots(co,X)
!DEC$ ATTRIBUTES DLLEXPORT :: cubicroots

use local

IMPLICIT NONE

real(kind=dbl),INTENT(IN)            :: co(4)
complex(kind=dbl),INTENT(OUT)        :: X(3)

real(kind=dbl)                       :: del0, del1 
complex(kind=dbl)                    :: u(3), C, pre
integer(kind=irg)                    :: i

del0 = co(2)*co(2) - 3.D0*co(1)*co(3)
del1 = 2.D0*co(2)**3 - 9.D0*co(1)*co(2)*co(3) + 27.D0*co(4)*co(1)**2;
C = (0.5D0*(-del1 + dsqrt(del1*del1 - 4.D0*del0**3)))**(1.D0/3.D0);

u(1) = cmplx(1.D0,0.D0);
u(2) = cmplx(-0.5D0,dsqrt(3.D0)*0.5D0)
u(3) = cmplx(-0.5D0,-dsqrt(3.D0)*0.5D0)

pre = cmplx(-1.D0/3.D0/co(1),0.D0)
X(1) = pre * (cmplx(co(2),0.D0) - C - cmplx(del0,0.D0)/C);
X(2) = pre * (cmplx(co(2),0.D0) + u(3) * C + u(2) * cmplx(del0,0.D0)/C);
X(3) = pre * (cmplx(co(2),0.D0) + u(2) * C + u(3) * cmplx(del0,0.D0)/C);

end subroutine cubicroots
!--------------------------------------------------------------------------
!
! SUBROUTINE: KClusterWeights
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Weight factor computation for KCluster routine
!
!> @param Matrix Input matrix
!> @param NRow number of rows in matrix
!> @param NCol number of columns
!> @param NClusters number of clusters
!> @param Wts returns the Weights array
! 
!> @date  12/27/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine KClusterWeights(Matrix, NRow, NCol, NClusters, Niter, Wts)
!DEC$ ATTRIBUTES DLLEXPORT :: KClusterWeights

use local
use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: NRow
integer(kind=irg),INTENT(IN)    :: NCol
real(kind=dbl),INTENT(IN)       :: Matrix(NCol, NRow)
integer(kind=irg),INTENT(IN)    :: NClusters
integer(kind=irg),INTENT(IN)    :: Niter
real(kind=dbl),INTENT(OUT)      :: Wts(NCol, NCLusters)

integer(kind=irg)               :: i, j, k, cnt, cr, cm, mloc(1), mnm, seed, io_int(2), ier
real(kind=dbl)                  :: ILR, DLR, V(NCol,NClusters), rn(NCol), s, M(NClusters)
integer(kind=irg)               :: idx(NClusters)
real(kind=dbl)                  :: MM(NClusters)

call Message('KClusterWeights: determining cluster weight factors')

! initial parameters
io_int(2) = Niter
Wts = 0.D0

ILR = 0.2D0
DLR = 0.1D0 / dble(Niter)

! generate uniformly random cluster weights (normalized).
call SYSTEM_CLOCK(cnt,cr,cm)
seed =  cnt
do k=1,NClusters
  call r8vec_uniform_01 ( NCol, seed, rn )
  Wts(1:NCol,k) = rn(1:NCol)
end do

do k=1,NClusters
  s = 1.D0/sum(Wts(1:NCol,k))
  Wts(1:NCol,k) = Wts(1:NCol,k) * s
end do

! use the spsort.f routine from SLATEC for sorting purposes
!open(unit=dataunit,file='weights.data',status='unknown',form='unformatted')
!write (dataunit) Niter+1, NCol, NClusters
!write (dataunit) Wts

do k=1,Niter
  io_int(1) = k
  call WriteValue('Iteration ',io_int,2,"(I4,' of ',I4)")
  do i=1,NRow
    do j=1,NClusters
      V(1:NCol,j) = Matrix(1:NCol,i)
    end do
    V = V - Wts
    MM = sum( dabs( V ), 1 )
    idx = 0
    call SPSORT(sngl(MM),NClusters,idx,1,ier)

    mnm = idx(1)
    Wts(1:NCol,mnm) = ILR * V(1:NCol,mnm) + Wts(1:NCol,mnm)

    findothers: do j=2,NClusters
        if (MM(idx(j)).eq.MM(idx(1))) then 
          mnm = idx(j)
          Wts(1:NCol,mnm) = ILR * V(1:NCol,mnm) + Wts(1:NCol,mnm)
        else
          EXIT findothers
        end if
    end do findothers
! if (mod(i,5000).eq.0) write (*,*) 'completed pattern ',i
  end do
  ILR = ILR - DLR
!write(dataunit) Wts
end do

!close(unit=dataunit,status='keep')

end subroutine KClusterWeights

!--------------------------------------------------------------------------
!
! SUBROUTINE: KCluster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief a sort of K-means clustering routine, based on CLUSTER ANALYSIS (Third Edition)
!> Brian S. Everitt, ISBN 0-340-58479-3, and IDL implementation in Cluster.pro
!
!> @param Matrix Input matrix
!> @param NRow number of rows in matrix
!> @param NCol number of columns
!> @param NClusters number of cluster to look for
!> @param IndexArray returns the IndexArray
! 
!> @date  12/27/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine KCluster(Matrix, NRow, NCol, NClusters, Niter, IndexArray)
!DEC$ ATTRIBUTES DLLEXPORT :: KCluster

use local
use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: NRow
integer(kind=irg),INTENT(IN)    :: NCol
real(kind=dbl),INTENT(IN)       :: Matrix(NCol,NRow)
integer(kind=irg),INTENT(IN)    :: NClusters
integer(kind=irg),INTENT(IN)    :: Niter
integer(kind=irg),INTENT(OUT)   :: IndexArray(NRow)

real(kind=dbl)                  :: Weights(NCol,NClusters), M(NClusters), tW(NClusters,NCol)
integer(kind=irg)               :: i, j, mloc(1), iaHist(NClusters), Hmin, Hmax, io_int(5), cntarr(NClusters), ebin

! determine the weights 
call KClusterWeights(Matrix, NRow, NCol, NClusters, Niter, Weights)

! and loop over all rows of the input array
tw = transpose(Weights)
do i = 1, NRow
    M = sum( dabs( spread( Matrix(1:NCol,i),dim=1,ncopies=NClusters ) - tw ), 2 )
    mloc = minloc(M)
    IndexArray(i) = mloc(1)
if (mod(i,2500).eq.0) write (*,*) 'done with row ',i
end do

! here we need to analyze the IndexArray; easiest way to do this is to compute 
! its histogram and determine whether or not all the bins have counts in them.
! if they do, then the NClusters variable was likely set too small, so we need 
! to let the user know about this.  If there are bins with no entries in them, 
! then we need to relabel the indices to make sure there are no empty bins
! anywhere... If there are a lot of empty bins, then we also need to let the user know.
iaHist = -1 
do i=1,NRow
  j = IndexArray(i)
  if (iaHist(j).lt.0) then 
   iaHist(j) = 1 
  else 
   iaHist(j) = iaHist(j) + 1
  end if
end do

Hmin = minval(iaHist)
Hmax = maxval(iaHist)

cntarr = 0
where (iaHist.eq.-1) 
  cntarr = 1
end where
ebin = sum(cntarr)

call Message('KCluster:  IndexArray analysis ')
io_int(1) = NClusters
call WriteValue('  Number of bins       : ',io_int,1,"(I5)")
io_int(1) = Hmin 
io_int(2) = Hmax
call WriteValue('  Min/Max bin counts   : ',io_int,2,"(I5,'/',I5)")
io_int(1) = ebin
call WriteValue('  Number of empty bins : ',io_int,1,"(I5)")


end subroutine KCluster


!--------------------------------------------------------------------------
!
! SUBROUTINE: ReorganizeClusters
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief use Monte Carlo style algorithm to minimize the sum of deviations from
!> the cluster centroids
!
!> @param Matrix Input matrix
!> @param NRow number of rows in matrix
!> @param NCol number of columns
!> @param NClusters number of cluster to look for
!> @param NSC number of scan columns
!> @param NSR number of scan rows
!> @param IndexArray returns the IndexArray
! 
!> @date  01/06/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ReorganizeClusters(Matrix, NRow, NCol, NClusters, NSC, NSR, IndexArray)
!DEC$ ATTRIBUTES DLLEXPORT :: ReorganizeClusters

use local
use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: NRow
integer(kind=irg),INTENT(IN)    :: NCol
real(kind=dbl),INTENT(IN)       :: Matrix(NCol, NRow)
integer(kind=irg),INTENT(IN)    :: NClusters
integer(kind=irg),INTENT(IN)    :: NSC
integer(kind=irg),INTENT(IN)    :: NSR
integer(kind=irg),INTENT(INOUT) :: IndexArray(NRow)
!f2py intent(in,out) ::  IndexArray

integer(kind=irg)               :: IA(NSC,NSR), NinC(NClusters)
integer(kind=irg)               :: i, j, k, ic, nc, ix, px, py, Emax(1), Emin(1), nloop, nchanged, totswap
real(kind=dbl)                  :: TotalEnergy, LastEnergy
real(kind=dbl)                  :: PointEnergy(NRow), ClusterEnergy(NClusters), Col(NCol)
real(kind=dbl),allocatable      :: centroids(:,:), cE(:), cds(:,:)
integer(kind=irg),allocatable   :: cset(:)
logical                         :: go

!-------- set things up for the Monte Carlo part -------
!-------------------------------------------------------
! reshape the index array for easier neighbor finding
!IA = reshape( IndexArray, (/ NSC, NSR /) )
!write (*,*) 'reshaped IndexArray ', shape (IA)

! count the number of points in each cluster and add the x-y coordinates to get the cluster centroid 
allocate(centroids(NCol, NClusters))
NinC = 0
do i=1,NRow
  ix = IndexArray(i)
  NinC(ix) = NinC(ix) + 1
  centroids(1:NCol,ix) = centroids(1:NCol,ix) + Matrix(1:NCol,i)
end do

! and divide by the number of entries in each cluster
do i=1,NClusters
  if (NinC(i).ne.0) centroids(1:NCol,i) = centroids(1:NCol,i) / dble( NinC(i) )
end do

! initialize the PointEnergy array, which contains the contribution of each pattern to the TotalEnergy
! we do not need to take a square root!
do i=1,NRow
  PointEnergy(i) = sum( (Matrix(1:NCol,i) - centroids(1:NCol,IndexArray(i)))**2 )
end do 

TotalEnergy = sum(PointEnergy)
write (*,*) ' Total Energy       = ',dsqrt(TotalEnergy)

! next we try one loop of the following algorithm
! for each cluster, we determine the point with the highest energy and compute 
! its energy with respect to all the other centroids; if any one of them is lower,
! then we swap the point to the other cluster.  We run through the array once for now
nloop = 0
totswap = 0
nchanged = 0
go = .TRUE.
do while (go)
 nloop = nloop+1
 totswap = totswap + nchanged
 nchanged = 0
 LastEnergy = TotalEnergy
 do ic=1,NClusters
! define an array to hold all the members of the cluster
  nc = NinC(ic)
  allocate(cset(nc),cE(nc))
! add all member indices to cset and their corresponding energies to cE
  k = 1
  do j=1,NRow
     if (IndexArray(j).eq.ic) then
       cset(k) = j
       cE(k) = PointEnergy(j)
       k = k+1
     end if
  end do 

! get the point with the largest energy
  Emax = maxloc(cE)
! compute the point energies if this point were to belong to all the other clusters
  do i=1,NClusters
    ClusterEnergy(i) = sum( (Matrix(1:NCol,cset(Emax(1))) - centroids(1:NCol,i))**2 )
  end do
! find the lowest energy point
  Emin = minloc(ClusterEnergy)
! if this point is not the original point, then we add the point to the new cluster
  if (Emin(1).ne.ic) then
    nchanged = nchanged + 1
    NinC(ic) = NinC(ic) - 1
    NinC(Emin(1)) = NinC(Emin(1)) + 1
    PointEnergy(cset(Emax(1))) = ClusterEnergy(Emin(1))
    IndexArray(cset(Emax(1))) = Emin(1)
! we must also update the centroids ... 
    Col = Matrix(1:NCol,cset(Emax(1)))
    centroids(1:NCol,ic) = ( centroids(1:NCol,ic)*dble(NinC(ic)+1) - Col ) / dble(NinC(ic))
    centroids(1:NCol,Emin(1)) = ( centroids(1:NCol,Emin(1))*dble(NinC(Emin(1))-1) + Col ) / dble(NinC(Emin(1)))
  end if 
! and repeat this for all the clusters
  deallocate(cset, cE)

  TotalEnergy = sum(PointEnergy)
 end do
write (*,*) 'loop ', nloop, nchanged, dsqrt(TotalEnergy)
if (LastEnergy.eq.TotalEnergy) go=.FALSE.
end do

write (*,*) ' --> total number of cluster assignments changed : ',totswap

! write an array of centroid-centroid distances to a file for further analysis 
open(unit=dataunit2,file='cds.data',status='unknown',form='unformatted')
allocate(cds(NClusters,NClusters))
do i=1,NClusters
  do j=1,NClusters
    cds(i,j) = dsqrt(sum( (centroids(1:NCol,i)-centroids(1:NCol,j))**2 )) 
  end do
end do
write(dataunit2) NClusters
write(dataunit2) cds
close(unit=dataunit2,status='keep')

end subroutine ReorganizeClusters

!--------------------------------------------------------------------------
!
! FUNCTION: cross3
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  cross product of two 3D vector in the order of input
!
!
!> @param u input vector 1
!> @param v input vector 2
! 
!> @date 03/03/16   SS 1.0 original
!> @date 12/01/16  MDG 1.1 split in single and double precision versions
!--------------------------------------------------------------------------
recursive function cross3(u, v) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: cross3

IMPLICIT NONE

real(kind=sgl),INTENT(IN)      :: u(3)
real(kind=sgl),INTENT(IN)      :: v(3)
real(kind=sgl)                 :: res(3)

res(1) = u(2)*v(3) - u(3)*v(2)
res(2) = u(3)*v(1) - u(1)*v(3)
res(3) = u(1)*v(2) - u(2)*v(1)

end function cross3

!--------------------------------------------------------------------------
!
! FUNCTION: cross3_d
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  cross product of two 3D vector in the order of input (double precision)
!
!
!> @param u input vector 1
!> @param v input vector 2
! 
!> @date 03/03/16   SS 1.0 original
!--------------------------------------------------------------------------
recursive function cross3_d(u, v) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: cross3_d

IMPLICIT NONE

real(kind=dbl),INTENT(IN)      :: u(3)
real(kind=dbl),INTENT(IN)      :: v(3)
real(kind=dbl)                 :: res(3)

res(1) = u(2)*v(3) - u(3)*v(2)
res(2) = u(3)*v(1) - u(1)*v(3)
res(3) = u(1)*v(2) - u(2)*v(1)

end function cross3_d


!--------------------------------------------------------------------------
!
! SUBROUTINE: vectormatch
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns the number of common entries in two vectors of unique positive integers
!
!> @param n number of entries in vectors
!> @param va first vector of integers
!> @param vb second vector of integers
! 
!> @date  07/28/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function vectormatch(n, va, vb) result(nce)
!DEC$ ATTRIBUTES DLLEXPORT :: vectormatch

IMPLICIT NONE

integer(kind=irg),INTENT(IN)     :: n
integer(kind=irg),INTENT(IN)     :: va(n)
integer(kind=irg),INTENT(IN)     :: vb(n)
integer(kind=irg)                :: nce ! number of common entries

integer(kind=irg)                :: i, mi

nce = 0

! we'll use a very simplistic approach: for each entry va_i in va, compute
! abs(vb-va_i); if the minval of this array equals zero, then the entry
! va_i appears in vb.  Note that this only works if there are no 
! duplicate entries in either of the vectors and all the entries are positive...
! this routine is used by the dictionary indexing codes, so that is always true.

do i=1,n
  mi = minval(abs(vb-va(i)))
  if (mi.eq.0) nce = nce+1
end do

end function vectormatch



!--------------------------------------------------------------------------
!
! function: trilinear_splat
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief distributes a function value onto a cubic grid of 3x3x3 nodes (used for 3D histograms)
!
!> @details This function takes a volume array and a point coordinate, and adds the value
!> of 1 to the array, but distributed ("splatted") over a group of 27 grid points using a 
!> relatively sharp Gaussian function.  The function takes the remainder part of the coordinates
!> and computes Gaussian values for each grid point with the peak at the position r mod dr. The
!> 3x3x3 array is then returned to the calling program with the extrapolated values on the grid nodes.
!> Routine verified against a similar IDL script on 6/20/2017.
!
!> @param r the point to be splatted
!> @param dr the step sizes in the current array
!> @param init initalize the coordinate arrays
! 
!> @date  06/20/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function trilinear_splat(r, dr, init) result(grid3)
!DEC$ ATTRIBUTES DLLEXPORT :: trilinear_splat

IMPLICIT NONE

real(kind=sgl),INTENT(IN)   :: r(3)
real(kind=sgl),INTENT(IN)   :: dr(3)
logical,INTENT(IN),OPTIONAL :: init
real(kind=sgl)              :: grid3(3,3,3)

real(kind=sgl),SAVE         :: grx(3,3,3), gry(3,3,3), grz(3,3,3)
real(kind=sgl)              :: alpha = 2.0, rx, ry, rz, drx, dry, drz

if (present(init)) then 
 if (init.eqv..TRUE.) then
  grx = 0.0 
  gry = 0.0 
  grz = 0.0 
  grx(1,:,:) = -1.0
  grx(3,:,:) =  1.0
  gry(:,1,:) = -1.0
  gry(:,3,:) =  1.0
  grz(:,:,1) = -1.0
  grz(:,:,3) =  1.0
  grid3 = 0.0
 else
  rx = r(1)/dr(1)
  ry = r(2)/dr(2)
  rz = r(3)/dr(3)

  drx = rx - nint(rx)
  dry = ry - nint(ry)
  drz = rz - nint(rz)

  grid3 = exp( -alpha * ( (grx-drx)**2 + (gry-dry)**2 + (grz-drz)**2 ) )
  grid3 = grid3/sum(grid3)
 end if
end if

end function trilinear_splat

!--------------------------------------------------------------------------
!
! function: CalcDeterminant
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate determinant of mxn real matrix
!
!> @details a generic subroutine to calculate the determinant of any mxn matrix.
!> we will be using the dgetrf subroutine of BLAS to calculate the decomposition of
!> A as A = P * L * U, where L is the unitriangular matrix i.e. det(L) = 1; P is the
!> permutation matrix so its determinant is either +/- 1. using the property det(A) = 
!> det(P) * det(L) * det(U), we can calculate determinant as simply the product of diagonal
!> elements of U.
!
!> @param A nxn real matrix
!> @param n size of A
! 
!> @date  06/01/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function CalcDeterminant(A, m, n) result(determinant)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcDeterminant

use local
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)             :: A(m,n)
integer(kind=irg),INTENT(IN)          :: m, n

real(kind=dbl),allocatable            :: Ap(:,:)
integer(kind=irg),allocatable         :: IPIV(:)
integer(kind=irg)                     :: LDA, INFO, mm, nn, ii
real(kind=dbl)                        :: determinant

LDA = maxval((/m,1/))
nn = minval((/m,n/))

allocate(IPIV(nn), Ap(LDA,n))
Ap = A

call dgetrf(m, n, Ap, LDA, IPIV, INFO)

if(INFO .ne. 0) call FatalError('CalcDeterminant:','BLAS subroutine did not return successfully.')

determinant = 1.D0
do ii = 1,nn
  determinant = determinant * Ap(ii,ii)
  if(IPIV(ii) .ne. ii) determinant = -determinant
end do


end function CalcDeterminant


end module math
