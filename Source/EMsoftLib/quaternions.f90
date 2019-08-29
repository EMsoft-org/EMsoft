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
! EMsoft:quaternions.f90
!--------------------------------------------------------------------------
!
! MODULE: quaternions
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief module with basic quaternion functions (some overloaded operators)
!                    
!> @details   [verified against Mathematica Quaternion package on 3/15/12]
!>
!> REMEMBER THAT QUATERNION MULTIPLICATION IS NON-COMMUTATIVE !!!!!
!>
!> quaternions are defined as arrays of 4 single or double precision reals;
!> the first entry is the scalar part, the remaining three form the vector part.
!>
!> If you want to try it out, here is an example test program\n
!>\n
!> program qtest\n
!>\n
!>use local\n
!>use quaternions\n
!>\n
!>IMPLICIT NONE\n
!>\n
!>! for single precision, use the following lines\n
!>!real(kind=sgl)  ::  u(4), v(4), w(4)\n
!>!real(kind=sgl) :: x, a=2\n
!>! define two quaternions (single)\n
!>!u = (/1.0,2.0,3.0,4.0/)\n
!>!v = (/5.0,6.0,7.0,8.0/)\n
!>\n
!>! for double precision, uncomment the next set and comment the previous set lines\n
!>real(kind=dbl)  ::  u(4), v(4), w(4)\n
!>real(kind=dbl) :: x, a=2.D0\n
!>! double\n
!>u = (/1.D0,2.D0,3.D0,4.D0/)\n
!>v = (/5.D0,6.D0,7.D0,8.D0/)\n
!>\n
!>\n
!>write (stdout,*) ' quaternion u '\n
!>call quaternion_print(u)\n
!>write (stdout,*) ' quaternion v '\n
!>call quaternion_print(v)\n
!>\n
!>! next, do all the operations to make sure that they are correct\n
!>\n
!>write (stdout,*) '   addition u+v '\n
!>w = u+v\n
!>call quaternion_print(w)\n
!>write (stdout,*) ' '\n
!>\n
!>write (stdout,*) '   subtraction u-v '\n
!>w = u-v\n
!>call quaternion_print(w)\n
!>write (stdout,*) ' '\n
!>\n
!>write (stdout,*) '   scalar multiplication (both orderings)  '\n
!>w = a*u\n
!>call quaternion_print(w)\n
!>w = u*a\n
!>call quaternion_print(w)\n
!>write (stdout,*) ' '\n
!>\n
!>write (stdout,*) '   multiplication uv '\n
!>w = quat_mult(u,v)\n
!>call quaternion_print(w)\n
!>write (stdout,*) ' '\n
!>\n
!write (stdout,*) '   conjugate u '\n
!>w = conjg(u)\n
!>call quaternion_print(w)\n
!>write (stdout,*) ' '\n
!>\n
!>write (stdout,*) '   norm(u) '\n
!>x = cabs(u)\n
!>write (stdout,*) x\n
!>write (stdout,*) ' '\n
!>\n
!>write (stdout,*) '   division u/v '\n
!>w = quat_div(u,v)\n
!>call quaternion_print(w)\n
!>write (stdout,*) ' '\n
!>\n
!>end program qtest\n
!>
!
!> @note Quaternions are defined with the scalar part in position 1, and the 
!> vector part in positions 2:4.
! 
!> @date 03/15/12   MDG 1.0 original
!> @date 08/04/13   MDG 1.1 moved rotation conversion functions to rotations.f90
!> @date 08/12/13   MDG 2.0 re-defined quaternions to be arrays of 4 reals rather than by letter ... 
!> @date 02/06/15   MDG 2.1 added quat_slerp interpolation routines
!> @date 03/11/15   MDG 2.2 renamed quaternion vector rotation routines and split into active and passive
!> @date 03/11/15   MDG 2.3 redefined quaternion product using epsijk constant (see rotations tutorial paper)
!> @date 03/14/15   MDG 2.4 sign change in quaternion product; removed quat_Lpstar routines
!--------------------------------------------------------------------------
module quaternions

use local

IMPLICIT NONE

! only this routine is public; all the others are done via overloaded operators
public :: quaternion_print
interface quaternion_print
        module procedure quaternion_print
        module procedure quaternion_print_d
 end interface

! quaternion multiplication (single and double precision)
public :: quat_mult
interface quat_mult
     module procedure quat_mult
     module procedure quat_mult_d
  end interface

! complex conjugation (single and double precision)
intrinsic :: conjg
public :: conjg
interface conjg
     module procedure quat_conjg
     module procedure quat_conjg_d
  end interface

! quaternion norm (single and double precision)
intrinsic :: cabs
public :: cabs
interface cabs
     module procedure quat_norm
     module procedure quat_norm_d
  end interface

! quaternion division (single and double precision)
public :: quat_div
interface quat_div
     module procedure quat_div
     module procedure quat_div_d
  end interface

! quaternion inner product (single and double precision)
public :: quat_innerproduct
interface quat_innerproduct
     module procedure quat_innerproduct
     module procedure quat_innerproduct_d
  end interface

! interquaternion angle (single and double precision)
public :: quat_angle
interface quat_angle
     module procedure quat_angle
     module procedure quat_angle_d
  end interface

! active quaternion rotation of a unit vector  q v q*
public :: quat_Lp
interface quat_Lp
        module procedure quat_Lp
        module procedure quat_Lp_d
end interface

! quaternion SLERP interpolation between two quaternions
public :: quat_slerp
interface quat_slerp
        module procedure quat_slerp
        module procedure quat_slerp_d
end interface

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: quaternion_print
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print a quaternion (for debugging purposes mostly)
!
!> @param q quaternion to be printed (single precision)  
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive subroutine quaternion_print(q)
!DEC$ ATTRIBUTES DLLEXPORT :: quaternion_print

use io

    real(kind=sgl), intent(in)  :: q(4)         !< input quaternion (single precision)

    call WriteValue('', q, 4, "('(',4f12.6,')')")

end subroutine quaternion_print

!--------------------------------------------------------------------------
!
! SUBROUTINE:quaternion_print_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print a quaternion (for debugging purposes mostly)
!
!> @param q quaternion to be printed (double precision)  
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive subroutine quaternion_print_d(q)
!DEC$ ATTRIBUTES DLLEXPORT :: quaternion_print_d

use io

    real(kind=dbl), intent(in)  :: q(4)         !< input quaternion (double precision)

    call WriteValue('', q, 4, "('(',4f15.9,')')")

end subroutine quaternion_print_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_mult
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion multiplication   (single precision)
!
!> @param x first quaternion 
!> @param y second quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!> @date 3/11/15   MDG 3.0 redefined quaternion product (see rotations tutorial paper)
!--------------------------------------------------------------------------
recursive function quat_mult(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_mult

use constants

    real(kind=sgl), intent(in)          :: x(4), y(4)           !< input quaternions
    real(kind=sgl)                      :: res(4)


! the following is a way to reduce the number of multiplications
! needs to be tested and merged with epsijk approach
!
! QuatMul(QUAT *q1, QUAT *q2, QUAT *res){
! float A, B, C, D, E, F, G, H;
! A = (q1->w + q1->x)*(q2->w + q2->x);
! B = (q1->z - q1->y)*(q2->y - q2->z);
! C = (q1->w - q1->x)*(q2->y + q2->z); 
! D = (q1->y + q1->z)*(q2->w - q2->x);
! E = (q1->x + q1->z)*(q2->x + q2->y);
! F = (q1->x - q1->z)*(q2->x - q2->y);
! G = (q1->w + q1->y)*(q2->w - q2->z);
! H = (q1->w - q1->y)*(q2->w + q2->z);
! res->w = B + (-E - F + G + H) /2;
! res->x = A - (E + F + G + H)/2; 
! res->y = C + (E - F + G - H)/2; 
! res->z = D + (E - F - G + H)/2;
! }

    res = (/ x(1)*y(1) - x(2)*y(2) - x(3)*y(3) - x(4)*y(4), &
             x(1)*y(2) + x(2)*y(1) + epsijk * ( x(3)*y(4) - x(4)*y(3) ), &
             x(1)*y(3) + x(3)*y(1) + epsijk * ( x(4)*y(2) - x(2)*y(4) ), &
             x(1)*y(4) + x(4)*y(1) + epsijk * ( x(2)*y(3) - x(3)*y(2) ) /)
    
end function quat_mult

!--------------------------------------------------------------------------
!
! FUNCTION: quat_mult_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion multiplication   (double precision)
!
!> @param x first quaternion 
!> @param y second quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_mult_d(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_mult_d

use constants

    real(kind=dbl), intent(in)          :: x(4), y(4)           !< input quaternions
    real(kind=dbl)                      :: res(4)

    res = (/ x(1)*y(1) - x(2)*y(2) - x(3)*y(3) - x(4)*y(4), &
             x(1)*y(2) + x(2)*y(1) + epsijkd * ( x(3)*y(4) - x(4)*y(3) ), &
             x(1)*y(3) + x(3)*y(1) + epsijkd * ( x(4)*y(2) - x(2)*y(4) ), &
             x(1)*y(4) + x(4)*y(1) + epsijkd * ( x(2)*y(3) - x(3)*y(2) ) /)

end function quat_mult_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_conjg
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion complex conjugation (extends intrinsic routine conjg)
!
!> @param x quaternion to be conjugated (single precision)
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_conjg(x) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_conjg

    real(kind=sgl), intent(in)          :: x(4)         !< input quaternion
    real(kind=sgl)                      :: res(4)

    res = (/ x(1), -x(2), -x(3), -x(4) /)

end function quat_conjg

!--------------------------------------------------------------------------
!
! FUNCTION: quat_conjg_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion complex conjugation (extends intrinsic routine conjg)
!
!> @param x quaternion to be conjugated (double precision)
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_conjg_d(x) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_conjg_d

    real(kind=dbl), intent(in)          :: x(4)         !< input quaternion
    real(kind=dbl)                      :: res(4)

    res = (/ x(1), -x(2), -x(3), -x(4) /)

end function quat_conjg_d


!--------------------------------------------------------------------------
!
! FUNCTION: quat_norm
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion norm (extends intrinsic routine cabs)
!
!> @param x quaternion to be normed (single precision)
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_norm(x) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_norm

   real(kind=sgl), intent(in)           :: x(4)         !< input quaternion
   real(kind=sgl)                       :: res

    res =  sqrt( sum(x*x) )

end function quat_norm

!--------------------------------------------------------------------------
!
! FUNCTION: quat_norm_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion norm (extends intrinsic routine cabs)
!
!> @param x quaternion to be normed (double precision)
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_norm_d(x) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_norm_d

    real(kind=dbl), intent(in)  :: x(4)         !< input quaternion
    real(kind=dbl)              :: res

    res =  dsqrt( sum(x*x) )

end function quat_norm_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_div
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion division (single precision)
!
!> @param x nominator quaternion 
!> @param y denominator quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_div(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_div

    real(kind=sgl), intent(in)          :: x(4),y(4)            !< input quaternions
    real(kind=sgl)                      :: res(4), p(4), q

    q = quat_norm(y)
    p = quat_conjg(y)/(q*q)
    res =  quat_mult(x,p)

end function quat_div

!--------------------------------------------------------------------------
!
! FUNCTION: quat_div_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief quaternion division (double precision)
!
!> @param x nominator quaternion 
!> @param y denominator quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_div_d(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_div_d

    real(kind=dbl), intent(in)          :: x(4),y(4)            !< input quaternions
    real(kind=dbl)                      :: res(4), p(4), q

    q = quat_norm_d(y)
    p = quat_conjg_d(y)/(q*q)
    res =  quat_mult_d(x,p)

end function quat_div_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_innerproduct
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  quaternion inner product  (single precision)
!
!> @param x nominator quaternion 
!> @param y denominator quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_innerproduct(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_innerproduct

    real(kind=sgl), intent(in)          :: x(4),y(4)            !< input quaternions
    real(kind=sgl)                      :: res

    res = sum(x * y)

end function quat_innerproduct

!--------------------------------------------------------------------------
!
! FUNCTION: quat_innerproduct_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  quaternion inner product  (double precision)
!
!> @param x nominator quaternion 
!> @param y denominator quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------
recursive function quat_innerproduct_d(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_innerproduct_d

    real(kind=dbl), intent(in)          :: x(4),y(4)            !< input quaternions
    real(kind=dbl)                      :: res

    res = sum(x * y) 

end function quat_innerproduct_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_angle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   interquaternion angle   (single precision)
!
!> @param x first quaternion 
!> @param y second quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------!
recursive function quat_angle(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_angle

    real(kind=sgl), intent(in)                  :: x(4),y(4)            !< input quaternions
    real(kind=sgl)                              :: res, q

    q = quat_innerproduct(x,y)
    res = acos( 2.0_sgl*q*q - 1.0_sgl )

end function quat_angle

!--------------------------------------------------------------------------
!
! FUNCTION: quat_angle_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   interquaternion angle   (double precision)
!
!> @param x first quaternion 
!> @param y second quaternion 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!--------------------------------------------------------------------------!
recursive function quat_angle_d(x,y) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_angle_d

    real(kind=dbl), intent(in)          :: x(4),y(4)            !< input quaternions
    real(kind=dbl)                      :: res, q

    q = quat_innerproduct_d(x,y)
    res = dacos( 2.0_dbl*q*q - 1.0_dbl )

end function quat_angle_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_Lp
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   actively rotate a unit vector by a unit quaternion, L_p = p v p*
!
!> @param q quaternion 
!> @param v vector to be rotated 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!> @date 3/11/15   MDG 3.0 name change, to be compatible with rotations tutorial paper
!--------------------------------------------------------------------------!
recursive function quat_Lp(q,v) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_Lp

    real(kind=sgl),intent(in)                   :: q(4)         !< input quaternion
    real(kind=sgl),intent(in)                   :: v(3)         !< input vector
    real(kind=sgl)                              :: qv(4), rqv(4)
    real(kind=sgl)                              :: res(3)

    qv = (/ 0.0, v(1), v(2), v(3) /)   
    rqv = quat_mult(q,quat_mult(qv,quat_conjg(q) ) )
    res = rqv(2:4)

end function quat_Lp

!--------------------------------------------------------------------------
!
! FUNCTION: quat_Lp_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   actively rotate a unit vector by a unit quaternion, L_p = p v p*
!
!> @param q quaternion 
!> @param v vector to be rotated 
! 
!> @date 3/15/12   MDG 1.0 original
!> @date 8/12/13   MDG 2.0 rewrite
!> @date 3/11/15   MDG 3.0 name change, to be compatible with rotations tutorial paper
!--------------------------------------------------------------------------!
recursive function quat_Lp_d(q,v) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_Lp_d

    real(kind=dbl),intent(in)                   :: q(4)         !< input quaternion
    real(kind=dbl),intent(in)                   :: v(3)         !< input vector
    real(kind=dbl)                              :: qv(4), rqv(4)
    real(kind=dbl)                              :: res(3)

    qv = (/ 0.D0, v(1), v(2), v(3) /)   
    rqv = quat_mult_d(q,quat_mult_d(qv,quat_conjg_d(q) ) )
    res = rqv(2:4)

end function quat_Lp_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_slerp
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   return an array of interpolated quaternions
!
!> @param qa starting quaternion 
!> @param qb ending quaternion 
!> @param n number of interpolation points
! 
!> @date 02/06/15   MDG 1.0 original
!--------------------------------------------------------------------------!
recursive function quat_slerp(qa,qb,n) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_slerp


    real(kind=sgl),intent(in)                   :: qa(4)         !< input quaternion
    real(kind=sgl),intent(in)                   :: qb(4)         !< input quaternion
    integer(kind=irg),intent(in)                :: n            

    real(kind=sgl)                              :: qv(4), theta, phi, dphi, s
    real(kind=sgl)                              :: res(4,n)
    integer(kind=irg)                           :: i

    res = 0.0
    theta = acos( dot_product(qb, quat_conjg(qa) ))
    if (theta.ne.0.0) then
      s = 1.0/sin(theta)
      dphi = theta/real(n-1)

      do i=1,n
        phi = real(i-1)*dphi
        res(1:4,i) = s * ( qa(1:4) * sin(theta-phi) + qb(1:4) * sin(phi) )
      end do
    else
      do i=1,n
        res(1:4,i) = qa(1:4)
      end do
    end if

end function quat_slerp

!--------------------------------------------------------------------------
!
! FUNCTION: quat_slerp_d
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   return an array of interpolated quaternions (double precision)
!
!> @param qa starting quaternion 
!> @param qb ending quaternion 
!> @param n number of interpolation points
! 
!> @date 02/06/15   MDG 1.0 original
!--------------------------------------------------------------------------!
recursive function quat_slerp_d(qa,qb,n) result (res)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_slerp_d


    real(kind=dbl),intent(in)                   :: qa(4)         !< input quaternion
    real(kind=dbl),intent(in)                   :: qb(4)         !< input quaternion
    integer(kind=irg),intent(in)                :: n            

    real(kind=dbl)                              :: qv(4), theta, phi, dphi, s
    real(kind=dbl)                              :: res(4,n)
    integer(kind=irg)                           :: i

    res = 0.D0
    theta = dacos( dot_product(qb, quat_conjg_d(qa) ) )
    if (theta.ne.0.D0) then
      s = 1.D0/dsin(theta)
      dphi = theta/dble(n-1)

      do i=1,n
        phi = dble(i-1)*dphi
        res(1:4,i) = s * ( qa(1:4) * dsin(theta-phi) + qb(1:4) * dsin(phi) )
      end do
    else
      do i=1,n
        res(1:4,i) = qa(1:4)
      end do
    end if

end function quat_slerp_d

!--------------------------------------------------------------------------
!
! FUNCTION: quat_Marsaglia
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   return a single random quaternion using the Marsaglia approach
!
!> @param seed seed number 
! 
!> @date 04/23/18   MDG 1.0 original
!--------------------------------------------------------------------------!
recursive function quat_Marsaglia(seed) result (q)
!DEC$ ATTRIBUTES DLLEXPORT :: quat_Marsaglia

use rng 

IMPLICIT NONE

type(rng_t),INTENT(INOUT)           :: seed 
!f2py intent(in,out) ::  seed 
real(kind=dbl)                      :: q(4) 

real(kind=dbl)                      :: x1,x2,y1,y2,s1,s2

x1 = 2.D0*rng_uniform(seed)-1.D0
y1 = 2.D0*rng_uniform(seed)-1.D0
s1 = x1*x1+y1*y1
if (s1.gt.1.D0) then 
  do while (s1.gt.1.D0) 
    x1 = 2.D0*rng_uniform(seed)-1.D0
    x2 = 2.D0*rng_uniform(seed)-1.D0
    s1 = x1*x1+y1*y1
  end do
end if 

x2 = 2.D0*rng_uniform(seed)-1.D0
y2 = 2.D0*rng_uniform(seed)-1.D0
s2 = x2*x2+y2*y2
if (s2.gt.1.D0) then 
  do while (s2.gt.1.D0) 
    x2 = 2.D0*rng_uniform(seed)-1.D0
    y2 = 2.D0*rng_uniform(seed)-1.D0
    s2 = x2*x2+y2*y2
  end do
end if 

s1 = sqrt( (1.D0-s2)/s2 )

q = (/ x1, y1, x2*s1, y2*s1 /)

end function quat_Marsaglia


end module quaternions
