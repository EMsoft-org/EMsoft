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
! EMsoft:Wigner.f90
!--------------------------------------------------------------------------
!
! MODULE: Wigner
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Wigner d and D coefficient routines 
!
!> @details 
!>  reference: Gutman, B., Wang, Y., Chan, T., Thompson, P. M., & Toga, A. W. (2008, October). Shape registration with 
!>     spherical cross correlation. In 2nd MICCAI workshop on mathematical foundations of computational anatomy (pp. 56-67).
!>  note: the reference is for the generic case (complex functions) but restricting to real valued functions allow for 
!>    savings from symmetry:
!>        \hat{f}(l, -m) = \hat{f}(l, m) * (-1)^m
!>  additionally since the decomposition of the Wigner D function to 2 Wigner d functions @ pi/2 introduces more symmetry:
!>        d^j_{-k,-m} = (-1)^(   k- m) d^j_{k,m}
!>        d^j_{ k,-m} = (-1)^(j+ k+2m) d^j_{k,m}
!>        d^j_{-k, m} = (-1)^(j+2k+3m) d^j_{k,m}
!>        d^j_{ m, k} = (-1)^(   k- m) d^j_{k,m}
!>  finally since the cross correlation of the real functions is also real there is another factor of 2 savings
!
!> @note: variable names are consistent with Gutman et. al. (see eq 12 for details) except 'j' is used in place of 'l'
! 
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's C++ routines
!--------------------------------------------------------------------------
module Wigner

use local

IMPLICIT NONE

! Quiet NAN, double precision.
REAL(8), PARAMETER :: D_QNAN = TRANSFER((/ Z'00000000', Z'7FF80000' /),1.0_8)
private :: D_QNAN

contains


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! a couple of helper functions
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_zyz2qu
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   : convert ZYZ Euler angles to quaternion
!
!> @param eu: euler angles to convert to quaternion (Z, Y', Z'')
!> @param qu: location to write quaternion as w, x, y, z
!
!> @note    : materials science euler angles are generally ZXZ
!> @note    : equivalent to eu[0] -= pi/2 and eu[2] += pi/2 followed by eu2qu for ZXZ
!
!> @date 02/12/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_zyz2qu(eu) result(qu)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_zyz2qu

use constants 

IMPLICIT NONE

real(kind=dbl),INTENT(IN)           :: eu(3)
real(kind=dbl)                      :: qu(4)

real(kind=dbl)                      :: c, s, sigma, delta

c = cos(eu(2)*0.5D0)
s = sin(eu(2)*0.5D0)
sigma = (eu(3)+eu(1))*0.5D0
delta = (eu(3)-eu(1))*0.5D0

qu = (/ c*cos(sigma), s*sin(delta)*epsijkd, s*cos(delta)*epsijkd, c*sin(sigma)*epsijkd /)
if (qu(1).lt.0.D0) qu = -qu

end function Wigner_zyz2qu

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_qu2zyz
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   : convert ZYZ Euler angles to quaternion
!
!> @param qu: quaternion to convert to euler angles as w, x, y, z
!> @param eu: location to write euler angles (Z, Y', Z'')
!
!> @note    : materials science euler angles are generally ZXZ
!> @note    : equivalent to eu[0] -= pi/2 and eu[2] += pi/2 followed by eu2qu for ZXZ
!
!> @date 02/12/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_qu2zyz(qu) result(eu)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_qu2zyz

use constants 

IMPLICIT NONE

real(kind=dbl),INTENT(IN)           :: qu(4)
real(kind=dbl)                      :: eu(3)

real(kind=dbl)                      :: qu0, q03, q12, chi, thr, x1, x2, y1, y2 

thr = epsilon(1.D0) * 10.D0
qu0 = qu(1) * epsijkd
q03 = qu(1)*qu(1) + qu(4)*qu(4)
q12 = qu(2)*qu(2) + qu(3)*qu(3)
chi = sqrt(q03*q12)

if (chi.le.thr) then 
    if (q12.le.thr) then 
        eu(1) = atan2(-2.D0 * qu0 * qu(4), qu(1)*qu(1)-qu(4)*qu(4))
        eu(2) = 0.D0
    else
        eu(1) = atan2(-2.D0 * qu(2) * qu(3), qu(3)*qu(3)-qu(2)*qu(2))
        eu(2) = cPi
    end if
    eu(3) = 0.D0
else
    y1 = qu(3)*qu(4)
    y2 = -qu(2) * qu0
    x1 = -qu(3) * qu0
    x2 = -qu(2) * qu(4)
    eu = (/ atan2(y1-y2,x1-x2), atan2(2.D0*chi, q03-q12), atan2(y1+y2, x1+x2) /)
end if

where(eu.lt.0) eu = eu + 2.D0*cPi 

end function Wigner_qu2zyz


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! simplified helper functions to compute Wigner (lowercase) d functions d^j_{k,m}(beta) for 0 <= beta <= pi and integer j,k,m
! all functions use the notation of: Fukushima, Toshio. (2016). Numerical computation of Wigner's d-function of arbitrary 
! high degree and orders by extending exponent of floating point numbers.
! URL: https://doi.org/10.13140/RG.2.2.31922.20160
! blocks functions share the same basic structure but the factor of 2 has been removed since only whole (not half) integers 
! are needed.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_u_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief     : compute intermediate recursion coefficient u_{j,k,m} (equation 13)
!> @param j   : degree in d^j_{k,m}
!> @param k   : first order in d^j_{k,m}
!> @param m   : second order in d^j_{k,m}
!> @param t/tc: cos(beta) / 1 - cos(beta)
!> @return    : recursion coefficient
!> @note      : _0, _1, _2 for 0 <= beta < pi / 2, beta == pi / 2, and pi / 2 < beta <= pi / 2 respectively
!
!> @date 02/13/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_u_jkm0(j, k, m, tc) result(ujkm0)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_u_jkm0

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: tc
real(kind=dbl)                      :: ujkm0 

ujkm0 = -tc * ((j - 1) * j) - (k * m - (j - 1) * j)

end function Wigner_u_jkm0

recursive function Wigner_u_jkm1(k, m) result(ujkm1)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_u_jkm1

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
integer(kind=ill)                   :: ujkm1 

ujkm1 =  -  k * m

end function Wigner_u_jkm1

recursive function Wigner_u_jkm2(j, k, m, t) result(ujkm2)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_u_jkm2

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: t
real(kind=dbl)                      :: ujkm2 

ujkm2 = t  * ((j - 1) * j) -  k * m

end function Wigner_u_jkm2

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_v_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief     : compute intermediate recursion coefficient v_{j,k,m} (equation 14)
!> @param j   : degree in d^j_{k,m}
!> @param k   : first order in d^j_{k,m}
!> @param m   : second order in d^j_{k,m}
!> @return    : recursion coefficient
!
!> @date 02/13/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_v_jkm(j, k, m) result(vjkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_v_jkm

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: vjkm 

vjkm = sqrt( dble( (j+k-1) * (j-k-1) * (j+m-1) * (j-m-1) ) ) * dble(j)

end function Wigner_v_jkm


!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_w_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute intermediate recursion coefficients
!
!> @param j: current j in recursive calculation of d^j_{k,m}
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: recursion coefficient
!> @note: this function is most susceptible to integer overflow (particularly at k = m = 0) with a 
!>        max k of only 215 for 32 bit ints (2^31-1)^(1/4), 64 bit integers buys up to ~55k which should be plenty
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_w_jkm(j, k, m) result(wjkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_w_jkm

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: wjkm 

wjkm = 1.D0 / ( dble(j-1) * sqrt( dble( (j+k) * (j-k) * (j+m) * (j-m) ) ) )

end function Wigner_w_jkm

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_a_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief     : compute intermediate recursion coefficient a_{j,k,m} (equation 11)
!> @param j   : degree in d^j_{k,m}
!> @param k   : first order in d^j_{k,m}
!> @param m   : second order in d^j_{k,m}
!> @param t/tc: cos(beta) / 1 - cos(beta)
!> @return    : recursion coefficient
!> @note      : _0, _1, _2 for 0 <= beta < pi / 2, beta == pi / 2, and pi / 2 < beta <= pi / 2 respectively
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_a_jkm0(j, k, m, tc) result(ajkm0)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_jkm0

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: tc
real(kind=dbl)                      :: ajkm0 

ajkm0 = Wigner_w_jkm(j, k, m) * ( Wigner_u_jkm0(j, k, m, tc) * dble(2*j-1) )

end function Wigner_a_jkm0

recursive function Wigner_a_jkm1(j, k, m) result(ajkm1)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_jkm1

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: ajkm1 

ajkm1 = Wigner_w_jkm(j, k, m) * ( Wigner_u_jkm1(k, m) * dble(2*j-1) )

end function Wigner_a_jkm1

recursive function Wigner_a_jkm2(j, k, m, t) result(ajkm2)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_jkm2

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: t
real(kind=dbl)                      :: ajkm2 

ajkm2 = Wigner_w_jkm(j, k, m) * ( Wigner_u_jkm2(j, k, m, t) * dble(2*j-1) )

end function Wigner_a_jkm2

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_b_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute intermediate recursion coefficient  (equation 12)
!
!> @param j: current j in recursive calculation of d^j_{k,m}
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: recursion coefficient
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_b_jkm(j, k, m) result(bjkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_b_jkm

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: bjkm 

bjkm = Wigner_w_jkm(j,k,m) * Wigner_v_jkm(j,k,m)

end function Wigner_b_jkm

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_u_km
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief     : compute intermediate recursion coefficient  (equation 23)
!> @param k   : first order in d^j_{k,m}
!> @param m   : second order in d^j_{k,m}
!> @param t/tc: cos(beta) / 1 - cos(beta)
!> @return    : recursion coefficient
!> @note      : _0, _1, _2 for 0 <= beta < pi / 2, beta == pi / 2, and pi / 2 < beta <= pi / 2 respectively
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_u_km0(k, m, tc) result(ukm0)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_u_km0

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: tc
real(kind=dbl)                      :: ukm0 

ukm0 =  ( -tc * dble(k + 1) - dble(m - 1 - k) )

end function Wigner_u_km0

recursive function Wigner_u_km1(k, m) result(ukm1)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_u_km1

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: ukm1 

ukm1 =  -dble(m)  

end function Wigner_u_km1

recursive function Wigner_u_km2(k, m, t) result(ukm2)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_u_km2

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: t
real(kind=dbl)                      :: ukm2 

ukm2 = ( t  * dble(k + 1) - dble(m) )

end function Wigner_u_km2

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_a_km
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief     : compute intermediate recursion coefficient (equation 22)
!> @param k   : first order in d^j_{k,m}
!> @param m   : second order in d^j_{k,m}
!> @param t/tc: cos(beta) / 1 - cos(beta)
!> @return    : recursion coefficient
!> @note      : _0, _1, _2 for 0 <= beta < pi / 2, beta == pi / 2, and pi / 2 < beta <= pi / 2 respectively
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_a_km0(k, m, tc) result(akm0)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_km0

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: tc
real(kind=dbl)                      :: akm0 

akm0 = sqrt( dble( 2*k+1 ) / dble( (k+m+1) * (k-m+1) ) ) * Wigner_u_km0(k, m, tc)

end function Wigner_a_km0

recursive function Wigner_a_km1(k, m) result(akm1)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_km1

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: akm1 

akm1 = sqrt( dble( 2*k+1 ) / dble( (k+m+1) * (k-m+1) ) ) * Wigner_u_km1(k, m)

end function Wigner_a_km1

recursive function Wigner_a_km2(k, m, t) result(akm2)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_km2

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: t
real(kind=dbl)                      :: akm2 

akm2 = sqrt( dble( 2*k+1 ) / dble( (k+m+1) * (k-m+1) ) ) * Wigner_u_km2(k, m, t)

end function Wigner_a_km2

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_e_km
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief: compute recursion seed coefficient e_{k,m} = \sqrt{\frac{(2k)!}{(k+m)!(k-m)!}} recursively (m <= k) (equation 21)
!
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: ekm where d^k_{k,m} = 2^-k * e_km
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_e_km(k, m) result(ekm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_e_km

IMPLICIT NONE

integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl)                      :: ekm 

integer(kind=ill)                   :: l 

ekm = 1.D0
do l=m+1,k 
    ekm = 2.D0 * ekm * sqrt( dble(l*(2*l-1)) / dble( 2 * (l+m) * (l-m) ) )
end do

end function Wigner_e_km

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! The main routines
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_d3    (3 input parameters)
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief compute Wigner (lowercase) d function at pi/2
!> @param j: degree
!> @param k: first order
!> @param m: second order
!> @return: d^j_{k,m}(\frac{\pi}{2})
!
!> @note: NAN when j < max(|k|, |m|)
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_d3(j, k, m) result(djkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_d3

use, intrinsic :: IEEE_ARITHMETIC

IMPLICIT NONE

integer(kind=ill),INTENT(IN)            :: j
integer(kind=ill),INTENT(IN)            :: k
integer(kind=ill),INTENT(IN)            :: m
real(kind=dbl)                          :: djkm

real(kind=dbl)                          :: d_kkm, d_k1km, d_ikm, d_i1km, d_i2km 
integer(kind=ill)                       :: i

! require 0 <= m <= k <= j (handle with symmetry where possible)
! we'll reorganize the indices by means of recursive calls to itself...

if ((k.lt.0).and.(m.lt.0)) then ! d^j_{-k,-m} = (-1)^(   k- m) d^j_{k,m}
    if (mod(k-m,2).eq.0) then 
        djkm =  Wigner_d3(j,-k,-m)
        RETURN
    else
        djkm = -Wigner_d3(j,-k,-m)
        RETURN
    end if 
else 
    if (m.lt.0) then  ! d^j_{ k,-m} = (-1)^(j+ k+2m) d^j_{k,m}
        if (mod(j+k+2*m,2).eq.0) then
            djkm =  Wigner_d3(j,k,-m)
            RETURN
        else
            djkm = -Wigner_d3(j,k,-m)
            RETURN     
        end if 
    else
        if (k.lt.0) then  ! d^j_{-k, m} = (-1)^(j+2k+3m) d^j_{k,m}
            if (mod(j+2*k+3*m,2).eq.0) then
                djkm =  Wigner_d3(j,-k,m)
                RETURN
            else
                djkm = -Wigner_d3(j,-k,m)
                RETURN
            end if
        else
            if (k.lt.m) then  ! d^j_{ m, k} = (-1)^(   k- m) d^j_{k,m}
                if (mod(k-m,2).eq.2) then 
                    djkm =  Wigner_d3(j,m,k)
                    RETURN
                else
                    djkm = -Wigner_d3(j,m,k)
                    RETURN
                end if
            end if 
        end if 
    end if 
end if 

! We need to mimic NaN in f95...
! in C++:  if (j < k) return NAN;
if (j.lt.k) then 
    djkm = D_QNAN
    RETURN
end if

! here is the actual recursive computation ... 

! compute first two terms for three term recursion 
d_kkm  = Wigner_e_km(k, m)/ 2.D0**k
if (j.eq.k) then
    djkm = d_kkm
    RETURN
end if
d_k1km = d_kkm * Wigner_a_km1(k, m)
if (j.eq.k+1) then
    djkm = d_k1km
    RETURN
end if

! recursively compute
d_i2km = d_kkm 
d_i1km = d_k1km
do i=k+2,j
    d_ikm = Wigner_a_jkm1(i, k, m) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km
    d_i2km = d_i1km
    d_i1km = d_ikm
end do

djkm = d_ikm

end function Wigner_d3

!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_d5   (5 input parameters)
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   : compute Wigner (lowercase) d function (also called reduced wigner d)
!> @param j : degree in d^j_{k,m}(beta)
!> @param k : first order in d^j_{k,m}(beta)
!> @param m : second order in d^j_{k,m}(beta)
!> @param t : cos(beta)
!> @param nB: true/false for negative/positive beta
!> @return  : d^j_{k,m}(beta)
!> @note    : NAN when j < max(|k|, |m|)
!> @note    : equivalent to D^j_{k,m}({0, beta, 0})
!> @note    : equivalent to the mathematica function WignerD[{j, k, m}, beta]
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_d5(j, k, m, t, nB) result(djkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_d5

use, intrinsic :: IEEE_ARITHMETIC

IMPLICIT NONE

integer(kind=ill),INTENT(IN)            :: j
integer(kind=ill),INTENT(IN)            :: k
integer(kind=ill),INTENT(IN)            :: m
real(kind=dbl),INTENT(IN)               :: t 
logical,INTENT(IN)                      :: nB
real(kind=dbl)                          :: djkm

real(kind=dbl)                          :: d_kkm, a_km, d_k1km, d_ikm, d_i1km, d_i2km, tc, c2, s2, cn, sn 
integer(kind=ill)                       :: i
integer(kind=irg)                       :: tp

! require 0 <= m <= k <= j (handle with symmetry where possible)
! we'll reorganize the indices by means of recursive calls to itself...

if (nB.eqv..TRUE.) then             ! d^j_{ k, m}(-beta) =                d^j_{m,k}(     beta)
    djkm = Wigner_d5(j, m, k, t, .FALSE.)
    RETURN                          ! equation 5
else
    if ((k.lt.0).and.(m.lt.0)) then ! d^j_{-k,-m}( beta) = (-1)^(   k- m) d^j_{k,m}(     beta)
        if (mod(k-m,2).eq.0) then 
            djkm =  Wigner_d5(j,-k,-m, t, .FALSE.)
            RETURN                  ! equation 6
        else
            djkm = -Wigner_d5(j,-k,-m, t, .FALSE.)
            RETURN                  ! equation 6
        end if 
    else 
        if (m.lt.0) then            ! d^j_{ k,-m}( beta) = (-1)^(j+ k+2m) d^j_{k,m}(pi - beta)
            if (mod(j+k,2).eq.0) then
                djkm =  Wigner_d5(j, k,-m,-t, .FALSE.)
                RETURN              ! equation 7
            else
                djkm = -Wigner_d5(j, k,-m,-t, .FALSE.)
                RETURN              ! equation 7
            end if 
        else
            if (k.lt.0) then        ! d^j_{-k, m}( beta) = (-1)^(j+2k+3m) d^j_{k,m}(pi - beta)
                if (mod(j+m,2).eq.0) then
                    djkm =  Wigner_d5(j,-k, m,-t, .FALSE.)
                    RETURN          ! equation 8
                else
                    djkm = -Wigner_d5(j,-k, m,-t, .FALSE.)
                    RETURN          ! equation 8
                end if
            else
                if (k.lt.m) then    ! d^j_{ m, k}( beta) = (-1)^(   k- m) d^j_{k,m}(     beta)
                    if (mod(k-m,2).eq.2) then 
                        djkm =  Wigner_d5(j, m, k, t, .FALSE.)
                        RETURN      ! equation 9
                    else
                        djkm = -Wigner_d5(j, m, k, t, .FALSE.)
                        RETURN      ! equation 9
                    end if
                end if 
            end if 
        end if 
    end if 
end if

! We need to mimic NaN in f95...
! in C++:  if (j < k) return NAN;
if (j.lt.k) then 
    djkm = D_QNAN
    RETURN
end if

! here is the actual recursive computation ... 

! determine if beta is < (0), > (2), or = (1) to pi/2
tp = 0
if (t.lt.0.D0) tp = 2
if (t.eq.0.D0) tp = 1
tc = 1.D0 - t

! compute powers of cos/sin of beta / 2
c2 = sqrt( (1.D0 + t) * 0.5D0 )     ! cos(acos(t)) == cos(beta / 2), always positive since at this point 0 <= beta <= pi
s2 = sqrt( (1.D0 - t) * 0.5D0 )     ! sin(acos(t)) == sin(beta / 2), always positive since at this point 0 <= beta <= pi
cn = c2**(k+m)                      ! equation 20 for n = k+m
sn = s2**(k-m)                      ! equation 20 for n = k-m

! compute first term for three term recursion 
d_kkm  = cn * sn * Wigner_e_km(k, m)! equation 18, d^k_{k, m}(beta)
if (j.eq.k) then 
    djkm = d_kkm   ! if j == k we're done
    RETURN 
end if

! compute second term for three term recursion 
select case (tp) 
    case (0)
        a_km = Wigner_a_km0(k, m, tc) ! beta <  pi/2
    case (1) 
        a_km = Wigner_a_km1(k, m    ) ! beta == pi/2
    case (2) 
        a_km = Wigner_a_km2(k, m, t ) ! beta >  pi/2
end select 

d_k1km = d_kkm * a_km               ! equation 19, d^{k+1}_{k, m}(beta)
if (j.eq.k+1) then 
    djkm =  d_k1km  ! if j == k + 1 we're done
    RETURN 
end if 

! recursively compute by degree to j
d_i2km = d_kkm
d_i1km = d_k1km
select case (tp) 
    case (0) ! beta <  pi/2
        do i = k + 2, j
            d_ikm = Wigner_a_jkm0(i, k, m, tc) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km ! equation 10, d^i_{k, m}(beta)
            d_i2km = d_i1km
            d_i1km = d_ikm
        end do

    case (1) ! beta == pi/2
        do i = k + 2, j
            d_ikm = Wigner_a_jkm1(i, k, m) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km ! equation 10, d^i_{k, m}(beta)
            d_i2km = d_i1km
            d_i1km = d_ikm
        end do

    case (2) ! beta >  pi/2
        do i = k + 2, j
            d_ikm = Wigner_a_jkm2(i, k, m, t) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km ! equation 10, d^i_{k, m}(beta)
            d_i2km = d_i1km
            d_i1km = d_ikm
        end do
end select

djkm = d_ikm

end function Wigner_d5

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_dSign
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute symmetry of wigner d function @ pi/2
!
!> @param bandWidth: max bandWidth
!> @param j: degree
!> @param k: first order
!> @param m: second order
!> @return:  +/-1 such that dSign(j, k, m) * d(j, |k|, |m|) == d(j, k, m)
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_dSign(j, k, m) result(sgn)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dSign

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: j
integer(kind=irg),INTENT(IN)            :: k
integer(kind=irg),INTENT(IN)            :: m
integer(kind=irg)                       :: sgn

sgn = 1

if ((k.lt.0).and.(m.lt.0)) then     ! d^j_{-k,-m} = (-1)^(   k- m) d^j_{k,m}
    if (mod(k-m,2).eq.1) then
        sgn = -1
        RETURN
    end if
else 
    if (m.lt.0) then                ! d^j_{ k,-m} = (-1)^(j+ k+2m) d^j_{k,m}
        if (mod(j+k,2).eq.1) then 
            sgn = -1
            RETURN
        end if 
    else 
        if (k.lt.0) then            ! d^j_{-k, m} = (-1)^(j+2k+3m) d^j_{k,m}
            if (mod(j+m,2).eq.1) then
                sgn = -1
                RETURN
            end if 
        end if 
    end if 
end if

end function Wigner_dSign

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_capD
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   : compute Wigner (uppercase) D function
!> @param j : degree in d^j_{k,m}(beta)
!> @param k : first order in d^j_{k,m}(beta)
!> @param m : second order in d^j_{k,m}(beta)
!> @param eu: ZYZ euler angles
!> @return  : D^j_{k,m}(eu)
!> @note    : NAN when j < max(|k|, |m|)
!> @note    : equivalent to the mathematica function WignerD[{j, k, m}, eu[2], eu[1], eu[0]]
!> @note    : for ZYZ euler angles this d^j_{k,m}(eu[1]) * exp(I*m*eu[0] + I*k*eu[2])
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_capD(j, k, m, eu) result(wD)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_capD

integer(kind=ill),INTENT(IN)        :: j
integer(kind=ill),INTENT(IN)        :: k
integer(kind=ill),INTENT(IN)        :: m
real(kind=dbl),INTENT(IN)           :: eu(3)
complex(kind=dbl)                   :: wD 

real(kind=dbl)                      :: sm
logical                             :: sgn 

sm = eu(1) * dble(m) + eu(3) * dble(k)
sgn = .FALSE.
if (eu(2).lt.0.D0) sgn = .TRUE.

wD = cmplx(cos(sm), sin(sm)) * cmplx(Wigner_d5(j, k, m, cos(eu(2)), sgn), 0.D0)

end function Wigner_capD


!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_dTable1
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute a table of Wigner (lowercase) d functions at pi/2
!
!> @param jMax: max bandWidth
!> @return: dTable: location to write d^j_{k,m}(\frac{\pi}{2}) for j = [0,jMax), k = [0,jMax), m = [0, jMax)
!
!> @note: d^j_{k,m} located at k * jMax * jMax + m * jMax + j in original C++ code; 3D array here ...
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive subroutine Wigner_dTable1(jMax, wigD)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dTable1

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: jMax
real(kind=dbl),INTENT(INOUT)            :: wigD(0:jMax-1,0:jMax-1,0:jMax-1)

integer(kind=ill)                       :: k, m, j
real(kind=dbl)                          :: d_kkm, d_kmk, d_k1km, d_k1mk, djkm, d_j1km, d_j2km, djmk
logical                                 :: km

wigD = D_QNAN

! recursively compute wigner d function values for all d^j_{k,m} where k >= m >= 0
! use symmetry to fill in table for m < 0 and m > k: /d^j_{ m, k} = (-1)^(  k- m) d^j_{k,m}
kloop: do k=0,jMax-1
    mloop: do m=0,k
        km = .FALSE.
        if (mod(k-m,2).eq.0) km=.TRUE.

! compute d^k_{k,m} with closed form solution
        d_kkm = Wigner_e_km(k, m) / 2.D0**dble(k) ! d^{k  }_{k,m}
        d_kmk = d_kkm 
        if (km.eqv..FALSE.) d_kmk = -d_kkm
        wigD(k, m, k) = d_kkm ! save d^{k  }_{k,m}
        wigD(k, k, m) = d_kmk ! save d^{k  }_{m,k}
        if ((k + 1).ne.jMax) then ! we don't need any higher order terms

! compute d^{k+1}_{k,m} with recursion
            d_k1km = d_kkm * Wigner_a_km1(k, m) ! d^{k+1}_{k,m}
            d_k1mk = d_k1km 
            if (km.eqv..FALSE.) d_k1mk = -d_k1km
            wigD(k+1, m, k) = d_k1km ! save d^{k+1}_{k,m}
            wigD(k+1, k, m) = d_k1mk ! save d^{k+1}_{m,k}
            if ((k + 2).ne.jMax) then ! we don't need any higher order terms        

! compute higher order terms with 3 term recursion
                d_j1km = d_k1km ! d^{j-1}_{k,m}
                d_j2km = d_kkm  ! d^{j-2}_{m,m}
                do j=k+2,jMax-1
                    djkm = Wigner_a_jkm1(j, k, m) * d_j1km - Wigner_b_jkm(j, k, m) * d_j2km ! d^{j}_{b,a}
                    djmk = djkm
                    if (km.eqv..FALSE.) djmk = -djkm
                    wigD(j, m, k) = djkm ! save d^{j}_{k,m}
                    wigD(j, k, m) = djmk ! save d^{j}_{m,k}
                    d_j2km = d_j1km
                    d_j1km = djkm
                end do
            end if 
        end if
    end do mloop
end do kloop

end subroutine Wigner_dTable1

!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_dTable2
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute a table of Wigner (lowercase) d functions at 0 <= beta <= pi
!
!> @param jMax: max bandWidth
!> @param t   : cos(beta)
!> @param nB  : 
!> @return: wigD: location to write d^j_{k,m}(beta) for all non negative j, k, m (must have space for jMax * jMax * jMax * 2 Reals)
!
!> @note: d^j_{k,m}(beta) located at (k * jMax * jMax + m * jMax + j)*2+0 in original C++ code; 4D array here ...
!> @note: d^j_{k,m}(pi-beta) located at (k * jMax * jMax + m * jMax + j)*2+1 in original C++ code; 4D array here ...
!> @note       : the table has unused (an uninitialized space) for j < max(|k|, |m|)
!> @note       : d^j_{k,m}(   beta) at table[(k * jMax * jMax + m * jMax + j)*2 + 0]
!> @note       : d^j_{k,m}(pi-beta) at table[(k * jMax * jMax + m * jMax + j)*2 + 1]
!> @note       : negative k/m values can be found with the following symmetry relationships
!>               d^j_{-k,-m}( beta) = (-1)^(k-m) d^j_{k,m}(     beta)
!>               d^j_{ k,-m}( beta) = (-1)^(j+k) d^j_{k,m}(pi - beta)
!>               d^j_{-k, m}( beta) = (-1)^(j+m) d^j_{k,m}(pi - beta)
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive subroutine Wigner_dTable2(jMax, t, nB, wigD)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dTable2

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: jMax
real(kind=dbl),INTENT(IN)               :: t
logical,INTENT(IN)                      :: nB
real(kind=dbl),INTENT(INOUT)            :: wigD(0:1,0:jMax-1,0:jMax-1,0:jMax-1)

logical                                 :: isType0 
real(kind=dbl)                          :: tc, tcN, c2, s2, t0, tN, cn, sn, cnN, snN, d_kkm, d_kkmN, ekm, a_km, a_kmN, &
                                           d_k1km, d_k1kmN, d_ikm, d_i2km, d_i1km, d_ikmN, d_i2kmN, d_i1kmN
integer(kind=ill)                       :: i, k, m, sign, signN, itmp  

! determine which branch of function is needed and compute cos/sin of half angles
isType0 = .FALSE.  ! std::signbit(t);//is this type 0 or type 2? (type 1 will be grouped with type 0)
if (t.lt.0.D0) isType0 = .TRUE.
tc  = 1.D0 - t
tcN = 1.D0 + t  ! tc for -t
c2  = sqrt(tcN * 0.5D0)   ! cos(acos(t)) == cos(beta / 2), always positive since at this point 0 <= beta <= pi
s2  = sqrt(tc  * 0.5D0)   ! sin(acos(t)) == sin(beta / 2), always positive since at this point 0 <= beta <= pi

t0 = t 
if (isType0.eqv..TRUE.) t0 = tc
tN = tcN
if (isType0.eqv..TRUE.) tN = -t

do k = 0, jMax-1
    do m = 0, k
! determine sign change for swapping k/m
        sign = 1
        signN = -1
        if (mod(k-m,2).eq.0) signN = 1
        if (nB.eqv..TRUE.) then   ! swap sign and signN
            itmp = sign
            sign = signN
            signN = sign
        end if 

! compute powers of cos/sin of beta / 2
        cn  = c2**(k+m) ! equation 20 for n = k+m for  t
        sn  = s2**(k-m) ! equation 20 for n = k-m for  t
        cnN = s2**(k+m) ! equation 20 for n = k+m for -t
        snN = c2**(k-m) ! equation 20 for n = k-m for -t

! compute first term for three term recursion 
        ekm = Wigner_e_km(k, m)
        d_kkm  = cn  * sn  * ekm              ! equation 18, d^k_{k, m}(beta) for  t
        d_kkmN = cnN * snN * ekm              ! equation 18, d^k_{k, m}(beta) for -t
        wigD(0, k, m, k) = d_kkm  * sign 
        wigD(0, k, k, m) = d_kkm  * signN     ! symmetry from eq 9
        wigD(1, k, m, k) = d_kkmN * sign 
        wigD(1, k, k, m) = d_kkmN * signN     ! symmetry from eq 9
        if (jMax .ne. k+1) then  ! if j == k we're done

! compute second term for three term recursion 
            if (isType0.eqv..TRUE.) then
                a_km  = Wigner_a_km0(k, m, t0)
                a_kmN = Wigner_a_km2(k, m, tN)
            else
                a_kmN = Wigner_a_km0(k, m, tN)
                a_km  = Wigner_a_km2(k, m, t0)
            end if
            d_k1km  = d_kkm  * a_km     ! equation 19, d^{k+1}_{k, m}(beta) for  t
            d_k1kmN = d_kkmN * a_kmN    ! equation 19, d^{k+1}_{k, m}(beta) for -t
            wigD(0, k+1, m, k) = d_k1km  * sign
            wigD(0, k+1, k, m) = d_k1km  * signN        ! symmetry from eq 9
            wigD(1, k+1, m, k) = d_k1kmN * sign
            wigD(1, k+1, k, m) = d_k1kmN * signN        ! symmetry from eq 9
            if (jMax.ne.k+2) then ! if j == k + 1 we're done

! recursively compute by degree to j
                d_i2km = d_kkm
                d_i1km = d_k1km
                d_i2kmN = d_kkmN
                d_i1kmN = d_k1kmN

                do i = k + 2, jMax-1
                    if (isType0.eqv..TRUE.) then 
                        d_ikm = Wigner_a_jkm0(i, k, m, t0) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km  ! equation 10, d^i_{k, m}(beta)
                    else
                        d_ikm = Wigner_a_jkm2(i, k, m, t0) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km  ! equation 10, d^i_{k, m}(beta)
                    end if
                    d_i2km = d_i1km
                    d_i1km = d_ikm
                    if (isType0.eqv..TRUE.) then 
                        d_ikmN = Wigner_a_jkm2(i, k, m, tN) * d_i1kmN - Wigner_b_jkm(i, k, m) * d_i2kmN  ! equation 10, d^i_{k, m}(beta)
                    else
                        d_ikmN = Wigner_a_jkm0(i, k, m, tN) * d_i1kmN - Wigner_b_jkm(i, k, m) * d_i2kmN  ! equation 10, d^i_{k, m}(beta)
                    end if 
                    d_i2kmN = d_i1kmN
                    d_i1kmN = d_ikmN
                    wigD(0, i, m, k) = d_i1km  * sign
                    wigD(0, i, k, m) = d_i1km  * signN
                    wigD(1, i, m, k) = d_i1kmN * sign
                    wigD(1, i, k, m) = d_i1kmN * signN
                end do 
            end if 
        end if 
    end do 
end do 

end subroutine Wigner_dTable2

!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_rotateHarmonics
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  rotate the spherical harmonic transformation of a real function
!
!> @param jMax: max bandWidth
!> @param bw : bandwidth of harmonic coefficients (max l exclusive)
!> @param alm: spherical harmonic coefficients to rotate with \hat{a}^l_{m} at alm[m * bw + j]
!> @param blm: location to write rotated spherical harmonic coefficients with \hat{b}^l_{m} at blm[m * bw + j]
!> @param qu : rotation to apply
!> @note     : b^l_m = \sum_{n=-1}^l a^l_n D^l_{m,n}(qu)
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive subroutine Wigner_rotateHarmonics(jMax, bw, alm, blm, qu)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_rotateHarmonics

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: jMax 
integer(kind=irg),INTENT(IN)            :: bw 
complex(kind=dbl),INTENT(IN)            :: alm(0:bw-1,0:bw-1)
complex(kind=dbl),INTENT(INOUT)         :: blm(0:bw-1,0:bw-1)
real(kind=dbl),INTENT(IN)               :: qu(4)

real(kind=dbl)                          :: eu(3), rr, ri, ir, ii, dmn0, dmn1 
complex(kind=dbl)                       :: almRot(0:bw-1,0:bw-1), expAlpha, expGamma, alGamma, vp, vc
real(kind=dbl)                          :: wigD(0:1,0:bw-1,0:bw-1,0:bw-1)
logical                                 :: sgn 
integer(kind=irg)                       :: m, n, j

! convert rotation to ZYZ euler angles and clear output array
eu = Wigner_qu2zyz(qu)
almRot = cmplx(0.D0,0.D0)

! construct wigner (lowercase) d lookup table for beta
sgn = .TRUE.
if (eu(2).lt.0.D0) sgn = .FALSE.
call Wigner_dTable2(bw, cos(eu(2)), sgn, wigD)              ! compute wigner (lowercase) d(beta) once

! now that we can easily compute D^l_{m,n}(qu) do the actual summation
do m = 0, bw-1
    expAlpha = cmplx(cos(eu(3) * m), sin(eu(3) * m))        ! exp(I m gamma)
    do n = 0, bw-1
        expGamma = cmplx(cos(eu(1) * n), sin(eu(1) * n))    ! exp(I n alpha)
        do j = maxval( (/m, n/) ), bw-1 
            alGamma = alm(j, n) * expGamma                  ! \hat{a}^l_{n}
            rr = real(expAlpha) * real(alGamma)             ! ac
            ri = real(expAlpha) * aimag(alGamma)            ! ad
            ir = aimag(expAlpha) * real(alGamma)            ! bc
            ii = aimag(expAlpha) * aimag(alGamma)           ! bd
            vp = cmplx(rr - ii, ir + ri)                    ! expAlpha *      alGamma  = \hat{a}^l_{+n} * exp(I m gamma) * exp(I +n alpha)
            vc = cmplx(rr + ii, ir - ri)                    ! expAlpha * conj(alGamma) = \hat{a}^l_{-n} * exp(I m gamma) * exp(I -n alpha) * (-1)^n using symmetry of real SHT
            dmn0 = wigD(0, j, n, m)                         ! d^j_{m,n}(     beta)
            dmn1 = wigD(1, j, n, m)                         ! d^j_{m,n}(pi - beta) since d^j_{m,-n}( beta) = (-1)^(j+m) d^j_{m,n}(pi - beta)
            almRot(j,m) = almRot(j, m) + vp * dmn0          ! \hat{a}^l_{+n} * D^l_{m,+n}(eu)
            if (n.gt.0) then 
                if (mod(j+m+n,2).eq.0) then 
                    almRot(j,m) = almRot(j,m) + vc * dmn1   ! \hat{a}^l_{-n} * D^l_{m,-n}(eu)
                else
                    almRot(j,m) = almRot(j,m) - vc * dmn1   ! \hat{a}^l_{-n} * D^l_{m,-n}(eu)
                end if
            end if
        end do 
    end do 
end do

blm = almRot

end subroutine Wigner_rotateHarmonics

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_dPrime
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   : compute first derivative of Wigner (lowercase) d function (also called reduced wigner d) with respect to beta
!> @param j : degree in (d/dBeta) d^j_{k,m}(beta)
!> @param k : first order in (d/dBeta) d^j_{k,m}(beta)
!> @param m : second order in (d/dBeta) d^j_{k,m}(beta)
!> @param t : cos(beta)
!> @param nB: true/false for negative/positive beta
!> @return  : (d/dBeta) d^j_{k,m}(beta)
!> @note    : NAN when j < max(|k|, |m|)
!> @note    : equivalent to the mathematica function D[WignerD[{j, k, m}, beta], beta]
!> @note    : negative k/m values have the following symmetry relationships
!>            dPrime^j_{-k,-m}( beta) = (-1)^(k+m  ) dPrime^j_{k,m}(     beta)
!>            dPrime^j_{ k,-m}( beta) = (-1)^(j+k+1) dPrime^j_{k,m}(pi - beta)
!>            dPrime^j_{-k, m}( beta) = (-1)^(j+m+1) dPrime^j_{k,m}(pi - beta)
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_dPrime(j, k, m, t, nB) result(dd)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dPrime

IMPLICIT NONE

integer(kind=ill),INTENT(IN)            :: j
integer(kind=ill),INTENT(IN)            :: k
integer(kind=ill),INTENT(IN)            :: m
real(kind=dbl),INTENT(IN)               :: t 
logical,INTENT(IN)                      :: nB 
real(kind=dbl)                          :: dd

real(kind=dbl)                          :: csc, d0Term, d1Term 

! compute prefactor (same for all j, k, and m for a given beta)
csc = 1.D0 / sqrt(1.D0 - t * t)     ! csc(beta), cot(beta) is csc * t
if (nB.eqv..TRUE.) csc = -csc 

! compute derivative
d0Term = Wigner_d5(j, k, m, t, nB) * (t * k - m) * csc  ! d^j_{k,m}(beta) * (k*cot(beta) - m*csc(beta))
if (j.eq.k) then 
    d1Term = 0.D0 
else 
    d1Term = Wigner_d5(j, k+1, m, t, nB) * sqrt( dble( (j - k) * (j + k + 1) ) )  ! d^j_{k+1,m}(beta) * (j+k+1) * sqrt( (j-k) / (j+kl+1) )
end if 

dd = d0Term - d1Term

end function Wigner_dPrime


!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_dPrime2
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   : compute second derivative of Wigner (lowercase) d function (also called reduced wigner d) with respect to beta
!> @param j : degree in (d/dBeta)^2 d^j_{k,m}(beta)
!> @param k : first order in (d/dBeta)^2 d^j_{k,m}(beta)
!> @param m : second order in (d/dBeta)^2 d^j_{k,m}(beta)
!> @param t : cos(beta)
!> @param nB: true/false for negative/positive beta
!> @return  : (d/dBeta)^2 d^j_{k,m}(beta)
!> @note    : NAN when j < max(|k|, |m|)
!> @note    : equivalent to the mathematica function D[WignerD[{j, k, m}, beta], {beta, 2}]
!> @note    : negative k/m values have the following symmetry relationships
!>               dPrime2^j_{-k,-m}( beta) = (-1)^(k+m) dPrime2^j_{k,m}(     beta)
!>               dPrime2^j_{ k,-m}( beta) = (-1)^(j+k) dPrime2^j_{k,m}(pi - beta)
!>               dPrime2^j_{-k, m}( beta) = (-1)^(j+m) dPrime2^j_{k,m}(pi - beta)
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in Wigner.hpp
!--------------------------------------------------------------------------
recursive function Wigner_dPrime2(j, k, m, t, nB) result(dd)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dPrime2

IMPLICIT NONE

integer(kind=ill),INTENT(IN)            :: j
integer(kind=ill),INTENT(IN)            :: k
integer(kind=ill),INTENT(IN)            :: m
real(kind=dbl),INTENT(IN)               :: t 
logical,INTENT(IN)                      :: nB 
real(kind=dbl)                          :: dd

real(kind=dbl)                          :: csc, d0Term, d1Term, d2Term, rjk, d0Coef, d1Coef, d2Coef 

! compute prefactor (same for all j, k, and m for a given beta)
csc = 1.D0 / sqrt(1.D0 - t * t)     ! csc(beta), cot(beta) is csc * t
if (nB.eqv..TRUE.) csc = -csc 

! compute derivative prefactors
rjk  = sqrt( dble( (j - k    ) * (j + k + 1) ) )
d0Coef = ( t * t * k * k + t * m * (1 - 2 * k) + (m * m - k) ) * csc * csc
d1Coef = rjk * (t * (1 + 2 * k) - 2 * m) * csc
d2Coef = rjk * sqrt( dble( (j - k - 1) * (j + k + 2) ) )

! compute derivative
d0Term = Wigner_d5(j, k  , m, t, nB) * d0Coef
if (k.ge.j) then 
    d1Term = 0.D0 
else 
    d1Term = Wigner_d5(j, k+1, m, t, nB) * d1Coef
end if 
if (k+1.ge.j) then 
    d2Term = 0.D0
else
    d2Term = Wigner_d5(j, k+2, m, t, nB) * d2Coef
end if

dd = d0Term - d1Term + d2Term

end function Wigner_dPrime2

end module Wigner
