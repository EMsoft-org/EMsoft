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
! FUNCTION: Wigner_e_km
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute \sqrt{\frac{(2k)!}{(k+m)!(k-m)!}} recursively (m <= k)
!
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: ekm where d^k_{k,m} = 2^-k * e_km
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_e_km(k, m) result(ekm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_e_km

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: k
integer(kind=irg),INTENT(IN)        :: m
real(kind=dbl)                      :: ekm 

integer(kind=irg)                   :: l 

ekm = 1.D0
do l=m+1,k 
    ekm = 2.D0 * ekm * sqrt( dble(l*(2*l-1)) / dble( 2 * (l+m) * (l-m) ) )
end do

end function Wigner_e_km

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_a_km
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute seeds for recursion coefficients
!
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: ekm where d^k_{k,m} = 2^-k * e_km
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_a_km(k, m) result(akm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_km

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: k
integer(kind=irg),INTENT(IN)        :: m
real(kind=dbl)                      :: akm 

integer(kind=irg)                   :: l 

akm = sqrt( dble(2*k+1) / dble(  4 * (k+m+1) * (k-m+1) ) ) * dble(-2*m)

end function Wigner_a_km

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_a_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute intermediate recursion coefficients
!
!> @param j: current j in recursive calculation of d^j_{k,m}
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: recursion coefficient
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_a_jkm(j, k, m) result(ajkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_a_jkm

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: j
integer(kind=irg),INTENT(IN)        :: k
integer(kind=irg),INTENT(IN)        :: m
real(kind=dbl)                      :: ajkm 

ajkm = Wigner_w_jkm(j,k,m) * dble((1-2*j)*k*m)

end function Wigner_a_jkm

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_b_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute intermediate recursion coefficients
!
!> @param j: current j in recursive calculation of d^j_{k,m}
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: recursion coefficient
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_b_jkm(j, k, m) result(bjkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_b_jkm

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: j
integer(kind=irg),INTENT(IN)        :: k
integer(kind=irg),INTENT(IN)        :: m
real(kind=dbl)                      :: bjkm 

bjkm = Wigner_w_jkm(j,k,m) * Wigner_v_jkm(j,k,m)

end function Wigner_b_jkm

!--------------------------------------------------------------------------
!
! FUNCTION: Wigner_v_jkm
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute intermediate recursion coefficients
!
!> @param j: current j in recursive calculation of d^j_{k,m}
!> @param k: k in d^k_{k,m}
!> @param m: m in d^k_{k,m}
!> @return: recursion coefficient
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_v_jkm(j, k, m) result(vjkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_v_jkm

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: j
integer(kind=irg),INTENT(IN)        :: k
integer(kind=irg),INTENT(IN)        :: m
real(kind=dbl)                      :: vjkm 

vjkm = dble(j) * sqrt( dble( (j+k-1) * (j-k-1) * (j+m-1) * (j-m-1) ) )

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
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_w_jkm(j, k, m) result(wjkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_w_jkm

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: j
integer(kind=irg),INTENT(IN)        :: k
integer(kind=irg),INTENT(IN)        :: m
real(kind=dbl)                      :: wjkm 

wjkm = 1.D0 / ( dble(j-1) * sqrt( dble( (j+k) * (j-k) * (j+m) * (j-m) ) ) )

end function Wigner_w_jkm


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! The main routines
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_d
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
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_d(j, k, m) result(djkm)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_d

use, intrinsic :: IEEE_ARITHMETIC

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: j
integer(kind=irg),INTENT(IN)            :: k
integer(kind=irg),INTENT(IN)            :: m
real(kind=dbl)                          :: djkm

real(kind=dbl)                          :: d_kkm, d_k1km, d_ikm, d_i1km, d_i2km 
integer(kind=irg)                       :: i

! require 0 <= m <= k <= j (handle with symmetry where possible)
! we'll reorganize the indices by means of recursive calls to itself...

if ((k.lt.0).and.(m.lt.0)) then ! d^j_{-k,-m} = (-1)^(   k- m) d^j_{k,m}
! in C++:    return (    k-  m) % 2 == 0 ? d<Real>(j, -k, -m) : -d<Real>(j, -k, -m);
    if (mod(k-m,2).eq.0) then 
        djkm = Wigner_d(j,-k,-m)
        RETURN
    else
        djkm = -Wigner_d(j,-k,-m)
        RETURN
    end if 
else 
    if (m.lt.0) then  ! d^j_{ k,-m} = (-1)^(j+ k+2m) d^j_{k,m}
! in C++:   return (j+  k+2*m) % 2 == 0 ? d<Real>(j,  k, -m) : -d<Real>(j,  k, -m);
        if (mod(j+k+2*m,2).eq.0) then
            djkm = Wigner_d(j,k,-m)
            RETURN
        else
            djkm = -Wigner_d(j,k,-m)
            RETURN     
        end if 
    else
        if (k.lt.0) then  ! d^j_{-k, m} = (-1)^(j+2k+3m) d^j_{k,m}
! in C++:    return (j+2*k+3*m) % 2 == 0 ? d<Real>(j, -k,  m) : -d<Real>(j, -k,  m);
            if (mod(j+2*k+3*m,2).eq.0) then
                djkm = Wigner_d(j,-k,m)
                RETURN
            else
                djkm = -Wigner_d(j,-k,m)
                RETURN
            end if
        else
            if (k.lt.m) then  ! d^j_{ m, k} = (-1)^(   k- m) d^j_{k,m}
! in C++:    return (    k-  m) % 2 == 0 ? d<Real>(j,  m,  k) : -d<Real>(j,  m,  k);
                if (mod(k-m,2).eq.2) then 
                    djkm = Wigner_d(j,m,k)
                    RETURN
                else
                    djkm = -Wigner_d(j,m,k)
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
d_k1km = d_kkm * Wigner_a_km(k, m)
if (j.eq.k+1) then
    djkm = d_k1km
    RETURN
end if

! recursively compute
d_i2km = d_kkm 
d_i1km = d_k1km
do i=k+2,j
    d_ikm = Wigner_a_jkm(i, k, m) * d_i1km - Wigner_b_jkm(i, k, m) * d_i2km
    d_i2km = d_i1km
    d_i1km = d_ikm
end do
djkm = d_ikm

end function Wigner_d

!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_dTable
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  compute a table of Wigner (lowercase) d functions at pi/2
!
!> @param bandWidth: max bandWidth
!> @return: dTable   d^j_{k,m}(\frac{\pi}{2}) for j 0->bandWidth-1, k 0->bandWidth-1, m 0->bandWidth
!
!> @note: d^j_{k,m} located at k * bandWidth * bandWidth + m * bandWidth + j
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive subroutine Wigner_dTable(bandWidth, wigD)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dTable

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: bandWidth
real(kind=dbl),INTENT(INOUT)            :: wigD(0:bandWidth-1,0:bandWidth-1,0:bandWidth-1)

integer(kind=irg)                       :: a, b, j ! , na, nb
real(kind=dbl)                          :: d_bba, d_bab, d_b1ba, d_b1ab, djba, d_j1ba, d_j2ba, djab 
logical                                 :: ba

wigD = D_QNAN

! recursively compute wigner d function values for all d^j_{ba} where b >= a >= 0
! use symmetry to fill in table for a < 0 and a > b: /d^j_{ a, b} = (-1)^(  b- a) d^j_{b,a}
bloop: do b=0,bandWidth-1
    ! nb = 0
    ! if (b.gt.0) nb = bandWidth-b
    aloop: do a=0,b
        ! na = 0
        ! if (a.gt.0) na = bandWidth-a
        ba = .FALSE.
        if (mod(b-a,2).eq.0) ba=.TRUE.

! compute d^b_{ba} with closed form solution
        d_bba = Wigner_e_km(b, a) / 2.D0**dble(b) ! d^{b  }_{b,a}
        d_bab = d_bba 
        if (ba.eqv..FALSE.) d_bab = -d_bba
        wigD(b, a, b) = d_bba ! save d^{b  }_{b,a}
        wigD(b, b, a) = d_bab ! save d^{b  }_{a,b}
        if ((b + 1).ne.bandWidth) then ! we don't need any higher order terms

! compute d^{b+1}_{b,a} with recursion
            d_b1ba = d_bba * Wigner_a_km(b, a) ! d^{b+1}_{b,a}
            d_b1ab = d_b1ba 
            if (ba.eqv..FALSE.) d_b1ab = -d_b1ba
            wigD(b+1, a, b) = d_b1ba ! save d^{b+1}_{b,a}
            wigD(b+1, b, a) = d_b1ab ! save d^{b+1}_{a,b}
            if ((b + 2).ne.bandWidth) then ! we don't need any higher order terms        

! compute higher order terms with 3 term recursion
                d_j1ba = d_b1ba ! d^{j-1}_{b,a}
                d_j2ba = d_bba  ! d^{j-2}_{b,a}
                do j=b+2,bandWidth-1
                    djba = Wigner_a_jkm(j, b, a) * d_j1ba - Wigner_b_jkm(j, b, a) * d_j2ba ! d^{j}_{b,a}
                    djab = djba 
                    if (ba.eqv..FALSE.) djab = -djba
                    wigD(j, a, b) = djba ! save d^{j}_{b,a}
                    wigD(j, b, a) = djab ! save d^{j}_{a,b}
                    d_j2ba = d_j1ba
                    d_j1ba = djba
                end do
            end if 
        end if
    end do aloop
end do bloop


end subroutine Wigner_dTable


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
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function Wigner_dSign(j, k, m) result(sgn)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_dSign

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: j
integer(kind=irg),INTENT(IN)            :: k
integer(kind=irg),INTENT(IN)            :: m
integer(kind=irg)                       :: sgn

sgn = 1

if ((k.lt.0).and.(m.lt.0)) then  ! d^j_{-k,-m} = (-1)^(   k- m) d^j_{k,m}
    if (mod(k-m,2).eq.1) then
        sgn = -1
        RETURN
    end if
else 
    if (m.lt.0) then ! d^j_{ k,-m} = (-1)^(j+ k+2m) d^j_{k,m}
        if (mod(j+k+2*m,2).eq.1) then 
            sgn = -1
            RETURN
        end if 
    else 
        if (k.lt.0) then  ! d^j_{-k, m} = (-1)^(j+2k+3m) d^j_{k,m}
            if (mod(j+2*k+3*m,2).eq.1) then
                sgn = -1
                RETURN
            end if 
        end if 
    end if 
end if

end function Wigner_dSign


!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_coeffTable
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief  construct a table of prefactors for the captial D function
!
!> @param jMax: maximum j value to build matrix for (exclusive)
!> @return: dTable   d^j_{k,m}(\frac{\pi}{2}) for j 0->bandWidth-1, k 0->bandWidth-1, m 0->bandWidth
!
!> @note: table(j, m1, m2) = sqrt( ( (j+m1)! * (j-m2)! ) / ( (j+m2)! * (j-m1)! ) ) / (m1-m2)! if m2 <= m1, table(j, m2, m1)
!> @note: there is overflow even for very small numbers (j ~ 12) when computed directly so a recursive calculation is used instead
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive subroutine Wigner_coeffTable(jMax, cTable)
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_coeffTable

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: jMax
real(kind=dbl),INTENT(INOUT)            :: cTable(jMax * (2*jMax-1) * (2*jMax-1)) 

integer(kind=irg)                       :: j, m1, m2, sl
real(kind=dbl)                          :: ratio 

sl = 2*jMax-1
cTable = D_QNAN

do j=0,jMax-1
    do m2 = -j, j
        cTable(j * sl * sl + (jMax-1 + m2) * sl + (jMax-1 + m2)) = 1.D0
        do m1 = m2, j-1 
            ratio = sqrt( dble((j - m1) * (j + m1 + 1)) / dble(m1 + 1 - m2) )
            cTable(j * sl * sl + (m2 + jMax-1) * sl + (m1+1 + jMax-1)) = &
            cTable(j * sl * sl + (m2 + jMax-1) * sl + (m1 + jMax-1)) * ratio
        end do
    end do
end do

do j=0,jMax-1
    do m2 = -j, j
        do m1 = -j, m2
            cTable(j * sl * sl + (m2 + jMax-1) * sl + (m1 + jMax-1)) = &
            cTable(j * sl * sl + (m1 + jMax-1) * sl + (m2 + jMax-1)) 
        end do
    end do
end do

end subroutine Wigner_coeffTable

!--------------------------------------------------------------------------
!
! SUBROUTINE: Wigner_capD
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief compute Wigner (capital) D function for a given rotation
!> @param jMax : maximum j value to build matrix for (exclusive)
!> @param qu   : rotation to calculate matrix for as quaternion
!> @param cTable : prefactor table built via coeffTable(jMax)
!> @param wigD : location to write wigner D matrix with j incrementing slowest and m1 fastest for D^j_{m2,m1}(qu)
!
!> @note       : wigner (lowercase) d function is (uppercase) D for qu = {0.707107, 0.0, 0.707107, 0.0}
!
!> @reference  : http://moble.github.io/spherical_functions/WignerDMatrices.html#mjx-eqn-eqD_RbApprox0
!
!> @date 01/29/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive subroutine Wigner_capD(jMax, qu, cTable, wigD) 
!DEC$ ATTRIBUTES DLLEXPORT :: Wigner_capD

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: jMax
real(kind=dbl),INTENT(IN)               :: qu(0:3)
real(kind=dbl),INTENT(IN)               :: cTable(jMax * (2*jMax-1) * (2*jMax-1))
complex(kind=dbl),INTENT(INOUT)         :: wigD(jMax * (2*jMax-1) * (2*jMax-1))

integer(kind=irg)                       :: j, m, iJ, m1, m2, ms, rhoMin, rhoMax, N1, N2, MM, rho, sideLength 
real(kind=dbl)                          :: rA, rB, pA, pB, rL, rS, eps, rLpow, absRRatioSquared, d, phi1, phi2, sm 
complex(kind=dbl)                       :: pre1, pre2 
logical                                 :: smallA

! extract a and b parts of quaternion as magnitude and phase
rA = sqrt(qu(0)*qu(0) + qu(3)*qu(3) ) ! |w + J * z|
rB = sqrt(qu(2)*qu(2) + qu(1)*qu(1) ) ! |y + J * x|

! see the infamous rotations tutorial paper for an explanation of the epsijkd parameter
pA = atan2(-epsijkd * qu(3), qu(0)        ) ! phase of w + J * z
pB = atan2(-epsijkd * qu(1), -epsijkd * qu(2)) ! phase of w + J * z

! determine which half of the quaternion is smallest
smallA = .TRUE.
if (rB.lt.rA) then
    smallA = .FALSE.
    rS = rB
    rL = rA 
else
    rS = rA
    rL = rB 
end if

eps = epsilon(1.D0) * 8
sideLength = 2*jMax-1

! check for special case
if (rS.le.eps) then  ! one of the complex halves is ~0 so we have a simplified expression for wigD
    wigD = cmplx(0.D0,0.D0)
    do j = 0, jMax - 1
        iJ = j * sideLength * sideLength
        if (smallA.eqv..TRUE.) then
            do  m = -j, j 
                rLpow = rL ** dble(-2*m)
                if (mod(j+m,2).eq.0) then 
                    wigD(iJ + (m + jMax-1) * sideLength + (m + jMax-1)) = rLpow 
                else 
                    wigD(iJ + (m + jMax-1) * sideLength + (m + jMax-1)) = -rLpow 
                end if
            end do
        else
            do m = -j, j 
                wigD(iJ + (m + jMax-1) * sideLength + (m + jMax-1)) = rL ** dble(2*m)
            end do 
        end if
    end do
else  ! otherwise do the general case
    absRRatioSquared = -rS * rS / (rL * rL)
    do j = 0, jMax-1 
        iJ = j * sideLength * sideLength
        do m2 = -j, 0
            do m1 = m2, -m2
                ms = -m2
                if (smallA.eqv..FALSE.) ms = m2
                rhoMin = maxval( (/0, ms - m1 /) )
! protect against overflow with polar decomposition of rA and rB and remove factor of absRRatioSquared^rhoMin
! rS could be very small making rS^(m1-ms) very large for negative exponents
! avoid underflow by adding 2*rhoMin to the exponent (always posive, underflow should be ok)
                d = cTable(j * sideLength * sideLength + (m1 + jMax-1) * sideLength + (ms + jMax-1)) * &
                        rL**dble(2 * j - m1 + ms - 2 * rhoMin) * rS**dble(m1 - ms + 2 * rhoMin)
                if (d.eq.0.D0) then
                    wigD(iJ + ( m2 + jMax-1) * sideLength + ( m1 + jMax-1)) = cmplx(0.D0,0.D0)  ! D_{m2,m1}
                    if (abs(m1).ne.abs(m2)) then
                        wigD(iJ + (-m2 + jMax-1) * sideLength + (-m1 + jMax-1)) = cmplx(0.D0,0.D0) ! D_{-m2,-m1}(R) = (-1)^{m2+m1} \bar{D}_{m2,m1}(R)
                        wigD(iJ + ( m1 + jMax-1) * sideLength + ( m2 + jMax-1)) = cmplx(0.D0,0.D0) ! D_{m1,m2}(R) = \bar{D}_{m2,m1}(\bar{R})
                        wigD(iJ + (-m1 + jMax-1) * sideLength + (-m2 + jMax-1)) = cmplx(0.D0,0.D0) ! D_{-m1,-m2}(R) = (-1)^{m2+m1} D_{m2,m1}(\bar{R})
                    else 
                        if (m1.ne.0) then
                            wigD(iJ + (-m2 + jMax-1) * sideLength + (-m1 + jMax-1)) = cmplx(0.D0,0.D0) ! D_{-m2,-m1}(R) = (-1)^{m2+m1} \bar{D}_{m2,m1}(R)
                        end if
                    end if
                else 
                    if (smallA.eqv..TRUE.) then
                        if (mod(j + m1 + rhoMin,2).ne.0) d = -d
                    else
                        if (mod(rhoMin,2).ne.0) d = -d
                    end if
                    phi1 =  pA * (m1 + m2) +  pB        * (m1 - m2)
                    phi2 = -pA * (m1 + m2) + (pB + cPi) * (m1 - m2)
                    pre1 = cmplx(cos(phi1), sin(phi1)) * d
                    pre2 = cmplx(cos(phi2), sin(phi2)) * d
                    rhoMax = minval( (/j + ms, j - m1 /) )
                    N1 = j + ms + 1
                    N2 = j - m1 + 1
                    MM = m1 - ms

                    sm = 1.D0
                    do rho = rhoMax, rhoMin+1, -1 ! for rho in range(rhoMax, rhoMin, -1):
                        sm = sm * ( absRRatioSquared * dble((N1 - rho) * (N2 - rho)) ) / dble(rho * (MM + rho))
                        sm = sm + 1.D0
                    end do 

                    wigD(iJ + ( m2 + jMax-1) * sideLength + ( m1 + jMax-1)) = pre1 * sm ! D_{m2,m1}(R)
                    if (abs(m1).ne.abs(m2)) then
                        if (mod(m1 + m2, 2).eq.0) then
                            wigD(iJ + (-m2 + jMax-1) * sideLength + (-m1 + jMax-1)) =  conjg(pre1) * sm ! D_{-m2,-m1}(R) = (-1)^{m2+m1} \bar{D}_{m2,m1}(R)
                            wigD(iJ + (-m1 + jMax-1) * sideLength + (-m2 + jMax-1)) =        pre2  * sm ! D_{-m1,-m2}(R) = (-1)^{m2+m1} D_{m2,m1}(\bar{R})
                        else 
                            wigD(iJ + (-m2 + jMax-1) * sideLength + (-m1 + jMax-1)) = -conjg(pre1) * sm ! D_{-m2,-m1}(R) = (-1)^{m2+m1} \bar{D}_{m2,m1}(R)
                            wigD(iJ + (-m1 + jMax-1) * sideLength + (-m2 + jMax-1)) = -      pre2  * sm ! D_{-m1,-m2}(R) = (-1)^{m2+m1} D_{m2,m1}(\bar{R})
                        end if
                        wigD(iJ + ( m1 + jMax-1) * sideLength + ( m2 + jMax-1)) =  conjg(pre2) * sm ! D_{m1,m2}(R) = \bar{D}_{m2,m1}(\bar{R})
                    else 
                        if (m1.ne.0) then
                            if (mod(m1 + m2,2).eq.0) then
                                wigD(iJ + (-m2 + jMax-1) * sideLength + (-m1 + jMax-1)) =  conjg(pre1) * sm ! D_{-m2,-m1}(R) = (-1)^{m2+m1} \bar{D}_{m2,m1}(R)
                            else
                                wigD(iJ + (-m2 + jMax-1) * sideLength + (-m1 + jMax-1)) = -conjg(pre1) * sm ! D_{-m2,-m1}(R) = (-1)^{m2+m1} \bar{D}_{m2,m1}(R)
                            end if
                        end if
                    end if
                end if
            end do
        end do
   end do 
end if

end subroutine Wigner_capD

end module Wigner
