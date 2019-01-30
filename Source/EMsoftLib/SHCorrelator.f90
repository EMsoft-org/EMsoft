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
! SERVICES LOSS OF USE, DATA, OR PROFITS OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################
!--------------------------------------------------------------------------
! EMsoft:SHCorrelator.f90
!--------------------------------------------------------------------------
!
! MODULE: SHCorrelator
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Spherical Harmonic cross-correlation routines 
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
module SHcorrelator

use local
use Wigner

IMPLICIT NONE

public :: SH_interpolateMaxima

contains


!--------------------------------------------------------------------------
!
! FUNCTION: SH_interpolateMaxima
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   interpolate subpixel peak location from a 3d voxel grid
!
! @param p: neighborhood around peak
! @param x: location to store subpixel maxima location within neighborhood (x, y, z from -1->1)
! @param return: value of fit quadratic at maxima
!
!> @date 01/30/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function SH_interpolateMaxima(p, x, maxIter) result(vPeak)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_interpolateMaxima

IMPLICIT NONE

real(kind=dbl),INTENT(IN)           :: p(0:2,0:2,0:2)
real(kind=dbl),INTENT(INOUT)        :: x(0:2)
integer(kind=irg),INTENT(IN)        :: maxIter
real(kind=dbl)                      :: vPeak

integer(kind=irg)                   :: i 
real(kind=dbl)                      :: a000, a001, a002, a010, a020, a100, a200, a022, a011, a012, a021, a220, a110, a120, a122, &
                                       a210, a202, a101, a201, a102, a222, a211, a121, a112, a111, a212, a221, eps, xx, xy, &
                                       h00, h11, h22, h01, h12, h02, det, i00, i11, i22, i01, i12, i02, d0, d1, d2, step(0:2), &
                                       maxStep, yy, zz, zx, yz

! compute the 27 biquadratic coefficients, f(x,y,z) = a_{kji} x^i y^j z^k
! f(0, 0, 0) == a000
a000 = p(1,1,1)

! f(1,0,0) = a000 + a100 + a200 && f(-1,0,0) = a000 - a100 + a200
a001 = (p(1,1,2) - p(1,1,0)) / 2.D0
a002 = (p(1,1,2) + p(1,1,0)) / 2.D0 - a000

! same relationships for y and z
a010 = (p(1,2,1) - p(1,0,1)) / 2.D0
a020 = (p(1,2,1) + p(1,0,1)) / 2.D0 - a000
a100 = (p(2,1,1) - p(0,1,1)) / 2.D0
a200 = (p(2,1,1) + p(0,1,1)) / 2.D0 - a000

! f( 1, 1,0) = a000 + a100 + a200 + a010 + a020 + a110 + a210 + a120 + a220
! f( 1,-1,0) = a000 + a100 + a200 - a010 + a020 - a110 - a210 + a120 + a220
! f(-1, 1,0) = a000 - a100 + a200 + a010 + a020 - a110 + a210 - a120 + a220
! f(-1,-1,0) = a000 - a100 + a200 - a010 + a020 + a110 - a210 - a120 + a220
!  --> f( 1, 1,0) + f( 1,-1,0) + f(-1, 1,0) + f(-1,-1,0) = 4 * (a000 + a020 + a200 + a220)
!  --> f( 1, 1,0) - f( 1,-1,0) - f(-1, 1,0) + f(-1,-1,0) = 4 * a110
!  --> f( 1, 1,0) - f( 1,-1,0) + f(-1, 1,0) - f(-1,-1,0) = 4 * (a100 + a120)
!  --> f( 1, 1,0) + f( 1,-1,0) - f(-1, 1,0) - f(-1,-1,0) = 4 * (a010 + a210)
a022 = (p(1,2,2) + p(1,2,0) + p(1,0,2) + p(1,0,0)) / 4.D0 - a000 - a020 - a002
a011 = (p(1,2,2) - p(1,2,0) - p(1,0,2) + p(1,0,0)) / 4.D0
a012 = (p(1,2,2) + p(1,2,0) - p(1,0,2) - p(1,0,0)) / 4.D0 - a010
a021 = (p(1,2,2) - p(1,2,0) + p(1,0,2) - p(1,0,0)) / 4.D0 - a001

! same relationships for yz and zx
a220 = (p(2,2,1) + p(2,0,1) + p(0,2,1) + p(0,0,1)) / 4.D0 - a000 - a200 - a020
a110 = (p(2,2,1) - p(2,0,1) - p(0,2,1) + p(0,0,1)) / 4.D0
a120 = (p(2,2,1) + p(2,0,1) - p(0,2,1) - p(0,0,1)) / 4.D0 - a100
a210 = (p(2,2,1) - p(2,0,1) + p(0,2,1) - p(0,0,1)) / 4.D0 - a010
a202 = (p(2,1,2) + p(0,1,2) + p(2,1,0) + p(0,1,0)) / 4.D0 - a000 - a002 - a200
a101 = (p(2,1,2) - p(0,1,2) - p(2,1,0) + p(0,1,0)) / 4.D0
a201 = (p(2,1,2) + p(0,1,2) - p(2,1,0) - p(0,1,0)) / 4.D0 - a001
a102 = (p(2,1,2) - p(0,1,2) + p(2,1,0) - p(0,1,0)) / 4.D0 - a100

! similar relationships for corners
a222 = (p(2,2,2) + p(0,0,0) + p(0,2,2) + p(2,0,2) + p(2,2,0) + p(2,0,0) + p(0,2,0) + p(0,0,2)) / 8.D0 - a000 - a200 - a020 - &
       a002 - a022 - a202 - a220
a211 = (p(2,2,2) + p(0,0,0) + p(0,2,2) - p(2,0,2) - p(2,2,0) + p(2,0,0) - p(0,2,0) - p(0,0,2)) / 8.D0 - a011
a121 = (p(2,2,2) + p(0,0,0) - p(0,2,2) + p(2,0,2) - p(2,2,0) - p(2,0,0) + p(0,2,0) - p(0,0,2)) / 8.D0 - a101
a112 = (p(2,2,2) + p(0,0,0) - p(0,2,2) - p(2,0,2) + p(2,2,0) - p(2,0,0) - p(0,2,0) + p(0,0,2)) / 8.D0 - a110
a111 = (p(2,2,2) - p(0,0,0) - p(0,2,2) - p(2,0,2) - p(2,2,0) + p(2,0,0) + p(0,2,0) + p(0,0,2)) / 8.D0
a122 = (p(2,2,2) - p(0,0,0) - p(0,2,2) + p(2,0,2) + p(2,2,0) + p(2,0,0) - p(0,2,0) - p(0,0,2)) / 8.D0 - a100 - a120 - a102
a212 = (p(2,2,2) - p(0,0,0) + p(0,2,2) - p(2,0,2) + p(2,2,0) - p(2,0,0) + p(0,2,0) - p(0,0,2)) / 8.D0 - a010 - a012 - a210
a221 = (p(2,2,2) - p(0,0,0) + p(0,2,2) + p(2,0,2) - p(2,2,0) - p(2,0,0) - p(0,2,0) + p(0,0,2)) / 8.D0 - a001 - a201 - a021

! newton iterate to find maxima
x = 0.D0  !  initial guess at maximum voxel (z,y,x)
eps = sqrt(epsilon(1.D0))
do i = 0, maxIter-1 
! compute components of hessian matrix
    xx = x(0) * x(0) 
    yy = x(1) * x(1) 
    zz = x(2) * x(2)
    xy = x(0) * x(1) 
    yz = x(1) * x(2) 
    zx = x(2) * x(0)
    h00 = (a200 + a210 * x(1) + a201 * x(2) + a220 * yy + a202 * zz + a211 * yz + a221 * yy * x(2) + a212 * x(1) * zz + &
           a222 * yy * zz) * 2.D0
    h11 = (a020 + a021 * x(2) + a120 * x(0) + a022 * zz + a220 * xx + a121 * zx + a122 * zz * x(0) + a221 * x(2) * xx + &
           a222 * zz * xx) * 2.D0
    h22 = (a002 + a102 * x(0) + a012 * x(1) + a202 * xx + a022 * yy + a112 * xy + a212 * xx * x(1) + a122 * x(0) * yy + &
           a222 * xx * yy) * 2.D0
    h01 = a110 + a111 * x(2) + a112 * zz + (a210 * x(0) + a120 * x(1) + a211 * zx + a121 * yz + a212 * x(0) * zz + &
          a122 * x(1) * zz + (a220 * xy + a221 * xy * x(2) + a222 * xy * zz) * 2) * 2.D0
    h12 = a011 + a111 * x(0) + a211 * xx + (a021 * x(1) + a012 * x(2) + a121 * xy + a112 * zx + a221 * x(1) * xx + &
          a212 * x(2) * xx + (a022 * yz + a122 * yz * x(0) + a222 * yz * xx) * 2) * 2.D0
    h02 = a101 + a111 * x(1) + a121 * yy + (a102 * x(2) + a201 * x(0) + a112 * yz + a211 * xy + a122 * x(2) * yy + &
          a221 * x(0) * yy + (a202 * zx + a212 * zx * x(1) + a222 * zx * yy) * 2) * 2.D0
    
! build inverse of hessian matrix
    det = h00 * h11 * h22 - h00 * h12 * h12 - h11 * h02 * h02 - h22 * h01 * h01 + h01 * h12 * h02 * 2.D0
    i00 = (h11 * h22 - h12 * h12) / det
    i11 = (h22 * h00 - h02 * h02) / det
    i22 = (h00 * h11 - h01 * h01) / det
    i01 = (h02 * h12 - h01 * h22) / det
    i12 = (h01 * h02 - h12 * h00) / det
    i02 = (h12 * h01 - h02 * h11) / det

! compute gradient
    d0 = a100 + a110 * x(1) + a101 * x(2) + a120 * yy + a102 * zz + a111 * yz + a121 * yy * x(2) + a112 * x(1) * zz + &
         a122 * yy * zz + x(0) * (a200 + a210 * x(1) + a201 * x(2) + a220 * yy + a202 * zz + a211 * yz + a221 * yy * x(2) + &
         a212 * x(1) * zz + a222 * yy * zz) * 2.D0
    d1 = a010 + a011 * x(2) + a110 * x(0) + a012 * zz + a210 * xx + a111 * zx + a112 * zz * x(0) + a211 * x(2) * xx + &
         a212 * zz * xx + x(1) * (a020 + a021 * x(2) + a120 * x(0) + a022 * zz + a220 * xx + a121 * zx + a122 * zz * x(0) + &
         a221 * x(2) * xx + a222 * zz * xx) * 2.D0
    d2 = a001 + a101 * x(0) + a011 * x(1) + a201 * xx + a021 * yy + a111 * xy + a211 * xx * x(1) + a121 * x(0) * yy + &
         a221 * xx * yy + x(2) * (a002 + a102 * x(0) + a012 * x(1) + a202 * xx + a022 * yy + a112 * xy + a212 * xx * x(1) + &
         a122 * x(0) * yy + a222 * xx * yy) * 2.D0

! update x
    step = (/ i00 * d0 + i01 * d1 + i02 * d2, i01 * d0 + i11 * d1 + i12 * d2, i02 * d0 + i12 * d1 + i22 * d2 /)
    x = x - step 

! check for convergence
write (*,*) i, x, step 
    maxStep = maxval(abs(step)) 
    if (maxStep.lt.eps) EXIT
    if (i+1.eq.maxIter) x = 0.D0   ! don't interpolate if convergence wasn't reached
end do

! compute interpolated value of maxima
xx = x(0) * x(0) 
yy = x(1) * x(1) 
zz = x(2) * x(2)
xy = x(0) * x(1) 
yz = x(1) * x(2) 
zx = x(2) * x(0)
vPeak = a000                    + a111 * x(0) * x(1) * x(2) + a222 * xx   * yy   * zz + &
        a100 * x(0)             + a010 * x(1)               + a001 * x(2) + &
        a200 * xx               + a020 * yy                 + a002 * zz + &
        a110 * xy               + a011 * yz                 + a101 * zx + &
        a120 * x(0) * yy        + a012 * x(1) * zz          + a201 * x(2) * xx + &
        a210 * xx   * x(1)      + a021 * yy   * x(2)        + a102 * zz   * x(0) + &
        a220 * xx   * yy        + a022 * yy   * zz          + a202 * zz   * xx + &
        a112 * xy   * x(2)      + a211 * yz   * x(0)        + a121 * zx   * x(1) + &
        a122 * x(0) * yy   * zz + a212 * xx   * x(1) * zz   + a221 * xx   * yy   * x(2)

! zyx -> xyz
xx = x(2)
x(2) = x(0)
x(0) = xx

end function SH_interpolateMaxima



!--------------------------------------------------------------------------
!
! FUNCTION: SH_zyz2qu
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   convert ZYZ euler angles to quaternion
!
! @param eu : euler angles to convert to quaternion (Z, Y', Z'')
! @param pos: true/false to restrict rotation to [0,pi] (positive w)
! @result qu: location to write quaternion as w, x, y, z
!   
!> @date 01/30/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function SH_zyz2qu(eu, pos) result(qu)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_zyz2qu

use constants

IMPLICIT NONE

real(kind=dbl), INTENT(IN)          :: eu(0:2)
logical,INTENT(IN)                  :: pos 
real(kind=dbl)                      :: qu(0:3) 

real(kind=dbl)                      :: s, c, sigma, delta

c = 0.5D0 * cos(eu(1))
s = 0.5D0 * sin(eu(1))
sigma = 0.5D0 * (eu(2) + eu(0))
delta = 0.5D0 * (eu(2) - eu(0))

! this uses the epsijkd constant to define the 3D rotations convention (see constants.f90 module)
qu(0) = c * cos(sigma)
qu(1) = -epsijkd * s * sin(delta)
qu(2) = -epsijkd * s * cos(delta)
qu(3) = -epsijkd * s * sin(sigma)

if ((pos.eqv..TRUE.).and.(qu(0).lt.0.D0)) qu = -qu 

end function SH_zyz2qu


!--------------------------------------------------------------------------
!
! FUNCTION: SH_quatAverage
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   temporary helper function to average 2 quaternions for my garbage nonlinear optimization test
!
! @param qIn1: first quat to average
! @param qIn2: second quat to average
! @param qOut: average + normalized quaternion
!   
!> @date 01/30/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function SH_quatAverage(qIn1, qIn2) result(qOut)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_quatAverage

IMPLICIT NONE

real(kind=dbl),INTENT(IN)           :: qIn1(0:3)
real(kind=dbl),INTENT(IN)           :: qIn2(0:3)
real(kind=dbl)                      :: qOut(0:3)

qOut = 0.5D * (qIn1 + qIn2)
qOut = qOut/NORM2(qOut)

end function SH_quatAverage

!--------------------------------------------------------------------------
!
! FUNCTION: SH_correlate
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief   compute the cross correlation between two spherical functions
!
! @param flm: spherical harmonic coefficients for the first function
! @param gln: spherical harmonic coefficients for the second function
! @param qu : location to write rotation of maximum cross correlation as quaternion
! @return   : maximum cross correlation
!   
! from the original C++ code :
! //naive implementation (no symmetry) to make summation clear
! /*
! const bool realFft = true;//true/false to use half sized fft format
! const size_t dm = realFft ? bw : sl;//length of fastest indexing dimension
! for(size_t ic = 0; ic < sl; ic++) {
!     const int k = ic >= bw ? int(ic) - sl : ic;
!     const size_t ak = std::abs(k);
!     for(size_t ib = 0; ib < sl; ib++) {
!         const int n = ib >= bw ? int(ib) - sl : ib;
!         const size_t an = std::abs(n);
!         const size_t maxKN = std::max(ak, an);
!         for(size_t ia = 0; ia < dm; ia++) {
!             const int m = ia >= bw ? int(ia) - sl : ia;
!             const size_t am = std::abs(m);
!             const size_t start = std::max<size_t>(am, maxKN);
!             for(size_t j = start; j < bw; j++) {
!                 const Real dlkm = wigD[ak * bw * bw + am * bw + j] * wigner::dSign(j, k, m);//wigner::d<Real>(j, k, m);
!                 const Real dlnk = wigD[an * bw * bw + ak * bw + j] * wigner::dSign(j, n, k);//wigner::d<Real>(j, n, k);
!                 const std::complex<Real>& vflm = flm[am * bw + j];//\hat{f}^l_{|m|}
!                 const std::complex<Real>& vgln = gln[an * bw + j];//\hat{g}^l_{|n|}
!                 const std::complex<Real> f = std::signbit(m) ? std::conj(vflm) * Real(0 == am % 2 ? 1 : -1) : vflm;//symmetry of real SHT coefficients
!                 const std::complex<Real> g = std::signbit(n) ? std::conj(vgln) * Real(0 == an % 2 ? 1 : -1) : vgln;//symmetry of real SHT coefficients
!                 fxc[ic * sl * dm + ib * dm + ia] += f * std::conj(g) * dlkm * dlnk;
!             }
!         }
!     }
! }
! */

!> @date 01/30/19 MDG 1.0 original, based on Will Lenthe's classes in sht_xcorr.hpp
!--------------------------------------------------------------------------
recursive function SH_correlate(SHCOR, flm, gln, qu) result(peak)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_correlate

IMPLICIT NONE

type(SH_correlatorType),INTENT(IN)      :: SHCOR
complex(kind=dbl),INTENT(IN)            :: flm(:)
complex(kind=dbl),INTENT(IN)            :: gln(:)
real(kind=dbl),INTENT(IN)               :: qu(0:3)
real(kind=dbl)                          :: peak 

integer(kind=irg)                       :: j, k, m, n, bw, maxKN, m2, ind, match
complex(kind=dbl)                       :: v, vnc, vc
real(kind=dbl)                          :: rr, ri, ir, ii

! the above quadruple loop is conceptually simple but has many redundant calculations
! the loop that follows is mathematically equivalent (for SHT of real functions only!!) but much faster:
!  -use \hat{f}^l_{-m} = (-1)^m * \hat{f}^l_{m} for real valued functions (and the same for \hat{g}^l_{-n})
!  -build the fft of a real valued cross correlation
!  -precompute values of \hat{f}^l_{m} * d^l_{k,m}(\frac{\pi}{2}) [stored in fm]
!  -precompute values of \hat{g}^l_{n} * d^l_{n,k}(\frac{\pi}{2}) [stored in gn]
!  -eliminate redundant calculations from f * g and f * conj(g)

bw = SHCOR%bw

! loop over planes
do k = 0, bw-1
! precompute flm values * wigner d function
    do m = 0, bw-1
        do j = maxval( (/ m, k /) ), bw-1 
            fm(m * bw + j) = flm(m * bw + j) * wigD(k * bw * bw + m * bw + j) ! f^j_m * wigner::d<Real>(j, k, m)
        end do
    end do

! loop over rows
    do n = 0, bw-1
! precompute gln values * wigner d function
        maxKN = maxval( (/ k, n /) )
        do j = maxKN, bw-1
            gn(j) = conjg(gln(n * bw + j) * wigD(n * bw * bw + k * bw + j) ! \hat{g}^j_n * wigner::d<Real>(j, n, k)
        end do

! loop over columns
        do m = 0, bw-1 
! build a pair of values as dot product
            m2 = mod(m, 2)
            v = cmplx(0.D0,0.D0)
            vnc = cmplx(0.D0,0.D0)
            do j = maxval( (/ m, maxKN /) ), bw-1
! do complex multiplication of components by hand to eliminate duplicate flops from multiplying with conjugate
                ind = m * bw + j
                rr = real(fm(ind)) * real(gn(j))
                ri = real(fm(ind)) * aimag(gn(j))
                ir = aimag(fm(ind)) * real(gn(j))
                ii = aimag(fm(ind)) * aimag(gn(j))
                v = v + cmplx(rr - ii, ri + ir) ! pF[j] * gn[j]
                if (mod(j,2).eq.m2) then 
                    vnc = vnc + cmplx( rr + ii, ir - ri)  
                else 
                    vnc = vnc + cmplx(-(rr + ii), ri - ir)  ! +/- pF[g] * std::conj(gn[j])
                end if
            end do

 ! fill in symmetric values using symmetry from: wigner d function, sht of real signal, sht of real pattern
            match = mod(m + n,2)
            fxc(k * sl * bw + n * bw + m] = v ! fxc(k, n, m)
            if (k.gt.0) then
                if (match.eq.0) then
                    fxc((sl - k) * sl * bw + n * bw + m) = v ! fxc(-k, n, m)
                else 
                    fxc((sl - k) * sl * bw + n * bw + m) = -v ! fxc(-k, n, m)
                end if
            end if
            if (n.gt.0) then ! fill symmetry within a slice
                if (mod(k,2).eq.match) then 
                    vc = vnc 
                else
                    vc = -vnc
                end if
                fxc(k * sl * bw + (sl - n) * bw + m) = vc ! fxc(k, -n, m)
                if (k.gt.0) then 
                    if (match.eq.0) then 
                        fxc((sl - k) * sl * bw + (sl - n) * bw + m) = vc  ! fxc(-k, -n, m)
                    else
                        fxc((sl - k) * sl * bw + (sl - n) * bw + m) = -vc  ! fxc(-k, -n, m)
                    end if 
                end if
            end if 
        end do 
    end do
end do







end function SH_correlate


end module SHcorrelator
