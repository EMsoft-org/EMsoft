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
! EMsoft:Ylm.f90
!--------------------------------------------------------------------------
!
! MODULE: Ylm
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Spherical Harmonic stuff
! 
!> @date 01/13/19 MDG 1.0 original, based on Will Lenthe's C++ routines, tested against Mathematica
!--------------------------------------------------------------------------
module Ylm

use local

IMPLICIT NONE

public :: SH_PT, SH_YR, SH_initialize, SH_ComputeP, SH_ComputeY, SH_ComputeYlm

contains

!--------------------------------------------------------------------------
!
! FUNCTION: SH_PT
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief combine l and m into an array index for Spherical Harmonics
!
!> @details Based on https://arxiv.org/pdf/1410.1748.pdf
!
!> @param l SH index
!> @param m SH index
!
!> @date 01/12/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function SH_PT(l, m) result(PT)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_PT

IMPLICIT NONE

integer(kind=irg), INTENT(IN)   :: l
integer(kind=irg), INTENT(IN)   :: m
integer(kind=irg)               :: PT

PT = m + l * (l+1) / 2

end function SH_PT

!--------------------------------------------------------------------------
!
! FUNCTION: SH_YR
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief combine l and m into an array index for Spherical Harmonics
!
!> @details Based on https://arxiv.org/pdf/1410.1748.pdf
!
!> @param l SH index
!> @param m SH index
!
!> @date 01/12/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function SH_YR(l, m) result(PT)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_YR

IMPLICIT NONE

integer(kind=irg), INTENT(IN)   :: l
integer(kind=irg), INTENT(IN)   :: m
integer(kind=irg)               :: PT

PT = m + l * (l+1) 

end function SH_YR


!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_initialize 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize Spherical Harmonic auxiliary arrays
!
!> @details Based on https://arxiv.org/pdf/1410.1748.pdf
!
!> @param SH_A 
!> @param SH_B
!> @param z0 slice thickness in [nm]
!> @param TP 'Tayl' or 'Pade', to select method
!> @param nn number of row/column entries in A
!
!> @date 09/16/13 MDG 1.0 original, tested against analytical version for small array
!> @date 06/05/14 MDG 1.1 updated IO
!--------------------------------------------------------------------------
recursive subroutine SH_initialize(SHcoeff, maxL)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_initialize

use typedefs

IMPLICIT NONE

type(SH_Coefficients),INTENT(INOUT)   :: SHcoeff
integer(kind=irg), INTENT(IN)         :: maxL

integer(kind=irg)                     :: l, m, ls, lm1s, ms

if (allocated(SHcoeff%Alm).eqv..TRUE.) deallocate(SHcoeff%Alm)
if (allocated(SHcoeff%Blm).eqv..TRUE.) deallocate(SHcoeff%Blm)
if (allocated(SHcoeff%Plm).eqv..TRUE.) deallocate(SHcoeff%Plm)
if (allocated(SHcoeff%Ylm).eqv..TRUE.) deallocate(SHcoeff%Ylm)
 
allocate(SHcoeff%Alm(0:SH_PT(maxL,maxL)+1))
allocate(SHcoeff%Blm(0:SH_PT(maxL,maxL)+1))
allocate(SHcoeff%Plm(0:SH_PT(maxL,maxL)+1))
allocate(SHcoeff%Ylm(0:SH_YR(maxL,maxL)+1))

SHcoeff%Alm = 0.D0
SHcoeff%Blm = 0.D0
SHcoeff%Plm = 0.D0
SHcoeff%Ylm = 0.D0

do l=2,maxL
  ls = l*l
  lm1s = (l-1)*(l-1)
  do m=0,l-2
    ms = m*m
    SHcoeff%Alm(SH_PT(l,m)) = sqrt((4.D0*dble(ls)-1.D0)/dble(ls-ms))
    SHcoeff%Blm(SH_PT(l,m)) = -sqrt((dble(lm1s-ms))/dble(4*lm1s-1))
  end do
end do

SHcoeff%maxL = maxL

! set these to random large numbers
SHcoeff%lastTheta = 213424324.D0
SHcoeff%lastPhi = 54325154325.D0


end subroutine SH_initialize

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_ComputeP
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute a set of Plm for a given value of x
!
!> @details Based on https://arxiv.org/pdf/1410.1748.pdf
!
!> @param SH_coeff
!> @param x
!
!> @date 01/12/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_ComputeP(SHcoeff, x)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_ComputeP

use typedefs

IMPLICIT NONE

type(SH_Coefficients),INTENT(INOUT)   :: SHcoeff
real(kind=dbl), INTENT(IN)            :: x

real(kind=dbl), parameter             :: sqrt3 = 1.7320508075688772935D0, sqrt3div2 = -1.2247448713915890491D0
real(kind=dbl)                        :: temp, sintheta
integer(kind=irg)                     :: L, ll, m, PTlm

if (x.ne.SHcoeff%lastTheta) then 
    SHcoeff%lastTheta = x

    L = SHcoeff%maxL
    temp = 0.39894228040143267794D0
    sintheta = sqrt(1.D0-x*x)

    SHcoeff%Plm(SH_PT(0,0)) = temp

    if (L.gt.0) then 
      SHcoeff%Plm(SH_PT(1,0)) = x * sqrt3 * temp
      temp = sqrt3div2*sintheta*temp
      SHcoeff%Plm(SH_PT(1,1)) = temp

      do ll=2,L
        do m=0,l-2
          PTlm = SH_PT(ll,m)
          SHcoeff%Plm(PTlm) = SHcoeff%Alm(PTlm) * ( x * SHcoeff%Plm(SH_PT(ll-1,m)) &
                                                    + SHcoeff%Blm(PTlm) * SHcoeff%Plm(SH_PT(ll-2,m)) )
        end do
        SHcoeff%Plm(SH_PT(ll,ll-1)) = x * sqrt(dble(2*(ll-1)+3)) * temp
        temp = -sqrt(1.D0+0.5D0/dble(ll)) * sintheta * temp
        SHcoeff%Plm(SH_PT(ll,ll)) = temp
      end do
    end if 
end if

end subroutine SH_ComputeP

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_ComputeY
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  precompute a set of Ylm for a given value of x
!
!> @details Based on https://arxiv.org/pdf/1410.1748.pdf
!> verified against Mathematica computation
!
!> @param SH_coeff
!> @param theta
!> @param phi
!
!> @date 01/12/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_ComputeY(SHcoeff, theta, phi)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_ComputeY

use typedefs

IMPLICIT NONE

type(SH_Coefficients),INTENT(INOUT)   :: SHcoeff
real(kind=dbl), INTENT(IN)            :: theta
real(kind=dbl), INTENT(IN)            :: phi

real(kind=dbl)                        :: c1, c2, s1, s2, tc, s, c, tt, ctheta
integer(kind=irg)                     :: L, ll, m 

! this routine assumes that phi is the fastest changing variable !!!


if (phi.ne.SHcoeff%lastPhi) then  ! has phi changed ?  if so, then recompute the arrays
    ctheta = cos(theta)
    if (ctheta.ne.SHcoeff%lastTheta) then  ! has theta changed ? 
        call SH_ComputeP(SHcoeff, ctheta)
    end if

    L = SHcoeff%maxL

    m = 0
    do ll=0,L
      SHcoeff%Ylm(SH_YR(ll,m)) = SHcoeff%Plm(SH_PT(ll,m)) * sqrt(0.5D0)
    end do 

    c1 = 1.D0
    c2 = cos(phi)
    s1 = 0.D0
    s2 = -sin(phi)
    tc = 2.D0 * c2
    do m=1,L
      s = tc * s1 - s2
      c = tc * c1 - c2
      s2 = s1
      s1 = s
      c2 = c1
      c1 = c
      do ll=m,L 
        tt = SHcoeff%Plm(SH_PT(ll,m))
        SHcoeff%Ylm(SH_YR(ll,-m)) = tt * s
        SHcoeff%Ylm(SH_YR(ll,m)) = tt * c
      end do
    end do
    SHcoeff%lastPhi = phi
end if

end subroutine SH_ComputeY

!--------------------------------------------------------------------------
!
! FUNCTION: SH_ComputeYlm
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute a set of Ylm for a given value of l, m, theta, phi
!
!> @details Based on https://arxiv.org/pdf/1410.1748.pdf
!
!> @param SH_coeff
!> @param l
!> @param m
!> @param theta
!> @param phi
!
!> @date 01/12/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function SH_ComputeYlm(SHcoeff, l, m, theta, phi) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_ComputeYlm

use typedefs

IMPLICIT NONE

type(SH_Coefficients),INTENT(INOUT)   :: SHcoeff
integer(kind=irg), INTENT(IN)         :: l
integer(kind=irg), INTENT(IN)         :: m
real(kind=dbl), INTENT(IN)            :: theta
real(kind=dbl), INTENT(IN)            :: phi
complex(kind=dbl)                     :: res

real(kind=dbl),parameter              :: s = 0.707106781186547524D0   ! sqrt(1/2)

call SH_ComputeY(SHcoeff, theta, phi)

if (m.lt.0) then
    res = cmplx(SHcoeff%Ylm(SH_YR(l,-m))*s,-SHcoeff%Ylm(SH_YR(l,m))*s)
    if (mod(m,2).eq.1) res = -res 
else if (m.eq.0) then
    res = cmplx(SHcoeff%Ylm(SH_YR(l,0)),0.D0)
else
    res = cmplx(SHcoeff%Ylm(SH_YR(l,m))*s,SHcoeff%Ylm(SH_YR(l,-m))*s)
end if

end function SH_ComputeYlm

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_buildHarmonic
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief build a set of Spherical Harmonics
!
!> @param SH_coeff
!> @param dim
!> @param modes
!> @param data
!
!> @date 01/13/19 MDG 1.0 original, based on Will Lenthe's classes
!--------------------------------------------------------------------------
recursive subroutine SH_buildHarmonic(SHcoeff, dim, modes, data) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_buildHarmonic

use typedefs
use Lambert

IMPLICIT NONE

type(SH_Coefficients),INTENT(INOUT)   :: SHcoeff
integer(kind=irg),INTENT(IN)          :: dim
type(SH_Mode),INTENT(IN)              :: modes(:)
real(kind=dbl),INTENT(INOUT)          :: data(2*dim*dim)

integer(kind=irg)					  :: i, j, l, m, sz, ierr
real(kind=dbl)						  :: xyz(3), XY(2), Phi, thetaN, thetaS
complex(kind=dbl)					  :: nh, sh 

! determine the maximum mode
SHcoeff%maxL = 0
sz = size(modes)
do m=1,sz 
  if (modes(m)%l.gt.SHcoeff%maxL) SHcoeff%maxL=modes(m)%l
end do

! initialize the spherical harmonic coefficient arrays
call SH_initialize(SHcoeff, SHcoeff%maxL)

! loop over the Lambert square
do j=0,dim-1
  XY(2) = dble(j)/dble(dim-1)
  do i=0,dim-1
    XY(1) = dble(i)/dble(dim-1)
    nh = cmplx(0.D0,0.D0)
    sh = cmplx(0.D0,0.D0)

! transform to a unit vector on the sphere
    xyz = LambertSquareToSphere(XY, ierr)
! we may need to convert to the Legendre grid at this point ...

! get the angles
    Phi = atan2(xyz(2),xyz(1))
    thetaN = acos(xyz(3))
    thetaS = acos(-xyz(3))

! compute signal in NH
    call SH_ComputeY(SHcoeff, thetaN, Phi)
    do m=1,sz
      nh = nh + SH_ComputeYlm(SHcoeff, modes(m)%l, modes(m)%m, thetaN, Phi) * modes(m)%weight
      if (modes(m)%m.ne.0) then
        if (mod(modes(m)%m,2).eq.0) then 
          nh = nh + SH_ComputeYlm(SHcoeff, modes(m)%l, -modes(m)%m, thetaN, Phi) * conjg(modes(m)%weight)
        else
          nh = nh - SH_ComputeYlm(SHcoeff, modes(m)%l, -modes(m)%m, thetaN, Phi) * conjg(modes(m)%weight)
        end if
      end if
    end do

! compute signal in SH
    call SH_ComputeY(SHcoeff, thetaS, Phi)
    do m=1,sz
      sh = sh + SH_ComputeYlm(SHcoeff, modes(m)%l, modes(m)%m, thetaN, Phi) * modes(m)%weight
      if (modes(m)%m.ne.0) then
        if (mod(modes(m)%m,2).eq.0) then 
          sh = sh + SH_ComputeYlm(SHcoeff, modes(m)%l, -modes(m)%m, thetaN, Phi) * conjg(modes(m)%weight)
        else
          sh = sh - SH_ComputeYlm(SHcoeff, modes(m)%l, -modes(m)%m, thetaN, Phi) * conjg(modes(m)%weight)
        end if
      end if
    end do

    data(j*dim+i) = real(nh)
    data(dim*dim + j*dim+i) = real(sh)

  end do 
end do

end subroutine SH_buildHarmonic

end module Ylm