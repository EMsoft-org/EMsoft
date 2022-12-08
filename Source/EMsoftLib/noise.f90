! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:noise.f90
!--------------------------------------------------------------------------
!
! MODULE: noise
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Poisson noise routine
!
!> @date 03/26/14 MDG 1.0 added to EMsoft from earlier EBSD version
!--------------------------------------------------------------------------

module noise

use local

IMPLICIT NONE

contains

recursive function alogam ( x, ifault )
!DEC$ ATTRIBUTES DLLEXPORT :: alogam
! this function can be used to compute the volume of a super ellipsoid. 

!*****************************************************************************80
!
!! ALOGAM computes the logarithm of the Gamma function.
!
!  Modified:
!
!    28 March 1999
!
!  Author:
!
!    Malcolm Pike, David Hill
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Malcolm Pike, David Hill,
!    Algorithm 291: 
!    Logarithm of Gamma Function,
!    Communications of the ACM,
!    Volume 9, Number 9, September 1966, page 684.
!
!  Parameters:
!
!    Input, real (kind=8) X, the argument of the Gamma function.
!    X should be greater than 0.
!
!    Output, integer (kind=4) IFAULT, error flag.
!    0, no error.
!    1, X <= 0.
!
!    Output, real (kind=8) ALOGAM, the logarithm of the Gamma 
!    function of X.
!
  implicit none

  real    (kind=8) alogam
  real    (kind=8) f
  integer (kind=4) ifault
  real    (kind=8) x
  real    (kind=8) y
  real    (kind=8) z

  if ( x <= 0.0D+00 ) then
    ifault = 1
    alogam = 0.0D+00
    return
  end if

  ifault = 0
  y = x

  if ( x < 7.0D+00 ) then

    f = 1.0D+00
    z = y

    do while ( z < 7.0D+00 )
      f = f * z
      z = z + 1.0D+00
    end do

    y = z
    f = - log ( f )

  else

    f = 0.0D+00

  end if

  z = 1.0D+00 / y / y
    
  alogam = f + ( y - 0.5D+00 ) * log ( y ) - y &
    + 0.918938533204673D+00 + &
    ((( &
    - 0.000595238095238D+00   * z &
    + 0.000793650793651D+00 ) * z &
    - 0.002777777777778D+00 ) * z &
    + 0.083333333333333D+00 ) / y

  return
end function alogam
!*****************************************************************************
real(kind=sgl) FUNCTION ran(idum)
!DEC$ ATTRIBUTES DLLEXPORT :: ran
!
! copied from Numerical Recipes
!
 IMPLICIT NONE
 INTEGER, PARAMETER :: K4B=selected_int_kind(9)
 INTEGER(K4B), INTENT(INOUT) :: idum
!f2py intent(in,out) ::  idum
! “Minimal” random number generator of Park and Miller combined with a Marsaglia shift sequence. 
! Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint values). 
! This fully portable, scalar generator has the “traditional” (not Fortran 90) calling 
! sequence with a random deviate as the returned function value: call with idum a negative 
! integer to initialize; thereafter, do not alter idum except to reinitialize. The period 
! of this generator is about 3.1 × 10^18.
 INTEGER(K4B), PARAMETER :: IA=16807,IM=2147483647,IQ=127773,IR=2836
 REAL, SAVE :: am
 INTEGER(K4B), SAVE :: ix=-1,iy=-1,k
 if (idum <= 0 .or. iy < 0) then
   am=nearest(1.0,-1.0)/IM
   iy=ior(ieor(888889999,abs(idum)),1)
   ix=ieor(777755555,abs(idum))
   idum=abs(idum)+1
 end if 
 ix=ieor(ix,ishft(ix,13))
 ix=ieor(ix,ishft(ix,-17))
 ix=ieor(ix,ishft(ix,5))
 k=iy/IQ
 iy=IA*(iy-k*IQ)-IR*k
 if (iy < 0) iy=iy+IM
 ran=am*ior(iand(IM,ieor(ix,iy)),1)
END FUNCTION ran
!*****************************************************************************
real(kind=sgl) FUNCTION POIDEV(XM,IDUM)
!DEC$ ATTRIBUTES DLLEXPORT :: POIDEV
! 
! this is essentially the poidev routine from Numerical Recipes,
! but converted to Fortran-90
!

use local

IMPLICIT NONE

real(kind=sgl),INTENT(IN) :: XM
real(kind=sgl),PARAMETER  :: PI=3.141592654
real(kind=sgl)            :: OLDM=-1.0, T, G, EM, SQ, ALXM, Y
integer(kind=irg)         :: ifault
INTEGER, PARAMETER :: K4B=selected_int_kind(9)
INTEGER(K4B), INTENT(INOUT) :: IDUM
!f2py intent(in,out) ::  IDUM


! start here:
if (XM.lt.20.) then    ! use 20 instead of original 12
 if (XM.ne.OLDM) then
   OLDM=XM
   G=exp(-XM)
 end if
 EM=-1.
 T=1.
 do 
   EM=EM+1.
   T=T*ran(IDUM)
   if (T.le.G) exit
 end do
else
 if (XM.ne.OLDM) THEN
   OLDM=XM
   SQ=sqrt(2.*XM)
   ALXM=alog(XM)
   G=XM*ALXM-alogam(dble(XM+1.),ifault)
 end if
 do
   innerdo: do
     Y=TAN(PI*ran(IDUM))
     EM=SQ*Y+XM
     if (EM.ge.0.) exit innerdo
   end do innerdo
   EM=INT(EM)
   T=0.9*(1.+Y**2)*EXP(EM*ALXM-alogam(dble(EM+1.),ifault)-G)
   if (ran(IDUM).le.T) exit
 end do
end if
POIDEV=EM
end function

end module noise
