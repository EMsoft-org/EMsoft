! ###################################################################
! Copyright (c) 2013-2017, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:MuellerCalculus.f90
!--------------------------------------------------------------------------
!
! PROGRAM: MuellerCalculus
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines to generate and handle Mueller matrices and Stokes vectors for polarized light microscopy
!
!> @details Most of the routines in this module are based on the book by Collett:
!> Polarized Light: Fundamentals and Applications, E. Collett, 1993 (M. Decker, Inc)
!
!> @date 02/12/17 MDG 1.0 initial version
!--------------------------------------------------------------------------
module MuellerCalculus

use local
use typedefs
use error

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_basicMuellerMatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns a basic 4x4 Mueller matrix by type
!
!> @param MMtype integer describing the optical element (0 prduces list)
!
!> @date   02/14/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_basicMuellerMatrix(MMtype) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_basicMuellerMatrix

use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: MMtype
type(MuellerMatrixType)         :: res

select case (MMtype)
    case (0)
        call Message('The following basic Mueller matrix types are available:')
        call Message('1: linear horizontal polarizer (along x)')
        call Message('2: linear vertical polarizer (along y)')
        call Message('3: linear polarizer at +45°')
        call Message('4: linear polarizer at -45°')
        call Message('5: quarter-wave plate, fast axis vertical')
        call Message('6: quarter-wave plate, fast axis horizontal')
        call Message('7: circular polarizer, right-handed')
        call Message('8: circular polarizer, left-handed')
        call Message('9: ideal mirror')
    case (1)
        res%descriptor = 'linear horizontal polarizer'
        res%M(1,1:4) = (/ 1.D0, 1.D0, 0.D0, 0.D0 /)
        res%M(2,1:4) = (/ 1.D0, 1.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M = 0.5D0 * res%M
    case (2)
        res%descriptor = 'linear vertical polarizer'
        res%M(1,1:4) = (/ 1.D0,-1.D0, 0.D0, 0.D0 /)
        res%M(2,1:4) = (/-1.D0, 1.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M = 0.5D0 * res%M
    case (3)
        res%descriptor = 'linear polarizer at +45°'
        res%M(1,1:4) = (/ 1.D0, 0.D0, 1.D0, 0.D0 /)
        res%M(2,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 1.D0, 0.D0, 1.D0, 0.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M = 0.5D0 * res%M
    case (4)
        res%descriptor = 'linear polarizer at -45°'
        res%M(1,1:4) = (/ 1.D0, 0.D0,-1.D0, 0.D0 /)
        res%M(2,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/-1.D0, 0.D0, 1.D0, 0.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M = 0.5D0 * res%M
    case (5)
        res%descriptor = 'quarter-wave plate, fast axis vertical'
        res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(2,1:4) = (/ 0.D0, 1.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0, 0.D0,-1.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0, 1.D0, 0.D0 /)
    case (6)
        res%descriptor = 'quarter-wave plate, fast axis horizontal'
        res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(2,1:4) = (/ 0.D0, 1.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0, 0.D0, 1.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0,-1.D0, 0.D0 /)
    case (7)
        res%descriptor = 'circular polarizer, right-handed'
        res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0, 1.D0 /)
        res%M(2,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(4,1:4) = (/ 1.D0, 0.D0, 0.D0, 1.D0 /)
        res%M = 0.5D0 * res%M
    case (8)
        res%descriptor = 'circular polarizer, left-handed'
        res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0,-1.D0 /)
        res%M(2,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(4,1:4) = (/-1.D0, 0.D0, 0.D0, 1.D0 /)
        res%M = 0.5D0 * res%M
    case (9)
        res%descriptor = 'ideal mirror'
        res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
        res%M(2,1:4) = (/ 0.D0, 1.D0, 0.D0, 0.D0 /)
        res%M(3,1:4) = (/ 0.D0, 0.D0,-1.D0, 0.D0 /)
        res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0,-1.D0 /)
        !res%M = 0.5D0 * res%M
    case default
end select

end function MC_get_basicMuellerMatrix

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_diattenuator
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns a 4x4 Mueller matrix for a diattenuator (polarizer)
!
!> @param px amplitude attenuation coefficient along x or magnitude of vector p
!> @param py amplitude attenuation coefficient along y or polar angle of vector p
!> @param polar (OPTIONAL) absent/FALSE: cartesian components; TRUE: polar components of px + i py = (p, alpha)
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_diattenuator(px, py, polar) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_diattenuator

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: px
real(kind=dbl),INTENT(IN)       :: py
logical,OPTIONAL,INTENT(IN)     :: polar

type(MuellerMatrixType)         :: res
logical                         :: usepolar


! initialize a Mueller matrix for a diattenuator
res%descriptor = 'diattenuator'

usepolar = .FALSE.
if (present(polar)) then
    if (polar.eqv..TRUE.) usepolar = .TRUE.
end if 

if (usepolar) then
    if ((px.lt.0.D0).or.(px.gt.1.D0)) then
        call FatalError('MC_get_diattenuator','attenuation magnitude must lie in range [0,1]')
    end if 
    res%M(1,1:4) = (/ 1.D0, cos(2.D0*py), 0.D0, 0.D0 /)
    res%M(2,1:4) = (/ cos(2.D0*py), 1.D0, 0.D0, 0.D0 /)
    res%M(3,1:4) = (/ 0.D0, 0.D0, sin(2.D0*py), 0.D0 /)
    res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, sin(2.D0*py) /)
    res%M = 0.5D0*px*px*res%M
else
    if ((minval((/ px, py /)).lt.0.D0).or.(maxval((/px, py/)).gt.1.D0)) then
        call FatalError('MC_get_diattenuator','attenuation factors must lie in range [0,1]')
    end if 
    res%M(1,1:4) = (/ px*px+py*py, px*px-py*py, 0.D0, 0.D0 /)
    res%M(2,1:4) = (/ px*px-py*py, px*px+py*py, 0.D0, 0.D0 /)
    res%M(3,1:4) = (/ 0.D0, 0.D0, 2.D0*px*py, 0.D0 /)
    res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, 2.D0*px*py /)
    res%M = 0.5D0*res%M
end if 
    
end function MC_get_diattenuator


!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_rotator
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns a 4x4 Mueller matrix for a rotator
!
!> @param theta rotator angle (radians)
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_rotator(theta) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_rotator

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: theta
type(MuellerMatrixType)         :: res

real(kind=dbl)                  :: ct, st

ct = cos(2.D0*theta)
st = sin(2.D0*theta)

! initialize a Mueller matrix for a rotator
res%descriptor = 'rotator'
res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
res%M(2,1:4) = (/ 0.D0, ct, st, 0.D0 /)
res%M(3,1:4) = (/ 0.D0, -st, ct, 0.D0 /)
res%M(4,1:4) = (/ 0.D0, 0.D0, 0.D0, 1.D0 /)

end function MC_get_rotator

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_retarder
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns a 4x4 Mueller matrix for a retarder
!
!> @param phi retardation angle (radians)
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_retarder(phi) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_retarder

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: phi
type(MuellerMatrixType)         :: res

real(kind=dbl)                  :: cp, sp

cp = cos(phi)
sp = sin(phi)

! initialize a Mueller matrix for a retarder
res%descriptor = 'retarder'
res%M(1,1:4) = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
res%M(2,1:4) = (/ 0.D0, 1.D0, 0.D0, 0.D0 /)
res%M(3,1:4) = (/ 0.D0, 0.D0, cp, -sp /)
res%M(4,1:4) = (/ 0.D0, 0.D0, sp, cp /)

end function MC_get_retarder

!--------------------------------------------------------------------------
!
! FUNCTION: MC_rotate_MuellerMatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @param MM input Mueller matrix
!> @param theta rotation angle (radians)
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_rotate_MuellerMatrix(MM, theta) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_rotate_MuellerMatrix

IMPLICIT NONE

type(MuellerMatrixType),INTENT(IN)  :: MM 
real(kind=dbl),INTENT(IN)           :: theta
type(MuellerMatrixType)             :: res

type(MuellerMatrixType)             :: Mrot 

! initialize the output Mueller matrix descriptor
res%descriptor = trim(MM%descriptor)//'-rotated'

Mrot = MC_get_rotator(theta)

res%M = matmul(transpose(Mrot%M), matmul(MM%M, Mrot%M))

end function MC_rotate_MuellerMatrix

!--------------------------------------------------------------------------
!
! SUBROUTINE: MC_print_MuellerMatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @param MM input Mueller matrix
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine MC_print_MuellerMatrix(MM)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_print_MuellerMatrix

use io 

IMPLICIT NONE

type(MuellerMatrixType),INTENT(IN)  :: MM

real(kind=dbl)                      :: io_double(4)
integer(kind=irg)                   :: i

call Message('Mueller Matrix Type : '//trim(MM%descriptor))

do i=1,4 
    io_double(1:4) = MM%M(i,1:4)
    call WriteValue('  --> ',io_double,4)
end do

end subroutine MC_print_MuellerMatrix


!--------------------------------------------------------------------------
!
! FUNCTION: MC_propagateStokesVector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief multiplies a Stokes vector by a Mueller matrix 
!
!> @param MM Mueller matrix structure
!> @param SV Stokes vector structure
!> @param descriptor string to describe the state of the Stokes vector
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_propagateStokesVector(MM, SV, descriptor) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_propagateStokesVector

IMPLICIT NONE

type(MuellerMatrixType),INTENT(IN)          :: MM
type(StokesVectorType),INTENT(IN)           :: SV
character(fnlen),INTENT(IN)                 :: descriptor
type(StokesVectorType)                      :: res

res%S = matmul(MM%M, SV%S)
res%descriptor = trim(descriptor)

end function MC_propagateStokesVector


!--------------------------------------------------------------------------
!
! FUNCTION: MC_concatenateMuellerMatrices
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief multiplies a Mueller matrix M1 by M2, in the order  M2 x M1
!
!> @param MM1 Mueller matrix structure (earlier in the optical path)
!> @param MM2 Mueller matrix structure (later in the optical path)
!
!> @date   02/14/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_concatenateMuellerMatrices(MM1, MM2) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_concatenateMuellerMatrices

IMPLICIT NONE

type(MuellerMatrixType),INTENT(IN)          :: MM1
type(MuellerMatrixType),INTENT(IN)          :: MM2
type(MuellerMatrixType)                     :: res

res%M = matmul(MM2%M, MM1%M)
res%descriptor = trim(MM1%descriptor)//'->'//trim(MM2%descriptor)

end function MC_concatenateMuellerMatrices


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! conversion routines between Stokes vector components and other ellipsometry parameters
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_EllipticityAngle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extracts the ellipticity angle from a Stokes vector
!
!> @param SV Stokes vector structure
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_EllipticityAngle(SV) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_EllipticityAngle

use constants
use io

IMPLICIT NONE

type(StokesVectorType),INTENT(IN)           :: SV
real(kind=dbl)                              :: res

real(kind=dbl)                              :: p4, io_double(1)

p4 = cPi * 0.25D0

res = 0.5D0 * asin(SV%S(3)/SV%S(0))

if (abs(res).gt.p4) then
    io_double(1) = res
    call WriteValue('Ellipticity angle = ',io_double,1)
    call FatalError('MC_get_EllipticityAngle','Ellipticity angle does not lie in range [-pi/4,pi/4]')
end if

end function MC_get_EllipticityAngle

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_OrientationAngle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extracts the polarization ellipse orientation angle from a Stokes vector
!
!> @param SV Stokes vector structure
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_OrientationAngle(SV) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_OrientationAngle

use constants

IMPLICIT NONE

type(StokesVectorType),INTENT(IN)           :: SV
real(kind=dbl)                              :: res

res = 0.5D0 * atan2(SV%S(2),SV%S(1))

res = mod(res+2.D0*cPi,cPi)

end function MC_get_OrientationAngle

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_AuxiliaryAngle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extracts the auxiliary angle from a Stokes vector
!
!> @param SV Stokes vector structure
!
!> @date   02/13/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_AuxiliaryAngle(SV) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_AuxiliaryAngle

use constants
use io

IMPLICIT NONE

type(StokesVectorType),INTENT(IN)           :: SV
real(kind=dbl)                              :: res

real(kind=dbl)                              :: psi, chi, alpha, delta

chi = MC_get_EllipticityAngle(SV)
psi = MC_get_OrientationAngle(SV)
call MC_get_AD_from_EO(chi, psi, alpha, delta)

res = alpha

end function MC_get_AuxiliaryAngle

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_PhaseShiftAngle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extracts the phase shift angle from a Stokes vector
!
!> @param SV Stokes vector structure
!
!> @date   02/13/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_PhaseShiftAngle(SV) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_PhaseShiftAngle

use constants
use io

IMPLICIT NONE

type(StokesVectorType),INTENT(IN)           :: SV
real(kind=dbl)                              :: res

real(kind=dbl)                              :: psi, chi, alpha, delta

chi = MC_get_EllipticityAngle(SV)
psi = MC_get_OrientationAngle(SV)
call MC_get_AD_from_EO(chi, psi, alpha, delta)

res = delta

end function MC_get_PhaseShiftAngle




!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_Polarization
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extracts the polarization from a Stokes vector
!
!> @param SV Stokes vector structure
!
!> @date   02/12/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_Polarization(SV) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_Polarization

use io

IMPLICIT NONE

type(StokesVectorType),INTENT(IN)           :: SV
real(kind=dbl)                              :: res

if (SV%S(0).eq.0.D0) then
    call FatalError('MC_get_Polarization','Total intensity in Stokes Vector is zero')
end if

res = dsqrt(sum(SV%S(1:3)**2)) / SV%S(0)

end function MC_get_Polarization

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_Stokes_EO
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate a Stokes vector for a given Ellipticity and Orientation angle
!
!> @param chi ellipticity angle (radians)
!> @param psi orientation angle (radians)
!
!> @date   02/13/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_Stokes_EO(chi, psi, descriptor) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_Stokes_EO

IMPLICIT NONE

real(kind=dbl),INTENT(IN)                   :: chi
real(kind=dbl),INTENT(IN)                   :: psi
character(fnlen),INTENT(IN)                 :: descriptor
type(StokesVectorType)                      :: res

real(kind=dbl)                              :: cp, sp, cc, sc

cp = cos(2.D0*psi)
sp = sin(2.D0*psi)
cc = cos(2.D0*chi)
sc = sin(2.D0*chi)

res%descriptor = trim(descriptor)
res%S = (/ 1.D0, cc*cp, cc*sp, sc /)

end function MC_get_Stokes_EO

!--------------------------------------------------------------------------
!
! FUNCTION: MC_get_Stokes_AD
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate a Stokes vector for a given auxiliary and phase shift angle
!
!> @param alpha auxiliary angle (radians)
!> @param delta phase shift angle (radians)
!
!> @date   02/13/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_get_Stokes_AD(alpha, delta, descriptor) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_Stokes_AD

IMPLICIT NONE

real(kind=dbl),INTENT(IN)                   :: alpha
real(kind=dbl),INTENT(IN)                   :: delta 
character(fnlen),INTENT(IN)                 :: descriptor
type(StokesVectorType)                      :: res

real(kind=dbl)                              :: ca, sa, cd, sd

ca = cos(2.D0*alpha)
sa = sin(2.D0*alpha)
cd = cos(delta)
sd = sin(delta)

res%descriptor = trim(descriptor)
res%S = (/ 1.D0, ca, sa*cd, sa*sd /)

end function MC_get_Stokes_AD


!--------------------------------------------------------------------------
!
! SUBROUTINE: MC_get_AD_from_EO
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert auxiliary and phase shift angle to ellipticity and orientation angles
!
!> @details determined using a Mathematica script
!
!> @param chi ellipticity angle (radians)
!> @param psi orientation angle (radians)
!> @param alpha auxiliary angle (radians)
!> @param delta phase shift angle (radians)
!
!> @date   02/13/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine MC_get_AD_from_EO(chi, psi, alpha, delta)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_AD_from_EO

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)                   :: chi
real(kind=dbl),INTENT(IN)                   :: psi
real(kind=dbl),INTENT(OUT)                  :: alpha
real(kind=dbl),INTENT(OUT)                  :: delta 

real(kind=dbl)                              :: sc, tp, cp, cc, p2, p4, st, tt, ss, ct, sa

p2 = cPi*0.5D0
p4 = cPi*0.25D0

cc = cos(2.D0*chi)
sc = sin(2.D0*chi)
tp = tan(2.D0*psi)
cp = cos(2.D0*psi)

st = dsqrt(sc*sc+tp*tp)
tt = dsqrt(1.D0+tp*tp)
ss = dsqrt(1.D0-sc*sc)

ct = cos(2.D0*chi) * tan(2.D0*psi)
sa = sc/abs(cp)

! get alpha
if (abs(psi-p2).ge.p4) then
  alpha = 0.5D0 * atan2(st/tt,ss/tt)
else
  alpha = 0.5D0 * (cPi - atan2(st/tt,ss/tt))
end if

! get delta, such that there is only one cut in the delta surface for chi=0, psi<pi/2
if (abs(psi-p2).lt.p4) then
    delta = atan2(-sa/st,ct/st)-cPi
else
    delta = atan2(sa/st,ct/st)
    if (chi.gt.0.D0) delta = delta - 2.0D0*cPi
end if

end subroutine MC_get_AD_from_EO


!--------------------------------------------------------------------------
!
! SUBROUTINE: MC_get_EO_from_AD
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert ellipticity and orientation angles to auxiliary and phase shift angles
!
!> @details determined using a Mathematica script
!
!> @param alpha auxiliary angle (radians)
!> @param delta phase shift angle (radians)
!> @param chi ellipticity angle (radians)
!> @param psi orientation angle (radians)
!
!> @date   02/13/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine MC_get_EO_from_AD(alpha, delta, chi, psi)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_get_EO_from_AD

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)                   :: alpha
real(kind=dbl),INTENT(IN)                   :: delta 
real(kind=dbl),INTENT(OUT)                  :: chi
real(kind=dbl),INTENT(OUT)                  :: psi

real(kind=dbl)                              :: p2, p4

p2 = cPi * 0.5D0
p4 = cPi * 0.25D0

chi = 0.5D0 * asin ( sin(2.D0 * alpha) * sin(delta))

if (delta.le.p2) then
    psi = 0.5D0 * atan(cos(delta) * tan(2.D0 * alpha))
else
    psi = cPi - 0.5D0 * atan(cos(delta) * tan(2.D0 * alpha))
end if

! make sure chi falls in the range [-pi/4,pi/4]
if (abs(chi).gt.p4) then
    call FatalError('MC_get_EO_from_AD','ellipticity angle must be in interval [-pi/4,pi/4]')
end if

! make sure psi falls in the range [0,pi]
if (psi.lt.0.D0) psi = psi + cPi

end subroutine MC_get_EO_from_AD

!--------------------------------------------------------------------------
!
! function: MC_getUniaxialReflectivities
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the reflectivities [rss,rsp,rps,rpp] for uniaxial symmetry
!
!> @details determined using a Mathematica script, based on Lekner book
!
!> @param wl light wave length lambda [m]
!> @param epsac principal dielectric constants in standard cartesian reference frame
!> @param nincident refractive index of the incident medium
!> @param dc direction cosines of uniaxial c-axis in reflection reference frame 
!> @param beamtilt tilt angle theta1 [degrees] of parallel illumination w.r.t. to reflection z-axis
!
!> @date   08/28/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_getUniaxialReflectivities(wl, epsac, nincident, dc, beamtilt) result(rvals)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_getUniaxialReflectivities

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)           :: wl
complex(kind=dbl),INTENT(IN)        :: epsac(2)
real(kind=dbl),INTENT(IN)           :: nincident
real(kind=dbl),INTENT(IN)           :: dc(3)
real(kind=dbl),INTENT(IN)           :: beamtilt
complex(kind=dbl)                   :: rvals(4)

real(kind=dbl)                      :: k, theta1, ct, st, tt
complex(kind=dbl)                   :: eps0, Deps, eps1, epsgam, no, n1, ko, k1, KK, q1, qt, qo, qroot, qe, q
complex(kind=dbl)                   :: A, B, Ap, Bp, factor, cdc(3), ke, ro, re, nne, ngam

rvals = cmplx(0.D0,0.D0)

! get the incident light wave number [m^-1]
k = 2.D0 * cPi / wl 

! turn direction cosines into complex numbers
cdc(1) = cmplx(dc(1),0.D0)
cdc(2) = cmplx(dc(2),0.D0)
cdc(3) = cmplx(dc(3),0.D0)

! dielectric parameters and refractive indices (complex valued !)
Deps = epsac(2)-epsac(1)
epsgam = epsac(1) + cdc(3)**2 * Deps
n1 = cmplx(nincident,0.D0)
nne = sqrt(epsac(2))
no = sqrt(epsac(1))
ngam = sqrt(epsgam)

if (beamtilt.eq.0.D0) then   ! we'll use the simplified expressions for the reflection coefficients
! source:  J. Lekner, "Normal-incidence reflection and tramsission by uniaxial crystals and crystal plates"
! J. Phys.: Condens. Matter 4 (1992) 1387-1398
    ro = (n1 - no)/(n1 + no)
    re = (n1*ngam - nne*no)/(n1*ngam + nne*no)
    A = sqrt(cdc(1)**2 + cdc(2)**2)
    cdc(1:2) = cdc(1:2)/A
    rvals(1) = ro * cdc(1)**2 + re * cdc(2)**2
    rvals(2) = (re - ro) * cdc(1) * cdc(2)
    rvals(3) = rvals(2)
    rvals(4) = ro * cdc(2)**2 + re * cdc(1)**2
else   ! if there is a beam tilt, then we need to employ the full expressions
! source: J. Lekner, "Reflection and refraction by uniaxial crystals"
! J. Phys.: Condens. Matter 3 (1991) 6121-6133

! beam tilt angle
    theta1 = beamtilt * cPi / 180.D0
    ct = cos(theta1)
    st = sin(theta1)
    tt = tan(theta1)

! various wave numbers and wave vector components 
    ko = cmplx(k,0.D0) * no
    k1 = cmplx(k,0.D0) * nincident
    KK = cmplx(st,0.D0) * k
    q1 = cmplx(ct,0.D0) * k1
    qt = q1 + KK * cmplx(tt,0.D0)
    qo = sqrt(-KK**2+ko**2)
    qroot = sqrt( (epsac(1)/epsgam**2) * (k**2 * epsac(2) * epsgam - KK**2*(epsac(2) - cdc(2)**2 * Deps)) )
    qe = - KK * cdc(1) * cdc(3) * Deps / epsgam + qroot

! reflection constants
    A  = (qo*cdc(1)-KK*cdc(3))*(cdc(1)*(qe*ko**2+qo**2*qt)-KK*cdc(3)*(ko**2+qe*qt))
    Ap = (qo*cdc(1)-KK*cdc(3))*(cdc(1)*(qe*ko**2-qo**2*qt)-KK*cdc(3)*(ko**2-qe*qt))
    B  = (ko*cdc(2))**2 * (ko**2+qo*qt)
    Bp = (ko*cdc(2))**2 * (ko**2-qo*qt)

! and finally the four reflection parameters rss, rsp, rps, rpp
    factor = A*(q1+qo)+B*(q1+qe)

    rvals(1) = (A*(q1-qo)+B*(q1-qe))
    rvals(2) = 2.0*cdc(2)*(qo-qe)*(qo*cdc(1)+KK*cdc(3))*k1*ko**2
    rvals(3) = 2.0*cdc(2)*(qo-qe)*(qo*cdc(1)-KK*cdc(3))*k1*ko**2
    rvals(4) = - (Ap*(q1+qo)+Bp*(q1+qe))
    rvals = rvals/factor
end if 

end function MC_getUniaxialReflectivities

!--------------------------------------------------------------------------
!
! function: MC_getSampleMuellerMatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the sample Mueller matrix based on [rss,rsp,rps,rpp] for uniaxial symmetry
!
!> @details determined using a Mathematica script, based on Lekner book
!
!> @param rvals four reflectivity coefficients
!
!> @date   09/06/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function MC_getSampleMuellerMatrix(rvals) result(MM)
!DEC$ ATTRIBUTES DLLEXPORT :: MC_getSampleMuellerMatrix

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)           :: rvals(4)
real(kind=dbl)                         :: MM(4,4)

complex(kind=dbl)                      :: rpp2, rsp2, rps2, rss2, &
                                          rssrsp, rssrps, rssrpp, rsprss, rsprps, rsprpp, &
                                          rpsrss, rpsrsp, rpsrpp, rpprss, rpprsp, rpprps

! this is a straighforward application of the definitions in the 2012 paper
! by Letnes et al.  
!
! Calculation of the Mueller matrix for scattering of light from two-dimensional rough surfaces
! PA Letnes, AA Maradudin, T Nordam, I Simonsen
! Physical Review A 86 (3), 031803, 2012


! first get all the constants
rss2 = rvals(1) * conjg(rvals(1))
rsp2 = rvals(2) * conjg(rvals(2))
rps2 = rvals(3) * conjg(rvals(3))
rpp2 = rvals(4) * conjg(rvals(4))

MM(1,1) = real(rpp2 + rsp2 + rps2 + rss2)
MM(1,2) = real(rpp2 + rsp2 - rps2 - rss2)
MM(2,1) = real(rpp2 - rsp2 + rps2 - rss2)
MM(2,2) = real(rpp2 - rsp2 - rps2 + rss2)

rssrsp = rvals(1) * conjg(rvals(2))
rssrps = rvals(1) * conjg(rvals(3))
rssrpp = rvals(1) * conjg(rvals(4))

rsprss = rvals(2) * conjg(rvals(1))
rsprps = rvals(2) * conjg(rvals(3))
rsprpp = rvals(2) * conjg(rvals(4))

rpsrss = rvals(3) * conjg(rvals(1))
rpsrsp = rvals(3) * conjg(rvals(2))
rpsrpp = rvals(3) * conjg(rvals(4))

rpprss = rvals(4) * conjg(rvals(1))
rpprsp = rvals(4) * conjg(rvals(2))
rpprps = rvals(4) * conjg(rvals(3))

MM(1,3) = real(rpprps + rsprss + rpsrpp + rssrsp)
MM(1,4) = real(cmplx(0.D0,1.D0) * (rpprps + rsprss - rpsrpp - rssrsp))
MM(2,3) = real(rpprps - rsprss + rpsrpp - rssrsp)
MM(2,4) = real(cmplx(0.D0,1.D0) * (rpprps - rsprss - rpsrpp + rssrsp))

MM(3,1) = real(rpprsp + rsprpp + rpsrss + rssrps)
MM(3,2) = real(rpprsp + rsprpp - rpsrss - rssrps)
MM(4,1) = real(cmplx(0.D0,-1.D0) * (rpprsp - rsprpp + rpsrss - rssrps))
MM(4,2) = real(cmplx(0.D0,-1.D0) * (rpprsp - rsprpp - rpsrss + rssrps))

MM(3,3) = real(rpprss + rsprps + rpsrsp + rssrpp)
MM(3,4) = real(cmplx(0.D0,1.D0) * (rpprss + rsprps - rpsrsp - rssrpp))
MM(4,3) = real(cmplx(0.D0,-1.D0) * (rpprss - rsprps + rpsrsp - rssrpp))
MM(4,4) = real(rpprss - rsprps - rpsrsp + rssrpp)

end function MC_getSampleMuellerMatrix





end module MuellerCalculus