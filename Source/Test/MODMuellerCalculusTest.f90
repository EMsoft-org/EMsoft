! ###################################################################
! Copyright (c) 2017-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:MODMuellerCalculusTest.f90
!--------------------------------------------------------------------------
!
! MODULE: MODMuellerCalculusTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief test of functions in the crystal module 
!
!> @date 10/29/16   MDG 1.0 original
!--------------------------------------------------------------------------

module MODMuellerCalculusTest


contains 

subroutine MODMuellerCalculusExecuteTest(res) &
           bind(c, name='MODMuellerCalculusExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: MODMuellerCalculusExecuteTest

use,INTRINSIC :: ISO_C_BINDING
use local
use constants
use typedefs
use MuellerCalculus

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)  :: res
real(kind=dbl),parameter        :: eps = 1.0D-10
type(StokesVectorType)          :: SV, SV_ref, SV_ref2, SV1, SV2, SV_ref3, SV_ref4
type(MuellerMatrixType)         :: MM, MM_ref, MMpol, MMret 
character(fnlen)                :: descriptor

real(kind=dbl)                  :: pol, pol_ref, psi, psi_ref, chi, chi_ref, alpha, alpha_ref, delta, delta_ref, diff, &
                                   alpha_ref2, delta_ref2, chi_ref2, psi_ref2

! list all the theoretical results here
pol_ref = dsqrt(0.4D0**2+0.3D0**2+0.2D0**2) / 0.8D0

alpha_ref = cPi * 0.2D0
delta_ref = cPi * 0.4D0
SV_ref%S = (/ 1.D0, cos(2.D0*alpha_ref), sin(2.D0*alpha_ref)*cos(delta_ref), sin(2.D0*alpha_ref)*sin(delta_ref) /)

chi_ref = cPi * 0.125D0
psi_ref = cPi * 0.4D0
SV_ref2%S = (/ 1.D0, cos(2.D0*chi_ref)*cos(2.D0*psi_ref), cos(2.D0*chi_ref)*sin(2.D0*psi_ref), sin(2.D0*psi_ref) /)

alpha_ref2 = 1.0899066227778444D0
delta_ref2 = -5.243778632045043D0

chi_ref2 = 0.39269908169872436D0
psi_ref2 = 2.827433388230814D0

SV_ref3%S = (/ 1.D0, 1.D0/5.D0,2.D0/5.D0, 3.D0/5D0 /)
SV_ref4%S = (/ 0.623205080756888D0, -0.311602540378444D0, 0.381633613241090D0, 0.381633613241090D0 /)

! initialize the return counter
res = 0 

!---------------------------------
! test polarization value
SV%descriptor = 'some random Stokes vector'
SV%S = (/ 0.8D0, 0.4D0, 0.3D0, 0.2D0 /)

pol = MC_get_Polarization(SV)
if (pol.ne.pol_ref) then
    res = 1
    write (*,"('polarization difference = ',D18.10)") pol_ref - pol
  return
end if

!---------------------------------
! test Stokes vector from auxiliary and phase shift angles
alpha = cPi * 0.2D0
delta = cPi * 0.4D0

descriptor = 'testvector'
SV = MC_get_Stokes_AD(alpha, delta, descriptor)

diff = sum( SV_ref%S(0:3)-SV%S(0:3) )
if (diff.gt.eps) then
    res = 2
    write (*,"(' Stokes vector(alpha, delta) difference = ',D18.10)") diff
  return
end if

!---------------------------------
! test Stokes vector from ellipticity and orientation
chi = cPi * 0.125D0
psi = cPi * 0.4D0

descriptor = 'testvector'
SV = MC_get_Stokes_EO(chi, psi, descriptor)

diff = sum( SV_ref2%S(0:3)-SV%S(0:3) )
if (diff.gt.eps) then
    res = 3
    write (*,"(' Stokes vector(psi, chi) difference = ',D18.10)") diff
  return
end if

!---------------------------------
! test auxiliary and phase shift angles from ellipticity and orientation
call MC_get_AD_from_EO(chi_ref, psi_ref, alpha, delta)

diff = abs(alpha - alpha_ref2) + abs(delta - delta_ref2)

if (diff.gt.eps) then
    res = 4
    write (*,"('  alpha/delta from chi/psi difference = ',D18.10)") diff
  return
end if

!---------------------------------
! test ellipticity and orientation angles from auxiliary and phase shift angles
call MC_get_EO_from_AD(alpha_ref2, delta_ref2, chi, psi)

diff = abs(chi - chi_ref2) + abs(psi - psi_ref2)

if (diff.gt.eps) then
    res = 5
    write (*,"('  chi/psi from alpha/delta difference = ',D18.10)") diff
  return
end if

!---------------------------------
! test phase shift angle from Stokes vector
SV2 = MC_get_Stokes_AD(alpha_ref2,delta_ref2,descriptor)
delta = MC_get_PhaseShiftAngle(SV2)

diff = delta - delta_ref2

if (diff.gt.eps) then
    res = 6
    write (*,"('  extracted phase shift angle difference = ',D18.10)") diff
!  return
end if

!---------------------------------
! test Stokes vectors derived from equivalent (chi/psi) and (alpha/delta) paramters
! descriptor = 'testvector'
! SV1 = MC_get_Stokes_EO(chi_ref2,psi_ref2,descriptor)
! SV2 = MC_get_Stokes_AD(alpha_ref2,delta_ref2,descriptor)

! diff = sum( abs(SV1%S(0:3) - SV2%S(0:3)) ) 

! write (*,*) SV1%S
! write (*,*) SV2%S

! if (diff.gt.eps) then
!     res = 7
!     write (*,"('  derived Stokes vector difference = ',D18.10)") diff
!   return
! end if

!---------------------------------
! test Stokes vector transformation through a rotated polarizer followed by a retarder
! [numbers verified by means of Mathematica script]
!
! get a standard horizontal polarization matrix
MMpol = MC_get_basicMuellerMatrix(1)
! get a retarder matrix for phi = pi/4
MMret = MC_get_retarder(cPi/4.D0)
! rotate the Mueller matrix by pi/3
MMpol = MC_rotate_MuellerMatrix(MMpol, cPi/3.D0) 
! concatenate the two matrices to complete the optical chain
MMpol = MC_concatenateMuellerMatrices(MMpol, MMret)
! and finally, propagate a Stokes vector through this optical path
descriptor = 'final Stokes vector'
SV = MC_propagateStokesVector(MMpol, SV_ref3, descriptor)

diff = sum(abs(SV%S-SV_ref4%S))

if (diff.gt.eps) then
    res = 8
    write (*,"('  transformed Stokes vector difference = ',D18.10)") diff
  return
end if




end subroutine MODMuellerCalculusExecuteTest

end module MODMuellerCalculusTest
