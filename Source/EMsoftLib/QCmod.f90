! ###################################################################
! Copyright (c) 2017-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:QCmod.f90
!--------------------------------------------------------------------------
!
! MODULE: QCmod
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Routines to handle 2-D quasi-crystals (QC) geometry and scattering
!
!> @details Eventually, this module will be able to generate all the data structures
!> needed to perform dynamical simulations for QCs... in particular EBSD and ECP...
!
!> @date 06/25/18 SS 1.0 original
!--------------------------------------------------------------------------
module QCmod

use local
use typedefs
use constants

public

interface QC_setMetricParameters
	module procedure QC_setMetricParameters2DQC
	module procedure QC_setMetricParameters3DQC
end interface QC_setMetricParameters

interface QC_getvectorLength
	module procedure QC_getvectorLength2DQC
	module procedure QC_getvectorLength3DQC
end interface QC_getvectorLength

interface QC_getGvector
	module procedure QC_getGvector2DQC
	module procedure QC_getGvector3DQC
end interface QC_getGvector

interface QC_TransSpace
	module procedure QC_TransSpace2DQC
	module procedure QC_TransSpace3DQC
end interface QC_TransSpace
contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:QC_setMetricParameters2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Generate the parallel and normal dodecagonal basis vectors and other parameters
!
!> @param QCcell TDQCStructureType variable
!> @param QClatparm lattice parameter in nm
!
!> @date 03/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine QC_setMetricParameters2DQC(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_setMetricParameters2DQC 

use error
use math

IMPLICIT NONE

type(TDQCStructureType),pointer   :: QCcell

real(kind=dbl)                    :: QClatparm_a, QClatparm_c
character(fnlen)                  :: QCtype
real(kind=dbl)                    :: tmp(5,3), c(4), s(4), c2(4), s2(4), c3(4), s3(4),&
                                     c4(4), s4(4), A, B, CC
integer(kind=irg)                 :: i, j

! set the QCtype
QCtype = trim(QCcell%QCtype)

! and set the hyper-lattice parameter
QClatparm_a = QCcell%QClatparm_a
QClatparm_c = QCcell%QClatparm_c

if(trim(QCtype) .eq. 'DoD') then

  do i = 1,4

    c3(i) = dcos(2.D0*cPi*dble(i-1)/12.D0)
    s3(i) = dsin(2.D0*cPi*dble(i-1)/12.D0)

    c4(i) = -dcos(10.D0*cPi*dble(i-1)/12.D0)
    s4(i) = -dsin(10.D0*cPi*dble(i-1)/12.D0)

  end do

  do i = 0,1

    c(i+1)  = dcos(2.D0*cPi*dble(i-1)/12.D0)
    s(i+1)  = dsin(2.D0*cPi*dble(i-1)/12.D0)

    c2(i+1) = dcos(10.D0*cPi*dble(i-1)/12.D0)
    s2(i+1) = dsin(10.D0*cPi*dble(i-1)/12.D0)    

  end do

  do i = 2,3

    c(i+1)  = dcos(2.D0*cPi*dble(i+1)/12.D0)
    s(i+1)  = dsin(2.D0*cPi*dble(i+1)/12.D0)

    c2(i+1) = dcos(10.D0*cPi*dble(i+1)/12.D0)
    s2(i+1) = dsin(10.D0*cPi*dble(i+1)/12.D0)

  end do

  QCcell%scaling(1,1:5)    = (/1.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
  QCcell%scaling(2,1:5)    = (/1.D0, 1.D0, 1.D0, 0.D0, 0.D0/)
  QCcell%scaling(3,1:5)    = (/0.D0, 1.D0, 1.D0, 1.D0, 0.D0/)
  QCcell%scaling(4,1:5)    = (/-1.D0, 0.D0, 1.D0, 1.D0, 0.D0/)
  QCcell%scaling(5,1:5)    = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  QCcell%dsm(1,1:5) = (/c(1), s(1), c2(1), s2(1), 0.D0/)
  QCcell%dsm(2,1:5) = (/c(2), s(2), c2(2), s2(2), 0.D0/)
  QCcell%dsm(3,1:5) = (/c(3), s(3), c2(3), s2(3), 0.D0/)
  QCcell%dsm(4,1:5) = (/c(4), s(4), c2(4), s2(4), 0.D0/)
  QCcell%dsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, sqrt(3.D0)*QCcell%QClatparm_c/(sqrt(2.D0)*QCcell%QClatparm_a)/) ! 

  QCcell%dsm(1:5,1:5) = (sqrt(2.D0)*QCcell%QClatparm_a/sqrt(3.D0)) * QCcell%dsm(1:5,1:5)

  QCcell%rsm(1,1:5) = (/c3(1), s3(1), c4(1), s4(1), 0.D0/)
  QCcell%rsm(2,1:5) = (/c3(2), s3(2), c4(2), s4(2), 0.D0/)
  QCcell%rsm(3,1:5) = (/c3(3), s3(3), c4(3), s4(3), 0.D0/)
  QCcell%rsm(4,1:5) = (/c3(4), s3(4), c4(4), s4(4), 0.D0/)
  QCcell%rsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, sqrt(2.D0)*QCcell%QClatparm_a/QCcell%QClatparm_c/) 

  QCcell%rsm(1:5,1:5) = (1.D0/QCcell%QClatparm_a/dsqrt(2.D0)) * QCcell%rsm(1:5,1:5) 

  QCcell%dsm = transpose(QCcell%dsm)
  QCcell%rsm = transpose(QCcell%rsm)

  A   = (sqrt(2.D0) * QCcell%QClatparm_a / dsqrt(3.D0)) ** 2
  B   = QCcell%QClatparm_c ** 2
  CC  = A * dcos(QCcell%alphaij * cPi / 180.D0)

  QCcell%dmt(1,1:5) = (/A, 0.D0, CC, 0.D0, 0.D0/)
  QCcell%dmt(2,1:5) = (/0.D0, A, 0.D0, CC, 0.D0/)
  QCcell%dmt(3,1:5) = (/CC, 0.D0, A, 0.D0, 0.D0/)
  QCcell%dmt(4,1:5) = (/0.D0, CC, 0.D0, A, 0.D0/)
  QCcell%dmt(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, B/)

  QCcell%vol  = sqrt(CalcDeterminant(QCcell%dmt, 5, 5))

  A   = 2.D0 * (1.D0 / QCcell%QClatparm_a) ** 2
  B   = (1.D0 / QCcell%QClatparm_c) ** 2
  CC  = A * dcos(QCcell%alphastarij * cPi / 180.D0)

  QCcell%rmt(1,1:5) = (/A, 0.D0, CC, 0.D0, 0.D0/)
  QCcell%rmt(2,1:5) = (/0.D0, A, 0.D0, CC, 0.D0/)
  QCcell%rmt(3,1:5) = (/CC, 0.D0, A, 0.D0, 0.D0/)
  QCcell%rmt(4,1:5) = (/0.D0, CC, 0.D0, A, 0.D0/)
  QCcell%rmt(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, B/)

else if(trim(QCtype) .eq. 'Dec') then

  do i = 1,4
    c(i)  = dcos(2.D0*cPi*dble(i)/5.D0) 
    s(i)  = dsin(2.D0*cPi*dble(i)/5.D0) 

    c2(i) = dcos(6.D0*cPi*dble(i)/5.D0)
    s2(i) = dsin(6.D0*cPi*dble(i)/5.D0)

  end do

  QCcell%scaling(1,1:5)    = (/0.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
  QCcell%scaling(2,1:5)    = (/0.D0, 1.D0, 1.D0, -1.D0, 0.D0/)
  QCcell%scaling(3,1:5)    = (/-1.D0, 1.D0, 1.D0, 0.D0, 0.D0/)
  QCcell%scaling(4,1:5)    = (/-1.D0, 0.D0, 1.D0, -1.D0, 0.D0/)
  QCcell%scaling(5,1:5)    = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  ! QCcell%dsm(1,1:5) = (/c(1)-1.D0, s(1), c(2)-1.D0, s(2), 0.D0/)
  ! QCcell%dsm(2,1:5) = (/c(2)-1.D0, s(2), c(4)-1.D0, s(4), 0.D0/)
  ! QCcell%dsm(3,1:5) = (/c(3)-1.D0, s(3), c(1)-1.D0, s(1), 0.D0/)
  ! QCcell%dsm(4,1:5) = (/c(4)-1.D0, s(4), c(3)-1.D0, s(3), 0.D0/)
  ! QCcell%dsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, sqrt(5.D0)* QClatparm_c/(2.D0*QClatparm_a)/) ! 

  ! QCcell%dsm(1:5,1:5) = (2.D0*QClatparm_a/sqrt(5.D0)) * QCcell%dsm(1:5,1:5) 

  QCcell%dsm(1,1:5) = (/c(1)-1.D0, s(1), c2(1)-1.D0, s2(1), 0.D0/)
  QCcell%dsm(2,1:5) = (/c(2)-1.D0, s(2), c2(2)-1.D0, s2(2), 0.D0/)
  QCcell%dsm(3,1:5) = (/c(3)-1.D0, s(3), c2(3)-1.D0, s2(3), 0.D0/)
  QCcell%dsm(4,1:5) = (/c(4)-1.D0, s(4), c2(4)-1.D0, s2(4), 0.D0/)
  QCcell%dsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, 5.D0 * QClatparm_c/(2.D0*QClatparm_a)/) ! 

  QCcell%dsm(1:5,1:5) = (2.D0*QClatparm_a/5.D0) * QCcell%dsm(1:5,1:5) 

  ! QCcell%rsm(1,1:5) = (/c(1), s(1), c(2), s(2), 0.D0/)
  ! QCcell%rsm(2,1:5) = (/c(2), s(2), c(4), s(4), 0.D0/)
  ! QCcell%rsm(3,1:5) = (/c(3), s(3), c(1), s(1), 0.D0/)
  ! QCcell%rsm(4,1:5) = (/c(4), s(4), c(3), s(3), 0.D0/)
  ! QCcell%rsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, sqrt(5.D0)*QClatparm_a/QClatparm_c/) 

  ! QCcell%rsm(1:5,1:5) = (1.D0/sqrt(5.D0)/QClatparm_a) * QCcell%rsm(1:5,1:5) 

  QCcell%rsm(1,1:5) = (/c(1), s(1), c2(1), s2(1), 0.D0/)
  QCcell%rsm(2,1:5) = (/c(2), s(2), c2(2), s2(2), 0.D0/)
  QCcell%rsm(3,1:5) = (/c(3), s(3), c2(3), s2(3), 0.D0/)
  QCcell%rsm(4,1:5) = (/c(4), s(4), c2(4), s2(4), 0.D0/)
  QCcell%rsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, QClatparm_a/QClatparm_c/) 

  QCcell%rsm(1:5,1:5) = (1.D0/QClatparm_a) * QCcell%rsm(1:5,1:5) 

  QCcell%dsm = transpose(QCcell%dsm)
  QCcell%rsm = transpose(QCcell%rsm)

  A   = (2.D0 * QCcell%QClatparm_a / dsqrt(5.D0)) ** 2
  B   = QCcell%QClatparm_c ** 2
  CC  = A * dcos(QCcell%alphaij * cPi / 180.D0)

  QCcell%dmt(1,1:5) = (/A, CC, CC, CC, 0.D0/)
  QCcell%dmt(2,1:5) = (/CC, A, CC, CC, 0.D0/)
  QCcell%dmt(3,1:5) = (/CC, CC, A, CC, 0.D0/)
  QCcell%dmt(4,1:5) = (/CC, CC, CC, A, 0.D0/)
  QCcell%dmt(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, B/)

  QCcell%vol  = sqrt(CalcDeterminant(QCcell%dmt, 5, 5))

  A   = 2.D0 * (1.D0 / QCcell%QClatparm_a) ** 2
  B   = (1.D0 / QCcell%QClatparm_c) ** 2
  CC  = A * dcos(QCcell%alphastarij * cPi / 180.D0)

  QCcell%rmt(1,1:5) = (/A, CC, CC, CC, 0.D0/)
  QCcell%rmt(2,1:5) = (/CC, A, CC, CC, 0.D0/)
  QCcell%rmt(3,1:5) = (/CC, CC, A, CC, 0.D0/)
  QCcell%rmt(4,1:5) = (/CC, CC, CC, A, 0.D0/)
  QCcell%rmt(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, B/)

else if(trim(QCtype) .eq. 'Oct') then

  do i = 1,4
    c(i)  = dcos(2.D0*cPi*dble(i)/8.D0)
    s(i)  = dsin(2.D0*cPi*dble(i)/8.D0)

    c2(i) = dcos(6.D0*cPi*dble(i)/8.D0)
    s2(i) = dsin(6.D0*cPi*dble(i)/8.D0)

    c3(i) = dcos(2.D0*cPi*dble(i-1)/8.D0)
    s3(i) = dsin(2.D0*cPi*dble(i-1)/8.D0)

    c4(i) = dcos(6.D0*cPi*dble(i-1)/8.D0)
    s4(i) = dsin(6.D0*cPi*dble(i-1)/8.D0)
  end do

  QCcell%scaling(1,1:5)    = (/1.D0, 1.D0, 0.D0, -1.D0, 0.D0/)
  QCcell%scaling(2,1:5)    = (/1.D0, 1.D0, 1.D0, 0.D0, 0.D0/)
  QCcell%scaling(3,1:5)    = (/0.D0, 1.D0, 1.D0, 1.D0, 0.D0/)
  QCcell%scaling(4,1:5)    = (/-1.D0, 0.D0, 1.D0, 1.D0, 0.D0/)
  QCcell%scaling(5,1:5)    = (/0.D0, 0.D0, 0.D0, 0.D0, 1.D0/)

  QCcell%dsm(1,1:5) = (/c(1), s(1), c2(1), s2(1), 0.D0/)
  QCcell%dsm(2,1:5) = (/c(2), s(2), c2(2), s2(2), 0.D0/)
  QCcell%dsm(3,1:5) = (/c(3), s(3), c2(3), s2(3), 0.D0/)
  QCcell%dsm(4,1:5) = (/c(4), s(4), c2(4), s2(4), 0.D0/)
  QCcell%dsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, dsqrt(2.D0)*QCcell%QClatparm_c/QCcell%QClatparm_a/) ! 

  QCcell%dsm(1:5,1:5) = (QCcell%QClatparm_a / dsqrt(2.D0))*QCcell%dsm(1:5,1:5) 

  QCcell%rsm(1,1:5) = (/c(1), s(1), c2(1), s2(1), 0.D0/)
  QCcell%rsm(2,1:5) = (/c(2), s(2), c2(2), s2(2), 0.D0/)
  QCcell%rsm(3,1:5) = (/c(3), s(3), c2(3), s2(3), 0.D0/)
  QCcell%rsm(4,1:5) = (/c(4), s(4), c2(4), s2(4), 0.D0/)
  QCcell%rsm(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, dsqrt(2.D0)*QCcell%QClatparm_a/QCcell%QClatparm_c/) 

  QCcell%rsm(1:5,1:5) = (1.D0 / QCcell%QClatparm_a / dsqrt(2.D0)) * QCcell%rsm(1:5,1:5) 

  QCcell%dsm = transpose(QCcell%dsm)
  QCcell%rsm = transpose(QCcell%rsm)

  A   = (QCcell%QClatparm_a / dsqrt(2.D0)) ** 2
  B   = QCcell%QClatparm_c ** 2

  QCcell%dmt(1,1:5) = (/A, 0.D0, 0.D0, 0.D0, 0.D0/)
  QCcell%dmt(2,1:5) = (/0.D0, A, 0.D0, 0.D0, 0.D0/)
  QCcell%dmt(3,1:5) = (/0.D0, 0.D0, A, 0.D0, 0.D0/)
  QCcell%dmt(4,1:5) = (/0.D0, 0.D0, 0.D0, A, 0.D0/)
  QCcell%dmt(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, B/)

  QCcell%vol  = sqrt(CalcDeterminant(QCcell%dmt, 5, 5))

  A   = 2.D0 * (1.D0 / QCcell%QClatparm_a) ** 2
  B   = (1.D0 / QCcell%QClatparm_c) ** 2

  QCcell%rmt(1,1:5) = (/A, 0.D0, 0.D0, 0.D0, 0.D0/)
  QCcell%rmt(2,1:5) = (/0.D0, A, 0.D0, 0.D0, 0.D0/)
  QCcell%rmt(3,1:5) = (/0.D0, 0.D0, A, 0.D0, 0.D0/)
  QCcell%rmt(4,1:5) = (/0.D0, 0.D0, 0.D0, A, 0.D0/)
  QCcell%rmt(5,1:5) = (/0.D0, 0.D0, 0.D0, 0.D0, B/)

else
  call FatalError('QC_setMetricParameters:','unknown symmetry type.')

end if

end subroutine QC_setMetricParameters2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE:QC_setMetricParameters3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate the parallel and normal icosahedral basis vectors and other parameters
!
!> @param QCcell QCStructureType variable
!> @param QClatparm lattice parameter in nm
!
!> @date 03/15/17 MDG 1.0 original
!> @date 06/25/18 SS  1.1 moved to this module
!> @date 06/26/18 SS  2.0 changed basis vectors and format same as 2DQC
!--------------------------------------------------------------------------
recursive subroutine QC_setMetricParameters3DQC(QCcell)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_setMetricParameters3DQC 

use error
use math

IMPLICIT NONE

type(QCStructureType),pointer   :: QCcell

real(kind=dbl)			        :: QClatparm
real(kind=dbl)					:: s(5), c(5), c2(5), s2(5), A
real(kind=dbl),parameter        :: ct = 1.D0/dsqrt(5.D0),&
                                   st = dsqrt(1.D0 - ct * ct)

integer(kind=irg)               :: i, j

QClatparm = QCcell%QClatparm

do i = 2,6
	c(i-1) 	= dcos(2.D0*cPi*dble(i)/5.D0)
	s(i-1) 	= dsin(2.D0*cPi*dble(i)/5.D0)

	c2(i-1) = dcos(4.D0*cPi*dble(i)/5.D0)
	s2(i-1) = dsin(4.D0*cPi*dble(i)/5.D0)
end do	

QCcell%scaling(1,1:6) = (/1.D0, 1.D0, 1.D0, 1.D0, 1.D0, 1.D0/)
QCcell%scaling(2,1:6) = (/1.D0, 1.D0, 1.D0, -1.D0, -1.D0, 1.D0/)
QCcell%scaling(3,1:6) = (/1.D0, 1.D0, 1.D0, 1.D0, -1.D0, -1.D0/)
QCcell%scaling(4,1:6) = (/1.D0, -1.D0, 1.D0, 1.D0, 1.D0, -1.D0/)
QCcell%scaling(5,1:6) = (/1.D0, -1.D0, -1.D0, 1.D0, 1.D0, 1.D0/)
QCcell%scaling(6,1:6) = (/1.D0, 1.D0, -1.D0, -1.D0, 1.D0, 1.D0/)

QCcell%scaling 		  = 0.5D0 * QCcell%scaling

QCcell%dsm(1,1:6) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 1.D0/)
QCcell%dsm(2,1:6) = (/c(1)*st, s(1)*st, ct, c2(1)*st, s2(1)*st, -ct/)
QCcell%dsm(3,1:6) = (/c(2)*st, s(2)*st, ct, c2(2)*st, s2(2)*st, -ct/)
QCcell%dsm(4,1:6) = (/c(3)*st, s(3)*st, ct, c2(3)*st, s2(3)*st, -ct/)
QCcell%dsm(5,1:6) = (/c(4)*st, s(4)*st, ct, c2(4)*st, s2(4)*st, -ct/)
QCcell%dsm(6,1:6) = (/c(5)*st, s(5)*st, ct, c2(5)*st, s2(5)*st, -ct/)

QCcell%dsm = QCcell%dsm * QClatparm

QCcell%rsm(1,1:6) = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 1.D0/)
QCcell%rsm(2,1:6) = (/c(1)*st, s(1)*st, ct, c2(1)*st, s2(1)*st, -ct/)
QCcell%rsm(3,1:6) = (/c(2)*st, s(2)*st, ct, c2(2)*st, s2(2)*st, -ct/)
QCcell%rsm(4,1:6) = (/c(3)*st, s(3)*st, ct, c2(3)*st, s2(3)*st, -ct/)
QCcell%rsm(5,1:6) = (/c(4)*st, s(4)*st, ct, c2(4)*st, s2(4)*st, -ct/)
QCcell%rsm(6,1:6) = (/c(5)*st, s(5)*st, ct, c2(5)*st, s2(5)*st, -ct/)

QCcell%rsm = ( QCcell%rsm / (QClatparm * 2.D0) ) 

QCcell%dsm = transpose(QCcell%dsm)
QCcell%rsm = transpose(QCcell%rsm)

A   = QClatparm ** 2

QCcell%dmt(1,1:6) = (/A, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
QCcell%dmt(2,1:6) = (/0.D0, A, 0.D0, 0.D0, 0.D0, 0.D0/)
QCcell%dmt(3,1:6) = (/0.D0, 0.D0, A, 0.D0, 0.D0, 0.D0/)
QCcell%dmt(4,1:6) = (/0.D0, 0.D0, 0.D0, A, 0.D0, 0.D0/)
QCcell%dmt(5,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, A, 0.D0/)
QCcell%dmt(6,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0, A/)

QCcell%vol  = sqrt(CalcDeterminant(QCcell%dmt, 6, 6))

A   = (1.D0 / QClatparm) ** 2

QCcell%rmt(1,1:6) = (/A, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/)
QCcell%rmt(2,1:6) = (/0.D0, A, 0.D0, 0.D0, 0.D0, 0.D0/)
QCcell%rmt(3,1:6) = (/0.D0, 0.D0, A, 0.D0, 0.D0, 0.D0/)
QCcell%rmt(4,1:6) = (/0.D0, 0.D0, 0.D0, A, 0.D0, 0.D0/)
QCcell%rmt(5,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, A, 0.D0/)
QCcell%rmt(6,1:6) = (/0.D0, 0.D0, 0.D0, 0.D0, 0.D0, A/)

end subroutine QC_setMetricParameters3DQC

!--------------------------------------------------------------------------
!
! FUNCTION:QC_TransSpace2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief convert between different vector spaces 'd', 'r', 'c'
!
!> @param QCcell    2-D QCStructureType variable
!> @param t         in vector
!> @param d         out vector
!> @param inspace   input space
!> @param outspace  output space
!
!> @date 06/13/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine QC_TransSpace2DQC(cell, t, d, inspace, outspace)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_TransSpace2DQC

IMPLICIT NONE

type(TDQCStructureType),pointer   :: cell
real(kind=dbl),INTENT(IN)         :: t(5)
real(kind=dbl),INTENT(OUT)        :: d(5)
character(1),INTENT(IN)           :: inspace
character(1),INTENT(IN)           :: outspace

! intercept the case where inspace and outspace are the same 
if (inspace.eq.outspace) then
  d = t
  return
end if

if (inspace.eq.'d') then
! direct to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%dsm,t)
   return
  end if
! direct to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dmt)
   return
  end if
 end if

 if (inspace.eq.'r') then
! reciprocal to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%rsm,t)
   return
  end if
! reciprocal to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(t,cell%rmt)
   return
  end if
 end if

 if (inspace.eq.'c') then
! Cartesian to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(cell%rsm,t)
   return
  end if
! Cartesian to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dsm)
   return
  end if
 end if

end subroutine QC_TransSpace2DQC

!--------------------------------------------------------------------------
!
! FUNCTION:QC_TransSpace3DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief convert between different vector spaces 'd', 'r', 'c'
!
!> @param QCcell    3-D QCStructureType variable
!> @param t         in vector
!> @param d         out vector
!> @param inspace   input space
!> @param outspace  output space
!
!> @date 06/26/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine QC_TransSpace3DQC(cell, t, d, inspace, outspace)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_TransSpace3DQC

IMPLICIT NONE

type(QCStructureType),pointer     :: cell
real(kind=dbl),INTENT(IN)         :: t(6)
real(kind=dbl),INTENT(OUT)        :: d(6)
character(1),INTENT(IN)           :: inspace
character(1),INTENT(IN)           :: outspace

! intercept the case where inspace and outspace are the same 
if (inspace.eq.outspace) then
  d = t
  return
end if

if (inspace.eq.'d') then
! direct to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%dsm,t)
   return
  end if
! direct to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dmt)
   return
  end if
 end if

 if (inspace.eq.'r') then
! reciprocal to Cartesian (pre-multiplication)
  if (outspace.eq.'c') then
   d = matmul(cell%rsm,t)
   return
  end if
! reciprocal to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(t,cell%rmt)
   return
  end if
 end if

 if (inspace.eq.'c') then
! Cartesian to direct (post-multiplication)
  if (outspace.eq.'d') then
   d = matmul(cell%rsm,t)
   return
  end if
! Cartesian to reciprocal (post-multiplication)
  if (outspace.eq.'r') then
   d = matmul(t,cell%dsm)
   return
  end if
 end if

end subroutine QC_TransSpace3DQC

!--------------------------------------------------------------------------
!
! FUNCTION:QC_getGvector2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Generate the parallel/orthogonal reciprocal lattice vector in cartesian frame for a set of indices 
!
!> @param QCcell 2-D QCStructureType variable
!> @param QCindex set of QC Miller indices
!> @param OP Orthogonal or Parallel space?
!
!> @date 03/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function QC_getGvector2DQC(QCcell, QCindex, OP) result(gvector)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_getGvector2DQC 

use error

IMPLICIT NONE

type(TDQCStructureType),pointer        :: QCcell
real(kind=dbl),INTENT(IN)              :: QCindex(5)
character(1),INTENT(IN)                :: OP
real(kind=dbl)                         :: gvector(3), dvec(5)

!dvec = matmul(QCindex,QCcell%Mrecip)
call QC_TransSpace(QCcell,QCindex,dvec,'r','c')
if (OP .eq. 'O') then
  gvector = (/dvec(3), dvec(4), 0.D0/)
else if (OP .eq. 'P') then
  gvector  = (/dvec(1), dvec(2), dvec(5)/)
else
  call FatalError('QC_getGvector','Unknown OP parameter passed to function')
end if

end function QC_getGvector2DQC

!--------------------------------------------------------------------------
!
! FUNCTION:QC_getGvector3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate the parallel/orthogonal reciprocal lattice vector for a set of indices 
!
!> @param QCcell QCStructureType variable
!> @param QCindex set of QC Miller indices
!> @param OP Orthogonal or Parallel space?
!
!> @date 03/15/17 MDG 1.0 original
!> @date 06/25/18 SS  1.1 moved to this module
!--------------------------------------------------------------------------
recursive function QC_getGvector3DQC(QCcell, QCindex, OP) result(gvector)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_getGvector3DQC 

use error

IMPLICIT NONE

type(QCStructureType),pointer          :: QCcell
real(kind=dbl),INTENT(IN)              :: QCindex(6)
character(1),INTENT(IN)                :: OP
real(kind=dbl)   					   :: gvector(3), dvec(6)

call QC_TransSpace(QCcell,QCindex,dvec,'r','c')
if (OP .eq. 'O') then
  gvector = (/dvec(4), dvec(5), dvec(6)/)
else if (OP .eq. 'P') then
  gvector  = (/dvec(1), dvec(2), dvec(3)/)
else
  call FatalError('QC_getGvector','Unknown OP parameter passed to function')
end if

end function QC_getGvector3DQC

!--------------------------------------------------------------------------
!
! FUNCTION:QC_getvectorLength2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Get the length of a parallel/orthogonal reciprocal lattice vector for a set of indices 
!
!> @param QCcell 2-D QCStructureType variable
!> @param QCindex set of QC Miller indices
!> @param OP Orthogonal or Parallel space?
!
!> @date 03/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function QC_getvectorLength2DQC(QCcell, QCindex, OP, space) result(gl)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_getvectorLength2DQC

use error

IMPLICIT NONE

type(TDQCStructureType),pointer        :: QCcell
integer(kind=irg),INTENT(IN)           :: QCindex(5)
character(1),INTENT(IN)                :: OP
character(1),INTENT(IN)                :: space  ! 'r' or 'd'

real(kind=dbl)                         :: gl
real(kind=dbl)                         :: dvec(5), dvec2(5)

dvec = dble(QCindex)

if(space .eq. 'r') then
  !dvec2 = matmul(dvec,QCcell%Mrecip)
  call QC_TransSpace(QCcell,dvec,dvec2,'r','c')
  if (OP.eq.'O') then
    !gl = dsqrt(DOT_PRODUCT(dvec, matmul(QCcell%rmto, dvec)))
    gl = sqrt(dvec2(3)**2 + dvec2(4)**2)
  else if (OP .eq. 'P') then
    !gl = dsqrt(DOT_PRODUCT(dvec, matmul(QCcell%rmtp, dvec)))
    gl = sqrt(dvec2(1)**2 + dvec2(2)**2 + dvec2(5)**2)
  else
    call FatalError('QC_getvectorLength : ','Unknown OP parameter passed to function')
  end if

else if(space .eq. 'd') then
  !dvec2 = matmul(dvec,QCcell%Mdirect)
  call QC_TransSpace(QCcell,dvec,dvec2,'d','c')
  if (OP.eq.'O') then
    !gl = dsqrt(DOT_PRODUCT(dvec, matmul(QCcell%dmto, dvec)))
    gl = sqrt(dvec2(3)**2 + dvec2(4)**2)
  else if (OP .eq. 'P') then
    !gl = dsqrt(DOT_PRODUCT(dvec, matmul(QCcell%dmtp, dvec)))
    gl = sqrt(dvec2(1)**2 + dvec2(2)**2 + dvec2(5)**2)
  else
    call FatalError('QC_getvectorLength : ','Unknown OP parameter passed to function')
  end if

else

  call FatalError('QC_getvectorLength: ', 'Unkown space input.')

end if

end function QC_getvectorLength2DQC

!--------------------------------------------------------------------------
!
! FUNCTION:QC_getGvectorLength3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Get the length of a parallel/orthogonal reciprocal lattice vector for a set of indices 
!
!> @param QCcell QCStructureType variable
!> @param QCindex set of QC Miller indices
!> @param OP Orthogonal or Parallel space?
!
!> @date 03/15/17 MDG 1.0 original
!> @date 06/25/18 SS  1.1 moved to this module
!> @date 06/26/18 SS  1.2 added space as an input variable
!--------------------------------------------------------------------------
recursive function QC_getvectorLength3DQC(QCcell, QCindex, OP, space) result(gl)
!DEC$ ATTRIBUTES DLLEXPORT ::QC_getvectorLength3DQC 

use error

IMPLICIT NONE

type(QCStructureType),pointer          :: QCcell
integer(kind=irg),INTENT(IN)           :: QCindex(6)
character(1),INTENT(IN)                :: OP
character(1),INTENT(IN)                :: space  ! 'r' or 'd'

real(kind=dbl)   					   :: gl

real(kind=dbl)               		   :: dvec(6), dvec2(6)

dvec = dble(QCindex)

if(space .eq. 'r') then
  call QC_TransSpace(QCcell,dvec,dvec2,'r','c')
  if (OP.eq.'O') then
    gl = sqrt(dvec2(4)**2 + dvec2(5)**2 + dvec2(6)**2)
  else if (OP .eq. 'P') then
    gl = sqrt(dvec2(1)**2 + dvec2(2)**2 + dvec2(3)**2)
  else
    call FatalError('QC_getvectorLength : ','Unknown OP parameter passed to function')
  end if

else if(space .eq. 'd') then
  call QC_TransSpace(QCcell,dvec,dvec2,'d','c')
  if (OP.eq.'O') then
    gl = sqrt(dvec2(4)**2 + dvec2(5)**2 + dvec2(6)**2)
  else if (OP .eq. 'P') then
    gl = sqrt(dvec2(1)**2 + dvec2(2)**2 + dvec2(3)**2)
  else
    call FatalError('QC_getvectorLength : ','Unknown OP parameter passed to function')
  end if

else
  call FatalError('QC_getvectorLength: ', 'Unkown space input.')
end if

end function QC_getvectorLength3DQC

!--------------------------------------------------------------------------
!
! FUNCTION: ShapeTransformTriangle
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate shape transform of the a triangle, which
!> is the building block for any polygon occupation domain. this result is known
!> analytically. we will simply sum it up for the 24 triangles. the
!> limits of the expression was calculated using mathematica
!
!> @param ap  edge length in reciprocal space of the pyramid 
!>            
!> @param hkl indices of the reciprocal vector in perp-space
!
!> @date   03/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function ShapeTransformTriangle(QCcell, ap, g) result(stt)
!DEC$ ATTRIBUTES DLLEXPORT :: ShapeTransformTriangle

use error

IMPLICIT NONE

type(TDQCStructureType), pointer      :: QCcell
real(kind=dbl),INTENT(IN)             :: g(5)
real(kind=dbl),INTENT(IN)             :: ap
complex(kind=dbl)                     :: stt, imagj
real(kind=dbl)                        :: e1(5), e2(5), etmp(5), vlen, dvec1(2), dvec2(2), dvec3(2)
real(kind=dbl)                        :: a1, a2, a3, Ar, prod, t
complex(kind=dbl)                     :: t1, t2
integer(kind=irg)                     :: icase
real(kind=dbl),parameter              :: eps = 1.0D-12
real(kind=dbl),parameter              :: tau = (1.D0 + dsqrt(5.D0))/2.D0


imagj = complex(0.D0, 1.D0)

if(trim(QCcell%QCtype) .eq. 'DoD') then

  !e1    = (/0.D0, -1.D0, 0.D0, 1.D0, 0.D0/)
  e1 = (/1.D0, -1.D0, 0.D0, 1.D0, 0.D0/)
  call QC_TransSpace(QCcell, e1, etmp, 'd', 'c')
  dvec1 = (/etmp(3), etmp(4)/)

  !e2    = (/0.D0, 0.D0, 0.D0, 1.D0, 0.D0/)!/dsqrt(3.D0)
  e2    = (/0.D0, -1.D0, -1.D0, 1.D0, 0.D0/)
  call QC_TransSpace(QCcell, e2, etmp, 'd', 'c')
  dvec2 = (/etmp(3), etmp(4)/)

  Ar    = ap**2 * dsin(cPi/6.D0)

else if(trim(QCcell%QCtype) .eq. 'Dec') then

  !e1    = (/1.D0, 0.D0, 0.D0, 1.D0, 0.D0/)
  e1    = (/1.D0, 0.D0, 0.D0, 0.D0, 0.D0/) - (/1.D0, 1.D0, 1.D0, 1.D0, 0.D0/)/5.D0
  call QC_TransSpace(QCcell, e1, etmp, 'd', 'c')
  dvec1 = (/etmp(3), etmp(4)/)

  e2    = (/0.D0, 0.D0, 1.D0, 0.D0, 0.D0/) - (/1.D0, 1.D0, 1.D0, 1.D0, 0.D0/)/5.D0
  !e2    = (/0.D0, 1.D0, 0.D0, 1.D0, 0.D0/) - (/1.D0, 1.D0, 1.D0, 1.D0, 0.D0/)/5.D0
  call QC_TransSpace(QCcell, e2, etmp, 'd', 'c')
  dvec2 = (/etmp(3), etmp(4)/)
  
  Ar    = ap**2 * dsin(cPi/5.D0)

else if(trim(QCcell%QCtype) .eq. 'Oct') then

  e1    = (/1.D0, -1.D0, 0.D0, 1.D0, 0.D0/)/2.D0
  call QC_TransSpace(QCcell, e1, etmp, 'd', 'c')
  dvec1 = (/etmp(3), etmp(4)/)

  e2    = (/1.D0, -1.D0, -1.D0, 1.D0, 0.D0/)/2.D0
  call QC_TransSpace(QCcell, e2, etmp, 'd', 'c')
  dvec2 = (/etmp(3), etmp(4)/)

  Ar    = ap**2 * dsin(cPi/4.D0)

else
  call FatalError('ShapeTransformPolygon','unknown symmetry type.')

end if

call QC_TransSpace(QCcell, g, etmp, 'r', 'c')
dvec3 = (/etmp(3), etmp(4)/)

a1  = 2.D0 * cPi * DOT_PRODUCT(dvec1, dvec3)
a2  = 2.D0 * cPi * DOT_PRODUCT(dvec2, dvec3)

a3 = a1 - a2

icase = 0

! (a1 -> 0) or (a2 -> 0) or (a1 -> a2 ~-> 0) or (a1 -> a2 -> 0) 
if(abs(a1) .lt. eps .and. abs(a2) .gt. eps .and. abs(a3) .gt. eps) then
  icase = 1
else if(abs(a1) .gt. eps .and. abs(a2) .lt. eps .and. abs(a3) .gt. eps) then
  icase = 2
else if(abs(a1) .gt. eps .and. abs(a2) .gt. eps .and. abs(a3) .lt. eps) then
  icase = 3
else if(abs(a1) .lt. eps .and. abs(a2) .lt. eps) then
  icase = 4
end if

select case (icase)

case(1)
  t1    = 1 + imagj * a2 - exp(imagj * a2) 
  prod  = a2 * a2
  stt   = Ar * t1 / prod 

case(2)
  t1    = 1 + imagj * a1 - exp(imagj * a1) 
  prod  = a1 * a1
  stt   = Ar * t1 / prod

case(3)
  t1    = imagj * a2 * exp(imagj * a2) - exp(imagj * a2) + complex(1.D0, 0.D0)
  prod  = a2 * a2
  stt   = -Ar * t1 / prod

case(4)
  stt = complex(Ar/2, 0.D0)

case DEFAULT
  t1    = a2 * exp(imagj * a1)
  t2    = a1 * exp(imagj * a2)
  prod  = a1 * a2 * a3
  stt   = -Ar * (t1 - t2 + a3)/prod

end select

end function ShapeTransformTriangle

!--------------------------------------------------------------------------
!
! FUNCTION: ShapeTransformPolygonCa
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate shape transform of the polygon Ca (see Franz gahler, 
!> Crystallography of Dodecagonal Quasicrystals)
!> @param  QCcell 2-D QC cell type
!> @param  ar     edge length of the OD
!> @param  h      hkl index of the reciprocal lattice vector
!
!> @date   03/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function ShapeTransformPolygonCa(QCcell, hkl, asite) result(stp)
!DEC$ ATTRIBUTES DLLEXPORT :: ShapeTransformPolygonCa

use error

IMPLICIT NONE

type(TDQCStructureType), pointer      :: QCcell
integer(kind=irg),INTENT(IN)          :: hkl(5)
integer(kind=irg),INTENT(IN)          :: asite

real(kind=dbl)                        :: ar
real(kind=dbl),parameter              :: tau = (1.D0 + dsqrt(5.D0))/2.D0
complex(kind=dbl)                     :: stp
integer(kind=irg)                     :: Pmdims, ii
real(kind=dbl)                        :: A_Polygon, hkl2(5), mat(5,5), mat2(5,5)

stp = complex(0.D0,0.D0)

if(trim(QCcell%QCtype) .eq. 'DoD') then
  ar        = QCcell%ATOM_pos(asite, 10) * dsqrt(2.D0/3.D0) * (QCcell%QClatparm_a)
  Pmdims    = 24

else if(trim(QCcell%QCtype) .eq. 'Dec') then
  ar        = QCcell%ATOM_pos(asite, 10)  * QCcell%QClatparm_a !* 2.D0/ sqrt(5.D0)
  Pmdims    = 5

else if(trim(QCcell%QCtype) .eq. 'Oct') then
  ar        = QCcell%ATOM_pos(asite, 10) * QCcell%QClatparm_a/dsqrt(2.D0)
  Pmdims    = 16 

else
  call FatalError('ShapeTransformPolygonCa','unknown symmetry type.')

end if

do ii = 1,Pmdims
  hkl2 = matmul(QCcell%SYM_icos(:,:,ii),dble(hkl))
  stp  = stp + ShapeTransformTriangle(QCcell, ar, hkl2)
end do

!stp = stp / A_Polygon

end function ShapeTransformPolygonCa

!--------------------------------------------------------------------------
!
! FUNCTION: ShapeTransformPyramid
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate shape transform of the a triangular pyramid, which
!> is the building block for the Triacontahedron. this result is known
!> analytically. we will simply sum it up for the 120 pyramids. the
!> limits of the expression was calculated using mathematica
!
!> @param ap  edge length in reciprocal space of the pyramid 
!>            (same length as rhombus in Triacontahedron)
!> @param hkl indices of the reciprocal vector in perp-space
!
!> @date   02/01/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function ShapeTransformPyramid(QCcell, ap, g) result(stp)
!DEC$ ATTRIBUTES DLLEXPORT :: ShapeTransformPyramid

use error

IMPLICIT NONE

type(QCStructureType), pointer        :: QCcell
real(kind=dbl),INTENT(IN)             :: g(6)
real(kind=dbl),INTENT(IN)             :: ap
complex(kind=dbl)                     :: stp
real(kind=dbl)                        :: a1, a2, a3, a4, a5, a6, prod, Vr
complex(kind=dbl)                     :: t1, t2, t3, t4, imagj
real(kind=dbl)                        :: e1(6), e2(6), e3(6), etmp(6)              ! basis vectors
real(kind=dbl)                        :: e1_c(3), e2_c(3), e3_c(3), gperp(3), vlen
real(kind=dbl),parameter              :: eps = 1.0D-12
integer(kind=irg)                     :: icase

imagj = complex(0.D0, 1.D0)

! edges of tetrahedron in perp space
e1    = (/1.D0, -1.D0, -1.D0, -1.D0, -1.D0, -1.D0/) * 0.5D0 
call QC_TransSpace(QCcell, e1, etmp, 'd', 'c')
e1_c  = etmp(4:6) 

e2    =  (/1.D0, 1.D0, -1.D0, -1.D0, -1.D0, -1.D0/) * 0.5D0 
call QC_TransSpace(QCcell, e2, etmp, 'd', 'c')
e2_c  =  etmp(4:6) 

e3    =  (/1.D0, 0.D0, -1.D0, -1.D0, 0.D0, -1.D0/) * 0.5D0 
call QC_TransSpace(QCcell, e3, etmp, 'd', 'c')
e3_c  =  etmp(4:6) 

gperp =  QC_getGvector(QCcell, g, 'O')

a1    =  2.D0 * cPi * DOT_PRODUCT(e1_c, gperp)
a2    =  2.D0 * cPi * DOT_PRODUCT(e2_c, gperp)
a3    =  2.D0 * cPi * DOT_PRODUCT(e3_c, gperp)

a4    =  a2 - a3
a5    =  a3 - a1
a6    =  a1 - a2

! volume of tetrahedron
Vr    =  ap**3 * DOT_PRODUCT(e1_c,(/e2_c(2)*e3_c(3) - e2_c(3)*e3_c(2), e2_c(3)*e3_c(1) - e2_c(1)*e3_c(3), &
                          e2_c(1)*e3_c(2) - e2_c(2)*e3_c(1)/))

! define different cases with limits (icase variable)
! the limits were calculated in Mathematica

icase = 0

! case 1: a1 -> 0, a2 -> 0, a3 -> 0
if((abs(a1) .lt. eps) .and. (abs(a2) .lt. eps) .and. (abs(a3) .lt. eps)) then
  icase = 1
! case 2: a1 -> 0, a2 -> 0
else if((abs(a1) .lt. eps) .and. (abs(a2) .lt. eps) .and. (abs(a3) .gt. eps)) then
  icase = 2
! case3: a1 -> 0, a3 -> 0
else if((abs(a1) .lt. eps) .and. (abs(a3) .lt. eps) .and. (abs(a2) .gt. eps)) then
  icase = 3
! case 4: a2 -> 0, a3 -> 0
else if((abs(a2) .lt. eps) .and. (abs(a3) .lt. eps) .and. (abs(a1) .gt. eps)) then
  icase = 4
! case 5/6: a1 -> 0
else if((abs(a1) .lt. eps) .and. (abs(a2) .gt. eps) .and. (abs(a3) .gt. eps)) then

  if(abs(a4) .lt. eps) then
    icase = 5
  else
    icase = 6
  end if

! case 7/8: a2 -> 0
else if((abs(a2) .lt. eps) .and. (abs(a1) .gt. eps) .and. (abs(a3) .gt. eps)) then

  if(abs(a5) .lt. eps) then
    icase = 7
  else
    icase = 8
  end if

! case 9/10: a3 -> 0
else if((abs(a3) .lt. eps) .and. (abs(a2) .gt. eps) .and. (abs(a1) .gt. eps)) then

  if(abs(a6) .lt. eps) then
    icase = 9
  else
    icase = 10
  end if

! case 11/14: a3 -> 0
else if((abs(a1) .gt. eps) .and. (abs(a2) .gt. eps) .and. (abs(a3) .gt. eps)) then

  if((abs(a4) .lt. eps) .and. (abs(a5) .gt. eps) .and. (abs(a6) .gt. eps)) then
    icase = 11
  else if((abs(a5) .lt. eps) .and. (abs(a4) .gt. eps) .and. (abs(a6) .gt. eps)) then
    icase = 12
  else if((abs(a6) .lt. eps) .and. (abs(a4) .gt. eps) .and. (abs(a5) .gt. eps)) then
    icase = 13
  else if((abs(a4) .lt. eps) .and. (abs(a5) .lt. eps)) then
    icase = 14
  end if

end if

select case (icase)

case(1)
  stp = complex(Vr/6.D0, 0.D0)
  
case(2)
  t1    = 2.D0 * exp(imagj * a3)
  t2    = a3 * (2.D0 * imagj - a3)
  t3    = 2.D0 * a3**3.D0
  stp   = -imagj * Vr * (2.D0 - t1 + t2)/t3
  
case(3)
  t1    = 2.D0 * exp(imagj * a2)
  t2    = a2 * (2.D0 * imagj - a2)
  t3    = 2.D0 * a2**3.D0
  stp   = -imagj * Vr * (2.D0 - t1 + t2)/t3
  
case(4)
  t1    = 2.D0 * exp(imagj * a1)
  t2    = a1 * (2.D0 * imagj - a1)
  t3    = 2.D0 * a1**3.D0
  stp   = -imagj * Vr * (2.D0 - t1 + t2)/t3
  
case(5)
  t1    = (2.D0 - imagj * a3) * exp(imagj * a3)
  t2    = -2.D0 - imagj * a3
  t3    = a3**3
  stp   = -imagj * Vr * (t1 + t2)/t3
  
case(6)
  t1    = (-1.D0 + exp(imagj * a3) - imagj*a3) * a2**2
  t2    = (1.D0 - exp(imagj * a2)) * a3**2
  t3    = imagj * a2 * a3**2
  t4    = a4 * (a2 * a3)**2
  stp   = -imagj * Vr * (t1 + t2 + t3)/t4
  
case(7)
  t1    = (2.D0 - imagj * a1) * exp(imagj * a1)
  t2    = -2.D0 - imagj * a1
  t3    = a1**3
  stp   = -imagj * Vr * (t1 + t2)/t3
  
case(8)
  t1    = (-1.D0 + exp(imagj * a3) - imagj*a3) * a1**2
  t2    = (1.D0 - exp(imagj * a1)) * a3**2
  t3    = imagj * a1 * a3**2
  t4    = -a5 * (a1 * a3)**2
  stp   = -imagj * Vr * (t1 + t2 + t3)/t4
  
case(9)
  t1    = (2.D0 - imagj * a2) * exp(imagj * a2)
  t2    = -2.D0 - imagj * a2
  t3    = a2**3
  stp   = -imagj * Vr * (t1 + t2)/t3
  
case(10)
  t1    = (-1.D0 + exp(imagj * a2) - imagj*a2) * a1**2
  t2    = (1.D0 - exp(imagj * a1)) * a2**2
  t3    = imagj * a1 * a2**2
  t4    = a6 * (a1 * a2)**2
  stp   = -imagj * Vr * (t1 + t2 + t3)/t4
  
case(11)
  t1    = a3 * a1 * (-2.D0 + exp(imagj * a3) * (2.D0 - imagj * a3))
  t2    = (1.D0 - exp(imagj * a1)) * a3**2
  t3    = (1.D0 + imagj * exp(imagj * a3) * (imagj + a3)) * a1**2
  t4    = a1 * (a3 * a5)**2
  stp   = -imagj * Vr * (t1 + t2 + t3)/t4
  
case(12)
  t1    = a3 * a2 * (-2.D0 + exp(imagj * a3) * (2.D0 - imagj * a3))
  t2    = (1.D0 - exp(imagj * a2)) * a3**2
  t3    = (1.D0 + imagj * exp(imagj * a3) * (imagj + a3)) * a2**2
  t4    = a2 * (a3 * a4)**2
  stp   = -imagj * Vr * (t1 + t2 + t3)/t4

case(13)
  t1    = a3 * a2 * (-2.D0 + exp(imagj * a2) * (2.D0 + imagj * a3))
  t2    = (1.D0 - exp(imagj * a2)) * a3**2
  t3    = (1.D0 - imagj * exp(imagj * a2) * a3 - exp(imagj * a3) ) * a2**2
  t4    = a1 * (a3 * a5)**2
  stp   = -imagj * Vr * (t1 + t2 + t3)/t4

case(14) 
  t1    =  (-2.D0 + a2 * (2.D0 * imagj + a2)) * exp(imagj * a2)
  t2    =  2.D0 * a2**3
  stp   =  -imagj * Vr * (2.D0 + t1)/t2

case DEFAULT
  t1    =  a2 * a3 * a4 * exp(imagj * a1)
  t2    =  a1 * a3 * a5 * exp(imagj * a2)
  t3    =  a1 * a2 * a6 * exp(imagj * a3)
  t4    =  a4 * a5 * a6
  prod  =  a1 * a2 * a3 * a4 * a5 * a6
  stp   =  -imagj * Vr * (t1 + t2 + t3 + t4) / prod

end select

end function

!--------------------------------------------------------------------------
!
! FUNCTION: ShapeTransformTriacontahedron
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate shape transform of the Triacontahedron, which
!> is the atomic surface in 6D to get the 3D Amman Tiling
!
!> @param  dict dict structure containing symmetry operators
!> @param  ar   edge length of the rhombus in Triacontahedron ( = 1/2a for G)
!
!> @date   02/01/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive function ShapeTransformTriacontahedron(QCcell, hkl, asite) result(stt)
!DEC$ ATTRIBUTES DLLEXPORT :: ShapeTransformTriacontahedron


IMPLICIT NONE

type(QCStructureType), pointer        :: QCcell
integer(kind=irg),INTENT(IN)          :: hkl(6)
integer(kind=irg),INTENT(IN)		  :: asite
complex(kind=dbl)                     :: stt
integer(kind=irg)                     :: Pmdims, ii, jj, nn
real(kind=dbl)                        :: V_Triacontahedron, hkl2(6), ar


ar = QCcell%ATOM_pos(asite,10) * QCcell%QClatparm

stt = complex(0.D0,0.D0)

Pmdims = QCcell%SG%SYM_NUMpt

do ii = 1,Pmdims
  hkl2 = matmul(QCcell%SG%SYM_direc(ii,:,:),dble(hkl))
  stt  = stt + ShapeTransformPyramid(QCcell, ar, hkl2)
end do

end function ShapeTransformTriacontahedron

end module QCmod
