! ###################################################################
! Copyright (c) 2019-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:xrd.f90
!--------------------------------------------------------------------------
!
! MODULE: xrd
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic routines for XRD computations
!
!> @date 07/30/19  MDG 1.0 original version 
!--------------------------------------------------------------------------
module xrdmod

use local


contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: getXRDwavenumber
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief return the x-ray wave number for a given accelerating voltage
!
!> @param kV accelerating voltage of x-ray tube (in kV, obviously)
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getXRDwavenumber(kV) result(wavenumber)
!DEC$ ATTRIBUTES DLLEXPORT :: getXRDwavenumber

IMPLICIT NONE

real(kind=sgl),INTENT(IN)             :: kV 
real(kind=sgl)                        :: wavenumber

wavenumber = kV / 1.23984193  ! in nm 

end function getXRDwavenumber






end module xrdmod