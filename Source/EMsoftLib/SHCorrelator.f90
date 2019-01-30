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

contains






end module SHcorrelator
