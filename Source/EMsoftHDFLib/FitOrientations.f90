! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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

module FitOrientations

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:EMFitOrientationcalfunEBSD
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief This function is used by bobyqa to fit an EBSD pattern
!
!> @details The main purpose of this routine is to calculte the difference of 1 with the dot
!> product of an experimental pattern with the given set of detector parameters. This is used
!> by bobyqa module to fit an EBSD pattern.
!>
!> This routine will first compute the detector arrays rgx etc. if necessary, and then perform
!> the usual interpolation from the square Lambert projection. The pattern will be a basic pattern,
!> without any intensity scaling or binning etc; the calling program should take care of those 
!> operations.
!
!> @param ipar array with integer input parameters
!> @param initmeanval mean value of search space
!> @param EBSDpattern output array
!> @param quats quaternion input array
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 12/12/15 SS 1.0 original
!> @date 03/28/16 SS 1.1 omega is no longer a variable parameter
!> @date 03/18/18 SS 1.2 changed refinement from euler to homochoric space
!--------------------------------------------------------------------------

recursive subroutine EMFitOrientationcalfunEBSD(ipar, initmeanval, expt, accum, &
                                mLPNH, mLPSH, n, x, f, mask, prefactor, rgx, rgy, rgz,&
                                stepsize, gammaval, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EMFitOrientationcalfunEBSD

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:

! ipar(1)  = binning
! ipar(2)  = numsx
! ipar(3)  = numsy
! ipar(4)  = npx
! ipar(5)  = npy
! ipar(6)  = numEbins
! ipar(7)  = nE
! ipar(8)  = Emin
! ipar(9)  = Emax
! ipar(10) = nregions

! initmeanval(2) = phi1
! initmeanval(3) = phi
! initmeanval(4) = phi2

! stepsize(1) = step_phi1 
! stepsize(2) = step_phi 
! stepsize(3) = step_phi2 

! X = (/phi1, phi, phi2/) in DEGREES

            
use local
use rotations
use constants
use filters
use math, ONLY:Jaccard_Distance
use EBSDmod
use error

use,INTRINSIC :: ISO_C_BINDING
           
implicit none

integer(c_size_t),intent(in)            :: ipar(10)
real(sgl),intent(in)                    :: initmeanval(3)
real(sgl),intent(in)                    :: stepsize(3)

real(c_float),intent(in)                :: expt(ipar(2)*ipar(3)/ipar(1)**2)
real(kind=sgl),INTENT(IN)               :: accum(ipar(6),ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)               :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)               :: rgx(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: rgy(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: rgz(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: mask(ipar(2)/ipar(1),ipar(3)/ipar(1))
real(kind=dbl),INTENT(IN)               :: prefactor
integer(irg),intent(in)                 :: n
real(dbl),dimension(:),intent(in)       :: x
real(dbl),intent(out)                   :: f
real(kind=sgl),intent(IN)               :: gammaval
logical,intent(in),optional             :: verbose

integer(kind=irg)                       :: nnx, nny, binx, biny, Emin, Emax, nregions
complex(dbl)                            :: D
real(kind=sgl)                          :: quat(4), bindx, ma, mi, eu(3), ho(3)
real(kind=sgl)                          :: binned(ipar(2)/ipar(1),ipar(3)/ipar(1))

real(kind=sgl),allocatable              :: EBSDpatternintd(:,:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:)

! variables that must be saved for the next time this function is called
integer(kind=irg),allocatable           :: img1(:), img2(:)

! other variables
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl), allocatable             :: EBSDvector(:)
integer(kind=irg)                       :: i, j, istat, jpar(7)

logical                                 :: stat, readonly
integer(kind=irg)                       :: hdferr, nlines


! EULER ANGLE INPUT IS IN DEGREES

!eu = (/X(1)*2.0*stepsize(1) - stepsize(1) + initmeanval(1), X(2)*2.0*stepsize(2) - stepsize(2)  + initmeanval(2), &
!       X(3)*2.0*stepsize(3) - stepsize(3) + initmeanval(3)/)*dtor 

ho = (/X(1)*2.0*stepsize(1) - stepsize(1) + initmeanval(1), X(2)*2.0*stepsize(2) - stepsize(2)  + initmeanval(2), &
       X(3)*2.0*stepsize(3) - stepsize(3) + initmeanval(3)/)

binx = ipar(2)/ipar(1)
biny = ipar(3)/ipar(1)
bindx = 1.0/float(ipar(1)**2)

allocate(EBSDvector(binx*biny))
allocate(EBSDpatternintd(binx,biny),&
EBSDpatterninteger(binx,biny), EBSDpatternad(binx,biny))

binned              = 0.0
EBSDpatternintd     = 0.0
EBSDpatterninteger  = 0
EBSDpatternad       = 0
EBSDvector          = 0.0

!==============================================================================
!============IMAGE 1===========================================================
!==============================================================================

!quat(1:4) = eu2qu(eu)
quat(1:4)   = ho2qu(ho)
jpar(1:7)   = ipar(1:7)
Emin        = ipar(8)
Emax        = ipar(9)
nregions    = ipar(10)

call CalcEBSDPatternSingleFull(jpar,quat,accum,mLPNH,mLPSH,rgx,&
                               rgy,rgz,binned,Emin,Emax,mask,prefactor)

binned = binned**gammaval

mi = minval(binned)
ma = maxval(binned)

EBSDpatternintd     = ((binned - mi)/ (ma-mi))
EBSDpatterninteger  = nint(EBSDpatternintd*255.0)
EBSDpatternad       =  adhisteq(nregions,binx,biny,EBSDpatterninteger)
binned              = float(EBSDpatternad)

binned              = binned*mask
    
do i = 1,biny
    do j = 1,binx
        EBSDvector((i-1)*binx+j) = binned(j,i)
    end do
end do

if(vecnorm(EBSDvector) .ne. 0.0) then
    EBSDvector = EBSDvector/vecnorm(EBSDvector)
else
    call FatalError('EMFitOrientationcalfunEBSD:','Norm of calculated pattern is zero...check input data.')
end if

!=======================================================================
!===================MUTUAL INFORMATION SECTION==========================
!=======================================================================
!allocate(img1(binx*biny),img2(binx*biny))
!img1 = 0
!img2 = 0

!ma = maxval(EBSDvector)
!mi = minval(EBSDvector)

!img1 = nint(255.0*(EBSDvector - mi)/(ma - mi))

!ma = maxval(expt)
!mi = minval(expt)

!img2 = nint(255.0*(expt - mi)/(ma - mi))

!img_fit_cumul(1:binx*biny) = img1(1:binx*biny)
!img_expt_cumul(1:binx*biny) = img2(1:binx*biny)

!open(unit=13,file='/Users/saranshsingh/Desktop/testd.txt',action='write')
!open(unit=14,file='/Users/saranshsingh/Desktop/teste.txt',action='write')

!do i = 1,binx
!    do j = 1,biny
!        write(13,'(F15.6)',advance='no')EBSDvector((i-1)*biny+j)
!        write(14,'(F15.6)',advance='no')expt((i-1)*biny+j)
!    end do
!    write(13,*)''
!    write(14,*)''
!end do

!close(13)
!close(14)
!stop

!F = Jaccard_Distance(img1, img2, binx*biny)

!=======================================================================
! CALCULATE THE DOT PRODUCT
!=======================================================================

F = 1.0 - DOT_PRODUCT(EBSDvector,expt)

if (present(verbose)) then
    if(verbose) then
        eu = ho2eu(ho)    
        print*,'eu, F = ',eu(1:3)*180.0/cPi, F
    end if
end if

end subroutine EMFitOrientationcalfunEBSD

!--------------------------------------------------------------------------
!
! SUBROUTINE:EMFitOrientationcalfunECP
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief This function is used by bobyqa to fit an EC pattern
!
!> @details The main purpose of this routine is to calculte the difference of 1 with the dot
!> product of an experimental pattern with the given set of detector parameters. This is used
!> by bobyqa module to fit an EC pattern.
!>
!> This routine will first compute the detector arrays rgx etc. if necessary, and then perform
!> the usual interpolation from the square Lambert projection. The pattern will be a basic pattern,
!> without any intensity scaling or binning etc; the calling program should take care of those 
!> operations.
!
!> @param ipar array with integer input parameters
!> @param initmeanval mean value of search space
!> @param EBSDpattern output array
!> @param quats quaternion input array
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 12/12/15 SS 1.0 original
!> @date 03/28/16 SS 1.1 omega is no longer a variable parameter
!--------------------------------------------------------------------------

recursive subroutine EMFitOrientationcalfunECP(ipar, initmeanval, expt, accum, &
                                mLPNH, mLPSH, n, x, f, mask, prefactor, rgx, rgy, rgz,&
                                stepsize, gammaval, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EMFitOrientationcalfunECP

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:

! ipar(1)  = binning always set to 1
! ipar(2)  = npix
! ipar(3)  = npix
! ipar(4)  = npx
! ipar(5)  = npy
! ipar(6)  = 1
! ipar(7)  = 1
! ipar(8)  = nregions
! ipar(9)  = not initialized (for maintaining same interface as EBSD)
! ipar(10) = not initialized (for maintaining same interface as EBSD)

! initmeanval(2) = phi1
! initmeanval(3) = phi
! initmeanval(4) = phi2

! stepsize(1) = step_phi1 
! stepsize(2) = step_phi 
! stepsize(3) = step_phi2 

! X = (/phi1, phi, phi2/) in DEGREES

            
use local
use rotations
use constants
use filters
use math, ONLY:Jaccard_Distance
use ECPmod

use,INTRINSIC :: ISO_C_BINDING
           
implicit none

integer(c_size_t),intent(in)            :: ipar(10)
real(sgl),intent(in)                    :: initmeanval(3)
real(sgl),intent(in)                    :: stepsize(3)

real(c_float),intent(in)                :: expt(ipar(2)*ipar(3)/ipar(1)**2)
real(kind=sgl),INTENT(IN)               :: accum(ipar(6),ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)               :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)               :: rgx(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: rgy(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: rgz(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)               :: mask(ipar(2)/ipar(1),ipar(3)/ipar(1))
real(kind=dbl),INTENT(IN)               :: prefactor
integer(irg),intent(in)                 :: n
real(dbl),dimension(:),intent(in)       :: x
real(dbl),intent(out)                   :: f
real(kind=sgl),intent(IN)               :: gammaval
logical,intent(in),optional             :: verbose


integer(kind=irg)                       :: nnx, nny, binx, biny, Emin, Emax
complex(dbl)                            :: D
real(kind=sgl)                          :: quat(4), bindx, ma, mi, eu(3), ho(3)
real(kind=sgl)                          :: binned(ipar(2),ipar(3))

real(kind=sgl),allocatable              :: ECPpatternintd(:,:)
integer(kind=irg),allocatable           :: ECPpatterninteger(:,:), ECPpatternad(:,:)

! variables that must be saved for the next time this function is called
integer(kind=irg),allocatable           :: img1(:), img2(:)

! other variables
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl), allocatable             :: ECPvector(:)
integer(kind=irg)                       :: i, j, istat, jpar(7)

logical                                 :: stat, readonly
integer(kind=irg)                       :: hdferr, nlines, nregions


! EULER ANGLE INPUT IS IN DEGREES

!eu = (/X(1)*2.0*stepsize(1) - stepsize(1) + initmeanval(1), X(2)*2.0*stepsize(2) - stepsize(2)  + initmeanval(2), &
!       X(3)*2.0*stepsize(3) - stepsize(3) + initmeanval(3)/)*dtor 

ho = (/X(1)*2.0*stepsize(1) - stepsize(1) + initmeanval(1), X(2)*2.0*stepsize(2) - stepsize(2)  + initmeanval(2), &
       X(3)*2.0*stepsize(3) - stepsize(3) + initmeanval(3)/)

binx = ipar(2)
biny = ipar(3)

allocate(ECPvector(binx*biny))
allocate(ECPpatternintd(ipar(2),ipar(3)),&
ECPpatterninteger(ipar(2),ipar(3)), ECPpatternad(ipar(2),ipar(3)))

binned = 0.0
ECPpatternintd = 0.0
ECPpatterninteger = 0
ECPpatternad = 0

if (present(verbose)) then
    if(verbose) then    
        print*,'eu = ',eu(1:3)*180.0/cPi
    end if
end if

!==============================================================================
!============IMAGE 1===========================================================
!==============================================================================

!quat(1:4) = eu2qu(eu)
quat(1:4) = ho2qu(ho)
jpar(1:7) = ipar(1:7)

call CalcECPatternSingleFull(jpar,quat,accum,mLPNH,mLPSH,rgx,&
                               rgy,rgz,binned,mask)

binned = binned**gammaval

mi = minval(binned)
ma = maxval(binned)

nregions = ipar(8)
ECPpatternintd = (binned - mi)/ (ma-mi)
ECPpatterninteger = nint(ECPpatternintd*255.0)
ECPpatternad =  adhisteq(nregions,binx,biny,ECPpatterninteger)
binned = ECPpatternad

do i=1,biny
    do j=1,binx
        ECPvector((i-1)*binx+j) = binned(j,i)
    end do
end do

if(vecnorm(ECPvector) .ne. 0.0) then
    ECPvector = ECPvector/vecnorm(ECPvector)
else
    print*,'Norm of calculated pattern is zero...check input data.'
    stop
end if

!=======================================================================
! CALCULATE THE DOT PRODUCT
!=======================================================================

F = 1.0 - DOT_PRODUCT(ECPvector,expt)

end subroutine EMFitOrientationcalfunECP


end module
