! ###################################################################
! Copyright (c) 2016-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:Distortion.f90
!--------------------------------------------------------------------------
!
! MODULE: Distortion
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief 
!> Everything to do with distortion in electron microscopy
!
!> @details  
!> this module takes care of distortions in electron microscopy modalities due to imperfect lenses
!
!> @date 1/18/16 SS 1.0 original
!--------------------------------------------------------------------------
module distortion

use local 

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:BarrelDistortion
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief barrel distortion observed in TESCAN MIRA microscope
!
!> @param D     complex distortion coefficient
!> @param img   Input=undistorted image/output = distorted image
!> @param nnx   size of image in x
!> @param nny   size of image in y
!
!> @date 06/24/14  SS 1.0 original
!> @date 01/25/16 MDG 1.1 minor mods to make routine more efficient
!--------------------------------------------------------------------------
recursive subroutine BarrelDistortion(D, img, nnx, nny)
!DEC$ ATTRIBUTES DLLEXPORT :: BarrelDistortion

use math

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)                        :: D
real(kind=sgl),INTENT(INOUT)                        :: img(nnx,nny)
!f2py intent(in,out) ::  img
integer(kind=irg),INTENT(IN)                        :: nnx, nny

real(kind=sgl),allocatable                          :: dimg(:,:)
real(kind=dbl)                                      :: co(4), Dp, thetad
integer(kind=irg)                                   :: ii, jj, I(1)
complex(kind=dbl)                                   :: roots(3)
real(kind=dbl)                                      :: x, y, r0, r, Ap, Bp, theta
integer(kind=irg)                                   :: nix, niy, nixp, niyp
real(kind=dbl)                                      :: x_new, y_new, dx, dy, dxm, dym, shx, shy, x2, tmp, x_new_s, y_new_s
real(kind=dbl),parameter                            :: epsdp = 1.0D-8,epsth = 1.0D-2

Dp = abs(D)
thetad = datan2(aimag(D),real(D))
if (Dp .le. epsdp) return

allocate(dimg(nnx,nny))
dimg = 0.0

x_new_s = dble(nnx)*0.5D0
y_new_s = dble(nny)*0.5D0

shx = dble(floor(x_new_s))
shy = dble(floor(y_new_s))

do ii = 1,nnx
    x = dble(ii) - shx
    x2 = x*x
    do jj = 1,nny
        y = dble(jj) - shy

        co = (/Dp*Dp, 2.D0*Dp*dcos(thetad), 1.D0,  -(x2 + y*y)/)
        
        r0 = dsqrt(-co(4))

        if (r0.eq.0D0) then
          r = 0.D0
          Ap = 0.D0
          Bp = 0.D0
        else
            if (abs(thetad) .le. epsth) then
                co = (/Dp, 0.D0, 1.D0,  -dsqrt(x2 + y*y)/)  
                call cubicroots(co,roots)
                I = MINLOC((/abs(aimag(roots(1))),abs(aimag(roots(2))),abs(aimag(roots(3)))/))
                r = real(roots(I(1)))
                theta = datan2(y,x)
            else
                call cubicroots(co,roots)
                I = MINLOC((/abs(aimag(roots(1))),abs(aimag(roots(2))),abs(aimag(roots(3)))/))
                r = dsqrt(real(roots(I(1))))
                tmp = Dp*r*r*r/r0
                Ap = r/r0 + tmp*dcos(thetad)
                Bp = tmp*dsin(thetad)
                theta = datan2(y,x) - datan2(Bp, Ap)
            end if
        end if
        x_new = r*dcos(theta)
        y_new = r*dsin(theta)
        
        nix = floor(x_new + x_new_s)
        nixp = nix + 1
        dx = x_new + shx - dble(nix)

        niy = floor(y_new + y_new_s)
        niyp = niy + 1
        dy = y_new + shy - dble(niy)

        if (nix .lt. 1) then
            nix = 1
            dx = 1.D0
        end if

        if (nix .gt. nnx) then
            nix = nnx
            dx = 0.D0
        end if
       
        if (niy .lt. 1) then
            niy = 1
            dy = 1.D0
        end if

        if (niy .gt. nny) then
            niy = nny
            dy = 0.D0
        end if
        
        if (nixp .lt. 1) nixp = 1
        if (nixp .gt. nnx) nixp = nnx

        if (niyp .lt. 1) niyp = 1
        if (niyp .gt. nny) niyp = nny

        dxm = 1.D0 - dx
        dym = 1.D0 - dy

        dimg(ii,jj) = img(nix,niy)*dxm*dym + img(nixp,niy)*dxm*dy + &
                      img(nix,niyp)*dx*dym + img(nixp,niyp)*dx*dy

    end do
end do

img = dimg
deallocate(dimg)

end subroutine BarrelDistortion

end module distortion

