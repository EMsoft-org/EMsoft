! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
module PFInversion

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: ForwardProjection
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief forward projection from ODF in cubochoric space to IPF in Lambert space
!
!> @param ODF         orientation distribution function
!> @param ncub        number of bins in semi-edge in x,y and z directions
!> @param PFhkl       hkl indices of the specific pole figure
!> @param nLam        bins in the output along semi-edge in Lambert projection
!
!> @out PFLam         Pole figure in Lambert projection
!
!> @date 11/07/16     SS 1.0 original
!> @date 11/10/16     SS 1.1 added symmetry; all integration done in FZ
!> @date 12/08/16     SS 1.2 all computations in quaternions to avoid infinity problem
!--------------------------------------------------------------------------
recursive subroutine ForwardProjection(ncub, ODF, PFhkl, nLam, PFLam, pgnum) &
bind(c, name='ForwardProjection')
!DEC$ ATTRIBUTES DLLEXPORT :: ForwardProjection

use local
use constants
use Lambert
use rotations
use error
use io
use dictmod
use typedefs,only:dicttype
use quaternions
use symmetry,only:CalcFamily
use ISO_C_BINDING

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: ncub
real(kind=dbl),INTENT(IN)           :: ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub)
integer(kind=irg),INTENT(IN)        :: PFhkl(3)
integer(kind=irg),INTENT(IN)        :: nLam
real(kind=dbl),INTENT(OUT)          :: PFLam(-nLam:nLam,-nLam:nLam)
integer(kind=irg),INTENT(IN)        :: pgnum

real(kind=dbl)                      :: lamx(-nlam:nlam), lamy(-nlam:nlam)
real(kind=dbl)                      :: lam(2), xyz(3), hcrossy(3), PFhkl_f(3)
integer(kind=irg)                   :: ii, jj, kk, ierr, nth
real(kind=dbl)                      :: cth, sth, rod(4), rod2(4), r(3), r2(4), cu(3), eu(3), eu2(3)
real(kind=dbl)                      :: qu(4), qu2(4), qures(4)
real(kind=dbl),allocatable          :: dth(:)
integer(kind=irg)                   :: nix, niy, niz, nixp, niyp, nizp
real(kind=dbl)                      :: nbx, nby, nbz, dx, dy, dz, dxm, dym, dzm
type(dicttype),pointer              :: dict
integer(kind=irg)                   :: Pmdims, FZtype, FZorder
!integer(kind=irg),INTENT(OUT)       :: itmp(48,3)                   


! allocate the dict structure
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum

! initialize the symmetry matrices
call DI_Init(dict,'nil') 
Pmdims = dict%Nqsym

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

lamx = (/(dble(ii)/dble(nLam), ii = -nLam,nLam)/)
lamy = (/(dble(ii)/dble(nLam), ii = -nLam,nLam)/)

! only valid for cubic/tetragonal/orthorhombic symmetry
! normalize the input pole figure
PFhkl_f = dble(PFhkl)
PFhkl_f = PFhkl_f/NORM2(PFhkl_f)

nth = 101
allocate(dth(1:nth))
dth = 0.D0

dth = (/(2.D0*cPi*float(ii-1)/(float(nth-1)),ii=1,nth)/)

do ii = -nLam,nLam
    do jj = -nLam,nLam

        lam = (/lamx(ii),lamy(jj)/)
        xyz = LambertSquareToSphere(lam,ierr)

        if (ierr .eq. 0) then
           xyz = xyz/NORM2(xyz)
        else
            call FatalError('LambertSquareToSphere:','Coulnd not convert lambert square to sphere')
        end if

!       hcrossy =  PFhkl_f x xyz; 
!       only valid for cubic/tetragonal/orthorhombic symmetry
!       general case will be dealt with later

        hcrossy =  (/ PFhkl_f(2)*xyz(3) - PFhkl_f(3)*xyz(2),&
                      PFhkl_f(3)*xyz(1) - PFhkl_f(1)*xyz(3),&
                      PFhkl_f(1)*xyz(2) - PFhkl_f(2)*xyz(1)/)

        if(sum(abs(hcrossy)) .ne. 0) hcrossy = hcrossy/NORM2(hcrossy)

        cth = sum(PFhkl_f*xyz)
        qu = ax2qu((/hcrossy(1),hcrossy(2),hcrossy(3),acos(cth)/))

        do kk = 1,nth 
            qu2 = ax2qu((/xyz(1),xyz(2),xyz(3),dth(kk)/))

            qures = quat_mult(qu2,qu)
            
            eu = qu2eu(qures)
            call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)
                        
            cu = eu2cu(eu2)

            nbx = cu(1)/LPs%ap*ncub
            nby = cu(2)/LPs%ap*ncub
            nbz = cu(3)/LPs%ap*ncub

            nix = floor(nbx)
            niy = floor(nby)
            niz = floor(nbz)

            if(nix < -ncub) nix = -ncub
            if(niy < -ncub) niy = -ncub
            if(niz < -ncub) niz = -ncub
 
            nixp = nix + 1
            niyp = niy + 1
            nizp = niz + 1
 
            if(nixp > ncub) nixp = ncub
            if(niyp > ncub) niyp = ncub
            if(nizp > ncub) nizp = ncub

            dx = nbx - nix
            dy = nby - niy
            dz = nbz - niz

            dxm = 1.D0 - dx
            dym = 1.D0 - dy
            dzm = 1.D0 - dz

! trilinear interpolation of the ODF

            PFLam(ii,jj) = PFLam(ii,jj) + &
                             (ODF(nix,niy,niz)*dxm*dym + ODF(nixp,niy,niz)*dx*dym +&
                             ODF(nix,niyp,niz)*dxm*dy + ODF(nixp,niyp,niz)*dx*dy)*dzm +&
                             (ODF(nix,niy,nizp)*dxm*dym + ODF(nixp,niy,nizp)*dx*dym +&
                             ODF(nix,niyp,nizp)*dxm*dy + ODF(nixp,niyp,nizp)*dx*dy)*dz

        end do
    end do
end do

end subroutine ForwardProjection
!--------------------------------------------------------------------------
!
! SUBROUTINE: BackProjection
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief back projection from PF in Lambert space to ODF in cubochoric space
!
!> @param ncub     number of bins in semi-edge in x,y and z directions
!> @param PFhkl    hkl indices of the specific pole figure
!> @param nLam     bins in the output along semi-edge in Lambert projection
!> @param PFLam    Pole figure in Lambert projection
!
!> @out ODF        orientation distribution function
!
!> @date 11/07/16  SS 1.0 original
!> @date 11/10/16  SS 1.1 added symmetry; all integration done in FZ
!> @date 12/08/16  SS 1.2 all computations in quaternion to avoid infinity problem
!--------------------------------------------------------------------------
recursive subroutine BackProjection(ncub, ODF, PFhkl, nLam, PFLam, pgnum) &
bind(c, name = 'BackProjection')
!DEC$ ATTRIBUTES DLLEXPORT :: BackProjection

use local
use constants
use Lambert
use rotations
use error
use dictmod
use typedefs,only:dicttype
use symmetry,only:CalcFamily
use ISO_C_BINDING

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: ncub
real(kind=dbl),INTENT(OUT)          :: ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub)
integer(kind=irg),INTENT(IN)        :: PFhkl(3)
integer(kind=irg),INTENT(IN)        :: nLam
real(kind=dbl),INTENT(IN)           :: PFLam(-nLam:nLam,-nLam:nLam)
integer(kind=irg),INTENT(IN)        :: pgnum

real(kind=dbl)                       :: xy(2), PFhkl_f(3), xyz(3), hcrossy(3)
integer(kind=irg)                    :: ii, jj, kk, ll, ierr, nth
real(kind=dbl)                       :: cth, sth, rod(4), rod2(4), r(3), r2(4), cu(3), qu(4), qu2(4), &
                                        eu(3), eu2(3), qures(4)
real(kind=dbl),allocatable           :: dth(:)
integer(kind=irg)                    :: nix, niy, niz, nixp, niyp, nizp
real(kind=dbl)                       :: nbx, nby, nbz, dx, dy, dz, dxm, dym, dzm, dconst
type(dicttype),pointer               :: dict
integer(kind=irg)                    :: Pmdims, FZtype, FZorder

! allocate the dict structure
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum

! initialize the symmetry matrices
call DI_Init(dict,'nil') 
Pmdims = dict%Nqsym

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

! only valid for cubic/tetragonal/orthorhombic symmetry
! normalize the input pole figure
PFhkl_f = dble(PFhkl)
PFhkl_f = PFhkl_f/NORM2(PFhkl_f)

nth = 101
allocate(dth(1:nth))
dth = 0.D0

dth = (/(2.D0*cPi*float(ii-1)/(float(nth-1)),ii=1,nth)/)

dconst = 1.D0/dble(nth)

do ii = -nLam,nLam
    do jj = -nLam,nLam
        
        xy = (/dble(ii)/dble(nLam), dble(jj)/dble(nLam)/)
        xyz = LambertSquareToSphere(xy,ierr)

        if (ierr .eq. 0) then
           xyz = xyz/NORM2(xyz)
        else
            call FatalError('LambertSquareToSphere:','Coulnd not convert lambert square to sphere')
        end if

!       hcrossy =  PFhkl_f x xyz; 
!       only valid for cubic/tetragonal/orthorhombic symmetry
!       general case will be dealt with later

        hcrossy =  (/ PFhkl_f(2)*xyz(3) - PFhkl_f(3)*xyz(2),&
                      PFhkl_f(3)*xyz(1) - PFhkl_f(1)*xyz(3),&
                      PFhkl_f(1)*xyz(2) - PFhkl_f(2)*xyz(1)/)

        if(sum(abs(hcrossy)) .ne. 0) hcrossy = hcrossy/NORM2(hcrossy)
! rotation by -omega about cross(h,y)

        cth = sum(PFhkl_f*xyz)
        qu = ax2qu((/hcrossy(1),hcrossy(2),hcrossy(3),acos(cth)/))

        do kk = 1,nth
            qu2 = ax2qu((/xyz(1),xyz(2),xyz(3),dth(kk)/))

            qures = quat_mult(qu2,qu)
 
            eu = qu2eu(qures)
            call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)

            cu = eu2cu(eu2)

            nbx = cu(1)/LPs%ap*ncub
            nby = cu(2)/LPs%ap*ncub
            nbz = cu(3)/LPs%ap*ncub

            nix = floor(nbx)
            niy = floor(nby)
            niz = floor(nbz)
 
            if(nix < -ncub) nix = -ncub
            if(niy < -ncub) niy = -ncub
            if(niz < -ncub) niz = -ncub
 
            nixp = nix + 1
            niyp = niy + 1
            nizp = niz + 1
 
            if(nixp > ncub) nixp = ncub
            if(niyp > ncub) niyp = ncub
            if(nizp > ncub) nizp = ncub

            dx = nbx - nix
            dy = nby - niy
            dz = nbz - niz

            dxm = 1.D0 - dx
            dym = 1.D0 - dy
            dzm = 1.D0 - dz

! assign intensity in ODF according to how close to each cell the resultant orientation is
! sort of like tri-linear interpolation
            ODF(nix,niy,niz)  = ODF(nix,niy,niz)  + PFLam(ii,jj)*dxm*dym*dzm*dconst
            ODF(nixp,niy,niz) = ODF(nixp,niy,niz) + PFLam(ii,jj)*dx*dym*dzm*dconst
            ODF(nix,niyp,niz) = ODF(nix,niyp,niz) + PFLam(ii,jj)*dxm*dy*dzm*dconst
            ODF(nix,niy,nizp) = ODF(nix,niy,nizp) + PFLam(ii,jj)*dxm*dym*dz*dconst

            ODF(nixp,niyp,niz)  = ODF(nixp,niyp,niz)  + PFLam(ii,jj)*dx*dy*dzm*dconst
            ODF(nixp,niy,nizp)  = ODF(nixp,niy,nizp)  + PFLam(ii,jj)*dx*dym*dz*dconst
            ODF(nix,niyp,nizp)  = ODF(nix,niyp,nizp)  + PFLam(ii,jj)*dxm*dy*dz*dconst
            ODF(nixp,niyp,nizp) = ODF(nixp,niyp,nizp) + PFLam(ii,jj)*dx*dy*dz*dconst
 
        end do       
    end do
end do

end subroutine BackProjection

end module PFInversion
