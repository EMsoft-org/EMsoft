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
module PFInversionmod

use math

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
recursive subroutine ForwardProjection(ncub, ODF, PFhkl_eqv, neqv, nLam, PFLam, pgnum, cell)
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
use crystal

use ISO_C_BINDING

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: ncub
real(kind=dbl),INTENT(IN)           :: ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub)
integer(kind=irg),INTENT(IN)        :: neqv
integer(kind=irg),INTENT(IN)        :: PFhkl_eqv(neqv,3)
integer(kind=irg),INTENT(IN)        :: nLam
real(kind=dbl),INTENT(OUT)          :: PFLam(-nLam:nLam,-nLam:nLam)
integer(kind=irg),INTENT(IN)        :: pgnum
type(unitcell)                      :: cell

real(kind=dbl)                      :: lamx(-nlam:nlam), lamy(-nlam:nlam)
real(kind=dbl)                      :: lam(2), xyz(3), hcrossy(3), PFhkl_f(3), PFhkl_dbl(3)
integer(kind=irg)                   :: ii, jj, kk, ll, ierr, nth
real(kind=dbl)                      :: cth, sth, rod(4), rod2(4), r(3), r2(4), cu(3), eu(3), eu2(3)
real(kind=dbl)                      :: qu(neqv,4), qu2(4), qures(4)
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

nth = 101
allocate(dth(1:nth))
dth = 0.D0

dth = (/(2.D0*cPi*float(ii-1)/(float(nth-1)),ii=1,nth)/)

do ii = -nLam,nLam
    do jj = -nLam,nLam

        lam = (/lamx(ii),lamy(jj)/)
        xyz = LambertSquareToSphere(lam,ierr)
        
        if (ierr .eq. 0) then
           xyz = xyz/vecnorm(xyz)
        end if

        do ll = 1,neqv 

            ! convert to cartesian reference frame and normalize the input pole figure
            PFhkl_dbl = dble(PFhkl_eqv(ll,1:3))

            call TransSpace(cell,PFhkl_dbl,PFhkl_f,'r','c')
            call NormVec(cell,PFhkl_f,'c')

            ! hcrossy =  PFhkl_f x xyz;  
            hcrossy =  (/ PFhkl_f(2)*xyz(3) - PFhkl_f(3)*xyz(2),&
                      PFhkl_f(3)*xyz(1) - PFhkl_f(1)*xyz(3),&
                      PFhkl_f(1)*xyz(2) - PFhkl_f(2)*xyz(1)/)

            if(sum(abs(hcrossy)) .ne. 0) hcrossy = hcrossy/vecnorm(hcrossy)

            cth = sum(PFhkl_f*xyz)
            qu(ll,1:4) = ax2qu((/hcrossy(1),hcrossy(2),hcrossy(3),acos(cth)/))
            if(qu(ll,1) .lt. 0.D0) qu(ll,1:4) = -qu(ll,1:4)

        end do

        do kk = 1,nth 

            qu2 = ax2qu((/xyz(1),xyz(2),xyz(3),dth(kk)/))
            if(qu2(1) .lt. 0.D0) qu2 = -qu2

            do ll = 1,neqv

                qures = quat_mult(qu2,qu(ll,1:4))
                if(qures(1) .lt. 0.D0) qures = -qures

                eu = qu2eu(qures)
                call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)
                        
                cu = eu2cu(eu2)

                nbx = cu(1)*dble(ncub)/LPs%ap/2.D0
                nby = cu(2)*dble(ncub)/LPs%ap/2.D0
                nbz = cu(3)*dble(ncub)/LPs%ap/2.D0

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
recursive subroutine BackProjection(ncub, ODF, PFhkl, nLam, PFLam, pgnum, cell)
!DEC$ ATTRIBUTES DLLEXPORT :: BackProjection

use local
use constants
use Lambert
use rotations
use error
use dictmod
use typedefs,only:dicttype
use crystal

use ISO_C_BINDING

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: ncub
real(kind=dbl),INTENT(OUT)          :: ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub)
integer(kind=irg),INTENT(IN)        :: PFhkl(3)
integer(kind=irg),INTENT(IN)        :: nLam
real(kind=dbl),INTENT(IN)           :: PFLam(-nLam:nLam,-nLam:nLam)
integer(kind=irg),INTENT(IN)        :: pgnum
type(unitcell)                      :: cell

real(kind=dbl)                       :: xy(2), PFhkl_f(3), xyz(3), hcrossy(3), PFhkl_dbl(3)
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

! convert to cartesian frame and normalize the input pole figure
PFhkl_dbl = dble(PFhkl)

call TransSpace(cell,PFhkl_dbl,PFhkl_f,'r','c')
call NormVec(cell,PFhkl_f,'c')

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
           xyz = xyz/vecnorm(xyz)
        else
            call FatalError('LambertSquareToSphere:','Coulnd not convert lambert square to sphere')
        end if

!       hcrossy =  PFhkl_f x xyz; 
        hcrossy =  (/ PFhkl_f(2)*xyz(3) - PFhkl_f(3)*xyz(2),&
                      PFhkl_f(3)*xyz(1) - PFhkl_f(1)*xyz(3),&
                      PFhkl_f(1)*xyz(2) - PFhkl_f(2)*xyz(1)/)

        if(sum(abs(hcrossy)) .ne. 0) hcrossy = hcrossy/vecnorm(hcrossy)
! rotation by -omega about cross(h,y)

        cth = sum(PFhkl_f*xyz)

        qu = ax2qu((/hcrossy(1),hcrossy(2),hcrossy(3),acos(cth)/))
        if(qu(1) .lt. 0.D0) qu = -qu

        do kk = 1,nth
            
            qu2 = ax2qu((/xyz(1),xyz(2),xyz(3),dth(kk)/))
            if(qu2(1) .lt. 0.D0) qu2 = -qu2

            qures = quat_mult(qu2,qu)
            if(qures(1) .lt. 0.D0) qures = -qures

            eu = qu2eu(qures)
            call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)

            cu = eu2cu(eu2)

            nbx = cu(1)*dble(ncub)/LPs%ap/2.D0
            nby = cu(2)*dble(ncub)/LPs%ap/2.D0
            nbz = cu(3)*dble(ncub)/LPs%ap/2.D0

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
! tri-linear interpolation
            ODF(nix,niy,niz)   = ODF(nix,niy,niz)  + PFLam(ii,jj)*dxm*dym*dzm*dconst
            ODF(nixp,niy,niz)  = ODF(nixp,niy,niz) + PFLam(ii,jj)*dx*dym*dzm*dconst
            ODF(nix,niyp,niz)  = ODF(nix,niyp,niz) + PFLam(ii,jj)*dxm*dy*dzm*dconst
            ODF(nixp,niyp,niz) = ODF(nixp,niyp,niz) + PFLam(ii,jj)*dx*dy*dzm*dconst

            ODF(nix,niy,nizp)   = ODF(nix,niy,nizp)  + PFLam(ii,jj)*dxm*dym*dz*dconst
            ODF(nixp,niy,nizp)  = ODF(nixp,niy,nizp)  + PFLam(ii,jj)*dx*dym*dz*dconst
            ODF(nix,niyp,nizp)  = ODF(nix,niyp,nizp)  + PFLam(ii,jj)*dxm*dy*dz*dconst
            ODF(nixp,niyp,nizp) = ODF(nixp,niyp,nizp) + PFLam(ii,jj)*dx*dy*dz*dconst   
        end do  
         
    end do
end do

end subroutine BackProjection

!--------------------------------------------------------------------------
!
! SUBROUTINE: StereoInverse
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief stereographic projection to spherical vector
!
!> @param xy      lambert projection
!> @param radius  radius of circle for which stereographi projection is performed
!> @param xyz     spherical vector
!
!
!> @date 03/31/17  SS 1.0 original
!--------------------------------------------------------------------------

subroutine StereoInverse(xy,radius,xyz,ierr)
!DEC$ ATTRIBUTES DLLEXPORT :: StereoInverse

use local
use Lambert

IMPLICIT NONE

real(kind=dbl),INTENT(IN)          :: xy(2)
real(kind=dbl),INTENT(IN)          :: radius
integer(kind=irg),INTENT(INOUT)    :: ierr
!f2py intent(in,out) ::  ierr
real(kind=dbl),INTENT(OUT)         :: xyz(3)
real(kind=dbl)                     :: qq, q

ierr = 0
if(maxval(dabs(xy)) == 0.0) then

    xyz = (/0.D0,0.D0,1.D0/)
else

    qq = sum(xy**2)
    if (qq .ge. radius*radius) then
        xyz = (/0.D0,0.D0,0.D0/)
        ierr = 1
    else

        q = 1.D0/(radius*radius + qq)
        xyz = (/2.D0*xy(1), 2.D0*xy(2), 1.D0 - qq/)
        xyz = xyz * q

    end if

end if

end subroutine StereoInverse

!--------------------------------------------------------------------------
!
! SUBROUTINE: InterpolateLambert
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Interpolate from a Lambert projection map for a specific spherical vector
!
!> @param xyz     spherical vector
!> @param PFLam   Lambert projection map
!> @param nLam    size of lambert map
!> @param res     resultant intensity for xyz
!
!> @date 03/31/17  SS 1.0 original
!--------------------------------------------------------------------------
subroutine InterpolateLambert(xyz,PFLam,nLam,res)
!DEC$ ATTRIBUTES DLLEXPORT :: InterpolateLambert

use local
use Lambert

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)   :: xyz(3)
!f2py intent(in,out) ::  xyz
integer(kind=irg),INTENT(IN)   :: nLam
real(kind=dbl),INTENT(IN)      :: PFLam(-nLam:nLam,-nLam:nLam)
real(kind=dbl),INTENT(OUT)     :: res
real(kind=dbl)                 :: xy(2), scl
integer(kind=irg)              :: ierr, nix, niy, nixp, niyp
real(kind=dbl)                 :: dx, dy, dxm, dym

scl = float(nLam)

if(xyz(3) .lt. 0.D0) xyz = -xyz

xy = LambertSphereToSquare(xyz,ierr)
xy = scl*xy
res = 0.D0

if (ierr .eq. 0) then
    nix = floor(xy(1))
    niy = floor(xy(2))

    nixp = nix + 1
    niyp = niy + 1


    if(nix < -nLam) nix = nLam
    if(niy < -nLam) niy = nLam
    if(nixp > nLam) nixp = nLam
    if(niyp > nLam) niyp = nLam

    dx = xy(1) - nix
    dy = xy(2) - niy
    dxm = 1.0 - dx
    dym = 1.0 - dy

    res = PFLam(nix,niy)*dxm*dym + PFLam(nixp,niy)*dx*dym + &
          PFLam(nix,niyp)*dxm*dy + PFLam(nixp,niyp)*dx*dy

end if

end subroutine InterpolateLambert

!--------------------------------------------------------------------------
!
! SUBROUTINE: PFLamToStereo
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Interpolate from a Lambert projection map for a pole figure
!
!> @date 04/02/17  SS 1.0 original
!--------------------------------------------------------------------------
function PFLamToStereo(nLam, PFLam) result(PFstereo)
!DEC$ ATTRIBUTES DLLEXPORT :: PFLamToStereo

use local
use Lambert

IMPLICIT NONE

integer(kind=irg),INTENT(IN)           :: nLam
real(kind=dbl),INTENT(IN)              :: PFLam(-nLam:nLam,-nLam:nLam)

real(kind=dbl)                         :: PFstereo(-nLam:nLam,-nLam:nLam)
real(kind=dbl)                         :: xy(2), xyz(3), delta, res
integer(kind=irg)                      :: ii,jj, ierr

delta = 1.D0/dble(nLam)

do ii = -nLam,nLam
    do jj = -nLam,nLam

        xy = (/dble(ii), dble(jj)/)*delta
        xyz = LambertSquareToSphere(xy,ierr)

        xy = (/float(ii)/float(nLam), float(jj)/float(nLam)/);
        call StereoInverse(xy,1.D0,xyz,ierr);
        xyz = xyz/vecnorm(xyz)

        if (ierr .eq. 0) then
            res = InterpolateLambert(xyz,PFLam,nLam)
            PFstereo(ii,jj) = res
        end if

    end do
end do


end function PFLamToStereo

!--------------------------------------------------------------------------
!
! SUBROUTINE: ConvertCubochoric2Stereo3Dvol
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  convert 3D volume from cubochoric to stereographic projection of quaternion
!
!> @date 06/08/17  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ConvertCubochoric2Stereo3Dvol(dict, vol_cub_in, vol_stereo_out, nx, ny, nz)
!DEC$ ATTRIBUTES DLLEXPORT:: ConvertCubochoric2Stereo3Dvol

use local 
use rotations
use constants
use quaternions
use dictmod
use typedefs

type(dicttype),pointer             :: dict
real(kind=dbl),INTENT(IN)          :: vol_cub_in(1:nx,1:ny,1:nz)
real(kind=dbl),INTENT(OUT)         :: vol_stereo_out(1:nx,1:ny,1:nz)
integer(kind=irg),INTENT(IN)       :: nx, ny, nz

integer(kind=irg)                  :: i, j, k, l, idcub, idcub2, nsym, ncux, ncuy, ncuz
integer(kind=irg)                  :: idx, idy, idz, Pmdims
integer(kind=irg)                  :: nix, niy, niz, nixp, niyp, nizp, pgnum, FZtype, FZorder
real(kind=dbl)                     :: nbx, nby, nbz, dx, dy, dz, dxm, dym, dzm, LPapn, st(3)
real(kind=dbl)                     :: ax(4), qu(4), qu2(4), mu(4), cu(3), r(3), tanwo4, eu(3), eu2(3)

ncux = (nx - 1)/2
ncuy = (ny - 1)/2
ncuz = (nz - 1)/2

vol_stereo_out = 0.D0

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

LPapn = LPs%ap/2.D0

Pmdims = dict%Nqsym

do i = -ncux,ncux
  do j = -ncuy,ncuy
    do k = -ncuz,ncuz

        cu = (/(dble(k)*LPapn/dble(ncux)),(dble(j)*LPapn/dble(ncuy)),(dble(i)*LPapn/dble(ncuz))/)

        qu = cu2qu(cu)
        if(qu(1) .lt. 0.D0) qu = -qu

        do l = 1,Pmdims

          qu2 = quat_mult(dict%Pm(1:4,l),qu)
          if(qu2(1) .lt. 0.D0) qu2 = -qu2

          r = qu2(2:4)
          if(vecnorm(r) .ne. 0.D0) then
              r = r/vecnorm(r)
              st(1:3) = tan(0.5D0*acos(qu2(1)))*r(1:3)
          else
              st(1:3) = 0.D0
          end if

          nbx = st(1)*dble(ncux) + ncux + 1
          nby = st(2)*dble(ncuy) + ncuy + 1
          nbz = st(3)*dble(ncuz) + ncuz + 1

          nix = floor(nbx) 
          niy = floor(nby) 
          niz = floor(nbz) 
 
          if(nix .lt. 1) nix = 1
          if(niy .lt. 1) niy = 1
          if(niz .lt. 1) niz = 1

          if(nix .gt. nx) nix = nx
          if(niy .gt. ny) niy = ny
          if(niz .gt. nz) niz = nz

          nixp = nix + 1
          niyp = niy + 1
          nizp = niz + 1

          if(nixp .lt. 1) nixp = 1
          if(niyp .lt. 1) niyp = 1
          if(nizp .lt. 1) nizp = 1

          if(nixp .gt. nx) nixp = nx
          if(niyp .gt. ny) niyp = ny
          if(nizp .gt. nz) nizp = nz

          dx = nbx - nix
          dy = nby - niy
          dz = nbz - niz

          dxm = 1.D0 - dx
          dym = 1.D0 - dy
          dzm = 1.D0 - dz

          idx = k + ncux + 1
          idy = j + ncuy + 1
          idz = i + ncuz + 1

          vol_stereo_out(nix,niy,niz)    = vol_stereo_out(nix,niy,niz)    + vol_cub_in(idx,idy,idz)*dxm*dym*dzm
          vol_stereo_out(nixp,niy,niz)   = vol_stereo_out(nixp,niy,niz)   + vol_cub_in(idx,idy,idz)*dx*dym*dzm
          vol_stereo_out(nix,niyp,niz)   = vol_stereo_out(nix,niyp,niz)   + vol_cub_in(idx,idy,idz)*dxm*dy*dzm
          vol_stereo_out(nixp,niyp,niz)  = vol_stereo_out(nixp,niyp,niz)  + vol_cub_in(idx,idy,idz)*dx*dy*dzm
          vol_stereo_out(nix,niy,nizp)   = vol_stereo_out(nix,niy,nizp)   + vol_cub_in(idx,idy,idz)*dxm*dym*dz
          vol_stereo_out(nixp,niy,nizp)  = vol_stereo_out(nixp,niy,nizp)  + vol_cub_in(idx,idy,idz)*dx*dym*dz
          vol_stereo_out(nix,niyp,nizp)  = vol_stereo_out(nix,niyp,nizp)  + vol_cub_in(idx,idy,idz)*dxm*dy*dz
          vol_stereo_out(nixp,niyp,nizp) = vol_stereo_out(nixp,niyp,nizp) + vol_cub_in(idx,idy,idz)*dx*dy*dz

        end do

        !if((dble(i)/dble(ncux))**2 + (dble(j)/dble(ncuy))**2 + (dble(k)/dble(ncuz))**2 .le. 1.D0) then

        !    r = (/dble(k)/dble(ncux), dble(j)/dble(ncuy), dble(i)/dble(ncuz)/)

        !    if(vecnorm(r) .ne. 0) then
        !        tanwo4 = vecnorm(r)
        !        r = r/vecnorm(r)
        !        ax(1:4) = (/r,4.D0*atan(tanwo4)/)
        !        !qu = ax2qu(ax)
        !    else
        !        ax = (/1.D0, 0.D0, 0.D0, 0.D0/)
        !        !qu = ax2qu(ax)
        !    end if

        !    eu = ax2eu(ax)
        !    call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)
        !    cu = eu2cu(eu2)
 
        !    nbx = cu(1)*dble(ncux)/LPs%ap/2.D0 + ncux + 1
        !    nby = cu(2)*dble(ncuy)/LPs%ap/2.D0 + ncuy + 1
        !    nbz = cu(3)*dble(ncuz)/LPs%ap/2.D0 + ncuz + 1

        !    nix = floor(nbx) 
        !    niy = floor(nby) 
        !    niz = floor(nbz) 
 
        !    if(nix .lt. 1) nix = 1
        !    if(niy .lt. 1) niy = 1
        !    if(niz .lt. 1) niz = 1

        !    if(nix .gt. nx) nix = nx
        !    if(niy .gt. ny) niy = ny
        !    if(niz .gt. nz) niz = nz

        !    nixp = nix + 1
        !    niyp = niy + 1
        !    nizp = niz + 1

        !    if(nixp .gt. nx) nixp = nx
        !    if(niyp .gt. ny) niyp = ny
        !    if(nizp .gt. nz) nizp = nz

        !    dx = nbx - nix
        !    dy = nby - niy
        !    dz = nbz - niz

        !    dxm = 1.D0 - dx
        !    dym = 1.D0 - dy
        !    dzm = 1.D0 - dz
            
        !    idx = k + ncux + 1
        !    idy = j + ncuy + 1
        !    idz = i + ncuz + 1

        !    vol_stereo_out(idx,idy,idz) = vol_stereo_out(idx,idy,idz) + &
        !                   vol_cub_in(nix,niy,niz)*dxm*dym*dzm + &
        !                   vol_cub_in(nixp,niy,niz)*dx*dym*dzm + &
        !                   vol_cub_in(nix,niyp,niz)*dxm*dy*dzm + &
        !                   vol_cub_in(nixp,niyp,niz)*dx*dy*dzm + &
        !                   vol_cub_in(nix,niy,nizp)*dxm*dym*dz + &
        !                   vol_cub_in(nixp,niy,nizp)*dx*dym*dz + &
        !                   vol_cub_in(nix,niyp,nizp)*dxm*dy*dz + &
        !                   vol_cub_in(nixp,niyp,nizp)*dx*dy*dz 
        !end if
      end do ! k loop
  end do ! j loop
end do ! i loop


end subroutine ConvertCubochoric2Stereo3Dvol

!--------------------------------------------------------------------------
!
! SUBROUTINE: WriteStereoOutline
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write outline of stereographic sphere for 3d visualization
!
!> @date 06/08/17  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine WriteStereoOutline(MRCheader,FEIheaders,fname,numx,numy,numz)
!DEC$ ATTRIBUTES DLLEXPORT:: WriteStereoOutline

use local
use MRCmod
use typedefs

IMPLICIT NONE

type(MRCstruct),INTENT(INOUT)                 :: MRCheader
!f2py intent(in,out) ::  MRCheader
type(FEIstruct),INTENT(INOUT)                 :: FEIheaders(1024)
!f2py intent(in,out) ::  FEIheaders
character(fnlen),INTENT(IN)                   :: fname
integer(kind=irg),INTENT(IN)                  :: numx, numy, numz

real(kind=dbl),allocatable                    :: volume(:,:,:)
real(kind=dbl)                                :: rmax2, eps
integer(kind=irg)                             :: i, j, k, nx, ny, nz, cx, cy, cz

eps = 0.01D0

allocate(volume(numx,numy,numz))
volume = 0.D0

nx = (numx - 1)/2
ny = (numy - 1)/2
nz = (numz - 1)/2

rmax2 = minval((/nx,ny,nz/))**2

do i = -nx,nx
  do j = -ny,ny
    do k = -nz,nz
      cx = i + nx + 1
      cy = j + ny + 1
      cz = k + nz + 1
      if(i .eq. 0 .and. abs(j**2 + k**2 - rmax2) .le. eps) volume(cx,cy,cz) =  volume(cx,cy,cz) + 1.D0
      if(j .eq. 0 .and. abs(i**2 + k**2 - rmax2) .le. eps) volume(cx,cy,cz) =  volume(cx,cy,cz) + 1.D0
      if(k .eq. 0 .and. abs(i**2 + j**2 - rmax2) .le. eps) volume(cx,cy,cz) =  volume(cx,cy,cz) + 1.D0
    end do ! k loop
  end do ! j loop
end do ! i loop

call MRC_write_3Dvolume(MRCheader,FEIheaders,fname,numx,numy,numz,volume,verbose=.TRUE.)

deallocate(volume)

end subroutine WriteStereoOutline

!--------------------------------------------------------------------------
!
! SUBROUTINE: SymmetrizeODF
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief apply crystal orientation to ODF
!
!> @date 05/17/17  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SymmetrizeODF(ncub, ODFlin, dict)
!DEC$ ATTRIBUTES DLLEXPORT :: SymmetrizeODF

use local 
use rotations
use constants
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: ncub
real(kind=dbl),INTENT(INOUT)        :: ODFlin((2*ncub+1)**3)
!f2py intent(in,out) ::  ODFlin
type(dicttype),pointer              :: dict

real(kind=dbl),allocatable          :: ODF(:,:,:)
integer(kind=irg)                   :: i, j, k, l, idcub, idcub2, nsym
integer(kind=irg)                   :: nix, niy, niz, nixp, niyp, nizp
real(kind=dbl)                      :: nbx, nby, nbz, dx, dy, dz, dxm, dym, dzm
real(kind=dbl)                      :: qu(4), mu(4), cu(3), cu2(3)

nsym = dict%nqsym

do i = -ncub,ncub
  do j = -ncub,ncub
    do k = -ncub,ncub

      idcub = (i + ncub)*(2*ncub + 1)**2 + (j + ncub)*(2*ncub + 1) + k + ncub + 1

      if(ODFlin(idcub) .gt. 0.D0) then

        cu(1:3) = (/dble(i), dble(j), dble(k)/)*LPs%ap/2.D0/dble(ncub)
        qu = cu2qu(cu)
        if(qu(1) .lt. 0.D0) qu = -qu

        do l = 1,nsym

          mu = quat_mult(dict%Pm(1:4,l),qu)
          if(mu(1) .lt. 0.D0) mu = -mu

          cu2 = qu2cu(mu)

          nbx = cu2(1)*dble(ncub)/LPs%ap/2.D0
          nby = cu2(2)*dble(ncub)/LPs%ap/2.D0
          nbz = cu2(3)*dble(ncub)/LPs%ap/2.D0

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
            
          nix = nix + ncub + 1
          niy = niy + ncub + 1
          niz = niz + ncub + 1

          nixp = nixp + ncub + 1
          niyp = niyp + ncub + 1
          nizp = nizp + ncub + 1

          idcub2 = (niz - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nix
          ODFlin(idcub2) =ODFlin(idcub2) + dxm*dym*dzm/dble(nsym)

          idcub2 = (niz - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nixp
          ODFlin(idcub2) = ODFlin(idcub2) + dx*dym*dzm/dble(nsym)

          idcub2 = (niz - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nix
          ODFlin(idcub2) = ODFlin(idcub2) + dxm*dy*dzm/dble(nsym)

          idcub2 = (niz - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nixp
          ODFlin(idcub2) = ODFlin(idcub2) + dx*dy*dzm/dble(nsym)

          idcub2 = (nizp - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nix
          ODFlin(idcub2) = ODFlin(idcub2) + dxm*dym*dz/dble(nsym)

          idcub2 = (nizp - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nixp
          ODFlin(idcub2) = ODFlin(idcub2) + dx*dym*dz/dble(nsym)

          idcub2 = (nizp - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nix
          ODFlin(idcub2) = ODFlin(idcub2) + dxm*dy*dz/dble(nsym)

          idcub2 = (nizp - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nixp
          ODFlin(idcub2) = ODFlin(idcub2) + dx*dy*dz/dble(nsym)

        end do

      end if

    end do
  end do
end do  

end subroutine SymmetrizeODF 

!--------------------------------------------------------------------------
!
! SUBROUTINE: ConvertCubochoric2Stereo3Dvol
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief  convert 3D volume from cubochoric to stereographic projection of quaternion
!
!> @date 06/08/17  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ConvertCubochoric2Homochoric3Dvol(dict, vol_cub_in, vol_stereo_out, nx, ny, nz)
!DEC$ ATTRIBUTES DLLEXPORT:: ConvertCubochoric2Stereo3Dvol

use local 
use rotations
use constants
use quaternions
use dictmod
use typedefs

type(dicttype),pointer             :: dict
real(kind=dbl),INTENT(IN)          :: vol_cub_in(1:nx,1:ny,1:nz)
real(kind=dbl),INTENT(OUT)         :: vol_stereo_out(1:nx,1:ny,1:nz)
integer(kind=irg),INTENT(IN)       :: nx, ny, nz

integer(kind=irg)                  :: i, j, k, l, idcub, idcub2, nsym, ncux, ncuy, ncuz
integer(kind=irg)                  :: idx, idy, idz, Pmdims
integer(kind=irg)                  :: nix, niy, niz, nixp, niyp, nizp, pgnum, FZtype, FZorder
real(kind=dbl)                     :: nbx, nby, nbz, dx, dy, dz, dxm, dym, dzm, LPapn, ho(3), maho
real(kind=dbl)                     :: ax(4), qu(4), qu2(4), mu(4), cu(3), r(3), tanwo4, eu(3), eu2(3)

ncux = (nx - 1)/2
ncuy = (ny - 1)/2
ncuz = (nz - 1)/2

vol_stereo_out = 0.D0

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

LPapn = LPs%ap/2.D0

Pmdims = dict%Nqsym

maho = (3.D0 * cPi / 4.D0)**(1.D0/3.D0) ! maximum length of homochoric vector

do i = -ncux,ncux
  do j = -ncuy,ncuy
    do k = -ncuz,ncuz

        cu = (/(dble(k)*LPapn/dble(ncux)),(dble(j)*LPapn/dble(ncuy)),(dble(i)*LPapn/dble(ncuz))/)

        qu = cu2qu(cu)
        if(qu(1) .lt. 0.D0) qu = -qu

        do l = 1,Pmdims

          qu2 = quat_mult(dict%Pm(1:4,l),qu)
          if(qu2(1) .lt. 0.D0) qu2 = -qu2

          ho = qu2ho(qu2)
          ho = ho/maho ! scale length of homochoric vector

          nbx = ho(1)*dble(ncux) + ncux + 1
          nby = ho(2)*dble(ncuy) + ncuy + 1
          nbz = ho(3)*dble(ncuz) + ncuz + 1

          nix = floor(nbx) 
          niy = floor(nby) 
          niz = floor(nbz) 
 
          if(nix .lt. 1) nix = 1
          if(niy .lt. 1) niy = 1
          if(niz .lt. 1) niz = 1

          if(nix .gt. nx) nix = nx
          if(niy .gt. ny) niy = ny
          if(niz .gt. nz) niz = nz

          nixp = nix + 1
          niyp = niy + 1
          nizp = niz + 1

          if(nixp .lt. 1) nixp = 1
          if(niyp .lt. 1) niyp = 1
          if(nizp .lt. 1) nizp = 1

          if(nixp .gt. nx) nixp = nx
          if(niyp .gt. ny) niyp = ny
          if(nizp .gt. nz) nizp = nz

          dx = nbx - nix
          dy = nby - niy
          dz = nbz - niz

          dxm = 1.D0 - dx
          dym = 1.D0 - dy
          dzm = 1.D0 - dz

          idx = k + ncux + 1
          idy = j + ncuy + 1
          idz = i + ncuz + 1

          vol_stereo_out(nix,niy,niz)    = vol_stereo_out(nix,niy,niz)    + vol_cub_in(idx,idy,idz)*dxm*dym*dzm
          vol_stereo_out(nixp,niy,niz)   = vol_stereo_out(nixp,niy,niz)   + vol_cub_in(idx,idy,idz)*dx*dym*dzm
          vol_stereo_out(nix,niyp,niz)   = vol_stereo_out(nix,niyp,niz)   + vol_cub_in(idx,idy,idz)*dxm*dy*dzm
          vol_stereo_out(nixp,niyp,niz)  = vol_stereo_out(nixp,niyp,niz)  + vol_cub_in(idx,idy,idz)*dx*dy*dzm
          vol_stereo_out(nix,niy,nizp)   = vol_stereo_out(nix,niy,nizp)   + vol_cub_in(idx,idy,idz)*dxm*dym*dz
          vol_stereo_out(nixp,niy,nizp)  = vol_stereo_out(nixp,niy,nizp)  + vol_cub_in(idx,idy,idz)*dx*dym*dz
          vol_stereo_out(nix,niyp,nizp)  = vol_stereo_out(nix,niyp,nizp)  + vol_cub_in(idx,idy,idz)*dxm*dy*dz
          vol_stereo_out(nixp,niyp,nizp) = vol_stereo_out(nixp,niyp,nizp) + vol_cub_in(idx,idy,idz)*dx*dy*dz

        end do

      end do ! k loop
  end do ! j loop
end do ! i loop


end subroutine ConvertCubochoric2Homochoric3Dvol

end module PFInversionmod
