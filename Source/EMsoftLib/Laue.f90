! ###################################################################
! Copyright (c) 2019-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:Laue.f90
!--------------------------------------------------------------------------
!
! MODULE: Laue
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic routines for Laue pattern simulation
!
!> @date 03/28/19  MDG 1.0 original version 
!--------------------------------------------------------------------------
module Lauemod

use local

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:Lauereadangles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file (either Euler or quaternions)
!
!> @param orientationfilename file name
!> @param numangles number of angles read 
!> @param angles list of angles 
!
!> @todo this should really become a routine that is independent of all the 
!> diffraction modalities
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Lauereadangles(orientationfilename,numangles,angles,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Lauereadangles

use typedefs
use NameListTypedefs
use io
use error 
use files
use quaternions
use rotations

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: orientationfilename
integer(kind=irg),INTENT(OUT)           :: numangles
type(AngleType),pointer                 :: angles
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: atype
character(3)							:: degrad
real(kind=sgl),allocatable              :: eulang(:,:)   ! euler angle array

real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
integer(kind=irg)                       :: istat
character(fnlen)                        :: anglefile

!====================================
! get the angular information, either in Euler angles or in quaternions, from a text file
!====================================
! open the angle file 
anglefile = trim(EMsoft_getEMdatapathname())//trim(orientationfilename)
anglefile = EMsoft_toNativePath(anglefile)
open(unit=dataunit,file=trim(anglefile),status='old',action='read')

! get the type of angle first [ 'eu' or 'qu' ]
read(dataunit,*) atype
if ((atype.ne.'eu').and.(atype.ne.'qu')) then 
	call FatalError('Lauereadangles','orientations need to be in Euler or quaternion form')
end if

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue(' Number of angle entries = ',io_int,1)
end if

if (atype.eq.'eu') then
! allocate the euler angle array
  allocate(eulang(3,numangles),stat=istat)
! if istat.ne.0 then do some error handling ... 
  do i=1,numangles
    read(dataunit,*) eulang(1:3,i)
  end do
  close(unit=dataunit,status='keep')

! are the angles in radians or degrees ?
  ! if (maxval(eulang).gt.6.3) then
  	degrad = 'deg'
  ! else 
  ! 	degrad = 'rad'
  ! end if

! convert the euler angle triplets to quaternions
  allocate(angles%quatang(4,numangles),stat=istat)

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A/)")
  
  if (degrad.eq.'deg') then 
	  do i=1,numangles
	    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i)*dtor)
	  end do
  else
	  do i=1,numangles
	    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i))
	  end do
  end if
  deallocate(eulang)
else
! the input file has quaternions, not Euler triplets
  allocate(angles%quatang(4,numangles),stat=istat)
  do i=1,numangles
    read(dataunit,*) angles%quatang(1:4,i)
  end do
end if

close(unit=dataunit,status='keep')

call Message(' completed reading orientations')

end subroutine Lauereadangles


!--------------------------------------------------------------------------
!
! FUNCTION:getLauePattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single Laue pattern
!
!> @param lnl Laue name list
!> @param qu orientation quaternion
!> @param reflist list of potential reflections
!> @param kouter largest wave number
!> @param kinner smallest wave number 
!> @param npx number of pixels along x on detector
!> @param npy along y
!> @param refcnt number of reflections in linked list
!
!> @date 07/30/19 MDG 1.0 original
!> @date 09/06/19 MDG 1.1 correct scale factor
!--------------------------------------------------------------------------
recursive function getLauePattern(lnl, qu, reflist, kouter, kinner, npx, npy, refcnt) result(pattern)
!DEC$ ATTRIBUTES DLLEXPORT :: getLauePattern

use local
use typedefs
use NameListTypedefs
use io
use files
use quaternions
use rotations

IMPLICIT NONE

type(LaueNameListType),INTENT(IN)			:: lnl 
real(kind=dbl),INTENT(IN) 					:: qu(4) 
type(Laue_g_list),pointer                   :: reflist 
real(kind=sgl),INTENT(IN) 					:: kouter 
real(kind=sgl),INTENT(IN) 					:: kinner 
integer(kind=irg),INTENT(IN) 				:: npx 
integer(kind=irg),INTENT(IN) 				:: npy 
integer(kind=irg),INTENT(IN) 				:: refcnt
real(kind=sgl) 								:: pattern(npx, npy)

real(kind=sgl) 								:: r1, r2, grot(3), gyz, gbig, gsmall, kspot, kprime(3), kp(3), scl
type(Laue_g_list),pointer                   :: rltmp
integer(kind=irg)							:: gg, traref

traref = 1  ! reflection mode = 1
if (trim(lnl%Lauemode).eq.'transmission') traref = 2 ! transmission mode = 2

scl = lnl%SDdistance * 1000.0 / lnl%pixelsize 

pattern = 0.0

nullify(rltmp)
rltmp => reflist

do gg=1,refcnt 
  grot = sngl(quat_Lp(conjg(qu),rltmp%xyz))
! make sure this reflection lies between the two limiting Ewald spheres
  gyz = sum(grot*grot) 
  gbig = 2.0*grot(1)*kouter + gyz 
  gsmall = 2.0*grot(1)*kinner + gyz 
  if ((gbig.le.0.0).and.(gsmall.ge.0.0)) then 
! this is a good point so let's draw stuff
! first, determine the wave number for this point
    kspot = - gyz / (2.0*grot(1))
    kprime = (/ kspot, 0.0, 0.0 /)  + grot
    if ((kprime(1).gt.0.0).and.(traref.eq.2)) then 
      kp = (kprime/abs(kprime(1))) * scl
      if ( (abs(kp(2)).le.(lnl%numpx/2)).and.(abs(kp(3)).le.(lnl%numpy/2)) ) then 
! draw the reflection on the transmission screen 
	    call addLauereflection(pattern, lnl%numpx, lnl%numpy, kp, sngl(rltmp%sfs), lnl%spotw)
      end if
    end if 
    if ((kprime(1).lt.0.0).and.(traref.eq.1)) then 
      kp = (kprime/abs(kprime(1))) * scl
      if ( (abs(kp(2)).le.(lnl%numpx/2)).and.(abs(kp(3)).le.(lnl%numpy/2)) ) then 
! draw the reflection on the backreflection screen 
		kp(2) = -kp(2)
	    call addLauereflection(pattern, lnl%numpx, lnl%numpy, kp, sngl(rltmp%sfs), lnl%spotw)
      end if
    end if 
  end if
  rltmp => rltmp%next
end do

end function getLauePattern




!--------------------------------------------------------------------------
!
! FUNCTION:getLaueSlitPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single Laue pattern by summing sample voxels; incident beam direction 
!> is now a parameter instead of being along a coordinate axis.
!
!> @param lnl Laue name list
!> @param qu orientation quaternion
!> @param reflist list of potential reflections (of type Laue_grow_list)
!> @param kouter largest wave number
!> @param kinner smallest wave number 
!> @param npx number of pixels along x on detector
!> @param npy along y
!> @param refcnt number of reflections in linked list
!> @param kinpre distance traveled in sample before scattering event 
!> @param kvec unit wave vector direction for this voxel
!> @param kvox voxel coordinates w.r.t. back focal plane slit projection center  
!
!> @date 01/29/20 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getLaueSlitPattern(lnl, qu, reflist, lmin, lmax, refcnt, &
                                      kinpre, kvec, kvox) result(pattern)
!DEC$ ATTRIBUTES DLLEXPORT :: getLaueSlitPattern

use local
use typedefs
use constants
use NameListTypedefs
use io
use math
use files
use quaternions
use rotations

IMPLICIT NONE

type(LaueSlitNameListType),INTENT(IN)   :: lnl 
real(kind=dbl),INTENT(IN)               :: qu(4) 
type(Laue_grow_list),pointer            :: reflist 
real(kind=sgl),INTENT(IN)               :: lmin
real(kind=sgl),INTENT(IN)               :: lmax
integer(kind=irg),INTENT(IN)            :: refcnt
real(kind=sgl),INTENT(IN)               :: kinpre
real(kind=sgl),INTENT(IN)               :: kvec(3)
real(kind=sgl),INTENT(IN)               :: kvox(3)
real(kind=sgl)                          :: pattern(lnl%Ny, lnl%Nz)
    
real(kind=sgl)                          :: th, la, s0(3), s(3), G(3), d, scl, dvec(3), kexit(3), kinpost, dins, atf, Ly, Lz
type(Laue_grow_list),pointer            :: rltmp
integer(kind=irg)                       :: i, j, k 

! this is always in transmission mode; we follow the algorithm suggested in
! the paper by Arnaud et al., "A laboratory transmission diffraction Laue 
! setup to evaluate single crystal quality", to appear in JAC.  We take the 
! unit incident beam direction and dot it with all the unit reciprocal lattice
! vectors to get the diffraction angle; then we use Bragg's law and the interplanar
! spacing to extract the wave length that gives rise to the reflection; if this 
! wavelength falls inside the given interval, then we proceed and add the reflection
! to the pattern.  

d = lnl%sampletodetector - lnl%samplethickness
Ly = float(lnl%Ny/2) * lnl%ps
Lz = float(lnl%Nz/2) * lnl%ps

pattern = 0.0

! first handle the transmitted beam  (normalized to unit intensity)
if (kvec(1).gt.0.0) then 
  scl = (d + abs(kvox(1))) / kvec(1)    ! scale factor to get to the detector plane
  dvec = kvox + kvec * scl             ! this is with respect to the optical axis
  dvec = dvec + (/ 0.D0, lnl%Dy, lnl%Dz  /)   ! correct for the pattern center to get detector coordinates
  if ((abs(dvec(2)).lt.Ly).or.(abs(dvec(3)).lt.Lz)) then ! we plot this reflection
! correct the intensity for absorption (total distance inside sample dins = kinpre + kinpost)
    scl = abs(kvox(1)) / kvec(1)    ! scale factor to get to the sample exit plane
    kexit = kvox + kvec * scl       ! this is with respect to the optical axis
    kinpost = sqrt(sum((kexit-kvox)**2))
    dins = kinpre + kinpost 
    atf = exp( - dins/lnl%absl ) * lnl%beamstopatf
! and draw the reflection
    dvec = dvec / lnl%ps 
    call addLaueSlitreflection(pattern, lnl%Ny, lnl%Nz, -dvec, atf, lnl%spotw)
  end if 
end if

nullify(rltmp)
rltmp => reflist

! go through the entire linked list and determine for each potential reflector whether or not
! the corresponding wave length falls inside the allowed range; if so, then compute whether or 
! not this diffracted beam intersects the detector and generate the correct intensity at that
! detector pixel 

do i = 1, refcnt 
! for all the allowed reflections along this systematic row, compute the wave length
! using Bragg's law 
  rltmp%xyz = rltmp%xyz / vecnorm(rltmp%xyz)
  G = sngl(quat_Lp(conjg(qu),rltmp%xyz))
  if (G(1).lt.0.0) then 
    G = G / vecnorm(G)
! get the diffraction angle for the unit vectors
    th = acos( DOT_PRODUCT(kvec, G) ) - 0.5D0*cPi
    do j = 1, rltmp%Nentries
      if (rltmp%sfs(j).ne.0.0) then 
        la = 2.0 * rltmp%dspacing(j) * sin(th)
        if ((la.gt.lmin).and.(la.lt.lmax)) then ! we have a potential diffracted beam !
          ! get the scattered beam 
          s0 = kvec ! / la
          s = s0 + la * G / rltmp%dspacing(j)   ! check this relation !!!
! normalize this back to a Cartesian unit vector 
          s = s / vecnorm(s)
! this vector originates at the point kvox in the sample, so next we compute 
! where the intersection with the detector plane will be; we only need to take 
! into account those s-vectors that have a positive x-component. 
          if (s(1).gt.0.0) then 
            scl = (d + abs(kvox(1))) / s(1)    ! scale factor to get to the detector plane
            dvec = kvox + s * scl             ! this is with respect to the optical axis
            dvec = dvec + (/ 0.D0, lnl%Dy, lnl%Dz  /)   ! correct for the pattern center to get detector coordinates
            if ((abs(dvec(2)).lt.Ly).or.(abs(dvec(3)).lt.Lz)) then ! we plot this reflection
! correct the intensity for absorption (total distance inside sample dins = kinpre + kinpost)
              scl = abs(kvox(1)) / s(1)    ! scale factor to get to the sample exit plane
              kexit = kvox + s * scl       ! this is with respect to the optical axis
              kinpost = sqrt(sum((kexit-kvox)**2))
              dins = kinpre + kinpost 
              atf = exp( - dins/lnl%absl )
! and draw the reflection
              dvec = dvec / lnl%ps 
              call addLaueSlitreflection(pattern, lnl%Ny, lnl%Nz, -dvec, sngl(atf*rltmp%sfs(j)), lnl%spotw)
            end if 
          else
            CYCLE
          end if
        end if
      end if 
    end do 
  end if
  rltmp => rltmp%next
end do 


! draw the reflection on the transmission screen 
 

end function getLaueSlitPattern

!--------------------------------------------------------------------------
!
! SUBROUTINE:addLauereflection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a single reflection to a Laue pattern as a Gaussian spot
!
!> @param pattern Laue pattern intensity array
!> @param npx number of pixels along x on detector
!> @param npy along y
!> @param kp diffracted wave vector scaled to the detector geometry
!> @param sfs kinematical intensity (structure factor squared)
!> @param spotw spot size parameter 
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine addLauereflection(pattern, npx, npy, kp, sfs, spotw) 
!DEC$ ATTRIBUTES DLLEXPORT :: addLauereflection

IMPLICIT NONE

integer(kind=irg),INTENT(IN)			:: npx
integer(kind=irg),INTENT(IN)			:: npy
real(kind=sgl),INTENT(INOUT) 			:: pattern(npx, npy)
!f2py intent(in,out) ::  pattern
real(kind=sgl),INTENT(IN)				:: kp(3)
real(kind=sgl),INTENT(IN) 				:: sfs 
real(kind=sgl),INTENT(IN)				:: spotw 

integer(kind=irg),parameter 			:: dd = 15
real(kind=sgl),allocatable,save 		:: xar(:,:), yar(:,:) 
real(kind=sgl) 							:: row(dd), px, py, evals(dd,dd), dx, dy
integer(kind=irg) 						:: i, ddd, ix, iy

! make sure that the xar and yar arrays are allocated 
if (allocated(xar).eqv..FALSE.) then 
	row = (/ (real(i), i=0,dd-1) /) - real(dd/2)
	allocate(xar(dd,dd), yar(dd,dd))
	do i=1,dd 
		xar(:,i) = row
		yar(i,:) = row
	end do
end if

ddd = (dd-1)/2

px = kp(2) + real(npx/2)
py = kp(3) + real(npy/2)

dx = px-int(px)
dy = py-int(py)
ix = int(px)-ddd
iy = int(py)-ddd 

evals = alog(sfs+1.0) * exp( - ( (xar-dx)**2 + (yar -dy)**2 ) * spotw )

if ( (ix+dd.lt.npx).and.(iy+dd.lt.npy).and.(ix.gt.0).and.(iy.gt.0) ) then
  pattern(ix:ix+dd-1,iy:iy+dd-1) = pattern(ix:ix+dd-1,iy:iy+dd-1) + evals
end if

end subroutine addLauereflection



!--------------------------------------------------------------------------
!
! SUBROUTINE:addLauereflection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a single reflection to a Laue pattern as a Gaussian spot
!
!> @param pattern Laue pattern intensity array
!> @param npx number of pixels along x on detector
!> @param npy along y
!> @param kp diffracted wave vector scaled to the detector geometry
!> @param sfs kinematical intensity (structure factor squared)
!> @param spotw spot size parameter 
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine addLaueSlitreflection(pattern, npx, npy, kp, sfs, spotw) 
!DEC$ ATTRIBUTES DLLEXPORT :: addLauereflection

IMPLICIT NONE

integer(kind=irg),INTENT(IN)      :: npx
integer(kind=irg),INTENT(IN)      :: npy
real(kind=sgl),INTENT(INOUT)      :: pattern(npx, npy)
!f2py intent(in,out) ::  pattern
real(kind=sgl),INTENT(IN)         :: kp(3)
real(kind=sgl),INTENT(IN)         :: sfs 
real(kind=sgl),INTENT(IN)         :: spotw 

integer(kind=irg),parameter       :: dd = 15
real(kind=sgl),allocatable,save   :: xar(:,:), yar(:,:) 
real(kind=sgl)                    :: row(dd), px, py, evals(dd,dd), dx, dy
integer(kind=irg)                 :: i, ddd, ix, iy

! make sure that the xar and yar arrays are allocated 
if ( (allocated(xar).eqv..FALSE.) .and. (allocated(yar).eqv..FALSE.) ) then 
  row = (/ (real(i), i=0,dd-1) /) - real(dd/2)
  allocate(xar(dd,dd), yar(dd,dd))
  do i=1,dd 
    xar(:,i) = row
    yar(i,:) = row
  end do
end if

ddd = (dd-1)/2

px = kp(2) + real(npx/2)
py = kp(3) + real(npy/2)

dx = px-int(px)
dy = py-int(py)
ix = int(px)-ddd
iy = int(py)-ddd 

! evals = alog(sfs+1.0) * exp( - ( (xar-dx)**2 + (yar -dy)**2 ) * spotw )
evals = sfs * exp( - ( (xar-dx)**2 + (yar -dy)**2 ) * spotw )

if ( (ix+dd.lt.npx).and.(iy+dd.lt.npy).and.(ix.gt.0).and.(iy.gt.0) ) then
  pattern(ix:ix+dd-1,iy:iy+dd-1) = pattern(ix:ix+dd-1,iy:iy+dd-1) + evals
end if

end subroutine addLaueSlitreflection


!--------------------------------------------------------------------------
!
! FUNCTION:backprojectLauePattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief back project a single Laue pattern onto a square Lambert map (Northern hemisphere)
!
!> @param kk (/ kouter, kinner /) largest and smallest wave numbers
!> @param delta detector pixel size (micron)
!> @param L distance detector to sample (micron)
!> @param Ldims (/npx,npy/) detector dimensions
!> @param LPdims (/numsx, numsy /) Lambert projection dimensions (-numsx:numsx, -numsy:numsy)
!> @param Lpat input Laue pattern 
!> @param Lauemode 'transmission' or 'reflection'
!
!> @date 07/31/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function backprojectLauePattern(kk, delta, L, Ldims, LPdims, Lpat, Lauemode) result(mLPNH)
!DEC$ ATTRIBUTES DLLEXPORT :: backprojectLauePattern

use local
use typedefs
use NameListTypedefs
use io
use files
use quaternions
use rotations
use constants
use Lambert

IMPLICIT NONE

real(kind=sgl),INTENT(IN)           :: kk(2)
real(kind=sgl),INTENT(IN)           :: delta 
real(kind=sgl),INTENT(IN)           :: L
integer(kind=irg),INTENT(IN)        :: Ldims(2) 
integer(kind=irg),INTENT(IN)        :: LPdims(2) 
real(kind=sgl),INTENT(IN)           :: Lpat(Ldims(1),Ldims(2))
character(fnlen),INTENT(IN)         :: Lauemode
real(kind=sgl)                      :: mLPNH(-LPdims(1):LPdims(1), -LPdims(2):LPdims(2))

integer(kind=irg)                   :: ix, iy, ierr, slp1(2), slp2(2)
real(kind=dbl)                      :: px, py, phi, quat(4), yquat(4), r, r2, p(2), q(2), n1(3), n2(3), &
                                       rn1(3), rn2(3), Ledge

! scale factor for square Lambert projection
Ledge = dble((LPdims(1)-1)/2)

! set the array to zero
mLPNH = 0.0

yquat = (/ 1.D0/sqrt(2.D0), 0.D0, -1.D0/sqrt(2.D0), 0.D0 /)

do ix=1,Ldims(1)
  px = dble(ix-Ldims(1)/2) * delta
  do iy=1,Ldims(2)
    py = dble(iy-Ldims(2)/2) * delta
    if (Lpat(ix,iy).ne.0.D0) then 
    ! if (Lpat(ix,iy).ge.1.D-3) then 
! get the azimuthal angle phi from px and py
        phi = datan2(px, py) - cPi*0.5D0
        quat = (/ cos(phi*0.5D0), -sin(phi*0.5D0), 0.D0, 0.D0 /)
        r = sqrt(px*px+py*py)
        r2 = r*r
        p = (/ sqrt((kk(1)+L)**2+r2), sqrt((kk(2)+L)**2+r2) /)
        q = (/ 1.D0/sqrt(2.D0*p(1)**2-2.D0*(kk(1)+L)*p(1)), 1.D0/sqrt(2.D0*p(2)**2-2.D0*(kk(2)+L)*p(2)) /)
        n1 = q(1) * (/ kk(1) + L - p(1), 0.D0, r /)
        n2 = q(2) * (/ kk(2) + L - p(2), 0.D0, r /)
! these are the normals in the azimuthal plane (x,z); next we need to apply the rotation by phi 
! around x to bring the vector into the correct location
! also rotate these unit vectors by 90° around the y-axis so that they fall in along the equator
        rn1 = quat_Lp(yquat,quat_Lp(quat, n1))
        rn2 = quat_Lp(yquat,quat_Lp(quat, n2))
        rn1 = rn1/norm2(rn1) 
        rn2 = rn2/norm2(rn2) 
! and project both points onto the Lambert square
        slp1 = nint(LambertSphereToSquare(rn1,ierr) * Ledge) - Ledge
        slp2 = nint(LambertSphereToSquare(rn2,ierr) * Ledge) - Ledge
        mLPNH(slp1(1),slp1(2)) = mLPNH(slp1(1),slp1(2)) + Lpat(ix,iy)
        mLPNH(slp2(1),slp2(2)) = mLPNH(slp2(1),slp2(2)) + Lpat(ix,iy)
        ! mLPNH(slp1(1):slp2(1),slp1(2):slp2(2)) = mLPNH(slp1(1):slp2(1),slp1(2):slp2(2)) + Lpat(ix,iy)
    end if
  end do 
end do


end function backprojectLauePattern

end module Lauemod
