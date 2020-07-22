! ###################################################################
! Copyright (c) 2014-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:detectors.f90
!--------------------------------------------------------------------------
!
! MODULE: detectors
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief module containing (ideally) all detector related codes
!
!> @date 09/11/19 MDG 1.0 original 
!--------------------------------------------------------------------------
module detectors

use math 

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:GenerateEBSDDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl EBSD name list structure
!> @param mcnl Monte Carlo name list structure
!> @param EBSDMCdata MC data
!> @param EBSDdetector detector arrays
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS 1.1 added omega as the second tilt angle
!> @date 07/07/15   SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 04/03/18  MDG 3.0 new version with split use of name list arrays
!> @date 02/19/19  MDG 4.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!> @date 09/12/19  MDG 4.1 modification of pattern orientation; now looking from detector to sample
!>                         moved subroutine into its own module file (detectors.f90)
!--------------------------------------------------------------------------
recursive subroutine GenerateEBSDDetector(enl, mcnl, EBSDMCdata, EBSDdetector, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: GenerateEBSDDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
!f2py intent(in,out) ::  mcnl
type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
!f2py intent(in,out) ::  EBSDMCdata
type(EBSDDetectorType),INTENT(INOUT)    :: EBSDdetector
!f2py intent(in,out) ::  EBSDdetector
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:), testarray(:,:)  ! scintillator coordinate arrays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable              :: z(:,:)           
integer(kind=irg)                       :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nsx, nsy, elp  
real(kind=sgl)                          :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                          :: ixy(2)

!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(enl%numsy),stat=istat)
! if (istat.ne.0) then ...
! change to the detector point of view necessitates negating the x pattern center coordinate
scin_x = - ( -enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i-1, i=1,enl%numsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (mcnl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(mcnl%omega * dtor)
sw = sin(mcnl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert Nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...

elp = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   EBSDdetector%rgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   EBSDdetector%rgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   EBSDdetector%rgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(EBSDdetector%rgx*EBSDdetector%rgx+EBSDdetector%rgy*EBSDdetector%rgy+EBSDdetector%rgz*EBSDdetector%rgz)
  EBSDdetector%rgx = EBSDdetector%rgx*z
  EBSDdetector%rgy = EBSDdetector%rgy*z
  EBSDdetector%rgz = EBSDdetector%rgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!
  nsx = (mcnl%numsx - 1)/2
  nsy = nsx
! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(nsx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

  Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins

! correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
  ipx = enl%numsx/2 + nint(enl%xpc)
  ipy = enl%numsy/2 + nint(enl%ypc)
  pcvec = (/ EBSDdetector%rgx(ipx,ipy), EBSDdetector%rgy(ipx,ipy), EBSDdetector%rgz(ipx,ipy) /)
  calpha = cos(alpha)
  do i=1,enl%numsx
    do j=1,enl%numsy
! do the coordinate transformation for this detector pixel
       dc = (/ EBSDdetector%rgx(i,j),EBSDdetector%rgy(i,j),EBSDdetector%rgz(i,j) /)
! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
!      call LambertgetInterpolation(dc, scl, enl%nsx, enl%nsy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)
        ixy = scl * LambertSphereToSquare( dc, istat )
        x = ixy(1)
        ixy(1) = ixy(2)
        ixy(2) = -x
! four-point interpolation (bi-quadratic)
        nix = int(nsx+ixy(1))-nsx
        niy = int(nsy+ixy(2))-nsy
        dx = ixy(1)-nix
        dy = ixy(2)-niy
        dxm = 1.0-dx
        dym = 1.0-dy
! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          g = 0.25 
        else
          g = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if
! interpolate the intensity 
        do k=Emin,Emax 
          s = EBSDMCdata%accum_e(k,nix,niy) * dxm * dym + &
              EBSDMCdata%accum_e(k,nix+1,niy) * dx * dym + &
              EBSDMCdata%accum_e(k,nix,niy+1) * dxm * dy + &
              EBSDMCdata%accum_e(k,nix+1,niy+1) * dx * dy
! intensities do not need to be flipped vertically
          EBSDdetector%accum_e_detector(k,i,j) = g * s
        end do
    end do
  end do 

if (present(verbose)) then
  if (verbose.eqv..TRUE.) call Message(' -> completed detector generation', frm = "(A)")
end if 

!====================================
end subroutine GenerateEBSDDetector


!--------------------------------------------------------------------------
!
! SUBROUTINE:GeneratemyEBSDDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays for the case where each pattern has a (slightly) different detector configuration
!
!> @param enl EBSD name list structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS 1.1 added omega as the second tilt angle
!> @date 07/07/15   SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 02/22/18  MDG 1.3 forked from EBSDGenerateDetector; uses separate pattern center coordinates patcntr
!> @date 04/03/18  MDG 2.0 updated with new name list and data structures
!> @date 02/19/19  MDG 3.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine GeneratemyEBSDDetector(enl, mcnl, EBSDMCdata, nsx, nsy, numE, tgx, tgy, tgz, accum_e_detector, patcntr, bg)
!DEC$ ATTRIBUTES DLLEXPORT :: GeneratemyEBSDDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
!f2py intent(in,out) ::  mcnl
type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
!f2py intent(in,out) ::  EBSDMCdata
integer(kind=irg),INTENT(IN)            :: nsx
integer(kind=irg),INTENT(IN)            :: nsy
integer(kind=irg),INTENT(IN)            :: numE
real(kind=sgl),INTENT(INOUT)            :: tgx(nsx,nsy)
!f2py intent(in,out) ::  tgx
real(kind=sgl),INTENT(INOUT)            :: tgy(nsx,nsy)
!f2py intent(in,out) ::  tgy
real(kind=sgl),INTENT(INOUT)            :: tgz(nsx,nsy)
!f2py intent(in,out) ::  tgz
real(kind=sgl),INTENT(INOUT)            :: accum_e_detector(numE,nsx,nsy)
!f2py intent(in,out) ::  accum_e_detector
real(kind=sgl),INTENT(IN)               :: patcntr(3)
logical,INTENT(IN),OPTIONAL             :: bg

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:), testarray(:,:)    ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable              :: z(:,:)           
integer(kind=irg)                       :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nx, ny, elp   
real(kind=sgl)                          :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx, xpc, ypc, L         ! various parameters
real(kind=sgl)                          :: ixy(2)
integer(kind=irg)                       :: ss(3)


ss=shape(EBSDMCdata%accum_e)
!====================================
! ------ generate the detector arrays
!====================================
xpc = patcntr(1)
ypc = patcntr(2)
L = patcntr(3)

allocate(scin_x(nsx),scin_y(nsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( -xpc - ( 1.0 - nsx ) * 0.5 - (/ (i-1, i=1,nsx) /) ) * enl%delta
scin_y = ( ypc - ( 1.0 - nsy ) * 0.5 - (/ (i-1, i=1,nsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (mcnl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(mcnl%omega * dtor)
sw = sin(mcnl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...

elp = nsy + 1
L2 = L * L
do j=1,nsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + L*cw
  Lc = cw * scin_x(j) + L*sw
  do i=1,nsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   tgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   tgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   tgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(tgx*tgx+tgy*tgy+tgz*tgz)
  tgx = tgx*z
  tgy = tgy*z
  tgz = tgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!
nx = (mcnl%numsx - 1)/2
ny = nsx
if (present(bg)) then
 if (bg.eqv..TRUE.) then 
! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(nx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

  Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins

! correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(enl%delta/L/sqrt(sngl(cPi)))
  ipx = nsx/2 + nint(xpc)
  ipy = nsy/2 + nint(ypc)
  pcvec = (/ tgx(ipx,ipy), tgy(ipx,ipy), tgz(ipx,ipy) /)
  calpha = cos(alpha)
  do i=1,nsx
    do j=1,nsy
! do the coordinate transformation for this detector pixel
       dc = (/ tgx(i,j),tgy(i,j),tgz(i,j) /)
! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc
! convert these direction cosines to coordinates in the Rosca-Lambert projection
        ixy = scl * LambertSphereToSquare( dc, istat )
        x = ixy(1)
        ixy(1) = ixy(2)
        ixy(2) = -x
! four-point interpolation (bi-quadratic)
        nix = int(nx+ixy(1))-nx
        niy = int(ny+ixy(2))-ny
		if ((nix+1).gt.((ss(2)-1)/2)) nix=nix-1
		if ((niy+1).gt.((ss(3)-1)/2)) niy=niy-1
        dx = ixy(1)-nix
        dy = ixy(2)-niy
        dxm = 1.0-dx
        dym = 1.0-dy
! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          g = 0.25 
        else
          g = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if
! interpolate the intensity 
        do k=Emin,Emax 
          s = EBSDMCdata%accum_e(k,nix,niy) * dxm * dym + &
              EBSDMCdata%accum_e(k,nix+1,niy) * dx * dym + &
              EBSDMCdata%accum_e(k,nix,niy+1) * dxm * dy + &
              EBSDMCdata%accum_e(k,nix+1,niy+1) * dx * dy
          accum_e_detector(k,i,j) = g * s
        end do
    end do
  end do 
 else
   accum_e_detector = 1.0
 end if 
end if
end subroutine GeneratemyEBSDDetector


!--------------------------------------------------------------------------
!
! SUBROUTINE:GeneratedefectEBSDDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays for the case where each pattern has a (slightly) different detector configuration
!
!> @param enl EBSD name list structure
!
!> @date 11/05/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GeneratedefectEBSDDetector(enl, mcnl, nsx, nsy, tgx, tgy, tgz, patcntr)
!DEC$ ATTRIBUTES DLLEXPORT :: GeneratedefectEBSDDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDdefectNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(MCCLNameListType),INTENT(INOUT)          :: mcnl
!f2py intent(in,out) ::  mcnl
integer(kind=irg),INTENT(IN)                  :: nsx
integer(kind=irg),INTENT(IN)                  :: nsy
real(kind=sgl),INTENT(INOUT)                  :: tgx(nsx,nsy)
!f2py intent(in,out) ::  tgx      
real(kind=sgl),INTENT(INOUT)                  :: tgy(nsx,nsy)
!f2py intent(in,out) ::  tgy      
real(kind=sgl),INTENT(INOUT)                  :: tgz(nsx,nsy)
!f2py intent(in,out) ::  tgz      
real(kind=dbl),INTENT(IN)                     :: patcntr(3)
      
real(kind=sgl),allocatable                    :: scin_x(:), scin_y(:), testarray(:,:)  ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                      :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                                :: alp, ca, sa, cw, sw
real(kind=sgl)                                :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable                    :: z(:,:)           
integer(kind=irg)                             :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nx, ny, elp   
real(kind=sgl)                                :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                                :: sx, dx, dxm, dy, dym, rhos, x, bindx, xpc, ypc, L         ! various parameters
real(kind=sgl)                                :: ixy(2)

!====================================
! ------ generate the detector arrays
!====================================
xpc = sngl(patcntr(1))
ypc = sngl(patcntr(2))
L = sngl(patcntr(3))

allocate(scin_x(nsx),scin_y(nsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( -xpc - ( 1.0 - nsx ) * 0.5 - (/ (i-1, i=1,nsx) /) ) * enl%delta
scin_y = ( ypc - ( 1.0 - nsy ) * 0.5 - (/ (i-1, i=1,nsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (mcnl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(mcnl%omega * dtor)
sw = sin(mcnl%omega * dtor)

elp = nsy + 1
L2 = L * L
do j=1,nsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + L*cw
  Lc = cw * scin_x(j) + L*sw
  do i=1,nsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   tgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   tgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   tgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(nsx,nsy))
  z = 1.0/sqrt(tgx*tgx+tgy*tgy+tgz*tgz)
  tgx = tgx*z
  tgy = tgy*z
  tgz = tgz*z
deallocate(z)
!====================================

! for the defect EBSD mode we do not create a realistic background,
! so no need to deal with the energy arrays at all...
end subroutine GeneratedefectEBSDDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl EBSD name list structure
!
!> @date 06/24/14 MDG 1.0 original
!> @date 07/01/15  SS 1.1 added omega as the second tilt angle
!> @date 07/07/15  SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 05/10/18 MDG 1.3 added arguments to clean up namelists 
!> @date 02/19/19 MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine EBSDFullGenerateDetector(enl, EBSDdetector, numEbins, numzbins, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDFullGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use math
use io
use Lambert
use error

IMPLICIT NONE

type(EBSDFullNameListType),INTENT(INOUT):: enl
!f2py intent(in,out) ::  enl
type(EBSDDetectorType),INTENT(INOUT)    :: EBSDdetector
!f2py intent(in,out) ::  EBSDdetector
integer(kind=irg),INTENT(IN)            :: numEbins, numzbins
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
integer(kind=irg)                       :: i, j, Emin, Emax, istat, k, ipx, ipy, ierr, elp   
real(kind=sgl)                          :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                          :: ixy(2)


!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(enl%numsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( -enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i-1, i=1,enl%numsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (enl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
elp = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy

   rhos = 1.0/sqrt(sx + scin_y(i)**2)

   allocate(EBSDdetector%detector(j,i)%lambdaEZ(1:numEbins,1:numzbins))

   EBSDdetector%detector(j,i)%lambdaEZ = 0.D0

   EBSDdetector%detector(j,i)%dc = (/(scin_y(i) * ca + sa * Ls) * rhos, Lc * rhos,&
                                    (-sa * scin_y(i) + ca * Ls) * rhos/)

   EBSDdetector%detector(j,i)%dc =  &
         EBSDdetector%detector(j,i)%dc/vecnorm(EBSDdetector%detector(j,i)%dc)

!  if (ierr .ne. 0) then
!      call FatalError('EBSDFullGenerateDetector:','Lambert Projection coordinate undefined')
!  end if

  end do
end do
deallocate(scin_x, scin_y)

alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
ipx = nint(enl%numsx/2 + enl%xpc)
ipy = nint(enl%numsy/2 + enl%ypc)
pcvec = EBSDdetector%detector(ipx,ipy)%dc
calpha = cos(alpha)

do i = 1,enl%numsx
    do j = 1,enl%numsy

        dc = EBSDdetector%detector(i,j)%dc 
        dp = DOT_PRODUCT(pcvec,dc)
        theta = acos(dp)

        if ((i.eq.ipx).and.(j.eq.ipy)) then
          EBSDdetector%detector(i,j)%cfactor = 0.25 
        else
          EBSDdetector%detector(i,j)%cfactor = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if

    end do
end do

if (present(verbose)) call Message(' -> completed detector generation', frm = "(A)")

!====================================
end subroutine EBSDFullGenerateDetector






end module detectors 
