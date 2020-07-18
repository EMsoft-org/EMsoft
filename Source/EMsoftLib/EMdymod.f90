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
!--------------------------------------------------------------------------
! EMsoft:EMdymod.f90
!--------------------------------------------------------------------------
!
! MODULE: EMdymod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines that can be called by external code; all routines requiring HDF are in EMdymodHDF.f90
!
!> @date  10/16/15 MDG 1.0 original
!> @date  01/11/16 MDG 2.0 split into this file and EMdymodHDF.f90
!> @date  01/12/16 MDG 2.1 added functionality for DREAM.3D progress callback and cancel option
!> @date  01/13/16 MDG 2.2 name change of SingleEBSDPattern routine and split into two versions (C and other)
!> @date  01/14/16 MDG 2.3 added EMsoftCgetECPatterns routine
!> @date  01/25/16 MDG 2.4 several routine name changes
!> @date  04/28/16 MDG 2.5 unified the ipar and fpar arrays for all C-callable routines
!> @date  11/07/17 MDG 3.0 added spar string array for passing on EMsoft configuration strings
!> @date  01/21/18 MDG 4.0 removed C/C++ callable routines into the EMsoftwrapperLib folder
!--------------------------------------------------------------------------
!
! general information: the ipar and fpar arrays for all the routines that are C-callable
! are identical, so we document here their component definitions; to allow for future expansion, each
! array has 40 entries, of which about half are currently (April 2016) used.
!
! integer(kind=irg) :: ipar(40)  components 
! ipar(1) : nx  = (numsx-1)/2
! ipar(2) : globalworkgrpsz
! ipar(3) : num_el
! ipar(4) : totnum_el
! ipar(5) : multiplier
! ipar(6) : devid
! ipar(7) : platid
! ipar(8) : CrystalSystem
! ipar(9) : Natomtypes
! ipar(10): SpaceGroupNumber
! ipar(11): SpaceGroupSetting
! ipar(12): numEbins
! ipar(13): numzbins
! ipar(14): mcmode  ( 1 = 'full', 2 = 'bse1' )
! ipar(15): numangle
! ipar(16): nxten = nx/10
! the following are only used in the master routine
! ipar(17): npx
! ipar(18): nthreads
! the following are only used in the EBSD pattern routine
! ipar(19): numx of detector pixels
! ipar(20): numy of detector pixels
! ipar(21): number of orientation in quaternion set
! ipar(22): binning factor (0-3)
! ipar(23): binned x-dimension
! ipar(24): binned y-dimension
! ipar(25): anglemode  (0 for quaternions, 1 for Euler angles)
! ipar(26:40) : 0 (unused for now)

! real(kind=dbl) :: fpar(40)  components
! fpar(1) : sig
! fpar(2) : omega
! fpar(3) : EkeV
! fpar(4) : Ehistmin
! fpar(5) : Ebinsize
! fpar(6) : depthmax
! fpar(7) : depthstep
! fpar(8) : sigstart
! fpar(9) : sigend
! fpar(10): sigstep
! parameters only used in the master pattern routine
! fpar(11) : dmin
! fpar(12) : Bethe  c1
! fpar(13) : Bethe  c2
! fpar(14) : Bethe  c3
! parameters only used in the EBSD pattern routine
! fpar(15): pattern center x
! fpar(16): pattern center y
! fpar(17): scintillator pixel size
! fpar(18): detector tilt angle
! fpar(19): sample-scintillator distance
! fpar(20): beam current
! fpar(21): dwelltime
! fpar(22): gamma value
! fpar(23:40): 0 (unused for now)

! newly added in version 3.2, to facilitate passing EMsoft configuration
! strings back and forth to C/C++ programs that call EMdymod routines...
! character(fnlen)  :: spar(40)   configuration string components
! spar(1): EMsoftpathname
! spar(2): EMXtalFolderpathname
! spar(3): EMdatapathname
! spar(4): EMtmppathname
! spar(5): EMsoftLibraryLocation
! spar(6): EMSlackWebHookURL
! spar(7): EMSlackChannel
! spar(8): UserName
! spar(9): UserLocation
! spar(10): UserEmail
! spar(11): EMNotify
! spar(12): Develop
! spar(13): Release
! spar(14): h5copypath
! spar(15): EMsoftplatform
! spar(16): EMsofttestpath
! spar(17): EMsoftTestingPath
! spar(18): EMsoftversion
! spar(19): Configpath
! spar(20): Templatepathname
! spar(21): Resourcepathname
! spar(22): Homepathname
! spar(23): OpenCLpathname
! spar(24): Templatecodefilename
! spar(25): WyckoffPositionsfilename
! spar(26): Randomseedfilename
! spar(27): EMsoftnativedelimiter
! spar(28:40): '' (unused for now)


!
module EMdymod

use math

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:getEBSDPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This function can be called as a standalone function to compute an EBSD pattern
!
!> @etails The main purpose of this routine and its accompanying wrapper routine is to
!> provide a way for an external program to compute a channeling pattern.  The idea is that 
!> all the necessary arrays and variables are passed in by reference as arguments, without
!> the need for the routine to fetch any other data from files etc...  The initial goal is
!> to have a function that can be called with the CALL_EXTERNAL mechanism in IDL or MatLab.
!> This routine should be called via the getEBSDPatternsWrapper routine!  For calls from
!> a C/C++ program, use the EMsoftCgetEBSDPatterns routine in the EMsoftwrapperLib/EMSEMwrappermod instead.
!
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param EBSDpattern output array
!> @param quats quaternion input array
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 10/16/15 MDG 1.0 original
!> @date 11/02/15 MDG 1.1 simplification of the input variables
!> @date 11/04/15 MDG 1.2 added array of quaternions as input parameter; used complete mLPNH/SH arrays with local sum
!> @date 01/12/15 MDG 1.3 added arguments and functionality for interface with DREAM.3D and other calling programs
!> @date 01/13/15 MDG 1.4 after split with EMsoftCgetEBSDPatterns subroutine, removed DREAM.3D interfacing stuff
!> @date 07/10/16 MDG 1.5 added energy min/max parameters
!> @date 08/03/16 MDG 1.6 corrected normalizing issue in rgx,y,z arrays that causes NANs to be returned from Lambert projection routines
!> @date 08/25/16 MDG 1.7 added transfer optics barrel distortion to rgx,y,z arrays.
!> @date 02/19/19 MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine getEBSDPatterns(ipar, fpar, EBSDpattern, quats, accum_e, mLPNH, mLPSH)
!DEC$ ATTRIBUTES DLLEXPORT :: getEBSDPatterns

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structure to
! make this routine callable by external programs; for calls from  C/C++, use the EsoftCgetEBSDPatterns routine instead.  

! The following is the mapping for the ipar and fpar arrays:
!
! ipar(1) = 2 if rgx, rgy, rgz detector arrays need to be computed, 1 if not (arrays will have save status)
! ipar(2) = detnumsx
! ipar(3) = detnumsy
! ipar(4) = detnumEbins
! ipar(5) = mcnsx
! ipar(6) = mpnpx
! ipar(7) = numset
! ipar(8) = numquats
! ipar(9) = Eminsel
! ipar(10) = Emaxsel

! fpar(1) = enl%xpc
! fpar(2) = enl%ypc
! fpar(3) = enl%delta
! fpar(4) = enl%MCsig
! fpar(5) = enl%omega
! fpar(6) = enl%thetac
! fpar(7) = enl%L
! fpar(8) = enl%beamcurrent
! fpar(9) = enl%dwelltime
! fpar(10) = enl%alphaBD

use local
use constants
use Lambert
use quaternions
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(c_size_t),PARAMETER             :: nipar=10
integer(c_size_t),PARAMETER             :: nfpar=10
integer(c_size_t),PARAMETER             :: nq=4
integer(c_size_t),INTENT(IN)            :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
real(kind=sgl),INTENT(IN)               :: quats(nq,ipar(8))
real(kind=sgl),INTENT(IN)               :: accum_e(ipar(4),-ipar(5):ipar(5),-ipar(5):ipar(5))
real(kind=sgl),INTENT(IN)               :: mLPNH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(4), ipar(7))
real(kind=sgl),INTENT(IN)               :: mLPSH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(4), ipar(7))
real(kind=sgl),INTENT(OUT)              :: EBSDpattern(ipar(2),ipar(3),ipar(8))

! variables that must potentially be saved for the next time this function is called
real(kind=sgl),allocatable,save         :: accum_e_detector(:,:,:)
real(kind=sgl),allocatable,save         :: rgx(:,:), rgy(:,:), rgz(:,:)
real(kind=sgl),allocatable,save         :: mLPNHsum(:,:,:), mLPSHsum(:,:,:)
real(kind=sgl),save                     :: prefactor

! other variables
real(kind=sgl),allocatable              :: scin_x(:), scin_y(:)                 ! scintillator coordinate arrays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, pcxd, pcyd, xx, yy     ! distances
integer(kind=irg)                       :: nix, niy, binx, biny,  nixp, niyp, i, j, Emin, Emax, istat, k, ip, ipx, ipy, epl ! various parameters
real(kind=sgl)                          :: dc(3), scl, alpha, theta, gam, pcvec(3), dp, calpha           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                          :: ixy(2)
real(kind=dbl),parameter                :: nAmpere = 6.241D+18 

!====================================
! ------ generate the detector rgx, rgy, rgz arrays if needed (calling program must decide this via ipar(1))
!====================================
if (ipar(1).ge.1) then

 if ((ipar(1).eq.2).or.(.not.allocated(mLPNHsum))) then ! complete reset, including the mLPNHsum and mLPSHsum arrays
    if (allocated(mLPNHsum)) deallocate(mLPNHsum)
    if (allocated(mLPSHsum)) deallocate(mLPSHsum)

    allocate(mLPNHsum(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(4)))
    allocate(mLPSHsum(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(4)))
    mLPNHsum = sum(mLPNH,4)
    mLPSHsum = sum(mLPSH,4)

 end if

! This needs to be done only once for a given detector geometry (i.e., when ipar(1)=1 or larger)
  allocate(scin_x(ipar(2)),scin_y(ipar(3)),stat=istat)
  
  pcxd = fpar(1) * fpar(3)
  pcyd = fpar(2) * fpar(3)

  scin_x = - ( -fpar(1) - ( 1.0 - float(ipar(2)) ) * 0.5 - (/ (i-1, i=1,ipar(2)) /) ) * fpar(3)
  scin_y = ( fpar(2) - ( 1.0 - float(ipar(3)) ) * 0.5 - (/ (i-1, i=1,ipar(3)) /) ) * fpar(3)

! auxiliary angle to rotate between reference frames
  alp = 0.5 * cPi - (fpar(4) - fpar(6)) * dtor
  ca = cos(alp)
  sa = sin(alp)

  cw = cos(fpar(5) * dtor)
  sw = sin(fpar(5) * dtor)

! compute auxilliary interpolation arrays
  if (allocated(rgx)) deallocate(rgx, rgy, rgz)

  allocate(rgx(ipar(2),ipar(3)), rgy(ipar(2),ipar(3)), rgz(ipar(2),ipar(3)))

! do we need to perform a Barrel Distortion?
! we will do this here by expanding/contracting the radial component of the 
! (rgx, rgy) to (rgx,rgy) * (1+alphaBD * (rgx^2+rgy^2))
! in other words, we pre-distort the sampling grid with the barrel distortion.

epl = ipar(3)+1
  L2 = fpar(7) * fpar(7)
  do j=1,ipar(2)
    Ls = -sw * scin_x(j) + fpar(7) * cw
    Lc = cw * scin_x(j) + fpar(7) * sw
    do i=1,ipar(3)
!    rhos = 1.0/sqrt(sx + scin_y(i)**2)
     rgx(j,epl-i) = (scin_y(i) * ca + sa * Ls) ! * rhos
     rgy(j,epl-i) = Lc ! * rhos
     rgz(j,epl-i) = (-sa * scin_y(i) + ca * Ls) ! * rhos
! apply Barrel Distortion ?
     if (fpar(10).ne.0.0) then
! shift the components to the detector center coordinate frame
       xx = rgx(j,epl-i)-pcyd
       yy = rgy(j,epl-i)+pcxd
! compute the distortion amount; the factor of 10^(-10) is inserted here...
       sx = 1.0 + 1.E-10 * fpar(10) * (xx**2+yy**2) 
! and shift them back to the pattern center reference frame
       rgx(j,epl-i) = xx*sx+pcyd
       rgy(j,epl-i) = yy*sx-pcxd
     end if
! make sure that these vectors are normalized !
     x = sqrt(rgx(j,epl-i)**2+rgy(j,epl-i)**2+rgz(j,epl-i)**2)
     rgx(j,epl-i) = rgx(j,epl-i) / x
     rgy(j,epl-i) = rgy(j,epl-i) / x
     rgz(j,epl-i) = rgz(j,epl-i) / x
    end do
  end do

! test dump of rgx/y/z arrays to check for proper inclusion of barrel distortion:
!open(unit=dataunit,file='rgxyz.data',status='unknown',form='unformatted')
!write(dataunit) rgx
!write(dataunit) rgy
!write(dataunit) rgz
!close(unit=dataunit,status='keep')

! remove the auxiliary arrays scin_x and scin_y
  deallocate(scin_x, scin_y)

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(ipar(5)) 

! energy summation will go over all energy bins
  Emin = ipar(9)
  Emax = ipar(10)

  if (allocated(accum_e_detector)) deallocate(accum_e_detector)

  allocate(accum_e_detector(ipar(4),ipar(2),ipar(3)))

! correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(fpar(3)/fpar(7)/sqrt(sngl(cPi)))
  ipx = ipar(2)/2 + nint(fpar(1))
  ipy = ipar(3)/2 + nint(fpar(2))
  
  if ((abs(ipy).gt.ipar(3)).or.(abs(ipx).gt.ipar(2))) then 
    pcvec = (/pcyd*ca + pcxd*sa*sw + fpar(7)*cw*sa, &
             fpar(7)*sw - pcxd*cw,&
             fpar(7)*ca*cw + pcxd*ca*sw - pcyd*sa/)
    pcvec = pcvec/vecnorm(pcvec)
  else
    pcvec = (/ rgx(ipx,ipy), rgy(ipx,ipy), rgz(ipx,ipy) /)
  end if

  calpha = cos(alpha)
  do i=1,ipar(2)
    do j=1,ipar(3)
! do the coordinate transformation for this detector pixel
       dc = (/ rgx(i,j), rgy(i,j), rgz(i,j) /)

! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
       call LambertgetInterpolation(dc, scl, int(ipar(5)), int(ipar(5)), nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)

! do the area correction for this detector pixel
       dp = dot_product(pcvec,dc)
       if ((i.eq.ipx).and.(j.eq.ipy)) then
         gam = 0.25 
       else
         gam = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3) * 0.25
       end if

! interpolate the intensity 
        do k= Emin, Emax
          accum_e_detector(k,i,j) = gam * (accum_e(k,nix,niy) * dxm * dym + &
                                               accum_e(k,nixp,niy) * dx * dym + &
                                               accum_e(k,nix,niyp) * dxm * dy + &
                                               accum_e(k,nixp,niyp) * dx * dy)
        end do
    end do
  end do 
  prefactor = 0.25D0 * nAmpere * fpar(8) * fpar(9)  * 1.0D-15 / sum(accum_e_detector)
end if   ! end of ipar(1)=1 test

!open(unit=dataunit,file='TKDdetectorarray_dymod.data',status='unknown',form='unformatted')
!write(dataunit) accum_e_detector
!close(unit=dataunit,status='keep')

! from here on, we simply compute the EBSD patterns by interpolation, using the saved arrays from above
! no intensity scaling or anything else...other than multiplication by pre-factor
! intensity scaling is left to the user of the calling program.

! define some parameters and initialize EBSDpattern
scl = float(ipar(6)) 
EBSDpattern = 0.0

! here is the main loop over all quaternions
quatloop: do ip=1,ipar(8)
  do i=1,ipar(2)
    do j=1,ipar(3)
! do the active coordinate transformation for this euler angle
      dc = quat_Lp(quats(1:4,ip),  (/ rgx(i,j), rgy(i,j), rgz(i,j) /) )

! normalize dc
      dc = dc/sqrt(sum(dc*dc))

! convert these direction cosines to coordinates in the Rosca-Lambert projection (always square projection !!!)
      call LambertgetInterpolation(dc, scl, int(ipar(6)), int(ipar(6)), nix, niy, nixp, niyp, dx, dy, dxm, dym) 

      if (dc(3).gt.0.0) then ! we're in the Northern hemisphere
        do k=Emin,Emax
          EBSDpattern(i,j,ip) = EBSDpattern(i,j,ip) + accum_e_detector(k,i,j) * ( mLPNHsum(nix,niy,k) * dxm * dym +&
                                      mLPNHsum(nixp,niy,k) * dx * dym + mLPNHsum(nix,niyp,k) * dxm * dy + &
                                      mLPNHsum(nixp,niyp,k) * dx * dy )
        end do
      else                   ! we're in the Southern hemisphere
        do k=Emin,Emax 
          EBSDpattern(i,j,ip) = EBSDpattern(i,j,ip) + accum_e_detector(k,i,j) * ( mLPSHsum(nix,niy,k) * dxm * dym +&
                                      mLPSHsum(nixp,niy,k) * dx * dym + mLPSHsum(nix,niyp,k) * dxm * dy + &
                                      mLPSHsum(nixp,niyp,k) * dx * dy )
        end do
      end if
    end do
  end do
end do quatloop

! finally, scale the patterns by the appropriate factor and return to the calling program
EBSDpattern = prefactor * EBSDpattern

end subroutine getEBSDPatterns


!--------------------------------------------------------------------------
!
! SUBROUTINE:getEBSDPatterns2
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This function can be called as a standalone function to compute an EBSD pattern
!
!> @etails The main purpose of this routine and its accompanying wrapper routine is to
!> provide a way for an external program to compute a channeling pattern.  The idea is that 
!> all the necessary arrays and variables are passed in by reference as arguments, without
!> the need for the routine to fetch any other data from files etc...  The initial goal is
!> to have a function that can be called with the CALL_EXTERNAL mechanism in IDL or MatLab.
!> This routine should be called via the getEBSDPatternsWrapper routine!  For calls from
!> a C/C++ program, use the EMsoftCgetEBSDPatterns routine instead.  This routine is a slightly
!> modified version of the regular getEBSDPatterns routine and does not SAVE any variables.
!
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param EBSDpattern output array
!> @param quats quaternion input array
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 10/16/15 MDG 1.0 original
!> @date 11/02/15 MDG 1.1 simplification of the input variables
!> @date 11/04/15 MDG 1.2 added array of quaternions as input parameter; used complete mLPNH/SH arrays with local sum
!> @date 01/12/15 MDG 1.3 added arguments and functionality for interface with DREAM.3D and other calling programs
!> @date 01/13/15 MDG 1.4 after split with EMsoftCgetEBSDPatterns subroutine, removed DREAM.3D interfacing stuff
!> @date 07/10/16 MDG 1.5 added energy min/max parameters
!> @date 08/03/16 MDG 1.6 corrected normalizing issue in rgx,y,z arrays that causes NANs to be returned from Lambert projection routines
!> @date 08/25/16 MDG 1.7 added transfer optics barrel distortion to rgx,y,z arrays.
!> @date 04/24/17 MDG 1.8 forked from original routine without SAVEd variables
!> @date 02/19/19 MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine getEBSDPatterns2(ipar, fpar, EBSDpattern, quats, accum_e, mLPNHsum, mLPSHsum)
!DEC$ ATTRIBUTES DLLEXPORT :: getEBSDPatterns2

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structure to
! make this routine callable by external programs; for calls from  C/C++, use the EsoftCgetEBSDPatterns routine instead.  

! The following is the mapping for the ipar and fpar arrays:
!
! ipar(1) = 2 if rgx, rgy, rgz detector arrays need to be computed, 1 if not (arrays will have save status)
! ipar(2) = detnumsx
! ipar(3) = detnumsy
! ipar(4) = detnumEbins
! ipar(5) = mcnsx
! ipar(6) = mpnpx
! ipar(7) = numset
! ipar(8) = numquats
! ipar(9) = Eminsel
! ipar(10) = Emaxsel

! fpar(1) = enl%xpc
! fpar(2) = enl%ypc
! fpar(3) = enl%delta
! fpar(4) = enl%MCsig
! fpar(5) = enl%omega
! fpar(6) = enl%thetac
! fpar(7) = enl%L
! fpar(8) = enl%beamcurrent
! fpar(9) = enl%dwelltime
! fpar(10) = enl%alphaBD

use local
use constants
use Lambert
use quaternions
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(c_size_t),PARAMETER             :: nipar=10
integer(c_size_t),PARAMETER             :: nfpar=10
integer(c_size_t),PARAMETER             :: nq=4
integer(c_size_t),INTENT(IN)            :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
real(kind=sgl),INTENT(IN)               :: quats(nq,ipar(8))
real(kind=sgl),INTENT(IN)               :: accum_e(ipar(4),-ipar(5):ipar(5),-ipar(5):ipar(5))
real(kind=sgl),INTENT(IN)               :: mLPNHsum(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(4))
real(kind=sgl),INTENT(IN)               :: mLPSHsum(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(4))
real(kind=sgl),INTENT(OUT)              :: EBSDpattern(ipar(2),ipar(3),ipar(8))

! allocatable variables
real(kind=sgl),allocatable              :: accum_e_detector(:,:,:)
real(kind=sgl),allocatable              :: rgx(:,:), rgy(:,:), rgz(:,:)
real(kind=sgl),save                     :: prefactor

! other variables
real(kind=sgl),allocatable              :: scin_x(:), scin_y(:)                 ! scintillator coordinate arrays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, pcxd, pcyd, xx, yy     ! distances
integer(kind=irg)                       :: nix, niy, binx, biny,  nixp, niyp, i, j, Emin, Emax, istat, k, ip, ipx, ipy, elp ! various parameters
real(kind=sgl)                          :: dc(3), scl, alpha, theta, gam, pcvec(3), dp, calpha           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                          :: ixy(2)
real(kind=dbl),parameter                :: nAmpere = 6.241D+18 

!====================================
! ------ generate the detector rgx, rgy, rgz arrays 
!====================================

! This needs to be done only once for a given detector geometry (i.e., when ipar(1)=1 or larger)
  allocate(scin_x(ipar(2)),scin_y(ipar(3)),stat=istat)
  
  pcxd = fpar(1) * fpar(3)
  pcyd = fpar(2) * fpar(3)

  scin_x = - ( -fpar(1) - ( 1.0 - float(ipar(2)) ) * 0.5 - (/ (i-1, i=1,ipar(2)) /) ) * fpar(3)
  scin_y = ( fpar(2) - ( 1.0 - float(ipar(3)) ) * 0.5 - (/ (i-1, i=1,ipar(3)) /) ) * fpar(3)

! auxiliary angle to rotate between reference frames
  alp = 0.5 * cPi - (fpar(4) - fpar(6)) * dtor
  ca = cos(alp)
  sa = sin(alp)

  cw = cos(fpar(5) * dtor)
  sw = sin(fpar(5) * dtor)

! compute auxilliary interpolation arrays
  allocate(rgx(ipar(2),ipar(3)), rgy(ipar(2),ipar(3)), rgz(ipar(2),ipar(3)))

! do we need to perform a Barrel Distortion?
! we will do this here by expanding/contracting the radial component of the 
! (rgx, rgy) to (rgx,rgy) * (1+alphaBD * (rgx^2+rgy^2))
! in other words, we pre-distort the sampling grid with the barrel distortion.

  elp = ipar(3)+1
  L2 = fpar(7) * fpar(7)
  do j=1,ipar(2)
    Ls = -sw * scin_x(j) + fpar(7) * cw
    Lc = cw * scin_x(j) + fpar(7) * sw
    do i=1,ipar(3)
!    rhos = 1.0/sqrt(sx + scin_y(i)**2)
     rgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) ! * rhos
     rgy(j,elp-i) = Lc ! * rhos
     rgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) ! * rhos
! apply Barrel Distortion ?
     if (fpar(10).ne.0.0) then
! shift the components to the detector center coordinate frame
       xx = rgx(j,elp-i)-pcyd
       yy = rgy(j,elp-i)+pcxd
! compute the distortion amount; the factor of 10^(-10) is inserted here...
       sx = 1.0 + 1.E-10 * fpar(10) * (xx**2+yy**2) 
! and shift them back to the pattern center reference frame
       rgx(j,elp-i) = xx*sx+pcyd
       rgy(j,elp-i) = yy*sx-pcxd
     end if
! make sure that these vectors are normalized !
     x = sqrt(rgx(j,elp-i)**2+rgy(j,elp-i)**2+rgz(j,elp-i)**2)
     rgx(j,i) = rgx(j,elp-i) / x
     rgy(j,i) = rgy(j,elp-i) / x
     rgz(j,i) = rgz(j,elp-i) / x
    end do
  end do

! remove the auxiliary arrays scin_x and scin_y
  deallocate(scin_x, scin_y)

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(ipar(5)) 

! energy summation will go over all energy bins
  Emin = ipar(9)
  Emax = ipar(10)

  allocate(accum_e_detector(ipar(4),ipar(2),ipar(3)))

! correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(fpar(3)/fpar(7)/sqrt(sngl(cPi)))
  ipx = ipar(2)/2 + nint(fpar(1))
  ipy = ipar(3)/2 + nint(fpar(2))
  
  if (ipx .gt. ipar(2)) ipx = ipar(2)
  if (ipx .lt. 1) ipx = 1
  
  if (ipy .gt. ipar(3)) ipy = ipar(3)
  if (ipy .lt. 1) ipy = 1
  
  pcvec = (/ rgx(ipx,ipy), rgy(ipx,ipy), rgz(ipx,ipy) /)
  calpha = cos(alpha)
  do i=1,ipar(2)
    do j=1,ipar(3)
! do the coordinate transformation for this detector pixel
       dc = (/ rgx(i,j), rgy(i,j), rgz(i,j) /)

! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
       call LambertgetInterpolation(dc, scl, int(ipar(5)), int(ipar(5)), nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)

! do the area correction for this detector pixel
       dp = dot_product(pcvec,dc)
       if ((i.eq.ipx).and.(j.eq.ipy)) then
         gam = 0.25 
       else
         gam = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3) * 0.25
       end if

! interpolate the intensity 
       do k= Emin, Emax
         accum_e_detector(k,i,j) = gam * (accum_e(k,nix,niy) * dxm * dym + &
                                              accum_e(k,nixp,niy) * dx * dym + &
                                              accum_e(k,nix,niyp) * dxm * dy + &
                                              accum_e(k,nixp,niyp) * dx * dy)
       end do
    end do
  end do 
  prefactor = 0.25D0 * nAmpere * fpar(8) * fpar(9)  * 1.0D-15 / sum(accum_e_detector)

! from here on, we simply compute the EBSD patterns by interpolation, 
! no intensity scaling or anything else...other than multiplication by pre-factor
! intensity scaling is left to the user of the calling program.

! define some parameters and initialize EBSDpattern
scl = dble(ipar(6)) 
EBSDpattern = 0.0

! here is the main loop over all quaternions
quatloop: do ip=1,ipar(8)
  do i=1,ipar(2)
    do j=1,ipar(3)
! do the active coordinate transformation for this euler angle
      dc = quat_Lp(quats(1:4,ip),  (/ rgx(i,j), rgy(i,j), rgz(i,j) /) )
! normalize dc
      dc = dc/sqrt(sum(dc*dc))
! convert these direction cosines to coordinates in the Rosca-Lambert projection (always square projection !!!)
      call LambertgetInterpolation(dc, scl, int(ipar(6)), int(ipar(6)), nix, niy, nixp, niyp, dx, dy, dxm, dym)

      ixy = scl * LambertSphereToSquare( dc, istat )

      if (dc(3).gt.0.0) then ! we're in the Northern hemisphere
        do k=Emin,Emax
          EBSDpattern(i,j,ip) = EBSDpattern(i,j,ip) + accum_e_detector(k,i,j) * ( mLPNHsum(nix,niy,k) * dxm * dym +&
                                      mLPNHsum(nixp,niy,k) * dx * dym + mLPNHsum(nix,niyp,k) * dxm * dy + &
                                      mLPNHsum(nixp,niyp,k) * dx * dy )
        end do
      else                   ! we're in the Southern hemisphere
        do k=Emin,Emax 
          EBSDpattern(i,j,ip) = EBSDpattern(i,j,ip) + accum_e_detector(k,i,j) * ( mLPSHsum(nix,niy,k) * dxm * dym +&
                                      mLPSHsum(nixp,niy,k) * dx * dym + mLPSHsum(nix,niyp,k) * dxm * dy + &
                                      mLPSHsum(nixp,niyp,k) * dx * dy )
        end do
      end if
    end do
  end do
end do quatloop

deallocate(accum_e_detector)

! finally, scale the patterns by the appropriate factor and return to the calling program
EBSDpattern = prefactor * EBSDpattern

end subroutine getEBSDPatterns2


!--------------------------------------------------------------------------
!
! SUBROUTINE:getECPatterns
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief This function can be called as a standalone function to compute an electron channeling pattern
!> based on Marc's code above
!
!> @etails The main purpose of this routine and its accompanying wrapper routine is to
!> provide a way for an external program to compute an EC pattern.  The idea is that 
!> all the necessary arrays and variables are passed in by reference as arguments, without
!> the need for the routine to fetch any other data from files etc...  The initial goal is
!> to have a function that can be called with the CALL_EXTERNAL mechanism in IDL, but 
!> in the long run this will also be the approach for calling the routine from C/C++, which
!> is an essential part of integration with DREAM.3D.  This routine is a simplified version
!> of the core of the EMECP program. 
!>
!> This routine will first compute the incident cone vectors etc. if necessary, and then perform
!> the usual interpolation from the square Lambert projection. The pattern will be a basic pattern,
!> without any intensity scaling or binning etc; the calling program should take care of those 
!> operations.
!
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param ECPattern output array
!> @param quats array of quaternions
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 10/16/15  SS 1.0 original
!> @date 11/02/14 MDG 1.1 put all integer parameters inside ipar and fixed size of ipar/fpar
!> @date 11/04/15 MDG 1.2 added array of quaternions as input parameter
!--------------------------------------------------------------------------
recursive subroutine getECPatterns(ipar, fpar, ECpattern, quats, accum_e, mLPNH, mLPSH)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPatterns

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:
!
! ipar(1) = 1 if GetVectorsCone detector arrays need to be computed, 0 if not (arrays will have save status)
! ipar(2) = detnumsx
! ipar(3) = detnumsy
! ipar(4) = numangle
! ipar(5) = mcnsx
! ipar(6) = numset
! ipar(7) = mpnpx
! ipar(8) = numquats

! fpar(1) = ecpnl%thetac
! fpar(2) = ecpnl%sampletilt
! fpar(3) = ecpnl%workingdistance
! fpar(4) = ecpnl%Rin
! fpar(5) = ecpnl%Rout
! fpar(6) = ecpnl%sigstart
! fpar(7) = ecpnl%sigend
! fpar(8) = ecpnl%sigstep

!!!!!!!! removed:  fpar(9-12) =  quaternion for requested Euler angles

use local
use constants
use Lambert
use quaternions
use distortion
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(c_size_t),PARAMETER             :: nipar=8
integer(c_size_t),PARAMETER             :: nfpar=8
integer(c_size_t),PARAMETER             :: nq=4
integer(c_size_t),INTENT(IN)            :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
real(kind=sgl),INTENT(OUT)              :: ECpattern(ipar(2),ipar(3),ipar(8))
real(kind=sgl),INTENT(IN)               :: quats(nq,ipar(8))
real(kind=sgl),INTENT(IN)               :: accum_e(ipar(4),-ipar(5):ipar(5),-ipar(5):ipar(5))
real(kind=sgl),INTENT(IN)               :: mLPNH(-ipar(7):ipar(7), -ipar(7):ipar(7), ipar(6))
real(kind=sgl),INTENT(IN)               :: mLPSH(-ipar(7):ipar(7), -ipar(7):ipar(7), ipar(6))

real(kind=sgl),allocatable,save         :: klist(:,:,:), rgx(:,:), rgy(:,:), rgz(:,:), weightfact(:)
real(kind=sgl),allocatable,save         :: mLPNHsum(:,:), mLPSHsum(:,:)

real(kind=dbl),parameter                :: Rtod = 57.2957795131D0
real(kind=dbl),parameter                :: dtoR = 0.01745329251D0

real(kind=sgl)                          :: kk(3), thetacr, ktmax, delta, wf, quat(4)
integer(kind=irg)                       :: istat, imin, imax, jmin, jmax, ii ,jj, nazimuth, npolar, nsig, ip
integer(kind=irg)                       :: ipolar, iazimuth, isig, isampletilt, nix, niy, nixp, niyp, isigp
real(kind=sgl)                          :: thetain, thetaout, polar, azimuthal, delpolar, delazimuth, om(3,3)
real(kind=sgl)                          :: dc(3), scl, deltheta, acc_sum, MCangle, ixy(2), dx, dy, dxm, dym, dp


!==================================================================================
! ------ generate the detector klist, rgx, rgy, rgz, weightfactors arrays if needed 
!------- (calling program must decide this via ipar(1))
!==================================================================================

imin = 1
imax = ipar(2)
jmin = 1
jmax = ipar(3)

if (ipar(1).ge.1) then

  if (ipar(1).eq.2) then ! complete reset, including the mLPNHsum and mLPSHsum arrays
    if (allocated(mLPNHsum)) deallocate(mLPNHsum)
    if (allocated(mLPSHsum)) deallocate(mLPSHsum)

    allocate(mLPNHsum(-ipar(7):ipar(7), -ipar(7):ipar(7)))
    allocate(mLPSHsum(-ipar(7):ipar(7), -ipar(7):ipar(7)))
    mLPNHsum = sum(mLPNH,3)
    mLPSHsum = sum(mLPSH,3)
  end if

    if (allocated(klist)) deallocate(klist)
    allocate(klist(1:3,1:ipar(2),1:ipar(3)), stat=istat)
    kk = (/0.0,0.0,1.0/)
    thetacr = DtoR*fpar(1)
    ktmax = tan(thetacr)
    delta = 2.0*ktmax/dble(ipar(2)-1)
     
    do ii = imin, imax
        do jj = jmin, jmax
            klist(1:3,ii,jj) = (/-ktmax+delta*(ii-1),-ktmax+delta*(jj-1),0.0/) + kk(1:3)
            klist(1:3,ii,jj) =  klist(1:3,ii,jj)/sqrt(sum( klist(1:3,ii,jj)**2))
        end do
    end do

    thetain = atan2(fpar(4),fpar(3))
    thetaout = atan2(fpar(5),fpar(3))

    om(1,:) = (/cos(fpar(2)*sngl(dtor)),0.0,sin(fpar(2)*sngl(dtor))/)
    om(2,:) = (/0.0,1.0,0.0/)
    om(3,:) = (/-sin(fpar(2)*sngl(dtor)),0.0,cos(fpar(2)*sngl(dtor))/)

    npolar = nint((thetaout - thetain)*180.0/cPi) + 1
    delpolar = (thetaout - thetain)/float(npolar-1)

    nazimuth = 361
    delazimuth = 2.0*cPi/float(nazimuth-1)

    if (allocated(rgx)) deallocate(rgx, rgy, rgz)
    allocate(rgx(npolar, nazimuth), rgy(npolar, nazimuth), rgz(npolar, nazimuth), stat=istat)

    do ipolar = 1,npolar
         polar = thetain + float(ipolar-1)*delpolar

         do iazimuth = 1,nazimuth
             azimuthal = float(iazimuth-1)*delazimuth

             dc(1) = cos(azimuthal)*sin(polar)
             dc(2) = sin(azimuthal)*sin(polar)
             dc(3) = cos(polar)

             dc = matmul(om,dc)

             rgx(ipolar,iazimuth) = dc(1)
             rgy(ipolar,iazimuth) = dc(2)
             rgz(ipolar,iazimuth) = dc(3)
        end do
    end do

!===================================================================
! ------ generate the weight factors from the monte carlo histogram
!===================================================================

    scl = float(ipar(5))
    nsig = nint(fpar(1) + abs(fpar(2))) + 1

    deltheta = (fpar(1)+abs(fpar(2)))/float(nsig-1)

    if (allocated(weightfact)) deallocate(weightfact)
    allocate(weightfact(1:nsig), stat=istat)
    weightfact = 0.0

    do isig = 1,nsig
        acc_sum = 0.0
        MCangle = (isig - 1)*deltheta
        isampletilt = nint((MCangle - fpar(6))/fpar(8))
    
        if (isampletilt .lt. 1) then
            isampletilt = abs(isampletilt) + 1
        else
            isampletilt = isampletilt + 1
        end if

        do ipolar = 1,npolar
            do iazimuth = 1,nazimuth
                dc(1:3) = (/rgx(ipolar,iazimuth), rgy(ipolar,iazimuth), rgz(ipolar,iazimuth)/)

! convert to Rosca-lambert projection
                call LambertgetInterpolation(dc, scl, int(ipar(5)), int(ipar(5)), nix, niy, nixp, niyp, dx, dy, dxm, dym)

                acc_sum = 0.25*(accum_e(isampletilt,nix,niy) * dxm * dym + &
                                accum_e(isampletilt,nixp,niy) * dx * dym + &
                                accum_e(isampletilt,nix,niyp) * dxm * dy + &
                                accum_e(isampletilt,nixp,niyp) * dx * dy)
             
                weightfact(isig) = weightfact(isig) + acc_sum

            end do
        end do
    end do

    weightfact(1:nsig) = weightfact(1:nsig)/weightfact(1)

end if

!===================================================================
! ------ perform interpolation from square lambert map
!===================================================================
scl = float(ipar(7))

do ip=1,ipar(8)
  do ii = imin, imax
    do jj = jmin, jmax

        dc(1:3) = klist(1:3,ii,jj)
        dc = dc/sqrt(sum(dc*dc))
        
        dp = DOT_PRODUCT(dc(1:3),(/sin(fpar(2)*dtoR),0.D0,cos(fpar(2)*dtoR)/))      
        if (dp .gt. 1.D0) dp = 1.0
        MCangle = acos(dp)*Rtod
        isig = int(MCangle) + 1
        if (isig .gt. nsig) isig = nsig

        isigp = isig + 1
        if (isigp .gt. nsig) isigp = nsig
        dx = MCangle - int(MCangle)
        dxm =  1.0 - dx
        
        wf = weightfact(isig) * dxm + weightfact(isigp) * dx
        wf = 1.0
        dc = quat_LP(quats(1:4,ip), dc)
        dc = dc/sqrt(sum(dc*dc))

        call LambertgetInterpolation(dc, scl, int(ipar(7)), int(ipar(7)), nix, niy, nixp, niyp, dx, dy, dxm, dym)

        if (dc(3).gt.0.0) then 
            ECpattern(ii,jj,ip) = wf * ( mLPNHsum(nix,niy) * dxm * dym + &
                         mLPNHsum(nixp,niy) * dx * dym + &
                         mLPNHsum(nix,niyp) * dxm * dy + &
                         mLPNHsum(nixp,niyp) * dx * dy )

        else
            ECpattern(ii,jj,ip) =  wf * ( mLPSHsum(nix,niy) * dxm * dym + &
                         mLPSHsum(nixp,niy) * dx * dym + &
                         mLPSHsum(nix,niyp) * dxm * dy + &
                         mLPSHsum(nixp,niyp) * dx * dy )
        end if

    end do
  end do
end do


end subroutine getECPatterns

!--------------------------------------------------------------------------
!
! SUBROUTINE:getKosselPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This function can be called as a standalone function to compute a Kossel pattern
!
!> @etails The main purpose of this routine and its accompanying wrapper routine is to
!> provide a way for an external program to compute a Kossel pattern.  The idea is that 
!> all the necessary arrays and variables are passed in by reference as arguments, without
!> the need for the routine to fetch any other data from files etc...  The initial goal is
!> to have a function that can be called with the CALL_EXTERNAL mechanism in IDL, but 
!> in the long run this will also be the approach for calling the routine from C/C++, which
!> is an essential part of integration with DREAM.3D.  
!>
!> This routine will first compute the incident cone vectors etc. if necessary, and then perform
!> the usual interpolation from the square Lambert projection. The pattern will be a basic pattern,
!> without any intensity scaling or binning etc; the calling program should take care of those 
!> operations. This is simpler than the ECP case, since there is no energy dependent stuff to 
!> worry about. We're also keeping the ipar and fpar arrays the same as for the ECP case, even
!> though we could in principle simplify them; this facilitates integration with the SEMDisplay 
!> program.
!
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param KosselPattern output array
!> @param quats array of quaternions
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 11/09/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getKosselPatterns(ipar, fpar, Kosselpattern, quats, mLPNH, mLPSH)
!DEC$ ATTRIBUTES DLLEXPORT :: getKosselPatterns

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:
!
! ipar(1) = 1 if GetVectorsCone detector arrays need to be computed, 0 if not (arrays will have save status)
! ipar(2) = detnumsx
! ipar(3) = mpnpx
! ipar(4) = numquats
! ipar(5) = numdepths
! ipar(6) = depthsel

! fpar(1) = ecpnl%thetac

use local
use constants
use Lambert
use quaternions
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(c_size_t),PARAMETER             :: nipar=6
integer(c_size_t),PARAMETER             :: nfpar=1
integer(c_size_t),PARAMETER             :: nq=4
integer(c_size_t),INTENT(IN)            :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
real(kind=sgl),INTENT(OUT)              :: Kosselpattern(ipar(2),ipar(2),ipar(4))
real(kind=sgl),INTENT(IN)               :: quats(nq,ipar(4))
real(kind=sgl),INTENT(IN)               :: mLPNH(-ipar(3):ipar(3), -ipar(3):ipar(3),ipar(5))
real(kind=sgl),INTENT(IN)               :: mLPSH(-ipar(3):ipar(3), -ipar(3):ipar(3),ipar(5))

real(kind=sgl),allocatable,save         :: klist(:,:,:)

real(kind=dbl),parameter                :: Rtod = 57.2957795131D0
real(kind=dbl),parameter                :: dtoR = 0.01745329251D0

real(kind=sgl)                          :: kk(3), thetacr, ktmax, delta, quat(4)
integer(kind=irg)                       :: istat, imin, imax, jmin, jmax, ii ,jj, nsig, ip
integer(kind=irg)                       :: isig, nix, niy, nixp, niyp, isigp
real(kind=sgl)                          :: dc(3), scl, ixy(2), dx, dy, dxm, dym, dp


!==================================================================================
! ------ generate the detector klist array if needed 
!------- (calling program must decide this via ipar(1))
!==================================================================================

if (ipar(1).ge.1) then

    if (allocated(klist)) deallocate(klist)
    allocate(klist(1:3,-ipar(2):ipar(2),-ipar(2):ipar(2)), stat=istat)
    kk = (/0.0,0.0,1.0/)
    thetacr = DtoR*fpar(1)
    ktmax = tan(thetacr)
    delta = 2.0*ktmax/dble(ipar(2)-1)

    imin = 1
    imax = ipar(2)
    jmin = 1
    jmax = ipar(2)
     
    do ii = imin, imax
        do jj = jmin, jmax
            klist(1:3,ii,jj) = (/-ktmax+delta*(ii-1),-ktmax+delta*(jj-1),0.0/) + kk(1:3)
            klist(1:3,ii,jj) =  klist(1:3,ii,jj)/sqrt(sum( klist(1:3,ii,jj)**2))
        end do
    end do
end if

!===================================================================
! ------ perform interpolation from square lambert map
!===================================================================

scl = float(ipar(3))

do ip=1,ipar(4)
  do ii = imin, imax
    do jj = jmin, jmax

        dc(1:3) = klist(1:3,ii,jj)
        dc = quat_LP(quats(1:4,ip), dc)
        dc = dc/sqrt(sum(dc*dc))

        call LambertgetInterpolation(dc, scl, int(ipar(3)), int(ipar(3)), nix, niy, nixp, niyp, dx, dy, dxm, dym)

        if (dc(3).gt.0.0) then 
            Kosselpattern(ii,jj,ip) =  mLPNH(nix,niy,ipar(6)) * dxm * dym + &
                         mLPNH(nixp,niy,ipar(6)) * dx * dym + &
                         mLPNH(nix,niyp,ipar(6)) * dxm * dy + &
                         mLPNH(nixp,niyp,ipar(6)) * dx * dy 

        else
            Kosselpattern(ii,jj,ip) =  mLPSH(nix,niy,ipar(6)) * dxm * dym + &
                         mLPSH(nixp,niy,ipar(6)) * dx * dym + &
                         mLPSH(nix,niyp,ipar(6)) * dxm * dy + &
                         mLPSH(nixp,niyp,ipar(6)) * dx * dy 
        end if
    end do
  end do
end do

end subroutine getKosselPatterns

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSD4calfun
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief This function is used by bobyqa to fit an EBSD pattern
!
!> @details The main purpose of this routine is to calculte the difference of 1 with the dot
!> product of an experimental pattern with the given set of detector parameters. This is used
!> by bobyqa module to fit an EBSD pattern when 4 patterns are fitted simultaneously
!>
!> This routine will first compute the detector arrays rgx etc. if necessary, and then perform
!> the usual interpolation from the square Lambert projection. The pattern will be a basic pattern,
!> without any intensity scaling or binning etc; the calling program should take care of those 
!> operations.
!
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
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

recursive subroutine EBSD4calfun(nipar, nfpar, ninit, ipar, fpar, initmeanval, expt, accum_e, &
                                mLPNH, mLPSH, n, x, f, expt2, expt3, expt4, nstep, stepsize, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSD4calfun

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:
!

! ipar(1) = 2
! ipar(2) = detnumsx
! ipar(3) = detnumsy
! ipar(4) = detnumEbins
! ipar(5) = mcnsx
! ipar(6) = mpnpx
! ipar(7) = numset
! ipar(8) = numquats
! ipar(9) = Emin
! ipar(10) = Emax
! ipar(11) = 0/1 ;0 for no mask, 1 for mask
! ipar(12) = binning
! ipar(13) = pixx_pat1
! ipar(14) = pixy_pat1
! ipar(15) = pixx_pat2
! ipar(16) = pixy_pat2
! ipar(17) = pixx_pat3
! ipar(18) = pixy_pat3
! ipar(19) = pixx_pat4
! ipar(20) = pixy_pat4

! fpar(1) = enl%xpc
! fpar(2) = enl%ypc
! fpar(3) = enl%delta
! fpar(4) = enl%MCsig
! fpar(5) = enl%omega
! fpar(6) = enl%thetac
! fpar(7) = enl%L
! fpar(8) = enl%beamcurrent
! fpar(9) = enl%dwelltime
! fpar(10) = enl%gammavalue
! fpar(11) = maskradius
! fpar(12) = stepx
! fpar(13) = stepy

! initmeanval(1) = fpar(7)
! initmeanval(2) = phi1
! initmeanval(3) = phi
! initmeanval(4) = phi2
! initmeanval(5) = xpc
! initmeanval(6) = ypc

! stepsize(1) = step_xpc
! stepsize(2) = step_ypc
! stepsize(3) = step_phi1 ; all 4 patterns
! stepsize(4) = step_phi ; all 4 patterns
! stepsize(5) = step_phi2 ; all 4 patterns
! stepsize(6) = step_L

! X = (/xpc, ypc, omega, L, phi1, phi, phi2/)

            
use local
use rotations
use constants
use distortion
use filters
use math, ONLY:Jaccard_Distance
use,INTRINSIC :: ISO_C_BINDING
           
implicit none

integer(irg),intent(in)                 :: nipar
integer(irg),intent(in)                 :: nfpar
integer(irg),intent(in)                 :: ninit         
integer(c_size_t),intent(in)            :: ipar(nipar)
real(sgl),intent(inout)                 :: fpar(nfpar)
real(sgl),intent(in)                    :: initmeanval(ninit)
integer(irg),intent(in)                 :: nstep
real(sgl),intent(in)                    :: stepsize(nstep)
real(c_float),intent(in)                :: expt(ipar(2)*ipar(3)/ipar(12)/ipar(12))
real(c_float),intent(in)                :: expt2(ipar(2)*ipar(3)/ipar(12)/ipar(12))
real(c_float),intent(in)                :: expt3(ipar(2)*ipar(3)/ipar(12)/ipar(12))
real(c_float),intent(in)                :: expt4(ipar(2)*ipar(3)/ipar(12)/ipar(12))
real(kind=sgl),INTENT(in)               :: accum_e(ipar(4),-ipar(5):ipar(5),-ipar(5):ipar(5))
real(kind=sgl),INTENT(in)               :: mLPNH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(10), ipar(7))
real(kind=sgl),INTENT(in)               :: mLPSH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(10), ipar(7))
integer(irg),intent(in)                 :: n
real(dbl),dimension(:),intent(in)       :: x
real(dbl),intent(out)                   :: f
logical,intent(in),optional             :: verbose

integer(kind=irg)                       :: nnx, nny, binx, biny
complex(dbl)                            :: D
real(kind=sgl)                          :: quats(4,1), bindx, ma, mi
real(kind=sgl),allocatable              :: EBSDpattern(:,:,:), binned(:,:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:)

! variables that must be saved for the next time this function is called
real(kind=sgl)                          :: prefactor
integer(kind=irg),allocatable           :: img1(:), img2(:), img_fit_cumul(:), img_expt_cumul(:)
! other variables
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: ixy(2), eu(3), eu2(3), eu3(3), eu4(3)
real(kind=sgl), allocatable             :: EBSDvector(:), EBSDflip(:,:), mask(:,:)
integer(kind=irg)                       :: i, j, istat

logical                                 :: stat, readonly
integer(kind=irg)                       :: hdferr, nlines
real(kind=sgl)                          :: fpar2(nfpar)

!fpar(1) = sngl(X(1))*ipar(2) - ipar(2)/2 + initmeanval(5) ! xpc +/- detnumx/2 pixels
!fpar(2) = sngl(X(2))*ipar(3) - ipar(3)/2 + initmeanval(6) ! ypc +/- detnumy/2 pixels

fpar(1) = sngl(X(1))*2.0*stepsize(1) - stepsize(1) + initmeanval(5) ! xpc +/- 5 pixels
fpar(2) = sngl(X(2))*2.0*stepsize(2) - stepsize(2) + initmeanval(6) ! ypc +/- 5 pixels

!fpar(1) = initmeanval(5) ! xpc +/- detnumx/2 pixels
!fpar(2) = initmeanval(6) ! ypc +/- detnumy/2 pixels

fpar(7) = sngl(X(4))*2.0*stepsize(6)*fpar(3) - stepsize(6)*fpar(3) + initmeanval(1) ! mean +/- 5 pixels
!fpar(7) = initmeanval(1) ! mean +/- 2000 microns

! 03/28/16 omega is no longer a variable parameter anymore
fpar(5) = sngl(X(3))*0.0 - 0.0 ! omega 0 +/- 5 degrees

!eu = (/initmeanval(2), initmeanval(3), initmeanval(4)/)*dtor ! don't change the values for euler angles
eu = (/X(5)*2.0*stepsize(3) - stepsize(3) + initmeanval(2), X(6)*2.0*stepsize(4) - stepsize(4)  + initmeanval(3), &
       X(7)*2.0*stepsize(5) - stepsize(5) + initmeanval(4)/)*dtor ! mean +/- 2 degrees
eu2 = (/X(8)*2.0*stepsize(3) - stepsize(3) + initmeanval(7), X(9)*2.0*stepsize(4) - stepsize(4) + initmeanval(8), &
       X(10)*2.0*stepsize(5) - stepsize(5) + initmeanval(9)/)*dtor ! mean +/- 2 degrees
eu3 = (/X(11)*2.0*stepsize(3) - stepsize(3) + initmeanval(10), X(12)*2.0*stepsize(4) - stepsize(4) + initmeanval(11), &
       X(13)*2.0*stepsize(5) - stepsize(5) + initmeanval(12)/)*dtor ! mean +/- 2 degrees
eu4 = (/X(14)*2.0*stepsize(3) - stepsize(3) + initmeanval(13), X(15)*2.0*stepsize(4) - stepsize(4) + initmeanval(14), &
       X(16)*2.0*stepsize(5) - stepsize(5) + initmeanval(15)/)*dtor ! mean +/- 2 degrees

!D = cmplx(X(8)*0.000002D0 - 0.000001D0 + dble(initmeanval(5)), X(9)*0.000002D0 - 0.000001D0 + dble(initmeanval(6)))

binx = ipar(2)/ipar(12)
biny = ipar(3)/ipar(12)
bindx = 1.0/float(ipar(12)**2)

allocate(EBSDvector(binx*biny),mask(binx,biny))
allocate(EBSDpattern(ipar(2),ipar(3),1))
allocate(binned(binx,biny))
allocate(EBSDpatternintd(ipar(2),ipar(3)),EBSDpatterninteger(ipar(2),ipar(3)), EBSDpatternad(ipar(2),ipar(3)))
allocate(img1(binx*biny),img2(binx*biny))
allocate(img_fit_cumul(4*binx*biny),img_expt_cumul(4*binx*biny))

binned = 0.0
EBSDpatternintd = 0.0
EBSDpatterninteger = 0
EBSDpatternad = 0
img1 = 0
img2 = 0
img_fit_cumul = 0
img_expt_cumul = 0

mask = 1.0

if (present(verbose)) then
    if(verbose) then    
        print*,'xpc, ypc, L, eu_pat1, eu_pat2, eu_pat3, eu_pat4 = ', fpar(1), fpar(2), fpar(7),eu(1:3)*180.0/cPi,&
        eu2(1:3)*180.0/cPi,eu3(1:3)*180.0/cPi,eu4(1:3)*180.0/cPi
    end if
end if

fpar2(1:nfpar) = fpar(1:nfpar)

!==============================================================================
!============IMAGE 1===========================================================
!==============================================================================

quats(1:4,1) = eu2qu(eu)

fpar2(1:nfpar) = fpar(1:nfpar)
fpar2(1) = fpar(1) + ipar(13)*fpar(12)/fpar(3)
fpar2(2) = fpar(2) - ipar(14)*fpar(13)/fpar(3)

call getEBSDPatterns(ipar, fpar2, EBSDpattern, quats, accum_e, mLPNH, mLPSH)

!nnx = ipar(2)
!nny = ipar(3)

!EBSDpatternintd = ((EBSDPattern(:,:,1) - mi)/ (ma-mi))
!EBSDpatterninteger = nint(EBSDpatternintd*255.0)
!EBSDpatternad =  adhisteq(10,nnx,nny,EBSDpatterninteger)
!EBSDPattern(:,:,1) = float(EBSDpatternad)

if (ipar(12) .ne. 1) then
    do i=1,binx
        do j=1,biny
            binned(i,j) = sum(EBSDpattern((i-1)*ipar(12)+1:i*ipar(12),(j-1)*ipar(12)+1:j*ipar(12),1))
        end do
    end do 
    binned = binned * bindx 
else
    binned(1:binx,1:biny) = EBSDpattern(1:binx,1:biny,1)
end if


if (ipar(11) .eq. 1) then
    do i = 1,binx
        do j = 1,biny
            if(((float(i)-ceiling(float(binx)/2.0))**2 + (float(j)-ceiling(float(biny)/2.0))**2) .gt. fpar(11)**2) then
                mask(i,j) = 0.0
            end if
        end do
    end do
end if

binned(1:binx,1:biny) = binned(1:binx,1:biny)*mask(1:binx,1:biny)
binned = binned**fpar(10)


do i=1,biny
    do j=1,binx
        EBSDvector((i-1)*binx+j) = binned(j,i)
    end do
end do

ma = maxval(EBSDvector)
mi = minval(EBSDvector)

img1 = nint(255.0*(EBSDvector - mi)/(ma - mi))

ma = maxval(expt)
mi = minval(expt)

img2 = nint(255.0*(expt - mi)/(ma - mi))

img_fit_cumul(1:binx*biny) = img1(1:binx*biny)
img_expt_cumul(1:binx*biny) = img2(1:binx*biny)

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


!==============================================================================
!============IMAGE 2===========================================================
!==============================================================================

quats(1:4,1) = eu2qu(eu2)
fpar2(1:nfpar) = fpar(1:nfpar)
fpar2(1) = fpar(1) + ipar(15)*fpar(12)/fpar(3)
fpar2(2) = fpar(2) - ipar(16)*fpar(13)/fpar(3)


call getEBSDPatterns(ipar, fpar2, EBSDpattern, quats, accum_e, mLPNH, mLPSH)

if (ipar(12) .ne. 1) then
    do i=1,binx
        do j=1,biny
            binned(i,j) = sum(EBSDpattern((i-1)*ipar(12)+1:i*ipar(12),(j-1)*ipar(12)+1:j*ipar(12),1))
        end do
    end do 
    binned = binned * bindx 
else
    binned(1:binx,1:biny) = EBSDpattern(1:binx,1:biny,1)
end if

binned(1:binx,1:biny) = binned(1:binx,1:biny)*mask(1:binx,1:biny)
binned = binned**fpar(10)


do i=1,biny
    do j=1,binx
        EBSDvector((i-1)*binx+j) = binned(j,i)
    end do
end do

ma = maxval(EBSDvector)
mi = minval(EBSDvector)

img1 = nint(255.0*(EBSDvector - mi)/(ma - mi))

ma = maxval(expt2)
mi = minval(expt2)

img2 = nint(255.0*(expt2 - mi)/(ma - mi))

img_fit_cumul(binx*biny+1:2*binx*biny) = img1(1:binx*biny)
img_expt_cumul(binx*biny+1:2*binx*biny) = img2(1:binx*biny)

!==============================================================================
!============IMAGE 3===========================================================
!==============================================================================

quats(1:4,1) = eu2qu(eu3)
fpar2(1:nfpar) = fpar(1:nfpar)
fpar2(1) = fpar(1) + ipar(17)*fpar(12)/fpar(3)
fpar2(2) = fpar(2) - ipar(18)*fpar(13)/fpar(3)


call getEBSDPatterns(ipar, fpar2, EBSDpattern, quats, accum_e, mLPNH, mLPSH)

if (ipar(12) .ne. 1) then
    do i=1,binx
        do j=1,biny
            binned(i,j) = sum(EBSDpattern((i-1)*ipar(12)+1:i*ipar(12),(j-1)*ipar(12)+1:j*ipar(12),1))
        end do
    end do 
    binned = binned * bindx 
else
    binned(1:binx,1:biny) = EBSDpattern(1:binx,1:biny,1)
end if

binned(1:binx,1:biny) = binned(1:binx,1:biny)*mask(1:binx,1:biny)
binned = binned**fpar(10)


do i=1,biny
    do j=1,binx
        EBSDvector((i-1)*binx+j) = binned(j,i)
    end do
end do

ma = maxval(EBSDvector)
mi = minval(EBSDvector)

img1 = nint(255.0*(EBSDvector - mi)/(ma - mi))

ma = maxval(expt3)
mi = minval(expt3)

img2 = nint(255.0*(expt3 - mi)/(ma - mi))

img_fit_cumul(2*binx*biny+1:3*binx*biny) = img1(1:binx*biny)
img_expt_cumul(2*binx*biny+1:3*binx*biny) = img2(1:binx*biny)

!==============================================================================
!============IMAGE 4===========================================================
!==============================================================================

quats(1:4,1) = eu2qu(eu4)
fpar2(1:nfpar) = fpar(1:nfpar)
fpar2(1) = fpar(1) + ipar(19)*fpar(12)/fpar(3)
fpar2(2) = fpar(2) - ipar(20)*fpar(13)/fpar(3)

call getEBSDPatterns(ipar, fpar2, EBSDpattern, quats, accum_e, mLPNH, mLPSH)

if (ipar(12) .ne. 1) then
    do i=1,binx
        do j=1,biny
            binned(i,j) = sum(EBSDpattern((i-1)*ipar(12)+1:i*ipar(12),(j-1)*ipar(12)+1:j*ipar(12),1))
        end do
    end do 
    binned = binned * bindx 
else
    binned(1:binx,1:biny) = EBSDpattern(1:binx,1:biny,1)
end if

binned(1:binx,1:biny) = binned(1:binx,1:biny)*mask(1:binx,1:biny)
binned = binned**fpar(10)


do i=1,biny
    do j=1,binx
        EBSDvector((i-1)*binx+j) = binned(j,i)
    end do
end do

ma = maxval(EBSDvector)
mi = minval(EBSDvector)

img1 = nint(255.0*(EBSDvector - mi)/(ma - mi))

ma = maxval(expt4)
mi = minval(expt4)

img2 = nint(255.0*(expt4 - mi)/(ma - mi))

img_fit_cumul(3*binx*biny+1:4*binx*biny) = img1(1:binx*biny)
img_expt_cumul(3*binx*biny+1:4*binx*biny) = img2(1:binx*biny)

F = Jaccard_Distance(img_fit_cumul,img_expt_cumul,4*binx*biny)

!F = 1.0 - DOT_PRODUCT(EBSDvector,expt)

end subroutine EBSD4calfun

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDcalfun
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
!> @param fpar array with float input parameters
!> @param initmeanval mean value of search space
!> @param EBSDpattern output array
!> @param quats quaternion input array
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 12/12/15 SS 1.0 original
!> @date 03/28/16 SS 1.1 omega is no longer a variable parameter
!> @date 03/18/18 SS 1.2 refinement in homochoric space
!--------------------------------------------------------------------------

recursive subroutine EBSDcalfun(nipar, nfpar, ninit, ipar, fpar, initmeanval, expt, accum_e, &
                                mLPNH, mLPSH, n, x, f, nstep, stepsize, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDcalfun

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:
!

! ipar(1) = 2
! ipar(2) = detnumsx
! ipar(3) = detnumsy
! ipar(4) = detnumEbins
! ipar(5) = mcnsx
! ipar(6) = mpnpx
! ipar(7) = numset
! ipar(8) = numquats
! ipar(9) = Emin
! ipar(10) = Emax
! ipar(11) = 0/1 ;0 for no mask, 1 for mask
! ipar(12) = binning
! ipar(13) = 0/1; 0 for DP and 1 for JD
! ipar(14) = nregions

! fpar(1) = enl%xpc
! fpar(2) = enl%ypc
! fpar(3) = enl%delta
! fpar(4) = enl%MCsig
! fpar(5) = enl%omega
! fpar(6) = enl%thetac
! fpar(7) = enl%L
! fpar(8) = enl%beamcurrent
! fpar(9) = enl%dwelltime
! fpar(10) = alphaBD ; barrell distortion coefficient
! fpar(11) = maskradius
! fpar(12) = enl%gammavalue

! initmeanval(1) = fpar(7)
! initmeanval(2) = phi1
! initmeanval(3) = phi
! initmeanval(4) = phi2
! initmeanval(5) = xpc
! initmeanval(6) = ypc

! stepsize(1) = step_xpc
! stepsize(2) = step_ypc
! stepsize(3) = step_phi1 
! stepsize(4) = step_phi 
! stepsize(5) = step_phi2 
! stepsize(6) = step_L

! X = (/xpc, ypc, omega, L, phi1, phi, phi2/)

            
use local
use rotations
use constants
use distortion
use filters
use math, ONLY:Jaccard_Distance
use,INTRINSIC :: ISO_C_BINDING
           
implicit none

integer(irg),intent(in)                 :: nipar
integer(irg),intent(in)                 :: nfpar
integer(irg),intent(in)                 :: ninit         
integer(c_size_t),intent(in)            :: ipar(nipar)
real(sgl),intent(inout)                 :: fpar(nfpar)
real(sgl),intent(in)                    :: initmeanval(ninit)
integer(irg),intent(in)                 :: nstep
real(sgl),intent(in)                    :: stepsize(nstep)
real(c_float),intent(in)                :: expt(ipar(2)*ipar(3)/ipar(12)**2)
real(kind=sgl),INTENT(in)               :: accum_e(ipar(4),-ipar(5):ipar(5),-ipar(5):ipar(5))
real(kind=sgl),INTENT(in)               :: mLPNH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(10), ipar(7))
real(kind=sgl),INTENT(in)               :: mLPSH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(10), ipar(7))
integer(irg),intent(in)                 :: n
real(dbl),dimension(:),intent(in)       :: x
real(dbl),intent(out)                   :: f
logical,intent(in),optional             :: verbose

integer(kind=irg)                       :: nnx, nny, binx, biny
complex(dbl)                            :: D
real(kind=sgl)                          :: quats(4,1), bindx, ma, mi
real(kind=sgl),allocatable              :: EBSDpattern(:,:,:), binned(:,:)
real(kind=sgl),allocatable              :: EBSDpatternintd(:,:)
integer(kind=irg),allocatable           :: EBSDpatterninteger(:,:), EBSDpatternad(:,:)

! variables that must be saved for the next time this function is called
real(kind=sgl)                          :: prefactor
integer(kind=irg),allocatable           :: img1(:), img2(:)
! other variables
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: ixy(2), eu(3), ho(3)
real(kind=sgl), allocatable             :: EBSDvector(:), EBSDflip(:,:), mask(:,:)
integer(kind=irg)                       :: i, j, istat

logical                                 :: stat, readonly
integer(kind=irg)                       :: hdferr, nlines, nregions
real(kind=sgl)                          :: fpar2(10)
integer(kind=8)                         :: ipar2(10)

fpar(1) = sngl(X(1))*2.0*stepsize(1) - stepsize(1) + initmeanval(5) ! xpc +/- 5 pixels
fpar(2) = sngl(X(2))*2.0*stepsize(2) - stepsize(2) + initmeanval(6) ! ypc +/- 5 pixels


fpar(7) = sngl(X(4))*2.0*stepsize(6)*fpar(3) - stepsize(6)*fpar(3) + initmeanval(1) ! mean +/- 5 pixels

! 03/28/16 omega is no longer a variable parameter anymore
fpar(5) = sngl(X(3))*0.0 - 0.0 

!eu = (/X(5)*2.0*stepsize(3) - stepsize(3) + initmeanval(2), X(6)*2.0*stepsize(4) - stepsize(4)  + initmeanval(3), &
!       X(7)*2.0*stepsize(5) - stepsize(5) + initmeanval(4)/)*dtor ! mean +/- 2 degrees

ho = (/X(5)*2.0*stepsize(3) - stepsize(3) + initmeanval(2), X(6)*2.0*stepsize(4) - stepsize(4)  + initmeanval(3), &
       X(7)*2.0*stepsize(5) - stepsize(5) + initmeanval(4)/)

binx = ipar(2)/ipar(12)
biny = ipar(3)/ipar(12)
bindx = 1.0/float(ipar(12)**2)
nnx = ipar(2)
nny = ipar(3)
nregions = IPAR(14)

allocate(EBSDvector(binx*biny),mask(binx,biny))
allocate(EBSDpattern(ipar(2),ipar(3),1))
allocate(binned(binx,biny))
allocate(EBSDpatternintd(ipar(2),ipar(3)),EBSDpatterninteger(ipar(2),ipar(3)), EBSDpatternad(ipar(2),ipar(3)))
allocate(img1(binx*biny),img2(binx*biny))

binned = 0.0
EBSDpatternintd = 0.0
EBSDpatterninteger = 0
EBSDpatternad = 0
img1 = 0
img2 = 0

mask = 1.0

if (present(verbose)) then
    if(verbose) then
        eu = ho2eu(ho) * 180.0/cPi    
        print*,'xpc, ypc, L, eu = ', fpar(1), fpar(2), fpar(7), eu(1:3)
     end if
end if

fpar2(1:10)   = fpar(1:10)
ipar2(1:10)   = ipar(1:10)
quats(1:4,1)  = ho2qu(ho) 

call getEBSDPatterns(ipar2, fpar2, EBSDpattern, quats, accum_e, mLPNH, mLPSH)

ma = maxval(EBSDPattern(:,:,1))
mi = minval(EBSDPattern(:,:,1))

EBSDpatternintd = ((EBSDPattern(:,:,1) - mi)/ (ma-mi))
EBSDpatterninteger = nint(EBSDpatternintd*255.0)
EBSDpatternad =  adhisteq(nregions,nnx,nny,EBSDpatterninteger)
EBSDPattern(:,:,1) = float(EBSDpatternad)

if (ipar(12) .ne. 1) then
    do i=1,binx
        do j=1,biny
            binned(i,j) = sum(EBSDpattern((i-1)*ipar(12)+1:i*ipar(12),(j-1)*ipar(12)+1:j*ipar(12),1))
        end do
    end do 
    binned = binned * bindx 
else
    binned(1:binx,1:biny) = EBSDpattern(1:binx,1:biny,1)
end if


if (ipar(11) .eq. 1) then
    do i = 1,binx
        do j = 1,biny
            if(((float(i)-ceiling(float(binx)/2.0))**2 + (float(j)-ceiling(float(biny)/2.0))**2) .gt. fpar(11)**2) then
                mask(i,j) = 0.0
            end if
        end do
    end do
end if

binned(1:binx,1:biny) = binned(1:binx,1:biny)*mask(1:binx,1:biny)
binned = binned**fpar(12)

do i=1,biny
    do j=1,binx
        EBSDvector((i-1)*binx+j) = binned(j,i)
    end do
end do
EBSDvector = EBSDvector/vecnorm(EBSDvector)

if(IPAR(13) .eq. 1) then
    ma = maxval(EBSDvector)
    mi = minval(EBSDvector)

    img1 = nint(255.0*(EBSDvector - mi)/(ma - mi))

    ma = maxval(expt)
    mi = minval(expt)

    img2 = nint(255.0*(expt - mi)/(ma - mi))

    F = Jaccard_Distance(img1,img2,binx*biny)
else
    F = 1.0 - DOT_PRODUCT(EBSDvector,expt)
end if

end subroutine EBSDcalfun


!--------------------------------------------------------------------------
!
! SUBROUTINE:ECPcalfun
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief This function is used by bobyqa to fit an EBSD pattern
!
!> @etails The main purpose of this routine is to calculte the difference of 1 with the dot
!> product of an experimental pattern with the given set of detector parameters. This is used
!> by bobyqa module to fit an EBSD pattern.
!>
!> This routine will first compute the detector arrays rgx etc. if necessary, and then perform
!> the usual interpolation from the square Lambert projection. The pattern will be a basic pattern,
!> without any intensity scaling or binning etc; the calling program should take care of those 
!> operations.
!
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param initmeanval array with mean value of search space
!> @param ECPattern output array
!> @param quats array of quaternions
!> @param accum_e array with Monte Carlo histogram
!> @param mLPNH Northern hemisphere master pattern
!> @param mLPSH Southern hemisphere master pattern
!
!> @date 12/12/15  SS 1.0 original
!--------------------------------------------------------------------------

recursive subroutine ECPcalfun (nipar, nfpar, ninit, ipar, fpar, initmeanval, expt, accum_e, mLPNH, mLPSH, &
                                n, x, f, nstep, stepsize, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: ECPcalfun

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:
!
! ipar(1) = 1 
! ipar(2) = detnumsx
! ipar(3) = detnumsy
! ipar(4) = numangle
! ipar(5) = mcnsx
! ipar(6) = mpnpx
! ipar(7) = numset
! ipar(8) = numquats
! ipar(9) = 0/1 ;0 for no mask, 1 for mask
! ipar(10) = 1; equal to numEbins
! ipar(11) = 0/1; 0 for DP 1 for JD
! ipar(12) = 1 ;binning

! fpar(1) = ecpnl%thetac
! fpar(2) = ecpnl%sampletilt
! fpar(3) = ecpnl%workingdistance
! fpar(4) = ecpnl%Rin
! fpar(5) = ecpnl%Rout
! fpar(6) = ecpnl%sigstart
! fpar(7) = ecpnl%sigend
! fpar(8) = ecpnl%sigstep
! fpar(9) = ecpnl%gammavalue
! fpar(10) = maskradius

! initmeanval(1) = thetac
! initmeanval(2) = sampletilt
! initmeanval(3) = working distance 
! initmeanval(4) = phi1
! initmeanval(5) = phi
! initmeanval(6) = phi2

! stepsize(1) = step_thetacone
! stepsize(2) = step_phi1
! stepsize(3) = step_phi ; all 4 patterns
! stepsize(4) = step_phi2 ; all 4 patterns


use local
use rotations
use constants
use distortion 
use,INTRINSIC :: ISO_C_BINDING
use filters
use math, ONLY:Jaccard_Distance

IMPLICIT NONE

integer(irg),intent(in)                 :: nipar
integer(irg),intent(in)                 :: nfpar
integer(irg),intent(in)                 :: ninit
integer(c_size_t),intent(in)            :: ipar(nipar)
real(sgl),intent(inout)                 :: fpar(nfpar)
real(sgl),intent(in)                    :: initmeanval(ninit)
real(c_float),intent(in)                :: expt(ipar(2)*ipar(3)/ipar(12)**2)
integer(irg),intent(in)                 :: nstep
real(sgl),intent(in)                    :: stepsize(nstep)
real(kind=sgl),INTENT(in)               :: accum_e(ipar(4),-ipar(5):ipar(5),-ipar(5):ipar(5))
real(kind=sgl),INTENT(in)               :: mLPNH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(10), ipar(7))
real(kind=sgl),INTENT(in)               :: mLPSH(-ipar(6):ipar(6), -ipar(6):ipar(6), ipar(10), ipar(7))
integer(irg),intent(in)                 :: n
real(dbl),dimension(:),intent(in)       :: x
real(dbl),intent(out)                   :: f
logical,intent(in),optional             :: verbose

integer(kind=irg)                       :: nnx, nny
complex(dbl)                            :: D
real(kind=sgl)                          :: quats(4,1), ma, mi
real(kind=sgl),allocatable              :: ECPpattern(:,:,:)

real(kind=sgl),allocatable              :: binned(:,:)
real(kind=sgl),allocatable              :: ECPpatternintd(:,:)
integer(kind=irg),allocatable           :: ECPpatterninteger(:,:), ECPpatternad(:,:)
real(kind=sgl),allocatable              :: ECPvector(:), ECPvectorcpy(:), ECPtmp(:,:)
real(kind=sgl),allocatable              :: mask(:,:)
integer(kind=irg),allocatable           :: img1(:), img2(:)

integer(kind=irg)                       :: istat, i, j
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: eu(3), ho(3)
logical                                 :: stat, readonly
integer(kind=irg)                       :: hdferr, nlines, nregions


fpar(1) = sngl(X(1))*2.0*stepsize(1) - stepsize(1) + initmeanval(1) ! thetac mean +/- stepsize degrees degrees only

!eu = (/X(2)*2.0*stepsize(2) - stepsize(2) + initmeanval(2), X(3)*2.0*stepsize(3) - stepsize(3)  + initmeanval(3), &
!       X(4)*2.0*stepsize(4) - stepsize(4) + initmeanval(4)/)*cPi/180.0 ! mean +/- stepsize

ho = (/X(2)*2.0*stepsize(2) - stepsize(2) + initmeanval(2), X(3)*2.0*stepsize(3) - stepsize(3)  + initmeanval(3), &
       X(4)*2.0*stepsize(4) - stepsize(4) + initmeanval(4)/)

!quats(1:4,1) = eu2qu(eu)
quats(1:4,1) = ho2qu(ho)

!D = cmplx(0.D0,0.D0)
! read all the files 

allocate(ECPvector(ipar(2)*ipar(3)),mask(ipar(2),ipar(3)))
allocate(ECPpattern(ipar(2),ipar(3),ipar(8)))
allocate(ECPpatternintd(ipar(2),ipar(3)),ECPpatterninteger(ipar(2),ipar(3)), ECPpatternad(ipar(2),ipar(3)))
ECPpatternintd = 0.0
ECPpatterninteger = 0
ECPpatternad = 0

if (present(verbose)) then
    if(verbose) then
        eu = ho2eu(ho)*180.0/cPi    
        print*,'thetac, eu = ',sngl(X(1))*2.0*stepsize(1) - stepsize(1) + initmeanval(1), eu
    end if
end if

mask = 1.0
do i = 1,ipar(2)
    do j = 1,ipar(3)
        if(((float(i)-ceiling(float(ipar(2))/2.0))**2 + (float(j)-ceiling(float(ipar(3))/2.0))**2) .gt. fpar(10)**2) then
            mask(i,j) = 0.0
        end if
    end do
end do

call getECPatterns(ipar, fpar, ECPpattern, quats, accum_e, mLPNH, mLPSH)

nnx = ipar(2)
nny = ipar(3)
!nregions = ipar(12)

allocate(ECPvector(ipar(2)*ipar(3)),stat=istat)

if (ipar(9) .eq. 1) then
    do i = 1,ipar(8)
        ECPpattern(:,:,i) = ECPpattern(:,:,i)*mask
    end do
end if

do i=1,ipar(2)
    do j=1,ipar(3)
        ECPvector((i-1)*ipar(3)+j) = ECPpattern(j,i,1)
    end do
end do

!ECPvector = 0.0

!do i = 1,ipar(2)
!    ECPvector((i-1)*ipar(3)+1:i*ipar(3)) = ECPvectorcpy((ipar(2)-i)*ipar(3)+1:(ipar(2)-i+1)*ipar(3))
!end do

ECPvector = ECPvector**fpar(9)
ECPvector = ECPvector/vecnorm(ECPvector)

if(IPAR(11) .eq. 0) then
    F = 1.0 - DOT_PRODUCT(ECPvector,expt)
else
    allocate(img1(ipar(2)*ipar(3)),img2(ipar(2)*ipar(3)))
    ma = maxval(ECPvector)
    mi = minval(ECPvector)

    img1 = nint(255.0*(ECPvector - mi)/(ma - mi))

    ma = maxval(expt)
    mi = minval(expt)

    img2 = nint(255.0*(expt - mi)/(ma - mi))

    F = Jaccard_Distance(img1,img2,nnx*nny)
    
end if

end subroutine ECPcalfun

!===================================================================
!===================================================================
! here we start with the Wrapper routines that are actually
! called from another language
!
! Tested languages:  IDL
! To be tested:  Matlab
!===================================================================
!===================================================================

!--------------------------------------------------------------------------
!
! SUBROUTINE:getEBSDPatternsWrapper
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief wrapper routine for getEBSDPatterns
!>
!> see example at https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/Gk0xxVFbW8E
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 10/16/15 MDG 1.0 original
!> @date 11/02/15 MDG 1.1 simplified parameters
!> @date 01/12/16 MDG 1.2 added dummy arguments for progress callback and cancel handling
!> @date 01/13/16 MDG 1.3 removed dummy arguments for progress callback and cancel handling
!> @date 07/10/16 MDG 1.4 added energy min/max indices
!--------------------------------------------------------------------------
recursive function getEBSDPatternsWrapper(argc, argv) bind(c, name='getEBSDPatternsWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: getEBSDPatternsWrapper

use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
!f2py intent(in,out) ::  argv
REAL(c_float)                                   :: getEBSDPatternsWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(c_size_t)                               :: nipar, nfpar, nq
integer(c_size_t),dimension(:), pointer         :: ipar
real(c_float), dimension(:), pointer            :: fpar
real(c_float), dimension(:,:), pointer          :: quats
real(c_float), dimension(:,:,:), pointer        :: EBSDpattern, accum_e 
real(c_float), dimension(:,:,:,:),pointer       :: mLPNH, mLPSH

! the following line just helps in identifying the correct order of the subroutine arguments...
!                             1      2      3           4         5       6     7
!subroutine getEBSDPatterns(ipar, fpar, EBSDpattern, quats, accum_e, mLPNH, mLPSH)
!
! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 10
nfpar = 10
nq = 4

call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),fpar,(/nfpar/)) 
call c_f_pointer(argv(3),EBSDpattern,(/ipar(2),ipar(3),ipar(8)/))
call c_f_pointer(argv(4),quats,(/nq,ipar(8)/))
call c_f_pointer(argv(5),accum_e,(/ipar(4),2*ipar(5)+1,2*ipar(5)+1/))
call c_f_pointer(argv(6),mLPNH,(/2*ipar(6)+1, 2*ipar(6)+1, ipar(4), ipar(7)/))
call c_f_pointer(argv(7),mLPSH,(/2*ipar(6)+1, 2*ipar(6)+1, ipar(4), ipar(7)/))

call getEBSDPatterns(ipar, fpar, EBSDpattern, quats, accum_e, mLPNH, mLPSH)

getEBSDPatternsWrapper = 1._c_float
end function getEBSDPatternsWrapper


!--------------------------------------------------------------------------
!
! SUBROUTINE:getECPatternsWrapper
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief wrapper routine for SingleECPPattern; based on Marc's routine above
!>
!> see https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/Gk0xxVFbW8E
!>
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 10/28/15  SS 1.0 original
!> @date 11/02/15 MDG 1.1 simplified parameters
!--------------------------------------------------------------------------
recursive function getECPatternsWrapper(argc, argv) bind(c, name='getECPatternsWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: getECPatternsWrapper

use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
!f2py intent(in,out) ::  argv
REAL(c_float)                                   :: getECPatternsWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(c_size_t)                               :: nipar, nfpar, nq
integer(c_size_t),dimension(:), pointer         :: ipar
real(c_float), dimension(:), pointer            :: fpar
real(c_float), dimension(:,:,:), pointer        :: accum_e, mLPNH, mLPSH, ECPattern
real(c_float), dimension(:,:), pointer          :: quats

! the following line just helps in identifying the correct order of the subroutine arguments...
!                             1      2     3       4       5       6       7
!subroutine getECPatterns(ipar, fpar, ECPattern, quats, accum_e, mLPNH, mLPSH)
!
! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 8
nfpar = 8
nq = 4
call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),fpar,(/nfpar/)) 
call c_f_pointer(argv(3),ECpattern,(/ipar(2),ipar(3),ipar(8)/))
call c_f_pointer(argv(4),quats,(/nq,ipar(8)/))
call c_f_pointer(argv(5),accum_e,(/ipar(4),2*ipar(5)+1,2*ipar(5)+1/))
call c_f_pointer(argv(6),mLPNH,(/2*ipar(7)+1, 2*ipar(7)+1, ipar(6)/))
call c_f_pointer(argv(7),mLPSH,(/2*ipar(7)+1, 2*ipar(7)+1, ipar(6)/))

call getECPatterns(ipar, fpar, ECpattern, quats, accum_e, mLPNH, mLPSH)

getECPatternsWrapper = 1._c_float
end function getECPatternsWrapper

!--------------------------------------------------------------------------
!
! SUBROUTINE: getKosselPatternsWrapper
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief wrapper routine for SingleKosselPattern; nearly identical to ECP case
!>
!> see https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/Gk0xxVFbW8E
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 11/09/15 MDG 1.0 first version
!--------------------------------------------------------------------------
recursive function getKosselPatternsWrapper(argc, argv) bind(c, name='getKosselPatternsWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: getKosselPatternsWrapper

use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
!f2py intent(in,out) ::  argv
REAL(c_float)                                   :: getKosselPatternsWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(c_size_t)                               :: nipar, nfpar, nq
integer(c_size_t),dimension(:), pointer         :: ipar
real(c_float), dimension(:), pointer            :: fpar
real(c_float), dimension(:,:), pointer          :: quats
real(c_float), dimension(:,:,:), pointer        :: KosselPattern, mLPNH, mLPSH

! the following line just helps in identifying the correct order of the subroutine arguments...
!                             1      2     3             4       5       6
!subroutine getKosselPatterns(ipar, fpar, KosselPattern, quats, mLPNH, mLPSH)
!
! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 6
nfpar = 1
nq = 4
call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),fpar,(/nfpar/)) 
call c_f_pointer(argv(3),Kosselpattern,(/ipar(2),ipar(2),ipar(4)/))
call c_f_pointer(argv(4),quats,(/nq,ipar(4)/))
call c_f_pointer(argv(5),mLPNH,(/2*ipar(3)+1, 2*ipar(3)+1,ipar(5)/))
call c_f_pointer(argv(6),mLPSH,(/2*ipar(3)+1, 2*ipar(3)+1,ipar(5)/))

call getKosselPatterns(ipar, fpar, Kosselpattern, quats, mLPNH, mLPSH)

getKosselPatternsWrapper = 1._c_float
end function getKosselPatternsWrapper

!--------------------------------------------------------------------------
!
! SUBROUTINE:efitECPWrapper
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief wrapper routine for fitting ECP pattern
!>
!> see https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/Gk0xxVFbW8E
!>
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 12/15/15  SS 1.0 original
!--------------------------------------------------------------------------
recursive function efitECPWrapper(argc, argv) bind(c, name='efitECPWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: efitECPWrapper

use,INTRINSIC :: ISO_C_BINDING
! use bobyqa_module
use local

IMPLICIT NONE

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
!f2py intent(in,out) ::  argv
REAL(c_float)                                   :: efitECPWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(4)                                      :: nipar, nfpar, ninit, n, iprint, maxfun, npt
real(c_double), dimension(:), pointer           :: rhobeg, rhoend
integer(c_size_t),dimension(:), pointer         :: ipar
real(c_float), dimension(:), pointer            :: fpar
real(c_float), dimension(:), pointer            :: initmeanval
real(c_float), dimension(:), pointer            :: expt
real(c_float), dimension(:,:,:), pointer        :: accum_e
real(c_float), dimension(:,:,:,:), pointer      :: mLPNH
real(c_float), dimension(:,:,:,:), pointer      :: mLPSH
real(c_double), dimension(:), pointer           :: X
real(c_double), dimension(:), pointer           :: XL
real(c_double), dimension(:), pointer           :: XU


! the following line just helps in identifying the correct order of the subroutine arguments...
!                                        
!subroutine BOBYQA(nipar, nfpar, ninit, ipar, fpar, initmeanval, expt, N, NPT, X, XL, XU, RHOBEG, RHOEND, IPRINT, MAXFUN, ECPCALFUN, ACCUM_E, mLPNH, mLPSH)
!
! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 9
nfpar = 10
ninit = 6
n = 6
iprint = 2
maxfun = 10000
npt = n + 6

call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),fpar,(/nfpar/)) 
call c_f_pointer(argv(4),initmeanval,(/n/))
call c_f_pointer(argv(5),expt,(/ipar(2)*ipar(3)/))
call c_f_pointer(argv(6),X,(/n/))
call c_f_pointer(argv(7),XL,(/n/))
call c_f_pointer(argv(8),XU,(/n/))
call c_f_pointer(argv(9),RHOBEG,(/1/))
call c_f_pointer(argv(10),RHOEND,(/1/))
call c_f_pointer(argv(11),accum_e,(/ipar(4),2*ipar(5)+1,2*ipar(5)+1/))
call c_f_pointer(argv(12),mLPNH,(/2*ipar(6)+1, 2*ipar(6)+1, ipar(4), ipar(7)/))
call c_f_pointer(argv(13),mLPSH,(/2*ipar(6)+1, 2*ipar(6)+1, ipar(4), ipar(7)/))

! call BOBYQA(NIPAR, NFPAR, NINIT, IPAR, FPAR, INITMEANVAL, EXPT, N, NPT, X, XL, XU, RHOBEG(1), RHOEND(1),&
!      IPRINT, MAXFUN, ECPCALFUN, accum_e, mLPNH, mLPSH)

efitECPWrapper = 1._c_float

end function efitECPWrapper

!--------------------------------------------------------------------------
!
! SUBROUTINE:efitEBSDWrapper
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief wrapper routine for fitting EBSD pattern
!>
!> see https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/Gk0xxVFbW8E
!>
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 12/15/15  SS 1.0 original
!--------------------------------------------------------------------------
recursive function efitEBSDWrapper(argc, argv) bind(c, name='efitEBSDWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: efitEBSDWrapper

use,INTRINSIC :: ISO_C_BINDING
!use bobyqa_module
use local

IMPLICIT NONE

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
!f2py intent(in,out) ::  argv
REAL(c_float)                                   :: efitEBSDWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(4)                                      :: nipar, nfpar, ninit, n, iprint, maxfun, npt
real(c_double), dimension(:), pointer           :: rhobeg, rhoend
integer(c_size_t),dimension(:), pointer         :: ipar
real(c_float), dimension(:), pointer            :: fpar
real(c_float), dimension(:), pointer            :: initmeanval
real(c_float), dimension(:), pointer            :: expt
real(c_float), dimension(:,:,:), pointer        :: accum_e
real(c_float), dimension(:,:,:,:), pointer      :: mLPNH
real(c_float), dimension(:,:,:,:), pointer      :: mLPSH
real(c_double), dimension(:), pointer           :: X
real(c_double), dimension(:), pointer           :: XL
real(c_double), dimension(:), pointer           :: XU



! the following line just helps in identifying the correct order of the subroutine arguments...
!                                        1      2     3       4           5           6   7   8   9        10
!subroutine BOBYQA(nipar, nfpar, ninit, ipar, fpar, fname, initmeanval, expt, N, NPT, X, XL, XU, RHOBEG, RHOEND, IPRINT, MAXFUN, EBSDCALFUN)
!
! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 9
nfpar = 11
ninit = 4
n = 7
iprint = 2
maxfun = 10000
npt = n + 6

call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),fpar,(/nfpar/)) 
call c_f_pointer(argv(3),initmeanval,(/n/))
call c_f_pointer(argv(4),expt,(/ipar(2)*ipar(3)/))
call c_f_pointer(argv(5),X,(/n/))
call c_f_pointer(argv(6),XL,(/n/))
call c_f_pointer(argv(7),XU,(/n/))
call c_f_pointer(argv(8),RHOBEG,(/1/))
call c_f_pointer(argv(9),RHOEND,(/1/))
call c_f_pointer(argv(10),accum_e,(/ipar(4),2*ipar(5)+1,2*ipar(5)+1/))
call c_f_pointer(argv(11),mLPNH,(/2*ipar(6)+1, 2*ipar(6)+1, ipar(4), ipar(7)/))
call c_f_pointer(argv(12),mLPSH,(/2*ipar(6)+1, 2*ipar(6)+1, ipar(4), ipar(7)/))

! call BOBYQA(NIPAR, NFPAR, NINIT, IPAR, FPAR, INITMEANVAL, EXPT, N, NPT, X, XL, XU, RHOBEG(1), RHOEND(1),&
!      IPRINT, MAXFUN, EBSDCALFUN, accum_e, mLPNH, mLPSH)

efitEBSDWrapper = 1._c_float

end function efitEBSDWrapper

!--------------------------------------------------------------------------
!
! SUBROUTINE:adhisteqWrapper
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief wrapper routine to test the adhisteq filter and compare to IDL results
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 07/18/20  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function adhisteqWrapper(argc, argv) bind(c, name='adhisteqWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: adhisteqWrapper

use,INTRINSIC :: ISO_C_BINDING
use local
use filters

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
REAL(c_float)                                   :: adhisteqWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(c_size_t)                               :: nipar
integer(c_size_t),dimension(:), pointer         :: ipar
integer(c_size_t), dimension(:,:), pointer      :: image
integer(c_size_t), dimension(:,:), pointer      :: imahe

! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 3
call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),image,(/ipar(2),ipar(3)/))
call c_f_pointer(argv(3),imahe,(/ipar(2),ipar(3)/))

imahe = adhisteq( int(ipar(1)), int(ipar(2)), int(ipar(3)), int(image) ) 

end function adhisteqWrapper



end module EMdymod
