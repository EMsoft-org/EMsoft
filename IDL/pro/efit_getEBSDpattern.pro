;
; Copyright (c) 2015, Marc De Graef/Carnegie Mellon University
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are 
; permitted provided that the following conditions are met:
;
;     - Redistributions of source code must retain the above copyright notice, this list 
;        of conditions and the following disclaimer.
;     - Redistributions in binary form must reproduce the above copyright notice, this 
;        list of conditions and the following disclaimer in the documentation and/or 
;        other materials provided with the distribution.
;     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
;        of its contributors may be used to endorse or promote products derived from 
;        this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; ###################################################################
;--------------------------------------------------------------------------
; EMsoft:Efit_getEBSDpattern.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_getEBSDpattern.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief compute an EBSD pattern for a given master pattern and detector geometry
;
;> @date 02/08/23 MDG 1.0 first version
;--------------------------------------------------------------------------
pro Efit_getEBSDpattern, ipar, fpar, quats
;--------------------------------------------------------------------------
;
; SUBROUTINE:getEBSDPatterns
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief This function can be called as a standalone function to compute an EBSD pattern
;
;> @etails The main purpose of this routine and its accompanying wrapper routine is to
;> provide a way for an external program to compute a channeling pattern.  The idea is that 
;> all the necessary arrays and variables are passed in by reference as arguments, without
;> the need for the routine to fetch any other data from files etc...  The initial goal is
;> to have a function that can be called with the CALL_EXTERNAL mechanism in IDL or MatLab.
;> This routine should be called via the getEBSDPatternsWrapper routine;  For calls from
;> a C/C++ program, use the EMsoftCgetEBSDPatterns routine in the EMsoftwrapperLib/EMSEMwrappermod instead.
;
; The original function is part of the EMdymod.f90 module in EMsoftPublic and is translated 
; here to IDL.
;
;> @param ipar array with integer input parameters
;> @param fpar array with float input parameters
;> @param EBSDpattern output array
;> @param quats quaternion input array
;> @param accum_e array with Monte Carlo histogram
;> @param mLPNH Northern hemisphere master pattern
;> @param mLPSH Southern hemisphere master pattern
;
;> @date 10/16/15 MDG 1.0 original
;> @date 11/02/15 MDG 1.1 simplification of the input variables
;> @date 11/04/15 MDG 1.2 added array of quaternions as input parameter; used complete mLPNH/SH arrays with local sum
;> @date 01/12/15 MDG 1.3 added arguments and functionality for interface with DREAM.3D and other calling programs
;> @date 01/13/15 MDG 1.4 after split with EMsoftCgetEBSDPatterns subroutine, removed DREAM.3D interfacing stuff
;> @date 07/10/16 MDG 1.5 added energy min/max parameters
;> @date 08/03/16 MDG 1.6 corrected normalizing issue in rgx,y,z arrays that causes NANs  from Lambert projection routines
;> @date 08/25/16 MDG 1.7 added transfer optics barrel distortion to rgx,y,z arrays.
;> @date 02/19/19 MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
;--------------------------------------------------------------------------

; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common CommonCore, status, logmode, logunit
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern

common cancelcommon, cancel
common Efitdisplaycommon, mask, maskready, expvector

; The following is the mapping for the ipar and fpar arrays:
;
; ipar(1) = 2 if rgx, rgy, rgz detector arrays need to be computed, 1 if not (arrays will have save status)
; ipar(2) = detnumsx
; ipar(3) = detnumsy
; ipar(4) = detnumEbins
; ipar(5) = mcnsx
; ipar(6) = mpnpx
; ipar(7) = numset
; ipar(8) = numquats
; ipar(9) = Eminsel
; ipar(10) = Emaxsel

; fpar(1) = enl%xpc
; fpar(2) = enl%ypc
; fpar(3) = enl%delta
; fpar(4) = enl%MCsig
; fpar(5) = enl%omega
; fpar(6) = enl%thetac
; fpar(7) = enl%L
; fpar(8) = enl%beamcurrent
; fpar(9) = enl%dwelltime
; fpar(10) = enl%alphaBD

; variables that are saved for the next time this function is called
common Detector, accum_e_detector, rgx, rgy, rgz, mLPNHsum, mLPSHsum, prefactor

nAmpere = 6.241D+18 

;====================================
; ------ generate the detector rgx, rgy, rgz arrays 
;====================================
if (ipar(1) ge 1) then begin
; get the sum arrays
  mLPNHsum = total(mLPNH,4)
  mLPSHsum = total(mLPSH,4)

; This needs to be done only once for a given detector geometry (i.e., when ipar(1)=1 or larger)
  scin_x = fltarr(ipar[1])
  scin_y = fltarr(ipar[2])
  
  pcxd = fpar[0] * fpar[2]
  pcyd = fpar[1] * fpar[2]
  
  ii = indgen(ipar[1])
  scin_x = - ( -fpar[0] - ( 1.0 - float(ipar[1]) ) * 0.5 - ii ) * fpar[2]
  scin_y = ( fpar[1] - ( 1.0 - float(ipar[2]) ) * 0.5 - ii ) * fpar[2]

; auxiliary angle to rotate between reference frames
  alp = 0.5 * !pi - (fpar[3] - fpar[5]) * !dtor
  ca = cos(alp)
  sa = sin(alp)

  cw = cos(fpar[4] * !dtor)
  sw = sin(fpar[4] * !dtor)

; compute auxiliary interpolation arrays
  rgx = fltarr(ipar[1],ipar[2])
  rgy = fltarr(ipar[1],ipar[2])
  rgz = fltarr(ipar[1],ipar[2])

; do we need to perform a Barrel Distortion?
; we will do this here by expanding/contracting the radial component of the 
; (rgx, rgy) to (rgx,rgy) * (1+alphaBD * (rgx^2+rgy^2))
; in other words, we pre-distort the sampling grid with the barrel distortion.

epl = ipar[2]-1
  L2 = fpar[6] * fpar[6]
  for j=0,ipar[1]-1 do begin
    Ls = -sw * scin_x[j] + fpar[6] * cw
    Lc = cw * scin_x[j] + fpar[6] * sw
    for i=0,ipar[2]-1 do begin
;    rhos = 1.0/sqrt(sx + scin_y(i)**2)
     rgx[j,epl-i] = (scin_y[i] * ca + sa * Ls) ; * rhos
     rgy[j,epl-i] = Lc ; * rhos
     rgz[j,epl-i] = (-sa * scin_y[i] + ca * Ls) ; * rhos
; apply Barrel Distortion ?
     if (fpar[9] ne 0.0) then begin
; shift the components to the detector center coordinate frame
       xx = rgx[j,epl-i]-pcyd
       yy = rgy[j,epl-i]+pcxd
; compute the distortion amount; the factor of 10^(-10) is inserted here...
       sx = 1.0 + 1.E-10 * fpar[9] * (xx^2+yy^2) 
; and shift them back to the pattern center reference frame
       rgx[j,epl-i] = xx*sx+pcyd
       rgy[j,epl-i] = yy*sx-pcxd
     endif
; make sure that these vectors are normalized ;
     x = sqrt(rgx[j,epl-i]^2+rgy[j,epl-i]^2 +rgz[j,epl-i]^2)
     rgx[j,epl-i] = rgx[j,epl-i] / x
     rgy[j,epl-i] = rgy[j,epl-i] / x
     rgz[j,epl-i] = rgz[j,epl-i] / x
    endfor
  endfor

;====================================
; ------ create the equivalent detector energy array
;====================================
; from the Monte Carlo energy data, we need to extract the relevant
; entries for the detector geometry defined above.  

; determine the scale factor for the Lambert interpolation; the square has
; an edge length of 2 x sqrt(pi/2)
  scl = float(ipar[4]) 

; energy summation will go over all energy bins
  Emin = ipar[8]
  Emax = ipar[9]

  accum_e_detector = fltarr(ipar[3],ipar[1],ipar[2])

; correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(fpar[2]/fpar[6]/sqrt(!pi))
  ipx = ipar[1]/2 + round(fpar[0])
  ipy = ipar[2]/2 + round(fpar[1])
  
  if ((abs(ipy) gt ipar[2]) or (abs(ipx) gt ipar[1])) then begin
    pcvec = [ pcyd*ca + pcxd*sa*sw + fpar[6]*cw*sa, $
             fpar[6]*sw - pcxd*cw, $
             fpar[6]*ca*cw + pcxd*ca*sw - pcyd*sa ]
    pcvec = pcvec/sqrt(total(pcvec^2))
  end else begin
    pcvec = [ rgx[ipx,ipy], rgy[ipx,ipy], rgz[ipx,ipy] ]
  endelse

  calpha = cos(alpha)
  for i=0,ipar[1]-1 do begin
    for j=0,ipar[2]-1 do begin
; do the coordinate transformation for this detector pixel
       dc = [ rgx[i,j], rgy[i,j], rgz[i,j] ]

; make sure the third one is positive; if not, switch all 
       if (dc[2] lt 0.0) then dc = -dc

; convert these direction cosines to coordinates in the Rosca-Lambert projection
       Core_LambertInterpolation, dc, scl, fix(ipar[4]), fix(ipar[4]), nix, niy, nixp, niyp, dx, dy, dxm, dym, /swap

; do the area correction for this detector pixel
       dp = total(pcvec*dc)
       if ((i eq ipx) and (j eq ipy)) then begin
         gam = 0.25 
       end else begin
         gam = ((calpha*calpha + dp*dp - 1.0)^1.5)/(calpha^3) * 0.25
       endelse

; interpolate the intensity 
        for k= Emin-1, Emax-1 do begin
          accum_e_detector[k,i,j] = gam * (accum_e[k,nix,niy] * dxm * dym + $
                                               accum_e[k,nixp,niy] * dx * dym + $
                                               accum_e[k,nix,niyp] * dxm * dy + $
                                               accum_e[k,nixp,niyp] * dx * dy)
        endfor
    endfor
  endfor 
  prefactor = 0.25D0 * nAmpere * fpar[7] * fpar[8]  * 1.0D-15 / total(accum_e_detector)
endif  ; end of ipar(1)>=1 test

; from here on, we simply compute the EBSD patterns by interpolation, using the saved arrays from above
; no intensity scaling or anything else...other than multiplication by pre-factor
; intensity scaling is left to the user of the calling program.

; define some parameters and initialize EBSDpattern
scl = float(ipar[5]) 

; here is the main loop 
  for i=0,ipar[1]-1 do begin
    for j=0,ipar[2]-1 do begin
; do the active coordinate transformation for this euler angle
      dc = Core_quat_Lp(quats, [ rgx[i,j], rgy[i,j], rgz[i,j] ])

; normalize dc
      dc = dc/sqrt(total(dc*dc))

; convert these direction cosines to coordinates in the Rosca-Lambert projection (always square projection ;;;)
      Core_LambertInterpolation, dc, scl, fix(ipar[5]), fix(ipar[5]), nix, niy, nixp, niyp, dx, dy, dxm, dym 

      if (dc[2] gt 0.0) then begin ; we're in the Northern hemisphere
        for k=Emin-1,Emax-1 do begin
          EBSDpattern[i,j] = EBSDpattern[i,j] + accum_e_detector[k,i,j] * ( mLPNHsum[nix,niy,k] * dxm * dym +$
                                      mLPNHsum[nixp,niy,k] * dx * dym + mLPNHsum[nix,niyp,k] * dxm * dy + $
                                      mLPNHsum[nixp,niyp,k] * dx * dy )
        endfor
      end else begin                   ; we're in the Southern hemisphere
        for k=Emin-1,Emax-1 do begin 
          EBSDpattern[i,j] = EBSDpattern[i,j] + accum_e_detector[k,i,j] * ( mLPSHsum[nix,niy,k] * dxm * dym +$
                                      mLPSHsum[nixp,niy,k] * dx * dym + mLPSHsum[nix,niyp,k] * dxm * dy + $
                                      mLPSHsum[nixp,niyp,k] * dx * dy )
        endfor
      endelse
    endfor
  endfor

; finally, scale the patterns by the appropriate factor and return to the calling program
EBSDpattern = prefactor * EBSDpattern

end 
