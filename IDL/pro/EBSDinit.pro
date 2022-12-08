;
; Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:EBSDinit.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDinit.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief initialize all arrays for EBSD pattern sumulation (mostly from EBSDmod.f90)
;
;> @details 10/12/15 a new GUI to interactively determine the best fit parameters
;> for an EBSD pattern; this routine is an IDL copy of the EMEBSD initialization code
;
;> @date 10/12/15 MDG 1.0 first attempt at a user-friendly interface
;> @date 10/13/15 MDG 1.1 updated with corrected structure names
;--------------------------------------------------------------------------
pro EBSDinit,initonly=initonly

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, $
                    expEBSDpattern, rgx, rgy, rgz, accum_e_detector          


if keyword_set(initonly) then goto,skip

Core_Print,'Loading data files'

EMDatapathname = Core_getenv(/data)

; first read the master file
file_id = H5F_OPEN(strtrim(Efitdata.pathname+'/'+Efitdata.mpfilename,2))

group1_id = H5G_OPEN(file_id,'NMLparameters')
group2_id = H5G_OPEN(group1_id,'EBSDMasterNameList')

dset_id = H5D_OPEN(group2_id,'energyfile')
energyfile = H5D_READ(dset_id)
Efitdata.energyfilename = energyfile
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'npx')
npx = H5D_READ(dset_id)
npx = npx[0]
npy = npx
H5D_close,dset_id

H5G_close,group2_id
H5G_close,group1_id

group2_id = H5G_OPEN(file_id,'EMData')

dset_id = H5D_OPEN(group2_id,'numEbins')
nnE = H5D_READ(dset_id)
nnE = nnE[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'numset')
numset = H5D_READ(dset_id)
numset = numset[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'mLPNH')
mLPNH = H5D_READ(dset_id)
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'mLPSH')
mLPSH = H5D_READ(dset_id)
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'xtalname')
Masterxtalname = H5D_READ(dset_id)
H5D_close,dset_id

H5G_close,group2_id
H5F_close,file_id

Core_Print,'   -> Master Pattern file read'

; then the Monte Carlo file
file_id = H5F_OPEN(strtrim(EMdatapathname+'/'+Efitdata.energyfilename,2))

group1_id = H5G_OPEN(file_id,'NMLparameters')
group2_id = H5G_OPEN(group1_id,'MCCLNameList')

dset_id = H5D_OPEN(group2_id,'xtalname')
MCxtalname = H5D_READ(dset_id)
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'MCmode')
MCmode = H5D_READ(dset_id)
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'numsx')
nsx = H5D_READ(dset_id)
nsx = nsx[0]
nsx = (nsx-1)/2
nsy = nsx
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'EkeV')
EkeV = H5D_READ(dset_id)
EkeV = EkeV[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'Ehistmin')
Ehistmin = H5D_READ(dset_id)
Ehistmin = Ehistmin[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'Ebinsize')
Ebinsize = H5D_READ(dset_id)
Ebinsize = Ebinsize[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'depthmax')
depthmax = H5D_READ(dset_id)
depthmax = depthmax[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'depthstep')
depthstep = H5D_READ(dset_id)
depthstep = depthstep[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'sig')
MCsig = H5D_READ(dset_id)
MCsig = MCsig[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'omega')
MComega = H5D_READ(dset_id)
MComega = MComega[0]
H5D_close,dset_id

H5G_close,group2_id
H5G_close,group1_id

group2_id = H5G_OPEN(file_id,'EMData')

dset_id = H5D_OPEN(group2_id,'numEbins')
numEbins = H5D_READ(dset_id)
numEbins = numEbins[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'numzbins')
numzbins = H5D_READ(dset_id)
numzbins = numzbins[0]
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'accum_e')
accum_e = H5D_READ(dset_id)
H5D_close,dset_id

dset_id = H5D_OPEN(group2_id,'accum_z')
accum_z = H5D_READ(dset_id)
H5D_close,dset_id

H5G_close,group2_id
H5F_close,file_id

Core_Print,'   -> Monte Carlo file read'

skip:

; next, pre-compute a number of arrays and parameters  (from EBSDGenerateDetector function)
;====================================
; ------ generate the detector arrays
;====================================
; This needs to be done only once for a given detector geometry
scin_x = - ( Efitdata.detxpc - ( 1.0 - Efitdata.detnumsx ) * 0.5 - findgen(Efitdata.detnumsx) ) * Efitdata.detdelta
scin_y = ( Efitdata.detypc - ( 1.0 - Efitdata.detnumsy ) * 0.5 - findgen(Efitdata.detnumsy) ) * Efitdata.detdelta

rgx = replicate(0.0,Efitdata.detnumsx,Efitdata.detnumsy)
rgy = rgx
rgz = rgx

; auxiliary angle to rotate between reference frames
alp = 0.5 * !pi - (MCsig - Efitdata.dettheta) * !dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(Efitdata.detomega * !dtor)
sw = sin(Efitdata.detomega * !dtor)

; compute auxilliary interpolation arrays
L2 = Efitdata.detL * Efitdata.detL
for j=0,Efitdata.detnumsx-1 do begin
  sx = L2 + scin_x[j] * scin_x[j]
  Ls = -sw * scin_x[j] + Efitdata.detL*cw
  Lc = cw * scin_x[j] + Efitdata.detL*sw
  for i=0,Efitdata.detnumsy-1 do begin
   rhos = 1.0/sqrt(sx + scin_y[i]^2)
   rgx[j,i] = (scin_y[i] * ca + sa * Ls) * rhos
   rgy[j,i] = Lc * rhos
   rgz[j,i] = (-sa * scin_y[i] + ca * Ls) * rhos
  endfor
endfor

; normalize the direction cosines.
z = 1.0/sqrt(rgx^2+rgy^2+rgz^2)
rgx = rgx*z
rgy = rgy*z
rgz = rgz*z

;====================================
; ------ create the equivalent detector energy array
;====================================
; from the Monte Carlo energy data, we need to extract the relevant
; entries for the detector geometry defined above.  Once that is 
; done, we can get rid of the larger energy array
;
; in the old version, we either computed the background model here, or 
; we would load a background pattern from file.  In this version, we are
; using the background that was computed by the MC program, and has 
; an energy histogram embedded in it, so we need to interpolate this 
; histogram to the pixels of the scintillator.  In other words, we need
; to initialize a new accum_e array for the detector by interpolating
; from the Lambert projection of the MC results.
;

; determine the scale factor for the Lambert interpolation; the square has
; an edge length of 2 x sqrt(pi/2)
  scl = float(nsx) ;  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

; set the indices of the minimum and maximum energy
  Emin=1
  Emax=numEbins

; check for negative array indices !!!! the f90 program does use them, but they do not exist in IDL

  accum_e_detector = fltarr(numEbins,Efitdata.detnumsx,Efitdata.detnumsy)

  for i=0,Efitdata.detnumsx-1 do begin
    for j=0,Efitdata.detnumsy-1 do begin
; do the coordinate transformation for this detector pixel
       dc = [ rgx[i,j],rgy[i,j],rgz[i,j] ]
; make sure the third one is positive; if not, switch all 
       if (dc[2] lt 0.0) then dc = -dc
; convert these direction cosines to coordinates in the Rosca-Lambert projection
        ixy = scl * Core_LambertSphereToSquare( dc, istat )
        x = ixy[0]
        ixy[0] = ixy[1]
        ixy[1] = -x
; four-point interpolation (bi-quadratic) [to be tested]
        nix = fix(nsx+ixy[0]) - nsx
        niy = fix(nsy+ixy[1]) - nsy
        dx = ixy[0]-nix
        dy = ixy[1]-niy
        dxm = 1.0-dx
        dym = 1.0-dy
; interpolate the intensity 
        accum_e_detector[0:*,i,j] = accum_e[0:*,nix,niy] * dxm * dym + accum_e[0:*,nix+1,niy] * dx * dym + $
                                    accum_e[0:*,nix,niy+1] * dxm * dy + accum_e[0:*,nix+1,niy+1] * dx * dy
    endfor
  endfor 
  accum_e_detector = accum_e_detector * 0.25


Core_Print,'   -> detector arrays precomputed'




end
