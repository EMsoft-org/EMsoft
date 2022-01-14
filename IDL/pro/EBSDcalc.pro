;
; Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:EBSDcalc.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDcalc.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Electron backscatter diffraction calculation via interpolation
;
;> @date 10/15/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
function EBSDcalc,dummy

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, $
                    expEBSDpattern, rgx, rgy, rgz, accum_e_detector          

nAmpere = 6.241D+18 

nel = total(accum_e_detector)

bindx = 1.0/float(1.0+Efitdata.detbinning)^2
binx = Efitdata.detnumsx * bindx
biny = Efitdata.detnumsy * bindx

; intensity prefactor
prefactor = 0.25D0 * nAmpere * Efitdata.detbeamcurrent * Efitdata.detdwelltime * 1.0D-15 / nel

; Lambert scalefactor
scl = float(npx[0])

; convert the Euler angles to a quaternion
Efitdata.quaternion = Core_eu2qu( [Efitdata.detphi1, Efitdata.detphi, Efitdata.detphi2] )

EBSDpattern = replicate(0.0, Efitdata.detnumsx, Efitdata.detnumsy)

for i=0,Efitdata.detnumsx-1 do begin
   for j=0,Efitdata.detnumsy-1 do begin
; do the active coordinate transformation for this euler angle
      dc = Core_quat_Lp( Efitdata.quaternion, [rgx(i,j),rgy(i,j),rgz(i,j)] )
; normalize dc
      dc = dc/sqrt(total(dc*dc))
; convert these direction cosines to coordinates in the Rosca-Lambert projection (always square projection !!!)
      ixy = scl * Core_LambertSphereToSquare( dc, istat )

      if (istat eq 0) then begin
; four-point interpolation (bi-quadratic)
        nix = fix(npx+ixy[0])-npx
        niy = fix(npy+ixy[1])-npy
        nixp = nix+1
        niyp = niy+1
        if (nixp gt npx) then nixp = nix
        if (niyp gt npy) then niyp = niy
        if (nix lt -npx) then nix = nixp
        if (niy lt -npy) then niy = niyp
        dx = ixy[0]-nix
        dy = ixy[1]-niy
        dxm = 1.0-dx
        dym = 1.0-dy
        if (dc[2] gt 0.0) then begin
          for k=0,numEbins[0]-1 do begin
            EBSDpattern(i,j) += accum_e_detector[k,i,j] * ( mLPNH[nix,niy,k] * dxm * dym + $
              mLPNH[nixp,niy,k] * dx * dym + mLPNH[nix,niyp,k] * dxm * dy + $
              mLPNH[nixp,niyp,k] * dx * dy )
          endfor
        end else begin
          for k=0,numEbins[0]-1 do begin
            EBSDpattern(i,j) += accum_e_detector[k,i,j] * ( mLPSH[nix,niy,k] * dxm * dym + $
              mLPSH[nixp,niy,k] * dx * dym + mLPSH[nix,niyp,k] * dxm * dy + $
              mLPSH[nixp,niyp,k] * dx * dy )
          endfor
        endelse
      endif
    endfor
endfor

return,EBSDpattern
end
