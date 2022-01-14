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
; EMsoft:Efitcalc.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efitcalc.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Electron backscatter diffraction calculation via call_external of EMdymod.f90 routine
;
;> @date 10/15/15 MDG 1.0 first attempt at a user-friendly interface
;> @date 10/17/15 MDG 1.1 integrated with EMdymod.f90 routine
;--------------------------------------------------------------------------
pro Efitcalc,dummy
compile_opt idl2 

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern
common getenv_common, librarylocation

; first convert the Euler angle triplet (in degrees) to a quaternion
Efitdata.quaternion = Core_eu2qu( [Efitdata.detphi1, Efitdata.detphi, Efitdata.detphi2] )

; determine whether or not we can re-use the rgx, rgy, rgz arrays in SingleEBSDPattern
; if the sum of the first four fitOnOff values is zero, and the remainder is not, then recompute = 1 after the first call_external, else 0

; set up the ipar and fpar arrays; all integers must be long64 !!!!
ipar = lon64arr(10)
ipar[0] = long64(2) ; long(recompute) ; 1 if rgx, rgy, rgz detector arrays need to be computed, 0 if not (arrays will have save status)
ipar[1] = long64(Efitdata.detnumsx)
ipar[2] = long64(Efitdata.detnumsy)
ipar[3] = long64(numEbins)
ipar[4] = long64(nsx)
ipar[5] = long64(npx)
ipar[6] = long64(numset)
ipar[7] = long64(1)
ipar[8] = long64(1)
ipar[9] = long64(numEbins)

nfpar = long(13)
fpar = fltarr(nfpar)
fpar[0] = Efitdata.detxpc
fpar[1] = Efitdata.detypc
fpar[2] = Efitdata.detdelta
fpar[3] = Efitdata.detMCsig
fpar[4] = Efitdata.detomega
fpar[5] = Efitdata.dettheta
fpar[6] = Efitdata.detL
fpar[7] = Efitdata.detbeamcurrent
fpar[8] = Efitdata.detdwelltime
quats = reform(Efitdata.quaternion[0:3],4,1)

EBSDpattern = fltarr(Efitdata.detnumsx,Efitdata.detnumsy)
EBSDpattern = reform(EBSDpattern,Efitdata.detnumsx,Efitdata.detnumsy,1)

callname = 'getEBSDPatternsWrapper'

if (!version.os eq 'darwin') then begin
    res = call_external(librarylocation+'/libEMsoftLib.dylib', callname, $
                       ipar, fpar, EBSDpattern, quats, float(accum_e), mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)
endif

if (!version.os eq 'Win32') then begin
    res = call_external(librarylocation+'/EMsoftLib.dll', callname, $
                      ipar, fpar, EBSDpattern, quats, float(accum_e), mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)
endif

if (!version.os eq 'linux') then begin
    res = call_external(librarylocation+'/libEMsoftLib.so', callname, $
                      ipar, fpar, EBSDpattern, quats, float(accum_e), mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)
endif

; IDL puts the pattern origin in the lower left corner so we need to flip the pattern vertically. 
EBSDpattern = reverse(reform(EBSDpattern),2)

if (res ne 1.0) then begin
  Core_print,'getEBSDPatternsWrapper return code = '+string(res,format="(F4.1)")
end 

wset,Efitdata.drawID
Efit_showpattern

end
