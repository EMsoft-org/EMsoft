;
; Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:KosselExecute.pro
;--------------------------------------------------------------------------
;
; PROGRAM: KosselExecute.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main routine for creation of nml file and execution of CTEMEBSD code
;
;> @details This routine still needs to be cleaned up, along with the SingleKosselPatternWrapper etc...
;
;> @date 05/22/14 MDG 1.0 first version
;> @date 04/14/15 MDG 1.1 added HDF5 support
;> @date 11/09/15 MDG 2.0 copied from EBSDExecute.pro and adapted for Kossel
;--------------------------------------------------------------------------
pro KosselExecute, status, single=single

; the keyword /single indicates that only one pattern should be computed

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common EBSDpatterns, pattern, image, finalpattern
common EBSD_anglearrays, euler, quaternions
common EBSDmasks, circularmask

common EBSD_rawdata, accum_e, accum_z, mLPNH, mLPSH
common Kosseldata, KosselPattern
common getenv_common, librarylocation

status = 1

; check whether the mask needs to be recomputed or not
s = size(circularmask)
sm = SEMdata.detnumsx
if (s[0] ne sm) then begin
  d = shift(dist(sm),sm/2,sm/2)
  d[where(d le sm/2)] = 1.0
  d[where(d gt sm/2)] = 0.0
  circularmask = fltarr(SEMdata.detnumsx, SEMdata.detnumsx)
  dm = (SEMdata.detnumsx - sm)/2
  circularmask[dm,0] = d
endif

; next, generate the ipar and fpar parameter arrays used for call_external
nipar = long(6)
ipar = lon64arr(nipar)

ipar[0] = long64(2)    ; will need to be modified 
ipar[1] = long64(SEMdata.detnumsx)
ipar[2] = long64(SEMdata.mpimx)
ipar[3] = long64(SEMdata.numangles)
ipar[4] = long64(SEMdata.mcenergynumbin)
ipar[5] = long64(SEMdata.Esel+1)

nfpar = long(1)
fpar = fltarr(nfpar)

fpar[0] = SEMdata.detthetac

callname = 'getKosselPatternsWrapper'

if keyword_set(single) then begin

; and here is the quaternion that represents the Euler angle triplet
  quats = Core_eu2qu( [SEMdata.detphi1, SEMdata.detphi, SEMdata.detphi2] )
  quats = reform(quats,4,1)
  ipar[3] = 1

; initialize the simulated pattern array
  KosselPattern = fltarr(SEMdata.detnumsx,SEMdata.detnumsx)
  KosselPattern = reform(KosselPattern,SEMdata.detnumsx,SEMdata.detnumsx,1)

  res = call_external(librarylocation+'/libEMsoftLib.dylib', callname, $
        ipar, fpar, KosselPattern, quats, mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)

  if (res ne 1.0) then begin
    Core_print,'getKosselPatternsWrapper return code = '+string(res,format="(F4.1)")
    status = 0
  end 

end else begin

  if (SEMdata.numangles gt 50) then begin
    Core_Print,'',/blank
    Core_Print,'You are computing more than 50 Kossel patterns; this will take a while...'
    Core_Print,'The program will not provide any further updates until the run has been completed.'
    Core_Print,'',/blank
  endif

  KosselPattern = fltarr(SEMdata.detnumsx,SEMdata.detnumsx,SEMdata.numangles)

  res = call_external(librarylocation+'/libEMsoftLib.dylib', callname, $
        ipar, fpar, KosselPattern, quaternions, mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)

  if (res ne 1.0) then begin
    Core_print,'getKosselPatternsWrapper return code = '+string(res,format="(F4.1)")
    status = 0
  end 
endelse

end

