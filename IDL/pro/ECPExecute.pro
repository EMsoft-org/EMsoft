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
; EMsoft:ECPExecute.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECPExecute.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main routine for creation of nml file and execution of CTEMEBSD code
;
;> @date 05/22/14 MDG 1.0 first version
;> @date 04/14/15 MDG 1.1 added HDF5 support
;> @date 10/30/15 MDG 2.0 copied from EBSDExecute.pro and adapted for ECP
;> @date 11/04/15 MDG 2.1 added multiple ECP pattern support
;--------------------------------------------------------------------------
pro ECPExecute, status, single=single

; the keyword /single indicates that only one pattern should be computed

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common EBSDpatterns, pattern, image, finalpattern
common EBSD_anglearrays, euler, quaternions
common EBSDmasks, circularmask

common EBSD_rawdata, accum_e, accum_z, mLPNH, mLPSH
common ECPdata, ECPattern
common getenv_common, librarylocation

status = 1

; check whether the mask needs to be recomputed or not
s = size(circularmask)
sm = SEMdata.detnumsx
if (s[0] ne sm) then begin
  d = shift(dist(sm),sm/2,sm/2)
  d[where(d le sm/2)] = 1.0
  d[where(d gt sm/2)] = 0.0
  circularmask = fltarr(SEMdata.detnumsx, SEMdata.detnumsy)
  dm = (SEMdata.detnumsx - sm)/2
  circularmask[dm,0] = d
endif

; next, generate the ipar and fpar parameter arrays used for call_external
nipar = long(8)
ipar = lon64arr(nipar)

ipar[0] = long64(2)    ; will need to be modified 
ipar[1] = long64(SEMdata.detnumsx)
ipar[2] = long64(SEMdata.detnumsy)
ipar[3] = long64(SEMdata.mcenergynumbin)
ipar[4] = long64(SEMdata.mcimx)
ipar[5] = long64(SEMdata.numset)
ipar[6] = long64(SEMdata.mpimx)
ipar[7] = long64(SEMdata.numangles)

nfpar = long(8)
fpar = fltarr(nfpar)

fpar[0] = SEMdata.detthetac
fpar[1] = SEMdata.detsampleytilt
fpar[2] = SEMdata.detW
fpar[3] = SEMdata.detRi
fpar[4] = SEMdata.detRo
fpar[5] = SEMdata.mcsigstart
fpar[6] = SEMdata.mcsigend
fpar[7] = SEMdata.mcsigstep

callname = 'getECPatternsWrapper'
faccum_e = float(accum_e)

if keyword_set(single) then begin

; and here is the quaternion that represents the Euler angle triplet
  quats = Core_eu2qu( [SEMdata.detphi1, SEMdata.detphi, SEMdata.detphi2] )
  quats = reform(quats,4,1)
  ipar[7] = 1

; initialize the simulated pattern array
  ECPattern = fltarr(SEMdata.detnumsx,SEMdata.detnumsy)
  ECPattern = reform(ECPattern,SEMdata.detnumsx,SEMdata.detnumsy,1)

  res = call_external(librarylocation+'/libEMsoftLib.dylib', callname, $
        ipar, fpar, ECPattern, quats, faccum_e, mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)

  if (res ne 1.0) then begin
    Core_print,'getECPatternsWrapper return code = '+string(res,format="(F4.1)")
    status = 0
  end 

end else begin

  if (SEMdata.numangles gt 50) then begin
    Core_Print,'',/blank
    Core_Print,'You are computing more than 50 ECPs; this will take a while...'
    Core_Print,'The program will not provide any further updates until the run has been completed.'
    Core_Print,'',/blank
  endif

  ECPattern = fltarr(SEMdata.detnumsx,SEMdata.detnumsy,SEMdata.numangles)

  res = call_external(librarylocation+'/libEMsoftLib.dylib', callname, $
        ipar, fpar, ECPattern, quaternions, faccum_e, mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)

  if (res ne 1.0) then begin
    Core_print,'getECPatternsWrapper return code = '+string(res,format="(F4.1)")
    status = 0
  end 
endelse

end

