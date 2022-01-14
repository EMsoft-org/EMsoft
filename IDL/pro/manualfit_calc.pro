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
; EMsoft:manualfit_calc.pro
;--------------------------------------------------------------------------
;
; PROGRAM: manualfit_calc.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief compute patterns and display difference patterns
;
;> @date 04/10/17 MDG 1.0 initial version
;--------------------------------------------------------------------------
pro manualfit_calc, patsel

common FIT_widget_common, FITwidget_s
common FIT_data_common, FITdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common quadfit_common, numpat, stepsize, delta, numsx, numsy, EBSDpatterns, patloc, posx, posy, soln, mival, cnt, HX, HY, HXY, hcnt, maxMI, cuinit, newdetectorparams

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname

common savepatterns, saveEBSDpatterns, mask, prevmax, saveit, filename, Epat


; either we compute all the patterns (patsel=-1), or just one

nset = size(param,/dimensions)
nset = nset[0]

; set up the ipar and fpar arrays; all integers must be long64 !!!!
; none of the integer parameters are refinable, so this part is always the same
nipar = long(10)
ipar = lon64arr(nipar)

; ipar(1) = 1 if rgx, rgy, rgz detector arrays need to be computed, 0 if not (arrays will have save status)
; ipar(2) = detnumsx
; ipar(3) = detnumsy
; ipar(4) = detnumEbins
; ipar(5) = mcnsx
; ipar(6) = mpnpx
; ipar(7) = numset
; ipar(8) = numquats
; ipar(9) = 1
; ipar(10) = detnumEbins

ipar[0] = long64(2) ; long(recompute) ; 1 if rgx, rgy, rgz detector arrays need to be computed, 0 if not (arrays will have save status)
; the following lines resulted in a crash of the EMdymod routine... not sure why, so they are commented out for now... program will
; run marginally slower this way...
;if ((fitIterations gt 0L) and (total(fitOnOff[0:4]) eq 0)) then begin
;  ipar[0] = long64(0) 
;endif
ipar[1] = long64(FITdata.numsx)
ipar[2] = long64(FITdata.numsy)
ipar[3] = long64(numEbins)
ipar[4] = long64(nsx)
ipar[5] = long64(npx)
ipar[6] = long64(numset)
ipar[7] = long64(1)
ipar[8] = long64(1)
ipar[9] = long64(numEbins)

nfpar = long(10)
fpar = fltarr(nfpar)

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

; the following parameters are constant for all patterns
fpar[2] = FITdata.delta
fpar[3] = FITdata.detMCsig
fpar[4] = 0.0
fpar[5] = 10.0
fpar[6] = FITdata.L
fpar[7] = 1000.0
fpar[8] = 1000.0
fpar[9] = 0.0
faccum_e = float(accum_e)
callname = 'getEBSDPatternsWrapper'

z = size(param,/dimension)
gma = 0.34

if (patsel eq -1) then begin   ; we need to compute all four patterns
  	for ii=0,FITdata.numpat-1 do begin
; next we loop over all the patterns and compute them using the current parameter values
	  fpar[0] = FITdata.xpc + FITdata.patloc[2*ii]/FITdata.delta
	  fpar[1] = FITdata.ypc + FITdata.patloc[2*ii+1]/FITdata.delta

	  quaternion = Core_eu2qu( [FITdata.phi1[ii+1], FITdata.phi[ii+1], FITdata.phi2[ii+1] ] )
	  print,ii+1,quaternion
	  quats = float(reform(quaternion[0:3],4,1))

	  EBSDsim = replicate(0.0,FITdata.numsx,FITdata.numsy)
	  EBSDsim = reform(EBSDsim,FITdata.numsx*FITdata.numsy,1)

	  res = call_external('/Users/mdg/Files/EMsoftBuild/Bin/libEMsoftLib.dylib', callname, $
	        ipar, fpar, EBSDsim, quats, faccum_e, mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)

	  EBSDsim = EBSDsim^gma
	  EBSDsim = reform(EBSDsim,FITdata.numsx,FITdata.numsy)
	  EBSDsim -= sfit(EBSDsim,2)
	  ;EBSDsim = reform(bytscl(adapt_hist_equal(EBSDsim,nregions=10)),numsx*numsy)
	  EBSDsim = bytscl(reform(EBSDsim,FITdata.numsx*FITdata.numsy))
	  ;Epat[0,i] = reform(adapt_hist_equal(reform(EBSDsim,numsx,numsy),nregions=10),numsx*numsy)
	  Epat[0,ii] = EBSDsim
	endfor
end else begin
	  ii = patsel - 1
	; compute pattern using the current parameter values
	  fpar[0] = FITdata.xpc + FITdata.patloc[2*ii]/FITdata.delta
	  fpar[1] = FITdata.ypc + FITdata.patloc[2*ii+1]/FITdata.delta

	  quaternion = Core_eu2qu( [FITdata.phi1[ii+1], FITdata.phi[ii+1], FITdata.phi2[ii+1] ] )
	  print,ii+1,quaternion
	  quats = float(reform(quaternion[0:3],4,1))

	  EBSDsim = replicate(0.0,FITdata.numsx,FITdata.numsy)
	  EBSDsim = reform(EBSDsim,FITdata.numsx*FITdata.numsy,1)

	  res = call_external('/Users/mdg/Files/EMsoftBuild/Bin/libEMsoftLib.dylib', callname, $
	        ipar, fpar, EBSDsim, quats, faccum_e, mLPNH, mLPSH, /F_VALUE, /VERBOSE, /SHOW_ALL_OUTPUT)

	  EBSDsim = EBSDsim^gma
	  EBSDsim = reform(EBSDsim,FITdata.numsx,FITdata.numsy)
	  EBSDsim -= sfit(EBSDsim,2)
	  ;EBSDsim = reform(bytscl(adapt_hist_equal(EBSDsim,nregions=10)),numsx*numsy)
	  EBSDsim = bytscl(reform(EBSDsim,FITdata.numsx*FITdata.numsy))
	  ;Epat[0,i] = reform(adapt_hist_equal(reform(EBSDsim,numsx,numsy),nregions=10),numsx*numsy)
	  Epat[0,ii] = EBSDsim
endelse

; display the updated pattern(s)
if (patsel eq -1) then begin   ; we need to compute all four patterns
  	for ii=0,FITdata.numpat-1 do begin
  		tvscl,reform(abs(float(EBSDpatterns[*,ii])-float(Epat[*,ii])),FITdata.numsx,FITdata.numsy),FITdata.dloc[0,ii],FITdata.dloc[1,ii]
  	endfor
end else begin
	ii = patsel-1
  	tvscl,reform(abs(float(EBSDpatterns[*,ii])-float(Epat[*,ii])),FITdata.numsx,FITdata.numsy),FITdata.dloc[0,ii],FITdata.dloc[1,ii]
endelse

; recompute the fit quality parameters
V = bytarr(2,FITdata.numsx*FITdata.numsy*FITdata.numpat)
MI = 0.0
V[0,0:*] = reform(EBSDpatterns,FITdata.numsx*FITdata.numsy*FITdata.numpat)
V[1,0:*] = reform(Epat,FITdata.numsx*FITdata.numsy*FITdata.numpat)
bs = 1
h = double(Core_histnd(V,bs,/normalize))


; get the entropies of the individual images by projecting the joint histogram onto the axes
q = where(h ne 0.0,ccnt)
if (ccnt ne 0) then HXY = -(total(h[q] * alog(h[q])))
W = total(h,1)
q = where(W ne 0.0,ccnt)
if (ccnt ne 0) then HX = -(total(W[q] * alog(W[q])))
W = total(h,2)
q = where(W ne 0.0,ccnt)
if (ccnt ne 0) then HY = -(total(W[q] * alog(W[q])))

MI = HX+HY-HXY
print,MI

print,FITdata.phi1[1:4]
print,FITdata.phi[1:4]
print,FITdata.phi2[1:4]
print,FITdata.xpc, FITdata.ypc, FITdata.L

; and display the mutual information value in the appropriate widget
wset,2
tvscl,alog(h+1.e-6)
wset,FITwidget_s.patterndrawID


end
