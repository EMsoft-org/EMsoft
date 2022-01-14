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
; EMsoft:Efit_fit.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_fit.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief actual fitting routine; sets up the run, then calls Efit_amoeba.
;
;> @date 10/20/15 MDG 1.0 first version
;--------------------------------------------------------------------------
pro Efit_fit, dummy

;------------------------------------------------------------
; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common CommonCore, status, logmode, logunit
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations


common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern

common Efitdisplaycommon, mask, maskready, expvector


; start time
t1 = systime(1)

; set iteration parameter
fitIterations = 0L

; make sure that the mask has been defined, if necessary
maskready = 0
Efit_showpattern

; store the experimental pattern in a normalized column vector
v = reform(float(expEBSDpattern),Efitdata.detnumsx*Efitdata.detnumsy)
vl = sqrt(total(v*v))
expvector = v/vl

; how many parameters in this fit ?
nset = total(fitOnOff)
pa = fltarr(nset)
scl = fltarr(nset)

; starting values for refinable parameters
fitValue[0] = Efitdata.detL
fitValue[1] = Efitdata.detomega
fitValue[2] = Efitdata.detxpc
fitValue[3] = Efitdata.detypc
fitValue[4] = Efitdata.detgamma
fitValue[5] = Efitdata.detphi1
fitValue[6] = Efitdata.detphi
fitValue[7] = Efitdata.detphi2
fitValue[8] = Efitdata.dettheta

npos = 0
for i=0,nFit-1 do begin
  if (fitOnOff[i] ne 0) then begin
    pa[npos] = fitValue[i]
    scl[npos] = fitStep[i]
    npos += 1
  endif
endfor

;next, and BEFORE we call the Efit_amoeba routine, we must create the cancel button as a 
;standalong button widget with explicit WIDGET_EVENT handling instead of XMANAGER handling...
Device, Get_Screen_Size=sizes
xlocation = (sizes(0) / 2.0) - 75 
ylocation = (sizes(1) / 2.0) + 25 
Efitwidget_s.cancelwidget = Widget_Base(Column=1, Title='Cancel Fit', TLB_Frame_Attr=1, $
                                        Base_Align_Center=1, XOffSet=xlocation, YOffSet=ylocation)

label = Widget_Label(EfitWidget_s.cancelwidget, Value='Click button to end computation')
Efitwidget_s.cancelbutton = Widget_Button(Efitwidget_s.cancelwidget, Value='ABORT FIT')

Widget_Control, Efitwidget_s.cancelwidget, /Realize



; next we start the modified amoeba routine Efit_amoeba to improve the experimental parameters
ftol=2.0e-6
fmin=Efit_amoeba(ftol,function_name='Efit_update', function_value=fv, ncalls=iter, nmax=500,p0=pa, scale=scl)

if (n_elements(fmin) ne nset) then begin
  if (fmin eq -1) then Core_Print,'Efit_amoeba failed to converge after maximum number of steps'
  if (fmin eq -2) then Core_Print,'Efit_amoeba interrupted by user'
end else begin
  t2 = systime(1)
  Core_Print,'Total execution time [s] : '+string(t2-t1,FORMAT="(F9.2)")
  Core_Print,'Number of iterations : '+string(iter,FORMAT="(I4)")
endelse

; copy the refined values into the current value set
Efitdata.detL = fitValue[0]
Efitdata.detomega = fitValue[1]
Efitdata.detxpc = fitValue[2]
Efitdata.detypc = fitValue[3]
Efitdata.detgamma = fitValue[4]
Efitdata.detphi1 = fitValue[5]
Efitdata.detphi = fitValue[6]
Efitdata.detphi2 = fitValue[7]
Efitdata.dettheta = fitValue[8]

Efit_updatePC,/display

; and remove the cancel button widget...
WIDGET_CONTROL, Efitwidget_s.cancelwidget, /DESTROY

end 
