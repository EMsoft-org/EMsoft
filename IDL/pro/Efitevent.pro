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
; EMsoft:Efitevent.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efitevent.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief special event handler for all the CW_BGROUP calls, since CW_BGROUP does not support event_pro
;
;> @date 03/19/14 MDG 1.0 first version
;> @date 08/03/16 MDG 1.1 modified default fit options
;--------------------------------------------------------------------------
function Efitevent, event

;------------------------------------------------------------
; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common CommonCore, status, logmode, logunit
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern

WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

CASE eventval OF
        'FITMODE' : begin
                oldmode = Efitdata.fitmode
                Efitdata.fitmode = Core_WidgetChoiceEvent( Efitwidget_s.fitmode,  'Fit mode? ',/value)
                if ((oldmode ne 0) and (Efitdata.fitmode eq 0)) then begin
; reset the fitOnOff parameters to off for all of them...
                  fitOnOff = replicate(0L,nFit)
                  for i=0,nFit-1 do WIDGET_CONTROL, set_value=0, Efitwidget_s.fitOnOff[i]
                endif
                if (Efitdata.fitmode eq 1) then begin
                  for i=0,nFit-1 do WIDGET_CONTROL, set_value=0, Efitwidget_s.fitOnOff[0]
                  WIDGET_CONTROL, set_value=1, Efitwidget_s.fitOnOff[0]
                  WIDGET_CONTROL, set_value=1, Efitwidget_s.fitOnOff[2]
                  WIDGET_CONTROL, set_value=1, Efitwidget_s.fitOnOff[3]
                  fitOnOff = replicate(0L,nFit)
                  fitOnOff[0] = 1L
                  fitOnOff[2:3] = 1L
                endif
                if (Efitdata.fitmode eq 2) then begin
                  for i=0,nFit-1 do WIDGET_CONTROL, set_value=0, Efitwidget_s.fitOnOff[0]
                  for i=5,7 do WIDGET_CONTROL, set_value=1, Efitwidget_s.fitOnOff[i]
                  fitOnOff = replicate(0L,nFit)
                  fitOnOff[5:7] = 1L
                endif
        endcase

        'PREPROC' : begin
                Efitdata.preproc = Core_WidgetChoiceEvent( Efitwidget_s.preproc,  'Preprocessing mode? ')
        endcase

        'CONVCRIT' : begin
                Efitdata.convcrit = Core_WidgetChoiceEvent( Efitwidget_s.convcrit,  'Convergence criterion? ')
        endcase

        'SMOOTHVAL' : begin
                Efitdata.smoothval = Core_WidgetChoiceEvent( Efitwidget_s.smoothval,  'Smoothing parameter? ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
        endcase

        'RAMPONOFF' : begin
                Efitdata.ramponoff = Core_WidgetChoiceEvent( Efitwidget_s.ramponoff,  'Ramp filter? ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
        endcase

        'HIPASSONOFF' : begin
                Efitdata.hipassonoff = Core_WidgetChoiceEvent( Efitwidget_s.hipassonoff,  'Hipass filter? ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
        endcase

        'CIRCULARMASK' : begin
                Efitdata.showcircularmask = Core_WidgetChoiceEvent( Efitwidget_s.circularmask,  'Add circular mask? ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
	endcase

        'INVERSEGAUSSIAN' : begin
                Efitdata.inverseGaussian = Core_WidgetChoiceEvent( Efitwidget_s.inverseGaussian,  'Use inverted Gaussian weight factor? ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
	endcase

        'BINNING' : begin
                Efitdata.detbinning = Core_WidgetChoiceEvent( Efitwidget_s.detbinning,  'Binning set to  ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
	endcase

        'EBSDEULERCONVENTION' : begin
                Efitdata.EulerConvention = Core_WidgetChoiceEvent( Efitwidget_s.EulerConvention,  'Euler Convention set to ')
	endcase

        'EBSDPATTERNORIGIN' : begin
                Efitdata.PatternOrigin = Core_WidgetChoiceEvent( Efitwidget_s.PatternOrigin,  'Pattern origin set to ')
                if ((max(EBSDpattern) gt 0) or (max(expEBSDpattern) gt 0)) then Efit_showpattern
	endcase

        'DEToL' : begin
                Efitdata.detoL = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[0],  'Fit scintillator distance? ')
                if (Efitdata.detoL eq 0) then fitOnOff[0] = 0 else fitOnOff[0] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DEToOMEGA' : begin
                Efitdata.detoomega = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[1],  'Fit sample omega angle? ')
                if (Efitdata.detoomega eq 0) then fitOnOff[1] = 0 else fitOnOff[1] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DEToXPC' : begin
                Efitdata.detoxpc = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[2],  'Fit pattern center x? ')
                if (Efitdata.detoxpc eq 0) then fitOnOff[2] = 0 else fitOnOff[2] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DEToYPC' : begin
                Efitdata.detoypc = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[3],  'Fit pattern center y? ')
                if (Efitdata.detoypc eq 0) then fitOnOff[3] = 0 else fitOnOff[3] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DEToGAMMA' : begin
                Efitdata.detogamma = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[4],  'Fit intensity gammma? ') 
                if (Efitdata.detogamma eq 0) then fitOnOff[4] = 0 else fitOnOff[4] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DETophi1' : begin
                Efitdata.detophi1 = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[5],  'Fit Euler phi1 angle? ')
                if (Efitdata.detophi1 eq 0) then fitOnOff[5] = 0 else fitOnOff[5] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DETophi' : begin
                Efitdata.detophi = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[6],  'Fit Euler Phi angle? ')
                if (Efitdata.detophi eq 0) then fitOnOff[6] = 0 else fitOnOff[6] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DETophi2' : begin
                Efitdata.detophi2 = Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[7],  'Fit Euler phi2 angle? ')
                if (Efitdata.detophi2 eq 0) then fitOnOff[7] = 0 else fitOnOff[7] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

       'DETotheta' : begin
                Efitdata.detotheta= Core_WidgetChoiceEvent( Efitwidget_s.fitOnOff[8],  'Fit detector tilt angle? ')
                if (Efitdata.detotheta eq 0) then fitOnOff[8] = 0 else fitOnOff[8] = 1
                if (total(fitOnOff) gt 0) then begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =1
                end else begin
                  WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive =0
                endelse
	endcase

        'DISPLAYOPTION' : begin   ; this comes from the Efit_control widget...
                Efitdata.displayoption = Core_WidgetChoiceEvent( Efitwidget_s.displayoption, 'Set option to ',/value)
                Efit_showpattern
        endcase

        'PATTERNFORMAT' : begin
                Efitdata.imageformat = Core_WidgetChoiceEvent( Efitwidget_s.imageformat, 'Set option to ',/value)
        endcase

else: MESSAGE, "Efitevent: Event User Value Not Found"

endcase

return,eventval
end 
