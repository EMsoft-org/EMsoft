
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
; EMsoft:Core_FitLine.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_FitLine.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief generate a widget line for one of the fitting parameters in Efit.pro
;
;> @date 10/12/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
function Core_FitLine,block2, sel

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations


; refinable parameters, each one a row with name, value, fit step size, fit on/off, manual inc and dec, and manual step

line1 = WIDGET_BASE(block2, XSIZE=1200, /ROW)

; fit parameter name
item1 = WIDGET_LABEL(line1, VALUE=fitName[sel], font=fontstr, /ALIGN_LEFT)
; current value (editable)
Efitwidget_s.fitValue[sel] = Core_WTextE(line1,'', fontstr, 10, 25, 10, 1, string(fitValue[sel],format="(F9.2)"), fitUserLabel[sel],'Efit_event')
; initial step size
Efitwidget_s.fitStep[sel] = Core_WTextE(line1,'', fontstr, 10, 25, 10, 1, string(fitStep[sel],format="(F9.2)"), fitStepLabel[sel],'Efit_event')
; include in fit toggle
vals = ['No','Yes']
Efitwidget_s.fitOnOff[sel]= CW_BGROUP(line1, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT=' ', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE=fitOnOffLabel[sel], $
                        SET_VALUE=fitOnOff[sel])
; include a separator
item1 = WIDGET_LABEL(line1, VALUE='       ', font=fontstrlarge, /ALIGN_LEFT)
; then insert the + and - buttons (increment/decrement)
Efitwidget_s.fitManualUp[sel]= WIDGET_BUTTON(line1, $
                                UVALUE=fitUpLabel[sel], $
                                VALUE=' + ', $
                                FONT=fontstrlarge, $
                                EVENT_PRO='Efit_event', $
                                SENSITIVE=1)

qitem1 = WIDGET_LABEL(line1, VALUE=' ', font=fontstrlarge, /ALIGN_LEFT)
Efitwidget_s.fitManualDown[sel]= WIDGET_BUTTON(line1, $
                                UVALUE=fitDownLabel[sel], $
                                VALUE=' - ', $
                                FONT=fontstrlarge, $
                                EVENT_PRO='Efit_event', $
                                SENSITIVE=1)
; manual step size
item1 = WIDGET_LABEL(line1, VALUE='   ', font=fontstrlarge, /ALIGN_LEFT)
Efitwidget_s.fitManualStep[sel] = Core_WTextE(line1,'', fontstr, 10, 25, 10, 1, string(fitManualStep[sel],format="(F9.2)"), fitManualStepLabel[sel],'Efit_event')





return,0
end
