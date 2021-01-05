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
; EMsoft:EBSDfit_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDfit_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for EBSDfit.pro program
;
;> @date 10/13/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro EBSDfit_event,event

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common CommonCore, status, logmode, logunit
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations


if (event.id eq Efitwidget_s.base) then begin
  Efitdata.xlocation = event.x
  Efitdata.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
        'EXPFILE': begin
; ask the user to select the data file
		Efitgetfilename,validfile,/PATTERNFILE
; start the controller widget (it if isn't already running)
                if (XRegistered("Efit_control") EQ 0) then begin
                  Efit_control
                endif
; then start the actual display widget and put the experimental pattern in it...
; better yet, we can check what the dimensions of the current drawing area are in 
; case the new experimental pattern has different dimensions...
                if (XRegistered("Efit_displaybase") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.displaybase, /DESTROY
                endif 
                Efit_display
; and draw the pattern in the current display mode
                Efit_showpattern
        endcase

        'MPFILE': begin
; ask the user to select the data file
		Efitgetfilename,validfile,/MPFILE
                if (validfile eq 1) then begin
                  EBSDinit
                endif
        endcase

        'GOFIT': begin
Core_Print,'Starting pattern computation'
                EBSDpattern = EBSDCalc()
                wset,Efitdata.drawID
                tvscl,EBSDpattern
        endcase

 	'QUIT': begin
		Efitwritepreferences
; do a general cleanup of potentially open widgets
                if (XRegistered("Efit_control") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.controlbase, /DESTROY
                endif
                if (XRegistered("Efit_display") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.displaybase, /DESTROY
                endif
 		Core_Print,'Quitting program',/blank
                wait,1.0
		WIDGET_CONTROL, Efitwidget_s.base, /DESTROY
		!EXCEPT=1
	endcase



;fitUserLabel = ['DETL','DETOMEGA','DETXPC','DETYPC','DETGAMMA','DETphi1','DETphi','DETphi2']
        'DETL' : begin
                Efitdata.detL = Core_WidgetEvent( Efitwidget_s.fitValue[0],  'Scintillator distance set to [micron] ', '(F9.2)', /flt)
	endcase
        'DETOMEGA' : begin
                Efitdata.detomega = Core_WidgetEvent( Efitwidget_s.fitValue[1],  'Sample omega angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETXPC' : begin
                Efitdata.detxpc = Core_WidgetEvent( Efitwidget_s.fitValue[2],  'Pattern center x set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETYPC' : begin
                Efitdata.detypc = Core_WidgetEvent( Efitwidget_s.fitValue[3],  'Pattern center y set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETGAMMA' : begin
                Efitdata.detgamma = Core_WidgetEvent( Efitwidget_s.fitValue[4],  'Intensity gammma value set to ', '(F9.2)', /flt)
	endcase
        'DETphi1' : begin
                Efitdata.detphi1 = Core_WidgetEvent( Efitwidget_s.fitValue[3],  'Euler phi1 angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETphi' : begin
                Efitdata.detphi = Core_WidgetEvent( Efitwidget_s.fitValue[3],  'Euler Phi angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETphi2' : begin
                Efitdata.detphi2 = Core_WidgetEvent( Efitwidget_s.fitValue[3],  'Euler phi2 angle set to [degrees] ', '(F9.2)', /flt)
	endcase

;fitStepLabel = ['DETsL','DETsOMEGA','DETsXPC','DETsYPC','DETsGAMMA','DETsphi1','DETsphi','DETsphi2']
        'DETsL' : begin
                Efitdata.detsL = Core_WidgetEvent( Efitwidget_s.fitStep[0],  'Scintillator distance step size set to [micron] ', '(F9.2)', /flt)
	endcase
        'DETsOMEGA' : begin
                Efitdata.detsomega = Core_WidgetEvent( Efitwidget_s.fitStep[1],  'Sample omega angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETsXPC' : begin
                Efitdata.detsxpc = Core_WidgetEvent( Efitwidget_s.fitStep[2],  'Pattern center x step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETsYPC' : begin
                Efitdata.detsypc = Core_WidgetEvent( Efitwidget_s.fitStep[3],  'Pattern center y step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETsGAMMA' : begin
                Efitdata.detsgamma = Core_WidgetEvent( Efitwidget_s.fitStep[4],  'Intensity gammma step size value set to ', '(F9.2)', /flt)
	endcase
        'DETsphi1' : begin
                Efitdata.detsphi1 = Core_WidgetEvent( Efitwidget_s.fitStep[3],  'Euler phi1 angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETsphi' : begin
                Efitdata.detsphi = Core_WidgetEvent( Efitwidget_s.fitStep[3],  'Euler Phi angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETsphi2' : begin
                Efitdata.detsphi2 = Core_WidgetEvent( Efitwidget_s.fitStep[3],  'Euler phi2 angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase

;fitOnOffLabel = ['DEToL','DEToOMEGA','DEToXPC','DEToYPC','DEToGAMMA','DETophi1','DETophi','DETophi2']
; these are dealt with in Efitevent.pro

;fitUpLabel = ['DETuL','DETuOMEGA','DETuXPC','DETuYPC','DETuGAMMA','DETuphi1','DETuphi','DETuphi2']
        'DETuL' : begin
                Efitdata.detL += float(Efitdata.detmL)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detL,FORMAT='(F9.2)'), Efitwidget_s.fitValue[0]
                Core_Print, 'Scintillator distance set to [micron] '+string(Efitdata.detL,FORMAT='(F9.2)')
	endcase
        'DETuOMEGA' : begin
                Efitdata.detomega += float(Efitdata.detmomega)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detomega,FORMAT='(F9.2)'), Efitwidget_s.fitValue[1]
                Core_Print, 'Sample omega angle set to [degree] '+string(Efitdata.detomega,FORMAT='(F9.2)')
	endcase
        'DETuXPC' : begin
                Efitdata.detxpc += float(Efitdata.detmxpc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detxpc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[2]
                Core_Print, 'Pattern center x set to [pixels] '+string(Efitdata.detxpc,FORMAT='(F9.2)')
	endcase
        'DETuYPC' : begin
                Efitdata.detypc += float(Efitdata.detmypc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detypc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[3]
                Core_Print, 'Pattern center y set to [pixels] '+string(Efitdata.detypc,FORMAT='(F9.2)')
	endcase
        'DETuGAMMA' : begin
                Efitdata.detgamma += float(Efitdata.detmgamma)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detgamma,FORMAT='(F9.2)'), Efitwidget_s.fitValue[4]
                Core_Print, 'Intensity gamma value set to '+string(Efitdata.detgamma,FORMAT='(F9.2)')
	endcase
        'DETuphi1' : begin
                Efitdata.detphi1 += float(Efitdata.detmphi1)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
                Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
	endcase
        'DETuphi' : begin
                Efitdata.detphi += float(Efitdata.detmphi)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
                Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
	endcase
        'DETuphi2' : begin
                Efitdata.detphi2 += float(Efitdata.detmphi2)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
                Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')
	endcase

;fitDownLabel = ['DETdL','DETdOMEGA','DETdXPC','DETdYPC','DETdGAMMA','DETdphi1','DETdphi','DETdphi2']
        'DETdL' : begin
                Efitdata.detL -= float(Efitdata.detmL)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detL,FORMAT='(F9.2)'), Efitwidget_s.fitValue[0]
                Core_Print, 'Scintillator distance set to [micron] '+string(Efitdata.detL,FORMAT='(F9.2)')
	endcase
        'DETdOMEGA' : begin
                Efitdata.detomega -= float(Efitdata.detmomega)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detomega,FORMAT='(F9.2)'), Efitwidget_s.fitValue[1]
                Core_Print, 'Sample omega angle set to [degree] '+string(Efitdata.detomega,FORMAT='(F9.2)')
	endcase
        'DETdXPC' : begin
                Efitdata.detxpc -= float(Efitdata.detmxpc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detxpc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[2]
                Core_Print, 'Pattern center x set to [pixels] '+string(Efitdata.detxpc,FORMAT='(F9.2)')
	endcase
        'DETdYPC' : begin
                Efitdata.detypc -= float(Efitdata.detmypc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detypc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[3]
                Core_Print, 'Pattern center y set to [pixels] '+string(Efitdata.detypc,FORMAT='(F9.2)')
	endcase
        'DETdGAMMA' : begin
                Efitdata.detgamma -= float(Efitdata.detmgamma)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detgamma,FORMAT='(F9.2)'), Efitwidget_s.fitValue[4]
                Core_Print, 'Intensity gamma value set to '+string(Efitdata.detgamma,FORMAT='(F9.2)')
	endcase
        'DETdphi1' : begin
                Efitdata.detphi1 -= float(Efitdata.detmphi1)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
                Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
	endcase
        'DETdphi' : begin
                Efitdata.detphi -= float(Efitdata.detmphi)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
                Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
	endcase
        'DETdphi2' : begin
                Efitdata.detphi2 -= float(Efitdata.detmphi2)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
                Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')
	endcase

;fitManualStepLabel = ['DETmL','DETmOMEGA','DETmXPC','DETmYPC','DETmGAMMA','DETmphi1','DETmphi','DETmphi2']
        'DETmL' : begin
                Efitdata.detmL = Core_WidgetEvent( Efitwidget_s.fitManualStep[0],  'Scintillator distance manual step size set to [micron] ', '(F9.2)', /flt)
	endcase
        'DETmOMEGA' : begin
                Efitdata.detmomega = Core_WidgetEvent( Efitwidget_s.fitManualStep[1],  'Sample omega angle manual step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmXPC' : begin
                Efitdata.detmxpc = Core_WidgetEvent( Efitwidget_s.fitManualStep[2],  'Pattern center x manual step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETmYPC' : begin
                Efitdata.detmypc = Core_WidgetEvent( Efitwidget_s.fitManualStep[3],  'Pattern center y manual step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETmGAMMA' : begin
                Efitdata.detmgamma = Core_WidgetEvent( Efitwidget_s.fitManualStep[4],  'Intensity gammma manual step size value set to ', '(F9.2)', /flt)
	endcase
        'DETmphi1' : begin
                Efitdata.detmphi1 = Core_WidgetEvent( Efitwidget_s.fitManualStep[3],  'Euler phi1 angle manual step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmphi' : begin
                Efitdata.detmphi = Core_WidgetEvent( Efitwidget_s.fitManualStep[3],  'Euler Phi angle step manual size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmphi2' : begin
                Efitdata.detmphi2 = Core_WidgetEvent( Efitwidget_s.fitManualStep[3],  'Euler phi2 angle step manual size set to [degrees] ', '(F9.2)', /flt)
	endcase






  else: MESSAGE, "EBSDfit_event: Event User Step "+eventval+" Not Found"

  endcase


endelse

end
