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
; EMsoft:ECPDetectorWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECPDetectorWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for detector widget
;
;> @date 10/30/15 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECPDetectorWidget_event, event

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common ECPdata, ECPattern


if (SEMdata.eventverbose eq 1) then help,event,/structure

; intercept the detector widget movement here 
if (event.id eq SEMwidget_s.detectorbase) then begin
  SEMdata.Detectorxlocation = event.x
  SEMdata.Detectorylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

  CASE eventval OF

 'DETW': SEMdata.detW = Core_WidgetEvent( SEMwidget_s.detW,  'Working distance set to [mm] ', '(F9.2)', /flt)
 'DETRI': SEMdata.detRi = Core_WidgetEvent( SEMwidget_s.detRi,  'Detector inner radius set to [mm] ', '(F9.2)', /flt)
 'DETRO': SEMdata.detRo = Core_WidgetEvent( SEMwidget_s.detRo,  'Detector outer radius set to [mm] ', '(F9.2)', /flt)

 'DETSAMPLEYTILT': SEMdata.detsampleytilt = Core_WidgetEvent( SEMwidget_s.detsampleytilt, 'Sample y-tilt angle set to [deg] ', '(F6.2)', /flt)
 'DETNUMSX': begin
                SEMdata.detnumsx = Core_WidgetEvent( SEMwidget_s.detnumsx, 'Scintillator number of pixels along x set to ', '(I4)', /lng)
                SEMdata.detnumsy = SEMdata.detnumsx
        endcase

 'DETTHETAC': SEMdata.detthetac = Core_WidgetEvent( SEMwidget_s.detthetac, 'Incident beam cone semi-angle to [deg] ', '(F6.2)', /flt)

 'DETBEAMCURRENT': SEMdata.detbeamcurrent = Core_WidgetEvent( SEMwidget_s.detbeamcurrent, 'Beam current set to [nA] ', '(F9.2)', /flt)
 'DETDWELLTIME': SEMdata.detdwelltime = Core_WidgetEvent( SEMwidget_s.detdwelltime, 'Dwell time set to [mu s] ', '(F9.2)', /flt)

 'DETphi1': SEMdata.detphi1 = Core_WidgetEvent( SEMwidget_s.detphi1, 'Euler angle phi1 set to [deg] ', '(F6.2)', /flt)
 'DETPhi': SEMdata.detphi = Core_WidgetEvent( SEMwidget_s.detphi, 'Euler angle Phi set to [deg] ', '(F6.2)', /flt)
 'DETphi2': SEMdata.detphi2 = Core_WidgetEvent( SEMwidget_s.detphi2, 'Euler angle phi2 set to [deg] ', '(F6.2)', /flt)

 'GETANGLEFILENAME': begin
; display a filesaving widget in the data folder with the file extension filled in
	  filename = DIALOG_PICKFILE(/write,path=SEMdata.pathname,title='Select angle input file')
	  if (filename ne '') then begin
	    SEMdata.ECPanglefilename = filename
	    WIDGET_CONTROL, set_value=filename, SEMwidget_s.ECPanglefilename
	    EBSDreadanglefile,filename,/list
	    WIDGET_CONTROL, SEMwidget_s.GoAngle, sensitive=1
	  end
	endcase

 'DISPLAYECP': begin
; first we need to make sure that the path to the fortran executables is known... this is stored in the 
; preferences file, but is initially set to 'path_unknown'
	  if (SEMdata.EMsoftpathname eq 'path_unknown') then begin
                SEMdata.EMsoftpathname = Core_getenv(/bin)+'Build/Bin'
                Core_print,'exeutable path set to '+SEMdata.EMsoftpathname 
	  end

; is the correct widget up on the screen ?
	  if XRegistered("ECPatternWidget") then begin
	    if (SEMdata.currentdisplaywidgetmode ne 0) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
	  end

; first we need to set up the array structures to do a call_external of the getECPatternsWrapper
; routine; and then we display the pattern in a new widget
	  status = 0
	  ECPExecute,status,/single

; then we create the EBSDpattern widget and let the user adjust the imaging parameters
	  if (status eq 1) then begin
	    if (XRegistered("ECPatternWidget") EQ 0) then ECPatternWidget,/single else ECPshowPattern,/single
	  end

	endcase

 'GOANGLE': begin
; is the correct widget up on the screen ?
	  if XRegistered("ECPatternWidget") then begin
	    if (SEMdata.currentdisplaywidgetmode ne 1) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
	  end

; first, create the nml file and execute the CTEMEBSD program
	  status = 0
	  ECPExecute,status

; then we create the EBSDpattern widget and let the user adjust the imaging parameters
	  if (status eq 1) then begin
	    if (XRegistered("ECPPatternWidget") EQ 0) then ECPatternWidget else ECPshowPattern
	  end

	endcase

;'GODICTIONARY': begin
; is the correct widget up on the screen ?
;  if XRegistered("EBSDPatternWidget") then begin
;    if (SEMdata.currentdisplaywidgetmode ne 1) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
;  end

; this does two things.  First of all, the CTEMEBSD program is called with the current
; parameters for the detector and microscope geometry, and the angle file
;
; Then, when the CTEMEBSD program has produced its output file, we create a new widget
; that displays these EBSD patterns; the user can then save selected patterns or all patterns.
; At this point, there is no option to change the imaging parameters; all the settings of the 
; other parts of the widget apply to this pattern calculation

; first, create the nml file and execute the CTEMEBSD program
;  status = 0
;  EBSDExecute,status

; then we create the EBSDpattern widget and let the user adjust the imaging parameters
;  if (status eq 1) then begin
;    if (XRegistered("EBSDPatternWidget") EQ 0) then EBSDPatternWidget else EBSDshowPattern
;  end

;endcase

 'CLOSEDETECTOR': begin
; kill the base widget
		WIDGET_CONTROL, SEMwidget_s.detectorbase, /DESTROY
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
