;
; Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
; CTEMsoft2013:CBEDDisplay_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDDisplay_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 06/13/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro CBEDDisplay_event, event

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist


if (data.eventverbose eq 1) then help,event,/structure

if (event.id eq widget_s.base) then begin
  data.xlocation = event.x
  data.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
  'STARTLACBEDWIDGET': begin
		if (XRegistered("CBEDCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDDrawbase, /DESTROY
		if (XRegistered("CBEDCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDbase, /DESTROY
		if (XRegistered("CBEDMBCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDDrawBase, /DESTROY
		if (XRegistered("CBEDMBCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDBase, /DESTROY
		if (XRegistered("CBEDDrawWidget") EQ 0) then CBEDDrawWidget
		if (XRegistered("CBEDLACBEDWidget") EQ 0) then CBEDLACBEDWidget
	endcase

  'STARTCBEDWIDGET': begin
		if (XRegistered("CBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDDrawbase, /DESTROY
		if (XRegistered("CBEDLACBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDbase, /DESTROY
		if (XRegistered("CBEDMBCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDDrawBase, /DESTROY
		if (XRegistered("CBEDMBCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDBase, /DESTROY
		if (XRegistered("CBEDCBEDDrawWidget") EQ 0) then CBEDCBEDDrawWidget
		if (XRegistered("CBEDCBEDWidget") EQ 0) then CBEDCBEDWidget
	endcase

  'STARTMBCBEDWIDGET': begin
		if (XRegistered("CBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDDrawbase, /DESTROY
		if (XRegistered("CBEDLACBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDbase, /DESTROY
		if (XRegistered("CBEDCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDDrawbase, /DESTROY
		if (XRegistered("CBEDCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDbase, /DESTROY
		if (XRegistered("CBEDMBCBEDDrawWidget") EQ 0) then CBEDMBCBEDDrawWidget
		if (XRegistered("CBEDMBCBEDWidget") EQ 0) then CBEDMBCBEDWidget
	endcase

  'LOADLACBEDFILE': begin
; loading a new file means that a bunch of variables need to be reset
		if (XRegistered("CBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDDrawbase, /DESTROY
		if (XRegistered("CBEDLACBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDbase, /DESTROY
		if (XRegistered("CBEDCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDDrawbase, /DESTROY
		if (XRegistered("CBEDCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDbase, /DESTROY
		if (XRegistered("CBEDMBCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDDrawBase, /DESTROY
		if (XRegistered("CBEDMBCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDBase, /DESTROY
	
; ask the user to select the data file
		CBEDgetfilename,validfile,/LACBED

; read the data file and populate all the relevant fields
		if (validfile eq 1) then CBEDreaddatafile,/LACBED
	  endcase

  'LOADMBCBEDFILE': begin
; loading a new file means that a bunch of variables need to be reset
		if (XRegistered("CBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDDrawbase, /DESTROY
		if (XRegistered("CBEDLACBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDbase, /DESTROY
		if (XRegistered("CBEDCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDDrawbase, /DESTROY
		if (XRegistered("CBEDCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDbase, /DESTROY
		if (XRegistered("CBEDMBCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDDrawBase, /DESTROY
		if (XRegistered("CBEDMBCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDBase, /DESTROY
; ask the user to select the data file
		CBEDgetfilename,validfile,/MBCBED

; read the data file and populate all the relevant fields
		if (validfile eq 1) then CBEDreaddatafile,/MBCBED
	  endcase

 'QUIT': begin
; do a general cleanup of potentially open widgets
		CBEDprint,'Quitting program',/blank
		if (XRegistered("CBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDDrawbase, /DESTROY
		if (XRegistered("CBEDLACBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.LACBEDbase, /DESTROY
		if (XRegistered("CBEDCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDDrawbase, /DESTROY
		if (XRegistered("CBEDCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.CBEDbase, /DESTROY
		if (XRegistered("CBEDMBCBEDDrawWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDDrawBase, /DESTROY
		if (XRegistered("CBEDMBCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.MBCBEDBase, /DESTROY

; write the preferences file
		CBEDwritepreferences

; close the log file if it is open
		if (data.logfileopen eq 1) then begin
		  close,data.logunit
		endif
; wait briefly
		wait,2.0
; and finally kill the base widget
		WIDGET_CONTROL, widget_s.base, /DESTROY
		!EXCEPT=1
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
