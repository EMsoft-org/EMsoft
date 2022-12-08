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
; EMsoft:OMETbuttonevent.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETbuttonevent.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief special event handler for all the CW_BGROUP calls, since CW_BGROUP does not support event_pro
;
;> @date 02/15/17 MDG 1.0 first version
;--------------------------------------------------------------------------
function OMETbuttonevent, event

;------------------------------------------------------------
; common blocks  (OMET = Optical Microscopy Ellipsometry Tool)
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common CommonCore, status, logmode, logunit

if (OMETdata.eventverbose eq 1) then print,'entering OMETbuttonevent'

if (OMETdata.eventverbose eq 1) then help,event,/structure

WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

CASE eventval OF

 'LOGFILE':  begin
; toggle the log mode 
		if (OMETdata.logmode eq 0) then begin
		   Core_Print,'Turning log mode on',/blank
		 q = systime()
 		 z = strsplit(q,' ',/extract,/regex)
 		 OMETdata.logname = 'EBSDDisplay'+z[0]+z[1]+z[2]+'_'+z[4]+'_'+z[3]+'.log'
		   Core_Print,'Log file: '+OMETdata.logname
		 OMETdata.logmode = 1
		 logmode = 1
		 openw,OMETdata.logunit,OMETdata.logname
		 OMETdata.logfileopen = 1
		end else begin
		   Core_Print,'Turning log mode off',/blank
		 if (OMETdata.logfileopen eq 1) then begin
		   close,OMETdata.logunit
		   OMETdata.logfileopen = 0
		 endif
    	 OMETdata.logmode = 0
		 logmode = 0
		endelse
	  endcase

else: MESSAGE, "Event User Value Not Found"

endcase

return,eventval
end 
