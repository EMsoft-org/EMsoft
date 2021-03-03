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
; CTEMsoft2013:ECCIevent.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIevent.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief special event handler for all the CW_BGROUP calls, since CW_BGROUP does not support event_pro
;
;> @date 06/13/13 MDG 1.0 first version
;--------------------------------------------------------------------------
function ECCIevent, event

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata

if (data.eventverbose eq 1) then help,event,/structure


WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

CASE eventval OF

  'MOSAICSCALE': begin
		WIDGET_CONTROL, get_value=val,widget_s.ECCImosaicbgroup
		data.mosaicscale = fix(val[0])
	  endcase

  'ECCIFORMAT': begin
		WIDGET_CONTROL, get_value=val,widget_s.ECCIformatbgroup
		data.ecciformat = fix(val[0])
	  endcase

  'ECPFORMAT': begin
		WIDGET_CONTROL, get_value=val,widget_s.ECPformatbgroup
		ECPdata.ecpformat = fix(val[0])
	  endcase

  'ECPGRID': begin
		WIDGET_CONTROL, get_value=val,widget_s.ECPgridbgroup
		ECPdata.ecpgrid = fix(val[0])
		ECCIECPshow
	  endcase

 'LOGFILE':  begin
; toggle the log mode 
		if (data.logmode eq 0) then begin
		   ECCIprint,'Turning log mode on',/blank
		 q = systime()
 		 z = strsplit(q,' ',/extract,/regex)
 		 data.logname = data.pathname+'/STEMDisplay'+z[0]+z[1]+z[2]+'_'+z[3]+'_'+z[4]+'.log'
		   ECCIprint,'Log file: '+data.logname
		 data.logmode = 1
		 openw,data.logunit,data.logname
		 data.logfileopen = 1
		end else begin
		   ECCIprint,'Turning log mode off',/blank
		 if (data.logfileopen eq 1) then begin
		   close,data.logunit
		   data.logfileopen = 0
		 endif
	    	 data.logmode = 0
		endelse
	  endcase
  
else: MESSAGE, "Event User Value Not Found"

endcase

return,eventval
end 
