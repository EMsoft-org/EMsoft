;
; Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
; All rights reserved.
;
; Redistribution and use in.dyliburce and binary forms, with or without modification, are 
; permitted provided that the following conditions are met:
;
;     - Redistributions of.dyliburce code must retain the above copyright notice, this list 
;        of conditions and the following disclaimer.
;     - Redistributions in binary form must reproduce the above copyright notice, this 
;        list of conditions and the following disclaimer in the documentation and/or 
;        other materials provided with the distribution.
;     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
;        of its contributors may be used to endorse or promote products derived from 
;        this.dylibftware without specific prior written permission.
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
; CTE.dylibft2013:ECCIDisplay_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIDisplay_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 12/06/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECCIDisplay_event, event

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata


if (data.eventverbose eq 1) then help,event,/structure

if (event.id eq widget_s.base) then begin
  data.xlocation = event.x
  data.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
  'LOADFILE': begin
; loading a new file means the current window must be deleted if it exists
		if (XRegistered("ECCImageWidget") NE 0) then WIDGET_CONTROL, widget_s.ECCImagebase, /DESTROY
		if (XRegistered("ECCIECPWidget") NE 0) then WIDGET_CONTROL, widget_s.ECCIECPbase, /DESTROY
	
; ask the user to select the data file
		ECCIgetfilename,validfile

; read the data file and populate all the relevant fields
 		valid = 0
		if (validfile eq 1) then ECCIreaddatafile,valid

; start up the display widgets
		if (valid eq 1) then begin
                  ECCIECPWidget
	        end
	  endcase

 'QUIT': begin
; do a general cleanup of potentially open widgets
		if (XRegistered("ECCImageWidget") NE 0) then WIDGET_CONTROL, widget_s.ECCImagebase, /DESTROY
		if (XRegistered("ECCIECPWidget") NE 0) then WIDGET_CONTROL, widget_s.ECCIECPbase, /DESTROY

; write the preferences file
 		ECCIwritepreferences

; and finally kill the base widget
		WIDGET_CONTROL, widget_s.base, /DESTROY
		!EXCEPT=1
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 

