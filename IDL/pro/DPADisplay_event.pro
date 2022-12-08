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
; EMsoft:DPADisplay_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: SDPAisplay_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Electron backscatter diffraction pattern display event handler
;
;> @date 06/28/16 MDG 1.0 initial version
;--------------------------------------------------------------------------
pro DPADisplay_event, event

;------------------------------------------------------------
; common blocks
common DPA_widget_common, DPAwidget_s
common DPA_data_common, DPAcommon, DPAdata


if (DPAcommon.eventverbose eq 1) then help,event,/structure

if (event.id eq DPAwidget_s.base) then begin
  DPAcommon.xlocation = event.x
  DPAcommon.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
        'DPFILE1': begin
          DPAcommon.currentphase = 0
          DPAgetfilename,validfile
          DPAloadfile
        endcase

        'DPFILE2': begin
          DPAcommon.currentphase = 1
          DPAgetfilename,validfile
          DPAloadfile
        endcase

 	'QUIT': begin
		;DPAwritepreferences
; do a general cleanup of potentially open widgets
 		Core_Print,'Quitting program',/blank
                ;if (XRegistered("") NE 0) then WIDGET_CONTROL, DPAwidget_s., /DESTROY
		WIDGET_CONTROL, DPAwidget_s.base, /DESTROY
		!EXCEPT=1
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 



