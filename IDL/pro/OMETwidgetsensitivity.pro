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
; EMsoft:OMETwidgetsensitivity.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETwidgetsensitivity.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief turn the optical element widget sensitivity to widget movement on or off
;
;> @date 02/16/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETwidgetsensitivity, curchainID, off=off, on=on

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata


if (keyword_set(off)) then begin
if (OMETdata.eventverbose eq 1) then print,'entering OMETwidgetsensitivity: ',curchainID,' off'
	for i=0,OMETgetnextfreepos()-1 do begin
	;if (i ne curchainID) then i
		WIDGET_CONTROL, OMETwidget_s.chainIDs[i], TLB_MOVE_EVENTS=0
	endfor
endif

if (keyword_set(on)) then begin
if (OMETdata.eventverbose eq 1) then print,'entering OMETwidgetsensitivity: ',curchainID,' on'
	for i=0,OMETgetnextfreepos()-1 do begin
		;if (i ne curchainID) then 
		WIDGET_CONTROL, OMETwidget_s.chainIDs[i], TLB_MOVE_EVENTS=1
	endfor
endif

end 
