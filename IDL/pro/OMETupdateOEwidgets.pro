;
; Copyright (c) 2017-, Marc De Graef/Carnegie Mellon University
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
; E.dylibft:OMETupdateOEwidgets.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETupdateOEwidgets.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief updates the series of optical elements widgets
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro OMETupdateOEwidgets, dummy

common OMET_data_common, OMETdata
common OMET_widget_common, OMETwidget_s
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then print,'entering OMETupdateOEwidgets'

; for every widget that does not already exist, create it in the order listed
; in the optelemptr array;  when this is called with the ADD optional parameter,
; then we assume that all the others already exists and we add an element in the 
; next free slot and generate its widget.

for i = 0, OMETgetnextfreepos()-1 do begin
	if ((*optelemptr[i]).widgetexists ne 1) then begin
		case (*optelemptr[i]).oetype of
			1: begin
				OMETpolarizerWidget, i
				(*optelemptr[i]).widgetexists = 1
				(*optelemptr[i]).widgetID = OMETwidget_s.chainIDs[i]
			end
			2: begin
				OMETretarderWidget, i
				(*optelemptr[i]).widgetexists = 1
				(*optelemptr[i]).widgetID = OMETwidget_s.chainIDs[i]
			end
			3: begin
				OMETrotatorWidget, i
				(*optelemptr[i]).widgetexists = 1
				(*optelemptr[i]).widgetID = OMETwidget_s.chainIDs[i]
			end
			4: begin
				OMETStokesVectorWidget, i
				(*optelemptr[i]).widgetexists = 1
				(*optelemptr[i]).widgetID = OMETwidget_s.chainIDs[i]
			end
			5: begin
				OMETsampleWidget, i
				(*optelemptr[i]).widgetexists = 1
				(*optelemptr[i]).widgetID = OMETwidget_s.chainIDs[i]
			end
			6: begin
				OMETStokesVectorWidget, i, /output
				(*optelemptr[i]).widgetexists = 1
				(*optelemptr[i]).widgetID = OMETwidget_s.chainIDs[i]
			end

			else: Core_Print,'Optical element type does not exist'
		endcase
	endif
endfor

print,optelemptr
end