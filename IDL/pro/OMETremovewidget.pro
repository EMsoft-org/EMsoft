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
; EMsoft:OMETremovewidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETremovewidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief deletes a given widget and moves the remainder of the chain one spot to the left
;
;> @date 02/18/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETremovewidget, creationorder

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then print,' entering OMETremovewidget: ', creationorder

if (creationorder lt 3) then begin
	Core_Print,'Stokes vector widgets and sample widget can not be deleted'
end else begin
	if (OMETdata.eventverbose eq 1) then OMETdebug
; if we're going to delete a widget, there's a good chance that others will need to be moved as well,
; so we need to turn off temporarily their sensitivity to move events to prevent their event handler
; routines from being called; this caused all sorts of problems during the debug phase of this program...
	curchainID = OMETgetcurchainID(creationorder)
	OMETwidgetsensitivity, curchainID, /off

; we'll delete this element's information structure and widget
	PTR_FREE, optelemptr[curchainID]
	WIDGET_CONTROL, OMETwidget_s.chainIDs[curchainID], /destroy

; next, we need to move the various pointers and reset the OMETdata.oenum value for all affected widgets
	for i=curchainID,OMETgetnextfreepos()-2 do begin
		optelemptr[i] = optelemptr[i+1]
		(*optelemptr[i]).oenum -= 1
		OMETwidget_s.chainIDs[i] = OMETwidget_s.chainIDs[i+1]
	    WIDGET_CONTROL, OMETwidget_s.chainIDs[i], TLB_SET_XOFFSET = (*optelemptr[i]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
  	    if (OMETdata.eventverbose eq 1) then OMETdebug
	endfor
	; and nullify the last pointer
	PTR_FREE, optelemptr[OMETgetnextfreepos()]
	OMETdata.numleft += 1
	if (OMETdata.eventverbose eq 1) then OMETdebug

; make the event handler routines sensitive to widget movements again
	OMETwidgetsensitivity, curchainID, /on

	WIDGET_CONTROL, OMETwidget_s.numleft, SET_VALUE=string(OMETdata.numleft,format="(I2)")
endelse

end
