;
; Copyright (c) 2017-, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:OMETpolarizerWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETpolarizerWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for polarizer widget
;
;> @date 02/16/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETpolarizerWidget_event, event

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then help,event,/structure
if (OMETdata.eventverbose eq 1) then Print,' entering OMETpolarizerWidget_event'


; find which chainID gave rise to this particular event and then handle the proper widget
curchainID = OMETfindchainID(event.id)

if (curchainID eq -1) then begin
; this is a regular event id, not one that attempts to move the widget
  	WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  	print,' User value requested :', eventval
; determine from the eventval which of the widget's components requested an event handler;
; this is important since there may be more than one polarizer widget!
	res = strsplit(eventval,'_',/extract)
	creationorder = fix(res[1])  
	eventname = res[0]
  curchainID = OMETgetcurchainID(creationorder)
  	CASE eventname OF
  	'PXVAL': begin
              (*optelemptr[curchainID]).px = Core_WidgetEvent( OMETwidget_s.pxvals[creationorder],  ' polarizer px set to ', '(F10.6)', /flt)
              optelemptr[curchainID] = PTR_NEW(OMETinitpolarizer(curchainID,/update))
             end
  	'PYVAL': begin
              (*optelemptr[curchainID]).py = Core_WidgetEvent( OMETwidget_s.pyvals[creationorder],  ' polarizer py set to ', '(F10.6)', /flt)
              optelemptr[curchainID] = PTR_NEW(OMETinitpolarizer(curchainID,/update))
            end
  	'PVAL': begin
              (*optelemptr[curchainID]).pmag = Core_WidgetEvent( OMETwidget_s.pvals[creationorder],  ' polarizer |p| set to ', '(F10.6)', /flt)
              optelemptr[curchainID] = PTR_NEW(OMETinitpolarizer(curchainID,/update))
            end
  	'ALPHAVAL': begin
              (*optelemptr[curchainID]).alpha = Core_WidgetEvent( OMETwidget_s.alphavals[creationorder],  ' polarizer angle set to [deg] ', '(F10.6)', /flt)
              optelemptr[curchainID] = PTR_NEW(OMETinitpolarizer(curchainID,/update))
            end
  	'PTHETA': begin
              (*optelemptr[curchainID]).theta = Core_WidgetEvent( OMETwidget_s.pthetas[creationorder],  ' polarizer rotation angle set to [deg] ', '(F10.6)', /flt)*!dtor
              optelemptr[curchainID] = PTR_NEW(OMETinitpolarizer(curchainID,/update))
            end
    'DELETE': OMETremovewidget, creationorder
	else: MESSAGE, "OMETpolarizerWidget_event: Event User Value "+eventval+" Not Found "
  endcase
  OMETupdateStokesVector
end else begin
; we're moving this widget, which may mean that others need to be moved as well...
  OMETshufflewidgets, event.x, curchainID
endelse




end