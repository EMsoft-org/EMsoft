;
; Copyright (c) 2017-, Marc De Graef Research Group/Carnegie Mellon University
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
; E.dylibft:OMETStokesVectorWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETStokesVectorWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for Stokes Vector widget
;
;> @date 02/16/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETStokesVectorWidget_event, event

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr



if (OMETdata.eventverbose eq 1) then help,event,/structure
if (OMETdata.eventverbose eq 1) then print,' entering OMETStokesVectorWidget_event'

; find which chainID gave rise to this particular event and then handle the proper widget
curchainID = OMETfindchainID(event.id)

if (curchainID eq -1) then begin
; this is a regular event id, not one that attempts to move the widget
  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  curchainID=0
  CASE eventval OF
  	'SV0': begin
  			(*optelemptr[curchainID]).S[0] = Core_WidgetEvent( OMETwidget_s.inputSV0,  'Input SV0 set to ', '(F10.6)', /flt)
  			optelemptr[curchainID] = PTR_NEW(OMETinitStokesVector(curchainID,(*optelemptr[curchainID]).S,/update))
           end
  	'SV1': begin
  			(*optelemptr[curchainID]).S[1] = Core_WidgetEvent( OMETwidget_s.inputSV1,  'Input SV1 set to ', '(F10.6)', /flt)
  			optelemptr[curchainID] = PTR_NEW(OMETinitStokesVector(curchainID,(*optelemptr[curchainID]).S,/update))
           end
  	'SV2': begin
  			(*optelemptr[curchainID]).S[2] = Core_WidgetEvent( OMETwidget_s.inputSV2,  'Input SV2 set to ', '(F10.6)', /flt)
  			optelemptr[curchainID] = PTR_NEW(OMETinitStokesVector(curchainID,(*optelemptr[curchainID]).S,/update))
           end
  	'SV3': begin
  			(*optelemptr[curchainID]).S[3] = Core_WidgetEvent( OMETwidget_s.inputSV3,  'Input SV3 set to ', '(F10.6)', /flt)
  			optelemptr[curchainID] = PTR_NEW(OMETinitStokesVector(curchainID,(*optelemptr[curchainID]).S,/update))
           end
	else: MESSAGE, "OMETStokesVectorWidget_event: Event User Value"+eventval+" Not Found "
  endcase
  OMETupdateStokesVector
end else begin
; is this the input Stokes vector widget ?
	if ((curchainID eq 0) or (curchainID eq OMETgetnextfreepos()-1)) then begin
	; this is the input Stokes vector widget and the user if trying to move the widget,
	; which we do not allow at this point in time...
		if ((event.x ne (*optelemptr[curchainID]).oenum*OMETdata.xstepsize) or (event.y ne OMEtdata.ylocationwidgets)) then begin
		; move the widget back to its original location...
			WIDGET_CONTROL, OMETwidget_s.chainIDs[curchainID], TLB_SET_XOFFSET = (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
		endif	
	endif	

endelse

end