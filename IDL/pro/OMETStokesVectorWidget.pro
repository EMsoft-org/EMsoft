;
; Copyright (c) 2017-, Marc De Graef/Carnegie Mellon University
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
; EMsoft:OMETStokesVectorWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETStokesVectorWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief creates a Stokes Vector widget at the end of the optical chain
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro OMETStokesVectorWidget, inum, output=output

common OMET_data_common, OMETdata
common OMET_widget_common, OMETwidget_s
common OMET_optelem_common, optelemptr

; generate the top widget (all of them should have the same size)

if (OMETdata.eventverbose eq 1) then print,'entering OMETStokesVectorWidget'

OMETwidget_s.chainIDs[inum] = WIDGET_BASE(TITLE='SV', $
			                    /COLUMN, $
			                    XSIZE=150, $
			                    YSIZE=400, $
			                    /ALIGN_CENTER, $
			  					/TLB_MOVE_EVENTS, $
  				        		EVENT_PRO='OMETStokesVectorWidget_event', $
                      			XOFFSET= (*optelemptr[inum]).oenum * OMETdata.xstepsize, $
                      			YOFFSET=OMETdata.ylocationwidgets)
 
 ; set the widget ID in the pointer structure
(*optelemptr[inum]).widgetID = OMETwidget_s.chainIDs[inum]

file1 = WIDGET_BASE(OMETwidget_s.chainIDs[inum], /FRAME, /COLUMN, XSIZE=140, /ALIGN_CENTER)

;---------- 
if (KEYWORD_SET(output)) then begin
	file2 = WIDGET_LABEL(file1, VALUE='OUTPUT SV', font=fontstr, /ALIGN_CENTER)
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.outputSV0= Core_WText(file2,'S0:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[0],format="(F10.6)"))
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.outputSV1= Core_WText(file2,'S1:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[1],format="(F10.6)"))
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.outputSV2= Core_WText(file2,'S2:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[2],format="(F10.6)"))
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.outputSV3= Core_WText(file2,'S3:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[3],format="(F10.6)"))
end else begin
	file2 = WIDGET_LABEL(file1, VALUE='INPUT SV', font=fontstr, /ALIGN_CENTER)
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.inputSV0= Core_WTextE(file2,'S0:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[0],format="(F10.6)"),'SV0','OMETStokesVectorWidget_event')
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.inputSV1= Core_WTextE(file2,'S1:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[1],format="(F10.6)"),'SV1','OMETStokesVectorWidget_event')
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.inputSV2= Core_WTextE(file2,'S2:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[2],format="(F10.6)"),'SV2','OMETStokesVectorWidget_event')
	file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
	OMETwidget_s.inputSV3= Core_WTextE(file2,'S3:', fontstr, 25, 25, 12, 1, string((*optelemptr[inum]).S[3],format="(F10.6)"),'SV3','OMETStokesVectorWidget_event')
endelse

; the Stokes vector widgets do not have a delete button because we always need a source and a detector

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,OMETwidget_s.chainIDs[inum],/REALIZE

XMANAGER,"OMETStokesVectorWidget",OMETwidget_s.chainIDs[inum],/NO_BLOCK


end