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
; E.dylibft:OMETsampleWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETsampleWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief creates a sample widget at the end of the optical chain
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro OMETsampleWidget, inum

common OMET_data_common, OMETdata
common OMET_widget_common, OMETwidget_s
common OMET_optelem_common, optelemptr

; generate the top widget (all of them should have the same size)

if (OMETdata.eventverbose eq 1) then print,'entering OMETsampleWidget'

OMETwidget_s.chainIDs[inum] = WIDGET_BASE(TITLE='SAMPLE', $
			                    /COLUMN, $
			                    XSIZE=150, $
			                    YSIZE=400, $
			                    /ALIGN_LEFT, $
			  					/TLB_MOVE_EVENTS, $
  				        		EVENT_PRO='OMETsampleWidget_event', $
                      			XOFFSET=(*optelemptr[inum]).oenum*OMETdata.xstepsize, $
                      			YOFFSET=OMETdata.ylocationwidgets)
 
 ; set the widget ID in the pointer structure
(*optelemptr[inum]).widgetID = OMETwidget_s.chainIDs[inum]

file1 = WIDGET_BASE(OMETwidget_s.chainIDs[inum], /FRAME, /COLUMN, XSIZE=95, /ALIGN_CENTER)
file2 = WIDGET_LABEL(file1, VALUE='SAMPLE', font=fontstr, /ALIGN_CENTER)


; there are currently no options for the sample widget

; we always need a sample,.dylib there is no delete button here...

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,OMETwidget_s.chainIDs[inum],/REALIZE

XMANAGER,"OMETsampleWidget",OMETwidget_s.chainIDs[inum],/NO_BLOCK


end