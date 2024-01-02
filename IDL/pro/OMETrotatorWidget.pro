;
; Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:OMETrotatorWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETrotatorWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief creates a rotator widget at the end of the optical chain
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro OMETrotatorWidget, inum

common OMET_data_common, OMETdata
common OMET_widget_common, OMETwidget_s
common OMET_optelem_common, optelemptr

; generate the top widget (all of them should have the same size)

if (OMETdata.eventverbose eq 1) then print,'entering OMETrotatorWidget'

OMETwidget_s.chainIDs[inum] = WIDGET_BASE(TITLE='ROTATOR', $
			                    /COLUMN, $
			                    XSIZE=150, $
			                    YSIZE=400, $
			                    /ALIGN_CENTER, $
			  					/TLB_MOVE_EVENTS, $
		  						TLB_FRAME_ATTR=6, $
  				        		EVENT_PRO='OMETrotatorWidget_event', $
                      			XOFFSET=(*optelemptr[inum]).oenum * OMETdata.xstepsize, $
                      			YOFFSET=OMETdata.ylocationwidgets)
 
; set the widget ID in the pointer structure
(*optelemptr[inum]).widgetID = OMETwidget_s.chainIDs[inum]

; generate the widget base and structure
file1 = WIDGET_BASE(OMETwidget_s.chainIDs[inum], /FRAME, /COLUMN, XSIZE=140, /ALIGN_CENTER)

file2 = WIDGET_LABEL(file1, VALUE='ROTATOR', font=fontstr, /ALIGN_CENTER)

;					theta:float(0.0), $	; rotation angle (radians)

; rotation values
file2 = WIDGET_BASE(file1, /ROW, XSIZE=130, /ALIGN_CENTER)
OMETwidget_s.rotthetas[inum] = Core_WTextE(file2,'theta :', fontstr, 45, 25, 10, 1, string((*optelemptr[inum]).theta,format="(F10.6)"), $
							'RTHETA_'+string(inum,format="(I2.2)"),'OMETrotatorWidget_event')

file1 = WIDGET_BASE(OMETwidget_s.chainIDs[inum], /FRAME, /COLUMN, XSIZE=95, /ALIGN_CENTER)

OMETwidget_s.deletewidget[inum] = WIDGET_BUTTON(file1, $
			                        VALUE='Delete', $
			                        UVALUE='DELETE_'+string(inum,format="(I2.2)"), $
			                        EVENT_PRO='OMETrotatorWidget_event', $
			                        SENSITIVE=1, $
			                        /FRAME)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,OMETwidget_s.chainIDs[inum],/REALIZE

XMANAGER,"OMETrotatorWidget",OMETwidget_s.chainIDs[inum],/NO_BLOCK


end
