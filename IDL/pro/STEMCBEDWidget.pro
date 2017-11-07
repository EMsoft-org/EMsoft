;
; Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
; CTEMsoft2013:STEMCBEDWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMCBEDWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the CBED widget
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro STEMCBEDWidget,dummy
;
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall


;------------------------------------------------------------
; create the top level widget
widget_s.cbedbase = WIDGET_BASE(TITLE='CBED Pattern Display', $
                        /COLUMN, $
                        XSIZE=data.patx+30, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='STEMCBEDwidget_event', $
                        XOFFSET=data.cbedxlocation, $
                        YOFFSET=data.cbedylocation)


;------------------------------------------------------------
; block 2 contains the button groups and save button
block2 = WIDGET_BASE(widget_s.cbedbase, $
			/FRAME, $
			/ROW)

vals = ['Off','On']
widget_s.cbedlegendbgroup = CW_BGROUP(block2, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstr, $
			LABEL_LEFT = 'Scale Bar', $
			/FRAME, $
                        EVENT_FUNC='STEMevent', $
			UVALUE='CBEDLEGEND', $
			SET_VALUE=data.cbedlegend)


vals = ['jpeg','tiff','bmp']
widget_s.cbedformatbgroup = CW_BGROUP(block2, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstr, $
			LABEL_LEFT = 'File Format', $
			/FRAME, $
                        EVENT_FUNC='STEMevent', $
			UVALUE='CBEDFORMAT', $
			SET_VALUE=data.cbedformat)

; and, finally, a save and a close button
widget_s.savecbed = WIDGET_BUTTON(block2, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='STEMCBEDWidget_event', $
			UVALUE='SAVECBED', $
			/ALIGN_RIGHT)

widget_s.closecbed = WIDGET_BUTTON(block2, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='STEMCBEDWidget_event', $
			UVALUE='CLOSECBED', $
			/ALIGN_RIGHT)


;------------------------------------------------------------
; create the various blocks
; block 1 contains the drawing windows
block1 = WIDGET_BASE(widget_s.cbedbase, $
			/FRAME, $
			/ALIGN_CENTER, $
			/COLUMN)



;------------------------------------------------------------
block2a = WIDGET_BASE(block1, $
			/FRAME, $
			/ROW)

label1 = WIDGET_LABEL(block2a, $
			VALUE='min/max', $
			FONT=fontstr, $
			XSIZE=75, $
			YSIZE=30, $
			/ALIGN_LEFT)

widget_s.CBEDmin= WIDGET_TEXT(block2a, $
			VALUE=string(data.CBEDmin,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

widget_s.CBEDmax= WIDGET_TEXT(block2a, $
			VALUE=string(data.CBEDmax,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

vals = ['1','2','4']
case (data.CBEDzoom) of
 '1' : cbedzoom = 0
 '2' : cbedzoom = 1
 '4' : cbedzoom = 2
endcase

widget_s.CBEDzoom = CW_BGROUP(block2a, $
			vals, $
			/ROW, $
			/EXCLUSIVE, $
			/NO_RELEASE, $
			LABEL_LEFT = 'Zoom Factor', $
			FONT=fontstr, $
			/FRAME, $
                        EVENT_FUNC='STEMevent', $
			UVALUE='CBEDZOOM', $
			SET_VALUE=cbedzoom)

vals = ['normal','logarithmic']
widget_s.cbedmodebgroup = CW_BGROUP(block2a, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			FONT=fontstr, $
			/EXCLUSIVE, $
			LABEL_LEFT = 'Intensity Scale', $
			/FRAME, $
                        EVENT_FUNC='STEMevent', $
			UVALUE='CBEDMODE', $
			SET_VALUE=data.cbedmode)

widget_s.CBEDdraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/BUTTON_EVENTS, $
			EVENT_PRO='STEMCBEDWidget_event', $
			UVALUE = 'DRAWCBEDPATTERN', $
			XSIZE=data.patx, $
			YSIZE=data.paty)



;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.cbedbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.CBEDdraw, GET_VALUE=drawID
widget_s.CBEDdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"STEMCBEDWidget",widget_s.cbedbase,/NO_BLOCK

end
