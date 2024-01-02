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
; CTEMsoft2013:CBEDDrawWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDDrawWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Draw widget for LACBED
;
;> @date 10/09/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro CBEDDrawWidget,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common fontstrings, fontstr, fontstrlarge, fontstrsmall

; note that there are no user input parameters here, so no need for an event handler routine

;------------------------------------------------------------
; create the top level widget
widget_s.LACBEDDrawbase = WIDGET_BASE(TITLE='LACBED Pattern Widget', $
                        /ROW, $
                        XSIZE=2*513+20, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
                        XOFFSET=data.LACBEDPatternxlocation, $
                        YOFFSET=data.LACBEDPatternylocation)

;------------------------------------------------------------
; create the various blocks
; block 1 contains the BF drawing window
block1 = WIDGET_BASE(widget_s.LACBEDDrawbase, $
			/FRAME, $
			/ALIGN_LEFT, $
			/COLUMN)

;------------------------------------------------------------
label1 = WIDGET_LABEL(block1, $
			VALUE='Bright Field', $
			FONT=fontstrlarge, $
			XSIZE=150, $
			YSIZE=30, $
			/ALIGN_CENTER)

widget_s.BFdraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			X_SCROLL_SIZE = 513, $
			Y_SCROLL_SIZE = 513, $
			XSIZE=data.datadims[0], $
			YSIZE=data.datadims[0])


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

widget_s.BFmin= WIDGET_TEXT(block2a, $
			VALUE=string(data.BFmin,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

widget_s.BFmax= WIDGET_TEXT(block2a, $
			VALUE=string(data.BFmax,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

;------------------------------------------------------------
; create the various blocks
; block 2 contains the DF/Eades drawing window
block2 = WIDGET_BASE(widget_s.LACBEDDrawbase, $
			/FRAME, $
			/ALIGN_LEFT, $
			/COLUMN)

;------------------------------------------------------------
label1 = WIDGET_LABEL(block2, $
			VALUE='Dark Field/Eades', $
			FONT=fontstrlarge, $
			XSIZE=150, $
			YSIZE=30, $
			/ALIGN_CENTER)

widget_s.DFdraw = WIDGET_DRAW(block2, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			XSIZE=data.datadims[0], $
			YSIZE=data.datadims[0])


;------------------------------------------------------------
block2a = WIDGET_BASE(block2, $
			/FRAME, $
			/ROW)

label1 = WIDGET_LABEL(block2a, $
			VALUE='min/max', $
			FONT=fontstr, $
			XSIZE=75, $
			YSIZE=30, $
			/ALIGN_LEFT)

widget_s.DFmin= WIDGET_TEXT(block2a, $
			VALUE=string(data.DFmin,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

widget_s.DFmax= WIDGET_TEXT(block2a, $
			VALUE=string(data.DFmax,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

block2b = WIDGET_BASE(block2a, $
			/FRAME, $
			/ROW)

label2 = WIDGET_LABEL(block2b, $
			VALUE='Current g  ', $
			FONT=fontstrlarge, $
			XSIZE=100, $
			YSIZE=25, $
			/ALIGN_LEFT)

wv = '('+string(gvecs[0,data.famsel],format="(I3)")+' '+ string(gvecs[1,data.famsel],format="(I3)")+' '+ string(gvecs[2,data.famsel],format="(I3)")+')'
widget_s.gsel= WIDGET_TEXT(block2b, $
			VALUE=wv,$
			XSIZE=20, $
			/ALIGN_LEFT)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.LACBEDDrawbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.BFdraw, GET_VALUE=drawID
data.BFdrawID = drawID
WIDGET_CONTROL, widget_s.DFdraw, GET_VALUE=drawID
data.DFdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"CBEDDrawWidget",widget_s.LACBEDDrawbase,/NO_BLOCK

end
