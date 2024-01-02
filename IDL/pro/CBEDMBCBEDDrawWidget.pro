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
; CTEMsoft2013:CBEDCBEDDrawWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDMBCBEDDrawWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Draw widget for MBCBED
;
;> @date 10/09/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro CBEDMBCBEDDrawWidget,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common fontstrings, fontstr, fontstrlarge, fontstrsmall

; note that there are no user input parameters here, so no need for an event handler routine

;------------------------------------------------------------
; create the top level widget
widget_s.MBCBEDDrawbase = WIDGET_BASE(TITLE='MBCBED Pattern', $
                        /ROW, $
                        XSIZE=data.datadims[0]+5, $
                        YSIZE=data.datadims[1]+5, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
                        XOFFSET=data.MBCBEDDrawxlocation, $
                        YOFFSET=data.MBCBEDDrawylocation)

;------------------------------------------------------------
; create the various blocks
; block 1 contains the BF drawing window
block1 = WIDGET_BASE(widget_s.MBCBEDDrawbase, /FRAME, /ALIGN_CENTER, /COLUMN)

;------------------------------------------------------------
widget_s.MBdraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/ALIGN_CENTER, $
			XSIZE=data.datadims[0], $
			YSIZE=data.datadims[1])


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
; realize the widget structure
WIDGET_CONTROL,widget_s.MBCBEDDrawbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.MBdraw, GET_VALUE=drawID
data.MBdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"CBEDMBCBEDDrawWidget",widget_s.MBCBEDDrawbase,/NO_BLOCK

end
