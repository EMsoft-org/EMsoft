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
; PROGRAM: CBEDCBEDDrawWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Draw widget for LACBED
;
;> @date 10/09/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro CBEDCBEDDrawWidget,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common fontstrings, fontstr, fontstrlarge, fontstrsmall

; note that there are no user input parameters here, so no need for an event handler routine

;------------------------------------------------------------
; create the top level widget
widget_s.CBEDDrawbase = WIDGET_BASE(TITLE='CBED Pattern', $
                        /ROW, $
                        XSIZE=1030, $
                        YSIZE=1100, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
                        XOFFSET=data.CBEDDrawxlocation, $
                        YOFFSET=data.CBEDDrawylocation)

;------------------------------------------------------------
; create the various blocks
; block 1 contains the BF drawing window
block1 = WIDGET_BASE(widget_s.CBEDDrawbase, /FRAME, /ALIGN_CENTER, /COLUMN)

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


; and a save button
saveCBEDPattern = WIDGET_BUTTON(block2a, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='CBEDCBEDDrawWidget_event', $
                        /FRAME, $
                        UVALUE='SAVECBEDPATTERN', $
                        /ALIGN_LEFT)

; and the save format selector
vals = ['jpeg','tiff','bmp','mrc','hdf5']
widget_s.imageformatbgroup = CW_BGROUP(block2a, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        LABEL_LEFT = 'File Format', $
                        /FRAME, $
                        EVENT_FUNC ='CBEDevent', $
                        UVALUE='IMAGEFORMAT', $
                        SET_VALUE=data.imageformat)

;------------------------------------------------------------
widget_s.CBdraw = WIDGET_DRAW(block1, $
                        COLOR_MODEL=2, $
                        RETAIN=2, $
                        /ALIGN_CENTER, $
                        XSIZE=1025, $
                        YSIZE=1025)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.CBEDDrawbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.CBdraw, GET_VALUE=drawID
data.CBdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"CBEDCBEDDrawWidget",widget_s.CBEDDrawbase,/NO_BLOCK

end
