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
; CTEMsoft2013:ECPatternWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECPatternWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the CBED control widget
;
;> @date 10/15/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro ECPatternWidget,dummy

;------------------------------------------------------------
; common blocks
common ECP_widget_common, widget_s
common ECP_data_common, data
common BED_rawdata, rawdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

;------------------------------------------------------------
; create the top level widget
widget_s.ECPatternbase= WIDGET_BASE(TITLE='ECP Widget', $
                        /COLUMN, $
                        XSIZE=20 + max([513,data.datadims[0]]), $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='ECPatternWidget_event', $
                        XOFFSET=data.ECPxlocation, $
                        YOFFSET=data.ECPylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the thickness selection
block1 = WIDGET_BASE(widget_s.ECPatternbase, /FRAME, /COLUMN)

;----------
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=400, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file1, $
			VALUE='Integration Depth [nm]', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

tvals = strarr(data.datadims[2])
for i=0,data.datadims[2]-1 do begin
  th = data.startthick + float(i)*data.thickinc
  tvals[i] = string(th,format="(F5.1)")
end

widget_s.ECPthicklist = WIDGET_DROPLIST(file1, $
			EVENT_PRO='ECPatternWidget_event', $
			VALUE=tvals,$
			UVALUE='ECPTHICKLIST', $
			/ALIGN_LEFT)

data.thicksel = 0
WIDGET_CONTROL, set_droplist_select=data.thicksel, widget_s.ECPthicklist

;----------
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=500, $
			/ALIGN_LEFT)

grid = ['off','on']
widget_s.ECPgridbgroup = CW_BGROUP(file1, $
			grid, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'Coordinate Grid', $
			/FRAME, $
                        EVENT_FUNC ='ECPevent', $
			UVALUE='ECPGRID', $
			SET_VALUE=data.ecpgrid)

label4 = WIDGET_LABEL(file1, $
			VALUE='Blur Radius', $
			FONT=fontstrlarge, $
			XSIZE=110, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.blur=  WIDGET_TEXT(file1, $
		VALUE=string(data.blur,FORMAT="(F6.2)"),$
		XSIZE=10, $
		YSIZE=1, $
		/EDITABLE, $
                EVENT_PRO='ECPatternWidget_event', $
		UVALUE='BLUR', $
		/ALIGN_LEFT)

;----------
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=500, $
			/ALIGN_LEFT)

label4 = WIDGET_LABEL(file1, $
			VALUE='Pattern Rotation', $
			FONT=fontstrlarge, $
			XSIZE=150, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.patrot=  WIDGET_TEXT(file1, $
		VALUE=string(data.patrot,FORMAT="(F6.2)"),$
		XSIZE=10, $
		YSIZE=1, $
		/EDITABLE, $
                EVENT_PRO='ECPatternWidget_event', $
		UVALUE='PATROT', $
		/ALIGN_LEFT)



;---------- save pattern
file1 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

vals = ['jpeg','tiff','bmp']
widget_s.ECPformatbgroup = CW_BGROUP(file1, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'File Format', $
			/FRAME, $
                        EVENT_FUNC ='ECPevent', $
			UVALUE='ECPFORMAT', $
			SET_VALUE=data.ecpformat)

;------------  coordinate display
file3 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

label4 = WIDGET_LABEL(file3, $
			VALUE='Selected position ', $
			FONT=fontstrlarge, $
			XSIZE=160, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.cx= WIDGET_TEXT(file3, $
			VALUE=string(data.cx,format="(F6.3)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

labela = WIDGET_LABEL(file3, $
			VALUE=',', $
			FONT=fontstrlarge, $
			XSIZE=12, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.cy= WIDGET_TEXT(file3, $
			VALUE=string(data.cy,format="(F6.3)"),$
			XSIZE=10, $
			/ALIGN_LEFT)


;------------ a few control buttons
block2 = WIDGET_BASE(widget_s.ECPatternbase, $
			/FRAME, $
			/ROW)

saveMBCBED = WIDGET_BUTTON(block2, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='ECPatternWidget_event', $
			/FRAME, $
			UVALUE='SAVEECP', $
			/ALIGN_LEFT)


closeECP = WIDGET_BUTTON(block2, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='ECPatternWidget_event', $
			/FRAME, $
			UVALUE='CLOSEECP', $
			/ALIGN_RIGHT)

;------------ the actual pattern 
widget_s.ECPdraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/BUTTON_EVENTS, $
			UVALUE = 'GETCOORDINATES', $
			/ALIGN_CENTER, $
			XSIZE=data.datadims[0], $
			YSIZE=data.datadims[1])


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.ECPatternbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.ECPdraw, GET_VALUE=drawID
widget_s.ECPdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"ECPatternWidget",widget_s.ECPatternbase,/NO_BLOCK

ECPshow

end
