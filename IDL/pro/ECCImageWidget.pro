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
; CTEMsoft2013:ECCImageWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCImageWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the ECCI control widget
;
;> @date 12/07/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro ECCImageWidget,dummy

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata
common ECCI_rawdata, indices, offsets, kperp, rawdata, ECCILUT


;------------------------------------------------------------
; create the top level widget
widget_s.ECCImagebase= WIDGET_BASE(TITLE='ECCI Image Widget', $
                        /COLUMN, $
                        XSIZE= max([543,2*data.datadims[0]]), $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='ECCImageWidget_event', $
                        XOFFSET=data.ECCIxlocation, $
                        YOFFSET=data.ECCIylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the thickness selection
block1 = WIDGET_BASE(widget_s.ECCImagebase, /FRAME, /COLUMN)

;---------- save pattern
file1 = WIDGET_BASE(block1, /COLUMN, /ALIGN_LEFT)

; save format
vals = ['jpeg','tiff','bmp']
widget_s.ECCIformatbgroup = CW_BGROUP(file1, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'File Format', $
			/FRAME, $
                        EVENT_FUNC ='ECCIevent', $
			UVALUE='ECCIFORMAT', $
			SET_VALUE=data.ecciformat)

;------------  coordinate display
file3 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.ECCIcx = Core_WText(file3, 'Selected position ',fontstrlarge, 160, 25, 10, 1, string(ECPdata.cx,FORMAT="(F6.3)"))
widget_s.ECCIcy = Core_WText(file3, ',',fontstr, 12, 25, 10, 1, string(ECPdata.cy,FORMAT="(F6.3)"))


;------------  coordinate display
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
if (data.progmode eq 'array') then begin
  kpsum = total(kperp,2)/float(data.numk)
  kp = kperp
  for i=0,data.numk-1 do kp[0:1,i] -= kpsum[0:1]
  ma = max(abs(kp))
  data.mosaicdim = (2L*ma+1L) * data.datadims[0]
  widget_s.mosaicdim= Core_WTextE(file1, 'Dimension of ECCI Mosaic',fontstrlarge, 230, 25, 10, 1, string(data.mosaicdim,FORMAT="(I6)"),'MOSAICDIM','ECCImageWidget_event')
end else begin
  data.mosaicdim = 5
  widget_s.mosaicdim= Core_WTextE(file1, 'Number of images in row ',fontstrlarge, 230, 25, 10, 1, string(data.mosaicdim,FORMAT="(I6)"),'MOSAICDIM','ECCImageWidget_event')
endelse

DoMosaic = WIDGET_BUTTON(file1, $
			VALUE='Mosaic', $
			/NO_RELEASE, $
                        EVENT_PRO='ECCImageWidget_event', $
			/FRAME, $
			UVALUE='DOMOSAIC', $
			/ALIGN_LEFT)

grid = ['global','by image']
widget_s.ECCImosaicbgroup = CW_BGROUP(block1, $
			grid, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'Intensity Scaling', $
			/FRAME, $
                        EVENT_FUNC ='ECCIevent', $
			UVALUE='MOSAICSCALE', $
			SET_VALUE=data.mosaicscale)


file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
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
                EVENT_PRO='ECCImageWidget_event', $
		UVALUE='BLUR', $
		/ALIGN_LEFT)


;------------  coordinate display
; then we put in a row with two display areas and a few controls
file3 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)

file4 = WIDGET_BASE(file3, /COLUMN, /ALIGN_LEFT, /FRAME)


widget_s.ECCIdraw = WIDGET_DRAW(file4, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/BUTTON_EVENTS, $
                        EVENT_PRO='ECCImageWidget_event', $
			UVALUE = 'GETCOORDINATES', $
			/ALIGN_CENTER, $
			XSIZE=data.datadims[0], $
			YSIZE=data.datadims[1])

; then put some min-max info and a save button
block2a = WIDGET_BASE(file4, /FRAME, /COLUMN)
block2b = WIDGET_BASE(block2a, /FRAME, /ROW)
widget_s.ECCIdrawmin = Core_WText(block2b, 'min/max ',fontstr, 75, 25, 8, 1, string(data.ECCIdrawmin,FORMAT="(F7.1)"))
widget_s.ECCIdrawmax = Core_WText(block2b, '/',fontstr, 5, 25, 8, 1, string(data.ECCIdrawmax,FORMAT="(F7.1)"))


saveECCI = WIDGET_BUTTON(block2a, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='ECCImageWidget_event', $
			/FRAME, $
			UVALUE='SAVEECCI', $
			/ALIGN_LEFT)

; and the next block
file5 = WIDGET_BASE(file3, /COLUMN, /ALIGN_LEFT, /FRAME)

widget_s.ECCIavdraw = WIDGET_DRAW(file5, $
			COLOR_MODEL=2, $
			RETAIN=2, $
                        EVENT_PRO='ECCImageWidget_event', $
			/ALIGN_CENTER, $
			XSIZE=data.datadims[0], $
			YSIZE=data.datadims[1])

; then put some min-max info and a save button
block2a = WIDGET_BASE(file5, /FRAME, /COLUMN)
block2b = WIDGET_BASE(block2a, /FRAME, /ROW)
widget_s.ECCIavdrawmin = Core_WText(block2b, 'min/max ',fontstr, 75, 25, 8, 1, string(data.ECCIavdrawmin,FORMAT="(F7.1)"))
widget_s.ECCIavdrawmax = Core_WText(block2b, '/',fontstr, 5, 25, 8, 1, string(data.ECCIavdrawmax,FORMAT="(F7.1)"))

saveavECCI = WIDGET_BUTTON(block2a, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='ECCImageWidget_event', $
			/FRAME, $
			UVALUE='SAVEAVECCI', $
			/ALIGN_LEFT)

; and finally the averaging radius in units of |ga|
block2b = WIDGET_BASE(block2a, /FRAME, /ROW)
label2 = WIDGET_LABEL(block2b, $
		VALUE='Averaging Radius', $
		FONT=fntstr, $
		XSIZE=100, $
		YSIZE=25, $
		/ALIGN_LEFT)
    
widget_s.avrad =  WIDGET_TEXT(block2b, $
		VALUE=string(data.avrad,FORMAT="(F6.2)"),$
		XSIZE=10, $
		YSIZE=1, $
		/EDITABLE, $
                EVENT_PRO='ECCImageWidget_event', $
		UVALUE='AVRAD', $
		/ALIGN_LEFT)

;------------ and finally the close button
block2 = WIDGET_BASE(widget_s.ECCImagebase, /FRAME, /ROW)

closeECCI = WIDGET_BUTTON(block2, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='ECCImageWidget_event', $
			/FRAME, $
			UVALUE='CLOSEECCI', $
			/ALIGN_RIGHT)


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.ECCImagebase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.ECCIdraw, GET_VALUE=drawID
widget_s.ECCIdrawID = drawID
WIDGET_CONTROL, widget_s.ECCIavdraw, GET_VALUE=drawID
widget_s.ECCIavdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"ECCImageWidget",widget_s.ECCImagebase,/NO_BLOCK

end
