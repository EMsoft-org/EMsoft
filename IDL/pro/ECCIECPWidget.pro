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
; CTEMsoft2013:ECCIECPWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIECPWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the ECP control widget
;
;> @date 12/06/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro ECCIECPWidget,dummy

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata


;------------------------------------------------------------
; create the top level widget
widget_s.ECCIECPbase= WIDGET_BASE(TITLE='ECP Pattern Widget', $
                        /COLUMN, $
                        XSIZE=20 + max([513,ECPdata.datadims[0]]), $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='ECCIECPWidget_event', $
                        XOFFSET=data.ECPxlocation, $
                        YOFFSET=data.ECPylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the thickness selection
block1 = WIDGET_BASE(widget_s.ECCIECPbase, /FRAME, /COLUMN)

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

tvals = strarr(ECPdata.datadims[2])
for i=0,ECPdata.datadims[2]-1 do begin
  th = ECPdata.startthick + float(i)*ECPdata.thickinc
  tvals[i] = string(th,format="(F5.1)")
end

widget_s.ECPthicklist = WIDGET_DROPLIST(file1, $
			EVENT_PRO='ECCIECPWidget_event', $
			VALUE=tvals,$
			UVALUE='ECPTHICKLIST', $
			/ALIGN_LEFT)

ECPdata.thicksel = 0
WIDGET_CONTROL, set_droplist_select=ECPdata.thicksel, widget_s.ECPthicklist

;----------
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=400, $
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
                        EVENT_FUNC ='ECCIevent', $
			UVALUE='ECPGRID', $
			SET_VALUE=ECPdata.ecpgrid)


;---------- save pattern
file1 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

; not really any need to save the pattern here ... that is already done in the ECPDisplay program
;vals = ['jpeg','tiff','bmp']
;widget_s.ECPformatbgroup = CW_BGROUP(file1, $
;			vals, $
;			/ROW, $
;			/NO_RELEASE, $
;			/EXCLUSIVE, $
;			FONT=fontstrlarge, $
;			LABEL_LEFT = 'File Format', $
;			/FRAME, $
;                        EVENT_FUNC ='ECPevent', $
;			UVALUE='ECPFORMAT', $
;			SET_VALUE=ECPdata.ecpformat)

;------------  coordinate display
file3 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.cx = Core_WText(file3, 'Selected position ',fontstrlarge, 160, 25, 10, 1, string(ECPdata.cx,FORMAT="(F6.3)"))
widget_s.cy = Core_WText(file3, ',',fontstrlarge, 12, 25, 10, 1, string(ECPdata.cy,FORMAT="(F6.3)"))


;------------ a few control buttons
block2 = WIDGET_BASE(widget_s.ECCIECPbase, /FRAME, /ROW)

;saveMBCBED = WIDGET_BUTTON(block2, $
;			VALUE='Save', $
;			/NO_RELEASE, $
;                        EVENT_PRO='ECCIECPWidget_event', $
;			/FRAME, $
;			UVALUE='SAVEECP', $
;			/ALIGN_LEFT)


closeECP = WIDGET_BUTTON(block2, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='ECCIECPWidget_event', $
			/FRAME, $
			UVALUE='CLOSEECP', $
			/ALIGN_RIGHT)

;------------ the actual pattern 
widget_s.ECPdraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/BUTTON_EVENTS, $
                        EVENT_PRO='ECCIECPWidget_event', $
			UVALUE = 'GETCOORDINATES', $
			/ALIGN_CENTER, $
			XSIZE=ECPdata.datadims[0], $
			YSIZE=ECPdata.datadims[1])


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.ECCIECPbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.ECPdraw, GET_VALUE=drawID
widget_s.ECPdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"ECCIECPWidget",widget_s.ECCIECPbase,/NO_BLOCK

ECCIECPshow

end
