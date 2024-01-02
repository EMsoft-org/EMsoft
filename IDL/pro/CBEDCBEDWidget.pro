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
; CTEMsoft2013:CBEDCBEDWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDCBEDWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the CBED control widget
;
;> @date 10/15/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro CBEDCBEDWidget,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall

;------------------------------------------------------------
; create the top level widget
widget_s.CBEDbase = WIDGET_BASE(TITLE='CBED Widget', $
                        /COLUMN, $
                        XSIZE=550, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='CBEDCBEDWidget_event', $
                        XOFFSET=data.CBEDxlocation, $
                        YOFFSET=data.CBEDylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the thickness selection
block1 = WIDGET_BASE(widget_s.CBEDbase, /FRAME, /COLUMN)

;---------- camera length 
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.camlenval = Core_WTextE(file1, 'Camera Length [mm]',fontstrlarge, 250, 25, 10, 1, string(data.camlen,FORMAT="(F8.1)"),'CAMLEN','CBEDCBEDWidget_event')
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.convangval = Core_WTextE(file1, 'Convergence Angle [mrad]',fontstrlarge, 250, 25, 10, 1, string(data.thetau,FORMAT="(F8.3)"),'CONVANG','CBEDCBEDWidget_event')

file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.Lauex= Core_WTextE(file1, 'Laue Center    x=',fontstrlarge, 150, 25, 10, 1, string(data.Lauex,FORMAT="(F8.3)"),'LAUEX','CBEDCBEDWidget_event')
widget_s.Lauey= Core_WTextE(file1, ' y=',fontstrlarge, 30, 25, 10, 1, string(data.Lauey,FORMAT="(F8.3)"),'LAUEY','CBEDCBEDWidget_event')

;---------- intensity scaling 
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)

vals = ['normal','logarithmic']
widget_s.cbedmodebgroup = CW_BGROUP(file1, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'Intensity Scaling ', $
                        EVENT_FUNC='CBEDevent', $
			/FRAME, $
			UVALUE='CBEDMODE', $
			SET_VALUE=data.cbedmode)

widget_s.logoffset = Core_WTextE(file1,'',fontstrlarge, 1, 25, 10, 1, string(data.logoffset,FORMAT="(E9.2)"),'LOGOFFSET','CBEDCBEDWidget_event')

;----------
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=400, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file1, $
			VALUE='Foil Thickness [nm]', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

tvals = strarr(data.numt)
for i=0,data.numt-1 do begin
  th = data.startthick + float(i)*data.thickinc
  tvals[i] = string(th,format="(F6.1)")
end

widget_s.LACBEDthicklist = WIDGET_DROPLIST(file1, $
			EVENT_PRO='CBEDCBEDWidget_event', $
			VALUE=tvals,$
			UVALUE='CBEDCBEDTHICKLIST', $
			/ALIGN_LEFT)
WIDGET_CONTROL, set_droplist_select=data.thicksel, widget_s.LACBEDthicklist

;------------------------------------------------------------
; the next block is really just a simple drawing window with a sketch of the diffraction disks
; along with a superimposed circle indicating the maximum range of the Laue center
; we'll make the drawing surface 401x401 pixels.
block2 = WIDGET_BASE(widget_s.CBEDbase, /FRAME, /ROW, /ALIGN_CENTER)

file1 = WIDGET_BASE(block2, /COLUMN, XSIZE=100, /ALIGN_LEFT)

vals = ['jump','track']
widget_s.movemodegroup = CW_BGROUP(file1, $
			vals, $
			/COLUMN, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstr, $
			LABEL_TOP = 'Move Mode ', $
                        EVENT_FUNC='CBEDevent', $
			/FRAME, $
			UVALUE='MOVEMODE', $
			SET_VALUE=data.movemode)

tvals = strarr(20)
for i=0,19 do begin
  tvals[i] = string(5*(i+1),format="(I2)")
end

widget_s.jumplist = WIDGET_DROPLIST(file1, $
			EVENT_PRO='CBEDCBEDWidget_event', $
			VALUE=tvals,$
			UVALUE='JUMPLIST', $
			/ALIGN_LEFT)

WIDGET_CONTROL, set_droplist_select=data.jumpsel, widget_s.jumplist


widget_s.CBdraw = WIDGET_DRAW(block2, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/BUTTON_EVENTS, $
			EVENT_PRO='CBEDCBEDWidget_event', $
			UVALUE = 'SELECTLAUE', $
			TOOLTIP='Select Laue center location inside red circle', $
			XSIZE=data.detwinx, $
			YSIZE=data.detwiny)

;------------------------------------------------------------
; block 3 contains the go, create nml, and close buttons
; there is no save button here, because this CBED pattern is really just an approximation; if a 
; good pattern is needed, then the user should use the create nml option and run the mbcbed program
block3 = WIDGET_BASE(widget_s.CBEDbase, /FRAME, /ROW)

goCBED = WIDGET_BUTTON(block3, $
			VALUE='Go', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDCBEDWidget_event', $
			UVALUE='GOCBED', $
			/ALIGN_LEFT)

makenml = WIDGET_BUTTON(block3, $
			VALUE='Create .nml file', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDCBEDWidget_event', $
			UVALUE='MAKENML', $
			/ALIGN_LEFT)

closeCBED= WIDGET_BUTTON(block3, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDCBEDWidget_event', $
			UVALUE='CLOSECBED', $
			/ALIGN_RIGHT)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.CBEDbase,/REALIZE

; realize the draw widget
WIDGET_CONTROL, widget_s.CBdraw, GET_VALUE=drawID
data.diskdrawID = drawID

wset,data.diskdrawID
CBEDcircles

CBEDupdateLaue

; and hand over control to the xmanager
XMANAGER,"CBEDCBEDWidget",widget_s.CBEDbase,/NO_BLOCK



end
