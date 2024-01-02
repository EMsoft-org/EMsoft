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
; CTEMsoft2013:CBEDMBCBEDWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDMBCBEDWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the CBED control widget
;
;> @date 10/15/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro CBEDMBCBEDWidget,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall

;------------------------------------------------------------
; create the top level widget
widget_s.MBCBEDbase = WIDGET_BASE(TITLE='MBCBED Widget', $
                        /COLUMN, $
                        XSIZE=500, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='CBEDMBCBEDWidget_event', $
                        XOFFSET=data.MBCBEDxlocation, $
                        YOFFSET=data.MBCBEDylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the thickness selection
block1 = WIDGET_BASE(widget_s.MBCBEDbase, /FRAME, /COLUMN)

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

widget_s.MBCBEDthicklist = WIDGET_DROPLIST(file1, $
			EVENT_PRO='CBEDMBCBEDWidget_event', $
			VALUE=tvals,$
			UVALUE='CBEDMBCBEDTHICKLIST', $
			/ALIGN_LEFT)
data.thicksel = 0
WIDGET_CONTROL, set_droplist_select=data.thicksel, widget_s.MBCBEDthicklist

;---------- intensity scaling 
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)

vals = ['normal','logarithmic']
widget_s.mbcbedmodebgroup = CW_BGROUP(file1, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'Intensity Scaling ', $
                        EVENT_FUNC='CBEDevent', $
			/FRAME, $
			UVALUE='MBCBEDMODE', $
			SET_VALUE=data.cbedmode)

widget_s.logoffset = Core_WTextE(file1,'',fontstrlarge, 1, 25, 10, 1, string(data.logoffset,FORMAT="(E9.2)"),'LOGOFFSET','CBEDMBCBEDWidget_event')

;---------- intensity scaling 
file1 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

vals = ['jpeg','tiff','bmp']
widget_s.mbcbedformatbgroup = CW_BGROUP(file1, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'File Format', $
			/FRAME, $
                        EVENT_FUNC ='CBEDevent', $
			UVALUE='MBCBEDFORMAT', $
			SET_VALUE=data.cbedformat)


;------------------------------------------------------------
; block 3 contains the go, create nml, and close buttons
; there is no save button here, because this CBED pattern is really just an approximation; if a 
; good pattern is needed, then the user should use the create nml option and run the mbcbed program
block3 = WIDGET_BASE(widget_s.MBCBEDbase, /FRAME, /ROW)

goMBCBED = WIDGET_BUTTON(block3, $
			VALUE='Go', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDMBCBEDWidget_event', $
			UVALUE='GOMBCBED', $
			/ALIGN_LEFT)

saveMBCBED = WIDGET_BUTTON(block3, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDMBCBEDWidget_event', $
			UVALUE='SAVEMBCBED', $
			/ALIGN_LEFT)


closeMBCBED= WIDGET_BUTTON(block3, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDMBCBEDWidget_event', $
			UVALUE='CLOSEMBCBED', $
			/ALIGN_RIGHT)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.MBCBEDbase,/REALIZE

; and hand over control to the xmanager
XMANAGER,"CBEDMBCBEDWidget",widget_s.MBCBEDbase,/NO_BLOCK


end
