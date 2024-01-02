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
; CTEMsoft2013:STEMCTEMBFDFWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMCTEMBFDFWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the CTEM/BF-HAADF widget
;
;> @date 11/18/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro STEMCTEMBFDFWidget,dummy
;
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common STEM_rawdata, indices, offsets, kperp, rawdata


;------------------------------------------------------------
; create the top level widget
widget_s.CTEMBFDFbase = WIDGET_BASE(TITLE='CTEM/BFDF Display', $
                        /COLUMN, $
                        XSIZE=max([data.datadims[1]*2+200,700]), $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='STEMCTEMBFDFWidget_event', $
                        XOFFSET=data.CTEMBFDFxlocation, $
                        YOFFSET=data.CTEMBFDFylocation)

;------------------------------------------------------------
; create the various blocks
; block 1 contains the drawing windows and the beam/CL selectors
block1 = WIDGET_BASE(widget_s.CTEMBFDFbase, $
			/FRAME, $
			/ROW)

;------------------------------------------------------------
; this column contains the beam/CL selector
block1a = WIDGET_BASE(block1, $
			/FRAME, $
			/ALIGN_LEFT, $
			/COLUMN)

if (data.progmode eq 'CTEM') then begin
; we'll need to list the number of beams and their indices
  block2a = WIDGET_BASE(block1a, $
			/FRAME, $
			/COLUMN)

  label1 = WIDGET_LABEL(block2a, $
			VALUE='g-vectors', $
			FONT=fontstr, $
			XSIZE=125, $
			YSIZE=30, $
			/ALIGN_LEFT)

; Miller index selection widget
  tvals = strarr(data.datadims[0]+1)
  for i=1,data.datadims[0]-1 do begin
    wv = '('+string(indices[0,i],format="(I3)")+' '+ string(indices[1,i],format="(I3)")+' '+ string(indices[2,i],format="(I3)")+')'
    tvals[i] = wv
  end
  widget_s.CTEMdroplist = WIDGET_LIST(block2a, $
			EVENT_PRO='STEMCTEMBFDFWidget_event', $
			FONT=fontstrsmall, $
			VALUE=tvals,$
			UVALUE='REFSEL', $
			YSIZE = 10, $
			/ALIGN_LEFT)

end else begin
; we'll need to list the number of camera lengths and their values
  block2a = WIDGET_BASE(block1a, $
			/FRAME, $
			/COLUMN)

  label1 = WIDGET_LABEL(block2a, $
			VALUE='Camera Lengths', $
			FONT=fontstr, $
			XSIZE=125, $
			YSIZE=30, $
			/ALIGN_LEFT)

; camera length selection widget
  tvals = strarr(data.datadims[3]+1)
  for i=0,data.numCL-1 do begin
    wv = string(data.CLarray[i],format="(F10.1)")
    tvals[i] = wv
  end
  widget_s.CTEMdroplist = WIDGET_LIST(block2a, $
			EVENT_PRO='STEMCTEMBFDFWidget_event', $
			FONT=fontstrsmall, $
			VALUE=tvals,$
			UVALUE='CLSEL', $
			YSIZE = 10, $
			/ALIGN_CENTER)

end

;------------------------------------------------------------
block1a = WIDGET_BASE(block1, $
			/FRAME, $
			/ALIGN_CENTER, $
			/COLUMN)

label1 = WIDGET_LABEL(block1a, $
			VALUE='BF Image', $
			FONT=fontstrlarge, $
			XSIZE=100, $
			YSIZE=30, $
			/ALIGN_CENTER)

widget_s.BFdraw = WIDGET_DRAW(block1a, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			XSIZE=data.datadims[1], $
			YSIZE=data.datadims[2])

block2a = WIDGET_BASE(block1a, $
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
block1b = WIDGET_BASE(block1, $
			/FRAME, $
			/COLUMN)

label1 = WIDGET_LABEL(block1b, $
			VALUE='HAADF/DF Image', $
			FONT=fontstrlarge, $
			XSIZE=175, $
			YSIZE=30, $
			/ALIGN_CENTER)

widget_s.HAADFdraw = WIDGET_DRAW(block1b, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			XSIZE=data.datadims[1], $
			YSIZE=data.datadims[2])


block2b = WIDGET_BASE(block1b, $
			/FRAME, $
			/ROW)

label1 = WIDGET_LABEL(block2b, $
			VALUE='min/max', $
			FONT=fontstr, $
			XSIZE=75, $
			YSIZE=30, $
			/ALIGN_LEFT)

widget_s.HAADFmin= WIDGET_TEXT(block2b, $
			VALUE=string(data.HAADFmin,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

widget_s.HAADFmax= WIDGET_TEXT(block2b, $
			VALUE=string(data.HAADFmax,format="(F)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

;------------------------------------------------------------
; block 2 contains the button groups and save button
block2 = WIDGET_BASE(widget_s.CTEMBFDFbase, $
			/FRAME, $
			/ROW)

vals = ['jpeg','tiff','bmp']
widget_s.imageformatbgroup = CW_BGROUP(block2, $
			vals, $
			/ROW, $
			/EXCLUSIVE, $
			/NO_RELEASE, $
			LABEL_LEFT = 'File Format', $
			/FRAME, $
                        EVENT_FUNC='STEMevent', $
			UVALUE='IMAGEFORMAT', $
			SET_VALUE=data.imageformat)

; and, finally, a save and a close button
widget_s.saveimage = WIDGET_BUTTON(block2, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='STEMCTEMBFDFWidget_event', $
			UVALUE='SAVEIMAGE', $
			/ALIGN_RIGHT)

widget_s.closeimage = WIDGET_BUTTON(block2, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='STEMCTEMBFDFWidget_event', $
			UVALUE='CLOSEIMAGE', $
			/ALIGN_RIGHT)


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.CTEMBFDFbase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.BFdraw, GET_VALUE=drawID
widget_s.BFdrawID = drawID
WIDGET_CONTROL, widget_s.HAADFdraw, GET_VALUE=drawID
widget_s.HAADFdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"STEMCTEMBFDFWidget",widget_s.CTEMBFDFbase,/NO_BLOCK

end

