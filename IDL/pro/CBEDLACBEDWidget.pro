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
; CTEMsoft2013:CBEDLACBEDWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDLACBEDWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Create the LACBED widget
;
;> @date 10/09/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------

pro CBEDLACBEDWidget,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall


;------------------------------------------------------------
; create the top level widget
widget_s.LACBEDbase = WIDGET_BASE(TITLE='LACBED Widget', $
                        /COLUMN, $
                        XSIZE=700, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='CBEDLACBEDWidget_event', $
                        XOFFSET=data.LACBEDxlocation, $
                        YOFFSET=data.LACBEDylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the thickness selection
block1 = WIDGET_BASE(widget_s.LACBEDbase, $
			/FRAME, $
			/COLUMN)

;---------- display mode selector
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=400, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file1, $
			VALUE='Dark Field Display Mode', $
			FONT=fontstrlarge, $
			XSIZE=220, $
			YSIZE=25, $
			/ALIGN_LEFT)

vals = ['Single DF','Symmetrized DF','Eades']
widget_s.dfdisplaymode = CW_BGROUP(file1, $
			vals, $
			/COLUMN, $
			/EXCLUSIVE, $
			/NO_RELEASE, $
			FONT=fontstr, $
			/FRAME, $
			UVALUE='DFDISPLAYMODE', $
			SET_VALUE=data.dfdisplaymode)

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
			EVENT_PRO='CBEDLACBEDWidget_event', $
			VALUE=tvals,$
			UVALUE='CBEDLACBEDTHICKLIST', $
			/ALIGN_LEFT)

WIDGET_CONTROL, set_droplist_select=data.thicksel, widget_s.LACBEDthicklist

;---------- droplists for each of the HOLZ layers
file1 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_CENTER)

;; first of all, get the HOLZ identifier list
;HOLZlist = indgen(data.datadims[3])
;HOLZlist[0] = 0	; transmitted beam
;for i=1,data.datadims[3]-1 do begin
;  HOLZlist[i] = abs( gvecs[0,i]*data.wavek[0]+gvecs[1,i]*data.wavek[1]+gvecs[2,i]*data.wavek[2] )
;endfor
;
;; then determine how many there are in each layer
;numHOLZ = indgen(data.maxHOLZ+1)
;for i=0,data.maxHOLZ do begin
;  q=where(HOLZlist eq i,cnt)
;  numHOLZ[i] = cnt
;endfor

; create the droplist widgets
ioffset = 0
icnt = 0
HOLZvals = strarr(data.numfam+1)
HOLZvals[icnt] = '-----'
for ii=0,data.maxHOLZ do begin
  if (ii gt 3) then goto,skiptherest
  tvals = strarr(numHOLZ[ii]+1)
  case ii of
	0: tvals[0] = '  ZOLZ  '
	1: tvals[0] = ' HOLZ 1 '
	2: tvals[0] = ' HOLZ 2 '
	3: tvals[0] = ' HOLZ 3 '
  endcase
  for i=0,numHOLZ[ii]-1 do begin
    j = ioffset + i
    icnt += 1
    wv = '('+string(gvecs[0,j],format="(I3)")+' '+ string(gvecs[1,j],format="(I3)")+' '+ string(gvecs[2,j],format="(I3)")+'); '+string(gtt[j],format="(F6.2)")+'; '+string(gmult[j],format="(I2)")
    HOLZvals[icnt] = wv
    tvals[i+1] = wv
  end
  ioffset += numHOLZ[ii]
; and generate the widget
  case ii of
    0: widget_s.LACBEDdroplist0 = WIDGET_LIST(file1, $
			EVENT_PRO='CBEDLACBEDWidget_event', $
			FONT=fontstrsmall, $
			VALUE=tvals,$
			UVALUE='HOLZlayer0', $
;		PRO_SET_VALUE='CBEDprosetvalue', $
			YSIZE = 5, $
			/ALIGN_LEFT)

    1: widget_s.LACBEDdroplist1 = WIDGET_LIST(file1, $
			EVENT_PRO='CBEDLACBEDWidget_event', $
			FONT=fontstrsmall, $
			VALUE=tvals,$
			UVALUE='HOLZlayer1', $
;		PRO_SET_VALUE='CBEDprosetvalue', $
			YSIZE = 5, $
			/ALIGN_LEFT)

    2: widget_s.LACBEDdroplist2 = WIDGET_LIST(file1, $
			EVENT_PRO='CBEDLACBEDWidget_event', $
			FONT=fontstrsmall, $
			VALUE=tvals,$
			UVALUE='HOLZlayer2', $
;		PRO_SET_VALUE='CBEDprosetvalue', $
			YSIZE = 5, $
			/ALIGN_LEFT)

    3: widget_s.LACBEDdroplist3 = WIDGET_LIST(file1, $
			EVENT_PRO='CBEDLACBEDWidget_event', $
			FONT=fontstrsmall, $
			VALUE=tvals,$
			UVALUE='HOLZlayer3', $
;		PRO_SET_VALUE='CBEDprosetvalue', $
			YSIZE = 5, $
			/ALIGN_LEFT)

    else: print,'This program is limited to a maximum of 3 HOLZ layers'
  endcase
skiptherest:
endfor

;---------- Eades angular range
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.eadesrhoin  = Core_WTextE(file1, 'Eades angular range [mrad]',fontstrlarge, 290, 25, 10, 1, string(data.Eadesrhoin,FORMAT="(F8.3)"),'EADESMIN','CBEDLACBEDWidget_event')
widget_s.eadesrhoout = Core_WTextE(file1, '',fontstrlarge, 0, 25, 10, 1, string(data.Eadesrhoout,FORMAT="(F8.3)"),'EADESMAX','CBEDLACBEDWidget_event')

;---------- rotate disks
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.diskrotation = Core_WTextE(file1, 'Disk rotation angle[CW, deg]',fontstrlarge, 290, 25, 10, 1, string(data.diskrotation,FORMAT="(F6.2)"),'DISKROTATION','CBEDLACBEDWidget_event')

;---------- intensity scaling 
file1 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

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

widget_s.logoffset = WIDGET_TEXT(file1, $
			VALUE=string(data.logoffset,format="(E9.2)"),$
			XSIZE=10, $
			/EDITABLE, $
                        EVENT_PRO='CBEDLACBEDWidget_event', $
			UVALUE='LOGOFFSET', $
			/ALIGN_LEFT)


;---------- intensity scaling 
file1 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

vals = ['jpeg','tiff','bmp']
widget_s.cbedformatbgroup = CW_BGROUP(file1, $
			vals, $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			FONT=fontstrlarge, $
			LABEL_LEFT = 'File Format', $
			/FRAME, $
                        EVENT_FUNC ='CBEDevent', $
			UVALUE='CBEDFORMAT', $
			SET_VALUE=data.cbedformat)




;------------------------------------------------------------
; block 2 contains the go, surprise me, save, and close buttons
block2 = WIDGET_BASE(widget_s.LACBEDbase, $
			/FRAME, $
			/ROW)

goLACBED = WIDGET_BUTTON(block2, $
			VALUE='Go', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDLACBEDWidget_event', $
			UVALUE='GOLACBED', $
			/ALIGN_LEFT)

surpriseLACBED = WIDGET_BUTTON(block2, $
			VALUE='Surprise Me', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDLACBEDWidget_event', $
			UVALUE='SURPRISELACBED', $
			/ALIGN_LEFT)

saveLACBED = WIDGET_BUTTON(block2, $
			VALUE='Save', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDLACBEDWidget_event', $
			UVALUE='SAVELACBED', $
			/ALIGN_LEFT)


closeLACBED= WIDGET_BUTTON(block2, $
			VALUE='Close', $
			/NO_RELEASE, $
                        EVENT_PRO='CBEDLACBEDWidget_event', $
			UVALUE='CLOSELACBED', $
			/ALIGN_RIGHT)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.LACBEDbase,/REALIZE

; realize the draw widgets
;WIDGET_CONTROL, widget_s.BFdraw, GET_VALUE=drawID
;widget_s.BFdrawID = drawID
;WIDGET_CONTROL, widget_s.HAADFdraw, GET_VALUE=drawID
;widget_s.HAADFdrawID = drawID

; and hand over control to the xmanager
XMANAGER,"CBEDLACBEDWidget",widget_s.LACBEDbase,/NO_BLOCK



end
