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
; CTEMsoft2013:STEMCTEMBFDFWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMCTEMBFDFWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 06/13/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro STEMCTEMBFDFWidget_event, event

;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common STEM_detectordata, STEMdata, STEMcimage, BFdisk, DFdisk, clickablemap, STEMsectormaps, STEMsectors, STEMsectorranges
common STEM_CBEDpatterns, CBED, CBEDdisplay
common STEM_images, BFimage, HAADFimage, DFimage
common STEM_rawdata, indices, offsets, kperp, rawdata


if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.CTEMBFDFbase) then begin
  data.CTEMBFDFxlocation = event.x
  data.CTEMBFDFylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
  'REFSEL':  begin
; this option displays a DF image for the selected reflection
	    data.diffractionmode = 1
	    slice = reform(rawdata[0,*,*,0])
	    data.BFmin = min(slice)
	    data.BFmax = max(slice)
	    WIDGET_CONTROL, SET_VALUE=string(data.BFmin,FORMAT="(E9.2)"), widget_s.BFmin
	    WIDGET_CONTROL, SET_VALUE=string(data.BFmax,FORMAT="(E9.2)"), widget_s.BFmax
	    wset,widget_s.BFdrawID
	    tvscl,slice
	    BFimage = slice
	    slice = reform(rawdata[event.index,*,*,0])
	    data.HAADFmin = min(slice)
	    data.HAADFmax = max(slice)
	    WIDGET_CONTROL, SET_VALUE=string(data.HAADFmin,FORMAT="(E9.2)"), widget_s.HAADFmin
	    WIDGET_CONTROL, SET_VALUE=string(data.HAADFmax,FORMAT="(E9.2)"), widget_s.HAADFmax
	    wset,widget_s.HAADFdrawID
	    tvscl,slice
	    DFimage = slice
	  endcase

  'CLSEL':  begin
; this option displays a HAADF image for the selected camera length value
	    data.diffractionmode = 0
	    slice = reform(rawdata[0,*,*,event.index])
	    data.BFmin = min(slice)
	    data.BFmax = max(slice)
	    WIDGET_CONTROL, SET_VALUE=string(data.BFmin,FORMAT="(E9.2)"), widget_s.BFmin
	    WIDGET_CONTROL, SET_VALUE=string(data.BFmax,FORMAT="(E9.2)"), widget_s.BFmax
	    wset,widget_s.BFdrawID
	    tvscl,slice
	    BFimage = slice
	    slice = reform(rawdata[1,*,*,event.index])
	    data.HAADFmin = min(slice)
	    data.HAADFmax = max(slice)
	    WIDGET_CONTROL, SET_VALUE=string(data.HAADFmin,FORMAT="(E9.2)"), widget_s.HAADFmin
	    WIDGET_CONTROL, SET_VALUE=string(data.HAADFmax,FORMAT="(E9.2)"), widget_s.HAADFmax
	    wset,widget_s.HAADFdrawID
	    tvscl,slice
	    HAADFimage = slice
	  endcase

  'SAVEIMAGE': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[data.imageformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
		im = bytarr(2*data.datadims[1],data.datadims[2])
		im[0,0] = bytscl(BFimage)
		if (data.diffractionmode eq 0) then im[data.datadims[1],0] = bytscl(HAADFimage) else im[data.datadims[1],0] = bytscl(DFimage)
		case de of
		  'jpeg': write_jpeg,filename,im,quality=100
		  'tiff': write_tiff,filename,reverse(im,2)
		  'bmp': write_bmp,filename,im
		 else: MESSAGE,'unknown file format option'
		endcase
	  endcase

 'CLOSEIMAGE': begin
; kill the base widget
		WIDGET_CONTROL, widget_s.CTEMBFDFbase, /DESTROY
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
