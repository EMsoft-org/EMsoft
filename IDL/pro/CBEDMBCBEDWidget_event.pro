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
; CTEMsoft2013:CBEDMBCBEDWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDMBCBEDWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for MBCBED mode
;
;> @date 10/15/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro CBEDMBCBEDWidget_event, event

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common CBED_current, BFcurrent, DFcurrent, RGBcurrent, mask
common trafos, done, c030, c060, c120, c150, c240, c300, s030, s060, s120, s150, s240, s300
common SYM2D, SYM_MATnum, SYM_direc
common CBEDcirclestuff, CBEDschematic, midx, midy, drad, cang, scl, gxpos, ct, st, sc

if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.MBCBEDbase) then begin
  data.MBCBEDxlocation = event.x
  data.MBCBEDylocation = event.y-25
    CBEDprint,' Window moved to location ('+string(fix(data.MBCBEDxlocation),format="(I4)")+','+string(fix(data.MBCBEDylocation),format="(I4)")+')'
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF

 'LOGOFFSET':  begin
		WIDGET_CONTROL, get_value=val,widget_s.logoffset
		data.logoffset= float(val[0])
		  CBEDprint,'Logarithm offset value set to '+string(float(val[0]),FORMAT="(E9.2)")
		WIDGET_CONTROL, set_value=string(data.logoffset,format="(E9.2)"), widget_s.logoffset
	endcase

 'CBEDMBCBEDTHICKLIST': begin
	data.thicksel = event.index
	data.thickness = data.startthick + float(event.index) * data.thickinc
	  CBEDprint,'Foil thickness set to '+string(data.thickness,FORMAT="(F6.1)")+' nm'
	endcase

  'GOMBCBED': begin
; display the selected pattern using the correct intensity scaling mode
	  BFcurrent = reform(disks[0:*,0:*,data.thicksel])
	  data.BFmin = min(BFcurrent,max = ma)
	  data.BFmax = ma
	  WIDGET_CONTROL, SET_VALUE=string(data.BFmin,FORMAT="(E9.2)"), widget_s.BFmin
	  WIDGET_CONTROL, SET_VALUE=string(data.BFmax,FORMAT="(E9.2)"), widget_s.BFmax
	  wset,data.MBdrawID
	  if (data.cbedmode eq 0) then begin
	    tvscl,BFcurrent
	  end else begin
	    tvscl,alog10(BFcurrent+data.logoffset)
	  end
	  data.lastmode = 0
	endcase

  'SAVEMBCBED': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[data.cbedformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
		im = fltarr(data.datadims[0],data.datadims[1])
		if (data.cbedmode eq 0) then begin
		  im[0,0] = BFcurrent
		end else begin
		  im[0,0] = alog10(BFcurrent + data.logoffset)
		end
		im = bytscl(im)
		case de of
		    'jpeg': write_jpeg,filename,im,quality=100
		    'tiff': write_tiff,filename,reverse(im,2)
		    'bmp': write_bmp,filename,im
		   else: MESSAGE,'unknown file format option'
		endcase
	  endcase


  'CLOSEMBCBED': begin
; kill the base widget
	WIDGET_CONTROL, widget_s.MBCBEDDrawbase, /DESTROY
	WIDGET_CONTROL, widget_s.MBCBEDbase, /DESTROY
	  CBEDprint,'MBCBED Widget closed'
	  CBEDprint,'MBCBED Draw Widget closed'
	endcase

  else: MESSAGE, "Event User Value Not Found"

  END

endelse


end
