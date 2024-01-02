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
; CTEMsoft2013:CBEDLACBEDWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDLACBEDWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for LACBED mode
;
;> @date 10/09/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro CBEDLACBEDWidget_event, event

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common CBED_current, BFcurrent, DFcurrent, RGBcurrent, mask

if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.LACBEDbase) then begin
  data.LACBEDxlocation = event.x
  data.LACBEDylocation = event.y-25
    CBEDprint,' Window moved to location ('+string(fix(data.LACBEDxlocation),format="(I4)")+','+string(fix(data.LACBEDylocation),format="(I4)")+')'
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
 'CBEDmode': begin
	  WIDGET_CONTROL, get_value=val,widget_s.cbedmodebgroup
	  data.cbedmode= fix(val[0])
	    CBEDprint,'CBED intensity mode set to '+string(data.cbedmode,format="(I1)")
	endcase

 'DFDISPLAYMODE': begin
	  WIDGET_CONTROL, get_value=val,widget_s.dfdisplaymode
	  data.dfdisplaymode = fix(val[0])
	    CBEDprint,'Display mode set to '+string(data.dfdisplaymode,format="(I1)")	
	  if (data.dfdisplaymode eq 2) then begin
	    WIDGET_CONTROL, set_value='Eades mode', widget_s.gsel
	  end else begin
	    wv = '('+string(gvecs[0,data.famsel],format="(I3)")+' '+ string(gvecs[1,data.famsel],format="(I3)")+' '+ string(gvecs[2,data.famsel],format="(I3)")+')'
	    WIDGET_CONTROL, set_value=wv, widget_s.gsel
	  end
	endcase

 'HOLZlayer0': begin
	  ioffset = 0
;  WIDGET_CONTROL, get_value=val,widget_s.LACBEDdroplist0
	  data.famsel = ioffset + fix(event.index) - 1
	  wv = '('+string(gvecs[0,data.famsel],format="(I3)")+' '+ string(gvecs[1,data.famsel],format="(I3)")+' '+ string(gvecs[2,data.famsel],format="(I3)")+')'
	  WIDGET_CONTROL, set_value=wv, widget_s.gsel
	endcase

 'HOLZlayer1': begin
	  ioffset = numHOLZ[0]
;  WIDGET_CONTROL, get_value=val,widget_s.LACBEDdroplist1
	  data.famsel = ioffset + fix(event.index) - 1
	  wv = '('+string(gvecs[0,data.famsel],format="(I3)")+' '+ string(gvecs[1,data.famsel],format="(I3)")+' '+ string(gvecs[2,data.famsel],format="(I3)")+')'
	  WIDGET_CONTROL, set_value=wv, widget_s.gsel
	endcase

 'HOLZlayer2': begin
	  ioffset = total(numHOLZ[0:1])
;  WIDGET_CONTROL, get_value=val,widget_s.LACBEDdroplist2
	  data.famsel = ioffset + fix(event.index) - 1
	  wv = '('+string(gvecs[0,data.famsel],format="(I3)")+' '+ string(gvecs[1,data.famsel],format="(I3)")+' '+ string(gvecs[2,data.famsel],format="(I3)")+')'
	  WIDGET_CONTROL, set_value=wv, widget_s.gsel
	endcase

 'HOLZlayer3': begin
	  ioffset = total(numHOLZ[0:2])
;  WIDGET_CONTROL, get_value=val,widget_s.LACBEDdroplist3
	  data.famsel = ioffset + fix(event.index) - 1
	  wv = '('+string(gvecs[0,data.famsel],format="(I3)")+' '+ string(gvecs[1,data.famsel],format="(I3)")+' '+ string(gvecs[2,data.famsel],format="(I3)")+')'
	  WIDGET_CONTROL, set_value=wv, widget_s.gsel
	endcase

 'EADESMIN':  begin
		WIDGET_CONTROL, get_value=val,widget_s.eadesrhoin
		data.Eadesrhoin = float(val[0])
		if (data.Eadesrhoin gt data.Eadesrhoout) then begin
		  data.Eadesrhoin -= 1.0
		end
		if (data.Eadesrhoin lt 0.0) then begin
		  data.Eadesrhoin = 0.01    ; avoid the zero beam
		end
		  CBEDprint,'Eades minimum angle set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(data.Eadesrhoin,format="(F6.2)"), widget_s.eadesrhoin
	endcase

 'EADESMAX':  begin
		WIDGET_CONTROL, get_value=val,widget_s.eadesrhoout
		data.Eadesrhoout = float(val[0])
		if (data.Eadesrhoout lt data.Eadesrhoin) then begin
		  data.Eadesrhoout = data.Eadesrhoin+0.1
		end
		  CBEDprint,'Eades maximum angle set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(data.Eadesrhoout,format="(F6.2)"), widget_s.eadesrhoout
	endcase

 'DISKROTATION':  begin
		WIDGET_CONTROL, get_value=val,widget_s.diskrotation
		data.diskrotation = float(val[0])
		  CBEDprint,'Disk rotation angle set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(data.diskrotation,format="(F6.2)"), widget_s.diskrotation
	endcase

 'LOGOFFSET':  begin
		WIDGET_CONTROL, get_value=val,widget_s.logoffset
		data.logoffset= float(val[0])
		  CBEDprint,'Logarithm offset value set to '+string(float(val[0]),FORMAT="(E9.2)")
		WIDGET_CONTROL, set_value=string(data.logoffset,format="(E9.2)"), widget_s.logoffset
	endcase


 'GOLACBED':  begin
; first display the BF image using the correct mode
	  data.lastmode = 0
	  wset,data.BFdrawID
	  z = disks[*,*,data.thicksel,0]
	  data.BFmin = min(z,max = ma)
	  data.BFmax = ma
	  if (data.diskrotation ne 0.0) then z = rot(z,data.diskrotation,cubic=-0.5)
	  z *= mask
	  BFcurrent = z
	  tvscl, BFcurrent
	  WIDGET_CONTROL, SET_VALUE=string(data.BFmin,FORMAT="(E9.2)"), widget_s.BFmin
	  WIDGET_CONTROL, SET_VALUE=string(data.BFmax,FORMAT="(E9.2)"), widget_s.BFmax
; then display the appropriate dark field pattern (single, symmetric single, or Eades)
	  wset,data.DFdrawID
	  case data.dfdisplaymode of 
; single DF image
	    0: begin
	  	 z = disks[*,*,data.thicksel,data.famsel]
	  	 data.DFmin = min(z,max = ma)
	  	 data.DFmax = ma
		 if (data.diskrotation ne 0.0) then z = rot(z,data.diskrotation,cubic=-0.5)
		 z *= mask
	  	 if (data.cbedmode eq 0) then begin
	    	   DFcurrent = z
	    	   tvscl, DFcurrent
	  	 end else begin
	           DFcurrent = alog10(z+data.logoffset)
	           tvscl, DFcurrent
	  	 end
	       endcase

; symmetrized DF image
	    1: begin
	  	 z = disks[*,*,data.thicksel,data.famsel]
		 z = CBEDApply2DSymmetry(z,gmult[data.famsel])
	  	 data.DFmin = min(z,max = ma) > 0.0
	  	 data.DFmax = ma
		 if (data.diskrotation ne 0.0) then z = rot(z,data.diskrotation,cubic=-0.5)
		 z *= mask
	  	 if (data.cbedmode eq 0) then begin
	    	   DFcurrent = z
	    	   tvscl, DFcurrent
	  	 end else begin
	           DFcurrent = alog10(z+data.logoffset)
	           tvscl, DFcurrent
	  	 end
	       endcase

; Eades pattern
	    2: begin
		 z = fltarr(data.datadims[0],data.datadims[1])
		 isum = 1
		 for i=1,data.numfam-1 do begin
		   if ( (gtt[i] ge data.Eadesrhoin) and (gtt[i] le data.Eadesrhoout) ) then begin
		     z += CBEDApply2DSymmetry(reform(disks[*,*,data.thicksel,i]), gmult[i])
		     isum += 1
		   end
		 end
		 z = z/float(isum)
	  	 data.DFmin = min(z,max = ma) > 0.0
	  	 data.DFmax = ma
		 if (data.diskrotation ne 0.0) then z = rot(z,data.diskrotation,cubic=-0.5)
		 z *= mask
	  	 if (data.cbedmode eq 0) then begin
	    	   DFcurrent = z
	    	   tvscl, DFcurrent
	  	 end else begin
	           DFcurrent = alog10(z+data.logoffset)
	           tvscl, DFcurrent
	  	 end
	       endcase
 
	  endcase
	  WIDGET_CONTROL, SET_VALUE=string(data.DFmin,FORMAT="(E9.2)"), widget_s.DFmin
	  WIDGET_CONTROL, SET_VALUE=string(data.DFmax,FORMAT="(E9.2)"), widget_s.DFmax
	endcase

  'SURPRISELACBED': begin
	  data.lastmode = 1
; this is just for fun... display a colorized version of a random selection of ZOLZ patterns,
; symmetrized except for the BF pattern
	  rgb = bytarr(3,data.datadims[0],data.datadims[1])
	  sel = fix( randomu(seed,15) * float(numHOLZ[0]+numHOLZ[1]) ) 
; red channel
	  zr = replicate(0.0,data.datadims[0],data.datadims[1])
	  for i=0,4 do zr += CBEDApply2DSymmetry(reform(disks[*,*,data.thicksel,sel[i]]),0) 
	  if (data.diskrotation ne 0.0) then zr = rot(zr,data.diskrotation,cubic=-0.5)
	  zr *= mask
; green channel
	  zg = replicate(0.0,data.datadims[0],data.datadims[1])
	  for i=5,9 do zg += CBEDApply2DSymmetry(reform(disks[*,*,data.thicksel,sel[i]]),0) 
	  if (data.diskrotation ne 0.0) then zg = rot(zg,data.diskrotation,cubic=-0.5)
	  zg *= mask
; blue channel
	  zb = replicate(0.0,data.datadims[0],data.datadims[1])
	  for i=10,14 do zb += CBEDApply2DSymmetry(reform(disks[*,*,data.thicksel,sel[i]]),0) 
	  if (data.diskrotation ne 0.0) then zb = rot(zb,data.diskrotation,cubic=-0.5)
	  zb *= mask
; copy into the RGB channels
	  rgb[0,0:*,0:*] = bytscl(zr)
	  rgb[1,0:*,0:*] = bytscl(zg)
	  rgb[2,0:*,0:*] = bytscl(zb)
	  RGBcurrent = rgb
	  wset,data.DFdrawID
	  erase
	  wset,data.BFdrawID
	  tvscl,RGBcurrent,true=1
	endcase

  'SAVELACBED': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[data.cbedformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
		if (data.lastmode eq 0) then begin
		  im = bytarr(2*data.datadims[0],data.datadims[0])
		  im[0,0] = bytscl(BFcurrent)
		  im[data.datadims[0],0] = bytscl(DFcurrent)
		  case de of
		    'jpeg': write_jpeg,filename,im,quality=100
		    'tiff': write_tiff,filename,reverse(im,2)
		    'bmp': write_bmp,filename,im
		   else: MESSAGE,'unknown file format option'
		  endcase
		end else begin  ; it's an RGB image
		  case de of
		    'jpeg': write_jpeg,filename,RGBcurrent,true=1,quality=100
		    'tiff': begin
			rgb = RGBcurrent
			for i=0,2 do begin
			  slice = reform(rgb[i,*,*])
			  rgb[i,0:*,0:*] = reverse(slice,2)
			end
			write_tiff,filename,rgb
		      endcase
		    'bmp': write_bmp,filename,RGBcurrent,/RGB
		   else: MESSAGE,'unknown file format option'
		  endcase
		end
	  endcase


 'CBEDLACBEDTHICKLIST': begin
	data.thicksel = event.index
	data.thickness = data.startthick + float(event.index) * data.thickinc
	  CBEDprint,'Foil thickness set to '+string(data.thickness,FORMAT="(F6.1)")+' nm'
	endcase

 'CLOSELACBED': begin
; kill the base widget
	WIDGET_CONTROL, widget_s.LACBEDDrawbase, /DESTROY
	WIDGET_CONTROL, widget_s.LACBEDbase, /DESTROY
	  CBEDprint,'LACBED Widget closed'
	  CBEDprint,'LACBED Draw Widget closed'
	endcase

  else: MESSAGE, "Event User Value Not Found"

  END

endelse


end
