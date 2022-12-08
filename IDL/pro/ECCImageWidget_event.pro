;
; Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
; CTEMsoft2013:ECCImageWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCImageWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 12/06/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECCImageWidget_event, event

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata
common ECCI_rawdata, indices, offsets, kperp, rawdata, ECCILUT


if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.ECCImagebase) then begin
  data.ECCIxlocation = event.x
  data.ECCIylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
 'MOSAICDIM': begin
	    WIDGET_CONTROL, get_value=val,widget_s.mosaicdim
	    data.mosaicdim = long(val[0])
		if (data.progmode eq 'array') then begin
		  ECCIprint,'ECCI mosaic dimension set to '+string(data.mosaicdim,FORMAT="(I6)")
		end else begin
		  ECCIprint,'# ECCI images per row set to '+string(data.mosaicdim,FORMAT="(I6)")
		endelse
	    WIDGET_CONTROL, SET_VALUE=string(data.mosaicdim,FORMAT="(I6)"), widget_s.mosaicdim
	endcase

 'AVRAD': begin
	    WIDGET_CONTROL, get_value=val,widget_s.avrad
	    data.avrad= float(val[0])
		ECCIprint,'ECCI averaging radius set to '+string(data.avrad,FORMAT="(F6.3)")
	    WIDGET_CONTROL, SET_VALUE=string(data.avrad,FORMAT="(F6.3)"), widget_s.avrad
	endcase

 'BLUR':  begin
	    WIDGET_CONTROL, get_value=val,widget_s.blur
	    data.blur= float(val[0])
	    WIDGET_CONTROL, SET_VALUE=string(data.blur,FORMAT="(F6.3)"), widget_s.blur
	endcase

 'DOMOSAIC': begin
; make a large image that contains the ECCI mosaic and then rescale it to the requested size and store it in a file
; we'll do this in the Z-buffer
	  set_plot,'Z'
	  if (data.progmode eq 'array') then begin
  	    kp = kperp
 	    kpsum = total(kperp,2)/float(data.numk)
  	    for i=0,data.numk-1 do kp[0:1,i] -= kpsum[0:1]
  	    ma = max(abs(kp))
	    dm = (2L*ma+1) * data.datadims[0]
	    device,set_resolution=[dm,dm]
	    erase
	    eccimin = min(rawdata,max=eccimax)
; write into the buffer
	    for i=0,data.numk-1 do begin
	      xp = (kp[0,i]+ma)*data.datadims[0]
   	      yp = (kp[1,i]+ma)*data.datadims[1]
   	      if (data.mosaicscale eq 0) then begin
	        tv,bytscl(rawdata[*,*,i],min=eccimin,max=eccimax),xp,yp
 	      end else begin
	        tvscl,rawdata[*,*,i],xp,yp
	      endelse
 	    endfor

; get the buffer
  	    big = tvrd()
	    set_plot,'X'

; scale it to the right size
	    big = congrid(big,data.mosaicdim,data.mosaicdim)
	  end else begin ; this is for a trace computation
	    ncols = data.mosaicdim
	    nrows = data.numk/ncols
	    if ((data.numk mod ncols) ne 0) then nrows += 1
	    dmx = ncols * data.datadims[0]
	    dmy = nrows * data.datadims[0]
	    device,set_resolution=[dmx,dmy]
	    erase
	    eccimin = min(rawdata,max=eccimax)
; write into the buffer
	    for i=0,data.numk-1 do begin
	      c = i mod ncols
	      r = nrows - 1 - (i / ncols)
	      xp = c*data.datadims[0]
   	      yp = r*data.datadims[1]
   	      if (data.mosaicscale eq 0) then begin
	        tv,bytscl(rawdata[*,*,i],min=eccimin,max=eccimax),xp,yp
 	      end else begin
	        tvscl,rawdata[*,*,i],xp,yp
	      endelse
 	    endfor

; get the buffer
  	    big = tvrd()
	    set_plot,'X'
	  endelse

; display a filesaving widget in the data folder with the file extension filled in
	  delist = ['jpeg','tiff','bmp']
	  de = delist[data.ecciformat]
	  filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
	  if (filename ne '') then begin
	   case de of
	    'jpeg': write_jpeg,filename,big,quality=100
	    'tiff': write_tiff,filename,reverse(big,2)
	    'bmp': write_bmp,filename,big
	    else: MESSAGE,'unknown file format option'
  	   endcase
	    ECCIprint,'ECCI mosaic saved to file '+filename
	  endif

	endcase

 'GETCOORDINATES': begin
	  if (event.press eq 1B) then begin    ; only act on clicks, not on releases
; we need to take the cordinates of the selected point in the ECCI image, and 
; extract all the ECCI intensities for that point in all images and combine them
; into a low resolution ECP ...
	    px = event.x
	    py = event.y

  	    kp = kperp
 	    kpsum = total(kperp,2)/float(data.numk)
  	    for i=0,data.numk-1 do kp[0:1,i] -= kpsum[0:1]
	    kp = fix(kp)
  	    ma = max(abs(kp))
	    kp += ma
	    miniECP = replicate(0.0,2*ma+1,2*ma+1)

	    for i=0,data.numk-1 do begin 
		miniECP[kp[0,i],kp[1,i]] = rawdata[px,py,i]
	    endfor
 
; and display the miniECP, scaled up as far as the window size will allow it...
	    mm = 2*ma+1
	    dm = round(float(data.datadims[0])/float(mm))
	    miniECP = congrid(miniECP,dm*mm,dm*mm)
	    wset,widget_s.ECCIavdrawID
	    tvscl,miniECP
	  end
	endcase

 'SAVEECCI': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[data.ecciformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
	 	if (filename ne '') then begin
	          ma = max(abs(kperp))
  	          z = sqrt( (reform(kperp[0,*])-ECPdata.cx)^2 + (reform(kperp[1,*])-ECPdata.cy)^2 )
  	          q = where(z eq min(z),cnt)
	      	  eccimin = min(rawdata,max=eccimax)
		  if (data.blur ne 0.0) then begin
  		    slice = ECCIblur(reform(rawdata[*,*,q[0]]),data.blur)
		  end else begin
  		    slice = reform(rawdata[*,*,q[0]])
		  endelse
   	          if (data.mosaicscale eq 0) then begin
	            im = bytscl(slice,min=eccimin,max=eccimax)
 	          end else begin
	            im = bytscl(slice)
	          endelse
		  case de of
		    'jpeg': write_jpeg,filename,im,quality=100
		    'tiff': write_tiff,filename,reverse(im,2)
		    'bmp': write_bmp,filename,im
		   else: MESSAGE,'unknown file format option'
		  endcase
		endif
	  endcase

 'SAVEAVECCI': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[data.ecciformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
	 	if (filename ne '') then begin
		  wset,widget_s.ECCIavdrawID
		  im = tvrd()
		  case de of
		    'jpeg': write_jpeg,filename,im,quality=100
		    'tiff': write_tiff,filename,reverse(im,2)
		    'bmp': write_bmp,filename,im
		   else: MESSAGE,'unknown file format option'
		  endcase
		endif
	  endcase

 'CLOSEECCI': begin
; kill the base widget
		WIDGET_CONTROL, widget_s.ECCImagebase, /DESTROY
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
