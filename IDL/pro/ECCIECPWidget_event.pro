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
; CTEMsoft2013:ECCIECPWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIECPWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 12/06/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECCIECPWidget_event, event

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata
common ECCI_rawdata, indices, offsets, kperp, rawdata, ECCILUT


if (data.progmode eq 'trace') then begin
  dkt = 1.0
end else begin
  dkt = data.dkt
endelse

dkt = 1.0

if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.ECCIECPbase) then begin
  data.ECPxlocation = event.x
  data.ECPylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
 'GETCOORDINATES': begin
	  if (event.press eq 1B) then begin    ; only act on clicks, not on releases
; we need to put these in units of dkt
;    if (data.progmode ne 'array') then begin
	      bx = (event.x - ECPdata.xmid) / ECPdata.dgrid / dkt 
	      by = (event.y - ECPdata.xmid) / ECPdata.dgrid / dkt 
  	      z = sqrt( (reform(kperp[0,*])-bx)^2 + (reform(kperp[1,*])-by)^2 )
  	      q = where(z eq min(z),cnt)
	      ECPdata.cx = kperp[0,q[0]]
	      ECPdata.cy = kperp[1,q[0]]
;    end else begin
;      ECPdata.cx = float(round( (event.x - ECPdata.xmid) / ECPdata.dgrid / dkt ))
;      ECPdata.cy = float(round( (event.y - ECPdata.xmid) / ECPdata.dgrid / dkt ))
;    endelse

	    WIDGET_CONTROL, SET_VALUE=string(float(ECPdata.cx)*dkt,format="(F6.3)"), widget_s.cx
	    WIDGET_CONTROL, SET_VALUE=string(float(ECPdata.cy)*dkt,format="(F6.3)"), widget_s.cy
	    
; we indicate the selected point by turning it blue
            ECCIECPshow,/point

; then we need to create the ECCI image widget, if it doesn't already exist
	    if (XRegistered("ECCImageWidget") EQ 0) then ECCImageWidget

; and display the proper ECCI image
	    ma = max(abs(kperp))
	    wset,widget_s.ECCIdrawID
	    case data.progmode of
		'trace': begin
	      	    slice = reform(rawdata[*,*,q[0]])
	      	    eccimin = min(rawdata,max=eccimax)
		    if (data.blur ne 0.0) then begin
  			slice = ECCIblur(slice,data.blur)
		    endif

	      	    if (data.mosaicscale eq 0) then begin
	              im = bytscl(slice,min=eccimin,max=eccimax)
 	            end else begin
	              im = bytscl(slice)
	            endelse
	            tv,im
                    mi = min(slice,max=ma)
	            WIDGET_CONTROL, SET_VALUE=string(mi,format="(F6.1)"), widget_s.ECCIdrawmin
	            WIDGET_CONTROL, SET_VALUE=string(ma,format="(F6.1)"), widget_s.ECCIdrawmax


; then we also need to display the averaged ECCI image
	            wset,widget_s.ECCIavdrawID
	            erase
	            WIDGET_CONTROL, SET_VALUE=string(0.0,format="(F6.1)"), widget_s.ECCIavdrawmin
	            WIDGET_CONTROL, SET_VALUE=string(0.0,format="(F6.1)"), widget_s.ECCIavdrawmax
	            if (data.avrad ne 0.0) then begin
	              ECCIsum = fltarr(data.datadims[0],data.datadims[1])
	              npat = 0
	              for i=0,data.numk-1 do if (z[i] le data.avrad) then begin
		         if (data.blur ne 0.0) then begin
		            ECCISUM += ECCIblur(reform(rawdata[*,*,i]),data.blur)
			 end else begin
		            ECCISUM += reform(rawdata[*,*,i])
		         endelse
	                npat += 1
	              endif
		        ECCIprint,'Number of patterns averaged : '+string(npat,format="(I4)")
	              if (npat ne 0) then begin
		        ECCIsum /= float(npat)
	                if (data.mosaicscale eq 0) then begin
	                  im = bytscl(ECCIsum,min=eccimin,max=eccimax)
 	                end else begin
	                  im = bytscl(ECCIsum)
	                endelse
	                tv,im
                        mi = min(ECCIsum,max=ma)
	                WIDGET_CONTROL, SET_VALUE=string(mi,format="(F6.1)"), widget_s.ECCIavdrawmin
	                WIDGET_CONTROL, SET_VALUE=string(ma,format="(F6.1)"), widget_s.ECCIavdrawmax
	              endif
	            endif
		endcase

		'array': begin
	      	      slice = reform(rawdata[*,*,q[0]])
	      	      eccimin = min(rawdata,max=eccimax)
		      if (data.blur ne 0.0) then begin
  			  slice = ECCIblur(slice,data.blur)
		      endif
	      	      if (data.mosaicscale eq 0) then begin
	        	    im = bytscl(slice,min=eccimin,max=eccimax)
 	      	      end else begin
	        	    im = bytscl(slice)
	      	      endelse
	      	      tv,im
              	      mi = min(slice,max=ma)
	      	      WIDGET_CONTROL, SET_VALUE=string(mi,format="(F6.1)"), widget_s.ECCIdrawmin
	      	      WIDGET_CONTROL, SET_VALUE=string(ma,format="(F6.1)"), widget_s.ECCIdrawmax

; then we also need to display the averaged ECCI image
; first determine all the points that are inside the circle and add the corresponding ECCI images 
; together
	      	      wset,widget_s.ECCIavdrawID
	      	      erase
	      	      WIDGET_CONTROL, SET_VALUE=string(0.0,format="(F6.1)"), widget_s.ECCIavdrawmin
	      	      WIDGET_CONTROL, SET_VALUE=string(0.0,format="(F6.1)"), widget_s.ECCIavdrawmax
	      	      if (data.avrad ne 0.0) then begin
	        	dd = sqrt( (reform(kperp[0,*])-ECPdata.cx)^2 + (reform(kperp[1,*])-ECPdata.cy)^2 )
	        	ECCIsum = fltarr(data.datadims[0],data.datadims[1])
	        	npat = 0
	        	for i=0,data.numk-1 do if (dd[i] le data.avrad) then begin
		         if (data.blur ne 0.0) then begin
		            ECCISUM += ECCIblur(reform(rawdata[*,*,i]),data.blur)
			 end else begin
		            ECCISUM += reform(rawdata[*,*,i])
		         endelse
	          	  npat += 1
	        	endif
		  	ECCIprint,'Number of patterns averaged : '+string(npat,format="(I4)")
	        	if (npat ne 0) then begin
		  	  ECCIsum /= float(npat)
	          	  if (data.mosaicscale eq 0) then begin
	            	    im = bytscl(ECCIsum,min=eccimin,max=eccimax)
 	          	  end else begin
	            	    im = bytscl(ECCIsum)
	          	  endelse
	          	  tv,im
                  	  mi = min(ECCIsum,max=ma)
	          	  WIDGET_CONTROL, SET_VALUE=string(mi,format="(F6.1)"), widget_s.ECCIavdrawmin
	          	  WIDGET_CONTROL, SET_VALUE=string(ma,format="(F6.1)"), widget_s.ECCIavdrawmax
	        	endif
	      	      endif
		endcase

		else: MESSAGE,"Unknown program mode "+data.progmode
	     endcase
	  end
	endcase

 'ECPTHICKLIST': begin
	  ECPdata.thicksel = event.index

; and display the selected ECPattern
          ECCIECPshow
	endcase

 'SAVEECP': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[ECPdata.ecpformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
		im = bytscl(rawdata[*,*,ECPdata.thicksel])
		case de of
		  'jpeg': write_jpeg,filename,im,quality=100
		  'tiff': write_tiff,filename,reverse(im,2)
		  'bmp': write_bmp,filename,im
		 else: MESSAGE,'unknown file format option'
		endcase
	  endcase

 'CLOSEECP': begin
; kill the base widget
		WIDGET_CONTROL, widget_s.ECCIECPbase, /DESTROY
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
