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
; CTEMsoft2013:STEMImageWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMImageWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 06/13/13 MDG 1.0 first version
;> @date 02/14/14 MDG 2.0 added file series output option
;--------------------------------------------------------------------------
pro STEMImageWidget_event, event

;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common STEM_detectordata, STEMdata, STEMcimage, BFdisk, DFdisk, clickablemap, STEMsectormaps, STEMsectors, STEMsectorranges
common STEM_CBEDpatterns, CBED, CBEDdisplay
common STEM_images, BFimage, HAADFimage, DFimage

if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.imagebase) then begin
  data.imagexlocation = event.x
  data.imageylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF
  'DRAWCBED':  begin
; this option generates the CBED widget and then draws the correct pattern in it.
		if (event.press eq 1B) then begin    ; only act on clicks, not on releases
		  if (XRegistered("STEMCBEDWidget") EQ 0) then STEMCBEDWidget
		  STEMcomputeCBEDpatterns,event.x,event.y
		end
	  endcase

  'DOSERIES' : begin
	  	 if (data.seriesroot ne 'noseries') then begin
; this assumes that the file name has the form string__0001.data and that there is a 
; series of consecutive files with that name; for each one, the file is read and the
; current imaging parameters are applied; then the corresponding images are stored in
; a folder called imageseries, with names image_0001.jpeg (tiff, bmp), so that one can
; make a movie out of the individual frames, without having to load each individual 
; input file manually...  [note the double underscore in the file name !]

; determine which folder the image series will be stored in
    		  data.seriesfoldernum = 1
    		  foldername = data.pathname + '/' + data.seriesroot + string(data.seriesfoldernum,format="(I4.4)")
    		  if (file_test(foldername,/directory) eq 1) then begin
      		  file_exists = 1
      		  while (file_exists eq 1) do begin
        		  foldername = data.pathname + '/' + data.seriesroot + string(data.seriesfoldernum+1,format="(I4.4)")
        		  if (file_test(foldername,/directory) ne 1) then begin
          		  data.seriesfoldernum += 1
	  		  cmd = '/bin/mkdir '+foldername
	  		  spawn,cmd
	  		  file_exists = 0
			  end else data.seriesfoldernum += 1
      		  end 
    		  end else begin
			  cmd = '/bin/mkdir '+foldername
			  spawn,cmd
    		  end
    		  data.seriesfolder = foldername

 		  for inum=data.seriesstart,data.serieslast do begin
		   data.dataname = data.seriesroot+'__'+string(inum,format="(I4.4)")+'.'+data.seriestype
		   STEMreadgeometry
	  	   STEMcomputeBFHAADF

		   delist = ['jpeg','tiff','bmp']
		   de = delist[data.imageformat]
		   filename = data.seriesfolder+'/'+data.seriesroot+'_'+string(inum,format="(I4.4)")+'.'+de
		   im = bytarr(2*data.datadims[0],data.datadims[1])
		   im[0,0] = bytscl(BFimage)
		   if (data.diffractionmode eq 0) then im[data.datadims[0],0] = bytscl(HAADFimage) else im[data.datadims[0],0] = bytscl(DFimage)
		   case de of
		     'jpeg': write_jpeg,filename,im,quality=100
		     'tiff': write_tiff,filename,reverse(im,2)
		     'bmp': write_bmp,filename,im
		    else: MESSAGE,'unknown file format option'
		   endcase
		 end
		end
	  endcase

  'SAVEIMAGE': begin
; display a filesaving widget in the data folder with the file extension filled in
		delist = ['jpeg','tiff','bmp']
		de = delist[data.imageformat]
		filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
		im = bytarr(2*data.datadims[0],data.datadims[1])
		im[0,0] = bytscl(BFimage)
		if (data.diffractionmode eq 0) then im[data.datadims[0],0] = bytscl(HAADFimage) else im[data.datadims[0],0] = bytscl(DFimage)
		case de of
		  'jpeg': write_jpeg,filename,im,quality=100
		  'tiff': write_tiff,filename,reverse(im,2)
		  'bmp': write_bmp,filename,im
		 else: MESSAGE,'unknown file format option'
		endcase
	  endcase

 'CLOSEIMAGE': begin
; kill the base widget
		WIDGET_CONTROL, widget_s.imagebase, /DESTROY
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
