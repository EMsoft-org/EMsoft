;
; Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
; CTEMsoft2013:STEMevent.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMevent.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief special event handler for all the CW_BGROUP calls, since CW_BGROUP does not support event_pro
;
;> @date 06/13/13 MDG 1.0 first version
;--------------------------------------------------------------------------
function STEMevent, event

;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common STEM_detectordata, STEMdata, STEMcimage, BFdisk, DFdisk, clickablemap, STEMsectormaps, STEMsectors, STEMsectorranges
common STEM_CBEDpatterns, CBED, CBEDdisplay
; and this one is used to create the blue channel of the detector plot
common STEM_circles, th, cth, sth, blue, diskpos
; the next common block contains all the raw data needed to generate the CBED patterns
common STEM_rawdata, indices, offsets, kperp, rawdata
common STEM_masks, ktpg, ktpgang, BFmask, HAADFmask, BFindices, HAADFindices, BFcnt, HAADFcnt
common STEM_images, BFimage, HAADFimage, DFimage


if (data.eventverbose eq 1) then help,event,/structure


WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

CASE eventval OF

 'SECTORMODE': begin
		if (data.diffractionmode eq 0) then begin
		  WIDGET_CONTROL, get_value=val,widget_s.sectormode
		  data.sectormode = fix(val[0])
		  if (data.sectormode eq 0) then begin
			  STEMprint,'sector selection mode set to single sector',/blank
		  end else begin 
			  STEMprint,'sector selection mode set to multiple sectors',/blank
		  end
		endif
	  endcase

 'DIFFRACTIONMODE': begin
		WIDGET_CONTROL, get_value=val,widget_s.diffractionmode
		data.diffractionmode = fix(val[0])
		if (data.diffractionmode eq 0) then begin
			if (XRegistered("STEMImageWidget") EQ 0) then STEMImageWidget
			STEMprint,'diffraction mode set to HAADF mode',/blank
			WIDGET_CONTROL, widget_s.gosector,sensitive=1
			WIDGET_CONTROL, widget_s.clearsector,sensitive=1
			WIDGET_CONTROL, widget_s.aprad,sensitive=0
; get the number of segments from the corresponding widget, since it was reset to 1 for regular diffraction mode
			WIDGET_CONTROL, get_value=val,widget_s.detsegm
			data.detsegm = fix(val[0])
			STEMdetectorsetup
		end else begin 
			if (XRegistered("STEMImageWidget") EQ 0) then STEMImageWidget
			WIDGET_CONTROL, widget_s.gosector,sensitive=0
			WIDGET_CONTROL, widget_s.clearsector,sensitive=0
			WIDGET_CONTROL, widget_s.aprad,sensitive=1
			WIDGET_CONTROL, SET_VALUE=0, widget_s.dfmode
			data.dfmode = 0
			STEMprint,'diffraction mode set to conventional (aperture driven) dark field mode',/blank
			STEMdetectorsetup,/darkfield	; this creates the array of blue disks
			data.detsegm = 1
			wset,widget_s.BFdrawID
			erase
			wset,widget_s.HAADFdrawID
			erase
		end

	  endcase

 'DFMODE': begin
; accept this only if data.diffractionmode = 1
	     if (data.diffractionmode eq 0) then begin
		WIDGET_CONTROL,SET_VALUE=data.dfmode,widget_s.dfmode ; don't allow user to change this 
	     end else begin
		WIDGET_CONTROL, get_value=val,widget_s.dfmode
		data.dfmode = fix(val[0])
		if (data.dfmode eq 0) then begin
		  STEMprint,'Select a point in the central disk'
		  WIDGET_CONTROL, TOOLTIP='Click on location of aperture center in BF disk', widget_s.detdraw
		end else begin
		  STEMprint,'Select a point in the central disk'
		  WIDGET_CONTROL, TOOLTIP='Select a diffracted disk to display dark field image', widget_s.detdraw
		endelse 
	     endelse
	  endcase

 'LOGFILE':  begin
; toggle the log mode 
		if (data.logmode eq 0) then begin
		   STEMprint,'Turning log mode on',/blank
		 q = systime()
 		 z = strsplit(q,' ',/extract,/regex)
 		 data.logname = data.pathname+'/STEMDisplay'+z[0]+z[1]+z[2]+'_'+z[3]+'_'+z[4]+'.log'
		 STEMprint,'Log file: '+data.logname
		 data.logmode = 1
		 openw,data.logunit,data.logname
		 data.logfileopen = 1
		end else begin
		   STEMprint,'Turning log mode off',/blank
		 if (data.logfileopen eq 1) then begin
		   close,data.logunit
		   data.logfileopen = 0
		 endif
	    	 data.logmode = 0
		endelse
	  endcase
  

  'CBEDFORMAT': begin
		WIDGET_CONTROL, get_value=val,widget_s.cbedformatbgroup
		data.cbedformat = fix(val[0])
	  endcase

  'CBEDLEGEND': begin
		  WIDGET_CONTROL, get_value=val,widget_s.cbedlegendbgroup
		  data.cbedlegend = fix(val[0])
		  STEMshowCBED
	  endcase

  'CBEDMODE': begin
		WIDGET_CONTROL, get_value=val,widget_s.cbedmodebgroup
		data.cbedmode = fix(val[0])
		STEMshowCBED
	  endcase

  'CBEDZOOM': begin
		WIDGET_CONTROL, get_value=val,widget_s.CBEDzoom
		cbedzoom = fix(val[0])
		case (cbedzoom) of
		'0': data.CBEDzoom = 1
		'1': data.CBEDzoom = 2
		'2': data.CBEDzoom = 4
		endcase

		if (data.CBEDzoom ne 1) then begin
  		  mid = (data.CBEDzoom * data.patx)/2
  		  CBEDdisplay = rebin(CBED,data.CBEDzoom*data.patx,data.CBEDzoom*data.paty)
  		  CBEDdisplay = CBEDdisplay[mid-data.patx/2:mid+data.patx/2-1,mid-data.paty/2:mid+data.paty/2-1]
		end else begin
  		  CBEDdisplay = CBED
		endelse

		STEMshowCBED
	  endcase


  'IMAGEFORMAT': begin
		WIDGET_CONTROL, get_value=val,widget_s.imageformatbgroup
		data.imageformat = fix(val[0])
	  endcase

  'IMAGELEGEND': begin
		WIDGET_CONTROL, get_value=val,widget_s.imagelegendbgroup
		data.imagelegend = fix(val[0])
		if (data.diffractionmode eq 0) then begin
		  wset,widget_s.BFdrawID
		  tvscl,BFimage
		  wset,widget_s.HAADFdrawID
		  tvscl,HAADFimage
		  if (data.imagelegend eq 1) then STEMimagelegend,widget_s.BFdrawID
		end else begin
		  wset,widget_s.HAADFdrawID
		  tvscl,DFimage
		  if (data.imagelegend eq 1) then STEMimagelegend,widget_s.HAADFdrawID
		endelse
	  endcase


else: MESSAGE, "Event User Value Not Found"

endcase

return,eventval
end 
