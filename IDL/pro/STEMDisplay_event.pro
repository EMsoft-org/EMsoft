;
; Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
; CTEMsoft2013:STEMDisplay_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMDisplay_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 06/13/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro STEMDisplay_event, event

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


if (data.eventverbose eq 1) then help,event,/structure

if (event.id eq widget_s.base) then begin
  data.xlocation = event.x
  data.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF

 'BFRHO': begin
		WIDGET_CONTROL, get_value=val,widget_s.BFrho
		data.BFrho = float(val[0])
		  STEMprint,'BF detector radius set to '+string(float(val[0]),FORMAT="(F6.2)")
		if (data.BFrho gt data.HAADFrhoin) then begin
		  data.HAADFrhoin = data.BFrho
		  WIDGET_CONTROL, set_value=string(data.HAADFrhoin,format="(F6.2)"), widget_s.HAADFrhoin
		end
		STEMdetectorsetup
		WIDGET_CONTROL, set_value=string(val[0],format="(F6.2)"), widget_s.BFrho
	  endcase

 'HAADFRHOIN': begin
		WIDGET_CONTROL, get_value=val,widget_s.HAADFrhoin
		data.HAADFrhoin = float(val[0])
		STEMdetectorsetup
		  STEMprint,'HAADF detector inner radius set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(val[0],format="(F6.2)"), widget_s.HAADFrhoin
	  endcase

 'HAADFRHOOUT': begin
		WIDGET_CONTROL, get_value=val,widget_s.HAADFrhoout
		data.HAADFrhoout = float(val[0])
		STEMdetectorsetup
		  STEMprint,'HAADF detector outer radius set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(val[0],format="(F6.2)"), widget_s.HAADFrhoout
	  endcase

 'DETSEGM': begin
		WIDGET_CONTROL, get_value=val,widget_s.detsegm
		data.detsegm = fix(val[0])
		STEMdetectorsetup
		  STEMprint,'HAADF detector number of segments set to '+string(fix(val[0]),FORMAT="(I4)")
		WIDGET_CONTROL, set_value=string(val[0],format="(I4)"), widget_s.detsegm
	  endcase

 'ANGSEGM': begin
		WIDGET_CONTROL, get_value=val,widget_s.angsegm
		data.angsegm = float(val[0])
		STEMdetectorsetup
		  STEMprint,'HAADF detector rotation angle set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(val[0],format="(F6.2)"), widget_s.angsegm
	  endcase

 'CAMLEN': begin
		WIDGET_CONTROL, get_value=val,widget_s.camlen
		data.camlen= float(val[0])
		if (data.diffractionmode eq 0) then STEMdetectorsetup else STEMdetectorsetup,/darkfield
		  STEMprint,'camera length set to '+string(float(val[0]),FORMAT="(F8.2)")
		WIDGET_CONTROL, set_value=string(val[0],format="(F8.2)"), widget_s.camlen
	  endcase


 'SELECTSECTOR': begin
	        if (data.srzamode eq 'ZA') then zeropos = 0 else zeropos = (data.numref-1)/2
		if (event.press eq 1B) then begin    ; only act on clicks, not on releases
		  sel = clickablemap[event.x,event.y]
print,'selectsector: ',sel,event.x,event.y
		  if (data.diffractionmode eq 0) then begin
		    if (sel gt 0) then begin
		      if (data.sectormode eq 0) then begin   	; single sector mode
		    	  slice = 150B * reform(STEMsectormaps[*,*,sel])
			  STEMsectors[0:*] = 0B
		      end else begin				; multiple sector mode
		    	  sector = reform(STEMcimage[2,*,*]) + 150B * reform(STEMsectormaps[*,*,sel])
		          slice = reform(STEMcimage[2,*,*])
		          q = where((sector gt 0) and (slice ne 255B),cnt)
		          if (cnt gt 0) then slice[q] = 150B
		      endelse
		        STEMprint,' Selected sector #'+string(sel,FORMAT="(I2)")
		      STEMsectors[sel] = 1B
		      STEMcimage[2,0:*,0:*] = slice
		      wset,widget_s.detdrawID
		      tvscl,STEMcimage,true=1
		      STEMdrawdisks
		    endif
		  end else begin   ; dark field display mode 
; what we do here depends on the data.dfmode value
		      if (data.dfmode eq 0) then begin 		; set the k-vector position
; these are in pixel coordinates
;	        if (data.srzamode eq 'ZA') then data.apx = -(event.x-200) else data.apx = event.x-200
data.apx = event.x-200

		        data.apy = event.y-200
			dis = sqrt(data.apx^2+data.apy^2)
			if (dis gt data.rdisk) then goto,skiptherest
; next, make sure that if this point is close to the disk edge, the aperture size is not too large; reduce if necessary
			ratio = data.aprad/data.thetac
			appix = data.rdisk * ratio    ; aperture radius in pixels
			diff = dis + appix
			if (diff gt data.rdisk) then begin
; reset the aperture radius
			  newappix = data.rdisk - dis
			  data.aprad = data.thetac * newappix / data.rdisk
			  if (data.aprad lt data.apminrad) then data.aprad = data.apminrad
			  WIDGET_CONTROL, SET_VALUE=string(data.aprad,FORMAT="(F6.2)"), widget_s.aprad
		            STEMprint,'Aperture radius reset to meaningful value '+string(data.aprad,FORMAT="(F6.2)")
			endif
; draw the pattern
			STEMdrawdisks,/darkfield,/addaperture
; determine which pixels contribute to the aperture signal
			STEMgetmasks,/aperture
; if there are beam directions inside the aperture, then compute the bright field image
			if (array_equal(BFindices,[0]) eq 0) then begin
			  s = size(BFindices,/dimensions)
			  BFcnt = s[0]
			  sel = 0
			  STEMcomputeBFHAADF,selection=sel,/aperture
			endif
			skiptherest:
		      end else begin
; select a g-vector
;	        if (data.srzamode eq 'ZA') then px = -(event.x-200) else px = event.x - 200
		        px = event.x - 200
		        py = event.y - 200
		        d = sqrt( (diskpos[0,*]-px)^2 + (diskpos[1,*]-py)^2 )
		        q = where(d eq min(d))
		        sel = q[0]		; number of selected reflection
		        hkl = indices(0:2,sel)
		          STEMprint,'Reflection selected : '+string(hkl[0],FORMAT="(I3)")+' '+ $
		      	    string(hkl[1],FORMAT="(I3)")+' '+string(hkl[2],FORMAT="(I3)")+'; '+string(sel,format="(I2)")
			STEMdrawdisks,/darkfield,/addaperture,highlightdisk=sel
			if (array_equal(BFindices,[0]) eq 0) then begin
			  s = size(BFindices,/dimensions)
			  BFcnt = s[0]
			  STEMcomputeBFHAADF,selection=sel,/aperture
			endif
		      endelse
		  endelse
		endif
	  endcase

 'APRAD': begin
		WIDGET_CONTROL, get_value=val,widget_s.aprad
		data.aprad= float(val[0])
; check on the value; can not be smaller than data.apminrad and cannot be larger than data.thetac
		if (data.aprad lt data.apminrad) then begin
		  STEMprint,'Zero value not allowed for aperture radius; value changed to first meaningful value'
		  data.aprad = data.apminrad
		endif
		if (data.aprad gt data.thetac) then begin
		  STEMprint,'Aperture radius can not be larger than the beam divergence angle'
		  data.aprad = data.thetac
		endif
		  STEMprint,'Aperture radius set to [mrad] '+string(data.aprad,FORMAT="(F5.2)")
		WIDGET_CONTROL, set_value=string(data.aprad,format="(F5.2)"), widget_s.aprad
		STEMdrawdisks,/darkfield
	  endcase

  'GOSECTOR': begin
; this option does the actual calculation of the BF and HAADF images
		if (XRegistered("STEMImageWidget") EQ 0) then STEMImageWidget
		STEMgetmasks
		STEMcomputeBFHAADF
	  endcase

  'CLEARSECTOR': begin
		STEMdetectorsetup
	  endcase

  'LOADFILE': begin
; loading a new file means that a bunch of variables need to be reset
; ask the user to select an input geometry file
		if (XRegistered("STEMCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.cbedbase, /DESTROY
		if (XRegistered("STEMImageWidget") NE 0) then WIDGET_CONTROL, widget_s.imagebase, /DESTROY
		if (XRegistered("STEMCTEMBFDFWidget") NE 0) then WIDGET_CONTROL, widget_s.CTEMBFDFbase, /DESTROY
		STEMgetfilename
		data.progmode = 'STEM'

; read the data file and populate all the relevant fields
		STEMreadgeometry

; reset the diffraction mode
		data.diffractionmode = 0
		WIDGET_CONTROL,SET_VALUE=data.diffractionmode,widget_s.diffractionmode 
		data.detsegm = 1
		WIDGET_CONTROL, SET_VALUE=string(data.detsegm,format="(I3)"),widget_s.detsegm
		WIDGET_CONTROL, widget_s.gosector,sensitive=1
		WIDGET_CONTROL, widget_s.clearsector,sensitive=1
		WIDGET_CONTROL, widget_s.aprad,sensitive=0
		WIDGET_CONTROL, get_value=val,widget_s.detsegm
		data.detsegm = fix(val[0])
	endcase
		
  'LOADCTEMFILE': begin
; loading a new file means that a bunch of variables need to be reset
; ask the user to select an input file
		if (XRegistered("STEMCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.cbedbase, /DESTROY
		if (XRegistered("STEMImageWidget") NE 0) then WIDGET_CONTROL, widget_s.imagebase, /DESTROY
		if (XRegistered("STEMCTEMBFDFWidget") NE 0) then WIDGET_CONTROL, widget_s.CTEMBFDFbase, /DESTROY
		STEMgetfilename
		data.progmode = 'CTEM'

; read the data file and populate all the relevant fields
		STEMreadgeometry

; reset the diffraction mode
		data.diffractionmode = 0
		WIDGET_CONTROL,SET_VALUE=data.diffractionmode,widget_s.diffractionmode 
		data.detsegm = 1
		WIDGET_CONTROL, SET_VALUE=string(data.detsegm,format="(I3)"),widget_s.detsegm
		WIDGET_CONTROL, widget_s.gosector,sensitive=0
		WIDGET_CONTROL, widget_s.clearsector,sensitive=0
		WIDGET_CONTROL, widget_s.aprad,sensitive=0

; and erase the detector display
		wset,widget_s.detdrawID 
		erase
		empty
	  endcase

  'LOADBFDFFILE': begin
; loading a new file means that a bunch of variables need to be reset
; ask the user to select an input file
		if (XRegistered("STEMCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.cbedbase, /DESTROY
		if (XRegistered("STEMImageWidget") NE 0) then WIDGET_CONTROL, widget_s.imagebase, /DESTROY
		if (XRegistered("STEMCTEMBFDFWidget") NE 0) then WIDGET_CONTROL, widget_s.CTEMBFDFbase, /DESTROY

		STEMgetfilename
		data.progmode = 'BFDF'

; read the data file and populate all the relevant fields
		STEMreadgeometry

; reset the diffraction mode
		data.diffractionmode = 0
		WIDGET_CONTROL,SET_VALUE=data.diffractionmode,widget_s.diffractionmode 
		data.detsegm = 1
		WIDGET_CONTROL, SET_VALUE=string(data.detsegm,format="(I3)"),widget_s.detsegm
		WIDGET_CONTROL, widget_s.gosector,sensitive=0
		WIDGET_CONTROL, widget_s.clearsector,sensitive=0
		WIDGET_CONTROL, widget_s.aprad,sensitive=0

; and erase the detector display
		wset,widget_s.detdrawID 
		erase
		empty
	  endcase

 'QUIT': begin
; do a general cleanup
		  STEMprint,'Shutting down program',/blank
		if (XRegistered("STEMCBEDWidget") NE 0) then WIDGET_CONTROL, widget_s.cbedbase, /DESTROY
		if (XRegistered("STEMImageWidget") NE 0) then WIDGET_CONTROL, widget_s.imagebase, /DESTROY
		if (XRegistered("STEMCTEMBFDFWidget") NE 0) then WIDGET_CONTROL, widget_s.CTEMBFDFbase, /DESTROY
; write the preferences file
		STEMwritepreferences

; close the log file if it is open
		if (data.logfileopen eq 1) then begin
		  close,data.logunit
		endif
; wait briefly
		wait,2.0
; and finally kill the base widget
		WIDGET_CONTROL, widget_s.base, /DESTROY
		!EXCEPT=1
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
