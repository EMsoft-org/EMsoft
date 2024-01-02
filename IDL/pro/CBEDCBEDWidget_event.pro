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
; CTEMsoft2013:CBEDCBEDWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDCBEDWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for LACBED mode
;
;> @date 10/15/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro CBEDCBEDWidget_event, event

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
if (event.id eq widget_s.CBEDbase) then begin
  data.CBEDxlocation = event.x
  data.CBEDylocation = event.y-25
    CBEDprint,' Window moved to location ('+string(fix(data.CBEDxlocation),format="(I4)")+','+string(fix(data.CBEDylocation),format="(I4)")+')'
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

; IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

  CASE eventval OF

 'CAMLEN':  begin
		WIDGET_CONTROL, get_value=val,widget_s.camlenval
		data.camlen = float(val[0])
		if (data.camlen gt 5000) then begin
		  data.camlen = 5000.0
		end
		if (data.camlen lt 50.0) then begin
		  data.camlen = 50.0  
		end
		  CBEDprint,'Camera length set to '+string(float(val[0]),FORMAT="(F8.2)")
		WIDGET_CONTROL, set_value=string(data.camlen,format="(F8.2)"), widget_s.camlenval
	endcase

 'CONVANG':  begin
		WIDGET_CONTROL, get_value=val,widget_s.convangval
		data.thetau = float(val[0])
		if (data.thetau gt data.thetac) then begin
		  data.thetau = data.thetac
		end
		  CBEDprint,'Beam convergence angle set to '+string(float(val[0]),FORMAT="(F6.2)")
		WIDGET_CONTROL, set_value=string(data.thetau,format="(F6.2)"), widget_s.convangval
		CBEDcircles
	endcase

 'LAUEX':  begin
		WIDGET_CONTROL, get_value=val,widget_s.Lauex
; check to make sure that this value keeps the point inside the red circle
; if it does not, then put the point at the largest x coordinate that is allowed
; for the current y coordinate.
		maxrad = drad * (data.thetac - data.thetau)/data.thetac ;/ gxpos * data.thetac/40.0
	 	dx = float(val[0]) 
	 	dy = data.Lauey 
		s = sqrt(dx^2+dy^2)
		if (s lt maxrad) then begin
		  data.oldLauex = data.Lauex
		  data.Lauex= float(val[0])
		    CBEDprint,'Lauex center x-coordinate set to '+string(float(val[0]),FORMAT="(F8.2)")
		  WIDGET_CONTROL, set_value=string(data.Lauex,format="(F8.2)"), widget_s.Lauex
		end else begin
; determine the largest x coordinate allowed for the current y coordinate
		  mx = sqrt(maxrad^2-dy^2) 
		  if (dx lt 0.0) then mx = -mx
		  data.oldLauex = data.Lauex
		  data.Lauex= mx
		    CBEDprint,'Requested Laue x coordinate would bring center outside of red circle'
		    CBEDprint,'Lauex center x-coordinate limited to '+string(mx,FORMAT="(F8.2)")
		  WIDGET_CONTROL, set_value=string(data.Lauex,format="(F8.2)"), widget_s.Lauex
		end
		wset,data.diskdrawID
		CBEDupdateLaue
	endcase

 'LAUEY':  begin
		WIDGET_CONTROL, get_value=val,widget_s.Lauey
; check to make sure that this value keeps the point inside the red circle
; if it does not, then put the point at the largest y coordinate that is allowed
; for the current x coordinate.
		maxrad = drad * (data.thetac - data.thetau)/data.thetac / gxpos
	 	dx = data.Lauex 
	 	dy = float(val[0]) 
		s = sqrt(dx^2+dy^2)
		if (s lt maxrad) then begin
		  data.oldLauey = data.Lauey
		  data.Lauey= float(val[0])
		    CBEDprint,'Lauey center y-coordinate set to '+string(float(val[0]),FORMAT="(F8.2)")
		  WIDGET_CONTROL, set_value=string(data.Lauey,format="(F8.2)"), widget_s.Lauey
		end else begin
; determine the largest y coordinate allowed for the current x coordinate
		  my = sqrt(maxrad^2-dx^2) 
		  if (dy lt 0.0) then my = -my
		  data.oldLauey = data.Lauey
		  data.Lauey= my
		    CBEDprint,'Requested Laue y coordinate would bring center outside of red circle'
		    CBEDprint,'Lauey center y-coordinate limited to '+string(my,FORMAT="(F8.2)")
		  WIDGET_CONTROL, set_value=string(data.Lauey,format="(F8.2)"), widget_s.Lauey
		end
		wset,data.diskdrawID
		CBEDupdateLaue
	endcase

 'LOGOFFSET':  begin
		WIDGET_CONTROL, get_value=val,widget_s.logoffset
		data.logoffset= float(val[0])
		  CBEDprint,'Logarithm offset value set to '+string(float(val[0]),FORMAT="(E9.2)")
		WIDGET_CONTROL, set_value=string(data.logoffset,format="(E9.2)"), widget_s.logoffset
	endcase

 'CBEDCBEDTHICKLIST': begin
	data.thicksel = event.index
	data.thickness = data.startthick + float(event.index) * data.thickinc
	  CBEDprint,'Foil thickness set to '+string(data.thickness,FORMAT="(F6.1)")+' nm'
	CBEDcircles
	endcase

 'JUMPLIST': begin
	data.jumpsel = event.index
	  CBEDprint,'Number of tracking steps set to '+string(data.jumpsel+2,FORMAT="(I2)")
	endcase

 'SELECTLAUE':  begin
	  if (event.type eq 0) then begin
  	    maxrad = drad * (data.thetac - data.thetau)/data.thetac 
	    newx = event.x-midx
	    newy = event.y-midy
	    m = sqrt(newx^2+newy^2)
	    if (m gt maxrad) then begin
	      newx = newx * maxrad/m
	      newy = newy * maxrad/m
	    end
	    data.oldLauex = data.Lauex
	    data.oldLauey = data.Lauey
	    data.Lauex = newx/gxpos * 40.0/data.thetac
	    data.Lauey = newy/gxpos * 40.0/data.thetac
	      CBEDprint,'Lauex center x-coordinate set to '+string(newx,FORMAT="(F8.2)")
	      CBEDprint,'Lauex center x-coordinate set to '+string(newx,FORMAT="(F8.2)")
	    WIDGET_CONTROL, set_value=string(data.Lauex,format="(F8.2)"), widget_s.Lauex
	    WIDGET_CONTROL, set_value=string(data.Lauey,format="(F8.2)"), widget_s.Lauey
	    CBEDmoveLaue
	  end 
	endcase


 'GOCBED':  begin
; display the CBED pattern
		CBEDmoveLaue
	endcase

  'MAKENML': begin
; write a namelist file that can be read by the CTEMmbcbed program.  The default name
; for this file will be CTEMmbcbed.nml
		openw,1,data.pathname+'/'+'CTEMmbcbed.nml'
		printf,1,'&MBCBEDlist'
		 printf,1,'! do not change the first line in this file !'
		 printf,1,'!--------------'
		 printf,1,'! the name of the crystal input file MUST be specified (full or relative pathname)'
		printf,1,'xtalname = '''+data.xtalname+''''
		 printf,1,'! if output should go to a file, then insert stdout = some number > 10; comment for screen output'
		printf,1,'stdout = 6'
		 printf,1,'! beam accelerating voltage [V]'
		printf,1,'voltage = '+string(data.voltage,format="(F10.1)")
		 printf,1,'! camera length [mm]'
		printf,1,'camlen = '+string(data.camlen,format="(F8.2)")
		 printf,1,'! incident beam direction (zone axis)'
		v = string(data.wavek[0],format="(I3)")+','+string(data.wavek[1],format="(I3)")+','+string(data.wavek[2],format="(I3)")
		printf,1,'k = '+v
		 printf,1,'! foil normal indices'
		v = string(data.fn[0],format="(I3)")+','+string(data.fn[1],format="(I3)")+','+string(data.fn[2],format="(I3)")
		printf,1,'fn = '+v
		 printf,1,'! coordinates of the Laue center in units of the length of g_a'
		v = string(data.Lauex,format="(F6.2)")+','+string(data.Lauey,format="(F6.2)")
		printf,1,'klaue = '+v
		 printf,1,'! number of edge pixels of the square output CBED pattern'
		printf,1,'npix = 1025'
		 printf,1,'! minimum d-spacing in the computation '
		printf,1,'dmin = '+string(data.dmin,format="(F6.3)")
		 printf,1,'! beam convergence angle [mrad]'
		printf,1,'convergence = '+string(data.thetau,format="(F5.2)")
		 printf,1,'! starting value for thickness series [nm]'
		printf,1,'startthick = '+string(data.startthick,format="(F6.1)")
		 printf,1,'! thickness increment [nm]'
		printf,1,'thickinc = '+string(data.thickinc,format="(F6.1)")
		 printf,1,'! number of increments'
		printf,1,'numthick = '+string(data.numt,format="(I4)")
		 printf,1,'! file name of output file'
		printf,1,'outname = ''/somewhere/output.data'''
		 printf,1,'! this last line must be present!'
		printf,1,'/'
		close,1
		CBEDprint,'The following namelist file has been created: '+data.pathname+'/'+'CTEMmbcbed.nml',/blank
		CBEDprint,'Please edit this file and modify any values as needed; the current values are the ones'
		CBEDprint,'corresponding to the current settings on the CBED Widget window.'
	endcase

  'CLOSECBED': begin
; kill the base widget
	WIDGET_CONTROL, widget_s.CBEDDrawbase, /DESTROY
	WIDGET_CONTROL, widget_s.CBEDbase, /DESTROY
	  CBEDprint,'CBED Widget closed'
	  CBEDprint,'CBED Draw Widget closed'
	endcase

  else: MESSAGE, "Event User Value Not Found"

  END

endelse


end
