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
; EMsoft:EllipsometryTool_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EllipsometryTool_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Main event handler for EllipsometryTool routine
;
;> @date 02/15/17 MDG 1.0 initial version
;--------------------------------------------------------------------------
pro EllipsometryTool_event, event

;------------------------------------------------------------
; common blocks  (OMET = Optical Microscopy Ellipsometry Tool)
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then print,'entering EllipsometryTool_event'

; used for debugging purposes
if (OMETdata.eventverbose eq 1) then help,event,/structure

; there are only two different events: either the window has been moved or 
; some button or something has been activated inside the window
if (event.id eq OMETwidget_s.base) then begin
  OMETdata.xlocation = event.x
  OMETdata.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
    'CREATEPOLARIZER': begin  ; generate a new instantiation of a polarizer widget and insert it at the end of the optical chain
; first we need to update a few counters and initialize the data structure for this optical elements
      optelemptr[OMETgetnextfreepos()] = PTR_NEW(OMETinitpolarizer(OMETgetnextfreepos()) )
      OMETupdateOEwidgets
    end

    'CREATERETARDER': begin
      optelemptr[OMETgetnextfreepos()] = PTR_NEW(OMETinitretarder(OMETgetnextfreepos()) )
      OMETupdateOEwidgets
    end

    'CREATEROTATOR': begin
      optelemptr[OMETgetnextfreepos()] = PTR_NEW(OMETinitrotator(OMETgetnextfreepos()) )
      OMETupdateOEwidgets
    end

    'INPUTCHI': begin
          OMETdata.inputchi = Core_WidgetEvent( OMETwidget_s.inputchi, 'input ellipticity set to ', '(F10.6)', /flt)
          ; convert the chi-psi pair into alpha-delta and update the input widgets, the input Stokes vector,
          ; the entire pipeline and the outputdrawing
          MC_get_AD_from_EO, OMETdata.inputchi*!dtor, OMETdata.inputpsi*!dtor, alpha, delta
          if ((alpha lt 0.D0) or (alpha gt !dpi*0.5D0)) then alpha = (alpha + 2.D0*!dpi) mod (0.5D0*!dpi) 
          if ((delta lt 0.D0) or (delta gt !dpi*2.D0)) then delta = (delta + 12.D0*!dpi) mod (2.D0*!dpi) 
          OMETdata.inputalpha = alpha/!dtor
          OMETdata.inputdelta = delta/!dtor
          WIDGET_CONTROL, OMETwidget_s.inputalpha, SET_VALUE=string(OMETdata.inputalpha,format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputdelta, SET_VALUE=string(OMETdata.inputdelta,format="(F10.6)")
          Core_Print,'input auxiliary angle set to '+string(OMETdata.inputalpha,format="(F10.6)")
          Core_Print,'input phase angle set to '+string(OMETdata.inputdelta,format="(F10.6)")
          print, OMETdata.inputchi, OMETdata.inputpsi, OMETdata.inputalpha, OMETdata.inputdelta
          ; update the input Stokes vector
          optelemptr[0] = PTR_NEW(OMETinitStokesVector(0,[1.D0, cos(2.0*OMETdata.inputalpha*!dtor), sin(2.0*OMETdata.inputalpha*!dtor)*cos(OMETdata.inputdelta*!dtor), sin(2.0*OMETdata.inputalpha*!dtor)*sin(OMETdata.inputdelta*!dtor)],/update)) 
          WIDGET_CONTROL, OMETwidget_s.inputSV0, SET_VALUE=string((*optelemptr[0]).S[0],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV1, SET_VALUE=string((*optelemptr[0]).S[1],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV2, SET_VALUE=string((*optelemptr[0]).S[2],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV3, SET_VALUE=string((*optelemptr[0]).S[3],format="(F10.6)")
          ; and finally, update the whole optical chain and the drawing of the polarization ellipse
          OMETupdateStokesVector
    end

    'INPUTPSI': begin
          OMETdata.inputpsi = Core_WidgetEvent( OMETwidget_s.inputpsi, 'input orientation angle set to ', '(F10.6)', /flt)
          ; convert the chi-psi pair into alpha-delta and update the input widgets, the input Stokes vector,
          ; the entire pipeline and the outputdrawing
          MC_get_AD_from_EO, OMETdata.inputchi*!dtor, OMETdata.inputpsi*!dtor, alpha, delta
          if ((alpha lt 0.D0) or (alpha gt !dpi*0.5D0)) then alpha = (alpha + 2.D0*!dpi) mod (0.5D0*!dpi) 
          if ((delta lt 0.D0) or (delta gt !dpi*2.D0)) then delta = (delta + 12.D0*!dpi) mod (2.D0*!dpi) 
          OMETdata.inputalpha = alpha/!dtor
          OMETdata.inputdelta = delta/!dtor
          WIDGET_CONTROL, OMETwidget_s.inputalpha, SET_VALUE=string(OMETdata.inputalpha,format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputdelta, SET_VALUE=string(OMETdata.inputdelta,format="(F10.6)")
          Core_Print,'input auxiliary angle set to '+string(OMETdata.inputalpha,format="(F10.6)")
          Core_Print,'input phase angle set to '+string(OMETdata.inputdelta,format="(F10.6)")
          print, OMETdata.inputchi, OMETdata.inputpsi, OMETdata.inputalpha, OMETdata.inputdelta
          ; update the input Stokes vector
          optelemptr[0] = PTR_NEW(OMETinitStokesVector(0,[1.D0, cos(2.0*OMETdata.inputalpha*!dtor), sin(2.0*OMETdata.inputalpha*!dtor)*cos(OMETdata.inputdelta*!dtor), sin(2.0*OMETdata.inputalpha*!dtor)*sin(OMETdata.inputdelta*!dtor)],/update)) 
          WIDGET_CONTROL, OMETwidget_s.inputSV0, SET_VALUE=string((*optelemptr[0]).S[0],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV1, SET_VALUE=string((*optelemptr[0]).S[1],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV2, SET_VALUE=string((*optelemptr[0]).S[2],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV3, SET_VALUE=string((*optelemptr[0]).S[3],format="(F10.6)")
          ; and finally, update the whole optical chain and the drawing of the polarization ellipse
          OMETupdateStokesVector
    end

    'INPUTALPHA': begin
          OMETdata.inputalpha = Core_WidgetEvent( OMETwidget_s.inputalpha, 'input auxiliary angle set to ', '(F10.6)', /flt)
          ; convert the alpha-delta pair into psi-chi and update the input widgets, the input Stokes vector,
          ; the entire pipeline and the outputdrawing
          MC_get_EO_from_AD, OMETdata.inputalpha*!dtor, OMETdata.inputdelta*!dtor, chi, psi
          OMETdata.inputchi = chi/!dtor
          OMETdata.inputpsi = psi/!dtor
          WIDGET_CONTROL, OMETwidget_s.inputpsi, SET_VALUE=string(OMETdata.inputpsi,format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputchi, SET_VALUE=string(OMETdata.inputchi,format="(F10.6)")
          Core_Print,'input orientation angle set to '+string(OMETdata.inputpsi,format="(F10.6)")
          Core_Print,'input ellipticity angle set to '+string(OMETdata.inputchi,format="(F10.6)")
          print, OMETdata.inputchi, OMETdata.inputpsi, OMETdata.inputalpha, OMETdata.inputdelta
          ; update the input Stokes vector
          optelemptr[0] = PTR_NEW(OMETinitStokesVector(0,[1.D0, cos(2.0*OMETdata.inputchi*!dtor)*cos(2.0*OMETdata.inputpsi*!dtor), cos(2.0*OMETdata.inputchi*!dtor)*sin(2.0*OMETdata.inputpsi*!dtor),sin(2.0*OMETdata.inputchi*!dtor)],/update)) 
          WIDGET_CONTROL, OMETwidget_s.inputSV0, SET_VALUE=string((*optelemptr[0]).S[0],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV1, SET_VALUE=string((*optelemptr[0]).S[1],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV2, SET_VALUE=string((*optelemptr[0]).S[2],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV3, SET_VALUE=string((*optelemptr[0]).S[3],format="(F10.6)")
          ; and finally, update the whole optical chain and the drawing of the polarization ellipse
          OMETupdateStokesVector
    end

    'INPUTDELTA': begin
          OMETdata.inputdelta = Core_WidgetEvent( OMETwidget_s.inputdelta, 'input phase angle set to ', '(F10.6)', /flt)
          ; convert the alpha-delta pair into psi-chi and update the input widgets, the input Stokes vector,
          ; the entire pipeline and the outputdrawing
          MC_get_EO_from_AD, OMETdata.inputalpha*!dtor, OMETdata.inputdelta*!dtor, chi, psi
          OMETdata.inputchi = chi/!dtor
          OMETdata.inputpsi = psi/!dtor
          WIDGET_CONTROL, OMETwidget_s.inputpsi, SET_VALUE=string(OMETdata.inputpsi,format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputchi, SET_VALUE=string(OMETdata.inputchi,format="(F10.6)")
          Core_Print,'input orientation angle set to '+string(OMETdata.inputpsi,format="(F10.6)")
          Core_Print,'input ellipticity angle set to '+string(OMETdata.inputchi,format="(F10.6)")
          print, OMETdata.inputchi, OMETdata.inputpsi, OMETdata.inputalpha, OMETdata.inputdelta
          WIDGET_CONTROL, OMETwidget_s.inputSV0, SET_VALUE=string((*optelemptr[0]).S[0],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV1, SET_VALUE=string((*optelemptr[0]).S[1],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV2, SET_VALUE=string((*optelemptr[0]).S[2],format="(F10.6)")
          WIDGET_CONTROL, OMETwidget_s.inputSV3, SET_VALUE=string((*optelemptr[0]).S[3],format="(F10.6)")
          ; update the input Stokes vector
          optelemptr[0] = PTR_NEW(OMETinitStokesVector(0,[1.D0, cos(2.0*OMETdata.inputchi*!dtor)*cos(2.0*OMETdata.inputpsi*!dtor), cos(2.0*OMETdata.inputchi*!dtor)*sin(2.0*OMETdata.inputpsi*!dtor),sin(2.0*OMETdata.inputchi*!dtor)],/update)) 
          ; and finally, update the whole optical chain and the drawing of the polarization ellipse
          OMETupdateStokesVector
    end

 	  'QUIT': begin
		    ; OMETwritepreferences
; do a general cleanup of potentially open widgets
 		    Core_Print,'Quitting program',/blank
        for i=0,OMETgetnextfreepos()-1 do begin
          if (OMETwidget_s.chainIDs[i] ne 0L) then begin
            if (OMETdata.eventverbose eq 1) then print, ' destroying widget ID', OMETwidget_s.chainIDs[i]
            WIDGET_CONTROL, OMETwidget_s.chainIDs[i], /DESTROY
          endif
        endfor
        if (OMETdata.eventverbose eq 1) then print, ' destroying widget ID', OMETwidget_s.base
		    WIDGET_CONTROL, OMETwidget_s.base, /DESTROY
		    !EXCEPT=1  ; turn exceptions back on (turned off at start of widget)
        loadct,0
        device,decomposed=1
	  end

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
