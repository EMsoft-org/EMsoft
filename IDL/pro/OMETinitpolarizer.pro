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
; EMsoft:OMETinitpolarizer.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETinitpolarizer.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief returns a structure with all the fields necessary to describe a polarizer
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
function OMETinitpolarizer, oenumber, update=update

common OMET_data_common, OMETdata
common OMET_widget_common, OMETwidget_s
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then print,'entering OMETinitpolarizer'

if (keyword_set(update)) then begin
	polarizerstructure = (*optelemptr[oenumber])  
end else begin
	polarizerstructure = {polarizerstruct, $
						oetype:fix(1), $	; optical element type number
						px:float(1.0), $	; px attenuation coefficient
						py:float(0.0), $	; py attenuation coefficient
						pmag:float(0.0), $	; magnitude p of complex px+i py
						alpha:float(0.0), $	; phase angle of complex py+i py
						pmode:fix(0), $		; 0 for cartesian, 1 for polar
						theta:float(0.0), $ ; rotation angle for polarizer
						widgetexists:fix(0), $
						widgetID:long(0), $
						creationorder:fix(oenumber), $
						MuellerMatrix:dblarr(4,4), $ ; Mueller matrix for polarizer element
						oenum:fix(oenumber)}; sequential number of optical element in chain

	OMETdata.availableOEnumber += 1
	OMETdata.numleft -= 1

	WIDGET_CONTROL, OMETwidget_s.numleft, SET_VALUE=string(OMETdata.numleft,format="(I2)")
endelse

; generate the Mueller matrix for this element
if (polarizerstructure.pmode eq 0) then begin
  res = MC_get_diattenuator(polarizerstructure.px,polarizerstructure.py)
end else begin
  res = MC_get_diattenuator(polarizerstructure.px,polarizerstructure.py,polarizerstructure.pmode)
endelse

if (polarizerstructure.theta ne 0.0) then begin
	res = MC_rotate_MuellerMatrix(res, polarizerstructure.theta)
endif

polarizerstructure.MuellerMatrix = res.M

return, polarizerstructure
end 
