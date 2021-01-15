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
; EMsoft:OMETinitStokesVector.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETinitStokesVector.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief returns a structure with all the fields necessary to describe a Stokes Vector
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
function OMETinitStokesVector, oenumber, SV, output=output, update=update

common OMET_data_common, OMETdata
common OMET_widget_common, OMETwidget_s
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then print,'entering OMETinitStokesVector'

if (keyword_set(update)) then begin
	SVstructure = (*optelemptr[oenumber])  
end else begin
	if (KEYWORD_SET(output)) then begin
		SVstructure = {SV2struct, $
					oetype:fix(6), $	; Stokes vector type number (output vector)
					S:dblarr(4), $ ; Mueller matrix for polarizer element
					widgetexists:fix(0), $
					widgetID:long(0), $
					creationorder:fix(oenumber), $
					TotalMuellerMatrix:dblarr(4,4), $
					oenum:fix(oenumber)}; sequential number of optical element in chain, also determines the x-coordinate
	end else begin
		SVstructure = {SVstruct, $
					oetype:fix(4), $	; Stokes vector type number
					S:dblarr(4), $ ; Mueller matrix for polarizer element
					widgetexists:fix(0), $
					widgetID:long(0), $
					creationorder:fix(oenumber), $
					oenum:fix(oenumber)}; sequential number of optical element in chain, also determines the x-coordinate
	endelse
	OMETdata.availableOEnumber += 1
	OMETdata.numleft -= 1
endelse

SVstructure.S = SV


return, SVstructure
end 
