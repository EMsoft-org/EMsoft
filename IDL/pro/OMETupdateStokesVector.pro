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
; EMsoft:OMETupdateStokesVector.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETupdateStokesVector.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief updates the Stokes vector whenever a value or widget position has changed
;
;> @date 02/22/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETupdateStokesVector, dummy

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr
common OMET_MM, chainMM

if (OMETdata.eventverbose eq 1) then print,'entering OMETupdateStokesVector'

nextfree = OMETgetnextfreepos()

; check to make sure that the optical chain is a valid chain... when a widget is 
; added, it is placed past the output Stokes vector to force the user to reposition 
; it to the proper location.  Hence, we need to disable updating of the chain when
; the output Stokes vector is not the last element in the chain.

if ((*optelemptr[nextfree-1]).oetype ne 6) then begin
	Core_Print,'Invalid optical chain: '
	Core_Print,'   --> please move all optical elements to the left of the output Stokes vector'
end else begin
	; first we need to get the effective Mueller matrix for the complete optical chain
	; OMETupdateMuellerMatrix
	chainMM = diag_matrix(replicate(1.D0,4))
	for i=1,nextfree-2 do begin
		chainMM = chainMM##(*optelemptr[i]).MuellerMatrix
		print,'----- element ',i
		print,(*optelemptr[i]).MuellerMatrix
		print,'-----'
	endfor
	print,chainMM

	; then we apply this vector to the input Stokes vector
	(*optelemptr[nextfree-1]).S = chainMM##(*optelemptr[0]).S

	; finally, we update the output vector widget and call the polarization ellipse draw routine
	WIDGET_CONTROL, OMETwidget_s.outputSV0, SET_VALUE=string((*optelemptr[nextfree-1]).S[0],format="(F10.6)")
	WIDGET_CONTROL, OMETwidget_s.outputSV1, SET_VALUE=string((*optelemptr[nextfree-1]).S[1],format="(F10.6)")
	WIDGET_CONTROL, OMETwidget_s.outputSV2, SET_VALUE=string((*optelemptr[nextfree-1]).S[2],format="(F10.6)")
	WIDGET_CONTROL, OMETwidget_s.outputSV3, SET_VALUE=string((*optelemptr[nextfree-1]).S[3],format="(F10.6)")

; get the polarization ellipse angles
	OMETdata.outputchi = MC_get_EllipticityAngle((*optelemptr[nextfree-1])) / !dtor
	OMETdata.outputpsi = MC_get_OrientationAngle((*optelemptr[nextfree-1])) / !dtor
	OMETdata.outputalpha = MC_get_AuxiliaryAngle((*optelemptr[nextfree-1])) / !dtor
	OMETdata.outputdelta = MC_get_PhaseShiftAngle((*optelemptr[nextfree-1])) / !dtor

; and update the corresponding widgets
	WIDGET_CONTROL, OMETwidget_s.outputchi, SET_VALUE=string(OMETdata.outputchi,format="(F10.6)")
	WIDGET_CONTROL, OMETwidget_s.outputpsi, SET_VALUE=string(OMETdata.outputpsi,format="(F10.6)")
	WIDGET_CONTROL, OMETwidget_s.outputalpha, SET_VALUE=string(OMETdata.outputalpha,format="(F10.6)")
	WIDGET_CONTROL, OMETwidget_s.outputdelta, SET_VALUE=string(OMETdata.outputdelta,format="(F10.6)")

  print, OMETdata.outputchi, OMETdata.outputpsi, OMETdata.outputalpha, OMETdata.outputdelta

; draw the polarization ellipse for input and output Stokes vectors and update a bunch of informational parameters
	OMET_drawPolarizationEllipse, (*optelemptr[nextfree-1]).S, 150
	OMET_drawPolarizationEllipse, (*optelemptr[0]).S, 60, /overlap

endelse


end
