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
; CTEMsoft2013:CBEDApply2DSymmetry.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDApply2DSymmetry.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief apply all the symmetry operators for a given 2D point group
;
;> @date 10/08/13 MDG 1.0 first attempt 
;> @date 10/15/13 MDG 1.1 correction for relative orientation of point group to disk
;--------------------------------------------------------------------------
function CBEDApply2DSymmetry,inp,iorder
;
; apply a series of basic 2D symmetry operations to the input array
;
common SYM2D, SYM_MATnum, SYM_direc
common CBED_data_common, data


; first of all, apply the identity operation, corrected for the point group orientation if necessary
if (data.thetam ne 0.0) then begin
  zinp = rot(inp,-data.thetam,cubic=-0.5)
end else begin
  zinp = inp
end

z = zinp

; then loop over all the symmetry operators
for i=1,SYM_MATnum-1 do begin
  z += CBEDApply2DOperator(zinp,i)
endfor

; and rotate back to the original orientation if necessary
if (data.thetam ne 0.0) then begin
  z = rot(z,data.thetam,cubic=-0.5)
end 

; and return the array, properly normalized to avoid double counting
return,z
end
