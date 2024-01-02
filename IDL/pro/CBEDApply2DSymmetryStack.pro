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
; CTEMsoft2013:CBEDApply2DSymmetryStack.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDApply2DSymmetryStack.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief apply all the symmetry operators for a given 2D point group to a stack of disks
;
;> @date 10/08/13 MDG 1.0 first attempt 
;> @date 10/15/13 MDG 1.1 correction for relative orientation of point group to disk
;--------------------------------------------------------------------------
function CBEDApply2DSymmetryStack,inp
;
; apply a series of basic 2D symmetry operations to the input array
;
common SYM2D, SYM_MATnum, SYM_direc
common CBED_data_common, data

; loop over all the symmetry operators
for i=1,SYM_MATnum-1 do begin
  slice = reform(inp[*,*,i])
  if (data.thetam ne 0.0) then slice = rot(slice,-data.thetam,cubic=-0.5)
  case SYM_direc[i] of 
    'rot1': slice = rotate(slice,1)
    'rot2': slice = rotate(slice,2)
    'rot3': slice = rotate(slice,3)
    'rot030': slice = rot(slice,-30.0,missing=0.0)
    'rot060': slice = rot(slice,-60.0,missing=0.0)
    'rot120': slice = rot(slice,-120.0,missing=0.0)
    'rot150': slice = rot(slice,-150.0,missing=0.0)
    'rot240': slice = rot(slice,-240.0,missing=0.0)
    'rot300': slice = rot(slice,-300.0,missing=0.0)
    'rev1': slice = reverse(slice,1)
    'rev2': slice = reverse(slice,2)
    'rev030': slice = rot(reverse(rot(slice,30.0,missing=0.0),2),-30.0,missing=0.0)
    'rev060': slice = rot(reverse(rot(slice,60.0,missing=0.0),2),-60.0,missing=0.0)
    'rev120': slice = rot(reverse(rot(slice,120.0,missing=0.0),2),-120.0,missing=0.0)
    'rev150': slice = rot(reverse(rot(slice,150.0,missing=0.0),2),-150.0,missing=0.0)
    'tra1': slice = transpose(slice)
    'tra2': slice = rotate(transpose(rotate(slice,1)),3)
    else: print,'unknown symmetry operation; continuing '+SYM_direc[i]
  endcase
  if (data.thetam ne 0.0) then slice = rot(slice,data.thetam,cubic=-0.5)
  inp[0,0,i] = slice 
endfor

return,inp
end

