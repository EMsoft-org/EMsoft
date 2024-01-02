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
; CTEMsoft2013:CBEDApply2DOperator.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDApply2DOperator.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief apply a single symmetry operators from the current 2D point group
;
;> @date 10/08/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
function CBEDApply2DOperator,inp,i
;
; apply a series of basic 2D symmetry operations to the input array
;
common SYM2D, SYM_MATnum, SYM_direc

  case SYM_direc[i] of 
    'rot1': z = rotate(inp,1)
    'rot2': z = rotate(inp,2)
    'rot3': z = rotate(inp,3)
    'rot030': z = rot(inp,-30.0,cubic=-0.5,missing=0.0)
    'rot060': z = rot(inp,-60.0,cubic=-0.5,missing=0.0)
    'rot120': z = rot(inp,-120.0,cubic=-0.5,missing=0.0)
    'rot150': z = rot(inp,-150.0,cubic=-0.5,missing=0.0)
    'rot240': z = rot(inp,-240.0,cubic=-0.5,missing=0.0)
    'rot300': z = rot(inp,-300.0,cubic=-0.5,missing=0.0)
    'rev1': z = reverse(inp,1)
    'rev2': z = reverse(inp,2)
    'rev030': z = rot(reverse(rot(inp,30.0,cubic=-0.5,missing=0.0),2),-30.0,cubic=-0.5,missing=0.0)
    'rev060': z = rot(reverse(rot(inp,60.0,cubic=-0.5,missing=0.0),2),-60.0,cubic=-0.5,missing=0.0)
    'rev120': z = rot(reverse(rot(inp,120.0,cubic=-0.5,missing=0.0),2),-120.0,cubic=-0.5,missing=0.0)
    'rev150': z = rot(reverse(rot(inp,150.0,cubic=-0.5,missing=0.0),2),-150.0,cubic=-0.5,missing=0.0)
    'tra1': z = transpose(inp)
    'tra2': z = rotate(transpose(rotate(inp,1)),3)
    else: print,'unknown symmetry operation; continuing'
  endcase

return,z
end
