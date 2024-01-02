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
; CTEMsoft2013:CBEDgenerate2Dsymmetry.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDgenerate2Dsymmetry.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief define the symmetry operators for a given 2D point group
;
;> @date 09/25/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDgenerate2Dsymmetry,isym
;
; this routine defines the symmetry operations for a given 2D point group;
; these operations are expressed as IDL intrinsics rather than actual
; transformation matrices because those will operate much faster on the images.
; These operators are implemented in CBEDApply2DSymmetry.pro
;
common SYM2D, SYM_MATnum, SYM_direc

SYM_direc = strarr(12)

; the identity is present for each group
SYM_MATnum = 1
SYM_direc[0] = 'iden'

case isym of
  1: begin  ; 1
; do nothing
     end
  2: begin  ; 2
	SYM_MATnum = 2
	SYM_direc[1] = 'rot2'
     end
  3: begin  ; m
	SYM_MATnum = 2
	SYM_direc[1] = 'rev2'
     end
  4: begin  ; 2mm
	SYM_MATnum = 4
	SYM_direc[1] = 'rot2'
	SYM_direc[2] = 'rev1'
	SYM_direc[3] = 'rev2'
     end
  5: begin  ; 4
	SYM_MATnum = 4
	SYM_direc[1] = 'rot1'
	SYM_direc[2] = 'rot2'
	SYM_direc[3] = 'rot3'
     end
  6: begin  ; 4mm
	SYM_MATnum = 8
	SYM_direc[1] = 'rot1'
	SYM_direc[2] = 'rot2'
	SYM_direc[3] = 'rot3'
	SYM_direc[4] = 'rev1'
	SYM_direc[5] = 'rev2'
	SYM_direc[6] = 'tra1'
	SYM_direc[7] = 'tra2'
     end
  7: begin  ; 3
	SYM_MATnum = 3
	SYM_direc[1] = 'rot120'
	SYM_direc[2] = 'rot240'
     end
  8: begin  ; 3m1
	SYM_MATnum = 6
	SYM_direc[1] = 'rot120'
	SYM_direc[2] = 'rot240'
	SYM_direc[3] = 'rev1'
	SYM_direc[4] = 'rev030'
	SYM_direc[5] = 'rev150'
     end
  9: begin  ; 6
	SYM_MATnum = 6
	SYM_direc[1] = 'rot060'
	SYM_direc[2] = 'rot120'
	SYM_direc[3] = 'rot2'
	SYM_direc[4] = 'rot240'
	SYM_direc[5] = 'rot300'
      end
  10: begin  ; 6mm
	SYM_MATnum = 12
	SYM_direc[1] = 'rot060'
	SYM_direc[2] = 'rot120'
	SYM_direc[3] = 'rot2'
	SYM_direc[4] = 'rot240'
	SYM_direc[5] = 'rot300'
	SYM_direc[6] = 'rev2'
	SYM_direc[7] = 'rev030'
	SYM_direc[8] = 'rev060'
	SYM_direc[9] = 'rev1'
	SYM_direc[10] = 'rev120'
	SYM_direc[11] = 'rev150'
      end
  11: begin  ; 31m
	SYM_MATnum = 6
	SYM_direc[1] = 'rot120'
	SYM_direc[2] = 'rot240'
	SYM_direc[3] = 'rev2'
	SYM_direc[4] = 'rev060'
	SYM_direc[5] = 'rev120'
     end
  else: CBEDprint,'2D symmetry group not implemented',/blank
endcase

end
