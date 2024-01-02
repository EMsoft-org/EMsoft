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
; CTEMsoft2013:CBEDApply2DSymmetryPoint.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDApply2DSymmetryPoint.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief apply all the symmetry operators for a given 2D point group to a point
;
;> @date 10/08/13 MDG 1.0 first attempt 
;> @date 10/15/13 MDG 1.1 correction for relative orientation of point group to disk
;--------------------------------------------------------------------------
function CBEDApply2DSymmetryPoint,rinp,inverse=inverse
;
; apply a series of basic 2D symmetry operations to the input array
;
common SYM2D, SYM_MATnum, SYM_direc
common CBED_data_common, data
common trafos, done, c030, c060, c120, c150, c240, c300, s030, s060, s120, s150, s240, s300

; precompute the transformation cosines and sines 
if (done eq 0) then begin
  th = 30.0*!dtor & c030 = cos(th) & s030 = sin(th)
  th = 60.0*!dtor & c060 = cos(th) & s060 = sin(th)
  th =120.0*!dtor & c120 = cos(th) & s120 = sin(th)
  th =150.0*!dtor & c150 = cos(th) & s150 = sin(th)
  th =240.0*!dtor & c240 = cos(th) & s240 = sin(th)
  th =300.0*!dtor & c300 = cos(th) & s300 = sin(th)
  done = 1
endif

; the inverse keyword replaces each symmetry operator with its inverse...
; this is useful because for each diffraction disk, we need to go through
; the entire list of equivalent reflections, and when there is a Laue center
; shift, then the effective position of the mask is different for each of
; the reflections; to get the correct mask position, we then need the 
; inverse operations.

; create an output array
xyout = fltarr(2,SYM_MATnum)

; first of all, apply the identity operation
xyout[0:1,0] = rinp

; then loop over all the symmetry operators
if keyword_set(inverse) then begin ; all the mirror planes are their own inverse !
 if (data.thetam ne 0.0) then begin
  ct = cos(data.thetam*!dtor)
  st = sin(data.thetam*!dtor)
  inp = [ ct*rinp[0] + st*rinp[1], -st*rinp[0] + ct*rinp[1] ]
 end else begin
  inp = rinp
 end

 iloc = 1
 for i=1,SYM_MATnum-1 do begin
  case SYM_direc[i] of 
    'rot3': begin
	      xyout[0:1,iloc] = [-inp[1], inp[0] ] 
	      iloc += 1
	endcase
    'rot2': begin
	      xyout[0:1,iloc] = [-inp[0],-inp[1] ] 
	      iloc += 1
	endcase
    'rot1': begin
	      xyout[0:1,iloc] = [ inp[1],-inp[0] ] 
	      iloc += 1
	endcase
    'rot030': begin
	      xyout[0:1,iloc] = [ c030*inp[0] + s030*inp[1],-s030*inp[0] + c030*inp[1] ]
	      iloc += 1
	endcase
    'rot060': begin
	      xyout[0:1,iloc] = [ c060*inp[0] + s060*inp[1],-s060*inp[0] + c060*inp[1] ]
	      iloc += 1
	endcase
    'rot120': begin
	      xyout[0:1,iloc] = [ c120*inp[0] + s120*inp[1],-s120*inp[0] + c120*inp[1] ]
	      iloc += 1
	endcase
    'rot150': begin
	      xyout[0:1,iloc] = [ c150*inp[0] + s150*inp[1],-s150*inp[0] + c150*inp[1] ]
	      iloc += 1
	endcase
    'rot240': begin
	      xyout[0:1,iloc] = [ c240*inp[0] + s240*inp[1],-s240*inp[0] + c240*inp[1] ]
	      iloc += 1
	endcase
    'rot300': begin
	      xyout[0:1,iloc] = [ c300*inp[0] + s300*inp[1],-s300*inp[0] + c300*inp[1] ]
	      iloc += 1
	endcase
    'rev1': begin
	      xyout[0:1,iloc] = [-inp[0], inp[1] ] 
	      iloc += 1
	endcase
    'rev2': begin
	      xyout[0:1,iloc] = [ inp[0],-inp[1] ] 
	      iloc += 1
	endcase
    'rev030': begin
	      xyout[0:1,iloc] = [ c060*inp[0] + s060*inp[1], s060*inp[0]-c060*inp[1] ] 
	      iloc += 1
	endcase
    'rev060': begin
	      xyout[0:1,iloc] = [ c120*inp[0] + s120*inp[1], s120*inp[0]-c120*inp[1] ] 
	      iloc += 1
	endcase
    'rev120': begin
	      xyout[0:1,iloc] = [ c240*inp[0] + s240*inp[1], s240*inp[0]-c240*inp[1] ] 
	      iloc += 1
	endcase
    'rev150': begin
	      xyout[0:1,iloc] = [ c300*inp[0] + s300*inp[1], s300*inp[0]-c300*inp[1] ] 
	      iloc += 1
	endcase
    'tra1': begin
	      xyout[0:1,iloc] = [ inp[1], inp[0] ] 
	      iloc += 1
	endcase
    'tra2': begin
	      xyout[0:1,iloc] = [-inp[1],-inp[0] ] 
	      iloc += 1
	endcase
    else: print,'unknown symmetry operation; continuing '+SYM_direc[i]
  endcase
  if (data.thetam ne 0.0) then begin  ; rotate back to original orientation
    inp = xyout[0:1,iloc-1]
    xyout[0:1,iloc-1] = [ ct*inp[0] - st*inp[1],  st*inp[0] + ct*inp[1] ]
  endif
 endfor
end else begin
 if (data.thetam ne 0.0) then begin
  ct = cos(data.thetam*!dtor)
  st =-sin(data.thetam*!dtor)
  inp = [ ct*rinp[0] + st*rinp[1], -st*rinp[0] + ct*rinp[1] ]
 end else begin
  inp = rinp
 end

; then loop over all the symmetry operators
 iloc = 1
 for i=1,SYM_MATnum-1 do begin
  case SYM_direc[i] of 
    'rot1': begin
	      xyout[0:1,iloc] = [-inp[1], inp[0] ] 
	      iloc += 1
	endcase
    'rot2': begin
	      xyout[0:1,iloc] = [-inp[0],-inp[1] ] 
	      iloc += 1
	endcase
    'rot3': begin
	      xyout[0:1,iloc] = [ inp[1],-inp[0] ] 
	      iloc += 1
	endcase
    'rot030': begin
	      xyout[0:1,iloc] = [ c030*inp[0] - s030*inp[1], s030*inp[0] + c030*inp[1] ]
	      iloc += 1
	endcase
    'rot060': begin
	      xyout[0:1,iloc] = [ c060*inp[0] - s060*inp[1], s060*inp[0] + c060*inp[1] ]
	      iloc += 1
	endcase
    'rot120': begin
	      xyout[0:1,iloc] = [ c120*inp[0] - s120*inp[1], s120*inp[0] + c120*inp[1] ]
	      iloc += 1
	endcase
    'rot150': begin
	      xyout[0:1,iloc] = [ c150*inp[0] - s150*inp[1], s150*inp[0] + c150*inp[1] ]
	      iloc += 1
	endcase
    'rot240': begin
	      xyout[0:1,iloc] = [ c240*inp[0] - s240*inp[1], s240*inp[0] + c240*inp[1] ]
	      iloc += 1
	endcase
    'rot300': begin
	      xyout[0:1,iloc] = [ c300*inp[0] - s300*inp[1], s300*inp[0] + c300*inp[1] ]
	      iloc += 1
	endcase
    'rev1': begin
	      xyout[0:1,iloc] = [-inp[0], inp[1] ] 
	      iloc += 1
	endcase
    'rev2': begin
	      xyout[0:1,iloc] = [ inp[0],-inp[1] ] 
	      iloc += 1
	endcase
    'rev030': begin
	      xyout[0:1,iloc] = [ c060*inp[0] + s060*inp[1], s060*inp[0]-c060*inp[1] ] 
	      iloc += 1
	endcase
    'rev060': begin
	      xyout[0:1,iloc] = [ c120*inp[0] + s120*inp[1], s120*inp[0]-c120*inp[1] ] 
	      iloc += 1
	endcase
    'rev120': begin
	      xyout[0:1,iloc] = [ c240*inp[0] + s240*inp[1], s240*inp[0]-c240*inp[1] ] 
	      iloc += 1
	endcase
    'rev150': begin
	      xyout[0:1,iloc] = [ c300*inp[0] + s300*inp[1], s300*inp[0]-c300*inp[1] ] 
	      iloc += 1
	endcase
    'tra1': begin
	      xyout[0:1,iloc] = [ inp[1], inp[0] ] 
	      iloc += 1
	endcase
    'tra2': begin
	      xyout[0:1,iloc] = [-inp[1],-inp[0] ] 
	      iloc += 1
	endcase
    else: print,'unknown symmetry operation; continuing '+SYM_direc[i]
  endcase
  if (data.thetam ne 0.0) then begin  ; rotate back to original orientation
    inp = xyout[0:1,iloc-1]
    xyout[0:1,iloc-1] = [ ct*inp[0] - st*inp[1],  st*inp[0] + ct*inp[1] ]
  endif
 endfor
endelse

return,xyout
end
