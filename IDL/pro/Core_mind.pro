;
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
; EMsoft:Core_mind.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_mind.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief special event handler for all the CW_BGROUP calls, since CW_BGROUP does not support event_pro
;
;> @date 03/19/14 MDG 1.0 first version
;--------------------------------------------------------------------------
function Core_mind,ndhist,level=level
;
; computes the mutual information for a set of n images
;
; input: the normalized n-dimensional histogram, as computed by new_hist_nd(array,binsize,/normalize)
;
; output: the higher dimensional mutual information
;
; The idea is that we compute the n-dimensional joint histogram only once,
; and then project it down into lower dimensions for the computation of 
; all the image entropies...
;
; This routine calls itself recursively with the lower dimensional joint histograms
; as input ...
;
; version 1, MDG/EIC, 3/17/10
;> @date 10/20/15 MDG 1.1 adapted for Efit.pro routine

if arg_present(level) then level += 1 else level = 0

; get the dimensions to determine the sign of this contribution
sz = size(ndhist,/dimension)
szsz = size(sz,/dimension)
if (szsz[0] mod 2 eq 0) then sign=-1 else sign=+1

; compute the entropy for this level of recursion
q = where(ndhist ne 0.0,cnt)
if (cnt ne 0) then H = -sign*total(ndhist[q] * alog(ndhist[q]))/factorial(level) else begin
	print,'Warning: All joint histogram entries are zero'
	return,-1
endelse

; now recursively call this routine for all the lower level projections
if (szsz[0] gt 1) then begin
  for i=1,szsz[0] do begin
    H += Core_mind(reform(total(ndhist,i)),level=level)
    level -= 1
  endfor
endif

return,H
end


