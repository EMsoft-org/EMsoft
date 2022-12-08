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
; EMsoft:Core_histnd.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_histnd.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief n-dimensional histogram computation
;
;> @date 03/19/14 MDG 1.0 first version
;> @date 10/20/15 MDG 1.1 integration with Efit program
;--------------------------------------------------------------------------
function Core_histnd,V,bs,normalize=normalize

  s=size(V,/DIMENSIONS)
  if n_elements(s) ne 2 then message,'Input must be P (points) x N (dimensions)'

  q = 0
  while (ishft(bs,-q) ne 1) do q+=1

  nbins=long(ishft(256,-q)) 
  total_bins= nbins^s[0]

  h=long(ishft(V[s[0]-1,*],-q))
  for i=s[0]-2,0,-1 do h = ishft(h,8-q) + long(ishft(V[i,*],-q))

  ret=make_array(TYPE=3,DIMENSION=replicate(nbins,s[0]),/NOZERO)
  ret[0]=histogram(h,MIN=0L,MAX=total_bins-1L)

  if keyword_set(normalize) then ret = ret/total(ret)
  
  return,ret
end
