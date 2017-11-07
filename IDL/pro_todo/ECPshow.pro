;
; Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
; CTEMsoft2013:ECPshow.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECPshow.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler
;
;> @date 06/13/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECPshow, dummy

;------------------------------------------------------------
; common blocks
common ECP_widget_common, widget_s
common ECP_data_common, data
common ECP_rawdata, rawdata

wset,widget_s.ECPdrawID

if (data.blur eq 0.0) then begin
  image = reform(rawdata[*,*,data.thicksel])
end else begin
  dim = round(6.0*data.blur)
; make sure filter has odd size and is at least 3 pixels
  if (dim mod 2 eq 0) then dim=dim+1
  if (dim lt 3) then dim=3
  d2 = (dim-1)/2

; some useful auxiliary variables
  kernel = fltarr(dim,dim)
  line = findgen(dim)-float(dim/2)
  x = line#replicate(1.0,dim)
  y = rotate(x,3)
  r = x^2+y^2
  kernel = exp(-r*0.5/data.blur^2)/(2.0*!pi*data.blur^2)
  image = convol(reform(rawdata[*,*,data.thicksel]),kernel,/edge_truncate)
endelse

; do we need to draw a grid ?
if (data.ecpgrid eq 1) then begin
  tvscl,image
; draw the grid lines horizontally and vertically
  for i=-data.kt,data.kt do begin
    plots,data.xmid + [i,i]*data.dgrid,[0,data.datadims[1]],/dev,color=200
    plots,[0,data.datadims[1]],data.xmid+[i,i]*data.dgrid,/dev,color=200
  end
end else begin
  tvscl,rot(image,data.patrot)
endelse

end 
