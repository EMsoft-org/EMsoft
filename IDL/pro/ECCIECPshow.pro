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
; CTEMsoft2013:ECCIECPshow.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIECPshow.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief show an ECP pattern
;
;> @date 12/06/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECCIECPshow, point=point

;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common ECCI_rawdata, indices, offsets, kperp, rawdata, ECCILUT
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata


ignore = -1
if (data.progmode eq 'trace') then begin
  dkt = 1.0
end else begin
  dkt = data.dkt
endelse

dkt = 1.0


ECPimage = bytarr(3,ECPdata.datadims[0],ECPdata.datadims[1])

; do all the drawing in the Z-buffer

set_plot,'Z'
device,set_resolution=[ECPdata.datadims[0],ECPdata.datadims[1]]
erase

pattern = bytscl(reform(ECPrawdata[*,*,ECPdata.thicksel]))

; blue channel
tvscl,pattern[0:*,0:*]

; check whether or not a point has been selected; if so, put a blue symbol at its position
if keyword_set(point) then begin
  bx = ECPdata.cx
  by = ECPdata.cy 
  z = sqrt( (reform(kperp[0,*])-bx)^2 + (reform(kperp[1,*])-by)^2 )
  q = where(z eq 0.0,cnt)
  if (cnt ne 0) then ignore = q[0]
  plots,ECPdata.xmid + bx*ECPdata.dgrid*dkt, ECPdata.xmid + by*ECPdata.dgrid*dkt,psym=2,/dev,color=255
  if (data.avrad ne 0.0) then begin
    rad = data.avrad*ECPdata.dgrid *data.dkt
    th = findgen(180)*2.0*!dtor
    ct = cos(th)
    st = sin(th)
    plots,ECPdata.xmid +  bx*ECPdata.dgrid*dkt + rad*ct, ECPdata.xmid +  by*ECPdata.dgrid*dkt + rad*st,/dev,color=255
  endif
endif

channel = tvrd()
ECPimage[2,0:*,0:*] = channel

; red channel
tvscl,pattern

; do we need to draw a grid ?
if (ECPdata.ecpgrid eq 1) then begin
; draw the grid lines horizontally and vertically
  for i=-ECPdata.kt,ECPdata.kt do begin
    plots,ECPdata.xmid + [i,i]*ECPdata.dgrid,[0,ECPdata.datadims[1]],/dev,color=255
    plots,[0,ECPdata.datadims[1]],ECPdata.xmid+[i,i]*ECPdata.dgrid,/dev,color=255
  end
end 

; check whether or not a point has been selected; if so, put a blue symbol at its position
if keyword_set(point) then begin
  bx = ECPdata.cx
  by = ECPdata.cy 
  plots,ECPdata.xmid + bx*ECPdata.dgrid*dkt, ECPdata.xmid + by*ECPdata.dgrid*dkt,psym=2,/dev,color=255
; do we need to superimpose an averaging circle ?
  if (data.avrad ne 0.0) then begin
    plots,ECPdata.xmid +  bx*ECPdata.dgrid*dkt + rad*ct, ECPdata.xmid +  by*ECPdata.dgrid*dkt + rad*st,/dev,color=255
  endif
endif

channel = tvrd()
ECPimage[0,0:*,0:*] = channel[0:*,0:*]

; and the green channel
erase
tvscl,pattern

for i=0,data.numk-1 do begin
    if (i ne ignore) then plots,ECPdata.xmid + kperp[0,i]*ECPdata.dgrid*dkt, ECPdata.xmid + kperp[1,i]*ECPdata.dgrid*dkt,psym=2,/dev,color=255
endfor

channel = tvrd()
ECPimage[1,0:*,0:*] = channel[0:*,0:*]

erase

; and finally display the color image
set_plot,'X'
wset,widget_s.ECPdrawID
tvscl,ECPimage,true=1


end 
