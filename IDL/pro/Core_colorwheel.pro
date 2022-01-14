;
; Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:Core_colorwheel.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_colorwheel.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief generate color version of energy distribution with legend
;
;> @date 03/20/14 MDG 1.0 initial implementation 
;--------------------------------------------------------------------------
pro Core_colorwheel, bx, by, cimage, clegend 
;
; color wheel representation 
;
sz = size(bx,/dimensions)
dimx = sz[0]
dimy = sz[1]

mmax = max(sqrt(bx^2+by^2))
bx=bx/mmax
by=by/mmax
cmag = sqrt((bx^2+by^2))*255.0
cimage = intarr(3,dimx,dimy)
red = bytarr(dimx,dimy)
green = bytarr(dimx,dimy)
blue = bytarr(dimx,dimy)
cang = 255.0*bx/cmag
sang = sqrt(1.0-cang^2)
; first green component
q=where((bx lt 0.0) and (by ge 0.0),cnt)
if (cnt gt 0) then green[q]=byte(cmag[q]*abs(cang[q]))
q=where((bx ge 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then green[q]=byte(cmag[q]*abs(sang[q]))
q=where((bx lt 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then green[q]=byte(cmag[q])
; then red  
q=where((bx ge 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then red[q]=byte(cmag[q])
q=where((bx ge 0.0) and (by ge 0.0),cnt)
if (cnt gt 0) then red[q]=byte(cmag[q]*abs(cang[q]))
q=where((bx lt 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then red[q]=byte(cmag[q]*abs(sang[q]))
; finally blue
q=where(by ge 0.0,cnt)
if (cnt gt 0) then blue[q]=byte(cmag[q]*abs(sang[q]))
cimage(0,0:dimx-1,0:*)=red(0:*,0:*)
cimage(1,0:dimx-1,0:*)=green(0:*,0:*)
cimage(2,0:dimx-1,0:*)=blue(0:*,0:*)
;
; add color legend in the upper right corner
;
rd= 25
red = bytarr(2*rd,2*rd)
green = bytarr(2*rd,2*rd)
blue = bytarr(2*rd,2*rd)
line=findgen(2*rd)-rd
bx=fltarr(2*rd,2*rd)
for i=0,2*rd-1 do bx(0:*,i)=line(0:*)
by=fltarr(2*rd,2*rd)
for i=0,2*rd-1 do by(i,0:*)=line(0:*)
mmax = max([abs(bx),abs(by)])
bx=bx/mmax
by=by/mmax
tr = shift(dist(2*rd),rd,rd)
qtr = where(tr gt rd,qtrsize)
cmag = sqrt((bx^2+by^2))*215.0
cang = 215.0*bx/cmag
sang = sqrt(1.0-cang^2)
; first green component
q=where((bx lt 0.0) and (by ge 0.0),cnt)
if (cnt gt 0) then green[q]=byte(cmag[q]*abs(cang[q]))
q=where((bx ge 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then green[q]=byte(cmag[q]*abs(sang[q]))
q=where((bx lt 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then green[q]=byte(cmag[q])
; then red 
q=where((bx ge 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then red[q]=byte(cmag[q])
q=where((bx ge 0.0) and (by ge 0.0),cnt)
if (cnt gt 0) then red[q]=byte(cmag[q]*abs(cang[q]))
q=where((bx lt 0.0) and (by lt 0.0),cnt)
if (cnt gt 0) then red[q]=byte(cmag[q]*abs(sang[q]))
; finally blue
q=where(by ge 0.0,cnt)
if (cnt gt 0) then blue[q]=byte(cmag[q]*abs(sang[q]))
red(qtr)=0
green(qtr)=0
blue(qtr)=0
;
; remove the lower right quadrant
red[*,0:rd-1] = 0
green[*,0:rd-1] = 0
blue[*,0:rd-1] = 0
red = rotate(red,1)
green= rotate(green,1)
blue = rotate(blue,1)

clegend = bytarr(3,rd,2*rd)
clegend[0,0:*,0:*] = red[0:rd-1,0:*]
clegend[1,0:*,0:*] = green[0:rd-1,0:*]
clegend[2,0:*,0:*] = blue[0:rd-1,0:*]
;
end





