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
; EMsoft:Core_LambertS2C.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_LambertS2C.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief modified lambert to regular lambert conversion routine; inits the xcircle and ycircle arrays
;
;> @date 03/20/14 MDG 1.0 initial implementation 
;--------------------------------------------------------------------------
pro Core_LambertS2C,image,mc=mc,mp=mp

common projections, mcxcircle, mcycircle, mpxcircle, mpycircle, mcSPxcircle, mcSPycircle, mpSPxcircle, mpSPycircle 

; first the square to circle mapping using the bilinear function.
; we need two arrays that contain for each point in the circle map
; the equivalent point in the square map.
sz = size(image,/dimensions)
nx = sz[0]
ny = sz[1]
nx2 = (nx-1)/2
ny2 = (ny-1)/2

Rmax = float(nx2)^2
line = findgen(nx)-(nx-1)/2
xsquare = line # replicate(1,nx)
ysquare = replicate(1,ny) # line
xcircle = fltarr(nx,ny)
ycircle = fltarr(nx,ny)

sp2 = sqrt(!pi)/2.0
spi = 1.0/sp2
for i=0,nx-1 do begin
  A = float(i-nx2)
  for j=0,ny-1 do begin
    B = float(j-ny2)
    if (A^2+B^2 le Rmax) then begin
      if ( (0 le abs(B)) and (abs(B) le abs(A)) and (abs(A)+abs(B) ne 0) ) then begin
        xcircle[i,j] = (A/abs(A)) * sqrt(A^2+B^2) * sp2
        ycircle[i,j] = (A/abs(A)) * sqrt(A^2+B^2) * spi * atan(B/A)
      end

      if ( (0 le abs(A)) and (abs(A) le abs(B)) and (abs(A)+abs(B) ne 0) ) then begin
        xcircle[i,j] = (B/abs(B)) * sqrt(A^2+B^2) * spi * atan(A/B)
        ycircle[i,j] = (B/abs(B)) * sqrt(A^2+B^2) * sp2
      end

    endif
  endfor
endfor

xcircle *= spi
ycircle *= spi

xcircle += nx2
ycircle += ny2

if keyword_set(mc) then begin
	mcxcircle = xcircle
	mcycircle = ycircle
end
  
if keyword_set(mp) then begin
	mpxcircle = xcircle
	mpycircle = ycircle
end
  

end

