;
; Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:OMET_drawPolarizationEllipse.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMET_drawPolarizationEllipse.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief draw the polarization ellipse and return a bunch of geometrical parameters
;
;> @date 02/27/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMET_drawPolarizationEllipse, SV, pcol, overlap = overlap

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr
common OMET_MM, chainMM

if (OMETdata.eventverbose eq 1) then print,'entering OMET_drawPolarizationEllipse'

wset, OMETwidget_s.PEdrawID
loadct,27,/silent

th = findgen(360)*!dtor
ct = cos(th)
st = sin(th)

w = 451
w2 = (w-1)/2

if (not keyword_set(overlap)) then begin
; plot,findgen(3),xrange=[-1.1,1.1],yrange=[-1.1,1.1],xstyle=7, ystyle=7, /nodata, thick=2, xthick=2, ythick=2, color = 2, charsize = 1.2
  erase
  plots,[w2,w2],[0,w-1],/dev,linestyle=2
  plots,[0,w-1],[w2,w2],/dev,linestyle=2
  plots,w2+w2*ct,w2+w2*st,/dev,linestyle=2
endif

print,'polarization = ',total(SV[1:3]^2)/SV[0]
loadct,27,/silent

if (abs(SV[3]) lt 1.e-5) then begin
  ; this is linearly polarized light
  psi = 0.5 * atan( SV[2], SV[1] )
  chi = 0.0
; draw just a straight line at angle psi
  r2 = w2+w2*[cos(psi), sin(psi)]
  r1 = w2-w2*[cos(psi), sin(psi)]
  plots,[r1[0],r2[0]],[r1[1],r2[1]],color=pcol,/dev, thick=2
; and finally, put the relevant parameters in an array to be returned to the calling program

end else begin
  ; this is circularly/elliptically polarized light
  A = 2.0*(SV[0]-SV[1])/SV[3]^2
  B = 2.0*(SV[0]+SV[1])/SV[3]^2
  C = 2.0*SV[2]/SV[3]^2
  print,'ellipse parameters A, B, C : ', A, B, C
  phi = th
  cp = cos(2.0*phi)
  sp = sin(2.0*phi)
  L = (A+B) - 2.0*C*sp + (A-B)*cp
  rho = sqrt(2.0/L)

  phimax = -0.5*atan(2.0*C , A-B)
  LA = (A+B) + sqrt(4.0*C^2+(A-B)^2)
  LB = (A+B) - sqrt(4.0*C^2+(A-B)^2)
  rhoA = sqrt(2.0/LA)
  rhoB = sqrt(2.0/LB)
  rm = [ [cos(phimax), -sin(phimax)], [sin(phimax), cos(phimax)] ]

print,phimax/!dtor, 4.0*C*cos(2.0*phimax)-2.0*(A-B)*sin(2.0*phimax), rhoA, rhoB

  rhox = w2+w2*rho * cos(phi)
  rhoy = w2+w2*rho * sin(phi)
  plots,rhox,rhoy,/dev,color=pcol,thick=2

  rx = max(rhox)
  ry = max(rhoy)
  mrx = 2*w2 - rx
  mry = 2*w2 - ry

  v1 = w2+w2*(rm ## [ rhoA, rhoB])
  v2 = w2+w2*(rm ## [ rhoA,-rhoB])
  v3 = w2+w2*(rm ## [-rhoA,-rhoB])
  v4 = w2+w2*(rm ## [-rhoA, rhoB])

  plots,[v1[0],v2[0]],[v1[1],v2[1]],/dev,color=pcol
  plots,[v2[0],v3[0]],[v2[1],v3[1]],/dev,color=pcol
  plots,[v3[0],v4[0]],[v3[1],v4[1]],/dev,color=pcol
  plots,[v4[0],v1[0]],[v4[1],v1[1]],/dev,color=pcol

  plots,[mrx, rx],[mry,mry],/dev,color=pcol
  plots,[mrx,mrx],[mry, ry],/dev,color=pcol
  plots,[mrx, rx],[ ry, ry],/dev,color=pcol
  plots,[ rx, rx],[mry, ry],/dev,color=pcol

; and finally, put the relevant parameters in an array to be returned to the calling program


endelse



end
