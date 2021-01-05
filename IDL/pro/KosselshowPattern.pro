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
; EMsoft:KosselshowPattern.pro
;--------------------------------------------------------------------------
;
; PROGRAM: KosselshowPattern.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main routine for display of EBSD patterns
;
;> @date 11/09/15 MDG 1.0 modified from ECPshowPattern.pro
;--------------------------------------------------------------------------
pro KosselshowPattern, single=single, nodisplay=nodisplay, select=selection

; the keyword /single indicates that only one pattern is available 

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common EBSDpatterns, pattern, image, finalpattern
common EBSDmasks, circularmask
common Kosseldata, KosselPattern

; check whether the mask needs to be recomputed or not
s = size(circularmask)
sm = SEMdata.detnumsx
if (s[0] ne sm) then begin
  d = shift(dist(sm),sm/2,sm/2)
  d[where(d le sm/2)] = 1.0
  d[where(d gt sm/2)] = 0.0
  circularmask = fltarr(SEMdata.detnumsx, SEMdata.detnumsx)
  dm = (SEMdata.detnumsx - sm)/2
  circularmask[dm,0] = d
endif

if not keyword_set(nodisplay) then begin
  wset,SEMwidget_s.PatternDrawID
  erase
  empty
end

patterns = KosselPattern

sz = size(patterns)
if (sz[0] eq 3) then begin
  if (keyword_set(nodisplay)) then pattern = reform(patterns[*,*,selection]) else pattern = reform(patterns[*,*,SEMdata.currentpatternID]) 
end else pattern = patterns

; set the min and max fields
SEMdata.Patternmin = min(pattern)
SEMdata.Patternmax = max(pattern)

WIDGET_CONTROL, set_value=string(SEMdata.Patternmin,format="(F7.2)"), SEMwidget_s.Patternmin
WIDGET_CONTROL, set_value=string(SEMdata.Patternmax,format="(F7.2)"), SEMwidget_s.Patternmax

; display the pattern
; first apply the necessary intensity scaling to the current pattern

; what kind of intensity scaling do we need?
  if (SEMdata.PatternScaling eq 0) then begin  ; this is regular linear scaling
    finalpattern = bytscl(pattern,min=SEMdata.Patternmin,max=SEMdata.Patternmax)
  end else begin
    image = pattern^SEMdata.gammavalue
    finalpattern = bytscl(image)
  end

; then we apply the pattern origin 
;    vals = ['Upper Left','Lower Left','Upper Right','Lower Right']
  if (SEMdata.PatternOrigin ne 0) then begin
    if (SEMdata.PatternOrigin eq 1) then finalpattern = reverse(finalpattern,2)
    if (SEMdata.PatternOrigin eq 2) then finalpattern = reverse(finalpattern,1)
    if (SEMdata.PatternOrigin eq 3) then finalpattern = reverse(reverse(finalpattern,2),1)
  endif

; and we display the result
if not keyword_set(nodisplay) then if (SEMdata.showcircularmask eq 1) then tv,finalpattern*byte(circularmask) else tv,finalpattern

end
