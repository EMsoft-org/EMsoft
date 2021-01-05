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
; EMsoft:EBSDshowPattern.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDshowPattern.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main routine for display of EBSD patterns
;
;> @date 05/22/14 MDG 1.0 first version
;> @date 02/06/15 MDG 1.1 added pattern orientation parameters
;--------------------------------------------------------------------------
pro EBSDshowPattern, single=single, nodisplay=nodisplay

; the keyword /single indicates that only one pattern is available 

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common EBSDpatterns, pattern, image, finalpattern
common EBSDmasks, circularmask
common counter, patterncount

; check whether the mask needs to be recomputed or not
s = size(circularmask)
dbin = 2^SEMdata.detbinning
sm = min( [SEMdata.detnumsx/dbin, SEMdata.detnumsy/dbin] )
if (s[0] ne sm) then begin
  d = shift(dist(sm),sm/2,sm/2)
  d[where(d le sm/2)] = 1.0
  d[where(d gt sm/2)] = 0.0
  circularmask = fltarr(SEMdata.detnumsx/dbin, SEMdata.detnumsy/dbin)
  if (sm eq SEMdata.detnumsx/dbin) then begin
    dm = (SEMdata.detnumsy/dbin - sm)/2
    circularmask[0,dm] = d
  end else begin
    dm = (SEMdata.detnumsx/dbin - sm)/2
    circularmask[dm,0] = d
  end
endif

if not keyword_set(nodisplay) then begin
  wset,SEMwidget_s.PatternDrawID
  erase
  empty
end

sz = size(pattern)
if (sz[0] eq 3) then begin
  thispattern = reform(pattern[*,*,SEMdata.currentpatternID])
end else begin
  thispattern = pattern
endelse


; set the min and max fields
SEMdata.Patternmin = min(thispattern)
SEMdata.Patternmax = max(thispattern)

; temporary code to write the pattern to a file
;patterncount+=1
;filename = '/Users/mdg/Files/EMPlay/MCStuart/pattern10_'+string(patterncount,format="(I2.2)")+'.data'
;cmd = 'touch '+filename
;spawn,cmd
;openu,25,filename,/f77
;writeu,25,thispattern
;close,25


WIDGET_CONTROL, set_value=string(SEMdata.Patternmin,format="(F14.2)"), SEMwidget_s.Patternmin
WIDGET_CONTROL, set_value=string(SEMdata.Patternmax,format="(F14.2)"), SEMwidget_s.Patternmax

; display the pattern
; first apply the necessary intensity scaling to the current pattern

; what kind of intensity scaling do we need?
  if (SEMdata.PatternScaling eq 0) then begin  ; this is regular linear scaling
    finalpattern = bytscl(thispattern,min=SEMdata.Patternmin,max=SEMdata.Patternmax)
  end else begin
    finalpattern = bytscl(thispattern^SEMdata.gammavalue)
  end

; then we apply the pattern origin 
;    vals = ['Upper Left','Lower Left','Upper Right','Lower Right']
  if (SEMdata.PatternOrigin ne 0) then begin
    if (SEMdata.PatternOrigin eq 1) then finalpattern = reverse(finalpattern,2)
    if (SEMdata.PatternOrigin eq 2) then finalpattern = reverse(finalpattern,1)
    if (SEMdata.PatternOrigin eq 3) then finalpattern = reverse(reverse(finalpattern,2),1)
  endif

; finally, we use the binning factor
  if (SEMdata.detbinning ne 0) then finalpattern = congrid(finalpattern,SEMdata.detnumsx/2^SEMdata.detbinning,SEMdata.detnumsy/2^SEMdata.detbinning)

; and we display the result
if not keyword_set(nodisplay) then if (SEMdata.showcircularmask eq 1) then tv,finalpattern*byte(circularmask) else tv,finalpattern

end
