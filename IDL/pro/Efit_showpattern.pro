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
; EMsoft:Efit_showpattern.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_showpattern.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display a pattern (experimental or simulated or ... )
;
;> @date 10/15/15 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro Efit_showpattern,dummy

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern

common Efitdisplaycommon, mask, maskready, expvector
common inverseGaussian, inverseGaussianMask

; first we prepare the pattern (scaling, binning, flips, mask, ... 
if (maskready eq 0) then begin
 if (Efitdata.showcircularmask eq 1) then begin
  radius = min([Efitdata.detnumsx,Efitdata.detnumsy])
  d = dist(radius)
  d = shift(d,radius/2,radius/2)
  d[where (d le radius/2)] = 1.0
  d[where (d gt radius/2)] = 0.0
  mask = replicate(0.0,Efitdata.detnumsx,Efitdata.detnumsy)
  if (radius eq Efitdata.detnumsx) then begin
    i = (Efitdata.detnumsy - radius)/2
    mask[0,i] = d
  end else begin
    i = (Efitdata.detnumsx - radius)/2
    mask[i,0] = d
  endelse
 end else begin
  mask = 1.0
 endelse
 maskready = 1
endif

if (max(EBSDpattern) gt 0.0) then begin

; apply the correct intensity scaling
  Epat = EBSDpattern^Efitdata.detgamma

; try a high pass filter
  if (Efitdata.hipassonoff eq 1) then begin
    hipass = DIGITAL_FILTER(Efitdata.hipasscutoff,1.0,50,7)
    Epat = Convol(Epat,hipass)
  endif

; remove any ramp
  if (Efitdata.ramponoff eq 1) then begin
    Eav = mean(Epat)
    ramp = sfit(Epat,1)
    Epat = Epat/ramp
    Epat = Epat * Eav/mean(Epat)
  endif

  if (Efitdata.smoothval ne 0) then begin
    Epat = smooth(Epat,2*Efitdata.smoothval+1)
  endif
end else Epat = replicate(0.0,Efitdata.detnumsx,Efitdata.detnumsy)

if (Efitdata.inverseGaussian eq 1) then begin
  gmask = reform(inverseGaussianMask,Efitdata.detnumsx,Efitdata.detnumsy)
end else begin
  gmask = 1.0
end

case (Efitdata.displayoption) of 
        0 : begin       ; experimental pattern only
          mi = min(expEBSDpattern,max=ma)
          WIDGET_CONTROL, set_value=string(mi,format="(F9.2)"), Efitwidget_s.min
          WIDGET_CONTROL, set_value=string(ma,format="(F9.2)"), Efitwidget_s.max
          tvscl,expEBSDpattern * mask * gmask
          Efit_drawPC
        endcase

        1 : begin       ; simulated pattern only
          mi = min(Epat,max=ma)
          WIDGET_CONTROL, set_value=string(mi,format="(F9.2)"), Efitwidget_s.min
          WIDGET_CONTROL, set_value=string(ma,format="(F9.2)"), Efitwidget_s.max
          tvscl,Epat * mask * gmask
          Efit_drawPC
        endcase

        2 : begin       ; difference pattern
          z = abs(float(bytscl(expEBSDpattern)) - float(bytscl(Epat))) 
          mi = min(z,max=ma)
          WIDGET_CONTROL, set_value=string(mi,format="(F9.2)"), Efitwidget_s.min
          WIDGET_CONTROL, set_value=string(ma,format="(F9.2)"), Efitwidget_s.max
          tvscl,z * mask * gmask
          Efit_drawPC
        endcase

        3 : begin       ; overlap pattern 
          z = float(bytscl(expEBSDpattern)) + float(bytscl(Epat))
          mi = min(z,max=ma)
          WIDGET_CONTROL, set_value=string(mi,format="(F9.2)"), Efitwidget_s.min
          WIDGET_CONTROL, set_value=string(ma,format="(F9.2)"), Efitwidget_s.max
          tvscl,z * mask * gmask
          Efit_drawPC
        endcase

        4 : begin       ; overlap pattern RGB 
          c = bytarr(3,Efitdata.detnumsx,Efitdata.detnumsy)
          c[0,0:*,0:*] = bytscl(expEBSDpattern*mask * gmask)
          c[1,0:*,0:*] = bytscl(Epat*mask * gmask)
          c[2,0:*,0:*] = bytscl(Epat*mask * gmask)
          WIDGET_CONTROL, set_value='---', Efitwidget_s.min
          WIDGET_CONTROL, set_value='---', Efitwidget_s.max
          tvscl,c,true=1
          Efit_drawPC
        endcase

        5 : begin       ; flicker patterns
          z1 = bytscl(expEBSDpattern * mask * gmask)
          z2 = bytscl(Epat * mask * gmask)
          WIDGET_CONTROL, set_value='---', Efitwidget_s.min
          WIDGET_CONTROL, set_value='---', Efitwidget_s.max
          for i=0,4 do begin
            tv,z1
            Efit_drawPC
            wait,0.25
            tv,z2
            Efit_drawPC
            wait,0.25
          endfor
        endcase

else: MESSAGE, "Efit_showpattern: unknown display option"

endcase


end
