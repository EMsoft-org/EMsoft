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
; EMsoft:EBSDshowMC.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDshowMC.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display MC or MP using either modified Lambert or regular Lambert on a circle
;
;> @note Note that the circular Lambert projections are all precomputed at the time 
;> the data is read from the file...
;
;> @date 03/20/14 MDG 1.0 first version
;--------------------------------------------------------------------------
pro EBSDshowMC, dummy

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common Image_common, MCimage, MPimage
common EBSD_rawdata, accum_e, accum_z, mLPNH, mLPSH
common projections, mcxcircle, mcycircle, mpxcircle, mpycircle, mcSPxcircle, mcSPycircle, mpSPxcircle, mpSPycircle 


if (SEMdata.mpfiletype eq 3) then goto,skipMC

wset,SEMwidget_s.MCdrawID

if (SEMdata.mpfiletype eq 1) then begin
  energy = SEMdata.mcenergymin + SEMdata.Esel * SEMdata.mcenergybinsize
  if (SEMdata.MCLSum eq 0) then begin
    image = reform(accum_e[SEMdata.Esel,*,*])
    WIDGET_CONTROL, set_value=string(energy,format="(F5.2)"), SEMwidget_s.MCenergyval
  end 
end 
if (SEMdata.mpfiletype eq 2) then begin
  angle = SEMdata.mcsigstart + SEMdata.Esel * SEMdata.mcsigstep
  if (SEMdata.MCLSum eq 0) then begin
    image = reform(accum_e[SEMdata.Esel,*,*])
    WIDGET_CONTROL, set_value=string(angle,format="(F5.2)"), SEMwidget_s.MCenergyval
  end 
end

if (SEMdata.MCLSum eq 1) then begin
  image = reform(total(accum_e,1))
  WIDGET_CONTROL, set_value='sum', SEMwidget_s.MCenergyval
end

if (SEMdata.MCLSum eq 2) then begin   ; RGB display, needs some data preparation first
  numEbins = SEMdata.mcenergynumbin
  energy = SEMdata.mcenergymin + findgen(SEMdata.mcenergynumbin) * SEMdata.mcenergybinsize
  sz = size(accum_e,/dimensions)
  dims2 = sz[1]
  dims3 = sz[2]
  ener = fltarr(dims2,dims3)
  accum_t = float(total(accum_e,1))
  accum_t[where(accum_t eq 0.0)] = 1.0
  for i=0,dims2-1 do for j=0,dims3-1 do ener[i,j] = total(accum_e[0:*,i,j]*energy[0:*])/accum_t[i,j]

  bx = fltarr(dims2,dims3)
  by = fltarr(dims2,dims3)

; scale the energy as an angle between 0 and 270 counterclockwise
  minE = SEMdata.mcenergymin
  maxE = SEMdata.mcenergymax
  e = ener
  ener = (ener-minE)/(maxE-minE)
  ener = ener*180. - 180.
  ener *= !dtor
  c = cos(ener)
  s = -sin(ener)

  accum_t /= max(accum_t)

  bx = accum_t*c
  by = accum_t*s

  Core_colorwheel,bx,by,cimage,clegend

  image = cimage
  WIDGET_CONTROL, set_value='sumRGB', SEMwidget_s.MCenergyval
end

if ((SEMdata.MCLSmode eq 1) and (SEMdata.MCLSum ne 2)) then begin
  image = bilinear(image,mcxcircle,mcycircle,missing = 0)
end 

if ((SEMdata.MCLSmode eq 2) and (SEMdata.MCLSum ne 2)) then begin
  image = bilinear(image,mcSPxcircle,mcSPycircle,missing = 0)
end 

if ((SEMdata.MCLSmode eq 1) and (SEMdata.MCLSum eq 2)) then begin
  for i=0,2 do begin
    image = reform(cimage[i,*,*])
    image = bilinear(image,mcxcircle,mcycircle,missing = 0)
    cimage[i,0:*,0:*] = image[0:*,0:*]
  endfor
end 

if (SEMdata.MCLSum ne 2) then begin
  tvscl, image 
  WIDGET_CONTROL, set_value=string(min(image),format="(F9.1)"), SEMwidget_s.MCmin
  WIDGET_CONTROL, set_value=string(max(image),format="(F9.1)"), SEMwidget_s.MCmax
  MCimage = image
end else begin
  tvscl, cimage, true=1
; and add the color legend
  tvscl, clegend, dims2-25, dims3-50, true=1
  WIDGET_CONTROL, set_value='', SEMwidget_s.MCmin
  WIDGET_CONTROL, set_value='', SEMwidget_s.MCmax
  MCimage = cimage
  MCimage[0:2,dims2-25:*,dims3-50:*] = clegend[0:*,0:*,0:*]
end

skipMC:

if (SEMdata.mpfiletype lt 3) then begin
; should we also display the Master Pattern ?
 if (SEMdata.MCMPboth eq 1) then begin

  if (SEMdata.EBSDorECP eq 0) then begin
    if (SEMdata.NHSH eq 0) then begin
      if (SEMdata.numset gt 1) then MParraysum = total(mLPNH,4) else MParraysum = mLPNH
    end else begin
      if (SEMdata.numset gt 1) then MParraysum = total(mLPSH,4) else MParraysum = mLPSH
    endelse
  end else begin
    if (SEMdata.NHSH eq 0) then begin
      if (SEMdata.numset gt 1) then MParraysum = total(mLPNH,3) else MParraysum = mLPNH
    end else begin
      if (SEMdata.numset gt 1) then MParraysum = total(mLPSH,3) else MParraysum = mLPSH
    endelse
  endelse

  wset,SEMwidget_s.MPdrawID
  if (SEMdata.MCLSum eq 0) then begin
    if (SEMdata.EBSDorECP eq 0) then begin
      if (SEMdata.Asymsel lt 0) then image = reform(MParraysum[*,*,SEMdata.Esel]) else image = reform(MParray[*,*,SEMdata.Esel,SEMdata.Asymsel])
    end else begin
      if (SEMdata.Asymsel lt 0) then image = reform(MParraysum[*,*]) else image = reform(MParray[*,*,SEMdata.Asymsel])
    endelse
  end 

  if (SEMdata.MCLSum eq 1) then begin
; here, we want to display the energy-weighted master pattern as an example
    if (SEMdata.Asymsel lt 0) then begin
      image = MParraysum[0:*,0:*,0] * congrid(reform(float(accum_e[0,0:*,0:*])),2*SEMdata.MPimx+1,2*SEMdata.MPimy+1)
      for i=1,SEMdata.MCenergynumbin-1 do image += MParraysum[0:*,0:*,i] * congrid(reform(float(accum_e[i,0:*,0:*])),2*SEMdata.MPimx+1,2*SEMdata.MPimy+1)
    end else begin
      image = MParray[0:*,0:*,0,SEMdata.Asymsel] * congrid(reform(float(accum_e[0,0:*,0:*])),2*SEMdata.MPimx+1,2*SEMdata.MPimy+1)
      for i=1,SEMdata.MCenergynumbin-1 do image += MParray[0:*,0:*,i,SEMdata.Asymsel] * congrid(reform(float(accum_e[i,0:*,0:*])),2*SEMdata.MPimx+1,2*SEMdata.MPimy+1)
    end
  end

  if (SEMdata.MCLSum eq 2) then begin   ; RGB display, for now left blank
    MPimage = 0
    erase
    empty
  end

  if ((SEMdata.MCLSmode eq 1) and (SEMdata.MCLSum ne 2)) then begin
    image = bilinear(image,mpxcircle,mpycircle,missing = 0)
  end 

  if ((SEMdata.MCLSmode eq 2) and (SEMdata.MCLSum ne 2)) then begin
    image = bilinear(image,mpSPxcircle,mpSPycircle,missing = 0)
  end 

; if ((SEMdata.MCLSmode eq 1) and (SEMdata.MCLSum eq 2)) then begin
; end 

  if (SEMdata.MCLSum ne 2) then begin
    tvscl, image 
    WIDGET_CONTROL, set_value=string(min(image),format="(E12.4)"), SEMwidget_s.MPmin
    WIDGET_CONTROL, set_value=string(max(image),format="(E12.4)"), SEMwidget_s.MPmax
    MPimage = image
  end 
 end
end else begin ; we're displaying the Kossel pattern only
  depth = SEMdata.mcdepthmax + SEMdata.Esel * SEMdata.mcdepthstep
  WIDGET_CONTROL, set_value=string(depth,format="(F6.2)"), SEMwidget_s.MCenergyval

  wset,SEMwidget_s.MPdrawID
  image = mLPNH[*,*,SEMdata.Esel]

  if (SEMdata.MCLSmode eq 1) then begin
    image = bilinear(image,mpxcircle,mpycircle,missing = 0)
  end 
  
  if (SEMdata.MCLSmode eq 2) then begin
    image = bilinear(image,mpSPxcircle,mpSPycircle,missing = 0)
  end 

  tvscl, image 
  WIDGET_CONTROL, set_value=string(min(image),format="(E12.4)"), SEMwidget_s.MPmin
  WIDGET_CONTROL, set_value=string(max(image),format="(E12.4)"), SEMwidget_s.MPmax
  MPimage = image
end



end

