;
; Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
; CTEMsoft2013:STEMcomputeBFHAADF.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMcomputeBFHAADF.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Compute the BF and HAADF images for the selected detector configuration
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMcomputeBFHAADF,selection=selection,aperture=aperture
;
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
; the next common block contains all the raw data needed to generate the CBED patterns
common STEM_rawdata, indices, offsets, kperp, rawdata
; this one contains all the arrays needed to apply the BF and HAADF masks
common STEM_masks, ktpg, ktpgang, BFmask, HAADFmask, BFindices, HAADFindices, BFcnt, HAADFcnt
common STEM_images, BFimage, HAADFimage, DFimage
; and this common block is used for the systematic row mode look up table
common STEM_srmode, SRLUT

if (data.srzamode eq 'ZA') then zeropos = 0 else zeropos = (data.numref-1)/2


if arg_present(selection) then begin
 if keyword_set(aperture) then begin ;we need to compute the HAADF image with the selected aperture position/radius
  if (selection eq 0) then begin  ; BF image
    BFimage = replicate(0.0,data.datadims[0],data.datadims[1])
    if (data.srzamode eq 'ZA') then begin
      for i=0,BFcnt-1 do BFimage += reform(rawdata[*,*,0,BFindices[i]])
    end else begin
      for i=0,BFcnt-1 do BFimage += reform(rawdata[*,*,zeropos,SRLUT[BFindices[i]]])
    endelse
    wset,widget_s.BFdrawID
    tvscl,BFimage
    data.BFmin = min(BFimage)
    WIDGET_CONTROL, set_value=string(data.BFmin,format="(F)"), widget_s.BFmin
    data.BFmax = max(BFimage)
    WIDGET_CONTROL, set_value=string(data.BFmax,format="(F)"), widget_s.BFmax
  end else begin ; DF image
    DFimage = replicate(0.0,data.datadims[0],data.datadims[1])
    if (data.srzamode eq 'ZA') then begin
      for i=0,BFcnt-1 do DFimage += reform(rawdata[*,*,selection,BFindices[i]])
    end else begin
      for i=0,BFcnt-1 do DFimage += reform(rawdata[*,*,selection,SRLUT[BFindices[i]]])
    endelse
    wset,widget_s.HAADFdrawID
    tvscl,DFimage
    data.HAADFmin = min(DFimage)
    WIDGET_CONTROL, set_value=string(data.HAADFmin,format="(F)"), widget_s.HAADFmin
    data.HAADFmax = max(DFimage)
    WIDGET_CONTROL, set_value=string(data.HAADFmax,format="(F)"), widget_s.HAADFmax
  endelse
 end else begin		; no aperture keyword present
  DFimage = replicate(0.0,data.datadims[0],data.datadims[1])
  if (data.srzamode eq 'ZA') then begin
    for i=0,data.numk-1 do DFimage += reform(rawdata[*,*,selection,i])
  end else begin
    for i=0,data.numk-1 do DFimage += reform(rawdata[*,*,selection,SRLUT[i]])
  endelse

  wset,widget_s.HAADFdrawID
  tvscl,DFimage

; add a legend
  if (data.imagelegend eq 1) then STEMimagelegend,widget_s.HAADFdrawID

  WIDGET_CONTROL, set_value=string(0.0,format="(F)"), widget_s.BFmin
  WIDGET_CONTROL, set_value=string(0.0,format="(F)"), widget_s.BFmax
  data.HAADFmin = min(DFimage)
  WIDGET_CONTROL, set_value=string(data.HAADFmin,format="(F)"), widget_s.HAADFmin
  data.HAADFmax = max(DFimage)
  WIDGET_CONTROL, set_value=string(data.HAADFmax,format="(F)"), widget_s.HAADFmax
 endelse

end else begin ; no reflection has been selected 
 if keyword_set(aperture) then begin ;we need to compute the BF image with the selected aperture position/radius

 end else begin
; allocate the two image arrays
  BFimage = replicate(0.0,data.datadims[0],data.datadims[1])
  HAADFimage = replicate(0.0,data.datadims[0],data.datadims[1])

 if (data.SRZAmode eq 'ZA') then begin
  for i=0,BFcnt-1 do BFimage += reform(rawdata[*,*,BFindices[0,i],BFindices[1,i]])
  for i=0,HAADFcnt-1 do HAADFimage += reform(rawdata[*,*,HAADFindices[0,i],HAADFindices[1,i]])
 end else begin
  for i=0,BFcnt-1 do BFimage += reform(rawdata[*,*,BFindices[0,i],SRLUT[BFindices[1,i]]])
  for i=0,HAADFcnt-1 do HAADFimage += reform(rawdata[*,*,HAADFindices[0,i],SRLUT[HAADFindices[1,i]]])
 end

; display the images
  wset,widget_s.BFdrawID
  tvscl,BFimage
  wset,widget_s.HAADFdrawID
  tvscl,HAADFimage

; add a legend
  if (data.imagelegend eq 1) then STEMimagelegend,widget_s.BFdrawID

; and update the min/max values for the two images
  data.BFmin = min(BFimage)
  WIDGET_CONTROL, set_value=string(data.BFmin,format="(F)"), widget_s.BFmin
  data.BFmax = max(BFimage)
  WIDGET_CONTROL, set_value=string(data.BFmax,format="(F)"), widget_s.BFmax
  data.HAADFmin = min(HAADFimage)
  WIDGET_CONTROL, set_value=string(data.HAADFmin,format="(F)"), widget_s.HAADFmin
  data.HAADFmax = max(HAADFimage)
  WIDGET_CONTROL, set_value=string(data.HAADFmax,format="(F)"), widget_s.HAADFmax
 endelse

endelse


end 
