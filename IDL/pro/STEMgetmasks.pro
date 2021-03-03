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
; CTEMsoft2013:STEMgetmasks.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMgetmasks.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Creates the detector masks for standard or segmented imaging.
;
;> @note need to check this for the SR program mode; BFindices is currently
; likely incorrect for that mode ... 
;
;> @date 06/20/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMgetmasks,aperture=aperture
; 
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common STEM_detectordata, STEMdata, STEMcimage, BFdisk, DFdisk, clickablemap, STEMsectormaps, STEMsectors, STEMsectorranges
; the next common block contains all the raw data needed to generate the CBED patterns
common STEM_rawdata, indices, offsets, kperp, rawdata
; this one contains all the arrays needed to apply the BF and HAADF masks
common STEM_masks, ktpg, ktpgang, BFmask, HAADFmask, BFindices, HAADFindices, BFcnt, HAADFcnt
; and this one is used to create the blue channel of the detector plot
common STEM_circles, th, cth, sth, blue, diskpos


; we need to create the BF and HAADF detector masks for the image computation
; obviously, this can only be done if a dataset has been loaded into memory
; There are two different modes: STEM and regular dark field.  For STEM mode,
; we define the BF and HAADF detectors, and determine the indices of the precomputed
; intensities that contribute to them.  For dark field mode, we compute the indices
; that fall inside the specified aperture and return them for the BF detector and
; one single selected reflection

if (data.srzamode eq 'ZA') then zeropos = 0 else zeropos = (data.numref-1)/2

if keyword_set(aperture) then begin
; extract the central disk information from the ktpg and ktpgang arrays (ktpg is in mrad)
  ktp = reform(ktpg(zeropos,*))
  ktpang = reform(ktpgang(zeropos,*))
; convert to x,y coordinates
  ktx = ktp * cos(ktpang)
  kty = ktp * sin(ktpang)
; get the center of the aperture
  cpx = data.apx * data.thetac/data.rdisk
  cpy = data.apy * data.thetac/data.rdisk
; determine which of the beam directions lie inside the aperture
  d = sqrt((ktx-cpx)^2+(kty-cpy)^2)
  q = where(d le data.aprad, cnt)
; and convert those indices to the BFindices array
    if (cnt gt 0) then begin
      BFindices = array_indices(ktp,q)
    end else begin
      BFindices = [0]
        STEMprint,'No beam directions found inside aperture'
    endelse
end else begin

; define the angular detector radii
data.BFmrad = 1000.0*atan(data.BFrho/data.camlen)
data.HAADFimrad = 1000.0*atan(data.HAADFrhoin/data.camlen)
data.HAADFomrad = 1000.0*atan(data.HAADFrhoout/data.camlen)

  STEMprint,'Detector radii in mrad for this camera length: '+string(data.BFmrad,FORMAT="(F7.3)")+' '+ $
  	string(data.HAADFimrad,FORMAT="(F7.3)")+' '+string(data.HAADFomrad,FORMAT="(F7.3)")

; next, compute the two masks
BFmask = replicate(0.0,data.numref,data.numk)
HAADFmask = replicate(0.0,data.numref,data.numk)

; decide which points contribute to the BF detector, both as a mask and as an array of indices
q = where(ktpg le data.BFmrad,BFcnt)
if (BFcnt gt 0) then begin
  BFmask[q] = 1.0
  BFindices = array_indices(ktpg,q)
    STEMprint,' Total number of pixels in BF detector = '+string(BFcnt,FORMAT="(I6)")
end else begin
  STEMprint,' There are no pixels on the BF detector'
end


; if there is only one HAADF detector segment, then the masks are easy (standard BF/HAADF imaging)
if (data.detsegm eq 1) then begin
  q = where(((ktpg gt data.HAADFimrad) and (ktpg le data.HAADFomrad)),HAADFcnt)
  if (HAADFcnt gt 0) then begin
    HAADFmask[q] = 1.0
    HAADFindices = array_indices(ktpg,q)
      STEMprint,' Total number of pixels in HAADF detector = '+string(HAADFcnt,FORMAT="(I6)")
  end else begin
    STEMprint,' There are no pixels on the HAADF detector'
  end
end else begin   ; there is more than one detector segment, so we need to determine the active segment(s)
; first thing to do: determine which detector segment spans the zero angle value
  diff = reform(STEMsectorranges[0,*]) - reform(STEMsectorranges[1,*])
  q=where(diff lt 0,cnt)
  secnum = 0	; no problems for any sector
  if (cnt gt 0) then secnum = q[0]
; print,'problem sector = ',secnum, q
; make a copy of ktpg
  ktpgc = ktpg
; and exclude the points that can not contribute to any sector because of the radial detector size
  q = where(((ktpg le data.HAADFimrad) or (ktpg gt data.HAADFomrad)),cnt)
  if (cnt gt 0) then ktpgc[q] = 0.0
; then we need to check the angular ranges for each sector and keep only the active sectors

;print,transpose(STEMsectorranges[0,1:data.detsegm])
;print,transpose(STEMsectorranges[1,1:data.detsegm])
;print,STEMsectors

;help,ktpgang
;print, min(ktpgang,max=ma),ma
;help, ktpgc
;print, min(ktpgc,max=ma),ma

  for i=1,data.detsegm do begin			; loop over sectors
    if (STEMsectors[i] eq 0) then begin		; is this sector inactive?
;print,' working on sector ',i,secnum
      if (i eq secnum) then begin		; is this the problem sector?
; yes, it is
	q = where(((ktpgang ge STEMsectorranges[1,i]) or (ktpgang lt STEMsectorranges[0,i])),gcnt)
 	if (gcnt gt 0) then ktpgc[q] = 0.0
;print,'problem sector: > ',STEMsectorranges[1,i], ' or < ', STEMsectorranges[0,i], ', # ',gcnt
      end else begin				
; no, it is not
	q = where(((ktpgang ge STEMsectorranges[1,i]) and (ktpgang lt STEMsectorranges[0,i])),gcnt)
	if (gcnt gt 0) then ktpgc[q] = 0.0
;print,'regular sector: > ',STEMsectorranges[1,i], ' and < ', STEMsectorranges[0,i],', # ',gcnt
      endelse
    endif
  endfor
; and now determine the contributing points and their indices
  q = where(((ktpgc gt data.HAADFimrad) and (ktpgc le data.HAADFomrad)),HAADFcnt)
  if (HAADFcnt gt 0) then begin
    HAADFmask[q] = 1.0
    HAADFindices = array_indices(ktpgc,q)
      STEMprint,' Total number of pixels on HAADF detector = '+string(HAADFcnt,FORMAT="(I6)")
  end else begin
    STEMprint,' There are no pixels on the HAADF detector'
  end
end

endelse ; if keyword_set(aperture)


end

