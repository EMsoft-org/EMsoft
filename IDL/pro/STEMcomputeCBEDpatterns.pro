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
; CTEMsoft2013:STEMcomputeCBEDpatterns.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMcomputeCBEDpatterns.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select a file
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMcomputeCBEDpatterns,ipos,jpos
;
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
; the next common block contains all the raw data needed to generate the CBED patterns
common STEM_rawdata, indices, offsets, kperp, rawdata
; here's where we will store the CBED patterns
common STEM_CBEDpatterns, CBED, CBEDdisplay

; First of all, let's determine how large the CBED pattern ought to be...
; In the regular CBED code, we always generate patterns that are 900x900 pixels large.
; If we use a 256x256 image size, then that means a total of 65,536 CBED patterns,
; which, in turn, means 49 Gb if stored in single precision.  So, it is really not
; manageable to preompute all the patterns.  We'll just let the user click on the image,
; and then the CBED pattern for that particular image pixel (ipos,jpos) will be computed and 
; displayed.

; allocate the CBED pattern array
CBED = replicate(0.0,data.patx,data.paty)

; a few geometrical prefactors; this is different from the mbcbed program
; in that the camera length is included in the detector summation masks, not 
; in the CBED pattern size.  So the CBED pattern remains unchanged when the 
; camera length is adjusted; it is the detector size that is scaled.  The reason
; for this is that it is computationally much faster to change the detector mask
; than to recompute the CBED pattern.   Actually, the image computation does not
; require knowledge of each individual CBED pattern...
; To do this, we compute the CBED pattern for a reference camera length, and scale
; based on this reference for other camera lengths

; each diffraction disk has a diameter of 2*data.nums+1 pixels.  In the CBED pattern, 
; computed pixel corresponds to an actual image pixel, so that the angular magnification
; of the pattern depends on the number of pixels and the beam divergence angle (not on the
; camera length, since that is part of the detector parameters).  The disk offset parameters
; are specified in reciprocal nanometers, so we need to scale this to get the correct 
; angular magnfication.

data.scale = 1000.0 * data.wavelength * (data.nums+1) * data.bragg / (data.thetac * sin(data.bragg))
PX = data.patx/2
scmax = PX + data.nums		; if a disk center coordinate is larger than this, exclude that disk

if (data.SRZAmode eq 'SR') then begin
  mrad = (data.datadims[3]-1)/2
; this next line may require some verification on more complex crystal structures...
  data.scale *= 0.5
end

lipos = long64(ipos)
ljpos = long64(jpos)

; next, start the actual loops over the beam directions and reflections
for ibeam=0LL,long64(data.numk)-1LL do begin
  for iref=0LL,long64(data.numref)-1LL do begin
   if ((abs(offsets[0,iref]) lt scmax) and (abs(offsets[1,iref]) lt scmax)) then begin
    ip = PX + offsets[0,iref]*data.scale + kperp[0,ibeam]
;   jp = PX + offsets[1,iref]*data.scale + kperp[1,ibeam]
; the following line needs to be verified
    jp = PX - offsets[1,iref]*data.scale + kperp[1,ibeam]
    if (((ip ge 0) and (ip le data.patx-1)) and ((jp ge 0) and (jp le data.paty-1))) then begin
      if (data.SRZAmode eq 'ZA' ) then begin
        CBED[ip,jp] += rawdata[lipos,ljpos,iref,ibeam]
      end else begin
        CBED[ip,jp] += rawdata[lipos,ljpos,iref,kperp[0,ibeam]+mrad]
      end
    end
   end
  end
end

data.CBEDmin = min(CBED > 0.0)
data.CBEDmax = max(CBED)
WIDGET_CONTROL,SET_VALUE=string(data.CBEDmin,format="(F8.5)"),widget_s.CBEDmin
WIDGET_CONTROL,SET_VALUE=string(data.CBEDmax,format="(F8.5)"),widget_s.CBEDmax


; when we're done, display the CBED pattern in the proper window
if (data.CBEDzoom ne 1) then begin
  mid = (data.CBEDzoom * data.patx)/2
  CBEDdisplay = rebin(CBED,data.CBEDzoom*data.patx,data.CBEDzoom*data.paty)
  CBEDdisplay = CBEDdisplay[mid-data.patx/2:mid+data.patx/2-1,mid-data.paty/2:mid+data.paty/2-1]
end else begin
  CBEDdisplay = CBED
endelse

STEMshowCBED

end
