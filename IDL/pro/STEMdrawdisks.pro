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
; CTEMsoft2013:STEMdrawdisks.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMdrawdisks.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Draw the diffraction disk outlines in the blue chanel of the detector window
;
;> @date 06/20/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMdrawdisks,darkfield=darkfield,addaperture=addaperture,highlightdisk=highlightdisk
;
common STEM_widget_common, widget_s
common STEM_data_common, data
common STEM_circles, th, cth, sth, blue, diskpos
common STEM_rawdata, indices, offsets, kperp, rawdata
common STEM_masks, ktpg, ktpgang, BFmask, HAADFmask, BFindices, HAADFindices, BFcnt, HAADFcnt
common STEM_detectordata, STEMdata, STEMcimage, BFdisk, DFdisk, clickablemap, STEMsectormaps, STEMsectors, STEMsectorranges

; we're using the Z-buffer to draw the array of circles, and then we crop and copy the resulting plot
set_plot,'Z'
device,set_resolution=[900,900]
erase

if keyword_set(darkfield) then diskpos=fltarr(2,data.numref)

if (data.SRZAmode eq 'ZA') then begin
;get the radius for the disks
  magn = 0.001*float((data.detwinx-1)/2)/data.patang
  cdisk = reform(ktpg[*,0]) * data.camlen * magn  ; pixel distance for disk centers
  data.rdisk = data.thetac * data.camlen * magn  ; disk radius in pixels

; skip the center reflection in this loop
  for iref=1,data.numref-1 do begin
; center of disk in units of pixels
    cpos =  [ offsets[0,iref], offsets[1,iref] ]
; normalize cpos and convert to mrad scale
    cpos /= sqrt(cpos[0]^2+cpos[1]^2)
    cpos *= cdisk[iref]  ; this is now in mm
    if keyword_set(darkfield) then diskpos[0:1,iref] = cpos[0:1]   ; this is in pixel coordinates, without the offset
; and plot the disk
    if (data.srzamode eq 'ZA') then plots,450+cpos[0]+data.rdisk*cth,450+cpos[1]+data.rdisk*sth,/dev,thick=3 else plots,450+cpos[0]+data.rdisk*cth,450+cpos[1]+data.rdisk*sth,/dev,thick=3 
  endfor
end else begin  ; SR mode has zero beam at different location
;get the radius for the disks
  magn = 0.001*float((data.detwinx-1)/2)/data.patang
  zeropos = (data.datadims[3]-1)/2
  cdisk = reform(ktpg[*,zeropos]) * data.camlen * magn  ; pixel distance for disk centers
  data.rdisk = data.thetac * data.camlen * magn  ; disk radius in pixels

  zeropos = (data.numref-1)/2
; skip the center reflection in this loop
  for iref=0,data.numref-1 do begin
   if (iref ne zeropos) then begin
; center of disk in units of pixels
    cpos =  [ offsets[0,iref], offsets[1,iref] ]
; normalize cpos and convert to mrad scale
    cpos /= sqrt(cpos[0]^2+cpos[1]^2)
    cpos *= cdisk[iref]  ; this is now in mm
    if keyword_set(darkfield) then diskpos[0:1,iref] = cpos[0:1]   ; this is in pixel coordinates, without the offset
; and plot the disk
    plots,450+cpos[0]+data.rdisk*cth,450+cpos[1]+data.rdisk*sth,/dev,thick=3 
   end
  endfor
end

; and add the central disk
plots,450+data.rdisk*cth,450+data.rdisk*sth,/dev,thick=2 

blue = tvrd()

erase

if keyword_set(darkfield) then begin
; next do the red channel
; add a red cross in the central disk
  plots,450+[-data.rdisk,data.rdisk],450,/dev,thick=1
  plots,450,450+[-data.rdisk,data.rdisk],/dev,thick=1

; do we need to superimpose the aperture outline ?
  if keyword_set(addaperture) then begin
; convert the aperture radius to pixels
    aprad = data.aprad * data.rdisk / data.thetac
; and plot a circle at the correct position
    if (data.srzamode eq 'ZA') then plots,450+data.apx+aprad*cth,450+data.apy+aprad*sth,/dev,thick=1 else plots,450+data.apx+aprad*cth,450+data.apy+aprad*sth,/dev,thick=1 
  endif

; what if we selected one of the diffracted disks?
  if arg_present(highlightdisk) then begin
; center of disk in units of pixels
    iref = highlightdisk
    cpos =  [ offsets[0,iref], offsets[1,iref] ]
; normalize cpos and convert to mrad scale
    cpos /= sqrt(cpos[0]^2+cpos[1]^2)
    cpos *= cdisk[iref]  ; this is now in mm
; and plot the disk
    if (data.srzamode eq 'ZA') then begin
      plots,450+cpos[0]+data.rdisk*cth,450+cpos[1]+data.rdisk*sth,/dev,thick=3 
      plots,450+diskpos[0,iref]+data.apx+aprad*cth,450+diskpos[1,iref]+data.apy+aprad*sth,/dev,thick=1 
    end else begin
      plots,450+cpos[0]+data.rdisk*cth,450+cpos[1]+data.rdisk*sth,/dev,thick=3 
      plots,450+diskpos[0,iref]+data.apx+aprad*cth,450+diskpos[1,iref]+data.apy+aprad*sth,/dev,thick=1 
    endelse
  endif

  red = tvrd()
endif

set_plot,'X'

; this image is now 900 by 900, and we need to take the central 401x401
map = blue[450-200:450+200,450-200:450+200]
slice = reform(STEMcimage[2,*,*])
q = where(map gt 0,cnt)
if (cnt gt 0) then slice[q] = 255B
STEMcimage[2,0:*,0:*] = slice[0:*,0:*]
if keyword_set(darkfield) then begin
  map = red[450-200:450+200,450-200:450+200]
  STEMcimage[0,0:*,0:*] = map[0:*,0:*]
endif

wset,widget_s.detdrawID
tvscl,STEMcimage,true=1

end
