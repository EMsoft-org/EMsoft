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
; CTEMsoft2013:CBEDmoveLaue.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDmoveLaue.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Move from one Laue position to another one
;
;> @date 10/15/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDmoveLaue,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common SYM2D, SYM_MATnum, SYM_direc
common CBEDcirclestuff, CBEDschematic, midx, midy, drad, cang, scl, gxpos, ct, st, sc


  if (data.movemode eq 0) then begin
    CBEDupdateLaue
    CBEDgocbed
  end else begin
    dx = (data.Lauex-data.oldLauex) / float(5*(data.jumpsel+1)+1)
    dy = (data.Lauey-data.oldLauey) / float(5*(data.jumpsel+1)+1)
    if (( dx ne 0.0) or (dy ne 0.0) ) then begin
      savex = data.Lauex
      savey = data.Lauey
      for i=1,5*(data.jumpsel+1)+1 do begin
        data.Lauex = data.oldLauex +  float(i) * dx
        data.Lauey = data.oldLauey +  float(i) * dy
        WIDGET_CONTROL, set_value=string(data.Lauex,format="(F6.2)"), widget_s.Lauex
        WIDGET_CONTROL, set_value=string(data.Lauey,format="(F6.2)"), widget_s.Lauey
        CBEDupdateLaue
        CBEDgocbed
      endfor
      data.Lauex = savex
      data.Lauey = savey
    end else begin
      CBEDupdateLaue
      CBEDgocbed
    end 
  end 
  data.oldLauex = data.Lauex
  data.oldLauey = data.Lauey

end
