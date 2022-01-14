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
; CTEMsoft2013:STEMimagelegend.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMimagelegend.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display the CBED pattern using the correct intensity scaling and add scale bar (optional)
;
;> @date 06/25/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMshowCBED,dummy

;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
common STEM_CBEDpatterns, CBED, CBEDdisplay

; display the CBED pattern
wset,widget_s.CBEDdrawID
if (data.cbedmode eq 0) then begin  ; normal intensity mode
  tvscl,CBEDdisplay
end else begin ; logarithmic intensity
  tvscl,alog(CBEDdisplay+data.addlog)
endelse

; add a scale bar ?
if (data.cbedlegend eq 1) then begin
  bars = [0.5,1.0,2.0,5.0,10.0,20.0]
  length = float(data.patx/4)/(data.CBEDzoom * data.scale) 
  d = abs(bars-length)
  q = where(d eq min(d))
  plots,[10,10+(bars[q[0]]/length)*data.patx/4],[10,10],thick=3,/dev
    STEMprint,'Scale bar in CBED pattern has length '+string(bars[q[0]],format="(F5.1)")+' nm^(-1)'
endif

end
