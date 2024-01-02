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
; CTEMsoft2013:CBEDprogressbar.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDprogressbar.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display a progress bar in the detector graphics window
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDprogressbar,pct
;
;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data

wset, widget_s.progressdrawID 

if (pct eq 0.0) then begin
; clear the graphics window
  erase
end 

; progress bar
rect = replicate(0B,200,20)
if (pct gt 0.0) then rect[0:2*pct-1,*] = 200
rect[*,0] = 250
rect[*,19] = 250
rect[0,*] = 250
rect[199,*] = 250
tv,rect
empty

; when completely done (pct=100) re-display the detector screen after a short pause
if (pct eq 100.0) then begin
  wait,1.0
  erase
  empty
end


end
