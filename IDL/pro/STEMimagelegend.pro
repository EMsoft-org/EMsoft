;
; Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
;> @brief Add a scale bar to the image (BF or DF in regular diffraction mode)
;
;> @date 06/25/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMimagelegend,wdw
;
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data

wset,wdw
bars = [20,50,100,200,500]
leng = data.dfl * data.datadims[0]/4
d = abs(bars-leng)
q = where(d eq min(d))
if (data.diffractionmode eq 0) then begin
  plots,[10,10+(float(bars[q[0]])/leng)*(data.datadims[0]/4)],[10,10],thick=3,/dev,color=0
end else begin
  plots,[10,10+(float(bars[q[0]])/leng)*(data.datadims[0]/4)],[10,10],thick=3,/dev,color=255
endelse
  STEMprint,'Image scale bar is '+string(bars[q[0]],format="(I3)")+' nanometer long' 

end 
