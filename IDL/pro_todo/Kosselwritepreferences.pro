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
; CTEMsoft2013:Kosselwritepreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Kosselwritepreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief write the preferences file
;
;> @date 11/23/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro Kosselwritepreferences,dummy
 
;------------------------------------------------------------
; common blocks
common Kossel_widget_common, widget_s
common Kossel_data_common, data

; prefs file
  openw,1,data.prefname
  nprefs = 6
  data.nprefs = nprefs
  printf,1,nprefs
  printf,1,'Kosselroot::'+data.Kosselroot
; pattern output format
  printf,1,'Kosselformat::'+string(data.Kosselformat,format="(I1)")

; window locations
  printf,1,'xlocation::'+string(data.xlocation,format="(F6.1)")
  printf,1,'ylocation::'+string(data.ylocation,format="(F6.1)")
  printf,1,'Kosselxlocation::'+string(data.Kosselxlocation,format="(F6.1)")
  printf,1,'Kosselylocation::'+string(data.Kosselylocation,format="(F6.1)")
  close,1

end

