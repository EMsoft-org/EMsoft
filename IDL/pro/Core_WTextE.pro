;
; Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:Core_WTextE.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_WTextE.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Editable text widget short cut routine
;
;> @date 09/25/13 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
function Core_WTextE, parent, title, fnt, x1, y1, x2, y2, value, uvalue, routine, aright=aright

if keyword_set(aright) then begin
  label2 = WIDGET_LABEL(parent, $
		VALUE=title, $
		FONT=fnt, $
		XSIZE=x1, $
		YSIZE=y1, $
		/ALIGN_RIGHT)

  val =  WIDGET_TEXT(parent, $
		VALUE=value,$
		XSIZE=x2, $
		YSIZE=y2, $
		/EDITABLE, $
        EVENT_PRO=routine, $
		UVALUE=uvalue, $
		/ALIGN_RIGHT)
end else begin
  label2 = WIDGET_LABEL(parent, $
		VALUE=title, $
		FONT=fnt, $
		XSIZE=x1, $
		YSIZE=y1, $
		/ALIGN_LEFT)
    
  val =  WIDGET_TEXT(parent, $
		VALUE=value,$
		XSIZE=x2, $
		YSIZE=y2, $
		/EDITABLE, $
        EVENT_PRO=routine, $
		UVALUE=uvalue, $
		/ALIGN_LEFT)
end

return,val
end


