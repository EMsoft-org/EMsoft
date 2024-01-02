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
; CTEMsoft2013:CBEDprosetvalue.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDprosetvalue.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief This is the routine called by the PRO_SET_VALUE option for other widgets
;
;> @date 10/09/13 MDG 1.0 first version
;--------------------------------------------------------------------------
pro CBEDprosetvalue, wid, widval

; by default, this routine has to two 4 things (see IDL manual)
; 1. Process the Value supplied to the procedure in the necessary way
; 2. Unset the PRO_SET_VALUE keyword for the affected widget by setting its value to a null string
; 3. Set the affected widget's value using the SET_VALUE keyword to WIDGET_CONTROL
; 4. Reset the PRO_SET_VALUE keyword to the original procedure
; the reason for this is that if we do not unset, the keyword, then this routine 
; would start calling itself in an infinite loop ...

; 1.  nothing needs to be done here

; 2.
WIDGET_CONTROL, PRO_SET_VALUE='', wid

; 3.
WIDGET_CONTROL, SET_VALUE = widval, wid

; 4.
WIDGET_CONTROL, PRO_SET_VALUE='CBEDprosetvalue', wid


end
