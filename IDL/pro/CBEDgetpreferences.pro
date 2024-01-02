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
; CTEMsoft2013:CBEDgetpreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDgetpreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief read the preferences file and initialize all relevant widgets
;
;> @date 09/25/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDgetpreferences,dummy
 
;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data

; does the preferences file exist ?
rs = file_test(data.appdir+data.prefname)

if (rs eq 1) then begin
  s = ''
  i = 0
  openr,1,data.appdir+data.prefname
  readf,1,i
  data.nprefs = i

; next, do a little loop and read name:value pairs
  for i=0,data.nprefs-1 do begin
    readf,1,s
    spos = strpos(s,'::')
    nm = strmid(s,0,spos)
    val = strmid(s,spos+2)
    case nm of 
; root folder
	'CBEDroot': data.CBEDroot=val
; MBCBED root folder
	'MBCBEDroot': data.MBCBEDroot=val
; image output format
  	'imageformat': data.imageformat=fix(val)
; cbed output format
  	'cbedformat': data.cbedformat=fix(val)
; cbed output mode (linear or logarithmic)
  	'cbedmode': data.cbedmode=fix(val)
; Eades inner detector radius [mrad]
  	'Eadesrhoin': data.eadesrhoin=float(val)
; Eades outer detector radius [mrad]
  	'Eadesrhoout': data.eadesrhoout=float(val)
; disk rotation angle
  	'diskrotation': data.diskrotation=float(val)
; camera length [mm]
  	'camlen': data.camlen=float(val)
; dark field display mode
  	'dfdisplaymode': data.dfdisplaymode=fix(val)
; user defined beam convergence angle
  	'thetau': data.thetau=float(val)
; user defined camera length
  	'camlen': data.camlen=float(val)
; Laue center x-coordinate
  	'Lauex': data.Lauex=float(val)
; Laue center y-coordinate
  	'Lauey': data.Lauey=float(val)
; logarithm offset value
  	'logoffset': data.logoffset=float(val)

; window locations
  	'xlocation': data.xlocation=float(val)
  	'ylocation': data.ylocation=float(val)
  	'lacbedxlocation': data.LACBEDxlocation=float(val)
  	'lacbedylocation': data.LACBEDylocation=float(val)
  	'lacbedpatternxlocation': data.LACBEDPatternxlocation=float(val)
  	'lacbedpatternylocation': data.LACBEDPatternylocation=float(val)
  	'cbedxlocation': data.CBEDxlocation=float(val)
  	'cbedylocation': data.CBEDylocation=float(val)
  	'cbeddrawxlocation': data.CBEDDrawxlocation=float(val)
  	'cbedrawdylocation': data.CBEDDrawylocation=float(val)
  	'mbcbedxlocation': data.MBCBEDxlocation=float(val)
  	'mbcbedylocation': data.MBCBEDylocation=float(val)
  	'mbcbeddrawxlocation': data.MBCBEDDrawxlocation=float(val)
  	'mbcbedrawdylocation': data.MBCBEDDrawylocation=float(val)
;
    else: MESSAGE,'unknown option for preferences file'
    endcase
  endfor

  close,1
end else begin
  s = ''
  cd,current=s
  data.CBEDroot=s
; prefs file does not exist yet, so let's create it with default values
  if (widget_s.status ne 0L) then CBEDprint,'Creating preferences file '+data.appdir+data.prefname
  CBEDwritepreferences
endelse

end

