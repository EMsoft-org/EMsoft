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
; CTEMsoft2013:STEMgetpreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMgetpreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief read the preferences file and initialize all relevant widgets
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMgetpreferences,noprint=noprint
 
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data

; does the preferences file exist ?
rs = file_test(data.prefname)

if (rs eq 1) then begin
  s = ''
  i = 0
  openr,1,data.prefname
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
	'STEMroot': data.STEMroot=val
; CBED zoom
  	'CBEDzoom': data.CBEDzoom=fix(val)
; image legend on or off ?
  	'imagelegend': data.imagelegend = fix(val)
; image output format
  	'imageformat': data.imageformat = fix(val)
; cbed legend on or off ?
  	'cbedlegend': data.cbedlegend = fix(val)
; cbed output format
  	'cbedformat': data.cbedformat = fix(val)
; cbed output mode (linear or logarithmic)
  	'cbedmode': data.cbedmode = fix(val)
; BF detector radius [mm]
  	'BFrho': data.BFrho = float(val)
; HAADF inner detector radius [mm]
  	'HAADFrhoin': data.HAADFrhoin = float(val)
; HAADF outer detector radius [mm]
  	'HAADFrhoout': data.HAADFrhoout = float(val)
; BF detector radius in mrad
  	'BFmrad': data.BFmrad = float(val)
; HAADF inner detector radius [mrad]
  	'HAADFimrad': data.HAADFimrad = float(val)
; HAADF outer detector radius [mrad]
  	'HAADFomrad': data.HAADFomrad = float(val)
; number of detector segments
  	'detsegm': data.detsegm = fix(val)
; detector segment ofset angle
  	'angsegm': data.angsegm = float(val)
; camera length [mm]
  	'camlen': data.camlen = float(val)
; single or multiple sector mode
  	'sectormode': data.sectormode = fix(val)
; k or g selection mode
  	'dfmode': data.dfmode = fix(val)
; aperture radius [mm]
  	'aprad': data.aprad = float(val)
; window locations
  	'xlocation': data.xlocation = float(val)
  	'ylocation': data.ylocation = float(val)
  	'imagexlocation': data.imagexlocation = float(val)
  	'imageylocation': data.imageylocation = float(val)
  	'CTEMBFDFylocation': data.CTEMBFDFylocation = float(val)
  	'CTEMBFDFxlocation': data.CTEMBFDFxlocation = float(val)
  	'cbedxlocation': data.cbedxlocation = float(val)
  	'cbedylocation': data.cbedylocation = float(val)

    else: MESSAGE,'unknown option for preferences file'
    endcase
  endfor

  close,1
end else begin
  s = ''
  cd,current=s
  data.STEMroot=s
; prefs file does not exist yet, so let's create it with default values
  if not keyword_set(noprint) then STEMprint,'Creating preferences file '+data.prefname
  if keyword_set(noprint) then STEMwritepreferences,/noprint else STEMwritepreferences
endelse

end

