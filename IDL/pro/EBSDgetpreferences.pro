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
; EMsoft:EBSDgetpreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDgetpreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief read the preferences file and initialize all relevant widgets
;
;> @date 06/13/13 MDG 1.0 first attempt 
;> @date 10/31/15 MDG 1.1 added ECP parameters; changed pathname for preference file
;--------------------------------------------------------------------------
pro EBSDgetpreferences,noprint=noprint
 
;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata

; does the preferences file exist ?
rs = file_test(SEMdata.appdir+SEMdata.prefname)

if (rs eq 1) then begin
  s = ''
  i = 0
  openr,1,SEMdata.appdir+SEMdata.prefname
  readf,1,i
  SEMdata.nprefs = i

; next, do a little loop and read name:value pairs
  for i=0,SEMdata.nprefs-1 do begin
    readf,1,s
    spos = strpos(s,'::')
    nm = strmid(s,0,spos)
    val = strmid(s,spos+2)
    case nm of 
; root folder
	'EBSDroot': SEMdata.EBSDroot=val
	'EBSDMCroot': SEMdata.EBSDMCroot=val
	'f90exepath': SEMdata.f90exepath=val

; various parameters
  	'detl': SEMdata.detL = float(val)
  	'dettheta': SEMdata.dettheta = float(val)
  	'detdelta': SEMdata.detdelta = float(val)
  	'detnumsx': SEMdata.detnumsx = long(val)
  	'detnumsy': SEMdata.detnumsy = long(val)
  	'detxpc': SEMdata.detxpc = float(val)
  	'detypc': SEMdata.detypc = float(val)
  	'detxs': SEMdata.detxs = long(val)
  	'detys': SEMdata.detys = long(val)
  	'detxss': SEMdata.detxss = float(val)
  	'detyss': SEMdata.detyss = float(val)
  	'detbinning': SEMdata.detbinning = long(val)
  	'detbeamcurrent': SEMdata.detbeamcurrent = float(val)
  	'detdwelltime': SEMdata.detdwelltime = float(val)
  	'detalphaBD': SEMdata.detalphaBD = float(val)

; ECP parameters
  	'detW': SEMdata.detW = float(val)
  	'detRi': SEMdata.detRi = float(val)
  	'detRo': SEMdata.detRo = float(val)
  	'detsampleytilt': SEMdata.detsampleytilt = float(val)
  	'detthetac': SEMdata.detthetac = float(val)

; window locations
  	'xlocation': SEMdata.xlocation = float(val)
  	'ylocation': SEMdata.ylocation = float(val)
  	'EBSDxlocation': SEMdata.EBSDxlocation = float(val)
  	'EBSDylocation': SEMdata.EBSDylocation = float(val)
  	'Patternxlocation': SEMdata.patternxlocation = float(val)
  	'Patternylocation': SEMdata.patternylocation = float(val)
  	'Detectorxlocation': SEMdata.Detectorxlocation = float(val)
  	'Detectorylocation': SEMdata.Detectorylocation = float(val)
  	'MCxlocation': SEMdata.MCxlocation = float(val)
  	'MCylocation': SEMdata.MCylocation = float(val)
  	'MPxlocation': SEMdata.MPxlocation = float(val)
  	'MPylocation': SEMdata.MPylocation = float(val)
  	'Detectorxlocation': SEMdata.Detectorxlocation = float(val)
  	'Detectorylocation': SEMdata.Detectorylocation = float(val)

    else: MESSAGE,'unknown option for preferences file'
    endcase
  endfor

  close,1
end else begin
  s = ''
  cd,current=s
  SEMdata.EBSDroot=s
; prefs file does not exist yet, so let's create it with default values
  if not keyword_set(noprint) then Core_Print,'Creating preferences file '+SEMdata.appdir+SEMdata.prefname
  if keyword_set(noprint) then EBSDwritepreferences,/noprint else EBSDwritepreferences
endelse

end

