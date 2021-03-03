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
; EMsoft:Efitgetpreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efitgetpreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief read the preferences file and initialize all relevant widgets
;
;> @date 10/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro Efitgetpreferences,noprint=noprint
 
;------------------------------------------------------------
; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

; does the preferences file exist ?
rs = file_test(Efitdata.appdir+Efitdata.prefname)

if (rs eq 1) then begin
  s = ''
  i = 0
  openr,1,Efitdata.appdir+Efitdata.prefname
  readf,1,i
  Efitdata.nprefs = i

; next, do a little loop and read name:value pairs
  for i=0,Efitdata.nprefs-1 do begin
    readf,1,s
    spos = strpos(s,'::')
    nm = strmid(s,0,spos)
    val = strmid(s,spos+2)
    case nm of 
; root folder
	'Efitroot': Efitdata.Efitroot=val
	'Efitpatternpathname': Efitdata.patternpathname=val

; various parameters
  	'detbinning': Efitdata.detbinning = long(val)
  	'detbeamcurrent': Efitdata.detbeamcurrent = float(val)
  	'detdwelltime': Efitdata.detdwelltime = float(val)
  	'detnumsx': Efitdata.detnumsx = long(val)
  	'detnumsy': Efitdata.detnumsy = long(val)
  	'dettheta': Efitdata.dettheta = float(val)
  	'detdelta': Efitdata.detdelta = float(val)
  	'showcircularmask': Efitdata.showcircularmask = long(val)
  	'EulerConvention': Efitdata.EulerConvention = long(val)
  	'PatternOrigin': Efitdata.PatternOrigin = long(val)
  	'NavStepSize': Efitdata.navstepsize= float(val)

  	'detl': Efitdata.detL = float(val)
  	'detomega': Efitdata.detomega = float(val)
  	'detxpc': Efitdata.detxpc = float(val)
  	'detypc': Efitdata.detypc = float(val)
  	'detgamma': Efitdata.detgamma = float(val)
  	'detphi1': Efitdata.detphi1 = float(val)
  	'detphi': Efitdata.detphi = float(val)
  	'detphi2': Efitdata.detphi2 = float(val)

  	'detsl': Efitdata.detsL = float(val)
  	'detsomega': Efitdata.detsomega = float(val)
  	'detsxpc': Efitdata.detsxpc = float(val)
  	'detsypc': Efitdata.detsypc = float(val)
  	'detsgamma': Efitdata.detsgamma = float(val)
  	'detsphi1': Efitdata.detsphi1 = float(val)
  	'detsphi': Efitdata.detsphi = float(val)
  	'detsphi2': Efitdata.detsphi2 = float(val)

  	'detml': Efitdata.detmL = float(val)
  	'detmomega': Efitdata.detmomega = float(val)
  	'detmxpc': Efitdata.detmxpc = float(val)
  	'detmypc': Efitdata.detmypc = float(val)
  	'detmgamma': Efitdata.detmgamma = float(val)
  	'detmphi1': Efitdata.detmphi1 = float(val)
  	'detmphi': Efitdata.detmphi = float(val)
  	'detmphi2': Efitdata.detmphi2 = float(val)

; window locations
  	'xlocation': Efitdata.xlocation = float(val)
  	'ylocation': Efitdata.ylocation = float(val)
  	'xlocationcontrol': Efitdata.xlocationcontrol = float(val)
  	'ylocationcontrol': Efitdata.ylocationcontrol = float(val)
  	'xlocationnavigator': Efitdata.xlocationnavigator= float(val)
  	'ylocationnavigator': Efitdata.ylocationnavigator= float(val)
  	'xlocationdisplay': Efitdata.xlocationdisplay = float(val)
  	'ylocationdisplay': Efitdata.ylocationdisplay = float(val)

    else: MESSAGE,'unknown option for preferences file'
    endcase
  endfor

  close,1
end else begin
  s = ''
  cd,current=s
  Efitdata.Efitroot=s
; prefs file does not exist yet, so let's create it with default values
  if not keyword_set(noprint) then Core_Print,'Creating preferences file '+Efitdata.appdir+Efitdata.prefname
  if keyword_set(noprint) then Efitwritepreferences,/noprint else Efitwritepreferences
endelse

end

