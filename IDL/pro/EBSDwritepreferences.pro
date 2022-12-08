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
; EMsoft:EBSDwritepreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDwritepreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief write the preferences file
;
;> @date 06/13/13 MDG 1.0 first attempt 
;> @date 10/31/15 MDG 1.1 added ECP parameters; changed pathname for preference file
;--------------------------------------------------------------------------
pro EBSDwritepreferences,noprint=noprint
 
;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata

; prefs file
  openw,1,SEMdata.appdir+SEMdata.prefname
  nprefs = 35
  SEMdata.nprefs = nprefs
  printf,1,nprefs
  printf,1,'EBSDroot::'+SEMdata.EBSDroot
  printf,1,'EBSDMCroot::'+SEMdata.EBSDMCroot
  printf,1,'f90exepath::'+SEMdata.f90exepath

  printf,1,'detl::'+string(SEMdata.detL,format="(F9.2)")
  printf,1,'dettheta::'+string(SEMdata.dettheta,format="(F6.2)")
  printf,1,'detdelta::'+string(SEMdata.detdelta,format="(F6.2)")
  printf,1,'detnumsx::'+string(SEMdata.detnumsx,format="(I6)")
  printf,1,'detnumsy::'+string(SEMdata.detnumsy,format="(I6)")
  printf,1,'detxpc::'+string(SEMdata.detxpc,format="(F7.2)")
  printf,1,'detypc::'+string(SEMdata.detypc,format="(F7.2)")
  printf,1,'detxs::'+string(SEMdata.detxs,format="(I4)")
  printf,1,'detys::'+string(SEMdata.detys,format="(I4)")
  printf,1,'detxss::'+string(SEMdata.detxss,format="(F7.2)")
  printf,1,'detyss::'+string(SEMdata.detyss,format="(F7.2)")
  printf,1,'detbinning::'+string(SEMdata.detbinning,format="(I3)")
  printf,1,'detbeamcurrent::'+string(SEMdata.detbeamcurrent,format="(D9.2)")
  printf,1,'detdwelltime::'+string(SEMdata.detdwelltime,format="(D9.2)")
  printf,1,'detalphaBD::'+string(SEMdata.detalphaBD,format="(D10.4)")

; ECP specific parameters
  printf,1,'detW::'+string(SEMdata.detW,format="(D9.2)")
  printf,1,'detRi::'+string(SEMdata.detRi,format="(D9.2)")
  printf,1,'detRo::'+string(SEMdata.detRo,format="(D9.2)")
  printf,1,'detsampleytilt::'+string(SEMdata.detsampleytilt,format="(D9.2)")
  printf,1,'detthetac::'+string(SEMdata.detthetac,format="(F6.2)")

; window locations
  printf,1,'xlocation::'+string(SEMdata.xlocation,format="(F6.1)")
  printf,1,'ylocation::'+string(SEMdata.ylocation,format="(F6.1)")
  printf,1,'EBSDxlocation::'+string(SEMdata.EBSDxlocation,format="(F6.1)")
  printf,1,'EBSDylocation::'+string(SEMdata.EBSDylocation,format="(F6.1)")
  printf,1,'Detectorxlocation::'+string(SEMdata.Detectorxlocation,format="(F6.1)")
  printf,1,'Detectorylocation::'+string(SEMdata.Detectorylocation,format="(F6.1)")
  printf,1,'Patternxlocation::'+string(SEMdata.patternxlocation,format="(F6.1)")
  printf,1,'Patternylocation::'+string(SEMdata.patternylocation,format="(F6.1)")
  printf,1,'MCxlocation::'+string(SEMdata.MCxlocation,format="(F6.1)")
  printf,1,'MCylocation::'+string(SEMdata.MCylocation,format="(F6.1)")
  printf,1,'MPxlocation::'+string(SEMdata.MPxlocation,format="(F6.1)")
  printf,1,'MPylocation::'+string(SEMdata.MPylocation,format="(F6.1)")
; and close the file
  close,1

  if not keyword_set(noprint) then Core_Print,'The preferences file '+SEMdata.appdir+SEMdata.prefname+' was successfully saved '

end

