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
; EMsoft:Efitwritepreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efitwritepreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief write the Efit preferences file
;
;> @date 10/13/15 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro Efitwritepreferences,noprint=noprint
 
;------------------------------------------------------------
; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

; prefs file
  openw,1,Efitdata.appdir+Efitdata.prefname
  nprefs = 45
  Efitdata.nprefs = nprefs
  printf,1,nprefs
  printf,1,'Efitroot::'+Efitdata.Efitroot
  printf,1,'Efitpatternpathname::'+Efitdata.patternpathname

; fixed parameters
  printf,1,'detbinning::'+string(Efitdata.detbinning,format="(I3)")
  printf,1,'detbeamcurrent::'+string(Efitdata.detbeamcurrent,format="(D9.2)")
  printf,1,'detdwelltime::'+string(Efitdata.detdwelltime,format="(D9.2)")
  printf,1,'detnumsx::'+string(Efitdata.detnumsx,format="(I6)")
  printf,1,'detnumsy::'+string(Efitdata.detnumsy,format="(I6)")
  printf,1,'dettheta::'+string(Efitdata.dettheta,format="(F6.2)")
  printf,1,'detdelta::'+string(Efitdata.detdelta,format="(F6.2)")
  printf,1,'showcircularmask::'+string(Efitdata.showcircularmask,format="(I1)")
  printf,1,'EulerConvention::'+string(Efitdata.EulerConvention,format="(I1)")
  printf,1,'PatternOrigin::'+string(Efitdata.PatternOrigin,format="(I1)")
  printf,1,'NavStepSize::'+string(Efitdata.navstepsize,format="(F6.2)")

; refinable parameters
  printf,1,'detl::'+string(Efitdata.detL,format="(F9.2)")
  printf,1,'detomega::'+string(Efitdata.detomega,format="(F7.2)")
  printf,1,'detxpc::'+string(Efitdata.detxpc,format="(F7.2)")
  printf,1,'detypc::'+string(Efitdata.detypc,format="(F7.2)")
  printf,1,'detgamma::'+string(Efitdata.detgamma,format="(D9.2)")
  printf,1,'detphi1::'+string(Efitdata.detphi1,format="(F7.2)")
  printf,1,'detphi::'+string(Efitdata.detphi,format="(F7.2)")
  printf,1,'detphi2::'+string(Efitdata.detphi2,format="(F7.2)")

  printf,1,'detsl::'+string(Efitdata.detsL,format="(F9.2)")
  printf,1,'detsomega::'+string(Efitdata.detsomega,format="(F7.2)")
  printf,1,'detsxpc::'+string(Efitdata.detsxpc,format="(F7.2)")
  printf,1,'detsypc::'+string(Efitdata.detsypc,format="(F7.2)")
  printf,1,'detsgamma::'+string(Efitdata.detsgamma,format="(D9.2)")
  printf,1,'detsphi1::'+string(Efitdata.detsphi1,format="(F7.2)")
  printf,1,'detsphi::'+string(Efitdata.detsphi,format="(F7.2)")
  printf,1,'detsphi2::'+string(Efitdata.detsphi2,format="(F7.2)")

  printf,1,'detml::'+string(Efitdata.detmL,format="(F9.2)")
  printf,1,'detmomega::'+string(Efitdata.detmomega,format="(F7.2)")
  printf,1,'detmxpc::'+string(Efitdata.detmxpc,format="(F7.2)")
  printf,1,'detmypc::'+string(Efitdata.detmypc,format="(F7.2)")
  printf,1,'detmgamma::'+string(Efitdata.detmgamma,format="(D9.2)")
  printf,1,'detmphi1::'+string(Efitdata.detmphi1,format="(F7.2)")
  printf,1,'detmphi::'+string(Efitdata.detmphi,format="(F7.2)")
  printf,1,'detmphi2::'+string(Efitdata.detmphi2,format="(F7.2)")


; window locations
  printf,1,'xlocation::'+string(Efitdata.xlocation,format="(F6.1)")
  printf,1,'ylocation::'+string(Efitdata.ylocation,format="(F6.1)")
  printf,1,'xlocationcontrol::'+string(Efitdata.xlocationcontrol,format="(F6.1)")
  printf,1,'ylocationcontrol::'+string(Efitdata.ylocationcontrol,format="(F6.1)")
  printf,1,'xlocationnavigator::'+string(Efitdata.xlocationnavigator,format="(F6.1)")
  printf,1,'ylocationnavigator::'+string(Efitdata.ylocationnavigator,format="(F6.1)")
  printf,1,'xlocationdisplay::'+string(Efitdata.xlocationdisplay,format="(F6.1)")
  printf,1,'ylocationdisplay::'+string(Efitdata.ylocationdisplay,format="(F6.1)")
; and close the file
  close,1

  if not keyword_set(noprint) then Core_Print,'The preferences file '+Efitdata.appdir+Efitdata.prefname+' was successfully saved '

end

