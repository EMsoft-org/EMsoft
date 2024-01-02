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
; CTEMsoft2013:CBEDwritepreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDwritepreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief write the preferences file
;
;> @date 09/25/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDwritepreferences,dummy
 
;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data

; prefs file
  openw,1,data.appdir+data.prefname
  nprefs = 27
  data.nprefs = nprefs
  printf,1,nprefs
  printf,1,'CBEDroot::'+data.CBEDroot
  printf,1,'MBCBEDroot::'+data.MBCBEDroot
; cbed output format
  printf,1,'cbedformat::'+string(data.cbedformat,format="(I1)")
; cbed output mode (linear or logarithmic)
  printf,1,'cbedmode::'+string(data.cbedmode,format="(I1)")
; Eades inner detector radius [mrad]
  printf,1,'Eadesrhoin::'+string(data.eadesrhoin,format="(F8.3)")
; Eades outer detector radius [mrad]
  printf,1,'Eadesrhoout::'+string(data.eadesrhoout,format="(F8.3)")
; disk rotation angle
  printf,1,'diskrotation::'+string(data.diskrotation,format="(F5.2)")
; camera length [mm]
  printf,1,'camlen::'+string(data.camlen,format="(F6.1)")
; dark field display mode
  printf,1,'dfdisplaymode::'+string(data.dfdisplaymode,format="(I1)")
; user defined beam convergence angle
  printf,1,'thetau::'+string(data.thetau,format="(F6.2)")
; Laue center x-coordinate
  printf,1,'Lauex::'+string(data.Lauex,format="(F6.2)")
; Laue center y-coordinate
  printf,1,'Lauey::'+string(data.Lauey,format="(F6.2)")
; logarithm offset value
  printf,1,'logoffset::'+string(data.logoffset,format="(E9.2)")

; window locations
  printf,1,'xlocation::'+string(data.xlocation,format="(F6.1)")
  printf,1,'ylocation::'+string(data.ylocation,format="(F6.1)")
  printf,1,'lacbedxlocation::'+string(data.LACBEDxlocation,format="(F6.1)")
  printf,1,'lacbedylocation::'+string(data.LACBEDylocation,format="(F6.1)")
  printf,1,'lacbedpatternxlocation::'+string(data.LACBEDPatternxlocation,format="(F6.1)")
  printf,1,'lacbedpatternylocation::'+string(data.LACBEDPatternylocation,format="(F6.1)")
  printf,1,'cbedxlocation::'+string(data.CBEDxlocation,format="(F6.1)")
  printf,1,'cbedylocation::'+string(data.CBEDylocation,format="(F6.1)")
  printf,1,'cbeddrawxlocation::'+string(data.CBEDDrawxlocation,format="(F6.1)")
  printf,1,'cbedrawdylocation::'+string(data.CBEDDrawylocation,format="(F6.1)")
  printf,1,'mbcbedxlocation::'+string(data.MBCBEDxlocation,format="(F6.1)")
  printf,1,'mbcbedylocation::'+string(data.MBCBEDylocation,format="(F6.1)")
  printf,1,'mbcbeddrawxlocation::'+string(data.MBCBEDDrawxlocation,format="(F6.1)")
  printf,1,'mbcbedrawdylocation::'+string(data.MBCBEDDrawylocation,format="(F6.1)")
; and close the file
  close,1

  if (widget_s.status ne 0L) then CBEDprint,'The preferences file '+data.prefname+' was successfully saved '

end

