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
; CTEMsoft2013:STEMwritepreferences.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMwritepreferences.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief write the preferences file
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMwritepreferences,noprint=noprint
 
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data

; prefs file
  openw,1,data.prefname
  nprefs = 27
  data.nprefs = nprefs
  printf,1,nprefs
  printf,1,'STEMroot::'+data.STEMroot
; CBED zoom
  printf,1,'CBEDzoom::'+string(data.CBEDzoom,format="(I1)")
; image legend on or off ?
  printf,1,'imagelegend::'+string(data.imagelegend,format="(I1)")
; image output format
  printf,1,'imageformat::'+string(data.imageformat,format="(I1)")
; cbed legend on or off ?
  printf,1,'cbedlegend::'+string(data.cbedlegend,format="(I1)")
; cbed output format
  printf,1,'cbedformat::'+string(data.cbedformat,format="(I1)")
; cbed output mode (linear or logarithmic)
  printf,1,'cbedmode::'+string(data.cbedmode,format="(I1)")
; BF detector radius [mm]
  printf,1,'BFrho::'+string(data.BFrho,format="(F5.2)")
; HAADF inner detector radius [mm]
  printf,1,'HAADFrhoin::'+string(data.HAADFrhoin,format="(F5.2)")
; HAADF outer detector radius [mm]
  printf,1,'HAADFrhoout::'+string(data.HAADFrhoout,format="(F5.2)")
; BF detector radius in mrad
  printf,1,'BFmrad::'+string(data.BFmrad,format="(F5.2)")
; HAADF inner detector radius [mrad]
  printf,1,'HAADFimrad::'+string(data.HAADFimrad,format="(F5.2)")
; HAADF outer detector radius [mrad]
  printf,1,'HAADFomrad::'+string(data.HAADFomrad,format="(F5.2)")
; number of detector segments
  printf,1,'detsegm::'+string(data.detsegm,format="(I2)")
; detector segment ofset angle
  printf,1,'angsegm::'+string(data.angsegm,format="(F5.2)")
; camera length [mm]
  printf,1,'camlen::'+string(data.camlen,format="(F6.1)")
; single or multiple sector mode
  printf,1,'sectormode::'+string(data.sectormode,format="(I1)")
; k or g selection mode
  printf,1,'dfmode::'+string(data.dfmode,format="(I1)")
; aperture radius [mm]
  printf,1,'aprad::'+string(data.aprad,format="(F6.2)")
; window locations
  printf,1,'xlocation::'+string(data.xlocation,format="(F6.1)")
  printf,1,'ylocation::'+string(data.ylocation,format="(F6.1)")
  printf,1,'imagexlocation::'+string(data.imagexlocation,format="(F6.1)")
  printf,1,'imageylocation::'+string(data.imageylocation,format="(F6.1)")
  printf,1,'CTEMBFDFxlocation::'+string(data.CTEMBFDFxlocation,format="(F6.1)")
  printf,1,'CTEMBFDFylocation::'+string(data.CTEMBFDFylocation,format="(F6.1)")
  printf,1,'cbedxlocation::'+string(data.cbedxlocation,format="(F6.1)")
  printf,1,'cbedylocation::'+string(data.cbedylocation,format="(F6.1)")
; and close the file
  close,1

  if not keyword_set(noprint) then STEMprint,'The preferences file '+data.prefname+' was successfully saved '

end

