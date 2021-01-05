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
; EMsoft:SEMDisplay_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: SEMDisplay_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Electron backscatter diffraction pattern display event handler
;
;> @date 03/19/14 MDG 1.0 initial version
;> @date 10/11/16 MDG 2.0 removed explicit Monte Carlo options, since they are now part of the master file
;--------------------------------------------------------------------------
pro SEMDisplay_event, event

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata


; used for debugging purposes
if (SEMdata.eventverbose eq 1) then help,event,/structure

; there are only two different events: either the window has been moved or 
; some button or something has been activated inside the window
if (event.id eq SEMwidget_s.base) then begin
  SEMdata.xlocation = event.x
  SEMdata.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
;REMOVE FROM HERE -------------------------------------------
  	'MCDISPLAY': begin
; create the Monte Carlo display widget
		              EBSDMCDisplayWidget
	               endcase
; UNTIL HERE

  	'MPDISPLAY': begin
; create the Master Pattern display widget
		              EBSDMCDisplayWidget
	               endcase

  	'MPFILE': begin
; ask the user to select the master pattern data file
		            EBSDgetfilename,validfile
; read the data file and populate all the relevant fields
		            if (validfile eq 1) then begin
                   res = H5F_IS_HDF5(SEMdata.pathname+'/'+SEMdata.mpfilename)
                   if (res eq 0) then begin
                     Core_print,'This file is not an HDF5 file ...'
                   end else EBSDreadHDFdatafile
                endif
	             endcase

	'DETECTOR': begin
                if (SEMdata.mpfiletype eq 1) then EBSDDetectorWidget
                if (SEMdata.mpfiletype eq 2) then ECPDetectorWidget
                if (SEMdata.mpfiletype eq 3) then KosselDetectorWidget
                if (SEMdata.mpfiletype eq 4) then EBSDDetectorWidget
	             endcase

 	'QUIT': begin
		            EBSDwritepreferences
; do a general cleanup of potentially open widgets
 		            Core_Print,'Quitting program',/blank
                if (XRegistered("KosselDetectorWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.detectorbase, /DESTROY
                if (XRegistered("ECPDetectorWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.detectorbase, /DESTROY
                if (XRegistered("EBSDDetectorWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.detectorbase, /DESTROY
                if (XRegistered("EBSDPatternWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
                if (XRegistered("ECPatternWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
                if (XRegistered("KosselPatternWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
                if (XRegistered("EBSDMCDisplayWidget") NE 0) then WIDGET_CONTROL, SEMwidget_s.MCdisplaybase, /DESTROY
		            WIDGET_CONTROL, SEMwidget_s.base, /DESTROY
		            !EXCEPT=1  ; turn exceptions back on (turned off at start of widget)
	             endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 



