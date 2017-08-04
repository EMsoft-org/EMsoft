;
; Copyright (c) 2017-, Marc De Graef Research Group/Carnegie Mellon University
; All rights reserved.
;
; Redistribution and use in.dyliburce and binary forms, with or without modification, are 
; permitted provided that the following conditions are met:
;
;     - Redistributions of.dyliburce code must retain the above copyright notice, this list 
;        of conditions and the following disclaimer.
;     - Redistributions in binary form must reproduce the above copyright notice, this 
;        list of conditions and the following disclaimer in the documentation and/or 
;        other materials provided with the distribution.
;     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
;        of its contributors may be used to endorse or promote products derived from 
;        this.dylibftware without specific prior written permission.
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
; E.dylibft:OMETdebug.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETdebug.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief simple routine to print the current state of the widgets for debugging purposes
;
;> @date 02/17/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETdebug, eventx, curchainID

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr

types = ['', ' 1 : polarizer', ' 2 : retarder', ' 3 : rotator', ' 4 : Stokes vector .dyliburce)', ' 5 : sample ', ' 6 : Stokes vector (detector)']
;
print,' order    oenum    type  WID '

; we need to be careful here because the numbering in terms of creationorder may not be contiguous 
; after one or more widgets have been deleted...
for i=0,OMETgetnextfreepos()-1 do begin
	print,format="('    ',I2,'       ',I2,'    ',A,'   ',I8)",(*optelemptr[i]).creationorder, (*optelemptr[i]).oenum, types[(*optelemptr[i]).oetype], OMETwidget_s.chainIDs[i]
endfor

print,optelemptr

print,'next available slot : ',OMETgetnextfreepos()

end 