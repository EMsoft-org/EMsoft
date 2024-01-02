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
; CTEMsoft2013:CBEDupdateLaue.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDupdateLaue.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Draw the Laue position on top of the CBED schematic
;
;> @date 10/15/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDupdateLaue,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common SYM2D, SYM_MATnum, SYM_direc
common CBEDcirclestuff, CBEDschematic, midx, midy, drad, cang, scl, gxpos, ct, st, sc

; use the Z-buffer to draw the individual components
set_plot,'Z'
device,set_resolution=[data.detwinx,data.detwiny]
erase

; then add the Laue position as a red cross
Lx = data.Lauex * gxpos * data.thetac/40.0
Ly = data.Lauey * gxpos * data.thetac/40.0
plots, midx+Lx+[-5,5],midy+Ly,/dev,thick=2
plots, midx+Lx,midy+Ly+[-5,5],/dev,thick=2
empty

; and add a red circle around that spot
plots,midx+Lx+cang*ct,midy+Ly+cang*st,/dev,thick=2

red = tvrd()

set_plot,'X'

; display the disk pattern + the new red channel
wset,data.diskdrawID
output = CBEDschematic
r = reform(output[0,*,*])
r = r or red
output[0,0:*,0:*] = r
tv,output,true=1


end
