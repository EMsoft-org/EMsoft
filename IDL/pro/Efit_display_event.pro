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
; EMsoft:Efit_display_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_display_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for Efit_display.pro routine
;
;> @date 10/13/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro Efit_display_event,event

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata


if (event.id eq Efitwidget_s.displaybase) then begin
  Efitdata.xlocationdisplay = event.x
  Efitdata.ylocationdisplay = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
        'CLOSEDISPLAY': begin
                WIDGET_CONTROL, Efitwidget_s.displaybase, /DESTROY
        endcase
        'SAVEPATTERN': begin
                delist = ['jpeg','tiff','bmp']
                de = delist[Efitdata.imageformat]
                filename = DIALOG_PICKFILE(/write,default_extension=de,path=Efitdata.pathname,title='enter filename without extension')
                im = tvrd()
                case de of
                    'jpeg': write_jpeg,filename,im,quality=100
                    'tiff': write_tiff,filename,reverse(im,2)
                    'bmp': write_bmp,filename,im
                 else: MESSAGE,'unknown file format option'
                endcase
        endcase

  else: MESSAGE, "Efit_display_event: Event "+eventval+" Unknown"

  endcase

endelse

end


