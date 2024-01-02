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
; CTEMsoft2013:CBEDCBEDDrawWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDCBEDDrawWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for CBED pattern display mode
;
;> @date 10/09/13 MDG 1.0 first version
;> @date 11/28/20 MDG 2.0 added pattern save options
;--------------------------------------------------------------------------
pro CBEDCBEDDrawWidget_event, event

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBEDpattern, CBEDpattern

if (data.eventverbose eq 1) then help,event,/structure

; intercept the image widget movement here 
if (event.id eq widget_s.CBEDDrawbase) then begin
  data.CBEDDrawxlocation = event.x
  data.CBEDDrawylocation = event.y-25
    CBEDprint,' Window moved to location ('+string(fix(data.CBEDDrawxlocation),format="(I4)")+','+string(fix(data.CBEDDrawylocation),format="(I4)")+')'
end else begin

    WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

    CASE eventval OF

    'SAVECBEDPATTERN': begin
; display a filesaving widget in the data folder with the file extension filled in
        delist = ['jpeg','tiff','bmp','mrc','hdf5']
        de = delist[data.imageformat]
        filename = DIALOG_PICKFILE(/write,default_extension=de,path=data.pathname,title='enter filename without extension')
        if (data.imageformat le 2) then im = bytscl(CBEDpattern)
        case de of
            'jpeg': write_jpeg,filename,im,quality=100
            'tiff': write_tiff,filename,reverse(im,2)
            'bmp': write_bmp,filename,im
; for the next two file format we need to use the raw data values, not the scaled ones
            'mrc': write_mrc,filename
            'hdf5': write_hdf5_CBED,filename
         else: MESSAGE,'unknown file format option'
        endcase
    endcase

    else: MESSAGE, "Event User Value Not Found"

    endcase

endelse

end
