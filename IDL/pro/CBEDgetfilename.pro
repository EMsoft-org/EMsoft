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
; CTEMsoft2013:CBEDgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select a file
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDgetfilename,validfile,MBCBED=MBCBED,LACBED=LACBED
 
;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data



 
  s = ''
  cd,current = s
  data.homefolder = s
  if (data.CBEDroot eq 'undefined') then begin
    data.CBEDroot = data.homefolder
  end 

  if ( keyword_set(LACBED) ) then begin
    rootpath = data.CBEDroot
  end else begin
    rootpath = data.MBCBEDroot
  end

  if keyword_set(LACBED) then begin
    res=dialog_pickfile(title='Select a valid CTEMlacbed data file',path=rootpath)
  end else begin
    res=dialog_pickfile(title='Select a valid CTEMmbcbed data file',path=rootpath)
  end

  if (res eq '') then begin
	  print,'No selection made; Exiting program'
	  validfile = 0
	  return
  end
  validfile = 1
  finfo = file_info(res)
  data.filesize = finfo.size
; find the last folder separator
  spos = strpos(res,'/',/reverse_search)
  dpos = strpos(res,'.',/reverse_search)
  plen = strlen(res)
  data.pathname = strmid(res,0,spos)
  data.dataname = strmid(res,spos+1)
  data.suffix = strmid(res,dpos+1)
  data.CBEDroot = data.pathname

WIDGET_CONTROL, SET_VALUE=data.dataname, widget_s.dataname

end
