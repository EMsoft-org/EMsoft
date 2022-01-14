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
; CTEMsoft2013:ECCIgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select a CTEMZAdefect output file
;
;> @date 12/05/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro ECCIgetfilename,dummy
 
;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data

  dummy = 1

  s = ''
  cd,current = s
  data.homefolder = s
  if (data.ECCIroot eq 'undefined') then begin
    data.ECCIroot = data.homefolder
  end 

  rootpath = data.ECCIroot

  res=dialog_pickfile(title='Select a valid CTEMECCI data file',path=rootpath)
  if (res eq '') then begin
	  ECCIprint,'No selection made'
	  dummy = 0
	  goto, skip
  end
  finfo = file_info(res)
  data.filesize = finfo.size
; find the last folder separator
  spos = strpos(res,'/',/reverse_search)
  dpos = strpos(res,'.',/reverse_search)
  plen = strlen(res)
  data.pathname = strmid(res,0,spos)
  data.ECCIname = strmid(res,spos+1)
  data.suffix = strmid(res,dpos+1)
  data.ECCIroot = data.pathname

WIDGET_CONTROL, SET_VALUE=data.ECCIname, widget_s.ECCIname

  ECCIprint,' full path '+res
  ECCIprint,' path '+data.pathname
  ECCIprint,' data file '+data.ECCIname
  ECCIprint,' suffix '+data.suffix

skip:
end

