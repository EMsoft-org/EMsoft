;
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
; EMsoft:DPAgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: DPAgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface 
;
;> @date 06/28/16 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro DPAgetfilename,validfile
 
;------------------------------------------------------------
; common blocks
common DPA_widget_common, DPAwidget_s
common DPA_data_common, DPAcommon, DPAdata

validfile = 0

s = ''
cd,current = s
DPAcommon.homefolder = s
if (DPAcommon.DPAroot eq 'undefined') then begin
  DPAcommon.DPAroot = DPAcommon.homefolder
end 

rootpath = DPAcommon.DPAroot

; get the filename
res=dialog_pickfile(title='Select a valid dot product file',path=rootpath,filter='*.h5')
if (res eq '') then begin
  Core_Print,'No selection made'
  goto, skip
end
validfile = 1
finfo = file_info(res)

cp = DPAcommon.currentphase

DPAdata[cp].dpfilesize= finfo.size
; find the last folder separator
spos = strpos(res,'/',/reverse_search)
dpos = strpos(res,'.',/reverse_search)
plen = strlen(res)
DPAdata[cp].dppathname = strmid(res,0,spos)
DPAdata[cp].dpfilename = strmid(res,spos+1)
DPAdata[cp].dpsuffix = strmid(res,dpos+1)
WIDGET_CONTROL, SET_VALUE=DPAdata[cp].dpfilename, DPAwidget_s.dp1filename

Core_Print,' full path '+res
Core_Print,' path '+DPAdata[cp].dppathname
Core_Print,' common file '+DPAdata[cp].dpfilename
Core_Print,' suffix '+DPAdata[cp].dpsuffix

skip:
end

