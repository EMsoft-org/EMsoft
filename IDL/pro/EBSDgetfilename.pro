;
; Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:EBSDgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select a MC or MP output file
;
;> @date 03/19/14 MDG 1.0 first attempt 
;> @date 10/11/16 MDG 2.0 removed separate treatment of Monte Carlo file
;--------------------------------------------------------------------------
pro EBSDgetfilename,validfile
 
;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata

  validfile = 0

  s = ''
  cd,current = s
  SEMdata.homefolder = s
  if (SEMdata.EBSDroot eq 'undefined') then begin
    SEMdata.EBSDroot = SEMdata.homefolder
  end 

  rootpath = SEMdata.EBSDroot

 
  res=dialog_pickfile(title='Select a valid Master Pattern data file',path=rootpath,filter='*Master*.*;*master*.*;*MASTER*.*')
  if (res eq '') then begin
	  Core_Print,'No selection made'
	  goto, skip
  end
	validfile = 1
  finfo = file_info(res)
	SEMdata.mpfilesize = finfo.size
; find the last folder separator
	spos = strpos(res,'/',/reverse_search)
	dpos = strpos(res,'.',/reverse_search)
	plen = strlen(res)
	SEMdata.pathname = strmid(res,0,spos)
	SEMdata.EBSDroot = SEMdata.pathname
	SEMdata.mpfilename = strmid(res,spos+1)
	SEMdata.suffix = strmid(res,dpos+1)

  WIDGET_CONTROL, SET_VALUE=SEMdata.mpfilename, SEMwidget_s.mpfilename

  Core_Print,' full path '+res
  Core_Print,' path '+SEMdata.pathname
  Core_Print,' data file '+SEMdata.mpfilename
  Core_Print,' suffix '+SEMdata.suffix

skip:
end

