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
; CTEMsoft2013:STEMgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select a CTEMZAdefect output file
;
;> @date 06/13/13 MDG 1.0 first attempt 
;> @date 02/14/14 MDG 2.0 added support for image series 
;--------------------------------------------------------------------------
pro STEMgetfilename,dummy
 
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data

  s = ''
  cd,current = s
  data.homefolder = s
  if (data.STEMroot eq 'undefined') then begin
    data.STEMroot = data.homefolder
  end 

  rootpath = data.STEMroot

  res=dialog_pickfile(title='Select a valid CTEMZAdefect data file',path=rootpath)
  if (res eq '') then begin
	  STEMprint,'No selection made'
	  goto, skip
  end
  finfo = file_info(res)
  data.filesize = finfo.size
; find the last folder separator
  spos = strpos(res,'/',/reverse_search)
  dpos = strpos(res,'.',/reverse_search)
  plen = strlen(res)
  data.pathname = strmid(res,0,spos)
  data.dataname = strmid(res,spos+1)
  data.suffix = strmid(res,dpos+1)
  data.STEMroot = data.pathname

; finally, check whether or not this filename contains a double underscore; if it
; does, then this file is potentially a part of an image series, and a few other 
; parameters will need to be set as well.
  spos = strpos(data.dataname,'__')
  data.seriesroot = 'noseries'
  if (spos ne -1) then begin
; first we deal with the file name variables
    data.seriesroot = strmid(data.dataname,0,spos)
    next = strlen(data.dataname) - (spos+7)
    data.seriestype = strmid(data.dataname,spos+7,next)
    data.seriesstart= fix(strmid(data.dataname,spos+2,4))
; how many potential images are there in this series
    file_exists = 1
    data.serieslast = data.seriesstart
    while (file_exists eq 1) do begin
      fname = data.pathname+'/'+data.seriesroot+'__'+string(data.serieslast+1,format="(I4.4)")+'.'+data.seriestype
      if (file_test(fname) eq 1) then begin
	data.serieslast += 1
	  STEMprint,' found data file '+fname
      end else begin
	file_exists = 0
      end
    end
    data.seriesnum = data.serieslast - data.seriesstart + 1
  end

  WIDGET_CONTROL, SET_VALUE=data.dataname, widget_s.filename

  STEMprint,' full path '+res
  STEMprint,' path '+data.pathname
  STEMprint,' data file '+data.dataname
  STEMprint,' suffix '+data.suffix

  if (data.seriesroot ne 'none') then begin
     STEMprint,' this file is part of a '+string(data.seriesnum,format="(I4)")+' part series'
  end

skip:
end

