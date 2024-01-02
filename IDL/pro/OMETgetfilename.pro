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
; EMsoft:OMETgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select an input EMOMmaster file
;
;> @date 10/21/17 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro OMETgetfilename,validfile
 
;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common sample_common, EMOMmaster
common fontstrings, fontstr, fontstrlarge, fontstrsmall

  validfile = 0

  s = ''
  cd,current = s
  OMETdata.homefolder = s
  if (OMETdata.OMETroot eq 'undefined') then begin
    OMETdata.OMETroot = OMETdata.homefolder
  end 

  rootpath = OMETdata.OMETroot

  res=dialog_pickfile(title='Select an EMOMmaster file',path=rootpath,filter='*.h5')
  if (res eq '') then begin
    Core_Print,'No selection made'
   goto, skip
  end
	validfile = 1
  finfo = file_info(res)
; find the last folder separator
	spos = strpos(res,'/',/reverse_search)
	dpos = strpos(res,'.',/reverse_search)
	plen = strlen(res)
	OMETdata.masterpathname = strmid(res,0,spos)
	OMETdata.masterfilename = strmid(res,spos+1)
	OMETdata.HDFsuffix = strmid(res,dpos+1)
  OMETdata.OMETroot = OMETdata.masterpathname

  	;WIDGET_CONTROL, SET_VALUE=OMETdata.masterfilename, OMETwidget_s.expfilename

  Core_Print,' full path '+res
  Core_Print,' path '+OMETdata.masterpathname
  Core_Print,' data file '+OMETdata.masterfilename
  Core_Print,' suffix '+OMETdata.EBSPsuffix

; then load the stereographic version of the EMOM master pattern from the HDF5 file
  file_id = H5F_OPEN(res)
  group_id = H5G_OPEN(file_id,'EMData/OMmaster')
  dset_id = H5D_OPEN(group_id,'OMmasterSPNH')
  EMOMmaster = H5D_READ(dset_id)
  H5D_close,dset_id
  H5G_close,group_id
  H5F_close,file_id 

; next get the size of this array (4,4,MPx,MPy) and set a few auxiliary parameters
  sz = size(EMOMmaster,/dimensions)
  EMOMdata.MPx = sz[2]
  EMOMdata.MPy = sz[3]

; finally, we need to display the name of the master pattern file in the main widget...

skip:
end

