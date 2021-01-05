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
; EMsoft:Efitgetfilename.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efitgetfilename.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display an interface and ask user to select an input EBSD pattern file, or a Master Pattern file
;
;> @date 10/13/15 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro Efitgetfilename,validfile,PATTERNFILE=PATTERNFILE,MPFILE=MPFILE,JSONFILE=JSONFILE,EULERFILE=EULERFILE
 
;------------------------------------------------------------
; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern
common inverseGaussian, inverseGaussianMask


  validfile = 0

  s = ''
  cd,current = s
  Efitdata.homefolder = s
  if (Efitdata.Efitroot eq 'undefined') then begin
    Efitdata.Efitroot = Efitdata.homefolder
  end 

  rootpath = Efitdata.Efitroot

  if keyword_set(PATTERNFILE) then begin
    res=dialog_pickfile(title='Select an experimental EBSD pattern file',path=rootpath,filter='*.jpg;*.jpeg;*.tiff;*.tif;*.bmp')
    if (res eq '') then begin
	  Core_Print,'No selection made'
	  goto, skip
    end
	validfile = 1
  	finfo = file_info(res)
	Efitdata.patternfilesize = finfo.size
; find the last folder separator
	spos = strpos(res,'/',/reverse_search)
	dpos = strpos(res,'.',/reverse_search)
	plen = strlen(res)
	Efitdata.patternpathname = strmid(res,0,spos)
	Efitdata.patternfilename = strmid(res,spos+1)
	Efitdata.EBSPsuffix = strmid(res,dpos+1)
        Efitdata.Efitroot = Efitdata.patternpathname

  	WIDGET_CONTROL, SET_VALUE=Efitdata.patternfilename, Efitwidget_s.expfilename

  	Core_Print,' full path '+res
  	Core_Print,' path '+Efitdata.patternpathname
  	Core_Print,' data file '+Efitdata.patternfilename
  	Core_Print,' suffix '+Efitdata.EBSPsuffix

; then load the image
        expEBSDpattern = read_image(res)
; get the dimensions of the pattern; if this is an RGB-style file, extract one color plane only
        sz = size(expEBSDpattern)
        if (sz[0] eq 3) then begin
          sz = size(expEBSDpattern,/dimensions)
          if (sz[0] eq 3) then expEBSDpattern = reform(expEBSDpattern[0,*,*]) else expEBSDpattern = reform(expEBSDpattern[*,*,0])
        end 
        sz = size(expEBSDpattern,/dimensions)
        Efitdata.detnumsx = sz[0]
        Efitdata.detnumsy = sz[1]
; added 8/3/16: inverseGaussian mask
	inverseGaussianMask = fltarr(sz)
	line = findgen(sz[0])-float(sz[0]/2)
	xgrid = fltarr(sz)
	for i=0,sz[1]-1 do xgrid[0:*,i] = line[0:*]
	line = findgen(sz[1])-float(sz[1]/2)
	ygrid = fltarr(sz)
	for i=0,sz[0]-1 do ygrid[i,0:*] = line[0:*]
        grid = xgrid^2+ygrid^2
	grid = grid / float(max(sz)/2)^2
	inverseGaussianMask = 1.0-0.75*exp(-grid*1.5)
	inverseGaussianMask = reform(inverseGaussianMask,sz[0]*sz[1])
; added 6/18/16
        if ((Efitdata.EBSPsuffix eq 'tif') or (Efitdata.EBSPsuffix eq 'tiff')) then expEBSDpattern = reverse(expEBSDpattern,2)
; set the corresponding widgets to these values
        WIDGET_CONTROL, set_value=string(sz[0],format="(I5)"), Efitwidget_s.detnumsx
        WIDGET_CONTROL, set_value=string(sz[1],format="(I5)"), Efitwidget_s.detnumsy
        Core_Print,' Pattern dimensions set'
; and activate the master pattern button
        WIDGET_CONTROL, Efitwidget_s.mploadfile, sensitive = 1
; 
  endif

  if keyword_set(MPFILE) then begin 
    res=dialog_pickfile(title='Select a valid Master Pattern data file',path=rootpath,filter='*Master*.h5;*master*.h5;*MASTER*.h5')
    if (res eq '') then begin
	  Core_Print,'No selection made'
	  goto, skip
    end
	validfile = 1
  	finfo = file_info(res)
	Efitdata.mpfilesize = finfo.size
; find the last folder separator
	spos = strpos(res,'/',/reverse_search)
	dpos = strpos(res,'.',/reverse_search)
	plen = strlen(res)
	Efitdata.pathname = strmid(res,0,spos)
	Efitdata.mpfilename = strmid(res,spos+1)
	Efitdata.suffix = strmid(res,dpos+1)

  	WIDGET_CONTROL, SET_VALUE=Efitdata.mpfilename, Efitwidget_s.mpfilename

  	Core_Print,' full path '+res
  	Core_Print,' path '+Efitdata.pathname
  	Core_Print,' data file '+Efitdata.mpfilename
  	Core_Print,' suffix '+Efitdata.suffix
  endif

  if keyword_set(JSONFILE) then begin 
    res=dialog_pickfile(title='Enter *.json file name',path=rootpath,filter='*.json',default_extension='json',/write)
    if (res eq '') then begin
	  Core_Print,'No selection made'
	  goto, skip
    end
	validfile = 1
; find the last folder separator
	spos = strpos(res,'/',/reverse_search)
	dpos = strpos(res,'.',/reverse_search)
	plen = strlen(res)
	Efitdata.jsonpathname = strmid(res,0,spos)
	Efitdata.jsonfilename = strmid(res,spos+1)
	Efitdata.jsonsuffix = strmid(res,dpos+1)

  	Core_Print,' full path '+res
  	Core_Print,' path '+Efitdata.jsonpathname
  	Core_Print,' data file '+Efitdata.jsonfilename
  	Core_Print,' suffix '+Efitdata.jsonsuffix
  endif

  if keyword_set(EULERFILE) then begin 
    res=dialog_pickfile(title='Enter *.txt file name for Euler angles',path=rootpath,filter='*.txt',default_extension='txt',/write)
    if (res eq '') then begin
	  Core_Print,'No selection made'
	  goto, skip
    end
	validfile = 1
; find the last folder separator
	spos = strpos(res,'/',/reverse_search)
	dpos = strpos(res,'.',/reverse_search)
	plen = strlen(res)
	Efitdata.eulerpathname = strmid(res,0,spos)
	Efitdata.eulerfilename = strmid(res,spos+1)
	Efitdata.eulersuffix = strmid(res,dpos+1)

  	Core_Print,' full path '+res
  	Core_Print,' path '+Efitdata.eulerpathname
  	Core_Print,' data file '+Efitdata.eulerfilename
  	Core_Print,' suffix '+Efitdata.eulersuffix
  endif

skip:
end

