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
; CTEMsoft2013:STEMreadgeometry.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMreadgeometry.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Reads the data files produced by the CTEMZAdefect.f90 program
;
;> @date 06/20/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMreadgeometry,dummy
;
;------------------------------------------------------------
; common blocks
common STEM_widget_common, widget_s
common STEM_data_common, data
; the next common block contains all the raw data needed to generate the CBED patterns
common STEM_rawdata, indices, offsets, kperp, rawdata
; and this common block is used for the systematic row mode look up table
common STEM_srmode, SRLUT

STEMprint,'Reading data file '+data.dataname
openu,1,data.pathname+'/'+data.dataname,/f77

; get the file type (first four characters of the file)
ftp = bytarr(4)
readu,1,ftp
ftp = string(ftp)

case data.progmode of 
  'STEM': begin
; next, verify that we have the correct filetype
		if ( (ftp eq 'STEM') or (ftp eq 'SRST')) then begin
;  ok, we are in STEM mode, either zone axis or systematic row
  		  if (ftp eq 'STEM') then begin
     		    data.SRZA = 'Zone Axis     ' 
     		    data.SRZAmode = 'ZA'
  		  end else begin
     		    data.SRZA = 'Systematic Row'
     		    data.SRZAmode = 'SR'
  		  end
  		  WIDGET_CONTROL, SET_VALUE=data.SRZA, widget_s.SRZA
		  end else begin
  		    STEMprint,'File type found '+ftp
  		    STEMprint,'File type expected '+data.progmode
  		    STEMprint,'Please select a file of the type '+data.progmode
  		    close,1
  		    goto, skip
		  end
	endcase

  'BFDF': begin
; next, verify that we have the correct filetype
		if ( (ftp eq 'BFDF') or (ftp eq 'SRBF')) then begin
;  ok, we are in BFDF mode, either zone axis or systematic row
  		  if (ftp eq 'BFDF') then begin
     		    data.SRZA = 'Zone Axis     ' 
     		    data.SRZAmode = 'ZA'
  		  end else begin
     		    data.SRZA = 'Systematic Row'
     		    data.SRZAmode = 'SR'
  		  end
  		  WIDGET_CONTROL, SET_VALUE=data.SRZA, widget_s.SRZA
		  end else begin
  		    STEMprint,'File type found '+ftp
  		    STEMprint,'File type expected '+data.progmode
  		    STEMprint,'Please select a file of the type '+data.progmode
  		    close,1
  		    goto, skip
		  end
	endcase

  'CTEM': begin
; next, verify that we have the correct filetype
		if (ftp eq 'CTEM') then begin
;  ok, we are in CTEM mode, which is only available for zone axis geometry
     		  data.SRZA = 'Zone Axis     ' 
     		  data.SRZAmode = 'ZA'
  		  WIDGET_CONTROL, SET_VALUE=data.SRZA, widget_s.SRZA
		  end else begin
  		    STEMprint,'File type found '+ftp
  		    STEMprint,'File type expected '+data.progmode
  		    STEMprint,'Please select a file of the type '+data.progmode
  		    close,1
  		    goto, skip
		  end
	endcase

  else: MESSAGE, "Unknown data format"
endcase

; ok, we've sorted out the filetype; next we read the remainder of the input file

; first a pair of strings of 132 characters each
dataname = bytarr(132)
readu,1,dataname
  STEMprint,'Dataname = ->'+data.dataname+'<-'
WIDGET_CONTROL, SET_VALUE=data.dataname, widget_s.filename

finfo = file_info(data.pathname+'/'+data.dataname)
data.filesize = finfo.size
WIDGET_CONTROL, SET_VALUE=string(data.filesize,FORMAT="(I14)")+' bytes', widget_s.filesize

xtalname = bytarr(132)
readu,1,xtalname
data.xtalname = strtrim(string(xtalname))
  STEMprint,'Xtalname = ->'+data.xtalname+'<-'
WIDGET_CONTROL, SET_VALUE=data.xtalname, widget_s.xtalname

; wave vector indices (3 longints)
wavek = lonarr(3)
readu,1,wavek
data.wavek = wavek
wv = '['+string(data.wavek[0],format="(I2)")+' '+ string(data.wavek[1],format="(I2)")+' '+ string(data.wavek[2],format="(I2)")+']'
  STEMprint,'Wave vector = '+wv
WIDGET_CONTROL, SET_VALUE=wv, widget_s.wavek


; Bragg angle of first (ga) reflection
bragg = 0.0
readu,1,bragg
data.bragg= bragg
  STEMprint,'Primary Bragg angle = '+string(data.bragg,FORMAT="(F6.3)")

; number of pixels along disk radius
nums = 0L
readu,1,nums
data.nums = nums
  STEMprint,'Number of pixels along disk radius = '+string(data.nums,FORMAT="(I)")

; wave length
mLambda = 0.0
readu,1,mLambda
data.wavelength = mlambda
  STEMprint,'Wave length = '+string(data.wavelength*1000.0,FORMAT="(F7.4)")
WIDGET_CONTROL, SET_VALUE=string(1000.0*data.wavelength,FORMAT="(F7.4)"), widget_s.wavelength

; beam convergence
thetac = 0.0
readu,1,thetac
data.thetac = thetac
  STEMprint,'Beam convergence = '+string(data.thetac,FORMAT="(F6.3)")
WIDGET_CONTROL, SET_VALUE=string(data.thetac,FORMAT="(F6.3)"), widget_s.thetac

; set the minimum meaningfull aperture radius for bright field/dark field imaging
data.apminrad = data.thetac / float(data.nums)
WIDGET_CONTROL, SET_VALUE=string(data.apminrad,FORMAT="(F5.2)"), widget_s.aprad

; pixel size 
dfl = 1.0
readu,1,dfl
data.dfl= dfl
  STEMprint,'Pixel Size [nm] = '+string(data.dfl,FORMAT="(F6.3)")

; reflections
numref = 0L
readu,1,numref
data.numref = numref
  STEMprint,'Number of reflections = '+string(data.numref,FORMAT="(I4)")
WIDGET_CONTROL, SET_VALUE=string(data.numref,FORMAT="(I4)"), widget_s.numref
hkl = lonarr(3)
qx = 0.0
qy = 0.0
indices = lonarr(3,data.numref)
offsets = fltarr(2,data.numref)
for i=0,data.numref-1 do begin
  readu,1,hkl
  readu,1,qx,qy
  indices[0:2,i] = hkl
  offsets[0:1,i] = [qx,qy] 
endfor

; wavevectors
numk = 0L
readu,1,numk
data.numk = numk
  STEMprint,'Number of wave vectors = '+string(data.numk,FORMAT="(I6)"),/blank
WIDGET_CONTROL, SET_VALUE=string(data.numk,FORMAT="(I6)"), widget_s.numk
ki = 0L
kj = 0L
kperp=fltarr(2,data.numk)
for i=0,data.numk-1 do begin
  readu,1,ki,kj
  kperp[0:1,i] = [ki,kj]
endfor

; for progmode='BFDF' we also need to read the camera length list
if (data.progmode eq 'BFDF') then begin
  numCL = 0L
  readu,1,numCL
  data.numCL = numCL
  CLarray = fltarr(20)
  readu,1,CLarray
  data.CLarray = CLarray
end 

; read the dimensions of the output array
datadims = lonarr(4)
readu,1,datadims
datadims = long64(datadims)
data.datadims = datadims
  STEMprint,'Data array has dimensions '+string(data.datadims[0],FORMAT="(I8)")+string(data.datadims[1],FORMAT="(I8)")+ $
	string(data.datadims[2],FORMAT="(I8)")+string(data.datadims[3],FORMAT="(I8)")
  STEMprint,'Allocating memory for data array',/blank
rawdata = fltarr(data.datadims)

; and, finally, read the actual data
case data.progmode of
 'STEM': begin
; next we also read the actual pattern data; keep in mind that this data is stored as
; multiple [nx,ny,nbeams,5] arrays, to avoid having arrays that are too large in 
; the data file.  With smaller arrays, we can read them with the f77 option and 
; then store them in the larger rawdata array; it is necessary to do things this way due 
; to the inability of IDL to read f77 data items that are larger than 2Gb.
	  WIDGET_CONTROL, SET_VALUE=string(data.datadims[0],FORMAT="(I4)"), widget_s.imx
	  WIDGET_CONTROL, SET_VALUE=string(data.datadims[1],FORMAT="(I4)"), widget_s.imy
	  STEMprogressbar,0.0
	  datablock = fltarr(data.datadims[0],data.datadims[1],data.datadims[2],5LL)
	  kpos = 0LL
	  numk = data.datadims[3]
	  extra = numk mod 5LL
	  last = numk-extra
	  stepsize = data.datadims[3]/20
	  for ik=0,data.datadims[3]-1 do begin
  	    if ((ik mod 5) eq 0) then begin
    	      readu,1,datablock
    	      if (ik ne last) then begin
      	        rawdata[0:*,0:*,0:*,kpos:kpos+4] = datablock[0:*,0:*,0:*,0:4]
    	      end else begin
      	        rawdata[0:*,0:*,0:*,kpos:kpos+extra-1] = datablock[0:*,0:*,0:*,0:extra-1]
    	      endelse
    	      kpos += 5
  	    end
  	    if ((ik mod stepsize) eq 0) then STEMprogressbar,100.0*float(ik)/float(data.datadims[3])
	  end

	  STEMprogressbar,100.0
	  datablock=0

; and draw the detector pattern for the current parameters
	  STEMdetectorsetup
	endcase

 'CTEM': begin
    	  readu,1,rawdata
	  STEMCTEMBFDFwidget
	endcase

 'BFDF': begin
    	  readu,1,rawdata
	  STEMCTEMBFDFwidget
	endcase

  else: MESSAGE, "Unknown data format"
endcase

close,1
  STEMprint,'Completed reading of data file',/blank

; if we are in systematic row mode, then we need to "fill up" the diffraction disks
; with the results from the central line; once that is done, the remainder of the 
; program should work the same way as for the ZA case...
;
; We'll keep a systematic row look-up table, SRLUT, that labels for each point in the disk what the 
; corresponding intensity value is in the rawdata array... 
if (data.SRZAmode eq 'SR') then begin
; first make a mask of the correct radius and get the integer indices of all the points inside
  mrad = (data.numk-1)/2
  dm = shift(dist(data.numk),mrad,mrad)
  dm[where(dm le mrad)] = 1.0
  dm[where(dm gt mrad)] = 0.0
  q = where(dm gt 0.0)
  dmlist = array_indices(dm,q)
  sz = size(dmlist,/dimensions)
  SRLUT = lonarr(sz[1])
; then get all points except for the the central line of points
  x = reform(dmlist[0,*]) - mrad
  y = reform(dmlist[1,*]) - mrad
  q = where(y ne 0L,cnt)
; and add those points to the current k-vector coordinate arrays
  kp = kperp
  kperp = lonarr(2,data.numk+cnt)
  kperp[0:1,0:data.numk-1] = kp[0:1,0:data.numk-1]
  SRLUT[0:data.numk-1] = lindgen(data.numk)
  for i=data.numk,data.numk+cnt-1 do begin
    kperp[0:1,i] = [x[q[i-data.numk]],y[q[i-data.numk]]]
    SRLUT[i] = x[q[i-data.numk]] + mrad
  endfor
  data.numk += cnt

; and display some information
    STEMprint,'Converted systematic row data to full diffraction disks'
    STEMprint,'Number of k-vectors increased from '+string(2*mrad+1,FORMAT="(I3)")+' to '+string(data.numk,FORMAT="(I6)"),/blank
  WIDGET_CONTROL, SET_VALUE=string(data.numk,FORMAT="(I6)"), widget_s.numk
endif
 

skip:

end
