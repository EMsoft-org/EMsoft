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
; CTEMsoft2013:ECCIreaddatafile.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIreaddatafile.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Reads the data files produced by the CTEMECCI.f90 program
;
;> @date 12/05/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro ECCIreaddatafile,dummy
;
;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
; the next common block contains all the raw data needed to generate the ECP patterns
common ECCI_rawdata, indices, offsets, kperp, rawdata, ECCILUT
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata

ECCIprint,'Reading data file '+data.ECCIname
openu,1,data.pathname+'/'+data.ECCIname,/f77

dummy = 1

; get the file type (first four characters of the file)
ftp = bytarr(4)
readu,1,ftp
ftp = string(ftp)

; next, verify that we have the correct filetype
if (ftp ne 'ECCI') then begin
  ECCIprint,'File type found '+ftp
  ECCIprint,'File type expected ECCI'
  ECCIprint,'Please select a file of the type ECCI'
  close,1
  dummy = 0
  goto, skip
end

; ok, we've sorted out the filetype; next we read the remainder of the input file

; first a pair of strings of 132 characters each
ECCIname = bytarr(132)
readu,1,ECCIname
data.ECCIname = string(ECCIname)
  ECCIprint,'File name = ->'+strtrim(data.ECCIname,2)+'<-'
WIDGET_CONTROL, SET_VALUE=data.ECCIname, widget_s.ECCIname

ECPname = bytarr(132)
readu,1,ECPname
data.ECPname = string(ECPname)
  ECCIprint,'File name = ->'+strtrim(data.ECPname,2)+'<-'
WIDGET_CONTROL, SET_VALUE=data.ECPname, widget_s.ECPname

finfo = file_info(strtrim(data.pathname+'/'+data.ECCIname,2))
data.filesize = finfo.size
  ECCIprint,'File size = '+string(data.filesize,FORMAT="(I14)")+' bytes'
;WIDGET_CONTROL, SET_VALUE=string(data.filesize,FORMAT="(I14)")+' bytes', widget_s.filesize

; get the program mode
progmode = bytarr(5)
readu,1,progmode
data.progmode = strtrim(string(progmode),2)
  ECCIprint,'Program mode = '+data.progmode
WIDGET_CONTROL, SET_VALUE=data.progmode, widget_s.progmode

; get the summation mode
summode = bytarr(4)
readu,1,summode
data.summode = strtrim(string(summode),2)
  ECCIprint,'Summation mode '+data.summode
WIDGET_CONTROL, SET_VALUE=data.summode, widget_s.summode

; then the crystal structure file name
xtalname = bytarr(132)
readu,1,xtalname
data.xtalname = strtrim(string(xtalname),2)
  ECCIprint,'Xtalname = ->'+data.xtalname+'<-'
WIDGET_CONTROL, SET_VALUE=data.xtalname, widget_s.xtalname

; zone axis vector indices (3 longints)
zoneaxis = lonarr(3)
readu,1,zoneaxis
data.zoneaxis = zoneaxis
wv = '['+string(data.zoneaxis[0],format="(I2)")+' '+ string(data.zoneaxis[1],format="(I2)")+' '+ string(data.zoneaxis[2],format="(I2)")+']'
  ECCIprint,'Zone axis = '+wv
WIDGET_CONTROL, SET_VALUE=wv, widget_s.zoneaxis

; Bragg angle of first (ga) reflection
bragg = 0.0
readu,1,bragg
data.bragg= bragg
  ECCIprint,'Primary Bragg angle = '+string(data.bragg,FORMAT="(F6.3)")

; max beam tilt angle in units of bragg
ktmax = 0.0
readu,1,ktmax
data.ktmax = float(ktmax)
if (data.progmode eq 'array') then begin
    ECCIprint,'Max beam tilt angle in units of |g_a| = '+string(data.ktmax,FORMAT="(F6.3)")
  WIDGET_CONTROL, SET_VALUE=string(data.ktmax,format="(F6.3)"), widget_s.ktmax
end else begin
  WIDGET_CONTROL, SET_VALUE='-------', widget_s.ktmax
endelse

; beam tilt step size
dkt = 0.0
readu,1,dkt
data.dkt = float(dkt)
  ECCIprint,'Beam tilt step size = '+string(data.dkt,FORMAT="(F6.3)")
WIDGET_CONTROL, SET_VALUE=string(data.dkt,format="(F6.3)"), widget_s.dkt


; wave length
voltage = 0.0
readu,1,voltage
data.voltage= voltage
  ECCIprint,'Microscope voltage [V] = '+string(data.voltage,FORMAT="(F7.1)")
WIDGET_CONTROL, SET_VALUE=string(data.voltage,FORMAT="(F7.1)"), widget_s.voltage

; beam convergence
thetac = 0.0
readu,1,thetac
data.thetac = thetac*1000.0/data.dkt
  ECCIprint,'Beam convergence = '+string(data.thetac,FORMAT="(F8.3)")
WIDGET_CONTROL, SET_VALUE=string(data.thetac,FORMAT="(F8.3)"), widget_s.thetac

; pixel size 
dfl = 1.0
readu,1,dfl
data.dfl= float(dfl)
  ECCIprint,'Pixel Size [nm] = '+string(data.dfl,FORMAT="(F6.3)")

; reflections
numref = 0L
readu,1,numref
data.numref = fix(numref)
  ECCIprint,'Number of reflections = '+string(data.numref,FORMAT="(I4)")
WIDGET_CONTROL, SET_VALUE=string(data.numref,FORMAT="(I4)"), widget_s.numref
;hkl = lonarr(3)
;qx = 0.0
;qy = 0.0
;indices = lonarr(3,data.numref)
;offsets = fltarr(2,data.numref)
;for i=0,data.numref-1 do begin
;  readu,1,hkl
;  readu,1,qx,qy
;  indices[0:2,i] = hkl
;  offsets[0:1,i] = [qx,qy] 
;endfor

; wavevectors
numk = 0L
readu,1,numk
data.numk = fix(numk)
  ECCIprint,'Number of wave vectors = '+string(data.numk,FORMAT="(I6)"),/blank
WIDGET_CONTROL, SET_VALUE=string(data.numk,FORMAT="(I6)"), widget_s.numk
ki = 0.0
kj = 0.0
kperp=fltarr(2,data.numk)
for i=0,data.numk-1 do begin
  readu,1,ki,kj
  kperp[0:1,i] = [ki,kj]
  print,i,ki,kj
endfor

; read the dimensions of the images 
datadims = lonarr(2)
readu,1,datadims
datadims = long64(datadims)
data.datadims[0:2] = [datadims,long64(data.numk)]
  ECCIprint,'Data array has dimensions '+string(data.datadims[0],FORMAT="(I8)")+string(data.datadims[1],FORMAT="(I8)")+ $
	string(data.datadims[2],FORMAT="(I8)")
  ECCIprint,'Allocating memory for data array',/blank
rawdata = fltarr(data.datadims)
WIDGET_CONTROL, SET_VALUE=string(data.datadims[0],FORMAT="(I4)"), widget_s.imx
WIDGET_CONTROL, SET_VALUE=string(data.datadims[1],FORMAT="(I4)"), widget_s.imy

; and, finally, read the actual data
ECCIprogressbar,0.0
im = fltarr(data.datadims[0:1])
stepsize = 5
for ik = 0,data.numk-1 do begin
  readu,1,im
  rawdata[0:*,0:*,ik] = im[0:*,0:*]
  if ((ik mod stepsize) eq 0) then ECCIprogressbar,100.0*float(ik)/float(data.datadims[2])
end
ECCIprogressbar,100.0

; and close the main ECCI data file
close,1
  ECCIprint,'Completed reading of ECCI data file',/blank


;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
; we're not done yet, since we also need to read the ECP data ... 
; we'll do this silently, since the user shoud be able to run the ECPDisplay program
; to get the details for that pattern
; The only reason for producing output in the log window is if one of the parameters
; of the ECP computation is different from the corresponding one in the ECCI file

; test for existence of file 

  ECCIprint,'Reading ECP data file',/blank

  openu,1,strtrim(string(data.ECPname),2),/f77
  progname = bytarr(11)
  readu,1,progname
  progname = strtrim(string(progname),2)

  if (progname ne 'CTEMECP.f90') then begin
    dummy = 0
    ECCIprint,'The ECP file name does not link to a CTEMECP output file ',/blank
    goto,skip
    dummy = 0
  end 

; version string
  scversion = bytarr(8)
  readu,1,scversion
  ECPdata.scversion = strtrim(string(scversion),2)

  dims = lonarr(3)
  readu,1,dims
  ECPdata.datadims = long64(dims)
  ECPdata.imx = dims[0]
  ECPdata.imy = dims[1]

  xtalname = bytarr(132)
  readu,1,xtalname
  ECPdata.xtalname = strtrim(string(xtalname),2)

  if (data.xtalname ne ECPdata.xtalname) then begin
    ECCIprint,'  ==> Warning: Crystal structure file names in ECCI and ECP files are different'
    ECCIprint,'     ECP  : '+ECPdata.xtalname 
    ECCIprint,'     ECCI : '+data.xtalname 
    ECCIprint,'  continuing program'
  end 

; distorted unit cell ?
  distort = 0L
  readu,1,distort
  ECPdata.distort= distort

  if (ECPdata.distort ne 0L) then begin
    ECCIprint,'  ==> Warning: Crystal structure in ECP files has been distorted'
    ECCIprint,'  continuing program'
  end 

; unit cell lattice parameters
  abcdist = dblarr(3)
  albegadist = dblarr(3)
  readu,1,abcdist
  readu,1,albegadist
  ECPdata.abcdist = abcdist
  ECPdata.albegadist = albegadist

; accelerating voltage
  voltage = 0.0
  readu,1,voltage
  ECPdata.voltage = voltage
  ECPdata.wavelength= 1226.39/sqrt(voltage + 0.97845E-6 * voltage^2)

  if (ECPdata.voltage ne data.voltage) then begin
    ECCIprint,'  ==> Fatal error: ECP and ECCI files have different microscope voltages '
    ECCIprint,'  aborting file load '
    dummy = 0
    close,1
    goto,skip
  end 

; beam convergence
  thetac = 0.0
  readu,1,thetac
  ECPdata.thetac = thetac
  WIDGET_CONTROL, SET_VALUE=string(ECPdata.thetac,FORMAT="(F8.3)"), widget_s.thetac

; ktmax
  ktmax = 0.0
  readu,1,ktmax
  ECPdata.ktmax = ktmax

  ECPdata.padding = 0
  if (data.thetac gt ECPdata.thetac) then begin
    ECCIprint,'  ==> Warning: ECCI beam tilt outside of range of ECP pattern'
    ECCIprint,'  will adjust ECP pattern size accordingly by padding with zeroes'
    ECCIprint,'  continuing program'
    ECPdata.padding = 1
  end 

; wave vector indices (3 longints)
  wavek = lonarr(3)
  readu,1,wavek
  ECPdata.wavek = wavek

  if (total(abs(data.zoneaxis-ECPdata.wavek)) ne 0) then begin
    ECCIprint,'  ==> Fatal error: ECP and ECCI files have different zone axes'
    ECCIprint,'  aborting file load '
;   dummy = 0
;   close,1
;   goto,skip
  end 

; foil normal indices (3 longints)
  fn = lonarr(3)
  readu,1,fn
  ECPdata.fn = fn

; number of wave vectors inside the disk
  numk = 0L
  readu,1,numk
  ECPdata.numk = numk

; dmin value (not editable or viewable)
  dmin = 0.0
  readu,1,dmin
  ECPdata.dmin = dmin

; first (ga) reflection (not viewable)
  ga = lonarr(3)
  readu,1,ga
  ECPdata.ga = ga

; length of ga, used for proper scaling of the Laue center position
  galen = 0.0
  readu,1,galen
  ECPdata.galen = galen

; delta and gperp
  delta = 0.0
  readu,1,delta
  ECPdata.delta = delta
  gperp = fltarr(3)
  readu,1,gperp
  ECPdata.gperp = gperp

goto, skiptheselines
; convert the coordinates of the beams to the ECP reference frame
  m = [[ga[0]/galen,-gperp[0]],[ga[1]/galen,-gperp[1]]]
  ivm = invert(m)

print, ga
print, galen
print, gperp
print, ivm
  if (data.progmode eq 'array') then begin
    delta = data.dkt*galen
  end else begin
    delta = galen
  endelse
;print,'delta = ',delta
print,' transformed coordinates'
  for i=0,data.numk-1 do begin
    kk = reform(kperp[*,i])/delta
    ij = ivm ## kk
    kperp[0:1,i] = ij[0:1]
print,kperp[0,i],kperp[1,i]
;print,i,transpose(ij)
  endfor
  
skiptheselines:

; various symmetry group numbers 
  symgroups = lonarr(8)
  readu,1,symgroups
  ECPdata.symgroups = symgroups

; starting thickness and thickness increment
  startthick = 0.0
  thickinc = 0.0
  readu,1,startthick,thickinc
  ECPdata.startthick = startthick
  ECPdata.thickinc = thickinc

; and finally, read the data array
  ECPrawdata = fltarr(ECPdata.datadims)
  readu,1,ECPrawdata

; and close the data file
  close,1

; what is the grid spacing in units of pixels ?
  xmid = (ECPdata.datadims[0]-1)/2 
  bragg = 2.0*asin(ECPdata.galen * ECPdata.wavelength * 0.0005)
  ECPdata.dgrid = float(xmid)/ECPdata.ktmax
  ECPdata.xmid = xmid
  ECPdata.kt = round(ktmax)+1


  ECCIprint,'Completed reading ECP data file',/blank

skip:

end
