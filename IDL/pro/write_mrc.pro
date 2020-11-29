;
; Copyright (c) 2013-2020, Marc De Graef/Carnegie Mellon University
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
; EMsoft:write_mrc.pro
;--------------------------------------------------------------------------
;
; PROGRAM: write_mrc.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief; create an .mrc file with a CBED pattern in it
;
;> @date 11/29/20 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro MRCinit,MRCHeader,FEIHeaders
;
; initialize the mrc FEIHeaders and MRCHeader structures; these must 
; be filled by the calling program. 
;
; written by MDG, 11/29/20 
;

;/**
; * @brief this was taken from http://www.biochem.mpg.de/doc_tom/index.html using the
; * tom_mrcfeistack2emseries code.
; */
FEIHeaders = replicate({FEIstruct,  $
    a_tilt : float(0), $
    b_tilt : float(0), $
    x_stage : float(0), $
    y_stage : float(0), $
    z_stage : float(0), $
    x_shift : float(0), $
    y_shift : float(0), $
    defocus : float(0), $
    exp_time : float(0), $
    mean_int : float(0), $
    tiltaxis : float(0), $
    pixelsize : float(0), $
    magnification : float(0), $
    voltage : float(0), $
    unused : string(' ',format='(A72)') $
},1024)

;/**
; * @brief This spec was taken from http://bio3d.colorado.edu/imod/doc/mrc_format.txt
; * and we are going to assume an IMOD version of 2.6.20 and above:
; */
MRCHeader = {MRCstruct, $
    nx : long(0), $  ; number of columns
    ny : long(0), $  ; number of rows
    nz : long(0), $ ; number of sections
    mode : long(2), $ ; type of image pixel (floating point values)
    nxstart : long(0), $ ; starting point of subimage
    nystart : long(0), $ 
    nzstart : long(0), $ 
    mx : long(0), $ ; grid size in x
    my : long(0), $ ; grid size in y
    mz : long(0), $ ; grid size in z
    xlen : float(0), $ ; cell size; pixel spacing = xlen/mx, ylen/my, zlen/mz
    ylen : float(0), $
    zlen : float(0), $
    alpha : float(90), $ ; cell angles - ignored by IMOD
    beta : float(90), $
    gamma: float(90), $
    mapc : long(1), $ ; map column  1=x,2=y,3=z
    mapr : long(2), $ ; map row     1=x,2=y,3=z
    maps : long(3), $ ; map section 1=x,2=y,3=z
    amin : float(0), $ ; minimum pixel value (needs to be set for proper scaling of data)
    amax : float(0), $ ; maximum pixel value
    amean : float(0), $ ; mean pixel value
    ispg : fix(0), $ ; space group number
    nsymbt : fix(0), $ ; NOT SURE
    next : long(131072), $ ; number of bytes in extended header (1024 * 128 for FEI)
    creatid : fix(0), $ ; used to be an ID number, is 0 as of IMOD 4.2.23
    extra_data : string(' ',format='(A30)'), $ ; not used, first two bytes should be 0
    nint : fix(0), $ ; number of bytes per section (SerialEM interpretation)
    nreal : fix(32), $ ; bit flags for short data type
    extra_data_2 : string(' ',format='(A20)'), $ ; not used
    imodStamp : long(0), $ ; 
    imodFlags : long(0), $ ;
    idtype : fix(0), $ ; ( 0 = mono, 1 = tilt, 2 = tilts, 3 = lina, 4 = lins)
    lens : fix(0), $
    nd1 : fix(0), $ ; for idtype = 1, nd1 = axis (1, 2, or 3)
    nd2 : fix(0), $ 
    vd1 : fix(0), $ ; vd1 = 100. * tilt increment
    vd2 : fix(0), $ ; vd2 = 100. * starting angle
    tiltangles : fltarr(6), $ ; 0,1,2 = original:  3,4,5 = current
    xorg : float(0), $ ; origin of image
    yorg : float(0), $
    zorg : float(0), $
    cmap : 'MAP ', $
    stamp : '    ', $ ; First two bytes have 17 and 17 for big-endian or 68 and 65 for little-endian
    rms : float(0), $ ; RMS deviation of densities from mean density
    nLabels : long(0), $ ; Number of labels with useful data
    labels : string(' ',format='(A800)') $ ; 10 labels of 80 characters each
}

 MRCHeader.nlabels = long(1)
 s = 'EMsoft IDL implementation, MDG/CMU Copyright 2020'
 MRCHeader.labels = s + string(' ',format="(A751)")

end



pro write_mrc,outname

common CBED_data_common, data
common CBEDpattern, CBEDpattern

print,'generating .mrc file ... '
;
; take a CBED pattern and dump it to a .mrc file.
;
dims = size(CBEDpattern,/dimensions)

nth = 1

mi = min(CBEDpattern,max=ma)
me = mean(CBEDpattern)

; and then convert these slices into a single .mrc file
MRCinit,M,F

; set the M variable
M.nx = long(dims[0])
M.ny = long(dims[1])
M.mx = M.nx
M.my = M.ny
M.nz = long(nth)
M.mz = M.nz
M.amin = float(mi)
M.amax = float(ma)
M.amean = float(me)
M.xlen = M.nx
M.ylen = M.ny
M.zlen = M.nz


; set the tilt angles (meaningless in this case, but needs to be done)
for i=0,nth-1 do begin
  F[i].b_tilt = 0.0
  F[i].defocus = 0.0
  F[i].pixelsize = 1.0
  F[i].magnification = 10000.0
  F[i].voltage = data.voltage
  F[i].mean_int = me
endfor

; and write the file
spawn,'/usr/bin/touch '+outname
openu,1,outname
writeu,1,M,F
writeu,1,CBEDpattern
close,1

end
