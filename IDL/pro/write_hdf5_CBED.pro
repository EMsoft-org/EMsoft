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
; EMsoft:write_hdf5_CBED
;--------------------------------------------------------------------------
;
; PROGRAM: write_hdf5_CBED
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief; create an hdf5 file with a CBED pattern in it
;
;> @date 11/29/20 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro write_hdf5_CBED, hdfname

common CBED_data_common, data
common CBEDpattern, CBEDpattern

save_thicksel = data.thicksel 

; compute a CBED thickness series and dump it to a .mrc file.
;
dims = size(CBEDpattern,/dimensions)
CBEDstack = fltarr(dims[0],dims[1],data.numt)

for i=0,data.numt-1 do begin 
  data.thicksel = i
  CBEDgocbed
  CBEDstack[0:*,0:*,i] = CBEDpattern
endfor

; create the file and close it immediately so that we can use H5_PUTDATA to add datasets
res = H5F_CREATE(hdfname)
H5F_CLOSE, res

; put the CBED pattern into the file 
H5_PUTDATA, hdfname, 'CBEDpattern', CBEDstack

; then write a bunch of parameters to the file as well in the 'parameters' group 
H5_PUTDATA, hdfname, 'parameters/CameraLength-mm', data.camlen 
H5_PUTDATA, hdfname, 'parameters/ConvergenceAngle-mrad', data.thetau 
if (data.cbedmode eq 0) then begin 
    H5_PUTDATA, hdfname, 'parameters/IntensityScaling', 'linear'
end else begin 
    H5_PUTDATA, hdfname, 'parameters/intensityscaling', 'logarithmic'
    H5_PUTDATA, hdfname, 'parameters/logoffset', data.logoffset
endelse
H5_PUTDATA, hdfname, 'parameters/LaueCenter', [data.Lauex, data.Lauey]
H5_PUTDATA, hdfname, 'parameters/pix-per-nm', data.scale
H5_PUTDATA, hdfname, 'parameters/thickness-nm', data.thickness

; reset original parameters
data.thicksel = save_thicksel
CBEDgocbed
end
