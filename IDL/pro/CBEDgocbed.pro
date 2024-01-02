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
; CTEMsoft2013:CBEDgocbed.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDgocbed.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief;Compute and display an extracted CBED image
;
;> @date 10/15/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro CBEDgocbed,dummy

;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common CBED_rawdata, gvecs, gmult, gtt, gxy, disks, numHOLZ, HOLZlist
common CBED_HOLZlists, HOLZvals
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common SYM2D, SYM_MATnum, SYM_direc
common CBEDcirclestuff, CBEDschematic, midx, midy, drad, cang, scl, gxpos, ct, st, sc
common CBEDpattern, CBcurrent

wset,data.CBdrawID


; set the desired convergence angle thetau as a fraction of the LACBED convergence angle
	thfrac = data.thetau/data.thetac

; theta value [mrad] for the reference horizontal reflection
 	tth = asin( data.wavelength * data.galen * 0.5 / 1000.0) * 1000.0


; set the desired camera length as a fraction of 1000 mm (the reference camera length) and also scaled w.r.t. the 
; overall beam convergence angle; this is used to scale the coordinates of the reciprocal lattice points
	clscl = data.camlen / data.refcamlen 

; set the reciprocal nm per pixel parameter ( based on (0 2 -1) reflection for L-glutamic acid )
    data.scale = clscl * 2.895324/128.0    ; [nm^{-1}]

; extract a subset of the disks array
	dx = round(0.5*thfrac * (data.datadims[0]-1) * float(drad)/float(midx)) > 1
	ndx = fix(2 * clscl * dx * data.thetac/40.0) > 1
	cx = (data.datadims[0]-1)/2
	dd = 2*dx
	ndd = 2*ndx
	sc = data.wavelength * 6.47113 ; 300.0 / 25.4

; then make a mask to get circular disks
	cmask = shift(dist(2*dx+1),dx,dx)
	cmask[where (cmask le dx)] = 1.0
	cmask[where (cmask gt 1.0)] = 0.0

	thick = data.thicksel
	dimx = 1024
	d2 = dimx/2

; define the Laue center and get its equivalent positions;
	Lauexy = -sc * [data.Lauex, data.Lauey] * data.galen
	Lxy = CBEDApply2DSymmetryPoint(Lauexy,/inverse)

; allocate the pattern array
	CBEDpattern = replicate(0.0,dimx+1,dimx+1)

; for each family, first we decide whether or not the representative disk would fall inside the 
; field of view which we can take to be circular for simplicity; if the disk does fall inside,
; then we loop over all equivalent disks and sample them at the corresponding rotated Laue positions.
	for i=0,data.datadims[3]-1 do begin  ; loop over all families
; equivalent disk positions
    	  posxy = clscl * CBEDApply2DSymmetryPoint(reform(gxy[0:1,i]))
;  z = round( d2 + posxy - ndx )
;  if (min(z) gt 0.0) and (max(z+ndd) lt dimx) then begin 
	  z = round( d2 + posxy - dx )
	  if (min(z) gt 0.0) and (max(z+dd) lt dimx) then begin 

; first we extract each of the circles from the i-th disk
    	    diskstack = fltarr(2*dx+1,2*dx+1,SYM_MATnum)
    	    for j=0,SYM_MATnum-1 do begin
      	      px = round(cx + Lxy[0,j])
      	      py = round(cx + Lxy[1,j])
      	      slice = disks[px-dx:px+dx,py-dx:py+dx, thick, i]
      	      slice = reform(slice) * cmask
      	      diskstack[0,0,j] = slice
    	    endfor

; then we need to apply the symmetry operators to each of the disks in diskstack, in the 
; same order that the Laue shifts are organized
    	    diskstack = CBEDApply2DSymmetryStack(diskstack)

; finally, we rescale the disks by the camera length ratio
; and add all the rotated disks to the hopefully correct location in the CBEDpattern
    	    for j=0,SYM_MATnum-1 do begin
      	      slice = congrid(reform(diskstack[*,*,j]),ndd+1,ndd+1)
      	      px = round(d2 + posxy[0,j] - ndx)
      	      py = round(d2 + posxy[1,j] - ndx)
      	      if ( (px ge 0.0) and (px+ndd lt dimx) and (py ge 0.0) and (py+ndd lt dimx) ) then CBEDpattern[px:px+ndd,py:py+ndd] += slice[0:ndd,0:ndd]
   	    endfor
	  endif
   	endfor
	data.BFmin = min(CBEDpattern,max=ma)
	data.BFmax = ma
	WIDGET_CONTROL, SET_VALUE=string(data.BFmin,FORMAT="(E9.2)"), widget_s.BFmin
	WIDGET_CONTROL, SET_VALUE=string(data.BFmax,FORMAT="(E9.2)"), widget_s.BFmax

  	if (data.cbedmode eq 0) then begin
    	   CBcurrent = CBEDpattern
  	end else begin
           CBcurrent = alog10(CBEDpattern+data.logoffset)
  	end
        tvscl, CBcurrent

end
