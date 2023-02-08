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
; EMsoft:Core_LambertInterpolation.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_LambertInterpolation.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief standard Lambert interpolation routine
;
;> @date 02/08/23 MDG 1.0 initial implementation 
;--------------------------------------------------------------------------
pro Core_LambertInterpolation, dc, scl, npx, npy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=swap

; Lambert sphere to square transformation
xy = scl * Core_LambertSphereToSquare( dc, istat )
if (istat ne 0) then begin
  print,'input direction cosines : ', dc
  print,'input scale factor      : ', scl
  print,' LambertgetInterpolation: Something went wrong during interpolation...'
endif

if (keyword_set(swap)) then begin
  x = xy[0]
  xy[0] = xy[1]
  xy[1] = -x
endif

; four-point interpolation (bi-quadratic)
nix = fix(npx+xy[0])-npx
niy = fix(npy+xy[1])-npy
nixp = nix+1
niyp = niy+1
if (nixp gt npx) then nixp = nix
if (niyp gt npy) then niyp = niy
if (nix lt -npx) then nix = nixp
if (niy lt -npy) then niy = niyp
dx = xy[0]-nix
dy = xy[1]-niy
dxm = 1.0-dx
dym = 1.0-dy

nix = nix+npx
niy = niy+npy
nixp= nixp+npx
niyp= niyp+npy

end

