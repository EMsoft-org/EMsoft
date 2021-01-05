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
; EMsoft:Core_LambertSphereToSquare.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_LambertSphereToSquare.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief modified Lambert projection from unit sphere to standard square
;
;> @date 10/13/15 MDG 1.0 initial implementation 
;--------------------------------------------------------------------------
function Core_LambertSphereToSquare, dc, ierr

eps = 1.0E-6
ierr = 0
sPi2 = 0.886226925452758
sPio2 = 1.253314137315500

; check to make sure that the input point lies on the unit sphere
if (abs(1.0-total(dc^2)) gt eps) then begin
  res = [ 0.0, 0.0 ]
  ierr = 1
end else begin
; intercept the points (0,0,+-1)
  if (abs(dc[2]) eq 1.0) then begin
    res = [ 0.0, 0.0 ]
  end else begin
    if (abs(dc[1]) le abs(dc[0])) then begin
      q = abs(dc[0])/dc[0] * sqrt(2.0*(1.0-abs(dc[2])))
      res = [ q * sPi2, q * atan(dc[1]/dc[0]/sPi2) ]
    end else begin
      q = abs(dc[1])/dc[1] * sqrt(2.0*(1.0-abs(dc[2])))
      res = [  q * atan(dc[0]/dc[1])/sPi2, q * sPi2 ]
    endelse
  endelse
endelse

res = res / sPio2

return, res
end
