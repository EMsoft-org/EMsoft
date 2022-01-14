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
;
; Function: qu2eu
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Quaternions to Euler angles [Morawiec, page 40]
;
;> @note verified 8/5/13
;
;> @param q quaternion (scalar first)
; 
;> @date 08/04/16 MDG 1.0 original
;--------------------------------------------------------------------------
function Core_qu2eu,q

;common rotationcommon, LPs, epsijk
epsijk = 1.0

qq = q

q03 = qq[0]^2+qq[3]^2
q12 = qq[1]^2+qq[2]^2
chi = sqrt(q03*q12)

if (chi eq 0.D0) then begin
  if (q12 eq 0.D0) then  begin
   if (epsijk eq 1) then begin
    Phi = 0.D0
    phi2 = 0.D0                  ; arbitrarily due to degeneracy
    phi1 = atan(-2.D0*qq[0]*qq[3],qq[0]^2-qq[3]^2)
   end else begin
    Phi = 0.D0
    phi2 = 0.D0                  ; arbitrarily due to degeneracy
    phi1 = atan( 2.D0*qq[0]*qq[3],qq[0]^2-qq[3]^2)
   end
  end else begin
   if (epsijk eq 1) then begin
    Phi = !dpi
    phi2 = 0.D0                  ; arbitrarily due to degeneracy
    phi1 = atan(2.D0*qq[1]*qq[2],qq[1]^2-qq[2]^2)
   end else begin
    Phi = !dpi
    phi2 = 0.D0                  ; arbitrarily due to degeneracy
    phi1 = atan(2.D0*qq[1]*qq[2],qq[1]^2-qq[2]^2)
   end
  end
end else begin           ; this is not a special degenerate case
  if (epsijk eq 1) then begin
    Phi = atan( 2.D0*chi, q03-q12 )
    chi = 1.D0/chi
    phi1 = atan( (-qq[0]*qq[2]+qq[1]*qq[3])*chi, (-qq[0]*qq[1]-qq[2]*qq[3])*chi )
    phi2 = atan( (qq[0]*qq[2]+qq[1]*qq[3])*chi, (-qq[0]*qq[1]+qq[2]*qq[3])*chi )
  end else begin
    Phi = atan( 2.D0*chi, q03-q12 )
    chi = 1.D0/chi
    phi1 = atan( (qq[0]*qq[2]+qq[1]*qq[3])*chi, (qq[0]*qq[1]-qq[2]*qq[3])*chi )
    phi2 = atan( (-qq[0]*qq[2]+qq[1]*qq[3])*chi, (qq[0]*qq[1]+qq[2]*qq[3])*chi )
  end
end

res = [ phi1, Phi, phi2 ]

; reduce Euler angles to definition ranges (and positive values only)
if (res[0] lt 0.D0) then res[0] = (res[0]+100.D0*!dpi) mod (2.D0*!dpi)
if (res[1] lt 0.D0) then res[1] = (res[1]+100.D0*!dpi) mod !dpi
if (res[2] lt 0.D0) then res[2] = (res[2]+100.D0*!dpi) mod (2.D0*!dpi)

return,res / !dtor
end ;function Core_eu2qu
