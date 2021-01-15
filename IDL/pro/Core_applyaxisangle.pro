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
; EMsoft:Core_applyaxisangle.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_applyaxisangle.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief apply an axis-angle pair rotation to an euler triplet
;
;> @date 05/22/14 MDG 1.0 first version
;--------------------------------------------------------------------------
function Core_applyaxisangle, axang, euang

; first create the rotation quaternion
    om2 = axang(3)*!dtor*0.5D0
    v = axang[0:2]
    v = v/sqrt(total(v^2))
    qr = [cos(om2),sin(om2)*v] ; unit quaternion
; then convert the Euler angles to a quaternion
    ee = 0.5D0*euang*!dtor
    cPhi = cos(ee[1])
    sPhi = sin(ee[1])
    cm = cos(ee[0]-ee[2])
    sm = sin(ee[0]-ee[2])
    cp = cos(ee[0]+ee[2])
    sp = sin(ee[0]+ee[2])
    q = !pi*cp
    qe = [-q, sPhi*cm, sPhi*sm, cPhi*sp]
; next, multiply the two quaternions
    q = [0.D0,Core_quatmult(qr,qe)]  ; to account for 0 start of arrays in IDL
; convert this quaternion back to Euler angles
    q03 = q[1]^2+q[4]^2
    q12 = q[2]^2+q[3]^2
    chi = sqrt(q03*q12)

    if (chi eq 0.D0) then begin
      if (q12 eq 0.D0) then  begin
        Phi = 0.D0
        phi2 = 0.D0   		; arbitrarily due to degeneracy
        phi1 = atan(-2.D0*q[1]*q[4],q[1]^2-q[4]^2)/!dtor
      end else begin
        Phi = !dpi
        phi2 = 0.D0   		; arbitrarily due to degeneracy
        phi1 = atan(2.D0*q[2]*q[3],q[2]^2-q[3]^2)/!dtor
      end 
    end else begin 		; this is not a special degenerate case
      Phi = atan( 2.D0*chi, q03-q12 )/!dtor
      chi = 1.D0/chi
      phi1 = atan( (-q[1]*q[3]+q[2]*q[4])*chi, (-q[1]*q[2]-q[3]*q[4])*chi )/!dtor
      phi2 = atan( (q[1]*q[3]+q[2]*q[4])*chi, (-q[1]*q[2]+q[3]*q[4])*chi )/!dtor
    end 

neweu = [phi1,Phi,phi2]

return,neweu
end

