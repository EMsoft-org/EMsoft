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
;
; Function: eu2qu
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Euler angles to quaternion [Morawiec, page 40]
;
;> @note verified 8/5/13
;
;> @param e 3 Euler angles in radians 
; 
;> @date 08/04/13 MDG 1.0 original
;> @date 08/07/14 MDG 1.1 verified
;> @date 03/11/15 MDG 2.0 IDL version
;--------------------------------------------------------------------------
function Core_eu2qu,e

;common rotationcommon, LPs, epsijk
epsijk = 1.0

ee = 0.5*e*!dtor

cPhi = cos(ee[1])
sPhi = sin(ee[1])
cm = cos(ee[0]-ee[2])
sm = sin(ee[0]-ee[2])
cp = cos(ee[0]+ee[2])
sp = sin(ee[0]+ee[2])

; passive quaternion
res = [ cPhi*cp, -epsijk*sPhi*cm, -epsijk*sPhi*sm, -epsijk*cPhi*sp ]

; first component must be positive
if (res[0] lt 0.D0) then res = -res

return,res
end ;function Core_eu2qu
