; ###################################################################
; Copyright (c) 2013-2017, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:MuellerCalculus.f90
;--------------------------------------------------------------------------
;
; PROGRAM: OMMuellerMatrixCalculus
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief routines to generate and handle Mueller matrices and Stokes vectors for polarized light microscopy
;
;> @details Most of the routines in this module are based on the book by Collett:
;> Polarized Light: Fundamentals and Applications, E. Collett, 1993 (M. Decker, Inc)
;
;> @date 02/14/17 MDG 1.0 initial version
;--------------------------------------------------------------------------
;
; this file contains a number of routines for manipulation of Stokes vectors and 
; Mueller matrices; the function names are identical to those in the fortran-90 MuellerCalculus module
; to make it easier to modify things...

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_basicMuellerMatrix
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief returns a basic 4x4 Mueller matrix by type
;
;> @param MMtype integer describing the optical element (0 prduces list)
;
;> @date   02/14/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_basicMuellerMatrix,MMtype

res = {muellermatrixstruct, descriptor:'', M:dblarr(4,4)}

case (MMtype) of 
	0: begin
		print,'The following basic Mueller matrix types are available:'
		print,'1: linear horizontal polarizer'
		print,'2: linear vertical polarizer'
		print,'3: linear polarizer at +45°'
		print,'4: linear polarizer at -45°'
		print,'5: quarter-wave plate, fast axis vertical'
		print,'6: quarter-wave plate, fast axis horizontal'
		print,'7: circular polarizer, right-handed'
		print,'8: circular polarizer, left-handed'
        ; add half wave plate matrices
    end
	1: begin
		res.descriptor = 'linear horizontal polarizer'
        res.M[0,0:3] = [ 1.D0, 1.D0, 0.D0, 0.D0 ]
        res.M[1,0:3] = [ 1.D0, 1.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M = 0.5D0 * res.M
    end
	2: begin
		res.descriptor = 'linear vertical polarizer'
		res.M[0,0:3] = [ 1.D0,-1.D0, 0.D0, 0.D0 ]
        res.M[1,0:3] = [-1.D0, 1.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M = 0.5D0 * res.M
    end
	3: begin
		res.descriptor = 'linear polarizer at +45°'
        res.M[0,0:3] = [ 1.D0, 0.D0, 1.D0, 0.D0 ]
        res.M[1,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 1.D0, 0.D0, 1.D0, 0.D0 ]
        res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M = 0.5D0 * res.M
    end
	4: begin
		res.descriptor = 'linear polarizer at -45°'
        res.M[0,0:3] = [ 1.D0, 0.D0,-1.D0, 0.D0 ]
        res.M[1,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [-1.D0, 0.D0, 1.D0, 0.D0 ]
        res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M = 0.5D0 * res.M
    end
	5: begin
		res.descriptor = 'quarter-wave plate, fast axis vertical'
        res.M[0,0:3] = [ 1.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[1,0:3] = [ 0.D0, 1.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 0.D0, 0.D0, 0.D0,-1.D0 ]
        res.M[3,0:3] = [ 0.D0, 0.D0, 1.D0, 0.D0 ]
    end
	6: begin
		res.descriptor = 'quarter-wave plate, fast axis horizontal'
        res.M[0,0:3] = [ 1.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[1,0:3] = [ 0.D0, 1.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 0.D0, 0.D0, 0.D0, 1.D0 ]
        res.M[3,0:3] = [ 0.D0, 0.D0,-1.D0, 0.D0 ]
    end
	7: begin
		res.descriptor = 'circular polarizer, right-handed'
        res.M[0,0:3] = [ 1.D0, 0.D0, 0.D0, 1.D0 ]
        res.M[1,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[3,0:3] = [ 1.D0, 0.D0, 0.D0, 1.D0 ]
        res.M = 0.5D0 * res.M
    end
	8: begin
		res.descriptor = 'circular polarizer, left-handed'
        res.M[0,0:3] = [ 1.D0, 0.D0, 0.D0, 1.D0 ]
        res.M[1,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[2,0:3] = [ 0.D0, 0.D0, 0.D0, 0.D0 ]
        res.M[3,0:3] = [-1.D0, 0.D0, 0.D0, 1.D0 ]
        res.M = 0.5D0 * res.M
	end
	else: MESSAGE,'requested Mueller matrix type not known'
endcase

return,res
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_diattenuator
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief returns a 4x4 Mueller matrix for a diattenuator (polarizer)
;
;> @param px amplitude attenuation coefficient along x or magnitude of vector p
;> @param py amplitude attenuation coefficient along y or polar angle of vector p
;> @param polar (OPTIONAL) absent/FALSE: cartesian components; TRUE: polar components of px + i py = (p, alpha)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_diattenuator, px, py, polar

res = {muellermatrixstruct, descriptor:'', M:dblarr(4,4)}

; initialize a Mueller matrix for a diattenuator
res.descriptor = 'diattenuator'

usepolar = 0
if (ARG_PRESENT(polar)) then if (polar eq 1) then usepolar = 1

if (usepolar eq 1) then begin
    if ((px lt 0.D0) or (px gt 1.D0)) then begin
        MESSAGE,'MC_get_diattenuator:  attenuation magnitude must lie in range [0,1]'
    endif 
    res.M[0,0:3] = [ 1.D0, cos(2.D0*py), 0.D0, 0.D0 ]
    res.M[1,0:3] = [ cos(2.D0*py), 1.D0, 0.D0, 0.D0 ]
    res.M[2,0:3] = [ 0.D0, 0.D0, sin(2.D0*py), 0.D0 ]
    res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, sin(2.D0*py) ]
    res.M = 0.5D0*px*px*res.M
end else begin
    if ((min([ px, py ]) lt 0.D0) or (max([px, py]) gt 1.D0)) then begin
        MESSAGE,'MC_get_diattenuator: attenuation factors must lie in range [0,1]'
    endif 
    res.M[0,0:3] = [ px*px+py*py, px*px-py*py, 0.D0, 0.D0 ]
    res.M[1,0:3] = [ px*px-py*py, px*px+py*py, 0.D0, 0.D0 ]
    res.M[2,0:3] = [ 0.D0, 0.D0, 2.D0*px*py, 0.D0 ]
    res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, 2.D0*px*py ]
    res.M = 0.5D0*res.M
endelse
    
return,res
end


;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_rotator
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief returns a 4x4 Mueller matrix for a rotator
;
;> @param theta rotator angle (radians)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_rotator,theta

res = {muellermatrixstruct, descriptor:'', M:dblarr(4,4)}

ct = cos(2.D0*theta)
st = sin(2.D0*theta)

; initialize a Mueller matrix for a rotator
res.descriptor = 'rotator'
res.M[0,0:3] = [ 1.D0, 0.D0, 0.D0, 0.D0 ]
res.M[1,0:3] = [ 0.D0, ct, -st, 0.D0 ]
res.M[2,0:3] = [ 0.D0, st, ct, 0.D0 ]
res.M[3,0:3] = [ 0.D0, 0.D0, 0.D0, 1.D0 ]

return,res
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_retarder
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief returns a 4x4 Mueller matrix for a retarder
;
;> @param phi retardation angle (radians)
;
;> @date   02/12/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_retarder,phi

res = {muellermatrixstruct, descriptor:'', M:dblarr(4,4)}

cp = cos(phi)
sp = sin(phi)

; initialize a Mueller matrix for a retarder
res.descriptor = 'retarder'
res.M[0,0:3] = [ 1.D0, 0.D0, 0.D0, 0.D0 ]
res.M[1,0:3] = [ 0.D0, 1.D0, 0.D0, 0.D0 ]
res.M[2,0:3] = [ 0.D0, 0.D0, cp, sp ]
res.M[3,0:3] = [ 0.D0, 0.D0, -sp, cp ]

return,res
end 

;--------------------------------------------------------------------------
;
; FUNCTION: MC_rotate_MuellerMatrix
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @param MM input Mueller matrix 
;> @param theta rotation angle (radians)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_rotate_MuellerMatrix, MM, theta

; initialize the output Mueller matrix descriptor
MM.descriptor = MM.descriptor+'-rotated'

Mrot = MC_get_rotator(theta)

MM.M = transpose(Mrot.M) ## (MM.M ## Mrot.M)

return,MM
end 

;--------------------------------------------------------------------------
;
; SUBROUTINE: MC_print_MuellerMatrix
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @param MM input Mueller matrix
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
pro MC_print_MuellerMatrix,MM

Core_Print,'Mueller Matrix Type : '+MM.descriptor

for i=0,3 do begin
    Core_Print,' --> '+ string(MM.M[i,0])+' '+string(MM.M[i,1])+' '+string(MM.M[i,2])+' '+string(MM.M[i,3])
endfor

end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_propagateStokesVector
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief multiplies a Stokes vector by a Mueller matrix 
;
;> @param MM Mueller matrix structure
;> @param SV Stokes vector structure
;> @param descriptor string to describe the state of the Stokes vector
;
;> @date   02/12/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_propagateStokesVector, MM, SV, descriptor

res = {Stokesvectorstruct, descriptor:'', S:dblarr(4)}

res.S = MM.M ## SV.S
res.descriptor = descriptor

return,res
end

;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
; conversion routines between Stokes vector components and other ellipsometry parameters
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_EllipticityAngle
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief extracts the ellipticity angle from a Stokes vector
;
;> @param SV Stokes vector structure
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_EllipticityAngle, SV

p4 = !dpi * 0.25D0

res = 0.5D0 * asin(SV.S[3]/SV.S[0])

if (abs(res) gt p4) then begin
    Core_Print,'Ellipticity angle = '+string(res,format="(F10.6)")
    Core_Print,'MC_get_EllipticityAngle: Ellipticity angle does not lie in range [-pi/4,pi/4]'
endif

return,res
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_OrientationAngle
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief extracts the polarization ellipse orientation angle from a Stokes vector
;
;> @param SV Stokes vector structure
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_OrientationAngle, SV

res = 0.5D0 * atan(SV.S[2],SV.S[1])

res = (res+2.D0*!dpi) mod !dpi

return, res
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_AuxiliaryAngle
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief extracts the auxiliary angle from a Stokes vector
;
;> @param SV Stokes vector structure
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_AuxiliaryAngle, SV

chi = MC_get_EllipticityAngle(SV)
psi = MC_get_OrientationAngle(SV)
MC_get_AD_from_EO, chi, psi, alpha, delta

return, alpha
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_PhaseShiftAngle
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief extracts the phase shift angle from a Stokes vector
;
;> @param SV Stokes vector structure
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_PhaseShiftAngle, SV

chi = MC_get_EllipticityAngle(SV)
psi = MC_get_OrientationAngle(SV)
MC_get_AD_from_EO, chi, psi, alpha, delta

if (delta lt 0.D0) then delta = (delta+10*!dpi) mod (2.0*!dpi)

return,delta
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_Polarization
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief extracts the polarization from a Stokes vector
;
;> @param SV Stokes vector structure
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_Polarization, SV

if (SV.S[0] eq 0.D0) then begin
  Core_Print,'MC_get_Polarization: Total intensity in Stokes Vector is zero'
  res = 0.0
end else begin
  res = sqrt(total(SV.S[1:3]^2)) / SV.S[0]
endelse

return,res
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_Stokes_EO
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief generate a Stokes vector for a given Ellipticity and Orientation angle
;
;> @param chi ellipticity angle (radians)
;> @param psi orientation angle (radians)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_Stokes_EO, chi, psi, descriptor

res = {Stokesvectorstruct, descriptor:'', S:dblarr(4)}

cp = cos(2.D0*psi)
sp = sin(2.D0*psi)
cc = cos(2.D0*chi)
sc = sin(2.D0*chi)

res.descriptor = descriptor
res.S[0:3] = [ 1.D0, cc*cp, cc*sp, sc ]

return,res
end

;--------------------------------------------------------------------------
;
; FUNCTION: MC_get_Stokes_AD
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief generate a Stokes vector for a given auxiliary and phase shift angle
;
;> @param alpha auxiliary angle (radians)
;> @param delta phase shift angle (radians)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
function MC_get_Stokes_AD, alpha, delta, descriptor

res = {Stokesvectorstruct, descriptor:'', S:dblarr(4)}

ca = cos(2.D0*alpha)
sa = sin(2.D0*alpha)
cd = cos(delta)
sd = sin(delta)

res.descriptor = descriptor
res.S[0:3] = [ 1.D0, ca, sa*cd, sa*sd ]

return,res
end

;--------------------------------------------------------------------------
;
; SUBROUTINE: MC_get_AD_from_EO
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief convert auxiliary and phase shift angle to ellipticity and orientation angles
;
;> @details determined using a Mathematica script
;
;> @param chi ellipticity angle (radians)
;> @param psi orientation angle (radians)
;> @param alpha auxiliary angle (radians)
;> @param delta phase shift angle (radians)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
pro MC_get_AD_from_EO, chi, psi, alpha, delta

p2 = !dpi*0.5D0
p4 = !dpi*0.25D0

cc = cos(2.D0*chi)
sc = sin(2.D0*chi)
tp = tan(2.D0*psi)
cp = cos(2.D0*psi)

st = sqrt(sc*sc+tp*tp)
tt = sqrt(1.D0+tp*tp)
ss = sqrt(1.D0-sc*sc)

ct = cos(2.D0*chi) * tan(2.D0*psi)
sa = sc/abs(cp)

; get alpha
if (abs(psi-p2) ge p4) then begin
; alpha = 0.5D0 * atan(st/tt,ss/tt)
  alpha = 0.5D0 * atan(st,ss)
end else begin
; alpha = 0.5D0 * (!dpi - atan(st/tt,ss/tt))
  alpha = 0.5D0 * (!dpi - atan(st,ss))
endelse

; get delta, such that there is only one cut in the delta surface for chi=0, psi<pi/2
if (abs(psi-p2) lt p4) then begin
;    delta = atan(-sa/st,ct/st)-!dpi
    delta = atan(-sa,ct)-!dpi
end else begin
;   delta = atan(sa/st,ct/st)
    delta = atan(sa,ct)
    if (chi gt 0.D0) then delta = delta - 2.0D0*!dpi
endelse

end ; subroutine MC_get_AD_from_EO


;--------------------------------------------------------------------------
;
; SUBROUTINE: MC_get_EO_from_AD
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief convert ellipticity and orientation angles to auxiliary and phase shift angles
;
;> @details determined using a Mathematica script
;
;> @param alpha auxiliary angle (radians)
;> @param delta phase shift angle (radians)
;> @param chi ellipticity angle (radians)
;> @param psi orientation angle (radians)
;
;> @date   02/15/17 MDG 1.0 original
;--------------------------------------------------------------------------
pro MC_get_EO_from_AD, alpha, delta, chi, psi

p2 = !dpi * 0.5D0
p4 = !dpi * 0.25D0

chi = 0.5D0 * asin ( sin(2.D0 * alpha) * sin(delta))

if (delta le p2) then begin
    psi = 0.5D0 * atan(cos(delta) * tan(2.D0 * alpha))
end else begin
    psi = !dpi - 0.5D0 * atan(cos(delta) * tan(2.D0 * alpha))
endelse

; make sure chi falls in the range [-pi/4,pi/4]
if (abs(chi) gt p4) then begin
    Core_Print,'MC_get_EO_from_AD: ellipticity angle must be in interval [-pi/4,pi/4]'
endif

; make sure psi falls in the range [0,pi]
if (psi lt 0.D0) then psi = (psi + 10.0*!dpi) mod !dpi

end
