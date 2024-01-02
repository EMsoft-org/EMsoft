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
; EMsoft:Efit_correction.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_correction.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Applies an orientation correction to compensate for a pattern center change 
;
;> @date 6/24/22 MDG 1.0 separate routine for orientation correction after PC change
;--------------------------------------------------------------------------
pro Efit_correction, ldx, ldy, eu

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

; compute the correction quaternion based on the pattern center shifts w.r.t. the original pattern center
alpha = !pi*0.5+(Efitdata.dettheta-Efitdata.detMCsig)*!dtor
dx = -ldx * Efitdata.detdelta
dy = -ldy * Efitdata.detdelta
quat = Core_eu2qu( eu )
Lval = Efitdata.detL

; compute correction axis and angle
rho = sqrt( dx^2+(dy*cos(2.D0*alpha))^2)
n = [-dx*cos(alpha),-dy*cos(2.D0*alpha),dx*sin(alpha)]/rho
rho = sqrt(Lval^2 + 2.D0*Lval*dy*sin(2.0*alpha)+dx^2+dy^2)
if ((Lval+dy*sin(2.D0*alpha)) gt rho) then omega = 0.0 else omega = acos((Lval+dy*sin(2.D0*alpha))/rho)

; apply the correction to the orientation quaternion
qu = Core_ax2qu( [ n[0], n[1], n[2], omega ] )
quats = float(reform(Core_quatmult(quat,qu),4,1))

; convert quaternion to Euler angles and update widgets
eu = Core_qu2eu(quats)
Efitdata.detphi1 = eu[0]
Efitdata.detphi  = eu[1]
Efitdata.detphi2 = eu[2]

WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')

end