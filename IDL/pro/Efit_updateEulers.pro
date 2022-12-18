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
; EMsoft:Efit_updateEulers.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_updateEulers.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief update the Euler angles after a pattern center change 
;
;> @date 09/26/22 MDG 1.0 
;--------------------------------------------------------------------------
pro Efit_updateEulers,newx,newy
;
;------------------------------------------------------------
; common blocks
common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common constrainedparameters, c_sv

alpha = !pi*0.5+(Efitdata.dettheta-Efitdata.detMCsig)*!dtor
Lval = Efitdata.detL
dx = (newx - Efitdata.detxpc) * Efitdata.detdelta / Lval
dy = (newy - Efitdata.detypc) * Efitdata.detdelta  / Lval
quat = Core_eu2qu( [Efitdata.detphi1, Efitdata.detphi, Efitdata.detphi2] )
if ((dx eq 0.0) and (dy eq 0.0)) then begin
  quats = quat
end else begin
  rho = sqrt( dx^2+dy^2 )
  n = [dx*cos(alpha),-dy,-dx*sin(alpha)]/rho
  rho = sqrt(1.D0+dx^2+dy^2)
  omega = acos(1.D0/rho)

  qu = Core_ax2qu( [ n[0], n[1], n[2], omega ] )
  quats = float(reform(Core_quatmult(quat,qu),4,1))
endelse

neweu = Core_qu2eu( quats )
Efitdata.detphi1 = neweu[0];/!dtor
Efitdata.detphi = neweu[1];/!dtor
Efitdata.detphi2 = neweu[2];/!dtor

WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]

end
