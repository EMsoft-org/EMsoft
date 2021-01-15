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
; EMsoft:Efit_navigator_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_navigator_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for Efit_navigator.pro routine
;
;> @date 08/04/16 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro Efit_navigator_event,event

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

applyrotation = -1

if (event.id eq Efitwidget_s.navigatorbase) then begin
  Efitdata.xlocationnavigator = event.x
  Efitdata.ylocationnavigator = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
        'CW': begin
		  applyrotation = 1
		endcase

        'CCW': begin
		  applyrotation = 2
		endcase

        'UP': begin
		  applyrotation = 3
		endcase

        'DOWN': begin
		  applyrotation = 4
		endcase

        'LEFT': begin
		  applyrotation = 5
		endcase

        'RIGHT': begin
		  applyrotation = 6
		endcase

        'ZERO': begin
		  applyrotation = 7
		endcase

        'NAVSTEPSIZE': begin
                  Efitdata.navstepsize = Core_WidgetEvent( Efitwidget_s.navstepsize,  'Navigator step size set to [°] ', '(F6.2)', /flt)
; next, update the rotation quaternions
		  ang = Efitdata.navstepsize * !dtor * 0.5
		  cang = cos(ang)
		  sang = sin(ang)
		  eta = (Efitdata.detMCsig - Efitdata.dettheta) * !dtor
		  delta = !pi*0.5 - eta
		  ceta = cos(eta)
		  seta = sin(eta)
		  cdelta = cos(delta)
		  sdelta = sin(delta)
		  Efitdata.navqx = [ cang, 0.0, sang, 0.0]
		  Efitdata.navqy = [ cang, sang*cdelta, 0.0, -sang*sdelta]
		  Efitdata.navqz = [ cang, sang*ceta, 0.0, sang*seta]
		endcase

  else: MESSAGE, "Efit_navigator_event: Event "+eventval+" Not Found"

  endcase

endelse

; do we need to carry out a rotation ?
if (applyrotation gt 0) then begin
  if (applyrotation eq 7) then begin
; reset the Euler angles to zero
	Efitdata.detphi1 = 0.0
	Efitdata.detphi = 0.0
	Efitdata.detphi2 = 0.0
        WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
        Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
        WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
        Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
        WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
        Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')
	EfitCalc
  end else begin
; convert the current Euler angles to a quaternion
	eu = [ Efitdata.detphi1, Efitdata.detphi, Efitdata.detphi2 ] 
	qu = Core_eu2qu(eu)
; determine the rotation quaternion, depending on which button was pressed
	case applyrotation of
	  1: begin
	       qr = Efitdata.navqz
	  endcase

	  2: begin
	       qr = Efitdata.navqz
	       qr[1:3] *= -1.0
	  endcase

	  3: begin
	       qr = Efitdata.navqx
	  endcase

	  4: begin
	       qr = Efitdata.navqx
	       qr[1:3] *= -1.0
	  endcase

	  5: begin
	       qr = Efitdata.navqy
	       qr[1:3] *= -1.0
	  endcase

	  6: begin
	       qr = Efitdata.navqy
	  endcase
       else: MESSAGE, "Efit_navigator_event: rotation option "+string(applyrotation,format="(I2)")+" not found"
       endcase
; multiply the two quaternions, depending on which button was pressed 
       qnew = Core_quatmult(qu, qr)
; and convert this quaternion back to Euler angles
       eunew = Core_qu2eu(qnew)
; set the new Euler angles in the corresponding widgets
	Efitdata.detphi1 = eunew[0]
	Efitdata.detphi = eunew[1]
	Efitdata.detphi2 = eunew[2]
        WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
        Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
        WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
        Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
        WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
        Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')
; and finally update the EBSD pattern for these new angles
        EfitCalc
  endelse
endif

end

