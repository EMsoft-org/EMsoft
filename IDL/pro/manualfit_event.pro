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
; EMsoft:manualfit_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: manualfit_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief event handler for manual pattern fit
;
;> @date 04/10/17 MDG 1.0 initial version
;--------------------------------------------------------------------------
pro manualfit_event, event

;------------------------------------------------------------
; common blocks
common FIT_widget_common, FITwidget_s
common FIT_data_common, FITdata

applyrotation = -1
patsel = -1

; used for debugging purposes
if (FITdata.eventverbose eq 1) then help,event,/structure

; there are only two different events: either the window has been moved or 
; some button or something has been activated inside the window
if (event.id eq FITwidget_s.base) then begin
  FITdata.xlocation = event.x
  FITdata.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  

  CASE eventval OF
  	'LEFT1': begin
  				applyrotation = 5
  				patsel = 1
	         endcase

  	'LEFT2': begin
  				applyrotation = 5
  				patsel = 2
	         endcase

  	'LEFT3': begin
  				applyrotation = 5
  				patsel = 3
	         endcase

  	'LEFT4': begin
  				applyrotation = 5
		  		patsel = 4
	         endcase

  	'UP1': begin
  				applyrotation = 3
  				patsel = 1
	         endcase

  	'UP2': begin
  				applyrotation = 3
  				patsel = 2
	         endcase

  	'UP3': begin
  				applyrotation = 3
  				patsel = 3
	         endcase

  	'UP4': begin
  				applyrotation = 3
  				patsel = 4
	         endcase

  	'DOWN1': begin
  				applyrotation = 4
  				patsel = 1
	         endcase

  	'DOWN2': begin
  				applyrotation = 4
  				patsel = 2
	         endcase

  	'DOWN3': begin
  				applyrotation = 4
  				patsel = 3
	         endcase

  	'DOWN4': begin
  				applyrotation = 4
  				patsel = 4
	         endcase

  	'RIGHT1': begin				
  				applyrotation = 6
  				patsel = 1
	         endcase

  	'RIGHT2': begin
  				applyrotation = 6
  				patsel = 2
	         endcase

  	'RIGHT3': begin
  				applyrotation = 6
  				patsel = 3
	         endcase

  	'RIGHT4': begin
  				applyrotation = 6
  				patsel = 4
	         endcase

  	'CW1': begin
  				applyrotation = 1
  				patsel = 1
	         endcase

  	'CW2': begin
  				applyrotation = 1
  				patsel = 2
	         endcase

  	'CW3': begin
  				applyrotation = 1
  				patsel = 3
	         endcase

  	'CW4': begin
  				applyrotation = 1
  				patsel = 4
	         endcase

  	'CCW1': begin
  				applyrotation = 2
  				patsel = 1
	         endcase

  	'CCW2': begin
  				applyrotation = 2
  				patsel = 2
	         endcase

  	'CCW3': begin
  				applyrotation = 2
  				patsel = 3
	         endcase

  	'CCW4': begin
  				applyrotation = 2
  				patsel = 4
	         endcase

  	'ZERO1': begin
  				applyrotation = 7
  				patsel = 1
	         endcase

  	'ZERO2': begin
  				applyrotation = 7
  				patsel = 2
	         endcase

  	'ZERO3': begin
  				applyrotation = 7
  				patsel = 3
	         endcase

  	'ZERO4': begin
  				applyrotation = 7
  				patsel = 4
	         endcase

  	'XPCUP': begin
                FITdata.xpc += FITdata.xpcstepsize
		        manualfit_calc,-1
	         endcase

  	'XPCDOWN': begin
                FITdata.xpc -= FITdata.xpcstepsize
		        manualfit_calc,-1
	         endcase

  	'XPCSTEPSIZE': begin
                FITdata.xpcstepsize = Core_WidgetEvent( FITwidget_s.xpcstepsize,  'xpc step size set to ', '(F6.2)', /flt)
	         endcase

  	'YPCUP': begin
                FITdata.ypc += FITdata.ypcstepsize
		        manualfit_calc,-1
	         endcase

  	'YPCDOWN': begin
                FITdata.ypc -= FITdata.ypcstepsize
		        manualfit_calc,-1
	         endcase

  	'YPCSTEPSIZE': begin
                FITdata.ypcstepsize = Core_WidgetEvent( FITwidget_s.ypcstepsize,  'ypc step size set to ', '(F6.2)', /flt)
	         endcase

  	'LUP': begin
                FITdata.L += FITdata.Lstepsize
		        manualfit_calc,-1
	         endcase

  	'LDOWN': begin
                FITdata.L -= FITdata.Lstepsize
		        manualfit_calc,-1
	         endcase

  	'LSTEPSIZE': begin
                FITdata.Lstepsize = Core_WidgetEvent( FITwidget_s.Lstepsize,  'L step size set to ', '(F6.2)', /flt)
	         endcase

    'NAVSTEPSIZE': begin
                  FITdata.navstepsize = Core_WidgetEvent( FITwidget_s.navstepsize,  'Navigator step size set to [°] ', '(F6.2)', /flt)
; next, update the rotation quaternions
				  ang = FITdata.navstepsize * !dtor * 0.5
				  cang = cos(ang)
				  sang = sin(ang)
				  eta = (FITdata.detMCsig - FITdata.dettheta) * !dtor
				  delta = !pi*0.5 - eta
				  ceta = cos(eta)
				  seta = sin(eta)
				  cdelta = cos(delta)
				  sdelta = sin(delta)
				  FITdata.navqx = [ cang, 0.0, sang, 0.0]
				  FITdata.navqy = [ cang, sang*cdelta, 0.0, -sang*sdelta]
				  FITdata.navqz = [ cang, sang*ceta, 0.0, sang*seta]
		endcase


  	'QUIT': begin
		   WIDGET_CONTROL, FITwidget_s.base, /DESTROY
	         endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

print, 'applyrotation, patsel : ',applyrotation, patsel

; do we need to carry out a rotation ?
if (applyrotation gt 0) then begin
  if (applyrotation eq 7) then begin
; reset the Euler angles to their initial values
	FITdata.phi1[patsel] = FITdata.phi1init[patsel]
	FITdata.phi[patsel] = FITdata.phiinit[patsel]
	FITdata.phi2[patsel] = FITdata.phi2init[patsel]
;       Core_Print, 'Euler angle phi1 set to '+string(FITdata.phi1[patsel],FORMAT='(F9.2)')
;       Core_Print, 'Euler angle phi set to '+string(FITdata.phi[patsel],FORMAT='(F9.2)')
;       Core_Print, 'Euler angle phi2 set to '+string(FITdata.phi2[patsel],FORMAT='(F9.2)')
  end else begin
; convert the current Euler angles to a quaternion
	eu = [ FITdata.phi1[patsel], FITdata.phi[patsel], FITdata.phi2[patsel] ] 
	qu = Core_eu2qu(eu)
; determine the rotation quaternion, depending on which button was pressed
	case applyrotation of
	  1: begin
	       qr = FITdata.navqz
	  endcase

	  2: begin
	       qr = FITdata.navqz
	       qr[1:3] *= -1.0
	  endcase

	  3: begin
	       qr = FITdata.navqx
	  endcase

	  4: begin
	       qr = FITdata.navqx
	       qr[1:3] *= -1.0
	  endcase

	  5: begin
	       qr = FITdata.navqy
	       qr[1:3] *= -1.0
	  endcase

	  6: begin
	       qr = FITdata.navqy
	  endcase
       else: MESSAGE, "manualfit_event: rotation option "+string(applyrotation,format="(I2)")+" not found"
       endcase
       print,'quaternion = ', qr
; multiply the two quaternions, depending on which button was pressed 
       qnew = Core_quatmult(qu, qr)
; and convert this quaternion back to Euler angles
       eunew = Core_qu2eu(qnew)
       print,'new eulers : ', eunew
; set the new Euler angles in the corresponding widgets
	FITdata.phi1[patsel] = eunew[0]
	FITdata.phi[patsel] = eunew[1]
	FITdata.phi2[patsel] = eunew[2]
;       Core_Print, 'Euler angle phi1 set to '+string(FITdata.phi1[patsel],FORMAT='(F9.2)')
;       Core_Print, 'Euler angle phi set to '+string(FITdata.phi[patsel],FORMAT='(F9.2)')
;       Core_Print, 'Euler angle phi2 set to '+string(FITdata.phi2[patsel],FORMAT='(F9.2)')
; and finally update the selected EBSD pattern for these new angles and compute the fit quality parameter
        manualfit_calc,patsel
  endelse
endif

end 

