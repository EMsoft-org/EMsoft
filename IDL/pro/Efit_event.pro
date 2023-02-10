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
; EMsoft:Efit_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for Efit.pro program
;
;> @date 10/13/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro Efit_event,event

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common CommonCore, status, logmode, logunit
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname, expEBSDpattern, EBSDpattern

common Efitdisplaycommon, mask, maskready, expvector


if (event.id eq Efitwidget_s.base) then begin
  Efitdata.xlocation = event.x
  Efitdata.ylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value
  
  CASE eventval OF
        'EXPFILE': begin
; ask the user to select the data file
		Efitgetfilename,validfile,/PATTERNFILE
; start the controller widget (it if isn't already running)
                if (XRegistered("Efit_control") EQ 0) then begin
                  Efit_control
                endif
; then start the actual display widget and put the experimental pattern in it...
; better yet, we can check what the dimensions of the current drawing area are in 
; case the new experimental pattern has different dimensions...
                if (XRegistered("Efit_displaybase") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.displaybase, /DESTROY
                endif 
                Efit_display
                EBSDpattern = replicate(0.0,Efitdata.detnumsx,Efitdata.detnumsy)
; and draw the pattern in the current display mode
                maskready = 0
                Efit_showpattern
                if (XRegistered("Efit_navigatorbase") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.navigatorbase, /DESTROY
                endif 
		Efit_updatePC,/display
		Efit_navigator
        endcase

        'MPFILE': begin
; ask the user to select the data file
		Efitgetfilename,validfile,/MPFILE
                if (validfile eq 1) then begin
                  Efitinit
                  WIDGET_CONTROL, Efitwidget_s.compute, sensitive=1
                  WIDGET_CONTROL, Efitwidget_s.gofit, sensitive=1
                  WIDGET_CONTROL, Efitwidget_s.goconstrainedfit, sensitive=1
                endif
        endcase

        'COMPUTE': begin
                EfitCalc
                WIDGET_CONTROL, Efitwidget_s.mknml5, sensitive=1
                WIDGET_CONTROL, Efitwidget_s.mknml6, sensitive=1
        endcase

        'GOFIT': begin
                if (Efitdata.displayoption eq 5) then begin
                  Efitdata.displayoption = 4
                  WIDGET_CONTROL, set_value=Efitdata.displayoption, Efitwidget_s.displayoption
                endif
                Efit_fit
        endcase

        'GOCONSTRAINEDFIT': begin
                if (Efitdata.displayoption eq 5) then begin
                  Efitdata.displayoption = 4
                  WIDGET_CONTROL, set_value=Efitdata.displayoption, Efitwidget_s.displayoption
                endif
                Efit_constrainedfit,'NelderMead'
        endcase

 	'QUIT': begin
		Efitwritepreferences
; do a general cleanup of potentially open widgets
                if (XRegistered("Efit_control") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.controlbase, /DESTROY
                endif
                if (XRegistered("Efit_display") NE 0) then begin
		  WIDGET_CONTROL, Efitwidget_s.displaybase, /DESTROY
                endif
                if (XRegistered("Efit_navigator") NE 0) then begin
      WIDGET_CONTROL, Efitwidget_s.navigatorbase, /DESTROY
                endif

 		Core_Print,'Quitting program',/blank
                wait,1.0
		WIDGET_CONTROL, Efitwidget_s.base, /DESTROY
		!EXCEPT=1
	endcase

; some image processing parameters
        'HIPASSCUTOFF' : begin
                Efitdata.hipasscutoff = Core_WidgetEvent( Efitwidget_s.hipasscutoff,  'hipass cut off set to ', '(F9.2)', /flt)
                if (Efitdata.hipasscutoff gt 0.5) then begin
                  Efitdata.hipasscutoff = 0.5
                  Core_Print,'Hipass filter cutoff should be smaller than 0.5'
                endif
                if (Efitdata.hipasscutoff lt 0.0) then begin
                  Efitdata.hipasscutoff = 0.0
                  Core_Print,'Hipass filter cutoff should be positive'
                endif
                Efit_showpattern
        endcase

        'MKNML5' : begin
; write an EMsoft 5.2 EMEBSDDI.nml name list file with name efit_EMEBSDDI.nml
                openw,1,Efitdata.pathname+'/efit_EMEBSDDI.nml'
                printf,1,'&EBSDIndexingdata'
                printf,1,'indexingmode = ''dynamic'','
                printf,1,'similaritymetric = ''ndp'','
                printf,1,'notify = ''off'','
                printf,1,'ipf_wd = 100,'
                printf,1,'ipf_ht = 100,'
                printf,1,'ROI = 0 0 0 0,'
                printf,1,'stepx = 1.0,'
                printf,1,'stepy = 1.0,'
                printf,1,'nnk = 50,'
                printf,1,'nosm = 20,'
                printf,1,'nism = 5,'
                printf,1,'isangle = 1.5,'
                printf,1,'maskfile = ''undefined'','
                printf,1,'maskpattern = ''n'','
                printf,1,'maskradius = 240,'
                printf,1,'hipassw = 0.05,'
                printf,1,'nregions = 10,'
                printf,1,'exptnumsx = '+string(efitdata.detnumsx,format="(I5)")+','
                printf,1,'exptnumsy = '+string(efitdata.detnumsy,format="(I5)")+','
                printf,1,'exptnumsy = 480,'
                printf,1,'binning = 1, '
                printf,1,'numsx = '+string(efitdata.detnumsx,format="(I5)")+','
                printf,1,'numsy = '+string(efitdata.detnumsy,format="(I5)")+','
                printf,1,'ncubochoric = 100,'
                printf,1,'thetac = '+string(efitdata.dettheta,format="(F5.1)")+','
                printf,1,'omega = '+string(efitdata.detomega,format="(F5.1)")+','
                printf,1,'L = '+string(efitdata.detL,format="(F12.5)")+','
                printf,1,'delta = '+string(efitdata.detdelta,format="(F12.5)")+','
                printf,1,'xpc = '+string(efitdata.detxpc,format="(F12.5)")+','
                printf,1,'ypc = '+string(efitdata.detypc,format="(F12.5)")+','
                printf,1,'energymin = '+string(EkeV - float(numebins-1)*ebinsize,format="(F12.5)")+','
                printf,1,'energymax = '+string(EkeV,format="(F12.5)")+','
                printf,1,'beamcurrent = '+string(efitdata.detbeamcurrent,format="(F12.5)")+','
                printf,1,'dwelltime = '+string(efitdata.detdwelltime,format="(F12.5)")+','
                printf,1,'scalingmode = ''gam'','
                printf,1,'gammavalue = '+string(efitdata.detgamma,format="(F12.5)")+','
                printf,1,'exptfile = ''undefined'','
                printf,1,'inputtype = ''binary'','
                printf,1,'hdfstrings = '''' '''' '''' '''' '''' '''' '''' '''' '''' '''' ,'
                printf,1,'tmpfile = ''EMEBSDdict_tmp.data'','
                printf,1,'keeptmpfile = ''n'','
                printf,1,'usetmpfile = ''n'','
                printf,1,'datafile = ''undefined'','
                printf,1,'ctffile = ''undefined'','
                printf,1,'angfile = ''undefined'','
                printf,1,'eulerfile = ''undefined'',
                printf,1,'dictfile = ''undefined'','
                printf,1,'masterfile = '''+strtrim(Efitdata.mpfilename)+''','
                printf,1,'refinementnmlfile = ''undefined'','
                printf,1,'numdictsingle = 1024,'
                printf,1,'numexptsingle = 1024,'
                printf,1,'nthreads = 1,'
                printf,1,'platid = 1,'
                printf,1,'devid = 1,'
                printf,1,'multidevid = 0 0 0 0 0 0 0 0,'
                printf,1,'usenumd = 0,'
                printf,1,'/'
                close,1
                core_print,'fit data stored in EMsoft 5.2 name list file efit_EMEBSDDI.nml'
        endcase

        'MKNML6' : begin
; write an EMsoftOO 6.0 EMDI.nml name list file with name efit_EMDI.nml
                openw,1,Efitdata.pathname+'/efit_EMDI.nml'
                printf,1,'&DIdata'
                printf,1,'indexingmode = ''dynamic'','
                printf,1,'similaritymetric = ''ndp'','
                printf,1,'notify = ''off'','
                printf,1,'ipf_wd = 100,'
                printf,1,'ipf_ht = 100,'
                printf,1,'ROI = 0 0 0 0,'
                printf,1,'stepx = 1.0,'
                printf,1,'stepy = 1.0,'
                printf,1,'nnk = 50,'
                printf,1,'nosm = 20,'
                printf,1,'nism = 5,'
                printf,1,'isangle = 1.5,'
                printf,1,'maskfile = ''undefined'','
                printf,1,'maskpattern = ''n'','
                printf,1,'maskradius = 240,'
                printf,1,'hipassw = 0.05,'
                printf,1,'nregions = 10,'
                printf,1,'exptnumsx = '+string(efitdata.detnumsx,format="(I5)")+','
                printf,1,'exptnumsy = '+string(efitdata.detnumsy,format="(I5)")+','
                printf,1,'exptnumsy = 480,'
                printf,1,'binning = 1, '
                printf,1,'numsx = '+string(efitdata.detnumsx,format="(I5)")+','
                printf,1,'numsy = '+string(efitdata.detnumsy,format="(I5)")+','
                printf,1,'doNLPAR = .FALSE.,'
                printf,1,'sw = 3,'
                printf,1,'lambda = 0.375,'
                printf,1,'ncubochoric = 100,'
                printf,1,'thetac = '+string(efitdata.dettheta,format="(F5.1)")+','
                printf,1,'omega = '+string(efitdata.detomega,format="(F5.1)")+','
                printf,1,'L = '+string(efitdata.detL,format="(F12.5)")+','
                printf,1,'delta = '+string(efitdata.detdelta,format="(F12.5)")+','
                printf,1,'xpc = '+string(efitdata.detxpc,format="(F12.5)")+','
                printf,1,'ypc = '+string(efitdata.detypc,format="(F12.5)")+','
                printf,1,'energymin = '+string(EkeV - float(numebins-1)*ebinsize,format="(F12.5)")+','
                printf,1,'energymax = '+string(EkeV,format="(F12.5)")+','
                printf,1,'beamcurrent = '+string(efitdata.detbeamcurrent,format="(F12.5)")+','
                printf,1,'dwelltime = '+string(efitdata.detdwelltime,format="(F12.5)")+','
                printf,1,'scalingmode = ''gam'','
                printf,1,'gammavalue = '+string(efitdata.detgamma,format="(F12.5)")+','
                printf,1,'exptfile = ''undefined'','
                printf,1,'inputtype = ''binary'','
                printf,1,'hdfstrings = '''' '''' '''' '''' '''' '''' '''' '''' '''' '''' ,'
                printf,1,'tmpfile = ''EMEBSDdict_tmp.data'','
                printf,1,'keeptmpfile = ''n'','
                printf,1,'usetmpfile = ''n'','
                printf,1,'datafile = ''undefined'','
                printf,1,'ctffile = ''undefined'','
                printf,1,'angfile = ''undefined'','
                printf,1,'eulerfile = ''undefined'',
                printf,1,'dictfile = ''undefined'','
                printf,1,'masterfile = '''+strtrim(Efitdata.mpfilename)+''','
                printf,1,'refinementnmlfile = ''undefined'','
                printf,1,'numdictsingle = 1024,'
                printf,1,'numexptsingle = 1024,'
                printf,1,'nthreads = 1,'
                printf,1,'platid = 1,'
                printf,1,'devid = 1,'
                printf,1,'multidevid = 0 0 0 0 0 0 0 0,'
                printf,1,'usenumd = 0,'
                printf,1,'/'
                close,1
                Core_print,'fit data stored in EMsoftOO 6.0 name list file efit_EMEBSDDI.nml'
        endcase

; next are the non-refinable parameter widgets
        'DETTHETA' : begin
                Efitdata.dettheta = Core_WidgetEvent( Efitwidget_s.dettheta,  'Detector tilt angle set to [degree] ', '(F9.2)', /flt)
                EfitCalc
	endcase
        'DETDELTA' : begin
                Efitdata.detdelta = Core_WidgetEvent( Efitwidget_s.detdelta,  'Scintillator pixel size set to [micron] ', '(F9.2)', /flt)
		Efit_updatePC,/display
                EfitCalc
	endcase
        'BEAMCURRENT' : begin
                Efitdata.detbeamcurrent = Core_WidgetEvent( Efitwidget_s.detbeamcurrent,  'Beam current set to [nA] ', '(F9.2)', /flt)
                EfitCalc
	endcase
        'DWELLTIME' : begin
                Efitdata.detdwelltime = Core_WidgetEvent( Efitwidget_s.detdwelltime,  'Dwell time set to [mu s] ', '(F9.2)', /flt)
                EfitCalc
	endcase


;fitUserLabel = ['DETL','DETOMEGA','DETXPC','DETYPC','DETGAMMA','DETphi1','DETphi','DETphi2','DETtheta']]
        'DETL' : begin
                Efitdata.detL = Core_WidgetEvent( Efitwidget_s.fitValue[0],  'Scintillator distance set to [micron] ', '(F9.2)', /flt)
		Efit_updatePC,/display
	endcase
        'DETOMEGA' : begin
                Efitdata.detomega = Core_WidgetEvent( Efitwidget_s.fitValue[1],  'Sample omega angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETXPC' : begin
                newx = Core_WidgetEvent( Efitwidget_s.fitValue[2],  'Pattern center x set to [pixels] ', '(F9.2)', /flt)
                Efitdata.detxpc = newx
; we need to update the Euler angles to undo this shift in terms of a rotation, so that
; the pattern itself stays roughly stationary
                Efit_updateEulers, newx, Efitdata.detypc
                Efit_updatePC,/display
	endcase
        'DETYPC' : begin
               newy = Core_WidgetEvent( Efitwidget_s.fitValue[3],  'Pattern center y set to [pixels] ', '(F9.2)', /flt)
                Efitdata.detypc = newy
; we need to update the Euler angles to undo this shift in terms of a rotation, so that
; the pattern itself stays roughly stationary
                Efit_updateEulers, Efitdata.detxpc, newy
                Efit_updatePC,/display
	endcase
        'DETGAMMA' : begin
                Efitdata.detgamma = Core_WidgetEvent( Efitwidget_s.fitValue[4],  'Intensity gammma value set to ', '(F9.2)', /flt)
	endcase
        'DETphi1' : begin
                Efitdata.detphi1 = Core_WidgetEvent( Efitwidget_s.fitValue[5],  'Euler phi1 angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETphi' : begin
                Efitdata.detphi = Core_WidgetEvent( Efitwidget_s.fitValue[6],  'Euler Phi angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETphi2' : begin
                Efitdata.detphi2 = Core_WidgetEvent( Efitwidget_s.fitValue[7],  'Euler phi2 angle set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETtheta' : begin
                Efitdata.dettheta = Core_WidgetEvent( Efitwidget_s.fitValue[8],  'Detector tilt angle set to [degrees] ', '(F9.2)', /flt)
; next, update the rotation quaternions for navigation
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

;fitStepLabel = ['DETsL','DETsOMEGA','DETsXPC','DETsYPC','DETsGAMMA','DETsphi1','DETsphi','DETsphi2','DETstheta']
        'DETsL' : begin
                Efitdata.detsL = Core_WidgetEvent( Efitwidget_s.fitStep[0],  'Scintillator distance step size set to [micron] ', '(F9.2)', /flt)
	endcase
        'DETsOMEGA' : begin
                Efitdata.detsomega = Core_WidgetEvent( Efitwidget_s.fitStep[1],  'Sample omega angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETsXPC' : begin
                Efitdata.detsxpc = Core_WidgetEvent( Efitwidget_s.fitStep[2],  'Pattern center x step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETsYPC' : begin
                Efitdata.detsypc = Core_WidgetEvent( Efitwidget_s.fitStep[3],  'Pattern center y step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETsGAMMA' : begin
                Efitdata.detsgamma = Core_WidgetEvent( Efitwidget_s.fitStep[4],  'Intensity gammma step size value set to ', '(F9.2)', /flt)
	endcase
        'DETsphi1' : begin
                Efitdata.detsphi1 = Core_WidgetEvent( Efitwidget_s.fitStep[5],  'Euler phi1 angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETsphi' : begin
                Efitdata.detsphi = Core_WidgetEvent( Efitwidget_s.fitStep[6],  'Euler Phi angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETsphi2' : begin
                Efitdata.detsphi2 = Core_WidgetEvent( Efitwidget_s.fitStep[7],  'Euler phi2 angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETstheta' : begin
                Efitdata.detstheta = Core_WidgetEvent( Efitwidget_s.fitStep[8],  'Detector tilt angle step size set to [degrees] ', '(F9.2)', /flt)
	endcase

;fitOnOffLabel = ['DEToL','DEToOMEGA','DEToXPC','DEToYPC','DEToGAMMA','DETophi1','DETophi','DETophi2','DETotheta']
; these are dealt with in Efitevent.pro

;fitUpLabel = ['DETuL','DETuOMEGA','DETuXPC','DETuYPC','DETuGAMMA','DETuphi1','DETuphi','DETuphi2','DETutheta']
        'DETuL' : begin
                Efitdata.detL += float(Efitdata.detmL)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detL,FORMAT='(F9.2)'), Efitwidget_s.fitValue[0]
                Core_Print, 'Scintillator distance set to [micron] '+string(Efitdata.detL,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETuOMEGA' : begin
                Efitdata.detomega += float(Efitdata.detmomega)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detomega,FORMAT='(F9.2)'), Efitwidget_s.fitValue[1]
                Core_Print, 'Sample omega angle set to [degree] '+string(Efitdata.detomega,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETuXPC' : begin
                Efit_updateEulers, Efitdata.detxpc+Efitdata.detmxpc, Efitdata.detypc
                Efitdata.detxpc += float(Efitdata.detmxpc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detxpc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[2]
                Core_Print, 'Pattern center x set to [pixels] '+string(Efitdata.detxpc,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETuYPC' : begin
                Efit_updateEulers, Efitdata.detxpc, Efitdata.detypc+Efitdata.detmypc
                Efitdata.detypc += float(Efitdata.detmypc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detypc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[3]
                Core_Print, 'Pattern center y set to [pixels] '+string(Efitdata.detypc,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETuGAMMA' : begin
                Efitdata.detgamma += float(Efitdata.detmgamma)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detgamma,FORMAT='(F9.2)'), Efitwidget_s.fitValue[4]
                Core_Print, 'Intensity gamma value set to '+string(Efitdata.detgamma,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETuphi1' : begin
                Efitdata.detphi1 += float(Efitdata.detmphi1)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
                Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETuphi' : begin
                Efitdata.detphi += float(Efitdata.detmphi)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
                Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETuphi2' : begin
                Efitdata.detphi2 += float(Efitdata.detmphi2)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
                Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETutheta' : begin
                Efitdata.dettheta += float(Efitdata.detmtheta)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.dettheta,FORMAT='(F9.2)'), Efitwidget_s.fitValue[8]
                Core_Print, 'Detector tilt angle set to '+string(Efitdata.dettheta,FORMAT='(F9.2)')
                EfitCalc
	endcase

;fitDownLabel = ['DETdL','DETdOMEGA','DETdXPC','DETdYPC','DETdGAMMA','DETdphi1','DETdphi','DETdphi2','DETdtheta']
        'DETdL' : begin
                Efitdata.detL -= float(Efitdata.detmL)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detL,FORMAT='(F9.2)'), Efitwidget_s.fitValue[0]
                Core_Print, 'Scintillator distance set to [micron] '+string(Efitdata.detL,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETdOMEGA' : begin
                Efitdata.detomega -= float(Efitdata.detmomega)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detomega,FORMAT='(F9.2)'), Efitwidget_s.fitValue[1]
                Core_Print, 'Sample omega angle set to [degree] '+string(Efitdata.detomega,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETdXPC' : begin
                Efit_updateEulers, Efitdata.detxpc-Efitdata.detmxpc, Efitdata.detypc
                Efitdata.detxpc -= float(Efitdata.detmxpc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detxpc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[2]
                Core_Print, 'Pattern center x set to [pixels] '+string(Efitdata.detxpc,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETdYPC' : begin
                Efit_updateEulers, Efitdata.detxpc, Efitdata.detypc-Efitdata.detmypc 
                Efitdata.detypc -= float(Efitdata.detmypc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detypc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[3]
                Core_Print, 'Pattern center y set to [pixels] '+string(Efitdata.detypc,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETdGAMMA' : begin
                Efitdata.detgamma -= float(Efitdata.detmgamma)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detgamma,FORMAT='(F9.2)'), Efitwidget_s.fitValue[4]
                Core_Print, 'Intensity gamma value set to '+string(Efitdata.detgamma,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETdphi1' : begin
                Efitdata.detphi1 -= float(Efitdata.detmphi1)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi1,FORMAT='(F9.2)'), Efitwidget_s.fitValue[5]
                Core_Print, 'Euler angle phi1 set to '+string(Efitdata.detphi1,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETdphi' : begin
                Efitdata.detphi -= float(Efitdata.detmphi)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi,FORMAT='(F9.2)'), Efitwidget_s.fitValue[6]
                Core_Print, 'Euler angle phi set to '+string(Efitdata.detphi,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETdphi2' : begin
                Efitdata.detphi2 -= float(Efitdata.detmphi2)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detphi2,FORMAT='(F9.2)'), Efitwidget_s.fitValue[7]
                Core_Print, 'Euler angle phi2 set to '+string(Efitdata.detphi2,FORMAT='(F9.2)')
                EfitCalc
	endcase
        'DETdtheta' : begin
                Efitdata.dettheta-= float(Efitdata.detmtheta)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.dettheta,FORMAT='(F9.2)'), Efitwidget_s.fitValue[8]
                Core_Print, 'Detector tilt angle set to '+string(Efitdata.dettheta,FORMAT='(F9.2)')
                EfitCalc
	endcase

;fitManualStepLabel = ['DETmL','DETmOMEGA','DETmXPC','DETmYPC','DETmGAMMA','DETmphi1','DETmphi','DETmphi2','DETmtheta']
        'DETmL' : begin
                Efitdata.detmL = Core_WidgetEvent( Efitwidget_s.fitManualStep[0],  'Scintillator distance manual step size set to [micron] ', '(F9.2)', /flt)
	endcase
        'DETmOMEGA' : begin
                Efitdata.detmomega = Core_WidgetEvent( Efitwidget_s.fitManualStep[1],  'Sample omega angle manual step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmXPC' : begin
                Efitdata.detmxpc = Core_WidgetEvent( Efitwidget_s.fitManualStep[2],  'Pattern center x manual step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETmYPC' : begin
                Efitdata.detmypc = Core_WidgetEvent( Efitwidget_s.fitManualStep[3],  'Pattern center y manual step size set to [pixels] ', '(F9.2)', /flt)
	endcase
        'DETmGAMMA' : begin
                Efitdata.detmgamma = Core_WidgetEvent( Efitwidget_s.fitManualStep[4],  'Intensity gammma manual step size value set to ', '(F9.2)', /flt)
	endcase
        'DETmphi1' : begin
                Efitdata.detmphi1 = Core_WidgetEvent( Efitwidget_s.fitManualStep[5],  'Euler phi1 angle manual step size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmphi' : begin
                Efitdata.detmphi = Core_WidgetEvent( Efitwidget_s.fitManualStep[6],  'Euler Phi angle step manual size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmphi2' : begin
                Efitdata.detmphi2 = Core_WidgetEvent( Efitwidget_s.fitManualStep[7],  'Euler phi2 angle step manual size set to [degrees] ', '(F9.2)', /flt)
	endcase
        'DETmtheta' : begin
                Efitdata.detmtheta = Core_WidgetEvent( Efitwidget_s.fitManualStep[8],  'Detector tilt angle step manual size set to [degrees] ', '(F9.2)', /flt)
	endcase

  else: MESSAGE, "Efit_event: Event User Step "+eventval+" Not Found"

  endcase


endelse

end
