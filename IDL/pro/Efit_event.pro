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
                WIDGET_CONTROL, Efitwidget_s.mkjson, sensitive=1
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

        'MKJSON' : begin
; here we first make a structure that has all the f90 namelist entries in it, and then we 
; use implied print into a json file...
 		Efitgetfilename,validfile,/JSONFILE
 		Efitgetfilename,validfile,/EULERFILE
; some of the string variables need to have the correct path inserted, since files in EMsoft have relative 
; pathnames with respect to fitdata.EMdatapathname; give a warning if the path is not found in the absolute path 
                eulerconvention = ['tsl', 'hkl']
                maskpattern = ['n','y']

; remove the EMdatapathname portion of the euler angle file path
                z=strsplit(Efitdata.EMdatapathname,'/',/extract)
                sz = size(z,/dimensions)
                last = z[sz[0]-1]
                z2 = strsplit(Efitdata.eulerpathname,last,/regex,/ex)
                sz = size(z2,/dimensions)
                if (sz[0] ne 2) then begin
                  message = ['EMdatapathname does not appear to be a part of the Euler angle file name', $
                             'In the EMsoft package, all file names must be relative to '+Efitdata.EMdatapathname+'.', $
                             'The file will be created at the requested location with the requested name,', $
                             'but the EMsoft package will not be able to find it.  Please move the Euler angle file', $
                             'to a location inside the accessible path, and update the corresponding json file to reflect this path']
                  result = DIALOG_MESSAGE(message, DIALOG_PARENT=Efitwidget_s.base)
                  eulerpath = Efitdata.eulerpathname+'/'+Efitdata.eulerfilename
                end else begin
                  z3 = z2[sz[0]-1]
                  eulerpath = strmid(z3,1)+'/'+Efitdata.eulerfilename
                endelse

; and do the same for the masterpattern and energy files
                z2 = strsplit(Efitdata.pathname,last,/regex,/ex)
                sz = size(z2,/dimensions)
                if (sz[0] ne 2) then begin
                  message = ['EMdatapathname does not appear to be a part of the master pattern file name', $
                             'In the EMsoft package, all file names must be relative to '+Efitdata.EMdatapathname+'.', $
                             'The file will be created at the requested location with the requested name,', $
                             'but the EMsoft package will not be able to find it.  Please make sure that the master file', $
                             'is located inside the accessible path, and update the corresponding json file to reflect this path']
                  result = DIALOG_MESSAGE(message, DIALOG_PARENT=Efitwidget_s.base)
                  masterpath = Efitdata.pathname+'/'+Efitdata.mpfilename
                end else begin
                  z3 = z2[sz[0]-1]
                  masterpath = strmid(z3,1)+'/'+Efitdata.mpfilename
                endelse

; define the SEMdata structure that will be written to the JSON file
                SEMdata = {ebsddatastructure, $
                        L : Efitdata.detL, $ 
                        thetac : Efitdata.dettheta, $ 
                        delta : Efitdata.detdelta, $
                        numsx : Efitdata.detnumsx, $ 
                        numsy : Efitdata.detnumsy, $ 
                        energymin : EkeV - float(numEbins-1)*Ebinsize, $
                        energymax : EkeV, $ 
                        energyaverage : 0, $ 
                        xpc : Efitdata.detxpc, $
                        ypc : Efitdata.detypc, $
                        omega : Efitdata.detomega, $ 
                        anglefile : eulerpath, $ 
                        eulerconvention : eulerconvention[Efitdata.EulerConvention], $ 
                        masterfile : masterpath, $ 
                        energyfile : Efitdata.energyfilename, $ 
                        datafile : "undefined", $ 
                        beamcurrent : Efitdata.detbeamcurrent, $ 
                        dwelltime : Efitdata.detdwelltime, $ 
                        binning : 2^Efitdata.detbinning, $ 
                        scalingmode : 'not', $ 
                        gammavalue : Efitdata.detgamma, $ 
                        outputformat : 'bin', $ 
                        maskpattern : maskpattern[Efitdata.showcircularmask], $
                        nthreads : 1, $
                        spatialaverage : 'n' $ 
                }
; embed it into a new structure
                jsonstruct = {SEMdata: SEMdata}

; and write this to a file using implied_print formatting
                 openw,1,Efitdata.jsonpathname+'/'+Efitdata.jsonfilename
                 printf,1,jsonstruct,/implied_print
                 close,1
                 Core_Print,'Structure saved to '+Efitdata.jsonfilename+'.json file'

; next, create the euler angle file
; in a future version, we will also include the pattern center coordinates in this file...
                 openw,1,Efitdata.eulerpathname+'/'+Efitdata.eulerfilename
                 printf,1,'eu'
                 printf,1,'1'
                 printf,1,Efitdata.detphi1,', ',Efitdata.detphi,', ',Efitdata.detphi2
                 close,1
                 Core_Print,'Euler angles saved to '+Efitdata.eulerfilename+'.txt file'
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
                Efitdata.detxpc = Core_WidgetEvent( Efitwidget_s.fitValue[2],  'Pattern center x set to [pixels] ', '(F9.2)', /flt)
		Efit_updatePC,/display
	endcase
        'DETYPC' : begin
                Efitdata.detypc = Core_WidgetEvent( Efitwidget_s.fitValue[3],  'Pattern center y set to [pixels] ', '(F9.2)', /flt)
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
                Efitdata.detxpc += float(Efitdata.detmxpc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detxpc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[2]
                Core_Print, 'Pattern center x set to [pixels] '+string(Efitdata.detxpc,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETuYPC' : begin
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
                Efitdata.detxpc -= float(Efitdata.detmxpc)
                WIDGET_CONTROL, SET_VALUE=string(Efitdata.detxpc,FORMAT='(F9.2)'), Efitwidget_s.fitValue[2]
                Core_Print, 'Pattern center x set to [pixels] '+string(Efitdata.detxpc,FORMAT='(F9.2)')
		Efit_updatePC,/display
                EfitCalc
	endcase
        'DETdYPC' : begin
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
