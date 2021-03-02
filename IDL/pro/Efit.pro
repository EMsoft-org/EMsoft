@Core_WidgetEvent
@Core_WidgetChoiceEvent
@Core_WText
@Core_WTextE
@Core_FitLine
@Core_Print
@Core_getenv
@Core_quatmult
@Core_quat_Lp
@Core_eu2qu
@Core_qu2eu
@Core_LambertSphereToSquare
@Core_histnd
@Core_mind
@Efitcalc
@Efitinit
@Efit_display
@Efit_display_event
@Efit_drawPC
@Efitevent
@Efit_control
@Efit_control_event
@Efit_event
@Efit_amoeba
@Efit_fit
@Efit_constrainedfit
@Efit_update
@Efit_constrainedupdate
@Efitgetpreferences
@Efitwritepreferences
@Efitgetfilename
@Efit_navigator
@Efit_navigator_event
@Efit_updatePC
@Efit_showpattern

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
; EMsoft:Efit.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Electron backscatter diffraction detector parameter fit interface
;
;> @details 10/12/15 a new GUI to interactively determine the best fit parameters
;> for an EBSD pattern.
;> The idea is to have the main widget offer options for refinement and default
;> values for detector variables.  Then there is a pattern widget which displays
;> one of several options: experimental pattern, fitted pattern, difference pattern,
;> color coded overlap pattern.  These options can be changed during the fitting
;> portion because they are set from a separate widget window, as is the Cancel 
;> button.  In addition, the use can manually adjust all the individual parameters
;> using the + and - buttons, and the simulated pattern will be updated automatically.
;
;> @date 10/12/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro Efit,dummy

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata
common CommonCore, status, logmode, logunit
common FitParameters, nFit, fitName, defValue, fitValue, fitStep, fitOnOff, fitManualStep, fitManualUpDown, fitUserLabel, fitStepLabel, fitOnOffLabel, fitUpLabel, fitDownLabel, fitManualStepLabel, fitIterations
common getenv_common, librarylocation

; before we do anything, we make sure that the location of the app_user_dir is set 
appdir = app_user_dir('EMsoft','EMsoftPackage','VMapps','Virtual Machine Apps',['This folder is used by vitual machine apps within EMsoft'],1)

!EXCEPT = 0
logmode = 0
logunit = 10
nFit = 9


; widget structure
Efitwidget_s = {widgetstruct, $
        	base:long(0), $                    	
        	controlbase:long(0), $                 
        	navigatorbase:long(0), $              
        	navstepsize:long(0), $                    	
        	displaybase:long(0), $               
                displayoption:long(0), $
                cancelbutton:long(0), $
                cancelwidget:long(0), $
                CWbutton:long(0), $
                CCWbutton:long(0), $
                UPbutton:long(0), $
                DOWNbutton:long(0), $
                LEFTbutton:long(0), $
                RIGHTbutton:long(0), $
                ZERObutton:long(0), $
                progress:long(0), $
                imageformat:long(0), $
                logodraw:long(0), $
                logodrawid:long(0), $
                draw:long(0), $
                drawid:long(0), $
                mpfilename:long(0), $
                expfilename:long(0), $
                mploadfile:long(0), $
                exploadfile:long(0), $
                mainstop:long(0), $
                gofit:long(0), $
                dettheta:long(0), $
                detomega:long(0), $
                detdelta:long(0), $
                detnumsx:long(0), $
                detnumsy:long(0), $
                detbeamcurrent:long(0), $
                detdwelltime:long(0), $
                detbinning:long(0), $
                detL:long(0), $
                defValue:replicate(0L,nFit), $
                fitValue:replicate(0L,nFit), $
                fitStep:replicate(0L,nFit), $
                fitOnOff:replicate(0L,nFit), $
                fitManualUp:replicate(0L,nFit), $
                fitManualDown:replicate(0L,nFit), $
                fitManualStep:replicate(0L,nFit), $
                status:long(0), $
		xstar: long(0), $			; x^* parameter widget
		ystar: long(0), $			; y^* parameter widget
		zstar: long(0), $			; z^* parameter widget
                min:long(0), $
                max:long(0), $
                smoothval:long(0), $
                compute:long(0), $
                mkjson:long(0), $
                convcrit:long(0), $
                fitmode:long(0), $
                goconstrainedfit:long(0), $
                preproc:long(0), $
                ramponoff:long(0), $
                hipassonoff:long(0), $
                hipasscutoff:long(0), $
                circularmask:long(0), $
                inverseGaussian:long(0), $
                EulerConvention:long(0), $
                PatternOrigin:long(0), $
                test:long(0) }

; data structure
Efitdata = {Efitdatastruct, $
                scrdimx:fix(0), $
                scrdimy:fix(0), $
                xlocation:fix(0), $
                ylocation:fix(0), $
                xlocationcontrol:fix(0), $
                ylocationcontrol:fix(0), $
                xlocationnavigator:fix(0), $
                ylocationnavigator:fix(0), $
                xlocationdisplay:fix(0), $
                ylocationdisplay:fix(0), $
                navstepsize:float(0), $
                navselector:long(0), $
                navqx:fltarr(4), $
                navqy:fltarr(4), $
                navqz:fltarr(4), $
                imageformat:long(0), $
                displayoption:long(0), $
                drawID:long(0), $
                mpfilename:'', $
                energyfilename:'', $
                expfilename:'', $
                detdelta:float(25.0), $
                detL:float(15000), $
                detomega:float(0), $
                detxpc:float(0), $
                detypc:float(0), $
                detgamma:float(0.3), $
                detphi1:float(0), $
                detphi:float(0), $
                detphi2:float(0), $
                dettheta:float(10), $
                detsL:float(100), $
                detsomega:float(0.5), $
                detsxpc:float(5.0), $
                detsypc:float(5.0), $
                detsgamma:float(0.1), $
                detsphi1:float(2.0), $
                detsphi:float(2.0), $
                detsphi2:float(2.0), $
                detstheta:float(2.0), $
                detoL:float(0), $
                detoomega:float(0), $
                detoxpc:float(0), $
                detoypc:float(0), $
                detogamma:float(0), $
                detophi1:float(0), $
                detophi:float(0), $
                detophi2:float(0), $
                detotheta:float(0), $
                detmL:float(100), $
                detmomega:float(0.5), $
                detmxpc:float(5.0), $
                detmypc:float(5.0), $
                detmgamma:float(0.1), $
                detmphi1:float(2.0), $
                detmphi:float(2.0), $
                detmphi2:float(2.0), $
                detmtheta:float(2.00), $
                quaternion:fltarr(4), $
                detbeamcurrent:float(1000), $
                detdwelltime:float(1000), $
                detbinning:long(0), $
                detnumsx:long(0), $
                detnumsy:long(0), $
                detMCsig:float(0), $
		xstar: float(0), $			; x^* value
		ystar: float(0), $			; y^* value
		zstar: float(0), $			; z^* value
                showcircularmask:long(0), $
                inverseGaussian:long(0), $
                EulerConvention:long(0), $
                PatternOrigin:long(0), $
                Efitroot:'undefined', $
                mpfilesize:long(0), $
                patternfilesize:long(0), $
                EMsoftpathname:'', $
                EMdatapathname:'', $
                patternpathname:'', $
                pathname:'', $
                patternfilename:'', $
                EBSPsuffix:'', $
                suffix:'', $
                fitmode:long(0), $
                preproc:long(0), $
                compute:long(0), $
                jsonpathname:'', $
                jsonfilename:'', $
                jsonsuffix:'', $
                eulerpathname:'', $
                eulerfilename:'', $
                eulersuffix:'', $
                ramponoff:long(0), $
                convcrit:long(0), $
                smoothval:long(0), $
                hipassonoff:long(0), $
                hipasscutoff:float(0), $
                homefolder:'', $
                nprefs:long(0), $
                appdir: appdir, $               ; location of the user application folder
                prefname: 'Efitgui.prefs', $    ; filename of preferences file (will be located inside data.appdir)
                foldersep: '/', $               ; folder separator character ('/' for OS X and Linux, '\' for Windows)
                test:long(0) }

Efitdata.EMsoftpathname = Core_getenv(/bin)
Efitdata.EMdatapathname = Core_getenv(/data)
librarylocation = Core_getenv(/lib)

; set the foldersep string
if ( (!version.os ne 'darwin') and (!version.os ne 'linux') ) then Efitdata.foldersep = '\'
Efitdata.appdir = Efitdata.appdir+Efitdata.foldersep

;------------------------------------------------------------
; get the display window size to 80% of the current screen size (but be careful with double screens ... )
; We'll need to guess whether or not the user has a double screen: if the aspect ratio is larger than 16/9,
; then there are likely two screens, so we need to limit ourselves to just the first one...
; This should really become a core function that we can call from all programs.
device,decomposed = 0
device, GET_SCREEN_SIZE = scr

sar = float(scr[0])/float(scr[1])
if (sar gt (1.1*16.0/9.0)) then begin
	scr[0] = scr[0]/2
end
Efitdata.scrdimy = scr[1] * 0.8
Efitdata.scrdimx = scr[0]
Efitdata.xlocation = Efitdata.scrdimx / 8.0
Efitdata.ylocation = Efitdata.scrdimx / 8.0

;------------------------------------------------------------
; does the preferences file exist ?  If not, create it, otherwise read it
; this should also fill in some of the default values for the refinable parameters and the stepsizes and such
Efitgetpreferences,/noprint

;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
; number of fitting parameters in this program and associated arrays; new parameters should be added at the end of each array!
fitName = [ ' Scintillator Distance',  $
            '   Sample omega angle ',  $
            '         Detector pcx ',  $
            '         Detector pcy ',  $
            'Intensity Gamma value ',  $
            '           Euler phi1 ',  $
            '            Euler Phi ',  $
            '           Euler phi2 ',  $
            '  Detector tilt angle ']
fitUserLabel = ['DETL','DETOMEGA','DETXPC','DETYPC','DETGAMMA','DETphi1','DETphi','DETphi2','DETtheta']
fitStepLabel = ['DETsL','DETsOMEGA','DETsXPC','DETsYPC','DETsGAMMA','DETsphi1','DETsphi','DETsphi2','DETstheta']
fitOnOffLabel = ['DEToL','DEToOMEGA','DEToXPC','DEToYPC','DEToGAMMA','DETophi1','DETophi','DETophi2','DETotheta']
fitUpLabel = ['DETuL','DETuOMEGA','DETuXPC','DETuYPC','DETuGAMMA','DETuphi1','DETuphi','DETuphi2','DETutheta']
fitDownLabel = ['DETdL','DETdOMEGA','DETdXPC','DETdYPC','DETdGAMMA','DETdphi1','DETdphi','DETdphi2','DETdtheta']
fitManualStepLabel = ['DETmL','DETmOMEGA','DETmXPC','DETmYPC','DETmGAMMA','DETmphi1','DETmphi','DETmphi2','DETmtheta']
defValue = fltarr(nFit)
fitValue = fltarr(nFit)
fitValue[0] = Efitdata.detL
fitValue[1] = Efitdata.detomega
fitValue[2] = Efitdata.detxpc
fitValue[3] = Efitdata.detypc
fitValue[4] = Efitdata.detgamma
fitValue[5] = Efitdata.detphi1
fitValue[6] = Efitdata.detphi
fitValue[7] = Efitdata.detphi2
fitValue[8] = Efitdata.dettheta
fitStep = fltarr(nFit)
fitStep[0] = Efitdata.detsL
fitStep[1] = Efitdata.detsomega
fitStep[2] = Efitdata.detsxpc
fitStep[3] = Efitdata.detsypc
fitStep[4] = Efitdata.detsgamma
fitStep[5] = Efitdata.detsphi1
fitStep[6] = Efitdata.detsphi
fitStep[7] = Efitdata.detsphi2
fitStep[8] = Efitdata.detstheta
fitOnOff = replicate(0L,nFit)
fitManualStep = fltarr(nFit)
fitManualStep[0] = Efitdata.detmL
fitManualStep[1] = Efitdata.detmomega
fitManualStep[2] = Efitdata.detmxpc
fitManualStep[3] = Efitdata.detmypc
fitManualStep[4] = Efitdata.detmgamma
fitManualStep[5] = Efitdata.detmphi1
fitManualStep[6] = Efitdata.detmphi
fitManualStep[7] = Efitdata.detmphi2
fitManualStep[8] = Efitdata.detmtheta


; a few font strings (this will need to be redone for Windows systems)
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; create the top level widget
Efitwidget_s.base = WIDGET_BASE(TITLE='Electron Backscatter Diffraction Pattern Fit Program', $
                        /COLUMN, $
                        XSIZE=1220, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='Efit_event', $
                        XOFFSET=Efitdata.xlocation, $
                        YOFFSET=Efitdata.ylocation)

block0 = WIDGET_BASE(Efitwidget_s.base, $
			XSIZE=1220, $
			/ALIGN_CENTER, $
			/ROW)

;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
; create the two main columns for the top row
; block 1 is the left column, with the logo 
block1 = WIDGET_BASE(block0, $
			/FRAME, $
			XSIZE=610, $
			/ALIGN_CENTER, $
			/ROW)

Efitwidget_s.logodraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/ALIGN_CENTER, $
			XSIZE=600, $
			YSIZE=200)
			;YSIZE=200)

;------------------------------------------------------------
; block 2 is the right column, with the input file widgets
; we're asking for the master pattern file name, which will load everything needed.
; then we display here the energyfile name, and perhaps a few other items
block2 = WIDGET_BASE(block0, $
			XSIZE=610, $
			/FRAME, $
			/COLUMN)

tmp1 = WIDGET_LABEL(block2, VALUE='Data Files', font=fontstrlarge, /ALIGN_LEFT)

;---------- experimental pattern file name
file1 = WIDGET_BASE(block2, /ROW, XSIZE=600, /ALIGN_CENTER)
Efitwidget_s.expfilename = Core_WText(file1,'Exp Pattern File Name', fontstr, 200, 25, 60, 1, Efitdata.expfilename)

;---------- followed by the master pattern file name and load button
file2 = WIDGET_BASE(block2, /ROW, XSIZE=600, /ALIGN_CENTER)
Efitwidget_s.mpfilename = Core_WText(file2,'MP Data File Name', fontstr, 200, 25, 60, 1, Efitdata.mpfilename)

;---------- finally, we'll put a couple of buttons here
file3 = WIDGET_BASE(block2, XSIZE=550, /ROW)

Efitwidget_s.exploadfile = WIDGET_BUTTON(file3, $
                                UVALUE='EXPFILE', $
                                VALUE='Load experimental pattern', $
                                EVENT_PRO='Efit_event', $
                                SENSITIVE=1, $
                                /FRAME)

Efitwidget_s.mploadfile = WIDGET_BUTTON(file3, $
                                UVALUE='MPFILE', $
                                VALUE='Load master file', $
                                EVENT_PRO='Efit_event', $
                                SENSITIVE=0, $
                                /FRAME)

Efitwidget_s.mainstop = WIDGET_BUTTON(file3, $
                                UVALUE='QUIT', $
                                VALUE='Quit', $
                                EVENT_PRO='Efit_event', $
                                SENSITIVE=1, $
                                /FRAME)

;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------


; all the parameters below already exist in the regular EBSDDisplay GUI, but will be reused 
; here in a different widget configuration

block1 = WIDGET_BASE(Efitwidget_s.base, $
			XSIZE=1220, $
			/ALIGN_CENTER, $
			/COLUMN)

block2 = WIDGET_BASE(block1, $
			/FRAME, $
			XSIZE=1200, $
			/ALIGN_CENTER, $
			/COLUMN)

;------------------------------------------------------------
tmp1 = WIDGET_LABEL(block2, VALUE='Non-refinable Parameters', font=fontstrlarge, /ALIGN_LEFT)


line1 = WIDGET_BASE(block2, XSIZE=1200, /ROW)
;----------  detector tilt angle
;item1 = WIDGET_BASE(line1, /ROW, XSIZE=350, /ALIGN_LEFT)
;Efitwidget_s.dettheta = Core_WTextE(item1,'Detector Tilt Angle [deg]', fontstr, 250, 25, 10, 1, string(Efitdata.dettheta,format="(F9.2)"),'DETTHETA','Efit_event')

;----------  scintillator pixel size
item2 = WIDGET_BASE(line1, /ROW, XSIZE=350, /ALIGN_LEFT)
Efitwidget_s.detdelta = Core_WTextE(item2,'Scintillator Pixel Size [micron]', fontstr, 250, 25, 10, 1, string(Efitdata.detdelta,format="(F9.2)"),'DETDELTA','Efit_event')

;----------  number of pixels on detector
item3 = WIDGET_BASE(line1, /ROW, XSIZE=560, /ALIGN_LEFT)
Efitwidget_s.detnumsx = Core_WTextE(item3,'Number of pixels ', fontstr, 140, 25, 5, 1, string(Efitdata.detnumsx,format="(I4)"),'DETNUMSX','Efit_event')
Efitwidget_s.detnumsy = Core_WTextE(item3,' by ', fontstr, 30, 25, 5, 1, string(Efitdata.detnumsy,format="(I4)"),'DETNUMSY','Efit_event')

;----------  Inverse Gaussian
item4 = WIDGET_BASE(line1, /ROW, XSIZE=350, /ALIGN_LEFT)
vals = ['Off','On']
Efitwidget_s.inverseGaussian = CW_BGROUP(item3, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Inverse Gaussian', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='INVERSEGAUSSIAN', $
                        SET_VALUE=Efitdata.inverseGaussian)

;------------------------------------------------------------
line2 = WIDGET_BASE(block2, XSIZE=1200, /ROW)

;----------  Beam current
item1 = WIDGET_BASE(line2, /ROW, XSIZE=350, /ALIGN_LEFT)
Efitwidget_s.detbeamcurrent = Core_WTextE(item1,'Beam current [nA]', fontstr, 250, 25, 10, 1, string(Efitdata.detbeamcurrent,format="(F9.2)"),'DETBEAMCURRENT','Efit_event')

;----------  Dwell time
item2 = WIDGET_BASE(line2, /ROW, XSIZE=350, /ALIGN_LEFT)
Efitwidget_s.detdwelltime = Core_WTextE(item2,'Dwell Time [mu s] ', fontstr, 250, 25, 10, 1, string(Efitdata.detdwelltime,format="(F9.2)"),'DETDWELLTIME','Efit_event')

;----------  Circular mask
item3 = WIDGET_BASE(line2, /ROW, XSIZE=350, /ALIGN_LEFT)
vals = ['Off','On']
Efitwidget_s.circularmask = CW_BGROUP(item3, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Circular Mask', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='CIRCULARMASK', $
                        SET_VALUE=Efitdata.showcircularmask)


;------------------------------------------------------------
;line3 = WIDGET_BASE(block2, XSIZE=1200, /ROW)

;----------   binning factor
;item1 = WIDGET_BASE(line3, /ROW, XSIZE=350, /ALIGN_LEFT)
;vals = ['1','2','4','8']
;Efitwidget_s.detbinning = CW_BGROUP(item1, $
;                        vals, $
;                        /ROW, $
;                        /NO_RELEASE, $
;                        /EXCLUSIVE, $
;                        FONT=fontstr, $
;                        LABEL_LEFT = 'Detector Binning', $
;                        EVENT_FUNC ='Efitevent', $
;                        UVALUE='EBSDPATTERNBINNING', $
;                        SET_VALUE=Efitdata.detbinning)

;----------   Euler convention
;item2 = WIDGET_BASE(line3, /ROW, XSIZE=350, /ALIGN_LEFT)
;vals = ['TSL', 'HKL']
;Efitwidget_s.EulerConvention = CW_BGROUP(item2, $
;                        vals, $
;                        /ROW, $
;                        /NO_RELEASE, $
;                        /EXCLUSIVE, $
;                        FONT=fontstr, $
;                        LABEL_LEFT = 'Euler phi1 Convention', $
;                        EVENT_FUNC ='Efitevent', $
;                        UVALUE='EBSDEULERCONVENTION', $
;                        SET_VALUE=Efitdata.EulerConvention)

;----------   origina location
;item3 = WIDGET_BASE(line3, /ROW, XSIZE=350, /ALIGN_LEFT)
;vals = ['UL','LL','UR','LR']
;Efitwidget_s.PatternOrigin = CW_BGROUP(item3, $
;                        vals, $
;                        /ROW, $
;                        /NO_RELEASE, $
;                        /EXCLUSIVE, $
;                        FONT=fontstr, $
;                        LABEL_LEFT = 'Pattern Origin', $
;                        EVENT_FUNC ='Efitevent', $
;                        UVALUE='EBSPATTERNORIGIN', $
;                        SET_VALUE=Efitdata.PatternOrigin)

row2 = WIDGET_BASE(block1, $
			XSIZE=1200, $
			/ALIGN_LEFT, $
			/ROW)


block2 = WIDGET_BASE(row2, $
			/FRAME, $
			XSIZE=750, $
			/ALIGN_LEFT, $
			/COLUMN)

;------------------------------------------------------------
tmp1 = WIDGET_LABEL(block2, VALUE='Refinable Parameters', font=fontstrlarge, /ALIGN_LEFT)

; add a header
header = WIDGET_LABEL(block2, VALUE='     Parameter          Value        Stepsize      Refine?              Up   Down     Manual stepsize', font=fontstrlarge, /ALIGN_LEFT)

; and create each row of fitting widgets
for i=0,nFit-1 do ret = Core_FitLine(block2, i)

;------------------------------------------------------------
;------------------------------------------------------------
block3 = WIDGET_BASE(row2, $
			XSIZE=440, $
			/ALIGN_CENTER, $
			/COLUMN)

block4 = WIDGET_BASE(block3, $
			/FRAME, $
			XSIZE=430, $
			/ALIGN_CENTER, $
			/COLUMN)

; here we put a couple of image adjustment options
;
; hipass filter with filter coefficient
; linear ramp on/off
; smoothing
;
; the pattern should automatically update each time a parameter is changed
; but we should also have a Update Pattern button for the first column of 
; the refinable parameters section.
;
line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)

vals = ['Off','On']
Efitwidget_s.hipassonoff = CW_BGROUP(line2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Hipass filter', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='HIPASSONOFF', $
                        SET_VALUE=Efitdata.hipassonoff)
line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)

Efitwidget_s.hipasscutoff = Core_WTextE(line2,'     Hipass filter low cut off ', fontstr, 250, 25, 10, 1, string(Efitdata.hipasscutoff,format="(F9.2)"),'HIPASSCUTOFF','Efit_event')

; ramp filter on/off
line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
vals = ['Off','On']
Efitwidget_s.ramponoff = CW_BGROUP(line2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Linear ramp subtraction', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='RAMPONOFF', $
                        SET_VALUE=Efitdata.ramponoff)


line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)

vals = ['0','3','5','7','9','11']
Efitwidget_s.smoothval = CW_BGROUP(line2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Smoothing parameter', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='SMOOTHVAL', $
                        SET_VALUE=Efitdata.smoothval)

line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
vals = ['Regular','Laplacian']
Efitwidget_s.preproc = CW_BGROUP(line2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Preprocessing mode', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='PREPROC', $
                        SET_VALUE=Efitdata.preproc)


;------------------------------------------------------------
;------------------------------------------------------------
block4 = WIDGET_BASE(block3, $
			/FRAME, $
			XSIZE=430, $
			/ALIGN_CENTER, $
			/COLUMN)

line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)

vals = ['dot product','mutual information']
Efitwidget_s.convcrit= CW_BGROUP(line2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Fit criterion', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='CONVCRIT', $
                        SET_VALUE=Efitdata.convcrit)

line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
vals = ['free','detector','orientation']
Efitwidget_s.fitmode = CW_BGROUP(line2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Fit mode', $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='FITMODE', $
                        SET_VALUE=Efitdata.fitmode)


line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
Efitwidget_s.compute = WIDGET_BUTTON(line2, $
                                UVALUE='COMPUTE', $
                                VALUE='Compute', $
                                EVENT_PRO='Efit_event', $
                                /ALIGN_CENTER, $
                                SENSITIVE=0)

line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
Efitwidget_s.gofit = WIDGET_BUTTON(line2, $
                                UVALUE='GOFIT', $
                                VALUE='Start Fit', $
                                EVENT_PRO='Efit_event', $
                                /ALIGN_CENTER, $
                                SENSITIVE=0)

Efitwidget_s.progress = Core_WText(line2,'convergence parameter', fontstr, 200, 25, 60, 1, string(0.0,FORMAT="(F12.6)"))

line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
Efitwidget_s.goconstrainedfit = WIDGET_BUTTON(line2, $
                                UVALUE='GOCONSTRAINEDFIT', $
                                VALUE='Start Constrained Fit', $
                                EVENT_PRO='Efit_event', $
                                /ALIGN_CENTER, $
                                SENSITIVE=0)


line2 = WIDGET_BASE(block4, XSIZE=410, /ROW, /ALIGN_LEFT)
Efitwidget_s.mkjson= WIDGET_BUTTON(line2, $
                                UVALUE='MKJSON', $
                                VALUE='Create JSON file', $
                                EVENT_PRO='Efit_event', $
                                /ALIGN_CENTER, $
                                SENSITIVE=0)


;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT, /FRAME)
Efit_updatePC
Efitwidget_s.xstar = Core_WText(file1,'x*', fontstr, 20, 25, 10, 1, string(Efitdata.xstar,FORMAT='(F6.4)'))
Efitwidget_s.ystar = Core_WText(file1,'y*', fontstr, 20, 25, 10, 1, string(Efitdata.ystar,FORMAT='(F6.4)'))
Efitwidget_s.zstar = Core_WText(file1,'z*', fontstr, 20, 25, 10, 1, string(Efitdata.zstar,FORMAT='(F6.4)'))

; then we have the program message window

Efitwidget_s.status= WIDGET_TEXT(block1, $
			XSIZE=195, $
			YSIZE=4, $
			/SCROLL, $
			VALUE=' ',$
			/ALIGN_LEFT)

; the following is needed by the Core_Print routine
status = Efitwidget_s.status 




;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,Efitwidget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, Efitwidget_s.logodraw, GET_VALUE=drawID
Efitwidget_s.logodrawID = drawID
;
read_jpeg,'Resources/EMsoftVBFFlogo.jpeg',logo
wset,Efitwidget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"Efit",Efitwidget_s.base,/NO_BLOCK


end ; program
