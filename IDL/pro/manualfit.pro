@manualfit_event
@Core_getenv
@Core_WTextE
@manualfit_loadfiles
@Core_eu2qu
@Core_qu2eu
@manualfit_calc

;
; Copyright (c) 2014-2017, Marc De Graef/Carnegie Mellon University
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
; EMsoft:manualfit.pro
;--------------------------------------------------------------------------
;
; PROGRAM: manualfit.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief manual fit program for four simultaneous EBSD patterns
;
;> @date 04/10/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro manualfit, dummy
;
; widget interface to manually fit four EBSD patterns with known relative locations.
;
;------------------------------------------------------------
; common blocks
common FIT_widget_common, FITwidget_s
common FIT_data_common, FITdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common quadfit_common, numpat, stepsize, delta, numsx, numsy, EBSDpatterns, patloc, posx, posy, soln, mival, cnt, HX, HY, HXY, hcnt, maxMI, cuinit, newdetectorparams

common EBSD_EMsoft, MCxtalname, MCmode, nsx, nsy, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, MCsig, MComega, $
                    numEbins, numzbins, accum_e, accum_z, Masterenergyfile, npx, npy, nnE, numset, mLPNH, mLPSH, Masterxtalname

common savepatterns, saveEBSDpatterns, mask, prevmax, saveit, filename, Epat
common CommonCore, status, logmode, logunit


;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("manualfit") NE 0) then begin
  print,'manualfit is already running ... (if it is not, please restart your IDL session)'
  return
end

; do not report exceptions, such as division by zero etc...
!EXCEPT=0

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
FITwidget_s = {widgetstruct, $
	base:long(0), $                     	; base widget ID
    status:long(0), $
	UP1button:long(0), $
	UP2button:long(0), $
	UP3button:long(0), $
	UP4button:long(0), $
	LEFT1button:long(0), $
	LEFT2button:long(0), $
	LEFT3button:long(0), $
	LEFT4button:long(0), $
	RIGHT1button:long(0), $
	RIGHT2button:long(0), $
	RIGHT3button:long(0), $
	RIGHT4button:long(0), $
	DOWN1button:long(0), $
	DOWN2button:long(0), $
	DOWN3button:long(0), $
	DOWN4button:long(0), $
	CW1button:long(0), $
	CW2button:long(0), $
	CW3button:long(0), $
	CW4button:long(0), $
	CCW1button:long(0), $
	CCW2button:long(0), $
	CCW3button:long(0), $
	CCW4button:long(0), $
	ZERO1button:long(0), $
	ZERO2button:long(0), $
	ZERO3button:long(0), $
    ZERO4button:long(0), $
    xpcup:long(0), $
    xpcdown:long(0), $
    xpcstepsize:long(0), $
    ypcup:long(0), $
    ypcdown:long(0), $
    ypcstepsize:long(0), $
    Lup:long(0), $
    Ldown:long(0), $
    Lstepsize:long(0), $
	navstepsize:long(0), $
	patterndraw:long(0), $
	patterndrawID:long(0), $
    mainstop:long(0)}; , $

FITdata = {FITdatastruct, $
	scrdimx:fix(0), $
	scrdimy:fix(0), $
	xlocation:fix(0), $
	ylocation:fix(0), $
	f90exepath:'', $
	EMsoftpathname:'', $
    navstepsize:float(1), $
    navqx:fltarr(4), $
    navqy:fltarr(4), $
    navqz:fltarr(4), $
    phi1init:fltarr(5), $
    phiinit:fltarr(5), $
    phi2init:fltarr(5), $
    phi1:fltarr(5),$
    phi:fltarr(5),$
    phi2:fltarr(5),$
    relpos:fltarr(2,4), $
    xstar:float(0.0), $
    ystar:float(0.0), $
    zstar:float(0.0), $
    xpc:float(0.0), $
    ypc:float(0.0), $
    L:float(0.0), $
    dloc : intarr(2,4), $
    delta:float(0.0), $
    detMCsig:float(0.0), $
    dettheta:float(0.0), $
    xpcstepsize:float(1.0), $
    ypcstepsize:float(1.0), $
    Lstepsize:float(100.0), $
    patloc:fltarr(8), $
    logmode: fix(0), $              ; keep a log file or not
    logunit: fix(13), $             ; logical file unit for log file
    logname: '', $                  ; filename for log output
    logfileopen: fix(0), $          ; log file is open when 1, closed when 0
    numsx:long(0), $
    numsy:long(0), $
    numpat:long(4), $
	eventverbose:byte(0)}; , $

; a few font strings (this will need to be redone for Windows systems !!!)
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

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
FITdata.scrdimy = scr[1] * 0.8
FITdata.scrdimx = scr[0]
FITdata.xlocation = FITdata.scrdimx / 8.0
FITdata.ylocation = FITdata.scrdimx / 8.0
logmode = FITdata.logmode
logunit = FITdata.logunit

;------------------------------------------------------------
; get the pathname for the executables and the EMsoftLib library
FITdata.f90exepath = Core_getenv(/bin)
FITdata.EMsoftpathname = Core_getenv(/bin)
librarylocation = Core_getenv(/lib)

;------------------------------------------------------------
; create the top level widget
FITwidget_s.base = WIDGET_BASE(TITLE='EBSD Four-Pattern FIT Program', $
                    /COLUMN, $
                    XSIZE=400 + 2 * 640+40, $
                    /ALIGN_LEFT, $
                    /TLB_MOVE_EVENTS, $
                    EVENT_PRO='manualfit_event', $
                    XOFFSET=FITdata.xlocation, $
                    YOFFSET=FITdata.ylocation)

;------------------------------------------------------------
; create the two main columns
; then a row of three columns with buttons
mainrow = WIDGET_BASE(FITwidget_s.base, $
            /ALIGN_LEFT, $
            /ROW)

; block 1 is the left column, with the navigation buttons
block1 = WIDGET_BASE(mainrow, $
			/FRAME, $
			XSIZE=400, $
			/ALIGN_CENTER, $
			/COLUMN)


col0 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /COL)
; first we have a text message explaining what this widget does ... 
tmp1 = WIDGET_LABEL(col0, VALUE='Pattern 1 (UL)', font=fontstr, /ALIGN_LEFT)

row1 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /ROW)

col1 = WIDGET_BASE(row1, $
            XSIZE=120, $
            /ALIGN_CENTER, $
            /COL)

; CCW and LEFT buttons
FITwidget_s.CCW1button = WIDGET_BUTTON(col1, $
                                UVALUE='CCW1', $
                                VALUE='CCW1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.LEFT1button = WIDGET_BUTTON(col1, $
                                UVALUE='LEFT1', $
                                VALUE='LEFT1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col2 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; UP, ZERO, and DOWN buttons
FITwidget_s.UP1button = WIDGET_BUTTON(col2, $
                                UVALUE='UP1', $
                                VALUE='UP1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.ZERO1button = WIDGET_BUTTON(col2, $
                                UVALUE='ZERO1', $
                                VALUE='ZERO1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.DOWN1button = WIDGET_BUTTON(col2, $
                                UVALUE='DOWN1', $
                                VALUE='DOWN1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col3 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; CW and RIGHT buttons
FITwidget_s.CW1button = WIDGET_BUTTON(col3, $
                                UVALUE='CW1', $
                                VALUE='CW1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.RIGHT1button = WIDGET_BUTTON(col3, $
                                UVALUE='RIGHT1', $
                                VALUE='RIGHT1', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

; and now we repeat this for the other three patterns ...
;==============
; PATTERN 2
;==============


col0 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /COL)
; first we have a text message explaining what this widget does ... 
tmp1 = WIDGET_LABEL(col0, VALUE='Pattern 2 (UR)', font=fontstr, /ALIGN_LEFT)

row1 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /ROW)

col1 = WIDGET_BASE(row1, $
            XSIZE=120, $
            /ALIGN_CENTER, $
            /COL)

; CCW and LEFT buttons
FITwidget_s.CCW2button = WIDGET_BUTTON(col1, $
                                UVALUE='CCW2', $
                                VALUE='CCW2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.LEFT2button = WIDGET_BUTTON(col1, $
                                UVALUE='LEFT2', $
                                VALUE='LEFT2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col2 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; UP, ZERO, and DOWN buttons
FITwidget_s.UP2button = WIDGET_BUTTON(col2, $
                                UVALUE='UP2', $
                                VALUE='UP2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.ZERO2button = WIDGET_BUTTON(col2, $
                                UVALUE='ZERO2', $
                                VALUE='ZERO2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.DOWN2button = WIDGET_BUTTON(col2, $
                                UVALUE='DOWN2', $
                                VALUE='DOWN2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col3 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; CW and RIGHT buttons
FITwidget_s.CW2button = WIDGET_BUTTON(col3, $
                                UVALUE='CW2', $
                                VALUE='CW2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.RIGHT2button = WIDGET_BUTTON(col3, $
                                UVALUE='RIGHT2', $
                                VALUE='RIGHT2', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

;==============
; PATTERN 3
;==============


col0 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /COL)
; first we have a text message explaining what this widget does ... 
tmp1 = WIDGET_LABEL(col0, VALUE='Pattern 3 (LL)', font=fontstr, /ALIGN_LEFT)

row1 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /ROW)

col1 = WIDGET_BASE(row1, $
            XSIZE=120, $
            /ALIGN_CENTER, $
            /COL)

; CCW and LEFT buttons
FITwidget_s.CCW3button = WIDGET_BUTTON(col1, $
                                UVALUE='CCW3', $
                                VALUE='CCW3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.LEFT3button = WIDGET_BUTTON(col1, $
                                UVALUE='LEFT3', $
                                VALUE='LEFT3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col2 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; UP, ZERO, and DOWN buttons
FITwidget_s.UP3button = WIDGET_BUTTON(col2, $
                                UVALUE='UP3', $
                                VALUE='UP3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.ZERO3button = WIDGET_BUTTON(col2, $
                                UVALUE='ZERO3', $
                                VALUE='ZERO3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.DOWN3button = WIDGET_BUTTON(col2, $
                                UVALUE='DOWN3', $
                                VALUE='DOWN3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col3 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; CW and RIGHT buttons
FITwidget_s.CW3button = WIDGET_BUTTON(col3, $
                                UVALUE='CW3', $
                                VALUE='CW3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.RIGHT3button = WIDGET_BUTTON(col3, $
                                UVALUE='RIGHT3', $
                                VALUE='RIGHT3', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

;==============
; PATTERN 4
;==============


col0 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /COL)
; first we have a text message explaining what this widget does ... 
tmp1 = WIDGET_LABEL(col0, VALUE='Pattern 4 (RL)', font=fontstr, /ALIGN_LEFT)

row1 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /ROW)

col1 = WIDGET_BASE(row1, $
            XSIZE=120, $
            /ALIGN_CENTER, $
            /COL)

; CCW and LEFT buttons
FITwidget_s.CCW4button = WIDGET_BUTTON(col1, $
                                UVALUE='CCW4', $
                                VALUE='CCW4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.LEFT4button = WIDGET_BUTTON(col1, $
                                UVALUE='LEFT4', $
                                VALUE='LEFT4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col2 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; UP, ZERO, and DOWN buttons
FITwidget_s.UP4button = WIDGET_BUTTON(col2, $
                                UVALUE='UP4', $
                                VALUE='UP4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.ZERO4button = WIDGET_BUTTON(col2, $
                                UVALUE='ZERO4', $
                                VALUE='ZERO4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.DOWN4button = WIDGET_BUTTON(col2, $
                                UVALUE='DOWN4', $
                                VALUE='DOWN4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

col3 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; CW and RIGHT buttons
FITwidget_s.CW4button = WIDGET_BUTTON(col3, $
                                UVALUE='CW4', $
                                VALUE='CW4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.RIGHT4button = WIDGET_BUTTON(col3, $
                                UVALUE='RIGHT4', $
                                VALUE='RIGHT4', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

; and finally, a text window for the user to set the navigation step size (in degrees)
row1 = WIDGET_BASE(block1, $
			XSIZE=360, $
			/FRAME, $
			/ALIGN_CENTER, $
			/ROW)

item1 = WIDGET_LABEL(row1, VALUE='Rotation step size (degrees)', font=fontstr, /ALIGN_LEFT)
; current value (editable)
FITwidget_s.navstepsize = Core_WTextE(row1,'', fontstr, 10, 25, 10, 1, string(FITdata.navstepsize,format="(F6.2)"), 'NAVSTEPSIZE','manualfit_event')

; =============================
; controls to set the detector parameters
; =============================

col0 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /COL)
; first we have a text message explaining what this widget does ... 
tmp1 = WIDGET_LABEL(col0, VALUE='Detector Parameters', font=fontstr, /ALIGN_LEFT)

col1 = WIDGET_BASE(block1, $
            XSIZE=380, $
            /ALIGN_CENTER, $
            /COL)

row1 = WIDGET_BASE(col1, $
            XSIZE=380, $
            /ALIGN_LEFT, $
            /ROW)

; detector up and down buttons
item1 = WIDGET_LABEL(row1, VALUE='xpc', font=fontstr, /ALIGN_LEFT)

FITwidget_s.xpcup= WIDGET_BUTTON(row1, $
                                UVALUE='XPCUP', $
                                VALUE='XPCUP', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.xpcdown= WIDGET_BUTTON(row1, $
                                UVALUE='XPCDOWN', $
                                VALUE='XPCDOWN', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.xpcstepsize = Core_WTextE(row1,'', fontstr, 10, 25, 10, 1, string(FITdata.xpcstepsize,format="(F6.2)"), 'XPCSTEPSIZE','manualfit_event')


row1 = WIDGET_BASE(col1, $
            XSIZE=380, $
            /ALIGN_LEFT, $
            /ROW)

; detector up and down buttons
item1 = WIDGET_LABEL(row1, VALUE='ypc', font=fontstr, /ALIGN_LEFT)

FITwidget_s.ypcup= WIDGET_BUTTON(row1, $
                                UVALUE='YPCUP', $
                                VALUE='YPCUP', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.ypcdown= WIDGET_BUTTON(row1, $
                                UVALUE='YPCDOWN', $
                                VALUE='YPCDOWN', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.ypcstepsize = Core_WTextE(row1,'', fontstr, 10, 25, 10, 1, string(FITdata.ypcstepsize,format="(F6.2)"), 'YPCSTEPSIZE','manualfit_event')


row1 = WIDGET_BASE(col1, $
            XSIZE=380, $
            /ALIGN_LEFT, $
            /ROW)

; detector up and down buttons
item1 = WIDGET_LABEL(row1, VALUE='L', font=fontstr, /ALIGN_LEFT)

FITwidget_s.Lup= WIDGET_BUTTON(row1, $
                                UVALUE='LUP', $
                                VALUE='LUP', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.Ldown= WIDGET_BUTTON(row1, $
                                UVALUE='LDOWN', $
                                VALUE='LDOWN', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

FITwidget_s.Lstepsize = Core_WTextE(row1,'', fontstr, 10, 25, 10, 1, string(FITdata.Lstepsize,format="(F6.2)"), 'LSTEPSIZE','manualfit_event')



; =============================
; some control buttons...
; =============================

; block 1 is the left column, with the navigation buttons

row1 = WIDGET_BASE(block1, $
            XSIZE=360, $
            /FRAME, $
            /ALIGN_CENTER, $
            /ROW)

FITwidget_s.mainstop = WIDGET_BUTTON(row1, $
                                VALUE='Quit', $
                                UVALUE='QUIT', $
                                EVENT_PRO='manualfit_event', $
                                SENSITIVE=1, $
                                /FRAME)

; create the pattern display window
col1 = WIDGET_BASE(mainrow, $
            XSIZE=2*645, $
            /FRAME, $
            /ALIGN_CENTER, $
            /COL)

FITwidget_s.patterndraw = WIDGET_DRAW(col1, $
            COLOR_MODEL=2, $
            RETAIN=2, $
            /FRAME, $
            /ALIGN_CENTER, $
            XSIZE=2*640, $
            YSIZE=2*480)

FITwidget_s.status= WIDGET_TEXT(col1, $
            XSIZE=100, $
            YSIZE=15, $
            /SCROLL, $
            VALUE=' ',$
            /ALIGN_LEFT)

; the following is needed by the Core_Print routine
status = FITwidget_s.status 

; =============================
; initialize parameters
; =============================
; for now we will just hard code the loading of the patterns and the 
; initial values of the detector parameters and Euler angles ...

FITdata.phi1init = [0.0, 41.023, 40.868, 29.8, 35.640]
FITdata.phiinit = [0.0, 108.206, 108.748, 133.829, 123.588]
FITdata.phi2init = [0.0, 186.924, 186.649, 65.225, 76.9912]
FITdata.phi1 = FITdata.phi1init
FITdata.phi = FITdata.phiinit
FITdata.phi2 = FITdata.phi2init

FITdata.xstar = 0.527
FITdata.ystar = 0.485
FITdata.zstar = 0.469

FITdata.delta = 50.0
FITdata.numsx = 640L
FITdata.numsy = 480L
FITdata.detMCsig = 70.0
FITdata.dettheta = 10.0

FITdata.xpc = FITdata.xstar * FITdata.numsx - 0.5 * float(FITdata.numsx) 
FITdata.ypc = FITdata.ystar * FITdata.numsx - 0.5 * float(FITdata.numsy) 
FITdata.L = FITdata.numsx * FITdata.delta * FITdata.zstar  ; in microns

print,'starting detector parameters : ',FITdata.xpc, FITdata.ypc, FITdata.L

FITdata.patloc = [0.0, 0.0, -1360.0, 0.0, 0.0, 400.0, -1360.0, 400.0]
patloc = FITdata.patloc

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,FITwidget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, FITwidget_s.patterndraw, GET_VALUE=drawID
FITwidget_s.patterndrawID = drawID
;

; and hand over control to the xmanager
XMANAGER,"manualfit",FITwidget_s.base,/NO_BLOCK

; complete the initialization and loading of parameters and patterns
mpfilename = 'playarea/DetectorFit/calibration_PFIB/Si-master-15kV.h5'
;   Core_Print,'reading master pattern data'
manualfit_loadfiles,mpfilename
;   Core_Print,'   --> master pattern data successfuly loaded'


;   Core_Print,'reading experimental patterns'
saveEBSDpatterns = bytarr(FITdata.numsx*FITdata.numsy,4)
gma = 0.34

fname = ['patt__018','patt__034','patt__086','patt__102']
flocation = '/Users/mdg/Files/EMPlay/playarea/DetectorFit/calibration_PFIB/'
FITdata.dloc = [[0,FITdata.numsy],[FITdata.numsx,FITdata.numsy],[0,0],[FITdata.numsx,0]]

EBSDpatterns = bytarr(FITdata.numsx*FITdata.numsy,FITdata.numpat)

wset,FITwidget_s.patterndrawID
erase
; read the pattern files
for i=0,3 do begin
  z = read_image(flocation+fname[i]+'.tiff')
  z = reverse(float(z[*,*]),2)
  z = z-sfit(z,2)
  z = bytscl(z)
  tvscl,z,FITdata.dloc[0,i],FITdata.dloc[1,i]
  ;z = adapt_hist_equal(z,nregions=10)*mask
  z = reform(z,FITdata.numsx*FITdata.numsy)
; write_tiff,fname[i]+'_filtered.tiff',reverse(reform(z,numsx,numsy),2)
  EBSDpatterns[0,i] = z
endfor
;   Core_Print,'  ---> experimental patterns read'
Epat = bytarr(FITdata.numsx*FITdata.numsy,FITdata.numpat)

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

; and compute all the patterns for the current setting of the parameters
manualfit_calc,-1


end
