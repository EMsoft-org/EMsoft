@Core_WTextE
@Core_Print
@EllipsometryTool_event
@OMETbuttonevent
@OMMuellerMatrixCalculus
@OMETinitrotator
@OMETinitpolarizer
@OMETinitretarder
@OMETinitSample
@OMETinitStokesVector
@OMETupdateOEwidgets
@OMETpolarizerWidget
@OMETpolarizerWidget_event
@OMETretarderWidget
@OMETretarderWidget_event
@OMETrotatorWidget
@OMETrotatorWidget_event
@OMETStokesVectorWidget
@OMETStokesVectorWidget_event
@OMETsampleWidget
@OMETsampleWidget_event
@EllipsometryTool_event
@OMETfindchainID
@OMETdebug
@OMETshufflewidgets
@OMETwidgetsensitivity
@OMETgetcurchainID
@OMETremovewidget
;
; Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
; EMsoft:EllipsometryTool.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EllipsometryTool.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Simple user interface to create a chain of adjustable optical elements and display the resulting polarization ellipse
;
;> @date 02/15/17 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro EllipsometryTool,dummy
;
;------------------------------------------------------------
; common blocks  (OMET = Optical Microscopy Ellipsometry Tool)
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr

common SHUFFLE_common, shuffleflag

shuffleflag = 1

;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("EllipsometryTool") NE 0) then begin
  print,'EllipsometryTool is already running ... (if it is not, please restart your IDL session)'
  return
end

; set the maximum number of optical elements, including sample and input/output Stokes vectors
maxnumoptelem = 13
device,decomposed=0

; do not report exceptions, such as division by zero etc...
!EXCEPT=0

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
OMETwidget_s = {widgetstruct, $
	  base:long(0), $               ; base widget ID
    status:long(0), $             ; program status widget
    mainstop:long(0), $           ; stop button
    logfile:long(0), $            ; logfile widget
    resetchain:long(0), $         ; button to reset the optical chain to the initial default (no optical elements)
    loadchain:long(0), $          ; button to load a chain description file
    savechain:long(0), $          ; sbutton to save a chain description file
    chainIDs:lonarr(maxnumoptelem), $
    deletewidget:lonarr(maxnumoptelem), $
    pxvals:lonarr(maxnumoptelem), $
    pyvals:lonarr(maxnumoptelem), $
    pvals:lonarr(maxnumoptelem), $
    pthetas:lonarr(maxnumoptelem), $
    phivals:lonarr(maxnumoptelem), $
    rthetas:lonarr(maxnumoptelem), $
    rotthetas:lonarr(maxnumoptelem), $
    alphavals:lonarr(maxnumoptelem), $
    inputchi:long(0), $
    inputpsi:long(0), $
    inputalpha:long(0), $
    inputdelta:long(0), $
    outputchi:long(0), $
    outputpsi:long(0), $
    outputalpha:long(0), $
    outputdelta:long(0), $
    inputpolarization:long(0), $
    outputpolarization:long(0), $
    logodraw:long(0), $           ; top widget with sponsor logo
    logodrawID:long(0), $
    PEdraw:long(0), $
    PEdrawID:long(0), $
    createpolarizer:long(0), $
    createretarder:long(0), $
    createrotator:long(0), $
    loadEMOMmasterfile:long(0), $
    numleft:long(0), $
    inputSV0:long(0), $           ; widgets for the input Stokes vector
    inputSV1:long(0), $
    inputSV2:long(0), $
    inputSV3:long(0), $
    outputSV0:long(0), $           ; widgets for the input Stokes vector
    outputSV1:long(0), $
    outputSV2:long(0), $
    outputSV3:long(0)}  ; end of widget_s structure definition

OMETdata = {OMETdatastruct, $
  ; program control parameters
    logmode: fix(0), $
    logunit: fix(0), $
    eventverbose: fix(1), $
    logname:'', $
    logfileopen:fix(0), $
    maxnumoptelem:fix(maxnumoptelem), $
    numleft:fix(maxnumoptelem), $
    availableOEnumber:fix(0), $
    xstepsize:fix(160), $
    ylocationwidgets:fix(650), $
    inputchi:float(0), $
    inputpsi:float(0), $
    inputalpha:float(0), $
    inputdelta:float(0), $
    outputchi:float(0), $
    outputpsi:float(0), $
    outputalpha:float(0), $
    outputdelta:float(0), $
    inputpolarization:float(0), $
    outputpolarization:float(0), $
    PEdrawsize:fix(451), $
    masterfilename:'', $
    homefolder:'', $
    masterpathname:'', $
    HDFsuffix:'', $
    OMETroot:'undefined', $
    MPx:long(0), $
    MPy:long(0), $


	; widget location parameters
	  xlocation: float(0.0), $		; main widget x-location (can be modified and stored in preferences file)
    ylocation: float(0.0), $		; main widget y-location (can be modified and stored in preferences file)
    scrdimx:0L, $                   ; display area x size in pixels 
    scrdimy:0L}                   ; display area y size in pixels 

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
OMETdata.scrdimy = scr[1] * 0.8
OMETdata.scrdimx = scr[0]
OMETdata.xlocation = OMETdata.scrdimx / 32.0
OMETdata.ylocation = OMETdata.scrdimx / 64.0

;------------------------------------------------------------
;------------------------------------------------------------
; we'll keep track of the optical elements by means of an array of pointers to
; structures that describe each of the elements in detail
optelemptr = ptrarr(maxnumoptelem)

; oetypes are as follows:
;
; 1 : polarizer
; 2 : retarder
; 3 : rotator
; 4 : Stokes vector (source)
; 5 : sample (which has its own Mueller matrix)
; 6 : Stokes vector (detector)
;

; we count the incident and outgoing Stokes vectors as elements, as well as the sample
OMETdata.numleft = 13
OMETdata.availableOEnumber = 0

; first the incoming Stokes vector
optelemptr[OMETdata.availableOEnumber] = PTR_NEW(OMETinitStokesVector(OMETdata.availableOEnumber,[1.D0, 0.D0, 0.D0, 0.D0]))

; then the sample, which is initialized as a perfectly neutral object for now (identity matrix)
optelemptr[OMETdata.availableOEnumber] = PTR_NEW(OMETinitSample(OMETdata.availableOEnumber))

; and finally the outgoing Stokes vector
optelemptr[OMETdata.availableOEnumber] = PTR_NEW(OMETinitStokesVector(OMETdata.availableOEnumber,[1.D0, 0.D0, 0.D0, 0.D0], /output))

; these can be addressed as follows:  (*optelemptr[i]).structureelement

; one can copy an entry to a new entry as follows: optelemptr[3] = PTR_NEW(*optelemptr[1]) 
; assumes that 3 has not be addressed and 1 was properly initialized
; 1 can be nulled by using: optelemptr[1] = PTR_NEW()
;------------------------------------------------------------
;------------------------------------------------------------


;------------------------------------------------------------
; does the preferences file exist ?  If not, create it, otherwise read it
;OMETgetpreferences,/noprint

;------------------------------------------------------------
; create the top level widget
OMETwidget_s.base = WIDGET_BASE(TITLE='Optical Microscopy Ellipsometry Tool', $
                      /ROW, $
                      XSIZE=1200, $
                      /ALIGN_LEFT, $
  						        /TLB_MOVE_EVENTS, $
  				        		EVENT_PRO='EllipsometryTool_event', $
                      XOFFSET=OMETdata.xlocation, $
                      YOFFSET=OMETdata.ylocation)

;------------------------------------------------------------
; create the two main columns
; block 1 is the left column, with logo, input Stokes vector, and add buttons for optical elements 
block1 = WIDGET_BASE(OMETwidget_s.base, $
    			/FRAME, $
    			XSIZE=610, $
    			/ALIGN_LEFT, $
    			/COLUMN)

OMETwidget_s.logodraw = WIDGET_DRAW(block1, $
                    			COLOR_MODEL=2, $
                    			RETAIN=2, $
                    			/FRAME, $
                    			/ALIGN_CENTER, $
                    			XSIZE=600, $
                    			YSIZE=200)

;------------------------------------------------------------
;------------------------------------------------------------
; this is the block that will display the components of the input Stokes vector
; as well as the add buttons for all the optical elements
block21 = WIDGET_BASE(block1, /FRAME, /COLUMN, XSIZE=600, /ALIGN_RIGHT)
file2 = WIDGET_BASE(block21, /ROW, XSIZE=600, /ALIGN_RIGHT)

;---------- add the creation buttons for optical elements
file2 = WIDGET_BASE(block21, /ROW, XSIZE=600, /ALIGN_RIGHT)

file3 = WIDGET_LABEL(file2, VALUE='Select an optical element:', font=fontstr, /ALIGN_LEFT)

OMETwidget_s.createpolarizer = WIDGET_BUTTON(file2, $
                                UVALUE='CREATEPOLARIZER', $
                                VALUE='New polarizer', $
                                EVENT_PRO='EllipsometryTool_event', $
                                SENSITIVE=1, $
                                /FRAME)

OMETwidget_s.createretarder = WIDGET_BUTTON(file2, $
                                UVALUE='CREATERETARDER', $
                                VALUE='New retarder', $
                                EVENT_PRO='EllipsometryTool_event', $
                                SENSITIVE=1, $
                                /FRAME)

OMETwidget_s.createrotator  = WIDGET_BUTTON(file2, $
                                UVALUE='CREATEROTATOR', $
                                VALUE='New rotator', $
                                EVENT_PRO='EllipsometryTool_event', $
                                SENSITIVE=1, $
                                /FRAME)

; and add a counter to show how many optical elements the user can still pick
OMETwidget_s.numleft = Core_WText(file2,' # available', fontstr, 90, 25, 3, 1, string(OMETdata.numleft,format="(I2)"))

file11 = WIDGET_BASE(block21, XSIZE=600, /ROW)
OMETwidget_s.inputchi= Core_WTextE(file11,'Input: chi ', fontstr, 90, 25, 10, 1, string(OMETdata.inputchi,format="(F10.6)"),'INPUTCHI','EllipsometryTool_event')
OMETwidget_s.inputpsi= Core_WTextE(file11,' psi ', fontstr, 30, 25, 10, 1, string(OMETdata.inputpsi,format="(F10.6)"),'INPUTPSI','EllipsometryTool_event')
OMETwidget_s.inputalpha= Core_WTextE(file11,' alpha ', fontstr, 50, 25, 10, 1, string(OMETdata.inputalpha,format="(F10.6)"),'INPUTALPHA','EllipsometryTool_event')
OMETwidget_s.inputdelta= Core_WTextE(file11,' delta ', fontstr, 50, 25, 10, 1, string(OMETdata.inputdelta,format="(F10.6)"),'INPUTDELTA','EllipsometryTool_event')

;------------------------------------------------------------
;------------------------------------------------------------
; then we have the program message window

OMETwidget_s.status= WIDGET_TEXT(block1, $
			XSIZE=94, $
			YSIZE=10, $
			/SCROLL, $
			VALUE=' ',$
			/ALIGN_LEFT)

; the following is needed by the Core_Print routine
status = OMETwidget_s.status 

;------------------------------------------------------------
;------------------------------------------------------------
; finally we need control buttons: Quit, LoadOpticalChain

file11 = WIDGET_BASE(block1, XSIZE=590, /FRAME, /ROW)

OMETwidget_s.loadchain = WIDGET_BUTTON(file11, $
                          UVALUE='LOADOPTICALCHAIN', $
                          VALUE='Load Optical Chain', $
                          EVENT_PRO='EllipsometryTool_event', $
                          SENSITIVE=1, $
                          /FRAME)

OMETwidget_s.savechain = WIDGET_BUTTON(file11, $
                          UVALUE='SAVEOPTICALCHAIN', $
                          VALUE='Save Optical Chain', $
                          EVENT_PRO='EllipsometryTool_event', $
                          SENSITIVE=0, $
                          /FRAME)

OMETwidget_s.resetchain = WIDGET_BUTTON(file11, $
                          UVALUE='RESETOPTICALCHAIN', $
                          VALUE='Reset Optical Chain', $
                          EVENT_PRO='EllipsometryTool_event', $
                          SENSITIVE=0, $
                          /FRAME)

OMETwidget_s.mainstop = WIDGET_BUTTON(file11, $
                          VALUE='Quit', $
                          UVALUE='QUIT', $
                          EVENT_PRO='EllipsometryTool_event', $
                          SENSITIVE=1, $
                          /FRAME)

values = ['Off','On']
OMETwidget_s.logfile= CW_BGROUP(file11, $
              					values, $
              					/FRAME, $
                        LABEL_LEFT='LogFile', $
              					/ROW, $
              					/NO_RELEASE, $
              					/EXCLUSIVE, $
              					SET_VALUE=OMETdata.logmode, $
                        EVENT_FUNC='OMETbuttonevent', $
              					UVALUE='LOGFILE')

; the following is needed by the Core_Print routine
logmode = OMETdata.logmode
logunit = OMETdata.logunit

; finally, we create the right hand portion of the GUI, which contains the 
; display for the polarization ellipses (input and output) as well as 
; the ellipse parameters
block2 = WIDGET_BASE(OMETwidget_s.base, $
          /FRAME, $
          XSIZE=590, $
          /ALIGN_LEFT, $
          /COLUMN)

OMETwidget_s.PEdraw = WIDGET_DRAW(block2, $
                          COLOR_MODEL=2, $
                          RETAIN=2, $
                          /FRAME, $
                          /ALIGN_CENTER, $
                          XSIZE=OMETdata.PEdrawsize, $
                          YSIZE=OMETdata.PEdrawsize)

; and add a counter to show how many optical elements the user can still pick
file11 = WIDGET_BASE(block2, XSIZE=580, /ROW)
OMETwidget_s.inputpolarization = Core_WText(file11,'Input Polarization P ', fontstr, 160, 25, 10, 1, string(OMETdata.inputpolarization,format="(F10.6)"))
OMETwidget_s.outputpolarization = Core_WText(file11,'; Output Polarization P ', fontstr, 175, 25, 10, 1, string(OMETdata.outputpolarization,format="(F10.6)"))

file11 = WIDGET_BASE(block2, XSIZE=580, /ROW)
OMETwidget_s.outputchi= Core_WText(file11,'Output: chi ', fontstr, 90, 25, 10, 1, string(OMETdata.outputchi,format="(F10.6)"))
OMETwidget_s.outputpsi= Core_WText(file11,' psi ', fontstr, 30, 25, 10, 1, string(OMETdata.outputpsi,format="(F10.6)"))
OMETwidget_s.outputalpha= Core_WText(file11,' alpha ', fontstr, 50, 25, 10, 1, string(OMETdata.outputalpha,format="(F10.6)"))
OMETwidget_s.outputdelta= Core_WText(file11,' delta ', fontstr, 50, 25, 10, 1, string(OMETdata.outputdelta,format="(F10.6)"))


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,OMETwidget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, OMETwidget_s.PEdraw, GET_VALUE=PEdrawID
OMETwidget_s.PEdrawID = PEdrawID

WIDGET_CONTROL, OMETwidget_s.logodraw, GET_VALUE=drawID
OMETwidget_s.logodrawID = drawID
;
read_jpeg,'../Resources/EMsoftVBFFlogo.jpg',logo
;read_jpeg,'Resources/EMsoftlogo.jpg',logo
wset,OMETwidget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"EllipsometryTool",OMETwidget_s.base,/NO_BLOCK

; at this point we need to generate the widgets for all existing optical elements;
; in the default case, this will simply be the incident and outgoing Stokes vectors
; and the sample

OMETupdateOEwidgets

Core_Print,'Program successfully initialized'
Core_Print,'-----'

end

