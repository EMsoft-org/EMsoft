; compile some basic auxiliary routines
@CTEM_define_idlvars
@CTEM_load_snapshot
@CTEM_save_snapshot

; compute all event handlers
@CTEMevent_topbgroup

; compile all widget creation routines
@CTEM_InputWidget
@CTEM_ProgramExecuteWidget
@CTEM_ResultDisplayWidget




;---------------------------------------------------------------------
;---------------------------------------------------------------------
; 
; This Graphical User Interface (GUI) for the CTEMsoft package allows
; the user to (1) create namelist input files (the main form of input
; for the CTEMsoft programs; (2) run the programs in background; and (3)
; display the data in graphical form, including saving to various file
; formats.
;
; history:
;
; MDG 1.0 05/14/13 created original framework
;
;---------------------------------------------------------------------
;---------------------------------------------------------------------


pro CTEMgui,dummy
;
; main program section; calls a few other routines
;

;------------------------------------------------------------
; common blocks
common widget_common, widget_s
common data_common, data


;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("CTEMGUI") NE 0) then begin
  print,'CTEMgui is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
widget_s = {widgetstruct, $
	    base:long(0), $			; base widget ID
	    inputbase:long(0), $		; input base widget ID
	    mainstop:long(0), $			; stop button
	    topbgroup:long(0) $			; top level button group widget
	   }

data = {datastruct, $
	status:'waiting for input', $		; current status line
	scrdimx:0L, $				; display area x size in pixels	
	scrdimy:0L $				; display area y size in pixels	
 	}

; a few font strings
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; get the display window size to 80% of the current screen size (but be careful with double screens ... )
device, GET_SCREEN_SIZE = scr
data.scrdimy = scr[1] * 0.8
data.scrdimx = 0.75 * data.scrdimy   ; doing it this way avoids problems with multiple screens

; use regular color tables, not RGB-based color values
device,decomposed = 0

; some initial parameters
set_sensitivity = 0
btnx = 80
xlocation = data.scrdimx / 8.0
ylocation = data.scrdimx / 8.0

;------------------------------------------------------------
; define a series of IDL system variables (mostly pathnames)
CTEM_define_idlvars,/verbose

; load the snapshot file that contains the status from the previous run
CTEM_load_snapshot,/verbose

;------------------------------------------------------------
; create the top level widget
widget_s.base = WIDGET_BASE(TITLE='CTEMsoft GUI', $
			    /COLUMN, $
			    XSIZE=200, $
			    /ALIGN_CENTER, $
			    XOFFSET=xlocation, $
			    YOFFSET=ylocation)

;------------------------------------------------------------
; the top level widget is really only used to select one of the larger widgets:
;   - input file widget
;   - execute widget
;   - display results widget
;
; We'll use Dave Rowenhorst's suggestion of the NO_COPY and UVALUE option on the WIDGET_CONTROL command
; to shuttle data back and forth instead of using a bunch of common blocks that need to be updated all
; the time ... We can have three common blocks between this main widget routine and the individual ones...

; top menu has three options
topvalues = ['Create an input file', 'Execute a program', 'Display program output']
base = WIDGET_BASE(widget_s.base, $
		   /ALIGN_CENTER)

widget_s.topbgroup = CW_BGROUP(base, $
				topvalues, $
				UVALUE='SELECTION', $
				/COLUMN, $
				/EXCLUSIVE, $
				/NO_RELEASE, $
				LABEL_TOP='What do you wish to do?', $
				EVENT_FUNC='CTEMevent_topbgroup', $
				/FRAME)
; and a QUIT button
widget_s.mainstop = WIDGET_BUTTON(widget_s.base, $
				VALUE='Quit', $
				UVALUE='QUIT', $
				EVENT_FUNC='CTEMevent_topbgroup', $
				SENSITIVE=1, $
				/ALIGN_LEFT, $
				/FRAME)

WIDGET_CONTROL,widget_s.base,/REALIZE

XMANAGER,"CTEMGUI",widget_s.base,/NO_BLOCK




end
