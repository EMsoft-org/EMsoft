@Core_WText			; generate a text widget with a label
@Core_WTextE			; generate an editable text widget with a label
@Core_Print			; print messages to status window and log file
@Core_WidgetEvent		; general data handler for various widget events
@Core_getenv                    ; read the environment variable(s)
@DPADisplay_event               ; event handles for this program
@DPAevent
@DPAgetfilename
@DPAloadfile
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
; EMsoft:DPanalysis.pro
;--------------------------------------------------------------------------
;
; PROGRAM: DPanalysis.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Interactive GUI for analysis of dictionary indexing dot product results
;
;> @details 06/07/16 First attempt at producing a useful analysis widget interface
;> to produce a number of patterns and other data displays; based on a number of 
;> standalone routines that turned out to be useful...
;>
;> the user should be able to do the following things:
;>
;> - read dot-product h5 file and extract all important arrays
;> - read ctf files and merge them together
;> - display ADP, CI, IQ, IPF, and other maps
;> - display dp histograms and set shift parameters if necessary with automatic updates to all displayed maps
;> - handle up to three phases (or look into object oriented approach...)
;> - display master patterns and MC output for each of the phases
;> - compute EBSP for given point selected by user and compare with experimental pattern
;> - compute and display average near-match patterns
;> - 
;>
;
;> @date 06/07/16 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro DPanalysis,dummy
;
;------------------------------------------------------------
; common blocks
common DPA_widget_common, DPAwidget_s
common DPA_data_common, DPAcommon, DPAdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common CommonCore, status, logmode, logunit

!EXCEPT=0

;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("DPanalysis") NE 0) then begin
  print,'DPanalysis is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
DPAwidget_s = {widgetstruct, $
	base:long(0), $                     	; base widget ID
	logodraw:long(0), $                    	; logo draw widget ID
	logodrawID:long(0), $                   ; logo draw widget ID
	numphases:long(0), $                   	; number of phases button
        status:long(0), $                       ; status widget
        mainstop:long(0), $                     ; program stop widget
	dp2button:long(0), $                    ; filename of first dot product file
	dp1filename:long(0), $                  ; filename of first dot product file
	dp2filename:long(0)  $                  ; filename of first dot product file
           }

; this structure stores all the data that is common for the widget and the phases
DPAcommon = {DPAcommonstruct, $
    	     logmode: fix(0), $			; keep a log file or not
	     logunit: fix(13), $	        ; logical file unit for log file
	     logname: '', $			; filename for log output
	     logfileopen: fix(0), $		; log file is open when 1, closed when 0
             f90exepath:'', $
             homefolder:'', $
             DPAroot:'undefined', $
             numphases:long(0), $
             currentphase:long(0), $
             eventverbose:long(0), $
             EMsoftpathname:'', $
             xlocation:long(0), $
             ylocation:long(0), $
             scrdimx:long(0), $
             scrdimy:long(0) $
            }

; this array of structures stores phase-specific data 
; this is tricky because this structure contains pointers, so we can not use 
; the usual replicate command to generate the array of structures... We must 
; define the pointers but not initialize them...

DPAdatastructure = {DPAdatastruct, $
	     dpfilename:'', $                  ; filename of first dot product file
	     dpfilesize:long(0), $             ; filesize of first dot product file
	     dppathname:'', $                  ; path of second dot product file
             dpsuffix:'', $                    ; file suffix of first dot product file
             ctffile:'', $                     ; filename for CTF file
             EulerAngles:ptr_new(), $          ; Euler angles from dictionary (radians)
             Eulers:ptr_new(), $               ; Euler angles from top matches
             tdp:ptr_new(), $                  ; top dot product list
             tmi:ptr_new(), $                  ; top matching indices
             ADPmap:ptr_new(), $               ; Average Dot Product map (bytes)
             CI:ptr_new(), $                   ; Confidence Index 
             IQ:ptr_new(), $                   ; Image Quality 
             CImap:ptr_new(), $                ; Confidence Index map (bytes)
             IQmap:ptr_new(), $                ; Image Quality map (bytes)
             FZcnt:long(0), $                  ; FZcnt (number of Euler angles in RFZ)
             Ncubochoric:long(0), $            ; number of sampling steps along cubochoric semi-edge
             Nexp:long(0), $                   ; Nexp (number of experimental patterns)
             pgnum:long(0), $                  ; point group number
             ipf_wd:long(0), $                 ; IPF width (pixels)
             ipf_ht:long(0), $                 ; IPF height (pixels)
             nnk:long(0), $                    ; max number of near-matches
             test:long(1) $                    ; EBSD (0) or ECP (1) data type
        }

DPAdata = replicate(DPAdatastructure,2)

; note that the pointers in this array of structures need to allocated next
DPAdata.EulerAngles = PtrArr(2,/allocate_heap)
DPAdata.Eulers = PtrArr(2,/allocate_heap)
DPAdata.tdp = PtrArr(2,/allocate_heap)
DPAdata.tmi = PtrArr(2,/allocate_heap)
DPAdata.ADPmap = PtrArr(2,/allocate_heap)
DPAdata.CI = PtrArr(2,/allocate_heap)
DPAdata.IQ = PtrArr(2,/allocate_heap)
DPAdata.CImap = PtrArr(2,/allocate_heap)
DPAdata.IQmap = PtrArr(2,/allocate_heap)



; a few font strings (this will need to be redone for Windows systems)
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

logmode = DPAcommon.logmode
logunit = DPAcommon.logunit


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
DPAcommon.scrdimy = scr[1] * 0.8
DPAcommon.scrdimx = scr[0]
DPAcommon.xlocation = DPAcommon.scrdimx / 8.0
DPAcommon.ylocation = DPAcommon.scrdimx / 8.0

;------------------------------------------------------------
; does the preferences file exist ?  If not, create it, otherwise read it
;DPAgetpreferences,/noprint

;------------------------------------------------------------
; get the pathname for the executables and the EMsoftLib library
DPAcommon.f90exepath = Core_getenv(/bin)
DPAcommon.EMsoftpathname = Core_getenv(/bin)
librarylocation = Core_getenv(/lib)

;------------------------------------------------------------
; create the top level widget
DPAwidget_s.base = WIDGET_BASE(TITLE='Dot Product Analysis Program', $
                        /COLUMN, $
                        XSIZE=620, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='DPADisplay_event', $
                        XOFFSET=DPAcommon.xlocation, $
                        YOFFSET=DPAcommon.ylocation)

;------------------------------------------------------------
; create the main column
; block 1 is the left column, with logo and MC information
block1 = WIDGET_BASE(DPAwidget_s.base, $
			/FRAME, $
			XSIZE=610, $
			/ALIGN_CENTER, $
			/COLUMN)

DPAwidget_s.logodraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/ALIGN_CENTER, $
			XSIZE=600, $
			YSIZE=200)

;------------------------------------------------------------
;------------------------------------------------------------
; this is the block that will ask the user whether there is one phase or two phases
block11 = WIDGET_BASE(block1, /FRAME, /COLUMN)

file1 = WIDGET_BASE(block11, /ROW, XSIZE=600, /ALIGN_RIGHT)

values = ['1','2']
DPAwidget_s.numphases = CW_BGROUP(file1, $
			values, $
			/FRAME, $
                        LABEL_LEFT='Number of Phases', $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			SET_VALUE=DPAcommon.numphases, $
                        EVENT_FUNC='DPAevent', $
			UVALUE='NUMPHASES')

file1 = WIDGET_BASE(block11, /ROW, XSIZE=600, /ALIGN_CENTER)
button = WIDGET_BUTTON(file1, $
                          UVALUE='DPFILE1', $
                          VALUE='Load DP file phase 1', $
                          EVENT_PRO='DPADisplay_event', $
                          SENSITIVE=1, $
                          /FRAME)
DPAwidget_s.dp1filename = Core_WText(file1,'DP1 File Name', fontstrlarge, 130, 15, 50, 1, DPAdata[0].dpfilename)

file1 = WIDGET_BASE(block11, /ROW, XSIZE=600, /ALIGN_CENTER)
DPAwidget_s.dp2button = WIDGET_BUTTON(file1, $
                          UVALUE='DPFILE2', $
                          VALUE='Load DP file phase 2', $
                          EVENT_PRO='DPADisplay_event', $
                          SENSITIVE=0, $
                          /FRAME)
DPAwidget_s.dp2filename = Core_WText(file1,'DP2 File Name', fontstrlarge, 130, 15, 50, 1, DPAdata[1].dpfilename)


file11 = WIDGET_BASE(block1, XSIZE=590, /FRAME, /ROW)
DPAwidget_s.mainstop = WIDGET_BUTTON(file11, $
                                VALUE='Quit', $
                                UVALUE='QUIT', $
                                EVENT_PRO='DPADisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

;------------------------------------------------------------
;------------------------------------------------------------
; then we have the program message window

DPAwidget_s.status= WIDGET_TEXT(block1, $
			XSIZE=95, $
			YSIZE=5, $
			/SCROLL, $
			VALUE=' ',$
			/ALIGN_LEFT)

; the following is needed by the Core_Print routine
status = DPAwidget_s.status 

;------------------------------------------------------------
;------------------------------------------------------------

; the following is needed by the Core_Print routine
logmode = DPAcommon.logmode
logunit = DPAcommon.logunit

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,DPAwidget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, DPAwidget_s.logodraw, GET_VALUE=drawID
DPAwidget_s.logodrawID = drawID
;
read_jpeg,'../Resources/EMsoftlogo.jpg',logo
wset,DPAwidget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"DPADisplay",DPAwidget_s.base,/NO_BLOCK

end

