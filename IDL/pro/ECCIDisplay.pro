@Core_WText			; core text widget creation 
@Core_WTextE			; core editable text widget creation 
@ECCIDisplay_event    		; compile the ECCI event handler
@ECCIevent    			; compile the ECCI CW_BGROUP event handler
@ECCIgetpreferences		; load preferences
@ECCIwritepreferences		; save preferences
@ECCIgetfilename		; select a geometry file
@ECCIreaddatafile		; read geometry and data files
@ECCIprogressbar		; display a progress bar during CBED pattern computation
@ECCIprint			; print to the log window or file
@ECCIECPShow   			; ECP drawing routine
;@ECCImageShow   		; ECCI drawing routine
@ECCIECPWidget 			; ECP widget
@ECCIECPWidget_event            ; ECP widget event handler
@ECCImageWidget			; ECCI display widget
@ECCImageWidget_event		; ECCI display widget event handler
@ECCIblur			; Gaussian blurring of ECCI images

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
; EMsoft:ECCIDisplay.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECCIDisplay.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Near zone axis ECCI defect image display
;
;> @date 12/05/13 MDG 1.0 first attempt at a user-friendly interface
;> @date 01/19/17 MDG 2.0 updated for HDF5 input and a few other things...
;--------------------------------------------------------------------------
pro ECCIDisplay,dummy
;
;------------------------------------------------------------
; common blocks
common ECCI_widget_common, widget_s
common ECCI_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; and two common blocks for the ECP data
common ECP_data_common, ECPdata
common ECP_rawdata, ECPrawdata

!EXCEPT=0

;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("ECCIDisplay") NE 0) then begin
  print,'ECCIDisplay is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
widget_s = {widgetstruct, $
            base:long(0), $                     ; base widget ID
            status:long(0), $                   ; status text widget ID
            filesize:long(0), $                 ; main file size widget ID
            progmode:long(0), $                 ; program mode widget ID
            summode:long(0), $                  ; summation mode widget ID
            progress:long(0), $                 ; progress status bar widget ID
            progressdrawID:long(0), $           ; progress status bar widget draw ID
            logodraw:long(0), $                 ; logo widget ID
            logodrawID:long(0), $               ; logo draw widget ID
            logfile:long(0), $                  ; log file widget ID
            mainstop:long(0), $                 ; stop button
            loadfile:long(0), $                 ; load file button
            numref:long(0), $                   ; number of reflections widget ID
            xtalname:long(0), $                 ; structure file name widget ID
            blur:long(0), $                     ; image blur widget ID
            numk:long(0), $                     ; number of wave vectors widget ID
	    ECCImagebase:long(0), $             ; base widget for ECCI Image display
	    ECCIECPbase:long(0), $              ; base widget for ECP Image display
	    ECPgridbgroup:long(0), $            ; ECP grid toggle widget
	    ECPformatbgroup:long(0), $          ; ECP image format selection widget
	    ECCIformatbgroup:long(0), $         ; ECCI image format selection widget
	    ECCImosaicbgroup:long(0), $		; mosaic intensity scaling selection widget
            ktmax:long(0), $                    ; ktmax widget ID
            dkt:long(0), $                      ; kt step size widget ID
            cx:long(0), $                       ; Laue x coordinate
            cy:long(0), $                       ; Laue y coordinate
            mosaicdim:long(0), $                ; dimension of ECCI mosaic [pixels]
            avrad:long(0), $                    ; radius for ECCI image averaging in units of |ga|
            ECCIcx:long(0), $                   ; Laue x coordinate in ECCI widget
            ECCIcy:long(0), $                   ; Laue y coordinate in ECCI widget
            imx:long(0), $                      ; image x size
            imy:long(0), $                      ; image y size
            voltage:long(0), $                  ; voltage widget ID
            thetac:long(0), $                   ; beam convergence widget ID
            zoneaxis:long(0), $                 ; zone axis indices widget ID
            ECPthicklist:long(0), $             ; ECP thickness list widget ID
            ECPname:long(0), $                  ; ECP name widget ID
            ECPdraw:long(0), $                  ; ECP draw widget
            ECPdrawID:long(0), $                ; ECP draw widget ID
            ECCIdrawmin:long(0), $              ; ECCI draw widget minimum intensity
            ECCIdrawmax:long(0), $              ; ECCI draw widget maximum intensity
            ECCIdraw:long(0), $                 ; ECCI draw widget
            ECCIdrawID:long(0), $               ; ECCI draw widget ID
            ECCIavdraw:long(0), $               ; ECCI average draw widget
            ECCIavdrawID:long(0), $             ; ECCI average draw widget ID
            ECCIavdrawmin:long(0), $            ; ECCI average draw widget minimum intensity
            ECCIavdrawmax:long(0), $            ; ECCI average draw widget maximum intensity
            ECCIname:long(0) $                  ; ECCI name widget ID
           }

data = {datastruct, $
	eventverbose: fix(0), $			; used for event debugging (0=off, 1=on)
	ECPname: '', $				; filename (without pathname)
	ECCIname: '', $				; filename (without pathname)
	ECCIroot: '', $				; root filename 
	pathname: '', $				; pathname (obviously)
	xtalname: '', $				; crystal structure filename (without pathname)
	ecciformat: long(0), $			; file format selector
	voltage: float(0.0), $			; microscope voltage
	thetac: float(0.0), $			; beam convergence
	zoneaxis: lonarr(3), $			; zone axis indices 
	datadims: lonarr(3), $			; dimensions of data array
	mosaicdim: 5L, $			; dimension of ECCI mosaic
	mosaicscale: fix(0), $			; global or by-image intensity scaling for mosaic and ECCI display
	avrad: float(0.0), $			; radius for ECCI pattern averaging
	bragg: float(0.0), $			; Bragg angle for g_a
	ktmax: float(0.0), $			; max beam tilt 
	dkt: float(0.0), $			; beam tilt step size
	dfl: float(0.0), $			; image pixel size
	numref: long(0), $			; number of reflections
	numk: long(0), $			; number of wave vectors
	blur: float(0),$			; blurring radius (Gaussian filter)
	suffix: '', $				; filename suffix 
	prefname: '~/.ECCIgui.prefs', $		; filename of preferences file (including path)
	filesize: long64(0), $			; input file size in bytes
	homefolder: '', $			; startup folder of the program
	STEMroot: 'undefined', $		; current pathname (is stored in preferences file)
	nprefs: fix(0), $			; number of preferences in file
        status:'waiting for input', $           ; current status line
	progmode:'array', $			; program mode (array or image)
	summode:'diag', $			; summation mode (full or diag)
	logmode: fix(0), $			; keep a log file or not
	logunit: fix(13), $			; logical file unit for log file
	logname: '', $				; filename for log output
	logfileopen: fix(0), $			; log file is open when 1, closed when 0
        ECCIdrawmin:float(0.0), $               ; ECCI min intensity
        ECCIdrawmax:float(0.0), $               ; ECCI max intensity
        ECCIavdrawmin:float(0.0), $             ; ECCI average min intensity
        ECCIavdrawmax:float(0.0), $             ; ECCI average max intensity
        xlocation:0L, $                         ; main window x location
        ylocation:0L, $                         ; main window y location
        ECPxlocation:0L, $                      ; ECP  window x location
        ECPylocation:0L, $                      ; ECP  window y location
        ECCIxlocation:0L, $                     ; ECCI window x location
        ECCIylocation:0L, $                     ; ECCI window y location
        scrdimx:0L, $                           ; display area x size in pixels 
        scrdimy:0L $                            ; display area y size in pixels 
        }

; modified from ECPDisplay to suit the needs of this program...
ECPdata = {ECPdatastruct, $
	dataname: '', $				; filename 
	pathname: '', $				; pathname (obviously)
	progname: '',$				; program name
	scversion: '',$				; program version
	datadims: lon64arr(3), $		; dimensions of rawdata array
	xtalname: '', $				; crystal structure filename
	padding: fix(0), $			; do we need to pad the ECP pattern ?
	distort: long(0), $			; is the cell distorted ?
	abcdist:fltarr(3), $			; lattice parameters
	albegadist:fltarr(3), $			; lattice angles
	voltage: float(0.0), $			; microscope voltage
	wavelength: float(0.0), $		; electron wavelength
	thetac: float(0.0), $			; beam divergence angle [mrad]
	ktmax: float(0.0), $			; beam divergence angle [units of ga]
	delta: float(0.0), $			; scale factor
	gperp: fltarr(3), $			; vector normal to ga
	wavek: lonarr(3), $			; wave vector indices
	fn: lonarr(3), $			; foil normal
	numk: long(0), $			; number of wave vectors in ECP pattern
	dmin: long(0), $			; smallest d-spacing 
	xmid: long(0), $			; half the pattern size
	dgrid: float(0.0), $			; coordinate grid-spacing 
	ecpgrid: long(0), $			; grid toggle
	ecpformat: long(0), $			; file format selector
	cx: float(0.0), $			; x-coordinate in pattern 
	cy: float(0.0), $			; y-coordinate in pattern
	kt: float(0.0), $			; auxiliary parameter for drawing grid lines
	ga: lonarr(3), $			; horizontal g-vector in pattern
	galen: float(0.0), $			; horizontal g-vector length
	symgroups: lonarr(8), $			; symmetry group numbers for zone axis symmetry
	startthick: float(0.0), $		; starting integration depth
	thickinc: float(0.0), $			; integration depth step size
	thicksel: long(0), $			; selected integration depth
	imx: long(0), $				; number of image pixels along x
	imy: long(0), $				; number of image pixels along y
	nums: long(0), $			; number of pixels along disk radius (diameter = 2*nums+1)
	scale: float(0.0) $			; scale factor for CBED, [number of pixels per reciprocal nanometer]
        }

; a few font strings
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; get the display window size to 80% of the current screen size (but be careful with double screens ... )
device,decomposed = 0
device, GET_SCREEN_SIZE = scr
data.scrdimy = scr[1] * 0.8
data.scrdimx = 0.75 * data.scrdimy   ; doing it this way avoids problems with multiple screens
data.xlocation = data.scrdimx / 8.0
data.ylocation = data.scrdimx / 8.0

;------------------------------------------------------------
; does the preferences file exist ?  If not, create it, otherwise read it
ECCIgetpreferences

;------------------------------------------------------------
; create the top level widget
widget_s.base = WIDGET_BASE(TITLE='Zone Axis ECCI Display Program', $
                        /COLUMN, $
                        XSIZE=700, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='ECCIDisplay_event', $
                        XOFFSET=data.xlocation, $
                        YOFFSET=data.ylocation)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the input file and displays the data dimensions
block1 = WIDGET_BASE(widget_s.base, $
			/FRAME, $
			/ALIGN_CENTER, $
			/COLUMN)

widget_s.logodraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/ALIGN_CENTER, $
			XSIZE=600, $
			YSIZE=200)

;----------
;----------
block1 = WIDGET_BASE(widget_s.base, /FRAME, /COLUMN)

;----------
file1 = WIDGET_BASE(block1, /ROW, XSIZE=700, /ALIGN_CENTER)
widget_s.ECPname = Core_WText(file1, 'ECP data file  ',fontstrlarge, 200, 25, 77, 1, data.ECPname)

;----------
file1 = WIDGET_BASE(block1, /ROW, XSIZE=700, /ALIGN_CENTER)
widget_s.ECCIname = Core_WText(file1, 'ECCI data file ',fontstrlarge, 200, 25, 77, 1, data.ECCIname)

;----------
file1 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.imx = Core_WText(file1, 'Image Dimensions',fontstrlarge, 200, 25, 10, 1, string(data.datadims[0],FORMAT="(I5)"))
widget_s.imy = Core_WText(file1, 'by',fontstrlarge, 25, 25, 10, 1, string(data.datadims[1],FORMAT="(I5)"))

;----------
widget_s.progress = WIDGET_DRAW(file1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/ALIGN_RIGHT, $
			XSIZE=200, $
			YSIZE=20)


;----------
;---------- next we have a series of parameters that are 
; derived from the input file and can not be changed by
; the user...

block2 = WIDGET_BASE(widget_s.base, /FRAME, /ROW)
;-------------
;-------------
file4 = WIDGET_BASE(block2, /COLUMN, /ALIGN_LEFT)

;-------------
file1 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.numref = Core_WText(file1, '# of g-vectors',fontstrlarge, 230, 25, 10, 1, string(data.numref,FORMAT="(I5)"))

;-------------
file1 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.thetac = Core_WText(file1, 'Beam Convergence [mrad]',fontstrlarge, 230, 25, 10, 1, string(data.thetac,FORMAT="(F6.3)"))

;-------------
file1 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.voltage = Core_WText(file1, 'Accelerating Voltage [V]',fontstrlarge, 230, 25, 10, 1, string(data.voltage,FORMAT="(F6.3)"))

;-------------
file1 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.ktmax = Core_WText(file1, 'Maximum k_t value [|ga|]',fontstrlarge, 230, 25, 10, 1, string(data.ktmax,FORMAT="(F6.3)"))

;-------------
file1 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.dkt = Core_WText(file1, 'k_t step size [|ga|]',fontstrlarge, 230, 25, 10, 1, string(data.dkt,FORMAT="(F6.3)"))

;-------------
;-------------
file6 = WIDGET_BASE(block2, /COLUMN, /ALIGN_LEFT)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.numk = Core_WText(file7, '# of k-vectors',fontstrlarge, 200, 25, 10, 1, string(data.numk,FORMAT="(I5)"))

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.xtalname = Core_WText(file7, 'Structure File',fontstrlarge, 200, 25, 10, 1, data.xtalname)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
wv = '['+string(data.zoneaxis[0],format="(I2)")+' '+ string(data.zoneaxis[1],format="(I2)")+' '+ string(data.zoneaxis[2],format="(I2)")+']'
widget_s.zoneaxis = Core_WText(file7, 'Zone Axis [uvw]',fontstrlarge, 200, 25, 10, 1, wv)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.progmode = Core_WText(file7, 'Program Mode        ',fontstrlarge, 200, 25, 10, 1, data.progmode)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.summode = Core_WText(file7, 'Summation Mode      ',fontstrlarge, 200, 25, 10, 1, data.summode)

;-------------
;-------------
; next, add a text window for program messages

widget_s.status= WIDGET_TEXT(widget_s.base, $
			XSIZE=115, $
			YSIZE=10, $
			/SCROLL, $
			VALUE=' ',$
			/ALIGN_CENTER)

;------------------------------------------------------------
; block 3 QUIT button, LOAD FILE button and progress bar (used for file loading)
block3 = WIDGET_BASE(widget_s.base, $
			XSIZE=650, $
			/FRAME, $
			/ROW)

file11 = WIDGET_BASE(block3, $
			/ROW, $
			/ALIGN_LEFT)

widget_s.mainstop = WIDGET_BUTTON(file11, $
                                VALUE='Quit', $
                                UVALUE='QUIT', $
                                EVENT_PRO='ECCIDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

widget_s.loadfile = WIDGET_BUTTON(file11, $
                                VALUE='ECCI File', $
                                UVALUE='LOADFILE', $
                                EVENT_PRO='ECCIDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

values = ['Off','On']
widget_s.logfile= CW_BGROUP(file11, $
			values, $
			/FRAME, $
                        LABEL_LEFT='LogFile', $
			/ROW, $
			/NO_RELEASE, $
			/EXCLUSIVE, $
			SET_VALUE=data.logmode, $
                        EVENT_FUNC='ECCIevent', $
			UVALUE='LOGFILE')


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.logodraw, GET_VALUE=drawID
widget_s.logodrawID = drawID
WIDGET_CONTROL, widget_s.progress, GET_VALUE=drawID
widget_s.progressdrawID = drawID
;
logo = read_image('Resources/SEMlogo.jpg')
wset,widget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"ECCIDisplay",widget_s.base,/NO_BLOCK

; init the status text window
ECCIprint,'Zone Axis ECCI Defect Image Display Program [M. De Graef, 2013-2017]',/blank

end

