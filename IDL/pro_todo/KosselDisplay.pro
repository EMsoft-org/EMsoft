@KosselDisplay_event    	; Kossel event handler
@Kosselgetfilename		; select a geometry file
@Kosselreaddatafile		; read geometry and data files
@KosselPatternWidget		; display widget
@KosselPatternWidget_event	; even handler
@Kosselshow			; show an KosselPattern (with grid)
@Kosselevent			; event handler for button groups
@Kosselgetpreferences		; read preferences file
@Kosselwritepreferences		; write preferences file
;
; Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
; CTEMsoft2013:KosselDisplay.pro
;--------------------------------------------------------------------------
;
; PROGRAM: KosselDisplay.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Zone axis electron Kossel pattern display
;
;> @date 11/23/13 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro KosselDisplay,dummy
;
;------------------------------------------------------------
; common blocks
common Kossel_widget_common, widget_s
common Kossel_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common PointGroups, PGTHD, PGTWD, DG

PGTWD = [ 'none',' 1',' 2',' m',' 2mm',' 4',' 4mm',' 3',' 3m1',' 31m',' 6',' 6mm'] 
PGTHD = ['  ' ,' 1',' -1',' 2',' m',' 2/m',' 222', $
         ' mm2',' mmm',' 4',' -4',' 4/m',' 422', $
         ' 4mm',' -42m','4/mmm',' 3',' -3',' 32', $
         ' 3m',' -3m',' 6',' -6',' 6/m',' 622', $
         ' 6mm',' -6m2',' 6/mmm',' 23',' m3',' 432', $
         ' -43m',' m-3m']
DG = ['  ',' 1',' 1R',' 2',' 2R',' 21R','  mR', $
      ' m',' m1R',' 2mRmR',' 2mm',' 2RmmR',' 2mm1R', $
      ' 4',' 4R',' 41R',' 4mRmR',' 4mm',' 4RmmR', $
      ' 4mm1R',' 3',' 6R',' 3mR',' 3m',' 6RmmR', $
      ' 6',' 31R',' 61R',' 6mRmR',' 6mm',' 3m1R', $
      ' 6mm1R']

!EXCEPT=0

;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("KosselDisplay") NE 0) then begin
  print,'KosselDisplay is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
widget_s = {widgetstruct, $
            base:long(0), $                     ; base widget ID
            KosselPatternbase:long(0), $        ; display window widget ID
            Kosselthicklist:long(0), $          ; integration depth widget
            Kosselformatbgroup:long(0), $       ; file format button group
            KosselDraw:long(0), $               ; pattern draw widget
            KosselDrawID:long(0), $             ; pattern draw widget ID
            filename:long(0), $                 ; file name field
            xtalname:long(0), $                 ; structure file name field
            filesize:long(0), $                 ; file size field
            mainstop:long(0), $                 ; program quit button
            loadfile:long(0), $                 ; load file button
            symCPG:long(0), $                   ; crystallographic point group
            symWPG:long(0), $                   ; whole pattern symmetry group
            imx:long(0), $                      ; pattern x dimension
            imy:long(0), $                      ; pattern y dimension
            numk:long(0), $                     ; number of wave vectors used 
            numthick:long(0), $                 ; number of thickness values
            thetac:long(0), $                   ; pattern convergence angle
            wavek:long(0), $                    ; zone axis indices 
            blur:float(0.0), $                  ; blur factor widget
            voltage:long(0), $                  ; microscope voltage
            abcdist:long(0), $                  ; lattice parameters
            albegadist:long(0), $               ; lattice parameters (angles)
            logodraw:long(0), $                 ; logo widget 
            logodrawID:long(0) $                ; logo widget ID
           }

data = {datastruct, $
	eventverbose: fix(0), $			; used for event debugging (0=off, 1=on)
	dataname: '', $				; filename (without pathname)
	pathname: '', $				; pathname (obviously)
	suffix: '', $				; filename suffix 
	filesize: long64(0), $			; input file size in bytes
	homefolder: '', $			; startup folder of the program
	Kosselroot: 'undefined', $		; current pathname (is stored in preferences file)
	prefname: '~/.Kosselgui.prefs', $	; filename of preferences file (including path)
	nprefs: fix(0), $			; number of preferences in file
	progname: '',$				; program name
	scversion: '',$				; program version
	datadims: lon64arr(3), $		; dimensions of rawdata array
	xtalname: '', $				; crystal structure filename
	distort: long(0), $			; is the cell distorted ?
	voltage: float(0.0), $			; microscope voltage
	wavelength: float(0.0), $		; electron wavelength
	ktmax: float(0.0), $			; beam divergence angle [units of ga]
	delta: float(0.0), $			; scale factor
	gperp: fltarr(3), $			; vector normal to ga
	blur: float(0.0), $			; blurring factor
	thetac: float(0.0), $			; beam divergence angle [mrad]
	wavek: lonarr(3), $			; wave vector indices
	fn: lonarr(3), $			; foil normal
	numk: long(0), $			; number of wave vectors in Kossel pattern
	dmin: long(0), $			; smallest d-spacing 
	xmid: long(0), $			; half the pattern size
	kt: float(0.0), $			; auxiliary parameter for drawing grid lines
	ga: lonarr(3), $			; horizontal g-vector in pattern
	galen: float(0.0), $			; horizontal g-vector length
	symgroups: lonarr(8), $			; symmetry group numbers for zone axis symmetry
	startthick: float(0.0), $		; starting integration depth
	thickinc: float(0.0), $			; integration depth step size
	thicksel: long(0), $			; selected integration depth
	imx: long(0), $				; number of image pixels along x
	imy: long(0), $				; number of image pixels along y
	Kossellegend: long(0), $		; display pattern scale bar toggle (0=do not display, 1=display)
	Kosselformat: long(0), $		; pattern output format selector (0=jpeg, 1=tiff, 2=bmp)
	nums: long(0), $			; number of pixels along disk radius (diameter = 2*nums+1)
	scale: float(0.0), $			; scale factor for CBED, [number of pixels per reciprocal nanometer]
	xlocation: float(0.0), $		; main widget x-location (can be modified and stored in preferences file)
	ylocation: float(0.0), $		; main widget y-location (can be modified and stored in preferences file)
	Kosselxlocation: float(600.0), $	; image widget x-location (can be modified and stored in preferences file)
	Kosselylocation: float(100.0), $	; image widget y-location 
        scrdimx:0L, $                           ; display area x size in pixels 
        scrdimy:0L $                            ; display area y size in pixels 
        }

data.wavek = replicate(0L,3)
data.fn = replicate(0L,3)
data.ga = replicate(0L,3)
data.datadims = replicate(0L,3)
data.symgroups = replicate(0L,8)
data.thicksel = 0L

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
Kosselgetpreferences

;------------------------------------------------------------
; create the top level widget
widget_s.base = WIDGET_BASE(TITLE='Electron Kossel Pattern Display Program', $
                        /COLUMN, $
                        XSIZE=700, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='KosselDisplay_event', $
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

block1 = WIDGET_BASE(widget_s.base, $
			/FRAME, $
			/COLUMN)

;----------
file1 = WIDGET_BASE(block1, $
			/ROW, $
                        XSIZE=700, $
			/ALIGN_CENTER)

label2 = WIDGET_LABEL(file1, $
			VALUE='Data File Name', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.filename = WIDGET_TEXT(file1, $
			VALUE=data.dataname,$
			XSIZE=77, $
			/ALIGN_LEFT)

;----------
file3 = WIDGET_BASE(block1, $
			/ROW, $
			/BASE_ALIGN_BOTTOM, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file3, $
			VALUE='Data File Size', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.filesize = WIDGET_TEXT(file3, $
			VALUE=string(data.filesize,FORMAT="(I)")+' bytes', $
			XSIZE=40, $
			/ALIGN_RIGHT)

file3 = WIDGET_BASE(block1, $
			/ROW, $
			/ALIGN_LEFT)

label4 = WIDGET_LABEL(file3, $
			VALUE='Pattern Dimensions', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.imx= WIDGET_TEXT(file3, $
			VALUE=string(data.imx,format="(I5)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

labela = WIDGET_LABEL(file3, $
			VALUE='by', $
			FONT=fontstrlarge, $
			XSIZE=25, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.imy= WIDGET_TEXT(file3, $
			VALUE=string(data.imy,format="(I5)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

;----------- next we have a series of parameters that are 
; derived from the input file and can not be changed by
; the user...

block2 = WIDGET_BASE(widget_s.base, $
			/FRAME, $
			/ROW)

file4 = WIDGET_BASE(block2, $
			/COLUMN, $
			/ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, $
			/ROW, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file5, $
			VALUE='# of thicknesses', $
			FONT=fontstrlarge, $
			XSIZE=230, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.numthick= WIDGET_TEXT(file5, $
			VALUE=string(data.datadims[2],format="(I5)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, $
			/ROW, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file5, $
			VALUE='Beam Convergence [mrad]', $
			FONT=fontstrlarge, $
			XSIZE=230, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.thetac= WIDGET_TEXT(file5, $
			VALUE=string(data.thetac,format="(F6.3)"),$
			XSIZE=10, $
			/ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, $
			/ROW, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file5, $
			VALUE='Voltage [V]', $
			FONT=fontstrlarge, $
			XSIZE=230, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.voltage = WIDGET_TEXT(file5, $
			VALUE=string(data.voltage,format="(F7.1)"),$
			XSIZE=10, $
			/ALIGN_LEFT)



;-------------
;-------------
file6 = WIDGET_BASE(block2, $
			/COLUMN, $
			/ALIGN_LEFT)

;-------------
file7 = WIDGET_BASE(file6, $
			/ROW, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file7, $
			VALUE='# of k-vectors', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.numk= WIDGET_TEXT(file7, $
			VALUE=string(data.numk,format="(I5)"),$
			XSIZE=20, $
			/ALIGN_LEFT)

;-------------
file7 = WIDGET_BASE(file6, $
			/ROW, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file7, $
			VALUE='Structure File', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

widget_s.xtalname= WIDGET_TEXT(file7, $
			VALUE=data.xtalname,$
			XSIZE=20, $
			/ALIGN_LEFT)

;-------------
file7 = WIDGET_BASE(file6, $
			/ROW, $
			/ALIGN_LEFT)

label2 = WIDGET_LABEL(file7, $
			VALUE='Zone Axis [uvw]', $
			FONT=fontstrlarge, $
			XSIZE=200, $
			YSIZE=25, $
			/ALIGN_LEFT)

wv = '['+string(data.wavek[0],format="(I2)")+' '+ string(data.wavek[1],format="(I2)")+' '+ string(data.wavek[2],format="(I2)")+']'
widget_s.wavek= WIDGET_TEXT(file7, $
			VALUE=wv,$
			XSIZE=20, $
			/ALIGN_LEFT)

;------------------------------------------------------------
block3 = WIDGET_BASE(widget_s.base, /FRAME, /ROW)

;-------------
file5 = WIDGET_BASE(block3, /ROW, /ALIGN_LEFT)
widget_s.symCPG = Core_WText(file5, 'Crystal PG',fontstrlarge, 230, 25, 10, 1, PGTHD[data.symgroups[0]] )
widget_s.symWPG = Core_WText(file5, 'Whole Pattern PG',fontstrlarge, 230, 25, 10, 1, PGTWD[data.symgroups[5]] )


;------------------------------------------------------------
; block 3 QUIT button, LOAD FILE button
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
                                EVENT_PRO='KosselDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

widget_s.loadfile = WIDGET_BUTTON(file11, $
                                VALUE='Kossel File', $
                                UVALUE='LOADFILE', $
                                EVENT_PRO='KosselDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.logodraw, GET_VALUE=drawID
widget_s.logodrawID = drawID
;
read_jpeg,'Resources/CTEMlogo.jpg',logo
wset,widget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"KosselDisplay",widget_s.base,/NO_BLOCK

end

