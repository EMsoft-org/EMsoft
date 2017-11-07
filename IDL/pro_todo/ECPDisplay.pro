@ECPDisplay_event    		; ECP event handler
@ECPgetfilename			; select a geometry file
@ECPreaddatafile		; read geometry and data files
@ECPatternWidget		; display widget
@ECPatternWidget_event		; even handler
@ECPshow			; show an ECPattern (with grid)
@ECPevent			; event handler for button groups
@ECPgetpreferences		; read preferences file
@ECPwritepreferences		; write preferences file
;
; Copyright (c) 2013-2015, Marc De Graef/Carnegie Mellon University
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
; EMsoft2013:ECPDisplay.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECPDisplay.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Zone axis electron channeling pattern display
;
;> @date 11/23/13 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro ECPDisplay,dummy
;
;------------------------------------------------------------
; common blocks
common ECP_widget_common, widget_s
common ECP_data_common, data
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
if (XRegistered("ECPDisplay") NE 0) then begin
  print,'ECPDisplay is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
widget_s = {widgetstruct, $
            base:long(0), $                     ; base widget ID
            ECPatternbase:long(0), $            ; display window widget ID
            ECPthicklist:long(0), $             ; integration depth widget
            ECPgridbgroup:long(0), $            ; grid button group
            ECPformatbgroup:long(0), $          ; file format button group
            ECPDraw:long(0), $                  ; pattern draw widget
            ECPDrawID:long(0), $                ; pattern draw widget ID
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
            patrot:float(0.0), $                ; pattern rotation angle widget
            cx:long(0), $                       ; x-coordinate field
            cy:long(0), $                       ; y-coordinate field
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
	ECProot: 'undefined', $			; current pathname (is stored in preferences file)
	prefname: '~/.ECPgui.prefs', $		; filename of preferences file (including path)
	nprefs: fix(0), $			; number of preferences in file
	progname: '',$				; program name
	scversion: '',$				; program version
	datadims: lon64arr(3), $		; dimensions of rawdata array
	xtalname: '', $				; crystal structure filename
	distort: long(0), $			; is the cell distorted ?
	abcdist:fltarr(3), $			; lattice parameters
	albegadist:fltarr(3), $			; lattice angles
	voltage: float(0.0), $			; microscope voltage
	wavelength: float(0.0), $		; electron wavelength
	ktmax: float(0.0), $			; beam divergence angle [units of ga]
	delta: float(0.0), $			; scale factor
	gperp: fltarr(3), $			; vector normal to ga
	blur: float(0.0), $			; blurring factor
	patrot: float(0.0), $			; pattern rotation angle 
	thetac: float(0.0), $			; beam divergence angle [mrad]
	wavek: lonarr(3), $			; wave vector indices
	fn: lonarr(3), $			; foil normal
	numk: long(0), $			; number of wave vectors in ECP pattern
	dmin: long(0), $			; smallest d-spacing 
	xmid: long(0), $			; half the pattern size
	dgrid: float(0.0), $			; coordinate grid-spacing 
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
	ecplegend: long(0), $			; display pattern scale bar toggle (0=do not display, 1=display)
	ecpformat: long(0), $			; pattern output format selector (0=jpeg, 1=tiff, 2=bmp)
	ecpgrid: long(0), $			; grid display selector (0 = off, 1 = on)
	nums: long(0), $			; number of pixels along disk radius (diameter = 2*nums+1)
	scale: float(0.0), $			; scale factor for CBED, [number of pixels per reciprocal nanometer]
	xlocation: float(0.0), $		; main widget x-location (can be modified and stored in preferences file)
	ylocation: float(0.0), $		; main widget y-location (can be modified and stored in preferences file)
	ECPxlocation: float(600.0), $		; image widget x-location (can be modified and stored in preferences file)
	ECPylocation: float(100.0), $		; image widget y-location 
        scrdimx:0L, $                           ; display area x size in pixels 
        scrdimy:0L $                            ; display area y size in pixels 
        }

data.abcdist = replicate(0.0,3)
data.albegadist = replicate(0.0,3)
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
ECPgetpreferences

;------------------------------------------------------------
; create the top level widget
widget_s.base = WIDGET_BASE(TITLE='Electron Channeling Pattern Display Program', $
                        /COLUMN, $
                        XSIZE=700, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='ECPDisplay_event', $
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

block1 = WIDGET_BASE(widget_s.base, /FRAME, /COLUMN)
file1 = WIDGET_BASE(block1, /ROW, XSIZE=700, /ALIGN_CENTER)
widget_s.filename = Core_WText(file1, 'Data File Name ',fontstrlarge, 200, 25, 77, 1, data.dataname)

file1 = WIDGET_BASE(block1, /ROW, XSIZE=700, /ALIGN_CENTER)
widget_s.filesize = Core_WText(file1, 'Data File Size ',fontstrlarge, 200, 25, 40, 1, string(data.filesize,FORMAT="(I)")+' bytes')

file2 = WIDGET_BASE(block1, /ROW, /ALIGN_LEFT)
widget_s.imx = Core_WText(file2, 'Pattern Dimensions',fontstrlarge, 200, 25, 10, 1, string(data.imx,FORMAT="(I5)"))
widget_s.imy = Core_WText(file2, ' by ',fontstrlarge, 35, 25, 10, 1, string(data.imy,FORMAT="(I5)"))

;----------- next we have a series of parameters that are 
; derived from the input file and can not be changed by
; the user...

block2 = WIDGET_BASE(widget_s.base, /FRAME, /ROW)
file4 = WIDGET_BASE(block2, /COLUMN, /ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.numthick = Core_WText(file5, '# of thicknesses',fontstrlarge, 230, 25, 10, 1, string(data.datadims[2],FORMAT="(I5)"))

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.thetac = Core_WText(file5, 'Beam Convergence [mrad]',fontstrlarge, 230, 25, 10, 1, string(data.thetac,FORMAT="(F6.3)"))

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.voltage = Core_WText(file5, 'Voltage [V]',fontstrlarge, 230, 25, 10, 1, string(data.voltage,FORMAT="(F7.1)"))


;-------------
;-------------
file6 = WIDGET_BASE(block2, /COLUMN, /ALIGN_LEFT)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.numk= Core_WText(file7, '# of k-vectors/Mode',fontstrlarge, 200, 25, 20, 1, string(data.numk,FORMAT="(I5)"))

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.xtalname = Core_WText(file7, 'Structure File',fontstrlarge, 200, 25, 20, 1, data.xtalname)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
wv = '['+string(data.wavek[0],format="(I2)")+' '+ string(data.wavek[1],format="(I2)")+' '+ string(data.wavek[2],format="(I2)")+']'
widget_s.wavek= Core_WText(file7, 'Zone Axis [uvw]',fontstrlarge, 200, 25, 20, 1, wv)


;----------- next we have the lattice parameters

block2 = WIDGET_BASE(widget_s.base, /FRAME, /COLUMN)
file4 = WIDGET_BASE(block2, /ROW, /ALIGN_LEFT)

pp = string(data.abcdist[0],format="(F10.5)")+', '+ string(data.abcdist[1],format="(F10.5)")+', '+ string(data.abcdist[2],format="(F10.5)")+' [nm]'
widget_s.abcdist = Core_WText(file4, 'Lattice Parameters',fontstrlarge, 200, 25, 45, 1, pp)

file4 = WIDGET_BASE(block2, /ROW, /ALIGN_LEFT)
pp = string(data.albegadist[0],format="(F10.5)")+', '+ string(data.albegadist[1],format="(F10.5)")+', '+ string(data.albegadist[2],format="(F10.5)")+' [degrees]'
widget_s.albegadist = Core_WText(file4, '                  ',fontstrlarge, 200, 25, 45, 1, pp)

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
                                EVENT_PRO='ECPDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

widget_s.loadfile = WIDGET_BUTTON(file11, $
                                VALUE='ECP File', $
                                UVALUE='LOADFILE', $
                                EVENT_PRO='ECPDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.logodraw, GET_VALUE=drawID
widget_s.logodrawID = drawID
;
read_jpeg,'Resources/SEMlogo.jpg',logo
wset,widget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"ECPDisplay",widget_s.base,/NO_BLOCK

end

