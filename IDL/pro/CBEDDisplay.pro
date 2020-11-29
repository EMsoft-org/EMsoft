@Core_WText			; core text widget creation 
@Core_WTextE			; core editable text widget creation 
@Core_getenv			; core editable text widget creation 
@Core_Print			; core editable text widget creation 
@CBEDprint			; appends messages to the status text widget
@CBEDgetpreferences		; read the preferences file
@CBEDwritepreferences		; write the preferences file
@CBEDGenerate2DSymmetry		; generate a set of 2D symmetry opertors for a given point group
@CBEDApply2DOperator		; apply a single symmetry operator to a disk image
@CBEDApply2DSymmetry		; apply all the symmetry operators to a disk image and add the results for Eades patterns
@CBEDCompute2DEquivalents 	; apply all the symmetry operators to a disk image and generate all equivalent disks
@CBEDDisplay_event		; main event handler 
@CBEDgetfilename		; select a data file
@CBEDreaddatafile		; read a data file
@CBEDprogressbar		; draws a progress bar during file loading
@CBEDprosetvalue		; used for event handling
@CBEDLACBEDWidget		; LACBED display widget
@CBEDLACBEDWidget_event		; LACBED display widget event handler
;@CBEDLACBEDDrawWidget		; drawing widget for BF and DF/Eades patterns
;@CBEDDrawWidget_event		; LACBED draw widget event handler
@CBEDCBEDWidget			; CBED display widget
@CBEDMBCBEDWidget		; MBCBED display widget
@CBEDMBCBEDDrawWidget		; MBCBED display widget
@CBEDMBCBEDWidget_event		; MBCBED display widget event handler
@CBEDDrawWidget	; Draw widget 
@CBEDMBCBEDDrawWidget_event	; MBCBED Draw widget event handler
@CBEDCBEDDrawWidget		; CBED Draw widget
@CBEDCBEDDrawWidget_event	; CBED Draw widget event handler
@CBEDCBEDWidget_event		; CBED display widget event handler
@CBEDApply2DSymmetryPoint	; apply 2D point symmetry to a point
@CBEDApply2DSymmetryStack	; apply 2D point symmetry to a stack of diffraction disks
@CBEDevent			; special routine to deal with CW_BGROUP events
@CBEDgocbed			; compute a synthetic CBED pattern from the LACBED data
@CBEDcircles			; draw a schematic diffraction pattern along with Laue limiting circle
@CBEDupdateLaue			; draw the Laue position on top of the CBED schematic
@CBEDmoveLaue			; move CBED pattern from one Laue position to another one
@write_mrc              ; creates an .mrc file with a CBED pattern 
@write_hdf5_CBED        ; create an HDF5 file with a CBED pattern 

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
; CTEMsoft2013:CBEDDisplay.pro
;--------------------------------------------------------------------------
;
; PROGRAM: CBEDDisplay.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Zone axis CBED display, used for both MBCBED and LACBED programs
;
;> @date 09/25/13 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro CBEDDisplay,dummy
;
;------------------------------------------------------------
; common blocks
common CBED_widget_common, widget_s
common CBED_data_common, data
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common PointGroups, PGTHD, PGTWD, DG
common trafos, done, c030, c060, c120, c150, c240, c300, s030, s060, s120, s150, s240, s300
common CommonCore, status, logmode, logunit

done = 0
!EXCEPT=0

;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("CBEDDisplay") NE 0) then begin
  print,'CBEDDisplay is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
widget_s = {widgetstruct, $
            base:long(0), $                     ; base widget ID
            status:long(0), $                   ; status text widget ID
            progress:long(0), $                 ; progress status bar widget ID
            progressdrawID:long(0), $           ; progress status bar widget draw ID
            imagebase:long(0), $                ; base widget ID for BF/HAADF image display widget
         ;  cbedbase:long(0), $                 ; base widget ID for CBED pattern display widget
            dataname:long(0), $                 ; filename widget ID
            xtalname:long(0), $                 ; crystal structure filename widget ID
            filesize:long(0), $                 ; filesize widget ID
            pathname:long(0), $                 ; pathname widget ID
            loadlacbedfile:long(0), $           ; load LACBED file button ID
            loadmbcbedfile:long(0), $           ; load MBCBED file button ID
    	    logfile: long(0), $			; logfile toggle widget ID
    	    logoffset: long(0), $		; logarithm offset parameter widget
            imx:long(0), $                 	; number of image pixels along x
            imy:long(0), $                 	; number of image pixels along y
            patx:long(0), $                 	; number of CBED pattern pixels along x
            paty:long(0), $                 	; number of CBED pattern pixels along y
            CBEDzoom:long(0), $                 ; CBED pattern zoom factor
            maxHOLZ:long(0), $                  ; maximum HOLZ layer number
    	    ga:long(0), $			; indices of horizontal g-vector
    	    startLACBED: long(0), $		; button to generate LACBED widget
    	    startCBED: long(0), $		; button to generate CBED widget
    	    startMBCBED: long(0), $		; button to generate MBCBED widget
    	    LACBEDbase: long(0), $		; LACBED widget base
    	    LACBEDDrawbase: long(0), $		; LACBED draw widget base
    	    CBEDbase: long(0), $		; CBED widget base
    	    CBEDDrawbase: long(0), $		; CBED Draw widget base
    	    MBCBEDDrawbase: long(0), $		; MBCBED Draw widget base
    	    CBDraw: long(0), $			; Draw widget base for diffraction disk outline
    	    MBDraw: long(0), $			; Draw widget base for MBCBED
    	    MBCBEDbase: long(0), $		; MBCBED widget base
    	    LACBEDthicklist: long(0), $		; LACBED thickness drop list
    	    MBCBEDthicklist: long(0), $		; MBCBED thickness drop list
    	    dfdisplaymode: long(0), $		; dark field display mode (0=single; 1=symmetrize; 2=Eades)
    	    LACBEDdroplist0: long(0), $		; droplist for ZOLZ
    	    LACBEDdroplist1: long(0), $		; droplist for HOLZ layer 1
    	    LACBEDdroplist2: long(0), $		; droplist for HOLZ layer 2
    	    LACBEDdroplist3: long(0), $		; droplist for HOLZ layer 3  [for now, a maximum of 3 HOLZ layers are included]
    	    gsel: long(0), $			; selected g-vector widget
    	    fn: long(0), $			; foil normal widget 
    	    minten: long(0), $			; minimum intensity cutoff
    	    diskrotation: long(0), $		; disk rotation angle [CW, degrees]
    	    imagelegendbgroup:long(0), $	; image legend button group
    	    imageformatbgroup:long(0), $	; image format button group
    	    cbedlegendbgroup:long(0), $		; cbed legend button group
    	    cbedformatbgroup:long(0), $		; cbed format button group
    	    cbedmodebgroup:long(0), $		; cbed intensity mode button group
    	    mbcbedformatbgroup:long(0), $	; mbcbed format button group
    	    mbcbedmodebgroup:long(0), $		; mbcbed intensity mode button group
    	    saveimage:long(0), $		; save image button
    	    savecbed:long(0), $			; save cbed pattern button
    	    bfrho: long(0), $			; Bright Field radius in mrad
    	    eadesrhoin: long(0), $		; Eades pattern inner radius in mrad
    	    eadesrhoout: long(0), $		; Eades pattern outer radius in mrad
    	    detsegm: long(0), $			; number of HAADF detector segments
    	    angsegm: long(0), $			; off set angle for first HAADF detector segment
    	    patang: long(0), $			; maximum CBED pattern angle (from input file)
            detdraw:long(0), $                  ; detector draw widget ID
            detdrawID:long(0), $                ; detector draw window ID
    	    logodraw: long(0), $		; logo widget base 
    	    logodrawID: long(0), $		; logo widget base ID
    	    BFdraw: long(0), $			; BF widget
    	    BFdrawID: long(0), $		; BF widget ID
    	    DFdraw: long(0), $			; DF widget
    	    DFdrawID: long(0), $		; DF widget ID
    	    HAADFdraw: long(0), $		; HAADF widget
    	    HAADFdrawID: long(0), $		; HAADF widget ID
    	    CBEDdraw: long(0), $		; CBED widget
    	    CBEDdrawID: long(0), $		; CBED widget ID
    	    BFmin: long(0), $			; BF image minimum intensity
    	    BFmax: long(0), $			; BF image maximum intensity
    	    DFmin: long(0), $			; DF image minimum intensity
    	    DFmax: long(0), $			; DF image maximum intensity
    	    HAADFmin: long(0), $		; HAADF image minimum intensity
    	    HAADFmax: long(0), $		; HAADF image maximum intensity
    	    CBEDmin: long(0), $			; CBED pattern minimum intensity
    	    CBEDmax: long(0), $			; CBED atptern maximum intensity
            Lauex:long(0), $                    ; Laue center x-coordinate
            Lauey:long(0), $                    ; Laue center y-coordinate
            camlen:long(0), $                   ; default camera length (mm)
            camlenval:long(0), $                ; user selected camera length
            convangval:long(0), $               ; user selected convergence angle 
            wavelength:long(0), $               ; wave length field (pm)
            wavek:long(0), $                    ; wave vector indices
            numfam:long(0), $                   ; number of g-vectors
            numk:long(0), $                     ; number of k-vectors
            numt:long(0), $                     ; number of thickness values
            thetac:long(0), $                   ; beam divergence angle (mrad)
            aprad:long(0), $               	; aperture radius widget
            dfmode:long(0), $               	; set k/set g selection mode widget
    	    symCPG: long(0), $			; crystal point group
    	    symLPG: long(0), $			; Laue point group
    	    symDPG: long(0), $			; diffraction point group
    	    symPDG: long(0), $			; projection diffraction point group
    	    symBFG: long(0), $			; Bright Field point group
    	    symWPG: long(0), $			; Whole Pattern point group
    	    symDFG: long(0), $			; General Dark Field point group
    	    symDFS: long(0), $			; Special Dark Field point group
            sectormode:long(0), $               ; single or multiple sector mode
            diffractionmode:long(0), $          ; HAADF or regular dark field diffraction mode
            gosector:long(0), $                 ; compute image for multiple sector mode
            clearsector:long(0), $              ; clear selected sectors and image
            inputbase:long(0), $                ; input base widget ID
            closecbed:long(0), $                ; close CBED widget button
            closeimage:long(0), $               ; close image widget button
            mainstop:long(0), $                 ; stop button
            jumplist:long(0), $                 ; droplist with tracking values
            movemodegroup:long(0), $            ; button group widget for Laue center tracking mode
            topbgroup:long(0) $                 ; top level button group widget
           }

data = {datastruct, $
	scversion: '', $			    ; version identifier
	eventverbose: fix(0), $			; used for event debugging (0=off, 1=on)
	dataname: '', $				    ; filename (without pathname)
	pathname: '', $				    ; pathname (obviously)
	xtalname: '', $				    ; crystal structure filename (without pathname)
	suffix: '', $				    ; filename suffix 
	prefname: '~/.CBEDgui.prefs', $	; filename of preferences file (including path)
	filesize: long64(0), $			; input file size in bytes
	homefolder: '', $			    ; startup folder of the program
	CBEDroot: 'undefined', $		; current pathname (is stored in preferences file)
	MBCBEDroot: 'undefined', $		; current MBCBED pathname (is stored in preferences file)
	nprefs: fix(0), $			    ; number of preferences in file
    status:'waiting for input', $   ; current status line
	logmode: fix(0), $			    ; keep a log file or not
	logunit: fix(13), $			    ; logical file unit for log file
	logname: '', $				    ; filename for log output
	logfileopen: fix(0), $			; log file is open when 1, closed when 0
	logoffset: float(0.001), $		; logarithmic display offset parameter (to avoid taking log(0)... )
	detwinx: fix(401), $			; detector display window size x
	detwiny: fix(401), $			; detector display window size y
	imx: long(0), $				    ; number of image pixels along x
	imy: long(0), $				    ; number of image pixels along y
	patx: long(900), $			    ; number of pattern pixels along x
	paty: long(900), $			    ; number of pattern pixels along y
	CBEDzoom: fix(1), $			    ; zoom factor for CBED pattern
	ga: lonarr(3), $			    ; indices of horizontal g-vector
	galen: float(0), $			    ; length of horizontal g-vector
	addlog: float(0.0001), $		; factor to add for logarithmic CBED display
	imagelegend: long(0), $			; display image scale bar toggle (0=do not display, 1=display)
	imageformat: long(0), $			; image output format selector (0=jpeg, 1=tiff, 2=bmp, 3=mrc, 4=hdf5)
	cbedlegend: long(0), $			; display cbed scale bar toggle (0=do not display, 1=display)
	cbedformat: long(0), $			; cbed output format selector (0=jpeg, 1=tiff, 2=bmp)
	cbedmode: long(0), $			; cbed intensity mode toggle (0=normal, 1=logarithmic)
	dfdisplaymode: long(0), $		; dark field display mode (0=single; 1=symmetrized; 2=Eades)
	diffractionmode: long(0), $		; diffraction mode (MBCBED=0, LACBED=1)
	lastmode: long(0), $			; last drawing mode (0=grayscale or 1=RGB)
	diskrotation: float(0), $		; disk rotation angle [CW, degrees]
	maxHOLZ: long(2), $		    	; maximum HOLZ lyer number in data file
	famsel: fix(0), $			    ; selected family ID number
	BFrho: float(3.5), $			; Bright Field radius in mrad
	Eadesrhoin: float(10.0), $		; Eades detector inner radius in mrad
	Eadesrhoout: float(50.0), $		; Eades detector outer radius in mrad
	BFmin: float(0.0), $			; BF image minimum intensity
	BFmax: float(0.0), $			; BF image maximum intensity
	DFmin: float(0.0), $			; DF image minimum intensity
	DFmax: float(0.0), $			; DF image maximum intensity
	Eadesmin: float(0.0), $			; Eades image minimum intensity
	Eadesmax: float(0.0), $			; Eades image maximum intensity
	CBEDmin: float(0.0), $			; CBED pattern minimum intensity
	CBEDmax: float(0.0), $			; CBED pattern maximum intensity
	BFdrawID: long(0), $			; BF window ID 
	DFdrawID: long(0), $			; DF window ID 
	CBdrawID: long(0), $			; CB window ID 
	MBdrawID: long(0), $			; CB window ID 
	diskdrawID: long(0), $			; disk window ID 
	patang: float(15.0), $			; this is the horizontal half scale of the detector plot in mm
    camlen: float(500), $           ; camera length field (mm)
    refcamlen: float(1000), $       ; reference camera length to which the others will be scaled (mm)
	dmin: float(0.0), $		       	; minimal d-spacing
	aprad: float(0.5), $			; aperture radius for CBED pattern computation in LACBED mode [mrad]
	apx: float(0.0), $		       	; aperture x position
	apy: float(0.0), $			    ; aperture y position
	apminrad: float(0.1), $			; minimal aperture radius for CBED imaging [mrad]
	rdisk: float(0.0), $			; disk radius in units of pixels
	wavek: lonarr(3), $			    ; wave vector indices
	minten: float(0), $			    ; maximum intensity for thinnest section
	fn: lonarr(3), $			    ; foil normal indices
	voltage: float(0.0), $			; acceleration voltage [V]
	wavelength: float(0.0), $		; wave length [nm] (will be displayed in [pm])
	dfl: float(1.0), $			    ; pixel size [nm]
	thetac: float(0.0), $			; beam convergence angle [mrad]
	thetau: float(0.0), $			; user selected beam convergence angle [mrad]
	thetam: float(0.0), $			; point group rotation angle [degrees]
    Lauex: float(0), $              ; Laue center x-coordinate
    Lauey: float(0), $              ; Laue center y-coordinate
    oldLauex: float(0), $           ; old Laue center x-coordinate
    oldLauey: float(0), $           ; old Laue center y-coordinate
	nums: long(0), $			    ; number of pixels along disk radius (diameter = 2*nums+1)
	scale: float(0.0), $			; scale factor for CBED, [number of pixels per reciprocal nanometer]
	numfam: long(0), $ 			    ; number of reflections in CBED pattern
	numk: long(0), $			    ; number of wave vectors in CBED pattern
	numt: long(0), $			    ; number of sample thicknesses
	startthick: float(0), $			; starting thickness
	thickinc: float(0), $			; thickness increment
	thicksel: long(0), $			; selected thickness index
	thickness: float(0), $			; selected thickness
	jumpsel: long(0), $			    ; selected jump mode index
	movemode: long(0), $			; move mode index
	symgroups: lonarr(8), $			; symmetry group labels
	datadims: lon64arr(4), $		; dimensions of rawdata array
	xlocation: float(0.0), $		; main widget x-location (can be modified and stored in preferences file)
	ylocation: float(0.0), $		; main widget y-location (can be modified and stored in preferences file)
	LACBEDPatternxlocation: float(600), $	; initital x-location of LACBEDdraw widget
	LACBEDPatternylocation: float(200), $	; initital y-location of LACBEDdraw widget
	LACBEDxlocation: float(600), $		; initital x-location of LACBED widget
	LACBEDylocation: float(200), $		; initital y-location of LACBED widget
	CBEDxlocation: float(600), $		; initital x-location of CBED widget
	CBEDylocation: float(200), $		; initital y-location of CBED widget
	MBCBEDxlocation: float(600), $		; initital x-location of MBCBED widget
	MBCBEDylocation: float(200), $		; initital y-location of MBCBED widget
	CBEDDrawxlocation: float(600), $	; initital x-location of CBED Draw widget
	CBEDDrawylocation: float(200), $	; initital y-location of CBED Draw widget
	MBCBEDDrawxlocation: float(600), $	; initital x-location of MBCBED Draw widget
	MBCBEDDrawylocation: float(200), $	; initital y-location of MBCBED Draw widget
	imagexlocation: float(600.0), $		; image widget x-location (can be modified and stored in preferences file)
	imageylocation: float(100.0), $		; image widget y-location 
    scrdimx:0L, $                       ; display area x size in pixels 
    scrdimy:0L $                        ; display area y size in pixels 
        }

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
CBEDgetpreferences

;------------------------------------------------------------
; create the top level widget
widget_s.base = WIDGET_BASE(TITLE='Zone Axis CBED Display Program', $
                        /ROW, $
                        XSIZE=1300, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='CBEDDisplay_event', $
                        XOFFSET=data.xlocation, $
                        YOFFSET=data.ylocation)

block0 = WIDGET_BASE(widget_s.base, /COLUMN)

block1 = WIDGET_BASE(block0, /FRAME, /COLUMN)

file1 = WIDGET_BASE(block1, /ROW, XSIZE=650, /ALIGN_LEFT)

widget_s.logodraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/ALIGN_CENTER, $
			XSIZE=600, $
			YSIZE=200)

;------------------------------------------------------------
; create the various vertical blocks
; block 1 deals with the input file and displays the data dimensions
block1 = WIDGET_BASE(block0, /FRAME, /COLUMN)

;----------
file1 = WIDGET_BASE(block1, /ROW, XSIZE=650, /ALIGN_CENTER)
widget_s.dataname = Core_WText(file1, 'Data File Name',fontstrlarge, 150, 25, 77, 1, data.dataname)

;----------
file3 = WIDGET_BASE(block1,  /ROW,  /BASE_ALIGN_BOTTOM,  /ALIGN_LEFT)
widget_s.filesize = Core_WText(file3, 'Data File Size',fontstrlarge, 150, 25, 30, 1, string(float(data.filesize)/1024./1024.,FORMAT="(F8.2)")+' Mb' )

widget_s.progress = WIDGET_DRAW(file3, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/ALIGN_RIGHT, $
			XSIZE=200, $
			YSIZE=20)


;----------
file3 = WIDGET_BASE(block1,  /ROW,  /ALIGN_LEFT)
widget_s.imx = Core_WText(file3, 'Disk Dimensions',fontstrlarge, 150, 25, 10, 1, string(data.imx,FORMAT="(I5)"))
widget_s.imy = Core_WText(file3, 'by',fontstrlarge, 25, 25, 10, 1, string(data.imy,FORMAT="(I5)"))


;----------- next we have a series of parameters that are 
; derived from the input file and can not be changed by
; the user...

block2 = WIDGET_BASE(block0, /FRAME, /ROW)
file4 = WIDGET_BASE(block2, /COLUMN, /ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.numfam = Core_WText(file5, '# of g-families',fontstrlarge, 230, 25, 10, 1, string(data.numfam,FORMAT="(I5)"))

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.thetac = Core_WText(file5, 'Beam Convergence [mrad]',fontstrlarge, 230, 25, 10, 1, string(data.thetac,FORMAT="(F6.3)"))

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.wavelength = Core_WText(file5, 'Wave Length [pm]',fontstrlarge, 230, 25, 10, 1, string(data.wavelength,FORMAT="(F7.4)"))

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.maxHOLZ = Core_WText(file5, 'Maximum HOLZ    ',fontstrlarge, 230, 25, 10, 1, string(data.maxHOLZ,FORMAT="(I4)"))

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
wv = '['+string(data.fn[0],format="(I2)")+' '+ string(data.fn[1],format="(I2)")+' '+ string(data.fn[2],format="(I2)")+']'
widget_s.fn= Core_WText(file5, 'Foil Normal',fontstrlarge, 230, 25, 10, 1, string(data.maxHOLZ,FORMAT="(I4)"))

;-------------
;-------------
file6 = WIDGET_BASE(block2, /COLUMN, /ALIGN_LEFT)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.numk = Core_WText(file7, '# of k-vectors  ',fontstrlarge, 150, 25, 20, 1, string(data.numk,FORMAT="(I5)"))

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.xtalname = Core_WText(file7, 'Structure File  ',fontstrlarge, 150, 25, 20, 1, data.xtalname)

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
wv = '['+string(data.wavek[0],format="(I3)")+' '+ string(data.wavek[1],format="(I3)")+' '+ string(data.wavek[2],format="(I3)")+']'
widget_s.wavek = Core_WText(file7, 'Zone axis [uvw] ',fontstrlarge, 150, 25, 20, 1, wv )

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
wv = '('+string(data.ga[0],format="(I3)")+' '+ string(data.ga[1],format="(I3)")+' '+ string(data.ga[2],format="(I3)")+')'
widget_s.ga = Core_WText(file7, 'Horizontal g    ',fontstrlarge, 150, 25, 20, 1, wv )

;-------------
file7 = WIDGET_BASE(file6, /ROW, /ALIGN_LEFT)
widget_s.minten = Core_WText(file7, 'Intensity cutoff',fontstrlarge, 150, 25, 20, 1, string(data.minten,format="(E10.2)") )


;------------------------------------------------------------
; block 3 displays a number of symmetry properties (there are 8 in total)

block0 = WIDGET_BASE(widget_s.base, XSIZE = 610, /COLUMN)

block3 = WIDGET_BASE(block0, /FRAME, /ROW)
file4 = WIDGET_BASE(block3, /COLUMN, /ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
data.symgroups = [0,0,0,0,0,0,0,0]
widget_s.symCPG = Core_WText(file5, 'Crystal PG',fontstrlarge, 200, 25, 10, 1, PGTHD[data.symgroups[0]] )

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symDPG = Core_WText(file5, 'Diffraction PG',fontstrlarge, 200, 25, 10, 1, DG[data.symgroups[2]] )

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symWPG = Core_WText(file5, 'Whole Pattern PG',fontstrlarge, 200, 25, 10, 1, PGTWD[data.symgroups[5]] )

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symDFG = Core_WText(file5, 'Dark Field General PG',fontstrlarge, 200, 25, 10, 1, PGTHD[data.symgroups[6]] )

;-------------
;-------------
file4 = WIDGET_BASE(block3, /COLUMN, /ALIGN_LEFT)

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symLPG = Core_WText(file5, 'Laue PG',fontstrlarge, 200, 25, 10, 1, PGTHD[data.symgroups[1]] )

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symPDG = Core_WText(file5, 'Projection Diff. PG',fontstrlarge, 200, 25, 10, 1, DG[data.symgroups[3]] )

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symBFG = Core_WText(file5, 'Bright Field PG',fontstrlarge, 200, 25, 10, 1, PGTWD[data.symgroups[4]] )

;-------------
file5 = WIDGET_BASE(file4, /ROW, /ALIGN_LEFT)
widget_s.symDFS = Core_WText(file5, 'Dark Field Special PG',fontstrlarge, 200, 25, 10, 1, PGTWD[data.symgroups[7]] )

;-------------
; next we have the buttons that generate LACBED, CBED, or MBCBED widgets
;-------------
block3 = WIDGET_BASE(block0, XSIZE=550, /FRAME, /ROW)
file5 = WIDGET_BASE(block3, /ROW, /ALIGN_CENTER)

widget_s.startLACBED = WIDGET_BUTTON(file5, $
                        VALUE='LACBED Window', $
                        UVALUE='STARTLACBEDWIDGET', $
                        EVENT_PRO='CBEDDisplay_event', $
                        SENSITIVE=0, $
                        /FRAME)

widget_s.startCBED = WIDGET_BUTTON(file5, $
                        VALUE='CBED Window', $
                        UVALUE='STARTCBEDWIDGET', $
                        EVENT_PRO='CBEDDisplay_event', $
                        SENSITIVE=0, $
                        /FRAME)

widget_s.startMBCBED = WIDGET_BUTTON(file5, $
                        VALUE='MBCBED Window', $
                        UVALUE='STARTMBCBEDWIDGET', $
                        EVENT_PRO='CBEDDisplay_event', $
                        SENSITIVE=0, $
                        /FRAME)

;----------
; next, add a text window for program messages

widget_s.status= WIDGET_TEXT(block0, $
			XSIZE=115, $
			YSIZE=18, $
			/SCROLL, $
			VALUE=' ',$
			/ALIGN_CENTER)

;------------------------------------------------------------
; block 3 QUIT button, LOAD FILE button and progress bar (used for file loading)
block3 = WIDGET_BASE(block0, XSIZE=550, /FRAME, /ROW)
file11 = WIDGET_BASE(block3, /ROW, /ALIGN_LEFT)

widget_s.mainstop = WIDGET_BUTTON(file11, $
                        VALUE='Quit', $
                        UVALUE='QUIT', $
                        EVENT_PRO='CBEDDisplay_event', $
                        SENSITIVE=1, $
                        /FRAME)

widget_s.loadlacbedfile = WIDGET_BUTTON(file11, $
                        VALUE='Load LACBED File', $
                        UVALUE='LOADLACBEDFILE', $
                        EVENT_PRO='CBEDDisplay_event', $
                        SENSITIVE=1, $
                        /FRAME)

widget_s.loadmbcbedfile = WIDGET_BUTTON(file11, $
                        VALUE='Load MBCBED File', $
                        UVALUE='LOADMBCBEDFILE', $
                        EVENT_PRO='CBEDDisplay_event', $
                        SENSITIVE=0, $
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
                        EVENT_FUNC='CBEDevent', $
			UVALUE='LOGFILE')


;------------------------------------------------------------
;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,widget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, widget_s.progress, GET_VALUE=drawID
widget_s.progressdrawID = drawID
WIDGET_CONTROL, widget_s.logodraw, GET_VALUE=drawID
widget_s.logodrawID = drawID

read_jpeg,'Resources/EMsoftlogo.jpg',logo,true=1
wset,widget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"CBEDDisplay",widget_s.base,/NO_BLOCK

; init the status text window
CBEDprint,'Zone Axis CBED Display Program [M. De Graef, 2013 and counting...]',/blank
CBEDprint,'',/blank
CBEDprint,'Please load a file'


end

