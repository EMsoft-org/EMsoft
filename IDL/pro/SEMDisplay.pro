@Core_LambertS2C				; modified Lambert to Lambert projection
@Core_LambertS2SP				; modified Lambert to stereographic projection
@Core_colorwheel				; color representation of energy distribution
@Core_WText						; generate a text widget with a label
@Core_WTextE					; generate an editable text widget with a label
@Core_Print						; print messages to status window and log file
@Core_WidgetEvent				; general data handler for various widget events
@Core_getenv                    ; read the environment variable(s)
@Core_eu2qu                     ; convert euler angles to quaternion
@Core_Tag_Exists				; routine to check for existence of data sets in a given H5_PARSE structure
@SEMDisplay_event    			; EBSD event handler
@EBSDgetfilename				; select a geometry file
@EBSDreadHDFdatafile			; read geometry and HDF data files
@EBSDMCDisplayWidget			; MC display widget
@EBSDMCDisplayWidget_event		; MC display widget event handler
@EBSDDetectorWidget				; detector widget
@EBSDDetectorWidget_event		; detector widget event handler
@EBSDPatternWidget				; pattern widget
@EBSDPatternWidget_event		; pattern widget event handler
@EBSDreadanglefile				; reads a file of euler angles or other representations
@EBSDevent						; event handler for button groups
@EBSDshowMC						; display a Lambert projection image
@EBSDshowPattern				; display an BSD pattern
@EBSDgetpreferences				; read preferences file
@EBSDwritepreferences			; write preferences file
@EBSDExecute					; perform the actual pattern computation
@EBSD_updatePC					; update pattern center coordinates
@ECPDetectorWidget              ; ECP detector widget
@ECPDetectorWidget_event        ; ECP detector widget event handler
@ECPExecute                     ; executes the ECP calculation
@ECPatternWidget                ; ECP pattern widget
@ECPatternWidget_event          ; ECP pattern widget event handler
@ECPevent                       ; ECP choice event handler
@ECPshowPattern                 ; show ECP pattern
@KosselDetectorWidget           ; Kossel detector widget
@KosselDetectorWidget_event     ; Kossel detector widget event handler
@KosselExecute                  ; executes the Kossel calculation
@KosselPatternWidget            ; Kossel pattern widget
@KosselPatternWidget_event      ; Kossel pattern widget event handler
@Kosselevent                    ; Kossel choice event handler
@KosselshowPattern              ; show Kossel pattern
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
; EMsoft:SEMDisplay.pro
;--------------------------------------------------------------------------
;
; PROGRAM: SEMDisplay.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Electron backscatter diffraction and electron channeling pattern display
;
;> @details 03/19/14 This is the very first implementation of the EBSD visualization GUI.
;> At this stage, we allow the user to display various versions of the Monte Carlo
;> results, as well as the master EBSD pattern in a number of projection modes.
;> The user can then define a series of detector parameters and compute EBSD
;> patterns.  In this version, there are no PSF, scintillator energy dependence,
;> or microscope geometric distortions; only the detector geometry, noise (on/off),
;> binning, and brightness/contrast controls are available.  The other options 
;> will be included in a next version (as will be the dictionary generation option).
;> The current program can also read ECP and Kossel pattern files.
;
;> @todo Long list of things to be added:
;> - incorporate automated calibration of pattern parameters (now done in separate Efit program)
;> - allow user to save setup for different microscopes
;
;> @date 01/27/14 MDG 1.0 first attempt at a user-friendly interface
;> @date 03/17/14 MDG 1.1 main widget rewrite; prepended 'EBSD' to widget_s and data structures for future merge with other IDL routines
;> @date 03/19/14 MDG 1.2 implementation of Monte Carlo and master EBSD widgets
;> @date 05/22/14 MDG 1.3 completion of single pattern display mode
;> @date 05/27/14 MDG 1.4 completion of angle file display mode
;> @date 10/29/15 MDG 2.0 added ECP handling, first only Monte Carlo part
;> @date 10/30/15 MDG 2.1 added ECP calculation for detector model, using call_external to getECPatternsWrapper
;> @date 10/31/15 MDG 3.0 simplification of main interface; file data is now listed in message window
;> @date 11/05/15 MDG 3.1 program name change from EBSDDisplay to SEMDisplay
;> @date 11/09/15 MDG 3.2 added Kossel pattern visualization, similar to ECP but simpler detector model
;> @date 11/09/15 MDG 3.3 major variable rename
;> @date 10/11/16 MDG 4.0 modification to new joint HDF5 format for master pattern input files
;--------------------------------------------------------------------------
pro SEMDisplay,dummy
;
;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common PointGroups, PGTHD, PGTWD, DG
common getenv_common, librarylocation
common counter, patterncount

common EBSDmasks, circularmask

common CommonCore, status, logmode, logunit

common projections, mcxcircle, mcycircle, mpxcircle, mpycircle, mcSPxcircle, mcSPycircle, mpSPxcircle, mpSPycircle 

; before we do anything, we make sure that the location of the app_user_dir is set 
appdir = app_user_dir('EMsoft','EMsoftPackage','VMapps','Virtual Machine Apps',['This folder is used by vitual machine apps within EMsoft'],1)

;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("SEMDisplay") NE 0) then begin
  print,'SEMDisplay is already running ... (if it is not, please restart your IDL session)'
  return
end

; define point group symbol arrays
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

; do not report exceptions, such as division by zero etc...
!EXCEPT=0

; set a few parameters
patterncount = 0
circularmask = fltarr(10,10)

;------------------------------------------------------------
; define a few structures (one for widgets, and one for data)
SEMwidget_s = {widgetstruct, $
	base:long(0), $                     	; base widget ID

	; Monte Carlo widget ids
	mcfilename: long(0), $			; Monte Carlo filename
	mcfilesize: long(0), $			; file size in bytes
	mcenergymin: long(0), $			; minimum energy
	mcenergymax: long(0), $			; maximum energy
	mcenergybinsize: long(0), $		; energy bin size
	mcenergynumbin: long(0), $		; number of energy bins
	voltage: long(0), $				; microscope voltage
	mcdepthmax: long(0), $			; maximum penetration depth
	mcdepthstep: long(0), $			; step size for depth
	mcdepthnumbins: long(0), $		; number of depth bins
	mcimx: long(0), $				; N in 2N+1 (x-pixels)
	mcimy: long(0), $				; N in 2N+1 (y-pixels)
	mctotale: long(0), $			; total number of incident electrons
	mcbse: long(0), $				; total number of bacck-scattered electrons
	mcvangle: long(0), $			; vertical tilt angle
	mchangle: long(0), $			; horizontal tilt angle
	mcmode: long(0), $				; Monte Carlo mode
	Esel: long(0), $				; Monte Carlo mode
	mcloadfile: long(0), $          ; load file button
	mcdisplay: long(0), $           ; MC display button
    mcfilmthickness: long(0), $     ; film thickness for two-layer structures
	   
	; Master Pattern widget ids
	mpfilename: long(0), $			; file name for master pattern
	mpfiletype: long(0), $			; file type for master pattern
	mpfilesize: long(0), $			; file size in bytes
	mpimx: long(0), $				; x-pixels N as in (2N+1)
	mpimy: long(0), $				; y-pixels (should be equal to mpimx)
	mpgridmode: long(0), $			; Lambert grid mode
	asymunit: long(0), $			; widget for asymmetric unit position selection
	xtalname: long(0), $			; file name for crystal structure data
	mploadfile: long(0), $          ; load file button

	; detector parameter widget ids
	detL: long(0), $				; sample scintillator distance [microns]
	detW: long(0), $				; working distance [millimeters]
	detRi: long(0), $				; BSE detector inner radius [millimeters]
	detRo: long(0), $				; BSE detector outer radius [millimeters]
	detsampleytilt: long(0), $		; sample y-tilt angle [degrees]
	dettheta: long(0), $			; detector tilt angle [degrees]
	detthetac: long(0), $			; incident beam cone semi-angle [degrees]
	detomega: long(0), $			; detector omega tilt angle [degrees]
	detdelta: long(0), $			; detector pixel size [microns]
	detnumsx: long(0), $			; number of x-pixels
	detnumsy: long(0), $			; number of y-pixels
	detxpc: long(0), $				; x -pattern center [pixels]
	detypc: long(0), $				; y -pattern center [pixels]
	detxs: long(0), $				; x sampling coordinate [integer]
	detys: long(0), $				; y sampling coordinate [integer]
	detxss: long(0), $				; x sampling step size [micron]
	detyss: long(0), $				; y sampling step size [micron]
	detbinning: long(0), $			; binning
	detbeamcurrent: long(0), $		; beam current [nA]
	detdwelltime: long(0), $		; dwell time [mu s]
	detphi1: long(0), $				; phi1 Euler angle 
	detphi: long(0), $				; phi Euler angle 
	detphi2: long(0), $				; phi2 Euler angle 
	detalphaBD: long(0), $			; transfer barrel distortion parameter
	EulerConvention: long(0), $		; Euler angle convention for phi2 (TSL or HKL)
	BGmode: long(0), $			; background/full display mode
	EBSDminenergylist: long(0), $		; min energy widget
	EBSDmaxenergylist: long(0), $		; max energy widget
	PatternOrigin: long(0), $		; pattern origin widget
	Patternmin: long(0), $			; pattern min widget
	Patternmax: long(0), $			; pattern max widget
	detax1: long(0), $			; axis angle pair component
	detax2: long(0), $			; axis angle pair component
	detax3: long(0), $			; axis angle pair component
	detax4: long(0), $			; axis angle pair component
	xstar: long(0), $			; x^* parameter widget
	ystar: long(0), $			; y^* parameter widget
	zstar: long(0), $			; z^* parameter widget

	; pattern parameters for display
	Pmode: long(0), $			; pattern mode widget
	EBSDpatternfilename: long(0), $		; output file name widget
	ECPpatternfilename: long(0), $		; output file name widget
	ECPgetpatternfilename: long(0), $	; output file name button
	EBSDgetpatternfilename: long(0), $	; output file name button
	ECPanglefilename: long(0), $		; angle file name widget
	Kosselanglefilename: long(0), $		; angle file name widget
	ECPgetanglefilename: long(0), $	        ; angle file name widget
	Kosselgetanglefilename: long(0), $      ; angle file name widget
	EBSDanglefilename: long(0), $		; angle file name widget
	EBSDgetanglefilename: long(0), $	; angle file name widget
	EBSDdictfilename: long(0), $		; dictionary file name widget
	EBSDgetdictfilename: long(0), $		; dictionary file name widget
        NHSH: long(0), $                        ; Northern or Southern hemisphere for display?
	angletype: long(0), $			; angle type widget (Euler of Quaternion)
	numangles: long(0), $			; number of angles widget
	PGdroplist: long(0), $			; point group selector droplist
	Ncubochoric: long(0), $			; cubochoric # sampling points widget
	NinRFZ: long(0), $			; number of point in Rodrigues FZ widget
	GoEBSD: long(0), $			; EBSD compute button
	GoDict: long(0), $			; Dictionary compute button
	GoAngle: long(0), $			; Angle file compute button
	GoDictionary: long(0), $		; Dictionary file compute button
	DisplayEBSD: long(0), $			; display EBSD button
	DisplayECP: long(0), $			; display ECP button
	DisplayKossel: long(0), $		; display Kossel button
	PatternScaling: long(0), $		; pattern scaling type widget
	gammaslider: long(0), $			; gamma correction slider
	DetectorClose: long(0), $		; Close detector widget
	PatternClose: long(0), $		; Close pattern widget
	PatternDraw: long(0), $			; Pattern draw widget
	PatternDrawID: long(0), $		; Pattern draw widget ID
	angledisplay: long(0), $		; text widget to display angles
	circularmask: long(0), $		; circular mask display

	; other collected items
	MCdisplaybase: long(0), $		; Monte Carlo display base
	MPdisplaybase: long(0), $		; Master Pattern & Monte Carlo display base
	detectorbase: long(0), $		; detector base widget
	patternbase: long(0), $			; pattern base widget
	status:long(0), $               ; status window
	logfile: long(0), $				; logfile toggle widget ID
	detector:long(0), $             ; detector widget
	MCbutton:long(0), $             ; MC button ID
	MCslider:long(0), $             ; MC slider ID
	MCenergyval: long(0), $			; MC energy value
    MCmin: long(0), $               ; MC display minimum
    MCmax: long(0), $               ; MC display maximum
    MPbutton:long(0), $             ; MP button ID
    MPmin: long(0), $               ; MP display minimum
    MPmax: long(0), $               ; MP display maximum
	MCDraw:long(0), $               ; pattern draw widget
	MCDrawID:long(0), $             ; pattern draw widget
	MPDraw:long(0), $               ; pattern draw widget
	MPDrawID:long(0), $             ; pattern draw widget
	mainstop:long(0), $             ; program quit button
	logodraw:long(0), $             ; logo widget 
	logodrawID:long(0),$            ; logo widget ID
	EBSDformatbgroup: long(0), $	; image file format widget
	MCLambertSelector:long(0),$     ; Lambert Selector widget ID
	MPLambertSelector:long(0),$     ; Lambert Selector widget ID
	MCLambertMode: long(0) $		; Lambert sum or individual image mode
           }  ; end of widget_s structure definition

SEMdata = {SEMdatastruct, $
	; Monte Carlo parameters first 
    EBSDorECP:long(0), $            ; EBSD (0) or ECP (1) data type
	mcfilename: '', $				; Monte Carlo result file name
	mcfilesize: long64(0), $		; Monte Carlo file size [bytes]
	mcenergymin: float(0.0), $		; minimum energy
	mcenergymax: float(0.0), $		; maximum energy
	mcenergybinsize: float(0.0), $		; energy bin size
	mcenergynumbin: long(0), $		; number of energy bins
	voltage: float(0.0), $			; microscope voltage
	mcdepthmax: float(0.0), $		; maximum depth in MC file, or start depth in Kossel MP file
	mcdepthstep: float(0.0), $		; depth step size
	mcdepthnumbins: long(0), $		; number of depth bins
	mcimx: long(0), $				; number of pixels along x in modified Lambert map
	mcimy: long(0), $				; same along y
	mctotale: long64(0), $			; total number of electrons hitting the sample
	mcbse: long(0), $				; total number of BSE electrons
	mcsigstart: float(0.0), $		; start angle for ECP beam tilts 
	mcsigend: float(0.0), $			; end angle for ECP beam tilts 
	mcsigstep: float(0.0), $		; angular step size for ECP beam tilts 
	mcvangle: float(0.0), $			; vertical sample tilt angle (around TD)
	mchangle: float(0.0), $			; horizontal sample tilt angle (around RD)
	mcmode: '', $					; 'CSDA' (continuous slowing down approximation) or 'DLOS' (discrete losses)
	Esel: long(0), $				; energy selection for slider in MC and MP display routines
	mcprogname: '', $ 				; MC program name
	mcscversion: '', $ 				; source code version number
	mcdataedims: lon64arr(3), $		; dimensions of accum_e
	mcdatazdims: lon64arr(4), $		; dimensions of accum_z
    mcfilmthickness: float(0.0), $  ; film thickness for two-layer structures

	; then Master Pattern parameters
	mpfilename: '', $ 				; master pattern file name
	mpfiletype: long(0), $			; file type for master pattern
	mpfiletypestring: strarr(5), $	; file type strings for master pattern
	mpfilesize: long(0), $			; size (in bytes) of master pattern file
	mpimx: long(0), $				; number of x-pixels in master pattern (N in 2N+1)
	mpimy: long(0), $				; same along y
	numset: long(0), $				; number of positions in asymmetric unit
	Asymsel: long(-1), $			; which asymmetric unit position to display?
	atnum: lonarr(250), $			; number of atoms in asymmetric unit 
	mpenergynumbin: long(0), $		; number of energy bins (may be different from MC file)
	mpgridmode: '', $				; 'hex' or 'squ' for Lambert grid type
	xtalname: '', $					; crystal structure filename
	xtalname2: '', $				; crystal structure 2 filename
	mpprogname: '', $ 				; Master Pattern program name
	mpscversion: '', $ 				; source code version number
	mpdatadims: lon64arr(3), $		; dimensions of raw data array

	; detector parameters
	detL: float(0), $				; scintillator - sample distance [microns]
	detW: float(0), $				; working distance [millimeters]
	detRi: float(0), $				; BSE detector inner radius [millimeters]
	detRo: float(0), $				; BSE detector outer radius [millimeters]
	detsampleytilt: float(0), $		; sample y-tilt angle [degrees]
	dettheta: float(0), $			; detector tilt angle [degrees]
	detthetac: float(0), $			; incident beam cone semi-angle [degrees]
	detomega: float(0), $			; detector omega tilt angle [degrees]
	detdelta: float(0), $			; scintillator pixel size [microns]
	detnumsx: long(0), $			; number of x-pixels
	detnumsy: long(0), $			; number of y-pixels
	detxpc: float(0), $				; x-pattern center [pixels]
	detypc: float(0), $				; y-pattern center [pixels]
	detxs: long(0), $				; x sampling coordinate [integer]
	detys: long(0), $				; y sampling coordinate [integer]
	detxss: float(0), $				; x sampling step size [micron]
	detyss: float(0), $				; y sampling step size [micron]
	detbinning: long(0), $			; binning
	detbeamcurrent: float(0), $		; beam current [nA]
	detdwelltime: float(0), $		; dwell time [mu s]
	detphi1: float(0), $			; phi1 Euler angle 
	detphi: float(0), $				; phi Euler angle 
	detphi2: float(0), $			; phi2 Euler angle 
	detalphaBD: float(0), $			; transfer barrel distortion parameter
	EulerConvention: long(0), $		; Euler angle convention (TSL = 0, HKL = 1) [disabled for now; always TSL]
	BGmode: long(0), $				; background/full pattern display mode
	Eminsel: long(0), $				; min energy selection 
	Emaxsel: long(0), $				; max energy selection 
	PatternOrigin: long(0), $		; pattern origin indicator
	PatternScaling: long(0), $		; pattern scaling type (0=linear, 1=gamma)
	gammavalue: float(0), $			; gamma correction factor
	Patternmin: float(0), $			; pattern min indicator
	Patternmax: float(0), $			; pattern max indicator
	detax1: float(0), $				; axis angle pair component
	detax2: float(0), $				; axis angle pair component
	detax3: float(0), $				; axis angle pair component
	detax4: float(0), $				; axis angle pair component
	xstar: float(0), $				; x^* value
	ystar: float(0), $				; y^* value
	zstar: float(0), $				; z^* value
	currentpatternID: long(0), $	; pattern ID number
	showcircularmask: long(0), $	; show circular mask (or not)
	currentdisplaywidgetmode:long(0), $	; current display widget mode (single or other) 

	; pattern parameters for display
	Pmode: long(0), $				; pattern mode (0=single, 1=angle file, 2=dictionary)
	EBSDpatternfilename: '', $		; name for EBSD output file
	ECPpatternfilename: '', $     	; output file name widget
	Kosselpatternfilename: '', $    ; output file name widget
	EBSDanglefilename: '', $		; name for EBSD angle file
	ECPanglefilename: '', $		    ; name for ECP angle file
	Kosselanglefilename: '', $		; name for Kossel angle file
	EBSDdictfilename: '', $			; name for EBSD dictionary file
    NHSH: long(0), $                ; Northern or Southern hemisphere for display?
	angletype: '', $				; angle type (euler, quaternion)
	numangles: long(0), $			; number of angles in file
	Dictpointgroup: long(0), $		; point group number
	RodriguesFZType: long(0), $		; Fundamental Zone type (0=full, 1=Cyclic, 2=Dihedral, 3=Tetrahedral, 4=Octahedral)
	Ncubochoric: long(0), $			; number of sampling points for cubochoric cell (N, as in 2N+1)
	NinRFZ: long(0), $				; number of points in Rodrigues Fundamental Zone
	f90exepath: 'path_unknown', $	; path to f90 executables
	EMsoftpathname: 'path_unknown', $ ; new path to f90 executables

	; then general program parameters
	eventverbose: fix(0), $			; used for event debugging (0=off, 1=on)
	scversion: '', $				; source code version number
	pathname: '', $					; pathname (obviously)
	mcpathname: '', $				; MC pathname (obviously)
	suffix: '', $					; filename suffix 
	homefolder: '', $				; startup folder of the program
	EBSDroot: 'undefined', $		; current pathname (is stored in preferences file)
	EBSDMCroot: 'undefined', $		; current pathname (is stored in preferences file)
	appdir: appdir, $               ; location of the user application folder
	prefname: 'EBSDgui.prefs', $	; filename of preferences file (will be located inside data.appdir)
    foldersep: '/', $               ; folder separator character ('/' for OS X and Linux, '\' for Windows)
	nprefs: fix(0), $				; number of preferences in file
	MCLSmode: fix(0), $				; Monte Carlo Lambert Selector tag
	MCLSum: fix(0), $				; Monte Carlo Lambert sum or individual tag
	MPLSmode: fix(0), $				; Master Pattern Lambert Selector tag
    MCMPboth: long(0), $            ; switch for MC or MC/MP display
    MCmin: long(0), $               ; min value for MC display
    MCmax: long(0), $               ; max value for MC display
    MPmin: float(0), $              ; min value for MP display
    MPmax: float(0), $              ; max value for MP display
    imageformat: long(0), $         ; image format 'jpeg', 'tiff', 'bmp'
	logmode: fix(0), $				; keep a log file or not
	logunit: fix(13), $				; logical file unit for log file
	logname: '', $					; filename for log output
	logfileopen: fix(0), $			; log file is open when 1, closed when 0


	; widget location parameters
	xlocation: float(0.0), $		; main widget x-location (can be modified and stored in preferences file)
	ylocation: float(0.0), $		; main widget y-location (can be modified and stored in preferences file)
	patternxlocation: float(600.0), $	; pattern widget x-location (can be modified and stored in preferences file)
	patternylocation: float(100.0), $	; pattern widget y-location 
	EBSDxlocation: float(600.0), $	; image widget x-location (can be modified and stored in preferences file)
	EBSDylocation: float(100.0), $	; image widget y-location 
	MCxlocation: float(600.0), $	; Monte Carlo widget x-location (can be modified and stored in preferences file)
	MCylocation: float(100.0), $	; Monte Carlo widget y-location 
	MPxlocation: float(600.0), $	; Master Pattern widget x-location (can be modified and stored in preferences file)
	MPylocation: float(100.0), $	; Master Pattern widget y-location 
	Detectorxlocation: float(600.0), $	; detector widget x-location (can be modified and stored in preferences file)
	Detectorylocation: float(100.0), $	; detector widget y-location 
    scrdimx:0L, $                   ; display area x size in pixels 
    scrdimy:0L $                    ; display area y size in pixels 
        } ; end of data structure definition

; set the foldersep string
if ( (!version.os ne 'darwin') and (!version.os ne 'linux') ) then begin 
    SEMdata.foldersep = '\'
    fontstr='DejaVuSans'
    fontstrlarge='DejaVuSans Bold'
    fontstrsmall='DejaVuSans Italic'
end else begin
    fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
    fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
    fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'
endelse
SEMdata.appdir = SEMdata.appdir+SEMdata.foldersep

; here are the possible master pattern file types
SEMdata.mpfiletypestring = ['  ','EBSD','ECP','Kossel','TKD']

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
SEMdata.scrdimy = scr[1] * 0.8
SEMdata.scrdimx = scr[0]
SEMdata.xlocation = SEMdata.scrdimx / 8.0
SEMdata.ylocation = SEMdata.scrdimx / 8.0

;------------------------------------------------------------
; does the preferences file exist ?  If not, create it, otherwise read it
EBSDgetpreferences,/noprint

;------------------------------------------------------------
; get the pathname for the executables and the EMsoftLib library
SEMdata.f90exepath = Core_getenv(/bin)
SEMdata.EMsoftpathname = Core_getenv(/bin)
librarylocation = Core_getenv(/lib)

;------------------------------------------------------------
; create the top level widget
SEMwidget_s.base = WIDGET_BASE(TITLE='EBSD, ECP, and Kossel Pattern Display Program', $
                        /COLUMN, $
                        XSIZE=620, $
                        /ALIGN_LEFT, $
						/TLB_MOVE_EVENTS, $
						EVENT_PRO='SEMDisplay_event', $
                        XOFFSET=SEMdata.xlocation, $
                        YOFFSET=SEMdata.ylocation)

;------------------------------------------------------------
; create the two main columns
; block 1 is the left column, with logo 
block1 = WIDGET_BASE(SEMwidget_s.base, $
			/FRAME, $
			XSIZE=610, $
			/ALIGN_CENTER, $
			/COLUMN)

SEMwidget_s.logodraw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/ALIGN_CENTER, $
			XSIZE=600, $
			YSIZE=200)

;------------------------------------------------------------
;REMOVE FROM HERE -------------------------------------------
; with the new HDF5 data format that has both MC and Master pattern data in a single file
; there is no more need for the code below; we keep it here until everything has been 
; shown to work without problems.

; this is the block that will display Monte Carlo file information; no user interactions here
;block11 = WIDGET_BASE(block1, /FRAME, /COLUMN)

;file1 = WIDGET_BASE(block11, /ROW, XSIZE=600, /ALIGN_RIGHT)
;file2 = WIDGET_LABEL(file1, VALUE='Monte Carlo Simulation Data', font=fontstrlarge, /ALIGN_RIGHT)

;SEMwidget_s.MCbutton= WIDGET_BUTTON(file1, $
;                      UVALUE='MCDISPLAY', $
;                      VALUE='Display', $
;                      EVENT_PRO='SEMDisplay_event', $
;                      SENSITIVE=0, $
;		      /ALIGN_RIGHT, $
;                      /FRAME)

;---------- file name and size
;file1 = WIDGET_BASE(block11, /ROW, XSIZE=600, /ALIGN_CENTER)
;SEMwidget_s.mcfilename = Core_WText(file1,'MC Data File Name', fontstrlarge, 200, 25, 60, 1, SEMdata.mcfilename)
; UNTIL HERE

;------------------------------------------------------------
;------------------------------------------------------------
; this is the block that will display Master Pattern file information; no user interactions here
; except for a few buttons.
block21 = WIDGET_BASE(block1, /FRAME, /COLUMN, XSIZE=600, /ALIGN_RIGHT)
file2 = WIDGET_BASE(block21, /ROW, XSIZE=600, /ALIGN_RIGHT)
file3 = WIDGET_LABEL(file2, VALUE='Master Pattern Simulation Data', font=fontstrlarge, /ALIGN_CENTER)

SEMwidget_s.MPbutton = WIDGET_BUTTON(file2, $
                      	UVALUE='MPDISPLAY', $
                      	VALUE='Display', $
                      	EVENT_PRO='SEMDisplay_event', $
                      	SENSITIVE=0, $
		      		  	/ALIGN_RIGHT, $
                      	/FRAME)

;---------- file name and size
file1 = WIDGET_BASE(block21, /ROW, XSIZE=600, /ALIGN_CENTER)
SEMwidget_s.mpfilename = Core_WText(file1,'MP Data File Name', fontstrlarge, 200, 25, 60, 1, SEMdata.mpfilename)

;---------- file type
file1 = WIDGET_BASE(block21, /ROW, XSIZE=600, /ALIGN_CENTER)
SEMwidget_s.mpfiletype = Core_WText(file1,'Master File Type', fontstrlarge, 200, 25, 10, 1, SEMdata.mpfiletypestring[0])

;------------------------------------------------------------
;------------------------------------------------------------
; then we have the program message window

SEMwidget_s.status= WIDGET_TEXT(block1, $
			XSIZE=90, $
			YSIZE=15, $
			/SCROLL, $
			VALUE=' ',$
			/ALIGN_LEFT)

; the following is needed by the Core_Print routine
status = SEMwidget_s.status 

;------------------------------------------------------------
;------------------------------------------------------------
; finally we need a couple of control buttons: Quit, LoadMaster, Detector

file11 = WIDGET_BASE(block1, XSIZE=590, /FRAME, /ROW)

SEMwidget_s.mainstop = WIDGET_BUTTON(file11, $
                                VALUE='Quit', $
                                UVALUE='QUIT', $
                                EVENT_PRO='SEMDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

;REMOVE FROM HERE -------------------------------------------
;SEMwidget_s.mcloadfile = WIDGET_BUTTON(file11, $
;                                UVALUE='MCFILE', $
;                                VALUE='Load MC file', $
;                                EVENT_PRO='SEMDisplay_event', $
;                                SENSITIVE=1, $
;                                /FRAME)
;
; UNTIL HERE

SEMwidget_s.mploadfile = WIDGET_BUTTON(file11, $
                                UVALUE='MPFILE', $
                                VALUE='Load Master File', $
                                EVENT_PRO='SEMDisplay_event', $
                                SENSITIVE=1, $
                                /FRAME)

SEMwidget_s.detector = WIDGET_BUTTON(file11, $
                                UVALUE='DETECTOR', $
                                VALUE='Define detector', $
                                EVENT_PRO='SEMDisplay_event', $
                                SENSITIVE=0, $
                                /FRAME)

values = ['Off','On']
SEMwidget_s.logfile= CW_BGROUP(file11, $
						values, $
						/FRAME, $
                        LABEL_LEFT='LogFile', $
						/ROW, $
						/NO_RELEASE, $
						/EXCLUSIVE, $
						SET_VALUE=SEMdata.logmode, $
                        EVENT_FUNC='EBSDevent', $
						UVALUE='LOGFILE')

; the following is needed by the Core_Print routine
logmode = SEMdata.logmode
logunit = SEMdata.logunit

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,SEMwidget_s.base,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, SEMwidget_s.logodraw, GET_VALUE=drawID
SEMwidget_s.logodrawID = drawID
;
read_jpeg,'Resources/EMsoftVBFFlogo.jpeg',logo
wset,SEMwidget_s.logodrawID
tvscl,logo,true=1

; and hand over control to the xmanager
XMANAGER,"SEMDisplay",SEMwidget_s.base,/NO_BLOCK

end

