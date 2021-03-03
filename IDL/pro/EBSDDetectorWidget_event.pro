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
; EMsoft:EBSDDetectorWidget_event.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDDetectorWidget_event.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main event handler for detector widget
;
;> @date 05/07/14 MDG 1.0 first version
;--------------------------------------------------------------------------
pro EBSDDetectorWidget_event, event

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall


if (SEMdata.eventverbose eq 1) then help,event,/structure

; intercept the detector widget movement here 
if (event.id eq SEMwidget_s.detectorbase) then begin
  SEMdata.Detectorxlocation = event.x
  SEMdata.Detectorylocation = event.y-25
end else begin

  WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

  CASE eventval OF

 'DETL': begin
	  SEMdata.detL = Core_WidgetEvent( SEMwidget_s.detL,  'Detector distance set to [micron] ', '(F9.2)', /flt)
	  EBSD_updatePC,/display
	endcase

 'DETTHETA': SEMdata.dettheta = Core_WidgetEvent( SEMwidget_s.dettheta, 'Detector angle set to [deg] ', '(F8.2)', /flt)
 'DETOMEGA': SEMdata.detomega = Core_WidgetEvent( SEMwidget_s.detomega, 'Detector omega angle set to [deg] ', '(F6.2)', /flt)

 'DETDELTA': begin
	  SEMdata.detdelta = Core_WidgetEvent( SEMwidget_s.detdelta, 'Scintillator pixel size set to [micron] ', '(F6.2)', /flt)
	  EBSD_updatePC,/display
	endcase

 'DETNUMSX': begin
	  SEMdata.detnumsx = Core_WidgetEvent( SEMwidget_s.detnumsx, 'Scintillator number of pixels along x set to ', '(I4)', /lng)
	  EBSD_updatePC,/display
	endcase
 'DETNUMSY': begin
	  SEMdata.detnumsy = Core_WidgetEvent( SEMwidget_s.detnumsy, 'Scintillator number of pixels along y set to ', '(I4)', /lng)
	  EBSD_updatePC,/display
	endcase

 'DETXPC': begin
	  SEMdata.detxpc = Core_WidgetEvent( SEMwidget_s.detxpc, 'Pattern Center x-coordinate set to [pixels] ', '(F7.2)', /flt)
	  EBSD_updatePC,/display
	endcase

 'DETYPC': begin
	  SEMdata.detypc = Core_WidgetEvent( SEMwidget_s.detypc, 'Pattern Center y-coordinate set to [pixels] ', '(F7.2)', /flt)
	  EBSD_updatePC,/display
	endcase

 'DETXS': begin
	  SEMdata.detxs = Core_WidgetEvent( SEMwidget_s.detxs, 'Sampling pixel x-coordinate set to [integer] ', '(I4)', /lng)
	endcase

 'DETYS': begin
	  SEMdata.detys = Core_WidgetEvent( SEMwidget_s.detys, 'Sampling pixel y-coordinate set to [integer] ', '(I4)', /lng)
	endcase

 'DETXSS': begin
	  SEMdata.detxss = Core_WidgetEvent( SEMwidget_s.detxss, 'Sampling x step size to [micron] ', '(F9.3)', /flt)
	endcase

 'DETYSS': begin
	  SEMdata.detyss = Core_WidgetEvent( SEMwidget_s.detyss, 'Sampling y step size to [micron] ', '(F9.3)', /flt)
	endcase

 'DETBEAMCURRENT': SEMdata.detbeamcurrent = Core_WidgetEvent( SEMwidget_s.detbeamcurrent, 'Beam current set to [nA] ', '(F9.2)', /flt)
 'DETDWELLTIME': SEMdata.detdwelltime = Core_WidgetEvent( SEMwidget_s.detdwelltime, 'Dwell time set to [mu s] ', '(F9.2)', /flt)
 'DETALPHABD': SEMdata.detalphaBD = Core_WidgetEvent( SEMwidget_s.detalphaBD, 'Transfer optics barrel distortion parameter set to [x10^(-10)]', '(F10.4)', /flt)

 'DETax1': SEMdata.detax1 = Core_WidgetEvent( SEMwidget_s.detax1, 'Axis-angle entry 1 set to ', '(F6.2)', /flt)
 'DETax2': SEMdata.detax2 = Core_WidgetEvent( SEMwidget_s.detax2, 'Axis-angle entry 2 set to ', '(F6.2)', /flt)
 'DETax3': SEMdata.detax3 = Core_WidgetEvent( SEMwidget_s.detax3, 'Axis-angle entry 3 set to ', '(F6.2)', /flt)
 'DETax4': SEMdata.detax4 = Core_WidgetEvent( SEMwidget_s.detax4, 'Axis-angle entry 4 set to [deg] ', '(F6.2)', /flt)

 'EBSDMINENERGYLIST': begin
		SEMdata.Eminsel = fix(event.index)
		if (SEMdata.Eminsel gt SEMdata.Emaxsel) then begin
			SEMdata.Emaxsel = SEMdata.Eminsel
			WIDGET_CONTROL, set_droplist_select = SEMdata.Emaxsel, SEMwidget_s.EBSDmaxenergylist
		end
		WIDGET_CONTROL, set_droplist_select = SEMdata.Eminsel, SEMwidget_s.EBSDminenergylist
	 endcase

 'EBSDMAXENERGYLIST': begin
		SEMdata.Emaxsel = fix(event.index)
		if (SEMdata.Emaxsel lt SEMdata.Eminsel) then begin
			SEMdata.Eminsel = SEMdata.Emaxsel
			WIDGET_CONTROL, set_droplist_select = SEMdata.Eminsel, SEMwidget_s.EBSDminenergylist
		end
		WIDGET_CONTROL, set_droplist_select = SEMdata.Emaxsel, SEMwidget_s.EBSDmaxenergylist
	 endcase

 'GETEBSDFILENAME': begin
; display a filesaving widget in the data folder with the file extension filled in
	  filename = DIALOG_PICKFILE(/write,path=SEMdata.pathname,title='enter EBSD output file name ')
	  if (filename ne '') then begin
	    SEMdata.EBSDpatternfilename = filename
; and correct this for the relative pathnames of the EMsoft package
            SEMdata.EBSDpatternfilename = strmid(SEMdata.EBSDpatternfilename,strpos(SEMdata.EBSDpatternfilename,SEMdata.mcpathname))
	    WIDGET_CONTROL, set_value=filename, SEMwidget_s.EBSDpatternfilename
	    WIDGET_CONTROL, SEMwidget_s.DisplayEBSD, sensitive=1
	    WIDGET_CONTROL, SEMwidget_s.EBSDgetanglefilename, sensitive=1
	  end
	endcase


 'DETphi1': SEMdata.detphi1 = Core_WidgetEvent( SEMwidget_s.detphi1, 'Euler angle phi1 set to [deg] ', '(F6.2)', /flt)
 'DETPhi': SEMdata.detphi = Core_WidgetEvent( SEMwidget_s.detphi, 'Euler angle Phi set to [deg] ', '(F6.2)', /flt)
 'DETphi2': SEMdata.detphi2 = Core_WidgetEvent( SEMwidget_s.detphi2, 'Euler angle phi2 set to [deg] ', '(F6.2)', /flt)

 'GETANGLEFILENAME': begin
; display a filesaving widget in the data folder with the file extension filled in
	  filename = DIALOG_PICKFILE(/write,path=SEMdata.pathname,title='Select angle input file')
	  if (filename ne '') then begin
	    SEMdata.EBSDanglefilename = filename
	    WIDGET_CONTROL, set_value=filename, SEMwidget_s.EBSDanglefilename
	    EBSDreadanglefile,filename,/list
	    WIDGET_CONTROL, SEMwidget_s.GoAngle, sensitive=1
	  end
	endcase

 'DICTIONARYPG': begin
	  SEMdata.Dictpointgroup = event.index
	  if ( (SEMdata.Ncubochoric ne 0) and (SEMdata.EBSDdictfilename ne '') ) then begin
	    WIDGET_CONTROL, SEMwidget_s.GoDict, sensitive=1
	  end
	endcase

 'NCUBOCHORIC': begin 
	  SEMdata.Ncubochoric = Core_WidgetEvent( SEMwidget_s.Ncubochoric,  'Number of smapling points along cube semi-edge set to ', '(I4)', /lng)
	  if ( (SEMdata.Dictpointgroup ne 0) and (SEMdata.EBSDdictfilename ne '') ) then begin
	    WIDGET_CONTROL, SEMwidget_s.GoDict, sensitive=1
	  end
	endcase

;'GETDICTFILENAME': begin
; display a filesaving widget 
;  filename = DIALOG_PICKFILE(/write,path=SEMdata.pathname,title='Set dictionary angle file name ')
;  if (filename ne '') then begin
;    SEMdata.EBSDdictfilename = filename
;    WIDGET_CONTROL, set_value=filename, SEMwidget_s.EBSDdictfilename
;  end
;  if ( (SEMdata.Dictpointgroup ne 0) and (SEMdata.Ncubochoric ne 0) ) then begin
;    WIDGET_CONTROL, SEMwidget_s.GoDict, sensitive=1
;  end
;endcase

;'GODICT': begin
; this option calls the sampleRFZ program to create a dictionary angle file
;      openw,10,SEMdata.pathname+'/CTEMsampleRFZ.nml'
;      printf,10,'&RFZlist'
;      printf,10,'pgnum = '+string(SEMdata.Dictpointgroup,FORMAT="(I2)")
;      printf,10,'nsteps = '+string(SEMdata.Ncubochoric,FORMAT="(I6)")
;      printf,10,'outname = '''+SEMdata.EBSDdictfilename+''''
;      printf,10,'/'
;      close,10
; then run the CTEMsampleRFZ program on this file to create the angle file
;      cmd = SEMdata.f90exepath+'/CTEMsampleRFZ '+SEMdata.pathname+'/CTEMsampleRFZ.nml'
;      spawn,cmd
; then read the number of entries from that file and display it, then activate the GO button
;      EBSDreadanglefile, SEMdata.EBSDdictfilename
;      WIDGET_CONTROL, set_value=string(SEMdata.numangles,FORMAT="(I8)"), SEMwidget_s.NinRFZ
;      WIDGET_CONTROL, SEMwidget_s.GoDictionary, sensitive=1
;endcase

 'DISPLAYEBSD': begin
; first we need to make sure that the path to the fortran executables is known... this is stored in the 
; preferences file, but is initially set to 'path_unknown'
          if (SEMdata.f90exepath eq 'path_unknown') then SEMdata.f90exepath = Core_getenv(/bin)+'Build/Bin/'

; is the correct widget up on the screen ?
	  if XRegistered("EBSDPatternWidget") then begin
	    if (SEMdata.currentdisplaywidgetmode ne 0) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
	  end

; this does two things.  First of all, the CTEMEBSD program is called with the current
; parameters for the detector and microscope geometry, and the single set of Euler angles
; entered in the 'Detector and Pattern Mode Widget'.
;
; Then, when the CTEMEBSD program has produced its output file, we create a new widget
; that displays this raw EBSD pattern; the user can then apply a number of intensity
; corrections, as well as binning, to optimize the pattern quality.  Once this is done, 
; those parameters will be used for the Angle File mode and the Dictionary mode.

; first, create the nml file and execute the CTEMEBSD program
	  status = 0
	  EBSDExecute,status,/single

; then we create the EBSDpattern widget and let the user adjust the imaging parameters
	  if (status eq 1) then begin
	    if (XRegistered("EBSDPatternWidget") EQ 0) then EBSDPatternWidget,/single else EBSDshowPattern,/single
	  end

	endcase

 'GOANGLE': begin
; first we need to make sure that the path to the fortran executables is known... this is stored in the 
; preferences file, but is initially set to 'path_unknown'
          if (SEMdata.f90exepath eq 'path_unknown') then SEMdata.f90exepath = Core_getenv(/bin)+'Build/Bin/'

; is the correct widget up on the screen ?
	  if XRegistered("EBSDPatternWidget") then begin
	    if (SEMdata.currentdisplaywidgetmode ne 1) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
	  end

; this does two things.  First of all, the CTEMEBSD program is called with the current
; parameters for the detector and microscope geometry, and the angle file
;
; Then, when the CTEMEBSD program has produced its output file, we create a new widget
; that displays these EBSD patterns; the user can then save selected patterns or all patterns.
; At this point, there is no option to change the imaging parameters; all the settings of the 
; other parts of the widget apply to this pattern calculation

; first, create the nml file and execute the CTEMEBSD program
	  status = 0
	  EBSDExecute,status

; then we create the EBSDpattern widget and let the user adjust the imaging parameters
	  if (status eq 1) then begin
	    if (XRegistered("EBSDPatternWidget") EQ 0) then EBSDPatternWidget else EBSDshowPattern
	  end

	endcase

;'GODICTIONARY': begin
; first we need to make sure that the path to the fortran executables is known... this is stored in the 
; preferences file, but is initially set to 'path_unknown'
;         if (SEMdata.f90exepath eq 'path_unknown') then SEMdata.f90exepath = Core_getenv(/bin)+'Build/Bin/'

; is the correct widget up on the screen ?
;  if XRegistered("EBSDPatternWidget") then begin
;    if (SEMdata.currentdisplaywidgetmode ne 1) then WIDGET_CONTROL, SEMwidget_s.patternbase, /DESTROY
;  end

; this does two things.  First of all, the CTEMEBSD program is called with the current
; parameters for the detector and microscope geometry, and the angle file
;
; Then, when the CTEMEBSD program has produced its output file, we create a new widget
; that displays these EBSD patterns; the user can then save selected patterns or all patterns.
; At this point, there is no option to change the imaging parameters; all the settings of the 
; other parts of the widget apply to this pattern calculation

; first, create the nml file and execute the CTEMEBSD program
;  status = 0
;  EBSDExecute,status

; then we create the EBSDpattern widget and let the user adjust the imaging parameters
;  if (status eq 1) then begin
;    if (XRegistered("EBSDPatternWidget") EQ 0) then EBSDPatternWidget else EBSDshowPattern
;  end

;endcase

 'CLOSEDETECTOR': begin
; kill the base widget
		WIDGET_CONTROL, SEMwidget_s.detectorbase, /DESTROY
	endcase

  else: MESSAGE, "Event User Value Not Found"

  endcase

endelse

end 
