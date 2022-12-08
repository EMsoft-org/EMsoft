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
; EMsoft:ECPDetectorWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: ECPDetectorWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief basic detector GUI for ECP modality
;
;> @date 10/30/15 MDG 1.0 first version
;--------------------------------------------------------------------------
pro ECPDetectorWidget, event

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall

common PointGroups, PGTHD, PGTWD, DG
common projections, mcxcircle, mcycircle, mpxcircle, mpycircle, mcSPxcircle, mcSPycircle, mpSPxcircle, mpSPycircle 


;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("ECPDetectorWidget") NE 0) then begin
  print,'ECPDetectorWidget is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; create the top level widget
SEMwidget_s.detectorbase = WIDGET_BASE(TITLE='ECP Detector and Pattern Mode Widget', $
                        /ROW, $
                        XSIZE=810, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='ECPDetectorWidget_event', $
                        XOFFSET=SEMdata.Detectorxlocation, $
                        YOFFSET=SEMdata.Detectorylocation)

;------------------------------------------------------------
; create the two main columns
block1 = WIDGET_BASE(SEMwidget_s.detectorbase, $
			XSIZE=350, $
			/ALIGN_TOP, $
			/COLUMN)

block2 = WIDGET_BASE(SEMwidget_s.detectorbase, $
			XSIZE=440, $
			/ALIGN_TOP, $
			/COLUMN)

;------------------------------------------------------------
;------------------------------------------------------------
file1 = WIDGET_BASE(block1, /COLUMN, /FRAME, YPAD=8, XSIZE=340, /ALIGN_LEFT)
file2 = WIDGET_LABEL(file1, VALUE='Detector & Microscope Geometry', font=fontstrlarge, /ALIGN_LEFT, /FRAME)

; we'll create two columns, one with all the detector geometry parameters,
; the other with pattern variables, including Euler angles, Euler angle
; convention, origin position, display mode (background only or full pattern),
; energy filter window, ...

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detW = Core_WTextE(file2,'Working Distance [mm]', fontstr, 250, 25, 10, 1, string(SEMdata.detW,format="(F9.2)"),'DETW','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detRi = Core_WTextE(file2,'BSE Detector Inner Radius [mm]', fontstr, 250, 25, 10, 1, string(SEMdata.detRi,format="(F7.2)"),'DETRI','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detRo = Core_WTextE(file2,'BSE Detector Outer Radius [mm]', fontstr, 250, 25, 10, 1, string(SEMdata.detRo,format="(F7.2)"),'DETRO','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detsampleytilt = Core_WTextE(file2,'Sample y-Tilt Angle [deg]', fontstr, 250, 25, 10, 1, string(SEMdata.detsampleytilt,format="(F6.2)"),'DETSAMPLEYTILT','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detnumsx = Core_WTextE(file2,'Number of Pattern Pixels ', fontstr, 250, 25, 10, 1, string(SEMdata.detnumsx,format="(I4)"),'DETNUMSX','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detthetac = Core_WTextE(file2,'Incident Beam Cone Semi-Angle [deg]', fontstr, 250, 25, 10, 1, string(SEMdata.detthetac,format="(F6.2)"),'DETTHETAC','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detbeamcurrent = Core_WTextE(file2,'Beam current [nA]', fontstr, 140, 25, 10, 1, string(SEMdata.detbeamcurrent,format="(F9.2)"),'DETBEAMCURRENT','ECPDetectorWidget_event')

;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
SEMwidget_s.detdwelltime = Core_WTextE(file2,'Dwell Time [mu s] ', fontstr, 140, 25, 10, 1, string(SEMdata.detdwelltime,format="(F9.2)"),'DETDWELLTIME','ECPDetectorWidget_event')

;file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_CENTER)
;vals = ['TSL', 'HKL']
;SEMwidget_s.EulerConvention = CW_BGROUP(file2, $
;                        vals, $
;                        /ROW, $
;                        /NO_RELEASE, $
;                        /EXCLUSIVE, $
;                        FONT=fontstr, $
;                        LABEL_LEFT = 'Euler phi1 Convention', $
;                        EVENT_FUNC ='EBSDevent', $
;                        UVALUE='EBSDEULERCONVENTION', $
;                        SET_VALUE=SEMdata.EulerConvention)

;------------------------------------------------------------
;------------------------------------------------------------
; and here is the Close button
file1 = WIDGET_BASE(block1, XSIZE=340, /ALIGN_LEFT, /ROW)

SEMwidget_s.DetectorClose = WIDGET_BUTTON(file1, $
                                UVALUE='CLOSEDETECTOR', $
                                VALUE='Close', $
                                EVENT_PRO='ECPDetectorWidget_event', $
                                SENSITIVE=1, $
                                /FRAME)

;------------------------------------------------------------
;------------------------------------------------------------
; this box defines the pattern mode and the output file name
file1 = WIDGET_BASE(block2, /COLUMN, /FRAME, YPAD=8, XSIZE=430, /ALIGN_LEFT)
file2 = WIDGET_LABEL(file1, VALUE='Pattern Mode', font=fontstrlarge, /ALIGN_LEFT, /FRAME)

vals = ['Single Pattern','Angle File'] ; ,'Dictionary']
SEMwidget_s.Pmode = CW_BGROUP(file1, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        EVENT_FUNC ='ECPevent', $
                        UVALUE='PMODE', $
                        SET_VALUE=SEMdata.Pmode)

;------------------------------------------------------------
;------------------------------------------------------------
file1 = WIDGET_BASE(block2, /COLUMN, /FRAME, YPAD=8, XSIZE=430, /ALIGN_LEFT)
file2 = WIDGET_LABEL(file1, VALUE='Single Pattern Parameters', font=fontstrlarge, /ALIGN_LEFT,/FRAME)


;---------- 
file2 = WIDGET_BASE(file1, /ROW, XSIZE=430, /ALIGN_CENTER)
SEMwidget_s.detphi1 = Core_WTextE(file2,'Euler [deg] phi1', fontstr, 120, 25, 8, 1, string(SEMdata.detphi1,format="(F6.2)"),'DETphi1','ECPDetectorWidget_event')
SEMwidget_s.detphi = Core_WTextE(file2,' Phi', fontstr, 40, 25, 8, 1, string(SEMdata.detphi,format="(F6.2)"),'DETPhi','ECPDetectorWidget_event')
SEMwidget_s.detphi2 = Core_WTextE(file2,' phi2', fontstr, 40, 25, 8, 1, string(SEMdata.detphi2,format="(F6.2)"),'DETphi2','ECPDetectorWidget_event')

file2 = WIDGET_BASE(file1, /ROW, XSIZE=430, /ALIGN_CENTER)
SEMwidget_s.DisplayECP = WIDGET_BUTTON(file2, $
                                VALUE='Display Pattern', $
                                UVALUE='DISPLAYECP', $
                                EVENT_PRO='ECPDetectorWidget_event', $
				/ALIGN_LEFT, $
                                SENSITIVE=1, $
                                /FRAME)

vals = ['Off','On']
SEMwidget_s.circularmask = CW_BGROUP(file2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
			LABEL_LEFT='Circular Mask', $
                        EVENT_FUNC ='ECPevent', $
                        UVALUE='CIRCULARMASK', $
                        SET_VALUE=SEMdata.showcircularmask)

;------------------------------------------------------------
;------------------------------------------------------------
file1 = WIDGET_BASE(block2, /COLUMN, /FRAME, YPAD=8, XSIZE=430, /ALIGN_LEFT)
file3 = WIDGET_BASE(file1, /ROW, /ALIGN_LEFT)
file2 = WIDGET_LABEL(file3, VALUE='Angle File Parameters', font=fontstrlarge, /ALIGN_LEFT,/FRAME)

SEMwidget_s.GoAngle = WIDGET_BUTTON(file3, $
                                VALUE='Go', $
                                UVALUE='GOANGLE', $
                                EVENT_PRO='ECPDetectorWidget_event', $
                                SENSITIVE=0, $
                                /FRAME)


SEMwidget_s.ECPanglefilename = Core_WText(file1,'Angle File Name', fontstr, 200, 25, 50, 1, SEMdata.ECPanglefilename)

SEMwidget_s.ECPgetanglefilename = WIDGET_BUTTON(file1, $
                      UVALUE='GETANGLEFILENAME', $
                      VALUE='Load Angle File', $
                      EVENT_PRO='ECPDetectorWidget_event', $
                      SENSITIVE=0, $
		      /ALIGN_LEFT, $
                      /FRAME)

file2 = WIDGET_BASE(file1, /ROW, XSIZE=340, /ALIGN_LEFT)
SEMwidget_s.angletype = Core_WText(file2,'Angle Type', fontstr, 90, 25, 10, 1, SEMdata.angletype)
SEMwidget_s.numangles = Core_WText(file2,'# Angles  ', fontstr, 80, 25, 10, 1, string(SEMdata.numangles,format="(I8)"))


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,SEMwidget_s.detectorbase,/REALIZE

; and hand over control to the xmanager
XMANAGER,"ECPDetectorWidget",SEMwidget_s.detectorbase,/NO_BLOCK

end
