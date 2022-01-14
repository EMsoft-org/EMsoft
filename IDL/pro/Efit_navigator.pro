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
; EMsoft:Efit_navigator.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_navigator.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Generates a detector space navigator interface
;
;> @date 08/04/16 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro Efit_navigator,dummy

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

; the navigator has 7 navigation buttons as well as a step size widget

; a few font strings (this will need to be redone for Windows systems)
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; create the top level widget
Efitwidget_s.navigatorbase = WIDGET_BASE(TITLE='Detector Navigator Panel', $
                        /COLUMN, $
                        XSIZE=400, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='Efit_navigator_event', $
                        XOFFSET=Efitdata.xlocationnavigator, $
                        YOFFSET=Efitdata.ylocationnavigator)

block1 = WIDGET_BASE(Efitwidget_s.navigatorbase, $
			XSIZE=380, $
			/FRAME, $
			/ALIGN_CENTER, $
			/COLUMN)

; first we have a text message explaining what this widget does ... 
tmp1 = WIDGET_LABEL(block1, VALUE='Detector Angular Navigator Controls', font=fontstr, /ALIGN_LEFT)

; then a row of three columns with buttons
row1 = WIDGET_BASE(block1, $
			XSIZE=360, $
			/FRAME, $
			/ALIGN_CENTER, $
			/ROW)

col1 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)
; CCW and LEFT buttons
Efitwidget_s.CCWbutton = WIDGET_BUTTON(col1, $
                                UVALUE='CCW', $
                                VALUE='CCW', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

Efitwidget_s.LEFTbutton = WIDGET_BUTTON(col1, $
                                UVALUE='LEFT', $
                                VALUE='LEFT', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

col2 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; UP, ZERO, and DOWN buttons
Efitwidget_s.UPbutton = WIDGET_BUTTON(col2, $
                                UVALUE='UP', $
                                VALUE='UP', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

Efitwidget_s.ZERObutton = WIDGET_BUTTON(col2, $
                                UVALUE='ZERO', $
                                VALUE='ZERO', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

Efitwidget_s.DOWNbutton = WIDGET_BUTTON(col2, $
                                UVALUE='DOWN', $
                                VALUE='DOWN', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

col3 = WIDGET_BASE(row1, $
			XSIZE=120, $
			/ALIGN_CENTER, $
			/COL)

; CW and RIGHT buttons
Efitwidget_s.CWbutton = WIDGET_BUTTON(col3, $
                                UVALUE='CW', $
                                VALUE='CW', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

Efitwidget_s.RIGHTbutton = WIDGET_BUTTON(col3, $
                                UVALUE='RIGHT', $
                                VALUE='RIGHT', $
                                EVENT_PRO='Efit_navigator_event', $
                                SENSITIVE=1, $
                                /FRAME)

; and finally, a text window for the user to set the navigation step size (in degrees)
row1 = WIDGET_BASE(block1, $
			XSIZE=360, $
			/FRAME, $
			/ALIGN_CENTER, $
			/ROW)

item1 = WIDGET_LABEL(row1, VALUE='Rotation step size (degrees)', font=fontstr, /ALIGN_LEFT)
; current value (editable)
Efitwidget_s.navstepsize = Core_WTextE(row1,'', fontstr, 10, 25, 10, 1, string(Efitdata.navstepsize,format="(F6.2)"), 'NAVSTEPSIZE','Efit_navigator_event')


;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,Efitwidget_s.navigatorbase,/REALIZE

; and hand over control to the xmanager
XMANAGER,"Efit_navigator",Efitwidget_s.navigatorbase,/NO_BLOCK

end
