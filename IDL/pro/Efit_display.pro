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
; EMsoft:Efit_display.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_display.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Generates a display widget for the simulated EBSD pattern display
;
;> @date 10/15/15 MDG 1.0 first attempt at a user-friendly interface
;--------------------------------------------------------------------------
pro Efit_display,dummy

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

; the display widget will have a Close, Save, and Save format option available
; in addition to the display region.

; a few font strings (this will need to be redone for Windows systems)
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; create the top level widget
Efitwidget_s.displaybase = WIDGET_BASE(TITLE='Pattern Display Panel', $
                        /COLUMN, $
                        XSIZE=max([Efitdata.detnumsx+20,420]), $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='Efit_display_event', $
                        XOFFSET=Efitdata.xlocationdisplay, $
                        YOFFSET=Efitdata.ylocationdisplay)

block0 = WIDGET_BASE(Efitwidget_s.displaybase, $
			XSIZE=max([Efitdata.detnumsx,400]), $
			/ALIGN_CENTER, $
			/ROW)

; a close button
closedisplaybutton = WIDGET_BUTTON(block0, $
                                UVALUE='CLOSEDISPLAY', $
                                VALUE='Close', $
                                /NO_RELEASE, $
                                EVENT_PRO='Efit_display_event', $
                                SENSITIVE=1, $
                                /FRAME)

; a save button
savepattern = WIDGET_BUTTON(block0, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='Efit_display_event', $
                        UVALUE='SAVEPATTERN', $
                        SENSITIVE=1, $
                        /FRAME)

; and a format selector
vals = ['jpeg','tiff','bmp']
Efitwidget_s.imageformat = CW_BGROUP(block0, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        LABEL_LEFT = 'File Format', $
                        /FRAME, $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='PATTERNFORMAT', $
                        SET_VALUE=Efitdata.imageformat)

; the min-max indicators
Efitwidget_s.min = Core_WText(block0, 'Min/Max ',fontstr, 75, 25, 15, 1, '---')
Efitwidget_s.max = Core_WText(block0, '/',fontstr, 5, 25, 15, 1, '---')


; finally, the draw area...
block1 = WIDGET_BASE(Efitwidget_s.displaybase, $
			XSIZE=Efitdata.detnumsx, $
			/ALIGN_CENTER, $
			/COLUMN)

Efitwidget_s.draw = WIDGET_DRAW(block1, $
			COLOR_MODEL=2, $
			RETAIN=2, $
			/FRAME, $
			/ALIGN_CENTER, $
			XSIZE=Efitdata.detnumsx, $
			YSIZE=Efitdata.detnumsy)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,Efitwidget_s.displaybase,/REALIZE

; realize the draw widgets
WIDGET_CONTROL, Efitwidget_s.draw, GET_VALUE=drawID
Efitwidget_s.drawID = drawID
Efitdata.drawID = drawID

; and hand over control to the xmanager
XMANAGER,"Efit_display",Efitwidget_s.displaybase,/NO_BLOCK

end

