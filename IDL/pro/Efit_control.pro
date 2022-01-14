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
; EMsoft:Efit_control.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Efit_control.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Generates a controller widget for the simulated EBSD pattern display
;
;> @date 10/15/15 MDG 1.0 first attempt at a user-friendly interface
;> @date 10/20/15 MDG 1.1 removed cancel button
;--------------------------------------------------------------------------
pro Efit_control,dummy

common Efit_widget_common, Efitwidget_s
common Efit_data_common, Efitdata

; the controller will have only one components: a list op display options 

; a few font strings (this will need to be redone for Windows systems)
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; create the top level widget
Efitwidget_s.controlbase = WIDGET_BASE(TITLE='Display Control Panel', $
                        /COLUMN, $
                        XSIZE=400, $
                        /ALIGN_LEFT, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='Efit_control_event', $
                        XOFFSET=Efitdata.xlocationcontrol, $
                        YOFFSET=Efitdata.ylocationcontrol)

vals = [' Display Experimental Pattern',' Display Simulated Pattern',' Display Difference Pattern',' Display Overlap Pattern',' Display Color Overlap',' Flicker']
Efitwidget_s.displayoption = CW_BGROUP(Efitwidget_s.controlbase, $
                        vals, $
                        /COLUMN, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        EVENT_FUNC ='Efitevent', $
                        UVALUE='DISPLAYOPTION', $
                        SET_VALUE=Efitdata.displayoption)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,Efitwidget_s.controlbase,/REALIZE

; and hand over control to the xmanager
XMANAGER,"Efit_control",Efitwidget_s.controlbase,/NO_BLOCK

end
