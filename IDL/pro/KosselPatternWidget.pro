; Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
; All rights reserved.
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
; EMsoft:KosselPatternWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: KosselPatternWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief main widget for display of the computed ECP pattern
;
;> @date 11/09/15 MDG 1.0 first version
;--------------------------------------------------------------------------
pro KosselPatternWidget, single=single

; the keyword /single indicates that only one pattern is available

;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
common ECPdata, ECPattern


;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("KosselPatternWidget") NE 0) then begin
  print,'ECPPatternWidget is already running ... (if it is not, please restart your IDL session)'
  return
end

psize = max( [600,SEMdata.detnumsx+100] )
;------------------------------------------------------------
; create the top level widget
SEMwidget_s.patternbase = WIDGET_BASE(TITLE='Pattern Display Widget', $
                        /ROW, $
                        XSIZE=psize, $
                        /ALIGN_CENTER, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='KosselPatternWidget_event', $
                        XOFFSET=SEMdata.patternxlocation, $
                        YOFFSET=SEMdata.patternylocation)


block1 = WIDGET_BASE(SEMwidget_s.patternbase, $
			XSIZE=psize, $
			/ALIGN_TOP, $
			/COLUMN)

if keyword_set(single) then begin
 SEMdata.currentdisplaywidgetmode = 0
;------------------------------------------------------------
;------------------------------------------------------------
  file1 = WIDGET_BASE(block1, /COLUMN, /FRAME, YPAD=8, XSIZE=600, /ALIGN_LEFT)
  file2 = WIDGET_LABEL(file1, VALUE='Pattern parameters', font=fontstrlarge, /ALIGN_LEFT, /FRAME)

;------------------------------------------------------------
  file2 = WIDGET_BASE(file1, /ROW, XSIZE=600, /ALIGN_CENTER)
  vals = ['UL','LL','UR','LR']
  SEMwidget_s.PatternOrigin = CW_BGROUP(file2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        LABEL_LEFT = 'Pattern Origin', $
                        EVENT_FUNC ='Kosselevent', $
                        UVALUE='KOSSELPATTERNORIGIN', $
                        SET_VALUE=SEMdata.PatternOrigin)

;------------------------------------------------------------
  file2 = WIDGET_BASE(file1, /ROW, XSIZE=600, /ALIGN_CENTER)
  vals = ['linear','gamma']
  SEMwidget_s.PatternScaling = CW_BGROUP(file2, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        LABEL_LEFT = 'Pattern Scaling', $
                        EVENT_FUNC ='Kosselevent', $
                        UVALUE='KOSSELPATTERNSCALING', $
                        SET_VALUE=SEMdata.PatternScaling)


  file2 = WIDGET_BASE(file1, /ROW, XSIZE=600, /ALIGN_CENTER)
; here's a slider to select the gamma setting ...
  SEMwidget_s.gammaslider = CW_FSLIDER(file2, $
;		EVENT_PRO='KosselPatternWidget_event', $
			/EDIT, $
			MINIMUM = 0.01, $
			MAXIMUM = 2.00, $
			FORMAT = "(F4.2)", $
			TITLE = 'Gamma Correction Factor', $
			XSIZE = 400, $
			VALUE = SEMdata.gammavalue, $
			UVALUE = 'GAMMASLIDER')

;------------------------------------------------------------
;------------------------------------------------------------
; and here's the display window itself
SEMwidget_s.Patterndraw = WIDGET_DRAW(block1, $
                        COLOR_MODEL=2, $
                        RETAIN=2, $
                        /FRAME, $
                        XSIZE=SEMdata.detnumsx, $
                        YSIZE=SEMdata.detnumsx)

; and the min-max indicators
block4 = WIDGET_BASE(block1, /ROW, /ALIGN_CENTER)
SEMwidget_s.Patternmin = Core_WText(block4, 'min/max ',fontstr, 75, 25, 15, 1, string(SEMdata.Patternmin,FORMAT="(F9.3)"))
SEMwidget_s.Patternmax = Core_WText(block4, '/',fontstr, 5, 25, 15, 1, string(SEMdata.Patternmax,FORMAT="(F9.3)"))

; and a save button
block4 = WIDGET_BASE(block1, /ROW, /ALIGN_CENTER)
saveKosselPattern = WIDGET_BUTTON(block4, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='KosselPatternWidget_event', $
                        /FRAME, $
                        UVALUE='SAVEKOSSELPATTERN', $
                        /ALIGN_LEFT)

; and the save format selector
vals = ['jpeg','tiff','bmp']
SEMwidget_s.EBSDformatbgroup = CW_BGROUP(block4, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        LABEL_LEFT = 'File Format', $
                        /FRAME, $
                        EVENT_FUNC ='Kosselevent', $
                        UVALUE='KOSSELFORMAT', $
                        SET_VALUE=SEMdata.imageformat)

;------------------------------------------------------------
;------------------------------------------------------------
; and here is the Close button
file1 = WIDGET_BASE(block1, XSIZE=340, /ALIGN_LEFT, /ROW)

SEMwidget_s.PatternClose = WIDGET_BUTTON(file1, $
                                UVALUE='PATTERNCLOSE', $
                                VALUE='Close', $
                                EVENT_PRO='KosselPatternWidget_event', $
                                SENSITIVE=1, $
                                /FRAME)


end else begin
; this is not single image mode, so we do not need to display the 
; image scaling parameters and such; we'll have a simpler interface
; that allows the user to browse through the series of images, 
; and save individual ones or all of them (after warning)

 SEMdata.currentdisplaywidgetmode = 1
;------------------------------------------------------------
;------------------------------------------------------------
  file1 = WIDGET_BASE(block1, /COLUMN, YPAD=8, XSIZE=psize, /ALIGN_LEFT)
  file2 = WIDGET_BASE(file1, /ROW, XSIZE=600, /ALIGN_LEFT)

saveECPattern = WIDGET_BUTTON(file2, $
                        VALUE='Previous', $
                        /NO_RELEASE, $
                        EVENT_PRO='KosselPatternWidget_event', $
                        /FRAME, $
                        UVALUE='PREVIOUSKOSSELPATTERN', $
                        /ALIGN_LEFT)

saveECPattern = WIDGET_BUTTON(file2, $
                        VALUE='Next', $
                        /NO_RELEASE, $
                        EVENT_PRO='KosselPatternWidget_event', $
                        /FRAME, $
                        UVALUE='NEXTKOSSELPATTERN', $
                        /ALIGN_LEFT)

; display the euler angles/quaternion for the currently displayed pattern
; we'll do this as a simple non-editable text widget

file2 = WIDGET_BASE(block1, /ROW, XSIZE=600, /ALIGN_CENTER)
SEMwidget_s.angledisplay = Core_WText(file2,'Orientation:', fontstr, 120, 25, 60, 1, ' ')


; then the display window
SEMwidget_s.Patterndraw = WIDGET_DRAW(block1, $
                        COLOR_MODEL=2, $
                        RETAIN=2, $
                        /FRAME, $
                        XSIZE=SEMdata.detnumsx, $
                        YSIZE=SEMdata.detnumsy)

; and the min-max indicators
block4 = WIDGET_BASE(block1, /ROW, /ALIGN_CENTER)
SEMwidget_s.Patternmin = Core_WText(block4, 'min/max ',fontstr, 75, 25, 15, 1, string(SEMdata.Patternmin,FORMAT="(F9.3)"))
SEMwidget_s.Patternmax = Core_WText(block4, '/',fontstr, 5, 25, 15, 1, string(SEMdata.Patternmax,FORMAT="(F9.3)"))

; a save all button
block4 = WIDGET_BASE(block1, /ROW, /ALIGN_CENTER)
saveECPattern = WIDGET_BUTTON(block4, $
                        VALUE='SaveAll', $
                        /NO_RELEASE, $
                        EVENT_PRO='KosselPatternWidget_event', $
                        /FRAME, $
                        UVALUE='SAVEALLKOSSELPATTERNS', $
                        /ALIGN_LEFT)

; and a save button
saveECPattern = WIDGET_BUTTON(block4, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='KosselPatternWidget_event', $
                        /FRAME, $
                        UVALUE='SAVEKOSSELPATTERN', $
                        /ALIGN_LEFT)

; and the save format selector
vals = ['jpeg','tiff','bmp']
SEMwidget_s.EBSDformatbgroup = CW_BGROUP(block4, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstr, $
                        LABEL_LEFT = 'File Format', $
                        /FRAME, $
                        EVENT_FUNC ='Kosselevent', $
                        UVALUE='KOSSELFORMAT', $
                        SET_VALUE=SEMdata.imageformat)

;------------------------------------------------------------
;------------------------------------------------------------
; and here is the Close button
file1 = WIDGET_BASE(block1, XSIZE=340, /ALIGN_LEFT, /ROW)

SEMwidget_s.PatternClose = WIDGET_BUTTON(file1, $
                                UVALUE='PATTERNCLOSE', $
                                VALUE='Close', $
                                EVENT_PRO='KosselPatternWidget_event', $
                                SENSITIVE=1, $
                                /FRAME)
end




;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,SEMwidget_s.patternbase,/REALIZE

; realize the draw widget
WIDGET_CONTROL, SEMwidget_s.Patterndraw, GET_VALUE=drawID
SEMwidget_s.PatternDrawID = drawID

; and display the pattern with the current intensity settings
if (SEMdata.Pmode eq 0) then KosselshowPattern,/single else KosselshowPattern

; and hand over control to the xmanager
XMANAGER,"KosselPatternWidget",SEMwidget_s.patternbase,/NO_BLOCK

end

