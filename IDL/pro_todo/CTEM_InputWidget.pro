
pro CTEM_InputWidget

;---------------------------------------------------------------------
;---------------------------------------------------------------------
; Creates a widget that handles all parameter input 
;
; history
;
; MDG 1.0 06/06/12 original routine
;
;---------------------------------------------------------------------
;---------------------------------------------------------------------


;------------------------------------------------------------
; make sure that this widget isn't already running
if (XRegistered("CTEMINPUTWIDGET") NE 0) then begin
  print,'CTEMINPUTWIDGET is already running ... (if it is not, please restart your IDL session)'
  return
end


; a few font strings
fontstr='-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1'
fontstrlarge='-adobe-new century schoolbook-medium-r-normal--20-140-100-100-p-103-iso8859-1'
fontstrsmall='-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1'

;------------------------------------------------------------
; create the top level widget
widget_s.inputbase = WIDGET_BASE(TITLE='CTEMsoft Parameter Input GUI', $
			    /COLUMN, $
			    XOFFSET=xlocation, $
			    YOFFSET=ylocation)





WIDGET_CONTROL,widget_s.inputbase,/REALIZE

XMANAGER,"CTEMINPUTWIDGET",widget_s.inputbase,/NO_BLOCK





end pro
