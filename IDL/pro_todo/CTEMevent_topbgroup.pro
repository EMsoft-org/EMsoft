
function CTEMevent_topbgroup, event
;
; event handler for top level button group
;

;------------------------------------------------------------
; common blocks
common widget_common, widget_s
common data_common, data

WIDGET_CONTROL, event.id, GET_UVALUE = eventval         ;find the user value

IF N_ELEMENTS(eventval) EQ 0 THEN RETURN,eventval

CASE eventval OF

 'SELECTION': begin
		WIDGET_CONTROL, get_value=val,widget_s.topbgroup
		print,'selected value = ',val
		print,' Iconifying base widget'
		WIDGET_CONTROL, ICONIFY=1, widget_s.base
		CASE val OF
		 0: begin
; this is the input widget option
			CTEM_InputWidget
		    endcase
		 1: begin
; this is the program execute widget option
			CTEM_ProgramExecuteWidget

		    endcase
		 2: begin
; this is the input widget option
			CTEM_ResultDisplayWidget

		    endcase
		 else: MESSAGE, "Eveny Value not found"
		ENDCASE

		wait,2
		print,' Restoring base widget'
		WIDGET_CONTROL, ICONIFY=0, widget_s.base
	 endcase

 'QUIT': begin
		WIDGET_CONTROL, widget_s.base, /DESTROY
	endcase

else: MESSAGE, "Event User Value Not Found"

endcase


return,eventval
end 
