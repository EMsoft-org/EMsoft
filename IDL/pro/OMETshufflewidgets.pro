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
; EMsoft:OMETshufflewidgets.pro
;--------------------------------------------------------------------------
;
; PROGRAM: OMETshufflewidgets.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief shuffles widgets around and optionally deletes one of them as well.
;
;> @date 02/18/17 MDG 1.0 first version
;--------------------------------------------------------------------------
pro OMETshufflewidgets, eventx, curchainID

;------------------------------------------------------------
; common blocks
common OMET_widget_common, OMETwidget_s
common OMET_data_common, OMETdata
common fontstrings, fontstr, fontstrlarge, fontstrsmall
; common getenv_common, librarylocation
common CommonCore, status, logmode, logunit
common OMET_optelem_common, optelemptr

if (OMETdata.eventverbose eq 1) then print,'entering OMETshufflewidgets', curchainID

; if we're going to move a widget, there's a good chance that others will need to be moved as well,
; so we need to turn off temporarily their sensitivity to move events to prevent their event handler
; routines from being called; this caused all sorts of problems during the debug phase of this program...
OMETwidgetsensitivity, curchainID, /off

wtype = ['',  'polarizer', 'retarder', 'rotator', 'Stokes vector (source)', 'sample', 'Stokes vector (detector)']

maxoenum = OMETgetnextfreepos()
maxposx = (maxoenum-1) * OMETdata.xstepsize

if (OMETdata.eventverbose eq 1) then OMETdebug

; first, make sure that we are not trying to move the widget beyond the end of the chain
if (eventx ge maxposx) then begin
	; move the widget back to its original location  since we tried to move it past the output Stokes vector
	    print,'widget moved too far to the right ', eventx, maxposx
	    WIDGET_CONTROL, OMETwidget_s.chainIDs[curchainID], TLB_SET_XOFFSET = (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'1 moving widget '+wtype[(*optelemptr[curchainID]).oetype]+':'+string(curchainID,format="(I2)")+' to '+string((*optelemptr[curchainID]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
end else begin
; determine where to move the curchainID widget to, and also which other widgets need to be moved 	
; this requires a bit of logic ,,,
    if (curchainID eq OMETgetnextfreepos()-1) then begin
	; if this is the last widget in the row, then we can only move it to the left and we need to move the 
	; others to the right... 
	    newpos = fix(eventx/OMETdata.xstepsize)
	    if (newpos eq 0) then begin
	    	Core_Print,'input Stokes vector must be first optical element '
    		WIDGET_CONTROL, OMETwidget_s.chainIDs[curchainID], TLB_SET_XOFFSET = (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'2 moving widget '+wtype[(*optelemptr[curchainID]).oetype]+':'+string(curchainID,format="(I2)")+' to '+string((*optelemptr[curchainID]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
    	end else begin
		    print,'widget moved to the left ', eventx, (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, newpos
		    ; next, we need to move the various pointers and reset the OMETdata.oenum value for all affected widgets
		    hold = optelemptr[curchainID]   ; store the current pointer in a holding variable
		    holdWID = OMETwidget_s.chainIDs[curchainID]
		    for i=maxoenum-1,newpos+1,-1 do begin
		    	(*optelemptr[i-1]).oenum += 1
		    	optelemptr[i] = optelemptr[i-1]
		    	print,'moving ',i-1,' to ',i, maxoenum, newpos
		    	OMETwidget_s.chainIDs[i] = OMETwidget_s.chainIDs[i-1]
			    WIDGET_CONTROL, OMETwidget_s.chainIDs[i], TLB_SET_XOFFSET = (*optelemptr[i]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'3 moving widget '+wtype[(*optelemptr[i]).oetype]+':'+string(i,format="(I2)")+' to '+string((*optelemptr[i]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
		    endfor
		    optelemptr[newpos] = hold
		    (*optelemptr[newpos]).oenum = newpos
	    	OMETwidget_s.chainIDs[newpos] = holdWID
		    WIDGET_CONTROL, OMETwidget_s.chainIDs[newpos], TLB_SET_XOFFSET = newpos*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'4 moving widget '+wtype[(*optelemptr[newpos]).oetype]+':'+string(newpos,format="(I2)")+' to '+string(newpos*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
		endelse
    ;endif
end else begin
    if ((curchainID lt OMETdata.availableOEnumber-1) and (curchainID gt 0)) then begin
	; this is not the last or the first widget, so we can really move it either to the left or to the right 
	; the logic is thus a little more complicated than the case above...
	; get the new position to move to as well as the old position
	    newpos = fix(eventx/OMETdata.xstepsize)
	    oldpos = curchainID
	    print,' oldpos/newpos ',oldpos, newpos
	    if (oldpos eq newpos) then begin
	    ; don't move the widget, just reposition it back where it came from... perhaps the user changed their mind about moving it ...
		    WIDGET_CONTROL, OMETwidget_s.chainIDs[curchainID], TLB_SET_XOFFSET = (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'5 moving widget '+wtype[(*optelemptr[curchainID]).oetype]+':'+string(curchainID,format="(I2)")+' to '+string((*optelemptr[curchainID]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
	    endif 
	    if ((oldpos gt newpos) and (newpos gt 0)) then begin
	    ; that's basically the same as the case abovem except that the we don't need to move all the way to the end of the chain
		    print,'inner widget moved to the left ', eventx, (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, newpos, oldpos
		    ; next, we need to move the various pointers and reset the OMETdata.oenum value for all affected widgets
		    hold = optelemptr[curchainID]   ; store the current pointer in a holding variable
		    holdWID = OMETwidget_s.chainIDs[curchainID]
		    for i=oldpos,newpos+1,-1 do begin
		    	(*optelemptr[i-1]).oenum += 1
		    	optelemptr[i] = optelemptr[i-1]
		    	OMETwidget_s.chainIDs[i] = OMETwidget_s.chainIDs[i-1]
			    WIDGET_CONTROL, OMETwidget_s.chainIDs[i], TLB_SET_XOFFSET = (*optelemptr[i]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'6 moving widget '+wtype[(*optelemptr[i]).oetype]+':'+string(i,format="(I2)")+' to '+string((*optelemptr[i]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
		    endfor
		    optelemptr[newpos] = hold
		    (*optelemptr[newpos]).oenum = newpos
	    	OMETwidget_s.chainIDs[newpos] = holdWID
		    WIDGET_CONTROL, OMETwidget_s.chainIDs[newpos], TLB_SET_XOFFSET = newpos*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'7 moving widget '+wtype[(*optelemptr[newpos]).oetype]+string(newpos,format="(I2)")+' to '+string(newpos*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
	    endif
	    if (newpos eq 0) then begin
	    	Core_Print,'widgets can not be moved to the left of the input Stokes vector widget'
		    WIDGET_CONTROL, OMETwidget_s.chainIDs[curchainID], TLB_SET_XOFFSET = (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'8 moving widget '+wtype[(*optelemptr[curchainID]).oetype]+':'+string(curchainID,format="(I2)")+' to '+string((*optelemptr[curchainID]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
		endif
	    if (oldpos lt newpos) then begin
		    print,'inner widget moved to the right', eventx, (*optelemptr[curchainID]).oenum*OMETdata.xstepsize, newpos, oldpos
		    ; next, we need to move the various pointers and reset the OMETdata.oenum value for all affected widgets
		    hold = optelemptr[curchainID]   ; store the current pointer in a holding variable
		    holdWID = OMETwidget_s.chainIDs[curchainID]
		    for i=oldpos,newpos-1 do begin
		    	optelemptr[i] = optelemptr[i+1]
		    	(*optelemptr[i]).oenum -= 1
		    	OMETwidget_s.chainIDs[i] = OMETwidget_s.chainIDs[i+1]
			    WIDGET_CONTROL, OMETwidget_s.chainIDs[i], TLB_SET_XOFFSET = (*optelemptr[i]).oenum*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'9 moving widget '+wtype[(*optelemptr[i]).oetype]+':'+string(i,format="(I2)")+' to '+string((*optelemptr[i]).oenum*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
		    endfor
		    optelemptr[newpos] = hold
		    (*optelemptr[newpos]).oenum = newpos
	    	OMETwidget_s.chainIDs[newpos] = holdWID
		    WIDGET_CONTROL, OMETwidget_s.chainIDs[newpos], TLB_SET_XOFFSET = newpos*OMETdata.xstepsize, TLB_SET_YOFFSET = OMEtdata.ylocationwidgets
	    Core_Print,'10 moving widget '+wtype[(*optelemptr[newpos]).oetype]+':'+string(newpos,format="(I2)")+' to '+string(newpos*OMETdata.xstepsize,format="(I4)")+', '+string(OMEtdata.ylocationwidgets,format="(I4)")
	    endif

    endif
 endelse
endelse

if (OMETdata.eventverbose eq 1) then OMETdebug

Core_Print,' --- '
print,optelemptr

; make the event handler routines sensitive to widget movements again
OMETwidgetsensitivity, curchainID, /on

; once we've shuffled the widgets, we need to recompute the output Stokes vector
OMETupdateStokesVector

end
