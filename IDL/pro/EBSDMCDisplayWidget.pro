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
; EMsoft:EBSDMCDisplayWidget.pro
;--------------------------------------------------------------------------
;
; PROGRAM: EBSDMCDisplayWidget.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Display widget for Monte Carlo simulation results
;
;> @date 03/19/14 MDG 1.0 first version
;--------------------------------------------------------------------------
pro EBSDMCDisplayWidget,dummy
;
;------------------------------------------------------------
; common blocks
common SEM_widget_common, SEMwidget_s
common SEM_data_common, SEMdata

; the next common block contains all the raw data needed to generate the EBSD patterns
common EBSD_rawdata, accum_e, accum_z, mLPNH, mLPSH


ATOMsym=[' H','He','Li','Be',' B',' C',' N',' O',' F','Ne', $
         'Na','Mg','Al','Si',' P',' S','Cl','Ar',' K','Ca', $
         'Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu','Zn', $
         'Ga','Ge','As','Se','Br','Kr','Rb','Sr',' Y','Zr', $
         'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn', $
         'Sb','Te',' I','Xe','Cs','Ba','La','Ce','Pr','Nd', $
         'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb', $
         'Lu','Hf','Ta',' W','Re','Os','Ir','Pt','Au','Hg', $
         'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th', $
         'Pa',' U','Np','Pu','Am','Cm','Bk','Cf']


;------------------------------------------------------------
; make sure that this program isn't already running
if (XRegistered("EBSDMCDisplayWidget") NE 0) then begin
  print,'EBSDMCDisplayWidget is already running ... (if it is not, please restart your IDL session)'
  return
end

;------------------------------------------------------------
; create the top level widget
;if (SEMdata.MCMPboth eq 0) then begin
;  SEMwidget_s.MCdisplaybase = WIDGET_BASE(TITLE='Monte Carlo Display Widget', $
;                        /COLUMN, $
;                        XSIZE=20+max([ 500, 2*SEMdata.mcimx+1 ]), $
;                        /ALIGN_CENTER, $
;			MBAR = menubar, $
;			/TLB_MOVE_EVENTS, $
;			EVENT_PRO='EBSDMCDisplayWidget_event', $
;                        XOFFSET=SEMdata.MCxlocation, $
;                        YOFFSET=SEMdata.MCylocation)
;end else begin
if (SEMdata.mpfiletype lt 3) then begin
  SEMwidget_s.MCdisplaybase = WIDGET_BASE(TITLE='Master Pattern & Monte Carlo Display Widget', $
                        /COLUMN, $
                        XSIZE=20+max([ 500, 2*SEMdata.mcimx+1 ]) + max( [ 500, 2*SEMdata.mpimx+1 ] ), $
                        /ALIGN_CENTER, $
			MBAR = menubar, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='EBSDMCDisplayWidget_event', $
                        XOFFSET=SEMdata.MCxlocation, $
                        YOFFSET=SEMdata.MCylocation)
end else begin
  SEMwidget_s.MCdisplaybase = WIDGET_BASE(TITLE='Kossel Master Pattern Display Widget', $
                        /COLUMN, $
                        XSIZE=20+ max( [ 500, 2*SEMdata.mpimx+1 ] ), $
                        /ALIGN_CENTER, $
			MBAR = menubar, $
			/TLB_MOVE_EVENTS, $
			EVENT_PRO='EBSDMCDisplayWidget_event', $
                        XOFFSET=SEMdata.MCxlocation, $
                        YOFFSET=SEMdata.MCylocation)
end

menu1 = WIDGET_BUTTON(menubar, VALUE='Projection Mode', /MENU)
b1 = WIDGET_BUTTON(menu1, VALUE='Lambert [square]', UVALUE='LAMBERTS')
b2 = WIDGET_BUTTON(menu1, VALUE='Lambert [circle]', UVALUE='LAMBERTC')
b3 = WIDGET_BUTTON(menu1, VALUE='Stereographic P.', UVALUE='STEREOP')

; we only have a Display Mode menu item for the EBSD mode
if (SEMdata.mpfiletype eq 1) then begin
  menu2 = WIDGET_BUTTON(menubar, VALUE='Display Mode', /MENU)
; if (SEMdata.MCMPboth eq 1) then begin
    b2 = WIDGET_BUTTON(menu2, VALUE='Individual Energy/Master Bin', UVALUE='DISPEBIN')
    b2 = WIDGET_BUTTON(menu2, VALUE='Simple Energy Sum/Weighted Master', UVALUE='DISPESUM')
    b2 = WIDGET_BUTTON(menu2, VALUE='Simple Energy Sum RGB/blank', UVALUE='DISPESUMRGB')
; end else begin
;   b2 = WIDGET_BUTTON(menu2, VALUE='Individual Energy Bin', UVALUE='DISPEBIN')
;   b2 = WIDGET_BUTTON(menu2, VALUE='Simple Energy Sum', UVALUE='DISPESUM')
;   b2 = WIDGET_BUTTON(menu2, VALUE='Simple Energy Sum RGB', UVALUE='DISPESUMRGB')
; endelse
endif

;------------------------------------------------------------
;if (SEMdata.MCMPboth eq 0) then begin
;	block1 = WIDGET_BASE(SEMwidget_s.MCdisplaybase, $
;			/FRAME, $
;                        XSIZE=max([ 500, 2*SEMdata.mcimx+1 ]) , $
;			/ALIGN_CENTER, $
;			/COLUMN)
;end else begin
if (SEMdata.mpfiletype lt 3) then begin
	block1 = WIDGET_BASE(SEMwidget_s.MCdisplaybase, $
			/FRAME, $
                        XSIZE=max([ 500, 2*SEMdata.mcimx+1 ]) + max( [ 500, 2*SEMdata.mpimx+1 ] ), $
			/ALIGN_CENTER, $
			/COLUMN)
end else begin
	block1 = WIDGET_BASE(SEMwidget_s.MCdisplaybase, $
			/FRAME, $
                        XSIZE= max( [ 500, 2*SEMdata.mpimx+1 ] ), $
			/ALIGN_CENTER, $
			/COLUMN)
end
;endelse

block2 = WIDGET_BASE(block1, /ROW, /ALIGN_CENTER)

; here's a slider to select the energy window ...
if (SEMdata.mpfiletype eq 1) then begin
  SEMwidget_s.MCslider = WIDGET_SLIDER(block2, $
			EVENT_PRO='EBSDMCDisplayWidget_event', $
			MINIMUM = 1, $
			MAXIMUM = SEMdata.mcenergynumbin, $
			SENSITIVE = 1, $
			TITLE = 'Select an energy', $
			XSIZE = 400, $
			VALUE = 1, $
			UVALUE = 'MCSLIDER', $
			/ALIGN_CENTER)
end 
if (SEMdata.mpfiletype eq 2) then begin
  SEMwidget_s.MCslider = WIDGET_SLIDER(block2, $
			EVENT_PRO='EBSDMCDisplayWidget_event', $
			MINIMUM = 1, $
			MAXIMUM = SEMdata.mcenergynumbin, $
			SENSITIVE = 1, $
			TITLE = 'Select a beam tilt angle', $
			XSIZE = 400, $
			VALUE = 1, $
			UVALUE = 'MCSLIDER', $
			/ALIGN_CENTER)
end
if (SEMdata.mpfiletype eq 3) then begin
  SEMwidget_s.MCslider = WIDGET_SLIDER(block2, $
			EVENT_PRO='EBSDMCDisplayWidget_event', $
			MINIMUM = 1, $
			MAXIMUM = SEMdata.mcenergynumbin, $
			SENSITIVE = 1, $
			TITLE = 'Select a sample depth', $
			XSIZE = 400, $
			VALUE = 1, $
			UVALUE = 'MCSLIDER', $
			/ALIGN_CENTER)
end

; and right next to it we display the actual energy or beam tilt angle or sample depth in a text box
if (SEMdata.mpfiletype eq 1) then begin
  energy = SEMdata.mcenergymin + SEMdata.Esel * SEMdata.mcenergybinsize
  SEMwidget_s.MCenergyval =  WIDGET_TEXT(block2, $
			VALUE=string(energy,format="(F5.2)"), $
			XSIZE=10, $
			YSIZE=1, $
			/ALIGN_RIGHT)
end

if (SEMdata.mpfiletype eq 2) then begin
  angle = SEMdata.mcsigstart + SEMdata.Esel * SEMdata.mcsigstep
  SEMwidget_s.MCenergyval =  WIDGET_TEXT(block2, $
			VALUE=string(angle,format="(F5.2)"), $
			XSIZE=10, $
			YSIZE=1, $
			/ALIGN_RIGHT)
end

if (SEMdata.mpfiletype eq 3) then begin
  depth = SEMdata.mcdepthmax + SEMdata.Esel * SEMdata.mcdepthstep
  SEMwidget_s.MCenergyval =  WIDGET_TEXT(block2, $
			VALUE=string(depth,format="(F5.2)"), $
			XSIZE=10, $
			YSIZE=1, $
			/ALIGN_RIGHT)
end


if (SEMdata.MCMPboth eq 1) then begin
; in the same block we also generate a list of all the asymmetric unit positions
; along with a SUM option to display the total EBSD pattern
vals = strarr(SEMdata.numset+1)
vals[0] = 'SUM all sites'
for i=1,SEMdata.numset do vals[i] = string(i,format="(I3)")+' '+ATOMsym[SEMdata.atnum[i-1]-1]+' ('+string(SEMdata.atnum[i-1]^2,format="(I4)")+')'

SEMwidget_s.asymunit = WIDGET_DROPLIST(block2, $
			EVENT_PRO='EBSDMCDisplayWidget_event', $
			VALUE=vals,$
			/FRAME, $
			UVALUE='ASYMPOS', $
			/ALIGN_LEFT)

WIDGET_CONTROL, set_droplist_select=0, SEMwidget_s.asymunit
endif

;------------------------------------------------------------
block2 = WIDGET_BASE(block1, /ROW, /ALIGN_CENTER)
block3 = WIDGET_BASE(block2, /COLUMN, /ALIGN_CENTER)

if (SEMdata.mpfiletype lt 3) then begin
; and here's the MC display window itself
  SEMwidget_s.MCdraw = WIDGET_DRAW(block3, $
                        COLOR_MODEL=2, $
                        RETAIN=2, $
                        /FRAME, $
                        XSIZE=2*SEMdata.mcimx+1, $
                        YSIZE=2*SEMdata.mcimy+1)

; and the min-max indicators
  block4 = WIDGET_BASE(block3, /ROW, /ALIGN_CENTER)
  SEMwidget_s.MCmin = Core_WText(block4, 'min/max ',fontstr, 75, 25, 15, 1, string(SEMdata.MCmin,FORMAT="(F9.1)"))
  SEMwidget_s.MCmax = Core_WText(block4, '/',fontstr, 5, 25, 15, 1, string(SEMdata.MCmax,FORMAT="(F9.1)"))

; and a save button
  saveEBSDMC = WIDGET_BUTTON(block4, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='EBSDMCDisplayWidget_event', $
                        /FRAME, $
                        UVALUE='SAVEEBSDMC', $
                        /ALIGN_LEFT)

;------------------------------------------------------------
; and the MP window
  block3 = WIDGET_BASE(block2, /COLUMN, /ALIGN_CENTER)
  SEMwidget_s.MPdraw = WIDGET_DRAW(block3, $
                        COLOR_MODEL=2, $
                        RETAIN=2, $
                        /FRAME, $
                        XSIZE=2*SEMdata.mpimx+1, $
                        YSIZE=2*SEMdata.mpimy+1)

; and the min-max indicators
  block4 = WIDGET_BASE(block3, /ROW, /ALIGN_CENTER)
  SEMwidget_s.MPmin = Core_WText(block4, 'min/max ',fontstr, 75, 25, 15, 1, string(SEMdata.MPmin,FORMAT="(F9.1)"))
  SEMwidget_s.MPmax = Core_WText(block4, '/',fontstr, 5, 25, 15, 1, string(SEMdata.MPmax,FORMAT="(F9.1)"))

; and a save button
  saveEBSDMP = WIDGET_BUTTON(block4, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='EBSDMCDisplayWidget_event', $
                        /FRAME, $
                        UVALUE='SAVEEBSDMP', $
                        /ALIGN_LEFT)
end else begin
; the Kossel MP window
  block3 = WIDGET_BASE(block2, /COLUMN, /ALIGN_CENTER)
  SEMwidget_s.MPdraw = WIDGET_DRAW(block3, $
                        COLOR_MODEL=2, $
                        RETAIN=2, $
                        /FRAME, $
                        XSIZE=2*SEMdata.mpimx+1, $
                        YSIZE=2*SEMdata.mpimy+1)

; and the min-max indicators
  block4 = WIDGET_BASE(block3, /ROW, /ALIGN_CENTER)
  SEMwidget_s.MPmin = Core_WText(block4, 'min/max ',fontstr, 75, 25, 15, 1, string(SEMdata.MPmin,FORMAT="(F9.1)"))
  SEMwidget_s.MPmax = Core_WText(block4, '/',fontstr, 5, 25, 15, 1, string(SEMdata.MPmax,FORMAT="(F9.1)"))

; and a save button
  saveEBSDMP = WIDGET_BUTTON(block4, $
                        VALUE='Save', $
                        /NO_RELEASE, $
                        EVENT_PRO='EBSDMCDisplayWidget_event', $
                        /FRAME, $
                        UVALUE='SAVEEBSDMP', $
                        /ALIGN_LEFT)

end


;------------------------------------------------------------
; and finally a close button
file1 = WIDGET_BASE(block1, $
			XSIZE=500, $
			/FRAME, $
			/ROW)

file2 = WIDGET_BUTTON(file1, $
                        VALUE='Close', $
                        UVALUE='CLOSEMC', $
                        EVENT_PRO='EBSDMCDisplayWidget_event', $
                        SENSITIVE=1, $
                        /FRAME)

vals = ['jpeg','tiff','bmp']
SEMwidget_s.EBSDformatbgroup = CW_BGROUP(file1, $
                        vals, $
                        /ROW, $
                        /NO_RELEASE, $
                        /EXCLUSIVE, $
                        FONT=fontstrlarge, $
                        LABEL_LEFT = 'File Format', $
                        /FRAME, $
                        EVENT_FUNC ='EBSDevent', $
                        UVALUE='EBSDFORMAT', $
                        SET_VALUE=SEMdata.imageformat)

;------------------------------------------------------------
; realize the widget structure
WIDGET_CONTROL,SEMwidget_s.MCdisplaybase,/REALIZE

; realize the draw widget
if (SEMdata.mpfiletype lt 3) then begin
  WIDGET_CONTROL, SEMwidget_s.MCdraw, GET_VALUE=drawID
  SEMwidget_s.MCdrawID = drawID
  WIDGET_CONTROL, SEMwidget_s.MPdraw, GET_VALUE=drawID
  SEMwidget_s.MPdrawID = drawID
end else begin
  WIDGET_CONTROL, SEMwidget_s.MPdraw, GET_VALUE=drawID
  SEMwidget_s.MPdrawID = drawID
end

SEMdata.MCLSum = 0
EBSDshowMC

; and hand over control to the xmanager
XMANAGER,"EBSDMCDisplayWidget",SEMwidget_s.MCdisplaybase,/NO_BLOCK

end 

