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
; CTEMsoft2013:STEMfilehandling.pro
;--------------------------------------------------------------------------
;
; PROGRAM: STEMfilehandling.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief Basic file handling routines
;
;> @date 06/13/13 MDG 1.0 first attempt 
;--------------------------------------------------------------------------
pro STEMfilehandling,dummy
;
; define a number of new IDL system variables 
; to facilitate the main program navigation.
;

; if the file ~/Library/Preferences/com.CTEMgui.prefs does not
; exist, ask the user to set the default CTEMroot folder
; and create the SYSVARS.txt file, so that it will be found
; the next time.

prefname = '~/Library/Preferences/com.CTEMgui.prefs'
rs = file_test(prefname)

if (rs eq 1) then begin
  s = ''
  openr,1,prefname
  readf,1,s
  close,1
  CTEMroot = strtrim(s,2)
end else begin
; prefs file does not exist yet, so let's figure out where the 
; root folder is by asking the user, and then we create the prefs file
  cd,current = s
  rootpath = ''
  res=dialog_pickfile(title='Select CTEMsoft top folder',path=rootpath,get_path=s,/directory)
  if (res eq '') then begin
	  STEMprint,'No root folder selection made'
	  goto, skip
  end
  CTEMroot = strtrim(res,2)
; and create the file
  openw,1,prefname
  printf,1,CTEMroot
  close,1
endelse

defsysv,'!CTEM_root',exist=exist
if (exist eq 1) then begin $
 !CTEM_root = CTEMroot
end else begin $
 defsysv,'!CTEM_root',CTEMroot
end 

; relative to the root, we know where all other folders are ...

; main IDL folder
defsysv,'!CTEM_idl',exist=exist
if (exist eq 1) then begin $
 !CTEM_idl = CTEMroot+'IDL/'
end else begin $
 defsysv,'!CTEM_idl',CTEMroot+'IDL/'
end 

; IDL resources folder
defsysv,'!CTEM_res',exist=exist
if (exist eq 1) then begin $
 !CTEM_res = CTEMroot+'IDL/Resources/'
end else begin $
 defsysv,'!CTEM_res',CTEMroot+'IDL/Resources/'
end 

; logs folder
defsysv,'!CTEM_log',exist=exist
if (exist eq 1) then begin $
 !CTEM_log = CTEMroot+'IDL/Logs/'
end else begin $
 defsysv,'!CTEM_log',CTEMroot+'IDL/Logs/'
end 

; Results folder
defsysv,'!CTEM_results',exist=exist
if (exist eq 1) then begin $
 !CTEM_results = CTEMroot+'IDL/Results/'
end else begin $
 defsysv,'!CTEM_results',CTEMroot+'IDL/Results/'
end 

  STEMprint,'The following IDL system variables were defined: '
  STEMprint,'    !CTEM_root 		'+!CTEM_root
  STEMprint,'    !CTEM_idl  		'+!CTEM_idl 
  STEMprint,'    !CTEM_res  		'+!CTEM_res 
  STEMprint,'    !CTEM_log  		'+!CTEM_log 
  STEMprint,'    !CTEM_results	'+!CTEM_results

skip:
end

