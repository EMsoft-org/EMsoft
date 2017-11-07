
pro CTEM_define_idlvars,verbose=verbose
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
	  print,'No root folder selection made; Exiting program'
	  return
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

if keyword_set(verbose) then begin
  print,'The following IDL system variables were defined: '
  print,'    !CTEM_root 		',!CTEM_root
  print,'    !CTEM_idl  		',!CTEM_idl 
  print,'    !CTEM_res  		',!CTEM_res 
  print,'    !CTEM_log  		',!CTEM_log 
  print,'    !CTEM_results	',!CTEM_results
end

end

