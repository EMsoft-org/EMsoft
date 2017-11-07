
function replace_in_file,fname,searchstring

lrep = 0

s = searchstring+' ='

outname = 'tmp.txt'

line = ''

openr,1,fname
openw,2,outname
while not(eof(1)) do begin
; read a line
  readf,1,line
; search for the searchstring
  pos = strpos(line,s)
  if (pos ne -1) then begin
    items = strsplit(line,' = ',/extract)
    sz = size(items,/dimensions)
; look for a '(' character
    parens = strsplit(line,'(',/extract)
    sparens = size(parens,/dimensions)
    if ((sz[0] eq 2) and (items[0] eq searchstring) and (sparens[0] eq 1)) then begin
; look for a single or double quote
      pos = strpos(items[1],'''')
      if (pos ne -1) then begin
        items2 = strsplit(items[1],'''',/extract)
      end else begin
        pos = strpos(items[1],'"')
        if (pos ne -1) then items2 = strsplit(items[1],'"',/extract)
      endelse
      newline = items[0]+' = SC_'+items2[0]
      printf,2,newline
      lrep += 1
    end else begin
      printf,2,line
    endelse
  end else begin
    printf,2,line
  endelse
endwhile
close,2
close,1

cmd = 'mv '+outname+' '+fname
spawn,cmd

return, lrep
end 


pro replace,originalfolder

; this is a routine to replace all dataset and groupname variables in the entire EMsoft 
; source code package by string constants; we do this by first looking all occurrences
; of the 'dataset =' string and then we replace the string by a string constant starting
; with SC_
;

cmd = 'rm *.f90'
spawn,cmd

cmd = 'cp '+originalfolder+'/*.f90 .'
spawn,cmd

; first get all the *.f90 files
flist = file_search('*.f90')
sz = size(flist,/dimensions)
nfiles = sz[0]
print,'found ',nfiles,' f90 source code files'
print,'starting conversion of string constants'

; loop over all files
for i=0L,nfiles-1 do begin
; test each file to see if it has a 'dataset = ' entry 
  cmd = 'grep ''dataset ='' '+flist[i]
  spawn,cmd,res
  if (res[0] ne '') then begin  ; let's open the file and parse it
    lrep = replace_in_file(flist[i],'dataset')
    print,' --> replaced '+string(lrep,format="(I4)")+' dataset lines in file ',flist[i]
  end else begin
    print,'     nothing to replace in file ',flist[i]
  endelse
end

; loop over all files
print,' '
print,' replacing groupname strings'
print,' '
for i=0L,nfiles-1 do begin
; test each file to see if it has a 'dataset = ' entry 
  cmd = 'grep ''groupname ='' '+flist[i]
  spawn,cmd,res
  if (res[0] ne '') then begin  ; let's open the file and parse it
    lrep = replace_in_file(flist[i],'groupname')
    print,' --> replaced '+string(lrep,format="(I4)")+' groupname lines in file ',flist[i]
  end else begin
    print,'     nothing to replace in file ',flist[i]
  endelse
end



stop
end
