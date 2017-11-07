pro insertspace,fname
;
; insert a starting space in each line of the input file
; and add a comma at then end, except for the first and last line
;

cmd = 'mv '+fname+'.template '+fname+'.txt'
spawn,cmd

openr,1,fname+'.txt'
openw,2,fname+'.template'

line = ''
while not eof(1) do begin
  readf,1,line
  c = strmid(line,0,1)
  if (c ne '!') then begin
; first insert a space
    line = ' '+line
; if the first character is & or / then we're done
    if ((c ne '&') and (c ne '/')) then line = line+','
  end 
  printf,2,line
end
close,1
close,2

cmd = 'rm '+fname+'.txt'
spawn,cmd
end 


pro dospace,dummy

insertspace,'EMKosselmaster'
insertspace,'EMECPmaster'
insertspace,'EMECP'
insertspace,'EMEBSDmaster'
insertspace,'BetheParameters'
insertspace,'EMsampleRFZ'
insertspace,'EMfoil'
insertspace,'EMdefects'
insertspace,'EMdefectdata'
insertspace,'EMSTEMDCI'
insertspace,'EMPEDkin'
insertspace,'EMPEDZA'
insertspace,'EMMCLIPSS'
insertspace,'EMMC'
insertspace,'EMLACBED'
insertspace,'EMECPIndexing'
insertspace,'EMECCI'
insertspace,'EMEBSDcluster'
insertspace,'EMEBSDIndexing'
insertspace,'EMEBSD'
insertspace,'EMDPFit'
insertspace,'EMMCOpenCL'

end
