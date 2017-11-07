pro addentries,infile,outfile

openr,1,infile
openw,2,outfile

line = ''

while not(eof(1)) do begin
  readf,1,line
; h5copy;"/bin/h5copy"
  outline = strtrim(line,2)+';"'+strtrim(line,2)+'"'
  printf,2,outline  
endwhile

close,2
close,1

stop
end
