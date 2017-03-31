pro insert,fname
;
; insert a DLLEXPORT statement after every subroutine or function call
;
cmd = 'cp '+fname+'.f90 '+fname+'.txt'
spawn,cmd

line = ''
openr,1,fname+'.txt'
openw,2,fname+'.txt2'
imatch = 0
while not eof(1) do begin
  readf,1,line
  fmatch = stregex(line,'recursive function')
  smatch = stregex(line,'recursive subroutine')
  ssmatch = stregex(line,'subroutine')
  printf,2,line
  if ((fmatch ge 0) or (smatch ge 0) or (ssmatch eq 0)) then begin
    if (fmatch ge 0) then begin
      fpos = stregex(line,'function')
      ppos = stregex(line,'\(')
      name = strmid(line,fpos+9,ppos-fpos-9)
print,'found function ',name
      printf,2,'!DEC$ ATTRIBUTES DLLEXPORT :: '+name
      imatch += 1
    endif
    if (smatch ge 0) then begin
      fpos = stregex(line,'subroutine')
      ppos = stregex(line,'\(')
      name = strmid(line,fpos+11,ppos-fpos-11)
print,'found subroutine ',name
      printf,2,'!DEC$ ATTRIBUTES DLLEXPORT :: '+name
      imatch += 1
    endif
    if (ssmatch eq 0) then begin
      fpos = stregex(line,'subroutine')
      ppos = stregex(line,'\(')
      name = strmid(line,fpos+11,ppos-fpos-11)
print,'found subroutine ',name
      printf,2,'!DEC$ ATTRIBUTES DLLEXPORT :: '+name
      imatch += 1
    endif
  endif
end

close,1
close,2

cmd = 'mv '+fname+'.txt2 '+fname+'.f90'
spawn,cmd
cmd = 'rm '+fname+'.txt'
spawn,cmd

print,'Total number of inserts = ',imatch

end


pro doinsert,dummy

insert,'eispack'

stop
insert,'multibeams'
insert,'NameListJSONwriters'
insert,'noise'
insert,'others'
insert,'rng'
insert,'void'
insert,'typedefs'
insert,'timing'
insert,'tiff'
insert,'symmetry'
insert,'stacking_fault'
insert,'so3'
insert,'rotations'
insert,'quaternions'
insert,'postscript'
insert,'pgm'
insert,'math'
insert,'kvectors'
insert,'io'
insert,'initializers'
insert,'inclusion'
insert,'gvectors'
insert,'graphics'
insert,'foilmodule'
insert,'files'
insert,'error'
insert,'dislocation'
insert,'diffraction'
insert,'dictmod'
insert,'defectmodule'
insert,'crystal'
insert,'constants'
insert,'apb'
insert,'YSHmodule'
insert,'STEMmodule'
insert,'NameListTypedefs'
insert,'NameListHandlers'
insert,'NameListHDFwriters'
insert,'MBmodule'
insert,'Lambert'
insert,'JSONsupport'
insert,'HDFsupport'
insert,'EMdymod'
insert,'ECPmod'
insert,'EBSDmod'

end
