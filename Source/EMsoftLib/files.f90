! ###################################################################
! Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

!--------------------------------------------------------------------------
! EMsoft:files.f90
!--------------------------------------------------------------------------
!
! MODULE: files
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief everything that has to do with file-based input-output
! 
!> @version
!
!> @date    1/ 5/99 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update after new cell type
!> @date   03/29/15 MDG 5.0 reformatted xtal files in HDF5 format; removed obsolete routines
!> @date   05/05/15 MDG 5.1 removed all getenv() calls; replaced with global path strings
!> @date   03/29/18 MDG 5.2 removed all stdout use
!--------------------------------------------------------------------------

module files

use local
use typedefs


interface Interpret_Program_Arguments
  module procedure Interpret_Program_Arguments_with_nml
  module procedure Interpret_Program_Arguments_no_nml
end interface

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: DumpXtalInfo
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Write a brief summary of the crystal structure on the screen
! 
!> @date    1/ 5/99 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update after new cell type
!> @date   06/06/14 MDG 4.1 added cell pointer as argument, corrected Message routine
!> @date   07/31/18 MDG 4.2 added a few more output parameters
!--------------------------------------------------------------------------
recursive subroutine DumpXtalInfo(cell)    
!DEC$ ATTRIBUTES DLLEXPORT :: DumpXtalInfo

use constants
use io
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell

integer(kind=irg)                       :: i, j, oi_int(3)
real(kind=dbl)                          :: oi_real(5)


 call Message('', frm = "(A/)")
 call Message('Crystal Structure Information', frm = "('-->',A,'<--')")
 oi_real(1) = cell%a
 call WriteValue('  a [nm]             : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%b
 call WriteValue('  b [nm]             : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%c
 call WriteValue('  c [nm]             : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%alpha
 call WriteValue('  alpha [deg]        : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%beta
 call WriteValue('  beta  [deg]        : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%gamma
 call WriteValue('  gamma [deg]        : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%vol
 call WriteValue('  Volume [nm^3]      : ', oi_real, 1, "(F12.8)")
 oi_int(1) = cell%SYM_SGnum
 call WriteValue('  Space group #      : ', oi_int, 1, "(1x,I3)")
 call WriteValue('  Space group symbol : ', trim(SYM_SGname(cell%SYM_SGnum)) )
 call WriteValue('  Generator String   : ',  trim(SYM_GL(cell%SYM_SGnum)) )
 if ((cell%SYM_SGset.eq.2).AND.(cell%xtal_system.ne.5)) then 
  call Message('   Using second origin setting', frm = "(A)")
 endif
 if ((cell%SYM_SGset.eq.2).AND.(cell%xtal_system.eq.5)) then 
  call Message('   Using rhombohedral parameters', frm = "(A)")
 endif
  if (cell%SG%SYM_centrosym) then 
    call Message('   Structure is centrosymmetric', frm = "(A)")
 else 
   call Message('   Structure is non-centrosymmetric', frm = "(A)")
 end if

! space group and point group information
 oi_int(1) = cell%SG%SYM_GENnum
 call WriteValue('  # generators       : ', oi_int, 1, "(1x,I3)")
 oi_int(1) = cell%SG%SYM_MATnum
 call WriteValue('  # symmetry matrices: ', oi_int, 1, "(1x,I3)")
 oi_int(1) = cell%SG%SYM_NUMpt
 call WriteValue('  # point sym. matr. : ', oi_int, 1, "(1x,I3)")

! generate atom positions and dump output  
 call Message('', frm = "(A/)")
 call CalcPositions(cell,'v')
 oi_int(1) = cell%ATOM_ntype
 call WriteValue('  Number of asymmetric atom positions ', oi_int, 1)
 do i=1,cell%ATOM_ntype
  oi_int(1:3) = (/i, cell%ATOM_type(i), cell%numat(i)/)
  call WriteValue('  General position / atomic number / multiplicity :', oi_int, 3,"(1x,I3,'/',I2,'/',I3)",advance="no")
  call Message(' ('//ATOM_sym(cell%ATOM_type(i))//')', frm = "(A)")
  call Message('   Equivalent positions  (x y z  occ  DWF) ', frm = "(A)")
  do j=1,cell%numat(i)
    oi_real(1:5) = (/cell%apos(i, j,1:3),dble(cell%ATOM_pos(i,4:5))/)
    call WriteValue('         > ', oi_real, 5,"(2x,4(F9.5,','),F9.5)")
  end do
end do
call Message('', frm = "(A/)")

end subroutine DumpXtalInfo

!--------------------------------------------------------------------------
!
! SUBROUTINE: ConvertWiki2PDF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @note programs that do not have a template file will have wiki codes starting at 900 
!
!> @param nt number of files to copy
!> @param wikilist integer array identifying the wiki file(s) to be converted to PDF by pandoc
! 
!> @date   09/08/19 MDG 1.0 first attempt
!--------------------------------------------------------------------------
recursive subroutine ConvertWiki2PDF(nt,wikilist)
!DEC$ ATTRIBUTES DLLEXPORT :: ConvertWiki2PDF

use io
use error 

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nt
integer(kind=irg),INTENT(IN)            :: wikilist(*)

logical                                 :: pandoc_found, fexist
integer(kind=irg)                       :: i, j, ios, nlines
integer(kind=irg),parameter             :: maxnumtemplates = 1024
character(fnlen)                        :: wikifiles(maxnumtemplates), wcf, tpl, input_name, output_name, wikipath, &
                                           pandoc_tpl, defcmd, line, cmd
character(3)                            :: wplextension = '.md'
character(4)                            :: pdfextension = '.pdf'


! first make sure that pandoc is available on this platform
! we'll ask for the version number, and if the returned output contains more than one line
! then the program is available; this will work on UNIX platforms but will need to be 
! modified for Windows.
pandoc_found = .FALSE. 
call system('pandoc -v | wc -l > linecount')
open(unit=dataunit,file='linecount',status='old',form='formatted')
read(dataunit,"(I10)") nlines
close(unit=dataunit,status='delete')
if (nlines.gt.1) pandoc_found=.TRUE.

if (pandoc_found.eqv..TRUE.) then 
! read the wikifile resources file to get all relevant file names 
  wcf = trim(EMsoft_getwikicodefilename())
  wcf = EMsoft_toNativePath(wcf)
  inquire(file=trim(wcf),exist=fexist)
  if (.not.fexist) then 
    call FatalError('ConvertWiki2PDF','wiki code file not found: '//wcf)
  end if

  open(UNIT=dataunit,FILE=trim(wcf), STATUS='old', FORM='formatted',ACCESS='sequential')

  wikifiles = ''
  do
   read(dataunit,'(I3.3,A)',iostat=ios) j, line
   if (ios.ne.0) then 
    exit
   end if
   wikifiles(j+1) = trim(line)
  end do
  CLOSE(UNIT=dataunit, STATUS='keep')

! get the correct path for the wiki files 
  wikipath = trim(EMsoft_toNativePath(EMsoft_getwikipathname()))

! then get the pandoc default.latex location (in the resources folder)
  pandoc_tpl = trim(EMsoft_getResourcepathname())//'default.latex'
  pandoc_tpl = EMsoft_toNativePath(pandoc_tpl)
  inquire(file=trim(pandoc_tpl),exist=fexist)

  if (fexist.eqv..TRUE.) then 
    defcmd = '-V fontsize=10pt --template '//trim(pandoc_tpl)
  else
    defcmd = ''
    call Message(' Warning: the pandoc default.latex template file could not be found; continuing... ')
  end if

! loop over all relevant wiki files and generate the corresponding PDF file
  do i=1,nt
    tpl = trim(wikifiles(wikilist(i)+1))
    input_name = trim(wikipath)//trim(tpl)//trim(wplextension)
    input_name = EMsoft_toNativePath(input_name)

    inquire(file=trim(input_name),exist=fexist)

    if (fexist.eqv..TRUE.) then  ! create a shell script that will call pandoc and generate the PDF file 
      output_name = trim(tpl)//pdfextension
! example command string:
!  pandoc -V fontsize=10pt --template EMsoftResourcesFolder/default.latex -s EMGBOdm.md -o EMGBOdm.pdf
      cmd = 'pandoc '//trim(defcmd)//' -s '//trim(input_name)//' -o '//trim(output_name)
      open(unit=dataunit,file='wiki2pdf',status='unknown',form='formatted')
      write(dataunit,"(A)") '#!/bin/bash'
      write(dataunit,"(A)") 'cdir=`pwd`'
      write(dataunit,"(A)") 'cd '//trim(wikipath)
      write(dataunit,"(A)") trim(cmd)
      write(dataunit,"(A)") 'mv '//trim(output_name)//' ${cdir}'
      write(dataunit,"(A)") 'cd ${cdir}'
      close(unit=dataunit, status='keep')
      call system('chmod +x wiki2pdf')
      call system('./wiki2pdf')
      open(unit=dataunit,file='wiki2pdf',status='unknown',form='formatted')
      close(unit=dataunit, status='delete')
      call Message(' wiki file converted to PDF: '//trim(output_name))
    else 
      call Message(' wiki file '//trim(input_name)//' not found; continuing ...')
    end if 
  end do
else 
  call FatalError('ConvertWiki2PDF',' pandoc program not found in search PATH')
end if 

end subroutine ConvertWiki2PDF


!--------------------------------------------------------------------------
!
! SUBROUTINE: CopyTemplateFiles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  copy template files into local folder
!
!> @details In the resources folder, there is a text file called templatecodes.txt
!> in which each template file is given a unique ID number.  The present routine
!> receives the requested numbers and then looks into the file to figure out 
!> which ones need to be copied.
!
!> @param nt number of files to copy
!> @param templatelist integer array identifying the template files to be copied
! 
!> @date   06/26/13 MDG 1.0 first attempt
!> @date   06/08/14 MDG 2.0 added stdout argument
!> @date   10/06/14 MDG 2.1 corrected off-by-one error in templatelist
!> @date   05/05/15 MDG 2.2 removed getenv() call; replaced by global path string
!> @date   08/19/16 MDG 2.3 added handling of separate NameListTemplate folders for developers
!> @date   05/11/17 MDG 3.0 added support for json template files
!> @date   03/29/18 MDG 3.1 removed stdout argument
!--------------------------------------------------------------------------
recursive subroutine CopyTemplateFiles(nt,templatelist,json)
!DEC$ ATTRIBUTES DLLEXPORT :: CopyTemplateFiles

use io
use error 

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nt
integer(kind=irg),INTENT(IN)            :: templatelist(*)
logical,INTENT(IN),OPTIONAL             :: json

integer(kind=irg),parameter             :: maxnumtemplates = 512
character(fnlen)                        :: templates(maxnumtemplates)
character(fnlen)                        :: input_name, output_name, tcf, tppath1, tppath2, tpl, tplextension
integer(kind=irg)                       :: ios, i, j, ipos
character(255)                          :: line
logical                                 :: fexist, develop

! first open and read the resources/templatecodes.txt file

tcf = trim(EMsoft_toNativePath(EMsoft_getTemplatecodefilename()))
inquire(file=trim(tcf),exist=fexist)
if (.not.fexist) then 
  call FatalError('CopyTemplateFiles','template code file not found:'//tcf)
end if

open(UNIT=dataunit,FILE=trim(tcf), STATUS='old', FORM='formatted',ACCESS='sequential')

templates = ''
do
 read(dataunit,'(I3.3,A)',iostat=ios) j, line
! write (*,*) j,' ->',trim(line),'<-'
 if (ios.ne.0) then 
  exit
 end if
 templates(j+1) = trim(line)
end do
CLOSE(UNIT=dataunit, STATUS='keep')

tplextension = '.template'
if (present(json)) then
  if (json.eqv..TRUE.) then 
    tppath1 = trim(EMsoft_getTemplatepathname(json))
    tplextension = '.jtemplate'
  else
    tppath1 = trim(EMsoft_getTemplatepathname())
  end if
else
  tppath1 = trim(EMsoft_getTemplatepathname())
end if

! then, determine whether or not the user is working in develop mode by checking for the 
! Develop keyword in the EMsoftconfig.json file... Regular users will only have a single
! NameListTemplates folder, but developers have two, so we need to make sure we check both
! locations.  The second location is the private folder...
develop = EMsoft_getEMdevelop()
tppath2 = ''
if (develop.eqv..TRUE.) then
  ipos = index(tppath1,'Public')
  do i=1,ipos-1
    tppath2(i:i) = tppath1(i:i)
  end do
  if (present(json)) then
    if (json.eqv..TRUE.) then 
      tcf = 'Private/JSONTemplates/'
    else
      tcf = 'Private/NamelistTemplates/'
    end if
  else
    tcf = 'Private/NamelistTemplates/'
  endif
  do i=ipos,ipos+26
    j = i-ipos+1
    tppath2(i:i) = tcf(j:j)
  end do
end if

do i=1,nt
 if (templatelist(i).lt.900) then  ! exclude programs that haev a wiki file but no template file
   tpl = trim(templates(templatelist(i)+1))
   input_name = trim(tppath1)//trim(tpl)//trim(tplextension)
   input_name = EMsoft_toNativePath(input_name)

   inquire(file=trim(input_name),exist=fexist)
   if (.not.fexist) then 
    if (develop.eqv..TRUE.) then
     input_name = trim(tppath2)//trim(tpl)//trim(tplextension)
     input_name = EMsoft_toNativePath(input_name)
     inquire(file=trim(input_name),exist=fexist)
     if (.not.fexist) then 
       call FatalError('CopyTemplateFiles','template file '//trim(templates(templatelist(i)+1))//trim(tplextension)// &
                      ' not found in either template folder')
     end if
    else
     call FatalError('CopyTemplateFiles','template file '//trim(templates(templatelist(i)+1))//trim(tplextension)//' not found')
    end if
   end if
   output_name = trim(templates(templatelist(i)+1))//trim(tplextension)
   output_name = EMsoft_toNativePath(output_name)
   open(UNIT=dataunit,FILE=trim(input_name), STATUS='old', FORM='formatted',ACCESS='sequential')
   open(UNIT=dataunit2,FILE=trim(output_name), STATUS='unknown', FORM='formatted',ACCESS='sequential')
   do
          read(dataunit,'(A)',iostat=ios) line
          if (ios.ne.0) then 
            exit
          end if
          write(dataunit2,'(A)') trim(line)
    end do
   close(UNIT=dataunit, STATUS='keep')
   close(UNIT=dataunit2, STATUS='keep')
   call Message('  -> created template file '//trim(templates(templatelist(i)+1))//trim(tplextension), frm = "(A)")
 end if
end do
 
end subroutine CopyTemplateFiles

!--------------------------------------------------------------------------
!
! SUBROUTINE: Interpret_Program_Arguments_with_nml
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  interpret the command line arguments
!
!> @details In the resources folder, there is a text file called templatecodes.txt
!> in which each template file is given a unique ID number.  The present routine
!> receives the requested numbers and then looks into the file to figure out 
!> which ones need to be copied.
!
!> @param nmldefault default nml file name on input, actual file name on output
!> @param numt number of files to (potentially) copy
!> @param templatelist integer array identifying the template files to be copied
! 
!> @date   06/26/13 MDG 1.0 first attempt (this might belong elsewhere...)
!> @date   09/04/13 MDG 1.1 minor modifications to arguments
!> @date   06/08/14 MDG 2.0 added stdout argument
!> @date   05/11/17 MDG 3.0 added support for JSON-formatted template files
!> @date   03/29/18 MDG 3.1 removed stdout argument
!> @date   09/08/19 MDG 4.0 add support for automatic pandoc wiki->pdf conversion
!--------------------------------------------------------------------------
recursive subroutine Interpret_Program_Arguments_with_nml(nmldefault,numt,templatelist,progname)
!DEC$ ATTRIBUTES DLLEXPORT :: Interpret_Program_Arguments_with_nml

use io

IMPLICIT NONE

character(fnlen),INTENT(INOUT)          :: nmldefault
!f2py intent(in,out) ::  nmldefault
integer(kind=irg),INTENT(IN)            :: numt
integer(kind=irg),INTENT(IN)            :: templatelist(*)
character(fnlen),INTENT(IN)             :: progname

integer(kind=irg)                       :: numarg       !< number of command line arguments
integer(kind=irg)                       :: iargc        !< external function for command line
character(fnlen)                        :: arg          !< to be read from the command line
character(fnlen)                        :: nmlfile      !< nml file name
integer(kind=irg)                       :: i, io_int(1)
logical                                 :: haltprogram, json

json = .FALSE.

!numarg = iargc()
numarg = command_argument_count()
nmlfile = ''
nmlfile = trim(nmldefault)

if (numarg.gt.0) then
  io_int(1) = numarg
  call WriteValue('Number of command line arguments detected: ',io_int,1)
end if

haltprogram = .FALSE.
if (numarg.ge.1) haltprogram = .TRUE.

if (numarg.gt.0) then ! there is at least one argument
  do i=1,numarg
    call getarg(i,arg)
!    mess = 'Found the following argument: '//trim(arg); call Message("(/A/)")
! does the argument start with a '-' character?    
    if (arg(1:1).eq.'-') then
        if (trim(arg).eq.'-h') then
         call Message(' Program should be called as follows: ', frm = "(/A)")
         call Message('        '//trim(progname)//' -h -t -j [nmlfile]', frm = "(A)")
         call Message(' where nmlfile is an optional file name for the namelist file;', frm = "(A/)")
         call Message(' If absent, the default name '''//trim(nmldefault)//''' will be used.', frm = "(A)")
         call Message(' To create templates of all possible input files, type '//trim(progname)//' -t', frm = "(A)")
         call Message(' To produce this message, type '//trim(progname)//' -h', frm = "(A)")
         call Message(' All program arguments can be combined in the same order;  ', frm = "(A)")
         call Message(' the argument without - will be interpreted as the input file name.', frm = "(A/)")
        end if
        if (trim(arg).eq.'-t') then
! with this option the program creates template namelist files in the current folder so that the 
! user can edit them (file extension will be .template; should be changed by user to .nml)
                call Message('Creating program name list template files:', frm = "(/A)")
                call CopyTemplateFiles(numt,templatelist)
        end if
        if (trim(arg).eq.'-pdf') then 
! if the pandoc program is installed (for instance within the anaconda distribution)
! then the user can ask for the wiki manual page (if it exists) to be converted into a pdf
! file that will be placed in the current folder.
!
! example command to generate the pdf file for the EMGBOdm program:
!   pandoc -V fontsize=10pt --template ~/templates/default.latex -s EMGBOdm.md -o EMGBOdm.pdf
!
! We use the same template codes but now they are linked to the corresponding wiki file
! which should be located in a folder at the same level as the resources folder.
!
          call Message(' User requested wiki-to-pdf conversion', frm = "(/A)")
          call ConvertWiki2PDF(numt, templatelist)
        end if 
        if (trim(arg).eq.'-j') then
          json = .TRUE.
! with this option the program creates template JSON files in the current folder so that the 
! user can edit them (file extension will be .jsontemplate; should be changed by user to .json)
!
! It should be noted that the template files contain comment lines starting with the "!" character;
! this is not standard JSON (which does not allow for comment lines).  The EMsoft JSON files will 
! first be filtered to remove all the comment lines before being passed to the json parser routine.
                call Message('Creating program JSON template files:', frm = "(/A)")
                call CopyTemplateFiles(numt,templatelist,json=json)
        end if
    else
! no, the first character is not '-', so this argument must be the filename
! If it is present, but any of the other arguments were present as well, then
! we stop the program. 
        nmlfile = arg
        if (numarg.eq.1) haltprogram = .FALSE.
    end if
  end do
end if

if (haltprogram) then
  call Message('To execute program, remove all flags except for nml/json input file name', frm = "(/A/)")
  stop
end if

nmldefault = nmlfile

end subroutine Interpret_Program_Arguments_with_nml

!--------------------------------------------------------------------------
!
! SUBROUTINE: Interpret_Program_Arguments_no_nml
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  interpret the command line arguments
!
!> @details This routine assumes that there is no template file, but there 
!> could still be a wiki file... 
!
!> @param numt number of files to (potentially) copy
!> @param templatelist integer array identifying the template files to be copied
! 
!> @date   06/26/13 MDG 1.0 first attempt (this might belong elsewhere...)
!> @date   09/04/13 MDG 1.1 minor modifications to arguments
!> @date   06/08/14 MDG 2.0 added stdout argument
!> @date   05/11/17 MDG 3.0 added support for JSON-formatted template files
!> @date   03/29/18 MDG 3.1 removed stdout argument
!> @date   09/08/19 MDG 4.0 add support for automatic pandoc wiki->pdf conversion
!--------------------------------------------------------------------------
recursive subroutine Interpret_Program_Arguments_no_nml(numt,templatelist,progname,flagset)
!DEC$ ATTRIBUTES DLLEXPORT :: Interpret_Program_Arguments_no_nml

use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: numt
integer(kind=irg),INTENT(IN)            :: templatelist(*)
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(INOUT),OPTIONAL :: flagset  

integer(kind=irg)                       :: numarg       !< number of command line arguments
integer(kind=irg)                       :: iargc        !< external function for command line
character(fnlen)                        :: arg          !< to be read from the command line
character(fnlen)                        :: nmlfile      !< nml file name
integer(kind=irg)                       :: i, io_int(1)
logical                                 :: haltprogram, json, flags

json = .FALSE.

flags = .FALSE. 
if (present(flagset)) then
  flags = .TRUE. 
end if

!numarg = iargc()
numarg = command_argument_count()

if (numarg.gt.0) then
  io_int(1) = numarg
  call WriteValue('Number of command line arguments detected: ',io_int,1)
end if

haltprogram = .FALSE.
if (numarg.ge.1) haltprogram = .TRUE.

if (numarg.gt.0) then ! there is at least one argument
  do i=1,numarg
    call getarg(i,arg)
! does the argument start with a '-' character?    
    if (arg(1:1).eq.'-') then
        if (trim(arg).eq.'-h') then
         call Message(' Program should be called as follows: ', frm = "(/A)")
         call Message('        '//trim(progname)//' -h -pdf ', frm = "(A)")
         call Message(' To produce this message, type '//trim(progname)//' -h', frm = "(A)")
         call Message(' use -pdf to produce a PDF help file if the corresponding wiki file exists ', frm = "(A)")
        end if
        if (trim(arg).eq.'-pdf') then 
! if the pandoc program is installed (for instance within the anaconda distribution)
! then the user can ask for the wiki manual page (if it exists) to be converted into a pdf
! file that will be placed in the current folder.
!
! example command to generate the pdf file for the EMGBOdm program:
!   pandoc -V fontsize=10pt --template ~/templates/default.latex -s EMGBOdm.md -o EMGBOdm.pdf
!
! We use the same template codes but now they are linked to the corresponding wiki file
! which should be located in a folder at the same level as the resources folder.
!
          call Message(' User requested wiki-to-pdf conversion', frm = "(/A)")
          call ConvertWiki2PDF(numt, templatelist)
        end if 
        if (flags.eqv..TRUE.) then
          if (trim(arg).eq.trim(flagset)) then 
            flagset = 'yes'
            haltprogram = .FALSE.
          end if
        end if 
    else
! no, the first character is not '-', so this argument must be the filename
! If it is present, but any of the other arguments were present as well, then
! we stop the program. 
        nmlfile = arg
        if (numarg.eq.1) haltprogram = .FALSE.
    end if
  end do
end if

if (haltprogram) then
  call Message('To execute program, remove all flags ', frm = "(/A/)")
  stop
end if

end subroutine Interpret_Program_Arguments_no_nml




end module files
