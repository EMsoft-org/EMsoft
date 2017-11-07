! ###################################################################
! Copyright (c) 2014, Marc De Graef/Carnegie Mellon University
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
!--------------------------------------------------------------------------

module files

use local
use typedefs

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
!--------------------------------------------------------------------------
recursive subroutine DumpXtalInfo(cell, stdout)    
!DEC$ ATTRIBUTES DLLEXPORT :: DumpXtalInfo

use constants
use io
use symmetry

IMPLICIT NONE

type(unitcell), pointer                 :: cell
integer(kind=irg),INTENT(IN),OPTIONAL   :: stdout

integer(kind=irg)                       :: i, j, oi_int(3), std
real(kind=dbl)                          :: oi_real(5)

 std = 6
 if (PRESENT(stdout)) std = stdout

 call Message('', frm = "(A/)", stdout = std)
 call Message('Crystal Structure Information', frm = "('-->',A,'<--')", stdout = std)
 oi_real(1) = cell%a
 call WriteValue('  a [nm]             : ', oi_real, 1, "(F9.5)", stdout = std)
 oi_real(1) = cell%b
 call WriteValue('  b [nm]             : ', oi_real, 1, "(F9.5)", stdout = std)
 oi_real(1) = cell%c
 call WriteValue('  c [nm]             : ', oi_real, 1, "(F9.5)", stdout = std)
 oi_real(1) = cell%alpha
 call WriteValue('  alpha [deg]        : ', oi_real, 1, "(F9.5)", stdout = std)
 oi_real(1) = cell%beta
 call WriteValue('  beta  [deg]        : ', oi_real, 1, "(F9.5)", stdout = std)
 oi_real(1) = cell%gamma
 call WriteValue('  gamma [deg]        : ', oi_real, 1, "(F9.5)", stdout = std)
 oi_real(1) = cell%vol
 call WriteValue('  Volume [nm^3]      : ', oi_real, 1, "(F12.8)", stdout = std)
 oi_int(1) = cell%SYM_SGnum
 call WriteValue('  Space group #      : ', oi_int, 1, "(1x,I3)", stdout = std)
 call WriteValue('  Space group symbol : ', trim(SYM_SGname(cell%SYM_SGnum)) , stdout = std)
 call WriteValue('  Generator String   : ',  trim(SYM_GL(cell%SYM_SGnum)) , stdout = std)
 if ((cell%SYM_SGset.eq.2).AND.(cell%xtal_system.ne.5)) then 
  call Message('   Using second origin setting', frm = "(A)", stdout = std)
 endif
 if ((cell%SYM_SGset.eq.2).AND.(cell%xtal_system.eq.5)) then 
  call Message('   Using rhombohedral parameters', frm = "(A)", stdout = std)
 endif
  if (cell%SG%SYM_centrosym) then 
    call Message('   Structure is centrosymmetric', frm = "(A)", stdout = std)
 else 
   call Message('   Structure is non-centrosymmetric', frm = "(A)", stdout = std)
 end if
! generate atom positions and dump output  
 call Message('', frm = "(A/)", stdout = std)
 call CalcPositions(cell,'v')
 oi_int(1) = cell%ATOM_ntype
 call WriteValue('  Number of asymmetric atom positions ', oi_int, 1, stdout = std)
 do i=1,cell%ATOM_ntype
  oi_int(1:3) = (/i, cell%ATOM_type(i), cell%numat(i)/)
  call WriteValue('  General position / atomic number / multiplicity :', oi_int, 3,"(1x,I3,'/',I2,'/',I3,$)", stdout = std)
  call Message(' ('//ATOM_sym(cell%ATOM_type(i))//')', frm = "(A)", stdout = std)
  call Message('   Equivalent positions  (x y z  occ  DWF) ', frm = "(A)", stdout = std)
  do j=1,cell%numat(i)
    oi_real(1:5) = (/cell%apos(i, j,1:3),dble(cell%ATOM_pos(i,4:5))/)
    call WriteValue('         > ', oi_real, 5,"(2x,4(F9.5,','),F9.5)", stdout = std)
  end do
end do
call Message('', frm = "(A/)", stdout = std)

end subroutine DumpXtalInfo



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
!--------------------------------------------------------------------------
recursive subroutine CopyTemplateFiles(nt,templatelist,stdout,json)
!DEC$ ATTRIBUTES DLLEXPORT :: CopyTemplateFiles

use io
use error 

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nt
integer(kind=irg),INTENT(IN)            :: templatelist(*)
integer(kind=irg),INTENT(IN),OPTIONAL   :: stdout
logical,INTENT(IN),OPTIONAL             :: json

integer(kind=irg),parameter     :: maxnumtemplates = 256
character(fnlen)                :: templates(maxnumtemplates)
character(fnlen)                :: input_name, output_name, tcf, tppath1, tppath2, tpl, tplextension
integer(kind=irg)               :: ios, i, j, std, ipos
character(255)                  :: line
logical                         :: fexist, develop

std = 6
if (PRESENT(stdout)) std = stdout

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
 call Message('  -> created template file '//trim(templates(templatelist(i)+1))//trim(tplextension), frm = "(A)", stdout = std)
end do
 
end subroutine CopyTemplateFiles

!--------------------------------------------------------------------------
!
! SUBROUTINE: Interpret_Program_Arguments
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
!--------------------------------------------------------------------------
recursive subroutine Interpret_Program_Arguments(nmldefault,numt,templatelist,progname,stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: Interpret_Program_Arguments

use io

IMPLICIT NONE

character(fnlen),INTENT(INOUT)          :: nmldefault
integer(kind=irg),INTENT(IN)            :: numt
integer(kind=irg),INTENT(IN)            :: templatelist(*)
character(fnlen),INTENT(IN)             :: progname
integer(kind=irg),INTENT(IN),OPTIONAL   :: stdout

integer(kind=irg)                       :: numarg       !< number of command line arguments
integer(kind=irg)                       :: iargc        !< external function for command line
character(fnlen)                        :: arg          !< to be read from the command line
character(fnlen)                        :: nmlfile      !< nml file name
integer(kind=irg)                       :: i, std, io_int(1)
logical                                 :: haltprogram, json

json = .FALSE.
std = 6
if (PRESENT(stdout)) std = stdout

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
         call Message(' Program should be called as follows: ', frm = "(/A)", stdout = std)
         call Message('        '//trim(progname)//' -h -t -j [nmlfile]', frm = "(A)", stdout = std)
         call Message(' where nmlfile is an optional file name for the namelist file;', frm = "(A/)", stdout = std)
         call Message(' If absent, the default name '''//trim(nmldefault)//''' will be used.', frm = "(A)", stdout = std)
         call Message(' To create templates of all possible input files, type '//trim(progname)//' -t', frm = "(A)", stdout = std)
         call Message(' To produce this message, type '//trim(progname)//' -h', frm = "(A)", stdout = std)
         call Message(' All program arguments can be combined in the same order;  ', frm = "(A)", stdout = std)
         call Message(' the argument without - will be interpreted as the input file name.', frm = "(A/)", stdout = std)
        end if
        if (trim(arg).eq.'-t') then
! with this option the program creates template namelist files in the current folder so that the 
! user can edit them (file extension will be .template; should be changed by user to .nml)
                call Message('Creating program name list template files:', frm = "(/A)", stdout = std)
                call CopyTemplateFiles(numt,templatelist)
        end if
        if (trim(arg).eq.'-j') then
          json = .TRUE.
! with this option the program creates template JSON files in the current folder so that the 
! user can edit them (file extension will be .jsontemplate; should be changed by user to .json)
!
! It should be noted that the template files contain comment lines starting with the "!" character;
! this is not standard JSON (which does not allow for comment lines).  The EMsoft JSON files will 
! first be filtered to remove all the comment lines before being passed to the json parser routine.
                call Message('Creating program JSON template files:', frm = "(/A)", stdout = std)
                call CopyTemplateFiles(numt,templatelist,json=json)
        end if
    else
! no, the first character is not '-', so this argument must be the filename
! if it is present, but any of the other arguments were present as well, then
! we stop the program. 
        nmlfile = arg
        if (numarg.eq.1) haltprogram = .FALSE.
    end if
  end do
end if

if (haltprogram) then
  call Message('To execute program, remove all flags except for nml/json input file name', frm = "(/A/)", stdout = std)
  stop
end if

nmldefault = nmlfile

end subroutine Interpret_Program_Arguments




end module files
