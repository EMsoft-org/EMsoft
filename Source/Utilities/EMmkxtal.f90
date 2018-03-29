! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMmkxtal.f90
!--------------------------------------------------------------------------
!
! PROGRAM: mkxtal 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a crystal structure file (very simple program)
!
!> @date   11/23/13 MDG 1.0 original
!> @date   08/10/14 MDG 1.1 added GenerateSymmetry call
!> @date   03/30/15 MDG 2.0 modified output to HDF format
!> @date   10/07/15 MDG 2.1 minor cleanup in preparation for release 3.0
!> @date   09/06/16 MDG 3.0 added Wyckoff position entry as an option
!--------------------------------------------------------------------------
program EMmkxtal

use local
use typedefs
use HDFsupport
use io
use crystal
use symmetry
use files

IMPLICIT NONE

type(unitcell), pointer         :: cell
character(fnlen)                :: progname, progdesc, fname
integer(kind=irg)               :: numarg, i
integer(kind=irg)               :: iargc        !< external function for command line
character(fnlen)                :: arg          !< to be read from the command line
logical                         :: useWyckoff

 progname = 'EMmkxtal.f90'
 progdesc = 'Create an HDF crystal structure file and place it in the XtalFolder'

 call EMsoft(progname, progdesc)
 
 useWyckoff = .FALSE.
 numarg = iargc()
 if (numarg.gt.0) then ! there is at least one argument
  do i=1,numarg
    call getarg(i,arg)
!    mess = 'Found the following argument: '//trim(arg); call Message("(/A/)")
! does the argument start with a '-' character?    
    if (arg(1:1).eq.'-') then
        if (trim(arg).eq.'-h') then
         call Message(' Program should be called as follows: ', frm = "(/A)")
         call Message('        '//trim(progname)//' [-h] [-w] ', frm = "(A)")
         call Message(' To produce this message, type '//trim(progname)//' -h', frm = "(A)")
         call Message(' To use Wyckoff positions to enter atom coordinates, use -w option', frm = "(A)")
        end if
        if (trim(arg).eq.'-w') then
! with this option the GetAsymPosWyckoff routine will ask the user for Wyckoff Positions instead of regular cordinate strings
         useWyckoff = .TRUE.
        end if
    end if
  end do
 end if

 allocate(cell)
 
 cell%SYM_SGset=0
 call GetLatParm(cell)
 call GetSpaceGroup(cell)
 call GenerateSymmetry(cell,.TRUE.)
 if (useWyckoff) then
  call GetAsymPosWyckoff(cell)
 else
  call GetAsymPos(cell)
 end if
 call ReadValue('Enter output file name (*.xtal) ', fname)
 cell%fname = fname
 call SaveDataHDF(cell)

end program EMmkxtal
