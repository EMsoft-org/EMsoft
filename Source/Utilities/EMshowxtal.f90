! ###################################################################
! Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMshowxtal.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMshowxtal 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Display crystal structure information
!
!> @date   07/31/18 MDG 1.0 original
!> @date   12/23/18 MDG 1.1 minor reorganization
!--------------------------------------------------------------------------
program EMshowxtal

use local
use typedefs
use io
use HDFsupport
use crystal
use files
use diffraction
use symmetry
use constants
use postscript

IMPLICIT NONE

character(fnlen)               :: progname, progdesc, gname
type(unitcell)                 :: cell
logical                        :: verbose=.TRUE.
integer(kind=irg)			   :: i, j
character(1)                   :: yesno

 progname = 'EMshowxtal.f90'
 progdesc = 'Display crystal structure information'
 call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
 call Interpret_Program_Arguments(1,(/ 921 /), progname)
 
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = trim(gname)
 call CrystalData(cell,verbose)

 call Message('Do you want to print the symmetry matrices as well ? (y/n) ')
 read (5,"(A1)") yesno

 if (yesno.eq.'y') then
    call Message('Space group operators (last column = translation)')
     do i=1,cell%SG%SYM_MATnum 
       write (*,*) i,':'
       write (*,*) (cell%SG%SYM_data(i,1,j),j=1,4)
       write (*,*) (cell%SG%SYM_data(i,2,j),j=1,4)
       write (*,*) (cell%SG%SYM_data(i,3,j),j=1,4)
       write (*,*) ' '
     end do

     call Message('Point group operators')
     do i=1,cell%SG%SYM_NUMpt 
       write (*,*) i,':'
       write (*,*) (cell%SG%SYM_direc(i,1,j),j=1,3)
       write (*,*) (cell%SG%SYM_direc(i,2,j),j=1,3)
       write (*,*) (cell%SG%SYM_direc(i,3,j),j=1,3)
       write (*,*) ' '
     end do
 endif    

end program EMshowxtal
