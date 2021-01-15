! ###################################################################
! Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMxtalExtract.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMmkxtalExtract 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extract information from a crystal structure file and put in txt file for input redirect
!
!> @date   07/19/18 MDG 1.0 original
!--------------------------------------------------------------------------
program EMxtalExtract

use local
use typedefs
use HDFsupport
use io
use crystal
use symmetry
use files

IMPLICIT NONE

type(unitcell)                  :: cell
character(fnlen)                :: progname, progdesc, fname, txtname
integer(kind=irg)               :: numarg, i, io_int(3)
real(kind=dbl)                  :: io_real(5)
integer(kind=irg)               :: iargc        !< external function for command line
character(fnlen)                :: arg          !< to be read from the command line

!> numbers of the space groups with two settings
integer(kind=irg),parameter     :: tworig(24)=(/48,50,59,68,70,85,86,88,125,126,129,130,133,134,137,138,&
                                                141,142,201,203,222,224,227,228/)


 !nullify(cell)        
 !allocate(cell)        

 progname = 'EMxtalExtract.f90'
 progdesc = 'Extract all information from an HDF crystal structure file and dump it in regular or Wyckoff form to a text file'

! deal with the command line arguments, if any
 call Interpret_Program_Arguments(1,(/ 926 /), progname)

 call EMsoft(progname, progdesc)
 
 numarg = iargc()
 if (numarg.gt.0) then ! there is at least one argument
  do i=1,numarg
    call getarg(i,arg)
! does the argument start with a '-' character?    
    if (arg(1:1).eq.'-') then
        if (trim(arg).eq.'-h') then
         call Message(' Program should be called as follows: ', frm = "(/A)")
         call Message('        '//trim(progname)//' [-h] ', frm = "(A)")
         call Message(' To produce this message, type '//trim(progname)//' -h', frm = "(A)")
        end if
    end if
  end do
 end if

 call ReadValue('Enter input xtal file name (*.xtal) ', fname)
 cell%fname = trim(fname)
 call CrystalData(cell,verbose=.TRUE.)
 call CalcPositions(cell,'v')
 
 call ReadValue('Enter text file name for output (*.txt) ', txtname)
 open (unit=dataunit, file=trim(txtname), status='unknown',form='formatted')

! 1. write the crystal system
 if (cell%SG%SYM_trigonal.eqv..TRUE.) then
    write(dataunit,"(I1)") 5
    if (cell%xtal_system.eq.4) then
      write(dataunit,"(I1)") 1
    else
      write(dataunit,"(I1)") 0
    end if
 else
    write(dataunit,"(I1)") cell%xtal_system
 end if

! 2. lattice parameters (only the ones that are needed)
! a is always needed
 write(dataunit,"(F10.5)") cell%a
 select case (cell%xtal_system)
    case (1)
  ! tetragonal
    case (2)
     write(dataunit,"(F10.5)") cell%c
  ! orthorhombic
    case (3)
     write(dataunit,"(F10.5)") cell%b
     write(dataunit,"(F10.5)") cell%c
  ! hexagonal
    case (4)
     write(dataunit,"(F10.5)") cell%c
  ! rhombohedral 
    case (5)
     write(dataunit,"(F10.5)") cell%alpha
  ! monoclinic   
    case (6)
     write(dataunit,"(F10.5)") cell%b
     write(dataunit,"(F10.5)") cell%c
     write(dataunit,"(F10.5)") cell%beta
  ! triclinic    
    case (7) 
     write(dataunit,"(F10.5)") cell%b
     write(dataunit,"(F10.5)") cell%c
     write(dataunit,"(F10.5)") cell%alpha
     write(dataunit,"(F10.5)") cell%beta
     write(dataunit,"(F10.5)") cell%gamma
 end select

! 3. space group number
 write(dataunit,"(I3)") cell%SYM_SGnum

! 4. some space groups have a second setting
 if (minval(tworig-cell%SYM_SGnum).eq.0) then 
   write(dataunit,"(I3)") cell%SYM_SGset
 end if

! 5. the atom coordinates, site occupations, and Debye-Waller factors are output here...
 do i=1,cell%ATOM_ntype
   if (i.gt.1) write(dataunit,"('y')")
   write(dataunit,"(I2)") cell%ATOM_type(i)
   write(dataunit,"(4(F10.5,','),F10.5)") cell%ATOM_pos(i,1:5)
 end do
 write(dataunit,"('n')")

! 6. xtal file name
 write(dataunit,"(A)") trim(cell%fname)

! 7. source, if defined
 if (trim(cell%source).eq.'undefined') then
   write(dataunit,"(A)") ''''''
   call Message('')
   call Message('=========================')
   call Message('The current version of the structure file has an empty Source field.')
   call Message('Please edit the text file and add a citation describing where the structure data comes from (max 512 characters).')
   call Message('=========================')
   call Message('')
 else
   write(dataunit,"('''',A,'''')") trim(cell%source)
 end if 

 close(unit=dataunit,status='keep')

 call Message('')
 call Message('The text output file '//trim(fname)//' has been created.')
 call Message('You can edit this file with a text editor to make corrections,')
 call Message('or to add a Source string on the last line (if not already present) between single quotes') 
 call Message('Then, use the following command to update the actual structure file:')
 call Message('')
 call Message('EMmkxtal < '//trim(txtname))
 call Message('')

end program EMxtalExtract
