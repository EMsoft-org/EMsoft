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
! EMsoft:io.f90
!--------------------------------------------------------------------------
!
! MODULE: io
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief IO routines
!
!> @details  Several IO routines; the thinking is that this might be the only file that needs
!> to be rewritten if the package IO is modified.  In particular, in this version of the package,
!> all program input, with the exception of the mkxtal.f90 program and some other simple
!> programs, is done by means of namelist files.  This allows the entire package to be
!> controlled from inside the IDL environment or from the standard command line.
! 
!> @date 01/05/99 MDG 1.0 original
!> @date 05/19/01 MDG 2.0 f90 version
!> @date 03/19/13 MDG 3.0 major changes in IO routines (use of interface)
!> @date 05/16/13 MDG 3.1 added stdout as an option to run from IDL
!> @date 06/05/14 MDG 4.0 changed stdout to regular argument; removed global "mess" declaration
!--------------------------------------------------------------------------

module io

use local

public

interface ReadValue
        module procedure ReadValueIntShort
        module procedure ReadValueIntLong
        module procedure ReadValueRealSingle
        module procedure ReadValueRealDouble
        module procedure ReadValueString
        module procedure ReadValueStringArray
end interface ReadValue

interface WriteValue
        module procedure WriteValueIntShort
        module procedure WriteValueIntLong
        module procedure WriteValueIntLongLong
        module procedure WriteValueRealSingle
        module procedure WriteValueRealDouble
        module procedure WriteValueRealComplex
        module procedure WriteValueString
end interface WriteValue

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: Message
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief dump a message to standard output
!
!> @details Simple routine to print a string on the standard output, with optional formatting
!> instructions, for instance if one wants an empty line before (frm='(/A)') or after (frm='(A/)') 
!> the string.  Note that one can include the name of the optional variable in the subroutine
!> call, as in:
!> call Message('this is a string', frm='(//A//)' , stdout = 22)
!> this makes it clear that frm and stdout are optional variables.
! 
!> @param mess message string
!> @param frm optional string formatting command
!> @param stdout optional output unit identifier
!
!> @date 01/05/99 MDG 1.0 original
!> @date 05/19/01 MDG 2.0 f90 version
!> @date 03/19/13 MDG 3.0 made argument optional and introduced default format
!> @date 06/05/14 MDG 4.0 added stdout and mess as mandatory arguments
!--------------------------------------------------------------------------
recursive subroutine Message(mess,frm,stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: Message

character(*),INTENT(IN)                 :: mess         !< message string
character(*),OPTIONAL,INTENT(IN)        :: frm          !< optional formatting string
integer(kind=irg),OPTIONAL,INTENT(IN)   :: stdout       !< optional output unit identifier

integer(kind=irg)                       :: std

std = 6
if (PRESENT(stdout)) std = stdout

! default format or not ?
if (PRESENT(frm)) then
 write (std,fmt=frm) trim(mess)
else    ! default output format: a simple string
 write (std,fmt="(A)") trim(mess)
end if 

end subroutine Message

! ###################################################################
! reading routines
! ###################################################################


! ###################################################################
! 
!  subroutine ReadValueString   
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a string from standard input (unit = 5)
!
!> @param Qstring question string
!> @param rd_string string to be read
!> @param frm optional format string
!> @param stdout optional output unit identifier

!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine ReadValueString( Qstring, rd_string, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadValueString

character(*),INTENT(IN)                         :: Qstring
character(*),INTENT(OUT)                        :: rd_string
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),OPTIONAL,INTENT(IN)           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

call Message(Qstring, frm = "(' ',A,' ',$)", stdout = std)

if (PRESENT(frm)) then
  read (5, fmt=frm) rd_string
else
  read (5,*) rd_string
end if

end subroutine ReadValueString

! ###################################################################
! 
!  subroutine ReadValueStringArray   
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read an array of strings from standard input (unit = 5)
!
!> @param Qstring question string
!> @param rd_string string to be read
!> @param num number of strings in array
!> @param frm optional format string
!> @param stdout optional output unit identifier

!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine ReadValueStringArray(Qstring, rd_string, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadValueStringArray

character(*),INTENT(IN)                         :: Qstring
character(1),INTENT(OUT)                        :: rd_string(num)
integer(kind=irg),INTENT(IN)                    :: num
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),OPTIONAL,INTENT(IN)           :: stdout

integer(kind=irg)                               :: std, i

std = 6
if (PRESENT(stdout)) std = stdout

call Message(Qstring, frm = "(' ',A,' ',$)", stdout = std)

if (PRESENT(frm)) then 
  do i=1,num
    read (5, fmt=frm) rd_string(i)
  end do
else  
  do i=1,num
    read (5,*) rd_string(i)
  end do
end if 

end subroutine ReadValueStringArray

! ###################################################################
! 
!  subroutine ReadValueIntShort   
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read one or more short integers
!
!> @param Qstring question string
!> @param rd_int integer to be read
!> @param num optional number of integers to be read
!> @param stdout optional output unit identifier

!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine ReadValueIntShort(Qstring, rd_int, num, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadValueIntShort

character(*), INTENT(IN)                        :: Qstring
integer(kind=ish),INTENT(OUT)                   :: rd_int(*)
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),OPTIONAL,INTENT(IN)           :: stdout

integer(kind=irg)                               :: std, i

std = 6
if (PRESENT(stdout)) std = stdout

call Message(Qstring, frm = "(' ',A,' ',$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
  read (5,*) (rd_int(i),i=1,num)
else
  read (5,*) rd_int(1)
end if
  
end subroutine ReadValueIntShort

! ###################################################################
! 
!  subroutine ReadValueIntLong 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read one or more regular (4-byte) integers
!
!> @param Qstring question string
!> @param rd_int integer to be read
!> @param num optional number of integers to be read
!> @param stdout optional output unit identifier

!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine ReadValueIntLong(Qstring, rd_int, num, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadValueIntLong

character(*), INTENT(IN)                        :: Qstring
integer(kind=irg),INTENT(OUT)                   :: rd_int(*)
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),OPTIONAL,INTENT(IN)           :: stdout

integer(kind=irg)                               :: std, i

std = 6
if (PRESENT(stdout)) std = stdout

call Message(Qstring, frm = "(' ',A,' ',$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
  read (5,*) (rd_int(i),i=1,num)
else
  read (5,*) rd_int(1)
end if
  
end subroutine ReadValueIntLong

! ###################################################################
! 
!  subroutine ReadValueRealSingle 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read one or more regular (4-byte) reals
!
!> @param Qstring question string
!> @param rd_real integer to be read
!> @param num optional number of integers to be read
!> @param stdout optional output unit identifier

!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine ReadValueRealSingle(Qstring, rd_real, num, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadValueRealSingle

character(*), INTENT(IN)                        :: Qstring
real(kind=sgl),INTENT(OUT)                      :: rd_real(*)
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),OPTIONAL,INTENT(IN)           :: stdout

integer(kind=irg)                               :: std, i

std = 6
if (PRESENT(stdout)) std = stdout

call Message(Qstring, frm = "(' ',A,' ',$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
  read (5,*) (rd_real(i),i=1,num)
else
  read (5,*) rd_real(1)
end if

end subroutine ReadValueRealSingle

! ###################################################################
! 
!  subroutine ReadValueRealDouble 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read one or more regular (4-byte) reals
!
!> @param Qstring question string
!> @param rd_real integer to be read
!> @param num optional number of integers to be read
!> @param stdout optional output unit identifier

!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine ReadValueRealDouble(Qstring, rd_real, num, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadValueRealDouble

character(*), INTENT(IN)                        :: Qstring
real(kind=dbl),INTENT(OUT)                      :: rd_real(*)
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),OPTIONAL,INTENT(IN)           :: stdout

integer(kind=irg)                               :: std, i

std = 6
if (PRESENT(stdout)) std = stdout

call Message(Qstring, frm = "(' ',A,' ',$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
  read (5,*) (rd_real(i),i=1,num)
else
  read (5,*) rd_real(1)
end if

end subroutine ReadValueRealDouble


! ###################################################################
! writing routines
! ###################################################################

! ###################################################################
! 
!  subroutine WriteValueString 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a string
!
!> @param Qstring question string
!> @param out_string output string
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueString(Qstring, out_string, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueString

character(*),INTENT(IN)                         :: Qstring 
character(*),INTENT(IN)                         :: out_string
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

! send Qstring to the output only if it is non-zero length
if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)


if (PRESENT(frm)) then 
  call Message(out_string, frm = frm, stdout = std)
else
 call Message(out_string, frm = "(A)", stdout = std)
end if

end subroutine WriteValueString

! ###################################################################
! 
!  subroutine WriteValueIntShort 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write one or more short integers
!
!> @param Qstring question string
!> @param out_int output string
!> @param num optional number of integers
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueIntShort(Qstring, out_int, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueIntShort

character(*), INTENT(IN)                        :: Qstring
integer(kind=ish),INTENT(IN)                    :: out_int(*)
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
 if (PRESENT(frm)) then
  write (std, fmt=frm) (out_int(i),i=1,num)
 else
  write (std,*) (out_int(i),i=1,num)
 end if
else
 if (PRESENT(frm)) then
  write (std, fmt=frm) out_int(1)
 else
  write (std,*) out_int(1)
 end if
end if

end subroutine WriteValueIntShort

! ###################################################################
! 
!  subroutine WriteValueIntLong 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write one or more 4-byte integers
!
!> @param Qstring question string
!> @param out_int output string
!> @param num optional number of integers
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueIntLong(Qstring, out_int, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueIntLong

character(*), INTENT(IN)                        :: Qstring
integer(kind=irg),INTENT(IN)                    :: out_int(*)
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
 if (PRESENT(frm)) then
  write (std, fmt=frm) (out_int(i),i=1,num)
 else
  write (std,*) (out_int(i),i=1,num)
 end if
else
 if (PRESENT(frm)) then
  write (std, fmt=frm) out_int(1)
 else
  write (std,*) out_int(1)
 end if
end if

end subroutine WriteValueIntLong

! ###################################################################
! 
!  subroutine WriteValueIntLongLong
!
!> @author Saransh, Carnegie Mellon University
!
!> @brief write one or more 8-byte integers
!
!> @param Qstring question string
!> @param out_int output string
!> @param num optional number of integers
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueIntLongLong(Qstring, out_int, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueIntLongLong

character(*), INTENT(IN)                        :: Qstring
integer(kind=ill),INTENT(IN)                    :: out_int(*)
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
 if (PRESENT(frm)) then
  write (std, fmt=frm) (out_int(i),i=1,num)
 else
  write (std,*) (out_int(i),i=1,num)
 end if
else
 if (PRESENT(frm)) then
  write (std, fmt=frm) out_int(1)
 else
  write (std,*) out_int(1)
 end if
end if

end subroutine WriteValueIntLongLong


! ###################################################################
! 
!  subroutine WriteValueRealSingle 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write one or more single precision reals
!
!> @param Qstring question string
!> @param out_real output string
!> @param num optional number of integers
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueRealSingle(Qstring, out_real, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueRealSingle

character(*), INTENT(IN)                        :: Qstring
real(kind=sgl),INTENT(IN)                       :: out_real(*)
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
 if (PRESENT(frm)) then
  write (std, fmt=frm) (out_real(i),i=1,num)
 else
  write (std,*) (out_real(i),i=1,num)
 end if
else
 if (PRESENT(frm)) then
  write (std, fmt=frm) out_real(1)
 else
  write (std,*) out_real(1)
 end if
end if

end subroutine WriteValueRealSingle



! ###################################################################
! 
!  subroutine WriteValueRealDouble 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write one or more double precision reals
!
!> @param Qstring question string
!> @param out_real output string
!> @param num optional number of integers
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueRealDouble(Qstring, out_real, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueRealDouble

character(*), INTENT(IN)                        :: Qstring
real(kind=dbl),INTENT(IN)                       :: out_real(*)
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
 if (PRESENT(frm)) then
  write (std, fmt=frm) (out_real(i),i=1,num)
 else
  write (std,*) (out_real(i),i=1,num)
 end if
else
 if (PRESENT(frm)) then
  write (std, fmt=frm) out_real(1)
 else
  write (std,*) out_real(1)
 end if
end if

end subroutine WriteValueRealDouble


! ###################################################################
! 
!  subroutine WriteValueRealComplex 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write one or more single precision complex numbers
!
!> @param Qstring question string
!> @param out_real output string
!> @param num optional number of integers
!> @param frm optional formatting argument
!> @param stdout optional output unit identifier
!
!> @date 03/19/13 MDG 1.0 new routine
!> @date 06/05/14 MDG 2.0 changed io handling
! ###################################################################
recursive subroutine WriteValueRealComplex(Qstring, out_cmplx, num, frm, stdout)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteValueRealComplex

character(*), INTENT(IN)                        :: Qstring
complex(kind=sgl),INTENT(IN)                    :: out_cmplx(*)
character(*),INTENT(IN),OPTIONAL                :: frm
integer(kind=irg),INTENT(IN),OPTIONAL           :: num
integer(kind=irg),INTENT(IN),OPTIONAL           :: stdout

integer(kind=irg)                               :: std

std = 6
if (PRESENT(stdout)) std = stdout

if (len(Qstring).ne.0) call Message(Qstring, frm = "(A$)", stdout = std)

! one or more than one values expected ?
if (PRESENT(num)) then
 if (PRESENT(frm)) then
  write (std, fmt=frm) (out_cmplx(i),i=1,num)
 else
  write (std,*) (out_cmplx(i),i=1,num)
 end if
else
 if (PRESENT(frm)) then
  write (std, fmt=frm) out_cmplx(1)
 else
  write (std,*) out_cmplx(1)
 end if
end if

end subroutine WriteValueRealComplex




recursive subroutine PrintMatrixd(s,a)
!DEC$ ATTRIBUTES DLLEXPORT :: PrintMatrixd

use local

IMPLICIT NONE

real(kind=dbl)   :: a(3,3)
integer(kind=irg):: i,j
character(4)     :: s

write (*,"(A/)") s
do i=1,3
  write (*,"(3(F12.5,2x))") (a(i,j),j=1,3)
end do
write (*,"(/)")

end subroutine

recursive subroutine PrintMatrixcd(s,a)
!DEC$ ATTRIBUTES DLLEXPORT :: PrintMatrixcd

use local

IMPLICIT NONE

complex(kind=dbl)   :: a(3,3)
integer(kind=irg):: i,j
character(4)     :: s

write (*,"(A/)") s
do i=1,3
  write (*,*) (a(i,j),j=1,3)
end do
write (*,"(/)")

end subroutine
! 



end module io
