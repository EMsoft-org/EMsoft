!###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:HDFsupport.f90
!--------------------------------------------------------------------------
!
! MODULE: HDFsupport
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief HDF5 helper routines
!
!> @details: the EM HDF format has four Groups : 
!>
!> EMheader, created with HDF_writeEMheader
!> 
!> EMNMLfiles, which contains verbatim all the nml files as string arrays
!>
!> EMNMLparameters contains the parsed nml files, one parameter at a time
!>
!> and finally the EMData section, which has all the program output
!>
!
!> @date  03/17/15 MDG 1.0 original
!> @date  03/27/15 MDG 1.1 added integer and real write routines
!> @date  03/29/15 MDG 1.2 removed all h5lt routines
!> @date  03/31/15 MDG 1.3 added support for arrays of c_chars
!> @date  04/07/15 MDG 1.4 added hyperslab routines for char, integer, float and double in 2, 3, and 4D
!> @date  04/08/15 MDG 1.5 removed HDF_tail pointer as it was no longer needed
!> @date  04/08/15 MDG 1.6 added optional overwrite keyword to all HDF_writeDataset... routines
!> @date  01/06/15 MDG 1.7 changed order of array declarations so that dimensions are declared first (needed for Intel Fortran compiler)
!> @date  10/31/16 MDG 1.8 added C_NULL_CHAR to every string that is an argument of a core HDF5 routine (via cstringify function)
!> @date  11/01/16 MDG 1.9 fixed dimensional error in hyperslab read routines
!> @date  12/14/16 MDG 2.0 added logical switch to flag DREAM.3D-generated files which require different string handling (fixed length vs variable length)
!> @date  12/16/16 MDG 3.0 completely reworked HDF error handling; introduced h5open_EMsoft to initialize the fortran HDF interface
!> @date  08/30/19 MDG 4.0 modified HDF_head definition for python f90wrap compatibility
!> @date  09/30/19 MAJ 4.1 initial mods of allocations that caused Mac OSX/ifort issues in write routines 
!> @date  10/01/19 MDG 4.2 additional mods to make ifort work on Mac OS X 
!> @date  11/08/19 MDG 4.3 replaced individual dims parameters by single dims array in multiple routines
!--------------------------------------------------------------------------
module HDFsupport

use local
use typedefs
use HDF5
use stringconstants

! THIS COMMENT NO LONGER APPLIES; KEPT FOR HISTORICAL REASONS
!--------------------------------------------------------------------------
!- declared here to allow for a split of the EMSoftLib into 2 dylibs-------
!- this is needed to avoid a conflict between the HDF5 dylibs needed by ---
!- DREAM.3D and the static HDF5 libraries used by EMSoft.               ---
!--------------------------------------------------------------------------

! COMMENT ADDED FOR 5.0 EMsoft release
! ====================================
! With the addition of python f90wrap support, the use of the HDF_head variable 
! had to be changed slightly. Up to this point, HDF_head was always defined as a 
! pointer linked list.  Pointers are not allowed as function/subroutine arguments
! in the f90wrap implementation, so we change the top HDF_head variable to be 
! a regular variable of type HDFobjectStackType, and it will always exist as 
! soon as it is defined.  The top level is simply not used, ever, other than to
! point to the real top level via HDF_head%next.  
!
! This means that the occurence of the following line:
! type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
! must be replaced EVERYWHERE by 
! type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
! and any command
! nullify(HDF_head)
! must be replaced by 
! nullify(HDF_head%next)
!


! type definition for HDF-based output
type HDFobjectStackType   ! this is a push-pop stack to keep track of the open objects
  character(LEN=1)                      :: objectType
  character(fnlen)                      :: objectName
  integer(HID_T)                        :: objectID
  type(HDFobjectStackType),pointer      :: next
end type HDFobjectStackType

logical, private, save                  :: FixedLengthflag

private :: HDF_readfromTextfile, HDF_push !, HDF_stackdump

contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: h5open_EMsoft
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the HDF interface and turn standard error reporting off
!
!> @param hdferr error variable
!
!> @date 12/16/16  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine h5open_EMsoft(hdferr)
!DEC$ ATTRIBUTES DLLEXPORT :: h5open_EMsoft

use local

IMPLICIT NONE

integer(kind=irg),INTENT(INOUT) :: hdferr
!f2py intent(in,out) ::  hdferr
integer(kind=irg)               :: printonoff

if (EMsoft_getEMsoftHDFtest().eqv..TRUE.) write (*,*) '>>>>>>>>>>>>>>>  OPENING HDF INTERFACE !!!!!!!!!'
! open the HDF fortran interface
call h5open_f(hdferr)
call HDFerror_check('h5open_EMsoft:h5open_f', hdferr)

! and turn standard error reporting off; we'll handle errors our way...
printonoff = 0
call h5eset_auto_f(printonoff,hdferr)
call HDFerror_check('h5open_EMsoft:h5eset_auto_f', hdferr)

end subroutine h5open_EMsoft

!--------------------------------------------------------------------------
!
! SUBROUTINE: h5close_EMsoft
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief close the HDF interface and turn standard error reporting back on
!
!> @param hdferr error variable
!
!> @date 12/16/16  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine h5close_EMsoft(hdferr)
!DEC$ ATTRIBUTES DLLEXPORT :: h5close_EMsoft

use local

IMPLICIT NONE

integer(kind=irg),INTENT(INOUT) :: hdferr
!f2py intent(in,out) ::  hdferr

if (EMsoft_getEMsoftHDFtest().eqv..TRUE.) write (*,*) '>>>>>>>>>>>>>>>  CLOSING HDF INTERFACE !!!!!!!!!'

! turn standard error reporting on
!call h5eset_auto_f(1,hdferr)
!call HDFerror_check('h5close_EMsoft:h5eset_auto_f', hdferr)

! close the HDF fortran interface
call h5close_f(hdferr)
call HDFerror_check('h5close_EMsoft:h5close_f', hdferr)

end subroutine h5close_EMsoft

!--------------------------------------------------------------------------
!
! function: cstringify
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief turn a fortran string into a null-terminated c-string
!
!> @param strin input fortran string character(len=###)
!> @param cstrput output null-terminated C-string character(len=###,kind=c_char)
!
!> @date 10/31/16  MDG 1.0 original
!--------------------------------------------------------------------------
pure recursive function cstringify(strin) result(cstrout)
!DEC$ ATTRIBUTES DLLEXPORT :: cstringify

use local
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                  :: strin
character(len=len_trim(strin)+1,kind=c_char) :: cstrout

integer(kind=irg)                            :: slen, i

slen = len_trim(strin)
do i=1,slen
  cstrout(i:i) = strin(i:i)
end do
slen = slen+1
cstrout(slen:slen) = C_NULL_CHAR 

end function cstringify

!--------------------------------------------------------------------------
!
! function: carstringify
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief turn a fortran string into a null-terminated c-string character array
!
!> @param strin input fortran string character(len=###)
!> @param cstrput output null-terminated C-string character(kind=c_char)
!
!> @date 02/09/20  MDG 1.0 original
!--------------------------------------------------------------------------
pure recursive function carstringify(strin) result(cstrout)
!DEC$ ATTRIBUTES DLLEXPORT :: carstringify

use local
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: strin
character(kind=c_char)                      :: cstrout(len_trim(strin)+1)

integer(kind=irg)                           :: slen, i

slen = len_trim(strin)
do i=1,slen
  cstrout(i) = strin(i:i)
end do
slen = slen+1
cstrout(slen) = C_NULL_CHAR 

end function carstringify

!--------------------------------------------------------------------------
!
! function: fstringify
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief turn a null-terminated c-string into a fortran string
!
!> @param strin input null-terminated C-string character array (kind=c_char) :: strin(:)
!> @param fstrout output fortran string character(fnlen)
!
!> @date 02/09/20  MDG 1.0 original
!--------------------------------------------------------------------------
pure recursive function fstringify(strin) result(fstrout)
!DEC$ ATTRIBUTES DLLEXPORT :: fstringify

use local
use ISO_C_BINDING

IMPLICIT NONE

character(kind=c_char),INTENT(IN)         :: strin(:)
character(fnlen)                          :: fstrout

integer(kind=irg)                         :: slen, i, s(1)

s = shape(strin)

do i=1,s(1)-1    ! don't copy the null character...
  fstrout(i:i) = strin(i)
end do

end function fstringify


!--------------------------------------------------------------------------!
! SUBROUTINE:HDF_writeEMheader
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write the EMsoft header information to the HDF file
!
!> @details The EMheader is a group that contains
!> the following basic dataset strings
!>
!> EMsoft version       : scversion from local.f90
!> execution date       : dstr
!> start time           : tstr1
!> end time             : tstr2
!> program name         : prn
!> user name            : EMsoft_getUsername() (local.f90) [these can/should be redefined by the user via nml files] 
!> user location        : EMsoft_getUserlocation() (local.f90)
!> user email           : EMsoft_getUseremail() (local.f90)
!> computer name        : read via system call hostnm()
!
!> @note The original version used regular character arrays and had problems when
!> datasets needed to be overwritten.  Because of this, we decided to start using 
!> the fortran2003 extensions, which have a richer data type set and allow for more
!> flexible C-bindings.
!>
!> @param HDF_head pointer to top of push-pop stack
!> @param dstr date string
!> @param tstrb time start string
!> @param tstre time end string
!> @param prn program name
!> @param dataname
!>
!> @date 03/20/15 MDG 1.0 original
!> @date 03/26/15 MDG 2.0 modified with fortran2003 resources
!> @date 05/21/16 MDG 3.0 updated HDF format for multiple program runs in single file
!> @date 05/21/16 MDG 3.1 changed StartTime and StopTime to include the date for multi-day runs
!--------------------------------------------------------------------------
recursive subroutine HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, prn, dataname)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeEMheader

use local
use io
use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
character(11),INTENT(INOUT)                           :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                              :: tstrb
character(15),INTENT(IN)                              :: tstre
character(fnlen),INTENT(IN)                           :: prn
character(fnlen),INTENT(IN),OPTIONAL                  :: dataname

integer                                               :: hdferr ! error flag
integer                                               :: i,ic,nlen 
character(100)                                        :: c
character(fnlen)                                      :: line, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create and open the EMheader group
groupname = SC_EMheader
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFerror_check('HDF_writeEMheader:HDF_createGroup:'//trim(groupname), hdferr)

! create and open the dataname group to allow for different data sets in the same file
if (PRESENT(dataname).eqv..TRUE.) then 
  groupname = trim(dataname)
  hdferr = HDF_createGroup(groupname, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_createGroup:'//trim(groupname), hdferr)
end if

! version number /EMheader/Version 'character'
line = 'Version'
line2(1) = trim(EMsoft_getEMsoftversion())
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! execution data /EMheader/Date 'character'
line = 'Date'
line2(1) = dstr
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! start time /EMheader/StartTime 'character'
line = 'StartTime'
line2(1) = dstr//', '//tstrb
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)

else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)

end if

! stop time /EMheader/StopTime 'character'; this is often updated at the end of a run
! since the end date can be different from the start date, especially for long runs, we get a new dstr string
call timestamp(datestring=dstr)
line = 'StopTime'
line2(1) = dstr//', '//tstre
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! program name /EMheader/ProgramName 'character'
line = 'ProgramName'
line2(1) = prn 
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! user name /EMheader/UserName 'character'
line = 'UserName'
line2(1) = EMsoft_getUsername()
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! user location /EMheader/UserLocation 'character'
line = 'UserLocation'
line2(1) = EMsoft_getUserlocation()
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! user email /EMheader/UserEmail 'character'
line = 'UserEmail'
line2(1) = EMsoft_getUseremail()
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! hostname /EMheader/HostName 'character'
call hostnm(c)

! lowercase it
nlen = len(c) 
do i=1,nlen 
   ic = ichar(c(i:i)) 
   if (ic >= 65 .and. ic < 90) c(i:i) = char(ic+32) 
end do 
line = 'HostName'
line2(1) = c
call H5Lexists_f(HDF_head%next%objectID,trim(line),g_exists, hdferr)
call HDFerror_check('HDF_writeEMheader:H5Lexists_f:'//trim(line), hdferr)

if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head, overwrite)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line)//':overwrite', hdferr)
else
  hdferr = HDF_writeDatasetStringArray(line, line2, 1, HDF_head)
  call HDFerror_check('HDF_writeEMheader:HDF_writeDatasetStringArray:'//trim(line), hdferr)
end if

! close the dataname group, if present
if (PRESENT(dataname).eqv..TRUE.) call HDF_pop(HDF_head)

! and close this group
call HDF_pop(HDF_head)

end subroutine HDF_writeEMheader

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! to avoid user usage of file, group, dataset, etc, IDs, we use a push-pop
! stack to keep track of the open items and close them again.  The only 
! price to pay for doing things this way, is that the output must be written
! in a particular order at first.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_push
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief push an HDF object to the stack
!
!> @param HDF_head top of the current stack
!> @param oT object type character
!> @param oID object identifier
!> @param oName name
!> @param verbose (optional) 
!
!> @date 03/17/15 MDG 1.0 original
!> @date 08/30/19 MDG 2.0 modified HDF_head definition for python f90wrap compatibility
!--------------------------------------------------------------------------
recursive subroutine HDF_push(HDF_head, oT, oID, oName, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_push

use local
use io

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
character(LEN=1),INTENT(IN)                           :: oT
integer(HID_T),INTENT(IN)                             :: oID 
character(fnlen),INTENT(IN)                           :: oName
logical,INTENT(IN),OPTIONAL                           :: verbose

type(HDFobjectStackType),pointer                      :: node
integer(kind=irg)                                     :: istat

! the stack always exists but we never use the top level
if (.not.associated(HDF_head%next)) then 
   allocate(HDF_head%next,stat=istat)                        ! allocate new value
   call HDFerror_check('HDF_push: unable to allocate HDF_head pointer', istat, .TRUE.)

   nullify(HDF_head%next%next)                               ! nullify next in list
   if (PRESENT(verbose)) then 
     if (verbose) call Message('  -> creating HDF_head linked list', frm = "(A)")
   end if
else
   allocate(node,stat=istat)                        ! allocate new value
   call HDFerror_check('HDF_push: unable to allocate node pointer', istat, .TRUE.)

   node%next => HDF_head%next
   HDF_head%next => node
end if

! set the values
HDF_head%next % objectType = oT
HDF_head%next % objectID = oID
HDF_head%next % objectname = trim(oName)

if (present(verbose)) call HDF_stackdump(HDF_head)

end subroutine HDF_push

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_pop
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief pop an HDF object from the stack and close it
!
!> @param HDF_head top of the current stack
!> @param closeall (optional) close all open objects
!
!> @date 03/17/15 MDG 1.0 original
!> @date 04/15/15 MDG 1.1 corrected off-by-one error in linked list, causing file to be improperly closed
!> @date 08/30/19 MDG 2.0 modified HDF_head definition for python f90wrap compatibility
!--------------------------------------------------------------------------
recursive subroutine HDF_pop(HDF_head, closeall, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_pop

use local
use io

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),optional                             :: closeall
logical,INTENT(IN),optional                             :: verbose

integer                                                 :: error, istat
type(HDFobjectStackType),pointer                        :: tmp

nullify(tmp)

if (PRESENT(closeall)) then  
! this would be called if an error arises that forces a complete shutdown of the program, or at the end of a regular program
  do while (associated(HDF_head%next)) 
! close the current object 
    error = HDF_close_level(HDF_head%next % objectType, HDF_head%next % objectID)
    call HDFerror_check('HDF_pop:unable to close requested level for object type '//HDF_head%next%objectType, error,.TRUE.)

! and re-point the stack head
    tmp => HDF_head%next
    HDF_head%next => HDF_head % next % next
! delete the old entry
    deallocate(tmp)
  end do
else
! close the current object 
  error = HDF_close_level(HDF_head%next % objectType, HDF_head%next % objectID)
  call HDFerror_check('HDF_pop:unable to close requested level for object type '//HDF_head%next%objectType, error,.TRUE.)

! and re-point the stack head
  tmp => HDF_head%next
  HDF_head%next => HDF_head % next % next
! delete the old entry
  deallocate(tmp)

  if (present(verbose)) call HDF_stackdump(HDF_head)
end if

contains

  recursive function HDF_close_level(oT, oID) result(error)
  
  use local

  IMPLICIT NONE

  character(LEN=1),INTENT(IN)   :: oT
  integer(HID_T),INTENT(IN)     :: oID 
  integer(kind=irg)             :: error

  select case(oT)
  case ('f') 
    call h5fclose_f(oID, error)  ! close the file
    call HDFerror_check('HDF_pop:h5fclose_f', error)


  case ('g') 
    call h5gclose_f(oID, error)  ! close the group
    call HDFerror_check('HDF_pop:h5gclose_f', error)


  case ('d') 
    call h5dclose_f(oID, error)  ! close the data set
    call HDFerror_check('HDF_pop:h5dclose_f', error)


  case ('a') 
    call h5aclose_f(oID, error)  ! close the attribute
    call HDFerror_check('HDF_pop:h5aclose_f', error)


  case ('t') 
    call h5tclose_f(oID, error)  ! close the data type
    call HDFerror_check('HDF_pop:h5tclose_f', error)


  case ('s') 
    call h5sclose_f(oID, error)  ! close the data space
    call HDFerror_check('HDF_pop:h5sclose_f', error)


  case DEFAULT
  end select

end function HDF_close_level

end subroutine HDF_pop

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_stackdump
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print out the entire stack for debugging purposes
!
!> @param HDF_head top of the current stack
!
!> @date 03/19/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_stackdump(HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_stackdump

use local
use io

IMPLICIT NONE

type(HDFobjectStackType),INTENT(IN)                  :: HDF_head

type(HDFobjectStackType),pointer                     :: tmp
integer(kind=irg)                                    :: io_int(1)

tmp => HDF_head % next
if (.not.associated(tmp)) then
  call WriteValue('Stack is empty','')

else
  call WriteValue('HDF stack entries','')

  do
    if (.not.associated(tmp)) EXIT
    call WriteValue('','>'//tmp%objectType//'<  >'//trim(tmp%objectName)//'<', frm = "(A)",advance="no") 

    io_int(1) = tmp%objectID
    call WriteValue('',io_int,1,frm="(I12)")

    tmp => tmp%next
  end do
end if

end subroutine HDF_stackdump

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFerror_check
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief deal with an HDF error
!
!> @param OffendingRoutine name of the routine that caused the error + possible message
!> @param error non-zero integer
!> @param Fatal (optional logical parameter) Fatal or non-fatal error (logical)
!
!> @date 03/17/15  MDG 1.0 original
!> @date 12/16/16  MDG 2.0 reworked with new error handling approach
!--------------------------------------------------------------------------
recursive subroutine HDFerror_check(OffendingRoutine, error, Fatal)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFerror_check

use io
use local

IMPLICIT NONE

character(LEN=*),INTENT(IN)  :: OffendingRoutine   ! name of offending routine + message
integer(kind=irg),INTENT(IN) :: error              ! returned error code
logical,OPTIONAL,INTENT(IN)  :: Fatal              ! if true, then report the error and stop the program

integer(kind=irg)            :: io_int(1)

if (error.lt.0) then 
  io_int(1) = error
  call WriteValue('Error code : ',io_int,1)
  
  call Message('   returned by routine '//OffendingRoutine,frm="(A)")
  
  if (present(Fatal)) then
    if (Fatal.eqv..TRUE.) STOP 'Unrecoverable Error' ! this is not very graceful, but it'll do the job for now ...
  end if
end if

end subroutine HDFerror_check


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_handleError  [obsolete; replaced by HDFerror_check]
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief deal with an HDF error (old routine, to be replaced everywhere by HDFerror_check)
!
!> @param error non-zero integer
!> @param OffendingRoutine name of the routine that caused the error + possible message
!> @param NonFatal (optional logical parameter) Fatal or non-fatal error (logical)
!
!> @date 03/17/15  MDG 1.0 original
!> @date 12/18/16  MDG 1.1 changed output to be identical to that of HDFerror_check
!--------------------------------------------------------------------------
recursive subroutine HDF_handleError(error,OffendingRoutine, NonFatal)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_handleError

use io
use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN) :: error              ! returned error code
character(LEN=*),INTENT(IN)  :: OffendingRoutine   ! name of offending routine + message
logical,OPTIONAL,INTENT(IN)  :: NonFatal           ! if true, then report the error but don't stop

integer(kind=irg)            :: io_int(1)

if (error.lt.0) then 
  io_int(1) = error
  call WriteValue('Error code : ',io_int,1)
  call Message('   returned by routine '//OffendingRoutine,frm="(A)")

  if (.not.present(NonFatal)) STOP 'Unrecoverable Error' ! this is not very graceful, but it'll do the job for now ...
end if

end subroutine HDF_handleError


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! from here on, we have basic HDF support routines
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_createFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Create a new HDF file (this also opens the file)
!
!> @param HDFname filename string
!> @param HDF_head
!
!> @date 03/17/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_createFile(HDFname, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_createFile

use local
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: HDFname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: file_id ! file identifier
integer                                                 :: hdferr  ! hdferr flag

success = 0

! Create a new file using default properties.
call h5fcreate_f(cstringify(HDFname), H5F_ACC_TRUNC_F, file_id, hdferr)
call HDFerror_check('HDF_createFile:h5fcreate_f:'//trim(HDFname), hdferr)

if (hdferr.lt.0) then
  success = -1
else
  call HDF_push(HDF_head, 'f', file_id, HDFname)
end if

FixedLengthflag = .FALSE.

end function HDF_createFile


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_openFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Open an HDF file
!
!> @param HDFname filename string
!> @param HDF_head
!> @param readonly (optional) file open mode
!
!> @date 03/17/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_openFile(HDFname, HDF_head, readonly) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_openFile

use local
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: HDFname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: readonly
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: file_id ! file identifier
integer                                                 :: hdferr  ! hdferr flag

success = 0
if (present(readonly)) then 
  call H5Fopen_f(cstringify(HDFname), H5F_ACC_RDONLY_F, file_id, hdferr)
  call HDFerror_check('HDF_openFile:H5Fopen_f:'//trim(HDFname)//':readonly', hdferr)
else
  call H5Fopen_f(cstringify(HDFname), H5F_ACC_RDWR_F, file_id, hdferr)
  call HDFerror_check('HDF_openFile:H5Fopen_f:'//trim(HDFname), hdferr)
end if

if (hdferr.lt.0) then 
  success = -1
else
  call HDF_push(HDF_head, 'f', file_id, HDFname)
end if

FixedLengthflag = .FALSE.

end function HDF_openFile


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_createGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create and open a new group; if it already exists, just open it
!
!> @param groupname filename string
!> @param HDF_head
!
!> @date 03/17/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_createGroup(groupname, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_createGroup

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: groupname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: group_id!  identifier
integer                                                 :: hdferr  ! hdferr flag
logical                                                 :: g_exists

success = 0

call H5Lexists_f(HDF_head%next%objectID,cstringify(groupname),g_exists, hdferr)
call HDFerror_check('HDF_createGroup:H5Lexists_f:'//trim(groupname), hdferr)

if (g_exists) then 
  call H5Gopen_f(HDF_head%next%objectID, cstringify(groupname), group_id, hdferr)
  call HDFerror_check('HDF_createGroup:H5Gopen_f:'//trim(groupname), hdferr)

  if (hdferr.lt.0) then
    success = -1
  else
  ! put the group_id onto the HDF_stack
    call HDF_push(HDF_head, 'g', group_id, groupname)
  end if
else 
  call H5Gcreate_f(HDF_head%next%objectID, cstringify(groupname), group_id, hdferr)
  call HDFerror_check('HDF_createGroup:H5Gcreate_f:'//trim(groupname), hdferr)

  if (hdferr.lt.0) then
    success = -1
  else
  ! and put the group_id onto the HDF_stack
    call HDF_push(HDF_head, 'g', group_id, groupname)
  end if
end if

end function HDF_createGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_openGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief open an existing group 
!
!> @param groupname filename string
!> @param HDF_head
!
!> @date 03/17/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_openGroup(groupname, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_openGroup

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: groupname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: group_id !  identifier
integer                                                 :: hdferr  ! hdferr flag

success = 0

call H5Gopen_f(HDF_head%next%objectID, cstringify(groupname), group_id, hdferr)
call HDFerror_check('HDF_openGroup:H5Gopen_f:'//trim(groupname), hdferr)

if (hdferr.lt.0) then
  success = -1
else
! put the group_id onto the HDF_stack
  call HDF_push(HDF_head, 'g', group_id, groupname)
end if

end function HDF_openGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_openDataset
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief open an existing dataset
!
!> @param dataname string
!> @param HDF_head
!
!> @date 03/17/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_openDataset(dataname, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_openDataset

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: data_id !  identifier
integer                                                 :: hdferr  ! hdferr flag

success = 0

call H5dopen_f(HDF_head%next%objectID, cstringify(dataname), data_id, hdferr)
call HDFerror_check('HDF_openDataset:H5dopen_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
else
! put the data_id onto the HDF_stack
  call HDF_push(HDF_head, 'd', data_id, dataname)
end if

end function HDF_openDataset


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetTextFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a text file data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param filename of the text file
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetTextFile(dataname, filename, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetTextFile

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
character(fnlen),INTENT(IN)                             :: filename
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

character(len=fnlen, KIND=c_char),allocatable, TARGET   :: stringarray(:) 
integer(kind=irg)                                       :: nlines

integer(HSIZE_T)                                        :: dim0 
integer(SIZE_T)                                         :: sdim 
integer(HID_T)                                          :: filetype, space, dset ! Handles
integer                                                 :: hdferr, i, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims
integer(HSIZE_T), DIMENSION(1:2)                        :: maxdims
logical                                                 :: g_exists

TYPE(C_PTR), ALLOCATABLE, TARGET                        :: wdata(:)
TYPE(C_PTR)                                             :: f_ptr

success = 0

stringarray =  HDF_readfromTextfile(filename, nlines) 

! first, convert the stringarray to an array of C-pointers, with each string
! terminated by a C_NULL_CHAR.
dims(1) = nlines
allocate(wdata(1:dims(1)))
do i=1,dims(1)
  wdata(i) = C_LOC(stringarray(i))
end do

! then we write this C_ptr to the HDF file in the proper data set

! first create the memory data type (filetype)
call H5Tcopy_f(H5T_STRING, filetype, hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:H5Tcopy_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
call H5Lexists_f(HDF_head%next%objectID,cstringify(dataname),g_exists, hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:H5Lexists_f:'//trim(dataname), hdferr)

if (g_exists) then 
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetTextFile:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), filetype, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetTextFile:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

f_ptr = C_LOC(wdata(1))
call h5dwrite_f(dset, filetype, f_ptr, hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:h5sclose_f:'//trim(dataname), hdferr)

call h5tclose_f(filetype, hdferr)
call HDFerror_check('HDF_writeDatasetTextFile:h5tclose_f:'//trim(dataname), hdferr)

deallocate(wdata)

! that's it

end function HDF_writeDatasetTextFile

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readfromTextfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a text file and return it in a C_NULL_CHAR terminated string array
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param filename file name (string)
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readfromTextfile(filename,nlines) result(stringarray)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readfromTextFile

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                                  :: filename
integer(kind=irg),INTENT(OUT)                                :: nlines
character(len=fnlen, KIND=c_char), allocatable               :: stringarray(:) 

integer(kind=irg)                                            :: i, j, dt
character(len=fnlen, KIND=c_char), DIMENSION(1)              :: line 

dt = 55
! read the file first to determine the number of lines
open(unit=dt,file=trim(filename),action='read',form='formatted',status='old')
nlines = 0
do
  read (dt,"(A)",end=10) line(1)
  nlines = nlines + 1
end do
10 close(unit=dt,status='keep')

! then re-read the file and store all the lines in the wdata array
allocate(stringarray(1:nlines))
open(unit=dt,file=trim(filename),action='read',form='formatted',status='old')
do i=1,nlines
! initialize the line to null characters before each read
  do j=1,fnlen
    line(1)(j:j) = char(0)
  end do
! read the line
  read (dt,"(A)") line(1)
! find the string length and put the next character equal to C_NULL_CHAR
  j = len(trim(line(1)))+1
! truncate a line if it has more than fnlen characters
  if (j.gt.fnlen) j = fnlen
  line(1)(j:j) = C_NULL_CHAR
! store the line in the array
  stringarray(i) = line(1)
end do
close(unit=dt,status='keep')

end function HDF_readfromTextfile


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetStringArray
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a string array from a data set 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param nlines number of lines in string array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!> @date 12/14/16  MDG 1.1 added functionality for fixed length strings used by, e.g., DREAM.3D
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetStringArray(dataname, nlines, HDF_head, hdferr, stringarray)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetStringArray

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(OUT)                           :: nlines
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg),INTENT(OUT)                           :: hdferr
! character(len=fnlen, KIND=c_char),allocatable, TARGET, INTENT(OUT)   :: stringarray(:) 
character(len=fnlen, KIND=c_char),allocatable, TARGET, INTENT(OUT)   :: stringarray(:) 

integer(HID_T)                                          :: filetype, space, memtype ! Handles
integer                                                 :: i, length
integer(HSIZE_T), DIMENSION(1:1)                        :: dims
integer(HSIZE_T), DIMENSION(1:1)                        :: maxdims
integer(SIZE_T)                                         :: size

character(len = fnlen, kind=c_char),  POINTER           :: pfstr ! A pointer to a Fortran string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET          :: rdata ! Read buffer
character(len=fnlen), TARGET                            :: fl_rdata 
TYPE(C_PTR)                                             :: f_ptr

! Open dataset.
!
hdferr = HDF_openDataset(dataname, HDF_head)
!
! Get the datatype.
!
call H5Dget_type_f(HDF_head%next%objectID, filetype, hdferr)
call HDFerror_check('HDF_readDatasetStringArray:H5Dget_type_f:'//trim(dataname), hdferr)

if (FixedLengthflag.eqv..TRUE.) then ! this option is only set up to read one single string into stringarray...
  call H5Tget_size_f(filetype, size, hdferr)
  call HDFerror_check('HDF_readDatasetStringArray:H5Tget_size_f:'//trim(dataname), hdferr)

! Get dataspace and allocate memory for read buffer.
!
  call H5Dget_space_f(HDF_head%next%objectID, space, hdferr)
  call HDFerror_check('HDF_readDatasetStringArray:H5Dget_space_f:'//trim(dataname), hdferr)
 
  call H5Tcopy_f(H5T_FORTRAN_S1, memtype, hdferr)
  call HDFerror_check('HDF_readDatasetStringArray:H5Tcopy_f:'//trim(dataname), hdferr)
  
  call H5Tset_size_f(memtype, size-1, hdferr)
  call HDFerror_check('HDF_readDatasetStringArray:H5Tset_size_f:'//trim(dataname), hdferr)

!
! Read the data.
!
  f_ptr = C_LOC(fl_rdata(1:1))
  call h5dread_f(HDF_head%next%objectID, memtype, f_ptr, hdferr) !, space)
  call HDFerror_check('HDF_readDatasetStringArray:h5dread_f:'//trim(dataname), hdferr)

  allocate(stringarray(1))
  do i=1,size-1
    stringarray(1)(i:i) = fl_rdata(i:i)
  end do
  nlines = 1
else ! there could be multiple variable length strings to be read...
! Get dataspace and allocate memory for read buffer.
  call H5Dget_space_f(HDF_head%next%objectID, space, hdferr)
  call HDFerror_check('HDF_readDatasetStringArray:H5Dget_space_f:'//trim(dataname), hdferr)

  ! this routine returns the rank of the data set in the hdferr variable when successful, otherwise -1
  call H5Sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
  if (hdferr.lt.0) then 
    call HDFerror_check('HDF_readDatasetStringArray:H5Sget_simple_extent_dims_f:'//trim(dataname), hdferr)
  end if

  ALLOCATE(rdata(1:dims(1)), stringarray(1:dims(1)))
!
! Read the data.
!
  f_ptr = C_LOC(rdata(1))
  call h5dread_f(HDF_head%next%objectID, H5T_STRING, f_ptr, hdferr)
  call HDFerror_check('HDF_readDatasetStringArray:h5dread_f:'//trim(dataname), hdferr)

!
! convert the data to a string array
!
  DO i = 1, dims(1)
    call C_F_POINTER(rdata(i), pfstr)

    length = 0
    DO
     IF(pfstr(length+1:length+1).EQ.C_NULL_CHAR.OR.length.GE.fnlen) EXIT
     length = length + 1
    ENDDO
    stringarray(i) = pfstr(1:length)
  END DO

  nlines = dims(1)
  DEALLOCATE(rdata)
end if

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetStringArray:h5sclose_f:'//trim(dataname), hdferr)

call H5Tclose_f(filetype, hdferr)
call HDFerror_check('HDF_readDatasetStringArray:H5Tclose_f:'//trim(dataname), hdferr)

! close the dataset
call HDF_pop(HDF_head)

end subroutine HDF_readDatasetStringArray

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_extractDatasetTextfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a string array from a data set and stores it as a text file 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param textfile name of output text file
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_extractDatasetTextfile(dataname, textfile, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_extractDatasetTextfile

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
character(fnlen),INTENT(IN)                             :: textfile
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: filetype, space ! Handles
integer                                                 :: hdferr, i, length, dt
integer(HSIZE_T), DIMENSION(1:1)                        :: dims
integer(HSIZE_T), DIMENSION(1:1)                        :: maxdims

character(len = fnlen, kind=c_char),  POINTER           :: pfstr ! A pointer to a Fortran string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET          :: rdata ! Read buffer
TYPE(C_PTR)                                             :: f_ptr

! Open dataset.
!
hdferr = HDF_openDataset(dataname, HDF_head)
!
! Get the datatype.
!
call H5Dget_type_f(HDF_head%next%objectID, filetype, hdferr)
call HDFerror_check('HDF_extractDatasetTextfile:H5Dget_type_f:'//trim(dataname), hdferr)

! Get dataspace and allocate memory for read buffer.
!
call H5Dget_space_f(HDF_head%next%objectID, space, hdferr)
call HDFerror_check('HDF_extractDatasetTextfile:H5Dget_space_f:'//trim(dataname), hdferr)

call H5Sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
if (hdferr.lt.0) then 
  call HDFerror_check('HDF_extractDatasetTextfile:H5Sget_simple_extent_dims_f:'//trim(dataname), hdferr)
end if

ALLOCATE(rdata(1:dims(1)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1))
call h5dread_f(HDF_head%next%objectID, H5T_STRING, f_ptr, hdferr)
call HDFerror_check('HDF_extractDatasetTextfile:h5dread_f:'//trim(dataname), hdferr)

!
! store the datain a textfile
!
dt = 55
open(unit=dt, file=trim(textfile), status='unknown', form='formatted')
DO i = 1, dims(1)
  call C_F_POINTER(rdata(i), pfstr)

  length = 0
  DO
     IF(pfstr(length+1:length+1).EQ.C_NULL_CHAR.OR.length.GE.fnlen) EXIT
     length = length + 1
  ENDDO
  write(dt,"(A)") pfstr(1:length)
END DO
close(unit=dt,status='keep')

DEALLOCATE(rdata)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_extractDatasetTextfile:h5sclose_f:'//trim(dataname), hdferr)

call H5Tclose_f(filetype, hdferr)
call HDFerror_check('HDF_extractDatasetTextfile:H5Tclose_f:'//trim(dataname), hdferr)

! close the dataset
call HDF_pop(HDF_head)

success = 0

end function HDF_extractDatasetTextfile

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetStringArray
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a string array
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param filename of the text file
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!> @date 05/20/16  MDG 1.1 correction for overflow of stringarray when input string is actually 132 characters long
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetStringArray(dataname, inputarray, nlines, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetStringArray

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: nlines
character(len=fnlen),INTENT(IN)                         :: inputarray(nlines) 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(kind=irg),parameter                             :: fnlenp = fnlen+1
character(len=fnlenp, KIND=c_char),TARGET               :: stringarray(nlines) 
integer(HSIZE_T)                                        :: dim0 
integer(SIZE_T)                                         :: sdim 
integer(HID_T)                                          :: filetype, space, dset ! Handles
integer                                                 :: hdferr, i, rnk, l
integer(HSIZE_T), DIMENSION(1:1)                        :: dims
! integer(HSIZE_T), DIMENSION(1:2)                        :: maxdims

TYPE(C_PTR), ALLOCATABLE, TARGET                        :: wdata(:)
TYPE(C_PTR)                                             :: f_ptr

success = 0

stringarray = ''
! first, convert the stringarray to an array of C-pointers, with each string
! terminated by a C_NULL_CHAR.
dims(1) = nlines
allocate(wdata(1:dims(1)))
do i=1,dims(1)
  l = len(trim(inputarray(i)))+1
  if (l.gt.fnlenp) l = fnlenp           ! if a string is too long for some reason, we just truncate it
  stringarray(i) = trim(inputarray(i))
  stringarray(i)(l:l) = C_NULL_CHAR
  wdata(i) = C_LOC(stringarray(i)(1:1))
end do

! then we write this C_ptr to the HDF file in the proper data set

! first create the memory data type (filetype)
call H5Tcopy_f(H5T_STRING, filetype, hdferr)
call HDFerror_check('HDF_writeDatasetStringArray:H5Tcopy_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
if (hdferr.lt.0) then
  call HDFerror_check('HDF_writeDatasetStringArray:h5screate_simple_f:'//trim(dataname), hdferr)
end if

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then 
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetStringArray:h5dopen_f:'//trim(dataname)//':overwrite', hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), filetype, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetStringArray:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

f_ptr = C_LOC(wdata(1))
call h5dwrite_f(dset, filetype, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetStringArray:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetStringArray:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetStringArray:h5sclose_f:'//trim(dataname), hdferr)

call H5Tclose_f(filetype, hdferr)
call HDFerror_check('HDF_writeDatasetStringArray:H5Tclose_f:'//trim(dataname), hdferr)

deallocate(wdata)

! that's it

end function HDF_writeDatasetStringArray

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetCharArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 1D array of c_chars    [variable type H5T_STD_U8LE]
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param filename of the text file
!> @param HDF_head
!
!> @date 03/31/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetCharArray1D(dataname, chararray, dims, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetCharArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T), INTENT(IN)                            :: dims(1)
character(len=1),TARGET                                 :: chararray(dims(1)) 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, i, rnk, l

TYPE(C_PTR), dimension(1:1), TARGET                     :: wdata
TYPE(C_PTR)                                             :: f_ptr

success = 0

wdata(1) = C_LOC(chararray(1))

! then we write this C_ptr to the HDF file in the proper data set

! Create dataspace.
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray1D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the c_char data to it.
!
if (present(overwrite)) then 
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray1D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray1D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_STD_U8LE, wdata(1), hdferr )
call HDFerror_check('HDF_writeDatasetCharArray1D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetCharArray1D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray1D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetCharArray1D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetCharArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 2D array of c_chars    [variable type H5T_STD_U8LE]
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param filename of the text file
!> @param HDF_head
!
!> @date 03/31/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetCharArray2D(dataname, chararray, dims, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetCharArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T), INTENT(IN)                            :: dims(2)
character(len=1),TARGET                                 :: chararray(dims(1), dims(2)) 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, i, rnk, l

TYPE(C_PTR), dimension(1:1), TARGET                     :: wdata

success = 0

wdata(1) = C_LOC(chararray(1,1))
! then we write this C_ptr to the HDF file in the proper data set

! Create dataspace.
rnk = 2
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the c_char data to it.
!
if (present(overwrite)) then 
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if 

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_STD_U8LE, wdata(1), hdferr )
call HDFerror_check('HDF_writeDatasetCharArray2D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetCharArray2D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray2D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetCharArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetCharArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 3D array of c_chars    [variable type H5T_STD_U8LE]
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param filename of the text file
!> @param HDF_head
!
!> @date 03/31/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetCharArray3D(dataname, chararray, dims, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetCharArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T), INTENT(IN)                            :: dims(3)
character(len=1),TARGET                                 :: chararray(dims(1), dims(2), dims(3)) 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, i, rnk, l

TYPE(C_PTR), dimension(1:3), TARGET                     :: wdata

success = 0

wdata(1) = C_LOC(chararray(1,1,1))
! then we write this C_ptr to the HDF file in the proper data set

! Create dataspace.
rnk = 3
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the c_char data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_STD_U8LE, wdata(1), hdferr )
call HDFerror_check('HDF_writeDatasetCharArray3D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetCharArray3D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray3D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetCharArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetCharArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 4D array of c_chars    [variable type H5T_STD_U8LE]
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param filename of the text file
!> @param HDF_head
!
!> @date 03/31/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetCharArray4D(dataname, chararray, dims, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetCharArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T), INTENT(IN)                            :: dims(4)
character(len=1),TARGET                                 :: chararray(dims(1), dims(2), dims(3), dims(4)) 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, i, rnk, l

TYPE(C_PTR), dimension(1:4), TARGET                     :: wdata

success = 0

wdata(1) = C_LOC(chararray(1,1,1,1))

! then we write this C_ptr to the HDF file in the proper data set

! Create dataspace.
rnk = 4
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the c_char data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetCharArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if 

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_STD_U8LE, wdata(1), hdferr )
call HDFerror_check('HDF_writeDatasetCharArray4D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetCharArray4D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetCharArray4D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetCharArray4D



!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetInteger
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write an integer data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param intval  integer 
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetInteger(dataname, intval, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetInteger

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: intval
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

integer, dimension(1:1), TARGET                         :: wdata
TYPE(C_PTR)                                             :: f_ptr

success = 0

dims(1) = 1
wdata(1) = intval

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetInteger:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetInteger:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetInteger:h5dcreate_f:'//trim(dataname), hdferr)
end if 

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetInteger:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetInteger:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetInteger:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetInteger



!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetInteger1byteArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 1D integer array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param intarr 1D integer array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetInteger1byteArray1D(dataname, intarr, dim0, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetInteger1byteArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=1),INTENT(IN)                              :: intarr(dim0)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

integer, dimension(1:dim0), TARGET                      :: wdata
TYPE(C_PTR)                                             :: f_ptr

success = 0

dims(1) = dim0
wdata = intarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetInteger1byteArray1D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetInteger1byteArray1D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetInteger1byteArray1D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetInteger1byteArray1D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetInteger1byteArray1D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetInteger1byteArray1D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetInteger1byteArray1D


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetIntegerArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 1D integer array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param intarr 1D integer array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetIntegerArray1D(dataname, intarr, dim0, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetIntegerArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: intarr(dim0)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

integer(kind=4),allocatable,TARGET                      :: wdata(:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0), stat=istat)

wdata = intarr
dims(1) = dim0

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray1D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray1D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray1D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetIntegerArray1D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray1D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray1D:h5sclose_f:'//trim(dataname), hdferr)
DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetIntegerArray1D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetIntegerArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 2D integer array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param intarr 1D integer array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetIntegerArray2D(dataname, intarr, dim0, dim1, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetIntegerArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: intarr(dim0, dim1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: dims

integer(kind=4),allocatable,TARGET                      :: wdata(:,:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0, dim1), stat=istat)

wdata = intarr

dims(1:2) = (/ dim0, dim1 /)


! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1))

! Create dataspace.
!
rnk = 2
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetIntegerArray2D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray2D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray2D:h5sclose_f:'//trim(dataname), hdferr)
DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetIntegerArray2D


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetIntegerArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 3D integer array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param intarr 1D integer array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetIntegerArray3D(dataname, intarr, dim0, dim1, dim2, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetIntegerArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
integer(kind=irg),INTENT(IN),TARGET                     :: intarr(dim0, dim1, dim2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: dims

integer(kind=4),allocatable,TARGET                      :: wdata(:,:,:)
TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0, dim1, dim2), stat=istat)

dims(1:3) = (/ dim0, dim1, dim2 /)
wdata = intarr

! get a C pointer to the integer array
f_ptr = C_LOC(intarr(1, 1, 1))

! Create dataspace.
!
rnk = 3
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetIntegerArray3D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray3D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray3D:h5sclose_f:'//trim(dataname), hdferr)

DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetIntegerArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetIntegerArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 4D integer array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param intarr 1D integer array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetIntegerArray4D(dataname, intarr, dim0, dim1, dim2, dim3, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetIntegerArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
integer(kind=irg),INTENT(IN)                            :: dim3
integer(kind=irg),INTENT(IN)                            :: intarr(dim0, dim1, dim2, dim3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: dims

integer(kind=4),allocatable,TARGET                      :: wdata(:,:,:,:)
TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0, dim1, dim2, dim3), stat=istat)

wdata = intarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1,1,1))

! Create dataspace.
!
rnk = 4
dims(1:4) = (/ dim0, dim1, dim2, dim3 /)

call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetIntegerArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if 

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetIntegerArray4D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray4D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetIntegerArray4D:h5sclose_f:'//trim(dataname), hdferr)

DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetIntegerArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetFloat
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a single precision array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltval real 
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetFloat(dataname, fltval, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetFloat

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
real(kind=sgl),INTENT(IN)                               :: fltval
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

real(real_kind4), dimension(1:1), TARGET                 :: wdata
TYPE(C_PTR)                                             :: f_ptr

success = 0

dims(1) = 1
wdata(1) = fltval

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetFloat:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloat:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloat:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_REAL, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetFloat:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetFloat:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetFloat:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetFloat

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a single precision array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltval real 
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetDouble(dataname, dblval, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetDouble

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
real(kind=dbl),INTENT(IN)                               :: dblval
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

real(real_kind8), dimension(1:1), TARGET                :: wdata
TYPE(C_PTR)                                             :: f_ptr

success = 0

dims(1) = 1
wdata(1) = dblval

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetDouble:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDouble:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDouble:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetDouble:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetDouble:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetDouble:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetDouble



!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetFloatArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 1D single precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltarr 1D real array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetFloatArray1D(dataname, fltarr, dim0, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetFloatArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
real(kind=sgl),INTENT(IN)                               :: fltarr(dim0)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

real(real_kind4),allocatable,TARGET                     :: wdata(:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

dims(1) = dim0
allocate(wdata(dim0), stat=istat)

wdata = fltarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray1D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray1D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray1D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_REAL, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetFloatArray1D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray1D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray1D:h5sclose_f:'//trim(dataname), hdferr)
DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetFloatArray1D


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetFloatArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 2D single precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltarr 2D real array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetFloatArray2D(dataname, fltarr, dim0, dim1, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetFloatArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
real(kind=sgl),INTENT(IN)                               :: fltarr(dim0, dim1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: dims

real(real_kind4),allocatable,TARGET                     :: wdata(:,:)

TYPE(C_PTR)                                             :: f_ptr

success = 0
allocate(wdata(dim0, dim1), stat=istat)

wdata = fltarr


dims(1:2) = (/ dim0, dim1 /)

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1))

! Create dataspace.
!
rnk = 2
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_REAL, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetFloatArray2D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray2D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray2D:h5sclose_f:'//trim(dataname), hdferr)
DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetFloatArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetFloatArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 3D single precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltarr 3D real array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetFloatArray3D(dataname, fltarr, dim0, dim1, dim2, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetFloatArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
real(kind=sgl),INTENT(IN)                               :: fltarr(dim0, dim1, dim2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: dims

real(real_kind4),allocatable,TARGET                     :: wdata(:,:,:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0, dim1, dim2), stat=istat)

wdata = fltarr

dims(1:3) = (/ dim0, dim1, dim2 /)

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1,1))

! Create dataspace.
!
rnk = 3
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_REAL, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetFloatArray3D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray3D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray3D:h5sclose_f:'//trim(dataname), hdferr)
DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetFloatArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetFloatArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 4D single precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltarr 4D real array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetFloatArray4D(dataname, fltarr, dim0, dim1, dim2, dim3, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetFloatArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
integer(kind=irg),INTENT(IN)                            :: dim3
real(kind=sgl),INTENT(IN)                               :: fltarr(dim0, dim1, dim2, dim3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: dims

real(real_kind4),allocatable,TARGET                     :: wdata(:,:,:,:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0, dim1, dim2, dim3), stat=istat)

wdata = fltarr

dims(1:4) = (/ dim0, dim1, dim2, dim3 /)

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1,1,1))

! Create dataspace.
!
rnk = 4
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_REAL, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetFloatArray4D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray4D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray4D:h5sclose_f:'//trim(dataname), hdferr)
DEALLOCATE(wdata, stat=istat)

! that's it

end function HDF_writeDatasetFloatArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetFloatArray6D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 6D single precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltarr 6D real array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetFloatArray6D(dataname, fltarr, dim0, dim1, dim2, dim3, dim4, dim5, &
                                                HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetFloatArray6D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
integer(kind=irg),INTENT(IN)                            :: dim3
integer(kind=irg),INTENT(IN)                            :: dim4
integer(kind=irg),INTENT(IN)                            :: dim5
real(kind=sgl),INTENT(IN)                               :: fltarr(dim0, dim1, dim2, dim3, dim4, dim5)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:6)                        :: dims

TYPE(C_PTR)                                             :: f_ptr
real(real_kind4),allocatable,TARGET                     :: wdata(:,:,:,:,:,:)

success = 0

allocate(wdata(dim0,dim1,dim2,dim3,dim4,dim5), stat=istat)
dims(1:6) = (/ dim0, dim1, dim2, dim3, dim4, dim5 /)
wdata = fltarr

 ! get a C pointer to the float array
f_ptr = C_LOC(wdata(1,1,1,1,1,1))

! Create dataspace.
!
rnk = 6
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray6D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the 6D float data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray6D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetFloatArray6D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_REAL, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetFloatArray6D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray6D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetFloatArray6D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetFloatArray6D
!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetDoubleArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 1D double precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dblarr 1D double array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetDoubleArray1D(dataname, dblarr, dim0, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetDoubleArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
real(kind=dbl),INTENT(IN)                               :: dblarr(dim0)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: dims

real(real_kind8),allocatable,TARGET                     :: wdata(:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0), stat=istat)
dims(1) = dim0
wdata = dblarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1))

! Create dataspace.
!
rnk = 1
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray1D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray1D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray1D:h5dcreate_f:'//trim(dataname), hdferr)
end if 

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetDoubleArray1D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray1D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray1D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetDoubleArray1D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetDoubleArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 2D double precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dblarr 2D double array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetDoubleArray2D(dataname, dblarr, dim0, dim1, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetDoubleArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
real(kind=dbl),INTENT(IN)                               :: dblarr(dim0, dim1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: dims

real(real_kind8),allocatable,TARGET                     :: wdata(:,:)
TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0,dim1), stat=istat)
dims(1:2) = (/ dim0, dim1 /)
wdata = dblarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1))

! Create dataspace.
!
rnk = 2
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetDoubleArray2D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray2D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray2D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetDoubleArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetDoubleArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 3D double precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dblarr 3D double array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetDoubleArray3D(dataname, dblarr, dim0, dim1, dim2, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetDoubleArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
real(kind=dbl),INTENT(IN)                               :: dblarr(dim0, dim1, dim2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: dims

real(real_kind8),allocatable,TARGET                     :: wdata(:,:,:)

TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0,dim1,dim2), stat=istat)
dims(1:3) = (/ dim0, dim1, dim2 /)
wdata = dblarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1,1))

! Create dataspace.
!
rnk = 3
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetDoubleArray3D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray3D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray3D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetDoubleArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeDatasetDoubleArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a 4D double precision float array data set to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dblarr 4D double array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_writeDatasetDoubleArray4D(dataname, dblarr, dim0, dim1, dim2, dim3, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeDatasetDoubleArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(kind=irg),INTENT(IN)                            :: dim0
integer(kind=irg),INTENT(IN)                            :: dim1
integer(kind=irg),INTENT(IN)                            :: dim2
integer(kind=irg),INTENT(IN)                            :: dim3
real(kind=dbl),INTENT(IN)                               :: dblarr(dim0, dim1, dim2, dim3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success, istat

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)
integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: hdferr, rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: dims

real(real_kind8),allocatable,TARGET                     :: wdata(:,:,:,:)
TYPE(C_PTR)                                             :: f_ptr

success = 0

allocate(wdata(dim0,dim1,dim2,dim3), stat=istat)
dims(1:4) = (/ dim0, dim1, dim2, dim3 /)
wdata = dblarr

! get a C pointer to the integer array
f_ptr = C_LOC(wdata(1,1,1,1))

! Create dataspace.
!
rnk = 4
call h5screate_simple_f(rnk, dims, space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

!
! Create the dataset and write the variable-length string data to it.
!
if (present(overwrite)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeDatasetDoubleArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr )
call HDFerror_check('HDF_writeDatasetDoubleArray4D:h5dwrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray4D:h5dclose_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_writeDatasetDoubleArray4D:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_writeDatasetDoubleArray4D


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------READ subroutines below--------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetCharArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 1D char array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetCharArray1D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetCharArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
character(len=1), dimension(:), allocatable, TARGET, INTENT(OUT)     :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetCharArray1D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray1D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetCharArray1D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1))
call h5dread_f( dset, H5T_STD_U8LE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetCharArray1D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray1D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetCharArray1D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetCharArray1D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetCharArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D char array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetCharArray2D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetCharArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
character(len=1), dimension(:,:), allocatable, TARGET, INTENT(OUT)   :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetCharArray2D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetCharArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1))
call h5dread_f( dset, H5T_STD_U8LE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetCharArray2D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetCharArray2D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetCharArray2D


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetCharArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D char array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetCharArray3D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetCharArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
character(len=1), dimension(:,:,:), allocatable, TARGET, INTENT(OUT) :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetCharArray3D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetCharArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1))
call h5dread_f( dset, H5T_STD_U8LE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetCharArray3D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetCharArray3D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetCharArray3D


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetCharArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D char array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetCharArray4D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetCharArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
character(len=1), dimension(:,:,:,:), allocatable, TARGET, INTENT(OUT) :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetCharArray4D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray4D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetCharArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1,1))
call h5dread_f( dset, H5T_STD_U8LE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetCharArray4D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetCharArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetCharArray4D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetCharArray4D


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetInteger
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns an integer data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!> @date 02/14/17  MDG 1.1 corrected closing order of dataspace and dataset
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetInteger(dataname, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetInteger

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
integer,  TARGET, INTENT(OUT)                                        :: rdata

integer(HID_T)                                          :: space, dset ! Handles

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetInteger:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetInteger:h5dget_space_f:'//trim(dataname), hdferr)

!
! Read the data.
!
f_ptr = C_LOC(rdata)
call h5dread_f( dset, H5T_NATIVE_INTEGER, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetInteger:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetInteger:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetInteger:h5dclose_f:'//trim(dataname), hdferr)

! that's it

end subroutine HDF_readDatasetInteger


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetIntegerArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 1D integer array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetIntegerArray1D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetIntegerArray1D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
integer, dimension(:), allocatable, TARGET, INTENT(OUT)              :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray1D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray1D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray1D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1))
call h5dread_f( dset, H5T_NATIVE_INTEGER, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray1D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray1D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray1D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetIntegerArray1D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetIntegerArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D integer array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetIntegerArray2D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetIntegerArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
integer, dimension(:,:), allocatable, TARGET, INTENT(OUT)            :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray2D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1))
call h5dread_f( dset, H5T_NATIVE_INTEGER, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray2D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray2D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetIntegerArray2D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetIntegerArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D integer array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetIntegerArray3D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetIntegerArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
integer, dimension(:,:,:), allocatable, TARGET, INTENT(OUT)          :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray3D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1))
call h5dread_f( dset, H5T_NATIVE_INTEGER, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray3D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray3D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetIntegerArray3D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetIntegerArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D integer array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetIntegerArray4D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetIntegerArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
!integer, dimension(:,:,:,:), allocatable, TARGET, INTENT(OUT)        :: rdata
integer, dimension(:,:,:,:), allocatable, TARGET        :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray4D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray4D:h5dget_space_f:'//trim(dataname), hdferr)


call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1,1))
call h5dread_f( dset, H5T_NATIVE_INTEGER, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray4D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetIntegerArray4D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetIntegerArray4D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetFloat
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a float data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetFloat(dataname, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetFloat

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind4), TARGET, INTENT(OUT)                                 :: rdata

integer(HID_T)                                          :: space, dset ! Handles

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetFloat:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetFloat:h5dget_space_f:'//trim(dataname), hdferr)


! Read the data.
!
f_ptr = C_LOC(rdata)
call h5dread_f( dset, H5T_NATIVE_REAL, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetFloat:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetFloat:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetFloat:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetFloat

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetFloatArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 1D float array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetFloatArray1D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetFloatArray1D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind4), dimension(:), allocatable, TARGET, INTENT(OUT)      :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray1D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray1D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray1D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1))
call h5dread_f( dset, H5T_NATIVE_REAL, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray1D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray1D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetFloatArray1D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetFloatArray1D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetFloatArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D float array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetFloatArray2D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetFloatArray2D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind4), dimension(:,:), allocatable, TARGET, INTENT(OUT)    :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray2D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1))
call h5dread_f( dset, H5T_NATIVE_REAL, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray2D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetFloatArray2D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetFloatArray2D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetFloatArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D float array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetFloatArray3D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetFloatArray3D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind4), dimension(:,:,:), allocatable, TARGET, INTENT(OUT)  :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray3D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1))
call h5dread_f( dset, H5T_NATIVE_REAL, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray3D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetFloatArray3D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetFloatArray3D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetFloatArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D float array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetFloatArray4D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetFloatArray4D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind4), dimension(:,:,:,:), allocatable, TARGET, INTENT(OUT):: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray4D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray4D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1,1))
call h5dread_f( dset, H5T_NATIVE_REAL, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray4D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetFloatArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetFloatArray4D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetFloatArray4D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a double data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetDouble(dataname, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetDouble

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind8), TARGET, INTENT(OUT)                                 :: rdata

integer(HID_T)                                          :: space, dset ! Handles

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetDouble:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetDouble:h5dget_space_f:'//trim(dataname), hdferr)


! Read the data.
!
f_ptr = C_LOC(rdata)
call h5dread_f( dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetDouble:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetDouble:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetDouble:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetDouble


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetDoubleArray1D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 1D double array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetDoubleArray1D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetDoubleArray1D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(1)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind8), dimension(:), allocatable, TARGET, INTENT(OUT)      :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:1)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray1D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray1D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray1D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1))
call h5dread_f( dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray1D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray1D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray1D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetDoubleArray1D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetDoubleArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D double array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetDoubleArray2D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetDoubleArray2D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind8), dimension(:,:), allocatable, TARGET, INTENT(OUT)    :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:2)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray2D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1))
call h5dread_f( dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray2D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray2D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetDoubleArray2D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetDoubleArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D double array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetDoubleArray3D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetDoubleArray3D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind8), dimension(:,:,:), allocatable, TARGET, INTENT(OUT)  :: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:3)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray3D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1))
call h5dread_f( dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray3D:h5dread_f:'//trim(dataname), hdferr)


!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray3D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetDoubleArray3D

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_readDatasetDoubleArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D double array data set from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param dims dimensions of the array
!> @param HDF_head
!
!> @date 03/26/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HDF_readDatasetDoubleArray4D(dataname, dims, HDF_head, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readDatasetDoubleArray4D

use ISO_C_BINDING

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(OUT)                            :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg), INTENT(OUT)                          :: hdferr
real(real_kind8), dimension(:,:,:,:), allocatable, TARGET, INTENT(OUT):: rdata

integer(HID_T)                                          :: space, dset ! Handles
integer                                                 :: rnk
integer(HSIZE_T), DIMENSION(1:4)                        :: maxdims

TYPE(C_PTR)                                             :: f_ptr

! open the data set
call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray4D:h5dopen_f:'//trim(dataname), hdferr)

! get dataspace and allocate memory for read buffer 
call h5dget_space_f(dset,space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray4D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, dims, maxdims, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


allocate(rdata(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
!
! Read the data.
!
f_ptr = C_LOC(rdata(1,1,1,1))
call h5dread_f( dset, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray4D:h5dread_f:'//trim(dataname), hdferr)

!
! Close and release resources.
!
call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset , hdferr)
call HDFerror_check('HDF_readDatasetDoubleArray4D:h5dclose_f:'//trim(dataname), hdferr)


! that's it

end subroutine HDF_readDatasetDoubleArray4D

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! hyperslab read and write routines
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabCharArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 2D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabCharArray2D(dataname, wdata, hdims, offset, &
                                       dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabCharArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(2)
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
character(kind=c_char),INTENT(IN),TARGET                :: wdata(dims(1),dims(2))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

TYPE(C_PTR)                                             :: f_ptr

success = 0

rnk = 2
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabCharArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabCharArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

f_ptr = C_LOC(wdata(1,1))

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_STD_U8LE, f_ptr, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabCharArray2D:h5dwrite_f:'//trim(dataname), hdferr)


call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabCharArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabCharArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 3D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabCharArray3D(dataname, wdata, hdims, offset, &
                                                 dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabCharArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(3)
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
character(kind=c_char),INTENT(IN)                       :: wdata(dims(1),dims(2),dims(3))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 3
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabCharArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabCharArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_STD_U8LE, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabCharArray3D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabCharArray3D


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabCharArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 4D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabCharArray4D(dataname, wdata, hdims, offset, &
                                                 dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabCharArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(4)
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
character(kind=c_char),INTENT(IN)                       :: wdata(dims(1),dims(2),dims(3),dims(4))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 4
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabCharArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_U8LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabCharArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_STD_U8LE, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabCharArray4D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabCharArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabCharArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabIntegerArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 2D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabIntegerArray2D(dataname, wdata, hdims, offset, &
                                                    dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabIntegerArray2D

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(2)
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
integer(kind=irg),INTENT(IN)                            :: wdata(dims(1),dims(2))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 2
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabIntegerArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabIntegerArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 3D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabIntegerArray3D(dataname, wdata, hdims, offset, &
                                                    dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabIntegerArray3D

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(3)
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
integer(kind=irg),INTENT(IN)                            :: wdata(dims(1),dims(2),dims(3))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 3
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabIntegerArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabIntegerArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabIntegerArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 4D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabIntegerArray4D(dataname, wdata, hdims, offset, &
                                                    dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabintegerArray4D

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(4)
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
integer(kind=irg),INTENT(IN)                            :: wdata(dims(1),dims(2),dims(3),dims(4))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 4
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_STD_I32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_INTEGER, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabintegerArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabIntegerArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabFloatArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 2D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabFloatArray2D(dataname, wdata, hdims, offset, &
                                                  dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabFloatArray2D

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(2)
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
real(real_kind4),INTENT(IN)                             :: wdata(dims(1),dims(2))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 2
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_REAL, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabFloatArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabFloatArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 3D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabFloatArray3D(dataname, wdata, hdims, offset, &
                                                  dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabFloatArray3D

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(3)
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
real(real_kind4),INTENT(IN)                             :: wdata(dims(1),dims(2),dims(3))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 3
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_REAL, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabFloatArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabFloatArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 4D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabFloatArray4D(dataname, wdata, hdims, offset, &
                                                  dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabFloatArray4D

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(4)
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
real(real_kind4),INTENT(IN)                             :: wdata(dims(1),dims(2),dims(3),dims(4))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 4
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F32LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_REAL, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabFloatArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabFloatArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabDoubleArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 2D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabDoubleArray2D(dataname, wdata, hdims, offset, &
                                                   dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabDoubleArray2D

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(2)
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
real(real_kind8),INTENT(IN)                             :: wdata(dims(1),dims(2))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 2
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabDoubleArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabDoubleArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 3D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabDoubleArray3D(dataname, wdata, hdims, offset, &
                                                   dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabDoubleArray3D

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(3)
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
real(real_kind8),INTENT(IN)                             :: wdata(dims(1),dims(2),dims(3))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 3
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabDoubleArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_writeHyperslabDoubleArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief writes a 4D hyperslab to the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param wdata data to be written 
!> @param hdims original dimensions of complete dataset
!> @param offset offset of the hyperslab
!> @param dim0 ...  dimensions of the hyperslab
!> @param HDF_head
!> @param insert (optional) if present, add to existing dataset
!
!> @date 04/06/15  MDG 1.0 original
!> @date 11/08/19  MDG 1.1 modified handling of array dimensions
!--------------------------------------------------------------------------
recursive function HDF_writeHyperslabDoubleArray4D(dataname, wdata, hdims, offset, &
                                                   dims, HDF_head, insert) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeHyperslabDoubleArray4D

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: hdims(4)
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
real(real_kind8),INTENT(IN)                             :: wdata(dims(1),dims(2),dims(3),dims(4))
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical, OPTIONAL, INTENT(IN)                           :: insert
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer                                                 :: hdferr, rnk

success = 0

rnk = 4
call h5screate_simple_f(rnk, hdims, space, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

if (present(insert)) then
  call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5dopen_f:'//trim(dataname), hdferr)
else
  call h5dcreate_f(HDF_head%next%objectID, cstringify(dataname), H5T_IEEE_F64LE, space, dset, hdferr)
  call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5dcreate_f:'//trim(dataname), hdferr)
end if

call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dwrite_f(dset, H5T_NATIVE_DOUBLE, wdata, dims, hdferr, memspace, space)
call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5dwrite_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_writeHyperslabDoubleArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_writeHyperslabDoubleArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabCharArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabCharArray2D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabCharArray2D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
character(len=1,kind=c_char), dimension(:,:), allocatable, TARGET   :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(2), max_dims(2)
integer                                                 :: hdferr, rnk

allocate(rdata(1:dims(1),1:dims(2)))

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5dget_space_f:'//trim(dataname), hdferr)


call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 2
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabCharArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5screate_simple_f:'//trim(dataname), hdferr)


call h5dread_f(dset, H5T_STD_U8LE, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabCharArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabCharArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabCharArray3D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabCharArray3D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
character(kind=c_char), dimension(:,:,:), allocatable, TARGET   :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(3), max_dims(3)
integer                                                 :: hdferr, rnk

allocate(rdata(1:dims(1),1:dims(2),1:dims(3)))

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)

rnk = 3
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabCharArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5screate_simple_f:'//trim(dataname), hdferr)

call h5dread_f(dset, H5T_STD_U8LE, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5dread_f:'//trim(dataname), hdferr)

call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabCharArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabCharArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabCharArray4D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabCharArray4D

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
character(len=1,kind=c_char), dimension(:,:,:,:), allocatable, TARGET   :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(4), max_dims(4)
integer                                                 :: hdferr, rnk

allocate(rdata(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5dget_space_f:'//trim(dataname), hdferr)


call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 4
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabCharArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5screate_simple_f:'//trim(dataname), hdferr)


call h5dread_f(dset, H5T_STD_U8LE, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabCharArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabCharArray4D


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabIntegerArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabIntegerArray2D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabIntegerArray2D

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer, dimension(:,:), allocatable, TARGET            :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(2), max_dims(2)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 2
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2)))
call h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabIntegerArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabIntegerArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabIntegerArray3D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabIntegerArray3D

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer, dimension(:,:,:), allocatable, TARGET          :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(3), max_dims(3)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 3
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2),dims(3)))
call h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabIntegerArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabIntegerArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabIntegerArray4D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabIntegerArray4D

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer, dimension(:,:,:,:), allocatable, TARGET        :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(4), max_dims(4)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 4
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2),dims(3),dims(4)))
call h5dread_f(dset, H5T_NATIVE_INTEGER, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabIntegerArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabIntegerArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabFloatArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabFloatArray2D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabFloatArray2D

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
real(real_kind4), dimension(:,:), allocatable, TARGET    :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(2), max_dims(2)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 2
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2)))
call h5dread_f(dset, H5T_NATIVE_REAL, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabFloatArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabFloatArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabFloatArray3D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabFloatArray3D

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
real(real_kind4), dimension(:,:,:), allocatable, TARGET  :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(3), max_dims(3)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 3
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2),dims(3)))
call h5dread_f(dset, H5T_NATIVE_REAL, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabFloatArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabFloatArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabFloatArray4D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabFloatArray4D

IMPLICIT NONE

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
real(real_kind4), dimension(:,:,:,:), allocatable, TARGET:: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(4), max_dims(4)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 4
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2),dims(3),dims(4)))
call h5dread_f(dset, H5T_NATIVE_REAL, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabFloatArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabFloatArray4D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabDoubleArray2D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 2D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabDoubleArray2D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabDoubleArray2D

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(2)
integer(HSIZE_T),INTENT(IN)                             :: dims(2)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
real(real_kind8), dimension(:,:), allocatable, TARGET    :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(2), max_dims(2)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 2
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2)))
call h5dread_f(dset, H5T_NATIVE_DOUBLE, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray2D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabDoubleArray2D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabDoubleArray3D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 3D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabDoubleArray3D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_readHyperslabDoubleArray3D

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(3)
integer(HSIZE_T),INTENT(IN)                             :: dims(3)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
real(real_kind8), dimension(:,:,:), allocatable, TARGET  :: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(3), max_dims(3)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 3
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2),dims(3)))
call h5dread_f(dset, H5T_NATIVE_DOUBLE, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('HDF_readHyperslabDoubleArray3D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabDoubleArray3D

!--------------------------------------------------------------------------
!
! FUNCTION:HDF_readHyperslabDoubleArray4D
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reads and returns a 4D hyperslab from the current file or group ID 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param offset offset of the hyperslab
!> @param dims dimensions of the hyperslab
!> @param HDF_head
!
!> @date 04/06/15  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_readHyperslabDoubleArray4D(dataname, offset, dims, HDF_head) result(rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: hdf_readHyperslabDoubleArray4D

IMPLICIT NONE

integer,parameter                                       :: real_kind8 = SELECTED_REAL_KIND(Fortran_REAL_8)

character(fnlen),INTENT(IN)                             :: dataname
integer(HSIZE_T),INTENT(IN)                             :: offset(4)
integer(HSIZE_T),INTENT(IN)                             :: dims(4)
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
real(real_kind8), dimension(:,:,:,:), allocatable, TARGET:: rdata

integer(HID_T)                                          :: memspace, space, dset ! Handles
integer(HSIZE_T)                                        :: hdims(4), max_dims(4)
integer                                                 :: hdferr, rnk

call h5dopen_f(HDF_head%next%objectID, cstringify(dataname), dset, hdferr)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5dopen_f:'//trim(dataname), hdferr)

call h5dget_space_f(dset, space, hdferr)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5dget_space_f:'//trim(dataname), hdferr)

call h5sget_simple_extent_dims_f(space, hdims, max_dims, hdferr)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5sget_simple_extent_dims_f:'//trim(dataname), hdferr)


rnk = 4
call h5sselect_hyperslab_f(space, H5S_SELECT_SET_F, offset, dims, hdferr) 
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5sselect_hyperslab_f:'//trim(dataname), hdferr)

call h5screate_simple_f(rnk, dims, memspace, hdferr)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5screate_simple_f:'//trim(dataname), hdferr)


allocate(rdata(dims(1),dims(2),dims(3),dims(4)))
call h5dread_f(dset, H5T_NATIVE_DOUBLE, rdata, hdims, hdferr, memspace, space)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5dread_f:'//trim(dataname), hdferr)


call h5sclose_f(space, hdferr)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5sclose_f:'//trim(dataname), hdferr)

call h5dclose_f(dset, hdferr)
call HDFerror_check('hdf_readHyperslabDoubleArray4D:h5dclose_f:'//trim(dataname), hdferr)


end function HDF_readHyperslabDoubleArray4D


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!-- routines that used to be in files.f90 but contain HDF stuff -----------
!--------------------------------------------------------------------------
!
! SUBROUTINE: CrystalData
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief load or generate crystal data
! 
!> @param cell unit cell pointer
!> @param verbose (OPTIONAL)
!> @param existingHDFhead (OPTIONAL) pass-on variable with current HDF_head pointer, if any
!
!> @date    1/ 5/99 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update after new cell type
!> @date   06/06/14 MDG 4.1 added cell pointer and loadingfile arguments
!> @date   03/30/15 MDG 5.0 changed file format to HDF; always assume that the file exists
!> @date   09/29/16 MDG 5.1 added option to read CrystalData from currently open HDF file
!--------------------------------------------------------------------------
recursive subroutine CrystalData(cell,verbose, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: CrystalData

use io
use crystal
use files
use symmetry
use typedefs

IMPLICIT NONE

type(unitcell),INTENT(INOUT)            :: cell
!f2py intent(in,out) ::  cell
logical,INTENT(IN),OPTIONAL             :: verbose
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

integer(kind=irg)                       :: i, ipg, isave

call ReadDataHDF(cell, existingHDFhead)

! strucdef = .TRUE.
 cell%hexset = .FALSE.
 if (cell%xtal_system.eq.4) cell%hexset = .TRUE.
 if ((cell%xtal_system.eq.5).AND.(cell%SYM_SGset.ne.2)) cell%hexset = .TRUE.

! compute the metric matrices
 call CalcMatrices(cell)

! [code modified on 8/1/18 (MDG), to correct k-vector sampling symmetry errors]
! First generate the point symmetry matrices, then the actual space group.
! if the actual group is also the symmorphic group, then both 
! steps can be done simultaneously, otherwise two calls to 
! GenerateSymmetry are needed.
 if (SGsymnum(cell%SYM_SGnum).eq.cell%SYM_SGnum) then
  call GenerateSymmetry(cell,.TRUE.)
 else
  isave = cell%SYM_SGnum
  cell%SYM_SGnum = SGsymnum(cell%SYM_SGnum)
  call GenerateSymmetry(cell,.TRUE.)
  cell%SYM_SGnum = isave
  call GenerateSymmetry(cell,.FALSE.)
 end if

! and print the information on the screen
if (present(verbose)) then
 if (verbose) then
   call DumpXtalInfo(cell)
 end if
end if 

end subroutine CrystalData

!--------------------------------------------------------------------------
!
! SUBROUTINE: SaveDataHDF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief save crystal structure data to an HDF file
! 
!> @param cell unit cell pointer
!> @param existingHDFhead (optional) if present, then use this as HDF_head
!
!> @date    1/ 5/99 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update after new cell type
!> @date   06/06/14 MDG 4.1 added cell pointer argument
!> @date   03/29/15 MDG 5.0 branched from old version; HDF support 
!> @date   11/07/15 MDG 5.1 correction to writing of SEM_SGset variable
!> @date   09/28/16 MDG 5.2 added option to store CrystalData in currently open HDF file
!--------------------------------------------------------------------------
recursive subroutine SaveDataHDF(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: SaveDataHDF

use io
use crystal
use HDF5
use error
 
IMPLICIT NONE

type(unitcell)         , INTENT(INOUT)  :: cell
!f2py intent(in,out) ::  cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(11)                           :: dstr
character(15)                           :: tstr
character(fnlen)                        :: progname = 'EMmkxtal.f90', groupname, dataset, fname, strings(1)
integer(kind=irg)                       :: hdferr
real(kind=dbl)                          :: cellparams(6)
integer(kind=irg),allocatable           :: atomtypes(:)
real(kind=sgl),allocatable              :: atompos(:,:)
logical                                 :: openHDFfile

openHDFfile = .TRUE.
if (present(existingHDFhead)) then
  if (associated(existingHDFhead%next)) then
    openHDFfile = .FALSE.
    HDF_head = existingHDFhead
  else
    call FatalError("SaveDataHDF","HDF_head pointer passed in to routine is not associated")
  end if 
end if

call timestamp(datestring=dstr, timestring=tstr)

! Initialize FORTRAN interface if needed.
!
if (openHDFfile) then 
  nullify(HDF_head%next)
  call h5open_EMsoft(hdferr)
  call HDFerror_check('SaveDataHDF:h5open_EMsoft', hdferr)

  fname = trim(EMsoft_getXtalpathname())//trim(cell%fname)
  fname = EMsoft_toNativePath(fname)
  hdferr =  HDF_createFile(fname, HDF_head)
  call HDFerror_check('SaveDataHDF:HDF_createFile:'//trim(fname), hdferr)
end if

groupname = SC_CrystalData
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_createGroup:'//trim(groupname), hdferr)

dataset = SC_ProgramName
hdferr = HDF_writeDatasetStringArray(dataset, progname, 1, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetStringArray:'//trim(dataset), hdferr)

dataset = SC_CreationDate
hdferr = HDF_writeDatasetStringArray(dataset, dstr, 1, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetStringArray:'//trim(dataset), hdferr)

dataset = SC_CreationTime
hdferr = HDF_writeDatasetStringArray(dataset, tstr, 1, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetStringArray:'//trim(dataset), hdferr)

dataset = SC_Creator
hdferr = HDF_writeDatasetStringArray(dataset, EMsoft_getUsername(), 1, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetStringArray:'//trim(dataset), hdferr)

dataset = SC_CrystalSystem
hdferr = HDF_writeDatasetInteger(dataset, cell%xtal_system, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetStringArray:'//trim(dataset), hdferr)

dataset = SC_LatticeParameters
cellparams = (/ cell%a, cell%b, cell%c, cell%alpha, cell%beta, cell%gamma /)
hdferr = HDF_writeDatasetDoubleArray1D(dataset, cellparams, 6, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetDoubleArray1D:'//trim(dataset), hdferr)

dataset = SC_SpaceGroupNumber
hdferr = HDF_writeDatasetInteger(dataset, cell%SYM_SGnum, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetInteger:'//trim(dataset), hdferr)

! make sure we do not write a '0' for the SGset variable; it must be either 1 or 2
if (cell%SYM_SGset.eq.0) cell%SYM_SGset = 1
dataset = SC_SpaceGroupSetting
hdferr = HDF_writeDatasetInteger(dataset, cell%SYM_SGset, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetInteger:'//trim(dataset), hdferr)

dataset = SC_Natomtypes
hdferr = HDF_writeDatasetInteger(dataset, cell%ATOM_ntype, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetInteger:'//trim(dataset), hdferr)

allocate(atomtypes(cell%ATOM_ntype))
atomtypes(1:cell%ATOM_ntype) = cell%ATOM_type(1:cell%ATOM_ntype)
dataset = SC_Atomtypes
hdferr = HDF_writeDatasetIntegerArray1D(dataset, atomtypes, cell%ATOM_ntype, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetIntegerArray1D:'//trim(dataset), hdferr)
deallocate(atomtypes)

allocate(atompos(cell%ATOM_ntype,5))
atompos(1:cell%ATOM_ntype,1:5) = cell%ATOM_pos(1:cell%ATOM_ntype,1:5)
dataset = SC_AtomData
hdferr = HDF_writeDatasetFloatArray2D(dataset, atompos, cell%ATOM_ntype, 5, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetFloatArray2D:'//trim(dataset), hdferr)
deallocate(atompos)

dataset = SC_Source
strings(1) = trim(cell%source)
hdferr = HDF_writeDatasetStringArray(dataset, strings, 1, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetStringArray:'//trim(dataset), hdferr)

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)
  call h5close_EMsoft(hdferr)
  call HDFerror_check('SaveDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

end subroutine SaveDataHDF

!--------------------------------------------------------------------------
!
! SUBROUTINE: ReadDataHDF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read crystal structure data from an HDF file
! 
!> @param cell unit cell pointer
!> @param existingHDFhead (optional) if present, then use this as HDF_head
!
!> @date    1/ 5/99 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update after new cell type
!> @date   06/06/14 MDG 4.1 added cell pointer argument
!> @date   03/29/15 MDG 5.0 branched from old version; HDF support 
!> @date   11/07/15 MDG 5.1 corrected reading of SYM_SGset for older xtal files
!> @date   09/29/16 MDG 5.2 added option to read CrystalData from currently open HDF file
!--------------------------------------------------------------------------
recursive subroutine ReadDataHDF(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadDataHDF

use io
use crystal
use error
use HDF5
use ISO_C_BINDING
 
IMPLICIT NONE

type(unitcell)         , INTENT(INOUT)  :: cell
!f2py intent(in,out) ::  cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(fnlen)                        :: dataset, groupname, fname
integer(HSIZE_T)                        :: dims(1), dims2(2)
integer(kind=irg)                       :: hdferr, nlines
real(kind=dbl),allocatable              :: cellparams(:)
integer(kind=irg),allocatable           :: atomtypes(:)
real(kind=sgl),allocatable              :: atompos(:,:)
character(fnlen)                        :: pp
logical                                 :: openHDFfile, d_exists
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)


openHDFfile = .TRUE.
if (present(existingHDFhead)) then
  if (associated(existingHDFhead%next)) then
    openHDFfile = .FALSE.
    HDF_head = existingHDFhead
  else
    call FatalError("ReadDataHDF","HDF_head pointer passed in to routine is not associated")
  end if 
end if

if (openHDFfile) then 
  nullify(HDF_head%next)
  call h5open_EMsoft(hdferr)
  call HDFerror_check('ReadDataHDF:h5open_EMsoft', hdferr)

  fname = trim(EMsoft_getXtalpathname())//trim(cell%fname)
  fname = EMsoft_toNativePath(fname)
  hdferr =  HDF_openFile(fname, HDF_head)
  call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)
end if

groupname = SC_CrystalData
hdferr = HDF_openGroup(groupname, HDF_head)
call HDFerror_check('ReadDataHDF:HDF_openGroup:'//trim(groupname), hdferr)

dataset = SC_CrystalSystem
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%xtal_system)
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)


dataset = SC_LatticeParameters
call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head, hdferr, cellparams)
call HDFerror_check('ReadDataHDF:HDF_readDatasetDoubleArray1D:'//trim(dataset), hdferr)

cell%a = cellparams(1)
cell%b = cellparams(2)
cell%c = cellparams(3)
cell%alpha = cellparams(4)
cell%beta = cellparams(5)
cell%gamma = cellparams(6)

dataset = SC_SpaceGroupNumber
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SYM_SGnum) 
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

dataset = SC_SpaceGroupSetting
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SYM_SGset)
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

! this parameter must be either 1 or 2, but is initialized to 0;
! some older .xtal files may still have 0 in them, so we correct this here
if (cell%SYM_SGset.eq.0) cell%SYM_SGset = 1

dataset = SC_Natomtypes
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%ATOM_ntype)
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)


dataset = SC_Atomtypes
call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, atomtypes)
call HDFerror_check('ReadDataHDF:HDF_readDatasetIntegerArray1D:'//trim(dataset), hdferr)

cell%ATOM_type(1:cell%ATOM_ntype) = atomtypes(1:cell%ATOM_ntype) 
deallocate(atomtypes)

dataset = SC_AtomData
call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, atompos)
call HDFerror_check('ReadDataHDF:HDF_readDatasetFloatArray2D:'//trim(dataset), hdferr)

cell%ATOM_pos(1:cell%ATOM_ntype,1:5) = atompos(1:cell%ATOM_ntype,1:5) 
deallocate(atompos)

dataset = SC_Source
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),d_exists, hdferr)
if (d_exists) then 
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  cell%source = trim(stringarray(1))
  deallocate(stringarray)
else
  cell%source = 'undefined'
  call Message('ReadDataHDF: There is no Source data set in this structure file')
end if

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)

  call h5close_EMsoft(hdferr)
  call HDFerror_check('ReadDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

! for trigonal space groups we need to set SYM_trigonal to .TRUE.
if ((cell%SYM_SGnum.ge.143).and.(cell%SYM_SGnum.le.167)) then
  cell%SG%SYM_trigonal = .TRUE.
else
  cell%SG%SYM_trigonal = .FALSE.
end if 

! we have not yet implemented the rhombohedral setting of the trigonal 
! space groups, so this needs to remain at .FALSE. always.
cell%SG%SYM_second = .FALSE.
!if (cell%SYM_SGset.ne.0) cell%SG%SYM_second=.TRUE.

end subroutine ReadDataHDF

!--------------------------------------------------------------------------
!
! FUNCTION: CheckFixedLengthflag
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns TRUE if there is a FixedLength=1 data set in the current EMheader
! 
!> @param dataset path to the current EMheader
!
!> @date   12/14/16 MDG 1.0 original code 
!--------------------------------------------------------------------------
recursive function CheckFixedLengthflag(dataset, HDF_head) result(itis)
!DEC$ ATTRIBUTES DLLEXPORT :: CheckFixedLengthflag

use HDF5
use h5lt
 
IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: dataset
type(HDFobjectStackType),INTENT(INOUT)          :: HDF_head
!f2py intent(in,out) ::  HDFhead

logical                                         :: itis

integer(kind=irg)                               :: FL, hdferr, i

! we assume this file has variable length strings (the default for EMsoft)
itis = .FALSE.

! look for the data set
i = h5ltfind_dataset_f(HDF_head%next%ObjectID, trim(dataset))

if (i.eq.1) then 
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FL)
  call HDFerror_check('CheckFixedLengthflag:HDF_readDatasetInteger:'//trim(dataset), hdferr)

  if (FL.eq.1) then 
    itis = .TRUE.
  end if
end if

! and set the saved FixedLength flag (private variable)
FixedLengthflag = itis

end function CheckFixedLengthflag

!--------------------------------------------------------------------------
!
! FUNCTION: resetFixedLengthflag
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @briefets the FixedLengthflag to .FALSE.
! 
!> @date   12/14/16 MDG 1.0 original code 
!--------------------------------------------------------------------------
recursive subroutine resetFixedLengthflag()
!DEC$ ATTRIBUTES DLLEXPORT :: resetFixedLengthflag

FixedLengthflag = .FALSE.

end subroutine resetFixedLengthflag

!--------------------------------------------------------------------------
!
! SUBROUTINE: h5_write_pseudo_bse_image
!
!> @author Patrick G. Callahan, UCSB
!
!> @brief Write an hdf5 file containing psuedo/virtual bse images
! 
!> @note  helper routine for h5tslpbse
!
!> @date   10/27/16 PGC 1.0 original
!--------------------------------------------------------------------------
recursive SUBROUTINE h5_write_pseudo_bse_image(fname, dsetnm, hdferr, wdata)
!DEC$ ATTRIBUTES DLLEXPORT :: h5_write_pseudo_bse_image

  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER,PARAMETER               :: real_kind = SELECTED_REAL_KIND(4)
  REAL(KIND=4), DIMENSION(:,:,:),  TARGET, INTENT(INOUT)    :: wdata
!f2py intent(in,out) :: wdata

  INTEGER                         :: rnk
  
  CHARACTER(LEN=fnlen), INTENT(IN)  :: fname  !NAME OF THE HDF5 FILE YOU WANT TO CREATE
  CHARACTER(LEN=fnlen), INTENT(IN)  :: dsetnm !NAME OF THE DATASET YOU WANT TO WRITE IN THE HDF5 FILE
  INTEGER,INTENT(OUT)             :: hdferr   !hdferr flag    
  INTEGER(HID_T)                  :: fid, dsetid, spaceid, memspace !FILE ID, DATASET ID, DATASPACE ID, MEMORY SPACE
  INTEGER(HSIZE_T), ALLOCATABLE   :: dims(:)!, max_dims(2)    
  TYPE(C_PTR)                     :: buff

  buff = C_LOC(wdata(1,1,1))
  dims = shape(wdata)
  rnk = rank(wdata)
  write(*,*) 'dims =', dims!, 'rank = ',rnk

  !START THE FORTRAN INTERFACE
  CALL h5open_EMsoft(hdferr)
  !CREATE THE FILE
  CALL h5fcreate_f(fname, H5F_ACC_TRUNC_F, fid, hdferr)
  !CREATE THE DATASPACE
  CALL h5screate_simple_f(rnk, dims, spaceid, hdferr)
  !CREATE THE DATASET
  CALL h5dcreate_f(fid, dsetnm, H5T_IEEE_F32LE, spaceid, dsetid, hdferr)
  !WRITE THE DATASET
  CALL h5dwrite_f(dsetid, H5T_NATIVE_REAL, buff, hdferr)
  

  !CLEANUP
  !CLOSE THE DATASPACE
  CALL h5sclose_f(spaceid, hdferr)
  !CLOSE THE DATASET
  CALL h5dclose_f(dsetid, hdferr)
!  !CLOSE THE DATASPACE
!  CALL h5sclose_f(spaceid, hdferr)
  !CLOSE THE FILE
  CALL h5fclose_f(fid, hdferr)
  !CLOSE THE FORTRAN INTERFACE
  CALL h5close_EMsoft(hdferr)

END SUBROUTINE h5_write_pseudo_bse_image

!--------------------------------------------------------------------------
!
! SUBROUTINE:  h5_tsl_read_ebsd_pattern
!
!> @author Patrick G. Callahan, UCSB
!
!> @brief Read a single EBSD pattern from a TSL hdf5 file
! 
!> @note  Helper routine for h5tslpbse. Reads a single pattern because
!> many of our datasets are 100s of GB so can't load them all in memory.
!> This will also be useful for dictionary indexing of our scans that are
!> large.In future
!> we should read a number of patterns at once depending on memory limits 
!> and work with them, then move to the next set of patterns.

!
!> @date   10/27/16 PGC 1.0 original
!--------------------------------------------------------------------------
recursive SUBROUTINE h5_tsl_read_ebsd_pattern(fname,dsetnm,hdferr,rdata, offset, szx, szy)
!DEC$ ATTRIBUTES DLLEXPORT :: h5_tsl_read_ebsd_pattern

  USE ISO_C_BINDING
  IMPLICIT NONE

  CHARACTER(LEN=fnlen), INTENT(IN)  :: fname  !NAME OF THE HDF5 FILE YOU ARE READING FROM
  CHARACTER(LEN=fnlen), INTENT(IN)  :: dsetnm !NAME OF THE DATASET YOU WANT TO READ
  INTEGER,INTENT(OUT)             :: hdferr   !hdferr flag
  INTEGER(HID_T)                  :: fid, dsetid, spaceid, memspace !FILE ID, DATASET ID, DATASPACE ID, MEMORY SPACE
  INTEGER, DIMENSION(:,:), TARGET, INTENT(INOUT)    :: rdata  ! variable for data to be read into
!f2py intent(in,out) :: rdata
  TYPE(C_PTR)                     :: buff
  INTEGER(KIND=8),INTENT(IN)      :: offset ! 1d index of pattern in nRow x nColumns sized vector
  INTEGER,INTENT(IN)              :: szx, szy ! nColumns, nRows in the scan
  INTEGER(KIND=8)                 :: start(3), cnt(3) 
  INTEGER(KIND=4)                 :: rnk 

  !! start is the offset of the starting element of the specificied hyperslab
  start(1) = 0
  start(2) = 0
  start(3) = offset

  !! how many blocks to select from the dataspace in each dimension
  cnt(1) = szx
  cnt(2) = szy
  cnt(3) = 1


  !Initialize fortran interface
  CALL h5open_EMsoft(hdferr)
  !Open the hdf5 file
  CALL h5fopen_f(fname, H5F_ACC_RDONLY_F, fid, hdferr)
  !Open an existing dataset
  CALL h5dopen_f(fid, dsetnm, dsetid, hdferr)  
  !GET DATASPACE AND ALLOCATE MEMORY FOR READ BUFFER
  CALL h5dget_space_f(dsetid, spaceid, hdferr)
  !READ THE DATA
  buff = C_LOC(rdata)
  ! Select the region in the hyperslab 
  CALL h5sselect_hyperslab_f( spaceid, H5S_SELECT_SET_F, start, cnt, hdferr)
  rnk=3
  !CREATE A SIMPLE DATASPACE AND OPEN IT FOR ACCESS
  CALL h5screate_simple_f( rnk, cnt, memspace, hdferr)
  CALL h5dread_f(dsetid, H5T_NATIVE_INTEGER, rdata, cnt, hdferr, memspace, spaceid)
  
  !CLOSE MEMSPACE
  CALL h5sclose_f(memspace,hdferr)
  !CLOSE THE DATASPACE
  CALL h5sclose_f(spaceid,hdferr) 
  !CLOSE THE DATASET
  CALL h5dclose_f(dsetid,hdferr)
  !CLOSE THE FORTRAN INTERFACE
  CALL h5close_EMsoft(hdferr)

END SUBROUTINE h5_tsl_read_ebsd_pattern

!--------------------------------------------------------------------------
!
! SUBROUTINE: h5_read_integer_dataset
!
!> @author Patrick G. Callahan, UCSB
!
!> @brief Read a single EBSD pattern from a TSL hdf5 file
! 
!> @note  Helper routine for h5tslpbse. Used to determine 
!> pattern and scan size from TSL H5 files.
!
!> @date   10/27/16 PGC 1.0 original
!--------------------------------------------------------------------------
recursive SUBROUTINE h5_read_integer_dataset(fname, dsetnm, hdferr, rdata)
!DEC$ ATTRIBUTES DLLEXPORT :: h5_read_integer_dataset

  USE ISO_C_BINDING
  IMPLICIT NONE

  CHARACTER(LEN=fnlen), INTENT(IN)  :: fname  !NAME OF THE HDF5 FILE YOU ARE READING FROM
  CHARACTER(LEN=fnlen), INTENT(IN)  :: dsetnm !NAME OF THE DATASET YOU WANT TO READ
  INTEGER,INTENT(OUT)             :: hdferr   !hdferr flag
  INTEGER(HID_T)                  :: fid      !FILE ID
  INTEGER(HID_T)                  :: dsetid   !DATASET ID
  INTEGER(HID_T)                  :: spaceid  !DATASPACE ID
  INTEGER, TARGET, INTENT(OUT)    :: rdata
  TYPE(C_PTR)                     :: buff

  !Initialize fortran interface
  CALL h5open_EMsoft(hdferr)
  !Open the hdf5 file
  CALL h5fopen_f(fname, H5F_ACC_RDONLY_F, fid, hdferr)

  !Open an existing dataset
  CALL h5dopen_f(fid, dsetnm, dsetid, hdferr)  

  !GET DATASPACE AND ALLOCATE MEMORY FOR READ BUFFER
  CALL h5dget_space_f(dsetid, spaceid, hdferr)

  !READ THE DATA
  buff = C_LOC(rdata)
  CALL h5dread_f(dsetid,H5T_NATIVE_INTEGER, buff, hdferr)
  !CLOSE THE DATASPACE
  CALL h5sclose_f(spaceid,hdferr)     
  !CLOSE THE DATASET
  CALL h5dclose_f(dsetid,hdferr)
  !CLOSE THE FORTRAN INTERFACE
  CALL h5close_EMsoft(hdferr)

END SUBROUTINE h5_read_integer_dataset


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_addStringAttributeToGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a string attribute to the current level in the HDF file 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltval real 
!> @param HDF_head
!
!> @date 07/11/18  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_addStringAttributeToGroup(dataname, stratt, HDF_head, overwrite) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_addStringAttributeToGroup

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                             :: dataname
character(len=fnlen, KIND=c_char),INTENT(INOUT)         :: stratt 
!f2py intent(in,out) ::  stratt 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
logical,INTENT(IN),OPTIONAL                             :: overwrite
integer(kind=irg)                                       :: success

integer,parameter                                       :: real_kind4 = SELECTED_REAL_KIND(Fortran_REAL_4)
integer(HID_T)                                          :: aspace_id, dset, atype_id, attr_id ! Handles
integer                                                 :: hdferr, rnk
integer(SIZE_T)                                         :: attrlen
integer(HSIZE_T), DIMENSION(1:1)                        :: dims
integer(HSIZE_T), DIMENSION(1)                          :: data_dims

success = 0

dims(1) = 1

attrlen = len_trim(stratt)
attrlen = attrlen+1
stratt(attrlen:attrlen) = C_NULL_CHAR

! Create dataspace.
rnk = 1
call h5screate_simple_f(rnk, dims, aspace_id, hdferr)
call HDFerror_check('HDF_addStringAttribute:h5screate_simple_f:'//trim(dataname), hdferr)
!
! Create datatype for the attribute.
!
call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, hdferr)
call HDFerror_check('HDF_addStringAttribute:h5tcopy_f:'//trim(dataname), hdferr)

call h5tset_size_f(atype_id, attrlen, hdferr)
call HDFerror_check('HDF_addStringAttribute:h5tset_size_f:'//trim(dataname), hdferr)
!
! Create the attribute and write the string data to it.
!
if (present(overwrite)) then
  call h5aopen_f(HDF_head%next%objectID, cstringify(dataname), attr_id, hdferr)
  call HDFerror_check('HDF_addStringAttribute:h5aopen_f:'//trim(dataname), hdferr)
else
  call h5acreate_f(HDF_head%next%objectID, cstringify(dataname), atype_id, aspace_id, attr_id, hdferr)
  call HDFerror_check('HDF_addStringAttribute:h5acreate_f:'//trim(dataname), hdferr)
end if

if (hdferr.lt.0) then
  success = -1
end if

data_dims(1) = 1
call h5awrite_f(attr_id, atype_id, stratt, data_dims, hdferr )
call HDFerror_check('HDF_addStringAttribute:h5awrite_f:'//trim(dataname), hdferr)

if (hdferr.lt.0) then
  success = -1
end if
!
! Close and release resources.
!
call h5aclose_f(attr_id , hdferr)
call HDFerror_check('HDF_addStringAttribute:h5aclose_f:'//trim(dataname), hdferr)

call h5sclose_f(aspace_id, hdferr)
call HDFerror_check('HDF_addStringAttribute:h5sclose_f:'//trim(dataname), hdferr)

! that's it

end function HDF_addStringAttributeToGroup


!--------------------------------------------------------------------------
!
! FUNCTION:HDF_getStringAttributeFromGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a string attribute from the current level in the HDF file 
!
!> @note Note that this routine uses fortran-2003 options
!
!> @param dataname dataset name (string)
!> @param fltval real 
!> @param HDF_head
!
!> @date 07/11/18  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function HDF_getStringAttributeFromGroup(dataname, stratt, slen, HDF_head) result(success)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_getStringAttributeFromGroup

use ISO_C_BINDING

character(fnlen),INTENT(IN)                             :: dataname
integer(SIZE_T),INTENT(IN)                              :: slen
character(len=slen, KIND=c_char),INTENT(INOUT)          :: stratt 
!f2py intent(in,out) ::  stratt 
type(HDFobjectStackType),INTENT(INOUT)                  :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg)                                       :: success

integer(HID_T)                                          :: aspace_id, filetype, atype_id, attr_id, memtype ! Handles
integer                                                 :: hdferr, rnk
integer(SIZE_T)                                         :: attrlen
INTEGER(HSIZE_T), DIMENSION(1:1)                        :: maxdims
INTEGER(hsize_t), DIMENSION(1:1)                        :: dims
CHARACTER(LEN=slen), DIMENSION(:), ALLOCATABLE, TARGET  :: rdata
INTEGER(SIZE_T)                                         :: size

INTEGER, DIMENSION(:), POINTER                          :: ptr_r 
TYPE(C_PTR)                                             :: f_ptr
  

dims(1) = slen

success = 0

! open the attribute for this group
  call h5aopen_f(HDF_head%next%objectID, cstringify(dataname), attr_id, hdferr)
  call HDFerror_check('HDF_getStringAttributeFromGroup:h5aopen_f:'//trim(dataname), hdferr)

  ! Get the datatype and its size.
  !
  CALL H5Aget_type_f(attr_id, filetype, hdferr)
  CALL H5Tget_size_f(filetype, size, hdferr)

  ! Make sure the declared length is large enough
  IF(size.GT.slen+1)THEN
     PRINT*,'ERROR:HDFsupport:HDF_getStringAttributeFromGroup Character LEN is too small'
     STOP
  ENDIF
  !
  ! Get dataspace and allocate memory for read buffer.
  ! 
  CALL H5Aget_space_f(attr_id, aspace_id, hdferr)
  CALL H5Sget_simple_extent_dims_f(aspace_id, dims, maxdims, hdferr)

  ALLOCATE(rdata(1:dims(1)))
  !
  ! Create the memory datatype.
  !
  CALL H5Tcopy_f(H5T_FORTRAN_S1, memtype, hdferr)
  CALL H5Tset_size_f(memtype, slen, hdferr)
  !
  ! Read the data.
  !
  f_ptr = C_LOC(rdata(1)(1:1))
  CALL H5Aread_f(attr_id, memtype, f_ptr, hdferr)
  stratt = trim(rdata(1))
  !
  ! Close and release resources.
  !
  CALL H5Aclose_f(attr_id, hdferr)
  CALL H5Sclose_f(aspace_id, hdferr)
  CALL H5Tclose_f(memtype, hdferr)

end function HDF_getStringAttributeFromGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_read2DImage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a gray scale image from the HDF5 file 
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine HDF_read2DImage(dataset, image, numx, numy, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_read2DImage

use error
use h5im
use h5lt

IMPLICIT NONE

character(fnlen),INTENT(IN)                         :: dataset
integer(kind=irg),INTENT(IN)                        :: numx
integer(kind=irg),INTENT(IN)                        :: numy
integer(kind=irg),INTENT(INOUT)                     :: image(numx,numy)
!f2py intent(in,out) ::  image
type(HDFobjectStackType)                            :: HDF_head

integer(kind=irg),allocatable                       :: vec(:)
integer(kind=irg)                                   :: hdferr

! read the image from the file
allocate(vec(numx*numy))
call h5imread_image_f(HDF_head%next%objectID,dataset,vec,hdferr)

! reorganize it into a regular image
image = reshape( vec, (/ numx, numy/) )

end subroutine HDF_read2DImage







end module HDFsupport
