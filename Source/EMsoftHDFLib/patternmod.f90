! ###################################################################
! Copyright (c) 2018, Marc De Graef/Carnegie Mellon University
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
! EMsoft:patternmod.f90
!--------------------------------------------------------------------------
!
! MODULE: patternmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief a variety of routines to read various experimental diffaction pattern file formats
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
module patternmod

use local
use error
use HDF5
use HDFsupport

IMPLICIT NONE

private :: get_input_type

contains

!--------------------------------------------------------------------------
!
! FUNCTION: get_input_type
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert inputtype string to integer value
!
!> @param inputtype 
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function get_input_type(inputtype) result(itype)

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: inputtype
integer(kind=irg)                       :: itype

itype = 0

if (trim(inputtype).eq."Binary") itype = 1
if (trim(inputtype).eq."TSLup2") itype = 2
if (trim(inputtype).eq."TSLHDF") itype = 3
if (trim(inputtype).eq."OxfordBinary") itype = 4
if (trim(inputtype).eq."OxfordHDF") itype = 5
if (trim(inputtype).eq."EMEBSD") itype = 6
if (trim(inputtype).eq."BrukerHDF") itype = 7

end function get_input_type

!--------------------------------------------------------------------------
!
! FUNCTION: openExpPatternFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief open a file with experimental patterns for a given input file type
!
!> @param filename 
!> @param npat number of patterns per row to extract
!> @param inputtype 
!> @param recsize  some formats need a record size
!> @param funit logical unit for reading
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function openExpPatternFile(filename, npat, inputtype, recsize, funit) result(istat)
!DEC$ ATTRIBUTES DLLEXPORT :: openExpPatternFile

use io

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: filename
integer(kind=irg),INTENT(IN)            :: npat
character(fnlen),INTENT(IN)             :: inputtype
integer(kind=irg),INTENT(IN)            :: recsize
integer(kind=irg),INTENT(IN)            :: funit
integer(kind=irg)                       :: istat

character(fnlen)                        :: ename
integer(kind=irg)                       :: ierr, io_int(1), itype

! depending on the inputtype, we open the input file in the appropriate way
ename = trim(EMsoft_getEMdatapathname())//trim(filename)
ename = EMsoft_toNativePath(ename)

itype = get_input_type(inputtype)

select case (itype)
    case(1)  ! "Binary"
        open(unit=funit,file=trim(ename),&
            status='old',form='unformatted',access='direct',recl=recsize,iostat=ierr)
        if (ierr.ne.0) then
            io_int(1) = ierr
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if

    case(2)  ! "TSLup2"

    case(3)  ! "TSLHDF"

    case(4)  ! "OxfordBinary"

    case(5)  ! "OxfordHDF"

    case(6)  ! "EMEBSD"

    case(7)  ! "BrukerHDF"

    case default 
end select

end function openExpPatternFile


!--------------------------------------------------------------------------
!
! SUBROUTINE: getExpPatternRow
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a single row of patterns from the input file
!
!> @param iii row number
!> @param wd number of patterns in row
!> @param patsz pattern dimension
!> @param L array size
!> @param funit logical unit for reading
!> @param inputtype input file type identifier
!> @param exppatarray output array
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getExpPatternRow(iii, wd, patsz, L, funit, inputtype, exppatarray) 
!DEC$ ATTRIBUTES DLLEXPORT :: getExpPatternRow

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: iii
integer(kind=irg),INTENT(IN)            :: wd
integer(kind=irg),INTENT(IN)            :: patsz
integer(kind=irg),INTENT(IN)            :: L
integer(kind=irg),INTENT(IN)            :: funit
character(fnlen),INTENT(IN)             :: inputtype
real(kind=sgl),INTENT(INOUT)            :: exppatarray(patsz * wd)

integer(kind=irg)                       :: jj, itype
real(kind=sgl)                          :: imageexpt(L)

itype = get_input_type(inputtype)

select case (itype)
    case(1)  ! "Binary"
      do jj=1,wd
        read(funit,rec=(iii-1)*wd + jj) imageexpt
        exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = imageexpt(1:L)
      end do

    case(2)  ! "TSLup2"

    case(3)  ! "TSLHDF"

    case(4)  ! "OxfordBinary"

    case(5)  ! "OxfordHDF"

    case(6)  ! "EMEBSD"

    case(7)  ! "BrukerHDF"

    case default 
end select

end subroutine getExpPatternRow


!--------------------------------------------------------------------------
!
! SUBROUTINE: closeExpPatternFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief close a file with experimental patterns for a given input file type
!
!> @param inputtype 
!> @param recsize  some formats need a record size
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine closeExpPatternFile(inputtype, funit) 
!DEC$ ATTRIBUTES DLLEXPORT :: closeExpPatternFile

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: inputtype
integer(kind=irg),INTENT(IN)            :: funit

integer(kind=irg)                       :: itype

itype = get_input_type(inputtype)

select case (itype)
    case(1)  ! "Binary"
        close(unit=funit,status='keep')

    case(2)  ! "TSLup2"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(3)  ! "TSLHDF"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(4)  ! "OxfordBinary"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(5)  ! "OxfordHDF"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(6)  ! "EMEBSD"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(7)  ! "BrukerHDF"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case default 
        call FatalError("closeExpPatternFile","unknown input format")
end select

end subroutine closeExpPatternFile



!--------------------------------------------------------------------------
!
! subroutine: patternmod_errormessage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print error message 
!
!> @param istat error code
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine patternmod_errormessage(istat)
!DEC$ ATTRIBUTES DLLEXPORT :: patternmod_errormessage

use io 

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: istat
integer(kind=irg)                   :: io_int(1)

select case (istat)
    case(-1)
        call Message("Error code -1: ")
    case default
        io_int(1) = istat
        call WriteValue("unknown error code ",io_int,1)
end select

end subroutine patternmod_errormessage



end module patternmod