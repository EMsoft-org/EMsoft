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
!> @todo add options for TSL and Oxford individual pattern files (tiff, jpeg, bmp) to convert
!> folders of those pattern files into the same format; this will remove the need to externally
!> convert hundreds of thousands of pattern files into a single binary data file.
!
!> @date 02/13/18 MDG 1.0 original
!> @date 02/14/18 MDG 1.1 added old Binary, TSL .up2, and EMEBSD HDF5 formats
!> @date 02/15/18 MDG 1.2 added TSL and Bruker HDF formatted files
!--------------------------------------------------------------------------
module patternmod

use local
use error
use HDF5
use HDFsupport

IMPLICIT NONE


private :: get_input_type, get_num_HDFgroups

! the following two arrays are only used for the Bruker HDF5 format, since the order of the 
! EBSD patterns in the RawPatterns array is not necessarily the correct order !  We use these
! two index arrays to obtain the correct order ...
integer(kind=irg),allocatable,save,private          :: semix(:)
integer(kind=irg),allocatable,save,private          :: semiy(:)
integer(HSIZE_T),save,private                       :: semixydims(1)

type(HDFobjectStackType),pointer,save,private       :: pmHDF_head 

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

itype = -1

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
! FUNCTION: get_num_HDFgroups
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extract the number of HDF groups from the HDFstrings array
!
!> @param HDFstrings
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function get_num_HDFgroups(HDFstrings) result(numg)

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: HDFstrings(10)
integer(kind=irg)                       :: numg

integer(kind=irg)                       :: i 

numg = 0
do i=1,10 
  if (len(trim(HDFstrings(i))).gt.0) numg = numg+1
end do
numg = numg-1   ! the last one should be a data set name

end function get_num_HDFgroups

!--------------------------------------------------------------------------
!
! subroutine: invert_ordering_arrays
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief invert the pattern reordering arrays
!
!> @param npat number of patterns in a single row of the ROI
!
!> @date 02/16/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine invert_ordering_arrays(npat) 

IMPLICIT NONE

integer(kind=irg),INTENT(IN)       :: npat

integer(kind=irg),allocatable      :: semixnew(:), semiynew(:)
integer(kind=irg)                  :: i, ix, iy, ipos

! allocate the new reordering arrays
allocate(semixnew(semixydims(1)), semiynew(semixydims(1)))

! invert the coordinate arrays  [tested on 2/16/18, MDG]
do i=1,semixydims(1)
  ix = mod(i, npat)-1
  iy = i/npat
  if (ix.lt.0) then
    ix = npat-1
    iy = iy-1
  end if
  ipos = semiy(i) * npat + semix(i) + 1
  semixnew(ipos) = ix
  semiynew(ipos) = iy
end do 

! copy the new arrays over the old ones
semix = semixnew
semiy = semiynew
deallocate(semixnew, semiynew)

end subroutine invert_ordering_arrays

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
!> @param HDFstrings string array with group and dataset names for HDF5 input
!
!> @date 02/13/18 MDG 1.0 original
!> @date 02/15/18 MDG 1.1 added record length correction for windows platform
!> @date 02/15/18 MDG 1.2 added special handling for out-of-order patterns in the Bruker HDF5 format
!--------------------------------------------------------------------------
recursive function openExpPatternFile(filename, npat, inputtype, recsize, funit, HDFstrings) result(istat)
!DEC$ ATTRIBUTES DLLEXPORT :: openExpPatternFile

use io

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: filename
integer(kind=irg),INTENT(IN)            :: npat
character(fnlen),INTENT(IN)             :: inputtype
integer(kind=irg),INTENT(IN)            :: recsize
integer(kind=irg),INTENT(IN)            :: funit
integer(kind=irg)                       :: istat
character(fnlen),INTENT(IN)             :: HDFstrings(10)

character(fnlen)                        :: ename
integer(kind=irg)                       :: i, ierr, io_int(1), itype, hdferr, hdfnumg, recordsize
character(fnlen)                        :: groupname, dataset, platform

istat = 0

! first determine how many HDFgroups there are; the last entry in HDFstrings should be the data set name
hdfnumg = get_num_HDFgroups(HDFstrings)
itype = get_input_type(inputtype)

ename = trim(EMsoft_getEMdatapathname())//trim(filename)
ename = EMsoft_toNativePath(ename)
call Message('Pattern input file '//trim(ename))
call Message('  input file type '//trim(inputtype))

platform = EMsoft_getEMsoftplatform()

! depending on the inputtype, we open the input file in the appropriate way
select case (itype)
    case(1)  ! "Binary"
        if (trim(platform).eq.'Windows') then
            recordsize = recsize/4  ! windows record length is in units of 4 bytes
        else
            recordsize = recsize    ! all other platforms use record length in units of bytes
        end if
        open(unit=funit,file=trim(ename),&
            status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
        if (ierr.ne.0) then
            io_int(1) = ierr
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if

    case(2)  ! "TSLup2"
        if (trim(platform).eq.'Windows') then
            recordsize = 1    ! windows record length is in units of 4 bytes
        else
            recordsize = 4    ! all other platforms use record length in units of bytes
        end if
        open(unit=funit,file=trim(ename), &
            status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
        if (ierr.ne.0) then
            io_int(1) = ierr
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if

    case(4)  ! "OxfordBinary"
        call FatalError("openExpPatternFile","input format not yet implemented")

    case(3, 5:6)  ! "TSLHDF", "OxfordHDF", "EMEBSD"
        nullify(pmHDF_head)
        call h5open_EMsoft(hdferr)
        ! open the file
        hdferr =  HDF_openFile(ename, pmHDF_head)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
        ! open all the groups to the correct level of the data set
        do i=1,hdfnumg
            groupname = trim(HDFstrings(i))
            hdferr = HDF_openGroup(groupname, pmHDF_head)
            if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup: group name issue, check for typos ...')
        end do
        ! and here we leave this file open so that we can read data blocks using the hyperslab mechanism;
        ! we can do this because the pmHDF_head pointer is private and has SAVE status for this entire module

    case(7)  !  "BrukerHDF"
        nullify(pmHDF_head)
        call h5open_EMsoft(hdferr)
        ! open the file
        hdferr =  HDF_openFile(ename, pmHDF_head)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')

        ! open all the groups to the correct level of the data set
        do i=1,hdfnumg
            groupname = trim(HDFstrings(i))
            hdferr = HDF_openGroup(groupname, pmHDF_head)
            if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup: group name issue, check for typos ...')
            !  this part is different from the other vendors: the patterns are not necessarily in the correct order (why not???)
            !  so we need to read the reordering arrays here...  The reordering arrays are always in the SEM group,
            !  which is one level down from the top (i.e., where we are right now).  Both arrays have the SAVE attribute.
            if (i.eq.1) then 
               groupname = 'SEM'
               hdferr = HDF_openGroup(groupname, pmHDF_head)
               dataset = 'SEM IX'
               call HDF_readDatasetIntegerArray1D(dataset, semixydims, pmHDF_head, hdferr, semix)
               if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetIntegerArray1D: problem reading SEM IX array')
               dataset = 'SEM IY'
               call HDF_readDatasetIntegerArray1D(dataset, semixydims, pmHDF_head, hdferr, semiy)
               if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetIntegerArray1D: problem reading SEM IY array')
               call invert_ordering_arrays(npat)
               call Message('  found pattern reordering arrays')
               ! and leave this group
               call HDF_pop(pmHDF_head)
            end if
        end do
        ! and here we leave this file open so that we can read data blocks using the hyperslab mechanism;
        ! we can do this because the pmHDF_head pointer is private and has SAVE status for this entire module

    case default 
        istat = -1

end select

end function openExpPatternFile


!--------------------------------------------------------------------------
!
! SUBROUTINE: getExpPatternRow
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a single row of patterns from the input file(s)
!
!> @param iii row number
!> @param wd number of patterns in row
!> @param patsz pattern dimension
!> @param L array size
!> @param dims3 array size for hyperslab reading
!> @param offset3 array offset for hyperslab reading
!> @param funit logical unit for reading
!> @param inputtype input file type identifier
!> @param HDFstrings string array with group and datset names for HDF5 input
!> @param exppatarray output array
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getExpPatternRow(iii, wd, patsz, L, dims3, offset3, funit, inputtype, HDFstrings, exppatarray) 
!DEC$ ATTRIBUTES DLLEXPORT :: getExpPatternRow

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: iii
integer(kind=irg),INTENT(IN)            :: wd
integer(kind=irg),INTENT(IN)            :: patsz
integer(kind=irg),INTENT(IN)            :: L
integer(HSIZE_T),INTENT(IN)             :: dims3(3)
integer(HSIZE_T),INTENT(IN)             :: offset3(3)
integer(kind=irg),INTENT(IN)            :: funit
character(fnlen),INTENT(IN)             :: inputtype
character(fnlen),INTENT(IN)             :: HDFstrings(10)
real(kind=sgl),INTENT(INOUT)            :: exppatarray(patsz * wd)

integer(kind=irg)                       :: ii, jj, kk, itype, hdfnumg, offset, ispot
real(kind=sgl)                          :: imageexpt(L)
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=irg)                       :: sng 
integer(kind=ish)                       :: pair(2)
integer(HSIZE_T)                        :: dims3new(3), offset3new(3), newspot


itype = get_input_type(inputtype)
hdfnumg = get_num_HDFgroups(HDFstrings)
if (hdfnumg.gt.0) dataset = trim(HDFstrings(hdfnumg+1))

select case (itype)
    case(1)  ! "Binary"
      do jj=1,wd
        read(funit,rec=(iii-1)*wd + jj) imageexpt
        exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = imageexpt(1:L)
      end do

    case(2)  ! "TSLup2"
      offset = 4 + (iii-1) * wd * patsz / 2
      do kk=1,dims3(3)
        do jj=1,dims3(2)
          do ii=1,dims3(1)/2
            read(unit=funit,rec=offset + (kk-1)*patsz/2 + (jj-1)*dims3(1)/2 + ii) sng
            pair = transfer(sng,pair)  ! transform from 4-byte value to two short integers
            exppatarray((kk-1)*patsz+(jj-1)*dims3(1)+2*ii-1) = float(pair(1))
            exppatarray((kk-1)*patsz+(jj-1)*dims3(1)+2*ii) = float(pair(2))
          end do 
        end do 
      end do 
      ! correct for the fact that the original values were unsigned integers
      where(exppatarray.lt.0.0) exppatarray = exppatarray + 65536.0

    case(4)  ! "OxfordBinary"

    case(5)  ! "OxfordHDF"

    case(3,6)  ! "TSLHDF" "EMEBSD" passed tests on 2/14/18 by MDG
! read a hyperslab section from the HDF5 input file
        EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppatarray = 0.0
        do kk=1,dims3(3)
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                    exppatarray((kk-1)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,kk)))
                end do 
            end do 
        end do 

    case(7)  ! "BrukerHDF"  passed tests on 2/15/18 by MDG
! since the pattern order in the Bruker data file is not necessarily the correct order in which the patterns
! were acquired, we need to read each patttern separately from the file using the appropriate offset, which 
! is calculated using the semix and semiy arrays.  That means that we have to redefine both dims3 and offset3
! and loop over an entire row using the original pattern coordinate (ispot) as an index into the reordering arrays.
        exppatarray = 0.0
        dims3new = (/ dims3(1), dims3(2), 1_HSIZE_T /)
        do kk=1,dims3(3)  ! loop over all spots in the row
            ispot = (iii-1)*wd + kk
            newspot = semiy(ispot) * wd + semix(ispot)
            offset3new = (/ offset3(1), offset3(2),  newspot /)
            EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3new, dims3new, pmHDF_head) 
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                    exppatarray((kk-1)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,1)))
                end do 
            end do 
        end do 
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
!> @param funit some formats need a logical unit number
!
!> @date 02/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine closeExpPatternFile(inputtype, funit) 
!DEC$ ATTRIBUTES DLLEXPORT :: closeExpPatternFile

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: inputtype
integer(kind=irg),INTENT(IN)            :: funit

integer(kind=irg)                       :: itype, hdferr

itype = get_input_type(inputtype)

select case (itype)
    case(1)  ! "Binary"
        close(unit=funit,status='keep')

    case(2)  ! "TSLup2"
        close(unit=funit,status='keep')

    case(3, 6)  ! "TSLHDF" "EMEBSD"
        call HDF_pop(pmHDF_head,.TRUE.)
        call h5close_EMsoft(hdferr)
        nullify(pmHDF_head)

    case(4)  ! "OxfordBinary"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(5)  ! "OxfordHDF"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(7)  !  "BrukerHDF"
        call HDF_pop(pmHDF_head,.TRUE.)
        call h5close_EMsoft(hdferr)
        nullify(pmHDF_head)
        deallocate(semix, semiy)

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
        call Message("Error code -1: unknown input format")
    case default
        io_int(1) = istat
        call WriteValue("unknown error code ",io_int,1)
end select

end subroutine patternmod_errormessage



end module patternmod