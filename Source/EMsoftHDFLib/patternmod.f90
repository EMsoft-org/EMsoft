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
!> @todo add option to extract a rectangular sub-region of patterns from the input file
!
!> @date 02/13/18 MDG 1.0 original
!> @date 02/14/18 MDG 1.1 added old Binary, TSL .up2, and EMEBSD HDF5 formats
!> @date 02/15/18 MDG 1.2 added TSL and Bruker HDF formatted files
!> @date 02/20/18 MDG 1.3 added option to extract a single pattern from the input file
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

! this one is used to keep track of the even/odd patterns start locations in the .up2 input format
logical,save,private                                :: up2wdLeven
integer(kind=ill),save,private                      :: offset

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
!> @param L number of pixels in pattern; needed to initialize parameters for .up2 formatted input.
!> @param inputtype 
!> @param recsize  some formats need a record size
!> @param funit logical unit for reading
!> @param HDFstrings string array with group and dataset names for HDF5 input
!
!> @date 02/13/18 MDG 1.0 original
!> @date 02/15/18 MDG 1.1 added record length correction for windows platform
!> @date 02/15/18 MDG 1.2 added special handling for out-of-order patterns in the Bruker HDF5 format
!> @date 02/18/18 MDG 1.3 added special handling for patterns with odd number of pixels in .up2 format
!--------------------------------------------------------------------------
recursive function openExpPatternFile(filename, npat, L, inputtype, recsize, funit, HDFstrings) result(istat)
!DEC$ ATTRIBUTES DLLEXPORT :: openExpPatternFile

use io

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: filename
integer(kind=irg),INTENT(IN)            :: npat
integer(kind=irg),INTENT(IN)            :: L
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
        ! for wd * L odd, we need to take care of the proper starting bytes for each pattern
        ! so we check for wd * L odd here and initialize a flag
        if (mod(npat * L,2).eq.0) then
          up2wdLeven = .TRUE.
        else
          up2wdLeven = .FALSE.
        end if 
        open(unit=funit,file=trim(ename), &
            status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
        if (ierr.ne.0) then
            io_int(1) = ierr
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if
        offset = 4_ill

    case(4)  ! "OxfordBinary"
        call FatalError("openExpPatternFile","input format not yet implemented")


    case(5)  ! "OxfordHDF"
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

    case(3, 6)  ! "TSLHDF", "EMEBSD"
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
!> @param wd number of patterns in row in input file (may be different from requested number of patterns in ROI !)
!> @param patsz pattern dimension
!> @param L array size
!> @param dims3 array size for hyperslab reading
!> @param offset3 array offset for hyperslab reading
!> @param funit logical unit for reading
!> @param inputtype input file type identifier
!> @param HDFstrings string array with group and datset names for HDF5 input
!> @param exppatarray output array
!> @param ROI (OPTIONAL) region of interest parameters (lower left x,y; hor. and vert. dimensions)
!
!> @date 02/13/18 MDG 1.0 original
!> @date 02/21/18 MDG 1.1 added optional Region-of-Interest capability
!--------------------------------------------------------------------------
recursive subroutine getExpPatternRow(iii, wd, patsz, L, dims3, offset3, funit, inputtype, HDFstrings, exppatarray, ROI) 
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
integer(kind=irg),OPTIONAL,INTENT(IN)   :: ROI(4)

integer(kind=irg)                       :: itype, hdfnumg, ierr, ios
real(kind=sgl)                          :: imageexpt(L)
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=irg),allocatable           :: buffer(:)
integer(kind=ish),allocatable           :: pairs(:)
integer(kind=irg)                       :: sng, pixcnt
integer(kind=ish)                       :: pair(2)
integer(HSIZE_T)                        :: dims3new(3), offset3new(3), newspot
integer(kind=ill)                       :: recpos, ii, jj, kk, ispot, liii, lpatsz, lwd, lL, buffersize, kspot, jspot, &
                                           kkstart, kkend

itype = get_input_type(inputtype)
hdfnumg = get_num_HDFgroups(HDFstrings)
if (hdfnumg.gt.0) dataset = trim(HDFstrings(hdfnumg+1))

! are we dealing with a smaller ROI or the full field view ?
! we need to use ill-type integers since the numbers can get pretty large...
liii = iii
lwd = wd
lpatsz = patsz
lL = L
if (present(ROI)) then 
 kkstart = ROI(1)
 kkend = kkstart + ROI(3) - 1_ill 
 if ((itype.eq.2).and.(iii.eq.ROI(2))) then   ! make sure we do this only once ...
! we need to skip the first ROI(2)-1 rows (and potentially the first pattern as well)
   do ii=1,ROI(2)-1
      offset = offset + (lwd * lL) / 2_ill 
      if ((up2wdLeven.eqv..FALSE.).and.(mod(ii,2).eq.0)) then
        offset = offset + 1_ill
      end if
   end do
   offset = offset + lL / 2_ill
 end if
else
 kkstart = 1_ill
 kkend = dims3(3)
 if (itype.eq.2) then
! for the first row, we need to skip the first pattern completely...  needs to be verified
   if (iii.eq.1) offset = offset + lL / 2_ill
 end if
end if

select case (itype)
    case(1)  ! "Binary"  
! This is the original EMsoft binary format that we used initially for indexing runs 
! when the experimental patterns were only available in individual image file format. 
! This file would have been created using a Matlab or IDL routine.  We anticipate that 
! this format will not be used for much longer.
      do jj=kkstart,kkend
        read(funit,rec=(liii-1)*lwd + jj) imageexpt
        exppatarray((jj-kkstart)*patsz+1:(jj-1)*patsz+L) = imageexpt(1:L)
      end do

    case(2)  ! "TSLup2"  THIS MAY NOT WORK CORRECTLY ON WINDOWS DUE TO THE RECORD LENGTH BEING DIFFERENT !!!

! we need to be really really careful here because the .up2 file has 2-byte integers in it and they 
! run continuously with no separation between patterns; so, if a pattern has an odd number of pixels,
! then the next pattern will start in the middle of a 4-byte block... since we are reading things in
! multiples of 4 bytes (recl), that means that alternating patterns will begin at the start of the 
! 4-byte blocks or in the middle... Hence the somewhat convoluted code below which attempts to keep 
! track of where the current pattern starts (byte 1 or 3).
      buffersize = (lwd * lL) / 2_ill + 1_ill   ! +1 to allow for half record at the end.
      allocate(buffer(buffersize))
! first we read the entire buffer as 4-byte integers
      do ii=1_ill,buffersize
        read(unit=funit,rec=offset+ii, iostat=ios) buffer(ii)
      end do

! we convert the 4-byte integers into pairs of 2-byte integers
      allocate(pairs(2_ill*buffersize))
      pairs = transfer(buffer,pairs)
      if ((up2wdLeven.eqv..FALSE.).and.(mod(iii,2).eq.0)) then  ! shift the array by one entry to the left 
        pairs = cshift(pairs,1_ill)
      end if
      deallocate(buffer)

! then we need to place them in the exppatarray array with the proper offsets if patsz ne L 
      exppatarray = 0.0
      pixcnt = (kkstart-1)*dims3(1)*dims3(2)+1
      do kk=kkstart,kkend   ! loop over all the patterns in this row/ROI; remember to flip the patterns upside down !
        kspot = (kk-kkstart)*patsz
        do jj=1,dims3(2)
          jspot = (dims3(2)-jj)*dims3(1) 
          do ii=1,dims3(1)
            exppatarray(kspot+jspot+ii) = float(pairs(pixcnt))
            pixcnt = pixcnt + 1
          end do 
        end do 
      end do 

! increment the row offset parameter, taking into account any necessary shifts due to an odd value of wd*L
      offset = offset + (lwd * lL) / 2_ill 

      if ((up2wdLeven.eqv..FALSE.).and.(mod(iii,2).eq.0)) then
        offset = offset + 1_ill
      end if
      deallocate(pairs)

! finally, correct for the fact that the original values were unsigned integers
      where(exppatarray.lt.0.0) exppatarray = exppatarray + 65536.0

    case(4)  ! "OxfordBinary"

    case(5)  ! "OxfordHDF"
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

    case(3,6)  ! "TSLHDF" "EMEBSD" passed tests on 2/14/18 by MDG
! read a hyperslab section from the HDF5 input file
        EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppatarray = 0.0
        do kk=kkstart,kkend
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                    if (itype.eq.6) then
                      exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,kk)))
                    else  ! TSL patterns are upside down compared to the EMsoft convention...
                      exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,dims3(2)+1-jj,kk)))
                    end if 
                end do 
            end do 
        end do 

    case(7)  ! "BrukerHDF"  passed tests on 2/16/18 by MDG
! since the pattern order in the Bruker data file is not necessarily the order in which the patterns
! were acquired, we need to read each patttern separately from the file using the appropriate offset, which 
! is calculated using the semix and semiy arrays.  That means that we have to redefine both dims3 and offset3
! and loop over an entire row using the original pattern coordinate (ispot) as an index into the reordering arrays.
        exppatarray = 0.0
        dims3new = (/ dims3(1), dims3(2), 1_HSIZE_T /)
        do kk=kkstart,kkend  ! loop over all spots in the row/ROI
            ispot = (iii-1)*wd + kk
            newspot = semiy(ispot) * wd + semix(ispot)
            offset3new = (/ offset3(1), offset3(2),  newspot /)
            EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3new, dims3new, pmHDF_head) 
            do jj=1,dims3(2)
                do ii=1,dims3(1) 
                    ! Bruker patterns are stored upside down compared to the EMsoft convention...
                    exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,dims3(2)+1-jj,1)))
                end do 
            end do 
        end do 
    case default 
end select

end subroutine getExpPatternRow

!--------------------------------------------------------------------------
!
! SUBROUTINE: getSingleExpPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a single experimental pattern from the input file
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
!> @param exppat output array
!
!> @date 02/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getSingleExpPattern(iii, wd, patsz, L, dims3, offset3, funit, inputtype, HDFstrings, exppat) 
!DEC$ ATTRIBUTES DLLEXPORT :: getSingleExpPattern

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
real(kind=sgl),INTENT(INOUT)            :: exppat(patsz)

integer(kind=irg)                       :: itype, hdfnumg, ierr, ios
real(kind=sgl)                          :: imageexpt(L)
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=irg),allocatable           :: buffer(:)
integer(kind=ish),allocatable           :: pairs(:)
integer(kind=irg)                       :: sng, pixcnt
integer(kind=ish)                       :: pair(2)
integer(HSIZE_T)                        :: dims3new(3), offset3new(3), newspot
integer(kind=ill)                       :: recpos, ii, jj, kk, ispot, liii, lpatsz, lwd, lL, buffersize, kspot, jspot, l1, l2

itype = get_input_type(inputtype)
hdfnumg = get_num_HDFgroups(HDFstrings)
if (hdfnumg.gt.0) dataset = trim(HDFstrings(hdfnumg+1))

select case (itype)
    case(1)  ! "Binary"  
! This is the original EMsoft binary format that we used initially for indexing runs 
! when the experimental patterns were only available in individual image file format. 
! This file would have been created using a Matlab or IDL routine.  We anticipate that 
! this format will not be used for much longer.  To call the routine for a single pattern,
! simply place y*wd+x in the third entry of the offset3 array.
        read(funit,rec=offset3(3)) imageexpt
        exppat(1:L) = imageexpt(1:L)

    case(2)  ! "TSLup2"  THIS MAY NOT WORK CORRECTLY ON WINDOWS DUE TO THE RECORD LENGTH BEING DIFFERENT !!!
! tested and compared to IDL version of read_up2 routine on 2/20/18, MDG.
! the requested pattern should be in the encoded in the third entry of the offset3 array.
! we need to use ill-type integers since the numbers can get pretty large...
       lwd = wd
       lpatsz = patsz
       lL = L
       l1 = mod(offset3(3),wd) + 1_ill
       l2 = offset3(3)/wd + 1_ill
! we need to be really really careful here because the .up2 file has 2-byte integers in it and they 
! run continuously with no separation between patterns; so, if a pattern has an odd number of pixels,
! then the next pattern will start in the middle of a 4-byte block... since we are reading things in
! multiples of 4 bytes (recl), that means that alternating patterns will begin at the start of the 
! 4-byte blocks or in the middle... Hence the somewhat convoluted code below which attempts to keep 
! track of where the current pattern starts (byte 1 or 3).  Also, we are skipping the very first pattern
! in the file, so the initial offset parameter is 4 + first-pattern-size/2; special care is needed
! when pattern size and number of patterns in each row are both odd integers.

        offset = 4_ill + lL / 2_ill  ! initial offset
        offset = offset + ( (l2 * lwd + l1) * lL ) / 2_ill 
        buffersize = lL / 2_ill + 1_ill   ! +1 to allow for half record at the end.
        allocate(buffer(buffersize))
! ! first we read the entire buffer as 4-byte integers
        do ii=1_ill,buffersize
          read(unit=funit,rec=offset+ii, iostat=ios) buffer(ii)
        end do

! ! we convert the 4-byte integers into pairs of 2-byte integers
        allocate(pairs(2_ill*buffersize))
        pairs = transfer(buffer,pairs)
        if ((up2wdLeven.eqv..FALSE.).and.(mod(iii,2).eq.0)) then  ! shift the array by one entry to the left 
          pairs = cshift(pairs,1_ill)
        end if
        deallocate(buffer)

! ! then we need to place them in the exppatarray array with the proper offsets if patsz ne L 
        exppat = 0.0
        pixcnt = 1
        do jj=1,dims3(2)
          jspot = (dims3(2)-jj)*dims3(1) 
          do ii=1,dims3(1)
            exppat(jspot+ii) = float(pairs(pixcnt))
            pixcnt = pixcnt + 1
          end do 
        end do 
        deallocate(pairs)

! finally, correct for the fact that the original values were unsigned integers
        where(exppat.lt.0.0) exppat = exppat + 65536.0

    case(4)  ! "OxfordBinary"

    case(5)  ! "OxfordHDF"
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

    case(3,6)  ! "TSLHDF" "EMEBSD" passed tests on 2/20/18 by MDG
! read a hyperslab single pattern section from the HDF5 input file
! dims3 should have the pattern dimensions and then 1_HSIZE_T for the third dimension
! offset3 should have (0,0) and then the offset of the pattern (0-based)
        EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppat = 0.0
        do jj=1,dims3(2)
            do ii=1,dims3(1)
                if (itype.eq.6) then
                  exppat((jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,1)))
                else  ! TSL patterns are stored upside down compared to the EMsoft convention...
                  exppat((jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,dims3(2)+1-jj,1)))
                end if 
            end do 
        end do 

    case(7)  ! "BrukerHDF"  to be tested
! since the pattern order in the Bruker data file is not necessarily the order in which the patterns
! were acquired, we need to read each patttern separately from the file using the appropriate offset, which 
! is calculated using the semix and semiy arrays.  That means that we have to redefine both dims3 and offset3
! and use the original pattern coordinate (ispot) as an index into the reordering arrays.
        exppat = 0.0
        dims3new = (/ dims3(1), dims3(2), 1_HSIZE_T /)
        ispot = offset3(3)
        newspot = semiy(ispot) * wd + semix(ispot)
        offset3new = (/ offset3(1), offset3(2),  newspot /)
        EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3new, dims3new, pmHDF_head) 
        do jj=1,dims3(2)
            do ii=1,dims3(1) 
                ! Bruker patterns are stored upside down compared to the EMsoft convention...
                exppat((kk-1)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,dims3(2)+1-jj,1)))
            end do 
        end do 

    case default 

end select

end subroutine getSingleExpPattern


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