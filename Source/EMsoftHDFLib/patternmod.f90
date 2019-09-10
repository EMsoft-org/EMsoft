! ###################################################################
! Copyright (c) 2018-2019, Marc De Graef Research Group/Carnegie Mellon University
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
!> @date 04/01/18 MDG 2.0 added routine to preprocess EBSD patterns 
!> @date 05/09/18 MDG 2.1 added .up1 TSL format 
!> @date 11/30/18 MDG 2.2 added suport for TKD pattern preprocessing
!> @date 02/19/19 MDG 3.0 corrects pattern orientation; manual indexing of patterns computed with EMEBSD
!>                        revealed an unwanted upside down flip that was compensated by flipping the 
!>                        exp. patterns; thus, all indexing runs thus far produced the correct results.
!> @date 07/13/19 MDG 3.1 added option to read single pattern from OxfordBinary file
!> @date 08/20/19 MDG 3.2 added vendor pattern center conversion function [for EMSphInx indexing program]
!--------------------------------------------------------------------------
module patternmod

use local
use error
use HDF5
use HDFsupport
use commonmod

IMPLICIT NONE


private :: get_input_type, get_num_HDFgroups

! the following two arrays are only used for the Bruker HDF5 format, since the order of the 
! EBSD patterns in the RawPatterns array is not necessarily the correct order !  We use these
! two index arrays to obtain the correct order ...
integer(kind=irg),allocatable,save,private          :: semix(:)
integer(kind=irg),allocatable,save,private          :: semiy(:)
integer(HSIZE_T),save,private                       :: semixydims(1)

! this one is used to keep track of the even/odd patterns start locations in the .up1 and .up2 input formats
logical,save,private                                :: up1wdLeven, up1halfshift
logical,save,private                                :: up2wdLeven, up2halfshift
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
if (trim(inputtype).eq."TSLup1") itype = 2
if (trim(inputtype).eq."TSLup2") itype = 3
if (trim(inputtype).eq."TSLHDF") itype = 4
if (trim(inputtype).eq."OxfordBinary") itype = 5
if (trim(inputtype).eq."OxfordHDF") itype = 6    ! to be implemented
if (trim(inputtype).eq."EMEBSD") itype = 7
if (trim(inputtype).eq."BrukerHDF") itype = 8

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
!> @brief invert the pattern reordering arrays for Bruker HDF5 format
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
!> @date 04/12/18 MDG 1.4 added option for patterns that don't start at record boundaries in up2 format
!> @date 05/10/18 MDG 1.5 changed .up2 access mode to STREAM to facilitate record positioning
!> @date 06/21/18 SS  1.6 changed recorsize to L*4 instead of recsize (correctsize*4); recsize has padded 0's
!> @date 05/01/19 MA  1.7 add support for Oxford Instruments binary pattern files
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
character(fnlen),INTENT(IN)             :: HDFstrings(10)

character(fnlen)                        :: ename
integer(kind=irg)                       :: i, ierr, io_int(1), itype, hdferr, hdfnumg, recordsize, up2header(4), &
                                           ios, up1header(4), version, patx, paty, myoffset, istat
character(fnlen)                        :: groupname, dataset, platform
logical                                 :: f_exists

istat = 0

! first determine how many HDFgroups there are; the last entry in HDFstrings should be the data set name
hdfnumg = get_num_HDFgroups(HDFstrings)
itype = get_input_type(inputtype)

ename = trim(EMsoft_getEMdatapathname())//trim(filename)
ename = EMsoft_toNativePath(ename)

f_exists = .FALSE.
inquire(file=trim(ename), exist=f_exists)

if (.not.f_exists) then
   call Message(' Input file '//trim(ename)//' does not exist in this location ... ')
   call Message(' Please check the input parameters in the namelist file.')
   call Message(' ')
   call FatalError('openExpPatternFile','Unrecoverable error; file not found')
end if

call Message('Pattern input file '//trim(ename))
call Message('  input file type '//trim(inputtype))

platform = EMsoft_getEMsoftplatform()

! depending on the inputtype, we open the input file in the appropriate way
select case (itype)
    case(1)  ! "Binary"
        ! if (trim(platform).eq.'Windows') then
        !     recordsize = L        ! windows record length is in units of 4 bytes
        ! else
            recordsize = L*4      ! all other platforms use record length in units of bytes
        ! end if
        open(unit=funit,file=trim(ename),&
            status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
        if (ierr.ne.0) then
            io_int(1) = ierr
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if

    case(2,3)  ! "TSLup1", TSLup2"
        ! open the file in STREAM access mode to allow for byte-level access
        open(unit=funit,file=trim(ename), status='old',access='stream',iostat=ios)
        if (ios.ne.0) then
            io_int(1) = ios
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if
        ! the first four 4-byte entries form a header with a version number (unimportant), then 
        ! the two dimensions of patterns, and finally an offset parameter indicating at which byte
        ! the first pattern starts.  We don't really need the other parameters, but we'll read them anyway.
        read(unit=funit, iostat=ios) version, patx, paty, myoffset
        offset = myoffset + 1_ill
        if (ios.ne.0) then
            io_int(1) = ios
            call WriteValue("Read error in .up1/2 file header",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if

    case(5)  ! "OxfordBinary"
        ! open the file in STREAM access mode to allow for byte-level access
        open(unit=funit,file=trim(ename), status='old',access='stream',iostat=ios)
        if (ios.ne.0) then
            io_int(1) = ios
            call WriteValue("File open error; error type ",io_int,1)
            call FatalError("openExpPatternFile","Cannot continue program")
        end if

    case(6)  ! "OxfordHDF"
        call FatalError("openExpPatternFile","OxfordHDF input format not yet implemented")
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

    case(4, 7)  ! "TSLHDF", "EMEBSD"
        nullify(pmHDF_head)
        ! open the file
        hdferr =  HDF_openFile(ename, pmHDF_head, readonly=.TRUE.)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
        ! open all the groups to the correct level of the data set
        do i=1,hdfnumg
            groupname = trim(HDFstrings(i))
            hdferr = HDF_openGroup(groupname, pmHDF_head)
            if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup: group name issue, check for typos ...')
        end do
        ! and here we leave this file open so that we can read data blocks using the hyperslab mechanism;
        ! we can do this because the pmHDF_head pointer is private and has SAVE status for this entire module

    case(8)  !  "BrukerHDF"
        nullify(pmHDF_head)
        ! open the file
        hdferr =  HDF_openFile(ename, pmHDF_head, readonly=.TRUE.)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')

        ! open all the groups to the correct level of the data set
        do i=1,hdfnumg
            groupname = trim(HDFstrings(i))
            hdferr = HDF_openGroup(groupname, pmHDF_head)
            if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup: group name issue, check for typos ...')
            !  this part is different from the other vendors: the patterns are not necessarily in the correct order 
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
!> @date 04/12/18 MDG 1.2 added option for patterns that don't start at record boundaries in up2 format
!> @date 05/10/18 MDG 1.3 completely reworked up1 and up2 reading by switching to STREAM access instead of DIRECT access
!> @date 03/21/19 MDG 1.4 fixed off-by-one error in column labels for up1 and up2 file formats
!> @date 05/01/19 MA  1.5 add support for Oxford Instruments binary pattern files
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
real(kind=sgl)                          :: imageexpt(L), z
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=C_INT16_T),allocatable     :: EBSDpatint(:,:,:)
character(1),allocatable                :: buffer(:)
integer(kind=ish),allocatable           :: pairs(:)
integer(kind=irg)                       :: sng, pixcnt
integer(kind=ish)                       :: pair(2)
integer(HSIZE_T)                        :: dims3new(3), offset3new(3), newspot
integer(kind=ill)                       :: recpos, ii, jj, kk, ispot, liii, lpatsz, lwd, lL, buffersize, kspot, jspot, &
                                           kkstart, kkend, multfactor
integer(kind=8)                         :: patoffsets(wd)

itype = get_input_type(inputtype)
hdfnumg = get_num_HDFgroups(HDFstrings)
if (hdfnumg.gt.0) dataset = trim(HDFstrings(hdfnumg+1))

! are we dealing with a smaller ROI or the full field view ?
! we need to use ill-type integers since the numbers can get pretty large...
liii = iii
lwd = wd
lpatsz = patsz
lL = L
if (itype.eq.2) multfactor = 1_ill
if (itype.eq.3) multfactor = 2_ill

if (present(ROI)) then 
 kkstart = ROI(1)
 kkend = kkstart + ROI(3) - 1_ill 
! for the TSL up1 and up2 formats we need to skip the first ROI(2)-1 
! rows and set the correct offset value (in bytes) 
 if (((itype.eq.2).or.(itype.eq.3)).and.(iii.eq.ROI(2))) then   ! make sure we do this only once ...
   do ii=1,ROI(2)-1
     offset = offset + (lwd * lL) * multfactor   ! this is in units of bytes
   end do
 end if
else
 kkstart = 1_ill
 kkend = dims3(3)
end if

select case (itype)
    case(1)  ! "Binary"  
! This is the original EMsoft binary format that we used initially for indexing runs 
! when the experimental patterns were only available in individual image file format. 
! This file would have been created using a Matlab or IDL routine.  We anticipate that 
! this format will not be used for much longer.
! In view of the pattern flip resolution, the user must ensure that the Matlab script
!  DOES NOT flip the pattern upside down !
      do jj=kkstart,kkend
        read(funit,rec=(liii-1)*lwd + jj) imageexpt
        exppatarray((jj-kkstart)*patsz+1:(jj-1)*patsz+L) = imageexpt(1:L)
      end do

    case(2,3)  ! "TSLup1", TSLup2"  
! up1 file has single bytes as entries, up2 has 2-byte unsigned integers
      ! generate a buffer of the correct size ... 
      buffersize = (lwd * lL) * multfactor
      allocate(buffer(buffersize))
! first we read the entire buffer as bytes
      read(unit=funit, pos=offset, iostat=ios) buffer

! then we convert the byte values into single byte or 2-byte integers 
      if (multfactor.eq.2_ill) then ! .up2 format
        allocate(pairs(buffersize/2_ill))
        pairs = transfer(buffer,pairs)
      else ! .up1 format
        allocate(pairs(buffersize))
        do jj=1_ill,buffersize
         pairs(jj) = ichar(buffer(jj)) 
        end do
      end if
      deallocate(buffer)

! then we need to place them in the exppatarray array 
      exppatarray = 0.0
      pixcnt = (kkstart-1)*dims3(1)*dims3(2)+1
      do kk=kkstart,kkend   ! loop over all the patterns in this row/ROI
        kspot = (kk-kkstart)*patsz
        do jj=1,dims3(2)
          jspot = (jj-1)*dims3(1) 
          do ii=1,dims3(1)
            exppatarray(kspot+jspot+ii) = float(pairs(pixcnt))
            pixcnt = pixcnt + 1
          end do 
        end do 
      end do 

! increment the row offset parameter (in bytes)
      offset = offset + (lwd * lL) * multfactor
      deallocate(pairs)

! finally, correct for the fact that the original values were unsigned integers
      if (itype.eq.3) then
        where(exppatarray.lt.0.0) exppatarray = exppatarray + 65536.0
      else
        where(exppatarray.lt.0.0) exppatarray = exppatarray + 256.0
      end if

    case(5)  ! "OxfordBinary"

! read position of patterns in file for a single row from the header
      read(unit=funit, pos=(liii-1)*lwd*8+9, iostat=ios) patoffsets

! generate a buffer to load individual patterns into
      buffersize = lL
      allocate(buffer(buffersize))

! allocate pairs to store all patterns in a row
      buffersize = lwd * lL
      allocate(pairs(buffersize))

      do ii=1,lwd
! read each pattern into buffer with the 16 bytes of metadata skipped
        read(unit=funit, pos=patoffsets(ii)+17_8, iostat=ios) buffer

! loop over pixels and convert the byte values into single byte integers
        do jj=1_ill,lL
          pairs((ii - 1)*lL + jj) = ichar(buffer(jj)) 
        enddo
      end do

      deallocate(buffer)

 ! then we need to place them in the exppatarray array 
      exppatarray = 0.0
      pixcnt = (kkstart-1)*dims3(1)*dims3(2)+1
      do kk=kkstart,kkend   ! loop over all the patterns in this row/ROI
        kspot = (kk-kkstart)*patsz
        do jj=1,dims3(2)
          jspot = (jj-1)*dims3(1) 
          do ii=1,dims3(1)
            exppatarray(kspot+jspot+ii) = float(pairs(pixcnt))
            pixcnt = pixcnt + 1
          end do 
        end do 
      end do 

      deallocate(pairs)

! finally, correct for the fact that the original values were unsigned integers
      where(exppatarray.lt.0.0) exppatarray = exppatarray + 256.0

    case(6)  ! "OxfordHDF"
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

    case(4)  ! "TSLHDF" passed tests on 2/14/18 by MDG
! read a hyperslab section from the HDF5 input file
        EBSDpatint = HDF_readHyperslabIntegerArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppatarray = 0.0
        do kk=kkstart,kkend
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                   z = float(EBSDpatint(ii,jj,kk))
                   if (z.lt.0.0) z = z+2.0**16
                   exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = z
                end do 
            end do 
        end do 


    case(7)  ! "EMEBSD" passed tests on 2/14/18 by MDG
! read a hyperslab section from the HDF5 input file
        EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppatarray = 0.0
        do kk=kkstart,kkend
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                      exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,kk)))
                end do 
            end do 
        end do 


    case(8)  ! "BrukerHDF"  passed tests on 2/16/18 by MDG
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
                    exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,1)))
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
!> @date 07/13/19 MDG 1.1 added option to read single pattern from OxfordBinary file
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
real(kind=sgl)                          :: imageexpt(L), z
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=C_INT16_T),allocatable     :: EBSDpatint(:,:,:)
character(1),allocatable                :: buffer(:)
integer(kind=ish),allocatable           :: pairs(:)
integer(kind=irg)                       :: sng, pixcnt
integer(kind=ish)                       :: pair(2)
integer(HSIZE_T)                        :: dims3new(3), offset3new(3), newspot
integer(kind=ill)                       :: recpos, ii, jj, kk, ispot, liii, lpatsz, lwd, lL, buffersize, kspot, jspot, &
                                           l1, l2, multfactor
integer(kind=8)                         :: patoffsets(wd)

itype = get_input_type(inputtype)
hdfnumg = get_num_HDFgroups(HDFstrings)
if (hdfnumg.gt.0) dataset = trim(HDFstrings(hdfnumg+1))

if (itype.eq.2) multfactor = 1_ill
if (itype.eq.3) multfactor = 2_ill

select case (itype)
    case(1)  ! "Binary"  
! This is the original EMsoft binary format that we used initially for indexing runs 
! when the experimental patterns were only available in individual image file format. 
! This file would have been created using a Matlab or IDL routine.  We anticipate that 
! this format will not be used for much longer.  To call the routine for a single pattern,
! simply place y*wd+x in the third entry of the offset3 array.
        read(funit,rec=offset3(3)) imageexpt
        exppat(1:L) = imageexpt(1:L)


    case(2,3)  ! "TSL_up1", TSLup2" 
! tested and compared to IDL version of read_up2 routine on 2/20/18, MDG.
! the requested pattern should be in the encoded in the third entry of the offset3 array.
! we need to use ill-type integers since the numbers can get pretty large...
        lwd = wd
        lpatsz = patsz
        lL = L
        l1 = offset3(3)
        if (itype.eq.2) then
         multfactor = 1_ill
        else
         multfactor = 2_ill
        end if

        offset = offset + (l1-1_ill) * lpatsz * multfactor
        buffersize = lpatsz * multfactor
        allocate(buffer(buffersize))
        read(unit=funit, pos=offset, iostat=ios) buffer

! then we convert the byte values into single byte or 2-byte integers 
        if (multfactor.eq.2_ill) then ! .up2 format
          allocate(pairs(buffersize/2_ill))
          pairs = transfer(buffer,pairs)
        else ! .up1 format
          allocate(pairs(buffersize))
          do jj=1_ill,buffersize
           pairs(jj) = ichar(buffer(jj)) 
          end do
        end if
        deallocate(buffer)

! ! then we need to place them in the exppatarray array with the proper offsets if patsz ne L 
        exppat = 0.0
        pixcnt = 1
        do jj=1,dims3(2)
          jspot = (jj-1)*dims3(1) 
          do ii=1,dims3(1)
            exppat(jspot+ii) = float(pairs(pixcnt))
            pixcnt = pixcnt + 1
          end do 
        end do 
        deallocate(pairs)

! finally, correct for the fact that the original values were unsigned integers
        if (itype.eq.3) then
          where(exppat.lt.0.0) exppat = exppat + 65536.0
        else
          where(exppat.lt.0.0) exppat = exppat + 256.0
        end if

    case(5)  ! "OxfordBinary" ! [added/tested MDG 07/13/19]
! read position of patterns in file for a single row from the header
      liii = iii
      l1 = mod(offset3(3),wd)
      lL = L
      lwd = wd
      read(unit=funit, pos=(liii-1)*lwd*8+9, iostat=ios) patoffsets

! generate buffers to load individual pattern into
      buffersize = lL
      allocate(buffer(buffersize), pairs(buffersize))

! read single pattern into buffer with the 16 bytes of metadata skipped
      read(unit=funit, pos=patoffsets(l1)+17_8, iostat=ios) buffer

! convert the byte values into single byte integers
      pairs = ichar(buffer) 
      deallocate(buffer)

 ! then we need to place it in the exppat array 
      exppat = 0.0
      pixcnt = 1
      do jj=1,dims3(2)
        jspot = (jj-1)*dims3(1) 
        do ii=1,dims3(1)
          exppat(jspot+ii) = float(pairs(pixcnt))
          pixcnt = pixcnt + 1
        end do 
      end do 
      deallocate(pairs)

! finally, correct for the fact that the original values were unsigned integers
      where(exppat.lt.0.0) exppat = exppat + 256.0

    case(6)  ! "OxfordHDF"
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

! Update 07/13/19: after talking with Phillipe Pinard (Oxford) at the EMAS 2019 conference
! in Trondheim, it is clear that Oxford is working on including the patterns into their
! current HDF5 file version.  This might become available sometime by the end of 2019.

    case(4)  ! "TSLHDF" passed tests on 2/20/18 by MDG
! read a hyperslab single pattern section from the HDF5 input file
! dims3 should have the pattern dimensions and then 1_HSIZE_T for the third dimension
! offset3 should have (0,0) and then the offset of the pattern (0-based)
        EBSDpatint = HDF_readHyperslabIntegerArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppat = 0.0
        do jj=1,dims3(2)
            do ii=1,dims3(1)
                  z = float(EBSDpatint(ii,jj,1))
                  if (z.lt.0.0) z = z+2.0**16
                  exppat((jj-1)*dims3(1)+ii) = z
            end do 
        end do 

    case(7)  ! "EMEBSD" passed tests on 2/20/18 by MDG
! read a hyperslab single pattern section from the HDF5 input file
! dims3 should have the pattern dimensions and then 1_HSIZE_T for the third dimension
! offset3 should have (0,0) and then the offset of the pattern (0-based)
        EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppat = 0.0
        do jj=1,dims3(2)
            do ii=1,dims3(1)
                  exppat((jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,1)))
            end do 
        end do 


    case(8)  ! "BrukerHDF"  to be tested
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
                exppat((jj-1)*dims3(1)+ii) = float(ichar(EBSDpat(ii,jj,1)))
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

    case(2,3)  ! "TSLup2"
        close(unit=funit,status='keep')

    case(4, 7)  ! "TSLHDF" "EMEBSD"
        call HDF_pop(pmHDF_head,.TRUE.)
        nullify(pmHDF_head)

    case(5)  ! "OxfordBinary"
        close(unit=funit,status='keep')

    case(6)  ! "OxfordHDF"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(8)  !  "BrukerHDF"
        call HDF_pop(pmHDF_head,.TRUE.)
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


!--------------------------------------------------------------------------
!
! subroutine: PreProcessPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief standard preprocessing of EBSD patterns (hi-pass filter+adaptive histogram equalization)
!
!> @details This is one of the core routines used to pre-process EBSD patterns for dictionary indexing.
!> The routine reads the experimental patterns row by row, and performs a hi-pass filter and adaptive
!> histogram equalization.  Then the preprocessed patterns are either stored in a binary direct access file,
!> or they are kept in RAM, depending on the user parameter setting. 
!
!> @param nthreads number of threads to be used
!> @param inRAM write file or keep patterns in RAM ?
!> @param ebsdnl namelist for the ebsd computations
!> @param binx, biny pattern size 
!> @param masklin vector form of pattern mask 
!> @param correctsize 
!> @param totnumexpt 
!> @param epatterns (OPTIONAL) array with pre-processed patterns
!> @param exptIQ (OPTIONAL) computation of pattern quality (IQ) array
!
!> @date 04/01/18 MDG 1.0 original
!> @date 04/04/18 MDG 1.1 added optional exptIQ parameter
!--------------------------------------------------------------------------
recursive subroutine PreProcessPatterns(nthreads, inRAM, ebsdnl, binx, biny, masklin, correctsize, totnumexpt, &
                                        epatterns, exptIQ)
!DEC$ ATTRIBUTES DLLEXPORT :: PreProcessPatterns

use io
use local
use typedefs
!use ebsddimod
use NameListTypedefs
use error
use omp_lib
use filters
use timing
use commonmod
use ISO_C_BINDING

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                :: nthreads
logical,INTENT(IN)                          :: inRAM
type(EBSDIndexingNameListType),INTENT(IN)   :: ebsdnl
integer(kind=irg),INTENT(IN)                :: binx
integer(kind=irg),INTENT(IN)                :: biny
real(kind=sgl),INTENT(IN)                   :: masklin(binx*biny)
integer(kind=irg),INTENT(IN)                :: correctsize
integer(kind=irg),INTENT(IN)                :: totnumexpt
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: epatterns(correctsize, totnumexpt)
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: exptIQ(totnumexpt)

logical                                     :: ROIselected, f_exists
character(fnlen)                            :: fname
integer(kind=irg)                           :: istat, L, recordsize, io_int(2), patsz, iii, &
                                               iiistart, iiiend, jjend, TID, jj, kk, ierr
integer(HSIZE_T)                            :: dims3(3), offset3(3)
integer(kind=irg),parameter                 :: iunitexpt = 41, itmpexpt = 42
real(kind=sgl)                              :: tstart, tstop, vlen, tmp, ma, mi, io_real(1)
real(kind=dbl)                              :: w, Jres
integer(kind=irg),allocatable               :: EBSDpint(:,:)
real(kind=sgl),allocatable                  :: tmpimageexpt(:), EBSDPattern(:,:), imageexpt(:), exppatarray(:), EBSDpat(:,:)
real(kind=dbl),allocatable                  :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)
complex(kind=dbl),allocatable               :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable       :: inp(:,:), outp(:,:)
type(C_PTR)                                 :: planf, HPplanf, HPplanb


call Message('Preprocessing experimental patterns')

!===================================================================================
! define a bunch of mostly integer parameters
recordsize = correctsize*4
L = binx*biny
patsz = correctsize
w = ebsdnl%hipassw

if (sum(ebsdnl%ROI).ne.0) then
  ROIselected = .TRUE.
  iiistart = ebsdnl%ROI(2)
  iiiend = ebsdnl%ROI(2)+ebsdnl%ROI(4)-1
  jjend = ebsdnl%ROI(3)
else
  ROIselected = .FALSE.
  iiistart = 1
  iiiend = ebsdnl%ipf_ht
  jjend = ebsdnl%ipf_wd
end if

!===================================================================================
! open the output file if the patterns are not to be kept in memory
if (inRAM.eqv..FALSE.) then
! first, make sure that this file does not already exist
   f_exists = .FALSE.
   fname = trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)
   fname = EMsoft_toNativePath(fname)
   inquire(file=trim(trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)), exist=f_exists)

   call WriteValue('Creating temporary file :',trim(fname))

   if (f_exists) then
      open(unit=itmpexpt,file=trim(fname),&
          status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
      close(unit=itmpexpt,status='delete')
   end if
   open(unit=itmpexpt,file=trim(fname),&
   status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
end if
  
!===================================================================================
! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(ebsdnl%exptfile, ebsdnl%ipf_wd, L, ebsdnl%inputtype, recordsize, iunitexpt, &
                           ebsdnl%HDFstrings)
if (istat.ne.0) then
    call patternmod_errormessage(istat)
    call FatalError("PreProcessPatterns:", "Fatal error handling experimental pattern file")
end if

! this next part is done with OpenMP, with only thread 0 doing the reading;
! Thread 0 reads one line worth of patterns from the input file, then all threads do 
! the work, and thread 0 adds them to the epatterns array in RAM; repeat until all patterns have been processed.

call OMP_SET_NUM_THREADS(nthreads)
io_int(1) = nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

! allocate the arrays that holds the experimental patterns from a single row of the region of interest
allocate(exppatarray(patsz * ebsdnl%ipf_wd),stat=istat)
if (istat .ne. 0) stop 'could not allocate exppatarray'

if (present(exptIQ)) then
! prepare the fftw plan for this pattern size to compute pattern quality (pattern sharpness Q)
  allocate(EBSDPat(binx,biny),stat=istat)
  if (istat .ne. 0) stop 'could not allocate arrays for EBSDPat filter'
  EBSDPat = 0.0
  allocate(ksqarray(binx,biny),stat=istat)
  if (istat .ne. 0) stop 'could not allocate ksqarray array'
  Jres = 0.0
  call init_getEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf)
  deallocate(EBSDPat)
end if

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask array'
call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
deallocate(inp, outp)

call Message('Starting processing of experimental patterns')
call cpu_time(tstart)

dims3 = (/ binx, biny, ebsdnl%ipf_wd /)

!===================================================================================
! we do one row at a time
prepexperimentalloop: do iii = iiistart,iiiend

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat) &
!$OMP& PRIVATE(imageexpt, tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, vlen, tmp, inp, outp)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()

! initialize thread private variables
    allocate(EBSDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),tmpimageexpt(binx*biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(EBSDpint(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate EBSDpint array'

    allocate(inp(binx,biny),outp(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate inp, outp arrays'

    tmpimageexpt = 0.0
    rrdata = 0.D0
    ffdata = 0.D0

! thread 0 reads the next row of patterns from the input file
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*ebsdnl%ipf_wd /)
        if (ROIselected.eqv..TRUE.) then
            call getExpPatternRow(iii, ebsdnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                                  ebsdnl%inputtype, ebsdnl%HDFstrings, exppatarray, ebsdnl%ROI)
        else
            call getExpPatternRow(iii, ebsdnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                                  ebsdnl%inputtype, ebsdnl%HDFstrings, exppatarray)
        end if
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER
    jj=0

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=1,jjend
! convert imageexpt to 2D EBSD Pattern array
        do kk=1,biny
          EBSDPat(1:binx,kk) = exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx)
        end do

        if (present(exptIQ)) then
! compute the pattern Image Quality 
          exptIQ((iii-iiistart)*jjend + jj) = sngl(computeEBSDIQ(binx, biny, EBSDPat, ksqarray, Jres, planf))
        end if

! Hi-Pass filter
        rrdata = dble(EBSDPat)
        ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), w, hpmask, inp, outp, HPplanf, HPplanb)
        EBSDPat = sngl(ffdata)

! adaptive histogram equalization
        ma = maxval(EBSDPat)
        mi = minval(EBSDPat)
    
        EBSDpint = nint(((EBSDPat - mi) / (ma-mi))*255.0)
        EBSDPat = float(adhisteq(ebsdnl%nregions,binx,biny,EBSDpint))

! convert back to 1D vector
        do kk=1,biny
          exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx) = EBSDPat(1:binx,kk)
        end do

! apply circular mask and normalize for the dot product computation
        exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) * masklin(1:L)
        vlen = NORM2(exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L))
        if (vlen.ne.0.0) then
          exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L)/vlen
        else
          exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = 0.0
        end if
    end do
!$OMP END DO

! thread 0 writes the row of patterns to the epatterns array or to the file, depending on the enl%inRAM parameter
    if (TID.eq.0) then
      if (inRAM.eqv..TRUE.) then
        do jj=1,jjend
          epatterns(1:patsz,(iii-iiistart)*jjend + jj) = exppatarray((jj-1)*patsz+1:jj*patsz)
        end do
      else
        do jj=1,jjend
          write(itmpexpt,rec=(iii-iiistart)*jjend + jj) exppatarray((jj-1)*patsz+1:jj*patsz)
        end do
      end if
    end if

deallocate(tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, inp, outp)
!$OMP BARRIER
!$OMP END PARALLEL

! print an update of progress
    if (mod(iii-iiistart+1,5).eq.0) then
      if (ROIselected.eqv..TRUE.) then
        io_int(1:2) = (/ iii-iiistart+1, ebsdnl%ROI(4) /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
      else
        io_int(1:2) = (/ iii-iiistart+1, ebsdnl%ipf_ht /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
      end if
    end if
end do prepexperimentalloop

call Message(' -> experimental patterns preprocessed')

call closeExpPatternFile(ebsdnl%inputtype, iunitexpt)

if (inRAM.eqv..FALSE.) then
  close(unit=itmpexpt,status='keep')
end if

! print some timing information
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(ebsdnl%nthreads) * float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,/)")

end subroutine PreProcessPatterns


!--------------------------------------------------------------------------
!
! subroutine: PreProcessTKDPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief standard preprocessing of TKD patterns (hi-pass filter+adaptive histogram equalization)
!
!> @details This is one of the core routines used to pre-process TKD patterns for dictionary indexing.
!> The routine reads the experimental patterns row by row, and performs a hi-pass filter and adaptive
!> histogram equalization.  Then the preprocessed patterns are either stored in a binary direct access file,
!> or they are kept in RAM, depending on the user parameter setting. 
!
!> @param nthreads number of threads to be used
!> @param inRAM write file or keep patterns in RAM ?
!> @param tkdnl namelist for the tkd computations
!> @param binx, biny pattern size 
!> @param masklin vector form of pattern mask 
!> @param correctsize 
!> @param totnumexpt 
!> @param epatterns (OPTIONAL) array with pre-processed patterns
!> @param exptIQ (OPTIONAL) computation of pattern quality (IQ) array
!
!> @date 11/30/18 MDG 1.0 original, based on EBSD routine
!--------------------------------------------------------------------------
recursive subroutine PreProcessTKDPatterns(nthreads, inRAM, tkdnl, binx, biny, masklin, correctsize, totnumexpt, &
                                        epatterns, exptIQ)
!DEC$ ATTRIBUTES DLLEXPORT :: PreProcessTKDPatterns

use io
use local
use typedefs
!use ebsddimod
use NameListTypedefs
use error
use omp_lib
use filters
use timing

use ISO_C_BINDING

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                :: nthreads
logical,INTENT(IN)                          :: inRAM
type(TKDIndexingNameListType),INTENT(IN)    :: tkdnl
integer(kind=irg),INTENT(IN)                :: binx
integer(kind=irg),INTENT(IN)                :: biny
real(kind=sgl),INTENT(IN)                   :: masklin(binx*biny)
integer(kind=irg),INTENT(IN)                :: correctsize
integer(kind=irg),INTENT(IN)                :: totnumexpt
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: epatterns(correctsize, totnumexpt)
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: exptIQ(totnumexpt)

logical                                     :: ROIselected, f_exists
character(fnlen)                            :: fname
integer(kind=irg)                           :: istat, L, recordsize, io_int(2), patsz, iii, &
                                               iiistart, iiiend, jjend, TID, jj, kk, ierr
integer(HSIZE_T)                            :: dims3(3), offset3(3)
integer(kind=irg),parameter                 :: iunitexpt = 41, itmpexpt = 42
real(kind=sgl)                              :: tstart, tstop, vlen, tmp, ma, mi, io_real(1)
real(kind=dbl)                              :: w, Jres
integer(kind=irg),allocatable               :: TKDpint(:,:)
real(kind=sgl),allocatable                  :: tmpimageexpt(:), TKDPattern(:,:), imageexpt(:), exppatarray(:), TKDpat(:,:)
real(kind=dbl),allocatable                  :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)
complex(kind=dbl),allocatable               :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable       :: inp(:,:), outp(:,:)
type(C_PTR)                                 :: planf, HPplanf, HPplanb


call Message('Preprocessing experimental patterns')

!===================================================================================
! define a bunch of mostly integer parameters
recordsize = correctsize*4
L = binx*biny
patsz = correctsize
w = tkdnl%hipassw

if (sum(tkdnl%ROI).ne.0) then
  ROIselected = .TRUE.
  iiistart = tkdnl%ROI(2)
  iiiend = tkdnl%ROI(2)+tkdnl%ROI(4)-1
  jjend = tkdnl%ROI(3)
else
  ROIselected = .FALSE.
  iiistart = 1
  iiiend = tkdnl%ipf_ht
  jjend = tkdnl%ipf_wd
end if

!===================================================================================
! open the output file if the patterns are not to be kept in memory
if (inRAM.eqv..FALSE.) then
! first, make sure that this file does not already exist
   f_exists = .FALSE.
   fname = trim(EMsoft_getEMtmppathname())//trim(tkdnl%tmpfile)
   fname = EMsoft_toNativePath(fname)
   inquire(file=trim(trim(EMsoft_getEMtmppathname())//trim(tkdnl%tmpfile)), exist=f_exists)

   call WriteValue('Creating temporary file :',trim(fname))

   if (f_exists) then
      open(unit=itmpexpt,file=trim(fname),&
          status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
      close(unit=itmpexpt,status='delete')
   end if
   open(unit=itmpexpt,file=trim(fname),&
   status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
end if
  
!===================================================================================
! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMTKD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(tkdnl%exptfile, tkdnl%ipf_wd, L, tkdnl%inputtype, recordsize, iunitexpt, &
                           tkdnl%HDFstrings)
if (istat.ne.0) then
    call patternmod_errormessage(istat)
    call FatalError("PreProcessTKDPatterns:", "Fatal error handling experimental pattern file")
end if

! this next part is done with OpenMP, with only thread 0 doing the reading;
! Thread 0 reads one line worth of patterns from the input file, then all threads do 
! the work, and thread 0 adds them to the epatterns array in RAM; repeat until all patterns have been processed.

call OMP_SET_NUM_THREADS(nthreads)
io_int(1) = nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

! allocate the arrays that holds the experimental patterns from a single row of the region of interest
allocate(exppatarray(patsz * tkdnl%ipf_wd),stat=istat)
if (istat .ne. 0) stop 'could not allocate exppatarray'

if (present(exptIQ)) then
! prepare the fftw plan for this pattern size to compute pattern quality (pattern sharpness Q)
  allocate(TKDPat(binx,biny),stat=istat)
  if (istat .ne. 0) stop 'could not allocate arrays for EBSDPat filter'
  TKDPat = 0.0
  allocate(ksqarray(binx,biny),stat=istat)
  if (istat .ne. 0) stop 'could not allocate ksqarray array'
  Jres = 0.0
! we should be able to just use the EBSD routine to do this ...
  call init_getEBSDIQ(binx, biny, TKDPat, ksqarray, Jres, planf)
  deallocate(TKDPat)
end if

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask array'
call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
deallocate(inp, outp)

call Message('Starting processing of experimental patterns')
call cpu_time(tstart)

dims3 = (/ binx, biny, tkdnl%ipf_wd /)

!===================================================================================
! we do one row at a time
prepexperimentalloop: do iii = iiistart,iiiend

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat) &
!$OMP& PRIVATE(imageexpt, tmpimageexpt, TKDPat, rrdata, ffdata, TKDpint, vlen, tmp, inp, outp)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()

! initialize thread private variables
    allocate(TKDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),tmpimageexpt(binx*biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(TKDpint(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate TKDpint array'

    allocate(inp(binx,biny),outp(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate inp, outp arrays'

    tmpimageexpt = 0.0
    rrdata = 0.D0
    ffdata = 0.D0

! thread 0 reads the next row of patterns from the input file
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*tkdnl%ipf_wd /)
        if (ROIselected.eqv..TRUE.) then
            call getExpPatternRow(iii, tkdnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                                  tkdnl%inputtype, tkdnl%HDFstrings, exppatarray, tkdnl%ROI)
        else
            call getExpPatternRow(iii, tkdnl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, &
                                  tkdnl%inputtype, tkdnl%HDFstrings, exppatarray)
        end if
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER
    jj=0

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=1,jjend
! convert imageexpt to 2D TKD Pattern array
        do kk=1,biny
          TKDPat(1:binx,kk) = exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx)
        end do

        if (present(exptIQ)) then
! compute the pattern Image Quality 
          exptIQ((iii-iiistart)*jjend + jj) = sngl(computeEBSDIQ(binx, biny, TKDPat, ksqarray, Jres, planf))
        end if

! Hi-Pass filter
        rrdata = dble(TKDPat)
        ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), w, hpmask, inp, outp, HPplanf, HPplanb)
        TKDPat = sngl(ffdata)

! adaptive histogram equalization
        ma = maxval(TKDPat)
        mi = minval(TKDPat)
    
        TKDpint = nint(((TKDPat - mi) / (ma-mi))*255.0)
        TKDPat = float(adhisteq(tkdnl%nregions,binx,biny,TKDpint))

! convert back to 1D vector
        do kk=1,biny
          exppatarray((jj-1)*patsz+(kk-1)*binx+1:(jj-1)*patsz+kk*binx) = TKDPat(1:binx,kk)
        end do

! apply circular mask and normalize for the dot product computation
        exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) * masklin(1:L)
        vlen = NORM2(exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L))
        if (vlen.ne.0.0) then
          exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L)/vlen
        else
          exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L) = 0.0
        end if
    end do
!$OMP END DO

! thread 0 writes the row of patterns to the epatterns array or to the file, depending on the enl%inRAM parameter
    if (TID.eq.0) then
      if (inRAM.eqv..TRUE.) then
        do jj=1,jjend
          epatterns(1:patsz,(iii-iiistart)*jjend + jj) = exppatarray((jj-1)*patsz+1:jj*patsz)
        end do
      else
        do jj=1,jjend
          write(itmpexpt,rec=(iii-iiistart)*jjend + jj) exppatarray((jj-1)*patsz+1:jj*patsz)
        end do
      end if
    end if

deallocate(tmpimageexpt, TKDPat, rrdata, ffdata, TKDpint, inp, outp)
!$OMP BARRIER
!$OMP END PARALLEL

! print an update of progress
    if (mod(iii-iiistart+1,5).eq.0) then
      if (ROIselected.eqv..TRUE.) then
        io_int(1:2) = (/ iii-iiistart+1, tkdnl%ROI(4) /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
      else
        io_int(1:2) = (/ iii-iiistart+1, tkdnl%ipf_ht /)
        call WriteValue('Completed row ',io_int,2,"(I4,' of ',I4,' rows')")
      end if
    end if
end do prepexperimentalloop

call Message(' -> experimental patterns preprocessed')

call closeExpPatternFile(tkdnl%inputtype, iunitexpt)

if (inRAM.eqv..FALSE.) then
  close(unit=itmpexpt,status='keep')
end if

! print some timing information
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(tkdnl%nthreads) * float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,/)")

end subroutine PreProcessTKDPatterns


!--------------------------------------------------------------------------
!
! subroutine: getEMsoftPCcoordinates
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert pattern center coordinates to EMsoft units for each vendor
!
!> @param pctr array of 3 PC coordinates
!> @param vendor vendor string
!> @param delta pixel size [microns]
!> @param Nx number of detector pixels along x
!> @param Ny number of detector pixels along y
!
!> @date 08/20/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getEMsoftPCcoordinates(pctr, vendor, delta, Nx, Ny) result(EMsoftpc)
!DEC$ ATTRIBUTES DLLEXPORT :: getEMsoftPCcoordinates

use io

IMPLICIT NONE 

real(kind=sgl),INTENT(IN)           :: pctr(3) 
character(fnlen),INTENT(IN)         :: vendor 
real(kind=sgl),INTENT(IN)           :: delta
integer(kind=irg),INTENT(IN)        :: Nx
integer(kind=irg),INTENT(IN)        :: Ny
real(kind=sgl)                      :: EMsoftpc(3)

real(kind=sgl)                      :: io_real(3)

if (trim(vendor).eq.'EMsoft') then 
  EMsoftpc = pctr 
end if 

if (trim(vendor).eq.'EDAX/TSL') then 
  EMsoftpc = (/ Nx * (pctr(1) - 0.5), Nx * pctr(2) - Ny*0.5, Nx * delta * pctr(3) /)
end if 

if (trim(vendor).eq.'Oxford') then 
  EMsoftpc = (/ Nx * (pctr(1) - 0.5), Ny * (pctr(2) - 0.5), Nx * delta * pctr(3) /)
end if 

if (trim(vendor).eq.'Bruker') then 
  EMsoftpc = (/ Nx * (pctr(1) - 0.5), Ny * (0.5 - pctr(2)), Nx * delta * pctr(3) /)
end if 

if (trim(vendor).ne.'EMsoft') then 
  io_real = pctr 
  call WriteValue('Input pattern center coordinates in '//trim(vendor)//' convention : ',io_real,3)
  io_real = EMsoftpc
  call WriteValue('  --> pattern center coordinates in EMsoft convention : ',io_real,3)
end if

end function getEMsoftPCcoordinates


end module patternmod
