! ###################################################################
! Copyright (c) 2018-2020, Marc De Graef Research Group/Carnegie Mellon University
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
use math

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

type(HDFobjectStackType)        ,save,private       :: pmHDF_head 

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
if (trim(inputtype).eq."NORDIF") itype = 9       
if (trim(inputtype).eq."EMEBSD32i") itype = 10  ! for 32-bit integer pattern files
if (trim(inputtype).eq."EMEBSD32f") itype = 11  ! for 32-bit float pattern files

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
!> @date 10/09/19 HWÅ 1.8 support for NORDIF binary pattern files
!> @date 04/06/20 MDG 1.9 adds support for 32-bit integer and float HDF files generated by EMEBSD 
!--------------------------------------------------------------------------
recursive function openExpPatternFile(filename, npat, L, inputtype, recsize, funit, HDFstrings, verbose) result(istat)
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
logical,INTENT(IN),OPTIONAL             :: verbose
istat = 0

! first determine how many HDFgroups there are; the last entry in HDFstrings should be the data set name
hdfnumg = get_num_HDFgroups(HDFstrings)
itype = get_input_type(inputtype)

if (filename(1:1).eq.EMsoft_getEMsoftnativedelimiter()) then
  ename = trim(filename)
else
  ename = trim(EMsoft_getEMdatapathname())//trim(filename)
  ename = EMsoft_toNativePath(ename)
end if

f_exists = .FALSE.
inquire(file=trim(ename), exist=f_exists)

if (.not.f_exists) then
   call Message(' Input file '//trim(ename)//' does not exist in this location ... ')
   call Message(' Please check the input parameters in the namelist file.')
   call Message(' ')
   call FatalError('openExpPatternFile','Unrecoverable error; file not found')
end if

if (present(verbose)) then 
  if (verbose.eqv..TRUE.) then 
    call Message('Pattern input file '//trim(ename))
	call Message('  input file type '//trim(inputtype))
  end if
end if

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

    case(4, 7, 10, 11)  ! "TSLHDF", "EMEBSD"
        nullify(pmHDF_head%next)
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
        nullify(pmHDF_head%next)
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
               if (present(verbose)) then
	                if (verbose.eqv..TRUE.) then
                    call Message('  found pattern reordering arrays')
                  end if
               end if
               ! and leave this group
               call HDF_pop(pmHDF_head)
            end if
        end do
        ! and here we leave this file open so that we can read data blocks using the hyperslab mechanism;
        ! we can do this because the pmHDF_head pointer is private and has SAVE status for this entire module

    case(9)  !  "NORDIF"
        open(unit=funit, file=trim(ename), status='old', access='stream', &
            iostat=ios)
        if (ios.ne.0) then
            io_int(1) = ios
            call WriteValue("File open error; error type ", io_int, 1)
            call FatalError("openExpPatternFile", "Cannot continue program")
        end if

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
!> @date 09/26/19 MDG 1.6 add option for vertical flip of patterns
!> @date 10/09/19 HWÅ 1.7 support for NORDIF binary pattern files
!--------------------------------------------------------------------------
recursive subroutine getExpPatternRow(iii, wd, patsz, L, dims3, offset3, funit, inputtype, HDFstrings, &
                                      exppatarray, ROI, flipy) 
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
!f2py intent(in,out) ::  exppatarray
integer(kind=irg),OPTIONAL,INTENT(IN)   :: ROI(4)
logical,OPTIONAL,INTENT(IN)             :: flipy

integer(kind=irg)                       :: itype, hdfnumg, ierr, ios
real(kind=sgl)                          :: imageexpt(L), z
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=irg),allocatable           :: EBSDpat32i(:,:,:)
real(kind=sgl),allocatable              :: EBSDpat32f(:,:,:)
integer(kind=C_INT16_T),allocatable     :: EBSDpatint(:,:,:)
character(1),allocatable                :: buffer(:)
integer(kind=ish),allocatable           :: pairs(:)
integer(kind=irg)                       :: sng, pixcnt
integer(kind=ish)                       :: pair(2)
integer(HSIZE_T)                        :: dims3new(3), offset3new(3), newspot
integer(kind=ill)                       :: recpos, ii, jj, kk, ispot, liii, lpatsz, lwd, lL, buffersize, kspot, jspot, &
                                           kkstart, kkend, multfactor
integer(kind=8)                         :: patoffsets(wd)
logical                                 :: flip 

flip = .FALSE.
if (present(flipy)) then 
  if (flipy.eqv..TRUE.) flip = .TRUE.
end if 

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
          if (flip.eqv..TRUE.) then
            jspot = (dims3(2)-jj)*dims3(1) 
          else
            jspot = (jj-1)*dims3(1) 
          end if 
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

    case(10)  ! "EMEBSD32i" 
! read a hyperslab section from the HDF5 input file
        EBSDpat32i = HDF_readHyperslabIntegerArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppatarray = 0.0
        do kk=kkstart,kkend
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                      exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = float(EBSDpat32i(ii,jj,kk))
                end do 
            end do 
        end do 

    case(11)  ! "EMEBSD32f" passed tests on 2/14/18 by MDG
! read a hyperslab section from the HDF5 input file
        EBSDpat32f = HDF_readHyperslabFloatArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppatarray = 0.0
        do kk=kkstart,kkend
            do jj=1,dims3(2)
                do ii=1,dims3(1)
                      exppatarray((kk-kkstart)*patsz+(jj-1)*dims3(1)+ii) = EBSDpat32f(ii,jj,kk)
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

    case(9)  !  "NORDIF"
        ! Buffer for single patterns
        buffersize = lL
        allocate(buffer(buffersize))

        ! Pairs to store all patterns in a row
        buffersize = lwd * lL
        allocate(pairs(buffersize))

        ! Loop over pixels and convert byte values into single byte integers
        do ii = 1, lwd
            ! pos = [(row-1)*scan_width + column - 1]*pattern_size + 1
            read(unit=funit, pos=((liii-1)*lwd + ii - 1)*lL + 1, &
                iostat=ios) buffer
            do jj = 1_ill, lL
                pairs((ii-1)*lL + jj) = ichar(buffer(jj))
            end do
        end do
        deallocate(buffer)

        ! Place patterns in experimental pattern array 
        exppatarray = 0.0
        ! Pattern pixels to read (might not be full pattern, depending on ROI)
        pixcnt = (kkstart-1)*dims3(1)*dims3(2) + 1
        ! Loop over row (might not be full row, depending on ROI)
        do kk = kkstart, kkend
            kspot = (kk-kkstart) * patsz
            ! Loop over rows of pattern pixels
            do jj = 1, dims3(2)
                jspot = (jj-1) * dims3(1)
                ! Loop over columns of pattern pixels, converting into float32
                do ii = 1, dims3(1)
                    exppatarray(kspot + jspot + ii) = float(pairs(pixcnt))
                    pixcnt = pixcnt + 1
                end do
            end do
        end do
        deallocate(pairs)

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
!> @date 10/09/19 HWÅ 1.7 support for reading single pattern from NORDIF binary file
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
!f2py intent(in,out) ::  exppat

integer(kind=irg)                       :: itype, hdfnumg, ierr, ios
real(kind=sgl)                          :: imageexpt(L), z
character(fnlen)                        :: dataset
character(kind=c_char),allocatable      :: EBSDpat(:,:,:)
integer(kind=irg),allocatable           :: EBSDpat32i(:,:,:)
real(kind=sgl),allocatable              :: EBSDpat32f(:,:,:)
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

    case(10)  ! "EMEBSD32i"
! read a hyperslab single pattern section from the HDF5 input file
! dims3 should have the pattern dimensions and then 1_HSIZE_T for the third dimension
! offset3 should have (0,0) and then the offset of the pattern (0-based)
        EBSDpat32i = HDF_readHyperslabIntegerArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppat = 0.0
        do jj=1,dims3(2)
            do ii=1,dims3(1)
                  exppat((jj-1)*dims3(1)+ii) = float(EBSDpat32i(ii,jj,1))
            end do 
        end do 

    case(11)  ! "EMEBSD32f"
! read a hyperslab single pattern section from the HDF5 input file
! dims3 should have the pattern dimensions and then 1_HSIZE_T for the third dimension
! offset3 should have (0,0) and then the offset of the pattern (0-based)
        EBSDpat32f = HDF_readHyperslabFloatArray3D(dataset, offset3, dims3, pmHDF_head) 
        exppat = 0.0
        do jj=1,dims3(2)
            do ii=1,dims3(1)
                  exppat((jj-1)*dims3(1)+ii) = EBSDpat32f(ii,jj,1)
            end do 
        end do         

    case(8)  ! "BrukerHDF"  to be tested
! since the pattern order in the Bruker data file is not necessarily the order in which the patterns
! were acquired, we need to read each patttern separately from the file using the appropriate offset, which 
! is calculated using the semix and semiy arrays.  That means that we have to redefine both dims3 and offset3
! and use the original pattern coordinate (ispot) as an index into the reordering arrays.
        exppat = 0.0
        dims3new = (/ dims3(1), dims3(2), 1_HSIZE_T /)
        ispot = offset3(3)+1
        newspot = semiy(ispot) * wd + semix(ispot)
        offset3new = (/ offset3(1), offset3(2),  newspot /)
        EBSDpat32i = HDF_readHyperslabIntegerArray3D(dataset, offset3new, dims3new, pmHDF_head) 
        !EBSDpat = HDF_readHyperslabCharArray3D(dataset, offset3new, dims3new, pmHDF_head) 
        do jj=1,dims3(2)
            do ii=1,dims3(1) 
                exppat((jj-1)*dims3(1)+ii) = float(EBSDpat32i(ii,jj,1))
            end do 
        end do 

    case(9)  !  "NORDIF"
        ! Use ill-type integers
        lL = L
        lwd = wd

        ! Buffers for single patterns
        buffersize = lL
        allocate(buffer(buffersize), pairs(buffersize))

        ! Read single pattern into buffer
        ! offset3(3) = row * scan width + column
        offset = offset3(3)*lL + 1
        read(unit=funit, pos=offset, iostat=ios) buffer

        ! Convert byte values into single byte integers
        pairs = ichar(buffer)
        deallocate(buffer)

        ! Place pattern in experimental pattern array
        exppat = 0.0
        pixcnt = 1
        ! Loop over rows of pattern pixels
        do jj = 1, dims3(2)
            jspot = (jj-1)*dims3(1)
            ! Loop over columns of pattern pixels, converting into float32
            do ii = 1, dims3(1)
                exppat(jspot + ii) = float(pairs(pixcnt))
                pixcnt = pixcnt + 1
            end do
        end do
        deallocate(pairs)

    case default

end select

end subroutine getSingleExpPattern


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetMetaData
!
!> @author Chaoyi Zhu/Marc De Graef, Carnegie Mellon University
!
!> @brief read a list of metadat from input EBSD data file
!
!> @param enl EBSDNameListType
!> @param SEM EBSDSEMArray
!> @param patterndata EBSDDIpreviewNameListType
!> @param angles a list of 
!> @param numangle number of orientation data entry
!> @param filename 
!> @param inputtype
!> @param funit logical unit for reading
!> @param HDFstrings string array with group and dataset names for HDF5 input
!
!> @date 04/29/20 CZ 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetMetaData(enl, SEM, patterndata, angles, numangles, filename, inputtype, funit, HDFstrings, istat, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMetaData

use local
use typedefs
use NameListTypedefs
use error
use HDF5
use HDFsupport
use io
use ISO_C_BINDING
use rotations
use EBSDmod

IMPLICIT NONE
type(EBSDAngleType),INTENT(INOUT)       :: angles
type(EBSDNameListType),INTENT(INOUT)    :: enl
type(EBSDDIpreviewNameListType),INTENT(INOUT)    :: patterndata
type(EBSDSEMArray),INTENT(INOUT)        :: SEM
character(fnlen),INTENT(IN)             :: filename
character(fnlen),INTENT(IN)             :: inputtype
integer(kind=irg),INTENT(IN)            :: funit, numangles
character(fnlen),INTENT(IN)             :: HDFstrings(10)
integer(kind=irg)						:: istat

real(kind=sgl), allocatable           	:: eu1(:), eu2(:), eu3(:), eu(:,:)
real(kind=sgl), allocatable           	:: PCX(:), PCY(:), DD(:)
character(fnlen)                        :: ename
integer(kind=irg)                       :: i, ierr, io_int(1), itype, hdferr, hdfnumg, recordsize, up2header(4), &
                                           ios, up1header(4), version, patx, paty, myoffset, offset, nlines
character(fnlen)                        :: groupname, dataset, platform
integer(HSIZE_T)                        :: dims1(1), dims2(2)
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
character(1,KIND=c_char),allocatable,TARGET            :: charac1(:)
character(3,KIND=c_char),allocatable,TARGET            :: charac3(:)
character(5,KIND=c_char),allocatable,TARGET            :: charac5(:)
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
logical,INTENT(IN),OPTIONAL             :: verbose
logical                                 :: f_exists

istat = 0

! first determine how many HDFgroups there are; the last entry in HDFstrings should be the data set name
hdfnumg = get_num_HDFgroups(HDFstrings)
itype = get_input_type(inputtype)

if (filename(1:1).eq.EMsoft_getEMsoftnativedelimiter()) then
  ename = trim(filename)
else
  ename = trim(EMsoft_getEMdatapathname())//trim(filename)
  ename = EMsoft_toNativePath(ename)
end if

f_exists = .FALSE.
inquire(file=trim(ename), exist=f_exists)

if (.not.f_exists) then
   call Message(' Input file '//trim(ename)//' does not exist in this location ... ')
   call Message(' Please check the input parameters in the namelist file.')
   call Message(' ')
   call FatalError('GetMetaData','Unrecoverable error; file not found')
end if


if (present(verbose)) then 
  if (verbose.eqv..TRUE.) then 
    call Message('Pattern input file '//trim(ename))
	  call Message('  input file type '//trim(inputtype))
  end if
end if


platform = EMsoft_getEMsoftplatform()

! depending on the inputtype, we open the input file in the appropriate way
select case (itype)
    case(1)  ! "Binary"
        ! ! if (trim(platform).eq.'Windows') then
        ! !     recordsize = L        ! windows record length is in units of 4 bytes
        ! ! else
            ! recordsize = L*4      ! all other platforms use record length in units of bytes
        ! ! end if
        ! open(unit=funit,file=trim(ename),&
            ! status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
        ! if (ierr.ne.0) then
            ! io_int(1) = ierr
            ! call WriteValue("File open error; error type ",io_int,1)
            ! call FatalError("openExpPatternFile","Cannot continue program")
        ! end if

    case(2,3)  ! "TSLup1", TSLup2"
        ! ! open the file in STREAM access mode to allow for byte-level access
        ! open(unit=funit,file=trim(ename), status='old',access='stream',iostat=ios)
        ! if (ios.ne.0) then
            ! io_int(1) = ios
            ! call WriteValue("File open error; error type ",io_int,1)
            ! call FatalError("openExpPatternFile","Cannot continue program")
        ! end if
        ! ! the first four 4-byte entries form a header with a version number (unimportant), then 
        ! ! the two dimensions of patterns, and finally an offset parameter indicating at which byte
        ! ! the first pattern starts.  We don't really need the other parameters, but we'll read them anyway.
        ! read(unit=funit, iostat=ios) version, patx, paty, myoffset
        ! offset = myoffset + 1_ill
        ! if (ios.ne.0) then
            ! io_int(1) = ios
            ! call WriteValue("Read error in .up1/2 file header",io_int,1)
            ! call FatalError("openExpPatternFile","Cannot continue program")
        ! end if

    case(5)  ! "OxfordBinary"
        ! ! open the file in STREAM access mode to allow for byte-level access
        ! open(unit=funit,file=trim(ename), status='old',access='stream',iostat=ios)
        ! if (ios.ne.0) then
            ! io_int(1) = ios
            ! call WriteValue("File open error; error type ",io_int,1)
            ! call FatalError("openExpPatternFile","Cannot continue program")
        ! end if

    case(6)  ! "OxfordHDF"
        !call FatalError("openExpPatternFile","OxfordHDF input format not yet implemented")
! at this point in time (Feb. 2018) it does not appear that the Oxford HDF5 format has the 
! patterns stored in it... Hence this option is currently non-existent.

      case(4) ! TSLHDF
        nullify(pmHDF_head%next)
        ! open the file
        hdferr =  HDF_openFile(ename, pmHDF_head, readonly=.TRUE.)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
        ! open the correct level of the data set
        groupname = trim(HDFstrings(1))
        hdferr = HDF_openGroup(groupname, pmHDF_head)

		    ! open groups containing orientation information
        groupname = 'EBSD'
        hdferr = HDF_openGroup(groupname, pmHDF_head)
        groupname = 'Data'
        hdferr = HDF_openGroup(groupname, pmHDF_head)

        ! Euler angle and PC convention (TSL)
			  call Message('Euler angle convention: '//enl%eulerconvention)

        allocate(eu1(numangles),eu2(numangles),eu3(numangles), eu(numangles,3))
        dataset = 'Phi'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, eu2)
        dataset = 'Phi1'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, eu1)
        dataset = 'Phi2'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, eu3)
        allocate(angles%quatang(4,numangles),stat=istat)
        do i=1,numangles 
          eu(i,1)=eu1(i)
          eu(i,2)=eu2(i)
          eu(i,3)=eu3(i)
          angles%quatang(1:4,i) = eu2qu(eu(i,1:3))
        end do
        print *, "Converting Euler angles to quaternions"
        print *, "Number of orientations imported:", numangles
        deallocate(eu1, eu2, eu3, eu)
        call HDF_pop(pmHDF_head)
		! open groups containing pattern center and other scan parameters
        groupname = 'Header'
        hdferr = HDF_openGroup(groupname, pmHDF_head)
        
        dataset = 'Camera Elevation Angle'
        call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%thetac)
        print *, "Tilt angle of the camera:", enl%thetac

        dataset = 'Camera Azimuthal Angle'
        call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%omega)
        print *, "Camera Azimuthal Angle:",enl%omega

        groupname = 'Pattern Center Calibration'
        hdferr = HDF_openGroup(groupname, pmHDF_head)

        dataset = 'x-star'
        call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%xpc)
        call Message('Pattern center convention: '//enl%eulerconvention)
        print *, "Pattern Center Information:"
        print *, "PCx:", enl%xpc
        enl%xpc=-enl%numsx*(enl%xpc-0.5)! change view from detector to sample
        
        dataset = 'y-star'
        call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%ypc)	
        print *, "PCy:", enl%ypc	
        enl%ypc=enl%numsx*enl%ypc-enl%numsy*0.5

        dataset = 'z-star'
        call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%L)
        print *, "Detector Distance:", enl%L
        enl%L=enl%numsx*enl%delta*enl%L

        call HDF_pop(pmHDF_head)
        call HDF_pop(pmHDF_head)
        nullify(pmHDF_head%next)
        ! and here we leave this file open so that we can read data blocks using the hyperslab mechanism;
        ! we can do this because the pmHDF_head pointer is private and has SAVE status for this entire module
    case(7, 10, 11)  !  "EMEBSD"
    
          nullify(pmHDF_head%next)
          ! open the file
          hdferr =  HDF_openFile(ename, pmHDF_head, readonly=.TRUE.)
          if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
         
		  
          ! open Euler angles groups
          groupname = 'EMData'
          hdferr = HDF_openGroup(groupname, pmHDF_head)
          groupname = 'EBSD'
          hdferr = HDF_openGroup(groupname, pmHDF_head)
          dataset = 'EulerAngles'
          ! read angles
          allocate(angles%quatang(4,numangles),stat=istat)
          call HDF_readDatasetFloatArray2D(dataset,dims2, pmHDF_head, hdferr, eu)
          ! convert Euler angles to quaternions
          do i=1,numangles
          angles%quatang(1:4,i) = eu2qu(eu(1:3,i))
          end do
          deallocate(eu)
          call HDF_pop(pmHDF_head)
          call HDF_pop(pmHDF_head)
          ! open EBSDnamelist group 
          groupname = 'NMLparameters'
            hdferr = HDF_openGroup(groupname, pmHDF_head)
          groupname = 'EBSDNameList'
            hdferr = HDF_openGroup(groupname, pmHDF_head)
          ! extract simulation parameters
              dataset = 'xpc'
              call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%xpc)
          write(*,*)
          call Message('Pattern center convention: '//'EMsoft')
          print *, "Pattern Center Information:"
          print *, "xpc:", enl%xpc
              dataset = 'ypc'
              call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%ypc)	
          print *, "ypc:", enl%ypc		  
              dataset = 'L'
              call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%L)
          print *, "L:", enl%L
              dataset = 'thetac'
                      call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%thetac)
          write(*,*)
          print *, "Camera Tilt:", enl%thetac

          dataset = 'delta'
          call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%delta)
          print *, "CCD pixel size on the scintillator surface",enl%delta
          !dataset = 'omega'
          !        call HDF_readDatasetFloat(dataset, HDF_head, hdferr, enl%omega)
          dataset = 'alphaBD'
                  call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%alphaBD)
          dataset = 'energymin'
                  call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%energymin)
          dataset = 'energymax'
                  call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%energymax)
          print *, "Energy range for simulation (kV):",enl%energymin,"-", enl%energymax
          dataset = 'includebackground'
          call HDF_readDatasetStringArray(dataset, nlines, pmHDF_head, hdferr, stringarray)
          enl%includebackground=trim(stringarray(1))
          deallocate(stringarray)
          call Message('Include MC background: '//enl%includebackground)
            
          dataset = 'anglefiletype'
          call HDF_readDatasetStringArray(dataset, nlines, pmHDF_head, hdferr, stringarray)
          enl%anglefiletype=trim(stringarray(1))
          deallocate(stringarray)
        
          dataset = 'eulerconvention'
          call HDF_readDatasetStringArray(dataset, nlines, pmHDF_head, hdferr, stringarray)
          enl%eulerconvention=trim(stringarray(1))
          deallocate(stringarray)
          call Message('Euler angle convention: '//enl%eulerconvention)
        
          dataset = 'bitdepth'
          call HDF_readDatasetStringArray(dataset, nlines, pmHDF_head, hdferr, stringarray)
          enl%bitdepth=trim(stringarray(1))
          deallocate(stringarray)		  
          dataset = 'beamcurrent'
          call HDF_readDatasetDouble(dataset, pmHDF_head, hdferr, enl%beamcurrent)	  
          dataset = 'dwelltime'
          call HDF_readDatasetDouble(dataset, pmHDF_head, hdferr, enl%dwelltime)
          dataset = 'poisson'
          call HDF_readDatasetStringArray(dataset, nlines, pmHDF_head, hdferr, stringarray)
          enl%poisson=trim(stringarray(1))
          deallocate(stringarray)
          call Message('Possion noise included: '//enl%poisson)
            
          dataset = 'binning'
          call HDF_readDatasetInteger(dataset, pmHDF_head, hdferr, enl%binning)
          print *, "Pattern Binning:",enl%binning  
              
          dataset = 'applyDeformation'
          call HDF_readDatasetStringArray(dataset,nlines, pmHDF_head, hdferr, stringarray)
          enl%applyDeformation=trim(stringarray(1))
          deallocate(stringarray)
          call Message('Apply Deformation Tensor: '//enl%applyDeformation)

          dataset = 'scalingmode'
          call HDF_readDatasetStringArray(dataset, nlines, pmHDF_head, hdferr, stringarray)		
          enl%scalingmode=trim(stringarray(1))
          deallocate(stringarray)
          call Message('Intensity scaling mode: '//enl%scalingmode)	
          
          dataset = 'gammavalue'
          call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%gammavalue)	
          if (enl%scalingmode.ne.'not') then
          print *, "gamma value:", enl%gammavalue	  
          end if
          write(*,*)
          call HDF_pop(pmHDF_head)
          call HDF_pop(pmHDF_head)
          nullify(pmHDF_head%next)
    case(8)  !  "BrukerHDF" (a lot of meta data missing in the current HDF5 file)
        nullify(pmHDF_head%next)
        ! open the file
        hdferr =  HDF_openFile(ename, pmHDF_head, readonly=.TRUE.)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
        ! open the correct level of the data set
        groupname = trim(HDFstrings(1))
        hdferr = HDF_openGroup(groupname, pmHDF_head)

		    ! open groups containing orientation information
        groupname = 'EBSD'
        hdferr = HDF_openGroup(groupname, pmHDF_head)
        groupname = 'Data'
        hdferr = HDF_openGroup(groupname, pmHDF_head)

        ! Euler angle convention for Bruker is HKL (+90 degree for phi1)
			  enl%eulerconvention='hkl'
        call Message('Euler angle convention: '//enl%eulerconvention)

        allocate(eu1(numangles),eu2(numangles),eu3(numangles), eu(numangles,3))
        dataset = 'PHI'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, eu2)
        dataset = 'phi1'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, eu1)
        dataset = 'phi2'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, eu3)
        
        ! convert Euler angles to quaternions
        allocate(angles%quatang(4,numangles),stat=istat)
        do i=1,numangles 
          eu(i,1)=eu1(i)+90.0 ! Bruker convention
          eu(i,2)=eu2(i)
          eu(i,3)=eu3(i)
          angles%quatang(1:4,i) = eu2qu(eu(i,1:3)*dtor)
        end do
        print *, "Converting Euler angles to quaternions"
        print *, "Number of orientations imported:", numangles
        deallocate(eu1, eu2, eu3, eu)

        allocate(PCX(numangles),PCY(numangles),DD(numangles))
        dataset = 'PCX'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, PCX)
        enl%xpc=sum(PCX)/size(PCX)
        call Message('Pattern center convention: '//'Bruker')
        print *, "Pattern Center Information:"
        print *, "PCx:", enl%xpc
        enl%xpc=-enl%numsx*(enl%xpc-0.5)! change view from detector to sample
       
        
        dataset = 'PCY'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, PCY)
        enl%ypc=sum(PCY)/size(PCY)
        print *, "PCy:", enl%ypc	
        enl%ypc=enl%numsy*(0.5-enl%ypc)
    

        dataset = 'DD'
        call HDF_readDatasetFloatArray1D(dataset, dims1, pmHDF_head, hdferr, DD)
        enl%L=sum(DD)/size(DD)
        print *, "Detector Distance:", enl%L
        enl%L=enl%numsy*enl%delta*enl%L
        

        deallocate(PCX,PCY,DD)
        call HDF_pop(pmHDF_head)
		   
        groupname = 'Header'
        hdferr = HDF_openGroup(groupname, pmHDF_head)
        
        dataset = 'CameraTilt'
        call HDF_readDatasetFloat(dataset, pmHDF_head, hdferr, enl%thetac)
        print *, "Camera Tilt:", enl%thetac
        call HDF_pop(pmHDF_head)
        call HDF_pop(pmHDF_head)


        groupname = 'SEM'
        hdferr = HDF_openGroup(groupname, pmHDF_head)
        allocate(SEM%SEM_X(patterndata%ipf_wd*patterndata%ipf_ht),stat=istat)
        allocate(SEM%SEM_Y(patterndata%ipf_wd*patterndata%ipf_ht),stat=istat)
        
        dataset = 'SEM IX'
        call HDF_readDatasetIntegerArray1D(dataset, semixydims, pmHDF_head, hdferr, semix)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetIntegerArray1D: problem reading SEM IX array')
        

        dataset = 'SEM IY'
        call HDF_readDatasetIntegerArray1D(dataset, semixydims, pmHDF_head, hdferr, semiy)
        if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetIntegerArray1D: problem reading SEM IY array')
        call invert_ordering_arrays(patterndata%ipf_wd)
        SEM%SEM_Y=semiy
        SEM%SEM_X=semix
        call HDF_pop(pmHDF_head)

        nullify(pmHDF_head%next)


    case(9)  !  "NORDIF"
        ! open(unit=funit, file=trim(ename), status='old', access='stream', &
            ! iostat=ios)
        ! if (ios.ne.0) then
            ! io_int(1) = ios
            ! call WriteValue("File open error; error type ", io_int, 1)
            ! call FatalError("openExpPatternFile", "Cannot continue program")
        ! end if

    case default 
        istat = -1

end select

end subroutine GetMetaData

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

    case(4, 7, 10, 11)  ! "TSLHDF" "EMEBSD" "EMEBSD32i" "EMEBSD32f"
        call HDF_pop(pmHDF_head,.TRUE.)
        nullify(pmHDF_head%next)

    case(5)  ! "OxfordBinary"
        close(unit=funit,status='keep')

    case(6)  ! "OxfordHDF"
        call FatalError("closeExpPatternFile","input format not yet implemented")

    case(8)  !  "BrukerHDF"
        call HDF_pop(pmHDF_head,.TRUE.)
        nullify(pmHDF_head%next)
        deallocate(semix, semiy)

    case(9)  !  "NORDIF"
        close(unit=funit, status='keep')

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
!> @brief standard preprocessing of EBSD patterns (hi-pass filter+adaptive histogram equalization+binning)
!
!> @details This is one of the core routines used to pre-process EBSD patterns for dictionary indexing.
!> The routine reads the experimental patterns row by row, and performs a hi-pass filter and adaptive
!> histogram equalization.  Then the preprocessed patterns are either stored in a binary direct access file,
!> or they are kept in RAM, depending on the user parameter setting. 
!>
!> Pattern binning has been added in EMsoft version 5.0.3; this required splitting of the recordsizes
!> for the two files involved, the input pattern file and the preprocessed pattern file.  The input 
!> patterns have size (exptnumsx, exptnumsy) whereas the binned patterns have size (binx, biny), as
!> do the dictionary patterns. All the preprocessing steps are performed on the full size experimental
!> patterns *before* binning. 
!
!> @param nthreads number of threads to be used
!> @param inRAM write file or keep patterns in RAM ?
!> @param ebsdnl namelist for the ebsd computations
!> @param binx, biny binned pattern size 
!> @param masklin vector form of pattern mask 
!> @param correctsize (this is for the preprocessed output patterns !!!)
!> @param totnumexpt 
!> @param epatterns (OPTIONAL) array with pre-processed patterns
!> @param exptIQ (OPTIONAL) computation of pattern quality (IQ) array
!
!> @date 04/01/18 MDG 1.0 original
!> @date 04/04/18 MDG 1.1 added optional exptIQ parameter
!> @date 06/02/20 MDG 2.0 changed handling of pattern binning (version 5.0.3)
!> @date 10/06/20 MDG 2.1 add option for normalized cross correlation
!--------------------------------------------------------------------------
recursive subroutine PreProcessPatterns(nthreads, inRAM, ebsdnl, binx, biny, masklin, correctsize, totnumexpt, &
                                        epatterns, exptIQ)
!DEC$ ATTRIBUTES DLLEXPORT :: PreProcessPatterns

use io
use local
use typedefs
!use ebsddimod
use math
use NameListTypedefs
use error
use omp_lib
use filters
use timing
use commonmod
use ImageOPs 
use ISO_C_BINDING
use FFTW3mod
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                :: nthreads
logical,INTENT(IN)                          :: inRAM
type(EBSDIndexingNameListType),INTENT(IN)   :: ebsdnl
integer(kind=irg),INTENT(IN)                :: binx
integer(kind=irg),INTENT(IN)                :: biny
real(kind=sgl),INTENT(IN)                   :: masklin(ebsdnl%exptnumsx*ebsdnl%exptnumsy)
integer(kind=irg),INTENT(IN)                :: correctsize          ! this is really Bcorrectsize
integer(kind=irg),INTENT(IN)                :: totnumexpt
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: epatterns(correctsize, totnumexpt)
!f2py intent(in,out) ::  epatterns
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: exptIQ(totnumexpt)
!f2py intent(in,out) ::  exptIQ


type(ImageRescaler)                         :: rescaler       
logical                                     :: ROIselected, f_exists
character(fnlen)                            :: fname, gname
integer(kind=irg)                           :: istat, PL, BL, Precordsize, Brecordsize, io_int(2), Ppatsz, Bpatsz, iii, &
                                               iiistart, iiiend, jjend, TID, jj, kk, ierr, Pcorrectsize, Px, Py
integer(HSIZE_T)                            :: dims3(3), offset3(3)
integer(kind=irg),parameter                 :: iunitexpt = 41, itmpexpt = 42, itmpexpt2 = 43
integer(kind=irg)                           :: tickstart, tstop 
real(kind=sgl)                              :: vlen, tmp, ma, mi, io_real(1), Nval, Nval2, mean, sdev
real(kind=dbl)                              :: w, Jres, sclfct
integer(kind=irg),allocatable               :: EBSDpint(:,:)
real(kind=sgl),allocatable                  :: tmpimageexpt(:), EBSDPattern(:,:), exppatarray(:), EBSDpat(:,:), pat1D(:), &
                                               Pepatterns(:,:), mask(:,:)
real(kind=dbl),allocatable                  :: rrdata(:,:), ffdata(:,:), ksqarray(:,:), inpat(:,:), outpat(:,:)
complex(kind=dbl),allocatable               :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),pointer           :: inp(:,:), outp(:,:)
type(c_ptr), allocatable                    :: ip, op
type(C_PTR)                                 :: planf, HPplanf, HPplanb


call Message('Preprocessing experimental patterns')

if (ebsdnl%similaritymetric.eq.'ncc') then
  allocate(mask(ebsdnl%exptnumsx,ebsdnl%exptnumsy))
  mask = reshape(masklin,(/ebsdnl%exptnumsx,ebsdnl%exptnumsy/))
end if 

!===================================================================================
! define a bunch of mostly integer parameters

! for the input patterns we have the following parameters (P for experimental Pattern)
if (ebsdnl%exptnumsx.lt.10) then ! we'll assume that we should be using the binned sizes here 
  Px = binx 
  Py = biny 
else
  Px = ebsdnl%exptnumsx
  Py = ebsdnl%exptnumsy
end if
PL = Px * Py
if (mod(PL,16) .ne. 0) then
    Pcorrectsize = 16*ceiling(float(PL)/16.0)
else
    Pcorrectsize = PL
end if
Precordsize = Pcorrectsize*4
Ppatsz = Pcorrectsize

! for the preprocessed output patterns we have (B for preprocessed and Binned)
Brecordsize = correctsize*4       
BL = binx*biny
Bpatsz = correctsize

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
   if (ebsdnl%tmpfile(1:1).ne.EMsoft_getEMsoftnativedelimiter()) then 
     fname = trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)
     fname = EMsoft_toNativePath(fname)
     gname = trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)//'.temp'
     gname = EMsoft_toNativePath(gname)
   else 
     fname = trim(ebsdnl%tmpfile)
     gname = trim(ebsdnl%tmpfile)//'.temp'
   end if
   inquire(file=trim(trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)), exist=f_exists)

   if (Px.eq.binx) then   ! regular file with name fname 
     call WriteValue('Creating temporary file :',trim(fname))

     if (f_exists) then
        open(unit=itmpexpt,file=trim(fname),&
            status='unknown',form='unformatted',access='direct',recl=Brecordsize,iostat=ierr)
        close(unit=itmpexpt,status='delete')
     end if
     open(unit=itmpexpt,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=Brecordsize,iostat=ierr)
   else    ! we will need to bin the pre-processed patterns so we generate a different file
     call WriteValue('Creating temporary file :',trim(gname))

     if (f_exists) then
        open(unit=itmpexpt,file=trim(gname),&
            status='unknown',form='unformatted',access='direct',recl=Precordsize,iostat=ierr)
        close(unit=itmpexpt,status='delete')
     end if
     open(unit=itmpexpt,file=trim(gname),&
     status='unknown',form='unformatted',access='direct',recl=Precordsize,iostat=ierr)
   end if 
end if
  
if ((inRAM.eqv..TRUE.).and.(Px.ne.binx)) then 
  allocate(Pepatterns(Pcorrectsize, totnumexpt))
end if 

!===================================================================================
! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(ebsdnl%exptfile, ebsdnl%ipf_wd, PL, ebsdnl%inputtype, Precordsize, iunitexpt, &
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
allocate(exppatarray(Ppatsz * ebsdnl%ipf_wd),stat=istat)
if (istat .ne. 0) stop 'could not allocate exppatarray'

if (present(exptIQ)) then
! prepare the fftw plan for this pattern size to compute pattern quality (pattern sharpness Q)
  allocate(EBSDPat(Px,Py),stat=istat)
  if (istat .ne. 0) stop 'could not allocate arrays for EBSDPat filter'
  EBSDPat = 0.0
  allocate(ksqarray(Px,Py),stat=istat)
  if (istat .ne. 0) stop 'could not allocate ksqarray array'
  Jres = 0.0
  call init_getEBSDIQ(Px, Py, EBSDPat, ksqarray, Jres, planf)
  deallocate(EBSDPat)
end if

! initialize the HiPassFilter routine (has its own FFTW plans)
allocate(hpmask(Px,Py),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask array'

! use the fftw_alloc routine to create the inp and outp arrays
! using a regular allocate can occasionally cause issues, in particular with 
! the ifort compiler. [MDG, 7/14/20]
ip = fftw_alloc_complex(int(Px*Py,C_SIZE_T))
call c_f_pointer(ip, inp, [Px,Py])

op = fftw_alloc_complex(int(Px*Py,C_SIZE_T))
call c_f_pointer(op, outp, [Px,Py])

inp = cmplx(0.D0,0D0)
outp = cmplx(0.D0,0.D0)

call init_HiPassFilter(w, (/ Px,Py /), hpmask, inp, outp, HPplanf, HPplanb) 

! remove these pointers again because we will need them for the parallel part
call fftw_free(ip)
call fftw_free(op)

call Message('Starting processing of experimental patterns')
call Time_tick(tickstart)

dims3 = (/ Px, Py, ebsdnl%ipf_wd /)

! do we need rebinning arrays ? If so, we need to initialize the rescaler 
! method from the imageOPs module 
io_int = (/ Px, Py /)
call WriteValue(' pattern size : ',io_int,2,"(I4,' x ',I4)")
if (binx.ne.Px) then 
  io_int = (/ binx, biny /)
  call WriteValue(' binned size  : ',io_int,2,"(I4,' x ',I4)")
end if 

Nval = 1.0/float(Px*Py)
Nval2 = 1.0/float(Px*Py-1)

! ===================================================================================
! we do one row at a time
prepexperimentalloop: do iii = iiistart,iiiend

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat, ip, op) &
!$OMP& PRIVATE(tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint, vlen, tmp, inp, outp, mean, sdev)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()

! initialize thread private variables
    allocate(EBSDPat(Px,Py),rrdata(Px,Py),ffdata(Px,Py),tmpimageexpt(PL),stat=istat)
    if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(EBSDpint(Px,Py),stat=istat)
    if (istat .ne. 0) stop 'could not allocate EBSDpint array'

    ip = fftw_alloc_complex(int(Px*Py,C_SIZE_T))
    call c_f_pointer(ip, inp, [Px,Py])

    op = fftw_alloc_complex(int(Px*Py,C_SIZE_T))
    call c_f_pointer(op, outp, [Px,Py])

    inp = cmplx(0.D0,0D0)
    outp = cmplx(0.D0,0.D0)

    tmpimageexpt = 0.0
    rrdata = 0.D0
    ffdata = 0.D0

! thread 0 reads the next row of patterns from the input file
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*ebsdnl%ipf_wd /)
        if (ROIselected.eqv..TRUE.) then
            call getExpPatternRow(iii, ebsdnl%ipf_wd, Ppatsz, PL, dims3, offset3, iunitexpt, &
                                  ebsdnl%inputtype, ebsdnl%HDFstrings, exppatarray, ebsdnl%ROI)
        else
            call getExpPatternRow(iii, ebsdnl%ipf_wd, Ppatsz, PL, dims3, offset3, iunitexpt, &
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
        do kk=1,Py
          EBSDPat(1:Px,kk) = exppatarray((jj-1)*Ppatsz+(kk-1)*Px+1:(jj-1)*Ppatsz+kk*Px)
        end do

        if (present(exptIQ)) then
! compute the pattern Image Quality 
          exptIQ((iii-iiistart)*jjend + jj) = sngl(computeEBSDIQ(Px, Py, EBSDPat, ksqarray, Jres, planf))
        end if

! Hi-Pass filter
        rrdata = dble(EBSDPat)
        ffdata = applyHiPassFilter(rrdata, (/ Px, Py /), w, hpmask, inp, outp, HPplanf, HPplanb)
        EBSDPat = sngl(ffdata)

! in ncc mode, we multiply by the mask before scaling the intensities

        if (ebsdnl%similaritymetric.eq.'ndp') then 
! adaptive histogram equalization
          ma = maxval(EBSDPat)
          mi = minval(EBSDPat)
    
          EBSDpint = nint(((EBSDPat - mi) / (ma-mi))*255.0)
          EBSDPat = float(adhisteq(ebsdnl%nregions,Px,Py,EBSDpint))
        else  ! use normalized cross correlation so subtract mean and divided by standard deviation
          EBSDPat = EBSDPat * mask
          mean = sum(EBSDPat) * Nval
          EBSDPat = EBSDPat - mean 
          sdev = sqrt(Nval2 * sum( EBSDPat*EBSDPat ))
          EBSDPat = EBSDPat / sdev
        end if

! convert back to 1D vector
        do kk=1,Py
          exppatarray((jj-1)*Ppatsz+(kk-1)*Px+1:(jj-1)*Ppatsz+kk*Px) = EBSDPat(1:Px,kk)
        end do

! apply circular mask and normalize for the dot product computation
        if (ebsdnl%similaritymetric.eq.'ndp') then 
          exppatarray((jj-1)*Ppatsz+1:(jj-1)*Ppatsz+PL) = exppatarray((jj-1)*Ppatsz+1:(jj-1)*Ppatsz+PL) * masklin(1:PL)
          vlen = vecnorm(exppatarray((jj-1)*Ppatsz+1:(jj-1)*Ppatsz+PL))
          if (vlen.ne.0.0) then
            exppatarray((jj-1)*Ppatsz+1:(jj-1)*Ppatsz+PL) = exppatarray((jj-1)*Ppatsz+1:(jj-1)*Ppatsz+PL)/vlen
          else
            exppatarray((jj-1)*Ppatsz+1:(jj-1)*Ppatsz+PL) = 0.0
          end if
        end if
    end do
!$OMP END DO

! thread 0 performs the binning if necessary, and then writes the row of patterns 
! to the epatterns array or to the file, depending on the enl%inRAM parameter
    if (TID.eq.0) then
        if (inRAM.eqv..TRUE.) then
          if (Px.eq.binx) then 
            do jj=1,jjend
              epatterns(1:Ppatsz,(iii-iiistart)*jjend + jj) = exppatarray((jj-1)*Ppatsz+1:jj*Ppatsz)
            end do
          else
            do jj=1,jjend
              Pepatterns(1:Ppatsz,(iii-iiistart)*jjend + jj) = exppatarray((jj-1)*Ppatsz+1:jj*Ppatsz)
            end do
          end if
        else
          do jj=1,jjend
            write(itmpexpt,rec=(iii-iiistart)*jjend + jj) exppatarray((jj-1)*Ppatsz+1:jj*Ppatsz)
          end do
        end if
    end if

!$OMP BARRIER
deallocate(tmpimageexpt, EBSDPat, rrdata, ffdata, EBSDpint)
call fftw_free(ip)
call fftw_free(op)

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

! do we need to bin these patterns ?
! we do this after the pre-processing step because there is an interference 
! between the fftw codes in the rescaler routine and the one used in the high 
! pass filter; this is a simple solution to that problem.
if (binx.ne.Px) then 
  call Message(' -> rebinning experimental patterns')
  sclfct = dble(binx)/dble(Px)
  call rescaler%init(Px, Py, sclfct)
  rescaler%wIn = Px 
  rescaler%hIn = Py
  rescaler%wOut = binx
  rescaler%hOut = biny
  allocate(inpat(Px, Py), outpat(binx, biny), pat1D(BL))

  if (inRAM.eqv..FALSE.) then
    open(unit=itmpexpt,file=trim(gname),&
         status='old',form='unformatted',access='direct',recl=Precordsize,iostat=ierr)
    open(unit=itmpexpt2,file=trim(fname),&
         status='unknown',form='unformatted',access='direct',recl=Brecordsize,iostat=ierr)
  end if 

  allocate(tmpimageexpt(Pcorrectsize))
  do jj=1,totnumexpt
! read each pattern from file or memory and bin it, then write it to the output file  or new memory array
    if (inRAM.eqv..FALSE.) then
      read(itmpexpt,rec=jj) tmpimageexpt 
    else 
      tmpimageexpt = Pepatterns(1:Ppatsz,jj)
    end if
! first get the pattern into the inpat array
    inpat = reshape(dble(tmpimageexpt), (/ Px, Py/) )
! rebin the pattern 
    call rescaler%rescale(inpat, outpat, .false.)
! convert outpat to a 1D array for writing to file or epatterns array
    pat1D = sngl(reshape(outpat, (/ BL /) ))
! normalize the pattern 
    if (ebsdnl%similaritymetric.eq.'ndp') pat1D = pat1D/vecnorm(pat1D)
    if (inRAM.eqv..TRUE.) then
      epatterns(1:Bpatsz, jj) = pat1D(1:BL)
    else 
      write(itmpexpt2,rec=jj) pat1D(1:BL)
    end if
  end do 

  if (inRAM.eqv..FALSE.) then
    close(unit=itmpexpt,status='delete')
    close(unit=itmpexpt2,status='keep')
  end if 
end if 

! print some timing information
tstop = Time_tock(tickstart)
if (tstop.eq.0.0) then 
  call Message('Number of experimental patterns processed per second : ? [time shorter than system time resolution] ')
else
  io_real(1) = float(ebsdnl%nthreads) * float(totnumexpt)/tstop
  call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,/)")
end if

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
use FFTW3mod
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
!f2py intent(in,out) ::  epatterns
real(kind=sgl),INTENT(INOUT),OPTIONAL       :: exptIQ(totnumexpt)
!f2py intent(in,out) ::  exptIQ

logical                                     :: ROIselected, f_exists
character(fnlen)                            :: fname
integer(kind=irg)                           :: istat, L, recordsize, io_int(2), patsz, iii, &
                                               iiistart, iiiend, jjend, TID, jj, kk, ierr
integer(HSIZE_T)                            :: dims3(3), offset3(3)
integer(kind=irg),parameter                 :: iunitexpt = 41, itmpexpt = 42
integer(kind=irg)                           :: tickstart, tstop 
real(kind=sgl)                              :: vlen, tmp, ma, mi, io_real(1)
real(kind=dbl)                              :: w, Jres
integer(kind=irg),allocatable               :: TKDpint(:,:)
real(kind=sgl),allocatable                  :: tmpimageexpt(:), TKDPattern(:,:), imageexpt(:), exppatarray(:), TKDpat(:,:)
real(kind=dbl),allocatable                  :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)
complex(kind=dbl),allocatable               :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),pointer           :: inp(:,:), outp(:,:)
type(c_ptr), allocatable                    :: ip, op
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
allocate(hpmask(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask array'

! use the fftw_alloc routine to create the inp and outp arrays
! using a regular allocate can occasionally cause issues, in particular with 
! the ifort compiler. [MDG, 7/14/20]
ip = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
call c_f_pointer(ip, inp, [binx,biny])

op = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
call c_f_pointer(op, outp, [binx,biny])

inp = cmplx(0.D0,0D0)
outp = cmplx(0.D0,0.D0)

call init_HiPassFilter(w, (/ binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 

! remove these pointers again because we will need them for the parallel part
call fftw_free(ip)
call fftw_free(op)

call Message('Starting processing of experimental patterns')
call Time_tick(tickstart)

dims3 = (/ binx, biny, tkdnl%ipf_wd /)

!===================================================================================
! we do one row at a time
prepexperimentalloop: do iii = iiistart,iiiend

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, mi, ma, istat, ip, op) &
!$OMP& PRIVATE(imageexpt, tmpimageexpt, TKDPat, rrdata, ffdata, TKDpint, vlen, tmp, inp, outp)

! set the thread ID
    TID = OMP_GET_THREAD_NUM()

! initialize thread private variables
    allocate(TKDPat(binx,biny),rrdata(binx,biny),ffdata(binx,biny),tmpimageexpt(binx*biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'

    allocate(TKDpint(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate TKDpint array'

    ip = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
    call c_f_pointer(ip, inp, [binx,biny])

    op = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
    call c_f_pointer(op, outp, [binx,biny])

    inp = cmplx(0.D0,0D0)
    outp = cmplx(0.D0,0.D0)

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
        vlen = vecnorm(exppatarray((jj-1)*patsz+1:(jj-1)*patsz+L))
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

deallocate(tmpimageexpt, TKDPat, rrdata, ffdata, TKDpint)
call fftw_free(ip)
call fftw_free(op)

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
tstop = Time_tock(tickstart)
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
