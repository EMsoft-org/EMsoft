! ###################################################################
! Copyright (c) 2015-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMgetOSM.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgetOSM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute OSM map based on raw and filtered data 
!
!> @date 02/17/18 MDG 1.0 code extracted from EMEBSDDI program 
!--------------------------------------------------------------------------

program EMgetOSM

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use HDF5
use HDFsupport
use EBSDmod
use EBSDDImod
use commonmod
use ISO_C_BINDING
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(OSMNameListType)                       :: osmnl
integer(kind=irg)                           :: istat, res

type(EBSDIndexingNameListType)              :: dinl
type(EBSDDIdataType)                        :: EBSDDIdata
real(kind=sgl),allocatable                  :: OSMmap(:,:)
integer(kind=irg)                           :: dims(2), dimsOSM(2), hdferr, io_int(2), osmnum, i
character(fnlen)                            :: fname, TIFF_filename, dpfile, groupname, dataset
character(2)                                :: fnum
real(kind=sgl)                              :: ma, mi
type(HDFobjectStackType),pointer            :: HDF_head

! declare variables for use in object oriented image module
integer                                     :: iostat
character(len=128)                          :: iomsg
logical                                     :: isInteger
type(image_t)                               :: im
integer(int8)                               :: i8 (3,4)
integer(int8), allocatable                  :: TIFF_image(:,:)

nmldeffile = 'EMgetOSM.nml'
progname = 'EMgetOSM.f90'
progdesc = 'Standalone program to compute Orientation Similarity Map'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 260 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMgetOSM','JSON input not yet implemented')
else
  call GetOSMNameList(nmldeffile,osmnl)
end if


! open the fortran HDF interface and read the TopMatchIndices data set 
call h5open_EMsoft(hdferr)

call readEBSDDotProductFile(osmnl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                            getTopMatchIndices = .TRUE.)

call h5close_EMsoft(hdferr)

! first get the number of different OSM values in the list (non-zero entries)
osmnum = 0
do i=1,5
  if (osmnl%nmatch(i).ne.0) osmnum = osmnum+1
end do

! check to ake sure that the requested osmnl%nmatch values is <= the available number
dims = shape( EBSDDIdata%TopMatchIndices )
do i=1,osmnum
  if (osmnl%nmatch(i).gt.dinl%nnk) then 
   io_int(1) = osmnl%nmatch(i)
   io_int(2) = dims(1)
   call WriteValue(' Number of requested OSM levels = ',io_int, 2, "(I3,'; available number = ',I3)")
   call Message('   --> Resetting requested number to maximum available')
   osmnl%nmatch(i) = dims(1)
  end if
end do

if (sum(dinl%ROI).ne.0) then
  allocate(OSMmap( dinl%ROI(3), dinl%ROI(4) ) )
else
  allocate(OSMmap( dinl%ipf_wd, dinl%ipf_ht ) )
end if
OSMmap = 0.0

! allocate memory for image
dimsOSM = shape(OSMmap)
allocate(TIFF_image( dimsOSM(1), dimsOSM(2) ))

do i=1,osmnum
! compute the Orientation Similarity Map
  if (sum(dinl%ROI).ne.0) then
    call EBSDgetOrientationSimilarityMap( dims, EBSDDIdata%TopMatchIndices, osmnl%nmatch(i), dinl%ROI(3), dinl%ROI(4), OSMmap)
  else
    call EBSDgetOrientationSimilarityMap( dims, EBSDDIdata%TopMatchIndices, osmnl%nmatch(i), dinl%ipf_wd, dinl%ipf_ht, OSMmap)
  end if

  ! output the ADP map as a tiff file 
  write(fnum,"(I2.2)") osmnl%nmatch(i)

! we need to add this as a dataset to the dot product file so that it becomes available 
! to other programs...

  call h5open_EMsoft(hdferr)

  dpfile = trim(EMsoft_getEMdatapathname())//trim(osmnl%dotproductfile)
  dpfile = EMsoft_toNativePath(dpfile)

  nullify(HDF_head)
  hdferr =  HDF_openFile(dpfile, HDF_head)

! open the Scan 1/EBSD/Data group; dictionary indexing files only have one "scan" in them...
  groupname = 'Scan 1'
    hdferr = HDF_openGroup(groupname, HDF_head)
  groupname = SC_EBSD
    hdferr = HDF_openGroup(groupname, HDF_head)
  groupname = SC_Data
    hdferr = HDF_openGroup(groupname, HDF_head)

  dataset = 'OSM_'//fnum
  if (sum(dinl%ROI).ne.0) then
    hdferr = HDF_writeDatasetFloatArray2D(dataset, OSMmap, dinl%ROI(3), dinl%ROI(4), HDF_head)
  else
    hdferr = HDF_writeDatasetFloatArray2D(dataset, OSMmap, dinl%ipf_wd, dinl%ipf_ht, HDF_head)
  end if

! and close the HDF5 dot product file
  call HDF_pop(HDF_head,.TRUE.)

  call h5close_EMsoft(hdferr)

  
  fname = trim(EMsoft_getEMdatapathname())//trim(osmnl%tiffname)//fnum//'.tiff'
  fname = EMsoft_toNativePath(fname)
  TIFF_filename = trim(fname)

  ! fill the image with whatever data you have (between 0 and 255)
  ma = maxval(OSMmap)
  mi = minval(OSMmap)

  TIFF_image = int(255 * (OSMmap-mi)/(ma-mi))
  im = image_t(TIFF_image)
  if(im%empty()) call Message("EMgetOSM","failed to convert array to image")

  ! create the file
  call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
  if(0.ne.iostat) then
    call Message("failed to write image to file : "//iomsg)
  else  
    call Message('OSM map written to '//trim(TIFF_filename))
  end if 

  call im%clear()
  OSMmap = 0.0
end do

end program EMgetOSM

