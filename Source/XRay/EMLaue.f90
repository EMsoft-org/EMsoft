! ###################################################################
! Copyright (c) 2019-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMLaue.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMLaue
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic program to compute a realistic transmission/reflection Laue pattern
!
!> @date 07/30/19  MDG 1.0 original version 
!--------------------------------------------------------------------------
program EMLaue

use local
use NameListTypedefs
use NameListHandlers
use files

character(fnlen)                        :: nmldeffile, progname, progdesc
type(LaueNameListType)                  :: lnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMLaue.nml'
progname = 'EMLaue.f90'
progdesc = 'Transmission/reflection Laue pattern computation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 251 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
!  call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetLaueNameList(nmldeffile,lnl)
end if

! generate a realistic Laue master pattern
 call ComputeLauePattern(lnl, progname, nmldeffile)

end program EMLaue

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeLauePattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a series of transmission/reflection Laue patterns
!
!> @param lnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeLauePattern(lnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use NameListHandlers
use initializersHDF
use initializers
use Lauemod
use xrdmod
use symmetry
use crystal
use constants
use error
use gvectors
use kvectors
use io
use local
use files
use timing
use Lambert
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use omp_lib
use notifications
use stringconstants
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

type(LaueNameListType),INTENT(INOUT)       :: lnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

integer(kind=irg)                          :: numangles, numbatches, remainder, ii, jj, pid
integer(kind=irg),allocatable 			   :: batchnumangles(:)
integer(kind=irg),parameter 			   :: batchsize = 100
type(AngleType),pointer                    :: angles

integer(kind=irg)						   :: hdferr, npx, npy, refcnt, io_int(1), NUMTHREADS, TID
real(kind=sgl) 							   :: kouter, kinner, tstart, tstop, mi, ma
real(kind=sgl),allocatable 				   :: pattern(:,:),patternbatch(:,:,:)

type(HDFobjectStackType),pointer           :: HDF_head
type(unitcell),pointer                     :: cell
logical 								   :: verbose, g_exists, insert=.TRUE., overwrite=.TRUE.

type(LaueMasterNameListType)               :: lmnl
type(Laue_g_list),pointer                  :: reflist, rltmp          

character(fnlen) 						   :: hdfname, groupname, datagroupname, attributename, dataset, fname, TIFF_filename
character(11)                              :: dstr
character(15)                              :: tstrb
character(15)                              :: tstre
character(4)							   :: pnum
character(fnlen)						   :: HDF_FileVersion
integer(HSIZE_T)        			       :: dims3(3), cnt3(3), offset3(3)
character(fnlen,kind=c_char)               :: line2(1)

! declare variables for use in object oriented image module
integer                                    :: iostat
character(len=128)                         :: iomsg
logical                                    :: isInteger
type(image_t)                              :: im
integer(int8)                              :: i8 (3,4)
integer(int8), allocatable                 :: TIFF_image(:,:)


nullify(HDF_head)
nullify(cell)
call timestamp(datestring=dstr, timestring=tstrb)
tstre = ''
call cpu_time(tstart)

! read the list of orientations and convert them all to quaternions if they are not already
nullify(angles)
allocate(angles)
call Lauereadangles(lnl%orientationfile, numangles, angles, verbose=.TRUE.)

! compute the limiting wave numbers for the outer and inner Ewald spheres
kouter = getXRDwavenumber(lnl%maxVoltage)
kinner = getXRDwavenumber(lnl%minVoltage)

!=============================================
!=============================================
! crystallography section 
allocate(cell)
verbose = .TRUE.

! clear the cell variable (set everything to zero)
call ResetCell(cell)

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
cell%SG%SYM_reduce=.TRUE.
cell%fname = trim(lnl%xtalname)
call CrystalData(cell,verbose)

! generate all atom positions
call CalcPositions(cell,'v')

!=============================================
!=============================================
! compute possible reflection list with kinematical intensities
nullify(reflist)
lmnl%lambdamin = 1.0/kouter
lmnl%intfactor = 0.0001D0   ! default intensity cutoff factor (from EMLauemaster program)
call Laue_Init_Reflist(cell, lmnl, reflist, refcnt, verbose)



!=============================================
!=============================================
! start creation of the output file, using a hyperslab approach for the Laue patterns 
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Open a new file
  hdfname = trim(EMsoft_getEMdatapathname())//trim(lnl%hdfname)
  hdfname = EMsoft_toNativePath(hdfname)
  hdferr =  HDF_createFile(hdfname, HDF_head)

! write the EMheader to the file
  datagroupname = 'Laue'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_LaueNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteLaueNameList(HDF_head, lnl)

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

! create the Lauemaster group and add a HDF_FileVersion attribbute to it 
  hdferr = HDF_createGroup(datagroupname, HDF_head)
  HDF_FileVersion = '4.0'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! finally, write all the necessary data:  orientations and simulated patterns along with geometrical parameters
dataset = 'kouter'
    call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, kouter, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, kouter, HDF_head)
    end if

dataset = 'kinner'
    call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, kinner, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, kinner, HDF_head)
    end if

! adjust array dimension to be multiple of 100
	npx = lnl%numpx
	npy = lnl%numpy
	if (numangles.lt.batchsize) then 
		numbatches = 1
		allocate(batchnumangles(numbatches))
		batchnumangles(1) = numangles 
		remainder = 0
		allocate(patternbatch(npx, npy, numangles))
	else
		numbatches = numangles/batchsize+1
		allocate(batchnumangles(numbatches))
		batchnumangles(1:numbatches-1) = batchsize
		remainder = mod(numangles,batchsize)
		batchnumangles(numbatches) = remainder
		allocate(patternbatch(npx, npy, batchnumangles(1)))
	end if
	patternbatch = 0.0

dataset = 'numangles'
    call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head)
    end if

! create the hyperslabs and write zeroes to them for now
dataset = 'LauePatterns'
  if (numangles.lt.100) then 
	  dims3 = (/ npx, npy, numangles /)
	  cnt3 = (/ npx, npy, numangles /)
	  offset3 = (/ 0, 0, 0 /)
	  call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
	  if (g_exists) then 
	    hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternbatch, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
	  else
	    hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternbatch, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
	  end if
  else 
	  dims3 = (/ npx, npy, numangles /)
	  cnt3 = (/ npx, npy, batchnumangles(1) /)
	  offset3 = (/ 0, 0, 0 /)
	  call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
	  if (g_exists) then 
	    hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternbatch, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
	  else
	    hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternbatch, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
	  end if
  end if

! leave the output HDF5 file open so that we can write the hyperslabs as they are completed 


!=============================================
!=============================================
! here we perform the actual simulations; we compute batchsize patterns at a time 
! and write them to the HDF5 output file, along with (optionally) the pattern tiff files
! set the number of OpenMP threads 
  call OMP_SET_NUM_THREADS(lnl%nthreads)
  io_int(1) = lnl%nthreads
  call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

! outer loop ... 
  do ii = 1, numbatches

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, pid, jj, pattern)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()
  allocate(pattern(npx,npy))
  patternbatch = 0.0

!$OMP DO SCHEDULE(DYNAMIC)
    do jj = 1,batchnumangles(ii)
      pid = (ii-1) * batchnumangles(1) + jj  
      pattern = getLauePattern(lnl, dble(angles%quatang(1:4,pid)), reflist, kouter, kinner, npx, npy, refcnt)
      patternbatch(1:npx,1:npy,jj) = pattern**lnl%gammavalue
    end do 
!$OMP END DO
    if (TID.eq.0) write (*,*) 'batch ',ii,'; maxval() ',maxval(patternbatch)
    deallocate(pattern)

! end of OpenMP portion
!$OMP END PARALLEL

! write the hyperslab to the HDF5 file 
 if (numangles.lt.100) then 
	dims3 = (/ npx, npy, numangles /)
	cnt3 = (/ npx, npy, numangles /)
	offset3 = (/ 0, 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternbatch, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
  else 
	dims3 = (/ npx, npy, numangles /)
	cnt3 = (/ npx, npy, batchnumangles(ii) /)
	offset3 = (/ 0, 0, (ii-1) * batchnumangles(1) /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternbatch(:,:,1:batchnumangles(ii)), dims3, offset3, &
    	                                    cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
  end if

! optionally, write the individual tiff image files 

! output the ADP map as a tiff file 
  do jj=1,batchnumangles(ii)
  	write (pnum,"(I4.4)") (ii-1) * batchnumangles(1) + jj
	fname = trim(EMsoft_getEMdatapathname())//trim(lnl%tiffprefix)//'_'//pnum//'.tiff'
	fname = EMsoft_toNativePath(fname)
	TIFF_filename = trim(fname)

! allocate memory for image
	allocate(TIFF_image(npx,npy))

! fill the image with whatever data you have (between 0 and 255)
	ma = maxval(patternbatch(:,:,jj))
	mi = minval(patternbatch(:,:,jj))

	TIFF_image = int(255 * (patternbatch(:,:,jj)-mi)/(ma-mi))

! set up the image_t structure
	im = image_t(TIFF_image)
	if(im%empty()) call Message("ComputeLauePattern","failed to convert array to image")

! create the file
	call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
	if(0.ne.iostat) then
	  call Message("failed to write image to file : "//iomsg)
	! else  
	!   call Message('Laue pattern written to '//trim(TIFF_filename))
	end if 
	deallocate(TIFF_image)
  end do

 end do ! outer loop
! 
 call HDF_pop(HDF_head)
 call HDF_pop(HDF_head)
 call timestamp(datestring=dstr, timestring=tstre)

! update the time string
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  call CPU_TIME(tstop)
  tstop = tstop - tstart
  io_int(1) = tstop
  call WriteValue('Execution time [s]: ',io_int,1)

dataset = SC_Duration
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

 call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)

end subroutine ComputeLauePattern
