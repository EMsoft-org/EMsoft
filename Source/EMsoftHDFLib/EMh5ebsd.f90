! ###################################################################
! Copyright (c) 2016-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMh5ebsd.f90
!--------------------------------------------------------------------------
!
! MODULE: support routines to create an .h5ebsd HDF5 file
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @date 02/11/16 MDG 1.0 original
!> @date 03/10/16 MDG 1.1 completion of complete module; needs to be tested.
!> @date 05/07/17 MDG 1.2 added TKD output routines
!> @date 02/22/18 MDG 1.3 added functionality for region-of-interest selection in dictionary indexing
!--------------------------------------------------------------------------
module EMh5ebsd

use local 
use HDF5
use h5im
use h5lt
use HDFsupport
use stringconstants

IMPLICIT NONE

public :: h5ebsd_writeFile, h5tkd_writeFile 

private :: h5ebsd_writeInfo, h5ebsd_write2DImageFromVector, h5ebsd_writeCoordinateSystemGroup, &
           h5ebsd_writePatternCenterGroup, h5ebsd_writePhaseGroup

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:h5ebsd_writeInfo
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write general information fields to the h5ebsd file, including EMsoft specific fields
!
!> @param filetype integer to indicate EBSD, ECP, etc filetypes
!> @param dstr date string
!> @param tstrb begin time string
!> @param tstre end time string
!> @param progname name of the calling program
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5ebsd_writeInfo(filetype, dstr, tstrb, tstre, progname, ebsdnl, nmldeffile, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ebsd_writeInfo

use NameListTypedefs
use NameListHandlers
use NameListHDFwriters

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                        :: filetype
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
character(15),INTENT(IN)                            :: tstre
character(fnlen),INTENT(IN)                         :: progname
type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
character(fnlen),INTENT(IN)                         :: nmldeffile
type(HDFobjectStackType)                            :: HDF_head

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, nmlname, manufacturer
integer(kind=irg)                                   :: hdferr

if (filetype.eq.1) then ! EBSDDictionarIndexing file
  manufacturer = 'EMEBSDDictionaryIndexing.f90'
  nmlname = 'EBSDDictionaryIndexingNML'
else
  manufacturer = ''
  nmlname = ''
end if

allocate(stringarray(1))

! set the Manufacturer and Version data sets
dataset = SC_Manufacturer
  stringarray(1)= trim(manufacturer)
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

dataset = SC_Version
  stringarray(1)= 'EMsoft '//EMsoft_getEMsoftversion()
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! add the EMsoft header group
! write the EMheader to the file
groupname = SC_h5EBSD
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! and write the nml file for this program to the HDF5 file
! read the text file and write the array to the file
  dataset = trim(nmlname)
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (filetype.eq.1) then 
    call HDFwriteEBSDDictionaryIndexingNameList(HDF_head, ebsdnl)
  end if

! leave this group
  call HDF_pop(HDF_head)

end subroutine h5ebsd_writeInfo

!--------------------------------------------------------------------------
!
! SUBROUTINE:h5tkd_writeInfo
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write general information fields to the h5tkd file, including EMsoft specific fields
!
!> @param filetype integer to indicate EBSD, ECP, etc filetypes
!> @param dstr date string
!> @param tstrb begin time string
!> @param tstre end time string
!> @param progname name of the calling program
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5tkd_writeInfo(filetype, dstr, tstrb, tstre, progname, tkdnl, nmldeffile, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5tkd_writeInfo

use NameListTypedefs
use NameListHandlers
use NameListHDFwriters

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                        :: filetype
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
character(15),INTENT(IN)                            :: tstre
character(fnlen),INTENT(IN)                         :: progname
type(TKDIndexingNameListType),INTENT(INOUT)         :: tkdnl
!f2py intent(in,out) ::  tkdnl
character(fnlen),INTENT(IN)                         :: nmldeffile
type(HDFobjectStackType)                            :: HDF_head

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, nmlname, manufacturer
integer(kind=irg)                                   :: hdferr

if (filetype.eq.1) then ! EBSDDictionarIndexing file
  manufacturer = 'EMTKDDI.f90'
  nmlname = 'TKDDictionaryIndexingNML'
else
  manufacturer = ''
  nmlname = ''
end if

allocate(stringarray(1))

! set the Manufacturer and Version data sets
dataset = SC_Manufacturer
  stringarray(1)= trim(manufacturer)
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

dataset = SC_Version
  stringarray(1)= 'EMsoft '//EMsoft_getEMsoftversion()
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! add the EMsoft header group
! write the EMheader to the file
groupname = SC_h5EBSD
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! and write the nml file for this program to the HDF5 file
! read the text file and write the array to the file
  dataset = trim(nmlname)
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (filetype.eq.1) then 
    call HDFwriteTKDDictionaryIndexingNameList(HDF_head, tkdnl)
  end if

! leave this group
  call HDF_pop(HDF_head)

end subroutine h5tkd_writeInfo

!--------------------------------------------------------------------------
!
! SUBROUTINE:h5ebsd_write2DImageFromVector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a gray scale image to the HDF5 file starting from a 1D vector
!
!> @param filetype integer to indicate EBSD, ECP, etc filetypes
!> @param dstr date string
!> @param tstrb begin time string
!> @param tstre end time string
!> @param progname name of the calling program
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5ebsd_write2DImageFromVector(dataset, inpvec, nump, ebsdnl, HDF_head, binary)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ebsd_write2DImageFromVector

use error
use NameListTypedefs

IMPLICIT NONE

character(fnlen),INTENT(IN)                         :: dataset
integer(kind=irg),INTENT(IN)                        :: nump
real(kind=sgl),INTENT(IN)                           :: inpvec(nump)
type(EBSDIndexingNameListType),INTENT(IN)           :: ebsdnl
type(HDFobjectStackType)                            :: HDF_head
real(kind=sgl),OPTIONAL,INTENT(IN)                  :: binary

real(kind=sgl)                                      :: mi, ma
integer(kind=irg)                                   :: istat, ii, jj, hdferr
real(kind=sgl),allocatable                          :: newvec(:)
integer(kind=irg),allocatable                       :: image(:,:)
integer(HSIZE_T)                                    :: width, height
logical                                             :: isbinary

isbinary = .FALSE.
if (present(binary)) isbinary=.TRUE.

allocate(newvec(nump),stat=istat)
if (istat.ne.0) call FatalError('h5ebsd_write2DImageFromVector','Could not allocate array for copy of input image')

newvec = inpvec

if (sum(ebsdnl%ROI).ne.0) then
  width = ebsdnl%ROI(3)
  height = ebsdnl%ROI(4)
else
  width = ebsdnl%ipf_wd
  height = ebsdnl%ipf_ht
end if
allocate(image(width,height),stat=istat)
if (istat.ne.0) call FatalError('h5ebsd_write2DImageFromVector','Could not allocate array for output image')

if (isbinary.eqv..TRUE.) then 
  do jj = 1,height
    do ii = 1, width
      if (newvec((jj-1)*width+ii).gt.ebsdnl%isangle) then
        image(ii,jj) = 0
      else
        image(ii,jj) = 255
      end if 
    end do 
  end do
else
  mi = minval(newvec)
  newvec = newvec - mi
  ma = maxval(newvec)

  do jj = 1,height
    image(1:width,jj) = int(255.0*newvec((jj-1)*width+1:jj*width)/ma)
  end do
end if 

call h5immake_image_8bit_f(HDF_head%next%objectID,dataset,width,height,image,hdferr)
deallocate(image, newvec)

end subroutine h5ebsd_write2DImageFromVector

!--------------------------------------------------------------------------
!
! SUBROUTINE:h5tkd_write2DImageFromVector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a gray scale image to the HDF5 file starting from a 1D vector
!
!> @param filetype integer to indicate EBSD, ECP, etc filetypes
!> @param dstr date string
!> @param tstrb begin time string
!> @param tstre end time string
!> @param progname name of the calling program
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5tkd_write2DImageFromVector(dataset, inpvec, nump, tkdnl, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5tkd_write2DImageFromVector

use error
use NameListTypedefs

IMPLICIT NONE

character(fnlen),INTENT(IN)                         :: dataset
integer(kind=irg),INTENT(IN)                        :: nump
real(kind=sgl),INTENT(IN)                           :: inpvec(nump)
type(TKDIndexingNameListType),INTENT(IN)            :: tkdnl
type(HDFobjectStackType)                            :: HDF_head

real(kind=sgl)                                      :: mi, ma
integer(kind=irg)                                   :: istat, jj, hdferr
real(kind=sgl),allocatable                          :: newvec(:)
integer(kind=irg),allocatable                       :: image(:,:)
integer(HSIZE_T)                                    :: width, height

allocate(newvec(nump),stat=istat)
if (istat.ne.0) call FatalError('h5tkd_write2DImageFromVector','Could not allocate array for copy of input image')

newvec = inpvec

mi = minval(newvec)
newvec = newvec - mi
ma = maxval(newvec)
if (sum(tkdnl%ROI).ne.0) then
  width = tkdnl%ROI(3)
  height = tkdnl%ROI(4)
else
  width = tkdnl%ipf_wd
  height = tkdnl%ipf_ht
end if
allocate(image(width,height),stat=istat)
if (istat.ne.0) call FatalError('h5tkd_write2DImageFromVector','Could not allocate array for output image')

do jj = 1,height
  image(1:width,jj) = int(255.0*newvec((jj-1)*width+1:jj*width)/ma)
end do

call h5immake_image_8bit_f(HDF_head%next%objectID,dataset,width,height,image,hdferr)
deallocate(image, newvec)

end subroutine h5tkd_write2DImageFromVector


!--------------------------------------------------------------------------
!
! SUBROUTINE:h5ebsd_writeCoordinateSystemGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a gray scale image to the HDF5 file starting from a 1D vector
!
!> @param HDF_head pointer to HDF push-pop stack
!
!> @date 02/14/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5ebsd_writeCoordinateSystemGroup(HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ebsd_writeCoordinateSystemGroup

use error

IMPLICIT NONE

type(HDFobjectStackType)                            :: HDF_head

character(fnlen)                                    :: groupname, dataset, fname, resourcepathname
integer(kind=irg)                                   :: hdferr
integer(HSIZE_T),allocatable                        :: EBSDview(:,:,:), schematic(:,:,:)
character(1),allocatable                            :: chararr(:,:,:)
integer(kind=irg)                                   :: dims(3), istat
integer(HSIZE_T)                                    :: width, height

! create the Coordinate System group
groupname = 'Coordinate System'
hdferr = HDF_createGroup(groupname, HDF_head)
resourcepathname = EMsoft_getResourcepathname()

!=====================================================
! EBSD View Reference Frame
fname = trim(resourcepathname)//'EBSDview.data'
fname = EMsoft_toNativePath(fname)
open(unit=50,file=trim(fname),status='old',form='unformatted')
read(50) dims
allocate(EBSDview(dims(1),dims(2),dims(3)),chararr(dims(1),dims(2),dims(3)),stat=istat)
if (istat.ne.0) call FatalError('h5ebsd_writeCoordinateSystemGroup','Could not allocate array for EBSD view output image')
read(50) chararr
close(unit=50,status='keep')
EBSDview = ichar(chararr)

dataset = 'EBSD View Reference Frame'
width = dims(2)
height = dims(3)
call h5immake_image_24bit_f(HDF_head%next%objectID,dataset,width,height,'INTERLACE_PIXEL',int(EBSDview),hdferr)
deallocate(EBSDview,chararr)

!=====================================================
! Schematic 1
fname = trim(resourcepathname)//'Schematic1.data'
fname = EMsoft_toNativePath(fname)
open(unit=50,file=fname,status='old',form='unformatted')
read(50) dims
allocate(schematic(dims(1),dims(2),dims(3)),chararr(dims(1),dims(2),dims(3)),stat=istat)
if (istat.ne.0) call FatalError('h5ebsd_writeCoordinateSystemGroup','Could not allocate array for Schematic output image')
read(50) chararr
close(unit=50,status='keep')
schematic = ichar(chararr)

dataset = 'Schematic 1'
width = dims(2)
height = dims(3)
call h5immake_image_24bit_f(HDF_head%next%objectID,dataset,width,height,'INTERLACE_PIXEL',int(schematic),hdferr)

!=====================================================
! Schematic 2
fname = trim(resourcepathname)//'Schematic2.data'
fname = EMsoft_toNativePath(fname)
open(unit=50,file=fname,status='old',form='unformatted')
read(50) dims
read(50) chararr
close(unit=50,status='keep')
schematic = ichar(chararr)

dataset = 'Schematic 2'
width = dims(2)
height = dims(3)
call h5immake_image_24bit_f(HDF_head%next%objectID,dataset,width,height,'INTERLACE_PIXEL',int(schematic),hdferr)

!=====================================================
! Schematic 3
fname = trim(resourcepathname)//'Schematic3.data'
fname = EMsoft_toNativePath(fname)
open(unit=50,file=fname,status='old',form='unformatted')
read(50) dims
read(50) chararr
close(unit=50,status='keep')
schematic = ichar(chararr)

dataset = 'Schematic 3'
width = dims(2)
height = dims(3)
call h5immake_image_24bit_f(HDF_head%next%objectID,dataset,width,height,'INTERLACE_PIXEL',int(schematic),hdferr)

!=====================================================
! Schematic 4
fname = trim(resourcepathname)//'Schematic4.data'
fname = EMsoft_toNativePath(fname)
open(unit=50,file=fname,status='old',form='unformatted')
read(50) dims
read(50) chararr
close(unit=50,status='keep')
schematic = ichar(chararr)

dataset = 'Schematic 4'
width = dims(2)
height = dims(3)
call h5immake_image_24bit_f(HDF_head%next%objectID,dataset,width,height,'INTERLACE_PIXEL',int(schematic),hdferr)

deallocate(schematic,chararr)
!=====================================================
! and finally the selected type
dataset = SC_ID
hdferr = HDF_writeDatasetInteger(dataset, 2, HDF_head)
 
call HDF_pop(HDF_head)

end subroutine h5ebsd_writeCoordinateSystemGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE: h5ebsd_writePatternCenterGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write the pattern center group
!
!> @param HDF_head pointer to HDF push-pop stack
!
!> @date 02/14/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5ebsd_writePatternCenterGroup(xpc, ypc, L, delta, scdim, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ebsd_writePatternCenterGroup

use error

IMPLICIT NONE

real(kind=sgl),INTENT(IN)                           :: xpc      ! pattern center x [pixels]
real(kind=sgl),INTENT(IN)                           :: ypc      ! pattern center y [pixels]
real(kind=sgl),INTENT(IN)                           :: L        ! sample-scintillator distance [micron]
real(kind=sgl),INTENT(IN)                           :: delta    ! scintillator pixel size [micron]
integer(kind=irg),INTENT(IN)                        :: scdim(2) ! scintillator dimensions [pixels]

type(HDFobjectStackType)                            :: HDF_head

character(fnlen)                                    :: groupname, dataset
integer(kind=irg)                                   :: hdferr
real(kind=sgl)                                      :: xstar, ystar, zstar

! create the Coordinate System group
groupname = 'Pattern Center Calibration'
hdferr = HDF_createGroup(groupname, HDF_head)

! we assume that we are writing a TSL file

! in EMsoft, the pattern center is measured in units of pixels from the 
! center of the scintillator.  For TSL, the pattern center is measured 
! from the bottom left of the scintillator (when looking towards it from the 
! sample) and in units of the width of the scintillator.

xstar = ( float(scdim(1))*0.5 + xpc ) / float(scdim(1)) 
ystar = ( float(scdim(2))*0.5 + ypc ) / float(scdim(2)) 
zstar = L / ( delta * float(scdim(1)) )

dataset = SC_xstar
hdferr = HDF_writeDatasetFloat(dataset, xstar, HDF_head)

dataset = SC_ystar
hdferr = HDF_writeDatasetFloat(dataset, ystar, HDF_head)

dataset = SC_zstar
hdferr = HDF_writeDatasetFloat(dataset, zstar, HDF_head)

call HDF_pop(HDF_head)

end subroutine  h5ebsd_writePatternCenterGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE:h5ebsd_writePhaseGroup
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write the phase group, describing the crystal structure
!
!> @param HDF_head pointer to HDF push-pop stack
!
!> @date 02/14/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine h5ebsd_writePhaseGroup(groupname, xtalname, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ebsd_writePhaseGroup

use error
use io
use typedefs

IMPLICIT NONE

character(fnlen),intent(IN)                         :: groupname
character(fnlen),intent(IN)                         :: xtalname
type(HDFobjectStackType)                            :: HDF_head

character(fnlen)                                    :: dataset, grname, filename
integer(kind=irg)                                   :: istat, SGnum, hdferr
real(kind=dbl),allocatable                          :: cellparams(:)
integer(HSIZE_T)                                    :: dims(1)
logical                                             :: readonly, stat

integer(kind=irg)                                   :: i, pgnum

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

! new HDF_head pointer to handle a new HDF5 while another one is already open ...
type(HDFobjectStackType)                            :: HDF_head_local

! TSL point group labels [courtesy of S. Wright]
character(26),parameter       :: TSLpgname(32) = (/ "Triclinic (C1) [1]        ", "Triclinic (S2, Ci) [-1]   ",&
                        "Monoclinic b (C2)[2]      ", "Monoclinic b (C1h, Cs) [m]", "Monoclinic b (C2h) [2/m]  ",&
                        "Orthorhombic (D2) [222]   ", "Orthorhombic (C2v) [mm2]  ", "Orthorhombic (D2h) [mmm]  ",&
                        "Tetragonal (C4) [4]       ", "Tetragonal (S4) [-4]      ", "Tetragonal (C4h) [4/m]    ",&
                        "Tetragonal (D4) [422]     ", "Tetragonal (C4v) [4mm]    ", "Tetragonal (D2d) [-42m]   ",&
                        "Tetragonal (D4h) [4/mmm]  ", "Trigonal (C3) [3]         ", "Trigonal (S6, C3i) [-3]   ",&
                        "Trigonal (D3) [32]        ", "Trigonal (C3v) [3m]       ", "Trigonal (D3d) [-3m]      ",&
                        "Hexagonal (C6) [6]        ", "Hexagonal (C3h) [-6]      ", "Hexagonal (C6h) [6/m]     ",&
                        "Hexagonal (D6) [622]      ", "Hexagonal (C6v) [6mm]     ", "Hexagonal (D3h) [-6m2]    ",&
                        "Hexagonal (D6h) [6/mmm]   ", "Cubic (T) [23]            ", "Cubic (Th) [m3]           ",&
                        "Cubic (O) [432]           ", "Cubic (Td) [-43m]         ", "Cubic (Oh) [m3m]          " /)

! TSL old symmetry identifiers [courtesy of S. Wright]
integer(kind=irg),parameter    :: TSLoldID(32) = (/ 1,1,2,2,2,22,22,22,4,4,4,42,42,42,42,3,3, &
                                                  32,32,32,6,6,6,62,62,62,62,23,23,43,43,43 /)


! this routine first extracts information from the xtal file and then
! puts it in the right format for the h5ebsd file format.
! This is organized by phase, so each phase is a separate numbered
! subgroup; the subgroupname is passed in as groupname

! test to make sure the input file exists and is HDF5 format
filename = trim(EMsoft_getXtalpathname())//trim(xtalname)
filename = EMsoft_toNativePath(filename)

stat = .FALSE.

call h5fis_hdf5_f(filename, stat, hdferr)

if (stat) then
  nullify(HDF_head_local%next)

! open the xtal file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(filename, HDF_head_local, readonly)

! open the namelist group
  grname = 'CrystalData'
  hdferr = HDF_openGroup(grname, HDF_head_local)

! get the lattice parameters
dataset = SC_LatticeParameters
  call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head_local, hdferr, cellparams) 

! get the spacegroupnumber
dataset = SC_SpaceGroupNumber
  call HDF_readDatasetInteger(dataset, HDF_head_local, hdferr, SGnum)

! and close the xtal file
  call HDF_pop(HDF_head_local,.TRUE.)
else
  call FatalError('h5ebsd_writePhaseGroup','Error reading xtal file '//trim(filename))
end if

! create the subgroup [now we are back in the original HDF5 file]
hdferr = HDF_createGroup(groupname, HDF_head)

! the following data sets need to be created: Formula, Info, Lattice Constant a,
! b, c, alpha, beta, gamma, Laue Group, MaterialName, NumberFamilies, Point Group,
! Symmetry, hkl Families.  These last ones are typically used by the EDAX/TSL 
! software, so we do not necessarily have to fill them in here.

! lattice parameters [in Angstrom]
dataset = 'Lattice Constant a'
hdferr = HDF_writeDatasetFloat(dataset, sngl(cellparams(1))*10.0, HDF_head)
dataset = 'Lattice Constant b'
hdferr = HDF_writeDatasetFloat(dataset, sngl(cellparams(2))*10.0, HDF_head)
dataset = 'Lattice Constant c'
hdferr = HDF_writeDatasetFloat(dataset, sngl(cellparams(3))*10.0, HDF_head)
dataset = 'Lattice Constant alpha'
hdferr = HDF_writeDatasetFloat(dataset, sngl(cellparams(4)), HDF_head)
dataset = 'Lattice Constant beta'
hdferr = HDF_writeDatasetFloat(dataset, sngl(cellparams(5)), HDF_head)
dataset = 'Lattice Constant gamma'
hdferr = HDF_writeDatasetFloat(dataset, sngl(cellparams(6)), HDF_head)

allocate(stringarray(1))

! point group
pgnum = 0
do i=1,32
  if (SGPG(i).le.sgnum) pgnum = i
end do
dataset = 'Point Group'
stringarray(1)= trim(TSLpgname(pgnum))
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! Laue group
dataset = 'Laue Group'
stringarray(1)= trim(TSLpgname(PGrot(pgnum)))
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! Symmetry
dataset = SC_Symmetry
hdferr = HDF_writeDatasetInteger(dataset, TSLoldID(pgnum), HDF_head)

! various other strings

! Formula [extract this from the first part of xtalname]
dataset = SC_Formula
i = scan(trim(xtalname),'.')
stringarray(1) = xtalname(1:i-1)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! Material name [same as Formula; this would require adding a field to the .xtal files]
dataset = SC_MaterialName
stringarray(1) = xtalname(1:i-1)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! Info [empty string most of the time]
dataset = SC_Info
stringarray(1) = ''
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! hkl Families [this will require a bit of work !!!!!]
! this item uses the Compound data type; we will need to generate the 
! families of unique planes, and compute structure factors ... 

! in this version of the software [EMsoft 3.1], we leave these datasets empty
dataset = SC_NumberFamilies
i = 0
hdferr = HDF_writeDatasetInteger(dataset, i, HDF_head)
! call Message('h5ebsd_writePhaseGroup: writing of ->NumberFamilies<- data not yet implemented.')

dataset = 'hkl Families'
hdferr = HDF_writeDatasetInteger(dataset, i, HDF_head)
! call Message('h5ebsd_writePhaseGroup: writing of ->hkl Families<- data not yet implemented.')


! and leave this group
call HDF_pop(HDF_head)

end subroutine h5ebsd_writePhaseGroup

!--------------------------------------------------------------------------
!
! SUBROUTINE: h5ebsd_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write an .h5ebsd file
!
!> @param vendor vendor string 'TSL', 'HKL', 'BRU'; only 'TSL' implemented for now
!> @param ebsdnl input name list
!
!> @date 03/10/16 MDG 1.0 original
!> @date 05/19/19 MDG 1.1 disable option to compute orientation average map
!--------------------------------------------------------------------------
subroutine h5ebsd_writeFile(vendor, ebsdnl, xtalname, dstr, tstrb, ipar, resultmain, exptIQ, indexmain, dicteulerarray, &
                            dpmap, progname, nmldeffile, OSMmap)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ebsd_writeFile

use NameListTypedefs
use io
use constants
use ebsdiomod
use ebsdmod
!use ebsddimod
use dictmod
use commonmod

IMPLICIT NONE

character(3),INTENT(IN)                             :: vendor   ! 'TSL' 'HKL' 'BRU'
type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
character(fnlen),INTENT(IN)                         :: xtalname
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
integer(kind=irg),INTENT(INOUT)                     :: ipar(10)
!f2py intent(in,out) ::  ipar
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: exptIQ(ipar(3))
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(INOUT)                        :: dicteulerarray(3,ipar(4))
!f2py intent(in,out) ::  dicteulerarray
real(kind=sgl),INTENT(IN)                           :: dpmap(ipar(3))
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile
real(kind=sgl),INTENT(OUT)                          :: OSMmap(ipar(7),ipar(8))

character(15)                                       :: tstre
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
integer(kind=irg)                                   :: hdferr, filetype, i, j, ii, jj,indx, istat, ipar2(6), L
character(fnlen)                                    :: groupname, dataset, h5ebsdfile, savefile
logical                                             :: noindex, g_exists, overwrite=.TRUE.
type(dicttype)                                      :: dict
real(kind=sgl)                                      :: eulerarray(3,ipar(4))

real(kind=sgl),allocatable                          :: kam(:,:), ISMap(:)

real(kind=sgl),allocatable                          :: exptCI(:), eangle(:), eangles(:,:), results(:), avEuler(:,:), &
                                                       lresultmain(:,:), eulers(:,:) 
integer(kind=1),allocatable                         :: iPhase(:), valid(:)
integer(kind=irg),allocatable                       :: SEMsignal(:), lindexmain(:,:)
real(kind=sgl)                                      :: isratio, io_real(1)
type(HDFobjectStackType)                            :: HDF_head

! copy the dictionary euler angle array 
eulerarray = dicteulerarray

!=====================================================
! write the output in the format of an h5ebsd file
!!!! THIS PART IS STILL UNDER DEVELOPMENT !!!!
! we use the TSL h5ebsd file as a template for now; this 
! can be extended later other vendor formats
!=====================================================

if (vendor.ne.'TSL') then
  call Message('Only TSL h5ebsd file format is implemented in this version.')
  call Message('Program results will be saved in this format.')
end if

allocate(stringarray(1))

  nullify(HDF_head%next)
  call timestamp(timestring=tstre)

! Create a new file using the default properties.
  h5ebsdfile = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%datafile)
  h5ebsdfile = EMsoft_toNativePath(h5ebsdfile)
  hdferr =  HDF_createFile(h5ebsdfile, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening file')
  filetype = 1

  call h5ebsd_writeInfo(filetype, dstr, tstrb, tstre, progname, ebsdnl, nmldeffile, HDF_head)

! here we start with the h5ebsd-specific stuff
  groupname = 'Scan 1'
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group Scan 1')

groupname = SC_EBSD
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group EBSD')

!=====================================================
!=====================================================
groupname = SC_Data
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group Data')

! there are 15 datasets in this Data group: CI, Fit, IQ, PRIAS Bottom Strip,
! PRIAS Center Square, PRIAS Top Strip, Pattern, Phase, Phi, Phi1, Phi2,
! SEM Signal, Valid, X Position, Y Position

!=====================================================
! CI Confidence Index: real(kind=sgl), one for each pattern... we take this
! to be the largest dot product
dataset = SC_CI
  allocate(exptCI(ipar(3)))
  exptCI = resultmain(1,1:ipar(3))
  hdferr = HDF_writeDatasetFloatArray1D(dataset, exptCI, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset CI')

! we also insert a visual map of the Confidence Index, resampled on a rectangular array
dataset = SC_CIMap
  call h5ebsd_write2DImageFromVector(dataset, exptCI, ipar(3), ebsdnl, HDF_head)
  deallocate(exptCI)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset CImap')

!=====================================================
! Fit 
dataset = SC_Fit
  allocate(eangle(ipar(3)),stat=istat)
  eangle = 1.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Fit')
  deallocate(eangle)

!=====================================================
! this option is disabled starting in version 4.3 [05/19/19, MDG]; code block can be deleted
! Averaged Orientation Map (using near-match list and quaternion logarithm averaging)
! define the ipar2 entries
!   allocate(avEuler(3,ipar(3)))
  ipar2(1) = ipar(6)
  ipar2(2) = ipar(5)
  ipar2(3) = ipar(3)
  ipar2(4) = ipar(1)
  ipar2(5) = ipar(2)
  ipar2(6) = ebsdnl%nnav
! ! get the avEuler array
!   eulerarray = eulerarray * sngl(cPi)/180.0
!   call EBSDgetAverageOrientations(ipar2, eulerarray, indexmain(1:ipar2(4),1:ipar2(5)), resultmain(1:ipar2(4),1:ipar2(5)), &
!                                   avEuler)
!   eulerarray = eulerarray * 180.0/sngl(cPi)

! ! and write it to the HDF file
! dataset = SC_AverageOrientations
!   hdferr = HDF_writeDatasetFloatArray2D(dataset, avEuler, 3, ipar(3), HDF_head)
!   if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset AverageOrientations')

! get the nearest neighbor Kernel Average Misorientation Map (KAM)
  allocate(eulers(3,ipar(3)))
  do i=1,ipar(3)
    eulers(1:3,i) = eulerarray(1:3,indexmain(1,i))
  end do
  eulers = eulers*sngl(cPi)/180.0
  dict%Num_of_init = 3
  dict%Num_of_iterations = 30
  dict%pgnum = ipar2(1)
  call DI_Init(dict,'nil') 
dataset = SC_KAM
  if (sum(ebsdnl%ROI).ne.0) then
    allocate(kam(ebsdnl%ROI(3),ebsdnl%ROI(4)))
    call EBSDgetKAMMap(ipar(3), eulers, ebsdnl%ROI(3), ebsdnl%ROI(4), dict, kam)
    kam = kam*180.0/sngl(cPi)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, kam, ebsdnl%ROI(3), ebsdnl%ROI(4), HDF_head)
  else
    allocate(kam(ebsdnl%ipf_wd,ebsdnl%ipf_ht))
    call EBSDgetKAMMap(ipar(3), eulers, ebsdnl%ipf_wd, ebsdnl%ipf_ht, dict, kam)
    kam = kam*180.0/sngl(cPi)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, kam, ebsdnl%ipf_wd, ebsdnl%ipf_ht, HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset KAM')
  deallocate(kam, eulers)

! get the Orientation Similarity Map (OSM); map is now returned to calling routine [MDG, 3/5/18]
dataset = SC_OSM
  if (sum(ebsdnl%ROI).ne.0) then
!   allocate(osm(ebsdnl%ROI(3),ebsdnl%ROI(4)))
    call EBSDgetOrientationSimilarityMap( (/ipar(1), ipar(2)/), indexmain, ebsdnl%nosm, ebsdnl%ROI(3), ebsdnl%ROI(4), OSMmap)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, OSMmap, ebsdnl%ROI(3), ebsdnl%ROI(4), HDF_head)
  else
!   allocate(osm(ebsdnl%ipf_wd,ebsdnl%ipf_ht))
    call EBSDgetOrientationSimilarityMap( (/ipar(1), ipar(2)/), indexmain, ebsdnl%nosm, ebsdnl%ipf_wd, ebsdnl%ipf_ht, OSMmap)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, OSMmap, ebsdnl%ipf_wd, ebsdnl%ipf_ht, HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset OSM')

! this option is disabled starting in version 4.3 [05/1/19, MDG]; code can be deleted
! also create a second ctf file, if requested
  ! if (trim(ebsdnl%avctffile).ne.'undefined') then
  !   savefile = ebsdnl%ctffile
  !   ebsdnl%ctffile = ebsdnl%avctffile
  !   ipar2(1:6) = ipar(1:6)
  !   ipar(1) = 1
  !   ipar(2) = ipar2(3)
  !   ipar(3) = ipar2(3)
  !   ipar(4) = ipar2(3)
  
  !   allocate(lindexmain(1,ipar2(3)), lresultmain(1,ipar2(3)))
  !   lindexmain = 0
  !   lresultmain(1,1:ipar2(3)) = resultmain(1,1:ipar2(3))
  !   noindex = .TRUE.

  !   call ctfebsd_writeFile(ebsdnl,xtalname,ipar,lindexmain,avEuler,lresultmain,OSMmap,exptIQ,noindex)
  !   call Message('Average orientation data stored in ctf file : '//trim(ebsdnl%avctffile))
  !   ebsdnl%ctffile = savefile
  !   ipar(1:6) = ipar2(1:6)
  !   deallocate(lindexmain, lresultmain)
  ! end if

! we also insert a visual map of the Confidence Index, resampled on a rectangular array
! dataset = 'FitMap'
! call h5ebsd_write2DImageFromVector(dataset, totnumexpt, exptCI, ebsdnl, HDF_head)

!=====================================================
! IQ Image Quality; computed using the second moment of the pattern power spectrum
dataset = SC_IQ
  hdferr = HDF_writeDatasetFloatArray1D(dataset, exptIQ, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset IQ')
  
! we also insert a visual map of the Image Quality, resampled on a rectangular array
dataset = SC_IQMap
  call h5ebsd_write2DImageFromVector(dataset, exptIQ, ipar(3), ebsdnl, HDF_head)

!=====================================================
! generate the indexing success map (ISM) 
  eulerarray = eulerarray * sngl(cPi)/180.0
  allocate(ISMap(ipar(7)*ipar(8)))
  call EBSDgetIndexingSuccessMap(ipar, indexmain, eulerarray, ebsdnl, ISMap)
  eulerarray = eulerarray * 180.0/sngl(cPi)

dataset = SC_ISM
  hdferr = HDF_writeDatasetFloatArray1D(dataset, ISMap, ipar(7)*ipar(8), HDF_head)

dataset = SC_ISMap
  call h5ebsd_write2DImageFromVector(dataset, ISMap, ipar(7)*ipar(8), ebsdnl, HDF_head, binary=ebsdnl%isangle)
  j = 0
  do i=1,ipar(7)*ipar(8)
    if (ISMap(i).le.ebsdnl%isangle) j = j+1
  end do 
  isratio = 100.0 * real(j) / real(ipar(7)*ipar(8))
  io_real(1) = isratio 
  call WriteValue('Indexing Success Rate (%) : ',io_real,1)
  deallocate(ISMap)

dataset = SC_ISR 
  hdferr = HDF_writeDatasetFloat(dataset, isratio, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset ISR')
 

!=====================================================
! PRIAS Bottom Strip: to be implemented
!   call Message('h5ebsd_writeFile: writing of ->PRIAS Bottom Strip<- data not yet implemented.')

!=====================================================
! PRIAS Center Square: to be implemented
!   call Message('h5ebsd_writeFile: writing of ->PRIAS Center Strip<- data not yet implemented.')

!=====================================================
! PRIAS Top Strip: to be implemented
!   call Message('h5ebsd_writeFile: writing of ->PRIAS Top Strip<- data not yet implemented.')

!=====================================================
! Pattern: in principle, this is where the fitted patterns could be stored
! This will require re-computing them for the best match orientations; we 
! could leave this as an option for the user, to be implemented.
!   call Message('h5ebsd_writeFile: writing of ->Pattern<- data not yet implemented.')

!=====================================================
! Phase: Phase identifier (all zero for now)
dataset = SC_Phase
  allocate(iPhase(ipar(3)),stat=istat)
  iPhase = 0
  hdferr = HDF_writeDatasetInteger1byteArray1D(dataset, iPhase, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phase')
  deallocate(iPhase)

!=====================================================
! SEM Signal: all 0 for now
dataset = SC_SEMSignal
  allocate(SEMsignal(ipar(3)),stat=istat)
  SEMsignal = 10000
  hdferr = HDF_writeDatasetIntegerArray1D(dataset, SEMsignal, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset SEM Signal')
  deallocate(SEMsignal)

!=====================================================
! Valid : all 0 for now
dataset = SC_Valid
  allocate(valid(ipar(3)),stat=istat)
  valid = 0
  hdferr = HDF_writeDatasetInteger1byteArray1D(dataset, valid, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Valid')
  deallocate(valid)

dataset = SC_EulerAngles
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  allocate(eangles(3,ipar(3)),stat=istat)
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangles(1:3,ii) = eulerarray(1:3,indx)
  end do
  eangles = eangles * sngl(cPi)/180.0
  if (g_exists) then 
     hdferr = HDF_writeDatasetFloatArray2D(dataset, eangles, 3, ipar(3), HDF_head, overwrite)
  else
     hdferr = HDF_writeDatasetFloatArray2D(dataset, eangles, 3, ipar(3), HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset EulerAngles')

!=====================================================
! Euler angles: Phi 
dataset = SC_Phi
  allocate(eangle(ipar(3)),stat=istat)
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(2,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi')

!=====================================================
! Euler angles: Phi1
dataset = SC_Phi1
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(1,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi1')

!=====================================================
! Euler angles: Phi2 
dataset = SC_Phi2
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(3,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi2')
  deallocate(eangle)
 
!=====================================================
! X Position: list of x positions for sampling points; requires knowledge of step size
! from Header
dataset = SC_XPos
  allocate(results(ipar(3)),stat=istat)
  if (sum(ebsdnl%ROI).eq.0) then
    do jj=1,ebsdnl%ipf_ht
      do ii=1,ebsdnl%ipf_wd
        results(ebsdnl%ipf_wd*(jj-1)+ii) = (ii-1)*ebsdnl%StepX 
      end do
    end do
  else
    do jj=1,ebsdnl%ROI(4)
      do ii=1,ebsdnl%ROI(3)
        results(ebsdnl%ROI(3)*(jj-1)+ii) = (ii-1)*ebsdnl%StepX 
      end do
    end do
  end if
  hdferr = HDF_writeDatasetFloatArray1D(dataset, results, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset X Position')

 
!=====================================================
! Y Position: list of y positions for sampling points; requires knowledge of step size
! from Header
dataset = SC_YPos
  if (sum(ebsdnl%ROI).eq.0) then
    do jj=1,ebsdnl%ipf_ht
      do ii=1,ebsdnl%ipf_wd
        results(ebsdnl%ipf_wd*(jj-1)+ii) = (ii-1)*ebsdnl%StepY 
      end do
    end do
  else
    do jj=1,ebsdnl%ROI(4)
      do ii=1,ebsdnl%ROI(3)
        results(ebsdnl%ROI(3)*(jj-1)+ii) = (ii-1)*ebsdnl%StepY 
      end do
    end do
  end if
  hdferr = HDF_writeDatasetFloatArray1D(dataset, results, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Y position')
  deallocate(results)
 

!=====================================================
!=====================================================
! this concludes the standard data sets in the Data group
! here, we have additional data sets based on results from the 
! dictionary indexing program; these are not part of the standard
! TSL h5ebsd file format.
!=====================================================
! EBSD average dot product map 
dataset = SC_AvDotProductMap
  call h5ebsd_write2DImageFromVector(dataset, dpmap, ipar(3), ebsdnl, HDF_head)

! number of samples in dictionary
dataset = SC_FZcnt
  hdferr = HDF_writeDatasetInteger(dataset, ipar(5), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset FZcnt')

! point group number
dataset = SC_PointGroupNumber
  hdferr = HDF_writeDatasetInteger(dataset, ipar(6), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset PointGroupNumber')

! Ncubochoric
dataset = SC_Ncubochoric
  hdferr = HDF_writeDatasetInteger(dataset, ebsdnl%ncubochoric, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Ncubochoric')

! write the list of sampled Euler angles
dataset = SC_DictionaryEulerAngles
  hdferr = HDF_writeDatasetFloatArray2D(dataset, dicteulerarray, 3, ipar(4), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset EulerAngles')

! number of experimental patterns 
dataset = SC_NumExptPatterns
  hdferr = HDF_writeDatasetInteger(dataset, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset NumExptPatterns')

! list of top nnk dot product values
dataset = SC_TopDotProductList
  hdferr = HDF_writeDatasetFloatArray2D(dataset, resultmain, ipar(1), ipar(2), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopDotProductList')

! indices of top matches into Euler angle list
dataset = SC_TopMatchIndices
  hdferr = HDF_writeDatasetIntegerArray2D(dataset, indexmain, ipar(1), ipar(2), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopMatchIndices')

! leave this group
  call HDF_pop(HDF_head)
!=====================================================
!=====================================================

!=====================================================
!=====================================================
! create the Header group
groupname = SC_Header
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error creating group Header')

! there are 15 datasets in this group: Camera Azimuth Angle, Camera Elevation Angle,
! Grid Type, Notes, Operator, Pattern Height, Pattern Width, Sample ID, Sample Tilt,
! Scan ID, Step X, Step Y, Working Distance, nColumns, nRows
! there are also 3 groups: Coordinate System, Pattern Center Calibration, and Phase

!=====================================================
! Camera Azimuthal Angle
dataset = SC_CameraAzimuthalAngle
  hdferr = HDF_writeDatasetFloat(dataset, 0.0, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Camera Azimuthal Angle')
 
!=====================================================
! Camera Elevation Angle
dataset = SC_CameraElevationAngle
  hdferr = HDF_writeDatasetFloat(dataset, ebsdnl%thetac, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopMatchIndices')
 
!=====================================================
! Coordinate System group
  call h5ebsd_writeCoordinateSystemGroup(HDF_head)

!=====================================================
! Grid Type
dataset = SC_GridType
  stringarray(1) = 'SqrGrid'
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Grid Type')

!=====================================================
! Notes
dataset = SC_Notes
  stringarray(1) = ''
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Notes')

!=====================================================
! Operator
dataset = SC_Operator
  stringarray(1) = trim(EMsoft_getUsername())//' ['//trim(EMsoft_getUseremail())//']'
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Operator')

!=====================================================
! Pattern Center Calibration group
  call h5ebsd_writePatternCenterGroup(ebsdnl%xpc, ebsdnl%ypc, ebsdnl%L, ebsdnl%delta, (/ebsdnl%numsx, ebsdnl%numsy/), HDF_head)

!=====================================================
! Pattern height
dataset = SC_PatternHeight
  hdferr = HDF_writeDatasetInteger(dataset, ebsdnl%numsx/ebsdnl%binning, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Pattern Height')

!=====================================================
! Pattern width
dataset = SC_PatternWidth
  hdferr = HDF_writeDatasetInteger(dataset, ebsdnl%numsy/ebsdnl%binning, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Pattern Width')

!=====================================================
! Phase group
groupname = SC_Phase
  hdferr = HDF_createGroup(groupname, HDF_head)
groupname = "1"
  call h5ebsd_writePhaseGroup(groupname, xtalname, HDF_head)

! close the Phase group
  call HDF_pop(HDF_head)

!=====================================================
! Sample ID
dataset = SC_SampleID
  stringarray(1) = ''
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

!=====================================================
! Sample Tilt
dataset = SC_SampleTilt
  hdferr = HDF_writeDatasetFloat(dataset, sngl(ebsdnl%MCsig), HDF_head)
 
!=====================================================
! Scan ID
dataset = SC_ScanID
  stringarray(1) = ''
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

!=====================================================
! Step X
dataset = SC_StepX
  hdferr = HDF_writeDatasetFloat(dataset, ebsdnl%StepX, HDF_head)
 
!=====================================================
! Step Y
dataset = SC_StepY
  hdferr = HDF_writeDatasetFloat(dataset, ebsdnl%StepY, HDF_head)

!=====================================================
! Working Distance
dataset = SC_WorkingDistance
  hdferr = HDF_writeDatasetFloat(dataset, ebsdnl%WD, HDF_head)

!=====================================================
! nColumns
dataset = SC_nColumns
  hdferr = HDF_writeDatasetInteger(dataset, ebsdnl%ipf_wd, HDF_head)

!=====================================================
! nRows
dataset = SC_nRows
  hdferr = HDF_writeDatasetInteger(dataset, ebsdnl%ipf_ht, HDF_head)

!=====================================================
!=====================================================

! once all these have been written, we simply pop all the way to the top and close the file
  call HDF_pop(HDF_head,.TRUE.)

end subroutine h5ebsd_writeFile


!--------------------------------------------------------------------------
!
! SUBROUTINE: h5tkd_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write an .h5ebsd file
!
!> @param vendor vendor string 'TSL', 'HKL', 'BRU'; only 'TSL' implemented for now
!> @param ebsdnl input name list
!
!> @date 05/07/17 MDG 1.0 original, based on original EBSD routine
!--------------------------------------------------------------------------
subroutine h5tkd_writeFile(vendor, tkdnl, dstr, tstrb, ipar, resultmain, exptIQ, indexmain, eulerarray, dpmap, &
                            progname, nmldeffile, OSMmap)
!DEC$ ATTRIBUTES DLLEXPORT :: h5tkd_writeFile

use NameListTypedefs
use io
use constants
use ebsdiomod
use ebsdmod
! use ebsddimod
use dictmod
use commonmod

IMPLICIT NONE

character(3),INTENT(IN)                             :: vendor   ! 'TSL' 'HKL' 'BRU'
type(TKDIndexingNameListType),INTENT(INOUT)         :: tkdnl
!f2py intent(in,out) ::  tkdnl
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
integer(kind=irg),INTENT(INOUT)                     :: ipar(10)
!f2py intent(in,out) ::  ipar
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: exptIQ(ipar(3))
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(INOUT)                        :: eulerarray(3,ipar(4))
!f2py intent(in,out) ::  eulerarray
real(kind=sgl),INTENT(IN)                           :: dpmap(ipar(3))
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile
real(kind=sgl),INTENT(OUT)                          :: OSMmap(ipar(7),ipar(8))

character(15)                                       :: tstre
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
integer(kind=irg)                                   :: hdferr, filetype, i, ii, jj,indx, istat, ipar2(6), L
character(fnlen)                                    :: groupname, dataset, h5ebsdfile, savefile
logical                                             :: noindex
type(dicttype)                                      :: dict

real(kind=sgl),allocatable                          :: osm(:,:), kam(:,:)

real(kind=sgl),allocatable                          :: exptCI(:), eangle(:), results(:), avEuler(:,:), &
                                                       lresultmain(:,:), eulers(:,:) 
integer(kind=1),allocatable                         :: iPhase(:), valid(:)
integer(kind=irg),allocatable                       :: SEMsignal(:), lindexmain(:,:)

type(HDFobjectStackType)                            :: HDF_head



!=====================================================
! write the output in the format of an h5ebsd file
!!!! THIS PART IS STILL UNDER DEVELOPMENT !!!!
! we use the TSL hrebsd file as a template for now; this 
! can be extended later other vendor formats
!=====================================================

if (vendor.ne.'TSL') then
  call Message('Only TSL h5tkd file format is implemented in this version.')
  call Message('Program results will be saved in this format.')
end if

allocate(stringarray(1))

  nullify(HDF_head%next)
  call timestamp(timestring=tstre)

! Create a new file using the default properties.
  h5ebsdfile = trim(EMsoft_getEMdatapathname())//trim(tkdnl%datafile)
  h5ebsdfile = EMsoft_toNativePath(h5ebsdfile)
  hdferr =  HDF_createFile(h5ebsdfile, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening file')
  filetype = 1

  call h5tkd_writeInfo(filetype, dstr, tstrb, tstre, progname, tkdnl, nmldeffile, HDF_head)

! here we start with the h5ebsd-specific stuff
  groupname = 'Scan 1'
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group Scan 1')

groupname = SC_TKD
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group EBSD')

!=====================================================
!=====================================================
groupname = SC_Data
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group Data')

! there are 15 datasets in this Data group: CI, Fit, IQ, PRIAS Bottom Strip,
! PRIAS Center Square, PRIAS Top Strip, Pattern, Phase, Phi, Phi1, Phi2,
! SEM Signal, Valid, X Position, Y Position

!=====================================================
! CI Confidence Index: real(kind=sgl), one for each pattern... we take this
! to be the largest dot product
dataset = SC_CI
  allocate(exptCI(ipar(3)))
  exptCI = resultmain(1,1:ipar(3))
  hdferr = HDF_writeDatasetFloatArray1D(dataset, exptCI, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset CI')

! we also insert a visual map of the Confidence Index, resampled on a rectangular array
dataset = SC_CIMap
  call h5tkd_write2DImageFromVector(dataset, exptCI, ipar(3), tkdnl, HDF_head)
  deallocate(exptCI)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset CImap')

!=====================================================
! Fit (to be completed with Farangis' code)
dataset = SC_Fit
  allocate(eangle(ipar(3)),stat=istat)
  eangle = 1.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Fit')
  deallocate(eangle)

!=====================================================
! Averaged Orientation Map (using near-match list and quaternion logarithm averaging)
! define the ipar2 entries
  allocate(avEuler(3,ipar(3)))
  ipar2(1) = ipar(6)
  ipar2(2) = ipar(5)
  ipar2(3) = ipar(3)
  ipar2(4) = ipar(1)
  ipar2(5) = ipar(2)
  ipar2(6) = tkdnl%nnav
! get the avEuler array
  eulerarray = eulerarray * sngl(cPi)/180.0
  call EBSDgetAverageOrientations(ipar2, eulerarray, indexmain(1:ipar2(4),1:ipar2(5)), resultmain(1:ipar2(4),1:ipar2(5)), &
                                  avEuler)
  eulerarray = eulerarray * 180.0/sngl(cPi)

! and write it to the HDF file
dataset = SC_AverageOrientations
  hdferr = HDF_writeDatasetFloatArray2D(dataset, avEuler, 3, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset AverageOrientations')

! get the nearest neighbor Kernel Average Misorientation Map (KAM)
  allocate(eulers(3,ipar(3)))
  do i=1,ipar(3)
    eulers(1:3,i) = eulerarray(1:3,indexmain(1,i))
  end do
  eulers = eulers*sngl(cPi)/180.0
  dict%Num_of_init = 3
  dict%Num_of_iterations = 30
  dict%pgnum = ipar2(1)
  call DI_Init(dict,'nil') 
dataset = SC_KAM
  if (sum(tkdnl%ROI).ne.0) then
    allocate(kam(tkdnl%ROI(3),tkdnl%ROI(4)))
    call EBSDgetKAMMap(ipar(3), eulers, tkdnl%ROI(3), tkdnl%ROI(4), dict, kam)
    kam = kam*180.0/sngl(cPi)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, kam, tkdnl%ROI(3), tkdnl%ROI(4), HDF_head)
  else
    allocate(kam(tkdnl%ipf_wd,tkdnl%ipf_ht))
    call EBSDgetKAMMap(ipar(3), eulers, tkdnl%ipf_wd, tkdnl%ipf_ht, dict, kam)
    kam = kam*180.0/sngl(cPi)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, kam, tkdnl%ipf_wd, tkdnl%ipf_ht, HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset KAM')
  deallocate(kam, eulers)

! get the Orientation Similarity Map (OSM)
dataset = SC_OSM
  if (sum(tkdnl%ROI).ne.0) then
    call EBSDgetOrientationSimilarityMap( (/ipar(1), ipar(2)/), indexmain, tkdnl%nosm, tkdnl%ROI(3), tkdnl%ROI(4), OSMmap)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, OSMmap, tkdnl%ROI(3), tkdnl%ROI(4), HDF_head)
  else
    call EBSDgetOrientationSimilarityMap( (/ipar(1), ipar(2)/), indexmain, tkdnl%nosm, tkdnl%ipf_wd, tkdnl%ipf_ht, OSMmap)
    hdferr = HDF_writeDatasetFloatArray2D(dataset, OSMmap, tkdnl%ipf_wd, tkdnl%ipf_ht, HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset OSM')

! also create a second ctf file, if requested
  if (trim(tkdnl%avctffile).ne.'undefined') then
    savefile = tkdnl%ctffile
    tkdnl%ctffile = tkdnl%avctffile
    ipar2(1:6) = ipar(1:6)
    ipar(1) = 1
    ipar(2) = ipar2(3)
    ipar(3) = ipar2(3)
    ipar(4) = ipar2(3)
  
    allocate(lindexmain(1,ipar2(3)), lresultmain(1,ipar2(3)))
    lindexmain = 0
    lresultmain(1,1:ipar2(3)) = resultmain(1,1:ipar2(3))
    noindex = .TRUE.

    call ctftkd_writeFile(tkdnl,ipar,lindexmain,avEuler,lresultmain,OSMmap,exptIQ,noindex)
    call Message('Average orientation data stored in ctf file : '//trim(tkdnl%avctffile))
    tkdnl%ctffile = savefile
    ipar(1:6) = ipar2(1:6)
    deallocate(lindexmain, lresultmain)
  end if

! we also insert a visual map of the Confidence Index, resampled on a rectangular array
! dataset = 'FitMap'
! call h5ebsd_write2DImageFromVector(dataset, totnumexpt, exptCI, tkdnl, HDF_head)

!=====================================================
! IQ Image Quality; computed using the second moment of the pattern power spectrum
dataset = SC_IQ
  hdferr = HDF_writeDatasetFloatArray1D(dataset, exptIQ, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset IQ')
  
! we also insert a visual map of the Image Quality, resampled on a rectangular array
dataset = SC_IQMap
  call h5tkd_write2DImageFromVector(dataset, exptIQ, ipar(3), tkdnl, HDF_head)

!=====================================================
! PRIAS Bottom Strip: to be implemented
!   call Message('h5ebsd_writeFile: writing of ->PRIAS Bottom Strip<- data not yet implemented.')

!=====================================================
! PRIAS Center Square: to be implemented
!   call Message('h5ebsd_writeFile: writing of ->PRIAS Center Strip<- data not yet implemented.')

!=====================================================
! PRIAS Top Strip: to be implemented
!   call Message('h5ebsd_writeFile: writing of ->PRIAS Top Strip<- data not yet implemented.')

!=====================================================
! Pattern: in principle, this is where the fitted patterns could be stored
! This will require re-computing them for the best match orientations; we 
! could leave this as an option for the user, to be implemented.
!   call Message('h5ebsd_writeFile: writing of ->Pattern<- data not yet implemented.')

!=====================================================
! Phase: Phase identifier (all zero for now)
dataset = SC_Phase
  allocate(iPhase(ipar(3)),stat=istat)
  iPhase = 0
  hdferr = HDF_writeDatasetInteger1byteArray1D(dataset, iPhase, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phase')
  deallocate(iPhase)

!=====================================================
! SEM Signal: all 0 for now
  dataset = 'SEM Signal'
  allocate(SEMsignal(ipar(3)),stat=istat)
  SEMsignal = 10000
  hdferr = HDF_writeDatasetIntegerArray1D(dataset, SEMsignal, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset SEM Signal')
  deallocate(SEMsignal)

!=====================================================
! Valid : all 0 for now
dataset = SC_Valid
  allocate(valid(ipar(3)),stat=istat)
  valid = 0
  hdferr = HDF_writeDatasetInteger1byteArray1D(dataset, valid, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Valid')
  deallocate(valid)

!=====================================================
! Euler angles: Phi 
dataset = SC_Phi
  allocate(eangle(ipar(3)),stat=istat)
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(2,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi')

!=====================================================
! Euler angles: Phi1
dataset = SC_Phi1
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(1,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi1')

!=====================================================
! Euler angles: Phi2 
dataset = SC_Phi2
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(3,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi2')
  deallocate(eangle)
 
!=====================================================
! X Position: list of x positions for sampling points; requires knowledge of step size
! from Header
  dataset = 'X Position'
  allocate(results(ipar(3)),stat=istat)
  if (sum(tkdnl%ROI).ne.0) then
    do jj=1,tkdnl%ROI(3)
      do ii=1,tkdnl%ROI(4)
        results(tkdnl%ROI(4)*(jj-1)+ii) = (ii-1)*tkdnl%StepX 
      end do
    end do
  else
    do jj=1,tkdnl%ipf_ht
      do ii=1,tkdnl%ipf_wd
        results(tkdnl%ipf_wd*(jj-1)+ii) = (ii-1)*tkdnl%StepX 
      end do
    end do
  end if
  hdferr = HDF_writeDatasetFloatArray1D(dataset, results, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset X Position')

 
!=====================================================
! Y Position: list of y positions for sampling points; requires knowledge of step size
! from Header
  dataset = 'Y Position'
  if (sum(tkdnl%ROI).ne.0) then
    do jj=1,tkdnl%ROI(3)
      do ii=1,tkdnl%ROI(4)
        results(tkdnl%ROI(4)*(jj-1)+ii) = (jj-1)*tkdnl%StepY 
      end do
    end do
  else
    do jj=1,tkdnl%ipf_ht
      do ii=1,tkdnl%ipf_wd
        results(tkdnl%ipf_wd*(jj-1)+ii) = (jj-1)*tkdnl%StepY 
      end do
    end do
  end if
  hdferr = HDF_writeDatasetFloatArray1D(dataset, results, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Y position')
  deallocate(results)
 
!=====================================================
!=====================================================
! this concludes the standard data sets in the Data group
! here, we have additional data sets based on results from the 
! dictionary indexing program; these are not part of the standard
! TSL h5ebsd file format.
!=====================================================
! EBSD average dot product map 
dataset = SC_AvDotProductMap
  call h5tkd_write2DImageFromVector(dataset, dpmap, ipar(3), tkdnl, HDF_head)

! number of samples in dictionary
dataset = SC_FZcnt
  hdferr = HDF_writeDatasetInteger(dataset, ipar(5), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset FZcnt')

! point group number
dataset = SC_PointGroupNumber
  hdferr = HDF_writeDatasetInteger(dataset, ipar(6), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset PointGroupNumber')

! Ncubochoric
dataset = SC_Ncubochoric
  hdferr = HDF_writeDatasetInteger(dataset, tkdnl%ncubochoric, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Ncubochoric')

! write the list of sampled Euler angles
dataset = SC_EulerAngles
  hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerarray, 3, ipar(4), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset EulerAngles')

! number of experimental patterns 
dataset = SC_NumExptPatterns
  hdferr = HDF_writeDatasetInteger(dataset, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset NumExptPatterns')

! list of top nnk dot product values
dataset = SC_TopDotProductList
  hdferr = HDF_writeDatasetFloatArray2D(dataset, resultmain, ipar(1), ipar(2), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopDotProductList')

! indices of top matches into Euler angle list
dataset = SC_TopMatchIndices
  hdferr = HDF_writeDatasetIntegerArray2D(dataset, indexmain, ipar(1), ipar(2), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopMatchIndices')

! leave this group
  call HDF_pop(HDF_head)
!=====================================================
!=====================================================

!=====================================================
!=====================================================
! create the Header group
groupname = SC_Header
  hdferr = HDF_createGroup(groupname, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error creating group Header')

! there are 15 datasets in this group: Camera Azimuth Angle, Camera Elevation Angle,
! Grid Type, Notes, Operator, Pattern Height, Pattern Width, Sample ID, Sample Tilt,
! Scan ID, Step X, Step Y, Working Distance, nColumns, nRows
! there are also 3 groups: Coordinate System, Pattern Center Calibration, and Phase

!=====================================================
! Camera Azimuthal Angle
  dataset = 'Camera Azimuthal Angle'
  hdferr = HDF_writeDatasetFloat(dataset, 0.0, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Camera Azimuthal Angle')
 
!=====================================================
! Camera Elevation Angle
  dataset = 'Camera Elevation Angle'
  hdferr = HDF_writeDatasetFloat(dataset, tkdnl%thetac, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopMatchIndices')
 

!=====================================================
! Coordinate System group
  call h5ebsd_writeCoordinateSystemGroup(HDF_head)

!=====================================================
! Grid Type
  dataset = 'Grid Type'
  stringarray(1) = 'SqrGrid'
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Grid Type')

!=====================================================
! Notes
dataset = SC_Notes
  stringarray(1) = ''
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Notes')

!=====================================================
! Operator
dataset = SC_Operator
  stringarray(1) = trim(EMsoft_getUsername())//' ['//trim(EMsoft_getUseremail())//']'
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Operator')

!=====================================================
! Pattern Center Calibration group
  call h5ebsd_writePatternCenterGroup(tkdnl%xpc, tkdnl%ypc, tkdnl%L, tkdnl%delta, (/tkdnl%numsx, tkdnl%numsy/), HDF_head)

!=====================================================
! Pattern height
  dataset = 'Pattern Height'
  hdferr = HDF_writeDatasetInteger(dataset, tkdnl%numsx/tkdnl%binning, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Pattern Height')

!=====================================================
! Pattern width
  dataset = 'Pattern Width'
  hdferr = HDF_writeDatasetInteger(dataset, tkdnl%numsy/tkdnl%binning, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Pattern Width')

!=====================================================
! Phase group
groupname = SC_Phase
  hdferr = HDF_createGroup(groupname, HDF_head)
groupname = "1"
  call h5ebsd_writePhaseGroup(groupname, tkdnl%MCxtalname, HDF_head)

! close the Phase group
  call HDF_pop(HDF_head)


!=====================================================
! Sample ID
  dataset = 'Sample ID'
  stringarray(1) = ''
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

!=====================================================
! Sample Tilt
  dataset = 'Sample Tilt'
  hdferr = HDF_writeDatasetFloat(dataset, sngl(tkdnl%MCsig), HDF_head)
 
!=====================================================
! Scan ID
  dataset = 'Scan ID'
  stringarray(1) = ''
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

!=====================================================
! Step X
  dataset = 'Step X'
  hdferr = HDF_writeDatasetFloat(dataset, tkdnl%StepX, HDF_head)
 
!=====================================================
! Step Y
  dataset = 'Step Y'
  hdferr = HDF_writeDatasetFloat(dataset, tkdnl%StepY, HDF_head)

!=====================================================
! Working Distance
  dataset = 'Working Distance'
  hdferr = HDF_writeDatasetFloat(dataset, tkdnl%WD, HDF_head)

!=====================================================
! nColumns
dataset = SC_nColumns
  hdferr = HDF_writeDatasetInteger(dataset, tkdnl%ipf_wd, HDF_head)

!=====================================================
! nRows
dataset = SC_nRows
  hdferr = HDF_writeDatasetInteger(dataset, tkdnl%ipf_ht, HDF_head)

!=====================================================
!=====================================================

! once all these have been written, we simply pop all the way to the top and close the file
  call HDF_pop(HDF_head,.TRUE.)

end subroutine h5tkd_writeFile



end module EMh5ebsd
