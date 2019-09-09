! ###################################################################
! Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft: InitializeQCHDF.f90
!--------------------------------------------------------------------------
!
! MODULE: InitializeQCHDF
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Everything that has to do with qxtal I/O
! 
!> @date  05/28/18   SS 1.0 original
!--------------------------------------------------------------------------
module InitializersQCHDF

use local
use typedefs
use HDF5
use HDFsupport
use stringconstants

interface SaveQCDataHDF
  module procedure Save2DQCDataHDF
  module procedure Save3DQCDataHDF
end interface SaveQCDataHDF

interface ReadQCDataHDF
  module procedure Read2DQCDataHDF
  module procedure Read3DQCDataHDF
end interface ReadQCDataHDF

interface Initialize_QCCell
 module procedure Initialize_2DQCCell
 module procedure Initialize_3DQCCell
end interface Initialize_QCCell

interface DumpQXtalInfo
  module procedure Dump2DQXtalInfo
  module procedure Dump3DQXtalInfo
end interface DumpQXtalInfo

interface QCrystalData
  module procedure QCrystal2DData
  module procedure QCrystal3DData
end interface QCrystalData

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: Save2DQCDataHDF
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief save 2D QCcrystal structure data to an HDF file
! 
!> @param cell 2D quasicrystal unit cell pointer
!> @param existingHDFhead (optional) if present, then use this as HDF_head
!
!> @date    05/23/18 SS 1.0 original, adapted from SaveDataHDF
!--------------------------------------------------------------------------
recursive subroutine Save2DQCDataHDF(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: Save2DQCDataHDF

use io
use crystal
use error
 
IMPLICIT NONE

type(TDQCStructureType),pointer         :: cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(11)                           :: dstr
character(15)                           :: tstr
character(fnlen)                        :: progname = 'EMmkqxtal.f90', groupname, dataset, fname
integer(kind=irg)                       :: hdferr
real(kind=dbl)                          :: cellparams(5)
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

dataset = SC_LatticeParameters
cellparams = (/ cell%QClatparm_a, cell%QClatparm_c, cell%alphaij, cell%alphai5, cell%alphastarij /)
hdferr = HDF_writeDatasetDoubleArray1D(dataset, cellparams, 5, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetDoubleArray1D:'//trim(dataset), hdferr)

dataset = SC_AxialSymmetry !'Axial Symmetry'
hdferr = HDF_writeDatasetInteger(dataset, cell%SG%N_Axial, HDF_head)

dataset = SC_SpaceGroupNumber
hdferr = HDF_writeDatasetInteger(dataset, cell%SYM_SGnum, HDF_head)
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

allocate(atompos(cell%ATOM_ntype,10))
atompos(1:cell%ATOM_ntype,1:10) = cell%ATOM_pos(1:cell%ATOM_ntype,1:10)
dataset = SC_AtomData
hdferr = HDF_writeDatasetFloatArray2D(dataset, atompos, cell%ATOM_ntype, 10, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetFloatArray2D:'//trim(dataset), hdferr)
deallocate(atompos)

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)
  call h5close_EMsoft(hdferr)
  call HDFerror_check('SaveDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

end subroutine Save2DQCDataHDF

!--------------------------------------------------------------------------
!
! SUBROUTINE: Save3DQCDataHDF
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief save 3D QCcrystal structure data to an HDF file
! 
!> @param cell 3D quasicrystal unit cell pointer
!> @param existingHDFhead (optional) if present, then use this as HDF_head
!
!> @date    06/25/18 SS 1.0 original, adapted from SaveDataHDF
!--------------------------------------------------------------------------
recursive subroutine Save3DQCDataHDF(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: Save3DQCDataHDF

use io
use crystal
use error
 
IMPLICIT NONE

type(QCStructureType),pointer            :: cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(11)                           :: dstr
character(15)                           :: tstr
character(fnlen)                        :: progname = 'EMmkqxtal.f90', groupname, dataset, fname
integer(kind=irg)                       :: hdferr
real(kind=dbl)                          :: cellparams(3)
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

dataset = SC_LatticeParameters
cellparams = (/ cell%QClatparm, cell%alphaij, cell%alphastarij /)
hdferr = HDF_writeDatasetDoubleArray1D(dataset, cellparams, 3, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetDoubleArray1D:'//trim(dataset), hdferr)

dataset = SC_SpaceGroupNumber
hdferr = HDF_writeDatasetInteger(dataset, cell%SYM_SGnum, HDF_head)
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

allocate(atompos(cell%ATOM_ntype,10))
atompos(1:cell%ATOM_ntype,1:10) = cell%ATOM_pos(1:cell%ATOM_ntype,1:10)
dataset = SC_AtomData
hdferr = HDF_writeDatasetFloatArray2D(dataset, atompos, cell%ATOM_ntype, 10, HDF_head)
call HDFerror_check('SaveDataHDF:HDF_writeDatasetFloatArray2D:'//trim(dataset), hdferr)
deallocate(atompos)

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)
  call h5close_EMsoft(hdferr)
  call HDFerror_check('SaveDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

end subroutine Save3DQCDataHDF

!--------------------------------------------------------------------------
!
! SUBROUTINE: Read2DQCDataHDF
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read 2D quasicrystal crystal structure data from an HDF file
! 
!> @param 2D quasicrystal cell unit cell pointer
!> @param existingHDFhead (optional) if present, then use this as HDF_head
!
!> @date    05/23/18 SS 1.0 original, adapted from ReadDataHDF above
!--------------------------------------------------------------------------
recursive subroutine Read2DQCDataHDF(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: Read2DQCDataHDF

use io
use crystal
use error
 
IMPLICIT NONE

type(TDQCStructureType),pointer         :: cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(fnlen)                        :: dataset, groupname, fname
integer(HSIZE_T)                        :: dims(1), dims2(2)
integer(kind=irg)                       :: hdferr
real(kind=dbl),allocatable              :: cellparams(:)
integer(kind=irg),allocatable           :: atomtypes(:)
real(kind=sgl),allocatable              :: atompos(:,:)
character(fnlen)                        :: pp
logical                                 :: openHDFfile

openHDFfile = .TRUE.
if (present(existingHDFhead)) then
  if (associated(existingHDFhead%next)) then
    openHDFfile = .FALSE.
    HDF_head = existingHDFhead
  else
    call FatalError("Read2DQCDataHDF","HDF_head pointer passed in to routine is not associated")
  end if 
end if

if (openHDFfile) then 
  nullify(HDF_head%next)
  call h5open_EMsoft(hdferr)
  call HDFerror_check('Read2DQCDataHDF:h5open_EMsoft', hdferr)

  fname = trim(EMsoft_getXtalpathname())//trim(cell%fname)
  fname = EMsoft_toNativePath(fname)
  hdferr =  HDF_openFile(fname, HDF_head)
  call HDFerror_check('Read2DQCDataHDF:HDF_openFile:'//trim(fname), hdferr)
end if

groupname = SC_CrystalData
hdferr = HDF_openGroup(groupname, HDF_head)
call HDFerror_check('Read2DQCDataHDF:HDF_openGroup:'//trim(groupname), hdferr)

dataset = SC_LatticeParameters
call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head, hdferr, cellparams)
call HDFerror_check('Read2DQCDataHDF:HDF_readDatasetDoubleArray1D:'//trim(dataset), hdferr)

cell%QClatparm_a = cellparams(1)
cell%QClatparm_c = cellparams(2)
cell%alphaij     = cellparams(3)
cell%alphai5     = cellparams(4)
cell%alphastarij = cellparams(5)

dataset = SC_SpaceGroupNumber
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SYM_SGnum) 
call HDFerror_check('Read2DQCDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

dataset = SC_AxialSymmetry
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SG%N_Axial) 
call HDFerror_check('Read2DQCDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

if(cell%SG%N_Axial .eq. 8) then
  cell%QCType = 'Oct'
else if(cell%SG%N_Axial .eq. 10) then
  cell%QCType = 'Dec'
else if(cell%SG%N_Axial .eq. 12) then
  cell%QCType = 'DoD'
else
  call FatalError('Read2DQCDataHDF',&
  'The axial symmetry is not one of the implemented ones (only 8, 10 and 12 fold implemented.)')
end if

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

cell%ATOM_pos(1:cell%ATOM_ntype,1:10) = atompos(1:cell%ATOM_ntype,1:10) 
deallocate(atompos)

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)

  call h5close_EMsoft(hdferr)
  call HDFerror_check('ReadDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

end subroutine Read2DQCDataHDF

!--------------------------------------------------------------------------
!
! SUBROUTINE: Read3DQCDataHDF
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read 3D quasicrystal crystal structure data from an HDF file
! 
!> @param 3D quasicrystal cell unit cell pointer
!> @param existingHDFhead (optional) if present, then use this as HDF_head
!
!> @date    05/25/18 SS 1.0 original, adapted from ReadDataHDF above
!--------------------------------------------------------------------------
recursive subroutine Read3DQCDataHDF(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: Read3DQCDataHDF

use io
use crystal
use error
 
IMPLICIT NONE

type(QCStructureType),pointer            :: cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(fnlen)                        :: dataset, groupname, fname
integer(HSIZE_T)                        :: dims(1), dims2(2)
integer(kind=irg)                       :: hdferr
real(kind=dbl),allocatable              :: cellparams(:)
integer(kind=irg),allocatable           :: atomtypes(:)
real(kind=sgl),allocatable              :: atompos(:,:)
character(fnlen)                        :: pp
logical                                 :: openHDFfile

openHDFfile = .TRUE.
if (present(existingHDFhead)) then
  if (associated(existingHDFhead%next)) then
    openHDFfile = .FALSE.
    HDF_head = existingHDFhead
  else
    call FatalError("Read2DQCDataHDF","HDF_head pointer passed in to routine is not associated")
  end if 
end if

if (openHDFfile) then 
  nullify(HDF_head%next)
  call h5open_EMsoft(hdferr)
  call HDFerror_check('Read2DQCDataHDF:h5open_EMsoft', hdferr)

  fname = trim(EMsoft_getXtalpathname())//trim(cell%fname)
  fname = EMsoft_toNativePath(fname)
  hdferr =  HDF_openFile(fname, HDF_head)
  call HDFerror_check('Read2DQCDataHDF:HDF_openFile:'//trim(fname), hdferr)
end if

groupname = SC_CrystalData
hdferr = HDF_openGroup(groupname, HDF_head)
call HDFerror_check('Read2DQCDataHDF:HDF_openGroup:'//trim(groupname), hdferr)

dataset = SC_LatticeParameters
call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head, hdferr, cellparams)
call HDFerror_check('Read2DQCDataHDF:HDF_readDatasetDoubleArray1D:'//trim(dataset), hdferr)

cell%QClatparm   = cellparams(1)
cell%alphaij     = cellparams(2)
cell%alphastarij = cellparams(3)

dataset = SC_SpaceGroupNumber
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SYM_SGnum) 
call HDFerror_check('Read2DQCDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

cell%QCType = 'Ico'

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

cell%ATOM_pos(1:cell%ATOM_ntype,1:10) = atompos(1:cell%ATOM_ntype,1:10) 
deallocate(atompos)

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)

  call h5close_EMsoft(hdferr)
  call HDFerror_check('ReadDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

end subroutine Read3DQCDataHDF

!--------------------------------------------------------------------------
!
! SUBROUTINE: Dump2DQXtalInfo
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Write a brief summary of the 2D quasicrystal structure on the screen
! 
!> @date    05/23/18 SS 1.0 original, adapted from subroutine above
!--------------------------------------------------------------------------
recursive subroutine Dump2DQXtalInfo(cell)    
!DEC$ ATTRIBUTES DLLEXPORT :: Dump2DQXtalInfo

use constants
use io
use qcrystal 

IMPLICIT NONE

type(TDQCStructureType),pointer         :: cell

integer(kind=irg)                       :: i, j, oi_int(3)
real(kind=dbl)                          :: oi_real(10)


 call Message('', frm = "(A/)")
 call Message('Quasicrystal Structure Information', frm = "('-->',A,'<--')")
 oi_real(1) = cell%QClatparm_a
 call WriteValue('  a_i | i = {1,2,3,4} [nm]             : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%QClatparm_c
 call WriteValue('  a_5 [nm]                             : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%alphaij
 call WriteValue('  alpha_ij | (i,j) = {1,2,3,4} [deg]   : ', oi_real, 1, "(F10.5)")
  oi_real(1) = cell%alphai5
 call WriteValue('  alpha_i5 | (i)   = {1,2,3,4} [deg]   : ', oi_real, 1, "(F10.5)")
 oi_int(1)  = cell%SG%N_Axial
 call WriteValue('  Highest axial rotational symmetry    : ', oi_int, 1, "(I4)")
 oi_real(1) = cell%vol
 call WriteValue('  Volume [nm^5]                        : ', oi_real, 1, "(F12.8)")
 oi_int(1) = cell%SYM_SGnum
 call WriteValue('  Space group #                        : ', oi_int, 1, "(1x,I3)")
 call WriteValue('  Space group symbol                   : ', '  '//trim(cell%SGname(cell%SYM_SGnum)) )
 
! generate atom positions and dump output  
 call Message('', frm = "(A/)")
 call CalcQCPositions(cell)
 oi_int(1) = cell%ATOM_ntype
 call WriteValue('  Number of asymmetric atom positions ', oi_int, 1)
 do i=1,cell%ATOM_ntype
  oi_int(1:3) = (/i, cell%ATOM_type(i), cell%numat(i)/)
  call WriteValue('  General position / atomic number / multiplicity :', oi_int, 3,"(1x,I3,'/',I2,'/',I3,$)")
  call Message(' ('//ATOM_sym(cell%ATOM_type(i))//')', frm = "(A)")
  call Message('   Equivalent positions  (a_1 a_2 a_3 a_4 a_5  occ  Bpar_11 Bpar_33 Bperp lambda_k) ', frm = "(A)")
  do j=1,cell%numat(i)
    oi_real(1:10) = (/dble(cell%apos(i, j,1:5)),dble(cell%ATOM_pos(i,6:10))/)
    call WriteValue('         > ', oi_real, 10,"(2x,6(F9.5,','),4F9.5)")
  end do
end do
call Message('', frm = "(A/)")

end subroutine Dump2DQXtalInfo

!--------------------------------------------------------------------------
!
! SUBROUTINE: Dump3DQXtalInfo
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Write a brief summary of the 2D quasicrystal structure on the screen
! 
!> @date    05/25/18 SS 1.0 original, adapted from subroutine above
!--------------------------------------------------------------------------
recursive subroutine Dump3DQXtalInfo(cell)    
!DEC$ ATTRIBUTES DLLEXPORT :: Dump3DQXtalInfo

use constants
use io
use qcrystal 

IMPLICIT NONE

type(QCStructureType),pointer            :: cell

integer(kind=irg)                       :: i, j, oi_int(3)
real(kind=dbl)                          :: oi_real(10)


 call Message('', frm = "(A/)")
 call Message('Quasicrystal Structure Information', frm = "('-->',A,'<--')")
 oi_real(1) = cell%QClatparm
 call WriteValue('  a_i [nm]                : ', oi_real, 1, "(F9.5)")
 oi_real(1) = cell%alphaij
 call WriteValue('  alpha_ij [deg]                : ', oi_real, 1, "(F10.5)")
 oi_real(1) = cell%vol
 call WriteValue('  Volume [nm^5]                        : ', oi_real, 1, "(F12.8)")
 oi_int(1) = cell%SYM_SGnum
 call WriteValue('  Space group #                        : ', oi_int, 1, "(1x,I3)")
 call WriteValue('  Space group symbol                   : ', '  '//trim(cell%SGname(cell%SYM_SGnum)) )
 call WriteValue('  Space group generator string         : ', '  '//trim(cell%SG%SYM_GL(cell%SYM_SGnum)) )
! generate atom positions and dump output  
 call Message('', frm = "(A/)")
 call CalcQCPositions(cell)
 oi_int(1) = cell%ATOM_ntype
 call WriteValue('  Number of asymmetric atom positions ', oi_int, 1)
 do i=1,cell%ATOM_ntype
  oi_int(1:3) = (/i, cell%ATOM_type(i), cell%numat(i)/)
  call WriteValue('  General position / atomic number / multiplicity :', oi_int, 3,"(1x,I3,'/',I2,'/',I3,$)")
  call Message(' ('//ATOM_sym(cell%ATOM_type(i))//')', frm = "(A)")
  call Message('   Equivalent positions  (a_1 a_2 a_3 a_4 a_5 a_6  occ  Bpar Bperp lambda_k) ', frm = "(A)")
  do j=1,cell%numat(i)
    oi_real(1:10) = (/dble(cell%apos(i, j,1:6)),dble(cell%ATOM_pos(i,7:10))/)
    call WriteValue('         > ', oi_real, 10,"(2x,6(F9.5,','),4F9.5)")
  end do
end do
call Message('', frm = "(A/)")

end subroutine Dump3DQXtalInfo

!--------------------------------------------------------------------------
!
! SUBROUTINE: QCrystal2DData
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief load or generate 2D quasi crystal data
! 
!> @param cell 2D quasicrystal unit cell pointer
!> @param verbose (OPTIONAL)
!> @param existingHDFhead (OPTIONAL) pass-on variable with current HDF_head pointer, if any
!
!> @date    05/23/18 SS 1.0 original, adapted from CrystalData subroutine
!> @date    06/25/18 SS 1.1 name changed; handled by module interface
!--------------------------------------------------------------------------
recursive subroutine QCrystal2DData(cell, verbose, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: QCrystal2DData

use io
use crystal
use files
use symmetry
use typedefs
use QCmod
use qcrystal

IMPLICIT NONE

type(TDQCStructureType),pointer         :: cell
logical,INTENT(IN),OPTIONAL             :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

integer(kind=irg)                       :: i, ipg, isave

call ReadQCDataHDF(cell, existingHDFhead)

call PrintSGTable(cell,toprint=.FALSE.)

! compute the metric matrices
 call QC_setMetricParameters(cell)

 call GenerateQCSymmetry(cell,.TRUE.)

 call Get2DQCPGsymmetry(cell)

! and print the information on the screen
if (present(verbose)) then
 if (verbose) then
   call DumpQXtalInfo(cell)
 end if
end if 

end subroutine QCrystal2DData

!--------------------------------------------------------------------------
!
! SUBROUTINE: QCrystal3DData
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief load or generate 3D quasi crystal data
! 
!> @param cell 3D quasicrystal unit cell pointer
!> @param verbose (OPTIONAL)
!> @param existingHDFhead (OPTIONAL) pass-on variable with current HDF_head pointer, if any
!
!> @date    06/25/18 SS 1.1 original
!--------------------------------------------------------------------------
recursive subroutine QCrystal3DData(cell, verbose, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: QCrystal3DData

use io
use files
use symmetry
use typedefs
use QCmod
use qcrystal

IMPLICIT NONE

type(QCStructureType),pointer            :: cell
logical,INTENT(IN),OPTIONAL             :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

integer(kind=irg)                       :: i, ipg, isave

call ReadQCDataHDF(cell, existingHDFhead)

call PrintSGTable(cell,toprint=.FALSE.)

! compute the metric matrices
 call QC_setMetricParameters(cell)

 call GenerateQCSymmetry(cell,.TRUE.)

! and print the information on the screen
if (present(verbose)) then
 if (verbose) then
   call DumpQXtalInfo(cell)
 end if
end if 

end subroutine QCrystal3DData

!--------------------------------------------------------------------------
!
! subroutine: Initialize_2DQCCell
!
!> @author Saransh Singh
!
!> @brief perform all steps to initialize a 2D quasicrystal unit cell type variable
!
!> @param cell unit cell pointer
!> @param xtalname file name for crystal structure
!> @param dmin smallest d-spacing to consider
!> @param voltage accelerating voltage (needed to compute relativistic scattering factors)
!
!> @date 05/23/18 SS 1.0 original, adapted from routine above
!--------------------------------------------------------------------------
recursive subroutine Initialize_2DQCCell(cell, xtalname, dmin_qc, dmin_p, voltage, nthreads, verbose, existingHDFhead, initLUT)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_2DQCCell

use files
use io
use error
use diffractionQC
use QCmod
use qcrystal
use omp_lib

IMPLICIT NONE

type(TDQCStructureType), pointer           :: cell
character(fnlen),INTENT(IN)                :: xtalname
real(kind=sgl),INTENT(IN)                  :: dmin_qc
real(kind=sgl),INTENT(IN)                  :: dmin_p
real(kind=sgl),INTENT(IN)                  :: voltage
integer(kind=irg),INTENT(IN)            :: nthreads
logical,INTENT(IN),OPTIONAL                :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead
logical,INTENT(IN),OPTIONAL                :: initLUT

integer(kind=irg)                          :: istat, io_int(5), skip, QCindex, nLUT
integer(kind=irg)                          :: imh, imk, gg(5), ia1, ia2, ia3, ia4, ia5, id, TID
real(kind=sgl)                             :: dhkl, io_real(5), ddt
logical                                    :: loadingfile, justinit
complex(kind=dbl)                          :: Ucg
complex(kind=dbl)                          :: qg
real(kind=dbl)                             :: Vmod
real(kind=dbl)                             :: Vpmod, Upz
real(kind=dbl)                             :: xig
real(kind=dbl)                             :: xgp

justinit = .FALSE.
if(present(initLUT)) then
  if(initLUT) justinit = .TRUE.
end if

! clear the cell variable (set everything to zero)
 !call ResetCell(cell)

if(.not. justinit) then
! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = xtalname

 cell%voltage = dble(voltage)

 call QCrystalData(cell,verbose, existingHDFhead)
end if
 cell%voltage = dble(voltage)

 ! always use Weickenmeier&Kohl scattering coefficients, including absorptive form factors
 call QC_CalcWaveLength(cell, verbose)

! generate all atom positions
! if the cell is distorted, then this is not exactly correct, but it should be close for small distortions
 !call CalcQCPositions(cell)
! ! compute the range of reflections for the lookup table and allocate the table
! ! The master list is easily created by brute force
imh = cell%imax_qc/2
imk = cell%imax_p/2

if(.not. justinit) then
   imh = 1
   do 
      dhkl = 1.D0/QC_getvectorLength(cell, (/imh,0,0,0,0/), 'P', 'r')
      if (dhkl.lt.dmin_qc) EXIT
      imh = imh + 1
    end do

   imk = 1
   do 
      dhkl = 1.D0/QC_getvectorLength(cell, (/0,0,0,0,imk/), 'P', 'r')
      if (dhkl.lt.dmin_p) EXIT
      imk = imk + 1
   end do
   cell%imax_qc = 2*imh
   cell%imax_p  = 2*imk
end if

if (present(verbose)) then
  if (verbose) then
    io_int(1:2) = (/imh,imk/)
    call WriteValue('number of reflections along a*_i | i = {1,2,3,4} and a*_5 = ',io_int,2)
  end if
end if

gg   = (/cell%imax_qc, cell%imax_qc, cell%imax_qc, cell%imax_qc, cell%imax_p/)
nLUT = QC_getIndex(cell, gg) 

if(.not. justinit) then
! ! the LUT array stores all the Fourier coefficients, so that we only need to compute them once... i.e., here and now
  allocate(cell%LUT(nLUT),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%LUT array')

  allocate(cell%LUTqg(nLUT),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%LUTqg array')

! ! allocate an array that keeps track of potential double diffraction reflections
  allocate(cell%dbdiff(nLUT),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%dbdiff array')

  allocate(cell%inverseIndex(nLUT,6),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%inverseIndex array')
  cell%inverseIndex = 0

 end if 

cell%LUT = dcmplx(0.D0,0.D0)
cell%LUTqg = dcmplx(0.D0,0.D0)
cell%dbdiff = .FALSE.
ddt = 1.0e-5 
! ! changed from 1.0e-10 on 08/14/15 by MDG in response to some issues with double
! ! diffraction spots not being taken into account in EBSD master pattern simulations 


! ! next, we compute the overall lookup table cell%LUT; we do not, at this point, create a 
! ! list of linked reflections; in the old code, this was done at the same time, but it appears
! ! it is better to decouple these two computations. In this new approach, we'll compute a much
! ! shorter linked list based on the incident wave vector direction.

! ! first, we deal with the transmitted beam
 gg       = (/ 0,0,0,0,0 /)
 Ucg      = QC_getUcg(cell, gg, qg, Vmod, Vpmod, xig, xgp) 
 Upz      = Vpmod         ! U'0 normal absorption parameter 
 id        = QC_getIndex(cell, gg)
! ! and add this reflection to the look-up table
 cell%LUT(id)    = Ucg
 cell%LUTqg(id)  = qg

 if (present(verbose)) then
  if (verbose) then
   call Message('Generating Fourier coefficient lookup table ... ', frm = "(/A,$)")
  end if
 end if
 
if(.not. justinit) then
! now do the same for the other allowed reflections
! note that the lookup table must be twice as large as the list of participating reflections,
! since the dynamical matrix uses g-h as its index !!!  
   ia1l: do ia1 = -cell%imax_qc,cell%imax_qc       !-2*imh,2*imh
      ia2l:  do ia2 = -cell%imax_qc,cell%imax_qc       !-2*imh,2*imh
         ia3l:   do ia3 = -cell%imax_qc,cell%imax_qc    !-2*imh,2*imh
            ia4l:     do ia4 = -cell%imax_qc,cell%imax_qc   !-2*imh, 2*imh
               ia5l:       do ia5 = -cell%imax_p,cell%imax_p   !-2*imk, 2*imk
                          gg       = (/ ia1, ia2, ia3, ia4, ia5 /)
                          id       = QC_getIndex(cell, gg)
                          cell%inverseIndex(id,1:5) = gg(1:5)
              !if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
! add the reflection to the look up table
              !Ucg = QC_getUcg(cell, gg, qg, Vmod, Vpmod, xig, xgp)
              !cell%LUT(QCindex)   = Ucg 
              !cell%LUTqg(QCindex) = qg
! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
              !if (abs(Ucg).le.ddt) then 
              !  cell%dbdiff(QCindex) = .TRUE.
              !end if
            end do ia5l
          end do ia4l
       end do ia3l
      end do ia2l
    end do ia1l

end if
! set the number of OpenMP threads 
call OMP_SET_NUM_THREADS(nthreads)
if (present(verbose)) then
     if (verbose) then
        io_int(1) = nthreads
        call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")
     end if
end if

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL DEFAULT(PRIVATE) &
!$OMP& SHARED(cell, nLUT, ddt, verbose)

TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC)

do id = 1,nLUT

   gg             = cell%inverseIndex(id,1:5)

   Ucg          = QC_getUcg(cell, gg, qg, Vmod, Vpmod, xig, xgp)
   cell%LUT(id)      = Ucg !rlp%Ucg
   cell%LUTqg(id)    = qg

! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
   if (abs(Ucg).le.ddt) then 
      cell%dbdiff(id) = .TRUE.
   end if


    if(present(verbose)) then
       if(verbose) then
          if(mod(id,250000) .eq. 0) then
             io_real(1) = 100.D0 * dble(id)/dble(nLUT)
             call WriteValue(' Finished computing ',io_real, 1, '(F8.2, " % of the total coefficients ")')
          end if
       end if
    end if

end do
! end of OpenMP portion
!$OMP END DO
!$OMP END PARALLEL

  if (present(verbose)) then
   if (verbose) then
    call Message('Done', frm = "(A/)")
   end if
  end if

! that's it
end subroutine Initialize_2DQCCell

!--------------------------------------------------------------------------
!
! subroutine: Initialize_3DQCCell
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief perform all steps to initialize a 3D quasicrystal unit cell type variable
!
!> @param cell unit cell pointer
!> @param xtalname file name for crystal structure
!> @param dmin smallest d-spacing to consider
!> @param voltage accelerating voltage (needed to compute relativistic scattering factors)
!
!> @date 06/25/18 SS 1.0 original, adapted from routine above
!--------------------------------------------------------------------------
recursive subroutine Initialize_3DQCCell(cell, xtalname, dmin, voltage, nthreads, verbose, existingHDFhead, initLUT)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_3DQCCell

use files
use io
use error
use diffractionQC
use QCmod
use qcrystal
use omp_lib

IMPLICIT NONE

type(QCStructureType), pointer             :: cell
character(fnlen),INTENT(IN)                :: xtalname
real(kind=sgl),INTENT(IN)                  :: dmin
real(kind=sgl),INTENT(IN)                  :: voltage
integer(kind=irg),INTENT(IN)               :: nthreads
logical,INTENT(IN),OPTIONAL                :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead
logical,INTENT(IN),OPTIONAL                :: initLUT

integer(kind=irg)                          :: istat, io_int(6), skip
integer(kind=irg)                          :: imh, imk, gg(6), ia1, ia2, ia3, ia4, ia5, ia6, nLUT, id, TID
real(kind=sgl)                             :: dhkl, io_real(6), ddt
logical                                    :: loadingfile, justinit, allowed
complex(kind=dbl)                          :: Ucg
complex(kind=dbl)                          :: qg
real(kind=dbl)                             :: Vmod
real(kind=dbl)                             :: Vpmod, Upz
real(kind=dbl)                             :: xig
real(kind=dbl)                             :: xgp

justinit = .FALSE.
if(present(initLUT)) then
  if(initLUT) justinit = .TRUE.
end if

! clear the cell variable (set everything to zero)
 !call ResetCell(cell)

cell%voltage = dble(voltage)

if(.not. justinit) then
! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = xtalname

 call QCrystalData(cell,verbose, existingHDFhead)

end if

 ! always use Weickenmeier&Kohl scattering coefficients, including absorptive form factors
 call QC_CalcWaveLength(cell, verbose)

! generate all atom positions
! if the cell is distorted, then this is not exactly correct, but it should be close for small distortions
 !call CalcQCPositions(cell)
! ! compute the range of reflections for the lookup table and allocate the table
! ! The master list is easily created by brute force
imh = cell%imax/2

if(.not. justinit) then
 imh = 1
 do 
   dhkl = 1.D0/QC_getvectorLength(cell, (/imh,0,0,0,0,0/), 'P', 'r')
   if (dhkl.lt.dmin) EXIT
   imh = imh + 1
 end do

 cell%imax = 2*imh
end if

if (present(verbose)) then
  if (verbose) then
    io_int(1) = imh
    call WriteValue(' Number of reflections along a* = ',io_int,1)
  end if
end if

nLUT = QC_getindex(cell, (/cell%imax, cell%imax, cell%imax, cell%imax, cell%imax, cell%imax/))

if(.not. justinit) then
! ! the LUT array stores all the Fourier coefficients, so that we only need to compute them once... i.e., here and now
  allocate(cell%LUT(nLUT),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%LUT array')

  allocate(cell%LUTqg(nLUT),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%LUTqg array')

! ! allocate an array that keeps track of potential double diffraction reflections
  allocate(cell%dbdiff(nLUT),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%dbdiff array')

  allocate(cell%inverseIndex(nLUT,6),stat=istat)
  if (istat.ne.0) call FatalError('InitializeQCCell:',' unable to allocate cell%inverseIndex array')
  cell%inverseIndex = 0

end if  

!cell%LUT = dcmplx(0.D0,0.D0)
!cell%LUTqg = dcmplx(0.D0,0.D0)
!cell%dbdiff = .FALSE.
ddt = 1.0e-5

! ! changed from 1.0e-10 on 08/14/15 by MDG in response to some issues with double
! ! diffraction spots not being taken into account in EBSD master pattern simulations 


! ! next, we compute the overall lookup table cell%LUT; we do not, at this point, create a 
! ! list of linked reflections; in the old code, this was done at the same time, but it appears
! ! it is better to decouple these two computations. In this new approach, we'll compute a much
! ! shorter linked list based on the incident wave vector direction.

! ! first, we deal with the transmitted beam
gg  = (/ 0,0,0,0,0,0 /)
Ucg = QC_getUcg(cell, gg, qg, Vmod, Vpmod, xig, xgp) 
Upz = Vpmod         ! U'0 normal absorption parameter 

! ! and add this reflection to the look-up table

id              = QC_getindex(cell, gg)
cell%LUT(id)    = Ucg
cell%LUTqg(id)  = qg

 if (present(verbose)) then
  if (verbose) then
   call Message(' Generating Fourier coefficient lookup table ... ', frm = "(/A,$)")
  end if
 end if

if(.not. justinit) then 

! now do the same for the other allowed reflections
! note that the lookup table must be twice as large as the list of participating reflections,
! since the dynamical matrix uses g-h as its index !!!  
   ia1l: do ia1 = -cell%imax,cell%imax !-2*imh,2*imh
      ia2l: do ia2 = -cell%imax,cell%imax !-2*imh,2*imh
         ia3l: do ia3 = -cell%imax,cell%imax !-2*imh,2*imh
            ia4l: do ia4 = -cell%imax,cell%imax !-2*imh, 2*imh
               ia5l: do ia5 = -cell%imax,cell%imax !-2*imh, 2*imh
                  ia6l: do ia6 = -cell%imax,cell%imax !-2*imh, 2*imh
                             gg = (/ ia1, ia2, ia3, ia4, ia5, ia6 /)
                             id = QC_getindex(cell, gg)
                             cell%inverseIndex(id,1:6) = gg(1:6)
                       end do ia6l
                  end do ia5l
                end do ia4l
             end do ia3l
         end do ia2l
    end do ia1l

end if
! set the number of OpenMP threads 
call OMP_SET_NUM_THREADS(nthreads)
if (present(verbose)) then
     if (verbose) then
        io_int(1) = nthreads
        call WriteValue('  Attempting to set number of threads to ',io_int, 1, frm = "(I4)")
     end if
end if


io_int(1) = nLUT
call WriteValue(' Number of entries in look up table : ', io_int, 1)


! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL DEFAULT(PRIVATE) &
!$OMP& SHARED(cell, nLUT, ddt, verbose)

TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC)

do id = 1,nLUT

   gg       = cell%inverseIndex(id,1:6)
   allowed  = IsGAllowedQC(cell,gg)

   if (allowed) then           ! is this reflection allowed by lattice centering ?
      Ucg               = QC_getUcg(cell, gg, qg, Vmod, Vpmod, xig, xgp)
      cell%LUT(id)      = Ucg !rlp%Ucg
      cell%LUTqg(id)    = qg

   ! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
       if (abs(Ucg).le.ddt) then 
          cell%dbdiff(id) = .TRUE.
       end if

    else
       cell%LUT(id)    = dcmplx(0.D0,0.D0)
       cell%LUTqg(id)  = dcmplx(0.D0,0.D0)
       cell%dbdiff(id) = .FALSE.
    end if

    if(present(verbose)) then
       if(verbose) then
          if(mod(id,500000) .eq. 0) then
             io_real(1) = 100.D0 * dble(id)/dble(nLUT)
             call WriteValue(' Finished computing ',io_real, 1, '(F8.2, " % of Fourier coefficients ")')
          end if
       end if
    end if

end do
! end of OpenMP portion
!$OMP END DO
!$OMP END PARALLEL

if (present(verbose)) then
   if (verbose) then
      call Message('Done', frm = "(A/)")
   end if
end if

! that's it
end subroutine Initialize_3DQCCell

end module InitializersQCHDF
