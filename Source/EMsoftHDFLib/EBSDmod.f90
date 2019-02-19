! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EBSDmod.f90
!--------------------------------------------------------------------------
!
! MODULE: EBSDmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSD helper routines
!
!> @date  06/24/14  MDG 1.0 original, lifted from EMEBSD.f90 to simplify code
!> @date  09/01/15  MDG 1.1 modified EBSDMasterType definition to accommodate multiple Lambert maps
!> @date  09/15/15  SS  1.2 added accum_z to EBSDLargeAccumType
!> @date  08/18/16  MDG 1.3 modified HDF file format 
!> @date  02/22/18  MDG 1.4 added orientation/pattern center/deformation tensor format for angle input file
!--------------------------------------------------------------------------
module EBSDmod

use local
use typedefs
use stringconstants

IMPLICIT NONE

type EBSDAngleType
        real(kind=sgl),allocatable      :: quatang(:,:)
end type EBSDAngleType

type EBSDAnglePCDefType
        real(kind=sgl),allocatable      :: quatang(:,:)
        real(kind=sgl),allocatable      :: pcs(:,:)
        real(kind=sgl),allocatable      :: deftensors(:,:,:)
end type EBSDAnglePCDefType

type EBSDLargeAccumType
        integer(kind=irg),allocatable   :: accum_e(:,:,:),accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
end type EBSDLargeAccumType

type EBSDMasterType
        real(kind=sgl),allocatable      :: mLPNH(:,:,:) , mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)          ! auxiliary detector arrays needed for interpolation
end type EBSDMasterType

type EBSDPixel
        real(kind=sgl),allocatable      :: lambdaEZ(:,:)
        real(kind=dbl)                  :: dc(3) ! direction cosine in sample frame
        real(kind=dbl)                  :: cfactor
end type EBSDPixel

type EBSDMCdataType
        integer(kind=irg)               :: multiplier
        integer(kind=irg)               :: numEbins
        integer(kind=irg)               :: numzbins
        integer(kind=irg)               :: totnum_el
        integer(kind=irg),allocatable   :: accum_e(:,:,:)
        integer(kind=irg),allocatable   :: accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accumSP(:,:,:)
end type EBSDMCdataType

type EBSDMPdataType
        integer(kind=irg)               :: lastEnergy
        integer(kind=irg)               :: numEbins
        integer(kind=irg)               :: numset
        character(fnlen)                :: xtalname
        real(kind=sgl),allocatable      :: BetheParameters(:)
        real(kind=sgl),allocatable      :: keVs(:)
        real(kind=sgl),allocatable      :: mLPNH4(:,:,:,:)
        real(kind=sgl),allocatable      :: mLPSH4(:,:,:,:)
        real(kind=sgl),allocatable      :: mLPNH(:,:,:)
        real(kind=sgl),allocatable      :: mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: masterSPNH(:,:,:)
        real(kind=sgl),allocatable      :: masterSPSH(:,:,:)
end type EBSDMPdataType

type EBSDDetectorType
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)          ! auxiliary detector arrays needed for interpolation
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
        type(EBSDPixel),allocatable     :: detector(:,:) 
end type EBSDDetectorType


contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadangles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file
!
!> @param enl EBSD name list structure
!> @param quatang array of unit quaternions (output)
!
!> @date 06/24/14  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDreadangles(enl,numangles,angles,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadangles

use local
use typedefs
use NameListTypedefs
use io
use files
use quaternions
use rotations

IMPLICIT NONE


type(EBSDNameListType),INTENT(INOUT)    :: enl
integer(kind=irg),INTENT(OUT)           :: numangles
type(EBSDAngleType),pointer             :: angles
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: angletype
real(kind=sgl),allocatable              :: eulang(:,:)   ! euler angle array
real(kind=sgl)                          :: qax(4)        ! axis-angle rotation quaternion

real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
integer(kind=irg)                       :: istat
character(fnlen)                        :: anglefile

!====================================
! get the angular information, either in Euler angles or in quaternions, from a text file
!====================================
! open the angle file 
anglefile = trim(EMsoft_getEMdatapathname())//trim(enl%anglefile)
anglefile = EMsoft_toNativePath(anglefile)
open(unit=dataunit,file=trim(anglefile),status='old',action='read')

! get the type of angle first [ 'eu' or 'qu' ]
read(dataunit,*) angletype

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue('Number of angle entries = ',io_int,1)
end if

if (angletype.eq.'eu') then
! allocate the euler angle array
  allocate(eulang(3,numangles),stat=istat)
! if istat.ne.0 then do some error handling ... 
  do i=1,numangles
    read(dataunit,*) eulang(1:3,i)
  end do
  close(unit=dataunit,status='keep')

  if (enl%eulerconvention.eq.'hkl') then
    if (present(verbose)) call Message('  -> converting Euler angles to TSL representation', frm = "(A/)")
    eulang(1,1:numangles) = eulang(1,1:numangles) + 90.0
  end if

! convert the euler angle triplets to quaternions
  allocate(angles%quatang(4,numangles),stat=istat)

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A/)")
  
  do i=1,numangles
    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i)*dtor)
  end do

else
! the input file has quaternions, not Euler triplets
  allocate(angles%quatang(4,numangles),stat=istat)
  do i=1,numangles
    read(dataunit,*) angles%quatang(1:4,i)
  end do
end if

close(unit=dataunit,status='keep')

!====================================
! Do we need to apply an additional axis-angle pair rotation to all the quaternions ?
!
if (enl%axisangle(4).ne.0.0) then
  enl%axisangle(4) = enl%axisangle(4) * dtor
  qax = ax2qu( enl%axisangle )
  do i=1,numangles
    angles%quatang(1:4,i) = quat_mult(qax,angles%quatang(1:4,i))
  end do 
end if

call Message('completed reading Euler angles')

end subroutine EBSDreadangles


!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadangles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file
!
!> @param enl EBSD name list structure
!> @param quatang array of unit quaternions (output)
!
!> @date 06/24/14  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDFullreadangles(enl,numangles,angles,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDFullreadangles

use local
use typedefs
use NameListTypedefs
use io
use files
use quaternions
use rotations

IMPLICIT NONE


type(EBSDFullNameListType),INTENT(INOUT):: enl
integer(kind=irg),INTENT(OUT)           :: numangles
type(EBSDAngleType),pointer             :: angles
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: angletype
real(kind=sgl),allocatable              :: eulang(:,:)   ! euler angle array
real(kind=sgl)                          :: qax(4)        ! axis-angle rotation quaternion

real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
integer(kind=irg)                       :: istat
character(fnlen)                        :: anglefile

!====================================
! get the angular information, either in Euler angles or in quaternions, from a text file
!====================================
! open the angle file 
anglefile = trim(EMsoft_getEMdatapathname())//trim(enl%anglefile)
anglefile = EMsoft_toNativePath(anglefile)
open(unit=dataunit,file=trim(anglefile),status='old',action='read')

! get the type of angle first [ 'eu' or 'qu' ]
read(dataunit,*) angletype

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue('Number of angle entries = ',io_int,1)
end if

if (angletype.eq.'eu') then
! allocate the euler angle array
  allocate(eulang(3,numangles),stat=istat)
! if istat.ne.0 then do some error handling ... 
  do i=1,numangles
    read(dataunit,*) eulang(1:3,i)
  end do
  close(unit=dataunit,status='keep')

  if (enl%eulerconvention.eq.'hkl') then
    if (present(verbose)) call Message('  -> converting Euler angles to TSL representation', frm = "(A/)")
    eulang(1,1:numangles) = eulang(1,1:numangles) + 90.0
  end if

! convert the euler angle triplets to quaternions
  allocate(angles%quatang(4,numangles),stat=istat)
! if (istat.ne.0) then ...

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A/)")
  
  do i=1,numangles
    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i)*dtor)
  end do

else
! the input file has quaternions, not Euler triplets
  allocate(angles%quatang(4,numangles),stat=istat)
  do i=1,numangles
    read(dataunit,*) angles%quatang(1:4,i)
  end do
end if

close(unit=dataunit,status='keep')

call Message('completed reading Euler angles')

end subroutine EBSDFullreadangles


!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadorpcdef
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles, pattern centers, and deformation tensors from an angle file
!
!> @param enl EBSD name list structure
!> @param orpcdef array of unit quaternions, pattern centers, and deformation tensors (output)
!
!> @date 02/22/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDreadorpcdef(enl,numangles,orpcdef,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadorpcdef

use local
use typedefs
use NameListTypedefs
use io
use error
use files
use quaternions
use rotations

IMPLICIT NONE


type(EBSDNameListType),INTENT(INOUT)    :: enl
integer(kind=irg),INTENT(OUT)           :: numangles
type(EBSDAnglePCDefType),pointer        :: orpcdef
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: angletype
real(kind=sgl),allocatable              :: eulang(:,:)   ! euler angle array
real(kind=sgl)                          :: qax(4)        ! axis-angle rotation quaternion

real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
integer(kind=irg)                       :: istat
character(fnlen)                        :: anglefile

!====================================
! get the angular information, either in Euler angles or in quaternions, from a text file
!====================================
! open the angle file 
anglefile = trim(EMsoft_getEMdatapathname())//trim(enl%anglefile)
anglefile = EMsoft_toNativePath(anglefile)
open(unit=dataunit,file=trim(anglefile),status='old',action='read')

! get the type of angle first [ 'eu' or 'qu' ]
read(dataunit,*) angletype
if (angletype.ne.'eu') then 
  call FatalError("EBSDreadorpcdef","Other orientation formats to be implemented; only Euler for now")
end if

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue('Number of angle entries = ',io_int,1)
end if

! allocate the euler angle, pattern center, and deformation tensor arrays
  allocate(eulang(3,numangles),stat=istat)
  allocate(orpcdef%pcs(3,numangles),stat=istat)
  allocate(orpcdef%deftensors(3,3,numangles),stat=istat)

! if istat.ne.0 then do some error handling ... 
  do i=1,numangles
    read(dataunit,*) eulang(1:3,i), orpcdef%pcs(1:3,i), orpcdef%deftensors(1:3,1:3,i)
  end do
  close(unit=dataunit,status='keep')

  if (enl%eulerconvention.eq.'hkl') then
    if (present(verbose)) call Message('  -> converting Euler angles to TSL representation', frm = "(A/)")
    eulang(1,1:numangles) = eulang(1,1:numangles) + 90.0
  end if

! convert the euler angle triplets to quaternions
  allocate(orpcdef%quatang(4,numangles),stat=istat)
! if (istat.ne.0) then ...

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A/)")
  
  do i=1,numangles
    orpcdef%quatang(1:4,i) = eu2qu(eulang(1:3,i)*dtor)
  end do

close(unit=dataunit,status='keep')

write (*,*) 'completed reading Euler angles, pattern centers, and deformation tensors'

end subroutine EBSDreadorpcdef

!--------------------------------------------------------------------------
!
! SUBROUTINE: readEBSDMonteCarloFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read an EBSD Monte Carlo File into the correct namelist and data structure
!
!> @param MCfile filename of the EBSD Monte Carlo file
!> @param mcnl MCCLNameListType
!> @param hdferr error code
!
!> @date 04/02/18 MDG 1.0 started new routine, to eventually replace all other EBSD Monte Carlo reading routines
!--------------------------------------------------------------------------
recursive subroutine readEBSDMonteCarloFile(MCfile, mcnl, hdferr, EBSDMCdata, getAccume, getAccumz, getAccumSP)
!DEC$ ATTRIBUTES DLLEXPORT :: readEBSDMonteCarloFile

use local
use typedefs
use NameListTypedefs
use error
use HDF5
use HDFsupport
use io
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                         :: MCfile
type(MCCLNameListType),INTENT(INOUT)                :: mcnl
integer(kind=irg),INTENT(OUT)                       :: hdferr
type(EBSDMCdataType),INTENT(INOUT)                  :: EBSDMCdata
logical,INTENT(IN),OPTIONAL                         :: getAccume
logical,INTENT(IN),OPTIONAL                         :: getAccumz
logical,INTENT(IN),OPTIONAL                         :: getAccumSP

character(fnlen)                                    :: infile, groupname, datagroupname, dataset
logical                                             :: stat, readonly, g_exists, f_exists, FL
type(HDFobjectStackType),pointer                    :: HDF_head
integer(kind=irg)                                   :: ii, nlines, nx
integer(kind=irg),allocatable                       :: iarray(:)
real(kind=sgl),allocatable                          :: farray(:)
integer(kind=irg),allocatable                       :: accum_e(:,:,:)
integer(kind=irg),allocatable                       :: accum_z(:,:,:,:)
integer(HSIZE_T)                                    :: dims(1), dims2(2), dims3(3), offset3(3), dims4(4) 
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

! we assume that the calling program has opened the HDF interface

infile = trim(EMsoft_getEMdatapathname())//trim(MCfile)
infile = EMsoft_toNativePath(infile)
inquire(file=trim(infile), exist=f_exists)

if (.not.f_exists) then
  call FatalError('readEBSDMonteCarloFile','Monte Carlo input file does not exist')
end if

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(infile), stat, hdferr)

if (stat.eqv..FALSE.) then ! the file exists, so let's open it an first make sure it is an EBSD dot product file
   call FatalError('readEBSDMonteCarloFile','This is not a proper HDF5 file')
end if 
   
! open the Monte Carlo file 
nullify(HDF_head)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

! check whether or not the MC file was generated using DREAM.3D
! this is necessary so that the proper reading of fixed length vs. variable length strings will occur.
! this test sets a flag in side the HDFsupport module so that the proper reading routines will be employed
datagroupname = '/EMheader/MCOpenCL'
call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ComputeMasterPattern','This HDF file does not contain Monte Carlo header data')
end if

groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_MCOpenCL
hdferr = HDF_openGroup(groupname, HDF_head)
FL = .FALSE.
datagroupname = 'FixedLength'
FL = CheckFixedLengthflag(datagroupname, HDF_head)
if (FL.eqv..TRUE.) then 
  call Message('Input file was generated by a program using fixed length strings')
end if
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

!====================================
! make sure this is a Monte Carlo file
!====================================
groupname = SC_NMLfiles
    hdferr = HDF_openGroup(groupname, HDF_head)
dataset = 'MCOpenCLNML'
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..FALSE.) then
    call HDF_pop(HDF_head,.TRUE.)
    call FatalError('readEBSDMonteCarloFile','this is not an EBSD Monte Carlo file')
end if
call HDF_pop(HDF_head)

!====================================
! read all NMLparameters group datasets
!====================================
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_MCCLNameList
    hdferr = HDF_openGroup(groupname, HDF_head)

! we'll read these roughly in the order that the HDFView program displays them...
dataset = SC_Ebinsize
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%Ebinsize)

dataset = SC_Ehistmin
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%Ehistmin)

dataset = SC_EkeV
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%EkeV)

dataset = SC_MCmode
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%MCmode = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_dataname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%dataname = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_depthmax
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%depthmax)

dataset = SC_depthstep
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%depthstep)

dataset = SC_devid
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%devid)

dataset = SC_globalworkgrpsz
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%globalworkgrpsz)

dataset = SC_mode
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%mode = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_multiplier
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%multiplier)
else
    mcnl%multiplier  = 1
end if

dataset = 'num_el'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%num_el)

dataset = SC_numsx
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%numsx)

dataset = SC_omega
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%omega)

dataset = SC_platid
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%platid)
else
    mcnl%platid  = 1
end if

dataset = SC_sig
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%sig)

dataset = SC_stdout
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%stdout)

dataset = SC_totnumel
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%totnum_el)

dataset = SC_xtalname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%xtalname = trim(stringarray(1))
    deallocate(stringarray)

! and close the NMLparameters group
    call HDF_pop(HDF_head)
    call HDF_pop(HDF_head)
!====================================
!====================================

! open the Monte Carlo data group
groupname = SC_EMData
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_MCOpenCL
    hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_multiplier
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMCdata%multiplier)
else
    EBSDMCdata%multiplier = 1
end if

dataset = SC_numEbins
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMCdata%numEbins)

dataset = SC_numzbins
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMCdata%numzbins)

dataset = SC_totnumel
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMCdata%totnum_el)

! various optional arrays
if (present(getAccume)) then 
  if (getAccume.eqv..TRUE.) then
    dataset = SC_accume
    call HDF_readDatasetIntegerArray3D(dataset, dims3, HDF_head, hdferr, accum_e)
    nx = (dims3(2)-1)/2
    allocate(EBSDMCdata%accum_e(1:dims3(1),-nx:nx,-nx:nx))
    EBSDMCdata%accum_e = accum_e
    deallocate(accum_e)
  end if 
end if

if (present(getAccumz)) then 
  if (getAccumz.eqv..TRUE.) then
    dataset = SC_accumz
    call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, accum_z)
    nx = (dims4(3)-1)/2
    allocate(EBSDMCdata%accum_z(1:dims4(1),1:dims4(2),-nx:nx, -nx:nx))
    EBSDMCdata%accum_z = accum_z
    deallocate(accum_z)  
  end if 
end if

if (present(getAccumSP)) then 
  if (getAccumSP.eqv..TRUE.) then
    dataset = SC_accumSP
    call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, EBSDMCdata%accumSP)
  end if 
end if

! and close the HDF5 Monte Carloe file
call HDF_pop(HDF_head,.TRUE.)

call Message(' -> completed reading Monte Carlo data from '//trim(infile), frm = "(A/)")

end subroutine readEBSDMonteCarloFile




!--------------------------------------------------------------------------
!
! SUBROUTINE: readEBSDMasterPatternFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read an EBSD Master Pattern file into the correct namelist and data structure
!
!> @param MPfile filename of the EBSD Master Pattern file
!> @param mpnl EBSDMasterNameListType
!> @param hdferr error code
!
!> @date 04/02/18 MDG 1.0 started new routine, to eventually replace all other EBSD Monte Carlo reading routines
!> @date 08/28/18 MDG 1.1 added keep4 parameter to pass 4D master patterns instead of 3D
!--------------------------------------------------------------------------
recursive subroutine readEBSDMasterPatternFile(MPfile, mpnl, hdferr, EBSDMPdata, getkeVs, getmLPNH, getmLPSH, &
                                               getmasterSPNH, getmasterSPSH, keep4)
!DEC$ ATTRIBUTES DLLEXPORT :: readEBSDMasterPatternFile

use local
use typedefs
use NameListTypedefs
use error
use HDF5
use HDFsupport
use io
use ISO_C_BINDING

IMPLICIT NONE

character(fnlen),INTENT(IN)                         :: MPfile
type(EBSDMasterNameListType),INTENT(INOUT)          :: mpnl
integer(kind=irg),INTENT(OUT)                       :: hdferr
type(EBSDMPdataType),INTENT(INOUT)                  :: EBSDMPdata
logical,INTENT(IN),OPTIONAL                         :: getkeVs
logical,INTENT(IN),OPTIONAL                         :: getmLPNH
logical,INTENT(IN),OPTIONAL                         :: getmLPSH
logical,INTENT(IN),OPTIONAL                         :: getmasterSPNH
logical,INTENT(IN),OPTIONAL                         :: getmasterSPSH
logical,INTENT(IN),OPTIONAL                         :: keep4

character(fnlen)                                    :: infile, groupname, datagroupname, dataset
logical                                             :: stat, readonly, g_exists, f_exists, FL, keepall
type(HDFobjectStackType),pointer                    :: HDF_head
integer(kind=irg)                                   :: ii, nlines, restart, combinesites, uniform, istat
integer(kind=irg),allocatable                       :: iarray(:)
real(kind=sgl),allocatable                          :: farray(:)
real(kind=sgl),allocatable                          :: mLPNH(:,:,:,:)
integer(HSIZE_T)                                    :: dims(1), dims2(2), dims3(3), offset3(3), dims4(4) 
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

keepall = .FALSE.
if (present(keep4)) then
  if (keep4.eqv..TRUE.) keepall = .TRUE.
end if

! we assume that the calling program has opened the HDF interface

infile = trim(EMsoft_getEMdatapathname())//trim(MPfile)
infile = EMsoft_toNativePath(infile)
inquire(file=trim(infile), exist=f_exists)

if (.not.f_exists) then
  call FatalError('readEBSDMasterPatternFile','Master Pattern input file does not exist')
end if

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(infile), stat, hdferr)

if (stat.eqv..FALSE.) then ! the file exists, so let's open it an first make sure it is an EBSD dot product file
   call FatalError('readEBSDMasterPatternFile','This is not a proper HDF5 file')
end if 
   
! open the Monte Carlo file 
nullify(HDF_head)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

! check whether or not the MC file was generated using DREAM.3D
! this is necessary so that the proper reading of fixed length vs. variable length strings will occur.
! this test sets a flag in side the HDFsupport module so that the proper reading routines will be employed
datagroupname = '/EMheader/MCOpenCL'
call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ComputeMasterPattern','This HDF file does not contain Monte Carlo header data')
end if

groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_MCOpenCL
hdferr = HDF_openGroup(groupname, HDF_head)
FL = .FALSE.
datagroupname = 'FixedLength'
FL = CheckFixedLengthflag(datagroupname, HDF_head)
if (FL.eqv..TRUE.) then 
  call Message('Input file was generated by a program using fixed length strings')
end if
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

!====================================
! make sure this is a Master Pattern file
!====================================
groupname = SC_NMLfiles
    hdferr = HDF_openGroup(groupname, HDF_head)
dataset = 'EBSDmasterNML'
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..FALSE.) then
    call HDF_pop(HDF_head,.TRUE.)
    call FatalError('readEBSDMasterPatternFile','this is not an EBSD Master Pattern file')
end if
call HDF_pop(HDF_head)

!====================================
! read all NMLparameters group datasets
!====================================
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDMasterNameList
    hdferr = HDF_openGroup(groupname, HDF_head)

! we'll read these roughly in the order that the HDFView program displays them...
dataset = SC_Esel
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if(g_exists) call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mpnl%Esel)

dataset = SC_combinesites
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
mpnl%combinesites = .FALSE.
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, combinesites)
    if (combinesites.ne.0) mpnl%combinesites = .TRUE.
end if

dataset = SC_copyfromenergyfile
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mpnl%copyfromenergyfile = trim(stringarray(1))
    deallocate(stringarray)
else
    mpnl%copyfromenergyfile = 'n'
end if 

dataset = SC_dmin
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, mpnl%dmin)

dataset = SC_energyfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mpnl%energyfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_npx
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mpnl%npx)

dataset = SC_nthreads
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mpnl%nthreads)

dataset = SC_restart
call H5Lexists_f(HDF_head%objectID, trim(dataset), g_exists, hdferr)
if(g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, restart)
    mpnl%restart = .FALSE.
    if (restart.ne.0) mpnl%restart = .TRUE.
end if

dataset = SC_stdout
call H5Lexists_f(HDF_head%objectID, trim(dataset), g_exists, hdferr)
if(g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mpnl%stdout)
end if

dataset = SC_uniform
mpnl%uniform = .FALSE.
call H5Lexists_f(HDF_head%objectID, trim(dataset), g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, uniform)
    if (uniform.ne.0) mpnl%uniform = .TRUE.
end if 

! and close the NMLparameters group
    call HDF_pop(HDF_head)
    call HDF_pop(HDF_head)
!====================================
!====================================

! open the Monte Carlo data group
groupname = SC_EMData
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDmaster
    hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_lastEnergy
call H5Lexists_f(HDF_head%objectID, trim(dataset), g_exists, hdferr)
if(g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMPdata%lastEnergy)
end if

dataset = SC_numEbins
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMPdata%numEbins)

dataset = SC_numset
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMPdata%numset)

dataset = SC_xtalname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    EBSDMPdata%xtalname = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_BetheParameters
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDMPdata%BetheParameters)

! various optional arrays
if (present(getkeVs)) then 
  if (getkeVs.eqv..TRUE.) then
    dataset = SC_keVs
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, EBSDMPdata%keVs)
  end if 
end if

if (present(getmLPNH)) then 
  if (getmLPNH.eqv..TRUE.) then
    dataset = SC_mLPNH
    call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, mLPNH)
    if (keepall) then
      allocate(EBSDMPdata%mLPNH4(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,EBSDMPdata%numEbins, dims4(4)),stat=istat)
      EBSDMPdata%mLPNH4 = mLPNH
    else
      allocate(EBSDMPdata%mLPNH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,EBSDMPdata%numEbins),stat=istat)
      EBSDMPdata%mLPNH = sum(mLPNH,4)
    end if
    deallocate(mLPNH)
  end if 
end if

if (present(getmLPSH)) then 
  if (getmLPSH.eqv..TRUE.) then
    dataset = SC_mLPSH
    call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, mLPNH)
    if (keepall) then
      allocate(EBSDMPdata%mLPSH4(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,EBSDMPdata%numEbins, dims4(4)),stat=istat)
      EBSDMPdata%mLPSH4 = mLPNH
    else
      allocate(EBSDMPdata%mLPSH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,EBSDMPdata%numEbins),stat=istat)
      EBSDMPdata%mLPSH = sum(mLPNH,4)
    end if
    deallocate(mLPNH)
  end if 
end if

if (present(getmasterSPNH)) then 
  if (getmasterSPNH.eqv..TRUE.) then
    dataset = SC_masterSPNH
    call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, EBSDMPdata%masterSPNH)
  end if 
end if

if (present(getmasterSPSH)) then 
  if (getmasterSPSH.eqv..TRUE.) then
    dataset = SC_masterSPSH
    call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, EBSDMPdata%masterSPSH)
  end if 
end if

! and close the HDF5 Master Pattern file
call HDF_pop(HDF_head,.TRUE.)

call Message(' -> completed reading master pattern data from '//trim(infile), frm = "(A/)")

end subroutine readEBSDMasterPatternFile

! !--------------------------------------------------------------------------
! !
! ! SUBROUTINE:EBSDreadMasterfile_overlap
! !
! !> @author Marc De Graef, Carnegie Mellon University
! !
! !> @brief read EBSD master pattern from file
! !
! !> @param enl EBSDoverlap name list structure
! !> @param 
! !
! !> @date 06/24/14  MDG 1.0 original
! !> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
! !> @date 09/03/15  MDG 2.1 removed old file format support
! !> @date 06/03/18  MDG 3.0 commented out entire routine; not used anywhere.
! !--------------------------------------------------------------------------
! recursive subroutine EBSDreadMasterfile_overlap(enl, master, mfile, verbose)
! !DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadMasterfile_overlap

! use local
! use typedefs
! use NameListTypedefs
! use files
! use io
! use error
! use HDF5
! use HDFsupport


! IMPLICIT NONE

! type(EBSDoverlapNameListType),INTENT(INOUT)    :: enl
! type(EBSDMasterType),pointer            :: master
! character(fnlen),INTENT(IN),OPTIONAL    :: mfile
! logical,INTENT(IN),OPTIONAL             :: verbose

! real(kind=sgl),allocatable              :: sr(:,:,:) 
! real(kind=sgl),allocatable              :: EkeVs(:) 
! integer(kind=irg),allocatable           :: atomtype(:)

! real(kind=sgl),allocatable              :: srtmp(:,:,:,:)
! integer(kind=irg)                       :: istat

! logical                                 :: stat, readonly
! integer(kind=irg)                       :: hdferr, nlines
! integer(HSIZE_T)                        :: dims(1), dims4(4)
! character(fnlen)                        :: groupname, dataset, masterfile
! character(fnlen),allocatable            :: stringarray(:)

! type(HDFobjectStackType),pointer        :: HDF_head

! ! open the fortran HDF interface
! call h5open_EMsoft(hdferr)

! nullify(HDF_head, HDF_head)

! ! is the mfile parameter present? If so, use it as the filename, otherwise use the enl%masterfile parameter
! if (PRESENT(mfile)) then
!   masterfile = trim(EMsoft_getEMdatapathname())//trim(mfile)
! else
!   masterfile = trim(EMsoft_getEMdatapathname())//trim(enl%masterfile)
! end if 
! masterfile = EMsoft_toNativePath(masterfile)

! ! first, we need to check whether or not the input file is of the HDF5 format type
! call h5fis_hdf5_f(trim(masterfile), stat, hdferr)

! if (stat) then 
! ! open the master file 
!   readonly = .TRUE.
!   hdferr =  HDF_openFile(masterfile, HDF_head, readonly)

! ! open the namelist group
! groupname = SC_NMLparameters
!   hdferr = HDF_openGroup(groupname, HDF_head)

! groupname = SC_EBSDMasterNameList
!   hdferr = HDF_openGroup(groupname, HDF_head)

! ! read all the necessary variables from the namelist group
! dataset = SC_energyfile
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%Masterenergyfile = trim(stringarray(1))
!   deallocate(stringarray)

! dataset = SC_npx
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
!   enl%npy = enl%npx

!   call HDF_pop(HDF_head)
!   call HDF_pop(HDF_head)

! groupname = SC_EMData
!   hdferr = HDF_openGroup(groupname, HDF_head)

! dataset = SC_numEbins
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)

! dataset = SC_numset
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numset)

! ! dataset = 'squhex'
! ! call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
! ! enl%sqorhe = trim(stringarray(1))
! ! deallocate(stringarray)

! dataset = SC_mLPNH
!   call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
!   allocate(master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
!   master%mLPNH = sum(srtmp,4)
!   deallocate(srtmp)

! dataset = SC_mLPSH
!   call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
!   allocate(master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
!   master%mLPSH = sum(srtmp,4)
!   deallocate(srtmp)

! dataset = SC_xtalname
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%Masterxtalname = trim(stringarray(1))
!   deallocate(stringarray)

!   call HDF_pop(HDF_head)

! groupname = SC_EMheader
!   hdferr = HDF_openGroup(groupname, HDF_head)

! dataset = SC_ProgramName
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%Masterprogname = trim(stringarray(1))
!   deallocate(stringarray)
  
! dataset = SC_Version
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%Masterscversion = trim(stringarray(1))
!   deallocate(stringarray)
  
!   call HDF_pop(HDF_head,.TRUE.)

! ! close the fortran HDF interface
!   call h5close_EMsoft(hdferr)

! else
!   masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
!   call FatalError('EBSDreadMasterfile_overlap',masterfile)
! end if
! !====================================

! if (present(verbose)) then
!   if (verbose) call Message(' -> completed reading '//trim(masterfile), frm = "(A)")
! end if

! end subroutine EBSDreadMasterfile_overlap


!--------------------------------------------------------------------------
!
! SUBROUTINE:GenerateEBSDDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl EBSD name list structure
!> @param mcnl Monte Carlo name list structure
!> @param EBSDMCdata MC data
!> @param EBSDdetector detector arrays
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS 1.1 added omega as the second tilt angle
!> @date 07/07/15   SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 04/03/18  MDG 3.0 new version with split use of name list arrays
!> @date 02/19/19  MDG 4.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine GenerateEBSDDetector(enl, mcnl, EBSDMCdata, EBSDdetector, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: GenerateEBSDDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
type(EBSDDetectorType),INTENT(INOUT)    :: EBSDdetector
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:), testarray(:,:)                 ! scintillator coordinate arrays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable              :: z(:,:)           
integer(kind=irg)                       :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nsx, nsy, elp  ! various parameters
real(kind=sgl)                          :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                          :: ixy(2)


!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(enl%numsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i-1, i=1,enl%numsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (mcnl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(mcnl%omega * dtor)
sw = sin(mcnl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...

elp = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   EBSDdetector%rgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   EBSDdetector%rgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   EBSDdetector%rgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(EBSDdetector%rgx*EBSDdetector%rgx+EBSDdetector%rgy*EBSDdetector%rgy+EBSDdetector%rgz*EBSDdetector%rgz)
  EBSDdetector%rgx = EBSDdetector%rgx*z
  EBSDdetector%rgy = EBSDdetector%rgy*z
  EBSDdetector%rgz = EBSDdetector%rgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!
  nsx = (mcnl%numsx - 1)/2
  nsy = nsx
! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(nsx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

  Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins

! correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
  ipx = enl%numsx/2 + nint(enl%xpc)
  ipy = enl%numsy/2 + nint(enl%ypc)
  pcvec = (/ EBSDdetector%rgx(ipx,elp-ipy), EBSDdetector%rgy(ipx,elp-ipy), EBSDdetector%rgz(ipx,elp-ipy) /)
  calpha = cos(alpha)
  do i=1,enl%numsx
    do j=1,enl%numsy
! do the coordinate transformation for this detector pixel
       dc = (/ EBSDdetector%rgx(i,elp-j),EBSDdetector%rgy(i,elp-j),EBSDdetector%rgz(i,elp-j) /)
! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
!      call LambertgetInterpolation(dc, scl, enl%nsx, enl%nsy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)
        ixy = scl * LambertSphereToSquare( dc, istat )
        x = ixy(1)
        ixy(1) = ixy(2)
        ixy(2) = -x
! four-point interpolation (bi-quadratic)
        nix = int(nsx+ixy(1))-nsx
        niy = int(nsy+ixy(2))-nsy
        dx = ixy(1)-nix
        dy = ixy(2)-niy
        dxm = 1.0-dx
        dym = 1.0-dy
! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          g = 0.25 
        else
          g = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if
! interpolate the intensity 
        do k=Emin,Emax 
          s = EBSDMCdata%accum_e(k,nix,niy) * dxm * dym + &
              EBSDMCdata%accum_e(k,nix+1,niy) * dx * dym + &
              EBSDMCdata%accum_e(k,nix,niy+1) * dxm * dy + &
              EBSDMCdata%accum_e(k,nix+1,niy+1) * dx * dy
          EBSDdetector%accum_e_detector(k,i,elp-j) = g * s
        end do
    end do
  end do 

if (present(verbose)) call Message(' -> completed detector generation', frm = "(A)")

!====================================
end subroutine GenerateEBSDDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE:GeneratemyEBSDDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays for the case where each pattern has a (slightly) different detector configuration
!
!> @param enl EBSD name list structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS 1.1 added omega as the second tilt angle
!> @date 07/07/15   SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 02/22/18  MDG 1.3 forked from EBSDGenerateDetector; uses separate pattern center coordinates patcntr
!> @date 04/03/18  MDG 2.0 updated with new name list and data structures
!> @date 02/19/19  MDG 3.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine GeneratemyEBSDDetector(enl, mcnl, EBSDMCdata, nsx, nsy, numE, tgx, tgy, tgz, accum_e_detector, patcntr, bg)
!DEC$ ATTRIBUTES DLLEXPORT :: GeneratemyEBSDDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
type(EBSDMCdataType),INTENT(INOUT)      :: EBSDMCdata
integer(kind=irg),INTENT(IN)            :: nsx
integer(kind=irg),INTENT(IN)            :: nsy
integer(kind=irg),INTENT(IN)            :: numE
real(kind=sgl),INTENT(INOUT)            :: tgx(nsx,nsy)
real(kind=sgl),INTENT(INOUT)            :: tgy(nsx,nsy)
real(kind=sgl),INTENT(INOUT)            :: tgz(nsx,nsy)
real(kind=sgl),INTENT(INOUT)            :: accum_e_detector(numE,nsx,nsy)
real(kind=sgl),INTENT(IN)               :: patcntr(3)
logical,INTENT(IN),OPTIONAL             :: bg

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:), testarray(:,:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable              :: z(:,:)           
integer(kind=irg)                       :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nx, ny, elp     ! various parameters
real(kind=sgl)                          :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx, xpc, ypc, L         ! various parameters
real(kind=sgl)                          :: ixy(2)

!====================================
! ------ generate the detector arrays
!====================================
xpc = patcntr(1)
ypc = patcntr(2)
L = patcntr(3)

allocate(scin_x(nsx),scin_y(nsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( xpc - ( 1.0 - nsx ) * 0.5 - (/ (i-1, i=1,nsx) /) ) * enl%delta
scin_y = ( ypc - ( 1.0 - nsy ) * 0.5 - (/ (i-1, i=1,nsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (mcnl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(mcnl%omega * dtor)
sw = sin(mcnl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...

elp = nsy + 1
L2 = L * L
do j=1,nsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + L*cw
  Lc = cw * scin_x(j) + L*sw
  do i=1,nsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   tgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   tgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   tgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(tgx*tgx+tgy*tgy+tgz*tgz)
  tgx = tgx*z
  tgy = tgy*z
  tgz = tgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!
nx = (mcnl%numsx - 1)/2
ny = nsx
if (present(bg)) then
 if (bg.eqv..TRUE.) then 
! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(nx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins

  Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins

! correction of change in effective pixel area compared to equal-area Lambert projection
  alpha = atan(enl%delta/L/sqrt(sngl(cPi)))
  ipx = nsx/2 + nint(xpc)
  ipy = nsy/2 + nint(ypc)
  pcvec = (/ tgx(ipx,elp-ipy), tgy(ipx,elp-ipy), tgz(ipx,elp-ipy) /)
  calpha = cos(alpha)
  do i=1,nsx
    do j=1,nsy
! do the coordinate transformation for this detector pixel
       dc = (/ tgx(i,elp-j),tgy(i,elp-j),tgz(i,elp-j) /)
! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc
! convert these direction cosines to coordinates in the Rosca-Lambert projection
        ixy = scl * LambertSphereToSquare( dc, istat )
        x = ixy(1)
        ixy(1) = ixy(2)
        ixy(2) = -x
! four-point interpolation (bi-quadratic)
        nix = int(nx+ixy(1))-nx
        niy = int(ny+ixy(2))-ny
        dx = ixy(1)-nix
        dy = ixy(2)-niy
        dxm = 1.0-dx
        dym = 1.0-dy
! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          g = 0.25 
        else
          g = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if
! interpolate the intensity 
        do k=Emin,Emax 
          s = EBSDMCdata%accum_e(k,nix,niy) * dxm * dym + &
              EBSDMCdata%accum_e(k,nix+1,niy) * dx * dym + &
              EBSDMCdata%accum_e(k,nix,niy+1) * dxm * dy + &
              EBSDMCdata%accum_e(k,nix+1,niy+1) * dx * dy
          accum_e_detector(k,i,elp-j) = g * s
        end do
    end do
  end do 
 else
   accum_e_detector = 1.0
 end if 
end if
end subroutine GeneratemyEBSDDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcEBSDPatternSingleFull
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single EBSD pattern, used in many programs
!
!> @param ebsdnl EBSD namelist
!> @param holdexpt logical
!> @param holddict logical
!
!> @date 03/17/16 MDG 1.0 original
!> @date 09/26/17 MDG 1.1 added Umatrix argument to try out inclusion of lattice strains
!> @date 02/19/19 MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine CalcEBSDPatternSingleFull(ipar,qu,accum,mLPNH,mLPSH,rgx,rgy,rgz,binned,Emin,Emax,mask, &
                                               prefactor, Fmatrix, removebackground, applynoise)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcEBSDPatternSingleFull

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use symmetry
use crystal
use constants
use io
use files
use diffraction
use Lambert
use quaternions
use rotations
use filters

IMPLICIT NONE

integer, parameter                              :: K4B=selected_int_kind(9)

integer(kind=irg),INTENT(IN)                    :: ipar(7)
real(kind=sgl),INTENT(IN)                       :: qu(4) 
real(kind=dbl),INTENT(IN)                       :: prefactor
integer(kind=irg),INTENT(IN)                    :: Emin, Emax
real(kind=sgl),INTENT(IN)                       :: accum(ipar(6),ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: rgx(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgy(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgz(ipar(2),ipar(3))
real(kind=sgl),INTENT(OUT)                      :: binned(ipar(2)/ipar(1),ipar(3)/ipar(1))
real(kind=sgl),INTENT(IN)                       :: mask(ipar(2)/ipar(1),ipar(3)/ipar(1))
real(kind=dbl),INTENT(IN),optional              :: Fmatrix(3,3)
character(1),INTENT(IN),OPTIONAL                :: removebackground
integer(K4B),INTENT(INOUT),OPTIONAL             :: applynoise

real(kind=sgl),allocatable                      :: EBSDpattern(:,:)
real(kind=sgl),allocatable                      :: wf(:)
real(kind=sgl)                                  :: dc(3),ixy(2),scl,bindx, tmp
real(kind=sgl)                                  :: dx,dy,dxm,dym, x, y, z
integer(kind=irg)                               :: ii,jj,kk,istat
integer(kind=irg)                               :: nix,niy,nixp,niyp
logical                                         :: nobg, noise

! ipar(1) = ebsdnl%binning
! ipar(2) = ebsdnl%numsx
! ipar(3) = ebsdnl%numsy
! ipar(4) = ebsdnl%npx
! ipar(5) = ebsdnl%npy
! ipar(6) = ebsdnl%numEbins
! ipar(7) = ebsdnl%nE

nobg = .FALSE.
if (present(removebackground)) then
  if (removebackground.eq.'y') nobg = .TRUE.
end if

noise = .FALSE.
if (present(applynoise)) then
  if (applynoise.ne.0_K4B) noise = .TRUE.
end if

allocate(EBSDpattern(ipar(2),ipar(3)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(3)
! get the pixel direction cosines from the pre-computed array
        dc = (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /)
! apply the grain rotation 
        dc = quat_Lp(qu(1:4),  dc)
! apply the deformation if present
        if (present(Fmatrix)) then
          dc = matmul(sngl(Fmatrix), dc)
        end if
! and normalize the direction cosines (to remove any rounding errors)
        dc = dc/sqrt(sum(dc**2))

! convert these direction cosines to interpolation coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, ipar(4), ipar(5), nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
        if (nobg.eqv..TRUE.) then 
          if (dc(3) .ge. 0.0) then
            do kk = Emin, Emax
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + ( mLPNH(nix,niy,kk) * dxm * dym + &
                                               mLPNH(nixp,niy,kk) * dx * dym + mLPNH(nix,niyp,kk) * dxm * dy + &
                                               mLPNH(nixp,niyp,kk) * dx * dy )

            end do
          else
            do kk = Emin, Emax
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + ( mLPSH(nix,niy,kk) * dxm * dym + &
                                               mLPSH(nixp,niy,kk) * dx * dym + mLPSH(nix,niyp,kk) * dxm * dy + &
                                               mLPSH(nixp,niyp,kk) * dx * dy )

            end do

          end if
        else
          if (dc(3) .ge. 0.0) then
            do kk = Emin, Emax
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + accum(kk,ii,jj) * ( mLPNH(nix,niy,kk) * dxm * dym + &
                                               mLPNH(nixp,niy,kk) * dx * dym + mLPNH(nix,niyp,kk) * dxm * dy + &
                                               mLPNH(nixp,niyp,kk) * dx * dy )

            end do
          else
            do kk = Emin, Emax
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + accum(kk,ii,jj) * ( mLPSH(nix,niy,kk) * dxm * dym + &
                                               mLPSH(nixp,niy,kk) * dx * dym + mLPSH(nix,niyp,kk) * dxm * dy + &
                                               mLPSH(nixp,niyp,kk) * dx * dy )

            end do

          end if
        end if 
    end do
end do

EBSDpattern = prefactor * EBSDpattern

! do we need to apply Poisson noise ?  (slow...)
if (noise.eqv..TRUE.) then 
  EBSDpattern = applyPoissonNoise( EBSDpattern, ipar(2), ipar(3), applynoise )
end if

! do we need to bin the pattern ?
if (ipar(1) .ne. 1) then
    do ii=1,ipar(2),ipar(1)
        do jj=1,ipar(3),ipar(1)
            binned(ii/ipar(1)+1,jj/ipar(1)+1) = &
            sum(EBSDpattern(ii:ii+ipar(1)-1,jj:jj+ipar(1)-1))
        end do
    end do
! and divide by binning^2
!   binned = binned * bindx
else
    binned = EBSDpattern
end if

binned = binned * mask

end subroutine CalcEBSDPatternSingleFull


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcEBSDPatternSingleFullFast
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single EBSD pattern, used in many programs
!
!> @param ebsdnl EBSD namelist
!> @param holdexpt logical
!> @param holddict logical
!
!> @date 07/06/16 MDG 1.0 original, based on CalcEBSDPatternSingleFull
!--------------------------------------------------------------------------
recursive subroutine CalcEBSDPatternSingleFullFast(ipar,qu,accum,mLPNH,mLPSH,rgx,rgy,rgz,binned,Emin,Emax,prefactor)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcEBSDPatternSingleFullFast

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use symmetry
use crystal
use constants
use io
use files
use diffraction
use Lambert
use quaternions
use rotations

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                    :: ipar(8)
real(kind=sgl),INTENT(IN)                       :: qu(4) 
real(kind=dbl),INTENT(IN)                       :: prefactor
integer(kind=irg),INTENT(IN)                    :: Emin, Emax
real(kind=sgl),INTENT(IN)                       :: accum(ipar(6),ipar(2),ipar(8))
real(kind=sgl),INTENT(IN)                       :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: rgx(ipar(2),ipar(8))
real(kind=sgl),INTENT(IN)                       :: rgy(ipar(2),ipar(8))
real(kind=sgl),INTENT(IN)                       :: rgz(ipar(2),ipar(8))
real(kind=sgl),INTENT(OUT)                      :: binned(ipar(2),ipar(8))

real(kind=sgl),allocatable                      :: EBSDpattern(:,:)
real(kind=sgl),allocatable                      :: wf(:)
real(kind=sgl)                                  :: dc(3),ixy(2),scl,bindx
real(kind=sgl)                                  :: dx,dy,dxm,dym
integer(kind=irg)                               :: ii,jj,kk,istat, ystep
integer(kind=irg)                               :: nix,niy,nixp,niyp


! ipar(1) = ebsdnl%binning
! ipar(2) = ebsdnl%numsx
! ipar(3) = ebsdnl%numsy
! ipar(4) = ebsdnl%npx
! ipar(5) = ebsdnl%npy
! ipar(6) = ebsdnl%numEbins
! ipar(7) = ebsdnl%nE
! ipar(8) = ebsdnl%nlines


ystep = floor(float(ipar(3))/float(ipar(8)+1))

! bindx = 1.0/float(ipar(1))**2

allocate(EBSDpattern(ipar(2),ipar(8)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(8)

        dc = sngl(quat_Lp(qu(1:4),  (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /) ))

        dc = dc/sqrt(sum(dc**2))

! convert these direction cosines to interpolation coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, ipar(4), ipar(5), nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
        if (dc(3) .ge. 0.0) then
            do kk = Emin, Emax
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + accum(kk,ii,jj) * ( mLPNH(nix,niy,kk) * dxm * dym + &
                                               mLPNH(nixp,niy,kk) * dx * dym + mLPNH(nix,niyp,kk) * dxm * dy + &
                                               mLPNH(nixp,niyp,kk) * dx * dy )

            end do
        else
            do kk = Emin, Emax
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + accum(kk,ii,jj) * ( mLPSH(nix,niy,kk) * dxm * dym + &
                                               mLPSH(nixp,niy,kk) * dx * dym + mLPSH(nix,niyp,kk) * dxm * dy + &
                                               mLPSH(nixp,niyp,kk) * dx * dy )

            end do

        end if
    end do
end do

binned = prefactor * EBSDpattern

end subroutine CalcEBSDPatternSingleFullFast

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl EBSD name list structure
!
!> @date 06/24/14 MDG 1.0 original
!> @date 07/01/15  SS 1.1 added omega as the second tilt angle
!> @date 07/07/15  SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 05/10/18 MDG 1.3 added arguments to clean up namelists 
!> @date 02/19/19 MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine EBSDFullGenerateDetector(enl, EBSDdetector, numEbins, numzbins, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDFullGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert
use error

IMPLICIT NONE

type(EBSDFullNameListType),INTENT(INOUT):: enl
type(EBSDDetectorType),INTENT(INOUT)    :: EBSDdetector
integer(kind=irg),INTENT(IN)            :: numEbins, numzbins
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
integer(kind=irg)                       :: i, j, Emin, Emax, istat, k, ipx, ipy, ierr, elp   
real(kind=sgl)                          :: dc(3), scl, alpha, theta, g, pcvec(3), s, dp           ! direction cosine array
real(kind=sgl)                          :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                          :: ixy(2)


!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(enl%numsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i-1, i=1,enl%numsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (enl%sig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
elp = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy

   rhos = 1.0/sqrt(sx + scin_y(i)**2)

   allocate(EBSDdetector%detector(j,elp-i)%lambdaEZ(1:numEbins,1:numzbins))

   EBSDdetector%detector(j,elp-i)%lambdaEZ = 0.D0

   EBSDdetector%detector(j,elp-i)%dc = (/(scin_y(i) * ca + sa * Ls) * rhos, Lc * rhos,&
                                    (-sa * scin_y(i) + ca * Ls) * rhos/)

   EBSDdetector%detector(j,elp-i)%dc =  &
         EBSDdetector%detector(j,elp-i)%dc/NORM2(EBSDdetector%detector(j,elp-i)%dc)

!  if (ierr .ne. 0) then
!      call FatalError('EBSDFullGenerateDetector:','Lambert Projection coordinate undefined')
!  end if

  end do
end do
deallocate(scin_x, scin_y)

alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
ipx = nint(enl%numsx/2 + enl%xpc)
ipy = nint(enl%numsy/2 + enl%ypc)
pcvec = EBSDdetector%detector(ipx,elp-ipy)%dc
calpha = cos(alpha)

do i = 1,enl%numsx
    do j = 1,enl%numsy

        dc = EBSDdetector%detector(i,elp-j)%dc 
        dp = DOT_PRODUCT(pcvec,dc)
        theta = acos(dp)

        if ((i.eq.ipx).and.(j.eq.ipy)) then
          EBSDdetector%detector(i,elp-j)%cfactor = 0.25 
        else
          EBSDdetector%detector(i,elp-j)%cfactor = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if

    end do
end do

if (present(verbose)) call Message(' -> completed detector generation', frm = "(A)")

!====================================
end subroutine EBSDFullGenerateDetector



!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDcopyMCdata
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief copy Monte Carlo data from one file to a new file using h5copy
!
!> @param inputfile name of file with MC data in it
!> @param outputfile name of new file
!
!> @date 08/24/17  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDcopyMCdata(inputfile, outputfile)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDcopyMCdata

use local
use error
use HDFsupport
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)       :: inputfile
character(fnlen),INTENT(IN)       :: outputfile

character(fnlen)                  :: infile, outfile, h5copypath, groupname
character(512)                    :: cmd, cmd2
logical                           :: f_exists, readonly
type(HDFobjectStackType),pointer  :: HDF_head
integer(kind=irg)                 :: hdferr

! first make sure that the input file exists and has MC data in it
infile = trim(EMsoft_getEMdatapathname())//trim(inputfile)
infile = EMsoft_toNativePath(infile)
inquire(file=infile, exist=f_exists)

outfile = trim(EMsoft_getEMdatapathname())//trim(outputfile)
outfile = EMsoft_toNativePath(outfile)

! if the file does not exist, abort the program with an error message
if (f_exists.eqv..FALSE.) then 
  call FatalError('EBSDcopyMCdata','Monte Carlo copyfromenergyfile does not exist: '//trim(infile))
end if

! make sure it has MCopenCL data in it; hdf open is done in the calling program
nullify(HDF_head)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.eq.-1) then 
  call FatalError('EBSDcopyMCdata','EMData group does not exist in '//trim(infile))
end if

groupname = SC_MCOpenCL
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.eq.-1) then 
  call FatalError('EBSDcopyMCdata','MCOpenCL group does not exist in '//trim(infile))
end if

call HDF_pop(HDF_head,.TRUE.)

! OK, if we get here, then the file does exist and it contains Monte Carlo data, so we let
! the user know
call Message('--> Input file contains Monte Carlo data')

! next, we copy the necessary groups into the new Monte Carlo file
h5copypath = trim(EMsoft_geth5copypath())//' -p -v '
h5copypath = EMsoft_toNativePath(h5copypath)
cmd = trim(h5copypath)//' -i "'//trim(infile)
cmd = trim(cmd)//'" -o "'//trim(outfile)

cmd2 = trim(cmd)//'" -s "/CrystalData" -d "/CrystalData"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/EMData/MCOpenCL" -d "/EMData/MCOpenCL"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/EMheader/MCOpenCL" -d "/EMheader/MCOpenCL"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/NMLfiles/MCOpenCLNML" -d "/NMLfiles/MCOpenCLNML"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/NMLparameters/MCCLNameList" -d "/NMLparameters/MCCLNameList"'
call system(trim(cmd2))

call Message('--> Output file generated with Monte Carlo data copied from '//trim(infile))

end subroutine EBSDcopyMCdata 


end module EBSDmod
