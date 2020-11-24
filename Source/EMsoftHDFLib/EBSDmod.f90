! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
        real(kind=sgl),allocatable      :: quatangfield(:,:,:,:)
        real(kind=sgl),allocatable      :: pcs(:,:)
        real(kind=sgl),allocatable      :: deftensors(:,:,:)
        real(kind=dbl),allocatable      :: pcfield(:,:,:)
        real(kind=dbl),allocatable      :: deftensorfield(:,:,:,:)
end type EBSDAnglePCDefType

type EBSDLargeAccumType
        integer(kind=irg),allocatable   :: accum_e(:,:,:),accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
end type EBSDLargeAccumType

type EBSDMasterType
        real(kind=sgl),allocatable      :: mLPNH(:,:,:) , mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)          ! auxiliary detector arrays needed for interpolation
end type EBSDMasterType

type EBSDSEMArray
        integer(kind=irg),allocatable   ::SEM_X(:)
        integer(kind=irg),allocatable   ::SEM_Y(:)
end type

interface CalcEBSDPatternDefect
  module procedure CalcEBSDPatternDefect_zint
  module procedure CalcEBSDPatternDefect_noint
end interface


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
!f2py intent(in,out) ::  enl
integer(kind=irg),INTENT(OUT)           :: numangles
type(EBSDAngleType),INTENT(INOUT)       :: angles
!f2py intent(in,out) ::  angles
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: atype
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
read(dataunit,*) atype

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue('Number of angle entries = ',io_int,1)
end if

if (atype.eq.'eu') then
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
!f2py intent(in,out) ::  enl
integer(kind=irg),INTENT(OUT)           :: numangles
type(EBSDAngleType),INTENT(INOUT)       :: angles
!f2py intent(in,out) ::  angles
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: atype
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
read(dataunit,*) atype

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue('Number of angle entries = ',io_int,1)
end if

if (atype.eq.'eu') then
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
!f2py intent(in,out) ::  enl
integer(kind=irg),INTENT(OUT)           :: numangles
type(EBSDAnglePCDefType),INTENT(INOUT)  :: orpcdef
!f2py intent(in,out) ::  orpcdef
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: atype
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
read(dataunit,*) atype
if (atype.ne.'eu') then 
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
!> @date 11/11/19 MDG 1.1 adds support for interaction volume array reading
!--------------------------------------------------------------------------
recursive subroutine readEBSDMonteCarloFile(MCfile, mcnl, hdferr, EBSDMCdata, getAccume, getAccumz, getAccumSP, &
                                            getAccumxyz, verbose)
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
!f2py intent(in,out) ::  mcnl
integer(kind=irg),INTENT(OUT)                       :: hdferr
type(EBSDMCdataType),INTENT(INOUT)                  :: EBSDMCdata
!f2py intent(in,out) ::  EBSDMCdata
logical,INTENT(IN),OPTIONAL                         :: getAccume
logical,INTENT(IN),OPTIONAL                         :: getAccumz
logical,INTENT(IN),OPTIONAL                         :: getAccumSP
logical,INTENT(IN),OPTIONAL                         :: getAccumxyz   ! for interaction volume array
logical,INTENT(IN),OPTIONAL                         :: verbose

character(fnlen)                                    :: infile, groupname, datagroupname, dataset
logical                                             :: stat, readonly, g_exists, f_exists, FL
type(HDFobjectStackType)                            :: HDF_head
integer(kind=irg)                                   :: ii, nlines, nx, ny, nz
real(kind=dbl)                                      :: x
integer(kind=irg),allocatable                       :: iarray(:)
real(kind=sgl),allocatable                          :: farray(:)
integer(kind=irg),allocatable                       :: accum_e(:,:,:)
integer(kind=irg),allocatable                       :: accum_xyz(:,:,:)
integer(kind=irg),allocatable                       :: accum_z(:,:,:,:)
integer(HSIZE_T)                                    :: dims(1), dims2(2), dims3(3), offset3(3), dims4(4) 
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

! we assume that the calling program has opened the HDF interface

if (MCfile(1:1).eq.EMsoft_getEMsoftnativedelimiter()) then 
  infile = trim(MCfile)
else
  infile = trim(EMsoft_getEMdatapathname())//trim(MCfile)
  infile = EMsoft_toNativePath(infile)
end if
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
nullify(HDF_head%next)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

! check whether or not the MC file was generated using DREAM.3D
! this is necessary so that the proper reading of fixed length vs. variable length strings will occur.
! this test sets a flag in side the HDFsupport module so that the proper reading routines will be employed
datagroupname = '/EMheader/MCOpenCL'
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
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
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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
dataset = SC_mode
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%mode = trim(stringarray(1))
    deallocate(stringarray)


if (trim(mcnl%mode).ne.'Ivol') then
    dataset = SC_Ebinsize
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%Ebinsize)

    dataset = SC_Ehistmin
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%Ehistmin)

    dataset = SC_depthmax
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%depthmax)

    dataset = SC_depthstep
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%depthstep)
else
    dataset = 'ivolx'
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%ivolx) 

    dataset = 'ivoly'
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%ivoly) 

    dataset = 'ivolz'
      call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%ivolz) 

    dataset = 'ivolstepx'
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, x)
      mcnl%ivolstepx = sngl(x)

    dataset = 'ivolstepy'
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, x)
      mcnl%ivolstepy = sngl(x)

    dataset = 'ivolstepz'
      call HDF_readDatasetDouble(dataset, HDF_head, hdferr, x)
      mcnl%ivolstepz = sngl(x)
end if

dataset = SC_EkeV
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, mcnl%EkeV)

dataset = SC_dataname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%dataname = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_MCmode
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    mcnl%MCmode = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_devid
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%devid)

dataset = SC_globalworkgrpsz
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mcnl%globalworkgrpsz)


dataset = SC_multiplier
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMCdata%multiplier)
else
    EBSDMCdata%multiplier = 1
end if

if (trim(mcnl%mode).ne.'Ivol') then 
  dataset = SC_numEbins
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMCdata%numEbins)
end if

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

if (present(getAccumxyz)) then 
  if (getAccumxyz.eqv..TRUE.) then
    dataset = SC_accumxyz
    call HDF_readDatasetIntegerArray3D(dataset, dims3, HDF_head, hdferr, accum_xyz)
    nx = (dims3(1)-1)/2
    ny = (dims3(2)-1)/2
    nz = dims3(3)
    allocate(EBSDMCdata%accum_xyz(-nx:nx, -ny:ny, nz))
    EBSDMCdata%accum_xyz = accum_xyz
    deallocate(accum_xyz)  
  end if 
end if

! and close the HDF5 Monte Carloe file
call HDF_pop(HDF_head,.TRUE.)

if (present(verbose)) then
	if (verbose.eqv..TRUE.) then
		call Message(' -> completed reading Monte Carlo data from '//trim(infile), frm = "(A/)")
	end if 
end if

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
!> @date 11/05/19 MDG 1.2 added functionality for defect EBSD simulations
!--------------------------------------------------------------------------
recursive subroutine readEBSDMasterPatternFile(MPfile, mpnl, hdferr, EBSDMPdata, getkeVs, getmLPNH, getmLPSH, &
                                               getmasterSPNH, getmasterSPSH, keep4, defectMP, verbose)
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
!f2py intent(in,out) ::  mpnl
integer(kind=irg),INTENT(OUT)                       :: hdferr
type(EBSDMPdataType),INTENT(INOUT)                  :: EBSDMPdata
!f2py intent(in,out) ::  EBSDMPdata
logical,INTENT(IN),OPTIONAL                         :: getkeVs
logical,INTENT(IN),OPTIONAL                         :: getmLPNH
logical,INTENT(IN),OPTIONAL                         :: getmLPSH
logical,INTENT(IN),OPTIONAL                         :: getmasterSPNH
logical,INTENT(IN),OPTIONAL                         :: getmasterSPSH
logical,INTENT(IN),OPTIONAL                         :: keep4
logical,INTENT(IN),OPTIONAL                         :: defectMP
logical,INTENT(IN),OPTIONAL                         :: verbose

character(fnlen)                                    :: infile, groupname, datagroupname, dataset
logical                                             :: stat, readonly, g_exists, f_exists, FL, keepall, dfMP
type(HDFobjectStackType)                            :: HDF_head
integer(kind=irg)                                   :: ii, nlines, restart, combinesites, uniform, istat
integer(kind=irg),allocatable                       :: iarray(:)
real(kind=sgl),allocatable                          :: farray(:)
real(kind=sgl),allocatable                          :: mLPNH(:,:,:,:), mLPNH3(:,:,:)
integer(HSIZE_T)                                    :: dims(1), dims2(2), dims3(3), offset3(3), dims4(4) 
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

dfMP = .FALSE.
if (present(defectMP)) then
  if (defectMP.eqv..TRUE.) dfMP = .TRUE.
end if

keepall = .FALSE.
if (present(keep4)) then
  if (keep4.eqv..TRUE.) keepall = .TRUE.
end if

! we assume that the calling program has opened the HDF interface
if (MPfile(1:1).ne.EMsoft_getEMsoftnativedelimiter()) then 
  infile = trim(EMsoft_getEMdatapathname())//trim(MPfile)
  infile = EMsoft_toNativePath(infile)
else
  infile = trim(MPfile)
end if
inquire(file=trim(infile), exist=f_exists)

if (.not.f_exists) then
  call FatalError('readEBSDMasterPatternFile','Master Pattern input file does not exist')
end if

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(infile), stat, hdferr)

if (stat.eqv..FALSE.) then ! the file exists, so let's open it an first make sure it is an EBSD dot product file
   call FatalError('readEBSDMasterPatternFile','This is not a proper HDF5 file')
end if 
   
! open the Master Pattern file 
nullify(HDF_head%next)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

! check whether or not the MC file was generated using DREAM.3D
! this is necessary so that the proper reading of fixed length vs. variable length strings will occur.
! this test sets a flag in side the HDFsupport module so that the proper reading routines will be employed
if (dfMP.eqv..TRUE.) then 
  datagroupname = '/EMheader/EBSDdefectmaster'
else
  datagroupname = '/EMheader/EBSDmaster'
end if
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ComputeMasterPattern','This HDF file does not contain Master Pattern header data')
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
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..FALSE.) then
    call HDF_pop(HDF_head,.TRUE.)
    call FatalError('readEBSDMasterPatternFile','this is not an EBSD Master Pattern file')
end if
call HDF_pop(HDF_head)

!====================================
! check if this is an overlap EBSD pattern or a regular one  [ added by MDG, 06/19/19 ]; revised [10/18/19]
!====================================
dataset = 'READMEFIRST'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
! if the dataset exists, then this is an overlap EBSD pattern file 
EBSDMPdata%AveragedMP = .FALSE.
EBSDMPdata%newPGnumber = -1
if (g_exists.eqv..TRUE.) then
    EBSDMPdata%AveragedMP = .TRUE.
    call Message(' This master pattern file contains an averaged master pattern (generated by EMEBSDoverlap)')
end if

if (EBSDMPdata%AveragedMP.eqv..TRUE.) then
  ! read the new point group number from the newpgnum data set in the EBSDoverlapNameList NMLparameters group
  groupname = SC_NMLparameters
      hdferr = HDF_openGroup(groupname, HDF_head)
  groupname = 'EBSDoverlapNameList'
      hdferr = HDF_openGroup(groupname, HDF_head)

  dataset = 'newpgnum'
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if(g_exists) call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMPdata%newPGnumber)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)
end if

!====================================
! read all NMLparameters group datasets
!====================================
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDMasterNameList
    hdferr = HDF_openGroup(groupname, HDF_head)

! we'll read these roughly in the order that the HDFView program displays them...
dataset = SC_Esel
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if(g_exists) call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mpnl%Esel)

! we need to set the newPGnumber parameter to the correct value, to reflect the fact that 
! the symmetry of the overlap pattern will be different [ added by MDG, 06/20/19 ]
if (EBSDMPdata%AveragedMP.eqv..TRUE.) then 
  dataset = 'newpgnumber'
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if(g_exists) call HDF_readDatasetInteger(dataset, HDF_head, hdferr, EBSDMPdata%newPGnumber)
end if

dataset = SC_combinesites
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
mpnl%combinesites = .FALSE.
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, combinesites)
    if (combinesites.ne.0) mpnl%combinesites = .TRUE.
end if

dataset = SC_copyfromenergyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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
call H5Lexists_f(HDF_head%next%objectID, trim(dataset), g_exists, hdferr)
if(g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, restart)
    mpnl%restart = .FALSE.
    if (restart.ne.0) mpnl%restart = .TRUE.
end if

dataset = SC_stdout
call H5Lexists_f(HDF_head%next%objectID, trim(dataset), g_exists, hdferr)
if(g_exists) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, mpnl%stdout)
end if

dataset = SC_uniform
mpnl%uniform = .FALSE.
call H5Lexists_f(HDF_head%next%objectID, trim(dataset), g_exists, hdferr)
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
if (dfMP.eqv..TRUE.) then 
  groupname = SC_EBSDdefectmaster
else
  groupname = SC_EBSDmaster
end if
    hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_lastEnergy
call H5Lexists_f(HDF_head%next%objectID, trim(dataset), g_exists, hdferr)
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
    if (dfMP.eqv..TRUE.) then 
      call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, mLPNH3)
      allocate(EBSDMPdata%mLPNH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,dims3(3)),stat=istat)
      EBSDMPdata%mLPNH = mLPNH3
      deallocate(mLPNH3)
    else
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
end if

if (present(getmLPSH)) then 
  if (getmLPSH.eqv..TRUE.) then
    dataset = SC_mLPSH
    if (dfMP.eqv..TRUE.) then 
      call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, mLPNH3)
      allocate(EBSDMPdata%mLPSH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,dims3(3)),stat=istat)
      EBSDMPdata%mLPSH = mLPNH3
      deallocate(mLPNH3)
    else
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

if (present(verbose)) then
	if (verbose.eqv..TRUE.) then
		call Message(' -> completed reading master pattern data from '//trim(infile), frm = "(A/)")
	end if 
end if

end subroutine readEBSDMasterPatternFile


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
!f2py intent(in,out) ::  applynoise

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

deallocate(EBSDpattern)

end subroutine CalcEBSDPatternSingleFull

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcEBSDPatternDefect_zint
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single EBSD pattern for a defect containing column
!
!> @param ebsdnl EBSD namelist
!
!> @date 11/05/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcEBSDPatternDefect_zint(ipar,qu,mLPNH,mLPSH,rgx,rgy,rgz,binned, prefactor, Fmatrix)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcEBSDPatternDefect_zint

use local
use Lambert
use quaternions
use rotations

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                    :: ipar(7)
real(kind=sgl),INTENT(IN)                       :: qu(4,ipar(7)) 
real(kind=dbl),INTENT(IN)                       :: prefactor
real(kind=sgl),INTENT(IN)                       :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: rgx(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgy(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgz(ipar(2),ipar(3))
real(kind=sgl),INTENT(OUT)                      :: binned(ipar(2),ipar(3))
real(kind=dbl),INTENT(IN)                       :: Fmatrix(3,3,ipar(7))

real(kind=sgl),allocatable                      :: EBSDpattern(:,:)
real(kind=sgl)                                  :: dc(3),dcnew(3),ixy(2),scl
real(kind=sgl)                                  :: dx,dy,dxm,dym
integer(kind=irg)                               :: ii,jj,kk,istat
integer(kind=irg)                               :: nix,niy,nixp,niyp

! ipar(1) = not used 
! ipar(2) = ebsdnl%numsx
! ipar(3) = ebsdnl%numsy
! ipar(4) = ebsdnl%npx
! ipar(5) = ebsdnl%npy
! ipar(6) = not used 
! ipar(7) = number of depth steps

allocate(EBSDpattern(ipar(2),ipar(3)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(3)
! get the pixel direction cosines from the pre-computed array
        dc = (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /)
! here we loop over the depth instead of the energy, and we employ the deformation tensor at each depth 
! to determine the direction cosines of the sampling unit vector.        
        do kk = 1, ipar(7)
! apply the grain rotation 
          dc = quat_Lp( qu(1:4,kk), dc)
! apply the deformation
          dcnew = matmul(sngl(Fmatrix(1:3,1:3,kk)), dc)
! and normalize the direction cosines (to remove any rounding errors)
          dcnew = dcnew/sqrt(sum(dcnew**2))

! convert these direction cosines to interpolation coordinates in the Rosca-Lambert projection
          call LambertgetInterpolation(dcnew, scl, ipar(4), ipar(5), nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
          if (dcnew(3) .ge. 0.0) then
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + ( mLPNH(nix,niy,kk) * dxm * dym + &
                                               mLPNH(nixp,niy,kk) * dx * dym + mLPNH(nix,niyp,kk) * dxm * dy + &
                                               mLPNH(nixp,niyp,kk) * dx * dy )
          else
                EBSDpattern(ii,jj) = EBSDpattern(ii,jj) + ( mLPSH(nix,niy,kk) * dxm * dym + &
                                               mLPSH(nixp,niy,kk) * dx * dym + mLPSH(nix,niyp,kk) * dxm * dy + &
                                               mLPSH(nixp,niyp,kk) * dx * dy )
          end if
        end do 
    end do
end do

binned = prefactor * EBSDpattern

end subroutine CalcEBSDPatternDefect_zint


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcEBSDPatternDefect_noint
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief compute a column of EBSD patterns for a defect containing column
!
!> @param ebsdnl EBSD namelist
!
!> @date 11/12/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcEBSDPatternDefect_noint(ipar,qu,mLPNH,mLPSH,rgx,rgy,rgz,binned, prefactor, Fmatrix)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcEBSDPatternDefect_noint

use local
use Lambert
use quaternions
use rotations

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                    :: ipar(7)
real(kind=sgl),INTENT(IN)                       :: qu(4,ipar(7)) 
real(kind=dbl),INTENT(IN)                       :: prefactor
real(kind=sgl),INTENT(IN)                       :: mLPNH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: mLPSH(-ipar(4):ipar(4),-ipar(5):ipar(5),ipar(7))
real(kind=sgl),INTENT(IN)                       :: rgx(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgy(ipar(2),ipar(3))
real(kind=sgl),INTENT(IN)                       :: rgz(ipar(2),ipar(3))
real(kind=sgl),INTENT(OUT)                      :: binned(ipar(2),ipar(3),ipar(7))
real(kind=dbl),INTENT(IN)                       :: Fmatrix(3,3,ipar(7))

real(kind=sgl),allocatable                      :: EBSDpattern(:,:,:)
real(kind=sgl)                                  :: dc(3),dcnew(3),ixy(2),scl
real(kind=sgl)                                  :: dx,dy,dxm,dym
integer(kind=irg)                               :: ii,jj,kk,istat
integer(kind=irg)                               :: nix,niy,nixp,niyp

! ipar(1) = not used 
! ipar(2) = ebsdnl%numsx
! ipar(3) = ebsdnl%numsy
! ipar(4) = ebsdnl%npx
! ipar(5) = ebsdnl%npy
! ipar(6) = not used 
! ipar(7) = number of depth steps

allocate(EBSDpattern(ipar(2),ipar(3),ipar(7)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(3)
! get the pixel direction cosines from the pre-computed array
        dc = (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /)
! here we loop over the depth instead of the energy, and we employ the deformation tensor at each depth 
! to determine the direction cosines of the sampling unit vector.        
        do kk = 1, ipar(7)
! apply the grain rotation 
          dc = quat_Lp( qu(1:4, kk), dc)
! apply the deformation
          dcnew = matmul(sngl(Fmatrix(1:3,1:3,kk)), dc)
! and normalize the direction cosines (to remove any rounding errors)
          dcnew = dcnew/sqrt(sum(dcnew**2))

! convert these direction cosines to interpolation coordinates in the Rosca-Lambert projection
          call LambertgetInterpolation(dcnew, scl, ipar(4), ipar(5), nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
          if (dcnew(3) .ge. 0.0) then
                EBSDpattern(ii,jj,kk) = ( mLPNH(nix,niy,kk) * dxm * dym + &
                                          mLPNH(nixp,niy,kk) * dx * dym + mLPNH(nix,niyp,kk) * dxm * dy + &
                                          mLPNH(nixp,niyp,kk) * dx * dy )
          else
                EBSDpattern(ii,jj,kk) = ( mLPSH(nix,niy,kk) * dxm * dym + &
                                          mLPSH(nixp,niy,kk) * dx * dym + mLPSH(nix,niyp,kk) * dxm * dy + &
                                          mLPSH(nixp,niyp,kk) * dx * dy )
          end if
        end do 
    end do
end do

binned = prefactor * EBSDpattern

deallocate(EBSDpattern)

end subroutine CalcEBSDPatternDefect_noint

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
recursive subroutine EBSDcopyMCdata(inputfile, outputfile, h5)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDcopyMCdata

use local
use error
use HDFsupport
use io
use error

IMPLICIT NONE

character(fnlen),INTENT(IN)       :: inputfile
character(fnlen),INTENT(IN)       :: outputfile
character(fnlen),INTENT(IN)       :: h5

character(fnlen)                  :: infile, outfile, h5copypath, groupname
character(512)                    :: cmd, cmd2
logical                           :: f_exists, readonly, developer
type(HDFobjectStackType)          :: HDF_head
integer(kind=irg)                 :: hdferr

! first we make sure that we actually have the h5copy program available
! check for EMDevelop parameter 
developer = EMsoft_getEMdevelop()

if (developer.eqv..TRUE.) then 
! if TRUE, use EMsoft_geth5copypath which is defined at configure time 
  h5copypath = trim(EMsoft_geth5copypath())//' -p -v '
  h5copypath = EMsoft_toNativePath(h5copypath)
else 
! if FALSE, check name list h5copypath parameter 
  if (trim(h5).ne.'undefined') then 
    h5copypath = trim(h5)//' -p -v '
    h5copypath = EMsoft_toNativePath(h5copypath)
  else 
! if undefined, then fail
    call FatalError('EBSDcopyMCdata','h5copypath must be set in the name list file ')
  end if
end if

call Message(' Using '//trim(h5copypath)//' to copy Monte Carlo data to new file')

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
nullify(HDF_head%next)
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

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDcopyMPdata
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief copy Master Pattern data from one file to a new file using h5copy
!
!> @param inputfile name of file with MP data in it
!> @param outputfile name of new file
!
!> @date 06/18/19  MDG 1.0 original
!> @date 06/21/19  MDG 1.1 add option to skip copying of CrystalData group
!--------------------------------------------------------------------------
recursive subroutine EBSDcopyMPdata(inputfile, outputfile, h5, skipCrystalData)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDcopyMPdata

use local
use error
use HDFsupport
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)       :: inputfile
character(fnlen),INTENT(IN)       :: outputfile
character(fnlen),INTENT(IN)       :: h5
logical,INTENT(IN),OPTIONAL       :: skipCrystalData

character(fnlen)                  :: infile, outfile, h5copypath, groupname
character(512)                    :: cmd, cmd2
logical                           :: f_exists, readonly, developer
type(HDFobjectStackType)          :: HDF_head
integer(kind=irg)                 :: hdferr

! first we make sure that we actually have the h5copy program available
! check for EMDevelop parameter 
developer = EMsoft_getEMdevelop()

if (developer.eqv..TRUE.) then 
! if TRUE, use EMsoft_geth5copypath which is defined at configure time 
  h5copypath = trim(EMsoft_geth5copypath())//' -p -v '
  h5copypath = EMsoft_toNativePath(h5copypath)
else 
! if FALSE, check name list h5copypath parameter 
  if (trim(h5).ne.'undefined') then 
    h5copypath = trim(h5)//' -p -v '
    h5copypath = EMsoft_toNativePath(h5copypath)
  else 
! if undefined, then fail
    call FatalError('EBSDcopyMCdata','h5copypath must be set in the name list file ')
  end if
end if

call Message(' Using '//trim(h5copypath)//' to copy Master Pattern data to new file')

! first make sure that the input file exists and has MP data in it
infile = trim(EMsoft_getEMdatapathname())//trim(inputfile)
infile = EMsoft_toNativePath(infile)
inquire(file=infile, exist=f_exists)

outfile = trim(EMsoft_getEMdatapathname())//trim(outputfile)
outfile = EMsoft_toNativePath(outfile)

! if the file does not exist, abort the program with an error message
if (f_exists.eqv..FALSE.) then 
  call FatalError('EBSDcopyMPdata','Master Pattern file does not exist: '//trim(infile))
end if

! make sure it has EBSDmaster data in it; hdf open is done in the calling program
nullify(HDF_head%next)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.eq.-1) then 
  call FatalError('EBSDcopyMPdata','EMData group does not exist in '//trim(infile))
end if

groupname = SC_EBSDmaster
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.eq.-1) then 
  call FatalError('EBSDcopyMPdata','EBSDmaster group does not exist in '//trim(infile))
end if

call HDF_pop(HDF_head,.TRUE.)

! OK, if we get here, then the file does exist and it contains Master Pattern data, so we let
! the user know
call Message('--> Input file contains Master Pattern data')

! next, we copy the necessary groups into the new Master Pattern file
cmd = trim(h5copypath)//' -i "'//trim(infile)
cmd = trim(cmd)//'" -o "'//trim(outfile)

if (present(skipCrystalData)) then 
  if (skipCrystalData.eqv..FALSE.) then 
    cmd2 = trim(cmd)//'" -s "/CrystalData" -d "/CrystalData"'
    call system(trim(cmd2))
  end if
else
  cmd2 = trim(cmd)//'" -s "/CrystalData" -d "/CrystalData"'
  call system(trim(cmd2))
end if

cmd2 = trim(cmd)//'" -s "/EMData" -d "/EMData"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/EMheader" -d "/EMheader"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/NMLfiles" -d "/NMLfiles"'
call system(trim(cmd2))

cmd2 = trim(cmd)//'" -s "/NMLparameters" -d "/NMLparameters"'
call system(trim(cmd2))

call Message('--> Output file generated with Master Pattern data copied from '//trim(infile))

end subroutine EBSDcopyMPdata 


end module EBSDmod
