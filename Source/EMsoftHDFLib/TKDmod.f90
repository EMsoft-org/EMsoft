! ###################################################################
! Copyright (c) 2016-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:TKDmod.f90
!--------------------------------------------------------------------------
!
! MODULE: TKDmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMTKD helper routines
!
!> @date  05/09/17  MDG 1.0 original, based on EBSDmod.f90
!--------------------------------------------------------------------------
module TKDmod


use local
use typedefs

IMPLICIT NONE

type TKDAngleType
        real(kind=sgl),allocatable      :: quatang(:,:)
end type TKDAngleType

type TKDLargeAccumType
        integer(kind=irg),allocatable   :: accum_e(:,:,:),accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
end type TKDLargeAccumType

type TKDMasterType
        real(kind=sgl),allocatable      :: mLPNH(:,:,:) , mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)          ! auxiliary detector arrays needed for interpolation
end type TKDMasterType

type TKDPixel
        real(kind=sgl),allocatable      :: lambdaEZ(:,:)
        real(kind=dbl)                  :: dc(3) ! direction cosine in sample frame
        real(kind=dbl)                  :: cfactor
end type TKDPixel

type TKDFullDetector
        type(TKDPixel),allocatable     :: detector(:,:) 
end type TKDFullDetector
        
contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDreadangles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file
!
!> @param enl TKD name list structure
!> @param quatang array of unit quaternions (output)
!
!> @date 06/24/14  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine TKDreadangles(enl,angles,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDreadangles

use local
use typedefs
use NameListTypedefs
use io
use files
use quaternions
use rotations
use stringconstants

IMPLICIT NONE


type(TKDNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDAngleType),pointer              :: angles
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
if (atype.eq.'eu') then 
  enl%anglemode = 'euler'
else
  enl%anglemode = 'quats'
end if

! then the number of angles in the file
read(dataunit,*) enl%numangles

if (present(verbose)) then 
  io_int(1) = enl%numangles
  call WriteValue('Number of angle entries = ',io_int,1)
end if

if (enl%anglemode.eq.'euler') then
! allocate the euler angle array
  allocate(eulang(3,enl%numangles),stat=istat)
! if istat.ne.0 then do some error handling ... 
  do i=1,enl%numangles
    read(dataunit,*) eulang(1:3,i)
  end do
  close(unit=dataunit,status='keep')

  if (enl%eulerconvention.eq.'hkl') then
    if (present(verbose)) call Message('  -> converting Euler angles to TSL representation', frm = "(A)")
    eulang(1,1:enl%numangles) = eulang(1,1:enl%numangles) + 90.0
  end if

! convert the euler angle triplets to quaternions
  allocate(angles%quatang(4,enl%numangles),stat=istat)
! if (istat.ne.0) then ...

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A)")
  
  do i=1,enl%numangles
    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i)*dtor)
  end do

else
! the input file has quaternions, not Euler triplets
  allocate(angles%quatang(4,enl%numangles),stat=istat)
  do i=1,enl%numangles
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
  do i=1,enl%numangles
    angles%quatang(1:4,i) = quat_mult(qax,angles%quatang(1:4,i))
  end do 
end if

call Message(' -> completed reading Euler angles')

end subroutine TKDreadangles

!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDreadMCfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file
!
!> @param enl TKD name list structure
!> @param acc energy structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 11/18/14  MDG 1.1 removed enl%MCnthreads from file read
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 04/29/15  MDG 2.1 add optional parameter efile
!> @date 09/15/15  SS  2.2 added accum_z reading 
!> @date 08/18/16  MDG 2.3 modified HDF file format 
!--------------------------------------------------------------------------
recursive subroutine TKDreadMCfile(enl,acc,efile,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDreadMCfile

use local
use typedefs
use NameListTypedefs
use files
use io
use HDF5
use HDFsupport
use error
use stringconstants

IMPLICIT NONE

type(TKDNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDLargeAccumType),pointer         :: acc
character(fnlen),INTENT(IN),OPTIONAL    :: efile
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: istat, hdferr, nlines, nx
logical                                 :: stat, readonly, g_exists
integer(HSIZE_T)                        :: dims3(3),dims4(4)
character(fnlen)                        :: groupname, dataset, energyfile, datagroupname 
character(fnlen),allocatable            :: stringarray(:)

integer(kind=irg),allocatable           :: acc_e(:,:,:),acc_z(:,:,:,:)

type(HDFobjectStackType),pointer        :: HDF_head


! is the efile parameter present? If so, use it as the filename, otherwise use the enl%energyfile parameter
if (PRESENT(efile)) then
  energyfile = efile
else
  energyfile = trim(EMsoft_getEMdatapathname())//trim(enl%energyfile)
end if
energyfile = EMsoft_toNativePath(energyfile)

! first, we need to check whether or not the input file is of the HDF5 format type; if
! it is, we read it accordingly, otherwise we use the old binary format.
!
call h5fis_hdf5_f(energyfile, stat, hdferr)

if (stat) then
  nullify(HDF_head%next)

! open the MC file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLfoilNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCxtalname = trim(stringarray(1))
  deallocate(stringarray)
  
!  dataset = 'mode'
!  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!  enl%MCmode = trim(stringarray(1))
!  deallocate(stringarray)
!  if (enl%MCmode .ne. 'full') call FatalError('TKDreadMCfile','This file is not in full mode. Please input correct HDF5 file')
  enl%MCmode = 'full'

dataset = SC_numsx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nsx)
  enl%nsx = (enl%nsx - 1)/2
  enl%nsy = enl%nsx

dataset = SC_EkeV
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%EkeV)

dataset = SC_Ehistmin
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%Ehistmin)

dataset = SC_Ebinsize
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%Ebinsize)

dataset = SC_depthmax
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%depthmax)

dataset = SC_depthstep
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%depthstep)

dataset = SC_sig
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MCsig)

dataset = SC_omega
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MComega)

! close the name list group
  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

! read from the EMheader
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

! next we need to make sure that this file has Monte Carlo data in it...
  datagroupname = 'MCfoil'
  call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
  if (.not.g_exists) then
    call FatalError('TKDreadMCfile','This HDF file does not contain any Monte Carlo data')
  end if
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_ProgramName
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCprogname = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_Version
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCscversion = trim(stringarray(1))
  deallocate(stringarray)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

! open the Data group
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

! read data items 
dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numEbins)

dataset = SC_numzbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numzbins)

dataset = SC_accume
  call HDF_readDatasetIntegerArray3D(dataset, dims3, HDF_head, hdferr, acc_e)
  enl%num_el = sum(acc_e)
  nx = (dims3(2)-1)/2
  allocate(acc%accum_e(1:dims3(1),-nx:nx,-nx:nx))
  acc%accum_e = acc_e
  deallocate(acc_e)
  
dataset = SC_accumz
  call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
  allocate(acc%accum_z(1:dims4(1),1:dims4(2),1:dims4(3),1:dims4(4)))
  acc%accum_z = acc_z
  deallocate(acc_z)

! and close everything
  call HDF_pop(HDF_head,.TRUE.)
else
  call FatalError('TKDreadMCfile','MC file not found')
end if

if (present(verbose)) call Message(' -> completed reading Monte Carlo data from '//trim(enl%energyfile), frm = "(A)")

end subroutine TKDreadMCfile


!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDreadMasterfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read TKD master pattern from file
!
!> @param enl TKD name list structure
!> @param 
!
!> @date 06/24/14  MDG 1.0 original
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 09/01/15  MDG 3.0 changed Lambert maps to Northern + Southern maps; lots of changes...
!> @date 09/03/15  MDG 3.1 removed support for old file format (too difficult to maintain after above changes)
!> @date 08/18/16  MDG 3.2 modified HDF file format 
!--------------------------------------------------------------------------
recursive subroutine TKDreadMasterfile(enl, master, mfile, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDreadMasterfile

use local
use typedefs
use NameListTypedefs
use files
use io
use error
use HDF5
use HDFsupport
use stringconstants


IMPLICIT NONE

type(TKDNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDMasterType),pointer             :: master
character(fnlen),INTENT(IN),OPTIONAL    :: mfile
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: mLPNH(:,:,:) 
real(kind=sgl),allocatable              :: mLPSH(:,:,:) 
real(kind=sgl),allocatable              :: EkeVs(:) 
integer(kind=irg),allocatable           :: atomtype(:)

real(kind=sgl),allocatable              :: srtmp(:,:,:,:)
integer(kind=irg)                       :: istat

logical                                 :: stat, readonly, g_exists
integer(kind=irg)                       :: hdferr, nlines
integer(HSIZE_T)                        :: dims(1), dims4(4)
character(fnlen)                        :: groupname, dataset, masterfile, datagroupname
character(fnlen),allocatable            :: stringarray(:)

type(HDFobjectStackType),pointer        :: HDF_head

nullify(HDF_head%next)

! is the mfile parameter present? If so, use it as the filename, otherwise use the enl%masterfile parameter
if (PRESENT(mfile)) then
  masterfile = mfile
else
  masterfile = trim(EMsoft_getEMdatapathname())//trim(enl%masterfile)
end if
masterfile = EMsoft_toNativePath(masterfile)

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(masterfile), stat, hdferr)

if (stat) then 
! open the master file 
  readonly = .TRUE.
  hdferr =  HDF_openFile(masterfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDMasterNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_energyfile
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterenergyfile = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_npx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
  enl%npy = enl%npx

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

  datagroupname = 'TKDmaster'
  call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
  if (.not.g_exists) then
    call FatalError('TKDreadMasterfile','This HDF file does not contain any TKD master pattern data')
  end if
   hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)
! make sure that MC and Master results are compatible
  if ((enl%numEbins.ne.enl%nE).and.(.not.PRESENT(mfile))) then
    call Message('Energy histogram and Lambert stack have different energy dimension; aborting program', frm = "(A)")
    call HDF_pop(HDF_head,.TRUE.)
    stop
  end if

dataset = SC_numset
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numset)

! dataset = 'squhex'
! call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
! enl%sqorhe = trim(stringarray(1))
! deallocate(stringarray)

dataset = SC_mLPNH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPNH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_mLPSH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPSH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterxtalname = trim(stringarray(1))
  deallocate(stringarray)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_ProgramName
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterprogname = trim(stringarray(1))
  deallocate(stringarray)
  
dataset = SC_Version
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterscversion = trim(stringarray(1))
  deallocate(stringarray)
  
  call HDF_pop(HDF_head,.TRUE.)
else
  masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
  call FatalError('TKDreadMasterfile',masterfile)
end if
!====================================

if (present(verbose)) call Message(' -> completed reading master pattern data from '//trim(enl%masterfile), frm = "(A)")

end subroutine TKDreadMasterfile


!--------------------------------------------------------------------------
!
! SUBROUTINE: TKDreadMasterfile_overlap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read TKD master pattern from file
!
!> @param enl TKDoverlap name list structure
!> @param 
!
!> @date 01/03/18  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine TKDreadMasterfile_overlap(enl, master, mfile, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDreadMasterfile_overlap

use local
use typedefs
use NameListTypedefs
use files
use io
use error
use HDF5
use HDFsupport


IMPLICIT NONE

type(TKDoverlapNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
type(TKDMasterType),pointer             :: master
character(fnlen),INTENT(IN),OPTIONAL    :: mfile
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: sr(:,:,:) 
real(kind=sgl),allocatable              :: EkeVs(:) 
integer(kind=irg),allocatable           :: atomtype(:)

real(kind=sgl),allocatable              :: srtmp(:,:,:,:)
integer(kind=irg)                       :: istat

logical                                 :: stat, readonly
integer(kind=irg)                       :: hdferr, nlines
integer(HSIZE_T)                        :: dims(1), dims4(4)
character(fnlen)                        :: groupname, dataset, masterfile
character(fnlen),allocatable            :: stringarray(:)

type(HDFobjectStackType),pointer        :: HDF_head

nullify(HDF_head, HDF_head)

! is the mfile parameter present? If so, use it as the filename, otherwise use the enl%masterfile parameter
if (PRESENT(mfile)) then
  masterfile = trim(EMsoft_getEMdatapathname())//trim(mfile)
else
  masterfile = trim(EMsoft_getEMdatapathname())//trim(enl%masterfile)
end if 
masterfile = EMsoft_toNativePath(masterfile)

! first, we need to check whether or not the input file is of the HDF5 format type
call h5fis_hdf5_f(trim(masterfile), stat, hdferr)

if (stat) then 
! open the master file 
  readonly = .TRUE.
  hdferr =  HDF_openFile(masterfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDMasterNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_energyfile
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterenergyfile = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_npx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
  enl%npy = enl%npx

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDMaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)

dataset = SC_numset
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numset)

dataset = SC_mLPNH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPNH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_mLPSH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPSH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterxtalname = trim(stringarray(1))
  deallocate(stringarray)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDMaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_ProgramName
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterprogname = trim(stringarray(1))
  deallocate(stringarray)
  
dataset = SC_Version
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterscversion = trim(stringarray(1))
  deallocate(stringarray)
  
  call HDF_pop(HDF_head,.TRUE.)
else
  masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
  call FatalError('TKDreadMasterfile_overlap',masterfile)
end if
!====================================

if (present(verbose)) then
  if (verbose) call Message(' -> completed reading '//trim(masterfile), frm = "(A)")
end if

end subroutine TKDreadMasterfile_overlap





!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl TKD name list structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS 1.1 added omega as the second tilt angle
!> @date 07/07/15   SS 1.2 correction to the omega tilt parameter; old version in the comments
!> @date 02/19/19  MDG 2.0 corrects pattern orientation (manual indexing revealed an unwanted upside down flip)
!--------------------------------------------------------------------------
recursive subroutine TKDGenerateDetector(enl, acc, master, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert
use stringconstants

IMPLICIT NONE

type(TKDNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDLargeAccumType),pointer         :: acc
type(TKDMasterType),pointer             :: master
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:), testarray(:,:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable              :: z(:,:)           
integer(kind=irg)                       :: nix, niy, nixp, niyp, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, epl     ! various parameters
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
alp = 0.5 * cPi - (enl%MCsig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
epl = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy
!  rhos = 1.0/sqrt(sx + scin_y(i)**2)
   master%rgx(j,epl-i) = (scin_y(i) * ca + sa * Ls) 
   master%rgy(j,epl-i) = Lc 
   master%rgz(j,epl-i) = (-sa * scin_y(i) + ca * Ls) 
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(master%rgx*master%rgx+master%rgy*master%rgy+master%rgz*master%rgz)
  master%rgx = master%rgx*z
  master%rgy = master%rgy*z
  master%rgz = master%rgz*z
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

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(enl%nsx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.enl%numEbins)  Emin=enl%numEbins

  Emax = nint((enl%energymax - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.enl%numEbins)  Emax=enl%numEbins

! correction of change in effective pixel area compared to equal-area Lambert projection

! this needs to be verified after the pattern flip modification [MDG, 02/19/2019]
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
  ipx = enl%numsx/2 + nint(enl%xpc)
  ipy = enl%numsy/2 + nint(enl%ypc)
  if ((abs(ipy).gt.enl%numsy).or.(abs(ipx).gt.enl%numsx)) then 
    pcvec = (/enl%ypc*enl%delta*ca + enl%xpc*enl%delta*sa*sw + enl%L*cw*sa, &
             enl%L*sw - enl%xpc*enl%delta*cw,&
             enl%L*ca*cw + enl%xpc*enl%delta*ca*sw - enl%ypc*enl%delta*sa/)
    pcvec = pcvec/NORM2(pcvec)
  else
    pcvec = (/ master%rgx(ipx,ipy), master%rgy(ipx,ipy), master%rgz(ipx,ipy) /)
  end if

  calpha = cos(alpha)
  do i=1,enl%numsx
    do j=1,enl%numsy
! do the coordinate transformation for this detector pixel
       dc = (/ master%rgx(i,j),master%rgy(i,j),master%rgz(i,j) /)

! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, enl%nsx, enl%nsy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)

! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          g = 0.25 
        else
          g = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3) * 0.25
        end if

! interpolate the intensity 
        do k=Emin,Emax 
          s = acc%accum_e(k,nix,niy) * dxm * dym + &
              acc%accum_e(k,nixp,niy) * dx * dym + &
              acc%accum_e(k,nix,niyp) * dxm * dy + &
              acc%accum_e(k,nixp,niyp) * dx * dy
          acc%accum_e_detector(k,i,epl-j) = g * s
        end do
    end do
  end do 


! and finally, get rid of the original accum_e array which is no longer needed
! [we'll do that in the calling program ]
!  deallocate(accum_e)

if (present(verbose)) call Message(' -> completed detector generation', frm = "(A)")

!====================================
end subroutine TKDGenerateDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE:GenerateTKDBackground
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Generate a binned and normalized background for the dictionary patterns using the monte carlo simulation
!
!> @param enl TKD name list structure
!> @param master  TKDMasterType pointer
!> @param q unit quaternion providing the necessary rotation
!
!> @date 04/20/15 MDG 1.0 original, based on Saransh's twin routine above
!--------------------------------------------------------------------------
recursive subroutine GenerateTKDBackground(enl,acc,TKDBackground)
!DEC$ ATTRIBUTES DLLEXPORT :: GenerateTKDBackground

use local
use typedefs 
use NameListTypedefs
use stringconstants

type(TKDNameListType),INTENT(IN)        :: enl
type(TKDLargeAccumType),pointer         :: acc
real(kind=sgl),INTENT(OUT)              :: TKDBackground(enl%numsx/enl%binning,enl%numsy/enl%binning)

integer(kind=irg)                       :: ii, jj, kk, istat
real(kind=sgl),allocatable              :: TKDtmp(:,:)
integer(kind=irg)                       :: Emin, Emax
real(kind=sgl)                          :: bindx


allocate(TKDtmp(enl%numsx,enl%numsy),stat=istat)
TKDtmp = 0.0
TKDBackground = 0.0
! get the indices of the minimum and maximum energy
Emin = nint((enl%energymin - enl%Ehistmin)/enl%Ebinsize) +1
if (Emin.lt.1)  Emin=1
if (Emin.gt.enl%numEbins)  Emin=enl%numEbins

Emax = nint((enl%energymax - enl%Ehistmin)/enl%Ebinsize) +1
if (Emax.lt.1)  Emax=1
if (Emax.gt.enl%numEbins)  Emax=enl%numEbins

bindx = 1.0/float(enl%binning)**2

do ii = 1,enl%numsx
   do jj = 1,enl%numsy
      do kk = Emin,Emax
         TKDtmp(ii,jj) = TKDtmp(ii,jj) + acc%accum_e_detector(kk,ii,jj) 
      end do
   end do
end do

if(enl%binning .ne. 1) then
  do ii=1,enl%numsx/enl%binning
      do jj=1,enl%numsy/enl%binning
           TKDBackground(ii,jj) = sum(TKDtmp((ii-1)*enl%binning+1:ii*enl%binning,(jj-1)*enl%binning:jj*enl%binning))
           if(isnan(TKDBackground(ii,jj))) then
               stop 'Background pattern encountered NaN during binning'
           end if
      end do
  end do  
! and divide by binning^2
  TKDBackground = TKDBackground * bindx
else
   TKDBackground = TKDtmp
end if

! apply gamma scaling
TKDBackground = TKDBackground**enl%gammavalue

! normalize the pattern
TKDBackground = TKDBackground/NORM2(TKDBackground)

end subroutine GenerateTKDBackground


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcTKDPatternSingleFull
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single TKD pattern, used in many programs
!
!> @param ebsdnl TKD namelist
!> @param holdexpt logical
!> @param holddict logical
!
!> @date 03/17/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcTKDPatternSingleFull(ipar,qu,accum,mLPNH,mLPSH,rgx,rgy,rgz,binned,Emin,Emax,mask,prefactor)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcTKDPatternSingleFull

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
use stringconstants

IMPLICIT NONE

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

real(kind=sgl),allocatable                      :: TKDpattern(:,:)
real(kind=sgl),allocatable                      :: wf(:)
real(kind=sgl)                                  :: dc(3),ixy(2),scl,bindx
real(kind=sgl)                                  :: dx,dy,dxm,dym
integer(kind=irg)                               :: ii,jj,kk,istat
integer(kind=irg)                               :: nix,niy,nixp,niyp

! ipar(1) = ebsdnl%binning
! ipar(2) = ebsdnl%numsx
! ipar(3) = ebsdnl%numsy
! ipar(4) = ebsdnl%npx
! ipar(5) = ebsdnl%npy
! ipar(6) = ebsdnl%numEbins
! ipar(7) = ebsdnl%nE


bindx = 1.0/float(ipar(1))**2

allocate(TKDpattern(ipar(2),ipar(3)),stat=istat)
binned = 0.0
TKDpattern = 0.0
scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(3)

        dc = sngl(quat_Lp(qu(1:4),  (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /) ))

        dc = dc/sqrt(sum(dc**2))

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, ipar(4), ipar(5), nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
        if (dc(3) .ge. 0.0) then
            do kk = Emin, Emax
                TKDpattern(ii,jj) = TKDpattern(ii,jj) + accum(kk,ii,jj) * ( mLPNH(nix,niy,kk) * dxm * dym + &
                                               mLPNH(nixp,niy,kk) * dx * dym + mLPNH(nix,niyp,kk) * dxm * dy + &
                                               mLPNH(nixp,niyp,kk) * dx * dy )
            end do
        else
            do kk = Emin, Emax
                TKDpattern(ii,jj) = TKDpattern(ii,jj) + accum(kk,ii,jj) * ( mLPSH(nix,niy,kk) * dxm * dym + &
                                               mLPSH(nixp,niy,kk) * dx * dym + mLPSH(nix,niyp,kk) * dxm * dy + &
                                               mLPSH(nixp,niyp,kk) * dx * dy )
            end do

        end if
    end do
end do

TKDpattern = prefactor * TKDpattern

if (ipar(1) .ne. 1) then
    do ii=1,ipar(2),ipar(1)
        do jj=1,ipar(3),ipar(1)
            binned(ii/ipar(1)+1,jj/ipar(1)+1) = &
            sum(TKDpattern(ii:ii+ipar(1)-1,jj:jj+ipar(1)-1))
        end do
    end do
! and divide by binning^2

    binned = binned * bindx
else
    binned = TKDpattern
end if

binned = binned * mask

end subroutine CalcTKDPatternSingleFull

!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDFullGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl TKD name list structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS  1.1 added omega as the second tilt angle
!> @date 07/07/15   SS  1.2 correction to the omega tilt parameter; old version in the comments
!--------------------------------------------------------------------------
recursive subroutine TKDFullGenerateDetector(enl, scintillator, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDFullGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert
use error
use stringconstants

IMPLICIT NONE

type(TKDNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDFullDetector),pointer           :: scintillator
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
integer(kind=irg)                       :: i, j, Emin, Emax, istat, k, ipx, ipy, ierr, epl   
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
alp = 0.5 * cPi - (enl%MCsig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
epl = enl%numsy+1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy

   rhos = 1.0/sqrt(sx + scin_y(i)**2)

   allocate(scintillator%detector(j,epl-i)%lambdaEZ(1:enl%numEbins,1:enl%numzbins))

   scintillator%detector(j,epl-i)%lambdaEZ = 0.D0

   scintillator%detector(j,epl-i)%dc = (/(scin_y(i) * ca + sa * Ls) * rhos, Lc * rhos,&
                                    (-sa * scin_y(i) + ca * Ls) * rhos/)

   scintillator%detector(j,epl-i)%dc = scintillator%detector(j,i)%dc/NORM2(scintillator%detector(j,epl-i)%dc)

!  if (ierr .ne. 0) then
!      call FatalError('TKDFullGenerateDetector:','Lambert Projection coordinate undefined')
!  end if

  end do
end do
deallocate(scin_x, scin_y)

alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
ipx = nint(enl%numsx/2 + enl%xpc)
ipy = nint(enl%numsy/2 + enl%ypc)
! the following line needs to be modified to allow for the pattern center to fall outside 
! of the detector surface area ...
pcvec = scintillator%detector(ipx,ipy)%dc
calpha = cos(alpha)

do i = 1,enl%numsx
    do j = 1,enl%numsy

        dc = scintillator%detector(i,j)%dc 
        dp = DOT_PRODUCT(pcvec,dc)
        theta = acos(dp)

        if ((i.eq.ipx).and.(j.eq.ipy)) then
          scintillator%detector(i,j)%cfactor = 0.25 
        else
          scintillator%detector(i,j)%cfactor = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)
        end if

    end do
end do

if (present(verbose)) call Message(' -> completed detector generation', frm = "(A)")

!====================================
end subroutine TKDFullGenerateDetector

end module TKDmod
