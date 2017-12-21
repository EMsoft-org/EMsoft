! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
!--------------------------------------------------------------------------
module EBSDmod


use local
use typedefs
use stringconstants

IMPLICIT NONE

type EBSDAngleType
        real(kind=sgl),allocatable      :: quatang(:,:)
end type EBSDAngleType

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

type EBSDFullDetector
        type(EBSDPixel),allocatable     :: detector(:,:) 
end type EBSDFullDetector
        
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
recursive subroutine EBSDreadangles(enl,angles,verbose)
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
if (angletype.eq.'eu') then 
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
    if (present(verbose)) call Message('  -> converting Euler angles to TSL representation', frm = "(A/)")
    eulang(1,1:enl%numangles) = eulang(1,1:enl%numangles) + 90.0
  end if

! convert the euler angle triplets to quaternions
  allocate(angles%quatang(4,enl%numangles),stat=istat)
! if (istat.ne.0) then ...

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A/)")
  
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

write (*,*) 'completed reading Euler angles'

end subroutine EBSDreadangles

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadMCfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file
!
!> @param enl EBSD name list structure
!> @param acc energy structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 11/18/14  MDG 1.1 removed enl%MCnthreads from file read
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 04/29/15  MDG 2.1 add optional parameter efile
!> @date 09/15/15  SS  2.2 added accum_z reading 
!> @date 08/18/16  MDG 2.3 modified HDF file format 
!--------------------------------------------------------------------------
recursive subroutine EBSDreadMCfile(enl,acc,efile,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadMCfile

use local
use typedefs
use NameListTypedefs
use files
use io
use HDF5
use HDFsupport
use error

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(EBSDLargeAccumType),pointer        :: acc
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
! open the fortran HDF interface
  call h5open_EMsoft(hdferr)

  nullify(HDF_head)

! open the MC file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCxtalname = trim(stringarray(1))
  deallocate(stringarray)
  
dataset = SC_mode
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCmode = trim(stringarray(1))
  deallocate(stringarray)
  
  if (enl%MCmode .ne. 'full') call FatalError('EBSDreadMCfile','This file is not in full mode. Please input correct HDF5 file')
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
  datagroupname = 'MCOpenCL'
  call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
  if (.not.g_exists) then
    call Message('This file does not appear to contain any Monte Carlo data or the file')
    call Message('has the old data format; please use the EMmergeEBSD script to update')
    call Message('the Monte Carlo data file to the correct format.  You will need to use')
    call Message('the -C option to perform this update; consult the main EMsoft manual pages.')
    call FatalError('EBSDreadMCfile','This HDF file does not contain any Monte Carlo data')
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

! close the fortran HDF interface
  call h5close_EMsoft(hdferr)

else
  call FatalError('EBSDreadMCfile','MC file not found')
end if

if (present(verbose)) call Message(' -> completed reading Monte Carlo data from '//trim(enl%energyfile), frm = "(A)")

end subroutine EBSDreadMCfile


!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadMasterfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read EBSD master pattern from file
!
!> @param enl EBSD name list structure
!> @param 
!
!> @date 06/24/14  MDG 1.0 original
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 09/01/15  MDG 3.0 changed Lambert maps to Northern + Southern maps; lots of changes...
!> @date 09/03/15  MDG 3.1 removed support for old file format (too difficult to maintain after above changes)
!> @date 08/18/16  MDG 3.2 modified HDF file format 
!--------------------------------------------------------------------------
recursive subroutine EBSDreadMasterfile(enl, master, mfile, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadMasterfile

use local
use typedefs
use NameListTypedefs
use files
use io
use error
use HDF5
use HDFsupport


IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(EBSDMasterType),pointer            :: master
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

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

nullify(HDF_head)

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

groupname = SC_EBSDMasterNameList
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

  datagroupname = 'EBSDmaster'
  call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
  if (.not.g_exists) then
    call Message('This file does not appear to contain any EBSD master data or the file')
    call Message('has the old data format; please use the EMmergeEBSD script to update')
    call Message('the master pattern data file to the correct format.  You will need to use')
    call Message('the -M option to perform this update; consult the main EMsoft manual pages.')
    call FatalError('EBSDreadMasterfile','This HDF file does not contain any Monte Carlo data')
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

! close the fortran HDF interface
  call h5close_EMsoft(hdferr)

else
  masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
  call FatalError('EBSDreadMasterfile',masterfile)
end if
!====================================

if (present(verbose)) call Message(' -> completed reading master pattern data from '//trim(enl%masterfile), frm = "(A)")

end subroutine EBSDreadMasterfile

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadMasterfile_overlap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read EBSD master pattern from file
!
!> @param enl EBSDoverlap name list structure
!> @param 
!
!> @date 06/24/14  MDG 1.0 original
!> @date 04/02/15  MDG 2.0 changed program input & output to HDF format
!> @date 09/03/15  MDG 2.1 removed old file format support
!--------------------------------------------------------------------------
recursive subroutine EBSDreadMasterfile_overlap(enl, master, mfile, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadMasterfile_overlap

use local
use typedefs
use NameListTypedefs
use files
use io
use error
use HDF5
use HDFsupport


IMPLICIT NONE

type(EBSDoverlapNameListType),INTENT(INOUT)    :: enl
type(EBSDMasterType),pointer            :: master
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

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

nullify(HDF_head, HDF_head)

! is the mfile parameter present? If so, use it as the filename, otherwise use the enl%masterfile parameter
if (PRESENT(mfile)) then
  masterfile = trim(EMsoft_getEMdatapathname())//trim(mfile)
else
  masterfile = trim(EMsoft_getEMdatapathname())//trim(enl%masterfile)
end if 
masterfile = EMsoft_toNativePath(masterfile)

! first, we need to check whether or not the input file is of the HDF5 forat type
call h5fis_hdf5_f(trim(masterfile), stat, hdferr)

if (stat) then 
! open the master file 
  readonly = .TRUE.
  hdferr =  HDF_openFile(masterfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_EBSDMasterNameList
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

dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)

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

groupname = SC_EMheader
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

! close the fortran HDF interface
  call h5close_EMsoft(hdferr)

else
  masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
  call FatalError('EBSDreadMasterfile_overlap',masterfile)
end if
!====================================

if (present(verbose)) then
  if (verbose) call Message(' -> completed reading '//trim(masterfile), frm = "(A)")
end if

end subroutine EBSDreadMasterfile_overlap




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
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS  1.1 added omega as the second tilt angle
!> @date 07/07/15   SS  1.2 correction to the omega tilt parameter; old version in the comments

!--------------------------------------------------------------------------
recursive subroutine EBSDGenerateDetector(enl, acc, master, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(EBSDLargeAccumType),pointer        :: acc
type(EBSDMasterType),pointer            :: master
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:), testarray(:,:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
real(kind=sgl),allocatable              :: z(:,:)           
integer(kind=irg)                       :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy     ! various parameters
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

L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   master%rgx(j,i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   master%rgy(j,i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   master%rgz(j,i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
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
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
  ipx = enl%numsx/2 + nint(enl%xpc)
  ipy = enl%numsy/2 + nint(enl%ypc)
  pcvec = (/ master%rgx(ipx,ipy), master%rgy(ipx,ipy), master%rgz(ipx,ipy) /)
  calpha = cos(alpha)
  do i=1,enl%numsx
    do j=1,enl%numsy
! do the coordinate transformation for this detector pixel
       dc = (/ master%rgx(i,j),master%rgy(i,j),master%rgz(i,j) /)
! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc
! convert these direction cosines to coordinates in the Rosca-Lambert projection
        ixy = scl * LambertSphereToSquare( dc, istat )
        x = ixy(1)
        ixy(1) = ixy(2)
        ixy(2) = -x
! four-point interpolation (bi-quadratic)
        nix = int(enl%nsx+ixy(1))-enl%nsx
        niy = int(enl%nsy+ixy(2))-enl%nsy
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
          !g = 2.0 * tan(alpha) * dp / ( tan(theta+alpha) - tan(theta-alpha) ) * 0.25
          g = ((calpha*calpha + dp*dp - 1.0)**1.5)/(calpha**3)

        end if
! interpolate the intensity 
        do k=Emin,Emax 
          s = acc%accum_e(k,nix,niy) * dxm * dym + &
              acc%accum_e(k,nix+1,niy) * dx * dym + &
              acc%accum_e(k,nix,niy+1) * dxm * dy + &
              acc%accum_e(k,nix+1,niy+1) * dx * dy
          acc%accum_e_detector(k,i,j) = g * s
        end do
    end do
  end do 


! and finally, get rid of the original accum_e array which is no longer needed
! [we'll do that in the calling program ]
!  deallocate(accum_e)

if (present(verbose)) call Message(' -> completed detector generation', frm = "(A)")

!====================================
end subroutine EBSDGenerateDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE:TwinCubicMasterPattern
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Generate a master pattern with regular and twin master pattern overlapped, both with 50% weights
!
!> @param enl EBSD name list structure
!> @param master  EBSDMasterType pointer
!
!> @date 04/16/15  SS 1.0 original
!> @date 04/20/15 MDG 1.1 minor edits
!> @date 09/03/15 MDG 1.2 added support for Northern and Southern Lambert hemispheres
!--------------------------------------------------------------------------
recursive subroutine TwinCubicMasterPattern(enl,master)
!DEC$ ATTRIBUTES DLLEXPORT :: TwinCubicMasterPattern

use local
use typedefs
use io
use quaternions
use Lambert
use rotations
use NameListTypedefs
use NameListHandlers
use constants

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)                :: enl
type(EBSDMasterType),pointer                        :: master

real(kind=dbl),allocatable                          :: master_twinNH(:,:,:), master_twinSH(:,:,:)
type(EBSDLargeAccumType),pointer                    :: acc
logical                                             :: verbose
real(kind=dbl)                                      :: q(4),Lamproj(2),dc(3),dc_new(3),dx,dy,dxm,dym,ixy(2),scl
integer(kind=irg)                                   :: nix,niy,nixp,niyp
integer(kind=irg)                                   :: ii,jj,kk,ierr,istat,pp,qq

allocate(master_twinNH(-enl%npx:enl%npx,-enl%npy:enl%npy,1:enl%nE),stat=istat)
allocate(master_twinSH(-enl%npx:enl%npx,-enl%npy:enl%npy,1:enl%nE),stat=istat)

q = (/ dsqrt(3.D0)/2.D0,1/dsqrt(3.D0)/2.D0,1/dsqrt(3.D0)/2.D0,1/dsqrt(3.D0)/2.D0 /)

scl = float(enl%npx) ! / LPs%sPio2 [removed 09/01/15 by MDG for new Lambert module]

    master_twinNH = 0.0
    master_twinSH = 0.0
    do jj = -enl%npx,enl%npx
        do kk = -enl%npy,enl%npy

            Lamproj = (/ float(jj)/scl,float(kk)/scl /)
            dc = LambertSquareToSphere(Lamproj,ierr)
            dc_new = quat_Lp(conjg(q),dc)
            dc_new = dc_new/sqrt(sum(dc_new**2))
            if (dc_new(3) .lt. 0.0) dc_new = -dc_new

! convert direction cosines to lambert projections
            ixy = scl * LambertSphereToSquare( dc_new, istat )
! interpolate intensity from the neighboring points

            nix = floor(ixy(1))
            niy = floor(ixy(2))
            nixp = nix+1
            niyp = niy+1
            if (nixp.gt.enl%npx) nixp = nix
            if (niyp.gt.enl%npy) niyp = niy
            dx = ixy(1) - nix
            dy = ixy(2) - niy
            dxm = 1.0 - dx
            dym = 1.0 - dy

            master_twinNH(jj,kk,1:enl%nE) = master%mLPNH(nix,niy,1:enl%nE)*dxm*dym + master%mLPNH(nixp,niy,1:enl%nE)*dx*dym + &
                                    master%mLPNH(nix,niyp,1:enl%nE)*dxm*dy + master%mLPNH(nixp,niyp,1:enl%nE)*dx*dy
            master_twinSH(jj,kk,1:enl%nE) = master%mLPSH(nix,niy,1:enl%nE)*dxm*dym + master%mLPSH(nixp,niy,1:enl%nE)*dx*dym + &
                                    master%mLPSH(nix,niyp,1:enl%nE)*dxm*dy + master%mLPSH(nixp,niyp,1:enl%nE)*dx*dy
        end do
    end do
master%mLPNH = 0.5D0 * (master_twinNH + master%mLPNH)
master%mLPSH = 0.5D0 * (master_twinSH + master%mLPSH)

call Message(' -> completed superimposing twin and regular master patterns', frm = "(A)")

end subroutine TwinCubicMasterPattern

!--------------------------------------------------------------------------
!
! SUBROUTINE:OverlapMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate a master pattern with regular and rotated master pattern overlapped, both with 50% weights
!
!> @param enl EBSD name list structure
!> @param master  EBSDMasterType pointer
!> @param q unit quaternion providing the necessary rotation
!
!> @date 04/20/15 MDG 1.0 original, based on Saransh's twin routine above
!> @date 09/03/15 MDG 1.2 added support for Northern and Southern Lambert hemispheres
!> @date 10/16/15  SS 1.3 added alpha parameter for degree of mixing; 0<=alpha<=1; also
!> added master_inp and master_out variables in the subroutine for NtAg dataset
!--------------------------------------------------------------------------
recursive subroutine OverlapMasterPattern(enl,master_in,master_out,q,alpha)
!DEC$ ATTRIBUTES DLLEXPORT :: OverlapMasterPattern

use local
use typedefs
use io
use error
use quaternions
use Lambert
use rotations
use NameListTypedefs
use NameListHandlers
use constants

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)                :: enl
type(EBSDMasterType),pointer                        :: master_in, master_out
real(kind=dbl),INTENT(IN)                           :: q(4)
real(kind=dbl),INTENT(IN)                           :: alpha

real(kind=dbl),allocatable                          :: master_rotatedNH(:,:,:), master_rotatedSH(:,:,:)
type(EBSDLargeAccumType),pointer                    :: acc
logical                                             :: verbose
real(kind=dbl)                                      :: Lamproj(2),dc(3),dc_new(3),dx,dy,dxm,dym,ixy(2),scl
integer(kind=irg)                                   :: nix,niy,nixp,niyp
integer(kind=irg)                                   :: ii,jj,kk,ierr,istat,pp,qq

if (alpha .lt. 0.D0) then
    call FatalError('OverlapMasterPattern','value of mixing paramter is less than zero')
end if

if (alpha .gt. 1.D0) then
    call FatalError('OverlapMasterPattern','value of mixing paramter is greater than one')
end if

allocate(master_rotatedNH(-enl%npx:enl%npx,-enl%npy:enl%npy,1:enl%nE),stat=istat)
allocate(master_rotatedSH(-enl%npx:enl%npx,-enl%npy:enl%npy,1:enl%nE),stat=istat)

if (allocated(master_out%mLPNH)) deallocate(master_out%mLPNH)
if (allocated(master_out%mLPSH)) deallocate(master_out%mLPSH)

allocate(master_out%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,1:enl%nE),stat=istat)
allocate(master_out%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,1:enl%nE),stat=istat)

master_out%mLPNH = 0.0
master_out%mLPSH = 0.0

scl = float(enl%npx) ! / LPs%sPio2 [ removed on 09/01/15 by MDG for new Lambert module]

master_rotatedNH = 0.0
master_rotatedSH = 0.0
do jj = -enl%npx,enl%npx
    do kk = -enl%npy,enl%npy

        Lamproj = (/ float(jj)/scl,float(kk)/scl /)
        dc = LambertSquareToSphere(Lamproj,ierr)
        dc_new = quat_Lp(conjg(q), dc)
        dc_new = dc_new/sqrt(sum(dc_new**2))
        if (dc_new(3) .lt. 0.0) dc_new = -dc_new

! convert direction cosines to lambert projections
        ixy = scl * LambertSphereToSquare( dc_new, istat )

! interpolate intensity from the neighboring points
        nix = floor(ixy(1))
        niy = floor(ixy(2))
        nixp = nix+1
        niyp = niy+1
        if (nixp.gt.enl%npx) nixp = nix
        if (niyp.gt.enl%npy) niyp = niy
        dx = ixy(1) - nix
        dy = ixy(2) - niy
        dxm = 1.0 - dx
        dym = 1.0 - dy

        master_rotatedNH(jj,kk,1:enl%nE) = master_in%mLPNH(nix,niy,1:enl%nE)*dxm*dym + master_in%mLPNH(nixp,niy,1:enl%nE)&
                                    *dx*dym + master_in%mLPNH(nix,niyp,1:enl%nE)*dxm*dy + master_in%mLPNH(nixp,niyp,1:enl%nE)&
                                    *dx*dy
        master_rotatedSH(jj,kk,1:enl%nE) = master_in%mLPSH(nix,niy,1:enl%nE)*dxm*dym + master_in%mLPSH(nixp,niy,1:enl%nE)&
                                    *dx*dym + master_in%mLPSH(nix,niyp,1:enl%nE)*dxm*dy + master_in%mLPSH(nixp,niyp,1:enl%nE)*dx*dy
    end do
end do

master_out%mLPNH = (1 - alpha) * master_rotatedNH + alpha * master_in%mLPNH
master_out%mLPSH = (1 - alpha) * master_rotatedSH + alpha * master_in%mLPSH

call Message(' -> completed superimposing rotated and regular master patterns', frm = "(A)")

end subroutine OverlapMasterPattern

!--------------------------------------------------------------------------
!
! SUBROUTINE:GenerateBackground
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Generate a binned and normalized background for the dictionary patterns using the monte carlo simulation
!
!> @param enl EBSD name list structure
!> @param master  EBSDMasterType pointer
!> @param q unit quaternion providing the necessary rotation
!
!> @date 04/20/15 MDG 1.0 original, based on Saransh's twin routine above
!--------------------------------------------------------------------------
recursive subroutine GenerateBackground(enl,acc,EBSDBackground)
!DEC$ ATTRIBUTES DLLEXPORT :: GenerateBackground

use local
use typedefs 
use NameListTypedefs

type(EBSDNameListType),INTENT(IN)       :: enl
type(EBSDLargeAccumType),pointer        :: acc
real(kind=sgl),INTENT(OUT)              :: EBSDBackground(enl%numsx/enl%binning,enl%numsy/enl%binning)

integer(kind=irg)                       :: ii, jj, kk, istat
real(kind=sgl),allocatable              :: EBSDtmp(:,:)
integer(kind=irg)                       :: Emin, Emax
real(kind=sgl)                          :: bindx


allocate(EBSDtmp(enl%numsx,enl%numsy),stat=istat)
EBSDtmp = 0.0
EBSDBackground = 0.0
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
         EBSDtmp(ii,jj) = EBSDtmp(ii,jj) + acc%accum_e_detector(kk,ii,jj) 
      end do
   end do
end do

if(enl%binning .ne. 1) then
  do ii=1,enl%numsx/enl%binning
      do jj=1,enl%numsy/enl%binning
           EBSDBackground(ii,jj) = sum(EBSDtmp((ii-1)*enl%binning+1:ii*enl%binning,(jj-1)*enl%binning:jj*enl%binning))
           if(isnan(EBSDBackground(ii,jj))) then
               stop 'Background pattern encountered NaN during binning'
           end if
      end do
  end do  
! and divide by binning^2
  EBSDBackground = EBSDBackground * bindx
else
   EBSDBackground = EBSDtmp
end if

! apply gamma scaling
EBSDBackground = EBSDBackground**enl%gammavalue

! normalize the pattern
EBSDBackground = EBSDBackground/NORM2(EBSDBackground)

end subroutine GenerateBackground


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
!--------------------------------------------------------------------------
recursive subroutine CalcEBSDPatternSingleFull(ipar,qu,accum,mLPNH,mLPSH,rgx,rgy,rgz,binned,Emin,Emax,mask, &
                                               prefactor, Fmatrix, removebackground)
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
real(kind=dbl),INTENT(IN),optional              :: Fmatrix(3,3)
character(1),INTENT(IN),OPTIONAL                :: removebackground

real(kind=sgl),allocatable                      :: EBSDpattern(:,:)
real(kind=sgl),allocatable                      :: wf(:)
real(kind=sgl)                                  :: dc(3),ixy(2),scl,bindx, tmp
real(kind=sgl)                                  :: dx,dy,dxm,dym, x, y, z
integer(kind=irg)                               :: ii,jj,kk,istat
integer(kind=irg)                               :: nix,niy,nixp,niyp
logical                                         :: nobg

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

bindx = 1.0/float(ipar(1))**2

allocate(EBSDpattern(ipar(2),ipar(3)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(3)
        dc = (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /)
 ! apply the grain rotation 
        dc = quat_Lp(qu(1:4),  dc)

        if (present(Fmatrix)) then
! apply the deformation if present
          dc = matmul(sngl(Fmatrix), dc)
        end if
 
! and normalize the direction cosines (to remove any rounding errors)
        dc = dc/sqrt(sum(dc**2))

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        ixy = scl * LambertSphereToSquare( dc, istat )
        if (istat .ne. 0) stop 'Something went wrong during interpolation...'
! four-point interpolation (bi-quadratic)
        nix = int(ipar(4)+ixy(1))-ipar(4)
        niy = int(ipar(5)+ixy(2))-ipar(5)
        nixp = nix+1
        niyp = niy+1
        if (nixp.gt.ipar(4)) nixp = nix
        if (niyp.gt.ipar(5)) niyp = niy
        if (nix.lt.-ipar(4)) nix = nixp
        if (niy.lt.-ipar(5)) niy = niyp
        dx = ixy(1)-nix
        dy = ixy(2)-niy
        dxm = 1.0-dx
        dym = 1.0-dy
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

if (ipar(1) .ne. 1) then
    do ii=1,ipar(2),ipar(1)
        do jj=1,ipar(3),ipar(1)
            binned(ii/ipar(1)+1,jj/ipar(1)+1) = &
            sum(EBSDpattern(ii:ii+ipar(1)-1,jj:jj+ipar(1)-1))
        end do
    end do
! and divide by binning^2

    binned = binned * bindx
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

bindx = 1.0/float(ipar(1))**2

allocate(EBSDpattern(ipar(2),ipar(8)),stat=istat)

binned = 0.0
EBSDpattern = 0.0

scl = float(ipar(4)) 

do ii = 1,ipar(2)
    do jj = 1,ipar(8)

        dc = sngl(quat_Lp(qu(1:4),  (/ rgx(ii,jj),rgy(ii,jj),rgz(ii,jj) /) ))

        dc = dc/sqrt(sum(dc**2))

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        ixy = scl * LambertSphereToSquare( dc, istat )
        if (istat .ne. 0) stop 'Something went wrong during interpolation...'
! four-point interpolation (bi-quadratic)
        nix = int(ipar(4)+ixy(1))-ipar(4)
        niy = int(ipar(5)+ixy(2))-ipar(5)
        nixp = nix+1
        niyp = niy+1
        if (nixp.gt.ipar(4)) nixp = nix
        if (niyp.gt.ipar(5)) niyp = niy
        if (nix.lt.-ipar(4)) nix = nixp
        if (niy.lt.-ipar(5)) niy = niyp
        dx = ixy(1)-nix
        dy = ixy(2)-niy
        dxm = 1.0-dx
        dym = 1.0-dy
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
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS  1.1 added omega as the second tilt angle
!> @date 07/07/15   SS  1.2 correction to the omega tilt parameter; old version in the comments

!--------------------------------------------------------------------------
recursive subroutine EBSDFullGenerateDetector(enl, scintillator, verbose)
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

type(EBSDNameListType),INTENT(INOUT)    :: enl
type(EBSDFullDetector),pointer          :: scintillator
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=sgl),allocatable              :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                          :: alp, ca, sa, cw, sw
real(kind=sgl)                          :: L2, Ls, Lc, calpha     ! distances
integer(kind=irg)                       :: i, j, Emin, Emax, istat, k, ipx, ipy, ierr   
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

L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy

   rhos = 1.0/sqrt(sx + scin_y(i)**2)

   allocate(scintillator%detector(j,i)%lambdaEZ(1:enl%numEbins,1:enl%numzbins))

   scintillator%detector(j,i)%lambdaEZ = 0.D0

   scintillator%detector(j,i)%dc = (/(scin_y(i) * ca + sa * Ls) * rhos, Lc * rhos,&
                                    (-sa * scin_y(i) + ca * Ls) * rhos/)

   scintillator%detector(j,i)%dc = scintillator%detector(j,i)%dc/NORM2(scintillator%detector(j,i)%dc)

!  if (ierr .ne. 0) then
!      call FatalError('EBSDFullGenerateDetector:','Lambert Projection coordinate undefined')
!  end if

  end do
end do
deallocate(scin_x, scin_y)

alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))
ipx = nint(enl%numsx/2 + enl%xpc)
ipy = nint(enl%numsy/2 + enl%ypc)
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

! make sure it has MCopenCL data in it
nullify(HDF_head)
call h5open_EMsoft(hdferr)
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
call h5close_EMsoft(hdferr)

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
