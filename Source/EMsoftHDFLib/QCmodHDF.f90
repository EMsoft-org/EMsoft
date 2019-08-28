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
module QCmodHDF

use local 
use typedefs
use stringconstants
use ECPmod
use NameListTypedefs
use EBSDmod

contains
!--------------------------------------------------------------------------
!
! SUBROUTINE:ECPreadMCfile
!
!> @author Marc De Graef/Saransh Singh, Carnegie Mellon University
!
!> @brief read monte carlo file
!
!> @param enl ECP name list structure
!> @param acc energy structure
!
!> @date 02/07/18  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ECPreadQCMCMasterfile(enl,acc,master,efile,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: ECPreadQCMCMasterfile

use NameListTypedefs
use files
use io
use HDF5
use HDFsupport
use error
use ECPmod

IMPLICIT NONE

type(ECPNameListType),INTENT(INOUT)     :: enl
type(ECPLargeAccumType),pointer         :: acc
type(ECPMasterType),pointer             :: master
character(fnlen),INTENT(IN),OPTIONAL    :: efile
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: istat, hdferr, nlines, nx
logical                                 :: stat, readonly
integer(HSIZE_T)                        :: dims3(3),dims4(4)
character(fnlen)                        :: groupname, dataset, energyfile 
character(fnlen),allocatable            :: stringarray(:)

integer(kind=irg),allocatable           :: acc_z(:,:,:,:), acc_e(:,:,:)
real(kind=sgl),allocatable              :: srtmp(:,:,:)

type(HDFobjectStackType),pointer        :: HDF_head

! is the efile parameter present? If so, use it as the filename, otherwise use the enl%energyfile parameter
if (PRESENT(efile)) then
  energyfile = efile
else
  energyfile = trim(EMsoft_getEMdatapathname())//trim(enl%energyfile)
end if
energyfile = EMsoft_toNativePath(energyfile)

allocate(acc)
allocate(master)

! first, we need to check whether or not the input file is of the HDF5 format type; if
! it is, we read it accordingly, otherwise we give error. Old format not supported anymore
!
call h5fis_hdf5_f(energyfile, stat, hdferr)

if (stat) then
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

  if(enl%MCmode .ne. 'bse1') then
     call FatalError('ECPreadMCfile','This file is not bse1 mode. Please input correct HDF5 file')
  end if

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

dataset = SC_sigstart
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MCsigstart)

dataset = SC_sigend
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MCsigend)

dataset = SC_sigstep
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MCsigstep)

dataset = SC_omega
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MComega)

! close the MCCL name list group
  call HDF_pop(HDF_head)
  !call HDF_pop(HDF_head)

! ECPQCMaster name list group will be read next
! open the namelist group
!groupname = SC_NMLparameters
!  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = 'ECPQCMasterNameList'
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_energyfile
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterenergyfile = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_npx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
  enl%npy = enl%npx

! close the name list
  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)
!
! read from the EMheader
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCOpenCL
  hdferr = HDF_openGroup(groupname, HDF_head)

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

groupname = SC_MCOpenCL
  hdferr = HDF_openGroup(groupname, HDF_head)

! read data items 
dataset = SC_numangle
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numangle)

dataset = SC_numzbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numzbins)

dataset = SC_accumz
  call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
  allocate(acc%accum_z(1:dims4(1),1:dims4(2),1:dims4(3),1:dims4(4)))
  acc%accum_z = acc_z
  deallocate(acc_z)

dataset = SC_accume
  call HDF_readDatasetIntegerArray3D(dataset, dims3, HDF_head, hdferr, acc_e)
  allocate(acc%accum_e(1:dims3(1),1:dims3(2),1:dims3(3)))
  acc%accum_e = acc_e
  deallocate(acc_e)
 
  enl%num_el = sum(acc%accum_z)

  call HDF_pop(HDF_head)
  !call HDF_pop(HDF_head)

!groupname = SC_EMData
!  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = 'ECPQCmaster'
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_numset
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numset)

dataset = SC_mLPNH
  call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, srtmp)
  allocate(master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy),stat=istat)
  master%mLPNH = sum(srtmp,3)
  deallocate(srtmp)

dataset = SC_mLPSH
  call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, srtmp)
  allocate(master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy),stat=istat)
  master%mLPSH = sum(srtmp,3)
  deallocate(srtmp)

! and close everything
  call HDF_pop(HDF_head,.TRUE.)
else
!==============================================
! OLD VERSION OF MC FILE NOT SUPPORTED ANYMORE
! COMMENTING OUT THE FOLLOWING LINES
! REPLACING WITH FATALERROR COMMENT
!==============================================

  call FatalError('ECPreadQCMCMasterfile','The file is not a h5 file. Old version of MC file not supported anymore!')
  !if (present(verbose)) call Message('opening '//trim(enl%energyfile), frm = "(A)")
end if

if (present(verbose)) then
    if (verbose) call Message(' -> completed reading '//trim(enl%energyfile), frm = "(A)")
end if

end subroutine ECPreadQCMCMasterfile

! !-------------------ROUTINE DISABLED FOR NOW UNTIL NEW EMEBSDQC PROGRAM IS READY
! !
! ! SUBROUTINE:EBSDQCreadMCMasterfile
! !
! !> @author Saransh Singh, Carnegie Mellon University
! !
! !> @brief read monte carlo file for EBSD QC master
! !
! !> @param enl EBSD name list structure
! !> @param acc energy structure
! !
! !> @date 02/07/18  SS 1.0 original
! !
! !> ROUTINE SCHEDULED TO BE REMOVED AND REPLACED BY NEW SEPARATED NAMELIST APPROACH [MDG, 4/4/18]
! !
! !--------------------------------------------------------------------------
! recursive subroutine EBSDQCreadMCMasterfile(enl, acc, master, efile, verbose)
! !DEC$ ATTRIBUTES DLLEXPORT :: EBSDQCreadMCMasterfile

! use io
! use HDF5
! use HDFsupport
! use error

! IMPLICIT NONE

! type(EBSDNameListType),INTENT(INOUT)      :: enl
! type(EBSDAngleType),pointer               :: angles
! character(fnlen),INTENT(IN),OPTIONAL      :: efile
! logical,INTENT(IN),OPTIONAL               :: verbose

! type(EBSDLargeAccumType),pointer          :: acc
! type(EBSDMasterType),pointer              :: master

! integer(kind=irg)                         :: istat, hdferr, nlines, nx
! logical                                   :: stat, readonly, g_exists, FL
! integer(HSIZE_T)                          :: dims3(3), dims4(4), dims(1)

! character(fnlen)                          :: groupname, dataset, energyfile, masterfile, datagroupname 
! character(fnlen),allocatable              :: stringarray(:)

! integer(kind=irg),allocatable             :: acc_e(:,:,:), acc_z(:,:,:,:)

! real(kind=sgl),allocatable                :: mLPNH(:,:,:) 
! real(kind=sgl),allocatable                :: mLPSH(:,:,:) 
! real(kind=sgl),allocatable                :: EkeVs(:) 
! integer(kind=irg),allocatable             :: atomtype(:)

! real(kind=sgl),allocatable                :: srtmp(:,:,:,:)

! type(HDFobjectStackType),pointer          :: HDF_head

! ! is the efile parameter present? If so, use it as the filename, otherwise use the enl%energyfile parameter
! if (PRESENT(efile)) then
!   energyfile = efile
! else
!   energyfile = trim(EMsoft_getEMdatapathname())//trim(enl%energyfile)
! end if
! energyfile = EMsoft_toNativePath(energyfile)

! call h5fis_hdf5_f(energyfile, stat, hdferr)

! if (stat) then
! ! open the fortran HDF interface
!   call h5open_EMsoft(hdferr)

!   nullify(HDF_head)

! ! open the MC file using the default properties.
!   readonly = .TRUE.
!   hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! ! open the namelist group
! groupname = SC_NMLparameters
!   hdferr = HDF_openGroup(groupname, HDF_head)

! groupname = SC_MCCLNameList
!   hdferr = HDF_openGroup(groupname, HDF_head)

! ! read all the necessary variables from the namelist group
! dataset = SC_xtalname
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%MCxtalname = trim(stringarray(1))
!   deallocate(stringarray)
  
! dataset = SC_mode
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%MCmode = trim(stringarray(1))
!   deallocate(stringarray)
  
!   if (enl%MCmode .ne. 'full') call FatalError('EBSDQCreadMCMasterfile','File not in full mode. Please input correct HDF5 file')
! dataset = SC_numsx
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nsx)
!   enl%nsx = (enl%nsx - 1)/2
!   enl%nsy = enl%nsx

! dataset = SC_EkeV
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%EkeV)

! dataset = SC_Ehistmin
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%Ehistmin)

! dataset = SC_Ebinsize
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%Ebinsize)

! dataset = SC_depthmax
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%depthmax)

! dataset = SC_depthstep
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%depthstep)

! dataset = SC_sig
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MCsig)

! dataset = SC_omega
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MComega)

! ! close the name list group
!   call HDF_pop(HDF_head)

! groupname = SC_EBSDmasterNameList
!   hdferr = HDF_openGroup(groupname, HDF_head)

! dataset = SC_npx
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
!   enl%npy = enl%npx

!   call HDF_pop(HDF_head)
!   call HDF_pop(HDF_head)

! ! read from the EMheader
! groupname = SC_EMheader
!   hdferr = HDF_openGroup(groupname, HDF_head)

! ! next we need to make sure that this file has Monte Carlo data in it...
!   datagroupname = 'MCOpenCL'
!   call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
!   if (.not.g_exists) then
!     call Message('This file does not appear to contain any Monte Carlo data or the file')
!     call Message('has the old data format; please use the EMmergeEBSD script to update')
!     call Message('the Monte Carlo data file to the correct format.  You will need to use')
!     call Message('the -C option to perform this update; consult the main EMsoft manual pages.')
!     call FatalError('EBSDQCreadMCMasterfile','This HDF file does not contain any Monte Carlo data')
!   end if
!   hdferr = HDF_openGroup(datagroupname, HDF_head)

! dataset = SC_ProgramName
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%MCprogname = trim(stringarray(1))
!   deallocate(stringarray)

! dataset = SC_Version
!   call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
!   enl%MCscversion = trim(stringarray(1))
!   deallocate(stringarray)

!   call HDF_pop(HDF_head)
!   call HDF_pop(HDF_head)

! ! open the Data group
! groupname = SC_EMData
!   hdferr = HDF_openGroup(groupname, HDF_head)
!   hdferr = HDF_openGroup(datagroupname, HDF_head)

! ! read data items 
! dataset = SC_numEbins
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numEbins)

! dataset = SC_numzbins
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numzbins)

! dataset = SC_accume
!   call HDF_readDatasetIntegerArray3D(dataset, dims3, HDF_head, hdferr, acc_e)
!   enl%num_el = sum(acc_e)
!   nx = (dims3(2)-1)/2
!   allocate(acc%accum_e(1:dims3(1),-nx:nx,-nx:nx))
!   acc%accum_e = acc_e
!   deallocate(acc_e)
  
! dataset = SC_accumz
!   call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
!   allocate(acc%accum_z(1:dims4(1),1:dims4(2),1:dims4(3),1:dims4(4)))
!   acc%accum_z = acc_z
!   deallocate(acc_z)

! ! and close everything
!   call HDF_pop(HDF_head)

! datagroupname = 'EBSDmaster'
! call H5Lexists_f(HDF_head%objectID,trim(datagroupname),g_exists, hdferr)
!   if (.not.g_exists) then
!     call Message('This file does not appear to contain any EBSD master data or the file')
!     call Message('has the old data format; please use the EMmergeEBSD script to update')
!     call Message('the master pattern data file to the correct format.  You will need to use')
!     call Message('the -M option to perform this update; consult the main EMsoft manual pages.')
!     call FatalError('EBSDQCreadMCMasterfile','This HDF file does not contain any Monte Carlo data')
!   end if
!   hdferr = HDF_openGroup(datagroupname, HDF_head)

! dataset = SC_numEbins
!   call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)
! ! make sure that MC and Master results are compatible
!   if ((enl%numEbins.ne.enl%nE).and.(.not.PRESENT(efile))) then
!     call Message('Energy histogram and Lambert stack have different energy dimension; aborting program', frm = "(A)")
!     call HDF_pop(HDF_head,.TRUE.)
!     stop
!   end if

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

!   call HDF_pop(HDF_head)
!   call HDF_pop(HDF_head)

! groupname = SC_EMheader
!   hdferr = HDF_openGroup(groupname, HDF_head)
!   hdferr = HDF_openGroup(datagroupname, HDF_head)

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
!   call FatalError('EBSDQCreadMCMasterfile','MC file not found')
! end if

! if (present(verbose)) call Message(' -> completed reading Monte Carlo and Master data from '//trim(enl%energyfile), frm = "(A)")

! end subroutine EBSDQCreadMCMasterfile

end module
