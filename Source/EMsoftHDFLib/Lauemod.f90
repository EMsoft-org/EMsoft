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
! EMsoft:Lauemod.f90
!--------------------------------------------------------------------------
!
! MODULE: Laue
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic routines for Laue pattern simulation
!
!> @date 03/28/19  MDG 1.0 original version 
!--------------------------------------------------------------------------
module Lauemod

use local
use typedefs
use stringconstants

IMPLICIT NONE

type LaueMPdataType
        real(kind=sgl),allocatable      :: mLPNH(:,:)
        real(kind=sgl),allocatable      :: mLPSH(:,:)
        real(kind=sgl),allocatable      :: masterSPNH(:,:)
        real(kind=sgl),allocatable      :: masterSPSH(:,:)
end type LaueMPdataType

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: readLaueMasterFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read a Laue master pattern File into the correct namelist and data structure
!
!> @param MPfile filename of the Laue Master Pattern file
!> @param lmnl LaueMasterNameListType
!> @param hdferr error code
!
!> @date 03/28/19 MDG 1.0 original 
!--------------------------------------------------------------------------
recursive subroutine readLaueMasterFile(MPfile, lmnl, hdferr, LaueMPdata, getmLPNH, getmLPSH, getmasterSPNH, getmasterSPSH)
!DEC$ ATTRIBUTES DLLEXPORT :: readLaueMasterFile

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
type(LaueMasterNameListType),INTENT(INOUT)          :: lmnl
!f2py intent(in,out) ::  lmnl
integer(kind=irg),INTENT(OUT)                       :: hdferr
type(LaueMPdataType),INTENT(INOUT)                  :: LaueMPdata
!f2py intent(in,out) ::  LaueMPdata
logical,INTENT(IN),OPTIONAL                         :: getmLPNH
logical,INTENT(IN),OPTIONAL                         :: getmLPSH
logical,INTENT(IN),OPTIONAL                         :: getmasterSPNH
logical,INTENT(IN),OPTIONAL                         :: getmasterSPSH

character(fnlen)                                    :: infile, groupname, datagroupname, dataset
logical                                             :: stat, readonly, g_exists, f_exists, FL
type(HDFobjectStackType)                            :: HDF_head
integer(kind=irg)									:: nlines
integer(HSIZE_T)                                    :: dims2(2)
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)

! we assume that the calling program has opened the HDF interface

infile = trim(EMsoft_getEMdatapathname())//trim(MPfile)
infile = EMsoft_toNativePath(infile)
inquire(file=trim(infile), exist=f_exists)

if (.not.f_exists) then
  call FatalError('readLaueMasterFile','Laue master pattern input file does not exist')
end if

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(infile), stat, hdferr)

if (stat.eqv..FALSE.) then ! the file exists, so let's open it an first make sure it is an EBSD dot product file
   call FatalError('readLaueMasterFile','This is not a proper HDF5 file')
end if 
   
! open the Master Pattern file 
nullify(HDF_head%next)
readonly = .TRUE.
hdferr =  HDF_openFile(infile, HDF_head, readonly)

!====================================
! make sure this is a Laue Master Pattern file
!====================================
groupname = SC_NMLfiles
    hdferr = HDF_openGroup(groupname, HDF_head)
dataset = 'LauemasterNML'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..FALSE.) then
    call HDF_pop(HDF_head,.TRUE.)
    call FatalError('readLaueMasterFile','this is not a Laue Master Pattern file')
end if
call HDF_pop(HDF_head)

!====================================
! read all NMLparameters group datasets
!====================================
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_LauemasterNameList
    hdferr = HDF_openGroup(groupname, HDF_head)

! we'll read these roughly in the order that the HDFView program displays them...
dataset = SC_hdfname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    lmnl%hdfname = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_intfactor
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, lmnl%intfactor)

dataset = SC_kappaVMF
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, lmnl%kappaVMF)

dataset = SC_lambdamax
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, lmnl%lambdamax)

dataset = SC_lambdamin
    call HDF_readDatasetFloat(dataset, HDF_head, hdferr, lmnl%lambdamin)

dataset = SC_npx
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, lmnl%npx)

dataset = SC_patchw
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, lmnl%patchw)

dataset = SC_tiffname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    lmnl%tiffname = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_xtalname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    lmnl%xtalname = trim(stringarray(1))
    deallocate(stringarray)

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! open the Monte Carlo data group
groupname = SC_EMData
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Lauemaster
    hdferr = HDF_openGroup(groupname, HDF_head)

if (present(getmLPNH)) then 
  if (getmLPNH.eqv..TRUE.) then
    dataset = SC_mLPNH
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, LaueMPdata%mLPNH)
  end if 
end if

if (present(getmLPSH)) then 
  if (getmLPSH.eqv..TRUE.) then
    dataset = SC_mLPSH
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, LaueMPdata%mLPSH)
  end if 
end if

if (present(getmasterSPNH)) then 
  if (getmasterSPNH.eqv..TRUE.) then
    dataset = SC_masterSPNH
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, LaueMPdata%masterSPNH)
  end if 
end if

if (present(getmasterSPSH)) then 
  if (getmasterSPSH.eqv..TRUE.) then
    dataset = SC_masterSPSH
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, LaueMPdata%masterSPSH)
  end if 
end if

! and close the HDF5 Master Pattern file
call HDF_pop(HDF_head,.TRUE.)

end subroutine readLaueMasterFile


end module Lauemod
