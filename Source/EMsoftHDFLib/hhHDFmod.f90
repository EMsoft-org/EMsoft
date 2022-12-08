! ###################################################################
! Copyright (c) 2014-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:hhHDFmod.f90 
!--------------------------------------------------------------------------
!
! MODULE: hhHDFmod
!
!> @author Marc De Graef
! This module contains HDF support routines that are used by the EMhh4.f90 program
! 
!> @date 08/23/19 MDG 1.0 initial version
!--------------------------------------------------------------------------
module hhHDFmod

use local
use constants


contains 

!--------------------------------------------------------------------------
!
! SUBROUTINE: writeHH4_HDFfile
!
!> @author Marc De Graef
!
!> @brief generate HDF5 output file for the EMhh4 program
! 
!> @param hhnl name list 
!> @param BFINTENS Bright Field image array
!> @param DFINTENS Bright Field image array
!> @params progname calling program name
!> @params nmldeffile  name list parameter file name
!
!> @date 08/22/19 MDG 1.0 adapted from similar code in dictionary indexing program
!--------------------------------------------------------------------------
recursive subroutine writeHH4_HDFfile(hhnl, BF, DF, dstr, tstrb, tstre, legendfiles, progname, nmldeffile)
!DEC$ ATTRIBUTES DLLEXPORT :: writeHH4_HDFfile

use HDF5
use HDFsupport 
use NameListTypedefs


IMPLICIT NONE 

type(EMhh4NameListType),INTENT(INOUT)               :: hhnl
!f2py intent(in,out) ::  hhnl
real(kind=sgl),INTENT(IN)                           :: BF(hhnl%ICOL, hhnl%IROW, hhnl%wnum)
real(kind=sgl),INTENT(IN)                           :: DF(hhnl%ICOL, hhnl%IROW, hhnl%wnum)
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
character(15),INTENT(IN)                            :: tstre
character(fnlen),INTENT(IN)                         :: legendfiles(hhnl%wnum)
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

integer(kind=irg)                                   :: irow, icol, imnum, hdferr, i
character(fnlen)                                    :: groupname, dataset, hhfile, nmlname, manufacturer
character(3)                                        :: lnum
type(HDFobjectStackType)                            :: HDF_head

irow = hhnl%IROW
icol = hhnl%ICOL
imnum = hhnl%wnum

nullify(HDF_head%next)

! Create a new file using the default properties.
hhfile = trim(EMsoft_getEMdatapathname())//trim(hhnl%outname)
hhfile = EMsoft_toNativePath(hhfile)
hdferr =  HDF_createFile(hhfile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening file')

call hh_writeInfo(dstr, tstrb, tstre, progname, hhnl, nmldeffile, HDF_head)

!===============================
! create a namelist group to write all the legend files into
groupname = 'LegendFiles'
  hdferr = HDF_createGroup(groupname, HDF_head)

! and write the nml file for this program to the HDF5 file
! read the text file and write the array to the file
  do i=1,hhnl%wnum
    write (lnum,"(I3.3)") i
    dataset = 'Legend_'//lnum
    hdferr = HDF_writeDatasetTextFile(dataset, legendfiles(i), HDF_head)
  end do

! leave this group
  call HDF_pop(HDF_head)
  
!===============================
! and finally write all the actual data sets 
groupname = SC_EMdata
  hdferr = HDF_createGroup(groupname, HDF_head)

dataset = 'BF'
hdferr = HDF_writeDatasetFloatArray3D(dataset, BF, hhnl%ICOL, hhnl%IROW, hhnl%wnum, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'writeHH4_HDFfile: unable to create BF dataset',.TRUE.)

dataset = 'DF'
hdferr = HDF_writeDatasetFloatArray3D(dataset, DF, hhnl%ICOL, hhnl%IROW, hhnl%wnum, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'writeHH4_HDFfile: unable to create DF dataset',.TRUE.)

! close the file 
call HDF_pop(HDF_head,.TRUE.)
  

end subroutine writeHH4_HDFfile



!--------------------------------------------------------------------------
!
! SUBROUTINE:hh_writeInfo
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write general information fields to the h5ebsd file, including EMsoft specific fields
!
!> @param dstr date string
!> @param tstrb begin time string
!> @param tstre end time string
!> @param progname name of the calling program
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine hh_writeInfo(dstr, tstrb, tstre, progname, hhnl, nmldeffile, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: hh_writeInfo

use NameListTypedefs
use NameListHandlers
use NameListHDFwriters

IMPLICIT NONE

character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
character(15),INTENT(IN)                            :: tstre
character(fnlen),INTENT(IN)                         :: progname
type(EMhh4NameListType),INTENT(INOUT)               :: hhnl
!f2py intent(in,out) ::  hhnl
character(fnlen),INTENT(IN)                         :: nmldeffile
type(HDFobjectStackType)                            :: HDF_head

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)									:: dataset, groupname 
integer(kind=irg) 									:: hdferr

allocate(stringarray(1))

! set the Manufacturer and Version data sets
dataset = SC_Manufacturer
  stringarray(1)= trim(progname)
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

dataset = SC_Version
  stringarray(1)= 'EMsoft '//EMsoft_getEMsoftversion()
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! add the EMsoft header group
! write the EMheader to the file
groupname = 'EMheader'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! and write the nml file for this program to the HDF5 file
! read the text file and write the array to the file
  dataset = 'EMHH4NML'
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteHH4NameList(HDF_head, hhnl)

! leave this group
  call HDF_pop(HDF_head)

end subroutine hh_writeInfo

end module hhHDFmod 
