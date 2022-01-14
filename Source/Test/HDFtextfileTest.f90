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
! EMsoft:HDFtextfileTest.f90
!--------------------------------------------------------------------------
!
! MODULE: HDFtextfileTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief test module for writing and reading of nml files to/from HDF5 files
!
!> @date 10/29/16   MDG 1.0 original
!--------------------------------------------------------------------------

module HDFtextfileTest

use stringconstants

contains 

subroutine HDFtextfileExecuteTest(res) &
           bind(c, name='HDFtextfileExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFtextfileExecuteTest

use,INTRINSIC :: ISO_C_BINDING
use local
use HDF5
use typedefs
use HDFsupport

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)  :: res

character(fnlen)                :: HDFfilename, nmlname, tmppath, groupname, dataset, textfile

integer(kind=irg)               :: hdferr, isum

type(HDFobjectStackType)        :: HDF_head
character(len=1)                :: EMsoftnativedelimiter

! parameters for the namelist
integer(kind=irg)               :: ivalue, ivalue_save, istat
real(kind=sgl)                  :: fsingle, fsingle_save
real(kind=dbl)                  :: fdouble, fdouble_save
character(11)                   :: cstring, cstring_save

namelist / testnml / ivalue, fsingle, fdouble, cstring

! this program generates a namelist, writes it to a text file, then reads it in to
! write it to an HDF5 file; then read it back out and compare the values with the originals

!====================================
! generate the namelist and write it to a file
ivalue = 123
fsingle = 1.23
fdouble = 1.2D3
cstring = 'onetwothree'

ivalue_save = ivalue
fsingle_save = fsingle
fdouble_save = fdouble
cstring_save = cstring

! determine the pathname delimiter character
EMsoftnativedelimiter = EMsoft_getEMsoftnativedelimiter()

! get the location of the Temporary folder inside the Build folder (it always exists)
tmppath = EMsoft_getEMsofttestpath()

nmlname = trim(tmppath)//EMsoftnativedelimiter//'test.nml' ! //char(0)
open(dataunit, file=cstringify(nmlname), status='unknown', delim='apostrophe',iostat=istat)
write (*,*) 'nml file opened with iostatus ',istat
write(dataunit,nml=testnml,iostat=istat)
write (*,*) 'nml written with iostatus ',istat
close(dataunit,status='keep')
!====================================

!====================================
! nullify the push/pop stack pointer
nullify(HDF_head%next)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_nmlfile.h5'

write(*,*) 'writing file = <'//trim(HDFfilename)//'>'

hdferr =  HDF_createFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 1
  return
end if

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) then
  res = 2
  return
end if

! read the text file and write the array to the file
dataset = SC_testNML
hdferr = HDF_writeDatasetTextFile(dataset, nmlname, HDF_head)
if (hdferr.ne.0) then
  res = 3
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
write(*,*) '   --> done'
!====================================


!====================================
! next, we read this data set from the HDF5 file
! nullify the push/pop stack pointer
nullify(HDF_head%next)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_nmlfile.h5'

write(*,*) 'reading filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF_openFile(HDFfilename, HDF_head)
if (hdferr.ne.0) then
  res = 4
  return
end if


! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.ne.0) then
  res = 5
  return
end if


! read the dataset
dataset = SC_testNML
textfile = trim(tmppath)//EMsoftnativedelimiter//'testread.nml'
hdferr = HDF_extractDatasetTextfile(dataset, textfile, HDF_head)
if (hdferr.ne.0) then
  res = 6
  return
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
! read the new nml file and compare the entries with the input values
ivalue = 0
fsingle = 0.0
fdouble = 0.0D0
cstring = ''

nmlname = trim(tmppath)//EMsoftnativedelimiter//'testread.nml'
open(dataunit, file=cstringify(nmlname), status='unknown', delim='apostrophe')
read(dataunit,nml=testnml)
close(dataunit,status='keep')


isum = 20
if (ivalue.ne.ivalue_save) isum = isum + 1
if (fsingle.ne.fsingle_save) isum = isum + 2
if (fdouble.ne.fdouble_save) isum = isum + 4
if (cstring.ne.cstring_save) isum = isum + 8

if (isum.eq.20) then
  res = 0
else
  res = isum
end if
!====================================

end subroutine HDFtextfileExecuteTest


end module HDFtextfileTest
