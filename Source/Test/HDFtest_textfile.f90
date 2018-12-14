! ###################################################################
! Copyright (c) 2016,-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:HDFtest_textfile.f90
!--------------------------------------------------------------------------
!
! PROGRAM: HDFtest_textfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Test the insertion of an nml file into an HDF5 file
!
!> @date 10/28/16 MDG 1.0 original
!--------------------------------------------------------------------------
module HDFtest_textfile

use stringconstants

contains

subroutine HDFtestTextfile_ExecuteTest(result) &
bind(c, name='HDFtestTextfile_ExecuteTest') ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFtestTextfile_ExecuteTest


use local
use HDF5
use typedefs
use HDFsupport
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)               :: result

character(fnlen)                :: HDFfilename, tpn, nmlname, homepath, tmppath, groupname, dataset, textfile

integer(kind=irg)               :: i, j, k, l, dim1(1), dim2(2), dim3(3), dim4(4), ival, hdferr
integer(kind=irg),allocatable   :: iarr1(:), iarr2(:,:), iarr3(:,:,:), iarr4(:,:,:,:)
integer(kind=irg),allocatable   :: iarr1r(:), iarr2r(:,:), iarr3r(:,:,:), iarr4r(:,:,:,:)

type(HDFobjectStackType),pointer:: HDF_head
character(len=1)                :: EMsoftnativedelimiter
logical                         :: fexists

! parameters for the namelist
integer(kind=irg)               :: ivalue, ivalue_save
real(kind=sgl)                  :: fsingle, fsingle_save
real(kind=dbl)                  :: fdouble, fdouble_save
character(11)                   :: cstring, cstring_save

result = 1
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

tpn = EMsoft_toNativePath(EMsoft_getEMsoftTestingPath())
!homepath = EMsoft_getUserHomePath()
tmppath = trim(tpn)
write(*,*) tmppath
! does this folder exist? if not, create it
inquire(file=trim(tmppath),exist=fexists)
if (.not.(fexists)) then
  call system('mkdir '//trim(tmppath))
end if

nmlname = trim(tmppath)//EMsoftnativedelimiter//'test.nml'//CHAR(0)
open(dataunit, file=trim(nmlname), status='unknown', delim='apostrophe')
write(dataunit,nml=testnml)
close(dataunit,status='keep')
!====================================

!====================================
! nullify the push/pop stack pointer
nullify(HDF_head)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_integer.h5'//CHAR(0)
hdferr =  HDF_createFile(trim(HDFfilename), HDF_head)
write (*,*) 'HDFfilename>',HDFfilename,'<'
! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! write the single integer ival
dataset = SC_integer
ival = 123
hdferr = HDF_writeDatasetInteger(dataset, ival, HDF_head)

! read the text file and write the array to the file
dataset = SC_testNML
hdferr = HDF_writeDatasetTextFile(dataset, trim(nmlname), HDF_head)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
! next, we read this data set from the HDF5 file
! nullify the push/pop stack pointer
nullify(HDF_head)

! initialize the fortran HDF interface
CALL h5open_EMsoft(hdferr)

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_integer.h5'//CHAR(0)
hdferr =  HDF_openFile(trim(HDFfilename), HDF_head)
if(hdferr.lt.0) then
  result = hdferr
  return
end if


! create a namelist group to write all the namelist files into
groupname = "NMLfiles"//CHAR(0)
hdferr = HDF_openGroup(groupname, HDF_head)

! read the dataset
dataset = SC_testNML
textfile = trim(tmppath)//EMsoftnativedelimiter//'testread.nml'//CHAR(0)
hdferr = HDF_extractDatasetTextfile(dataset, textfile, HDF_head)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)
!====================================


!====================================
ivalue = 0
fsingle = 0.0
fdouble = 0.0D0
cstring = ''

! read the new nml file and compare the entries with the input values
nmlname = trim(tmppath)//EMsoftnativedelimiter//'testread.nml'//CHAR(0)
open(dataunit, file=trim(nmlname), status='unknown', delim='apostrophe')
read(dataunit,nml=testnml)
close(dataunit,status='keep')

write (*,*) ivalue, ivalue_save
write (*,*) fsingle, fsingle_save
write (*,*) fdouble, fdouble_save
write (*,*) cstring, cstring_save

result = 1


end subroutine HDFtestTextfile_ExecuteTest


end module HDFtest_textfile
