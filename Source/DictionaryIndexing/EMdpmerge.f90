! ###################################################################
! Copyright (c) 2015-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMdpmerge.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMdpmerge
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief merge the data from two or more dp files into a single .ctf and/or .ang file
!
!> @date 08/17/19 MDG 1.0 new program
!--------------------------------------------------------------------------

program EMdpmerge

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use HDFsupport
use EBSDmod
use EBSDDImod
use commonmod
use ISO_C_BINDING
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(dpmergeNameListType)                   :: dpmnl
integer(kind=irg)                           :: istat, res

type(EBSDIndexingNameListType)              :: dinl
type(EBSDDIdataType)                        :: EBSDDIdata
real(kind=sgl),allocatable                  :: dplist(:,:), OSMlist(:,:), exptIQ(:)
integer(kind=irg),allocatable               :: phaseID(:)
integer(kind=irg)                           :: ipf_wd, ipf_ht
integer(kind=irg)                           :: dims(2), hdferr, io_int(2), i, numdp
character(fnlen)                            :: fname, xtalname(5)

nmldeffile = 'EMdpmerge.nml'
progname = 'EMdpmerge.f90'
progdesc = 'Merge data from two or more dot product files into a single .ctf and/or .ang file'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 261 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMdpmerge','JSON input not yet implemented')
else
  call GetdpmergeNameList(nmldeffile,dpmnl)
end if


! how many input files are there ?
numdp = 0
do i=1,5
  if (trim(dpmnl%dotproductfile(i).ne.'')) numdp = numdp + 1
end do 
io_int(1) = numdp
call WriteValue('',io_int,1,"('found ',I2,' dot product file names')")


! open the fortran HDF interface 
call h5open_EMsoft(hdferr)

if (indexingmode.eq.'DI') then 
! loop over the input files and extract all the necessary data; at the same time, check to make 
! sure that they cover the same data (after the first one has been read, allocate the data arrays) 
  do i=1,numdp
    if (trim(dpmnl%usedp).eq.'original') then 
      call readEBSDDotProductFile(dpmnl%dotproductfile(i), dinl, hdferr, EBSDDIdata, &
                                  getEulerAngles = .TRUE., &
                                  getCI = .TRUE.)
    else
      call readEBSDDotProductFile(dpmnl%dotproductfile(i), dinl, hdferr, EBSDDIdata, &
                                  getTopMatchIndices = .TRUE.)
    end if

    if (i.eq.1) then 
  ! get the ROI dimensions and allocate the arrays 


    else 
  ! check dimensions of the ROI; they must be the same.  if they are, then add the data to the various arrays

    end if 


    deallocate(EBSDDIdata% )
  end do 

  ! close HDF5 interface
  call h5close_EMsoft(hdferr)


  ! determine which phase has the largest confidence index for each ROI sampling point
  allocate(phaseID(), )




  ! write a new .ctf file, if requested  
  if (trim(dpmnl%ctfname).ne.'undefined') then 
    call ctfmerge_writeFile(dinl,xtalname,ipar,eulerarray, dpvals, OSMmap, exptIQ)
    call Message('Data stored in ctf file : '//trim(dpmnl%ctfname))
  end if 

  ! write a new .ang file, if requested 
  if (trim(dpmnl%angname).ne.'undefined') then 
    call angmerge_writeFile()
    call Message('Data stored in ang file : '//trim(dpmnl%angname))
  end if 

else ! indexing mode must be SI
! the files are Spherical Indexing files, so they do not have an OSM map in them, and some other
! things are different, so we need a somewhat different approach.


end if 





end program EMdpmerge

