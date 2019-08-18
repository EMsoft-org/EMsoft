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
use EBSDiomod
use commonmod
use ISO_C_BINDING
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(dpmergeNameListType)                   :: dpmnl
type(EBSDMasterNameListType)                :: mpnl
integer(kind=irg)                           :: istat, res

type(EBSDIndexingNameListType)              :: dinl
type(EBSDDIdataType)                        :: EBSDDIdata
type(EBSDMPdataType)                        :: EBSDMPdata
real(kind=sgl),allocatable                  :: dplist(:,:), OSMlist(:,:), exptIQ(:), eangles(:,:,:), pfrac(:), pID(:)
integer(kind=irg),allocatable               :: phaseID(:), pnum(:)
integer(kind=irg)                           :: ipf_wd, ipf_ht, irow, numpat, ml(1), ipar(4)
integer(kind=irg)                           :: dims(1), hdferr, io_int(2), i, numdp
real(kind=sgl)                              :: io_real(1)
character(fnlen)                            :: fname, xtalname(5), infile, rdxtalname
logical                                     :: f_exists 

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
  if (trim(dpmnl%dotproductfile(i)).ne.'') numdp = numdp + 1
end do 
io_int(1) = numdp
call WriteValue('',io_int,1,"(' Found ',I2,' dot product file names'/)")


! open the fortran HDF interface 
call h5open_EMsoft(hdferr)

if (dpmnl%indexingmode.eq.'DI') then 
! loop over the input files and extract all the necessary data; at the same time, check to make 
! sure that they cover the same data (after the first one has been read, allocate the data arrays) 
  do i=1,numdp
    call Message(' Reading data from '//trim(dpmnl%dotproductfile(i)) )
    if (trim(dpmnl%usedp).eq.'original') then ! read the original DI results
      call readEBSDDotProductFile(dpmnl%dotproductfile(i), dinl, hdferr, EBSDDIdata, &
                                  getEulerAngles = .TRUE., &
                                  getIQ = .TRUE., &
                                  getOSM = .TRUE., &
                                  getCI = .TRUE.)
    else   ! read the results from the refinement run
      call readEBSDDotProductFile(dpmnl%dotproductfile(i), dinl, hdferr, EBSDDIdata, &
                                  getRefinedEulerAngles = .TRUE., &
                                  getIQ = .TRUE., &
                                  getOSM = .TRUE., &
                                  getRefinedDotProducts = .TRUE.)
    end if

    if (i.eq.1) then 
  ! get the ROI dimensions and allocate the arrays 
      if (sum(dinl%ROI).ne.0) then 
        ipf_wd = dinl%ROI(3)
        ipf_ht = dinl%ROI(4)
      else
        ipf_wd = dinl%ipf_wd
        ipf_ht = dinl%ipf_ht
      end if
      numpat = ipf_wd * ipf_ht 
      allocate( dplist(numpat,numdp), OSMlist(numpat, numdp), exptIQ(numpat), eangles(3,numpat,numdp) ) 
    else 
  ! check dimensions of the ROI; they must be the same.  if they are, then add the data to the various arrays
      dims = shape(EBSDDIdata%CI)
      if (dims(1).ne.numpat) then 
        call FatalError('EMdpmerge','inconsistent ROI dimensions in dot product input files ' )
      end if 
    end if 
! copy the data
    do irow = 1, ipf_ht
      OSMlist( (irow-1)*ipf_wd+1:irow*ipf_wd,i) = EBSDDIdata%OSM(1:ipf_wd,irow)
    end do
    exptIQ(:) = EBSDDIdata%IQ(:)
    deallocate( EBSDDIdata%OSM, EBSDDIdata%IQ )
    if (trim(dpmnl%usedp).eq.'original') then 
      eangles(1:3,1:numpat,i) = EBSDDIdata%EulerAngles(1:3,1:numpat)
      dplist(1:numpat,i) = EBSDDIdata%CI(1:numpat)
      deallocate( EBSDDIdata%EulerAngles, EBSDDIdata%CI )
    else 
      eangles(1:3,1:numpat,i) = EBSDDIdata%RefinedEulerAngles(1:3,1:numpat) 
      dplist(1:numpat,i) = EBSDDIdata%RefinedDotProducts(1:numpat)
      deallocate( EBSDDIdata%RefinedEulerAngles, EBSDDIdata%RefinedDotProducts )
    end if 

! finally, get the name of the xtal file from the master pattern file
! if that file can not be found, ask the user interactively to enter the xtalname parameter 
    infile = trim(EMsoft_getEMdatapathname())//trim(dinl%masterfile)
    infile = EMsoft_toNativePath(infile)
    inquire(file=trim(infile), exist=f_exists)

    if (f_exists.eqv..TRUE.) then
      call readEBSDMasterPatternFile(dinl%masterfile, mpnl, hdferr, EBSDMPdata)
      xtalname(i) = trim(EBSDMPdata%xtalname)
    else 
      call Message('***************************')
      call Message('Master pattern file '//trim(infile)//' can not be found')
      call Message('***************************')
      call WriteValue(' Current dot product file :', trim(dpmnl%dotproductfile(i)))
      call ReadValue('  Enter crystal structure file name (with extension) ', rdxtalname)
      xtalname(i) = trim(rdxtalname)
    end if

  end do 



  ! determine which phase has the largest confidence index for each ROI sampling point
  allocate(phaseID(numpat), pID(numdp))
  do i=1,numpat 
    pID(1:numdp) = dplist(i,1:numdp)
    ml = maxloc(pID)
    phaseID(i) = ml(1)
  end do
  deallocate(pID)

  ! determine the phase fractions and print that information 
  allocate(pnum(numdp),pfrac(numdp))
  pnum = 0
  do i=1,numpat
    pnum(phaseID(i)) = pnum(phaseID(i)) + 1
  end do
  pfrac = float(pnum)/float(numpat) * 100.0 
  call Message(' Phase fractions :',"(/A)")
  call Message(' -----------------')
  do i=1,numdp 
    io_real(1) = pfrac(i)
    call WriteValue('  Phase '//trim(xtalname(i)), io_real, 1, "(F6.2)")
  end do

  ! write a new .ctf file, if requested  
  ipar(1) = numpat
  ipar(2) = numdp 
  ipar(3) = ipf_wd 
  ipar(4) = ipf_ht 

  if (trim(dpmnl%ctfname).ne.'undefined') then 
    dinl%ctffile = trim(dpmnl%ctfname)
    call ctfmerge_writeFile(dinl,xtalname,ipar,eangles, phaseID, dplist, OSMlist, exptIQ)
    call Message('Merged orientation data stored in ctf file : '//trim(dpmnl%ctfname))
  end if 

  ! ! write a new .ang file, if requested 
  ! if (trim(dpmnl%angname).ne.'undefined') then 
  !   call angmerge_writeFile()
  !   call Message('Merged orientation data stored in ang file : '//trim(dpmnl%angname))
  ! end if 

else ! indexing mode must be SI
! the files are Spherical Indexing files, so they do not have an OSM map in them, and some other
! things are different, so we need a somewhat different approach.


end if 


  ! close HDF5 interface
call h5close_EMsoft(hdferr) 


end program EMdpmerge

