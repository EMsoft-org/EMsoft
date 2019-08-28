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
! EMsoft:EMgetEulers.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgetEulers
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief extract Euler angles into text file and generate EMEBSD.nml file 
!
!> @date 05/22/19 MDG 1.0 code extracted from EMgetANG program 
!--------------------------------------------------------------------------

program EMgetEulers

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use EBSDmod
use EBSDDImod

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(EulersNameListType)                    :: eunl
integer(kind=irg)                           :: res

nmldeffile = 'EMgetEulers.nml'
progname = 'EMgetEulers.f90'
progdesc = 'extract Euler angles into text file and generate EMEBSD.nml file'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 112 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMgetEulers','JSON input not yet implemented')
else
  call GetEulersNameList(nmldeffile,eunl)
end if

call EulerSubroutine(eunl, progname, nmldeffile)

end program EMgetEulers

!--------------------------------------------------------------------------
!
! SUBROUTINE:EulerSubroutine
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Subroutine to extract Euler angles and create EMEBSD.nml file
!
!> @param eunl  namelist pointer
!> @param progname name of the program
!> @param nmldeffile namelist filename
!
!> @date 05/22/19 MDG 1.1 first version
!> @date 05/24/19 MDG 1.2 added raddeg option
!--------------------------------------------------------------------------

subroutine EulerSubroutine(eunl, progname, nmldeffile)

use local
use typedefs 
use NameListTypedefs
use NameListHandlers
use initializersHDF
use HDF5
use HDFsupport
use constants
use EBSDmod
use ECPmod
use EBSDDImod
use timing
use error
use io
use EBSDiomod
use files
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(EulersNameListType)                :: eunl
type(EBSDDIdataType)                    :: EBSDDIdata
type(EBSDIndexingNameListType)          :: dinl

logical                                 :: refined 
character(fnlen)                        :: ename, nmlname
integer(kind=irg)                       :: hdferr, j, istat, Nexp

real(kind=sgl),allocatable              :: euler_best(:,:)                                   

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

refined = .FALSE.
if (trim(eunl%angledataset).eq.'refined') then 
    call readEBSDDotProductFile(eunl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                                getRefinedEulerAngles=.TRUE.)
    refined = .TRUE.
else
    call readEBSDDotProductFile(eunl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                                getEulerAngles=.TRUE.)
end if

! close the fortran HDF interface
call h5close_EMsoft(hdferr)

Nexp = EBSDDIdata%Nexp
allocate(euler_best(3,Nexp),stat=istat)
if (istat .ne. 0) then
    call FatalError('EMgetEulers','Failed to allocate euler angle array')
end if 
euler_best = 0.0
if (refined.eqv..TRUE.) then
  if (eunl%raddeg.eq.'deg') then 
    euler_best(1:3,1:Nexp) = EBSDDIdata%RefinedEulerAngles(1:3,1:Nexp)*180.0/cPi
  else 
    euler_best(1:3,1:Nexp) = EBSDDIdata%RefinedEulerAngles(1:3,1:Nexp)
  end if 
  deallocate(EBSDDIdata%RefinedEulerAngles)
  call Message(' Extracting refined Euler angles from dot product file')
else
  if (eunl%raddeg.eq.'deg') then 
    euler_best(1:3,1:Nexp) = EBSDDIdata%EulerAngles(1:3,1:Nexp)
  else
    euler_best(1:3,1:Nexp) = EBSDDIdata%EulerAngles(1:3,1:Nexp)*cPi/180.0
  end if 
  deallocate(EBSDDIdata%EulerAngles)
  call Message(' Extracting Euler angles from dot product file')
end if 

call Message('  --> dot product EBSD HDF5 file read')

!==============================
! and prepare the Euler angle text file 
ename = trim(EMsoft_getEMdatapathname())//trim(eunl%txtfile)
ename = EMsoft_toNativePath(ename)

open(unit=dataunit,file=trim(ename),status='unknown',action='write')
write (dataunit,*) 'eu'
write (dataunit,*) Nexp

do j=1,Nexp
  write(dataunit,*) euler_best(1:3,j)
end do

close(unit=dataunit,status='keep')
call Message('  --> closed Euler angle text file '//trim(ename))

!==============================
! if requested, also prepare an EMEBSD.nml file.
if (trim(eunl%EMEBSDnmlfile).ne.'undefined') then
  nmlname = trim(EMsoft_getEMdatapathname())//trim(eunl%EMEBSDnmlfile)
  nmlname = EMsoft_toNativePath(nmlname)

! write all the relevant parameters to the EMEBSD.nml file
  open(UNIT=dataunit,FILE=trim(nmlname),form='formatted',STATUS='unknown')
  write (dataunit,"(A11)") '&EBSDdata'
  write (dataunit,"(' L = ',F12.4,',')") dinl%L
  write (dataunit,"(' thetac = ',F12.4,',')") dinl%thetac
  write (dataunit,"(' delta = ',F12.4,',')") dinl%delta
  write (dataunit,"(' numsx = ',I4,',')") dinl%numsx
  write (dataunit,"(' numsy = ',I4,',')") dinl%numsy
  write (dataunit,"(' xpc = ',F12.4,',')") dinl%xpc
  write (dataunit,"(' ypc = ',F12.4,',')") dinl%ypc
  write (dataunit,"(' omega = ',F12.4,',')") dinl%omega
  write (dataunit,"(' alphaBD = ',F12.4,',')") 0.0
  write (dataunit,"(' energymin = ',F12.4,',')") dinl%energymin
  write (dataunit,"(' energymax = ',F12.4,',')") dinl%energymax
  write (dataunit,"(' includebackground = ''',A1,''',')") 'y'
  write (dataunit,"(' anglefile = ''',A,''',')") trim(eunl%txtfile)
  write (dataunit,"(' anglefiletype = ''',A12,''',')") 'orientations'
  write (dataunit,"(' eulerconvention = ''',A3,''',')") 'tsl'
  write (dataunit,"(' makedictionary = ''',A1,''',')") 'n'
  write (dataunit,"(' masterfile = ''',A,''',')") trim(dinl%masterfile)
  write (dataunit,"(' datafile = ''',A,''',')") trim(eunl%datafile)
  write (dataunit,"(' bitdepth = ''',A4,''',')") '8bit'
  write (dataunit,"(' beamcurrent = ',F12.4,',')") dinl%beamcurrent
  write (dataunit,"(' dwelltime = ',F12.4,',')") dinl%dwelltime
  write (dataunit,"(' poisson = ''',A1,''',')") 'n'
  write (dataunit,"(' binning = ',I2,',')") dinl%binning
  write (dataunit,"(' applyDeformation = ''',A1,''',')") 'n'
  write (dataunit,"(' scalingmode = ''',A3,''',')") dinl%scalingmode
  write (dataunit,"(' gammavalue = ',F12.4,',')") dinl%gammavalue
  write (dataunit,"(' nthreads = ',I2,',')") dinl%nthreads
  write (dataunit,"(A2)") ' /'
  close(UNIT=dataunit,STATUS='keep')
  call Message('')
  call Message('      You may need to edit the namelist file to make sure that the')
  call Message('      file paths are correct, or to change any other parameters before')
  call Message('      running the EMEBSD program with this input file.')
  call Message('')
  call Message('  --> closed EMEBSD name list file '//trim(nmlname))
end if

end subroutine EulerSubroutine
