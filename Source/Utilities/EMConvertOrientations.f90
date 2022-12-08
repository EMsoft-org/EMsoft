! ###################################################################
! Copyright (c) 2016-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMConvertOrientations.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMConvertOrientations
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief convert a text file with orientations from one representation to another
!
!> @date 01/31/17 MDG 1.0 original
!--------------------------------------------------------------------------
program EMConvertOrientations
! 

use io
use local
use files
use rotations
use constants
use Lambert
use quaternions
use povray
use so3
use dictmod
use math
use NameListTypedefs
use NameListHandlers
use ECPmod
use error

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(ConvertOrientationsNameListType)   :: enl

integer(kind=irg),parameter :: numtypes = 8

real(kind=dbl)          :: rod(4), cu(3), ho(3), qu(4), eu(3), euFZ(3), om(3,3), ax(4), sp(3), rom(9), q
integer(kind=irg)       :: i,j,k, ierr, io_int(3)

character(fnlen)        :: fname, dataname, outnames(numtypes)
character(2)            :: outstring(numtypes), angleformat
integer(kind=irg)       :: FZtype, FZorder, numpoints, startunit
type(dicttype),pointer  :: dict

nmldeffile = 'EMConvertOrientations.nml'
progname = 'EMConvertOrientations.f90'
progdesc = 'Convert orientations from one representation to another'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 87 /), progname)

! deal with the namelist stuff
call GetConvertOrientationsNameList(nmldeffile,enl)

! generate the symmetry quaternions
nullify(dict)
allocate(dict)
dict%Num_of_init = 3 
dict%Num_of_iterations = 30
! get the point group number from the xtal file
dict%pgnum = GetPointGroup(enl%xtalname)
call DI_Init(dict,'WAT')  ! generate the quaternions; 'WAT' does not matter, could also be 'VMF'

! set the FZ type 
FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

io_int = (/ dict%pgnum, FZtype, FZorder /)
call WriteValue('  Point group, type, and order: ',io_int,3)

! input data file of orientations
dataname = trim(enl%anglefile)

outnames(1) = trim(enl%cubochoric) 
outnames(2) = trim(enl%homochoric)
outnames(3) = trim(enl%rodrigues)
outnames(4) = trim(enl%stereographic)
outnames(5) = trim(enl%axisangle)
outnames(6) = trim(enl%eulerangles)
outnames(7) = trim(enl%quaternion)
outnames(8) = trim(enl%rotationmatrix)

outstring = (/ 'cu', 'ho', 'ro', 'sp', 'ax', 'eu', 'qu', 'om' /)

! open the input data file and read the orientation format and the number of orientations
fname = trim(EMsoft_getEMdatapathname())//trim(enl%anglefile)
fname = EMsoft_toNativePath(fname)
open(unit=53,file=trim(fname),status='old',action='read')
read (53,*) angleformat
read (53,*) numpoints

! open the necessary output files and print the first two lines
startunit = 20
do i=1,numtypes
  if (trim(outnames(i)).ne.'undefined') then
    fname = trim(EMsoft_getEMdatapathname())//trim(outnames(i))
    fname = EMsoft_toNativePath(fname)
    open(unit=startunit+i,file=trim(fname),status='unknown',action='write')
    write (startunit+i,*) outstring(i)
    write (startunit+i,*) numpoints
    call Message('opening output file '//trim(outnames(i)))
  end if
end do

do j=1,numpoints
! read the input data and convert to Euler angles (in radians)
  if (angleformat.eq.'eu') then
    read (53,*) eu(1:3)
    eu = eu * cPi/180.D0
  end if
  if (angleformat.eq.'ho') then
    read (53,*) ho(1:3)
    eu = ho2eu(ho)
  end if
  if (angleformat.eq.'qu') then
    read (53,*) qu(1:4)
    eu = qu2eu(qu)
  end if
  if (angleformat.eq.'ro') then
    read (53,*) rod(1:4)
    eu = ro2eu(rod)
  end if
  if (angleformat.eq.'cu') then
    read (53,*) cu(1:3)
    eu = cu2eu(cu)
  end if
  if (angleformat.eq.'sp') then
    read (53,*) sp(1:3)
    q = 1.D0/(1.D0+sum(sp**2))
    qu = (/ 1.D0-sum(sp**2),2.D0*sp(1),2.D0*sp(2),2.D0*sp(3)/) * q
    eu = qu2eu(qu)
  end if
  if (angleformat.eq.'om') then
    read (53,*) rom(1:9)
    om(1,1:3) = rom(1:3)
    om(2,1:3) = rom(4:6)
    om(3,1:3) = rom(7:9)
    eu = om2eu(om)
  end if
  if (angleformat.eq.'ax') then
    read (53,*) ax(1:4)
    eu = ax2eu(ax)
  end if

! do we need to reduce this to the fundamental zone (RFZ) ?
  if (enl%reducetoRFZ.eq.1) then
    call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, euFZ)
    eu = euFZ
  end if

! and write the resulting transformed orientation to the selected output files
  do i=1,numtypes
    if (trim(outnames(i)).ne.'undefined') then
      select case (i)
      case (1) 
        cu = eu2cu(eu)
        write(startunit+i,*) cu(1:3)
      case (2) 
        ho = eu2ho(eu)
        write(startunit+i,*) ho(1:3)
      case (3) 
        rod = eu2ro(eu)
        write(startunit+i,*) rod(1:4)
      case (4) 
        qu = eu2qu(eu)
        sp = (/ qu(2), qu(3), qu(4) /) / (1.0+qu(1))
        write(startunit+i,*) sp(1:3)
      case (5) 
        ax = eu2ax(eu)
        write(startunit+i,*) ax(1:4)
      case (6) 
        write(startunit+i,*) eu(1:3)*180.0/cPi
      case (7) 
        qu = eu2qu(eu)
        write(startunit+i,*) qu(1:4)
      case (8) 
        om = eu2om(eu)
        rom(1:3) = om(1,1:3)
        rom(4:6) = om(2,1:3)
        rom(7:9) = om(3,1:3)
        write(startunit+i,*) rom(1:9)
      case default
        call FatalError(' ','unknown orientation representation')   
      end select
    end if
  end do
end do

! and close the output files
do i=1,numtypes
  if (trim(outnames(i)).ne.'undefined') then
    close(unit=startunit+i,status='keep')
    call Message(' --> closed output file '//trim(outnames(i)))
  end if
end do

end program EMConvertOrientations
