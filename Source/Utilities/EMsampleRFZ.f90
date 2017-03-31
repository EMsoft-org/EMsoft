! ###################################################################
! Copyright (c) 2014, Marc De Graef/Carnegie Mellon University
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
!
! PROGRAM: EMsampleRFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic program to generate a uniform sampling of Rodrigues Fundamental Zone
!
!> @details This program calls the SampleRFZ routine of the so3 module to generate
!> an angle file of euler angles for points that uniformly sample an RFZ for a given
!> crystal symmetry.  
!
!> @date 5/12/14   MDG 1.0 original
!> @date 5/29/14   MDG 1.1 integrated with EMsoft package (started from standalone program)
!--------------------------------------------------------------------------
program EMsampleRFZ

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                :: nmldeffile, progname, progdesc
type(RFZNameListType)           :: rfznl

! deal with the command line arguments, if any
nmldeffile = 'EMsampleRFZ.nml'
progname = 'EMsampleRFZ.f90'
progdesc = 'Create a uniform sampling of Rodrigues space and output angle list'

! print some information
call EMsoft(progname, progdesc)

call Interpret_Program_Arguments(nmldeffile,1,(/ 60 /), progname )

! deal with the namelist stuff
call GetRFZNameList(nmldeffile,rfznl)

! perform the zone axis computations
call CreateSampling(rfznl,progname)

end program EMsampleRFZ

!--------------------------------------------------------------------------
!
! SUBROUTINE:CreateSampling
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate a sampling of the Rodrigues Fundamental Zone for a given xtal symmetry
!
!> @todo add an HDF5 output option 
!
!> @param nmlfile namelist file name
!
!> @date 05/29/14 MDG 1.0 original
!> @date 12/09/14 MDG 2.0 changed rfznl handling
!> @date 08/19/15 MDG 2.1 added all rotation representations as output options
!> @date 12/22/16 MDG 2.2 added option to generate reduced sampling inside constant misorientation ball
!> @date 02/01/17 MDG 2.3 added option to generate sampling inside a conical volume in Rodrigues space
!--------------------------------------------------------------------------
subroutine CreateSampling(rfznl, progname)

use local
use typedefs
use NameListTypedefs
use constants
use rotations
use io
use so3

IMPLICIT NONE

type(RFZNameListType),INTENT(IN)        :: rfznl
character(fnlen),INTENT(IN)             :: progname

integer(kind=irg)                       :: i, FZcnt, io_int(1), FZtype, FZorder
real(kind=dbl)                          :: eud(3), rtod, cud(3), qud(4), hod(3), ax(4), calpha, conevector(3)
type(FZpointd),pointer                  :: FZlist, FZtmp
logical                                 :: doeu = .FALSE., docu = .FALSE., doho = .FALSE., doqu = .FALSE., &
                                           doom = .FALSE., doax = .FALSE., doro = .FALSE.

rtod = 180.D0/cPi

! determine which files to create
if (trim(rfznl%euoutname).ne.'undefined') doeu = .TRUE.
if (trim(rfznl%cuoutname).ne.'undefined') docu = .TRUE.
if (trim(rfznl%hooutname).ne.'undefined') doho = .TRUE.
if (trim(rfznl%quoutname).ne.'undefined') doqu = .TRUE.
if (trim(rfznl%rooutname).ne.'undefined') doro = .TRUE.
if (trim(rfznl%omoutname).ne.'undefined') doom = .TRUE.
if (trim(rfznl%axoutname).ne.'undefined') doax = .TRUE.

! a bit of output
call Message('Starting computation for point group '//PGTHD(rfznl%pgnum))

! determine which function we should call for this point group symmetry
FZtype = FZtarray(rfznl%pgnum)
FZorder = FZoarray(rfznl%pgnum)

! get the linked list for the FZ for point group symmetry pgnum for nsteps along the cubic semi-edge
nullify(FZlist)
FZcnt = 0
if (trim(rfznl%samplemode).eq.'RFZ') then
  call SampleRFZ(rfznl%nsteps,rfznl%pgnum,rfznl%gridtype,FZcnt,FZlist)
end if
if (trim(rfznl%samplemode).eq.'MIS') then
  write(*,*) 'Rodrigues vector = ', rfznl%rodrigues
  call sample_isoCubeFilled(rfznl%maxmisor, rfznl%nsteps, FZcnt, FZlist)
  call SampleIsoMisorientation(rfznl%rodrigues, rfznl%maxmisor, FZcnt, FZlist)
end if
if (trim(rfznl%samplemode).eq.'CON') then
  conevector = rfznl%conevector/sqrt(sum(rfznl%conevector**2))
  write(*,*) 'cone axis unit vector   = ', conevector
  write(*,*) 'cone semi opening angle = ', rfznl%semiconeangle
  calpha = cos(rfznl%semiconeangle/rtod)
  write (*,*) 'minimum dot product    = ', calpha
  call sample_Cone(conevector, calpha, rfznl%nsteps, FZtype, FZorder, FZcnt, FZlist)
end if

io_int(1) = FZcnt
call WriteValue('Total number of unique orientations generated = ',io_int,1,"(I10)")

! generate a list of all orientations in Euler angle format (if requested)
if (doeu) then
  open (UNIT=20,FILE=trim(rfznl%euoutname),FORM='formatted',STATUS='unknown')
  write (20,"(A)") 'eu'
  write (20,"(I8)") FZcnt
end if

! generate a list of all orientations in cubochoric format (if requested)
if (docu) then
  open (UNIT=21,FILE=trim(rfznl%cuoutname),FORM='formatted',STATUS='unknown')
  write (21,"(A)") 'cu'
  write (21,"(I8)") FZcnt
end if

! generate a list of all orientations in homochoric format (if requested)
if (doho) then
  open (UNIT=22,FILE=trim(rfznl%hooutname),FORM='formatted',STATUS='unknown')
  write (22,"(A)") 'ho'
  write (22,"(I8)") FZcnt
end if

! generate a list of all orientations in quternion format (if requested)
if (doqu) then
  open (UNIT=23,FILE=trim(rfznl%quoutname),FORM='formatted',STATUS='unknown')
  write (23,"(A)") 'qu'
  write (23,"(I8)") FZcnt
end if

! generate a list of all orientations in Rodrigues format (if requested)
if (doro) then
  open (UNIT=24,FILE=trim(rfznl%rooutname),FORM='formatted',STATUS='unknown')
  write (24,"(A)") 'ro'
  write (24,"(I8)") FZcnt
end if

! generate a list of all orientations in orientation matrix format (if requested)
if (doom) then
  open (UNIT=25,FILE=trim(rfznl%omoutname),FORM='formatted',STATUS='unknown')
  write (25,"(A)") 'om'
  write (25,"(I8)") FZcnt
end if

! generate a list of all orientations in axis angle pair format (if requested)
if (doax) then
  open (UNIT=26,FILE=trim(rfznl%axoutname),FORM='formatted',STATUS='unknown')
  write (26,"(A)") 'ax'
  write (26,"(I8)") FZcnt
end if

if (trim(rfznl%samplemode).eq.'RFZ') then
! then scan through the list and write the requested representation(s) to its/their file(s) 
  FZtmp => FZlist
  do i = 1, FZcnt
! euler angles
    if (doeu) write (20,"(3F14.6)") ro2eu(FZtmp%rod) * rtod

! cubochoric
    if (docu) write (21,"(3F14.6)") ro2cu(FZtmp%rod)

! homochoric
    if (doho) write (22,"(3F14.6)") ro2ho(FZtmp%rod)

! quaternion
    if (doqu) write (23,"(4F14.6)") ro2qu(FZtmp%rod)

! rodrigues
    if (doro) write (24,"(4F14.6)") FZtmp%rod
  
! orientation matrix
    if (doom) write (25,"(9F14.6)") ro2om(FZtmp%rod)

! axis angle pair
    if (doax) then
      ax = ro2ax(FZtmp%rod)
      ax(4) = ax(4) * rtod
      write (26,"(4F14.6)") ax
    end if

! next orientation
    FZtmp => FZtmp%next
  end do
else  ! we use the rotated rodrigues vectors ...
! then scan through the list and write the requested representation(s) to its/their file(s) 
  FZtmp => FZlist
  do i = 1, FZcnt
! euler angles
    if (doeu) write (20,"(3F14.6)") ro2eu(FZtmp%trod) * rtod

! cubochoric
    if (docu) write (21,"(3F14.6)") ro2cu(FZtmp%trod)

! homochoric
    if (doho) write (22,"(3F14.6)") ro2ho(FZtmp%trod)

! quaternion
    if (doqu) write (23,"(4F14.6)") ro2qu(FZtmp%trod)

! rodrigues
    if (doro) write (24,"(4F14.6)") FZtmp%trod
  
! orientation matrix
    if (doom) write (25,"(9F14.6)") ro2om(FZtmp%trod)

! axis angle pair
    if (doax) then
      ax = ro2ax(FZtmp%trod)
      ax(4) = ax(4) * rtod
      write (26,"(4F14.6)") ax
    end if

! next orientation
    FZtmp => FZtmp%next
  end do
end if

if (doeu) close(UNIT=20,STATUS='keep')
if (docu) close(UNIT=21,STATUS='keep')
if (doho) close(UNIT=22,STATUS='keep')
if (doqu) close(UNIT=23,STATUS='keep')
if (doro) close(UNIT=24,STATUS='keep')
if (doom) close(UNIT=25,STATUS='keep')
if (doax) close(UNIT=26,STATUS='keep')

if (doeu) call Message('Euler angles stored in file '//rfznl%euoutname)
if (docu) call Message('Cubochoric representation stored in file '//rfznl%cuoutname)
if (doho) call Message('Homochoric representation stored in file '//rfznl%hooutname)
if (doqu) call Message('Quaternion representation stored in file '//rfznl%quoutname)
if (doro) call Message('Rodrigues vector representation stored in file '//rfznl%rooutname)
if (doom) call Message('Orientation matrix representation stored in file '//rfznl%omoutname)
if (doax) call Message('Axis-angle pair representation stored in file '//rfznl%axoutname)


end subroutine CreateSampling
