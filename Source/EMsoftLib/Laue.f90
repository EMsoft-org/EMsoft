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
! EMsoft:Laue.f90
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

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:Lauereadangles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles from an angle file (either Euler or quaternions)
!
!> @param orientationfilename file name
!> @param numangles number of angles read 
!> @param angles list of angles 
!
!> @todo this should really become a routine that is independent of all the 
!> diffraction modalities
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Lauereadangles(orientationfilename,numangles,angles,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Lauereadangles

use typedefs
use NameListTypedefs
use io
use error 
use files
use quaternions
use rotations

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: orientationfilename
integer(kind=irg),INTENT(OUT)           :: numangles
type(AngleType),pointer                 :: angles
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: io_int(1), i
character(2)                            :: atype
character(3)							:: degrad
real(kind=sgl),allocatable              :: eulang(:,:)   ! euler angle array

real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
integer(kind=irg)                       :: istat
character(fnlen)                        :: anglefile

!====================================
! get the angular information, either in Euler angles or in quaternions, from a text file
!====================================
! open the angle file 
anglefile = trim(EMsoft_getEMdatapathname())//trim(orientationfilename)
anglefile = EMsoft_toNativePath(anglefile)
open(unit=dataunit,file=trim(anglefile),status='old',action='read')

! get the type of angle first [ 'eu' or 'qu' ]
read(dataunit,*) atype
if ((atype.ne.'eu').and.(atype.ne.'qu')) then 
	call FatalError('Lauereadangles','orientations need to be in Euler or quaternion form')
end if

! then the number of angles in the file
read(dataunit,*) numangles

if (present(verbose)) then 
  io_int(1) = numangles
  call WriteValue(' Number of angle entries = ',io_int,1)
end if

if (atype.eq.'eu') then
! allocate the euler angle array
  allocate(eulang(3,numangles),stat=istat)
! if istat.ne.0 then do some error handling ... 
  do i=1,numangles
    read(dataunit,*) eulang(1:3,i)
  end do
  close(unit=dataunit,status='keep')

! are the angles in radians or degrees ?
  if (maxval(eulang).gt.6.3) then
  	degrad = 'deg'
  else 
  	degrad = 'rad'
  end if

! convert the euler angle triplets to quaternions
  allocate(angles%quatang(4,numangles),stat=istat)

  if (present(verbose)) call Message('  -> converting Euler angles to quaternions', frm = "(A/)")
  
  if (degrad.eq.'deg') then 
	  do i=1,numangles
	    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i)*dtor)
	  end do
  else
	  do i=1,numangles
	    angles%quatang(1:4,i) = eu2qu(eulang(1:3,i))
	  end do
  end if
  deallocate(eulang)
else
! the input file has quaternions, not Euler triplets
  allocate(angles%quatang(4,numangles),stat=istat)
  do i=1,numangles
    read(dataunit,*) angles%quatang(1:4,i)
  end do
end if

close(unit=dataunit,status='keep')

call Message(' completed reading orientations')

end subroutine Lauereadangles


!--------------------------------------------------------------------------
!
! FUNCTION:getLauePattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a single Laue pattern
!
!> @param lnl Laue name list
!> @param qu orientation quaternion
!> @param reflist list of potential reflections
!> @param kouter largest wave number
!> @param kinner smallest wave number 
!> @param npx number of pixels along x on detector
!> @param npy along y
!> @param refcnt number of reflections in linked list
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive function getLauePattern(lnl, qu, reflist, kouter, kinner, npx, npy, refcnt) result(pattern)
!DEC$ ATTRIBUTES DLLEXPORT :: getLauePattern

use local
use typedefs
use NameListTypedefs
use io
use files
use quaternions
use rotations

IMPLICIT NONE

type(LaueNameListType),INTENT(IN)			:: lnl 
real(kind=dbl),INTENT(IN) 					:: qu(4) 
type(Laue_g_list),pointer                   :: reflist 
real(kind=sgl),INTENT(IN) 					:: kouter 
real(kind=sgl),INTENT(IN) 					:: kinner 
integer(kind=irg),INTENT(IN) 				:: npx 
integer(kind=irg),INTENT(IN) 				:: npy 
integer(kind=irg),INTENT(IN) 				:: refcnt
real(kind=sgl) 								:: pattern(npx, npy)

real(kind=sgl) 								:: r1, r2, grot(3), gyz, gbig, gsmall, kspot, kprime(3), kp(3), scl
type(Laue_g_list),pointer                   :: rltmp
integer(kind=irg)							:: gg, traref

traref = 1  ! reflection mode = 1
if (trim(lnl%Lauemode).eq.'transmission') traref = 2 ! transmission mode = 2

scl = lnl%SDdistance / lnl%pixelsize 

pattern = 0.0

nullify(rltmp)
rltmp => reflist

do gg=1,refcnt 
  grot = sngl(quat_Lp(conjg(qu),rltmp%xyz))
! make sure this reflection lies between the two limiting Ewald spheres
  gyz = sum(grot*grot) 
  gbig = 2.0*grot(1)*kouter + gyz 
  gsmall = 2.0*grot(1)*kinner + gyz 
  if ((gbig.le.0.0).and.(gsmall.ge.0.0)) then 
! this is a good point so let's draw stuff
! first, determine the wave number for this point
    kspot = - gyz / (2.0*grot(1))
    kprime = (/ kspot, 0.0, 0.0 /)  + grot
    if ((kprime(1).gt.0.0).and.(traref.eq.2)) then 
      kp = (kprime/abs(kprime(1))) * scl
      if ( (abs(kp(2)).le.(lnl%numpx/2)).and.(abs(kp(3)).le.(lnl%numpy/2)) ) then 
! draw the reflection on the transmission screen 
	    call addLauereflection(pattern, lnl%numpx, lnl%numpy, kp, sngl(rltmp%sfs), lnl%spotw)
      end if
    end if 
    if ((kprime(1).lt.0.0).and.(traref.eq.1)) then 
      kp = (kprime/abs(kprime(1))) * scl
      if ( (abs(kp(2)).le.(lnl%numpx/2)).and.(abs(kp(3)).le.(lnl%numpy/2)) ) then 
! draw the reflection on the backreflection screen 
		kp(2) = -kp(2)
	    call addLauereflection(pattern, lnl%numpx, lnl%numpy, kp, sngl(rltmp%sfs), lnl%spotw)
      end if
    end if 
  end if
  rltmp => rltmp%next
end do

end function getLauePattern

!--------------------------------------------------------------------------
!
! SUBROUTINE:addLauereflection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a single reflection to a Laue pattern as a Gaussian spot
!
!> @param pattern Laue pattern intensity array
!> @param npx number of pixels along x on detector
!> @param npy along y
!> @param kp diffracted wave vector scaled to the detector geometry
!> @param sfs kinematical intensity (structure factor squared)
!> @param spotw spot size parameter 
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine addLauereflection(pattern, npx, npy, kp, sfs, spotw) 
!DEC$ ATTRIBUTES DLLEXPORT :: addLauereflection

IMPLICIT NONE

integer(kind=irg),INTENT(IN)			:: npx
integer(kind=irg),INTENT(IN)			:: npy
real(kind=sgl),INTENT(INOUT) 			:: pattern(npx, npy)
real(kind=sgl),INTENT(IN)				:: kp(3)
real(kind=sgl),INTENT(IN) 				:: sfs 
real(kind=sgl),INTENT(IN)				:: spotw 

integer(kind=irg),parameter 			:: dd = 15
real(kind=sgl),allocatable,save 		:: xar(:,:), yar(:,:) 
real(kind=sgl) 							:: row(dd), px, py, evals(dd,dd), dx, dy
integer(kind=irg) 						:: i, ddd, ix, iy

! make sure that the xar and yar arrays are allocated 
if (allocated(xar).eqv..FALSE.) then 
	row = (/ (real(i), i=0,dd-1) /) - real(dd/2)
	allocate(xar(dd,dd), yar(dd,dd))
	do i=1,dd 
		xar(:,i) = row
		yar(i,:) = row
	end do
end if

ddd = (dd-1)/2

px = kp(2) + real(npx/2)
py = kp(3) + real(npy/2)

dx = px-int(px)
dy = py-int(py)
ix = int(px)-ddd
iy = int(py)-ddd 

evals = alog(sfs+1.0) * exp( - ( (xar-dx)**2 + (yar -dy)**2 ) * spotw )

if ( (ix+dd.lt.npx).and.(iy+dd.lt.npy).and.(ix.gt.0).and.(iy.gt.0) ) pattern(ix:ix+dd-1,iy:iy+dd-1) = evals

end subroutine addLauereflection

end module Lauemod