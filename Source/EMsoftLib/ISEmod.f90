! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:ISEmod.f90
!--------------------------------------------------------------------------
!
! PROGRAM: ISEmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief ISEmod routines for ion-induced secondary electrons computations
!
!> @note This is based on the iCHORD model for ballistic ion channeling
!
!> @date 12/18/20  MDG 1.0 initial module, based on iCHORD forward model for ISE
!--------------------------------------------------------------------------
module ISEmod 

use local 
use rotations
use quaternions
use others

IMPLICIT NONE 

contains 

recursive function getISEintensity(k, atomcnt, atomlist, atomrad, rsphere, a, b) result(inten)
!DEC$ ATTRIBUTES DLLEXPORT :: getISEintensity

IMPLICIT NONE

real(kind=sgl), INTENT(IN)          :: k(3)                ! this is already a unit vector
integer(kind=irg), INTENT(IN)       :: atomcnt 
real(kind=sgl), INTENT(IN)          :: atomlist(3,atomcnt) ! in Angstrom units
real(kind=sgl), INTENT(IN)          :: atomrad(atomcnt)    ! in Angstrom units
real(kind=sgl), INTENT(IN)          :: rsphere             ! in Angstrom units
real(kind=sgl), INTENT(IN)          :: a
real(kind=sgl), INTENT(IN)          :: b
real(kind=sgl)                      :: inten 

real(kind=sgl)                      :: apos(3,atomcnt), dp, axang(4), qu(4), xyz(3), Dsphere, Appix, shft, px, py, &
                                       dis(atomcnt), ndis(atomcnt), Gd(atomcnt), adisk, bdisk, arad(atomcnt)
real(kind=sgl),allocatable          :: pplane(:,:) 
integer(kind=irg)                   :: i, iatom, ss(atomcnt), spsize, ipx, ipy, ix, iy, maxrad

! use the k vector to determine a rotation quaternion
dp = DOT_PRODUCT(k, (/ 0.0, 0.0, 1.0 /) )
if (abs(dp).eq.1.0) then ! no rotation needed
! just copy the atom coordinates
    apos = atomlist 
else 
    axang = (/ -k(2), k(1), 0.0, acos( dp ) /)
    qu = ax2qu( axang )
    do i=1,atomcnt 
        xyz = atomlist(1:3,i) 
        apos(1:3,i) = quat_Lp(qu, xyz)
    end do
end if 

! then compute the distances to the projection plane (normal to the z-axis)
do i=1,atomcnt 
    dis(i) = rsphere - apos(3,i)
end do 

! sort in ascending order
call qsortd( dble(dis), ss, atomcnt)
do i=1,atomcnt 
    ndis(i) = dis(ss(i))
end do 
dis = ndis
! and reverse the ndis array
do i=1,atomcnt
    ndis(i) = dis(atomcnt-i+1)
end do 

! get the brightness values 
Gd = a * ndis**b

! get the projection plane size
Dsphere = 2.0 * rsphere
Appix = 0.25
spsize = int(Dsphere * 8.0/Appix )
arad = (atomrad/Appix)**2

! initialize the projection plane
allocate(pplane(spsize,spsize)) 
pplane = 0.0 

! get the largest disk radius in units of pixels ... 
maxrad = int(sqrt(maxval(arad))) + 1

! move all the atom x and y coordinates so that the center of the sphere projects on the center of the plane
shft = spsize*0.5
apos = 6.0 * apos / Appix + shft 

! for all the atoms in the list, replicate the intensity in the projection plane in a small disk
! but only if the intensity is equal to zero to guarantee that the brightest disk is kept
do iatom=1,atomcnt 
  ! get the projected position in pixel units (nearest point) 
  px = apos(1,ss(iatom))
  py = apos(2,ss(iatom))
  ipx = nint(px)
  ipy = nint(py)
  do ix = ipx-maxrad,ipx+maxrad
    do iy = ipy-maxrad,ipy+maxrad
      if ( ((ix-px)**2+(iy-py)**2).le.arad(iatom) ) then
       if (pplane(ix,iy).eq.0.0) then
         pplane(ix,iy)=Gd(iatom)
       end if
      end if
    end do
  end do
end do

! and summ all the values to get the ISE intensity 
inten = sum(pplane)

deallocate(pplane)

end function getISEintensity

end module ISEmod
