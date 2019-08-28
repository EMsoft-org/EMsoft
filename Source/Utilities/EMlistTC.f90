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
! EMsoft:EMlistTC.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMlistTC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief List information for texture components in an arbitrary crystal system
!
!> @date 01/14/19 MDG 1.0 original
!--------------------------------------------------------------------------
program EMlistTC

use local
use constants
use crystal
use symmetry
use dictmod
use HDFsupport
use ECPmod
use rotations
use so3
use io

IMPLICIT NONE

integer(kind=irg)           :: io_int3(3), io_int4(4), io_int7(7), nvec(3), bvec4(4), bvec(3)
real(kind=dbl)              :: cnvec(3), cbvec(3), t(3), om(3,3)
character(fnlen)            :: progname, progdesc, gname
type(orientationtyped)      :: res
type(unitcell)              :: cell
integer(kind=irg)           :: ntmp(48,3), btmp(48,3), i, j, k, nnum, bnum, ntc, FZtype, FZorder, pgnum
integer(kind=irg), allocatable :: ortho(:,:)
type(dicttype),pointer      :: dict

progname = 'EMlistTC.f90'
progdesc = 'List information about texture components'

! print some information
call EMsoft(progname, progdesc)

!allocate(cell)        
! ask for the crystal structure file
cell % SG % SYM_reduce=.FALSE.
call ReadValue(' Enter xtal file name : ', gname,"(A)")
cell%fname = trim(gname)
call CrystalData(cell)

! allocate the symmetry operations for Fundamental Zone handling.
pgnum = 0
do i=1,32
 if (SGPG(i).le.cell % SYM_SGnum) pgnum = i
end do

allocate(dict)
dict%Num_of_init = 1 
dict%Num_of_iterations = 1
dict%pgnum = pgnum
call DI_Init(dict,'VMF')

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)



! ask for the texture component (distinguish between hexagonal indices and regular)
call Message('  ')
if (cell%hexset.eqv..TRUE.) then 
  call Message(' Enter the texture component symbol {hk.l}<uvtw>')
  call Message('  (This structure uses the hexagonal indexing system!) ')
  call ReadValue('   Enter the three-index planar indices {hkl} ',io_int3,3)
  nvec = io_int3(1:3)
  call ReadValue('   Enter the four-index direction indices <uvtw> ',io_int4,4)
  bvec4 = io_int4(1:4)
! convert four-index to three-index 
  call MilBrav(bvec,bvec4,'43')
  io_int3(1:3) = bvec(1:3)
  call WriteValue('   ---> in three index notation: ',io_int3,3,"('[',I4,I4,I4,']')")
else
  call Message(' Enter the texture component symbol {hkl}<uvw>')
  call ReadValue('   Enter the planar indices {hkl} ',io_int3,3)
  nvec = io_int3(1:3)
  call ReadValue('   Enter the direction indices <uvw> ',io_int3,3)
  bvec = io_int3(1:3)
end if 

! In determining the list of equivalent texture components, we need to make sure that we only keep
! those for which the plane vector is perpendicular to the direction vector !
call Message('  ')
call Message(' Determining unique texture components ')
call CalcFamily(cell,nvec,nnum,'r',ntmp)
call CalcFamily(cell,bvec,bnum,'d',btmp)
io_int3(1) = nnum
call WriteValue('    Multiplicity of {hkl} : ', io_int3,1,"(I2)")
io_int3(1) = bnum
call WriteValue('    Multiplicity of <uvw> : ', io_int3,1,"(I2)")

! next, take every single combination of {hkl} and <uvw> and keep only those that are orthogonal
allocate(ortho(nnum,bnum))
ortho = 0
do i=1,nnum
  do j=1,bnum
    if (sum(ntmp(i,1:3)*btmp(j,1:3)).eq.0) ortho(i,j) = 1
  end do
end do
! how many are there ?
ntc = sum(ortho)
io_int3(1) = ntc
call WriteValue(' There are ',io_int3,1,"(I4, ' equivalent texture components.')")
call Message('  ')

! next, get the rotation representation for all of them in all the rotation representations
k = 0
do i=1,nnum
  do j=1,bnum
    if (ortho(i,j).eq.1) then 
      k = k+1
      io_int3(1) = k
      call Message('-------')
      call WriteValue(' TC ',io_int3,1,"(I3,'  -->  ',$)")
! transform the two vectors to the Cartesian crystallographic reference frame
      call TransSpace(cell, dble(ntmp(i,1:3)), cnvec, 'r', 'c') 
      call TransSpace(cell, dble(btmp(j,1:3)), cbvec, 'd', 'c') 
! normalize them
      call NormVec(cell, cnvec, 'c')
      call NormVec(cell, cbvec, 'c')
! get the cross product n x b and normalize
      call CalcCross(cell,cnvec,cbvec,t,'c','c',0)
      call NormVec(cell, t, 'c')
! get these into a rotation matrix with b,t,n in the columns (in that order)
      om(1:3,1) = cbvec(1:3)
      om(1:3,2) = t(1:3)
      om(1:3,3) = cnvec(1:3)
      res = init_orientation(om,'om')
! and print the information to the command line
      if (cell%hexset.eqv..FALSE.) then
        io_int7(1:3) = ntmp(i,1:3)
        io_int7(4:6) = btmp(j,1:3)
        call WriteValue('',io_int7,6,"('(',I3,I3,I3,') [',I3, I3, I3,']')")
      else
        io_int7(1:3) = ntmp(i,1:3)
        call MilBrav(btmp(j,1:3),io_int7(4:7),'34')
        call WriteValue('',io_int7,7,"('(',I3,I3,' . ',I3,') [',I3, I3, I3, I3,']')")
      end if
      call print_orientation(res)
! is this one inside the fundamental zone ?
      if (IsinsideFZ(res%rodrigues,FZtype,FZorder).eqv..TRUE.) then 
        call Message('  ---> THIS ROTATION LIES INSIDE THE FUNDAMENTAL ZONE.',"(A/)")
      end if 
    end if
  end do
end do

end program EMlistTC
