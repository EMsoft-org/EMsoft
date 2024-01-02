! ###################################################################
! Copyright (c) 2015-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMeqvPS.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMeqvPS
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief For a given crystal symmetry and a pseudo-symmetry operation, list the independent equivalent operators
!
!> @date 03/19/18 MDG 1.0 original
!--------------------------------------------------------------------------
program EMeqvPS

use local
use constants
use dictmod
use rotations
use quaternions
use io
use so3
use files 

IMPLICIT NONE

integer(kind=irg)           :: pgnum, io_int(1), num, k, FZtype, FZorder, i, j
real(kind=dbl)              :: ro(4), rod(3), ax(4), qu(4), io_dbl(4), qus(4), io_dbl3(3), dtor, eu(3), &
                               eu1(3), eu2(3), qu1(4), qu2(4), diff
real(kind=dbl),allocatable  :: axlist(:,:), eulerlist(:,:)
integer(kind=irg),allocatable :: unique(:)
type(dicttype),pointer      :: dict
character(fnlen)            :: progname, progdesc

progname = 'EMeqvPS.f90'
progdesc = 'List equivalent pseudo-symmetric rotations'

! deal with the command line arguments, if any
call Interpret_Program_Arguments(1,(/ 910 /), progname)


! print some information
call EMsoft(progname, progdesc)

dtor = cPi/180.D0

! ask for point group number
call ReadValue('Enter the point group number ',io_int)
pgnum = io_int(1)

! allocate the symmetry operations, among other things.
allocate(dict)
dict%Num_of_init = 1 
dict%Num_of_iterations = 1
dict%pgnum = pgnum
call DI_Init(dict,'nil')
num = dict%Nqsym

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

! read the pseudo-symmetric axis angle pair
call ReadValue('Enter the pseudo-symmetry axis-angle pair (axis, angle°) ',io_dbl,4)
ax(1:4) = io_dbl(1:4)

! normalize the vector part and convert the angle to radians
ax(1:3) = ax(1:3) / vecnorm(ax(1:3))
ax(4) = ax(4) *cPi/180.D0 

! convert to quaternion
qu1 = ax2qu(ax)

! for all symmetry quaternions Pm, compute the quaternion corresponding to the rotated rotation axis of ax;
! the rotation angle remains the same for all of them
allocate(axlist(4,num))
do j=1,num
  axlist(1:4,j) = (/ quat_Lp(dict%Pm(1:4,j), ax(1:3)), ax(4) /)
end do

! take a random orientation, reduce it to the fundamental zone, and convert to quaternion q2
eu2 = (/ 94.43, 102.02, 228.77 /) * cPi/180.0
call ReduceOrientationtoRFZ(eu2, dict, FZtype, FZorder, eu1)
qu2 = eu2qu(eu1)

! apply this symmetrized pseudo-symmetry operators to the random orientation, and store them in an Euler list
allocate(eulerlist(3,num), unique(num))
unique = 1
do j=1,num
    qu = ax2qu(axlist(1:4,j))
    if (qu(1).lt.0.D0) qu = -qu
    eu = qu2eu(quat_mult(qu,qu2))
    call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eulerlist(1:3,j))
end do

! analyze this list for uniqueness, keeping only a single copy of each unique operator
do i=1,num-1 
  do j=i+1, num
    if (unique(i).eq.1) then 
      diff = sum(abs(eulerlist(1:3,i)-eulerlist(1:3,j)))
      if (diff.lt.0.01) unique(j) = 0
    end if 
  end do 
end do 

! and generate the list of pseudo-symmetric variants
call Message(' ')
call Message('Unique pseudo-symmetry operators (axis,angle°):')
do i=1,num
  if (unique(i).eq.1) then 
    axlist(4,i) = axlist(4,i) * 180.0/cPi
    io_dbl(1:4) = axlist(1:4,i)
    call WriteValue('',io_dbl,4,"(4(F12.8,'  '))")
  end if 
end do 

end program EMeqvPS
