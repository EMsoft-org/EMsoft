! ###################################################################
! Copyright (c) 2015-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMeqvrot.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMeqvrot
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief For a given crystal symmetry (point group number), compute equivalent orientations 
!
!> @date 09/24/16 MDG 1.0 original
!> @date 12/22/16 MDG 1.1 added indication of orientation that lies inside the Rodrigues FZ
!> @date 02/10/17 MDG 1.2 added option for output representation selection
!--------------------------------------------------------------------------
program EMeqvrot

use local
use constants
use symmetry
use dictmod
use rotations
use quaternions
use io
use so3
use files

IMPLICIT NONE

integer(kind=irg)           :: pgnum, Rtype, Otype, io_int(1), num, k, FZtype, FZorder
real(kind=dbl)              :: ro(4), rod(3), ax(4), qu(4), io_dbl(4), qus(4), io_dbl3(3), dtor, eu(3)
type(dicttype),pointer      :: dict
logical                     :: next
character(fnlen)            :: progname, progdesc
type(orientationtyped)      :: res, saveres

progname = 'EMeqvrot.f90'
progdesc = 'List equivalent rotations'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(1,(/ 911 /), progname)


dtor = cPi/180.D0

! ask for point group number
call ListPointGroups
call ReadValue('Enter the point group number ',io_int)
pgnum = io_int(1)

! what format are we using for the input?
call ReadValue('Input representation: Rodrigues (1), axis-angle (2), quaternion (3), or Euler (4) ',io_int)
Rtype = io_int(1)
call ReadValue('Output representation: Rodrigues (1), axis-angle (2), quaternion (3), or Euler (4) ',io_int)
Otype = io_int(1)
! allocate the symmetry operations, among other things.
allocate(dict)
dict%Num_of_init = 1 
dict%Num_of_iterations = 1
dict%pgnum = pgnum
call DI_Init(dict,'VMF')
num = dict%Nqsym

FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

! then in a loop ask for an orientation in Rodrigues form or axis angle
! and list all the equivalent orientations as axis angle pairs
next = .TRUE.
do while (next) 
  select case (Rtype)
    case (1) 
      call ReadValue('Rodrigues vector (nx, ny, nz, |R|) :', io_dbl,4)
      ro = io_dbl
      qu = ro2qu(ro)
    case (2) 
      call ReadValue('Axis-angle pair (nx, ny, nz, omega°) :', io_dbl,4)
      ax = io_dbl
      ax(4) = ax(4) * cPi/180.D0
      qu = ax2qu(ax)
    case (3) 
      call ReadValue('Quaternion (q0, q1, q2, q3) :', io_dbl,4)
      qu = io_dbl
    case (4) 
      call ReadValue('Euler (phi1°, Phi°, phi2°) :', io_dbl3,3)
      qu = eu2qu(io_dbl3*dtor)
    case default
  end select
  call Message(' ')

  do k=1,num
    qus = quat_mult( dict%Pm(1:4,k), qu )
    if (qus(1).lt.0.0) qus = -qus
    res = init_orientation(qus,'qu')
    ro = res%rodrigues
    ax = res%axang
    ax(4) = ax(4) *180.D0/cPi
    if (IsinsideFZ(ro,FZtype,FZorder).eqv..TRUE.) then
      saveres = res
      select case(Otype)
        case(1)
          call WriteValue('FZ  ', ro, num=4)
        case(2)
          call WriteValue('FZ  ', ax, num=4)
        case(3)
          call WriteValue('FZ  ', qus, num=4)
        case(4)
          eu = res%eulang/dtor
          call WriteValue('FZ  ', eu, num=3)
        case default
      end select
    else
      select case(Otype)
        case(1)
          call WriteValue('    ', ro, num=4)
        case(2)
          call WriteValue('    ', ax, num=4)
        case(3)
          call WriteValue('    ', qus, num=4)
        case(4)
          eu = res%eulang/dtor
          call WriteValue('    ', eu, num=3)
        case default
      end select
    end if
  end do

! write all the equivalent representations for the one in the FZ
  call Message(' ')
  call Message('Equivalent representations for the rotation inside the FZ:')
  call print_orientation(saveres)
  call Message(' ')
  call ReadValue('---> Another one ? (1/0) ',io_int)
  if (io_int(1).eq.0) next=.FALSE.
end do


















end program EMeqvrot
