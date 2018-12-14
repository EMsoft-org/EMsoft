! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:EMDisorientationsTwoPhase.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMDisorientationsTwoPhase
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief program to compute disorientations between two lists of Euler angles
!
!> @date 07/04/16 MDG 1.0 original
!--------------------------------------------------------------------------
program EMDisorientationsTwoPhase

use local
use typedefs
use NameListTypeDefs
use NameListHandlers
use dictmod
use files
use quaternions
use Lambert
use constants
use rotations
use so3
use error

IMPLICIT NONE

type(DisorientationsNameListType):: enl

integer(kind=irg)               :: nump, i, j, k, numt, nn, types(11), ii, jj, np, dimx, dimy, nr, FZtype, FZorder, Pmdims, &
                                   FZcnt, nume, N, iclr, get_color, it_max, it_num, rot_num, nx, CMcnt2, kk

real(kind=dbl),allocatable      :: av(:,:,:)
integer(kind=irg),allocatable   :: inside(:,:,:,:), im(:,:), output(:,:), indices(:,:)
real(kind=dbl)                  :: qu(4), s, Mu(4), DD, cuc(3), quc(4), cu(3), rod(4), eu(3), Mus(4), qus(4), p(4), a, ac, dx, &
                                   ome, omer, rhozero(4), sig1, sig2, sig3, evec(3,3), mismu, misang, rhovec(3), vvec(3), s2,  &
                                   diag(3,3), om(3,3), Mscale(4,4), Mrotate(4,4), Mtranslate(4,4), Mtrans(4,4), vx(3), vy(3), &
                                   vz(3), vv(4), misangr, x, y, z, x0, vx1(3), vx2(3), vx3(3), vx4(3), vx5(3), vx6(3), Tmat(4,4), &
                                   omegad, misang1, misang2, rvec(4), MO(200,200), disor, val,eu1(3),eu2(3),eu3(3),eu4(3),eu5(3),&
                                   eu6(3), rodr(3)
character(10)                   :: fnames(11)
real(kind=sgl)                  :: mi, ma, rhox, rhoy, rhoz, rho, omega, mumu, s1, s3, prev, alpha, beta, rad,e1,e2,e3
real(kind=dbl),allocatable      :: iminput(:,:), imoutput(:,:), LUT(:,:) 
real(kind=dbl),allocatable      :: qar(:,:)
type(dicttype),pointer          :: dict1, dict2
real(kind=dbl),allocatable      :: euarray(:,:), miso(:,:), eulocal(:,:,:)
 
real(kind=sgl)                  :: pattern(512,512)
real(kind=dbl)                  :: ksq(512,512), jres, Q, pi, distance, cxp, cyp, czp,  dy, delta1, delta2, XY(2)
logical                         :: init, fexists
character(fnlen)                :: path, stin, stin2, cwd, fname, arg, progname, progdesc, dirstring, dirname
integer(kind=irg)               :: narg, hdferr, CMcnt, nsteps, pgnum

character(fnlen)                :: confname, emsoftname, jsonname, jsonfilename, nmldeffile, prefix, extension

real(kind=dbl)                  :: edge, xc, yc, zc

nmldeffile = 'EMDisorientations.nml'
progname = 'EMDisorientations.f90'
progdesc = 'Compute disorientations between two lists of Euler angles'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 82 /), progname)

! deal with the namelist stuff
call GetDisorientationsNameList(nmldeffile,enl)

! allocate the dict structures
nullify(dict1,dict2)
allocate(dict1)
dict1%Num_of_init = 3
dict1%Num_of_iterations = 30
dict1%pgnum = enl%pgnum

allocate(dict2)
dict2%Num_of_init = 3
dict2%Num_of_iterations = 30
dict2%pgnum = enl%pgnum2

! initialize the symmetry matrices
call DI_Init(dict1,'nil') 
call DI_Init(dict2,'nil') 

fname = trim(EMsoft_getEMdatapathname())//trim(enl%inputfile)
fname = EMsoft_toNativePath(fname)

open(unit=10,file=trim(fname),status='unknown',form='formatted')
read(10,"(I10)") nump
write (*,*) 'number of patterns detected : ',nump

allocate(LUT(10,nump))
do i=1,nump
  read(10,*) LUT(1:6,i)
end do
close(10,status='keep')

LUT = LUT*cPi/180.0

! temporary code
! generate a file with just the g_beta (g_alpha)^(-1) misorientations, no symmetry reduction
open(unit=10,file=trim('rawmisorientations2.txt'),status='unknown',form='formatted')
write(10,"(A2)") 'ro'
write(10,"(I6)") nump

do i=1,nump
  qu = eu2qu(LUT(1:3,i))
  Mu = eu2qu(LUT(4:6,i))
  qu = quat_mult(qu,conjg(Mu))
  if (qu(1).lt.0.0) qu = -qu
  rod = qu2ro(qu)

! sign check, to make sure we end up in the correct FZ portion (1/4th of the full cell)
    if (product(rod(1:3)).ge.0.D0) then
      rod(1:3) = abs(rod(1:3))
    else
      rod(1:3) = abs(rod(1:3))
      rod(1) = -rod(1)
    end if
    write (10,"('sphere { <',F10.6,',',F10.6,',',F10.6,'>,0.002 pigment { Red }}')") rod(1:3)*rod(4)
end do
close(10,status='keep')

! and here we compute the disorientation angle from a quaternion product...
do i=1,nump
  call getDisorientationAngleAxisTwoPhases(LUT(1:3,i), LUT(4:6,i), dict1, dict2, LUT(7:10,i))
    rod = ax2ro(LUT(7:10,i))
! sign check, to make sure we end up in the correct FZ portion (1/4th of the full cell)
    if (product(rod(1:3)).ge.0.D0) then
      rod(1:3) = abs(rod(1:3))
    else
      rod(1:3) = abs(rod(1:3))
      rod(1) = -rod(1)
    end if
    LUT(7:10,i) = ro2ax(rod)
end do

fname = trim(EMsoft_getEMdatapathname())//trim(enl%outputfile)
fname = EMsoft_toNativePath(fname)

open(unit=10,file=trim(fname),status='unknown',form='formatted')
write(10,"(I7)") nump

do i=1,nump
  rod = ax2ro(LUT(7:10,i))
  write(10,"(4F12.6)") rod
end do
close(10,status='keep')

write (*,*) 'program completed successfully'


end program EMDisorientationsTwoPhase
