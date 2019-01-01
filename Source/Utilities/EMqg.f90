! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMqg.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMqg 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the complex extinction distance q_g
!
!> @date   10/13/98 MDG 1.0 original
!> @date   05/27/01 MDG 2.0 f90
!> @date   04/16/13 MDG 3.0 rewrite
!> @date   11/26/16 MDG 3.1 added handling of systematic absences
!--------------------------------------------------------------------------
program EMqg

use local
use typedefs
use io
use HDFsupport
use crystal
use files
use diffraction
use symmetry
use constants
use postscript

IMPLICIT NONE

integer(kind=irg)              :: ind(3),ans, oi_int(3)
real(kind=sgl)                 :: oi_real(7)
complex(kind=sgl)              :: oi_cmplx(1)
real(kind=sgl)                 :: preg, dmin, gstepsize
real(kind=dbl)				   :: eps = 1.0D-6
character(fnlen)               :: progname, progdesc, gname
character(200)                 :: parta
type(unitcell),pointer         :: cell
logical                        :: isallowed
type(gnode)                    :: rlp

 progname = 'EMqg.f90'
 progdesc = 'Display potential coefficient values'
 call EMsoft(progname, progdesc)

 allocate(cell)
 
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = trim(gname)
 call CrystalData(cell)

 call GetVoltage(cell,rlp)
 call CalcPositions(cell,'v')
 preg = 2.0 * sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18

! temporary code for testing
 dmin = 0.05
 gstepsize = 0.001
 call PreCalcFSCATT(cell, dmin, gstepsize)
! end of temporary code

 ans = 1
 do while (ans.eq.1)
  call Message('Enter Miller indices :', frm = "(/A)")
  call GetIndex(cell%hexset,ind,'r')
! call CalcUcg(cell, rlp, ind)
  rlp%method = 'IP'
  call CalcUcg(cell, rlp, ind,interpolate=.TRUE.)
  isallowed = IsGAllowed(cell,ind)

! check whether this is a lattice extinction, a symmetry extinction, or an allowed reflection
  if ((rlp%Umod.lt.eps).and.(isallowed.eqv..FALSE.)) then
    call Message('This reflection is absent due to lattice centering.')
    CYCLE
  end if
  if ((rlp%Umod.lt.eps).and.(isallowed.eqv..TRUE.)) then
    call Message('This reflection is absent due to glide or screw symmetry elements.')
    CYCLE
  end if
  if (rlp%Umod.ne.0.D0) then
    parta = '   h  k  l    |g|    Ucg_r     Ucg_i      |Ug|      phase'// &
            '     |Ugp|     phase     xi_g   xi_gp    ratio    Re-1/q_g-Im'  
    call Message(parta, frm = "(200A)") 

    oi_int(1:3) = rlp%hkl(1:3)
    call WriteValue('',oi_int, 3, "(1x,3I3,1x,$)")
    oi_real(1) = rlp%g
    call WriteValue('',oi_real, 1, "(F9.4,$)")
    oi_cmplx(1) = rlp%Ucg
    call WriteValue('',oi_cmplx, 1, "(2F10.6,1x,$)")
    oi_real(1:7)  = (/ rlp%Umod,rlp%Vphase*180.0/sngl(cPi),rlp%Upmod,rlp%Vpphase*180.0/sngl(cPi),rlp%xg,rlp%xgp,rlp%ar /)
    call WriteValue('',oi_real, 7, "(4F10.5,3F8.1,$)")
    oi_cmplx(1) = rlp%qg
    call WriteValue('',oi_cmplx, 1, "(2F8.5)")
  end if

  call ReadValue(' Another one ? (1/0) : ', oi_int, 1)
  ans = oi_int(1)
 end do 

end  program EMqg
       



