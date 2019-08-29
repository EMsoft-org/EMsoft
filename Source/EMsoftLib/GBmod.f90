! ###################################################################
! Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:GBmod.f90
!--------------------------------------------------------------------------
!
! MODULE: GBmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Grain Boundary algebra module, formally in terms of octonions, but implemented with quaternions
!
!> @date 04/20/18 MDG 1.0 original (based on work with Toby Francis)
!--------------------------------------------------------------------------
module GBmod

use local

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_minimal_U1_angle
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Compute the angle that will minimize a geodesic quaternion arc length with respect to U(1) symmetry
!
!> @details This routine works for a grain boundary octonion (GBO) pair in both the standard and grain
!> exchanged configurations
!
!> @param qa, qb, qc, qd  GBO rotation quaternions
!> @param exchange  (OPTIONAL) logical to indicate grain exchange when present and .TRUE.
!
!> @date 04/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_minimal_U1_angle(qa,qb,qc,qd,exchange)  result(zeta)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_minimal_U1_angle

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)         :: qa(4)
real(kind=dbl),INTENT(IN)         :: qb(4)
real(kind=dbl),INTENT(IN)         :: qc(4)
real(kind=dbl),INTENT(IN)         :: qd(4)
logical,INTENT(IN),OPTIONAL       :: exchange
real(kind=dbl)                    :: zeta

real(kind=dbl)					          :: nom, denom, mu

zeta = 0.D0

if (present(exchange)) then 
  if (exchange.eqv..TRUE.) then 
    nom = (qb(4)*qc(1)-qb(1)*qc(4)) + (qa(4)*qd(1)-qa(1)*qd(4)) + (qb(2)*qc(3)-qb(3)*qc(2)) + (qa(2)*qd(3)-qa(3)*qd(2))
    denom = sum(qb*qc) + sum(qa*qd)
	if ((denom.ne.0.D0).or.(nom.ne.0.D0)) then
      mu = 2.D0 * atan2(nom, denom)
	  if (mu.lt.0.D0) then
       zeta = 2.D0*cPi + mu
	  else
       zeta = mu
      end if 
    end if 
  end if 
else
  nom = (qa(4)*qc(1)-qa(1)*qc(4)) + (qb(4)*qd(1)-qb(1)*qd(4)) + (qa(2)*qc(3)-qa(3)*qc(2)) + (qb(2)*qd(3)-qb(3)*qd(2))
  denom = sum(qa*qc) + sum(qb*qd)
  if ((denom.ne.0.D0).or.(nom.ne.0.D0)) then
    mu = 2.D0 * atan2(nom, denom)
    if (mu.lt.0.D0) then
      zeta = 2.D0*cPi + mu
    else
      zeta = mu
    end if 
  end if 
end if 

end function GBO_minimal_U1_angle

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_minimal_U1_angle_NB
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Compute the angle that will minimize a geodesic quaternion arc length with respect to U(1) symmetry
!
!> @details This routine works for a grain boundary octonion (GBO) pair in both the standard and grain
!> exchanged configurations
!
!> @param qa, qb, qc, qd  GBO rotation quaternions
!> @param exchange  (OPTIONAL) logical to indicate grain exchange when present and .TRUE.
!
!> @date 04/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_minimal_U1_angle_NB(qa,qc)  result(zeta)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_minimal_U1_angle_NB

use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)         :: qa(4)
real(kind=dbl),INTENT(IN)         :: qc(4)
real(kind=dbl)                    :: zeta

real(kind=dbl)                    :: nom, denom, mu

zeta = 0.D0

nom = qa(3)*qc(2)-qa(2)*qc(3) 
denom = qa(2)*qc(2)+qa(3)*qc(3) 
if ((denom.ne.0.D0).or.(nom.ne.0.D0)) then
  mu = atan2(nom, denom)
  if (mu.lt.0.D0) then
    zeta = 2.D0*cPi + mu
  else
    zeta = mu
  end if 
end if 

end function GBO_minimal_U1_angle_NB

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_Omega
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Compute the S^7 geodesic arc length for a GBO pair, including U(1) symmetry, grain exchange and SO(3) double cover
!
!> @param qa, qb, qc, qd  GBO rotation quaternions
!
!> @date 04/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_Omega(qa,qb,qc,qd,noU1)  result(Omega)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_Omega

use constants
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)      :: qa(4)
!f2py intent(in,out) ::  qa
real(kind=dbl),INTENT(INOUT)      :: qb(4)
!f2py intent(in,out) ::  qb
real(kind=dbl),INTENT(INOUT)      :: qc(4)
!f2py intent(in,out) ::  qc
real(kind=dbl),INTENT(INOUT)      :: qd(4)
!f2py intent(in,out) ::  qd
logical,INTENT(IN),OPTIONAL       :: noU1
real(kind=dbl)                    :: Omega

real(kind=dbl)                    :: qq(4), zeta, sigma, cac, cbd, cbc, cad, cz, sz, cs, ss, &
                                     sum1, sum2, sum3, sum4, sums(4), smax
integer(kind=irg)                 :: isum(1)                                     

if (present(noU1)) then
  if (noU1.eqv..TRUE.) then
    cac = sum(qa*qc)
    cbd = sum(qb*qd)
    cbc = sum(qb*qc)
    cad = sum(qa*qd)
    sum1 = 0.5D0 * maxval( abs( (/ cac+cbd, cac-cbd /) ) )
    sum2 = 0.5D0 * maxval( abs( (/ cbc+cad, cbc-cad /) ) )
! and determine the smallest geodesic distance on S^7
    Omega = 2.0 * minval( (/ acos(sum1), acos(sum2) /) )
  end if
else
! determine the minimal U(1) angle for the (a,b) - (c,d) boundary pair
  zeta = GBO_minimal_U1_angle(qa,qb,qc,qd)
  cz = cos(zeta*0.5D0)
  sz = sin(zeta*0.5D0)
  qq = (/ qc(1)*cz-qc(4)*sz, cz*qc(2)+sz*qc(3), cz*qc(3)-sz*qc(2), cz*qc(4)+sz*qc(1) /)
  cac = sum(qa*qq)
  qq = (/ qd(1)*cz-qd(4)*sz, cz*qd(2)+sz*qd(3), cz*qd(3)-sz*qd(2), cz*qd(4)+sz*qd(1) /)
  cbd = sum(qb*qq)
  
  sum1 = 0.5D0 * maxval( abs( (/ cac+cbd, cac-cbd /) ) )

! determine the minimal U(1) angle for the (a,-b) - (c,d) boundary pair
  zeta = GBO_minimal_U1_angle(qa,-qb,qc,qd)
  cz = cos(zeta*0.5D0)
  sz = sin(zeta*0.5D0)
  qq = (/ qc(1)*cz-qc(4)*sz, cz*qc(2)+sz*qc(3), cz*qc(3)-sz*qc(2), cz*qc(4)+sz*qc(1) /)
  cac = sum(qa*qq)
  qq = (/ qd(1)*cz-qd(4)*sz, cz*qd(2)+sz*qd(3), cz*qd(3)-sz*qd(2), cz*qd(4)+sz*qd(1) /)
  cbd = sum(-qb*qq)
  
  sum3 = 0.5D0 * maxval( abs( (/ cac+cbd, cac-cbd /) ) )

! determine the minimal U(1) angle for the (b,a) - (c,d) boundary pair
  sigma = GBO_minimal_U1_angle(qa,qb,qc,qd,exchange=.TRUE.)
  cs = cos(sigma*0.5D0)
  ss = sin(sigma*0.5D0)
  qq = (/ qc(1)*cs-qc(4)*ss, cs*qc(2)+ss*qc(3), cs*qc(3)-ss*qc(2), cs*qc(4)+ss*qc(1) /)
  cbc = sum(qb*qq)
  qq = (/ qd(1)*cs-qd(4)*ss, cs*qd(2)+ss*qd(3), cs*qd(3)-ss*qd(2), cs*qd(4)+ss*qd(1) /)
  cad = sum(qa*qq)

  sum2 = 0.5D0 * maxval( abs( (/ cbc+cad, cbc-cad /) ) )

! determine the minimal U(1) angle for the (b,-a) - (c,d) boundary pair
  sigma = GBO_minimal_U1_angle(-qa,qb,qc,qd,exchange=.TRUE.)
  cs = cos(sigma*0.5D0)
  ss = sin(sigma*0.5D0)
  qq = (/ qc(1)*cs-qc(4)*ss, cs*qc(2)+ss*qc(3), cs*qc(3)-ss*qc(2), cs*qc(4)+ss*qc(1) /)
  cbc = sum(qb*qq)
  qq = (/ qd(1)*cs-qd(4)*ss, cs*qd(2)+ss*qd(3), cs*qd(3)-ss*qd(2), cs*qd(4)+ss*qd(1) /)
  cad = sum(-qa*qq)

  sum4 = 0.5D0 * maxval( abs( (/ cbc+cad, cbc-cad /) ) )

  sums = (/ sum1, sum2, sum3, sum4 /)
  smax = maxval(sums)
  isum = maxloc(sums)

! and determine the smallest geodesic distance on S^7
  Omega = 2.0 * acos(smax)

! do we also need to reorganize the quaternions depending on the value of isum?
end if

end function GBO_Omega

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_Omega_symmetric
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Compute the S^7 geodesic arc length for a GBO pair (U(1) symmetry, grain exchange and crystal symmetry)
!
!> @param qa, qb, qc, qd  GBO rotation quaternions
!
!> @date 04/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_Omega_symmetric(qa,qb,qc,qd,dict,solution,arclengths,single,noU1)  result(Omega)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_Omega_symmetric

use constants
use so3
use typedefs
use dictmod
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)      :: qa(4)
!f2py intent(in,out) ::  qa
real(kind=dbl),INTENT(INOUT)      :: qb(4)
!f2py intent(in,out) ::  qb
real(kind=dbl),INTENT(INOUT)      :: qc(4)
!f2py intent(in,out) ::  qc
real(kind=dbl),INTENT(INOUT)      :: qd(4)
!f2py intent(in,out) ::  qd
type(dicttype),INTENT(INOUT)      :: dict
!f2py intent(in,out) ::  dict
real(kind=dbl),INTENT(OUT),OPTIONAL :: solution(4,4)
real(kind=dbl),INTENT(OUT),OPTIONAL :: arclengths(dict%Nqsym**2,dict%Nqsym**2)
logical,INTENT(IN),OPTIONAL       :: single
logical,INTENT(IN),OPTIONAL       :: noU1
real(kind=dbl)                    :: Omega

integer(kind=irg)                 :: i, j, k, l
logical                           :: keep, arcs, skipU1
real(kind=dbl)                    :: smallest, Sqa(4), Sqb(4), Sqc(4), Sqd(4), x

skipU1 = .FALSE.
if (present(noU1)) then
  skipU1 = .TRUE.
end if

keep = .FALSE.
if (present(solution)) then
  keep = .TRUE.
end if 

arcs = .FALSE.
if (present(arclengths)) then
  arcs = .TRUE.
  write (*,*) 'input array : ',shape(arclengths)
end if 
smallest = 100.D0

if (dict%Nqsym.eq.1) then
  if (skipU1.eqv..TRUE.) then
    smallest = GBO_Omega(qa,qb,qc,qd,noU1=.TRUE.)
  else
    smallest = GBO_Omega(qa,qb,qc,qd)
  end if
  if (keep) then 
    solution(1:4,1) = qa
    solution(1:4,2) = qb
    solution(1:4,3) = qc
    solution(1:4,4) = qd
  end if 
  if (arcs) then
    arclengths(1,1) = smallest
  end if
else
  if (present(single)) then 
    if (single.eqv..TRUE.) then
      do k=1,dict%Nqsym
        Sqc = quat_mult(dict%Pm(1:4,k),qc)
        if (Sqc(1).lt.0.D0) Sqc = -Sqc
        do l=1,dict%Nqsym
          Sqd = quat_mult(dict%Pm(1:4,l),qd)
          if (Sqd(1).lt.0.D0) Sqd = -Sqd
          if (skipU1.eqv..TRUE.) then  
            x = GBO_Omega(qa,qb,Sqc,Sqd,noU1=.TRUE.)
          else
            x = GBO_Omega(qa,qb,Sqc,Sqd)
            end if
          if (arcs) then 
            arclengths((i-1)*dict%Nqsym+j,(k-1)*dict%Nqsym+l) = x
          end if
          if (x.lt.smallest) then 
            smallest = x
            if (keep) then 
              solution(1:4,1) =  qa
              solution(1:4,2) =  qb
              solution(1:4,3) = Sqc
              solution(1:4,4) = Sqd
            end if 
          end if
        end do
      end do
    end if 
  else
    do i=1,dict%Nqsym
      Sqa = quat_mult(dict%Pm(1:4,i), qa)
      do j=1,dict%Nqsym
        Sqb = quat_mult(dict%Pm(1:4,j), qb)
        do k=1,dict%Nqsym
          Sqc = quat_mult(dict%Pm(1:4,k), qc)
          do l=1,dict%Nqsym
            Sqd = quat_mult(dict%Pm(1:4,l), qd)
            if (skipU1.eqv..TRUE.) then  
             x = GBO_Omega(Sqa,Sqb,Sqc,Sqd,noU1=.TRUE.)
            else
              x = GBO_Omega(Sqa,Sqb,Sqc,Sqd)
            end if
            if (arcs) then 
              arclengths((i-1)*dict%Nqsym+j,(k-1)*dict%Nqsym+l) = x
            end if
            if (x.lt.smallest) then 
              smallest = x
              if (keep) then 
                solution(1:4,1) = Sqa
                solution(1:4,2) = Sqb
                solution(1:4,3) = Sqc
                solution(1:4,4) = Sqd
              end if 
            end if
          end do
        end do
      end do
    end do
  end if 
end if 

Omega = smallest

end function GBO_Omega_symmetric

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_Omega_NB
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Compute the S^7 geodesic arc length for a no-boundary GBO pair, including grain exchange
!
!> @param qa, qc  GBO rotation quaternions
!
!> @date 04/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_Omega_NB(qa,qc)  result(Omega)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_Omega_NB

use constants
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)      :: qa(4)
!f2py intent(in,out) ::  qa
real(kind=dbl),INTENT(INOUT)      :: qc(4)
!f2py intent(in,out) ::  qc
real(kind=dbl)                    :: Omega

real(kind=dbl)                    :: qq(4), pp(4), zeta, sigma, cac, cbc, cz, sz, cs, ss

qq = quat_mult(qa, conjg(qc))
pp = quat_mult(qc, conjg(qa))
Omega = minval( (/ 2.D0 * acos(abs(qq(1))), 2.D0*acos(abs(pp(1))) /) )

end function GBO_Omega_NB

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_Omega_symmetric_NB
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Compute the S^7 geodesic arc length for a GBO pair (U(1) symmetry, grain exchange and crystal symmetry)
!
!> @param qa, qc  GBO rotation quaternions
!
!> @date 04/20/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_Omega_symmetric_NB(qa,qc,dict)  result(Omega)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_Omega_symmetric_NB

use constants
use so3
use typedefs
use dictmod
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)      :: qa(4)
!f2py intent(in,out) ::  qa
real(kind=dbl),INTENT(INOUT)      :: qc(4)
!f2py intent(in,out) ::  qc
type(dicttype),INTENT(INOUT)      :: dict
!f2py intent(in,out) ::  dict
real(kind=dbl)                    :: Omega

integer(kind=irg)                 :: i, k
real(kind=dbl)                    :: smallest, Sqa(4), Sqc(4), x


if (dict%Nqsym.eq.1) then
  smallest = GBO_Omega_NB(qa,qc)
else
  smallest = 1000.D0
  do i=1,dict%Nqsym
    Sqa = quat_mult(dict%Pm(1:4,i),qa)
    if (Sqa(1).lt.0.D0) Sqa = -Sqa
    do k=1,dict%Nqsym
      Sqc = quat_mult(dict%Pm(1:4,k),qc)
      if (Sqc(1).lt.0.D0) Sqc = -Sqc
      x = GBO_Omega_NB(Sqa,Sqc)
      if (x.lt.smallest) smallest = x
    end do
  end do
end if 

Omega = smallest

end function GBO_Omega_symmetric_NB

!--------------------------------------------------------------------------
!
! FUNCTION: GBO_SLERP
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief interpolate between two hyper-complex numbers
!
!> @param hcn1, hcn2  quaternions or octonions
!> @param Omega  misorientation angle
!> @param t interpolation parameter in [0,1]
!> @param n  4 or 8
!
!> @date 05/05/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GBO_SLERP(hcn1, hcn2, Omega, t, n)  result(hcnt)
!DEC$ ATTRIBUTES DLLEXPORT :: GBO_SLERP

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: n
real(kind=dbl),INTENT(IN)             :: hcn1(n)
real(kind=dbl),INTENT(IN)             :: hcn2(n)
real(kind=dbl),INTENT(IN)             :: Omega
real(kind=dbl),INTENT(IN)             :: t
real(kind=dbl)                        :: hcnt(n)

real(kind=dbl)                        :: st, sp, sm, theta
 
theta = Omega*0.5D0

st = sin(theta)
sp = sin(t*theta)
sm = sin((1.D0-t)*theta)

hcnt = hcn1 * sm/st + hcn2 * sp/st

end function GBO_SLERP

!--------------------------------------------------------------------------
!
! FUNCTION: GB_getCSLrod
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief get the CSL Rodrigues vector and sequential number for a given input CSL type
!
!> @param CSLlabel
!> @param CSLnumber
!
!> @date 05/07/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GB_getCSLrod(CSLlabel, CSLnumber)  result(rod)
!DEC$ ATTRIBUTES DLLEXPORT :: GB_getCSLrod

use typedefs
use error

IMPLICIT NONE

character(3),INTENT(IN)         :: CSLlabel
integer(kind=irg),INTENT(OUT)   :: CSLnumber
real(kind=dbl)                  :: rod(4)

integer(kind=irg)               :: i
real(kind=dbl)                  :: l

! first find the sequential number for this boundary 
CSLnumber = 0
do i=1, CSLnumberdefined
  if (trim(CSLlabels(i)).eq.trim(CSLlabel)) CSLnumber = i
end do

if (CSLnumber.eq.0) then
  call FatalError('GB_getCSLrod','requested CSL type not recognized')
end if

rod = 0.D0

rod(1:3) = (/ dble(CSLintegers(1,CSLnumber))/dble(CSLintegers(2,CSLnumber)), &
              dble(CSLintegers(3,CSLnumber))/dble(CSLintegers(4,CSLnumber)), &
              dble(CSLintegers(5,CSLnumber))/dble(CSLintegers(6,CSLnumber)) /)

l = NORM2(rod(1:3))
rod(1:3) = rod(1:3)/l
rod(4) = l

end function GB_getCSLrod


!--------------------------------------------------------------------------
!
! FUNCTION: GB_getGrainNormalVector
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief for a given Lambert sampling point, determine the representation in the Olmsted approach
!
!> @param 
!> @param 
!
!> @date 06/21/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GB_getGrainNormalVector(xyz, f, pos)  result(nvec)
!DEC$ ATTRIBUTES DLLEXPORT :: GB_getGrainNormalVector

use typedefs
use error
use Lambert

IMPLICIT NONE

real(kind=dbl),INTENT(IN)              :: xyz(3)
real(kind=dbl),INTENT(IN)              :: f
real(kind=dbl),INTENT(OUT)             :: pos(3)
real(kind=dbl)                         :: nvec(3)

integer(kind=irg)                      :: p 
real(kind=dbl)                         :: xyz1(3), xyz3(3), newn(3), ppos(3)
real(kind=dbl)                         :: d, theta, phi, ct, st, tt, cp, sp, gth, gthp, s, sg

newn = (/ 0.D0, 0.D0, 0.D0 /)
ppos = newn

! determine pyramid
if (GetPyramidDouble(xyz).eq.1) then

  ! get the spherical angles theta and phi
  theta = acos(xyz(3))
  phi = atan2(xyz(2),xyz(1))

  if (theta.eq.0.D0) then
    nvec = (/ 0.D0, 0.D0, 1.D0 /)
    ppos = (/ 0.D0, 0.D0, 1.D0 + (sqrt(3.D0)-1.D0)*f /)
  else
  ! derived quantities
    ct = cos(theta)
    st = sin(theta)
    tt = tan(theta)
    cp = cos(phi)
    sp = sin(phi)
    s = sqrt(3.D0) - 1.D0/ct
    sg = 1.D0
    if (s.lt.0.D0) sg = -1.D0
    gth =  abs(s)
    gthp = -sg * tt / ct
    s = f * (1.D0+f*ct*gth) * tt * (gth*st - ct*gthp)
    nvec(1) = s * cp
    nvec(2) = s * sp
    nvec(3) = (1.D0+f*ct*gth) *  tt * (1.D0 + f*ct**3*gth + f*ct**2*st*gthp) / ct**2
    nvec = nvec / Norm2(nvec)
    pos = (/ f*cp*gth*st + cp*tt, f*sp*gth*st + sp*tt, 1.D0+f*ct*gth/) 
  end if
end if 

end function GB_getGrainNormalVector

!--------------------------------------------------------------------------
!
! FUNCTION: GB_getOlmstedRepresentation
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief for a given Lambert sampling point, determine the representation in the Olmsted approach
!
!> @param 
!> @param 
!
!> @date 06/21/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function GB_getOlmstedRepresentation(nvec, pA, qB, verbose)  result(qA)
!DEC$ ATTRIBUTES DLLEXPORT :: GB_getOlmstedRepresentation

use typedefs
use error
use Lambert
use quaternions 

IMPLICIT NONE

real(kind=dbl),INTENT(IN)              :: nvec(3)
real(kind=dbl),INTENT(IN)              :: pA(4)
real(kind=dbl),INTENT(OUT)             :: qB(4)
logical,INTENT(IN),OPTIONAL            :: verbose
real(kind=dbl)                         :: qA(4)

real(kind=dbl)                         :: phi, rho(4), msA(3), d, s
logical                                :: v 

v = .FALSE.
if (present(verbose)) then 
  if (verbose.eqv..TRUE.) then 
    v = .TRUE.
  end if 
end if

! transform the plane normal nvec (=mA) to the sample reference frame
msA = quat_Lp(conjg(pA),nvec)
! msA = quat_Lp(pA,nvec)
if (v) write (*,*) nvec, conjg(pA), msA 

! get the rotation angle and quaternion rho that bring msA onto the sample z-axis
phi = acos(msA(3))
if (abs(msA(3)).eq.1.D0) then
  rho = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
else
  s = sin(phi*0.5D0)
  d = sqrt(msA(1)**2+msA(2)**2)
  rho = (/ cos(phi*0.5D0), s * msA(2)/d, -s*msA(1)/d, 0.D0 /)
end if
if (v) write (*,*) phi, d, rho 

! prepare the return variables
qB = rho
qA = quat_mult(pA, rho)
if (v) write (*,*) qA, qB 

end function GB_getOlmstedRepresentation

end module GBmod
