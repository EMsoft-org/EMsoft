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
! EMsoft:diffractionQC.f90
!--------------------------------------------------------------------------
!
! MODULE: diffractionQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Everything that has to do with diffraction calculation for quasicrystals
! 
!> @date  06/25/18   SS 1.0 original
!--------------------------------------------------------------------------
module diffractionQC

use local
use typedefs
use QCmod

IMPLICIT NONE

public

interface QC_CalcWaveLength
	module procedure QC_CalcWaveLength2DQC
	module procedure QC_CalcWaveLength3DQC
end interface QC_CalcWaveLength

interface QC_getUcg
	module procedure QC_getUcg2DQC
	module procedure QC_getUcg3DQC
end interface QC_getUcg

interface QC_Calcsg
	module procedure QC_Calcsg2DQC
	module procedure QC_Calcsg3DQC
end interface QC_Calcsg

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_getUcg2DQC
!
!> @author Marc De Graef/Saransh Singh, Carnegie Mellon University
!
!> @brief compute the interaction coupling constants Ucg and qg for this reflection
!
!> @details For now, in this first version of the QC codes, we use the simplistic
!> primitive hypercubic structure model from Veit Elser's paper; we'll need to expand 
!> on this routine to be able to simulate more realistic QC structures.
!
!> @param QCcell QC structure pointer
!> @param hkl Miller indices
!> @param qg interaction coupling constant
!> @param Vmod, Vpmod, xig, and xgp are optional output parameters
!
!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  01/10/14 MDG 4.0 account for new version of cell type
!> @date  06/09/14 MDG 4.1 added rltail and cell as arguments
!> @date  06/17/14 MDG 4.2 modification for separate reflist pointers
!> @date  09/08/15 MDG 4.3 added qg entry
!> @date  03/15/17 MDG 4.3 copied from gvectors module for QCmod adaptation
!> @date  03/21/18 SS  4.4 copied from QCmod module for 2-D QC computations
!> @date  06/25/18 SS  4.5 moved to diffractyionQC module
!--------------------------------------------------------------------------
recursive function QC_getUcg2DQC(QCcell, hkl, qg, Vmod, Vpmod, xig, xgp) result(Ucg)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_getUcg2DQC

use error
use constants
use others

IMPLICIT NONE

type(TDQCStructureType),pointer      :: QCcell
integer(kind=irg),INTENT(IN)         :: hkl(5)               !< QC Miller indices of reflection 
complex(kind=dbl),INTENT(OUT)        :: qg
real(kind=dbl),INTENT(OUT)           :: Vmod
real(kind=dbl),INTENT(OUT)           :: Vpmod
real(kind=dbl),INTENT(OUT)           :: xig
real(kind=dbl),INTENT(OUT)           :: xgp
complex(kind=dbl)                    :: Ucg 

integer(kind=irg)                    :: i,j,absflg,m,ii
real(kind=sgl)                       :: s,twopi,arg,swk,dwwk,pref,ul,pre,preg,sct,fs,fsp, g, go, p, q, &
                                        Umod, Upmod, Vphase, Vpphase, multiplicity
complex(kind=sgl)                    :: ff,gg,sf,p1,pp
complex(kind=sgl)                    :: czero
logical                              :: accflg, dwflg
character(2)                         :: smb

twopi = sngl(2.D0*cPi)
czero = cmplx(0.0,0.0)

! we'll only use the Weickenmeier-Kohl scattering parameters here since this
! allows us to also get the absorption part of the Fourier coefficients
! some of this code is directly adapted from the CalcUcg routine in diffraction.f90

! first we need to get the scattering parameter

! compute the scattering parameter s^2=(g/2)^2
if (sum(hkl**2).eq.0) then 
  s = 0.0
  g = 0.0
  go = 0.0
else
  g = QC_getvectorLength(QCcell, hkl, 'P', 'r')
  go = QC_getvectorLength(QCcell, hkl, 'O', 'r')
  s = (0.50*g)**2
end if

! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
swk = 0.1*twopi
dwwk = 100.0/(8.0*cPi**2)

! properly scale the scattering parameter
s = g*swk

! let fscatt perform the relativistic corrections for f_g and fprime_g
accflg = .TRUE.

! include absorption ?
absflg = 3  ! include phonon and core contributions

! always include Debye-Waller factor
dwflg  = .TRUE.

! compute the scaling prefactors
! pref contains A to nm conversion, and divides by 4pi
! WE NEED TO FIGURE OUT WHAT TO DO FOR THE UNIT CELL VOLUME !!!
! pref = 0.04787801/QCcell % vol/(4.0*cPi) 
pref = 0.04787801/(4.0*cPi)/(QCcell%vol)

! preg is used to go from V to U, remembering that gamma is already
! included in the output from fscatt
preg = 2.0 * sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18
pre = pref * preg

! initialize the real and imaginary parts of the structure factor
ff=czero
gg=czero 

do m = 1,QCcell%ATOM_ntype

  ul = sqrt(QCcell%ATOM_pos(m,7)*dwwk + QCcell%ATOM_pos(m,8)*dwwk)

  j  = QCcell%ATOM_type(m)

  sf = FSCATT(s,ul,j,smb,sngl(QCcell%voltage),absflg,accflg,dwflg) * QCcell%ATOM_pos(m,6)

  pp = ShapeTransformPolygonCa(QCcell, hkl, m)

  sf = sf * cabs(pp)

  ! loop over all atoms in the orbit
  p1 = czero
  do j = 1,QCcell%numat(m)
   arg = twopi*sum(float(hkl(1:5))*QCcell%apos(m,j,1:5))
   p1  = p1 + exp(cmplx(0.0,-arg))
  end do

  ff = ff + p1*real(sf) 
  gg = gg + p1*aimag(sf)  

end do

! these are the modulus and phase of the real part of Vg
Vmod = pref * cabs(ff)
Vphase = atan2(aimag(ff),real(ff))

! modulus of U_g
Umod = preg*Vmod

! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g
Vpmod = pref * cabs(gg)
Vpphase = atan2(aimag(gg),real(gg))

! modulus of Uprime_g
Upmod = preg*Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))

! complex Vg 
if (QCcell%mLambda.ne.-1.0) then
 if (abs(Umod).gt.0.0) then 
  xig = 1.0/abs(Umod)/QCcell%mLambda
 else
  xig = 1.0E+8
 end if 

 if (abs(Upmod).gt.0.0) then 
  xgp = 1.0/abs(Upmod)/QCcell%mLambda
 else
  xgp = 1.0E+8
 end if 

 arg = Vpphase - Vphase
 qg  = cmplx(cos(Vphase)/xig-sin(Vpphase)/xgp,cos(Vpphase)/xgp+sin(Vphase)/xig)
 !qg = cmplx(1.0/xig-sin(arg)/xgp,cos(arg)/xgp)
end if

end function QC_getUcg2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_getUcg3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the interaction coupling constants Ucg and qg for this reflection
!
!> @details For now, in this first version of the QC codes, we use the simplistic
!> primitive hypercubic structure model from Veit Elser's paper; we'll need to expand 
!> on this routine to be able to simulate more realistic QC structures.
!
!> @param QCcell QC structure pointer
!> @param hkl Miller indices
!> @param qg interaction coupling constant
!> @param Vmod, Vpmod, xig, and xgp are optional output parameters
!
!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  01/10/14 MDG 4.0 account for new version of cell type
!> @date  06/09/14 MDG 4.1 added rltail and cell as arguments
!> @date  06/17/14 MDG 4.2 modification for separate reflist pointers
!> @date  09/08/15 MDG 4.3 added qg entry
!> @date  03/15/17 MDG 4.3 copied from gvectors module for QCmod adaptation
!> @date  06/25/18 SS  4.4 moved to diffractyionQC module
!--------------------------------------------------------------------------
recursive function QC_getUcg3DQC(QCcell, hkl, qg, Vmod, Vpmod, xig, xgp) result(Ucg)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_getUcg3DQC

use error
use constants
use others

IMPLICIT NONE

type(QCStructureType),pointer        :: QCcell
integer(kind=irg),INTENT(IN)         :: hkl(6)               !< QC Miller indices of reflection 
complex(kind=dbl),INTENT(OUT)        :: qg
real(kind=dbl),INTENT(OUT)           :: Vmod
real(kind=dbl),INTENT(OUT)           :: Vpmod
real(kind=dbl),INTENT(OUT)           :: xig
real(kind=dbl),INTENT(OUT)           :: xgp
complex(kind=dbl)     				       :: Ucg 

integer(kind=irg)                    :: i,j,absflg,m,ii
real(kind=sgl)                       :: s,twopi,arg,swk,dwwk,pref,ul,pre,preg,sct,fs,fsp, g, go, p, q, &
                                        Umod, Upmod, Vphase, Vpphase, multiplicity
complex(kind=sgl)                    :: ff,gg,sf,p1,pp
complex(kind=sgl)                    :: czero
logical                              :: accflg, dwflg
character(2)                         :: smb

twopi = sngl(2.D0*cPi)
czero = cmplx(0.0,0.0)

! we'll only use the Weickenmeier-Kohl scattering parameters here since this
! allows us to also get the absorption part of the Fourier coefficients
! some of this code is directly adapted from the CalcUcg routine in diffraction.f90

! first we need to get the scattering parameter
! compute the scattering parameter s^2=(g/2)^2
if (sum(hkl**2).eq.0) then 
	s = 0.0
	g = 0.0
	go = 0.0
else
	g = QC_getvectorLength(QCcell, hkl, 'P', 'r')
	s = (0.50*g)**2
end if

! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
swk = 0.1*twopi
dwwk = 100.0/(8.0*cPi**2)

! properly scale the scattering parameter
s = g*swk

! let fscatt perform the relativistic corrections for f_g and fprime_g
accflg = .TRUE.

! include absorption ?
absflg = 3  ! include phonon and core contributions

! always include Debye-Waller factor
dwflg  = .TRUE.

! compute the scaling prefactors
! pref contains A to nm conversion, and divides by 4pi
! WE NEED TO FIGURE OUT WHAT TO DO FOR THE UNIT CELL VOLUME !!!
! pref = 0.04787801/QCcell % vol/(4.0*cPi) 
pref = 0.04787801/(4.0*cPi)/QCcell%vol

! preg is used to go from V to U, remembering that gamma is already
! included in the output from fscatt
preg = 2.0 * sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18
pre = pref * preg

! initialize the real and imaginary parts of the structure factor
ff=czero
gg=czero 

do m = 1,QCcell%ATOM_ntype

	ul = sqrt(QCcell%ATOM_pos(m,8)*dwwk + QCcell%ATOM_pos(m,9)*dwwk)

	j  = QCcell%ATOM_type(m)

	sf = FSCATT(s,ul,j,smb,sngl(QCcell%voltage),absflg,accflg,dwflg) * QCcell%ATOM_pos(m,7)

	pp = ShapeTransformTriacontahedron(QCcell, hkl, m)

! loop over all atoms in the orbit
	p1 = czero
	do j = 1,QCcell%numat(m)
		arg = twopi*sum(float(hkl(1:6))*QCcell%apos(m,j,1:6))
		p1  = p1 + exp(cmplx(0.0,-arg))
	end do

	ff = ff + p1*real(sf)  * abs(real(pp))
	gg = gg + p1*aimag(sf) * abs(real(pp))

end do

! these are the modulus and phase of the real part of Vg
Vmod = pref * cabs(ff)
Vphase = atan2(aimag(ff),real(ff))

! modulus of U_g
Umod = preg*Vmod

! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g
Vpmod = pref * cabs(gg)
Vpphase = atan2(aimag(gg),real(gg))

! modulus of Uprime_g
Upmod = preg*Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))

! complex Vg 
if (QCcell%mLambda.ne.-1.0) then
	if (abs(Umod).gt.0.0) then 
		xig = 1.0/abs(Umod)/QCcell%mLambda
	else
		xig = 1.0E+8
	end if 

	if (abs(Upmod).gt.0.0) then 
		xgp = 1.0/abs(Upmod)/QCcell%mLambda
	else
		xgp = 1.0E+8
	end if 

	arg = Vpphase-Vphase
	qg  = cmplx(cos(Vphase)/xig-sin(Vpphase)/xgp,cos(Vpphase)/xgp+sin(Vphase)/xig)
	!qg = cmplx(1.0/xig-sin(arg)/xgp,cos(arg)/xgp)

end if


end function QC_getUcg3DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_CalcWaveLength2DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the electron wavelength and select scattering data (quasi-crystal version)
! 
!> @details computes the relativistic electron wavelength
!>  These quantities are computed in double precision because of the 
!>  wide range of magnitudes.  If a crystal structure has been defined
!>  then the gamma*V_0 term is added to correct for refraction.
!
!> @param QCcell unit cell pointer
!> @param verbose optional logical
!
!> @date  03/15/17 MDG 1.0 original based on routine in diffraction.f90
!> @date  03/23/18 SS  1.1 copied from QCmod.f90 and adapted for 2-D QC
!--------------------------------------------------------------------------
recursive subroutine QC_CalcWaveLength2DQC(QCcell,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_CalcWaveLength2DQC

use constants
use io

IMPLICIT NONE

type(TDQCStructureType),pointer         :: QCcell
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=dbl)                          :: temp1,temp2, oi_real(1), Vmod, Vpmod, xig, xgp, io_real(1)
integer(kind=irg)                       :: hkl(5), io_int(1), QCindex
complex(kind=dbl)                       :: Ucg, qg

temp1 = 1.0D+9*cPlanck/dsqrt(2.D0*cRestmass*cCharge)
temp2 = cCharge*0.5D0*QCcell%voltage*1000.D0/cRestmass/cLight**2

! relativistic correction factor (known as gamma)      
QCcell%mRelcor = 1.0D0+2.0D0*temp2

! relativistic acceleration voltage
QCcell%mPsihat = QCcell%voltage*(1.D0+temp2)*1000.D0

! we will always use the Weickenmeier-Kohl parameters
! we need the mean inner potential ... we'll just wing it on this one...
hkl=(/ 0, 0, 0, 0, 0/)
Ucg = QC_getUcg(QCcell,hkl,qg,Vmod,Vpmod,xig,xgp) 

QCcell%mPsihat = QCcell%mPsihat + Vmod
QCcell%mLambda = temp1/dsqrt(QCcell%mPsihat)

! interaction constant sigma
QCcell%mSigma = 2.D0*cPi*cRestmass*QCcell%mRelcor*cCharge*QCcell%mLambda
QCcell%mSigma = 1.0D-18*QCcell%mSigma/cPlanck**2

if (present(verbose)) then
  if (verbose) then
    oi_real(1) = Vmod
    call WriteValue('Mean inner potential [V] ', oi_real, 1,"(' ',E10.4)")
    call Message(' Wavelength corrected for refraction', frm = "(A)")
    oi_real(1) = QCcell%mRelcor
    call WriteValue('Relativistic correction factor [gamma]  ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = QCcell%mPsihat
    call WriteValue('Relativistic Accelerating Potential [V] ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = QCcell%mLambda
    call WriteValue('Electron Wavelength [nm]                ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = QCcell%mSigma
    call WriteValue('Interaction constant [V nm]^(-1)        ', oi_real, 1,"(' ',E10.4)")
  end if
end if

! we call this again, since the previous time the refraction-corrected electron wavelength was not yet known
Ucg = QC_getUcg(QCcell,hkl,qg,Vmod,Vpmod,xig,xgp) 
QCcell%Upzero = Vpmod         ! U'0 normal absorption parameter 
QCcell%xizerop = xgp          ! normal absorption length
if (present(verbose)) then
  if (verbose) then
    io_real(1) = xgp
    call WriteValue(' Normal absorption length [nm] = ', io_real, 1)
  end if
end if

end subroutine QC_CalcWaveLength2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_CalcWaveLength3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the electron wavelength and select scattering data (quasi-crystal version)
! 
!> @details computes the relativistic electron wavelength
!>  These quantities are computed in double precision because of the 
!>  wide range of magnitudes.  If a crystal structure has been defined
!>  then the gamma*V_0 term is added to correct for refraction.
!
!> @param QCcell unit cell pointer
!> @param verbose optional logical
!
!> @date  03/15/17 MDG 1.0 original based on routine in diffraction.f90
!> @date  06/25/18 SS  1.1 moved to this module
!--------------------------------------------------------------------------
recursive subroutine QC_CalcWaveLength3DQC(QCcell,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_CalcWaveLength3DQC

use constants
use io

IMPLICIT NONE

type(QCStructureType),pointer           :: QCcell
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=dbl)                          :: temp1,temp2, oi_real(1), Vmod, Vpmod, xig, xgp, io_real(1)
integer(kind=irg)                       :: hkl(6), io_int(1)
complex(kind=dbl)                       :: Ucg, qg

temp1 = 1.0D+9*cPlanck/dsqrt(2.D0*cRestmass*cCharge)
temp2 = cCharge*0.5D0*QCcell%voltage*1000.D0/cRestmass/cLight**2

! relativistic correction factor (known as gamma)      
QCcell%mRelcor = 1.0D0+2.0D0*temp2

! relativistic acceleration voltage
QCcell%mPsihat = QCcell%voltage*(1.D0+temp2)*1000.D0

! we will always use the Weickenmeier-Kohl parameters
! we need the mean inner potential ... we'll just wing it on this one...
hkl=(/ 0, 0, 0, 0, 0, 0/)
Ucg = QC_getUcg(QCcell,hkl,qg,Vmod,Vpmod,xig,xgp) 
QCcell%mPsihat = QCcell%mPsihat + Vmod
QCcell%mLambda = temp1/dsqrt(QCcell%mPsihat)
! interaction constant sigma
QCcell%mSigma = 2.D0*cPi*cRestmass*QCcell%mRelcor*cCharge*QCcell%mLambda
QCcell%mSigma = 1.0D-18*QCcell%mSigma/cPlanck**2

if (present(verbose)) then
  if (verbose) then
    oi_real(1) = Vmod
    call WriteValue('Mean inner potential [V] ', oi_real, 1,"(' ',E10.4)")
    call Message(' Wavelength corrected for refraction', frm = "(A)")
    oi_real(1) = QCcell%mRelcor
    call WriteValue('Relativistic correction factor [gamma]  ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = QCcell%mPsihat
    call WriteValue('Relativistic Accelerating Potential [V] ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = QCcell%mLambda
    call WriteValue('Electron Wavelength [nm]                ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = QCcell%mSigma
    call WriteValue('Interaction constant [V nm]^(-1)        ', oi_real, 1,"(' ',E10.4)")
  end if
end if

! we call this again, since the previous time the refraction-corrected electron wavelength was not yet known
Ucg = QC_getUcg(QCcell,hkl,qg,Vmod,Vpmod,xig,xgp) 
QCcell%Upzero = Vpmod         ! U'0 normal absorption parameter 
QCcell%xizerop = xgp          ! normal absorption length
if (present(verbose)) then
  if (verbose) then
    io_real(1) = xgp
    call WriteValue(' Normal absorption length [nm] = ', io_real, 1)
  end if
end if

end subroutine QC_CalcWaveLength3DQC

!--------------------------------------------------------------------------
!
! FUNCTION: QC_Calcsg2DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the excitation error for a given QC reflection 
!
!> @param QCcell cell pointer
!> @param gg reciprocal lattice point indices [in cartesian frame ! ] 
!> @param kk wave vector components [in cartesian frame ! ]
!> @param FN foil normal [in cartesian frame ! ]
!
!> @date   03/15/17 MDG 1.0 original
!> @date   03/23/18 SS  1.1 adapted from QCmod.f90
!> @date   06/25/18 SS  1.2 moved to diffractionQC module
!--------------------------------------------------------------------------
recursive function QC_Calcsg2DQC(QCcell,gg,kk,FN) result(sg)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_Calcsg2DQC

IMPLICIT NONE

type(TDQCStructureType),pointer :: QCcell
integer(kind=irg),INTENT(IN)    :: gg(5)                !< reciprocal lattice point
real(kind=sgl),INTENT(IN)       :: kk(3)                !< wave vector
real(kind=sgl),INTENT(IN)       :: FN(3)                !< foil normal

real(kind=sgl)                  :: kpg(3),tkpg(3),xnom,xden,q1,q2,sg,gvec(3)

! get the g-vector
 gvec = QC_getGvector(QCcell, dble(gg), 'P')
! auxiliary vectors
 kpg  = kk + gvec
 tkpg = 2.0 * kk + gvec

! use equation of Ewald sphere
 xnom = -DOT_PRODUCT(gvec,tkpg)
! 2|k0+g|cos(alpha) = 2(k0+g).Foilnormal
 q2   = DOT_PRODUCT(kpg,FN)
 xden = 2.D0*q2
 sg   = xnom/xden

end function QC_Calcsg2DQC

!--------------------------------------------------------------------------
!
! FUNCTION: QC_Calcsg3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the excitation error for a given QC reflection  TO BE VERIFIED !!!!
!
!> @param QCcell cell pointer
!> @param gg reciprocal lattice point indices [in cartesian frame ! ] 
!> @param kk wave vector components [in cartesian frame ! ]
!> @param FN foil normal [in cartesian frame ! ]
!
!> @date   03/15/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function QC_Calcsg3DQC(QCcell,gg,kk,FN) result(sg)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_Calcsg3DQC

IMPLICIT NONE

type(QCStructureType),pointer   :: QCcell
integer(kind=irg),INTENT(IN)    :: gg(6)                !< reciprocal lattice point
real(kind=sgl),INTENT(IN)       :: kk(3)                !< wave vector
real(kind=sgl),INTENT(IN)       :: FN(3)                !< foil normal

real(kind=sgl)                  :: kpg(3),tkpg(3),xnom,xden,q1,q2,sg,gvec(3)

! get the g-vector
 gvec = QC_getGvector(QCcell, dble(gg), 'P')

! auxiliary vectors
 kpg=kk+gvec
 tkpg=2.0*kk+gvec

! use equation of Ewald sphere
 xnom = -DOT_PRODUCT(gvec,tkpg)

! 2|k0+g|cos(alpha) = 2(k0+g).Foilnormal
 q2 = DOT_PRODUCT(kpg,FN)
 xden = 2.D0*q2
 sg = xnom/xden

end function QC_Calcsg3DQC

end module diffractionQC
