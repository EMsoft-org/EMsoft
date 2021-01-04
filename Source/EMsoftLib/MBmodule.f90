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
! EMsoft:MBmodule.f90
!--------------------------------------------------------------------------
!
! MODULE: MBmodule
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routine for multi-beam diffraction calculations, used by more than one program
!
!> @date 7/10/13   MDG 1.0 original
!> @date 7/12/13   MDG 1.1 added forward cube to ball to quaternion mappings
!> @date 8/01/13   MDG 1.2 added standard Lambert projection
!> @date 8/12/13   MDG 1.3 added inverse Lambert projections for Ball to Cube
!> @date 9/20/13   MDG 1.4 added ApplyLaueSymmetry
!> @date 1/07/16   MDG 1.5 replaced calls to cmplx() by cmplx() with dbl as third argument (needed for Visual Studio)
!--------------------------------------------------------------------------
module MBmodule

use local
use typedefs

IMPLICIT NONE

contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcBWint
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the scattered intensities for a range of thicknesses
!
!> @param Dyn dynamical scattering structure
!> @param cell unit cell pointer
!> @param ktmp wave vector structure
!> @param BetheParameter Bethe potential parameters
!> @param nn number of strong beams
!> @param nw number of weak beams
!> @param nt number of thickness values
!> @param thick thickness array
!> @param inten output intensity list, including weak beams
!
!> @date  10/13/98 MDG 1.0 original
!> @date   7/04/01 MDG 2.0 f90
!> @date  04/29/13 MDG 3.0 inclusion of Bethe weak beams
!> @date  06/10/14 MDG 4.0 added Dyn, cell, ktmp, and BetheParameter arguments
!--------------------------------------------------------------------------
subroutine CalcBWint(Dyn,cell,ktmp,BetheParameter,nn,nw,nt,thick,inten)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcBWint

use io
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

type(DynType),INTENT(INOUT)     :: Dyn
!f2py intent(in,out) ::  Dyn
type(unitcell)                  :: cell
type(kvectorlist),pointer       :: ktmp
type(BetheParameterType),INTENT(IN) :: BetheParameter
integer(kind=irg),INTENT(IN)    :: nn                   !< number of strong beams
integer(kind=irg),INTENT(IN)    :: nw                   !< number of weak beams
integer(kind=irg),INTENT(IN)    :: nt                   !< number of thickness values
real(kind=sgl),INTENT(IN)       :: thick(nt)            !< thickness array
real(kind=sgl),INTENT(INOUT)    :: inten(nt,nn+nw)      !< output intensities (both strong and weak)
!f2py intent(in,out) ::  inten

integer(kind=irg)               :: i,j,IPIV(nn), ll(3), jp
complex(kind=dbl)               :: CGinv(nn,nn), Minp(nn,nn),diag(nn),Wloc(nn), lCG(nn,nn), lW(nn), &
                                   lalpha(nn), delta(nn,nn), weak(nw,nn), Ucross(nw,nn), tmp(nw,nn), c
real(kind=sgl)                  :: th

! compute the eigenvalues and eigenvectors
 Minp = Dyn%DynMat
 IPIV = 0
 call BWsolve(Minp,Wloc,lCG,CGinv,nn,IPIV)

! the alpha coefficients are in the first column of the inverse matrix
! the minus sign in W(i) stems from the fact that k_n is in the direction
! opposite to the foil normal
 lW = cPi*Wloc/cmplx(ktmp%kn,0.0)
 do i=1,nn
  lalpha(i) = CGinv(i,1)
 end do

! in preparation for the intensity computation, we need the prefactor array for the
! weak beam amplitude computation...
! we will also need a potential coefficient array for the cross coefficients, which we 
! compute in the same loop
 do j=1,nn   ! strong beam loop
   do jp=1,nw  ! weak beam loop
! prefactor value
     c = cmplx(2.D0*BetheParameter%weaksg(jp)/cell%mLambda) - 2.D0*ktmp%kn*Wloc(j)
     weak(jp,j) = cmplx(-1.D0,0.D0)/c
! cross potential coefficient
     ll(1:3) = BetheParameter%weakhkl(1:3,jp) - BetheParameter%stronghkl(1:3,j)
     Ucross(jp,j) = cell%LUT( ll(1),ll(2),ll(3) )
   end do
 end do

! compute the strong beam intensities, stored in the first nn slots of inten 
! we can also compute the weak beams, since they make use of the same diag(1:nn) expression
! as the strong beams, plus a few other factors (excitation error, wave length, Fourier coefficients)
 do i=1,nt
  th = thick(i)
  diag(1:nn)=exp(-th*aimag(lW(1:nn)))*cmplx(cos(th*real(lW(1:nn))),sin(th*real(lW(1:nn))))*lalpha(1:nn)
! the delta array is common to the strong and weak beam intensity computation, so we compute it first
  do j=1,nn
   delta(j,1:nn) = lCG(j,1:nn)*diag(1:nn)
  end do
! strong beams
  do j=1,nn
   inten(i,j) = abs(sum(delta(j,1:nn)))**2
  end do 
! weak beams
  tmp = matmul(Ucross,delta)
  do jp=1,nw
   inten(i,nn+jp) = abs( sum(weak(jp,1:nn)*tmp(jp,1:nn)) )**2
  end do  
 end do
   
end subroutine CalcBWint


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcCBEDint
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the scattered intensities for a range of thicknesses (specifically for CBED mode)
!
!> @param Dyn dynamical matrix
!> @param cell unit cell pointer
!> @param ktmp wave vector structure
!> @param BetheParameter Bethe potential parameters
!> @param nn number of strong beams
!> @param nw number of weak beams
!> @param nt number of thickness values
!> @param thick thickness array
!> @param inten output intensity list, including weak beams
!
!> @date  10/13/98 MDG 1.0 original
!> @date   7/04/01 MDG 2.0 f90
!> @date  04/29/13 MDG 3.0 inclusion of Bethe weak beams
!> @date  06/10/14 MDG 4.0 added Dyn, cell, ktmp, and BetheParameter arguments
!--------------------------------------------------------------------------
subroutine CalcCBEDint(DynMat,cell,kn,BetheParameter,nn,nt,thick,inten)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcCBEDint

use io
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nn                   !< number of strong beams
complex(kind=dbl),INTENT(IN)    :: DynMat(nn,nn)
type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: kn
type(BetheParameterType),INTENT(IN) :: BetheParameter
integer(kind=irg),INTENT(IN)    :: nt                   !< number of thickness values
real(kind=sgl),INTENT(IN)       :: thick(nt)            !< thickness array
real(kind=sgl),INTENT(INOUT)    :: inten(nt,nn)      !< output intensities (both strong and weak)
!f2py intent(in,out) ::  inten

integer(kind=irg)               :: i,j,IPIV(nn), ll(3), jp
complex(kind=dbl)               :: CGinv(nn,nn), Minp(nn,nn),diag(nn),Wloc(nn), lCG(nn,nn), lW(nn), &
                                   lalpha(nn), delta(nn,nn) 
real(kind=sgl)                  :: th

! compute the eigenvalues and eigenvectors
 Minp = DynMat
 IPIV = 0
 call BWsolve(Minp,Wloc,lCG,CGinv,nn,IPIV)

! the alpha coefficients are in the first column of the inverse matrix
! the minus sign in W(i) stems from the fact that k_n is in the direction
! opposite to the foil normal
 lW = cPi*Wloc/cmplx(kn,0.0)
 do i=1,nn
  lalpha(i) = CGinv(i,1)
 end do

! we are going to ignore weak beam intensities for now...

! compute the strong beam intensities, stored in the first nn slots of inten 
! we could also compute the weak beams, since they make use of the same diag(1:nn) expression
! as the strong beams, plus a few other factors (excitation error, wave length, Fourier coefficients)
! that part would need to rewritten entirely
 do i=1,nt
  th = thick(i)
  diag(1:nn)=exp(-th*aimag(lW(1:nn)))*cmplx(cos(th*real(lW(1:nn))),sin(th*real(lW(1:nn))))*lalpha(1:nn)
! the delta array is common to the strong and weak beam intensity computation, so we compute it first
  do j=1,nn
   delta(j,1:nn) = lCG(j,1:nn)*diag(1:nn)
  end do
! strong beams
  do j=1,nn
   inten(i,j) = abs(sum(delta(j,1:nn)))**2
  end do 
 end do
   
end subroutine CalcCBEDint

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcPEDint
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the scattered intensities for the Precession Electron Diffraction mode
!
!> @param DynMat dynamical matrix
!> @param cell unit cell pointer
!> @param kn  normal component
!> @param nn number of strong beams
!> @param nt number of thickness values
!> @param thick thickness array
!> @param inten output intensity list
!
!> @date  10/13/98 MDG 1.0 original
!> @date   7/04/01 MDG 2.0 f90
!> @date  04/29/13 MDG 3.0 inclusion of Bethe weak beams
!> @date  06/10/14 MDG 4.0 added Dyn, cell, ktmp, and BetheParameter arguments
!> @date  11/28/14 MDG 4.1 forked from CalcBWint
!--------------------------------------------------------------------------
subroutine CalcPEDint(DynMat,cell,kn,nn,nt,thick,inten)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcPEDint

use io
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nn                   !< number of strong beams
complex(kind=dbl),INTENT(IN)    :: DynMat(nn,nn)
type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: kn
integer(kind=irg),INTENT(IN)    :: nt                   !< number of thickness values
real(kind=sgl),INTENT(IN)       :: thick(nt)            !< thickness array
real(kind=sgl),INTENT(INOUT)    :: inten(nt,nn)         !< output intensities (both strong and weak)
!f2py intent(in,out) ::  inten

integer(kind=irg)               :: i,j,IPIV(nn), ll(3), jp
complex(kind=dbl)               :: CGinv(nn,nn), Minp(nn,nn),diag(nn),Wloc(nn), lCG(nn,nn), lW(nn), &
                                   lalpha(nn), delta(nn,nn), s, c
real(kind=sgl)                  :: th

! compute the eigenvalues and eigenvectors
 Minp = DynMat
 IPIV = 0
 call BWsolve(Minp,Wloc,lCG,CGinv,nn,IPIV)

! the alpha coefficients are in the first column of the inverse matrix
 lW = cPi*Wloc/cmplx(kn,0.0)
 lalpha(1:nn) = CGinv(1:nn,1)

! make sure the alpha excitation coefficients are normalized 
! s = sum(abs(lalpha(1:nn))**2)
! if (s.ne.1.D0) then
!  s = cmplx(1.D0/dsqrt(s),0.D0)
!  lalpha = lalpha*s
! endif 

! compute the strong beam intensities, stored in the first nn slots of inten 
 do i=1,nt
  th = thick(i)
  diag(1:nn)=exp(-th*aimag(lW(1:nn)))*cmplx(cos(th*real(lW(1:nn))),sin(th*real(lW(1:nn))))*lalpha(1:nn)
! the delta array is common to the strong and weak beam intensity computation, so we compute it first
  do j=1,nn
   delta(j,1:nn) = lCG(j,1:nn)*diag(1:nn)
  end do
! strong beams
  do j=1,nn
   inten(i,j) = abs(sum(delta(j,1:nn)))**2
  end do 
 end do
   
end subroutine CalcPEDint



!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcKint
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the Kossel intensities for a range of thicknesses
!
!> @param DynMat dynamical matrix
!> @param ktmp wave vector structure
!> @param nn number of strong beams
!> @param nt number of thickness values
!> @param thick thickness array
!> @param inten output intensity list, including weak beams
!
!> @date  10/13/98 MDG 1.0 original
!> @date   7/04/01 MDG 2.0 f90
!> @date  04/29/13 MDG 3.0 inclusion of Bethe weak beams
!> @date  01/08/14 MDG 3.1 forked from CalcBWint, specialized for Kossel computation
!> @date  06/10/14 MDG 4.0 added Dyn, cell, and ktmp arguments
!> @date  06/15/14 MDG 4.1 removed global W, CG and alpha initializations
!> @date  06/16/14 MDG 4.2 made routine recursive for OPenMP
!--------------------------------------------------------------------------
recursive subroutine CalcKint(DynMat,kn,nn,nt,thick,Iz)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcKint

use local
use io
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nn                   !< number of strong beams
complex(kind=dbl),INTENT(IN)    :: DynMat(nn,nn)
real(kind=sgl),INTENT(IN)       :: kn
integer(kind=irg),INTENT(IN)    :: nt                   !< number of thickness values
real(kind=sgl),INTENT(IN)       :: thick(nt)            !< thickness array
real(kind=sgl),INTENT(INOUT)    :: Iz(nt)               !< output intensities
!f2py intent(in,out) ::  Iz

integer(kind=irg)               :: j, IPIV(nn), k
complex(kind=dbl)               :: CGinv(nn,nn), Minp(nn,nn), Wloc(nn), lCG(nn,nn), lW(nn), lalpha(nn)
real(kind=dbl)                  :: s, q, t


! compute the eigenvalues and eigenvectors
 Minp = DynMat
 IPIV = 0
 call BWsolve(Minp,Wloc,lCG,CGinv,nn,IPIV)


! the alpha coefficients are in the first column of the inverse matrix
 lW = cPi*Wloc/cmplx(kn,0.0)
 lalpha(1:nn) = CGinv(1:nn,1)

! make sure the alpha excitation coefficients are normalized 
 s = sum(abs(lalpha(1:nn))**2)
 if (s.ne.1.D0) then
  s = cmplx(1.D0/dsqrt(s),0.D0)
  lalpha = lalpha*s
 endif 
 
! compute the thickness array 
 Iz = 0.D0
 do j=1,nn
    q = -4.D0*cPi*aimag(lW(j))
    s = abs(lalpha(j))**2
    do k=1,nt
      t = q*thick(k)
      if (abs(t).lt.30.D0) Iz(k) = Iz(k) +  s * exp(t)
    end do
 end do   
   
end subroutine CalcKint

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcKthick
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the thickness for which the Kossel intensity drops below a treshold
!
!> @param DynMat dynamical matrix
!> @param ktmp wave vector structure
!> @param nn number of strong beams
!> @param thresh thickness fraction parameter
!> @param Iz returned thickness value 
!
!> @date  10/13/98 MDG 1.0 original
!> @date   7/04/01 MDG 2.0 f90
!> @date  04/29/13 MDG 3.0 inclusion of Bethe weak beams
!> @date  01/08/14 MDG 3.1 forked from CalcBWint, specialized for Kossel computation
!> @date  06/10/14 MDG 4.0 added Dyn, cell, and ktmp arguments
!> @date  06/15/14 MDG 4.1 removed global W, CG and alpha initializations
!> @date  06/16/14 MDG 4.2 made routine recursive for OPenMP
!> @date  02/14/15 MDG 4.3 spawned from CalcKint
!--------------------------------------------------------------------------
recursive subroutine CalcKthick(DynMat,kn,nn,thresh,Iz)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcKthick

use local
use io
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nn                   !< number of strong beams
complex(kind=dbl),INTENT(IN)    :: DynMat(nn,nn)
real(kind=sgl),INTENT(IN)       :: kn
real(kind=sgl),INTENT(IN)       :: thresh               !< thickness fraction parameter
real(kind=sgl),INTENT(INOUT)    :: Iz(1)                !< output (thickness)
!f2py intent(in,out) ::  Iz

integer(kind=irg)               :: j, IPIV(nn), k
complex(kind=dbl)               :: CGinv(nn,nn), Minp(nn,nn), Wloc(nn), lCG(nn,nn), lW(nn), lalpha(nn)
real(kind=dbl)                  :: s(nn), q(nn), t, ss


! compute the eigenvalues and eigenvectors
 Minp = DynMat
 IPIV = 0
 call BWsolve(Minp,Wloc,lCG,CGinv,nn,IPIV)


! the alpha coefficients are in the first column of the inverse matrix
 lW = cPi*Wloc/cmplx(kn,0.0)
 lalpha(1:nn) = CGinv(1:nn,1)

! make sure the alpha excitation coefficients are normalized 
 ss = sum(abs(lalpha(1:nn))**2)
 if (ss.ne.1.D0) then
  ss = cmplx(1.D0/dsqrt(ss),0.D0)
  lalpha = lalpha*ss
 endif 
 
! compute the thickness value in steps of 0.25 nm until less than thresh
 do j=1,nn
  q(j) = -4.D0*cPi*aimag(lW(j))
  s(j) = abs(lalpha(j))**2
 end do
 t = 0.D0
 do 
   t = t+0.25D0
   ss = sum(s*dexp(t*q))
   if (ss.le.thresh) EXIT
 end do
 Iz(1) = t
   
end subroutine CalcKthick

!--------------------------------------------------------------------------
!
! SUBROUTINE: Initialize_SghLUT
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize a look-up table with Sgh structure-factor-like entries
!
!> @param cell unit cell pointer
!> @param dmin smallest d-spacing to consider
!> @param verbose produce output (or not)
!
!> @date 05/02/16 MDG 1.0 original
!> @date 12/03/20 MDG 2.0 adds OpenMP to speed up the computation for large unit cells
!--------------------------------------------------------------------------
recursive subroutine Initialize_SghLUT(cell, dmin, numset, nat, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_SghLUT

use local
use typedefs
use crystal
use symmetry
use files
use io
use error
use gvectors
use diffraction

IMPLICIT NONE

type(unitcell)                             :: cell
real(kind=sgl),INTENT(IN)                  :: dmin
integer(kind=sgl),INTENT(IN)               :: numset
integer(kind=sgl),INTENT(INOUT)            :: nat(maxpasym)
!f2py intent(in,out) ::  nat
logical,INTENT(IN),optional                :: verbose

integer(kind=irg)                          :: istat, io_int(3), skip
integer(kind=irg)                          :: imh, imk, iml, gg(3), ix, iy, iz
real(kind=sgl)                             :: dhkl, io_real(3), ddt
complex(kind=dbl)                          :: Sghvec(numset)


! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.dmin) EXIT
   iml = iml + 1
 end do
 
 if (present(verbose)) then
  if (verbose) then
    io_int = (/ imh, imk, iml /)
    call WriteValue(' Range of reflections along a*, b* and c* = ',io_int,3)
  end if
 end if
  
! the SghLUT array stores all the structure-factor-like quantities of the Sgh matrix needed for EBSD, ECP, etc simulations
 allocate(cell%SghLUT(1:numset,-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%SghLUT array')
 cell%SghLUT = cmplx(0.D0,0.D0)

 if (present(verbose)) then
  if (verbose) then
   call Message('Generating Sgh coefficient lookup table ... ', frm = "(/A)",advance="no")
  end if
 end if
 
!  if (present(nthreads)) then 
!     call OMP_SET_NUM_THREADS(nthreads)
! !$OMP PARALLEL DEFAULT(shared) PRIVATE(iz, ix, iy, gg, mynat, Sghvec)

!   TID = OMP_GET_THREAD_NUM()
! ! note that the lookup table must be twice as large as the list of participating reflections,
! ! since the Sgh matrix uses g-h as its index !!!  
! !$OMP DO SCHEDULE(DYNAMIC) 
!     izlomp: do iz=-2*iml,2*iml
!     iylomp:  do iy=-2*imk,2*imk
!     ixlomp:   do ix=-2*imh,2*imh
!             gg = (/ ix, iy, iz /)
!             if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
! ! add the reflection to the look up table
!               call preCalcSgh(cell,gg,numset,mynat,Sghvec)
! !$OMP CRITICAL
!               cell%SghLUT(1:numset, ix, iy, iz) = Sghvec(1:numset)
! !$OMP END CRITICAL
!             end if ! IsGAllowed
!         end do ixlomp
!       end do iylomp
!     end do izlomp
!     if (TID.eq.0) nat = mynat
! !$OMP END PARALLEL 
!  else
! note that the lookup table must be twice as large as the list of participating reflections,
! since the Sgh matrix uses g-h as its index !!!  
    izl: do iz=-2*iml,2*iml
    iyl:  do iy=-2*imk,2*imk
    ixl:   do ix=-2*imh,2*imh
            gg = (/ ix, iy, iz /)
            if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
! add the reflection to the look up table
              call preCalcSgh(cell,gg,numset,nat,Sghvec)
              cell%SghLUT(1:numset, ix, iy, iz) = Sghvec(1:numset)
            end if ! IsGAllowed
        end do ixl
      end do iyl
    end do izl
 ! end if

  if (present(verbose)) then
   if (verbose) then
    call Message('Done', frm = "(A/)")
   end if
  end if

! that's it
end subroutine Initialize_SghLUT


!--------------------------------------------------------------------------
!
! SUBROUTINE: Initialize_SghLUTEEC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize a look-up table with Sgh structure-factor-like entries (EMEECmaster)
!
!> @param cell unit cell pointer
!> @param dmin smallest d-spacing to consider
!> @param numset number of equivalent isotope atom positions
!> @param ctmp array with isotope fractional coordinates
!> @param verbose produce output (or not)
!
!> @date 12/14/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Initialize_SghLUTEEC(cell, dmin, numset, ctmp, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_SghLUTEEC

use local
use typedefs
use crystal
use symmetry
use files
use io
use error
use gvectors
use diffraction

IMPLICIT NONE

type(unitcell)                             :: cell
real(kind=sgl),INTENT(IN)                  :: dmin
integer(kind=sgl),INTENT(IN)               :: numset
real(kind=dbl),INTENT(INOUT)               :: ctmp(192,3)          !< output array with orbit coordinates
logical,INTENT(IN),optional                :: verbose

integer(kind=irg)                          :: istat, io_int(3), skip
integer(kind=irg)                          :: imh, imk, iml, gg(3), ix, iy, iz
real(kind=sgl)                             :: dhkl, io_real(3), ddt
complex(kind=dbl)                          :: Sghvec


! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.dmin) EXIT
   iml = iml + 1
 end do
 
 if (present(verbose)) then
  if (verbose) then
    io_int = (/ imh, imk, iml /)
    call WriteValue(' Range of reflections along a*, b* and c* = ',io_int,3)
  end if
 end if
  
! the SghLUT array stores all the structure-factor-like quantities of the Sgh matrix needed for EBSD, ECP, etc simulations
 allocate(cell%SghLUT(1:numset,-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%SghLUT array')
 cell%SghLUT = cmplx(0.D0,0.D0)

 if (present(verbose)) then
  if (verbose) then
   call Message(' Generating Sgh coefficient lookup table ... ', frm = "(/A)",advance="no")
  end if
 end if
 
! note that the lookup table must be twice as large as the list of participating reflections,
! since the Sgh matrix uses g-h as its index !!!  
izl: do iz=-2*iml,2*iml
iyl:  do iy=-2*imk,2*imk
ixl:   do ix=-2*imh,2*imh
        gg = (/ ix, iy, iz /)
        if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
! add the reflection to the look up table
           call preCalcSghEEC(cell,gg,numset,ctmp,Sghvec)
           cell%SghLUT(1, ix, iy, iz) = Sghvec
        end if ! IsGAllowed
       end do ixl
      end do iyl
    end do izl

  if (present(verbose)) then
   if (verbose) then
    call Message(' Done', frm = "(A/)")
   end if
  end if

! that's it
end subroutine Initialize_SghLUTEEC


!--------------------------------------------------------------------------
!
! SUBROUTINE: getSghfromLUT
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like Sgh array entry for EBSD, ECCI and ECP simulations
!
!> @param cell unit cell pointer
!> @param reflist pointer to reflection list
!> @param nns number of strong reflections
!> @param numset number of atom positions in asymmetric unit
!> @param nat normalization array
!> @param Sgh output array
!
!> @date 05/03/16  MDG 1.0 original 
!--------------------------------------------------------------------------
recursive subroutine getSghfromLUT(cell,reflist,nns,numset,nat,Sgh)
!DEC$ ATTRIBUTES DLLEXPORT :: getSghfromLUT

use local
use typedefs
use crystal
use gvectors
use constants
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell
type(reflisttype),pointer               :: reflist
integer(kind=irg),INTENT(IN)            :: nns
integer(kind=irg),INTENT(IN)            :: numset
integer(kind=irg),INTENT(IN)            :: nat(maxpasym)
complex(kind=dbl),INTENT(INOUT)         :: Sgh(nns,nns,numset)
!f2py intent(in,out) ::  Sgh

type(reflisttype),pointer               :: rltmpa, rltmpb
integer(kind=irg)                       :: ir, ic, kkk(3)

! loop over all contributing reflections
! ir is the row index
    rltmpa => reflist%next    ! point to the front of the list
    do ir=1,nns
! ic is the column index
      rltmpb => reflist%next    ! point to the front of the list
      do ic=1,nns
        kkk = rltmpb%hkl - rltmpa%hkl
        Sgh(ir,ic,1:numset) = cell%SghLUT(1:numset,kkk(1),kkk(2),kkk(3))
        rltmpb => rltmpb%nexts  ! move to next column-entry
      end do
     rltmpa => rltmpa%nexts  ! move to next row-entry
   end do  

end subroutine getSghfromLUT

!--------------------------------------------------------------------------
!
! SUBROUTINE: getSghfromLUTEEC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like Sgh array entry for EEC simulations
!
!> @param cell unit cell pointer
!> @param reflist pointer to reflection list
!> @param nns number of strong reflections
!> @param Sgh output array
!
!> @date 12/14/19  MDG 1.0 original 
!--------------------------------------------------------------------------
recursive subroutine getSghfromLUTEEC(cell,reflist,nns,Sgh)
!DEC$ ATTRIBUTES DLLEXPORT :: getSghfromLUTEEC

use local
use typedefs
use crystal
use gvectors
use constants
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell
type(reflisttype),pointer               :: reflist
integer(kind=irg),INTENT(IN)            :: nns
complex(kind=dbl),INTENT(INOUT)         :: Sgh(nns,nns)
!f2py intent(in,out) ::  Sgh

type(reflisttype),pointer               :: rltmpa, rltmpb
integer(kind=irg)                       :: ir, ic, kkk(3)

! loop over all contributing reflections
! ir is the row index
    rltmpa => reflist%next    ! point to the front of the list
    do ir=1,nns
! ic is the column index
      rltmpb => reflist%next    ! point to the front of the list
      do ic=1,nns
        kkk = rltmpb%hkl - rltmpa%hkl
        Sgh(ir,ic) = cell%SghLUT(1,kkk(1),kkk(2),kkk(3))
        rltmpb => rltmpb%nexts  ! move to next column-entry
      end do
     rltmpa => rltmpa%nexts  ! move to next row-entry
   end do  

end subroutine getSghfromLUTEEC


!--------------------------------------------------------------------------
!
! SUBROUTINE: preCalcSgh
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like Sgh array entry for EBSD, ECCI and ECP simulations
!
!> @param cell unit cell pointer
!> @param kkk input g-vector
!> @param numset number of atom positions in asymmetric unit
!> @param Sghvec output array
!> @param nat normalization array
!
!> @date 03/05/14  MDG 1.0 original (used to be in-line in ECP and ECCI programs)
!> @date 03/11/14  MDG 1.1 converted to diagonal Sgh array only
!> @date 06/19/14  MDG 2.0 no globals, taken out of EMECCI.f90
!> @date 09/07/15  MDG 2.1 added zeroing of Sgh array
!--------------------------------------------------------------------------
recursive subroutine preCalcSgh(cell,kkk,numset,nat,Sghvec)
!DEC$ ATTRIBUTES DLLEXPORT :: preCalcSgh

use local
use typedefs
use crystal
use gvectors
use constants
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: kkk(3)
integer(kind=irg),INTENT(IN)            :: numset
integer(kind=irg),INTENT(INOUT)         :: nat(maxpasym)
!f2py intent(in,out) ::  nat
complex(kind=dbl),INTENT(INOUT)         :: Sghvec(numset)
!f2py intent(in,out) ::  Sghvec

integer(kind=irg)                       :: ip, ir, ic, ikk, n
real(kind=sgl)                          :: Znsq, DBWF, kkl
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: ctmp(192,3)
real(kind=dbl)                          :: arg, tpi
type(reflisttype),pointer               :: rltmpa, rltmpb

  tpi = 2.D0 * cPi
  Sghvec = cmplx(0.D0,0.D0)

! for each special position we need to compute its contribution to the Sgh array
  do ip=1,cell % ATOM_ntype
    call CalcOrbit(cell,ip,n,ctmp)
    nat(ip) = cell%numat(ip)
! get Zn-squared for this special position, and include the site occupation parameter as well
    Znsq = float(cell%ATOM_type(ip))**2 * cell%ATOM_pos(ip,4)
! We'll assume isotropic Debye-Waller factors for now ...
! That means we need the square of the length of s=kk^2/4
    kkl = 0.25 * CalcLength(cell,float(kkk),'r')**2
! Debye-Waller exponential times Z^2
    DBWF = Znsq * exp(-cell%ATOM_pos(ip,5)*kkl)
! here is where we insert the proper weight factor, Z^2 exp[-M_{h-g}]
! and also the detector geometry...   For now, we do nothing with the detector
! geometry; the Rossouw et al 1994 paper lists a factor A that does not depend
! on anything in particular, so we assume it is 1. 
    do ikk=1,n ! sum over all the atoms in this orbit
! get the argument of the complex exponential
      arg = tpi*sum(dble(kkk(1:3))*cell%apos(ip,ikk,1:3))
      carg = cmplx(dcos(arg),dsin(arg))
! multiply with the prefactor and add
      Sghvec(ip) = Sghvec(ip) + carg * cmplx(DBWF,0.D0)
    end do
  end do
  
end subroutine preCalcSgh


!--------------------------------------------------------------------------
!
! SUBROUTINE: preCalcSghEEC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like Sgh array entry for EEC simulations
!
!> @note this may need some more work ... 
!
!> @param cell unit cell pointer
!> @param kkk input g-vector
!> @param nSites number of atom positions in asymmetric unit
!> @param iSites equivalent atom postions array
!> @param Sghvec output array
!
!> @date 12/13/19  MDG 1.0 original 
!--------------------------------------------------------------------------
recursive subroutine preCalcSghEEC(cell,kkk,nSites,iSites,Sghvec)
!DEC$ ATTRIBUTES DLLEXPORT :: preCalcSghEEC

use local
use typedefs
use crystal
use gvectors
use constants
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: kkk(3)
integer(kind=irg),INTENT(IN)            :: nSites
real(kind=dbl),INTENT(IN)               :: iSites(192,3)
complex(kind=dbl),INTENT(INOUT)         :: Sghvec
!f2py intent(in,out) ::  Sghvec

integer(kind=irg)                       :: ip, ir, ic, ikk, n
real(kind=sgl)                          :: Znsq, DBWF, kkl
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: arg, tpi
type(reflisttype),pointer               :: rltmpa, rltmpb

tpi = 2.D0 * cPi
Sghvec = cmplx(0.D0,0.D0)

! for the isotope positions, we need to compute their contribution to the Sgh array
! We'll assume isotropic Debye-Waller factors for now ...
! That means we need the square of the length of s=kk^2/4
    ! kkl = 0.25 * CalcLength(cell,float(kkk),'r')**2
! Debye-Waller exponential times Z^2
    ! DBWF = Znsq * exp(-cell%ATOM_pos(ip,5)*kkl)
! here is where we insert the proper weight factor, Z^2 exp[-M_{h-g}]
! and also the detector geometry...   For now, we do nothing with the detector
! geometry; the Rossouw et al 1994 paper lists a factor A that does not depend
! on anything in particular, so we assume it is 1. 
do ikk=1,nSites ! sum over all the atoms in this orbit
! get the argument of the complex exponential
  arg = tpi*sum(dble(kkk(1:3))*iSites(ikk,1:3))
  carg = cmplx(dcos(arg),dsin(arg))
! multiply with the prefactor and add
  Sghvec = Sghvec + carg ! * cmplx(DBWF,0.D0)
end do

  
end subroutine preCalcSghEEC


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcSgh
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like array for EBSD, ECCI and ECP simulations
!
!> @param cell unit cell pointer
!> @param nn dimension of array
!> @param Sgh output array
!> @param nat normalization array
!
!> @date 03/05/14  MDG 1.0 original (used to be in-line in ECP and ECCI programs)
!> @date 03/11/14  MDG 1.1 converted to diagonal Sgh array only
!> @date 06/19/14  MDG 2.0 no globals, taken out of EMECCI.f90
!> @date 09/07/15  MDG 2.1 added zeroing of Sgh array
!--------------------------------------------------------------------------
recursive subroutine CalcSgh(cell,reflist,nn,numset,Sgh,nat)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcSgh

use local
use typedefs
use crystal
use gvectors
use constants
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell
type(reflisttype),pointer               :: reflist
integer(kind=irg),INTENT(IN)            :: nn
integer(kind=irg),INTENT(IN)            :: numset
complex(kind=dbl),INTENT(INOUT)         :: Sgh(nn,nn,numset)
!f2py intent(in,out) ::  Sgh
integer(kind=irg),INTENT(INOUT)         :: nat(maxpasym)
!f2py intent(in,out) ::  nat

integer(kind=irg)                       :: ip, ir, ic, kkk(3), ikk, n
real(kind=sgl)                          :: Znsq, DBWF, kkl
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: ctmp(192,3),arg, tpi
type(reflisttype),pointer               :: rltmpa, rltmpb

  tpi = 2.D0 * cPi
  Sgh = cmplx(0.D0,0.D0)

! for each special position we need to compute its contribution to the Sgh array
  do ip=1,cell % ATOM_ntype
    call CalcOrbit(cell,ip,n,ctmp)
    nat(ip) = cell%numat(ip)
! get Zn-squared for this special position, and include the site occupation parameter as well
    Znsq = float(cell%ATOM_type(ip))**2 * cell%ATOM_pos(ip,4)

! loop over all contributing reflections
! ir is the row index
    rltmpa => reflist%next    ! point to the front of the list
    do ir=1,nn
! ic is the column index
      rltmpb => reflist%next    ! point to the front of the list
      do ic=1,nn
        kkk = rltmpb%hkl - rltmpa%hkl
! We'll assume isotropic Debye-Waller factors for now ...
! That means we need the square of the length of s=  kk^2/4
        kkl = 0.25 * CalcLength(cell,float(kkk),'r')**2
! Debye-Waller exponential times Z^2
        DBWF = Znsq * exp(-cell%ATOM_pos(ip,5)*kkl)
! here is where we insert the proper weight factor, Z^2 exp[-M_{h-g}]
! and also the detector geometry...   For now, we do nothing with the detector
! geometry; the Rossouw et al 1994 paper lists a factor A that does not depend
! on anything in particular, so we assume it is 1. 
        do ikk=1,n
! get the argument of the complex exponential
          arg = tpi*sum(dble(kkk(1:3))*cell%apos(ip,ikk,1:3))
          carg = cmplx(dcos(arg),dsin(arg))
! multiply with the prefactor and add
          Sgh(ir,ic,ip) = Sgh(ir,ic,ip) + carg * cmplx(DBWF,0.D0)
        end do
        rltmpb => rltmpb%nexts  ! move to next column-entry
      end do
     rltmpa => rltmpa%nexts  ! move to next row-entry
   end do  
  end do
  
end subroutine CalcSgh

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcSghMaster
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute structure factor-like array for EBSD, ECCI and ECP simulations
!> WITHOUT the application of bethe potential (replace nexts pointer by next)
!
!> @param cell unit cell pointer
!> @param nn dimension of array
!> @param Sgh output array
!> @param nat normalization array
!
!> @date 04/22/15  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcSghMaster(cell,reflist,nn,numset,Sgh,nat)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcSghMaster

use local
use typedefs
use crystal
use gvectors
use constants
use symmetry

IMPLICIT NONE

type(unitcell)                          :: cell
type(reflisttype),pointer               :: reflist
integer(kind=irg),INTENT(IN)            :: nn
integer(kind=irg),INTENT(IN)            :: numset
complex(kind=dbl),INTENT(INOUT)         :: Sgh(nn,nn,numset)
!f2py intent(in,out) ::  Sgh
integer(kind=irg),INTENT(INOUT)         :: nat(maxpasym)
!f2py intent(in,out) ::  nat

integer(kind=irg)                       :: ip, ir, ic, kkk(3), ikk, n
real(kind=sgl)                          :: Znsq, DBWF, kkl
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: ctmp(192,3),arg, tpi
type(reflisttype),pointer               :: rltmpa, rltmpb

tpi = 2.D0 * cPi
Sgh = cmplx(0.D0,0.D0)

! for each special position we need to compute its contribution to the Sgh array
do ip=1,cell % ATOM_ntype
    call CalcOrbit(cell,ip,n,ctmp)
    nat(ip) = cell%numat(ip)
! get Zn-squared for this special position, and include the site occupation parameter as well
    Znsq = float(cell%ATOM_type(ip))**2 * cell%ATOM_pos(ip,4)
! loop over all contributing reflections
! ir is the row index
    rltmpa => reflist%next    ! point to the front of the list
    do ir=1,nn
! ic is the column index
        rltmpb => reflist%next    ! point to the front of the list
        do ic=1,nn
            kkk = rltmpb%hkl - rltmpa%hkl
! We'll assume isotropic Debye-Waller factors for now ...
! That means we need the square of the length of s=  kk^2/4
            kkl = 0.25 * CalcLength(cell,float(kkk),'r')**2
! Debye-Waller exponential times Z^2
            DBWF = Znsq * exp(-cell%ATOM_pos(ip,5)*kkl)
! here is where we insert the proper weight factor, Z^2 exp[-M_{h-g}]
! and also the detector geometry...   For now, we do nothing with the detector
! geometry; the Rossouw et al 1994 paper lists a factor A that does not depend
! on anything in particular, so we assume it is 1.
            do ikk=1,n
! get the argument of the complex exponential
!         arg = tpi*sum(kkk(1:3)*ctmp(ikk,1:3))
                arg = tpi*sum(kkk(1:3)*cell%apos(ip,ikk,1:3))
                carg = cmplx(dcos(arg),dsin(arg))
! multiply with the prefactor and add
                Sgh(ir,ic,ip) = Sgh(ir,ic,ip) + carg * cmplx(DBWF,0.D0)
            end do
            rltmpb => rltmpb%next  ! move to next column-entry
        end do
        rltmpa => rltmpa%next  ! move to next row-entry
    end do
end do

end subroutine CalcSghMaster

! ###################################################################
!
! SUBROUTINE: CalcLgh
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the Lgh matrix for EBSD, ECCI, ECP, etc simulations
!
!> @param DMat dynamical matrix
!> @param Lgh output array
!> @param thick integration thickness
!> @param kn normal wave vector component
!> @param nn number of strong beams
!> @param gzero index of incident beam
!> @param depthstep depth step size
!> @param lambdaE energy weight factors
!> @param izz number of energy weight factors
!
!> @date 10/13/98  MDG 1.0 original
!> @date 07/04/01  MDG 2.0 f90
!> @date 06/19/14  MDG 3.0 no globals
!> @date 06/23/14  MDG 4.0 moved to MBmodule
!> @date 09/09/15  MDG 4.1 verification of matrix multiplications after Silicon pattern issues
! ###################################################################
recursive subroutine CalcLgh(DMat,Lgh,thick,kn,nn,gzero,depthstep,lambdaE,izz)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcLgh

use local
use io
use files
use constants
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: nn
complex(kind=dbl),INTENT(IN)        :: DMat(nn,nn)
complex(kind=dbl),INTENT(OUT)       :: Lgh(nn,nn)
real(kind=dbl),INTENT(IN)           :: thick
real(kind=dbl),INTENT(IN)           :: kn
integer(kind=irg),INTENT(IN)        :: gzero
real(kind=dbl),INTENT(IN)           :: depthstep
integer(kind=irg),INTENT(IN)        :: izz
real(kind=sgl),INTENT(IN)           :: lambdaE(izz)

integer                             :: i,j,k, iz
complex(kind=dbl)                   :: CGinv(nn,nn), Minp(nn,nn), tmp3(nn,nn)

real(kind=dbl)                      :: tpi, dzt
complex(kind=dbl)                   :: Ijk(nn,nn), q, getMIWORK(1), qold

integer(kind=irg)                   :: INFO, LDA, LDVR, LDVL,  JPIV(nn), MILWORK
complex(kind=dbl)                   :: CGG(nn,nn), W(nn)
complex(kind=dbl),allocatable       :: MIWORK(:)

integer(kind=irg),parameter         :: LWMAX = 5000 
complex(kind=dbl)                   :: VL(nn,nn),  WORK(LWMAX)
real(kind=dbl)                      :: RWORK(2*nn)
character                           :: JOBVL, JOBVR
integer(kind=sgl)                   :: LWORK

! compute the eigenvalues and eigenvectors
! using the LAPACK CGEEV, CGETRF, and CGETRI routines
! 
 Minp = DMat

! set some initial LAPACK variables 
 LDA = nn
 LDVL = nn
 LDVR = nn
 INFO = 0

 ! first initialize the parameters for the LAPACK ZGEEV, CGETRF, and CGETRI routines
 JOBVL = 'N'   ! do not compute the left eigenvectors
 JOBVR = 'V'   ! do compute the right eigenvectors
 LWORK = -1 ! so that we can ask the routine for the actually needed value

! call the routine to determine the optimal workspace size
 LDA = nn
  call zgeev(JOBVL,JOBVR,nn,Minp,LDA,W,VL,LDVL,CGG,LDVR,WORK,LWORK,RWORK,INFO)
  LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )

! then call the eigenvalue solver
  LDA = nn
  call zgeev(JOBVL,JOBVR,nn,Minp,LDA,W,VL,LDVL,CGG,LDVR,WORK,LWORK,RWORK,INFO)
  if (INFO.ne.0) call FatalError('Error in CalcLgh3: ','ZGEEV return not zero')

 CGinv = CGG
 
 LDA=nn
 call zgetrf(nn,nn,CGinv,LDA,JPIV,INFO)
 MILWORK = -1
 LDA=nn
 call zgetri(nn,CGinv,LDA,JPIV,getMIWORK,MILWORK,INFO)
 MILWORK =  INT(real(getMIWORK(1)))
 if (.not.allocated(MIWORK)) allocate(MIWORK(MILWORK))
 MIWORK = cmplx(0.D0,0.D0)
 LDA=nn
 call zgetri(nn,CGinv,LDA,JPIV,MIWORK,MILWORK,INFO)
 deallocate(MIWORK)

! in all the time that we've used these routines, we haven't
! had a single problem with the matrix inversion, so we don't
! really need to do this test:
!
! if ((cabs(sum(matmul(CGG,CGinv)))-dble(nn)).gt.1.E-8) write (*,*) 'Error in matrix inversion; continuing'


! then compute the integrated intensity matrix
 W = W/cmplx(2.0*kn,0.0)

! recall that alpha(1:nn) = CGinv(1:nn,gzero)

! first the Ijk matrix (this is Winkelmann's B^{ij}(t) matrix)
! combined with numerical integration over [0, z0] interval,
! taking into account depth profiles from Monte Carlo simulations ...
! the depth profile lambdaE must be added to the absorption 
! components of the Bloch wave eigenvalues.

tpi = 2.D0*cPi*depthstep
dzt = depthstep/thick
 do j=1,nn
  do k=1,nn
     q =  cmplx(0.D0,0.D0)
     qold = cmplx(tpi*(aimag(W(j))+aimag(W(k))),tpi*(real(W(j))-real(W(k))))
     if(real(qold) .lt. 0.0) qold = -qold
     !if(real(qold) .lt. 0.D0) qold = -qold
     do iz = 1,izz
       q = q + dble(lambdaE(iz)) * exp( - qold * dble(iz) ) !MNS changed cexp to cdexp to be compatible with gfortran
     end do
     Ijk(j,k) = conjg(CGinv(j,gzero)) * q * CGinv(k,gzero)
  end do
 end do
Ijk = Ijk * dzt

! then the matrix multiplications to obtain Lgh 
tmp3 = matmul(conjg(CGG),Ijk) 
Lgh = matmul(tmp3,transpose(CGG))

end subroutine CalcLgh

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetDynMat
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, including Bethe potentials
!
!> details We compute the dynamical matrix as the structure matrix A, with 
!> the q_g elements along the off-diagonal; the reaso nfor this is the fact
!> that this approach leads to a dynamical matrix that is shift invariant.
!> A conversion to the Bloch wave dynamical matrix can be obtained by setting 
!> the optional keyword BlochMode
!
!> @param cell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!> @param BlochMode [optional] Bloch or Struc
!> @param noNormAbs [optional and experimental] no normal absorption
!
!> @date  04/22/14 MDG 1.0 new library version
!> @date  06/15/14 MDG 2.0 updated for removal of globals
!> @date  06/17/14 MDG 2.1 added listroot pointers etc to accommodate multiple threads
!> @date  06/18/14 MDG 2.2 corrected some pointer allocation errors in other routines; this one now works fine.
!> @date  09/08/15 MDG 3.0 rewrite to allow either dynamical matrix type (Bloch/structure matrix) to be generated
!> @date  09/14/15 SS  3.1 added exp(-pi/xgp) to the diagonal elements of the bloch dynamical matrix
!> @date  11/21/19 MDG 3.2 optional parameter to turn off inclusion of normal absorption on diagonal
!--------------------------------------------------------------------------
recursive subroutine GetDynMat(cell, listroot, listrootw, rlp, DynMat, nns, nnw, BlochMode, noNormAbs)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDynMat

use local
use typedefs
use io
use crystal
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

type(unitcell)                   :: cell
type(reflisttype),pointer        :: listroot
type(reflisttype),pointer        :: listrootw
type(gnode),INTENT(INOUT)        :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN)     :: nns
complex(kind=dbl),INTENT(INOUT)  :: DynMat(nns,nns)
!f2py intent(in,out) ::  DynMat
integer(kind=irg),INTENT(IN)     :: nnw
character(5),INTENT(IN),OPTIONAL :: BlochMode   ! 'Bloch' or 'Struc'
logical,INTENT(IN),OPTIONAL      :: noNormAbs

complex(kind=dbl)                :: czero, ughp, uhph, weaksum, cv, Agh, Ahgp, Ahmgp, Ahg, weakdiagsum, pq0, Ahh, Agpgp, ccpi 
real(kind=dbl)                   :: weaksgsum, tpi, Pioxgp
real(kind=sgl)                   :: Upz
integer(kind=sgl)                :: ir, ic, ll(3), istat, wc
type(reflisttype),pointer        :: rlr, rlc, rlw
character(1)                     :: AorD

czero = cmplx(0.0,0.0,dbl)      ! complex zero
tpi = 2.D0 * cPi
ccpi = cmplx(cPi,0.0D0,dbl)

nullify(rlr)
nullify(rlc)
nullify(rlw)

! if BlochMode is absent, then we compute the Bloch dynamical matrix D directly
! if Blochmode = Struc, we compute the structure matrix A directly
! if Blochmode = Bloch, we do compute the structure matrix A and convert it to D
! [in the absence of the BlochMode keyword, the dynamical matrix D
! will not be invariant to origin shifts; A, on the other hand, is always shift
! invariant, so that D derived from A will also be shift invariant]

AorD = 'D'
if (present(Blochmode)) AorD = 'A'

! Standard Bloch wave mode
if (AorD.eq.'D') then

        DynMat = czero
        call CalcUcg(cell, rlp, (/0,0,0/) )
        Upz = rlp%Upmod
        if (present(noNormAbs)) then 
          if (noNormAbs.eqv..TRUE.) then 
            Upz = 0.0
          end if
        end if
        !Pioxgp = cPi/rlp%xgp

        rlr => listroot%next
        ir = 1
        do
          if (.not.associated(rlr)) EXIT
          rlc => listroot%next
          ic = 1
          do
          if (.not.associated(rlc)) EXIT
          if (ic.ne.ir) then  ! not a diagonal entry
! here we need to do the Bethe corrections if necessary
            if (nnw.ne.0) then
              weaksum = czero
              rlw => listrootw
              do
               if (.not.associated(rlw)) EXIT
               ll = rlr%hkl - rlw%hkl
               ughp = cell%LUT(ll(1),ll(2),ll(3)) 
               ll = rlw%hkl - rlc%hkl
               uhph = cell%LUT(ll(1),ll(2),ll(3)) 
               weaksum = weaksum +  ughp * uhph *cmplx(1.D0/rlw%sg,0.0,dbl)
               rlw => rlw%nextw
              end do
!        ! and correct the dynamical matrix element to become a Bethe potential coefficient
              ll = rlr%hkl - rlc%hkl
              DynMat(ir,ic) = cell%LUT(ll(1),ll(2),ll(3))  - cmplx(0.5D0*cell%mLambda,0.0D0,dbl)*weaksum
            else
              ll = rlr%hkl - rlc%hkl
              DynMat(ir,ic) = cell%LUT(ll(1),ll(2),ll(3))
            end if
          else  ! it is a diagonal entry, so we need the excitation error and the absorption length
! determine the total contribution of the weak beams
            if (nnw.ne.0) then
              weaksgsum = 0.D0
              rlw => listrootw
              do
               if (.not.associated(rlw)) EXIT
                ll = rlr%hkl - rlw%hkl
                ughp = cell%LUT(ll(1),ll(2),ll(3)) 
                weaksgsum = weaksgsum +  abs(ughp)**2/rlw%sg
                rlw => rlw%nextw
              end do
              weaksgsum = weaksgsum * cell%mLambda/2.D0
              DynMat(ir,ir) = cmplx(2.D0*rlr%sg/cell%mLambda-weaksgsum,Upz,dbl)
            else
              DynMat(ir,ir) = cmplx(2.D0*rlr%sg/cell%mLambda,Upz,dbl)
            end if           
 
           end if       
           rlc => rlc%nexts
           ic = ic + 1
          end do        
          rlr => rlr%nexts
          ir = ir+1
        end do

else ! AorD = 'A' so we need to compute the structure matrix using LUTqg ... 

! note that the factor of i pi is added in at the end...
        DynMat = czero
        call CalcUcg(cell, rlp, (/0,0,0/),applyqgshift=.TRUE. )
        pq0 = cmplx(0.D0,1.D0/rlp%xgp,dbl)

        rlr => listroot%next
        ir = 1
        do
          if (.not.associated(rlr)) EXIT
          rlc => listroot%next
          ic = 1
          do
          if (.not.associated(rlc)) EXIT
          if (ic.ne.ir) then  ! not a diagonal entry
! here we need to do the Bethe corrections if necessary
            if (nnw.ne.0) then
              weaksum = czero
              rlw => listrootw
              do
               if (.not.associated(rlw)) EXIT
               ll = rlr%hkl - rlw%hkl
               Agh = cell%LUTqg(ll(1),ll(2),ll(3)) 
               ll = rlw%hkl - rlc%hkl
               Ahgp = cell%LUTqg(ll(1),ll(2),ll(3)) 
! denominator Ahh - Ag'g'
               Ahh = cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
               Agpgp = cmplx(2.D0 * rlc%sg,0.D0,dbl) + pq0
               weaksum = weaksum +  Agh * Ahgp / (Ahh - Agpgp)
               rlw => rlw%nextw
              end do
! and correct the dynamical matrix element to become a Bethe potential coefficient
              ll = rlr%hkl - rlc%hkl
              DynMat(ir,ic) = cell%LUTqg(ll(1),ll(2),ll(3))  -  weaksum
             else
              ll = rlr%hkl - rlc%hkl
              DynMat(ir,ic) = cell%LUTqg(ll(1),ll(2),ll(3))
            end if
          else  ! it is a diagonal entry, so we need the excitation error and the absorption length
! determine the total contribution of the weak beams
            if (nnw.ne.0) then
              weakdiagsum = 0.D0
              rlw => listrootw
              do
               if (.not.associated(rlw)) EXIT
                ll = rlr%hkl - rlw%hkl
                Agh = cell%LUTqg(ll(1),ll(2),ll(3)) 
                Ahg = cell%LUTqg(-ll(1),-ll(2),-ll(3)) 
! denominator Ahh - Agg
                Ahh = cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
                Agpgp = cmplx(2.D0 * rlr%sg,0.D0,dbl) + pq0
                weakdiagsum = weakdiagsum +  Agh * Ahg  / (Ahh - Agpgp)
                rlw => rlw%nextw
              end do
              DynMat(ir,ir) = cmplx( 2.D0 * rlr%sg, 0.D0, dbl) + pq0 - weakdiagsum
            else
              DynMat(ir,ir) = cmplx( 2.D0 * rlr%sg, 0.D0,dbl) + pq0 
            end if           
        
           end if       
           rlc => rlc%nexts
           ic = ic + 1
          end do        
          rlr => rlr%nexts
          ir = ir+1
        end do
        DynMat = DynMat * ccpi ! cmplx(cPi, 0.D0)
end if 


if (present(BlochMode)) then
  if (BlochMode.eq.'Bloch') then
    cv = cmplx(1.D0/cPi/cell%mLambda,0.D0)
    DynMat = DynMat * cv
  end if
end if

end subroutine GetDynMat


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetDynMatMaster
!
!> @author Saransh, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, WITHOUT Bethe potentials
!
!> @param cell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!
!> @date  04/22/14 MDG 1.0 new library version
!> @date  06/15/14 MDG 2.0 updated for removal of globals
!> @date  06/17/14 MDG 2.1 added listroot pointers etc to accommodate multiple threads
!> @date  06/18/14 MDG 2.2 corrected some pointer allocation errors in other routines; this one now works fine.
!--------------------------------------------------------------------------
recursive subroutine GetDynMatMaster(cell, listroot, DynMat, nref)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDynMatMaster

use local
use typedefs
use io
use crystal
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

type(unitcell)                   :: cell
type(reflisttype),pointer        :: listroot
integer(kind=irg),INTENT(IN)     :: nref
complex(kind=dbl),INTENT(INOUT)  :: DynMat(nref,nref)
!f2py intent(in,out) ::  DynMat

complex(kind=dbl)                :: czero, ughp, uhph, weaksum, qg0
real(kind=dbl)                   :: weaksgsum
real(kind=sgl)                   :: Upz
integer(kind=sgl)                :: ir, ic, ll(3), istat, wc
type(reflisttype),pointer        :: rlr, rlc, rlw

czero = cmplx(0.0,0.0,dbl)      ! complex zero

nullify(rlr)
nullify(rlc)

DynMat = czero

qg0 = cell%LUTqg(0,0,0)

rlr => listroot%next
ir = 1
    do
        if (.not.associated(rlr)) EXIT
        rlc => listroot%next
        ic = 1
        do
            if (.not.associated(rlc)) EXIT
            if (ic.ne.ir) then  ! not a diagonal entry
                ll = rlr%hkl - rlc%hkl
                DynMat(ir,ic) = cell%LUTqg(ll(1),ll(2),ll(3))
            else
                DynMat(ir,ic) = cmplx(2.D0*rlc%sg,0.D0) + qg0
            end if
            rlc => rlc%next
            ic = ic + 1
        end do
        rlr => rlr%next
        ir = ir+1
    end do

DynMat = DynMat * cmplx(cPi,0.D0)

end subroutine GetDynMatMaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:CalcLghECP
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the Bloch wave Lgh matrix for ECP mode
!
!> @param nn number of strong beams
!> @param nt number of thickness values
!> @param thick array of thickness values
!> @param kn normal component of incident wave vector
!> @param gzero index of zero beam (should always be the first one; legacy parameter)
!> @param Lgh output array
!
!> @date 11/18/13  MDG 1.0 major rewrite from older ECP program; merged with ECPz
!--------------------------------------------------------------------------
recursive subroutine CalcLghECP(DMat,Lgh,nn,nt,thick,kn,gzero)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcLghECP

use local
use io
use files
use diffraction
use constants

IMPLICIT NONE

integer(kind=sgl),INTENT(IN)        :: nn
integer(kind=sgl),INTENT(IN)        :: nt
complex(kind=dbl),INTENT(IN)        :: DMat(nn,nn)
complex(kind=dbl),INTENT(OUT)       :: Lgh(nn,nn,nt)
real(kind=sgl),INTENT(IN)           :: thick(nt)
real(kind=dbl),INTENT(IN)           :: kn
integer(kind=sgl),INTENT(IN)        :: gzero

integer                             :: i,j,it,ig,ih,IPIV(nn)
complex(kind=dbl),allocatable       :: CGinv(:,:), Minp(:,:),tmp3(:,:)
complex(kind=dbl)                   :: Ijk(nn,nn),q
complex(kind=dbl)                   :: CG(nn,nn), W(nn)


allocate(CGinv(nn,nn),Minp(nn,nn),tmp3(nn,nn))

! compute the eigenvalues and eigenvectors
! using the LAPACK CGEEV, CGETRF, and CGETRI routines
! 
! then get the eigenvalues and eigenvectors
 Minp = DMat
 IPIV = 0

 call BWsolve(Minp,W,CG,CGinv,nn,IPIV)

! then compute the integrated intensity matrix
 W = W/cmplx(2.0*kn,0.0)

! first do the Lgh matrices, looping over the thickness
do it=1,nt
! recall that alpha(1:nn) = CGinv(1:nn,gzero)
! first the Ijk matrix
 do i=1,nn
  do j=1,nn
    q = 2.0*cPi*thick(it)*cmplx(aimag(W(i))+aimag(W(j)),real(W(i))-real(W(j)))
    Ijk(i,j) = conjg(CGinv(i,gzero)) * (1.0-exp(-q))/q * CGinv(j,gzero)
  end do
 end do

! then the summations for Lgh
 do ih=1,nn
   do i=1,nn
      tmp3(ih,i) = sum(Ijk(i,1:nn)*CG(ih,1:nn))
   end do
 end do
 do ig=1,nn
  do ih=1,nn
     Lgh(ih,ig,it) = sum(conjg(CG(ig,1:nn))*tmp3(ih,1:nn))
  end do
 end do
end do ! thickness loop

deallocate(CGinv,Minp,tmp3)

end subroutine CalcLghECP

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcSigmaggSubstrate
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute dynamical contribution array array for ECP simulations for a film+substrate system
!
!> @cell_subs unit cell ponter of substrate
!> @param nn dimension of array i.e. number of strong beams
!> @param nnk number of incident beams on the substrate
!> @param ScatMat Scattering matrix for the substrate
!> @param Sg initial beam amplitudes for all the incident beams
!> @param sigmagg output array
!> @param nt number of thickness values
!> @param thick array of thickness values
!> @param lambdaZ array of weight factors
!> @param filmthickness
!
!> @date 11/29/14  SS  1.0 extended to film+substrate system
!> @date 09/01/15  SS  1.1 complete rewrite
!--------------------------------------------------------------------------
recursive subroutine CalcsigmaggSubstrate(cell_subs,nns_film,refliststrong_subs,S0,Sigmagg,&
                                          filmthickness,substhickness,lambdaZ,thick,nt)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcsigmaggSubstrate
                                          

use local
use io
use files
use constants 
use typedefs
use crystal
use symmetry

IMPLICIT NONE

type(unitcell)                              :: cell_subs
integer(kind=irg),INTENT(IN)                :: nns_film
type(refliststrongsubstype),pointer         :: refliststrong_subs
complex(kind=dbl),INTENT(IN)                :: S0(nns_film)
real(kind=dbl),INTENT(OUT)                  :: Sigmagg(nns_film,nns_film)
integer(kind=irg),INTENT(IN)                :: nt
real(kind=sgl),INTENT(IN)                   :: thick(nt)
real(kind=sgl),INTENT(IN)                   :: lambdaZ(nt)
integer(kind=irg),INTENT(IN)                :: filmthickness
integer(kind=irg),INTENT(IN)                :: substhickness

real(kind=sgl)                              :: dthick
type(refliststrongsubstype),pointer         :: refliststrongtmp1,refliststrongtmp2
integer(kind=irg)                           :: ii,jj,kk,ll,mm,pp,qq,nns1,nns2,n,istat
complex(kind=dbl),allocatable               :: Sg(:),Sgp(:),ampl1(:),ampl2(:)
complex(kind=dbl)                           :: czero
complex(kind=dbl),allocatable               :: Shhp(:,:),Lhhp(:,:)
real(kind=dbl)                              :: ctmp(192,3),Znsq,arg1(3),arg2(3),arg3,arg4,s
complex(kind=sgl)                           :: phase1,phase2
integer(kind=irg),allocatable               :: nat(:)


dthick = thick(2) - thick(1)
Sigmagg = 0.D0
czero = cmplx(0.D0,0.D0)
allocate(nat(1:cell_subs%ATOM_ntype))
nullify(refliststrongtmp1)
nullify(refliststrongtmp2)

! go through the list twice and calculate interaction between each of the diffracted beams in the film

refliststrongtmp1 => refliststrong_subs
do ii = 1,nns_film
    nns1 = refliststrongtmp1%nns

    if(allocated(Sg)) deallocate(Sg)
    if(allocated(ampl1)) deallocate(ampl1)

    allocate(Sg(nns1),ampl1(nns1),stat=istat)
    if(istat .ne. 0) stop 'Cannot allocate incident vector'

    refliststrongtmp2 => refliststrong_subs
    do jj = 1,nns_film
      if (jj .ge. ii) then
        nns2 = refliststrongtmp2%nns

        Sg = czero
        Sg(1) = S0(ii)  ! incident amplitude for outer loop
        ampl1 = czero
 
        if(allocated(Sgp)) deallocate(Sgp)
        if(allocated(ampl2)) deallocate(ampl2)

        allocate(Sgp(nns2),ampl2(nns2),stat=istat)
        if(istat .ne. 0) stop 'Cannot allocate incident vector'
        Sgp = czero
        Sgp(1) = S0(jj) ! incident amplitude for inner loop
        ampl2 = czero

        if(allocated(Shhp)) deallocate(Shhp)
        if(allocated(Lhhp)) deallocate(Lhhp)

        allocate(Shhp(nns1,nns2),Lhhp(nns1,nns2),stat=istat)
        if(istat .ne. 0) stop 'Cannot allocate Shhp and Lhhp'
        Shhp = czero
        Lhhp = czero

        arg1(1:3) = (refliststrongtmp2%g(1:3) - refliststrongtmp1%g(1:3)) ! 2*pi*(gprime - g)
!print*,'ii, tmp1, length = ',ii,refliststrongtmp1%kg,calclength(cell_subs,refliststrongtmp1%kg,'r')
!print*,'jj, tmp2 ,length = ',jj,refliststrongtmp2%kg,calclength(cell_subs,refliststrongtmp2%kg,'r')
!print*,'tmp2 - tmp1 = ',refliststrongtmp2%kg - refliststrongtmp1%kg 
!print*,'' 
! calculation of Lhh for a pair of incident wavevectors g and gprime
        do kk=1,substhickness
           ampl1(1:nns1) = matmul(refliststrongtmp1%DynMat,Sg)
           ampl2(1:nns2) = matmul(refliststrongtmp2%DynMat,Sgp)

           Lhhp(1:nns1,1:nns2) = Lhhp(1:nns1,1:nns2) + lambdaZ(kk+filmthickness)*spread(conjg(ampl1(1:nns1)),dim=2,ncopies=nns2)*&
                                     spread(ampl2(1:nns2),dim=1,ncopies= nns1)

           Sg(1:nns1)  = ampl1(1:nns1)
           Sgp(1:nns1) = ampl2(1:nns2)

        end do

! calculation of Shh for a pair of incident wavevectors g and gprime

        do ll = 1,cell_subs%ATOM_ntype
            call CalcOrbit(cell_subs,ll,n,ctmp)
            nat(ll) = cell_subs%numat(ll)
! get Zn-squared for this special position, and include the site occupation parameter as well
            Znsq = float(cell_subs%ATOM_type(ll))**2 *cell_subs%ATOM_pos(ll,4)
            do pp = 1,nns1
                do kk = 1,nns2
                    do qq = 1,n
                        s = 0.25*CalcLength(cell_subs,refliststrongtmp2%hlist(kk,1:3)-refliststrongtmp1%hlist(pp,1:3),'r')**2
                        arg2 = (refliststrongtmp2%hlist(kk,1:3)-refliststrongtmp1%hlist(pp,1:3))
                        arg3 = 2.0*cPi*sum(arg1(1:3)*cell_subs%apos(ll,qq,1:3))
                        arg4 = 2.0*cPi*sum(arg2(1:3)*cell_subs%apos(ll,qq,1:3))
                        Shhp(pp,kk) = Shhp(pp,kk) + Znsq*exp(-cell_subs%ATOM_pos(ll,5)*s)*cmplx(dcos(arg3+arg4),dsin(arg3+arg4))
                    end do
                end do
            end do
        end do





        Sigmagg(ii,jj) = real(sum(Lhhp(1:nns1,1:nns2)*Shhp(1:nns1,1:nns2)))
        Sigmagg(ii,jj) = Sigmagg(ii,jj)/float(sum(nat(1:cell_subs%ATOM_ntype)))
        if (ii .ne. jj) Sigmagg(jj,ii) = Sigmagg(ii,jj)

      end if
        refliststrongtmp2 => refliststrongtmp2%next
    end do
    
    refliststrongtmp1 => refliststrongtmp1%next
end do

end subroutine CalcsigmaggSubstrate

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetStrongBeamsSubs
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief get list of reflections for each incident beam on substrate
!
!> @param cell_film unit cell pointer of film
!> @param cell_subs unit cell pointer of substrate
!> @param reflist_film reflection list pointer to film
!> @param refliststrong_subs pointer for list of strong reflections in substrate
!> @param k0 wavevector incident on the film
!> @param FN normal to the substrate i.e. gS
!> @param nns_film number of strong beams in film
!> @param dmin cutoff value for the g vectors used
!> @param TTinv transformation matrix for the orientation relation between film and substrate
!
!> @date   12/01/14 SS 1.0 original
!> @date   09/01/15 SS 1.1 matrix exponential performed in this subroutine now
!> @date   09/21/15 SS 1.2 added FN_film and FN_subs as substrate, now everything seems to be correct
!--------------------------------------------------------------------------

recursive subroutine GetStrongBeamsSubs(cell_film,cell_subs,reflist_film,refliststrong_subs,&
k0,FN_film,FN_subs,nns_film,dmin,TTinv,rlp_subs,dthick)
!DEC$ ATTRIBUTES DLLEXPORT :: GetStrongBeamsSubs


use typedefs
use diffraction
use crystal
use constants
use gvectors
use initializers
use math

IMPLICIT NONE

type(unitcell)                          :: cell_film,cell_subs
type(reflisttype),pointer               :: reflist_film
type(refliststrongsubstype),pointer     :: refliststrong_subs
real(kind=sgl),INTENT(IN)               :: k0(3),dmin
real(kind=dbl),INTENT(IN)               :: FN_film(3),FN_subs(3)
real(kind=sgl),INTENT(IN)               :: TTinv(3,3)
integer(kind=irg),INTENT(IN)            :: nns_film
real(kind=dbl),INTENT(IN)               :: dthick
type(gnode),INTENT(INOUT)                             :: rlp_subs
!f2py intent(in,out) ::  rlp_subs

type(reflisttype),pointer               :: rltmpa,rltmpb,reflist_subs,firstw_subs
type(refliststrongsubstype),pointer     :: refliststrong_subs_tmp
type(BetheParameterType)                :: BetheParameters
real(kind=sgl)                          :: kg(3),kg1(3)
integer(kind=irg)                       :: nref_subs,nns_subs,nnw_subs,ii,jj,kk,istat
complex(kind=dbl),allocatable           :: ScatMat(:,:)

call Set_Bethe_Parameters(BetheParameters,.TRUE.)

! convert to substrate reference frame

rltmpa => reflist_film%next
kg = k0 + dble(rltmpa%hkl) !+ rltmpa%sg*sngl(FN_film)
kg1 = Convert_kgs_to_Substrate(cell_film,cell_subs,kg,TTinv,sngl(FN_subs))

! calculate reflection list and dynamical matrix
call Initialize_ReflectionList(cell_subs, reflist_subs, BetheParameters, sngl(FN_subs), kg1, dmin, nref_subs)

call Apply_BethePotentials(cell_subs, reflist_subs, firstw_subs, BetheParameters, nref_subs, nns_subs, nnw_subs)

allocate(refliststrong_subs_tmp)
nullify(refliststrong_subs_tmp%next)
refliststrong_subs => refliststrong_subs_tmp

allocate(refliststrong_subs_tmp%hlist(nns_subs,3),stat=istat)
allocate(refliststrong_subs_tmp%DynMat(nns_subs,nns_subs),stat=istat)

refliststrong_subs_tmp%kg(1:3) = kg1(1:3)
refliststrong_subs_tmp%g(1:3) = dble(rltmpa%hkl)
refliststrong_subs_tmp%nns = nns_subs

call GetDynMat(cell_subs,reflist_subs,firstw_subs,rlp_subs,refliststrong_subs_tmp%DynMat,nns_subs,nnw_subs)

refliststrong_subs_tmp%DynMat = refliststrong_subs_tmp%DynMat*cmplx(0.D0,cPi*cell_subs%mLambda)

if(allocated(ScatMat)) deallocate(ScatMat)
allocate(ScatMat(nns_subs,nns_subs),stat=istat)
if(istat .ne. 0) stop 'Cannot allocate memory for scattering matrix'

! calculate exponential of matrix here for efficient computation

call MatrixExponential(refliststrong_subs_tmp%DynMat,ScatMat,dthick,'Pade',nns_subs)
refliststrong_subs_tmp%DynMat = ScatMat

! copy list of all strong beams for the particular incident wave vector

rltmpb => reflist_subs%next
do jj = 1,nns_subs
    refliststrong_subs%hlist(jj,1:3) = dble(rltmpb%hkl(1:3))
    rltmpb => rltmpb%nexts
end do

! repeat calculation for all the diffracted beams
rltmpa => rltmpa%nexts

do ii = 1,nns_film-1

    kg = k0 + dble(rltmpa%hkl) !+ (rltmpa%sg)*sngl(FN_film) !kg = k0 + g + sg*FN

    kg1 = Convert_kgs_to_Substrate(cell_film, cell_subs,kg, TTinv,sngl(FN_subs)) ! go to substrate frame

!print*,'k0 = ',k0
!print*,'hkl = ',dble(rltmpa%hkl)
!print*,'kg = ',k0+dble(rltmpa%hkl)
!print*,'kg1 = ',kg1
!print*,''
! initialize reflection list, apply bethe perturbation and get corresponding dynamical matrices

    call Initialize_ReflectionList(cell_subs, reflist_subs, BetheParameters, sngl(FN_subs), kg1, sngl(dmin), nref_subs)

    call Apply_BethePotentials(cell_subs, reflist_subs, firstw_subs, BetheParameters, nref_subs, nns_subs, nnw_subs)

    allocate(refliststrong_subs_tmp%next,stat=istat) 

    refliststrong_subs_tmp => refliststrong_subs_tmp%next
    allocate(refliststrong_subs_tmp%hlist(nns_subs,3),stat=istat)
    allocate(refliststrong_subs_tmp%DynMat(nns_subs,nns_subs),stat=istat)
    nullify(refliststrong_subs_tmp%next)

    call GetDynMat(cell_subs,reflist_subs,firstw_subs,rlp_subs,refliststrong_subs_tmp%DynMat,nns_subs,nnw_subs)
    
    refliststrong_subs_tmp%kg(1:3) = kg1(1:3)
    refliststrong_subs_tmp%g(1:3) = dble(rltmpa%hkl)

    refliststrong_subs_tmp%nns = nns_subs
    refliststrong_subs_tmp%DynMat = refliststrong_subs_tmp%DynMat*cmplx(0.D0,cPi*cell_subs%mLambda)

    if(allocated(ScatMat)) deallocate(ScatMat)
    allocate(ScatMat(nns_subs,nns_subs),stat=istat)
    if(istat .ne. 0) stop 'Cannot allocate memory for scattering matrix'

! take exponential of structure matrix to get scattering matrix

    call MatrixExponential(refliststrong_subs_tmp%DynMat,ScatMat,dthick,'Pade',nns_subs)
    refliststrong_subs_tmp%DynMat = ScatMat

! save list of reflections for this incident beam

    rltmpb => reflist_subs%next
    do jj = 1,nns_subs
        refliststrong_subs_tmp%hlist(jj,1:3) = dble(rltmpb%hkl(1:3))
        rltmpb => rltmpb%nexts
    end do

    rltmpa => rltmpa%nexts


end do

end subroutine GetStrongBeamsSubs

!--------------------------------------------------------------------------
!
! SUBROUTINE: Delete_StrongBeamList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief delete the entire linked list
!
!> @param top top of the list to be removed
!
!> @date   12/2/14 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Delete_StrongBeamList(self)
!DEC$ ATTRIBUTES DLLEXPORT :: Delete_StrongBeamList

use local
use typedefs

IMPLICIT NONE

type(refliststrongsubstype),pointer      :: self

type(refliststrongsubstype),pointer      :: current,next

current => self

do while (associated(current))
    next => current%next
    if (allocated(current%hlist)) then
        deallocate(current%hlist)
    end if
    if (allocated(current%DynMat)) then
        deallocate(current%DynMat)
    end if
    deallocate(current)
    nullify(current)
    current => next
end do

end subroutine Delete_StrongBeamList

!--------------------------------------------------------------------------


end module MBmodule
