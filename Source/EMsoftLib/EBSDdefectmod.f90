! ###################################################################
! Copyright (c) 2014-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EBSDdefectmod.f90
!--------------------------------------------------------------------------
!
! MODULE: EBSDdefectmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines to compute EBSD patterns in defective materials
!
!> @date     1/5/99 MDG 1.0 original
!--------------------------------------------------------------------------
module EBSDdefectmod

use local
use typedefs

IMPLICIT NONE


contains



! ###################################################################
!
! SUBROUTINE: CalcLghdepth
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
!> @date 11/04/19  MDG 1.0 original (forked from CalcLgh in MBmodule)
! ###################################################################
recursive subroutine CalcLghdepth(DMat,Lgh,thick,kn,nn,gzero,depthstep,lambdaE,izz)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcLghdepth

use local
use io
use files
use constants
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)        :: nn
complex(kind=dbl),INTENT(IN)        :: DMat(nn,nn)
complex(kind=dbl),INTENT(OUT)       :: Lgh(nn,nn,izz)
real(kind=dbl),INTENT(IN)           :: thick
real(kind=dbl),INTENT(IN)           :: kn
integer(kind=irg),INTENT(IN)        :: gzero
real(kind=dbl),INTENT(IN)           :: depthstep
integer(kind=irg),INTENT(IN)        :: izz
real(kind=sgl),INTENT(IN)           :: lambdaE(izz)

integer                             :: i,j,k, iz
complex(kind=dbl)                   :: CGinv(nn,nn), Minp(nn,nn), tmp3(nn,nn,izz)

real(kind=dbl)                      :: tpi, dzt
complex(kind=dbl)                   :: Ijk(nn,nn,izz), q, getMIWORK, qold

integer(kind=irg)                   :: INFO, LDA, LDVR, LDVL, JPIV(nn), MILWORK
complex(kind=dbl)                   :: CGG(nn,nn), W(nn)
complex(kind=dbl),allocatable       :: MIWORK(:)

integer(kind=irg),parameter         :: LWMAX = 5000 
complex(kind=dbl)                   :: VL(nn,nn),  WORK(LWMAX)
real(kind=dbl)                      :: RWORK(2*nn)
character                           :: JOBVL, JOBVR
integer(kind=sgl)                   :: LWORK
integer(kind=sgl)                   :: ilaenv
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
 
! we reset LDA to nn, because the intel f90 implementation of LAPACK uses wrappers
! to the f77 LAPACK routines !!! That means that the variable LDA is changed in the
! f77 routine, even though it is set to INTENT(IN) in the f90 version.  That's a 
! major issue with this library on the Windows platform. But, the work-around is 
! straightforward...

 LDA=nn
 call zgetrf(nn,nn,CGinv,LDA,JPIV,INFO)
 MILWORK = -1

 LDA=nn
 !call zgetri(nn,CGinv,LDA,JPIV,getMIWORK,MILWORK,INFO)
 !MILWORK =  INT(real(getMIWORK))
 MILWORK=ilaenv( 1, 'ZGETRI', ' ', nn, -1, -1, -1 )
 if (.not.allocated(MIWORK)) allocate(MIWORK(nn*MILWORK))
 MIWORK = cmplx(0.D0,0.D0)

 LDA=nn
 call zgetri(nn,CGinv,LDA,JPIV,MIWORK,MILWORK,INFO)
 deallocate(MIWORK)

! then compute the integrated intensity matrix
 W = W/cmplx(2.0*kn,0.0)

! recall that alpha(1:nn) = CGinv(1:nn,gzero)

! first the Ijk matrix (this is Winkelmann's B^{ij}(t) matrix)
! combined with numerical integration over [0, z0] interval,
! taking into account depth profiles from Monte Carlo simulations ...
! the depth profile lambdaE must be added to the absorption 
! components of the Bloch wave eigenvalues.

! for the depth specific case, we need to remove the depth integration compared
! to the conventional master pattern computation. 
! That is simply done by moving the Ijk computation inside the iz-loop,
! and using the depth specific q value.

tpi = 2.D0*cPi*depthstep
dzt = depthstep/thick
 do j=1,nn
  do k=1,nn
     q =  cmplx(0.D0,0.D0)
     qold = cmplx(tpi*(aimag(W(j))+aimag(W(k))),tpi*(real(W(j))-real(W(k))))
     if(real(qold) .lt. 0.0) qold = -qold
     do iz = 1,izz
       q = dble(lambdaE(iz)) * exp( - qold * dble(iz) )
       Ijk(j,k,iz) = conjg(CGinv(j,gzero)) * q * CGinv(k,gzero)
     end do
  end do
 end do
! finally, multiply by dz/z_0 for the discrete integration (so we 
! don't forget to do that in the calling program...) 
Ijk = Ijk * dzt

! then the matrix multiplications to obtain Lgh(z) 
do iz=1,izz
  tmp3(1:nn,1:nn,iz) = matmul(conjg(CGG),Ijk(1:nn,1:nn,iz)) 
end do
do iz=1,izz
  Lgh(1:nn,1:nn,iz) = matmul(tmp3(1:nn,1:nn,iz),transpose(CGG))
end do

end subroutine CalcLghdepth

!--------------------------------------------------------------------------
!
! SUBROUTINE: getSghfromLUTsum
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like Sgh array entry for EBSD, ECCI and ECP simulations
!  but in this case, sum over the lattice sites...
!
!> @param cell unit cell pointer
!> @param reflist pointer to reflection list
!> @param nns number of strong reflections
!> @param numset number of atom positions in asymmetric unit
!> @param nat normalization array
!> @param Sgh output array
!
!> @date 11/04/19  MDG 1.0 copied from original in MBmodule.f90 
!--------------------------------------------------------------------------
recursive subroutine getSghfromLUTsum(cell,reflist,nns,numset,nat,Sgh)
!DEC$ ATTRIBUTES DLLEXPORT :: getSghfromLUTsum

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
        Sgh(ir,ic) = sum(cell%SghLUT(1:numset,kkk(1),kkk(2),kkk(3)))
        rltmpb => rltmpb%nexts  ! move to next column-entry
      end do
     rltmpa => rltmpa%nexts  ! move to next row-entry
   end do  

end subroutine getSghfromLUTsum


end module EBSDdefectmod
