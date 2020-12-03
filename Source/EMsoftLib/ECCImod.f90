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
!--------------------------------------------------------------------------
! EMsoft:ECCImod.f90
!--------------------------------------------------------------------------
!
! MODULE: ECCImod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMECCI helper routines
!
!> @date 11/25/15 MDG 1.0 original
!---------------------------------------------------------------------------
module ECCImod

use local

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: Calckvectorcone
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a set of incident beam directions for single image ECCI mode
!
!> @param cell unit cell pointer
!> @param khead head of kvector linked list
!> @param k incident wave vector (zone axis)
!> @param ga principal g vector
!> @param ktxy tangential components
!> @param ktrad cone opening angle
!> @param ktstep number of steps along cone radius
!> @param numk resulting number of incident beam directions
!
!> @date 11/29/01 MDG 1.0 original
!> @date 12/05/13 MDG 2.0 adapted for ECCI simulations 
!> @date 12/01/15 MDG 2.1 simplification of input parameters
!--------------------------------------------------------------------------
recursive subroutine Calckvectorcone(cell,khead,k,ga,ktxy,ktrad,ktstep,numk)
!DEC$ ATTRIBUTES DLLEXPORT :: Calckvectorcone

use io
use error
use diffraction
use crystal
use kvectors

IMPLICIT NONE

type(unitcell)                      :: cell
type(kvectorlist),pointer           :: khead
integer(kind=irg),INTENT(IN)        :: k(3)
integer(kind=irg),INTENT(IN)        :: ga(3)
real(kind=sgl),INTENT(IN)           :: ktxy(2)
real(kind=sgl),INTENT(IN)           :: ktrad
integer(kind=irg),INTENT(IN)        :: ktstep
integer(kind=irg),INTENT(OUT)       :: numk

type(kvectorlist),pointer           :: ktmp,ktail
integer                             :: istat,imin,imax,jmin,jmax,ijmax,i,j,ic,jc,ki
real                                :: kr(3),glen,delta,kstar(3),kt(3),gan(3),gperp(3),ktlen, dkt

! compute geometrical factors 
 glen = CalcLength(cell,float(ga),'r')         ! length of ga
 gan = ga/glen                                 ! normalized ga
 delta = ktrad*glen/float(ktstep)              ! grid step size in nm-1 
 dkt = ktrad/float(ktstep)
 call TransSpace(cell,float(k),kstar,'d','r')       ! transform incident direction to reciprocal space
 call CalcCross(cell,float(ga),kstar,gperp,'r','r',0)! compute g_perp = ga x k
 call NormVec(cell,gperp,'r')                       ! normalize g_perp
 call NormVec(cell,kstar,'r')                       ! normalize reciprocal beam vector

! deal only with the incident beam (parallel illumination)
if (ktstep.eq.0) then
 if (.not.associated(khead)) then     ! allocate the head and ktail of the linked list
   allocate(khead,stat=istat)         ! allocate new value
   if (istat.ne.0) call FatalError('Calckvectorcone: unable to allocate head pointer',' ')
   ktail => khead                      ! ktail points to new value
   nullify(ktail%next)                ! nullify next in new value
   numk = 1                          ! keep track of number of k-vectors so far
 ! this should be the center vector of the illumination cone !!!
   kt = - glen * (ktxy(1)*gan + ktxy(2) * gperp)
   ktail%kt = kt                           ! store tangential component of k
   ktlen = glen**2*(ktxy(1)**2+ktxy(2)**2)         ! squared length of tangential component
   
   kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar ! complete wave vector
   ktail%k = kr                            ! store in pointer list
   ktail%kn = CalcDot(cell,ktail%k,dble(kstar),'r')    ! normal component of k
 end if
else
! next, put the center of the cone in units of (i,j) (original ECP "screen" coordinates)
  ic = int(ktxy(1)*glen/delta)
  jc = int(ktxy(2)*glen/delta)
  ki = ktstep

 if (.not.associated(khead)) then     ! allocate the head and ktail of the linked list
   allocate(khead,stat=istat)         ! allocate new value
   if (istat.ne.0) call FatalError('Calckvectorcone: unable to allocate head pointer',' ')
   ktail => khead                      ! ktail points to new value
   nullify(ktail%next)                ! nullify next in new value
   numk = 1                          ! keep track of number of k-vectors so far
 ! this should be the center vector of the illumination cone !!!
   ktail%i = ic                            ! i-index of beam
   ktail%j = jc                            ! j-index of beam
   kt = -float(ktail%i)*delta*gan - float(ktail%j)*delta*gperp  ! tangential component of k
   ktail%kt = kt                           ! store tangential component of k
   ktlen = delta**2*(ktail%i**2+ktail%j**2)         ! squared length of tangential component

   kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar ! complete wave vector
   ktail%k = kr                            ! store in pointer list
   ktail%kn = CalcDot(cell,ktail%k,dble(kstar),'r')    ! normal component of k
 else
   call FatalError('Calckvectorcone: pointer head already allocated',' ')
 end if

! the following lines are quite different if symmetry is taken into account;
! check the MBsym.f90 program to determine how that can be done.
  imin =  -ki; imax = ki; jmin = -ki; jmax = ki; 
  ijmax = ki**2
! now do the real work
  do i=imin,imax
   do j=jmin,jmax
    if (.not.((i.eq.0).and.(j.eq.0))) then  ! the point (0,0) has already been taken care of
     if ((i**2+j**2).le.ijmax) then   ! is point inside the incident cone ?
      allocate(ktail%next,stat=istat)  ! allocate new value
      if (istat.ne.0) call FatalError('Calckvectorcone: unable to allocate pointer',' ')
      ktail => ktail%next               ! ktail points to new value
      nullify(ktail%next)              ! nullify next in new value
      numk = numk + 1                 ! keep track of number of k-vectors so far
      ktail%i = ic+i                   ! i-index of beam
      ktail%j = jc+j                   ! j-index of beam
      kt = - float(ktail%i)*delta*gan - float(ktail%j)*delta*gperp  ! tangential component of k
      ktail%kt = kt                    ! store tangential component of k
      ktlen = delta**2*(ktail%i**2+ktail%j**2)         ! squared length of tangential component
 
      kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar ! complete wave vector
      ktail%k = kr                     ! store in pointer list
      ktail%kn = CalcDot(cell,ktail%k,dble(kstar),'r')    ! normal component of k
     end if
    end if
   end do
  end do
end if

end subroutine Calckvectorcone




!--------------------------------------------------------------------------
!
! SUBROUTINE: Calckvectortrace
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a set of incident beam directions for line scan ECCI mode
!
!> @param cell unit cell pointer
!> @param khead head of k-vector list
!> @param k incident wave vector (zone axis)
!> @param ga principal g vector
!> @param ktxy tangential components of trace start point
!> @param ktxy2 tangential components of trace end point
!> @param ktrad cone opening angle
!> @param ktstep number of steps along cone radius
!> @param numk resulting number of incident beam directions
!
!> @date 12/08/13 MDG 1.0 original
!> @date 12/01/15 MDG 1.1 simplifcation of input variables
!--------------------------------------------------------------------------
recursive subroutine Calckvectortrace(cell,khead,k,ga,ktxy,ktxy2,ktrad,ktstep,numk)
!DEC$ ATTRIBUTES DLLEXPORT :: Calckvectortrace

use io
use error
use diffraction
use crystal
use kvectors

IMPLICIT NONE

type(unitcell)                      :: cell
type(kvectorlist),pointer           :: khead
integer(kind=irg),INTENT(IN)        :: k(3)
integer(kind=irg),INTENT(IN)        :: ga(3)
real(kind=sgl),INTENT(IN)           :: ktxy(2)
real(kind=sgl),INTENT(IN)           :: ktxy2(2)
real(kind=sgl),INTENT(IN)           :: ktrad
integer(kind=irg),INTENT(IN)        :: ktstep
integer(kind=irg),INTENT(OUT)       :: numk

type(kvectorlist),pointer           :: ktail
integer                             :: istat,j
real                                :: kr(3),glen,delta,kstar(3),kt(3),gan(3),gperp(3),ktlen, dktx, dkty

! compute geometrical factors 
 glen = CalcLength(cell,float(ga),'r')              ! length of ga
 gan = ga/glen                                 ! normalized ga
 delta = 2.0*ktrad*glen/float(2*ktstep+1)      ! grid step size in nm-1 
 call TransSpace(cell,float(k),kstar,'d','r')       ! transform incident direction to reciprocal space
 call CalcCross(cell,float(ga),kstar,gperp,'r','r',0)! compute g_perp = ga x k
 call NormVec(cell,gperp,'r')                       ! normalize g_perp
 call NormVec(cell,kstar,'r')                       ! normalize reciprocal beam vector

 j = 0
 if (.not.associated(khead)) then     ! allocate the head and ktail of the linked list
   allocate(khead,stat=istat)         ! allocate new value
   if (istat.ne.0) call FatalError('Calckvectortrace: unable to allocate head pointer',' ')
   ktail => khead                     ! ktail points to new value
   nullify(ktail%next)                ! nullify next in new value
   numk = 1                           ! keep track of number of k-vectors so far
! this should be the starting point of the line trace
!   kt = - glen * ( ktxy(1)*gan + ktxy(2) * gperp)
   kt = - glen * ( ktxy(1)*gan - ktxy(2) * gperp)
   ktail%kt = kt                           ! store tangential component of k
   ktlen = glen**2*(ktxy(1)**2+ktxy(2)**2)         ! squared length of tangential component
   kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar ! complete wave vector
   ktail%k = kr                            ! store in pointer list
   ktail%kn = CalcDot(cell,ktail%k,dble(kstar),'r')    ! normal component of k
 end if
 
 dktx = (ktxy2(1) - ktxy(1))/float(ktstep-1)
 dkty = (ktxy2(2) - ktxy(2))/float(ktstep-1)

 do j=1,ktstep-1
      allocate(ktail%next,stat=istat)  ! allocate new value
      if (istat.ne.0) call FatalError('Calckvectortrace: unable to allocate pointer',' ')
      ktail => ktail%next              ! ktail points to new value
      nullify(ktail%next)              ! nullify next in new value
      numk = numk + 1                  ! keep track of number of k-vectors so far
!     kt = - glen * ( (ktxy(1)+float(j)*dktx)*gan + (ktxy(2)+float(j)*dkty) * gperp) ! tangential component of k
      kt = - glen * ( (ktxy(1)+float(j)*dktx)*gan - (ktxy(2)+float(j)*dkty) * gperp) ! tangential component of k
      ktail%kt = kt                    ! store tangential component of k
      ktlen = delta**2*(ktail%i**2+ktail%j**2)         ! squared length of tangential component
      kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar ! complete wave vector
      ktail%k = kr                     ! store in pointer list
      ktail%kn = CalcDot(cell,ktail%k,dble(kstar),'r')    ! normal component of k
 end do

end subroutine Calckvectortrace


!--------------------------------------------------------------------------
!
! SUBROUTINE: ECCICalcSgh
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like array for ECCI and ECP simulations
!
!> @param nn dimension of array
!> @param Sgh output array
!> @param nat normalization array
!
!> @date 03/05/14  MDG 1.0 original (used to be in-line in ECP and ECCI programs)
!> @date 03/11/14  MDG 1.1 converted to diagonal Sgh array only
!> @date 10/22/14  MDG 1.2 corrected definition of Sgh and simplified loops; resolves issue #3
!--------------------------------------------------------------------------
recursive subroutine ECCICalcSgh(cell,nn,Sgh,nat)
!DEC$ ATTRIBUTES DLLEXPORT :: ECCICalcSgh

use local
use crystal
use gvectors
use constants
use symmetry
use typedefs

IMPLICIT NONE

type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: nn
! corrected on 10/22/14 in response to issue #3 of release 2.0 raised by jfikar
! Sgh array changed to one dimensional vector instead of 2D array
complex(kind=dbl),INTENT(INOUT) :: Sgh(nn)
!f2py intent(in,out) ::  Sgh
integer(kind=irg),INTENT(INOUT) :: nat(maxpasym)
!f2py intent(in,out) ::  nat

integer(kind=irg)                       :: ip, ir, ic, kkk(3), ikk, n, numset
real(kind=sgl)                          :: Znsq, DBWF, kkl
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: ctmp(192,3),arg, tpi

  tpi = 2.D0 * cPi
  Sgh = cmplx(0.D0,0.D0)
  numset = cell % ATOM_ntype  ! number of special positions in the unit cell

! comment: this can likely be further simplified and we'll take a closer look at this for 
! the next release.

! for each special position we need to compute its contribution to the Sgh vector
  do ip=1,numset
    call CalcOrbit(cell,ip,n,ctmp)
    nat(ip) = n
! get Zn-squared for this special position, and include the site occupation parameter as well
    Znsq = float(cell%ATOM_type(ip))**2 * cell%ATOM_pos(ip,4)
! loop over all contributing reflections
! ir is the row index
! we don't need to actually go through this list, so the lines are commented out
!   rltmpa => reflist%next    ! point to the front of the list
!   do ir=1,nn  ! we only need a single summation loop since Sgh is a column vector
! Debye-Waller exponential times Z^2
     Sgh = Sgh + cmplx(n * Znsq, 0.D0) 
!    rltmpa => rltmpa%next  ! move to next row-entry
!   end do  
  end do
  
! this is the older code which I'm leaving here commented [10/22/14]
!! for each special position we need to compute its contribution to the Sgh array
!  do ip=1,numset
!    call CalcOrbit(ip,n,ctmp)
!    nat(ip) = n
!! get Zn-squared for this special position, and include the site occupation parameter as well
!    Znsq = float(cell%ATOM_type(ip))**2 * cell%ATOM_pos(ip,4)
!! loop over all contributing reflections
!! ir is the row index
!    rltmpa => reflist%next    ! point to the front of the list
!    do ir=1,nn
!! ic is the column index
!      rltmpb => reflist%next    ! point to the front of the list
!      do ic=1,nn
!        kkk = rltmpb%hkl - rltmpa%hkl
!! We'll assume isotropic Debye-Waller factors for now ...
!! That means we need the square of the length of s=  kk^2/4
!        kkl = 0.25 * CalcLength(float(kkk),'r')**2
!! Debye-Waller exponential times Z^2
!        DBWF = Znsq * exp(-cell%ATOM_pos(ip,5)*kkl)
!! here is where we should insert the proper weight factor, Z^2 exp[-M_{h-g}]
!! and also the detector geometry...   For now, we do nothing with the detector
!! geometry; the Rossouw et al 1994 paper lists a factor A that does not depend
!! on anything in particular, so we assume it is 1. 
!        do ikk=1,n
!! get the argument of the complex exponential
!          arg = tpi*sum(kkk(1:3)*ctmp(ikk,1:3))
!          carg = cmplx(dcos(arg),dsin(arg))
!! multiply with the prefactor and add
!          Sgh(ir,ic) = Sgh(ir,ic) + carg * cmplx(DBWF,0.D0)
!        end do
!        rltmpb => rltmpb%next  ! move to next column-entry
!      end do
!     rltmpa => rltmpa%next  ! move to next row-entry
!   end do  
!  end do
  

  
end subroutine ECCICalcSgh




!C***********************************************************************
!C
!C                        naninfchk.f
!C
!C      *****************************************************************
!C      *                                                               *
!C	* 	Absoft Corporation 					* 
!C 	*	2781 Bond Street					*
!C	*	Rochester Hills, MI  48309				*
!C	*								*
!C	*	This file contains example code for demonstration	*
!C	*	purposes only.  Absoft makes no warranty of the	* 
!C	*	suitability of this code for any purpose.		*
!C	*								*
!C	*	In no event shall Absoft be liable for any incidental,*
!C	*	indirect, special, or consequential damages arising	*
!C	*	out of the use of this code.				*
!C	*								*
!C	***************************************************************** 
!C
!C Routines to test real and double values against NaN and INF
!C
!C            NANCHK(X) - tests REAL*4 value X against NaN
!!C            DNANCHK(X) - tests REAL*8 value X against NaN
!!C            INFCHK(X) - tests REAL*4 value X against INF
!!C            DINFCHK(X) - test REAL*8 value X against INF
!C
!C For little endian machines (Intel x86), compile with
!C
!C      f77 -c -DBYTE_SWAPPED=1 naninfchk.f
!C	or
!C      f90 -c -DBYTE_SWAPPED=1 naninfchk.f -YBOZTYPE=INT
!C
!C For big endian machines (PowerPC), compile with
!C
!C      f77 -c naninfchk.f
!C	or
!C      f90 -c naninfchk.f -YBOZTYPE=INT
!C
!C***********************************************************************
RECURSIVE LOGICAL FUNCTION NANCHK(X)
!DEC$ ATTRIBUTES DLLEXPORT :: NANCHK

IMPLICIT NONE
REAL,INTENT(IN)      :: X
REAL                 :: Y
INTEGER              :: I
EQUIVALENCE(Y,I)

Y = X
NANCHK = isnan(Y) !((I .AND. z'7f800000') .EQ. z'7f800000') .AND.((I .AND. z'007fffff') .NE. z'00000000')

RETURN
END






end module ECCImod

