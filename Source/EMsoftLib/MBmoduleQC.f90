! ###################################################################
! Copyright (c) 2017-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:MBmoduleQC.f90
!--------------------------------------------------------------------------
!
! MODULE: MBmoduleQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Routines to handle 2-D quasi-crystals multibeam computatuions
!
!> @details Eventually, this module will be able to generate all the data structures
!> needed to perform dynamical simulations for QCs... in particular EBSD and ECP...
!
!> @date 06/25/18 SS 1.0 original
!--------------------------------------------------------------------------
module MBmoduleQC

use local
use typedefs
use constants
use qcrystal
use QCmod
use diffractionQC

IMPLICIT NONE

public

interface QC_GetDynMat
  module procedure QC_GetDynMat2DQC
  module procedure QC_GetDynMat3DQC
end interface QC_GetDynMat

interface QC_GetDynMatMaster
  module procedure QC_GetDynMatMaster2DQC
  module procedure QC_GetDynMatMaster3DQC
end interface QC_GetDynMatMaster

interface QC_CalcSgh
  module procedure TDQC_CalcSgh
  module procedure QC_CalcSgh
end interface QC_CalcSgh

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_GetDynMat2DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, including Bethe potentials, for a quasi-crystal
!
!> details We compute the dynamical matrix as the structure matrix A, with 
!> the q_g elements along the off-diagonal; the reason for this is the fact
!> that this approach leads to a dynamical matrix that is shift invariant.
!> A conversion to the Bloch wave dynamical matrix can be obtained by setting 
!> the optional keyword BlochMode
!
!> @param QCcell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!> @param BlochMode [optional] Bloch or Struc
!
!> @date  04/22/14 MDG 1.0 new library version
!> @date  06/15/14 MDG 2.0 updated for removal of globals
!> @date  06/17/14 MDG 2.1 added listroot pointers etc to accommodate multiple threads
!> @date  06/18/14 MDG 2.2 corrected some pointer allocation errors in other routines; this one now works fine.
!> @date  09/08/15 MDG 3.0 rewrite to allow either dynamical matrix type (Bloch/structure matrix) to be generated
!> @date  09/14/15 SS  3.1 added exp(-pi/xgp) to the diagonal elements of the bloch dynamical matrix
!> @date  03/23/18 SS  3.2 adapted from QCmod.f90
!> @date  06/25/18 SS  3.3 moved to MBmoduleQC module
!--------------------------------------------------------------------------
recursive subroutine QC_GetDynMat2DQC(QCcell, listroot, listrootw, DynMat, nns, nnw, BlochMode)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_GetDynMat2DQC

use local
use typedefs
use io
use constants

IMPLICIT NONE

type(TDQCStructureType),pointer     :: QCcell
type(TDQCreflisttype),pointer       :: listroot
type(TDQCreflisttype),pointer       :: listrootw
integer(kind=irg),INTENT(IN)        :: nns
complex(kind=dbl),INTENT(INOUT)     :: DynMat(nns,nns)
!f2py intent(in,out) ::  DynMat
integer(kind=irg),INTENT(IN)        :: nnw
character(5),INTENT(IN),OPTIONAL    :: BlochMode   ! 'Bloch' or 'Struc'

complex(kind=dbl)                   :: czero, ughp, uhph, weaksum, cv, Agh, Ahgp, Ahmgp, Ahg, weakdiagsum, pq0, Ahh, Agpgp, ccpi 
real(kind=dbl)                      :: weaksgsum, tpi, Pioxgp
real(kind=sgl)                      :: Upz
integer(kind=sgl)                   :: ir, ic, ll(5), istat, wc, QCindex
type(TDQCreflisttype),pointer       :: rlr, rlc, rlw
character(1)                        :: AorD

complex(kind=dbl)                   :: Ugh, qg
real(kind=dbl)                      :: Vmod, Vpmod, xig, xgp

czero = cmplx(0.0,0.0,dbl)      ! complex zero
tpi = 2.D0 * cPi
ccpi = cmplx(cPi,0.0D0,dbl)

nullify(rlr)
nullify(rlc)
nullify(rlw)

! note that the factor of i pi is added in at the end...
        DynMat  = czero
        pq0     = cmplx(0.D0,1.D0/QCcell%xizerop,dbl)
        
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
              weaksum =   czero
              rlw     =>  listrootw
              do
               if (.not.associated(rlw)) EXIT
               ll       = rlr%hkl - rlw%hkl
               QCindex  = QC_getindex(QCcell, ll)
               Ugh     = QCcell%LUT(QCindex)
               Agh     = QCcell%LUTqg(QCindex)

               ll       = rlw%hkl - rlc%hkl
               QCindex  = QC_getindex(QCcell, ll)
               Ugh     = QCcell%LUT(QCindex)
               Ahgp   = QCcell%LUTqg(QCindex)

! denominator Ahh - Ag'g'
               Ahh      =   cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
               Agpgp    =   cmplx(2.D0 * rlc%sg,0.D0,dbl) + pq0
               weaksum  =   weaksum +  Agh * Ahgp / (Ahh - Agpgp)
               rlw      =>  rlw%nextw
              end do
! and correct the dynamical matrix element to become a Bethe potential coefficient
              ll            = rlr%hkl - rlc%hkl
              QCindex     = QC_getindex(QCcell, ll)
              DynMat(ir,ic) = QCcell%LUTqg(QCindex)  -  weaksum

             else
              ll            = rlr%hkl - rlc%hkl
              QCindex     = QC_getindex(QCcell, ll)
              DynMat(ir,ic) = QCcell%LUTqg(QCindex)

            end if
          else  ! it is a diagonal entry, so we need the excitation error and the absorption length
! determine the total contribution of the weak beams
            if (nnw.ne.0) then
              weakdiagsum = 0.D0
              rlw => listrootw
              do
               if (.not.associated(rlw)) EXIT
                ll    = rlr%hkl - rlw%hkl
                QCindex = QC_getindex(QCcell, ll)
                Ugh = QCcell%LUT(QCindex)
                Agh = QCcell%LUTqg(QCindex)

                QCindex = QC_get5Dindex(QCcell, -ll)
                Ugh = QCcell%LUT(QCindex)
                Ahg = QCcell%LUTqg(QCindex)

! denominator Ahh - Agg
                Ahh         =   cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
                Agpgp       =   cmplx(2.D0 * rlr%sg,0.D0,dbl) + pq0
                weakdiagsum =   weakdiagsum +  Agh * Ahg  / (Ahh - Agpgp)
                rlw         =>  rlw%nextw
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


!if (present(BlochMode)) then
!  if (BlochMode.eq.'Bloch') then
!    cv = cmplx(1.D0/cPi/cell%mLambda,0.D0)
!    DynMat = DynMat * cv
!  end if
!end if

end subroutine QC_GetDynMat2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_GetDynMatMaster2DQC
!
!> @author Saransh, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, WITHOUT Bethe potentials for 2D Quasicrystals
!
!> @param cell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!
!> @date  02/22/18 SS 1.0 original
!> @date  06/25/18 SS 1.1 moved to MBmoduleQC module
!--------------------------------------------------------------------------
recursive subroutine QC_GetDynMatMaster2DQC(QCcell, listroot, DynMat, nref)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_GetDynMatMaster2DQC

use io

IMPLICIT NONE

type(TDQCStructureType),pointer         :: QCcell
type(TDQCreflisttype),pointer           :: listroot
integer(kind=irg),INTENT(IN)            :: nref
complex(kind=dbl),INTENT(INOUT)         :: DynMat(nref,nref)
!f2py intent(in,out) ::  DynMat

complex(kind=dbl)                       :: czero, ughp, uhph, weaksum, qg0
real(kind=dbl)                          :: weaksgsum
real(kind=sgl)                          :: Upz
integer(kind=sgl)                       :: ir, ic, ll(5), istat, wc, gindex
type(TDQCreflisttype),pointer           :: rlr, rlc, rlw
complex(kind=dbl)                       :: Ugh, qg
real(kind=dbl)                          :: Vmod, Vpmod, xig, xgp

czero   = cmplx(0.0,0.0,dbl)      ! complex zero

nullify(rlr)
nullify(rlc)

DynMat  = czero

ll     = (/0,0,0,0,0/)
gindex   = QC_getindex(QCcell, ll)
qg0   = QCcell%LUTqg(gindex)

rlr => listroot%next
ir = 1
    do
        if (.not.associated(rlr)) EXIT
        rlc => listroot%next
        ic = 1
        do
            if (.not.associated(rlc)) EXIT
            if (ic.ne.ir) then  ! not a diagonal entry
                ll              = rlr%hkl - rlc%hkl
                gindex       = QC_getindex(QCcell, ll)
                !Ugh             = QC_getUcg(QCcell, ll, qg, Vmod, Vpmod, xig, xgp)
                DynMat(ir,ic)   = QCcell%LUTqg(gindex)! qg
            else
                DynMat(ir,ic)   = cmplx(2.D0*rlc%sg,0.D0) + qg0
            end if
            rlc => rlc%next
            ic = ic + 1
        end do
        rlr => rlr%next
        ir = ir+1
    end do

DynMat = DynMat * cmplx(cPi,0.D0)

end subroutine QC_GetDynMatMaster2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_GetDynMat3DQC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, including Bethe potentials, for a quasi-crystal
!
!> details We compute the dynamical matrix as the structure matrix A, with 
!> the q_g elements along the off-diagonal; the reason for this is the fact
!> that this approach leads to a dynamical matrix that is shift invariant.
!> A conversion to the Bloch wave dynamical matrix can be obtained by setting 
!> the optional keyword BlochMode
!
!> @param QCcell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!> @param BlochMode [optional] Bloch or Struc
!
!> @date  04/22/14 MDG 1.0 new library version
!> @date  06/15/14 MDG 2.0 updated for removal of globals
!> @date  06/17/14 MDG 2.1 added listroot pointers etc to accommodate multiple threads
!> @date  06/18/14 MDG 2.2 corrected some pointer allocation errors in other routines; this one now works fine.
!> @date  09/08/15 MDG 3.0 rewrite to allow either dynamical matrix type (Bloch/structure matrix) to be generated
!> @date  09/14/15 SS  3.1 added exp(-pi/xgp) to the diagonal elements of the bloch dynamical matrix
!> @date  06/25/18 SS 1.1 moved to MBmoduleQC module
!--------------------------------------------------------------------------
recursive subroutine QC_GetDynMat3DQC(QCcell, listroot, listrootw, DynMat, nns, nnw, BlochMode)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_GetDynMat3DQC

use local
use typedefs
use io
use constants

IMPLICIT NONE

type(QCStructureType),pointer    :: QCcell
type(QCreflisttype),pointer      :: listroot
type(QCreflisttype),pointer      :: listrootw
integer(kind=irg),INTENT(IN)     :: nns
complex(kind=dbl),INTENT(INOUT)  :: DynMat(nns,nns)
!f2py intent(in,out) ::  DynMat
integer(kind=irg),INTENT(IN)     :: nnw
character(5),INTENT(IN),OPTIONAL :: BlochMode   ! 'Bloch' or 'Struc'

complex(kind=dbl)                :: czero, ughp, uhph, weaksum, cv, Agh, Ahgp, Ahmgp, Ahg, weakdiagsum, pq0, Ahh, Agpgp, ccpi 
real(kind=dbl)                   :: weaksgsum, tpi, Pioxgp
real(kind=sgl)                   :: Upz
integer(kind=sgl)                :: ir, ic, ll(6), istat, wc, QCindex
type(QCreflisttype),pointer      :: rlr, rlc, rlw
character(1)                     :: AorD

complex(kind=dbl)                              :: Ugh, qg
real(kind=dbl)                                 :: Vmod, Vpmod, xig, xgp

czero = cmplx(0.0,0.0,dbl)      ! complex zero
tpi = 2.D0 * cPi
ccpi = cmplx(cPi,0.0D0,dbl)

nullify(rlr)
nullify(rlc)
nullify(rlw)

! note that the factor of i pi is added in at the end...
        DynMat = czero
        pq0 = cmplx(0.D0,1.D0/QCcell%xizerop,dbl)

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
               !QCindex = QC_get6Dindex(QCcell, ll)
               QCindex = QC_getindex(QCcell, ll)
               Ugh = QCcell%LUT(QCindex)
               Agh = QCcell%LUTqg(QCindex)
               ll = rlw%hkl - rlc%hkl
               !QCindex = QC_get6Dindex(QCcell, ll)
               QCindex = QC_getindex(QCcell, ll)
               Ugh = QCcell%LUT(QCindex)
               Ahgp = QCcell%LUTqg(QCindex)
! denominator Ahh - Ag'g'
               Ahh = cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
               Agpgp = cmplx(2.D0 * rlc%sg,0.D0,dbl) + pq0
               weaksum = weaksum +  Agh * Ahgp / (Ahh - Agpgp)
               rlw => rlw%nextw
              end do
! and correct the dynamical matrix element to become a Bethe potential coefficient
              ll = rlr%hkl - rlc%hkl
              !QCindex = QC_get6Dindex(QCcell, ll)
              QCindex = QC_getindex(QCcell, ll)
              DynMat(ir,ic) = QCcell%LUTqg(QCindex)  -  weaksum
             else
              ll = rlr%hkl - rlc%hkl
              !QCindex = QC_get6Dindex(QCcell, ll)
              QCindex = QC_getindex(QCcell, ll)
              DynMat(ir,ic) = QCcell%LUTqg(QCindex)
            end if
          else  ! it is a diagonal entry, so we need the excitation error and the absorption length
! determine the total contribution of the weak beams
            if (nnw.ne.0) then
              weakdiagsum = 0.D0
              rlw => listrootw
              do
               if (.not.associated(rlw)) EXIT
                ll = rlr%hkl - rlw%hkl
                QCindex = QC_getindex(QCcell, ll)
                Ugh = QCcell%LUT(QCindex)
                Agh = QCcell%LUTqg(QCindex)

                QCindex = QC_getindex(QCcell, -ll)
                Ugh = QCcell%LUT(QCindex)
                Ahg = QCcell%LUTqg(QCindex)
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


!if (present(BlochMode)) then
!  if (BlochMode.eq.'Bloch') then
!    cv = cmplx(1.D0/cPi/cell%mLambda,0.D0)
!    DynMat = DynMat * cv
!  end if
!end if

end subroutine QC_GetDynMat3DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_GetDynMatMaster3DQC
!
!> @author Saransh, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, WITHOUT Bethe potentials for Quasicrystals
!
!> @param cell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!
!> @date  02/22/18 SS 1.0 original
!> @date  06/25/18 SS 1.1 moved to MBmoduleQC module
!--------------------------------------------------------------------------
recursive subroutine QC_GetDynMatMaster3DQC(QCcell, listroot, DynMat, nref)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_GetDynMatMaster3DQC

use local
use typedefs
use io
use crystal
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

type(QCStructureType),pointer           :: QCcell
type(QCreflisttype),pointer             :: listroot
integer(kind=irg),INTENT(IN)            :: nref
complex(kind=dbl),INTENT(INOUT)         :: DynMat(nref,nref)
!f2py intent(in,out) ::  DynMat

complex(kind=dbl)                       :: czero, ughp, uhph, weaksum, qg0
real(kind=dbl)                          :: weaksgsum
real(kind=sgl)                          :: Upz
integer(kind=sgl)                       :: ir, ic, ll(6), istat, wc, gindex
type(QCreflisttype),pointer             :: rlr, rlc, rlw

czero   = cmplx(0.0,0.0,dbl)      ! complex zero

nullify(rlr)
nullify(rlc)

DynMat  = czero

gindex  = QC_getindex(QCcell, (/0,0,0,0,0,0/))
qg0     = QCcell%LUTqg(gindex)

rlr => listroot%next
ir = 1
    do
        if (.not.associated(rlr)) EXIT
        rlc => listroot%next
        ic = 1
        do
            if (.not.associated(rlc)) EXIT
            if (ic.ne.ir) then  ! not a diagonal entry
                ll            = rlr%hkl - rlc%hkl
                gindex        = QC_getindex(QCcell, ll)
                DynMat(ir,ic) = QCcell%LUTqg(gindex)
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

end subroutine QC_GetDynMatMaster3DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: TDQC_CalcSgh
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute structure factor-like array for EBSD, ECCI and ECP simulations (2-D QC version)
!
!> @param QCcell cell pointer
!> @param nn dimension of array
!> @param Sgh output array
!> @param nat normalization array
!
!> @date 05/01/18  SS 1.0 original (used to be in-line in ECP and ECCI programs)
!--------------------------------------------------------------------------
recursive subroutine TDQC_CalcSgh(QCcell,reflist,nn,numset,Sgh,nat)
!DEC$ ATTRIBUTES DLLEXPORT :: TDQC_CalcSgh

use local
use typedefs
use constants

IMPLICIT NONE

type(TDQCStructureType),pointer         :: QCcell
type(TDQCreflisttype),pointer           :: reflist
integer(kind=irg),INTENT(IN)            :: nn
integer(kind=irg),INTENT(IN)            :: numset
complex(kind=dbl),INTENT(INOUT)         :: Sgh(nn,nn,numset)
!f2py intent(in,out) ::  Sgh
integer(kind=irg),INTENT(INOUT)         :: nat(maxpasym)
!f2py intent(in,out) ::  nat

integer(kind=irg)                       :: ip, ir, ic, kkk(5), ikk, n
real(kind=sgl)                          :: Znsq, DBWF, kkl1, kkl2, kkl3
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: ctmp(QCcell%SG%SYM_MATnum,5), arg, tpi, cvec(5)
type(TDQCreflisttype),pointer           :: rltmpa, rltmpb

tpi = 2.D0 * cPi
Sgh = cmplx(0.D0,0.D0)
  
do ip = 1,QCcell % ATOM_ntype
  nat(ip) = QCcell%numat(ip)
  !nat(1) = QCcell%atno
! get Zn-squared for this special position, and include the site occupation parameter as well
  Znsq = float(QCcell%ATOM_type(ip))**2 * QCcell%ATOM_pos(ip,6)
  
! loop over all contributing reflections
! ir is the row index
  rltmpa => reflist%next    ! point to the front of the list
  do ir=1,nn
! ic is the column index
    rltmpb => reflist%next    ! point to the front of the list
    do ic=1,nn
      kkk = rltmpb%hkl - rltmpa%hkl
      call QC_TransSpace(QCcell,dble(kkk),cvec,'r','c')
! anisotropic debye waller facvtors
! Bpar_11 is in the quasiperiodic place:  ATOM_pos(ip,7)
! Bpar_33 is in the axial direction:      ATOM_pos(ip,8)
! Bperp is in perpendicular space:        ATOM_pos(ip,9)
      kkl1 = 0.25 * (cvec(1)**2 + cvec(2)**2) ! s^2/2 in quaisperiodic plane
      kkl2 = 0.25 * cvec(5)**2                ! s^2/2 in periodic direction
      kkl3 = 0.25 * (cvec(3)**2 + cvec(4)**2) ! s^2/2 in perpendicular space
! Debye-Waller exponential times Z^2
      DBWF = Znsq * exp(-QCcell%ATOM_pos(ip,7)*kkl1) * exp(-QCcell%ATOM_pos(ip,8)*kkl2) * exp(-QCcell%ATOM_pos(ip,9)*kkl3)
      
      do ikk=1,nat(ip)
! get the argument of the complex exponential
        arg = tpi*sum(dble(kkk(1:5))*QCcell%apos(ip,ikk,1:5))
        carg = cmplx(dcos(arg),dsin(arg))
! multiply with the prefactor and add
        Sgh(ir,ic,ip) = Sgh(ir,ic,ip) + carg * cmplx(DBWF,0.D0)
      end do
    
      rltmpb => rltmpb%nexts  ! move to next column-entry
    end do
   rltmpa => rltmpa%nexts  ! move to next row-entry
 end do 
end do 

end subroutine TDQC_CalcSgh

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_CalcSgh
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute structure factor-like array for EBSD, ECCI and ECP simulations (QC version)
!
!> @param QCcell cell pointer
!> @param nn dimension of array
!> @param Sgh output array
!> @param nat normalization array
!
!> @date 03/05/14  MDG 1.0 original (used to be in-line in ECP and ECCI programs)
!> @date 03/11/14  MDG 1.1 converted to diagonal Sgh array only
!> @date 06/19/14  MDG 2.0 no globals, taken out of EMECCI.f90
!> @date 09/07/15  MDG 2.1 added zeroing of Sgh array
!--------------------------------------------------------------------------
recursive subroutine QC_CalcSgh(QCcell,reflist,nn,numset,Sgh,nat)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_CalcSgh

use local
use typedefs
use constants
use qcrystal

IMPLICIT NONE

type(QCStructureType),pointer           :: QCcell
type(QCreflisttype),pointer             :: reflist
integer(kind=irg),INTENT(IN)            :: nn
integer(kind=irg),INTENT(IN)            :: numset
complex(kind=dbl),INTENT(INOUT)         :: Sgh(nn,nn,numset)
!f2py intent(in,out) ::  Sgh
integer(kind=irg),INTENT(INOUT)         :: nat(maxpasym)
!f2py intent(in,out) ::  nat

integer(kind=irg)                       :: ip, ir, ic, kkk(6), ikk, n
real(kind=sgl)                          :: Znsq, DBWF, kkl1, kkl2
complex(kind=dbl)                       :: carg
real(kind=dbl)                          :: ctmp(192,3),arg, tpi
type(QCreflisttype),pointer             :: rltmpa, rltmpb

tpi = 2.D0 * cPi
Sgh = cmplx(0.D0,0.D0)
  
do ip = 1,QCcell % ATOM_ntype
  nat(ip) = QCcell%numat(ip)
  !nat(1) = QCcell%atno
! get Zn-squared for this special position, and include the site occupation parameter as well
  Znsq = float(QCcell%ATOM_type(ip))**2 * QCcell%ATOM_pos(ip,7)
  
! loop over all contributing reflections
! ir is the row index
  rltmpa => reflist%next    ! point to the front of the list
  do ir=1,nn
! ic is the column index
    rltmpb => reflist%next    ! point to the front of the list
    do ic=1,nn
      kkk = rltmpb%hkl - rltmpa%hkl
      !call QC_TransSpace(QCcell,dble(kkk),cvec,'r','c')
! anisotropic debye waller facvtors
! Bpar  is in the physical space:          ATOM_pos(ip,8)
! Bperp is in perpendicular space:        ATOM_pos(ip,9)
    kkl1 = QC_getvectorLength(QCcell, kkk, 'P', 'r') 
      kkl1 = 0.25 * kkl1**2       ! (s/2)^2 in physical dimension quasiperiodic

      kkl2 = QC_getvectorLength(QCcell, kkk, 'O', 'r') 
      kkl2 = 0.25 * kkl2**2        ! (s/2)^2 in quasiperiodic dimension

! Debye-Waller exponential times Z^2
      DBWF = Znsq * exp(-QCcell%ATOM_pos(ip,8)*kkl1) * exp(-QCcell%ATOM_pos(ip,9)*kkl2)
      
      do ikk=1,nat(ip)
! get the argument of the complex exponential
        arg = tpi*sum(dble(kkk(1:6))*QCcell%apos(ip,ikk,1:6))
        carg = cmplx(dcos(arg),dsin(arg))
! multiply with the prefactor and add
        Sgh(ir,ic,ip) = Sgh(ir,ic,ip) + carg * cmplx(DBWF,0.D0)
      end do
    
      rltmpb => rltmpb%nexts  ! move to next column-entry
    end do
   rltmpa => rltmpa%nexts  ! move to next row-entry
 end do 
end do 
  
end subroutine QC_CalcSgh

end module MBmoduleQC
