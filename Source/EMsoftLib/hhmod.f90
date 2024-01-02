! ###################################################################
! Copyright (c) 2014-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:hhmod.f90 
!--------------------------------------------------------------------------
!
! MODULE: hhmod
!
!> @author Original hh.f77 code history, see
! Author = {Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.},
! Publisher = {North Holland Publishing Company},
! Series = {Defects in {C}rystalline {S}olids, edited by Amelinckx, S. and Gevers, R. and Nihoul, J.},
! Title = {Computed Electron Micrographs and Defect Identification},
! Volume = {7},
! Year = 1973}
!
! Translated to Fortran-90 by MDG.
! Integrated with other source code from EMsoft package.
! Added TIFF output + support for tilt series.
! Added namelist handling to replace the older input format
!
! This module contains support routines that are used by the EMhh4.f90 program
! 
!> @date ??/??/01 MDG 1.0 original conversion from f77 version
!> @date 08/13/19 MDG 2.0 new version, integrated with EMsoft 4.3
!--------------------------------------------------------------------------
module hhmod

use local
use constants
use typedefs 

IMPLICIT NONE

! All common blocks of the original hh.f program were converted
! into user-defined types and declared as variables with the 
! original common block name. See typedefs.f90 file.

! all of these need to disappear... no global variables!

! type(MAPN_block)                 :: MAPN
! type(MA_block)                   :: MA
! type(MKAP_block)                 :: MKAP
! type(MRD_block)                  :: MRD
! type(MT_block)                   :: MT
! type(MKT_block)                  :: MKT
! type(SCALE30_block)              :: SCALE30
! type(MP_block)                   :: MP
! type(MAP_block)                  :: MAP
! type(hhs_block)                  :: hhs

private :: NEWTON

public  :: ANCALC, PANCALC, RKM, DERIV

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: NEWTON
!
!> @author Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.
!
!> @brief  Newton routine to find root of 8th order polynomial
! 
!> @param MAPN
!
!> @date    08/13/19 MDG 1.0 adapted from .f77 original
!--------------------------------------------------------------------------
recursive subroutine NEWTON(MAPN) 
!DEC$ ATTRIBUTES DLLEXPORT :: NEWTON

IMPLICIT NONE

type(MAPN_block),INTENT(INOUT)    :: MAPN
!f2py intent(in,out) ::  MAPN

integer(kind=irg)                 :: KOUNT, KONVRG, J, M
real(kind=sgl)                    :: XR, XI, YR, YI, TR, TI, F
  
! FIND A ROOT OF THE POLYNOMIAL OF THE EIGHTH ORDER
 KONVRG=0
loop: do KOUNT=1,70 
  XR=0.0
  XI=0.0
  YR=0.0
  YI=0.0
  do J=1,MAPN%NEW
   TR=MAPN%ZR*YR-MAPN%ZI*YI+XR 
   YI=MAPN%ZR*YI+MAPN%ZI*YR+XI 
   YR=TR 
   M=MAPN%NEW+1-J 
   TR=MAPN%ZR*XR-MAPN%ZI*XI+MAPN%QR(M)
   TI=MAPN%ZR*XI+MAPN%ZI*XR+MAPN%QI(M)
   if (KONVRG.ne.0) then 
    MAPN%QR(M)=XR
    MAPN%QI(M)=XI
   end if
   XR=TR 
   XI=TI 
  end do
  if (KONVRG.eq.0) then 
   F=1.0/(YR**2+YI**2) 
   TR=F*(XR*YR+XI*YI)
   TI=F*(XI*YR-XR*YI)
   MAPN%ZR=MAPN%ZR-TR
   MAPN%ZI=MAPN%ZI-TI
   if (((TR**2+TI**2)/(MAPN%ZR**2+MAPN%ZI**2)-1.0E-12).le.0.0) then
    KONVRG=1
   end if
  else
   EXIT loop 
  end if
 end do loop
!
 if (KOUNT.eq.70) then 
   MAPN%KRASH=-70 
 else
  if ((ABS(MAPN%ZR)-1.0E5*ABS(MAPN%ZI)).le.0.0) then 
   MAPN%NEW = MAPN%NEW-1
  else
   MAPN%KRASH=10-MAPN%NEW
  end if
 end if
end subroutine NEWTON

!--------------------------------------------------------------------------
!
! SUBROUTINE: ANCALC
!
!> @author Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.
!
!> @brief  Displacement calculations for anisotropic elasticity
! 
!> @param MAP
!> @param MKAP
!> @param MAPN
!> @param MA
!> @param SCALE30
!
!> @date    09/08/16 MDG 1.0 original
!> @date    01/30/17 MDG 1.1 added optional name list file variable
!--------------------------------------------------------------------------
recursive subroutine ANCALC(MAP, MKAP, MAPN, MA, SCALE30) 
!DEC$ ATTRIBUTES DLLEXPORT :: ANCALC

IMPLICIT NONE

type(MAP_block),INTENT(INOUT)     :: MAP
!f2py intent(in,out) ::  MAP
type(MKAP_block),INTENT(INOUT)    :: MKAP
!f2py intent(in,out) ::  MKAP
type(MAPN_block),INTENT(INOUT)    :: MAPN
!f2py intent(in,out) ::  MAPN
type(MA_block),INTENT(INOUT)      :: MA
!f2py intent(in,out) ::  MA
type(SCALE30_block),INTENT(INOUT) :: SCALE30
!f2py intent(in,out) ::  SCALE30

integer(kind=irg)              :: I, J, K, L, M, N, LT, KQ, KR, KS, KT, NJ, I1, I2, J1, J2, K1, K2, LP, LQ, KP, NL, ML
integer(kind=irg),parameter    :: L1(6)=(/1,2,3,2,3,1/), L2(6)=(/1,2,3,3,1,2/), &
                                  L3(3,3)= reshape( (/1,6,5,6,2,4,5,4,3/), (/3,3/) ), & 
                                  N1(4)=(/2,4,2,1/), N2(4)=(/3,1,4,2/), N3(4)=(/4,3,1,3/), &
                                  NN(3)=(/6,2,4/), MM(3)=(/1,6,5/), NP(3) = (/2,3,1/), NQ(3) = (/3,1,2/)

real(kind=sgl)                 :: C(6,6), EE(3,6), EN(3,3), QM(7,4), G(9), E(9), F(9), HI(12), &
                                  DR(4,4), DI(4,4), B(4,4), ELR(4,4), ELI(4,4), X, Y, Z, PRK, PIK, SQR, SQI, XR, XI, YR, YI, &
                                  DELR, DELI, AUMR, AUMI, DEL
  
! CALCULATE DISPLACEMENTS 
 do M=1,6
  I=L1(M)
  J=L2(M)
  do N=1,M
   K=L1(N)
   L=L2(N)
   X=0.0
   DO LP=1,3 
    Y=0.0
    DO LQ=1,3
     LT=L3(LP,LQ)
     Y=Y+MAP%DC(J,LQ)*(MAP%DC(K,1)*(MAP%DC(L,1)*MKAP%D1(LT,1)+MAP%DC(L,2)*MKAP%D1(LT,6)+MAP%DC(L,3)*MKAP%D1(LT,5)) &
        +MAP%DC(K,2)*(MAP%DC(L,1)*MKAP%D1(LT,6)+MAP%DC(L,2)*MKAP%D1(LT,2)+MAP%DC(L,3)*MKAP%D1(LT,4))  &
        +MAP%DC(K,3)*(MAP%DC(L,1)*MKAP%D1(LT,5)+MAP%DC(L,2)*MKAP%D1(LT,4)+MAP%DC(L,3)*MKAP%D1(LT,3)))
    end do
    X=X+MAP%DC(I,LP)*Y
   end do
   C(M,N)=X
   C(N,M)=X
  end do
 end do
 if (SCALE30%LTEST.eq.1) then
  write(dataunit,"('  C-DC ',6F10.6)") ((C(I,J),J=1,6),I=1,6)
 end if 
 G(1)=C(5,5)
 G(2)=2.0*C(4,5)
 G(3)=C(4,4)
 G(4)=C(6,6)
 G(5)=2.0*C(2,6)
 G(6)=C(2,2)
 G(7)=C(1,1)
 G(8)=2.0*C(1,6)
 G(9)=C(6,6)
 E(1)=C(5,6)
 E(2)=C(2,5)+C(4,6)
 E(3)=C(2,4)
 E(4)=C(1,5)
 E(5)=C(5,6)+C(1,4)
 E(6)=C(4,6)
 E(7)=C(1,6)
 E(8)=C(6,6)+C(1,2)
 E(9)=C(2,6)
 MAPN%QR = 0.0
 MAPN%QI = 0.0
 do KQ=1,3
  do KR=1,3
   do KS=1,3
    KT=KQ+KR+KS-2
    MAPN%QR(KT)=MAPN%QR(KT)+G(KQ)*G(KR+3)*G(KS+6)+2.0*E(KQ)*E(KR+3)*E(KS+6) &
                -E(KQ)*E(KR)*G(KS+6)-E(KQ+3)*E(KR+3)*G(KS+3)-E(KQ+6)*E(KR+6)*G(KS)
   end do
  end do
 end do
 if (SCALE30%LTEST.eq.1) then
  write(dataunit,"('  QR- HH4 ',F30.15)") (MAPN%QR(KP),KP=1,7)
 end if 
 MAPN%QR=MAPN%QR/MAPN%QR(7)
 MAPN%KRASH=0
 MAPN%NEW=7
 MAPN%ZR=0.1
 MAPN%ZI=1.0
 CALL NEWTON(MAPN)
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MA%PR(1)=MAPN%ZR
 MA%PI(1)=ABS(MAPN%ZI)
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON(MAPN)
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MAPN%ZR=0.5
 MAPN%ZI=0.9
 CALL NEWTON(MAPN)
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MA%PR(2)=MAPN%ZR
 MA%PI(2)=ABS(MAPN%ZI)
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON(MAPN)
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MAPN%ZR=-MAPN%ZR
 CALL NEWTON(MAPN)
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MA%PR(3)=MAPN%ZR
 MA%PI(3)=ABS(MAPN%ZI) 
 MAPN%ZR=-C(4,5)/C(4,4) 
 MAPN%ZI=SQRT(ABS(C(4,4)*C(5,5)-C(4,5)**2))/C(4,4)
 do N=1,2 
   if (((MAPN%ZR-MA%PR(N))**2+(MAPN%ZI-MA%PI(N))**2-(MAPN%ZR-MA%PR(N+1))**2-(MAPN%ZI-MA%PI(N+1))**2).lt.0.0) then
    Z=MA%PR(N) 
    MA%PR(N)=MA%PR(N+1) 
    MA%PR(N+1)=Z 
    Z=MA%PI(N) 
    MA%PI(N)=MA%PI(N+1) 
    MA%PI(N+1)=Z 
   end if
 end do
!
 if (SCALE30%LTEST.eq.1) then
  do I=1,3
   WRITE(dataunit,"(' Roots HH4 ',2F20.15)") MA%PR(I),MA%PI(I)
  end do 
 end if 
 DO K=1,3 
  I=NP(K)
  L=NQ(K)
  PRK=MA%PR(K)
  PIK=MA%PI(K)
  SQR=PRK**2-PIK**2
  SQI=2.0*PRK*PIK
  DR(1,1)=C(1,1)+PRK*2.0*C(1,6)+SQR*C(6,6)
  DR(2,2)=C(6,6)+PRK*2.0*C(2,6)+SQR*C(2,2)
  DR(3,3)=C(5,5)+PRK*2.0*C(4,5)+SQR*C(4,4)
  DR(1,2)=C(1,6)+PRK*(C(1,2)+C(6,6))+SQR*C(2,6)
  DR(2,1)=DR(1,2)
  DR(1,3)=C(1,5)+PRK*(C(1,4)+C(5,6))+SQR*C(4,6)
  DR(3,1)=DR(1,3)
  DR(2,3)=C(5,6)+PRK*(C(4,6)+C(2,5))+SQR*C(2,4)
  DR(3,2)=DR(2,3)
  DI(1,1)=PIK*2.0*C(1,6)+SQI*C(6,6)
  DI(2,2)=PIK*2.0*C(2,6)+SQI*C(2,2)
  DI(3,3)=PIK*2.0*C(4,5)+SQI*C(4,4)
  DI(1,2)=PIK*(C(1,2)+C(6,6))+SQI*C(2,6)
  DI(2,1)=DI(1,2)
  DI(1,3)=PIK*(C(1,4)+C(5,6))+SQI*C(4,6)
  DI(3,1)=DI(1,3)
  DI(2,3)=PIK*(C(4,6)+C(2,5))+SQI*C(2,4)
  DI(3,2)=DI(2,3)
  do J=1,3
   M=NP(J)
   N=NQ(J)
   MA%AR(J,K)=DR(I,M)*DR(L,N)-DI(I,M)*DI(L,N)-DR(I,N)*DR(L,M)+DI(I,N)*DI(L,M)
   MA%AI(J,K)=DR(I,M)*DI(L,N)+DI(I,M)*DR(L,N)-DR(I,N)*DI(L,M)-DI(I,N)*DR(L,M)
  end do
 end do
 if (SCALE30%LTEST.eq.1) then
  do J=1,3
   do K=1,3
    WRITE(dataunit,"('   Vektoren as HH4 ',2F20.12)") MA%AR(J,K),MA%AI(J,K)
   end do
  end do 
 end if 
 do J=1,3
  NJ=NN(J)
  do K=1,3
   XR=0.0
   XI=0.0
   do L=1,3
    NL=NN(L)
    ML=MM(L)
    YR=C(NJ,ML)+C(NJ,NL)*MA%PR(K)
    YI=C(NJ,NL)*MA%PI(K) 
    XR=XR+YR*MA%AR(L,K)-YI*MA%AI(L,K)
    XI=XI+YI*MA%AR(L,K)+YR*MA%AI(L,K)
   end do
   ELR(J,K)=XR
   ELI(J,K)=XI
  end do
 end do
 do J=1,3
  J1=NP(J)
  J2=NQ(J)
  DO K=1,3
   K1=NP(K)
   K2=NQ(K)
   MA%EMR(K,J)=ELR(J1,K1)*ELR(J2,K2)-ELI(J1,K1)*ELI(J2,K2)-ELR(J1,K2)*ELR(J2,K1)+ELI(J1,K2)*ELI(J2,K1)
   MA%EMI(K,J)=ELR(J1,K1)*ELI(J2,K2)+ELI(J1,K1)*ELR(J2,K2)-ELR(J1,K2)*ELI(J2,K1)-ELI(J1,K2)*ELR(J2,K1)
  end do
 end do
 DELR=0.0
 DELI=0.0
 do J=1,3
  DELR=DELR+ELR(3,J)*MA%EMR(J,3)-ELI(3,J)*MA%EMI(J,3)
  DELI=DELI+ELR(3,J)*MA%EMI(J,3)+ELI(3,J)*MA%EMR(J,3)
 end do
 AUMR=DELR/(DELR**2+DELI**2)
 AUMI=-DELI/(DELR**2+DELI**2)
 do J=1,3
  do K=1,3
   X=MA%EMR(J,K)*AUMR-MA%EMI(J,K)*AUMI
   MA%EMI(J,K)=MA%EMR(J,K)*AUMI+MA%EMI(J,K)*AUMR
   MA%EMR(J,K)=X
  end do
 end do
 do I=1,3
  do J=1,3
   B(I,J)=-sum(MA%AR(I,1:3)*MA%EMI(1:3,J))-sum(MA%AI(I,1:3)*MA%EMR(1:3,J))
  end do
 end do
 do I=1,3
  I1=NP(I)
  I2=NQ(I)
  do J=1,3
   J1=NP(J)
   J2=NQ(J)
   MA%H(I,J)=B(I1,J1)*B(I2,J2)-B(I1,J2)*B(I2,J1)
  end do
 end do
 DEL=B(3,1)*MA%H(3,1)+B(3,2)*MA%H(3,2)+B(3,3)*MA%H(3,3)
 MA%H = MA%H/DEL
 MAPN%QR(8:9)=0.0 
 MAPN%QI(8:9)=0.0 
 MA%PR(4)=0.0 
 MA%PI(4)=0.0 
 MA%AR(1:4,4)=0.0 
 MA%AR(4,1:4)=0.0 
 MA%AI(1:4,4)=0.0 
 MA%AI(4,1:4)=0.0 
 MA%EMR(1:4,4)=0.0
 MA%EMR(4,1:4)=0.0
 MA%EMI(1:4,4)=0.0
 MA%EMI(4,1:4)=0.0
 MA%H(1:4,4)=0.0
 MA%H(4,1:4)=0.0
end subroutine ANCALC

!--------------------------------------------------------------------------
!
! SUBROUTINE: PANCALC
!
!> @author Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.
!
!> @brief  Displacement calculations for anisotropic elasticity with piezoelectric effect
! 
!> @param MAP
!> @param MKAP
!> @param MAPN
!> @param MA
!> @param SCALE30
!
!> @date    08/13/19 MDG 1.0 adapted from .f77 original
!--------------------------------------------------------------------------
recursive subroutine PANCALC(MAP, MKAP, MAPN, MA, MP, SCALE30) 
!DEC$ ATTRIBUTES DLLEXPORT :: PANCALC

IMPLICIT NONE

type(MAP_block),INTENT(INOUT)     :: MAP
!f2py intent(in,out) ::  MAP
type(MKAP_block),INTENT(INOUT)    :: MKAP
!f2py intent(in,out) ::  MKAP
type(MAPN_block),INTENT(INOUT)    :: MAPN
!f2py intent(in,out) ::  MAPN
type(MA_block),INTENT(INOUT)      :: MA
!f2py intent(in,out) ::  MA
type(MP_block),INTENT(INOUT)      :: MP
!f2py intent(in,out) ::  MP
type(SCALE30_block),INTENT(INOUT) :: SCALE30
!f2py intent(in,out) ::  SCALE30


!**************************************************** 
!*     SUBROUTINE PANCALC has been extended for a   * 
!*     piezoelectric crystal with a core-charge "Q" * 
!**************************************************** 
complex(kind=sgl)           :: PC, AS, EL
complex(kind=sgl)           :: A(4,4), AY(4), AXA(4,4), AXL(4,4), LXL(4,4), MXX(4), MXXX(4,4)
real(kind=sgl)              :: C(6,6), EE(3,6), EN(3,3), QM(7,4), G(9), E(9), F(9), HI(12),  &
                               PR(4), PI(4), X, Y, Z, PR1, PI1, PR2, PI2, PR3, PI3, PR4, PI4, AZ, &
                               BY, CY, D, AZZ, BZ, CZ, DZ
integer(kind=irg)           :: I, J, K, L, M, N, LP, LQ, LT, LV, KP, KQ, KR, KS, KT, &
                               MX, MY, MZ, NX, NY, NZ, NJ, NL, ML, K1, K2, K3, J1, I1, I2
integer(kind=irg),parameter :: L1(6)=(/1,2,3,2,3,1/), L2(6)=(/1,2,3,3,1,2/), &
                               L3(3,3)= reshape( (/1,6,5,6,2,4,5,4,3/), (/3,3/)), & 
                               N1(4)=(/2,1,1,1/), N2(4)=(/3,3,2,2/), N3(4)=(/4,4,4,3/), &
                               NN(3)=(/6,2,4/), MM(3)=(/1,6,5/)
logical                     :: goto1000

 if (SCALE30%LTEST.eq.1) then
  write(6,"('  D1--',6F8.4)") ((MKAP%D1(I,J),J=1,6),I=1,6) 
  write(6,"('  EP--',6F8.4)") ((MKAP%EP(K,L),L=1,6),K=1,3) 
  write(6,"('  EA--',3F8.4)") ((MKAP%EA(M,N),N=1,3),M=1,3) 
 end if 
!*******************************************************************
!*      Elasticity tensor in DC-reference frame                    *
!*******************************************************************
 do M=1,6 
  I=L1(M) 
  J=L2(M) 
  do N=1,M 
   K=L1(N) 
   L=L2(N) 
   X=0.0 
   do LP=1,3
    Y=0.0 
    do LQ=1,3
     LT=L3(LP,LQ)
     Y=Y+MAP%DC(J,LQ)*(MAP%DC(K,1)*(MAP%DC(L,1)*MKAP%D1(LT,1)+MAP%DC(L,2)*MKAP%D1(LT,6)+MAP%DC(L,3)*MKAP%D1(LT,5)) &
        +MAP%DC(K,2)*(MAP%DC(L,1)*MKAP%D1(LT,6)+MAP%DC(L,2)*MKAP%D1(LT,2)+MAP%DC(L,3)*MKAP%D1(LT,4)) &
        +MAP%DC(K,3)*(MAP%DC(L,1)*MKAP%D1(LT,5)+MAP%DC(L,2)*MKAP%D1(LT,4)+MAP%DC(L,3)*MKAP%D1(LT,3)))
    end do
    X=X+MAP%DC(I,LP)*Y
   end do
   C(M,N)=X
   C(N,M)=X
  end do
 end do
 where (abs(C).lt.1.0e-8) C=0.0
 if (SCALE30%LTEST.eq.1) then 
  write(6,"(' C-DC',6F8.4)") ((C(M,N),N=1,6),M=1,6)
 end if 
!*******************************************************************
!*       Piezo-tensor --E-- in DC-system                           *
!*******************************************************************
 do L=1,3 
  do M=1,6 
   I=L1(M) 
   J=L2(M) 
   X=0.0 
   do LV=1,3
    X=X+MAP%DC(L,LV)*(MAP%DC(I,1)*(MAP%DC(J,1)*MKAP%EP(LV,1)+MAP%DC(J,2)*MKAP%EP(LV,6)+MAP%DC(J,3)*MKAP%EP(LV,5)) &
       +MAP%DC(I,2)*(MAP%DC(J,1)*MKAP%EP(LV,6)+MAP%DC(J,2)*MKAP%EP(LV,2)+MAP%DC(J,3)*MKAP%EP(LV,4)) &
       +MAP%DC(I,3)*(MAP%DC(J,1)*MKAP%EP(LV,5)+MAP%DC(J,2)*MKAP%EP(LV,4)+MAP%DC(J,3)*MKAP%EP(LV,3)))
   end do
   EE(L,M)=X 
  end do
 end do
 where (ABS(EE).lt.1E-8) EE=0.0
 if (SCALE30%LTEST.eq.1) then
  write(6,"(' E-DC',6F8.4)")  ((EE(M,N),N=1,6),M=1,3)
 end if 
!*******************************************************************
!*        Dielectric tensor  in DC-system                          *
!*******************************************************************
 do I=1,3 
  do J=1,3 
   EN(I,J)=0.0
   do K=1,3 
    EN(I,J)=EN(I,J)+MAP%DC(I,K)*(MAP%DC(J,1)*MKAP%EA(K,1)+MAP%DC(J,2)*MKAP%EA(K,2)+MAP%DC(J,3)*MKAP%EA(K,3)) 
   end do
   EN(J,I)=EN(I,J) 
  end do
 end do
 where (ABS(EN).lt.1E-8) EN=0.0
 if (SCALE30%LTEST.eq.1) then
  write(6,"(' DI-DC',3F8.4)") ((EN(M,N),N=1,3),M=1,3) 
 end if 
!*******************************************************************
!        Computation of eighth-order polynomial.  The 
!        4 by 4 matrix is decomposed into four sixth-order
!        polynomials.  (Original German text below)
!*******************************************************************
!*       BERECHNUNG DES POLYNOMS ACHTEN GRADES. DIE 4 X 4          *
!*       MATRIX WIRD ENTWICKELT, D.H. ES WERDEN ZUERST VIER        *
!*       POLYNOME SECHSTEN GRADES BERECHNET, DANN JEWEILS          *
!*       MIT DEM ENTSPRECHENDEN ELEMENT DER VIERTEN ZEILE          *
!*       MULTIPLIZIERT. ADDITION DER VIER POLYNOME ACHTEN          *
!*       GRADES ERGEBEN DANN DAS GESUCHTE POLYNOM.                 *
!*******************************************************************
  MAPN%QR=0.0
  MAPN%QI=0.0
!*******************************************************************
!    BERUECKSICHTIGUNG DER ENTKOPPELTEN  4 X 4   MATRIX            *
! 
!                         X  X  0  0                               *
!                         X  X  0  0                               *
!                         0  0  X  X                               *
!                         0  0  X  X                                * 
!*******************************************************************
 X=C(1,4)+C(1,5)+C(2,4)+C(4,6)+C(5,6)+EE(1,1)+EE(1,2)+EE(1,6)+EE(2,1)+EE(2,2)+EE(2,6)
 goto1000 = .FALSE.
 if (X.eq.0.0) then
  MAPN%QR(1)=C(1,1)*C(6,6)-C(1,6)**2
  MAPN%QR(2)=2*(C(1,1)*C(2,6)-C(1,6)*C(1,2))
  MAPN%QR(3)=C(1,1)*C(2,2)+2*(C(1,6)*C(2,6)-C(1,2)*C(6,6))-C(1,2)**2
  MAPN%QR(4)=2*(C(1,6)*C(2,2)-C(1,2)*C(2,6))
  MAPN%QR(5)=C(2,2)*C(6,6)-C(2,6)**2
  do KP=1,5
   MAPN%QR(KP)=MAPN%QR(KP)/MAPN%QR(5)
   if (SCALE30%LTEST.eq.1) then 
    write(6,"('  1 QR=',F15.5)") MAPN%QR(KP) 
   end if
  end do
  MAPN%KRASH=0
  MAPN%NEW=5
  MAPN%ZR=0.1 
  MAPN%ZI=1.0 
  CALL NEWTON(MAPN)
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  PR(1)=MAPN%ZR 
  PI(1)=ABS(MAPN%ZI)
  MAPN%ZI=-MAPN%ZI 
  CALL NEWTON(MAPN)
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP  'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  MAPN%ZR=0.5 
  MAPN%ZI=0.9 
  CALL NEWTON(MAPN)
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  PR(2)=MAPN%ZR 
  PI(2)=ABS(MAPN%ZI)
  IF (ABS(PR(1)).LT.1E-5)  PR(1)=0.0 
  IF (ABS(1.0-PI(1)).LT.1E-5)  PI(1)=1.0 
  IF (ABS(PR(2)).LT.1E-5)  PR(2)=0.0 
  IF (ABS(1.0-PI(2)).LT.1E-5)  PI(2)=1.0 
  MP%PC(1)=CMPLX(PR(1),PI(1)) 
  MP%PC(2)=CMPLX(PR(2),PI(2)) 
  MP%AS(1,1)=C(6,6)+2*MP%PC(1)*C(2,6)+C(2,2)*MP%PC(1)**2
  MP%AS(2,1)=-C(1,6)-MP%PC(1)*(C(1,2)+C(6,6))-C(2,6)*MP%PC(1)**2
  MP%AS(3,1)=CMPLX(0.0,0.0) 
  MP%AS(4,1)=CMPLX(0.0,0.0) 
  MP%AS(1,2)=-C(1,6)-MP%PC(2)*(C(1,2)+C(6,6))-C(2,6)*MP%PC(2)**2
  MP%AS(2,2)=C(1,1)+2*MP%PC(2)*C(1,6)+C(6,6)*MP%PC(2)**2
  MP%AS(3,2)=CMPLX(0.0,0.0) 
  MP%AS(4,2)=CMPLX(0.0,0.0) 
  do KT=1,5
   MAPN%QR(KT)=0.0 
   MAPN%QI(KT)=0.0 
  end do
  MAPN%QR(1)=C(5,5)*EN(1,1)+EE(1,5)**2
  MAPN%QR(2)=2*(C(5,5)*EN(1,2)+C(4,5)*EN(1,1)+EE(1,5)*EE(1,4)+EE(1,5)*EE(2,5))
  MAPN%QR(3)=C(5,5)*EN(2,2)+4*C(4,5)*EN(1,2)+C(4,4)*EN(1,1)+EE(1,4)**2+2*EE(1,4)*EE(2,5)+EE(2,5)**2+2*EE(1,5)*EE(2,4)
  MAPN%QR(4)=2*(C(4,5)*EN(2,2)+C(4,4)*EN(1,2)+EE(2,4)*EE(1,4)+EE(2,4)*EE(2,5)) 
  MAPN%QR(5)=C(4,4)*EN(2,2)+EE(2,4)**2
  do KP=1,5
   MAPN%QR(KP)=MAPN%QR(KP)/MAPN%QR(5)
   if (SCALE30%LTEST.eq.1) then 
    write(6,"('  2 QR=',F15.5)") MAPN%QR(KP) 
   end if
  end do
  MAPN%KRASH=0
  MAPN%NEW=5
  MAPN%ZR=0.1 
  MAPN%ZI=1.0 
  CALL NEWTON(MAPN)
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  PR(3)=MAPN%ZR 
  PI(3)=ABS(MAPN%ZI)
  MAPN%ZI=-MAPN%ZI 
  CALL NEWTON(MAPN)
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  MAPN%ZR=0.5 
  MAPN%ZI=0.9 
  CALL NEWTON(MAPN)
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  PR(4)=MAPN%ZR 
  PI(4)=ABS(MAPN%ZI)
  if (ABS(PR(3)).LT.1E-5)  PR(3)=0.0 
  if (ABS(PR(4)).LT.1E-5)  PR(4)=0.0 
  if (ABS(1.0-PI(3)).LT.1E-5) PI(3)=1.0
  if (ABS(1.0-PI(4)).LT.1E-5) PI(4)=1.0
  MP%PC(3)=CMPLX(PR(3),PI(3)) 
  MP%PC(4)=CMPLX(PR(4),PI(4)) 
  MP%AS(1,3)=CMPLX(0.0,0.0) 
  MP%AS(2,3)=CMPLX(0.0,0.0) 
  MP%AS(3,3)=-EN(1,1)-2*MP%PC(3)*EN(1,2)-EN(2,2)*MP%PC(3)**2
  MP%AS(4,3)=-EE(1,5)-MP%PC(3)*(EE(1,4)+EE(2,5))-EE(2,4)*MP%PC(3)**2
  if (ABS(CMPLX(0.0,0.0)-MP%AS(3,3)).lt.1E-5) then
   if (ABS(CMPLX(0.0,0.0)-MP%AS(4,3)).lt.1E-5) then 
    MP%AS(3,3)=CMPLX(1.0,1.0)*(1./(2.*SQRT(C(4,4))))
    MP%AS(4,3)=CMPLX(0.0,0.0) 
   end if 
  end if
  MP%AS(1,4)=CMPLX(0.0,0.0) 
  MP%AS(2,4)=CMPLX(0.0,0.0) 
  MP%AS(3,4)=-EE(1,5)-MP%PC(4)*(EE(1,4)+EE(2,5))-EE(2,4)*MP%PC(4)**2
  MP%AS(4,4)=C(5,5)+2*MP%PC(4)*C(4,5)+C(4,4)*MP%PC(4)**2
  if (ABS(CMPLX(0.0,0.0)-MP%AS(4,4)).lt.1E-5) then
   if (ABS(CMPLX(0.0,0.0)-MP%AS(3,4)).lt.1E-5) then 
    MP%AS(3,4)=CMPLX(0.0,0.0) 
    MP%AS(4,4)=CMPLX(0.0,0.0) 
   end if 
  end if
  if (SCALE30%LTEST.eq.1) then 
   do I=1,4
    WRITE(6,"('  QUARTIC  PC= ',2F15.5)")  MP%PC(I)
   end do
   do I=1,4
    do J=1,4
     write(6,"('  QUARTIC  AS ',I1,' = ',2F20.12)") I, MP%AS(J,I)
    end do
   end do
  end if
! the original code has a goto statement here, skipping many lines below.
! we will replace this with a logical test.
!  GOTO 1000
  goto1000 = .TRUE.
 end if 
! 
!***********************************************************************
! 
!     AB  HIER WERDEN DIE "NORMALEN" FAELLE BEHANDELT                  *
!                                                                      *
!           X  X  X  X                   X  X  0  X                    *
!           X  X  X  X                   X  X  0  X                    *
!           X  X  X  X      ODER         0  0  X  0                    *
!           X  X  X  X                   X  X  0  X                    *
! 
!*********************************************************************
if (goto1000.eqv..FALSE.) then 
 QM=0.0
 G(1)=C(5,5) 
 G(2)=2.0*C(4,5) 
 G(3)=C(4,4) 
 G(4)=C(6,6) 
 G(5)=2.0*C(2,6) 
 G(6)=C(2,2) 
 G(7)=C(1,1) 
 G(8)=2.0*C(1,6) 
 G(9)=C(6,6) 
 E(1)=C(5,6) 
 E(2)=C(4,6)+C(2,5)
 E(3)=C(2,4) 
 E(4)=C(1,5) 
 E(5)=C(1,4)+C(5,6)
 E(6)=C(4,6) 
 E(7)=C(1,6) 
 E(8)=C(1,2)+C(6,6)
 E(9)=C(2,6) 
 do KQ=1,3
  do KR=1,3
   do KS=1,3
    KT=KQ+KR+KS-2 
    QM(KT,1)=QM(KT,1)+G(KQ)*G(KR+3)*G(KS+6)+2.0*E(KQ)*E(KR+3)*E(KS+6)- &
             E(KQ)*E(KR)*G(KS+6)-E(KQ+3)*E(KR+3)*G(KS+3)-E(KQ+6)*E(KR+6)*G(KS)
   end do 
  end do 
 end do 
 G(1)=EE(1,5)
 G(2)=EE(2,5)+EE(1,4)
 G(3)=EE(2,4)
 E(1)=EE(1,6)
 E(2)=EE(2,6)+EE(1,2)
 E(3)=EE(2,2)
 E(4)=EE(1,1)
 E(5)=EE(2,1)+EE(1,6)
 E(6)=EE(2,6)
 F(1)=C(5,6) 
 F(2)=C(4,6)+C(2,5)
 F(3)=C(2,4) 
 F(4)=C(1,5) 
 F(5)=C(1,4)+C(5,6)
 F(6)=C(4,6) 
 F(7)=C(1,6) 
 F(8)=C(1,2)+C(6,6)
 F(9)=C(2,6) 
 do KQ=1,3
  do KR=1,3
   do KS=1,3
    KT=KQ+KR+KS-2 
    QM(KT,2)=QM(KT,2)-(G(KQ)*G(KR+3)*G(KS+6)+E(KQ)*E(KR+6)*F(KS+3)+  &
                       E(KQ+3)*F(KR+6)*F(KS)-E(KQ+3)*F(KR+3)*G(KS+3)- &
                       E(KQ)*F(KR)*G(KS+6)-E(KQ+6)*F(KR+6)*G(KS))
   end do
  end do
 end do
 G(4)=C(5,6) 
 G(5)=C(4,6)+C(2,5)
 G(6)=C(2,4) 
 E(7)=C(1,5) 
 E(8)=C(1,4)+C(5,6)
 E(9)=C(4,6) 
 F(1)=C(5,5) 
 F(2)=2.0*C(4,5) 
 F(3)=C(4,4) 
 do KQ=1,3
  do KR=1,3
   do KS=1,3
    KT=KQ+KR+KS-2 
    QM(KT,3)=QM(KT,3)+(G(KQ)*G(KR+3)*G(KS+6)+E(KQ)*E(KR+6)*F(KS+3)+  &
                    E(KQ+3)*F(KR+6)*F(KS)-E(KQ+3)*F(KR+3)*G(KS+3)- &
                    E(KQ)*F(KR)*G(KS+6)-E(KQ+6)*F(KR+6)*G(KS))
   end do
  end do
 end do
 G(7)=C(1,6) 
 G(8)=C(1,2)+C(6,6)
 G(9)=C(2,6) 
 F(7)=C(6,6) 
 F(8)=2.0*C(2,6) 
 F(9)=C(2,2) 
 F(4)=C(5,6) 
 F(5)=C(4,6)+C(2,5)
 F(6)=C(2,4) 
 do KQ=1,3
  do KR=1,3
   do KS=1,3
    KT=KQ+KR+KS-2 
    QM(KT,4)=QM(KT,4)-(G(KQ)*G(KR+3)*G(KS+6)+E(KQ)*E(KR+6)*F(KS+3)+  &
                    E(KQ+3)*F(KR+6)*F(KS)-E(KQ+3)*F(KR+3)*G(KS+3)- &
                    E(KQ)*F(KR)*G(KS+6)-E(KQ+6)*F(KR+6)*G(KS))
   end do
  end do
 end do
 HI(1)=-EN(1,1)
 HI(5)=-2.0*EN(1,2)
 HI(9)=-EN(2,2)
 HI(2)=G(1)
 HI(6)=G(2)
 HI(10)=G(3) 
 HI(3)=E(1)
 HI(7)=E(2)
 HI(11)=E(3) 
 HI(4)=E(4)
 HI(8)=E(5)
 HI(12)=E(6) 
 do J=1,4 
  MAPN%QR(1)=MAPN%QR(1)+QM(1,J)*HI(J) 
  MAPN%QR(2)=MAPN%QR(2)+QM(2,J)*HI(J)+QM(1,J)*HI(J+4) 
  MAPN%QR(3)=MAPN%QR(3)+QM(3,J)*HI(J)+QM(2,J)*HI(J+4)+QM(1,J)*HI(J+8) 
  MAPN%QR(4)=MAPN%QR(4)+QM(4,J)*HI(J)+QM(3,J)*HI(J+4)+QM(2,J)*HI(J+8) 
  MAPN%QR(5)=MAPN%QR(5)+QM(5,J)*HI(J)+QM(4,J)*HI(J+4)+QM(3,J)*HI(J+8) 
  MAPN%QR(6)=MAPN%QR(6)+QM(6,J)*HI(J)+QM(5,J)*HI(J+4)+QM(4,J)*HI(J+8) 
  MAPN%QR(7)=MAPN%QR(7)+QM(7,J)*HI(J)+QM(6,J)*HI(J+4)+QM(5,J)*HI(J+8) 
  MAPN%QR(8)=MAPN%QR(8)+QM(7,J)*HI(J+4)+QM(6,J)*HI(J+8) 
  MAPN%QR(9)=MAPN%QR(9)+QM(7,J)*HI(J+8) 
 end do
 do KP=1,9
  MAPN%QR(KP)=MAPN%QR(KP)/MAPN%QR(9) 
  if (SCALE30%LTEST.eq.1) then
   write(6,"('  QR=',F15.5)") MAPN%QR(KP)
  end if 
 end do
!*******************************************************************
!*       Find zeroes of polynomials                                *
!*******************************************************************
 MAPN%KRASH=0 
 MAPN%NEW=9 
 MAPN%ZR=0.1
 MAPN%ZI=1.0
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(1)=MAPN%ZR
 PI(1)=ABS(MAPN%ZI) 
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 MAPN%ZR=0.5
 MAPN%ZI=0.9
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(2)=MAPN%ZR
 PI(2)=ABS (MAPN%ZI)
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 MAPN%ZR=0.1
 MAPN%ZI=1.0
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(3)=MAPN%ZR
 PI(3)=ABS(MAPN%ZI) 
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 MAPN%ZR=-MAPN%ZR
 CALL NEWTON(MAPN) 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(4)=MAPN%ZR
 PI(4)=ABS(MAPN%ZI) 
 MAPN%ZR=-C(4,5)/C(4,4) 
 MAPN%ZI=SQRT(ABS(C(4,4)*C(5,5)-C(4,5)**2))/C(4,4)
 do N=1,2
  if (((MAPN%ZR-PR(N))**2+(MAPN%ZI-PI(N))**2-(MAPN%ZR-PR(N+1))**2-(MAPN%ZI-PI(N+1))**2).lt.0.0) then 
   Z=PR(N) 
   PR(N)=PR(N+1) 
   PR(N+1)=Z 
   Z=PI(N) 
   PI(N)=PI(N+1) 
   PI(N+1)=Z 
  end if
 end do
 MP%PC=CMPLX(PR,PI)
 if (SCALE30%LTEST.eq.1) then
  write(6,"('  SOLUTIONS OF THE POLYNOM  ')") 
  write(6,"('  PC= ',2F15.8)") (MP%PC(J),J=1,4) 
 end if 
!*******************************************************************
!*        BESTIMMUNG DER EIGENVEKTOREN. VERWENDET WIRD DER         *
!*        " KOFAKTOR "ZUR BESTIMMUNG DER ELEMENTE DER EIGENVEKTOREN*
!*******************************************************************
 do K=1,4 
  A(1,1)=C(1,1)+2.0*MP%PC(K)*C(1,6)+C(6,6)*MP%PC(K)**2
  A(1,2)=C(1,6)+MP%PC(K)*(C(1,2)+C(6,6))+C(2,6)*MP%PC(K)**2 
  A(1,3)=C(1,5)+MP%PC(K)*(C(1,4)+C(5,6))+C(4,6)*MP%PC(K)**2 
  A(1,4)=EE(1,1)+MP%PC(K)*(EE(2,1)+EE(1,6))+EE(2,6)*MP%PC(K)**2 
  A(2,2)=C(6,6)+2.0*MP%PC(K)*C(2,6)+C(2,2)*MP%PC(K)**2
  A(2,3)=C(5,6)+MP%PC(K)*(C(4,6)+C(2,5))+C(2,4)*MP%PC(K)**2 
  A(2,4)=EE(1,6)+MP%PC(K)*(EE(2,6)+EE(1,2))+EE(2,2)*MP%PC(K)**2 
  A(3,3)=C(5,5)+2.0*MP%PC(K)*C(4,5)+C(4,4)*MP%PC(K)**2
  A(3,4)=EE(1,5)+MP%PC(K)*(EE(2,5)+EE(1,4))+EE(2,4)*MP%PC(K)**2 
  A(4,4)=-(EN(1,1)+2.0*MP%PC(K)*EN(1,2)+EN(2,2)*MP%PC(K)**2)
  A(2,1)=A(1,2) 
  A(3,1)=A(1,3) 
  A(3,2)=A(2,3) 
  A(4,1)=A(1,4) 
  A(4,2)=A(2,4) 
  A(4,3)=A(3,4) 
  MX=N1(K)
  MY=N2(K)
  MZ=N3(K)
  do J=1,4 
   NX=N1(J)
   NY=N2(J)
   NZ=N3(J)
   MP%AS(J,K)=((-1)**(J+K))*(A(NX,MX)*A(NY,MY)*A(NZ,MZ)+A(NX,MY)*A(NY,MZ)*A(NZ,MX)+A(NX,MZ)*A(NY,MX)*A(NZ,MY)- &
               A(NX,MZ)*A(NY,MY)*A(NZ,MX)-A(NX,MX)*A(NY,MZ)*A(NZ,MY)-A(NX,MY)*A(NY,MX)*A(NZ,MZ))
  end do
  if (SCALE30%LTEST.eq.1) then
   write(6,"('  AS--EIGENVEKTOR',8F8.4)") MP%AS(1,K),MP%AS(2,K),MP%AS(3,K),MP%AS(4,K)
   do K1=1,4 
    AY(K1)=CMPLX(0.0,0.0) 
    do K2=1,4 
     AY(K1)=AY(K1)+A(K1,K2)*MP%AS(K2,K) 
    end do
   end do
   write(6,"('  PROBE--',F15.8)") (AY(K3),K3=1,4)
  endif 
 end do 
 do I=1,4 
  X=0.0 
  do J=1,4 
   X=X+MP%AS(J,I)*CONJG(MP%AS(J,I))
  end do
  X=SQRT(X) 
  do J=1,4
   MP%AS(J,I)=MP%AS(J,I)/X 
  end do
 end do
end if  ! (if (goto1000.eqv..FALSE.))
! original code has a continue statement here ...
!1000 CONTINUE
!*******************************************************************
!*        BERECHNUNG DER VEKTOREN  L(I,K)                          *
!*        NACH BARNETT UND LOTHE                                   *
!*******************************************************************
 do J=1,3 
  NJ=NN(J)
  do K=1,4 
   MP%EL(J,K)=CMPLX(0.0,0.0)
   do L=1,3 
    NL=NN(L)
    ML=MM(L)
    MP%EL(J,K)=MP%EL(J,K)+(C(NJ,ML)+MP%PC(K)*C(NJ,NL))*MP%AS(L,K) 
   end do
   MP%EL(J,K)=-(MP%EL(J,K)+(EE(1,NJ)+MP%PC(K)*EE(2,NJ))*MP%AS(4,K))
  end do
 end do
 do K=1,4 
  MP%EL(4,K)=CMPLX(0.0,0.0)
  do L=1,3 
   NL=NN(L)
   ML=MM(L)
   MP%EL(4,K)=MP%EL(4,K)+(EE(2,ML)+MP%PC(K)*EE(2,NL))*MP%AS(L,K) 
  end do
  MP%EL(4,K)=-(MP%EL(4,K)-(EN(2,1)+MP%PC(K)*EN(2,2))*MP%AS(4,K))
 end do
!*******************************************************************
!*      NORMIERUNG     2*A(I,K)*L(J,K)=1                           *
!*******************************************************************
 do K=1,4 
  MXX(K)=CMPLX(0.0,0.0) 
  do L=1,4 
   MXX(K)=MXX(K)+2.0*MP%AS(L,K)*MP%EL(L,K) 
  end do
  IF (MXX(K).EQ.CMPLX(0.0,0.0)) MXX(K)=CMPLX(1.0,0.0) 
  do J1=1,4
   MP%EL(J1,K)=MP%EL(J1,K)/CSQRT(MXX(K)) 
   MP%AS(J1,K)=MP%AS(J1,K)/CSQRT(MXX(K)) 
  end do
 end do
 if (SCALE30%LTEST.eq.1) then
  write(6,"('  AS-LOTHE',4F15.8)") ((MP%AS(I,J),J=1,4),I=1,4)
  write(6,"('  EL-LOTHE',4F15.8)")  ((MP%EL(I,J),J=1,4),I=1,4) 
  do K=1,4
   do L=1,4
    AXA(K,L)=CMPLX(0.0,0.0) 
    AXL(K,L)=CMPLX(0.0,0.0) 
    LXL(K,L)=CMPLX(0.0,0.0) 
    do I=1,4
     AXA(K,L)=AXA(K,L)+MP%AS(K,I)*MP%AS(L,I) 
     LXL(K,L)=LXL(K,L)+MP%EL(K,I)*MP%EL(L,I) 
     AXL(K,L)=AXL(K,L)+MP%AS(K,I)*MP%EL(L,I) 
    end do
    AXA(K,L)=2.0*REAL(AXA(K,L)) 
    LXL(K,L)=2.0*REAL(LXL(K,L)) 
    AXL(K,L)=2.0*REAL(AXL(K,L)) 
   end do
  end do
  write(6,"('  AXA',4F20.8)") ((AXA(I1,I2),I2=1,4),I1=1,4) 
  write(6,"('  LXL',4F20.8)") ((LXL(I1,I2),I2=1,4),I1=1,4) 
  write(6,"('  AXL',4F20.8)") ((AXL(I1,I2),I2=1,4),I1=1,4) 
  PR1=PR(1)**2
  PI1=PI(1)**2
  PR2=PR(2)**2
  PI2=PI(2)**2
  PR3=PR(3)**2
  PI3=PI(3)**2
  PR4=PR(4)**2
  PI4=PI(4)**2
  AZ=-2.0*(PR(1)+PR(2)) 
  BY=PR1+PI1+PR2+PI2+4.0*PR(1)*PR(2)
  CY=-2.0*((PR1+PI1)*PR(2)+(PR2+PI2)*PR(1)) 
  D=(PR1+PI1)*(PR2+PI2) 
  AZZ=-2.0*(PR(3)+PR(4))
  BZ=PR3+PI3+PR4+PI4+4.0*PR(3)*PR(4)
  CZ=-2.0*((PR3+PI3)*PR(4)+(PR4+PI4)*PR(3)) 
  DZ=(PR3+PI3)*(PR4+PI4)
  MAPN%QR(9)=1.0 
  MAPN%QR(8)=AZ+AZZ
  MAPN%QR(7)=BY+AZ*AZZ+BZ
  MAPN%QR(6)=CY+AZZ*BY+AZ*BZ+CZ
  MAPN%QR(5)=D+AZZ*CY+BZ*BY+AZ*CZ+DZ 
  MAPN%QR(4)=AZZ*D+BZ*CY+BY*CZ+AZ*DZ 
  MAPN%QR(3)=BZ*D+CY*CZ+BY*DZ
  MAPN%QR(2)=D*CZ+CY*DZ
  MAPN%QR(1)=D*DZ
  write(6,"('  KONTROLLE DER WURZELN DES POLYNOMS DURCH BERECHNUNG DER POLYNOMSKOEFFIZIENTEN ') ")
  write(6,"(3F10.6)") (MAPN%QR(K),K=1,9)
 end if 
end subroutine PANCALC

!--------------------------------------------------------------------------
!
! SUBROUTINE: RKM
!
!> @author Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.
!
!> @brief Runge-Kutta integration of Howie-Whelan equations
! 
!> @param MRD
!
!> @date    08/13/19 MDG 1.0 adapted from .f77 original
!--------------------------------------------------------------------------
recursive subroutine RKM(MRD)
!DEC$ ATTRIBUTES DLLEXPORT :: RKM

! Note:
! The original RKM routine was rather complicated due to the extensive
! use of computed goto statements.  The new routine is still ugly, but 
! no longer uses any goto statements, computed or otherwise...
! 
! INTEGRATE THE HOWIE-WHELAN EQUATIONS

use error 

IMPLICIT NONE

type(MRD_block),INTENT(INOUT)     :: MRD
!f2py intent(in,out) ::  MRD

integer(kind=irg)                 :: M1, M2, J, M, leave
real(kind=sgl)                    :: ERHIGH, ERLOW, H1, H2, H3, XT, TEST
logical                           :: verbose = .FALSE.

M1=4 
MRD%KOUNT=0  
ERHIGH=5.0*MRD%ERROR 
ERLOW=0.03125*ERHIGH  
J = 1
do while (J.ne.100) 
 select case(J)
  case(1)
    if (verbose) write (*,*) 'case 1'
    if (MRD%Q.ne.0.0) then
      J = 2
    else
      J = 20
    end if

  case(2)
    if (verbose) write (*,*) 'case 2'
    IF ( ( (MRD%X1-MRD%X)/MRD%Q - 1.0000001 ).le.0.0 ) then ! 29,29,7
      J=29
    else 
      J=7
    end if  

  case(3)
    if (verbose) write (*,*) 'case 3'
    MRD%Q=H1; J=11

  case(4)
    if (verbose) write (*,*) 'case 4'
    MRD%Q=2.0*MRD%Q; H2=0.5*MRD%Q; H3=MRD%Q/3.0; J=2

  case(5)
    if (verbose) write (*,*) 'case 5'
    IF ((H1-2.0*MRD%Q).lt.0.0) then ! 30,3,3
      J = 30
    else 
      J = 3
    end if

  case(6)
    if (verbose) write (*,*) 'case 6'
    M1=5; H2=0.5*MRD%Q; H3=MRD%Q/3.0; J=7

  case(7)
    if (verbose) write (*,*) 'case 7'
    XT=MRD%X; MRD%YT(:)=MRD%Y(:); J=8

  case(8)
    if (verbose) write (*,*) 'case 8 '
    CALL DERIV(MRD)
    MRD%DT(:,1)=H3*MRD%D(:); MRD%Y(:)=MRD%Y(:)+MRD%DT(:,1); MRD%X=MRD%X+H3
    CALL DERIV(MRD)
    MRD%Y(:)=MRD%YT(:)+0.5*(MRD%DT(:,1)+H3*MRD%D(:)); MRD%SKIP=1.0
    CALL DERIV(MRD)
    MRD%SKIP=0.0; MRD%DT(:,2)=MRD%Q*MRD%D(:); MRD%Y(:)=MRD%YT(:)+0.375*(MRD%DT(:,1)+MRD%DT(:,2)); MRD%X=XT+H2
    CALL DERIV(MRD)
    MRD%DT(:,3)=4.0*H3*MRD%D(:); MRD%Y(:)=MRD%YT(:)+1.5*(MRD%DT(:,1)+MRD%DT(:,3)-MRD%DT(:,2)); MRD%X=XT+MRD%Q
    CALL DERIV(MRD)
    M2=0
    J = 10

  case(9)
    if (verbose) write (*,*) 'case 9'
    M1=4; MRD%Q=0.5*MRD%Q; H2=0.5*MRD%Q; H3=MRD%Q/3.0; MRD%Y(:)=MRD%YT(:); MRD%X=XT; J=8

  case(10)
    if (verbose) write (*,*) 'case 10'
    leave = 0
    doten: do M=1,8 
      MRD%DT(M,4)=H3*MRD%D(M)
      TEST=ABS (MRD%DT(M,1)+MRD%DT(M,3)-0.5*(MRD%DT(M,4)+3.0*MRD%DT(M,2)))
      IF ((TEST-ERHIGH).lt.0.0) then ! 26,9,9
        IF ((TEST-ERLOW).lt.0.0) then ! 10,27,27
         CYCLE doten
        END IF 
        M2=-2     ! old line 27
      ELSE 
        J = 9
        leave = 1
        EXIT doten     ! jump out of case select
      END IF
    end do doten  ! old line 10
    if (leave.eq.0) then 
      MRD%Y(:)=MRD%YT(:)+0.5*(MRD%DT(:,1)+MRD%DT(:,3)+MRD%DT(:,4))
      MRD%KOUNT=MRD%KOUNT+1
      J=M1+M2
    end if

  case(11)
    if (verbose) write (*,*) 'case 11'
    H2=0.5*MRD%Q; H3=MRD%Q/3.0; J=100

  case(20)
    if (verbose) write (*,*) 'case 20'
    MRD%Q=MRD%X1-MRD%X; H1=MRD%Q; J=6 

  case(29)
    if (verbose) write (*,*) 'case 29'
    H1=MRD%Q; MRD%Q=MRD%X1-MRD%X; J=6

  case(30)
    if (verbose) write (*,*) 'case 30'
    MRD%Q=2.0*MRD%Q; J=11

  case default 
    call FatalError('RKM','case value does not exist')

 end select
end do 
! if J=100, then we return from this routine     

end subroutine RKM 

!--------------------------------------------------------------------------
!
! SUBROUTINE: DERIV
!
!> @author Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.
!
!> @brief Derivative calculation
! 
!> @param MRD
!
!> @date    08/13/19 MDG 1.0 adapted from .f77 original
!--------------------------------------------------------------------------
recursive subroutine DERIV(MRD)
!DEC$ ATTRIBUTES DLLEXPORT :: DERIV
! 
!************************************************************** 
!*     SUBROUTINE DERIV   BERECHNUNG DER ABLEITUNG VON        * 
!*     G IN R NACH DER FORMEL VON STROH, ERWEITERT FUER       * 
!*     PIEZOELEKTRISCHE KRISTALLE. BERUECKSICHTIGUNG DER      * 
!*     ANOMALEN ABSORPTION                                    * 
!************************************************************** 

IMPLICIT NONE

type(MRD_block),INTENT(INOUT)     :: MRD
!f2py intent(in,out) ::  MRD

real       :: X11, X22, X33, X44, R1, R2, R3, R4, BETA1, BETA2, BETA3, BETA4, Z
real,save  :: BETA
! 
! 
 if (MRD%SKIP.eq.0.0) then
!*************************************
!*     ERSTE VERSETZUNG              *
!*************************************
  if ((MRD%X-MRD%CN(21)).eq.0.0) then 
   X11=1.0E-10 
  else  
   X11=MRD%X-MRD%CN(21)
  end if
  R1=(MRD%CN(30)-MRD%CN(31))/X11
!*************************************
!*     ZWEITE VERSETZUNG             *
!*************************************
  if ((MRD%X+MRD%CN(21)).eq.0.0) then 
   X22=1.0E-10 
  else  
   X22=MRD%X+MRD%CN(21)
  end if
  R2=(MRD%CN(30)+MRD%CN(31))/X22
!*************************************
!*     DRITTE VERSETZUNG             *
!*************************************
  if ((MRD%X+MRD%CN(43)).eq.0.0) then
   X33=1.0E-10 
  else 
   X33= MRD%X+MRD%CN(43) 
  end if
  R3=(MRD%CN(30)+MRD%CN(44))/X33
!*************************************
!*     VIERTE VERSETZUNG             *
!*************************************
  if ((MRD%X-MRD%CN(43)).eq.0.0) then  
   X44=1.0E-10 
  else 
   X44=MRD%X-MRD%CN(43)
  end if
  R4=(MRD%CN(30)-MRD%CN(44))/X44
  BETA1=(((R1*MRD%CN(1)+MRD%CN(5))/((R1+MRD%CN(9))**2+MRD%CN(13)))+&
         ((R1*MRD%CN(2)+MRD%CN(6))/((R1+MRD%CN(10))**2+MRD%CN(14)))+&
         ((R1*MRD%CN(3)+MRD%CN(7))/((R1+MRD%CN(11))**2+MRD%CN(15)))+&
         ((R1*MRD%CN(4)+MRD%CN(8))/((R1+MRD%CN(12))**2+MRD%CN(16))))/X11 
  BETA2=(((R2*MRD%CN(22)+MRD%CN(26))/((R2+MRD%CN(9))**2+MRD%CN(13)))+&
         ((R2*MRD%CN(23)+MRD%CN(27))/((R2+MRD%CN(10))**2+MRD%CN(14)))+&
         ((R2*MRD%CN(24)+MRD%CN(28))/((R2+MRD%CN(11))**2+MRD%CN(15)))+&
         ((R2*MRD%CN(25)+MRD%CN(29))/((R2+MRD%CN(12))**2+MRD%CN(16))))/X22 
  BETA3=(((R3*MRD%CN(32)+MRD%CN(36))/((R3+MRD%CN(9))**2+MRD%CN(13)))+&
         ((R3*MRD%CN(33)+MRD%CN(37))/((R3+MRD%CN(10))**2+MRD%CN(14)))+&
         ((R3*MRD%CN(34)+MRD%CN(38))/((R3+MRD%CN(11))**2+MRD%CN(15)))+&
         ((R3*MRD%CN(35)+MRD%CN(39))/((R3+MRD%CN(12))**2+MRD%CN(16))))/X33 
  BETA4=(((R4*MRD%CN(51)+MRD%CN(55))/((R4+MRD%CN(9))**2+MRD%CN(13)))+&
         ((R4*MRD%CN(52)+MRD%CN(56))/((R4+MRD%CN(10))**2+MRD%CN(14)))+&
         ((R4*MRD%CN(53)+MRD%CN(57))/((R4+MRD%CN(11))**2+MRD%CN(15)))+&
         ((R4*MRD%CN(54)+MRD%CN(58))/((R4+MRD%CN(12))**2+MRD%CN(16))))/X44 
  BETA=MRD%CN(18)+BETA1+BETA2+BETA3+BETA4 
end if
!*******************************************************************
!*     BERUECKSICHTIGUNG DER ANOMALEN ABSORPTION                   *
!*******************************************************************
  Z=MRD%ANO*(MRD%Y(1)+MRD%Y(3)) 
  MRD%D(1)=Z-MRD%Y(4) 
  MRD%D(3)=-BETA*MRD%Y(4)+Z-MRD%Y(2)
  Z=MRD%ANO*(MRD%Y(2)+MRD%Y(4)) 
  MRD%D(2)=Z+MRD%Y(3) 
  MRD%D(4)=BETA*MRD%Y(3)+Z+MRD%Y(1) 
  Z=MRD%ANO*(MRD%Y(5)+MRD%Y(7)) 
  MRD%D(5)=Z-MRD%Y(8) 
  MRD%D(7)=-BETA*MRD%Y(8)+Z-MRD%Y(6)
  Z=MRD%ANO*(MRD%Y(6)+MRD%Y(8)) 
  MRD%D(6)=Z+MRD%Y(7) 
  MRD%D(8)=BETA*MRD%Y(7)+Z+MRD%Y(5) 
end subroutine DERIV

end module hhmod
