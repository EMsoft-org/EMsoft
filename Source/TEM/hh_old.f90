
module hh

IMPLICIT NONE

! All common blocks of the original hh.f program are converted
! into user-defined types and declared as variables with the 
! original common block name.

! COMMON/MAPN/NEW,ZR,ZI,QR(9),QI(9),KRASH 
type MAPN_block
  integer        :: KRASH, NEW
  real           :: ZR, ZI, QR(9), QI(9)
end type
type(MAPN_block) :: MAPN

! COMMON/MA/PR(4),PI(4),AR(4,4),AI(4,4),EMR(4,4),EMI(4,4),H(4,4) 
type MA_block
  real           :: PR(4), PI(4), AR(4,4), AI(4,4), EMR(4,4), EMI(4,4), H(4,4) 
end type
type(MA_block)   :: MA

! COMMON/MKAP/D1(6,6),EP(3,6),EA(3,3) 
type MKAP_block
 real            :: D1(6,6), EP(3,6), EA(3,3) 
end type
type(MKAP_block) :: MKAP

! COMMON/MRD/CN(61),X,X1,Y(8),ERROR,Q,KOUNT,D(8),YT(8),DT(8,4),ANO,SKIP 
type MRD_block
 real            :: CN(61), X, X1, Y(8), ERROR, Q, D(8), YT(8), DT(8,4), ANO, SKIP 
 integer         :: KOUNT
end type
type(MRD_block)  :: MRD

! COMMON/MT/LU(3),LG(3),LBM(3),LFN(3),LB(3),LB2(3),LB3(3),LB4(3), 
!           LFP(3),LFP1(3),LFP3(3),LS1(3),LS2(3),LS3(3),TLU(3),TLG(3),
!           TLBM(3),TLFN(3),TLB(3),TLB2(3),TLB3(3),TLB4(3),TLFP(3),
!           TLFP1(3),TLFP3(3),TLS1(3),TLS2(3),TLS3(3),LF1(3),
!           LF2(3),LF3(3),LF4(3),TLF1(3),TLF2(3),TLF3(3),TLF4(3) 
type MT_block
 real            :: TLU(3), TLG(3), TLBM(3), TLFN(3), TLB(3), TLB2(3), TLB3(3), TLB4(3), TLFP(3), &
                    TLFP1(3), TLFP3(3), TLS1(3), TLS2(3), TLS3(3), TLF1(3), TLF2(3), TLF3(3), TLF4(3) 
 integer         :: LU(3), LG(3), LBM(3), LFN(3), LB(3), LB2(3), LB3(3), LB4(3), &
                    LFP(3), LFP1(3), LFP3(3), LS1(3), LS2(3), LS3(3), LF1(3), LF2(3), LF3(3), LF4(3)
end type
type(MT_block)   :: MT

! COMMON/MKT/AT(3,3),ATR(3,3)
type MKT_block
 real            :: AT(3,3), ATR(3,3)
end type
type(MKT_block)  :: MKT

! COMMON/SCALE30/LTEST
type SCALE30_block
 integer         :: LTEST
end type
type(SCALE30_block)   :: SCALE30

! COMMON/MP/PC(4),AS(4,4),EL(4,4) 
type MP_block
 complex         :: PC(4), AS(4,4), EL(4,4) 
end type
type(MP_block)   :: MP

! COMMON/MAP/DC(3,3)
type MAP_block
 real            :: DC(3,3)
end type
type(MAP_block)  :: MAP


! variables not in common blocks
complex          :: SU(4), CNX(8), MXXX(4,4), MYYY(4,4), MZZZ(4,4)
real             :: LC1, LC2, LC3, LC4
integer,parameter:: ICOL=256, IROW=160, ICOLP=257
character(15)    :: IY
real             :: GD(3), BD(4), B2D(4), B3D(4), B4D(4), BM(3), FN(3), FP1X(3),  &
                    FPX(3), FP3X(3), FP(3), FP3(3), FNX(3), DCX(3,3), DR(4), DI(4), &
                    UR(4,4), UI(4,4), VR(4,4), VI(4,4), DD(3), SUR(4), SUI(4), &
                    FX(ICOL,4), UX(3), AB(3), AB1(3), POSA(4), POSB(4), &
                    COORD(4), HANDL(4), HANDR(4), TBD(IROW,ICOLP), TQB(ICOLP),TQD(ICOLP), &
                    TEMPY(8), QL1(4), QL2(4), QL3(4), QL4(4), S(4,4), QS(4,4), &
                    BFINTENS(ICOL,IROW),DFINTENS(ICOL,IROW)
integer          :: IX(ICOLP), IXX(ICOLP), ITYPE(4)
integer,parameter:: NP(3) = (/2,3,1/), NQ(3) = (/3,1,2/)





contains

subroutine NEWTON 

IMPLICIT NONE

integer          :: KOUNT, KONVRG, J, M
real             :: XR, XI, YR, YI, TR, TI, F
  
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


subroutine ANCALC 
! 


IMPLICIT NONE

integer           :: I, J, K, L, M, N, LT, KQ, KR, KS, KT, NJ, I1, I2, J1, J2, K1, K2, LP, LQ, KP, NL, ML
integer,parameter :: L1(6)=(/1,2,3,2,3,1/), L2(6)=(/1,2,3,3,1,2/), L3(3,3)= reshape( (/1,6,5,6,2,4,5,4,3/), (/3,3/) ), & 
                     N1(4)=(/2,4,2,1/), N2(4)=(/3,1,4,2/), N3(4)=(/4,3,1,3/), NN(3)=(/6,2,4/), MM(3)=(/1,6,5/)
real              :: C(6,6), EE(3,6), EN(3,3), QM(7,4), G(9), E(9), F(9), HI(12), &
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
  write(*,"('  C-DC ',6F10.6)") ((C(I,J),J=1,6),I=1,6)
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
    MAPN%QR(KT)=MAPN%QR(KT)+G(KQ)*G(KR+3)*G(KS+6)+2.0*E(KQ)*E(KR+3)*E(KS+6)-E(KQ)*E(KR)*G(KS+6)-E(KQ+3)*E(KR+3)*G(KS+3)-E(KQ+6)*E(KR+6)*G(KS)
   end do
  end do
 end do
 if (SCALE30%LTEST.eq.1) then
  write(6,"('  QR- HH4 ',F30.15)") (MAPN%QR(KP),KP=1,7)
 end if 
 MAPN%QR=MAPN%QR/MAPN%QR(7)
 MAPN%KRASH=0
 MAPN%NEW=7
 MAPN%ZR=0.1
 MAPN%ZI=1.0
 CALL NEWTON
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MA%PR(1)=MAPN%ZR
 MA%PI(1)=ABS(MAPN%ZI)
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MAPN%ZR=0.5
 MAPN%ZI=0.9
 CALL NEWTON
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MA%PR(2)=MAPN%ZR
 MA%PI(2)=ABS(MAPN%ZI)
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON
 if (MAPN%KRASH.lt.0) then
  STOP ' Solution of sextic does not converge '
 end if
 if (MAPN%KRASH.gt.0) then
  STOP ' Sextic has real root '
 end if
 MAPN%ZR=-MAPN%ZR
 CALL NEWTON
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
   WRITE(6,"(' Roots HH4 ',2F20.15)") MA%PR(I),MA%PI(I)
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
    WRITE(6,"('   Vektoren as HH4 ',2F20.12)") MA%AR(J,K),MA%AI(J,K)
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


subroutine PANCALC

IMPLICIT NONE

!**************************************************** 
!*     SUBROUTINE PANCALC has been extended for a   * 
!*     piezoelectric crystal with a core-charge "Q" * 
!**************************************************** 
complex         :: PC, AS, EL
complex         :: A(4,4), AY(4), AXA(4,4), AXL(4,4), LXL(4,4), MXX(4), MXXX(4,4)
real            :: C(6,6), EE(3,6), EN(3,3), QM(7,4), G(9), E(9), F(9), HI(12),  &
                   PR(4), PI(4), X, Y, Z, PR1, PI1, PR2, PI2, PR3, PI3, PR4, PI4, AZ, &
                   BY, CY, D, AZZ, BZ, CZ, DZ
integer           :: I, J, K, L, M, N, LP, LQ, LT, LV, KP, KQ, KR, KS, KT, &
                     MX, MY, MZ, NX, NY, NZ, NJ, NL, ML, K1, K2, K3, J1, I1, I2
integer,parameter :: L1(6)=(/1,2,3,2,3,1/), L2(6)=(/1,2,3,3,1,2/), L3(3,3)= reshape( (/1,6,5,6,2,4,5,4,3/), (/3,3/)), & 
                     N1(4)=(/2,1,1,1/), N2(4)=(/3,3,2,2/), N3(4)=(/4,4,4,3/), NN(3)=(/6,2,4/), MM(3)=(/1,6,5/)

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
  CALL NEWTON
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  PR(1)=MAPN%ZR 
  PI(1)=ABS(MAPN%ZI)
  MAPN%ZI=-MAPN%ZI 
  CALL NEWTON
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP  'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  MAPN%ZR=0.5 
  MAPN%ZI=0.9 
  CALL NEWTON
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
  CALL NEWTON
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  PR(3)=MAPN%ZR 
  PI(3)=ABS(MAPN%ZI)
  MAPN%ZI=-MAPN%ZI 
  CALL NEWTON
  if (MAPN%KRASH.gt.0) then 
   STOP 'POLYNOM(4) HAS A REAL ROOT '
  end if
  if (MAPN%KRASH.lt.0) then 
   STOP 'SOLUTION OF POLYNOM(4) DOES NOT CONVERGE '
  end if
  MAPN%ZR=0.5 
  MAPN%ZI=0.9 
  CALL NEWTON
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
  GOTO 1000
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
 CALL NEWTON 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(1)=MAPN%ZR
 PI(1)=ABS(MAPN%ZI) 
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 MAPN%ZR=0.5
 MAPN%ZI=0.9
 CALL NEWTON 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(2)=MAPN%ZR
 PI(2)=ABS (MAPN%ZI)
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 MAPN%ZR=0.1
 MAPN%ZI=1.0
 CALL NEWTON 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 PR(3)=MAPN%ZR
 PI(3)=ABS(MAPN%ZI) 
 MAPN%ZI=-MAPN%ZI
 CALL NEWTON 
 if (MAPN%KRASH.gt.0) then 
  STOP 'POLYNOM(4) HAS A REAL ROOT '
 end if
 if (MAPN%KRASH.lt.0) then 
  STOP 'SOLUTION OF POLYNOMIAL DOES NOT CONVERGE '
 end if
 MAPN%ZR=-MAPN%ZR
 CALL NEWTON 
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
 1000 CONTINUE
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


subroutine RKM
! 
! The original RKM routine was rather complicated due to the extensive
! use of computed goto statements.  The new routine uses modern program
! control statements instead of the computed goto and such.
! 
! INTEGRATE THE HOWIE-WHELAN EQUATIONS

IMPLICIT NONE

integer         :: M1, M2, J, M
real            :: ERHIGH, ERLOW, H1, H2, H3, XT, TEST

 M1=4 
 MRD%KOUNT=0 
 ERHIGH=5.0*MRD%ERROR 
 ERLOW=0.03125*ERHIGH 
 J = 1
 do while (J.gt.0)
  if (J.eq.1) then
   if (MRD%Q.ne.0.0) then 
    if (((MRD%X1-MRD%X)/MRD%Q-1.0000001).le.0.0) then
     H1=MRD%Q    
     MRD%Q=MRD%X1-MRD%X  
     M1=5  
     H2=0.5*MRD%Q
     H3=MRD%Q/3.0 
     XT=MRD%X    
     MRD%YT=MRD%Y  
    else 
     XT=MRD%X    
     MRD%YT=MRD%Y  
    end if
   else
    MRD%Q=MRD%X1-MRD%X      
    H1=MRD%Q   
    M1=5  
    H2=0.5*MRD%Q
    H3=MRD%Q/3.0 
    XT=MRD%X    
    MRD%YT=MRD%Y  
   end if
  end if

eight: do
   CALL DERIV 
   do M=1,8 
    MRD%DT(M,1)=H3*MRD%D(M) 
    MRD%Y(M)=MRD%Y(M)+MRD%DT(M,1) 
   end do
   MRD%X=MRD%X+H3        
   CALL DERIV   
   do  M=1,8  
    MRD%Y(M)=MRD%YT(M)+0.5*(MRD%DT(M,1)+H3*MRD%D(M))
   end do
   MRD%SKIP=1.0  
   CALL DERIV
   MRD%SKIP=0.0  
   do M=1,8 
    MRD%DT(M,2)=MRD%Q*MRD%D(M)
    MRD%Y(M)=MRD%YT(M)+0.375*(MRD%DT(M,1)+MRD%DT(M,2)) 
   end do
   MRD%X=XT+H2   
   CALL DERIV 
   do M=1,8 
    MRD%DT(M,3)=4.0*H3*MRD%D(M) 
    MRD%Y(M)=MRD%YT(M)+1.5*(MRD%DT(M,1)+MRD%DT(M,3)-MRD%DT(M,2)) 
   end do
   MRD%X=XT+MRD%Q 
   CALL DERIV  
   M2=0   
   do M=1,8  
    MRD%DT(M,4)=H3*MRD%D(M)  
    TEST=ABS (MRD%DT(M,1)+MRD%DT(M,3)-0.5*(MRD%DT(M,4)+3.0*MRD%DT(M,2)))
    if ((TEST-ERHIGH).ge.0.0) then 
     M1=4 
     MRD%Q=0.5*MRD%Q
     H2=0.5*MRD%Q   
     H3=MRD%Q/3.0  
     MRD%Y=MRD%YT
     MRD%X=XT     
     CYCLE eight
    else
     if ((TEST-ERLOW).ge.0.0) then 
      M2=-2 
     end if
    end if
   end do 
   EXIT eight
  end do eight

  do  M=1,8 
   MRD%Y(M)=MRD%YT(M)+0.5*(MRD%DT(M,1)+MRD%DT(M,3)+MRD%DT(M,4))  
  end do
  J=M1+M2    
  MRD%KOUNT=MRD%KOUNT+1 

  select case(J)
  case(1);

  case(2); 
  if (((MRD%X1-MRD%X)/MRD%Q-1.0000001).le.0.0) then 
   H1=MRD%Q    
   MRD%Q=MRD%X1-MRD%X  
   M1=5  
   H2=0.5*MRD%Q
   H3=MRD%Q/3.0 
   XT=MRD%X    
   MRD%YT=MRD%Y  
  else
   XT=MRD%X    
   MRD%YT=MRD%Y  
  end if 

  case(3);
   MRD%Q=H1
   H2=0.5*MRD%Q 
   H3=MRD%Q/3.0 
   J=0

  case(4);
   MRD%Q=2.0*MRD%Q   
   H2=0.5*MRD%Q  
   H3=MRD%Q/3.0  
   if (((MRD%X1-MRD%X)/MRD%Q-1.0000001).le.0.0) then   
    H1=MRD%Q    
    MRD%Q=MRD%X1-MRD%X  
    M1=5  
    H2=0.5*MRD%Q
    H3=MRD%Q/3.0 
    XT=MRD%X    
    MRD%YT=MRD%Y  
   else
    XT=MRD%X    
    MRD%YT=MRD%Y  
   end if 

  case(5);
   if ((H1-2.0*MRD%Q).lt.0.0) then 
    MRD%Q=2.0*MRD%Q   
   else 
    MRD%Q=H1   
   end if
   H2=0.5*MRD%Q 
   H3=MRD%Q/3.0 
   J=0

  end select
 end do
end subroutine RKM 



subroutine DERIV
! 
!************************************************************** 
!*     SUBROUTINE DERIV   BERECHNUNG DER ABLEITUNG VON        * 
!*     G IN R NACH DER FORMEL VON STROH, ERWEITERT FUER       * 
!*     PIEZOELEKTRISCHE KRISTALLE. BERUECKSICHTIGUNG DER      * 
!*     ANOMALEN ABSORPTION                                    * 
!************************************************************** 

IMPLICIT NONE

real       :: X11, X22, X33, X44, R1, R2, R3, R4, BETA1, BETA2, BETA3, BETA4, BETA, Z
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
  BETA1=(((R1*MRD%CN(1)+MRD%CN(5))/((R1+MRD%CN(9))**2+MRD%CN(13)))+((R1*MRD%CN(2)+MRD%CN(6))/((R1+MRD%CN(10))**2+MRD%CN(14)))+&
     ((R1*MRD%CN(3)+MRD%CN(7))/((R1+MRD%CN(11))**2+MRD%CN(15)))+((R1*MRD%CN(4)+MRD%CN(8))/((R1+MRD%CN(12))**2+MRD%CN(16))))/X11 
  BETA2=(((R2*MRD%CN(22)+MRD%CN(26))/((R2+MRD%CN(9))**2+MRD%CN(13)))+((R2*MRD%CN(23)+MRD%CN(27))/((R2+MRD%CN(10))**2+MRD%CN(14)))+&
     ((R2*MRD%CN(24)+MRD%CN(28))/((R2+MRD%CN(11))**2+MRD%CN(15)))+((R2*MRD%CN(25)+MRD%CN(29))/((R2+MRD%CN(12))**2+MRD%CN(16))))/X22 
  BETA3=(((R3*MRD%CN(32)+MRD%CN(36))/((R3+MRD%CN(9))**2+MRD%CN(13)))+((R3*MRD%CN(33)+MRD%CN(37))/((R3+MRD%CN(10))**2+MRD%CN(14)))+&
     ((R3*MRD%CN(34)+MRD%CN(38))/((R3+MRD%CN(11))**2+MRD%CN(15)))+((R3*MRD%CN(35)+MRD%CN(39))/((R3+MRD%CN(12))**2+MRD%CN(16))))/X33 
  BETA4=(((R4*MRD%CN(51)+MRD%CN(55))/((R4+MRD%CN(9))**2+MRD%CN(13)))+((R4*MRD%CN(52)+MRD%CN(56))/((R4+MRD%CN(10))**2+MRD%CN(14)))+&
     ((R4*MRD%CN(53)+MRD%CN(57))/((R4+MRD%CN(11))**2+MRD%CN(15)))+((R4*MRD%CN(54)+MRD%CN(58))/((R4+MRD%CN(12))**2+MRD%CN(16))))/X44 
  BETA=MRD%CN(18)+BETA1+BETA2+BETA3+BETA4 
 else
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
 end if
end subroutine DERIV

end module hh




program hh4
! original program header 
!** (INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,TAPE10) 
!***********************************************************************
!*           HEADHUMBLE 4 - PIEZOELEKTRISCHE KRISTALLE                 *
!*           PROGRAMM FUER 4 SYMMETRISCHE VERSETZUNGEN MIT             *
!*           3 STAPELFEHLERN WOBEI FUER DIE FAULTPLANES                *
!*           GILT: FP1 = FP3. KONFIGURATION SYMMETRISCH ZUM            *
!*           URSPRUNG. KEIN D - UND M - DRUCK                          *
!*            30. AUGUST 1983                                          *
!***********************************************************************
!
! Translated to Fortran-90 by MDG.
! Integrated with other source code from CTEMsoft package.
! Added TIFF output + support for tilt series.
!
! The new f90 code is more readable than the original f77 code,
! mostly because the more modern Fortran language has better control
! structures.
!
! All ordinary and computed goto statements have been replaced with
! case and other control statements.  No line labels are used at all.
!
!***********************************************************************
!*       Main program - Problem geometry                               *
!***********************************************************************

use constants 
use io
use files
use crystal
use crystalvars
use hh

IMPLICIT NONE

! Namelist structure for input data
NAMELIST /defects/ fname,LPIEZO,IND, &
                   IY,CN17,XIGEE,FAP1,FAP3,ANO,LTEST,LBOD,LPR,LQ, &
                   LB,LD,LB2,LD2,LB3,LD3,LB4,LD4, &
                   BD,B2D,B3D,B4D,LF1,LC1,LF2,LC2,LF3,LC3,LF4,LC4, &
                   QL1,QL2,QL3,QL4,LU,LG,LBM,LFN,THICK,START,FINISH, &
                   LFP1,LFP,LFP3,LS1,LQ1,LS2,LQ2,LS3,LQ3,SEP,SEP2,D1,EP,EA 
! and the namelist data types
real         :: CN17,XIGEE,FAP1,FAP3,ANO, THICK, START, FINISH, SEP, SEP2, D1(6,6), EP(3,6), EA(3,3)
integer      :: LPIEZO, IND, LTEST, LBOD, LPR, LQ, LB(3), LD, LB2(3), LD2, LB3(3), LD3, LB4(3), LD4, &
                LF1(3), LF2(3), LF3(3), LF4(3), LU(3), LG(3), LBM(3), LFN(3), LFP1(3), &
                LFP(3), LFP3(3), LS1(3), LS2(3), LS3(3),  LQ1, LQ2, LQ3
character(15) :: fname
! other variables for the main program

integer      :: ICNT, LLQ, NNN, NNNN, I, J, JB, JC, K, L, KMIN, KMAX, KTOT, MOVE, LUCK, ISTORE, LSWITC, &
                IFLAG, JT, JM, KOUNTF, INDL, KK, JZ

real         :: GINB, RR(3), GINBXU, Z, SBM1, SBM2, SBM3, SFN1, SFN2, SFN3, FNBM, PT, SL, PT2, SL2, &
                THBM, EXT1, EXT2, EXT3, EXT4, EXTRA, FRACTI, DIVISO, DELT, WL, DELW, DELL, BACK, BACKD, &
                STORE, DELTA, DEL, DEL2, DISTR, DISTRA, DISTL, DISTLA, XXX, YYY, ZZZ, VVV, FAULT1, &
                ALPHA, COSA1, SINA1, FAULT2, COSA2, SINA2, FAULT3, COSA3, SINA3, STARTA, SURFAC, XX1, &
                TRAMP3, TRAMP7,DNR, DNI, DNN, TTB, TTD, WW

!***********************************************************************
!*    Read all program data from the namelist input file fort.10       *
!***********************************************************************
 READ(UNIT=10,NML=defects) 
! copy namelist entries to appropriate variables
 MRD%CN(17)=CN17
 MKAP%D1 = D1; MKAP%EP = EP; MKAP%EA = EA;
 MT%LU = LU; MT%LG = LG; MT%LBM = LBM; MT%LFN = LFN; 
 MT%LB = LB ; MT%LB2 = LB2; MT%LB3 = LB3; MT%LB4 = LB4;
 MT%LFP1 = LFP1; MT%LFP = LFP; MT%LFP3 = LFP3; 
 MT%LS1 = LS1; MT%LS2 = LS2; MT%LS3 = LS3;
 MT%LF1 = LF1; MT%LF2 = LF2; MT%LF3 = LF3; MT%LF4 = LF4;
 SCALE30%LTEST = LTEST
! open file for output
 ICNT=1
 OPEN(6,FILE='HH.DAT', Status='UNKNOWN')
 OPEN(3,FILE='hh.ima', Status='UNKNOWN', Form='UNFORMATTED')
!*********************************************************************
!*      Select the crystal system                                    *
!*      for trigonal we can use both trigonal and hexagonal indices  *
!*      (0=trigonal) (1=hexagonal)                                   *
!
! the hh.f program calls an individual routine for each crystal system
! and essentially computes the direct and reciprocal structure matrices.
! In terms of the variables defined for the CTEM book, the following
! relations hold:
!   hh.f        CTEM source code
!   AT          cell%dsm/cell%a
!   ATR         cell%rsm*cell%a
!
! We will stick to the hh.f variable names for easy translation.
! The routine CrystalData does everything that was originally done
! by the following hh.f subroutines:  TRICLIN, MONOCLI, RHOMBIS,
! TRIGONA, TETRAGONA, HEXAGON, and CUBIC.
!*********************************************************************
 call CrystalData(fname)
 MKT%AT  = cell%dsm/cell%a
 MKT%ATR = cell%rsm*cell%a
!*********************************************************************
! transform all crystal variables to Cartesian frame (former TRAFO routine)
!*********************************************************************
 MT%TLU =   matmul(MKT%AT,MT%LU)
 MT%TLF1 =  matmul(MKT%AT,MT%LF1)
 MT%TLF2 =  matmul(MKT%AT,MT%LF2)
 MT%TLF3 =  matmul(MKT%AT,MT%LF3)
 MT%TLF4 =  matmul(MKT%AT,MT%LF4)
 MT%TLG =   matmul(MKT%ATR,MT%LG)
 MT%TLBM =  matmul(MKT%AT,MT%LBM)
 MT%TLFN =  matmul(MKT%AT,MT%LFN)
 MT%TLB  =  matmul(MKT%AT,MT%LB)
 MT%TLB2 =  matmul(MKT%AT,MT%LB2)
 MT%TLB3 =  matmul(MKT%AT,MT%LB3)
 MT%TLB4 =  matmul(MKT%AT,MT%LB4)
 MT%TLFP =  matmul(MKT%ATR,MT%LFP)
 MT%TLFP1 = matmul(MKT%ATR,MT%LFP1)
 MT%TLFP3 = matmul(MKT%ATR,MT%LFP3)
 MT%TLS1 =  matmul(MKT%AT,MT%LS1)
 MT%TLS2 =  matmul(MKT%AT,MT%LS2)
 MT%TLS3 =  matmul(MKT%AT,MT%LS3)
!***********************************************************
!*      Computation of  G.B   and  G.(B X U)               *
!***********************************************************
 GINB=CalcDot(MT%TLG,MT%TLB,'c')/FLOAT(LD) 
 call CalcCross(MT%TLB,MT%TLU,RR,'c','c',0)
 GINBXU=CalcDot(MT%TLG,RR,'c')/FLOAT(LD)
!***********************************************************************
!*       Make sure that the input data make geometric sense            *
!*       and initialize some default values; a lot of this was         *
!*       typical spaghetti-code....                                    *
!***********************************************************************
 MRD%ANO = -MRD%ANO
 if (MRD%ANO.eq.0.0) MRD%ANO=0.1
 if ((FINISH.eq.0.0).AND.(START.eq.0.0)) then 
  START=0.0 
  FINISH=THICK
 end if
 if (FINISH.le.START) then
   STOP 'START after FINISH'
 end if
 MRD%CN(18)=2.0*MRD%CN(17) 
 if (LD.eq.0)  LD=1
 if (LD2.eq.0)  LD2=1
 if (LD3.eq.0)  LD3=1
 if (LD4.eq.0)  LD4=1
 if (LQ1.eq.0)  LQ1=1
 if (LQ2.eq.0)  LQ2=1
 if (LQ3.eq.0)  LQ3=1
 if (LBOD.eq.0)  LBOD=1
!***************************************
! At this point the old program allowed the user to select between a 
! slow or fast run;  this is no longer needed, but the variables that 
! were set by the old lines are still initialized
!***************************************
 LQ=2
 LLQ=1 
 NNN=ICOL
 NNNN=30+10*LQ 
!***********************************************************************
!***     Default foil normal LFN = LBM                               ***
!***********************************************************************
 if (sum(MT%TLFN**2).eq.0.0) then 
  MT%TLFN=MT%TLBM
 end if
!***********************************************************************
!***     Make sure that FP1 and FP3 are identical                    ***
!***********************************************************************
 if (sum(MT%TLFP3**2).eq.0.0) then 
  MT%TLFP3=MT%TLFP1 
 else
  if ((((MT%TLFP3(1)-MT%TLFP1(1)).eq.0.0).or.((MT%TLFP3(2)-MT%TLFP1(2)).eq.0.0)).or.((MT%TLFP3(3)-MT%TLFP1(3)).eq.0.0)) then
    mess = 'Fault planes 1 and 3 not identical in input; identity 3=1 imposed'; call Message("(A)")
    MT%TLFP3=MT%TLFP1 
  end if
 end if
!***********************************************************************
!*       Create and normalize the dislocation reference frame (DC)     *
!***********************************************************************
 MAP%DC(3,1:3)=MT%TLU(1:3)
 do J=1,3 
  K=NP(J) 
  L=NQ(J) 
  MAP%DC(1,J)=MT%TLBM(K)*MT%TLU(L)-MT%TLBM(L)*MT%TLU(K) 
 end do
 do J=1,3 
  K=NP(J) 
  L=NQ(J) 
  MAP%DC(2,J)=MT%TLU(K)*MAP%DC(1,L)-MT%TLU(L)*MAP%DC(1,K) 
 end do
 do J=1,3 
  DD(J)=0.0 
  do K=1,3 
   DD(J)=DD(J)+MAP%DC(J,K)**2
  end do
  if ((DD(J)-0.0001).le.0.0) then
   STOP ' Beam parallel to line direction '
  end if
 end do
 if ((sum(MT%LU*MT%LFP1)**2+sum(MT%LU*MT%LFP)**2+sum(MT%LU*MT%LFP3)**2).ne.0.0) then
  STOP ' Line direction not in fault planes '
 end if
 do J=1,3 
  MAP%DC(J,1:3)=MAP%DC(J,1:3)/SQRT(DD(J)) 
 end do
!********************************************************************** 
!***   Create and normalize the reference frame attached to the    **** 
!***   incident beam direction (DCX)                               **** 
!********************************************************************** 
 do J=1,3 
  DCX(1,J)=-MAP%DC(1,J) 
  DCX(2,J)=-MT%TLBM(J) 
 end do
 DCX(3,1)=DCX(1,2)*DCX(2,3)-DCX(2,2)*DCX(1,3)
 DCX(3,2)=DCX(2,1)*DCX(1,3)-DCX(1,1)*DCX(2,3)
 DCX(3,3)=DCX(1,1)*DCX(2,2)-DCX(2,1)*DCX(1,2)
 do J=1,3 
  DD(J)=sum(DCX(J,1:3)**2) 
 end do
 do J=1,3 
  DCX(J,1:3)=DCX(J,1:3)/SQRT(DD(J)) 
 end do
! 
!********************************************************************** 
!***   Transformation of input data from crystal reference frame   **** 
!***   to DC and DCX reference frames                              **** 
!********************************************************************** 
! 
 BD=0.0; QL1=0.0; QL2=0.0; QL3=0.0; QL4=0.0
 GD=0.0 ; BM=0.0 ; FN=0.0 ; FNX=0.0; B2D=0.0
 B3D=0.0; B4D=0.0; FPX=0.0; UX=0.0 ; FP1X=0.0 
 FP3X=0.0 ; FP=0.0 ; FP3=0.0
 do J=1,3 
  BD(J)=BD(J)+sum((MT%TLB(1:3))*MAP%DC(J,1:3))/FLOAT(LD)
  QL1(J)=QL1(J)+sum(MT%TLF1(1:3)*MAP%DC(J,1:3))*LC1 
  QL2(J)=QL2(J)+sum(MT%TLF2(1:3)*MAP%DC(J,1:3))*LC2 
  QL3(J)=QL3(J)+sum(MT%TLF3(1:3)*MAP%DC(J,1:3))*LC3 
  QL4(J)=QL4(J)+sum(MT%TLF4(1:3)*MAP%DC(J,1:3))*LC4 
  BM(J)=BM(J)+sum(MT%TLBM(1:3)*MAP%DC(J,1:3))
  FN(J)=FN(J)+sum(MT%TLFN(1:3)*MAP%DC(J,1:3))
  FNX(J)=FNX(J)+sum(MT%TLFN(1:3)*DCX(J,1:3))
  FPX(J)=FPX(J)+sum(MT%TLFP(1:3)*DCX(J,1:3))
  FP(J)=FP(J)+sum(MT%TLFP(1:3)*MAP%DC(J,1:3))
  FP3(J)=FP3(J)+sum(MT%TLFP3(1:3)*MAP%DC(J,1:3))
  FP1X(J)=FP1X(J)+sum(MT%TLFP1(1:3)*DCX(J,1:3)) 
  FP3X(J)=FP3X(J)+sum(MT%TLFP3(1:3)*DCX(J,1:3)) 
  B2D(J)=B2D(J)+sum(MT%TLB2(1:3)*MAP%DC(J,1:3))/FLOAT(LD2)
  B3D(J)=B3D(J)+sum(MT%TLB3(1:3)*MAP%DC(J,1:3))/FLOAT(LD3)
  B4D(J)=B4D(J)+sum(MT%TLB4(1:3)*MAP%DC(J,1:3))/FLOAT(LD4)
  UX(J)=UX(J)+sum(MT%TLU(1:3)*DCX(J,1:3)) 
  GD(J)=GD(J)+sum(MT%TLG(1:3)*MAP%DC(J,1:3))
 end do
!
 Z=SQRT(sum(FN**2))
 FN = FN/Z
 Z=SQRT(sum(BM(2:3)**2))
 BM(2:3)=BM(2:3)/Z 
 Z=sum(MT%TLBM**2)
 Z=SQRT(Z) 
 SBM1=MT%TLBM(1)/Z
 SBM2=MT%TLBM(2)/Z
 SBM3=MT%TLBM(3)/Z
 Z=sum(MT%TLFN**2)
 Z=SQRT(Z) 
 SFN1=MT%TLFN(1)/Z
 SFN2=MT%TLFN(2)/Z
 SFN3=MT%TLFN(3)/Z
!
 if (FN(3).le.0.0) then
  STOP ' Line direction parallel to surface '
 end if
 FNBM=SFN1*SBM1+SFN2*SBM2+SFN3*SBM3
 if (FNBM.le.0.0) then 
  STOP ' Foil normal and beam not acute '
 end if 
!***********************************************************************
!*       Computation of image size and positions of dislocations       *
!***********************************************************************
 Z=SQRT(FP(1)**2+FP(2)**2) 
 if(Z.eq.0.0) Z=1. 
 if(SEP.ne.0.0) then
  mess = ' FAULT PLANE 2 ZERO WITH SEP NONZERO'; call Message("(A)")
 end if
 AB(1)=FP(2)/Z 
 AB(2)=-FP(1)/Z
 PT=SEP*AB(1)
 SL=SEP*AB(2)/BM(2)
 Z=SQRT(FP3(1)**2+FP3(2)**2) 
 if(Z.eq.0.0) Z=1. 
 AB1(1)=FP3(2)/Z 
 AB1(2)=-FP3(1)/Z
 PT2=SEP2*AB1(1) 
 SL2=-SEP2*AB1(2)/BM(2)
 if (LPIEZO.eq.0) then 
  call ANCALC 
  if (MAPN%KRASH.eq.0) then
   do I=1,3
    write(6,*) (MA%AR(I,J),MA%AI(I,J),J=1,3)
   end do
   write(6,*) (GD(I),I=1,3)
   do I=1,3
    write(6,*) MA%PR(I),MA%PI(I)
   end do
   do I=1,3
    write(6,*) (MA%EMR(I,J),MA%EMI(I,J),J=1,3)
   end do
   write(6,"('  H  ',3F20.10)") ((MA%H(I,J),J=1,3),I=1,3)
   do JB=1,3 
    SUR(JB)=0.0
    SUI(JB)=0.0
    do K=1,3
     SUR(JB)=SUR(JB)+GD(K)*MA%AR(K,JB) 
     SUI(JB)=SUI(JB)+GD(K)*MA%AI(K,JB) 
    end do
   end do
   do JB=1,3 
    DR(JB)=SUR(JB)*MA%PR(JB)-SUI(JB)*MA%PI(JB) 
    DI(JB)=SUR(JB)*MA%PI(JB)+SUI(JB)*MA%PR(JB) 
   end do
   do JB=1,3 
    do L=1,3
     UR(JB,L)=0.0 
     UI(JB,L)=0.0 
     do J=1,3
      UR(JB,L)=UR(JB,L)+MA%EMR(JB,J)*MA%H(J,L) 
      UI(JB,L)=UI(JB,L)+MA%EMI(JB,J)*MA%H(J,L) 
     end do
    end do 
   end do 
   do JB=1,3 
    do L=1,3
     VR(JB,L)=DR(JB)*UR(JB,L)-DI(JB)*UI(JB,L) 
     VI(JB,L)=DR(JB)*UI(JB,L)+DI(JB)*UR(JB,L) 
    end do
   end do
   do JB=1,3 
    do L=1,3
     UI(JB,L)=VR(JB,L)*MA%PR(JB)+VI(JB,L)*MA%PI(JB) 
    end do
   end do
!***********************************************************
!*            First Dislocation                            *
!***********************************************************
   do J=1,3
    MRD%CN(J+8)=MA%PR(J)
    MRD%CN(J+12)=MA%PI(J)**2
    MRD%CN(J)=0.0
    MRD%CN(J+4)=0.0
    do L=1,3
     MRD%CN(J)=MRD%CN(J)+VR(J,L)*BD(L)
     MRD%CN(J+4)=MRD%CN(J+4)+UI(J,L)*BD(L)
    end do
   end do
   write(6,*) (MRD%CN(I),I=1,16)
!***********************************************************
!*        Second Dislocation                               *
!***********************************************************
   do J=1,3
    MRD%CN(J+21)=0.0 
    MRD%CN(J+25)=0.0 
    do L=1,3
     MRD%CN(J+21)=MRD%CN(J+21)+VR(J,L)*B2D(L) 
     MRD%CN(J+25)=MRD%CN(J+25)+UI(J,L)*B2D(L) 
    end do
   end do
!***********************************************************
!*       Third Dislocation                                 *
!***********************************************************
   do J=1,3
    MRD%CN(J+31)=0.0 
    MRD%CN(J+35)=0.0 
    do L=1,3
     MRD%CN(J+31)=MRD%CN(J+31)+VR(J,L)*B3D(L) 
     MRD%CN(J+35)=MRD%CN(J+35)+UI(J,L)*B3D(L) 
    end do
   end do
!***********************************************************
!*       Fourth Dislocation                                *
!***********************************************************
   do J=1,3
    MRD%CN(J+50)=0.0 
    MRD%CN(J+54)=0.0 
    do L=1,3
     MRD%CN(J+50)=MRD%CN(J+50)+VR(J,L)*B4D(L) 
     MRD%CN(J+54)=MRD%CN(J+54)+UI(J,L)*B4D(L) 
    end do
   end do 
   MRD%CN(4)=0.0
   MRD%CN(8)=0.0
   MRD%CN(12)=0.0 
   MRD%CN(16)=0.0 
   MRD%CN(25)=0.0 
   MRD%CN(29)=0.0 
   MRD%CN(35)=0.0 
   MRD%CN(39)=0.0 
   MRD%CN(54)=0.0 
   MRD%CN(58)=0.0 
   write(6,"(' H-ANCALC ',4F10.4)") ((MA%H(I,J),J=1,4),I=1,4) 
  else
   STOP 
  end if 
 else
  call PANCALC
  if (MAPN%KRASH.eq.0) then
   do K=1,4
    SU(K)=CMPLX(0.0,0.0)
    do I=1,3
     SU(K)=SU(K)+GD(I)*MP%AS(I,K) 
    end do
   end do
   do K=1,4
    SU(K)=SU(K)*MP%PC(K) 
   end do
!*******************************************************************
!   First Dislocation                                              *
!*******************************************************************
   do J=1,4 
    MRD%CN(J+8)=REAL(MP%PC(J)) 
    MRD%CN(J+12)=AIMAG(MP%PC(J))**2
    CNX(J)=CMPLX(0.0,0.0) 
    do L=1,4
     CNX(J)=CNX(J)+(MP%EL(L,J)*BD(L)-MP%AS(L,J)*QL1(L))
    end do
    MRD%CN(J)=AIMAG(CNX(J)*SU(J))*2.0 
    MRD%CN(J+4)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0
   end do
!*******************************************************************
!   Second Dislocation                                             *
!*******************************************************************
   do J=1,4
    CNX(J)=CMPLX(0.0,0.0) 
    do L=1,4
     CNX(J)=CNX(J)+(MP%EL(L,J)*B2D(L)-MP%AS(L,J)*QL2(L)) 
    end do
    MRD%CN(J+21)=AIMAG(CNX(J)*SU(J))*2.0
    MRD%CN(J+25)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0 
   end do
!*******************************************************************
!   Third Dislocation                                                *
!****************************************************************** 
   do J=1,4
    CNX(J)=CMPLX(0.0,0.0) 
    do L=1,4
     CNX(J)=CNX(J)+(MP%EL(L,J)*B3D(L)-MP%AS(L,J)*QL3(L)) 
    end do
    MRD%CN(J+31)=AIMAG(CNX(J)*SU(J))*2.0
    MRD%CN(J+35)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0 
   end do
!*******************************************************************
!   Fourth Dislocation                                               *
!*******************************************************************
   do J=1,4
    CNX(J)=CMPLX(0.0,0.0) 
    do L=1,4
     CNX(J)=CNX(J)+(MP%EL(L,J)*B4D(L)-MP%AS(L,J)*QL4(L)) 
    end do
    MRD%CN(J+50)=AIMAG(CNX(J)*SU(J))*2.0
    MRD%CN(J+54)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0 
   end do
!*********************************************************************
!*    BERECHNEN DER MATRIZEN   H, S, Q   UND AUSDRUCKEN              *
!*       SIEHE DEFINITION NACH                                       *
!*                - BARNETT UND LOTHE -                              *
!*            PHYS.STAT.SOL(B) 67,105(1975)                          *
!*********************************************************************
!
   do I=1,4
    do J=1,4
     MXXX(I,J)=CMPLX(0.0,0.0)
     MYYY(I,J)=CMPLX(0.0,0.0)
     MZZZ(I,J)=CMPLX(0.0,0.0)
     do K=1,4
      MXXX(I,J)=MXXX(I,J)+MP%EL(I,K)*MP%EL(J,K) 
      MYYY(I,J)=MYYY(I,J)+MP%AS(I,K)*MP%EL(J,K) 
      MZZZ(I,J)=MZZZ(I,J)+MP%AS(I,K)*MP%AS(J,K) 
     end do
     MA%H(I,J)=-(1.0/(2.0*cPi))*AIMAG(MXXX(I,J)) 
     S(I,J)=-2.0*AIMAG(MYYY(I,J))
     QS(I,J)=-2.0*AIMAG(MZZZ(I,J)) 
    end do
   end do
   write(6,"(' ',4F10.4,' H- MATRIX')") ((MA%H(K,L),L=1,4),K=1,4) 
   write(6,"(' ',4F10.4,' S- MATRIX')") ((S(K,L),L=1,4),K=1,4)
   write(6,"(' ',4F10.4,' Q- MATRIX')") ((QS(K,L),L=1,4),K=1,4) 
  else 
   STOP
  end if
 end if 
 THBM=THICK/FNBM 
 KMIN=999
 KMAX=0
 KTOT=0
! 
!********************************************************************** 
!  BESTIMMUNG DER BILDLAENGE                                          * 
!********************************************************************** 
! 
 EXT1=ABS(SL+PT*FNX(1)/FNX(2)) 
 EXT2=ABS(SL2-PT2*FNX(1)/FNX(2)) 
 EXT3=ABS((SL2-PT2*FNX(1)/FNX(2))-(SL+PT*FNX(1)/FNX(2))) 
 if (SL.gt.0.0) then
  EXT4=((SL+PT*FNX(1)/FNX(2))+2*(SL2-PT2*FNX(1)/FNX(2))-(SL+PT*FNX(1)/FNX(2))) 
 else
  EXT4=EXT1+2*EXT2
 end if
 EXTRA=AMAX1(EXT1,EXT2,EXT3,EXT4)
 FRACTI=(FINISH-START)/THICK 
 DIVISO=BM(3)/BM(2)-FNX(3)/FNX(2)
 DELT=cPi*FRACTI*(THBM+EXTRA)/FLOAT(NNN)
 WL=((THICK*BM(2)/FN(3))+EXTRA/DIVISO)*FRACTI
 DELW=0.7650*cPi*WL/FLOAT(IROW-1)
 DELL=DELW/2.0+0.00000001
 MRD%CN(20)=cPi*PT/2.0
 MRD%CN(21)=cPi*SL/2.0
 MRD%CN(31)=MRD%CN(20)/BM(2) 
 MRD%CN(40)=cPi*PT2 
 MRD%CN(41)=cPi*SL2 
 MRD%CN(42)=MRD%CN(20)+MRD%CN(40)
 MRD%CN(43)=MRD%CN(21)-MRD%CN(41)
 MRD%CN(44)=MRD%CN(42)/BM(2) 
! 
!********************************************************************** 
!*       AUSDRUCKEN DER KONSTANTEN, KOORDINATEN UND DER GRAU-          *
!*       SKALA  (PARAMETER "LTEST"=1)                         * 
!********************************************************************** 
! 
 if (SCALE30%LTEST.eq.1) then
  write (6,"(1H1,16H DC-KOORDINATEN )") 
  write (6,"(1X,3F12.8)") ((MAP%DC(I,J),J=1,3),I=1,3) 
  write (6,"(1H ,17H DCX-KOORDINATEN )") 
  write (6,"(1X,3F12.8)") ((DCX(I,J),J=1,3),I=1,3)
  write (6,"(1H ,48H KOORDINATEN DER BURGERSVEKTOREN UND FAULTPLANES)") 
  write (6,"(1H0,3F12.8,'  BD',3F12.8,' B2D',3F12.8,' B3D'/1H ,3F12.8,'  BM',3F12.8,'  GD',3F12.8,'  FN'/1H ,3F12.8,' FNX',3F12.8,'FP1X',3F12.8,' FPX'/1H ,3F12.8,'FP3X',3F12.8,'  FP',3F12.8,' FP3')") BD,B2D,B3D,BM,GD,FN,FNX,FP1X,FPX,FP3X,FP,FP3
  write (6,"(1H0,26H ERSTER SEPARATIONSVEKTOR )") 
  write (6,"(1H ,3F12.8,3H AB,F12.8,3H SL,F12.8,3H PT,F12.8,5H DELT)") AB,SL,PT,DELT 
  write (6,"(1H0,27H ZWEITER SEPARATIONSVEKTOR )") 
  write (6,"(1H ,3F12.8,3HAB1,F12.8,3HPT2/1H ,F12.8,4HEXT1,1F12.8,4HEXT2,F12.8,4HEXT3,F12.8,5HEXTRA)") AB1,SL2,PT2,EXT1,EXT2,EXT3,EXTRA
  write (6,"(1H ,15H CN-KONSTANTEN )") 
  write (6,"(1H ,4F12.8)") (MRD%CN(J),J=1,16)
  write(6,"(' ',4F12.6)") (MRD%CN(J+21),J=1,8)
  write(6,"(' ',4F12.6)") (MRD%CN(J+31),J=1,8)
  write(6,"(' ',4F12.6)") (MRD%CN(J+50),J=1,8)
 end if
 MRD%CN(30)=1000.0 
 MRD%SKIP=0.0
 MRD%X=0.0 
 MRD%Q=0.0 
 MRD%ERROR=0.0001
! 
!********************************************************************** 
!*       BERECHNUNG DER UNTERGRUNDINTENSITAET DURCH INTEGRATION        *
!*       UEBER DIE GESAMMTE DICKE DES UNGESTOERTEN KRISTALLS           *
!********************************************************************** 
! 
 MRD%Y(1:8)=0.0 
 MRD%Y(1)=1.0
 MRD%Y(7)=1.0
 MRD%X1=THBM*cPi/FLOAT(ICOL/2) 
 CALL RKM
 MRD%X1=cPi*THBM
 CALL RKM
 BACK=1.0
 BACKD=1.0
!  This is a very long do-loop;  could be rewritten with function
!  and subroutine calls...
 do JC=1,IROW
  MRD%CN(19)=(FLOAT(JC)-FLOAT(IROW/2)-0.5)*DELW
  MOVE=0
  COORD(1)=-MRD%CN(42)
  COORD(2)=-MRD%CN(20)
  COORD(3)=+MRD%CN(20)
  COORD(4)=+MRD%CN(42) 
! 
!***************************************************************************
!*          ETWAIGE KORREKTUR DER VERSETZUNGSREIHENFOLGE 1-4 VON LINKS     * 
!*          NACH RECHTS. UMSPEICHERN DER KOORDINATEN                       *
!***************************************************************************
! 
  if ((COORD(2)-COORD(3)).ne.0.0) then
   STORE=COORD(3)
   COORD(3)=COORD(2) 
   COORD(2)=STORE
   if ((COORD(3)-COORD(4)).gt.0.0) then  
    STORE=COORD(4)
    COORD(4)=COORD(3) 
    COORD(3)=STORE
    STORE=COORD(2)
    COORD(2)=COORD(1) 
    COORD(1)=STORE
    if ((COORD(2)-COORD(3)).gt.0.0) then
     STORE=COORD(3)
     COORD(3)=COORD(2) 
     COORD(2)=STORE
    end if
   end if
  end if
!***************************************************************************
!*        DEFINITION DER SCHUTZZONE = 5 A                                  *
!***************************************************************************
  DELTA=5.0 
  DEL=DELTA*cPi/XIGEE
  DEL2=2.0*DEL
  do J=1,4
   HANDR(J)=COORD(J)-DEL 
   HANDL(J)=COORD(J)+DEL 
  end do
  if ((HANDL(1)-HANDR(2)).ge.0.0) then 
   if ((HANDL(2)-HANDR(3)).ge.0.0) then 
!*Case 1
    HANDL(1)=HANDL(4) 
    K=1 
    L=1 
!*Case 2
   else
    HANDL(1)=HANDL(2) 
    HANDL(3)=HANDL(4) 
    K=3 
    L=2 
   end if 
!*Case 3 
  else
   if ((HANDL(2)-HANDR(3)).ge.0.0) then 
    HANDL(2)=HANDL(3) 
    HANDL(3)=HANDL(4) 
    HANDR(3)=HANDR(4) 
    K=3 
    L=1 
!*Case 4
   else 
    K=4 
    L=1 
   end if
  end if
  do KK=1,K,L 
   DISTR=MRD%CN(19)+HANDL(KK)
   DISTRA=ABS(DISTR) 
   DISTL=-(HANDR(KK)+MRD%CN(19)) 
   DISTLA=ABS(DISTL) 
   if (DISTR.gt.0.0) then 
    if (DISTL.gt.0.0) then
     if ((DISTRA-DEL).gt.0.0) then
      if ((DISTLA-DEL).gt.0.0) then  
       if ((DISTRA-DEL2).gt.0.0) then 
        if ((DISTLA-DEL2).gt.0.0) then 
         mess = ' DISLOCATIONS TOO CLOSE TOGETHER FOR COLUMN TO BE MOVED MEANINGFULLY '; call Message("(A)")
         STOP 
        else
         MRD%CN(15)=-HANDR(KK) 
         MOVE=1
         EXIT  ! the do KK loop
        end if
       else
        MRD%CN(15)=-HANDL(KK) 
        MOVE=1
        EXIT  ! the do KK loop
       end if
      else
       MRD%CN(15)=-HANDR(KK) 
       MOVE=1
       EXIT  ! the do KK loop
      end if
     end if
    end if
   end if
  end do 
! 
!***********************************************************************
!*      BEGRENZUNGEN DER STAPELFEHLER DURCH VERSETZUNGEN               *
!***********************************************************************
  XXX=MRD%CN(19)+MRD%CN(20) 
  YYY=MRD%CN(19)-MRD%CN(20) 
  ZZZ=MRD%CN(19)+MRD%CN(42) 
  VVV=MRD%CN(19)-MRD%CN(42) 
  MRD%CN(30)=MRD%CN(19)/BM(2) 
!***********************************
!*   First Stacking Fault          *
!***********************************
  FAULT1=10000.0
  if (YYY*VVV.le.0.0) then 
   if (sum(MT%TLS1**2).ne.0.0) then 
    ALPHA=cPi*sum(MT%TLG*MT%TLS1)*2.0/FLOAT(LQ1) 
    COSA1=COS(ALPHA)
    SINA1=SIN(ALPHA)
    if (FP1X(2).ne.0.0) then 
     FAULT1=MRD%CN(21)-(MRD%CN(19)-MRD%CN(20))*FP1X(1)/FP1X(2)+FAP1
    end if
   end if
  end if
!*************************************
!*  Second stacking fault          ***
!*************************************
  FAULT2=10001.0
  if (XXX*YYY.lt.0.0) then
   if (sum(MT%TLS2**2).ne.0.0) then
    ALPHA=cPi*sum(MT%TLG*MT%TLS2)*2.0/FLOAT(LQ2) 
    COSA2=COS(ALPHA)
    SINA2=SIN(ALPHA)
    if (FPX(2).ne.0.0) then
     FAULT2=-MRD%CN(19)*FPX(1)/FPX(2)
    end if
   end if
  end if

!*************************************
!*  Third stacking fault           ***
!*************************************
  FAULT3=10002.0
  if (XXX.lt.0.0) then
   if (ZZZ.ge.0.0) then
    if (sum(MT%TLS3**2).ne.0.0) then 
     ALPHA=cPi*sum(MT%TLG*MT%TLS3)*2.0/FLOAT(LQ3)
     COSA3=COS(ALPHA)
     SINA3=SIN(ALPHA)
     if (FP3X(2).ne.0.0) then
      FAULT3=-MRD%CN(21)-(MRD%CN(19)+MRD%CN(20))*FP3X(1)/FP3X(2)+FAP3 
     end if
    end if 
   end if 
  end if
!
  STARTA=cPi*(EXTRA/2.0-(THBM+EXTRA)*FINISH/THICK)-(MRD%CN(19)*FNX(1)/FNX(2))
  SURFAC=STARTA+cPi*THBM 
  POSA(1)=FAULT1
  ITYPE(1)=1
  POSA(2)=FAULT2
  ITYPE(2)=2
  POSA(3)=FAULT3
  ITYPE(3)=3
  POSA(4)=SURFAC
  ITYPE(4)=4
  do J=1,3
   LUCK=0
   do K=1,3
    if ((POSA(K)-POSA(K+1)).gt.0.0) then 
     STORE=POSA(K+1) 
     POSA(K+1)=POSA(K) 
     POSA(K)=STORE 
     ISTORE=ITYPE(K+1) 
     ITYPE(K+1)=ITYPE(K) 
     ITYPE(K)=ISTORE 
     LUCK=1
    end if
   end do
   if (LUCK.eq.0) EXIT 
  end do
  LSWITC=0
  do J=1,4
   if ((ITYPE(J)-4).ne.0) then 
    if (LSWITC.ne.0) then
     POSB(J)=POSA(J) 
     EXIT
    else
     POSB(J)=-10050.0+FLOAT(J) 
    end if 
   else
    LSWITC=1
    POSB(J)=-10050.0+FLOAT(J) 
   end if
  end do
  IFLAG=0 
  MRD%X=STARTA
  MRD%X1=MRD%X+DELT 
  MRD%Y(1:8)=0.0 
  MRD%Y(1)=1.0
  MRD%Y(7)=1.0
  do JT=1,NNN 
   KOUNTF=1
   if ((JT-1).ne.0) then 
    MRD%X1=MRD%X1+DELT
   end if
 ifkount: do
    if ((KOUNTF-5).eq.0) EXIT ifkount
    if ((POSA(KOUNTF)-MRD%X).lt.0.0) then 
     KOUNTF=KOUNTF+1 
     CYCLE ifkount
    else
     if ((MRD%X1-POSA(KOUNTF)).ge.0.0) then 
      XX1=MRD%X1
      I=ITYPE(KOUNTF) 
      MRD%X1=POSA(KOUNTF) 
      call RKM
      KTOT=KTOT+MRD%KOUNT 
      select case(I)
      case(1); TRAMP3=MRD%Y(3) 
               TRAMP7=MRD%Y(7) 
               MRD%Y(3)=MRD%Y(3)*COSA1-MRD%Y(4)*SINA1
               MRD%Y(7)=MRD%Y(7)*COSA1-MRD%Y(8)*SINA1
               MRD%Y(4)=MRD%Y(4)*COSA1+TRAMP3*SINA1
               MRD%Y(8)=MRD%Y(8)*COSA1+TRAMP7*SINA1
               MRD%X1=XX1
               POSA(KOUNTF)=-9001. 
               KOUNTF=KOUNTF+1 
      case(2); TRAMP3=MRD%Y(3) 
               TRAMP7=MRD%Y(7) 
               MRD%Y(3)=MRD%Y(3)*COSA2-MRD%Y(4)*SINA2
               MRD%Y(7)=MRD%Y(7)*COSA2-MRD%Y(8)*SINA2
               MRD%Y(4)=MRD%Y(4)*COSA2+TRAMP3*SINA2
               MRD%Y(8)=MRD%Y(8)*COSA2+TRAMP7*SINA2
               MRD%X1=XX1
               POSA(KOUNTF)=-9000. 
               KOUNTF=KOUNTF+1 
      case(3); TRAMP3=MRD%Y(3) 
               TRAMP7=MRD%Y(7) 
               MRD%Y(3)=MRD%Y(3)*COSA3-MRD%Y(4)*SINA3
               MRD%Y(7)=MRD%Y(7)*COSA3-MRD%Y(8)*SINA3
               MRD%Y(4)=MRD%Y(4)*COSA3+TRAMP3*SINA3
               MRD%Y(8)=MRD%Y(8)*COSA3+TRAMP7*SINA3
               MRD%X1=XX1
               POSA(KOUNTF)=-9002. 
               KOUNTF=KOUNTF+1 
      case(4); TEMPY(1:8)=MRD%Y(1:8) 
               MRD%X1=XX1
               POSA(KOUNTF)=-9003. 
               KOUNTF=KOUNTF+1 
               IFLAG=1 
      end select
     else 
      EXIT ifkount
     end if
    end if
   end do ifkount
!
   call RKM
   KTOT=KTOT+MRD%KOUNT 
   DNR=MRD%Y(1)*MRD%Y(7)-MRD%Y(2)*MRD%Y(8)-MRD%Y(3)*MRD%Y(5)+MRD%Y(4)*MRD%Y(6) 
   DNI=MRD%Y(1)*MRD%Y(8)+MRD%Y(2)*MRD%Y(7)-MRD%Y(3)*MRD%Y(6)-MRD%Y(4)*MRD%Y(5) 
   DNN=1.0/(DNR**2+DNI**2) 
   FX(JT,1)=DNN*(MRD%Y(7)*DNR+MRD%Y(8)*DNI)
   FX(JT,2)=DNN*(MRD%Y(8)*DNR-MRD%Y(7)*DNI)
   FX(JT,3)=-DNN*(MRD%Y(3)*DNR+MRD%Y(4)*DNI) 
   FX(JT,4)=DNN*(MRD%Y(3)*DNI-MRD%Y(4)*DNR)
  end do
  if (IFLAG.eq.0) then 
    MRD%X1=SURFAC 
    KOUNTF=1
 ifkount2:  do
     if ((KOUNTF-5).eq.0) EXIT ifkount2
     if ((POSA(KOUNTF)-MRD%X).lt.0.0) then 
      KOUNTF=KOUNTF+1 
      CYCLE ifkount2
     else
      if ((MRD%X1-POSA(KOUNTF)).ge.0.0) then 
       XX1=MRD%X1
       I=ITYPE(KOUNTF) 
       MRD%X1=POSA(KOUNTF) 
       CALL RKM
       KTOT=KTOT+MRD%KOUNT 
       if (I.eq.4) EXIT ifkount2
       select case(I)
       case(1); TRAMP3=MRD%Y(3) 
                TRAMP7=MRD%Y(7) 
                MRD%Y(3)=MRD%Y(3)*COSA1-MRD%Y(4)*SINA1
                MRD%Y(7)=MRD%Y(7)*COSA1-MRD%Y(8)*SINA1
                MRD%Y(4)=MRD%Y(4)*COSA1+TRAMP3*SINA1
                MRD%Y(8)=MRD%Y(8)*COSA1+TRAMP7*SINA1
                MRD%X1=XX1
                POSA(KOUNTF)=-9050. 
                KOUNTF=KOUNTF+1 
       case(2); TRAMP3=MRD%Y(3) 
                TRAMP7=MRD%Y(7) 
                MRD%Y(3)=MRD%Y(3)*COSA2-MRD%Y(4)*SINA2
                MRD%Y(7)=MRD%Y(7)*COSA2-MRD%Y(8)*SINA2
                MRD%Y(4)=MRD%Y(4)*COSA2+TRAMP3*SINA2
                MRD%Y(8)=MRD%Y(8)*COSA2+TRAMP7*SINA2
                MRD%X1=XX1
                POSA(KOUNTF)=-9051. 
                KOUNTF=KOUNTF+1 
       case(3); TRAMP3=MRD%Y(3) 
                TRAMP7=MRD%Y(7) 
                MRD%Y(3)=MRD%Y(3)*COSA3-MRD%Y(4)*SINA3
                MRD%Y(7)=MRD%Y(7)*COSA3-MRD%Y(8)*SINA3
                MRD%Y(4)=MRD%Y(4)*COSA3+TRAMP3*SINA3
                MRD%Y(8)=MRD%Y(8)*COSA3+TRAMP7*SINA3
                MRD%X1=XX1
                POSA(KOUNTF)=-9052. 
                KOUNTF=KOUNTF+1 
       case default
       end select
       CYCLE ifkount2
      end if
     end if
    end do ifkount2
    if (I.ne.4) then
     CALL RKM
     KTOT=KTOT+MRD%KOUNT 
    end if
    TEMPY(1:8)=MRD%Y(1:8) 
  end if
! 
  MRD%X=SURFAC
  MRD%X1=MRD%X+DELT 
  MRD%Y(1:8)=TEMPY(1:8) 
  do JM=1,NNN 
    KOUNTF=1
    if ((JM-1).ne.0) then 
     MRD%X1=MRD%X1+DELT
    end if
 ifkount3: do
     if ((KOUNTF-5).ne.0) then 
      if ((POSB(KOUNTF)-MRD%X).lt.0.0) then 
       KOUNTF=KOUNTF+1 
       CYCLE ifkount3
      else 
       if ((MRD%X1-POSB(KOUNTF)).lt.0.0) EXIT ifkount3
       XX1=MRD%X1
       I=ITYPE(KOUNTF) 
       MRD%X1=POSB(KOUNTF) 
       CALL RKM
       KTOT=KTOT+MRD%KOUNT 
       if (I.eq.4) then
        write(6,"(1HG/26H1ITYPE(4) IN B INTEGRATION)")
        STOP
       end if
       select case(I)
       case(1); TRAMP3=MRD%Y(3) 
                TRAMP7=MRD%Y(7) 
                MRD%Y(3)=MRD%Y(3)*COSA1-MRD%Y(4)*SINA1
                MRD%Y(7)=MRD%Y(7)*COSA1-MRD%Y(8)*SINA1
                MRD%Y(4)=MRD%Y(4)*COSA1+TRAMP3*SINA1
                MRD%Y(8)=MRD%Y(8)*COSA1+TRAMP7*SINA1
                MRD%X1=XX1
                POSB(KOUNTF)=-8050. 
                KOUNTF=KOUNTF+1 
       case(2); TRAMP3=MRD%Y(3) 
                TRAMP7=MRD%Y(7) 
                MRD%Y(3)=MRD%Y(3)*COSA2-MRD%Y(4)*SINA2
                MRD%Y(7)=MRD%Y(7)*COSA2-MRD%Y(8)*SINA2
                MRD%Y(4)=MRD%Y(4)*COSA2+TRAMP3*SINA2
                MRD%Y(8)=MRD%Y(8)*COSA2+TRAMP7*SINA2
                MRD%X1=XX1
                POSB(KOUNTF)=-8051. 
                KOUNTF=KOUNTF+1 
       case(3); TRAMP3=MRD%Y(3) 
                TRAMP7=MRD%Y(7) 
                MRD%Y(3)=MRD%Y(3)*COSA3-MRD%Y(4)*SINA3
                MRD%Y(7)=MRD%Y(7)*COSA3-MRD%Y(8)*SINA3
                MRD%Y(4)=MRD%Y(4)*COSA3+TRAMP3*SINA3
                MRD%Y(8)=MRD%Y(8)*COSA3+TRAMP7*SINA3
                MRD%X1=XX1
                POSB(KOUNTF)=-8052. 
                KOUNTF=KOUNTF+1 
       end select
       CYCLE ifkount3
      end if
     end if
    end do ifkount3
    call RKM
    KTOT=KTOT+MRD%KOUNT 
    INDL=LLQ*JM+1 
! 
!********************************************************************** 
!***   BERECHNUNG DER 129 INTENSITAETEN UND GLEICHZEITIGES AB-      *** 
!***   SPEICHERN DER UNTERGRUNDINTENSITAETEB (TTD)                  *** 
!********************************************************************** 
! 
    TTB=(FX(JM,1)*MRD%Y(1)-FX(JM,2)*MRD%Y(2)+FX(JM,3)*MRD%Y(5)-FX(JM,4)*MRD%Y(6))**2+ &
        (FX(JM,1)*MRD%Y(2)+FX(JM,2)*MRD%Y(1)+FX(JM,3)*MRD%Y(6)+FX(JM,4)*MRD%Y(5))**2 
    TTD=(FX(JM,1)*MRD%Y(3)-FX(JM,2)*MRD%Y(4)+FX(JM,3)*MRD%Y(7)-FX(JM,4)*MRD%Y(8))**2+ &
        (FX(JM,1)*MRD%Y(4)+FX(JM,2)*MRD%Y(3)+FX(JM,3)*MRD%Y(8)+FX(JM,4)*MRD%Y(7))**2 
    TQB(INDL)=TTB
    TQD(INDL)=TTD
  end do  ! ???
  TQB(1)=(TEMPY(1)**2+TEMPY(2)**2)
  TQD(1)=(TEMPY(3)**2+TEMPY(4)**2)

  if (LQ.eq.0) then
   do JZ=2,ICOL,2 
      TQB(JZ)=0.5*(TQB(JZ-1)+TQB(JZ+1))
      TQD(JZ)=0.5*(TQD(JZ-1)+TQD(JZ+1))
   end do
  end if
  do J=1,3
   if ((ABS(MRD%CN(19)+COORD(J))-DELL).le.0.0) then 
   end if
  end do
! 
!  AUSDRUCK EINER BILDZEILE 
! 
  do j=1,ICOL
   BFINTENS(j,ICNT)=TQB(j)
   DFINTENS(j,ICNT)=TQD(j)
  end do
  ICNT=ICNT+1
 end do  ! from several pages back !!!
 WW=79.0*DELW/cPi 
! 
!*******************************************************
!*       AUSDRUCK DER BILDLEGENDE                *******
!*******************************************************
!
 
 write(6,"('    HH.f90  ',A15,' TWO-BEAM ',F6.2,' WL',F6.2,' WW', &
         F5.2,' STR ',F5.2,' FIN ',F7.3,'TH',F7.3,'THBM')") IY,WL,WW,START,FINISH,THICK,THBM 

 write(6,"(' ',3I2,'/',I1,'B   ',F8.4,' Q1 ',3I2,'U    ',3I2,  &
         'G    ',3I2,'BM   ',3I2,'FN',F7.3,'W',F9.3,'BACK') ") LB,LD,QL1(4),LU,LG,LBM,LFN,MRD%CN(17),BACK

 write(6,"(' ',3I2,'/',I1,'B2  ',F8.4,' Q2 ',F5.2,'SEP  ',3I2, &
         'FP1  ',3I2,'FP2  ',3I2,'FP3  ',3I2,'/',I1,'SH1  ',3I2,'/',I1, &
         'SH2  ',3I2,'/',I1,'SH3  ',' 4VS-SYM ',I2,' NNNN')") LB2,LD2,QL2(4),SEP,LFP1,LFP,LFP3,LS1,LQ1,LS2,LQ2,LS3,LQ3,NNNN

 write(6,"(' ',3I2,'/',I1,'B3  ',F8.4,' Q3 ',F5.2,'SEP2  ',F5.2, &
         ' FAP1 ',F5.2,' FAP3 ',F8.1,' XIGEE ',F4.1,' DELTA ',F8.5, &
          ' ANO ')") LB3,LD3,QL3(4),SEP2,FAP1,FAP3,XIGEE,DELTA,ANO

 write(6,"(' ',3I2,'/',I1,'B4  ',F8.4,' Q4 ',' PIEZO=',I1,' IND=',I1)") LB4,LD4,QL4(4),LPIEZO,IND 

 write(6,"('  G.B=',F10.4,'  G.(B X U)=',F10.4)") GINB,GINBXU
!
! create a tiff file with the calculated images
 write (3) BFINTENS,DFINTENS
!
 CLOSE (UNIT=3)
 CLOSE (UNIT=6)
end program
 
