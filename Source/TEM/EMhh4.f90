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
! EMsoft:EMhh4.f90 
!--------------------------------------------------------------------------
!
! PROGRAM: EMhh4
!
!> @author Original hh.f77 code history, see
!> Author = {Head, A.K. and Humble, P. and Clarebrough, L.M. and Morton, A.J. and Forwood, C.T.},
!> Publisher = {North Holland Publishing Company},
!> Series = {Defects in {C}rystalline {S}olids, edited by Amelinckx, S. and Gevers, R. and Nihoul, J.},
!> Title = {Computed Electron Micrographs and Defect Identification},
!> Volume = {7},
!> Year = 1973}
!> Translated to fortran-90 by MDG. All original variable names are kept in upper case
!> notation, all EMsoft add-ons and new code is in lower case.
!
!> @note improvements to the code:
!> - Integrated with EMsoft modules.
!> - Added HDF5 and image output + support for tilt series.
!> - Added standard EMsoft namelist handling to replace the older input format
!
!> @todo
!> - incorporate OpenMP parallel thread support
!
! The new f90 code is a bit more readable than the original f77 code,
! mostly because the modern Fortran language has better control structures.
! All ordinary and computed goto statements have been replaced with
! case and other control statements.  No line labels are used at all.
! Variable names have been left unchanged (mostly) to facilitate comparison
! with original hh4 code as well as debugging.  The original f77 code (a version
! generated in the '80s by the group of Prof. Skalicky at the Technical University
! of Vienna, Austria.) can be downloaded from:
!
! http://ctem.web.cmu.edu/software/HeadHumble.tar.gz
! 
!> @date 01/15/03 MDG 1.0 original conversion from f77 version
!> @date 08/13/19 MDG 2.0 new version, integrated with EMsoft 4.3
!--------------------------------------------------------------------------
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
!***********************************************************************
!*       Main program                                                  *
!***********************************************************************
program EMhh4

use constants 
use io
use files
use crystal
use typedefs
use hhmod
use HDFsupport

IMPLICIT NONE

! all original COMMON blocks are replaced by user-defined structures in typedefs.f90
type(MAPN_block)                 :: MAPN
type(MA_block)                   :: MA
type(MKAP_block)                 :: MKAP
type(MRD_block)                  :: MRD
type(MT_block)                   :: MT
type(MKT_block)                  :: MKT
type(SCALE30_block)              :: SCALE30
type(MP_block)                   :: MP
type(MAP_block)                  :: MAP



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

!=======================
! original variables for the main program
!=======================
! regular integers
integer(kind=irg)             :: ICNT, LLQ, NNN, NNNN, I, J, JB, JC, K, L, KMIN, KMAX, KTOT, MOVE, LUCK, ISTORE, LSWITC, &
                                 IFLAG, JT, JM, KOUNTF, INDL, KK, JZ 
! regular integer arrays
integer(kind=irg)             :: ITYPE(4)
! integer constants
integer(kind=irg),parameter   :: NP(3) = (/2,3,1/), NQ(3) = (/3,1,2/)
integer(kind=irg)             :: ICOL=256, IROW=160, ICOLP=257   ! needs to become variable via namelist !
! integer allocatable
integer(kind=irg),allocatable :: IX(:), IXX(:)
! integer(kind=irg)           :: IX(ICOLP), IXX(ICOLP)

! regular reals
real(kind=sgl)                :: GINB, GINBXU, Z, SBM1, SBM2, SBM3, SFN1, SFN2, SFN3, FNBM, PT, SL, PT2, SL2, &
                                 THBM, EXT1, EXT2, EXT3, EXT4, EXTRA, FRACTI, DIVISO, DELT, WL, DELW, DELL, BACK, BACKD, &
                                 STORE, DELTA, DEL, DEL2, DISTR, DISTRA, DISTL, DISTLA, XXX, YYY, ZZZ, VVV, FAULT1, &
                                 ALPHA, COSA1, SINA1, FAULT2, COSA2, SINA2, FAULT3, COSA3, SINA3, STARTA, SURFAC, XX1, &
                                 TRAMP3, TRAMP7,DNR, DNI, DNN, TTB, TTD, WW, LC1, LC2, LC3, LC4
! regular real arrays
real(kind=sgl)                :: GD(3), BD(4), B2D(4), B3D(4), B4D(4), BM(3), FN(3), FP1X(3),  &
                                 FPX(3), FP3X(3), FP(3), FP3(3), FNX(3), DCX(3,3), DR(4), DI(4), &
                                 UR(4,4), UI(4,4), VR(4,4), VI(4,4), DD(3), SUR(4), SUI(4), &
                                 UX(3), AB(3), AB1(3), POSA(4), POSB(4), COORD(4), HANDL(4), &
                                 HANDR(4), TEMPY(8), QL1(4), QL2(4), QL3(4), QL4(4), S(4,4), QS(4,4), RR(3)
! real allocatable 
real(kind=sgl),allocatable    :: FX(:,:),TBD(:,:), TQB(:),TQD(:), BFINTENS(:,:),DFINTENS(:,:)
! real(kind=sgl)              :: FX(ICOL,4),TBD(IROW,ICOLP), TQB(ICOLP),TQD(ICOLP), BFINTENS(ICOL,IROW),DFINTENS(ICOL,IROW)

! regular complex
complex(kind=sgl)             :: SU(4), CNX(8), MXXX(4,4), MYYY(4,4), MZZZ(4,4)

! character variables
character(15)                 :: IY

!=======================
! additional EMsoft variables (not originally in hh4.f code)
!=======================
type(unitcell), pointer       :: cell
character(fnlen)              :: mess




nullify(cell)


! to be replaced by standard namelist handling
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
! the original hh.f program called an individual routine for each crystal system
! and essentially computed the direct and reciprocal structure matrices.
! In terms of the crystallographic variables of EMsoft, the following
! relations hold:
!   hh.f        EMsoft
!   AT          cell%dsm/cell%a
!   ATR         cell%rsm*cell%a
!
! We will mostly stick to the hh.f variable names for easy translation.
! The routine CrystalData does everything that was originally done
! by the following hh.f subroutines:  TRICLIN, MONOCLI, RHOMBIS,
! TRIGONA, TETRAGONA, HEXAGON, and CUBIC.
!*********************************************************************
 call CrystalData(cell)
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
! this uses EMsoft routines rather than the original code
 GINB=CalcDot(cell,MT%TLG,MT%TLB,'c')/FLOAT(LD) 
 call CalcCross(cell,MT%TLB,MT%TLU,RR,'c','c',0)
 GINBXU=CalcDot(cell,MT%TLG,RR,'c')/FLOAT(LD)
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
  call ANCALC(MAP, MKAP, MAPN, MA, SCALE30) 
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
  call PANCALC(MAP, MKAP, MAPN, MA, MP, SCALE30)
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
  write (6,"(1H0,3F12.8,'  BD',3F12.8,' B2D',3F12.8,' B3D'/1H ,3F12.8,'  BM',3F12.8,'  GD',3F12.8,'  FN'/1H ,3F12.8,' FNX', &
             3F12.8,'FP1X',3F12.8,' FPX'/1H ,3F12.8,'FP3X',3F12.8,'  FP',3F12.8,' FP3')") &
             BD,B2D,B3D,BM,GD,FN,FNX,FP1X,FPX,FP3X,FP,FP3
  write (6,"(1H0,26H ERSTER SEPARATIONSVEKTOR )") 
  write (6,"(1H ,3F12.8,3H AB,F12.8,3H SL,F12.8,3H PT,F12.8,5H DELT)") AB,SL,PT,DELT 
  write (6,"(1H0,27H ZWEITER SEPARATIONSVEKTOR )") 
  write (6,"(1H ,3F12.8,3HAB1,F12.8,3HPT2/1H ,F12.8,4HEXT1,1F12.8,4HEXT2,F12.8,4HEXT3,F12.8,5HEXTRA)") &
             AB1,SL2,PT2,EXT1,EXT2,EXT3,EXTRA
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
 CALL RKM(MRD)
 MRD%X1=cPi*THBM
 CALL RKM(MRD)
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
      call RKM(MRD)
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
   call RKM(MRD)
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
       CALL RKM(MRD)
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
     CALL RKM(MRD)
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
       CALL RKM(MRD)
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
    call RKM(MRD)
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
 
