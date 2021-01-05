!###################################################################
! Copyright (c) 2014-2021, Marc De Graef Research Group/Carnegie Mellon University
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
!> @date 08/23/19 MDG 2.1 debugged and tested; added HDF5 and image output formats
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
use NameListTypedefs
use NameListHandlers

IMPLICIT NONE

character(fnlen)                 :: nmldeffile, progname, progdesc
type(EMhh4NameListType)          :: hhnl

nmldeffile = 'EMhh4.nml'
progname = 'EMhh4.f90'
progdesc = 'Two-Beam Head&Humble dislocation simulation program'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 262 /), progname)

! deal with the namelist stuff
call GetEMhh4NameList(nmldeffile,hhnl)

! generate a set of master EBSD patterns
 call HHComputeImages(hhnl, progname, nmldeffile)

end program EMhh4

!--------------------------------------------------------------------------
!
! SUBROUTINE:HHComputeImages
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute pairs of two-beam BF/DF images for a set of dislocations 
!
!> @param hhnl namelist
!> @param progname program name string
!> @param nmldeffile namelist file name
!
!> @date 08/15/19 MDG 1.0 original based on hh4.f program
!--------------------------------------------------------------------------
subroutine HHComputeImages(hhnl,progname,nmldeffile)

use local
use constants
use crystal
use diffraction
use symmetry
use io 
use files
use error 
use timing
use hhmod
use hhHDFmod
use HDFsupport
use NameListTypedefs
use ISO_C_BINDING
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

type(EMhh4NameListType),INTENT(INOUT)   :: hhnl
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile

! all original COMMON blocks are replaced by user-defined structures in typedefs.f90
type(MAPN_block)              :: MAPN
type(MA_block)                :: MA
type(MKAP_block)              :: MKAP
type(MRD_block)               :: MRD
type(MT_block)                :: MT
type(MKT_block)               :: MKT
type(SCALE30_block)           :: SCALE30
type(MP_block)                :: MP
type(MAP_block)               :: MAP

!=======================
! original hh4.f variables 
!=======================
! regular integers
integer(kind=irg)             :: LLQ, NNN, NNNN, I, J, JB, JC, K, L, KMIN, KMAX, KTOT, MOVE, LUCK, ISTORE, LSWITC, &
                                 IFLAG, JT, JM, KOUNTF, INDL, KK, JZ, IND, LD, LQ, LPIEZO
! regular integer arrays
integer(kind=irg)             :: ITYPE(4)
! integer constants
integer(kind=irg),parameter   :: NP(3) = (/2,3,1/), NQ(3) = (/3,1,2/), addon(4) = (/ 1, 0, 2, 3 /)
integer(kind=irg)             :: ICOL, IROW, ICOLP 
! integer allocatable
integer(kind=irg),allocatable :: IX(:), IXX(:)

! regular reals
real(kind=sgl)                :: GINB, GINBXU, Z, SBM1, SBM2, SBM3, SFN1, SFN2, SFN3, FNBM, PT, SL, PT2, SL2, &
                                 THBM, EXT1, EXT2, EXT3, EXT4, EXTRA, FRACTI, DIVISO, DELT, WL, DELW, DELL, BACK, BACKD, &
                                 STORE, DELTA, DEL, DEL2, DISTR, DISTRA, DISTL, DISTLA, XXX, YYY, ZZZ, VVV, FAULT1, &
                                 ALPHA, FAULT2, FAULT3, STARTA, SURFAC, XX1, ANO, FINISH, START, THICK, XIGEE, &
                                 TRAMP3, TRAMP7,DNR, DNI, DNN, TTB, TTD, WW, LC1, LC2, LC3, LC4
! regular real arrays
real(kind=sgl)                :: GD(3), BD(4), B2D(4), B3D(4), B4D(4), BM(3), FN(3), FP1X(3),  &
                                 FPX(3), FP3X(3), FP(3), FP3(3), FNX(3), DCX(3,3), DR(4), DI(4), &
                                 UR(4,4), UI(4,4), VR(4,4), VI(4,4), DD(3), SUR(4), SUI(4), COSA(3), SINA(3), &
                                 UX(3), AB(3), AB1(3), POSA(4), POSB(4), COORD(4), HANDL(4), &
                                 HANDR(4), TEMPY(8), QL1(4), QL2(4), QL3(4), QL4(4), S(4,4), QS(4,4), RR(3)
! real allocatable 
real(kind=sgl),allocatable    :: FX(:,:), TBD(:,:), TQB(:),TQD(:), BFINTENS(:,:,:), DFINTENS(:,:,:)

! regular complex
complex(kind=sgl)             :: SU(4), CNX(8), MXXX(4,4), MYYY(4,4), MZZZ(4,4)

! character variables
character(15)                 :: IY
character(fnlen)              :: diagfile = 'HHdiagnostics.txt'

!=======================
! additional EMsoft variables (not originally in hh4.f code)
!=======================
type(unitcell)                :: cell
type(gnode)                   :: rlp
character(fnlen)              :: mess, fname
character(fnlen),allocatable  :: legendfiles(:)
character(3)                  :: filenum
character(11)                 :: dstr
character(15)                 :: tstrb
character(15)                 :: tstre
integer(kind=irg)             :: numim, imnum, hdferr
real(kind=sgl),allocatable    :: wvalues(:)
logical                       :: isallowed
real(kind=dbl)                :: eps = 1.0D-6
real(kind=sgl)                :: wstep, io_real(1), mimi, mama

! declare variables for use in object oriented image module
integer                       :: iostat
character(len=128)            :: iomsg
logical                       :: isInteger
type(image_t)                 :: im
integer(int8), allocatable    :: output_image(:,:)

call timestamp(datestring=dstr, timestring=tstrb)

!nullify(cell)        
!allocate(cell)        
cell%fname = trim(hhnl%xtalname)

! get some parameters from the namelist
 ICOL = hhnl%ICOL 
 IROW = hhnl%IROW
 ICOLP = hhnl%ICOL+1 
 numim = hhnl%wnum

! allocate the string array for the legend files 
 allocate( legendfiles(numim) )
 do i=1,numim
   write (filenum,"(I3.3)") i
   legendfiles(i) = trim(EMsoft_getEMdatapathname())//'legend_'//filenum//'.txt'
   legendfiles(i) = EMsoft_toNativePath(legendfiles(i))
 end do

! allocate all allocatable arrays here 
 allocate( IX(ICOLP), IXX(ICOLP) )
 allocate( FX(ICOL,4), TBD(IROW,ICOLP), TQB(ICOLP), TQD(ICOLP) )
 allocate( BFINTENS(ICOL, IROW, numim), DFINTENS(ICOL, IROW, numim) )
 allocate( wvalues(numim) )

! set original hh4.f parameters that are not used in this f90 version of the code
 IND = 0
 LPIEZO = 0
 LQ=2
 LLQ=1 
 NNN=ICOL
 NNNN=30+10*LQ 

! copy namelist entries to appropriate variables

! elastic moduli
 MKAP%D1(1,1:6) = hhnl%D1row1 
 MKAP%D1(2,1:6) = hhnl%D1row2 
 MKAP%D1(3,1:6) = hhnl%D1row3 
 MKAP%D1(4,1:6) = hhnl%D1row4 
 MKAP%D1(5,1:6) = hhnl%D1row5 
 MKAP%D1(6,1:6) = hhnl%D1row6 
! other parameters
 MT%LU = hhnl%LU
 MT%LG = hhnl%LG
 MT%LBM = hhnl%LBM
 MT%LFN = hhnl%LFN
 MT%LB = hhnl%LB
 MT%LB2 = hhnl%LB2
 MT%LB3 = hhnl%LB3
 MT%LB4 = hhnl%LB4
 MT%LD = hhnl%LD
 MT%LD2 = hhnl%LD2
 MT%LD3 = hhnl%LD3
 MT%LD4 = hhnl%LD4
 MT%LFP1 = hhnl%LFP1
 MT%LFP = hhnl%LFP
 MT%LFP3 = hhnl%LFP3 
 MT%LS1 = hhnl%LS1
 MT%LS2 = hhnl%LS2
 MT%LS3 = hhnl%LS3
 MT%LQ1 = hhnl%LQ1
 MT%LQ2 = hhnl%LQ2
 MT%LQ3 = hhnl%LQ3
 ! MT%LF1 = hhnl%LF1
 ! MT%LF2 = hhnl%LF2
 ! MT%LF3 = hhnl%LF3
 ! MT%LF4 = hhnl%LF4
 SCALE30%LTEST = hhnl%LTEST
 LD = hhnl%LD 

! we don't do piezoelectric contributions in this version of the program
 MKAP%EP = 0.0
 MKAP%EA = 0.0

! open diagnostic file for output if hhnl%LTEST.eq.1
 if (hhnl%LTEST.eq.1) then 
   OPEN(dataunit,FILE=trim(diagfile), Status='UNKNOWN')
 end if

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
! We will mostly stick to the hh.f variable names for easy translation.
! The routine CrystalData does everything that was originally done
! by the following hh.f subroutines:  TRICLIN, MONOCLI, RHOMBIS,
! TRIGONA, TETRAGONA, HEXAGON, and CUBIC.
!*********************************************************************
 call CrystalData(cell, verbose=.TRUE.)
 ! scale the direct and reciprocal structure matrices
 cell%dsm = cell%dsm/cell%a
 cell%rsm = cell%rsm*cell%a
 MKT%AT  = cell%dsm
 MKT%ATR = cell%rsm

!*********************************************************************
! transform all crystal variables to Cartesian frame (former TRAFO routine)
!*********************************************************************
 call TransSpace(cell, float(MT%LU),  MT%TLU,  'd', 'c')
 call TransSpace(cell, float(MT%LF1), MT%TLF1, 'd', 'c')
 call TransSpace(cell, float(MT%LF2), MT%TLF2, 'd', 'c')
 call TransSpace(cell, float(MT%LF3), MT%TLF3, 'd', 'c')
 call TransSpace(cell, float(MT%LF4), MT%TLF4, 'd', 'c')
 call TransSpace(cell, float(MT%LBM), MT%TLBM, 'd', 'c')
 call TransSpace(cell, float(MT%LFN), MT%TLFN, 'd', 'c')
 call TransSpace(cell, float(MT%LB),  MT%TLB,  'd', 'c')
 call TransSpace(cell, float(MT%LB2), MT%TLB2, 'd', 'c')
 call TransSpace(cell, float(MT%LB3), MT%TLB3, 'd', 'c')
 call TransSpace(cell, float(MT%LB4), MT%TLB4, 'd', 'c')
 call TransSpace(cell, float(MT%LS1), MT%TLS1, 'd', 'c')
 call TransSpace(cell, float(MT%LS2), MT%TLS2, 'd', 'c')
 call TransSpace(cell, float(MT%LS3), MT%TLS3, 'd', 'c')

 call TransSpace(cell, float(MT%LG),   MT%TLG,   'r', 'c')
 call TransSpace(cell, float(MT%LFP),  MT%TLFP,  'r', 'c')
 call TransSpace(cell, float(MT%LFP1), MT%TLFP1, 'r', 'c')
 call TransSpace(cell, float(MT%LFP3), MT%TLFP3, 'r', 'c')
! undo the scaling
 cell%dsm = cell%dsm*cell%a
 cell%rsm = cell%rsm/cell%a

!***********************************************************
!*      Computation of  G.B   and  G.(B X U)               *
!***********************************************************
! this uses EMsoft routines rather than the original code
 GINB=CalcDot(cell,MT%TLG,MT%TLB,'c')/FLOAT(LD) 
 call CalcCross(cell,MT%TLB,MT%TLU,RR,'c','c',0)
 GINBXU=CalcDot(cell,MT%TLG,RR,'c')/FLOAT(LD)

!***********************************************************************
! new code: compute the extinction distance and the ano ratio
! for the selected g-vector
!***********************************************************************
cell%voltage = dble(hhnl%kV)
call CalcWaveLength(cell,rlp,skip=3,verbose=.TRUE.)
call CalcPositions(cell,'v')
rlp%method = 'WK'
call CalcUcg(cell, rlp, MT%LG)
isallowed = IsGAllowed(cell, MT%LG)
! check whether this is a lattice extinction, a symmetry extinction, or an allowed reflection
if ((rlp%Umod.lt.eps).and.(isallowed.eqv..FALSE.)) then
  call FatalError('HHComputeImages','This reflection is absent due to lattice centering.')
end if
if ((rlp%Umod.lt.eps).and.(isallowed.eqv..TRUE.)) then
  call FatalError('HHComputeImages','This reflection is absent due to glide or screw symmetry elements.')
end if
MRD%CN = 0.0
! get the extinction distance and the absorption ratio for this reflection
XIGEE = 10.0*rlp%xg
MRD%ANO = -1.0/rlp%ar
io_real(1) = XIGEE
call WriteValue(' Extinction distance [nm] ',io_real, 1)
io_real(1) = MRD%ANO
call WriteValue(' Absorption ratio         ',io_real, 1)

!***********************************************************************
!*       Make sure that the input data make geometric sense            *
!*       and initialize some default values; a lot of this was         *
!*       typical spaghetti-code....                                    *
!***********************************************************************
! make sure the START and FINISH values make sense
 FINISH = hhnl%FINISH 
 START = hhnl%START 
 THICK = hhnl%THICK 
 if ((FINISH.eq.0.0).AND.(START.eq.0.0)) then 
  START=0.0 
  FINISH=THICK
 end if
 if (FINISH.le.START) then
   call FatalError('HHComputeImages','START after FINISH')
 end if

! intercept zero denominators by setting them to 1
 if (MT%LD.eq.0)  MT%LD=1
 if (MT%LD2.eq.0)  MT%LD2=1
 if (MT%LD3.eq.0)  MT%LD3=1
 if (MT%LD4.eq.0)  MT%LD4=1
 if (MT%LQ1.eq.0)  MT%LQ1=1
 if (MT%LQ2.eq.0)  MT%LQ2=1
 if (MT%LQ3.eq.0)  MT%LQ3=1

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
  if (sum((MT%TLFP3-MT%TLFP1)**2).ne.0.0) then 
    call Message('Fault planes 1 and 3 not identical in input; identity 3=1 imposed')
  end if
  MT%TLFP3=MT%TLFP1 
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
   call FatalError('HHComputeImages', ' Beam parallel to line direction ')
  end if
 end do
 if ((sum(MT%LU*MT%LFP1)**2+sum(MT%LU*MT%LFP)**2+sum(MT%LU*MT%LFP3)**2).ne.0.0) then
  call FatalError('HHComputeImages',' Line direction not in fault planes ')
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
  BD(J)=BD(J)+sum((MT%TLB(1:3))*MAP%DC(J,1:3))/FLOAT(MT%LD)
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
  B2D(J)=B2D(J)+sum(MT%TLB2(1:3)*MAP%DC(J,1:3))/FLOAT(MT%LD2)
  B3D(J)=B3D(J)+sum(MT%TLB3(1:3)*MAP%DC(J,1:3))/FLOAT(MT%LD3)
  B4D(J)=B4D(J)+sum(MT%TLB4(1:3)*MAP%DC(J,1:3))/FLOAT(MT%LD4)
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
  call FatalError('HHComputeImages',' Line direction parallel to surface ')
 end if
 FNBM=SFN1*SBM1+SFN2*SBM2+SFN3*SBM3
 if (FNBM.le.0.0) then 
  call FatalError('HHComputeImages',' Foil normal and beam not acute ')
 end if 

!***********************************************************************
!*       Computation of image size and positions of dislocations       *
!***********************************************************************
 Z=SQRT(FP(1)**2+FP(2)**2) 
 if(Z.eq.0.0) Z=1. 
 ! if(hhnl%SEP.ne.0.0) then
 !  call Message(' FAULT PLANE 2 ZERO WITH SEP NONZERO')
 ! end if
 AB(1)=FP(2)/Z 
 AB(2)=-FP(1)/Z
 AB(3)=0.0
 PT=hhnl%SEP*AB(1)
 SL=hhnl%SEP*AB(2)/BM(2)
 Z=SQRT(FP3(1)**2+FP3(2)**2) 
 if(Z.eq.0.0) Z=1. 
 AB1(1)=FP3(2)/Z 
 AB1(2)=-FP3(1)/Z
 AB1(3)=0.0
 PT2=hhnl%SEP2*AB1(1) 
 SL2=-hhnl%SEP2*AB1(2)/BM(2)
! we do not consider piezoelectric effects in this version of the program
!if (LPIEZO.eq.0) then 
  call ANCALC(MAP, MKAP, MAPN, MA, SCALE30) 
  if (MAPN%KRASH.eq.0) then
   if (SCALE30%LTEST.eq.1) then
     do I=1,3
      write(dataunit,*) (MA%AR(I,J),MA%AI(I,J),J=1,3)
     end do
     write(dataunit,*) (GD(I),I=1,3)
     do I=1,3
      write(dataunit,*) MA%PR(I),MA%PI(I)
     end do
     do I=1,3
      write(dataunit,*) (MA%EMR(I,J),MA%EMI(I,J),J=1,3)
     end do
     write(dataunit,"('  H  ',3F20.10)") ((MA%H(I,J),J=1,3),I=1,3)
   end if
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
   if (SCALE30%LTEST.eq.1) then
     write(dataunit,*) (MRD%CN(I),I=1,16)
   end if
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
   if (SCALE30%LTEST.eq.1) then
     write(dataunit,"(' H-ANCALC ',4F10.4)") ((MA%H(I,J),J=1,4),I=1,4) 
   end if
  else
   STOP 
  end if 
!else  
! the following lines are all commented out because we do not 
! want to consider piezoelectric effects at this point in time
!   call PANCALC(MAP, MKAP, MAPN, MA, MP, SCALE30)
!   if (MAPN%KRASH.eq.0) then
!    do K=1,4
!     SU(K)=CMPLX(0.0,0.0)
!     do I=1,3
!      SU(K)=SU(K)+GD(I)*MP%AS(I,K) 
!     end do
!    end do
!    do K=1,4
!     SU(K)=SU(K)*MP%PC(K) 
!    end do
! !*******************************************************************
! !   First Dislocation                                              *
! !*******************************************************************
!    do J=1,4 
!     MRD%CN(J+8)=REAL(MP%PC(J)) 
!     MRD%CN(J+12)=AIMAG(MP%PC(J))**2
!     CNX(J)=CMPLX(0.0,0.0) 
!     do L=1,4
!      CNX(J)=CNX(J)+(MP%EL(L,J)*BD(L)-MP%AS(L,J)*QL1(L))
!     end do
!     MRD%CN(J)=AIMAG(CNX(J)*SU(J))*2.0 
!     MRD%CN(J+4)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0
!    end do
! !*******************************************************************
! !   Second Dislocation                                             *
! !*******************************************************************
!    do J=1,4
!     CNX(J)=CMPLX(0.0,0.0) 
!     do L=1,4
!      CNX(J)=CNX(J)+(MP%EL(L,J)*B2D(L)-MP%AS(L,J)*QL2(L)) 
!     end do
!     MRD%CN(J+21)=AIMAG(CNX(J)*SU(J))*2.0
!     MRD%CN(J+25)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0 
!    end do
! !*******************************************************************
! !   Third Dislocation                                                *
! !****************************************************************** 
!    do J=1,4
!     CNX(J)=CMPLX(0.0,0.0) 
!     do L=1,4
!      CNX(J)=CNX(J)+(MP%EL(L,J)*B3D(L)-MP%AS(L,J)*QL3(L)) 
!     end do
!     MRD%CN(J+31)=AIMAG(CNX(J)*SU(J))*2.0
!     MRD%CN(J+35)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0 
!    end do
! !*******************************************************************
! !   Fourth Dislocation                                               *
! !*******************************************************************
!    do J=1,4
!     CNX(J)=CMPLX(0.0,0.0) 
!     do L=1,4
!      CNX(J)=CNX(J)+(MP%EL(L,J)*B4D(L)-MP%AS(L,J)*QL4(L)) 
!     end do
!     MRD%CN(J+50)=AIMAG(CNX(J)*SU(J))*2.0
!     MRD%CN(J+54)=AIMAG(CNX(J)*SU(J)*CONJG(MP%PC(J)))*2.0 
!    end do
! !*********************************************************************
! !*    BERECHNEN DER MATRIZEN   H, S, Q   UND AUSDRUCKEN              *
! !*       SIEHE DEFINITION NACH                                       *
! !*                - BARNETT UND LOTHE -                              *
! !*            PHYS.STAT.SOL(B) 67,105(1975)                          *
! !*********************************************************************
! !
!    do I=1,4
!     do J=1,4
!      MXXX(I,J)=CMPLX(0.0,0.0)
!      MYYY(I,J)=CMPLX(0.0,0.0)
!      MZZZ(I,J)=CMPLX(0.0,0.0)
!      do K=1,4
!       MXXX(I,J)=MXXX(I,J)+MP%EL(I,K)*MP%EL(J,K) 
!       MYYY(I,J)=MYYY(I,J)+MP%AS(I,K)*MP%EL(J,K) 
!       MZZZ(I,J)=MZZZ(I,J)+MP%AS(I,K)*MP%AS(J,K) 
!      end do
!      MA%H(I,J)=-(1.0/(2.0*cPi))*AIMAG(MXXX(I,J)) 
!      S(I,J)=-2.0*AIMAG(MYYY(I,J))
!      QS(I,J)=-2.0*AIMAG(MZZZ(I,J)) 
!     end do
!    end do
!    write(6,"(' ',4F10.4,' H- MATRIX')") ((MA%H(K,L),L=1,4),K=1,4) 
!    write(6,"(' ',4F10.4,' S- MATRIX')") ((S(K,L),L=1,4),K=1,4)
!    write(6,"(' ',4F10.4,' Q- MATRIX')") ((QS(K,L),L=1,4),K=1,4) 
!   else 
!    STOP
!   end if
!  end if 
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
  write (dataunit,"(1H1,16H DC-KOORDINATEN )") 
  write (dataunit,"(1X,3F12.8)") ((MAP%DC(I,J),J=1,3),I=1,3) 
  write (dataunit,"(1H ,17H DCX-KOORDINATEN )") 
  write (dataunit,"(1X,3F12.8)") ((DCX(I,J),J=1,3),I=1,3)
  write (dataunit,"(1H ,48H KOORDINATEN DER BURGERSVEKTOREN UND FAULTPLANES)") 
  write (dataunit,"(1H0,3F12.8,'  BD',3F12.8,' B2D',3F12.8,' B3D'/1H ,3F12.8,'  BM',3F12.8,'  GD',3F12.8,'  FN'/1H ,3F12.8,' FNX', &
             3F12.8,'FP1X',3F12.8,' FPX'/1H ,3F12.8,'FP3X',3F12.8,'  FP',3F12.8,' FP3')") &
             BD,B2D,B3D,BM,GD,FN,FNX,FP1X,FPX,FP3X,FP,FP3
  write (dataunit,"(1H0,26H ERSTER SEPARATIONSVEKTOR )") 
  write (dataunit,"(1H ,3F12.8,3H AB,F12.8,3H SL,F12.8,3H PT,F12.8,5H DELT)") AB,SL,PT,DELT 
  write (dataunit,"(1H0,27H ZWEITER SEPARATIONSVEKTOR )") 
  write (dataunit,"(1H ,3F12.8,3HAB1,F12.8,3HPT2/1H ,F12.8,4HEXT1,1F12.8,4HEXT2,F12.8,4HEXT3,F12.8,5HEXTRA)") &
             AB1,SL2,PT2,EXT1,EXT2,EXT3,EXTRA
  write (dataunit,"(1H ,15H CN-KONSTANTEN )") 
  write (dataunit,"(1H ,4F12.8)") (MRD%CN(J),J=1,16)
  write (dataunit,"(' ',4F12.6)") (MRD%CN(J+21),J=1,8)
  write (dataunit,"(' ',4F12.6)") (MRD%CN(J+31),J=1,8)
  write (dataunit,"(' ',4F12.6)") (MRD%CN(J+50),J=1,8)
 end if
 MRD%CN(30)=1000.0 
 

 if (hhnl%wnum.eq.1) then 
   wvalues(1) = hhnl%wmin
 else  
   wstep = (hhnl%wmax-hhnl%wmin) / float(hhnl%wnum-1) 
   wvalues = hhnl%wmin + (/ (i*wstep, i=0,hhnl%wnum-1) /)
 end if

! loop over all the image pairs to be computed 
do imnum=1,hhnl%wnum
! set the excitation error parameter and related quantities
 MRD%CN(17) = wvalues(imnum)
 MRD%CN(18) = 2.0*MRD%CN(17) 
 MRD%X=0.0 
 MRD%Q=0.0 
 MRD%D=0.0
 MRD%DT=0.0
 MRD%YT=0.0
 MRD%ERROR=0.0001
 MRD%SKIP=0.0

 io_real(1) = MRD%CN(17)
 MRD%KOUNT = 0
 call WriteValue(' starting image computation for w ', io_real,1)

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
!  and subroutine calls... and really should be parallelized using OpenMP
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
  HANDR=COORD-DEL 
  HANDL=COORD+DEL 
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
     else
       MRD%CN(15)=-HANDL(KK) 
       MOVE=1
       EXIT  ! the do KK loop
     end if
    end if ! else cycle KK loop
   end if ! else cycle KK loop
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
  COSA = 0.0
  SINA = 0.0
!***********************************
!*   First Stacking Fault          *
!***********************************
  FAULT1=10000.0
  if (YYY*VVV.le.0.0) then 
   if (sum(MT%TLS1**2).ne.0.0) then 
    ALPHA=cPi*sum(MT%TLG*MT%TLS1)*2.0/FLOAT(MT%LQ1) 
    COSA(1)=COS(ALPHA)
    SINA(1)=SIN(ALPHA)
    if (FP1X(2).ne.0.0) then 
     FAULT1=MRD%CN(21)-(MRD%CN(19)-MRD%CN(20))*FP1X(1)/FP1X(2)+hhnl%FAP1
    end if
   end if
  end if
!*************************************
!*  Second stacking fault          ***
!*************************************
  FAULT2=10001.0
  if (XXX*YYY.lt.0.0) then
   if (sum(MT%TLS2**2).ne.0.0) then
    ALPHA=cPi*sum(MT%TLG*MT%TLS2)*2.0/FLOAT(MT%LQ2) 
    COSA(2)=COS(ALPHA)
    SINA(2)=SIN(ALPHA)
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
     ALPHA=cPi*sum(MT%TLG*MT%TLS3)*2.0/FLOAT(MT%LQ3)
     COSA(3)=COS(ALPHA)
     SINA(3)=SIN(ALPHA)
     if (FP3X(2).ne.0.0) then
      FAULT3=-MRD%CN(21)-(MRD%CN(19)+MRD%CN(20))*FP3X(1)/FP3X(2)+hhnl%FAP3 
     end if
    end if 
   end if 
  end if
!
  STARTA=cPi*(EXTRA/2.0-(THBM+EXTRA)*FINISH/THICK)-(MRD%CN(19)*FNX(1)/FNX(2))
  SURFAC=STARTA+cPi*THBM 
  POSA = (/ FAULT1, FAULT2, FAULT3, SURFAC /)
  ITYPE = (/ 1, 2, 3, 4 /)
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
     CYCLE
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
      if (I.lt.4) then 
        TRAMP3=MRD%Y(3) 
        TRAMP7=MRD%Y(7) 
        MRD%Y(3)=MRD%Y(3)*COSA(I)-MRD%Y(4)*SINA(I)
        MRD%Y(7)=MRD%Y(7)*COSA(I)-MRD%Y(8)*SINA(I)
        MRD%Y(4)=MRD%Y(4)*COSA(I)+TRAMP3*SINA(I)
        MRD%Y(8)=MRD%Y(8)*COSA(I)+TRAMP7*SINA(I)
        MRD%X1=XX1
        POSA(KOUNTF)=-9000 - addon(I) 
        KOUNTF=KOUNTF+1 
      else 
        TEMPY(1:8)=MRD%Y(1:8) 
        MRD%X1=XX1
        POSA(KOUNTF)=-9000 - addon(4) 
        KOUNTF=KOUNTF+1 
        IFLAG=1
      end if 
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
       TRAMP3=MRD%Y(3) 
       TRAMP7=MRD%Y(7) 
       MRD%Y(3)=MRD%Y(3)*COSA(I)-MRD%Y(4)*SINA(I)
       MRD%Y(7)=MRD%Y(7)*COSA(I)-MRD%Y(8)*SINA(I)
       MRD%Y(4)=MRD%Y(4)*COSA(I)+TRAMP3*SINA(I)
       MRD%Y(8)=MRD%Y(8)*COSA(I)+TRAMP7*SINA(I)
       MRD%X1=XX1
       POSA(KOUNTF)=-9050 - (I-1) 
       KOUNTF=KOUNTF+1 
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
       TRAMP3=MRD%Y(3) 
       TRAMP7=MRD%Y(7) 
       MRD%Y(3)=MRD%Y(3)*COSA(I)-MRD%Y(4)*SINA(I)
       MRD%Y(7)=MRD%Y(7)*COSA(I)-MRD%Y(8)*SINA(I)
       MRD%Y(4)=MRD%Y(4)*COSA(I)+TRAMP3*SINA(I)
       MRD%Y(8)=MRD%Y(8)*COSA(I)+TRAMP7*SINA(I)
       MRD%X1=XX1
       POSA(KOUNTF)=-8050 - (I-1) 
       KOUNTF=KOUNTF+1 
       CYCLE ifkount3
      end if
     end if
    end do ifkount3
    call RKM(MRD)
    KTOT=KTOT+MRD%KOUNT 
    INDL=LLQ*JM+1 
! 
!********************************************************************** 
!***   BERECHNUNG DER  INTENSITAETEN UND GLEICHZEITIGES AB-         *** 
!***   SPEICHERN DER UNTERGRUNDINTENSITAETEB (TTD)                  *** 
!********************************************************************** 
! 
    TTB=(FX(JM,1)*MRD%Y(1)-FX(JM,2)*MRD%Y(2)+FX(JM,3)*MRD%Y(5)-FX(JM,4)*MRD%Y(6))**2+ &
        (FX(JM,1)*MRD%Y(2)+FX(JM,2)*MRD%Y(1)+FX(JM,3)*MRD%Y(6)+FX(JM,4)*MRD%Y(5))**2 
    TTD=(FX(JM,1)*MRD%Y(3)-FX(JM,2)*MRD%Y(4)+FX(JM,3)*MRD%Y(7)-FX(JM,4)*MRD%Y(8))**2+ &
        (FX(JM,1)*MRD%Y(4)+FX(JM,2)*MRD%Y(3)+FX(JM,3)*MRD%Y(8)+FX(JM,4)*MRD%Y(7))**2 
    TQB(INDL)=TTB
    TQD(INDL)=TTD
  end do  ! 
  TQB(1)=(TEMPY(1)**2+TEMPY(2)**2)
  TQD(1)=(TEMPY(3)**2+TEMPY(4)**2)

  if (LQ.eq.0) then
   do JZ=2,ICOL,2 
      TQB(JZ)=0.5*(TQB(JZ-1)+TQB(JZ+1))
      TQD(JZ)=0.5*(TQD(JZ-1)+TQD(JZ+1))
   end do
  end if
  ! do J=1,3
  !  if ((ABS(MRD%CN(19)+COORD(J))-DELL).le.0.0) then 
  !  end if
  ! end do
! 
!  AUSDRUCK EINER BILDZEILE 
! 
  do j=1,ICOL
   BFINTENS(j,JC,imnum)=TQB(j)
   DFINTENS(j,JC,imnum)=TQD(j)
  end do
 end do  ! from several pages back !!!


 WW=79.0*DELW/cPi 

 if (SCALE30%LTEST.eq.1) then  
! close the diagnostics file if this is the last image 
   if (imnum.eq.hhnl%wnum) CLOSE (UNIT=dataunit)
 end if
! 
!*******************************************************
!*       AUSDRUCK DER BILDLEGENDE                *******
!*******************************************************
!

! open a temporary file with the image legend for addition to the output HDF5 file 
   call Message('  --> writing image legend to '//trim(legendfiles(imnum)) )
   OPEN(dataunit,FILE=trim(legendfiles(imnum)), status='UNKNOWN')

   write(dataunit,"('    HH.f90  ',' TWO-BEAM ',F6.2,' WL',F6.2,' WW', &
           F5.2,' STR ',F5.2,' FIN ',F7.3,'TH',F7.3,'THBM')") WL,WW,START,FINISH,THICK,THBM 
  
   write(dataunit,"(' ',3I2,'/',I1,'B   ',F8.4,' Q1 ',3I2,'U    ',3I2,  &
           'G    ',3I2,'BM   ',3I2,'FN',F7.3,'W',F9.3,'BACK')") &
           MT%LB,LD,QL1(4),MT%LU,MT%LG,MT%LBM,MT%LFN,MRD%CN(17),BACK
  
   write(dataunit,"(' ',3I2,'/',I1,'B2  ',F8.4,' Q2 ',F5.2,'SEP  ',3I2, &
           'FP1  ',3I2,'FP2  ',3I2,'FP3  ',3I2,'/',I1,'SH1  ',3I2,'/',I1, &
           'SH2  ',3I2,'/',I1,'SH3  ',' 4VS-SYM ',I2,' NNNN')") &
           MT%LB2,MT%LD2,QL2(4),hhnl%SEP,MT%LFP1,MT%LFP,MT%LFP3,MT%LS1,MT%LQ1,MT%LS2,MT%LQ2,MT%LS3,MT%LQ3,NNNN
  
   write(dataunit,"(' ',3I2,'/',I1,'B3  ',F8.4,' Q3 ',F5.2,'SEP2  ',F5.2, &
           ' FAP1 ',F5.2,' FAP3 ',F8.1,' XIGEE ',F4.1,' DELTA ',F8.5, &
            ' ANO ')") MT%LB3,MT%LD3,QL3(4),hhnl%SEP2,hhnl%FAP1,hhnl%FAP3,XIGEE,DELTA,MRD%ANO
  
   write(dataunit,"(' ',3I2,'/',I1,'B4  ',F8.4,' Q4 ',' PIEZO=',I1,' IND=',I1)") &
             MT%LB4,MT%LD4,QL4(4),LPIEZO,IND 
  
   write(dataunit,"('  G.B=',F10.4,'  G.(B X U)=',F10.4)") GINB,GINBXU

   CLOSE(dataunit, status='keep')
!
end do ! loop over all images 

call timestamp(datestring=dstr, timestring=tstre)

call Message(' Writing HDF5 outfile file ',"(/A)")

call h5open_EMsoft(hdferr)
call writeHH4_HDFfile(hhnl, BFINTENS, DFINTENS, dstr, tstrb, tstre, legendfiles, progname, nmldeffile)
call h5close_EMsoft(hdferr)

! cleanup: remove the legendfiles 
call Message(' Cleaning up legend files (they have been added to the HDF5 output file)',"(/A/)")
do i=1,numim
   OPEN(dataunit,FILE=trim(legendfiles(i)), status='OLD')
   CLOSE(dataunit,status='delete')
end do

! image output requested ?  If so, then BF and DF pairs are put together in single images but with 
! a common gray scale for the entire series.
if (trim(hhnl%imageprefix).ne.'undefined') then 
  allocate(output_image(2*ICOL, IROW))
  mimi = minval ( (/ minval(BFINTENS), minval(DFINTENS) /) )
  mama = maxval ( (/ maxval(BFINTENS), maxval(DFINTENS) /) )
  BFINTENS = 255.0*(BFINTENS-mimi)/(mama-mimi)
  DFINTENS = 255.0*(DFINTENS-mimi)/(mama-mimi)

  do i=1,numim
   write (filenum,"(I3.3)") i
   fname = trim(EMsoft_getEMdatapathname())//trim(hhnl%imageprefix)//filenum//'.'//trim(hhnl%imagetype)
   fname = EMsoft_toNativePath(fname)
   
   output_image(1:ICOL,1:IROW) = int(BFINTENS(1:ICOL,1:IROW,i))
   output_image(ICOL+1:2*ICOL,1:IROW) = int(DFINTENS(1:ICOL,1:IROW,i))

   im = image_t(output_image)
   if(im%empty()) call Message(" HHComputeImages: failed to convert array to image")

   call im%write(trim(fname), iostat, iomsg) ! format automatically detected from extension
   if(0.ne.iostat) then
      call Message(" HHComputeImages: failed to write image to file : "//iomsg)
   else  
      call Message(' BF/DF images written to '//trim(fname))
   end if 
  end do 

end if

end subroutine HHComputeImages
 
