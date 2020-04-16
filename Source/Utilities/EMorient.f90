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
! EMsoft:EMorient.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMorient
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief stereographic projection of a crystal orientation relation
!
! 
!> @date 10/13/98 MDG 1.0 original
!> @date 05/22/01 MDG 2.0 f90
!> @date 04/16/13 MDG 3.0 rewrite 
!> @date 06/13/14 MDG 4.0 rewrite without global variables
!> @date 03/29/17 MDG 4.1 correction of transformation matrices
!--------------------------------------------------------------------------
program EMorient

use local
use typedefs
use crystal
use HDFsupport
use graphics
use files
use postscript
use io
use math

IMPLICIT NONE

character(1)      		:: sp
logical           		:: nn,topbot
type(orientation) 		:: orel
real(kind=sgl)    		:: rr(3),gg(3),g(3),r(3),M(3,3),negthresh,p(3),Ep(3,3),E(3,3),TT(3,3), io_real(3), &
				   CX, CY, CRad, xst, yst
real(kind=dbl)    		:: dE(3,3),dgg(3)
integer(kind=irg) 		:: h,k,l,cr,hkl(3),iview(3),inm, i, ih, ik, il, imanum
character(fnlen)		:: progname, progdesc, gname
type(unitcell)        		:: cellA, cellB
type(postscript_type)		:: PS
logical				:: loadingfile

interface
	subroutine LocalDrawFrame(PS,CX,CY,CRad,iview,sp,cellA,cellB,orel)

	use local
	use typedefs
	use io
	use postscript


	type(postscript_type),INTENT(INOUT)	:: PS
	real(kind=sgl),INTENT(IN)               :: CX, CY, CRad
	integer(kind=irg),INTENT(INOUT)		:: iview(3)
	character(1),INTENT(IN)		       	:: sp
	type(unitcell)               		:: cellA,cellB
	type(orientation),INTENT(IN)  		:: orel

	end subroutine LocalDrawFrame
end interface



 progname = 'EMorient.f90'
 progdesc = 'Stereographic projection of orientation relation'
 call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
 call Interpret_Program_Arguments(1,(/ 919 /), progname)
 
 inm=2
 cellA % SG % SYM_reduce=.TRUE.
 cellB % SG % SYM_reduce=.TRUE.
 topbot=.FALSE.

! 20cm radius projection circle, centered on page [inches]
 CRad = 3.937
 CX = 3.25
 CY = 3.5
 negthresh=-0.0001

! read crystal A information
 call ReadValue(' Enter xtal file name A : ', gname,"(A)")
 cellA%fname = trim(gname)
 call CrystalData(cellA)

! read crystal B information
 call ReadValue(' Enter xtal file name B : ', gname,"(A)")
 cellB%fname = trim(gname)
 call CrystalData(cellB)

! get orientation relation
 call GetOR(orel)

! compute E matrix  [page 74]
 call TransSpace(cellA,orel % gA,r,'r','d')
 call NormVec(cellA,r,'d')
 call NormVec(cellA,orel % tA,'d')
 call CalcCross(cellA,orel % tA,r,p,'d','d',0)
 call NormVec(cellA,p,'d')
 E(1,1:3)=matmul(cellA % dsm, r)
 E(2,1:3)=matmul(cellA % dsm, p)
 E(3,1:3)=matmul(cellA % dsm, orel % tA)
 call mInvert(dble(E),dE,.FALSE.)
 E = dE
 call Message('Transformation matrix E', frm = "(A)")
 do i=1,3
  io_real(1:3) = E(i,1:3)
  call WriteValue('', io_real, 3)
 end do

! compute E-prime matrix 
 call TransSpace(cellB,orel % gB,r,'r','d')
 call NormVec(cellB,r,'d')
 call NormVec(cellB,orel % tB,'d')
 call CalcCross(cellB,orel % tB,r,p,'d','d',0)
 call NormVec(cellB,p,'d')
 Ep(1,1:3)=matmul(cellB % dsm, r)
 Ep(2,1:3)=matmul(cellB % dsm, p)
 Ep(3,1:3)=matmul(cellB % dsm, orel % tB)
 call Message('Transformation matrix E-prime', frm = "(A)")
 do i=1,3
  io_real(1:3) = Ep(i,1:3)
  call WriteValue('', io_real, 3)
 end do

! and multiply both matrices to get transformation matrix M
 TT = matmul(E,Ep)
 call Message('Transformation matrix for orientation relation', frm = "(A)")
 do i=1,3
  io_real(1:3) = TT(i,1:3)
  call WriteValue('', io_real, 3)
 end do
 call Message(' --- ', frm = "(A)")
 
! real space or reciprocal space
 call GetDrawingSpace(sp)

! and from here one it is the same as a regular stereographic projection
! except that there are two sets of points to be drawn.
! viewing direction
 call GetViewingDirection(cellA%hexset,iview)

! create transformation matrix
 call ProjectionMatrix(cellA,iview,M)

! open PostScript file
 imanum = 1
 call PS_openfile(PS, progdesc, imanum)

! write text and draw projection circle
 call LocalDrawFrame(PS,CX,CY,CRad,iview,sp,cellA,cellB,orel)

! loop over all planes or directions
 do h=-inm,inm
  do k=-inm,inm
   do l=-inm,inm
    ih=h
    ik=k
    il=l

! skip the origin
    if ((ih**2+ik**2+il**2).ne.0) then

! reduce to smallest integers to avoid overlap
! of indices, such as (111) and (222)
     hkl(1)=ih
     hkl(2)=ik
     hkl(3)=il
     call IndexReduce(hkl)

! transform to cartesian coordinates
     g(1)=float(hkl(1))
     g(2)=float(hkl(2))
     g(3)=float(hkl(3))
     ih = hkl(1)
     ik = hkl(2)
     il = hkl(3)

! crystal A
     call TransSpace(cellA,g,r,sp,'c')
     call NormVec(cellA,r,'c')

! apply viewing tansformation
     rr = matmul(M,r)

! compute stereographic projection coordinates
     xst=CX+CRad*rr(1)/(1.0+abs(rr(3)))
     yst=CY+CRad*rr(2)/(1.0+abs(rr(3)))
     cr=1
     if (rr(3).gt.negthresh) then
      call PS_filledcircle(xst,yst,0.015/PS % psscale,0.0)
      nn = .TRUE.
      call DumpIndices(PS,cellA%hexset,sp,ih,ik,il,cr,xst,yst,nn)
     else if (topbot) then
      call PS_circle(xst,yst,0.035/PS % psscale)
      nn = .FALSE.
      call DumpIndices(PS, cellA%hexset,sp,ih,ik,il,cr,xst,yst,nn)
     end if

! crystal B
!    call TransCoor(cellB,dble(matmul(cellB % rsm,g)),dgg,dble(TT),sp,'on')
     call TransSpace(cellB,g,r,sp,'c')
     call TransCoor(cellB,dble(r),dgg,dble(TT),sp,'on')
     gg = sngl(dgg)
     call NormVec(cellB,gg,'c')

! apply viewing tansformation
     rr = matmul(M,gg)

! compute stereographic projection coordinates
     xst=CX+CRad*rr(1)/(1.0+abs(rr(3)))
     yst=CY+CRad*rr(2)/(1.0+abs(rr(3)))
     cr=2
     if (rr(3).gt.negthresh) then
      call PS_filledsquare(xst,yst,0.035/PS % psscale,0.0)
      nn = .TRUE.
      call DumpIndices(PS, cellB%hexset,sp,ih,ik,il,cr,xst,yst,nn)
     else if (topbot) then
      call PS_square(xst,yst,0.050/PS % psscale)
      nn = .FALSE.
      call DumpIndices(PS, cellB%hexset,sp,ih,ik,il,cr,xst,yst,nn)
     end if
    end if
   end do 
  end do 
 end do 

! close Postscript file
 call PS_closefile(PS)

end program EMorient

!--------------------------------------------------------------------------
!
! SUBROUTINE: LocalDrawFrame
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw the page layout
!
!> @param PS Postscript structure
!> @param CX x-center coordinate
!> @param CY y-center coordinate
!> @param CRad circle radius
!> @param iview viewing direction indices
!> @param sp drawing space
!> @param cellA unit cell structure A
!> @param cellB unit cell structure B
!> @param orel orientation relation
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date  4/16/13 MDG 3.0 rewrite 
!--------------------------------------------------------------------------
subroutine LocalDrawFrame(PS,CX,CY,CRad,iview,sp,cellA,cellB,orel)

use local
use typedefs
use io
use postscript

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)	:: PS
real(kind=sgl),INTENT(IN)               :: CX, CY, CRad
integer(kind=irg),INTENT(INOUT)		:: iview(3)
character(1),INTENT(IN)		       	:: sp
type(unitcell)               		:: cellA,cellB
type(orientation),INTENT(IN)  		:: orel

character(17)      			:: str
character(12)      			:: instr
integer(kind=irg)            		:: hkl(3)

 call PS_newpage(PS,.FALSE.,'Stereographic Projection')
 call PS_setlinewidth(0.016)
 call PS_circle(CX,CY,CRad)
 call PS_setlinewidth(0.004)
 call PS_line(CX-CRad,CY,CX+CRad,CY)
 call PS_line(CX,CY-CRad,CX,CY+CRad)
 call PS_setfont(PSfonts(2),0.08)
 call PS_text(CX-CRad-0.07,CY-0.025,'A')
 call PS_text(CX+CRad+0.03,CY-0.025,'B')
 call PS_text(CX-0.03,CY-CRad-0.09,'M''')
 call PS_text(CX-0.03,CY+CRad+0.07,'M"')
 call PS_setfont(PSfonts(2),0.12/PS % psscale)
 call PS_text(0.35,8.30,'Crystal A : '//cella % fname)
 call PS_filledcircle(0.0,8.30,0.015,0.0)
 call DumpIndices(PS,cellA%hexset,sp,0,0,0,1,0.0,8.30,.TRUE.)
 call PS_setfont(PSfonts(2),0.12)
 call PS_text(0.35,8.10,'Crystal B : '//cellb % fname)
 call PS_filledsquare(0.0,8.10,0.035,0.0)
 call DumpIndices(PS,cellB%hexset,sp,0,0,0,2,0.0,8.10,.TRUE.)
 call PS_setfont(PSfonts(2),0.12)
 call IndexString(cellA%hexset,instr,iview,'d')
 call PS_text(0.0,7.90,'Viewing Direction '//instr//' [A]')

 if (sp.eq.'d') then 
  str='direct space'
 else
  str='reciprocal space'
 endif
 call PS_text(0.0,7.70,'Projection of '//str)

 call PS_text(CX,8.20,'Orientation Relation ')
 hkl(1:3)=int(orel % gA(1:3))

 call IndexString(cellA%hexset,instr,hkl,'r')
 call PS_text(CX,8.00,'\(hkl\) : ')
 call PS_text(CX+0.4,8.00,'A-'//instr)
 hkl(1:3)=int(orel % gB(1:3))

 call IndexString(cellB%hexset,instr,hkl,'r')
 call PS_text(CX+0.9,8.00,'|| B-'//instr)

! Space=.True.
 hkl(1:3)=int(orel % tA(1:3))
 call IndexString(cellA%hexset,instr,hkl,'d')
 call PS_text(CX,7.80,'[uvw] : ')
 call PS_text(CX+0.4,7.80,'A-'//instr)
 hkl(1:3)=int(orel % tB(1:3))
 call IndexString(cellB%hexset,instr,hkl,'d')
 call PS_text(CX+0.9,7.80,'|| B-'//instr)
 
end subroutine LocalDrawFrame

