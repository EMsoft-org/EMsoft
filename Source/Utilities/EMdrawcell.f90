! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMdrawcell.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMdrawcell 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Draw a unit cell
!
!> @detail This is not meant to be a very good drawing program; it is just a simple illustration
!> of how one can use crystallographic information to make structure drawings.
!
!> @todo Reciprocal space drawing has not yet been implemented.
! 
!> @date  10/13/98 MDG 1.0 original
!> @date  05/22/01 MDG 2.0 f90
!> @date  04/16/13 MDG 3.0 rewrite

!--------------------------------------------------------------------------
program EMdrawcell 

use local
use typedefs
use io
use crystal
use HDFsupport
use symmetry
use graphics
use postscript
use math
use constants
use files
        
integer(kind=irg),parameter             :: n=1000
character(1)                            :: sp
character(3)                            :: acol(n)
        
real(kind=sgl)                          :: p(4),q(4),xmax,x(n),y(n),z(n),x1,y1,z1,asize(n),M(4,4),VD,diam, io_real(3)
integer(kind=irg)                       :: idx(n),iview(3),iform, io_int(3), imanum
character(fnlen)                        :: progname, progdesc, gname
type(unitcell),pointer                  :: cell
logical                                 :: loadingfile
type(postscript_type)                   :: PS

interface
        subroutine LocalDrawFrame(PS,cell,iview,sp,CX,CY)

        use local
        use typedefs
        use io
        use postscript

        IMPLICIT NONE

        type(postscript_type),INTENT(INOUT)     :: PS
        type(unitcell),pointer          :: cell
        integer(kind=irg),INTENT(INOUT) :: iview(3)             !< viewing direction
        character(1),INTENT(IN)         :: sp                   !< drawing space character
        real(kind=sgl)                  :: CX, CY               !< center of page		
        end subroutine LocalDrawFrame
end interface

 progname = 'EMdrawcell.f90'
 progdesc='Draw one or more unit cells in perspective mode'
 call EMsoft(progname, progdesc)
 
 allocate(cell)

 cell % SG % SYM_reduce=.TRUE.
 CX=7.0
 CY=7.0

! read crystal information
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = gname
 call CrystalData(cell)

! real space or reciprocal space
 sp = 'd'
! call GetDrawingSpace(sp)
! if (sp.ne.'d') then
!   call Message('Reciprocal space drawings not implemented in this version', frm = "(/A/)")
!   stop
! end if

! create all atoms
 call CalcPositions(cell,'m')

! Viewing Distance = 3 times CX = 6 times xmax
 call ReadValue(' Viewing distance [nm] : ',io_real, 1)
 VD = io_real(1)

! viewing direction
 call GetViewingDirection(cell%hexset,iview)

! create transformation matrix
 call ComputeViewTrans(cell,iview,M,VD)

! open PostScript file
 imanum = 1
 call PS_openfile(PS, progdesc, imanum)

! write text and draw box
 call LocalDrawFrame(PS,cell,iview,sp,CX,CY)

! draw unit cell outline first
! which radii should be used for the drawing ?
 call ReadValue(' Use ionic radii (1) or metallic (2) :', io_int, 1)
 iform = io_int(1)

! then get all atom coordinates
 icnt=0
 xmin=100.0
 xmax=-100.0
 ymin=100.0
 ymax=-100.0
 do i=1,cell % ATOM_ntype
  do j=1,cell%numat(i)
   p=(/sngl(cell%apos(i,j,1)),sngl(cell%apos(i,j,2)),sngl(cell%apos(i,j,3)),1.0/)
   q = matmul(p,M)
   x1=VD*q(1)/q(4) 
   y1=VD*q(2)/q(4) 
   z1=VD*q(3)/q(4) 
   x1 = VD*x1/z1
   y1 = VD*y1/z1
   icnt=icnt+1
   x(icnt)=0.5*CX+2.5*x1
   y(icnt)=0.5*CY+2.5*y1
   if (x(icnt).lt.xmin) xmin=x(icnt)
   if (x(icnt).gt.xmax) xmax=x(icnt)
   if (y(icnt).lt.ymin) ymin=y(icnt)
   if (y(icnt).gt.ymax) ymax=y(icnt)
   z(icnt)=z1
   if (iform.eq.1) then 
    asize(icnt)=ATOM_SPradii(cell % ATOM_type(i))
   else
    asize(icnt)=ATOM_MTradii(cell % ATOM_type(i))
   endif
   acol(icnt)=ATOM_color(cell % ATOM_type(i))
  end do
 end do

! shift the drawing back to the center of the page
 xmid = (xmax+xmin)*0.5
 ymid = (ymax+ymin)*0.5
 shx = 0.5*CX - xmid
 shy = 0.5*CY - ymid
 do i=1,icnt
  x(i) = x(i) + shx
  y(i) = y(i) + shy
 end do

! rank according to distance from observer
! draw atoms/relpoints in reverse order (farthest first)
 call SPSORT(z,icnt,idx,-1,ier)

 do i=1,icnt
  j=idx(i) 
  diam = asize(j)
  call PS_sphere(x(j),y(j),diam,acol(j))
 end do 

! close PostScript file
 call PS_closefile(PS)

end program EMdrawcell

!--------------------------------------------------------------------------
!
! SUBROUTINE:LocalDrawFrame
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief format the output page
!
!> @param iview viewing direction
!> @param sp drawing space
!> @param CX X-size of drawing area
!> @param CY Y-size of drawing area (inches)

!> @date   10/13/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date  4/16/13 MDG 3.0 rewrite
!--------------------------------------------------------------------------
subroutine LocalDrawFrame(PS,cell,iview,sp,CX,CY)

use local
use typedefs
use io
use postscript

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)     :: PS
type(unitcell),pointer          :: cell
integer(kind=irg),INTENT(INOUT) :: iview(3)             !< viewing direction
character(1),INTENT(IN)         :: sp                   !< drawing space character
real(kind=sgl)                  :: CX, CY               !< center of page		
character(12)                   :: instr
character(17)                   :: str

 if (sp.eq.'d') then 
  call PS_newpage(PS,.FALSE.,'Crystal Structure Drawing')
 else
  call PS_newpage(PS,.FALSE.,'Reciprocal Lattice Drawing')
 endif
 call PS_setlinewidth(0.012)
 call PS_drawrect(0.0,0.0,CX,CY)
 call PS_setlinewidth(0.008)
 call PS_setfont(PSfonts(2),0.12/PS % psscale)
 call PS_cellinfo(PS,cell,0.0,8.3)
 call IndexString(cell%hexset,instr,iview,'d')
 call PS_text(CX*0.5,8.14,'Viewing Direction '//instr)
 if (sp.eq.'d') then 
  str='direct space'
 else
  str='reciprocal space'
 endif
 call PS_text(CX*0.5,8.00,'Drawing of '//str)

end subroutine LocalDrawFrame
