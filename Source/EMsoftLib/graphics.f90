! ###################################################################
! Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:graphics.f90
!--------------------------------------------------------------------------
!
! MODULE: graphics
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief graphics output routines
!
!> @details This contains all graphics output routines, as well as a contour plot module;
!>  Many of these are legacy code that was ported from Pascal to f90 by MDG; much of
!> this code was written in the 1980's as part of MDG's Ph.D. thesis research.
!
!> @todo Instead of using these routines, it might make more sense to use a standard
!> graphics library of some kind.
! 
!> @date 1/5/99 (and long before that)  MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 dded kind support
!--------------------------------------------------------------------------

module graphics

use local
use typedefs

contains
!--------------------------------------------------------------------------
!
! SUBROUTINE: ProjectionMatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief constuct a projection matrix for a given viewing direction
!
!> @details viewing direction is defined as a direct space direction [uvw]
!  
!> @param cell unit cell pointer
!> @param iview direction indices
!> @param M 3x3 transformation matrix
! 
!> @date   10/20/98 MDG 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   06/0914  MDG 3.0 added cell argument
!--------------------------------------------------------------------------
recursive subroutine ProjectionMatrix(cell,iview,M)
!DEC$ ATTRIBUTES DLLEXPORT :: ProjectionMatrix

use crystal

IMPLICIT NONE

type(unitcell)        	               :: cell
real(kind=sgl),INTENT(OUT)        	:: M(3,3)				!< output transformation matrix
integer(kind=irg),INTENT(IN)     	:: iview(3)				!< input viewing direction indices

real(kind=sgl)        			:: g(3),r(3),q(3),qmin	!< auxiliary variables
integer(kind=irg)     			:: i,imin				!< auxiliary variables

 g=float(iview)
 q=(/ 0.0,0.0,0.0 /)
 call TransSpace(cell,g,r,'d','c')
 call NormVec(cell,r,'c')

! the direction with the smallest direction cosine
! will be put parallel to the horizontal axis of the projection
 qmin=1.0
 do i=1,3
  if (abs(r(i)).lt.qmin) then
   qmin=abs(r(i))
   imin=i
  endif
 end do
 q(imin)=1.0

! cross rxq to get the y-direction.
 call CalcCross(cell,r,q,g,'c','c',0)
 call NormVec(cell,g,'c')

! cross gxr to get the x-direction.
 call CalcCross(cell,g,r,q,'c','c',0)
 call NormVec(cell,q,'c')

! fill the projection matrix
 M(1,1:3)=q(1:3)
 M(2,1:3)=g(1:3)
 M(3,1:3)=r(1:3)

end subroutine
!--------------------------------------------------------------------------
!
! SUBROUTINE: GetViewingDirection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief get theindices of the viewing direction
!
!> @details viewing direction is defined as a direct space direction [uvw]
!  
!> @param hexset hexagonal setting logical
!> @param iview direction indices
! 
!> @date   10/20/98 MDG 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 modified IO
!> @date   06/09/14 MDG 4.0 added hexset argument
!--------------------------------------------------------------------------
recursive subroutine GetViewingDirection(hexset,iview)
!DEC$ ATTRIBUTES DLLEXPORT :: GetViewingDirection

use crystal
use postscript
use io

IMPLICIT NONE

logical,INTENT(IN)                    :: hexset
integer(kind=irg),INTENT(OUT)    	:: iview(3)	!< direction indices in three index notation
integer(kind=irg)			:: vview(4)	!< to transform between 3 and 4 index notation
integer(kind=irg)			:: io_int(4)	!< used for IO


! viewing direction (watch for hexagonal indices !)
 if (hexset.eqv..FALSE.) then
  call ReadValue('Enter viewing direction indices [uvw] : ', io_int,3)
  iview(1:3) = io_int(1:3)
 else
  call ReadValue('Enter viewing direction indices [uvtw] : ', io_int, 4)
  vview(1:4) = io_int(1:4)
  call MilBrav(iview,vview,'43')
 endif
 call IndexReduce(iview)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetDrawingSpace
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief real space or reciprocal space drawing
!
!> @param sp space character 'd' or 'r'
! 
!> @date   10/20/98 MDG 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 modified IO 
!--------------------------------------------------------------------------
recursive subroutine GetDrawingSpace(sp)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDrawingSpace

use io

IMPLICIT NONE

character(1),INTENT(OUT)   :: sp		!< character 'd' or 'r'

call ReadValue('Real Space (d) or reciprocal space (r) : ', sp,'(A1)')

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: StereoProj
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a stereographic projection
!
!> details makes extensive use of PostScript routines
!
!> @param PS Postscript structure
!> @param cell unit cell pointer
!> @param sp space character 'd' or 'r'
!> @param iview projection direction
!> @param hm maximum h index value
!> @param km maximum k index value
!> @param lm maximum l index value
!> @param topbot logical to XXX
! 
!> @date   10/20/98 MDG 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 modified IO 
!> @date   06/09/14 MDG 4.0 added PS and cell arguments
!--------------------------------------------------------------------------
recursive subroutine StereoProj(PS,cell,sp,iview,hm,km,lm,topbot)
!DEC$ ATTRIBUTES DLLEXPORT :: StereoProj

use crystal
use symmetry
use math
use postscript
use io

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)   :: PS
!f2py intent(in,out) ::  PS
type(unitcell)        	               :: cell
character(1),INTENT(IN)     		:: sp			!< space character 'd' or 'r'
integer(kind=irg),INTENT(INOUT)	:: iview(3)		!< viewing direction
!f2py intent(in,out) ::  iview
integer(kind=irg),INTENT(IN)		:: hm, km, lm		!< maximum h,k,l indices to be included in drawing
logical,INTENT(IN)          		:: topbot		!< logical for XXX

logical					:: nn			!< auxiliary logical	
logical,allocatable 			:: z(:,:,:)		!< auxiliary logical array
real(kind=sgl)   			:: rr(3),g(3),r(3),M(3,3), CX, CY, CRad,xst,yst	!< auxiliary variables
real(kind=sgl),parameter   		:: negthresh = -0.000001				!< threshold parameter
integer(kind=irg) 			:: i,h,k,l,hkl(3),hkil(4),cr,hh,kk,ll,num,hkm		!< auxiliary integers
integer(kind=irg)	               :: itmp(48,3)		!< array used for family computations etc

! 20cm radius projection circle [inches]
 CRad = 3.937
 CX = 3.25
 CY = 3.5
 
! create transformation matrix
 call ProjectionMatrix(cell,iview,M)
 
! write text and draw projection circle
 call DrawSPFrame(PS,cell,CX, CY, CRad, iview, sp)

! loop over families
! make sure that the arrays are big enough for the hexagonal case...
if (cell%hexset.eqv..TRUE.) then 
  hkm = abs(hm)+abs(km)
  allocate(z(-hkm:hkm,-hkm:hkm,-lm:lm))
else
 allocate(z(-hm:hm,-km:km,-lm:lm))
end if

z = .FALSE.
 do hh=-hm,hm
  do kk=-km,km
   do ll=-lm,lm
    if (z(hh,kk,ll).eqv..TRUE.) cycle
! determine the family members
    if ((cell%hexset.eqv..TRUE.).AND.(sp.eq.'d')) then
     hkil= (/ hh,kk,-(hh+kk),ll /)
     call MilBrav(hkl,hkil,'43')
    else
     hkl= (/ hh,kk,ll /)
    end if
    if ((hh**2+kk**2+ll**2).ne.0) then
     call IndexReduce(hkl)
     call CalcFamily(cell,hkl,num,sp,itmp)
! loop over all points and draw projection+label
      do i=0,num-1
       h=itmp(i,1)
       k=itmp(i,2)
       l=itmp(i,3)
       hkl(1:3)=itmp(i,1:3)
       if ((h.le.hm).and.(k.le.km).and.(l.le.lm)) then
! reduce to smallest integers to avoid overlap
! of indices, such as (111) and (222)
         call IndexReduce(hkl)
         h=hkl(1)
         k=hkl(2)
         l=hkl(3)
         g=float( (/h,k,l/) )
         call TransSpace(cell,g,r,sp,'c')
         call NormVec(cell,r,'c')
! apply viewing tansformation
         rr = matmul(M,r)
! compute stereographic projection coordinates
         xst=CX+CRad*rr(1)/(1.0+abs(rr(3)))
         yst=CY+CRad*rr(2)/(1.0+abs(rr(3)))
         cr=1
         if (z(h,k,l).eqv..FALSE.) then
          if (rr(3).gt.negthresh) then
           call PS_filledcircle(xst,yst,0.015/PS % psscale,0.0)
           nn = .TRUE.
           call DumpIndices(PS,cell%hexset,sp,h,k,l,cr,xst,yst,nn)
          else if (topbot) then
           call PS_circle(xst,yst,0.035/PS % psscale)
           nn = .FALSE.
           call DumpIndices(PS,cell%hexset,sp,h,k,l,cr,xst,yst,nn)
          end if
         end if
         z(h,k,l) = .TRUE.
       end if
      end do
    end if
   end do 
  end do 
 end do 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: ComputeViewTrans
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the viewing transformation matrix
!
!> @details Based on sections 13.5.1-5 in "Computer Graphics: Systems and Concepts", by R. Salmon and M. Slater, 
!> pp. 397-408, Addison-Wesley 1987
!
!> @param cell unit cell pointer
!> @param iview viewing direction
!> @param M transformation matrix
!> @param VD viewing distance [nm]
! 
!> @date   10/20/98 MDG 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 modified IO
!> @date   06/09/14 MDG 4.0 added cell argument 
!--------------------------------------------------------------------------
recursive subroutine ComputeViewTrans(cell,iview,M,VD)
!DEC$ ATTRIBUTES DLLEXPORT :: ComputeViewTrans

use crystal

IMPLICIT NONE

type(unitcell)        	               :: cell
integer(kind=irg),INTENT(IN)		:: iview(3)		!< viewing direction indices
real(kind=sgl),INTENT(OUT)		:: M(4,4)		!< transformation matrix
real(kind=sgl),INTENT(IN)		:: VD			!< viewing distance in [nm]

real(kind=sgl)                        :: p(3),n(3),u(3),v(3),pmin		!< auxiliary reals
integer(kind=irg)                     :: i,j,imin				!< auxiliary integers


 n=float(iview)                          ! VPN View Plane Normal
 call TransSpace(cell,n,p,'d','c')       ! convert to cartesian
 call NormVec(cell,p,'c')                ! and normalize
 
pmin=1.1                                ! select vector v normal to VPN in projection
 do i=1,3
  if (abs(p(i)).lt.pmin) then
   pmin=abs(p(i))
   imin=i
  end if
 end do
 v= (/0.0,0.0,0.0/)
 v(imin)=1.0
 call CalcCross(cell,v,n,u,'c','c',0)         ! compute u = v x VPN
 call NormVec(cell,u,'c')                     ! parallel to the U axis and normalized
 n=-p
 call CalcCross(cell,u,n,v,'c','c',0)         ! the third vector, right handed !!
 call NormVec(cell,v,'c')                     ! and normalize

! and store the vectors in the M matrix
 do i=1,3
  M(i,1)=u(i)
  M(i,2)=v(i)
  M(i,3)=n(i)
  M(i,4)=0.0
  M(4,i)=0.0
 end do
 M(4,4)=1.0

! apply the viewing distance for perspective projections
 do j=1,3
  M(4,1)=M(4,1)-p(j)*u(j)*VD
  M(4,2)=M(4,2)-p(j)*v(j)*VD
  M(4,3)=M(4,3)-p(j)*n(j)*VD
 end do
 
end subroutine

!###################################################################
!###################################################################
!###################################################################
!--------------------------------------------------------------------------
!
! SUBROUTINE: axonometry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a wireframe or phong-shaded surface
!
!> @details This is based on a Pascal routine of the same name 
!> that was written in 1987 by MDG and K. Mols, while graduate
!> students at the Catholic University of Leuven, Belgium.
!>
!> Given that this is somewhat older code, for now we will not extensively 
!> document each subroutine.
!>
!> Subroutines:  initparameterset; setmenu, drawing, initframe, axonometry
! 
!> @todo comment the axonometry subroutines
!
!> @date   10/20/87 MDG/KM 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 modified IO 
!> @date   06/09/14 MDG 4.0 made AXO and PS as arguments instead of globals
!--------------------------------------------------------------------------
recursive subroutine initparameterset(AXO)
!DEC$ ATTRIBUTES DLLEXPORT :: initparameterset

use postscript

IMPLICIT NONE

type(axonotype),INTENT(INOUT)	:: AXO
!f2py intent(in,out) ::  AXO

 AXO%xi=1
 AXO%yi=2
 AXO%beta=30
 AXO%visibility=.FALSE.
 AXO%scle=1.0
 AXO%xmod=1
 AXO%ymod=1
 AXO%vscle=1.0
end subroutine
! ###################################################################
recursive subroutine setmenu(AXO,what)
!DEC$ ATTRIBUTES DLLEXPORT :: setmenu
use postscript

IMPLICIT NONE

type(axonotype),INTENT(INOUT)	:: AXO
!f2py intent(in,out) ::  AXO
character(*),INTENT(IN)       :: what

! clear the screen
! call system('clear')


 write (*,'(3(/))')
 if (what.eq.'change_xi') then 
  write (*,"('1- xi        : ')",advance="no") 
  read (*,"(I8)") AXO%xi
 else 
  write (*,"('1- xi        : ',i4)") AXO%xi
 end if
 if (what.eq.'change_yi') then 
  write (*,"('2- yi        : ')",advance="no") 
  read (*,"(I8)") AXO%yi
 else 
  write (*,"('2- yi        : ',i4)") AXO%yi
 end if
 if (what.eq.'change_beta') then 
  write (*,"('3- beta      : ')",advance="no") 
  read (*,"(I8)") AXO%beta
 else 
  write (*,"('3- beta      : ',i4)") AXO%beta
 end if
 if (what.eq.'change_vis') then 
  AXO%visibility = .not. AXO%visibility 
 end if
 write (*,"('4- draw mode :')",advance="no") 
 if (AXO%visibility) then 
  write (*,"(' Phong shading')")
 else 
  write (*,"(' Wireframe')")
 end if
 if (what.eq.'change_scale') then 
  write (*,"('5- scale     : ')",advance="no") 
  read (*,*) AXO%scle
 else 
  write (*,"('5- scale     : ',f10.4)") AXO%scle
 end if
 if (what.eq.'change_xmod') then 
  write (*,"('6- xmod      : ')",advance="no") 
  read (*,"(I8)") AXO%xmod
 else 
  write (*,"('6- xmod      : ',i4)") AXO%xmod
 end if
 if (what.eq.'change_ymod') then 
  write (*,"('7- ymod      : ')",advance="no") 
  read (*,"(I8)") AXO%ymod
 else 
  write (*,"('7- ymod      : ',i4)") AXO%ymod
 end if
 if (what.eq.'change_vscale') then 
  write (*,"('8- vscale    : ')",advance="no") 
  read (*,*) AXO%vscle
 else 
  write (*,"('8- vscale    : ',f10.4)") AXO%vscle
 end if
 write (*,"('D- make drawing')") 
 write (*,"('E- Export PostScript')") 
 write (*,"('Q- quit routine')") 
 write (*,'(3(/))')
end subroutine
! ###################################################################

recursive subroutine drawing(AXO,PS,AX,progdesc,imanum,zz,inten,nx,ny,dmode,axname)
!DEC$ ATTRIBUTES DLLEXPORT :: drawing

use postscript
use constants
use io

IMPLICIT NONE

type(axonotype),INTENT(INOUT)	            :: AXO
!f2py intent(in,out) ::  AXO
type(postscript_type),INTENT(INOUT)       :: PS
!f2py intent(in,out) ::  PS
type(axistype),INTENT(INOUT)              :: AX
!f2py intent(in,out) ::  AX
character(fnlen),INTENT(IN)               :: progdesc
integer(kind=irg),INTENT(INOUT)           :: imanum
!f2py intent(in,out) ::  imanum
integer(kind=irg),INTENT(IN)              :: nx
integer(kind=irg),INTENT(IN)              :: ny
real(kind=sgl),INTENT(IN)                 :: zz(nx,ny)
real(kind=sgl),INTENT(INOUT)              :: inten(nx,ny)
!f2py intent(in,out) ::  inten
character(6),INTENT(IN)                   :: dmode
character(fnlen),INTENT(IN)               :: axname


integer(kind=irg)   :: i,j,kk,ip,jp
logical             :: pensw,ipp
real(kind=sgl)      :: alfa,v1,v2,w1,w2,w3,n(3),e(3),l(3),h(3),pointx,pointy,xr,yr,xp,yp,xi,yi,u1,u2, &
                       tv(4,2),pp(4,2),k_a,k_d,k_s,In_a,In_p,inmax,pn,zero,nl,hn,sx,sy, &
                       xmin,xmax,ymin,ymax,s,t,u,bb

    
! define the geometrical transformation to the image reference frame
 xi = float(AXO%xi)
 yi = float(AXO%yi)
 if (AXO%xi.eq.0) then 
  alfa=sngl(cPi)/2.0
 else 
  alfa=atan(yi/xi)
 end if
 v1=-sin(alfa)
 v2=cos(alfa)
 u1=sin(sngl(cPi)*AXO%beta/180.0)
 u2=cos(sngl(cPi)*AXO%beta/180.0)
 w1=-u1*v2
 w2=u1*v1
 w3=u2
! initalize the Postscript file
! open a complete file for online mode, else
! open a temporary file for export mode.
 if (dmode.eq.'export') then 
  open(UNIT=psunit,FILE=trim(EMsoft_toNativePath(axname)),STATUS='UNKNOWN',FORM='FORMATTED')
 else
  PS%psname = 'tmp.ps'
  call PS_openfile(PS, progdesc, imanum, .TRUE.)  ! PS, progdesc, imanum, dontask
  PS%pspage = 0
 end if
 write (psunit,*) 'gsave'
! set the origin
 write (psunit,"(f8.3,f8.3,' T')") AX%xll-1.0,AX%yll-1.0
! determine the viewing window boundaries
 sx = AXO%scle/20.0
 sy = AXO%scle/20.0
 write (psunit,"(E14.6,' ',E14.6,' scale')") sx,sy
! clip the drawing, so that none of it appears outside of the square
 xmin = -AX%axw/2.0/sx
 xmax =  AX%axw/2.0/sx
 ymin = -AX%axw/2.0/sy
 ymax =  AX%axw/2.0/sy
 write (psunit,*) '1.0 setgray'
 call PS_closepath
 call PS_move(xmin,ymin)
 call PS_draw(xmax,ymin)
 call PS_draw(xmax,ymax)
 call PS_draw(xmin,ymax)
 call PS_clippath
 write (psunit,*) '0.0 setgray'
! the next line MUST be present, otherwise the clippath will be
! visible on the drawing !
 call PS_newpath
 if (AXO%visibility) then
! Phong shading
! first compute the bisector unit vector for Phong shading
! (see Computer Graphics: Systems and Concepts, by R. Salmon and
! M. Slater, Addison-Wesley, 1987, p. 418)
  call Message('Computing bisector vector', frm = "(A)")
  zero = 0.0
! eye direction
  e(1) = float(AXO%xi)
  e(2) = float(AXO%yi)
  e(3) = tan(sngl(cPi)*AXO%beta/180.0)*sqrt(e(1)**2+e(2)**2)
! light source direction
  l(1) = 5.0
  l(2) = -10.0
  l(3) = 50.0
! sum and normalize
  do i=1,3
   h(i) = e(i)+l(i)
  end do
  s = 0.0
  t = 0.0
  u = 0.0
  do i=1,3
   s = s + h(i)**2
   t = t + l(i)**2
   u = u + e(i)**2
  end do
  s = 1.0/sqrt(s)
  t = 1.0/sqrt(t)
  u = 1.0/sqrt(u)
  h = h*s
  l = l*t
  e = e*u
! compute all the polygon normals (triangles)
  call Message('Computing polygon normals and intensities', frm = "(A)")
  bb = AXO%vscle
  inmax = -10000.0
  In_a = 0.2
  In_p = 1.0
  k_a  = 0.4
  k_d  = 0.3
  k_s  = 1.0-k_a-k_d
  pn   = 2.0
  do i=1,AXO%countx-1
   ip = i+1
   do j=1,AXO%county-1
    jp = j+1
! average normal 
    n(1)= bb*(zz(i,j)-zz(ip,j)-zz(ip,jp)+zz(i,jp))
    n(2)= bb*(zz(i,j)+zz(ip,j)-zz(ip,jp)-zz(i,jp))
    n(3) = 2.0
! normalize normals
    s = n(1)**2+n(2)**2+n(3)**2
    s = 1.0/sqrt(s)
    n = n*s
! compute Phong shading 
!   I = I_ak_a + I_p[k_d N.L + k_s (H.N)^n]
    nl=0.0
    hn=0.0
    do kk=1,3
     nl = nl + n(kk)*l(kk)
     hn = hn + n(kk)*h(kk)
    end do
! take only positive angles
    nl = max(nl,zero)
    hn = max(hn,zero)
! and compute the intensity (keep track of the maximum)
    inten(i,j)=In_a*k_a +  In_p*(k_d*nl + k_s * hn**pn)
    inmax = max(inmax,inten(i,j))
   end do
  end do
! normalize scattered intensities to maximum
  inmax = 1.0/inmax
  do i=1,AXO%countx
   do j=1,AXO%county
    inten(i,j) = max(zero,inten(i,j)*inmax)
   end do
  end do
  call Message('Producing Postscript output', frm = "(A)")
  call PS_newpath
  do i=1,AXO%countx-1
   ip = i+1
   do j=1,AXO%county-1
    jp = j+1
! square 
    tv(1,1)=AXO%grid*float(i-AXO%countx/2)
    tv(1,2)=AXO%grid*(j-AXO%county/2)
    tv(2,1)=AXO%grid*float(ip-AXO%countx/2)
    tv(2,2)=AXO%grid*(j-AXO%county/2)
    tv(3,1)=AXO%grid*float(ip-AXO%countx/2)
    tv(3,2)=AXO%grid*(jp-AXO%county/2)
    tv(4,1)=AXO%grid*float(i-AXO%countx/2)
    tv(4,2)=AXO%grid*(jp-AXO%county/2)
    pp(1,1)=v1*tv(1,1)+v2*tv(1,2)
    pp(1,2)=w1*tv(1,1)+w2*tv(1,2)+w3*AXO%vscle*zz(i,j)
    pp(2,1)=v1*tv(2,1)+v2*tv(2,2)
    pp(2,2)=w1*tv(2,1)+w2*tv(2,2)+w3*AXO%vscle*zz(ip,j)
    pp(3,1)=v1*tv(3,1)+v2*tv(3,2)
    pp(3,2)=w1*tv(3,1)+w2*tv(3,2)+w3*AXO%vscle*zz(ip,jp)
    pp(4,1)=v1*tv(4,1)+v2*tv(4,2)
    pp(4,2)=w1*tv(4,1)+w2*tv(4,2)+w3*AXO%vscle*zz(i,jp)
    write (psunit,*) inten(i,j),' setgray'
    call PS_move(pp(1,1),pp(1,2))
    call PS_draw(pp(2,1),pp(2,2))
    call PS_draw(pp(3,1),pp(3,2))
    call PS_draw(pp(4,1),pp(4,2))
    write (psunit,*) 'F'
   end do
  end do
 else
! wireframe model 
! plot the x-lines
  do i=1,AXO%countx 
   if (mod(i,AXO%xmod).eq.0) then 
    call PS_newpath
    xr=AXO%grid*float(i-AXO%countx/2)
    do j=1,AXO%county
     yr=AXO%grid*(j-AXO%county/2)
     xp=v1*xr+v2*yr
     yp=w1*xr+w2*yr+w3*AXO%vscle*zz(i,j)
     if (j.eq.1) then 
      pensw=.FALSE.
     else 
      pensw=.TRUE.
     end if
     if (.not.pensw) then 
      pointx=xp
      pointy=yp
      ipp=.TRUE.
     else 
      if (ipp) then 
       call PS_move(pointx,pointy)
       ipp=.FALSE.
      end if
      call PS_draw(xp,yp)
     end if
    end do
    write (psunit,*) 'S'
   end if
  end do
  call Message('x-grid completed', frm = "(A)")
! plot y lines 
  do j=1,AXO%county
   if (mod(j,AXO%ymod).eq.0) then 
    call PS_newpath
    yr=AXO%grid*(j-AXO%county/2)
    do i=1,AXO%countx
     xr=AXO%grid*(i-AXO%countx/2)
     xp=v1*xr+v2*yr
     yp=w1*xr+w2*yr+w3*AXO%vscle*zz(i,j)
     if (i.eq.1) then 
      pensw=.FALSE.
     else 
      pensw=.TRUE.
     end if
     if (.not.pensw) then 
      pointx=xp
      pointy=yp
      ipp=.TRUE.
     else
      if (ipp) then 
       call PS_move(pointx,pointy)
       ipp=.FALSE.
      end if
      call PS_draw(xp,yp)
     end if
    end do
    write (psunit,*) 'S'
   end if
  end do
  call Message('y-grid completed', frm = "(A)")
 end if
 if (dmode.eq.'export') then
  close (unit=psunit,status='keep')
 else
  call initframe(AX,'stop ',.FALSE.)
  call PS_closefile(PS)
  call Message('Use a postscript viewing program to display the file '//PS%psname, frm = ("A"))
 end if
end subroutine drawing
! ###################################################################
recursive subroutine initframe(AX,mode,db)
!DEC$ ATTRIBUTES DLLEXPORT :: initframe
 
use postscript

IMPLICIT NONE

type(axistype),INTENT(IN)        :: AX
character(5),INTENT(IN)          :: mode
logical,INTENT(IN)               :: db
 
! initialize the viewing window (from (-20,-20) to (120,120))
! (these are user coordinates, chosen so that the actual drawing
! will go from 0 to 100 along both x and y)
! On the output this means that the entire drawing, including
! axis labels and such, will always fit inside a region that
! is axw*axw inches squared. 
!
! For a single drawing on an 8.5*11 inch page with 1 inch margins 
! left and right and top and bottom 2.25 inch margins requires
! axw = 6.5 and (xll,yll) = (1.0,2.25) 
!
 if (mode.eq.'start') then 
  write (psunit,*) 'gsave'
  write (psunit,"(F8.3,f8.3,' T')") AX%xll-1.0,AX%yll-1.0
  write (psunit,"(E14.6,' dup scale')") AX%axw/140.0
! set the origin
  write (psunit,*) '20 20 T'
! define the font (default size is 2.0)
  call PS_setfont(PSfonts(1),2.0)
! draw the main rectangle
  if (db) call PS_drawrect(0.0,0.0,100.0,100.0)
 else
! reset the origin
  write (psunit,*) 'grestore'
 end if
end subroutine

! ###################################################################
recursive subroutine axonometry(AXO,PS,AX,progdesc,imanum,zz,nx,ny,g,axname)
!DEC$ ATTRIBUTES DLLEXPORT :: axonometry
use postscript
use io

IMPLICIT NONE

type(axonotype),INTENT(INOUT)        :: AXO
!f2py intent(in,out) ::  AXO
type(postscript_type),INTENT(INOUT)  :: PS
!f2py intent(in,out) ::  PS
type(axistype),INTENT(INOUT)         :: AX
!f2py intent(in,out) ::  AX
character(fnlen),INTENT(IN)          :: progdesc
integer(kind=irg),INTENT(INOUT)      :: imanum
!f2py intent(in,out) ::  imanum
integer(kind=irg),INTENT(INOUT)      :: nx
!f2py intent(in,out) ::  nx
integer(kind=irg),INTENT(INOUT)      :: ny
!f2py intent(in,out) ::  ny
real(kind=sgl),INTENT(INOUT)         :: zz(nx,ny)
!f2py intent(in,out) ::  zz
real(kind=sgl),INTENT(IN)            :: g
character(fnlen),INTENT(IN)          :: axname

real(kind=sgl)          :: inten(1)
logical                 :: more
character(1)            :: selection 
character(fnlen)        :: dummyname
    
 AXO%countx=nx
 AXO%county=ny
 AXO%grid=g
 call initparameterset(AXO) 

 more = .TRUE.
 do while (more) 
  call setmenu(AXO,'all')
  call ReadValue('Enter selection : ', selection, frm = "(A1)")
  select case (selection)
   case('1'); call setmenu(AXO,'change_xi')
   case('2'); call setmenu(AXO,'change_yi')
   case('3'); call setmenu(AXO,'change_beta')
   case('4'); call setmenu(AXO,'change_vis')
   case('5'); call setmenu(AXO,'change_scale')
   case('6'); call setmenu(AXO,'change_xmod')
   case('7'); call setmenu(AXO,'change_ymod')
   case('8'); call setmenu(AXO,'change_vscale')
   case('d','D')
        dummyname = ''
        call drawing(AXO,PS,AX,progdesc,imanum,zz,inten,nx,ny,'online',dummyname)
   case('e','E'); call drawing(AXO,PS,AX,progdesc,imanum,zz,inten,nx,ny,'export',axname)
   case('q','Q'); more = .FALSE.
   case default;
  end select
 end do
end subroutine
!###################################################################
!###################################################################
!###################################################################


!###################################################################
!###################################################################
!###################################################################
!--------------------------------------------------------------------------
!
! SUBROUTINE: axis
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw an x-y plot
!
!> @details This is based on a Pascal routine of the same name 
!> that was written in 1987 by MDG and K. Mols, while graduate
!> students at the Catholic University of Leuven, Belgium.
!>
!> Given that this is somewhat older code, for now we will not extensively 
!> document each subroutine.
!
!> Original written in Pascal by:   
!>                   Koen Mols
!>                   Dept Metaalkunde en Toegepaste Materiaalkunde
!>                   KU-Leuven
!>                   Belgium
!                   
!>                   version unix 1.1      oct 1987
!    
!>  Converted to Fortran by Marc De Graef, May 2001    
!     
!>   AXIS generates linear and logarithmic x-y plots.
!>   The figure can be scaled according to user-boundaries or, more 
!>   conveniently, in an automatic way.
!>   
!>   
!>   Syntax :  [several options of the Pascal version are incompatible
!>              with Fortran syntax, and have been removed;  axis
!>              now generates PostScript output instead of screen-directed
!>              graphics commands]
!   
!>  subroutine axis(points,xvec,yvec,xmin,xmax,ymin,ymax,
!>                  xautorange,yautorange,xmode,ymode,pmode,mark,
!>                  scalex,scaley,overplot,drawborder);
!>      
!>      
!>  item             type           description
!>
!>  points           INTEGER        number of points to draw
!>  xvec             ARRAY of REAL  x-component of the array of drawing postions
!>  yvec             ARRAY of REAL  y-component of the array of drawing postions
!>  xmin,xmax,
!>  ymin,ymax        REAL           minimum en maximum value of x- and y-component 
!>                                  upon return from axis these vars will contain 
!>                                  the actual drawing boundaries.
!>                                  e.g.   if xmin=0.3 and xmax=73 then a drawing
!>                                         will be generated from 0 to 80, thus
!>                                         xmin--> 0  and xmax--> 80
!>  xautorange,
!>  yautorange       LOGICAL        .TRUE. minimum en maximum values of each component 
!>                                         are internally defined
!>                                  .FALSE. the values passed are being used
!>                                         ( the figure will not be clipped )
!>  xmode,ymode      character*3    kind of representation (LIN,LOG)
!>  pmode            character*3    CON          : continuous line
!>                                  DOT          : markers put at each (x,y) position
!>                                  BAR          : bar-graph (vertical lines)
!>  mark             INTEGER        marker    ( for DOT )  see set_marker
!>                                  line_type ( for CON )  see set_line_style
!>  scalex           character*3    BOT,TOP,NON
!>  scaley           character*3    LEF,RIG,NON
!>  overplot         logical        .FALSE. -> draw frame
!>                                  .TRUE.  -> do not draw frame, but use same scaling
!>  drawborder       logical        .FALSE. -> do not draw the axes
!>                                  .TRUE.  -> draw the axes
!>
!>
!>  NOTES
!>      
!>      When the min and max values of both the coordinates are known (whether
!>      by passing them or by autoranging, further scaling depends on the 
!>      axis_mode :
!>      
!>      a)  linear scaling :
!>      
!>      These values are then used to trim to the actual limits,
!>      member of following set :
!>      
!>                       (0,1,1.5,2,2.5,3,4,6,8,10)
!>      
!>      b)  logarithmic scaling :
!>      
!>      The graph is always trimmed to decade boundaries.
!>  
!> Functions: stringl; power; omag; limit; border; getshift; determinestep;
!> Subroutines:  setticks; setexponent; drawborder; drawfigure; axis
! 
!> @todo comment the axis subroutines
!
!> @date   10/20/87 MDG/KM 1.0 original
!> @date    5/21/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 modified IO 
!-------------------------------------------------------------------------
!  ******************************************************************************
integer recursive function stringl(t)
!DEC$ ATTRIBUTES DLLEXPORT :: stringl

IMPLICIT NONE

character(*)      :: t
integer(kind=irg) :: i

 i=len(t) 
 do while (t(i:i).eq.' ')
  i=i-1
 end do 
 stringl=i
end function
!  *****************************************************************************
real recursive function power(n)
!DEC$ ATTRIBUTES DLLEXPORT :: power
!  raises 10 to the power n
!  this seems silly, but for negative powers this is a simple way to do it

use local

IMPLICIT NONE

real(kind=dbl)       :: q
integer(kind=irg)    :: n
        
 if (n.lt.0) then 
  q = 1.D0/10**(-n)
 else
  q = 10**n
 endif
 power = sngl(q)
end function
!  ******************************************************************************
integer recursive function omag(x)
!DEC$ ATTRIBUTES DLLEXPORT :: omag
!    determines the order of magnitude of a real value x
!    
!                    n-1                n
!    O(r) = n :    10    < abs(r)  <= 10 
!    
! [this could also be done by taking the base 10 logarithm, but this
!  routine is faster]


IMPLICIT NONE

integer(kind=irg)   :: n
real(kind=sgl)      :: x,magn,y 
 
 n=0
 magn=1.0
 y=abs(x)
 if (x.eq.0.0) then 
  omag=-1000
 else 
  if (y.lt.1) then 
   do while (y.le.magn*0.1)
    n=n-1
    magn=magn*0.1
   end do
   omag=n
  else 
   do while (magn.lt.y) 
    n=n+1
    magn=magn*10.0
   end do
   omag=n
  end if
 end if
end function
!  ******************************************************************************
real recursive function limit(l,n,hilo)
!DEC$ ATTRIBUTES DLLEXPORT :: limit

IMPLICIT NONE

integer(kind=irg) :: i,n
real(kind=sgl)    :: l,r,h
character(2)      :: hilo
real(kind=sgl),parameter    :: hl(10) = (/0.0,1.0,1.50,2.0,2.50,3.0,4.0,5.0,6.0,8.0/)

intent(IN)        :: n,hilo
intent(INOUT)     :: l

 l=abs(l)
 r=power(1-n)
 l=l*r
 if (hilo.eq.'hi') then 
  h=10.0 
  do i=10,2,-1
   if (l.le.hl(i)) h=hl(i)
  end do
 else
  h=8.0 
  do i=10,2,-1
   if (l.lt.hl(i)) h=hl(i-1)
  end do
 end if
 limit = h
end function
!  ******************************************************************************
recursive subroutine border(mode,xmin,xmax,n)
!DEC$ ATTRIBUTES DLLEXPORT :: border
!    
!     border determines the world coordinates of the upper and
!                                 n
!     lower bound in the form a*10
! 
!    LOG
!    
!    inputs are the min and max values of a row of reals
!    outputs are the standard borders when a log graph is used
!    
!    caution :  
!    suppose min=10
!    since O(min) = 1  normally the lower border would then be one less = 0
!                      (resulting in an empty decade)
!    therefore O(min*delta) is used, in which delta is a factor that will
!    change min to the first representable real value greater then min
!    so O(min*delta) will then be 2 and lowborder will be 1 
!    
!    LIN
!    
!    inputs are the min and max values of a row of reals
!    outputs are the standard borders that will be used in the
!    drawing
!    n is max(O(min),O(max))
! 

IMPLICIT NONE

real(kind=sgl),parameter      :: delta=1.0000001
integer(kind=irg)             :: omin,omax,n
real(kind=sgl)                :: xmin,xmax,lowborder,highborder
character(3)                  :: mode

 if (mode.eq.'log') then
  lowborder=omag(xmin*delta)-1
  highborder=omag(xmax)
  n=0
 else 
  lowborder =0
  highborder=0
  if (abs(xmax).gt.abs(xmin)) then 
   omax=omag(xmax)
   omin=omag(xmin*delta)
  else 
   omax=omag(xmax*delta)
   omin=omag(xmin)
  end if
!  this multiplication with delta will force the limit (the closest to zero)
!  to jump to a higher order of magnitude if it is a border value
!  e.g.
!  min = 1000    max = 1200
!  normally O(min)=3 and O(max)=4)  resulting in a plot starting from zero
!  (different orders of magn)
!  but  O(min*delta) = 4 so same order of magnitude and other scale then
!  zero used for lower bound
!
  if (omax.ge.omin) then 
   n=omax 
  else 
   n=omin
  end if
! orders of magnitude are different 
  if (omax.ne.omin) then  
   if (xmin.ge.0) highborder=limit(xmax,n,'hi')
   if (xmax.le.0) lowborder=-limit(xmin,n,'hi')
   if (xmax*xmin.lt.0) then 
    if (omax.gt.omin) then 
     highborder=limit(xmax,n,'hi')
     lowborder=-1.0
    else 
     highborder=1.0
     lowborder=-limit(xmin,n,'hi')
    end if
   end if 
! orders of magnitude are the same 
  else 
   if (xmin.ge.0) then 
    lowborder=limit(xmin,n,'lo')
   else 
    lowborder=-limit(xmin,n,'hi')
   end if
   if (xmax.lt.0) then 
    highborder=-limit(xmax,n,'lo')
   else 
    highborder=limit(xmax,n,'hi')
   end if
  end if
! lowborder and highborder are between 0..100 instead of  0..1 
  n=n-1  
 end if
 xmin=lowborder
 xmax=highborder
end subroutine
!  ******************************************************************************
recursive subroutine setticks(cp,d,low,high,ts,cs,cw,ch,sh,ich,typ,sx,sy)
!DEC$ ATTRIBUTES DLLEXPORT :: setticks

use postscript

IMPLICIT NONE

real(kind=sgl)      :: tick2,cp,d,low,high,ts,cs,cw,ch,sh,sx,sy
integer(kind=irg)   :: i,ich
character(4)        :: typ
real(kind=sgl),parameter      :: logtick(9) = (/0.000000000,0.301029996,0.477121255,0.602059991, &
                                                0.698970004,0.778151250,0.845098040,0.903089987,0.954242509/)

 call PS_setfont(PSfonts(2),2.2)
 call PS_setlinewidth(0.001)
! draw the tickmarks for linear scaling on the x-axis
 if (typ.eq.'xlin') then 
  call PS_move(cp,ts)
  call PS_draw(cp,ts+2)
  call PS_stroke
  write (psunit,"(E14.6,E14.6,' scale')") 1.0/sx,1.0/sy
  write (psunit,"(E14.6,' ',E14.6,' M ( ',F5.1,' ) show')") sx*(cp-1.5*sh),sy*cs,cp
  write (psunit,"(E14.6,E14.6,' scale')") sx,sy
  tick2=cp+d/2
  if ((tick2.ge.low).and.(tick2.le.high)) then
   call PS_move(tick2,ts) 
   call PS_draw(tick2,ts+2) 
   call PS_stroke
   write (psunit,"(E14.6,E14.6,' scale')") 1.0/sx,1.0/sy
   write (psunit,"(E14.6,' ',E14.6,' M ( ',F5.1,' ) show')") sx*(tick2-1.5*sh),sy*cs,tick2
   write (psunit,"(E14.6,E14.6,' scale')") sx,sy
  end if
 end if
! draw the tickmarks for logarithmic scaling on the x-axis
 if (typ.eq.'xlog') then 
  if (ts.eq.100) then
   call PS_move(cp,ts)
   call PS_draw(cp,ts+2)
  else
   call PS_move(cp,ts-1)
   call PS_draw(cp,ts+2)
  end if
  call PS_stroke
  write (psunit,"(E14.6,E14.6,' scale')") 1.0/sx,1.0/sy
  call PS_text(sx*(cp-0.8*cw),sy*(cs-0.1*ch),'10')
  call PS_textint(sx*(cp-0.8*cw),sy*(cs+0.1*ch),'   ',int(cp))
  write (psunit,"(E14.6,E14.6,' scale')") sx,sy
  do i=1,9
   tick2=cp+logtick(i)
   if ((tick2.ge.low).and.(tick2.le.high)) then
    call PS_move(tick2,ts) 
    call PS_draw(tick2,ts+2) 
    call PS_stroke
   end if
  end do
 end if
! draw the tickmarks for linear scaling on the y-axis
 if (typ.eq.'ylin') then 
  call PS_move(ts,cp)
  call PS_draw(ts+2,cp)
  call PS_stroke
  write (psunit,"(E14.6,E14.6,' scale')") 1.0/sx,1.0/sy
  write (psunit,"(E14.6,' ',E14.6,' M ( ',F5.1,' ) show')") sx*(cs-0.2*sh),sy*(cp-0.1*ch),cp
  write (psunit,"(E14.6,E14.6,' scale')") sx,sy
  tick2=cp+d/2
  if ((tick2.ge.low).and.(tick2.le.high)) then
   call PS_move(ts,tick2) 
   call PS_draw(ts+2,tick2) 
   call PS_stroke
   write (psunit,"(E14.6,E14.6,' scale')") 1.0/sx,1.0/sy
   write (psunit,"(E14.6,' ',E14.6,' M ( ',F5.1,' ) show')") sx*(cs-0.2*sh),sy*(tick2-0.1*ch),tick2
   write (psunit,"(E14.6,E14.6,' scale')") sx,sy
  end if
 end if
! draw the tickmarks for logarithmic scaling on the y-axis
 if (typ.eq.'ylog') then 
  if (ts.eq.100) then
   call PS_move(ts,cp)
   call PS_draw(ts+3,cp)
  else
   call PS_move(ts-1,cp)
   call PS_draw(ts+2,cp)
  end if
  call PS_stroke
  write (psunit,"(E14.6,E14.6,' scale')") 1.0/sx,1.0/sy
  call PS_text(sx*(cs-ch),sy*(cp-0.1*ch),'10')
  call PS_textint(sx*(cs-ch),sy*(cp+0.1*ch),'  ',int(cp))
  write (psunit,"(E14.6,E14.6,' scale')") sx,sy
  do i=1,9
   tick2=cp+logtick(i)
   if ((tick2.ge.low).and.(tick2.le.high)) then
    call PS_move(ts,tick2) 
    call PS_draw(ts+2,tick2) 
    call PS_stroke
   end if
  end do
 end if
end subroutine
!  ******************************************************************************)
recursive subroutine setexponent(n,s)
!DEC$ ATTRIBUTES DLLEXPORT :: setexponent

use postscript

IMPLICIT NONE

! draws the order of magnitude of the axis 
!
!              n
!            10        ( if n<>0 )
!

real(kind=sgl)          :: xs,ys
integer(kind=irg)       :: n
character(3)            :: s

intent(IN)              :: n,s

 if (s.eq.'BOT') then 
  xs=102
  ys=2
 end if
 if (s.eq.'TOP') then 
  xs=95
  ys=104
 end if
 if (s.eq.'LEF') then 
  xs=-5
  ys=102
 end if
 if (s.eq.'RIG') then 
  xs=108
  ys=98
 end if
 call PS_setfont(PSfonts(2),2.5)
 call PS_text(xs,ys,'*10')
 call PS_setfont(PSfonts(2),2.0)
 call PS_textint(xs+2.3,ys+1.2,' ',n)
end subroutine
!  ******************************************************************************
real recursive function determinestep(m,range)
!DEC$ ATTRIBUTES DLLEXPORT :: determinestep

IMPLICIT NONE

character(3)    :: m
integer(kind=irg)         :: i
real(kind=sgl)            :: range
real(kind=sgl),parameter  :: x(4) = (/2.0,3.0,6.0,15.0/), y(4) = (/0.4,0.5,1.0,2.0/)
!    
!    from the border values (low and high) a step used for making
!    the numbered ticks is derived
!    
!    caution : additional ticks are made at half way 
! 
 determinestep=4.0
 if (m.eq.'log') then 
  determinestep=1
 else
  do i=1,4
   if (range.le.x(i)) determinestep=y(i)
  end do
 end if
end function
!  ******************************************************************************
integer recursive function getshift(low,high,m)
!DEC$ ATTRIBUTES DLLEXPORT :: getshift
! 
!     determines the maximum length of the numbering at an axis
!     


IMPLICIT NONE

real(kind=sgl)    :: low,high
character(3)      :: m

 if (m.eq.'lin') then
  if (low.eq.-100) then 
   if (high.ne.100) then 
    getshift=1
   else 
    getshift=1
   end if
  else 
   if (low.lt.0) then 
    getshift=1
   else 
    if (high.ne.100) then 
     getshift=2
    else 
     getshift=1
    end if
   end if
  end if
! mode = 'log'
 else 
  if (omag(low).gt.omag(high)) then 
   getshift=omag(low)+2 
  else 
   getshift=omag(high)+2
  end if
 end if
end function 
!  ******************************************************************************)
recursive subroutine drawborder(low,high,n,s,m)
!DEC$ ATTRIBUTES DLLEXPORT :: drawborder

use postscript

IMPLICIT NONE

integer(kind=irg)       :: ich,mlog,n
real(kind=sgl)          :: sh,xl,xh,yl,yh,ts,cs,cp,ch,cw,d,low,high,q,sx,sy
character(3)            :: s,m 
character(4)            :: settick

 if (m.eq.'log') then
  mlog = 1
 else
  mlog = 0
 end if
! bottom or top
 if ((s.eq.'BOT').or.(s.eq.'TOP')) then
  if (m.eq.'lin') then
   settick='xlin'
  else 
   settick='xlog'
  end if
  xl=low-0.2*(high-low)
  xh=high+0.2*(high-low) 
  yl=-20
  yh=120
  if (s.eq.'BOT') then
   ts=0 
   cs=-4-4*mlog 
  else 
   ts=98 
   cs=101+mlog
  end if
 end if
! left or right
 if ((s.eq.'LEF').or.(s.eq.'RIG')) then
  if (m.eq.'lin') then
   settick='ylin'
  else 
   settick='ylog'
  end if
  yl=low-0.2*(high-low)
  yh=high+0.2*(high-low) 
  xl=-20
  xh=120
  if (s.eq.'LEF') then
   ts=0 
   cs=-5
  else 
   ts=98 
   cs=102+mlog
  end if
 end if
 if ((n.ne.0).and.(m.ne.'log')) call setexponent(n,s)
! go to the new origin
 if ((s.eq.'BOT').or.(s.eq.'TOP')) then  
  q = -xl*140.0/(xh-xl) - 20.0
  write (psunit,"(f12.6,f12.6,' T')") q,0.0
 end if
 if ((s.eq.'LEF').or.(s.eq.'RIG')) then  
  q = -yl*140.0/(yh-yl) - 20.0
  write (psunit,"(f12.6,f12.6,' T')") 0.0,q
 end if
! and switch the scale to draw the tickmarks 
 sx = 140.0/(xh-xl)
 sy = 140.0/(yh-yl)
 write (psunit,"(E14.6,E14.6,' scale')") sx,sy
 cw=0.02*(xh-xl)
 ch=0.06*(yh-yl)

 d=determinestep(m,abs(high-low))
 ich=getshift(low,high,m)
 sh=cw*(ich-4.0/9.0)
 if (s.eq.'RIG') sh=0.0
 if (low*high.ge.0.0) then 
  cp=low
 else 
  d=-d
  cp=0
  do while (nint(cp).ge.low) 
   call setticks(cp,d,low,high,ts,cs,cw,ch,sh,ich,settick,sx,sy)
   cp=cp+d
  end do
  cp=0
  d=-d
 end if
 do while (nint(cp).le.high) 
  call setticks(cp,d,low,high,ts,cs,cw,ch,sh,ich,settick,sx,sy)
  cp=cp+d
 end do
! set the scale back to what is was !!!
 sx = 1.0/sx
 sy = 1.0/sy
 write (psunit,"(E14.6,E14.6,' scale')") sx,sy
! and return to the origin
 if ((s.eq.'BOT').or.(s.eq.'TOP')) then  
  write (psunit,"(f12.6,f12.6,' T')") -q,0.0
 end if
 if ((s.eq.'LEF').or.(s.eq.'RIG')) then  
  write (psunit,"(f12.6,f12.6,' T')") 0.0,-q
 end if
end subroutine
!  ******************************************************************************
recursive subroutine drawfigure(xmin,xmax,ymin,ymax,pmode,mark,points,xmode,ymode,xvec,yvec)
!DEC$ ATTRIBUTES DLLEXPORT :: drawfigure

use postscript
 
IMPLICIT NONE
 
integer(kind=irg)    :: i,mark,points
real(kind=sgl)       :: xdraw,ydraw,xmin,xmax,ymin,ymax,qx,qy,sx,sy,q
character(3)         :: pmode,xmode,ymode
real(kind=sgl)       :: xvec(points),yvec(points) 
  
! go to the new origin
 qx = -xmin*100.0/(xmax-xmin)
 qy = -ymin*100.0/(ymax-ymin)
 call PS_translate(qx,qy)
! switch the scale to draw figure
 sx = 140.0/(1.4*(xmax-xmin))
 sy = 140.0/(1.4*(ymax-ymin))
 q = 0.001/sy
 call PS_setlinewidth(q)
 write(psunit,"(E14.6,' ',E14.6,' scale')") sx,sy
! clip the drawing, so that none of it appears outside of the square
! write (psunit,*) '1.0 setgray'
! call PS_closepath
! call PS_move(xmin,ymin)
! call PS_draw(xmax,ymin)
! call PS_draw(xmax,ymax)
! call PS_draw(xmin,ymax)
! call PS_clippath
! write (psunit,*) '0.0 setgray'
! the next line MUST be present, otherwise the clippath will be
! visible on the drawing !
! call PS_newpath
! make the drawing 
 do i=1,points 
  if (xmode.eq.'log') then 
   xdraw=log10(xvec(i))
  else 
   xdraw=xvec(i)
  end if
  if (ymode.eq.'log') then 
   ydraw=log10(yvec(i))
  else
   ydraw=yvec(i)
  end if
  if (pmode.eq.'CON') then
   if (i.eq.1) then
    call PS_move(xdraw,ydraw) 
   else 
    call PS_draw(xdraw,ydraw)
   end if
  end if
! the folowing lines are to be implemented
!         if (pmode.eq.'DOT') then
!    point(1)=xdraw
!           point(2)=ydraw
!           polymarker2d(point,1,0)
!         end if
  if (pmode.eq.'BAR') then
   call PS_move(xdraw,ymin)
   call PS_draw(xdraw,ydraw)
  end if
 end do 
 call PS_stroke
! set the scale back to what is was !!!
 write (psunit,"(E14.6,' ',E14.6,' scale')") 1.0/sx,1.0/sy
 call PS_translate(-qx,-qy)
end subroutine
!  ******************************************************************************
recursive subroutine axis(AX,points,xvec,yvec,xmin,xmax,ymin,ymax,xautorange,yautorange, &
                xmode,ymode,pmode,mark,scalex,scaley,overplot,db,title,xtitle,ytitle)
!DEC$ ATTRIBUTES DLLEXPORT :: axis

use postscript

IMPLICIT NONE

type(axistype),INTENT(IN)  :: AX
integer(kind=irg)          :: points,mark
integer(kind=irg)          :: nx,ny
real(kind=sgl)             :: xvec(points), yvec(points), xmin, xmax, ymin, ymax,q,r
real(kind=sgl)             :: sxmin,sxmax,symin,symax 
logical                    :: xautorange, yautorange,overplot,db
character(3)               :: xmode, ymode, pmode, scalex, scaley
character(*)               :: title,xtitle,ytitle

! determine the scale of the figure
 if (xautorange) then
  xmin = minval(xvec)
  xmax = maxval(xvec)
 end if
 sxmin=xmin
 sxmax=xmax
 if (yautorange) then
  ymin = minval(yvec)
  ymax = maxval(yvec)
 end if
 symin=ymin
 symax=ymax
! determine the border parameters 
 call border(xmode,xmin,xmax,nx)
 call border(ymode,ymin,ymax,ny)
! initialize graphics stuff  
 call initframe(AX,'start',db)
 if (db) then 
! draw the borders if needed
  if (scalex.ne.'NON') then
   call drawborder(xmin,xmax,nx,scalex,xmode)
  end if
  if (scaley.ne.'NON') then
   call drawborder(ymin,ymax,ny,scaley,ymode)
  end if
 end if
 xmin=xmin*power(nx)
 xmax=xmax*power(nx)
 ymin=ymin*power(ny)
 ymax=ymax*power(ny)
! draw the curve 
 call drawfigure(xmin,xmax,ymin,ymax,pmode,mark,points,xmode,ymode,xvec,yvec)
! draw the titles (this could also be done from the 
! calling program, immediately after the axis call)
! write (psunit,*) 'initclip'
 call PS_setfont(PSfonts(2),6.0)
 q=20.0-stringl(title)/2.0
 r=110.0
 call PS_text(q,r,title)
 call PS_setfont(PSfonts(2),4.0)
 q=50.0-stringl(xtitle)/2.0
 r=-10.0
 call PS_text(q,r,xtitle)
! rotate the text by 90 degrees
 write (psunit,"(f12.6, f12.6,' T 90.0 rotate')") -10.0,50.0-stringl(ytitle)/2.0
 r=0
 call PS_text(r,r,ytitle)
 write (psunit,"(' -90.0 rotate ',f12.6, f12.6,' T')") 10.0,-50.0+stringl(ytitle)/2.0
! return the proper min and max values
 if (xmode.eq.'log') then 
  xmin=power(nint(xmin))
  xmax=power(nint(xmax))
 end if
 if (ymode.eq.'log') then 
  ymin=power(nint(ymin))
  ymax=power(nint(ymax))
 end if
 call initframe(AX,'stop ',.FALSE.)
end subroutine

end module


!--------------------------------------------------------------------------
! EMsoft:cont_mod.f90
!--------------------------------------------------------------------------
!
! MODULE: cont_mod
!
!> @author Marc De Graef, Carnegie Mellon University (but see below)
!
!> @brief module to draw contour plots
!
!> @todo add more comments; make routines public and private
! 
!> @date   ?/?/7? PVH 1.0 original f77 version by P. Van Houtte (KULeuven)
!> @date   ?/?/85 KOM 2.0 pascal version using pointers by Koen Mols
!> @date  10/10/01 MDG 3.0 f90 version (after a lot of cursing at pointers...)
!> @date  11/27/01 MDG 3.1 added kind support
! ###################################################################
module cont_mod

use local

! all variable names start with a capital C

! user defined pointer types
type Cnode 
 integer(kind=irg)      :: ix,iy
 real(kind=sgl)         :: f
 logical                :: gt
end type
       
type Cvector 
 real(kind=sgl)          :: sx,sy
 type(Ctriangle),pointer :: t1,t2
 integer(kind=irg)       :: iv
 logical                 :: border
 type(Cvector),pointer   :: next
end type
                   
type Ctriangle
 type(Cvector),pointer   :: v1,v2 
 logical                 :: to_plot
 integer(kind=irg)       :: it
 type(Ctriangle),pointer :: next
end type

type Crow_type
 type(Ctriangle),pointer :: rw
end type

! parameters specified in calling program
type Cparameters
 integer(kind=irg)       :: nx,ny 
 real(kind=sgl)          :: level,x0,y0,dx,dy
 real(kind=sgl)          :: distort(2,2)
end type


! [06/09/14] This will need to be changed so that there are no global variables left ...
! Shouldn't be difficult, but it is not urgent at this point in time and there
! is likely no variable name clashing...
  
! array (1D) of values to be contoured
real(kind=sgl),allocatable         :: Cdata(:)

! basic pointers and "pointer array"
type(Cvector),pointer    :: Cv_root
type(Ctriangle),pointer  :: Ct_root
type(Crow_type),dimension(1024) :: Crow_t
type(Cparameters)        :: Cparam
integer(kind=irg)        :: Civ,Cit

contains 

!---------------------------------------------
recursive subroutine make_node(i,j,nd)
!DEC$ ATTRIBUTES DLLEXPORT :: make_node

IMPLICIT NONE

integer(kind=irg)   :: i,j
type(Cnode)         :: nd

! fill the node vector with coordinates, function value, and 
! a logical indicating whether or not the function value is
! greater than the level requested for the contour
 nd%ix=2*i
 nd%iy=2*j
 nd%f =Cdata(i+j*Cparam%nx+1)
 nd%gt=(nd%f.gt.Cparam%level)
end subroutine make_node

!---------------------------------------------
recursive subroutine make_central_node(i,j,nd)
!DEC$ ATTRIBUTES DLLEXPORT :: make_central_node

IMPLICIT NONE

integer(kind=irg)  :: i,j
type(Cnode)        :: nd

! same a make_node but for the center of the grid square;
! uses an average value for the function
 nd%ix=2*i-1
 nd%iy=2*j-1
 nd%f =(Cdata(i+j*Cparam%nx+1)+Cdata((i-1)+j*Cparam%nx+1)+Cdata(i+(j-1)*Cparam%nx+1)+Cdata((i-1)+(j-1)*Cparam%nx+1))/4.0
 nd%gt=(nd%f.gt.Cparam%level)
end subroutine make_central_node

!---------------------------------------------
recursive subroutine make_triangle(n1,n2,n3,t)
!DEC$ ATTRIBUTES DLLEXPORT :: make_triangle

IMPLICIT NONE

type(Cnode)             :: n1,n2,n3
type(Ctriangle),pointer :: t

! if the three corner points of the triangle are not all
! greater than or all smaller than the contour level, then
! create a triangle and insert it in the linked list, otherwise
! return a nil value.
 if ((n1%gt.eqv.n2%gt).and.(n1%gt.eqv.n3%gt)) then 
  nullify(t)
 else 
  Cit = Cit+1
  allocate(t)
  t%to_plot=.TRUE.
  nullify(t%v1)
  nullify(t%v2)
  t%it = Cit
  t%next => Ct_root
  Ct_root => t
 end if
end subroutine make_triangle

!---------------------------------------------
recursive subroutine make_vector(n1,n2,t1,t2)
!DEC$ ATTRIBUTES DLLEXPORT :: make_vector

IMPLICIT NONE

type(Cvector),pointer   :: v 
type(Cnode)             :: n1,n2
type(Ctriangle),pointer :: t1,t2
real(kind=sgl)          :: sx,sy
    
! Find the intersection point of the contour with the line
! common to the two triangles t1 and t2 (linear interpolation);
! Connect this vector to the two triangles
 if (.not.(n1%gt.eqv.n2%gt)) then 
  allocate(v)
  Civ=Civ+1
  sx=0.50*Cparam%dx*((Cparam%level-n1%f)*float(n2%ix-n1%ix)/(n2%f-n1%f)+float(n1%ix))
  sy=0.50*Cparam%dy*((Cparam%level-n1%f)*float(n2%iy-n1%iy)/(n2%f-n1%f)+float(n1%iy))
  v%sx = Cparam%x0+Cparam%distort(1,1)*sx+Cparam%distort(1,2)*sy
  v%sy = Cparam%y0+Cparam%distort(2,1)*sx+Cparam%distort(2,2)*sy
  v%border=.FALSE.
  v%t1=>t1
  v%t2=>t2
  v%iv=Civ
! connect t1 with vector if it is not a border point
  if (.not.associated(t1)) then 
   v%border=.TRUE.
  else 
   if (.not.associated(t1%v1)) then 
    t1%v1=>v
   else 
    t1%v2=>v
   end if
  end if
! connect t2 with vector if it is not a border point
  if (.not.associated(t2)) then 
   v%border=.TRUE.
  else 
   if (.not.associated(t2%v1)) then 
    t2%v1=>v
   else 
    t2%v2=>v
   end if
  end if
  v%next=>Cv_root
  Cv_root=>v
 end if
end subroutine

!---------------------------------------------

recursive subroutine plot_line(t,v)
!DEC$ ATTRIBUTES DLLEXPORT :: plot_line

use postscript 

IMPLICIT NONE

type(Ctriangle),pointer  :: t
type(Cvector),pointer    :: v

! follow the linked list and draw all points along a single line
! Since this line may close on itself, we will have to do this
! many times, until there are no more triangles left.
 do while (associated(t)) 
   t%to_plot=.FALSE.
   if (t%v1%iv.eq.v%iv) then 
    v=>t%v2 
   else 
    v=>t%v1
   end if
   call PS_draw(v%sx,v%sy)
   Cit=Cit-1
   v%border=.FALSE.
   if (v%t1%it.eq.t%it) then 
    t=>v%t2 
   else 
    t=>v%t1
   end if
 end do 
end subroutine

!---------------------------------------------
recursive subroutine plot_contour
!DEC$ ATTRIBUTES DLLEXPORT :: plot_contour

use postscript 

IMPLICIT NONE

type(Cvector),pointer   :: v
type(Ctriangle),pointer :: t
integer(kind=irg)       :: i

! find and plot the open contour lines  (2 passes should suffice)
 do i=1,2
  v=>Cv_root
  do while (associated(v)) 
   if (v%border) then 
    v%border=.FALSE.
    call PS_stroke
    call PS_move(v%sx,v%sy)
    if (associated(v%t1)) then 
      call plot_line(v%t1,v)
    else
      call plot_line(v%t2,v)
    end if
   end if
   v=>v%next
  end do
 end do
! 
! find and plot the closed contour lines (several passes needed)
!
 do while (Cit.gt.0) 
  t=>Ct_root
  do while (associated(t))
   if (t%to_plot) then
     v=>t%v1
     call PS_stroke
     call PS_move(v%sx,v%sy)
     if (v%t1%it.eq.t%it) then 
      nullify(v%t1)
     else 
      nullify(v%t2)
     end if
     call plot_line(t,v)
   end if
   t=>t%next
  end do
 end do
!
! dispose of both linked lists
!
 do while (associated(Ct_root)) 
  t=>Ct_root
  Ct_root=>t%next
  deallocate(t)
 end do
 do while (associated(Cv_root))
  v=>Cv_root
  Cv_root=>v%next
  deallocate(v)
 end do
end subroutine
!---------------------------------------------

recursive subroutine contour
!DEC$ ATTRIBUTES DLLEXPORT :: contour                      
!                                              
! definition of vectors, nodes and triangles
!                                              
!            ul       ur                       
!            |\       /                         
!            | \  tt /                          
!            |  4   5                           
!            |   \ /                            
!            1 tl c  tr                         
!            |   / \                            
!            |  3   6                           
!            | /  tb \ 
!             /___2___\
!            ll       lr                       
!                                              
! Each grid square is triangulated by four triangles
! which are labelled tl, tt, tb, and tr.  Each grid
! square has four corners (ll, ul, lr, ur) and a center (c).
! The algorithm computes for each of the six connecting
! vectors whether or not there is an intersection of the 
! requested contour.  If there is one, then the corresponding
! coordinates are added to a linked list which keeps track
! of the two neighbouring triangles that make up the vector.
!
! At the end the entire linked list is traversed to draw all
! contours, both the ones that intersect the border and the
! ones that are fully inside the border.
!
! There may be better ways to do this, but it definitely works.
!


use postscript
use io

type(Cnode)              ::  nd_ll,nd_ul,nd_lr,nd_ur,nd_c
type(Ctriangle),pointer  ::  t_l,t_r,t_b,t_t,dummy
integer(kind=irg)        ::  Cit_save

! initialize vector and triangle counters
 Civ = 0
 Cit = 0

! initialize vector and triangle linked lists 
 if (.not.(associated(Cv_root))) then
  allocate(Cv_root)
  nullify(Cv_root)
 end if
 if (.not.(associated(Ct_root))) then
  allocate(Ct_root)
  nullify(Ct_root)
 end if 
! allocate a dummy triangle
 allocate(dummy)
 nullify(dummy)     
! loop over all rows in the grid
 do j=1,Cparam%ny-1 
  call make_node(0,j-1,nd_ll)                     ! lower left node
  call make_node(0,j  ,nd_ul)                     ! upper left node
! there is no neighbouring triangle at the left 
  nullify(t_r)                  
! loop over all colums in the row
  do i=1,Cparam%nx-1
   call make_node(i,j-1,nd_lr)                    ! lower right node
   call make_node(i,j  ,nd_ur)                    ! upper right node
   call make_central_node(i,j,nd_c)               ! central node
             
   call make_triangle(nd_ll,nd_c,nd_ul,t_l)       ! left triangle
   call make_vector(nd_ll,nd_ul,t_r,t_l)          ! vector 1 
             
   call make_triangle(nd_ll,nd_c,nd_lr,t_b)       ! bottom triangle
   call make_triangle(nd_ul,nd_c,nd_ur,t_t)       ! top triangle
   call make_triangle(nd_lr,nd_c,nd_ur,t_r)       ! right triangle
             
   call make_vector(nd_ll,nd_lr,Crow_t(i)%rw,t_b) ! vector 2 
   Crow_t(i)%rw=>t_t                              ! keep for the next row

   call make_vector(nd_ll,nd_c,t_l,t_b)           ! vector 3
   call make_vector(nd_ul,nd_c,t_l,t_t)           ! vector 4
   call make_vector(nd_ur,nd_c,t_r,t_t)           ! vector 5
   call make_vector(nd_lr,nd_c,t_r,t_b)           ! vector 6
             
   nd_ll=nd_lr                                    ! copy lower right to lower left
   nd_ul=nd_ur                                    ! copy upper right to upper left
  end do  ! end loop over i
  call make_vector(nd_ll,nd_ul,t_r,dummy)         ! last vector of a row
 end do   ! end loop over j 
     
! close the upperside 
 call make_node(0,Cparam%ny-1,nd_ll)
 do i=1,Cparam%nx-1
  call make_node(i,Cparam%ny-1,nd_lr)
  call make_vector(nd_ll,nd_lr,Crow_t(i)%rw,dummy)  
  nd_ll=nd_lr
 end do 

! and plot it 
 Cit_save = Cit 
 call plot_contour
 call PS_stroke  ! to make sure the last line is drawn

! mess = 'Level : '; oi_real(1)=Cparam%level; call WriteReal(1,"(F10.5,';')",advance="no")
! mess = 'vectors/triangles : '; oi_int(1) = Civ; oi_int(2) = Cit_save
! call WriteInt(2,"(I6,'/',I6)")
end subroutine

end module
