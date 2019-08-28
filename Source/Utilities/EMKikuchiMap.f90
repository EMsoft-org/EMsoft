! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMKikuchiMap.f90
!--------------------------------------------------------------------------
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief PostScript output of kinematical kikuchi maps 
!
!> @date 02/25/02 MDG 1.0 original
!> @date 05/14/15 MDG 2.0 rewrite for new version
!> @date 11/17/15 MDG 2.1 corrected minor errors; tested program
!--------------------------------------------------------------------------
program EMKikuchiMap

use local
use typedefs
use crystal
use HDFsupport
use symmetry
use diffraction
use graphics
use files
use postscript
use io

IMPLICIT NONE

real(kind=sgl)          :: camlen, io_real(1)
integer(kind=irg)       :: imanum
character(fnlen)        :: progdesc, progname, gname
logical                 :: loadingfile
type(unitcell)          :: cell
type(gnode)             :: rlp
type(postscript_type)   :: PS

interface
  subroutine KikmapPage(cell, rlp, PS, camlen)

  use local
  use typedefs
  use postscript
  use crystal
  use symmetry
  use math
  use io
  use constants
  use diffraction
  
  IMPLICIT NONE
  
    type(unitcell)        ,INTENT(IN)  :: cell
  type(gnode),INTENT(INOUT)          :: rlp
  type(postscript_type),INTENT(INOUT):: PS
  real(kind=sgl),INTENT(IN)          :: camlen
  end subroutine KikmapPage
end interface


 progname = 'kikmap.f90'
 progdesc = 'Kinematical Kikuchi map simulations'
 call EMsoft(progname, progdesc)


 allocate(cell)

 cell % SG % SYM_reduce=.TRUE.

! read crystal information
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = gname
 call CrystalData(cell)

! read crystal information, microscope voltage, and camera length
 call GetVoltage(cell, rlp)

 call ReadValue(' Camera length L  [mm, real] ', io_real, 1)
 camlen = io_real(1)

! generate all atom positions in the fundamental unit cell
 call CalcPositions(cell,'v')

! open PostScript file
 imanum = 1
 call PS_openfile(PS, progdesc, imanum)
! generate a set of HOLZ patterns
 call KikmapPage(cell, rlp, PS, camlen)
! close Postscript file
 call PS_closefile(PS)

end program EMKikuchiMap

! ###################################################################
!
!  subroutine KikmapPage
!
!  Author: Marc De Graef
!
!  Description: draw Kikuchi map 
!
!  History
!
!  modified by  rev reason
!  -------- --- --- -----------
!> @date 02/25/02 MDG 1.0 original
!> @date 12/02/14 MDG 2.0 removed global variables
! ###################################################################
subroutine KikmapPage(cell, rlp, PS, camlen)

use local
use typedefs
use postscript
use crystal
use symmetry
use math
use io
use constants
use diffraction

IMPLICIT NONE

type(unitcell)        ,INTENT(IN)  :: cell
type(gnode),INTENT(INOUT)          :: rlp
type(postscript_type),INTENT(INOUT):: PS
real(kind=sgl),INTENT(IN)          :: camlen

logical                         :: again, first, newzone, nexttop
real(kind=sgl)                  :: negative(2),twopi,ggl,gg(3),igl,RR,thr, &
                                   RHOLZmax,RHOLZ(20),xo,yo,sc,pos(2),dy, oi_real(6), io_real(1)
real(kind=sgl),parameter        :: xoff(0:5)=(/0.0,3.3125,0.0,3.3125,0.0,3.3125/), &
                                   yoff(0:5)=(/6.0,6.0,3.0,3.0,0.0,0.0/), &
                                   eps = 1.0E-3
integer(kind=irg)               :: g1i(3),g2i(3),i,numHOLZ,nref
real(kind=sgl),parameter        :: le=3.25,he=2.9375
character(1)                    :: z

real(kind=sgl)                  :: g1(3),g2(3),PX,PY,laL,Gmax,radius, Imax
integer(kind=irg)               :: uvw(3),contrast
type(kikuchireflection),pointer :: top

interface
  subroutine Calcmap(cell, rlp, PS, top, camlen, g1, g2, Gmax, radius, uvw, Imax)

  use local
  use io
  use postscript
  use crystal
  use symmetry
  use error
  use diffraction
  use constants
  
  IMPLICIT NONE
  
  type(unitcell)        ,INTENT(IN)  :: cell
  type(gnode),INTENT(INOUT)          :: rlp
  type(postscript_type),INTENT(INOUT):: PS
  type(kikuchireflection),pointer,INTENT(INOUT)  :: top
  real(kind=sgl),INTENT(IN)          :: camlen
  real(kind=sgl),INTENT(IN)          :: g1(3), g2(3), Gmax, radius
  integer(kind=irg),INTENT(IN)       :: uvw(3)
  real(kind=sgl),INTENT(INOUT)       :: Imax
  end subroutine Calcmap

  subroutine PlotBands(PS, top, radius, contrast, Imax)

  use local
  use io
  use postscript
  use constants
  
  IMPLICIT NONE
  
  type(postscript_type),INTENT(IN)     :: PS
  real(kind=sgl),INTENT(IN)            :: radius
  type(kikuchireflection),pointer,INTENT(INOUT)  :: top
  integer(kind=irg),INTENT(IN)            :: contrast
  real(kind=sgl),INTENT(IN)            :: Imax 
  end subroutine PlotBands
end interface

! set contrast to 1 for white lines on gray background,
! 0 for black lines on white background
 contrast = 0
 PS%pspage = 0
 radius = 80.0
 thr = 1.E-4 
 twopi = 2.0*cPi
 Imax = 0.0

! camera length
 laL = sngl(cell%mLambda) * camlen
 oi_real(1) = sngl(cell%mLambda)
 call WriteValue('wavelength [nm] = ', oi_real, 1, "(F10.6)")

 oi_real(1) = camlen
 call WriteValue(' L         [mm] = ', oi_real, 1, "(F10.2)")

 oi_real(1) = laL
 call WriteValue('camera length lambda*L [mm nm] = ', oi_real, 1, "(F10.5)")

! what portion of reciprocal space is to be covered 
 call ReadValue('Enter maximum spatial frequency to be considered [nm^-1] ', io_real, 1)
 Gmax = io_real(1)

! get the zone axis
 call GetIndex(cell%hexset, uvw, 'd')

! get the basis vectors g1 and g2
 call ShortestG(cell, uvw,g1i,g2i,i)
 g1 = float(g1i); g2 = float(g2i)
    
! get Laue center in terms of g1 and g2
  oi_real(1:3) = g1(1:3)
  oi_real(4:6) = g2(1:3)
  call WriteValue('The basis vectors for this zone axis are ',oi_real, 6, "(/'g1 = ',3f10.5,/'g2 = ',3f10.5,/)") 

! do page preamble stuff if this is a new page
! [This assumes that the PostScript file has already been opened]
  call PS_newpage(PS,.FALSE.,'Kinematical Kikuchi Map')
  call PS_text(5.25,-0.05,'scale bar in reciprocal nm')
  call PS_textvar(5.25,PS % psfigheight+0.02,'Camera Constant [nm mm]',laL)
  call PS_setfont(PSfonts(2),0.15)
  call PS_text(-0.25,PS % psfigheight+0.02,'Structure File : '//cell % fname)
! draw frame and related stuff
  PX = 3.75
  PY = 4.00
! add other data lines to the upper left
  call PS_setfont(PSfonts(2),0.15)
  call PS_textvar(-0.25,PS%psfigheight-0.18,'Acc. Voltage [kV] ',sngl(cell%voltage)*0.001)
  call PS_text(-0.25,PS%psfigheight-0.38,'Zone axis ')
  call PrintIndices('d',cell%hexset,uvw(1),uvw(2),uvw(3),-0.25+1.5,PS%psfigheight-0.38)
! scale bar (sc is the conversion factor from nm-1 to inches)
  sc = laL/25.4
  call PS_setlinewidth(0.020)
  call PS_line(xo+0.05,yo+0.06,xo+0.05+5.0*sc,yo+0.06)
  call PS_setfont(PSfonts(2),0.15)
  call PS_text(xo+0.05+2.5*sc,yo+0.10,'5 ')
! draw main circle graylevel 50%
  if (contrast.eq.1) then
    call PS_filledcircle(PX,PY,radius/25.4,0.5)
  else
    call PS_filledcircle(PX,PY,radius/25.4,1.0)
  end if
! plot origin of reciprocal space 
  call PS_filledcircle(PX,PY,0.03,0.0)

! compute the Kikuchimap
  call Calcmap(cell, rlp, PS, top, camlen, g1, g2, Gmax, radius, uvw, Imax)

! once that is done, we can determine the intensities to be drawn and 
! draw all reflections and lines.
    call PlotBands(PS, top, radius, contrast, Imax)

end subroutine KikmapPage



! ###################################################################
!
!  subroutine Calcmap
!
!  Author: Marc De Graef
!
!  Description: compute the actual map
!
!  History
!
!  modified by  rev reason
!  -------
!  01/25/02 MDG 1.0 original
! ###################################################################
subroutine Calcmap(cell, rlp, PS, top, camlen, g1, g2, Gmax, radius, uvw, Imax)

use local
use io
use postscript
use crystal
use symmetry
use error
use diffraction
use constants

IMPLICIT NONE

type(unitcell)        ,INTENT(IN)  :: cell
type(gnode),INTENT(INOUT)          :: rlp
type(postscript_type),INTENT(INOUT):: PS
type(kikuchireflection),pointer,INTENT(INOUT)  :: top
real(kind=sgl),INTENT(IN)          :: camlen
real(kind=sgl),INTENT(IN)          :: g1(3), g2(3), Gmax, radius
integer(kind=irg),INTENT(IN)       :: uvw(3)
real(kind=sgl),INTENT(INOUT)       :: Imax


integer(kind=irg)                :: inmhkl(3),hc,i,j,nref,istat,inm,hh,kk,ll,ind(3),ih,ik,il,imin,hhh, oi_int(3)
real(kind=sgl)                   :: gg(3),Ig,x,alp,beta,theta,D,gpx,gpy,hlx(2),hly(2),phi,glen,gtoc(2,2),pxy(2), &
                                    da,db,dd,dec(2,2)
logical                          :: drawit
logical,allocatable              :: z(:,:,:)
character(1)                     :: q
type(kikuchireflection),pointer  :: temp,bot
 
nullify(top)
nullify(bot)
nullify(temp)

! allocate the linked list to store all reflections
 if (.not.associated(top)) then 
    allocate(top)
    bot => top
    nullify(bot%next)
 end if
 call WriteValue('','Computing map',"(/A)")

! set the index boundaries
 do i=1,3
   inmhkl(i) = int(1.2*Gmax/sqrt(cell%rmt(i,i)))
 end do
 oi_int(1:3) = inmhkl(1:3)
 call WriteValue('Index range ', oi_int, 3, "(3I4)")

! allocate logical array to keep track of reflections
 allocate(z(-inmhkl(1):inmhkl(1),-inmhkl(2):inmhkl(2),-inmhkl(3):inmhkl(3)))
 z = .FALSE.
 inm = maxval(inmhkl)

! initialize the geometrical parameters
 alp = atan(radius/camlen)
 Imax = 0.0
 phi = Calcangle(cell,g1,g2,'r')
 glen = CalcLength(cell,g2,'r')
 gtoc(1,1) = CalcLength(cell,g1,'r')
 gtoc(1,2) = glen*cos(phi)
 gtoc(2,1) = 0.0
 gtoc(2,2) = glen*sin(phi)
! matrix to decompose g w.r.t. g1 and g2
 da = CalcDot(cell,g1,g1,'r')
 db = CalcDot(cell,g1,g2,'r')
 dd = CalcDot(cell,g2,g2,'r')
 dec = reshape( (/dd, -db, -db, da/), (/2,2/)) / (da*dd-db**2)
 nref=0
! eliminate central spot
 z(0,0,0) = .TRUE.

! loop over all reflections
 do hh=-inmhkl(1),inmhkl(1)
  do kk=-inmhkl(2),inmhkl(2)
   do ll=-inmhkl(3),inmhkl(3)
! have we done this one already ? if so, skip
   if (z(hh,kk,ll).eqv..TRUE.) cycle
! reduce to lowest common denominator
   ind= (/ hh, kk, ll /)
   call IndexReduce(ind)
   if (z(ind(1),ind(2),ind(3)).eqv..TRUE.) cycle
! next, make sure that this reflection is allowed; if not, then
! take twice that g
   imin = 1
   if (IsGAllowed(cell, ind).eqv..FALSE.) then
! check if a multiple of g is allowed
     i=1
     imin=-1
     do while ((i.le.inm).and.(imin.eq.-1))
      if (IsGAllowed(cell, i*ind).eqv..TRUE.) then 
       imin = i
      end if
      i=i+1     
     end do
   end if
   if (imin.eq.-1) then  ! no multiple is allowed, so take next reflection
    cycle
   else
    ind = imin*ind
   end if
! make sure no higher indices get through
   if (maxval(abs(ind)).gt.inm) cycle
   if (z(ind(1),ind(2),ind(3)).eqv..TRUE.) cycle
! compute the angle between this vector and the incident beam direction
   beta = -CalcAngle(cell,float(ind),float(uvw),'c')+0.5*cPi
! if negative, take the opposite reflection (-g)
   if (beta.lt.0.0) then
     ind = -ind
     beta = -beta
   end if
! diffraction angle
   theta = CalcDiffAngle(cell,ind(1),ind(2),ind(3))
! if the entire band falls outside of the region of interest, then return
   if ((beta-theta).gt.alp) cycle
! store data in linked list, along with calculated coordinates of lines
   allocate(bot%next,stat=istat)
   nref = nref+1
   bot => bot%next
   nullify(bot%next)
   bot%theta = theta
   bot%beta  = beta
   bot%hkl = ind
! get intensity (kinematical)
   call CalcUcg(cell,rlp,ind)
   bot%Ig = rlp%Vmod**2
   if (bot%Ig.gt.Imax) Imax = bot%Ig
! find the normalized Cartesian projection of g
   gpx = CalcDot(cell,float(ind),g1,'r')
   gpy = CalcDot(cell,float(ind),g2,'r')
   pxy = matmul(dec,(/gpx,gpy/))
! convert it to a Cartesian reference frame instead of g1,g2
   pxy = matmul(gtoc,pxy)
   D = sqrt(pxy(1)**2+pxy(2)**2)
   gpx = pxy(1)/D
   gpy = pxy(2)/D
! get the intercept coordinate of both lines with the projection of g
! first line
    x = radius*tan(beta+theta)/tan(alp)/25.4
    call CalcLine(x,gpx,gpy,radius/25.4,hlx,hly,drawit)
    if (drawit.eqv..TRUE.) then
     bot%drawh=.TRUE.
     bot%hlx=hlx
     bot%hly=hly
    else
     bot%drawh = .FALSE.
    end if
! second line
    x = radius*tan(beta-theta)/tan(alp)/25.4
    call CalcLine(x,gpx,gpy,radius/25.4,hlx,hly,drawit)
    if (drawit.eqv..TRUE.) then
     bot%drawk=.TRUE.
     bot%klx=hlx
     bot%kly=hly
    else
     bot%drawk = .FALSE.
    end if
! center line, to be drawn as a thin line
    x = radius*tan(beta)/tan(alp)/25.4
    call CalcLine(x,gpx,gpy,radius/25.4,hlx,hly,drawit)
    if (drawit.eqv..TRUE.) then
     bot%drawc=.TRUE.
     bot%clx=hlx
     bot%cly=hly
    else
     bot%drawc = .FALSE.
    end if
! remove the multiples of those Miller indices from the list 
! so that we only keep the shortest vectors in any direction
   do hhh=-inm,inm
     ih=ind(1)*hhh
     ik=ind(2)*hhh
     il=ind(3)*hhh
     if (((abs(ih).le.inmhkl(1)).and.(abs(ik).le.inmhkl(2))).and.(abs(il).le.inmhkl(3))) then 
         z(ih,ik,il)=.TRUE.
     end if
   end do
! 
  end do
 end do
end do

oi_int(1) = nref
call WriteValue('number of bands drawn : ',oi_int, 1, "(I6)")

end subroutine Calcmap

! ###################################################################
!
!  subroutine PlotBands
!
!  Author: Marc De Graef
!
!  Description: draw a Kikuchi band
!
!  History
!
!  modified by  rev reason
!  -------
!  01/25/02 MDG 1.0 original
! ###################################################################
subroutine PlotBands(PS, top, radius, contrast, Imax)

use local
use io
use postscript
use constants

IMPLICIT NONE

type(postscript_type),INTENT(IN)     :: PS
real(kind=sgl),INTENT(IN)            :: radius
type(kikuchireflection),pointer,INTENT(INOUT)  :: top
integer(kind=irg),INTENT(IN)            :: contrast
real(kind=sgl),INTENT(IN)            :: Imax 

real(kind=sgl)         :: V,qx,qy,CB,limit,PX,PY
character(12)          :: txt
integer(kind=irg)      :: i,nref
type(kikuchireflection),pointer  :: temp,bot

 call WriteValue('Plotting Kikuchi bands and labels',"(/A/)")
 PX = 3.75
 PY = 4.00

! point to the top of the linked list
 temp => top%next
 nref = 0
 limit = (1.001*radius)**2
 open(unit=30,file='temp.txt',status='unknown',form='formatted')
 if (contrast.eq.1) then 
   write (psunit,"(F12.7,' setgray')") 1.0
 else
   write (psunit,"(F12.7,' setgray')") 0.0
 end if
 call PS_setfont(PSfonts(4),0.10)
! move through the entire list 
 do while (associated(temp))
! first line
  ! V=0.015*(temp%Ig/Imax)**0.1
  V=0.01*(temp%Ig/Imax)
write (*,*) V, temp%Ig, Imax, temp%hkl
  if (temp%drawh.eqv..TRUE.) then  
   call PS_setlinewidth(V)
   !call PS_line(PX+temp%hlx(1),PY+temp%hly(1),PX+temp%hlx(2),PY+temp%hly(2))
   call PS_line_gray(PX+temp%hlx(1),PY+temp%hly(1),PX+temp%hlx(2),PY+temp%hly(2),0.01-V)
  end if
! second line
  if (temp%drawk.eqv..TRUE.) then  
   call PS_setlinewidth(V)
   !call PS_line(PX+temp%klx(1),PY+temp%kly(1),PX+temp%klx(2),PY+temp%kly(2))
   call PS_line_gray(PX+temp%klx(1),PY+temp%kly(1),PX+temp%klx(2),PY+temp%kly(2),0.01-V)
  end if
! central line
  if (temp%drawc.eqv..TRUE.) then  
!  call PS_setlinewidth(0.004)
!  call PS_line(PX+temp%clx(1),PY+temp%cly(1),PX+temp%clx(2),PY+temp%cly(2))
! add indices along continuation of lines
   qx= PX+1.01*temp%clx(2)
   qy= PY+1.01*temp%cly(2)
   V = 180.0*atan2(temp%cly(2)-temp%cly(1),temp%clx(2)-temp%clx(1))/cPi
   write (30,"(1x,I3,1x,I3,1x,I3,1x,3(f10.5,1x))") (temp%hkl(i),i=1,3),qx,qy,V
   nref = nref+1
  end if
! move to the next reflection
  temp=>temp%next
 end do
! add indices along continuation of lines
 close(unit=30,status='keep')
 CB = (radius/25.4)**2
 write (psunit,"(F12.7,' setgray')") 0.0
 open(unit=30,file='temp.txt',status='old',form='formatted')
 do i=1,nref
   read (30,"(A12,1x,3(f10.5,1x))") txt,qx,qy,V
   if (((qx-PX)**2+(qy-PY)**2).gt.CB) then
    call PS_move(qx,qy)  ! just outside clipping ring
    write (psunit,"(1x,F10.5,' rotate')") V
    write (psunit,"(1x,'( ',A12,' ) show')") txt
    write (psunit,"(1x,F10.5,' rotate')") -V
   end if
 end do
 close(unit=30,status='delete')
end subroutine PlotBands






subroutine CalcLine(x,px,py,rad,hlx,hly,drawit)

use local

IMPLICIT NONE

real(kind=sgl)  :: x,px,py,rad,hlx(2),hly(2),det,tgm,y,qx,qy
logical         :: drawit


        drawit = .FALSE.
        if (abs(x).le.rad) then
         if (abs(px*py).gt.1.0e-6) then
           tgm = py/px
           y = atan2(py,px)
           qx = x*cos(y)
           qy = x*sin(y)
           det = 1.0-(1.0+tgm**2)*(1.0-(tgm*rad/(qx+tgm*qy))**2)
           if (det.gt.0.0) then  ! there is an intersection for this line so it should be drawn
             drawit = .TRUE.
             hlx(1) = (qx+tgm*qy)*(1.0-sqrt(det))/(1.0+tgm**2)
             hly(1) = qy-(hlx(1)-qx)/tgm
             hlx(2) = (qx+tgm*qy)*(1.0+sqrt(det))/(1.0+tgm**2)
             hly(2) = qy-(hlx(2)-qx)/tgm
           end if
         else  
           if (abs(px).lt.1.0e-6) then
! parallel to the x-axis 
             drawit = .TRUE.
             hlx(1) = sqrt(rad**2-x**2)
             hly(1) = x*py/abs(py)
             hlx(2) = -hlx(1)
             hly(2) = hly(1)       
           else
! parallel to the x-axis 
             drawit = .TRUE.
             hly(1) = sqrt(rad**2-x**2)
             hlx(1) = x*px/abs(px)
             hly(2) = -hly(1)
             hlx(2) = hlx(1)       
           end if
         end if
        end if
end subroutine CalcLine
