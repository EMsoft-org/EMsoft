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
! EMsoft:EMHOLZ.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMHOLZ.f90 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate kinematical HOLZ patterns
!
!> @date 01/05/99  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 revision
!> @date 11/16/15  MDG 3.0 complete revision
!> @date 11/17/15  MDG 3.1 order of pointer argument list corrected in CalcHOLZ
!--------------------------------------------------------------------------
program EMHOLZ

use local
use typedefs
use crystal
use symmetry
use graphics
use files
use HDFsupport
use postscript
use io
use diffraction

IMPLICIT NONE

real(kind=sgl)                  :: camlen, io_real(1)
integer(kind=irg)               :: imanum
character(fnlen)                :: progname, progdesc, gname
type(unitcell),pointer          :: cell
logical                         :: loadingfile
type(gnode)                     :: rlp
type(postscript_type)           :: PS

interface
        subroutine HOLZPage(cell, PS, rlp, camlen)

        use local
        use typedefs
        use postscript
        use diffraction
        use crystal
        use symmetry
        use math
        use io
        use constants

        IMPLICIT NONE

        type(unitcell),pointer,INTENT(IN)       :: cell
        type(postscript_type),INTENT(INOUT)     :: PS
        type(gnode),INTENT(INOUT)               :: rlp
        real(kind=sgl),INTENT(IN)               :: camlen

        end subroutine HOLZPage
end interface

progname = 'EMHOLZ.f90'
progdesc = 'Kinematical HOLZ pattern and HOLZ line simulations'
call EMsoft(progname, progdesc)

allocate(cell)
 
cell%SG%SYM_reduce=.TRUE.

! read crystal information
call ReadValue(' Enter xtal file name : ', gname,"(A)")
cell%fname = gname
call CrystalData(cell)
call GetVoltage(cell,rlp)
call CalcPositions(cell,'v')

call ReadValue(' Camera length L  [mm, real] ', io_real, 1)
camlen = io_real(1)

! open PostScript file
imanum = 1
call PS_openfile(PS, progdesc, imanum)

! generate a set of HOLZ patterns
call HOLZPage(cell, PS, rlp, camlen)

! close Postscript file
call PS_closefile(PS)

end program

!--------------------------------------------------------------------------
!
! SUBROUTINE: HOLZPage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw zone axis HOLZ diffraction pattern and line pattern
! 
!> @date 1/16/02  MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 12/02/14 MDG 3.0 removed camlen as global variable
!> @date 11/16/15 MDG 4.0 complete rewrite without global variables
!--------------------------------------------------------------------------
subroutine HOLZPage(cell, PS, rlp, camlen)

use local
use typedefs
use postscript
use diffraction
use crystal
use symmetry
use math
use io
use constants

IMPLICIT NONE

type(unitcell),pointer,INTENT(IN)       :: cell
type(postscript_type),INTENT(INOUT)     :: PS
type(gnode),INTENT(INOUT)               :: rlp
real(kind=sgl),INTENT(IN)               :: camlen

type (unitcell)                         :: savecell

type(holzreflection),pointer            :: top,temp,bot
type(HOLZvartype)                       :: HOLZvar

logical                                 :: again, first, newzone, nexttop
real(kind=sgl)                          :: negative(2),twopi,thr, oi_real(9), &
                                           RHOLZmax,RHOLZ(20),xo,yo,sc,pos(2),dy
real(kind=sgl),parameter                :: xoff(0:5)=(/0.0,3.3125,0.0,3.3125,0.0,3.3125/), &
                                           yoff(0:5)=(/6.0,6.0,3.0,3.0,0.0,0.0/), &
                                           eps = 1.0E-3
integer(kind=irg)                       :: g1i(3),g2i(3),i,numHOLZ, io_int(1)
real(kind=sgl),parameter                :: le=3.25,he=2.9375

interface

        subroutine ShortestGFOLZ(cell,HOLZvar)

        use local
        use io
        use postscript
        use crystal
        use error

        IMPLICIT NONE

        type(unitcell),pointer,INTENT(IN)       :: cell
        type(HOLZvartype),INTENT(INOUT)         :: HOLZvar
        end subroutine ShortestGFOLZ

        subroutine CalcHOLZ(cell,rlp,PS,top,bot,HOLZvar,N)

        use local
        use io
        use postscript
        use crystal
        use symmetry
        use error
        use diffraction
        use constants

        IMPLICIT NONE

        type(unitcell),pointer,INTENT(IN)       :: cell
        type(gnode),INTENT(INOUT)               :: rlp
        type(postscript_type)                   :: PS
        type(holzreflection),pointer,INTENT(INOUT) :: top, bot
        type(HOLZvartype),INTENT(INOUT)         :: HOLZvar
        integer(kind=irg),INTENT(IN)            :: N 
        end subroutine CalcHOLZ
        
        subroutine PlotHOLZ(HOLZvar,top)
        
        use local
        use io
        use postscript
        
        IMPLICIT NONE
        
        type(HOLZvartype),INTENT(INOUT)            :: HOLZvar
        type(holzreflection),pointer,INTENT(INOUT) :: top 
        end subroutine PlotHOLZ
        
        subroutine PlotHOLZlines(dy, HOLZvar, top)
        
        use local
        use io
        use postscript
        use constants
        
        IMPLICIT NONE
        
        real(kind=sgl),INTENT(IN)                  :: dy
        type(HOLZvartype),INTENT(INOUT)            :: HOLZvar
        type(holzreflection),pointer,INTENT(INOUT) :: top 
        end subroutine PlotHOLZlines
        
        subroutine ReCalcHOLZ(cell,top, HOLZvar)
        
        use local
        use io
        use postscript
        use crystal
        use symmetry
        use error
        use diffraction
        use constants
        
        IMPLICIT NONE
        
        type(unitcell),pointer,INTENT(IN)       :: cell
        type(holzreflection),pointer,INTENT(INOUT) :: top 
        type(HOLZvartype),INTENT(INOUT)         :: HOLZvar
        end subroutine ReCalcHOLZ

end interface


! this program was originally written for comparison with a TEM negative, hence 
! the old-fashioned program style (command line input only) and the strange 
! pattern dimensions ... This will need to be redone in some other way at some point...

! dimensions of a standard TEM negative in inches:  3.9375 x 3.1875
 negative = (/ 3.9375, 3.1875 /)
 HOLZvar%rectangle = negative*0.5
 thr = 1.E-4 
 twopi = 2.0*cPi
 HOLZvar%Imax = 0.0

! camera length
 HOLZvar%laL = sngl(cell%mLambda) * camlen
 oi_real(1) = sngl(cell%mLambda)
 call WriteValue(' wavelength [nm] = ', oi_real, 1, "(F10.6)")
 oi_real(1) = camlen
 call WriteValue(' L         [mm] = ', oi_real, 1, "(f10.2)")
 oi_real(1) = HOLZvar%laL
 call WriteValue(' camera length lambda*L [mm nm] = ', oi_real, 1, "(f10.5)")

! what portion of reciprocal space is covered by the negative along horizontal direction ?
 HOLZvar%Gmax = sqrt(HOLZvar%rectangle(1)**2+HOLZvar%rectangle(2)**2)*25.4/HOLZvar%laL

! this is also the maximum allowable HOLZ radius
 RHOLZmax = HOLZvar%Gmax

! next loop over multiple zone axis orientations or different lattice parameters
 again = .TRUE.
 first = .TRUE.
 do while (again)

! new zone axis or modify lattice parameters ?
  if (first.eqv..TRUE.) then
   newzone = .TRUE.

! get the zone axis
   call GetIndex(cell%hexset,HOLZvar%uvw,'d')
   call ReadValue(' Enter foil thickness [nm, R] (to get relrods of proper length) : ', oi_real, 1)
   HOLZvar%thickness = oi_real(1)
   first = .FALSE.

! allocate the linked list to store all reflections
   if (.not.associated(top)) then 
     allocate(top)
     bot => top
     nullify(bot%next)
   end if
  else
! either get a new zone or change the lattice parameters and keep the zone
   call ReadValue(' New zone (1) or change lattice parameters for present zone (2) ', io_int, 1)
   if (io_int(1).eq.1) then
    newzone = .TRUE.
    call GetIndex(cell%hexset,HOLZvar%uvw,'d')
    cell = savecell
! deallocate the previous linked list and allocate a new one
    temp => top%next
    do while (associated(temp%next))
     deallocate(top)
     top => temp
     temp => top%next
    end do
    deallocate(top)
    allocate(top)
    bot => top
    nullify(bot%next)
   else
! show the current lattice parameters and save the parameters
     newzone = .FALSE.
   end if
  end if

! it is a new zone, so draw the diffraction pattern on the top half of the page
  if (newzone.eqv..TRUE.) then
! get the basis vectors g1 and g2
    call ShortestG(cell,HOLZvar%uvw,g1i,g2i,i)
    HOLZvar%g1 = float(g1i); HOLZvar%g2 = float(g2i)
    
! get the beam divergence angle to determine the diameter of the central disk
    oi_real(1) = 500.0*minval( (/ CalcDiffAngle(cell,g1i(1),g1i(2),g1i(3)), &
                               CalcDiffAngle(cell,g2i(1),g2i(2),g2i(3)) /) )
    call WriteValue(' Maximum disk diameter without overlap [mrad]= ', oi_real, 1, "(f10.4)")
    call ReadValue(' Enter the beam divergence angle [mrad, R] ', oi_real, 1); 
    HOLZvar%thetac = oi_real(1)*0.001
    
! distance between consecutive HOLZ layers in nm-1
    HOLZvar%H = 1.0/CalcLength(cell,float(HOLZvar%uvw),'d')

! determine g3 basis vector
    call CalcCross(cell, HOLZvar%g1,HOLZvar%g2,HOLZvar%g3,'r','r',1)
    call NormVec(cell,HOLZvar%g3,'r')
    HOLZvar%g3 = HOLZvar%H * HOLZvar%g3

! get foil normal
    call Message('Enter Foil Normal F [real space indices]',"(A)")
    call GetIndex(cell%hexset,HOLZvar%FN,'d')
! compute components of FN with respect to g1, g2, g3
    call TransSpace(cell,float(HOLZvar%FN),HOLZvar%FNr,'d','r')
    call NormVec(cell,HOLZvar%FNr,'r')
    HOLZvar%FNg = (/ CalcDot(cell, HOLZvar%FNr,HOLZvar%g1,'r'), CalcDot(cell, HOLZvar%FNr,HOLZvar%g2,'r'), &
             CalcDot(cell, HOLZvar%FNr,HOLZvar%g3,'r') /)

! determine shortest vector of FOLZ layer
    call ShortestGFOLZ(cell,HOLZvar)
    oi_real(1:3) = HOLZvar%gp(1)*HOLZvar%g1(1:3)+HOLZvar%gp(2)*HOLZvar%g2(1:3)
    call WriteValue(' HOLZ shift vector = ', oi_real, 3, "(3f9.4)") 

! get Laue center in terms of g1 and g2
    oi_real(1:3) = HOLZvar%g1(1:3)
    oi_real(4:6) = HOLZvar%g2(1:3)
    oi_real(7:9) = HOLZvar%g3(1:3)
    call WriteValue( 'The new basis vectors for this zone axis are ', &
          oi_real, 9, "(/'g1 = ',3f10.5,/'g2 = ',3f10.5,/'g3 = ',3f10.5,/)")
    oi_real(1) = HOLZvar%H
    call WriteValue(' reciprocal interplanar spacing H = ', oi_real, 1, "(F10.4,' nm^-1'/)")
    call ReadValue(' Enter the coordinates of the Laue center with respect to g1 and g2', oi_real, 2)
    HOLZvar%LC1 = oi_real(1)
    HOLZvar%LC2 = oi_real(2)

! compute how many HOLZ layers need to be drawn.
! this follows from the camera length and the size of the micrograph
    i=1
    numHOLZ = 0
    do while(i.lt.20)
     RHOLZ(i) = sqrt(2.0*HOLZvar%H*float(i)/cell%mLambda - (float(i)*HOLZvar%H)**2)    
     if (RHOLZ(i).lt.RHOLZmax) numHOLZ = numHOLZ+1
     i=i+1
    end do
    
! print the  number and radii of the HOLZ rings in the field of view
    call Message('Number and radii of possible HOLZ rings inside field of view',"(A)")
    oi_real(1) = RHOLZmax
    call WriteValue(' RHOLZmax = ', oi_real, 1, "(F10.5)")
    do i=1,numHOLZ
      oi_real(1)=float(i); oi_real(2)=RHOLZ(i)
      call WriteValue(' Ring ', oi_real, 2, "(F5.0,3x,F10.5)")
    end do

! do page preamble stuff if this is a new page
! [This assumes that the PostScript file has already been opened]
    if (newzone) then
     call PS_newpage(PS,.FALSE.,'Kinematical HOLZ Diffraction Patterns')
     call PS_text(5.25,-0.05,'scale bar in reciprocal nm')
     call PS_textvar(5.25,PS % psfigheight+0.02,'Camera Constant [nm mm]',HOLZvar%laL)
     call PS_setfont(PSfonts(2),0.15)
     call PS_text(-0.25,PS % psfigheight+0.02,'Structure File : '//cell % fname)
    end if

! draw frame and related stuff
    xo = 2.25
    yo = 5.00
    HOLZvar%PX = xo + HOLZvar%rectangle(1)
    HOLZvar%PY = yo + HOLZvar%rectangle(2)
    HOLZvar%CBEDrad = 1.5
    HOLZvar%CBEDsc = 1.3
    call PS_setlinewidth(0.012)
    call PS_balloon(xo,yo,negative(1),negative(2),0.0312)

! zone axis
    call PS_setfont(PSfonts(2),0.12)
    call PS_text(xo+0.05,yo+negative(2)+0.12,'Zone axis ')
    call PrintIndices('d',cell%hexset,HOLZvar%uvw(1),HOLZvar%uvw(2),HOLZvar%uvw(3),xo+0.6,yo+negative(2)+0.12)

! add other data lines to the upper left
    call PS_setfont(PSfonts(2),0.15)
    call PS_textvar(-0.25,PS%psfigheight-0.18,'Acc. Voltage [kV] ',sngl(cell%voltage))
    call PS_text(-0.25,PS%psfigheight-0.38,'Foil normal ')
    call PrintIndices('d',cell%hexset,HOLZvar%FN(1),HOLZvar%FN(2),HOLZvar%FN(3),-0.25+1.5,PS%psfigheight-0.38)
    call PS_textvar(-0.25,PS%psfigheight-0.58,'Foil thickness [nm] ',HOLZvar%thickness)
    call PS_text(-0.25,PS%psfigheight-0.78,'Laue center ')
    call PS_textvar(-0.25+1.1,PS%psfigheight-0.78,'',HOLZvar%LC1)
    call PS_textvar(-0.25+1.6,PS%psfigheight-0.78,'',HOLZvar%LC2)

! HOLZ ring radii
    call PS_text(xo-1.5,PS%psfigheight-1.45,'HOLZ radii [nm-1] ')
    do i=1,numHOLZ
        call PS_textint(xo-1.5,PS%psfigheight-1.5-float(i)*0.14,'',i)
        call PS_textvar(xo-1.3,PS%psfigheight-1.5-float(i)*0.14,'',RHOLZ(i))
    end do

! CBED 000 disk text
    call PS_setfont(PSfonts(2),0.12)
    call PS_textvar(-0.25,0.5,'Convergence angle [mrad] ',HOLZvar%thetac*1000.0)

! lattice parameters
    call PS_setfont(PSfonts(4),0.14)
    call PS_text(-0.25,2.00,'a :')
    call PS_text(-0.25,1.84,'b :') 
    call PS_text(-0.25,1.68,'c :')
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,2.00,cell % a
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.84,cell % b
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.68,cell % c
    call PS_setfont(PSfonts(1),0.14)
    call PS_text(-0.25,1.52,'a :')
    call PS_text(-0.25,1.36,'b :')
    call PS_text(-0.25,1.20,'g :')
    call PS_setfont(PSfonts(4),0.14)
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.52,cell % alpha
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.36,cell % beta 
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.20,cell % gamma

! scale bar (sc is the conversion factor from nm-1 to inches)
    sc = HOLZvar%laL/25.4
    call PS_setlinewidth(0.020)
    call PS_line(xo+0.05,yo+0.06,xo+0.05+5.0*sc,yo+0.06)
    call PS_setfont(PSfonts(2),0.15)
    call PS_text(xo+0.05+2.5*sc,yo+0.10,'5 ')

! plot origin of reciprocal space 
    call PS_filledcircle(HOLZvar%PX,HOLZvar%PY,0.03,0.0)

! set clip path
    call PS_closepath
    call PS_gsave
    call PS_move(xo,yo)
    call PS_draw(xo,yo+negative(2))
    call PS_draw(xo+negative(1),yo+negative(2))
    call PS_draw(xo+negative(1),yo)
    call PS_clippath
    call PS_newpath

! now we are ready to compute the HOLZ layers and store them in the linked list
! first the ZOLZ layer
    call CalcHOLZ(cell,rlp,PS,top,bot,HOLZvar,0)

! then the HOLZ layers 
    do i=1,numHOLZ
      call PS_setlinewidth(0.005)
      call PS_circle(HOLZvar%PX,HOLZvar%PY,RHOLZ(i)*HOLZvar%laL/25.4)
      call CalcHOLZ(cell,rlp,PS,top,bot,HOLZvar,i)
    end do

! once that is done, we can determine the intensities to be drawn and 
! draw all reflections.
    call PlotHOLZ(HOLZvar,top)

! and eliminate the current clippath
    call PS_closepath
    call PS_grestore

! draw the vectors g1 and g2, and the projection gp of G
    call PS_setlinewidth(0.02)
    call PS_filledcircle(0.5,HOLZvar%PY-2.5,0.015,0.0)

! g1
    pos = matmul(HOLZvar%gtoc,(/1.0,0.0/) )
    HOLZvar%glen = 2.0*sqrt(pos(1)**2+pos(2)**2)
    pos = pos/HOLZvar%glen
    call PS_line(0.5,HOLZvar%PY-2.5,0.5+pos(1),HOLZvar%PY-2.5+pos(2))
    call PS_filledcircle(0.5+pos(1),HOLZvar%PY-2.5+pos(2),0.04,0.0)
    call PrintIndices('r',cell%hexset,int(HOLZvar%g1(1)),int(HOLZvar%g1(2)),int(HOLZvar%g1(3)),0.5+pos(1)+0.1, &
                      HOLZvar%PY-2.5+pos(2))

! g2
    pos = matmul(HOLZvar%gtoc,(/0.0,1.0/) )
    pos = pos/HOLZvar%glen
    call PS_line(0.5,HOLZvar%PY-2.5,0.5+pos(1),HOLZvar%PY-2.5+pos(2))
    call PS_filledcircle(0.5+pos(1),HOLZvar%PY-2.5+pos(2),0.04,0.0)
    call PrintIndices('r',cell%hexset,int(HOLZvar%g2(1)),int(HOLZvar%g2(2)),int(HOLZvar%g2(3)),0.5+pos(1)+0.1, &
                      HOLZvar%PY-2.5+pos(2))

! and then the projection of G onto g1,g2
    pos = matmul(HOLZvar%gtoc,(/HOLZvar%gp(1),HOLZvar%gp(2)/) )
    pos = pos/HOLZvar%glen
    call PS_setlinewidth(0.02)
    call PS_line(0.45+pos(1),HOLZvar%PY-2.5+pos(2),0.55+pos(1),HOLZvar%PY-2.5+pos(2))
    call PS_line(0.5+pos(1),HOLZvar%PY-2.45+pos(2),0.5+pos(1),HOLZvar%PY-2.55+pos(2))
    call PS_text(-0.5,3.2,'Basis vectors g1, g2,')
    call PS_text(-0.5,3.0,'and projection of G (cross)') 
! draw fixed radius circle for bright field CBED disk  
    call PS_setlinewidth(0.025)
    call PS_circle(HOLZvar%PX,yo-2.5,HOLZvar%CBEDrad)
! indicate center of pattern
    call PS_setlinewidth(0.01)
    call PS_line(HOLZvar%PX-0.05,yo-2.5,HOLZvar%PX+0.05,yo-2.5)
    call PS_line(HOLZvar%PX,yo-2.45,HOLZvar%PX,yo-2.55)

    call PlotHOLZlines(0.0,HOLZvar,top)
    nexttop = .TRUE.
  else  ! this is not a new zone axis
! let the user define new lattice parameters 
    savecell = cell
    oi_real(1) = cell%a; oi_real(2) = cell%b; oi_real(3) = cell%c
    call WriteValue(' Current lattice parameters [nm] ', oi_real, 3, "(/'a = ',f7.5,', b = ',f7.5,', c = ',f7.5)")
    oi_real(1) = cell%alpha; oi_real(2) = cell%beta; oi_real(3) = cell%gamma
    call WriteValue('', oi_real, 3, "(/'alpha = ',f7.2,', beta = ',f7.2,', gamma = ',f7.2)")

! ask for the new parameters (all must be entered) and recompute metric information
    call ReadValue(' Enter new lattice parameters a, b, and c [nm] ', oi_real, 3)
    cell%a = oi_real(1); cell%b = oi_real(2); cell%c = oi_real(3)
    call ReadValue(' Enter new angles alpha, beta, and gamma [degrees] ', oi_real, 3)
    cell%alpha = oi_real(1); cell%beta = oi_real(2); cell%gamma = oi_real(3)
    call CalcMatrices(cell)

! redo the geometry
    call ReCalcHOLZ(cell,top,HOLZvar)

! move to top or bottom for next drawing ?
    if (nexttop.eqv..TRUE.) then
     call PS_newpage(PS,.FALSE.,'Kinematical HOLZ Diffraction Patterns')
     dy = 4.25
     nexttop=.FALSE.
    else
     dy = -0.25
     nexttop=.TRUE.
    end if

! CBED 000 disk text
    call PS_setfont(PSfonts(2),0.12)
    call PS_textvar(-0.25,0.5+dy,'Convergence angle [mrad] ',HOLZvar%thetac*1000.0)

! lattice parameters
    call PS_setfont(PSfonts(4),0.14)
    call PS_text(-0.25,2.00+dy,'a :')
    call PS_text(-0.25,1.84+dy,'b :') 
    call PS_text(-0.25,1.68+dy,'c :')
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,2.00+dy,cell % a
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.84+dy,cell % b
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.68+dy,cell % c
    call PS_setfont(PSfonts(1),0.14)
    call PS_text(-0.25,1.52+dy,'a :')
    call PS_text(-0.25,1.36+dy,'b :')
    call PS_text(-0.25,1.20+dy,'g :')
    call PS_setfont(PSfonts(4),0.14)
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.52+dy,cell % alpha
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.36+dy,cell % beta 
    write (psunit,"(1x,F12.7,' ',F12.7,' M (',F12.4,') show')") 0.0,1.20+dy,cell % gamma

! draw fixed radius circle for bright field CBED disk  
    call PS_setlinewidth(0.025)
    call PS_circle(HOLZvar%PX,yo-2.5+dy,HOLZvar%CBEDrad)

! indicate center of pattern
    call PS_setlinewidth(0.01)
    call PS_line(HOLZvar%PX-0.05,yo-2.5+dy,HOLZvar%PX+0.05,yo-2.5+dy)
    call PS_line(HOLZvar%PX,yo-2.45+dy,HOLZvar%PX,yo-2.55+dy)

    call PlotHOLZlines(dy,HOLZvar,top)
  end if ! newzone .eq. .TRUE.

  call ReadValue(' Another pattern ? [1/0] ', io_int, 1)
  if (io_int(1).ne.1) again=.FALSE.
 end do  ! end of main loop

end subroutine

!--------------------------------------------------------------------------
! 
! SUBROUTINE:ShortestGFOLZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief find the shortest G vector
!
!> @details  see chapter 3
!
! 
!> @date 01/29/02 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 11/16/15 MDG 3.0 complete rewrite
!--------------------------------------------------------------------------
subroutine ShortestGFOLZ(cell,HOLZvar)

use local
use io
use postscript
use crystal
use error

IMPLICIT NONE

type(unitcell),pointer,INTENT(IN)       :: cell
type(HOLZvartype),INTENT(INOUT)         :: HOLZvar

real(kind=sgl)                          :: gmin,gam11,gam12,gam22
integer(kind=irg),parameter             :: inm = 8
integer(kind=irg)                       :: ih,ik,il,NN, oi_int(1)

! look for the shortest reflection satisfying hu+kv+lw = 1
! This could be replaced by code from Jackson's paper (1987),
! but it does essentially the same thing.
 gmin = 100.0
 NN=1
 do while((gmin.eq.100.0).and.(NN.lt.4))
  do ih=-inm,inm
   do ik=-inm,inm
    do il=-inm,inm
! does this reflection lie in the plane NN ?
     if ((ih*HOLZvar%uvw(1)+ik*HOLZvar%uvw(2)+il*HOLZvar%uvw(3)).eq.NN) then
      HOLZvar%glen = CalcLength(cell,float((/ih,ik,il/)),'r')
      if (HOLZvar%glen.lt.gmin) then
       gmin = HOLZvar%glen
       HOLZvar%gshort = float( (/ ih,ik,il /) )
      end if
     end if
    end do
   end do
  end do
  oi_int(1) = NN
  call WriteValue(' Could not find any reflections with hu+kv+lw = ', oi_int, 1, "(I2)")
  NN = NN+1
 end do
 if (gmin.eq.100.0) then ! for some reason there is no reflection with N<=3 ...
  call FatalError('ShortestGFOLZ: ',' could not find any reflections with hu+kv+lw<=3 ...')
 end if

! projected components of G
 gam11 = CalcDot(cell,HOLZvar%g1,HOLZvar%g1,'r')
 gam12 = CalcDot(cell,HOLZvar%g1,HOLZvar%g2,'r')
 gam22 = CalcDot(cell,HOLZvar%g2,HOLZvar%g2,'r')
 gmin = 1.0/(gam11*gam22-gam12**2)
 HOLZvar%gp(1) = (CalcDot(cell,HOLZvar%gshort,HOLZvar%g1,'r')*gam22-&
                  CalcDot(cell,HOLZvar%gshort,HOLZvar%g2,'r')*gam12)*gmin
 HOLZvar%gp(2) = (CalcDot(cell,HOLZvar%gshort,HOLZvar%g2,'r')*gam11-&
                  CalcDot(cell,HOLZvar%gshort,HOLZvar%g1,'r')*gam12)*gmin

end subroutine ShortestGFOLZ


!--------------------------------------------------------------------------
! 
! SUBROUTINE:CalcHOLZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the HOLZ reflections for zone N 
!
!> @param N HOLZ number
!
! 
!> @date 01/29/02 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 11/16/15 MDG 3.0 complete rewrite
!--------------------------------------------------------------------------
subroutine CalcHOLZ(cell,rlp,PS,top,bot,HOLZvar,N)

use local
use io
use postscript
use crystal
use symmetry
use error
use diffraction
use constants

IMPLICIT NONE

type(unitcell),pointer,INTENT(IN)       :: cell
type(gnode),INTENT(INOUT)               :: rlp
type(postscript_type)                   :: PS
type(holzreflection),pointer,INTENT(INOUT) :: top, bot
type(HOLZvartype),INTENT(INOUT)         :: HOLZvar
integer(kind=irg),INTENT(IN)            :: N 

integer(kind=irg)                       :: inmhkl(2),i,j,nref,istat, oi_int(1)
real(kind=sgl)                          :: correction,gg(3),Ig,smax,gxy(2),pxy(2),exer,sgdenom,x,tgm,qx,qy,y,det,LC3, &
                                           ll(3),lpg(3),gplen
logical                                 :: a,dbdiff

interface
        subroutine PlotHOLZ(HOLZvar,top)
        
        use local
        use io
        use postscript
        
        IMPLICIT NONE
        
        type(HOLZvartype),INTENT(INOUT)            :: HOLZvar
        type(holzreflection),pointer,INTENT(INOUT) :: top 
        end subroutine PlotHOLZ
end interface


 call Message('Computing HOLZ reflection data',"(/A)")

! set the index boundaries
 inmhkl(1) = int(1.1*HOLZvar%Gmax/CalcLength(cell,HOLZvar%g1,'r'))
 inmhkl(2) = int(1.1*HOLZvar%Gmax/CalcLength(cell,HOLZvar%g2,'r'))

! we will take g1 to be the x-axis of the pattern
! so the transformation matrix from g1,g2 to Cartesian is given by
 if (N.eq.0) then
  HOLZvar%phi = CalcAngle(cell,HOLZvar%g1,HOLZvar%g2,'r')
  HOLZvar%glen = CalcLength(cell,HOLZvar%g2,'r')
  HOLZvar%gtoc(1,1) = CalcLength(cell,HOLZvar%g1,'r')
  HOLZvar%gtoc(1,2) = HOLZvar%glen*cos(HOLZvar%phi)
  HOLZvar%gtoc(2,1) = 0.0
  HOLZvar%gtoc(2,2) = HOLZvar%glen*sin(HOLZvar%phi)
  HOLZvar%gtoc = HOLZvar%gtoc*HOLZvar%laL/25.4  ! nm^-1 to inches via the camera length
end if

! loop over all possible reflections in this layer (2D loop !!!)
 smax = 20.0/HOLZvar%thickness  ! the number 20 is arbitrary and could be changed at will
 nref=0
 do i=-inmhkl(1),inmhkl(1)
  do j=-inmhkl(2),inmhkl(2)
! make sure this reflection is close enough to the Ewald sphere to give some intensity;
! use crystal thickness to determine this according to the argument of the sinc function.
      gg = i*HOLZvar%g1 + j*HOLZvar%g2 + N*HOLZvar%gshort

! Do this only for those reflections that are allowed by
! the lattice centering !
      a = IsGAllowed(cell,int(gg))
      if (a) then

! compute excitation error, including Laue center, foil normal, and HOLZ reflection.
       HOLZvar%glen = CalcLength(cell,gg,'r')
       if (HOLZvar%glen.ne.0.0) then
         ll = HOLZvar%LC1*HOLZvar%g1 + HOLZvar%LC2*HOLZvar%g2
         lpg = ll + gg
         gplen = CalcLength(cell,lpg,'r')
         LC3 = sqrt(1.0-cell%mLambda**2*CalcLength(cell,ll,'r')**2)
         if (gplen.eq.0.0) then
           exer = -cell%mLambda*CalcDot(cell,gg,2.0*ll+gg,'r')/ &
                   (2.0*LC3*cos(CalcAngle(cell,float(HOLZvar%uvw),float(HOLZvar%FN),'d')) )
         else
           sgdenom = 2.0*LC3*cos(CalcAngle(cell,float(HOLZvar%uvw),float(HOLZvar%FN),'d'))- &
                   2.0*cell%mLambda*gplen*cos(CalcAngle(cell,lpg,HOLZvar%FNr,'r'))
           exer = -(cell%mLambda*CalcDot(cell,gg,2.0*ll+gg,'r')-2.0*LC3*gplen*cos(CalcAngle(cell,HOLZvar%g3,lpg,'r')))/sgdenom
         end if
       else
         exer = 10000.0
       end if

! exclude the 000 reflection
       if (abs(exer).le.smax) then

! OK, it is close enough.  Does it have any intensity ?
        call CalcUcg(cell,rlp,int(gg))

! yes, it does.  get the scaled intensity using the sinc function
        Ig = rlp%Vmod**2
        if (Ig.lt.1.0e-16) then 
          dbdiff = .TRUE.
        else
          dbdiff = .FALSE.
        end if
        if (abs(exer).ge.1.0e-5) then
         Ig = Ig * (sin(cPi*exer*HOLZvar%thickness)/(cPi*exer*HOLZvar%thickness))**2
        end if 

! store maximum intensity
        if (Ig.gt.HOLZvar%Imax) HOLZvar%Imax = Ig

! next, determine the drawing coordinates, first in terms of g1 and g2
        correction = 1.0/(1.0-cell%mLambda*HOLZvar%H*(float(N)+exer*HOLZvar%FNg(3)))
        gxy = (/ (i+N*HOLZvar%gp(1)+exer*HOLZvar%FNg(1)), (j+N*HOLZvar%gp(2)+exer*HOLZvar%FNg(2))  /) * correction

! convert to Cartesian drawing coordinates
        pxy = matmul(HOLZvar%gtoc,gxy)

! and add the point to the linked list 
        allocate(bot%next,stat=istat)
        if (istat.ne.0) then
          call PlotHOLZ(HOLZvar,top)
          call PS_closefile(PS)
          call FatalError('CalcHOLZ: ',' unable to allocate memory for linked list')
        end if
        bot => bot%next
        nullify(bot%next)
        bot%hkl = gg
        bot%n1  = i
        bot%n2  = j
        bot%N   = N
        bot%sg  = exer
        bot%Ig  = Ig
        bot%pxy = pxy
        bot%dbdiff = dbdiff
        nref = nref+1

! would this point contribute to the HOLZ line drawing in the central disk ?
        HOLZvar%phi = asin(cell%mLambda*HOLZvar%glen*0.5) - asin(N*HOLZvar%H/HOLZvar%glen)
        bot%draw = .FALSE.
        if (abs(HOLZvar%phi).le.HOLZvar%thetac) then
         x = HOLZvar%phi/HOLZvar%thetac  *  HOLZvar%CBEDrad
         if (pxy(1).ne.0.0) then
           tgm = pxy(2)/pxy(1)
           y = atan2(pxy(2),pxy(1))
           qx = x*cos(y)
           qy = x*sin(y)
           det = 1.0-(1.0+tgm**2)*(1.0-(tgm*HOLZvar%CBEDrad*HOLZvar%CBEDsc/(qx+tgm*qy))**2)
           if (det.gt.0.0) then  ! there is an intersection for this line so it should be drawn
             bot%draw = .TRUE.
             bot%hlx(1) = (qx+tgm*qy)*(1.0-sqrt(det))/(1.0+tgm**2)
             bot%hly(1) = qy-(bot%hlx(1)-qx)/tgm
             bot%hlx(2) = (qx+tgm*qy)*(1.0+sqrt(det))/(1.0+tgm**2)
             bot%hly(2) = qy-(bot%hlx(2)-qx)/tgm
           end if
         else  ! parallel to the y-axis (easy to deal with)
             bot%draw = .TRUE.
             bot%hlx(1) = qx
             bot%hly(1) = sqrt((HOLZvar%CBEDrad*HOLZvar%CBEDsc)**2-qx**2)
             bot%hlx(2) = qx
             bot%hly(2) = -bot%hly(1)      
         end if
        end if

       end if
    end if
   end do
  end do

  oi_int(1) = nref
  Call WriteValue(' number of reflections to be drawn : ', oi_int, 1, "(I6)")

end subroutine CalcHOLZ

!--------------------------------------------------------------------------
! 
! SUBROUTINE:ReCalcHOLZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the HOLZ reflections for zone N for new lattice parameters
!
!> @date 2/03/02   MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!--------------------------------------------------------------------------
subroutine ReCalcHOLZ(cell,top, HOLZvar)

use local
use io
use postscript
use crystal
use symmetry
use error
use diffraction
use constants

IMPLICIT NONE

type(unitcell),pointer,INTENT(IN)       :: cell
type(holzreflection),pointer,INTENT(INOUT) :: top 
type(HOLZvartype),INTENT(INOUT)         :: HOLZvar

type(holzreflection),pointer            :: temp

real(kind=sgl)                          :: correction,gg(3),gxy(2),pxy(2),exer,sgdenom,x,tgm,qx,qy, &
                                           y,det,LC3,ll(3),lpg(3),gplen

 call Message('Computing HOLZ reflection data',"(/A/)")

    temp => top%next
    do while (associated(temp))
       gg = temp%hkl

! compute excitation error
       HOLZvar%glen = CalcLength(cell,gg,'r')
       ll = HOLZvar%LC1*HOLZvar%g1 + HOLZvar%LC2*HOLZvar%g2
       lpg = ll + gg
       gplen = CalcLength(cell,lpg,'r')
       LC3 = sqrt(1.0-cell%mLambda**2*CalcLength(cell,ll,'r')**2)
       if (gplen.eq.0.0) then
         exer = -cell%mLambda*CalcDot(cell,gg,2.0*ll+gg,'r')/2.0*LC3*cos(CalcAngle(cell,float(HOLZvar%uvw),float(HOLZvar%FN),'d'))      
       else
         sgdenom = 2.0*LC3*cos(CalcAngle(cell,float(HOLZvar%uvw),float(HOLZvar%FN),'d'))- &
                 2.0*cell%mLambda*gplen*cos(CalcAngle(cell,lpg,HOLZvar%FNr,'r'))
         exer = -(cell%mLambda*CalcDot(cell,gg,2.0*ll+gg,'r')-2.0*LC3*gplen*cos(CalcAngle(cell,HOLZvar%g3,lpg,'r')))/sgdenom
       end if

! next, determine the drawing coordinates, first in terms of g1 and g2
       correction = 1.0/(1.0-cell%mLambda*HOLZvar%H*(float(temp%N)+exer*HOLZvar%FNg(3)))
       gxy = (/ (temp%n1+temp%N*HOLZvar%gp(1)+exer*HOLZvar%FNg(1)), &
                (temp%n2+temp%N*HOLZvar%gp(2)+exer*HOLZvar%FNg(2))  /) * correction

! convert to Cartesian drawing coordinates
       pxy = matmul(HOLZvar%gtoc,gxy)
       HOLZvar%phi = asin(cell%mLambda*HOLZvar%glen*0.5) - asin(temp%N*HOLZvar%H/HOLZvar%glen)
       temp % draw = .FALSE.
       if (abs(HOLZvar%phi).le.HOLZvar%thetac) then
        x = HOLZvar%phi/HOLZvar%thetac  *  HOLZvar%CBEDrad
        if (pxy(1).ne.0.0) then
         tgm = pxy(2)/pxy(1)
         y = atan2(pxy(2),pxy(1))
         qx = x*cos(y)
         qy = x*sin(y)
         det = 1.0-(1.0+tgm**2)*(1.0-(tgm*HOLZvar%CBEDrad*HOLZvar%CBEDsc/(qx+tgm*qy))**2)
         if (det.gt.0.0) then  ! there is an intersection for this line so it should be drawn
          temp%draw = .TRUE.
          temp%hlx(1) = (qx+tgm*qy)*(1.0-sqrt(det))/(1.0+tgm**2)
          temp%hly(1) = qy-(temp%hlx(1)-qx)/tgm
          temp%hlx(2) = (qx+tgm*qy)*(1.0+sqrt(det))/(1.0+tgm**2)
          temp%hly(2) = qy-(temp%hlx(2)-qx)/tgm
         end if
        else  ! parallel to the y-axis (easy to deal with)
          temp%draw = .TRUE.
          temp%hlx(1) = qx
          temp%hly(1) = sqrt((HOLZvar%CBEDrad*HOLZvar%CBEDsc)**2-qx**2)
          temp%hlx(2) = qx
          temp%hly(2) = -temp%hly(1)       
        end if
       end if

! move to the next reflection
       temp=>temp%next
     end do

end subroutine ReCalcHOLZ

!--------------------------------------------------------------------------
! 
! SUBROUTINE:PlotHOLZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a single HOLZ zone axis diffraction pattern
!
!> @date 01/22/02 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 11/16/15 MDG 3.0 complete rewrite
!--------------------------------------------------------------------------
subroutine PlotHOLZ(HOLZvar,top)

use local
use io
use postscript

IMPLICIT NONE

type(HOLZvartype),INTENT(INOUT)            :: HOLZvar
type(holzreflection),pointer,INTENT(INOUT) :: top 

type(holzreflection),pointer               :: temp

real(kind=sgl)                             :: V,qx,qy

 call Message('Plotting HOLZ reflections',"(/A/)")

! point to the top of the linked list
 temp => top%next

! move through the entire list and draw all reflections with exponentially scaled intensity;
 do while (associated(temp))
  V=0.05*(temp%Ig/HOLZvar%Imax)**0.1
  qx = temp%pxy(1)
  qy = temp%pxy(2)

! make sure the point is inside the rectangle
  if ((abs(qx).lt.HOLZvar%rectangle(1)).and.(abs(qy).lt.HOLZvar%rectangle(2))) then
   if (temp%dbdiff) then  ! potential double diffraction spot
    call PS_setlinewidth(0.005)
    call PS_square(HOLZvar%PX+qx,HOLZvar%PY+qy,0.04)
   else ! regular reflection
    call PS_filledcircle(HOLZvar%PX+qx,HOLZvar%PY+qy,V,0.0)
   end if
  end if

! move to the next reflection
  temp=>temp%next
 end do

end subroutine PlotHOLZ


!--------------------------------------------------------------------------
! 
! SUBROUTINE:PlotHOLZlines
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a single HOLZ line convergent beam disk
!
!> @param dy offset parameter
!
!> @date 01/22/02 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 11/16/15 MDG 3.0 complete rewrite
!--------------------------------------------------------------------------

subroutine PlotHOLZlines(dy, HOLZvar, top)

use local
use io
use postscript
use constants

IMPLICIT NONE

real(kind=sgl),INTENT(IN)               :: dy
type(HOLZvartype),INTENT(INOUT)            :: HOLZvar
type(holzreflection),pointer,INTENT(INOUT) :: top 

type(holzreflection),pointer               :: temp

real(kind=sgl)                          :: V,V2,qx,qy,CB
character(12)                           :: txt
integer(kind=irg)                       :: i,nref

 call Message('Plotting HOLZ lines and labels',"(/A/)")

! point to the top of the linked list
 temp => top%next
 nref = 0
 open(unit=30,file='temp.txt',status='unknown',form='formatted')
 call PS_setfont(PSfonts(4),0.08)

! move through the entire list and draw the corresponding HOLZ line across the 000 diffraction disk
 do while (associated(temp))
 
  if (temp%draw.eqv..TRUE.) then ! draw the HOLZ line on the second drawing 
   V=0.015*(temp%Ig/HOLZvar%Imax)**0.1
   V2=1.0-1.0*(temp%Ig/HOLZvar%Imax)**0.1
   if (V2.lt.0.0) V2 = 0.0
   call PS_setlinewidth(V)

! make sure that all the lines will actually fit inside the region of interest
   if ((maxval(abs(temp%hlx)).le.HOLZvar%CBEDrad*HOLZvar%CBEDsc).and. &
       (maxval(abs(temp%hly)).le.HOLZvar%CBEDrad*HOLZvar%CBEDsc)) then
    call PS_line_gray(HOLZvar%PX+temp%hlx(1),2.5+temp%hly(1)+dy,HOLZvar%PX+temp%hlx(2),2.5+temp%hly(2)+dy,V2)

! add indices along continuation of lines
    qx= HOLZvar%PX+1.02*temp%hlx(2)
    qy= 2.5+1.02*temp%hly(2)+dy
    V = 180.0*atan2(temp%hly(2)-temp%hly(1),temp%hlx(2)-temp%hlx(1))/cPi
    write (30,"(1x,I3,1x,I3,1x,I3,1x,3(f10.5,1x))") (temp%hkl(i),i=1,3),qx,qy,V
    nref = nref+1
   end if
  end if

! move to the next reflection
  temp=>temp%next
 end do

! add indices along continuation of lines
 close(unit=30,status='keep')

 CB = HOLZvar%CBEDrad**2
 open(unit=30,file='temp.txt',status='old',form='formatted')
 do i=1,nref
   read (30,"(A12,1x,3(f10.5,1x))") txt,qx,qy,V
   if (((qx-HOLZvar%PX)**2+(qy-(2.5+dy))**2).gt.CB) then
    call PS_move(qx,qy)  ! just outside clipping ring
    write (psunit,*) V,' rotate'
    write (psunit,"(1x,'( ',A12,' ) show')") txt
    write (psunit,*) -V,' rotate'
   end if
 end do
 close(unit=30,status='delete')

end subroutine PlotHOLZlines
