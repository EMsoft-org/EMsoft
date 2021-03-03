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
! EMsoft:postscript.f90
!--------------------------------------------------------------------------
!
! MODULE: postscript
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief A collection of postscript output routines used to create a variety of graphics output
!
!> @details  This file contains a number of Postscript generating subroutines
!> which can be used to generate PS-drawings from a Fortran program.
!> The routines are based on the Pascal version written by J. Fransaer
!> of the Catholic University of Leuven, Belgium.
!> Translated into Fortran-90 by M. De Graef on 12/31/92.
!>
!> A typical program would start as follows\n
!>
!>      call PS_openfile(scale)\n
!>       call PS_drawframe(...)\n
!>       ....
!>
!> and end with \n
!>
!>       call PS_closefile\n
!>
!> The code to draw spheres (sp) is taken from the appendix of 
!> Earl J. Kirklands book on Advanced Computing in Electron Microscopy,
!> and slightly modified to include color.  
!> 
!> All dimensions are in inches 
!> pspage      = pagenumber for multi-page files
!> psfigwidth  = width of drawing (pagewidth - 2 inches for margins = 6.5)
!> psfigheight = height of drawing (pageheight - 2 = 9.0)
!> psscale     = scale factor for overall file
!> psname      = PostScript file name
!> psdash      = used to store a dash pattern
!> fonts       = array with standard PostScript fonts (more may be added)
!>
!> The axis routine has its own common block which has the following entries
!> axw         = the width of the square region inside which the entire
!>               user space is contained (inches)
!> xll,yll     = x and y coordinates (in inches) of the lower left corner of 
!>               the square region (measured from lower left corner of page)
!>
!> The axonometry routine has its own common block

!> @date 1/5/99   MDG 1.0 original
!> @date   12/31/92 MDG 1.0 original
!> @date   1/8/98   MDG 2.0 added new routines
!> @date   7/19/99  MDG 2.1 added sp command tp Postscript preamble
!> @date   5/20/01  MDG 3.0 f90
!> @date  11/27/01  MDG 3.1 added kind support
!> @date  03/25/13 MDG 3.2 checked all IO and updated where needed 
!--------------------------------------------------------------------------
module postscript

use local
use typedefs

! the following postscript preamble is inspired on the one from the old EMS package
! (before it was converted to JEMS) written by Pierre Stadelmann
character(55),parameter,private :: PSpreamble(23) = (/ &
        "%!PS-Adobe-3.0                                         ", &
        "%%Creator:                                             ", &
        "%%Title:                                               ", &
        "%%Pages: (atend)                                       ", &
        "%%EndComments                                          ", &
        "/M {moveto} def /N {newpath} def /L {lineto} def       ", &
        "/S {stroke} def /T {translate} def /R {rotate} def     ", &
        "/F {fill} def /Cl {closepath} def                      ", &
        "/circle {N 0 0 1 0 360 arc Cl F} def                   ", &
        "/sp { gsave T scale 1.0 -0.04 0 { pop 3 array astore   ", &
        "{1.02 mul} forall 3 copy setrgbcolor -0.025 0.030 T    ", &
        "circle 0.93 0.93 scale } for                           ", &
        "pop pop pop grestore } def                             ", &
        "/frame {1.0 setgray N left rad add bottom M            ", &
        "right bottom right top rad arcto L right top left top  ", &
        "rad arcto L left top left bottom rad arcto L left      ", &
        "bottom right bottom rad arcto L Cl F 0.0 setgray N     ", &
        "left rad add bottom M right bottom right top rad       ", &
        "arcto L right top left top rad arcto L left top left   ", &
        "bottom rad arcto L left bottom right bottom rad arcto  ", &
        "L Cl S } def                                           ", &
        "%%EndProlog                                            ", &
        "72 dup scale                                           " /)
!DEC$ ATTRIBUTES DLLEXPORT :: PSpreamble


! font-related stuff
character(20),parameter :: PSlbl = "Written by MDG, 2001"
character(20),parameter :: PSfonts(5) = (/"Symbol              ", &
                                          "Times-Bold          ", &
                                          "Times-BoldItalic    ", &
                                          "Times-Italic        ", &
                                          "Times-Roman         "/)
!DEC$ ATTRIBUTES DLLEXPORT :: PSlbl
!DEC$ ATTRIBUTES DLLEXPORT :: PSfonts

contains 

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_openfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief open postscript file and dump the preamble to the file
!
!> @param PS Postscript Structure
!> @param imanum image number 
!> @param dontask logical to prevent use of SafeOpenFile routine (from files.f90)
! 
!> @todo add A4 paper format as an option
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added PS, progdesc, imanum as arguments
!--------------------------------------------------------------------------
recursive subroutine PS_openfile(PS, progdesc, imanum, dontask)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_openfile

use io
use files

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)    :: PS
!f2py intent(in,out) ::  PS
character(fnlen),INTENT(IN)            :: progdesc
integer(kind=irg),INTENT(INOUT)        :: imanum
!f2py intent(in,out) ::  imanum
logical,INTENT(IN),optional            :: dontask		!< optional parameter to select file opening route
logical	                                :: loadingfile

real(kind=sgl)    		        :: fw, fh		!< page format parameters
integer(kind=irg) 		        :: i			!< loop counter
character(fnlen)                        :: gname

! define the writeable portion of the page (should be made more user-friendly for A4 format...)
 imanum = 0
 PS%psfigwidth=6.5 
 PS%psfigheight=9.0
 PS%psscale=1.0

! open file and dump Prolog and Comments sections
 if (present(dontask)) then
! if we get here, it means that we are using a temporary PostScript file
! and we should not go through the regular SafeOpenFile routine.
   open(unit=psunit,file=trim(EMsoft_toNativePath(PS%psname)),status='unknown',action='write',form='formatted')
   call Message('Opening temporary file for PostScript output', frm = "(A)")
 else
   call ReadValue(' Enter Postscript file name : ', gname,"(A)")
   PS%psname = gname
   open(unit=psunit,file=trim(EMsoft_toNativePath(PS%psname)),status='unknown',form='formatted')
 end if

! write the preamble
 write (psunit,"(A)") PSpreamble(1)
 write (psunit,"(A,' ',A)") trim(PSpreamble(2)), EMsoft_getUsername()
 write (psunit,"(A,' ',A)") trim(PSpreamble(3)), progdesc
 do i=4,23
  write (psunit,"(A)") PSpreamble(i)
 end do 

! determine lower left corner and translate to that point
 fw=0.5*(8.50-PS%psscale*PS%psfigwidth)
 fh=0.5*(11.0-PS%psscale*PS%psfigheight)
 write (psunit,"(F12.7,' ',F12.7,' T')") fw,fh
 write (psunit,"(F12.7,' setlinewidth')") 0.01
 write (psunit,"(F12.7,' ',F12.7,' scale')") PS%psscale,PS%psscale

! set page number counter to zero
 PS%pspage = 0

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_closefile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief close and save postscript file 
! 
!> @param PS Postscript Structure
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added PS as argument
!--------------------------------------------------------------------------
recursive subroutine PS_closefile(PS)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_closefile

use files

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)    :: PS
!f2py intent(in,out) ::  PS

! write the trailer to the file 
 write (psunit,*) 'showpage'
 write (psunit,"(' %%Pages: ',i3)") PS%pspage
 write (psunit,"(' %%EOF')")
 
! and close it
! call SafeCloseFile('ps','keep',PS%psname)
  close(unit=psunit,status='keep')

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_newpage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief start a new page inthe PS file
!
!> @param PS Postscript structure
!> @param frm logical to draw a frame or not
!> @param btxt string for the title balloon
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added PS as argument
!--------------------------------------------------------------------------
recursive subroutine PS_newpage(PS, frm, btxt)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_newpage

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)    :: PS
!f2py intent(in,out) ::  PS

logical,INTENT(IN)        	        :: frm		!< logical draw frame or not
character(*),INTENT(IN)   	        :: btxt		!< character string for header balloon

 if (PS%pspage.ne.0) then
  write (psunit,*) 'showpage saveobj restore'
 end if

! update the page counter
 PS%pspage = PS%pspage + 1
 write (psunit,"(' %%Page: ',i3,i3)") PS%pspage-1,PS%pspage
 write (psunit,*) '/saveobj save def'
 
! prepare to draw a header balloon 
 call PS_setfont(PSfonts(3),0.18)
 write (psunit,"(1x,F12.7,' ',F12.7,' M (',I8,') show')") 6.75,PS%psfigheight-0.2,PS%pspage
 if (frm.eqv..TRUE.) then  ! we need a frame
  call PS_drawframe(6.75,PS%psfigheight)
 endif
 
! output the text balloon
 call PS_setlinewidth(0.012)
 call PS_textballoon(2.0,9.2,btxt,PSfonts(2),0.25)
 call PS_setfont(PSfonts(5),0.07)
 call PS_text(0.1,-0.1,PSlbl)
 
end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_cellinfo
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  write unit cell information (for drawing programs)
!
!> @param PS Postscript structure
!> @param cell unit cell pointer
!> @param xo  x-position of output
!> @param yo  y-position of output
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added PS and cell as argument
!--------------------------------------------------------------------------
recursive subroutine PS_cellinfo(PS, cell, xo, yo)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_cellinfo

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)        :: PS
!f2py intent(in,out) ::  PS
type(unitcell)                             :: cell

real(kind=sgl),INTENT(IN)                  :: xo,yo		!< starting location for output 

 call PS_setfont(PSfonts(2),0.12/PS%psscale)
 call PS_text(xo,yo,'Filename: '//cell%fname)
 call PS_text(xo,yo-0.16,'a [nm]          ')
 call PS_text(xo,yo-0.30,'b [nm]          ')
 call PS_text(xo,yo-0.44,'c [nm]          ')
 call PS_text(xo,yo-0.58,'alpha [deg]     ')
 call PS_text(xo,yo-0.72,'beta  [deg]     ')
 call PS_text(xo,yo-0.86,'gamma [deg]     ')
 call PS_textvar8(xo+0.75,yo-0.16,': ',cell%a)
 call PS_textvar8(xo+0.75,yo-0.30,': ',cell%b)
 call PS_textvar8(xo+0.75,yo-0.44,': ',cell%c)
 call PS_textvar8(xo+0.75,yo-0.58,': ',cell%alpha)
 call PS_textvar8(xo+0.75,yo-0.72,': ',cell%beta)
 call PS_textvar8(xo+0.75,yo-0.86,': ',cell%gamma)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_clippath
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  make the last path the clippath
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_clippath
!DEC$ ATTRIBUTES DLLEXPORT :: PS_clippath

IMPLICIT NONE

 write (psunit,"('Cl clip')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_translate
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  redefine the origin of the current coordinate frame
!
!> @param x  x-position of new origin
!> @param y  y-position of new origin
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_translate(x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_translate

IMPLICIT NONE

real(kind=sgl),INTENT(IN) 	 :: x,y	!< coordinates of new origin

 write (psunit,"(F18.7,' ',F18.7,' T')") x,y

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_move
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  move to a given location 
!
!> @param x  x-position of move
!> @param y  y-position of move
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_move(x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_move

IMPLICIT NONE

real(kind=sgl),INTENT(IN)  :: x,y	!< move to this location

 write (psunit,"(F18.7,' ',F18.7,' M')") x,y

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_draw
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a line from the current point to the new point
!
!> @param x  x-position of end point
!> @param y  y-position of end point
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_draw(x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_draw

IMPLICIT NONE

real(kind=sgl),INTENT(IN)  :: x,y	!< end coordinates of draw

write (psunit,"(F18.7,' ',F18.7,' L')") x,y

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_draw
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a line with a given gray level from the current point to the new point
!
!> @param x1  x-position of start point
!> @param y1  y-position of start point
!> @param x2  x-position of end point
!> @param y2  y-position of end point
!> @param gray gray level at which to draw the line
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_line_gray(x1,y1,x2,y2,gray)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_line_gray
        
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x1,y1		!< starting point
real(kind=sgl),INTENT(IN)  	:: x2,y2		!< end point
real(kind=sgl),INTENT(IN)  	:: gray		!< gray level

  write (psunit,"(F18.7,' setgray ')") gray  
  call PS_move(x1,y1)
  call PS_draw(x2,y2)

! and reset the gray level to black
  write (psunit,"('S  0.0 setgray ')")      

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_setlinewidth
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  set the line width
!
!> @param x  line width
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_setlinewidth(x)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_setlinewidth

IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x		!< line width parameter

 write (psunit,"(F12.7,' setlinewidth')") x

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_square
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a square
!
!> @param x  center x
!> @param y  center y
!> @param edge edge length
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_square(x,y,edge)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_square

IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x,y	!< center coordinates
real(kind=sgl),INTENT(IN)  	:: edge	!< edge length

real(kind=sgl)  		:: ed	! auxiliary variable

 ed=0.5*edge
 write (psunit,"('0.0 setgray')")
 write (psunit,"('newpath')")
 write (psunit,"(2(F12.7,' '),'moveto')") x-ed,y-ed
 write (psunit,"(2(F12.7,' '),'lineto')") x-ed,y+ed
 write (psunit,"(2(F12.7,' '),'lineto')") x+ed,y+ed
 write (psunit,"(2(F12.7,' '),'lineto closepath S')") x+ed,y-ed

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_filledsquare
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a filledsquare
!
!> @param x  center x
!> @param y  center y
!> @param edge edge length
!> @param graylevel gray level for filling
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_filledsquare(x,y,edge,graylevel)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_filledsquare
       
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x,y		!< center coordinates
real(kind=sgl),INTENT(IN)  	:: edge		!< edge length
real(kind=sgl),INTENT(IN)  	:: graylevel	!< gray level for filling

real(kind=sgl)  		:: ed		!< auxiliary variable

 ed=0.5*edge
 write (psunit,"(F12.7,' setgray')") graylevel
 call PS_newpath
 call PS_move(x-ed,y-ed)
 call PS_draw(x-ed,y+ed)
 call PS_draw(x+ed,y+ed)
 write (psunit,"(2(F12.7,' '),'lineto closepath fill S')") x+ed,y-ed

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_cross
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a small cross
!
!> @param x  center x
!> @param y  center y
!> @param edge edge length
!> @param lw line width
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_cross(x,y,edge,lw)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_cross
        
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x,y		!< center coordinates
real(kind=sgl),INTENT(IN)  	:: edge		!< edge length
real(kind=sgl),INTENT(IN)  	:: lw		!< line width

real(kind=sgl)  		:: ed		!< auxiliary variable

 ed=0.5*edge
 call PS_setlinewidth(lw)
 call PS_newpath
 call PS_move(x-ed,y-ed)
 call PS_draw(x+ed,y+ed)
 call PS_move(x-ed,y+ed)
 call PS_draw(x+ed,y-ed)
 call PS_stroke

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_sphere
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a colored sphere
!
!> @details method modified from Earl J. Kirkland''s book, page 226, adapted for 
!> color PostScript
!>
!> @param x  center x
!> @param y  center y
!> @param r radius
!> @param clr color string (red, grn, blu, bro, ylw, pnk, and cyn)
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   12/21/18 MDG 4.0 new color model
!--------------------------------------------------------------------------
recursive subroutine PS_sphere(x,y,r,clr)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_sphere

use constants

IMPLICIT NONE

real(kind=sgl),INTENT(IN)     :: x,y    !< center coordinates
real(kind=sgl),INTENT(IN)     :: r      !< radius
integer(kind=irg),INTENT(IN)  :: clr    !< atomic number

write (psunit,"(1x,7(f12.5,1x),'sp')") ATOM_colors(1:3,clr),r,r,x,y

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_arc
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw an arc of a circle (see PostScript 'arc' command for details)
!
!> @param x0  new coordinate origin x
!> @param y0  new coordinate origin y
!> @param x  center x
!> @param y  center y
!> @param radius radius
!> @param ang1 starting angle (degrees)
!> @param ang2 end angle (degrees)
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_arc(x0,y0,x,y,radius,ang1,ang2)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_arc

IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x0,y0		!< new origin coordinates
real(kind=sgl),INTENT(IN)  	:: x,y			!< center coordinates
real(kind=sgl),INTENT(IN)  	:: radius		!< radius
real(kind=sgl),INTENT(IN)  	:: ang1,ang2	!< start and end angles


 write (psunit,"('N ',2(F16.10,' '),' moveto ',5(E16.8,' '),' arc S')") x0,y0,x,y,radius,ang1,ang2

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_circle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a circle
!
!> @param x  center x
!> @param y  center y
!> @param radius radius
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_circle(x,y,radius)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_circle

IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x,y			!< center coordinates
real(kind=sgl),INTENT(IN)  	:: radius		!< radius

 write (psunit,"('N ',3(F16.10,' '),'0 360 arc Cl S')") x,y,radius

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_filledcircle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a filled circle
!
!> @param x  center x
!> @param y  center y
!> @param radius radius
!> @param graylevel gray level value to use for fill
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_filledcircle(x,y,radius,graylevel)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_filledcircle
        
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x,y		!< center coordinates
real(kind=sgl),INTENT(IN)  	:: radius	!< radius
real(kind=sgl),INTENT(IN)  	:: graylevel	!< gray level

 write (psunit,"(F12.7,' setgray')") graylevel
 write (psunit,"('N ',3(F12.7,' '),'0 360 arc Cl F')") x,y,radius

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_drawframe
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw the main frame
!
!> @param x  frame width
!> @param y  frame height
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_drawframe(x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_drawframe
        
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x		!< frame width
real(kind=sgl),INTENT(IN)  	:: y 		!< frame height

call PS_drawrect(0.0,0.0,x,y)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_drawrect
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a rectangle
!
!> @param x1  lower left x
!> @param y1  lower left y
!> @param x2  upper right x
!> @param y2  upper right y
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_drawrect(x1,y1,x2,y2)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_drawrect
        
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x1, y1		!< lower left
real(kind=sgl),INTENT(IN)  	:: x2, y2 		!< upper right

 write (psunit,"('N')") 
 call PS_move(x1,y1)
 call PS_draw(x1,y2)
 call PS_draw(x2,y2)
 call PS_draw(x2,y1)
 call PS_draw(x1,y1)
 call PS_closepathS
 write (psunit,"('[0.15 0.03 0.02 0.03] 0 setdash')") 
 write (psunit,"('[] 0 setdash')") 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_line
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a line between two points
!
!> @param x1  first point x
!> @param y1  first point y
!> @param x2  second point x
!> @param y2  second point y
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_line(x1,y1,x2,y2)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_line
        
IMPLICIT NONE

real(kind=sgl),INTENT(IN)  	:: x1, y1		!< first point
real(kind=sgl),INTENT(IN)  	:: x2, y2 		!< second point

  call PS_move(x1,y1)
  call PS_draw(x2,y2)
  write (psunit,"('S')")      

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_setdash
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  define a dash pattern
!
!< @details Note that the dash pattern must be defined in the calling program.
!
!> @param PS Postscript structure
!> @param num number of components in dash pattern
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added PS as argument
!--------------------------------------------------------------------------
recursive subroutine PS_setdash(PS, num)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_setdash

IMPLICIT NONE

type(postscript_type), INTENT(INOUT)        :: PS
!f2py intent(in,out) ::  PS
integer(kind=irg),INTENT(IN)  	           :: num	!< dash pattern number of components/segments

integer(kind=irg)  		           :: i	!< loop counter

 write (psunit,"('[')")
 do i=1,num
  write (psunit,"(F12.7,' ')") PS%psdash(i)
 end do
 write (psunit,"('] ',I4,' setdash')") int(PS%psdash(num+1))

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_closepathS
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  close current path and Stroke
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_closepathS  
!DEC$ ATTRIBUTES DLLEXPORT :: PS_closepathS 

IMPLICIT NONE

write (psunit,"('Cl S')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_stroke
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  stroke the current path
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_stroke
!DEC$ ATTRIBUTES DLLEXPORT :: PS_stroke

IMPLICIT NONE

write (psunit,"('S ')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_gsave
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  save the current graphics settings
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_gsave
!DEC$ ATTRIBUTES DLLEXPORT :: PS_gsave

IMPLICIT NONE

write (psunit,"('gsave ')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_grestore
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  restore the previous graphics settings
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_grestore
!DEC$ ATTRIBUTES DLLEXPORT :: PS_grestore

IMPLICIT NONE

write (psunit,"('grestore ')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_closepath
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief close the current path
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_closepath   
!DEC$ ATTRIBUTES DLLEXPORT :: PS_closepath

IMPLICIT NONE

write (psunit,"('Cl ')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_newpath
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief start a new path
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_newpath     
!DEC$ ATTRIBUTES DLLEXPORT :: PS_newpath

IMPLICIT NONE

write (psunit,"('newpath ')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_text
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw text at a given location
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_text(x,y,line)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_text

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y	!< text start coordinates
character(*),INTENT(IN)	:: line	!< output string

 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"(') show')") 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_textv
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw text rotated counterclockwise by 90 degrees
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_textv(x,y,line)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_textv

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y	!< text start coordinates
character(*),INTENT(IN)	:: line	!< output string

 write (psunit,"('gsave ')") 
 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('90.0 rotate')") 
 write (psunit,"('( ',A,' ) show')") line
 write (psunit,"('-90.0 rotate grestore')") 
end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_texttitle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw the title
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!> @param q 
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_texttitle(x,y,line,q)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_texttitle

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y	!< text start coordinates
character(*),INTENT(IN)	:: line	!< output string
real(kind=sgl),INTENT(IN)	:: q	!< 

 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"('  [x',1PE8.0,'] ) show')") q

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_textvtitle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a vertical title
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!> @param q 
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_textvtitle(x,y,line,q)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_textvtitle

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y		!< text start coordinates
character(*),INTENT(IN)	:: line	        !< output string
real(kind=sgl),INTENT(IN)	:: q		!< 

 write (psunit,"('gsave ')") 
 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('90.0 rotate')") 
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"('  [x',1PE8.0,'] ) show')") q
 write (psunit,"('-90.0 rotate grestore')") 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_textint
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  text followed by an integer number
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!> @param vl integer output value
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_textint(x,y,line,vl)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_textint

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y		!< text start coordinates
character(*),INTENT(IN)	:: line	        !< output string
integer(kind=irg),INTENT(IN)	:: vl		!< integer output value

 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"(I4)",advance="no") vl
 write (psunit,"(') show')") 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_textvar
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  text followed by a real number
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!> @param vl real output value
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_textvar(x,y,line,vl)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_textvar

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y		!< text start coordinates
character(*),INTENT(IN)	:: line	        !< output string
real(kind=sgl),INTENT(IN)	:: vl		!< real output value

 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"(F14.4)",advance="no") vl
 write (psunit,"(') show')") 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_textvar8
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  text followed by a double precision real number
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!> @param vl double output value
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_textvar8(x,y,line,vl)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_textvar8

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y		!< text start coordinates
character(*),INTENT(IN)	:: line	        !< output string
real(kind=dbl),INTENT(IN)	:: vl		!< double output value

 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"(F12.6)",advance="no") vl
 write (psunit,"(') show')") 

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_textballoon
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  text inside a rounded balloon
!
!> @param x x-coordinate of text
!> @param y y-coordinate
!> @param line string with output text
!> @param font font string
!> @param sc scalefactor for the balloon
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_textballoon(x,y,line,font,sc)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_textballoon

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y	!< text start coordinates
character(*),INTENT(IN)	:: line	!< output string
character(*),INTENT(IN)	:: font	!< font string
real(kind=sgl),INTENT(IN)	:: sc	!< scale factor

 call PS_setfont(font,sc)
 write (psunit,"('/length (')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"(') stringwidth pop def')") 
 write (psunit,"('/height ',F6.4,' def /border ',F6.4,' def')") 0.11*sc/0.2,0.06*sc/0.2
 write (psunit,"('/bottom ',F12.7,' border sub def')") y
 write (psunit,"('/top ',F12.7,' height add border add def')") y
 write (psunit,"('/left ',F12.7,' border sub def')") x
 write (psunit,"('/right ',F12.7,' length add border add def')") x
 write (psunit,"('/rad 0.04 def frame')") 
 write (psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (psunit,"('(')",advance="no") 
 write (psunit,"(A)",advance="no") line
 write (psunit,"(') show')") 
 
end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_balloon
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw an empty balloon
!
!> @param x x-coordinate of lower left point
!> @param y y-coordinate
!> @param le length
!> @param he height
!> @param w width parameter
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_balloon(x,y,le,he,w)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_balloon

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: x,y		!< start coordinates
real(kind=sgl),INTENT(IN)	:: le, he	!< length and height
real(kind=sgl),INTENT(IN)	:: w		!< width parameter

 write (psunit,"('/he ',F6.4,' def /bo ',F6.4,' def /wi ',F6.4,' def')") he,0.5*w,le
 write (psunit,"('/bottom ',F12.7,' bo add def')") y
 write (psunit,"('/top bottom he add bo sub bo sub def')")  
 write (psunit,"('/left ',F12.7,' bo add def')") x
 write (psunit,"('/right left wi add bo sub def')")  
 write (psunit,"('/rad 0.04 def frame')") 

end subroutine 

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_setfont
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief select a font and make it active
!
!> @param line font string
!> @param sc font scale factor
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_setfont(line,sc)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_setfont

IMPLICIT NONE

real(kind=sgl),INTENT(IN)	:: sc	!< font scale factor  
character(*),INTENT(IN)	:: line	!< font string

 write (psunit,"()",advance="no") 
 write (psunit,"('/',A)",advance="no") line
 write (psunit,"(' findfont')") 
 write (psunit,"(F6.4,' scalefont ')") sc
 write (psunit,"('setfont')")

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: Printhkl
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  print hkl indices in PostScript format
!
!> @param x x position of text
!> @param y y position of text
!> @param h  h-index
!> @param k  k-index
!> @param l  l-index
!
!> @todo This is a really dumb implementation that needs to be completely 
!> reworked to accomodate arbitrarily large integers. 
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine Printhkl(x,y,h,k,l)
!DEC$ ATTRIBUTES DLLEXPORT :: Printhkl

IMPLICIT NONE

integer(kind=irg),INTENT(IN)      	:: h,k,l		!< Miller index triplet
real(kind=sgl),INTENT(IN)		:: x,y			!< starting position of indices
character(1),parameter 		:: numbers(0:9) = (/'0','1','2','3','4','5','6','7','8','9'/)
real(kind=sgl)         		:: xo,yo,dx,dy,x1,y1
character(1)           		:: line

 call PS_setfont(PSfonts(5),0.065)
 call PS_setlinewidth(0.004)
 xo = 0.050
 yo = 0.050
 dx = 0.050
 dy = 0.065
! THIS ONLY WORKS FOR INDICES -9 <= ... <= 9  !!!
! first index
 x1=x+xo
 y1=y+yo
 line=numbers(abs(h))
 call PS_text(x1,y1,line)
 if (h.lt.0) then
  call PS_line(x1,y1+dy,x1+0.5*dx,y1+dy)
 end if
! second index
 x1=x1+dx
 line=numbers(abs(k))
 call PS_text(x1,y1,line)
 if (k.lt.0) then
  call PS_line(x1,y1+dy,x1+0.5*dx,y1+dy)
 end if
! third index
 x1=x1+dx
 line=numbers(abs(l))
 call PS_text(x1,y1,line)
 if (l.lt.0) then
  call PS_line(x1,y1+dy,x1+0.5*dx,y1+dy)
 end if

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: DumpIndices
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  version of Printhkl used by stereographic projection program
!
!> @param PS Postscript structure
!> @param S  space character 'd' or 'r'
!> @param h  h-index
!> @param k  k-index
!> @param l  l-index
!> @param c positioning parameter
!> @param x x position of text
!> @param y y position of text
!> @param n logical
!
!> @todo This is a really dumb implementation that needs to be completely 
!> reworked to accomodate arbitrarily large integers. 
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added PS as argument
!--------------------------------------------------------------------------
recursive subroutine DumpIndices(PS,hexset,S,h,k,l,c,x,y,n)
!DEC$ ATTRIBUTES DLLEXPORT :: DumpIndices

use crystal

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)      :: PS
!f2py intent(in,out) ::  PS
logical,INTENT(IN)                    :: hexset
character(1),INTENT(IN)           	:: S			!< space character 'd' or 'r'
integer(kind=irg),INTENT(IN)      	:: h,k,l		!< Miller index triplet
integer(kind=irg),INTENT(IN)		:: c			!< positioning parameter
real(kind=sgl),INTENT(IN)		:: x,y			!< starting position of indices
logical,INTENT(IN)		        :: n			!< logical

character(1),parameter :: numbers(0:9) = (/'0','1','2','3','4','5','6','7','8','9'/)
character(1)           :: line
integer(kind=irg)      :: uvw(3),uvtw(4)
real(kind=sgl)         :: xo,yo,dx,dy,x1,y1

 call PS_setfont(PSfonts(5),0.08/PS%psscale)
 xo = 0.050/PS%psscale
 yo = 0.050/PS%psscale
 dx = 0.050/PS%psscale
 dy = 0.075/PS%psscale

 if (n.eqv..FALSE.) then
  xo = -0.30/PS%psscale
  yo = -0.10/PS%psscale
 endif

 if (c.eq.2) then
  xo = -0.30/PS%psscale
  yo = 0.05/PS%psscale
 end if

 uvw =(/ h,k,l /)

 if ((S.eq.'d').AND.(hexset.eqv..TRUE.)) then
  call MilBrav(uvw,uvtw,'34')
  call IndexReduceMB(uvtw)
 end if

! opening bracket
 if (S.eq.'d') then
  call PS_text(x+xo,y+yo,'[')
 else
  call PS_text(x+xo,y+yo,'\(')
 end if

!first index
 x1=x+xo+dx
 y1=y+yo
 if ((S.eq.'d').AND.(hexset.eqv..TRUE.)) then
  line=numbers(abs(uvtw(1)))
 else
  line=numbers(abs(uvw(1)))
 end if
 call PS_text(x1,y1,line)
 if (h.lt.0) then
  call PS_line(x1,y1+dy,x1+0.8*dx,y1+dy)
 end if

! second index
 x1=x1+dx
 if ((S.eq.'d').AND.(hexset.eqv..TRUE.)) then
  line=numbers(abs(uvtw(2)))
 else
  line=numbers(abs(uvw(2)))
 end if
 call PS_text(x1,y1,line)
 if (k.lt.0) then
  call PS_line(x1,y1+dy,x1+0.8*dx,y1+dy)
 end if

! third index (if hexset = .TRUE.) -> put a period
 if (hexset.eqv..TRUE.) then
  x1=x1+dx
  line='.'
  call PS_text(x1,y1,line)
 end if

! last index
 x1=x1+dx
 if ((S.eq.'d').AND.(hexset.eqv..TRUE.)) then
  line=numbers(abs(uvtw(4)))
 else
  line=numbers(abs(uvw(3)))
 end if
 call PS_text(x1,y1,line)
 if (l.lt.0) then
  call PS_line(x1,y1+dy,x1+0.8*dx,y1+dy)
 end if

! closing bracket
 x1=x1+dx
 if (S.eq.'d') then
  call PS_text(x1,y+yo,']')
 else
  call PS_text(x1,y+yo,'\)')
 end if

 dx=dx*0.3
 if (c.eq.2) then
  call PS_line(x+xo,y1-0.02/PS%psscale,x1+dx,y1-0.02/PS%psscale)
 end if

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PrintIndices
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   draw indices in PostScript format
!
!> @param S  space character 'd' or 'r'
!> @param hexset hexagonal setting logical
!> @param h  h-index
!> @param k  k-index
!> @param l  l-index
!> @param x x position of text
!> @param y y position of text
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/09/14 MDG 4.0 added argument hexset
!--------------------------------------------------------------------------
recursive subroutine PrintIndices(S,hexset,h,k,l,x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: PrintIndices

IMPLICIT NONE

character(1),INTENT(IN)           	:: S			!< space character 'd' or 'r'
logical,INTENT(IN)                    :: hexset
integer(kind=irg),INTENT(IN)      	:: h,k,l		!< Miller index triplet
real(kind=sgl),INTENT(IN)		:: x,y			!< starting position of indices
character(12)    			:: line
integer(kind=irg)			:: hkl(3)

 hkl = (/ h,k,l /)
 call IndexString(hexset,line,hkl,S)
 call PS_text(x,y,line)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_DumpImage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   draw integer image (512x512 maximum size) at given location with given scale
!
!> @details image is stored in global imnm variable
!
!> @param imaint image array
!> @param imanum image number
!> @param x0 x position of image
!> @param y0 y position of image
!> @param npx number of pixels along x
!> @param npy number of pixels along y
!> @param scl scale factor
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine PS_DumpImage(imaint,imanum,x0,y0,npx,npy,scl)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_DumpImage

IMPLICIT NONE

integer(kind=irg),INTENT(IN) 	        :: imaint(npx,npy)
integer(kind=irg),INTENT(INOUT)       :: imanum
!f2py intent(in,out) ::  imanum
real(kind=sgl),INTENT(IN)             :: x0,y0		!< image position
integer(kind=irg),INTENT(IN)         	:: npx,npy		!< image size
real(kind=sgl),INTENT(IN)             :: scl			!< image scale factor

 call PS_DumpImageDistort(imaint,imanum,x0,y0,npx,npy,scl,scl)
 
end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: PS_DumpImageDistort
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief   draw integer image (512x512 maximum size) at given location with given scale which may be different along x and y
!
!> @details image is stored in global imnm variable
!
!> @param imaint image array
!> @param imanum image number
!> @param x0 x position of image
!> @param y0 y position of image
!> @param npx number of pixels along x
!> @param npy number of pixels along y
!> @param sclx x-scale factor
!> @param scly y-scale factor
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/08/14 MDG 4.0 added imaint and imanum as arguments
!--------------------------------------------------------------------------
recursive subroutine PS_DumpImageDistort(imaint,imanum,x0,y0,npx,npy,sclx,scly)   
!DEC$ ATTRIBUTES DLLEXPORT :: PS_DumpImageDistort

IMPLICIT NONE

integer(kind=irg),INTENT(IN) 	        :: imaint(npx,npy)
integer(kind=irg),INTENT(INOUT)       :: imanum
!f2py intent(in,out) ::  imanum
real(kind=sgl),INTENT(IN)             :: x0,y0		!< image position
integer(kind=irg),INTENT(IN)         	:: npx,npy		!< image size
real(kind=sgl),INTENT(IN)             :: sclx,scly	!< image scale factors

integer(kind=irg)                 	:: iq,i,j,ir,iq1,iq2,k
integer(kind=irg),parameter       	:: bpp=8
character(2*npx)                  	:: bigone
character(3),parameter            	:: imnm(20) = (/'i01','i02','i03','i04','i05','i06', &
                                                  'i07','i08','i09','i10','i11','i12','i13','i14', &
                                                  'i15','i16','i17','i18','i19','i20'/)
character(1),parameter            	:: hd(0:15) = (/'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'/)

 imanum = imanum + 1

! integer image
 call PS_translate(x0,y0)
 write (psunit,"('/picstr ',i3,' string def')") npx
 write (psunit,"(' ')")
 write (psunit,"('/',3A)") imnm(imanum)
 write (psunit,"('{ ',i3,' ',i3,' ',i1,' [',i3,' 0 0 ',i3,' 0 0]')") npx,npy,bpp,npx,npy
 write (psunit,"(' { currentfile picstr readhexstring pop } image } def')")
 write (psunit,"(' gsave ',f7.4,' ',f7.4,' scale ')") sclx,scly*float(npy)/float(npx)
 write (psunit,"(3A)") imnm(imanum)
 
 do j=1,npy
  do i=1,npx
   ir=2*i-1
   iq=imaint(i,j)
   iq1=iq/16
   iq2=mod(iq,16)
   bigone(ir:ir)=hd(iq1)
   ir=ir+1
   bigone(ir:ir)=hd(iq2)
  end do
  k=2*npx
  write (psunit,"(A)") bigone
 end do

 write (psunit,"('grestore')")
 call PS_translate(-x0,-y0)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: IndexReduce
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Reduce an index triplet to smallest integers
!
!> @param hkl  index triplet
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine IndexReduce(hkl)
!DEC$ ATTRIBUTES DLLEXPORT :: IndexReduce

IMPLICIT NONE

integer(kind=irg),INTENT(INOUT)  		:: hkl(3)		!< indices
!f2py intent(in,out) ::  hkl
integer(kind=irg)  				:: mi,i,j
real(kind=sgl)     				:: rhkl(3),ir

 mi=100
 do i=1,3
  if ((abs(hkl(i)).lt.mi).and.(hkl(i).ne.0)) mi=abs(hkl(i))
 end do
 
! then check if this index is a common divider of the others
 j = 0
 do i=1,3
  rhkl(i) = float(hkl(i))/float(mi)
  ir = abs(rhkl(i))-float(int(abs(rhkl(i))))
  if (ir.eq.0.0) j=j+1
 end do
 
 if (j.eq.3) mi=1
 hkl = int(rhkl*mi)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: IndexReduceMB
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Reduce a Miller-Bravais index quartet to smallest integers
!
!> @param hkl  index quartet
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine IndexReduceMB(hkl)
!DEC$ ATTRIBUTES DLLEXPORT :: IndexReduceMB

IMPLICIT NONE


integer(kind=irg),INTENT(INOUT)  		:: hkl(4)		!< indices
!f2py intent(in,out) ::  hkl
integer(kind=irg)  				:: mi,i,j
real(kind=sgl)     				:: rhkl(4),ir

 mi=100
 do i=1,4
  if ((abs(hkl(i)).lt.mi).and.(hkl(i).ne.0)) mi=abs(hkl(i))
 end do

! then check if this index is a common divider of the others
 j = 0
 do i=1,4
  rhkl(i) = float(hkl(i))/float(mi)
  ir = abs(rhkl(i))-float(int(abs(rhkl(i))))
  if (ir.eq.0.0) j=j+1
 end do

 if (j.eq.4) mi=1
 hkl = int(rhkl*mi)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: IndexString
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Return a string of indices for printing (only deals with indices up to 9)
!
!> @param st  output string
!> @param hkl index triplet
!> @param sp space character
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!< @date   06/09/14 MDG 4.0 added argument hexset
!--------------------------------------------------------------------------
recursive subroutine IndexString(hexset,st,hkl,sp)
!DEC$ ATTRIBUTES DLLEXPORT :: IndexString

use crystal 

IMPLICIT NONE

logical,INTENT(IN)                            :: hexset
character(12),INTENT(OUT)            		:: st		!< output string
integer(kind=irg),INTENT(INOUT)        	:: hkl(3)	!< index triplet
!f2py intent(in,out) ::  hkl
character(1),INTENT(IN)             		:: sp		!< space character 'd' or 'r'

integer(kind=irg)        			:: l,hkil(4),i
character(1),parameter                        :: numbers(0:9) = (/'0','1','2','3','4','5','6','7','8','9'/)

 do l=1,12
  st(l:l) = ' '
 end do
 l=1
 if (sp.eq.'d') then
  st(l:l)='['
  l=l+1
 else
  st(l:l)='\'
  l=l+1
  st(l:l)='('
  l=l+1
 end if

 if (hexset.eqv..FALSE.) then 
  do i=1,3
   if (hkl(i).lt.0) then
    st(l:l)='-'
   else
    st(l:l)=' '
   end if
   l=l+1
   st(l:l)=numbers(abs(hkl(i)))
   l=l+1
  end do
 else
  if (sp.eq.'d') then 
    call MilBrav(hkl,hkil,'34')
    call IndexReduceMB(hkil)
  else
    hkil(1:2) = hkl(1:2)
    hkil(3) = -(hkl(1)+hkl(2))
    hkil(4) = hkl(3)
  end if
  do i=1,4
   if ((hkl(i).lt.0).and.(i.ne.3)) then
    st(l:l)='-'
   else
    st(l:l)=' '
   end if
   l=l+1
   if (i.eq.3) then 
    st(l:l)='.'
   else
    st(l:l)=numbers(abs(hkil(i)))
   end if
   l=l+1
  end do
 end if
 if (sp.eq.'d') then
  st(l:l)=']'
  l=l+1
  st(l:l)=' '
  l=l+1
  st(l:l)=' '
 else
  st(l:l)='\'
  l=l+1
  st(l:l)=')'
 end if

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: DrawSPFrame
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  draw a stereographic projection layout
!
!> @param PS Postscript structure
!> @param cell unit cell structure
!> @param CX center x-coordinate
!> @param CY center y-coordinate
!> @param CRad circle radius
!> @param iview index triplet for zone axis
!> @param sp space character
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/09/14 MDG 4.0 added arguments PS, cell
!--------------------------------------------------------------------------
recursive subroutine DrawSPFrame(PS,cell,CX,CY,CRad,iview,sp)
!DEC$ ATTRIBUTES DLLEXPORT :: DrawSPFrame

IMPLICIT NONE

type(postscript_type),INTENT(INOUT)           :: PS
!f2py intent(in,out) ::  PS
type(unitcell)        	                       :: cell
real(kind=sgl),INTENT(IN)    			:: CX, CY		!< circle center coordinates 
real(kind=sgl),INTENT(IN)    			:: CRad 		!< circle radius
integer(kind=irg),INTENT(INOUT)		:: iview(3)		!< zone axis indices
!f2py intent(in,out) ::  iview
character(1),INTENT(IN)	      		:: sp			!< drawing space

character(12)     :: instr
character(17)     :: str

 call PS_newpage(PS,.FALSE.,'Stereographic Projection')

 call PS_setlinewidth(0.012)
 call PS_circle(CX,CY,CRad)
 call PS_setlinewidth(0.008)
 call PS_line(CX-CRad,CY,CX+CRad,CY)
 call PS_line(CX,CY-CRad,CX,CY+CRad)
 call PS_text(CX-CRad-0.07,CY-0.025,'A')
 call PS_text(CX+CRad+0.03,CY-0.025,'B')
 call PS_text(CX-0.03,CY-CRad-0.08,'M''')
 call PS_text(CX-0.03,CY+CRad+0.05,'M"')
 call PS_cellinfo(PS,cell,0.00,8.30)
 call IndexString(cell%hexset,instr,iview,'d')
 call PS_text(CX,8.14,'Viewing Direction '//instr)

 if (sp.eq.'d') then 
  str='direct space'
 else
  str='reciprocal space'
 end if

 call PS_text(CX,8.00,'Projection of '//str)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetIndex
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  get the u,v,w or h,k,l indices
!
!> @param hexset hexagonal setting logical
!> @param ind index triplet
!> @param sp space character
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/25/13 MDG 3.0 updated IO
!> @date   06/09/14 MDG 4.0 added hexset argument
!--------------------------------------------------------------------------
recursive subroutine GetIndex(hexset,ind,sp)
!DEC$ ATTRIBUTES DLLEXPORT :: GetIndex

use crystal
use io

IMPLICIT NONE

logical,INTENT(IN)                    :: hexset
integer(kind=irg),INTENT(OUT)         :: ind(3)		!< indices
character(1),INTENT(IN)               :: sp			!< space 'd' or 'r'

integer(kind=irg) 		        :: jnd(4)

 if (sp.eq.'d') then 
  if (hexset.eqv..FALSE.) then
   call ReadValue('Enter u,v,w :', ind,3)
  else
   call ReadValue('Enter u,v,t,w :',jnd,4)
   call MilBrav(ind,jnd,'43')
   call IndexReduce(ind)
  end if
 else
  call ReadValue('Enter h,k,l :', ind,3)
 endif

end subroutine

end module postscript
