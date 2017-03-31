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
! EMsoft:EMstereo.f90
!--------------------------------------------------------------------------
!
! PROGRAM:EMstereo 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Standard stereographic projections
!
!> @todo fix a bug that causes a segmentation fault when different range parameters are
!> entered.  The program does produce a correct ps file, but does not end gracefully....
!
!> @date  10/13/98 MDG 1.0 original
!> @date  05/21/01 MDG 2.0 f90
!> @date  04/16/13 MDG 3.0 rewrite
!> @date  06/13/14 MDG 4.0 removed all globals
!--------------------------------------------------------------------------
program EMstereo

use local
use typedefs
use crystal
use HDFsupport
use graphics
use files
use postscript
use io

IMPLICIT NONE

character(1)                    :: sp
logical                         :: topbot
integer(kind=irg)               :: iview(3), io_int(3), imanum
character(fnlen)                :: progname, progdesc, gname
type(unitcell),pointer          :: cell
logical                         :: loadingfile
type(postscript_type)           :: PS

 progname = 'EMstereo.f90'
 progdesc = 'Stereographic projections (direct/ reciprocal space)'
 call EMsoft(progname, progdesc)
 
 allocate(cell)

 cell % SG % SYM_reduce=.TRUE.
 topbot=.FALSE.

! read crystal information
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = gname
 call CrystalData(cell)

! real space or reciprocal space
 call GetDrawingSpace(sp)

! viewing direction
 call GetViewingDirection(cell%hexset,iview)

! open PostScript file
 imanum = 1
 call PS_openfile(PS, progdesc, imanum)

! get index ranges
 call Message('  Enter the maximum index for h,k and l, or for ', frm = "(/A)")
 call Message('  u,v, and w. For a hexagonal system, please use', frm = "(A)")
 call Message('  4-index notation [uv.w] or (hk.l) to determine', frm = "(A)")
 call Message('  the largest index.', frm = "(A/)")
 call ReadValue(' Enter maximum indices (h,k,l) : ', io_int, 3)

! call the drawing routine
 call StereoProj(PS,cell,sp,iview,io_int(1),io_int(2),io_int(3),topbot)

! close Postscript file
 call PS_closefile(PS)

end program EMstereo

