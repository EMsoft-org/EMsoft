! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:EMzap.f90
!--------------------------------------------------------------------------
!
! PROGRAM:EMzap 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief PostScript output of kinematical zone axis diffraction patterns
!
! 
!> @date 12/11/98 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 04/13/14 MDG 3.0 removed all globals
!> @date 12/02/14 MDG 3.1 added camlen as argument to DiffPage
!--------------------------------------------------------------------------
program EMzap

use local
use typedefs
use crystal
use symmetry
use HDFsupport
use graphics
use files
use postscript
use io
use diffraction

IMPLICIT NONE

real(kind=sgl)                  :: io_real(1), camlen
character(fnlen)                :: progname, progdesc, gname
integer(kind=irg)               :: imanum
type(unitcell),pointer          :: cell
type(gnode)                     :: rlp
type(postscript_type)           :: PS
logical                         :: loadingfile

 progname = 'EMzap.f90'
 progdesc = 'Kinematical Zone Axis Diffraction Patterns'
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

! generate a set of zone axis patterns
 call DiffPage(PS, cell, rlp, camlen)

! close Postscript file
 call PS_closefile(PS)

end program EMzap
