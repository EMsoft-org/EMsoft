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
! EMsoft:EMZAgeom.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMZAgeom 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMZAgeom computes zone axis geometry & symmetry
! 
!> @date 12/17/13 MDG 1.0 simple interface
!> @date 06/13/14 MDG 2.0 removed all globals
!> @date 11/28/15 MDG 2.1 minor modification
!--------------------------------------------------------------------------
program EMZAgeom 

use local
use typedefs
use error
use crystal
use HDFsupport
use symmetry
use files
use io

integer(kind=irg)       :: kk(3), ga(3), gb(3), io_int(6), j, i, dgn
character(fnlen)        :: progname, progdesc, gname
type(unitcell),pointer  :: cell
logical                 :: loadingfile

! display the standard program info
 progname = 'EMZAgeom.f90'
 progdesc = 'Zone axis geometry and symmetry'
 call EMsoft(progname, progdesc)

 allocate(cell)
  
! first get the crystal data and microscope voltage
 call ReadValue('Enter xtal file name : ', gname,"(A)")
 cell%fname = trim(gname)
 call CrystalData(cell)

! get the zone axis
 call ReadValue('Enter the zone axis indices : ',io_int,3)
 kk(1:3) = io_int(1:3)  

! determine the point group number and get the ZAP 2-D symmetry
 j=0
 do i=1,32
  if (SGPG(i).le.cell % SYM_SGnum) j=i
 end do

! use the new routine to get the whole pattern 2D symmetry group, since that
! is the one that determines the independent beam directions.
 dgn = GetPatternSymmetry(cell,kk,j,.TRUE.)
 
! determine and display the shortest reciprocal lattice vectors for this zone
 call ShortestG(cell,kk,ga,gb,j)
 io_int(1:3) = ga(1:3)
 io_int(4:6) = gb(1:3)
 call WriteValue(' Reciprocal lattice vectors : ', io_int, 6, "(' (',3I3,') and (',3I3,')',/)")

  call Message(' --> Note that the first of these vectors is by default the horizontal direction in ', frm = "(A)")
  call Message('     any diffraction pattern or image simulation. All reference frames are right-handed.', frm = "(A/)")

end program EMZAgeom

