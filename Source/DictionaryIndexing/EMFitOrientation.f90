! ###################################################################
! Copyright (c) 2015-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMFitOrientationPS.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMFitOrientationPS
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief Refine the orientation in a dot product file by searching orientation
!> space around the best indexed point and optionally include pseudosymmetric variants
!
!> @date 08/01/16  SS 1.0 original
!> @date 03/12/18 MDG 1.1 replaced HDF5 dot product file reading by subroutine call
!> @date 04/01/18 MDG 2.0 merged various versions of the orientation fit/refine into a single program
!> @date 11/19/18 MDG 2.1 correction of bug caused by incorrectly initialized CIlist array
!--------------------------------------------------------------------------
program EMFitOrientation

use local
use typedefs 
use NameListTypedefs
use NameListHandlers
use files
use EBSDDImod

IMPLICIT NONE 

character(fnlen)              :: nmldeffile, progname, progdesc 
type(RefineOrientationtype)   :: ronl

nmldeffile = 'EMFitOrientation.nml'
progname = 'EMFitOrientation.f90'
progdesc = 'Refine orientations by searching orientation space about a point including the pseudosymmetric variant(s)'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 91 /), progname)

! deal with the namelist stuff
call GetRefineOrientationNameList(nmldeffile,ronl)

! and start the refinement
call EMEBSDrefinement(progname, ronl, nmldeffile)

end program EMFitOrientation
