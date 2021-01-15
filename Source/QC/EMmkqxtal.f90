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
! EMsoft:EMmkqxtal.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMmkqxtal 
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief create a quasicrystal structure file (very simple program)
!
!> @date   05/22/18 SS 1.0 original
!--------------------------------------------------------------------------
program EMmkqxtal

use local
use typedefs
use HDFsupport
use io
use error
use symmetry
use files
use qcrystal
use InitializersQCHDF

IMPLICIT NONE
type(TDQCStructureType), pointer          :: TDQCcell
type(QCStructureType),pointer             :: QCcell
integer(kind=irg)                         :: qcdim, ii, jj
character(fnlen)                          :: progname, progdesc, fname

 progname = 'EMmkqxtal.f90'
 progdesc = 'Create an HDF quasi-crystal structure file and place it in the XtalFolder'

 call EMsoft(progname, progdesc)

 call GetQCType(qcdim)

 select case (qcdim)
  case(1)
    ! 2D QC
    allocate(TDQCcell)
    call GetQCLatParm(TDQCcell)
    call PrintSGTable(TDQCcell)
    call GetQCSpaceGroup(TDQCcell)
    call GetQCAsymPos(TDQCcell)
    call ReadValue('Enter output file name (*.qxtal) ', fname)
    TDQCcell%fname = fname
    call SaveQCDataHDF(TDQCcell)

  case(2)
    ! 3D QC
    allocate(QCcell)
    call GetQCLatParm(QCcell)
    call PrintSGTable(QCcell)
    call GetQCSpaceGroup(QCcell)
    call GetQCAsymPos(QCcell)
    call ReadValue('Enter output file name (*.qxtal) ', fname)
    QCcell%fname = fname
    call SaveQCDataHDF(QCcell)

  case DEFAULT
    ! anything else
    call FatalError('EMmkqxtal:','unknown quasicrystal dimensionality (only 2 and 3 implemented for now)')

 end select


end program EMmkqxtal
