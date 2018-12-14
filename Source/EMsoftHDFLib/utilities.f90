! ###################################################################
! Copyright (c) 2014,-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:utilities.f90
!--------------------------------------------------------------------------
!
! MODULE: utilities
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief several auxiliary routines that use HDF calls
!
!> @date 11/11/18 MDG 1.0 new version
!--------------------------------------------------------------------------

module utilities

use local
use stringconstants

IMPLICIT NONE

contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: getXtalData
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief get structure data from .xtal file
!
!> @param xtalname filename 
!> @param cellparams lattice parameters
!> @param SGnum space group number
!
!> @date 11/08/18 MDG 1.0 original
!> @date 11/10/18 NDG 1.1 added optional TSLsymmetry argument
!--------------------------------------------------------------------------
recursive subroutine getXtalData(xtalname, cellparams, SGnum, TSLsymmetry)
!DEC$ ATTRIBUTES DLLEXPORT :: getXtalData

use NameListTypedefs
use HDF5
use HDFsupport
use typedefs
use error
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)           :: xtalname
real(kind=dbl),INTENT(OUT)            :: cellparams(6)
integer(kind=irg),INTENT(OUT)         :: SGnum
character(2),INTENT(OUT),OPTIONAL     :: TSLsymmetry

character(fnlen)                      :: filename, grname, dataset
logical                               :: stat, readonly
integer(kind=irg)                     :: hdferr, pgnum, i
integer(HSIZE_T)                      :: dims(1)
real(kind=dbl),allocatable            :: cpm(:)

type(HDFobjectStackType),pointer      :: HDF_head_local

nullify(HDF_head_local)

filename = trim(EMsoft_getXtalpathname())//trim(xtalname)
filename = EMsoft_toNativePath(filename)

stat = .FALSE.

call h5fis_hdf5_f(filename, stat, hdferr)

if (stat) then

! open the xtal file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(filename, HDF_head_local, readonly)

! open the namelist group
  grname = 'CrystalData'
  hdferr = HDF_openGroup(grname, HDF_head_local)

! get the spacegroupnumber
dataset = SC_SpaceGroupNumber
  call HDF_readDatasetInteger(dataset, HDF_head_local, hdferr, SGnum)

! get the lattice parameters
dataset = SC_LatticeParameters
  call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head_local, hdferr, cpm) 

! and close the xtal file
  call HDF_pop(HDF_head_local,.TRUE.)
  nullify(HDF_head_local)
else
  call Message('getXtalData','Error reading xtal file '//trim(filename))
  call Message('Writing default lattice parameter set; .ctf/.ang file will need to be edited manually')
  cpm = (/ 0.4D0, 0.4D0, 0.4D0, 90.D0, 90.D0, 90.D0 /)
  SGnum = 225
end if

cellparams = cpm 

! optionally, we may need to mapping the regular point group onto the EDAX/TSL convention
!==========================
if (present(TSLsymmetry)) then
! convert the space group number into a point group number
      pgnum = 0
      do i=1,32
        if (SGPG(i).le.SGnum) pgnum = i
      end do
! and get the TSL symmetry string from the TSLsymtype array
      TSLsymmetry = TSLsymtype(pgnum)
end if

end subroutine getXtalData




end module utilities
