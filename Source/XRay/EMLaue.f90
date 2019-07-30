! ###################################################################
! Copyright (c) 2019-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMLaue.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMLaue
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic program to compute a realistic transmission/reflection Laue pattern
!
!> @date 07/30/19  MDG 1.0 original version 
!--------------------------------------------------------------------------
program EMLaue

use local
use NameListTypedefs
use NameListHandlers
use files

character(fnlen)                        :: nmldeffile, progname, progdesc
type(LaueNameListType)                  :: lnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMLaue.nml'
progname = 'EMLaue.f90'
progdesc = 'Transmission/reflection Laue pattern computation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 251 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
!  call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetLaueNameList(nmldeffile,lnl)
end if

! generate a realistic Laue master pattern
 call ComputeLauePattern(lnl, progname, nmldeffile)

end program EMLaue

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeLauePattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a series of transmission/reflection Laue patterns
!
!> @param lnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 07/30/19  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeLauePattern(lnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use NameListHandlers
use initializersHDF
use initializers
use Lauemod
use xrdmod
use symmetry
use crystal
use constants
use error
use gvectors
use kvectors
use io
use local
use files
use timing
use Lambert
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use omp_lib
use notifications
use stringconstants
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

type(LaueNameListType),INTENT(INOUT)       :: lnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

integer(kind=irg)                          :: numangles
type(AngleType),pointer                    :: angles

integer(kind=irg)						   :: hdferr, npx, npy
real(kind=sgl) 							   :: kouter, kinner 
real(kind=sgl),allocatable 				   :: detector(:,:), patterns(:,:,:)

type(HDFobjectStackType),pointer           :: HDF_head

! Initialize FORTRAN interface.
! nullify(HDF_head)
! call h5open_EMsoft(hdferr)

! read the master pattern file 
! call readLaueMasterFile(lnl%MPfname, lmnl, hdferr, LaueMPdata, getmLPNH=.TRUE., getmLPSH=.TRUE.)

! read the list of orientations and convert them all to quaternions if they are not already
nullify(angles)
allocate(angles)
call Lauereadangles(lnl%orientationfile, numangles, angles, verbose=.TRUE.)

! compute the limiting wave numbers for the outer and inner Ewald spheres
kouter = getXRDwavenumber(lnl%maxVoltage)
kinner = getXRDwavenumber(lnl%minVoltage)

! read the crystal structure file 

! generate the detector array
npx = lnl%numpx
npy = lnl%numpy
allocate(detector(npx, npy))

! 


end subroutine ComputeLauePattern
