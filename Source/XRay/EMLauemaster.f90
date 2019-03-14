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
! EMsoft:EMLauemaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMLauemaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Basic program to compute a realistic Laue master pattern on the unit sphere
!
!> @date 03/14/19  MDG 1.0 original version
!--------------------------------------------------------------------------
program EMLauemaster

character(fnlen)                        :: nmldeffile, progname, progdesc
type(LaueMasterNameListType)            :: lmnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMLauemaster.nml'
progname = 'EMLauemaster.f90'
progdesc = 'realistic Laue master pattern computation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 250, 0 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
!  call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetEBSDMasterNameList(nmldeffile,emnl)
end if

! generate a realistic Laue master pattern
 call ComputeLaueMasterPattern(lmnl, progname, nmldeffile)

end program EMLauemaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an EBSD master pattern as a function of energy
!
!> @param lmnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 03/14/19  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeLaueMasterPattern(lmnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use initializersHDF
use initializers
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
 
IMPLICIT NONE

type(LaueMasterNameListType),INTENT(INOUT) :: lmnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

logical 								   :: verbose
type(HDFobjectStackType),pointer           :: HDF_head
type(unitcell),pointer                     :: cell
type(gnode),save                           :: rlp
type(Laue_g_list),pointer                  :: reflist, rltmp
real(kind=sgl),allocatable                 :: mLPNH(:,:), mLPSH(:,:), masterSPNH(:,:), masterSPSH(:,:)
integer(kind=irg)						               :: npx, npy, gcnt, ierr, nix, niy, nixp, niyp, i, j 
real(kind=sgl)							               :: inten, xy(2), xyz(3), dx, dy, dxm, dym, Radius
real(kind=dbl)                             :: VMFnorm

! basic explanation: this is a really simple and fast Laue master pattern; we compute all the plane normals 
! that fall inside the extended Ewald sphere volume.  For each we compute the kinematic intensity
! using the x-ray scattering factors.  Then we add a narrow Gaussian peak to the square Lambert projection
! (either Northern or Southern hemisphere) in the correct position, using spherical interpolation 
! (a von Mises-type distribution might be useful here ...).  Finally, standard output to an HDF5 file.

! lmnl components
! xtalname
! lambdamin
! lambdamax
! IntFraction
! kappaVMF


nullify(HDF_head)
nullify(cell)

call timestamp(datestring=dstr, timestring=tstrb)
call cpu_time(tstart)

!=============================================
!=============================================
! crystallography section; 
 allocate(cell)
 verbose = .TRUE.

! clear the cell variable (set everything to zero)
 call ResetCell(cell)

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = trim(lmnl%xtalname)
 call CrystalData(cell,verbose)

! generate all atom positions
 call CalcPositions(cell,'v')

!=============================================
!=============================================
! compute reflection list with kinematical intensities
 call Laue_Init_Reflist(cell, lmnl, reflist, gcnt, verbose)

!=============================================
!=============================================
! populate the master pattern in square Lambert projection
  npx = lmnl%npx
  npy = npx
  allocate(mLPNH(-npx:npx,-npy:npy),stat=istat)
  allocate(mLPSH(-npx:npx,-npy:npy),stat=istat)
  mLPNH = 0.0
  mLPSH = 0.0

  VMFnorm = lmnl%kappaVMF / (4.0 * sngl(cPi) * sinh(lmnl%kappaVMF))

  rltmp => reflist
  do i=1,gcnt
! locate the nearest Lambert pixel
    call InterpolateLambert(sngl(rltmp%xyz), float(npx), npx, npy, nix, niy, nixp, niyp, dx, dy, dxm, dym)
! intensity with polarization correction
    inten = sngl(rltmp%sfs * rltmp%polar)
! depending on the sign of xyz(3) we put this point in the Northern or Southern hemisphere, taking into account the
! special case of reflections along the equator which should appear in both hemisphere arrays.  The intensities are 
! computed on a small grid of w x w points on the Lambert projection, which are then interpolated from a Gaussian on
! the sphere. we use the von Mises-Fisher distribution with p=3, so that the prefactor equals kappa / ( 4 pi sinh(kappa) )
    call sampleVMF(sngl(rltmp%xyz), lmnl%kappaVMF, VMFnorm*dble(inten), npx, nix, niy, lmnl%w, mLPNH, mLPSH)
! and go to the next point
    rltmp => rltmp%next
  end do 


!=============================================
!=============================================
! convert to stereographic projection
  allocate(masterSPNH(-npx:npx,-npy:npy))
  allocate(masterSPSH(-npx:npx,-npy:npy))
  masterSPNH = 0.0
  masterSPSH = 0.0

! get stereographic projections (summed over the atomic positions)
  Radius = 1.0
  do i=-npx,npx 
    do j=-npx,npx 
      xy = (/ float(i), float(j) /) / float(npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      xyz = xyz/NORM2(xyz)
      if (ierr.ne.0) then 
        masterSPNH(i,j) = 0.0
        masterSPSH(i,j) = 0.0
      else
        masterSPNH(i,j) = InterpolateLambert(xyz, mLPNH, npx)
        masterSPSH(i,j) = InterpolateLambert(xyz, mLPSH, npx)
      end if
    end do
  end do

!=============================================
!=============================================
! save everything to HDF5 file










end subroutine ComputeLaueMasterPattern
