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
!> @brief Basic program to compute a realistic Laue master pattern on the unit sphere (square Lambert and stereographic)
!
!> @date 03/14/19  MDG 1.0 original version (tested on 3/14, appears to work correctly)
!--------------------------------------------------------------------------
program EMLauemaster

use local
use NameListTypedefs
use NameListHandlers
use files

character(fnlen)                        :: nmldeffile, progname, progdesc
type(LaueMasterNameListType)            :: lmnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMLauemaster.nml'
progname = 'EMLauemaster.f90'
progdesc = 'realistic Laue master pattern computation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 250 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
!  call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetLaueMasterNameList(nmldeffile,lmnl)
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
use NameListHandlers
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
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

type(LaueMasterNameListType),INTENT(INOUT) :: lmnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

logical 								                   :: verbose
type(HDFobjectStackType),pointer           :: HDF_head
type(unitcell)                             :: cell
type(gnode),save                           :: rlp
type(Laue_g_list),pointer                  :: reflist, rltmp
real(kind=sgl),allocatable                 :: mLPNH(:,:), mLPSH(:,:), masterSPNH(:,:), masterSPSH(:,:)
integer(kind=irg)						               :: npx, npy, gcnt, ierr, nix, niy, nixp, niyp, i, j, w, istat, TIFF_nx, TIFF_ny, &
                                              hdferr
real(kind=sgl)							               :: xy(2), xyz(3), dx, dy, dxm, dym, Radius, ma, tstart, tstop
real(kind=dbl)                             :: VMFscale, inten
character(fnlen)                           :: fname, TIFF_filename, attributename, groupname, datagroupname, dataset, &
                                              HDF_FileVersion, hdfname

! declare variables for use in object oriented image module
integer                                    :: iostat
character(len=128)                         :: iomsg
logical                                    :: isInteger
type(image_t)                              :: im
integer(int8)                              :: i8 (3,4)
integer(int8), allocatable                 :: TIFF_image(:,:)
character(11)                              :: dstr
character(15)                              :: tstrb
character(15)                              :: tstre

! basic explanation: this is a really simple and fast Laue master pattern; we compute all the plane normals 
! that fall inside the extended Ewald sphere volume.  For each we compute the kinematic intensity
! using the x-ray scattering factors.  Then we add a narrow Gaussian peak to the square Lambert projection
! (either Northern or Southern hemisphere) in the correct position, using spherical interpolation 
! (a von Mises-type distribution might be useful here ...).  Finally, standard output to an HDF5 file.

! lmnl components
! xtalname
! lambdamin
! lambdamax
! kappaVMF
! hdfname


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
 nullify(reflist)
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


! the von Mises-Fisher distribution is defined by 
!
!   vmf(x;mu,kappa) = ( kappa / (4 pi sinh(kappa) ) ) exp[ kappa mu.x ]
!
! and this is multiplied by the intensity;  since these can be really large numbers we 
! we will work with the large kappa expansion as well as logarithms; first of all, the 
! distribution becomes (for large kappa)
!
! vmf(x;mu,kappa) = ( kappa exp[ kappa (mu.x-1) ]/  (2 pi)    (for large kappa)
!
! multiplying this by the intensity I and taking the logarithm, we have
!
! log(vmf) = (-1 + mux) kappa + Log(Inten) - Log(Pi) + Log(kappa) - Log(2) 
! 
! we'll take the constant part of this and call it VMFscale

  VMFscale = log(lmnl%kappaVMF) - log(2.D0) - log(cPi)

! set the size of the patches in the square Lambert space that we need to evaluate the VMF distribution for
  w = lmnl%patchw  ! this could become a part of the input namelist

! go through the entire reflection list
  rltmp => reflist
  do i=1,gcnt
! locate the nearest Lambert pixel (we need to make sure that the cartesian vector has unit length)
    call NormVec(cell, rltmp%xyz, 'c') 
    call LambertgetInterpolation(sngl(rltmp%xyz), float(npx), npx, npy, nix, niy, nixp, niyp, dx, dy, dxm, dym)
! intensity with polarization correction
    inten = rltmp%sfs * rltmp%polar
! depending on the sign of xyz(3) we put this point in the Northern or Southern hemisphere, taking into account the
! special case of reflections along the equator which should appear in both hemisphere arrays.  The intensities are 
! computed on a small grid of w x w points on the Lambert projection, which are then interpolated from a "Gaussian" on
! the sphere. we use the von Mises-Fisher distribution with p=3
    call sampleVMF(sngl(rltmp%xyz), lmnl%kappaVMF, VMFscale, inten, npx, nix, niy, w, mLPNH, mLPSH)
! and go to the next point
    rltmp => rltmp%next
  end do 

! finally, make sure that the equator is copied into both arrays
  mLPSH(-npx,-npx:npx) = mLPNH(-npx,-npx:npx)
  mLPSH( npx,-npx:npx) = mLPNH( npx,-npx:npx)
  mLPSH(-npx:npx,-npx) = mLPNH(-npx:npx,-npx)
  mLPSH(-npx:npx, npx) = mLPNH(-npx:npx, npx)
! that completes the computation of the master pattern

!=============================================
!=============================================
! convert to stereographic projection
  allocate(masterSPNH(-npx:npx,-npy:npy))
  allocate(masterSPSH(-npx:npx,-npy:npy))
  masterSPNH = 0.0
  masterSPSH = 0.0

! get stereographic projections
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
! we save an image for the Northern hemisphere in stereographic projection

! output the master pattern as a tiff file 
fname = trim(EMsoft_getEMdatapathname())//trim(lmnl%tiffname)
fname = EMsoft_toNativePath(fname)
TIFF_filename = trim(fname)

! allocate memory for image
TIFF_nx = 2*npx+1
TIFF_ny = 2*npx+1
allocate(TIFF_image(TIFF_nx,TIFF_ny))

! fill the image with whatever data you have (between 0 and 255)
ma = maxval(masterSPNH)
write(*,*) 'maximum intensity = ', ma 

TIFF_image = int(255 * (masterSPNH/ma))

! set up the image_t structure
im = image_t(TIFF_image)
if(im%empty()) call Message("ComputeLaueMasterPattern","failed to convert array to image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message("failed to write image to file : "//iomsg)
else  
  call Message('image written to '//trim(TIFF_filename))
end if 
deallocate(TIFF_image)

!=============================================
!=============================================
! save everything to HDF5 file
  call timestamp(datestring=dstr, timestring=tstre)

  nullify(HDF_head)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Open a new file
  hdfname = trim(EMsoft_getEMdatapathname())//trim(lmnl%hdfname)
  hdfname = EMsoft_toNativePath(hdfname)
  hdferr =  HDF_createFile(hdfname, HDF_head)

! write the EMheader to the file
  datagroupname = 'Lauemaster'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_LauemasterNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteLaueMasterNameList(HDF_head, lmnl)

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

! create the Lauemaster group and add a HDF_FileVersion attribbute to it 
  hdferr = HDF_createGroup(datagroupname, HDF_head)
  HDF_FileVersion = '4.0'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! now start writing the ouput arrays...
dataset = SC_mLPNH
  hdferr = HDF_writeDatasetFloatArray2D(dataset, mLPNH, 2*npx+1, 2*npx+1, HDF_head)

dataset = SC_mLPSH
  hdferr = HDF_writeDatasetFloatArray2D(dataset, mLPSH, 2*npx+1, 2*npx+1, HDF_head)

dataset = SC_masterSPNH
  hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSPNH, 2*npx+1, 2*npx+1, HDF_head)

dataset = SC_masterSPSH
  hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSPSH, 2*npx+1, 2*npx+1, HDF_head)

! and close the file
  call HDF_pop(HDF_head,.TRUE.)



end subroutine ComputeLaueMasterPattern
