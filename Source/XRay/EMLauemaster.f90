! ###################################################################
! Copyright (c) 2019-2022, Marc De Graef Research Group/Carnegie Mellon University
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
!  call JSONreadEBSDmasterNameList(lmnl, nmldeffile, error_cnt)
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
!> @date 01/27/20  MDG 1.1 adds .sht file output (basically copied from EBSD master pattern program)
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
use math
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
use DSHT 
use fft_wrap

IMPLICIT NONE

interface 
  recursive function writeShtFile (fn, iprm, fprm, doi, note, alm, aTy, aCd, vers, cprm) result(res) &
  bind(C, name ='writeShtFile_')

  use ISO_C_BINDING

  IMPLICIT NONE 

  character(c_char)             :: fn
  integer(c_int)                :: iprm(11) 
  real(c_float)                 :: fprm(25)
  character(c_char)             :: doi
  character(c_char)             :: note 
  real(C_DOUBLE_COMPLEX)        :: alm(2*(iprm(3)+3)*(iprm(3)+3))
  integer(c_int)                :: aTy(iprm(6))
  real(c_float)                 :: aCd(iprm(6),5)
  character(c_char)             :: vers 
  character(c_char)             :: cprm
  integer(c_int)                :: res
  end function writeShtFile
end interface 


type(LaueMasterNameListType),INTENT(INOUT) :: lmnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

logical 								                   :: verbose
type(HDFobjectStackType)                   :: HDF_head
type(unitcell)                             :: cell
type(gnode),save                           :: rlp
type(Laue_g_list),pointer                  :: reflist, rltmp
real(kind=sgl),allocatable                 :: mLPNH(:,:), mLPSH(:,:), masterSPNH(:,:), masterSPSH(:,:)
integer(kind=irg)						               :: npx, npy, gcnt, ierr, nix, niy, nixp, niyp, i, j, w, istat, TIFF_nx, TIFF_ny, &
                                              hdferr, bw, d, ll, res, timestart, timestop, info, Lindex
real(kind=sgl)							               :: xy(2), xyz(3), dx, dy, dxm, dym, Radius, mi, ma, tstart, tstop, sdev, mean, kl(2)
real(kind=dbl)                             :: VMFscale, inten, p, LegendreLattitude
character(fnlen)                           :: fname, TIFF_filename, attributename, groupname, datagroupname, dataset, &
                                              HDF_FileVersion, hdfname, doiString, layout, SHTfile

! declare variables for use in object oriented image module
integer                         :: iostat
character(len=128)              :: iomsg
logical                         :: isInteger, north
integer(int8), allocatable      :: TIFF_image(:,:)
character(11)                   :: dstr
character(15)                   :: tstrb
character(15)                   :: tstre
character(fnlen)                :: image_filename
type(image_t)                   :: im, im2
integer(int8)                   :: i8 (3,4), int8val
integer(int8), allocatable      :: output_image(:,:)

! parameters for the .sht output file 
integer(kind=irg),parameter     :: nipar=11, nfpar=25
character(fnlen)                :: EMversion, cprm, note, notestring
character(6)                    :: vstring
character(8)                    :: vstring2
character(fnlen)                :: revision
integer(c_int32_t)              :: sgN      ! space group number [1,230]
integer(c_int32_t)              :: sgS      ! space group setting [1,2]
integer(c_int32_t)              :: numAt    ! number of atoms
integer(c_int32_t),allocatable  :: aTy(:)   ! atom types (nAt atomic numbers)
real(c_float),allocatable       :: aCd(:,:) ! atom coordinates, (nAt * 5 floats {x, y, z, occupancy, Debye-Waller in nm^2})
real(c_float)                   :: lat(6)   ! lattice parameters {a, b, a, alpha, beta, gamma} (in nm / degree)
real(c_float)                   :: fprm(nfpar) ! floating point parameters (float32 EMsoftED parameters in order)
integer(c_int32_t)              :: iprm(nipar) ! integer parameters {# electrons, electron multiplier, numsx, npx, latgridtype}
real(kind=dbl),allocatable      :: finalmLPNH(:,:), finalmLPSH(:,:), weights(:)
real(kind=dbl),allocatable      :: LegendreArray(:), upd(:), diagonal(:)

type(DiscreteSHT)               :: transformer 
complex(kind=dbl), allocatable  :: almMaster(:,:)   ! spectra of master pattern to index against
complex(kind=dbl), allocatable  :: almPat   (:,:)   ! work space to hold spectra of exerimental pattern
real(kind=dbl),allocatable      :: alm(:)


! basic explanation: this is a really simple and fast Laue master pattern; we compute all the plane normals 
! that fall inside the extended Ewald sphere volume.  For each we compute the kinematic intensity
! using the x-ray scattering factors.  Then we add a narrow Gaussian peak to the square Lambert projection
! (either Northern or Southern hemisphere) in the correct position, using spherical interpolation 
! (a von Mises-type distribution might be useful here ...).  Finally, standard output to an HDF5 file, or 
! output to an .sht file.

! lmnl components
! xtalname
! lambdamin
! lambdamax
! kappaVMF
! hdfname

if (lmnl%outformat.eq.'SHT') then 
  npx = 193
  layout = 'legendre'
else 
  npx = lmnl%npx
end if 

nullify(HDF_head%next)
!nullify(cell)        

call timestamp(datestring=dstr, timestring=tstrb)
call cpu_time(tstart)

!=============================================
!=============================================
! crystallography section; 
 !allocate(cell)        
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
! npx = lmnl%npx
  npy = npx
  allocate(mLPNH(-npx:npx,-npy:npy),stat=istat)
  allocate(mLPSH(-npx:npx,-npy:npy),stat=istat)
  mLPNH = 0.0
  mLPSH = 0.0


!=============================================
!=============================================
! precompute the Legendre array for the new lattitudinal grid values
  call Message(' Computing Legendre lattitudinal grid values')
  allocate(diagonal(2*npx+1),upd(2*npx+1))
  diagonal = 0.D0
  upd = (/ (dble(i) / dsqrt(4.D0 * dble(i)**2 - 1.D0), i=1,2*npx+1) /)
  call dsterf(2*npx-1, diagonal, upd, info) 
! the eigenvalues are stored from smallest to largest and we need them in the opposite direction
  allocate(LegendreArray(0:2*npx))
  LegendreArray(0:2*npx) = diagonal(2*npx+1:1:-1)
! set the center eigenvalue to 0
  LegendreArray(npx) = 0.D0
  deallocate(diagonal, upd)

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
! log(vmf) = (-1 + mu.x) kappa + Log(Inten) - Log(Pi) + Log(kappa) - Log(2) 
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

! do we need to modify the direction cosines to coincide with the Legendre lattitudinal grid values?
    north = .TRUE.
    if (rltmp%xyz(3).lt.0) north=.FALSE.
    if (abs(rltmp%xyz(3)).ne.1.D0) then
      kl = LambertSpheretoSquare(rltmp%xyz, ierr) * float(npx)
! here we need to be very careful to determine the index of the Legendre ring, NOT the Lambert ring !!!
      Lindex = npx 
      do while(LegendreArray(Lindex).lt.rltmp%xyz(3)) 
        Lindex = Lindex - 1
      end do  
      LegendreLattitude = LegendreArray( Lindex - 1)
! the factor p rescales the x and y components of kstar to maintain a unit vector
      p = sqrt((1.D0-LegendreLattitude**2)/(1.D0-rltmp%xyz(3)**2))
      rltmp%xyz = (/ p*rltmp%xyz(1), p*rltmp%xyz(2), LegendreLattitude /)
! rescale the coordinates in the Legendre square to be on the correct ring
      kl = kl * float(Lindex)/maxval(abs(kl))
    end if
    if (.not.north) rltmp%xyz(3) = -rltmp%xyz(3)
! and continue with the projection
    ! call LambertgetInterpolation(sngl(rltmp%xyz), float(npx), npx, npy, nix, niy, nixp, niyp, dx, dy, dxm, dym)
! intensity with polarization correction
    inten = rltmp%sfs * rltmp%polar
    if (lmnl%binarize.eqv..TRUE.) inten = 1.0
! depending on the sign of xyz(3) we put this point in the Northern or Southern hemisphere, taking into account the
! special case of reflections along the equator which should appear in both hemisphere arrays.  The intensities are 
! computed on a small grid of w x w points on the Lambert projection, which are then interpolated from a "Gaussian" on
! the sphere. we use the von Mises-Fisher distribution with p=3
    call sampleVMF(sngl(rltmp%xyz), lmnl%kappaVMF, VMFscale, inten, npx, int(kl(1)), int(kl(2)), w, mLPNH, mLPSH, LegendreArray)
! and go to the next point
    rltmp => rltmp%next
  end do 

! finally, make sure that the equator is copied into both arrays
  mLPSH(-npx,-npx:npx) = mLPNH(-npx,-npx:npx)
  mLPSH( npx,-npx:npx) = mLPNH( npx,-npx:npx)
  mLPSH(-npx:npx,-npx) = mLPNH(-npx:npx,-npx)
  mLPSH(-npx:npx, npx) = mLPNH(-npx:npx, npx)
! that completes the computation of the master pattern

! do we need to rebinarize?
if (lmnl%binarize.eqv..TRUE.) then 
  where (mLPNH.gt.0.75) 
    mLPNH = 1.0
  end where 
  where (mLPSH.gt.0.75) 
    mLPSH = 1.0
  end where 
end if 

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
      xyz = xyz/vecnorm(xyz)
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
if (lmnl%outformat.eq.'LMP') then   ! regular master pattern HDF5 output
  nullify(HDF_head%next)
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
else  ! spherical harmonic transform of master pattern and storage in .sht file

! build energy weighted master pattern
  d = npx
  allocate(finalmLPNH(-d:d,-d:d))
  allocate(finalmLPSH(-d:d,-d:d))
  finalmLPNH = mLPNH
  finalmLPSH = mLPSH

write (*,*) 'shape final patterns ', shape(finalmLPNH)

!=====================================
! form the .sht file name from the formula, name, structuresymbol and voltage parameters
  SHTfile = trim(lmnl%SHT_folder)//'/'//trim(lmnl%SHT_formula)
! compound name (:brass, forsterite, alpha-quartz ... )
  if ((trim(lmnl%SHT_name).ne.'undefined').and.(trim(lmnl%SHT_name).ne.'')) then
    SHTfile = trim(SHTfile)//' ('//trim(lmnl%SHT_name)//')'
  end if
! structure symbol (StrukturBericht, Pearson, ...)
  if ((trim(lmnl%SHT_structuresymbol).ne.'undefined').and.(trim(lmnl%SHT_structuresymbol).ne.'')) then
    SHTfile = trim(SHTfile)//' ['//trim(lmnl%SHT_structuresymbol)//']'
  end if
! ! voltage string 
!   write(vstring,"(' {',I2.2,'kV')") int(mcnl%EkeV) 
!   SHTfile = trim(SHTfile)//trim(vstring)
! ! if the sample tilt is NOT equal to 70 deg, then add it in F4.1 format to the comment string 
!   if (mcnl%sig.ne.70.0) then 
!     write (vstring2,"(' ',F4.1,'deg')") sngl(mcnl%sig)
!     SHTfile = trim(SHTfile)//trim(vstring2)//'}.sht'
!   else
    ! SHTfile = trim(SHTfile)//'}.sht'
!   end if

    SHTfile = trim(SHTfile)//'.sht'

!=====================================
  call Message(' Saving Lambert squares to tiff file ',"(//A)")
! output these patterns to a tiff file
  image_filename = trim(SHTfile)
  ll = len(trim(image_filename))
  ll = ll-2
! replace the .sht extension by .tiff
  image_filename(ll:ll) = 't'
  image_filename(ll+1:ll+1) = 'i'
  image_filename(ll+2:ll+2) = 'f'
  image_filename(ll+3:ll+3) = 'f'
  image_filename = trim(EMsoft_getEMdatapathname())//trim(image_filename)
  image_filename = EMsoft_toNativePath(image_filename)

  allocate(output_image(2*(2*d+1)+2,2*d+1))
  output_image = -1

  mi = minval( (/ minval(finalmLPNH), minval(finalmLPSH) /) )
  ma = maxval( (/ maxval(finalmLPNH), maxval(finalmLPSH) /) )

  do i=1,2*d+1
    output_image(1:2*d+1,i) = int(255*(finalmLPNH(-d:d,i-1-d)-mi)/(ma-mi))
    output_image(2*d+1+2:2*(2*d+1)+2,i) = int(255*(finalmLPSH(-d:d,i-1-d)-mi)/(ma-mi))
  end do
  
  im2 = image_t(output_image)
  if(im2%empty()) call Message("ComputeLaueMasterPattern","failed to convert array to image")

! create the file
  call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
  if(0.ne.iostat) then
    call Message("   --> Failed to write image to file : "//iomsg)
  else  
    call Message('   --> Lambert projections written to '//trim(image_filename))
  end if 
  deallocate(output_image)

!=====================================
! normalize the master patterns by subtracting the mean and dividing by the standard deviation 
  call Message(' Normalizing master patterns ')
  mean = sum(finalmLPNH) / dble( (2*d+1)*(2*d+1) )
  sdev = sqrt( sum( (finalmLPNH - mean)**2 )/ dble((2*d+1)*(2*d+1) - 1) )
  finalmLPNH = (finalmLPNH - mean) / sdev 

  mean = sum(finalmLPSH) / dble( (2*d+1)*(2*d+1) )
  sdev = sqrt( sum( (finalmLPSH - mean)**2 )/ dble((2*d+1)*(2*d+1) - 1) )
  finalmLPSH = (finalmLPSH - mean) / sdev 

!=====================================
! build transformer
   call FFTWisdom%load()
   call Message(' Initializing the spherical harmonic transformer')
   layout = 'legendre'
   bw = 384
   d = bw / 2 + 1
   call transformer%init(d, bw, layout)

!=====================================
! compute spherical harmonic transform of master pattern
   call Message(' Computing spherical harmonic transform')
   allocate(almMaster(0:bw-1, 0:bw-1))
   allocate(almPat   (0:bw-1, 0:bw-1))
   call transformer%analyze(finalmLPNH, finalmLPSH, almMaster)
   call FFTWisdom%save()

  timestop = Time_tock(timestart)

!=====================================
! prepare all parameters for the writing of the final .sht file 
!
! first the integers [see EMsoftLib/sht_file.cpp for definitions]
  bw = 384
  iprm = (/ cell%SYM_SGnum, 1, bw, & 
            cell%SYM_SGnum, cell%SYM_SGset, cell%ATOM_ntype, &
            0, 0, 0, bw, 2 /)
  lat(1) = sngl(cell%a)
  lat(2) = sngl(cell%b)
  lat(3) = sngl(cell%c) 
  lat(4) = sngl(cell%alpha) 
  lat(5) = sngl(cell%beta) 
  lat(6) = sngl(cell%gamma)

! then the floats [see EMsoftLib/sht_file.cpp for definitions]
  fprm = (/ nan(), nan(), 0.0, nan(), lat(1),lat(2),lat(3),lat(4),lat(5),lat(6), &
            nan(), nan(), nan(),nan(), nan(), &
            nan(), nan(), nan(), nan(), infty(), &
            nan(), nan(), nan(), nan(), nan() /)

! doi string 
  doiString = ''
! the 'addtoKiltHub' option should only be used for the original 120 structures in the KiltHub data base
  if (trim(lmnl%addtoKiltHub).eq.'Yes') then 
    doiString = SC_EMSHTDOI
  else
    if (trim(lmnl%useDOI).ne.'undefined') then ! use the user-defined DOI string
      doiString = trim(lmnl%useDOI)
    else
! if we get here, then we use the generic DOI for the Zenodo link to the GitHub .sht data base repository
      doiString = SC_ZENODODOI
    end if 
  end if 

! atom types and coordinates
  numAt = cell%ATOM_ntype 
  allocate(aTy(numAt))
  aTy = cell%ATOM_type(1:numAt)
  allocate(aCd(5,numAt))
  aCd = transpose(cell%ATOM_pos(1:numAt,1:5))

! transfer the complex almMaster array to a flat array with alternating real and imaginary parts
  allocate(alm( 2 * bw * bw ))
  alm = transfer(almMaster,alm)

  revision = trim(EMsoft_getEMsoftRevision())
  notestring = ''

  cprm = cstringify(lmnl%SHT_formula)// &
         cstringify(lmnl%SHT_name)// &
         cstringify(lmnl%SHT_structuresymbol)// &
         cstringify(cell%source)// &
         trim(notestring)
  
! write an .sht file using EMsoft style EBSD data
  SHTfile = trim(EMsoft_getEMdatapathname())//trim(SHTfile)
  SHTfile = EMsoft_toNativePath(SHTfile)

  res = writeShtFile(cstringify(SHTfile), iprm, fprm, &
                     cstringify(doiString), cstringify(note), alm, &
                     aTy, aCd, cstringify(revision), cstringify(cprm))

  call Message(' Final data stored in binary file '//trim(SHTfile), frm = "(A/)")

end if 

end subroutine ComputeLaueMasterPattern
