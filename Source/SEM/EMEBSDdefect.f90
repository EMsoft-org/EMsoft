! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDdefect.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDdefect
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSD computes EBSD patterns for a volume containing a defect
!
!> @date  11/05/19  MDG 1.0 forked from EMEBSD.f90
! ###################################################################

program EMEBSDdefect

use local
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use io
use HDF5
use HDFsupport
use error
use detectors
use EBSDmod
use EBSDdefectHDFmod
use stringconstants

IMPLICIT NONE

character(fnlen)                       :: nmldeffile, progname, progdesc
type(EBSDdefectNameListType)           :: enl
type(MCCLNameListType)                 :: mcnl
type(MCCLNameListType)                 :: mcnl_ivol
type(EBSDMasterNameListType)           :: mpnl

type(EBSDAnglePCDefType)               :: orpcdef
type(EBSDMCdataType)                   :: EBSDMCdata
type(EBSDMCdataType)                   :: EBSDivoldata
type(EBSDMPdataType)                   :: EBSDMPdata
type(EBSDDetectorType)                 :: EBSDdetector

integer(kind=irg)                      :: res, error_cnt, hdferr, numangles, ipar(3), nx, ny, nz
integer(kind=irg)                      :: istat, sz(3), io_int(3)
real(kind=dbl)                         :: fpar(4)
real(kind=sgl)                         :: io_real(3)
logical                                :: verbose
character(fnlen)                       :: writetofile

nmldeffile = 'EMEBSDdefect.nml'
progname = 'EMEBSDdefect.f90'
progdesc = 'Dynamical EBSD patterns for a volume containing a defect'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 282 /), progname)

! deal with the namelist stuff, either .nml or .json format
call GetEBSDdefectNameList(nmldeffile,enl)

! this program needs a lot of data, and it also should be integrated 
! with EMsoftWorkBench, so we need to make sure that all data is loaded outside
! of the main computational routine, and passed in as pointers/arguments
! either by the fortran program or by EMsoftWorkBench calls.  

! 1. read the angle and deformation arrays from the deformation HDF file
call h5open_EMsoft(hdferr)
call EBSDreadorpcdefHDF(enl, ipar, fpar, orpcdef)

! 2. read the Monte Carlo data file (HDF format)
call readEBSDMonteCarloFile(enl%masterfile, mcnl, hdferr, EBSDMCdata)

! we also need an interaction volume file; if it is "undefined", then we print out the
! appropriate namelist file for the EMMCOpenCL program so that the user can generate 
! the necessary file.  [we could also just spawn the EMMCOpenCL run...]
if ((enl%sampleInteractionVolume.eqv..TRUE.).and.(trim(enl%ivolfile).eq.'undefined')) then 
  call Message('========================================================================================')
  call Message('   This program needs an interaction volume file (ivolfile parameter in name list file).')
  call Message('   We will generate the necessary name list file as ivol_input.nml.')
  call Message('   Please edit this file to set the --dataname-- variable, and adjust any other parameters')
  call Message('   then run the following command:     EMMCOpenCL ivol_input.nml')
  call Message('   to generate the interaction volume data file; then rerun the present program.')
  call Message('   Aborting program.')
  call Message('========================================================================================')

! generate the name list and fill in initial values
  mcnl_ivol = mcnl
  mcnl_ivol%mode = 'Ivol'
  mcnl_ivol%ivolx = 501
  mcnl_ivol%ivoly = 501
  mcnl_ivol%ivolz = 100
  mcnl_ivol%ivolstepx = 1.0
  mcnl_ivol%ivolstepy = 1.0
  mcnl_ivol%ivolstepz = 1.0
  mcnl_ivol%dataname = 'undefined'

  writetofile = 'undefined'
  nmldeffile = 'ivol_input.nml'
  call GetMCCLNameList(nmldeffile, mcnl_ivol, writetofile=writetofile)
  stop
end if

if (enl%sampleInteractionVolume.eqv..TRUE.) then
! if we get here, then the file presumable exists and we read the relevant content
  call readEBSDMonteCarloFile(enl%ivolfile, mcnl_ivol, hdferr, EBSDivoldata, getAccumxyz=.TRUE.)


! copy the necessary parameters from mcnl_ivol to mcnl, as well as the accum_xyz array, then
! delete the old copy from EBSDivoldata...
  mcnl%ivolx = mcnl_ivol%ivolx
  mcnl%ivoly = mcnl_ivol%ivoly
  mcnl%ivolz = mcnl_ivol%ivolz
  mcnl%ivolstepx = mcnl_ivol%ivolstepx
  mcnl%ivolstepy = mcnl_ivol%ivolstepy
  mcnl%ivolstepz = mcnl_ivol%ivolstepz
  ! io_int(1:3) = (/ mcnl%ivolx, mcnl%ivoly, mcnl%ivolz /)
  ! call WriteValue(' interaction volume sizes :',io_int,3)
  ! io_real(1:3) = (/ mcnl%ivolstepx, mcnl%ivolstepy, mcnl%ivolstepz /)
  ! call WriteValue(' interaction volume step sizes :',io_real,3)

  nx = (mcnl%ivolx-1)/2
  ny = (mcnl%ivoly-1)/2
  nz = mcnl%ivolz
  allocate(EBSDMCdata%accum_xyz(-nx:nx,-ny:ny,nz))
  EBSDMCdata%accum_xyz = EBSDivoldata%accum_xyz
  deallocate(EBSDivoldata%accum_xyz)
end if 

! 3. read EBSD master pattern file (HDF format)
call readEBSDMasterPatternFile(enl%masterfile, mpnl, hdferr, EBSDMPdata, defectMP=.TRUE., &
                               getmLPNH=.TRUE., getmLPSH=.TRUE.)
call h5close_EMsoft(hdferr)

call ComputedeformedEBSDpatterns(enl, mcnl, mpnl, ipar, fpar, orpcdef, EBSDMCdata, EBSDMPdata, progname, nmldeffile)
  
end program EMEBSDdefect


!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputedeformedEBSDPatterns
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a energy-weighted EBSD patterns with different pattern centers and deformation states
!
!> @param enl name list
!> @param orpcdef angle/orientation/deformation structure
!> @param acc energy accumulator arrays
!> @param master structure with master and detector arrays
!> @param progname program name string
!> @param nmldeffile name of nml file
!
!> @date 11/05/19  MDG 1.0 forked from EMEBSD.f90 program
!> @date 11/05/19  MDG 1.1 converted to depth integration instead of energy integration
!> @date 11/13/19  MDG 1.2 added more realistic interaction volume integration
!> @date 11/20/19  MDG 1.3 adds support for memory file system use (tmpfs on Linux)
!> @date 11/20/19  MDG 1.4 changed output format to gray-scale (byte) EBSPs 
!--------------------------------------------------------------------------
subroutine ComputedeformedEBSDPatterns(enl, mcnl, mpnl, ipar, fpar, orpcdef, EBSDMCdata, EBSDMPdata, & 
                                       progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use symmetry
use crystal
use constants
use io
use error 
use files
use filters
use diffraction
use detectors
use EBSDmod
use Lambert
use quaternions
use rotations
use noise
use HDF5
use HDFsupport
use ISO_C_BINDING
use omp_lib
use timing
use stringconstants
use math

IMPLICIT NONE

type(EBSDdefectNameListType),INTENT(INOUT)  :: enl
type(MCCLNameListType),INTENT(INOUT)        :: mcnl
type(EBSDMasterNameListType),INTENT(INOUT)  :: mpnl
integer(kind=irg),INTENT(IN)                :: ipar(3)
real(kind=dbl),INTENT(IN)                   :: fpar(4)
type(EBSDAnglePCDefType),INTENT(IN)         :: orpcdef
type(EBSDMCdataType),INTENT(INOUT)          :: EBSDMCdata
type(EBSDMPdataType),INTENT(INOUT)          :: EBSDMPdata
character(fnlen),INTENT(IN)                 :: progname
character(fnlen),INTENT(IN)                 :: nmldeffile

! all geometrical parameters and filenames
real(kind=dbl)                          :: prefactor, qz(3)

! allocatable arrays
real(kind=sgl),allocatable              :: binned(:,:), binned3(:,:,:)        ! array with EBSD patterns
real(kind=sgl),allocatable              :: eulerangles(:,:,:,:)

! arrays for each OpenMP thread
real(kind=sgl),allocatable              :: tmLPNH(:,:,:) , tmLPSH(:,:,:)
real(kind=sgl),allocatable              :: trgx(:,:), trgy(:,:), trgz(:,:)          ! auxiliary detector arrays needed for interpolation
integer(kind=irg)                       :: dims2(2),dims3(3)

! quaternion variables
real(kind=dbl)                          :: qq(4), qq1(4), qq2(4), qq3(4)

! various items
integer(kind=irg)                       :: i, j, iang, jang, k, io_int(6), hdferr, ii, jj, ix, iy, jx, jy, jz, rowoffset, &
                                           idx, idy, dvy1, dvy2, idz, nx, ny, nz, iz, dvx, dvy, dvz, px, py, pz, sv, ivx, ivz ! various counters
integer(kind=irg)                       :: istat, iipar(7), tick, tock, tickstart, xmin, xmax, ymin, ymax, zmax, thr
integer(kind=irg)                       :: nix, niy, binx, biny, nixp, niyp, maxthreads, wdims(3), outx, outy, ibx, iby                 ! various parameters
integer(kind=irg)                       :: NUMTHREADS, TID   ! number of allocated threads, thread ID
integer(kind=irg)                       :: nthreads

real(kind=sgl)                          :: tstart, tstop, io_real(3), stepratiox, stepratioy, stepratioz, bitrange, ma, mi
real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second
real(kind=dbl)                          :: dc(3), scl, pctr(3)           ! direction cosine array
real(kind=dbl)                          :: sx, dx, dxm, dy, dym, rhos, x         ! various parameters
real(kind=dbl)                          :: ixy(2), tmp

character(kind=c_char),allocatable      :: batchpatterns32(:,:,:,:), bpat(:,:), patternrow(:,:,:,:) 
real(kind=sgl),allocatable              :: master_arrayNH(:,:), master_arraySH(:,:)
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)

! parameter for random number generator
type(HDFobjectStackType)                :: HDF_head
type(unitcell)                          :: cell
integer(HSIZE_T)                        :: dims4(4), cnt4(4), offset4(4)
character(fnlen,kind=c_char)            :: line2(1)
character(fnlen)                        :: groupname, dataset, datagroupname, EMtmpname
character(4)                            :: tmpid
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
character(fnlen)                        :: datafile, tmpname
logical                                 :: overwrite = .TRUE., insert = .TRUE., singlebatch, g_exists

! new stuff: deformation tensor
real(kind=dbl),allocatable              :: Fmatrix(:,:,:), Farray(:,:)
real(kind=sgl),allocatable              :: wsample(:,:,:), stackrow(:,:,:,:), mypatternrow(:,:,:), patternrowsum(:,:,:,:), &
                                           mystackrow(:,:,:,:), quarray(:,:)
real(kind=dbl)                          :: FF(3,3), FF_inv(3,3), gs2c(3,3)
integer(kind=irg),allocatable           :: sums(:)

!====================================
! max number of OpenMP threads on this platform
maxthreads = omp_get_max_threads()

bitrange = 255.0

if (enl%sampleInteractionVolume.eqv..TRUE.) then 
! first apply a Gaussian beam spread to the Monte Carlo accum_xyz array ... 
! we do this by defining a small Gaussian weight-factor array and adding 
! the shifted interaction volume array for all relevant shifts using the 
! eoshift intrinsic.  
  call Message(' --> applying Gaussian beam source spread to interaction volume array ...')
  call applyGaussianBeamSpread( (/ mcnl%ivolx, mcnl%ivoly, mcnl%ivolz /), (/ mcnl%ivolstepx, mcnl%ivolstepy /), &
                                EBSDMCdata%accum_xyz, dble(enl%spotsize), verbose=.FALSE.)
  call Message('     done.')

! normalize the values to have a max value of 1 in the entire array
! set all the points with values below the threshold equal to zero and then find 
! the bounds of the non-zero volume along x, y, and z 
  call Message(' --> determining interaction volume array bounds above 0.1% threshold level ')
  thr = 0.001 * maxval(EBSDMCdata%accum_xyz)
  where(EBSDMCdata%accum_xyz.lt.thr) EBSDMCdata%accum_xyz = 0

! along x
  allocate(sums(mcnl%ivolx))
  sums = sum(sum(EBSDMCdata%accum_xyz, dim=3), dim=1)
  xmin = 1
  do while (sums(xmin).eq.0) 
    xmin = xmin+1
  end do 
  xmax= mcnl%ivolx
  do while (sums(xmax).eq.0) 
    xmax = xmax-1
  end do 
  deallocate(sums) 

! along y
  allocate(sums(mcnl%ivoly))
  sums = sum(sum(EBSDMCdata%accum_xyz, dim=3), dim=2)
  ymin = 1
  do while (sums(ymin).eq.0) 
    ymin = ymin+1
  end do 
  ymax= mcnl%ivoly
  do while (sums(ymax).eq.0) 
    ymax = ymax-1
  end do 
  deallocate(sums) 

! along z
  allocate(sums(mcnl%ivolz))
  sums = sum(sum(EBSDMCdata%accum_xyz, dim=1), dim=1)
  zmax = 1
  do while (sums(zmax).ne.0) 
    zmax = zmax+1
  end do 
  zmax = zmax - 1
  deallocate(sums) 

! we need to have each pattern from a given depth slice contribute to all the final patterns
! that have a non-zero contribution in the interaction volume.
  nx = (mcnl%ivolx-1)/2
  ny = (mcnl%ivoly-1)/2
  nz = mcnl%ivolz
  stepratiox = fpar(2) / mcnl%ivolstepx
  stepratioy = fpar(3) / mcnl%ivolstepy
  stepratioz = fpar(4) / mcnl%ivolstepz

! sample the histogram at the same grid locations as the requested array of EBSD patterns
! take into account the (x,y) -> (-y,x) rotation for the correct interaction volume orientation
! the sample at each point contains the total of the values in the larger pixel (in x and y) 
  dvx = nint((xmax-xmin)*0.5*mcnl%ivolstepx/fpar(2))
  ! dvy = nint((ymax-ymin)*mcnl%ivolstepy/fpar(3))
  dvy1 = nint((ymin-ny)*mcnl%ivolstepy/fpar(3))
  dvy2 = nint((ymax-ny)*mcnl%ivolstepy/fpar(3))
  dvz = nint(zmax*mcnl%ivolstepz/fpar(4))
  ! case when the input data is thinner than the required interaction volume depth
  if (dvz.gt.ipar(3)) then
    call Message('')
    call Message('Warning: Interaction Volume Depth Greater than the Depth of Input Data')
    call Message('Warning: Now Using the Depth of Input Data to Sample the Interaction Volume Data')
    call Message('')
     dvz=ipar(3)
  end if
  
  allocate(wsample(-dvx:dvx,dvy1:dvy2,dvz))
  do ix=-dvx,dvx
    px = nint(ix * fpar(2)/mcnl%ivolstepx)
    do iy=dvy1,dvy2
      py = nint(iy * fpar(3)/mcnl%ivolstepy)
      do iz=1,dvz 
        pz = nint(iz * fpar(4)/mcnl%ivolstepz)
        wsample(ix, iy, iz) = EBSDMCdata%accum_xyz(py, px, pz)
      end do 
    end do 
  end do 
! and normalize to the highest count
  wsample = wsample / maxval(wsample)

! used to debug the array orientation:
!
open(unit=dataunit,file='ivolume.data',status='unknown',form='unformatted')
write (dataunit) shape(EBSDMCdata%accum_xyz)
write (dataunit) EBSDMCdata%accum_xyz 
close(unit=dataunit,status='keep')

open(unit=dataunit,file='wsample_new.data',status='unknown',form='unformatted')
write (dataunit) shape(wsample)
write (dataunit) wsample 
close(unit=dataunit,status='keep')
!

  call Message('')
  call Message(' Interaction volume parameters ')
  call Message(' ----------------------------- ')
  io_int(1:3) = (/ mcnl%ivolx, mcnl%ivoly, mcnl%ivolz /)
  call WriteValue('   interaction volume array size :',io_int,3)
  io_real(1:3) = (/ mcnl%ivolstepx, mcnl%ivolstepy, mcnl%ivolstepz /)
  call WriteValue('   interaction volume voxel size :',io_real,3)
  io_real(1:3) = io_int(1:3) * io_real(1:3)
  call WriteValue('   interaction volume [nm^3]     :',io_real,3)

  call Message('')
  call Message(' Sampled interaction volume ')
  call Message(' -------------------------- ')
  io_int(1:3) = shape(wsample)
  call WriteValue('   sampled ivol array size       : ', io_int, 3) 
  io_real(1:3) = fpar(2:4)
  call WriteValue('   sampled ivol array voxel size : ', io_real, 3) 
  io_real(1:3) = io_int(1:3) * fpar(2:4)
  call WriteValue('   sampled ivol [nm^3]           : ', io_real, 3) 

! due to the size of the interaction volume, the actual range of patterns that will be computed
! is smaller than the dimensions of the deformation volume, since the entire interaction volume 
! must fit inside the deformation volume in order to have reasonable simulated patterns with a 
! constant intensity level troughout...
  wdims = shape(wsample)
  outx = ipar(1) - wdims(1) + 1   ! +1 to correct for central column
  outy = ipar(2) - wdims(2) + 1   ! +1 to correct for incident beam row
  call Message('')
  call Message(' Pattern output area ')
  call Message(' ------------------- ')
  io_int(1:2) = (/ outx, outy /)
  call WriteValue('   array size                    : ',io_int,2)
  io_real(1:2) = io_int(1:2) * fpar(2:3)
  call WriteValue('   final image area [nm^2]       : ',io_real,2)

  if ((outx.le.0).or.(outy.le.0)) then 
    call Message('')
    call Message(' The final output array has negative dimensions... this indicates that the ')
    call Message(' interaction volume is larger than the deformation volume in at least one direction')
    call FatalError('ComputedeformedEBSDPatterns', 'No meaningfull pattern computations can be carried out.')
  end if 

! we also need to set where the incident beam is located in the interaction volume 
  iby = 0
  ibx = abs(dvy1)+1
  sv = abs(dvy1)+dvy2+1
  io_int(1:2) = (/ ibx, iby /)
  call Message('')
  call WriteValue(' Incident beam is located at position ', io_int, 2)
  io_int(1) = sv 
  call WriteValue(' Vertical dimension of interaction volume ', io_int, 1)
  call Message('')
else
  call Message(' Deformation volume ')
  call Message(' ------------------ ')
  io_int(1:3) = ipar(1:3)
  call WriteValue('   deformed volume array size    : ',io_int,3)
  io_real(1:3) = fpar(2:4)
  call WriteValue('   deformed volume voxel size    : ',io_real,3)
  io_real(1:3) = ipar(1:3) * fpar(2:4)
  call WriteValue('   deformed volume [nm^3]        : ',io_real,3)
end if 

nullify(HDF_head%next)

! Normally we would define the energy range here, but the energy integration has already been carried out.
! For that reason, in this program, there is no realistic background computation. 

!====================================
! init a bunch of parameters; many of these are leftovers from the original EMEBSD program and may not be used here...
!====================================
! binned pattern array
binx = enl%numsx
biny = enl%numsy

!====================================
cell%fname = trim(mcnl%xtalname)
call ReadDataHDF(cell)
call CalcMatrices(cell)

!====================================
! ------ and open the output file for IDL visualization (only thread 0 can write to this file)
!====================================
! we need to write the image dimensions, and also how many of those there are...

! Initialize FORTRAN interface.
!
CALL h5open_EMsoft(hdferr)

call timestamp(datestring=dstr, timestring=tstrb)
tstre = tstrb

! Create a new file using the default properties.
datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
datafile = EMsoft_toNativePath(datafile)

hdferr =  HDF_createFile(datafile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createFile ')

! write the EMheader to the file
datagroupname = 'DefectEBSD'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLfiles')

! read the text file and write the array to the file
dataset = SC_EMEBSDdefectNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetTextFile EMEBSDdefectNML')

call HDF_pop(HDF_head)

! create a NMLparameters group to write all the namelist entries into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLparameters')

call HDFwriteEBSDdefectNameList(HDF_head, enl)

! and leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup EMData')
hdferr = HDF_createGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup DefectEBSD')

dataset = SC_xtalname
allocate(stringarray(1))
stringarray(1)= trim(mcnl%xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetStringArray xtalname')

dataset = SC_numangles
hdferr = HDF_writeDatasetInteger(dataset, 1, HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger numangles')

! define the integer parameter list for the CalcEBSDPatternDefect call
iipar(1) = 1
iipar(2) = enl%numsx
iipar(3) = enl%numsy
iipar(4) = mpnl%npx
iipar(5) = mpnl%npx
iipar(6) = 1
iipar(7) = ipar(3)

dims3 = shape(EBSDMPdata%mLPNH)
if (dims3(3).lt.ipar(3)) iipar(7) = dims3(3)

! add the Euler angles to the output file
allocate(eulerangles(3,iipar(7),ipar(1),ipar(2)))
do k=1,iipar(7)
  do ix=1,ipar(1)
    do iy=1,ipar(2)
      eulerangles(1:3,k,ix,iy) = qu2eu(orpcdef%quatangfield(1:4,k,ix,iy))
    end do
  end do
end do
dataset = SC_Eulerangles
hdferr = HDF_writeDatasetFloatArray4D(dataset, eulerangles, 3, iipar(7), ipar(1), ipar(2), HDF_head) 
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloatArray4D Eulerangles')

! here we add the array that contains all the computed patterns; we'll access this 
! as a hyperslab array later on, except for interaction volume mode

if (enl%sampleInteractionVolume.eqv..TRUE.) then 
! allocate the batchpatterns array for hyperslab writing 
  allocate(batchpatterns32(binx,biny,outx,1),stat=istat)  ! outx = number of x-pixels
  batchpatterns32 = char(0)

! create the hyperslab and write zeroes to it for now
  dataset = SC_EBSDpatterns
    dims4 = (/ enl%numsx, enl%numsy, outx , outy /)
    cnt4 = (/ enl%numsx, enl%numsy, outx, 1 /)
    offset4 = (/ 0, 0, 0, 0 /)
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeHyperslabCharArray4D(dataset, batchpatterns32, dims4, offset4, cnt4, HDF_head, insert)
    else
      hdferr = HDF_writeHyperslabCharArray4D(dataset, batchpatterns32, dims4, offset4, cnt4, HDF_head)
    end if
else ! no interaction volume sampling
! allocate the batchpatterns array for hyperslab writing 
  allocate(batchpatterns32(binx,biny,ipar(1),1),stat=istat)  ! ipar(1) = number of x-pixels
  batchpatterns32 = char(0)

! create the hyperslab and write zeroes to it for now
  dataset = SC_EBSDpatterns
    dims4 = (/ enl%numsx, enl%numsy, ipar(1) , ipar(2) /)
    cnt4 = (/ enl%numsx, enl%numsy, ipar(1), 1 /)
    offset4 = (/ 0, 0, 0, 0 /)
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeHyperslabCharArray4D(dataset, batchpatterns32, dims4, offset4, cnt4, HDF_head, insert)
    else
      hdferr = HDF_writeHyperslabCharArray4D(dataset, batchpatterns32, dims4, offset4, cnt4, HDF_head)
    end if
end if

! and we leave this group open for further data output from the main program loop ... 

!====================================
! ------ start the actual pattern computation loop
!====================================

!====================================
! determine the scale factor for the Lambert interpolation
scl = dble(mpnl%npx) 
prefactor = enl%beamcurrent * enl%dwelltime * 1.0D-6

!====================================
! set the number of OpenMP threads 
if (enl%nthreads.eq.0) then 
  nthreads = OMP_GET_MAX_THREADS()
else
  nthreads = enl%nthreads
end if 

! if there are fewer pixels along a line than there are threads, set the number
! of threads equal to the number of pixels (since we are writing the patterns one line at a time).
if (nthreads.gt.ipar(1)) nthreads = ipar(1)

call OMP_SET_NUM_THREADS(nthreads)
io_int(1) = nthreads
call WriteValue(' Setting number of threads to ',io_int,1,"(I4)")

call Time_tick(tickstart)
call Time_tick(tick)

! should we use regular tmp files (slow) or memory files (can be very fast)
if (trim(enl%tmpfspath).eq.'undefined') then 
  EMtmpname = EMsoft_getEMtmppathname()
else
  EMtmpname = trim(enl%tmpfspath)//'/'
end if

!====================================
!====================================
! we will do this computation one horizontal line at a time and have each thread do a full depth integration; 
! whichever thread reaches the end of a line waits for the others and write the line patterns to the HDF5 file.

! with the interaction volume shape and percentage data, we need to perform a volume integration as part of the 
! summation, and this requires some clever pattern handling...

if (enl%sampleInteractionVolume.eqv..FALSE.) then

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL default(shared)  PRIVATE(TID, NUMTHREADS, ii, j, istat, binned, tmLPNH, tmLPSH, trgx, trgy, trgz)&
!$OMP& PRIVATE(Fmatrix, Farray, FF, gs2c, FF_inv, ix, pctr, bpat, mi, ma, quarray)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

! each thread needs a private copy of the master arrays; not having
! those can produce poor scaling... in addition, they need to be recomputed for each pattern !
  allocate(trgx(enl%numsx,enl%numsy), trgy(enl%numsx,enl%numsy), trgz(enl%numsx,enl%numsy))
  allocate(Farray(9,iipar(7)), Fmatrix(3,3,iipar(7)))
  allocate(quarray(4,iipar(7)))
  allocate(tmLPNH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,iipar(7)), tmLPSH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,iipar(7)))
  allocate(binned(enl%numsx,enl%numsy), bpat(enl%numsx,enl%numsy))

! and copy the data in
  tmLPNH = EBSDMPdata%mLPNH
  tmLPSH = EBSDMPdata%mLPSH

  do iy=1, ipar(2)
!$OMP DO SCHEDULE(DYNAMIC,1)  
    do ix=1, ipar(1)   ! loop over the entries along rows in the output ROI


! invert the transposed deformation tensors for this pattern and depth series
        Farray = orpcdef%deftensorfield(1:9,1:iipar(7),ix,iy)
        quarray= orpcdef%quatangfield(1:4,1:iipar(7),ix,iy)
        do j=1,iipar(7)
            FF = transpose(reshape(Farray(1:9,j), (/ 3,3 /)) )
            if (enl%Fframe.eq.'samp') then
              gs2c = DBLE(qu2om(quarray(1:4,j)))
              FF = matmul(matmul(gs2c,FF),transpose(gs2c))
            end if
            call mInvert(FF, FF_inv, .FALSE.)
            Fmatrix(1:3,1:3,j) = FF_inv(1:3,1:3)
        end do

! for each pattern we need to compute the detector arrays 
        pctr = (/ orpcdef%pcfield(1,ix,iy), orpcdef%pcfield(2,ix,iy), fpar(1) /)
        call GeneratedefectEBSDDetector(enl, mcnl, enl%numsx, enl%numsy, trgx, trgy, trgz, pctr)

! loop over the depth and add all patterns together for each ROI pixel
        binned = 0.0
        call CalcEBSDPatternDefect(iipar,quarray,tmLPNH,tmLPSH,trgx,trgy,trgz,binned,prefactor,Fmatrix)

! and put the pattern in the correct spot in the batch array   
        ma = maxval(binned)
        mi = minval(binned)
        binned = ((binned - mi)/ (ma-mi))
        bpat = char(nint(bitrange*binned))

        batchpatterns32(1:binx,1:biny, ix, 1) = bpat(1:binx, 1:biny)

   end do  ! (ix loop)
!$OMP END DO

! we wait here for all the threads to come together, and then we write the patterns
! for this line to the HDF5 file using thread 0
!$OMP BARRIER
   if (TID.eq.0) then 
       io_int(1) = iy 
       call WriteValue(' -> completed row ',io_int,1)
       dataset = SC_EBSDpatterns
          dims4 = (/ enl%numsx, enl%numsy, ipar(1) , ipar(2) /)
          cnt4 = (/ enl%numsx, enl%numsy, ipar(1), 1 /)
          offset4 = (/ 0, 0, 0, ipar(2)-iy /)
          call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
          if (g_exists) then 
            hdferr = HDF_writeHyperslabCharArray4D(dataset, batchpatterns32, dims4, offset4, cnt4, HDF_head, overwrite)
          end if
   end if

  end do ! end of iy loop
!$OMP END PARALLEL

else  ! sampleInteractionVolume = .TRUE.
! allocate an array to hold one row of pattern depth stacks
  allocate(stackrow(enl%numsx,enl%numsy,iipar(7),ipar(1)))
  stackrow = 0.0

! and a row of final patterns, to be written to the HDF5 output file (hyperslab)
  allocate(patternrow(enl%numsx,enl%numsy,outx,1))
  patternrow = char(0)

! final patterns
  allocate(patternrowsum(enl%numsx,enl%numsy,outx,1))
  patternrowsum = 0.0

! loop over all the deformation field rows 
  rowoffset = 0
  do iy=1, ipar(2)

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL default(shared)  PRIVATE(TID, NUMTHREADS, ii, j, istat, binned3, tmLPNH, tmLPSH, trgx, trgy, trgz)&
!$OMP& PRIVATE(Fmatrix, Farray, FF, gs2c, FF_inv, ix, jx, jy, pctr, quarray)

    NUMTHREADS = OMP_GET_NUM_THREADS()
    TID = OMP_GET_THREAD_NUM()

! each thread needs a private copy of the master arrays; not having those can produce poor scaling...
    allocate(trgx(enl%numsx,enl%numsy), trgy(enl%numsx,enl%numsy), trgz(enl%numsx,enl%numsy))
    allocate(Farray(9,iipar(7)), Fmatrix(3,3,iipar(7)))
    allocate(quarray(4,iipar(7)))
    allocate(tmLPNH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,iipar(7)), tmLPSH(-mpnl%npx:mpnl%npx,-mpnl%npx:mpnl%npx,iipar(7)))
    allocate(binned3(enl%numsx,enl%numsy,iipar(7)))

! and copy the data in
    tmLPNH = EBSDMPdata%mLPNH
    tmLPSH = EBSDMPdata%mLPSH

!$OMP DO SCHEDULE(DYNAMIC,1)  
      do ix=1, ipar(1)   ! loop over the entries along rows in deformation field array

! invert the transposed deformation tensors for this pattern and depth series
        Farray = orpcdef%deftensorfield(1:9,1:iipar(7),ix,iy)
        quarray= orpcdef%quatangfield(1:4,1:iipar(7),ix,iy)
        do j=1,iipar(7)
            FF = transpose(reshape(Farray(1:9,j), (/ 3,3 /)) )
            if (enl%Fframe.eq.'samp') then
              gs2c = DBLE(qu2om(quarray(1:4,j)))
              FF = matmul(matmul(gs2c,FF),transpose(gs2c))
            end if
            call mInvert(FF, FF_inv, .FALSE.)
            Fmatrix(1:3,1:3,j) = FF_inv(1:3,1:3)
        end do

! for each pattern we need to compute the detector arrays 
        pctr = (/ orpcdef%pcfield(1,ix,iy), orpcdef%pcfield(2,ix,iy), fpar(1) /)
        call GeneratedefectEBSDDetector(enl, mcnl, enl%numsx, enl%numsy, trgx, trgy, trgz, pctr)

! loop over the depth and return all individual slices in the binned array
        binned3 = 0.0
        call CalcEBSDPatternDefect(iipar,quarray,tmLPNH,tmLPSH,trgx,trgy,trgz,binned3,prefactor,Fmatrix)

        stackrow(:,:,:,ix) = binned3(:,:,:)
      end do 
!$OMP END DO
! and write this to a binary file with name  tmp_iy.data  in the EMsoft tmp folder 
      if (TID.eq.0) then 
        write (tmpid,"(I4.4)") iy
        tmpname = trim(EMtmpname)//'tmp_'//tmpid//'.data'
        tmpname = EMsoft_toNativePath(tmpname)
        open(unit=dataunit,file=trim(tmpname),status='unknown',form='unformatted')
        write (dataunit) stackrow
        close(unit=dataunit,status='keep')
        call Message('   ---> generated row file '//trim(tmpname))
      end if 
!$OMP BARRIER 
  deallocate(trgx, trgy, trgz, Farray, quarray, Fmatrix, tmLPNH, tmLPSH, binned3)

!$OMP END PARALLEL 

    if (iy.ge.sv) then 
! The first time we get here we have enough row files to compute the first row of real averaged patterns
! We need to read each of the row files in turn, and for each stack determine which final patterns that stack can 
! contribute to;  then we use the proper wsample weight factors to compute those patterns and store them 
! in the output file as a hyperslab array.
! It should be noted that the row of patterns is shorter than the input deformation array because due to the 
! interaction volume size there is a strip of patterns for which there is missing information; those patterns
! are not computed. 

      patternrowsum = 0.0

      allocate(mypatternrow(enl%numsx,enl%numsy,outx))  
      allocate(mystackrow(enl%numsx,enl%numsy,iipar(7),ipar(1) ) )

      do jy=1,sv 
            mypatternrow = 0.0
            mystackrow = 0.0

            write (tmpid,"(I4.4)") iy-sv+jy
            tmpname = trim(EMtmpname)//'tmp_'//tmpid//'.data'
            tmpname = EMsoft_toNativePath(tmpname)
            open(unit=dataunit,file=trim(tmpname),status='unknown',form='unformatted')
            read(dataunit) mystackrow
            close(unit=dataunit,status='keep')

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL default(shared)  PRIVATE(jx, TID, NUMTHREADS, ivx, ivz)&
!$OMP& PRIVATE(mi, ma, binned, bpat)

      NUMTHREADS = OMP_GET_NUM_THREADS()
      TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC,1)
        do jx=1,outx   ! loop over all the output patterns in this row 
          do ivx=-dvx,dvx
            do ivz=1,dvz
              if (wsample(ivx, jy-1+dvy1, ivz).ne.0.0) then 
                mypatternrow(:,:,jx) = mypatternrow(:,:,jx) + wsample(ivx, jy-1+dvy1, ivz) * mystackrow(:,:, ivz, jx+dvx+ivx)
              end if 
            end do
          end do 
        end do
!$OMP END DO
!$OMP BARRIER   
!$OMP END PARALLEL
      patternrowsum(:,:,:,1) = patternrowsum(:,:,:,1) + mypatternrow(:,:,:)
      end do

deallocate(mypatternrow, mystackrow)

! convert all the patterns in patternrowsum to byte grayscale values in patternrow for writing to HDF5 file


! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL default(shared) PRIVATE(TID, NUMTHREADS, jx, mi, ma, binned, bpat)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

  allocate(binned(enl%numsx,enl%numsy), bpat(enl%numsx,enl%numsy))
  binned = 0.0
  bpat = char(0)

!$OMP DO SCHEDULE(DYNAMIC,1)
      do jx=1,outx
        binned(:,:) = patternrowsum(:,:,jx,1)
        ma = maxval(binned)
        mi = minval(binned)
        binned = ((binned - mi)/ (ma-mi))
        bpat = char(nint(bitrange*binned))
        patternrow(:,:,jx,1) = bpat(:,:)
      end do 
!$OMP END DO
      deallocate(binned, bpat)
!$OMP BARRIER 
!$OMP END PARALLEL

! store the patterns in the output HDF5 file 
    dataset = SC_EBSDpatterns
      dims4 = (/  enl%numsx, enl%numsy, outx , outy /)
      cnt4 = (/ enl%numsx, enl%numsy, outx, 1 /)
      offset4 = (/ 0, 0, 0, rowoffset /)
      call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
      if (g_exists) then 
        hdferr = HDF_writeHyperslabCharArray4D(dataset, patternrow, dims4, offset4, cnt4, HDF_head, overwrite)
      else
        hdferr = HDF_writeHyperslabCharArray4D(dataset, patternrow, dims4, offset4, cnt4, HDF_head)
      end if
      rowoffset = rowoffset+1

! and delete the tmp file for the top row (no longer needed)
      if (rowoffset.lt.outy) then 
        write (tmpid,"(I4.4)") iy-sv+1
        tmpname = trim(EMtmpname)//'tmp_'//tmpid//'.data'
        tmpname = EMsoft_toNativePath(tmpname)
        write (*,*) ' deleting '//trim(tmpname)
        open(unit=dataunit,file=trim(tmpname),status='unknown',form='unformatted')
        close(unit=dataunit,status='delete')
      else  ! add the end of the run, delete all the remaining tmp files
        do ix=iy-sv+1, ipar(2)
          write (tmpid,"(I4.4)") ix
          tmpname = trim(EMtmpname)//'tmp_'//tmpid//'.data'
          tmpname = EMsoft_toNativePath(tmpname)
          write (*,*) ' deleting '//trim(tmpname)
          open(unit=dataunit,file=trim(tmpname),status='unknown',form='unformatted')
          close(unit=dataunit,status='delete')
        end do
      end if 
    end if
  end do
end if 
!====================================
!====================================

tstop = Time_tock(tickstart) 
tock = Time_tock(tick)
tstop = tstop - tstart

io_real(1) = tstop
call WriteValue('Execution time [CPU_TIME()] = ',io_real, 1)

io_int(1) = tock
call WriteValue('Execution time [system_clock()] = ',io_int,1,"(I8,' [s]')")

! and finalize the HDF5 file writing
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! update the end time
call timestamp(datestring=dstr, timestring=tstre)
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup EMheader')

datagroupname = "DefectEBSD"
hdferr = HDF_openGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup DefectEBSD')

! stop time /EMheader/StopTime 'character'
dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetStringArray StopTime')

dataset = SC_Duration
hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloat Duration')

! close the datafile
call HDF_pop(HDF_head,.TRUE.)

! close the Fortran interface
call h5close_EMsoft(hdferr)

call Message('')
call Message('  Computation completed; data stored in '//trim(datafile))

end subroutine ComputedeformedEBSDPatterns
