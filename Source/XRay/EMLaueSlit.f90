! ###################################################################
! Copyright (c) 2019-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMLaueSlit.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMLaueSlit
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Forward model for Laue patterns from a parallel slab with a divergent incident beam
!>        and a beam stop 
!
!> @date 01/28/20  MDG 1.0 original version 
!--------------------------------------------------------------------------
program EMLaueSlit

use local
use NameListTypedefs
use NameListHandlers
use files

character(fnlen)                        :: nmldeffile, progname, progdesc
type(LaueSlitNameListType)              :: lnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMLaueSlit.nml'
progname = 'EMLaueSlit.f90'
progdesc = 'Transmission Laue pattern computation for a divergent beam through a slit'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 252 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
!  call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetLaueSlitNameList(nmldeffile,lnl)
end if

! generate a realistic Laue master pattern
 call ComputeLauePattern(lnl, progname, nmldeffile)

end program EMLaueSlit

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeLauePattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Compute a Laue pattern for a given source, slit, sample, detector geometry
!> For now, we ignore most of the name list parameters and hard-code test values in 
!> the Laue_geometry structure.
!
!> @param lnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 07/30/19  MDG 1.0 original
!> @date 08/01/19  MDG 1.1 added optional backprojections to the output file
!--------------------------------------------------------------------------
subroutine ComputeLauePattern(lnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use NameListHandlers
use initializersHDF
use initializers
use xrdmod
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
use Lauemod
use postscript
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

type(LaueSlitNameListType),INTENT(INOUT)   :: lnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

integer(kind=irg)                          :: numangles, numbatches, remainder, ii, jj, pid, tickstart
integer(kind=irg),allocatable 			       :: batchnumangles(:)
integer(kind=irg),parameter 			         :: batchsize = 100
type(AngleType),pointer                    :: angles

integer(kind=irg)						               :: i, j, icnt, numvox, hdferr, npx, npy, refcnt, io_int(1), Lstart, &
                                              g(3), gr(3), rf, NUMTHREADS, TID, BPnpx, BPnpy, m, betamin, betamax
real(kind=sgl) 							               :: l, kouter, kinner, tstart, tstop, mi, ma, lambdamin, lambdamax, kv(3), &
                                              scl, kv2(3), shortg, info, gg, mps
real(kind=sgl),allocatable 				         :: pattern(:,:), patternsum(:,:,:), bppatterns(:,:,:), bp(:,:)
real(kind=dbl)                             :: qq(4)

type(HDFobjectStackType)                   :: HDF_head
type(unitcell)                             :: cell
logical 								                   :: verbose, f_exists, g_exists, insert=.TRUE., overwrite=.TRUE.

type(LaueMasterNameListType)               :: lmnl
type(Laue_grow_list),pointer               :: reflist, rltmp          

character(fnlen) 						               :: hdfname, groupname, datagroupname, attributename, dataset, fname, &
                                              TIFF_filename, Lauemode, BPmode
character(11)                              :: dstr
character(15)                              :: tstrb
character(15)                              :: tstre
character(4)							                 :: pnum
character(fnlen)						               :: HDF_FileVersion
integer(HSIZE_T)        			             :: dims3(3), cnt3(3), offset3(3)
character(fnlen,kind=c_char)               :: line2(1)

! declare variables for use in object oriented image module
integer                                    :: iostat
character(len=128)                         :: iomsg
logical                                    :: isInteger
type(image_t)                              :: im
integer(int8)                              :: i8 (3,4)
integer(int8), allocatable                 :: TIFF_image(:,:)

! Legendre lattitude arrays
real(kind=dbl),allocatable                 :: LegendreArray(:), upd(:), diagonal(:)

! new parameters for the slit model 
real(kind=dbl)    :: ds, dsvec(3), d0, d0vec(3), d, dvec(3), t, slitc(3), sw, sh, slitcorners(3,4), &
                     scuvec(3,4), scdet(3,4), scsbp(3,4), samplecenter(3), dx, dy, dz, kuvec(3)
integer(kind=irg) :: minvy, minvz, maxvy, maxvz, numvx, ix, iy, iz  
real(kind=sgl),allocatable :: kvecs(:,:), kvox(:,:), kinpre(:) 

  
nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
tstre = ''
call Time_tick(tickstart)

! read the list of orientations and convert them all to quaternions if they are not already
nullify(angles)
allocate(angles)
call Lauereadangles(lnl%orientationfile, numangles, angles, verbose=.TRUE.)

! compute the limiting wave numbers for the outer and inner Ewald spheres
kouter = getXRDwavenumber(sngl(lnl%VoltageH))
kinner = getXRDwavenumber(sngl(lnl%VoltageL))
lambdamin = 1.0/kouter
lambdamax = 1.0/kinner

!=============================================
!=============================================
! crystallography section 
!allocate(cell)        
verbose = .TRUE.

! clear the cell variable (set everything to zero)
call ResetCell(cell)

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
cell%SG%SYM_reduce=.TRUE.
cell%fname = trim(lnl%xtalname)
call CrystalData(cell,verbose)

! generate all atom positions
call CalcPositions(cell,'v')

!=============================================
!=============================================
! compute possible reflection list with kinematical intensities, and intensities
nullify(reflist)
lmnl%lambdamin = 1.0/kouter
lmnl%intfactor = lnl%intcutoffratio   ! default intensity cutoff factor (from EMLauemaster program)
call Laue_Init_Unit_Reflist(cell, lmnl, reflist, refcnt, verbose)

! go through the reflection list and determine the length of the shortest non-zero G-vector
rltmp => reflist%next
shortg = 100.D0 
do i=2,refcnt 
  gg = CalcLength(cell, real(rltmp%hkl), 'r') 
  if (gg.lt.shortg) shortg = gg 
  rltmp => rltmp%next 
end do 

! we need to compute the correct value for Lstart... setting to 4 for now
Lstart = 8

!=============================================
!=============================================
! start creation of the output file, using a hyperslab approach for the Laue patterns 
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Open a new file
  hdfname = trim(EMsoft_getEMdatapathname())//trim(lnl%hdfname)
  hdfname = EMsoft_toNativePath(hdfname)

  inquire(file=trim(hdfname), exist=f_exists)

  if (f_exists) then
    open(unit=dataunit, file=trim(hdfname), status='old',form='unformatted')
    close(unit=dataunit, status='delete')
  end if

  hdferr =  HDF_createFile(hdfname, HDF_head)

! write the EMheader to the file
  datagroupname = 'Laue'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_LaueNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteLaueSlitNameList(HDF_head, lnl)

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

! finally, write all the necessary data:  orientations and simulated patterns along with geometrical parameters
dataset = 'kouter'
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, kouter, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, kouter, HDF_head)
    end if

dataset = 'kinner'
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, kinner, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, kinner, HDF_head)
    end if

dataset = 'numangles'
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head)
    end if

! create the hyperslabs and write zeroes to them for now
allocate(patternsum(lnl%Ny, lnl%Nz, numangles))
patternsum = 0.0

! dataset = 'LauePatterns'
! 	  dims3 = (/ lnl%Ny, lnl%Nz, numangles /)
! 	  cnt3 = (/ lnl%Ny, lnl%Nz, numangles /)
! 	  offset3 = (/ 0, 0, 0 /)
! 	  hdferr = HDF_writeHyperslabFloatArray3D(dataset, patternsum, dims3, offset3, cnt3, HDF_head)
! deallocate(patternsum)

! should we add the backprojections to the same file ?
if (trim(lnl%backprojection).eq.'Yes') then 
  BPnpx = 2*lnl%BPx+1
  BPnpy = 2*lnl%BPx+1
  allocate(bppatterns(BPnpx, BPnpy, numangles))
  bppatterns = 0.0

! dataset = 'backprojections'
!     dims3 = (/ BPnpx, BPnpy, numangles /)
!     cnt3 = (/ BPnpx, BPnpy, numangles /)
!     offset3 = (/ 0, 0, 0 /)
!     hdferr = HDF_writeHyperslabFloatArray3D(dataset, bppatterns, dims3, offset3, cnt3, HDF_head)
end if
! deallocate(bppatterns)

! leave the output HDF5 file open so that we can write the hyperslabs as they are completed 


! all computations are done in mm
ds = lnl%Lx 
dsvec = (/ ds, 0.D0, 0.D0 /)   ! vector to the slit plane in x-direction 
d0 = lnl%Sx 
d0vec = (/ d0, 0.D0, 0.D0 /)   ! vector to the sample front surface in x-direction
d = lnl%sampletodetector 
dvec = (/ d0+d, 0.D0, 0.D0 /)  ! vector to the detector in x-direction
t = lnl%samplethickness 
slitc = (/ 0.D0, lnl%Ly, lnl%Lz /)  ! location of slit center in slit plane
! half width and height of the slit 
sw = lnl%Lw*0.5D0
sh = lnl%Lh*0.5D0

! vectors to the slit corners
slitcorners(1:3,1) = dsvec + slitc + (/ 0.D0, sw, sh /)
slitcorners(1:3,2) = dsvec + slitc + (/ 0.D0,-sw, sh /)
slitcorners(1:3,3) = dsvec + slitc + (/ 0.D0,-sw,-sh /)
slitcorners(1:3,4) = dsvec + slitc + (/ 0.D0, sw,-sh /)

! unit vectors to the slit corners
do i=1,4 
  scuvec(1:3,i) = slitcorners(1:3,i) / vecnorm(slitcorners(1:3,i))
end do

! write (*,*) 'slitcorners in slit plane '
! do i=1,4
!   write(*,*) (slitcorners(j,i), j=1,3), (scuvec(j,i), j=1,3)
! end do

! slit corner vectors extended to the detector plane; delineates the projected image of the slit
do i=1, 4
  l = dvec(1) / scuvec(1,i)
  scdet(1:3,i) = l * scuvec(1:3,i)
end do

! write (*,*) 'slitcorners in detector plane '
! do i=1,4
!   write(*,*) (scdet(j,i), j=1,3)
! end do


! the same in the sample back plane, which delineates the range of sample voxels to be considered
do i=1, 4
  l = (d0vec(1)+t) / scuvec(1,i)
  scsbp(1:3,i) = l * scuvec(1:3,i)
end do

! write (*,*) 'slitcorners in sample back plane '
! do i=1,4
!   write(*,*)  (scsbp(j,i), j=1,3)
! end do


! determine the integration range for voxels inside the sample 
minvy = int( minval(scsbp(2,:)) / lnl%vs) - 1
maxvy = int( maxval(scsbp(2,:)) / lnl%vs) + 1
minvz = int( minval(scsbp(3,:)) / lnl%vs) - 1
maxvz = int( maxval(scsbp(3,:)) / lnl%vs) + 1
numvx = int( t / lnl%vs) 

! the y and z coordinates need to be re-centered around 0 for the 
! following loops to function properly 
m = (maxvy-minvy)/2
minvy = -m
maxvy = m

m = (maxvz-minvz)/2
minvz = -m
maxvz = m

! write (*,*) 'voxel ranges : ', minvy, maxvy, minvz, maxvz, numvx 

! maximum number of sample voxels that can contribute to the pattern 
numvox = (maxvy-minvy)*(maxvz-minvz)*numvx 

! the coordinates of the projected center of the sample (in the back plane) 
samplecenter = sum(scsbp,2) * 0.25D0 

! write (*,*) 'sample back plane center : ', samplecenter 

! an incident wave vector is then proportional to a unit vector along the line connecting each of the 
! sample voxels to the source location; the complete pattern can then be formed by superimposing all 
! the individual patterns for each of these directions, taking into account the distance traveled
! in the sample (normal Beer's law absorption). The integration loop over the voxels is then as follows:
! (we use OpenMP to loop over the voxels)

allocate(kvecs(3,numvox), kvox(3,numvox), kinpre(numvox))

icnt = 0
do ix = 1, numvx
  dx = lnl%vs*( -0.5D0 - dble(ix-1) )
  do iy = minvy, maxvy 
    dy = lnl%vs * (0.5D0 + dble(iy))
    do iz = minvz, maxvz 
      dz = lnl%vs * (0.5D0 + dble(iz))
      kuvec = samplecenter + (/ dx, dy, dz /)
      kuvec = kuvec / vecnorm(kuvec)
! make sure this vector falls inside the projection of the slit into the back face of the sample
      scl = (lnl%Sx+lnl%samplethickness) / kuvec(1)
      kv = scl * kuvec
      if ( (kv(2).le.scsbp(2,1)).and.(kv(2).ge.scsbp(2,2)) .and. (kv(3).le.scsbp(3,1)).and.(kv(3).ge.scsbp(3,4)) ) then 
        icnt = icnt + 1
! write (*,*) ix, iy, iz, kuvec, scl, kv 
! get the distance traveled inside the sample for this k vector (we'll do the same after diffraction event)
        scl = (lnl%Sx) / kuvec(1)
        kv = scl * kuvec
        kv2 = samplecenter + (/ dx, dy, dz /)
        kinpre(icnt) = sqrt(sum( (kv-kv2)**2 ))
! write (*,*) scl, kv, kv2, kinpre(icnt)
! consider this vector for the diffraction process 
        kvecs(1:3, icnt) = kuvec(1:3)
        kvox(1:3, icnt) = (/ dx, dy+samplecenter(2), dz+samplecenter(3) /)  ! relative to projection center in sample back plane
        ! kvox(1:3, icnt) = (/ dx, dy, dz /)  ! relative to projection center in sample back plane
      end if 
    end do 
  end do 
end do 

! reset the total number of contributing voxels
numvox = icnt
io_int(1) = numvox
call WriteValue(' total number of sample voxels : ',io_int, 1, frm = "(I8)")
io_int(1) = refcnt
call WriteValue(' total number of potential reflections : ',io_int, 1, frm = "(I8)")

! next we need the Legendre lattitudes for the back projector 
call Message(' Computing Legendre lattitudinal grid values')
allocate(diagonal(BPnpx),upd(BPnpx))
diagonal = 0.D0
upd = (/ (dble(i) / dsqrt(4.D0 * dble(i)**2 - 1.D0), i=1,BPnpx) /)
call dsterf(BPnpx-2, diagonal, upd, info) 
! the eigenvalues are stored from smallest to largest and we need them in the opposite direction
allocate(LegendreArray(0:BPnpx-1))
LegendreArray(0:BPnpx-1) = diagonal(BPnpx:1:-1)
! set the center eigenvalue to 0
LegendreArray((BPnpx-1)/2) = 0.D0
deallocate(diagonal, upd)

!=============================================
!=============================================
! Here we perform the actual simulations; the threads cover all the sample voxels for a 
! given orientation and the resulting patterns are summed together.
! Then we go to the next orientation.
! Write them to the HDF5 output file, along with (optionally) the pattern tiff files

Lauemode = 'transmission'
patternsum = 0.0 
bppatterns = 0.0

! set the number of OpenMP threads 
  call OMP_SET_NUM_THREADS(lnl%nthreads)
  io_int(1) = lnl%nthreads
  call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

! outer loop ... 
  do ii = 1, numangles
    qq = dble(angles%quatang(1:4,ii))
    write (*,*) 'working on orientation ', qq

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, pid, pattern)

    NUMTHREADS = OMP_GET_NUM_THREADS()
    TID = OMP_GET_THREAD_NUM()
    allocate(pattern(lnl%Ny, lnl%Nz))

!$OMP DO SCHEDULE(DYNAMIC)
    do pid = 1,numvox
      pattern = getLaueSlitPattern(lnl, qq, reflist, lambdamin, lambdamax, refcnt, & 
                                   kinpre(pid), kvecs(1:3,pid), kvox(1:3,pid), lnl%binarize )
!$OMP CRITICAL
     patternsum(:, :, ii) = patternsum(:, :, ii) + pattern(:, :) ! **lnl%gammavalue
!$OMP END CRITICAL
    end do 
!$OMP END DO

    if (TID.eq.0) write (*,*) 'batch ',ii,'; patterns completed ', maxval(patternsum(:,:,ii))
    deallocate(pattern)

! end of OpenMP portion
!$OMP END PARALLEL

    if (lnl%binarize.eqv..TRUE.) then 
      mps = maxval(patternsum(:,:,ii))
      write (*,*) 'max value for this pattern = ', mps
      where (patternsum(:,:,ii) .gt. mps*0.01)
        patternsum(:,:,ii) = 1.0
      end where
      where (patternsum(:,:,ii) .le. mps*0.01)
        patternsum(:,:,ii) = 0.0
      end where
    end if 
end do ! outer loop
! 

  tstop = Time_tock(tickstart)
  io_int(1) = tstop
  call WriteValue('Execution time [s]: ',io_int,1)


! write the hyperslab to the HDF5 file 
dataset = 'LauePatterns'
  hdferr = HDF_writeDatasetFloatArray3D(dataset, patternsum, lnl%Ny, lnl%Nz, numangles, HDF_head)


  if (trim(lnl%backprojection).eq.'Yes') then 
    call Message('Starting pattern back projection computation')
    allocate(bp(1:BPnpx,1:BPnpy))
    BPmode = 'forward'
    do ii=1,numangles
      bp = 0.0

      bp = backprojectLauePattern( (/kouter, kinner/), sngl(lnl%ps), sngl(lnl%sampletodetector), Lstart, (/lnl%Ny, lnl%Nz/), &
                                   (/lnl%BPx, lnl%BPx/), patternsum(1:lnl%Ny,1:lnl%Nz,ii), BPmode, LegendreArray)
      write (*,*) ' max intensity in backprojection : ', maxval(bp), maxval(patternsum(1:lnl%Ny,1:lnl%Nz,ii))
      bppatterns(:,:,ii) = bp
    end do
    deallocate(bp)

dataset = 'backprojections'
    hdferr = HDF_writeDatasetFloatArray3D(dataset, bppatterns, BPnpx, BPnpy, numangles, HDF_head)
  end if


 call HDF_pop(HDF_head)
 call HDF_pop(HDF_head)
 call timestamp(datestring=dstr, timestring=tstre)

! update the time string
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)


dataset = SC_Duration
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

 call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)

npx = lnl%Ny 
npy = lnl%Nz 

! optionally, write the individual tiff image files 
 do ii=1,numangles
! output the ADP map as a tiff file 
    write (pnum,"(I4.4)") ii
    fname = trim(EMsoft_getEMdatapathname())//trim(lnl%tiffprefix)//'_'//pnum//'.tiff'
    fname = EMsoft_toNativePath(fname)
    TIFF_filename = trim(fname)

! allocate memory for image
    allocate(TIFF_image(npx,npy))

! fill the image with whatever data you have (between 0 and 255)
    ma = maxval(patternsum(:,:,ii))
    mi = minval(patternsum(:,:,ii))

    TIFF_image = 255 - int(255 * (patternsum(:,:,ii)-mi)/(ma-mi))

! set up the image_t structure
    im = image_t(TIFF_image)
    if(im%empty()) call Message("ComputeLauePattern","failed to convert array to image")

! create the file
    call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
    if(0.ne.iostat) then
        call Message("failed to write image to file : "//iomsg)
    ! else  
    !   call Message('Laue pattern written to '//trim(TIFF_filename))
    end if 
    deallocate(TIFF_image)
end do 

end subroutine ComputeLauePattern
