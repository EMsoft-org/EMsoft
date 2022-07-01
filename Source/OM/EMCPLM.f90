! ###################################################################
! Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMCPLM.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMCPLM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief program to predict intensity profiles based on Mueller matrix
!
!> @date 09/21/17 MDG 1.0 initial version
!--------------------------------------------------------------------------
program EMCPLM

use local
use files
use io 
use NameListTypedefs
use NameListHandlers
use JSONsupport

IMPLICIT NONE

character(fnlen)                   :: nmldeffile, progname, progdesc
type(CPLMNameListType)             :: omnl
integer(kind=irg)                  :: res

nmldeffile = 'EMCPLM.nml'
progname = 'EMCPLM.f90'
progdesc = 'Computation of the intensity profiles for a uniaxial/biaxial crystal system'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 111 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
! call JSONreadEMgammaNameList(enl, nmldeffile, error_cnt)
  call Message('JSON input not yet implemented')
  STOP
else
  call GetCPLMNameList(nmldeffile,omnl)
end if

! perform the master pattern simulations
call ComputeIntensityProfiles(omnl, progname, nmldeffile)

end program EMCPLM

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeIntensityProfiles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the 
!
!> @param omnl name list
!> @param progname program name
!> @param nmldeffile namelist file name
!
!> @date 09/06/13  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeIntensityProfiles(omnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use initializersHDF
use symmetry
use crystal
use constants
use Lambert
use io
use files
use rotations
use quaternions
use diffraction
use noise
use HDF5
use HDFsupport
use iso_c_binding
use MuellerCalculus
use omp_lib
use timing

IMPLICIT NONE

type(CPLMNameListType),INTENT(IN)        :: omnl
character(fnlen),INTENT(IN)              :: progname
character(fnlen),INTENT(IN)              :: nmldeffile

logical                                  :: f_exists, g_exists 
integer(kind=irg)                        :: i, j, ii, jj, io_int(1), hdferr, nx, ny, istat, ierr, numpoints
integer(HSIZE_T)                         :: dims4(4)
character(fnlen)                         :: dataname, groupname, datagroupname, dataset, fname, descriptor
character(2)                             :: angleformat
type(HDFobjectStackType)                 :: HDF_head
real(kind=dbl),allocatable               :: master(:,:,:,:), LPNH(:,:,:,:), euler(:,:), intensities(:,:), &
                                            quats(:,:), images(:,:,:), qrot(:,:)
real(kind=dbl)                           :: rod(4), cu(3), ho(3), qu(4), eu(3), vc(3), vr(3), dc(3), MM(4,4)
character(11)                            :: dstr
character(15)                            :: tstrb
character(15)                            :: tstre
integer(kind=irg)                        :: ixy(2), nix, niy, nixp, niyp, npx
real(kind=dbl)                           :: dx, dy, dxm, dym, phistepsize, scl
type(MuellerMatrixType)                  :: MMsample, MMchain
type(StokesVectorType)                   :: SVin, SV, SVout


call timestamp(datestring=dstr, timestring=tstrb)

!===============================================
! first of all, we read the master Mueller matrix data
! Initialize FORTRAN interface.
!
call h5open_EMsoft(hdferr)

nullify(HDF_head%next)


! get the filename; if it already exists, then delete it and create a new one
dataname = trim(EMsoft_getEMdatapathname())//trim(omnl%masterfile)
dataname = EMsoft_toNativePath(dataname)
inquire(file=trim(dataname), exist=f_exists)

if (f_exists.eqv..FALSE.) then
  call FatalError('ComputeIntensityProfiles','master input file not found')
end if

! open the file using readonly access
hdferr =  HDF_openFile(dataname, HDF_head, readonly = .TRUE.)

! open the correct group
groupname = SC_NMLparameters
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_CPLMmasterNameList
hdferr = HDF_openGroup(groupname, HDF_head)

! get the pattern dimension
dataset = SC_npx
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, npx)

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_CPLMmaster
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_CPLMmasterLPNH
call HDF_readDatasetDoubleArray4D(dataset, dims4, HDF_head, hdferr, LPNH)
allocate(master(4, 4, -npx:npx, -npx:npx))
do i=-npx,npx
  do j=-npx,npx
    master(1:4,1:4,i,j) = LPNH(1:4,1:4,i+1+npx,j+1+npx)
  end do
end do

call Message(' read data from'//trim(dataname))
!deallocate(LPNH)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

!===============================================
! next, read the angle data from text file (for now)
! open the input data file and read the orientation format and the number of orientations
fname = trim(EMsoft_getEMdatapathname())//trim(omnl%anglefile)
fname = EMsoft_toNativePath(fname)
call Message(' opening angle file '//trim(fname))
open(unit=53,file=trim(fname),status='old',action='read')
read (53,*) angleformat
read (53,*) numpoints

! make sure that numpoints equals the product of numpx and numpy
if (numpoints.ne.(omnl%numpx*omnl%numpy)) then
  call FatalError('ComputeIntensityProfiles','number of angles must equal number of image pixels')
end if

allocate(quats(4,numpoints))

! here is the loop over all points
do i = 1,numpoints
! we'll convert them first to Euler angle triplets, regardless of what the angleformat is...
  if (angleformat.eq.'ho') then
    read (53,*) ho(1:3)
    qu = ho2qu(ho)
  end if
  if (angleformat.eq.'qu') then
    read (53,*) qu(1:4)
  end if
  if (angleformat.eq.'ro') then
    read (53,*) rod(1:4)
    qu = ro2qu(rod)
  end if
  if (angleformat.eq.'cu') then
    read (53,*) cu(1:3)
    qu = cu2qu(cu)
  end if
  if (angleformat.eq.'eu') then
    read (53,*) eu(1:3)
    qu = eu2qu(eu*cPi/180.D0)
  end if
  quats(1:4,i) = conjg(qu(1:4))
end do
close(unit=53,status='keep')
call Message('    ---> completed reading angles ')

!===============================================
! allocate array for intensity results
allocate(intensities(omnl%phinum, numpoints))
intensities = 0.D0

!===============================================
! for each orientation in the list, generate phinum sets of direction cosines for the rotating [001] axis
! and for each of those, compute the Muller matrix by interpolation from the master pattern.  
! Then multiply the Mueller matrix with the analyzer matrix and the linearly polarized Stokes vector 
! and keep the first element of the resulting vector as the intensity.
vc = (/ 0.D0, 0.D0, 1.D0 /)  ! the original c-axis

! set up the sample rotations around the ND axis
allocate(qrot(4,omnl%phinum))
phistepsize = 2.D0*cPi/dble(omnl%phinum)
do i=1,omnl%phinum
  qrot(1:4,i) = ax2qu( (/0.D0, 0.D0, 1.D0, dble(i-1) * phistepsize /) )
end do

descriptor = 'computation step'

! define the input Stokes vector
SVin%S = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
SVin%descriptor = 'input Stokes vector'

SVin = MC_propagateStokesVector(MC_concatenateMuellerMatrices(MC_get_basicMuellerMatrix(1), &
        MC_get_basicMuellerMatrix(9)), SVin, descriptor)
!SVin = MC_propagateStokesVector(MC_get_basicMuellerMatrix(3), SVin, descriptor)

! define the analyzer optics
MMchain = MC_concatenateMuellerMatrices(MC_get_basicMuellerMatrix(9), &
        MC_rotate_MuellerMatrix(MC_get_basicMuellerMatrix(2), 1.D0*cPi/180.D0) )
!MMchain = MC_rotate_MuellerMatrix(MC_get_basicMuellerMatrix(4), 5.D0*cPi/180.D0)
write(*,*) trim(MMchain%descriptor)

scl = dble(npx)

!open(unit=53,file='dc.txt',status='unknown',form='formatted')

do i = 1,numpoints
  vr = quat_Lp(quats(1:4,i), vc)
!  write(53,"(2(F12.8,','),F12.8)") vr
if (vr(3).ne.1.D0) then 
! next we use these  to get the Mueller matrix by interpolation for each of the sample rotation steps
  do j = 1, omnl%phinum
    dc = quat_LP(qrot(1:4,j),vr)
    if (dc(3).le.0.D0) dc = -dc
! convert these direction cosines to coordinates in the Rosca-Lambert projection
    call LambertgetInterpolation(dc, scl, npx, npx, nix, niy, nixp, niyp, dx, dy, dxm, dym)

    MMsample%M = master(1:4,1:4,nix,niy) * dxm * dym + &
                 master(1:4,1:4,nixp,niy) * dx * dym + &
                 master(1:4,1:4,nix,niyp) * dxm * dy + &
                 master(1:4,1:4,nixp,niyp) * dx * dy 
! next we apply this to the incident Stokes vector 
    SV = MC_propagateStokesVector(MMsample, SVin, descriptor)
! and we propagate to the detector
    SVout = MC_propagateStokesVector(MMchain, SV, descriptor)
    intensities(j,i) = SVout%S(0)
  end do
end if
end do

!close(53,status='keep')

call timestamp(datestring=dstr, timestring=tstre)

!===============================================
! generate an HDF5 output file with the intensities and all other relevant variables.
! get the filename; if it already exists, then delete it and create a new one
dataname = trim(EMsoft_getEMdatapathname())//trim(omnl%outputfile)
dataname = EMsoft_toNativePath(dataname)
inquire(file=trim(dataname), exist=f_exists)

if (f_exists) then
  open(unit=dataunit, file=trim(dataname), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

! Create a new file using the default properties.
hdferr =  HDF_createFile(dataname, HDF_head)

! write the EMheader to the file
datagroupname = SC_CPLM
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_CPLMNameList
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteCPLMNameList(HDF_head, omnl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
hdferr = HDF_createGroup(datagroupname, HDF_head)

! and start writing the data arrays
dataset = SC_CPLMintensities
hdferr = HDF_writeDatasetDoubleArray2D(dataset, intensities, omnl%phinum, numpoints, HDF_head)

! here we also produce the images
allocate(images(omnl%phinum, omnl%numpx, omnl%numpy))
do i=1,omnl%phinum
  images(i,1:omnl%numpx,1:omnl%numpy) = reshape( intensities(i,1:numpoints), (/ omnl%numpx, omnl%numpy /) )
end do

dataset = SC_CPLMimages
hdferr = HDF_writeDatasetDoubleArray3D(dataset, images, omnl%phinum, omnl%numpx, omnl%numpy, HDF_head)

! close the output file
call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

end subroutine ComputeIntensityProfiles
