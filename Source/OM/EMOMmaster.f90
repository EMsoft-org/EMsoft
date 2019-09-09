! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMOMmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMOMmaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief program to compute a master Mueller Matrix
!
!> @date 09/06/17 MDG 1.0 initial version
!--------------------------------------------------------------------------
program EMOMmaster

use local
use files
use io 
use NameListTypedefs
use NameListHandlers
use JSONsupport

IMPLICIT NONE

character(fnlen)                   :: nmldeffile, progname, progdesc
type(OMmasterNameListType)         :: omnl
integer(kind=irg)                  :: res

nmldeffile = 'EMOMmaster.nml'
progname = 'EMOMmaster.f90'
progdesc = 'Computation of the master Mueller Matrix for a uniaxial crystal system'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 110 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
! call JSONreadEMgammaNameList(enl, nmldeffile, error_cnt)
  call Message('JSON input not yet implemented')
  STOP
else
  call GetOMmasterNameList(nmldeffile,omnl)
end if

! perform the master pattern simulations
call ComputeMasterMuellerMatrix(omnl, progname, nmldeffile)

end program EMOMmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeMasterMuellerMatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the uniaxial master Mueller matrix for an orientation hemisphere
!
!> @param omnl name list
!> @param progname program name
!> @param nmldeffile namelist file name
!
!> @date 09/06/13  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeMasterMuellerMatrix(omnl, progname, nmldeffile)

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
use diffraction
use noise
use HDF5
use HDFsupport
use iso_c_binding
use MuellerCalculus
use omp_lib
use timing

IMPLICIT NONE

type(OMmasterNameListType),INTENT(IN)    :: omnl
character(fnlen),INTENT(IN)              :: progname
character(fnlen),INTENT(IN)              :: nmldeffile

complex(kind=dbl)                        :: eps(2)    ! principal dielectric constants
complex(kind=dbl)                        :: rvals(4)  ! reflectivity coefficients
real(kind=dbl)                           :: lambda, xy(2), dc(3), edge, MM(4,4), xyz(3), Radius, theta, phii, tmp
real(kind=sgl)                           :: dmin, EkeV

type(unitcell)                           :: cell
type(DynType)                            :: Dyn
type(gnode)                              :: rlp

real(kind=dbl),allocatable               :: LPNH(:,:,:,:)
real(kind=sgl),allocatable               :: SPNH(:,:,:,:)
real(kind=sgl),allocatable               :: combo(:,:)

logical                                  :: f_exists, g_exists, overwrite=.TRUE.
integer(kind=irg)                        :: i, ii, jj, io_int(1), tstart, tstop, hdferr, nx, ny, istat, ierr
character(11)                            :: dstr
character(15)                            :: tstrb
character(15)                            :: tstre
character(fnlen)                         :: dataname, groupname, datagroupname, dataset
type(HDFobjectStackType)                 :: HDF_head

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(tstart)

! get the crystal structure from the *.xtal file
dmin = 0.1
EkeV = 1.0
call Initialize_Cell(cell,Dyn,rlp,omnl%xtalname, dmin, EkeV)

! illumination wave length [m]
lambda = omnl%wl * 1.0D-9 

! principal dielectric constants as complex numbers
eps = (/ cmplx(dble(omnl%eps1Re),dble(omnl%eps1Im)), cmplx(dble(omnl%eps2Re),dble(omnl%eps2Im)) /)

! allocate the output arrays
nx = 2*omnl%npx+1
ny = 2*omnl%npx+1

allocate(LPNH(4,4,-omnl%npx:omnl%npx,-omnl%npx:omnl%npx),stat=istat)
allocate(SPNH(4,4,-omnl%npx:omnl%npx,-omnl%npx:omnl%npx),stat=istat)

! dc = (/ 5.D0, 1.D0, 0.D0 /)
! dc = dc/NORM2(dc)

! rvals = MC_getUniaxialReflectivities(lambda, eps, 1.D0, dc, dble(omnl%theta))
! MM=  MC_getSampleMuellerMatrix(rvals)
! do ii=1,4
!   write(*,*) (MM(ii,jj),jj=1,4)
! end do 
! STOP
!===============================================
!  perform the actual computation of the master Mueller Matrix patterns
!===============================================
edge = 1.D0 / dble(omnl%npx)
call Message('Starting master Mueller Matrix computation')
do ii=-omnl%npx,omnl%npx
  do jj=-omnl%npx,omnl%npx
      xy = (/ dble(ii), dble(jj) /) * edge
      dc = LambertSquareToSphere(xy, ierr) 
      rvals = MC_getUniaxialReflectivities(lambda, eps, 1.D0, dc, dble(omnl%theta))
      MM = MC_getSampleMuellerMatrix(rvals)
      ! normalize against M[1,1] ; make this optional !!!
      if ((MM(1,1).ne.0.D0).and.(omnl%normalize.eqv..TRUE.)) MM = MM / MM(1,1)
      LPNH(1:4,1:4,ii,jj) = MM(1:4,1:4)
  end do
end do

! convert the arrays to stereographic projections
call Message('Starting stereographic projection conversion')
Radius = 1.D0
do ii=-omnl%npx,omnl%npx 
  do jj=-omnl%npx,omnl%npx 
    xy = (/ dble(ii), dble(jj) /) * edge
    xyz = StereoGraphicInverse( xy, ierr, Radius )
    xyz = xyz/NORM2(xyz)
    if (ierr.ne.0) then 
      SPNH(1:4,1:4,ii,jj) = 0.0
    else
      SPNH(1:4,1:4,ii,jj) = sngl(InterpolateLambert(xyz, LPNH, omnl%npx))
    end if
  end do
end do

! finally, place the SPs all together in a single array 
allocate( combo(4*nx, 4*ny) )
do ii=1,4
  do jj=1,4
    combo((jj-1)*nx+1:jj*nx,(ii-1)*ny+1:ii*ny) = transpose(SPNH(ii,jj,:,:))
  end do 
end do 

!===============================================
!===============================================
!===============================================
! 
tstop = Time_tock(tstart)
io_int(1) = tstop
call WriteValue('Total execution time [s] = ',io_int,1)

! write everything to an HDF5 file
! Initialize FORTRAN interface.
!
call h5open_EMsoft(hdferr)
call timestamp(datestring=dstr, timestring=tstre)

! get the filename; if it already exists, then delete it and create a new one
dataname = trim(EMsoft_getEMdatapathname())//trim(omnl%masterfile)
dataname = EMsoft_toNativePath(dataname)
inquire(file=trim(dataname), exist=f_exists)

if (f_exists) then
  open(unit=dataunit, file=trim(dataname), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

! Create a new file using the default properties.
hdferr =  HDF_createFile(dataname, HDF_head)

! write the EMheader to the file
datagroupname = SC_OMmaster
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_OMmasterNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteOMmasterNameList(HDF_head, omnl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
hdferr = HDF_createGroup(datagroupname, HDF_head)

! and start writing the data arrays
dataset = SC_OMmasterLPNH
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetDoubleArray4D(dataset, LPNH, 4, 4, nx, ny, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetDoubleArray4D(dataset, LPNH, 4, 4, nx, ny, HDF_head)
end if


! and also the stereographic projection version of these arrays
dataset = SC_OMmasterSPNH
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray4D(dataset, SPNH, 4, 4, nx, ny, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray4D(dataset, SPNH, 4, 4, nx, ny, HDF_head)
end if

! add the large output array
dataset = 'combo'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray2D(dataset, combo, 4*nx, 4*ny, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray2D(dataset, combo, 4*nx, 4*ny, HDF_head)
end if

! close the output file
call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

end subroutine ComputeMasterMuellerMatrix
