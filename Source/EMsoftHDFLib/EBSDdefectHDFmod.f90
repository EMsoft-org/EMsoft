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
! EMsoft:EBSDdefectHDFmod.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EBSDdefectHDFmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EBSDdefectHDFmod routines for defect deformation field reading
!
!> @date  11/05/19  MDG 1.0 new module
!-----------------------------------------------------------------------
module EBSDdefectHDFmod

use local
use typedefs 

IMPLICIT NONE

contains


!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDreadorpcdefHDF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read angles, pattern centers, and deformation tensor field from an HDF5 file
!
!> @param enl EBSD name list structure
!> @param ipar integer parameter array 
!> @param fpar float parameter array 
!> @param orpcdef array of unit quaternions, pattern centers, and deformation tensors (output)
!
! file format description:
! HDF5 file with a single Group at the top level:  
!
! DeformationFieldInfo
!
! the following datasets should be inside this group
!
! integers:
! npix  : number of region-of-interest pixels along x
! npiy  : number of region-of-interest pixels along y
! npiz  : number of depth steps
!
! floats:
! stepx : stepsize in nm along x
! stepy : stepsize in nm along y
! stepz : stepsize in nm along z
! 
! eu(3) : (phi_1, Phi, phi_2) Euler angles for grain orientation (in degrees)
!
! ; pattern center coordinates in EMsoft convention
! pcx(npix,npiy) : pattern center x-coordinates for all points in ROI
! pcy(npix,npiy) : pattern center y-coordinates for all points in ROI
! L              : distance sample-scintillator in microns
!
! deftensor(9,npiz,npix,npiy) : deformation tensor components for each voxel in ROI volume
!    Note the order of the dimensions in this array!
!
!> @date 11/05/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDreadorpcdefHDF(enl,ipar,fpar,orpcdef)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDreadorpcdefHDF

use NameListTypedefs
use io
use error
use files
use quaternions
use rotations
use HDF5
use HDFsupport
use EBSDmod

IMPLICIT NONE

type(EBSDdefectNameListType),INTENT(INOUT)  :: enl
!f2py intent(in,out) ::  enl
integer(kind=irg),INTENT(INOUT)             :: ipar(3)
!f2py intent(in,out) ::  ipar
real(kind=dbl),INTENT(INOUT)                :: fpar(4)
!f2py intent(in,out) ::  fpar 
type(EBSDAnglePCDefType),INTENT(INOUT)      :: orpcdef
!f2py intent(in,out) ::  orpcdef

integer(kind=irg)                           :: io_int(1), i, hdferr, k, j

real(kind=sgl),parameter                    :: dtor = 0.0174533  ! convert from degrees to radians
integer(kind=irg)                           :: istat
character(fnlen)                            :: deformationfile, groupname, dataset
logical                                     :: g_exists 

real(kind=dbl),allocatable                  :: pcxy(:,:), eu(:,:,:,:)
integer(HSIZE_T)                            :: dims1(1), dims2(2), dims3(3), dims4(4)

type(HDFobjectStackType)                    :: HDF_head

nullify(HDF_head%next)

deformationfile = trim(EMsoft_getEMdatapathname())//trim(enl%deformationfile)
deformationfile = EMsoft_toNativePath(deformationfile)

hdferr =  HDF_openFile(deformationfile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')

! write the EMheader to the file
groupname = 'DeformationFieldInfo'
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup NMLfiles')

! read the single integer parameters
ipar = 0
dataset = 'npix'  ! ipar(1)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ipar(1))
end if

dataset = 'npiy'  ! ipar(2)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ipar(2))
end if

dataset = 'npiz'  ! ipar(3)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ipar(3))
end if

! read single floats
fpar = 0.0
dataset = 'L'  ! fpar(1)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, fpar(1))
end if

dataset = 'stepx'  ! fpar(2)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, fpar(2))
end if

dataset = 'stepy'  ! fpar(3)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, fpar(3))
end if

dataset = 'stepz'  ! fpar(4)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, fpar(4))
end if

! read Euler angle triplet and convert to quaternion
! dataset = 'eu'
! call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
! if (g_exists.eqv..TRUE.) then
!     call HDF_readDatasetDoubleArray2D(dataset, dims2, HDF_head, hdferr, eu)
!     allocate(orpcdef%quatang(4,1),stat=istat)
!     eu(1,1) = eu(1,1) + 90.D0
!     orpcdef%quatang(1:4,1) = eu2qu(sngl(eu(1,1:3))*dtor)
! end if
dataset = 'eu'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDoubleArray4D(dataset, dims4, HDF_head, hdferr, eu)
    allocate(orpcdef%quatangfield(4,dims4(2),dims4(3),dims4(4)),stat=istat)
    do k=1,dims4(2)
        do i=1,dims4(3)
            do j=1,dims4(4)
                eu(1,k,i,j) = eu(1,k,i,j) + 90.D0
                orpcdef%quatangfield(1:4,k,i,j) = eu2qu(sngl(eu(1:3,k,i,j))*dtor)
             end do
        end do
    end do
end if


! read the pattern center data 
dataset = 'pcx'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDoubleArray2D(dataset, dims2, HDF_head, hdferr, pcxy)
    allocate( orpcdef%pcfield(2,dims2(1),dims2(2)) )
    orpcdef%pcfield(1,:,:) = pcxy
    deallocate(pcxy)
end if

dataset = 'pcy'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDoubleArray2D(dataset, dims2, HDF_head, hdferr, pcxy)
    orpcdef%pcfield(2,:,:) = pcxy
    deallocate(pcxy)
end if

! and finally, read the deformation field dataset
dataset = 'deftensor'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists.eqv..TRUE.) then
    call HDF_readDatasetDoubleArray4D(dataset, dims4, HDF_head, hdferr, orpcdef%deftensorfield)
end if

! close the group and file
call HDF_pop(HDF_head,.TRUE.)

call Message('')
call Message(' -> completed reading deformation field info from file '//trim(deformationfile))
call Message('')

end subroutine EBSDreadorpcdefHDF

end module EBSDdefectHDFmod
