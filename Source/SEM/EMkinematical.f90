! ###################################################################
! Copyright (c) 2015-2020, Marc De Graef Research Group/Carnegie Mellon University
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

program EMkinematical

use local
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use json_module
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(kinematicalNameListType)           :: knl
integer(kind=irg)                       :: res, error_cnt

nmldeffile = 'EMkinematical.nml'
progname = 'EMkinematical.f90'
progdesc = 'Generate kinematical master patterns for use in EBSD/ECP visualization'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 27 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  !call JSONreadkinematicalNameList(knl, nmldeffile, error_cnt)
  write (*,*) 'json input not yet implemented'
  STOP
else
  call GetkinematicalNameList(nmldeffile,knl)
end if

! call the main routine 
if (knl%mode.eq.'lines') call KinematicalLines(knl, progname, nmldeffile)

if (knl%mode.eq.'bands') call KinematicalBands(knl, progname, nmldeffile)

end program EMkinematical

!--------------------------------------------------------------------------
!
! SUBROUTINE:KinematicalLines
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate an outputfile that contains kinematical information for a region of reciprocal space
!
!> @param knl reflector name list
!> @param nmlfile namelist file name
!
!> @date 05/31/16  MDG 1.0 original
!> @date 11/20/16  MDG 1.1 added kinematical master pattern output
!--------------------------------------------------------------------------
subroutine KinematicalLines(knl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use crystal
use diffraction
use constants
use symmetry
use error
use io
use rotations
use quaternions
use files
use timing
! use diffraction, only:CalcWaveLength   [ifort compiler warning; already visible from other USE statement MDG 11/14/16]
use Lambert
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use stringconstants

IMPLICIT NONE

type(kinematicalNameListType),INTENT(INOUT)  :: knl
character(fnlen),INTENT(IN)                  :: progname
character(fnlen),INTENT(IN)                  :: nmldeffile

character(fnlen)                             :: datafile, groupname, dataset, xtalname
logical                                      :: f_exists, readonly, verbose
integer(kind=irg)                            :: hdferr, nlines, i, istat, ix, iy, nx
integer(HSIZE_T)                             :: dims3(3), dims4(4)
real(kind=dbl)                               :: EkeV
real(kind=sgl)                               :: m

integer(kind=irg)                            :: imh, imk, iml, ii, j, num, nums, mhkl, numphi, ierr
integer(kind=irg),allocatable                :: gvec(:,:)
integer(kind=irg)                            :: h,k,l,totfam,ind(3),icnt, oi_int(1), itmp(48,3), g1(3), g2(3)
logical                                      :: first
real(kind=sgl)                               :: g(3), thr, dphi, gc(3), gax(3), gz(3), v(3), qu(4), ax(4), x, gg, sgn, xy(2), xyz(3)
real(kind=sgl),allocatable                   :: Vgg(:),th(:), unitvec(:,:), scaledVgg(:)
character(1)                                 :: space
real(kind=sgl)                               :: dhkl, Radius
real(kind=sgl)                               :: ixy(2),scl
real(kind=sgl)                               :: dx,dy,dxm,dym, sa, ca
integer(kind=irg)                            :: jj,kk
integer(kind=irg)                            :: nix,niy,nixp,niyp
logical,allocatable                          :: keep(:)
character(11)                                :: dstr
character(15)                                :: tstrb
character(15)                                :: tstre

integer(kind=irg),allocatable                :: acc_e(:,:,:)
real(kind=sgl),allocatable                   :: Eweights(:)
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)


! arrays for master pattern and stereographic projections
real(kind=sgl),allocatable                   :: masterNH(:,:), masterSH(:,:), stereoNH(:,:), stereoSH(:,:), &
                                                phi(:), cp(:), sp(:), dc(:,:), mLPNH(:,:,:), mLPSH(:,:,:)

type(unitcell)          :: cell
type(DynType),save      :: Dyn
type(gnode),save        :: rlp
type(HDFobjectStackType)          :: HDF_head

interface
  subroutine AntiAlias(master,ixy,nix,niy,nx,inten)

  use local
  
  IMPLICIT NONE

  integer(kind=irg),INTENT(IN)    :: nx
  real(kind=sgl),INTENT(INOUT)    :: master(-nx:nx,-nx:nx)
  real(kind=sgl),INTENT(IN)       :: ixy(2)
  integer(kind=irg),INTENT(IN)    :: nix
  integer(kind=irg),INTENT(IN)    :: niy
  real(kind=sgl),INTENT(IN)       :: inten
  end subroutine AntiAlias

end interface



verbose = .FALSE.

call timestamp(datestring=dstr, timestring=tstrb)

nullify(HDF_head%next)

space = 'r'

! initialize the crystal structure and compute a list of potential reflectors 
!nullify(cell)        
!allocate(cell)        

! get the crystal structure from the *.xtal file
verbose = .TRUE.
call Initialize_Cell(cell,Dyn,rlp,knl%xtalname,knl%dmin, knl%voltage, verbose)

! generate a list of hkl indices 

! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.knl%dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.knl%dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.knl%dmin) EXIT
   iml = iml + 1
 end do

!write (*,*) 'reflection range for this program : ',imh, imk, iml

! allocate all arrays
 ii = (2*imh+1)*(2*imk+1)*(2*iml+1)
 allocate(gvec(3,ii))
 allocate(Vgg(ii))
 allocate(unitvec(3,ii))
 allocate(th(ii))

! loop through all (hkl) values
 first = .TRUE.
 icnt = 1
 do h=-imh,imh
  ind(1)=-h
  do k=-imk,imk
   ind(2)=-k
   do l=-iml,iml
    ind(3)=-l

! compute the Fourier coefficient
     call CalcUcg(cell,rlp,ind)

! ignore the reciprocal lattice point if Vgg is small
     if (rlp%Vmod.ge.knl%thr) then 

! store the Fourier coefficient of the lattice potential
      if (abs(h)+abs(k)+abs(l).eq.0) then
        Vgg(icnt) = 0.0
      else
        Vgg(icnt)=rlp%Vmod**2
      end if
! g-vector (Miller indices)
      gvec(1:3,icnt) = (/ h, k, l /)
! unit vector in cartesian basis
      g(1:3)=float( (/ h, k, l /) )
      gg=CalcLength(cell,g,'r')
      call TransSpace(cell,g,gc,'r','c')
      call NormVec(cell,gc,'c')
      unitvec(1:3,icnt) = gc(1:3) 
! and Bragg angle
      th(icnt)=asin(0.5*cell%mLambda*gg)

! increment counter
      icnt=icnt+1
    end if
   end do
  end do
 end do

 icnt=icnt-1
 oi_int(1)=icnt
 call WriteValue(' Total number of entries found = ', oi_int, 1, "(I6)")

 allocate(scaledVgg(icnt))
do i=1,icnt
  if (Vgg(i).ne.0.0) scaledVgg(i) = (Vgg(i))**0.25
end do
!scaledVgg = Vgg/maxval(Vgg)


! sort these points by increasing Bragg angle


! next, we need to generate a master pattern that has a line with Gaussian inverted
! profile for each of the Kossel lines.  We'll use the standard square Lambert projection
! to create this pattern, and then convert it also to stereographic projections.
nx = 500
allocate(masterNH(-nx:nx,-nx:nx),stat=istat)
allocate(masterSH(-nx:nx,-nx:nx),stat=istat)
masterNH = 1.0
masterSH = 1.0


numphi = 360*16
allocate(phi(numphi), cp(numphi), sp(numphi))
allocate( dc(3,numphi) )
dphi = 2.0*cPi/dble(numphi)

phi = (/ (float(i-1)*dphi,i=1,numphi) /)
cp = cos(phi)
sp = sin(phi)
gz = (/ 0.0, 0.0, 1.0 /)
scl = float(nx)

do kk=1,icnt   ! ignore the last point
  k = kk ! ksort(kk)   ! pick them in the correct order

! get the unrotated direction cosines of the sampling points on the sphere; this generate a circle on the sphere (Kossel cone trace)
  ca = cos( th(k) )
  sa = sin( th(k) )
  v = unitvec(1:3,k) 
  sgn = 1.0
  x = CalcDot(cell,gz,v,'c')
  if (x.lt.0.0) sgn = -1.0
  do i=1,numphi
      dc(1,i) = ca * cp(i)
      dc(2,i) = ca * sp(i)
      dc(3,i) = sa 
  end do

! then determine the rotation quaternion to bring the z axis onto the g direction (cartesian)
  if (x.ne.1.0) then   ! the cross product exists
    call CalcCross(cell,v,gz,gax,'c','c',0) ! gax is the rotation axis
    call NormVec(cell,gax,'c')
    x = acos(x)
    if (x.lt.0.0) then
      ax = (/-gax(1),-gax(2),-gax(3), -x /)
    else
      ax = (/ gax(1), gax(2), gax(3), x /)
    end if
    qu = conjg(ax2qu(ax))
    do i=1,numphi
      v(:) = dc(:,i)
      v = quat_LP(qu,v)
      call NormVec(cell,v,'c')
      dc(:,i) = v(:)
    end do
  end if

! ok, so we have rotated this set of directions; next we need to add some intensity to each 
! corresponding point in the master pattern
  do i=1,numphi
! convert these direction cosines to coordinates in the Rosca-Lambert projection
    v(:) = dc(:,i)
    ixy = scl * LambertSphereToSquare( v, istat )
    if (istat .ne. 0) stop 'Something went wrong during interpolation...'
    nix = int(nx+ixy(1))-nx
    niy = int(nx+ixy(2))-nx
    if (dc(3,i) .ge. 0.0) then
      call AntiAlias(masterNH,ixy,nix,niy,nx,scaledVgg(k))
     ! masterNH(nix,niy) = masterNH(nix,niy) - scaledVgg(k)
    else
      call AntiAlias(masterSH,ixy,nix,niy,nx,scaledVgg(k))
     ! masterSH(nix,niy) = masterNH(nix,niy) - scaledVgg(k)
    end if
  end do
end do

! make sure the master pattern intensities are positive
x = minval(masterNH)
ca = minval(masterSH)
masterNH = masterNH - minval( (/x, ca/) )
masterNH = masterNH/maxval(masterNH)
masterSH = masterSH - minval( (/x, ca/) )
masterSH = masterSH/maxval(masterSH)


allocate(stereoNH(-nx:nx,-nx:nx),stat=istat)
allocate(stereoSH(-nx:nx,-nx:nx),stat=istat)
! get stereographic projections
  Radius = 1.0
  do i=-nx,nx 
    do j=-nx,nx 
      xy = (/ float(i), float(j) /) / float(nx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      xyz = xyz/vecnorm(xyz)
      if (ierr.ne.0) then 
        stereoNH(i,j) = 0.0
        stereoSH(i,j) = 0.0
      else
        stereoNH(i,j) = InterpolateLambert(xyz, masterNH, nx)
        stereoSH(i,j) = InterpolateLambert(xyz, masterNH, nx)
      end if
    end do
  end do


! prepare the output

call timestamp(datestring=dstr, timestring=tstre)

!------------------------------
! write the output to an HDF5 file
!------------------------------
call Message('opening '//trim(knl%datafile), frm = "(A)" )

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! open the output file
datafile = trim(EMsoft_getEMdatapathname())//trim(knl%datafile)
datafile = EMsoft_toNativePath(datafile)

! open the file using the default properties.
hdferr =  HDF_createFile(datafile, HDF_head)

! write the EMheader to the file
groupname = SC_EMkinematical
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EMkinematical
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)

call HDFwritekinematicalNameList(HDF_head, knl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

groupname = SC_EMkinematical
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_numg
hdferr = HDF_writeDatasetInteger(dataset, icnt, HDF_head)

dataset = SC_hklmax
hdferr = HDF_writeDatasetIntegerArray1D(dataset, (/ imh, imk, iml /), 3, HDF_head)

dataset = SC_hklarray
hdferr = HDF_writeDatasetIntegerArray2D(dataset, gvec(1:icnt,1:3), icnt, 3, HDF_head)

dataset = SC_modFsquared
hdferr = HDF_writeDatasetFloatArray1D(dataset, Vgg(1:icnt), icnt, HDF_head)

dataset = SC_unitGvectors
hdferr = HDF_writeDatasetFloatArray2D(dataset, unitvec(1:icnt, 1:3), icnt, 3, HDF_head)

dataset = SC_BraggAngle
hdferr = HDF_writeDatasetFloatArray1D(dataset, th(1:icnt), icnt, HDF_head)

dataset = SC_masterNH
hdferr = HDF_writeDatasetFloatArray2D(dataset, masterNH, 2*nx+1, 2*nx+1, HDF_head)

dataset = SC_masterSH
hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSH, 2*nx+1, 2*nx+1, HDF_head)

dataset = SC_stereoNH
hdferr = HDF_writeDatasetFloatArray2D(dataset, stereoNH, 2*nx+1, 2*nx+1, HDF_head)

dataset = SC_stereoSH
hdferr = HDF_writeDatasetFloatArray2D(dataset, stereoSH, 2*nx+1, 2*nx+1, HDF_head)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

write(*,*) ' --> output file closed'


end subroutine KinematicalLines


! put intensities in array using simple anti-aliasing solution to reduce jaggies
subroutine AntiAlias(master,ixy,nix,niy,nx,inten)

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nx
real(kind=sgl),INTENT(INOUT)    :: master(-nx:nx,-nx:nx)
real(kind=sgl),INTENT(IN)       :: ixy(2)
integer(kind=irg),INTENT(IN)    :: nix
integer(kind=irg),INTENT(IN)    :: niy
real(kind=sgl),INTENT(IN)       :: inten

real(kind=sgl)                  :: dx, dy, d

! look for the nearest pixel and share the intensity with that pixel
! only for the interior pixels of the pattern; to make things faster,
! we'll use the simple Manhattan distance w.r.t. nearest neighbors and
! apply the lever rule to set the intensities

if ((abs(nix).lt.nx).and.(abs(niy).lt.nx)) then 
  dx = ixy(1)-nix
  dy = ixy(2)-niy
  if (abs(dx).gt.abs(dy)) then
    d = abs(dx)
    if (dx.lt.0.0) then  
      master(nix-1,niy) = master(nix-1,niy) - d * inten
      master(nix,niy) = master(nix,niy) - (1.0-d) * inten
    else
      master(nix,niy) = master(nix,niy) - (1.0-d) * inten
      master(nix+1,niy) = master(nix+1,niy) - d * inten
    end if
  else
    d = abs(dy)
    if (dy.lt.0.0) then  
      master(nix,niy-1) = master(nix,niy-1) - d * inten
      master(nix,niy) = master(nix,niy) - (1.0-d) * inten
    else
      master(nix,niy) = master(nix,niy) - (1.0-d) * inten
      master(nix,niy+1) = master(nix,niy+1) - d * inten
    end if
  end if
end if

end subroutine AntiAlias


!--------------------------------------------------------------------------
!
! SUBROUTINE:KinematicalBands
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw Kikuchi master pattern with kinematical bands
!
!> @param knl reflector name list
!> @param nmlfile namelist file name
!
!> @date 03/01/20  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine KinematicalBands(knl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use crystal
use diffraction
use constants
use symmetry
use error
use io
use rotations
use quaternions
use files
use timing
! use diffraction, only:CalcWaveLength   [ifort compiler warning; already visible from other USE statement MDG 11/14/16]
use Lambert
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use stringconstants

IMPLICIT NONE 


type(kinematicalNameListType),INTENT(INOUT)  :: knl
character(fnlen),INTENT(IN)                  :: progname
character(fnlen),INTENT(IN)                  :: nmldeffile

character(fnlen)                             :: listfile, masterfile, groupname, dataset, xtalname, outputfile, infile, datafile
logical                                      :: f_exists, readonly, verbose
integer(kind=irg)                            :: hdferr, nlines, i, istat, ix, iy, nx, io_int(1), nkeep, ierr
integer(HSIZE_T)                             :: dims3(3), dims4(3)
real(kind=dbl)                               :: EkeV
real(kind=sgl)                               :: m

integer(kind=irg)                            :: imh, imk, iml, ii, j, num, nums, mhkl, valpos, numphi,numtheta,iequiv(3,48),nequiv
integer(kind=irg),allocatable                :: family(:,:,:),numfam(:),idx(:), idx2(:), sfi(:)
integer(kind=irg)                            :: h,k,l,totfam,ind(3),icnt, oi_int(1), itmp(48,3), g1(3), g2(3), NUMTHREADS, TID
logical                                      :: first
logical,allocatable                          :: z(:,:,:)
real(kind=sgl)                               :: g(3), thr, dphi, gc(3), gax(3), gz(3), v(3), qu(4), ax(4), x, val1, val2, &
                                                ang, valmax, edge, dc(3), Radius, xy(2), xyz(3)
real(kind=sgl),allocatable                   :: Vgg(:),ddg(:),gg(:),th(:), gcart(:,:,:), gcrys(:,:), cp(:), sp(:), ca(:), sa(:), &
                                                phi(:), theta(:), Vg(:), VgX(:), VggX(:), cosnorm(:)
character(1)                                 :: space
real(kind=sgl)                               :: dhkl, incrad, glen, sd
real(kind=sgl)                               :: ixy(2),scl, cp2, dp
real(kind=sgl)                               :: dx,dy,dxm,dym
integer(kind=irg)                            :: jj,kk
integer(kind=irg)                            :: nix,niy,nixp,niyp
logical,allocatable                          :: keep(:)
character(11)                                :: dstr
character(15)                                :: tstrb
character(15)                                :: tstre
integer(kind=irg),allocatable                :: acc_e(:,:,:)
real(kind=sgl),allocatable                   :: Eweights(:)
real(kind=sgl),allocatable                   :: srtmp(:,:,:,:), mLPNH(:,:,:), mLPSH(:,:,:), masterNH(:,:), masterSH(:,:), &
                                                KBI(:), kinmasterNH(:,:), kinmasterSH(:,:), kinNH(:,:), kinSH(:,:), &
                                                stereoNH(:,:), stereoSH(:,:)
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)

type(unitcell)          :: cell
type(DynType),save      :: Dyn
type(gnode),save        :: rlp
type(HDFobjectStackType)          :: HDF_head

verbose = .FALSE.

nullify(HDF_head%next)

thr = 1.E-5
space = 'r'

! get the crystal structure from the *.xtal file
verbose = .TRUE.
call Initialize_Cell(cell,Dyn,rlp,knl%xtalname,knl%dmin, sngl(knl%voltage), verbose)

! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.knl%dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.knl%dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.knl%dmin) EXIT
   iml = iml + 1
 end do

! allocate all arrays
 allocate(z(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml))
 ii = (2*imh+1)*(2*imk+1)*(2*iml+1)
 allocate(family(ii,48,3))
 allocate(numfam(ii))
 allocate(Vgg(ii))
 allocate(VggX(ii))
 allocate(ddg(ii))
 allocate(gg(ii))
 allocate(th(ii))
! determine the families of reflections with (hkl)<=(imh,imk,iml)
! first initialize the boolean array z
 z = .FALSE.
! then loop through all (hkl) values
 first = .TRUE.
 icnt = 1
 totfam=0
 rlp%method= 'DT'   ! we're computing simple Doyle-Turner or Smith-Burge scattering factors to get the list of reflectors
 do h=-imh,imh
  ind(1)=-h
  do k=-imk,imk
   ind(2)=-k
   do l=-iml,iml
    ind(3)=-l

! make sure we have not already done this one in another family
    if (.not.z(-h,-k,-l)) then

! if it is a new one, then determine the entire family
     call CalcUcg(cell,rlp,ind)

! but ignore the reciprocal lattice point if Vgg is small
     if (abs(rlp%Ucg).ge.thr) then 
      Vgg(icnt) = (abs(rlp%Ucg))**2

! copy family in array and label all its members and multiples in z-array
      call CalcFamily(cell,ind,num,space,itmp)
      do i=1,num
        family(icnt,i,1:3)=itmp(i,1:3)
        z(itmp(i,1),itmp(i,2),itmp(i,3))=.TRUE.
      end do

! increment family counter
      numfam(icnt)=num
      totfam=totfam+num-1
      icnt=icnt+1
     end if
    end if
   end do
  end do
 end do

 icnt=icnt-1
 oi_int(1)=icnt
 call WriteValue(' Total number of families        = ', oi_int, 1, "(I6)")
 oi_int(1)=totfam
 call WriteValue(' Total number of family members  = ', oi_int, 1, "(I6)")

 icnt = icnt-1   ! to eliminate the origin ... 
! compute d-spacings, g-spacings, theta
 allocate(gcart(3,icnt,48))
 do k=1,icnt
  g(1:3)=float(family(k,1,1:3))
  gg(k)=CalcLength(cell,g,'r')
  th(k)=asin(0.5*cell%mLambda*gg(k))
  do j=1,numfam(k)
    g(1:3)=float(family(k,j,1:3))
    gg(k)=CalcLength(cell,g,'r')
    call TransSpace(cell,g,gc,'r','c')
    call NormVec(cell,gc,'c')
    gcart(1:3,k,j) = gc(1:3) 
!   write (*,*) family(k,j,1:3), gc(1:3)
  end do
 end do

! we're taking the dot product between the plane normals and the direction cosine
! vectors, so we will need the complement of the theta angles ... 
cp2 = sngl(cPi/2.D0)
th = cp2 - th 

! next, we scan over the entire Lambert square and, for each point, 
! determine which Kikuchi bands it belongs to; then we add all those 
! intensities together and place that value in the array. This should
! automatically take care of overlapping bands etc...

nx = 500
allocate(kinmasterNH(-nx:nx,-nx:nx), kinmasterSH(-nx:nx,-nx:nx))
kinmasterNH = 0.0 
kinmasterSH = 0.0 

edge = 1.D0 / dble(nx)

do ix=-nx,nx 
  do iy=-nx,nx 
! get the direction cosines for this point 
    dc = LambertSquareToSphere( (/ ix, iy /) * edge, ierr )

! loop over all family members 
    do k=1,icnt
      do j=1,numfam(k) 
        dp = DOT_PRODUCT( gcart(1:3,k,j), dc(1:3) )
        if (abs(dp).lt.1.E-5) then 
          kinmasterNH(ix,iy) = kinmasterNH(ix,iy) + Vgg(k)*0.5 ! to avoid double counting
        else
          ang = acos( dp )
          if  ( (ang.ge.th(k)) .and. (ang.le.cp2) )  then 
            kinmasterNH(ix,iy) = kinmasterNH(ix,iy) + Vgg(k)
          end if 
        end if 
      end do 
    end do 

      dc(3) = -dc(3)
      ! loop over all family members 
      do k=1,icnt
        do j=1,numfam(k) 
          dp = DOT_PRODUCT( gcart(1:3,k,j), dc(1:3) )
          if (abs(dp).lt.1.E-5) then 
            kinmasterNH(ix,iy) = kinmasterNH(ix,iy) + Vgg(k)*0.5 ! to avoid double counting
          else
            ang = acos( dp )
            if  ( (ang.ge.th(k)) .and. (ang.le.cp2) )  then 
              kinmasterNH(ix,iy) = kinmasterNH(ix,iy) + Vgg(k)
            end if 
          end if 
        end do 
      end do 

  end do 
end do 

! then save this in an HDF file 

call timestamp(datestring=dstr, timestring=tstre)

!------------------------------
! write the output to an HDF5 file
!------------------------------
call Message('opening '//trim(knl%datafile), frm = "(A)" )

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! open the output file
datafile = trim(EMsoft_getEMdatapathname())//trim(knl%datafile)
datafile = EMsoft_toNativePath(datafile)

! open the file using the default properties.
hdferr =  HDF_createFile(datafile, HDF_head)

! write the EMheader to the file
groupname = SC_EMkinematical
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EMkinematical
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)

call HDFwritekinematicalNameList(HDF_head, knl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

groupname = SC_EMkinematical
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_numg
hdferr = HDF_writeDatasetInteger(dataset, icnt, HDF_head)

dataset = SC_hklmax
hdferr = HDF_writeDatasetIntegerArray1D(dataset, (/ imh, imk, iml /), 3, HDF_head)

! dataset = SC_hklarray
! hdferr = HDF_writeDatasetIntegerArray2D(dataset, gvec(1:icnt,1:3), icnt, 3, HDF_head)

dataset = SC_modFsquared
hdferr = HDF_writeDatasetFloatArray1D(dataset, Vgg(1:icnt), icnt, HDF_head)

! dataset = SC_unitGvectors
! hdferr = HDF_writeDatasetFloatArray2D(dataset, unitvec(1:icnt, 1:3), icnt, 3, HDF_head)

dataset = SC_BraggAngle
hdferr = HDF_writeDatasetFloatArray1D(dataset, th(1:icnt), icnt, HDF_head)

dataset = SC_masterNH
hdferr = HDF_writeDatasetFloatArray2D(dataset, kinmasterNH, 2*nx+1, 2*nx+1, HDF_head)

dataset = SC_masterSH
hdferr = HDF_writeDatasetFloatArray2D(dataset, kinmasterSH, 2*nx+1, 2*nx+1, HDF_head)


! allocate(stereoNH(-nx:nx,-nx:nx),stat=istat)
! allocate(stereoSH(-nx:nx,-nx:nx),stat=istat)
! ! get stereographic projections
!   Radius = 1.0
!   do i=-nx,nx 
!     do j=-nx,nx 
!       xy = (/ float(i), float(j) /) / float(nx)
!       xyz = StereoGraphicInverse( xy, ierr, Radius )
!       xyz = xyz/vecnorm(xyz)
!       if (ierr.ne.0) then 
!         stereoNH(i,j) = 0.0
!         stereoSH(i,j) = 0.0
!       else
!         stereoNH(i,j) = InterpolateLambert(xyz, kinmasterNH, nx)
!         stereoSH(i,j) = InterpolateLambert(xyz, kinmasterSH, nx)
!       end if
!     end do
!   end do

! dataset = SC_stereoNH
! hdferr = HDF_writeDatasetFloatArray2D(dataset, stereoNH, 2*nx+1, 2*nx+1, HDF_head)

! dataset = SC_stereoSH
! hdferr = HDF_writeDatasetFloatArray2D(dataset, stereoSH, 2*nx+1, 2*nx+1, HDF_head)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

write(*,*) ' --> output file closed'




end subroutine KinematicalBands





