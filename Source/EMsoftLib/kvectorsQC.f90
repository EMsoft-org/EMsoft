! ###################################################################
! Copyright (c) 2017-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:kvectorsQC.f90
!--------------------------------------------------------------------------
!
! MODULE: kvectorsQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Routines to handle k vector sampling for quasi-crystals multibeam computations
!
!
!> @date 06/26/18 SS 1.0 original
!--------------------------------------------------------------------------
module kvectorsQC

use local 
use typedefs
use qcrystal
use QCmod

IMPLICIT NONE

public

interface QC_Addkvector
	module procedure TDQC_Addkvector
	module procedure QC_Addkvector
end interface QC_Addkvector

interface QC_Calckvectors
	module procedure QC_Calckvectors
	module procedure TDQC_Calckvectors
end interface QC_Calckvectors

interface QC_applySymmetry
	module procedure TDQC_applySymmetry
	module procedure QC_applyIcosahedralSymmetry
end interface QC_applySymmetry

interface GetVectorsConeCBEDQC
	module procedure GetVectorsConeCBED2DQC
	module procedure GetVectorsConeCBEDQC
end interface GetVectorsConeCBEDQC

type IncidentListCBED
        integer(kind=irg)                 :: i, j
        real(kind=dbl)                    :: k(3)
        type(IncidentListCBED),pointer    :: next
end type IncidentListCBED

contains
!--------------------------------------------------------------------------
!
! SUBROUTINE: TDQC_Calckvectors
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors for 2-D QC computations
!
!> @details This is a new version that combines several older routines.  The most important 
!> aspects of this routine are a) linked list can use regular mapping or modified Lambert mapping;
!> b) list makes use of octagonal, decagonal and dodecagonal crystal symmetry; c) routine
!> has been cleaned up, and there is now a Delete_kvectorlist function as well.
!
!> @param khead head of linked list
!> @param QCcell cell pointer
!> @param npx number of vectors along x 
!> @param npy number of vectors along y 
!> @param numk total number of wave vectors in list
!
!> @date   05/01/18 SS 1.0 original based on Calckvectors routine
!> @date   06/26/18 SS 1.1 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine TDQC_Calckvectors(khead,QCcell,npx,numk)
!DEC$ ATTRIBUTES DLLEXPORT :: TDQC_Calckvectors

use io
use error
use constants
use diffraction
use crystal

IMPLICIT NONE

type(kvectorlist),pointer               :: khead
type(TDQCStructureType),pointer         :: QCcell
integer(kind=irg),INTENT(IN)            :: npx          !< number of kvectors along x
integer(kind=irg),INTENT(OUT)           :: numk         !< total number of kvectors in linked list

integer(kind=irg)                       :: istat,i,j,istart,iend,jstart,jend 
real(kind=dbl)                          :: kstar(3), delta, xy(2), xx, yy, angle
type(kvectorlist),pointer               :: ktail, ktmp
real(kind=dbl),parameter                :: eps = 1.0D-6

delta = 1.D0/dble(npx)

! allocate the head of the linked list
allocate(khead,stat=istat)                   ! allocate new value
if (istat.ne.0) call FatalError('TDQC_Calckvectors',' unable to allocate khead pointer')
ktail => khead                               ! tail points to new value
nullify(ktail%next)                          ! nullify next in new value
numk = 1                                     ! keep track of number of k-vectors so far
ktail%hs = 1                                 ! this lies in the Northern Hemisphere
ktail%i = 0                                  ! i-index of beam
ktail%j = 0                                  ! j-index of beam
kstar = (/ 0.0, 0.0, 1.0 /)                  ! we always use c* as the center of the RoscaLambert projection
kstar = kstar/QCcell%mLambda                 ! divide by wavelength
ktail%k = kstar
ktail%kn = 1.0/QCcell%mLambda

! calculate the tangent of angle between x-axis and
! first symmetrically equivalant one
! for now we will assume point groups 822, 1022 and 1222; a more detailed 
! implementation with all possible 2D-QC space groups will be done later
!!!!![todo: implement sampling of k-vectors all 2D-QC space group]
if(trim(QCcell%QCtype) .eq. 'Oct') then
  !tanth  = tan(cPi/4.D0)
  angle   = cPi/4.D0

else if(trim(QCcell%QCtype) .eq. 'Dec') then
  !tanth  = tan(cPi/5.D0)
  angle   = cPi/5.D0

else if(trim(QCcell%QCtype) .eq. 'DoD') then
  !tanth  = tan(cPi/6.D0)
  angle   = cPi/6.D0

else
  call FatalError('TDQC_Calckvectors:','unknown 2-D quasi crystal type (allowed types: oct, dec, dod).')

end if

istart = 0
iend   = npx
jstart = 0
jend   = npx
do i=istart,iend
  do j=jstart,jend
    xx = dble(i)
    yy = dble(j)
    yy = datan2(yy,xx)
    if(yy .lt. (angle - eps)) then   ! 
      xy = (/ dble(i), dble(j) /) * delta
      call TDQC_Addkvector(ktail,QCcell,numk,xy,i,j)
    end if
  end do
end do

end subroutine TDQC_Calckvectors


!--------------------------------------------------------------------------
!
! SUBROUTINE: TDQC_Addkvector
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief add a k-vector for a 2-D quasi-crystal (used for RoscaLambert mapmode)
!
!> @param ktail current entry in linked list
!> @param QCcell cell pointer
!> @param numk total number of wave vectors in list
!> @param xy input coordinates (normalized on square or hexagonal grid)
!> @param i point index
!> @param j point index
!
!> @date   05/01/18 SS 1.0 original 
!> @date   06/26/18 SS 1.1 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine TDQC_Addkvector(ktail,QCcell,numk,xy,i,j)
!DEC$ ATTRIBUTES DLLEXPORT :: TDQC_Addkvector

use io
use typedefs
use constants
use error
use diffraction
use crystal
use Lambert
use crystal

IMPLICIT NONE

type(kvectorlist),pointer               :: ktail
type(TDQCStructureType),pointer         :: QCcell
integer(kind=irg),INTENT(INOUT)         :: numk
!f2py intent(in,out) ::  numk
real(kind=dbl),INTENT(IN)               :: xy(2)
integer(kind=irg),INTENT(IN)            :: i
integer(kind=irg),INTENT(IN)            :: j

integer(kind=irg)                       :: istat, ks, ii, ierr
real(kind=dbl)                          :: kstar(3)
logical                                 :: hex

kstar = LambertSquareToSphere(xy, ierr)

! add this vector to the linked list
allocate(ktail%next,stat=istat)                    ! allocate new value
if (istat.ne.0) call FatalError('TDQC_Addkvector:',' unable to allocate ktail pointer')
ktail => ktail%next                                ! tail points to new value
nullify(ktail%next)                                ! nullify next in new value
numk = numk + 1                                    ! keep track of number of k-vectors so far
ktail%hs = 1                                       ! which hemisphere (Northern = 1, Southern = -1)
ktail%i = i                                        ! i-index of beam
ktail%j = j                                        ! j-index of beam
kstar = kstar/Norm2(kstar)
ktail%k = kstar/QCcell%mLambda                     ! divide by wavelength
ktail%kn = 1.0/QCcell%mLambda

end subroutine TDQC_Addkvector

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_Calckvectors
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors for QC computations
!
!> @details This is a new version that combines several older routines.  The most important 
!> aspects of this routine are a) linked list can use regular mapping or modified Lambert mapping;
!> b) list makes use of icosahedral crystal symmetry; c) routine
!> has been cleaned up, and there is now a Delete_kvectorlist function as well.  This is a very 
!> complex routine so make sure you fully understand it before you attempt to modify or add anything!
!
!> @param khead head of linked list
!> @param QCcell cell pointer
!> @param npx number of vectors along x 
!> @param npy number of vectors along y 
!> @param numk total number of wave vectors in list
!
!> @date   03/16/17 MDG 1.0 original based on Calckvectors routine
!> @date   03/18/17 MDG 1.1 implementation of icosahedral symmetry sampling sector (1/120th segment) + cleanup
!> @date   06/26/18 SS  1.2 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine QC_Calckvectors(khead,QCcell,npx,npy,numk)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_Calckvectors

use io
use error
use constants
use diffraction
use crystal

IMPLICIT NONE

type(kvectorlist),pointer               :: khead
type(QCStructureType),pointer           :: QCcell
integer(kind=irg),INTENT(IN)            :: npx          !< number of kvectors along x
integer(kind=irg),INTENT(OUT)           :: npy          !< number of kvectors along y
integer(kind=irg),INTENT(OUT)           :: numk         !< total number of kvectors in linked list

integer(kind=irg)                       :: istat,i,j,istart,iend,jstart,jend 
real(kind=dbl)                          :: kstar(3), delta, xy(2), xsegment, ysegment, ratio
type(kvectorlist),pointer               :: ktail, ktmp

! npx is the number of samples to be computed along the x-axis between the center and the projection
! of the three-fold axes onto that axis...; delta is the corresponding step size along that line segment
! The segment length was computed using Mathematica
xsegment = 0.45315066777864719047D0 
ysegment = 0.36252053422291775238D0
delta =  xsegment / dble(npx)
npy = nint(ysegment/delta)+2    ! to make sure we can correctly interpolate for the complete master pattern
ratio = ysegment / xsegment

! allocate the head of the linked list
allocate(khead,stat=istat)                   ! allocate new value
if (istat.ne.0) call FatalError('QC_Calckvectors',' unable to allocate khead pointer')
ktail => khead                               ! tail points to new value
nullify(ktail%next)                          ! nullify next in new value
numk = 1                                     ! keep track of number of k-vectors so far
ktail%hs = 1                                 ! this lies in the Northern Hemisphere
ktail%i = 0                                  ! i-index of beam
ktail%j = 0                                  ! j-index of beam
kstar = (/ 0.0, 0.0, 1.0 /)                  ! we always use c* as the center of the RoscaLambert projection
kstar = kstar/QCcell%mLambda                 ! divide by wavelength
ktail%k = kstar
ktail%kn = 1.0/QCcell%mLambda

!open(unit=dataunit,file='kvectors.txt',status='unknown',form='formatted')

! we'll delineate the triangular area by cutting off points at 38.66° from horizontal, i.e., when j/i > ratio
istart = 0
iend = npx
jstart = 0
jend = npy
do i=istart,iend 
 jloop: do j=jstart,jend
   if ((i.eq.0).and.(j.eq.0)) CYCLE jloop        ! (0,0) is already taken care of
   if (dble(j-2)/dble(i).gt.ratio) CYCLE jloop   ! stay inside the triangular region with some room to spare for interpolation...
   xy = (/ dble(i), dble(j) /) * delta
   call QC_AddkVector(ktail,QCcell,numk,xy,i,j)
   !write (dataunit,"(I5,I5)") i, j
 end do jloop
end do
!close(unit=dataunit,status='keep')

! make sure the Iarray of the calling program does not run over its boundaries
npy = npy+2

end subroutine QC_Calckvectors

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetVectorsConeCBEDQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate list of incident vectors for CBED; will be replced BY
!> somthing more permanent later
!
!> @param cbednl CBEDQC namelist structure
!> @param klist IncidentListECP pointer
!> @param numk number of incident vectors in the linked list
!
!> @date 02/22/18  SS 1.0 original
!> @date   06/26/18 SS  1.2 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine GetVectorsConeCBEDQC(cbednl, klist, numk)
!DEC$ ATTRIBUTES DLLEXPORT :: GetVectorsConeCBEDQC

use local
use io
use NameListTypedefs
use error

type(EMCBEDQCNameListType),INTENT(IN)            :: cbednl
type(IncidentListCBED),pointer                   :: klist, ktmp
integer(kind=irg),INTENT(OUT)                    :: numk

real(kind=dbl)                                   :: kk(3), thetacr, delta, ktmax, cen, rad
real(kind=dbl),parameter                         :: DtoR = 0.01745329251D0
integer(kind=irg)                                :: imin, imax, jmin, jmax
integer(kind=irg)                                :: ii, jj, istat

numk      = 0
kk        = (/0.D0,0.D0,1.D0/)
thetacr   = cbednl%convergence
ktmax     = tan(thetacr/1000.0)
delta     = 2.0*ktmax/(float(cbednl%npix)-1.0)

imin      = 1
imax      = cbednl%npix
jmin      = 1
jmax      = cbednl%npix

cen       = cbednl%npix/2.0
rad       = cbednl%npix/2.0

allocate(klist,stat=istat)
if (istat .ne. 0) then
    call FatalError('GetVectorsCone','Failed to allocate klist pointer')
end if

ktmp => klist
nullify(ktmp%next)

do ii = imin, imax
    do jj = jmin, jmax
        if( (dble(ii) - cen)**2 + (dble(jj) - cen)**2 .le. rad**2) then
          ktmp%k(1:3) = (/-ktmax+delta*(ii-1),-ktmax+delta*(jj-1),0.D0/) + kk(1:3)
          ktmp%k      = ktmp%k/sqrt(sum(ktmp%k**2))
          ktmp%i      = ii
          ktmp%j      = jj
          numk        = numk + 1
          allocate(ktmp%next)
          ktmp => ktmp%next
          nullify(ktmp%next)
        end if
    end do
end do

end subroutine GetVectorsConeCBEDQC

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetVectorsConeCBED2DQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief generate list of incident vectors for CBED; will be replced BY
!> somthing more permanent later
!
!> @param cbednl CBEDQC namelist structure
!> @param klist IncidentListECP pointer
!> @param numk number of incident vectors in the linked list
!
!> @date 02/28/18  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetVectorsConeCBED2DQC(cbednl, klist, numk)
!DEC$ ATTRIBUTES DLLEXPORT :: GetVectorsConeCBED2DQC

use local
use io
use NameListTypedefs
use error

type(EMCBED2DQCNameListType),INTENT(IN)          :: cbednl
type(IncidentListCBED),pointer                   :: klist, ktmp
integer(kind=irg),INTENT(OUT)                    :: numk

real(kind=dbl)                                   :: kk(3), thetacr, delta, ktmax, cen, rad
real(kind=dbl),parameter                         :: DtoR = 0.01745329251D0
integer(kind=irg)                                :: imin, imax, jmin, jmax
integer(kind=irg)                                :: ii, jj, istat

numk      = 0
kk        = (/0.D0,0.D0,1.D0/)
thetacr   = cbednl%convergence
ktmax     = tan(thetacr/1000.0)
delta     = 2.0*ktmax/(float(cbednl%npix)-1.0)

imin      = 1
imax      = cbednl%npix
jmin      = 1
jmax      = cbednl%npix

cen       = cbednl%npix/2.0
rad       = cbednl%npix/2.0

allocate(klist,stat=istat)
if (istat .ne. 0) then
    call FatalError('GetVectorsCone','Failed to allocate klist pointer')
end if

ktmp => klist
nullify(ktmp%next)

do ii = imin, imax
    do jj = jmin, jmax
        if( (dble(ii) - cen)**2 + (dble(jj) - cen)**2 .le. rad**2) then
          ktmp%k(1:3) = (/-ktmax+delta*(ii-1),-ktmax+delta*(jj-1),0.D0/) + kk(1:3)
          ktmp%k      = ktmp%k/sqrt(sum(ktmp%k**2))
          ktmp%i      = ii
          ktmp%j      = jj
          numk        = numk + 1
          allocate(ktmp%next)
          ktmp => ktmp%next
          nullify(ktmp%next)
        end if
    end do
end do

end subroutine GetVectorsConeCBED2DQC

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_Addkvector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a k-vector for a quasi-crystal (used for RoscaLambert mapmode)
!
!> @param ktail current entry in linked list
!> @param QCcell cell pointer
!> @param numk total number of wave vectors in list
!> @param xy input coordinates (normalized on square or hexagonal grid)
!> @param i point index
!> @param j point index
!
!> @date   11/21/12 MDG 1.0 original 
!> @date   04/29/13 MDG 1.1 modified for kvectors module
!> @date   06/09/14 MDG 2.0 added ktail as argument
!> @date   08/25/15 MDG 2.1 added addSH as optional argument
!> @date   08/27/15 MDG 2.2 added flip for special case of rhombohedral sampling
!> @date   08/28/15 MDG 2.3 mappings replaced with calls to Lambert module (significant simplification of code)
!> @date   08/31/15 MDG 2.4 replaced integer coordinates by actual scaled coordinates
!> @date   06/26/18 SS  2.5 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine QC_Addkvector(ktail,QCcell,numk,xy,i,j)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_Addkvector

use io
use typedefs
use constants
use error
use diffraction
use crystal
use Lambert
use crystal

IMPLICIT NONE

type(kvectorlist),pointer               :: ktail
type(QCStructureType),pointer           :: QCcell
integer(kind=irg),INTENT(INOUT)         :: numk
!f2py intent(in,out) ::  numk
real(kind=dbl),INTENT(IN)               :: xy(2)
integer(kind=irg),INTENT(IN)            :: i
integer(kind=irg),INTENT(IN)            :: j

integer(kind=irg)                       :: istat, ks, ii, ierr
real(kind=dbl)                          :: kstar(3)
logical                                 :: hex

kstar = LambertSquareToSphere(xy, ierr)

! add this vector to the linked list
allocate(ktail%next,stat=istat)                    ! allocate new value
if (istat.ne.0) call FatalError('QC_Addkvector:',' unable to allocate ktail pointer')
ktail => ktail%next                                ! tail points to new value
nullify(ktail%next)                                ! nullify next in new value
numk = numk + 1                                    ! keep track of number of k-vectors so far
ktail%hs = 1                                       ! which hemisphere (Northern = 1, Southern = -1)
ktail%i = i                                        ! i-index of beam
ktail%j = j                                        ! j-index of beam
kstar = kstar/Norm2(kstar)
ktail%k = kstar/QCcell%mLambda                     ! divide by wavelength
ktail%kn = 1.0/QCcell%mLambda

end subroutine QC_Addkvector

!--------------------------------------------------------------------------
!
! SUBROUTINE: TDQC_applySymmetry
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief apply 2-D QC rotational symmetry to the Iarray to generate Iarrayout
!
!> @date   05/03/18 SS 1.0 original
!> @date   06/26/18 SS 1.1 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine TDQC_applySymmetry(dict, npx, numset, Iarrayout, Iarray, QCtype)
!DEC$ ATTRIBUTES DLLEXPORT :: TDQC_applySymmetry

use Lambert
use dictmod
use quaternions
use error
use constants

IMPLICIT NONE

type(dicttype),pointer            :: dict
integer(kind=irg),INTENT(IN)      :: npx
integer(kind=irg),INTENT(IN)      :: numset
real(kind=sgl),INTENT(INOUT)      :: Iarrayout(-npx:npx,-npx:npx,1:numset)
!f2py intent(in,out) ::  Iarrayout
real(kind=sgl),INTENT(IN)         :: Iarray(-npx:npx,-npx:npx,1:numset)
character(fnlen),INTENT(IN)       :: QCtype

integer(kind=irg)                 :: ii, jj, kk, ierr, num, nx, ny, nequiv, equivxy(24,2)
real(kind=dbl)                    :: xy(2), xyz(3), xyzr(3), delta, &
                                     dx, dy, dxm, dym, q(4), tanth, angle, xx, yy
real(kind=dbl),parameter          :: eps = 1.0D-6

delta = 1.D0/dble(npx)

if(dict%pgnum .eq. 34) then
  !tanth = tan(cPi/4.D0)
  angle = cPi/4.D0
else if(dict%pgnum .eq. 35) then
  !tanth = tan(cPi/5.D0)
  angle = cPi/5.D0
else if(dict%pgnum .eq. 36) then
  !tanth = tan(cPi/6.D0)
  angle = cPi/6.D0
else
  call FatalError('TDQC_applySymmetry','unknown 2D-quasi crystal type.')
end if

Iarrayout = 0.0

do ii = 0, npx
  do jj = 0, npx

    xx = dble(ii)
    yy = dble(jj)
    yy = datan2(yy,xx)

    if(yy .lt. (angle - eps)) then   ! 
      xy = (/ dble(ii), dble(jj) /) * delta

      call TDQC_calcequiv(dict, npx, xy, equivxy, nequiv)
      
      do kk = 1,nequiv
        nx  = equivxy(kk,1)
        ny  = equivxy(kk,2) 
        Iarrayout(nx,ny,1:numset) = Iarray(ii,jj,1:numset)
      end do

    end if
  end do
end do

end subroutine TDQC_applySymmetry

!--------------------------------------------------------------------------
!
! SUBROUTINE: TDQC_calcequiv
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate equivalent directions for 2-D quasi crystal symmetries
!
!> @date   05/03/18 SS 1.0 original
!> @date   06/26/18 SS 1.1 moved to kvectorsQC module

!--------------------------------------------------------------------------
recursive subroutine TDQC_calcequiv(dict, npx, xy, equivxy, nequiv)
!$DEC ATTRIBUTES DLLEXPORT :: TDQC_calcequiv

use dictmod
use local
use error
use Lambert
use quaternions 

IMPLICIT NONE

type(dicttype), pointer                 :: dict           ! dictionary type
integer(kind=irg),INTENT(IN)            :: npx            ! size of Lambert projection
real(kind=dbl),INTENT(IN)               :: xy(2)          ! input coordinates of point
integer(kind=irg),INTENT(OUT)           :: equivxy(24,2)  ! set of equivalent points; size is maximum order of 2-D QC
integer(kind=irg),INTENT(OUT)           :: nequiv         ! number of equivalent points

integer(kind=irg)                       :: ii, jj, kk, nsym, ierr
real(kind=dbl)                          :: xyz(3), xyzp(3), xyp(2)

nsym    = dict%Nqsym
nequiv  = 0
do ii = 1,nsym

  xyz             = LambertSquareToSphere(xy, ierr)
  if(ierr .ne. 0) then
    xyz = xyz/NORM2(xyz)
    if(xyz(3) .lt. 0.D0) xyz = -xyz
  end if
  xyzp            = quat_LP(dict%Pm(1:4,ii),xyz)
  if(xyzp(3) .lt. 0.D0) xyzp = -xyzp
  xyzp            = xyzp/NORM2(xyzp)
  xyp             = LambertSphereToSquare(xyzp, ierr)
  equivxy(ii,1:2) = (/nint(xyp(1)*dble(npx)), nint(xyp(2)*dble(npx))/)

end do
nequiv = nsym

end subroutine TDQC_calcequiv

!--------------------------------------------------------------------------
!
! FUNCTION: QC_ApplyInversionSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Apply the inversion group symmetry to a pair of coordinates on a Lambert grid
!
!> @param ipx x-coordinate
!> @param ipy y-coordinate
!> @param ipz y-coordinate (hemisphere)
!> @param npx number of points for Lambert projection
!> @param iequiv array with equivalent coordinates
!> @param nequiv number of equivalent coordinates
!> @param stereographic [optional] used to get the output in stereographic coordinates
!> @param cubictype [optional] to force use of hardcoded symmetry operations for cubic groups
!
!> @todo implement full symmetry handling; this may need some more verification tests
! 
!> @date  09/01/15 MDG 1.0 original
!> @date   06/26/18 SS 1.1 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine QC_ApplyInversionSymmetry(ipx,ipy,ipz,npx,iequiv,nequiv)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_ApplyInversionSymmetry

use local
use Lambert
use typedefs
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: ipx
integer(kind=irg),INTENT(IN)    :: ipy
integer(kind=irg),INTENT(IN)    :: ipz
integer(kind=irg),INTENT(IN)    :: npx
integer(kind=irg),INTENT(OUT)   :: iequiv(3,48)
integer(kind=irg),INTENT(OUT)   :: nequiv

real(kind=dbl)                  :: xy(2), xyz(3), kstar(3)
real(kind=dbl),parameter        :: neps = -0.0001D0
integer(kind=irg)               :: ierr, i, ix, iy
real(kind=dbl)                  :: stmp(48,3)           !< output array with equivalent vectors
integer(kind=irg)               :: n                    !< number of entries in equivalent vector array
character(1)                    :: space                !< 'd' or 'r'

xy = (/ dble(ipx), dble(ipy) /) / dble(npx)
xyz = LambertSquaretoSphere(xy,ierr)
if (ipz.lt.0) xyz(3) = -xyz(3)

xy = LambertSphereToSquare(xyz, ierr)
xy = xy * dble(npx)
iequiv(1,1) = nint(xy(1))
iequiv(2,1) = nint(xy(2))
iequiv(3,1) = 1

xy = LambertSphereToSquare(-xyz, ierr)
xy = xy * dble(npx)
iequiv(1,2) = nint(xy(1))
iequiv(2,2) = nint(xy(2))
iequiv(3,2) = -1

nequiv = n

end subroutine QC_ApplyInversionSymmetry

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_applyIcosahedralSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief apply icosahedral rotational symmetry to the Iarray to generate mLPNH
!
!> @param npx semi-size of mLPNH array
!> @param nsamples x-dimension of Iarray
!> @param npy semi y-dimension of Iarray
!> @param numset number of atoms in asymmetric unit
!> @param mLPNH odified Lambert Projection Northern Hemisphere
!> @param Iarray symmetrized icosahedral unit triangle
!
!> @date   03/18/17 MDG 1.0 original
!> @date   06/26/18 SS  1.1 moved to kvectorsQC module
!--------------------------------------------------------------------------
recursive subroutine QC_applyIcosahedralSymmetry(npx, nsamples, npy, numset, mLPNH, mLPSH, Iarray)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_applyIcosahedralSymmetry

use Lambert
use dictmod
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)      :: npx
integer(kind=irg),INTENT(IN)      :: nsamples
integer(kind=irg),INTENT(IN)      :: npy
integer(kind=irg),INTENT(IN)      :: numset
real(kind=sgl),INTENT(INOUT)      :: mLPNH(-npx:npx,-npx:npx,1:numset)
!f2py intent(in,out) ::  mLPNH
real(kind=sgl),INTENT(INOUT)      :: mLPSH(-npx:npx,-npx:npx,1:numset)
!f2py intent(in,out) ::  mLPSH
real(kind=sgl),INTENT(IN)         :: Iarray(0:nsamples-1,-npy:npy,1:numset)

type(dicttype),pointer            :: dict
integer(kind=irg)                 :: i, j, k, ierr, num, nix, nixp, niy, niyp
real(kind=dbl)                    :: xy(2), xyz(3), xyzr(3), delta, xsegment, ysegment, ratio, r, &
                                     dx, dy, dxm, dym, dparray(60,60), q(4), eps


eps = 0.00001

! npx is the number of samples to be computed along the x-axis between the center and the projection
! of the three-fold axes onto that axis...; delta is the corresponding step size along that line segment
! The segment length was computed using Mathematica; these are square Lambert projection coordinates
xsegment = 0.45315066777864719047D0 
ysegment = 0.36252053422291775238D0 
delta = xsegment / dble(nsamples-2)
ratio = ysegment / xsegment

! the symmetry operators have all been defined in the dictmod module, so we only need to
! initialize them here...
allocate(dict)
dict%Num_of_init = 1
dict%Num_of_iterations = 1
dict%pgnum = 33
call DI_Init(dict,'VMF')
num = dict%Nqsym

! then we loop over all the points in the mLPNH array, compute the (x,y,z) vector on the sphere,
! compute all equivalents until we find the one that falls inside the stereographic triangle
! and then we perform bilinear interpolation to get the intensity...

do i=-npx,npx
  do j=-npx,npx
    xy = (/ dble(i), dble(j) /) / dble(npx)
    xyz = LambertSquaretoSphere(xy,ierr)
    xyz = xyz/norm2(xyz)
! find the point that lies inside the sampling triangle
    kloop: do k=1,num
      xyzr = quat_Lp(dict%Pm(1:4,k), xyz)
      xyzr = xyzr/norm2(xyzr)
      xy = LambertSpheretoSquare(xyzr,ierr) 
      if (xyzr(3).ge.0.0) then
        if ((abs(xy(1)).le.xsegment).and.(xy(1).ge.0)) then ! x value is in correct range
          if ((abs(xy(2)).le.xy(1)*ratio+eps)) then
            EXIT kloop
          end if
        end if
      end if
    end do kloop

    xy = xy / delta

! four-point interpolation (bi-quadratic)
    nix = int(xy(1))
    niy = int(xy(2))
    nixp = nix+1
    niyp = niy+1
    dx = xy(1)-nix
    dy = xy(2)-niy
    dxm = 1.0-dx
    dym = 1.0-dy
! interpolate the intensity
    if (nixp.gt.npx) nixp = npx
    if (nix.gt.npx) nix = npx
    if (nix.lt.0) nix = 0
    if (nixp.lt.0) nixp= 0

    if (niyp.lt.-npy) niyp = -npy
    if (niy.lt.-npy) niy = -npy
    if (niyp.gt.npy) niyp = npy
    if (niy.gt.npy) niy = npy

      mLPNH(i,j,1:numset) = (Iarray(nix,niy,1:numset) * dxm * dym + &
                            Iarray(nixp,niy,1:numset) * dx * dym + &
                            Iarray(nix,niyp,1:numset) * dxm * dy + &
                            Iarray(nixp,niyp,1:numset) * dx * dy )

  end do 
end do 
    
end subroutine QC_applyIcosahedralSymmetry

end module kvectorsQC
