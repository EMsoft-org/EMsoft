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
! EMsoft:DSHT.f90
!--------------------------------------------------------------------------
!
! MODULE: DSHT
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Discrete Spherical Harmonic Transform (DSHT) routines
! 
!> @details This implements Will Lenthe's square_sphere.hpp routines for the computation of 
!> the spherical harmonic Fourier Transform, which are subsequently used in the EMSphInx algorithm.
!> @reference: Reinecke , M. (2011). Libpsht–algorithms for efficient spherical harmonic transforms. 
!>               Astronomy & Astrophysics, 526, A108.
!> @reference: Schaeffer, N. (2013). Efficient spherical harmonic transforms aimed at pseudospectral 
!>               numerical simulations. Geochemistry, Geophysics, Geosystems, 14(3), 751-758.
!> @note: grid requirements:
!>          -pixels are arranged on N_\phi \approx \sqrt{N_{pix}} iso-latitude rings w/ colatitude of 
!>           ring y = \theta_y
!>          -within a ring pixels are equidistance in azimuthal angle and have identical weight (solid 
!>           angle) w_y
!>          -the number of pixels in a given ring N_{\phi,y} can vary
!>          -the first pixel in a ring is at azimuthal angle \phi_{0,y}
!>        the square lambert grid satisfies these requirements:
!>          -\sqrt{ 2.5 * N_{pix} } rings for side length 3, rapidly approaching \sqrt{2 * N_{pix} } 
!>           rings (within 1% at side length 9)
!>          -all grid points cover the same solid angle -> w_y = 1 / rings
!>          -\phi_{0,y} = 0 for odd side lengths (must be calculated for even side lengths)
!>          -for the reverse transformation (synthesis) all N_{\phi,y} are even
!> @note: optimizations implemented (see Schaeffer for details):
!>          -use of real values FFT -> factor of 2 savings on FFT
!>          -mirror (conjugate) symmetry of spherical harmonics -> factor of 2 on direct summations 
!>           [only possible for ring positions that are symmetric across the equator]
!>          -polar optimization (implicitely via equal area grid)
!> @note: I've opted for on the fly (vs precomputed calculations) since the single thread performance 
!>        was very similar and on the fly has much lower memory overhead
!> @note: single thread performance is comparable to SHTns compiled without SIMD enabled for the same 
!>        number of rings, but non legendre root rings -> half the bandwidth
!> @note: complexity is ~n^2.7 for reasonable bandwidths (should probably be something like log(n)*n^2)
! 
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's C++ routines
!--------------------------------------------------------------------------
module DSHT

use local
use Lambert

IMPLICIT NONE

public :: SH_DiscreteSHT, SH_analyze, SH_synthesize, SH_cosLats, SH_printRow, SH_printRing, SH_row2ring, &
          SH_ring2row, SH_computeWeightsSkip, SH_SHTConstants 

! here we need to define an array of pointers (ffts) to fftw plans ...


contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_setSHTConstants
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Initialize a number of constants 
!
!> @param SHTC
!> @param d
!> @param l
!> @param c
!
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive subroutine SH_setSHTConstants(SHTC, d, l, c) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_setSHTConstants

use error
use constants

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: d 
integer(kind=irg),INTENT(IN)                  :: l 
real(kind=dbl),INTENT(IN)                     :: c

real(kind=dbl)								  :: k4p, kamm
integer(kind=irg)							  :: m, m, m2, n12, n2, n2m2, n12m2, i, y

! set a few constants
SHTC%dim = d 
SHTC%Nt = (d+1)/2
SHTC%maxL = l
SHTC%Nw = (d-2)/4+1
SHTC%d = (d-1)/2

! start with a sanity check on bandwidth and dimensions
if (dim.lt.3) call FatalError('SH_setSHTConstants','square lambert side length (dim) must be at least 3')
if (mod(dim,2).eq.1) call FatalError('SH_setSHTConstants','only odd Lambert square side lengths are supported')
if (l.ge.Nt) call FatalError('SH_setSHTConstants','maximum bandwidth is (Lambert square side length) / 2')

! everything is ok, so allocate the constant arrays
allocate(SHTC%wy(Nt*Nw), SHTC%cosTy(c,c+Nt), SHTC%amn(0:l*l), SHTC%bmn(0:l*l))
allocate(ffts(Nt)) ! these are defined separately, but are part of SHTC in the original C++ code
SHTC%amn = 0.D0
SHTC%bmn = 0.D0

! compute amn and bmn values
k4p = 1.D0/ (4.D0 * cPi)
kamm = 1.D0
iloop: do m=0,SHTC%maxL-1
! first compute a^m_m
  SHTC%(m * SHTC%maxL + m) = sqrt( kamm * k4p )   ! Schaeffer Eq. 16
  kamm = kamm * ( dble(2*m+3)/dble(2*m+2) )
  if ((m+1).eq.SHTC%maxL) EXIT iloop

! compute a^m_{m+1}
  m2 = m * m
  n12 = m2
  n2 = (m+1) * (m+1)
  n2m2 = n2 - m2
  n12m2 = 0
  SHTC%amn(m * SHTC%maxL + m+1) = sqrt(dble(4*n2-1)/dble(n2m2))

! compute remaining a^m_n and b^m_n values
  do n=m+2,SHTC%maxL-1
    n12 = n2 
    n12m2 = n2m2
    n2 = n*n
    n2m2 = n2 - m2
    SHTC%amn(m * SHTC%maxL + n) = sqrt( dble(4*n2-1)/dble(n2m2) )  ! Schaeffer Eq. 17
    SHTC%bmn(m * SHTC%maxL + n) = sqrt( dble((2*n+1) * n12m2)/dble((2*n-3)*n2m2) )  ! Schaeffer Eq. 18
  end do
end do iloop

! compute the ring weights (scales with dim^4 !!!)
do i=0,SHTC%Nw-1
  call SH_computeWeightsSkip(SHTC, i)
end do

! finally, build the array of pointers to fft plans
! do y=0,SHTC%Nt-1
!   ffts(y) = 
! end do



end subroutine SH_setSHTConstants








!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_printRow
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Print square lambert projection in row major order to a file 'SHTC-printrow.txt'
!
!> @param SHTC
!> @param dunit
!> @param sqr
!
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive subroutine SH_printRow(SHTC, dunit, sqr) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_printRow

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: dunit
real(kind=dbl),INTENT(IN)                     :: sqr(:)

integer(kind=irg)							  :: i, j

open(unit=dunit,file='SHTC-printrow.txt',status='unknown',form='formatted')

do j=0,SHTC%dim-1 
  do i=0,SHTC%dim-1
    write (dunit,"(F12.6)") sqr(SHTC%dim*j+i)
  end do 
end do

do j=0,SHTC%dim-1 
  do i=0,SHTC%dim-1
    write (dunit,"(F12.6)") sqr(SHTC%dim*SHTC%dim + SHTC%dim*j+i)
  end do 
end do

close(unit=dunit,status='keep')

end subroutine SH_printRow

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_printRing
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Print square lambert projection in row major order to a file 'SHTC-printrow.txt'
!
!> @param SHTC
!> @param dunit
!> @param rng
!
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive subroutine SH_printRing(SHTC, dunit, rng) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_printRing

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: dunit
real(kind=dbl),INTENT(IN)                     :: rng(:)

integer(kind=irg)							  :: i, j, idx, numRings, cutoff, r, count
logical                                       :: even, sh

idx = 0
even = .FALSE.
if (mod(SHTC%dim,2).eq.0) even = .TRUE.
numRings = SHTC%dim 
if (even.eqv..TRUE.) numRings = numRings - 1
cutoff = (SHTC%dim - 1)/2

open(unit=dunit,file='SHTC-printrng.txt',status='unknown',form='formatted')

do i=0,numRings-1
  sh = .FALSE.
  if (i.gt.cutoff) sh = .TRUE.
  r = i
  if (sh.eqv..TRUE.) r = 2*cutoff-r
  count = 8 * r 
  if (even.eqv..TRUE.) then
    count = count + 4
  else
    if (r.eq.0) count = count + 1
  end if
  do j=0,count-1
    write (dunit,"(F12.6)") rng(idx)
    idx = idx + 1
  end do
end do

close(unit=dunit,status='keep')

end subroutine SH_printRing


!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_readRing
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief get a single ring form a square lambert projection
!
!> @param SHTC
!> @param MP
!> @param ringID
!> @param buffer
!
!> @date 01/16/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_readRing(SHTC, MP, ringID, ringdim, buffer) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_readRing

use error

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
real(kind=dbl),INTENT(IN)                     :: MP(-SHTC%dim:SHTC%dim,-SHTC%dim:SHTC%dim)
integer(kind=irg),INTENT(IN)                  :: ringID
real(kind=dbl),INTENT(INOUT)                  :: buffer(0:4*ringID*(ringID+1)-1)

integer(kind=irg) 							              :: d, i, j, icol, jrow, idx

d = SHTC%d
if (ringID.gt.d) call FatalError('SH_readRing','requested ring number does not exist')

! set the starting location
idx = 0
icol = ringID
jrow = 0
buffer(idx) = MP(icol, jrow)

if (ringID.gt.1) then  ! this is a regular ring
! we spiral counter-clockwise through the array and make a 90° turn at the diagonals
! go up to the upper right diagonal
	do jrow=1,ringID
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end do
! move to the upper left diagonal
	jrow = ringID
	do irow = ringID-1,-ringID,-1
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end do
! move to the lower left diagonal
	icol = -ringID
	do jrow=ringID-1,-ringID,-1
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end
! move to the lower right diagonal
	jrow = -ringID
	do icol = -ringID+1,ringID
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end do
! and close the ring
	icol = ringID
	do jrow=-ringID+1,-1
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end do
else  ! this is the innermost ring with 8 slots
	buffer(1) = MP( 1, 1)
	buffer(2) = MP( 0, 1)
	buffer(3) = MP(-1, 1)
	buffer(4) = MP(-1, 0)
	buffer(5) = MP(-1,-1)
	buffer(6) = MP( 0,-1)
	buffer(7) = MP( 1,-1)
end if

end subroutine SH_readRing

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_writeRing
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief write a single ring to a square lambert projection
!
!> @param SHTC
!> @param MP
!> @param ringID
!> @param buffer
!
!> @date 01/16/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_writeRing(SHTC, MP, ringID, ringdim, buffer) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_writeRing

use error

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
real(kind=dbl),INTENT(INOUT)                  :: MP(-SHTC%dim:SHTC%dim,-SHTC%dim:SHTC%dim)
integer(kind=irg),INTENT(IN)                  :: ringID
real(kind=dbl),INTENT(IN)                     :: buffer(0:4*ringID*(ringID+1)-1)

integer(kind=irg)                             :: d, i, j, icol, jrow, idx

d = SHTC%d
if (ringID.gt.d) call FatalError('SH_writeRing','requested ring number does not exist')

! set the starting location
idx = 0
icol = ringID
jrow = 0
MP(icol, jrow) = buffer(idx)

if (ringID.gt.1) then  ! this is a regular ring
! we spiral counter-clockwise through the array and make a 90° turn at the diagonals
! go up to the upper right diagonal
  do jrow=1,ringID
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end do
! move to the upper left diagonal
  jrow = ringID
  do irow = ringID-1,-ringID,-1
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end do
! move to the lower left diagonal
  icol = -ringID
  do jrow=ringID-1,-ringID,-1
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end
! move to the lower right diagonal
  jrow = -ringID
  do icol = -ringID+1,ringID
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end do
! and close the ring
  icol = ringID
  do jrow=-ringID+1,-1
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end do
else  ! this is the innermost ring with 8 slots
  MP( 1, 1) = buffer(1)
  MP( 0, 1) = buffer(2)
  MP(-1, 1) = buffer(3)
  MP(-1, 0) = buffer(4)
  MP(-1,-1) = buffer(5)
  MP( 0,-1) = buffer(6)
  MP( 1,-1) = buffer(7)
end if

end subroutine SH_writeRing

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_computeWeightsSkip
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief compute quadrature weights for rings (w_y in equation 10 of Reinecke)
!
!> @details For details, see https://doi.org/10.1111/j.1365-246X.1994.tb03995.x
!
!> @param dim side length of square lambert projection to compute weights for
!> @param lat cosines of ring latitudes (symmetric across equator)
!> @param wgt weights for each row
!> @param skp ring to exclude from weights (e.g. skip = 0 will exclude the poles)
!
!> @date 01/16/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_computeWeightsSkip(dim, lat, wgt, skp)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_computeWeightsSkip

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                  :: dim
real(kind=dbl),INTENT(INOUT)                  :: lat(:)   ! nMat
real(kind=dbl),INTENT(INOUT)                  :: wgt(:)   ! nMat + 1
integer(kind=irg),INTENT(IN)                  :: skp

integer(kind=irg)                             :: num, nMat, gridPoints, INFO
integer(kind=irg),allocatable                 :: IPIV(:)
real(kind=dbl)                                :: wn, w0
real(kind=dbl),allocatable                    :: a(:,:), x(:), b(:)

num = dim
nMat = (num+1)/2 - 1   ! -1 for the skipped ring

! build matrix of the form a_ij = cos(2*i*latitude[j]) for Sneeuw equation (21)
! We'll compute them using Chebyshev recursion: cos(ni) = T_n(cos(i)) with: T_0(x) = 1, 
!  T_1(x) = x, T_{n}(x) = 2x T_{n-1}(x) - T_{n-2}(x)
allocate(a(0:nMat-1,0:nMat-1), x(0:nMat-1), b(0:nMat-1))
a(:,0) = 1.D0
! fill the second row with cos(2theta_j) values using the double angle formula for cos(2x)
if (skp.eq.0) then
  do i=1,nMat-1
    x(i) = lat(i)*lat(i)*2.D0-1.D0
  end do
else
  do i=1,skp-1
    x(i) = lat(i)*lat(i)*2.D0-1.D0
  end do
  do i=skp,nMat-1
    x(i) = lat(i+1)*lat(i+1)*2.D0-1.D0
  end do
end if 
a(:,1) = x(:)

! loop over remaining rows using Chebyshev recursion to fill in cos(2*n*i)
do j=2,nMat-1
  a(:,j) = x(:) * a(:,j-1) * 2.D0 - a(:,j-2)  
end do
deallocate(x)

! build the column vector for the right hand side of the equation
b = (/ (-1.D0/(4.D0*dble(i)*dble(i)-1.D0),i=0,nMat-1 ) /)

! solve the equation     a * wgt = b   (we'll use a Lapack routine to do so)
allocate(IPIV(nMat))
call dgesv(nMat, 1, a, nMat, IPIV, b, nMat, INFO)
if (INFO.ne.0) then 
  if (INFO.gt.0) then
    io_int(1) = INFO
    call WriteValue(' The following matrix argument had an illegal value',io_int,1)
  else
    io_int(1) = -INFO
    call WriteValue(' The following U matrix element is zero :',io_int,1,'(I2,$')
    call Message('; Matrix factorization completed, but solution could not be computed.')
  end if
  call FatalError('SH_computeWeightsSkip','Error returned from LAPACK dgesv routine ... ')
end if
wgt(:) = b(:)
deallocate(IPIV)

! compute the solid angle of a grid point
gridPoints = dim*dim*2 -(dim-1)*4     ! total number of points on sphere (equator has double cover)
wn = cPi * 4.D0 / dble(gridPoints)    ! solid angle of single point (solid angle of sphere / # grid points)
w0 = wn * dble( dim * (dim-2) +2 )    ! initial scaling factor (doesn't account for different # pts in each ring)

! rescale weights by solid angle and ratio of points to equatorial points and use 
! symmetry to fill southern hemisphere weights (the master pattern always has an 
! odd number of points along the side so we simplify the original C++ code, using offset = 0)
delta = sum(wgt(0:nMat-1)) - 1.D0     ! should be zero
if (delta.gt.(epsilon(1.D0)**(1.D0/3.D0))/64.D0) then 
    call FatalError('SH_computeWeightsSkip','insufficient precision to accurately compute ring weights')
end if

! correct for alignment for missing ring
do i=nMat+1,skp,-1
  wgt(i) = wgt(i-1)
end do
wgt(skp) = 0
wgt(0) = wgt(0) * w0
do i=1,nMat+1
  wgt(i) = wgt(i) * w0 / dble(8*i)
end do

end subroutine SH_computeWeightsSkip


!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_analyze
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief compute spherical harmonic coefficients from a spherical function (forward transformation)
!
!> @param pts value of function to analyze at each grid point (row major order, northern 
!             followed by southern hemisphere)
!> @param alm location to write alm values maxL * maxL with (m,l) stored: (0,0), (0,1), (0,2), ..., 
!             (0,maxL), (1,0), (1,1), (1,2), ..., (maxL,0), (maxL,1), (maxL,maxL)
!
!> @date 01/16/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_analyze(pts, alm)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_analyze


end subroutine SH_analyze

end module DSHT