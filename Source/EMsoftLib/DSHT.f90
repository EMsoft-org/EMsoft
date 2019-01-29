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

! public :: SH_DiscreteSHT, SH_analyze, SH_synthesize, SH_cosLats, SH_printRow, SH_printRing, SH_row2ring, &
!           SH_ring2row, SH_computeWeightsSkip, SH_setSHTConstants 

logical, parameter    :: verbose = .TRUE.

private :: verbose


contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_dicreteSHTConstructor
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief Initialize a number of constants 
!
!> @param SHTC structure to keep all relevant pre-computed variables
!> @param d edge length of the Lambert grid (odd number, full length)
!> @param l bandwidth
!> @param layout  Lambert or Legendre colattitudinal angles ?
!
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive subroutine SH_dicreteSHTConstructor(SHTC, d, l, layout) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_dicreteSHTConstructor

use error
use constants
use typedefs
use FFTW3MOD
use io

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: d 
integer(kind=irg),INTENT(IN)                  :: l 
character(fnlen),INTENT(IN)                   :: layout

real(kind=dbl)								                :: k4p, kamm
integer(kind=irg)							                :: n, m, m2, n2, i, y, Nr, limit
type(C_PTR)                                   :: planf
real(C_DOUBLE),pointer                        :: inp(:)
complex(C_DOUBLE_COMPLEX),pointer             :: outp(:)
type(C_PTR)                                   :: p, o

if (verbose.eqv..TRUE.) write (*,*) 'entering SH_dicreteSHTConstructor'

! set a few constants
SHTC%dim = d 
SHTC%Nt = (d+1)/2
SHTC%maxL = l
SHTC%Nw = (d-2)/4+1
SHTC%d = (d-1)/2

! start with a sanity check on bandwidth and dimensions
if (d.lt.3) call FatalError('SH_dicreteSHTConstructor','square lambert side length (dim) must be at least 3')
if (mod(d,2).ne.1) call FatalError('SH_dicreteSHTConstructor','only odd Lambert square side lengths are supported')
if (trim(layout).eq.'Legendre') then
  limit = 2*(SHTC%Nt-1) + (1-mod(d,2))
  if (l.ge.limit) call FatalError('SH_dicreteSHTConstructor','maximum bandwidth is side # rings - 1 (for Legendre lattitudes)')
else
  if (l.ge.SHTC%Nt) call FatalError('SH_dicreteSHTConstructor','maximum bandwidth is (Lambert square side length) / 2')
end if

! everything is ok, so allocate the constant arrays
allocate(SHTC%wy(SHTC%Nt*SHTC%Nw), SHTC%cosTy(0:SHTC%Nt-1), SHTC%amn(0:l,0:l), SHTC%bmn(0:l,0:l))
SHTC%amn = 0.D0
SHTC%bmn = 0.D0

! get the cosines of the lattitudes
SHTC%cosTy = SH_cosLats(SHTC%dim, layout)

! and precompute the sqrt(1-x*x) values for the Legendre polynomials
allocate(SHTC%r1x2(0:SHTC%Nt-1))
SHTC%r1x2 = sqrt( 1.D0 - SHTC%cosTy * SHTC%cosTy )

! compute amn and bmn values
k4p = 1.D0/ (4.D0 * cPi)
kamm = 1.D0

! first compute a^m_m and a^m_{m+1}
SHTC%amn(0,0) = k4p
do m=1,SHTC%maxL-1
  SHTC%amn(m,m) = SHTC%amn(m-1,m-1) * sqrt(1.D0 + 1.D0 / (2.D0*m))
  SHTC%amn(m,m+1) = sqrt(3.D0-2.D0*m)
end do

do m=0,SHTC%maxL-1  ! first index of amn and bmn arrays 
! compute remaining a^m_n and b^m_n values
    m2 = m*m
    do n=m+2,SHTC%maxL-1    ! second index
      n2 = n*n
      SHTC%amn(m, n) = sqrt( (4.D0*n2-1.D0)/dble(n2-m2) )  ! Schaeffer Eq. 17
      SHTC%bmn(m, n) = sqrt( (2.D0*n+1.D0) * dble((n-1)*(n-1)-m2) / (2.D0*n-3.D0)/ dble(n2-m2) )  ! Schaeffer Eq. 18
    end do
end do


! compute the ring weights (scales with dim^4 !!!)
i = 0
call SH_computeWeightsSkip(SHTC, i)

! finally, build the array of pointers to fftw plans; we need a backward and forward plan for each lattitudinal ring size
allocate(SHTC%fftwPlans(0:SHTC%Nt-1))
call Message ('Creating fftw plans for direct and inverse Fourier transforms')
do y=1,SHTC%Nt-1
  Nr = maxval( (/ 1, 8*y /) )

  p = fftw_alloc_real(int(Nr,C_SIZE_T))
  call c_f_pointer(p, inp, [Nr])
  inp = 0.D0

  o = fftw_alloc_complex(int(Nr,C_SIZE_T))
  call c_f_pointer(o, outp, [Nr])
  outp = cmplx(0.D0,0.D0)

  SHTC%fftwPlans(y)%frwdplan = fftw_plan_dft_r2c_1d(Nr, inp, outp, FFTW_FORWARD)! +FFTW_ESTIMATE)
  SHTC%fftwPlans(y)%bkwdplan = fftw_plan_dft_c2r_1d(Nr, outp, inp, FFTW_BACKWARD)! +FFTW_ESTIMATE)
  
  call fftw_free(p)
  call fftw_free(o)
  call fftw_cleanup()
end do

if (verbose.eqv..TRUE.) write (*,*) 'leaving SH_dicreteSHTConstructor'

end subroutine SH_dicreteSHTConstructor


!--------------------------------------------------------------------------
!
! FUNCTION: SH_cosLats
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief return the cosines of the Lambert or Legendre lattitudes
!
!> @param d
!> @param t
!> returns lat
!
!> @date 01/21/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive function SH_cosLats(d, t) result(lat)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_cosLats

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                  :: d
character(fnlen),INTENT(IN)                   :: t
real(kind=dbl)                                :: lat(0:(d+1)/2-1)

integer(kind=irg)                             :: i, count, denom, numer, delta, info 
logical                                       :: even
real(kind=dbl),allocatable                    :: upd(:), diagonal(:)



if (trim(t).eq.'lambert') then ! Lambert lattitudinal grid
! set some constants
  count = (d+1)/2
  even = .TRUE.
  if (mod(d,2).ne.0) even = .FALSE.

  denom = (d-1) * (d-1)
  numer = denom
  delta = 4
  if (even.eqv..TRUE.) then
    numer = numer - 1
    delta = 8
  end if

  do i = 0, count-1 
    lat(i) = dble(numer)/dble(denom)
    numer = numer - delta
    delta = delta + 8
  end do
else ! Legendre lattitudinal grid (we'll use a Lapack routine to get the lattitudes)
  count = d
  allocate(diagonal(count),upd(count))
  diagonal = 0.D0
  upd = (/ (dble(i) / dsqrt(4.D0 * dble(i)**2 - 1.D0), i=1,count) /)
  call dsterf(count, diagonal, upd, info)
  lat(:) = -diagonal(1:(d+1)/2)
  lat((d+1)/2-1) = 0.D0     ! force the last one to zero
  deallocate(diagonal, upd)
end if

end function SH_cosLats


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
!> @param fname
!
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive subroutine SH_printRow(SHTC, dunit, sqr, fname) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_printRow

use typedefs

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: dunit
real(kind=dbl),INTENT(IN)                     :: sqr(:,:)
character(fnlen),INTENT(IN)                   :: fname

integer(kind=irg)							  :: i, j

open(unit=dunit,file=trim(fname),status='unknown',form='formatted')

do j=0,SHTC%dim-1 
  do i=0,SHTC%dim-1
    write (dunit,"(F12.6)") sqr(j,i)
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
!> @param fname
!
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
!--------------------------------------------------------------------------
recursive subroutine SH_printRing(SHTC, dunit, rng, fname) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_printRing

use typedefs

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: dunit
real(kind=dbl),INTENT(IN)                     :: rng(:)
character(fnlen),INTENT(IN)                   :: fname

integer(kind=irg)							                :: i, j, idx, numRings, cutoff, r, count
logical                                       :: even, sh

idx = 0
even = .FALSE.
if (mod(SHTC%dim,2).eq.0) even = .TRUE.
numRings = SHTC%dim 
if (even.eqv..TRUE.) numRings = numRings - 1
cutoff = (SHTC%dim - 1)/2

open(unit=dunit,file=trim(fname),status='unknown',form='formatted')

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
recursive subroutine SH_readRing(SHTC, MP, ringID, buffer) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_readRing

use error
use typedefs

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
real(kind=dbl),INTENT(IN)                     :: MP(-SHTC%d:SHTC%d,-SHTC%d:SHTC%d)
integer(kind=irg),INTENT(IN)                  :: ringID
real(kind=dbl),INTENT(INOUT)                  :: buffer(0:8*ringID-1)
!real(kind=dbl),INTENT(INOUT)                  :: buffer(0:4*ringID*(ringID+1)-1)

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
	do icol = ringID-1,-ringID,-1
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end do
! move to the lower left diagonal
	icol = -ringID
	do jrow=ringID-1,-ringID,-1
	  idx = idx + 1
	  buffer(idx) = MP(icol, jrow)	
	end do
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
recursive subroutine SH_writeRing(SHTC, MP, ringID, buffer) 
!DEC$ ATTRIBUTES DLLEXPORT :: SH_writeRing

use error
use typedefs

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
real(kind=dbl),INTENT(INOUT)                  :: MP(-SHTC%d:SHTC%d,-SHTC%d:SHTC%d)
integer(kind=irg),INTENT(IN)                  :: ringID
real(kind=dbl),INTENT(IN)                     :: buffer(0:8*ringID-1)
!real(kind=dbl),INTENT(IN)                     :: buffer(0:4*ringID*(ringID+1)-1)

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
  do icol = ringID-1,-ringID,-1
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end do
! move to the lower left diagonal
  icol = -ringID
  do jrow=ringID-1,-ringID,-1
    idx = idx + 1
    MP(icol, jrow) = buffer(idx)
  end do
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
!> @param SHTC
!> @param skp ring to exclude from weights (e.g. skip = 0 will exclude the poles)
!
!> @date 01/16/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_computeWeightsSkip(SHTC, skp)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_computeWeightsSkip

use constants
use typedefs
use io
use error

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
integer(kind=irg),INTENT(IN)                  :: skp

integer(kind=irg)                             :: num, dim, nMat, gridPoints, INFO, i, j, io_int(1)
integer(kind=irg),allocatable                 :: IPIV(:)
real(kind=dbl)                                :: wn, w0, delta
real(kind=dbl),allocatable                    :: a(:,:), x(:), b(:)

if (verbose.eqv..TRUE.) write (*,*) 'entering SH_computeWeightsSkip'

num = SHTC%dim
dim = SHTC%dim
nMat = (num+1)/2 - 1   ! -1 for the skipped ring

! build matrix of the form a_ij = cos(2*i*latitude[j]) for Sneeuw equation (21)
! We'll compute them using Chebyshev recursion: cos(ni) = T_n(cos(i)) with: T_0(x) = 1, 
!  T_1(x) = x, T_{n}(x) = 2x T_{n-1}(x) - T_{n-2}(x)
allocate(a(0:nMat-1,0:nMat-1), x(0:nMat-1), b(0:nMat-1))

a(0,:) = 1.D0
! fill the second row with cos(2theta_j) values using the double angle formula for cos(2x)
if (skp.eq.0) then
  do i=1,nMat
    x(i-1) = SHTC%cosTy(i)*SHTC%cosTy(i)*2.D0-1.D0
  end do
else
  do i=0,skp-1
    x(i) = SHTC%cosTy(i)*SHTC%cosTy(i)*2.D0-1.D0
  end do
  do i=skp,nMat-1
    x(i) = SHTC%cosTy(i+1)*SHTC%cosTy(i+1)*2.D0-1.D0
  end do
end if 
a(1,:) = x(:)

! loop over remaining rows using Chebyshev recursion to fill in cos(2*n*i)
do j=2,nMat-1
  a(j,:) = x(:) * a(j-1,:) * 2.D0 - a(j-2,:)  
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
SHTC%wy(:) = b(:)
deallocate(IPIV)

! compute the solid angle of a grid point
gridPoints = dim*dim*2 -(dim-1)*4     ! total number of points on sphere (equator has double cover)
wn = cPi * 4.D0 / dble(gridPoints)    ! solid angle of single point (solid angle of sphere / # grid points)
w0 = wn * dble( dim * (dim-2) +2 )    ! initial scaling factor (doesn't account for different # pts in each ring)

! rescale weights by solid angle and ratio of points to equatorial points and use 
! symmetry to fill southern hemisphere weights (the master pattern always has an 
! odd number of points along the side so we simplify the original C++ code, using offset = 0)
delta = sum(SHTC%wy(0:nMat-1)) - 1.D0     ! should be zero
if (delta.gt.(epsilon(1.D0)**(1.D0/3.D0))/64.D0) then 
    write (*,*) 'delta = ',delta
    ! call FatalError('SH_computeWeightsSkip','insufficient precision to accurately compute ring weights')
end if

! correct for alignment for missing ring
do i=nMat+1,skp,-1
  SHTC%wy(i) = SHTC%wy(i-1)
end do
SHTC%wy(skp) = 0
SHTC%wy(0) = SHTC%wy(0) * w0
do i=1,nMat+1
  SHTC%wy(i) = SHTC%wy(i) * w0 / dble(8*i)
end do

if (verbose.eqv..TRUE.) write (*,*) 'leaving SH_computeWeightsSkip'
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
!> @date 01/21/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_analyze(SHTC, mLPNH, mLPSH, alm)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_analyze

use typedefs
use FFTW3MOD

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
real(kind=dbl),INTENT(IN)                     :: mLPNH(-SHTC%d:SHTC%d,-SHTC%d:SHTC%d) 
real(kind=dbl),INTENT(IN)                     :: mLPSH(-SHTC%d:SHTC%d,-SHTC%d:SHTC%d) 
complex(kind=dbl),INTENT(INOUT)               :: alm(0:SHTC%maxL,0:SHTC%maxL)

type(SH_LUT)                                  :: shtLut
integer(kind=irg)                             :: y, Npy, fftN, maxdim, mLim, m, d , n
real(kind=dbl),allocatable                    :: buffer(:)
complex(kind=dbl)                             :: nPt, sPt
real(kind=dbl)                                :: x, wy, r1x2, kpmm, pmn, pmn1, pmn2
complex(C_DOUBLE_COMPLEX),pointer             :: outp
complex(kind=dbl)                             :: gmyS, gmyA, sigma, delta

if (verbose.eqv..TRUE.) write (*,*) 'entering SH_analyze'

! fill alm with complex zeroes
alm = cmplx(0.D0,0.D0)

! allocate Wrk arrays
d = SHTC%dim
allocate(shtLut%cWrk1(0:4*(d-1)), shtLut%cWrk2(0:4*(d-1)))

! for each ring we have a series of operations...
do y=1,SHTC%Nt-1
! copy current rings and compute G_{m,y} by fft (Reinecke equation 10) leveraging real symmetry
  Npy = maxval( (/ 1, 8*y /) )
  fftN = Npy/2+1   ! //number of fft points: half from real -> conjugate symmetric, this includes problematic points (imag == 0 from conjugate symmetry)
  allocate(buffer(0:Npy-1)) 

! read a ring from Northern and Southern hemispheres, and perform the correct fft on it to result in complex-valued data
  call SH_readRing(SHTC, mLPNH, y, buffer)  
  ! outp => shtLut%cWrk1
  write(*,*) 'starting fft N', y
  call fftw_execute_dft_r2c(SHTC%fftwPlans(y)%frwdplan, buffer, shtLut%cWrk1)

  call SH_readRing(SHTC, mLPSH, y, buffer)
  ! outp => shtLut%cWrk2
  write(*,*) 'starting fft S', y
  call fftw_execute_dft_r2c(SHTC%fftwPlans(y)%frwdplan, buffer, shtLut%cWrk2)
  deallocate(buffer)

! compute G_{m,y} +/- G_{m,Nt-1-y} since they are multiplied by symmetric values
  mLim = minval( (/ SHTC%maxL, fftN /) )   ! anything after l+1 isn't needed and anything after fftN is 0
  do m=0,mLim-1 
! get ring weight w_y
! weights excluding only the closest problematic ring (missing complex value due to real even dft) are most stable + accurate
! negate odd m values to correct for sign error in legendre polynomial calculation

! const Real& wy = shtLut->wy[size_t(m/4) * shtLut->Nt + y] * (m % 2 == 1 ? -1 : 1);//mod 4 from rings having 8y points + real symmetry of dft

! combine northern / southern hemisphere rings to take advantages of spherical harmonic symmetry
    nPt = shtLut%cWrk1(m) * cmplx(wy,0.D0)
    sPt = shtLut%cWrk2(m) * cmplx(wy,0.D0)

    shtLut%cWrk1(m) = nPt + sPt ! for even l+m (harmonics are     symmetric across equator)
    shtLut%cWrk2(m) = nPt - sPt ! for odd  l+m (harmonics are antisymmetric across equator)
  end do

! calculate seed values for on the fly legendre polynomial calculation (already done in the setup routine)
  x = SHTC%cosTy(y)       ! cosine of ring latitude
  r1x2 = SHTC%r1x2(y)     ! (1-x)^\frac{1}{2}: constant for P^m_m calculation
  kpmm = 1.D0             ! (1 - x^2) ^ \frac{|m|}{2} for m = 0: for P^m_m calculation

! accumulate a_{l,m} via direct summation (Reinecke equation 9) via on the fly summation
  do m=0,mLim-1
! get weighted values from fft 
    gmyS = shtLut%cWrk1(m)       ! symmetric modes
    gmyA = shtLut%cWrk2(m)       ! antisymmetric modes

! first compute P^m_m
    pmn2 = SHTC%amn(m,m) * kpmm    ! recursively compute P^m_m (Schaeffer equation 13)
    kpmm = kpmm * r1x2             ! update recursion for (1 - x^2) ^ \frac{|m|}{2}
    alm(0,m) = alm(0,m) + gmyS * pmn2
    if (m+1.eq.SHTC%maxL) EXIT     ! P^m_{m+1} doesn't exist

! now compute P^m_{m+1}
    pmn1 = SHTC%amn(m,m+1) * x * pmn2   ! P^m_n for n = m+1 (Schaeffer equation 14)
    alm(0,m+1) = alm(m,m+1) + gmyA * pmn1

! now compute the remaining polynomial values
    do n=m+2, SHTC%maxL-1
      pmn = SHTC%amn(m,n) * x * pmn1 - SHTC%bmn(m,n) * pmn2     ! P^m_n (Schaeffer equation 15)
      pmn2 = pmn1
      pmn1 = pmn
      if (mod(n+m,2).eq.0) then
        alm(n,m) = alm(n,m) + gmyS * pmn
      else
        alm(n,m) = alm(n,m) + gmyA * pmn
      end if 
    end do
  end do

end do

deallocate(buffer, shtLut%cWrk1, shtLut%cWrk2)

if (verbose.eqv..TRUE.) write (*,*) 'leaving SH_analyze'
end subroutine SH_analyze

!--------------------------------------------------------------------------
!
! SUBROUTINE: SH_synthesize
!
!> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
!
!> @brief compute a real values spherical function from spherical harmonic coefficients (reverse transformation)
!
! @param  alm : spherical harmonic coefficients (non negative, maxL * maxL)
! @param  mLPNH : location to write function value at each grid point (row major order, northern followed by southern hemisphere)
! @param  mLPSH : location to write function value at each grid point (row major order, northern followed by southern hemisphere)
! @param  limL : maximum bandwidth to use (exclusive) in reverse calculation (must be <= maxL), if 0 all bandwidths are used
!
!> @date 01/27/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SH_synthesize(SHTC, alm, mLPNH, mLPSH, limL)
!DEC$ ATTRIBUTES DLLEXPORT :: SH_synthesize

use typedefs
use FFTW3MOD
use error
use ieee_arithmetic

IMPLICIT NONE

type(SH_SHTConstantsType),INTENT(INOUT)       :: SHTC 
complex(kind=dbl),INTENT(IN)                  :: alm(0:SHTC%maxL,0:SHTC%maxL)
real(kind=dbl),INTENT(INOUT)                  :: mLPNH(-SHTC%d:SHTC%d,-SHTC%d:SHTC%d) 
real(kind=dbl),INTENT(INOUT)                  :: mLPSH(-SHTC%d:SHTC%d,-SHTC%d:SHTC%d) 
integer(kind=irg),INTENT(IN)                  :: limL

type(SH_LUT)                                  :: shtLut
integer(kind=irg)                             :: y, Npy, fftN, mLim, bndMax, d, m, n, Nr, i
real(kind=dbl)                                :: x, r1x2, kpmm, pmn1, pmn2, pmn
complex(kind=dbl)                             :: sigma, delta

real(C_DOUBLE),pointer                        :: buffer(:)
complex(C_DOUBLE_COMPLEX),pointer             :: cWrk1(:), cWrk2(:)
type(C_PTR)                                   :: p, o1, o2


if (verbose.eqv..TRUE.) write (*,*) 'entering SH_synthesize'

! sanity check bandwidth limit
if (limL.gt.SHTC%maxL) then
  call FatalError('SH_synthesize','limL must be <= maxL')
end if
if (limL.eq.0) then 
  bndMax = SHTC%maxL 
else 
  bndMax = limL
end if

! allocate Wrk arrays
d = SHTC%dim
allocate(cWrk1(0:4*(d-1)), cWrk2(0:4*(d-1)))

! compute inverse SHT one ring at a time
do y=1, SHTC%Nt-1  ! loop over rings

      Npy  = maxval( (/1, 8 * y /) )! get number of points in this ring
      fftN = Npy/2 + 1  ! number of fft points: half from real -> conjugate symmetric, 
                        ! this includes problematic points (imag == 0 from conjugate symmetry)

      o1 = fftw_alloc_complex(int(Npy,C_SIZE_T))
      call c_f_pointer(o1, cWrk1, [Npy])

      o2 = fftw_alloc_complex(int(Npy,C_SIZE_T))
      call c_f_pointer(o2, cWrk2, [Npy])

! calculate seed values for on the fly legendre polynomial calculation (we've already initialized these in the set up)
      x = SHTC%cosTy(y)  ! cosine of ring latitude
      r1x2 = SHTC%r1x2(y)! (1-x)^\frac{1}{2}: constant for P^m_m calculation
      kpmm = 1.D0        ! (1 - x^2) ^ \frac{|m|}{2} for m = 0: for P^m_m calculation

      cWrk1 = cmplx(0.D0,0.D0)   ! for symmetric modes
      cWrk2 = cmplx(0.D0,0.D0)   ! for antisymmetric modes

      mLim = minval( (/ SHTC%maxL, fftN /) )   ! anything after l+1 isn't needed and anything after fftN is 0
      do m = 0, mLim-1
        ! cWrk1(m) = cmplx(0.D0,0.D0)   ! for symmetric modes
        ! cWrk2(m) = cmplx(0.D0,0.D0)   ! for antisymmetric modes

! first compute P^m_m
        pmn2 = SHTC%amn(m,m) * kpmm    ! recursively compute P^m_m (Schaeffer equation 13)
        kpmm = kpmm * r1x2             ! update recursion for (1 - x^2) ^ \frac{|m|}{2}
        cWrk1(m) = cWrk1(m) + alm(m,m) * pmn2
        if (m+1.eq.SHTC%maxL) EXIT     ! P^m_{m+1} doesn't exist

! now compute P^m_{m+1}
        pmn1 = SHTC%amn(m,m+1) * x * pmn2   ! P^m_n for n = m+1 (Schaeffer equation 14)
        cWrk2(m) = cWrk2(m) + alm(m,m+1) * pmn1

! now compute the remaining polynomial values    [check minus sign !!!]
        do n=m+2, SHTC%maxL-1
          pmn = SHTC%amn(m,n) * x * pmn1 - SHTC%bmn(m,n) * pmn2     ! P^m_n (Schaeffer equation 15)
          pmn2 = pmn1
          pmn1 = pmn
          if (mod(n+m,2).eq.0) then
            cWrk1(m) = cWrk1(m) + alm(n,m) * pmn
          else
            cWrk2(m) = cWrk2(m) + alm(n,m) * pmn
          end if 
        end do
       end do

! now convert from F_{m,y} +- F_{m,Nt-1-y} -> F_{m,y} and F_{m,Nt-1-y}
       do m = 0, mLim-1
! combine northern / southern hemisphere rings to take advantages of spherical harmonic symmetry
! negate odd m values to correct for sign error in legendre polynomial calculation
        if (mod(m,2).eq.1) then
          sigma = -cWrk1(m)         ! F_{m,y} + F_{m,Nt-1-y}
          delta = -cWrk2(m)         ! F_{m,y} - F_{m,Nt-1-y}
        else
          sigma = cWrk1(m)          ! F_{m,y} + F_{m,Nt-1-y}
          delta = cWrk2(m)          ! F_{m,y} - F_{m,Nt-1-y}
        end if
        cWrk1(m) = sigma + delta    ! northern hemisphere point
        cWrk2(m) = sigma - delta    ! southern hemisphere point
      end do
write (*,*) 'starting fft ',y,'; max val Wrk arrays ', maxval(abs(cWrk1)), maxval(abs(cWrk2))

! do the inverse ffts of F_{m,y} and F_{m,Nt-1-y} (Reinecke equation 7) and copy to output
      Nr = 8*y

      p = fftw_alloc_real(int(Nr,C_SIZE_T))
      call c_f_pointer(p, buffer, [Nr])
      buffer = 0.D0

write (*,*) cWrk1(0:2)
  
      call fftw_execute_dft_c2r(SHTC%fftwPlans(y)%bkwdplan, cWrk1, buffer)
      write (*,*) y, maxval(buffer)
      call SH_writeRing(SHTC, mLPNH, y, buffer)

      call fftw_execute_dft_c2r(SHTC%fftwPlans(y)%bkwdplan, cWrk2, buffer)
      call SH_writeRing(SHTC, mLPSH, y, buffer)

      call fftw_free(p)
      call fftw_free(o1)
      call fftw_free(o2)
      call fftw_cleanup()

end do

if (verbose.eqv..TRUE.) write (*,*) 'leaving  SH_synthesize'

end subroutine SH_synthesize

end module DSHT








