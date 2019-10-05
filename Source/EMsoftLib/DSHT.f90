!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!*                                                                     *
!* Copyright (c) 2019-2019, De Graef Group, Carnegie Mellon University *
!* All rights reserved.                                                *
!*                                                                     *
!* Author: William C. Lenthe                                           *
!*                                                                     *
!* EMSphInx is available for academic or non-profit non-commercial     *
!* research use. Please, see the license.txt file in this distribution *
!* for further details.                                                *
!*                                                                     *
!* Interested in a commercial license? Contact:                        *
!*                                                                     *
!* Center for Technology Transfer and Enterprise Creation              *
!* 4615 Forbes Avenue, Suite 302                                       *
!* Pittsburgh, PA 15213                                                *
!*                                                                     *
!* phone. : 412.268.7393                                               *
!* email  : innovation@cmu.edu                                         *
!* website: https://www.cmu.edu/cttec/                                 *
!*                                                                     *
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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
!>          -polar optimization (implicitly via equal area grid)
!> @note: I've opted for on the fly (vs precomputed calculations) since the single thread performance 
!>        was very similar and on the fly has much lower memory overhead
!> @note: single thread performance is comparable to SHTns compiled without SIMD enabled for the same 
!>        number of rings, but non legendre root rings -> half the bandwidth
!> @note: complexity is ~n^2.7 for reasonable bandwidths (should probably be something like log(n)*n^2)
! 
!> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's C++ routines
!> @date 07/29/19 WCL 2.0 switch from dim to half dim
!--------------------------------------------------------------------------
module DSHT

use local
use Lambert
use fft_wrap
use, intrinsic :: iso_c_binding

implicit none

  private ! default everything to private

  ! types
  public :: DiscreteSHT          ! actual interface for computing SHT
  public :: DiscreteSHTConstants ! read only constants for DiscreteSHT (+ associated helper functions)

  ! functions

  !@brief                   : compute grid ring latitudes
  !@param d [IN] integer    : half grid size - square grid is (-d:d,-d:d)
  !@param t [IN] character(): grid type ('legendre' or 'lambert')
  !@return       real(0:d)  : cosine(latitudes of grid rings) [north hemisphere only]
  public :: SH_cosLats

  !@brief                         : compute grid direction cosines
  !@param d [IN] integer          : half grid size - square grid is (-d:d,-d:d)
  !@param t [IN] character()      : grid type ('legendre' or 'lambert')
  !@return       real(3,-d:d,-d:d): (x,y,z) direction cosines for each grid point [north hemisphere only]
  public :: SH_dirCos

  !@brief                             : interpolate a square lambert image on a sqaure legendre grid
  !@param di [IN] intger              : input  image size
  !@param ni [IN] real(-di:do, -di:do): input  square lambert image (north hemisphere)
  !@param si [IN] real(-di:do, -di:do): input  square lambert image (south hemisphere)
  !@param do [IN] intger              : output image size
  !@param no [IN] real(-di:do, -di:do): output square lambert image (north hemisphere)
  !@param so [IN] real(-di:do, -di:do): output square lambert image (south hemisphere)
  public :: LegendreInterp

  ! primarily for debugging
  public :: SH_printRow, SH_printRing

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!                                                                    !!
  !!  these are the thread safe components required for a discrete SHT  !!
  !!                                                                    !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type DiscreteSHTConstants
    integer(kind=irg)             :: d           ! half length of square Lambert projection - grid is (-d:d,-d:d), e.g. dim is 2*d+1, Nt == d+1
    integer(kind=irg)             :: maxL        ! maximum bandwidth of square lambert projection (must be < Nt for arbitrary rings (Nt*2 for legendre rings))
    integer(kind=irg)             :: Nw          ! number of different types of weights [(dim-2) / 8 + 1]
    real   (kind=dbl),allocatable :: r1x2(:)     ! precomputed sqrt(1-x*x) values
    real   (kind=dbl),allocatable :: wy(:,:)     ! weighting factor for each ring [Nt * Nw]
    real   (kind=dbl),allocatable :: cosTy(:)    ! cosine of latitude of each ring [Nt]
    real   (kind=dbl),allocatable :: amn(:,:)    ! precomputed a^m_n values for on the fly ylm calculation [maxL^2]
    real   (kind=dbl),allocatable :: bmn(:,:)    ! precomputed b^m_n values for on the fly ylm calculation [maxL^2]
    type   (RealFFT ),allocatable :: fftPlans(:) ! array of plans for 1D real <--> complex ffts
  contains
    !@brief                        : initialize a discrete SHT constants type
    !@param d      [IN] integer    : half grid size - square grid is (-d:d,-d:d)
    !@param l      [IN] integer    : maximum bandwidth
    !@param layout [IN] character(): grid type ('legendre' or 'lambert')
    procedure         :: init    => DiscreteSHTConstants_Init

    !@brief: clean up a discrete SHT constants type
    procedure         :: destroy => DiscreteSHTConstants_Destroy

    !@brief: clean up resources automatically
    final             ::            DiscreteSHTConstants_Finalize ! just calls destroy w/ polymorphism
  end type DiscreteSHTConstants

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!                                                                    !!
  !!           this is the main type to do discrete SHT with            !!
  !!                                                                    !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type DiscreteSHT
    ! these are the non-thread safe components required for a discrete SHT
    complex(C_DOUBLE_COMPLEX    ),pointer     :: cWrk1(:), cWrk2(:) ! complex working arrays for a single ring
    real   (C_DOUBLE            ),pointer     :: rWrk1(:), rWrk2(:) ! real working array for a single ring
    type   (FFTBuffer           )             :: pc1, pc2, pr1, pr2 ! fft allocated arrays for pointers

    ! these are the thread safe (read only) components required for a discrete SHT
    type  (DiscreteSHTConstants),allocatable  :: shtLut

  contains
    !@brief                        : initialize a discrete SHT calculator
    !@param d      [IN] integer    : half grid size - square grid is (-d:d,-d:d)
    !@param l      [IN] integer    : maximum bandwidth
    !@param layout [IN] character(): grid type ('legendre' or 'lambert')
    procedure :: init       => DiscreteSHT_Init

    !@brief: clean up a discrete SHT calculator
    procedure :: destroy    => DiscreteSHT_Destroy

    !@brief: automatically clean up on destruction
    final     ::               DiscreteSHT_Finalize

    !@brief                                      : compute harmonic coefficients from a spherical function
    !@param mLPNH [IN   ] real (-d:d   , -d:d   ): square grid of function values for north hemisphere
    !@param mLPSH [IN   ] real (-d:d   , -d:d   ): square grid of function values for south hemisphere
    !@param alm   [INOUT] cmplx( 0:bw-1,  0:bw-1): location to write harmonic coefficients
    procedure :: analyze    => SH_analyze

    !@brief                                      : compute harmonic coefficients from a spherical function
    !@param alm   [IN   ] cmplx( 0:bw-1,  0:bw-1): harmonic coefficients to synthesize
    !@param mLPNH [INOUT] real (-d:d   , -d:d   ): square grid to write function values for north hemisphere
    !@param mLPSH [INOUT] real (-d:d   , -d:d   ): square grid to write function values for south hemisphere
    !@param limL  [IN   ] real (-d:d   , -d:d   ): maximum bandwidth to use (exclusive) [must be < maxL, 0 to use all bandwidths]
    procedure :: synthesize => SH_synthesize

  end type DiscreteSHT

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                          Helper Functions                          !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !--------------------------------------------------------------------------
  !
  ! FUNCTION: SH_cosLats
  !
  !> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
  !
  !> @brief return the cosines of the Lambert or Legendre latitudes
  !
  !> @param d half grid size - square grid is (-d:d,-d:d)
  !> @param t grid type ('legendre' or 'lambert')
  !> returns lat
  !
  !> @date 01/21/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
  !> @date 07/30/19 WCL 2.0 modified to take half grid size instead of full grid size
  !--------------------------------------------------------------------------
  recursive function SH_cosLats(d, t) result(lat)
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_cosLats
    use error

  implicit none

    integer  (kind=irg),INTENT(IN)  :: d
    character(fnlen   ),INTENT(IN)  :: t
    real     (kind=dbl)             :: lat(0:d)

    integer  (kind=irg)             :: i, count, denom, numer, delta, info 
    logical                         :: even
    real     (kind=dbl),allocatable :: upd(:), diagonal(:)

    if (trim(t).eq.'lambert') then ! Lambert latitudinal grid
    ! note: the c++ version of the code uses the side length dim == d * 2 + 1
    ! set some constants
      count = d+1
      even = .TRUE.
      if (mod(d,2).ne.0) even = .FALSE.

      denom = 4 * d * d
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
    else if (trim(t).eq.'legendre') then ! Legendre latitudinal grid (we'll use a Lapack routine to get the latitudes)
      ! this block compute the roots of the Legendre polynomial for N == d * 2 - 1
      count = d * 2 - 1 ! skip north + south pole (and don't double count equator)
      allocate(diagonal(count),upd(count))
      diagonal = 0.D0
      upd = (/ (dble(i) / dsqrt(4.D0 * dble(i)**2 - 1.D0), i=1,count) /)
      call dsterf(count, diagonal, upd, info)
      lat(0) = 1 ! add north pole back in
      lat(1:) = -diagonal(1:d) ! fill in after north pole
      lat(d) = 0.D0     ! force the last one to zero
      deallocate(diagonal, upd)
    else
      call FatalError('SH_cosLats','type must be "lambert" or "legendre" (got "' // trim(t) // '")')
    end if

  end function SH_cosLats


  !--------------------------------------------------------------------------
  !
  ! FUNCTION: SH_dirCos
  !
  !> @author Will Lenthe, Carnegie Mellon University
  !
  !> @brief compute grid direction cosines
  !
  !> @param d half grid size - square grid is (-d:d,-d:d)
  !> @param t grid type ('legendre' or 'lambert')
  !> returns (x,y,z) direction cosines for each grid point [north hemisphere only]
  !
  !> @date 07/30/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive function SH_dirCos(d, t) result(dc)
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_dirCos
    use constants

  implicit none

    integer  (kind=irg),INTENT(IN) :: d
    character(fnlen   ),INTENT(IN) :: t
    real     (kind=dbl)            :: dc(3,-d:d,-d:d)

    real     (kind=dbl)            :: n(3), cosLats(0:d)

    integer                        :: i, j, ai, aj, ar
    real     (kind=dbl)            :: sX, sY, qq, x, y, h

    ! start by getting the ring lattitudes
    cosLats = SH_cosLats(d,t)

    ! finally loop over output grid computing direction cosines
    do j = -d, d ! loop over first dimension of square grid
      aj = abs(j) ! get absolute radius
      y = real(j)/real(d)! get y position in [-1,1]
      do i = -d, d ! loop over second dimension of square grid
        ai = abs(i) ! get absolute radius
        x = real(i)/real(d) ! get x position in [-1,1]
        ar = maxval((/ ai, aj /) ) ! determine distance to center (ring number)

        if (ar.eq.0) then ! this is the north pole, don't divide by 0
          n = (/ 0.D0, 0.D0, 1.D0 /)
        else ! this is a normal point
          ! compute fractional progress to current ring
          sX = real(i) / ar
          sY = real(j) / ar

          ! do square lambert longitude calculation
          qq = cPi * sX * sY * 0.25D0
          if (ai.le.aj) then 
            x = sY * sin(qq)
            y = sY * cos(qq)
          else
            x = sX * cos(qq)
            y = sX * sin(qq)
          end if 
          h = sqrt(x*x+y*y)

          ! assemble unit direction
          n(3) = cosLats(ar) ! get z coordinate from cosine(ring latitude)
          qq = sqrt(1.D0-n(3)*n(3)) / h ! determine how much magnitude is left for x and y
          n(1) = qq * x ! x direction cosine
          n(2) = qq * y ! y direction cosine
        end if
        dc(:,i,j) = n ! save result
      end do  ! i
    end do ! j

  end function SH_dirCos

  !--------------------------------------------------------------------------
  !
  ! FUNCTION: LegendreInterp
  !
  !> @author Will Lenthe, Carnegie Mellon University
  !
  !> @brief interpolate a square lambert image on a sqaure legendre grid
  !
  !> @param di input  image size
  !> @param ni input  square lambert image (north hemisphere)
  !> @param si input  square lambert image (south hemisphere)
  !> @param do output image size
  !> @param no output square lambert image (north hemisphere)
  !> @param so output square lambert image (south hemisphere)
  !
  !> @date 07/30/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine LegendreInterp(di, ni, si, do, no, so)
  !DEC$ ATTRIBUTES DLLEXPORT :: LegendreInterp
    use imageOPs
    use Lambert
  implicit none
    integer  (kind=irg),INTENT(IN   ) :: di
    real     (kind=dbl),INTENT(IN   ) :: ni(-di:di,-di:di)
    real     (kind=dbl),INTENT(IN   ) :: si(-di:di,-di:di)
    integer  (kind=irg),INTENT(IN   ) :: do
    real     (kind=dbl),INTENT(INOUT) :: no(-do:do,-do:do)
    real     (kind=dbl),INTENT(INOUT) :: so(-do:do,-do:do)

    real     (kind=dbl)               :: ns(-do:do,-do:do), ss(-do:do,-do:do) ! scaled hemispheres
    real     (kind=dbl)               :: dc(3,-do:do,-do:do) ! square legendre direction cosines
    real     (kind=dbl)               :: n(3), xy(2)
    integer                           :: i, j, ierr, ix, iy, ix2, iy2
    real     (kind=dbl)               :: x, y, fx, fy, cx, cy, vn, vs
    character(fnlen   )               :: t = 'legendre'

    ! start by rescaling input image to have ~the same pixel size at the output image if needed
    if(di.eq.do) then
      ns = ni ! copy input north hemisphere to scaled north hemisphere
      ss = si ! copy input south hemisphere to scaled south hemisphere
    else
      call RescaleImage(ni, ns, .TRUE.) ! rescale input north hemisphere to output size (and make mean value 0)
      call RescaleImage(si, ss, .TRUE.) ! rescale input north hemisphere to output size (and make mean value 0)
    endif

    ! next get direction associated with each legendre grid point (output grid)
    dc = SH_dirCos(do, t)

    ! finally loop over output grid bilinearly interpolating
    do j = -do, do ! loop over first dimension of square grid
      do i = -do, do ! loop over second dimension of square grid
        n(:) = dc(:,i,j)

        ! now that we have the direction of our legendre grid get corresponding position position on lambert grid
        xy = LambertSphereToSquare(n, ierr) * do ! adjust from [-1,1] to [-do, do]
        x = xy(1)
        y = xy(2)

        ! get bounding indices
        ix = int(x) ! truncate to int
        iy = int(y) ! truncate to int
        if(x.ge.0.D0) then
          ix2 = min(ix+1,  do)
        else
          ix2 = max(ix-1, -do)
        endif
        if(y.ge.0.D0) then
          iy2 = min(iy+1,  do)
        else
          iy2 = max(iy-1, -do)
        endif

        ! bilinearly interpolate from the square lambert grid
        fx = abs(x - ix) ! fractional progress between pixels in x direction
        fy = abs(y - iy) ! fractional progress between pixels in y direction
        cx = 1.D0 - fx   ! complement of fx
        cy = 1.D0 - fy   ! complement of fy
        vn = cx * cy * ns(ix ,iy ) &
           + fx * cy * ns(ix2,iy ) &
           + cx * fy * ns(ix ,iy2) &
           + fx * fy * ns(ix2,iy2)
        vs = cx * cy * ss(ix ,iy ) &
           + fx * cy * ss(ix2,iy ) &
           + cx * fy * ss(ix ,iy2) &
           + fx * fy * ss(ix2,iy2)
        no(i,j) = vn
        so(i,j) = vs
      end do  ! i
    end do ! j
  end subroutine LegendreInterp

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: SH_printRow
  !
  !> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
  !
  !> @brief Print square lambert projection in row major order to a file 'SHTC-printrow.txt'
  !
  !> @param d
  !> @param dunit
  !> @param sqr
  !> @param fname
  !
  !> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
  !--------------------------------------------------------------------------
  recursive subroutine SH_printRow(d, dunit, sqr, fname) 
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_printRow

  implicit none

    integer  (kind=irg),INTENT(IN) :: d
    integer  (kind=irg),INTENT(IN) :: dunit
    real     (kind=dbl),INTENT(IN) :: sqr(-d:d,-d:d)
    character(fnlen   ),INTENT(IN) :: fname

    integer  (kind=irg)            :: i, j

    open(unit=dunit,file=trim(fname),status='unknown',form='formatted')

    do j=-d,d 
      do i=-d,d
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
  !> @note  This routine has not been tested yet...
  !
  !> @param dim
  !> @param dunit
  !> @param rng
  !> @param fname
  !
  !> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
  !--------------------------------------------------------------------------
  recursive subroutine SH_printRing(dim, dunit, rng, fname) 
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_printRing

  implicit none

    integer  (kind=irg),INTENT(IN) :: dim 
    integer  (kind=irg),INTENT(IN) :: dunit
    real     (kind=dbl),INTENT(IN) :: rng(:)
    character(fnlen   ),INTENT(IN) :: fname

    integer  (kind=irg)            :: i, j, idx, numRings, cutoff, r, count
    logical                        :: even, sh

    idx = 0
    even = .FALSE.
    if (mod(dim,2).eq.0) even = .TRUE.
    numRings = dim 
    if (even.eqv..TRUE.) numRings = numRings - 1
    cutoff = (dim - 1)/2

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
  !> @brief get a single ring from a square lambert projection
  !
  !> @param d
  !> @param MP
  !> @param ringID
  !> @param buffer
  !
  !> @date 01/16/19 MDG 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine SH_readRing(d, MP, ringID, buffer) 
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_readRing

  use error

  implicit none

    integer(kind=irg),INTENT(IN   ) :: d 
    real   (kind=dbl),INTENT(IN   ) :: MP(-d:d,-d:d)
    integer(kind=irg),INTENT(IN   ) :: ringID
    real   (kind=dbl),INTENT(INOUT) :: buffer(0:8*ringID-1)

    integer(kind=irg)               :: i, j, icol, jrow, idx

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
  !> @param d
  !> @param MP
  !> @param ringID
  !> @param buffer
  !
  !> @date 01/16/19 MDG 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine SH_writeRing(d, MP, ringID, buffer) 
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_writeRing

    use error

  implicit none

    integer(kind=irg),INTENT(IN   ) :: d 
    real   (kind=dbl),INTENT(INOUT) :: MP(-d:d,-d:d)
    integer(kind=irg),INTENT(IN   ) :: ringID
    real   (kind=dbl),INTENT(IN   ) :: buffer(0:8*ringID-1)

    integer(kind=irg)               :: i, j, icol, jrow, idx

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
  !> @note  This will only work correctly for the Legendre case 
  !
  !> @details For details, see https://doi.org/10.1111/j.1365-246X.1994.tb03995.x
  !
  !> @param SHTC
  !> @param skp ring to exclude from weights (e.g. skip = 0 will exclude the poles)
  !> @param ind index of weights to fill in SHTC%wy
  !
  !> @date 01/16/19 MDG 1.0 original
  !> @date 07/29/19 WCL 2.0 switch from dim to half dim
  !--------------------------------------------------------------------------
  recursive subroutine SH_computeWeightsSkip(SHTC, skp, ind)
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_computeWeightsSkip

    use constants
    use io
    use error

  implicit none

    type   (DiscreteSHTConstants),INTENT(INOUT) :: SHTC 
    integer(kind=irg            ),INTENT(IN   ) :: skp
    integer(kind=irg            ),INTENT(IN   ) :: ind

    integer(kind=irg            )               :: dim, nMat, gridPoints, INFO, i, j, io_int(1)
    integer(kind=irg            ),allocatable   :: IPIV(:)
    real   (kind=dbl            )               :: wn, w0, delta
    real   (kind=dbl            ),allocatable   :: a(:,:), x(:), b(:)

    dim  = SHTC%d * 2 + 1
    nMat = SHTC%d ! == # ring pairs -1

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
    deallocate(IPIV)

    ! make sure we didn't run into precision issues
    delta = sum(b) - 1.D0     ! should be zero
    if (delta.gt.(epsilon(1.D0)**(1.D0/3.D0))/64.D0) then 
        write (*,*) 'delta = ',delta
        call FatalError('SH_computeWeightsSkip','insufficient precision to accurately compute ring weights')
    end if

    ! compute the solid angle of a grid point
    gridPoints = dim*dim*2 -(dim-1)*4     ! total number of points on sphere (equator has double cover)
    wn = cPi * 4.D0 / dble(gridPoints)    ! solid angle of single point (solid angle of sphere / # grid points)
    w0 = wn * dble( dim * (dim-2) +2 )    ! initial scaling factor (doesn't account for different # pts in each ring)

    ! copy to output correcting alignment for missing ring
    SHTC%wy(0    :skp , ind) = b(0:skp)
    SHTC%wy(      skp , ind) = 0.D0
    SHTC%wy(skp+1:nMat, ind) = b(skp:nMat-1)

    ! rescale weights by solid angle and ratio of points to equatorial points and use 
    ! symmetry to fill southern hemisphere weights (the master pattern always has an 
    ! odd number of points along the side so we simplify the original C++ code, using offset = 0)
    SHTC%wy(0,ind) = SHTC%wy(0,ind) * w0 ! need to divide by for for even
    do i=1, nMat
      SHTC%wy(i,ind) = SHTC%wy(i,ind) * w0 / dble(8 * i) ! (8*i+4) for even
    enddo

  end subroutine SH_computeWeightsSkip

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                    DiscreteSHTConstants Members                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: DiscreteSHTConstants_Init
  !
  !> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
  !
  !> @brief Initialize a number of constants 
  !
  !> @param this structure to keep all relevant pre-computed variables
  !> @param d edge length of the Lambert grid (odd number, full length)
  !> @param l bandwidth
  !> @param layout  Lambert or Legendre colatitudinal angles ?
  !
  !> @date 01/16/19 MDG 1.0 original, based on Will Lenthe's classes in square_sphere.hpp
  !> @date 07/29/19 WCL 2.0 switch from dim to half dim
  !--------------------------------------------------------------------------
  recursive subroutine DiscreteSHTConstants_Init(this, d, l, layout) 
  !DEC$ ATTRIBUTES DLLEXPORT :: DiscreteSHTConstants_Init

    use error
    use constants
    use io

  implicit none

    class    (DiscreteSHTConstants),INTENT(INOUT) :: this 
    integer  (kind=irg            ),INTENT(IN   ) :: d 
    integer  (kind=irg            ),INTENT(IN   ) :: l 
    character(fnlen               ),INTENT(IN   ) :: layout

    integer(kind=irg        )                     :: n, m, m2, n2, i, y, Nr, limit

    ! clean up an existing object
    call this%destroy()

    ! set a few constants
    this%d    = d           ! half length of square Lambert projection
    this%maxL = l           ! maximum bandwidth of square lambert projection (must be < Nt for arbitrary rings (Nt*2 for legendre rings))
    this%Nw   = (2*d-1)/4+1 ! number of different types of weights [(dim-2) / 8 + 1]

    ! start with a sanity check on bandwidth and dimensions
    if (d.lt.1) call FatalError('DiscreteSHTConstants_Init','square lambert half side length must be at least 1')
    if (trim(layout).eq.'legendre') then
      limit = 2*d
      if (l.ge.limit) call FatalError('DiscreteSHTConstants_Init','maximum bandwidth is side # rings - 1 (for Legendre latitudes)')
    else if (trim(layout).eq.'lambert') then
      if (l.gt.this%d) call FatalError('DiscreteSHTConstants_Init','maximum bandwidth is (Lambert square side length) / 2')
    else
      call FatalError('DiscreteSHTConstants_Init','type must be "lambert" or "legendre" (got "' // trim(layout) // '")')
    end if

    ! allocate memory
    allocate(this%r1x2    (0:this%d)            ) ! precomputed sqrt(1-x*x) values
    allocate(this%wy      (0:this%d,0:this%Nw-1)) ! weighting factor for each ring [Nt * Nw]
    allocate(this%cosTy   (0:this%d)            ) ! cosine of latitude of each ring [Nt]
    allocate(this%amn     (0:l        ,0:l     )) ! precomputed a^m_n values for on the fly ylm calculation [maxL^2]
    allocate(this%bmn     (0:l        ,0:l     )) ! precomputed b^m_n values for on the fly ylm calculation [maxL^2]
    allocate(this%fftPlans(0:this%d)            ) ! array of plans for 1D real <--> complex ffts

    ! get the cosines of the latitudes
    this%cosTy = SH_cosLats(this%d, layout)

    ! and precompute the sqrt(1-x*x) values for the Legendre polynomials
    this%r1x2 = sqrt( 1.D0 - this%cosTy * this%cosTy )

    ! compute amn and bmn values

    ! first compute a^m_m and a^m_{m+1}
    this%amn = 0.D0
    this%bmn = 0.D0
    this%amn(0,0) = sqrt(1.D0/ (4.D0 * cPi))
    do m=1,this%maxL-1
      this%amn(m,m) = this%amn(m-1,m-1) * sqrt(1.D0 + 1.D0 / (2.D0*m))
    end do

    do m=0,this%maxL-1  ! first index of amn and bmn arrays 
    ! compute remaining a^m_n and b^m_n values
        m2 = m*m
        do n=m+1,this%maxL-1    ! second index
          n2 = n*n
          this%amn(m, n) = sqrt( (4.D0*n2-1.D0)/dble(n2-m2) )  ! Schaeffer Eq. 17
          this%bmn(m, n) = sqrt( (2.D0*n+1.D0) * dble((n-1)*(n-1)-m2) / (2.D0*n-3.D0)/ dble(n2-m2) )  ! Schaeffer Eq. 18
        end do
    end do

    ! compute the ring weights (scales with dim^4 !!!)
    i = 0
    if (trim(layout).eq.'legendre') then ! legendre weights
      call SH_computeWeightsSkip(this, i, i) ! compute weights once with north pole excluded
      do i=1, this%Nw-1
        this%wy(:,i) = this%wy(:,0) ! copy weights to other rings
      enddo
    else if (trim(layout).eq.'lambert') then ! lambert weights
      do i=0, this%Nw-1
        call SH_computeWeightsSkip(this, i, i) ! compute weights for each excluded ring
      enddo
    else
      call FatalError('DiscreteSHTConstants_Init','type must be "lambert" or "legendre" (got "' // trim(layout) // '")')
    endif

    ! finally, build the array of pointers to fft plans; we need a backward and forward plan for each latitudinal ring size
    do y=1,this%d
      Nr = maxval( (/ 1, 8*y /) )
      call this%fftPlans(y)%init(Nr, FFT_PLAN_ESTIMATE)
    end do

  end subroutine DiscreteSHTConstants_Init

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: DiscreteSHTConstants_Destroy
  !
  !> @author Will Lenthe, Carnegie Mellon University
  !
  !> @brief Free a number of arrays 
  !
  !> @param this structure to keep all relevant pre-computed variables
  !
  !> @date 07/17/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine DiscreteSHTConstants_Destroy(this) 
  !DEC$ ATTRIBUTES DLLEXPORT :: DiscreteSHTConstants_Destroy
  implicit none

    class(DiscreteSHTConstants),INTENT(INOUT) :: this

    if(allocated(this%r1x2    )) deallocate(this%r1x2    )
    if(allocated(this%wy      )) deallocate(this%wy      )
    if(allocated(this%cosTy   )) deallocate(this%cosTy   )
    if(allocated(this%amn     )) deallocate(this%amn     )
    if(allocated(this%bmn     )) deallocate(this%bmn     )
    if(allocated(this%fftPlans)) deallocate(this%fftPlans)
  end subroutine DiscreteSHTConstants_Destroy

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: DiscreteSHTConstants_Finalize
  !
  !> @author Will Lenthe, Carnegie Mellon University
  !
  !> @brief call %destroy at cleanup
  !
  !> @param this structure to keep all relevant pre-computed variables
  !
  !> @date 07/17/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine DiscreteSHTConstants_Finalize(this) 
  !DEC$ ATTRIBUTES DLLEXPORT :: DiscreteSHTConstants_Finalize
  implicit none
    type(DiscreteSHTConstants),INTENT(INOUT) :: this
    call this%destroy()
  end subroutine DiscreteSHTConstants_Finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                    DiscreteSHTConstants Members                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DiscreteSHT_Init(this, d, l, layout)
  !DEC$ ATTRIBUTES DLLEXPORT :: DiscreteSHT_Init

  implicit none

    class  (DiscreteSHT),INTENT(INOUT) :: this 
    integer  (kind=irg ),INTENT(IN   ) :: d 
    integer  (kind=irg ),INTENT(IN   ) :: l 
    character(fnlen    ),INTENT(IN   ) :: layout
    integer(kind=irg   )               :: Npy

    ! clean up an existing object
    call this%destroy()

    ! first allocate constants
    allocate(this%shtLut)
    call this%shtLut%init(d, l, layout)

    ! next allocate fft specidic memory
    Npy = (this%shtLut%d+1)*8 ! largest ring size (equator)
    call this%pc1%allocCplx(Npy)
    call this%pc2%allocCplx(Npy)
    call this%pr1%allocReal(Npy)
    call this%pr2%allocReal(Npy)

    ! finally wrap fft memory
    call c_f_pointer(this%pc1%ptr, this%cWrk1, [Npy])
    call c_f_pointer(this%pc2%ptr, this%cWrk2, [Npy])
    call c_f_pointer(this%pr1%ptr, this%rWrk1, [Npy])
    call c_f_pointer(this%pr2%ptr, this%rWrk2, [Npy])
  end subroutine DiscreteSHT_Init

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: DiscreteSHT_Destroy
  !
  !> @author Will Lenthe, Carnegie Mellon University
  !
  !> @brief Free a number of arrays 
  !
  !> @param this structure to keep all relevant pre-computed variables
  !
  !> @date 07/17/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine DiscreteSHT_Destroy(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: DiscreteSHT_Destroy
  implicit none
    class(DiscreteSHT),INTENT(INOUT) :: this ! for final

    ! free fft allocated memory
    call this%pc1%free()
    call this%pc2%free()
    call this%pr1%free()
    call this%pr2%free()

    ! break pointer association
    nullify(this%cWrk1)
    nullify(this%cWrk2)
    nullify(this%rWrk1)
    nullify(this%rWrk1)

    ! free shtLut
    if(allocated(this%shtLut)) then
      call this%shtLut%destroy()
      deallocate(this%shtLut)
    endif
  end subroutine DiscreteSHT_Destroy

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: DiscreteSHT_Finalize
  !
  !> @author Will Lenthe, Carnegie Mellon University
  !
  !> @brief call %destroy at cleanup
  !
  !> @param this structure to keep all relevant pre-computed variables
  !
  !> @date 07/17/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive subroutine DiscreteSHT_Finalize(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: DiscreteSHT_Finalize
  implicit none
    type(DiscreteSHT),INTENT(INOUT) :: this
    call this%destroy()
  end subroutine DiscreteSHT_Finalize

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
  !             (0,maxL-1), (1,0), (1,1), (1,2), ..., (maxL-1,0), (maxL-1,1), (maxL-1,maxL-1)
  !
  !> @date 01/21/19 MDG 1.0 original
  !> @date 07/29/19 WCL 2.0 switch from dim to half dim
  !--------------------------------------------------------------------------
  recursive subroutine SH_analyze(this, mLPNH, mLPSH, alm)
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_analyze

  implicit none

    class  (DiscreteSHT),INTENT(INOUT)   :: this 
    real   (kind=dbl   ),INTENT(IN   )   :: mLPNH(-this%shtLut%d:this%shtLut%d     ,-this%shtLut%d:this%shtLut%d     ) 
    real   (kind=dbl   ),INTENT(IN   )   :: mLPSH(-this%shtLut%d:this%shtLut%d     ,-this%shtLut%d:this%shtLut%d     ) 
    complex(kind=dbl   ),INTENT(INOUT)   :: alm  (             0:this%shtLut%maxL-1,             0:this%shtLut%maxL-1)

    integer(kind=irg        )            :: y, Npy, fftN, maxdim, mLim, m, d , n
    complex(kind=dbl        )            :: nPt, sPt
    real   (kind=dbl        )            :: x, wy, r1x2, kpmm, pmn, pmn1, pmn2, wcorr
    complex(C_DOUBLE_COMPLEX),pointer    :: outp
    complex(kind=dbl        )            :: sigma, delta

    ! fill alm with complex zeroes
    alm = cmplx(0.D0,0.D0)

    ! for each ring we have a series of operations...
    do y=1,this%shtLut%d
    ! copy current rings and compute G_{m,y} by fft (Reinecke equation 10) leveraging real symmetry
      Npy = maxval( (/ 1, 8*y /) )
      fftN = Npy/2+1   ! //number of fft points: half from real -> conjugate symmetric, this includes problematic points (imag == 0 from conjugate symmetry)

    ! read a ring from Northern and Southern hemispheres, and perform the correct fft on it to result in complex-valued data
      call SH_readRing(this%shtLut%d, mLPNH, y, this%rWrk1) ! extract yth ring from north pole into rWrk1
      call this%shtLut%fftPlans(y)%forward(this%rWrk1, this%cWrk1)
      call SH_readRing(this%shtLut%d, mLPSH, y, this%rWrk1) ! extract yth ring from south pole into rWrk
      call this%shtLut%fftPlans(y)%forward(this%rWrk1, this%cWrk2)

    ! compute G_{m,y} +/- G_{m,Nt-1-y} since they are multiplied by symmetric values
      mLim = minval( (/ this%shtLut%maxL, fftN /) )   ! anything after l+1 isn't needed and anything after fftN is 0
      do m=0,mLim-1 

    ! get ring weight w_y
    ! weights excluding only the closest problematic ring (missing complex value due to real even dft) are most stable + accurate
    ! negate odd m values to correct for sign error in legendre polynomial calculation
        wcorr = 1.D0
        if (mod(m,2).eq.1) wcorr = -1.D0

    ! combine northern / southern hemisphere rings to take advantages of spherical harmonic symmetry
        nPt = wcorr * this%cWrk1(m+1) * cmplx(this%shtLut%wy(y,m/4),0.D0)
        sPt = wcorr * this%cWrk2(m+1) * cmplx(this%shtLut%wy(y,m/4),0.D0)

        this%cWrk1(m+1) = nPt + sPt ! for even l+m (harmonics are     symmetric across equator)
        this%cWrk2(m+1) = nPt - sPt ! for odd  l+m (harmonics are antisymmetric across equator)
      end do

    ! calculate seed values for on the fly legendre polynomial calculation (already done in the setup routine)
      x = this%shtLut%cosTy(y)       ! cosine of ring latitude
      r1x2 = this%shtLut%r1x2(y)     ! (1-x)^\frac{1}{2}: constant for P^m_m calculation
      kpmm = 1.D0             ! (1 - x^2) ^ \frac{|m|}{2} for m = 0: for P^m_m calculation

    ! accumulate a_{l,m} via direct summation (Reinecke equation 9) via on the fly summation
      do m=0,mLim-1

    ! first compute P^m_m
        pmn2 = this%shtLut%amn(m,m) * kpmm    ! recursively compute P^m_m (Schaeffer equation 13)
        kpmm = kpmm * r1x2             ! update recursion for (1 - x^2) ^ \frac{|m|}{2}
        alm(m,m) = alm(m,m) + this%cWrk1(m+1) * pmn2
        if (m+1.eq.this%shtLut%maxL) EXIT     ! P^m_{m+1} doesn't exist

    ! now compute P^m_{m+1}
        pmn1 = this%shtLut%amn(m,m+1) * x * pmn2   ! P^m_n for n = m+1 (Schaeffer equation 14)
        alm(m+1,m) = alm(m+1,m) + this%cWrk2(m+1)  * pmn1

    ! now compute the remaining polynomial values
        do n=m+2, this%shtLut%maxL-1
          pmn = this%shtLut%amn(m,n) * x * pmn1 - this%shtLut%bmn(m,n) * pmn2     ! P^m_n (Schaeffer equation 15)
          pmn2 = pmn1
          pmn1 = pmn
          if (mod(n+m,2).eq.0) then
            alm(n,m) = alm(n,m) + this%cWrk1(m+1) * pmn
          else
            alm(n,m) = alm(n,m) + this%cWrk2(m+1)  * pmn
          end if 
        end do
      end do
    end do
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
  ! @param  limL : maximum bandwidth to use (exclusive) in reverse calculation (must be < maxL), if 0 all bandwidths are used
  !
  !> @date 01/27/19 MDG 1.0 original
  !> @date 07/29/19 WCL 2.0 switch from dim to half dim
  !--------------------------------------------------------------------------
  recursive subroutine SH_synthesize(this, alm, mLPNH, mLPSH, limL)
  !DEC$ ATTRIBUTES DLLEXPORT :: SH_synthesize

    use error
    use ieee_arithmetic

  implicit none

    class  (DiscreteSHT),INTENT(INOUT)       :: this 
    complex(kind=dbl   ),INTENT(IN   )       :: alm(               0:this%shtLut%maxL-1,             0:this%shtLut%maxL-1)
    real   (kind=dbl   ),INTENT(INOUT)       :: mLPNH(-this%shtLut%d:this%shtLut%d     ,-this%shtLut%d:this%shtLut%d     ) 
    real   (kind=dbl   ),INTENT(INOUT)       :: mLPSH(-this%shtLut%d:this%shtLut%d     ,-this%shtLut%d:this%shtLut%d     ) 
    integer(kind=irg   ),INTENT(IN   )       :: limL

    integer(kind=irg)                        :: y, Npy, fftN, mLim, bndMax, d, m, n, Nr, i
    real   (kind=dbl)                        :: x, r1x2, kpmm, pmn1, pmn2, pmn
    complex(kind=dbl)                        :: sigma, delta

    ! sanity check bandwidth limit
    if (limL.ge.this%shtLut%maxL) then
      call FatalError('SH_synthesize','limL must be < maxL')
    end if
    if (limL.eq.0) then 
      bndMax = this%shtLut%maxL 
    else 
      bndMax = limL
    end if

    ! compute inverse SHT one ring at a time
    do y=1, this%shtLut%d  ! loop over rings

      Npy  = maxval( (/1, 8 * y /) )! get number of points in this ring
      fftN = Npy/2 + 1  ! number of fft points: half from real -> conjugate symmetric, 
                        ! this includes problematic points (imag == 0 from conjugate symmetry)


    ! calculate seed values for on the fly legendre polynomial calculation (we've already initialized these in the set up)
      x = this%shtLut%cosTy(y)  ! cosine of ring latitude
      r1x2 = this%shtLut%r1x2(y)! (1-x)^\frac{1}{2}: constant for P^m_m calculation
      kpmm = 1.D0        ! (1 - x^2) ^ \frac{|m|}{2} for m = 0: for P^m_m calculation

      this%cWrk1 = cmplx(0.D0,0.D0)   ! for symmetric modes
      this%cWrk2 = cmplx(0.D0,0.D0)   ! for antisymmetric modes

      mLim = minval( (/ this%shtLut%maxL, fftN /) )   ! anything after l+1 isn't needed and anything after fftN is 0
      do m = 0, mLim-1
    ! first compute P^m_m
        pmn2 = this%shtLut%amn(m,m) * kpmm    ! recursively compute P^m_m (Schaeffer equation 13)
        kpmm = kpmm * r1x2             ! update recursion for (1 - x^2) ^ \frac{|m|}{2}
        this%cWrk1(m+1) = this%cWrk1(m+1) + alm(m,m) * pmn2
        if (m+1.eq.this%shtLut%maxL) EXIT     ! P^m_{m+1} doesn't exist

    ! now compute P^m_{m+1}
        pmn1 = this%shtLut%amn(m,m+1) * x * pmn2   ! P^m_n for n = m+1 (Schaeffer equation 14)
        this%cWrk2(m+1) = this%cWrk2(m+1) + alm(m+1,m) * pmn1

    ! now compute the remaining polynomial values 
        do n=m+2, this%shtLut%maxL-1
          pmn = this%shtLut%amn(m,n) * x * pmn1 - this%shtLut%bmn(m,n) * pmn2     ! P^m_n (Schaeffer equation 15)
          pmn2 = pmn1
          pmn1 = pmn
          if (mod(n+m,2).eq.0) then
            this%cWrk1(m+1) = this%cWrk1(m+1) + alm(n,m) * pmn
          else
            this%cWrk2(m+1) = this%cWrk2(m+1) + alm(n,m) * pmn
          end if 
        end do
      end do

    ! now convert from F_{m,y} +- F_{m,Nt-1-y} -> F_{m,y} and F_{m,Nt-1-y}
       do m = 0, mLim-1
    ! combine northern / southern hemisphere rings to take advantages of spherical harmonic symmetry
    ! negate odd m values to correct for sign error in legendre polynomial calculation
        if (mod(m,2).eq.1) then
          sigma = -this%cWrk1(m+1)         ! F_{m,y} + F_{m,Nt-1-y}
          delta = -this%cWrk2(m+1)         ! F_{m,y} - F_{m,Nt-1-y}
        else
          sigma = this%cWrk1(m+1)          ! F_{m,y} + F_{m,Nt-1-y}
          delta = this%cWrk2(m+1)          ! F_{m,y} - F_{m,Nt-1-y}
        end if
        this%cWrk1(m+1) = sigma + delta    ! northern hemisphere point
        this%cWrk2(m+1) = sigma - delta    ! southern hemisphere point
      end do

    ! do the inverse ffts of F_{m,y} and F_{m,Nt-1-y} (Reinecke equation 7) and copy to output

      this%rWrk1 = 0.D0
      call this%shtLut%fftPlans(y)%inverse(this%cWrk1, this%rWrk1)
      call SH_writeRing(this%shtLut%d, mLPNH, y, this%rWrk1)

      this%rWrk1 = 0.D0
      call this%shtLut%fftPlans(y)%inverse(this%cWrk2, this%rWrk1)
      call SH_writeRing(this%shtLut%d, mLPSH, y, this%rWrk1)
    end do
  end subroutine SH_synthesize

end module DSHT
