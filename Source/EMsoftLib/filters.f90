! ###################################################################
! Copyright (c) 2016-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:filters.f90
!--------------------------------------------------------------------------
!
! MODULE: filters
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief various useful filters
! 
!> @date 01/21/16 MDG 1.0 original
!> @date 02/02/16 MDG 1.1 added Hough Transform
!> @date 01/09/18 MDG 1.2 added getADPmap
!> @date 09/14/18 MDG 1.3 added fftw_cleanup() calls to eliminate momory leaks
!> @date 11/16/19 MDG 1.4 added Gaussian beam broadening filter for EBSD
!--------------------------------------------------------------------------

module filters

use local

IMPLICIT NONE

contains 

!--------------------------------------------------------------------------
!
! SUBROUTINE: applyGaussianBeamSpread
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  apply a 2D Gaussian beam broadening kernel to the planes of a 3D array 
!
!> @param ipar array dimensions
!> @param fpar step sizes
!> @param Vxyz counts array; will contain convolved array on exit
!> @param w FWHM of Gaussian beam
! 
!> @date 11/16/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine applyGaussianBeamSpread(ipar, fpar, Vxyz, w, verbose) 
!DEC$ ATTRIBUTES DLLEXPORT :: applyGaussianBeamSpread

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                :: ipar(3)
real(kind=sgl),INTENT(IN)                   :: fpar(2)
integer(kind=irg),INTENT(INOUT)             :: Vxyz(ipar(1),ipar(2),ipar(3))
real(kind=dbl),INTENT(IN)                   :: w 
logical,OPTIONAL,INTENT(IN)                 :: verbose

real(kind=dbl)                              :: sigma, pre, x, y, bd
integer(kind=irg)                           :: smax, ix, iy
real(kind=dbl),allocatable                  :: gkernel(:,:), accum(:,:,:), V(:,:,:)

! convert w to sigma
sigma = w / (2.D0*sqrt(2.D0*log(2.D0)))
pre = 1.D0/(sigma**2*2.0*cPi)

! get the largest distance from the origin for the convolution kernel
smax =  ceiling(2.5758*sigma) + 1  ! determined by requiring that kernel contains at least 98.5% of the Gaussian intensity

if (present(verbose)) then 
  if (verbose.eqv..TRUE.) write (*,*) 'w, sigma, pre, smax : ',w, sigma, pre, smax 
end if 

! convert sigma to effective exponential argument
sigma = 1.D0/(2.D0 * sigma*sigma)

! generate the Gaussian kernel
allocate(gkernel(-smax:smax,-smax:smax))
do ix = -smax, smax
  x = (fpar(1) * ix)**2 
  do iy = -smax, smax
    y = (fpar(2) * iy)**2
    gkernel(ix,iy) = exp( -sigma * (x+y) )
  end do
end do 
gkernel = pre * gkernel 

if (present(verbose)) then 
  if (verbose.eqv..TRUE.) then 
    do ix = -smax, smax 
      write (*,*) gkernel(ix,-smax:smax) 
    end do
    write (*,*) 'sum of all kernel elements : ', sum(gkernel) * fpar(1) * fpar(2)
  end if
end if 

! generate the accumulator array accum; the nint version of this will be returned in Vxyz
allocate(accum(ipar(1),ipar(2),ipar(3)))
accum = 0.D0
bd = 0.D0

if (present(verbose)) then 
  if (verbose.eqv..TRUE.) write(*,*) shape(Vxyz), shape(accum)
end if

! loop over x and y range and add the weighted eoshift() array to the accumulator 
! we begin by eoshifting the input array to row -smax
allocate(V(ipar(1),ipar(2),ipar(3)))
V = dble(Vxyz)
V = eoshift(V, shift=-smax, boundary=bd, dim=1)
do ix = 1,2*smax+1
! loop over all the column shifts
  do iy = -smax, smax
    accum = accum + gkernel(ix-1-smax,iy) * eoshift(V, shift = iy, boundary = bd, dim=2)
  end do
! go to the next row 
  if (ix.lt.2*smax+1) V = eoshift(V, shift=1, boundary=bd, dim=1)
end do 

! and copy the accumulator array into the input array as nearest integers
Vxyz = nint( accum * fpar(1) * fpar(2) )
deallocate(V, accum, gkernel)

end subroutine applyGaussianBeamSpread

!--------------------------------------------------------------------------
!
! FUNCTION: applyPoissonNoise
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  apply a Poisson random deviate to an image
!
!> @param nx x dimension
!> @param ny y dimension
!> @param im image array; must have values in range [1..256]
! 
!> @date 03/23/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function applyPoissonNoise(image, nx, ny, idum) result(noisy)
!DEC$ ATTRIBUTES DLLEXPORT :: applyPoissonNoise

use noise

IMPLICIT NONE

integer, parameter              :: K4B=selected_int_kind(9)

integer(kind=irg),INTENT(IN)    :: nx
integer(kind=irg),INTENT(IN)    :: ny
real(kind=sgl),INTENT(IN)       :: image(nx, ny)
integer(K4B),INTENT(INOUT)      :: idum
!f2py intent(in,out) ::  idum
real(kind=sgl)                  :: noisy(nx, ny)

integer(kind=irg)               :: i, j  
real(kind=sgl)                  :: av, mult, invmult 

do i=1, nx
  do j=1, ny
    noisy(i,j) = POIDEV(image(i,j),idum)
  end do 
end do

end function applyPoissonNoise

!--------------------------------------------------------------------------
!
! FUNCTION: image_histogram 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the histogram of an image 
!
!> @param nx x dimension
!> @param ny y dimension
!> @param im image array; must have values in range [1..256]
! 
!> @date 01/23/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function image_histogram( nx, ny, im ) result(h)
!DEC$ ATTRIBUTES DLLEXPORT :: image_histogram

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nx
integer(kind=irg),INTENT(IN)    :: ny
integer(kind=irg),INTENT(IN)    :: im(nx, ny)
integer(kind=irg)               :: h(256)

integer(kind=irg),parameter     :: nh = 256
integer(kind=irg)               :: i, j

! initialize parameters 
h = 0

do i=1,nx
  do j=1,ny
    h(im(i,j)) = h(im(i,j))+1
  end do
end do

end function image_histogram


!--------------------------------------------------------------------------
!
! FUNCTION: image_jointhistogram 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the joint histogram of two images 
!
!> @param nx x dimension
!> @param ny y dimension
!> @param im1 image array; must have values in range [1..256]
!> @param im2 image array; must have values in range [1..256]
! 
!> @date 04/23/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function image_jointhistogram( nx, ny, im1, im2 ) result(h)
!DEC$ ATTRIBUTES DLLEXPORT :: image_jointhistogram

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nx
integer(kind=irg),INTENT(IN)    :: ny
integer(kind=irg),INTENT(IN)    :: im1(nx, ny)
integer(kind=irg),INTENT(IN)    :: im2(nx, ny)
integer(kind=irg)               :: h(256,256)

integer(kind=irg),parameter     :: nh = 256
integer(kind=irg)               :: i, j

! initialize parameters 
h = 0

do i=1,nx
  do j=1,ny
    h(im1(i,j),im2(i,j)) = h(im1(i,j),im2(i,j))+1
  end do
end do

end function image_jointhistogram

!--------------------------------------------------------------------------
!
! FUNCTION: cumul_histogram 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the cumulative histogram of an image 
!
!> @param nx x dimension
!> @param ny y dimension
!> @param im image array; must have values in range [1..256]
! 
!> @date 01/23/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function cumul_histogram( nx, ny, im ) result(h)
!DEC$ ATTRIBUTES DLLEXPORT :: cumul_histogram

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nx
integer(kind=irg),INTENT(IN)    :: ny
integer(kind=irg),INTENT(IN)    :: im(nx, ny)
integer(kind=irg)               :: h(256)

integer(kind=irg),parameter     :: nh = 256
integer(kind=irg)               :: i, j, low, high, hst(nh), np, nploc(1)

np = nx*ny
hst = 0

! get the regular histogram
h = image_histogram( nx, ny, im )

! if all the intensities equal 0, then we need to return a zero cumulative
! histogram.
if (h(1).eq.np) then 
  h = 0
  return
end if

! we need to allow for the possibility that the image is flat, so that
! all intensity is in a single bin.  In that case, the cumulative histogram
! will be a step function and the usual noramlization to [1..256] will
! not work
if (maxval(h).eq.np) then 
  nploc = maxloc(h)
  hst(1:nploc(1)-1) = h(1)
  hst(nploc(1):nh) = 256
  h = hst
else
! convert h to a cumulative histogram hst
  hst = 0
  hst(1) = h(1)
  do i=2,nh
    hst(i) = h(i) + hst(i-1)
  end do

! and rescale to [1..256]
  h = int(255.0*((float(hst)-float(hst(1)))/float(hst(nh)-hst(1)))) + 1
end if

end function cumul_histogram

!--------------------------------------------------------------------------
!
! FUNCTION: image_entropy
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the Shannon entropy of an image 
!
!> @param h a 256 element intensity histogram
! 
!> @date 04/23/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function image_entropy( h ) result(e)
!DEC$ ATTRIBUTES DLLEXPORT :: image_entropy

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: h(256)
real(kind=sgl)                  :: e

integer(kind=irg),parameter     :: nh = 256
integer(kind=irg)               :: i, j
real(kind=sgl)                  :: hnorm(256)

! initialize parameters 
hnorm = float(h)

! normalize the histogram
hnorm = hnorm / sum(hnorm)

! add up the natural logarithm factors for the non-zero bins
e = 0.0

do i=1,nh
  if (h(i).ne.0) e = e - hnorm(i) * log(hnorm(i))
end do

end function image_entropy

!--------------------------------------------------------------------------
!
! FUNCTION: image_jointentropy
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the Shannon joint entropy of two images 
!
!> @param h a 256x256 element intensity joint histogram
! 
!> @date 04/23/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function image_jointentropy( h ) result(e)
!DEC$ ATTRIBUTES DLLEXPORT :: image_jointentropy

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: h(256,256)
real(kind=sgl)                  :: e

integer(kind=irg),parameter     :: nh = 256
integer(kind=irg)               :: i, j
real(kind=sgl)                  :: hnorm(256,256)

! initialize parameters 
hnorm = float(h)

! normalize the histogram
hnorm = hnorm / sum(hnorm)

! add up the natural logarithm factors for the non-zero bins
e = 0.0

do i=1,nh
 do j=1,nh
  if (h(i,j).ne.0) e = e - hnorm(i,j) * log(hnorm(i,j))
 end do
end do

end function image_jointentropy

!--------------------------------------------------------------------------
!
! FUNCTION: image_mutualinformation
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the mutual information of two images 
!
!> @param nx x dimension
!> @param ny y dimension
!> @param im image array; must have values in range [1..256] 
! 
!> @date 04/23/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function image_mutualinformation( nx, ny, im1, im2 ) result(mi)
!DEC$ ATTRIBUTES DLLEXPORT :: image_mutualinformation

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nx
integer(kind=irg),INTENT(IN)    :: ny
integer(kind=irg),INTENT(IN)    :: im1(nx, ny)
integer(kind=irg),INTENT(IN)    :: im2(nx, ny)
real(kind=sgl)                  :: mi

real(kind=sgl)                  :: e1, e2, je

! get the individual and joint histograms
e1 = image_entropy(image_histogram( nx, ny, im1))
e2 = image_entropy(image_histogram( nx, ny, im2))
je = image_jointentropy(image_jointhistogram( nx, ny, im1, im2))

! compute the mutual information
mi = e1 + e2 - je

end function image_mutualinformation

!--------------------------------------------------------------------------
!
! FUNCTION: adhisteq 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  adaptive histogram equalization
!
!> @details This algorithm is based on the original paper by Pizer et al., 
!> "Adaptive Histogram Equalization and its Variations", Computer Vision, 
!> Graphics, and Image Processing, 39:355-368, 1987.  
!
!> @param nr number of subregions to split the image into [10 works well]
!> @param dimx x dimension
!> @param dimy y dimension
!> @param im image array; must have integer values in range [0..255]
! 
!> @date 01/23/16 MDG 1.0 original
!> @date 01/27/16 MDG 1.1 correction of off-by-one error in final array copy
!--------------------------------------------------------------------------
recursive function adhisteq( nr, dimx, dimy, im, verbose ) result(output)
!DEC$ ATTRIBUTES DLLEXPORT :: adhisteq

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nr
integer(kind=irg),INTENT(IN)    :: dimx
integer(kind=irg),INTENT(IN)    :: dimy
integer(kind=irg),INTENT(IN)    :: im(dimx,dimy)
logical,INTENT(IN),OPTIONAL     :: verbose
integer(kind=irg)               :: output(dimx,dimy)

integer(kind=irg)               :: ts, hts, ntx, nty, i, j, i1, i2, ir, ic, istop, jstop, ix0, iy0, Tvalx, Tvaly
integer(kind=irg),parameter     :: nh=256
integer(kind=irg),allocatable   :: subim(:,:)
real(kind=sgl),allocatable      :: tmp(:), tintx(:,:), tinty(:,:), LL(:,:), LR(:,:), UL(:,:), UR(:,:) 
integer(kind=irg),allocatable   :: chistarr(:,:,:)

if (PRESENT(verbose)) write(*,*) minval(im),maxval(im)

output = 0

! determine integer parameters
ts = int(maxval( (/ dimx, dimy /)) / nr)        ! size of a single tile for histogram computation
hts = maxval( (/ ts/2, 1 /) )                   ! half the tile size
ntx = (dimx-1)/hts                              ! number of tiles along x
nty = (dimy-1)/hts                              ! number of tiles along y

! allocate temporary array to create tile interpolation matrices
allocate(tmp(hts))
tmp = (/ (dble(i), i=0,hts-1) /) / dble(hts)

allocate(tintx(hts,hts), tinty(hts,hts))
do j=1,hts
  tintx(1:hts,j) = tmp(1:hts)
end do
tinty = transpose(tintx)
deallocate(tmp)

! define the array that will hold two rows of cumulative histograms
allocate(chistarr(nh,ntx,2))
chistarr = 0

do ir = 1, nty+1
  do i = 1, ntx
    chistarr(1:256,i,1) = chistarr(1:256,i,2)  ! copy the histograms from the next row
  end do

! get the cumulative histograms for this row of tiles
  if (ir.lt.nty+1)  then
    do ic = 1, ntx
! set the sub image array limits
      ix0 = (ic-1)*hts+1
      istop = minval( (/ix0+ts-1, dimx /) ) 
      Tvalx = ts
      if (istop.eq.dimx) Tvalx = istop-ix0 + 1
      iy0 = (ir-1)*hts+1
      jstop = minval( (/iy0+ts-1, dimy /) )
      Tvaly = ts
      if (jstop.eq.dimy) Tvaly = jstop-iy0 + 1
!if (PRESENT(verbose)) write(*,*) ix0, istop, iy0, jstop, Tvalx, Tvaly

! extract the sub image and compute its cumulative histogram (histogram starts at bin 1)
      allocate(subim(Tvalx,Tvaly))
      subim = 0
      subim(1:Tvalx,1:Tvaly) = im(ix0:istop, iy0:jstop)  + 1
!if (PRESENT(verbose)) write (*,*) 'subim : ',minval(subim),maxval(subim)
      chistarr(1:nh,ic,2) = cumul_histogram( Tvalx, Tvaly, subim )
      deallocate(subim)
    end do
  end if
!if (PRESENT(verbose)) write(*,*) minval(chistarr),maxval(chistarr)
!stop

  if (ir.eq.1) then  ! this is done only the first time through...
     do i = 1, ntx
       chistarr(1:256,i,1) = chistarr(1:256,i,2)  ! copy the histograms from the next row
     end do
  end if

! and here is the actual interpolation part
  do ic = 1, ntx+1
! set the sub image array limits (different from above!)
    ix0 = (ic-1)*hts+1
    istop = minval( (/ix0+hts-1, dimx /) ) 
    Tvalx = hts
    if (istop.eq.dimx) Tvalx = istop-ix0 + 1
    iy0 = (ir-1)*hts+1
    jstop = minval( (/iy0+hts-1, dimy /) )
    Tvaly = hts
    if (jstop.eq.dimy) Tvaly = jstop-iy0 + 1

! allocate the sub image and the four interpolation arrays
    allocate(subim(Tvalx,Tvaly),LL(Tvalx,Tvaly),LR(Tvalx,Tvaly),UL(Tvalx,Tvaly),UR(Tvalx,Tvaly))

! get the sub image
    subim(1:Tvalx,1:Tvaly) = im(ix0:istop, iy0:jstop) + 1

! set the coordinates to be used in the histogram array
    i1 = maxval( (/ ic-1, 1 /) )
    i2 = minval( (/ ic, ntx /) )

! and extract the cumulative histogram values for the intensities in the sub image,
! for each of the four interpolation arrays
    do i = 1, Tvalx
      do j = 1, Tvaly
        LL(i,j) = float(chistarr(subim(i,j),i1,1))
        LR(i,j) = float(chistarr(subim(i,j),i2,1))
        UL(i,j) = float(chistarr(subim(i,j),i1,2))
        UR(i,j) = float(chistarr(subim(i,j),i2,2))
      end do
    end do

! perform the interpolation along x
    LL = LL + (LR-LL) * tintx(1:Tvalx,1:Tvaly) 
    UL = UL + (UR-UL) * tintx(1:Tvalx,1:Tvaly)
! and interpolate along y; store the result in the output array
    output(ix0:istop,iy0:jstop) = int(LL + (UL-LL) * tinty(1:Tvalx,1:Tvaly))

! deallocate the arrays 
    deallocate(subim, LL, LR, UL, UR)
  end do
end do

deallocate(tintx, tinty, chistarr)

end function adhisteq

!--------------------------------------------------------------------------
!
! SUBROUTINE: getADPmap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute Average Dot Product map, reading patterns from file unit iunit (expected to be open already)
!
!> @param iunit input file unit
!> @param nexpt number of experimental patterns
!> @param L number of pixels per pattern
!> @param wd ROI-width
!> @param ht ROI-ht
!> @param dpmap output ADP map
!
!> @date 01/09/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getADPmap(iunit, nexpt, L, wd, ht, dpmap)
!DEC$ ATTRIBUTES DLLEXPORT :: getADPmap

integer(kind=irg),INTENT(IN)        :: iunit
integer(kind=irg),INTENT(IN)        :: nexpt
integer(kind=irg),INTENT(IN)        :: L
integer(kind=irg),INTENT(IN)        :: wd
integer(kind=irg),INTENT(IN)        :: ht
real(kind=sgl),INTENT(OUT)          :: dpmap(nexpt)

integer(kind=irg)                   :: ii, iii, jj
real(kind=sgl)                      :: lstore(L,wd), pstore(L,wd), lp(L), cp(L), imageexpt(L), dp

dpmap= 0.0
pstore = 0.0
lstore = 0.0
lp = 0.0
cp = 0.0

do iii = 1,nexpt
    read(iunit,rec=iii) imageexpt
    ii = mod(iii,wd)
    if (ii.eq.0) ii = wd
    jj = iii/wd+1
! do we need to copy pstore into lstore ?
    if ((ii.eq.1).and.(jj.gt.1)) lstore = pstore
! determine to which dpmap entries we need to add the dot product
    if (ii.eq.1) then
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
    else
      lp = cp
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
      dp = sum(lp(1:L)*cp(1:L))
      dpmap(iii-1) = dpmap(iii-1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
    if (jj.gt.1) then
      dp = sum(lstore(1:L,ii)*cp(1:L))
      dpmap(iii-wd+1) = dpmap(iii-wd+1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
end do

! correct the dot product map values depending on inside, edge, or corner pixels
! divide by 4
dpmap = dpmap*0.25

! correct the straight segments
dpmap(2:wd-1) = dpmap(2:wd-1) * 4.0/3.0
dpmap(nexpt-wd+2:nexpt-1) = dpmap(nexpt-wd+2:nexpt-1) * 4.0/3.0
do jj=1,ht-2
  dpmap(wd*jj+1) = dpmap(wd*jj+1) * 4.0/3.0
end do
do jj=2,ht-1
  dpmap(wd*jj) = dpmap(wd*jj) * 4.0/3.0
end do

! and the corners
dpmap(1) = dpmap(1) * 4.0
dpmap(wd) = dpmap(wd) * 2.0
dpmap(nexpt) = dpmap(nexpt) * 2.0
dpmap(nexpt-wd+1) = dpmap(nexpt-wd+1) * 4.0/3.0

end subroutine getADPmap

!--------------------------------------------------------------------------
!
! SUBROUTINE: getADPmapRAM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute Average Dot Product map, reading patterns from file unit iunit
!
!> @param epatterns experimental patterns after preprocessing (held in RAM!)
!> @param nexpt number of experimental patterns
!> @param L number of pixels per pattern
!> @param wd ROI-width
!> @param ht ROI-ht
!> @param dpmap output ADP map
!
!> @date 01/09/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getADPmapRAM(epatterns, nexpt, cs, L, wd, ht, dpmap)
!DEC$ ATTRIBUTES DLLEXPORT :: getADPmapRAM

integer(kind=irg),INTENT(IN)        :: nexpt
integer(kind=irg),INTENT(IN)        :: cs
real(kind=sgl),INTENT(IN)           :: epatterns(cs,nexpt)
integer(kind=irg),INTENT(IN)        :: L
integer(kind=irg),INTENT(IN)        :: wd
integer(kind=irg),INTENT(IN)        :: ht
real(kind=sgl),INTENT(OUT)          :: dpmap(nexpt)

integer(kind=irg)                   :: ii, iii, jj
real(kind=sgl)                      :: lstore(L,wd), pstore(L,wd), lp(L), cp(L), imageexpt(L), dp

dpmap= 0.0
pstore = 0.0
lstore = 0.0
lp = 0.0
cp = 0.0

do iii = 1,nexpt
    imageexpt = epatterns(1:L,iii)
    ii = mod(iii,wd)
    if (ii.eq.0) ii = wd
    jj = iii/wd+1
! do we need to copy pstore into lstore ?
    if ((ii.eq.1).and.(jj.gt.1)) lstore = pstore
! determine to which dpmap entries we need to add the dot product
    if (ii.eq.1) then
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
    else
      lp = cp
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
      dp = sum(lp(1:L)*cp(1:L))
      dpmap(iii-1) = dpmap(iii-1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
    if (jj.gt.1) then
      dp = sum(lstore(1:L,ii)*cp(1:L))
      dpmap(iii-wd+1) = dpmap(iii-wd+1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
end do

! correct the dot product map values depending on inside, edge, or corner pixels
! divide by 4
dpmap = dpmap*0.25

! correct the straight segments
dpmap(2:wd-1) = dpmap(2:wd-1) * 4.0/3.0
dpmap(nexpt-wd+2:nexpt-1) = dpmap(nexpt-wd+2:nexpt-1) * 4.0/3.0
do jj=1,ht-2
  dpmap(wd*jj+1) = dpmap(wd*jj+1) * 4.0/3.0
end do
do jj=2,ht-1
  dpmap(wd*jj) = dpmap(wd*jj) * 4.0/3.0
end do

! and the corners
dpmap(1) = dpmap(1) * 4.0
dpmap(wd) = dpmap(wd) * 2.0
dpmap(nexpt) = dpmap(nexpt) * 2.0
dpmap(nexpt-wd+1) = dpmap(nexpt-wd+1) * 4.0/3.0

end subroutine getADPmapRAM


!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcHoughLUT
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  create a look-up table for a square Hough transform
!
!> @param dimx x dimension
!> @param LUT look-up table, allocated in calling program
! 
!> @date 02/02/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcHoughLUT( dimx, LUT ) 
!DEC$ ATTRIBUTES DLLEXPORT :: CalcHoughLUT

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dimx
real(kind=sgl),INTENT(OUT)              :: LUT(dimx*dimx,dimx)


real(kind=sgl)                          :: ct(dimx), st(dimx), pre, line(dimx), d2, x, y
integer(kind=irg)                       :: i, j, k, icnt

d2 = float(dimx/2)+0.5

! trigonometric look-up tables
ct = 0.0
st = 0.0
pre = sngl(cPi)/float(dimx)
line = (/ (i,i=0,dimx-1) /) * pre
ct = cos(line)
st = sin(line)

! and create the look up table
LUT = 0.0
icnt = 1
do j = 1,dimx
  y = float(j)-d2
  do i = 1,dimx
    x = float(i)-d2
    LUT(icnt,:) = nint(x*ct(:)+y*st(:)) 
    icnt = icnt + 1
  end do
end do

LUT = LUT + dimx/2 + 1

! this is here just so the ped indexing program can be tested out. 
! the out of bounds error in the main chunk of the program should be corrected

do i = 1,dimx*dimx
    do j = 1,dimx
        if (LUT(i,j) .gt. dimx) LUT(i,j) = dimx
        if (LUT(i,j) .lt. 1.0) LUT(i,j) = 1.0
    end do
end do

end subroutine CalcHoughLUT

!--------------------------------------------------------------------------
!
! SUBROUTINE: HoughTransform
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Perform a square Hough transform
!
!> @param dimx x dimension
!> @param LUT look-up table from CalcHoughLUT
!> @param im image array (must be mean-subtracted)
!> @param HT resulting Hough transform array
! 
!> @date 02/02/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine HoughTransform( dimx, LUT, im, HT ) 
!DEC$ ATTRIBUTES DLLEXPORT :: HoughTransform

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dimx
real(kind=sgl),INTENT(IN)               :: LUT(dimx*dimx,dimx)
real(kind=sgl),INTENT(IN)               :: im(dimx*dimx)
real(kind=sgl),INTENT(OUT)              :: HT(dimx,dimx)

integer(kind=irg)                       :: i, j, k, d2

HT = 0.0
d2 = dimx/2

do j = 1,dimx
  do i = 1, dimx*dimx
    k = LUT(i,j)
    HT(k,j) = HT(k,j) + im(i)
  end do
end do

HT = transpose(HT)

end subroutine HoughTransform

!--------------------------------------------------------------------------
!
! SUBROUTINE: HiPassFilter
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Perform a high pass filter
!
!> @param rdata real data to be transformed
!> @param dims dimensions of rdata array
!> @param w width of Gaussian profile
!> @param init (optional) initialize without computing anything
!> @param destroy (optional) destroy fft plans
! 
!> @date 02/02/16 MDG 1.0 original
!> @date 06/03/16 MDG 1.1 modified mask to inverted Gaussian profile; added init optional parameter
!--------------------------------------------------------------------------
recursive function HiPassFilter(rdata,dims,w,init,destroy) result(fdata)
!DEC$ ATTRIBUTES DLLEXPORT :: HiPassFilter

use FFTW3mod

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dims(2)
real(kind=dbl),INTENT(IN)               :: w
real(kind=dbl),INTENT(IN)               :: rdata(dims(1),dims(2))
logical,INTENT(IN),OPTIONAL             :: init
logical,INTENT(IN),OPTIONAL             :: destroy
real(kind=dbl)                          :: fdata(dims(1),dims(2))

complex(kind=dbl),SAVE,allocatable      :: hpmask(:,:)
complex(kind=dbl)                       :: cone = cmplx(1.D0,0.D0), czero = cmplx(0.D0,0.D0)
integer(kind=irg)                       :: i, j, k, ii, jj
real(kind=dbl)                          :: x, y, val

! fftw variables
type(C_PTR),SAVE                        :: planf, planb
complex(C_DOUBLE_COMPLEX),SAVE,allocatable :: inp(:,:), outp(:,:)

! are we just destroying the fftw plans ?
if (present(destroy)) then
  if (destroy) then
    deallocate(hpmask, inp, outp)
    call fftw_destroy_plan(planf)
    call fftw_destroy_plan(planb)
    fdata = 0.D0
    return
  end if
end if

! if init=.TRUE. then initialize the hpmask variable and the fftw plans
if (present(init)) then
  if (init) then
! allocate arrays
    allocate(hpmask(dims(1),dims(2)), inp(dims(1),dims(2)), outp(dims(1),dims(2)))

! generate the complex inverted Gaussian mask; w = 0.05 produces good results (usually)
  do i=1,dims(1)/2 
    x = dble(i)**2
    do j=1,dims(2)/2
      y = dble(j)**2
      if ((x+y).lt.30.D0) then
        val = 1.D0-dexp(-w*(x+y))
        hpmask(i,j) = cmplx(val, 0.D0)
        hpmask(dims(1)+1-i,j) = cmplx(val, 0.D0)
        hpmask(i,dims(2)+1-j) = cmplx(val, 0.D0)
        hpmask(dims(1)+1-i,dims(2)+1-j) = cmplx(val, 0.D0)
        fdata(i,j) = val
        fdata(dims(1)+1-i,j) = val
        fdata(i,dims(2)+1-j) = val
        fdata(dims(1)+1-i,dims(2)+1-j) = val
      end if
    end do
  end do

! then we set up the fftw plans for forward and reverse transforms
    planf = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_FORWARD, FFTW_ESTIMATE)
    planb = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_BACKWARD, FFTW_ESTIMATE)

! and return
    return
  end if
end if

! apply the hi-pass mask to rdata
do j=1,dims(1)
 do k=1,dims(2)
  inp(j,k) = cmplx(rdata(j,k),0.D0)    
 end do
end do
call fftw_execute_dft(planf, inp, outp)
inp = outp * hpmask
call fftw_execute_dft(planb, inp, outp) 
fdata(1:dims(1),1:dims(2)) = real(outp)

call fftw_cleanup()

end function HiPassFilter

!--------------------------------------------------------------------------
!
! SUBROUTINE: init_HiPassFilter
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Perform a high pass filter
!
!> @param rdata real data to be transformed
!> @param dims dimensions of rdata array
!> @param w width of Gaussian profile
!> @param init (optional) initialize without computing anything
! 
!> @date 02/02/16 MDG 1.0 original
!> @date 06/03/16 MDG 1.1 modified mask to inverted Gaussian profile; added init optional parameter
!> @date 01/11/18 MDG 1.2 split routine from original to allow for OpenMP access
!> @date 07/14/20 MDG 1.3 commented out fftw_cleanup; possibly caused issue with ifort compiler
!--------------------------------------------------------------------------
recursive subroutine init_HiPassFilter(w, dims, hpmask, inp, outp, planf, planb) 
!DEC$ ATTRIBUTES DLLEXPORT :: init_HiPassFilter

use FFTW3mod

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: w
integer(kind=irg),INTENT(IN)            :: dims(2)
complex(kind=dbl),INTENT(INOUT)         :: hpmask(dims(1),dims(2))
!f2py intent(in,out) ::  hpmask
complex(C_DOUBLE_COMPLEX),pointer,INTENT(INOUT) :: inp(:,:), outp(:,:)
!f2py intent(in,out) ::  inp
type(C_PTR),INTENT(INOUT)               :: planf, planb
!f2py intent(in,out) ::  planf, planb

integer(kind=irg)                       :: i, j
real(kind=dbl)                          :: x, y, v2
complex(kind=dbl)                       :: val

hpmask = cmplx(1.D0,0.D0)

! generate the complex inverted Gaussian mask; w = 0.05 produces good results (usually)
do i=1,dims(1)/2 
  x = dble(i)**2
  do j=1,dims(2)/2
    y = dble(j)**2
    v2 = w * ( x+y )
    if (v2.lt.30.D0) then
      val = cmplx(1.D0-dexp(-v2), 0.D0)
      hpmask(i,j) = val
      hpmask(dims(1)+1-i,j) = val
      hpmask(i,dims(2)+1-j) = val
      hpmask(dims(1)+1-i,dims(2)+1-j) = val
    end if
  end do
end do

! then we set up the fftw plans for forward and reverse transforms
planf = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_FORWARD, FFTW_ESTIMATE)
planb = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_BACKWARD, FFTW_ESTIMATE)

! call fftw_cleanup()

end subroutine init_HiPassFilter

!--------------------------------------------------------------------------
!
! SUBROUTINE: applyHiPassFilter
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Perform a high pass filter
!
!> @param rdata real data to be transformed
!> @param dims dimensions of rdata array
!> @param w width of Gaussian profile
! 
!> @date 02/02/16 MDG 1.0 original
!> @date 06/03/16 MDG 1.1 modified mask to inverted Gaussian profile
!> @date 01/11/18 MDG 1.2 split routine from original to allow for OpenMP access
!> @date 07/14/20 MDG 1.3 commented out fftw_cleanup
!--------------------------------------------------------------------------
recursive function applyHiPassFilter(rdata, dims, w, hpmask, inp, outp, planf, planb) result(fdata)
!DEC$ ATTRIBUTES DLLEXPORT :: applyHiPassFilter

use FFTW3mod

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dims(2)
real(kind=dbl),INTENT(IN)               :: w
real(kind=dbl),INTENT(IN)               :: rdata(dims(1),dims(2))
complex(kind=dbl),INTENT(IN)            :: hpmask(dims(1),dims(2))
complex(C_DOUBLE_COMPLEX),pointer,INTENT(INOUT) :: inp(:,:), outp(:,:)
!f2py intent(in,out) ::  inp
type(C_PTR),INTENT(IN)                  :: planf, planb
real(kind=dbl)                          :: fdata(dims(1),dims(2))

integer(kind=irg)                       :: j, k

! apply the hi-pass mask to rdata
do j=1,dims(1)
 do k=1,dims(2)
  inp(j,k) = cmplx(rdata(j,k),0.D0)    
 end do
end do
call fftw_execute_dft(planf, inp, outp)
inp = outp * hpmask
call fftw_execute_dft(planb, inp, outp) 
fdata(1:dims(1),1:dims(2)) = real(outp)

! call fftw_cleanup()

end function applyHiPassFilter

!--------------------------------------------------------------------------
!
! SUBROUTINE: HiPassFilterC
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Perform a high pass filter, callable from C
!
!> @param rdata real data to be transformed
!> @param dims dimensions of rdata array
!> @param w width of Gaussian profile
!> @param init initialize without computing anything
!> @param destroy destroy fft plans
!> @param fdata output data
! 
!> @date 05/17/17 MDG 1.0 original, taken from regular routine above
!--------------------------------------------------------------------------
recursive subroutine HiPassFilterC(rdata,dims,w,init,destroy,fdata) bind(c, name='HiPassFilterC')
!DEC$ ATTRIBUTES DLLEXPORT :: HiPassFilterC

use FFTW3mod
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(c_int32_t),INTENT(IN)           :: dims(2)
real(C_DOUBLE),INTENT(IN)               :: w
real(C_DOUBLE),INTENT(IN)               :: rdata(dims(1),dims(2))
logical(C_BOOL),INTENT(IN)              :: init
logical(C_BOOL),INTENT(IN)              :: destroy
real(C_DOUBLE)                          :: fdata(dims(1),dims(2))

complex(C_DOUBLE_COMPLEX),SAVE,allocatable      :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX)                       :: cone = cmplx(1.D0,0.D0), czero = cmplx(0.D0,0.D0)
integer(c_int32_t)                      :: i, j, k, ii, jj
real(C_DOUBLE)                          :: x, y, val

! fftw variables
type(C_PTR),SAVE                        :: planf, planb
complex(C_DOUBLE_COMPLEX),SAVE,allocatable :: inp(:,:), outp(:,:)

! are we just destroying the fftw plans ?
if (destroy.eqv..TRUE.) then
    deallocate(hpmask, inp, outp)
    call fftw_destroy_plan(planf)
    call fftw_destroy_plan(planb)
    fdata = 0.D0
end if

! if init=.TRUE. then initialize the hpmask variable and the fftw plans
if (init.eqv..TRUE.) then
! allocate arrays
    allocate(hpmask(dims(1),dims(2)), inp(dims(1),dims(2)), outp(dims(1),dims(2)))

! generate the complex inverted Gaussian mask; w = 0.05 produces good results (usually)
    do i=1,dims(1)/2 
      x = float(i)
      do j=1,dims(2)/2
        y = float(j)
        val = 1.D0-dexp(-w*(x*x+y*y))
        hpmask(i,j) = cmplx(val, 0.D0)
        hpmask(dims(1)+1-i,j) = cmplx(val, 0.D0)
        hpmask(i,dims(2)+1-j) = cmplx(val, 0.D0)
        hpmask(dims(1)+1-i,dims(2)+1-j) = cmplx(val, 0.D0)
        fdata(i,j) = val
        fdata(dims(1)+1-i,j) = val
        fdata(i,dims(2)+1-j) = val
        fdata(dims(1)+1-i,dims(2)+1-j) = val
      end do
   end do

! then we set up the fftw plans for forward and reverse transforms
    planf = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_FORWARD, FFTW_ESTIMATE)
    planb = fftw_plan_dft_2d(dims(2),dims(1),inp,outp, FFTW_BACKWARD, FFTW_ESTIMATE)
end if

! apply the hi-pass mask to rdata
if ((destroy.eqv..FALSE.).and.(init.eqv..FALSE.)) then
  do j=1,dims(1)
   do k=1,dims(2)
    inp(j,k) = cmplx(rdata(j,k),0.D0)    
   end do
  end do
  call fftw_execute_dft(planf, inp, outp)
  inp = outp * hpmask
  call fftw_execute_dft(planb, inp, outp) 
  fdata(1:dims(1),1:dims(2)) = real(outp)
endif

call fftw_cleanup()

end subroutine HiPassFilterC

!--------------------------------------------------------------------------
!
! SUBROUTINE: ButterflyMask9x9
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief apply 9x9 butterfly mask to the hough image
!
!> @param input input image
!> @param output output image
!> @param dims dimension of these images
! 
!> @date 12/06/16 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ButterflyMask9x9(input, output, dims)
!DEC$ ATTRIBUTES DLLEXPORT :: ButterflyMask9x9

use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)           :: dims
real(kind=sgl),INTENT(IN)              :: input(dims, dims)
real(kind=sgl),INTENT(OUT)             :: output(dims, dims)

integer(kind=irg)                      :: ii, jj, kk, ll
real(kind=sgl),allocatable             :: inputpadded(:,:)

allocate(inputpadded(-3:dims+4,-3:dims+4))
inputpadded = 0.0
inputpadded(1:dims,1:dims) = input(1:dims,1:dims)

!inputpadded(-3:0,:1:dims) = input(dims-3:dims,1:dims)
!inputpadded(dims+1:dims+4,1:dims) = input(1:4,1:dims)

!inputpadded(1:dims,-3:0) = inputpadded(1:dims,dims-3:dims)
!inputpadded(1:dims,dims+1:dims+4) = inputpadded(1:dims,1:4)

output = 0.0

do ii = 1,dims
    do jj = 1,dims
        do kk = -4,4
            do ll = -4,4
                output(ii,jj) = output(ii,jj) + inputpadded(ii+kk,jj+ll)*Butterfly9x9((kk+4)*9+ll+5)
            end do
        end do                
    end do
end do

deallocate(inputpadded)

end subroutine ButterflyMask9x9

!--------------------------------------------------------------------------
!
! SUBROUTINE: InversionDivision
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Inversion division operation to enhance contrast of hough image 
!
!> @param input input image; must be between 0-1
!> @param output output image
!> @param dims dimension of these image
! 
!> @date 12/06/16 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine InversionDivision(input, output, dims)
!DEC$ ATTRIBUTES DLLEXPORT :: InversionDivision

IMPLICIT NONE

integer(kind=irg),INTENT(IN)           :: dims
real(kind=sgl),INTENT(IN)              :: input(dims, dims)
real(kind=sgl),INTENT(OUT)             :: output(dims, dims)

integer(kind=irg)                      :: ii, jj
real(kind=sgl),allocatable             :: inverse(:,:)
real(kind=sgl)                         :: ma, mi

allocate(inverse(dims, dims))

inverse = (1.0 - Input)

do ii = 1,dims
    do jj = 1,dims
        if(inverse(ii,jj) .ne. 0.0) then
            output(ii,jj) = input(ii,jj)/inverse(ii,jj)
        else
            output(ii,jj) = 1.0
        end if
    end do
end do

deallocate(inverse)

ma = maxval(output)
mi = minval(output)

output = (output - mi)/(ma - mi)

end subroutine InversionDivision

end module filters
