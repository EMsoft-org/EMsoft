
module commonmod

use local
use typedefs
use stringconstants

!abstract interface
!        subroutine func (nipar, nfpar, ninit, ipar, fpar, initmeanval, expt, n, x, f, fname)  !! calfun interface

!            use local

!            implicit none

!            integer(8),intent(in)                :: ipar(nipar)
!            real(sgl),intent(inout)              :: fpar(nfpar)
!            real(sgl),intent(in)                 :: initmeanval(ninit)
!            integer(irg),intent(in)              :: nipar
!            integer(irg),intent(in)              :: nfpar
!            integer(irg),intent(in)              :: ninit
!            real(kind=sgl),intent(in)            :: expt(ipar(2)*ipar(3))
!            integer(irg),intent(in)              :: n
!            real(dbl),dimension(:),intent(in)    :: x
!            real(dbl),intent(out)                :: f
!            character(fnlen),intent(in),optional :: fname(2)

!    end subroutine func
!end interface


contains


!--------------------------------------------------------------------------
!
! FUNCTION: init_getEBSDIQ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize variables for the EBSD Image Quality using the second moment of the power spectrum
!
!> @details This is based on Krieger Lassen's pattern sharpness definition: Q = 1 - J / Jres wtot
!> more details page 93 of thesis of Farangis Ram.
!
!> @param dimx x pattern dimension
!> @param dimy y pattern dimension
!> @param pattern input EBSD pattern
!
!> @date 01/10/18 MDG 1.0 original, based on getEBSDIQ routine, but only init part
!--------------------------------------------------------------------------
recursive subroutine init_getEBSDIQ(dimx, dimy, pattern, ksqarray, Jres, planf) 
!DEC$ ATTRIBUTES DLLEXPORT :: init_getEBSDIQ

use local
use typedefs
use FFTW3mod

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dimx
integer(kind=irg),INTENT(IN)            :: dimy
real(kind=sgl),INTENT(IN)               :: pattern(dimx,dimy)
real(kind=dbl),INTENT(OUT)              :: ksqarray(dimx,dimy)
real(kind=dbl),INTENT(OUT)              :: Jres
type(C_PTR),INTENT(OUT)                 :: planf

complex(C_DOUBLE_COMPLEX),pointer       :: inp(:,:)
complex(C_DOUBLE_COMPLEX),pointer       :: outp(:,:)
type(C_PTR)                             :: p, o
real(kind=dbl)                          :: linex(dimx), liney(dimy)
integer(kind=irg)                       :: i

p = fftw_alloc_complex(int(dimx*dimy,C_SIZE_T))
call c_f_pointer(p, inp, [dimx,dimy])

o = fftw_alloc_complex(int(dimx*dimy,C_SIZE_T))
call c_f_pointer(o, outp, [dimx,dimy])

inp = cmplx(0.D0,0D0)
outp = cmplx(0.D0,0.D0)

! set up the fftw plan for the forward transform
planf = fftw_plan_dft_2d(dimy,dimx,inp,outp, FFTW_FORWARD,FFTW_ESTIMATE)

! generate the parameter/array needed by the getEBSDIQ function
ksqarray = 0.D0
Jres = 0.D0

linex = (/ (dble(i),i=0,dimx-1) /) 
linex(dimx/2+1:dimx) = linex(dimx/2+1:dimx) - dble(dimx)
linex = linex**2
liney = (/ (dble(i),i=0,dimy-1) /) 
liney(dimy/2+1:dimy) = liney(dimy/2+1:dimy) - dble(dimy)
liney = liney**2

do i=1,dimx
    ksqarray(i,1:dimy) = linex(i) + liney(1:dimy)
end do
Jres = sum(ksqarray) / dble(dimx) / dble(dimy)

call fftw_free(p)
call fftw_free(o)
call fftw_cleanup()

end subroutine init_getEBSDIQ


!--------------------------------------------------------------------------
!
! FUNCTION: computeEBSDIQ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the EBSD Image Quality using the second moment of the power spectrum
!
!> @details This is based on Krieger Lassen's pattern sharpness definition: Q = 1 - J / Jres wtot
!> more details page 93 of thesis of Farangis Ram.
!
!> @param dimx x pattern dimension
!> @param dimy y pattern dimension
!> @param pattern input EBSD pattern
!
!> @date 02/07/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function computeEBSDIQ(dimx, dimy, pattern, ksqarray, Jres, planf) result(Q)
!DEC$ ATTRIBUTES DLLEXPORT :: computeEBSDIQ


use local
use typedefs
use FFTW3mod

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: dimx
integer(kind=irg),INTENT(IN)            :: dimy
real(kind=sgl),INTENT(IN)               :: pattern(dimx,dimy)
real(kind=dbl),INTENT(IN)               :: ksqarray(dimx,dimy)
real(kind=dbl),INTENT(IN)               :: Jres
type(C_PTR),INTENT(IN)                  :: planf
real(kind=dbl)                          :: Q


real(kind=dbl)                          :: J, wtot
complex(C_DOUBLE_COMPLEX),pointer       :: inp(:,:)
complex(C_DOUBLE_COMPLEX),pointer       :: outp(:,:)
type(C_PTR)                             :: p, o
real(kind=dbl)                          :: w(dimx,dimy), linex(dimx), liney(dimy)
integer(kind=irg)                       :: i

p = fftw_alloc_complex(int(dimx*dimy,C_SIZE_T))
call c_f_pointer(p, inp, [dimx,dimy])

o = fftw_alloc_complex(int(dimx*dimy,C_SIZE_T))
call c_f_pointer(o, outp, [dimx,dimy])

inp = pattern
outp = cmplx(0.D0,0.D0)

! compute the Fourier transform
call fftw_execute_dft(planf, inp, outp)

w = sqrt(real(outp)**2 + aimag(outp)**2)

! sum over the arrays
J = sum(w*ksqarray)
wtot = sum(w)

! and return the quality parametere
Q = 1.0 - J/Jres/wtot

call fftw_free(p)
call fftw_free(o)
call fftw_cleanup()

end function computeEBSDIQ

!--------------------------------------------------------------------------
!
! SUBROUTINE: EBSDgetAverageOrientations
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Use the top near-matches list to compute the averaged orientations
!
!> @param ipar array of integers
!> @param Eulers array of dictionary Euler angle triplets
!> @param tmi top match index array
!> @param dplist dot product list
!> @param 
!> @param avEuler (returned) resulting averaged orientations array
!> @param disorient (optional) array of disorientation angles for top matches
!
!> @date 07/06/16 MDG 1.0 original
!> @date 09/06/16 MDG 1.1 correction of issue where the acos() function returned NAN
!--------------------------------------------------------------------------
recursive subroutine EBSDgetAverageOrientations(ipar, Eulers, tmi, dplist, avEuler, disorient)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDgetAverageOrientations

use dictmod
use rotations
use quaternions
use constants

IMPLICIT NONE

! ipar(1) = pgnum
! ipar(2) = FZcnt
! ipar(3) = Nexp
! ipar(4) = nnk
! ipar(5) = indi (=Ne*ceiling(float(totnumexpt)/float(Ne)))
! ipar(6) = nmuse

integer(kind=irg),INTENT(IN)            :: ipar(6)
real(kind=sgl),INTENT(IN)               :: Eulers(3,ipar(2))
integer(kind=irg),INTENT(IN)            :: tmi(ipar(4),ipar(5))
real(kind=irg),INTENT(IN)               :: dplist(ipar(4),ipar(5))
real(kind=sgl),INTENT(OUT)              :: avEuler(3,ipar(3))
real(kind=sgl),INTENT(OUT),OPTIONAL     :: disorient(ipar(3),ipar(6))

type(dicttype),pointer                  :: dict
integer(kind=irg)                       :: Nexp, nnm, nnk, Pmdims, i, j, k, ipat, pgnum, FZcnt, indi, nmuse
real(kind=sgl)                          :: ax(4), accum(3), q1(4), q2(4), qus(4), a, oldmo, p(4), qsmall(4), theta, vec(3)

real(kind=sgl),allocatable              :: logq(:,:), w(:)
integer(kind=irg),allocatable           :: EulerIDs(:)
logical                                 :: store

pgnum = ipar(1)
FZcnt = ipar(2)
Nexp = ipar(3)
nnk = ipar(4)
indi = ipar(5)
nmuse = ipar(6)

store = .FALSE.
if (PRESENT(disorient)) store = .TRUE.

!===================================
! set up the symmetry quaternions for this rotational symmetry
! allocate the dict structure
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum
! initialize the symmetry matrices
call DI_Init(dict,'nil') 
Pmdims = dict%Nqsym
!write (*,*) 'number of symmetry operators : ',Pmdims
!do i=1,Pmdims
!    write (*,*) dict%Pm(1:4,i)
!end do

!===================================
! ok, so now we have all the necessary data
! next, we convert all the dictionary Euler angles into axis-angle triplets 
! but with half the angle so that they become the logarithm of the corresponding 
! quaternions (we omit the scalar part which is always zero)
allocate(logq(3,FZcnt))
do i=1,FZcnt
  ax = eu2ax(Eulers(1:3,i))
  logq(1:3,i) = ax(1:3) * ax(4) * 0.5
end do

! next, loop over all the experimental points, determine the weight factors
! and perform the averaging over those symmetrically equivalent orientations
! that have the smallest possible misorientation with respect to the best match.
allocate(EulerIDs(nmuse),w(nmuse))

do i=1,Nexp
! get the Euler angle IDs in the short list
  EulerIDs(1:nmuse) = tmi(1:nmuse,i)

! get the weight factors and normalize them to sum to 1
  w(1:nmuse) = dplist(1:nmuse,i)
  w = w - w(nmuse)
  w = w/sum(w)

! for each orientation in the list, determine the symmetrycally equivalent one
! that has the smallest misorientation with respect to the first entry on the list
! and add its weighted logarithm to the accum sum
  q1 = eu2qu(Eulers(1:3,EulerIDs(1)))
  accum = logq(1:3,EulerIDs(1)) * w(1)
  do j=2,nmuse!  -1   ! -1 because the last one has weight factor 0
    q2 = eu2qu(Eulers(1:3,EulerIDs(j)))
    oldmo = 10.0
! determine the orientation with the smallest misorientation w.r.t. q1 and store it in qsmall
    do k=1,Pmdims
      qus = quat_mult(dict%Pm(1:4,k),dble(q2))
      if (qus(1).lt.0.0) qus=-qus
      p = quat_mult(q1,conjg(qus))
      if (p(1).lt.0.0) p=-p
      if (p(1).gt.1.0) p(1)=1.0
      a = 2.0*acos(p(1))
      if (a.lt.oldmo) then
        oldmo = a
        qsmall = qus
      end if
      p = quat_mult(qus,conjg(q1))
      if (p(1).lt.0.0) p=-p
      if (p(1).gt.1.0) p(1)=1.0
      a = 2.0*acos(p(1))
      if (a.lt.oldmo) then
        oldmo = a
        qsmall = qus
      end if
    end do
    if (store.eqv..TRUE.) disorient(i,j) = oldmo
! take the logarithm of qsmall and add it with the appropriate weight factor to accum
    ax = qu2ax(qsmall)
    accum(1:3) = accum(1:3) + ax(1:3) * ax(4) * 0.5 * w(j)
  end do
  
! accum is now the logarithm of the desired orientation quaternion, so we need to convert
! this back to an Euler angle triplet
  theta = sqrt(sum(accum**2))
  theta = mod(theta, 2.0*sngl(cPi))
  if (theta.ne.0.0) then
    vec = accum/theta
  else 
    vec = (/ 0.0, 0.0, 1.0 /)
  end if
  ax(1:3) = vec(1:3)
  ax(4) = theta  * 2.0
  avEuler(1:3,i) = ax2eu(ax)
end do

! and put this array back in degrees
avEuler = avEuler *180.0/sngl(cPi)

end subroutine EBSDgetAverageOrientations

!--------------------------------------------------------------------------
!
! SUBROUTINE: EBSDgetOrientationSimilarityMap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the OSM (Orientation Similarity Map) given a set of near matches
!
!> @param idims dimensions of TopMatchIndices (tmi) array
!> @param tmi Top Match Indices array
!> @param nm number of matches to use for OSM
!> @param ipf_wd width of the ROI
!> @param ipf_ht height of the ROI
!> @param osm (returned) Orientation Similarity Map (1D array)
!
!> @date 07/28/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDgetOrientationSimilarityMap(idims, tmi, nm, ipf_wd, ipf_ht, osm)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDgetOrientationSimilarityMap

use math
use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)     :: idims(2)
integer(kind=irg),INTENT(IN)     :: tmi(idims(1),idims(2))
integer(kind=irg),INTENT(IN)     :: nm
integer(kind=irg),INTENT(IN)     :: ipf_wd
integer(kind=irg),INTENT(IN)     :: ipf_ht
real(kind=sgl),INTENT(OUT)       :: osm(ipf_wd,ipf_ht)

real(kind=sgl),allocatable       :: localosm(:)
integer(kind=irg),allocatable    :: lstore(:,:), pstore(:,:), cp(:), lp(:)
integer(kind=irg)                :: ii, jj, iii , dp, lnm, io_int(2), totnumexpt

osm = 0.0
totnumexpt = ipf_wd * ipf_ht

! make sure that the requested number of near-matches is smaller than/equal to the available number
if (nm.gt.idims(1)) then
  io_int(1) = nm
  io_int(2) = idims(1)
  call WriteValue('Requested number of near matches is too large: ',io_int,2,"(I4,' > ',I4)")
  call Message(' --> Resetting requested number to maximum available')
  lnm = idims(1)
else
  lnm = nm
end if

allocate(lstore(lnm, ipf_wd), pstore(lnm, ipf_wd), cp(lnm), lp(lnm), localosm(ipf_wd*ipf_ht))

localosm = 0.0
lstore = 0
pstore = 0
cp = 0
lp = 0

! we'll do this computation on the 1D array, in the same way 
! as the ADP (Average Dot Product) map in the EBSDDI.f90 program
do iii = 1,totnumexpt
    ii = mod(iii,ipf_wd)
    if (ii.eq.0) ii = ipf_wd
    jj = iii/ipf_wd+1
! do we need to copy pstore into lstore ?
    if ((ii.eq.1).and.(jj.gt.1)) lstore = pstore
! determine to which osm entries we need to add the similarity count
    if (ii.eq.1) then
      cp(1:lnm) = tmi(1:lnm, iii)
      pstore(1:lnm,ii) = cp(1:lnm)
    else
      lp = cp
      cp(1:lnm) = tmi(1:lnm, iii)
      pstore(1:lnm,ii) = cp(1:lnm)
      dp = vectormatch(lnm, cp, lp)
      localosm(iii-1) = localosm(iii-1) + dp
      localosm(iii) = localosm(iii) + dp
    end if
    if (jj.gt.1) then
      dp = vectormatch(lnm,lstore(1:lnm,ii),cp)
      localosm(iii-ipf_wd+1) = localosm(iii-ipf_wd+1) + dp
      localosm(iii) = localosm(iii) + dp
    end if
end do

! correct the osm values depending on inside, edge, or corner pixels
! divide by 4
localosm = localosm*0.25

! correct the straight segments  THIS NEEDS TO BE VERIFIED !!!!!
localosm(2:ipf_wd-1) = localosm(2:ipf_wd-1) * 4.0/3.0
localosm(totnumexpt-ipf_wd+2:totnumexpt-1) = localosm(totnumexpt-ipf_wd+2:totnumexpt-1) * 4.0/3.0
do jj=1,ipf_ht-2
  localosm(ipf_wd*jj+1) = localosm(ipf_wd*jj+1) * 4.0/3.0
end do
do jj=2,ipf_ht-1
  localosm(ipf_wd*jj) = localosm(ipf_wd*jj) * 4.0/3.0
end do

! and the corners
localosm(1) = localosm(1) * 4.0
localosm(ipf_wd) = localosm(ipf_wd) * 2.0
localosm(totnumexpt) = localosm(totnumexpt) * 2.0
localosm(totnumexpt-ipf_wd+1) = localosm(totnumexpt-ipf_wd+1) * 4.0/3.0

! and we deallocate the auxiliary variables 
deallocate(lstore,pstore,lp,cp)

do ii=1,ipf_wd
  do jj=1,ipf_ht
    osm(ii,jj) = localosm(ipf_wd*(jj-1)+ii)
  end do
end do

end subroutine EBSDgetOrientationSimilarityMap

!--------------------------------------------------------------------------
!
! SUBROUTINE: EBSDgetIndexingSuccessMap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the ISM (Indexing Success Map) given a set of near matches
!
!> @param idims dimensions of TopMatchIndices (tmi) array
!> @param tmi Top Match Indices array
!> @param eq Euler angles array
!> @param ebsdnl namelist
!> @param ism (returned) Indexing Success Rate Map 
!
!> @date 06/27/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDgetIndexingSuccessMap(ipar, tmi, ea, ebsdnl, ism)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDgetIndexingSuccessMap

use NameListTypedefs
use omp_lib
use io
use dictmod
use constants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                :: ipar(10)
integer(kind=irg),INTENT(IN)                :: tmi(ipar(1),ipar(2))
real(kind=sgl),INTENT(INOUT)                :: ea(3,ipar(4))
!f2py intent(in,out) ::  ea
type(EBSDIndexingNameListType),INTENT(IN)   :: ebsdnl
real(kind=sgl),INTENT(OUT)                  :: ism(ipar(7)*ipar(8))

integer(kind=irg)                           :: io_int(2), lnism, i, j
real(kind=sgl),allocatable                  :: angles(:)
real(kind=sgl)                              :: angle
type(dicttype),pointer                      :: dict

ism = 0.0

! make sure that the requested number of near-matches is smaller than/equal to the available number
if (ebsdnl%nism.gt.ebsdnl%nnk-1) then
  io_int(1) = ebsdnl%nism
  io_int(2) = ebsdnl%nnk-1
  call WriteValue('Requested number of near matches is too large: ',io_int,2,"(I4,' > ',I4)")
  call Message(' --> Resetting requested number to maximum available')
  lnism = ebsdnl%nnk-1
else
  lnism = ebsdnl%nism
end if

! set up the correct symmetry variables 
nullify(dict)
allocate(dict)
dict%Num_of_init = 1
dict%Num_of_iterations = 2
dict%pgnum = ipar(6)
call DI_Init(dict,'nil') 

! next we go through the entire list of points in tmi and compute the misorientation angle
! for the best match with respect to the next nism matches

! this should be done in parallel ... 
call OMP_SET_NUM_THREADS(ebsdnl%nthreads)
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,angles,angle)
allocate(angles(lnism))
!$OMP DO SCHEDULE(DYNAMIC)
do i=1,ipar(3)
  do j=2,lnism+1
    call getDisorientationAngle(ea(1:3,tmi(1,i)), ea(1:3,tmi(j,i)), dict, angle)
    angles(j-1) = angle
  end do
  ism(i) = minval(angles)
end do
!$OMP END DO
deallocate(angles)
!$OMP END PARALLEL

ism = ism * 180.0/sngl(cPi)

! that's it.

end subroutine EBSDgetIndexingSuccessMap

!--------------------------------------------------------------------------
!
! SUBROUTINE: EBSDgetKAMMap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the KAM (Kernel Average Misorientation Map) given a set of orientations
!
!> @param numeu dimensions of TopMathcIndices (tmi) array
!> @param eulers 1D array of Euler angles 
!> @param ipf_wd width of the ROI
!> @param ipf_ht height of the ROI
!> @param dict dict structure
!> @param Pmdims number of symmetry operators
!> @param kam (returned) Kernel Average Misorientation Map (2D array, radians)
!
!> @date 07/30/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine EBSDgetKAMMap(numeu, eulers, ipf_wd, ipf_ht, dict, kam)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSDgetKAMMap

use math
use io
use dictmod

IMPLICIT NONE

integer(kind=irg),INTENT(IN)     :: numeu
real(kind=sgl),INTENT(IN)        :: eulers(3,numeu)
integer(kind=irg),INTENT(IN)     :: ipf_wd
integer(kind=irg),INTENT(IN)     :: ipf_ht
!type(dicttype),INTENT(INOUT),pointer:: dict
!f2py intent(in,out) ::  dict
type(dicttype),INTENT(INOUT):: dict
!f2py intent(in,out) ::  dict
real(kind=sgl),INTENT(OUT)       :: kam(ipf_wd,ipf_ht)

real(kind=sgl),allocatable       :: localkam(:)
real(kind=sgl),allocatable       :: lstore(:,:), pstore(:,:)
real(kind=sgl)                   :: cp(3), lp(3)
integer(kind=irg)                :: ii, jj, iii 
real(kind=sgl)                   :: dp

kam = 0.0

allocate(lstore(3,ipf_wd), pstore(3,ipf_wd), localkam(numeu))

localkam = 0.0
lstore = 0
pstore = 0
cp = 0
lp = 0

! we'll do this computation on the 1D array, in the same way 
! as the ADP (Average Dot Product) map in the EBSDDI.f90 program
do iii = 1,numeu
    ii = mod(iii,ipf_wd)
    if (ii.eq.0) ii = ipf_wd
    jj = iii/ipf_wd+1
! do we need to copy pstore into lstore ?
    if ((ii.eq.1).and.(jj.gt.1)) lstore = pstore
! determine to which kam entries we need to add the next disorientation value
    if (ii.eq.1) then
      cp = eulers(1:3,iii)
      pstore(1:3,ii) = cp
    else
      lp = cp
      cp = eulers(1:3,iii)
      pstore(1:3,ii) = cp
      call getDisorientationAngle(lp, cp, dict, dp)
      localkam(iii-1) = localkam(iii-1) + dp
      localkam(iii) = localkam(iii) + dp
    end if
    if (jj.gt.1) then
      call getDisorientationAngle(lstore(1:3,ii), cp, dict, dp)
      localkam(iii-ipf_wd+1) = localkam(iii-ipf_wd+1) + dp
      localkam(iii) = localkam(iii) + dp
    end if
end do

! correct the kam values depending on inside, edge, or corner pixels
! divide by 4
localkam = localkam*0.25

! correct the straight segments  
localkam(2:ipf_wd-1) = localkam(2:ipf_wd-1) * 4.0/3.0
localkam(numeu-ipf_wd+2:numeu-1) = localkam(numeu-ipf_wd+2:numeu-1) * 4.0/3.0
do jj=1,ipf_ht-2
  localkam(ipf_wd*jj+1) = localkam(ipf_wd*jj+1) * 4.0/3.0
end do
do jj=2,ipf_ht-1
  localkam(ipf_wd*jj) = localkam(ipf_wd*jj) * 4.0/3.0
end do

! and the corners
localkam(1) = localkam(1) * 4.0
localkam(ipf_wd) = localkam(ipf_wd) * 2.0
localkam(numeu) = localkam(numeu) * 2.0
localkam(numeu-ipf_wd+1) = localkam(numeu-ipf_wd+1) * 4.0/3.0

! and we deallocate the auxiliary variables 
deallocate(lstore,pstore)

do ii=1,ipf_wd
  do jj=1,ipf_ht
    kam(ii,jj) = localkam(ipf_wd*(jj-1)+ii)
  end do
end do

end subroutine EBSDgetKAMMap

end module commonmod
