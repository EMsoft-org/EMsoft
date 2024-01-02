! ###################################################################
! Copyright (c) 2014-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:dictmod.f90
!--------------------------------------------------------------------------
!
! MODULE: dictmod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Dictionary indexing routines
!
!> @details This module contains all the routines that deal with the dictionary
!> indexing approach, both in terms of computing all the dot products (which uses
!> OpenCL kernels) and in terms of the subsequent indexing on the symmetrized 
!> quaternion unit sphere using the modified von Mises-Fisher distribution, or the
!> modified mixture of axial Watson distributinos.  All
!> the details for this approach can be found in two papers:
!>
!> "A dictionary based approach for EBSD indexing", Yu Hui Chen, Se Un Park, Dennis Wei, 
!> Greg Newstadt, Michael Jackson, Jeff Simmons, Alfred Hero, and Marc De Graef, 
!> Microscopy & Microanalysis, under review (2015).
!>
!> "Parameter estimation in spherical symmetry groups", Yu Hui Chen, Dennis Wei,
!> Gregory Newstadt, Marc De Graef, Jeff Simmons, and Al Hero, IEEE Signal Processing 
!> Letters, in print (2015)
!>
!> Here is an example program showing how the routines can be called:
!>
!> program t
!> 
!> use local
!> use typedefs
!> use dictmod
!> use quaternions
!> use constants
!> 
!> integer(kind=irg)               :: nums, seed
!> real(kind=dbl),allocatable      :: samples(:,:)
!> type(dicttype)                  :: dict
!> real(kind=dbl)                  :: muhat(4), kappahat
!> 
!> ! this is a test of the dictionary indexing portion that deals with the 
!> ! modified von Mises-Fisher distribution; the results must be the same 
!> ! as those produced by the original Matlab code...
!> 
!> seed = 432514
!> nums = 1000
!> allocate(samples(4,nums))
!> 
!> allocate(dict)
!> dict%Num_of_init = 3
!> dict%Num_of_iterations = 30
!> dict%pgnum = 32
!> 
!> ! read a bunch of quaternions from a file... and store them in samples
!> 
!> call DI_Init(dict,'VMF') ! replace 'VMF' by 'WAT' to use the axial Watson distribution
!> 
!> do i=1,10
!>   call DI_EMforDD(samples, dict, nums, seed, muhat, kappahat,'VMF')  ! replace 'VMF' by 'WAT' to use the Watson distribution
!> 
!>   write (*,*) '  '
!>   write (*,*) 'mu    = ',muhat
!>   write (*,*) 'kappa = ',kappahat
!>   write (*,*) 'equivalent angular precision : ',180.D0*dacos(1.D0-1.D0/kappahat)/cPi
!> end do
!> 
!> end program
!> 
! 
!> @date 12/31/14 MDG 1.0 original (based on UMich Matlab code and IDL intermediate version)
!> @date 01/02/15 MDG 1.1 debug of code; produces same result as Matlab code
!> @date 01/04/15 MDG 1.2 trial implementation of model using hyperbolic functions instead of exponential
!> @date 01/06/15 MDG 1.3 changed public routine names with DI_ in front
!> @date 01/07/15 MDG 1.4 added VMF sampling routines 
!> @date 01/09/15 MDG 1.5 replaced several computations by numerically more stable versions
!> @date 02/05/15 MDG 1.6 added sampling for axial Watson distribution
!> @date 02/06/15 MDG 1.7 streamlined sampling code by removing duplications; VMF vs. Watson is now an argument 
!> @date 02/06/15 MDG 1.8 general rewrite and removal of duplications in indexing routines; some name changes
!--------------------------------------------------------------------------
module dictmod

use math

IMPLICIT NONE

public  :: DI_Init, DI_EMforDD, DD_Density, DI_Similarity_Classifier, DI_SampleDD, getDisorientationAngle, &
           ReduceOrientationtoRFZ
private :: DD_Estep, DD_Mstep, DD_getQandL, CardIntersection, randDDMarginal, randUniformSphere, &
           getDDDensityLBM,  VMFMeanDirDensity, WatsonMeanDirDensity, DI_RotateToMu, logCp 

interface getDisorientationAngle
        module procedure getDisorientationAngleSingle
        module procedure getDisorientationAngleDouble
end interface

interface getAverageDisorientationMap
        module procedure getAverageDisorientationMapSingle
        module procedure getAverageDisorientationMapDouble
end interface

contains

!--------------------------------------------------------------------------
!
! FUNCTION: DI_RotateToMu
!
!> @author Marc De Graef, Carnegie Mellon University 
!
!> @brief Rotate an array of quaternions to an average direction lmu using the null space approach
!
!> @param N number of samples to return
!> @param seed random number generator seed value
!> @param mu mean direction (unit quaternion)
!> @param kappa concentration
!
!> @date 02/05/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function DI_RotateToMu(N, lmu, y) result(ymu)
!DEC$ ATTRIBUTES DLLEXPORT :: DI_RotateToMu

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: N
real(kind=dbl),INTENT(IN)               :: lmu(4)
real(kind=dbl),INTENT(IN)               :: y(4,N)
real(kind=dbl)                          :: ymu(4,N)

integer(kind=irg)                       :: i

! parameters for the singular value decomposition
integer(kind=irg)                       :: nr, LDA, LDU, LDVT, lwork, info
real(kind=dbl)                          :: mA(4,4), ss(4), u(4,4), vt, work(20)

! Rotate the distribution along the desired mean direction mu
! In Matlab, one uses the null() operator which returns the null space of the argument
! This is then inserted into a 4x4 rotation matrix and multiplied with the quaternions
! from the random sample.  The null space of the input quaternion can be computed with
! singular value decomposition, which is done with the dgesvd Lapack routine. The matrix
! returned as u is the desired rotation matrix, except that the numbers in the first 
! column must have their signs reversed.

mA = 0.D0
mA(1:4,1) = lmu(1:4)
nr = 4
LDA = 4
LDVT = 1
LDU = 4
lwork = 20
call DGESVD('A','N',nr,nr,mA,LDA,ss, u, LDU, vt, LDVT, work, lwork, info)
u(1:4,1) = -u(1:4,1)

! next, apply this 4x4 rotation matrix to all of the generated quaternions to
! rotate them along the mean direction mu
do i=1,N
        ymu(1:4,i) = matmul(u,y(1:4,i))
end do

end function DI_RotateToMu

!--------------------------------------------------------------------------
!
! FUNCTION: randUniformSphere
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief Return a set of random vectors on the sphere S^2 using normal random sampling
!
!> @param N number of samples to return
!> @param seed random number generator seed value
!
!> @date 01/07/15 MDG 1.0 original, based on Yu-Hui's Matlab code, output transposed
!--------------------------------------------------------------------------
recursive function randUniformSphere(N,seed) result(ranSphere)
!DEC$ ATTRIBUTES DLLEXPORT :: randUniformSphere

use local
use math

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: N
integer(kind=irg),INTENT(INOUT)         :: seed
!f2py intent(in,out) ::  seed
real(kind=dbl)                          :: ranSphere(3,N)

real(kind=dbl)                          :: nq, NR(N*3), randNorm(3,N)
integer(kind=irg)                       :: i

ranSphere = 0.D0
call R8VEC_normal_01(N*3,seed,NR)
randNorm = reshape( NR, (/ 3, N /) )

! and normalize the three-vectors
do i=1,N
  nq = dsqrt(sum(randNorm(1:3,i)**2))
  RanSphere(1:3,i) = randNorm(1:3,i)/nq
end do

end function randUniformSphere

!--------------------------------------------------------------------------
!
! FUNCTION: DI_SampleDD
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief Sample a directional distribution (VMF or WAT) on the quaternion unit sphere
!
!> @param N number of samples to return
!> @param seed random number generator seed value
!> @param mu mean direction (unit quaternion)
!> @param kappa concentration
!> @param Dtype 'VMF' for von-Mises-Fisher  or 'WAT' for axial Watson
!
!> @date 01/07/15 MDG 1.0 original, based on Yu-Hui's Matlab code
!--------------------------------------------------------------------------
recursive function DI_SampleDD(N, seed, mu, kappa, Dtype) result(sDD)
!DEC$ ATTRIBUTES DLLEXPORT :: DI_SampleDD

use local
use error

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: N
integer(kind=irg),INTENT(INOUT)         :: seed
!f2py intent(in,out) ::  seed
real(kind=dbl),INTENT(IN)               :: mu(4)
real(kind=dbl),INTENT(IN)               :: kappa
character(3),INTENT(IN)                 :: Dtype
real(kind=dbl)                          :: sDD(4,N)

real(kind=dbl)                          :: nq, tmpmu(4), RandSphere(3,N), t(N), RS(4,N), lmu(4), y(4,N)
integer(kind=irg)                       :: i

! make sure the input quaternion is normalized
nq = dsqrt(sum(mu*mu))
if (nq.eq.0.D0) call FatalError('DI_SampleDD','Input quaternion has zero length')
lmu = mu/nq

! initialize a bunch of parameters
tmpmu = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
sDD = 0.D0
RS = 0.D0

! get the t-parameter 
t = randDDMarginal(N, kappa, seed, Dtype)

! and the distribution of random directions on the 2-sphere
RandSphere = randUniformSphere(N,seed)
RS(2:4,1:N) = RandSphere(1:3,1:N)

! merge these two parameters into the desired random variables
y = transpose( spread(t,DIM=2,NCOPIES=4) * spread(tmpmu,DIM=1,NCOPIES=N) + &
               spread(dsqrt(1.D0-t*t),DIM=2,NCOPIES=4) * transpose(RS) )

! Rotate the distribution along the desired mean direction mu
sDD = DI_RotateToMu(N, lmu, y)

end function DI_SampleDD

!--------------------------------------------------------------------------
!
! FUNCTION: randDDMarginal
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief rejection sampling for the t parameter for the marginal directional distribution
!
!> @details This algorithm samples the parameter t from a marginal distribution f(t), which
!> by itself is the VMF or Watson distribution integrated around the mean direction.  These
!> expressions were derived by Yu-Hui and verified by MDG [02/05/15] and then implemented;
!> there are some typographical errors in the literature, and the versions documented here
!> are correct [numerical verification].
!
!> @param N number of samples to return
!> @param k concentration
!> @param seed random number generator seed value
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 01/07/15 MDG 1.0 original, based on Yu-Hui's Matlab code
!> @date 02/05/15 MDG 1.1 consolidated routines for VMF and WAT distributions
!--------------------------------------------------------------------------
recursive function randDDMarginal(N, k, seed, Dtype) result(t)
!DEC$ ATTRIBUTES DLLEXPORT :: randDDMarginal

use local
use math

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: N
real(kind=dbl),INTENT(IN)               :: k
integer(kind=irg),INTENT(INOUT)         :: seed
!f2py intent(in,out) ::  seed
character(3),INTENT(IN)                 :: Dtype
real(kind=dbl)                          :: t(N)

real(kind=dbl)                          :: LBM(2), h, x, C
integer(kind=irg)                       :: i

! Find the left bound and maximum needed for rejection sampling
C = 0.D0
LBM = getDDDensityLBM(k,C, Dtype)

! apply the rejection sampling algorithm to either VMF or Watson marginal distributions
t = 0.D0
do i=1,N
  do 
    x = r8_uniform_01(seed)*(1.D0-LBM(1))+LBM(1)
    if (Dtype.eq.'VMF') h = VMFMeanDirDensity(x, k, C)
    if (Dtype.eq.'WAT') h = WatsonMeanDirDensity(x, k, C)
    if (r8_uniform_01(seed)*LBM(2).le.h) EXIT 
  end do
  t(i) = x
end do 

end function randDDMarginal

!--------------------------------------------------------------------------
!
! FUNCTION: getDDDensityLBM
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief determines the left bound and maximum for rejection sampling
!
!> @param k concentration
!> @param C constant prefactor of distribution function
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 01/07/15 MDG 1.0 original, based on Yu-Hui's Matlab code
!> @date 02/05/15 MDG 1.1 consolidated routines for VMF and WAT distributions
!--------------------------------------------------------------------------
recursive function getDDDensityLBM(k,C,Dtype) result(LBM)
!DEC$ ATTRIBUTES DLLEXPORT :: getDDDensityLBM
use local
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: k
real(kind=dbl),INTENT(INOUT)            :: C
!f2py intent(in,out) ::  C
character(3),INTENT(IN)                 :: Dtype
real(kind=dbl)                          :: LBM(2)

real(kind=dbl),parameter                :: min_thresh=0.00001D0
real(kind=dbl)                          :: s, x, start
integer(kind=irg)                       :: f

start = -1.D0
if (Dtype.eq.'WAT') start = 0.D0

! first we look for the left bound
f = 1
do 
 x = start+dble(f)*0.00001D0
 if (x.eq.1.D0) call FatalError('getDDDensityLBM','reached +1 in leftbound determination')
 if (Dtype.eq.'VMF') s = VMFMeanDirDensity(x,k,C)
 if (Dtype.eq.'WAT') s = WatsonMeanDirDensity(x,k,C)
 if (s.ge.min_thresh) EXIT
 f = f+1
end do
!
LBM(1) =  start+dble(f)*0.00001D0

if (Dtype.eq.'VMF') then
! for the simplified version of the density function, we have an analytical
! expression for where the maximum of the function occurs [convert the BesselI(3/2,x)
! to hyperbolic functions, then to exponential, and ignore the negative exponential
! which will be very small for reasonably sized k and t...]
  x = (-1.D0+dsqrt(1.D0+4.D0*k*k))/(2.D0*k)
  LBM(2) = VMFMeanDirDensity(x,k,C)
end if

if (Dtype.eq.'WAT') then
! for the simplified version of the density function, we have an analytical
! expression for where the maximum of the function occurs 
  x = dsqrt((2.D0*k-1.D0)/(2.D0*k))
  LBM(2) = WatsonMeanDirDensity(x,k,C)
end if

end function getDDDensityLBM

!--------------------------------------------------------------------------
!
! FUNCTION: VMFMeanDirDensity
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief function to be sampled for VMF random sampling; we're using a close approximation
!
!> @param x argument value
!> @param k concentration
!> @param C constant prefactor
!
!> @date 01/07/15 MDG 1.0 original, based on Yu-Hui's Matlab code
!--------------------------------------------------------------------------
recursive function VMFMeanDirDensity(x, k, C) result(y)
!DEC$ ATTRIBUTES DLLEXPORT :: VMFMeanDirDensity

use local
use constants
use math
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: x
real(kind=dbl),INTENT(IN)       :: k
real(kind=dbl),INTENT(INOUT)    :: C
!f2py intent(in,out) ::  C
real(kind=dbl)                  :: y


if (dabs(x).gt.1.D0) call FatalError('VMFMeanDirDensity','argument must be in [-1,1]')

! explicit expression for p=4 (Gamma[3/2]Gamma[1/2] = pi/2)
! and the BesselI(3/2) function reduces to hyperbolic functions
! diverges for k->0, and becomes really small for large k
if (C.eq.0.D0) then
  C = 2.D0*k**(2.5D0)/dsqrt(2.D0*cPi)/(k-1.D0)
end if

! this is a close approximation, really good for larger values of k
! and numerically more stable than the original, which has problems for k>600 or so
y = C * dexp(k*(x-1.D0))*dsqrt(1.D0-x*x)

end function VMFMeanDirDensity

!--------------------------------------------------------------------------
!
! FUNCTION: WatsonMeanDirDensity
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief function to be sampled for Watson random sampling; we're using a close approximation
!
!> @param x argument value
!> @param k concentration
!> @param C constant prefactor
!
!> @date 02/05/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function WatsonMeanDirDensity(x, k, C) result(y)
!DEC$ ATTRIBUTES DLLEXPORT :: WatsonMeanDirDensity

use local
use constants
use math
use error

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: x
real(kind=dbl),INTENT(IN)       :: k
real(kind=dbl),INTENT(INOUT)    :: C
!f2py intent(in,out) ::  C
real(kind=dbl)                  :: y

real(kind=dbl),parameter        :: CC = 144.43253338822560946D0         ! 256/sqrt(pi)


if (dabs(x).gt.1.D0) call FatalError('WatsonMeanDirDensity','argument must be in [-1,1]')

! approximate expression for p=4 
if (C.eq.0.D0) then
  C = CC*k**4.5D0/(525.D0+4.D0*k*(45.D0+8.D0*k*(3.D0+4.D0*k)))
end if

! this is a close approximation, really good for larger values of k
! and numerically more stable than the original, which has problems for k>600 or so
y = C * dexp(k*(x*x-1.D0))*dsqrt(1.D0-x*x)

end function WatsonMeanDirDensity



!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! below we have a series of functions/subroutines used to index patterns based
! on the set of 40 or so closest matches in the dictionary; these routines perform
! an averaging on the quaternion unit sphere and return the mean direction and
! the concentration parameter for either the von Mises-Fisher or the axial Watson distribution.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! SUBROUTINE: DI_Init
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief initialize the dictionary indexing parameters (symmetry operators and precomputed Ap lookup table)
!
!> @details For all details, see following paper:
!
!> @param dict dictionary parameter (must be declared in calling routine)
!> @param Dtype  'VMF' or 'WAT' for von Mises-Fisher and Watson distributions, respectively.
!
!> @date 12/31/14 MDG 1.0 original
!> @date 01/06/15 MDG 1.1 added optional argument full
!> @date 02/06/15 MDG 1.2 removed full again after extensive testing; no need to use 2M operators
!> @date 01/04/18 MDG 1.3 added icosahedral symmetry operator to handle quasi-crystal computations.
!> @date 03/12/19 MDG 1.4 added 'sym' option to only generate the symmetry quaternions
!--------------------------------------------------------------------------
recursive subroutine DI_Init(dict,Dtype) 
!DEC$ ATTRIBUTES DLLEXPORT :: DI_Init

use local
use typedefs
use error
use math

IMPLICIT NONE

type(dicttype),INTENT(INOUT)    :: dict
!f2py intent(in,out) ::  dict
character(3),INTENT(IN)         :: Dtype

integer(kind=irg)               :: i
real(kind=dbl)                  :: y1, y2


! here we need to analyze the rotational symmetry group, and copy the appropriate 
! quaternion symmetry operators into the dict%Pm array

! first get the number of the rotational point group that corresponds to the crystal point group
dict%prot = PGrot(dict%pgnum)
! possible values for dict%prot are: (/1,3,6,9,12,16,18,21,24,28,30/)
! corresponding to the point groups 1, 2, 222, 4, 422, 3, 32, 6, 622, 23, 432 and 532 respectively

!------------
! IMPORTANT NOTE: the original von Mises-Fischer (VMF) approach requires that q and -q are considered to 
! be separate quaternions, so the original Matlab code included the negatives of all quaternion symmetry operators 
! as well, leading to a cardinality of twice the rotational point group order.  It appears that we do not have to
! do so if we replace the exponential in the VMF by a hyperbolic cosine function, which would account directly
! for the q, -q duplicity... Alternatively, one can use the axial Watson distribution.
!------------

! identity operator is part of all point groups
dict%Pm = 0.D0                  ! initialize all entries to zero
dict%Pm(1:4,1) = SYM_Qsymop(1:4,1)

! select statement for each individual rotational point group (see typedefs.f90 for SYM_Qsymop definitions)
select case (dict%prot) 
        case(1)         ! 1 (no additional symmetry elements)
                dict%Nqsym = 1
                dict%Pm(1:4,2) = -dict%Pm(1:4,1)

        case(3)         ! 2  (we'll assume that the two-fold axis lies along the e_y-axis)
                dict%Nqsym = 2
                dict%Pm(1:4,2) = SYM_Qsymop(1:4,3)

        case(6)         ! 222
                dict%Nqsym = 4
                do i=2,4
                  dict%Pm(1:4,i) = SYM_Qsymop(1:4,i)
                end do

        case(9)         ! 4
                dict%Nqsym = 4
                dict%Pm(1:4,2) = SYM_Qsymop(1:4,4)
                dict%Pm(1:4,3) = SYM_Qsymop(1:4,7)
                dict%Pm(1:4,4) = SYM_Qsymop(1:4,10)

        case(12)        ! 422
                dict%Nqsym = 8
                dict%Pm(1:4,2) = SYM_Qsymop(1:4,4)
                dict%Pm(1:4,3) = SYM_Qsymop(1:4,7)
                dict%Pm(1:4,4) = SYM_Qsymop(1:4,10)
                dict%Pm(1:4,5) = SYM_Qsymop(1:4,2)
                dict%Pm(1:4,6) = SYM_Qsymop(1:4,3)
                dict%Pm(1:4,7) = SYM_Qsymop(1:4,11)
                dict%Pm(1:4,8) = SYM_Qsymop(1:4,12)

        case(16)        ! 3
                dict%Nqsym = 3
                dict%Pm(1:4,2) = SYM_Qsymop(1:4,26)
                dict%Pm(1:4,3) = SYM_Qsymop(1:4,28)
                !call FatalError('InitDictionaryIndexing','this symmetry has not yet been implemented (pg 3)')

        case(18)        ! 32 (needs special handling)
                dict%Nqsym = 6
                dict%Pm(1:4,2) = SYM_Qsymop(1:4,26)
                dict%Pm(1:4,3) = SYM_Qsymop(1:4,28)
                dict%Pm(1:4,4) = SYM_Qsymop(1:4,30)
                dict%Pm(1:4,5) = SYM_Qsymop(1:4,32)
                dict%Pm(1:4,6) = SYM_Qsymop(1:4,34)
                !call FatalError('InitDictionaryIndexing','this symmetry has not yet been implemented (pg 32)')

        case(21)        ! 6
                dict%Nqsym = 6
                do i=25,29
                  dict%Pm(1:4,i-23) = SYM_Qsymop(1:4,i)
                end do

        case(24)        ! 622
                dict%Nqsym = 12
                do i=25,35
                  dict%Pm(1:4,i-23) = SYM_Qsymop(1:4,i)
                end do

        case(28)        ! 23
                dict%Nqsym = 12
                do i=2,4
                  dict%Pm(1:4,i) = SYM_Qsymop(1:4,i)
                end do
                do i=17,24
                  dict%Pm(1:4,4+(i-16)) = SYM_Qsymop(1:4,i)
                end do

        case(30)        ! 432
                dict%Nqsym = 24
                do i=2,24
                  dict%Pm(1:4,i) = SYM_Qsymop(1:4,i)
                end do
        case(33)        ! 532
                dict%Nqsym = 60
                do i=2,60
                  dict%Pm(1:4,i) = SYM_Qsymop(1:4,35+i)
                end do
        case(34)  ! 822
                dict%Nqsym = 16
                do i = 1,15
                  dict%Pm(1:4,i+1) = SYM_Qsymop(1:4,95+i)
                end do
        case(35)  ! 1022
                dict%Nqsym = 20
                do i = 1,19
                  dict%Pm(1:4,i+1) = SYM_Qsymop(1:4,110+i)
                end do
        case(36)  ! 1222
                dict%Nqsym = 24
                do i = 1,23
                  dict%Pm(1:4,i+1) = SYM_Qsymop(1:4,129+i)
                end do

        case default    ! this should never happen ...
                call FatalError('InitDictionaryIndexing','unknown rotational point group number')
end select

if (Dtype.ne.'sym') then ! generate the other parameters only if Dtype is not 'sym'
! von Mises-Fisher mode:
! the next part of the initial Matlab code computes a lookup table for the parameter Ap(u) (Appendix in paper)
! this lookup table is only used when the ratio of the BesselI functions is between 0 and 0.95; for the 
! region between 0.95 and 1, we use an analytical approximation (see VMF_Mstep routine).
!
! Watson mode:
! we've used a similar approach to create a lookup table for values of kappa that are smaller than 35, in
! which case we use the standard ratio of Kummer functions:  Kummer[3/2,3,k]/Kummer[1/2,2,k]/k.  For
! larger kappa values, we have an expansion using the large argument behavior of the modified Bessel functions.
! 
  dict%Apnum = 35000
  allocate(dict%xAp(dict%Apnum), dict%yAp(dict%Apnum))

    ! define the xAp array
  dict%xAp = (/ (0.001D0+dble(i-1)*0.001D0,i=1,dict%Apnum)  /)

  if (Dtype.eq.'VMF') then ! von Mises-Fisher distribution
    do i=1,dict%Apnum
      dict%yAp(i) = BesselIn(dict%xAp(i), 2) / BesselI1(dict%xAp(i))
    end do
  end if

  if (Dtype.eq.'WAT') then ! Watson distribution
    do i=1,dict%Apnum
      y1 = BesselI1(dict%xAp(i)*0.5D0)
      y2 = BesselI0(dict%xAp(i)*0.5D0)
      dict%yAp(i) = y1 / (y2-y1) / dict%xAp(i)
    end do
  end if
end if

end subroutine DI_Init

!--------------------------------------------------------------------------
!
! SUBROUTINE: DI_EMforDD
!
!> @author Yu-Hui Chen, U. Michigan / Marc De Graef, Carnegie Mellon University
!
!> @brief Expectation maximization approach to maximum likelihood problem for mu and kappa
!
!> @details For all details, see following paper:
!
!> @param X list of input quaternions
!> @param dict dictionary parameter (must be declared in calling routine)
!> @param nums number of input quaternions
!> @param seed for normal random number generator 
!> @param muhat output mean orientation
!> @param kappahat output concentration parameter
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 01/01/15 MDG 1.0 original, based on Chen's Matlab version + simplifications
!> @date 01/06/15 MDG 1.1 added optional argument full
!> @date 02/06/15 MDG 1.2 removed full again, added Dtype and streamlined code; removed duplications
!--------------------------------------------------------------------------
recursive subroutine DI_EMforDD(X, dict, nums, seed, muhat, kappahat, Dtype)
!DEC$ ATTRIBUTES DLLEXPORT :: DI_EMforDD

use local
use constants
use typedefs
use math, only:r8vec_normal_01, r4_uniform_01          ! array of normal random numbers
use quaternions
use rotations, only:qu2ro               ! we only need to move to Rodrigues-Frank space
use so3, only:IsinsideFZ                ! we only need to do a test ...

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nums
real(kind=dbl),INTENT(IN)               :: X(4,nums)
type(dicttype),INTENT(INOUT)            :: dict
!f2py intent(in,out) ::  dict
integer(kind=irg),INTENT(INOUT)         :: seed
!f2py intent(in,out) ::  seed
real(kind=dbl),INTENT(OUT)              :: muhat(4)
real(kind=dbl),INTENT(OUT)              :: kappahat
character(3),INTENT(IN)                 :: Dtype

integer(kind=irg)                       :: i, j, N, Pmdims, init, dd
integer(kind=irg)                       :: FZtype, FZorder
real(kind=dbl),allocatable              :: Mu_All(:,:), Kappa_All(:), R_All(:,:,:), L_All(:), &
                                           R(:,:), Q(:), L(:)
real(kind=dbl)                          :: Mu(4), PmMu(4), MuKa(5), Qi, Li, rod(4), qu(4), Kappa

! In this routine, we perform the EM algorithm to obtain an estimate for the 
! mean direction and concentration parameter of the modified von Mises-Fisher (mVMF)
! distribution that models the statistics of the orientation point cloud.

! array sizes (we use shorthand notations)
N = nums
Pmdims = dict%Nqsym

! initialize some auxiliary arrays
allocate(Mu_All(dict%Num_of_init,4), Kappa_All(dict%Num_of_init), &
         R_All(N,Pmdims,dict%Num_of_init),L_All(dict%num_of_init))
Mu_All = 0.D0
Kappa_All = 0.D0
R_All = 0.D0
L_All = 0.D0


! main loop (EM typically uses a few starting parameter sets to make sure we don't get stuck in a local maximum)
do init=1,dict%Num_of_init 
! create a vector to hold the results
  allocate(R(N,Pmdims))
  R = 0.D0

! generate a normal random vector and normalize it as a starting guess for Mu (i.e., a unit quaternion)
  call R8VEC_normal_01(4,seed,Mu)
  Mu = Mu/cabs(Mu)

! the EMsoft package only considers quaternions with positive first component, 
! so we may need to change all the signs
  if (Mu(1).lt.0.D0) Mu = -Mu

! starting value for Kappa
  Kappa = 30.D0

! define the number of iterations and the Q and L function arrays
  allocate (Q(dict%Num_of_iterations), L(dict%Num_of_iterations))
  Q = 0.D0
  L = 0.D0

! and here we go with the EM iteration...
! we use quaternion multiplication throughout instead of the matrix version in the Matlab version
! quaternion multiplication has been verified against the 4x4 matrix multiplication of the Matlab code on 01/02/15 
  iloop: do i=1,dict%Num_of_iterations 
! E-step
    R = DD_Estep(X,dict,Pmdims,N,Mu,Kappa,Dtype)
! M-step
    MuKa = DD_Mstep(X,dict,Pmdims,N,R,Dtype)
! calculate the Q and Likelihood function values
    call DD_getQandL(X,dict,Pmdims,nums,MuKa,R,Qi,Li,Dtype)
    L(i) = Li
    Q(i) = Qi

! update the containers
    Mu_All(init,1:4) = MuKa(1:4)
    Kappa_All(init) = MuKa(5)
    R_All(1:N,1:Pmdims,init) = R(1:N,1:Pmdims)
    L_All(init) = L(i)
    Mu = MuKa(1:4)
    Kappa = MuKa(5)

! and terminate if necessary
    if (i.ge.2) then 
      if (abs(Q(i)-Q(i-1)).lt.0.01) then 
        EXIT iloop
      end if
    end if
  end do iloop
  deallocate(R,Q,L)
end do

dd = maxloc(L_All,1)
Mu = Mu_all(dd,1:4)
kappahat = Kappa_All(dd)

! the EMsoft package only considers quaternions with positive first component, 
! so we may need to change all the signs
if (Mu(1).lt.0.D0) Mu = -Mu

! the final step is to make sure that the resulting Mu lies in the same
! fundamental zone that the dictionary elements are located in; since we start
! the EM iterations from a random quaternion, there is no guarantee that the 
! result lies in the same fundamental zone. Therefore, we cycle through all the 
! equivalent quaternions, and stop as soon as we find one in the Rodrigues 
! fundamental zone, which requires routines from the rotations and so3 modules. 
FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

FZloop: do i=1,Pmdims
  qu = quat_mult(Mu,dict%Pm(1:4,i))
  if (qu(1).lt.0.D0) qu = -qu
  rod = qu2ro(qu)
  if (IsinsideFZ(rod,FZtype,FZorder)) EXIT FZloop
end do FZloop
muhat = qu

deallocate(Mu_All, Kappa_All, R_All, L_All)

end subroutine DI_EMforDD


!--------------------------------------------------------------------------
!
! FUNCTION: DD_Estep
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief computes the E step of the EM process, verified against Matlab code on 01/02/15
!
!> @param X list of input quaternions
!> @param dict dictionary type
!> @param Pmdims number of quaternion symmetry operators
!> @param nums number of samples
!> @param Mu current guess for mean quaternion
!> @param Kappa input parameter
!
!> @date 01/01/15 MDG 1.0 original
!> @date 01/06/15 MDG 1.1 added optional argument full
!> @date 01/09/15 MDG 1.2 removed optional argument full (incorporated in dicttype)
!> @date 02/06/15 MDG 1.3 removed full handling after extensive checking
!> @date 02/06/15 MDG 1.4 merged VMF and Watson Esteps into a single routine and renamed
!--------------------------------------------------------------------------
recursive function DD_Estep(X,dict,Pmdims,nums,Mu,Kappa,Dtype) result(R)
!DEC$ ATTRIBUTES DLLEXPORT :: DD_Estep

use local
use typedefs
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nums
real(kind=dbl),INTENT(IN)               :: X(4,nums)
type(dicttype),INTENT(IN)               :: dict
integer(kind=irg),INTENT(IN)            :: Pmdims
real(kind=dbl),INTENT(IN)               :: Mu(4)
real(kind=dbl),INTENT(IN)               :: Kappa
character(3),INTENT(IN)                 :: Dtype
real(kind=dbl)                          :: R(nums,Pmdims)

integer(kind=irg)                       :: j 
real(kind=dbl)                          :: Rdenom(nums), PmMu(4), C

C = logCp(kappa,Dtype)

do j=1,Pmdims
  PmMu = quat_mult(Mu,dict%Pm(1:4,j))
  R(1:nums,j) = DD_Density(X, nums, PmMu, Kappa, C, Dtype)
end do
! and determine the normalization factors
Rdenom = 1.D0/sum(R,2)

do j=1,Pmdims 
  R(1:nums,j) = R(1:nums,j)*Rdenom(1:nums)
end do

end function DD_Estep


!--------------------------------------------------------------------------
!
! FUNCTION: DD_Mstep
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief computes the M step of the EM process
!
!> @param X list of input quaternions
!> @param dict dictionary type
!> @param Pmdims number of quaternion symmetry operators
!> @param nums number of samples
!> @param R weight factors form the E step
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 01/01/15 MDG 1.0 original
!> @date 01/06/15 MDG 1.1 added optional full parameter
!> @date 01/09/15 MDG 1.2 removed optional argument full (incorporated in dicttype)
!> @date 01/09/15 MDG 1.3 introduced accurate numerical approximation for kappa determination
!> @date 02/06/15 MDG 1.4 removed full handling after extensive checking
!> @date 02/06/15 MDG 1.5 merged Msteps for VMF and WAT and renamed
!--------------------------------------------------------------------------
recursive function DD_Mstep(X,dict,Pmdims,nums,R,Dtype) result(MuKa)
!DEC$ ATTRIBUTES DLLEXPORT :: DD_Mstep

use local
use typedefs
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nums
real(kind=dbl),INTENT(IN)               :: X(4,nums)
type(dicttype),INTENT(IN)               :: dict
integer(kind=irg),INTENT(IN)            :: Pmdims
real(kind=dbl),INTENT(IN)               :: R(nums,Pmdims)
character(3),INTENT(IN)                 :: Dtype
real(kind=dbl)                          :: MuKa(5)

real(kind=dbl)                          :: tmpGamma(4), nGamma, diff(dict%Apnum), qu(4), y, Tscatt(4,4), tmp(4,4)
integer(kind=irg)                       :: minp, i, j

! variables needed for the dsyev Lapack eigenvalue routine
CHARACTER                               :: JOBZ, UPLO
INTEGER                                 :: INFO, LDA, LWORK, NN
DOUBLE PRECISION                        :: A( 4 , 4 ), W( 4 ), WORK( 20 )

if (Dtype.eq.'VMF') then
! this is simplified from the Matlab routine and uses straight summations and
! quaternion multiplication instead of arrays
  tmpGamma = 0.D0
  do j=1,Pmdims 
    do i=1,nums
      qu = quat_mult(X(1:4,i),conjg(dict%Pm(1:4,j)))
      tmpGamma = tmpGamma +  R(i,j) * qu
    end do
  end do
  nGamma = cabs(tmpGamma)
  MuKa(1:4) = tmpGamma/nGamma
  y = nGamma/dble(nums)
end if

if (Dtype.eq.'WAT') then
! here, we compute the modified scattering matrix Tscatt and compute its largest eigenvalue
! under the assumption that kappa will always be positive for the types of problems that we
! need to consider; we need to use the outer product, implemented using spread calls
  Tscatt = 0.D0
  do j=1,Pmdims 
    do i=1,nums
      qu = quat_mult(X(1:4,i),conjg(dict%Pm(1:4,j)))
      tmp = spread(qu,dim=2,ncopies=4)*spread(qu,dim=1,ncopies=4)
      Tscatt = Tscatt + R(i,j) * tmp
    end do
  end do
  Tscatt = Tscatt/dble(nums)

  JOBZ = 'V'
  UPLO = 'U'
  NN = 4
  LDA = 4
  LWORK = 20
  A = Tscatt
  call DSYEV( JOBZ, UPLO, NN, A, LDA, W, WORK, LWORK, INFO )
  qu(1:4) = A(1:4,4)
  MuKa(1:4) = qu(1:4)
  y = dot_product(qu,matmul(Tscatt,qu))
end if

! find kappa corresponding to this value of gamma (equation 17 in appendix of paper)
! we split this into two regionds: 0<=y<0.94, for which we use the look-up table 
! approach, and 0.94<=y<=1, for which we have derived an analytical approximation
! that is pretty accurate in the relevant region of kappa>30.
if (y.ge.0.94D0) then
  if (Dtype.eq.'VMF') MuKa(5) = (15.D0-3.D0*y+dsqrt(15.D0+90.D0*y+39.D0*y*y))/(16.D0*(1.0D0-y))
  if (Dtype.eq.'WAT') MuKa(5) = (5.D0*y-11.D0-dsqrt(39.D0-12.D0*y+9.D0*y**2))/(8.D0*(y-1.D0))
else
  diff = dabs( y - dict%yAp ) 
  minp = minloc( diff, 1 )
  if (minp.eq.1) minp = 2 
  MuKa(5) = dict%xAp(minp)
end if

end function DD_Mstep

!--------------------------------------------------------------------------
!
! SUBROUTINE: DD_getQandL
!
!> @author Yu-Hui Chen, U. Michigan / Marc De Graef, Carnegie Mellon University
!
!> @brief Computes the Q array and the log-likelihood array
!
!> @details For all details, see following paper:
!
!> @param X list of input quaternions
!> @param dict dictionary parameter (must be declared in calling routine)
!> @param Pmdims number of quaternion symmetry operators to consider
!> @param number of input quaternions
!> @param MuKa  vector with Mu and Kappa
!> @param R output from the E step
!> @param Q output Q 
!> @param L output L 
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 01/01/15 MDG 1.0 original, based on Chen's Matlab version + simplifications
!> @date 01.06/15 MDG 1.1 added optional full parameter
!> @date 01/09/15 MDG 1.2 removed optional argument full (incorporated in dicttype)
!> @date 02/06/15 MDG 1.3 removed full handling after extensive checking
!> @date 02/06/15 MDG 1.4 merged VMF and WAT routines and renamed
!> @date 12/05/16 MDG 1.5 intercepted case when Phi becomes zero for a VERY sharp texture...
!--------------------------------------------------------------------------
recursive subroutine DD_getQandL(X,dict,Pmdims,nums,MuKa,R,Q,L,Dtype)
!DEC$ ATTRIBUTES DLLEXPORT :: DD_getQandL

use local
use typedefs
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nums
real(kind=dbl),INTENT(IN)               :: X(4,nums)
type(dicttype),INTENT(INOUT)            :: dict
!f2py intent(in,out) ::  dict
integer(kind=irg),INTENT(IN)            :: Pmdims
real(kind=dbl),INTENT(IN)               :: MuKa(5)
real(kind=dbl),INTENT(IN)               :: R(nums,Pmdims)
real(kind=dbl),INTENT(INOUT)            :: Q
!f2py intent(in,out) ::  Q
real(kind=dbl),INTENT(INOUT)            :: L
!f2py intent(in,out) ::  L
character(3),INTENT(IN)                 :: Dtype

real(kind=dbl)                          :: Phi(nums,Pmdims), PmMu(4), qu(4), C, oldQ, oldL
integer(kind=irg)                       :: j
real(kind=dbl),parameter                :: eps = 0.00001D0

  oldQ = Q
  oldL = L

  C = logCp(MuKa(5),Dtype)
  if (Dtype.eq.'VMF') C = dexp(C)

! compute the auxiliary Phi array
  Phi = 0.D0
  qu = MuKa(1:4)
  do j=1,Pmdims
    PmMu = quat_mult(dict%Pm(1:4,j), qu)
    Phi(1:nums,j) = DD_Density(X, nums, PmMu, MuKa(5), C, Dtype)
  end do
  Phi = Phi/dble(dict%Nqsym)
  if (minval(Phi).gt.0.D0) then
! and convert the array into the Q and L parameters.
   L = sum(dlog(sum(Phi,2)))
   Q = sum(R*dlog(Phi))
  else
   L = oldL
   Q = oldQ
  end if ! else, we reuse the old values
end subroutine DD_getQandL

!--------------------------------------------------------------------------
!
! FUNCTION: DD_Density
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief computes the VMF or Watson density function
!
!> @details 
!> original in Matlab by Yu-Hui Chen, U. Michigan
!> converted to IDL by MDG, 12/18/14, simplified arguments
!> converted to f90 by MDG, 12/31/14, further simplifications
!> output validated against Matlab output on 12/31/14
!
!> @param X input quaternion samples
!> @param nums number of samples
!> @param mu mean direction
!> @param kappa concentration
!> @param C logCp(kappa) or exp(logCp(kappa) (precomputed in calling routine)
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 01/01/15 MDG 1.0 original
!> @date 01/06/15 MDG 1.1 added C and full parameters
!> @date 02/06/15 MDG 1.2 removed full handling
!> @date 02/06/15 MDG 1.3 merged 'VMF' and 'WAT' routines and renamed
!--------------------------------------------------------------------------
recursive function DD_Density(X,nums,mu,kappa,C,Dtype) result(y)
!DEC$ ATTRIBUTES DLLEXPORT :: DD_Density

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: nums
real(kind=dbl),INTENT(IN)               :: X(4,nums)
real(kind=dbl),INTENT(IN)               :: mu(4)
real(kind=dbl),INTENT(IN)               :: kappa
real(kind=dbl),INTENT(IN)               :: C
character(3),INTENT(IN)                 :: Dtype
real(kind=dbl)                          :: y(nums)

integer(kind=irg)                       :: j

if (Dtype.eq.'VMF') then 
 do j=1,nums
  y(j) = dexp(C+kappa*dot_product(mu,X(1:4,j)))
 end do
end if

if (Dtype.eq.'WAT') then 
 do j=1,nums
  y(j) = dexp(C+kappa*dot_product(mu,X(1:4,j))**2)
 end do
end if

end function DD_Density

!--------------------------------------------------------------------------
!
! FUNCTION: logCp
!
!> @author Marc De Graef, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief computes the logarithm of Cp for VMF and Watson distributions
!
!> @details 
!> original in Matlab by Yu-Hui Chen, U. Michigan
!> converted to IDL by MDG, 12/18/14, simplified arguments
!> converted to f90 by MDG, 12/31/14, further simplifications
!> output validated against Matlab output on 12/31/14
!
!> @param kappa input parameter
!> @param Dtype 'VMF' or 'WAT'
!
!> @date 12/31/14 MDG 1.0 original
!> @date 01/09/14 MDG 1.1 introduced more accurate numerical approximation for Cp
!> @date 01/09/14 MDG 1.2 moved some of the constants in front of the logarithm
!> @date 02/06/15 MDG 1.3 merged VMF and WAT routines
!--------------------------------------------------------------------------
recursive function logCp(kappa,Dtype) result(lCp)
!DEC$ ATTRIBUTES DLLEXPORT :: logCp

use local
use constants
use math 

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: kappa
character(3),INTENT(IN)         :: Dtype
real(kind=dbl)                  :: lCp

! pre-computed constants
real(kind=dbl),parameter        :: C=-3.675754132818690967D0    ! C = ln(1.D0/(2.D0*cPi)**2)
real(kind=dbl),parameter        :: C2=4.1746562059854348688D0   ! C2 = ln(512/sqrt(2)/pi^(3/2))
real(kind=dbl),parameter        :: C2W=5.4243952068443172530D0  ! C2 = ln(128*sqrt(pi))

if (Dtype.eq.'VMF') then
! for arguments larger than kappa=30, we use a simple numerical approximation
  if (kappa.gt.30.D0) then 
    lCp = kappa**4.5D0/(-105D0+8.D0*kappa*(-15.D0+16.D0*kappa*(-3.D0+8.D0*kappa)))
    lCp = C2 - kappa + dlog(lCp)
  else 
    lCp = C + dlog( kappa / BesselI1(kappa) )
  end if
end if 

if (Dtype.eq.'WAT') then 
! for arguments larger than kappa=20, we use a simple numerical approximation
  if (kappa.gt.20.D0) then 
    lCp = kappa**4.5D0/(525.D0 + 4.D0*kappa*(45.D0 + 8.D0*kappa*(3.D0 + 4.D0*kappa)))
    lCp = C2W - kappa + dlog(lCp)
  else 
    lCp = -kappa*0.5D0 - dlog( BesselI0(kappa*0.5D0) - BesselI1(kappa*0.5D0) )
  end if
end if

end function logCp

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! the following routines have to do with the neighborhood similarity analysis
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!
! SUBROUTINE: DI_similarity_classifier
!
!> @author Saransh Singh, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief classify the point as grain interior or anomalous point
!
!> @details takes the kNN neighbor information as input and returns the 
!  whether the point lies in the interior of the grain or lies on the 
!  grain boundary. Details in pg 11 of the dictionary indexing paper
!
!> @param array input array
!> @param k number of top matches for each pixel
!> @param npx number of pixels in the x direction
!> @param npy number of pixels in the y direction
!
!> @date 01/05/15 SS 1.0 original
!> @date 01/06/15 MDG 1.1 simplified summation loop and renamed routine
!--------------------------------------------------------------------------
recursive subroutine DI_Similarity_Classifier(array,k,npx,npy,returnarr)
!DEC$ ATTRIBUTES DLLEXPORT :: DI_Similarity_Classifier

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: k
integer(kind=irg),INTENT(IN)            :: npx
integer(kind=irg),INTENT(IN)            :: npy
integer(kind=sgl),INTENT(IN)            :: array(npx,npy,k)
real(kind=sgl),INTENT(OUT)              :: returnarr(npx,npy)

integer(kind=irg)                       :: ii,jj,ki,kj,similarity_measure_sum,res
real(kind=sgl)                          :: similarity_measure


similarity_measure_sum = 0
similarity_measure = 0.0
returnarr = 0.0

do ii = 2,npx-1
    do jj = 2,npy-1
      do ki = -1, 1
        do kj = -1, 1
          if ((abs(ki)+abs(kj)).ne.0) then 
            call CardIntersection(array(ii+ki,jj+kj,1:k),array(ii,jj,1:k),k,res)
            similarity_measure_sum = similarity_measure_sum + res

            similarity_measure = float(similarity_measure_sum)/float(8*k)
            returnarr(ii,jj) = similarity_measure

            similarity_measure = 0.0
            similarity_measure_sum = 0
          end if
        end do
      end do
    end do
end do

end subroutine DI_Similarity_Classifier

!--------------------------------------------------------------------------
!
! SUBROUTINE: CardIntersection
!
!> @author Saransh Singh, Carnegie Mellon University / Yu-Hui Chen, U. Michigan
!
!> @brief calculate the cardinality of the intersection of two sets
!
!> @param set1
!> @param set2
!> @param k number of elements in each set
!
!> @date 01/05/15 SS 1.0 original
!> @date MDG 1.1 changed types to integer 
!--------------------------------------------------------------------------
recursive subroutine CardIntersection(set1,set2,k,res)
!DEC$ ATTRIBUTES DLLEXPORT :: CardIntersection

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: k
integer(kind=irg),INTENT(IN)            :: set1(k)
integer(kind=irg),INTENT(IN)            :: set2(k)
integer(kind=irg),INTENT(OUT)           :: res

integer(kind=irg)                       :: ii,jj
jj = 1
res = 0

do ii = 1,k
    do jj = 1,k
        if (set1(ii) .eq. set2(jj)) then
            res = res + 1
            EXIT
        end if
    end do
end do

end subroutine CardIntersection

!--------------------------------------------------------------------------
!
! SUBROUTINE: ReduceDisorientationtoMFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Reduce a disorientation (Rodrigues) to the Mackenzie Fundamental Zone
!
!> @details This requires the symmetry operations of the Rodrigues FZ, which 
!> includes mirrors and inversion symmetry, i.e., the regular (full) point group
!> symmetry of the shape of the RFZ.  We already have those implemented in the 
!> regular symmetry module, and we assume that those symmetry matrices have been
!> initialized and are present in the cell structure.  Then we just call the regular
!> CalcStar routine to generate the equivalents and pick the one that is inside
!> the MFZ.
!
!> @param ro Rodrigues vector
!> @param cell cell pointer
!> @param FZtype Fundamental Zone type
!> @param FZorder Fundamental Zone order
!> @param euFZ Euler triplet in fundamental zone (in radians)
!
!> @date 07/29/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ReduceDisorientationtoMFZ(ro, cell, FZtype, FZorder, roMFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: ReduceDisorientationtoMFZ

use local
use rotations
use quaternions
use so3
use typedefs
use symmetry

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: ro(4)
type(unitcell)        ,INTENT(IN)       :: cell
integer(kind=irg),INTENT(IN)            :: FZtype
integer(kind=irg),INTENT(IN)            :: FZorder
real(kind=dbl),INTENT(OUT)              :: roMFZ(4)

real(kind=dbl)                          :: r(3), rod(4), mag
integer(kind=irg)                       :: i, j, n
real(kind=dbl)                          :: stmp(48,3)
logical                                 :: inMFZ

roMFZ = 0.D0
r(1:3) = ro(1:3)*ro(4)
stmp = 0.0
call CalcStar(cell,r,n,stmp,'d')


MFZloop: do j=1,n
  mag = dsqrt(sum(stmp(j,1:3)**2))
  rod(4) = mag
  rod(1:3) = stmp(j,1:3)/mag
  inMFZ = IsinsideMFZ(rod, FZtype, FZorder)
  if (inMFZ) then
   roMFZ = rod
   EXIT MFZloop
  end if
! we really should never get to the following line ...
  if (j.eq.n) then
    roMFZ = (/ 0.D0, 0.D0, 1.D0, 0.D0 /)
    ! write (*,*) 'problem ... ',j, inMFZ
    ! write(*,*) r, n
    ! write(*,*) mag, rod
    ! write (*,*) transpose(stmp)
    ! stop
  end if
end do MFZloop

end subroutine ReduceDisorientationtoMFZ


!--------------------------------------------------------------------------
!
! SUBROUTINE: ReduceOrientationtoCubicEFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Reduce an orientation (Euler angles) to the cubic FZ in Euler space
!
!> @param eu Euler triplet (in radians)
!> @param dict dict structure
!> @param euFZ Euler triplet in Euler fundamental zone (in radians)
!
!> @date 07/29/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ReduceOrientationtoCubicEFZ(eu, dict, euFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: ReduceOrientationtoCubicEFZ

use local
use rotations
use quaternions
use constants
use so3

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: eu(3)
type(dicttype),INTENT(IN)               :: dict
real(kind=dbl),INTENT(OUT)              :: euFZ(3)

real(kind=dbl)                          :: Mu(4), qu(4), rod(4), c, s, z
integer(kind=irg)                       :: i, j, Pmdims

euFZ = 0.D0
Pmdims = dict%Nqsym

Mu = eu2qu(eu)
if (Mu(1).lt.0.D0) Mu = -Mu
FZloop: do j=1,Pmdims
  qu = quat_mult(dict%Pm(1:4,j),Mu)
  if (qu(1).lt.0.D0) qu = -qu
  euFZ = qu2eu(qu)
  ! apply the cubic Euler FZ boundary conditions
  c = cos(euFZ(3))
  s = sin(euFZ(3))
  z = acos(minval( (/ c/sqrt(1+c*c), s/sqrt(1+s*s) /) ))
  if ((euFZ(2).gt.z).and.(euFZ(2).lt.cPi/2D0).and.(euFZ(3).lt.cPi/2.D0)) EXIT FZloop
! we really should never get to the following line ...
  if (j.eq.Pmdims) write (*,*) 'problem ... ',i
end do FZloop

end subroutine ReduceOrientationtoCubicEFZ


!--------------------------------------------------------------------------
!
! SUBROUTINE: ReduceOrientationtoRFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Reduce an orientation (Euler angles) to the Rodrigues Fundamental Zone
!
!> @param eu Euler triplet (in radians)
!> @param dict dict structure
!> @param FZtype Fundamental Zone type
!> @param FZorder Fundamental Zone order
!> @param euFZ Euler triplet in fundamental zone (in radians)
!> @param MFZ (optonal) apply MacKenzie cell
!
!> @date 07/29/16 MDG 1.0 original
!> @date 03/27/17 MDG 1.1 added checking of MacKenzie cell
!> @date 11/19/18 MDG 1.2 correction of tolerance issue
!--------------------------------------------------------------------------
recursive subroutine ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, euFZ, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: ReduceOrientationtoRFZ

use local
use rotations
use quaternions
use so3
use constants

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: eu(3)
type(dicttype),INTENT(IN)               :: dict
integer(kind=irg),INTENT(IN)            :: FZtype
integer(kind=irg),INTENT(IN)            :: FZorder
real(kind=dbl),INTENT(OUT)              :: euFZ(3)
logical,OPTIONAL,INTENT(IN)             :: MFZ

real(kind=dbl)                          :: Mu(4), qu(4), rod(4)
integer(kind=irg)                       :: i, j, Pmdims
logical                                 :: useMFZ
real(kind=dbl)                          :: tol

tol = 1.0D+5

useMFZ = .FALSE.
if (present(MFZ)) then 
  if (MFZ.eqv..TRUE.) then
    useMFZ = .TRUE.
  end if 
endif

euFZ = 0.D0
Pmdims = dict%Nqsym

Mu = eu2qu(eu)
if (Mu(1).lt.0.D0) Mu = -Mu
FZloop: do j=1,Pmdims
  qu = quat_mult(dict%Pm(1:4,j),Mu)
  if (qu(1).lt.0.D0) qu = -qu
  rod = qu2ro(qu)
  if(abs(rod(4)) .gt. tol) rod(4) = inftyd()
  
  if (useMFZ.eqv..TRUE.) then
    if (IsinsideMFZ(rod,FZtype,FZorder)) EXIT FZloop
  else
    if (IsinsideFZ(rod,FZtype,FZorder))  EXIT FZloop
  end if
  ! we really should never get to the following line ...
  !if (j.eq.Pmdims) write (*,*) 'problem ... ',180.0*eu(1:3)/cPi,eu2ro(eu)
end do FZloop
euFZ = ro2eu(rod)

end subroutine ReduceOrientationtoRFZ

!--------------------------------------------------------------------------
!
! SUBROUTINE: getDisorientationAngleDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Determine the disorientation angle between two orientations (in radians)
!
!> @param eu1 first Euler triplet (in radians)
!> @param eu2 first Euler triplet (in radians)
!> @param dict dict structure
!> @param Pmdims number of symmetry operators to consider
!
!> @date 07/29/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getDisorientationAngleDouble(eu1, eu2, dict, disang, ax)
!DEC$ ATTRIBUTES DLLEXPORT :: getDisorientationAngleDouble

use local
use rotations
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: eu1(3)
real(kind=dbl),INTENT(IN)               :: eu2(3)
type(dicttype),INTENT(IN)               :: dict
real(kind=dbl),INTENT(OUT)              :: disang
real(kind=dbl),OPTIONAL,INTENT(OUT)     :: ax(3)


real(kind=dbl)                          :: Mu(4), qu(4), Mus(4), qus(4), p(4), ac, a
integer(kind=irg)                       :: j, k, Pmdims

disang = 0.D0
Pmdims = dict%Nqsym
a = sum(abs(eu1-eu2))

if (a.ne.0.D0) then
  Mu = eu2qu(eu1)
  if (Mu(1).lt.0.D0) Mu=-Mu
  qu = eu2qu(eu2)
  if (qu(1).lt.0.D0) qu=-qu
  ac = 1000.D0
  do j=1,Pmdims ! loop over the symmetric equivalents of Mu
    Mus =  quat_mult(dict%Pm(1:4,j),Mu)
    if (Mus(1).lt.0.D0) Mus=-Mus
    do k=1,Pmdims
      qus =  quat_mult(dict%Pm(1:4,k),qu)
      if (qus(1).lt.0.D0) qus=-qus
      p = quat_mult(Mus,conjg(qus))
      if (p(1).lt.0.D0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then
          ac = a
          if(present(ax)) then
              ax(1:3) = p(2:4)/vecnorm(p(2:4))
          end if
      end if
      p = quat_mult(qus,conjg(Mus))
      if (p(1).lt.0.D0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then
          ac = a
          if(present(ax)) then
              ax(1:3) = p(2:4)/vecnorm(p(2:4))
          end if
      end if
    end do
  end do 
  disang = ac
end if

end subroutine getDisorientationAngleDouble

!--------------------------------------------------------------------------
!
! SUBROUTINE: getDisorientationAngleSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Determine the disorientation angle between two orientations (in radians, single precision)
!
!> @param eu1 first Euler triplet (in radians)
!> @param eu2 first Euler triplet (in radians)
!> @param dict dict structure
!> @param Pmdims number of symmetry operators to consider
!
!> @date 07/29/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getDisorientationAngleSingle(eu1, eu2, dict, disang, ax) ! result(disang)
!DEC$ ATTRIBUTES DLLEXPORT :: getDisorientationAngleSingle

use local
use rotations
use quaternions

IMPLICIT NONE

real(kind=sgl),INTENT(IN)               :: eu1(3)
real(kind=sgl),INTENT(IN)               :: eu2(3)
type(dicttype),INTENT(INOUT)            :: dict
!f2py intent(in,out) ::  dict
real(kind=sgl),INTENT(OUT)              :: disang
real(kind=dbl),OPTIONAL,INTENT(OUT)     :: ax(3)

real(kind=sgl)                          :: Mu(4), qu(4), Mus(4), qus(4), p(4), ac, a
integer(kind=irg)                       :: j, k, Pmdims

disang = 0.0
Pmdims = dict%Nqsym
a = sum(abs(eu1-eu2))

if (a.ne.0.0) then
  Mu = eu2qu(eu1)
  if (Mu(1).lt.0.0) Mu=-Mu
  qu = eu2qu(eu2)
  if (qu(1).lt.0.0) qu=-qu
  ac = 1000.0
  do j=1,Pmdims ! loop over the symmetric equivalents of Mu
    Mus =  quat_mult(sngl(dict%Pm(1:4,j)),Mu)
    if (Mus(1).lt.0.0) Mus=-Mus
    do k=1,Pmdims
      qus =  quat_mult(sngl(dict%Pm(1:4,k)),qu)
      if (qus(1).lt.0.0) qus=-qus
      p = quat_mult(Mus,conjg(qus))
      if (p(1).lt.0.0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then 
          ac = a
          if(present(ax)) then
              ax(1:3) = p(2:4)/vecnorm(p(2:4))
          end if
      end if
      p = quat_mult(qus,conjg(Mus))
      if (p(1).lt.0.0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then
          ac = a
          if(present(ax)) then
              ax(1:3) = p(2:4)/vecnorm(p(2:4))
          end if
      end if 
    end do
  end do 
  disang = ac
end if

end subroutine getDisorientationAngleSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE: getDisorientationAngleAxis
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Determine the disorientation angle and axis between two orientations (in radians)
!
!> @param eu1 first Euler triplet (in radians)
!> @param eu2 first Euler triplet (in radians)
!> @param dict dict structure
!
!> @date 02/14/17 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getDisorientationAngleAxis(eu1, eu2, dict, disax)
!DEC$ ATTRIBUTES DLLEXPORT :: getDisorientationAngleAxis

use local
use rotations
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: eu1(3)
real(kind=dbl),INTENT(IN)               :: eu2(3)
type(dicttype),INTENT(IN)               :: dict
real(kind=dbl),INTENT(OUT)              :: disax(4)

real(kind=dbl)                          :: Mu(4), qu(4), Mus(4), qus(4), p(4), ac, a, ax(3)
integer(kind=irg)                       :: j, k, Pmdims

disax = 0.D0
Pmdims = dict%Nqsym
a = sum(abs(eu1-eu2))

if (a.ne.0.D0) then
  Mu = eu2qu(eu1)
  if (Mu(1).lt.0.D0) Mu=-Mu
  qu = eu2qu(eu2)
  if (qu(1).lt.0.D0) qu=-qu
  ac = 1000.D0
  do j=1,Pmdims ! loop over the symmetric equivalents of Mu
    Mus =  quat_mult(dict%Pm(1:4,j),Mu)
    if (Mus(1).lt.0.D0) Mus=-Mus
    do k=1,Pmdims
      qus =  quat_mult(dict%Pm(1:4,k),qu)
      if (qus(1).lt.0.D0) qus=-qus
      p = quat_mult(Mus,conjg(qus))
      if (p(1).lt.0.D0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then
        ac = a
        ax(1:3) = p(2:4)/vecnorm(p(2:4))
      end if
      p = quat_mult(qus,conjg(Mus))
      if (p(1).lt.0.D0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then
        ac = a
        ax(1:3) = p(2:4)/vecnorm(p(2:4))
      end if
    end do
  end do 
  disax(1:3) = ax(1:3)
  disax(4) = ac
end if

end subroutine getDisorientationAngleAxis

!--------------------------------------------------------------------------
!
! SUBROUTINE: getDisorientationAngleAxisTwoPhases
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Determine the disorientation angle and axis between two orientations (in radians)
!
!> @param eu1 first Euler triplet (in radians)
!> @param eu2 first Euler triplet (in radians)
!> @param dict1 dict1 structure
!> @param dict2 dict2 structure
!> @param disax smallest rotation angle disorientation axis-angle pair
!
!> @date 02/14/17 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getDisorientationAngleAxisTwoPhases(eu1, eu2, dict1, dict2, disax)
!DEC$ ATTRIBUTES DLLEXPORT :: getDisorientationAngleAxisTwoPhases

use local
use rotations
use quaternions

IMPLICIT NONE

real(kind=dbl),INTENT(IN)               :: eu1(3)
real(kind=dbl),INTENT(IN)               :: eu2(3)
type(dicttype),INTENT(IN)               :: dict1
type(dicttype),INTENT(IN)               :: dict2
real(kind=dbl),INTENT(OUT)              :: disax(4)

real(kind=dbl)                          :: Mu(4), qu(4), Mus(4), qus(4), p(4), ac, a, ax(3), axax(4)
integer(kind=irg)                       :: j, k, Pmdims1, Pmdims2

disax = 0.D0
Pmdims1 = dict1%Nqsym
Pmdims2 = dict2%Nqsym
a = sum(abs(eu1-eu2))

if (a.ne.0.D0) then
  Mu = eu2qu(eu1)
  if (Mu(1).lt.0.D0) Mu=-Mu
  qu = eu2qu(eu2)
  if (qu(1).lt.0.D0) qu=-qu
  ac = 1000.D0
  do j=1,Pmdims1 ! loop over the symmetric equivalents of Mu
    Mus =  quat_mult(dict1%Pm(1:4,j),Mu)
    if (Mus(1).lt.0.D0) Mus=-Mus
    do k=1,Pmdims2
      qus =  quat_mult(dict2%Pm(1:4,k),qu)
      if (qus(1).lt.0.D0) qus=-qus
      p = quat_mult(Mus,conjg(qus))
      if (p(1).lt.0.D0) p=-p
      a = 2.0*acos(p(1))
      if (a.lt.ac) then
        ac = a
        ax(1:3) = p(2:4)/vecnorm(p(2:4))
      end if
    end do
  end do 
  disax(1:3) = ax(1:3)
  disax(4) = ac
end if

end subroutine getDisorientationAngleAxisTwoPhases

!--------------------------------------------------------------------------
!
! SUBROUTINE: getAverageDisorientationMapSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Determine the average disorientation map (in radians, single precision)
!
!> @param eulers Euler angle list (3 x Nexp)
!> @param dict dict structure
!> @param wd width of map
!> @param ht height of map
!> @param ADMap output map
!
!> @date 03/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getAverageDisorientationMapSingle(eulers, dict, wd, ht, ADMap) 
!DEC$ ATTRIBUTES DLLEXPORT :: getAverageDisorientationMapSingle

use local
use constants
use rotations
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: wd
integer(kind=irg),INTENT(IN)            :: ht
real(kind=sgl),INTENT(IN)               :: eulers(3, wd*ht)
type(dicttype),INTENT(INOUT)            :: dict
!f2py intent(in,out) ::  dict
real(kind=sgl),INTENT(OUT)              :: ADMap(wd,ht)

integer(kind=irg)                       :: i, j, ic, icr, ict
real(kind=sgl)                          :: misor(4,wd,ht), denom(wd, ht), disang

ADMap = 0.0
denom = 4.0
misor = 0.0   ! contains four misorientation angles in the order r, t, l, b 

!edges
denom(2:wd-1,1) = 3.0
denom(2:wd-1,ht) = 3.0
denom(1,2:ht-1) = 3.0
denom(wd,2:ht-1) = 3.0
! corners
denom(1,1) = 2.0
denom(1,ht) = 2.0
denom(wd,1) = 2.0
denom(wd,ht) = 2.0

! we'll do this line by line (horizontally)
do j=1,ht
  do i=1,wd
    ic = wd*(j-1)+i
    icr = wd*(j-1)+i+1
    ict = wd*j + i

! right neighbor (also includes left one)
    if (i.lt.wd) then 
      call getDisorientationAngleSingle(eulers(1:3,ic), eulers(1:3,icr), dict, disang)
      misor(1,i,j) = disang
      misor(3,i+1,j) = disang
    end if

! top neighbor
    if (j.lt.ht) then
      call getDisorientationAngleSingle(eulers(1:3,ic), eulers(1:3,ict), dict, disang)
      misor(2,i,j) = disang
      misor(4,i,j+1) = disang
    end if
  end do 
end do 

! then take the average
ADMap = sum(misor,1)/denom

! and convert to degrees
ADMap = ADMap * 180.0/sngl(cPi)

end subroutine getAverageDisorientationMapSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE: getAverageDisorientationMapDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Determine the average disorientation map (in radians, double precision)
!
!> @param eulers Euler angle list (3 x Nexp)
!> @param dict dict structure
!> @param wd width of map
!> @param ht height of map
!> @param ADMap output map
!
!> @date 03/13/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getAverageDisorientationMapDouble(eulers, dict, wd, ht, ADMap) 
!DEC$ ATTRIBUTES DLLEXPORT :: getAverageDisorientationMapDouble

use local
use constants
use rotations
use quaternions

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: wd
integer(kind=irg),INTENT(IN)            :: ht
real(kind=dbl),INTENT(IN)               :: eulers(3, wd*ht)
type(dicttype),INTENT(INOUT)            :: dict
!f2py intent(in,out) ::  dict
real(kind=dbl),INTENT(OUT)              :: ADMap(wd,ht)

integer(kind=irg)                       :: i, j, ic, icr, ict
real(kind=dbl)                          :: misor(4,wd,ht), denom(wd, ht), disang

ADMap = 0.D0
denom = 4.D0
misor = 0.D0   ! contains four misorientation angles in the order r, t, l, b 

!edges
denom(2:wd-1,1) = 3.D0
denom(2:wd-1,ht) = 3.D0
denom(1,2:ht-1) = 3.D0
denom(wd,2:ht-1) = 3.D0
! corners
denom(1,1) = 2.D0
denom(1,ht) = 2.D0
denom(wd,1) = 2.D0
denom(wd,ht) = 2.D0

! we'll do this line by line (horizontally)
do j=1,ht
  do i=1,wd
    ic = wd*(j-1)+i
    icr = wd*(j-1)+i+1
    ict = wd*j + i

! right neighbor (also includes left one)
    if (i.lt.wd) then 
      call getDisorientationAngleDouble(eulers(1:3,ic), eulers(1:3,icr), dict, disang)
      misor(1,i,j) = disang
      misor(3,i+1,j) = disang
    end if

! top neighbor
    if (j.lt.ht) then
      call getDisorientationAngleDouble(eulers(1:3,ic), eulers(1:3,ict), dict, disang)
      misor(2,i,j) = disang
      misor(4,i,j+1) = disang
    end if
  end do 
end do 

! then take the average
ADMap = sum(misor,1)/denom

! and convert to degrees
ADMap = ADMap * 180.D0/cPi

end subroutine getAverageDisorientationMapDouble

end module dictmod
