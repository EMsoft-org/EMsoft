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

program EMEBSDreflectors

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
type(reflectorNameListType)             :: rnl
integer(kind=irg)                       :: res, error_cnt

nmldeffile = 'EMEBSDreflectors.nml'
progname = 'EMEBSDreflectors.f90'
progdesc = 'Determine list of most intense EBSD reflectors (dynamical)'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 26 /), progname)

! deal with the namelist stuff, either .nml or .json format
! res = index(nmldeffile,'.nml',kind=irg)
! if (res.eq.0) then
!   call JSONreadreflectorNameList(rnl, nmldeffile, error_cnt)
! else
call GetreflectorNameList(nmldeffile,rnl)
! end if

! perform a Monte Carlo simulation
call GetReflectors(rnl, progname, nmldeffile)

end program EMEBSDreflectors

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetReflectors
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a list of strongest reflectors based on dynamical simulations
!
!> @param rnl reflector name list
!> @param nmlfile namelist file name
!
!> @date 05/31/16  MDG 1.0 original
!> @date 02/12/18  MDG 1.1 converted to most recent h5 format; modified reflection selection criteria
!> @date 06/06/18  MDG 1.2 modified discrete integration 
!> @date 11/29/18  MDG 1.3 added kinematical X-ray intensities to output
!> @date 03/01/20  MDG 1.4 add ability to compute a kinematical pattern 
!> @date 12/09/20  MDG 1.5 improved layout of LaTeX table and added some print statements for long runs
!--------------------------------------------------------------------------
subroutine GetReflectors(rnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use EBSDmod
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
use clfortran
use CLsupport
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use stringconstants
use omp_lib

IMPLICIT NONE

type(reflectorNameListType),INTENT(INOUT)    :: rnl
character(fnlen),INTENT(IN)                  :: progname
character(fnlen),INTENT(IN)                  :: nmldeffile

type(EBSDNameListType)                       :: enl
type(MCCLNameListType)                       :: mcnl
type(EBSDMasterNameListType)                 :: mpnl
type(EBSDMCdataType)                         :: EBSDMCdata
type(EBSDMPdataType)                         :: EBSDMPdata

character(fnlen)                             :: listfile, masterfile, groupname, dataset, xtalname, outputfile, infile
logical                                      :: f_exists, readonly, verbose
integer(kind=irg)                            :: hdferr, nlines, i, istat, ix, iy, nx, io_int(1), nkeep, nl2, k2
integer(HSIZE_T)                             :: dims3(3), dims4(3)
real(kind=dbl)                               :: EkeV
real(kind=sgl)                               :: m

integer(kind=irg)                            :: imh, imk, iml, ii, j, num, nums, mhkl, valpos, numphi,numtheta,iequiv(3,48),nequiv
integer(kind=irg),allocatable                :: family(:,:,:),numfam(:),idx(:), idx2(:), sfi(:)
integer(kind=irg)                            :: h,k,l,totfam,ind(3),icnt, oi_int(1), itmp(48,3), g1(3), g2(3), NUMTHREADS, TID
logical                                      :: first
logical,allocatable                          :: z(:,:,:)
real(kind=sgl)                               :: g(3), thr, dphi, gc(3), gax(3), gz(3), v(3), qu(4), ax(4), x, val1, val2, valmax
real(kind=sgl),allocatable                   :: Vgg(:),ddg(:),gg(:),th(:), gcart(:,:), gcrys(:,:), cp(:), sp(:), ca(:), sa(:), &
                                                phi(:), theta(:), dc(:,:), Vg(:), VgX(:), VggX(:), cosnorm(:)
character(1)                                 :: space
real(kind=sgl)                               :: dhkl, incrad, glen, sd
real(kind=sgl)                               :: ixy(2),scl
real(kind=sgl)                               :: dx,dy,dxm,dym
integer(kind=irg)                            :: jj,kk
integer(kind=irg)                            :: nix,niy,nixp,niyp
logical,allocatable                          :: keep(:)

integer(kind=irg),allocatable                :: acc_e(:,:,:)
real(kind=sgl),allocatable                   :: Eweights(:)
real(kind=sgl),allocatable                   :: srtmp(:,:,:,:), mLPNH(:,:,:), mLPSH(:,:,:), masterNH(:,:), masterSH(:,:), &
                                                KBI(:), kinmasterNH(:,:), kinmasterSH(:,:), kinNH(:,:), kinSH(:,:)
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)

type(unitcell)          :: cell
type(DynType),save      :: Dyn
type(gnode),save        :: rlp
type(HDFobjectStackType)          :: HDF_head

verbose = .FALSE.

nullify(HDF_head%next)

thr = 1.E-5
space = 'r'

! 1. read the Monte Carlo data file (HDF format)
call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(rnl%masterfile, mcnl, hdferr, EBSDMCdata, getAccume=.TRUE.)

!------------------------------
! compute the energy weight factors by integrating the lower rectangular portion
! of the Lambert projection; we'll take the lower quarter in vertical dimension
! and a similar distance to left and right in the horizontal direction
!------------------------------
dims3 = shape(EBSDMCdata%accum_e)
allocate(Eweights(dims3(1)))
Eweights = 0.0
do i=1,dims3(1)
  Eweights(i) = sum(EBSDMCdata%accum_e(i,-dims3(2)/4:dims3(2)/4,-dims3(3)/2:-dims3(3)/4))
end do
Eweights = Eweights/maxval(Eweights)
deallocate(EBSDMCdata%accum_e)

! 2. read EBSD master pattern file (HDF format)
call readEBSDMasterPatternFile(rnl%masterfile, mpnl, hdferr, EBSDMPdata, getmLPNH=.TRUE., getmLPSH=.TRUE.)
call h5close_EMsoft(hdferr)

dims4 = shape(EBSDMPdata%mLPNH)
nx = (dims4(1)-1)/2

! perform E-weighted averaging to get a single NH+SH master pattern
allocate(masterNH(-nx:nx,-nx:nx),stat=istat)
allocate(masterSH(-nx:nx,-nx:nx),stat=istat)
masterNH = 0.0
masterSH = 0.0
do ix=-nx,nx
  do iy=-nx,nx
    masterNH(ix,iy) = sum(EBSDMPdata%mLPNH(ix,iy,1:dims4(3))*Eweights(1:dims3(1))) 
    masterSH(ix,iy) = sum(EBSDMPdata%mLPSH(ix,iy,1:dims4(3))*Eweights(1:dims3(1))) 
  end do
end do
deallocate(EBSDMPdata%mLPNH, EBSDMPdata%mLPSH)

! do we need to generate a kinematical master pattern ?
if (rnl%kinematical.eqv..TRUE.) then 
  allocate(kinmasterNH(-nx:nx, -nx:nx), kinmasterSH(-nx:nx,-nx:nx) )
  kinmasterNH = 0.0 
  kinmasterSH = 0.0 
end if 

! subtract the average value from the master pattern arrays and divide by the standard deviation
m = sum(masterNH)/float((2*nx+1)**2)
masterNH = masterNH - m
sd = sqrt( sum(masterNH**2) / (float((2*nx+1)**2 - 1)))
masterNH = masterNH / sd

m = sum(masterSH)/float((2*nx+1)**2)
masterSH = masterSH - m
sd = sqrt( sum(masterSH**2) / (float((2*nx+1)**2 - 1)))
masterSH = masterSH / sd

! ok, now we have the averaged master pattern; next we need to init the crystal
! structure, get a list of unique reflectors, and for each one integrate the 
! Kikuchi band...

! initialize the crystal structure and compute a list of potential reflectors 
!nullify(cell)        
!allocate(cell)        

! get the crystal structure from the *.xtal file
verbose = .TRUE.
call Initialize_Cell(cell,Dyn,rlp,mcnl%xtalname,rnl%dmin, sngl(mcnl%EkeV), verbose)

! generate a list of hkl indices for which we need to compute the integral over the Kikuchi band
! since the commercial packages use the kinematical structure factor to generate this list, we
! will do the same...

! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.rnl%dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.rnl%dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.rnl%dmin) EXIT
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
     rlp%method = 'DT'
     call CalcUcg(cell,rlp,ind)

! but ignore the reciprocal lattice point if Vgg is small
     if (abs(rlp%Ucg).ge.thr) then 

! copy family in array and label all its members and multiples in z-array
      call CalcFamily(cell,ind,num,space,itmp)
      do i=1,num
       do j=1,3
        family(icnt,i,j)=itmp(i,j)
       end do
       z(itmp(i,1),itmp(i,2),itmp(i,3))=.TRUE.
      end do

! compute the X-ray kinematical structure factor
      rlp%method = 'XR'
      call CalcUcg(cell,rlp,ind)
      VggX(icnt) = abs(rlp%Ucg)

! also get the structure factor with the WK parameters and absorption
      rlp%method = 'WK'
      call CalcUcg(cell,rlp,ind)
      Vgg(icnt) = rlp%Vmod

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

! compute d-spacings, g-spacings, theta
call Message(' Computing d-spacings, g-spacings, and scattering angles')
 allocate(gcart(3,icnt),gcrys(3,icnt))
 do k=1,icnt
  g(1:3)=float(family(k,1,1:3))
  gg(k)=CalcLength(cell,g,'r')
  gcrys(1:3,k) = g(1:3) 
  call TransSpace(cell,g,gc,'r','c')
  call NormVec(cell,gc,'c')
  gcart(1:3,k) = gc(1:3) 
  th(k)=asin(0.5*cell%mLambda*gg(k))
 end do

! here we need to eliminate those entries that are multiples of a smaller hkl
! and only keep the one that has the largest structure factor.

allocate(idx(icnt))
call SPSORT(gg,icnt,idx,1,istat)

allocate(keep(icnt))
keep = .TRUE.
keep(idx(1)) = .FALSE.   ! eliminate (000) from the list

mhkl = int(maxval(gcrys))

call Message(' Selecting lowest hkl values with largest structure factor ')
do k=2,icnt-1
 if (keep(idx(k)).eqv..TRUE.) then
  valpos = idx(k)
  g1 = int(gcrys(:,valpos))
  val1 = VggX(valpos)
  valmax = val1
  keep(valpos) = .TRUE.
!  write(*,*) '-> ',g1, valmax, valpos
! scan through the multiples
  do j=2,mhkl
    g2 = j * g1
    do i=k+1,icnt 
      if (sum(abs(g2-int(gcrys(:,idx(i))))).eq.0) then
        if (VggX(idx(i)).gt.valmax) then
          keep(valpos) = .FALSE.
          valpos = idx(i)
          valmax = VggX(valpos)
          keep(valpos) = .TRUE.
        else
          keep(idx(i)) = .FALSE.
        end if
!  if (all(g1.eq.(/1,0,0/))) write (*,*) k, i,': ', g1,';',g2,';',g2-j*g1,'   -> ',VggX(idx(i)), valmax, valpos
      end if
    end do 
  end do
 end if
end do

nkeep = 0
do i=1,icnt
  if (keep(i).eqv..TRUE.) nkeep = nkeep+1
end do

! and here is the main part of this program: Kikuchi band integration for 
! each unique family (one member per family).  

! allocate the output arrays (KikuchiBandIntegral = KBI)
allocate(KBI(icnt),Vg(icnt),VgX(icnt))
KBI = 0.0

! azimuthal integration angle
numphi = 360.0/rnl%increment
allocate(phi(numphi), cp(numphi),sp(numphi))
dphi = rnl%increment * sngl(cPi)/180.0
phi = (/ (float(i-1)*dphi,i=1,numphi) /)
cp = cos(phi)
sp = sin(phi)

incrad = rnl%increment * sngl(cPi)/180.0
scl = float(nx)

! set the number of OpenMP threads 
call OMP_SET_NUM_THREADS(rnl%nthreads)
io_int(1) = rnl%nthreads
call WriteValue(' Setting # threads to ',io_int,1,"(I3)")
io_int(1) = nkeep
call WriteValue(' Total number of integrations to be carried out ',io_int,1,"(I6)")

if (rnl%kinematical.eqv..TRUE.) then 
  call Message(' Computation of symmetrized kinematical pattern will slow things down a bit ... ')
end if 

call Message(' Starting parallel integrations... (.=100, |=1000) ')
! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL DEFAULT(PRIVATE) &
!$OMP& SHARED(k, nx, cp, sp, icnt, keep, th, incrad, numphi, gcart, cell, scl, masterNH, masterSH) &
!$OMP& SHARED(Vg, VgX, Vgg, VggX, KBI, nkeep, kinmasterNH, kinmasterSH)

NUMTHREADS = OMP_GET_NUM_THREADS()
TID = OMP_GET_THREAD_NUM()

allocate(kinNH(-nx:nx,-nx:nx), kinSH(-nx:nx,-nx:nx))
kinNH = 0.0
kinSH = 0.0

!$OMP DO SCHEDULE(STATIC,rnl%nthreads)
do k=1,icnt-1   ! ignore the last point
 if (keep(k)) then
  ii = nint(th(k)/incrad)
  numtheta = 2*ii+1
  allocate(theta(numtheta),ca(numtheta),sa(numtheta))
  nums = numphi * numtheta
  allocate( dc(3,nums), cosnorm(nums) )

  theta = (/ (float(i),i=-ii,ii) /) * incrad
  gz = (/ 0.0, 0.0, 1.0 /)

! get the unrotated direction cosines of the sampling points on the sphere
! also initialize the cosine term in the surface integration, and the segment normalization
    ca = cos( theta )
    sa = sin( theta )
    ii = 1
    do i=1,numphi
      do j=1,numtheta
        dc(1,ii) = ca(j) * cp(i)
        dc(2,ii) = ca(j) * sp(i)
        dc(3,ii) = sa(j) 
        cosnorm(ii) = ca(j) / ( 4.0 * sngl(cPi) * sin(th(k)) )
        ii = ii+1
      end do
    end do

! then determine the rotation quaternion to bring the z axis onto the g direction (cartesian)
    v = gcart(1:3,k) 
    x = CalcDot(cell,gz,v,'c')
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
      do i=1,nums
        v(:) = dc(:,i)
        v = quat_LP(qu,v)
        call NormVec(cell,v,'c')
        dc(:,i) = v(:)
      end do
    end if

! now that all the points have been rotated, we simply transform the direction cosines
! into square Lambert coordinates and interpolate from the master patterns in the usual way...
    do i=1,nums
  ! convert these direction cosines to coordinates in the Rosca-Lambert projection
      v(:) = dc(:,i)
      call LambertgetInterpolation(v, scl, nx, nx, nix, niy, nixp, niyp, dx, dy, dxm, dym)

  ! interpolate the intensity
      if (dc(3,i) .ge. 0.0) then
         KBI(k) = KBI(k)+ ( masterNH(nix,niy) * dxm * dym +  masterNH(nixp,niy) * dx * dym + &
                            masterNH(nix,niyp) * dxm * dy +  masterNH(nixp,niyp) * dx * dy ) * 0.25 * cosnorm(i)
      else
         KBI(k) = KBI(k)+ ( masterSH(nix,niy) * dxm * dym +  masterSH(nixp,niy) * dx * dym + &
                            masterSH(nix,niyp) * dxm * dy +  masterSH(nixp,niyp) * dx * dy ) * 0.25 * cosnorm(i)
      end if
      Vg(k) = Vgg(k)
      VgX(k) = VggX(k)
      if (rnl%kinematical.eqv..TRUE.) then ! add the kinematical intensity and symmetrize it 
        call Apply3DPGSymmetry(cell,nix,niy,1,nx,iequiv,nequiv)
        do ix=1,nequiv
          if (iequiv(3,ix).eq.-1) then 
            kinSH(iequiv(1,ix),iequiv(2,ix)) = kinSH(iequiv(1,ix),iequiv(2,ix)) + Vg(k)
          else
            kinNH(iequiv(1,ix),iequiv(2,ix)) = kinNH(iequiv(1,ix),iequiv(2,ix)) + Vg(k)
          end if 
        end do
      end if 
    end do
    deallocate(theta,ca,sa,cosnorm,dc)
 else
    Vg(k) = 0.0
    VgX(k) = 0.0
 end if
 if (mod(k,100).eq.0) then
  if (mod(k,1000).eq.0) then
     write (*,"('|')",advance="no")
   else 
     write (*,"('.')",advance="no")
   end if
 end if
end do
!$OMP END DO

!$OMP CRITICAL
if (rnl%kinematical.eqv..TRUE.) then
  kinmasterNH = kinmasterNH + kinNH
  kinmasterSH = kinmasterSH + kinSH
end if 
!$OMP END CRITICAL

!$OMP END PARALLEL

call Message(' done ')

x = maxval(KBI)
KBI = KBI * 100.0/x
Vg(icnt) = 0.0
x = maxval(Vg)
Vg = Vg * 100.0/x
VgX(icnt) = 0.0
x = maxval(VgX)
VgX = VgX * 100.0/x

allocate(idx2(icnt))
call SPSORT(Vg,icnt,idx2,-1,istat)
call SPSORT(KBI,icnt,idx,-1,istat)

allocate(sfi(icnt))
do i=1,icnt
  k = idx2(i)
  sfi(k) = i
end do

listfile = trim(EMsoft_getEMdatapathname())//trim(rnl%listfile)
listfile = EMsoft_toNativePath(listfile)

if ((trim(rnl%outputformat).eq.'latex').or.(trim(rnl%outputformat).eq.'all')) then 
  outputfile = trim(listfile)//'.tex'
  open(unit=80,file=trim(outputfile),status='unknown',form='formatted')

! format everything as a LaTeX table, with rank, hkl, |g|, KBI, Vg (sfi)
  write (80,"('\begin{table}[th]\caption{reflector ranking}\centering\leavevmode\begin{tabular}{llrrrcllrrr}')")
  write (80,"('\hline $\#$ & $(hkl)$ & $\beta_{hkl}$ & $I^{\text{abs}}_{hkl}$ & $I^{\text{X}}_{hkl}$ & $\quad$ &')")
  write (80,"('$\#$ & $(hkl)$ & $\beta_{hkl}$ & $I^{\text{abs}}_{hkl}$ & $I^{\text{X}}_{hkl}$\\')")
  write (80,"('\hline')")
  if (mod(rnl%numlist,2).eq.0) then 
    nl2 = rnl%numlist/2
  else 
    nl2 = (rnl%numlist+1)/2
  end if 
  do i=1,nl2  ! rnl%numlist
    k = idx(i)
    if ((i+nl2).le.rnl%numlist) then 
      k2 = idx(i+nl2)
    else
      k2 = -1
    end if
! the first reflection for this line in the table
    if ((sum(abs(gcrys(:,k))).ne.0.0).and.(KBI(k).ne.0.0)) then
      glen = CalcLength(cell,gcrys(:,k),'r')
        write (80,"(I2,'& $(')",advance="no") i
        do jj=1,3
          if (int(gcrys(jj,k)).lt.0) then
            if (jj.lt.3) then
              write (80,"('\bar{',I3,'}\,')",advance="no") abs(int(gcrys(jj,k)))
            else
              write (80,"('\bar{',I3,'}')",advance="no") abs(int(gcrys(jj,k)))
            end if
          else
            if (jj.lt.3) then
              write (80,"(I3,'\,')",advance="no") int(gcrys(jj,k))
            else
              write (80,"(I3)",advance="no") int(gcrys(jj,k))
            end if
          end if
        end do
        write (80,"(')$ & ',F6.2,' & ',F6.2,' & ',F6.2,' & &')") KBI(k), Vg(k), VgX(k)
    end if
! the second reflection for this line in the table
    if ((sum(abs(gcrys(:,k2))).ne.0.0).and.(KBI(k2).ne.0.0).and.(k2.ne.-1)) then
      glen = CalcLength(cell,gcrys(:,k2),'r')
        write (80,"(I2,'& $(')",advance="no") i+nl2
        do jj=1,3
          if (int(gcrys(jj,k2)).lt.0) then
            if (jj.lt.3) then
              write (80,"('\bar{',I3,'}\,')",advance="no") abs(int(gcrys(jj,k2)))
            else
              write (80,"('\bar{',I3,'}')",advance="no") abs(int(gcrys(jj,k2)))
            end if
          else
            if (jj.lt.3) then
              write (80,"(I3,'\,')",advance="no") int(gcrys(jj,k2))
            else
              write (80,"(I3)",advance="no") int(gcrys(jj,k2))
            end if
          end if
        end do
        write (80,"(')$ & ',F6.2,' & ',F6.2,' & ',F6.2,'\\ ')") KBI(k2), Vg(k2), VgX(k2)
    end if
    if (k2.eq.-1) write (80,"('\\')")
  end do
  write(80,"('\hline\end{tabular}\end{table}')")
  close(unit=80,status='keep')
  call Message('Data stored in LaTeX file '//trim(outputfile))
end if


if ((trim(rnl%outputformat).eq.'csv').or.(trim(rnl%outputformat).eq.'all')) then 
  outputfile = trim(listfile)//'.csv'
  open(unit=80,file=trim(outputfile),status='unknown',form='formatted')

  write (80,"(A)") '#,h,k,l,KBI,Ikin+abs,IX' 

  do i=1,rnl%numlist
    k = idx(i)
    if ((sum(abs(gcrys(:,k))).ne.0.0).and.(KBI(k).ne.0.0)) then
      write (80,"(I4,',',3(I3,','),F6.2,',',F6.2,',',F6.2)") i,int(gcrys(:,k)),KBI(k),Vg(k),VgX(k)
    end if
  end do
  close(unit=80,status='keep')
  call Message('Data stored in .csv file '//trim(outputfile))
end if

if ((trim(rnl%outputformat).eq.'markdown').or.(trim(rnl%outputformat).eq.'all')) then 
  outputfile = trim(listfile)//'.md'
  open(unit=80,file=trim(outputfile),status='unknown',form='formatted')

  write (80,"(A)") '##EBSD Dynamical Reflector Ranking for '//trim(mcnl%xtalname) 
  write (80,"(A)") ' '
  write (80,"(A)") '|\# | (hkl) | beta_hkl | Ikin+abs |    IX   |' 
  write (80,"(A)") '|---|-------|----------|----------|---------|' 

  do i=1,rnl%numlist
    k = idx(i)
    if ((sum(abs(gcrys(:,k))).ne.0.0).and.(KBI(k).ne.0.0)) then
        write (80,"('|',I2,'| (')",advance="no") i
        do jj=1,3
          write (80,"(I3)",advance="no") int(gcrys(jj,k))
        end do
        write (80,"(')|',F6.2,'|',F6.2,'|',F6.2,'|')") KBI(k), Vg(k), VgX(k)
    end if
  end do
  close(unit=80,status='keep')
  call Message('Data stored in .md file '//trim(outputfile))
end if

! do we need to store the kinematical patterns in the MP file ?
if (rnl%kinematical.eqv..TRUE.) then 
  call h5open_EMsoft(hdferr)

  infile = trim(EMsoft_getEMdatapathname())//trim(rnl%masterfile)
  infile = EMsoft_toNativePath(infile)

  nullify(HDF_head%next)
  hdferr =  HDF_openFile(infile, HDF_head)

  groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  groupname = SC_EBSDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

write (*,*) 'maxval = ', maxval(kinmasterNH), maxval(kinmasterSH)

  dataset = 'kinmasterNH'
  hdferr = HDF_writeDatasetFloatArray2D(dataset, kinmasterNH, 2*nx+1, 2*nx+1, HDF_head)

  dataset = 'kinmasterSH'
  hdferr = HDF_writeDatasetFloatArray2D(dataset, kinmasterSH, 2*nx+1, 2*nx+1, HDF_head)

  call HDF_pop(HDF_head,.TRUE.)

  call h5close_EMsoft(hdferr)
end if 



end subroutine GetReflectors
