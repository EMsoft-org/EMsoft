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
! EMsoft:STEMmodule.f90
!--------------------------------------------------------------------------
!
! MODULE: STEMmodule
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Provides routines to handle the STEM detector geometry and weightfactors.
! 
!> @date   04/29/11 MDG 1.0 original
!> @date   06/12/13 MDG 2.0 rewrite 
!--------------------------------------------------------------------------
module STEMmodule

use local
use typedefs

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: init_STEM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the weight factors for the systematic row case.
!
!> @param STEM STEM structure
!> @param stemnl STEM namelist
!> @param cell unit cell pointer
!> @param nn number of beams
!> @param g systematic row basic vector
! 
!> @date   04/29/11 MDG 1.0 original
!> @date   06/12/13 MDG 2.0 rewrite 
!> @date   06/09/14 MDG 3.0 added STEM and cell arguments
!> @date   07/02/17 MDG 3.1 split STEM into STEM and stemnl
!--------------------------------------------------------------------------
recursive subroutine init_STEM(STEM,stemnl,cell,nn,g)
!DEC$ ATTRIBUTES DLLEXPORT :: init_STEM

use io
use crystal
use diffraction
use NameListTypeDefs

IMPLICIT NONE

type(STEMtype),INTENT(INOUT)            :: STEM
!f2py intent(in,out) ::  STEM
type(STEMGeometryNameListType),INTENT(INOUT)    :: stemnl
!f2py intent(in,out) ::  stemnl
type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: nn
integer(kind=irg),INTENT(IN)            :: g(3)

integer(kind=irg)                       :: i,j,n,ira,jj,k,kk, iCL
real(kind=sgl)                          :: glen, thb, alp, omega_c, omega_min, omega_max,omega,a,b,c,th,dom,p,q,dr,dx
real(kind=sgl),parameter                :: cPi=3.141592654

! these are only used to debug this routine
real(kind=sgl),allocatable              :: thetar(:),outar(:,:,:)
logical                                 :: debug = .FALSE., diffappresent = .FALSE., apinBF=.FALSE. , apinADF = .FALSE.

! this routine initializes the excitation error arrays and the weight-factor arrays for systematic row STEM signals
! we'll assume that the central beam is centered on the BF detector; then we can 
! compute the complete geometry by working in mrad units throughout.

! allocate the excitation error array areal(1..nn,1..stemnl%numberofsvalues)
  allocate(STEM%sgarray(nn,stemnl%numberofsvalues))

! determine the lower and upper bounds of the excitation error for the fundamental reflection G
  thb = CalcDiffAngle(cell,g(1),g(2),g(3))*0.5  ! Bragg angle in radians

! convert k_t to the alp and omega angles (in radians)
  glen = CalcLength(cell,float(g),'r')
  alp = -2.0*stemnl%kt*thb
  omega_c = cPi*0.5+alp
  omega_min = omega_c - stemnl%beamconvergence/1000.0
  omega_max = omega_c + stemnl%beamconvergence/1000.0

! step size
  dom = (omega_max - omega_min)/float(stemnl%numberofsvalues-1)

! and for each value in between, compute each reflection's excitation error
ira = (nn+1)/2

do j=1,stemnl%numberofsvalues
! set omega angle
   omega = omega_min+float(j-1)*dom
   do i=1,nn
    n = -ira+i
! excitation error
    STEM%sgarray(nn+1-i,j) = -n*glen*cos(omega)-(1.0-sqrt(1.0-(n*cell%mLambda*glen*sin(omega))**2))/cell%mLambda
   end do
end do

if (debug) then
  allocate(thetar(25))
  thetar = 0.0
  thetar(1:7) = (/ a,b,c,th,dom,dr,dx /)
end if 

! next, we compute the weightfactors, i.e., how much does each excitation error value contribute
! to the BF or ADF signal?  The weight factor is basically the length of the chord across the overlap
! area of the diffraction disk and the detector, which requires a little bit of math to figure out;
! the math employs the concept of the radical line (see mathworld.com section on circle-circle intersections)

! this computation is carried out in mrad units !
allocate(STEM%BFweightsarray(nn,stemnl%numberofsvalues,stemnl%numCL), &
         STEM%ADFweightsarray(nn,stemnl%numberofsvalues,stemnl%numCL))

STEM%BFweightsarray = 0.0
STEM%ADFweightsarray = 0.0


outerCLloop: do iCL=1,stemnl%numCL    ! this is the outer loop over the microscope camera lengths (very long loop !!!)

! fist, convert the detector parameters to mrad units
  STEM%BFmrad = atan(stemnl%BFradius/stemnl%CLarray(iCL))*1000.0
  STEM%ADFimrad = atan(stemnl%ADFinnerradius/stemnl%CLarray(iCL))*1000.0
  STEM%ADFomrad = atan(stemnl%ADFouterradius/stemnl%CLarray(iCL))*1000.0
  if (stemnl%diffaprad.ne.0.0) diffappresent = .TRUE.

! then, for each point inside each diffraction disk, determine where it falls with respect to
! the BD and ADF detectors ... Also, look for disk overlaps as they might require amplitudes
! to be added in instead of intensities (for starters, we could just not allow that to happen...)

! rename some variables to short symbols
  a = STEM%ADFimrad
  b = STEM%ADFomrad
c = STEM%BFmrad
  th = stemnl%beamconvergence
  n = stemnl%numberofsvalues
  if (diffappresent) then
    dr = stemnl%diffaprad
    dx = stemnl%diffapcenter
  end if
  omega_min =  -th
  omega_max = th
  dom = 2.0*th/float(n-1)

  if (.not.diffappresent) then   ! there is no diffraction aperture, so compute the regular weight factors
    ! first, do the math for the g=0 disk  (we're dropping common factors of 2 for the weights) 
    i = ira
    do j=(n+1)/2,n
      omega = omega_min+float(j-1)*dom
      if (th.gt.c) then       ! the zero disk is larger than the BF detector, so it (potentially) gives two signals
        if (omega.le.c) STEM%BFweightsarray(i,j,iCL) = sqrt(c**2-omega**2)
        if (th.ge.a) then    ! there's overlap with the ADF detector
          if (omega.le.a) then  ! the first part needs to have a bit subtracted
            STEM%ADFweightsarray(i,j,iCL) = sqrt((th**2-omega**2)) - sqrt((a**2-omega**2))
          else   ! the second part does not
            STEM%ADFweightsarray(i,j,iCL) = sqrt((th**2-omega**2)) 
          end if
        end if
      else                         ! the zero disk is smaller than the BF detector, so only a BF signal
       STEM%BFweightsarray(i,j,iCL) = sqrt((th**2-omega**2))
      end if
    ! then apply symmetry for the other half of the g=0 disk
      if (j.ne.(n+1)/2) then
        jj = n+1 - j
        STEM%BFweightsarray(i,jj,iCL) = STEM%BFweightsarray(i,j,iCL)
        STEM%ADFweightsarray(i,jj,iCL) = STEM%ADFweightsarray(i,j,iCL)
      end if  
    end do  ! that completes the central disk weight factors

! the other disks are quite a bit more difficult to deal with ... there are a lot of possible cases to consider ...
    do i=ira+1,nn      ! loop over the positive reflections of the systematic row (the rest follows by symmetry)
    ! redefine a couple of parameters
      j = i - ira
      thb = CalcDiffAngle(cell,j*g(1),j*g(2),j*g(3))*1000.0  ! diffraction angle in mrad
      omega_min = thb - th
      omega_max = thb + th
    ! only used for debugging
     if (debug)  thetar(7+j) = thb
    ! first check if a part of this disk lies inside the BF detector
      if (omega_min.lt.c) then     ! yes, it does, so determine the BF weight factors for this disk
        if (omega_max.le.c) then  ! does it lie completely inside the BF detector?
          do j=1,n   ! yes it does, so compute all weight factors
            omega = omega_min + float(j-1)*dom
            STEM%BFweightsarray(i,j,iCL) = sqrt(th**2 - (thb-omega)**2)
            STEM%BFweightsarray(2*ira-i,n+1-j,iCL) = STEM%BFweightsarray(i,j,iCL)
          end do
        else  ! no, there's some potential overlap with the ADF detector 
          do j=1,n   ! once again, there are a few cases
            omega = omega_min + float(j-1)*dom    ! this is the position
            p = (thb**2-th**2+a**2)*0.5/thb             ! this is the location of the radical line for the ADF detector
            q = (thb**2-th**2+c**2)*0.5/thb             ! this is the location of the radical line for the BF detector
            if (omega.le.q) then   ! this point contributes to the BF detector
              STEM%BFweightsarray(i,j,iCL) = sqrt(th**2 - (thb-omega)**2)
            end if
            if ((omega.gt.q).and.(omega.le.c)) then   ! this point contributes to the BF detector
              STEM%BFweightsarray(i,j,iCL) = sqrt(c**2 - omega**2)
            end if
            if ((omega_max.ge.a).and.(omega.ge.p).and.(omega.le.a)) then ! this point contributes to the ADF detector (using radical line position)
              STEM%ADFweightsarray(i,j,iCL) = sqrt(th**2 -  (thb-omega)**2)  - sqrt(a**2 - omega**2)
            end if
             if ((omega_max.ge.a).and.(omega.gt.a)) then ! this point lies on the ADF detector 
              STEM%ADFweightsarray(i,j,iCL) = sqrt(th**2 -  (thb-omega)**2)
            end if
           STEM%BFweightsarray(2*ira-i,n+1-j,iCL) = STEM%BFweightsarray(i,j,iCL)       
           STEM%ADFweightsarray(2*ira-i,n+1-j,iCL) = STEM%ADFweightsarray(i,j,iCL)
          end do
        end if 
      else    ! no, it does not intersect the BF detector, so this disk can only contribute to the ADF weight factors
    ! once more there are several cases which we'll treat in increasing value of the position...
          do j=1,n 
            omega = omega_min + float(j-1)*dom    ! this is the position
            p = (thb**2-th**2+a**2)*0.5/thb             ! this is the location of the radical line for the inner ADF detector edge
            q = (thb**2-th**2+b**2)*0.5/thb             ! this is the location of the radical line for the outer ADF detector edge
            if ((omega.lt.a).and.(omega.ge.p))  then    ! inside the inner ADF edge, but close enough to contribute
              STEM%ADFweightsarray(i,j,iCL) = sqrt(th**2 -  (thb-omega)**2)  - sqrt(a**2 - omega**2)
            end if
             if ((omega.ge.a).and.(omega_max.le.b)) then ! this point lies on the ADF detector 
              STEM%ADFweightsarray(i,j,iCL) = sqrt(th**2 -  (thb-omega)**2)
            end if
            if ((omega_max.gt.b).and.(omega.le.q)) then   ! this point lies on the ADF detector
              STEM%ADFweightsarray(i,j,iCL) = sqrt(th**2 - (thb-omega)**2)
            end if
            if ((omega_max.gt.b).and.(omega.gt.q).and.(omega.le.b))  then   ! this point contributes to the ADF detector
              STEM%ADFweightsarray(i,j,iCL) = sqrt(b**2 - omega**2)
            end if
            STEM%ADFweightsarray(2*ira-i,n+1-j,iCL) = STEM%ADFweightsarray(i,j,iCL)
          end do
      end if

    end do
  end if ! end of regular weight factors without a diffraction aperture



  if (diffappresent) then   ! there is a diffraction aperture, so revisit the weight factors.
  ! once again, there are many different cases that need to be addressed...
    
  ! we do not allow for a diffraction aperture that overlaps the boundary between BF and ADF detectors,
  ! nor an aperture that lies entirely beyond the ADF detector
  ! first the BF test
    if ( ((dx-dr).gt.-c).and.((dx+dr).lt.c))  apinBF = .TRUE.

  ! then the ADF detector
    if ( (((dx-dr).gt.-b).and.((dx+dr).lt.-c)) .or.(((dx-dr).gt.a).and.((dx+dr).lt.b)) )  apinADF = .TRUE. 

  ! if the aperture is outside the ADF detector, or it overlaps the space between the detectors, then abort
    if ( .not.apinBF .and. .not.apinADF ) then
      call Message('Please fix input: Diffraction aperture outside BF detector disk or ADF ring !', frm = "(A)")
      stop
    end if

    if (apinBF) then
    ! figure out which diffraction disk(s) contribute to the BF detector
     do i=1,nn      ! loop over all reflections of the systematic row
    ! redefine a couple of parameters
      j = -(nn-1)/2-1+i
      if (j.ne.0) then 
        thb = (j/abs(j)) * CalcDiffAngle(cell,j*g(1),j*g(2),j*g(3))*1000.0  ! diffraction angle in mrad
      else
        thb = 0.0
      end if  
     ! only used for debugging
     if (debug)  thetar(7+i) = thb
      omega_min = thb - th
      omega_max = thb + th
    ! check whether or not there is any overlap between this disk and the diffraction aperture opening
      if ((omega_max.lt.(dx-dr)).or.(omega_min.gt.(dx+dr))) then  ! this disks does not fall inside the diffraction aperture 
        STEM%BFweightsarray(i,1:n,iCL) = 0.0
      else
    ! case 1: dx-dr < omega_min < omega_max < dx+dr
        if ((omega_max.lt.(dx+dr)).and.(omega_min.gt.(dx-dr))) then
          do k=1,n
            omega = omega_min+float(k-1)*dom
            kk = k
            if (j.lt.0) kk = n+1-k 
            STEM%BFweightsarray(i,kk,iCL) =  sqrt((th**2-(thb-omega)**2))
         end do 
        end if
    ! case 2: omega_min < dx-dr  < dx+dr < omega_max 
        if ((omega_max.gt.(dx+dr)).and.(omega_min.lt.(dx-dr))) then
          do k=1,n
            omega = omega_min+float(k-1)*dom
            kk = k
            if (j.lt.0) kk = n+1-k 
            if ((omega.gt.dx-dr).and.(omega.lt.dx+dr)) then
               STEM%BFweightsarray(i,kk,iCL) =  sqrt((dr**2-(omega-dx)**2))
            end if
          end do 
        end if
    ! case 3: omega_min < dx-dr   < omega_max < dx+dr
        if ((omega_min.lt.(dx-dr)).and.(omega_max.lt.(dx+dr))) then
           p = ((dx-thb)**2-dr**2+th**2)/2.0/(dx-thb)+thb
           do k=1,n
             omega = omega_min+float(k-1)*dom
             kk = k
    !         if (j.lt.0) kk = n+1-k 
            if ((omega.gt.dx-dr).and.(omega.le.p)) then
              STEM%BFweightsarray(i,kk,iCL) =  sqrt(dr**2-(omega-dx)**2)
            end if
            if ((omega.gt.p).and.(omega.le.omega_max)) then
              STEM%BFweightsarray(i,kk,iCL) =  sqrt((th**2-(thb-omega)**2))       
            end if
          end do 
        end if
     ! case 4:  dx-dr   < omega_min < dx+dr < omega_max
        if ((omega_min.gt.(dx-dr)).and.(omega_max.gt.(dx+dr))) then
           p = ((dx-thb)**2-th**2+dr**2)/2.0/(thb-dx) + dx
            do k=1,n
            omega = omega_min+float(k-1)*dom
            kk = k
    !        if (j.lt.0) kk = n+1-k 
            if ((omega.gt.p).and.(omega.le.dx+dr)) then
              STEM%BFweightsarray(i,kk,iCL) = sqrt((dr**2-(omega-dx)**2)) 
            end if
            if ((omega.gt.omega_min).and.(omega.le.p)) then
               STEM%BFweightsarray(i,kk,iCL) =  sqrt((th**2-(thb-omega)**2))
            end if
          end do 
        end if 
      end if
   end do  ! this completes the BF weight factors when a diffraction aperture is present and apinBF=.TRUE.
  end if 


  ! next determine the ADF weight factors in the presence of an aperture
  if (apinADF) then
  ! figure out which diffraction disk(s) contribute to the ADF detector
   do i=1,nn      ! loop over all reflections of the systematic row
  ! redefine a couple of parameters
      j = -(nn-1)/2-1+i
      if (j.ne.0) then 
        thb = (j/abs(j)) * CalcDiffAngle(cell,j*g(1),j*g(2),j*g(3))*1000.0  ! diffraction angle in mrad
      else
        thb = 0.0
     end if  
   ! only used for debugging
   if (debug)  thetar(7+i) = thb
    omega_min = thb - th
    omega_max = thb + th
  ! check whether or not there is any overlap between this disk and the diffraction aperture opening
    if ((omega_max.lt.(dx-dr)).or.(omega_min.gt.(dx+dr))) then  ! this disks does not fall inside the diffraction aperture 
      STEM%ADFweightsarray(i,1:n,iCL) = 0.0
    else
  ! case 1: dx-dr < omega_min < omega_max < dx+dr
      if ((omega_max.lt.(dx+dr)).and.(omega_min.gt.(dx-dr))) then
        do k=1,n
          omega = omega_min+float(k-1)*dom
          kk = k
          if (j.lt.0) kk = n+1-k 
          STEM%ADFweightsarray(i,kk,iCL) =  sqrt((th**2-(thb-omega)**2))
       end do 
      end if
  ! case 2: omega_min < dx-dr  < dx+dr < omega_max 
      if ((omega_max.gt.(dx+dr)).and.(omega_min.lt.(dx-dr))) then
        do k=1,n
          omega = omega_min+float(k-1)*dom
          kk = k
          if (j.lt.0) kk = n+1-k 
          if ((omega.gt.dx-dr).and.(omega.lt.dx+dr)) then
             STEM%ADFweightsarray(i,kk,iCL) =  sqrt((dr**2-(omega-dx)**2))
          end if
        end do 
      end if
  ! case 3: omega_min < dx-dr   < omega_max < dx+dr
      if ((omega_min.lt.(dx-dr)).and.(omega_max.lt.(dx+dr))) then
         p = ((dx-thb)**2-dr**2+th**2)/2.0/(dx-thb)+thb
         do k=1,n
           omega = omega_min+float(k-1)*dom
           kk = k
  !         if (j.lt.0) kk = n+1-k 
          if ((omega.gt.dx-dr).and.(omega.le.p)) then
            STEM%ADFweightsarray(i,kk,iCL) =  sqrt(dr**2-(omega-dx)**2)
          end if
          if ((omega.gt.p).and.(omega.le.omega_max)) then
            STEM%ADFweightsarray(i,kk,iCL) =  sqrt((th**2-(thb-omega)**2))       
          end if
        end do 
      end if
   ! case 4:  dx-dr   < omega_min < dx+dr < omega_max
      if ((omega_min.gt.(dx-dr)).and.(omega_max.gt.(dx+dr))) then
         p = ((dx-thb)**2-th**2+dr**2)/2.0/(thb-dx) + dx
          do k=1,n
          omega = omega_min+float(k-1)*dom
          kk = k
  !        if (j.lt.0) kk = n+1-k 
          if ((omega.gt.p).and.(omega.le.dx+dr)) then
            STEM%ADFweightsarray(i,kk,iCL) = sqrt((dr**2-(omega-dx)**2)) 
          end if
          if ((omega.gt.omega_min).and.(omega.le.p)) then
             STEM%ADFweightsarray(i,kk,iCL) =  sqrt((th**2-(thb-omega)**2))
          end if
        end do 
      end if 
    end if
   end do  ! this completes the ADF weight factors when a diffraction aperture is present and apinADF=.TRUE.
   
  end if

end if ! if aperture is present

end do outerCLloop   ! see line 146


! and the rest is also only used for debugging purposes
if (debug) then 
  allocate(outar(2*nn,stemnl%numberofsvalues,stemnl%numCL))
  outar(1:nn,1:stemnl%numberofsvalues,1:stemnl%numCL) = STEM%BFweightsarray(1:nn,1:stemnl%numberofsvalues,1:stemnl%numCL)
  outar(nn+1:2*nn,1:stemnl%numberofsvalues,1:stemnl%numCL) = STEM%ADFweightsarray(1:nn,1:stemnl%numberofsvalues,1:stemnl%numCL)
! to make sure that everything is correct, let's export this array so that we can display it in IDL
  open(unit=dataunit,file='STEMprofiles.data',status='unknown',form='unformatted')
  write(unit=dataunit) nn,stemnl%numberofsvalues,stemnl%numCL
  write(unit=dataunit) thetar
  write(unit=dataunit) outar
  close(unit=dataunit,status='keep')
end if

end subroutine init_STEM



!--------------------------------------------------------------------------
!
! SUBROUTINE: init_STEM_ZA
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize weight factors for zone-axis STEM case
! 
!> @note This will need to be reconsidered when we implement sectored detectors ... 
!
!> @param STEM STEM structure
!> @param stemnl STEM namelist
!> @param cell unit cell pointer
!> @param F foil normal
!> @param khead top of kvector list
!> @param reflist top of reflection list
!> @param nn number of reflections
! 
!> @date   04/29/11 MDG 1.0 original
!> @date   06/12/13 MDG 2.0 rewrite 
!> @date   06/09/14 MDG 3.0 added STEM and cell structures and khead+reflist linked lists
!> @date   06/10/14 MDG 3.1 added F, Dyn argument
!> @date   07/02/17 MDG 3.2 split STEM into STEM and stemnl
!--------------------------------------------------------------------------
recursive subroutine init_STEM_ZA(STEM,stemnl,cell,F,Dyn,khead,reflist,nn)
!DEC$ ATTRIBUTES DLLEXPORT :: init_STEM_ZA

use crystal
use diffraction
use kvectors
use gvectors
use NameListTypeDefs

IMPLICIT NONE

type(STEMtype),INTENT(INOUT)        :: STEM
!f2py intent(in,out) ::  STEM
type(STEMGeometryNameListType),INTENT(INOUT)    :: stemnl
!f2py intent(in,out) ::  stemnl
type(unitcell)                      :: cell
real(kind=dbl),INTENT(INOUT)        :: F(3)
!f2py intent(in,out) ::  F
type(DynType),INTENT(INOUT)         :: Dyn
!f2py intent(in,out) ::  Dyn
type(kvectorlist),pointer           :: khead
type(reflisttype),pointer           :: reflist
integer(kind=irg),INTENT(IN)        :: nn

integer(kind=irg)                   :: ik,ig, iCL
real(kind=sgl)                      :: ll(3), lpg(3), gg(3), glen, gplen, kpg
type(kvectorlist),pointer           :: ktmp
type(reflisttype),pointer           :: rltmpa

! this routine initializes the excitation error arrays and the weight-factor arrays for zone axis STEM signals
! the weightfactors are quite a bit different from the ones for the systematic row case;
! they are simpler in principle, since each point in the diffracted disk can only lie in one
! place, and hence only contributes to one detector.  However, not all points in a disk
! contribute to the same detector...  The length of the vector k_t+g, expressed in mrad,
! is what needs to be compared to the radii of the BF and ADF detectors.  For each incident 
! beam direction, we take the tangential component of the wave vector and loop over all
! reflections to compute the relevant angle; this then allows us to assign the weight factors
! which are now either 1 or 0 (so they can be stored as logicals).

! allocate the excitation error array areal(1..nn,1..STEM%numk)
  allocate(STEM%sgarray(nn,STEM%numk))
  
! transform the foil normal to real space and normalize
  call TransSpace(cell,sngl(F),Dyn%FN,'d','r')
  call NormVec(cell,Dyn%FN,'r')

! allocate the weight factor arrays, one entry for each beam direction, reflection, and camera length
  allocate(STEM%ZABFweightsarray(nn,STEM%numk,stemnl%numCL),STEM%ZAADFweightsarray(nn,STEM%numk,stemnl%numCL))
  STEM%ZABFweightsarray = .FALSE.
  STEM%ZAADFweightsarray = .FALSE.

! loop over the wave vector linked list
  ktmp => khead
  beamloopCL: do ik=1,STEM%numk
    ll = ktmp%kt        ! this is the tangential component of the wave vector
! and loop over all reflections
    rltmpa => reflist%next
    reflectionloopCL: do ig=1,nn
      gg = float(rltmpa%hkl)
      glen = CalcLength(cell,gg,'r')
      lpg = ll + gg                ! Laue + g
      gplen = CalcLength(cell,lpg,'r')
      kpg = 2000.0*asin(0.50*sngl(cell%mLambda)*gplen)    ! 2theta in mrad
      do iCL=1,stemnl%numCL
        STEM%BFmrad = atan(stemnl%BFradius/stemnl%CLarray(iCL))*1000.0
        STEM%ADFimrad = atan(stemnl%ADFinnerradius/stemnl%CLarray(iCL))*1000.0
        STEM%ADFomrad = atan(stemnl%ADFouterradius/stemnl%CLarray(iCL))*1000.0
        if (kpg.le.STEM%BFmrad) STEM%ZABFweightsarray(ig,ik,iCL) = .TRUE.
        if ((kpg.ge.STEM%ADFimrad).AND.(kpg.le.STEM%ADFomrad)) STEM%ZAADFweightsarray(ig,ik,iCL) = .TRUE.
      end do  ! loop over camera lengths
      STEM%sgarray(ig,ik) = Calcsg(cell,gg,sngl(ktmp%k),Dyn%FN)
 ! and we move to the next reflection in the list
      rltmpa => rltmpa%next
    end do reflectionloopCL  
    ktmp => ktmp%next
  end do beamloopCL

! that's it folks!
end subroutine init_STEM_ZA



!--------------------------------------------------------------------------
!
! SUBROUTINE: read_STEM_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read detector and other parameters for the STEM case
! 
!> @param STEM STEM structure
!> @param cell unit cell pointer
!> @param F foil normal
!> @param Dyn dynamical scattering structure
!> @param khead top of kvector list
!> @param reflist top of reflection list
!> @param geometry 'SR' for systematic row or 'ZA' for zone axis
!> @param nn number of reflections
!> @param g fundamental g-vector for systematic row
!> @param kt tangential wave vector component
!> @param numk number of distinct wave vectors (optional)
!> @param beamdiv beam divergence parameter (optional)
! 
!> @date   04/29/11 MDG 1.0 original
!> @date   06/12/13 MDG 2.0 rewrite 
!> @date   11/26/13 MDG 2.1 made geometry an input parameter instead of part of the STEMdata namelist
!> @date   06/10/14 MDG 3.0 added STEM, cell, foil, and Dyn arguments
!> @date   07/02/17 MDG 3.1 split STEM into STEM and namelist data structures; substantial simplification
!--------------------------------------------------------------------------
recursive subroutine read_STEM_data(STEM,stemnl,cell,F,Dyn,khead,reflist,geometry,nn,g,numk)
!DEC$ ATTRIBUTES DLLEXPORT :: read_STEM_data

use io
use files
use kvectors
use gvectors
use NameListTypeDefs

IMPLICIT NONE

type(STEMtype),INTENT(INOUT)                    :: STEM
!f2py intent(in,out) ::  STEM
type(STEMGeometryNameListType),INTENT(INOUT)    :: stemnl
!f2py intent(in,out) ::  stemnl
type(unitcell)                                  :: cell
real(kind=dbl),INTENT(INOUT)                    :: F(3)
!f2py intent(in,out) ::  F
type(DynType),INTENT(INOUT)                     :: Dyn
!f2py intent(in,out) ::  Dyn
type(kvectorlist),pointer                       :: khead
type(reflisttype),pointer                       :: reflist
character(2),INTENT(IN)                         :: geometry
integer(kind=irg),INTENT(IN)                    :: nn
integer(kind=irg),INTENT(IN)                    :: g(3)
integer(kind=irg),INTENT(IN),OPTIONAL           :: numk

if (PRESENT(numk)) then
  STEM%numk = numk
else
  STEM%numk = stemnl%numberofsvalues
end if

! and initialize all other STEM related arrays 
if (geometry.eq.'SR') then
  call init_STEM(STEM,stemnl,cell,nn,g)
else
  call init_STEM_ZA(STEM,stemnl,cell,F,Dyn,khead,reflist,nn)
end if

end subroutine read_STEM_data

end module STEMmodule

