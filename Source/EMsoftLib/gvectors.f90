! ###################################################################
! Copyright (c) 2014-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:gvectors.f90 
!--------------------------------------------------------------------------
!
! MODULE: gvectors
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief variables and types needed to determine lists of reciprocal lattice vectors.  
!
!> @details This was part of the dynamical module, but was moved into a separate
!> module.  The new module includes a routine to delete the linked list, and also routines 
!> to allocate linked lists, which are used by almost all dynamical scattering codes.
!>
!> Due to some complicated module interdependencies the CalcDynMat routine is in
!> this module rather than in diffraction, where it would logically belong.  We may need
!> to figure out how to change that. 
! 
!> @date 04/29/13 MDG 1.0 original 
!> #date 01/10/14 MDG 2.0 new version, now use-d in crystalvars
!> @date 06/09/14 MDG 3.0 removed all global variables and replaced them by arguments
!--------------------------------------------------------------------------
module gvectors

use local
use typedefs

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: MakeRefList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief allocate and initialize the linked reflection list
!
!> @param listroot top of linked list
!> @param rltail auxiliary pointer
!> @param nref number of reflections in list 
!
!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  01/10/14 MDG 4.0 account for new version of cell type
!> @date  06/09/14 MDG 4.1 added cell and rltail as arguments
!> @date  06/17/14 MDG 4.2 modification for separate reflist pointers; removed cell pointer
!--------------------------------------------------------------------------
recursive subroutine MakeRefList(listroot, rltail, nref)
!DEC$ ATTRIBUTES DLLEXPORT :: MakeRefList

use error

IMPLICIT NONE

type(reflisttype),pointer       :: listroot 
type(reflisttype),pointer       :: rltail
integer(kind=irg),INTENT(INOUT) :: nref
!f2py intent(in,out) ::  nref

integer(kind=irg)  :: istat

! create it if it does not already exist
if (.not.associated(listroot)) then
  nref = 0
  allocate(listroot,stat=istat)
  if (istat.ne.0) call FatalError('MakeRefList:',' unable to allocate pointer')
  rltail => listroot               ! tail points to new value
  nullify(rltail%next)             ! nullify next in new value
end if

end subroutine MakeRefList



!--------------------------------------------------------------------------
!
! SUBROUTINE: AddReflection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a reflection to the linked reflection list
!
!> @param rltail reflisttype variable
!> @param listroot reflisttype variable
!> @param cell unit cell pointer
!> @param nref number of reflections
!> @param hkl Miller indices
!
!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  01/10/14 MDG 4.0 account for new version of cell type
!> @date  06/09/14 MDG 4.1 added rltail and cell as arguments
!> @date  06/17/14 MDG 4.2 modification for separate reflist pointers
!> @date  09/08/15 MDG 4.3 added qg entry
!--------------------------------------------------------------------------
recursive subroutine AddReflection(rltail,listroot,cell,nref,hkl)
!DEC$ ATTRIBUTES DLLEXPORT :: AddReflection

use error
use constants
use diffraction

IMPLICIT NONE

type(reflisttype),pointer       :: rltail
type(reflisttype),pointer       :: listroot
type(unitcell)                  :: cell
integer(kind=irg),INTENT(INOUT) :: nref
!f2py intent(in,out) ::  nref
integer(kind=irg),INTENT(IN)    :: hkl(3)               !< Miller indices of reflection to be added to list

integer(kind=irg)               :: istat

! create linked list if it does not already exist
 if (.not.associated(rltail)) then
   nullify(rltail)
 end if
 if (.not.associated(listroot)) then
   call MakeRefList(listroot,rltail,nref)
 end if

! create a new entry
 allocate(rltail%next,stat=istat)               ! allocate new value
 if (istat.ne.0) call FatalError('AddReflection',' unable to add new reflection')

 rltail => rltail%next                          ! tail points to new value
 nullify(rltail%next)                           ! nullify next in new value

 nref = nref + 1                                ! update reflection counter
 rltail%num = nref                              ! store reflection number
 rltail%hkl = hkl                               ! store Miller indices
 rltail%Ucg = cell%LUT( hkl(1), hkl(2), hkl(3) ) ! store Ucg  in the list
 rltail%qg = cell%LUTqg( hkl(1), hkl(2), hkl(3) ) ! store pi/qg  in the list
 rltail%famnum = 0                              ! init this value for Prune_ReflectionList
! rltail%Ucgmod = cabs(rlp%Ucg)                 ! added on 2/29/2012 for Bethe potential computations
! rltail%sangle = 1000.0*dble(CalcDiffAngle(hkl(1),hkl(2),hkl(3)))    ! added 4/18/2012 for EIC project HAADF/BF tomography simulations
! rltail%thetag = rlp%Vphase                   ! added 12/14/2013 for EMECCI program
 nullify(rltail%nextw)
 nullify(rltail%nexts)
 
end subroutine AddReflection

!--------------------------------------------------------------------------
!
! SUBROUTINE: Printrlp
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief output the contents of the rlp structure
!
!> @param rlp gnode structure
!> @param first logical switch to provide long form output (optional)
!> @param stdout optional output unit identifier
!
!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  06/09/14 MDG 4.0 added rlp and stdout arguments
!> @date  03/29/18 MDG 4.1 removed stdout argument
!--------------------------------------------------------------------------
recursive subroutine Printrlp(rlp,first)
!DEC$ ATTRIBUTES DLLEXPORT :: Printrlp
 
use io
use constants

IMPLICIT NONE

type(gnode),INTENT(IN)                 :: rlp
logical,optional,intent(INOUT)  :: first                !< switch for long/short output

integer(kind=irg)                       :: oi_int(3)
real(kind=sgl)                          :: oi_real(7)
complex(kind=sgl)                       :: oi_cmplx(1)


if (present(first)) then
 if (first) then
  call Message('     Scattering factors : ', frm = "(/A)",advance="no")
    if (rlp%method.eq.'WK') then 
   if (rlp%absorption.eqv..TRUE.) then 
    call Message(' Weickenmeier-Kohl (with absorption)', frm = "(A/)")
   else
    call Message(' Weickenmeier-Kohl', frm = "(A/)")
   end if
  else
    call Message(' Doyle-Turner/Smith-Burge', frm = "(A/)")
  end if

  if (rlp%absorption.eqv..TRUE.) then
    call Message('   h  k  l    |g|    Ucg_r  Ucg_i   |Ug|    phase   |Ugp|   phase   xi_g   xi_gp    ratio  Re-1/q_g-Im', &
        frm = "(A)")
  else
    call Message('   h  k  l    |g|    Ucg_r  |Ug|    phase    xi_g   1/q_g', frm = "(A)")
  end if
  first = .FALSE.
 end if
end if

if (rlp%absorption.eqv..TRUE.) then
 oi_int(1:3) = rlp%hkl(1:3)
 call WriteValue('',oi_int, 3, "(1x,3I3,1x)",advance="no")
 oi_real(1) = rlp%g
 call WriteValue('',oi_real, 1, "(F9.4)",advance="no")
 oi_cmplx(1) = rlp%Ucg
 call WriteValue('',oi_cmplx, 1, "(2F7.3,1x)",advance="no")
 oi_real(1:7)  = (/ rlp%Umod,rlp%Vphase*180.0/sngl(cPi),rlp%Upmod,rlp%Vpphase*180.0/sngl(cPi),rlp%xg,rlp%xgp,rlp%ar /)
 call WriteValue('',oi_real, 7, "(4F8.3,3F8.1)",advance="no")
 oi_cmplx(1) = rlp%qg
 call WriteValue('',oi_cmplx, 1, "(2F8.3)")
else
 oi_int(1:3) = rlp%hkl(1:3)
 call WriteValue('',oi_int, 3, "(1x,3I3,1x)",advance="no")
 oi_real(1) = rlp%g
 call WriteValue('',oi_real, 1, "(F9.4)",advance="no")
 oi_real(1) = real(rlp%Ucg)
 call WriteValue('',oi_real, 1, "(F7.3,1x)",advance="no")
 oi_real(1:3)  = (/ rlp%Umod,rlp%Vphase*180.0/sngl(cPi),rlp%xg /)
 call WriteValue('',oi_real, 3, "(2F8.3,F8.1)",advance="no")
 oi_cmplx(1) = rlp%qg
 call WriteValue('',oi_cmplx, 1, "(2F8.3)")
end if

end subroutine Printrlp


!--------------------------------------------------------------------------
!
! SUBROUTINE: Laue_Init_Reflist
!
!> @author Marc De Graef, Carnegie Melon University
!
!> @brief compute the list of all possible reciprocal lattice points for Laue XRD
!
!> @param verbose print output when .TRUE.
!
!> @date 03/14/19 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine Laue_Init_Reflist(cell, lmnl, reflist, gcnt, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Laue_Init_Reflist

use local
use io
use crystal
use error
use symmetry
use diffraction 
use NameListTypedefs

IMPLICIT NONE

type(unitcell)                    :: cell
type(LaueMasterNameListType),INTENT(INOUT) :: lmnl
!f2py intent(in,out) ::  lmnl
type(Laue_g_list),pointer         :: reflist                    ! linked list for allowed g-vector search 
integer(kind=irg),INTENT(OUT)     :: gcnt
logical,OPTIONAL,INTENT(IN)       :: verbose                    ! print output or not ?

type(Laue_g_list),pointer         :: gtmp, gtail                ! linked list for allowed g-vector search 
type(gnode)                       :: rlp

real(kind=sgl)                    :: gmax                       !< diameter of limiting sphere
real(kind=sgl)                    :: ghkl                       !< length of a reciprocal lattice vector
integer(kind=irg)                 :: imh, imk, iml              !< maximum index along a*, b*, and c*
real(kind=sgl)                    :: g(3), tt                   !< g-vector and 2theta
integer(kind=irg)                 :: io_int(3)                  !< io variable
real(kind=sgl)                    :: io_real(1)                 !< io variable
integer(kind=irg)                 :: istat, h, k, l, icnt       !< status variables and such
!real(kind=sgl),parameter          :: tdtr = 114.5915590262      !< 2 * 180.0 / pi
real(kind=sgl)                    :: threshold, th, sfs         !< threshold for discarding allowed reflections, |F|^2

! first get the range of Miller indices based on the lattice parameters and the xray wave length
 gmax = 2.0 / lmnl%lambdamin      ! radius of the limiting sphere for smallest wave length  [nm^-1]
 imh = 1
 do   ! a* direction
   imh = imh + 1
   ghkl = CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (ghkl.gt.gmax) EXIT
 end do
 imk = 1
 do  ! b* direction
   imk = imk + 1
   ghkl = CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (ghkl.gt.gmax) EXIT
 end do
 iml = 1
 do  ! c* direction
   iml = iml + 1
   ghkl = CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (ghkl.gt.gmax) EXIT
 end do

! output range
if (present(verbose)) then 
  if (verbose) then
    io_int = (/ imh, imk ,iml /)
    call WriteValue('Range of reflections along a*, b* and c* = ', io_int, 3)
  end if
end if 
 
! next we make a list of all rlp's that satisfy the following conditions
!  - rlp must be inside the limiting sphere;
!  - rlp must not be a systematic extinction;
!  - rlp must not be a symmetry-induced extinction (since everything is kinematical)
! Since we don't know a-priori how many rlps will satisfy all
! three conditions, we'll first create a linked list and then copy
! that list into an allocatable array reflist
 if (.not.associated(reflist)) then ! allocate the head and tail of the linked list
   allocate(reflist,stat=istat)         ! allocate new value
   if (istat.ne.0) call FatalError('Laue_Init_Reflist', 'unable to allocate reflist pointer')
   gtail => reflist                     ! tail points to new value
   nullify(gtail%next)                  ! nullify next in new value
 end if

! initialize the computation mode X-Ray
rlp%method = 'XR'

! compute the intensity threshold parameter as a fraction of |F(000)|^2 
 call CalcUcg(cell, rlp, (/0, 0, 0/) )
 threshold = cabs(rlp%Ucg)**2
if (present(verbose)) then
  if (verbose) then
    io_real(1) = threshold 
    call WriteValue(' Intensity Threshold value : ', io_real, 1)
  end if
end if

! now loop over all g-vectors inside the imh, imk, iml range
gcnt = 0
icnt = 0
th = lmnl%intfactor * threshold
do h=-imh,imh
 do k=-imk,imk
  do l=-iml,iml
   icnt = icnt + 1
    g =float( (/ h, k, l /) )
! first of all, is this reflection inside the limiting sphere? (CalcLength)
    ghkl = CalcLength(cell,g,'r')
    if ((ghkl.le.gmax).and.(ghkl.gt.0.0)) then 
! then see if the reflection is allowed by systematic extinction (IsGAllowed)
       if ( IsGAllowed(cell, (/ h,k,l /) ) ) then    ! this is not a systematic extinction
! does this reflection have a non-zero structure factor larger than the threshold?
           call CalcUcg(cell, rlp, (/ h, k, l /) )
           sfs = cabs(rlp % Ucg)**2 
           if (sfs.ge.th) then   ! count this reflection 
             gcnt = gcnt + 1
! fill in the values
             gtail % hkl = (/ h, k, l /)
             call TransSpace(cell, dble(gtail % hkl), gtail % xyz, 'r', 'c')
!             call NormVec(cell, gtail%xyz, 'c')    ! removed by MDG, 07/30/19 for EMLaue program
             gtail % tt = CalcDiffAngle(cell,h,k,l)
             gtail % polar = (1.D0+ cos(2.D0*gtail%tt)**2)*0.5D0
             gtail % sfs = sfs / threshold
! and add it to the linked list
             allocate(gtail%next,stat=istat)    ! allocate new value
             if (istat.ne.0) call FatalError('Laue_Init_Reflist', 'unable to allocate new entry in ghead linked list')
             gtail => gtail%next              ! gtail points to new value
             nullify(gtail%next)              ! nullify next in new value
           end if
       end if
    end if
  end do
 end do
end do

if (present(verbose)) then
  if (verbose) then
    io_int(1) = gcnt
    call WriteValue(' Total number of reflections = ', io_int, 1)
  end if
end if 

end subroutine Laue_Init_Reflist



!--------------------------------------------------------------------------
!
! SUBROUTINE: Laue_Init_Unit_Reflist
!
!> @author Marc De Graef, Carnegie Melon University
!
!> @brief compute the list of all possible reciprocal lattice points for Laue XRD;
!> in this particular version, we return only unit length g-vectors, and then only
!> one Friedel pair for each lattice row.  We also keep the structure factors for
!> entire row, since we will need to evaluate which multiple of g actually causes the
!> reflection; we will also need the d-spacings for each of them.  The linked list 
!> generated by this routine has a type that is different from the regular Laue linked list
!
!> @param verbose print output when .TRUE.
!
!> @date 01/29/20 MDG 1.0 original
!--------------------------------------------------------------------------
subroutine Laue_Init_Unit_Reflist(cell, lmnl, reflist, gcnt, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Laue_Init_Unit_Reflist

use local
use io
use crystal
use error
use symmetry
use diffraction 
use postscript
use NameListTypedefs

IMPLICIT NONE

type(unitcell)                    :: cell
type(LaueMasterNameListType),INTENT(INOUT) :: lmnl
!f2py intent(in,out) ::  lmnl
type(Laue_grow_list),pointer      :: reflist                    ! linked list for allowed g-vector search 
integer(kind=irg),INTENT(OUT)     :: gcnt
logical,OPTIONAL,INTENT(IN)       :: verbose                    ! print output or not ?

type(Laue_grow_list),pointer      :: gtmp, gtail                ! linked list for allowed g-vector search 
type(gnode)                       :: rlp

logical,allocatable               :: z(:,:,:)

real(kind=sgl)                    :: gmax                       !< diameter of limiting sphere
real(kind=sgl)                    :: ghkl                       !< length of a reciprocal lattice vector
integer(kind=irg)                 :: imh, imk, iml              !< maximum index along a*, b*, and c*
real(kind=sgl)                    :: tt                         !< 2theta
integer(kind=irg)                 :: io_int(3)                  !< io variable
real(kind=sgl)                    :: io_real(1)                 !< io variable
integer(kind=irg)                 :: i, istat, h, k, l, icnt, g(3), gr(3), rf, lcnt       !< status variables and such
!real(kind=sgl),parameter          :: tdtr = 114.5915590262      !< 2 * 180.0 / pi
real(kind=sgl)                    :: threshold, th, sfs         !< threshold for discarding allowed reflections, |F|^2

! first get the range of Miller indices based on the lattice parameters and the xray wave length
 gmax = 2.0 / lmnl%lambdamin      ! radius of the limiting sphere for smallest wave length  [nm^-1]
 imh = 1
 do   ! a* direction
   imh = imh + 1
   ghkl = CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (ghkl.gt.gmax) EXIT
 end do
 imk = 1
 do  ! b* direction
   imk = imk + 1
   ghkl = CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (ghkl.gt.gmax) EXIT
 end do
 iml = 1
 do  ! c* direction
   iml = iml + 1
   ghkl = CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (ghkl.gt.gmax) EXIT
 end do

! output range
if (present(verbose)) then 
  if (verbose) then
    io_int = (/ imh, imk ,iml /)
    call WriteValue('Range of reflections along a*, b* and c* = ', io_int, 3)
  end if
end if 
 
! logical array to keep track of reflections that we have already dealt with (.TRUE.)
allocate(z(-imh:imh, -imk:imk, -iml:iml))
z = .FALSE.   ! all are set to .FALSE. to start the triple loop through the reciprocal lattice 

! next we make a list of all rlp's that satisfy the following conditions
!  - rlp must be inside the limiting sphere;
!  - rlp must not be a systematic extinction;
!  - rlp must not be a symmetry-induced extinction (since everything is kinematical)
! Since we don't know a-priori how many rlps will satisfy all
! three conditions, we'll first create a linked list and then copy
! that list into an allocatable array reflist
 if (.not.associated(reflist)) then ! allocate the head and tail of the linked list
   allocate(reflist,stat=istat)         ! allocate new value
   if (istat.ne.0) call FatalError('Laue_Init_Reflist', 'unable to allocate reflist pointer')
   gtail => reflist                     ! tail points to new value
   nullify(gtail%next)                  ! nullify next in new value
 end if

! initialize the computation mode X-Ray
rlp%method = 'XR'

! compute the intensity threshold parameter as a fraction of |F(000)|^2 
 call CalcUcg(cell, rlp, (/0, 0, 0/) )
 threshold = cabs(rlp%Ucg)**2
if (present(verbose)) then
  if (verbose) then
    io_real(1) = threshold 
    call WriteValue(' Intensity Threshold value : ', io_real, 1)
  end if
end if

! now loop over all g-vectors inside the imh, imk, iml range
! we keep only the unit normal, and we also keep the structure factors 
! and d-spacings for a series of positive multiples of g (i.e., we keep Friedel
! pairs apart).  This will then allow for an efficient scan through the Ewald
! volume in the pattern generation module. 

! to ensure that we get the correct ranges for the sfs and dspacing arrays, we 
! first reduce each hkl to the smallest common denominator, which sets the Nentries
! parameter; then we compute all the sfs and dspacing values, as well as the unit 
! g-vector (in the Cartesian crystal reference frame) and we set all the points along
! the g row to .TRUE. in the z logical array
gcnt = 0
icnt = 0
lcnt = 0
th = lmnl%intfactor * threshold

! loop over the entire reciprocal space sublattice
do h=-imh,imh
 do k=-imk,imk
  do l=-iml,iml
! skip the origin !  (transmitted beam will be handled separately)
    if (maxval(abs( (/ h, k, l /) ) ).eq.0) CYCLE
! have we already dealt with this reflection?
    if (z(h,k,l).eqv..TRUE.) CYCLE 
! is this reflection forbidden by lattice centering ?
    if ( .not.IsGAllowed(cell, (/ h, k, l /) ) ) then 
      z(h,k,l) = .TRUE.
      CYCLE
    end if 
! we haven't covered this one yet so let's reduce the Miller indices to the lowest common denominator
    g = (/ h, k, l /)
    gr = g
    call IndexReduce( gr )
! the reduction factor is ...
    do i=1,3
      if (gr(i).ne.0) then 
        rf = g(i)/gr(i)
        EXIT 
      end if 
    end do 
! have we done this one yet ?  If not, then we fill in the linked list entry, and 
! allocate the next one 
    if (z(gr(1),gr(2),gr(3)).eqv..FALSE.) then
! set the entire systematic row to .TRUE. to avoid visiting them again 
      do rf=1,100
        ghkl = CalcLength(cell,float(rf*gr),'r')
        if (ghkl.gt.gmax) EXIT 
      end do
      do i=1,rf 
        if ( (abs(i*gr(1)).le.imh) .and. (abs(i*gr(2)).le.imk) .and. (abs(i*gr(3)).le.iml) ) then 
          z(i*gr(1), i*gr(2), i*gr(3)) = .TRUE.
        end if 
      end do
! the reduction factor is also the Nentries parameter for the linked list, so we create a 
! new entry in the list and generate the proper arrays sfs and dspacing
      gtail % hkl = gr 
! convert the shortest g-vector to a unit cartesian vector
      call TransSpace(cell, dble(gr), gtail % xyz, 'r', 'c')
      call NormVec(cell, gtail%xyz, 'c')
! then deal with the intensities and d-spacings
      gtail % Nentries = rf 
      allocate( gtail%sfs(rf), gtail%dspacing(rf) )
      gtail % sfs = 0.0
      gtail % dspacing = 0.0
      do i=1,rf
! is this reflection inside the limiting sphere? (CalcLength)
        ghkl = CalcLength(cell,float(i*gr),'r')
        if ((ghkl.le.gmax).and.(ghkl.gt.0.0)) then 
          if ( IsGAllowed(cell, i*gr ) ) then ! allowed reflection, so compute the entries
            call CalcUcg(cell, rlp, i*gr )
            gtail % sfs(i) = cabs(rlp % Ucg)**2 / threshold 
            if (gtail%sfs(i).gt.lmnl%intfactor) then 
              gtail % dspacing(i) = 1.0/CalcLength(cell, float(i*gr), 'r')
            else 
              gtail % dspacing(i) = 0.0
              gtail % sfs(i) = 0.0
            end if 
          end if
        end if
      end do
      if (sum(gtail%sfs).eq.0.0) then 
        deallocate(gtail%sfs, gtail%dspacing)
      else
! extend the linked list
        allocate(gtail%next,stat=istat)    ! allocate new value
        if (istat.ne.0) call FatalError('Laue_Init_Unit_Reflist', 'unable to allocate new entry in linked list')
        gtail => gtail%next              ! gtail points to new value
        nullify(gtail%next)              ! nullify next in new value
        gcnt = gcnt + 1
      end if
    end if
    icnt = icnt + 1
  end do
 end do
end do

if (present(verbose)) then
  if (verbose) then
    io_int(1:2) = (/ gcnt, icnt /)
    call WriteValue(' Total number of reflections accepted/tested = ', io_int, 2, frm="(I8,'/',I8)")
  end if
end if 

end subroutine Laue_Init_Unit_Reflist

!--------------------------------------------------------------------------
!
! SUBROUTINE: Apply_BethePotentials
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief tag weak and strong reflections in cell%reflist
!
!> @param cell unit cell pointer
!> @param BetheParameter Bethe Potential parameter structure
!> @param listroot top of reflection linked list
!> @param listrootw top of weak reflection linked list
!> @param nref total number of reflections
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!
!> @details This routine steps through the listroot linked list and 
!> determines for each reflection whether it is strong or weak or should be
!> ignored.  Strong and weak reflections are then linked in a new list via
!> the nexts and nextw pointers, along with the nns and nnw counters.
!> This routine makes use of the BetheParameter variables.
!
!> @date  01/14/14 MDG 1.0 original version
!> @date  06/09/14 MDG 2.0 added cell and BetheParameter arguments
!> @date  06/17/14 MDG 2.1 added listroot, listrootw, nns, nnw arguments
!--------------------------------------------------------------------------
recursive subroutine Apply_BethePotentials(cell, listroot, listrootw, BetheParameter, nref, nns, nnw)
!DEC$ ATTRIBUTES DLLEXPORT :: Apply_BethePotentials

use io
use diffraction

IMPLICIT NONE

type(unitcell)                                 :: cell
type(reflisttype),pointer                      :: listroot
type(reflisttype),pointer                      :: listrootw
type(BetheParameterType),INTENT(IN)            :: BetheParameter
integer(kind=irg),INTENT(IN)                   :: nref
integer(kind=irg),INTENT(OUT)                  :: nns
integer(kind=irg),INTENT(OUT)                  :: nnw

integer(kind=irg),allocatable                  :: glist(:,:)
real(kind=dbl),allocatable                     :: rh(:)
type(reflisttype),pointer                      :: rl, lastw, lasts
integer(kind=irg)                              :: icnt, istat, gmh(3), ir, ih
real(kind=dbl)                                 :: sgp, la, m


nullify(lasts)
nullify(lastw)
nullify(rl)

! first we extract the list of g-vectors from reflist, so that we can compute 
! all the g-h difference vectors
allocate(glist(3,nref),rh(nref),stat=istat)
rl => listroot%next
icnt = 0
do
  if (.not.associated(rl)) EXIT
  icnt = icnt+1
  glist(1:3,icnt) = rl%hkl(1:3)
  rl => rl%next
end do

! initialize the strong and weak reflection counters
nns = 1
nnw = 0

! the first reflection is always strong
rl => listroot%next
rl%strong = .TRUE.
rl%weak = .FALSE.
lasts => rl
nullify(lasts%nextw)

la = 1.D0/cell%mLambda

! next we need to iterate through all reflections in glist and 
! determine which category the reflection belongs to: strong, weak, ignore
irloop: do ir = 2,icnt
  rl => rl%next
  rh = 0.D0
  sgp = la * abs(rl%sg)
  do ih = 1,icnt
   gmh(1:3) = glist(1:3,ir) - glist(1:3,ih)
   if (cell%dbdiff(gmh(1),gmh(2),gmh(3))) then  ! it is a double diffraction reflection with |U|=0
! to be written
     rh(ih) = 10000.D0 
   else 
     rh(ih) = sgp/abs( cell%LUT(gmh(1), gmh(2), gmh(3)) )
   end if
  end do

! which category does reflection ir belong to ?
  m = minval(rh)

! m > c2 => ignore this reflection
  if (m.gt.BetheParameter%c2) then
    rl%weak = .FALSE.
    rl%strong = .FALSE.
    CYCLE irloop
  end if

! c1 < m < c2 => weak reflection
  if ((BetheParameter%c1.lt.m).and.(m.le.BetheParameter%c2)) then
    if (nnw.eq.0) then
      listrootw => rl
      lastw => rl
    else
      lastw%nextw => rl
      lastw => rl
      nullify(lastw%nexts)
    end if
    rl%weak = .TRUE.
    rl%strong = .FALSE.
    nnw = nnw + 1
    CYCLE irloop
  end if

! m < c1 => strong
  if (m.le.BetheParameter%c1) then
    lasts%nexts => rl
    nullify(lasts%nextw)
    lasts => rl
    rl%weak = .FALSE.
    rl%strong = .TRUE.
    nns = nns + 1
  end if  
end do irloop

deallocate(glist, rh)

end subroutine Apply_BethePotentials


!--------------------------------------------------------------------------
!
! SUBROUTINE: Prune_ReflectionList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief select from the reflection list those g-vectors that will be counted in an LACBED computation
!
!> @details This routine basicaly repeats a section from the Compute_DynMat routine
!> without actually computing the dynamical matrix; it simply keeps track of all the
!> beams that are at one point or another regarded as strong or weak beams.
!> We'll use the famnum field in the rlp linked list to flag the strong reflections.
!> Linked list entries that are not used are instantly removed from the linked list.
!
!> @param cell unit cell pointer
!> @param khead start of reflisttype linked list
!> @param reflist reflection linked list
!> @param Dyn dynamical scattering structure
!> @param BetheParameter Bethe parameter structure 
!> @param numk number of wave vectors to consider
!> @param nbeams total number of unique beams
!
!> @date  09/20/13 MDG 1.0 original
!> @date  10/05/13 MDG 1.1 removal of unused reflections from linked list
!> @date  10/05/13 MDG 1.2 changed the order of nested loops to speed things up a bit
!> @date  10/07/13 MDG 1.3 added section to reset the famhkl entries after pruning
!> @date  01/10/14 MDG 4.0 account for new version of cell type
!> @date  06/09/14 MDG 4.1 added cell, reflist, BetheParameter and khead arguments
!> @date  12/01/15 MDG 4.2 modified routine to work with new BetheParameters criteria and mods to ECCI and STEMDCI programs
!--------------------------------------------------------------------------
recursive subroutine Prune_ReflectionList(cell,khead,reflist,Dyn,BetheParameter,numk,nbeams)
!DEC$ ATTRIBUTES DLLEXPORT :: Prune_ReflectionList

use io
use crystal
use kvectors
use diffraction

IMPLICIT NONE

type(unitcell)                          :: cell
type(kvectorlist),pointer               :: khead
type(reflisttype),pointer               :: reflist
type(DynType),INTENT(INOUT)             :: Dyn
!f2py intent(in,out) ::  Dyn
type(BetheParameterType),INTENT(INOUT)  :: BetheParameter
!f2py intent(in,out) ::  BetheParameter
integer(kind=irg),INTENT(IN)            :: numk
integer(kind=irg),INTENT(OUT)           :: nbeams

integer(kind=irg)                       :: ik, ig, istrong, curfam(3), newfam(3)
real(kind=sgl)                          :: sgp, lUg, cut1, cut2
!integer(kind=irg),allocatable          :: strongreflections(:,:)
type(kvectorlist),pointer               :: ktmp
type(reflisttype),pointer               :: rltmpa, rltmpb

! reset the value of DynNbeams in case it was modified in a previous call 
cell%DynNbeams = cell%DynNbeamsLinked

nbeams = 0

! reset the reflection linked list
  rltmpa => cell%reflist%next

! pick the first reflection since that is the transmitted beam (only on the first time)
  rltmpa%famnum = 1    
  nbeams = nbeams + 1

! loop over all reflections in the linked list    
!!!! this will all need to be changed with the new Bethe potential criteria ...  
  rltmpa => rltmpa%next
  reflectionloop: do ig=2,cell%DynNbeamsLinked
    lUg = abs(rltmpa%Ucg) * cell%mLambda
    cut1 = BetheParameter%cutoff * lUg
    cut2 = BetheParameter%weakcutoff * lUg

! loop over all the incident beam directions
    ktmp => khead
! loop over all beam orientations, selecting them from the linked list
    kvectorloop: do ik = 1,numk
! We compare |sg| with two multiples of lambda |Ug|
!
!  |sg|>cutoff lambda |Ug|   ->  don't count reflection
!  cutoff lambda |Ug| > |sg| > weakcutoff lambda |Ug|  -> weak reflection
!  weakcutoff lambda |Ug| > |sg|  -> strong reflection
!
!       sgp = abs(CalcsgHOLZ(float(rltmpa%hkl),sngl(ktmp%kt),sngl(cell%mLambda)))
!       write (*,*) rltmpa%hkl,CalcsgHOLZ(float(rltmpa%hkl),sngl(ktmp%kt), &
!                       sngl(cell%mLambda)),Calcsg(float(rltmpa%hkl),sngl(ktmp%k),DynFN)
        sgp = abs(Calcsg(cell,float(rltmpa%hkl),sngl(ktmp%k),Dyn%FN)) 
! we have to deal separately with double diffraction reflections, since
! they have a zero potential coefficient !        
        if ( cell%dbdiff(rltmpa%hkl(1),rltmpa%hkl(2),rltmpa%hkl(3)) ) then  ! it is a double diffraction reflection
          if (sgp.le.BetheParameter%sgcutoff) then         
            nbeams = nbeams + 1
            rltmpa%famnum = 1    
            EXIT kvectorloop    ! this beam did contribute, so we no longer need to consider it
          end if
        else   ! it is not a double diffraction reflection
          if (sgp.le.cut1) then  ! count this beam, whether it is weak or strong
            nbeams = nbeams + 1
            rltmpa%famnum = 1    
            EXIT kvectorloop    ! this beam did contribute, so we no longer need to consider it
          end if
        end if
 
! go to the next incident beam direction
       if (ik.ne.numk) ktmp => ktmp%next
     end do kvectorloop  ! ik loop

! go to the next beam in the list
   rltmpa => rltmpa%next
  end do reflectionloop

  call Message(' Renumbering reflections', frm = "(A)")

! change the following with the new next2 pointer in the reflist type !!!
  
! ok, now that we have the list, we'll go through it again to set sequential numbers instead of 1's
! at the same time, we'll deallocate those entries that are no longer needed.
  rltmpa => reflist%next
  rltmpb => rltmpa
  rltmpa => rltmpa%next ! we keep the first entry, always.
  istrong = 1
  reflectionloop2: do ig=2,cell%DynNbeamsLinked
    if (rltmpa%famnum.eq.1) then
        istrong = istrong + 1
        rltmpa%famnum = istrong
        rltmpa => rltmpa%next
        rltmpb => rltmpb%next
    else   ! remove this entry from the linked list
        rltmpb%next => rltmpa%next
        deallocate(rltmpa)
        rltmpa => rltmpb%next
    endif
! go to the next beam in the list
  end do reflectionloop2

! reset the number of beams to the newly obtained number
  cell%DynNbeamsLinked = nbeams
  cell%DynNbeams = nbeams

! go through the entire list once again to correct the famhkl
! entries, which may be incorrect now; famhkl is supposed to be one of the 
! reflections on the current list, but that might not be the case since
! famhkl was first initialized when there were additional reflections on
! the list... so we set famhkl to be the same as the first hkl in each family.
  rltmpa => reflist%next%next  ! no need to check the first one
reflectionloop3:  do while (associated(rltmpa))
    curfam = rltmpa%famhkl
    if (sum(abs(curfam-rltmpa%hkl)).ne.0) then
      newfam = rltmpa%hkl
      do while (sum(abs(rltmpa%famhkl-curfam)).eq.0)
        rltmpa%famhkl = newfam
        rltmpa => rltmpa%next
        if ( .not.associated(rltmpa) ) EXIT reflectionloop3
      end do
    else
      do while (sum(abs(rltmpa%famhkl-curfam)).eq.0)
        rltmpa => rltmpa%next
        if ( .not.associated(rltmpa) ) EXIT reflectionloop3
      end do
    end if
! go to the next beam in the list
  end do reflectionloop3

end subroutine Prune_ReflectionList


!--------------------------------------------------------------------------
!
! SUBROUTINE: Compute_DynMat
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the entire dynamical matrix, including HOLZ and Bethe potentials
!
!> @details This is a complicated routine because it forms the core of the dynamical
!> scattering simulations.  The routine must be capable of setting up the dynamical 
!> matrix for systematic row and zone axis, with or without HOLZ reflections, and 
!> with Bethe potentials for the Bloch wave case.  The routine must also be able to
!> decide which reflections are weak, and which are strong (again for the Bloch wave
!> case, but potentially also for other cases? Further research needed...).
!
!> @param cell unit cell pointer
!> @param reflist reflection list pointer
!> @param Dyn dynamical scattering structure
!> @param BetheParameter Bethe parameter structure 
!> @param calcmode string that describes the particular matrix mode
!> @param kk incident wave vector
!> @param kt tangential component of incident wave vector (encodes the Laue Center)
!> @param IgnoreFoilNormal switch for foil normal inclusion in sg computation
!> @param IncludeSecondOrder (optional) switch to include second order correction to Bethe potentials
!
!> @date   05/06/13 MDG 1.0 original
!> @date   08/30/13 MDG 1.1 correction of effective excitation error
!> @date   09/20/13 MDG 1.2 added second order Bethe potential correction switch
!> @date   06/09/14 MDG 2.0 added cell, reflist and BetheParameter as arguments
!--------------------------------------------------------------------------
recursive subroutine Compute_DynMat(cell,reflist,Dyn,BetheParameter,calcmode,kk,kt,IgnoreFoilNormal,IncludeSecondOrder)
!DEC$ ATTRIBUTES DLLEXPORT :: Compute_DynMat

use error
use constants
use crystal
use diffraction
use io

IMPLICIT NONE

type(unitcell)                          :: cell
type(reflisttype),pointer               :: reflist
type(DynType),INTENT(INOUT)            :: Dyn
!f2py intent(in,out) ::  Dyn
type(BetheParameterType),INTENT(INOUT) :: BetheParameter
!f2py intent(in,out) ::  BetheParameter
character(*),INTENT(IN)         :: calcmode             !< computation mode
real(kind=dbl),INTENT(IN)               :: kk(3),kt(3)          !< incident wave vector and tangential component
logical,INTENT(IN)                      :: IgnoreFoilNormal     !< how to deal with the foil normal
logical,INTENT(IN),OPTIONAL             :: IncludeSecondOrder   !< second order Bethe potential correction switch

complex(kind=dbl)                       :: czero,pre, weaksum, ughp, uhph
integer(kind=irg)                       :: istat,ir,ic,nn, iweak, istrong, iw, ig, ll(3), gh(3), nnn, nweak, io_int(1)
real(kind=sgl)                          :: glen,exer,gg(3), kpg(3), gplen, sgp, lUg, cut1, cut2, io_real(3)
real(kind=dbl)                          :: lsfour, weaksgsum 
logical                                 :: AddSecondOrder
type(gnode)                           :: rlp
type(reflisttype),pointer               :: rltmpa, rltmpb
 
AddSecondOrder = .FALSE.
if (present(IncludeSecondOrder)) AddSecondOrder = .TRUE.

! has the list of reflections been allocated ?
if (.not.associated(reflist)) call FatalError('Compute_DynMat',' reflection list has not been allocated')

! if the dynamical matrix has already been allocated, deallocate it first
! this is partially so that no program will allocate DynMat itself; it must be done
! via this routine only.
if (allocated(Dyn%DynMat)) deallocate(Dyn%DynMat)

! initialize some parameters
czero = cmplx(0.0,0.0,dbl)      ! complex zero
pre = cmplx(0.0,cPi,dbl)                ! i times pi

if (calcmode.ne.'BLOCHBETHE') then

! allocate DynMat
          allocate(Dyn%DynMat(cell%DynNbeams,cell%DynNbeams),stat=istat)
          Dyn%DynMat = czero
! get the absorption coefficient
          call CalcUcg(cell, rlp, (/0,0,0/),applyqgshift=.TRUE. )
          Dyn%Upz = rlp%Vpmod

! are we supposed to fill the off-diagonal part ?
         if ((calcmode.eq.'D-H-W').or.(calcmode.eq.'BLOCH')) then
          rltmpa => cell%reflist%next    ! point to the front of the list
! ir is the row index
          do ir=1,cell%DynNbeams
           rltmpb => cell%reflist%next   ! point to the front of the list
! ic is the column index
           do ic=1,cell%DynNbeams
            if (ic.ne.ir) then  ! exclude the diagonal
! compute Fourier coefficient of electrostatic lattice potential 
             gh = rltmpa%hkl - rltmpb%hkl
             if (calcmode.eq.'D-H-W') then
              call CalcUcg(cell, rlp,gh,applyqgshift=.TRUE.)
              Dyn%DynMat(ir,ic) = pre*rlp%qg
             else
              Dyn%DynMat(ir,ic) = cell%LUT(gh(1),gh(2),gh(3))
             end if
            end if
            rltmpb => rltmpb%next  ! move to next column-entry
           end do
           rltmpa => rltmpa%next   ! move to next row-entry
          end do
         end if
         
! or the diagonal part ?
         if ((calcmode.eq.'DIAGH').or.(calcmode.eq.'DIAGB')) then
          rltmpa => reflist%next   ! point to the front of the list
! ir is the row index
          do ir=1,cell%DynNbeams
           glen = CalcLength(cell,float(rltmpa%hkl),'r')
           if (glen.eq.0.0) then
            Dyn%DynMat(ir,ir) = cmplx(0.0,Dyn%Upz,dbl)
           else  ! compute the excitation error
            exer = Calcsg(cell,float(rltmpa%hkl),sngl(kk),Dyn%FN)

            rltmpa%sg = exer
            if (calcmode.eq.'DIAGH') then  !
             Dyn%DynMat(ir,ir) = cmplx(0.0,2.D0*cPi*exer,dbl)
            else
             Dyn%DynMat(ir,ir) = cmplx(2.D0*exer/cell%mLambda,Dyn%Upz,dbl)
            end if
           endif
           rltmpa => rltmpa%next   ! move to next row-entry
          end do
         end if

else  ! this is the Bloch wave + Bethe potentials initialization (originally implemented in the EBSD programs)

! we don't know yet how many strong reflections there are so we'll need to determine this first
! this number depends on some externally supplied parameters, which we will get from a namelist
! file (which should be read only once by the Set_Bethe_Parameters routine), or from default values
! if there is no namelist file in the folder.
        if (BetheParameter%cutoff.eq.0.0) call Set_Bethe_Parameters(BetheParameter,silent=.TRUE.)


! reset the value of DynNbeams in case it was modified in a previous call 
        cell%DynNbeams = cell%DynNbeamsLinked
        
! precompute lambda^2/4
        lsfour = cell%mLambda**2*0.25D0
  
! first, for the input beam direction, determine the excitation errors of 
! all the reflections in the master list, and count the ones that are
! needed for the dynamical matrix (weak as well as strong)
        if (.not.allocated(BetheParameter%weaklist)) allocate(BetheParameter%weaklist(cell%DynNbeams))
        if (.not.allocated(BetheParameter%stronglist)) allocate(BetheParameter%stronglist(cell%DynNbeams))
        if (.not.allocated(BetheParameter%reflistindex)) allocate(BetheParameter%reflistindex(cell%DynNbeams))
        if (.not.allocated(BetheParameter%weakreflistindex)) allocate(BetheParameter%weakreflistindex(cell%DynNbeams))

        BetheParameter%weaklist = 0
        BetheParameter%stronglist = 0
        BetheParameter%reflistindex = 0
        BetheParameter%weakreflistindex = 0

! line modified by MDG on 12/2/2020
        ! rltmpa => cell%reflist%next
        rltmpa => reflist%next

! deal with the transmitted beam first
    nn = 1              ! nn counts all the scattered beams that satisfy the cutoff condition
    nnn = 1             ! nnn counts only the strong beams
    nweak = 0           ! counts only the weak beams
    BetheParameter%stronglist(nn) = 1   ! make sure that the transmitted beam is always a strong beam ...
    BetheParameter%weaklist(nn) = 0
    BetheParameter%reflistindex(nn) = 1

    rltmpa%sg = 0.D0    

! loop over all reflections in the linked list    
    rltmpa => rltmpa%next
    reflectionloop: do ig=2,cell%DynNbeamsLinked
      gg = float(rltmpa%hkl)                    ! this is the reciprocal lattice vector 

! deal with the foil normal; if IgnoreFoilNormal is .TRUE., then assume it is parallel to the beam direction
     if (IgnoreFoilNormal) then 
! we're taking the foil normal to be parallel to the incident beam direction at each point of
! the standard stereographic triangle, so cos(alpha) = 1 always in eqn. 5.11 of EM
        kpg = kk+gg                             ! k0 + g (vectors)
        gplen = CalcLength(cell,kpg,'r')        ! |k0+g|
        rltmpa%sg = (1.0/cell%mLambda**2 - gplen**2)*0.5/gplen
     else
        rltmpa%sg = Calcsg(cell,gg,sngl(kk),Dyn%FN)
! here we need to determine the components of the Laue Center w.r.t. the g1 and g2 vectors
! and then pass those on to the routine; 
!       rltmpa%sg = CalcsgHOLZ(gg,sngl(kt),sngl(cell%mLambda))
! write (*,*) gg, Calcsg(gg,sngl(kk),DynFN), CalcsgHOLZ(gg,sngl(kt),sngl(cell%mLambda))
     end if

! use the reflection num entry to indicate whether or not this
! reflection should be used for the dynamical matrix
! We compare |sg| with two multiples of lambda |Ug|
!
!  |sg|>cutoff lambda |Ug|   ->  don't count reflection
!  cutoff lambda |Ug| > |sg| > weakcutoff lambda |Ug|  -> weak reflection
!  weakcutoff lambda |Ug| > |sg|  -> strong reflection
!
        sgp = abs(rltmpa%sg) 
        lUg = abs(rltmpa%Ucg) * cell%mLambda
        cut1 = BetheParameter%cutoff * lUg
        cut2 = BetheParameter%weakcutoff * lUg

! we have to deal separately with double diffraction reflections, since
! they have a zero potential coefficient !        
        if ( cell%dbdiff(rltmpa%hkl(1),rltmpa%hkl(2),rltmpa%hkl(3)) ) then  ! it is a double diffraction reflection
          if (sgp.le.BetheParameter%sgcutoff) then         
                nn = nn+1
                nnn = nnn+1
                BetheParameter%stronglist(ig) = 1
                BetheParameter%reflistindex(ig) = nnn
          end if
        else   ! it is not a double diffraction reflection
          if (sgp.le.cut1) then  ! count this beam
                nn = nn+1
! is this a weak or a strong reflection (in terms of Bethe potentials)? 
                if (sgp.le.cut2) then ! it's a strong beam
                        nnn = nnn+1
                        BetheParameter%stronglist(ig) = 1
                        BetheParameter%reflistindex(ig) = nnn
                else ! it's a weak beam
                        nweak = nweak+1
                        BetheParameter%weaklist(ig) = 1
                        BetheParameter%weakreflistindex(ig) = nweak
                end if
          end if
        end if
! go to the next beam in the list
      rltmpa => rltmpa%next
    end do reflectionloop

! if we don't have any beams in this list (unlikely, but possible if the cutoff and 
! weakcutoff parameters have unreasonable values) then we abort the run
! and we report some numbers to the user 
         if (nn.eq.0) then
           call Message(' no beams found for the following parameters:', frm = "(A)")
           io_real(1:3) = kk(1:3)
           call WriteValue(' wave vector = ', io_real,3)
           io_int(1) = nn
           call WriteValue('  -> number of beams = ', io_int, 1)
           call Message( '   -> check cutoff and weakcutoff parameters for reasonableness', frm = "(A)")
           call FatalError('Compute_DynMat','No beams in list')
        end if

! next, we define nns to be the number of strong beams, and nnw the number of weak beams.
         BetheParameter%nns = sum(BetheParameter%stronglist)
         BetheParameter%nnw = sum(BetheParameter%weaklist)
         
! add nns to the weakreflistindex to offset it; this is used for plotting reflections on CBED patterns
        do ig=2,cell%DynNbeamsLinked
          if (BetheParameter%weakreflistindex(ig).ne.0) then
            BetheParameter%weakreflistindex(ig) = BetheParameter%weakreflistindex(ig) + BetheParameter%nns
          end if
        end do
        
! We may want to keep track of the total and average numbers of strong and weak beams  
         BetheParameter%totweak = BetheParameter%totweak + BetheParameter%nnw
         BetheParameter%totstrong = BetheParameter%totstrong + BetheParameter%nns
         if (BetheParameter%nnw.lt.BetheParameter%minweak) BetheParameter%minweak=BetheParameter%nnw
         if (BetheParameter%nnw.gt.BetheParameter%maxweak) BetheParameter%maxweak=BetheParameter%nnw
         if (BetheParameter%nns.lt.BetheParameter%minstrong) BetheParameter%minstrong=BetheParameter%nns
         if (BetheParameter%nns.gt.BetheParameter%maxstrong) BetheParameter%maxstrong=BetheParameter%nns

! allocate arrays for weak and strong beam information
        if (allocated(BetheParameter%weakhkl)) deallocate(BetheParameter%weakhkl)
        if (allocated(BetheParameter%weaksg)) deallocate(BetheParameter%weaksg)
        if (allocated(BetheParameter%stronghkl)) deallocate(BetheParameter%stronghkl)
        if (allocated(BetheParameter%strongsg)) deallocate(BetheParameter%strongsg)
        if (allocated(BetheParameter%strongID)) deallocate(BetheParameter%strongID)
        allocate(BetheParameter%weakhkl(3,BetheParameter%nnw),BetheParameter%weaksg(BetheParameter%nnw))
        allocate(BetheParameter%stronghkl(3,BetheParameter%nns),BetheParameter%strongsg(BetheParameter%nns))
        allocate(BetheParameter%strongID(BetheParameter%nns))

! here's where we extract the relevant information from the linked list (much faster
! than traversing the list each time...)
        rltmpa => reflist%next    ! reset the a list
        iweak = 0
        istrong = 0
        do ir=1,cell%DynNbeamsLinked
             if (BetheParameter%weaklist(ir).eq.1) then
                iweak = iweak+1
                BetheParameter%weakhkl(1:3,iweak) = rltmpa%hkl(1:3)
                BetheParameter%weaksg(iweak) = rltmpa%sg
             end if
             if (BetheParameter%stronglist(ir).eq.1) then
                istrong = istrong+1
                BetheParameter%stronghkl(1:3,istrong) = rltmpa%hkl(1:3)
                BetheParameter%strongsg(istrong) = rltmpa%sg
! make an inverse index list
                BetheParameter%strongID(istrong) = ir           
             end if
           rltmpa => rltmpa%next
        end do

! now we are ready to create the dynamical matrix
        cell%DynNbeams = BetheParameter%nns

! allocate DynMat if it hasn't already been allocated and set to complex zero
          if (allocated(Dyn%DynMat)) deallocate(Dyn%DynMat)
          allocate(Dyn%DynMat(cell%DynNbeams,cell%DynNbeams),stat=istat)
          Dyn%DynMat = czero

! get the absorption coefficient
          call CalcUcg(cell, rlp, (/0,0,0/) )
          Dyn%Upz = rlp%Vpmod

! ir is the row index
       do ir=1,BetheParameter%nns
! ic is the column index
          do ic=1,BetheParameter%nns
! compute the Bethe Fourier coefficient of the electrostatic lattice potential 
              if (ic.ne.ir) then  ! not a diagonal entry
                 ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%stronghkl(1:3,ic)
                 Dyn%DynMat(ir,ic) = cell%LUT(ll(1),ll(2),ll(3)) 
        ! and subtract from this the total contribution of the weak beams
         weaksum = czero
         do iw=1,BetheParameter%nnw
              ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%weakhkl(1:3,iw)
              ughp = cell%LUT(ll(1),ll(2),ll(3)) 
              ll = BetheParameter%weakhkl(1:3,iw) - BetheParameter%stronghkl(1:3,ic)
              uhph = cell%LUT(ll(1),ll(2),ll(3)) 
              weaksum = weaksum +  ughp * uhph *cmplx(1.D0/BetheParameter%weaksg(iw),0.0,dbl)
         end do
        ! and correct the dynamical matrix element to become a Bethe potential coefficient
         Dyn%DynMat(ir,ic) = Dyn%DynMat(ir,ic) - cmplx(0.5D0*cell%mLambda,0.0D0,dbl)*weaksum
! do we need to add the second order corrections ?
                  if (AddSecondOrder) then 
                    weaksum = czero
                  end if
              else  ! it is a diagonal entry, so we need the excitation error and the absorption length
! determine the total contribution of the weak beams
                 weaksgsum = 0.D0
                  do iw=1,BetheParameter%nnw
                      ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%weakhkl(1:3,iw)
                      ughp = cell%LUT(ll(1),ll(2),ll(3)) 
                      weaksgsum = weaksgsum +  abs(ughp)**2/BetheParameter%weaksg(iw)
                 end do
                 weaksgsum = weaksgsum * cell%mLambda/2.D0
                 Dyn%DynMat(ir,ir) = cmplx(2.D0*BetheParameter%strongsg(ir)/cell%mLambda-weaksgsum,Dyn%Upz,dbl)
! do we need to add the second order corrections ?
                  if (AddSecondOrder) then 
                    weaksum = czero
                  end if
               end if
          end do
        end do
! that should do it for the initialization of the dynamical matrix

end if   ! Bethe potential initialization

end subroutine Compute_DynMat

!--------------------------------------------------------------------------
!
! SUBROUTINE: Set_Bethe_Parameters
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Read the Bethe potential parameters from a file, if it exists; otherwise take defaults
!
!> @details The parameters set in this routine determine the difference between strong and
!> weak reflections.  The routine checks for the presence of the BetheParameters.nml file
!> in the current folder.  If present, it will read the parameters, otherwise it will use 
!> defaults which have been determined to be reasonable based on dynamical EBSD runs. 
!
!> @param silent optional, if present, then no output
!> @date   05/08/13 MDG 1.0 original
!> @date   10/07/15 MDG 1.1 added c1 etc parameters to namelist file
!> @date   09/27/16 MDG 1.2 removed defunct variables from name list
!--------------------------------------------------------------------------
recursive subroutine Set_Bethe_Parameters(BetheParameter,silent,filename)
!DEC$ ATTRIBUTES DLLEXPORT :: Set_Bethe_Parameters

use io

IMPLICIT NONE

type(BetheParameterType),INTENT(INOUT)        :: BetheParameter
!f2py intent(in,out) ::  BetheParameter
logical,INTENT(IN),OPTIONAL     :: silent
character(fnlen),INTENT(IN),OPTIONAL :: filename

character(fnlen)                :: Bethefilename
logical                         :: fexist
real(kind=sgl)                  :: c1, c2, c3, sgdbdiff

namelist /BetheList/ c1, c2, c3, sgdbdiff

if (present(filename)) then
  Bethefilename = trim(filename)
else 
  Bethefilename = 'BetheParameters.nml'
end if 

! check for the presence of the namelist file in the current folder
inquire(file=trim(Bethefilename),exist=fexist)

! set all default values (must be done here, since nml file may not contain all of them)
c1 = 8.0_sgl            ! changed from 8 and 12 for a test on 8/14/15
c2 = 50.0_sgl           !
c3 = 100.0_sgl          !
sgdbdiff = 1.00_sgl     !

if (fexist) then ! check for the file in the local folder
! read the parameters from the file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(Bethefilename)),DELIM='APOSTROPHE')
 READ(UNIT=dataunit,NML=BetheList)
 close(UNIT=dataunit)
 if (.not.present(silent)) then
   call Message('Read Bethe parameters from BetheParameters.nml', frm = "(A)")
   write (6,nml=BetheList)
 end if
end if

BetheParameter%c1 = c1
BetheParameter%c2 = c2
BetheParameter%c3 = c3
BetheParameter%sgdbdiff = sgdbdiff

end subroutine Set_Bethe_Parameters



!--------------------------------------------------------------------------
!
! SUBROUTINE: ShortestGFOLZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brieffind the G vector (displacement FOLZ w.r.t. ZOLZ, Chapter 3)
!
!> @param cell unit cell pointer
!> @param k wavevector
!> @param ga first reflection
!> @param gb second reflection
!> @param  gshort shortest g-vector inclined to ga-gb plane
!> @param gp
!
!> @date  01/29/02 MDG 1.0 original
!> @date  04/29/13 MDG 2.0 rewrite
!> @date  06/09/14 MDG 2.1 added cell argument 
!--------------------------------------------------------------------------
recursive subroutine ShortestGFOLZ(cell,k,ga,gb,gshort,gp)
!DEC$ ATTRIBUTES DLLEXPORT :: ShortestGFOLZ

use io
use crystal
use error

IMPLICIT NONE

type(unitcell)                  :: cell
integer(kind=irg),INTENT(IN)    :: k(3)
integer(kind=irg),INTENT(IN)    :: ga(3)
integer(kind=irg),INTENT(IN)    :: gb(3)
integer(kind=irg),INTENT(OUT)   :: gshort(3)
integer(kind=irg),INTENT(OUT)   :: gp(3)


real(kind=sgl)                  :: gmin,gam11,gam12,gam22,g1(3),g2(3),g3(3),glen
integer(kind=irg),parameter     :: inm = 8
integer(kind=irg)               :: ih,ik,il,NN, io_int(1)

! look for the shortest reflection satisfying hu+kv+lw = 1
! This could be replaced by code from Jackson's paper (1987),
! but it does essentially the same thing.
 gmin = 100.0
 NN=1
 g1 = float(ga)
 g2 = float(gb)
 do while((gmin.eq.100.0).and.(NN.lt.4))
  do ih=-inm,inm
   do ik=-inm,inm
    do il=-inm,inm
! does this reflection lie in the plane NN ?
     if ((ih*k(1)+ik*k(2)+il*k(3)).eq.NN) then
      glen = CalcLength(cell,float((/ih,ik,il/)),'r')
      if (glen.lt.gmin) then
       gmin = glen
       gshort =  (/ ih,ik,il /) 
      end if
     end if
    end do
   end do
  end do
  if (gmin.eq.100.0) then 
    io_int(1) = NN
    call WriteValue(' Could not find any reflections with hu+kv+lw = ', io_int, 1, "(I2)")
    NN = NN+1
  end if
 end do
 if (gmin.eq.100.0) then ! for some reason there is no reflection with N<=3 ...
  call FatalError('ShortestGFOLZ','HOLZ: could not find any reflections with hu+kv+lw<=maxholz ...')
 end if
 g3 = float(gshort)
! projected components of G
 gam11 = CalcDot(cell,g1,g1,'r')
 gam12 = CalcDot(cell,g1,g2,'r')
 gam22 = CalcDot(cell,g2,g2,'r')
 gmin = 1.0/(gam11*gam22-gam12**2)
 gp(1) = (CalcDot(cell,g3,g1,'r')*gam22-CalcDot(cell,g3,g2,'r')*gam12)*gmin
 gp(2) = (CalcDot(cell,g3,g2,'r')*gam11-CalcDot(cell,g3,g1,'r')*gam12)*gmin

end subroutine ShortestGFOLZ


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetSubReflist
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief used to subselect a series of reflections from an existing reflist
!
!> @date 11/18/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetSubReflist(cell, listroot, BetheParameter, FN, kk, nref, listrootw, nns, nnw, first)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSubRefList

use local
use typedefs
use diffraction

IMPLICIT NONE

type(unitcell)                                 :: cell
type(reflisttype),pointer                      :: listroot
type(BetheParameterType),INTENT(IN)            :: BetheParameter
real(kind=sgl),INTENT(IN)                      :: FN(3)
real(kind=sgl),INTENT(IN)                      :: kk(3)
integer(kind=irg),INTENT(IN)                   :: nref
type(reflisttype),pointer                      :: listrootw
integer(kind=irg),INTENT(OUT)                  :: nns
integer(kind=irg),INTENT(OUT)                  :: nnw
logical,INTENT(IN),OPTIONAL                    :: first

integer(kind=irg),allocatable,SAVE             :: glist(:,:)
real(kind=dbl),allocatable,SAVE                :: rh(:)
integer(kind=irg),SAVE                         :: icnt
real(kind=dbl),SAVE                            :: la

type(reflisttype),pointer                      :: rl, lastw, lasts
integer(kind=irg)                              :: istat, gmh(3), ir, ih
real(kind=dbl)                                 :: sgp, m, sg, gg(3)


! this routine must go through the entire reflist and apply the Bethe criteria for a 
! given beam and foil normal pair. Hence, it is similar to the Apply_BethePotentials
! routine, except that the excitation errors are recomputed each time this routine
! is called, to account for the different incident beam directions.

nullify(lasts)
nullify(lastw)
nullify(rl)

! first we extract the list of g-vectors from reflist, so that we can compute 
! all the g-h difference vectors; we only need to do this the first time we call
! this routine, since this list never changes (hence the SAVE qualifier for glist)
if (PRESENT(first)) then
  if (first) then
    la = 1.D0/cell%mLambda
    allocate(glist(3,nref),rh(nref),stat=istat)
    rl => listroot%next
    icnt = 0
    do
      if (.not.associated(rl)) EXIT
      icnt = icnt+1
      glist(1:3,icnt) = rl%hkl(1:3)
      rl => rl%next
    end do
  end if
else
end if

! initialize the strong and weak reflection counters
nns = 1
nnw = 0

! the first reflection is always strong
rl => listroot%next
rl%strong = .TRUE.
rl%weak = .FALSE.
lasts => rl
nullify(lasts%nextw)


! next we need to iterate through all reflections in glist and 
! determine which category the reflection belongs to: strong, weak, ignore
irloop: do ir = 2,icnt
  rl => rl%next
  rh = 0.D0
! here we need to actually compute the excitation error sg, since kk and FN are different each time the routine is called
  gg = dble(rl%hkl)
  sg = Calcsg(cell,gg,dble(kk),dble(FN))
  sgp = la * abs(sg)

  do ih = 1,icnt
   gmh(1:3) = glist(1:3,ir) - glist(1:3,ih)
   if (cell%dbdiff(gmh(1),gmh(2),gmh(3))) then  ! it is a double diffraction reflection with |U|=0
! to be written
     rh(ih) = 10000.D0 
   else 
     rh(ih) = sgp/abs( cell%LUT(gmh(1), gmh(2), gmh(3)) )
   end if
  end do

! which category does reflection ir belong to ?
  m = minval(rh)

! m > c2 => ignore this reflection
  if (m.gt.BetheParameter%c2) then
    rl%weak = .FALSE.
    rl%strong = .FALSE.
    CYCLE irloop
  end if

! c1 < m < c2 => weak reflection
  if ((BetheParameter%c1.lt.m).and.(m.le.BetheParameter%c2)) then
    if (nnw.eq.0) then
      listrootw => rl
      lastw => rl
    else
      lastw%nextw => rl
      lastw => rl
      nullify(lastw%nexts)
    end if
    rl%weak = .TRUE.
    rl%strong = .FALSE.
    nnw = nnw + 1
    CYCLE irloop
  end if

! m < c1 => strong
  if (m.le.BetheParameter%c1) then
    lasts%nexts => rl
    nullify(lasts%nextw)
    lasts => rl
    rl%weak = .FALSE.
    rl%strong = .TRUE.
    nns = nns + 1
  end if  
end do irloop

end subroutine GetSubRefList



!--------------------------------------------------------------------------
!
! SUBROUTINE: Delete_gvectorlist
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief delete the entire linked list
!
!> @param top top of the list to be removed
!
!> @date   04/29/13 MDG 1.0 original
!> @date   06/09/14 MDG 1.1 added cell argument
!> @date   06/17/14 MDG 1.2 replaced cell by top
!--------------------------------------------------------------------------
recursive subroutine Delete_gvectorlist(top)
!DEC$ ATTRIBUTES DLLEXPORT :: Delete_gvectorlist

IMPLICIT NONE

type(reflisttype),pointer       :: top

type(reflisttype),pointer       :: rltail, rltmpa

! deallocate the entire linked list before returning, to prevent memory leaks
rltail => top
rltmpa => rltail % next
do 
  deallocate(rltail)
  if (.not. associated(rltmpa)) EXIT
  rltail => rltmpa
  rltmpa => rltail % next
end do

end subroutine Delete_gvectorlist

!--------------------------------------------------------------------------
!
! SUBROUTINE: Compute_ReflectionList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the entire reflection list for general conditions (including HOLZ)
!
!> @details also computes the LUT (LookUpTable) that stores all the scattering
!> potential coefficients that are needed to fill the dynamical matrix (only for the 
!> general case).
!
!> @param dmin minimum d-spacing to allow in the list
!> @param k zone axis indices
!> @param ga first reciprocal vector of zone
!> @param gb second reciprocal vector 
!> @param method  approach to follow (ALL or ZA)
!> @param ConvertList logical, determines whether or not conversion of list is needed
!> @param maxholz maximum range of HOLZ reflections to include
!> @param convang (optional) beam convergence angle for beam inclusion selection (used for CBED etc.)
!
!> @date 04/29/13 MDG 1.0 original
!> @date 09/20/13 MDG 1.1 corrected handling of LUT
!> @date 10/05/13 MDG 1.2 limit the range of reflections by means of the convergence angle (optional)
!--------------------------------------------------------------------------
recursive subroutine Compute_ReflectionListZoneAxis(cell,listroot,BetheParameter,FN,dmin,k,ga,gb,nref)
!DEC$ ATTRIBUTES DLLEXPORT :: Compute_ReflectionListZoneAxis

use local
use io
use crystal
use constants
use diffraction
use symmetry
use typedefs

IMPLICIT NONE

type(unitcell)        				:: cell
type(reflisttype),pointer			:: listroot
real(kind=sgl),INTENT(IN)			:: FN(3)
real(kind=sgl),INTENT(IN)			:: dmin
type(BetheParameterType),INTENT(INOUT)		:: BetheParameter
!f2py intent(in,out) ::  BetheParameter
real(kind=sgl),INTENT(IN)			:: k(3)
integer(kind=irg),INTENT(IN)			:: ga(3)
integer(kind=irg),INTENT(IN)			:: gb(3)
integer(kind=irg),INTENT(OUT)			:: nref

integer(kind=irg)				:: imh, imk, iml, gg(3), ix, iy, iz, i, minholz, RHOLZ, im, istat, N, &
                                                   ig, numr, ir, irsel
real(kind=sgl)					:: dhkl, io_real(6), H, g3(3), g3n(3), FNg(3), ddt, s, kr(3), exer, &
                                                   rBethe_i, rBethe_d, sgp, r_g, la, dval
integer(kind=irg)				:: io_int(3), gshort(3), gp(3)

type(reflisttype),pointer			:: rltail

! set the truncation parameters
  rBethe_i = BetheParameter%c3          ! if larger than this value, we ignore the reflection completely
  rBethe_d = BetheParameter%sgdbdiff    ! excitation error cutoff for double diffraction reflections
  la = 1.0/sngl(cell%mLambda)
  
! get the size of the lookup table
  gp = shape(cell%LUT)
  imh = (gp(1)-1)/4
  imk = (gp(2)-1)/4
  iml = (gp(3)-1)/4
  
  nullify(listroot)
  nullify(rltail)
  kr = k
  call NormVec(cell, kr, 'r') 
  kr = kr/cell%mlambda 

  !io_real = (/ float(ga(1:3)), float(gb(1:3))/)
  !call WriteValue('basis vectors for this computation: ', io_real, 6, "(/'ga = ',3f10.5,/'gb = ',3f10.5,/')")
  gg = (/0,0,0/)
  call AddReflection(rltail, listroot, cell, nref, gg )  ! this guarantees that 000 is always the first reflection
  
  rltail%sg = 0.0
! now compute |sg|/|U_g|/lambda for the other allowed reflections; if this parameter is less than
! the threshhold, rBethe_i, then add the reflection to the list of potential reflections
! note that this uses the older form of the Bethe Potential truncation parameters for now

do ix=-imh,imh
	do iy=-imk,imk
		gg = ix*ga + iy*gb
		if ((abs(gg(1))+abs(gg(2))+abs(gg(3))).ne.0) then  ! avoid double counting the origin
			dval = 1.0/CalcLength(cell, float(gg), 'r' )
			if ((IsGAllowed(cell,gg)).AND.(dval .gt. dmin)) then
				sgp = Calcsg(cell,float(gg),kr,FN)
				if  ((abs(gg(1)).le.imh).and.(abs(gg(2)).le.imk).and.(abs(gg(3)).le.iml) ) then
					if (cell%dbdiff(gg(1), gg(2), gg(3))) then ! potential double diffraction reflection
						if (abs(sgp).le.rBethe_d) then 
							call AddReflection(rltail, listroot, cell, nref, gg)
							rltail%sg = sgp
							rltail%dbdiff = .TRUE.
						end if 
					else
						r_g = la * abs(sgp)/abs(cell%LUT(gg(1), gg(2), gg(3)))
            write (*,*) gg, sgp, r_g, rBethe_i
						if (r_g.le.rBethe_i) then 
							call AddReflection(rltail, listroot, cell, nref, gg )
							rltail%sg = sgp
							rltail%dbdiff = .FALSE.
						end if
					end if
				end if
			end if
		end if
	end do
end do
io_int(1) = nref
call WriteValue(' Length of the master list of reflections : ', io_int, 1, "(I5,/)")

 
end subroutine Compute_ReflectionListZoneAxis




end module gvectors
