! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMdymodHDF.f90
!--------------------------------------------------------------------------
!
! MODULE: EMdymodHDF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines that can be called by external code
!
!> @date  10/16/15 MDG 1.0 original
!> @date  01/11/15 MDG 2.0 split from EMdymod (everything that depends on HDF library)
!--------------------------------------------------------------------------
module EMdymodHDF

use EMdymod

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
! SUBROUTINE:SinglePEDPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief This function can be called as a standalone function to compute a kinematical PED pattern
!
!> @etails The main purpose of this routine and its accompanying wrapper routine is to
!> provide a way for an external program to compute a kinematical PED pattern.  The idea is that 
!> all the necessary arrays and variables are passed in by reference as arguments, without
!> the need for the routine to fetch any other data from files etc...  The initial goal is
!> to have a function that can be called with the CALL_EXTERNAL mechanism in IDL, but 
!> in the long run this will also be the approach for calling the routine from C/C++, which
!> is an essential part of integration with DREAM.3D.  
!>
!> @param ipar array with integer input parameters
!> @param fpar array with float input parameters
!> @param cpar array with string parameters
!> @param PEDPattern output array
!> @param quats array of quaternions
!
!> @date 11/15/15 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine SinglePEDPattern(ipar, fpar, cpar, PEDpattern, quats)
!DEC$ ATTRIBUTES DLLEXPORT :: SinglePEDPattern

! the input parameters are all part of a ipar and fpar input arrays instead of the usual namelist structures.
! The following is the mapping:
!
! ipar(1) = 1 if GetVectorsCone detector arrays need to be computed, 0 if not (arrays will have save status)
! ipar(2) = pednl%npix
! ipar(3) = numquats
! ipar(4) = 
! ipar(5) = 
! ipar(6) = 

! fpar(1) = pednl%dmin
! fpar(2) = pednl%voltage
! fpar(3) = pednl%rnmpp
! fpar(4) = pednl%thickness

! cpar(1) = pednl%xtalname


use local
use typedefs
use crystal
use initializersHDF
use gvectors
use io
use diffraction
use symmetry
use quaternions
use NameListTypedefs
use constants
use rotations
use so3
use math
use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

integer(c_size_t),PARAMETER             :: nipar=6
integer(c_size_t),PARAMETER             :: nfpar=4
integer(c_size_t),PARAMETER             :: ncpar=1
integer(c_size_t),PARAMETER             :: nq=4
integer(c_size_t),INTENT(IN)            :: ipar(nipar)
real(kind=sgl),INTENT(IN)               :: fpar(nfpar)
character(fnlen),INTENT(IN)             :: cpar(ncpar)
real(kind=sgl),INTENT(OUT)              :: PEDpattern(ipar(2),ipar(2),ipar(4))
real(kind=sgl),INTENT(IN)               :: quats(nq,ipar(4))

type(unitcell)        ,SAVE             :: cell
type(DynType),save                      :: Dyn
type(gnode),save                        :: rlp
type(reflisttype),pointer,SAVE          :: reflist, nexts, rltmpa
real(kind=sgl),SAVE                     :: Igmax, xgmin, sgmax, rnmpp
real(kind=sgl),allocatable,SAVE         :: xx(:,:), yy(:,:), dot(:,:)
integer(kind=irg),SAVE                  :: ww, tdp, nsize, nref

integer(kind=irg)                       :: pgnum, ival, i, j
real(kind=sgl)                          :: la, dval, dmin, glen, gmax, om(3,3), k(3), FN(3), Ig, & 
                                           maxint, w, ku(3), kp(3), dx, dy, eu(3), tstart, tstop, x, y, ma, mi
integer(kind=irg)                       :: gp(3), imh, imk, iml, gg(3), ix, iy, iz, sx, sy, ip
real(kind=sgl),allocatable              :: line(:), pedp(:,:)


! should we initialize the necessary arrays ? [this should be done the first time we call this routine]
if (ipar(1).gt.0) then
  sgmax = 0.50

!=============================================
! crystallography section
  !nullify(cell)        
  !allocate(cell)        
  call Initialize_Cell(cell,Dyn,rlp,cpar(1), fpar(1), fpar(2))

!=============================================
! generation of all potential reflections inside a reciprocal space sphere
! computed from the camera length and the detector size ...

! first set the maximum |g| value that can possibly give rise to a diffracted beam on the detector (diagonal)
  gmax = sqrt(2.0) * float(ipar(2)) * fpar(3)

! this code is taken from the Initialize_ReflectionList routine, but we do not
! need everything from that routine; first get the size of the lookup table
  gp = shape(cell%LUT)
  imh = (gp(1)-1)/4
  imk = (gp(2)-1)/4
  iml = (gp(3)-1)/4

! initialize the reflection list
  nullify(reflist)
  nullify(rltmpa)
  nref = 0
 
! transmitted beam always has excitation error zero
  gg = (/ 0,0,0 /)
  call AddReflection(rltmpa, reflist, cell, nref, gg)   ! this guarantees that 000 is always the first reflection
  rltmpa%xg = 0.0
  xgmin = 100000.0
  Igmax = 0.0

! now compute |U_g|^2 for all allowed reflections and place the values in the linked reflist; 
ixl: do ix=-imh,imh
iyl:  do iy=-imk,imk
izl:   do iz=-iml,iml
        if ((abs(ix)+abs(iy)+abs(iz)).ne.0) then  ! avoid double counting the origin
         gg = (/ ix, iy, iz /)
         glen = CalcLength(cell, float(gg), 'r' )

! find all reflections, ignoring double diffraction spots
         if ((IsGAllowed(cell,gg)).and.(glen.le.gmax)) then ! allowed by the lattice centering, if any
            call AddReflection(rltmpa, reflist, cell, nref, gg )
! we'll use the sangle field of the rltail structure to store |Ug|^2; we will also need the extinction distance
            rltmpa%sangle = abs(cell%LUT(ix, iy, iz))**2
            if (rltmpa%sangle.gt.Igmax) Igmax = rltmpa%sangle
            rltmpa%xg = 1.0/(abs(cell%LUT(ix,iy,iz))*cell%mLambda)
            if (rltmpa%xg.lt.xgmin) xgmin = rltmpa%xg
         end if ! IsGAllowed
        end if
       end do izl
      end do iyl
    end do ixl

!=============================================
! create the coordinate arrays for the Gaussian peaks that will represent the diffraction spots
  rnmpp = 1.0/fpar(3)
  ww = 4
  tdp = 2*ww+1
  nsize = ipar(2)/2 + ww 
  allocate(xx(-ww:ww,-ww:ww), yy(-ww:ww,-ww:ww), line(-ww:ww), dot(-ww:ww,-ww:ww))
  line = (/ (float(i),i=-ww,ww) /) * rnmpp
  xx = spread(line,dim=1,ncopies=tdp)
  yy = transpose(xx)
  deallocate(line)
end if

! here we start the actual pattern calculation ... 
allocate(pedp(-nsize:nsize,-nsize:nsize))

do ip=1,ipar(3)   ! loop over all requested orientations
  pedp = 0.0
! convert quaternion to orientation matrix
  om = qu2om(quats(1:4,ip)) 
! determine the appropriate wave vector and foil normal
  k = (/ 0.0, 0.0, 1.0 /)
  ku = matmul(om,k)
  FN = ku
  k = ku/sngl(cell%mLambda)

! first we go through the entire reflection list and compute the excitation errors
! those points that satisfy the cutoff are linked via the nexts pointers
    rltmpa => reflist%next
    nexts => rltmpa
    do j=1,nref
      gg = rltmpa%hkl
      rltmpa%sg = Calcsg(cell,float(gg),k,FN)
! should we consider this point any further ? If so, add it to the strong reflection linked list
      if (abs(rltmpa%sg).le.sgmax) then 
        nexts%nexts => rltmpa
        nexts => rltmpa
      end if
      rltmpa => rltmpa%next
    end do

! then, for each point in the nexts list, we compute the components of k' = k+g+s
! and place them in the proper reference frame; we skip the incident beam since it is 
! meaningless in the kinematical approximation
  nexts => reflist%next%nexts
  om = transpose(om)
  do 
! determine the vector k'
    kp = k + float(nexts%hkl) + nexts%sg*ku
    kp = matmul(om,kp)

! get the intensity for each point
    w = sngl(cPi)*nexts%sg*fpar(4)
    if (abs(w).lt.1.0e-6) then
      Ig = nexts%sangle  ! * (sngl(cPi)*pednl%thickness/nexts%xg)**2
    else 
      Ig = nexts%sangle * (sin(w)/w)**2 ! * (sngl(cPi)*pednl%thickness/nexts%xg)**2
    end if

! determine the spot coordinates on the detector
    x = rnmpp * kp(1)
    y = rnmpp * kp(2)

! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
    if ((abs(x).le.nsize-ww).and.(abs(y).le.nsize-ww)) then
      sx = nint(x)
      sy = nint(y)
      dx = x-sx
      dy = y-sy
      dot = (Ig/Igmax)**0.2 * exp(-((xx-dx)**2+(yy-dy)**2)*0.003)
      pedp(sx-ww:sx+ww,sy-ww:sy+ww) = pedp(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
    end if

! and repeat this until the end of the list
    if (.not. associated(nexts%nexts)) EXIT
    nexts => nexts%nexts
  end do

! put the pedp pattern in the output array
  PEDpattern(1:ipar(2),1:ipar(2),ip) = pedp

! reset the nexts linked list and start over
  nexts => reflist%next
  rltmpa => nexts%nexts
  do 
    nullify(nexts%nexts)
    if (.not. associated(rltmpa%nexts)) EXIT
    nexts => rltmpa
    rltmpa => rltmpa%nexts
  end do
end do

deallocate(pedp)

end subroutine SinglePEDPattern

!--------------------------------------------------------------------------
!
! SUBROUTINE:SinglePEDPatternWrapper
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief wrapper routine for SinglePEDPattern; nearly identical to ECP case
!>
!> see https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/Gk0xxVFbW8E
!
!> @param argc number of argument
!> @param argv pointers to subroutine parameters
!
!> @date 11/15/15 MDG 1.0 first version
!--------------------------------------------------------------------------
recursive function SinglePEDPatternWrapper(argc, argv) bind(c, name='SinglePEDPatternWrapper') 
!DEC$ ATTRIBUTES DLLEXPORT :: SinglePEDPatternWrapper

use,INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

INTEGER(c_size_t), VALUE, INTENT(IN)            :: argc 
type(c_ptr), dimension(argc), INTENT(INOUT)     :: argv
!f2py intent(in,out) ::  argv
REAL(c_float)                                   :: SinglePEDPatternWrapper

! wrapper function dependent declarations; they are all pointers 
! since we pass everything by reference from IDL 
integer(c_size_t)                               :: nipar, nfpar, ncpar, nq
integer(c_size_t),dimension(:), pointer         :: ipar
real(c_float), dimension(:), pointer            :: fpar
character(c_char), dimension(:), pointer        :: cpar
real(c_float), dimension(:,:), pointer          :: quats
real(c_float), dimension(:,:,:), pointer        :: PEDPattern

! the following line just helps in identifying the correct order of the subroutine arguments...
!                              1      2     3       4        5
!subroutine SinglePEDpattern(ipar, fpar, cpar, PEDPattern, quats)
!
! transform the C pointers above to fortran pointers, and use them in the regular function call
nipar = 6
nfpar = 1
nq = 4
call c_f_pointer(argv(1),ipar,(/nipar/)) 
call c_f_pointer(argv(2),fpar,(/nfpar/)) 
call c_f_pointer(argv(3),cpar,(/ncpar/)) 
call c_f_pointer(argv(4),PEDpattern,(/ipar(2),ipar(2),ipar(4)/))
call c_f_pointer(argv(5),quats,(/nq,ipar(4)/))

call SinglePEDPattern(ipar, fpar, cpar, PEDpattern, quats)

SinglePEDPatternWrapper = 1._c_float
end function SinglePEDPatternWrapper

end module EMdymodHDF
