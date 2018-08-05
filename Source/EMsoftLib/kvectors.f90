! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
! EMsoft:kvectors.f90
!--------------------------------------------------------------------------
!
! MODULE: kvectors
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief variables and types needed to determine lists of wave vectors
!
!> @details This was part of the dynamical module, but was moved into a separate
!> module once I realized that I had several versions of the Calckvectors subroutine.
!> From now on, there is only one single version that can deal with all possible cases.
! 
!> @date   04/29/13 MDG 1.0 original
!--------------------------------------------------------------------------
module kvectors

use local
use typedefs

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! FUNCTION: Kdelta
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Kronecker delta function, returns 1 or 0
!
!> @param i first entry
!> @param j second entry 
!
!> @date   04/29/13 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function Kdelta(i,j) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: Kdelta

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: i,j
integer(kind=irg)               :: res

 if (i.eq.j) then 
   res = 1
 else
   res = 0
 end if

end function Kdelta

!--------------------------------------------------------------------------
!
! SUBROUTINE: Calckvectors
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors
!
!> @details This is a new version that combines several older routines.  The most important 
!> aspects of this routine are a) linked list can use regular mapping or modified Lambert mapping;
!> b) list makes use of crystal symmetry (although that feature can be turned off); c) routine
!> has been cleaned up, and there is now a Delete_kvectorlist function as well.  This is a very 
!> complex routine so make sure you fully understand it before you attempt to modify or add anything!
!
!> @param khead head of linked list
!> @param cell unit cell pointer
!> @param k central wave vector
!> @param ga reciprocal lattice vector normal to k
!> @param ktmax maximum length of tangential component
!> @param npx number of vectors along x 
!> @param npy number of vectors along y 
!> @param numk total number of wave vectors in list
!> @param isym Laue group number
!> @param ijmax used for conical illumination
!> @param mapmode sets the mapping mode to be used ('Conical', 'Standard', 'StandardConical', 'RoscaLambert')
!
!> @todo The Standard and RoscaLambert mapmodes have different considerations of the 
!> Laue groups; this needs to be verified and, if necessary, simplified to a single set of 
!> conditions.  This might also allow Addkvector and Add_knode to become a single routine.
!
!> @date   04/29/13 MDG 1.0 original
!> @date   06/09/14 MDG 2.0 added khead and cell as arguments
!> @date   06/19/14
!> @date   08/25/15 MDG 3.0 modified symmetry handling to full point group, with doubled Lambert projection
!> @date   11/13/15 MDG 3.1 inclusion of small epsilon in trigonal and hexagonal cases, to eliminate rounding errors
!--------------------------------------------------------------------------
recursive subroutine Calckvectors(khead,cell,k,ga,ktmax,npx,npy,numk,isym,ijmax,mapmode,usehex)
!DEC$ ATTRIBUTES DLLEXPORT :: Calckvectors

use io
use error
use constants
use diffraction
use crystal

IMPLICIT NONE

type(kvectorlist),pointer               :: khead
type(unitcell),pointer                  :: cell
real(kind=dbl),INTENT(IN)               :: k(3)         !< initial wave vector
real(kind=dbl),INTENT(IN)               :: ga(3)        !< "horizontal" reciprocal lattice vector
real(kind=dbl),INTENT(IN)               :: ktmax        !< maximum length of tangential wave vector
integer(kind=irg),INTENT(IN)            :: npx          !< number of kvectors along x
integer(kind=irg),INTENT(IN)            :: npy          !< number of kvectors along y
integer(kind=irg),INTENT(OUT)           :: numk         !< total number of kvectors in linked list
integer(kind=irg),INTENT(IN)            :: isym         !< Laue symmetry group number 
integer(kind=irg),INTENT(INOUT)         :: ijmax        !< max parameter used for Conical and StandardConical modes
character(*),INTENT(IN)                 :: mapmode      !< controls the type of mapping used ('Standard' or 'RoscaLambert')
!real(kind=sgl),INTENT(IN)              :: klaue(2)     !< Laue center coordinates
logical,INTENT(IN),OPTIONAL             :: usehex       !< hexagonal mode for RoscaLambert mapmode

integer(kind=irg)                       :: istat,i,j,istart,iend,jstart,jend, imin, imax, jmin, jmax, ii, jj
real(kind=dbl)                          :: glen, gan(3), gperp(3), kstar(3), delta, xy(2), xx, yy, eps
logical                                 :: hexgrid = .FALSE., yes = .TRUE., flip = .TRUE., check
character(3)                            :: grid
type(kvectorlist),pointer               :: ktail, ktmp
real(kind=sgl)                          :: xytest(2), xxtest, yytest

! first, if khead already exists, delete it
 !if (associated(khead)) then                    ! deallocate the entire linked list
 !   call Delete_kvectorlist(khead)
 !end if
 
! do we know this mapmode ?
if ( .not.( (mapmode.eq.'Conical').or.(mapmode.eq.'Standard').or.(mapmode.eq.'StandardConical').or. &
   (mapmode.eq.'RoscaLambert') ) ) then
  call FatalError('Calckvectors','mapmode unknown')
end if

if (mapmode.eq.'Conical') then ! used for CBED without symmetry application, including EMZAdefect
! compute geometrical factors 
 glen = CalcLength(cell,ga,'r')                         ! length of ga
 gan = ga/glen                                  ! normalized ga
 delta = 2.0*ktmax*glen/(2.0*float(npx)+1.0)    ! grid step size in nm-1 
 call TransSpace(cell,k,kstar,'d','r')                  ! transform incident direction to reciprocal space
 call CalcCross(cell,ga,kstar,gperp,'r','r',0)          ! compute g_perp = ga x k
 call NormVec(cell,gperp,'r')                           ! normalize g_perp
 call NormVec(cell,kstar,'r')                           ! normalize reciprocal beam vector

! allocate the head and tail of the linked list
 allocate(khead,stat=istat)                             ! allocate new value
 if (istat.ne.0) call FatalError('Calckvectors','unable to allocate khead pointer')
 ktail => khead                                         ! tail points to new value
 nullify(ktail%next)                                    ! nullify next in new value
 numk = 1                                               ! keep track of number of k-vectors so far
 ktail%i = 0                                            ! i-index of beam
 ktail%j = 0                                            ! j-index of beam
 ktail%kt = (/0.0,0.0,0.0/)                             ! no tangential component for central beam direction
 ktail%k = kstar/cell%mLambda                           ! divide by wavelength
 ktail%kn = CalcDot(cell,ktail%k,kstar,'r')                     ! normal component

! set the loop limits
 imin = -npx; imax = npx; jmin = -npy; jmax = npy; 

! and loop over the entire range (without symmetry considerations
 do i=imin,imax
  do j=jmin,jmax
   if (.not.((i.eq.0).and.(j.eq.0))) then               ! the point (0,0) has already been taken care of
    if ((i**2+j**2).le.ijmax) then                      ! only directions inside the incident cone
     call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/))     ! add k-vector to linked list
    end if
   end if
  end do
 end do

end if  ! mapmode = Conical


! standard or standard-conical kvector list, as used by CBED and other programs
if ( (mapmode.eq.'Standard').or.(mapmode.eq.'StandardConical') ) then

! for standard mode, we want to make sure that ijmax, which need not be defined by 
! the calling program for this mode, is set to a large value
  if (mapmode.eq.'Standard') then 
    ijmax = (5*npx)**2
  end if

! compute geometrical factors 
 glen = CalcLength(cell,ga,'r')                         ! length of ga
 gan = ga/glen                                  ! normalized ga
 delta = 2.0*ktmax*glen/(2.0*float(npx)+1.0)    ! grid step size in nm-1 
 call TransSpace(cell,k,kstar,'d','r')                  ! transform incident direction to reciprocal space
 call CalcCross(cell,ga,kstar,gperp,'r','r',0)          ! compute g_perp = ga x k
 call NormVec(cell,gperp,'r')                           ! normalize g_perp
 call NormVec(cell,kstar,'r')                           ! normalize reciprocal beam vector

! allocate the head and tail of the linked list
 allocate(khead,stat=istat)                             ! allocate new value
 if (istat.ne.0) call FatalError('Calckvectors','unable to allocate khead pointer')
 ktail => khead                                         ! tail points to new value
 nullify(ktail%next)                                    ! nullify next in new value
 numk = 1                                               ! keep track of number of k-vectors so far
 ktail%i = 0                                            ! i-index of beam
 ktail%j = 0                                            ! j-index of beam
 ktail%kt = (/0.0,0.0,0.0/)                             ! no tangential component for central beam direction
 ktail%k = kstar/cell%mLambda                           ! divide by wavelength
 ktail%kn = CalcDot(cell,ktail%k,kstar,'r')                     ! normal component

! implement symmetry Table 7.3 from EM book
  select case(isym)  ! negative values -> systematic row; positive -> zone axis
   case(-1)  ! centrosymmetric systematic row
     imin = 0; imax = npx; grid = 'srw'
   case(-2)  ! non-centrosymmetric systematic row
     imin = -npx; imax = npx; grid = 'srw'
   case(1)  ! 2D Group 1
     imin = -npx; imax = npx; jmin = -npy; jmax = npy; grid = 'sqa'
   case(2)  ! 2D Group 2
     imin = -npx; imax = npx; jmin = 0; jmax = npy; grid = 'sqb'
   case(3)  ! 2D Group m
     imin = -npx; imax = npx; jmin = 0; jmax = npy; grid = 'sqa'
   case(4)  ! 2D Group 2mm
     imin = 0; imax = npx; jmin = 0; jmax = npy; grid = 'sqa'
   case(5)  ! 2D Group 4
     imin = 1; imax = npx; jmin = 0; jmax = npy; grid = 'sqa'
   case(6)  ! 2D Group 4mm
     imin = 0; imax = npx; jmin = 0; jmax = npy; grid = 'sqc'
   case(7)  ! 2D Group 3   (cubic version)
     grid = 'hxa'; hexgrid=.TRUE.
   case(8)  ! 2D Group 31m  (cubic version)
     grid = 'hxb'; hexgrid=.TRUE.
   case(9)  ! 2D Group 6
     grid = 'hxe'; hexgrid=.TRUE.
   case(10)  ! 2D Group 6mm
     grid = 'hxf'; hexgrid=.TRUE.
   case(11)  ! 2D Group 3  (hexagonal setting)
     grid = 'hxc'; hexgrid=.TRUE.
   case(12)  ! 2D Group 31m (hexagonal setting)
     grid = 'hxd'; hexgrid=.TRUE.
   case(13)  ! 2D Group 3m1 (cubic setting)
     grid = 'hxg'; hexgrid=.TRUE.
   case(14)  ! 2D Group 3m1 (hexagonal setting)
     grid = 'hxh'; hexgrid=.TRUE.
   case default   ! we should never get here
     call FatalError('Calckvectors','unknown isym value')
  end select

! now do the real work for standard sets of wave vectors
  select case(grid)

   case('srw')          ! systematic row incident beam orientations
     do i=imin,imax
      if (i.ne.0) then                                  ! the point (0,0) has already been taken care of
       call Add_knode(ktail,cell,i,0,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/)) 
      end if
     end do

   case('sqa')          ! from here on, all orientations are zone axis cases for all Laue groups
     do i=imin,imax
      do j=jmin,jmax
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/)) 
        end if
       end if
      end do
     end do

   case('sqb')
     do i=imin,imax
      jloop_sqb:  do j=jmin,jmax
       if ((j.eq.0).and.(i.lt.0)) cycle jloop_sqb       ! skip the points  (i<0,0)
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/)) 
        end if
       end if
      end do jloop_sqb   
     end do

   case('sqc')
     do j=0,jmax
      do i=j,imax
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2 .le. ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/)) 
        end if
       end if
      end do
     end do

   case('hxa')
     do j=0,npy
      do i=1-Kdelta(j,0),npx
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
    end do

   case('hxb')
     do j=0,npy
      do i=j,npx
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
    end do

   case('hxc')
     do j=0,npy
      do i=1-Kdelta(j,0)-j,npx-j
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
     end do

   case('hxd')
     do j=0,npy
      do i=0,npx-j
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
     end do

   case('hxe')
     do j=0,npy-1
      do i=1-Kdelta(j,0),npx-j
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
       end if
      end if
      end do
     end do

   case('hxf')
     do j=0,npy/2
      do i=j,npx-j
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
     end do

   case('hxg')
     do j=0,npy
      do i=j/2,min(2*j,npy)
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
     end do

   case('hxh')
     do j=0,npy
      do i=-j/2,min(j,npy-1)
       if (.not.((i.eq.0).and.(j.eq.0))) then   ! the point (0,0) has already been taken care of
        if (i**2+j**2.le.ijmax) then
         call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/),hexgrid) 
        end if
       end if
      end do
     end do

  case default  ! we should never get here
    call FatalError('Calckvectors:','unknown grid type value')
    
  end select  ! grid value

end if ! mapmode.eq.'Standard' or 'StandardConical'

! the next type of grid is the one used for the modified Lambert maps in the dynamical EBSD 
! programs; this requires some special care, since these mappings are a little trickier than 
! those of the standard mapmode.  While it is possible to use a plain Lambert projection as
! well, here we only allow for the RoscaLambert mode.

if (mapmode.eq.'RoscaLambert') then 
   delta =  1.D0 / dble(npx)
   if (usehex) then             ! hexagonal grid 
      hexgrid = .TRUE.
   else                         ! square grid
      hexgrid = .FALSE.
   end if

! allocate the head of the linked list
   allocate(khead,stat=istat)                   ! allocate new value
   if (istat.ne.0) call FatalError('Calckvectors',' unable to allocate khead pointer')
   ktail => khead                               ! tail points to new value
   nullify(ktail%next)                          ! nullify next in new value
   numk = 1                                     ! keep track of number of k-vectors so far
   ktail%hs = 1                                 ! this lies in the Northern Hemisphere
   ktail%i = 0                                  ! i-index of beam
   ktail%j = 0                                  ! j-index of beam
   kstar = (/ 0.0, 0.0, 1.0 /)                  ! we always use c* as the center of the RoscaLambert projection
   call NormVec(cell,kstar,'c')                 ! normalize incident direction
   kstar = kstar/cell%mLambda                        ! divide by wavelength
! and transform to reciprocal crystal space using the structure matrix
   ktail%k = matmul(transpose(cell%dsm),kstar)
   ktail%kn = 1.0/cell%mLambda

! MDG: as of 8/25/15, we no longer use the Laue groups to determine the set of independent wave vectors,
! but instead we use the complete point group symmetry, as it should be.  Upon reflection, using
! the Laue groups was equivalent to implicitly using Friedel's law, which makes all diffraction patterns
! centrosymmetric, and that is not correct for EBSD. So, the symbol isym now encodes the full point group,
! not the Laue group.  This will require a modification in each calling program as well.

! in addition, the modified Lambert projection will now require two hemispheres (NH and SH). We can handle this
! by means of an optional argument to the AddkVector routine; when the argument is present, the -k_z version
! of the direction is also added to the list.

! deal with each point group symmetry separately or in sets, depending on the value of isym
 select case (isym)
 
   case (1)  ! triclinic 1
        istart = -npx
        iend = npx
        jstart = -npy
        jend = npy
          do j=jstart,jend
            do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j,addSH = yes)
            end do
          end do

   case (2)  ! triclinic -1
        istart = -npx
        iend = npx
        jstart = -npy
        jend = npy
          do j=jstart,jend
            do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j)
            end do
          end do

  case (3)   !  monoclinic 2
        istart = 0
        iend = npx
        jstart = -npy
        jend = npy
          do j=jstart,jend
           do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j, addSH = yes)
           end do
          end do

  case (4)   !  monoclinic m
        istart = -npx
        iend = npx
        jstart = 0
        jend = npy
          do j=jstart,jend
           do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j, addSH = yes)
           end do
          end do

  case (5)  ! monoclinic 2/m, orthorhombic 222, mm2, tetragonal 4, -4
        istart = 0
        iend = npx
        jstart = 0
        jend = npy
          do j=jstart,jend
           do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j, addSH = yes)
           end do
          end do

  case (6)  ! orthorhombic mmm, tetragonal 4/m, 422, -4m2, cubic m-3, 432 (for now)
        istart = 0
        iend = npx
        jstart = 0
        jend = npy
          do j=jstart,jend
           do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j)
           end do
          end do

  case (7)  ! tetragonal 4mm
        istart = 0
        iend = npx
        jstart = 0
        jend = npx
          do i=istart,iend
           do j=jstart,i   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j, addSH = yes)
           end do
          end do

  case (8)  ! tetragonal -42m, cubic -43m (for now)
        istart = 0
        iend = npx
        jstart = -npx
        jend = npx
          do i=istart,iend
           do j=-i, i   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j)
           end do
          end do

  case (9)  ! tetragonal 4/mmm, cubic m-3m (for now)
        istart = 0
        iend = npx
        jstart = 0
        jend = npx
          do i=istart,iend
           do j=jstart,i   ! 
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j)
           end do
          end do

! cases 10 through 19 are all on a hexagonal grid...
! for now (08/31/15), we have not yet implemented the rhombohedral setting of the trigonal space groups;
! this case is truly a pain in the neck to implement...

  case (10)   ! hexagonal 3
        istart = 0
        iend = npx
        jstart = 0
        jend = npx
          do j=jstart,jend
            do i=istart,iend   ! 
                xy = (/ dble(i), dble(j) /) * delta
                if (InsideHexGrid(xy)) call AddkVector(ktail,cell,numk,xy,i,j,hexgrid, addSH = yes)
            end do
          end do

  case (11)   ! rhombohedral 3
        call FatalError('Calckvectors: ','rhombohedral setting has not been implemented yet, use hexagonal setting instead')
!       istart = 0
!       iend = npx
!       jstart = 0
!       jend = npx
!         do j=jstart,jend
!           do i=istart,iend   ! 
!               ii = 2*j-i
!               jj = j-2*i
!               xy = (/ dble(ii), dble(jj) /) * delta * LPs%isrt
!               if (InsideHexGrid(xy)) call AddkVector(ktail,cell,numk,xy,ii,jj,hexgrid, addSH = yes)
!           end do
!         end do

  case (12)   ! hexagonal -3, 321, -6; [not implemented: rhombohedral 32]
        if ((cell%SG%SYM_trigonal).and.(cell%SG%SYM_second)) then
          call FatalError('Calckvectors: ','rhombohedral setting has not been implemented yet, use hexagonal setting instead')
        else
          istart = 0
          iend = npx
          jstart = 0
          jend = npx
            do j=jstart,jend
              do i=istart,iend   ! 
                  xy = (/ dble(i), dble(j) /) * delta
                  if (InsideHexGrid(xy)) call AddkVector(ktail,cell,numk,xy,i,j,hexgrid)
              end do
            end do
        end if

  case (13)   ! [not implemented: rhombohedral -3], hexagonal 312  [ modified 7/31/18, MDG ]
        if ((cell%SG%SYM_trigonal).and.(cell%SG%SYM_second)) then
          call FatalError('Calckvectors: ','rhombohedral setting has not been implemented yet, use hexagonal setting instead')
        else
          istart = 0
          iend = npx
          jstart = 0
          jend = -npx
            do j=jstart,jend,-1
              do i=istart+j/2,iend   ! 
                xy = (/ dble(i),  dble(j) /) * delta 
                if (InsideHexGrid(xy)) then
                  ! call AddkVector(ktail,cell,numk,xy,-i,-j,hexgrid)
                  ! ktail%k(2) = -ktail%k(2)
                 call AddkVector(ktail,cell,numk,xy,i,j,hexgrid)
                end if
              end do
            end do
          istart = 0
          iend = npx
          jstart = 0
          jend = npx
            do i=istart,iend   ! 
              do j=jstart,i/2
                xy = (/ dble(i),  dble(j) /) * delta 
                if (InsideHexGrid(xy)) then 
                  call AddkVector(ktail,cell,numk,xy,i,j,hexgrid)
                 ! call AddkVector(ktail,cell,numk,xy,-i,-j,hexgrid)
                 !  ktail%k(2) = -ktail%k(2)
                end if
              end do
            end do
        end if

  case (14)   ! hexagonal 3m1, [not implemented: rhombohedral 3m]
        if ((cell%SG%SYM_trigonal).and.(cell%SG%SYM_second)) then
          call FatalError('Calckvectors: ','rhombohedral setting has not been implemented yet, use hexagonal setting instead')
        else
          istart = 1
          iend = npx
          jstart = 1
          jend = npx
            do j=jstart,jend
              do i=istart+(j-1)/2,2*j
                xy = (/ dble(i),  dble(j) /) * delta 
                if (InsideHexGrid(xy)) then 
                  call AddkVector(ktail,cell,numk,xy,i,j,hexgrid, addSH = yes)
                end if
              end do
            end do
        end if

  case (15)   ! hexagonal 31m, 6
          istart = 0
          iend = npx
          jstart = 1
          jend = npx
            do j=jstart,jend
              do i=istart+j,jend
                xy = (/ dble(i),  dble(j) /) * delta 
                if (InsideHexGrid(xy)) then 
                  call AddkVector(ktail,cell,numk,xy,i,j,hexgrid, addSH = yes)
                end if
              end do
            end do

  case (16)   ! hexagonal -3m1, 622, -6m2 [not implemented: rhombohedral -3m]
        if ((cell%SG%SYM_trigonal).and.(cell%SG%SYM_second)) then
          call FatalError('Calckvectors: ','rhombohedral setting has not been implemented yet, use hexagonal setting instead')
        else
          istart = 0
          iend = npx
          jstart = 0
          jend = npx
          eps = 1.0D-4
            do j=jstart,jend
              do i=istart,iend
                  xy = (/ dble(i), dble(j) /) * delta
                  xx = dble(i)-dble(j)/2.D0
                  yy = dble(j)*LPs%srt
                  check = .TRUE.
                  if (xx.lt.0.D0) then
                    check = .FALSE.
                  else
                    if (xx.ge.0.D0) then
                      yy = datan2(yy,xx)
                      if (yy .lt. (LPs%Pi/6.D0-eps)) check = .FALSE.
                    end if
                  end if
                  if (InsideHexGrid(xy).and.(check)) call AddkVector(ktail,cell,numk,xy,i,j,hexgrid)
              end do
            end do
        end if

  case (17)   ! hexagonal -31m, 6/m, -62m
        istart = 0
        iend = npx
        jstart = 0
        jend = npx
        eps = 1.0D-4
          do j=jstart,jend
            do i=istart,iend
                xy = (/ dble(i), dble(j) /) * delta
                xx = dble(i)-dble(j)/2.D0
                yy = dble(j)*LPs%srt
                check = .TRUE.
                if (xx.lt.0.D0) then
                   check = .FALSE.
                else
                   if (xx.ge.0.D0) then
                     yy = datan2(yy,xx)
                     if (yy.gt.(cPi/3.D0+eps)) check = .FALSE.
                   end if
                end if
                if (InsideHexGrid(xy).and.(check)) call AddkVector(ktail,cell,numk,xy,i,j,hexgrid)
            end do
          end do

  case (18)   ! hexagonal 6mm
        istart = 0
        iend = npx
        jstart = 0
        jend = npx
        eps = 1.0D-4
          do j=jstart,jend
            do i=istart,iend
                xy = (/ dble(i), dble(j) /) * delta
                xx = dble(i)-dble(j)/2.D0
                yy = dble(j)*LPs%srt
                check = .TRUE.
                if (xx.lt.0.D0) then
                   check = .FALSE.
                else
                   if (xx.ge.0.D0) then
                     yy = datan2(yy,xx)
                     if (yy.gt.(cPi/6.D0+eps)) check = .FALSE.
                   end if
                end if
                if (InsideHexGrid(xy).and.(check)) call AddkVector(ktail,cell,numk,xy,i,j,hexgrid, addSH = yes)
            end do
          end do

  case (19)   ! hexagonal 6/mmm
        istart = 0
        iend = npx
        jstart = 0
        jend = npx
        eps = 1.0D-4
          do j=jstart,jend
            do i=istart,iend
                xy = (/ dble(i), dble(j) /) * delta
                xx = dble(i)-dble(j)/2.D0
                yy = dble(j)*LPs%srt

                check = .TRUE.
                if (xx.lt.0.D0) then
                   check = .FALSE.
                else
                   if (xx.ge.0.D0) then
                     yy = datan2(yy, xx)
                     if (yy.gt.(LPs%Pi/6.D0+eps)) check = .FALSE.
                   end if

                end if
                if (InsideHexGrid(xy).and.(check)) call AddkVector(ktail,cell,numk,xy,i,j,hexgrid)

            end do
          end do

 end select

end if

end subroutine Calckvectors


!--------------------------------------------------------------------------
!
! FUNCTION: InsideHexGrid
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief determines whether or not a point lies inside the standard hexagonal Lambert grid
!
!> @details The hexagon has unit edge length, with one vertex at (1,0). Take the 
!> absolute values of x and y and check if this point lies inside a box with edge
!> lengths (1, sqrt(3)/2).  If not, exit; else, check on which side of the line
!> |x|+|y|/sqrt(3)=1 the point lies.
!
!> @param xy  coordinate 
!
!> @date   08/31/15 MDG 1.0 original 
!--------------------------------------------------------------------------
recursive function InsideHexGrid(xy) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: InsideHexGrid

use Lambert

IMPLICIT NONE

real(kind=dbl),INTENT(IN)       :: xy(2)
logical                         :: res
real(kind=dbl)                  :: ax, ay

! assume it is a good point
res = .TRUE.

! first of all, take the absolute values and see if the transformed point lies inside the 
! rectangular box with edge lengths (1,sqrt(3)/2)
ax = abs(xy(1)-0.5D0*xy(2))
ay = abs(xy(2)*LPs%srt)

if ((ax.gt.1.D0).or.(ay.gt.LPs%srt)) res = .FALSE. 
! then check for the inclined edge
if (res) then
  if (ax+ay*LPs%isrt .gt. 1.D0) res = .FALSE. 
end if

end function InsideHexGrid




!--------------------------------------------------------------------------
!
! SUBROUTINE: CalckvectorsSymmetry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors, using the whole pattern symmetry
!
!> @details This is a new version to test whether or not we can use the whole pattern
!> symmetry to determine the relevant list of incident wave vectors; this should be a 
!> general routine, so that we do not need to consider each symmetry case separately.
!> This will require a floating point version of the Apply2DPGSymmetry routine in symmetry.f90.
!
!> @param khead of head of linked list
!> @param cell unit cell pointer
!> @param TDPG 2D point group structure
!> @param k central wave vector
!> @param ga reciprocal lattice vector normal to k
!> @param ktmax maximum length of tangential component
!> @param npx number of vectors along x 
!> @param npy number of vectors along y 
!> @param numk total number of wave vectors in list
!> @param isym Laue group number
!> @param ijmax used for conical illumination
!> @param klaue fractional Laue center coordinates
!> @param debug when present, an output file with the kselected array is produced
!
!> @date   10/03/13 MDG 1.0 original
!> @date   06/09/14 MDG 2.0 added khead and cell arguments
!--------------------------------------------------------------------------
recursive subroutine CalckvectorsSymmetry(khead,cell,TDPG,k,ga,ktmax,npx,npy,numk,isym,ijmax,klaue,debug)
!DEC$ ATTRIBUTES DLLEXPORT :: CalckvectorsSymmetry

use io
use error
use constants
use diffraction
use crystal
use Lambert

IMPLICIT NONE

type(kvectorlist),pointer               :: khead
type(unitcell),pointer                  :: cell
type(symdata2D),INTENT(INOUT)           :: TDPG
real(kind=dbl),INTENT(IN)               :: k(3)         !< initial wave vector
real(kind=dbl),INTENT(IN)               :: ga(3)        !< "horizontal" reciprocal lattice vector
real(kind=dbl),INTENT(IN)               :: ktmax        !< maximum length of tangential wave vector
integer(kind=irg),INTENT(IN)            :: npx          !< number of kvectors along x
integer(kind=irg),INTENT(IN)            :: npy          !< number of kvectors along y
integer(kind=irg),INTENT(OUT)           :: numk         !< total number of kvectors in linked list
integer(kind=irg),INTENT(IN)            :: isym         !< Laue symmetry group number 
integer(kind=irg),INTENT(INOUT)         :: ijmax        !< max parameter used for Conical and StandardConical modes
real(kind=sgl),INTENT(IN)               :: klaue(2)     !< fractional Laue center coordinates
logical,INTENT(IN),OPTIONAL             :: debug

integer(kind=irg),allocatable           :: kselected(:,:)       !< keeps track of which k-vectors have already been considered

integer(kind=irg)                       :: istat,i,j, iequiv(2,12), nequiv, jj, nx, ny
real(kind=dbl)                          :: glen, gan(3), gperp(3), kstar(3), delta, Lauexy(2)
logical                                 :: hexgrid = .FALSE.
real(kind=sgl)                          :: kt(3),kr(3)
real(kind=sgl)                          :: ktlen
type(kvectorlist),pointer               :: ktail

nx = 2*npx
ny = 2*npy
allocate(kselected(-nx:nx,-ny:ny))

! initialize the kselected array to 0
kselected = 0

! compute geometrical factors 
 glen = CalcLength(cell,ga,'r')                         ! length of ga
 Lauexy = glen * klaue                                  ! scaled Laue center coordinates
 gan = ga/glen                                          ! normalized ga
 delta = 2.0*ktmax*glen/(2.0*float(npx)+1.0)            ! grid step size in nm-1 
 call TransSpace(cell,k,kstar,'d','r')                  ! transform incident direction to reciprocal space
 call CalcCross(cell,ga,kstar,gperp,'r','r',0)          ! compute g_perp = ga x k
 call NormVec(cell,gperp,'r')                           ! normalize g_perp
 call NormVec(cell,kstar,'r')                           ! normalize reciprocal beam vector

! allocate the head and tail of the linked list
 allocate(khead,stat=istat)                             ! allocate new value
 if (istat.ne.0) call FatalError('Calckvectors','unable to allocate khead pointer')
 ktail => khead                                         ! tail points to new value
 nullify(ktail%next)                                    ! nullify next in new value
 numk = 1                                               ! keep track of number of k-vectors so far
 ktail%i = 0                                            ! i-index of beam
 ktail%j = 0                                            ! j-index of beam
 
! use the Laue center coordinates to define the tangential component of the incident wave vector
 kt = - Lauexy(1)*gan - Lauexy(2)*gperp                 ! tangential component of k
 ktail%kt = kt                                          ! store tangential component of k
 ktlen = CalcLength(cell,kt,'r')**2                     ! squared length of tangential component

 kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar           ! complete wave vector
 ktail%k = kr                                           ! store in pointer list
 ktail%kn = CalcDot(cell,ktail%k,kstar,'r')             ! normal component of k
  
 kselected(0,0) = 2

 if (maxval(abs(klaue)).eq.0.0) then                    ! zone axis orientation, so we should use symmetry
! we scan over the entire range of potential beam directions, defined by npx and npy along with
! the conical truncation parameter ijmax; for each point we check whether or not it has been considered
! before; it it has, we move on, if it hasn't, then we add this point to the linked list in the usual way.
! we do this by computing the equivalent (i,j) using the Whole Pattern symmetry.
     do i=-nx,nx
      do j=-ny,ny
        if (kselected(i,j).eq.0) then
          if ((i*i+j*j).le.ijmax) then
! first of all, add the present point to the linked list
           call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,(/ 0.0,0.0/)) 
!write (*,*) numk,ktail%k
! then compute the equivalent points and flag all of them in kselected
!          call Apply2DLaueSymmetry(i,i,isym,iequiv,nequiv)
           call Apply2DPGSymmetry(TDPG,i,j,isym,iequiv,nequiv)
           kselected(iequiv(1,1),iequiv(2,1)) = 2
           if (nequiv.gt.1) then 
            do jj=2,nequiv
             kselected(iequiv(1,jj),iequiv(2,jj)) = 1
            end do
           end if
        end if
       end if
      end do
     end do
  else                                                  ! not a zone axis, so no symmmetry
     do i=-nx,nx
      do j=-ny,ny
        if (kselected(i,j).eq.0) then
          if ((i*i+j*j).le.ijmax) then
! first of all, add the present point to the linked list
            call Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,sngl(Lauexy))
            kselected(i,j) = 2
        end if
       end if
      end do
     end do
  end if

! for debugging purposes, we can write the kselected array to a file.
!if (present(debug)) then
! open(unit=20,file='kselected.data',status='unknown',form='unformatted')
! write (20) 2*nx+1,2*ny+1
! write (20) kselected
! close(unit=20,status='keep')
!nd if

! and clean up the kselected array
deallocate(kselected)

end subroutine CalckvectorsSymmetry


!--------------------------------------------------------------------------
!
! SUBROUTINE: Add_knode
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add one entry to the linked wave vector list (standard mode)
!
!> @param ktail current entry in linked list
!> @param cell unit cell pointer 
!> @param i x image coordinate 
!> @param j y image coordinate 
!> @param numk total number of wave vectors in list
!> @param delta scale parameter
!> @param gan normalized g-vector
!> @param gperp normalized perpendicular g-vector
!> @param kstar reciprocal components of wave vector
!> @param klaue fractional Laue center coordinates
!> @param hexgrid (optional) indicates hexagonal sampling if present
!
!> @todo implement Laue center coordinates for hexagonal grid
!
!> @date   04/29/13 MDG 1.0 original
!> @date   06/09/14 MDG 2.0 added ktail and cell as arguments
!--------------------------------------------------------------------------
recursive subroutine Add_knode(ktail,cell,i,j,numk,delta,gan,gperp,kstar,klaue,hexgrid)
!DEC$ ATTRIBUTES DLLEXPORT :: Add_knode

use error
use crystal
use constants
use diffraction

IMPLICIT NONE

type(kvectorlist),pointer              :: ktail
type(unitcell),pointer                  :: cell
integer(kind=irg),INTENT(IN)            :: i
integer(kind=irg),INTENT(IN)            :: j
integer(kind=irg),INTENT(INOUT) :: numk
real(kind=dbl),INTENT(IN)               :: delta
real(kind=dbl),INTENT(IN)               :: gan(3)
real(kind=dbl),INTENT(IN)               :: gperp(3)
real(kind=dbl),INTENT(IN)               :: kstar(3)
real(kind=sgl),INTENT(IN)               :: klaue(2)
logical,INTENT(IN),OPTIONAL             :: hexgrid

real(kind=sgl)                          :: kt(3),kr(3)
real(kind=sgl)                          :: ktlen
integer(kind=irg)                       :: istat


allocate(ktail%next,stat=istat)                 ! allocate new value
if (istat.ne.0) call FatalError('Add_knode:',' unable to allocate pointer')
ktail => ktail%next                             ! tail points to new value
nullify(ktail%next)                             ! nullify next in new value
numk = numk + 1                                 ! keep track of number of k-vectors so far
ktail%i = i                                     ! i-index of beam
ktail%j = j                                     ! j-index of beam

! is it a square or hexagonal grid ?
if (present(hexgrid)) then
  kt = -(float(i)-float(j)*0.5)*delta*gan - float(j)*delta*gperp*0.5*sqrt(3.0)  ! tangential component of k
else
  kt = -(klaue(1)+float(i)*delta)*gan - (klaue(2)+float(j)*delta)*gperp  ! tangential component of k
end if

ktail%kt = kt                                   ! store tangential component of k
ktlen = CalcLength(cell,kt,'r')**2                      ! squared length of tangential component

kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar       ! complete wave vector
ktail%k = kr                                    ! store in pointer list
ktail%kn = CalcDot(cell,ktail%k,kstar,'r')      ! normal component of k

end subroutine Add_knode


!--------------------------------------------------------------------------
!
! FUNCTION: GetSextant
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief determines in which sextant a point (x,y) is located (used for RoscaLambert mapmode)
!
!> @param x x coordinate 
!> @param y y coordinate 
!
!> @date   11/21/12 MDG 1.0 original 
!> @date   04/29/13 MDG 1.1 modified for kvectors module
!--------------------------------------------------------------------------
recursive function GetSextant(x,y) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSextant

IMPLICIT NONE

real(kind=dbl),INTENT(IN):: x, y 

real(kind=dbl),parameter        :: srt = 1.732050808   ! sqrt(3.D0)
integer(kind=irg)               :: res
real(kind=dbl)                  :: xx

xx = dabs(x*srt)        ! |x| sqrt(3)

if (y.ge.0) then
  if (y.ge.xx) then
        res = 0
  else
        if (x.gt.0.D0) then
          res = 1
        else
          res = 5
        end if
  end if
else
  if (dabs(y).ge.xx) then
        res = 3
  else
        if (x.gt.0.D0) then
          res = 2
        else
          res = 4
        end if
  end if
end if

end function GetSextant


!--------------------------------------------------------------------------
!
! SUBROUTINE: Addkvector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a k-vector for square or hexagonal grid sampling mode (used for RoscaLambert mapmode)
!
!> @todo replace the coordinate transformation with one defined in the Lambert module
!
!> @param ktail current entry in linked list
!> @param cell unit cell pointer
!> @param numk total number of wave vectors in list
!> @param xy input coordinates (normalized on square or hexagonal grid)
!> @param i point index
!> @param j point index
!> @param usehex (optional) indicates hexagonal sampling if present
!> @param addSH (optional) indicates if Southern Hemisphere sampling is needed
!
!> @date   11/21/12 MDG 1.0 original 
!> @date   04/29/13 MDG 1.1 modified for kvectors module
!> @date   06/09/14 MDG 2.0 added ktail as argument
!> @date   08/25/15 MDG 2.1 added addSH as optional argument
!> @date   08/27/15 MDG 2.2 added flip for special case of rhombohedral sampling
!> @date   08/28/15 MDG 2.3 mappings replaced with calls to Lambert module (significant simplification of code)
!> @date   08/31/15 MDG 2.4 replaced integer coordinates by actual scaled coordinates
!--------------------------------------------------------------------------
recursive subroutine AddkVector(ktail,cell,numk,xy,i,j,usehex,addSH)
!DEC$ ATTRIBUTES DLLEXPORT :: AddkVector

use io
use typedefs
use constants
use error
use diffraction
use crystal
use Lambert
use crystal

IMPLICIT NONE

type(kvectorlist),pointer               :: ktail
type(unitcell),pointer                  :: cell
integer(kind=irg),INTENT(INOUT)         :: numk
real(kind=dbl),INTENT(IN)               :: xy(2)
integer(kind=irg),INTENT(IN)            :: i
integer(kind=irg),INTENT(IN)            :: j
logical,INTENT(IN),OPTIONAL             :: usehex
logical,INTENT(IN),OPTIONAL             :: addSH

integer(kind=irg)                       :: istat, ks, ii, ierr
real(kind=dbl)                          :: kstar(3)
logical                                 :: hex

! project the coordinate up to the sphere, to get a unit 3D vector kstar in cartesian space
if (present(usehex)) then
  kstar = LambertHexToSphere(xy,ierr)
  hex = .TRUE.
else
  kstar = LambertSquareToSphere(xy, ierr)
  hex = .FALSE.
end if

! add this vector to the linked list
     allocate(ktail%next,stat=istat)                    ! allocate new value
     if (istat.ne.0) call FatalError('Addkvector:',' unable to allocate ktail pointer')
     ktail => ktail%next                                ! tail points to new value
     nullify(ktail%next)                                ! nullify next in new value
     numk = numk + 1                                    ! keep track of number of k-vectors so far
     ktail%hs = 1                                       ! which hemisphere (Northern = 1, Southern = -1)
     ! if (iv) then                                      ! transform the hex coordinates to square-array coordinates
     !   ktail%i = i - j/2+mod(j,2)/2                     ! i-index of beam
     !   ktail%j = j                                      ! j-index of beam
     ! else                                               ! leave the square coordinates unchanged
       ktail%i = i                                      ! i-index of beam
       ktail%j = j                                     ! j-index of beam
     ! end if 
     call NormVec(cell,kstar,'c')                       ! normalize incident direction in cartesian space
     kstar = kstar/cell%mLambda                         ! divide by wavelength
! and transform to reciprocal crystal space using the direct structure matrix
     call TransSpace(cell, kstar, ktail%k, 'c', 'r')
     ktail%kn = 1.0/cell%mLambda

! do we also need to add a Southern Hemisphere vector ?
     if (present(addSH)) then
       if (addSH.eqv..TRUE.) then
         allocate(ktail%next,stat=istat)                    ! allocate new value
         if (istat.ne.0) call FatalError('Addkvector:',' unable to allocate ktail pointer')
         ktail => ktail%next                                ! tail points to new value
         nullify(ktail%next)                                ! nullify next in new value
         numk = numk + 1                                    ! keep track of number of k-vectors so far
         ktail%hs = -1                                      ! which hemisphere (Northern = 1, Southern = -1)
         ! if (iv) then                                      ! transform the hex coordinates to square-array coordinates
         !   ktail%i = i - j/2+mod(j,2)/2                     ! i-index of beam
         !   ktail%j = j                                      ! j-index of beam
         ! else                                               ! leave the square coordinates unchanged
           ktail%i = i                                      ! i-index of beam
           ktail%j = j                                     ! j-index of beam
         ! end if 
! get the Southern hemisphere version of kstar
         kstar(3) = -kstar(3)
! and transform to reciprocal crystal space using the direct structure matrix
         call TransSpace(cell, kstar, ktail%k, 'c', 'r')
         ktail%kn = 1.0/cell%mLambda
       end if
     end if

end subroutine AddkVector

!--------------------------------------------------------------------------
!
! SUBROUTINE: Delete_kvectorlist
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief delete the entire linked list
!
!> @date   04/29/13 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Delete_kvectorlist(khead)
!DEC$ ATTRIBUTES DLLEXPORT :: Delete_kvectorlist

IMPLICIT NONE

type(kvectorlist),pointer        :: khead
type(kvectorlist),pointer        :: ktmp, ktail

! deallocate the entire linked list before returning, to prevent memory leaks
ktail => khead
ktmp => ktail % next
do 
  deallocate(ktail)
  if (.not. associated(ktmp)) EXIT
  ktail => ktmp
  ktmp => ktail % next
end do

end subroutine Delete_kvectorlist

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalckvectorsECP
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors for conical incidence in ECP
!
!
!> @param khead head of linked list
!> @param cell unit cell pointer
!> @param k central wave vector
!> @param ktmax maximum length of tangential component
!> @param npx number of vectors along x
!> @param npy number of vectors along y
!> @param numk total number of wave vectors in list
!> @param ijmax used for conical illumination
!
!> @date   08/25/14 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalckvectorsECP(khead,cell,rotmat,thetac,npx,npy,numk,FN)
!DEC$ ATTRIBUTES DLLEXPORT :: CalckvectorsECP

use io
use local
use error
use constants
use diffraction
use crystal
use Lambert

IMPLICIT NONE

type(kvectorlist),pointer               :: khead,ktail
type(unitcell),pointer                  :: cell
real(kind=sgl),INTENT(IN)               :: rotmat(3,3)         !< initial wave vector
real(kind=sgl),INTENT(IN)               :: thetac        !< half angle of cone of incident beam directions in degrees
integer(kind=irg),INTENT(IN)            :: npx          !< number of kvectors along x
integer(kind=irg),INTENT(IN)            :: npy          !< number of kvectors along y
real(kind=sgl),INTENT(IN)               :: FN(3)        ! foil normal in reciprocal frame
integer(kind=irg),INTENT(OUT)           :: numk
real(kind=dbl),parameter                :: DtoR = 0.01745329251D0
real(kind=sgl)                          :: delta,thetacr,ktmax
integer(kind=irg)                       :: i,j,imin,imax,jmin,jmax,ijmax,istat
real(kind=sgl)                          :: kk(3),krec(3)
real(kind=sgl)                          :: k(3),kcart(3),kkk

if (associated(khead)) then                    ! deallocate the entire linked list
    call Delete_kvectorlist(khead)
end if

k = (/0.0,0.0,1.0/)
k = k/cell%mLambda
thetacr = DtoR*thetac
kk = k

ktmax = tan(thetacr)*sqrt(sum(kk**2))
delta = 2.0*ktmax/(2.0*float(npx)+1.0)
imin = -npx
imax = npx
jmin = -npy
jmax = npy

ijmax = npx**2

! allocate the head and tail of the linked list
allocate(khead,stat=istat)                              ! allocate new value
if (istat.ne.0) call FatalError('Calckvectors','unable to allocate khead pointer')

ktail => khead                                          ! tail points to new value
nullify(ktail%next)                                     ! nullify next in new value
numk = 0
ktail%i = 0                                             ! i-index of beam
ktail%j = 0                                             ! j-index of beam
ktail%kt = (/0.0,0.0,0.0/)                              ! no tangential component for central beam direction
ktail%k = matmul(rotmat,kk)
k = ktail%k
call TransSpace(cell,k,krec,'c','r')
ktail%k = krec

kkk = CalcDot(cell,sngl(ktail%k),FN,'r')                ! normal component
ktail%kn = kkk

do i = imin,imax
    do j = jmin,jmax
        !if ((i**2 + j**2) .le. ijmax) then
            allocate(ktail%next)
            ktail => ktail%next
            nullify(ktail%next)
            numk = numk + 1
            ktail%i = i
            ktail%j = j
            ktail%kt = (/delta*i,delta*j,0.0/)
            ktail%k = ktail%kt + kk
            call NormVec(cell,ktail%k,'c')
            ktail%k = ktail%k/cell%mLambda
            ktail%kt = matmul(rotmat,ktail%kt)
            ktail%k = matmul(rotmat,ktail%k)
            k = ktail%k
            call TransSpace(cell,k,krec,'c','r')
            ktail%k = krec
            !call NormVec(cell,ktail%k,'c')
            k = ktail%kt
            call TransSpace(cell,k,krec,'c','r')
            ktail%kt = krec
            kkk = CalcDot(cell,sngl(ktail%k),FN,'r')
            ktail%kn = kkk

        !end if
    end do
end do


end subroutine CalckvectorsECP

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalckvectorsGPU
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors for approximate master pattern
!> calculation
!
!> @detail this subroutine calculates the list of k vectors in a small patch of
!> size npix*npix in lambert space. This central beam is used to calculate the list
!> of g vectors for the whole patch, and the k vectors are used to calculate the 
!> diagonal components i.e. the sg values.
!
!> @param khead head of linked list
!> @param cell unit cell pointer
!> @param npx half width of the master pattern
!> @param npix gives size of the small patch for which k vectors are calculated
!
!
!> @date   04/20/15  SS 1.0 original
!> @date   08/31/15 MDG 1.1 changes to AddkVector; THIS ROUTINE WILL NEED TO BE MODIFIED !!!!!!
!--------------------------------------------------------------------------
recursive subroutine CalckvectorsGPU(khead,cell,npx,npix,centralpix,numk,usehex)
!DEC$ ATTRIBUTES DLLEXPORT :: CalckvectorsGPU

use io
use local
use error
use constants
use diffraction
use crystal
use Lambert

IMPLICIT NONE

type(kvectorlist),pointer               :: khead,ktail
type(unitcell),pointer                  :: cell
integer(kind=irg),INTENT(IN)            :: npx ! 2*npx+1 is size of master pattern
integer(kind=irg),INTENT(IN)            :: npix ! the small patch will be 4*npix*npix
integer(kind=irg),INTENT(IN)            :: centralpix(2)
integer(kind=irg),INTENT(OUT)           :: numk
logical,INTENT(IN),OPTIONAL             :: usehex

logical                                 :: verbose,switchmirror,hex
integer(kind=irg)                       :: i,j,isym,pgnum,ks,istat
integer(kind=irg)                       :: istart,iend,jstart,jend
integer(kind=irg),parameter             :: LaueTest(11) = (/ 149, 151, 153, 156, 158, 160, 161, 164, 165, 166, 167 /)  ! space groups with 2 or mirror at 30 degrees
real(kind=dbl)                          :: delta,x,y,q,kstar(3),XX,YY,xp,yp,rr, xy(2)


allocate(khead,stat=istat)                   ! allocate new value
if (istat.ne.0) call FatalError('Calckvectors',' unable to allocate khead pointer')
ktail => khead                       ! tail points to new value
nullify(ktail%next)
numk = 1
! we deal with the symmetry of the master pattern in the main subroutine
istart = centralpix(1)-npix
iend = centralpix(1)+npix-1
jstart = centralpix(2)-npix
jend = centralpix(2)+npix-1

if (present(usehex)) then
    if (usehex) then
! hexagonal grid step size
        delta =  1.D0/dble(npx)
        ktail%i = centralpix(1)
        ktail%j = centralpix(2)
        x = (i - j*0.5)*delta
        y = j*delta*LPs%srt
        rr = x*x+y*y
        hex = .TRUE.
        ks = GetSextant(x,y)
        select case (ks)
        case (0,3)
            XX = LPs%preb*y*dcos(x*LPs%prec/y)
            YY = LPs%preb*y*dsin(x*LPs%prec/y)
        case (1,4)
            xp = y+LPs%rtt*x
            yp = y*LPs%pred/xp
            XX = LPs%prea*xp*dsin(yp)
            YY = LPs%prea*xp*dcos(yp)
        case (2,5)
            xp = y-LPs%rtt*x
            yp = y*LPs%pred/xp
            XX = LPs%prea*xp*dsin(yp)
            YY = -LPs%prea*xp*dcos(yp)
        end select

        q = XX**2+YY**2
        kstar = (/ 0.5D0*XX*dsqrt(4.D0-q), 0.5D0*YY*dsqrt(4.D0-q),1.D0-0.5D0*q /)
        ktail%i = centralpix(1) - centralpix(2)/2+mod(centralpix(2),2)/2                     ! i-index of beam
        ktail%j = centralpix(2)   ! j-index of beam
        call NormVec(cell,kstar,'c')
        kstar = kstar/cell%mLambda                              ! divide by wavelength
! and transform to reciprocal crystal space using the direct structure matrix
        ktail%k = matmul(transpose(cell%dsm),kstar)
        ktail%kn = 1.0/cell%mLambda
    else
        delta = 1.D0/dble(npx)
        ktail%i = centralpix(1)
        ktail%j = centralpix(2)
        x = centralpix(1)*delta
        y = centralpix(2)*delta
        rr = x*x+y*y
        hex = .FALSE.
        if (maxval(abs((/x,y/))).eq.0.0) then
            kstar = (/0.0,0.0,1.0/)
        else
            if (dabs(x).le.dabs(y)) then
                q = 2.D0*y*LPs%iPi*dsqrt(cPi-y*y)
                kstar = (/ q*dsin(x*LPs%Pi*0.25D0/y), q*dcos(x*LPs%Pi*0.25D0/y), 1.D0-2.D0*y*y*LPs%iPi /)
            else
                q = 2.D0*x*LPs%iPi*dsqrt(cPi-x*x)
                kstar = (/ q*dcos(y*LPs%Pi*0.25D0/x), q*dsin(y*LPs%Pi*0.25D0/x), 1.D0-2.D0*x*x*LPs%iPi /)
            end if
        end if

        ktail%i = centralpix(1)
        ktail%j = centralpix(2)
        call NormVec(cell,kstar,'c')
        kstar = kstar/cell%mLambda                              ! divide by wavelength
! and transform to reciprocal crystal space using the direct structure matrix
        ktail%k = matmul(transpose(cell%dsm),kstar)
        ktail%kn = 1.0/cell%mLambda
    end if
else
    delta = LPs%ap/dble(npx)
    ktail%i = centralpix(1)
    ktail%j = centralpix(2)
    x = centralpix(1)*delta
    y = centralpix(2)*delta
    rr = x*x+y*y
    hex = .FALSE.
    if (maxval(abs((/x,y/))).eq.0.0) then
        kstar = (/0.0,0.0,1.0/)
    else

        if (dabs(x).le.dabs(y)) then
            q = 2.D0*y*LPs%iPi*dsqrt(cPi-y*y)
            kstar = (/ q*dsin(x*LPs%Pi*0.25D0/y), q*dcos(x*LPs%Pi*0.25D0/y), 1.D0-2.D0*y*y*LPs%iPi /)
        else
            q = 2.D0*x*LPs%iPi*dsqrt(cPi-x*x)
            kstar = (/ q*dcos(y*LPs%Pi*0.25D0/x), q*dsin(y*LPs%Pi*0.25D0/x), 1.D0-2.D0*x*x*LPs%iPi /)
        end if
    end if
    ktail%i = centralpix(1)
    ktail%j = centralpix(2)
    call NormVec(cell,kstar,'c')
    kstar = kstar/cell%mLambda                              ! divide by wavelength
! and transform to reciprocal crystal space using the direct structure matrix
    ktail%k = matmul(transpose(cell%dsm),kstar)
    ktail%kn = 1.0/cell%mLambda
end if

do j=jstart,jend
    do i=istart,iend
        if (.not.((i .eq. centralpix(1)) .and. (j .eq. centralpix(2)))) then ! central pixel already taken care of
            if (present(usehex)) then
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j,usehex)
            else
                xy = (/ dble(i), dble(j) /) * delta
                call AddkVector(ktail,cell,numk,xy,i,j)
            end if
        end if
    end do
end do

end subroutine CalckvectorsGPU

end module kvectors
