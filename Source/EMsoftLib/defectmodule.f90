! ###################################################################
! Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:defectmodule.f90
!--------------------------------------------------------------------------
!
! MODULE: defectmodule
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Provides a routine to compute the displacement vector for an array of defects.
! 
!> @date 04/29/11 MDG 1.0 original
!> @date 06/04/13 MDG 2.0 rewrite + quaternions instead of rotations
!> @date 11/13/13 MDG 2.1 fixed error with coordinate transformations (after very long bug search!)
!> @date 11/17/15 MDG 3.0 start of complete rewrite; this mod will now include a routine to read all defect info from a json file
!> @date 11/23/15 MDG 3.1 inserted all defect mods into this file instead of separate files
!> @date 12/08/15 MDG 3.2 added artificial distortion to inclusion field to mimic ellipsoid shape (needs Eshelby for correct field)
!> @date 12/11/15 MDG 3.3 gave up on previous item and implemented full isotropic Eshelby ellipsoidal inclusion
!--------------------------------------------------------------------------
module defectmodule

use local
use quaternions
use typedefs


contains


!--------------------------------------------------------------------------
!
! SUBROUTINE: InitializeDefects
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read defect information, and generate all data that can be precomputed for each defect
!
!> @date  11/22/15 MDG 1.0 original
!> @date  11/24/15 MDG 1.1 added Ydislocations, stacking faults, inclusions and voids
!--------------------------------------------------------------------------
recursive subroutine InitializeDefects(cell,defects,jsonname,npix,npiy,L,gf,error_cnt,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: InitializeDefects

use local
use typedefs
use io
use JSONsupport

IMPLICIT NONE

type(unitcell)                          :: cell
type(defecttype),INTENT(INOUT)          :: defects
!f2py intent(in,out) ::  defects
character(fnlen),INTENT(IN)             :: jsonname
integer(kind=irg),INTENT(IN)            :: npix
integer(kind=irg),INTENT(IN)            :: npiy
real(kind=sgl),INTENT(IN)               :: L
real(kind=sgl),INTENT(IN)               :: gf(3)
integer(kind=irg),INTENT(INOUT)         :: error_cnt
!f2py intent(in,out) ::  error_cnt
logical,INTENT(IN),OPTIONAL             :: verbose

integer(kind=irg)                       :: v, i, io_int(1)

error_cnt = 0

v = 0
if (PRESENT(verbose)) then
  if (verbose) then
    v = 1
  end if
end if

defects%numdisl = 0
defects%numYdisl = 0
defects%numsf = 0
defects%numvoids = 0
defects%numinc = 0
defects%numEinc = 0

! first of all, we need to read all the defect data from the jsonname file, including the foil data
! note that the JSON file should have the .jsonc extension, isince it may have comments; those lines
! will be removed first
call JSONreadDefectFile(cell, jsonname, defects, error_cnt, verbose)

if (v.eq.1) then
  call Message('The following defects were initialized : ')
  io_int(1) = defects%numdisl
  call WriteValue('  Number of dislocations       : ',io_int,1)
  io_int(1) = defects%numYdisl
  call WriteValue('  Number of Yoffe dislocations : ',io_int,1)
  io_int(1) = defects%numsf
  call WriteValue('  Number of stacking faults    : ',io_int,1)
  io_int(1) = defects%numinc
  call WriteValue('  Number of inclusions         : ',io_int,1)
  io_int(1) = defects%numEinc
  call WriteValue('  Number of Eshelby inclusions : ',io_int,1)
  io_int(1) = defects%numvoids
  call WriteValue('  Number of voids              : ',io_int,1)
end if

! once we have this data, we need to initialize all other defect related parameters, including
! things like displacement field parameters etc...

! we begin with the foil itself
call init_foil_data(cell,defects,npix,npiy,L,v)
if (v.eq.1) call Message('========> completed foil generation')

! then we add the defects, starting with all the regular dislocations, if any
if (defects%numdisl.ne.0) then
  call init_dislocation_data(cell,defects,npix,npiy,gf,L,v)
  call Message('========> completed dislocation generation')
end if

! then Ydislocations
if (defects%numYdisl.ne.0) then
  call init_YSH_dislocation_data(cell,defects,npix,npiy,gf,L,v)
  if (v.eq.1) call Message('========> completed Ydislocation generation')
end if

! stacking faults
if (defects%numsf.ne.0) then
  call init_stacking_fault_data(cell,defects,L,npix,npiy,gf,v)
  if (v.eq.1) call Message('========> completed SF generation')
end if

! inclusions
if (defects%numinc.ne.0) then
  call init_inclusion_data(defects,L,npix,npiy,v)
  if (v.eq.1) call Message('========> completed inclusion generation')
end if

! Eshelby inclusions
if (defects%numEinc.ne.0) then 
  do i=1,defects%numEinc 
    call InitializeEshelbyInclusion(cell,defects,i,v,L,npix,npiy) 
  end do
  if (v.eq.1) call Message('========> completed E-inclusion generation')
end if

! voids
if (defects%numvoids.ne.0) then
  call init_void_data(defects,L,npix,npiy,v)
  if (v.eq.1) call Message('========> completed void generation')
end if


end subroutine InitializeDefects

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! here are the init_defect_data subroutines
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE: init_foil_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  initializes the foil geometry data
!
!> @param cell unit cell pointer
!> @param defects defects structure
!> @param npix number of x image pixels
!> @param npiy number of y image pixels
!> @param L pixel size for column approximation
!> @param dinfo flag to print information
! 
!> @date 11/22/15 MDG 1.0 new routine in Release 3.1; read portion replaced with JSONreadFoilData in JSONsupport
!--------------------------------------------------------------------------
recursive subroutine init_foil_data(cell,defects,npix,npiy,L,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: init_foil_data

use crystal
use io
use constants
use files
use rotations
use typedefs

IMPLICIT NONE

type(unitcell)                  :: cell
type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)    :: npix, npiy, dinfo
real(kind=sgl),INTENT(IN)       :: L

real(kind=sgl)                  :: io_real(1)
real(kind=dbl)                  :: amat(3,3)

! assign these values to the appropriate slots in foil%   [verified 4/23/11]
defects%foil%alP = defects%foil%alP*cPi/180.0            ! convert the tilt and rotation angles to radians
defects%foil%alS = defects%foil%alS*cPi/180.0
defects%foil%alR = defects%foil%alR*cPi/180.0
defects%foil%npix = npix                        ! image size (this duplicates some values, but it's easier this way)
defects%foil%npiy = npiy

! shape parameters
defects%foil%cpx = defects%foil%cpx * float(npix) * 0.5 * L  ! we'll define the foil shape center w.r.t. to the center 
                                                             ! of the image in [nm] coordinates
defects%foil%cpy = defects%foil%cpy * float(npiy) * 0.5 * L  ! 

! initialize a bunch of foil related quantities, using quaternions for all rotations
call initialize_foil_geometry(cell,defects%foil,dinfo)

! compute the projected thickness
amat = qu2om(defects%foil%a_fm)
defects%foil%zb = defects%foil%z0/amat(3,3)
if (dinfo.eq.1) then
  io_real(1)=defects%foil%z0
  call WriteValue('Nominal foil thickness = ', io_real, 1, "(F8.3)")
  io_real(1)=defects%foil%zb
  call WriteValue('Effective foil thickness = ', io_real, 1, "(F8.3/)")
end if

end subroutine init_foil_data


!--------------------------------------------------------------------------
!
! SUBROUTINE: init_dislocation_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  init dislocation namelist files
!
!> @param cell unit cell pointer
!> @param defects defect structure
!> @param DF_npix number of x-pixels
!> @param DF_npiy number of y-pixels
!> @param DF_gf 
!> @param L 
!> @param dinfo logical to trigger verbose output
! 
!> @date 01/05/99 MDG 1.0 original
!> @date 05/19/01 MDG 2.0 f90 version
!> @date 11/27/01 MDG 2.1 added kind support
!> @date 06/04/13 MDG 3.0 rewrite
!> @date 06/09/14 MDG 4.0 added cell, DL argument
!> @date 11/22/15 MDG 4.1 old routine obsolete with Release 3.1; replaced by JsonreadDefectFile
!> @date 11/23/15 MDG 4.2 moved from dislocation.f90 to defectmodule.f90
!--------------------------------------------------------------------------
recursive subroutine init_dislocation_data(cell,defects,DF_npix,DF_npiy,DF_gf,L,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: init_dislocation_data

use io
use files

IMPLICIT NONE

type(defecttype),INTENT(INOUT)          :: defects
!f2py intent(in,out) ::  defects
type(unitcell)                          :: cell
integer(kind=irg),INTENT(IN)            :: DF_npix, DF_npiy, dinfo
real(kind=sgl),INTENT(IN)               :: DF_gf(3), L

integer(kind=irg)                       :: i

! zfrac goes between -0.5 and +0.5, with -0.5 being the top surface and +0.5 the bottom
! this only really matters for dislocations that are parallel to the foil surfaces

! loop over all regular dislocations and initialize their displacement field parameters
   do i=1,defects%numdisl  ! +2*defects%numsf   ! we do not deal with partials in stacking faults here ...
    
! center of dislocation inside the foil is transformed to foil coordinates [nm] with defects%DL(i)%kd=0 (center of foil) [4/23/11]
! the point (0,0) is at the center of the image ... hence the factor of 0.5
    defects%DL(i)%id = defects%DL(i)%id * 0.5 * float(DF_npix) ! * L   scaling is done later in the image reference frame...
    defects%DL(i)%jd = defects%DL(i)%jd * 0.5 * float(DF_npiy) ! * L
    defects%DL(i)%g = DF_gf
     
! and pre-compute the dislocation displacement field parameters
    call makedislocation(cell,defects,i,dinfo, L)
  end do
  
end subroutine init_dislocation_data


!--------------------------------------------------------------------------
!
! SUBROUTINE: init_stacking_fault_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  read stacking fault namelist files
!
!> @param cell unit cell pointer
!> @param defects defect structure
!> @param DF_L 
!> @param DF_npix number of x-pixels
!> @param DF_npiy number of y-pixels
!> @param DF_gf 
!> @param dinfo logical to trigger verbose output
!> @param ECCI logical optional to indicate ECCI formatting rather than regular TEM
! 
!> @date    1/5/99  MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   06/04/13 MDG 3.0 rewrite
!> @date   12/17/13 MDG 3.1 added ECCI mode
!> @date   06/09/14 MDG 4.0 added cell, defects arguments
!> @date   06/10/14 MDG 4.1 added foil argument
!--------------------------------------------------------------------------
recursive subroutine init_stacking_fault_data(cell,defects,DF_L,DF_npix,DF_npiy,DF_g,dinfo,ECCI)
!DEC$ ATTRIBUTES DLLEXPORT :: init_stacking_fault_data

use io
use files

IMPLICIT NONE

type(unitcell)                          :: cell
type(defecttype),INTENT(INOUT)          :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)            :: DF_npix, DF_npiy, dinfo
real(kind=sgl),INTENT(IN)               :: DF_g(3)
real(kind=sgl),INTENT(IN)               :: DF_L
logical,INTENT(IN),OPTIONAL             :: ECCI

integer(kind=irg)                       :: i
real(kind=sgl)                          :: poisson


! read the namelist files for all of the stacking faults
 do i=1,defects%numsf
!   SFR = (/ 0.0, 0.0, 0.0 /)
    poisson = 0.0
! transform the fault fractional coordinates to nm in the image reference frame
    defects%SF(i)%id = defects%SF(i)%id * 0.5 * float(DF_npix) ! * DF_L  (zooming is done later in the image reference frame)
    defects%SF(i)%jd = defects%SF(i)%jd * 0.5 * float(DF_npiy) ! * DF_L
    defects%SF(i)%poisson = poisson
!   if (sum(abs(SFR)).eq.0.0) then  
      defects%SF(i)%Rdisp = defects%SF(i)%lpb
!   else
!     defects%SF(i)%Rdisp = SFR
!   end if
! initialize the stacking fault variables and both partial dislocations; this might depend
! on the imaging mode (TEM vs. ECCI); careful here, since the counting of dislocations has
! changed with respect to release 2.0 !!!
    if (present(ECCI)) then
      call makestackingfaultECCI(cell,defects,i,DF_L,DF_npix,DF_npiy,DF_g,dinfo)
      defects%numYdisl = defects%numYdisl + 2
    else
      call makestackingfault(cell,defects,i,DF_L,DF_npix,DF_npiy,DF_g,dinfo)
      defects%numdisl = defects%numdisl + 2
    end if
 end do
 
end subroutine init_stacking_fault_data

!--------------------------------------------------------------------------
!
! SUBROUTINE: init_YSH_dislocation_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief init Yoffe dislocation 
! 
!> @param cell unit cell pointer
!> @param defects defects structure
!> @param DF_npix number of x-pixels
!> @param DF_npiy number of y-pixels
!> @param DF_gf 
!> @param L 
!> @param dinfo logical to trigger verbose output
! 
!> @date  1/5/99  MDG 1.0 original
!> @date  5/19/01 MDG 2.0 f90 version
!> @date 11/27/01 MDG 2.1 added kind support
!> @date 03/25/13 MDG 3.0 updated IO
!> @date 11/21/13 MDG 3.1 verification
!> @date 06/10/14 MDG 4.0 added defects, cell and foil arguments
!> @date 11/23/15 MDG 4.1 made foil part of defects
!--------------------------------------------------------------------------
recursive subroutine init_YSH_dislocation_data(cell,defects,DF_npix,DF_npiy,DF_gf,L,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: init_YSH_dislocation_data

use io
use files

IMPLICIT NONE

type(unitcell)                  :: cell
type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)    :: DF_npix, DF_npiy, dinfo
real(kind=sgl),INTENT(IN)       :: DF_gf(3), L

integer(kind=irg)               :: i
real(kind=sgl)                  :: id,jd,u(3),bv(3),poisson

! these are just the individual dislocations; the ones that belong to 
! stacking faults are handled separately
do i=1,defects%numYdisl
! top-of-the-foil intersection of dislocation line transformed to foil coordinates [nm] with DL(i)%kd=0 (center of foil) [4/23/11]
! the point (0,0) is at the center of the image ... hence the factor of 0.5
  defects%YD(i)%id = defects%YD(i)%id * 0.5 * float(DF_npix) ! * L   scaling is done later in the image reference frame...
  defects%YD(i)%jd = defects%YD(i)%jd * 0.5 * float(DF_npiy) ! * L
  defects%YD(i)%g = DF_gf
    
! and pre-compute the dislocation displacement field parameters
  call makeYSHdislocation(cell,defects,i,dinfo, L)    
end do
  
end subroutine init_YSH_dislocation_data


!--------------------------------------------------------------------------
!
! SUBROUTINE: init_void_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  init void parameters 
! 
!> @param defects defects structure
!> @param DF_L column edge length 
!> @param DF_npix number of x-pixels
!> @param DF_npiy number of y-pixels
!> @param dinfo logical to trigger verbose output
! 
!> @date 01/05/99 MDG 1.0 original
!> @date 05/19/01 MDG 2.0 f90 version
!> @date 11/27/01 MDG 2.1 added kind support
!> @date 03/25/13 MDG 3.0 updated IO
!> @date 06/09/14 MDG 4.0 added defects argument
!> @date 06/10/14 MDG 4.1 added foil argument
!> @date 11/23/15 MDG 4.2 removed foil and put it inside defects
!--------------------------------------------------------------------------
recursive subroutine init_void_data(defects,DF_L,DF_npix,DF_npiy,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: init_void_data

use io
use files

IMPLICIT NONE

type(defecttype),INTENT(INOUT)      :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)        :: dinfo,DF_npix,DF_npiy
real(kind=sgl),INTENT(IN)           :: DF_L

integer(kind=irg)                   :: i
real(kind=sgl)                      :: tmp(3)

! read each subsequent line 
do i=1,defects%numvoids
  defects%voids(i)%xpos = defects%voids(i)%xpos * 0.5 * float(DF_npix) * DF_L
  defects%voids(i)%ypos = defects%voids(i)%ypos * 0.5 * float(DF_npiy) * DF_L
  defects%voids(i)%zpos = defects%voids(i)%zpos * defects%foil%z0
! transform to the foil reference frame  
  tmp = quat_Lp( conjg(defects%foil%a_fc), dble((/ defects%voids(i)%xpos, defects%voids(i)%ypos, defects%voids(i)%zpos /)) )  
  defects%voids(i)%xpos = tmp(1)
  defects%voids(i)%ypos = tmp(2)
  defects%voids(i)%zpos = tmp(3)
  if (dinfo.eq.1) write (*,*) i,defects%voids(i)%xpos,defects%voids(i)%ypos,defects%voids(i)%zpos,defects%voids(i)%radius
end do

end subroutine init_void_data

!--------------------------------------------------------------------------
!
! SUBROUTINE: init_inclusion_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  init inclusion parameters 
! 
!> @param defects defect structure
!> @param DF_L column edge length 
!> @param DF_npix number of x-pixels
!> @param DF_npiy number of y-pixels
!> @param dinfo logical to trigger verbose output
! 
!> @date  01/05/99 MDG 1.0 original
!> @date  05/19/01 MDG 2.0 f90 version
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/25/13 MDG 3.0 updated IO
!> @date  06/09/14 MDG 4.0 added defects argument
!> @date  06/10/14 MDG 4.1 added foil argument
!> @date  11/23/15 MDG 4.2 made foil part of defects
!--------------------------------------------------------------------------
recursive subroutine init_inclusion_data(defects,DF_L,DF_npix,DF_npiy,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: init_inclusion_data

use io
use files

IMPLICIT NONE

type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)    :: dinfo,DF_npix,DF_npiy
real(kind=sgl),INTENT(IN)       :: DF_L

integer(kind=irg)               :: i
real(kind=sgl)                  :: tmp(3)

! read each subsequent line 
do i=1,defects%numinc
  defects%inclusions(i)%xpos = defects%inclusions(i)%xpos * 0.5 * float(DF_npix)*DF_L
  defects%inclusions(i)%ypos = defects%inclusions(i)%ypos * 0.5 * float(DF_npiy)*DF_L
  defects%inclusions(i)%zpos = defects%inclusions(i)%zpos * defects%foil%z0   ! vertical fractional location in interval [-1,1]
  tmp = quat_Lp( conjg(defects%foil%a_fc), dble((/ defects%inclusions(i)%xpos, defects%inclusions(i)%ypos, &
        defects%inclusions(i)%zpos /)) )  
  defects%inclusions(i)%xpos = tmp(1)
  defects%inclusions(i)%ypos = tmp(2)
  defects%inclusions(i)%zpos = tmp(3)
  if (dinfo.eq.1) write (*,*) i, defects%inclusions(i)%xpos, defects%inclusions(i)%ypos, defects%inclusions(i)%zpos, &
                              defects%inclusions(i)%radius, defects%inclusions(i)%C
end do

end subroutine init_inclusion_data

!--------------------------------------------------------------------------
!
! SUBROUTINE: init_Einclusion_data
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  init inclusion parameters 
! 
!> @param cell cell structure pointer
!> @param defects defect structure
!> @param DF_L column edge length 
!> @param DF_npix number of x-pixels
!> @param DF_npiy number of y-pixels
!> @param dinfo logical to trigger verbose output
! 
!> @date  01/05/99 MDG 1.0 original
!> @date  05/19/01 MDG 2.0 f90 version
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/25/13 MDG 3.0 updated IO
!> @date  06/09/14 MDG 4.0 added defects argument
!> @date  06/10/14 MDG 4.1 added foil argument
!> @date  11/23/15 MDG 4.2 made foil part of defects
!> @date  12/08/15 MDG 4.3 forked from init_inclusion_data
!> @date  12/11/15 MDG 4.4 reworked based on IDL ellipsoid.pro script
!--------------------------------------------------------------------------
recursive subroutine init_Einclusion_data(cell,defects,DF_L,DF_npix,DF_npiy,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: init_Einclusion_data

use io
use files

IMPLICIT NONE

type(unitcell)                  :: cell
type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)    :: dinfo,DF_npix,DF_npiy
real(kind=sgl),INTENT(IN)       :: DF_L

integer(kind=irg)               :: i
real(kind=sgl)                  :: tmp(3)

! read each subsequent line 
do i=1,defects%numEinc
  defects%Einclusions(i)%xpos = defects%Einclusions(i)%xyz(1) * 0.5 * float(DF_npix)*DF_L
  defects%Einclusions(i)%ypos = defects%Einclusions(i)%xyz(2) * 0.5 * float(DF_npiy)*DF_L
  defects%Einclusions(i)%zpos = defects%Einclusions(i)%xyz(3) * defects%foil%z0    ! vertical fractional location in interval [-1,1]
  tmp = quat_Lp( conjg(defects%foil%a_fc), dble((/ defects%Einclusions(i)%xpos, defects%Einclusions(i)%ypos, &
        defects%Einclusions(i)%zpos /)) )  
  defects%Einclusions(i)%xpos = tmp(1)
  defects%Einclusions(i)%ypos = tmp(2)
  defects%Einclusions(i)%zpos = tmp(3)

! here we call the initialization routine to compute a series of arrays 
! and look-up tables for elliptic integrals  
  call InitializeEshelbyInclusion(cell,defects,i,dinfo,DF_L,DF_npix,DF_npiy)  

  if (dinfo.eq.1) write (*,*) i, defects%Einclusions(i)%xpos, defects%Einclusions(i)%ypos, defects%Einclusions(i)%zpos
end do

end subroutine init_Einclusion_data


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! and here are the routines that actually compute all the defect parameters
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE: initialize_foil_geometry
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Initializes the foil geometry
!
!> @details This new implementation uses quaternions for all rotations. 
! 
!> @param cell unit cell pointer
!> @param dinfo
! 
!> @date  1/ 5/99 MDG 1.0 original
!> @date  1/11/10 MDG 2.0 rewrite of beam direction part
!> @date  3/28/11 MDG 2.1 code verified
!> @date  4/23/11 MDG 2.2 redefined origin to be at center of image
!> @date  6/03/13 MDG 3.0 replaced rotation matrices by quaternions throughout
!> @date 10/30/13 MDG 3.1 complete debug of quaternion and rotation implementation 
!> @date 06/09/14 MDG 4.0 added cell and foil as argument
!--------------------------------------------------------------------------
recursive subroutine initialize_foil_geometry(cell,foil,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: initialize_foil_geometry

use local
use typedefs
use math
use constants
use crystal
use symmetry
use io
use rotations

IMPLICIT NONE

type(unitcell)                  :: cell
type(foiltype),INTENT(INOUT)    :: foil
!f2py intent(in,out) ::  foil
integer(kind=sgl),INTENT(IN)    :: dinfo

real(kind=dbl)                  :: ey(3), ex(3), tt, dx, dy 
real(kind=sgl)                  :: io_real(3)
real(kind=dbl)                  :: cp,sp,cs,ss,cr,sr, ca, sa, a_fc(3,3)
integer(kind=irg)               :: i,j
type(orientationtyped)          :: ot
character(10)                   :: pret

! determine the foil-to-microscope transformations [verified on 4/23/11, converted to quaternions on 6/4/13, 
! verified 10/30/13, and again on 11/11/13 after some changes elsewhere]
  if (foil%alR.eq.0.D0) then 
! the double tilt holder transformation a_fm; note quaternions, hence we need the half-angles !
! a_fm transforms a vector v FROM the microscope reference frame To the foil reference frame
! using the quat_rotate_vector routine.
    cp = dcos(foil%alP*0.5D0)
    sp = dsin(foil%alP*0.5D0)
    ca = dcos(foil%alP)
    sa = dsin(foil%alP)
    cs = dcos(foil%alS*0.5D0)
    ss = dsin(foil%alS*0.5D0)
    foil%a_fm = conjg( quat_mult( (/ cs, 0.D0, ss*ca, ss*sa /), (/ cp, sp, 0.D0, 0.D0 /) ) )
  else
! the rotation tilt holder transformation a_fm [verified on 4/23/11, converted to quaternions on 6/4/13,
! and again on 11/11/13 after changes elsewhere]
    cp = dcos(foil%alP*0.5D0)
    sp = dsin(foil%alP*0.5D0)
    cr = dcos(foil%alR*0.5D0)
    sr = dsin(foil%alR*0.5D0)
    ca = dcos(foil%alP)
    sa = dsin(foil%alP)
    foil%a_fm = conjg( quat_mult( (/ cr,0.D0, -sr*sa, sr*ca /), (/ cp, sp,0.D0,0.D0  /) ) )
  end if  
  if (dinfo.eq.1) then
    pret = 'a_fm: '
    ot = init_orientation(foil%a_fm,'qu')
    call print_orientation(ot,'om',pret) 
  end if

! a_mi (image to microscope) apart from a scale factor, these two are identical 
! The EM book uses a beta rotation angle between the image and the microscope,
! but that is really not necessary because we already fix the image with respect to
! the microscope by defining q (the horizontal image direction) to point to the 
! airlock. [verified 4/23/11, converted to quaternions on 6/4/13]
! So we'll keep this transformation equal to the identity at all times.
  foil%a_mi = (/ 1.D0,0.D0,0.D0,0.D0 /)   ! identity quaternion 
  if (dinfo.eq.1) then
    pret = 'a_mi: '
    ot = init_orientation(foil%a_mi,'qu')
    call print_orientation(ot,'om',pret) 
  end if
  
! This allows us to get the beam direction, since we know the foil normal and tilt angles
! The beam direction is the inverse transform of the microscope e_z-axis to the foil reference frame [verified 11/12/13]
  foil%B = quat_Lp( conjg(foil%a_fm), (/ 0.D0,0.D0,-1.D0 /) )
  foil%Bn = foil%B
  call NormVec(cell,foil%Bn,'c')
  if (dinfo.eq.1) then
    io_real(1:3) = foil%B(1:3)
    call WriteValue('  Beam direction (foil reference frame) = ',io_real,3,"('[',F12.5,',',F12.5,',',F12.5,']')")
  end if

! transform both the foil normal and the q-vector to the crystal cartesian reference frame (eq. 8.8) [verified 4/23/11,
! and again on 11/12/13 afterchanges elsewhere]
  call TransSpace(cell,foil%F,foil%Fn,'d','c')
  call TransSpace(cell,foil%q,foil%qn,'r','c')
  call NormVec(cell,foil%Fn,'c')
  call NormVec(cell,foil%qn,'c')
! a_fc (crystal to foil)  
  a_fc(3,1:3) = foil%Fn(1:3)
  a_fc(1,1:3) = foil%qn(1:3)
  call CalcCross(cell,foil%Fn,foil%qn,ey,'c','c',0)
  call NormVec(cell,ey,'c')
  a_fc(2,1:3) = ey(1:3)
  foil%a_fc = om2qu(a_fc)
  if (dinfo.eq.1) then
    pret = 'a_fc: '
    ot = init_orientation(foil%a_fc,'qu')
    call print_orientation(ot,'om',pret) 
  end if
  
! a_mc (crystal to microscope)
  foil%a_mc = quat_mult( conjg(foil%a_fm), foil%a_fc )
  if (dinfo.eq.1) then
    pret = 'a_mc: '
    ot = init_orientation(foil%a_mc,'qu')
    call print_orientation(ot,'om',pret) 
  end if
  
! a_ic (crystal to image)
  foil%a_ic = quat_mult( conjg(foil%a_mi) , foil%a_mc)
  if (dinfo.eq.1) then
    pret = 'a_ic: '
    ot = init_orientation(foil%a_ic,'qu')
    call print_orientation(ot,'om',pret) 
  end if
  
! a_fi (image to foil)
  foil%a_fi = quat_mult( foil%a_fc , conjg(foil%a_ic) )
  if (dinfo.eq.1) then
    pret = 'a_fi: '
    ot = init_orientation(foil%a_fi,'qu')
    call print_orientation(ot,'om',pret) 
  end if
  
! express the beam direction in the Bravais reference frame [verified 4/23/11, and again on 11/12/13
! after changes elsewhere]
  ex = quat_Lp( conjg(foil%a_fc), dble(foil%Bn) ) 
  call TransSpace(cell,ex,ey,'c','d')
  call NormVec(cell,ey,'c')
  if (dinfo.eq.1) then
    io_real(1:3) = ey(1:3)
    call WriteValue('  Beam direction (crystal reference frame) = ', io_real, 3, "('[',F12.5,',',F12.5,',',F12.5,']'/)")
  end if
  
! define the foil shape (for now as an elliptic paraboloid z = brx * (x-xc)^2 + bry * (y-yc)^2) 
if (.not.allocated(foil%sg)) allocate(foil%sg(foil%npix,foil%npiy))
! if the foil is not bent, then we set this array to zero, otherwise we compute the elliptical paraboloid
if ((foil%brx.eq.0.0).and.(foil%bry.eq.0.0)) then
  if (dinfo.eq.1) then
    call Message(' Initializing a flat foil ', frm = "(A)")
  end if
  foil%sg = 0.0
else
  dx = foil%npix*0.5
  dy = foil%npiy*0.5
  do i=1,foil%npix
   tt = foil%brx * (float(i)-dx-foil%cpx)**2
    do j=1,foil%npiy
! initialize the foil shape function; we assume that the center of the elliptic paraboloid is at location (cpx,cpy)
! presumably, this surface could also be a saddle point if the brx and bry values have opposite sign ...
      foil%sg(i,j) = tt + foil%bry * (float(j)-dy-foil%cpy)**2+ 2.0*foil%brxy * (float(j)-dy-foil%cpy)*(float(i)-dx-foil%cpx)
    end do
  end do
  if (dinfo.eq.1) then
    call Message(' Initializing a bent foil ', frm = "(A)")
    io_real(1)=minval(foil%sg); io_real(2)=maxval(foil%sg); 
    call WriteValue('Range of local excitation error deviations : ', io_real, 2, "(F10.6,',',F10.6/)")
  end if
end if

end subroutine initialize_foil_geometry

!--------------------------------------------------------------------------
!
! SUBROUTINE: makedislocation
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  Compute the dismat displacement matrix for a given dislocation
!
!> @details This subroutine computes the matrix dismat that describes the displacement field
!> of a dislocation.  The routine needs the elastic moduli tensor, the transformation
!> matrix between the crystal and dislocation reference frames, and the dislocation
!> Burgers vector.  The routine computes the arrays dismat and pa, which should be used as follows:
!> 
!> R_k = 2.0*real([ sum_a=1^3 (dismat(k,a)*log(Z_a)) ]),
!> 
!> with Z_a = x_1 + pa(a)*x_2
!>
!>  [see CalcR subroutine for more information]
!>
!> We must also make sure that the x=0 plane of the defect reference frame contains the 
!> incident beam direction, to avoid getting stacking-fault fringes in the wrong plane...
!> Actual stacking faults are added in using a different module (stacking_fault.f90).
!>
!> @param cell unit cell pointer
!> @param defects defect structure
!> @param inum
!> @param dinfo
!> @param DF_L column width
! 
!> @date   1/ 5/99 MDG 1.0 original
!> @date   5/19/01 MDG 2.0 f90 version
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  06/04/13 MDG 3.0 rewrite
!> @date  10/30/13 MDG 3.1 debug of all rotation parts
!> @date  06/09/14 MDG 4.0 added cell, defects arguments
!> @date  06/10/14 MDG 4.1 added foil as argument
!> @date  11/21/15 MDG 4.2 moved foil into defects structure
!--------------------------------------------------------------------------
recursive subroutine makedislocation(cell,defects,inum,dinfo,DF_L)
!DEC$ ATTRIBUTES DLLEXPORT :: makedislocation

use math
use constants
use crystal
use math
use io
use symmetry
use rotations

IMPLICIT NONE

type(unitcell)                          :: cell
type(defecttype),INTENT(INOUT)          :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)            :: inum
integer(kind=irg),INTENT(IN)            :: dinfo
real(kind=sgl),INTENT(IN)               :: DF_L

type(foiltype)                          :: foil
real(kind=dbl)                          :: zz,zang,zmin
real(kind=sgl)                          :: ec(6,6),lec(6,6)
real(kind=dbl)                          :: a_dc(3,3),tmp(3),ex(3),ey(3)
real(kind=dbl)                          :: Bij(3,3),Hij(3,3)
complex(kind=dbl)                       :: a(0:6),b(0:6),c(0:6),d(0:6),e(0:6),ff(0:6),tt(5,0:6),s(0:6),roots(6), &
                                           zero,pasq(3),mat(3,3),aka(3,3),Lia(3,3),Mai(3,3),v(3),pas
integer(kind=irg)                       :: i,j,k,l,imin,ind(3),jnd(3)

foil = defects%foil

! convert line direction and g-vector to the Cartesian crystal reference frame
call TransSpace(cell,defects%DL(inum)%u,defects%DL(inum)%un,'d','c')
call TransSpace(cell,defects%DL(inum)%g,defects%DL(inum)%gn,'r','c')
! normalize both vectors
call NormVec(cell,defects%DL(inum)%un,'c')
call NormVec(cell,defects%DL(inum)%gn,'c')

! first find the length of the dislocation line inside the foil along the
! dislocation z-axis, which is the line direction; also, compute the intersection
! points of the line with the top and bottom surfaces  (all components in [nm])
zang = CalcAngle(cell,defects%DL(inum)%u,dble(foil%F),'d')
zz = cos(zang)
if (abs(zz).gt.0.00001) then 
  defects%DL(inum)%zu = 0.5*foil%z0/zz
else
  defects%DL(inum)%zu = 100000.0         ! this is when the dislocation is nearly parallel to the foil
end if

! transform the line direction to the foil reference frame
tmp = quat_Lp( foil%a_fc, dble(defects%DL(inum)%un) ) / DF_L

if (dinfo.eq.1) then
  write (*,*) 'transformed line direction ', tmp, zang, zz
end if

! determine the top and bottom intersection coordinates 
if (zz.gt.0.0) then  ! u points to the top of the foil
    defects%DL(inum)%top = (/ defects%DL(inum)%id + tmp(1)*defects%DL(inum)%zu, defects%DL(inum)%jd + &
        tmp(2)*defects%DL(inum)%zu, 0.5D0*foil%z0 /)
    defects%DL(inum)%bottom = (/ defects%DL(inum)%id - tmp(1)*defects%DL(inum)%zu, defects%DL(inum)%jd - &
        tmp(2)*defects%DL(inum)%zu, -0.5D0*foil%z0 /)
else                 ! u points to the bottom of the foil
    defects%DL(inum)%top = (/ defects%DL(inum)%id - tmp(1)*defects%DL(inum)%zu, defects%DL(inum)%jd - &
        tmp(2)*defects%DL(inum)%zu, -0.5D0*foil%z0 /)
    defects%DL(inum)%bottom = (/ defects%DL(inum)%id + tmp(1)*defects%DL(inum)%zu, defects%DL(inum)%jd + &
        tmp(2)*defects%DL(inum)%zu, 0.5D0*foil%z0 /)
end if  

if (dinfo.eq.1) then
  write (*,*) defects%DL(inum)%id,defects%DL(inum)%jd
  write (*,*) 'dislocation top intersection at ',defects%DL(inum)%top
  write (*,*) 'dislocation bottom intersection at ',defects%DL(inum)%bottom
end if 

! a_dc (crystal to defect)  matrix corrected on 11/29/10 to put defect x-axis in the plane of u and B
if (dinfo.eq.1) then
  write (*,*) 'cartesian quantities'
  write (*,*) 'unit line direction = ',defects%DL(inum)%un
  write (*,*) 'unit beam direction = ',foil%Bn
end if

! transform beam direction (currently in foil frame) to cartesian 
tmp = quat_Lp( conjg(foil%a_fc), dble(foil%Bn))
!tmp = quat_rotate_vector(conjg(foil%a_fc), (/ 0.0D0, 0.0D0, -1.0D0/) )
call NormVec(cell,tmp,'c')

! the defect z axis is the line direction and x is in the plane of u and B to avoid the intrinsic discontinuity (cut plane)
a_dc(3,1:3) = defects%DL(inum)%un(1:3)
call CalcCross(cell,dble(defects%DL(inum)%un),tmp,ex,'c','c',0)
call NormVec(cell,ex,'c')
a_dc(1,1:3) = ex(1:3)
call CalcCross(cell,dble(defects%DL(inum)%un),ex,ey,'c','c',0)
call NormVec(cell,ey,'c')
a_dc(2,1:3) = ey(1:3)
defects%DL(inum)%a_dc = om2qu(a_dc)

if (dinfo.eq.1) then
  call PrintMatrixd('a_dc',a_dc)
end if

! a_di (image to defect)
defects%DL(inum)%a_di = quat_mult( defects%DL(inum)%a_dc, conjg(foil%a_ic) )
defects%DL(inum)%a_id = conjg(defects%DL(inum)%a_di)

if (dinfo.eq.1) then
  call print_orientation_d(init_orientation_d(defects%DL(inum)%a_di,'qu'),'om','a_di:     ')
  call print_orientation_d(init_orientation_d(defects%DL(inum)%a_id,'qu'),'om','a_id:     ')
end if

! finally, get the foil to defect transformation (used in defect module)
defects%DL(inum)%a_df = quat_mult( defects%DL(inum)%a_di, conjg(foil%a_fi) )

! Burgers vector (in the defect reference frame !!!)
! first transform Burgers vector to crystal cartesian reference frame
call TransSpace(cell,dble(defects%DL(inum)%burg),tmp,'d','c')
! then convert this to the defect reference frame
defects%DL(inum)%burgd(1:3) = quat_Lp( defects%DL(inum)%a_dc,dble(tmp))

if (dinfo.eq.1) then
  write (*,*) 'rotated burgers vector  = ', defects%DL(inum)%burgd(1:3) 
end if

! transform the elastic moduli
lec = foil%elmo

! transform lec to defect reference frame
a_dc = qu2om(defects%DL(inum)%a_dc)
call TransFourthRankTensor(a_dc,lec,ec)
if (dinfo.eq.1)  then 
  write (*,*) 'Elasticity tensor in defect reference frame'
  do i=1,6 
    write (*,"(6(F8.4,2x))") (ec(i,j),j=1,6)
  end do
  write (*,*) '----'
end if

! next, create the sextic polynomial
zero = cmplx(0.0,0.0,dbl)
a=zero; b=zero; c=zero; d=zero; e=zero; ff=zero
a(0:2) = (/ cmplx(ec(1,1),0.0,dbl), cmplx(ec(1,6)*2.0,0.0,dbl),     cmplx(ec(6,6),0.0,dbl) /)
b(0:2) = (/ cmplx(ec(6,6),0.0,dbl), cmplx(ec(2,6)*2.0,0.0,dbl),     cmplx(ec(2,2),0.0,dbl) /)
c(0:2) = (/ cmplx(ec(5,5),0.0,dbl), cmplx(ec(4,5)*2.0,0.0,dbl),     cmplx(ec(4,4),0.0,dbl) /)
d(0:2) = (/ cmplx(ec(5,6),0.0,dbl), cmplx(ec(4,6)+ec(2,5),0.0,dbl), cmplx(ec(2,4),0.0,dbl) /)
e(0:2) = (/ cmplx(ec(1,5),0.0,dbl), cmplx(ec(1,4)+ec(5,6),0.0,dbl), cmplx(ec(4,6),0.0,dbl) /)
ff(0:2) = (/ cmplx(ec(1,6),0.0,dbl), cmplx(ec(1,2)+ec(6,6),0.0,dbl), cmplx(ec(2,6),0.0,dbl) /)
tt = zero
s = zero

! matrix elements
do j=0,6 
 do i=0,j 
  tt(1,j) = tt(1,j) + a(j-i)*b(i)
  tt(2,j) = tt(2,j) + d(j-i)*e(i)
  tt(3,j) = tt(3,j) + a(j-i)*d(i)
  tt(4,j) = tt(4,j) + b(j-i)*e(i)
  tt(5,j) = tt(5,j) + c(j-i)*ff(i)
 end do
end do

! determinant leading to the sextic equation
do j=0,6 
 do i=0,j 
  s(j) = s(j) + tt(1,j-i)*c(i) + 2.0*tt(2,j-i)*ff(i) - tt(3,j-i)*d(i) - tt(4,j-i)*e(i) - tt(5,j-i)*ff(i)
 end do
end do

! get the complex root pairs
call zroots(s,roots)

! then, solve the equation for the vector A_k using the roots with positive imaginary part.
k=1
do j=1,6
  if (aimag(roots(j)).gt.0.0) then
    defects%DL(inum)%pa(k) = roots(j)
    k=k+1
  end if
end do

! renumber them to avoid the symmetry degeneracy (see page 328 Head et al.)
v(1:3) = ec(5,5) + 2.0*defects%DL(inum)%pa(1:3)*ec(4,5) + ec(4,4)*defects%DL(inum)%pa(1:3)**2
zmin = 100.0
imin = 0

! where is the smallest value ?
do i=1,3
  if (abs(v(i)).lt.zmin) then
    imin=i
    zmin = abs(v(i))
  end if
end do

! is the 3rd one the smallest ? if not, then swap with the current 3rd one.
if (imin.ne.3) then
  pas = defects%DL(inum)%pa(imin)
  defects%DL(inum)%pa(imin)=defects%DL(inum)%pa(3)
  defects%DL(inum)%pa(3)=pas
end if
  pas = defects%DL(inum)%pa(1)
  defects%DL(inum)%pa(1)=defects%DL(inum)%pa(2)
  defects%DL(inum)%pa(2)=pas

! eliminate really small numbers
do i=1,3
  if (abs(aimag(defects%DL(inum)%pa(i))).lt.1.0e-8)  defects%DL(inum)%pa(i)=cmplx(real(defects%DL(inum)%pa(i)),0.0,dbl)
  if (abs(real(defects%DL(inum)%pa(i))).lt.1.0e-8)   defects%DL(inum)%pa(i)=cmplx(0.0,aimag(defects%DL(inum)%pa(i)),dbl)
end do
if (dinfo.eq.1) then
  write (*,*) ' sextic roots'
  do i=1,3
    write (*,*) defects%DL(inum)%pa(i)
  end do
  write (*,*) '---'
end if

!  compute the A_ka vectors (see description on page 328 of Head et al.)
pasq = defects%DL(inum)%pa**2
if (dinfo.eq.1) write (*,*) 'Aka vectors'
do k=1,3
  mat = zero
  mat(1,1) = ec(1,1)+2.D0*defects%DL(inum)%pa(k)*ec(1,6)+ec(6,6)*pasq(k)
  mat(2,2) = ec(6,6)+2.D0*defects%DL(inum)%pa(k)*ec(2,6)+ec(2,2)*pasq(k)
  mat(3,3) = ec(5,5)+2.D0*defects%DL(inum)%pa(k)*ec(4,5)+ec(4,4)*pasq(k)
  mat(2,3) = ec(5,6)+defects%DL(inum)%pa(k)*(ec(4,6)+ec(2,5))+ec(2,4)*pasq(k)
  mat(1,3) = ec(1,5)+defects%DL(inum)%pa(k)*(ec(1,4)+ec(5,6))+ec(4,6)*pasq(k)
  mat(1,2) = ec(1,6)+defects%DL(inum)%pa(k)*(ec(1,2)+ec(6,6))+ec(2,6)*pasq(k)
  if (k.eq.1) then
    aka(1,1) = mat(2,2)*mat(3,3)-mat(2,3)*mat(2,3)
    aka(1,2) = mat(1,3)*mat(2,3)-mat(1,2)*mat(3,3)
    aka(1,3) = mat(1,2)*mat(2,3)-mat(1,3)*mat(2,2)
  end if
  if (k.eq.2) then
    aka(2,1) = mat(1,3)*mat(2,3)-mat(1,2)*mat(3,3)
    aka(2,2) = mat(1,1)*mat(3,3)-mat(1,3)*mat(1,3)
    aka(2,3) = mat(1,3)*mat(1,2)-mat(1,1)*mat(2,3)
  end if
  if (k.eq.3) then
    aka(3,1) = mat(1,2)*mat(2,3)-mat(1,3)*mat(2,2)
    aka(3,2) = mat(1,3)*mat(1,2)-mat(1,1)*mat(2,3)
    aka(3,3) = mat(1,1)*mat(2,2)-mat(1,2)*mat(1,2)
  end if  
  if (dinfo.eq.1) write (*,*) k,(aka(k,j),j=1,3)
end do
aka = transpose(aka)

! next, create the L_ialpha matrix
ind = (/ 6, 2, 4 /)
jnd = (/ 1, 6, 5 /)
Lia = zero
do i=1,3 
 do j=1,3
  do k=1,3
   Lia(i,j) = Lia(i,j)+(ec(ind(i),jnd(k))+defects%DL(inum)%pa(j)*ec(ind(i),ind(k)))*aka(k,j)
  end do
 end do
end do
if (dinfo.eq.1)  call PrintMatrixcd('Lia ',Lia)

! and invert it
call cInvert(Lia,Mai)
if (dinfo.eq.1)  call PrintMatrixcd('Mai ',Mai)

! compute Bij ( real matrix )
Bij = 0.D0
do i=1,3
 do j=1,3
  do k=1,3
   Bij(i,j) = Bij(i,j) - aimag(aka(i,k))*real(Mai(k,j)) - real(aka(i,k))*aimag(Mai(k,j))
  end do
 end do
end do
if (dinfo.eq.1)  call PrintMatrixd('Bij ',Bij)

! and invert to get Hij
call mInvert(Bij,Hij,.FALSE.)
if (dinfo.eq.1) call PrintMatrixd('Hij ',Hij)

! compute matrix (this is what actually gets to be used for the 
! displacement field); needs to know the Burgers vector.
defects%DL(inum)%dismat = zero
do k=1,3
 do l=1,3
   do i=1,3
    do j=1,3
     defects%DL(inum)%dismat(k,l) = defects%DL(inum)%dismat(k,l) + defects%DL(inum)%burgd(i)*Hij(j,i)*Mai(l,j)*aka(k,l)
    end do
   end do
 end do
end do

! scale by 1/4pi
defects%DL(inum)%dismat = defects%DL(inum)%dismat*0.25D0/cPi
if (dinfo.eq.1)  call PrintMatrixcd('dismat',defects%DL(inum)%dismat)

! and return to calling routine
end subroutine makedislocation

!--------------------------------------------------------------------------
!
! SUBROUTINE: makestackingfault
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute parameters for a stacking fault
!
!> @details  This subroutine computes the geometrical parameters for a 
!> stacking fault.  It computes, among others, the coordinates of the 
!> centers of the partial dislocations, the intersections of each dislocation
!> line with the top and bottom foil surface, and an array that indicates, for
!> each image pixel, whether or not the corresponding integration column 
!> contains this fault; if it does not, the  value in the array is set to 
!> -10000; if it does, then the value is equal to the point where the fault
!> plane intersects with the column, measured from the top surface.
!> In short, anything that is needed in the CalcR routine and can be computed 
!> ahead of time, is computed here.  The routine also calls the makedislocation 
!> routine to create the partials.
! 
!> @param cell unit cell pointer
!> @param defects defect structure
!> @param inum
!> @param DF_L column edge length
!> @param nx
!> @param ny
!> @param DF_g
!> @param ndl
!> @param dinfo trigger for verbose output
!
!> @date   11/05/13 MDG 1.0 new attempt to replace faulty original routine
!> @date   11/13/13 MDG 1.1 traced error to problem with transformations in defectmodule
!> @date   11/13/13 MDG 1.2 changed SF normal transformation for zpos array computation (to be tested)
!> @date   06/09/14 MDG 2.0 added defects and cell as arguments
!> @date   06/10/14 MDG 2.1 added foil as argument 
!--------------------------------------------------------------------------
recursive subroutine makestackingfault(cell,defects,inum,DF_L,nx,ny,DF_g,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT ::  makestackingfault

use math
use constants
use files
use rotations
use crystal
use symmetry
use math

IMPLICIT NONE

type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: DF_L
real(kind=sgl),INTENT(IN)       :: DF_g(3)
integer(kind=irg),INTENT(IN)    :: inum, nx, ny, dinfo

real(kind=sgl)                  :: fpn(3),am(4,4),midpoint(3), ex(3), ey(3),&
                                 lptopi(3),lpboti(3),tptopi(3),tpboti(3),det,A(4), xx(4), yy(4), tmp(3), &
                                 planenormal(3), rzero(3), unita(3)

integer(kind=irg)               :: i,j,info,ipiv,minx,maxx,miny,maxy, ndl

ndl = defects%numdisl

! we begin by computing the geometry in the foil reference frame, which is the cartesian frame 
! for zero sample tilt;  sample tilts are applied once we known the partial dislocation geometry
call TransSpace(cell,defects%SF(inum)%plane,tmp,'r','c')
call NormVec(cell,tmp,'c')
planenormal =  tmp

call CalcCross(cell, planenormal, (/ 0.0,0.0,1.0 /),  unita, 'c', 'c', 0)
call NormVec(cell,unita,'c')
fpn = planenormal
if (dinfo.eq.1) write (*,*) ' unita should have zero third component ',unita, fpn

! the fault plane goes through the point rzero in the foil center plane
rzero = (/ defects%SF(inum)%id, defects%SF(inum)%jd, 0.0 /)

! this leads to the location of the partial dislocation centers
defects%SF(inum)%lpr = rzero + 0.5*defects%SF(inum)%sep*unita / DF_L
defects%SF(inum)%tpr = rzero - 0.5*defects%SF(inum)%sep*unita / DF_L

if (dinfo.eq.1) write (*,*) 'lpr_i = ',defects%SF(inum)%lpr(1:3)
if (dinfo.eq.1) write (*,*) 'tpr_i = ',defects%SF(inum)%tpr(1:3)

! call makedislocation for each of the partials
  defects%DL(ndl+1)%id = defects%SF(inum)%lpr(1) 
  defects%DL(ndl+1)%jd = defects%SF(inum)%lpr(2) 
  defects%DL(ndl+1)%u =  defects%SF(inum)%lpu
  defects%DL(ndl+1)%burg = defects%SF(inum)%lpb
  defects%DL(ndl+1)%g = DF_g
  call makedislocation(cell,defects,ndl+1,dinfo,DF_L)
  if (dinfo.eq.1) write (*,*) 'Leading Partial Position ',defects%DL(ndl+1)%id,defects%DL(ndl+1)%jd
    
  defects%DL(ndl+2)%id = defects%SF(inum)%tpr(1) 
  defects%DL(ndl+2)%jd = defects%SF(inum)%tpr(2) 
  defects%DL(ndl+2)%u = defects%SF(inum)%tpu
  defects%DL(ndl+2)%burg = defects%SF(inum)%tpb
  defects%DL(ndl+2)%g = DF_g
  call makedislocation(cell,defects,ndl+2,dinfo,DF_L)
  if (dinfo.eq.1)  write (*,*) 'Trailing Partial Position ',defects%DL(ndl+2)%id,defects%DL(ndl+2)%jd

! copy the top and bottom dislocation intersections (computed in make_dislocation) 
! into the corresponding variables of the SF record

defects%SF(inum)%lpbot = defects%DL(ndl+1)%bottom
defects%SF(inum)%lptop = defects%DL(ndl+1)%top
defects%SF(inum)%tpbot = defects%DL(ndl+2)%bottom
defects%SF(inum)%tptop = defects%DL(ndl+2)%top

! obviously, these four points need to lie in a single plane; at this point, we check that this is indeed the case
! by computing the volume of the tetrahedron formed by these four points; if the volume is zero, then the 
! points are co-planar.  (Use LAPACK's LU-decomposition and compute the product of the diagonal elements of U)
am(1:4,1) = (/ defects%SF(inum)%lptop(1:3),1.0 /)
am(1:4,2) = (/ defects%SF(inum)%lpbot(1:3),1.0 /) 
am(1:4,3) = (/ defects%SF(inum)%tptop(1:3),1.0 /) 
am(1:4,4) = (/ defects%SF(inum)%tpbot(1:3),1.0 /) 
call sgetrf(4,4,am,4,ipiv,info)
det = abs(am(1,1)*am(2,2)*am(3,3)*am(4,4))
if (dinfo.eq.1) write (*,*) 'determinant (should be zero) = ',det

! ok, next we need to figure out which image pixels lie on the projection of the stacking fault plane.
! We need to transform the corner points into the image reference frame !!!
lptopi = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%lptop))
lpboti = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%lpbot))
tptopi = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%tptop))
tpboti = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%tpbot))
if (dinfo.eq.1) then
  write (*,*) 'SF parameters :'
  write (*,*) lptopi,' <> ',lpboti
  write (*,*) tptopi,' <> ',tpboti
end if

! define the array that will contain the zpos values
allocate(defects%SF(inum)%zpos(nx,ny))
defects%SF(inum)%zpos = -10000.0    ! set all points to be outside the projected SF

! first determine the smaller box
minx = nint(min( lptopi(1),lpboti(1),tptopi(1),tpboti(1) )) -2
maxx = nint(max( lptopi(1),lpboti(1),tptopi(1),tpboti(1) )) +2
miny = nint(min( lptopi(2),lpboti(2),tptopi(2),tpboti(2) )) -2
maxy = nint(max( lptopi(2),lpboti(2),tptopi(2),tpboti(2) )) +2

! the fault edges may fall outside of the viewing frame (origin at the center !!!)
if (minx.lt.(-nx/2+1)) minx=-nx/2+1
if (maxx.gt.nx/2) maxx=nx/2
if (miny.lt.(-ny/2+1)) miny=-ny/2+1
if (maxy.gt.ny/2) maxy=ny/2

if (dinfo.eq.1) write (*,*) 'Integer fault box = ',minx,maxx,miny,maxy

! get the equation of the stacking fault plane in the image reference frame
! first the unit plane normal in image space
! we'll take two vectors: ex = from ltop to ttop; ey = from ltop to lbot
ex = tptopi - lptopi
ey = lpboti - lptopi
call NormVec(cell,ex,'c')
call NormVec(cell,ey,'c')
call CalcCross(cell,ex,ey,fpn,'c','c',0)

A(1:3) = fpn ! quat_LPstar( conjg(foil%a_fi), dble(fpn(1:3)) )
midpoint = 0.25*(lptopi+lpboti+tptopi+tpboti) ! quat_LPstar( conjg(foil%a_fi), dble(0.25*(lptopi+lpboti+tptopi+tpboti) ))
A(4) = sum(A(1:3)*midpoint)
if (dinfo.eq.1) write (*,*) 'fault plane parameters : ',A, midpoint

! rank the corner points so that the polygon is convex
! call rank_points(tpboti(1:2),lpboti(1:2),lptopi(1:2),tptopi(1:2),xx,yy)
xx = (/ lptopi(1), tptopi(1), tpboti(1),lpboti(1) /)
yy = (/ lptopi(2), tptopi(2), tpboti(2),lpboti(2) /)

! for all of the points inside this box:
do i=minx,maxx
  do j=miny,maxy
    if (point_inside_polygon( float(i), float(j), xx, yy ).gt.0) then 
! the point lies inside the projected region, 
! so we need the depth of the SF plane at this position, taking into account the 
! proper coordinate transformation (depth must be expressed in image reference frame)
        defects%SF(inum)%zpos(i+nx/2,j+ny/2) =  DF_L * ( A(4) - A(1)*float(i) - A(2)*float(j) )/A(3)
    end if
  end do
end do
if (dinfo.eq.1) write (*,*) 'fault plane pixels determined'

! let's also make sure that the SF displacement vector is translated to the 
! cartesian reference frame, so that it can be used directly by the CalcR routine
defects%SF(inum)%lpbc = matmul(cell%dsm,defects%SF(inum)%Rdisp)

! that should do it for the stacking fault...  The rest 
! takes place in the CalcR routine.
end subroutine makestackingfault


!--------------------------------------------------------------------------
!
! SUBROUTINE: makestackingfaultECCI
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute parameters for a stacking fault in ECCI mode
!
!> @details  This subroutine computes the geometrical parameters for a 
!> stacking fault.  It computes, among others, the coordinates of the surface
!> intersections of the partial dislocations, and an array that indicates, for
!> each image pixel, whether or not the corresponding integration column 
!> contains this fault; if it does not, the  value in the array is set to 
!> -10000; if it does, then the value is equal to the point where the fault
!> plane intersects with the column, measured from the top surface.
!> In short, anything that is needed in the CalcR routine and can be computed 
!> ahead of time, is computed here.  The routine also calls the makedislocation 
!> routine to create the partials.
! 
!> @param cell unit cell pointer
!> @param defects defect structure
!> @param inum
!> @param DF_L column edge length
!> @param nx
!> @param ny
!> @param DF_g
!> @param ndl
!> @param dinfo trigger for verbose output
!
!> @date   11/05/13 MDG 1.0 new attempt to replace faulty original routine
!> @date   11/13/13 MDG 1.1 traced error to problem with transformations in defectmodule
!> @date   11/13/13 MDG 1.2 changed SF normal transformation for zpos array computation (to be tested)
!> @date   12/17/13 MDG 1.3 branch from original routine to deal with different ECCI geometry
!> @date   12/18/13 MDG 1.4 debug of stacking fault location array
!> @date   06/09/14 MDG 2.0 added cell, SF, YD arguments
!> @date   06/10/14 MDG 2.1 added foil argument
!--------------------------------------------------------------------------
recursive subroutine makestackingfaultECCI(cell,defects,inum,DF_L,nx,ny,DF_g,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: makestackingfaultECCI

use math
use constants
use files
use rotations
use crystal
use symmetry
use math

IMPLICIT NONE

type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: DF_L
real(kind=sgl),INTENT(IN)       :: DF_g(3)
integer(kind=irg),INTENT(IN)    :: inum, nx, ny, dinfo

real(kind=sgl)                  :: fpn(3),am(4,4),midpoint(3), ex(3), ey(3),&
                                 lptopi(3),lpboti(3),tptopi(3),tpboti(3),det,A(4), xx(4), yy(4), tmp(3), &
                                 planenormal(3), rzero(3), unita(3), lun(3), tun(3), zang, zu, zz

integer(kind=irg)               :: i,j,info,ipiv,minx,maxx,miny,maxy, ndl

ndl = defects%numYdisl

! we begin by computing the geometry in the foil reference frame, which is the cartesian frame 
! for zero sample tilt;  sample tilts are applied once we known the partial dislocation geometry
call TransSpace(cell,defects%SF(inum)%plane,tmp,'r','c')
call NormVec(cell,tmp,'c')
planenormal =  tmp

call CalcCross(cell, planenormal, (/ 0.0,0.0,1.0 /),  unita, 'c', 'c', 0)
call NormVec(cell,unita,'c')
fpn = planenormal
if (dinfo.eq.1) write (*,*) ' unita should have zero third component ',unita, fpn


! the fault plane goes through the point rzero in the foil top surface
rzero = (/ defects%SF(inum)%id, defects%SF(inum)%jd, sngl(defects%foil%z0)*0.5 /)

! this leads to the location of the partial dislocation intersections, and these must
! be Yoffe dislocations !!!
defects%SF(inum)%lpr = rzero + 0.5*defects%SF(inum)%sep*unita / DF_L
defects%SF(inum)%tpr = rzero - 0.5*defects%SF(inum)%sep*unita / DF_L

if (dinfo.eq.1) write (*,*) 'lpr_i = ',defects%SF(inum)%lpr(1:3)
if (dinfo.eq.1) write (*,*) 'tpr_i = ',defects%SF(inum)%tpr(1:3)


! convert line directions to the Cartesian crystal reference frame
call TransSpace(cell,defects%SF(inum)%lpu,lun,'d','c')
call TransSpace(cell,defects%SF(inum)%tpu,tun,'d','c')
! normalize both vectors
call NormVec(cell,lun,'c')
call NormVec(cell,tun,'c')


! call makeYSHdislocation for each of the partials [must be done in main program]
!  if (.not. allocated(YD)) allocate(defects%YD(3*maxdefects))

  defects%YD(ndl+1)%id = defects%SF(inum)%lpr(1) 
  defects%YD(ndl+1)%jd = defects%SF(inum)%lpr(2)
  defects%YD(ndl+1)%u(1:3) = dble(defects%SF(inum)%lpu(1:3))
  defects%YD(ndl+1)%burg(1:3) = dble(defects%SF(inum)%lpb(1:3))
  defects%YD(ndl+1)%g = DF_g
  defects%YD(ndl+1)%sig = defects%SF(inum)%poisson
  
  defects%YD(ndl+2)%id = defects%SF(inum)%tpr(1) 
  defects%YD(ndl+2)%jd = defects%SF(inum)%tpr(2)
  defects%YD(ndl+2)%u(1:3) = dble(defects%SF(inum)%tpu(1:3))
  defects%YD(ndl+2)%burg(1:3) = dble(defects%SF(inum)%tpb(1:3))
  defects%YD(ndl+2)%g = DF_g
  defects%YD(ndl+2)%sig = defects%SF(inum)%poisson

  call makeYSHdislocation(cell,defects,ndl+1,dinfo,DF_L)
  if (dinfo.eq.1) write (*,*) 'Leading Partial Position ',defects%YD(ndl+1)%id,defects%YD(ndl+1)%jd
  call makeYSHdislocation(cell,defects,ndl+2,dinfo,DF_L)
  if (dinfo.eq.1) write (*,*) 'Trailing Partial Position ',defects%YD(ndl+2)%id,defects%YD(ndl+2)%jd

! first find the length of the dislocation line inside the foil along the
! dislocation z-axis, which is the line direction; also, compute the intersection
! points of the line with the top and bottom surfaces  (all components in [nm])
zang = CalcAngle(cell, defects%YD(ndl+1)%u,defects%foil%F,'d')
zz = cos(zang)
if (abs(zz).gt.0.00001) then 
  zu = abs(defects%foil%z0/zz)
else
  zu = 100000.0         ! this is when the dislocation is nearly parallel to the foil
end if

! transform the line direction to the foil reference frame
  tmp = quat_Lp( conjg(defects%foil%a_fc), dble(lun) ) / DF_L

! determine the top and bottom intersection coordinates 
  defects%YD(ndl+1)%top = (/ defects%YD(ndl+1)%id, defects%YD(ndl+1)%jd, defects%foil%z0*0.5 /)
  if (zz.gt.0.0) then  ! u points to the top of the foil
    defects%YD(ndl+1)%bottom = (/ defects%YD(ndl+1)%id - tmp(1)*zu, defects%YD(ndl+1)%jd - tmp(2)*zu, -0.5D0*defects%foil%z0 /)
  else                 ! u points to the bottom of the foil
    defects%YD(ndl+1)%bottom = (/ defects%YD(ndl+1)%id + tmp(1)*zu, defects%YD(ndl+1)%jd + tmp(2)*zu, -0.5D0*defects%foil%z0 /)
  end if  


! first find the length of the dislocation line inside the foil along the
! dislocation z-axis, which is the line direction; also, compute the intersection
! points of the line with the top and bottom surfaces  (all components in [nm])
zang = CalcAngle(cell, defects%YD(ndl+2)%u,defects%foil%F,'d')
zz = cos(zang)
if (abs(zz).gt.0.00001) then 
  zu = abs(defects%foil%z0/zz)
else
  zu = 100000.0         ! this is when the dislocation is nearly parallel to the foil
end if
! transform the line direction to the foil reference frame
  tmp = quat_Lp( conjg(defects%foil%a_fc), dble(tun) ) / DF_L

! determine the top and bottom intersection coordinates 
  defects%YD(ndl+2)%top = (/ defects%YD(ndl+2)%id, defects%YD(ndl+2)%jd, defects%foil%z0*0.5 /)
  if (zz.gt.0.0) then  ! u points to the top of the foil
    defects%YD(ndl+2)%bottom = (/ defects%YD(ndl+2)%id - tmp(1)*zu, defects%YD(ndl+2)%jd - tmp(2)*zu, -0.5D0*defects%foil%z0 /)
  else                 ! u points to the bottom of the foil
    defects%YD(ndl+2)%bottom = (/ defects%YD(ndl+2)%id + tmp(1)*zu, defects%YD(ndl+2)%jd + tmp(2)*zu, -0.5D0*defects%foil%z0 /)
  end if  


! copy the top and bottom dislocation intersections
! into the corresponding variables of the SF record

defects%SF(inum)%lpbot = defects%YD(ndl+1)%bottom
defects%SF(inum)%lptop = defects%YD(ndl+1)%top
defects%SF(inum)%tpbot = defects%YD(ndl+2)%bottom
defects%SF(inum)%tptop = defects%YD(ndl+2)%top

! obviously, these four points need to lie in a single plane; at this point, we check that this is indeed the case
! by computing the volume of the tetrahedron formed by these four points; if the volume is zero, then the 
! points are co-planar.  (Use LAPACK's LU-decomposition and compute the product of the diagonal elements of U)
am(1:4,1) = (/ defects%SF(inum)%lptop(1:3),1.0 /)
am(1:4,2) = (/ defects%SF(inum)%lpbot(1:3),1.0 /) 
am(1:4,3) = (/ defects%SF(inum)%tptop(1:3),1.0 /) 
am(1:4,4) = (/ defects%SF(inum)%tpbot(1:3),1.0 /) 
call sgetrf(4,4,am,4,ipiv,info)
det = abs(am(1,1)*am(2,2)*am(3,3)*am(4,4))
if (dinfo.eq.1) write (*,*) 'determinant (should be zero) = ',det

! ok, next we need to figure out which image pixels lie on the projection of the stacking fault plane.
! We need to transform the corner points into the image reference frame !!!
lptopi = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%lptop))
lpboti = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%lpbot))
tptopi = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%tptop))
tpboti = quat_Lp( defects%foil%a_fi, dble(defects%SF(inum)%tpbot))
if (dinfo.eq.1) then
  write (*,*) 'SF parameters :'
  write (*,*) lptopi,' <> ',lpboti
  write (*,*) tptopi,' <> ',tpboti
end if

! define the array that will contain the zpos values
allocate(defects%SF(inum)%zpos(nx,ny))
defects%SF(inum)%zpos = -10000.0    ! set all points to be outside the projected SF

! first determine the smaller box
minx = nint(min( lptopi(1),lpboti(1),tptopi(1),tpboti(1) )) -2
maxx = nint(max( lptopi(1),lpboti(1),tptopi(1),tpboti(1) )) +2
miny = nint(min( lptopi(2),lpboti(2),tptopi(2),tpboti(2) )) -2
maxy = nint(max( lptopi(2),lpboti(2),tptopi(2),tpboti(2) )) +2

! the fault edges may fall outside of the viewing frame (origin at the center !!!)
if (minx.lt.(-nx/2+1)) minx=-nx/2+1
if (maxx.gt.nx/2) maxx=nx/2
if (miny.lt.(-ny/2+1)) miny=-ny/2+1
if (maxy.gt.ny/2) maxy=ny/2

if (dinfo.eq.1) write (*,*) 'Integer fault box = ',minx,maxx,miny,maxy

! get the equation of the stacking fault plane in the image reference frame
! first the unit plane normal in image space
! we'll take two vectors: ex = from ltop to ttop; ey = from ltop to lbot
ex = tptopi - lptopi
ey = lpboti - lptopi
call NormVec(cell,ex,'c')
call NormVec(cell,ey,'c')
call CalcCross(cell,ex,ey,fpn,'c','c',0)

A(1:3) = fpn ! quat_LPstar( conjg(foil%a_fi), dble(fpn(1:3)) )
midpoint = 0.25*(lptopi+lpboti+tptopi+tpboti) ! quat_LPstar( conjg(foil%a_fi), dble(0.25*(lptopi+lpboti+tptopi+tpboti) ))
A(4) = sum(A(1:3)*midpoint)
if (dinfo.eq.1) write (*,*) 'fault plane parameters : ',A, midpoint

! rank the corner points so that the polygon is convex
! call rank_points(tpboti(1:2),lpboti(1:2),lptopi(1:2),tptopi(1:2),xx,yy)
xx = (/ lptopi(1), tptopi(1), tpboti(1),lpboti(1) /)
yy = (/ lptopi(2), tptopi(2), tpboti(2),lpboti(2) /)

! for all of the points inside this box:
do i=minx,maxx
  do j=miny,maxy
    if (point_inside_polygon( float(i), float(j), xx, yy ).gt.0) then 
! the point lies inside the projected region, 
! so we need the depth of the SF plane at this position, taking into account the 
! proper coordinate transformation (depth must be expressed in image reference frame)
        defects%SF(inum)%zpos(i+nx/2,j+ny/2) = ( A(4) - A(1)*float(i) - A(2)*float(j) )/A(3)
    end if
  end do
end do
if (dinfo.eq.1) write (*,*) 'fault plane pixels determined'

! let's also make sure that the SF displacement vector is translated to the 
! cartesian reference frame, so that it can be used directly by the CalcR routine
defects%SF(inum)%lpbc = matmul(cell%dsm,defects%SF(inum)%Rdisp)

! that should do it for the stacking fault...  The rest 
! takes place in the CalcR routine.
end subroutine makestackingfaultECCI

!--------------------------------------------------------------------------
!
! FUNCTION: YSHDisp
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  compute the displacement field of an inclined dislocation intersecting the foil surface
!
!> @details compute the displacement field of an inclined dislocation intersecting the top surface of 
!> the foil, taking into account surface relaxations for the isotropic elastic case (cubic only) ... 
!>
!> equations are based on the Shaibani&Hazzledine 1981 paper, along with special limits for 
!> the alpha->0 case, which were derived by MDG using Mathematica. 
!
!> @paraqm defects defects structure
!> @param x dislocation x-coordinate
!> @param y dislocation y-coordinate
!> @param z dislocation z-coordinate
!> @param ii dislocation number
!
!> @todo There is a problem with dislocations normal to the foil surface, likely a typographical error
!> in the SH paper; this needs to be resolved further, which may require explicit repetition of all 
!> analytical computations! Mathematica gives an infinite limit for the bx edge case when normal
!> to the foil surface.
! 
!> @date    1/5/99  MDG 1.0 original
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   06/04/13 MDG 3.0 rewrite
!> @date   11/21/13 MDG 3.1 verification
!> @date   06/09/14 MDG 4.0 added defects as argument
!--------------------------------------------------------------------------
recursive function YSHDisp(defects,x,y,z,ii) result(res)
!DEC$ ATTRIBUTES DLLEXPORT :: YSHDisp

use constants

IMPLICIT NONE

type(defecttype),INTENT(INOUT)  :: defects
!f2py intent(in,out) ::  defects
real(kind=dbl),INTENT(IN)       :: x,y,z
integer(kind=irg),INTENT(IN)    :: ii

real(kind=dbl)                  :: eta, zeta, etap, zetap, r, oms, omts, xx, sgn, om, omp, AA, BB, BBp, th, &
                                 k, lam, alA, alB, u, v, w, ms, S, du, dv, dw, qe, me, De, qx, mx, Dx, rr, eps
real(kind=dbl)                  :: res(3) 

! initialize the geometrical parameters
eta  = y*defects%YD(ii)%ca - z*defects%YD(ii)%sa
zeta = y*defects%YD(ii)%sa + z*defects%YD(ii)%ca
etap  = -y*defects%YD(ii)%ca - z*defects%YD(ii)%sa
zetap = y*defects%YD(ii)%sa - z*defects%YD(ii)%ca
r = sqrt(x**2+y**2+z**2) 
oms = 1.D0-defects%YD(ii)%sig
omts = 1.D0-2.D0*defects%YD(ii)%sig

! cover the special case of negative x values (based on IDL tests)
xx = x
sgn = 1.D0
if (xx.lt.0.D0) then
  xx = dabs(x)
  sgn = -1.D0
else 
  sgn = 1.D0
end if

! more parameters
om =  (datan2(y,xx)-datan2(eta,xx)+datan2(xx*r*defects%YD(ii)%sa,eta*y+xx**2*defects%YD(ii)%ca))
omp= (datan2(y,xx)-datan2(etap,xx)+datan2(xx*r*defects%YD(ii)%sa,etap*y-xx**2*defects%YD(ii)%ca))

AA = r-z
BB  = r-zeta
BBp = r-zetap 
th = 2.D0*oms*(omp-om)
lam = omts*dlog(BBp/BB)
alA = dlog(AA)
alB = dlog(BB)


u = 0.D0
v = 0.D0
w = 0.D0
eps = 1.0D-6

! screw component first
if (abs(defects%YD(ii)%bs).gt.eps) then 
  ms = xx*sin(2.D0*defects%YD(ii)%alpha)/r/BB
  S = defects%YD(ii)%bs/(4.D0*cPi)
  if (defects%YD(ii)%alpha.gt.0.01) then 
    du = xx*ms+2.D0*eta*defects%YD(ii)%ca**2/BB+2.D0*omts*defects%YD(ii)%cota*(-1.D0+defects%YD(ii)%ca+&
            defects%YD(ii)%ca*alA-y*defects%YD(ii)%sa/AA-alB)-sin(2.D0*defects%YD(ii)%alpha)
    dv = y*ms-2.D0*xx*defects%YD(ii)%ca/BB-defects%YD(ii)%sa*(omp-om)+2.D0*omts*defects%YD(ii)%cota* &
         (xx*defects%YD(ii)%sa/AA-om*defects%YD(ii)%ca)
    dw = z*ms+defects%YD(ii)%ca*(omp-om)-2.D0*omts*om*defects%YD(ii)%ca
  else 
    du = 2.D0*y/(r-z)
    dv = -2.D0*xx*(r+z)/(xx**2+y**2)
    dw = cPi + datan2(y,xx) - datan2(-y,xx)
  end if
  u = u+du*S
  v = v-sgn*dv*S
  w = w+sgn*dw*S
end if

! then the edge component in the y-z plane
if (abs(defects%YD(ii)%be).gt.eps) then 
  qe = xx*(1.D0/BBp-1.D0/BB+2.D0*z*defects%YD(ii)%ca/BB**2)
  me = -qe/r-4.D0*oms*xx*defects%YD(ii)%ca**2/r/BB
  De = defects%YD(ii)%be/(8.D0*cPi*oms)
  if (defects%YD(ii)%alpha.gt.0.01) then 
    k = 4.D0*oms*omts*defects%YD(ii)%cota**2
    du = xx*me+lam+2.D0*defects%YD(ii)%ca*(z+2.D0*oms*eta*defects%YD(ii)%sa)/BB-4.D0*oms*defects%YD(ii)%sa**2+&
           k*(1.D0-defects%YD(ii)%ca-defects%YD(ii)%ca*alA+y*defects%YD(ii)%sa/AA+alB)
    dv = y*me+qe*defects%YD(ii)%sa+th*defects%YD(ii)%ca+k*(-xx*defects%YD(ii)%sa/AA+om*defects%YD(ii)%ca)
    dw = z*me+qe*defects%YD(ii)%ca+th*defects%YD(ii)%sa-2.D0*xx*defects%YD(ii)%ca*(1.D0/BBp+omts/BB)+k*om*defects%YD(ii)%sa
!    write (*,*) du,dv,dw
  else 
    rr = xx**2+y**2
    du = 2.D0*z/(r-z)+4.D0*xx**2*(defects%YD(ii)%sig*rr-r**2)/r/AA**2/(r+z)+2.D0*omts*oms*((xx**2+z*(z-r))/AA**2+alA)+&
         omts*dlog((r+z)/AA)
    dv = 4.D0*xx*y*(rr*defects%YD(ii)%sig-r**2)/r/AA**2/(r+z)+2.D0*xx*y*(rr+2.D0*z*(r+z))*oms*omts/rr**2+&
            2.D0*oms*(cPi + datan2(y,xx) - datan2(-y,xx))
    dw = 4.D0*xx*rr*defects%YD(ii)%sig*(z-2.D0*r*oms)/r/AA**2/(r+z)
  end if
  u = u+du*De
  v = v+sgn*dv*De
  w = w+sgn*dw*De
end if

! and finally the bx edge component
if (abs(defects%YD(ii)%bx).gt.eps) then 
  qx = etap/BBp-eta/BB-2.D0*z*eta*defects%YD(ii)%ca/BB**2
  mx = -qx/r+2.D0*omts*y*defects%YD(ii)%ca/r/BB
  Dx = defects%YD(ii)%bx/(8.D0*cPi*oms)
  if (defects%YD(ii)%alpha.gt.0.01) then 
    k = 4.D0*oms*omts*defects%YD(ii)%cota**2
    du = xx*mx+th+k*(xx*defects%YD(ii)%ta/AA-om)
    dv = y*mx+qx*defects%YD(ii)%sa-lam*defects%YD(ii)%ca-2.D0*defects%YD(ii)%ca*&
        (z*defects%YD(ii)%ca+omts*y*defects%YD(ii)%sa)/BB+ &
        k*(-1.D0+defects%YD(ii)%ca-alA+y*defects%YD(ii)%ta/AA+defects%YD(ii)%ca*alB)
    dw = z*mx+qx*defects%YD(ii)%ca-lam*defects%YD(ii)%sa-2.D0*etap*defects%YD(ii)%ca/BBp+&
        4.D0*defects%YD(ii)%ca*(oms*y*defects%YD(ii)%ca-omts*z*defects%YD(ii)%sa)/BB+ &
        k*defects%YD(ii)%ta*(defects%YD(ii)%ca-alA+defects%YD(ii)%ca*alB)+4.D0*oms*defects%YD(ii)%ca*defects%YD(ii)%cota
 else 
    rr = xx**2+y**2
    du = -4.D0*xx*y*(rr*defects%YD(ii)%sig-r**2)/r/AA**2/(r+z)-2.D0*xx*y*(rr+2.D0*z*(r+z))*oms*omts/rr**2+&
            2.D0*oms*(cPi + datan2(y,xx) - datan2(-y,xx))
    dv = 2.D0*z/(r-z)-4.D0*y**2*(defects%YD(ii)%sig*rr-r**2)/r/AA**2/(r+z)+2.D0*omts*oms*(-1.D0+(z*(r-z)-y**2)/AA**2-alA)- &
            omts*dlog((r+z)/AA)
    dw = 0.D0  ! not sure if this limit is correct ... Mathematica gives a directedinfinity value for the limit, which might 
               ! mean that the original YSH expression in the paper is incorrect for the w component ... this needs to be 
               ! rederived and verified !!!
  end if

  u = u+sgn*du*Dx
  v = v+dv*Dx
  w = w+dw*Dx
end if

! and return the displacement components
res = (/ u,v,w /)

end function YSHDisp


!--------------------------------------------------------------------------
!
! FUNCTION: makeYSHdislocation
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief pre-compute geometrical parametersf or the Yoffe&Shaibani&Hazzledine (YSH) 
!> surface-relaxed dislocation in an elastically isotropic matrix. 
!
!> @details These parameters are then used in the CalcR routine.
!>
!> We implemented the YSH expressions instead of Yoffe's 
!> since the former are more easily handled for numerical computations.
!>
!> SH have redefined the x-y-z reference frame used by Yoffe to fall along
!> the dislocation line itself.  As a result, the Burgers vector must be decomposed
!> into a screw component and two edge components, one in the plane of the 
!> discontinuity, the other normal to that plane (which is by definition the x-axis).
!> Check the SH paper for more details.
!
!> @param cell unit cell pointer
!> @param defects defects structure
!> @param i dislocation number
!> @param dinfo triggers verbose output
!> @param L column edge length
!
!> @todo Convert IO to Write_Value calls
! 
!> @date  1/5/99  MDG 1.0 original
!> @date  5/19/01 MDG 2.0 f90 version
!> @date 11/27/01 MDG 2.1 added kind support
!> @date 06/04/13 MDG 3.0 rewrite+added quaternions
!> @date 11/21/13 MDG 3.1 verification + rewrite of output handling
!> @date 06/09/14 MDG 4.0 added cell and defects arguments
!> @date 06/10/14 MDG 4.1 added foil argument
!--------------------------------------------------------------------------
recursive subroutine makeYSHdislocation(cell,defects,i,dinfo, L)    
!DEC$ ATTRIBUTES DLLEXPORT :: makeYSHdislocation

use constants
use crystal
use io
use error
use rotations

IMPLICIT NONE

type(unitcell)                      :: cell
type(defecttype),INTENT(INOUT)      :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)        :: i
integer(kind=irg),INTENT(IN)        :: dinfo
real(kind=sgl),INTENT(IN)           :: L

real(kind=dbl)                       :: alpha, beta, tu(3), tx(3), ty(3), te(3), tb(3), bl, fx(3), fy(3), fz(3), &
                                       dx, dy, a_di(3,3), io_real(3)

! first, determine the alpha angle between the 
! negative z-axis, which is really the negative foil normal, and the line direction
! (make sure to reduce the angle to [0,90] interval).
! Each YSH dislocation must have a line direction that points INTO the foil !!!
alpha = CalcAngle(cell,defects%foil%F,dble(defects%YD(i)%u),'d')*180.0/cPi
if (alpha.ge.90.0) then 
  alpha = 180.0-alpha
  if (dinfo.eq.1) then 
    io_real(1:3) = defects%foil%F(1:3)
    call WriteValue('Foil normal = ', io_real, 3, "('[',3F5.1,']')")
    io_real(1:3) = defects%YD(i)%u(1:3)
    call WriteValue('line direction = ', io_real, 3, "('[',3F5.1,']')")
    io_real(1) = alpha
    call WriteValue(' --> alpha angle = ', io_real, 1, "(F5.1)")
  end if
  alpha = alpha*cPi/180.0
else 
  call FatalError('makeYSHdislocation','YSH dislocations must have line directions pointing into the foil ! ')
end if

! normalize the line direction
call TransSpace(cell,defects%YD(i)%u,tu,'d','c')
call NormVec(cell,tu,'c')

! consider the case of alpha=0 separately
if (alpha.gt.0.0) then
  call TransSpace(cell,defects%foil%F,ty,'d','c')
  call NormVec(cell,ty,'c')                     !  F
  call CalcCross(cell,tu,ty,tx,'c','c',0)       ! x = u x F
  call NormVec(cell,tx,'c')
  call CalcCross(cell,tx,tu,te,'c','c',0)       ! e = x x u
  call NormVec(cell,te,'c')
  call CalcCross(cell,ty,tx,ty,'c','c',0)
  call NormVec(cell,ty,'c')
else
  tx = defects%foil%qn
  call CalcCross(cell,tx,tu,te,'c','c',0)       ! e = x x u
  call NormVec(cell,te,'c')
end if  
bl = CalcLength(cell,defects%YD(i)%burg,'d')

if (dinfo.eq.1) then 
  io_real(1:3) = tx(1:3)
  call WriteValue(' tx = ',io_real, 3, "(3F5.1)")
  io_real(1:3) = te(1:3)
  call WriteValue(' te = ',io_real, 3, "(3F5.1)")
  io_real(1:3) = tu(1:3)
  call WriteValue(' tu = ',io_real, 3, "(3F5.1)")
  io_real(1:3) = ty(1:3)
  call WriteValue(' ty = ',io_real, 3, "(3F5.1)")
  io_real(1) = bl
  call WriteValue(' bl = ',io_real, 1, "(F8.3)")
end if

call TransSpace(cell,defects%YD(i)%burg,tb,'d','c')
call NormVec(cell,tb,'c')
defects%YD(i)%bx = bl * CalcDot(cell,tb,tx,'c')   ! edge component normal to cut plane
defects%YD(i)%be = bl * CalcDot(cell,tb,te,'c')   ! edge component in cut plane
defects%YD(i)%bs = bl * CalcDot(cell,tb,tu,'c')   ! screw component

if (dinfo.eq.1) then 
  io_real(1:3) = (/ defects%YD(i)%bx,defects%YD(i)%be,defects%YD(i)%bs /)
  call WriteValue('Burgers vector components (bx,be,bs) ', io_real, 3, "(3F12.6)") 
end if
! verified MDG 7/31/11


! we will also need to know the quaternion rotation between the dislocation reference frame 
! and the foil reference frame, so that we can transform the foil coordinates to defect 
! coordinates...  We need the angle beta between the defect x axis (tx) and the foil x axis,
! which is the first column of the foil%a_fc matrix ...   We must make sure that this angle
! is measured in a CCW sense.

! projection of defect x axis onto foil x and y axes
call TransSpace(cell,defects%foil%q,fx,'d','c')
call TransSpace(cell,defects%foil%F,fz,'d','c')
call NormVec(cell,fx,'c')
call NormVec(cell,fz,'c')
call CalcCross(cell,fz,fx,fy,'c','c',0)
dx = CalcDot(cell,tx,fx,'c')
dy = CalcDot(cell,tx,fy,'c')

! use the arctan function to get the angle with correct quadrant computation
defects%YD(i)%beta = atan2(dy,dx) !+ cPi*0.5

if (dinfo.eq.1) then 
  io_real(1) = dx
  call WriteValue(' dx = ', io_real, 1, "(F8.3)")
  io_real(1) = dy
  call WriteValue(' dy = ', io_real, 1, "(F8.3)")
  io_real(1) = defects%YD(i)%beta
  call WriteValue(' beta = ', io_real, 1, "(F8.3)")
end if

! convert to a quaternion
beta = defects%YD(i)%beta
a_di(1,1:3) = (/ cos(beta), sin(beta), 0.D0 /)
a_di(2,1:3) = (/ -sin(beta), cos(beta), 0.D0 /)
a_di(3,1:3) = (/ 0.D0, 0.D0, 1.D0 /)
defects%YD(i)%a_di = om2qu(a_di)
defects%YD(i)%a_id = conjg(defects%YD(i)%a_di)

if (dinfo.eq.1) then 
  write (*,*) 'beta = ',beta
  write (*,*) defects%YD(i)%a_di
  write (*,*) defects%YD(i)%a_id
end if


! finally some geometrical parameters needed for the displacement field computation...
defects%YD(i)%alpha =  alpha
defects%YD(i)%ca = cos(alpha)
defects%YD(i)%sa = sin(alpha)
defects%YD(i)%ta = tan(alpha)
defects%YD(i)%cota = 1.0/defects%YD(i)%ta

! that's it! the rest is handled in the CalcR routine.

end subroutine makeYSHdislocation



!--------------------------------------------------------------------------
!
! FUNCTION: InitializeEshelbyInclusion
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief pre-compute all parameters for an isotropic Eshelby ellipsoidal inclusion
!
!> @details These parameters are then used in the CalcR routine.
!>
!> We implemented the Eshelby expressions based on Mura's 1987 book.
!
!> @param cell unit cell pointer
!> @param defects defects structure
!> @param i dislocation number
!> @param dinfo triggers verbose output
!> @param L column edge length
!
!> @date 12/11/15 MDG 1.0 initial version based on trial IDL script
!> @date 12/13/15 MDG 1.1 corrections to some of the auxiliary expressions; sphere limit is now correct
!--------------------------------------------------------------------------
recursive subroutine InitializeEshelbyInclusion(cell,defects,i,dinfo,L,npix,npiy)    
!DEC$ ATTRIBUTES DLLEXPORT :: InitalizeEshelbyInclusion

use local
use constants
use typedefs
use math

IMPLICIT NONE

type(unitcell)                      :: cell
type(defecttype),INTENT(INOUT)      :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)        :: i
integer(kind=irg),INTENT(IN)        :: dinfo
real(kind=sgl),INTENT(IN)           :: L
integer(kind=irg),INTENT(IN)        :: npix
integer(kind=irg),INTENT(IN)        :: npiy

integer(kind=irg)                   :: j, k, ll, ii
complex(kind=dbl)                   :: t, s, q
real(kind=dbl)                      :: rsq, v, w, modt, lambda, II1, II3, EF, EE, Delta, dth
real(kind=dbl)                      :: r2(3), IIinside(3), IIJinside(3,3), maxr(3), ES(3,3,3,3), ESV(6,6)
real(kind=dbl),allocatable          :: z(:)

! set the size of the elliptic integral lookup tables
defects%Einclusions(i)%nLUT = 500

! copy the defect position
defects%Einclusions(i)%xpos = defects%Einclusions(i)%xyz(1) * 0.5 * float(npix)
defects%Einclusions(i)%ypos = defects%Einclusions(i)%xyz(2) * 0.5 * float(npiy)
defects%Einclusions(i)%zpos = defects%Einclusions(i)%xyz(3) * defects%foil%z0

! copy the semi-axis into the individual variables
defects%Einclusions(i)%a1 = defects%Einclusions(i)%a123(1)
defects%Einclusions(i)%a2 = defects%Einclusions(i)%a123(2)
defects%Einclusions(i)%a3 = defects%Einclusions(i)%a123(3)

! rearrange the ellipsoid semi-axes from largest to smallest and initialize the 
! corresponding permutation matrix as well as the rotation matrix based on the 
! eigenvectors...
! to be implemented

! determine the maximum vector length for the computation of theta(lambda)
maxr = (/ 512.D0, 512.D0, 100.D0 /)
! to be corrected ...

! elastic parameters
defects%Einclusions(i)%omnu = 1.D0-2.D0*defects%Einclusions(i)%nu
defects%Einclusions(i)%pre = 1.D0/(8.D0*cPi*(1.D0-defects%Einclusions(i)%nu))

! volume
defects%Einclusions(i)%V = 4.D0*cPi*defects%Einclusions(i)%a1*defects%Einclusions(i)%a2*defects%Einclusions(i)%a3/3.D0

! and some other derived constants
defects%Einclusions(i)%a12 = defects%Einclusions(i)%a1**2
defects%Einclusions(i)%a22 = defects%Einclusions(i)%a2**2
defects%Einclusions(i)%a32 = defects%Einclusions(i)%a3**2
defects%Einclusions(i)%asq = (/ defects%Einclusions(i)%a12, defects%Einclusions(i)%a22, defects%Einclusions(i)%a32 /)
defects%Einclusions(i)%eta = defects%Einclusions(i)%a12+defects%Einclusions(i)%a22+defects%Einclusions(i)%a32

! used in s variable (to compute lambda)
defects%Einclusions(i)%ss1  = 3.D0*(defects%Einclusions(i)%a12*defects%Einclusions(i)%a22 + &
                                    defects%Einclusions(i)%a22*defects%Einclusions(i)%a32 + &
                                    defects%Einclusions(i)%a32*defects%Einclusions(i)%a12)
defects%Einclusions(i)%svec = 3.D0*(/ defects%Einclusions(i)%eta-defects%Einclusions(i)%a12, &
                                    defects%Einclusions(i)%eta-defects%Einclusions(i)%a22, &
                                    defects%Einclusions(i)%eta-defects%Einclusions(i)%a32 /)

! used in q variable (to compute lambda)
defects%Einclusions(i)%qs1 = (defects%Einclusions(i)%eta-3.D0*defects%Einclusions(i)%a12)* &
                             (defects%Einclusions(i)%eta-3.D0*defects%Einclusions(i)%a22)* &
                             (defects%Einclusions(i)%eta-3.D0*defects%Einclusions(i)%a32)
defects%Einclusions(i)%qvec1 = 9.D0*(/defects%Einclusions(i)%a12, defects%Einclusions(i)%a22, defects%Einclusions(i)%a32/)
defects%Einclusions(i)%qvec2 = 9.D0*(/defects%Einclusions(i)%a12**2+2.D0*defects%Einclusions(i)%a22*defects%Einclusions(i)%a32,&
                                      defects%Einclusions(i)%a22**2+2.D0*defects%Einclusions(i)%a12*defects%Einclusions(i)%a32,&
                                      defects%Einclusions(i)%a32**2+2.D0*defects%Einclusions(i)%a12*defects%Einclusions(i)%a22/)

! Delta matrix  (we'll symmetrize it)
defects%Einclusions(i)%Deltaij  = 0.D0
defects%Einclusions(i)%Deltaij(1,2) = defects%Einclusions(i)%a12-defects%Einclusions(i)%a22
defects%Einclusions(i)%Deltaij(1,3) = defects%Einclusions(i)%a12-defects%Einclusions(i)%a32
defects%Einclusions(i)%Deltaij(2,3) = defects%Einclusions(i)%a22-defects%Einclusions(i)%a32
defects%Einclusions(i)%Deltaij(2,1) = defects%Einclusions(i)%Deltaij(1,2)
defects%Einclusions(i)%Deltaij(3,1) = defects%Einclusions(i)%Deltaij(1,3)
defects%Einclusions(i)%Deltaij(3,2) = defects%Einclusions(i)%Deltaij(2,3)
defects%Einclusions(i)%Deltaij = dsqrt(defects%Einclusions(i)%Deltaij)

! second argument of Elliptic functions
defects%Einclusions(i)%kEl = defects%Einclusions(i)%Deltaij(1,2)/defects%Einclusions(i)%Deltaij(1,3)

! some other prefactors
defects%Einclusions(i)%preI1 = 3.D0* &
                        defects%Einclusions(i)%V/defects%Einclusions(i)%Deltaij(1,2)**2/defects%Einclusions(i)%Deltaij(1,3)
defects%Einclusions(i)%preI3 = 3.D0* &
                        defects%Einclusions(i)%V/defects%Einclusions(i)%Deltaij(2,3)**2/defects%Einclusions(i)%Deltaij(1,3)

defects%Einclusions(i)%s3 = dsqrt(3.D0)
defects%Einclusions(i)%c1 = 2.D0**(1.D0/3.D0)
defects%Einclusions(i)%c2 = 6.D0*2.D0**(2.D0/3.D0)

defects%Einclusions(i)%math = dasin(dsqrt(defects%Einclusions(i)%Deltaij(1,3)**2/defects%Einclusions(i)%a12))

! next we need the largest possible lambda value; we'll have the user pass in the coordinates of the furthest point
r2 = maxr**2
rsq = sum(r2)
s = cmplx( (rsq - defects%Einclusions(i)%eta)**2 + sum(defects%Einclusions(i)%svec*r2)-defects%Einclusions(i)%ss1, 0.D0 )
q = cmplx( -2.D0*rsq**3 - 3.D0*defects%Einclusions(i)%eta*rsq**2 + rsq*(3.D0*defects%Einclusions(i)%eta**2+ &
          sum(defects%Einclusions(i)%qvec1*r2))-sum(defects%Einclusions(i)%qvec2*r2)-defects%Einclusions(i)%qs1, 0.D0 )
t = (q + sqrt( q*q - 4.D0*s*s*s) )**(1.D0/3.D0)
v = real(t)
w = abs(aimag(t))
modt = v**2+w**2
lambda = (rsq-defects%Einclusions(i)%eta)/3.D0 + &
         (v+defects%Einclusions(i)%s3*w)*(2.D0*real(s)+defects%Einclusions(i)%c1*modt)/defects%Einclusions(i)%c2/modt

defects%Einclusions(i)%mith = dasin(defects%Einclusions(i)%Deltaij(1,3)/dsqrt(defects%Einclusions(i)%a12+lambda))

! pre-compute the look-up tables for the elliptic integrals
allocate(z(defects%Einclusions(i)%nLUT))
dth = (defects%Einclusions(i)%math-defects%Einclusions(i)%mith)/dble(defects%Einclusions(i)%nLUT-1)

do j=1,defects%Einclusions(i)%nLUT
  z(j) = defects%Einclusions(i)%mith + dble(j-1) * dth
end do

allocate(defects%Einclusions(i)%EFLUT(defects%Einclusions(i)%nLUT))
allocate(defects%Einclusions(i)%EELUT(defects%Einclusions(i)%nLUT))

defects%Einclusions(i)%EFLUT = 0.D0
defects%Einclusions(i)%EELUT = 0.D0
do j=1,defects%Einclusions(i)%nLUT  
  defects%Einclusions(i)%EFLUT(j) = el1k(z(j),defects%Einclusions(i)%kEl)
  defects%Einclusions(i)%EELUT(j) = el2k(z(j),defects%Einclusions(i)%kEl)
end do
defects%Einclusions(i)%thpre = dble(defects%Einclusions(i)%nLUT-1) / (defects%Einclusions(i)%math-defects%Einclusions(i)%mith)
deallocate(z)

! also, when lambda=0 (inside the ellipsoid), we can simplify things a little
! by using precomputed II and IIJ arrays; in that case we use math to compute
! the integrals
EF = defects%Einclusions(i)%EFLUT(defects%Einclusions(i)%nLUT)
EE = defects%Einclusions(i)%EELUT(defects%Einclusions(i)%nLUT)
Delta = defects%Einclusions(i)%a1*defects%Einclusions(i)%a2*defects%Einclusions(i)%a3

! first order integrals
II1 = defects%Einclusions(i)%preI1 * ( EF - EE )
II3 = defects%Einclusions(i)%preI3 * ( defects%Einclusions(i)%Deltaij(1,3)*defects%Einclusions(i)%a22/Delta - EE )
IIinside = (/ II1, 3.D0*defects%Einclusions(i)%V/Delta - II1 - II3, II3 /)
defects%Einclusions(i)%IIinside = IIinside

! second order integrals
IIJinside = 0.D0
IIJinside(1,2) = -(defects%Einclusions(i)%IIinside(1)-defects%Einclusions(i)%IIinside(2))/defects%Einclusions(i)%Deltaij(1,2)**2
IIJinside(2,1) = IIJinside(1,2)
IIJinside(1,3) = -(defects%Einclusions(i)%IIinside(1)-defects%Einclusions(i)%IIinside(3))/defects%Einclusions(i)%Deltaij(1,3)**2
IIJinside(3,1) = IIJinside(1,3)
IIJinside(2,3) = -(defects%Einclusions(i)%IIinside(2)-defects%Einclusions(i)%IIinside(3))/defects%Einclusions(i)%Deltaij(2,3)**2
IIJinside(3,2) = IIJinside(2,3)
IIJinside(1,1) = defects%Einclusions(i)%V/defects%Einclusions(i)%a12/Delta - (IIJinside(1,2)+IIJinside(1,3))/3.D0
IIJinside(2,2) = defects%Einclusions(i)%V/defects%Einclusions(i)%a22/Delta - (IIJinside(2,1)+IIJinside(2,3))/3.D0
IIJinside(3,3) = defects%Einclusions(i)%V/defects%Einclusions(i)%a32/Delta - (IIJinside(3,1)+IIJinside(3,2))/3.D0
defects%Einclusions(i)%IIJinside = IIJinside

! and finally the Eshelby S tensor for inside the inclusion (i.e., lambda=0)
ES = 0.D0
do ii=1,3
 do j=1,3
  do k=1,3
   do ll=1,3
    Es(ii,j,k,ll) = kdelta(ii,j)*kdelta(k,ll)*(2.D0*defects%Einclusions(i)%nu * IIinside(ii)-IIinside(k)+ &
                   defects%Einclusions(i)%a123(ii)**2*IIJinside(k,ii)) + (kdelta(ii,k)*kdelta(j,ll)+kdelta(j,k)*kdelta(ii,ll)) * &
                   (defects%Einclusions(i)%a123(ii)**2*IIJinside(ii,j)-IIinside(j)+(1.D0-defects%Einclusions(i)%nu)*&
                   (IIinside(k)+IIinside(ll)))
   end do
  end do
 end do                   
end do

ES = ES/(8.0*cPi*(1.D0-defects%Einclusions(i)%nu))

defects%Einclusions(i)%EshelbyS = ES

! convert this tensor to Voigt notation as a 6x6 matrix
ESV = 0.D0
ESV(1,1) = ES(1,1,1,1)
ESV(2,2) = ES(2,2,2,2)
ESV(3,3) = ES(3,3,3,3)


ESV(4,4) = ES(2,3,2,3)
ESV(5,5) = ES(3,1,3,1)
ESV(6,6) = ES(1,2,1,2)

ESV(1,2) = ES(1,1,2,2)
ESV(1,3) = ES(1,1,3,3)

ESV(2,1) = ES(2,2,1,1)
ESV(3,1) = ES(3,3,1,1)

ESV(3,2) = ES(3,3,2,2)
ESV(2,3) = ES(2,2,3,3)

defects%Einclusions(i)%ESV = ESV

end subroutine InitializeEshelbyInclusion



!--------------------------------------------------------------------------
!
! FUNCTION: Eshelby_disp
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the actual displacement vector for an isotropic Eshelby ellipsoidal inclusion
!
!> We implemented the Eshelby expressions based on Mura's 1987 book.
!
!> @param defects defects structure
!> @param i Einclusion number
!> @param xyz coordinate triplet
!
!> @date 12/11/15 MDG 1.0 initial version based on trial IDL script
!> @date 12/13/15 MDG 1.1 corrections to some of the auxiliary expressions; sphere limit is now correct
!--------------------------------------------------------------------------
recursive function Eshelby_disp(defects, i, xyz) result(u)
!DEC$ ATTRIBUTES DLLEXPORT :: Eshelby_disp
! 
! implement the displacement field equations for an isotropic ellipsoidal inclusion
!

use local
use math

IMPLICIT NONE

type(defecttype),INTENT(IN)     :: defects
integer(kind=irg),INTENT(IN)    :: i
real(kind=dbl),INTENT(IN)       :: xyz(3)
real(kind=dbl)                  :: u(3)

integer(kind=irg)               :: itheta, j, l, k
real(kind=dbl)                  :: Treps, r2(3), rsq, dd, v, w, modt, lambda, xx, thetaEl, dtheta, sigma, &
                                   II1, II3, II(3), IIJ(3,3), phici(3), psicjli(3,3,3), c, t1, t2, dt(3), Delta, EE, EF
complex(kind=dbl)               :: t, s, q

Treps = defects%Einclusions(i)%epsstar(1,1)+defects%Einclusions(i)%epsstar(2,2)+defects%Einclusions(i)%epsstar(3,3)

r2 = xyz**2 
rsq = sum(r2)
dd = dsqrt(xyz(1)**2/defects%Einclusions(i)%a12+xyz(2)**2/defects%Einclusions(i)%a22+xyz(3)**2/defects%Einclusions(i)%a32)

!  first we need the lambda value for this point
lambda = 0.D0
if (dd.gt.1.D0) then 
  s = cmplx( (rsq - defects%Einclusions(i)%eta)**2 +sum(defects%Einclusions(i)%svec*r2)-defects%Einclusions(i)%ss1, 0.D0 )
  q = cmplx( -2.D0*rsq**3 - 3.D0*defects%Einclusions(i)%eta*rsq**2 + rsq*(3.D0*defects%Einclusions(i)%eta**2+ &
      sum(defects%Einclusions(i)%qvec1*r2))-sum(defects%Einclusions(i)%qvec2*r2)-defects%Einclusions(i)%qs1, 0.D0 )
  t = (q + sqrt( q*q - 4.D0*s*s*s) )**(1.D0/3.D0)
  v = real(t)
  w = abs(aimag(t))
  modt = v**2+w**2
  lambda = (rsq-defects%Einclusions(i)%eta)/3.D0 + (v+defects%Einclusions(i)%s3*w)* &
           (2.D0*real(s)+defects%Einclusions(i)%c1*modt)/defects%Einclusions(i)%c2/modt
end if


! predefine a couple of parameters
dt = (/ defects%Einclusions(i)%a12, defects%Einclusions(i)%a22, defects%Einclusions(i)%a32 /) + lambda
Delta = dsqrt(dt(1)*dt(2)*dt(3))


if (lambda.ne.0.D0) then
! next, we compute the elliptic integrals by interpolating the look-up tables
  thetaEl = dasin(dsqrt(defects%Einclusions(i)%Deltaij(1,3)**2/dt(1)))
  xx = defects%Einclusions(i)%thpre * (thetaEl-defects%Einclusions(i)%mith)
  itheta = int(xx)+1
  dtheta = xx-int(xx)
  if (itheta.lt.defects%Einclusions(i)%nLUT) then
    EF = (1.D0-dtheta)*defects%Einclusions(i)%EFLUT(itheta) + dtheta*defects%Einclusions(i)%EFLUT(itheta+1)
    EE = (1.D0-dtheta)*defects%Einclusions(i)%EELUT(itheta) + dtheta*defects%Einclusions(i)%EELUT(itheta+1)
  else 
    EF = defects%Einclusions(i)%EFLUT(itheta) 
    EE = defects%Einclusions(i)%EELUT(itheta) 
  end if

! first order integrals
  II1 = defects%Einclusions(i)%preI1 * ( EF - EE )
  II3 = defects%Einclusions(i)%preI3 * ( defects%Einclusions(i)%Deltaij(1,3)*dt(2)/Delta - EE )
  II = (/ II1, 3.D0*defects%Einclusions(i)%V/Delta - II1 - II3, II3 /)
! second order integrals
  IIJ(1,2) = - (II(1) - II(2))/defects%Einclusions(i)%Deltaij(1,2)**2
  IIJ(2,1) = IIJ(1,2)
  IIJ(1,3) = - (II(1) - II(3))/defects%Einclusions(i)%Deltaij(1,3)**2
  IIJ(3,1) = IIJ(1,3)
  IIJ(2,3) = - (II(2) - II(3))/defects%Einclusions(i)%Deltaij(2,3)**2
  IIJ(3,2) = IIJ(2,3)
  IIJ(1,1) = defects%Einclusions(i)%V/dt(1)/Delta - (IIJ(1,2)+IIJ(1,3))/3.0
  IIJ(2,2) = defects%Einclusions(i)%V/dt(2)/Delta - (IIJ(2,1)+IIJ(2,3))/3.0
  IIJ(3,3) = defects%Einclusions(i)%V/dt(3)/Delta - (IIJ(3,1)+IIJ(3,2))/3.0
else 
  II = defects%Einclusions(i)%IIinside
  IIJ = defects%Einclusions(i)%IIJinside
end if

! then we need to evaluate the large number of "tensor" components in the phi_{,j}
! and psi_{,jli} arrays, as defined in Muro's 1987 book; we've actually computed the
! derivatives ourselves similar to the computation of eq. (11.40)
phici = (/ -xyz(1)*II(1), -xyz(2)*II(2), -xyz(3)*II(3) /)

c = 0.D0
sigma = (xyz(1)/dt(1))**2 + (xyz(2)/dt(2))**2 + (xyz(3)/dt(3))**2
do j=1,3 
 do l=1,3
  do k=1,3
   if (lambda.ne.0.D0) c = 3.D0*defects%Einclusions(i)%V*lambda*xyz(k)*xyz(j)*xyz(l)/dt(k)/dt(j)/dt(l)/Delta/sigma
   t1 = kdelta(j,l) * xyz(k) * (II(k) - defects%Einclusions(i)%asq(j)*IIJ(j,k))
   t2 = (kdelta(k,j)*xyz(l)+kdelta(k,l)*xyz(j))*(II(l)-defects%Einclusions(i)%asq(j)*IIJ(j,l))
   psicjli(j,l,k) = -t1 - t2 + c
  end do
 end do
end do

do k=1,3
! middle term of Mura eq. (11.30)
  u(k) = -2.0*defects%Einclusions(i)%nu*Treps*phici(k) 
! last term
  do l=1,3
    u(k) = u(k) - 4.D0*(1.D0-defects%Einclusions(i)%nu)*defects%Einclusions(i)%epsstar(k,l)*phici(l)
  end do
! first term
  do j=1,3
    do l=1,3
      u(k) = u(k) + defects%Einclusions(i)%epsstar(j,l) * psicjli(j,l,k)
    end do
  end do
end do
u = defects%Einclusions(i)%pre * u

end function Eshelby_disp






!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! and finally, here we compute the total displacements for an integration column
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcR
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief returns the total displacement vector for each slice in a column
!
!> @details Note that the end result MUST be expressed in the cartesian reference frame !
!
!> @param cell unit cell pointer
!> @param defects defect structure
!> @param i integer x coordinate 
!> @param j integer y coordinate
!
!> @note This entire routine was thoroughly verified after the quaternion conversion !
!>
!> General comment for those who wish to add other defects...
!>
!> the general procedure to implement a defect displacement field is as follows:
!> - if the defect has its own reference frame, then transform (xpos,ypos,zpos) to
!>   that frame (see the dislocation section below for an example), and then do
!>   the computation of the displacement vector and express it in the cartesian frame.
!>
!> - if the defect uses the foil reference frame (e.g., voids, inclusions), then use tmpf
!>  as the current position vector.

!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  10/30/13 MDG 3.1 debug of coordinate rotations
!> @date  11/13/13 MDG 3.2 finally, the bug has been found!  
!> @date  06/09/14 MDG 4.0 introduced defects argument and simplified routine
!> @date  06/10/14 MDG 4.1 added foil argument
!> @date  11/23/15 MDG 4.2 removed foil argument and placed it inside defects
!--------------------------------------------------------------------------
recursive subroutine CalcR(cell,defects,i,j)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcR

use local
use constants
use crystal
use rotations

IMPLICIT NONE

type(unitcell)                          :: cell
type(defecttype),INTENT(INOUT)          :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(IN)            :: i,j

integer(kind=irg)                       :: k, islice, ii
real(kind=dbl)                          :: dis,xpos,ypos,zpos,sumR(3),thick,tmp(3),tmp2(3), &
                                           tmpf(3),u(3),zaamp,zaphase,zar,zai,zr(3),zi(3), &
                                           zt,fx,fy,fz,a_fm(3,3),ar    !,&
!                                nu,x,y,z,zn,t,pre,r1,r2,r3,th,rn 
                                                 
complex(kind=dbl)                       :: za(3)
complex(kind=sgl)                       :: zero
logical                                 :: isvoid


! scale the image coordinates with respect to the origin at the center of the image
 xpos = float(i-defects%DF_npix/2)*defects%DF_L
 ypos = float(j-defects%DF_npiy/2)*defects%DF_L
! determine the starting point of the z-integration for the tilted foil
! this depends on the foil normal components which give the equation
! of the top foil plane as F . r = z0/2, from which we get zt...
 a_fm = qu2om(defects%foil%a_fm)
 fx = a_fm(3,1)
 fy = a_fm(3,2)
 fz = a_fm(3,3) 
 zt = defects%foil%zb*0.5 - (fx*xpos + fy*ypos)/fz

! initialize some other variables
 thick = defects%foil%zb
 zero = cmplx(0.0,0.0)

! loop over all slices (this is the main loop)
 sliceloop: do islice = 1,defects%DF_nums 

! zpos is the position down the column, starting at zt (in image coordinates)
    zpos = zt - float(islice)*defects%DF_slice

! set the displacements to zero
    sumR = 0.0

! set the position in the foil reference frame
    tmpf = quat_Lp( defects%foil%a_fi, dble( (/ xpos, ypos, zpos /)) )


!------------
!----VOIDS---
!------------
! voids are easy to deal with; we simply return -10000 for each point tmpf that lies inside
! one of the voids; the calling routine then knows to use the void scattering matrix.
if (defects%numvoids.ne.0) then 
! are we inside a void ?
    isvoid = .FALSE.
    voidloop: do ii=1,defects%numvoids
! subtract the void position from the current slice position to get the relative position vector
     tmp = tmpf -  (/ defects%voids(ii)%xpos, defects%voids(ii)%ypos, defects%voids(ii)%zpos /)
     dis = CalcLength(cell,tmp,'c')
     if (dis.lt.defects%voids(ii)%radius) then ! inside void
       isvoid = .TRUE.
       exit voidloop
     end if
    end do voidloop
! skip the rest of the computation for this slice if we are inside a void
    if (isvoid.eqv..TRUE.) then 
      defects%DF_R(islice,1) = -10000.0
      cycle sliceloop
    end if
 end if 


! ok, if we get here, then we're not inside a void...

!------------------
!----CURVED FOIL---
!------------------
! first we take the foil shape into account using equations (8.28) and (8.29)
 sumR = sumR + float(islice)*defects%DF_slice*defects%foil%sg(i,j)*defects%DF_gstar

!-----------------
!--DISLOCATIONS--
!-----------------
! let's put a few dislocations in ... (see section 8.4.2)
do ii=1,defects%numdisl
! compute the difference vector between the current (xpos,ypos,zpos) in the foil reference frame
! and the defect center coordinate
  tmp2 =  tmpf - dble((/ defects%DF_L*defects%DL(ii)%id, defects%DF_L*defects%DL(ii)%jd, defects%DL(ii)%zfrac*defects%foil%z0 /))

! then convert the difference vector to the defect reference frame for this dislocation (we will only need the x and y coordinates)
  tmp = quat_Lp( defects%DL(ii)%a_df, tmp2 ) 


! compute x1 + p_alpha x2  (eq. 8.38)
  za(1:3) = tmp(1) + defects%DL(ii)%pa(1:3)*tmp(2)
! compute the displacement vector u (eq. 8.38) [this expands the log of a complex number and takes the real part only,
! taking proper care of the branch cut] 
   if (tmp(1).gt.0.0) then
   do k=1,3
    zar =  real(za(k))
    zai = aimag(za(k))
    zaamp = abs(za(k))
    zaphase = abs(zai/zar)
    zr(k) = log(zaamp)
    zi(k) = atan(zaphase)
    if (zar.le.0.0) then
      if (zai.lt.0.0) zi(k) = -cPi+zi(k)
      if (zai.eq.0.0) zi(k) = cPi
      if (zai.gt.0.0) zi(k) = cPi-zi(k)
    else
      if (zai.lt.0.0) zi(k) = -zi(k)
    end if
   end do
  else
   do k=1,3
    zar =  real(za(k))
    zai = aimag(za(k))
    zaamp = abs(za(k))
    zaphase = abs(zai/zar)
    zr(k) = log(zaamp)
    zi(k) = atan(zaphase)
    if (zar.le.0.0) then
      if (zai.gt.0.0) zi(k) = cPi-zi(k)
      if (zai.eq.0.0) zi(k) = cPi
      if (zai.lt.0.0) zi(k) = cPi+zi(k)
    else
      if (zai.lt.0.0) zi(k) = 2.0*cPi-zi(k)
      if (zai.eq.0.0) zi(k) = 0.0
    end if
   end do  
  end if
  u = 2.0*real(matmul(defects%DL(ii)%dismat,cmplx(zr,zi)))
! transform displacement vector u to the Cartesian crystal reference frame
  u = quat_Lp( conjg(defects%DL(ii)%a_dc), dble(u) )  
  sumR = sumR + u

end do

!-------------------------------------
!--SURFACE INTERSECTING DISLOCATIONS--
!-------------------------------------
! this part is mostly used for ECCI-type image simulations, not for EM or STEM,
! although it could probably be used there as well; we would need to extend it 
! to incorporate both top and bottom foil surfaces

! do we have any dislocations with surface relaxations ?  YSH model
if (defects%numYdisl.gt.0) then 
   do ii=1,defects%numYdisl
! first, figure out what the coordinates are in the YSH reference frame for this dislocation ... 
! translate to the defect origin
     tmp =  tmpf -  (/ defects%DF_L*defects%YD(ii)%id, defects%DF_L*defects%YD(ii)%jd, defects%foil%z0*0.5 /)

! rotate into the defect reference frame
     tmp = quat_Lp( conjg(defects%YD(ii)%a_di), tmp )   

! compute the displacement vector
!     u = sngl(YSHDisp(dble(tmp(2)),-dble(tmp(1)),dble(tmp(3)),ii))
     u = sngl(YSHDisp(defects,dble(tmp(1)),dble(tmp(2)),dble(tmp(3)),ii))

! and rotate back to the image reference frame
     u = quat_Lp( conjg(defects%YD(ii)%a_id), u )
     u = quat_Lp( defects%foil%a_ic, u ) 

! that should do it !
     sumR = sumR + u
   end do
end if

!--------------------
!--STACKING FAULTS--
!--------------------
! stacking faults (this is easy because we've already done all the work in the stacking_fault module)
! all we need is the z-value at which the stacking fault plane is crossed in this particular image
! column; from that point on, we simply add the leading partial Burgers vector to the total displacement.
do ii=1,defects%numsf
  if ((zpos.lt.defects%SF(ii)%zpos(i,j)).and.(defects%SF(ii)%zpos(i,j).ne.-10000.0)) then 
    sumR = sumR + defects%SF(ii)%lpbc
  end if
end do


!--------------------
!--LARGE INCLUSIONS--  currently commented out 
!--------------------
! Mader's expression for the displacement field of a large inclusion
!   if (0.eq.1.) then 
!    nu = 0.25
!    ce = 0.005
!    rn = 25.0*DF_L
!    x = (float(i-DF_npix/2)-0.5)*DF_L
!    y = (float(j-DF_npiy/2)-0.5)*DF_L
!    z = float(k)*DF_slice
!    zn = 100.5*DF_slice
!    t = DF_slice * DF_nums
!    pre = (1.0+nu)/(3.0*(1.0-nu))*ce*rn**3
! 
!    r1 = sqrt(x**2+y**2+(z-zn)**2)
!    r2 = sqrt(x**2+y**2+(z+zn)**2)
!    r3 = sqrt(x**2+y**2+(2.0*t-z-zn)**2)
! 
!    if (((r1.eq.0.0).or.(r2.eq.0.0)).or.(r3.eq.0.0)) then
!      return
!    else
!     dis = (1.0/r1**3+(3.0-4.0*nu)/r2**3-6.0*z*(z+zn)/r2**5+(3.0-4.0*nu)/r3**3-6.0*(t-z)*(2.0*t-z-zn)/r3**5)
!     rx = x*dis
!     ry = y*dis
!     rz = (z-zn)/r1**3-(3.0-4.0*nu)*((z+zn)/r2**3+(2.0*t-z-zn)/r3**3)-6.0*z*(z+zn)**2/r2**5 + &
!          2.0*z/r2**3+6.0*(t-z)*(2.0*t-z-zn)**2/r3**5-2.0*(t-z)/r3**3
! 
!     sumR = pre*(/ rx, ry, rz /)
!     return
!    end if
!   end if

!--------------------
!--SMALL INCLUSIONS--
!--------------------
! then the coherent precipitates, using the model in section 8.4.1
  if (defects%numinc.gt.0) then
   do ii=1,defects%numinc
! subtract the inclusion position from the current slice position to get the relative position vector
     tmp = tmpf - (/ defects%inclusions(ii)%xpos, defects%inclusions(ii)%ypos, defects%inclusions(ii)%zpos /)
     dis = CalcLength(cell,tmp,'c')
     if (dis.ge.defects%inclusions(ii)%radius) then ! outside particle
       tmp = tmp*(defects%inclusions(ii)%radius/dis)**3
     end if
     sumR = sumR + defects%inclusions(ii)%C*tmp
   end do
  end if

!---------------------
!--SMALL EINCLUSIONS--
!---------------------
! then the coherent ellipsoidally distorted precipitates, using Eshelby's model 
   if (defects%numEinc.gt.0) then
    do ii=1,defects%numEinc
! subtract the inclusion position from the current slice position to get the relative position vector
      tmp = tmpf - (/ defects%Einclusions(ii)%xpos, defects%Einclusions(ii)%ypos, defects%Einclusions(ii)%zpos /)
! and also get the position vector for the mirror image inclusion, to make sure we get a traction-free surface...
      tmp2 = tmpf - (/ defects%Einclusions(ii)%xpos, defects%Einclusions(ii)%ypos, -defects%Einclusions(ii)%zpos /)
      u = Eshelby_disp(defects, ii, tmp) + Eshelby_disp(defects, ii, tmp2) 
! we need to check the reference frame here !
      sumR = sumR + u
    end do
   end if

! TO BE IMPLEMENTED FOR RICHARD LESAR'S Discrete Dislocation Dynamics ! 
! finally any displacement fields defined by the user routine UserDisp
! sumR = sumR + UserDisp()


! TO BE IMPLEMENTED FOR YUNZHI WANG's Dislocation Simulations ! 
! finally any displacement fields defined by the user routine UserDisp
! sumR = sumR + UserDisp()        


   defects%DF_R(islice,1:3) = sumR(1:3)

  end do sliceloop ! main loop over the slices

end subroutine CalcR


end module defectmodule
