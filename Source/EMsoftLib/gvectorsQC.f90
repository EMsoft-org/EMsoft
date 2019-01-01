! ###################################################################
! Copyright (c) 2017-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:gvectorsQC.f90
!--------------------------------------------------------------------------
!
! MODULE: gvectorsQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief routines to handle g vector sampling for quasi-crystals 
!> multibeam computations
!
!> @date 06/26/18 SS 1.0 original
!--------------------------------------------------------------------------
module gvectorsQC

use local 
use typedefs
use qcrystal
use diffractionQC
use QCmod

IMPLICIT NONE

public

interface MakeQCRefList
	module procedure Make2DQCRefList
	module procedure Make3DQCRefList
end interface MakeQCRefList

interface AddQCReflection
	module procedure Add2DQCReflection
	module procedure Add3DQCReflection
end interface AddQCReflection

interface Delete_QCgvectorlist
	module procedure Delete_QCgvectorlist
	module procedure Delete_TDQCgvectorlist
end interface Delete_QCgvectorlist

interface QC_Apply_BethePotentials
	module procedure TDQC_Apply_BethePotentials
	module procedure QC_Apply_BethePotentials
end interface QC_Apply_BethePotentials

interface Initialize_QCReflectionList
	module procedure Initialize_TDQCReflectionList
	module procedure Initialize_QCReflectionList
end interface Initialize_QCReflectionList

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: Make2DQCRefList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief allocate and initialize the linked reflection list for a quasi-crystal
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
!> @date  03/15/17 MDG 4.3 copied from gvectors module for QCmod adaptation
!> @date  03/23/18 SS  4.4 adapted from QCmod.f90
!--------------------------------------------------------------------------
recursive subroutine Make2DQCRefList(listroot, rltail, nref)
!DEC$ ATTRIBUTES DLLEXPORT :: Make2DQCRefList

use error

IMPLICIT NONE

type(TDQCreflisttype),pointer       :: listroot 
type(TDQCreflisttype),pointer       :: rltail
integer(kind=irg),INTENT(INOUT)     :: nref

integer(kind=irg)                   :: istat

! create it if it does not already exist
if (.not.associated(listroot)) then
  nref = 0
  allocate(listroot,stat=istat)
  if (istat.ne.0) call FatalError('MakeQCRefList:',' unable to allocate pointer')
  rltail => listroot               ! tail points to new value
  nullify(rltail%next)             ! nullify next in new value
end if

end subroutine Make2DQCRefList

!--------------------------------------------------------------------------
!
! SUBROUTINE: Add2DQCReflection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a reflection to the linked reflection list
!
!> @param rltail QCreflisttype variable
!> @param listroot QCreflisttype variable
!> @param QCcell QC structure pointer
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
!> @date  03/15/17 MDG 4.3 copied from gvectors module for QCmod adaptation
!> @date  03/23/18 SS  4.4 adapted from QCmod.f90
!--------------------------------------------------------------------------
recursive subroutine Add2DQCReflection(rltail,listroot,QCcell,nref,gg)
!DEC$ ATTRIBUTES DLLEXPORT :: Add2DQCReflection

use error

IMPLICIT NONE

type(TDQCreflisttype),pointer     :: rltail
type(TDQCreflisttype),pointer     :: listroot
type(TDQCStructureType),pointer   :: QCcell
integer(kind=irg),INTENT(INOUT)   :: nref
integer(kind=irg),INTENT(IN)      :: gg(5)

integer(kind=irg)                 :: istat, QCindex
complex(kind=dbl)                 :: Ucg, qg
real(kind=dbl)                    :: Vmod, Vpmod, xig, xgp

! create linked list if it does not already exist
 if (.not.associated(rltail)) then
   nullify(rltail)
 end if
 if (.not.associated(listroot)) then
   call MakeQCRefList(listroot,rltail,nref)
 end if

! create a new entry
 allocate(rltail%next,stat=istat)               ! allocate new value
 if (istat.ne.0) call FatalError('AddQCReflection',' unable to add new reflection')

 rltail => rltail%next                          ! tail points to new value
 nullify(rltail%next)                           ! nullify next in new value

 nref         = nref + 1                                ! update reflection counter
 rltail%num   = nref                              ! store reflection number
 rltail%hkl   = gg                                ! store Miller indices
 QCindex 	  = QC_getindex(QCcell, gg)
 rltail%Ucg   = QCcell%LUT(QCindex) !QCcell%LUT(gg(1),gg(2),gg(3),gg(4),gg(5))
 rltail%qg    = QCcell%LUT(QCindex) !QCcell%LUTqg(gg(1),gg(2),gg(3),gg(4),gg(5))
 rltail%glen  = QC_getvectorLength(QCcell, rltail%hkl, 'P', 'r')

 nullify(rltail%nextw)
 nullify(rltail%nexts)
 
end subroutine Add2DQCReflection

!--------------------------------------------------------------------------
!
! SUBROUTINE: Delete_QCgvectorlist
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
recursive subroutine Delete_QCgvectorlist(top)
!DEC$ ATTRIBUTES DLLEXPORT :: Delete_QCgvectorlist

IMPLICIT NONE

type(QCreflisttype),pointer       :: top

type(QCreflisttype),pointer       :: rltail, rltmpa

! deallocate the entire linked list before returning, to prevent memory leaks
rltail => top
rltmpa => rltail % next
do 
  deallocate(rltail)
  if (.not. associated(rltmpa)) EXIT
  rltail => rltmpa
  rltmpa => rltail % next
end do

end subroutine Delete_QCgvectorlist

!--------------------------------------------------------------------------
!
! SUBROUTINE: Delete_TDQCgvectorlist
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief delete the entire linked list
!
!> @param top top of the list to be removed
!
!> @date   05/01/18 SS 1.0 adapted from QCmod.f90
!--------------------------------------------------------------------------
recursive subroutine Delete_TDQCgvectorlist(top)
!DEC$ ATTRIBUTES DLLEXPORT :: Delete_TDQCgvectorlist

IMPLICIT NONE

type(TDQCreflisttype),pointer       :: top

type(TDQCreflisttype),pointer       :: rltail, rltmpa

! deallocate the entire linked list before returning, to prevent memory leaks
rltail => top
rltmpa => rltail % next
do 
  deallocate(rltail)
  if (.not. associated(rltmpa)) EXIT
  rltail => rltmpa
  rltmpa => rltail % next
end do

end subroutine Delete_TDQCgvectorlist

!--------------------------------------------------------------------------
!
! SUBROUTINE: Make3DQCRefList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief allocate and initialize the linked reflection list for a quasi-crystal
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
!> @date  03/15/17 MDG 4.3 copied from gvectors module for QCmod adaptation
!--------------------------------------------------------------------------
recursive subroutine Make3DQCRefList(listroot, rltail, nref)
!DEC$ ATTRIBUTES DLLEXPORT :: Make3DQCRefList

use error

IMPLICIT NONE

type(QCreflisttype),pointer       :: listroot 
type(QCreflisttype),pointer       :: rltail
integer(kind=irg),INTENT(INOUT)   :: nref

integer(kind=irg)  :: istat

! create it if it does not already exist
if (.not.associated(listroot)) then
  nref = 0
  allocate(listroot,stat=istat)
  if (istat.ne.0) call FatalError('MakeQCRefList:',' unable to allocate pointer')
  rltail => listroot               ! tail points to new value
  nullify(rltail%next)             ! nullify next in new value
end if

end subroutine Make3DQCRefList

!--------------------------------------------------------------------------
!
! SUBROUTINE: Add3DQCReflection
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief add a reflection to the linked reflection list
!
!> @param rltail QCreflisttype variable
!> @param listroot QCreflisttype variable
!> @param QCcell QC structure pointer
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
!> @date  03/15/17 MDG 4.3 copied from gvectors module for QCmod adaptation
!--------------------------------------------------------------------------
recursive subroutine Add3DQCReflection(rltail,listroot,QCcell,nref,QCindex,gg)
!DEC$ ATTRIBUTES DLLEXPORT :: Add3DQCReflection

use error

IMPLICIT NONE

type(QCreflisttype),pointer     :: rltail
type(QCreflisttype),pointer     :: listroot
type(QCStructureType),pointer   :: QCcell
integer(kind=irg),INTENT(INOUT) :: nref
integer(kind=irg),INTENT(IN)    :: QCindex               !< QC Miller indices of reflection to be added to list
integer(kind=irg),INTENT(IN)    :: gg(6)

integer(kind=irg)               :: istat
complex(kind=dbl)               :: Ucg, qg
real(kind=dbl)                  :: Vmod, Vpmod, xig, xgp

! create linked list if it does not already exist
 if (.not.associated(rltail)) then
   nullify(rltail)
 end if
 if (.not.associated(listroot)) then
   call MakeQCRefList(listroot,rltail,nref)
 end if

! create a new entry
 allocate(rltail%next,stat=istat)               ! allocate new value
 if (istat.ne.0) call FatalError('AddQCReflection',' unable to add new reflection')

 rltail => rltail%next                          ! tail points to new value
 nullify(rltail%next)                           ! nullify next in new value

 nref 		 = nref + 1                          ! update reflection counter
 rltail%num  = nref                              ! store reflection number
 rltail%hkl  = gg 								 ! store Miller indices
 rltail%Ucg  = QCcell%LUT(QCindex)
 rltail%qg 	 = QCcell%LUTqg(QCindex)
 rltail%glen = QC_getvectorLength(QCcell, rltail%hkl, 'P', 'r')
 nullify(rltail%nextw)
 nullify(rltail%nexts)
 
end subroutine Add3DQCReflection

!--------------------------------------------------------------------------
!
! SUBROUTINE: Initialize_TDQCReflectionList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the potential quasi-crystal reflection list for a given wave vector
!
!> @param QCcell cell pointer
!> @param BetheParameter Bethe potential structure
!> @param FN  foil normal
!> @param k zone axis direction cosines in direct Bravais lattice
!> @param listroot pointer to top of list (could be cell%reflist)
!> @param nref number of reflections in main list (used to be DynNbeams)
!> @param verbose (optional) used for debugging purposes mostly

!> @date 03/15/17 MDG 1.0 original, based on regular Initialize_ReflectionList
!> @date  03/23/18 SS 1.1 adapted from QCmod.f90
!--------------------------------------------------------------------------
recursive subroutine Initialize_TDQCReflectionList(QCcell, listroot, BetheParameter, FN, k, nref, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_TDQCReflectionList

use local
use typedefs
use io
use constants
use diffraction

IMPLICIT NONE

type(TDQCStructureType),pointer                 :: QCcell
type(TDQCreflisttype),pointer                   :: listroot
type(BetheParameterType),INTENT(INOUT)          :: BetheParameter
real(kind=sgl),INTENT(IN)                       :: FN(3)
real(kind=sgl),INTENT(IN)                       :: k(3)
integer(kind=irg),INTENT(INOUT)                 :: nref
logical,INTENT(IN),OPTIONAL                     :: verbose

integer(kind=irg)                               :: imh, imhz, gg(5), i, minholz, RHOLZ, im, istat, N, &
                                                   ig, numr, ir, irsel, i1, i2, i3, i4, i5, QCindex
integer(kind=irg),allocatable                   :: orbit(:,:)
real(kind=sgl)                                  :: dhkl, io_real(9), H, g3(3), g3n(3), FNg(3), ddt, s, kr(3), exer, &
                                                   rBethe_i, rBethe_d, sgp, r_g, la, dval
integer(kind=irg)                               :: io_int(3), gshort(3), gp(5), isym, nn
type(TDQCreflisttype),pointer                   :: rltail
complex(kind=dbl)                               :: Ucg, qg
real(kind=dbl)                                  :: Vmod, Vpmod, xig, xgp
logical                                         :: isnew

! set the truncation parameters
  rBethe_i = BetheParameter%c3          ! if larger than this value, we ignore the reflection completely
  rBethe_d = BetheParameter%sgdbdiff    ! excitation error cutoff for double diffraction reflections
  la = 1.0/sngl(QCcell%mLambda)
  
! get the size of the lookup table
  !gg   = shape(QCcell%LUT)
  imh  = QCcell%imax_qc/2 !(gg(1) - 1)/4
  imhz = QCcell%imax_p/2  !(gg(5) - 1)/4
  nullify(listroot)
  nullify(rltail)
 
! transmitted beam has excitation error zero
  gg = (/ 0,0,0,0,0 /)
  !QCindex = QC_get6Dindex(QCcell, gg)
  !QCindex = QC_getindex(QCcell, gg)
  call AddQCReflection(rltail, listroot, QCcell, nref, gg)   ! this guarantees that 000 is always the first reflection
  rltail%sg = 0.0

  allocate(orbit(5,QCcell%nsym))
  orbit = 0

! now compute |sg|/|U_g|/lambda for the other allowed reflections; if this parameter is less than
! the threshhold, rBethe_i, then add the reflection to the list of potential reflections
i1l: do i1=-imh,imh
 i2l: do i2=-imh,imh
  i3l: do i3=-imh,imh
   i4l: do i4=-imh,imh
    i5l: do i5=-imhz,imhz

      if ((abs(i1)+abs(i2)+abs(i3)+abs(i4)+abs(i5)).ne.0) then  ! avoid double counting the origin
        gg      = (/ i1, i2, i3, i4, i5/)
        QCindex = QC_getindex(QCcell, gg)
        sgp     = QC_Calcsg(QCcell,gg,k,FN)
        Ucg     = QCcell%LUT(QCindex)		!QCcell%LUT(gg(1),gg(2),gg(3),gg(4),gg(5))
        r_g     = la * abs(sgp)/cdabs(Ucg)
        
        if (r_g.le.rBethe_i) then 
          call AddQCReflection( rltail, listroot, QCcell, nref, gg )
          rltail%sg   = sgp
          rltail%glen = QC_getvectorLength(QCcell, gg, 'P', 'r')
        end if
      end if

    end do i5l
   end do i4l
  end do i3l
 end do i2l
end do i1l
    
if (present(verbose)) then 
  if (verbose) then 
    io_int(1) = nref
    call WriteValue(' Length of the master list of reflections : ', io_int, 1, "(I8)")
  end if
end if

end subroutine Initialize_TDQCReflectionList

!--------------------------------------------------------------------------
!
! SUBROUTINE: Initialize_QCReflectionList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the potential quasi-crystal reflection list for a given wave vector
!
!> @param QCcell cell pointer
!> @param BetheParameter Bethe potential structure
!> @param FN  foil normal
!> @param k zone axis direction cosines in direct Bravais lattice
!> @param listroot pointer to top of list (could be cell%reflist)
!> @param nref number of reflections in main list (used to be DynNbeams)
!> @param verbose (optional) used for debugging purposes mostly
!
!> @date 03/15/17 MDG 1.0 original, based on regular Initialize_ReflectionList
!--------------------------------------------------------------------------
recursive subroutine Initialize_QCReflectionList(QCcell, listroot, BetheParameter, FN, k, nref, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_QCReflectionList

use local
use typedefs
use io
use constants
use diffraction

IMPLICIT NONE

type(QCStructureType),pointer                   :: QCcell
type(QCreflisttype),pointer                     :: listroot
type(BetheParameterType),INTENT(INOUT)          :: BetheParameter
real(kind=sgl),INTENT(IN)                       :: FN(3)
real(kind=sgl),INTENT(IN)                       :: k(3)
integer(kind=irg),INTENT(INOUT)                 :: nref
logical,INTENT(IN),OPTIONAL                     :: verbose

integer(kind=irg)                               :: imh, gg(6), i, minholz, RHOLZ, im, istat, N, &
                                                   ig, numr, ir, irsel, i1, i2, i3, i4, i5, i6, QCindex
real(kind=sgl)                                  :: dhkl, io_real(9), H, g3(3), g3n(3), FNg(3), ddt, s, kr(3), exer, &
                                                   rBethe_i, rBethe_d, sgp, r_g, la, dval
integer(kind=irg)                               :: io_int(3), gshort(3), gp(6)
type(QCreflisttype),pointer                     :: rltail
complex(kind=dbl)                               :: Ucg, qg
real(kind=dbl)                                  :: Vmod, Vpmod, xig, xgp

! set the truncation parameters
  rBethe_i = BetheParameter%c3          ! if larger than this value, we ignore the reflection completely
  rBethe_d = BetheParameter%sgdbdiff    ! excitation error cutoff for double diffraction reflections
  la = 1.0/sngl(QCcell%mLambda)
  
! get the size of the lookup table
  imh = QCcell%imax / 2

  nullify(listroot)
  nullify(rltail)
 
! transmitted beam has excitation error zero
  gg = (/ 0,0,0,0,0,0 /)
  QCindex = QC_getindex(QCcell, gg)
  call AddQCReflection(rltail, listroot, QCcell, nref, QCindex, gg)   ! this guarantees that 000 is always the first reflection
  rltail%sg = 0.0


! now compute |sg|/|U_g|/lambda for the other allowed reflections; if this parameter is less than
! the threshhold, rBethe_i, then add the reflection to the list of potential reflections
i1l: do i1=-imh,imh
 i2l: do i2=-imh,imh
  i3l: do i3=-imh,imh
   i4l: do i4=-imh,imh
    i5l: do i5=-imh,imh
     i6l: do i6=-imh,imh

        if ((abs(i1)+abs(i2)+abs(i3)+abs(i4)+abs(i5)+abs(i6)).ne.0) then  ! avoid double counting the origin
          gg = (/ i1, i2, i3, i4, i5, i6 /)
          QCindex = QC_getindex(QCcell, gg)
          sgp = QC_Calcsg(QCcell,gg,k,FN)
          Ucg = QCcell%LUT(QCindex)
          r_g = la * abs(sgp)/cdabs(Ucg)
          if (r_g.le.rBethe_i) then 
            call AddQCReflection( rltail, listroot, QCcell, nref, QCindex, gg )
            rltail%sg = sgp
            rltail%glen = QC_getvectorLength(QCcell, gg, 'P', 'r')
          end if
        end if

     end do i6l
    end do i5l
   end do i4l
  end do i3l
 end do i2l
end do i1l
    
  if (present(verbose)) then 
    if (verbose) then 
      io_int(1) = nref
      call WriteValue(' Length of the master list of reflections : ', io_int, 1, "(I8)")
    end if
  end if

end subroutine Initialize_QCReflectionList

!--------------------------------------------------------------------------
!
! SUBROUTINE: QC_Apply_BethePotentials
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
recursive subroutine QC_Apply_BethePotentials(QCcell, listroot, listrootw, BetheParameter, nref, nns, nnw)
!DEC$ ATTRIBUTES DLLEXPORT :: QC_Apply_BethePotentials

use io
use diffraction

IMPLICIT NONE

type(QCStructureType),pointer                  :: QCcell
type(QCreflisttype),pointer                    :: listroot
type(QCreflisttype),pointer                    :: listrootw
type(BetheParameterType),INTENT(IN)            :: BetheParameter
integer(kind=irg),INTENT(IN)                   :: nref
integer(kind=irg),INTENT(OUT)                  :: nns
integer(kind=irg),INTENT(OUT)                  :: nnw

integer(kind=irg),allocatable                  :: glist(:,:)
real(kind=dbl),allocatable                     :: rh(:)
type(QCreflisttype),pointer                    :: rl, lastw, lasts
integer(kind=irg)                              :: icnt, istat, gmh(6), ir, ih, QCindex
real(kind=dbl)                                 :: sgp, la, m

complex(kind=dbl)                              :: Ugh, qg
real(kind=dbl)                                 :: Vmod, Vpmod, xig, xgp


nullify(lasts)
nullify(lastw)
nullify(rl)

! first we extract the list of g-vectors from reflist, so that we can compute 
! all the g-h difference vectors
allocate(glist(6,nref),rh(nref),stat=istat)
rl => listroot%next
icnt = 0
do
  if (.not.associated(rl)) EXIT
  icnt = icnt+1
  glist(1:6,icnt) = rl%hkl(1:6)
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

la = 1.D0/QCcell%mLambda

! next we need to iterate through all reflections in glist and 
! determine which category the reflection belongs to: strong, weak, ignore
irloop: do ir = 2,icnt
  rl => rl%next
  rh = 0.D0
  sgp = la * abs(rl%sg)
  do ih = 1,icnt
    gmh(1:6) = glist(1:6,ir) - glist(1:6,ih)
    !QCindex = QC_get6Dindex(QCcell, gmh)
    QCindex = QC_getindex(QCcell, gmh)
    Ugh = QCcell%LUT(QCindex)
    if (cdabs(Ugh).eq.0.D0) then 
      rh(ih) = 10000.D0
    else
      rh(ih) = sgp/cdabs(Ugh)
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

end subroutine QC_Apply_BethePotentials

!--------------------------------------------------------------------------
!
! SUBROUTINE: TDQC_Apply_BethePotentials
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
!> @date  03/23/18 SS  2.2 adapted from QCmod.f90
!--------------------------------------------------------------------------
recursive subroutine TDQC_Apply_BethePotentials(QCcell, listroot, listrootw, BetheParameter, nref, nns, nnw)
!DEC$ ATTRIBUTES DLLEXPORT :: TDQC_Apply_BethePotentials

use io
use diffraction

IMPLICIT NONE

type(TDQCStructureType),pointer                :: QCcell
type(TDQCreflisttype),pointer                  :: listroot
type(TDQCreflisttype),pointer                  :: listrootw
type(BetheParameterType),INTENT(IN)            :: BetheParameter
integer(kind=irg),INTENT(IN)                   :: nref
integer(kind=irg),INTENT(OUT)                  :: nns
integer(kind=irg),INTENT(OUT)                  :: nnw

integer(kind=irg),allocatable                  :: glist(:,:)
real(kind=dbl),allocatable                     :: rh(:)
type(TDQCreflisttype),pointer                  :: rl, lastw, lasts
integer(kind=irg)                              :: icnt, istat, gmh(5), ir, ih, QCindex
real(kind=dbl)                                 :: sgp, la, m

complex(kind=dbl)                              :: Ugh, qg
real(kind=dbl)                                 :: Vmod, Vpmod, xig, xgp, eps

eps = 1.D0-8

nullify(lasts)
nullify(lastw)
nullify(rl)

! first we extract the list of g-vectors from reflist, so that we can compute 
! all the g-h difference vectors
allocate(glist(5,nref),rh(nref),stat=istat)
rl => listroot%next
icnt = 0
do
  if (.not.associated(rl)) EXIT
  icnt = icnt+1
  glist(1:5,icnt) = rl%hkl(1:5)
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

la = 1.D0/QCcell%mLambda

! next we need to iterate through all reflections in glist and 
! determine which category the reflection belongs to: strong, weak, ignore
irloop: do ir = 2,icnt
  rl => rl%next
  rh = 0.D0
  sgp = la * abs(rl%sg)
  do ih = 1,icnt
    gmh(1:5) = glist(1:5,ir) - glist(1:5,ih)
    QCindex  = QC_getindex(QCcell, gmh)
    Ugh      = QCcell%LUT(QCindex) !QCcell%LUT(gmh(1),gmh(2),gmh(3),gmh(4),gmh(5))

    if (cdabs(Ugh) .lt. eps) then 
      rh(ih) = 10000.D0
    else
      rh(ih) = sgp/cdabs(Ugh)
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

end subroutine TDQC_Apply_BethePotentials

end module gvectorsQC
