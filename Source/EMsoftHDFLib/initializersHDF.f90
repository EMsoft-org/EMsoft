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
! EMsoft:initializersHDF.f90
!--------------------------------------------------------------------------
!
! MODULE: initializersHDF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief several basic initialization routines
!
!> @date 01/10/14 MDG 1.0 new version
!--------------------------------------------------------------------------

module initializersHDF

contains

!--------------------------------------------------------------------------
!
! subroutine: Initialize_Cell
!
!> @author Marc De Graef
!
!> @brief perform all steps to initialize a unit cell type variable
!
!> @param cell unit cell pointer
!> @param xtalname file name for crystal structure
!> @param dmin smallest d-spacing to consider
!> @param voltage accelerating voltage (needed to compute relativistic scattering factors)
!
!> @date 01/10/14 MDG 1.0 original
!> @date 06/10/14 MDG 2.0 rewrite without global variables
!> @date 08/14/15 MDG 2.1 increased threshold ddt for double diffraction spots
!> @date 09/08/15 MDG 2.2 added LUTqg array to cell
!> @date 09/29/16 MDG 5.2 added option to read CrystalData from currently open HDF file
!> @date 07/02/18 SS  5.3 added initLUT optional variable
!> @date 08/09/18 MDG 5.4 added FSCATT interpolation option
!--------------------------------------------------------------------------
recursive subroutine Initialize_Cell(cell,Dyn,rlp,xtalname, dmin, voltage, &
                                     verbose, existingHDFhead, initLUT, noLUT, interpolate)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_Cell

use local
use typedefs
use crystal
use symmetry
use files
use io
use error
use gvectors
use diffraction
use HDFsupport

IMPLICIT NONE

type(unitcell)                             :: cell
type(DynType),INTENT(INOUT)                :: Dyn
!f2py intent(in,out) ::  Dyn
type(gnode),INTENT(INOUT)                  :: rlp
!f2py intent(in,out) ::  rlp
character(fnlen),INTENT(IN)                :: xtalname
real(kind=sgl),INTENT(IN)                  :: dmin
real(kind=sgl),INTENT(IN)                  :: voltage
logical,INTENT(IN),OPTIONAL                :: verbose
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead
logical,INTENT(IN),OPTIONAL                :: initLUT
logical,INTENT(IN),OPTIONAL                :: noLUT
logical,INTENT(IN),OPTIONAL                :: interpolate

integer(kind=irg)                          :: istat, io_int(3), skip
integer(kind=irg)                          :: imh, imk, iml, gg(3), ix, iy, iz
real(kind=sgl)                             :: dhkl, io_real(3), ddt
logical                                    :: loadingfile, justinit, interp, compute
real(kind=sgl),parameter                   :: gstepsize = 0.001  ! [nm^-1] interpolation stepsize

interp = .FALSE.
if (present(interpolate)) then
  if (interpolate) then
    interp = .TRUE.
    rlp%method= 'IP'
  end if
end if

justinit = .FALSE.
if(present(initLUT)) then
  if(initLUT) justinit = .TRUE.
end if

compute = .TRUE.
if(present(noLUT)) then
  if(noLUT) compute= .FALSE.
end if

if(.not. justinit) then
! clear the cell variable (set everything to zero)
 call ResetCell(cell)

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = xtalname
 call CrystalData(cell,verbose, existingHDFhead)

! generate all atom positions
! if the cell is distorted, then this is not exactly correct, but it should be close for small distortions
 call CalcPositions(cell,'v')

end if

 cell%voltage = dble(voltage)

 skip = 3        ! always use Weickenmeier&Kohl scattering coefficients, including absorptive form factors
 call CalcWaveLength(cell,rlp,skip,verbose)


! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.dmin) EXIT
   iml = iml + 1
 end do
 
 if (present(verbose)) then
  if (verbose) then
    io_int = (/ imh, imk, iml /)
    call WriteValue(' Range of reflections along a*, b* and c* = ',io_int,3)
  end if
 end if

! do we need to pre-compute the scattering factors and store them in cell%scatfac ?
 if (interp.eqv..TRUE.) then
   call PreCalcFSCATT(cell, dmin, gstepsize)
 end if

if(.not. justinit) then  
! the LUT array stores all the Fourier coefficients, so that we only need to compute them once... i.e., here and now
 allocate(cell%LUT(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUT array')

 allocate(cell%LUTqg(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUTqg array')
 
! allocate an array that keeps track of potential double diffraction reflections
 allocate(cell%dbdiff(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%dbdiff array')

! changed from 1.0e-10 on 08/14/15 by MDG in response to some issues with double
! diffraction spots not being taken into account in EBSD master pattern simulations 
end if

 cell%LUT = cmplx(0.D0,0.D0)
 cell%LUTqg = cmplx(0.D0,0.D0)
 cell%dbdiff = .FALSE.
 ! ddt = 1.0e-5 
 ddt = 1.0e-10 

! next, we compute the overall lookup table cell%LUT; we do not, at this point, create a 
! list of linked reflections; in the old code, this was done at the same time, but it appears
! it is better to decouple these two computations. In this new approach, we'll compute a much
! shorter linked list based on the incident wave vector direction.

if (compute) then 
! first, we deal with the transmitted beam
 gg = (/ 0,0,0 /)
 if (interp.eqv..TRUE.) then
   call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.,interpolate=.TRUE.)  
 else
   call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)  
 end if
 Dyn%Upz = rlp%Vpmod         ! U'0 normal absorption parameter 
 if (present(verbose)) then
  if (verbose) then
   io_real(1) = rlp%xgp
   call WriteValue(' Normal absorption length [nm] = ', io_real, 1)
  end if
 end if
 
! and add this reflection to the look-up table
 cell%LUT(0,0,0) = rlp%Ucg
 cell%LUTqg(0,0,0) = rlp%qg

 if (present(verbose)) then
  if (verbose) then
   call Message(' Generating Fourier coefficient lookup table ... ', frm = "(/A)",advance="no")
  end if
 end if
 
!  if (present(nthreads)) then 
!     call OMP_SET_NUM_THREADS(nthreads)
! !$OMP PARALLEL DEFAULT(shared) PRIVATE(iz, ix, iy, gg, rlp)
! ! note that the lookup table must be twice as large as the list of participating reflections,
! ! since the Sgh matrix uses g-h as its index !!!  
! !$OMP DO SCHEDULE(DYNAMIC,5) 
! ! now do the same for the other allowed reflections
! ! note that the lookup table must be twice as large as the list of participating reflections,
! ! since the dynamical matrix uses g-h as its index !!!  
!   ixlomp: do ix=-2*imh,2*imh
!   iylomp:  do iy=-2*imk,2*imk
!   izlomp:   do iz=-2*iml,2*iml
!           gg = (/ ix, iy, iz /)
!           if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
!   ! add the reflection to the look up table
!              if (interp.eqv..TRUE.) then          
!                call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.,interpolate=.TRUE.)
!              else
!                call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)
!              end if
! !$OMP CRITICAL
!              cell%LUT(ix, iy, iz) = rlp%Ucg
!              cell%LUTqg(ix, iy, iz) = rlp%qg
!   ! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
!              if (cabs(rlp%Ucg).le.ddt) then 
!                cell%dbdiff(ix,iy,iz) = .TRUE.
!              end if
! !$OMP END CRITICAL
!           end if ! IsGAllowed
!          end do izlomp
!         end do iylomp
!       end do ixlomp
! !$OMP END PARALLEL 
!  else
! now do the same for the other allowed reflections
! note that the lookup table must be twice as large as the list of participating reflections,
! since the dynamical matrix uses g-h as its index !!!  
  ixl: do ix=-2*imh,2*imh
  iyl:  do iy=-2*imk,2*imk
  izl:   do iz=-2*iml,2*iml
          gg = (/ ix, iy, iz /)
          if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
  ! add the reflection to the look up table
             if (interp.eqv..TRUE.) then          
               call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.,interpolate=.TRUE.)
             else
               call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)
             end if
             cell%LUT(ix, iy, iz) = rlp%Ucg
             cell%LUTqg(ix, iy, iz) = rlp%qg
  ! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
             if (cabs(rlp%Ucg).le.ddt) then 
               cell%dbdiff(ix,iy,iz) = .TRUE.
             end if
          end if ! IsGAllowed
         end do izl
        end do iyl
      end do ixl
 ! end if

  if (present(verbose)) then
   if (verbose) then
    call Message('Done', frm = "(A/)")
   end if
  end if
 end if  

! that's it
end subroutine Initialize_Cell


!--------------------------------------------------------------------------
!
! subroutine: Initialize_Multicell
!
!> @author Marc De Graef, Joseph Tessmer
!
!> @brief perform all steps to initialize a unit cell type variable, with some 
!   added parameters: now we can specify the site occupations
!
!> @param cell unit cell pointer
!> @param xtalname file name for crystal structure
!> @param dmin smallest d-spacing to consider
!> @param voltage accelerating voltage (needed to compute relativistic scattering factors)
!
!> @date 03/26/19 JT Modified Initialize_Cell to allow for different site occupancies
!--------------------------------------------------------------------------
recursive subroutine Initialize_Multicell(cell,Dyn,rlp,xtalname, dmin, voltage,  &
                                     numatoms,numsites, occupancy, dwfs, verbose, existingHDFhead, initLUT, interpolate)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_Multicell

use local
use typedefs
use crystal
use symmetry
use files
use io
use error
use gvectors
use diffraction
use HDFsupport

IMPLICIT NONE

type(unitcell),pointer                     :: cell
type(DynType),INTENT(INOUT)                :: Dyn
type(gnode),INTENT(INOUT)                  :: rlp
character(fnlen),INTENT(IN)                :: xtalname
real(kind=sgl),INTENT(IN)                  :: dmin
real(kind=sgl),INTENT(IN)                  :: voltage
integer(kind=irg),INTENT(IN)               :: numatoms
integer(kind=irg),INTENT(IN)               :: numsites
real(kind=sgl),INTENT(IN)                  :: occupancy(numsites)
real(kind=sgl),INTENT(IN)                  :: dwfs(numatoms)
logical,INTENT(IN),OPTIONAL                :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
logical,INTENT(IN),OPTIONAL                :: initLUT
logical,INTENT(IN),OPTIONAL                :: interpolate

integer(kind=irg)                          :: istat, io_int(3), skip
integer(kind=irg)                          :: imh, imk, iml, gg(3), ix, iy, iz
real(kind=sgl)                             :: dhkl, io_real(3), ddt
logical                                    :: loadingfile, justinit, interp
real(kind=sgl),parameter                   :: gstepsize = 0.001  ! [nm^-1] interpolation stepsize

interp = .FALSE.
if (present(interpolate)) then
  if (interpolate) then
    interp = .TRUE.
    rlp%method= 'IP'
  end if
end if

justinit = .FALSE.
if(present(initLUT)) then
  if(initLUT) justinit = .TRUE.
end if

if(.not. justinit) then
! clear the cell variable (set everything to zero)
 call ResetCell(cell)

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = xtalname

! we need to modify crystalData to generate N atoms at each site, where N is atomtype
 call CrystalData(cell,verbose, existingHDFhead)



! generate all atom positions
! if the cell is distorted, then this is not exactly correct, but it should be close for small distortions
 call CalcPositions(cell,'v')

end if

 cell%voltage = dble(voltage)

 skip = 3        ! always use Weickenmeier&Kohl scattering coefficients, including absorptive form factors
 call CalcWaveLength(cell,rlp,skip,verbose)


! compute the range of reflections for the lookup table and allocate the table
! The master list is easily created by brute force
 imh = 1
 do 
   dhkl = 1.0/CalcLength(cell,  (/float(imh) ,0.0_sgl,0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imh = imh + 1
 end do
 imk = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,float(imk),0.0_sgl/), 'r')
   if (dhkl.lt.dmin) EXIT
   imk = imk + 1
 end do
 iml = 1
 do 
   dhkl = 1.0/CalcLength(cell, (/0.0_sgl,0.0_sgl,float(iml)/), 'r')
   if (dhkl.lt.dmin) EXIT
   iml = iml + 1
 end do
 
 if (present(verbose)) then
  if (verbose) then
    io_int = (/ imh, imk, iml /)
    call WriteValue(' Range of reflections along a*, b* and c* = ',io_int,3)
  end if
 end if

! do we need to pre-compute the scattering factors and store them in cell%scatfac ?
 if (interp.eqv..TRUE.) then
   call PreCalcFSCATT(cell, dmin, gstepsize)
 end if

if(.not. justinit) then  
! the LUT array stores all the Fourier coefficients, so that we only need to compute them once... i.e., here and now
 allocate(cell%LUT(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUT array')

 allocate(cell%LUTqg(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUTqg array')
 
! allocate an array that keeps track of potential double diffraction reflections
 allocate(cell%dbdiff(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%dbdiff array')

! changed from 1.0e-10 on 08/14/15 by MDG in response to some issues with double
! diffraction spots not being taken into account in EBSD master pattern simulations 
end if

 cell%LUT = cmplx(0.D0,0.D0)
 cell%LUTqg = cmplx(0.D0,0.D0)
 cell%dbdiff = .FALSE.
 ddt = 1.0e-5 

! next, we compute the overall lookup table cell%LUT; we do not, at this point, create a 
! list of linked reflections; in the old code, this was done at the same time, but it appears
! it is better to decouple these two computations. In this new approach, we'll compute a much
! shorter linked list based on the incident wave vector direction.

! first, we deal with the transmitted beam
 gg = (/ 0,0,0 /)
 if (interp.eqv..TRUE.) then
   call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.,interpolate=.TRUE.)  
 else
   call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)  
 end if
 Dyn%Upz = rlp%Vpmod         ! U'0 normal absorption parameter 
 if (present(verbose)) then
  if (verbose) then
   io_real(1) = rlp%xgp
   call WriteValue(' Normal absorption length [nm] = ', io_real, 1)
  end if
 end if
 
! and add this reflection to the look-up table
 cell%LUT(0,0,0) = rlp%Ucg
 cell%LUTqg(0,0,0) = rlp%qg

 if (present(verbose)) then
  if (verbose) then
   call Message('Generating Fourier coefficient lookup table ... ', frm = "(/A)",advance="no")
  end if
 end if
 
! now do the same for the other allowed reflections
! note that the lookup table must be twice as large as the list of participating reflections,
! since the dynamical matrix uses g-h as its index !!!  
ixl: do ix=-2*imh,2*imh
iyl:  do iy=-2*imk,2*imk
izl:   do iz=-2*iml,2*iml
        gg = (/ ix, iy, iz /)
        if (IsGAllowed(cell,gg)) then  ! is this reflection allowed by lattice centering ?
! add the reflection to the look up table
           if (interp.eqv..TRUE.) then          
             call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.,interpolate=.TRUE.)
           else
             call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)
           end if
           cell%LUT(ix, iy, iz) = rlp%Ucg
           cell%LUTqg(ix, iy, iz) = rlp%qg
! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
           if (cabs(rlp%Ucg).le.ddt) then 
             cell%dbdiff(ix,iy,iz) = .TRUE.
           end if
        end if ! IsGAllowed
       end do izl
      end do iyl
    end do ixl

  if (present(verbose)) then
   if (verbose) then
    call Message('Done', frm = "(A/)")
   end if
  end if
  

! that's it
end subroutine Initialize_Multicell

end module initializersHDF
