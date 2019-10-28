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
!============================================================
!These are versions of several subroutines that have been adapted
!to work with MD input data rather than calculating from the symmetry
!============================================================

module MDsubroutines 

use local
use typedefs
use HDF5
use HDFsupport

contains
!===========================================
! my version of intialize_cell

recursive subroutine Initialize_Cell_MD(cell, latticeparm, Dyn,rlp,xtalname, dmin, voltage, verbose, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_Cell_MD

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
real(kind=dbl), INTENT(IN)                 :: latticeparm
real(kind=sgl),INTENT(IN)                  :: voltage
logical,INTENT(IN),OPTIONAL                :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

integer(kind=irg)                          :: istat, io_int(3), skip
integer(kind=irg)                          :: imh, imk, iml, gg(3), ix, iy, iz
real(kind=sgl)                             :: dhkl, io_real(3), ddt, starts, finishs
logical                                    :: loadingfile

! I think this is OK as well
! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = xtalname

! this should work but several parts of cell are no longer filled here
 call CrystalData_MD(cell,verbose, existingHDFhead)

 cell%a = latticeparm
 cell%b = latticeparm
 cell%c = latticeparm

 cell%voltage = dble(voltage)

 skip = 3        ! always use Weickenmeier&Kohl scattering coefficients, including absorptive form factors
! this uses CalcPositions which will have to change.
! -----ATOM_ntype, ATOM_type, ATOM_pos, apos, numat need to be defined before this----
 call CalcWaveLength_MD(cell,rlp,skip,verbose)


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

! the LUT array stores all the Fourier coefficients, so that we only need to compute them once... i.e., here and now
 allocate(cell%LUT(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUT array')
 cell%LUT = cmplx(0.D0,0.D0)
 allocate(cell%LUTqg(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUTqg array')
 cell%LUTqg = cmplx(0.D0,0.D0)
 
! allocate an array that keeps track of potential double diffraction reflections
 allocate(cell%dbdiff(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%dbdiff array')
 cell%dbdiff = .FALSE.
 ddt = 1.0e-5  
! changed from 1.0e-10 on 08/14/15 by MDG in response to some issues with double
! diffraction spots not being taken into account in EBSD master pattern simulations 


! next, we compute the overall lookup table cell%LUT; we do not, at this point, create a 
! list of linked reflections; in the old code, this was done at the same time, but it appears
! it is better to decouple these two computations. In this new approach, we'll compute a much
! shorter linked list based on the incident wave vector direction.


! first, we deal with the transmitted beam
 gg = (/ 0,0,0 /)
 call CalcUcg_MD2(cell,rlp,gg,applyqgshift=.TRUE.)  
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
 
! now do the same for the other reflections - we don't know allowed/forbidden 
! this is looping over 2*im(h,k,l)
! note that the lookup table must be twice as large as the list of participating reflections,
! since the dynamical matrix uses g-h as its index !!!

! this is what's really slow
ixl: do ix=-2*imh,2*imh
iyl:  do iy=-2*imk,2*imk
izl:   do iz=-2*iml,2*iml
        gg = (/ ix, iy, iz /)
        if (IsGAllowed_MD(cell,gg)) then  ! is this reflection allowed by lattice centering ?

! add the reflection to the look up table
           call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)
           cell%LUT(ix, iy, iz) = rlp%Ucg
           cell%LUTqg(ix, iy, iz) = rlp%qg
! flag this reflection as a double diffraction candidate if cabs(Ucg)<ddt threshold
           if (cabs(rlp%Ucg).le.ddt) then 
             cell%dbdiff(ix,iy,iz) = .TRUE.
           end if
          end if
       end do izl
      end do iyl
    end do ixl

  if (present(verbose)) then
   if (verbose) then
    call Message('Done', frm = "(A/)")
   end if
  end if

  

! that's it
end subroutine Initialize_Cell_MD

!===========================================

!============================================================
recursive subroutine CalcWaveLength_MD(cell,rlp,skip,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcWaveLength_MD

use constants
use symmetry
use io
use diffraction

IMPLICIT NONE

type(unitcell)                          :: cell
type(gnode),INTENT(INOUT)               :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN),OPTIONAL   :: skip                 !< scattering set identifier
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=dbl)                          :: temp1,temp2, oi_real(1)
integer(kind=irg)                       :: hkl(3), io_int(1)

  temp1 = 1.0D+9*cPlanck/dsqrt(2.D0*cRestmass*cCharge)
  temp2 = cCharge*0.5D0*cell%voltage*1000.D0/cRestmass/cLight**2

! relativistic correction factor (known as gamma)      
  cell%mRelcor = 1.0D0+2.0D0*temp2

! relativistic acceleration voltage
  cell%mPsihat = cell%voltage*(1.D0+temp2)*1000.D0

! which scattering factors should be used ?
  if (present(skip)) then
   select case (skip) 
    case(1); rlp%method='DT'; 
    case(2); rlp%method='WK'; 
    case(3); rlp%method='WK'; rlp%absorption=.TRUE.
   end select
  else
   call Message(' The following scattering factor sets are available :', frm = "(/A/)")
   call Message('  [1] Doyle-Turner/Smith-Burge (no absorption) ', frm = "(A)")
   call Message('  [2] Weickenmeier-Kohl (no absorption) ', frm = "(A)")
   call Message('  [3] Weickenmeier-Kohl (with absorption) ', frm = "(A/)")
   call ReadValue('Which set do you want to use [1/2/3] ? ', io_int,1)
   rlp%absorption = .FALSE.
   select case (io_int(1)) 
    case(1); rlp%method='DT'; 
    case(2); rlp%method='WK'; 
    case(3); rlp%method='WK'; rlp%absorption = .TRUE.
   end select
  end if

 hkl=(/0,0,0/)
! ATOM_ntype, ATOM_type, ATOM_pos, apos, numat need to be defined before this
 call CalcUcg(cell,rlp,hkl) 
 cell%mPsihat = cell%mPsihat + dble(rlp%Vmod)
 cell%mLambda = temp1/dsqrt(cell%mPsihat)
! interaction constant sigma
 cell%mSigma = 2.D0*cPi*cRestmass*cell%mRelcor*cCharge*cell%mLambda
 cell%mSigma = 1.0D-18*cell%mSigma/cPlanck**2

 if (present(verbose)) then
  if (verbose) then
    oi_real(1) = rlp%Vmod
    call WriteValue('Mean inner potential [V] ', oi_real, 1,"(' ',E10.4)")
    call Message(' Wavelength corrected for refraction', frm = "(A)")
    oi_real(1) = cell%mRelcor
    call WriteValue('Relativistic correction factor [gamma]  ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = cell%mPsihat
    call WriteValue('Relativistic Accelerating Potential [V] ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = cell%mLambda
    call WriteValue('Electron Wavelength [nm]                ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = cell%mSigma
    call WriteValue('Interaction constant [V nm]^(-1)        ', oi_real, 1,"(' ',E10.4)")
  end if
 end if

 
end subroutine CalcWaveLength_MD

!===========================================


!--------------------------------------------------------------------------
recursive subroutine CrystalData_MD(cell,verbose, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: CrystalData_MD

use io
use crystal
use files
use symmetry
use typedefs

IMPLICIT NONE

type(unitcell), INTENT(INOUT)           :: cell
!f2py intent(in,out) ::  cell
logical,INTENT(IN),OPTIONAL             :: verbose
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

integer(kind=irg)                       :: i, ipg, isave

call ReadDataHDF_MD(cell, existingHDFhead)

! strucdef = .TRUE.
 cell%hexset = .FALSE.
 if (cell%xtal_system.eq.4) cell%hexset = .TRUE.
 if ((cell%xtal_system.eq.5).AND.(cell%SYM_SGset.ne.2)) cell%hexset = .TRUE.

! compute the metric matrices
! need to think about if these matrices actaully represent the cell or do we need to the angles/parameters for each cell

 call CalcMatrices(cell)



end subroutine CrystalData_MD

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
recursive subroutine ReadDataHDF_MD(cell, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: ReadDataHDF_MD

use io
use crystal
use error
use HDF5
 
IMPLICIT NONE

type(unitcell), INTENT(INOUT)           :: cell
!f2py intent(in,out) ::  cell
type(HDFobjectStackType),OPTIONAL,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

type(HDFobjectStackType)                :: HDF_head

character(fnlen)                        :: dataset, groupname, fname
integer(HSIZE_T)                        :: dims(1), dims2(2)
integer(kind=irg)                       :: hdferr
real(kind=dbl),allocatable              :: cellparams(:)
integer(kind=irg),allocatable           :: atomtypes(:)
real(kind=sgl),allocatable              :: atompos(:,:)
character(fnlen)                        :: pp
logical                                 :: openHDFfile

openHDFfile = .TRUE.
if (present(existingHDFhead)) then
  if (associated(existingHDFhead%next)) then
    openHDFfile = .FALSE.
    HDF_head = existingHDFhead
  else
    call FatalError("ReadDataHDF","HDF_head pointer passed in to routine is not associated")
  end if 
end if

if (openHDFfile) then 
  nullify(HDF_head%next)
  call h5open_EMsoft(hdferr)
  call HDFerror_check('ReadDataHDF:h5open_EMsoft', hdferr)

  fname = trim(EMsoft_getXtalpathname())//trim(cell%fname)
  fname = EMsoft_toNativePath(fname)
  hdferr =  HDF_openFile(fname, HDF_head)
  call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)
end if

groupname = 'CrystalData'
hdferr = HDF_openGroup(groupname, HDF_head)
call HDFerror_check('ReadDataHDF:HDF_openGroup:'//trim(groupname), hdferr)

dataset = 'CrystalSystem'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%xtal_system)
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)


dataset = 'LatticeParameters'
call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head, hdferr, cellparams)
call HDFerror_check('ReadDataHDF:HDF_readDatasetDoubleArray1D:'//trim(dataset), hdferr)

cell%a = cellparams(1)
cell%b = cellparams(2)
cell%c = cellparams(3)
cell%alpha = cellparams(4)
cell%beta = cellparams(5)
cell%gamma = cellparams(6)

! don't know if I need this or not
dataset = 'SpaceGroupNumber'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SYM_SGnum) 
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

dataset = 'SpaceGroupSetting'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%SYM_SGset)
call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

! this parameter must be either 1 or 2, but is initialized to 0;
! some older .xtal files may still have 0 in them, so we correct this here
if (cell%SYM_SGset.eq.0) cell%SYM_SGset = 1


! This data will not come from the HDF for our purposes
! dataset = 'Natomtypes'
! call HDF_readDatasetInteger(dataset, HDF_head, hdferr, cell%ATOM_ntype)
! call HDFerror_check('ReadDataHDF:HDF_readDatasetInteger:'//trim(dataset), hdferr)

! dataset = 'Atomtypes'
! call HDF_readDatasetIntegerArray1D(dataset, dims, HDF_head, hdferr, atomtypes)
! call HDFerror_check('ReadDataHDF:HDF_readDatasetIntegerArray1D:'//trim(dataset), hdferr)

! cell%ATOM_type(1:cell%ATOM_ntype) = atomtypes(1:cell%ATOM_ntype) 
! deallocate(atomtypes)

! dataset = 'AtomData'
! call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, atompos)
! call HDFerror_check('ReadDataHDF:HDF_readDatasetFloatArray2D:'//trim(dataset), hdferr)

! cell%ATOM_pos(1:cell%ATOM_ntype,1:5) = atompos(1:cell%ATOM_ntype,1:5) 
! deallocate(atompos)

if (openHDFfile) then
  call HDF_pop(HDF_head,.TRUE.)

  call h5close_EMsoft(hdferr)
  call HDFerror_check('ReadDataHDF:h5close_EMsoft', hdferr)
else ! just close this group, but not the file
  call HDF_pop(HDF_head)
end if

! for trigonal space groups we need to set SYM_trigonal to .TRUE.
if ((cell%SYM_SGnum.ge.143).and.(cell%SYM_SGnum.le.167)) then
  cell%SG%SYM_trigonal = .TRUE.
else
  cell%SG%SYM_trigonal = .FALSE.
end if 

! we have not yet implemented the rhombohedral setting of the trigonal 
! space groups, so this needs to remain at .FALSE. always.
cell%SG%SYM_second = .FALSE.
!if (cell%SYM_SGset.ne.0) cell%SG%SYM_second=.TRUE.

end subroutine ReadDataHDF_MD


!------------------------------------------------------------------------

!------------------------------------------------------------------------

recursive subroutine Initialize_ReflectionList_MD(cell, listroot, BetheParameter, FN, k, dmin, dmax, nref, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_ReflectionList_MD

use local
use typedefs
use io
use crystal
use constants
use gvectors
use diffraction
use symmetry

IMPLICIT NONE

type(unitcell)                                  :: cell
type(reflisttype),pointer                       :: listroot
type(BetheParameterType),INTENT(INOUT)          :: BetheParameter
!f2py intent(in,out) ::  BetheParameter
real(kind=sgl),INTENT(IN)                       :: FN(3)
real(kind=sgl),INTENT(IN)                       :: k(3)
real(kind=sgl),INTENT(IN)                       :: dmin, dmax
integer(kind=irg),INTENT(INOUT)                 :: nref
!f2py intent(in,out) ::  nref
logical,INTENT(IN),OPTIONAL                     :: verbose

integer(kind=irg)                               :: imh, imk, iml, gg(3), ix, iy, iz, i, minholz, RHOLZ, im, istat, N, &
                                                   ig, numr, ir, irsel
real(kind=sgl)                                  :: dhkl, io_real(9), H, g3(3), g3n(3), FNg(3), ddt, s, kr(3), exer, &
                                                   rBethe_i, rBethe_d, sgp, r_g, la, dval
integer(kind=irg)                               :: io_int(3), gshort(3), gp(3)
type(reflisttype),pointer                       :: rltail

! set the truncation parameters
  rBethe_i = BetheParameter%c3          ! if larger than this value, we ignore the reflection completely
  rBethe_d = BetheParameter%sgdbdiff    ! excitation error cutoff for double diffraction reflections
  la = 1.0/sngl(cell%mLambda)
  
! get the size of the lookup table
  gp = shape(cell%LUT)
  imh = (gp(1)-1)/4
  imk = (gp(2)-1)/4
  iml = (gp(3)-1)/4
  
  ! listroot is causing a memory leak because it gets allocated but never deallocated
  nullify(listroot)
  nullify(rltail)
 
! transmitted beam has excitation error zero
  gg = (/ 0,0,0 /)
  call AddReflection(rltail, listroot, cell, nref, gg )   ! this guarantees that 000 is always the first reflection
  rltail%sg = 0.0


! now compute |sg|/|U_g|/lambda for the other allowed reflections; if this parameter is less than
! the threshhold, rBethe_i, then add the reflection to the list of potential reflections
ixl: do ix=-imh,imh
iyl:  do iy=-imk,imk
izl:   do iz=-iml,iml
        if ((abs(ix)+abs(iy)+abs(iz)).ne.0) then  ! avoid double counting the origin
         gg = (/ ix, iy, iz /)
         dval = 1.0/CalcLength(cell, float(gg), 'r' )
         if ((IsGAllowed_MD(cell,gg)).and.(dval.gt.dmin).and.(dval.lt.dmax)) then ! allowed by the lattice centering, if any
          sgp = Calcsg(cell,float(gg),k,FN)
          if (cell%dbdiff(ix, iy, iz)) then ! potential double diffraction reflection
            if (abs(sgp).le.rBethe_d) then 
              call AddReflection(rltail, listroot, cell, nref, gg )
              rltail%sg = sgp
              rltail%dbdiff = .TRUE.
            end if
          else
            r_g = la * abs(sgp)/abs(cell%LUT(ix, iy, iz))
            if (r_g.le.rBethe_i) then 
              call AddReflection(rltail, listroot, cell, nref, gg )
              rltail%sg = sgp
              rltail%dbdiff = .FALSE.
            end if
          end if
         end if ! IsGAllowed
        end if
       end do izl
      end do iyl
    end do ixl
    
  if (present(verbose)) then 
    if (verbose) then 
      io_int(1) = nref
      call WriteValue(' Length of the master list of reflections : ', io_int, 1, "(I8)")
    end if
  end if

end subroutine Initialize_ReflectionList_MD

!-----------------------------------------------------------------


!--------------------------------------------------------------------------
!
! SUBROUTINE: PreCalcFSCATTMD
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief precompute the FSCATT values for interpolation purposes, to speed up STEM-DCI and other codes
!
!> @param cell unit cell pointer
!> @param dmin smallest d-spacing to consider
!> @param gstep step size in the cell%scatfacg array
!
!> @date   08/09/18 MDG 1.0 original
!> @date   08/09/18 JT  1.1 modified for MD data
!--------------------------------------------------------------------------
recursive subroutine PreCalcFSCATTMD(cell, dmin, gstep, atomtypes, atomtypesAN, dwfs, dwflg)
!DEC$ ATTRIBUTES DLLEXPORT :: PreCalcFSCATTMD

use crystal
!use symmetry
use constants
use others

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: dmin
real(kind=sgl),INTENT(IN)       :: gstep
integer(kind=irg),INTENT(IN)    :: atomtypes
integer(kind=sgl),INTENT(IN)    :: atomtypesAN(atomtypes)
real(kind=sgl), INTENT(IN)      :: dwfs(atomtypes)
logical, INTENT(IN)             :: dwflg



integer(kind=irg)               :: j,m,ii,i
real(kind=sgl)                  :: s,ul
real(kind=sgl),parameter        :: swk = 0.628318530717959
real(kind=sgl),parameter        :: dwwk = 1.26651479552922
integer(kind=irg),parameter     :: absflg = 3
logical                         :: accflg=.TRUE.
character(2)                    :: smb

! first generate the array of s-values for which the scattering factors need to be computed
s = 2.0/dmin   ! maximum range in reciprocal space
cell%numscatfac = nint(s/gstep) + 2
allocate(cell%scatfacg(cell%numscatfac))
cell%scatfacg = (/ (gstep * float(i-1),i=1,cell%numscatfac) /)
cell%scatfacg = cell%scatfacg * swk

! allocate the scattering factor interpolation array
allocate( cell%scatfac(cell%numscatfac,atomtypes) )

! The Weickenmeier-Kohl (WK) subroutine works in Angstrom, and also 
! scales reciprocal space by a factor of 2*pi;  this scaling
! is accomplished by changing the g-value in nm^{-1} by a 
! scaling factor swk = 2*pi/10, to go from book units to WK units.
!
! A similar scaling must be performed on the Debye Waller factor;
! the book defines it as exp(-Bs^2), with s in [nm^2]; WK define
! it in A^2, and with a scaled reciprocal space.  The conversion
! factor dwwk = 100.0/8*pi^2
!
! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
  
do i=1,cell%numscatfac
! properly scale the scattering parameter
 s = cell%scatfacg(i)

! loop over each type of atom that can be present in the sample
 do m=1,atomtypes
! get the atomic scattering factor for this atom
! scale and include Debye-Waller factor and site occupation parameter
  ul = sqrt(dwfs(m)*dwwk)
  j = atomtypesAN(m)
  cell%scatfac(i,m) = FSCATT(s,ul,j,smb,sngl(cell%voltage),absflg,accflg,dwflg) !*cell%ATOM_pos(m,4) always 1
 end do 
end do 

end subroutine PreCalcFSCATTMD


!-----------------------------------------------------------------

recursive subroutine CalcUcg_MD(cell, rlp, hkl, atom_ntype, atom_type, maxnumincell, apos, interpolate, atomtypes, dwflg)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcUcg_MD

use crystal
use symmetry
use constants
use diffraction
use others

IMPLICIT NONE

type(unitcell)                  :: cell
type(gnode),INTENT(INOUT)       :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN)    :: hkl(3)               !< Miller indices
integer(kind=irg),INTENT(IN)    :: ATOM_ntype
integer(kind=irg),INTENT(IN)    :: atom_type(maxpasym)
integer(kind=irg),INTENT(IN)    :: maxnumincell
real(kind=dbl),INTENT(IN)       :: apos(maxnumincell, 5)
logical,OPTIONAL,INTENT(IN)     :: interpolate          ! requires rlp%mode = 'IP'
integer(kind=irg),optional,INTENT(IN)    :: atomtypes
logical,OPTIONAL,INTENT(IN)     ::dwflg

integer(kind=irg)               :: j,m,ii
real(kind=sgl)                  :: s,arg,pref,ul,pre,sct,fs,fsp
real(kind=sgl),parameter        :: twopi = 6.2831853071795862 ! 2*pi
real(kind=sgl),parameter        :: swk = 0.62831853071795862 ! 0.2*pi
real(kind=sgl),parameter        :: dwwk = 1.2665147955292222 ! 100/(8*pi^2)
real(kind=sgl),parameter        :: c1 = 0.003810010846   ! 0.04787801/(4.0*cPi) 
real(kind=sgl),parameter        :: preg = 0.66484031370806618 ! 2.0 * sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18
complex(kind=sgl)               :: ff,gg,sf,p1
complex(kind=sgl),parameter     :: czero = cmplx(0.0,0.0)
logical,parameter               :: accflg=.TRUE.
integer(kind=irg),parameter     :: absflg=3
character(2)                    :: smb
complex(kind=sgl),allocatable   :: sfarray(:)
logical                         :: interp


rlp%hkl = hkl

! compute the scattering parameter s^2=(g/2)^2
 if (sum(hkl**2).eq.0) then 
  s = 0.0
  rlp%g = 0.0
 else
  rlp%g = sngl(CalcLength(cell,dble(hkl),'r'))
  ! figure out why this is different
  s = rlp%g * swk
   !s = (0.5 * rlp%g)**2
 end if

 ! interpolation is only used for the WK mode to pre-compute the scattering factor array
interp = .FALSE.
if (present(interpolate)) then
  if (interpolate.eqv..TRUE.) interp=.TRUE.
end if

if (rlp%method.eq.'WK') then 
!----------------------------------
! The Weickenmeier-Kohl (WK) subroutine works in Angstrom, and also 
! scales reciprocal space by a factor of 2*pi;  this scaling
! is accomplished by changing the g-value in nm^{-1} by a 
! scaling factor swk = 2*pi/10, to go from book units to WK units.
!
! A similar scaling must be performed on the Debye Waller factor;
! the book defines it as exp(-Bs^2), with s in [nm^2]; WK define
! it in A^2, and with a scaled reciprocal space.  The conversion
! factor dwwk = 100.0/8*pi^2
!
! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
! these are redefined as constant parameters
! swk = 0.1*twopi
! dwwk = 100.0/(8.0*cPi**2)
  
! compute the scaling prefactors
! pref contains A to nm conversion, and divides by 4pi
 pref = c1/cell % vol

! preg is used to go from V to U, remembering that gamma is already
! included in the output from fscatt
 pre = pref * preg

! initialize the real and imaginary parts of the structure factor
 ff=czero
 gg=czero


! loop over all atoms in the asymmetric unit
 do m=1,ATOM_ntype
! get the atomic scattering factor for this atom
! scale and include Debye-Waller factor and site occupation parameter
  ul = sqrt(apos(m,5)*dwwk)
  j = ATOM_type(m)
  sf = FSCATT(s,ul,j,smb,sngl(cell%voltage),absflg,accflg,dwflg)  !*apos(m,4) this is always 1

  arg=twopi*sum(float(hkl(1:3))*apos(m,1:3))
  p1 = exp(cmplx(0.0,-arg))

  ff = ff + p1*real(sf)
  gg = gg + p1*aimag(sf)
 end do
!
! these are the modulus and phase of the real part of Vg
! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g

 rlp%Vmod = pref * cabs(ff)
 rlp%Vphase = atan2(aimag(ff),real(ff))
 rlp%Vpmod = pref * cabs(gg)
 rlp%Vpphase = atan2(aimag(gg),real(gg))

! modulus of U_g and Uprime_g
 rlp%Umod = preg*rlp%Vmod
 rlp%Upmod = preg*rlp%Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
! rlp%Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))

! xg and xgp
! rlp%Vg = rlp%Ucg/preg
 if (abs(rlp%Umod).gt.0.0) then 
  rlp%xg = 1.0/abs(rlp%Umod)/cell%mLambda
 else
  rlp%xg = 1.0E+8
 end if 

 if (abs(rlp%Upmod).gt.0.0) then 
  rlp%xgp = 1.0/abs(rlp%Upmod)/cell%mLambda
 else
  rlp%xgp = 1.0E+8
 end if 

! rlp%ar = rlp%xgp/rlp%xg
 rlp%qg = cmplx(cos(rlp%Vphase)/rlp%xg-sin(rlp%Vpphase)/rlp%xgp,cos(rlp%Vpphase)/rlp%xgp+sin(rlp%Vphase)/rlp%xg)
end if





 !----------------------------------
if (rlp%method.eq.'IP') then 

  ! can we move this outside since it's a fixed size anyway
  ! probably would save a bit of time
   allocate(sfarray(atomtypes))

  ! The Weickenmeier-Kohl (WK) scattering parameters have been pre-calculated 
  ! so all we need to do is linear interpolation to get the correct value
   call getScatfacMD(cell, s, sfarray, atomtypes)

  ! compute the scaling prefactors
  ! pref contains A to nm conversion, and divides by 4pi
   pref = c1/ cell%vol

  ! preg is used to go from V to U, remembering that gamma is already
  ! included in the output from fscatt
   pre = pref * preg

  ! initialize the real and imaginary parts of the structure factor
   ff=czero
   gg=czero

  ! loop over all atoms in the cell
   do m=1,ATOM_ntype
    ! pull the correct matrix out of sf: I'm not allocating SF to contain every possible atom type so this is going 
    ! to have to map Atomtypes to atomtypesAN

    ! to get it to work I'm going to do this in an if-else structure
    ! once it's implemented the inputs to this routine will be changing to include atomtypesAN
    ! if (ATOM_type(m) == 24) sf = sfarray(1)
    ! if (ATOM_type(m) == 26) sf = sfarray(2)
    ! if (ATOM_type(m) == 28) sf = sfarray(3)
    ! !sf = sfarray(1)
    sf = sfarray(INT(apos(m,4)))

    p1 = czero
    arg= twopi*sum(float(hkl(1:3))*apos(m,1:3))
    p1 = p1 + exp(cmplx(0.0,-arg))

    ff = ff + p1*real(sf)
    gg = gg + p1*aimag(sf)

   end do

   rlp%Vmod = pref * cabs(ff)
   rlp%Vphase = atan2(aimag(ff),real(ff))
   rlp%Vpmod = pref * cabs(gg)
   rlp%Vpphase = atan2(aimag(gg),real(gg))

! modulus of U_g and Uprime_g
   rlp%Umod = preg*rlp%Vmod
   rlp%Upmod = preg*rlp%Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
! rlp%Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))

! xg and xgp
! rlp%Vg = rlp%Ucg/preg
   if (abs(rlp%Umod).gt.0.0) then 
    rlp%xg = 1.0/abs(rlp%Umod)/cell%mLambda
   else
    rlp%xg = 1.0E+8
   end if 

   if (abs(rlp%Upmod).gt.0.0) then 
    rlp%xgp = 1.0/abs(rlp%Upmod)/cell%mLambda
   else
    rlp%xgp = 1.0E+8
   end if 

! rlp%ar = rlp%xgp/rlp%xg
   rlp%qg = cmplx(cos(rlp%Vphase)/rlp%xg-sin(rlp%Vpphase)/rlp%xgp,cos(rlp%Vpphase)/rlp%xgp+sin(rlp%Vphase)/rlp%xg)


  
  ! ! ! fill in the entries of the rlp variable
  ! !  rlp%hkl = hkl

  ! ! these are the modulus and phase of the real part of Vg
  !  rlp%Vmod = pref * cabs(ff)
  !  rlp%Vphase = atan2(aimag(ff),real(ff))

  ! ! modulus of U_g
  !  rlp%Umod = preg*rlp%Vmod

  ! ! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g
  !  if (rlp%absorption.eqv..TRUE.) then 
  !   rlp%Vpmod = pref * cabs(gg)
  !   rlp%Vpphase = atan2(aimag(gg),real(gg))

  ! ! modulus of Uprime_g
  !   rlp%Upmod = preg*rlp%Vpmod

  ! ! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
  !   rlp%Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))
  !  else ! set absorption parameters to zero
  !   rlp%Vpmod = 0.0
  !   rlp%Vpphase = 0.0
  ! ! Ucg = U_g (complex number)
  !   rlp%Ucg = pre * ff
  !  end if

  ! ! complex Vg 
  !  rlp%Vg = rlp%Ucg/preg
  !  if (abs(rlp%Umod).gt.0.0) then 
  !   rlp%xg = 1.0/abs(rlp%Umod)/cell%mLambda
  !  else
  !   rlp%xg = 1.0E+8
  !  end if 

  !  if (abs(rlp%Upmod).gt.0.0) then 
  !   rlp%xgp = 1.0/abs(rlp%Upmod)/cell%mLambda
  !  else
  !   rlp%xgp = 1.0E+8
  !  end if 

  !  if (rlp%absorption.eqv..TRUE.) then 
  !   rlp%ar = rlp%xgp/rlp%xg
  !   ! if (present(applyqgshift)) then
  !   !   if (applyqgshift.eqv..TRUE.) then
  !   !     rlp%qg = cmplx(cos(rlp%Vphase)/rlp%xg-sin(rlp%Vpphase)/rlp%xgp,cos(rlp%Vpphase)/rlp%xgp+sin(rlp%Vphase)/rlp%xg)
  !   !   end if
  !   ! else
  !     arg = rlp%Vpphase-rlp%Vphase
  !     rlp%qg = cmplx(1.0/rlp%xg-sin(arg)/rlp%xgp,cos(arg)/rlp%xgp)
  !   ! end if
  !  else
  !   rlp%ar = 0.0
  !   rlp%qg = cmplx(1.0/rlp%xg,0.0)
  !  end if

   deallocate(sfarray)
end if

end subroutine CalcUcg_MD


!--------------------------------------------------------------------------
!
! SUBROUTINE: getScatfac
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief interpolate the precomputed FSCATT values 
!
!> @param cell unit cell pointer
!> @param s reciprocal distance value
!> @param sfarray returned scattering factor values
!
!> @date   08/09/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getScatfacMD(cell, s, sfarray, atomtypes)
!DEC$ ATTRIBUTES DLLEXPORT :: getScatfacMD

use crystal
!use symmetry
use constants
use others

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: s
complex(kind=sgl),INTENT(OUT)   :: sfarray(*)
integer(kind=irg),INTENT(IN)    :: atomtypes

integer(kind=irg)               :: jj
real(kind=sgl)                  :: dx

if (s.eq.0.0) then 
    sfarray(1:atomtypes) = cell%scatfac(1,1:atomtypes)
else
    jj = ifix(s/cell%scatfacg(2))
    if (jj.ge.cell%numscatfac) then
        sfarray(1:atomtypes) = cell%scatfac(cell%numscatfac,1:atomtypes)
    else
        dx = s/cell%scatfacg(2) - float(jj)
        sfarray(1:atomtypes) = cell%scatfac(jj,1:atomtypes)*(1.0-dx) + &
                                     cell%scatfac(jj+1,1:atomtypes)*dx
    end if
end if

end subroutine getScatfacMD




!-----------------------------------------------------------------

recursive subroutine CalcUcg_MD2(cell,rlp,hkl,applyqgshift)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcUcg_MD2

use crystal
use symmetry
use constants
use others

IMPLICIT NONE

type(unitcell)                  :: cell
type(gnode),INTENT(INOUT)       :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN)    :: hkl(3)               !< Miller indices
logical,OPTIONAL,INTENT(IN)     :: applyqgshift

integer(kind=irg)               :: j,absflg,m,ii
real(kind=sgl)                  :: s,twopi,arg,swk,dwwk,pref,ul,pre,preg,sct,fs,fsp
complex(kind=sgl)               :: ff,gg,sf,p1
complex(kind=sgl)               :: czero
logical                         :: accflg, dwflg
character(2)                    :: smb

twopi = sngl(2.D0*cPi)
czero = cmplx(0.0,0.0)
rlp%hkl = hkl

! compute the scattering parameter s^2=(g/2)^2
 if (sum(hkl**2).eq.0) then 
  s = 0.0
  rlp%g = 0.0
 else
  rlp%g = sngl(CalcLength(cell,dble(hkl),'r'))
  s = (0.50*rlp%g)**2
 end if



!----------------------------------
if (rlp%method.eq.'WK') then 
! The Weickenmeier-Kohl (WK) subroutine works in Angstrom, and also 
! scales reciprocal space by a factor of 2*pi;  this scaling
! is accomplished by changing the g-value in nm^{-1} by a 
! scaling factor swk = 2*pi/10, to go from book units to WK units.
!
! A similar scaling must be performed on the Debye Waller factor;
! the book defines it as exp(-Bs^2), with s in [nm^2]; WK define
! it in A^2, and with a scaled reciprocal space.  The conversion
! factor dwwk = 100.0/8*pi^2
!
! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
 swk = 0.1*twopi
 dwwk = 100.0/(8.0*cPi**2)
  
! properly scale the scattering parameter
 s = rlp%g*swk

! let fscatt perform the relativistic corrections for f_g and fprime_g
 accflg = .TRUE.

! include absorption ?
 absflg = 0
 if (rlp%absorption.eqv..TRUE.) absflg = 3  ! include phonon and core contributions

! always include Debye-Waller factor
! dwflg  = .TRUE.

! compute the scaling prefactors
! pref contains A to nm conversion, and divides by 4pi
 pref = 0.04787801/cell % vol/(4.0*cPi) 

! preg is used to go from V to U, remembering that gamma is already
! included in the output from fscatt
 preg = 2.0 * sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18
 pre = pref * preg

! initialize the real and imaginary parts of the structure factor
 ff=czero
 gg=czero

! loop over all atoms in the asymmetric unit
 do m=1,cell % ATOM_ntype
! get the atomic scattering factor for this atom
! scale and include Debye-Waller factor and site occupation parameter
  ul = sqrt(cell % ATOM_pos(m,5)*dwwk)
  j = cell % ATOM_type(m)
  ! print *, 'arguments of FSCATT'
  ! print *, s, ul, j, smb, sngl(cell%voltage), absflg, accflg, dwflg
  sf = FSCATT(s,ul,j,smb,sngl(cell%voltage),absflg,accflg,dwflg)*cell%ATOM_pos(m,4)

! loop over all atoms in the orbit
  p1 = czero
  do j=1,cell%numat(m)
   arg=twopi*sum(float(hkl(1:3))*cell%apos(m,j,1:3))
   p1 = p1 + exp(cmplx(0.0,-arg))
  end do

  ff = ff + p1*real(sf)
  gg = gg + p1*aimag(sf)
 end do
!
! fill in the entries of the rlp variable
 rlp%hkl = hkl

! these are the modulus and phase of the real part of Vg
 rlp%Vmod = pref * cabs(ff)
 rlp%Vphase = atan2(aimag(ff),real(ff))

! modulus of U_g
 rlp%Umod = preg*rlp%Vmod

! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g
 if (rlp%absorption.eqv..TRUE.) then 
  rlp%Vpmod = pref * cabs(gg)
  rlp%Vpphase = atan2(aimag(gg),real(gg))

! modulus of Uprime_g
  rlp%Upmod = preg*rlp%Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
  rlp%Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))
 else ! set absorption parameters to zero
  rlp%Vpmod = 0.0
  rlp%Vpphase = 0.0
! Ucg = U_g (complex number)
  rlp%Ucg = pre * ff
 end if

! complex Vg 
 rlp%Vg = rlp%Ucg/preg
 if (abs(rlp%Umod).gt.0.0) then 
  rlp%xg = 1.0/abs(rlp%Umod)/cell%mLambda
 else
  rlp%xg = 1.0E+8
 end if 

 if (abs(rlp%Upmod).gt.0.0) then 
  rlp%xgp = 1.0/abs(rlp%Upmod)/cell%mLambda
 else
  rlp%xgp = 1.0E+8
 end if 

 if (rlp%absorption.eqv..TRUE.) then 
  rlp%ar = rlp%xgp/rlp%xg
  if (present(applyqgshift)) then
    if (applyqgshift.eqv..TRUE.) then
      rlp%qg = cmplx(cos(rlp%Vphase)/rlp%xg-sin(rlp%Vpphase)/rlp%xgp,cos(rlp%Vpphase)/rlp%xgp+sin(rlp%Vphase)/rlp%xg)
    end if
  else
    arg = rlp%Vpphase-rlp%Vphase
    rlp%qg = cmplx(1.0/rlp%xg-sin(arg)/rlp%xgp,cos(arg)/rlp%xgp)
  end if
 else
  rlp%ar = 0.0
  rlp%qg = cmplx(1.0/rlp%xg,0.0)
 end if

end if

end subroutine CalcUcg_MD2


!-----------------------------------------------------------------


recursive subroutine Initialize_Cell_2(cell,Dyn,rlp,xtalname, dmin, voltage, verbose, existingHDFhead)
!DEC$ ATTRIBUTES DLLEXPORT :: Initialize_Cell_2

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
type(HDFobjectStackType),OPTIONAL,pointer,INTENT(INOUT)        :: existingHDFhead
!f2py intent(in,out) ::  existingHDFhead

integer(kind=irg)                          :: istat, io_int(3), skip
integer(kind=irg)                          :: imh, imk, iml, gg(3), ix, iy, iz
real(kind=sgl)                             :: dhkl, io_real(3), ddt
logical                                    :: loadingfile


! clear the cell variable (set everything to zero)
 call ResetCell(cell)

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
 cell%SG%SYM_reduce=.TRUE.
 cell%fname = xtalname
 call CrystalData(cell,verbose, existingHDFhead)
 cell%voltage = dble(voltage)

 skip = 3        ! always use Weickenmeier&Kohl scattering coefficients, including absorptive form factors
 call CalcWaveLength(cell,rlp,skip,verbose)

! generate all atom positions
! if the cell is distorted, then this is not exactly correct, but it should be close for small distortions
 call CalcPositions(cell,'v')

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
  
! the LUT array stores all the Fourier coefficients, so that we only need to compute them once... i.e., here and now
 allocate(cell%LUT(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUT array')
 cell%LUT = cmplx(0.D0,0.D0)
 allocate(cell%LUTqg(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%LUTqg array')
 cell%LUTqg = cmplx(0.D0,0.D0)
 
! allocate an array that keeps track of potential double diffraction reflections
 allocate(cell%dbdiff(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml),stat=istat)
 if (istat.ne.0) call FatalError('InitializeCell:',' unable to allocate cell%dbdiff array')
 cell%dbdiff = .FALSE.
 ddt = 1.0e-5  
! changed from 1.0e-10 on 08/14/15 by MDG in response to some issues with double
! diffraction spots not being taken into account in EBSD master pattern simulations 


! next, we compute the overall lookup table cell%LUT; we do not, at this point, create a 
! list of linked reflections; in the old code, this was done at the same time, but it appears
! it is better to decouple these two computations. In this new approach, we'll compute a much
! shorter linked list based on the incident wave vector direction.

! first, we deal with the transmitted beam
 gg = (/ 0,0,0 /)
 call CalcUcg_MD2(cell,rlp,gg,applyqgshift=.TRUE.)  
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
           call CalcUcg(cell,rlp,gg,applyqgshift=.TRUE.)
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
end subroutine Initialize_Cell_2


! !--------------------------------------------------------------------------
! !
! ! SUBROUTINE: DumpXtalInfo
! !
! !> @author Marc De Graef, Carnegie Mellon University
! !
! !> @brief Write a brief summary of the crystal structure on the screen
! ! 
! !> @date    1/ 5/99 MDG 1.0 original
! !> @date    5/19/01 MDG 2.0 f90 version
! !> @date   11/27/01 MDG 2.1 added kind support
! !> @date   03/25/13 MDG 3.0 updated IO
! !> @date   01/10/14 MDG 4.0 update after new cell type
! !> @date   06/06/14 MDG 4.1 added cell pointer as argument, corrected Message routine
! !--------------------------------------------------------------------------
! recursive subroutine DumpXtalInfo_MD(cell, stdout)    
! !DEC$ ATTRIBUTES DLLEXPORT :: DumpXtalInfo_MD

! use constants
! use io
! use symmetry

! IMPLICIT NONE

! type(unitcell), pointer                 :: cell
! integer(kind=irg),INTENT(IN),OPTIONAL   :: stdout

! integer(kind=irg)                       :: i, j, oi_int(3), std
! real(kind=dbl)                          :: oi_real(5)

!  std = 6
!  if (PRESENT(stdout)) std = stdout

!  call Message('', frm = "(A/)", stdout = std)
!  call Message('Crystal Structure Information', frm = "('-->',A,'<--')", stdout = std)
!  oi_real(1) = cell%a
!  call WriteValue('  a [nm]             : ', oi_real, 1, "(F9.5)", stdout = std)
!  oi_real(1) = cell%b
!  call WriteValue('  b [nm]             : ', oi_real, 1, "(F9.5)", stdout = std)
!  oi_real(1) = cell%c
!  call WriteValue('  c [nm]             : ', oi_real, 1, "(F9.5)", stdout = std)
!  oi_real(1) = cell%alpha
!  call WriteValue('  alpha [deg]        : ', oi_real, 1, "(F9.5)", stdout = std)
!  oi_real(1) = cell%beta
!  call WriteValue('  beta  [deg]        : ', oi_real, 1, "(F9.5)", stdout = std)
!  oi_real(1) = cell%gamma
!  call WriteValue('  gamma [deg]        : ', oi_real, 1, "(F9.5)", stdout = std)
!  oi_real(1) = cell%vol
!  call WriteValue('  Volume [nm^3]      : ', oi_real, 1, "(F12.8)", stdout = std)
!  oi_int(1) = cell%SYM_SGnum
!  call WriteValue('  Space group #      : ', oi_int, 1, "(1x,I3)", stdout = std)
!  call WriteValue('  Space group symbol : ', trim(SYM_SGname(cell%SYM_SGnum)) , stdout = std)
!  call WriteValue('  Generator String   : ',  trim(SYM_GL(cell%SYM_SGnum)) , stdout = std)
!  if ((cell%SYM_SGset.eq.2).AND.(cell%xtal_system.ne.5)) then 
!   call Message('   Using second origin setting', frm = "(A)", stdout = std)
!  endif
!  if ((cell%SYM_SGset.eq.2).AND.(cell%xtal_system.eq.5)) then 
!   call Message('   Using rhombohedral parameters', frm = "(A)", stdout = std)
!  endif
!   if (cell%SG%SYM_centrosym) then 
!     call Message('   Structure is centrosymmetric', frm = "(A)", stdout = std)
!  else 
!    call Message('   Structure is non-centrosymmetric', frm = "(A)", stdout = std)
!  end if
!  call Message('', frm = "(A/)", stdout = std)
!  oi_int(1) = cell%ATOM_ntype
!  call WriteValue('  Number of asymmetric atom positions ', oi_int, 1, stdout = std)
!  do i=1,cell%ATOM_ntype
!   oi_int(1:3) = (/i, cell%ATOM_type(i), cell%numat(i)/)
!   call WriteValue('  General position / atomic number / multiplicity :', oi_int, 3,"(1x,I3,'/',I2,'/',I3)",advance="no", stdout = std)
!   call Message(' ('//ATOM_sym(cell%ATOM_type(i))//')', frm = "(A)", stdout = std)
!   call Message('   Equivalent positions  (x y z  occ  DWF) ', frm = "(A)", stdout = std)
!   do j=1,cell%numat(i)
!     oi_real(1:5) = (/cell%apos(i, j,1:3),dble(cell%ATOM_pos(i,4:5))/)
!     call WriteValue('         > ', oi_real, 5,"(2x,4(F9.5,','),F9.5)", stdout = std)
!   end do
! end do
! call Message('', frm = "(A/)", stdout = std)

! end subroutine DumpXtalInfo_MD

! ==========================================================

recursive logical function IsGAllowed_MD(cell,g)
!DEC$ ATTRIBUTES DLLEXPORT :: IsGAllowed_MD

IMPLICIT NONE

type(unitcell)          :: cell
integer(kind=irg),INTENT(IN)            :: g(3)         !< input reciprocal lattice vector

integer(kind=irg)                       :: seo          !< auxiliary variable
character(1)                            :: lc           !< first letter of space group name

! Determine whether or not this vector is
! actually allowed by the lattice centering
 lc(1:1) =  cell%SG%SYM_name(2:2)
 IsGAllowed_MD = .TRUE.
  seo = sum(mod(g+100,2))
if ((seo.eq.1).or.(seo.eq.2)) then
  IsGAllowed_MD = .FALSE.
end if

 
end function IsGAllowed_MD

!==============================================================

recursive subroutine GetDynMat_MD(cell, atom_ntype, atom_type, maxnumincell, apos, listroot, listrootw, rlp, &
                                  DynMat, nns, nnw, BlochMode)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDynMat_MD

use local
use typedefs
use io
use crystal
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

type(unitcell)                   :: cell
integer(kind=irg),INTENT(IN)     :: ATOM_ntype
integer(kind=irg),INTENT(IN)     :: atom_type(maxpasym)
integer(kind=irg),INTENT(IN)     :: maxnumincell
real(kind=dbl),INTENT(IN)        :: apos(maxnumincell, 5)
type(reflisttype),pointer        :: listroot
type(reflisttype),pointer        :: listrootw
type(gnode),INTENT(INOUT)        :: rlp
!f2py intent(in,out) ::  rlp
complex(kind=dbl),INTENT(INOUT)  :: DynMat(nns,nns)
!f2py intent(in,out) ::  DynMat
integer(kind=irg),INTENT(IN)     :: nns
integer(kind=irg),INTENT(IN)     :: nnw
character(5),INTENT(IN),OPTIONAL :: BlochMode   ! 'Bloch' or 'Struc'

complex(kind=dbl)                :: czero, ughp, uhph, weaksum, cv, Agh, Ahgp, Ahmgp, Ahg, weakdiagsum, pq0, Ahh, Agpgp, ccpi 
real(kind=dbl)                   :: weaksgsum, tpi, Pioxgp
real(kind=sgl)                   :: Upz
integer(kind=sgl)                :: ir, ic, ll(3), istat, wc, gp(3), imh, imk, iml
type(reflisttype),pointer        :: rlr, rlc, rlw
character(1)                     :: AorD

complex(kind=dbl)                :: LUTrep(3), LUTqgrep(3)
real(kind=dbl)                   :: start, finish
complex(kind=dbl),allocatable    :: LUTqg(:,:,:)


czero = cmplx(0.0,0.0,dbl)      ! complex zero
tpi = 2.D0 * cPi
ccpi = cmplx(cPi,0.0D0,dbl)

nullify(rlr)
nullify(rlc)
nullify(rlw)



! if Blochmode = Struc, we compute the structure matrix A directly

! we are always doing struct mode so I got rid of the section on bloch

!------------------------------------------------------------------------------------------------------------------
! I have changed it to save the entries of lut%qg after it uses them so if they are reused we save a calculation
! because I think it was using a lot of time to recalculate them every time
!------------------------------------------------------------------------------------------------------------------

! initialize the LUT to -1 -> code will check for this value to see if it needs to calculate an entry
! if it is not -1 that value has already been calculated so we can reuse it
gp = shape(cell%LUTqg)
imh = (gp(1)-1)/4
imk = (gp(2)-1)/4
iml = (gp(3)-1)/4

allocate(LUTqg(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml))
LUTqg = cmplx(-1.0,0.0)

! note that the factor of i pi is added in at the end...
DynMat = czero
call CalcUcg_MD(cell, rlp, (/0,0,0/), atom_ntype, atom_type, maxnumincell, apos)
pq0 = cmplx(0.D0,1.D0/rlp%xgp,dbl)

rlr => listroot%next
ir = 1
do

  if (.not.associated(rlr)) EXIT !exit after doing the last strong reflection
  rlc => listroot%next
  ic = 1
  do
    if (.not.associated(rlc)) EXIT
    if (ic.ne.ir) then  ! not a diagonal entry
  ! here we need to do the Bethe corrections if necessary

  ! this loop is what's slow - each iteration of the loop is only about 0.00005 seconds but the whole loop is 0.005
      if (nnw.ne.0) then ! there are weak reflections
        weaksum = czero
        rlw => listrootw ! start at the first weak reflection
        do ! loop over the weak reflections to get weaksum
          if (.not.associated(rlw)) EXIT ! exit after the last weak reflection
         
          ll = rlr%hkl - rlw%hkl
          !Agh = cell%LUTqg(ll(1),ll(2),ll(3)) 
          if (LUTqg(ll(1),ll(2),ll(3)) == -1) then! haven't calculated this one yet
            call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos) 
            LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
            Agh = rlp%qg
          else ! have calcuated this one already
            Agh = LUTqg(ll(1),ll(2),ll(3)) 
          end if
                    
          ll = rlw%hkl - rlc%hkl
          !Ahgp = cell%LUTqg(ll(1),ll(2),ll(3)) 
          if (LUTqg(ll(1),ll(2),ll(3)) == -1) then ! haven't calculated this one yet
            call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos)
            LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
            Ahgp = rlp%qg
          else 
            Ahgp = LUTqg(ll(1),ll(2),ll(3)) 
          end if

         

  ! denominator Ahh - Ag'g'
          Ahh = cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
          Agpgp = cmplx(2.D0 * rlc%sg,0.D0,dbl) + pq0
          weaksum = weaksum +  Agh * Ahgp / (Ahh - Agpgp)
          rlw => rlw%nextw
        end do

  ! and correct the dynamical matrix element to become a Bethe potential coefficient
        ll = rlr%hkl - rlc%hkl
        !DynMat(ir,ic) = cell%LUTqg(ll(1),ll(2),ll(3))  -  weaksum
        if (LUTqg(ll(1),ll(2),ll(3)) == -1) then 
          call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos)
          LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
          DynMat(ir,ic) = rlp%qg - weaksum
        else
          DynMat(ir,ic) = LUTqg(ll(1),ll(2),ll(3))  -  weaksum
        end if


      else  ! there are no weak reflections
        ll = rlr%hkl - rlc%hkl
        if (LUTqg(ll(1),ll(2),ll(3)) == -1) then 
          call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos)
          !DynMat(ir,ic) = cell%LUTqg(ll(1),ll(2),ll(3))
          DynMat(ir,ic) = rlp%qg
          LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
        else
          DynMat(ir,ic) = LUTqg(ll(1),ll(2),ll(3))
        end if
      end if

    else  ! it is a diagonal entry, so we need the excitation error and the absorption length
  ! determine the total contribution of the weak beams
      if (nnw.ne.0) then ! there are weak reflections
        weakdiagsum = 0.D0
        rlw => listrootw
        do
          if (.not.associated(rlw)) EXIT
            
            ll = rlr%hkl - rlw%hkl
            !Agh = cell%LUTqg(ll(1),ll(2),ll(3))
            if (LUTqg(ll(1),ll(2),ll(3)) == -1) then ! haven't calculated this one yet
              call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos)
              LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
              Agh = rlp%qg
            else
              Agh = LUTqg(ll(1),ll(2),ll(3))
            end if

            ll = -ll
            !Ahg = cell%LUTqg(-ll(1),-ll(2),-ll(3)) 
            if (LUTqg(ll(1),ll(2),ll(3)) == -1) then ! haven't calculated this one yet
              call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos)
              Ahg = rlp%qg
              LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
            else
              Ahg = LUTqg(ll(1),ll(2),ll(3))
            end if 


  ! denominator Ahh - Agg
            Ahh = cmplx(2.D0 * rlw%sg,0.D0,dbl) + pq0
            Agpgp = cmplx(2.D0 * rlr%sg,0.D0,dbl) + pq0
            weakdiagsum = weakdiagsum +  Agh * Ahg  / (Ahh - Agpgp)
            rlw => rlw%nextw
        end do
        DynMat(ir,ir) = cmplx( 2.D0 * rlr%sg, 0.D0, dbl) + pq0 - weakdiagsum


      else ! no weak reflections
        DynMat(ir,ir) = cmplx( 2.D0 * rlr%sg, 0.D0,dbl) + pq0 
      end if
     end if       
     rlc => rlc%nexts
     ic = ic + 1
  end do        
  rlr => rlr%nexts
  ir = ir+1
end do
DynMat = DynMat * ccpi ! cmplx(0.D0, cPi)

deallocate(LUTqg)

end subroutine GetDynMat_MD


!=======================================================

!==============================================================

!--------------------------------------------------------------------------
!
! SUBROUTINE: MDSortData
!
!> @author Joseph Tessmer, Carnegie Mellon University
!
!> @brief Take in unsorted atom data and sort into cells
!
!> @param numatoms number of atoms in the section
!> @param rawatomdata the unsorted atom data in an array shaped like (numatoms,columns)
!> @param maxnumincell maximum number of atoms allowed in one cell
!> @param rotation the rotation about the z axis
!> @param lpabc the lattice parameters to segment into (a,b,c)
!> @param kji how many cells in k,j,i
!> @param labelarray 2d array that holds atom Information
!> @param posarray 4d array that holds which atoms (from labelarray) (atomindex,z,y,x)
! 
!> @date    10/14/17 JNT 1.0 original
!--------------------------------------------------------------------------

recursive subroutine MDSortData(numatoms,rawatomdata,maxnumincell,lpabc,kji,labelarray,posarray)
!DEC$ ATTRIBUTES DLLEXPORT :: MDSortData

use quaternions

integer(kind=irg),INTENT(IN)     :: numatoms
real(kind=sgl),INTENT(IN)        :: rawatomdata(5,numatoms)
integer(kind=irg),INTENT(IN)     :: maxnumincell
real(kind=dbl),INTENT(IN)        :: lpabc(3)
integer(kind=irg),INTENT(IN)     :: kji(3)
real(kind=dbl),INTENT(OUT)       :: labelarray(4,numatoms)
integer(kind=irg),INTENT(OUT)    :: posarray(maxnumincell,kji(1),kji(2),kji(3))

integer(kind=sgl)                :: p, q
integer(kind=irg)                :: xyz(3)

! some of these should be in the nml file - I will do that later
! labelarray components
! 1 = x 
! 2 = y
! 3 = z
! 4 = type 
! initially 1,2,3 are the full coordinates (i.e. not relative to the cell they are in) but they are replaced by relative coordinates later

! ALSO - need to decide on the the final structure of the input file
! I will assume it is X on 2, Y on 3, Z on 4, Type on 5

! we will need to initialize the first dimension of posarray to -1
posarray(1:maxnumincell,1:kji(1),1:kji(2),1:kji(3)) = -1

do p = 1, numatoms
    labelarray(1:4,p) = rawatomdata(2:5,p)

    ! get the i,j,k values for this atom
    xyz(1) = FLOOR(labelarray(1,p)/lpabc(1)) 
    xyz(2) = FLOOR(labelarray(2,p)/lpabc(2))
    xyz(3) = FLOOR(labelarray(3,p)/lpabc(3))

    

    xyz = xyz + 1


    ! put the location of this atom in labelarray into posarray
    do q = 1, maxnumincell
        if (posarray(q,xyz(3),xyz(2),xyz(1)) == -1) THEN
            posarray(q,xyz(3),xyz(2),xyz(1)) = p
            EXIT
        end if
    end do
    

    ! now we save the relative atom coordinates over the total atom coordinates in labelarray
    labelarray(1,p) = MODULO(labelarray(1,p),lpabc(1))/lpabc(1)
    labelarray(2,p) = MODULO(labelarray(2,p),lpabc(2))/lpabc(2)
    labelarray(3,p) = MODULO(labelarray(3,p),lpabc(3))/lpabc(3)

end do

! now labelarray has a list of atoms with relative x,y,z and type
! posarray has a list of the atoms in cell kji at (1:maxnumincell,k,j,i) and you can get the relative posiitons and types from labelarray
! entries in posarray that do not point to atoms have the value -1 so we know to stop when m,k,j,i = -1
end subroutine MDSortData


!==============================================================


!--------------------------------------------------------------------------
!
! SUBROUTINE: GetDynMatMasterMD
!
!> @author Joseph, Carnegie Mellon University
!
!> @brief compute the dynamical matrix, WITHOUT Bethe potentials - modified for atom coordinates
!
!> @param cell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!
!> @date  04/22/14 MDG 1.0 new library version
!> @date  06/15/14 MDG 2.0 updated for removal of globals
!> @date  06/17/14 MDG 2.1 added listroot pointers etc to accommodate multiple threads
!> @date  06/18/14 MDG 2.2 corrected some pointer allocation errors in other routines; this one now works fine.
!--------------------------------------------------------------------------
recursive subroutine GetDynMatMasterMD(cell, listroot, DynMat, rlp, maxnumincell, atom_ntype, atom_type, apos, nref, &
                                        gglist, gp, LUTqg, interpolate, dwflg)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDynMatMasterMD

use local
use typedefs
use io
use crystal
use diffraction
use kvectors
use gvectors
use constants

IMPLICIT NONE

type(unitcell)                   :: cell
type(reflisttype),pointer        :: listroot
complex(kind=dbl),INTENT(INOUT)  :: DynMat(nref,nref)
!f2py intent(in,out) ::  DynMat
type(gnode),INTENT(INOUT)        :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN)     :: ATOM_ntype
integer(kind=irg),INTENT(IN)     :: maxnumincell
integer(kind=irg),INTENT(IN)     :: atom_type(maxpasym)
real(kind=dbl),INTENT(IN)        :: apos(maxnumincell, 5)
integer(kind=irg),INTENT(IN)     :: nref
real(kind=sgl),INTENT(IN)        :: gglist(4,nref)
integer(kind=sgl),INTENT(IN)     :: gp(3)
complex(kind=dbl),INTENT(INOUT)  :: LUTqg(-(gp(1)-1)/2:(gp(1)-1)/2,-(gp(2)-1)/2:(gp(2)-1)/2,-(gp(3)-1)/2:(gp(3)-1)/2)
!f2py intent(in,out) ::  LUTqg
logical,OPTIONAL,INTENT(IN)      :: interpolate
logical,OPTIONAL,INTENT(IN)      :: dwflg


complex(kind=dbl)                :: czero, ughp, uhph, weaksum, qg0
real(kind=dbl)                   :: weaksgsum
real(kind=sgl)                   :: Upz
integer(kind=sgl)                :: ir, ic, ll(3), istat, wc!,gp(3), imh, imk, iml
logical                          :: interp
integer(kind=sgl)                :: atomtypes, scatfacshape(2)

!complex(kind=dbl),allocatable    :: LUTqg(:,:,:)

interp = .FALSE.
if (present(interpolate)) then
  if (interpolate.eqv..TRUE.) interp=.TRUE.
end if

if (rlp%method.eq.'IP') then 
  scatfacshape = shape(cell%scatfac)
  atomtypes = scatfacshape(2)
end if

czero = cmplx(0.0,0.0,dbl)      ! complex zero

! nullify(rlr)
! nullify(rlc)

! gp = shape(cell%LUTqg)
! imh = (gp(1)-1)/4
! imk = (gp(2)-1)/4
! iml = (gp(3)-1)/4

! allocate(LUTqg(-2*imh:2*imh,-2*imk:2*imk,-2*iml:2*iml))
LUTqg = cmplx(-1.0,0.0)

DynMat = czero
call CalcUcg_MD(cell, rlp, (/0,0,0/), atom_ntype, atom_type, maxnumincell, apos, interp, atomtypes, dwflg)
qg0 = cell%LUTqg(0,0,0)

!rlr => listroot%next
ir = 1
    do
      ! loop over each reflection
        !if (.not.associated(rlr)) EXIT
        !rlc => listroot%next
        if (ir > nref) EXIT
        ic = 1
        do
          ! loop over each reflection again
            !if (.not.associated(rlc)) EXIT
            if (ic > nref) EXIT
            if (ic.ne.ir) then  ! not a diagonal entry
                !ll = rlr%hkl - rlc%hkl
                ll = gglist(1:3,ir) - gglist(1:3,ic)
                if (LUTqg(ll(1),ll(2),ll(3)) == -1) then 
                  call CalcUcg_MD(cell,rlp,ll,atom_ntype, atom_type, maxnumincell, apos, interp, atomtypes, dwflg)
                  LUTqg(ll(1),ll(2),ll(3)) = rlp%qg
                  DynMat(ir,ic) = LUTqg(ll(1),ll(2),ll(3))
                else
                  DynMat(ir,ic) = LUTqg(ll(1),ll(2),ll(3))
                end if
            else
                !DynMat(ir,ic) = cmplx(2.D0*rlc%sg,0.D0) + qg0
                DynMat(ir,ic) = cmplx(2.D0*gglist(4,ic),0.D0) + qg0
            end if
            !rlc => rlc%next
            ic = ic + 1
        end do
        !rlr => rlr%next
        ir = ir+1
    end do

DynMat = DynMat * cmplx(cPi,0.D0)


! if (allocated(LUTqg)) DEALLOCATE(LUTqg)

end subroutine GetDynMatMasterMD

!==============================================================
!--------------------------------------------------------------------------
!
! SUBROUTINE: DeallocateSgList
!
!> @author Joseph, Carnegie Mellon University
!
!> @brief Deallocate a linked list (reflist) properly
!
!> @param cell unit cell pointer
!> @param listroot top of the main reflection list
!> @param listrootw top of the weak reflection list
!> @param Dyn dynamical scattering structure
!> @param nns number of strong reflections
!> @param nnw number of weak reflections
!
!--------------------------------------------------------------------------

recursive subroutine DeallocateSgList(linkedlist)
!DEC$ ATTRIBUTES DLLEXPORT :: DeallocateSgList

use local
use typedefs

type(sggamma),pointer,INTENT(INOUT)        :: linkedlist
!f2py intent(in,out) ::  linkedlist
type(sggamma),pointer                      :: current, next


if (.not. associated(linkedlist%next)) return

current => linkedlist%next
next => current%next
do
   deallocate(current)
   if (.not. associated(next)) exit
   current => next
   next => current%next
enddo


end subroutine DeallocateSgList


!--------------------------------------------------------------------------
!
! SUBROUTINE: DispGridInterp
!
!> @author Joseph, Carnegie Mellon University
!
!> @brief Resample displacement field grid on different axes, e.g. for tilt 
!   series of images
!
!> @param boxdims dimensions of input box 
!> @param tiltangle Angle to tilt the box of displacements
!> @param dispfield Untilted box of displacements
!> @param dispfieldinterp New, tilted box of displacements 
!
!--------------------------------------------------------------------------

recursive subroutine DispGridInterp(boxdims,tiltangle,dispfield,dispfieldinterp,tiltY)
!DEC$ ATTRIBUTES DLLEXPORT :: DispGridInterp

use local
use typedefs
use rotations

integer(kind=irg),INTENT(IN)               :: boxdims(3)
real(kind=sgl),INTENT(IN)                  :: tiltangle
real(kind=sgl),INTENT(IN)                  :: dispfield(4,boxdims(1),boxdims(2),boxdims(3))
real(kind=sgl),ALLOCATABLE,INTENT(OUT)     :: dispfieldinterp(:,:,:,:)
logical,optional,INTENT(IN)                :: tiltY

real(kind=sgl)                             :: axpair(4)
real(kind=sgl)                             :: om(3,3)
real(kind=sgl)                             :: displist(6,boxdims(1)*boxdims(2)*boxdims(3))

! first, get the rotation matrix for the rotation about X
! tiltangle@100
axpair = (/ 1.0, 0.0, 0.0, tiltangle /)
if (tiltY) then 
    ! we want to rotate around Y instead
    ! tiltangle@010
    axpair = (/ 0.0,  1.0, 0.0, tiltangle /)
end if

om = ax2om(axpair)

! Rotate the grid points as well as the displacement at each grid point
! this would be an ACTIVE rotation

do ii = 1,boxdims(1)
    do jj = 1,boxdims(2)
        do kk = 1,boxdims(3)
            displist(1:3,kk + (jj-1)*boxdims(3) + (ii-1)*boxdims(2)*boxdims(1)) = matmul(om,(/ ii, jj, kk /))
            displist(4:6,kk + (jj-1)*boxdims(3) + (ii-1)*boxdims(2)*boxdims(1)) = matmul(om,dispfield(1:3,ii,jj,kk))
        end do 
    end do 
end do

end subroutine DispGridInterp


end module
