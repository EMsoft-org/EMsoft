! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMTKDmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMTKDmaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMTKDmaster computes the energy-dependent master TKD pattern for a given structure
!
!> @todo implement full symmetry use (trigonal/rhombohedral is currently not implemented) 
!>
!> @date  01/15/17  MDG 1.0 copied from EMEBSDmaster program (version 5.5)
!> @date  10/06/17  MDG 1.1 added combinesites option for very large unit cells
!--------------------------------------------------------------------------
program EMTKDmaster

use local
use NameListTypedefs
use NameListHandlers
use JSONsupport
use json_module
use files
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(TKDMasterNameListType)             :: emnl
integer(kind=irg)                       :: res, error_cnt

nmldeffile = 'EMTKDmaster.nml'
progname = 'EMTKDmaster.f90'
progdesc = 'TKD Energy-dependent Master Pattern Simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 29, 0 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  ! call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetTKDMasterNameList(nmldeffile,emnl)
end if

! generate a set of master EBSD patterns
 call ComputeMasterPattern(emnl, progname, nmldeffile)

end program EMTKDmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a TKD master pattern as a function of energy
!
!> @param emnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 01/15/17  MDG 1.0 original (copied form version 6.6 of EMEBSDmaster::ComputeKMasterPattern)
!> @date 08/08/17  MDG 1.1 removed normal absorption from depth integration to avoid double counting
!--------------------------------------------------------------------------
subroutine ComputeMasterPattern(emnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use MBmodule
use symmetry
use crystal
use constants
use error
use gvectors
use kvectors
use io
use local
use files
use diffraction
use multibeams
use timing
use Lambert
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use omp_lib
use math
use stringconstants

IMPLICIT NONE

type(TKDMasterNameListType),INTENT(INOUT)  :: emnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

real(kind=dbl)          :: ctmp(192,3), arg, Radius, xyz(3)
integer(HSIZE_T)        :: dims4(4), cnt4(4), offset4(4)
integer(HSIZE_T)        :: dims3(3), cnt3(3), offset3(3)
integer(kind=irg)       :: isym,i,j,ik,npy,ipx,ipy,ipz,debug,iE,izz, izzmax, iequiv(3,48), nequiv, num_el, MCnthreads, & ! counters
                           numk, & ! number of independent incident beam directions
                           ir,nat(maxpasym),kk(3), skip, ijmax, one, NUMTHREADS, TID, SamplingType, tickstart, &
                           numset,n,ix,iy,iz, io_int(6), nns, nnw, nref, Estart, &
                           istat,gzero,ic,ip,ikk, totstrong, totweak, jh, ierr, nix, niy, nixp, niyp     ! counters
real(kind=dbl)          :: tpi,Znsq, kkl, DBWF, kin, delta, h, lambda, omtl, srt, dc(3), xy(2), edge, scl, tmp, dx, dxm, dy, dym !
real(kind=sgl)          :: io_real(5), selE, kn, FN(3), kkk(3), tstop, bp(4)
real(kind=sgl),allocatable      :: EkeVs(:), svals(:), auxNH(:,:,:,:), auxSH(:,:,:,:)  ! results
real(kind=sgl),allocatable      :: mLPNH(:,:,:,:), mLPSH(:,:,:,:), masterSPNH(:,:,:), masterSPSH(:,:,:)  ! results
complex(kind=dbl)               :: czero
complex(kind=dbl),allocatable   :: Lgh(:,:), Sgh(:,:,:)
logical                 :: usehex, switchmirror, verbose
character(fnlen)        :: xtalname

! Monte Carlo derived quantities
integer(kind=irg)       :: numEbins, numzbins, nsx, nsy, hdferr, nlines, lastEnergy, numsites    ! variables used in MC energy file
real(kind=dbl)          :: EkeV, Ehistmin, Ebinsize, depthmax, depthstep, etotal, nabsl ! enery variables from MC program
integer(kind=irg),allocatable :: accum_e(:,:,:), accum_z(:,:,:,:), thick(:), acc_z(:,:,:,:)
real(kind=sgl),allocatable :: lambdaE(:,:)
character(fnlen)        :: oldprogname, groupname, energyfile, outname, datagroupname, attributename, HDF_FileVersion
character(8)            :: MCscversion
character(11)           :: dstr
character(15)           :: tstrb
character(15)           :: tstre
logical                 :: f_exists, readonly, overwrite=.TRUE., insert=.TRUE., stereog, g_exists, xtaldataread, FL
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)
character(fnlen,kind=c_char)                     :: line2(1)

type(unitcell)                  :: cell
type(DynType),save              :: Dyn
type(gnode),save                :: rlp
type(reflisttype),pointer       :: reflist,firstw, rltmp
type(BetheParameterType)        :: BetheParameters
type(kvectorlist),pointer       :: khead, ktmp
real(kind=sgl),allocatable      :: karray(:,:)
integer(kind=irg),allocatable   :: kij(:,:)
complex(kind=dbl),allocatable   :: DynMat(:,:)
character(fnlen)                :: dataset, instring

type(HDFobjectStackType)          :: HDF_head

!$OMP THREADPRIVATE(rlp) 

stereog = .TRUE.

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(tickstart)

tpi = 2.D0*cPi
czero = cmplx(0.D0,0.D0)

!=============================================
!=============================================
! this program needs Monte Carlo information from the EMMCfoil program...
! ---------- read Monte Carlo .h5 output file and extract necessary parameters

xtaldataread = .FALSE.

call h5open_EMsoft(hdferr)

! does the file exist ?
energyfile = trim(EMsoft_getEMdatapathname())//trim(emnl%energyfile)
energyfile = EMsoft_toNativePath(energyfile)
call Message('opening '//trim(emnl%energyfile), frm = "(A)" )
inquire(file=energyfile, exist=f_exists)

if (.not.f_exists) then
  call FatalError('ComputeMasterPattern','Monte Carlo input file does not exist')
end if

! open the MC file using the default properties.
! readonly = .TRUE.
! hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

hdferr =  HDF_openFile(energyfile, HDF_head)

! next we need to make sure that this EM file actually contains a Monte Carlo 
! data set; if it does, then we can try to read all the information
groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)
datagroupname = SC_MCfoil
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ComputeMasterPattern','This HDF file does not contain any TKD Monte Carlo data')
end if
call HDF_pop(HDF_head)

! check whether or not the MC file was generated using DREAM.3D
! this is necessary so that the proper reading of fixed length vs. variable length strings will occur.
! this test sets a flag inside the HDFsupport module so that the proper reading routines will be employed
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
datagroupname = SC_MCfoil
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ComputeMasterPattern','This HDF file does not contain Monte Carlo header data')
end if
call HDF_pop(HDF_head)

groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_MCfoil
hdferr = HDF_openGroup(groupname, HDF_head)
FL = .FALSE.
datagroupname = 'FixedLength'
FL = CheckFixedLengthflag(datagroupname, HDF_head)
if (FL.eqv..TRUE.) then 
  call Message('Input file was generated by a program using fixed length strings (e.g., DREAM.3D)')
end if
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! open the namelist group
groupname = SC_NMLparameters
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLfoilNameList
hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_xtalname
call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
xtalname = trim(stringarray(1))
if (xtaldataread.eqv..TRUE.) cell%fname = trim(xtalname)

dataset = SC_numsx
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, nsx)
nsx = (nsx - 1)/2
nsy = nsx

dataset = SC_EkeV
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, EkeV)

dataset = SC_Ehistmin
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ehistmin)

dataset = SC_Ebinsize
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ebinsize)

dataset = SC_depthmax
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, depthmax)

dataset = SC_depthstep
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, depthstep)

! close the name list group
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! initialize the crystallographic parameters; we need to do this here rather than
! above because we need the EkeV parameter ... 
datagroupname = '/CrystalData'
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call Message('Master file does not contain crystal structure data; will look for .xtal file instead.')
else
! there is CrystalData present in this file, so let's read it here...
  !nullify(cell)        
  !allocate(cell)        
  verbose = .TRUE.
  call Initialize_Cell(cell,Dyn,rlp,xtalname, emnl%dmin, sngl(EkeV), verbose, HDF_head)
  xtaldataread = .TRUE.
end if

! open the Data group
groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)

datagroupname = 'MCfoil'
hdferr = HDF_openGroup(datagroupname, HDF_head)

! read data items 
dataset = SC_numEbins
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numEbins)

dataset = SC_numzbins
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numzbins)

dataset = SC_totnumel
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, num_el)

dataset = SC_accumz
! dims4 =  (/ numEbins, numzbins, 2*(nsx/10)+1,2*(nsy/10)+1 /)
call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
allocate(accum_z(numEbins,numzbins,-nsx/10:nsx/10,-nsy/10:nsy/10),stat=istat)
accum_z = acc_z
deallocate(acc_z)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

! close the fortran interface
call h5close_EMsoft(hdferr)

etotal = num_el 

io_int(1) = etotal
call WriteValue('total number of electrons in TKD-MC data set ', io_int, 1)

call Message(' -> completed reading '//trim(emnl%energyfile), frm = "(A/)")

!=============================================
!=============================================


!=============================================
!=============================================
! crystallography section; first we test to see if the CrystalData is present in the 
! master file; if it is then we read it (actually, that was already done above); if 
! it is not, then we look for the xtalname file... if this does not exist, then we abort
! the program (done deep inside the Initialize_Cell call.

if (xtaldataread.eqv..FALSE.) then
  !nullify(cell)        
  !allocate(cell)        

  verbose = .TRUE.
  call Initialize_Cell(cell,Dyn,rlp,xtalname, emnl%dmin, sngl(EkeV), verbose)
end if 

! allocate and compute the Sgh loop-up table
 numset = cell%ATOM_ntype  
 call Initialize_SghLUT(cell,emnl%dmin, numset, nat, verbose)

! if the combinesites parameter is .TRUE., then we only need to 
! allocate a dimension of 1 in the master pattern array since we are adding 
! together the master patterns for all sites in the asymmetric unit.
if (emnl%combinesites.eqv..TRUE.) then
  numsites = 1
else
  numsites = numset
end if

! the following line needs to be verified ... 
!  hexset = .TRUE.    ! if hexagonal structure this switch selects between three and four index notation (4 if true)

! determine the point group number
 j=0
 do i=1,32
  if (SGPG(i).le.cell%SYM_SGnum) j=i
 end do
 isym = j

! here is new code dealing with all the special cases (quite a few more compared to the 
! Laue group case)...  isym is the point group number. Once the symmetry case has been
! fully determined (taking into account things like 31m and 3m1 an such), then the only places
! that symmetry is handled are the modified Calckvectors routine, and the filling of the modified
! Lambert projections after the dynamical simulation step.  We are also changing the name of the 
! sr array (or srhex) to mLPNH and mLPSH (modified Lambert Projection Northern/Southern Hemisphere),
! and we change the output HDF5 file a little as well. We need to make sure that the EMEBSD program
! issues a warning when an old format HDF5 file is read.  

! Here, we encode isym into a new number that describes the sampling scheme; the new schemes are 
! described in detail in the EBSD manual pdf file.

SamplingType = PGSamplingType(isym)

! next, intercept the special cases (hexagonal vs. rhombohedral cases that require special treatment)
if ((SamplingType.eq.-1).or.(isym.eq.14).or.(isym.eq.26)) then 
  SamplingType = getHexvsRho(cell,isym)
  write (*,*) ' --> ',cell%SYM_SGnum, isym, SamplingType,cell%SG%SYM_second,cell%SYM_SGset 
end if 

! if the point group is trigonal or hexagonal, we need to switch usehex to .TRUE. so that
! the program will use the hexagonal sampling method
usehex = .FALSE.
if ((cell%xtal_system.eq.4).or.(cell%xtal_system.eq.5)) usehex = .TRUE.

! ---------- end of symmetry and crystallography section
!=============================================
!=============================================

!=============================================
!=============================================
! this is where we determine the value for the thickness integration limit for the CalcLgh3 routine...
allocate(EkeVs(numEbins),thick(numEbins))

do i=1,numEbins
  EkeVs(i) = Ehistmin + float(i-1)*Ebinsize
end do

! then, for each energy determine the 95% histogram thickness
izzmax = 0
do iE = 1,numEbins
 do ix=-nsx/10,nsx/10
  do iy=-nsy/10,nsy/10
   istat = sum(accum_z(iE,:,ix,iy))
   izz = 1
   do while (sum(accum_z(iE,1:izz,ix,iy)).lt.(0.99*istat)) 
    izz = izz+1
   end do
   if (izz.gt.izzmax) izzmax = izz
  end do
 end do
 thick(iE) = dble(izzmax) * depthstep
end do

! remove the normal absorption contribution from the MC results
izz = nint(maxval(thick)/depthstep)
allocate(lambdaE(1:numEbins,1:izz),stat=istat)
do iE=1,numEbins
 cell%voltage = Ekevs(iE)
 call CalcUcg(cell,rlp,(/0,0,0/))
 nabsl = rlp%xgp
 do iz=1,izz
  lambdaE(iE,iz) = float(sum(accum_z(iE,iz,-nsx/10:nsx/10,-nsy/10:nsy/10)))/etotal
  lambdaE(iE,iz) = lambdaE(iE,iz) * exp(2.0*sngl(cPi)*(iz-1)*depthstep/nabsl)
 end do
end do

write(*,*) 'range of lambdaE array ',minval(lambdaE), maxval(lambdaE)

! and get rid of the accum_z array
deallocate(accum_z)
! ---------- end of 'read Monte Carlo output file and extract necessary parameters' section
!=============================================
!=============================================


!=============================================
!=============================================
! ---------- a couple of initializations
   npy = emnl%npx
   allocate(svals(numset),stat=istat)
   gzero = 1  ! index of incident beam
   debug = 0  ! no longer used
! ----------
!=============================================
!=============================================

!=============================================
!=============================================
! ---------- allocate memory for the master patterns
  allocate(mLPNH(-emnl%npx:emnl%npx,-npy:npy,1,1:numsites),stat=istat)
  allocate(mLPSH(-emnl%npx:emnl%npx,-npy:npy,1,1:numsites),stat=istat)
  allocate(masterSPNH(-emnl%npx:emnl%npx,-npy:npy,1))
  allocate(masterSPSH(-emnl%npx:emnl%npx,-npy:npy,1))

! set various arrays to zero
  if (emnl%uniform.eqv..TRUE.) then
   mLPNH = 1.0
   mLPSH = 1.0
   masterSPNH = 1.0
   masterSPSH = 1.0
  else
   mLPNH = 0.0
   mLPSH = 0.0
   masterSPNH = 0.0
   masterSPSH = 0.0
  end if
! ---------- end allocate memory for the master patterns
!=============================================
!=============================================

! force dynamical matrix routine to read new Bethe parameters from file
! this will all be changed with the new version of the Bethe potentials
  call Set_Bethe_Parameters(BetheParameters,.TRUE.)

!=============================================
! should we create a new file or open an existing file?
!=============================================
  lastEnergy = -1
  outname = trim(EMsoft_getEMdatapathname())//trim(emnl%outname)
  outname = EMsoft_toNativePath(outname)

if (emnl%restart.eqv..TRUE.) then
! in this case we need to check whether or not the file exists, then open
! it and read the value of the last energy level that was simulated and written
! to that file; if this level is different from the lowest energy level we 
! know that there is at least one more level to be simulated.  If it is equal,
! then we can abort the program here.

  inquire(file=trim(outname), exist=f_exists)
  if (.not.f_exists) then 
    call FatalError('ComputeMasterPattern','restart HDF5 file does not exist')
  end if
  
!=============================================
! open the existing HDF5 file 
!=============================================
  datagroupname = 'TKDmaster'
  nullify(HDF_head%next)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(outname, HDF_head, readonly)

! all we need to get from the file is the lastEnergy parameter
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_lastEnergy
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, lastEnergy)

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

else

!=============================================
! create or update the HDF5 output file
!=============================================

  nullify(HDF_head%next)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Open an existing file or create a new file using the default properties.
  if (trim(energyfile).eq.trim(outname)) then
    hdferr =  HDF_openFile(outname, HDF_head)
  else
    hdferr =  HDF_createFile(outname, HDF_head)
  end if

! write the EMheader to the file
  datagroupname = 'TKDmaster'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_TKDmasterNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteTKDMasterNameList(HDF_head, emnl)
  call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

! create the TKDmaster group and add a HDF_FileVersion attribbute to it 
  hdferr = HDF_createGroup(datagroupname, HDF_head)
  HDF_FileVersion = '4.0'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0
! =====================================================

dataset = SC_xtalname
  stringarray(1)= trim(xtalname)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  end if

dataset = SC_numset
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head)
  end if

dataset = SC_BetheParameters
  bp = (/ BetheParameters%c1, BetheParameters%c2, BetheParameters%c3, BetheParameters%sgdbdiff /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head)
  end if

dataset = SC_lastEnergy
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, lastEnergy, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetInteger(dataset, lastEnergy, HDF_head)
  end if
  
  if (emnl%Esel.eq.-1) then
dataset = SC_numEbins
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, numEbins, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, numEbins, HDF_head)
    end if
  
dataset = SC_EkeVs
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, numEbins, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, numEbins, HDF_head)
    end if
  else
dataset = SC_numEbins
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, one, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, one, HDF_head)
    end if
  
dataset = SC_selE
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, selE, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, selE, HDF_head)
    end if
  end if  

! create the hyperslabs and write zeroes to them for now
dataset = SC_mLPNH
  dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins, numsites /)
  cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
  offset4 = (/ 0, 0, 0, 0 /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head)
  end if

dataset = SC_mLPSH
  dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins, numsites /)
  cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
  offset4 = (/ 0, 0, 0, 0 /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head)
  end if

dataset = SC_masterSPNH
  dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins /)
  cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
  offset3 = (/ 0, 0, 0 /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head)
  end if

dataset = SC_masterSPSH
  dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins /)
  cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
  offset3 = (/ 0, 0, 0 /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head)
  end if
! =====================================================
! end of HDF_FileVersion = 4.0 write statements
! =====================================================

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)

end if

!=============================================
!=============================================
! figure out what the start energy value is for the energyloop
if (lastEnergy.ne.-1) then
  Estart = lastEnergy-1
  if (Estart.eq.0) then 
    call Message('All energy levels are present in the HDF5 file')
    call Message('No further computations needed.')
    stop 'Program halted.'
  end if
else
  Estart = numEbins
end if


!=============================================
!=============================================
! ---------- from here on, we need to repeat the entire computation for each energy value
! so this is where we could in principle implement an OpenMP approach; alternatively, 
! we could do the inner loop over the incident beam directions in OpenMP (probably simpler)

energyloop: do iE=Estart,1,-1
 if (emnl%uniform.eqv..FALSE.) then
! is this a single-energy run ?
   if (emnl%Esel.ne.-1) then
     if (emnl%Esel.ne.iE) CYCLE energyloop
   end if

! is there any intensity in the Monte Carlo lambda function ?  If not, then skip this energylevel
   if (sum(lambdaE(iE,:)).lt.1.0e-7) then
     io_int(1) = iE
     call WriteValue('There are very few electrons in energy bin ',io_int,1)
     call Message('---> Skipping this energy level ')
     CYCLE energyloop
   end if
   
! print a message to indicate where we are in the computation
   io_int(1)=iE
   call Message('Starting computation for energy bin', frm = "(/A)",advance="no")
   call WriteValue(' ',io_int,1,"(I4)",advance="no")
   io_real(1) = EkeVs(iE)
   call WriteValue('; energy [keV] = ',io_real,1,"(F6.2/)")
   selE = EkeVs(iE)

! set the accelerating voltage
   skip = 3
   cell%voltage = dble(EkeVs(iE))
   call CalcWaveLength(cell, rlp, skip)

!=============================================
! ---------- create the incident beam directions list
! determine all independent incident beam directions (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation;
! note that this needs to be redone for each energy, since the wave vector changes with energy
   nullify(khead)
   if (usehex) then
    call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,emnl%npx,npy,numk, &
                SamplingType,ijmax,'RoscaLambert',usehex)
   else 
    call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,emnl%npx,npy,numk, &
                SamplingType,ijmax,'RoscaLambert',usehex)
   end if
   io_int(1)=numk
   call WriteValue('# independent beam directions to be considered = ', io_int, 1, "(I8)")

! convert part of the kvector linked list into arrays for OpenMP
  allocate(karray(4,numk), kij(3,numk),stat=istat)
! point to the first beam direction
  ktmp => khead
! and loop through the list, keeping k, kn, and i,j
  karray(1:3,1) = sngl(ktmp%k(1:3))
  karray(4,1) = sngl(ktmp%kn)
  kij(1:3,1) = (/ ktmp%i, ktmp%j, ktmp%hs /)
   do ik=2,numk
     ktmp => ktmp%next
     karray(1:3,ik) = sngl(ktmp%k(1:3))
     karray(4,ik) = sngl(ktmp%kn)
     kij(1:3,ik) = (/ ktmp%i, ktmp%j, ktmp%hs /)
   end do
! and remove the linked list
  call Delete_kvectorlist(khead)

  verbose = .FALSE.
  totstrong = 0
  totweak = 0

! ---------- end of "create the incident beam directions list"
!=============================================

! here's where we introduce the OpenMP calls, to spead up the overall calculations...

! set the number of OpenMP threads 
  call OMP_SET_NUM_THREADS(emnl%nthreads)
  io_int(1) = emnl%nthreads
  call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL COPYIN(rlp) &
!$OMP& PRIVATE(DynMat,Sgh,Lgh,ik,FN,TID,kn,ipx,ipy,ix,iequiv,nequiv,reflist,firstw) &
!$OMP& PRIVATE(kkk,nns,nnw,nref,svals,io_int)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC,100)    
! ---------- and here we start the beam direction loop
   beamloop:do ik = 1,numk

!=============================================
! ---------- create the master reflection list for this beam direction
! Then we must determine the masterlist of reflections (also a linked list);
! This list basically samples a large reciprocal space volume; it does not 
! distinguish between zero and higher order Laue zones, since that 
! distinction becomes meaningless when we consider the complete 
! reciprocal lattice.  
     nullify(reflist)
     kkk = karray(1:3,ik)
     FN = kkk

     call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, kkk, emnl%dmin, nref, verbose)
! ---------- end of "create the master reflection list"
!=============================================


! determine strong and weak reflections
     nullify(firstw)
     nns = 0
     nnw = 0
     call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, nref, nns, nnw)

! generate the dynamical matrix
     allocate(DynMat(nns,nns))
     call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)
     totstrong = totstrong + nns
     totweak = totweak + nnw

! then we need to initialize the Sgh and Lgh arrays
     if (allocated(Sgh)) deallocate(Sgh)
     if (allocated(Lgh)) deallocate(Lgh)
     allocate(Sgh(nns,nns,numset),Lgh(nns,nns))
     Sgh = czero
     Lgh = czero
     ! call CalcSgh(cell,reflist,nns,numset,Sgh,nat)
     call getSghfromLUT(cell,reflist,nns,numset,nat,Sgh)
!write(*,*) TID, maxval(abs(Sgh)), minval(abs(Sgh)), nat(1:numset),shape(Sgh),1.0/float(sum(nat(1:numset)))


! solve the dynamical eigenvalue equation for this beam direction  
     kn = karray(4,ik)
     call CalcLgh(DynMat,Lgh,dble(thick(iE)),dble(kn),nns,gzero,depthstep,lambdaE(iE,1:izzmax),izzmax)
     deallocate(DynMat)

! sum over the element-wise (Hadamard) product of the Lgh and Sgh arrays 
     svals = 0.0
     do ix=1,numset
       svals(ix) = real(sum(Lgh(1:nns,1:nns)*Sgh(1:nns,1:nns,ix)))
     end do
     svals = svals/float(sum(nat(1:numset)))

! and store the resulting svals values, applying point group symmetry where needed.
     ipx = kij(1,ik)
     ipy = kij(2,ik)
     ipz = kij(3,ik)
!
     if (usehex) then 
       call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv,usehex)
     else
       if ((cell%SYM_SGnum.ge.195).and.(cell%SYM_SGnum.le.230)) then
         call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv,cubictype=SamplingType)
       else
         call Apply3DPGSymmetry(cell,ipx,ipy,ipz,emnl%npx,iequiv,nequiv)
       end if
     end if
!$OMP CRITICAL
     if (emnl%combinesites.eqv..FALSE.) then
       do ix=1,nequiv
         if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1,1:numset) = svals(1:numset)
         if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1,1:numset) = svals(1:numset)
       end do
     else  ! in this case, we need to sum all the atom contributions and store the sum
       do ix=1,nequiv
         if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1,1) = sum(svals)
         if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1,1) = sum(svals)
       end do
     end if
!$OMP END CRITICAL
  
! if (TID.eq.0) write (*,*) maxval(abs(mLPNH))

     if (mod(ik,5000).eq.0) then
       io_int(1) = ik
       call WriteValue('  completed beam direction ',io_int, 1, "(I8)")
     end if

     call Delete_gvectorlist(reflist)

    end do beamloop

! end of OpenMP portion
!$OMP END PARALLEL

  deallocate(karray, kij)

if (usehex) then
! and finally, we convert the hexagonally sampled array to a square Lambert projection which will be used 
! for all EBSD pattern interpolations;  we need to do this for both the Northern and Southern hemispheres

! we begin by allocating auxiliary arrays to hold copies of the hexagonal data; the original arrays will
! then be overwritten with the newly interpolated data.
  allocate(auxNH(-emnl%npx:emnl%npx,-npy:npy,1,1:numsites),stat=istat)
  allocate(auxSH(-emnl%npx:emnl%npx,-npy:npy,1,1:numsites),stat=istat)
  auxNH = mLPNH
  auxSH = mLPSH

! 
  edge = 1.D0 / dble(emnl%npx)
  scl = float(emnl%npx) 
  do i=-emnl%npx,emnl%npx
    do j=-npy,npy
! determine the spherical direction for this point
      xy = (/ dble(i), dble(j) /) * edge
      dc = LambertSquareToSphere(xy, ierr)
! convert direction cosines to hexagonal Lambert projections
      xy = scl * LambertSphereToHex( dc, ierr )
! interpolate intensity from the neighboring points
      if (ierr.eq.0) then 
        nix = floor(xy(1))
        niy = floor(xy(2))
        nixp = nix+1
        niyp = niy+1
        if (nixp.gt.emnl%npx) nixp = nix
        if (niyp.gt.emnl%npx) niyp = niy
        dx = xy(1) - nix
        dy = xy(2) - niy
        dxm = 1.D0 - dx
        dym = 1.D0 - dy
        mLPNH(i,j,1,1:numsites) = auxNH(nix,niy,1,1:numsites)*dxm*dym + auxNH(nixp,niy,1,1:numsites)*dx*dym + &
                             auxNH(nix,niyp,1,1:numsites)*dxm*dy + auxNH(nixp,niyp,1,1:numsites)*dx*dy
        mLPSH(i,j,1,1:numsites) = auxSH(nix,niy,1,1:numsites)*dxm*dym + auxSH(nixp,niy,1,1:numsites)*dx*dym + &
                             auxSH(nix,niyp,1,1:numsites)*dxm*dy + auxSH(nixp,niyp,1,1:numsites)*dx*dy
      end if
    end do
  end do
  deallocate(auxNH, auxSH)
end if

! make sure that the outer pixel rim of the mLPSH patterns is identical to
! that of the mLPNH array.
mLPSH(-emnl%npx,-emnl%npx:emnl%npx,1,1:numsites) = mLPNH(-emnl%npx,-emnl%npx:emnl%npx,1,1:numsites)
mLPSH( emnl%npx,-emnl%npx:emnl%npx,1,1:numsites) = mLPNH( emnl%npx,-emnl%npx:emnl%npx,1,1:numsites)
mLPSH(-emnl%npx:emnl%npx,-emnl%npx,1,1:numsites) = mLPNH(-emnl%npx:emnl%npx,-emnl%npx,1,1:numsites)
mLPSH(-emnl%npx:emnl%npx, emnl%npx,1,1:numsites) = mLPNH(-emnl%npx:emnl%npx, emnl%npx,1,1:numsites)


! get stereographic projections (summed over the atomic positions)
  Radius = 1.0
  do i=-emnl%npx,emnl%npx 
    do j=-emnl%npx,emnl%npx 
      xy = (/ float(i), float(j) /) / float(emnl%npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      xyz = xyz/vecnorm(xyz)
      if (ierr.ne.0) then 
        masterSPNH(i,j,1) = 0.0
        masterSPSH(i,j,1) = 0.0
      else
        masterSPNH(i,j,1) = InterpolateLambert(xyz, mLPNH, emnl%npx, numsites)
        masterSPSH(i,j,1) = InterpolateLambert(xyz, mLPSH, emnl%npx, numsites)
      end if
    end do
  end do

! since these computations can take a long time, here we store 
! all the output at the end of each pass through the energyloop.

  io_int(1) = nint(float(totstrong)/float(numk))
  call WriteValue(' -> Average number of strong reflections = ',io_int, 1, "(I5)")
  io_int(1) = nint(float(totweak)/float(numk))
  call WriteValue(' -> Average number of weak reflections   = ',io_int, 1, "(I5)")

! and here is where the major changes are for version 5.0: all output now in HDF5 format
  call timestamp(datestring=dstr, timestring=tstre)
 end if

  datagroupname = 'TKDmaster'

  nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
  call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
  hdferr =  HDF_openFile(outname, HDF_head)

! update the time string
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  tstop = Time_tock(tickstart)
dataset = SC_Duration
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then     
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
  end if

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)
  
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

! update the current energy level counter, so that the restart option will function
dataset = SC_lastEnergy
  hdferr = HDF_writeDataSetInteger(dataset, iE, HDF_head, overwrite)

! add data to the hyperslab
dataset = SC_mLPNH
  dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins, numsites /)
  cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
  offset4 = (/ 0, 0, iE-1, 0 /)
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head, insert)

dataset = SC_mLPSH
  dims4 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins, numsites /)
  cnt4 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1, numsites /)
  offset4 = (/ 0, 0, iE-1, 0 /)
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head, insert)

dataset = SC_masterSPNH
  dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins /)
  cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
  offset3 = (/ 0, 0, iE-1 /)
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)

dataset = SC_masterSPSH
  dims3 = (/  2*emnl%npx+1, 2*emnl%npx+1, numEbins /)
  cnt3 = (/ 2*emnl%npx+1, 2*emnl%npx+1, 1 /)
  offset3 = (/ 0, 0, iE-1 /)
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

 if ((emnl%Esel.eq.-1).and.(iE.ne.1)) then 
  call Message('Intermediate data stored in file '//trim(emnl%outname), frm = "(A/)")
 end if

 if ((emnl%Esel.eq.-1).and.(iE.eq.1)) then 
  call Message('Final data stored in file '//trim(emnl%outname), frm = "(A/)")
 end if

end do energyloop

if (emnl%Esel.ne.-1) then
  call Message('Final data stored in file '//trim(emnl%outname), frm = "(A/)")
end if

end subroutine ComputeMasterPattern
