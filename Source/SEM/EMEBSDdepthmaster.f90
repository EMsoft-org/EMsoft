! ###################################################################
! Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDdepthmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDdepthmaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSDmaster computes the energy-dependent master EBSD pattern for a given structure
!
!> @todo implement full symmetry use (trigonal/rhombohedral is currently not implemented) 
!>
!> @date  11/04/19  MDG 1.0 split off from standard EMEBSDmaster program
!> @date  11/13/19  MDG 1.1 added option to compute patterns without energy weighting
!-----------------------------------------------------------------------
program EMEBSDdepthmaster

use local
use NameListTypedefs
use NameListHandlers
use JSONsupport
use EBSDmod
use json_module
use files
use io
use stringconstants
use HDF5
use HDFsupport

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(EBSDMasterNameListType)            :: emnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMEBSDdepthmaster.nml'
progname = 'EMEBSDdepthmaster.f90'
progdesc = 'EBSD Energy-dependent Master Pattern Simulation for Defect Inclusion'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 21, 0 /), progname)

! deal with the namelist stuff, either .nml or .json format
call GetEBSDMasterNameList(nmldeffile,emnl)

! generate a standard set of master EBSD patterns
call ComputeDepthMasterPattern(emnl, progname, nmldeffile)

end program EMEBSDdepthmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeDepthMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an EBSD master pattern as a function of energy
!
!> @param emnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 08/01/13  MDG 3.0 complete rewrite, eliminated old Lambert projection
!> @date 09/25/13  MDG 3.1 replaced k-vector code by kvectors module
!> @date 06/19/14  MDG 4.0 no more globals, nml split, added OpenMP
!> @date 03/26/15  MDG 5.0 all output now in HDF5 format (this is the first converted program)
!> @date 03/28/15  MDG 5.1 converted Monte Carlo input to HDF5 format and tested
!> @date 04/14/15  MDG 5.2 modified HDF5 output using hyperslabs
!> @date 04/15/15  MDG 5.3 debugged file closing problem, mostly in HDFsupport.f90
!> @date 04/15/15  MDG 5.4 corrected offset reading of accum_z array
!> @date 04/27/15  MDG 5.5 reactivate the hexagonal code; needs to be debugged
!> @date 05/08/15  MDG 5.6 added automated conversion from hexagonal to square Lambert; debugged
!> @date 09/01/15  MDG 6.0 changed symmetry to full point group and two Lambert hemispheres for output
!> @date 09/09/15  MDG 6.1 started to add a "restart" mode, so that long runs can be interrupted and restarted
!> @date 10/09/15  MDG 6.2 added BetheParameters and Duration to output HDF5 file
!> @date 05/21/16  MDG 6.3 added stereographic projections to output file
!> @date 06/30/16  MDG 6.4 added uniform option for background intensitiy studies
!> @date 08/17/16  MDG 6.5 modified for new HDF internal format
!> @date 09/29/16  MDG 6.6 added option to read structure data from master file instead of external .xtal file
!> @date 03/06/17  MDG 6.7 removed normal absorption from depth integration
!> @date 04/02/18  MDG 7.0 replaced MC file reading by new routine in EBSDmod
!> @date 04/03/18  MDG 7.1 replaced all regular MC variables by mcnl structure components
!> @date 01/07/19  MDG 7.2 added Legendre lattitudinal grid mode (used for spherical indexing)
!> @date 05/26/19  MDG 7.3 added warning about trigonal structures requiring hexagonal setting 
!> @date 07/01/19  MDG 7.4 corrected initial beam energy in restart mode 
!> @date 09/25/19  MDG 8.0 adds code to generate smaller SHTmaster file for Legendre grid
!> @date 11/04/19  MDG 9.0 modified code to keep depth slices
!> @date 11/13/19  MDG 9.1 allow for computations without depth weighting (for EMEBSDdefect computations)
!--------------------------------------------------------------------------
subroutine ComputeDepthMasterPattern(emnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use EBSDmod
use EBSDdefectmod
use MBmodule
use symmetry
use crystal
use constants
use error
use gvectors
use kvectors
use io
use math
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
use notifications
use stringconstants
 
IMPLICIT NONE

type(EBSDMasterNameListType),INTENT(INOUT) :: emnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

real(kind=dbl)          :: ctmp(192,3), arg, Radius, xyz(3)
integer(HSIZE_T)        :: dims4(4), cnt4(4), offset4(4)
integer(HSIZE_T)        :: dims3(3), cnt3(3), offset3(3)
integer(kind=irg)       :: isym,i,j,ik,npy,ipx,ipy,ipz,debug,iE,izz, izzmax, iequiv(3,48), nequiv, num_el, MCnthreads, & ! counters
                           numk, timestart, timestop, numsites, nthreads, & ! number of independent incident beam directions
                           ir,nat(maxpasym),kk(3), skip, ijmax, one, NUMTHREADS, TID, SamplingType, &
                           numset,n,ix,iy,iz, io_int(6), nns, nnw, nref, Estart, &
                           istat,gzero,ic,ip,ikk, totstrong, totweak, jh, ierr, nix, niy, nixp, niyp     ! counters
real(kind=dbl)          :: tpi,Znsq, kkl, DBWF, kin, delta, h, lambda, omtl, srt, dc(3), xy(2), edge, scl, tmp, dx, dxm, dy, dym !
real(kind=sgl)          :: io_real(5), selE, kn, FN(3), kkk(3), tstop, bp(4), nabsl, etotal, dens, avA, avZ
real(kind=sgl),allocatable      :: EkeVs(:), svals(:), auxNH(:,:,:), auxSH(:,:,:), Z2percent(:)  ! results
real(kind=sgl),allocatable      :: mLPNH(:,:,:), mLPSH(:,:,:), mLPNHsum(:,:,:), mLPSHsum(:,:,:), masterSPNH(:,:,:),  &
                                   masterSPSH(:,:,:)
real(kind=dbl),allocatable      :: LegendreArray(:), upd(:), diagonal(:)
complex(kind=dbl)               :: czero
complex(kind=dbl),allocatable   :: Lgh(:,:,:), Sgh(:,:)
logical                 :: usehex, switchmirror, verbose
character(fnlen)        :: xtalname

! Monte Carlo derived quantities
integer(kind=irg)       :: numEbins, nsx, nsy, hdferr, nlines, lastEnergy    ! variables used in MC energy file
integer(kind=irg),allocatable :: thick(:)
real(kind=sgl),allocatable :: lambdaE(:,:)
character(fnlen)        :: oldprogname, groupname, energyfile, outname, datagroupname, attributename, HDF_FileVersion
character(8)            :: MCscversion
character(11)           :: dstr
character(15)           :: tstrb
character(15)           :: tstre
logical                 :: f_exists, readonly, overwrite=.TRUE., insert=.TRUE., stereog, g_exists, xtaldataread, FL, doLegendre
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
type(EBSDMCdataType)            :: EBSDMCdata
type(MCCLNameListType)          :: mcnl

type(HDFobjectStackType)          :: HDF_head

character(fnlen),ALLOCATABLE      :: MessageLines(:)
integer(kind=irg)                 :: NumLines, info
character(fnlen)                  :: SlackUsername, exectime
character(100)                    :: c

!===============================================================
! This program differs from the regular EBSD master program in that
! the thickness integration is not carried out until the detector is
! implemented.  The energy integration is carried out in this program,
! which means that there is no option to properly compute the background
! intensity profile.  That is not really a problem, since we would 
! do background subtractions anyway for real experimental patterns.
!===============================================================

!$OMP THREADPRIVATE(rlp) 

! if copyfromenergyfile is different from 'undefined', then we need to 
! copy all the Monte Carlo data from that file into a new file, which 
! will then be read from and written to by the ComputeMasterPattern routine.
if (emnl%copyfromenergyfile.ne.'undefined') then
  call h5open_EMsoft(hdferr)
  call EBSDcopyMCdata(emnl%copyfromenergyfile, emnl%energyfile, emnl%h5copypath)
  call h5close_EMsoft(hdferr)
end if

stereog = .TRUE.

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(timestart)

tpi = 2.D0*cPi
czero = cmplx(0.D0,0.D0)

! is the master pattern used for spherical indexing only ?  If so, then we need to modifiy the k-vector sampling
doLegendre = .FALSE.

!=============================================
!=============================================
! ---------- read Monte Carlo .h5 output file and extract necessary parameters
call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(emnl%energyfile, mcnl, hdferr, EBSDMCdata, getAccumz=.TRUE.)
call h5close_EMsoft(hdferr)

nsx = (mcnl%numsx - 1)/2
nsy = nsx
etotal = float(EBSDMCdata%totnum_el)

io_int(1) = EBSDMCdata%totnum_el
call WriteValue(' --> total number of BSE electrons in MC data set ', io_int, 1)
!=============================================
!=============================================

allocate(EkeVs(EBSDMCdata%numEbins),thick(EBSDMCdata%numEbins))

do i=1,EBSDMCdata%numEbins
  EkeVs(i) = mcnl%Ehistmin + float(i-1)*mcnl%Ebinsize
end do
Estart = EBSDMCdata%numEbins

!=============================================
! we don't use the restart option from the original EMEBSDmaster program here.
!=============================================
  outname = trim(EMsoft_getEMdatapathname())//trim(emnl%outname)
  outname = EMsoft_toNativePath(outname)
  energyfile = trim(EMsoft_getEMdatapathname())//trim(emnl%energyfile)
  energyfile = EMsoft_toNativePath(energyfile)

!=============================================
! crystallography section; 
!=============================================
 verbose = .TRUE.
 call Initialize_Cell(cell,Dyn,rlp,mcnl%xtalname, emnl%dmin, sngl(mcnl%EkeV), verbose)

! check the crystal system and setting; abort the program for trigonal with rhombohedral setting with
! an explanation for the user

if ((cell%xtal_system.eq.5).and.(cell%b.eq.cell%c)) then 
    call Message('')
    call Message(' ========Program Aborted========')
    call Message(' The EBSD master pattern simulation for rhombohedral/trigonal structures')
    call Message(' requires that the structure be described using the hexagonal reference')
    call Message(' frame.  Please re-enter the crystal structure in this setting and re-run')
    call Message(' the Monte Carlo calculation and this master pattern program.')
    call Message('')
    stop
end if

! then calculate density, average atomic number and average atomic weight
 call CalcDensity(cell, dens, avZ, avA, Z2percent)

! allocate and compute the Sgh look-up table
 numset = cell%ATOM_ntype  
 call Initialize_SghLUT(cell,emnl%dmin, numset, nat, verbose)

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
! this is where we determine the value for the thickness integration limit 
! for the CalcLgh3 routine... This is important so that we don't need to 
! too many separate thickness master patterns.

! for each energy determine the 95% histogram thickness
! we need to check this computation... 
izzmax = 0
do iE = 1,EBSDMCdata%numEbins
 do ix=-nsx/10,nsx/10
  do iy=-nsy/10,nsy/10
   istat = sum(EBSDMCdata%accum_z(iE,:,ix,iy))
   izz = 1
   do while (sum(EBSDMCdata%accum_z(iE,1:izz,ix,iy)).lt.(0.99*istat)) 
    izz = izz+1
   end do
   if (izz.gt.izzmax) izzmax = izz
  end do
 end do
 thick(iE) = dble(izzmax) * mcnl%depthstep
end do

izz = nint(maxval(thick)/mcnl%depthstep)
allocate(lambdaE(1:EBSDMCdata%numEbins,1:izz),stat=istat)
do iE=1,EBSDMCdata%numEbins
 cell%voltage = Ekevs(iE)
 call CalcUcg(cell,rlp,(/0,0,0/))
 nabsl = rlp%xgp
 do iz=1,izz
  lambdaE(iE,iz) = float(sum(EBSDMCdata%accum_z(iE,iz,-nsx/10:nsx/10,-nsy/10:nsy/10)))/etotal
  lambdaE(iE,iz) = lambdaE(iE,iz) * exp(2.0*sngl(cPi)*(iz-1)*mcnl%depthstep/nabsl)
 end do
end do

io_int(1) = izz
call WriteValue('Total number of depth slices in output file: ', io_int, 1)

! and get rid of the EBSDMCdata%accum_z array
deallocate(EBSDMCdata%accum_z)
! ---------- end of 'read Monte Carlo output file and extract necessary parameters' section
!=============================================
!=============================================

!=============================================
!=============================================
! ---------- a couple of initializations
   npy = emnl%npx
   allocate(svals(izzmax),stat=istat)
   gzero = 1  ! index of incident beam
   debug = 0  ! no longer used
! ----------
!=============================================
!=============================================

!=============================================
!=============================================
! We will always combine all the sites in the asymmetric unit; this will 
! reduce the size of the arrays a bit.
  emnl%combinesites = .TRUE.

! ---------- allocate memory for the master patterns
  allocate(mLPNH(-emnl%npx:emnl%npx,-npy:npy,1:izzmax),stat=istat)
  allocate(mLPSH(-emnl%npx:emnl%npx,-npy:npy,1:izzmax),stat=istat)
  allocate(mLPNHsum(-emnl%npx:emnl%npx,-npy:npy,1:izzmax),stat=istat)
  allocate(mLPSHsum(-emnl%npx:emnl%npx,-npy:npy,1:izzmax),stat=istat)
  allocate(masterSPNH(-emnl%npx:emnl%npx,-npy:npy,1:izzmax))
  allocate(masterSPSH(-emnl%npx:emnl%npx,-npy:npy,1:izzmax))

! set various arrays to zero
   mLPNH = 0.0
   mLPSH = 0.0
   mLPNHsum = 0.0
   mLPSHsum = 0.0
   masterSPNH = 0.0
   masterSPSH = 0.0

! ---------- end allocate memory for the master patterns
!=============================================
!=============================================

! force dynamical matrix routine to read new Bethe parameters from file
! this will all be changed with the new version of the Bethe potentials
  call Set_Bethe_Parameters(BetheParameters,.TRUE.,emnl%BetheParametersFile)

!=============================================
! create the HDF5 output file
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

! write the EMheader to the file; this group name will distinguish
! this master pattern file from the standard EMEBSDmaster format;
! these two files are not interchangeable, so we will need a new 
! EMEBSDdefect program as well to generate images.
  datagroupname = 'EBSDdefectmaster'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EBSDmasterNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteEBSDMasterNameList(HDF_head, emnl)
  call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

! create the EBSDdefectmaster group and add a HDF_FileVersion attribute to it 
  hdferr = HDF_createGroup(datagroupname, HDF_head)
  HDF_FileVersion = '4.0'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0
! =====================================================
dataset = SC_xtalname
  allocate(stringarray(1))
  stringarray(1)= trim(mcnl%xtalname)
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
  
dataset = 'Z2percent'  ! SC_Z2percent
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray1D(dataset, Z2percent, cell%ATOM_ntype, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray1D(dataset, Z2percent, cell%ATOM_ntype, HDF_head)
  end if

dataset = SC_numEbins
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetInteger(dataset, EBSDMCdata%numEbins, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetInteger(dataset, EBSDMCdata%numEbins, HDF_head)
    end if

dataset = SC_EkeVs
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, EBSDMCdata%numEbins, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, EBSDMCdata%numEbins, HDF_head)
    end if

! we will keep overwriting the array as we sum up all the data in energy, so no need to use hyperslabs here ...
dataset = SC_mLPNH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray3D(dataset, mLPNHsum, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray3D(dataset, mLPNHsum, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head)
  end if

dataset = SC_mLPSH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray3D(dataset, mLPSHsum, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray3D(dataset, mLPSHsum, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head)
  end if

dataset = SC_masterSPNH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSPNH, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSPNH, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head)
  end if

dataset = SC_masterSPSH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSPSH, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSPSH, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head)
  end if

! =====================================================
! end of HDF_FileVersion = 4.0 write statements
! =====================================================

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)


!=============================================
!=============================================
! figure out what the start energy value is for the energyloop
lastEnergy=-1

!=============================================
!=============================================
! ---------- from here on, we need to repeat the entire computation for each energy value
! so this is where we could in principle implement an OpenMP approach; alternatively, 
! we could do the inner loop over the incident beam directions in OpenMP (probably simpler)

call Time_tick(timestart)

energyloop: do iE=Estart,1,-1
! print a message to indicate where we are in the computation
   io_int(1)=iE
   io_int(2)=Estart
   call Message(' Starting computation for energy bin (in reverse order)', frm = "(/A)",advance="no")
   call WriteValue(' ',io_int,2,"(I4,' of ',I4)",advance="no")
   io_real(1) = EkeVs(iE)
   call WriteValue('; energy [keV] = ',io_real,1,"(F6.2/)")
   selE = EkeVs(iE)

! set the accelerating voltage
   skip = 3
   cell%voltage = dble(EkeVs(iE))
   if(iE .ne. Estart) then
   	verbose = .TRUE.
   	call Initialize_Cell(cell,Dyn,rlp,mcnl%xtalname, emnl%dmin, EkeVs(iE), verbose, initLUT=.TRUE.)
   end if

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

! here's where we introduce the OpenMP calls, to speed up the overall calculations...

! set the number of OpenMP threads 
  if (emnl%nthreads.eq.0) then 
    nthreads = OMP_GET_MAX_THREADS()
  else
    nthreads = emnl%nthreads
  end if
  call OMP_SET_NUM_THREADS(nthreads)
  io_int(1) = nthreads
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
     if (emnl%useEnergyWeighting.eqv..TRUE.) then
       call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)
     else
       call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw, noNormAbs=.TRUE.)
     end if 
     totstrong = totstrong + nns
     totweak = totweak + nnw

! then we need to initialize the Sgh and Lgh arrays
     if (allocated(Sgh)) deallocate(Sgh)
     if (allocated(Lgh)) deallocate(Lgh)
     allocate(Sgh(nns,nns),Lgh(nns,nns,izz))
     Sgh = czero
     Lgh = czero
     call getSghfromLUTsum(cell,reflist,nns,numset,nat,Sgh)
!write(*,*) TID, maxval(abs(Sgh)), minval(abs(Sgh)), nat(1:numset),shape(Sgh),1.0/float(sum(nat(1:numset)))

! solve the dynamical eigenvalue equation for this beam direction  
! this part is different from the regular master pattern program, since we 
! do not carry out the depth integration; instead, the Lgh array is 3D
! with depth as the third dimension. We do carry out the energy integration
! in this routine
     kn = karray(4,ik)
     call CalcLghdepth(DynMat,Lgh,dble(thick(iE)),dble(kn),nns,gzero,mcnl%depthstep,lambdaE(iE,1:izzmax),izzmax)
     deallocate(DynMat)

! sum over the element-wise (Hadamard) product of the Lgh and Sgh arrays 
     svals = 0.0
     do iz=1,izzmax
       svals(iz) = real(sum(Lgh(1:nns,1:nns,iz)*Sgh(1:nns,1:nns)))
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
     do ix=1,nequiv
       if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1:izzmax) = svals(1:izzmax)
       if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1:izzmax) = svals(1:izzmax)
     end do
!$OMP END CRITICAL
  
! if (TID.eq.0) write (*,*) maxval(abs(mLPNH))

     if (mod(ik,5000).eq.0) then
       io_int(1) = ik
       io_int(2) = numk
       call WriteValue('  completed beam direction ',io_int, 2, "(I8,' of ',I8)")
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
    allocate(auxNH(-emnl%npx:emnl%npx,-npy:npy,1:izzmax),stat=istat)
    allocate(auxSH(-emnl%npx:emnl%npx,-npy:npy,1:izzmax),stat=istat)
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
          mLPNH(i,j,1:izzmax) = auxNH(nix,niy,1:izzmax)*dxm*dym + auxNH(nixp,niy,1:izzmax)*dx*dym + &
                               auxNH(nix,niyp,1:izzmax)*dxm*dy + auxNH(nixp,niyp,1:izzmax)*dx*dy
          mLPSH(i,j,1:izzmax) = auxSH(nix,niy,1:izzmax)*dxm*dym + auxSH(nixp,niy,1:izzmax)*dx*dym + &
                               auxSH(nix,niyp,1:izzmax)*dxm*dy + auxSH(nixp,niyp,1:izzmax)*dx*dy
        end if
      end do
    end do
    deallocate(auxNH, auxSH)
  end if

! make sure that the outer pixel rim of the mLPSH patterns is identical to
! that of the mLPNH array.
  mLPSH(-emnl%npx,-emnl%npx:emnl%npx,1:izzmax) = mLPNH(-emnl%npx,-emnl%npx:emnl%npx,1:izzmax)
  mLPSH( emnl%npx,-emnl%npx:emnl%npx,1:izzmax) = mLPNH( emnl%npx,-emnl%npx:emnl%npx,1:izzmax)
  mLPSH(-emnl%npx:emnl%npx,-emnl%npx,1:izzmax) = mLPNH(-emnl%npx:emnl%npx,-emnl%npx,1:izzmax)
  mLPSH(-emnl%npx:emnl%npx, emnl%npx,1:izzmax) = mLPNH(-emnl%npx:emnl%npx, emnl%npx,1:izzmax)

! and add this energy level to the overal master pattern arrays
  mLPNHsum = mLPNHsum + mLPNH
  mLPSHsum = mLPSHsum + mLPSH
  mLPNH = 0.0
  mLPSH = 0.0

! since these computations can take a long time, here we store 
! all the output at the end of each pass through the energyloop.

  io_int(1) = nint(float(totstrong)/float(numk))
  call WriteValue(' -> Average number of strong reflections = ',io_int, 1, "(I5)")
  io_int(1) = nint(float(totweak)/float(numk))
  call WriteValue(' -> Average number of weak reflections   = ',io_int, 1, "(I5)")

! and here is where the major changes are for version 5.0: all output now in HDF5 format
  call timestamp(datestring=dstr, timestring=tstre)

  datagroupname = 'EBSDdefectmaster'

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

  tstop = Time_tock(timestart)
  io_int(1) = tstop
  call WriteValue('Execution time [s]: ',io_int,1)

dataset = SC_Duration
  if (iE.eq.EBSDMCdata%numEbins) then 
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then     
      hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
    end if
  else
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
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
  hdferr = HDF_writeDatasetFloatArray3D(dataset, mLPNHsum, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)

dataset = SC_mLPSH
  hdferr = HDF_writeDatasetFloatArray3D(dataset, mLPSHsum, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)

 if ((emnl%Esel.eq.-1).and.(iE.ne.1)) then 
  call Message('Intermediate data stored in file '//trim(emnl%outname), frm = "(A/)")
 end if

 if ((emnl%Esel.eq.-1).and.(iE.eq.1)) then 
  call Message('Final data stored in file '//trim(emnl%outname), frm = "(A/)")
 end if

end do energyloop


! get stereographic projections 
Radius = 1.0
do i=-emnl%npx,emnl%npx 
  do j=-emnl%npx,emnl%npx 
    xy = (/ float(i), float(j) /) / float(emnl%npx)
    xyz = StereoGraphicInverse( xy, ierr, Radius )
    xyz = xyz/vecnorm(xyz)
    if (ierr.ne.0) then 
      masterSPNH(i,j,1:izzmax) = 0.0
      masterSPSH(i,j,1:izzmax) = 0.0
    else
      masterSPNH(i,j,1:izzmax) = InterpolateLambert(xyz, mLPNHsum, emnl%npx, izzmax)
      masterSPSH(i,j,1:izzmax) = InterpolateLambert(xyz, mLPSHsum, emnl%npx, izzmax)
    end if
  end do
end do


datagroupname = 'EBSDdefectmaster'

nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
hdferr =  HDF_openFile(outname, HDF_head)

groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_masterSPNH
  hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSPNH, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)

dataset = SC_masterSPSH
  hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSPSH, 2*emnl%npx+1, 2*emnl%npx+1, izzmax, HDF_head, overwrite)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)


timestop = Time_tock(timestart)
io_int(1) = timestop
call WriteValue('Total execution time [s] ',io_int,1)

if (emnl%Esel.ne.-1) then
  call Message('Final data stored in file '//trim(emnl%outname), frm = "(A/)")
end if

! if requested, we notify the user that this program has completed its run
if (trim(EMsoft_getNotify()).ne.'Off') then
  if (trim(emnl%Notify).eq.'On') then 
    NumLines = 3
    allocate(MessageLines(NumLines))

    call hostnm(c)
 
    MessageLines(1) = 'EMEBSDmaster program has ended successfully'
    MessageLines(2) = 'Master pattern data stored in '//trim(outname)
    write (exectime,"(F10.4)") tstop  
    MessageLines(3) = 'Total execution time [s]: '//trim(exectime)
    SlackUsername = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, SlackUsername)
  end if
end if

end subroutine ComputeDepthMasterPattern
