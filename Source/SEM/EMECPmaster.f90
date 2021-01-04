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
! EMsoft:EMECPmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMECPmaster
!
!> @author Marc De Graef/ Saransh Singh, Carnegie Mellon University
!
!> @brief Zone axis electron channeling master patterns
!
!> @date 07/23/14  SS 1.0 rewrite master pattern
!> @date 05/06/15  SS 1.1 added OpenMP and made master pattern site specific
!> @date 09/10/15 MDG 1.2 updated Lambert and symmetry stuff
!> @date 05/21/16 MDG 1.3 changes for HDF internal file reorganization
!> @date 08/16/17 MDG 1.4 added option to reuse the Monte Carlo results from a different input file
!--------------------------------------------------------------------------

program EMECPmaster

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use EBSDmod
use files
use io
use stringconstants
use HDFsupport
use HDF5

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(ECPMasterNameListType)             :: ecpnl
integer(kind=irg)                       :: hdferr 

nmldeffile = 'EMECPmaster.nml'
progname = 'EMECPmaster.f90'
progdesc = 'Master pattern generation for Electron channeling pattern'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 39, 0 /), progname)

! deal with the namelist stuff
call GetECPMasterNameList(nmldeffile,ecpnl)

! if copyfromenergyfile is different from 'undefined', then we need to 
! copy all the Monte Carlo data from that file into a new file, which 
! will then be read from and written to by the ECmasterpattern routine.
if (ecpnl%copyfromenergyfile.ne.'undefined') then
  call h5open_EMsoft(hdferr)  
  call EBSDcopyMCdata(ecpnl%copyfromenergyfile, ecpnl%energyfile, ecpnl%h5copypath)
  call h5close_EMsoft(hdferr)  
end if

! perform the zone axis computations
call ECmasterpattern(ecpnl, progname, nmldeffile)

end program EMECPmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ECmasterpattern
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute a master electron channeling pattern
!
!> @date 07/23/14  SS  1.0 original
!> @date 06/05/15  SS  1.1 added OpenMP support and HDF5 support
!> @date 09/10/15 MDG  1.2 updated Lambert and symmetry stuff
!> @date 09/15/15  SS  1.3 corrected small bug in writing stereo projection to h5 file
!> @date 08/17/16  MDG 1.4 modified for new HDF internal format
!> @date 09/29/16  MDG 2.0 added option to read structure data from master file instead of external .xtal file
!> @date 01/11/18  MDG 2.1 changed master pattern format for hexagonal symmetry to the square Lambert projection
!--------------------------------------------------------------------------
subroutine ECmasterpattern(ecpnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use crystal
use symmetry
use Lambert
use initializersHDF
use initializers
use constants
use gvectors
use kvectors
use error
use io
use math
use files
use diffraction
use omp_lib
use MBModule
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use notifications
use stringconstants
use timing 

IMPLICIT NONE

type(ECPMasterNameListType),INTENT(INOUT)        :: ecpnl
character(fnlen),INTENT(IN)                      :: progname
character(fnlen),INTENT(IN)                      :: nmldeffile

real(kind=dbl)          :: frac
integer(kind=irg)       :: gzero, istat, tickstart

integer(kind=irg)       :: numangle, numzbins, nx, ny, npy, totnum_el, numsites ! reading from MC file
real(kind=dbl)          :: EkeV, Ehistmin, Ebinsize, depthmax, depthstep, sig, omega  ! reading from MC file
integer(kind=irg), allocatable :: acc_z(:,:,:,:),accum_z(:,:,:,:) ! reading from MC file

integer(kind=irg)       :: io_int_sgl(1), io_int(6) ! integer output variable
real(kind=dbl)          :: io_real(5) ! real output variable

integer(kind=irg)       :: i, j, isym, pgnum, SamplingType, nix, nixp, niy, niyp ! variables for point group and Laue group
integer(kind=irg),parameter     :: LaueTest(11) = (/ 149, 151, 153, 156, 158, 160, 161, 164, 165, 166, 167 /)  ! space groups with 2 or mirror at 30 degrees
integer(kind=irg)       :: npyhex, ijmax, numk, skip ! parameters for calckvectors and calcwavelength subroutine

integer(kind=irg)       :: ga(3), gb(3) ! shortest reciprocal lattice vector for zone axis
real(kind=sgl), allocatable :: thick(:), mLPNH(:,:,:), mLPSH(:,:,:), svals(:), lambdaZ(:), klist(:,:), knlist(:),&
                               masterSPNH(:,:,:), masterSPSH(:,:,:), auxNH(:,:,:), auxSH(:,:,:) 
real(kind=dbl)          :: intthick, dc(3), dx, dxm, dy, dym, edge, scl, xy(2), Radius
complex(kind=dbl),allocatable   :: Lgh(:,:),Sgh(:,:),Sghtmp(:,:,:)
complex(kind=dbl),allocatable   :: DynMat(:,:)
complex(kind=dbl)       :: czero

integer(kind=irg)       :: nt, nns, nnw, tots, totw ! thickness array and BetheParameters strong and weak beams
real(kind=sgl)          :: FN(3), kk(3), fnat, kn, tstop
integer(kind=irg)       :: numset, nref, ipx, ipy, ipz, iequiv(3,48), nequiv, ip, jp, izz, IE, iz, one,ierr
integer(kind=irg),allocatable   :: kij(:,:), nat(:)
real(kind=dbl)          :: res(2), xyz(3), ind, nabsl

character(fnlen)        :: oldprogname, energyfile, outname
character(fnlen)        :: xtalname, groupname, datagroupname, HDF_FileVersion, attributename
character(8)            :: MCscversion
character(4)            :: MCmode
character(6)            :: projtype
character(11)           :: dstr
character(15)           :: tstrb
character(15)           :: tstre
logical                 :: f_exists, readonly, overwrite=.TRUE., insert=.TRUE., g_exists, xtaldataread
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)
character(fnlen,kind=c_char)                     :: line2(1)


logical                             :: verbose, usehex, switchmirror

type(unitcell)                      :: cell
type(gnode),save                    :: rlp
type(DynType),save                  :: Dyn
type(kvectorlist), pointer          :: khead, ktmp ! linked list for incident wave vectors for master list
type(kvectorlist), pointer          :: kheadcone,ktmpcone ! linked list for incident wave vectors for individual pattern
real(kind=dbl),allocatable          :: ecpattern(:,:)
type(BetheParameterType)            :: BetheParameters
type(reflisttype),pointer           :: reflist, firstw,rltmp
integer(kind=irg)                   :: nthreads,TID,ix,hdferr,num_el,etotal, nlines,nsx,nsy,SelE
type(HDFobjectStackType)            :: HDF_head
character(fnlen)                    :: dataset, instring
character(fnlen)                    :: mode
integer(HSIZE_T)                    :: dims4(4), cnt4(4), offset4(4), dims3(3), cnt3(3), offset3(3)

character(fnlen),ALLOCATABLE        :: MessageLines(:)
integer(kind=irg)                  :: NumLines
character(fnlen)                   :: SlackUsername, exectime
character(100)                     :: c

!$OMP THREADPRIVATE(rlp) 

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(tickstart)

gzero = 1
frac = 0.05

!=============================================================
!read Monte Carlo output file and extract necessary parameters
! first, we need to load the data from the MC program.
!=============================================================

xtaldataread = .FALSE.

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

energyfile = trim(EMsoft_getEMdatapathname())//trim(ecpnl%energyfile)
energyfile = EMsoft_toNativePath(energyfile)
inquire(file=trim(energyfile), exist=f_exists)

if (.not.f_exists) then
    call FatalError('ComputeMasterPattern','Monte Carlo input file does not exist')
end if
call Message('opening '//trim(energyfile), frm = "(A)" )

! open the MC file using the default properties.
nullify(HDF_head%next)
readonly = .TRUE.
hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! next we need to make sure that this EM file actually contains a Monte Carlo 
! data set; if it does, then we can open the file and read all the information
datagroupname = '/EMheader/MCOpenCL'
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ECmasterpattern','This HDF file does not contain any Monte Carlo data')
end if


! open the namelist group
groupname = SC_NMLparameters
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLNameList
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

dataset = SC_mode
call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
mode = trim(stringarray(1))

if (trim(mode) .ne. 'bse1') then
    call FatalError('ECmasterpattern','The mode is not bse1...select the correct monte carlo file')
end if

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
  call Initialize_Cell(cell,Dyn,rlp,xtalname, ecpnl%dmin, sngl(EkeV), verbose, HDF_head)
  xtaldataread = .TRUE.
end if

! open the Data group
groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)

datagroupname = 'MCOpenCL'
hdferr = HDF_openGroup(datagroupname, HDF_head)

! read data items
dataset = SC_numangle
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numangle)

dataset = SC_numzbins
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numzbins)

dataset = SC_totnumel
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, num_el)

dataset = SC_accumz
! dims4 =  (/ numangle, numzbins, 2*(nsx/10)+1,2*(nsy/10)+1 /)
call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
allocate(accum_z(numangle,numzbins,-nsx/10:nsx/10,-nsy/10:nsy/10),stat=istat)
accum_z = acc_z
deallocate(acc_z)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

! close the fortran interface
call h5close_EMsoft(hdferr)

etotal = sum(accum_z(numangle,:,:,:))

call Message(' -> completed reading '//trim(ecpnl%energyfile), frm = "(A//)")

!=============================================
! completed reading monte carlo file
!=============================================

!=============================================
!=============================================
! crystallography section
if (xtaldataread.eqv..FALSE.) then
  !nullify(cell)        
  !allocate(cell)        

  verbose = .TRUE.
  call Initialize_Cell(cell,Dyn,rlp,xtalname, ecpnl%dmin, sngl(EkeV), verbose)
end if 

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
! and we change the output HDF5 file a little as well. We need to make sure that the EMECP program
! issues an error when an old format HDF5 file is read.  

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

!if(usehex)  npyhex = nint(2.0*float(ecpnl%npx)/sqrt(3.0))
npy = ecpnl%npx
ijmax = float(ecpnl%npx)**2   ! truncation value for beam directions

! ---------- end of symmetry and crystallography section
!=============================================
!=============================================


!=============================================
! generating list of incident wave vectors
!=============================================

! determine all independent incident beam directions (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation;
nullify(khead)
if (usehex) then
  call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,ecpnl%npx,npy,numk, &
  SamplingType,ijmax,'RoscaLambert',usehex)
else
  call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,ecpnl%npx,ecpnl%npx,numk, &
  SamplingType,ijmax,'RoscaLambert',usehex)
end if
io_int_sgl(1)=numk

call WriteValue('# independent beam directions to be considered = ', io_int_sgl, 1, "(I8)")

ktmp => khead
czero = cmplx(0.D0, 0.D0)
! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters)

!nt = nint((depthmax - ecpnl%startthick)/depthstep)
!allocate(thick(nt))
!thick = ecpnl%startthick + depthstep * (/ (i-1,i=1,nt) /)

nullify(reflist)
nullify(firstw)

nns = 0
nnw = 0
tots = 0
totw = 0

numset = cell % ATOM_ntype  ! number of special positions in the unit cell
izz = numzbins

allocate(lambdaZ(1:izz),stat=istat)
allocate(nat(numset),stat=istat)
allocate(kij(3,numk),stat=istat)
allocate(klist(3,numk),knlist(numk),stat=istat)

call CalcUcg(cell,rlp,(/0,0,0/))
nabsl = rlp%xgp

do iz=1,izz
    lambdaZ(iz) = float(sum(accum_z(numangle,iz,:,:)))/float(etotal)
    lambdaZ(iz) = lambdaZ(iz) * exp(2.0*sngl(cPi)*(iz-1)*depthstep/nabsl)
end do

kij(1:3,1) = (/ ktmp%i, ktmp%j, ktmp%hs /)
klist(1:3,1) = ktmp%k
knlist(1) = ktmp%kn

do i = 2,numk
    ktmp => ktmp%next
    kij(1:3,i) = (/ ktmp%i, ktmp%j, ktmp%hs /)
    klist(1:3,i) = ktmp%k
    knlist(i) = ktmp%kn
end do

ktmp => khead

nat = 0
fnat = 1.0/float(sum(cell%numat(1:numset)))
intthick = dble(depthmax)

datagroupname = 'ECPmaster'

! if the combinesites parameter is .TRUE., then we only need to 
! allocate a dimension of 1 in the master pattern array since we are adding 
! together the master patterns for all sites in the asymmetric unit.
if (ecpnl%combinesites.eqv..TRUE.) then
  numsites = 1
else
  numsites = numset
end if


allocate(mLPNH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numsites),stat=istat)
allocate(mLPSH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numsites),stat=istat)
allocate(masterSPNH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numsites))
allocate(masterSPSH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numsites))

mLPNH = 0.0
mLPSH = 0.0
masterSPNH = 0.0
masterSPSH = 0.0

nullify(HDF_head%next)
! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! Open output file 
hdferr =  HDF_openFile(energyfile, HDF_head)

! if this file already contains an ECPmaster dataset, then we let the user
! know and gracefully abort the program.
dataset = trim(SC_EMData)//'/'//trim(SC_ECPmaster)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  call Message('ECmasterpattern: this output file already contains an ECPmaster dataset') 
  call FatalError('ECmasterpattern','Set a new ECP master file in the namelist input file') 
end if

! write the EMheader to the file
groupname = SC_ECPmaster
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_ECPmasterNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteECPMasterNameList(HDF_head, ecpnl)
call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

! create the ECPmaster group and add a HDF_FileVersion attribbute to it 
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

dataset = SC_EkeV
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetDouble(dataset, EkeV, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetDouble(dataset, EkeV, HDF_head)
end if

! dataset = 'cell%ATOM_type'
! hdferr = HDF_writeDatasetIntegerArray1D(dataset, cell%ATOM_type(1:numset), numset, HDF_head)

! dataset = 'squhex'
! if (usehex) then
! stringarray(1)= 'hexago'
! hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
! else
! stringarray(1)= 'square'
! hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
! end if

! create the hyperslab and write zeroes to it for now
dataset = SC_mLPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3, HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3, HDF_head)
end if

dataset = SC_mLPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3, HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3, HDF_head)
end if

dataset = SC_masterSPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head)
end if

dataset = SC_masterSPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
offset3 = (/ 0, 0, 0/)
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

call OMP_SET_NUM_THREADS(ecpnl%nthreads)
io_int(1) = ecpnl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")


!!$OMP PARALLEL default(shared) COPYIN(rlp) &
!$OMP PARALLEL COPYIN(rlp) &
!$OMP& PRIVATE(DynMat,Sgh,Sghtmp,Lgh,i,FN,TID,kn,ipx,ipy,ix,ip,iequiv,nequiv,reflist,firstw) &
!$OMP& PRIVATE(kk,nns,nnw,nref,nat,io_int,io_int_sgl,nthreads,svals) 
!!!!$OMP& SHARED(mLPNH,mLPSH,tots,totw)

nthreads = OMP_GET_NUM_THREADS()
TID = OMP_GET_THREAD_NUM()

io_int_sgl(1) = nthreads

if (TID .eq. 0) call WriteValue('Setting number of threads to ', io_int_sgl, 1, "(I8)")

allocate(svals(numset))

!$OMP DO SCHEDULE(DYNAMIC)

beamloop: do i = 1, numk
    kk = klist(1:3,i)
    FN = kk

    nullify(reflist)

    call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, kk, ecpnl%dmin, nref)

! determine strong and weak reflections
    nullify(firstw)
    nns = 0
    nnw = 0
    call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, nref, nns, nnw)

    allocate(DynMat(nns,nns))

    call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)

! then we need to initialize the Sgh and Lgh arrays
    if (allocated(Lgh)) deallocate(Lgh)
    if (allocated(Sghtmp)) deallocate(Sghtmp)

    allocate(Sghtmp(nns,nns,numset),Lgh(nns,nns))

    Lgh = czero
    Sghtmp = czero
    nat = 0
    call CalcSgh(cell,reflist,nns,numset,Sghtmp,nat)

! solve the dynamical eigenvalue equation
    kn = knlist(i)

    call CalcLgh(DynMat,Lgh,intthick,dble(kn),nns,gzero,depthstep,lambdaZ,izz)
    deallocate(DynMat)

! dynamical contributions
     svals = 0.0
     do ix=1,numset
       svals(ix) = real(sum(Lgh(1:nns,1:nns)*Sghtmp(1:nns,1:nns,ix)))
     end do
     svals = svals/float(sum(nat(1:numset)))

! and store the resulting values in all the symmetryically equivalent positions
    ipx = kij(1,i)
    ipy = kij(2,i)
    ipz = kij(3,i)

    if (usehex) then 
      call Apply3DPGSymmetry(cell,ipx,ipy,ipz,ecpnl%npx,iequiv,nequiv,usehex)
    else
      if ((cell%SYM_SGnum.ge.195).and.(cell%SYM_SGnum.le.230)) then
        call Apply3DPGSymmetry(cell,ipx,ipy,ipz,ecpnl%npx,iequiv,nequiv,cubictype=SamplingType)
      else
        call Apply3DPGSymmetry(cell,ipx,ipy,ipz,ecpnl%npx,iequiv,nequiv)
      end if
    end if
    
  if (ecpnl%combinesites.eqv..FALSE.) then
     do ix=1,nequiv
      if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1:numset) = svals(1:numset)
      if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1:numset) = svals(1:numset)
    end do
  else
     do ix=1,nequiv
      if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1) = sum(svals)
      if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1) = sum(svals)
    end do
  end if

    totw = totw + nnw
    tots = tots + nns

    deallocate(Lgh, Sghtmp)

    if (mod(i,5000).eq.0) then
        io_int(1) = i
        call WriteValue('  completed beam direction ',io_int, 1, "(I8)")
    end if

    call Delete_gvectorlist(reflist)

end do beamloop

!$OMP END PARALLEL

io_int(1) = nint(float(tots)/float(numk))
call WriteValue(' -> Average number of strong reflections = ',io_int, 1, "(I5)")
io_int(1) = nint(float(totw)/float(numk))
call WriteValue(' -> Average number of weak reflections   = ',io_int, 1, "(I5)")


 if (usehex.eqv..TRUE.) then
! and finally, we convert the hexagonally sampled array to a square Lambert projection which will be used 
! for all ECP pattern interpolations;  we need to do this for both the Northern and Southern hemispheres

! we begin by allocating auxiliary arrays to hold copies of the hexagonal data; the original arrays will
! then be overwritten with the newly interpolated data.
    allocate(auxNH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,1:numset),stat=istat)
    allocate(auxSH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,1:numset),stat=istat)
    auxNH = mLPNH
    auxSH = mLPSH

! 
    edge = 1.D0 / dble(ecpnl%npx)
    scl = float(ecpnl%npx) 
    do i=-ecpnl%npx,ecpnl%npx
      do j=-ecpnl%npx,ecpnl%npx
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
          if (nixp.gt.ecpnl%npx) nixp = nix
          if (niyp.gt.ecpnl%npx) niyp = niy
          dx = xy(1) - nix
          dy = xy(2) - niy
          dxm = 1.D0 - dx
          dym = 1.D0 - dy
          mLPNH(i,j,1:numset) = auxNH(nix,niy,1:numset)*dxm*dym + auxNH(nixp,niy,1:numset)*dx*dym + &
                               auxNH(nix,niyp,1:numset)*dxm*dy + auxNH(nixp,niyp,1:numset)*dx*dy
          mLPSH(i,j,1:numset) = auxSH(nix,niy,1:numset)*dxm*dym + auxSH(nixp,niy,1:numset)*dx*dym + &
                               auxSH(nix,niyp,1:numset)*dxm*dy + auxSH(nixp,niyp,1:numset)*dx*dy
        end if
      end do
    end do
    deallocate(auxNH, auxSH)
  end if

! make sure that the outer pixel rim of the mLPSH patterns is identical to
! that of the mLPNH array.
mLPSH(-ecpnl%npx,-ecpnl%npx:ecpnl%npx,1:numsites) = mLPNH(-ecpnl%npx,-ecpnl%npx:ecpnl%npx,1:numsites)
mLPSH( ecpnl%npx,-ecpnl%npx:ecpnl%npx,1:numsites) = mLPNH( ecpnl%npx,-ecpnl%npx:ecpnl%npx,1:numsites)
mLPSH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx,1:numsites) = mLPNH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx,1:numsites)
mLPSH(-ecpnl%npx:ecpnl%npx, ecpnl%npx,1:numsites) = mLPNH(-ecpnl%npx:ecpnl%npx, ecpnl%npx,1:numsites)

! get stereographic projections
  Radius = 1.0
  do i=-ecpnl%npx,ecpnl%npx 
    do j=-ecpnl%npx,ecpnl%npx 
      xy = (/ float(i), float(j) /) / float(ecpnl%npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      xyz = xyz/vecnorm(xyz)
      if (ierr.ne.0) then 
        masterSPNH(i,j,1:numsites) = 0.0
        masterSPSH(i,j,1:numsites) = 0.0
      else
        masterSPNH(i,j,1:numsites) = InterpolateLambert(xyz, mLPNH, ecpnl%npx, numsites)
        masterSPSH(i,j,1:numsites) = InterpolateLambert(xyz, mLPSH, ecpnl%npx, numsites)
      end if
    end do
  end do

! and here is where the major changes are for this version 5.0: all output now in HDF5 format
call timestamp(datestring=dstr, timestring=tstre)

tstop = Time_tock(tickstart)

datagroupname = 'ECPmaster'

nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
hdferr =  HDF_openFile(energyfile, HDF_head)

! update the time string
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)
hdferr = HDF_openGroup(datagroupname, HDF_head)

dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

!dataset = SC_Duration
!tstop = tstop - tstart
!if (iE.eq.numangle) then 
!  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
!  if (g_exists) then     
!    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
!  else
!    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
!  end if
!else
!  hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
!end if

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)
hdferr = HDF_openGroup(datagroupname, HDF_head)

! add data to the hyperslab
dataset = SC_mLPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites/)
offset3 = (/ 0, 0, 0 /)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3, HDF_head, insert)

dataset = SC_mLPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites/)
offset3 = (/ 0, 0, 0 /)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3, HDF_head, insert)

dataset = SC_masterSPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
offset3 = (/ 0, 0, 0/)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)

dataset = SC_masterSPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numsites /)
offset3 = (/ 0, 0, 0/)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

call Message('Final data stored in file '//trim(energyfile), frm = "(A/)")

! if requested, we notify the user that this program has completed its run
if (trim(EMsoft_getNotify()).ne.'Off') then
  if (trim(ecpnl%Notify).eq.'On') then 
    NumLines = 3
    allocate(MessageLines(NumLines))

    call hostnm(c)
 
    MessageLines(1) = 'EMECPmaster program has ended successfully'
    MessageLines(2) = 'Master pattern data stored in '//trim(energyfile)
    write (exectime,"(I10)") tstop  
    MessageLines(3) = 'Total execution time [s]: '//trim(exectime)
    SlackUsername = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, SlackUsername)
  end if
end if

end subroutine ECmasterpattern
