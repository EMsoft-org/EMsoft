! ###################################################################
! Copyright (c) 2013-2017, Marc De Graef/Saransh Singh/Carnegie Mellon University
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
! EMsoft:EMECPSingle.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMECPSingle
!
!> @author Marc De Graef/ Saransh Singh, Carnegie Mellon University
!
!> @brief Single orientation electron channeling pattern for zone axis orientation
!
!> @date 07/23/14  SS 1.0 rewrite master pattern
!> @date 05/06/15  SS 1.1 added OpenMP and made master pattern site specific
!> @date 09/10/15 MDG 1.2 updated Lambert and symmetry stuff
!> @date 05/21/16 MDG 1.3 changes for new HDF internal file organization
!> @date 01/25/17 MDG 1.4 corrected template file numbering
!--------------------------------------------------------------------------

program EMECPSingle

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(ECPSingleNameListType)             :: ecpnl

nmldeffile = 'EMECPSingle.nml'
progname = 'EMECPSingle.f90'
progdesc = 'Individual high quality electron channeling pattern calculation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 71, 0 /), progname)

! deal with the namelist stuff
call GetECPSingleNameList(nmldeffile,ecpnl)

! perform the zone axis computations
call ECPsinglepattern(ecpnl, progname, nmldeffile)

end program EMECPSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE:ECPsinglepattern
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute a single electron channeling pattern
!
!> @date 07/23/14  SS  1.0 original
!> @date 06/05/15  SS  1.1 added OpenMP support and HDF5 support
!> @date 09/10/15 MDG  1.2 updated Lambert and symmetry stuff
!> @date 09/15/15  SS  1.3 corrected small bug in writing stereo projection to h5 file
!--------------------------------------------------------------------------
subroutine ECPsinglepattern(ecpnl, progname, nmldeffile)

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
use files
use diffraction
use omp_lib
use MBModule
use HDF5
use NameListHDFwriters
use HDFsupport
use ECPmod
use rotations, only:eu2om
use ISO_C_BINDING

IMPLICIT NONE

type(ECPSingleNameListType),INTENT(INOUT)        :: ecpnl
character(fnlen),INTENT(IN)                      :: progname
character(fnlen),INTENT(IN)                      :: nmldeffile

real(kind=dbl)          :: frac
integer(kind=irg)       :: gzero, istat

integer(kind=irg)       :: numEbins, numzbins, nx, ny, totnum_el ! reading from MC file
real(kind=dbl)          :: EkeV, Ehistmin, Ebinsize, depthmax, depthstep, sig, omega  ! reading from MC file
integer(kind=irg), allocatable :: acc_z(:,:,:,:),accum_z(:,:,:,:) ! reading from MC file

integer(kind=irg)       :: io_int_sgl(1), io_int(6) ! integer output variable
real(kind=dbl)          :: io_real(5) ! real output variable

integer(kind=irg)       :: i, j, isym, pgnum, SamplingType ! variables for point group and Laue group
integer(kind=irg),parameter     :: LaueTest(11) = (/ 149, 151, 153, 156, 158, 160, 161, 164, 165, 166, 167 /)  ! space groups with 2 or mirror at 30 degrees
integer(kind=irg)       :: npyhex, ijmax, numk, skip ! parameters for calckvectors and calcwavelength subroutine

integer(kind=irg)       :: ga(3), gb(3) ! shortest reciprocal lattice vector for zone axis
real(kind=sgl), allocatable :: thick(:), mLPNH(:,:), svals(:), lambdaZ(:), klist(:,:), knlist(:), masterSP(:,:,:)
real(kind=dbl)          :: intthick
complex(kind=dbl),allocatable   :: Lgh(:,:),Sgh(:,:),Sghtmp(:,:,:)
complex(kind=dbl),allocatable   :: DynMat(:,:)
complex(kind=dbl)       :: czero

integer(kind=irg)       :: nt, nns, nnw, tots, totw ! thickness array and BetheParameters strong and weak beams
real(kind=sgl)          :: FN(3), kk(3), kkk(3), fnat, kn, Radius, xy(2), tstart, tstop
integer(kind=irg)       :: numset, nref, ipx, ipy, ipz, iequiv(3,48), nequiv, ip, jp, izz, IE, iz, one,ierr
integer(kind=irg),allocatable   :: kij(:,:), nat(:)
real(kind=dbl)          :: res(2), xyz(3), ind, om(3,3), eu(3)

character(fnlen)        :: oldprogname, energyfile, outname
character(fnlen)        :: xtalname, groupname
character(8)            :: MCscversion
character(4)            :: MCmode
character(6)            :: projtype
character(11)           :: dstr
character(15)           :: tstrb
character(15)           :: tstre
logical                 :: f_exists, readonly, overwrite=.TRUE., insert=.TRUE.
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)
character(fnlen,kind=c_char)                     :: line2(1)
type(IncidentListECP),pointer           :: khead, ktmp


logical                             :: verbose, usehex, switchmirror

type(unitcell), pointer             :: cell
type(gnode),save                    :: rlp
type(DynType),save                  :: Dyn
type(kvectorlist), pointer          :: kheadcone,ktmpcone ! linked list for incident wave vectors for individual pattern
real(kind=dbl),allocatable          :: ecpattern(:,:)
type(BetheParameterType)            :: BetheParameters
type(reflisttype),pointer           :: reflist, firstw,rltmp
integer(kind=irg)                   :: nthreads,TID,ix,hdferr,num_el,etotal, nlines,nsx,nsy,SelE
type(HDFobjectStackType),pointer    :: HDF_head
character(fnlen)                    :: dataset, instring
character(fnlen)                    :: mode
integer(HSIZE_T)                    :: dims4(4), cnt4(4), offset4(4), dims2(2), cnt2(2), offset2(2)

interface
  function InterpolateLambert(dc, master, npx, nf) result(res)

  use local
  use Lambert
  use EBSDmod
  use constants
  
  IMPLICIT NONE
  
  real(kind=dbl),INTENT(INOUT)            :: dc(3)
  real(kind=sgl),INTENT(IN)               :: master(-npx:npx,-npx:npx, 1:nf)
  integer(kind=irg),INTENT(IN)            :: npx 
  integer(kind=irg),INTENT(IN)            :: nf
  real(kind=sgl)                          :: res(nf)
  end function InterpolateLambert

end interface




!$OMP THREADPRIVATE(rlp) 

nullify(HDF_head)

call timestamp(datestring=dstr, timestring=tstrb)
call CPU_TIME(tstart)

gzero = 1
frac = 0.05

allocate(cell)

!=============================================================
!read Monte Carlo output file and extract necessary parameters
! first, we need to load the data from the MC program.
!=============================================================

call Message('opening '//trim(ecpnl%energyfile), frm = "(A)" )

energyfile = trim(EMsoft_getEMdatapathname())//trim(ecpnl%energyfile)
! this is now an HDF5 file as of version 5.1
! [since this is no longer a sequential access file, we do not need to read everything, just
! the quantities that we need...]

!
! Initialize FORTRAN interface.
!
call h5open_EMsoft(hdferr)

! first of all, if the file exists, then delete it and rewrite it on each energyloop
energyfile = trim(EMsoft_getEMdatapathname())//trim(ecpnl%energyfile)
inquire(file=energyfile, exist=f_exists)

if (.not.f_exists) then
    call FatalError('ComputeMasterPattern','Monte Carlo input file does not exist')
end if

! open the MC file using the default properties.
readonly = .TRUE.
hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! open the namelist group
groupname = 'NMLparameters'
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = 'MCCLNameList'
hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = 'xtalname'
call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
xtalname = trim(stringarray(1))

dataset = 'numsx'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, nsx)
nsx = (nsx - 1)/2
nsy = nsx

dataset = 'EkeV'
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, EkeV)

dataset = 'Ehistmin'
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ehistmin)

dataset = 'Ebinsize'
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ebinsize)

dataset = 'depthmax'
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, depthmax)

dataset = 'depthstep'
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, depthstep)

dataset = 'mode'
call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
mode = trim(stringarray(1))

if (trim(mode) .ne. 'bse1') then
    call FatalError('ECmasterpattern','The mode is not bse1...select the correct monte carlo file')
end if

! close the name list group
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! open the Data group
groupname = 'EMData'
hdferr = HDF_openGroup(groupname, HDF_head)

! read data items
dataset = 'numangle'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numEbins)

dataset = 'numzbins'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numzbins)

dataset = 'totnum_el'
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, num_el)

dataset = 'accum_z'
! dims4 =  (/ numEbins, numzbins, 2*(nsx/10)+1,2*(nsy/10)+1 /)
call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
allocate(accum_z(numEbins,numzbins,-nsx/10:nsx/10,-nsy/10:nsy/10),stat=istat)
accum_z = acc_z
deallocate(acc_z)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

! close the fortran interface
call h5close_EMsoft(hdferr)

ind = float(numEbins)/2.0+1.0
etotal = sum(accum_z(floor(ind),:,:,:))

call Message(' -> completed reading '//trim(ecpnl%energyfile), frm = "(A//)")

!=============================================
! completed reading monte carlo file
!=============================================

!=============================================
!=============================================
! crystallography section
nullify(cell)
allocate(cell)

! load the crystal structure and compute the Fourier coefficient lookup table
verbose = .TRUE.
call Initialize_Cell(cell,Dyn,rlp, xtalname, ecpnl%dmin, sngl(EkeV),verbose)

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
if (SamplingType.eq.-1) then 
  SamplingType = getHexvsRho(cell,isym)
end if

! if the point group is trigonal or hexagonal, we need to switch usehex to .TRUE. so that
! the program will use the hexagonal sampling method
usehex = .FALSE.
if ((cell%xtal_system.eq.4).or.(cell%xtal_system.eq.5)) usehex = .TRUE.

if(usehex)  npyhex = nint(2.0*float(ecpnl%npix)/sqrt(3.0))
ijmax = float(ecpnl%npix)**2   ! truncation value for beam directions

! ---------- end of symmetry and crystallography section
!=============================================
!=============================================


!=============================================
! generating list of incident wave vectors
!=============================================


czero = cmplx(0.D0, 0.D0)
! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters)


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

do iz=1,izz
    lambdaZ(iz) = float(sum(accum_z(floor(ind),iz,:,:)))/float(etotal)
end do

numk = 0
call GetVectorsConeSingle(ecpnl, khead, numk)
io_int_sgl(1)=numk
call WriteValue('# independent beam directions to be considered = ', io_int_sgl, 1, "(I8)")

allocate(kij(3,numk),stat=istat)
allocate(klist(3,numk),knlist(numk),stat=istat)

ktmp => khead
! converting to array for OpenMP parallelization
do i = 1,numk
   klist(1:3,i) = ktmp%k(1:3)
   kij(1:2,i) = (/ktmp%i,ktmp%j/)
   ktmp => ktmp%next
end do

ktmp => khead

nat = 0
fnat = 1.0/float(sum(cell%numat(1:numset)))
intthick = dble(depthmax)

outname = trim(EMsoft_getEMdatapathname())//trim(ecpnl%datafile)
allocate(mLPNH(1:ecpnl%npix,1:ecpnl%npix),stat=istat)
mLPNH = 0.0


nullify(HDF_head)
! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
groupname = 'ECPsingle'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = "NMLfiles"
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = 'ECPmasterNML'
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = "NMLparameters"
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteECPSingleNameList(HDF_head, ecpnl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = 'EMData'
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = 'xtalname'
stringarray(1)= trim(xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

dataset = 'numset'
hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head)

dataset = 'EkeV'
hdferr = HDF_writeDatasetDouble(dataset, EkeV, HDF_head)

dataset = 'cell%ATOM_type'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, cell%ATOM_type(1:numset), numset, HDF_head)

dataset = 'squhex'
if (usehex) then
stringarray(1)= 'hexago'
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
else
stringarray(1)= 'square'
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
end if

! create the hyperslab and write zeroes to it for now
dataset = 'ECP'
dims2 = (/  ecpnl%npix, ecpnl%npix /)
cnt2 = (/ ecpnl%npix, ecpnl%npix /)
offset2 = (/ 0, 0/)
hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPNH, dims2, offset2, cnt2(1), cnt2(2), HDF_head)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

call OMP_SET_NUM_THREADS(ecpnl%nthreads)
io_int(1) = ecpnl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")


!!$OMP PARALLEL default(shared) COPYIN(rlp) &
!$OMP PARALLEL COPYIN(rlp) &
!$OMP& PRIVATE(DynMat,Sgh,Sghtmp,Lgh,i,FN,TID,kn,ipx,ipy,ix,ip,iequiv,nequiv,reflist,firstw) &
!$OMP& PRIVATE(kk,kkk,nns,nnw,nref,nat,io_int,io_int_sgl,nthreads,svals) 
!!!!$OMP& SHARED(mLPNH,mLPSH,tots,totw)

nthreads = OMP_GET_NUM_THREADS()
TID = OMP_GET_THREAD_NUM()

io_int_sgl(1) = nthreads

if (TID .eq. 0) call WriteValue('Setting number of threads to ', io_int_sgl, 1, "(I8)")

allocate(svals(numset))

eu = (/ecpnl%phi1, ecpnl%phi, ecpnl%phi2/)
if (ecpnl%eulerconvention .eq. 'hkl') eu(1) = eu(1) + 90.D0
om = eu2om(eu*cPi/180.0)

!$OMP DO SCHEDULE(DYNAMIC)

beamloop: do i = 1, numk

    kk = klist(1:3,i)
    kk = matmul(om,kk)
    FN = kk

    call TransSpace(cell,kk,kkk,'c','r')

    kk = kkk/cell%mLambda

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
    kn = CalcDot(cell, kk, FN, 'r')

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

    mLPNH(ipx,ipy) = sum(svals)
    deallocate(Lgh, Sghtmp)

    if (mod(i,5000).eq.0) then
        io_int(1) = i
        call WriteValue('  completed beam direction ',io_int, 1, "(I8)")
    end if

    call Delete_gvectorlist(reflist)

end do beamloop

!$OMP END PARALLEL


! and here is where the major changes are for this version 5.0: all output now in HDF5 format
call timestamp(timestring=tstre)

nullify(HDF_head)
! Initialize FORTRAN HDF interface.
call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
hdferr =  HDF_openFile(outname, HDF_head)

! update the time string
groupname = 'EMheader'
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = 'ECPsingle'
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = 'StopTime'
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

dataset = 'Duration'
call CPU_TIME(tstop)
tstop = tstop - tstart
hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

groupname = 'EMData'
hdferr = HDF_openGroup(groupname, HDF_head)

! add data to the hyperslab
dataset = 'ECP'
dims2 = (/  ecpnl%npix, ecpnl%npix /)
cnt2 = (/ ecpnl%npix, ecpnl%npix/)
offset2 = (/ 0, 0 /)
hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPNH, dims2, offset2, cnt2(1), cnt2(2), HDF_head, insert)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

call Message('Final data stored in file '//trim(ecpnl%datafile), frm = "(A/)")


end subroutine ECPsinglepattern


function InterpolateLambert(dc, master, npx, nf) result(res)

use local
use Lambert
use EBSDmod
use constants

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)            :: dc(3)
real(kind=sgl),INTENT(IN)               :: master(-npx:npx,-npx:npx, 1:nf)
integer(kind=irg),INTENT(IN)            :: npx 
integer(kind=irg),INTENT(IN)            :: nf
real(kind=sgl)                          :: res(nf)

integer(kind=irg)                       :: nix, niy, nixp, niyp, istat
real(kind=sgl)                          :: xy(2), dx, dy, dxm, dym, scl

scl = float(npx) 

if (dc(3).lt.0.0) dc = -dc

! convert direction cosines to lambert projections
xy = scl * LambertSphereToSquare( dc, istat )
res = 0.0

if (istat.eq.0) then 
! interpolate intensity from the neighboring points
  nix = floor(xy(1))
  niy = floor(xy(2))
  nixp = nix+1
  niyp = niy+1
  if (nixp.gt.npx) nixp = nix
  if (niyp.gt.npx) niyp = niy
  dx = xy(1) - nix
  dy = xy(2) - niy
  dxm = 1.0 - dx
  dym = 1.0 - dy
  
  res(1:nf) = master(nix,niy,1:nf)*dxm*dym + master(nixp,niy,1:nf)*dx*dym + &
        master(nix,niyp,1:nf)*dxm*dy + master(nixp,niyp,1:nf)*dx*dy
end if

end function InterpolateLambert



