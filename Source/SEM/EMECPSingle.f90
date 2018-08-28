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
use stringconstants

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
use stringconstants

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
type(ECPLargeAccumType),pointer         :: acc

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

call Message(' -> opening '//trim(ecpnl%energyfile), frm = "(A)" )
call  ECPSinglereadMCfile(ecpnl, acc, verbose=.TRUE.)

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
xtalname = ecpnl%MCxtalname
EkeV = ecpnl%EkeV

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

numzbins = ecpnl%numzbins
numEbins = ecpnl%Ebinsize
izz = numzbins

allocate(lambdaZ(1:izz),stat=istat)
allocate(nat(numset),stat=istat)

accum_z = acc%accum_z

etotal = sum(acc%accum_z)
ind = float(numEbins)/2.0+1.0

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
groupname = SC_ECPsingle
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
call HDFwriteECPSingleNameList(HDF_head, ecpnl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_xtalname
hdferr = HDF_writeDatasetStringArray(dataset, trim(xtalname), 1, HDF_head)

dataset = SC_numset
hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head)

dataset = SC_EkeV
hdferr = HDF_writeDatasetDouble(dataset, EkeV, HDF_head)

dataset = SC_cellATOMtype
hdferr = HDF_writeDatasetIntegerArray1D(dataset, cell%ATOM_type(1:numset), numset, HDF_head)

dataset = SC_squhex
if (usehex) then
hdferr = HDF_writeDatasetStringArray(dataset, 'hexago', 1, HDF_head)
else
hdferr = HDF_writeDatasetStringArray(dataset, 'square', 1, HDF_head)
end if

! create the hyperslab and write zeroes to it for now
dataset = SC_ECP
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
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_ECPsingle
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

dataset = SC_Duration
call CPU_TIME(tstop)
tstop = tstop - tstart
hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)

! add data to the hyperslab
dataset = SC_ECP
dims2 = (/  ecpnl%npix, ecpnl%npix /)
cnt2 = (/ ecpnl%npix, ecpnl%npix/)
offset2 = (/ 0, 0 /)
hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPNH, dims2, offset2, cnt2(1), cnt2(2), HDF_head, insert)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

call Message('Final data stored in file '//trim(ecpnl%datafile), frm = "(A/)")


end subroutine ECPsinglepattern
