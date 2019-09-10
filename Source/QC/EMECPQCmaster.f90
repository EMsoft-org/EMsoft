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
! EMsoft:EMECPQCmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMECPQCmaster
!
!> @author Marc De Graef/ Saransh Singh, Carnegie Mellon University
!
!> @brief ECP master pattern for quasi-crystals ... test program
!
!> @date 03/15/17 MDG 1.0 original based on EMECPmaster program
!--------------------------------------------------------------------------

program EMECPQCmaster

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(ECPQCMasterNameListType)           :: ecpnl

nmldeffile = 'EMECPQCmaster.nml'
progname = 'EMECPQCmaster.f90'
progdesc = 'Master pattern generation for quasi-crystal electron channeling patterns'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 38, 0 /), progname)

! deal with the namelist stuff
call GetECPQCMasterNameList(nmldeffile,ecpnl)

! perform the zone axis computations
call ECQCmasterpattern(ecpnl, progname, nmldeffile)

end program EMECPQCmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ECQCmasterpattern
!
!> @author Marc De Graef/Saransh Singh, Carnegie Mellon University
!
!> @brief compute a quasi-crystal master electron channeling pattern
!
!> @date 03/15/17 MDG  1.0 original based on EMECPmaster program
!--------------------------------------------------------------------------
subroutine ECQCmasterpattern(ecpnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use QCmod
use Lambert
use initializersQCHDF
use constants
use gvectorsQC
use kvectorsQC
use MBmoduleQC
use MBmodule, only:CalcLgh
use kvectors, only:Delete_kvectorlist
use gvectors, only:Set_Bethe_Parameters
use error
use io
use files
use diffraction
use omp_lib
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use timing 

IMPLICIT NONE

type(ECPQCMasterNameListType),INTENT(INOUT)      :: ecpnl
character(fnlen),INTENT(IN)                      :: progname
character(fnlen),INTENT(IN)                      :: nmldeffile

real(kind=dbl)          :: frac
integer(kind=irg)       :: gzero, istat

integer(kind=irg)       :: numEbins, numzbins, nx, ny, totnum_el ! reading from MC file
real(kind=dbl)          :: EkeV, Ehistmin, Ebinsize, depthmax, depthstep, sig, omega  ! reading from MC file
integer(kind=irg), allocatable :: acc_z(:,:,:,:),accum_z(:,:,:,:) ! reading from MC file

integer(kind=irg)       :: io_int_sgl(1), io_int(6) ! integer output variable
real(kind=dbl)          :: io_real(5) ! real output variable

integer(kind=irg)       :: i, j, inumset, isym, pgnum, SamplingType ! variables for point group and Laue group
integer(kind=irg),parameter     :: LaueTest(11) = (/ 149, 151, 153, 156, 158, 160, 161, 164, 165, 166, 167 /)  ! space groups with 2 or mirror at 30 degrees
integer(kind=irg)       :: npyhex, ijmax, numk, skip ! parameters for calckvectors and calcwavelength subroutine

integer(kind=irg)       :: ga(3), gb(3) ! shortest reciprocal lattice vector for zone axis
real(kind=sgl), allocatable :: thick(:), mLPNH(:,:,:), mLPSH(:,:,:), svals(:), lambdaZ(:), klist(:,:), knlist(:),&
                               masterSPNH(:,:,:), masterSPSH(:,:,:), Iarray(:,:,:)
logical,allocatable     :: maskarray(:,:)
real(kind=dbl)          :: intthick
complex(kind=dbl),allocatable   :: Lgh(:,:),Sgh(:,:),Sghtmp(:,:,:)
complex(kind=dbl),allocatable   :: DynMat(:,:)
complex(kind=dbl)       :: czero

integer(kind=irg)       :: nt, nns, nnw, tots, totw, QCindex, ggg(6) ! thickness array and BetheParameters strong and weak beams
real(kind=sgl)          :: FN(3), kk(3), fnat, kn, Radius, xy(2), tstart, tstop, ma, mi, dmin
integer(kind=irg)       :: numset, nref, ipx, ipy, ipz, iequiv(3,48), nequiv, ip, jp, izz, IE, iz, one,ierr, npy, tickstart
integer(kind=irg),allocatable   :: kij(:,:), nat(:)
real(kind=dbl)          :: res(2), xyz(3), ind

character(fnlen)        :: oldprogname, energyfile, outname
character(fnlen)        :: xtalname, groupname, datagroupname
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

type(QCStructureType), pointer      :: QCcell
type(kvectorlist), pointer          :: khead, ktmp ! linked list for incident wave vectors for master list
type(kvectorlist), pointer          :: kheadcone,ktmpcone ! linked list for incident wave vectors for individual pattern
real(kind=dbl),allocatable          :: ecpattern(:,:)
type(BetheParameterType)            :: BetheParameters
type(QCreflisttype),pointer         :: reflist, firstw,rltmp
integer(kind=irg)                   :: nthreads,TID,ix,hdferr,num_el,etotal, nlines,nsx,nsy,SelE
type(HDFobjectStackType)            :: HDF_head
character(fnlen)                    :: dataset, instring
character(fnlen)                    :: mode
integer(HSIZE_T)                    :: dims4(4), cnt4(4), offset4(4), dims3(3), cnt3(3), offset3(3)

real(kind=sgl)                  :: kpg(3),tkpg(3),xnom,xden,q1,q2,sg,gvec(3)


! interface
!   function InterpolateLambert(dc, master, npx, nf) result(res)

!   use local
!   use Lambert
!   use EBSDmod
!   use constants
  
!   IMPLICIT NONE
  
!   real(kind=dbl),INTENT(INOUT)            :: dc(3)
!   real(kind=sgl),INTENT(IN)               :: master(-npx:npx,-npx:npx, 1:nf)
!   integer(kind=irg),INTENT(IN)            :: npx 
!   integer(kind=irg),INTENT(IN)            :: nf
!   real(kind=sgl)                          :: res(nf)
!   end function InterpolateLambert

! end interface

nullify(HDF_head%next)

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
    call FatalError('ECQCmasterpattern','Monte Carlo input file does not exist')
end if
call Message('opening '//trim(energyfile), frm = "(A)" )

! open the MC file using the default properties.
readonly = .TRUE.
hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! next we need to make sure that this EM file actually contains a Monte Carlo 
! data set; if it does, then we can open the file and read all the information
datagroupname = '/EMData/MCOpenCL'
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('ECPQCmasterpattern','This HDF file does not contain any Monte Carlo data')
end if

! open the namelist group
groupname = 'NMLparameters'
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = 'MCCLNameList'
hdferr = HDF_openGroup(groupname, HDF_head)

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
    call FatalError('ECQCmasterpattern','The mode is not bse1...select the correct monte carlo file')
end if

! close the name list group
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! open the Data group
groupname = 'EMData'
hdferr = HDF_openGroup(groupname, HDF_head)

datagroupname = 'MCOpenCL'
hdferr = HDF_openGroup(datagroupname, HDF_head)

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
! crystallography section: this is completely different from the original since 
! we are daling with a quasi-crystal...  so we need to initialize the QCcell structure
nullify(QCcell)
allocate(QCcell)

QCcell%voltage = EkeV
QCcell%mLambda = -1.0
QCcell%dmin    = ecpnl%dmin
dmin 		   = QCcell%dmin
xtalname 	   = 'iQC.qxtal'
nthreads 	   = ecpnl%nthreads

call Initialize_QCcell(QCcell, xtalname, dmin, sngl(QCcell%voltage), nthreads, verbose=.TRUE.)

! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters)

!=============================================
! generating list of incident wave vectors
!=============================================

! determine all independent incident beam directions inside the icosahedral unit triangle (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation
nullify(khead)
call QC_Calckvectors(khead,QCcell,ecpnl%nsamples,npy,numk)
io_int_sgl(1)=numk
call WriteValue('# independent beam directions to be considered = ', io_int_sgl, 1, "(I8)")

ktmp => khead
czero = cmplx(0.D0, 0.D0)

nullify(reflist)
nullify(firstw)

nns = 0
nnw = 0
tots = 0
totw = 0

numset = 1  ! number of special positions in the unit cell
izz = numzbins

allocate(lambdaZ(1:izz),stat=istat)
allocate(nat(numset),stat=istat)
allocate(kij(3,numk),stat=istat)
allocate(klist(3,numk),knlist(numk),stat=istat)

do iz=1,izz
    lambdaZ(iz) = float(sum(accum_z(floor(ind),iz,:,:)))/float(etotal)
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
fnat = 1.0
intthick = dble(depthmax)

outname = trim(EMsoft_getEMdatapathname())//trim(ecpnl%energyfile)
outname = EMsoft_toNativePath(outname)
datagroupname = 'ECPQCmaster'

! there are the full master pattern arrays to be filled at the end of the computation
! by applying the icosahedral rotational symmetry group to the computed data values
allocate(mLPNH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numset),stat=istat)
allocate(mLPSH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numset),stat=istat)
allocate(masterSPNH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numset))
allocate(masterSPSH(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx,numset))
allocate(maskarray(-ecpnl%npx:ecpnl%npx,-ecpnl%npx:ecpnl%npx))

! The computed intensities are stored in a smaller array of dimensions (ecpnl%nsamples+2) x (2*npy+1)
! This array encompasses two unit triangles of the icosahedral group and implements the mirror
! symmetry across the x-z plane, so that the final symmetry step only needs to apply the 60 rotational
! symmetry elements, not the full 120-member group 
allocate(Iarray(0:ecpnl%nsamples+1,-npy:npy,numset),stat=istat)

mLPNH = 0.0
mLPSH = 0.0
masterSPNH = 0.0
masterSPSH = 0.0
Iarray = -1.0      ! negative to make sure we do not use unassigned values in the interpolation process

nullify(HDF_head%next)
! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! Open an existing file or create a new file using the default properties.
if (trim(energyfile).eq.trim(outname)) then
  hdferr =  HDF_openFile(outname, HDF_head)
else
  hdferr =  HDF_createFile(outname, HDF_head)
end if

! if this file already contains an ECPmaster dataset, then we let the user
! know and gracefully abort the program.
!dataset = 'EMData/ECPmaster'
!call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
!if (g_exists) then 
!  call FatalError('ECQCmasterpattern','This file already contains an ECPQCmaster dataset') 
!end if

! write the EMheader to the file
groupname = 'ECPmaster'
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
call HDFwriteECPQCMasterNameList(HDF_head, ecpnl)
call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)
hdferr = HDF_createGroup(datagroupname, HDF_head)

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

! create the hyperslab and write zeroes to it for now
dataset = SC_mLPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
end if

dataset = SC_mLPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
end if

dataset = SC_masterSPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
end if

dataset = SC_masterSPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
offset3 = (/ 0, 0, 0/)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
else
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

call OMP_SET_NUM_THREADS(ecpnl%nthreads)
io_int(1) = ecpnl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

!$OMP PARALLEL &
!$OMP& PRIVATE(DynMat,Sgh,Sghtmp,Lgh,i,FN,TID,kn,ipx,ipy,ix,ip,iequiv,nequiv,reflist,firstw) &
!$OMP& PRIVATE(kk,nns,nnw,nref,nat,io_int,io_int_sgl,nthreads,svals) 

nthreads = OMP_GET_NUM_THREADS()
TID = OMP_GET_THREAD_NUM()

io_int_sgl(1) = nthreads

if (TID .eq. 0) call WriteValue('Setting number of threads to ', io_int_sgl, 1, "(I8)")

allocate(svals(numset))

!$OMP DO SCHEDULE(DYNAMIC)

beamloop: do i = 1, numk
    kk = klist(1:3,i)
    FN = kk/norm2(kk)

    nullify(reflist)

    call Initialize_QCReflectionList(QCcell, reflist, BetheParameters, FN, kk, nref)

! determine strong and weak reflections
    nullify(firstw)
    nns = 0
    nnw = 0
    call QC_Apply_BethePotentials(QCcell, reflist, firstw, BetheParameters, nref, nns, nnw)

    allocate(DynMat(nns,nns))

    call QC_GetDynMat(QCcell, reflist, firstw, DynMat, nns, nnw)
    DynMat = DynMat * cmplx(1.D0/cPi/QCcell%mLambda, 0.D0)

! then we need to initialize the Sgh and Lgh arrays
    if (allocated(Lgh)) deallocate(Lgh)
    if (allocated(Sghtmp)) deallocate(Sghtmp)

    allocate(Sghtmp(nns,nns,numset),Lgh(nns,nns))

    Lgh = czero
    Sghtmp = czero
    nat = 0
    call QC_CalcSgh(QCcell,reflist,nns,numset,Sghtmp,nat)

! solve the dynamical eigenvalue equation; this remains unchanged for QCs
    kn = knlist(i)
	izz = nint(thick(iE)/depthstep)

    call CalcLgh(DynMat,Lgh,intthick,dble(kn),nns,gzero,depthstep,lambdaZ,izz)
    deallocate(DynMat)

! dynamical contributions
     svals = 0.0
     do ix=1,numset
       svals(ix) = real(sum(Lgh(1:nns,1:nns)*Sghtmp(1:nns,1:nns,ix)))
     end do
     svals = svals/float(sum(nat(1:numset)))

! and store the resulting values in all the symmetryically equivalent positions
! for now, we're only using inversion symmetry
    ipx = kij(1,i)
    ipy = kij(2,i)
    Iarray(ipx,ipy,1) = svals(1)
    Iarray(ipx,-ipy,1) = svals(1)

    totw = totw + nnw
    tots = tots + nns

    deallocate(Lgh, Sghtmp)

    if (mod(i,500).eq.0) then
        io_int(1) = i
        call WriteValue('  completed beam direction ',io_int, 1, "(I8)")
    end if

    call Delete_QCgvectorlist(reflist)

end do beamloop

!$OMP END PARALLEL

io_int(1) = nint(float(tots)/float(numk))
call WriteValue(' -> Average number of strong reflections = ',io_int, 1, "(I5)")
io_int(1) = nint(float(totw)/float(numk))
call WriteValue(' -> Average number of weak reflections   = ',io_int, 1, "(I5)")

! next we need to apply icosahedral rotational symmetry to the double triangle of intensity values
! in the Iarray to generate the full mLPNH and mLPSH arrays
call QC_applyIcosahedralSymmetry(ecpnl%npx, ecpnl%nsamples+2, npy, numset, mLPNH, mLPSH, Iarray)
mLPSH = mLPNH

! get stereographic projections
  Radius = 1.0
  do i=-ecpnl%npx,ecpnl%npx 
    do j=-ecpnl%npx,ecpnl%npx 
      xy = (/ float(i), float(j) /) / float(ecpnl%npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      xyz = xyz/NORM2(xyz)
      if (ierr.ne.0) then 
        masterSPNH(i,j,1:numset) = 0.0
        masterSPSH(i,j,1:numset) = 0.0
        maskarray(i,j)           = .FALSE.
      else
        masterSPNH(i,j,1:numset) = InterpolateLambert(xyz, mLPNH, ecpnl%npx, numset)
        masterSPSH(i,j,1:numset) = InterpolateLambert(xyz, mLPSH, ecpnl%npx, numset)
        maskarray(i,j)           = .TRUE.
      end if
    end do
  end do

! and here is where the major changes are for this version 5.0: all output now in HDF5 format
call timestamp(datestring=dstr, timestring=tstre)

do inumset = 1,numset
! scale the stereographic projection
  ma = maxval(masterSPNH(:,:,inumset),mask = maskarray)
  mi = minval(masterSPNH(:,:,inumset),mask = maskarray)

  do i=-ecpnl%npx,ecpnl%npx 
    do j=-ecpnl%npx,ecpnl%npx 
      if(maskarray(i,j)) then
        masterSPNH(i,j,inumset) = (masterSPNH(i,j,inumset) - mi)/(ma - mi)
        masterSPSH(i,j,inumset) = (masterSPSH(i,j,inumset) - mi)/(ma - mi)
      end if
    end do
  end do

end do

datagroupname = 'ECPmaster'

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

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)
hdferr = HDF_openGroup(datagroupname, HDF_head)

! add data to the hyperslab
dataset = SC_mLPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset/)
offset3 = (/ 0, 0, 0 /)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)

dataset = SC_mLPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset/)
offset3 = (/ 0, 0, 0 /)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)

dataset = SC_masterSPNH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
offset3 = (/ 0, 0, 0/)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)

dataset = SC_masterSPSH
dims3 = (/  2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
cnt3 = (/ 2*ecpnl%npx+1, 2*ecpnl%npx+1, numset /)
offset3 = (/ 0, 0, 0/)
hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

call Message('Final data stored in file '//trim(outname), frm = "(A/)")


end subroutine ECQCmasterpattern


! function InterpolateLambert(dc, master, npx, nf) result(res)

! use local
! use Lambert
! use EBSDmod
! use constants

! IMPLICIT NONE

! real(kind=dbl),INTENT(INOUT)            :: dc(3)
! real(kind=sgl),INTENT(IN)               :: master(-npx:npx,-npx:npx, 1:nf)
! integer(kind=irg),INTENT(IN)            :: npx 
! integer(kind=irg),INTENT(IN)            :: nf
! real(kind=sgl)                          :: res(nf)

! integer(kind=irg)                       :: nix, niy, nixp, niyp, istat
! real(kind=sgl)                          :: xy(2), dx, dy, dxm, dym, scl

! scl = float(npx) 

! if (dc(3).lt.0.0) dc = -dc

! ! convert direction cosines to lambert projections
! xy = scl * LambertSphereToSquare( dc, istat )
! res = 0.0
! if (istat.eq.0) then 
! ! interpolate intensity from the neighboring points
!   nix = floor(xy(1))
!   niy = floor(xy(2))
!   nixp = nix+1
!   niyp = niy+1
!   if (nixp.gt.npx) nixp = nix
!   if (niyp.gt.npx) niyp = niy
!   dx = xy(1) - nix
!   dy = xy(2) - niy
!   dxm = 1.0 - dx
!   dym = 1.0 - dy
!   res(1:nf) = master(nix,niy,1:nf)*dxm*dym + master(nixp,niy,1:nf)*dx*dym + &
!         master(nix,niyp,1:nf)*dxm*dy + master(nixp,niyp,1:nf)*dx*dy
! end if

! end function InterpolateLambert



