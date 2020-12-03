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
! EMsoft:EMEBSDmasterSHT.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDmasterSHT
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSDmasterSHT produces a master .sht file for a given structure.
!
!> @todo implement full symmetry use (trigonal/rhombohedral is currently not implemented) 
!>
!> @date  10/25/19  MDG 1.0 split off from regular EMEBSDmaster program 
!--------------------------------------------------------------------------
program EMEBSDmasterSHT

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
type(EBSDMasterSHTNameListType)         :: emnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMEBSDmasterSHT.nml'
progname = 'EMEBSDmasterSHT.f90'
progdesc = 'EBSD Master Pattern Simulation for Spherical Indexing'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 281, 0 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  ! call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
else
  call GetEBSDMasterSHTNameList(nmldeffile,emnl)
end if

! generate a master pattern on the Legendre lattitudinal grid
! and store the SHT coefficients in a compact .h5 file.
call ComputeSHTMasterPattern(emnl, progname, nmldeffile)

end program EMEBSDmasterSHT

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeSHTMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an EBSD master pattern and output its Spherical Harmonic transform
!
!> @param emnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 09/25/19  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeSHTMasterPattern(emnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use EBSDmod
use MBmodule
use symmetry
use crystal
use constants
use error
use gvectors
use kvectors
use io
use math
use image
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
use, intrinsic :: iso_fortran_env
use omp_lib
use notifications
use stringconstants
use DSHT 
use fft_wrap

IMPLICIT NONE

interface 
  recursive function writeShtFile (fn, iprm, fprm, doi, note, alm, aTy, aCd, vers, cprm) result(res) &
  bind(C, name ='writeShtFile_')

  use ISO_C_BINDING

  IMPLICIT NONE 

  character(c_char)             :: fn
  integer(c_int)                :: iprm(11) 
  real(c_float)                 :: fprm(25)
  character(c_char)             :: doi
  character(c_char)             :: note 
  real(C_DOUBLE_COMPLEX)        :: alm(2*(iprm(3)+3)*(iprm(3)+3))
  integer(c_int)                :: aTy(iprm(6))
  real(c_float)                 :: aCd(iprm(6),5)
  character(c_char)             :: vers 
  character(c_char)             :: cprm
  integer(c_int)                :: res
  end function writeShtFile
end interface 


type(EBSDMasterSHTNameListType),INTENT(INOUT) :: emnl
character(fnlen),INTENT(IN)                   :: progname
character(fnlen),INTENT(IN)                   :: nmldeffile

real(kind=dbl)          :: ctmp(192,3), arg, Radius, xyz(3)
integer(HSIZE_T)        :: dims4(4), cnt4(4), offset4(4)
integer(HSIZE_T)        :: dims3(3), cnt3(3), offset3(3)
integer(kind=irg)       :: isym,i,j,ik,npy,ipx,ipy,ipz,debug,iE,izz, izzmax, iequiv(3,48), nequiv, num_el, MCnthreads, & ! counters
                           numk, timestart, timestop, numsites, res, ll, nthreads, & ! number of independent incident beam directions
                           ir,nat(maxpasym),kk(3), skip, ijmax, one, NUMTHREADS, TID, SamplingType, &
                           numset,n,ix,iy,iz, io_int(6), nns, nnw, nref, Estart, bw, d, &
                           istat,gzero,ic,ip,ikk, totstrong, totweak, jh, ierr, nix, niy, nixp, niyp     ! counters
real(kind=dbl)          :: tpi,Znsq, kkl, DBWF, kin, delta, h, lambda, omtl, srt, dc(3), xy(2), edge, scl, tmp, dx, dxm, dy, &
                           dym, mean, sdev !
real(kind=sgl)          :: io_real(5), selE, kn, FN(3), kkk(3), tstop, bp(4), nabsl, etotal, dens, avA, avZ, xxxxx, mi, ma
real(kind=sgl),allocatable      :: EkeVs(:), svals(:), auxNH(:,:), auxSH(:,:), Z2percent(:)  ! results
real(kind=sgl),allocatable      :: mLPNH(:,:,:), mLPSH(:,:,:)
real(kind=dbl),allocatable      :: LegendreArray(:), upd(:), diagonal(:)
complex(kind=dbl)               :: czero
complex(kind=dbl),allocatable   :: Lgh(:,:), Sgh(:,:,:)
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
logical                 :: f_exists, readonly, overwrite=.TRUE., insert=.TRUE., g_exists, xtaldataread, FL, doLegendre
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
character(fnlen)                :: dataset, instring, SHTfile
type(EBSDMCdataType)            :: EBSDMCdata
type(MCCLNameListType)          :: mcnl

type(HDFobjectStackType)        :: HDF_head

character(fnlen),ALLOCATABLE    :: MessageLines(:)
integer(kind=irg)               :: NumLines, info
character(fnlen)                :: SlackUsername, exectime, layout, doiString
character(100)                  :: c

real(kind=dbl),allocatable      :: finalmLPNH(:,:), finalmLPSH(:,:), weights(:)
type(DiscreteSHT)               :: transformer 
complex(kind=dbl), allocatable  :: almMaster(:,:)   ! spectra of master pattern to index against
complex(kind=dbl), allocatable  :: almPat   (:,:)   ! work space to hold spectra of exerimental pattern
real(kind=dbl),allocatable      :: alm(:)

! parameters for the .sht output file 
integer(kind=irg),parameter     :: nipar=11, nfpar=25, npx=193
character(fnlen)                :: EMversion, cprm, note, notestring
character(6)                    :: vstring
character(8)                    :: vstring2
character(fnlen)                :: revision
integer(c_int32_t)              :: sgN      ! space group number [1,230]
integer(c_int32_t)              :: sgS      ! space group setting [1,2]
integer(c_int32_t)              :: numAt    ! number of atoms
integer(c_int32_t),allocatable  :: aTy(:)   ! atom types (nAt atomic numbers)
real(c_float),allocatable       :: aCd(:,:) ! atom coordinates, (nAt * 5 floats {x, y, z, occupancy, Debye-Waller in nm^2})
real(c_float)                   :: lat(6)   ! lattice parameters {a, b, a, alpha, beta, gamma} (in nm / degree)
real(c_float)                   :: fprm(nfpar) ! floating point parameters (float32 EMsoftED parameters in order)
integer(c_int32_t)              :: iprm(nipar) ! integer parameters {# electrons, electron multiplier, numsx, npx, latgridtype}
! character(1,kind=c_char)                    :: zRot, mirInv 
! character(1,kind=c_char),dimension(2)       :: flg      ! flg: symmetry flags {zRot, mirInv}
! alm: actual harmonics (uncompressed format)

! declare variables for use in object oriented image module
character(fnlen)                        :: image_filename
integer                                 :: iostat
character(len=128)                      :: iomsg
logical                                 :: isInteger
type(image_t)                           :: im, im2
integer(int8)                           :: i8 (3,4), int8val
integer(int8), allocatable              :: output_image(:,:)


!$OMP THREADPRIVATE(rlp) 

! this routine computes a full master pattern and subsequently computes the SHT for 
! storage in a compact ..sht file; this routine uses a Legendre lattitudinal grid

emnl%combinesites = .TRUE.

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(timestart)

tpi = 2.D0*cPi
czero = dcmplx(0.D0,0.D0)

!=============================================
!=============================================
! ---------- read Monte Carlo .h5 output file and extract necessary parameters
call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(emnl%energyfile, mcnl, hdferr, EBSDMCdata, &
                            getAccume=.TRUE., getAccumz=.TRUE.)
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

energyfile = trim(EMsoft_getEMdatapathname())//trim(emnl%energyfile)
energyfile = EMsoft_toNativePath(energyfile)

!=============================================
!=============================================
! crystallography section; 
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

! allocate and compute the Sgh loop-up table
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
! this is where we determine the value for the thickness integration limit for the CalcLgh3 routine...

! then, for each energy determine the 95% histogram thickness
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

! and get rid of the EBSDMCdata%accum_z array
deallocate(EBSDMCdata%accum_z)
! ---------- end of 'read Monte Carlo output file and extract necessary parameters' section
!=============================================
!=============================================

!=============================================
!=============================================
! ---------- a couple of initializations
   npy = npx
   allocate(svals(numset),stat=istat)
   gzero = 1  ! index of incident beam
   debug = 0  ! no longer used
! ----------
!=============================================
!=============================================

!=============================================
!=============================================
! if the combinesites parameter is .TRUE., then we only need to 
! allocate a dimension of 1 in the master pattern array since we are adding 
! together the master patterns for all sites in the asymmetric unit.
  numsites = 1

! ---------- allocate memory for the master patterns
  allocate(mLPNH(-npx:npx,-npy:npy,EBSDMCdata%numEbins),stat=istat)
  allocate(mLPSH(-npx:npx,-npy:npy,EBSDMCdata%numEbins),stat=istat)
  mLPNH = 0.0
  mLPSH = 0.0
! ---------- end allocate memory for the master patterns
!=============================================
!=============================================

! force dynamical matrix routine to read new Bethe parameters from file
! this will all be changed with the new version of the Bethe potentials
  call Set_Bethe_Parameters(BetheParameters,.TRUE.)

!=============================================
!=============================================
! precompute the Legendre array for the new lattitudinal grid values
  call Message(' Computing Legendre lattitudinal grid values')
  allocate(diagonal(2*npx+1),upd(2*npx+1))
  diagonal = 0.D0
  upd = (/ (dble(i) / dsqrt(4.D0 * dble(i)**2 - 1.D0), i=1,2*npx+1) /)
  call dsterf(2*npx-1, diagonal, upd, info) 
! the eigenvalues are stored from smallest to largest and we need them in the opposite direction
!   allocate(LegendreArray(2*npx+1))
!   LegendreArray(1:2*npx+1) = diagonal(2*npx+1:1:-1)
! ! set the center eigenvalue to 0
!   LegendreArray(npx+1) = 0.D0
  allocate(LegendreArray(0:2*npx))
  LegendreArray(0:2*npx) = diagonal(2*npx+1:1:-1)
! set the center eigenvalue to 0
  LegendreArray(npx) = 0.D0
  deallocate(diagonal, upd)

!=============================================
!=============================================
! start energy value for the energyloop
  Estart = EBSDMCdata%numEbins

!=============================================
!=============================================

call Time_tick(timestart)

energyloop: do iE=Estart,1,-1
! print a message to indicate where we are in the computation
   io_int(1)=iE
   io_int(2)=Estart
   call Message(' Starting computation for energy bin (in reverse order)', frm = "(/A$)")
   call WriteValue(' ',io_int,2,"(I4,' of ',I4$)")
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
    call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,npx,npy,numk, &
                      SamplingType,ijmax,'RoscaLambertLegendre',usehex, LegendreArray)
  else 
    call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,npx,npy,numk, &
                      SamplingType,ijmax,'RoscaLambertLegendre',usehex, LegendreArray)
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
     call CalcLgh(DynMat,Lgh,dble(thick(iE)),dble(kn),nns,gzero,mcnl%depthstep,lambdaE(iE,1:izzmax),izzmax)
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
       call Apply3DPGSymmetry(cell,ipx,ipy,ipz,npx,iequiv,nequiv,usehex)
     else
       if ((cell%SYM_SGnum.ge.195).and.(cell%SYM_SGnum.le.230)) then
         call Apply3DPGSymmetry(cell,ipx,ipy,ipz,npx,iequiv,nequiv,cubictype=SamplingType)
       else
         call Apply3DPGSymmetry(cell,ipx,ipy,ipz,npx,iequiv,nequiv)
       end if
     end if
!$OMP CRITICAL
     do ix=1,nequiv
       if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),iE) = sum(svals)
       if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),iE) = sum(svals)
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
    allocate(auxNH(-npx:npx,-npy:npy),stat=istat)
    allocate(auxSH(-npx:npx,-npy:npy),stat=istat)
    auxNH(:,:) = mLPNH(:,:,iE)
    auxSH(:,:) = mLPSH(:,:,iE)
! 
    edge = 1.D0 / dble(npx)
    scl = float(npx) 
    do i=-npx,npx
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
          if (nixp.gt.npx) nixp = nix
          if (niyp.gt.npx) niyp = niy
          dx = xy(1) - nix
          dy = xy(2) - niy
          dxm = 1.D0 - dx
          dym = 1.D0 - dy
          mLPNH(i,j,iE) = auxNH(nix,niy)*dxm*dym + auxNH(nixp,niy)*dx*dym + &
                               auxNH(nix,niyp)*dxm*dy + auxNH(nixp,niyp)*dx*dy
          mLPSH(i,j,iE) = auxSH(nix,niy)*dxm*dym + auxSH(nixp,niy)*dx*dym + &
                               auxSH(nix,niyp)*dxm*dy + auxSH(nixp,niyp)*dx*dy
        end if
      end do
    end do
    deallocate(auxNH, auxSH)
  end if

! make sure that the outer pixel rim of the mLPSH patterns is identical to
! that of the mLPNH array.
  mLPSH(-npx,-npx:npx,iE) = mLPNH(-npx,-npx:npx,iE)
  mLPSH( npx,-npx:npx,iE) = mLPNH( npx,-npx:npx,iE)
  mLPSH(-npx:npx,-npx,iE) = mLPNH(-npx:npx,-npx,iE)
  mLPSH(-npx:npx, npx,iE) = mLPNH(-npx:npx, npx,iE)

! since these computations can take a long time, here we store 
! all the output at the end of each pass through the energyloop.

  io_int(1) = nint(float(totstrong)/float(numk))
  call WriteValue(' -> Average number of strong reflections = ',io_int, 1, "(I5)")
  io_int(1) = nint(float(totweak)/float(numk))
  call WriteValue(' -> Average number of weak reflections   = ',io_int, 1, "(I5)")

end do energyloop

!==================================
! next, we compute the final energy weighted master pattern and its spherical harmonic transform
call Message(' Computing energy weighted master pattern',"(//A)")
  n = size(EBSDMCdata%accum_e, 1) ! get number of energy bins
  allocate(weights(n)) ! allocate space for energy histogram
  do i = 1, n
    weights(i) = sum(EBSDMCdata%accum_e(i,:,:)) ! this could be modified to sum over partial rectangle
  enddo
  weights = weights / sum(weights) ! this is currently wieghted over the full square Lambert

 ! build energy weighted master pattern
  d = npx
  allocate(finalmLPNH(-d:d,-d:d))
  allocate(finalmLPSH(-d:d,-d:d))
  finalmLPNH = 0.D0
  finalmLPSH = 0.D0
  do i = 1, n
    finalmLPNH = finalmLPNH + mLPNH(:,:,i) * weights(i)
    finalmLPSH = finalmLPSH + mLPSH(:,:,i) * weights(i)
  enddo

!=====================================
! form the .sht file name from the formula, name, structuresymbol and voltage parameters
  SHTfile = trim(emnl%SHT_folder)//'/'//trim(emnl%SHT_formula)
! compound name (:brass, forsterite, alpha-quartz ... )
  if ((trim(emnl%SHT_name).ne.'undefined').and.(trim(emnl%SHT_name).ne.'')) then
    SHTfile = trim(SHTfile)//' ('//trim(emnl%SHT_name)//')'
  end if
! structure symbol (StrukturBericht, Pearson, ...)
  if ((trim(emnl%SHT_structuresymbol).ne.'undefined').and.(trim(emnl%SHT_structuresymbol).ne.'')) then
    SHTfile = trim(SHTfile)//' ['//trim(emnl%SHT_structuresymbol)//']'
  end if
! voltage string 
  write(vstring,"(' {',I2.2,'kV')") int(mcnl%EkeV) 
  SHTfile = trim(SHTfile)//trim(vstring)
! if the sample tilt is NOT equal to 70 deg, then add it in F4.1 format to the comment string 
  if (mcnl%sig.ne.70.0) then 
    write (vstring2,"(' ',F4.1,'deg')") sngl(mcnl%sig)
    SHTfile = trim(SHTfile)//trim(vstring2)//'}.sht'
  else
    SHTfile = trim(SHTfile)//'}.sht'
  end if

!=====================================
  call Message(' Saving Lambert squares to tiff file ',"(//A)")
! output these patterns to a tiff file
  image_filename = trim(SHTfile)
  ll = len(trim(image_filename))
  ll = ll-2
! replace the .sht extension by .tiff
  image_filename(ll:ll) = 't'
  image_filename(ll+1:ll+1) = 'i'
  image_filename(ll+2:ll+2) = 'f'
  image_filename(ll+3:ll+3) = 'f'
  image_filename = trim(EMsoft_getEMdatapathname())//trim(image_filename)
  image_filename = EMsoft_toNativePath(image_filename)

  allocate(output_image(2*(2*d+1)+2,2*d+1))
  output_image = -1

  mi = minval( (/ minval(finalmLPNH), minval(finalmLPSH) /) )
  ma = maxval( (/ maxval(finalmLPNH), maxval(finalmLPSH) /) )

  do i=1,2*d+1
    output_image(1:2*d+1,i) = int(255*(finalmLPNH(-d:d,i-1-d)-mi)/(ma-mi))
    output_image(2*d+1+2:2*(2*d+1)+2,i) = int(255*(finalmLPSH(-d:d,i-1-d)-mi)/(ma-mi))
  end do
  
  im2 = image_t(output_image)
  if(im2%empty()) call Message("ComputeSHTMasterPattern","failed to convert array to image")

! create the file
  call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
  if(0.ne.iostat) then
    call Message("   --> Failed to write image to file : "//iomsg)
  else  
    call Message('   --> Lambert projections written to '//trim(image_filename))
  end if 
  deallocate(output_image)


!=====================================
! normalize the master patterns by subtracting the mean and dividing by the standard deviation 
  call Message(' Normalizing master patterns ')
  mean = sum(finalmLPNH) / dble( (2*d+1)*(2*d+1) )
  sdev = sqrt( sum( (finalmLPNH - mean)**2 )/ dble((2*d+1)*(2*d+1) - 1) )
  finalmLPNH = (finalmLPNH - mean) / sdev 

  mean = sum(finalmLPSH) / dble( (2*d+1)*(2*d+1) )
  sdev = sqrt( sum( (finalmLPSH - mean)**2 )/ dble((2*d+1)*(2*d+1) - 1) )
  finalmLPSH = (finalmLPSH - mean) / sdev 

!=====================================
! build transformer
   call FFTWisdom%load()
   call Message(' Initializing the spherical harmonic transformer')
   layout = 'legendre'
   bw = 384
   d = bw / 2 + 1
   call transformer%init(d, bw, layout)

!=====================================
! compute spherical harmonic transform of master pattern
   call Message(' Computing spherical harmonic transform')
   allocate(almMaster(0:bw-1, 0:bw-1))
   allocate(almPat   (0:bw-1, 0:bw-1))
   call transformer%analyze(finalmLPNH, finalmLPSH, almMaster)
   call FFTWisdom%save()

  timestop = Time_tock(timestart)

!=====================================
! prepare all parameters for the writing of the final .sht file 
!
! first the integers [see EMsoftLib/sht_file.cpp for definitions]
  bw = 384
  iprm = (/ cell%SYM_SGnum, 1, bw, & 
            cell%SYM_SGnum, cell%SYM_SGset, cell%ATOM_ntype, &
            mcnl%totnum_el, mcnl%multiplier, mcnl%numsx, npx, 2 /)
  lat(1) = sngl(cell%a)
  lat(2) = sngl(cell%b)
  lat(3) = sngl(cell%c) 
  lat(4) = sngl(cell%alpha) 
  lat(5) = sngl(cell%beta) 
  lat(6) = sngl(cell%gamma)

! then the floats [see EMsoftLib/sht_file.cpp for definitions]
  fprm = (/ sngl(mcnl%EkeV), sngl(mcnl%sig), 0.0, nan(), lat(1),lat(2),lat(3),lat(4),lat(5),lat(6), &
            sngl(mcnl%sigstart), sngl(mcnl%sigend), sngl(mcnl%sigstep), sngl(mcnl%omega), sngl(mcnl%EkeV), &
            sngl(mcnl%Ehistmin), sngl(mcnl%Ebinsize), sngl(mcnl%depthmax), sngl(mcnl%depthstep), infty(), &
            BetheParameters%c1, BetheParameters%c2, BetheParameters%c3, BetheParameters%sgdbdiff, emnl%dmin /)

! doi string 
  doiString = ''
! the 'addtoKiltHub' option should only be used for the original 120 structures in the KiltHub data base
  if (trim(emnl%addtoKiltHub).eq.'Yes') then 
    doiString = SC_EMSHTDOI
  else
    if (trim(emnl%useDOI).ne.'undefined') then ! use the user-defined DOI string
      doiString = trim(emnl%useDOI)
    else
! if we get here, then we use the generic DOI for the Zenodo link to the GitHub .sht data base repository
      doiString = SC_ZENODODOI
    end if 
  end if 

! atom types and coordinates
  numAt = cell%ATOM_ntype 
  allocate(aTy(numAt))
  aTy = cell%ATOM_type(1:numAt)
  allocate(aCd(5,numAt))
  aCd = transpose(cell%ATOM_pos(1:numAt,1:5))

! transfer the complex almMaster array to a flat array with alternating real and imaginary parts
  allocate(alm( 2 * bw * bw ))
  alm = transfer(almMaster,alm)

  revision = trim(EMsoft_getEMsoftRevision())
  notestring = ''

  cprm = cstringify(emnl%SHT_formula)// &
         cstringify(emnl%SHT_name)// &
         cstringify(emnl%SHT_structuresymbol)// &
         cstringify(cell%source)// &
         trim(notestring)
  
! write an .sht file using EMsoft style EBSD data
  SHTfile = trim(EMsoft_getEMdatapathname())//trim(SHTfile)
  SHTfile = EMsoft_toNativePath(SHTfile)

  res = writeShtFile(cstringify(SHTfile), iprm, fprm, &
                     cstringify(doiString), cstringify(note), alm, &
                     aTy, aCd, cstringify(revision), cstringify(cprm))

  call Message(' Final data stored in binary file '//trim(SHTfile), frm = "(A/)")



io_int(1) = timestop
call WriteValue(' Total execution time [s] ',io_int,1)

! if requested, we notify the user that this program has completed its run
if (trim(EMsoft_getNotify()).ne.'Off') then
  if (trim(emnl%Notify).eq.'On') then 
    NumLines = 3
    allocate(MessageLines(NumLines))

    call hostnm(c)
 
    MessageLines(1) = ' EMEBSDmaster program has ended successfully'
    MessageLines(2) = ' Master pattern data stored in '//trim(outname)
    write (exectime,"(F10.4)") tstop  
    MessageLines(3) = ' Total execution time [s]: '//trim(exectime)
    SlackUsername = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, SlackUsername)
  end if
end if

end subroutine ComputeSHTMasterPattern

