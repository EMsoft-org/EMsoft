! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEECmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEECmaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEECmaster computes a master EEC pattern for a given crystal structure
!
!> @note This is a test program based on conversations with Eric Bosne
! 
!> @date  12/13/19  MDG 1.0 new program, based on EMEBSDmaster
!--------------------------------------------------------------------------
program EMEECmaster

use local
use NameListTypedefs
use NameListHandlers
use files
use io
use error

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(EECMasterNameListType)             :: emnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMEECmaster.nml'
progname = 'EMEECmaster.f90'
progdesc = 'Electron Emission Channeling Master Pattern Simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 121, 0 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  ! call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
  call FatalError('EMEECmaster','json input not yet implemented')
else
  call GetEECMasterNameList(nmldeffile,emnl)
end if

! generate a standard set of master EBSD patterns
call ComputeMasterPattern(emnl, progname, nmldeffile)

end program EMEECmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an EEC master pattern as a function of energy
!
!> @param emnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 12/13/19  MDG 1.0 original, bsased on EMEBSDmaster program
!--------------------------------------------------------------------------
subroutine ComputeMasterPattern(emnl, progname, nmldeffile)

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

type(EECMasterNameListType),INTENT(INOUT)  :: emnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

real(kind=dbl)          :: ctmp(192,3), arg, Radius, xyz(3)
integer(HSIZE_T)        :: dims4(4), cnt4(4), offset4(4)
integer(HSIZE_T)        :: dims3(3), cnt3(3), offset3(3)
integer(kind=irg)       :: isym,i,j,ik,npy,ipx,ipy,ipz,iE,izz, izzmax, iequiv(3,48), nequiv, num_el, MCnthreads, & ! counters
                           numk, timestart, timestop, numsites, nthreads, & ! number of independent incident beam directions
                           ir,kk(3), skip, ijmax, one, NUMTHREADS, TID, SamplingType, &
                           n,ix,iy,iz, io_int(6), nns, nnw, nref, &
                           istat,gzero,ic,ip,ikk, totstrong, totweak, jh, ierr, nix, niy, nixp, niyp     ! counters
real(kind=dbl)          :: tpi,Znsq, kkl, DBWF, kin, delta, h, lambda, omtl, srt, dc(3), xy(2), edge, scl, tmp, dx, dxm, dy, dym !
real(kind=sgl)          :: io_real(5), kn, FN(3), kkk(3), tstop, bp(4), nabsl, etotal, dens, avA, avZ, EkeV, &
                           mu, sig, depthstep, svals, xyzs(3)
real(kind=sgl),allocatable      :: auxNH(:,:), auxSH(:,:), Z2percent(:), lambdaE(:)  ! results
real(kind=sgl),allocatable      :: mLPNH(:,:), mLPSH(:,:), masterSPNH(:,:), masterSPSH(:,:)
complex(kind=dbl)               :: czero
complex(kind=dbl),allocatable   :: Lgh(:,:), Sgh(:,:)
logical                 :: usehex, switchmirror, verbose
character(fnlen)        :: xtalname

integer(kind=irg)       :: hdferr, nlines, thick! variables used in MC energy file
character(fnlen)        :: oldprogname, groupname, energyfile, outname, datagroupname, attributename, HDF_FileVersion
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

type(HDFobjectStackType)        :: HDF_head

character(fnlen),ALLOCATABLE    :: MessageLines(:)
integer(kind=irg)               :: NumLines, info
character(fnlen)                :: SlackUsername, exectime
character(100)                  :: c

!$OMP THREADPRIVATE(rlp) 

stereog = .TRUE.

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(timestart)

tpi = 2.D0*cPi
czero = cmplx(0.D0,0.D0)

doLegendre = .FALSE.
EkeV = emnl%IsotopeEnergy

!=============================================
! create a new output file or open an existing file?
!=============================================
  outname = trim(EMsoft_getEMdatapathname())//trim(emnl%mpfile)
  outname = EMsoft_toNativePath(outname)

!=============================================
!=============================================
! crystallography section; 
 verbose = .TRUE.
 call Initialize_Cell(cell, Dyn, rlp, emnl%xtalname, emnl%dmin, EkeV, verbose)

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
! call CalcDensity(cell, dens, avZ, avA, Z2percent)

!=============================================
! allocate and compute the Sgh look-up table
! generate all the sites equivalent to the input isotope site 
cell%SG%SYM_reduce = .TRUE.
call CalcEquivPos(cell, emnl%IsotopeSite, nequiv, ctmp)
call Initialize_SghLUTEEC(cell, emnl%dmin, nequiv, ctmp, verbose)

io_int(1) = nequiv
call WriteValue(' number of equivalent isotope positions: ', io_int, 1)
do i=1,nequiv
  write(*,*) ctmp(i,1:3)
end do
call Message('--------------')

!=============================================
! symmetry handling:
! determine the point group number
 j=0
 do i=1,32
  if (SGPG(i).le.cell%SYM_SGnum) j=i
 end do
 isym = j

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
! define the thickness profile using the mean free path parameter (may need to be modified, 12/14/19)

! for now, we'll go with a simple Gaussian implantation profile for a depth range of 50 nm, just
! as a proof-of-concept.  Usually, the sample temperature is pretty high, so we need to use a 
! realistic Debye-Waller factor for the input crystal structure.

depthstep = 1.0  ! nm 
thick = 50.0     ! nm
mu = 20.0        ! maximum of Gaussian
sig = 10.0       ! standard deviation (sigma)

izz = nint(thick/depthstep)
allocate(lambdaE(1:izz),stat=istat)
cell%voltage = EkeV
call CalcUcg(cell,rlp,(/0,0,0/))
nabsl = rlp%xgp
do iz=1,izz
  lambdaE(iz) = exp( -(izz*depthstep - mu)**2/2.0/sig**2 )
end do
lambdaE = lambdaE / (sqrt(2.0*sngl(cPi))*sig)

!=============================================
!=============================================
! ---------- a couple of initializations
   npy = emnl%npx
   gzero = 1  ! index of incident beam

!=============================================
!=============================================
! ---------- allocate memory for the master patterns
  allocate(mLPNH(-emnl%npx:emnl%npx,-npy:npy),stat=istat)
  allocate(mLPSH(-emnl%npx:emnl%npx,-npy:npy),stat=istat)
  allocate(masterSPNH(-emnl%npx:emnl%npx,-npy:npy))
  allocate(masterSPSH(-emnl%npx:emnl%npx,-npy:npy))

! set various arrays to zero
 mLPNH = 0.0
 mLPSH = 0.0
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
  hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
  datagroupname = SC_EECmaster
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EECmasterNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteEECMasterNameList(HDF_head, emnl)
  call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

! create the EBSDmaster group and add a HDF_FileVersion attribbute to it 
  hdferr = HDF_createGroup(datagroupname, HDF_head)
  HDF_FileVersion = '4.0'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0
! =====================================================
dataset = SC_xtalname
  allocate(stringarray(1))
  stringarray(1)= trim(emnl%xtalname)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  end if

dataset = SC_BetheParameters
  bp = (/ BetheParameters%c1, BetheParameters%c2, BetheParameters%c3, BetheParameters%sgdbdiff /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head)
  end if

dataset = SC_EkeV
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then 
      hdferr = HDF_writeDatasetFloat(dataset, EkeV, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, EkeV, HDF_head)
    end if

! =====================================================
! end of HDF_FileVersion = 4.0 write statements
! =====================================================

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

!=============================================
!=============================================

call Time_tick(timestart)

! print a message to indicate where we are in the computation
call Message(' Starting EEC master pattern computation')

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
call WriteValue(' # independent beam directions to be considered = ', io_int, 1, "(I8)")

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
  allocate(Sgh(nns,nns),Lgh(nns,nns))
  Sgh = czero
  Lgh = czero
  call getSghfromLUTEEC(cell,reflist,nns,Sgh)


! solve the dynamical eigenvalue equation for this beam direction  
  kn = karray(4,ik)
  call CalcLgh(DynMat,Lgh,dble(thick),dble(kn),nns,gzero,dble(depthstep),lambdaE,izz)
  deallocate(DynMat)

! sum over the element-wise (Hadamard) product of the Lgh and Sgh arrays 
  svals = real(sum(Lgh*Sgh))
  svals = svals/float(nequiv)

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
   if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix)) = svals
   if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix)) = svals
  end do
!$OMP END CRITICAL
  
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
  allocate(auxNH(-emnl%npx:emnl%npx,-npy:npy),stat=istat)
  allocate(auxSH(-emnl%npx:emnl%npx,-npy:npy),stat=istat)
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
        mLPNH(i,j) = auxNH(nix,niy)*dxm*dym + auxNH(nixp,niy)*dx*dym + &
                             auxNH(nix,niyp)*dxm*dy + auxNH(nixp,niyp)*dx*dy
        mLPSH(i,j) = auxSH(nix,niy)*dxm*dym + auxSH(nixp,niy)*dx*dym + &
                             auxSH(nix,niyp)*dxm*dy + auxSH(nixp,niyp)*dx*dy
      end if
    end do
  end do
  deallocate(auxNH, auxSH)
end if

! make sure that the outer pixel rim of the mLPSH patterns is identical to
! that of the mLPNH array.
mLPSH(-emnl%npx,-emnl%npx:emnl%npx) = mLPNH(-emnl%npx,-emnl%npx:emnl%npx)
mLPSH( emnl%npx,-emnl%npx:emnl%npx) = mLPNH( emnl%npx,-emnl%npx:emnl%npx)
mLPSH(-emnl%npx:emnl%npx,-emnl%npx) = mLPNH(-emnl%npx:emnl%npx,-emnl%npx)
mLPSH(-emnl%npx:emnl%npx, emnl%npx) = mLPNH(-emnl%npx:emnl%npx, emnl%npx)


! get stereographic projections (summed over the atomic positions)
Radius = 1.0
do i=-emnl%npx,emnl%npx 
  do j=-emnl%npx,emnl%npx 
    xy = (/ float(i), float(j) /) / float(emnl%npx)
    xyz = StereoGraphicInverse( xy, ierr, Radius )
    xyz = xyz/vecnorm(xyz)
    xyzs = sngl(xyz)
    if (ierr.ne.0) then 
      masterSPNH(i,j) = 0.0
      masterSPSH(i,j) = 0.0
    else
      masterSPNH(i,j) = InterpolateLambert(xyzs, mLPNH, emnl%npx)
      masterSPSH(i,j) = InterpolateLambert(xyzs, mLPSH, emnl%npx)
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

datagroupname = SC_EECMaster

nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
hdferr =  HDF_openFile(outname, HDF_head)

! ! update the time string
! groupname = SC_EMheader
!   hdferr = HDF_openGroup(groupname, HDF_head)
!   hdferr = HDF_openGroup(datagroupname, HDF_head)

! dataset = SC_StopTime
!   line2(1) = dstr//', '//tstre
!   hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

!   tstop = Time_tock(timestart)
!   io_int(1) = tstop
!   call WriteValue(' Execution time [s]: ',io_int,1)

! dataset = SC_Duration
!   call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
!   if (g_exists) then     
!     hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
!   else
!     hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
!   end if

!   call HDF_pop(HDF_head)
!   call HDF_pop(HDF_head)
  
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

! write the arrays
dataset = SC_mLPNH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray2D(dataset, mLPNH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head, insert)
  else
    hdferr = HDF_writeDatasetFloatArray2D(dataset, mLPNH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head)
  end if

dataset = SC_mLPSH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray2D(dataset, mLPSH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head, insert)
  else
    hdferr = HDF_writeDatasetFloatArray2D(dataset, mLPSH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head)
  end if

dataset = SC_masterSPNH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSPNH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head, insert)
  else
    hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSPNH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head)
  end if

dataset = SC_masterSPSH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSPSH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head, insert)
  else
    hdferr = HDF_writeDatasetFloatArray2D(dataset, masterSPSH, 2*emnl%npx+1, 2*emnl%npx+1, HDF_head)
  end if

call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

timestop = Time_tock(timestart)
io_int(1) = timestop
call WriteValue(' Total execution time [s] ',io_int,1)

! if requested, we notify the user that this program has completed its run
if (trim(EMsoft_getNotify()).ne.'Off') then
  if (trim(emnl%Notify).eq.'On') then 
    NumLines = 3
    allocate(MessageLines(NumLines))

    call hostnm(c)
 
    MessageLines(1) = 'EMEECmaster program has ended successfully'
    MessageLines(2) = 'Master pattern data stored in '//trim(outname)
    write (exectime,"(F10.4)") tstop  
    MessageLines(3) = 'Total execution time [s]: '//trim(exectime)
    SlackUsername = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, SlackUsername)
  end if
end if

end subroutine ComputeMasterPattern
