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
!--------------------------------------------------------------------------
! EMsoft:EMKosselmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMKosselmaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMKosselmaster computes the master Kossel pattern for a given structure
!
!> @todo implement full symmetry use; implement multiple reflection output
!> or, easier perhaps, selection of one reflection;
!>
!> implement OpenMP multithreading for the actual computation part; requires modifications
!> in EMlib.a routines (mostly THREADPRIVATE commands in several modules)
!
!> @date  03/08/12  MDG 1.0 EBSD program for fundamental zone patterns
!> @date  08/17/12  MDG 1.1 added generalized fundamental zone for all crystal symmetries
!> @date  08/20/12  MDG 1.2 modifid FZ to use the Lambert projection of a square sampling grid
!> @date  09/06/12  MDG 1.3 added support for second setting of Laue group -3m
!> @date  11/21/12  MDG 2.0 added full Lambert projection support for both square and hexagonal grids
!>				   the older code is still available for now, but will be removed after validation
!>				   of the newer code.
!> @date  12/04/12  MDG 2.1 added support for equilateral triangle mapping; needs to be validated.
!>				   also modified the structure of the output file, so that EBSD.f90 will
!>				   know which of the inverse mapping methods it should use.
!> @date  12/10/12  MDG 3.0 expanded EBSDFZ program to include energy-dependencies from Monte Carlo
!> @date  12/12/12  MDG 3.1 test to do an actual numerical integration for the I_jk integrals, using MC profiles
!> @date  08/01/13  MDG 4.0 complete rewrite with Lambert format for MC output and new ctemlib.a routines 
!>                          also, the earlier versions would do only one energy value, whereas this new 
!>                          implementation does the complete energy-dependent master pattern
!> @date  01/27/14  MDG 4.1 continued rewrite, fixed problem with kvector list, replaced gvector routines
!>		   	     with updated routines; changed program name to EMEBSDmaster.
!> @date  05/03/14  MDG 4.2 test version to resolve bug in the Sgh matrix part (solved)
!> @date  06/19/14  MDG 4.3 rewrite, removal of all globals, split of namelist handling from computation; add OpenMP
!> @date  09/09/14  MDG 5.0 forked from EMEBSDmaster to EMKosselmaster program
!> @date  02/14/15  MDG 5.1 added Kosselmode option for thickness fraction plot
!> @date  11/09/15  MDG 6.0 added HDF5 support and converted to more recent symmetry routines
!> @date  05/21/16  MDG 6.1 changes for HDF internal file reorganization
!--------------------------------------------------------------------------
program EMKosselmaster

use local
use NameListTypedefs
use NameListHandlers
use files
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(KosselMasterNameListType)          :: kmnl

nmldeffile = 'EMKosselmaster.nml'
progname = 'EMKosselmaster.f90'
progdesc = 'Kossel Master Pattern Simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 0, 14 /), progname)

! deal with the namelist stuff
call GetKosselMasterNameList(nmldeffile,kmnl)

! generate a series of master Kossel patterns
 call ComputeKosselMasterPattern(kmnl, progname, nmldeffile)

end program EMKosselmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeKosselMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an EBSD master pattern as a function of energy
!
!> @param nmlfile namelist file name
!
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 08/01/13  MDG 3.0 complete rewrite, eliminated old Lambert projection
!> @date 09/25/13  MDG 3.1 replaced k-vector code by kvectors module
!> @date 06/19/14  MDG 4.0 no more globals, nml split, added OpenMP
!> @date 09/09/14  MDG 5.0 new version for Kossel master pattern
!> @date 11/09/15  MDG 6.0 added HDF5 support and converted to more recent symmetry routines
!--------------------------------------------------------------------------
subroutine ComputeKosselMasterPattern(kmnl, progname, nmldeffile)

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
use stringconstants

IMPLICIT NONE

type(KosselMasterNameListType),INTENT(IN) :: kmnl
character(fnlen),INTENT(IN)               :: progname
character(fnlen),INTENT(IN)               :: nmldeffile

real(kind=dbl)                  :: ctmp(192,3), arg
integer(HSIZE_T)                :: dims3(3), cnt3(3), offset3(3)
integer(HSIZE_T)                :: dims2(2), cnt2(2), offset2(2)
integer(kind=irg)               :: isym,i,j,ik,npy,ipx,ipy,ipz,debug,izz, izzmax, iequiv(3,48), nequiv, num_el, MCnthreads, & ! counters
                                  SamplingType,numk,numthick,  & ! number of independent incident beam directions
                                  ir,kk(3), npyhex, skip, ijmax, one, NUMTHREADS, TID, hdferr, tickstart, &
                                  n,ix,iy, io_int(6), nns, nnw, nref, nix, niy, nixp, niyp, ierr, &
                                  istat,gzero,ic,ip,ikk, totstrong, totweak     ! counters
real(kind=dbl)                  :: tpi,Znsq, kkl, DBWF, kin, xy(2), dc(3), edge, scl, tmp, dx, dxm, dy, dym !!
real(kind=sgl)                  :: io_real(5), selE, kn, FN(3), kkk(3), bp(4), tstop
complex(kind=dbl)               :: czero
real(kind=sgl),allocatable      :: mLPNH(:,:,:), mLPSH(:,:,:), Iz(:), thick(:), trange(:,:)
real(kind=sgl),allocatable      :: auxNH(:,:,:), auxSH(:,:,:), auxtrange(:,:)
logical                         :: usehex, switchmirror, verbose, insert=.TRUE., overwrite=.TRUE., silent=.TRUE.
character(11)           :: dstr
character(15)           :: tstrb
character(15)           :: tstre
character(fnlen, KIND=c_char)           :: stringarray(1)
character(fnlen,kind=c_char)            :: line2(1)
character(fnlen)                :: oldprogname, groupname, energyfile, outname

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

tpi = 2.D0*cPi
czero = complex (0.D0,0.D0)

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(tickstart)

!=============================================
!=============================================
! crystallography section

!nullify(cell)        
!allocate(cell)        

 verbose = .TRUE.
 call Initialize_Cell(cell,Dyn,rlp,kmnl%xtalname, kmnl%dmin, kmnl%voltage, verbose)

! the following line needs to be verified ... 

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
! this is where we determine the values for the thicknesses 
if (kmnl%Kosselmode.eq.'thicks') then
  numthick = 1
else 
  numthick = kmnl%numthick
end if

allocate(thick(numthick),Iz(numthick))

do i=1,numthick
  thick(i) = kmnl%startthick + float(i-1)*kmnl%thickinc
end do
!=============================================
!=============================================


!=============================================
!=============================================
! ---------- a couple of initializations
   npy = kmnl%npx
   gzero = 1  ! index of incident beam
! ----------
!=============================================
!=============================================

!=============================================
!=============================================
! ---------- allocate memory for the master pattern
! we need to sample the stereographic projection Northern hemisphere or a portion
! thereoff, depending on the current symmetry.
if (kmnl%Kosselmode.eq.'normal') then 
  allocate(mLPNH(-kmnl%npx:kmnl%npx,-npy:npy,1:kmnl%numthick),stat=istat)
  allocate(mLPSH(-kmnl%npx:kmnl%npx,-npy:npy,1:kmnl%numthick),stat=istat)

! set various arrays to zero
   mLPNH = 0.0
   mLPSH = 0.0
else
! Kosselmode must be 'thicks'
  allocate(trange(-kmnl%npx:kmnl%npx,-npy:npy),stat=istat)
  trange = 0.0
end if
! ---------- end allocate memory for the master pattern
!=============================================
!=============================================

! force dynamical matrix routine to read new Bethe parameters from file
! this will all be changed with the new version of the Bethe potentials
 call Set_Bethe_Parameters(BetheParameters,silent)

!=============================================
! create the HDF5 output file
!=============================================

  nullify(HDF_head%next)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

  outname = trim(EMsoft_getEMdatapathname())//trim(kmnl%outname)
  outname = EMsoft_toNativePath(outname)

! Create a new file using the default properties.
  hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
groupname = SC_Kosselmaster
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! add the CrystalData group at the top level of the file
  call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_Kosselmasterlist
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)
  call HDFwriteKosselMasterNameList(HDF_head, kmnl)
  call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)
  
! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

groupname = SC_Kosselmaster
  hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_xtalname
  stringarray(1)= trim(kmnl%xtalname)
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

dataset = SC_npx
  hdferr = HDF_writeDatasetInteger(dataset, kmnl%npx, HDF_head)

dataset = SC_BetheParameters
  bp = (/ BetheParameters%c1, BetheParameters%c2, BetheParameters%c3, BetheParameters%sgdbdiff /)
  hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head)

dataset = SC_numthick
  hdferr = HDF_writeDatasetInteger(dataset, numthick, HDF_head)

dataset = SC_startthick
  hdferr = HDF_writeDatasetFloat(dataset, kmnl%startthick, HDF_head)

dataset = SC_thickinc
  hdferr = HDF_writeDatasetFloat(dataset, kmnl%thickinc, HDF_head)

dataset = SC_Kosselmode
  stringarray(1)= trim(kmnl%Kosselmode)
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! create the hyperslabs and write zeroes to them for now
  if (kmnl%Kosselmode.eq.'normal') then
dataset = SC_mLPNH
    dims3 = (/  2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    cnt3 = (/ 2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    offset3 = (/ 0, 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)

dataset = SC_mLPSH
    dims3 = (/  2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    cnt3 = (/ 2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    offset3 = (/ 0, 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head)
  else
dataset = SC_trange
    dims2 = (/  2*kmnl%npx+1, 2*kmnl%npx+1 /)
    cnt2 = (/ 2*kmnl%npx+1, 2*kmnl%npx+1 /)
    offset2 = (/ 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, trange, dims2, offset2, cnt2(1), cnt2(2), HDF_head)
  end if

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)

!=============================================
!=============================================
! print a message to indicate we're starting the computation
call Message('Starting computation', frm = "(/A)")

!=============================================
! ---------- create the incident beam directions list
! determine all independent incident beam directions (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation;
! note that this needs to be redone for each energy, since the wave vector changes with energy
   nullify(khead)
   if (usehex) then
    call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,kmnl%npx,npy,numk, &
                SamplingType,ijmax,'RoscaLambert',usehex)
   else 
! Calckvectors(k,ga,ktmax,npx,npy,numk,isym,ijmax,mapmode,usehex)
    call Calckvectors(khead,cell, (/ 0.D0, 0.D0, 1.D0 /), (/ 0.D0, 0.D0, 0.D0 /),0.D0,kmnl%npx,npy,numk, &
                SamplingType,ijmax,'RoscaLambert',usehex)
   end if
   io_int(1)=numk
   call WriteValue('# independent beam directions to be considered = ', io_int, 1, "(I8)")

! convert the kvector linked list into arrays for OpenMP
  allocate(karray(4,numk), kij(2,numk),stat=istat)
! point to the first beam direction
  ktmp => khead
! and loop through the list, keeping k, kn, and i,j
  karray(1:3,1) = sngl(ktmp%k(1:3))
  karray(4,1) = sngl(ktmp%kn)
  kij(1:2,1) = (/ ktmp%i, ktmp%j /)
  do ik=2,numk
    ktmp => ktmp%next
    karray(1:3,ik) = sngl(ktmp%k(1:3))
    karray(4,ik) = sngl(ktmp%kn)
    kij(1:2,ik) = (/ ktmp%i, ktmp%j /)
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
  call OMP_SET_NUM_THREADS(kmnl%nthreads)
  io_int(1) = kmnl%nthreads
  call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

! use OpenMP to run on multiple cores ... 
!$OMP PARALLEL COPYIN(rlp) &
!$OMP& PRIVATE(DynMat,ik,FN,TID,kn,ipx,ipy,ix,iequiv,nequiv,reflist,firstw) &
!$OMP& PRIVATE(kkk,nns,nnw,nref,io_int,Iz)

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
     call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, kkk, kmnl%dmin, nref, verbose)
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

! solve the dynamical eigenvalue equation for this beam direction
     kn = karray(4,ik)
     if (kmnl%Kosselmode.eq.'thicks') then 
       call CalcKthick(DynMat,kn,nns,kmnl%tfraction,Iz)
     else
       call CalcKint(DynMat,kn,nns,numthick,thick,Iz)
     end if
     deallocate(DynMat)

! and store the resulting svals values, applying point group symmetry where needed.
     ipx = kij(1,ik)
     ipy = kij(2,ik)
     ipz = kij(3,ik)
!
!$OMP CRITICAL
     if (usehex) then 
       call Apply3DPGSymmetry(cell,ipx,ipy,ipz,kmnl%npx,iequiv,nequiv,usehex)
     else
       if ((cell%SYM_SGnum.ge.195).and.(cell%SYM_SGnum.le.230)) then
         call Apply3DPGSymmetry(cell,ipx,ipy,ipz,kmnl%npx,iequiv,nequiv,cubictype=SamplingType)
       else
         call Apply3DPGSymmetry(cell,ipx,ipy,ipz,kmnl%npx,iequiv,nequiv)
       end if
     end if
     if (kmnl%Kosselmode.eq.'normal') then
      do ix=1,nequiv
       if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix),1:numthick) = Iz(1:numthick)
       if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix),1:numthick) = Iz(1:numthick)
      end do
     else
      do ix=1,nequiv
        trange(iequiv(1,ix),iequiv(2,ix)) = Iz(1)
      end do
     end if
!$OMP END CRITICAL
     
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
! for all Kossel pattern interpolations;  we need to do this for both the Northern and Southern hemispheres

! we begin by allocating auxiliary arrays to hold copies of the hexagonal data; the original arrays will
! then be overwritten with the newly interpolated data.
   if (kmnl%Kosselmode.eq.'normal') then
    allocate(auxNH(-kmnl%npx:kmnl%npx,-npy:npy,1:kmnl%numthick),stat=istat)
    allocate(auxSH(-kmnl%npx:kmnl%npx,-npy:npy,1:kmnl%numthick),stat=istat)
    auxNH = mLPNH
    auxSH = mLPSH
   else
    allocate(auxtrange(-kmnl%npx:kmnl%npx,-npy:npy),stat=istat)
    auxtrange = trange
   end if 
! 
   edge = 1.D0 / dble(kmnl%npx)
   scl = float(kmnl%npx) 
   do i=-kmnl%npx,kmnl%npx
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
          if (nixp.gt.kmnl%npx) nixp = nix
          if (niyp.gt.kmnl%npx) niyp = niy
          dx = xy(1) - nix
          dy = xy(2) - niy
          dxm = 1.D0 - dx
          dym = 1.D0 - dy
          if (kmnl%Kosselmode.eq.'normal') then
           mLPNH(i,j,1:numthick) = auxNH(nix,niy,1:numthick)*dxm*dym + auxNH(nixp,niy,1:numthick)*dx*dym + &
                               auxNH(nix,niyp,1:numthick)*dxm*dy + auxNH(nixp,niyp,1:numthick)*dx*dy
           mLPSH(i,j,1:numthick) = auxSH(nix,niy,1:numthick)*dxm*dym + auxSH(nixp,niy,1:numthick)*dx*dym + &
                               auxSH(nix,niyp,1:numthick)*dxm*dy + auxSH(nixp,niyp,1:numthick)*dx*dy
          else 
           trange(i,j) = auxtrange(nix,niy)*dxm*dym + auxtrange(nixp,niy)*dx*dym + &
                               auxtrange(nix,niyp)*dxm*dy + auxtrange(nixp,niyp)*dx*dy
          end if
        end if
      end do
    end do
end if

! and here is where the major changes are for version 5.0: all output now in HDF5 format
  call timestamp(datestring=dstr, timestring=tstre)

  nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
  call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
  hdferr =  HDF_openFile(outname, HDF_head)

! update the time string
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_Kosselmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  tstop = Time_tock(tickstart)
dataset = SC_Duration
  hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_Kosselmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

! add data to the hyperslab
  if (kmnl%Kosselmode.eq.'normal') then
dataset = SC_mLPNH
    dims3 = (/  2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    cnt3 = (/ 2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    offset3 = (/ 0, 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)

dataset = SC_mLPSH
    dims3 = (/  2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    cnt3 = (/ 2*kmnl%npx+1, 2*kmnl%npx+1, numthick /)
    offset3 = (/ 0, 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, mLPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, insert)
  else
dataset = SC_trange
    dims2 = (/  2*kmnl%npx+1, 2*kmnl%npx+1 /)
    cnt2 = (/ 2*kmnl%npx+1, 2*kmnl%npx+1 /)
    offset2 = (/ 0, 0 /)
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, trange, dims2, offset2, cnt2(1), cnt2(2), HDF_head, insert)
  end if

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

  call Message('Final data stored in file '//trim(kmnl%outname), frm = "(A/)")

end subroutine ComputeKosselMasterPattern
