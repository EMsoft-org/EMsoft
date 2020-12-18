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
! EMsoft:EMISEmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMISEmaster
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMISEmaster computes a master pattern for ion-induced secondary electrons
!
!> @note This is based on the iCHORD model for ballistic ion channeling
!
!> @date  12/18/20  MDG 1.0 initial program, based on iCHORD forward model for ISE
!--------------------------------------------------------------------------
program EMISEmaster

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
type(ISEMasterNameListType)             :: emnl
integer(kind=irg)                       :: res, error_cnt, hdferr

nmldeffile = 'EMISEmaster.nml'
progname = 'EMISEmaster.f90'
progdesc = 'Ion-Induced Secondary Electron Master Pattern Simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 93 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  ! call JSONreadEBSDmasterNameList(emnl, nmldeffile, error_cnt)
  call Message('JSON input not implemented at this time.')
else
  call GetISEMasterNameList(nmldeffile,emnl)
end if

! generate a master ISE pattern
call ComputeMasterPattern(emnl, progname, nmldeffile)

end program EMISEmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeMasterPattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute an ISE master pattern 
!
!> @param emnl namelist 
!> @param progname program name
!> @param nmldeffile namelist file name (so that the entire file can be stored inside the HDF5 file)
!
!> @date 12/18/20  MDG 1.0 original
!--------------------------------------------------------------------------
subroutine ComputeMasterPattern(emnl, progname, nmldeffile)

use typedefs
use NameListTypedefs
use initializersHDF
use initializers
use ISEmod
use symmetry
use crystal
use constants
use error
use kvectors
use io
use math
use local
use files
use timing
use Lambert
use HDF5
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use omp_lib
use notifications
use stringconstants
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

type(ISEMasterNameListType),INTENT(INOUT)  :: emnl
character(fnlen),INTENT(IN)                :: progname
character(fnlen),INTENT(IN)                :: nmldeffile

real(kind=dbl)          :: ctmp(192,3), arg, Radius, xyz(3)
integer(HSIZE_T)        :: dims2(2), cnt2(2), offset2(2)
integer(kind=irg)       :: isym,i,j,ik,npy,ipx,ipy,ipz,debug,iE,izz, izzmax, iequiv(3,48), nequiv, num_el, MCnthreads, & ! counters
                           numk, timestart, timestop, numsites, nthreads, & ! number of independent incident beam directions
                           ir,nat(maxpasym),kk(3), skip, ijmax, one, NUMTHREADS, TID, SamplingType, &
                           numset,n,ix,iy,iz, io_int(6), maxlat(1), numcells(3), ncells, atomcnt, ii, TIFF_nx, TIFF_ny, &
                           istat,gzero,ic,ip,ikk, totstrong, totweak, jh, ierr, nix, niy, nixp, niyp     ! counters
real(kind=dbl)          :: tpi,Znsq, kkl, DBWF, kin, delta, h, lambda, omtl, srt, dc(3), xy(2), edge, scl, tmp, dx, dxm, dy, dym !
real(kind=sgl)          :: io_real(5), kkk(3), tstop, rsphere, r2, kvec(3), ISEinten, sxyz(3), ma, mi
real(kind=sgl),allocatable      :: auxNH(:,:), auxSH(:,:), atomlist(:,:), atomrad(:)  ! results
real(kind=sgl),allocatable      :: mLPNH(:,:), mLPSH(:,:), masterSPNH(:,:), masterSPSH(:,:)
logical                 :: usehex, switchmirror, verbose
character(fnlen)        :: xtalname

integer(kind=irg)       :: hdferr, nlines
character(fnlen)        :: oldprogname, groupname, energyfile, outname, datagroupname, attributename, HDF_FileVersion, &
                           fname, TIFF_filename 
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
type(kvectorlist),pointer       :: khead, ktmp
real(kind=sgl),allocatable      :: karray(:,:)
integer(kind=irg),allocatable   :: kij(:,:)
character(fnlen)                :: dataset, instring

type(HDFobjectStackType)        :: HDF_head

character(fnlen),ALLOCATABLE    :: MessageLines(:)
integer(kind=irg)               :: NumLines, info
character(fnlen)                :: SlackUsername, exectime
character(100)                  :: c

! declare variables for use in object oriented image module
integer                         :: iostat
character(len=128)              :: iomsg
logical                         :: isInteger
type(image_t)                   :: im
integer(int8)                   :: i8 (3,4)
integer(int8), allocatable      :: TIFF_image(:,:)

!$OMP THREADPRIVATE(rlp) 

stereog = .TRUE.

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(timestart)

tpi = 2.D0*cPi

!=============================================
outname = trim(EMsoft_getEMdatapathname())//trim(emnl%outname)
outname = EMsoft_toNativePath(outname)

!=============================================
!=============================================
! crystallography section; 
verbose = .TRUE.
call Initialize_Cell(cell,Dyn,rlp,emnl%xtalname, 0.05, 30.0, verbose)

! check the crystal system and setting; abort the program for trigonal with rhombohedral setting with
! an explanation for the user

if ((cell%xtal_system.eq.5).and.(cell%b.eq.cell%c)) then 
    call Message('')
    call Message(' ========Program Aborted========')
    call Message(' The ISE master pattern simulation for rhombohedral/trigonal structures')
    call Message(' requires that the structure be described using the hexagonal reference')
    call Message(' frame.  Please re-enter the crystal structure in this setting.')
    call Message('')
    stop
end if

! determine the point group number
 j=0
 do i=1,32
  if (SGPG(i).le.cell%SYM_SGnum) j=i
 end do
 isym = j

! Here, we encode isym into a new number that describes the sampling scheme; the new schemes are 
! described in detail in the EBSD manual pdf file.  We use them here to generate a set of unique
! beam directions which we then convert to rotation matrices to rotate the set of atoms into the 
! correct orientation for the ballistic channeling projection algorithm.

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

!=============================================
! create the HDF5 output file
!=============================================

  nullify(HDF_head%next)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
  hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
  datagroupname = 'ISEmaster'
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_ISEmasterNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteISEMasterNameList(HDF_head, emnl)

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

! create the hyperslabs and write zeroes to them for now
dataset = SC_mLPNH
  dims2 = (/  2*emnl%npx+1, 2*emnl%npx+1 /)
  cnt2 = (/ 2*emnl%npx+1, 2*emnl%npx+1 /)
  offset2 = (/ 0, 0 /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPNH, dims2, offset2, cnt2, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPNH, dims2, offset2, cnt2, HDF_head)
  end if

dataset = SC_mLPSH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPSH, dims2, offset2, cnt2, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPSH, dims2, offset2, cnt2, HDF_head)
  end if

dataset = SC_masterSPNH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, masterSPNH, dims2, offset2, cnt2, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, masterSPNH, dims2, offset2, cnt2, HDF_head)
  end if

dataset = SC_masterSPSH
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, masterSPSH, dims2, offset2, cnt2, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray2D(dataset, masterSPSH, dims2, offset2, cnt2, HDF_head)
  end if

! =====================================================
! end of HDF_FileVersion = 4.0 write statements
! =====================================================

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)

!=============================================
!=============================================
! ---------- from here on, we need to repeat the entire computation for each energy value
! so this is where we could in principle implement an OpenMP approach; alternatively, 
! we could do the inner loop over the incident beam directions in OpenMP (probably simpler)

call Time_tick(timestart)

! print a message to indicate where we are in the computation
call Message(' -> Initializing incident beam direction list')

!=============================================
! ---------- create the incident beam directions list
! determine all independent incident beam directions (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation;  each beam direction
! will then be converted into the rotation matrix that takes the z-axis into the beam direction.
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
  allocate(karray(3,numk), kij(3,numk),stat=istat)
! point to the first beam direction
  ktmp => khead
! and loop through the list, keeping a normalized k and i,j
  karray(1:3,1) = sngl(ktmp%k(1:3))
  kij(1:3,1) = (/ ktmp%i, ktmp%j, ktmp%hs /)
   do ik=2,numk
     ktmp => ktmp%next
     kvec = sngl(ktmp%k(1:3))
     kvec = kvec / sqrt(sum(kvec*kvec))
     karray(1:3,ik) = kvec
     kij(1:3,ik) = (/ ktmp%i, ktmp%j, ktmp%hs /)
   end do
! and remove the linked list
  call Delete_kvectorlist(khead)

  verbose = .FALSE.
  totstrong = 0
  totweak = 0

! ---------- end of "create the incident beam directions list"
!=============================================

!=============================================
!=============================================
! generate the crystal lattice for the spherical ISE projection 
! we need to generate enough atoms to fill a sphere with radius
! three times the largest lattice parameter, and then keep only
! those atoms (so count them first and then allocate the coordinate array)
rsphere = 3.0 * maxval( (/ cell%a, cell%b, cell%c /) )
r2 = rsphere*rsphere
maxlat = maxloc( (/ cell%a, cell%b, cell%c /) )
if (maxlat(1).eq.1) then 
  numcells(1) = 3 + 1
  numcells(2) = nint( cell%a/cell%b ) * 3 + 1
  numcells(3) = nint( cell%a/cell%c ) * 3 + 1
end if
if (maxlat(1).eq.2) then 
  numcells(1) = nint( cell%b/cell%a ) * 3 + 1
  numcells(2) = 3 + 1
  numcells(3) = nint( cell%b/cell%c ) * 3 + 1
end if
if (maxlat(1).eq.3) then 
  numcells(1) = nint( cell%c/cell%a ) * 3 + 1
  numcells(2) = nint( cell%c/cell%b ) * 3 + 1
  numcells(3) = 3 + 1
end if
call CalcPositions(cell, 'm', numcells)
ncells = (2*numcells(1)+1)*(2*numcells(2)+1)*(2*numcells(3)+1)

! count how many atoms there are inside the sphere with radius rsphere 
atomcnt = 0
do i=1,cell%ATOM_ntype
  do j=1,ncells * cell%SG%SYM_MATnum
    if (sum(cell%apos(i,j,1:3)**2).le.r2) atomcnt = atomcnt + 1
  end do 
end do 

! and copy them to a new list, converted to Angstrom units 
allocate(atomlist(3,atomcnt), atomrad(atomcnt))
atomcnt = 0
do i=1,cell%ATOM_ntype
  do j=1,ncells * cell%SG%SYM_MATnum
    if (sum(cell%apos(i,j,1:3)**2).le.r2) then 
      atomcnt = atomcnt + 1
      atomlist(1:3,atomcnt) = cell%apos(i,j,1:3) * 10.0
      atomrad(atomcnt) = ATOM_MTradii( cell%ATOM_type(i) ) * 10.0 * emnl%iscale(3)
    end if 
  end do 
end do 
deallocate(cell%apos)
rsphere = rsphere * 10.0   ! the ISE algorithm expects Angstrom units ...

!=============================================
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
!$OMP PARALLEL PRIVATE(ik,TID,ipx,ipy,ix,iequiv,nequiv,io_int,ISEinten)

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC,100)    
! ---------- and here we start the beam direction loop
   beamloop:do ik = 1,numk

     ISEinten = getISEintensity(karray(1:3,ik), atomcnt, atomlist, atomrad, rsphere, emnl%iscale(1), emnl%iscale(2))

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
       if (iequiv(3,ix).eq.-1) mLPSH(iequiv(1,ix),iequiv(2,ix)) = ISEinten
       if (iequiv(3,ix).eq.1) mLPNH(iequiv(1,ix),iequiv(2,ix)) = ISEinten
     end do
!$OMP END CRITICAL
  
     if (mod(ik,5000).eq.0) then
       io_int(1) = ik
       io_int(2) = numk
       call WriteValue('  completed beam direction ',io_int, 2, "(I8,' of ',I8)")
     end if

    end do beamloop
  
! end of OpenMP portion
!$OMP END PARALLEL

  deallocate(karray, kij)

! we need to normalize the master pattern arrays to a max value of 1
  r2 = maxval( (/ maxval(mLPSH), maxval(mLPNH) /) )
  mLPNH = mLPNH / r2
  mLPSH = mLPSH / r2

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
          mLPNH(i,j) = auxNH(nix,niy)*dxm*dym + auxNH(nixp,niy)*dx*dym + auxNH(nix,niyp)*dxm*dy + auxNH(nixp,niyp)*dx*dy
          mLPSH(i,j) = auxSH(nix,niy)*dxm*dym + auxSH(nixp,niy)*dx*dym + auxSH(nix,niyp)*dxm*dy + auxSH(nixp,niyp)*dx*dy
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
      if (ierr.ne.0) then 
        masterSPNH(i,j) = 0.0
        masterSPSH(i,j) = 0.0
      else
        sxyz = sngl(xyz)
        masterSPNH(i,j) = InterpolateLambert(sxyz, mLPNH, emnl%npx)
        masterSPSH(i,j) = InterpolateLambert(sxyz, mLPSH, emnl%npx)
      end if
    end do
  end do

! and here is where the major changes are for version 5.0: all output now in HDF5 format
  call timestamp(datestring=dstr, timestring=tstre)

  datagroupname = SC_ISEmaster

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

! add data to the hyperslab
dataset = SC_mLPNH
  hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPNH, dims2, offset2, cnt2, HDF_head, insert)

dataset = SC_mLPSH
  hdferr = HDF_writeHyperslabFloatArray2D(dataset, mLPSH, dims2, offset2, cnt2, HDF_head, insert)

dataset = SC_masterSPNH
  hdferr = HDF_writeHyperslabFloatArray2D(dataset, masterSPNH, dims2, offset2, cnt2, HDF_head, insert)

dataset = SC_masterSPSH
  hdferr = HDF_writeHyperslabFloatArray2D(dataset, masterSPSH, dims2, offset2, cnt2, HDF_head, insert)

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

  call Message('Final data stored in file '//trim(emnl%outname), frm = "(A/)")

! if requested, we notify the user that this program has completed its run
if (trim(EMsoft_getNotify()).ne.'Off') then
  if (trim(emnl%Notify).eq.'On') then 
    NumLines = 3
    allocate(MessageLines(NumLines))

    call hostnm(c)
 
    MessageLines(1) = 'EMISEmaster program has ended successfully'
    MessageLines(2) = 'Master pattern data stored in '//trim(outname)
    write (exectime,"(F10.4)") tstop  
    MessageLines(3) = 'Total execution time [s]: '//trim(exectime)
    SlackUsername = 'EMsoft on '//trim(c)
    i = PostMessage(MessageLines, NumLines, SlackUsername)
  end if
end if


! output the ADP map as a tiff file 
fname = trim(EMsoft_getEMdatapathname())//trim(emnl%tiffname)
fname = EMsoft_toNativePath(fname)
TIFF_filename = trim(fname)
TIFF_nx = 2*emnl%npx+1 
TIFF_ny = 2*emnl%npx+1 

! allocate memory for image
allocate(TIFF_image(TIFF_nx,TIFF_ny))

! fill the image with whatever data you have (between 0 and 255)
ma = maxval(masterSPNH)
mi = minval(masterSPNH)

TIFF_image = int(255 * (masterSPNH-mi)/(ma-mi))

! set up the image_t structure
im = image_t(TIFF_image)
if(im%empty()) call Message("ComputeMasterPattern","failed to convert array to image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message("failed to write image to file : "//iomsg)
else  
  call Message('ISE masterSPNH map written to '//trim(TIFF_filename))
end if 
deallocate(TIFF_image)

end subroutine ComputeMasterPattern
