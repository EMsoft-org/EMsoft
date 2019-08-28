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
! EMsoft:EMLACBED.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMLACBED 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Zone axis CBED (Bloch wave approach), including OpenMP computations
!
!> @todo symmetry implementation
! 
!> @date 11/29/01 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 05/14/13 MDG 2.1 replaced all IO by namelist file and added command line argument handling
!> @date 09/25/13 MDG 2.2 improved command line argument handling
!> @date 09/26/13 MDG 2.3 corrected scaling error and added HOLZ
!> @date 11/22/18 MDG 3.0 complete rewrite and debug; program now runs correctly, apart from one workaround
!> @date 11/29/18 MDG 3.1 initial incorporation of LACBED-type output for IDL program visualization
!> @date 12/09/18 MDG 3.2 renamed program EMLACBED.f90 and made appropriate modifications
!--------------------------------------------------------------------------
program EMLACBED

use local
use files
use io
use NameListTypedefs
use NameListHandlers

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(LACBEDNameListType)                :: cbednl
integer(kind=irg)                       :: res

! deal with the command line arguments, if any
nmldeffile = 'EMLACBED.nml'
progname = 'EMLACBED.f90'
progdesc = 'Large angle convergent beam pattern simulation'
call EMsoft(progname, progdesc) 

call Interpret_Program_Arguments(nmldeffile,2,(/ 10, 0 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
        write (*,*) 'json input not yet supported'
        stop
  ! call JSONreadCBEDNameList(lacbednl, nmldeffile, error_cnt)
else
  call GetLACBEDNameList(nmldeffile,cbednl)
end if

! generate a zone axis LACBED pattern
call LACBEDcomputation(cbednl, progname, nmldeffile)

end program EMLACBED

!--------------------------------------------------------------------------
!
! SUBROUTINE:LACBEDcomputation
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute large area zone axis convergent beam electron diffraction pattern
!
!> @param nmlfile namelist input file
!
!> @todo implement symmetry
! 
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 09/25/13  MDG 2.2 minor program cleanup
!> @date 10/18/13  MDG 3.0 major modifications to bring program in line with EMlacbed program
!> @date 12/02/14  MDG 3.1 removed camlen as a global variable
!> @date 11/22/18  MDG 4.0 complete rewrite, mirroring the EMEBSDmaster program; tested on [111] Si.xtal Zuo inside cover CBED pattern
!> @date 12/13/18  MDG 4.1 fixed error in reflection ranking
!--------------------------------------------------------------------------
subroutine LACBEDcomputation(cbednl, progname, nmlfile)

use local
use typedefs
use files
use io
use crystal
use symmetry
use diffraction
use MBmodule
use error
use math
use HDFsupport
use HDF5
use NameListHDFWriters
use quaternions
use constants
use rotations
use initializers
use initializersHDF
use gvectors
use kvectors
use MBmodule
use omp_lib
use ISO_C_BINDING
! use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

type(LACBEDNameListType),INTENT(INOUT)    :: cbednl
character(fnlen),INTENT(IN)               :: progname
character(fnlen),INTENT(IN)               :: nmlfile

type(unitcell)                            :: cell
type(reflisttype),pointer                 :: reflist, firstw, nexts, rl, mainreflist, rltmpa, rltmpb
type(BetheParameterType)                  :: BetheParameters
type(gnode),save                          :: rlp
type(DynType),save                        :: Dyn
type(kvectorlist),pointer                 :: klist, ktmp
logical                                   :: verbose

integer(kind=irg)                         :: sLUT, i, ii, jj, ik, ithick, parity, hkl(6,23), Pmdims, h(6), gindex, io_int(6)
complex(kind=dbl)                         :: Ucg, qg, Ucg2, qg0
real(kind=dbl)                            :: Vmod, Vpmod, xig, xgp, sg
real(kind=dbl)                            :: lgpar, lgperp, st, nfact
integer(HSIZE_T)                          :: dims4(4), cnt4(4), offset4(4)

real(kind=sgl)                            :: FN(3), kk(3), dmin, kp(3), ku(3), io_real(3), qu(4), gx(3), cen, delta, gac(3), sc,&
                                             ktmax, rad, theta, thetacr, voltage, ma, mi, kkk(3), bragg, galen, minten, thetam
real(kind=dbl)                            :: epar(6,3), mLambda
integer(kind=irg)                         :: nref, nns, nnw, counter, numk, ierr, ga(3), gb(3), imax, imin, ir, istat, isym
real(kind=sgl),allocatable                :: klistarray(:,:), image(:,:), familytwotheta(:), diskoffset(:,:), &
                                             rfamilytwotheta(:), rdiskoffset(:,:)
integer(kind=irg),allocatable             :: kpix(:,:), hklarray(:,:), ranking(:), rranking(:), refctr(:)
integer(kind=irg),allocatable             :: familymult(:), familyhkl(:,:), whichHOLZ(:), gequiv(:,:), rfamilymult(:), &
                                             rfamilyhkl(:,:), rwhichHOLZ(:)
integer(kind=irg)                         :: itmp(48,3), famhkl(3), famnum, ghkl(3), numksame, nunique
logical,allocatable                       :: ksame(:)
real(kind=dbl)                            :: s(3)

complex(kind=dbl),allocatable             :: DynMat(:,:)
type(HOLZentries)                         :: HOLZdata

logical                                   :: f_exists, firstloop, insert=.TRUE.
real(kind=sgl),allocatable                :: intensity(:,:,:), thick(:), inten(:,:), slice(:,:,:,:)
integer(kind=irg)                         :: nthreads, TID, j, jmax, jmin, NUMTHREADS, it, TIFF_nx, TIFF_ny, dgn, icnt, ih, numir
real(kind=sgl),parameter                  :: DtoR = cPi/180.0
type(HDFobjectStackType),pointer          :: HDF_head
character(fnlen)                          :: dataset, instring, outname, groupname, fname, TIFF_filename, tmpfile
character(fnlen)                          :: mode, xtalname
integer(HSIZE_T)                          :: dims3(3), cnt3(3) 
integer(kind=irg)                         :: hdferr, d(3), refcnt
integer(kind=irg)                         :: tstart, tstop, clock_rate, nn, nt, numt, pgnum, ijmax, Tnref, ifamily
real(kind=sgl)                            :: exec_time, kc(3), pxy(2)
character(11)                             :: dstr
character(15)                             :: tstrb
character(15)                             :: tstre
character(2)                              :: str

! declare variables for use in object oriented image module
! integer                                   :: iostat
! character(len=128)                        :: iomsg
! logical                                   :: isInteger
! type(image_t)                             :: im
! integer(int8)                             :: i8 (3,4)
! integer(int8), allocatable                :: TIFF_image(:,:)


call timestamp(datestring=dstr, timestring=tstrb)
call system_clock(tstart, clock_rate)

!nullify(cell)        
allocate(cell)

voltage         = cbednl%voltage
minten          = cbednl%minten
dmin            = cbednl%dmin
xtalname        = trim(cbednl%xtalname)
nthreads        = cbednl%nthreads
!===========================================================================================
! CRYSTALLOGRAPHY
!===========================================================================================
verbose = .TRUE.
call Initialize_Cell(cell, Dyn, rlp, xtalname, dmin, voltage, verbose)

! determine the point group number
j=0
do i=1,32
 if (SGPG(i).le.cell % SYM_SGnum) j=i
end do

! use the new routine to get the whole pattern 2D symmetry group, since that
! is the one that determines the independent beam directions.
dgn = GetPatternSymmetry(cell,cbednl%k,j,.TRUE.)
pgnum = j
isym = WPPG(dgn) ! WPPG lists the whole pattern point group numbers vs. diffraction group numbers

! to do all this requires knowledge of the subset of 3D symmetry operators that keeps 
! the incident beam direction invariant, so we determine this subset first.  This 
! code comes from the CalcStar routine, but we only need a portion of it.
allocate(ksame(cell%SG%SYM_NUMpt))
ksame = .FALSE.
numksame = 1
ksame(1) = .TRUE.
  
! get all the symmetry operation IDs that leave the zone axis invariant (skip the identity operation)
do i=2,cell%SG%SYM_NUMpt 
  s = matmul(cell%SG%SYM_direc(i,1:3,1:3),dble(cbednl%k)) 
  if (sum(abs(s-dble(cbednl%k))).lt.1.0D-10) then 
    ksame(i) = .TRUE.
    numksame = numksame+1
  end if
end do

! and output the number of 3D symmetry operators that leave k invariant; they form
! the subset that we are after... This should really coincide with the 2D whole pattern symmetry group,
! but with 3D operators instead of 2D operators; so now we have the actual symmetry matrices.
io_int(1) = numksame
call WriteValue(' Number of 3D symmetry operators that leave k invariant : ',io_int, 1, "(I3)")
io_int(1) = PGTWDorder(WPPG(dgn))
call WriteValue(' Order of Whole Pattern point group : ',io_int, 1, "(I3,/)")
allocate(gequiv(numksame,3))


! determine the shortest reciprocal lattice points for this zone
call ShortestG(cell,cbednl%k,ga,gb,isym)
io_int(1:3)=ga(1:3)
io_int(4:6)=gb(1:3)
call WriteValue(' Reciprocal lattice vectors : ', io_int, 6,"('(',3I3,') and (',3I3,')')")
call Message('  (the first lattice vector is horizontal in the CBED pattern)')
call Message(' ')
galen = CalcLength(cell, float(ga), 'r')

! get number of thicknesses for which to compute the CBED disk images
numt = cbednl%numthick
allocate(thick(numt),stat=istat)
thick = cbednl%startthick + cbednl%thickinc* (/ (float(i),i=0,numt-1) /)

! set up the incident wave vector list
nullify(klist)
thetacr   = cbednl%convergence / 1000.0
bragg     = CalcDiffAngle(cell,ga(1),ga(2),ga(3)) * 0.5
ktmax     = thetacr / bragg
ijmax     = cbednl%npix**2
call Calckvectors(klist,cell, dble(cbednl%k), dble(ga), dble(ktmax), cbednl%npix, cbednl%npix, numk, &
                  0, ijmax, 'Conical', usehex=.FALSE.)

io_int(1) = numk
call WriteValue(' # independent beam directions to be considered = ', io_int, 1, "(I8)")

! copy some of these values into regular arrays for easier access in parallel section
allocate(klistarray(4,numk), kpix(2,numk))
klistarray = 0.0
kpix       = 0

ktmp => klist
do ii = 1,numk
  klistarray(1:3,ii)  =   ktmp%k(1:3)
  klistarray(4,ii)    =   ktmp%kn
  kpix(1:2,ii)        =   (/ktmp%i, ktmp%j/)
  ktmp                =>  ktmp%next
end do

! get rid of the linked list of k-vectors
call Delete_kvectorlist(klist)

!===========================================================================================
! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters,silent=.TRUE.)

nullify(mainreflist)
kkk = klistarray(1:3,1)
FN = kkk


! some parameters required for simulation
qg0 = cmplx(cPi,0.0) * cell%LUTqg(0,0,0)
qg0 = cmplx(0.D0,aimag(qg0))

!====================================
! in this computation, we are going to stick to the same set of reflections, and for each 
! incident beam direction we will update the excitation errors, apply the Bethe potentials,
! and then compute the intensities; the sequence of nref reflections is thus fixed for the 
! entire computation, and weak reflections are handled with the Bethe potentials when 
! appropriate.

call Initialize_ReflectionList(cell, mainreflist, BetheParameters, FN, kkk, cbednl%dmin, nref, verbose)
allocate(hklarray(3,nref))
hklarray = 0

nexts => mainreflist%next
do ii = 1,nref
  hklarray(1:3,ii)  =   nexts%hkl
  nexts             =>  nexts%next 
end do

!======================================
! allocate intensity array
allocate(intensity(nref,numk,numt))
intensity  = 0.0

!======================================
! for some unclear reason (Heisenbug...) this program produces incorrect results unless there is 
! a write statement immediately following the GetDynMat routine.  We'll write some stuff to a temp file
! and then delete the file afterwards until we can figure out what the underlying issue is [MDG, 11/28/18] 
tmpfile= trim(EMsoft_getEMtmppathname())//'tmpoutput.txt'
tmpfile= EMsoft_toNativePath(tmpfile)
open(unit=600,file=trim(tmpfile),status='unknown',form='formatted')
!======================================

!======================================
! initialize the OpenMP parallel processing environment
call OMP_SET_NUM_THREADS(cbednl%nthreads)
io_int(1) = cbednl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

!$OMP  PARALLEL DEFAULT(PRIVATE) SHARED(numk, klistarray, FN, kkk, verbose) &
!$OMP& SHARED(cbednl, intensity, cell, BetheParameters, thick, numt, ik)

NUMTHREADS = OMP_GET_NUM_THREADS()
TID = OMP_GET_THREAD_NUM()
firstloop = .TRUE.

! perform the main loop over the incident beam directions in parallel
!$OMP DO SCHEDULE(DYNAMIC)
do ik = 1,numk
! local wave vector
  kk  = klistarray(1:3,ik)

! we will have each thread do its own reflist initialization to allow for the lists to be used
! in combination with the Bethe potential approach; this needs to be done only once for each thread
  if (firstloop.eqv..TRUE.) then 
    nullify(reflist)
    call Initialize_ReflectionList(cell, reflist, BetheParameters, FN, kkk, cbednl%dmin, Tnref, verbose=.FALSE.)
    firstloop = .FALSE.
  end if

! go through the reflist and reset all strong and weak links to null pointers and .FALSE. identifiers;
! at the same time, compute the excitation errors for this incident beam direction kk
  rl => reflist%next
  do
    if (.not.associated(rl)) EXIT
    nullify(rl%nexts, rl%nextw)
    rl%strong = .FALSE.
    rl%weak = .FALSE.
    rl%sg = Calcsg(cell,float(rl%hkl(1:3)),kk,FN)
    rl => rl%next
  end do

! determine strong and weak reflections
  nullify(firstw)
  nns = 0
  nnw = 0
  call Apply_BethePotentials(cell, reflist, firstw, BetheParameters, Tnref, nns, nnw)

! generate the dynamical matrix
  allocate(DynMat(nns,nns))
  call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)

!========================
! this line is here as a workaround for some unidentified bug in the GetDynMat routine ...
! without this write, the remainder of this loop occasionally produces NaN results
  if (mod(ik,1000) .eq. 0) write (600,"(4I10,F12.4)") ik, TID, nns, nnw, maxval(abs(DynMat))
!========================

! solve the dynamical eigenvalue equation for this beam direction  
  allocate(inten(numt,nns))
  call CalcCBEDint(DynMat,cell,klistarray(4,ik),BetheParameters,nns,numt,thick,inten)

! and copy the intensities into the correct slot for the strong reflections only ...
  rl => reflist%next
  jj = 1
  intensity(1,ik,1:numt) = inten(1:numt,jj)    ! first reflection is always BF 
  rl => rl%next
  do ii = 2, Tnref
    if (rl%strong.eqv..TRUE.) then
      jj = jj+1
      intensity(ii,ik,1:numt) = inten(1:numt,jj)
    end if
    rl => rl%next
  end do

! every now and then, print a progress update
  if(mod(ik,5000) .eq. 0) then
    io_real(1) = 100.D0 * dble(ik)/dble(numk)
    call WriteValue('  completed ',io_real,1,'(F10.2,"% of beam directions")')
  end if
  deallocate(DynMat, inten)
end do
!$OMP END DO
!$OMP END PARALLEL

call timestamp(datestring=dstr, timestring=tstre)

!===============================
! get rid of the bug workaround file
close(unit=600,status='delete')
!===============================


!===============================
!===============================
! next we need to compute a number of arrays needed by the visualization routine as well as the 
! EMCBEDpattern program.  The arrays have to do with reflector identifications...
!
! We need to go through the reflection list and identify for each 
! reflection the multiplicity with respect to the family generated by the
! 2D whole pattern point group [see manual for IDL visualization program
! for an explicit example].  In other words, we need to tag each reflection
! that will need to end up in the final output file for the LACBED program.
! For instance, for the [112] Copper zone axis orientation, (1 1 -1) and
! (-1 -1 1) do not belong to the same family (since they lie on the mirror
! plane that makes up the 2D Whole Pattern group m.  Therefore, these must 
! be tagged as separate families with appropriate multiplicities, otherwise
! the visualization program will only know of one reflection (1 1 -1) and
! will not be able to generate the other one (-1 -1 1) {which need not be 
! identical to begin with, due to the m symmetry].
! At the same time we need to determine how many independent families there 
! are, as well as their multiplicities.
 
! set the scale parameter for a default camera length of 1000 mm.
  sc = cell%mLambda * 1000.0 * 300.0 / 25.4  ! the absolute value does not matter and is derived from legacy Postscript code
! The original code used 300 dpi (hence 300/25.4) which was convenient for Postscript output; in the current case, we
! do not actually use the true value, but in the IDL visualization program, we scale the user defined camera length by
! 1000.0, and use this ratio to scale the diskoffset coordinates.  So, there's no absolute length scale, only a relative scale.



! to be safe, we need to reset the family number for each of the reflections in the current list
! to zero, to reflect the fact that we haven't considered that reflection yet in terms of 2D symmetry.
  rltmpa => mainreflist%next
  do while (associated(rltmpa))
    rltmpa%famnum = 0
    if (.not.associated(rltmpa%next)) EXIT
    rltmpa => rltmpa%next
  end do

! next we go through the entire list again, this time with a double loop, and we
! determine for each hkl all the 2D equivalent ones and tag them as belonging to
! the same family in terms of 2D symmetry.  
  rltmpa => mainreflist%next
  rltmpa%famnum = 1             ! reflection at origin is always its own family
  ifamily = 1
  rltmpa => rltmpa%next
whileloop1: do while (associated(rltmpa))
! only look at points that haven't been tagged yet
   if (rltmpa%famnum.eq.0) then
    ghkl = rltmpa%hkl           ! get the reflection
    call Calc2DFamily(cell,ghkl,ksame,numksame,nunique,itmp)
! we add this point to a new famnum value
    ifamily = ifamily + 1
    rltmpa%famnum = ifamily
    rltmpa%famhkl = ghkl
    famhkl = ghkl
    if (nunique.gt.1) then
! the order is larger than 1, so we need to go through the list and tag all the equivalent ones;
! we need only consider those that have the same original famhkl.
      do i=2,nunique
        rltmpb => rltmpa%next
whileloop2: do while (associated(rltmpb))
! look for this reflection on the list
          if (sum(abs(itmp(i,1:3) - rltmpb%hkl(1:3))).eq.0) then  ! found it!
           rltmpb%famnum = ifamily
           rltmpb%famhkl = rltmpa%famhkl
           EXIT whileloop2
          end if
          if (.not.associated(rltmpb%next)) EXIT whileloop2
          rltmpb => rltmpb%next
        end do whileloop2
      end do
    end if
   end if
   if (.not.associated(rltmpa%next)) EXIT whileloop1
   rltmpa => rltmpa%next
  end do whileloop1

! ok, so there are ifamily families; next we need to store the corresponding
! hkl, and multiplicity, as well as the diffraction angle and the position of 
! the diffraction disk center for a standard camera length.
  allocate(familyhkl(3,ifamily), familymult(ifamily), familytwotheta(ifamily), diskoffset(2,ifamily), ranking(ifamily))

! redo the above loop, sort of, but now fill in the actual data
! we no longer need to keep the famnum entries in the linked list, so we
! can reset those to zero to keep track of points already visited.
  familyhkl(1:3,1) = (/ 0, 0, 0 /)
  familymult(1) = 1
  diskoffset(1:2,1) = (/ 0.0, 0.0 /)
  familytwotheta(1) = 0.0
  ranking(1) = 1
  rltmpa => mainreflist%next%next
  ifamily = 1
  
! for some of the 2D point groups, the standard orientation of the group according to ITC vol A
! may not be the orientation that we have here, so we need to determine by how much the 2D point
! group is rotated (CCW) with respect to the standard setting...
  call CheckPatternSymmetry(cell,cbednl%k,ga,isym,thetam)

! initialize the HOLZ geometry type
  call GetHOLZGeometry(cell,HOLZdata,float(ga),float(gb),cbednl%k,cbednl%fn) 

! keep track of the reflection identifier in ranking
refcnt = 1
outerloop2: do while (associated(rltmpa))
  refcnt = refcnt+1
  if (rltmpa%famnum.ne.0) then 
    ifamily = ifamily+1
    famnum = rltmpa%famnum
    rltmpa%famnum = 0
    famhkl = rltmpa%famhkl
    familyhkl(1:3,ifamily) = famhkl(1:3)
    familytwotheta(ifamily) = CalcDiffAngle(cell,famhkl(1),famhkl(2),famhkl(3))*1000.0
    familymult(ifamily) = 1
    ranking(ifamily) = refcnt
! get the disk offset parameters
    pxy = sc * GetHOLZcoordinates(cell,HOLZdata,float(famhkl), (/ 0.0, 0.0, 0.0 /), sngl(cell%mLambda))
    diskoffset(1:2,ifamily) = pxy
  
! and remove the equivalent reflections from the list
    rltmpb => rltmpa%next
whileloop3: do while (associated(rltmpb))
      if (rltmpb%famnum.eq.famnum) then
         familymult(ifamily) = familymult(ifamily) + 1
         rltmpb%famnum = 0
      end if
      if (.not.associated(rltmpb%next)) EXIT whileloop3
      rltmpb => rltmpb%next
    end do whileloop3
   end if
   if (.not.associated(rltmpa%next)) EXIT outerloop2
   rltmpa => rltmpa%next
  end do outerloop2

  io_int(1) = ifamily
  call WriteValue(' Maximum number of unique families in output = ', io_int, 1, "(I5)")

!===============================
!===============================
! before we write the intensities, we need to determine which reflection families 
! need to be written to the file; this is determined by the maxHOLZ parameter.  So,
! first we determine to which HOLZ layer each family belongs by using the zone 
! equation.   [check special case of hexagonal indices !!!!]
! Along the way, we count the ones up to order maxHOLZ.
! Also, to reduce the size of the output file a bit, we ignore those reflections that
! have a maximum intensity less than minten for the initial thickness value. On the
! other hand, if a reflection is due to double diffraction, then we include it, always.
  allocate(whichHOLZ(ifamily))
  icnt = 0
  do ir=1,ifamily
    whichHOLZ(ir) = iabs(cbednl%k(1)*familyhkl(1,ir)+cbednl%k(2)*familyhkl(2,ir)+cbednl%k(3)*familyhkl(3,ir))
    if (whichHOLZ(ir).le.cbednl%maxHOLZ) then 
      if ((maxval(intensity(ranking(ir),:,:)).ge.minten).or.(cell%dbdiff(familyhkl(1,ir),familyhkl(2,ir),familyhkl(3,ir)))) then
        icnt = icnt+1
      else  ! just change the HOLZ value to some large value to make sure it does not get written to the file
        whichHOLZ(ir) = 100
      end if
    end if
  end do    
  io_int(1) = icnt
  call WriteValue(' Actual number of unique families in output = ', io_int, 1, "(I5)")

! ok, so the whichHOLZ array tells us which reflections need to be included, but they are likely not in the 
! same order as in the familyhkl array, so we have some reordering to do... this can be done by defining new 
! familyhkl etc arrays that will then be written to the file with the correct reflection ordering.
numir = icnt
allocate(rfamilyhkl(3,numir), rfamilymult(numir), rfamilytwotheta(numir), rdiskoffset(2,numir), rwhichHOLZ(numir))
allocate(rranking(numir))

! fill the new arrays with the correctly ranked data
! first the central reflection
rfamilyhkl(1:3,1) = familyhkl(1:3,1)
rfamilymult(1) = familymult(1)
rfamilytwotheta(1) = familytwotheta(1)
rdiskoffset(1:2,1) = diskoffset(1:2,1)
rwhichHOLZ(1) = whichHOLZ(1)
rranking(1) = ranking(1)
icnt = 1
! then the others
do ih = 0,cbednl%maxHOLZ
 do ir = ifamily,2,-1
  if (whichHOLZ(ir).eq.ih) then
    icnt = icnt +1 
    rfamilyhkl(1:3,icnt) = familyhkl(1:3,ir)
    rfamilymult(icnt) = familymult(ir)
    rfamilytwotheta(icnt) = familytwotheta(ir)
    rdiskoffset(1:2,icnt) = diskoffset(1:2,ir)
    rwhichHOLZ(icnt) = whichHOLZ(ir)
    rranking(icnt) = ranking(ir)
  end if
 end do 
end do 

! then report a final count of the number of reflections in the output for each HOLZ layer
allocate(refctr(0:cbednl%maxHOLZ))
refctr = 0
do ir = 1,numir
  refctr(rwhichHOLZ(ir)) =  refctr(rwhichHOLZ(ir)) + 1
end do

call Message(' ')
do ir=0,cbednl%maxHOLZ
  io_int(1) = ir
  io_int(2) = refctr(ir)
  call WriteValue('  Number of unique reflections in Laue Zone ',io_int, 2,"(I3,' : ', I4)") 
end do
call Message(' ')

!===============================
! HDF5 I/O
! write out the data to the file
nullify(HDF_head)

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
outname = trim(EMsoft_getEMdatapathname())//trim(cbednl%outname)
outname = EMsoft_toNativePath(outname)

inquire(file=trim(outname), exist=f_exists)
! if file already exists, then delete it
if (f_exists) then
  open(unit=dataunit,file=trim(outname),&
      status='unknown',form='unformatted',access='direct',recl=1,iostat=ierr)
  close(unit=dataunit,status='delete')
end if

hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
groupname = 'LACBED'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_CBEDNameList
hdferr = HDF_writeDatasetTextFile(dataset, nmlfile, HDF_head)

! leave this group
call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteLACBEDNameList(HDF_head, cbednl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
! [this is the list of variables needed for the IDL visualization program CBEDDisplay.pro and the EMCBEDpattern.f90 program]
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

!========================================
! the following parameters are already part of either the header group or the namelist group ... 
! progname, EMsoftversion, npix, numt, xtalname, voltage, convergence, k, fn, dmin, maxHOLZ,
! startthick, thickinc

! to be written: icnt, numk, ga, galen, minten, pgnum, PGLaue(pgnum), dgn, PDG(dgn), BFPG(dgn), 
! WPPG(dgn), DFGN(dgn), DFSP(dgn), thetam, familyhkl, familymult, familytwotheta, 
! diskoffset, whichHOLZ, and all the diffraction disks, one per family... [using hyperslabs]
!========================================

! write integers 
dataset = 'numk'
hdferr = HDF_writeDatasetInteger(dataset, numk, HDF_head)

dataset = 'ifamily'
hdferr = HDF_writeDatasetInteger(dataset, ifamily, HDF_head)

dataset = 'ga'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ga, 3, HDF_head)

! integer arrays
dataset = 'diffgroup'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, (/ pgnum, PGLaue(pgnum), dgn, PDG(dgn), BFPG(dgn), &
                                        WPPG(dgn), DFGN(dgn), DFSP(dgn) /), 8, HDF_head)

dataset = SC_hkl
hdferr = HDF_writeDatasetIntegerArray2D(dataset, hklarray, 3, nref, HDF_head)

dataset = SC_PixelLocation
hdferr = HDF_writeDatasetIntegerArray2D(dataset, kpix, 2, numk, HDF_head)

dataset = 'familymult'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, rfamilymult, numir, HDF_head)

dataset = 'familyhkl'
hdferr = HDF_writeDatasetIntegerArray2D(dataset, rfamilyhkl, 3, numir, HDF_head)

! floats
dataset = 'galen'
hdferr = HDF_writeDatasetFloat(dataset, galen, HDF_head)

dataset = 'minten'
hdferr = HDF_writeDatasetFloat(dataset, minten, HDF_head)

dataset = 'thetam'
hdferr = HDF_writeDatasetFloat(dataset, thetam, HDF_head)

! float arrays
dataset = 'familytwotheta'
hdferr = HDF_writeDatasetFloatArray1D(dataset, rfamilytwotheta, numir, HDF_head)

dataset = 'diskoffset'
hdferr = HDF_writeDatasetFloatArray2D(dataset, rdiskoffset, 2, numir, HDF_head)

dataset = SC_klist  
hdferr = HDF_writeDatasetFloatArray2D(dataset, klistarray, 3, numk, HDF_head)

! before we write the intensities, we need to reorganize the array into a 4D array of diffraction disks 
! dataset = SC_Intensities
! hdferr = HDF_writeDatasetFloatArray3D(dataset, intensity, nref, numk, numt, HDF_head)


dataset = 'whichHOLZ'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, rwhichHOLZ, numir, HDF_head)


! The last data to be written is the set of diffraction disks, one for each family... 
! We also need to define the hyperslab parameters so that we can write each of the 
! diffraction disks for the complete thickness range; the disks array is a 4D array, but 
! we write 3D slabs into it
! create the hyperslabs and write zeroes to them for now
dataset = 'disks'
  allocate(slice(2*cbednl%npix+1, 2*cbednl%npix+1, numt, 1))
  slice = 0.0
  dims4 = (/  2*cbednl%npix+1, 2*cbednl%npix+1, numt, numir /)
  cnt4 = (/ 2*cbednl%npix+1, 2*cbednl%npix+1, numt, 1 /)
  offset4 = (/ 0, 0, 0, 0 /)
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, slice, dims4, offset4, cnt4(1), cnt4(2), cnt4(3), cnt4(4), HDF_head)

! Finally, write the correct diffraction disks to the hyperslab array
! Note that in the original EMLACBED program, the first subscript into the diffraction disks was reversed,
! so we will do this here as well... (before we do the offset to positive subscripts!)
  kpix(1,:) = -kpix(1,:)
  kpix = kpix + cbednl%npix + 1
  do ir = 1,numir
    do ik=1,numk
      slice(kpix(1,ik),kpix(2,ik),1:numt,1) = intensity(rranking(ir),ik,1:numt)
    end do 
    offset4 = (/ 0, 0, 0, ir-1 /)
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, slice, dims4, offset4, cnt4(1), cnt4(2), cnt4(3), cnt4(4), HDF_head, insert)
  end do

! write the icnt number to the file
dataset = 'icnt'
  hdferr = HDF_writeDatasetInteger(dataset, numir, HDF_head)

! leave this group and close the file
call HDF_pop(HDF_head,.TRUE.)

call Message(' Output data stored in '//trim(outname))
call Message(' ')

! print some output to the command line
call system_clock(tstop, clock_rate)
exec_time   = real(tstop - tstart)/real(clock_rate)
io_real(1)  = exec_time
call WriteValue('Execution time = ',io_real,1,'(F8.2,"sec")')






! !================= This needs to be moved to a different to-be-written program EMCBEDpatterns.f90
! ! for testing purposes we generate a series of tiff files for the central beam 
! allocate(image(2*cbednl%npix+1,2*cbednl%npix+1))

! ! output the ADP map as a tiff file 
! fname = trim(EMsoft_getEMdatapathname())//'playarea/CBED/BF_'
! fname = EMsoft_toNativePath(fname)

! ! allocate memory for image
! TIFF_nx = 2*cbednl%npix+1
! TIFF_ny = 2*cbednl%npix+1
! allocate(TIFF_image(TIFF_nx,TIFF_ny))

! do it=1,cbednl%numthick
!   write (str,"(I2.2)") it
!   TIFF_filename = trim(fname)//str//'.tiff'
!   image = 0.0
!   do ik=1,numk
! !   image(kpix(1,ik),kpix(2,ik)) = log(intensity(1,ik,it)+1.0)
!     image(kpix(1,ik),kpix(2,ik)) = intensity(1,ik,it)
!   end do 
! ! fill the image with whatever data you have (between 0 and 255)
!   ma = maxval(image)
!   mi = minval(image)
!   TIFF_image = int(255 * (image-mi)/(ma-mi))

! ! set up the image_t structure
!   im = image_t(TIFF_image)
!   if(im%empty()) call Message("EMgetADP","failed to convert array to image")

! ! create the file
!   call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
!   if(0.ne.iostat) then
!     call Message("failed to write image to file : "//iomsg)
!   else  
!     call Message('BF disk image written to '//trim(TIFF_filename))
!   end if 

! end do






 
end subroutine LACBEDcomputation

