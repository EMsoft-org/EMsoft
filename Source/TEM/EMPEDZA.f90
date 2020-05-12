! ###################################################################
! Copyright (c) 2015-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMPEDZA.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMPEDZA 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Zone axis Precession Electron Diffraction
!
!> @details This program is only useful for the exact zone axis orientations, but 
!> it illustrates how a dynamical PED pattern can be computed.  To simulate PEDs
!> for an arbitrary indicent beam orientation, us the EMPED program
!
!> @date 11/29/01 MDG 1.0 original
!> @date 04/08/13 MDG 2.0 rewrite
!> @date 05/08/13 MDG 2.1 forked from mbcbed and adapted for large angle CBED patterns
!> @date 05/14/13 MDG 2.2 replaced all IO by namelist file and added command line argument handling
!> @date 09/04/13 MDG 2.3 all command line argument handling now via files.f90 routine
!> @date 03/05/14 MDG 3.0 new version to deal with precession electron diffraction
!> @date 09/07/14 MDG 4.0 removal of all global variables; new TypeDef and NamelistHandler; OpenMP version
!> @date 11/18/15 MDG 4.1 restricted use of program to zone axis orientations only; split off EMPED
!> @date 05/21/16 MDG 4.2 change to HDF internal file organization
!--------------------------------------------------------------------------
program EMPEDZA


use local
use NameListTypedefs
use NameListHandlers
use files
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(PEDZANameListType)                 :: pednl

nmldeffile = 'EMPEDZA.nml'
progname = 'EMPEDZA.f90'
progdesc = 'Zone Axis Dynamical Precession Electron Diffraction Pattern Simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 13 /), progname)

! deal with the namelist stuff
call GetPEDZANameList(nmldeffile,pednl)

! generate a set of master EBSD patterns
 call PEDZAPattern(pednl,progname, nmldeffile)

end program EMPEDZA

!--------------------------------------------------------------------------
!
! SUBROUTINE:PEDZApattern
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a dynamical zone axis precession electron diffraction pattern
!
!> @param nmlfile namelist file name
!
!> @date 03/05/14 MDG 1.0 original, based on EMlacbed program
!> @date 09/07/14 MDG 2.0 complete rewrite using new EMSoftLib library
!> @date 11/17/15 MDG 3.0 another rewrite, this time with HDF5, testing etc...
!--------------------------------------------------------------------------
subroutine PEDZApattern(pednl,progname,nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use initializersHDF
use initializers
use constants
use crystal
use diffraction
use gvectors
use kvectors
use MBmodule
use postscript, ONLY: GetIndex
use symmetry
use math
use io
use error
use files
use HDF5
use HDFsupport
use ISO_C_BINDING
use omp_lib
use stringconstants
use timing

IMPLICIT NONE

type(PEDZANameListType),INTENT(INOUT):: pednl
character(fnlen),INTENT(IN)     :: progname
character(fnlen),INTENT(IN)     :: nmldeffile

real(kind=sgl)                  :: ktmax, io_real(3), bragg, thetac, sc, minten, pxy(2), galen, DM(2,2),DD,X(2),tstart,tstop, &
                                   frac, startthick, thick(1), klaue(2), thetam, kk(3), goffset,FN(3), kn, pmult
integer(kind=irg)               :: ijmax,ga(3),gb(3),cnt, PX, numthick, ss, icnt, pgnum, ih, nunique, famnum, tickstart, &
                                   newcount,count_rate,count_max, io_int(6), i, j, isym, ir, skip, ghkl(3),hdferr, &
                                   npx, npy, numt, numk, ik, ip, jp, istat, dgn, nbeams, refcnt, gzero, TID, &
                                   ifamily, famhkl(3), inum, maxHOLZ, numksame, nns, nnw, nref, nt, NUMTHREADS
character(3)                    :: method
character(fnlen)                :: datafile, groupname, dataset

real(kind=sgl),allocatable      :: disk(:,:),intarray(:),positions(:,:)
integer(kind=irg),allocatable   :: familymult(:), familyhkl(:,:), whichHOLZ(:), gequiv(:,:), hklarray(:,:)
real(kind=sgl),allocatable      :: inten(:,:)
real(kind=dbl)                  :: s(3)
logical                         :: verbose, silent, first, overwrite
logical,allocatable             :: ksame(:)
character(11)                   :: dstr
character(15)                   :: tstrb
character(15)                   :: tstre
character(fnlen,kind=c_char)    :: line2(1)
real(kind=sgl),allocatable      :: xx(:,:),yy(:,:),line(:),dot(:,:),pedpattern(:,:)
character(len=1),allocatable    :: ped(:,:),pedpat(:,:)
real(kind=sgl)                  :: rnmpp,Igmax,xc,yc,dx,dy,k(3),kp(3),sg,Ig,ma,mi
integer(kind=irg)               :: ww,tdp,sx,sy,nsize
integer(HSIZE_T)                :: dims2(2)

type(unitcell)                  :: cell
type(DynType),save              :: Dyn
type(gnode),save                :: rlp
type(reflisttype),pointer       :: reflist,firstw, rltmpa
type(BetheParameterType)        :: BetheParameters
type(kvectorlist),pointer       :: khead, ktmp
real(kind=sgl),allocatable      :: karray(:,:)
integer(kind=irg),allocatable   :: kij(:,:)
complex(kind=dbl),allocatable   :: DynMat(:,:)
type(HDFobjectStackType)        :: HDF_head

interface
recursive subroutine CalckvectorsPrecession(khead,cell,k,ga,precangle,prechalfwidth,precsample,precazimuthal,numk)

use local
use typedefs
use io
use error
use constants
use diffraction
use crystal
use Lambert
use kvectors

IMPLICIT NONE

type(kvectorlist),pointer,INTENT(INOUT) :: khead
type(unitcell)        ,INTENT(IN)       :: cell
real(kind=dbl),INTENT(IN)               :: k(3)         !< initial wave vector
real(kind=dbl),INTENT(IN)               :: ga(3)        !< "horizontal" reciprocal lattice vector
real(kind=sgl),INTENT(IN)               :: precangle    !< precession angle in [mrad]
real(kind=sgl),INTENT(IN)               :: prechalfwidth        !< halfwidth of tilted beam [mrad]
integer(kind=irg),INTENT(IN)            :: precsample           !< number of kvectors along beam tilt
integer(kind=irg),INTENT(IN)            :: precazimuthal        !< number of kvectors along circumference
integer(kind=irg),INTENT(OUT)           :: numk         !< total number of kvectors in linked list
end subroutine CalckvectorsPrecession
end interface

!=============================================
!=============================================
! crystallography section
!nullify(cell)        
!allocate(cell)        

verbose = .TRUE.
overwrite = .TRUE.
silent = .TRUE.
call Initialize_Cell(cell,Dyn,rlp,pednl%xtalname, pednl%dmin, pednl%voltage, verbose)

! set the foil normal 
 Dyn%FN = float(pednl%fn)
 call NormVec(cell, Dyn%FN, 'd')
 numt = 1
 goffset = 0.2
 thick(1) = pednl%thickness
 
! determine the point group number
 j=0
 do i=1,32
  if (SGPG(i).le.cell % SYM_SGnum) j=i
 end do

! use the new routine to get the whole pattern 2D symmetry group, since that
! is the one that determines the independent beam directions.
 dgn = GetPatternSymmetry(cell,pednl%k,j,.TRUE.)
 pgnum = j
 isym = WPPG(dgn) ! WPPG lists the whole pattern point group numbers vs. diffraction group numbers

! determine the shortest reciprocal lattice points for this zone
 call ShortestG(cell,pednl%k,ga,gb,isym)
 io_int(1:3)=ga(1:3)
 io_int(4:6)=gb(1:3)
 call WriteValue(' Reciprocal lattice vectors : ', io_int, 6,"('(',3I3,') and (',3I3,')',/)")

!=============================================
!=============================================
! determine the list of contributing wave vectors
  call CalckvectorsPrecession(khead,cell,dble(pednl%k),dble(ga),pednl%precangle,pednl%prechalfwidth,pednl%precsample,&
        pednl%precazimuthal,numk)
!=============================================
!=============================================

!=============================================
!=============================================
! set the scale parameter for a default camera length of 1000 mm.
  sc = cell%mLambda * 1000.0 * 300.0 / 25.4  ! the absolute value does not matter and is derived from legacy Postscript code
! The original code used 300 dpi (hence 300/25.4) which was convenient for Postscript output; in the current case, we
! do not actually use the true value, but in the IDL visualization program, we scale the user defined camera length by
! 1000.0, and use this ratio to scale the diskoffset coordinates.  So, there's no absolute length scale, only a relative scale.

! next we create the output array (not sure yet how to do this)
  allocate(disk(-pednl%npix:pednl%npix,-pednl%npix:pednl%npix))
  disk=0.0

! time the computation
  cnt = 0
  call system_clock(cnt,count_rate,count_max)


!======HDF5: generate output file=============
!=============================================
! Initialize FORTRAN interface.
!
  CALL h5open_EMsoft(hdferr)

  call timestamp(datestring=dstr, timestring=tstrb)
  tstre = tstrb
  call Time_tick(tickstart)

! Create a new file using the default properties.
  datafile = trim(EMsoft_getEMdatapathname())//trim(pednl%outname)
  datafile = EMsoft_toNativePath(datafile)
  hdferr =  HDF_createFile(datafile, HDF_head)

! write the EMheader to the file
groupname = SC_PEDZA
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EMPEDZA
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

  call HDF_pop(HDF_head)

! create a NMLparameters group to write all the namelist entries into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwritePEDZANameList(HDF_head, pednl)
      
! and leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

! we need to write the image dimensions
dataset = SC_numk
  hdferr = HDF_writeDatasetInteger(dataset, numk, HDF_head) 

dataset = SC_ga
  hdferr = HDF_writeDatasetIntegerArray1D(dataset, ga, 3, HDF_head) 

dataset = SC_gb
  hdferr = HDF_writeDatasetIntegerArray1D(dataset, gb, 3, HDF_head) 

dataset = SC_lenga
  hdferr = HDF_writeDatasetFloat(dataset, CalcLength(cell,float(ga),'r'), HDF_head) 

dataset = SC_lengb
  hdferr = HDF_writeDatasetFloat(dataset, CalcLength(cell,float(gb),'r'), HDF_head) 

dataset = SC_angab
  hdferr = HDF_writeDatasetFloat(dataset, CalcAngle(cell,float(ga),float(gb),'r'), HDF_head) 

! we leave the file open until the program is done.
!=============================================
!=============================================

  DM(1,1) = CalcDot(cell, float(gb),float(gb),'c')
  DM(1,2) = -CalcDot(cell, float(ga),float(gb),'c')
  DM(2,1) = DM(1,2)
  DM(2,2) = CalcDot(cell, float(ga),float(ga),'c')
  DD = 1.0/(DM(1,1)*DM(2,2) - DM(1,2)*DM(2,1))

! first convert the linked wave vector list to a set of regular arrays, so that we can 
! use OpenMP for this computation
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

! read the Bethe Potential parameters
  call Set_Bethe_Parameters(BetheParameters,silent)

! the following call establishes the linked list of reflections and initializes their xg
! parameters to zero; these will then be filled in for the individual incident beam orientations
  kk(1:3) = float(pednl%k)
  FN = kk
  call Initialize_ReflectionList_EwaldSweep(cell, reflist, FN, kk, nref, pednl%precangle, goffset,verbose)

! set the xg-field of this linked list to zero so that we can start accumulating intensities
   rltmpa => reflist%next
   do icnt=1,nref
     rltmpa%xg = 0.0
     rltmpa=>rltmpa%next
   end do

! set up OpenMP parameters
  io_int(1)=numk
  call WriteValue('Starting computation for # beam directions = ', io_int, 1, "(I8)")
  gzero = 1
  nt = 1
  
  verbose = .TRUE.

! set the number of OpenMP threads 
  call OMP_SET_NUM_THREADS(pednl%nthreads)
  io_int(1) = pednl%nthreads
  call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

! use OpenMP to run on multiple cores ... 
!!$OMP PARALLEL default(shared) PRIVATE(

! set the foil normal 

  NUMTHREADS = OMP_GET_NUM_THREADS()
  TID = OMP_GET_THREAD_NUM()

  nullify(firstw)

  first = .TRUE.

! loop over all beam orientations 
kvectorloop:  do ik = 1,numk
! generate the reflectionlist; this actually calls for a special routine, since the traditional ones
! are separate for the list generation and the subsequent application of the Bethe potentials; in the
! present case, we need to use an existing reflist, and in one pass determine the BethePotentials
! for a particular incident beam direction... we use a new routine called GetSubReflist() from gvectors module
   kk(1:3) = karray(1:3,ik)
   FN = kk
   call GetSubReflist(cell, reflist, BetheParameters, FN, kk, nref, firstw, nns, nnw, first)
   if (ik.eq.1) first = .FALSE.

! generate the dynamical matrix
   allocate(DynMat(nns,nns))
   call GetDynMat(cell, reflist, firstw, rlp, DynMat, nns, nnw)

! allocate the intensity array to include both strong beams and weak beams (in that order)
   allocate(inten(numt,nns))
   inten = 0.0
 
! solve the dynamical eigenvalue equation for this beam direction
   kn = karray(4,ik)
   call CalcPEDint(DynMat,cell,kn,nns,nt,thick,inten)

! now we need to write these intensities to the appropriate slots in the reflist linked list... 
   rltmpa => reflist%next
   do icnt=1,nns
     rltmpa%xg = rltmpa%xg + inten(1,icnt)
     rltmpa=>rltmpa%nexts
   end do


! and remove the intensity array
   deallocate(DynMat)
   deallocate(inten)

! remove all the computed intensities for per pattern storage    
   if (pednl%filemode.eq.'eachp') then

! first we count how many reflections have none-zero intensity  
     refcnt = 0
     rltmpa => reflist%next
     do i=1,nref
       if (rltmpa%xg.ne.0.D0) refcnt = refcnt + 1
       rltmpa => rltmpa%next
     end do

! write refcnt
     write (dataunit) refcnt
     rltmpa => reflist%next
     do i=1,nref
       if (rltmpa%xg.ne.0.D0) then
! decompose this point w.r.t ga and gb
         X(1) = CalcDot(cell,float(rltmpa%hkl),float(ga),'c')
         X(2) = CalcDot(cell,float(rltmpa%hkl),float(gb),'c')
         X = matmul(DM,X) * DD
         write (dataunit) rltmpa%hkl
         write (dataunit) rltmpa%xg
         write (dataunit) X
       end if
       rltmpa => rltmpa%next
     end do
  
! and reset the intensities for the next run
     rltmpa => reflist%next
     do i=1,nref
       rltmpa%xg = 0.D0
       rltmpa => rltmpa%next
     end do
   end if

! update computation progress
   if (float(ik)/float(numk) .gt. frac) then
    io_int(1) = nint(100.0*frac) 
    call WriteValue('       ', io_int, 1, "(1x,I3,' percent completed')") 
    frac = frac + 0.05
   end if  

! reset the strong reflection pointers to null
   rltmpa => reflist%next
   do i=1,nref
     rltmpa%strong = .FALSE.
     rltmpa%weak = .FALSE.
     nullify(rltmpa%nextw)
     nullify(rltmpa%nexts)
     rltmpa=>rltmpa%next
   end do
   nullify(firstw)

  end do kvectorloop


! write the intensities as individual arrays to the HDF5 file
if (pednl%filemode.eq.'total') then

! first we count how many reflections have none-zero intensity  
  refcnt = 0
  rltmpa => reflist%next
  do i=1,nref
    if (rltmpa%xg.ne.0.D0) refcnt = refcnt + 1
    rltmpa => rltmpa%next
  end do

! write refcnt
dataset = SC_refcnt
  hdferr = HDF_writeDatasetInteger(dataset, refcnt, HDF_head) 

! allocate arrays for output
  allocate(hklarray(3,refcnt), intarray(refcnt), positions(2,refcnt))

  rltmpa => reflist%next
  icnt = 1
  do i=1,nref
    if (rltmpa%xg.ne.0.D0) then
! decompose this point w.r.t ga and gb
      X(1) = CalcDot(cell,float(rltmpa%hkl),float(ga),'c')
      X(2) = CalcDot(cell,float(rltmpa%hkl),float(gb),'c')
      X = matmul(DM,X) * DD
      hklarray(1:3,icnt) = rltmpa%hkl
      intarray(icnt) = rltmpa%xg
      positions(1:2,icnt) = X
      icnt = icnt+1
    end if
    rltmpa => rltmpa%next
  end do

! and write these arrays to the HDF5 file
dataset = SC_hklarray
  hdferr = HDF_writeDatasetIntegerArray2D(dataset, hklarray, 3, refcnt, HDF_head) 

dataset = SC_intarray
  hdferr = HDF_writeDatasetFloatArray1D(dataset, intarray, refcnt, HDF_head) 

dataset = SC_positions
  hdferr = HDF_writeDatasetFloatArray2D(dataset, positions, 2, refcnt, HDF_head) 

!=============================================
!=============================================
! create the coordinate arrays for the Gaussian peaks that will represent the diffraction spots
  rnmpp = 1.0/0.075
  ww = 6
  tdp = 2*ww+1
  allocate(xx(-ww:ww,-ww:ww), yy(-ww:ww,-ww:ww), line(-ww:ww), dot(-ww:ww,-ww:ww))
  line = (/ (float(i),i=-ww,ww) /) * rnmpp
  xx = spread(line,dim=1,ncopies=2*ww+1)
  yy = transpose(xx)

!=============================================
!=============================================
! create the output array
  nsize = pednl%npix/2 + ww 
  allocate(pedpattern(-nsize:nsize,-nsize:nsize))
  Igmax = maxval(intarray(2:refcnt))
  write (*,*) ' Maximum diffracted intensity : ',Igmax
  allocate(ped(1:pednl%npix,1:pednl%npix),pedpat(-nsize:nsize,-nsize:nsize))

  k(1:3) = float(pednl%k)/cell%mLambda

  do i = 2,refcnt
    sg = calcsg(cell,float(hklarray(1:3,i)),k,FN)
    kp = k + float(hklarray(1:3,i)) + sg

    xc = rnmpp * kp(1)
    yc = rnmpp * kp(2)

! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
    if ((abs(xc).le.nsize-ww).and.(abs(yc).le.nsize-ww)) then
      sx = nint(xc)
      sy = nint(yc)
      dx = xc-sx
      dy = yc-sy
      Ig = intarray(i)
      dot = (Ig/Igmax)**0.2 * exp(-((xx-dx)**2+(yy-dy)**2)*0.0005)
      pedpattern(sx-ww:sx+ww,sy-ww:sy+ww) = pedpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
    end if
  end do

  ma = maxval(pedpattern)
  mi = minval(pedpattern)
  pedpattern = ((pedpattern - mi)/ (ma-mi))
  pedpat = char(nint(255.0*pedpattern))
  ped = ' '

! save the pedpattern to the ped array
  ped(1:pednl%npix,1:pednl%npix) = pedpat(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)

dataset = SC_pedpattern
  dims2 =  (/ pednl%npix, pednl%npix /)
  hdferr = HDF_writeDatasetCharArray2D(dataset, ped, dims2, HDF_head) 

  call HDF_pop(HDF_head)

! and update the end time
  call timestamp(datestring=dstr, timestring=tstre)
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_PEDZA
  hdferr = HDF_openGroup(groupname, HDF_head)

! stop time /EMheader/StopTime 'character'
dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  tstop = Time_tock(tickstart)
dataset = SC_Duration
  hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

! close the datafile
  call HDF_pop(HDF_head,.TRUE.)

! close the Fortran interface
  call h5close_EMsoft(hdferr)

  call Message(' Data stored in '//pednl%outname,"(/A/)") 


end if
  

end subroutine PEDZApattern




!--------------------------------------------------------------------------
!
! SUBROUTINE: CalckvectorsPrecession
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a linked list of wave vectors for precession electron diffraction
!
!> @details This is a new version to test whether or not we can use the whole pattern
!> symmetry to determine the relevant list of incident wave vectors; this should be a 
!> general routine, so that we do not need to consider each symmetry case separately.
!> This will require a floating point version of the Apply2DPGSymmetry routine in symmetry.f90.
!
!> @param khead wave vector list pointer
!> @param cell unit cell pointer
!> @param k central wave vector
!> @param ga reciprocal lattice vector normal to k
!> @param precangle main precession angle [mrad]
!> @param prechalfwidth precession beam half width [mrad]
!> @param precsample number of samples in half width 
!> @param precazimuthal number of samples around each precession circle
!> @param numk total number of wave vectors in list
!
!> @date  03/05/14 MDG 1.0 original
!> @date  11/28/14 MDG 2.0 rewrite without global variables
!--------------------------------------------------------------------------
recursive subroutine CalckvectorsPrecession(khead,cell,k,ga,precangle,prechalfwidth,precsample,precazimuthal,numk)

use local
use typedefs
use io
use error
use constants
use diffraction
use crystal
use Lambert
use kvectors

IMPLICIT NONE

type(kvectorlist),pointer,INTENT(INOUT) :: khead
type(unitcell)        ,INTENT(IN)       :: cell
real(kind=dbl),INTENT(IN)               :: k(3)         !< initial wave vector
real(kind=dbl),INTENT(IN)               :: ga(3)        !< "horizontal" reciprocal lattice vector
real(kind=sgl),INTENT(IN)               :: precangle    !< precession angle in [mrad]
real(kind=sgl),INTENT(IN)               :: prechalfwidth        !< halfwidth of tilted beam [mrad]
integer(kind=irg),INTENT(IN)            :: precsample           !< number of kvectors along beam tilt
integer(kind=irg),INTENT(IN)            :: precazimuthal        !< number of kvectors along circumference
integer(kind=irg),INTENT(OUT)           :: numk         !< total number of kvectors in linked list

type(kvectorlist),pointer               :: ktail
integer(kind=irg)                       :: istat,i,j, iequiv(2,12), nequiv, jj, nx, ny, il, ith
real(kind=dbl)                          :: gp, dgp, glen, gan(3), gperp(3), kstar(3), dth
real(kind=dbl),allocatable              :: gw(:), ct(:), st(:), th(:)
logical                                 :: hexgrid = .FALSE.
real(kind=sgl)                          :: kt(3),kr(3)
real(kind=sgl)                          :: ktlen


! compute geometrical factors 
 glen = CalcLength(cell,ga,'r')                 ! length of ga
 gan = ga/glen                                  ! normalized ga
 gp = 2.0*sin(precangle/1000.0)/cell%mLambda         ! precession angle converted to reciprocal length gp in units of glen
 dgp = 0.0
 if (precsample.gt.0) then
   dgp = 2.0*sin(0.001*(precangle-prechalfwidth))/cell%mLambda/glen/float(precsample)        ! half width step size converted to reciprocal length dgp in units of glen
 end if
 allocate(gw(2*precsample+1))                           ! sampling radii
 gw = gp + dgp * (/ (i,i=-precsample,precsample) /)     ! sampling radii

! pre-compute cosines and sines
 allocate(ct(precazimuthal),st(precazimuthal), th(precazimuthal))
 dth = 2.D0*cPi / dble(precazimuthal)
 th = (/ (i-1,i=1,precazimuthal) /) * dth
 ct = cos(th)
 st = sin(th)
 
 call TransSpace(cell,k,kstar,'d','r')               ! transform incident direction to reciprocal space
 call CalcCross(cell,ga,kstar,gperp,'r','r',0)       ! compute g_perp = ga x k
 call NormVec(cell,gperp,'r')                        ! normalize g_perp
 call NormVec(cell,kstar,'r')                        ! normalize reciprocal beam vector

! allocate the head and tail of the linked list
 allocate(khead,stat=istat)                             ! allocate new value
 if (istat.ne.0) call FatalError('CalckvectorsPrecession','unable to allocate khead pointer')
 ktail => khead                                         ! tail points to new value
 nullify(ktail%next)                                    ! nullify next in new value
 numk = 0                                               ! keep track of number of k-vectors so far

 
! next loop around each of the precession circles
 do il = 1,2*precsample+1                               ! number of concentric circles
  do ith = 1,precazimuthal                              ! number of points along each circle
! make a new one in the list, except for the first one
   if (numk.ne.0) then
     allocate(ktail%next,stat=istat)                    ! allocate new value
     if (istat.ne.0) call FatalError('Add_knode:',' unable to allocate pointer')
     ktail => ktail%next                        ! tail points to new value
     nullify(ktail%next)                                ! nullify next in new value
   end if
! and populate the fields   
   kt = - gw(il)*ct(ith)*gan - gw(il)*st(ith)*gperp     ! tangential component of k
   ktail%kt = kt                                        ! store tangential component of k
   ktlen = CalcLength(cell,kt,'r')**2                   ! squared length of tangential component

   kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar         ! complete wave vector
   ktail%k = kr                                         ! store in pointer list
   ktail%kn = CalcDot(cell,ktail%k,kstar,'r')           ! normal component of k
   numk = numk + 1
  end do
 end do


end subroutine CalckvectorsPrecession

