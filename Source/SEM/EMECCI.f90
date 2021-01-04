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
! EMsoft:EMECCI.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMECCI
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief tryECCI computes ECCI defect contrast for multiple defects
!
!> @details This is a test version based on the EMZAdefect program
! 
!> @date  03/07/10 MDG  1.0 original, based on STEMdefect.f90 (collaboration with OSU) 
!> @date  03/21/10 MDG  2.0 modified for STEM illumination, fast version with bi-variate interpolation
!> @date  06/19/13 MDG  3.0 conversion to new libraries 
!> @date  10/28/13 MDG  3.1 added Interpret_Program_Arguments line
!> @date  12/03/13 MDG  4.0 new start 
!> @date  12/08/13 MDG  4.1 added trace (line scan) mode
!> @date  02/10/14 MDG  4.2 added apbs
!> @date  11/04/14 MDG  5.0 rewrite without globals; new namelist handling
!> @date  11/24/15 MDG  5.1 new defect handling implemented
!> @date  11/25/15 MDG  5.2 HDF5 output instead of custom binary file
!> @date  05/21/16 MDG  5.3 changes for new HDF internal file organization
!--------------------------------------------------------------------------
program EMECCI

use local
use files
use NameListTypedefs
use NameListHandlers
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                      :: nmldeffile, progname, progdesc
type(ECCINameListType)                :: eccinl

nmldeffile = 'EMECCI.nml'
progname = 'EMECCI.f90'
progdesc = 'Dynamical ECCI defect image simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,4,(/ 0, 3, 41, 200 /), progname)

! deal with the namelist stuff
call GetECCINameList(nmldeffile,eccinl)

! and call the main routine
call ComputeECCI(eccinl, progname, nmldeffile)

end program EMECCI


!--------------------------------------------------------------------------
!
! SUBROUTINE: ComputeECCI
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a zone axis ECCI defect data set
!
!> @param nmlfile namelist file name
!
!> @todo check sign of kt for consistency 
!
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 11/13/13  MDG 2.2 implementation of Pade approximation for scattering matrix computation
!> @date 12/04/13  MDG 3.0 new implementation of the thickness integration, nmore along the lines of EMECP 
!> @date 12/07/13  MDG 3.1 added line scan mode (called "trace")
!> @date 02/10/14  MDG 3.2 added apbs
!> @date 02/24/14  MDG 3.3 removal of double-counted phase factor
!> @date 03/05/14  MDG 3.4 correction of integration to double integration for Lgh array
!> @date 03/12/14  MDG 3.5 conversion of Sgh and Lgh arrays to diagonal only
!> @date 10/04/14  MDG 4.0 no globals conversion
!> @date 04/20/15  MDG 4.1 continued no-globals conversion and HDF output
!> @date 12/23/15  MDG 4.2 fixed incorrect initialization of nat array that resulted in negative intensities
!> @date 11/02/18  MDG 4.3 add montage output option in tiff, jpeg, or png format
!--------------------------------------------------------------------------
subroutine ComputeECCI(eccinl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use initializers
use initializersHDF
use error
use crystal
use symmetry
use postscript
use constants
use diffraction
use MBmodule
use gvectors
use kvectors
use files
use io
use math
use defectmodule
use rotations
use timing
use ECCImod
use NameListHDFwriters
use HDFsupport
use ISO_C_BINDING
use omp_lib
use stringconstants
use ISO_C_BINDING
use image
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

type(ECCINameListType),INTENT(INOUT)    :: eccinl
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile

integer(HSIZE_T)                        :: dims3(3), cnt3(3), offset3(3)
integer(kind=irg)                       :: nn,i,j,k,npix,npiy,ii,jj,kkk, numset, t_interval,nat(maxpasym), montage_nx, montage_ny, &
                                           DF_nums_new,DF_npix_new,DF_npiy_new, numstart,numstop, isg, TID, tickstart, &
                                           NTHR, SETNTHR, isym, ir, ga(3), gb(3),ic,g,numd,ix,iy,nkt,nbeams, ik, ig, &
                                           numk,ixp,iyp, io_int(6), skip, gg(3), error_cnt, dinfo, nref, hdferr, maxXY

integer(kind=irg),parameter             :: numdd=360 ! 180
real(kind=sgl)                          :: thick, X(2), bragg, thetac, kstar(3), gperp(3), av, mi, ma, &
                                           gdotR,DF_gf(3), tpi, DM(2,2), DD, c(3), gx(3), gy(3), &
                                           gac(3), gbc(3),zmax, io_real(2), ijmax, tstop, kk(3)
real(kind=dbl)                          :: arg, glen, DynFN(3), xx
complex(kind=dbl),allocatable           :: DHWM(:,:),DHWMvoid(:,:),DDD(:,:),Sarray(:,:,:,:)
complex(kind=dbl),allocatable           :: amp(:),amp2(:),Azz(:,:),DF_R(:,:)
complex(kind=dbl)                       :: czero=cmplx(0.D0,0.D0),cone=dcmplx(1.D0,0.D0)
complex(kind=dbl)                       :: para(0:numdd),dx,dy,dxm,dym, xgp
real(kind=sgl),allocatable              :: sgarray(:,:)
real(kind=sgl),allocatable              :: disparray(:,:,:,:),imatvals(:,:), ECCIimages(:,:,:), XYarray(:,:), ECCIstore(:,:,:)
integer(kind=sgl),allocatable           :: expval(:,:,:), hklarray(:,:), XYint(:,:)
complex(kind=dbl),allocatable           :: Lgh(:),Sgh(:), DHWMz(:,:)
logical                                 :: ECCI, verbose, overwrite=.TRUE., insert=.TRUE.
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
character(fnlen)                        :: groupname, dataset, outname, Image_filename, fname
character(fnlen,kind=c_char)            :: line2(1)

type(unitcell)                          :: cell
type(gnode)                             :: rlp
type(DynType)                           :: Dyn
type(kvectorlist),pointer               :: khead, ktmp
type(symdata2D)                         :: TDPG
type(BetheParameterType)                :: BetheParameters
type(reflisttype),pointer               :: reflist, firstw, rltmp, rltmpa, rltmpb
type(defecttype)                        :: defects

type(HDFobjectStackType)                :: HDF_head

! declare variables for use in object oriented image module
integer                                 :: iostat
character(len=128)                      :: iomsg
logical                                 :: isInteger
type(image_t)                           :: im
integer(int8)                           :: i8 (3,4)
integer(int8), allocatable              :: montage(:,:)

 ECCI = .TRUE.
 
 nullify(HDF_head%next)
 nullify(khead)
 nullify(ktmp)

 !nullify(cell)        
 !allocate(cell)        
  
 call timestamp(datestring=dstr, timestring=tstrb)
 call Time_tick(tickstart)

! copy some of the namelist parameters into the defects structure
 defects%DF_npix = eccinl%DF_npix
 defects%DF_npiy = eccinl%DF_npiy
 defects%DF_L = eccinl%DF_L
 defects%DF_slice = eccinl%DF_slice

! first we define some default values
 tpi = 2.0*sngl(cPi)
 dinfo = 0              ! switch to make makedislocation verbose
 t_interval = 10        ! default timing interval (output every t_interval image columns)
 numd = numdd

 verbose = .TRUE.
 call Initialize_Cell(cell,Dyn,rlp,eccinl%xtalname, eccinl%dmin, eccinl%voltage, verbose)

! determine the point group number and get the ZAP 2-D symmetry  NEEDS TO BE MODIFIED WITH NEW ROUTINES
 j=0
 do i=1,32
  if (SGPG(i).le.cell%SYM_SGnum) j=i
 end do
 call BFsymmetry(cell,eccinl%k,j,isym,ir)
  
! determine and display the shortest reciprocal lattice vectors for this zone
 call ShortestG(cell,eccinl%k,ga,gb,isym)
 io_int(1:3) = eccinl%k(1:3)
 call WriteValue('', io_int, 3,  "(//,' ','[',3I2,'] has Bright Field symmetry ')",advance="no")
 call Message(PGTWD(isym),"(A)",advance="no")
 io_int(1) = ir
 call WriteValue(' order = ', io_int, 1, "(I4/)")
 io_int(1:3) = ga(1:3)
 io_int(4:6) = gb(1:3)
 call WriteValue(' Reciprocal lattice vectors : ', io_int, 6, "('(',3I3,') and (',3I3,')',/)")

! determine the cartesian components of ga
 defects%DF_gf = float(ga)
 defects%DF_gstar = defects%DF_gf/CalcLength(cell,defects%DF_gf,'r')**2    ! define G* such that G.G* = 1
 call TransSpace(cell,defects%DF_gf,defects%DF_gc,'r','c')         ! convert to Cartesian reference frame

! get the maximum beam tilt angle
 nkt = nint( eccinl%ktmax / eccinl%dkt )
 ijmax = nkt**2
 bragg = CalcDiffAngle(cell,ga(1),ga(2),ga(3)) * 0.5
 if (eccinl%ktmax.eq.0.0) then
   thetac = bragg
 else
   thetac = (eccinl%ktmax * 2.0 * bragg) * 0.5
 end if

! here we figure out how many beams there are
  if (eccinl%progmode.eq.'array') then 
    call Calckvectorcone(cell,khead,eccinl%k,ga,eccinl%lauec,eccinl%ktmax,nkt,numk)
    io_int(1)=numk 
    call WriteValue('Total number of independent incident beam directions inside cone = ', io_int, 1,"(I6)")
  end if
  if (eccinl%progmode.eq.'trace') then  
! single image mode, potentially with a narrow range of incident beam directions
    call Calckvectortrace(cell,khead,eccinl%k,ga,eccinl%lauec,eccinl%lauec2,1.0,eccinl%nktstep,numk)
    io_int(1)=numk 
    call WriteValue('Total number of independent incident beam directions along trace = ', io_int, 1,"(I6)")
  end if

! set the Bethe parameters
  call Set_Bethe_Parameters(BetheParameters,.TRUE.)

! next, we read all the foil and defect data using the new InitializeDefects routine in defectmodule.f90
  verbose = .FALSE.
  call InitializeDefects(cell,defects,eccinl%defectfilename,eccinl%DF_npix,eccinl%DF_npiy,eccinl%DF_L,DF_gf,error_cnt,verbose)
  DynFN = defects%foil%F
  
! and determine the overall reflection list
  xx = 1.0/CalcLength(cell,float(eccinl%k),'d')
  kk = xx*float(eccinl%k)/cell%mLambda     ! don't forget to scale the incident beam vector by the wavelength (reciprocal nm)
  call Initialize_ReflectionList(cell, reflist, BetheParameters, sngl(DynFN), kk, sngl(eccinl%dmin), nref, verbose)
  nn = nref

! this next set of lines is replaced by the more recent routine to compute the dynamical matrix, which 
! is done using the new Bethe potential truncation criteria.
!
! then we need to prune the reflection list to have only reflections that will actually occur in the 
! computation
! call Message(' Pruning reflection list (this may take a while ...) ',"(A)")
! call Prune_ReflectionList(cell,khead,reflist,Dyn,BetheParameters,numk,nbeams)
! io_int(1) = nbeams
! call WriteValue('Number of contributing beams  : ', io_int, 1, '(I)')
! nn = nbeams

! ideally, we should use Bethe potentials to reduce the size of the dynamical matrix;
! while the theory has been worked out to do this, it would require tremendous changes
! to the program starting about here; given the time limitations, there won't be any 
! chance to do this before the end of the year (2013), so we'll leave that for some other time...
  
! allocate the various DHW Matrices
  allocate(DHWMz(nn,nn),DHWM(nn,nn),DHWMvoid(nn,nn))
  DHWMvoid = czero; DHWMz=czero; DHWM(nn,nn)=czero
  
 ! Compute the off-diagonal part of the complex DHW matrix (factor i is included)
 ! We can precompute those because they will not change at all during the run
 !       (these lines implement the equations on page 476 of the EM book)
 ! In this program, the reflections are stored using linked lists, which does not lend itself to
 ! OpenMP acceleration;  so, we'll have to re-write this at some point in the future...
 !
 ! this is also where we compute the decomposition of the reflection indices w.r.t. ga and gb,
 ! and store them in the nab(1:2) field of the linked list; these are used to compute the 
 ! defect contributions to the dynamical matrix in the displace routine of the MEmath module
 !
 DM(1,1) = CalcDot(cell,float(gb),float(gb),'c')
 DM(1,2) = -CalcDot(cell,float(ga),float(gb),'c')
 DM(2,1) = DM(1,2)
 DM(2,2) = CalcDot(cell,float(ga),float(ga),'c')
 DD = DM(1,1)*DM(2,2) - DM(1,2)*DM(2,1)

 cone = cmplx(0.D0,cPi)

 rltmpa => reflist%next    ! point to the front of the list
! ir is the row index
  do ir=1,nn
   rltmpb => reflist%next   ! point to the front of the list
! ic is the column index
   do ic=1,nn
    if (ic.ne.ir) then  ! exclude the diagonal
! compute Fourier coefficient of electrostatic lattice potential 
     call CalcUcg(cell,rlp,rltmpa%hkl - rltmpb%hkl)
     DHWMz(ir,ic) = rlp%qg * cone
!cmplx(- cPi * aimag(rlp%qg), cPi * real(rlp%qg),dbl)  ! and initialize the off-diagonal matrix element (including i Pi)
    end if
    rltmpb => rltmpb%next  ! move to next column-entry
   end do
! decompose this point w.r.t ga and gb
   X(1) = CalcDot(cell,float(rltmpa%hkl),float(ga),'c')
   X(2) = CalcDot(cell,float(rltmpa%hkl),float(gb),'c')
   X = matmul(DM,X)/DD
   rltmpa%nab(1:2) = int(X(1:2))
   rltmpa => rltmpa%next   ! move to next row-entry
  end do
  call Message('Reference Darwin-Howie-Whelan matrix initialized',"(A/)")

! compute the normal absorption factor xgp (which equals rlp%qg with g=0)
  rlp%qg = cmplx(0.D0,0.D0)
  call CalcUcg(cell,rlp,(/0,0,0/))
  xgp = cmplx(-cPi/rlp%xgp,0.0) ! cPi * rlp%qg * cone
  io_real(1) = rlp%xgp
  call WriteValue('Normal absorption length : ', io_real, 1, "(F10.5/)")

! define the foil thickness, attenuation, and number slices per column
  thick = defects%foil%zb    ! this is the same everywhere for this version; needs to be updated in the next version
  defects%DF_nums = nint(thick/defects%DF_slice)  ! this is the number of slices for this particular column

! next, deal with all the defects  This is the same as for the systematic row, except that in the zone
! axis case we have two fundamental vectors and we can not use the same integer-based approach;  therefore,
! we have to store two floats for each slice in each column instead of a single integer.
!


! if there is a diplacement field file entered in the nml file,  
! then we simply read that file in; otherwise, we create it here
if ((eccinl%dispmode.eq.'new').or.(eccinl%dispmode.eq.'not')) then

! precompute ALL the defect columns and, if needed, store them in dispfile
! this portion should be carried out in multi-threaded mode as much as possible
  allocate(disparray(2,defects%DF_nums,defects%DF_npix,defects%DF_npiy),imatvals(2,defects%DF_nums))
  disparray = 0.0; imatvals = 0
  
! initiate multi-threaded segment
!!$OMP     PARALLEL PRIVATE(TID,DF_R,imatvals,gdotR,i,j,k,imat) &
!!$OMP&   SHARED(NTHR,DF_npix,DF_npiy,DF_nums,numvoids,numdisl,numsf,numinc,disparray,t_interval)
!  NTHR = OMP_GET_NUM_THREADS()
!  TID = OMP_GET_THREAD_NUM()
!  write (*,*) TID,': entering parallel region'
 TID = 0
 NTHR = 1
 allocate(defects%DF_R(defects%DF_nums,3))     ! each thread has its own DF_R array 

 call TransSpace(cell,float(ga),gac,'r','c')
 call TransSpace(cell,float(gb),gbc,'r','c')
 

!!$OMP DO SCHEDULE (GUIDED)
  do i=1,defects%DF_npix  
    do j=1,defects%DF_npiy
      defects%DF_R = 0.0
! compute the displacement vectors DF_R for all points in the column
      call CalcR(cell,defects,i,j)
! loop over the fixed thickness slices
      do ik=1,defects%DF_nums
! then convert to the dot-product 
       if (defects%DF_R(ik,1).eq.-10000.0) then  ! this is point inside a void
        imatvals(1:2,ik) = -10000
       else  ! it is not a void, so use the full dot product g.R (all vectors must be Cartesian !)
! use gac and gbc to get the two dot products and store both of them as integers mapped onto the 0..numd range
!
! due to the logarithmic singularity at a dislocation core, it is possible for gdotR to be NaN; we need
! to intercept these cases, and replace the value of gdotR by 0.0
         gdotR = sum(gac*defects%DF_R(ik,1:3))
         if (NANCHK(gdotR)) gdotR = 0.0         
         xx = numd*amod(gdotR+1000.0,1.0)
         imatvals(1,ik) = xx

         gdotR = sum(gbc*defects%DF_R(ik,1:3))
         if (NANCHK(gdotR)) gdotR = 0.0         
         xx = numd*amod(gdotR+1000.0,1.0)
         imatvals(2,ik) = xx
       end if
     end do ! ik loop
     disparray(1:2,1:defects%DF_nums,i,j) = imatvals(1:2,1:defects%DF_nums)
   end do
end do
!!$OMP END DO 
!!$OMP END PARALLEL
end if

! and, if needed, store the defect displacement field for re-runs
! this needs to be changed to an HDF5 formatted file
if (eccinl%dispmode.ne.'not') then 
  if (eccinl%dispmode.eq.'new') then 
    call Message('Displacement field data will be stored in '//eccinl%dispfile,"(/A/)")
    outname = trim(EMsoft_getEMdatapathname())//trim(eccinl%dispfile)
    outname = EMsoft_toNativePath(outname)
    open(unit=dataunit,file=trim(outname),status='new',action='write',form='unformatted')
    i = 2
    write (dataunit) i,defects%DF_nums,defects%DF_npix,defects%DF_npiy
    write (dataunit) disparray
    close(unit=dataunit,status='keep')
  else  ! there is a pre-computed defect file, so let's load it
   allocate(disparray(2,defects%DF_nums,defects%DF_npix,defects%DF_npiy))
   disparray = 0
   open(unit=dataunit,file=trim(EMsoft_toNativePath(eccinl%dispfile)),status='old',action='read',form='unformatted')
   read (dataunit) i,DF_nums_new,DF_npix_new,DF_npiy_new
! check to make sure that these dimensions are the same as the ones used in the current run of the program
   if ((DF_nums_new.ne.defects%DF_nums).or.(DF_npix_new.ne.defects%DF_npix).or.(DF_npiy_new.ne.defects%DF_npiy)) then
    io_int(1) = DF_nums_new; io_int(2) = DF_npix_new; io_int(3) = DF_npiy_new
    call WriteValue('The dimensions of the defect array in the file are : ', io_int, 3, "(3I5)")
    io_int(1) = defects%DF_nums; io_int(2) = defects%DF_npix; io_int(3) = defects%DF_npiy
    call WriteValue('The dimensions in the SRdef_rundata file do not agree : ', io_int, 3, "(3I5)")
    call Message('Terminating program run',"(A)")
    stop
   end if
! ok, we're good, so read the actual data...  
  read (dataunit) disparray
  close(unit=dataunit,status='keep')
  call Message('read displacement array from file '//trim(eccinl%dispfile),"(A)")
 end if
end if
io_real(1) = minval(disparray)
io_real(2) = maxval(disparray)
call WriteValue('disparray bounds: ', io_real, 2, "(2(F10.5,' '))")

! ok, all the set up is now complete;
  npix = defects%DF_npix
  npiy = defects%DF_npiy
  
  allocate(ECCIimages(npix,npiy,1))
  ECCIimages = 0.0

  if (trim(eccinl%montagename).ne.'undefined') then
    allocate(ECCIstore(npix,npiy,numk))
    ECCIstore = 0.0
  end if

! compute the excitation error array for all incident beam directions
  allocate(sgarray(nn,numk))
! loop over the wave vector linked list
  ktmp => khead
  beamloopCL: do ik=1,numk
!    ll = ktmp%kt        ! this is the tangential component of the wave vector
! and loop over all reflections
    rltmpa => reflist%next
    reflectionloopCL: do ig=1,nn
      gg = float(rltmpa%hkl)
      sgarray(ig,ik) = Calcsg(cell,float(gg),sngl(ktmp%k),sngl(DynFN))
 ! and we move to the next reflection in the list
      rltmpa => rltmpa%next
    end do reflectionloopCL  
    ktmp => ktmp%next
  end do beamloopCL

! loop over all reflections to get the appropriate powers
  allocate(expval(2,nn,nn))
  rltmpa => reflist%next    ! point to the front of the list
! ir is the row index
  do ir=1,nn
     rltmpb => reflist%next   ! point to the front of the list
! ic is the column index
     do ic=1,nn
       if (ic.ne.ir) then  ! exclude the diagonal
         expval(1,ir,ic) = rltmpa%nab(1)-rltmpb%nab(1) 
         expval(2,ir,ic) = rltmpa%nab(2)-rltmpb%nab(2)
       end if
       rltmpb => rltmpb%next  ! move to next column-entry
    end do
    rltmpa => rltmpa%next   ! move to next row-entry
  end do

! define the numd complex defect parameters (i.e., precompute the sin and cos arrays
  do i=0,numd
    arg = 2.D0*cPi*dble(i)/dble(numd)
    para(i) = cmplx(dcos(arg),-dsin(arg))
  end do

! determine the Sgh array, which is sort of a glorified structure factor...
! since all incident beam orientations use the same number of reflections,
! we can compute Sgh here instead of inside the main loop.  This will need
! to be modified when we start using Bethe potentials.

  call Message('Computing Sgh array ...',"(A)",advance="no")
  numset = cell % ATOM_ntype  ! number of special positions in the unit cell
  allocate(Sgh(nn))
  nat = 0
  call ECCICalcSgh(cell,nn,Sgh,nat)
  call Message(' done',"(A)")


!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! store necessary data in data file
  call Message('Storing output ECCIs in '//trim(eccinl%dataname),"(A/)")

  nullify(HDF_head%next)
! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
  outname = trim(EMsoft_getEMdatapathname())//trim(eccinl%dataname)
  outname = EMsoft_toNativePath(outname)
  hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
groupname = SC_ECCI
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_ECCImasterNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! in this case, we also put the defect json file inside the HDF file
dataset = SC_ECCIdefectJSON
  outname = trim(EMsoft_getEMdatapathname())//trim(eccinl%defectfilename)
  outname = EMsoft_toNativePath(outname)
  hdferr = HDF_writeDatasetTextFile(dataset, outname, HDF_head)

! and also the foil descriptor file
dataset = SC_ECCIfoilJSON
  outname = trim(EMsoft_getEMdatapathname())//trim(defects%foilname)
  outname = EMsoft_toNativePath(outname)
  hdferr = HDF_writeDatasetTextFile(dataset, outname, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)
  call HDFwriteECCINameList(HDF_head, eccinl)
! we do not need to parse the defect information, since the defect and foil JSON files
! are both included in the NMLfiles section of the HDF5 output file., and we have routines in 
! JSONsupport to parse those files.

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_Braggga
  hdferr = HDF_writeDatasetFloat(dataset, CalcDiffAngle(cell,ga(1),ga(2),ga(3))*0.5 , HDF_head)

! if (eccinl%progmode.eq.'array') then 
!   write (dataunit) eccinl%ktmax                                     ! max beam convergence in units of |g_a|
! else
!   write (dataunit) 1.0                                       ! max beam convergence in units of |g_a|
! end if

dataset = SC_thetac
  hdferr = HDF_writeDatasetFloat(dataset, thetac , HDF_head)

dataset = SC_nref
  hdferr = HDF_writeDatasetInteger(dataset, nn, HDF_head)

dataset = SC_numk
  hdferr = HDF_writeDatasetInteger(dataset, numk, HDF_head)

dataset = SC_npix
  hdferr = HDF_writeDatasetInteger(dataset, eccinl%DF_npix, HDF_head)

dataset = SC_npiy
  hdferr = HDF_writeDatasetInteger(dataset, eccinl%DF_npiy, HDF_head)

! number of reflections, and associated information (hkl, ...)
  call TransSpace(cell,float(eccinl%k),c,'d','c')
  call NormVec(cell,c,'c')
! then make ga the x-axis
  call TransSpace(cell,float(ga),gx,'r','c')
  call NormVec(cell,gx,'c')
! compute the cross product between k and gx; this is the y-axis
  call CalcCross(cell,c,gx,gy,'c','c',0)
  
! this needs to be fixed !!!!!
  call TransSpace(cell,float(eccinl%k),kstar,'d','r')        ! transform incident direction to reciprocal space
  call CalcCross(cell,float(ga),kstar,gperp,'r','r',0)! compute g_perp = ga x k
  call NormVec(cell,gperp,'r')                        ! normalize g_perp

  DM(1,1) = CalcDot(cell,gperp,gperp,'c')
  DM(1,2) = -CalcDot(cell,float(ga),gperp,'c')
  DM(2,1) = DM(1,2)
  DM(2,2) = CalcDot(cell,float(ga),float(ga),'c')
  DM = transpose(DM)
  DD = DM(1,1)*DM(2,2) - DM(1,2)*DM(2,1)
  glen = CalcLength(cell,float(ga),'r')

! number of wave vectors, tangential components, etc...
  allocate(hklarray(3,nn),XYarray(2,numk))

  ktmp => khead
  do ic=1,numk
      XYarray(1,ic) = -CalcDot(cell,sngl(ktmp%kt),float(ga),'c') ! / glen
      XYarray(2,ic) = -CalcDot(cell,sngl(ktmp%kt),gperp,'c') * glen
      ktmp => ktmp%next
  end do

dataset = SC_XYarray
  hdferr = HDF_writeDatasetFloatArray2D(dataset, XYarray, 2, numk, HDF_head)
  if (trim(eccinl%montagename).eq.'undefined') deallocate(XYarray)

  rltmpa => reflist%next 
  do ic=1,nn
    hklarray(1:3,ic) = rltmpa%hkl(1:3)
    rltmpa=>rltmpa%next
  end do

dataset = SC_hklarray
  hdferr = HDF_writeDatasetIntegerArray2D(dataset, hklarray, 3, nn, HDF_head)
  deallocate(hklarray)

! create the ECCIimage hyperslab and write zeroes to them for now
dataset = SC_ECCIimages
  dims3 = (/  npix, npiy, numk /)
  cnt3 = (/ npix, npiy, 1 /)
  offset3 = (/ 0, 0, 0 /)
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, ECCIimages, dims3, offset3, cnt3, HDF_head)

! the following 6 lines are temporary for testing
  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

  
! leave the HDF5 file open for final output at the end of the program
!--------------------------------------------------------------
!--------------------------------------------------------------
!--------------------------------------------------------------
  
  allocate(Sarray(nn,nn,0:numd,0:numd))

! initialize the timer
  numstart = 1
  numstop = numk   
  io_int(1) = numk
  call WriteValue('ECCI: number of beam directions =  ', io_int, 1, "(I5)")
  
  cone = cmplx(1.D0,0.D0)
  
!--------------------------------------------------------------
!--------------------------------------------------------------
!--------------------------------------------------------------
mainloop: do isg = numstart,numstop   ! this is the main computational loop
 ECCIimages = 0.0

!--------------------------------------------------------------
! here we precompute an array of scattering matrices that can 
! then be used, either directly, or via bi-linear interpolation,
! by the image computation portion of this program.
!
! For starters, we'll subdivide the range of possible alpha values
! in 180 segments (2 degrees each), with a copy of the last one
! (i.e., 181x181 = 32761 entries or 240 Mb for 31 beams)
!
! this part is essentially the same as the threaded section of the older
! ZAdefect.all.f90 program (which was a test program).  It is entirely
! possible that this computation would go way faster on a GPU...
!
! get the correct excitation errors for this beam orientation (in STEM mode);
! fill the diagonal of the reference dynamical matrix and the void matrix
! make sure that normal absorption is properly taken into account...
  forall (i=1:nn)
   DHWMz(i,i)= cmplx(0.D0,2.D0*cPi*sgarray(i,isg)) + xgp ! xgp already has i Pi in it.
   DHWMvoid(i,i) = DHWMz(i,i)
  end forall

NTHR = eccinl%nthreads 

!$OMP  PARALLEL NUM_THREADS(NTHR) DEFAULT(SHARED) PRIVATE(TID,i,j,k,ii,jj,ic,ir,g,Azz,DDD,zmax)
TID = OMP_GET_THREAD_NUM() 
!TID = 0
allocate(Azz(nn,nn), DDD(nn,nn))   ! these are private variables, so each thread must allocate them !

if (TID.eq.0) then
  io_int(1) = isg
  call WriteValue(' -> ',io_int,1,"(I4,' ')",advance="no")
  call Message('starting Sarray computation',"(A)",advance="no")
end if

!$OMP DO SCHEDULE(STATIC)
do j=0,numd
 do i=0,numd 
! loop over all reflections in the array DD using the information in expval
! ic is the column index
 do ic=1,nn
! ir is the row index
    do ir=1,nn
    if (ic.ne.ir) then  ! exclude the diagonal
     DDD(ir,ic) = DHWMz(ir,ic) * para(i)**expval(1,ir,ic) * para(j)**expval(2,ir,ic)
    else
     DDD(ir,ic) = DHWMz(ir,ic) 
    end if
   end do
  end do
    
  call MatrixExponential(DDD, Azz, dble(defects%DF_slice), 'Pade', nn)  
    
   Sarray(1:nn,1:nn,i,j) = Azz(1:nn,1:nn)
 end do
 if ((TID.eq.0).and.(mod(j,10).eq.0)) then
    call Message('.',"(A1)",advance="no")
 end if
end do
!$OMP END DO

deallocate(Azz, DDD)
!$OMP END PARALLEL

call Message(' done; scattering matrix computation ',"(A)",advance="no")

!----------------------------------------------------!
! Finally, here it is: the actual image computation  !
! This was modified from the regular (S)TEM mode to  !
! reflect the depth integration, which requires a    !
! summation of the product of Sgh and Lgh.           !
!----------------------------------------------------!

 NTHR = 12
!$OMP  PARALLEL NUM_THREADS(NTHR) DEFAULT(SHARED) PRIVATE(TID,i,j,k,ii,Azz,amp,amp2,ix,iy,dx,dy,dxm,dym,ixp,iyp,Lgh,ir,ic)
 TID = OMP_GET_THREAD_NUM() 
! TID = 0 
 allocate(Azz(nn,nn),amp(nn),amp2(nn),Lgh(nn))
 
!$OMP DO SCHEDULE (STATIC)
 donpix: do i=1,npix
 if ((TID.eq.0).and.(mod(i,10).eq.0)) then
   call Message('.',"(A1)",advance="no")
 end if
 donpiy:   do j=1,npiy
! initialize the wave function for this pixel with (1.0,0.0) for the incident beam
    Lgh = czero
    amp = czero
    amp(1) = cone
!    if ((TID.eq.0).and.(isg.eq.1).and.(i.eq.1).and.(j.eq.1)) then
!      write (*,*) 'Storing data for comparison with ECP program; dimensions : ',nn,DF_nums
!      open(unit=dataunit,file='ECCIcheck.data',status='unknown',action='write',form='unformatted')
!      write (dataunit) nn,DF_nums
!    end if

    doslices: do k=1,defects%DF_nums    ! loop over the fixed thickness slices
! compute the appropriate scattering matrix to propagate with (see section 8.3.3 in the book)
       if (disparray(1,k,i,j).eq.-10000) then  ! this is point inside a void
         Azz = DHWMvoid    ! so we use the void propagator matrix
       else  ! it is not a void
! in this version, we use complex bi-variate interpolation to select the appropriate Azz 
! matrix from the pre-computed Sarray.
         ix = int(disparray(1,k,i,j))
         ixp = ix+1
         if (ix.eq.numd) ixp=1
         iy = int(disparray(2,k,i,j))
         iyp = iy+1
         if (iy.eq.numd) iyp=1
         dx = cmplx(amod(disparray(1,k,i,j),1.0),0.0)
         dy = cmplx(amod(disparray(2,k,i,j),1.0),0.0)
         dxm = cone-dx
         dym = cone-dy
         Azz = dxm*dym*Sarray(1:nn,1:nn,ix,iy)+dx*dym*Sarray(1:nn,1:nn,ixp,iy)+ &
               dxm*dy*Sarray(1:nn,1:nn,ix,iyp)+dx*dy*Sarray(1:nn,1:nn,ixp,iyp)
       end if

       amp2 = matmul(Azz,amp)

       if (k.eq.1) then 
          Lgh = abs(amp2)**2
       else
          Lgh = Lgh + abs(amp2)**2
       end if

       amp = amp2
    end do doslices ! loop over slices 

! then we need to multiply Sgh and Lgh, sum, and take the real part which will
! produce the desired BSE intensity
    ECCIimages(i,j,1) = sngl(real(sum( Sgh * Lgh )))
 
    end do donpiy
end do donpix
!$OMP END DO
  deallocate(Azz,amp,amp2,Lgh)
!$OMP END PARALLEL

call Message('  done',"(A)")
  
  ECCIimages = ECCIimages / float(defects%DF_nums) / float(sum(nat))
! call Time_remaining(isg-numstart+1,numstop-numstart+1)

! and here we write the current image to the datafile; we also update the timestamp
  call timestamp(datestring=dstr, timestring=tstre)

  nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
  call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
  outname = trim(EMsoft_getEMdatapathname())//trim(eccinl%dataname)
  outname = EMsoft_toNativePath(outname)
  hdferr =  HDF_openFile(outname, HDF_head)

! update the time string
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_ECCI
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  tstop = Time_tock(tickstart)
dataset = SC_Duration
  if (isg.eq.numstart) then 
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
  else
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
  end if

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)
  
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

! update the current energy level counter, so that the restart option will function
! dataset = 'lastEnergy'
! hdferr = HDF_writeDataSetInteger(dataset, iE, HDF_head, overwrite)

! add data to the hyperslab
dataset = SC_ECCIimages
  dims3 = (/  npix, npiy, numk /)
  cnt3 = (/ npix, npiy, 1 /)
  offset3 = (/ 0, 0, isg-1 /)           ! offsets start at 0
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, ECCIimages, dims3, offset3, cnt3, HDF_head, insert)

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

  if (trim(eccinl%montagename).ne.'undefined') then
    do i=1,eccinl%DF_npix
      do j=1,eccinl%DF_npiy
        ECCIstore(i,j,isg) = ECCIimages(i,j,1)
      end do
    end do
  end if

200 end do mainloop

call Message('Data stored in file '//trim(outname),"(/A/)")

! option in response to feature request #31: add direct image output with a montage of all 
! simulated ECCIs, either in a 2D array (array mode illumination) or a linear array (trace mode).
if (trim(eccinl%montagename).ne.'undefined') then

  call Message('Generating image montage')

! output the montage as an image file (tiff, jpeg, or png) 
  fname = trim(EMsoft_getEMdatapathname())//trim(eccinl%montagename)
  fname = EMsoft_toNativePath(fname)
  Image_filename = trim(fname)

  
! allocate the montage array
  if (eccinl%progmode.eq.'array') then 
! divide the beam offsets by the dkt step size so that we can turn them into integers
    XYarray = XYarray / eccinl%dkt
    allocate(XYint(2,numk))
    XYint = nint(XYarray)
    maxXY = maxval(XYint)
    montage_nx = eccinl%DF_npix*(2*maxXY+1)
    montage_ny = eccinl%DF_npiy*(2*maxXY+1)
    allocate(montage(montage_nx,montage_ny))
  else ! progmode = 'trace'
    montage_nx = eccinl%DF_npix*numk
    montage_ny = eccinl%DF_npiy
    allocate(montage(montage_nx,montage_ny))
  end if

! assign the average value of the ECCIimages array to the montage
  av = sum(ECCIstore)/float(eccinl%DF_npix)/float(eccinl%DF_npiy)/float(numk)
  montage = av 

! get the intensity scaling parameters
  ma = maxval(ECCIstore)
  mi = minval(ECCIstore)

! fill and scale the montage array
  do kkk=1,numk
    do j=1,eccinl%DF_npiy
      do i=1,eccinl%DF_npix
        if (eccinl% progmode.eq.'array') then
          ii = eccinl%DF_npix * (maxXY + XYint(1,kkk)) + i
          jj = eccinl%DF_npiy * (maxXY + XYint(2,kkk)) + j
          montage(ii,jj) = int(255 * (ECCIstore(i,j,kkk)-mi)/(ma-mi))
        else
          ii = eccinl%DF_npix * (kkk-1) + i
          jj = j
          montage(ii,jj) = int(255 * (ECCIstore(i,j,kkk)-mi)/(ma-mi))
        end if
      end do
    end do
  end do
  deallocate(ECCIstore)

! set up the image_t structure
  im = image_t(montage)
  if (im%empty()) call Message("EMECCI","failed to convert array to image")

! create the file
  call im%write(trim(Image_filename), iostat, iomsg) ! format automatically detected from extension
  if (0.ne.iostat) then
    call Message("Failed to write image to file : "//iomsg)
  else  
    call Message('ECCI image montage written to '//trim(Image_filename))
  end if 
  deallocate(montage)

end if








end subroutine ComputeECCI
