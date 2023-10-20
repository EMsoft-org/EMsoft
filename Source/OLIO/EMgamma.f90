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
! EMsoft:EMgamma.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgamma 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Near zone axis image simulation for mix of gamma and gamma' phases
!
!> @details Based on a conversation with Mike Mills on 11/01/13; question we 
!> try to answer is the following: how can we perform a dynamical simulation
!> of a two-phase microstructure, in which the precipitates have a structure
!> that has many reflections in common with the disordered matrix.  In this
!> particular case, obviously we have fcc Ni solid solution, and L1_2 ordered
!> gamma phase.  
!>
!> We'll stick to near [001] zone axis orientations for now; we need to load 
!> two crystal structures, compute the dynamical matrix for the ordered phase,
!> then copy that for the disordered phase and recompute the scattering factors;
!> to do this efficiently, we may want to re-order the reflections into two
!> groups (fundamental and superlattice), although that is not essential to the
!> simulation.  The microstructure can be loaded from a file, and has simple
!> integers on each voxel, indicating which phase the voxel belongs to.  Once
!> we have the scattering matrices for both phases, it is just a matter of running
!> down each integration column.  We can then also implement a bent foil, which
!> is equivalent to a beam tilt, so we can likely reuse some of the lacbed code. 
!>
!> Starting late June 2017:
!> program incorporated in EMsoftPrivate repo in preparation for collaboration
!> with OSU group; since this is a special "one-off" program, we keep it in the
!> OLIO folder...
!
!> @date 11/02/13 MDG 1.0 original (took a few days to get it right...)
!> @date 06/28/17 MDG 2.0 complete rewrite to current EMsoft libraries.
!--------------------------------------------------------------------------
program EMgamma

use local
use files
use io
use NameListTypedefs
use NameListHandlers
use JSONsupport

IMPLICIT NONE

character(fnlen)                   :: nmldeffile, progname, progdesc
type(EMgammaNameListType)          :: enl
integer(kind=irg)                  :: res

nmldeffile = 'EMgamma.nml'
progname = 'EMgamma.f90'
progdesc = 'Gamma-gamma'' microstructure dynamical image simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 0, 30 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
! call JSONreadEMgammaNameList(enl, nmldeffile, error_cnt)
  call Message('JSON input not yet implemented')
  STOP
else
  call GetEMgammaNameList(nmldeffile,enl)
end if

! perform the (near) zone axis computations
call GAMMAimage(enl, progname, nmldeffile)

end program EMgamma

!--------------------------------------------------------------------------
!
! SUBROUTINE:GAMMAimage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the bright field and dark field images for a bent foil
!> with a gamma-gamma' microstructure, using full dynamical theory
!
!> @param enl name list
!> @param progname program name
!> @param nmldeffile namelist file name
!
!> @date 11/02/13  MDG 1.0 original
!> @date 11/13/13  MDG 1.1 moved MatrixExponential into math.f90 library file
!> @date 11/14/13  MDG 1.2 addition of Bethe potentials for scattering matrix formalism
!> @date 06/28/17  MDG 2.0 complete rewrite with modern EMsoft libraries
!--------------------------------------------------------------------------
subroutine GAMMAimage(enl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use symmetry
use crystal
use constants
use io
use files
use diffraction
use noise
use HDF5
use HDFsupport
use ISO_C_BINDING
use omp_lib
use timing

IMPLICIT NONE

type(EMgammaNameListType),INTENT(IN)  :: enl
character(fnlen),INTENT(IN)           :: progname
character(fnlen),INTENT(IN)	          :: nmldeffile


real(kind=sgl)      		:: ktmax, io_real(3), bragg, thetac, sc, minten, pxy(2), galen, &
                       	   frac, thetam , dhkl, xstep, ystep, zstep, gmax, gcur
integer(kind=irg)   		:: ijmax,ga(3),gb(3),cnt, PX, ss, icnt, pgnum, ih, nunique, famnum, numg, iz, &
                      		   newcount,count_rate,count_max, io_int(6), i, j, isym, ir, skip, ghkl(3), gammapNBeams, &
                      		   npx, npy, numt, numk, npix, ik, ip, jp, istat, dgn, nbeams, gafcc(3),gbfcc(3), sdm(2), &
                      		   ifamily, famhkl(3), inum, maxHOLZ, numksame, sLUT(3), imh, imk ,iml, dimx, dimy, dimz, &
                      		   ix, iy, jj, jnum, ii, vdimx, vdimy, vdimz
complex(kind=dbl)		    :: pre, DUg, DUgp, czero
real(kind=dbl)			    :: x, z0            
character(3)			      :: method
real(kind=dbl)			    :: glen, gan(3), gperp(3), kstar(3) 
real(kind=sgl)          :: ktlen, kt(3), kr(3)

type(kvectorlist),pointer       :: khead
type(unitcell)                  :: cell, gammacell, gammapcell
type(DynType)                   :: Dyn
type(gnode)                     :: rlp

type(reflisttype),pointer       :: gammapreflist, gammareflist
complex(kind=dbl),allocatable   :: gammaLUT(:,:,:), gammapLUT(:,:,:), ScatMat(:,:), amp(:), amp2(:), &
                                   ScatMat0(:,:), ScatMat1(:,:), ScatMat2(:,:), ScatMat3(:,:)
real(kind=sgl),allocatable    	:: images(:,:,:)
real(kind=sgl),allocatable 	    :: microstructure(:,:,:)
integer(kind=sgl),allocatable 	:: variantnumber(:,:,:)
real(kind=sgl),allocatable    	:: inten(:)
real(kind=dbl)			            :: s(3)
logical                         :: verbose, overwrite, silent

maxHOLZ = 0
pre = cmplx(0.D0,1.D0)*cPi	! i times pi
czero = cmplx(0.0,0.0)

! first we initialize the gamma' structure and dynamical matrix; once
! that is complete, we read the gamma structure and create its dynamical
! matrix, using all the reflections of the gamma' phase instead of the 
! smaller list for the gamma phase... this will produce a lot of zeroes
! in the matrix, but that's not a problem.

! first get the gamma' crystal data and microscope voltage
nullify(cell, gammacell, gammapcell)
!allocate(cell)        

verbose = .TRUE.
overwrite = .TRUE.
silent = .TRUE.
call Initialize_Cell(cell,Dyn,rlp,enl%gammapname, enl%dmin, enl%voltage, verbose)
gammapcell => cell

! set the foil normal 
Dyn%FN = float(enl%fn)
call NormVec(gammapcell, Dyn%FN, 'd')
thick(1) = enl%thick

! determine the point group number
 j=0
 do i=1,32
  if (SGPG(i).le.cell % SYM_SGnum) j=i
 end do

! use the new routine to get the whole pattern 2D symmetry group, since that
! is the one that determines the independent beam directions.
 dgn = GetPatternSymmetry(enl%k,j,.TRUE.)
 pgnum = j
 isym = WPPG(dgn) ! WPPG lists the whole pattern point group numbers vs. diffraction group numbers

! determine the shortest reciprocal lattice points for this zone
 call ShortestG(enl%k,ga,gb,isym)
 io_int(1:3)=ga(1:3)
 io_int(4:6)=gb(1:3)
 call WriteValue(' Reciprocal lattice vectors : ', io_int, 6,"('(',3I3,') and (',3I3,')',/)")

! ===============================
! updated to here; MDG 06/28/17
! ===============================

! initialize the HOLZ geometry type
 call GetHOLZGeometry(float(ga),float(gb),k,fn) 

! construct the list of all possible reflections
 method = 'ALL'
 thetac = convergence/1000.0
 call Compute_ReflectionList(dmin,k,ga,gb,method,.FALSE.,maxHOLZ,thetac)
 galen = CalcLength(float(ga),'r')

! determine range of incident beam directions
 bragg = CalcDiffAngle(ga(1),ga(2),ga(3))*0.5
  
! convert to ktmax along ga
 ktmax = 0.5*thetac/bragg

! the number of pixels across the disk is equal to 2*npix + 1
 npx = npix
 npy = npx
 io_int(1) = 2.0*npx + 1
 call WriteValue('Number of image pixels along edge of output images = ', io_int, 1, "(I4)")
 mess=' '; call Message("(A/)")
  
! set parameters for wave vector computation
! klaue = (/ 0.0, 0.0 /)
 ijmax = 2.0*float(npx)**2   ! truncation value for beam directions

! there's no point in using symmetry here because the microstructure file does not have any symmetry.
! isym = 1
! call Calckvectors(dble(k),dble(ga),dble(ktmax),npx,npy,numk,isym,ijmax,'Standard',.FALSE.)

! instead of generating a list of k-vectors, we only need a single one, possibly with a Laue tilt component
! compute geometrical factors 
 glen = CalcLength(float(ga),'r')              	! length of ga
 gan = ga/glen                                 	! normalized ga
 call TransSpace(dble(k),kstar,'d','r')       		! transform incident direction to reciprocal space
 call CalcCross(dble(ga),kstar,gperp,'r','r',0)      	! compute g_perp = ga x k
 call NormVec(gperp,'r')                       	! normalize g_perp
 call NormVec(kstar,'r')                       	! normalize reciprocal beam vector

! allocate the head and tail of the linked list
 allocate(khead,stat=istat)   				! allocate new value
 if (istat.ne.0) call FatalError('main program','unable to allocate khead pointer')
 ktail => khead                      			! tail points to new value
 nullify(ktail%next)                			! nullify next in new value
 numk = 1                          			! keep track of number of k-vectors so far

! use the Laue center coordinates to define the tangential component of the incident wave vector
 kt = - klaue(1)*gan - klaue(2)*gperp  		! tangential component of k
 ktail%kt = kt                    			! store tangential component of k
 ktlen = CalcLength(kt,'r')**2      			! squared length of tangential component

 kr = kt + sqrt(1.0/cell%mLambda**2 - ktlen)*kstar 	! complete wave vector
 ktail%k = kr                     			! store in pointer list
 ktail%kn = CalcDot(ktail%k,kstar,'r')    		! normal component of k


! we'll have to use a special routine to create the dynamical matrix; no need to use Bethe potentials
! at this point in time.  We need to do this for every incident beam direction, so we need to save
! the gamma' LUT table as well as the gamma LUT.  The gamma DynMat must have the same size as that 
! of the gamma' phase, to allow for phase overlap and continuity at the phase boundaries.
! The master list is easily created by brute force
write (*,*) 'prefactor = ',pre

 sLUT = shape(LUT)
 imh = (sLUT(1)-1)/2
 imk = (sLUT(2)-1)/2
 iml = (sLUT(3)-1)/2
! deallocate(LUT)
 allocate(gammapLUT(-imh:imh,-imk:imk,-iml:iml),stat=istat)
 do ix=-imh,imh
  do iy=-imk,imk
   do iz=-iml,iml
     call CalcUcg( (/ ix, iy, iz /) ) ! compute potential Fourier coefficient
     gammapLUT(ix, iy, iz) = pre*rlp%qg
   end do 
  end do 
 end do      
 gammapNBeams = DynNbeamsLinked

! in addition we need to keep the reflection linked lists for both phases, so we'll assign the 
! head of the reflection list for the gamma' phase to the pointer gammapreflist 
 gammapreflist => reflist%next 
 deallocate(reflist)
 gammapcell = cell
! get the absorption coefficient
 call CalcUcg( (/0,0,0/) )
 DUgp = pre*rlp%qg

! this is really all we need to keep for the gamma' phase

! ===================================================

! next, we load the gamma phase crystal structure and compute its list of reflections and LUT
! we might need to merge the two lists into a special two-phase structure for efficiency
 call ResetData   ! this resets all the structure items to zero before loading the new structure file
 SG%SYM_reduce=.TRUE.
 call CrystalData(gammaname)

! generate all atom positions
 call CalcPositions('v')
 
! here we need to create a new LUT, using the list of existing reflections
 allocate(gammaLUT(-imh:imh,-imk:imk,-iml:iml),stat=istat)
! we need to use all the reflections from the gamma prime list to compute the LUT entries for the gamma phase
 do ix=-imh,imh
  do iy=-imk,imk
   do iz=-iml,iml
     call CalcUcg( (/ ix, iy, iz /) ) ! compute potential Fourier coefficient
     gammaLUT(ix, iy, iz) = pre*rlp%qg
   end do 
  end do 
 end do      
 gammacell = cell
! get the absorption coefficient
 call CalcUcg( (/0,0,0/) )
 DUg = pre*rlp%qg

!write (*,*) 'prefactor = ',pre
!write (*,*) DUgp, gammapLUT(0,0,0), gammapLUT(1,0,0), gammapLUT(2,0,0)
!write (*,*) DUg, gammaLUT(0,0,0), gammaLUT(1,0,0), gammaLUT(2,0,0)

! and reset the number of linked beams to that in the gammapreflist
 DynNbeamsLinked = gammapNBeams

! ===================================================

! ok, so now we have two LUTs, one for gamma, one for gamma', and two linked lists of reflections.
! next we need to read the microstructure file, which has 0 for gamma and 1 for gamma' along with
! scale factors for the x, y, and z directions.  x lines up with the horizontal image direction
! (we can do more detailed foil geometry stuff at a later time so for now the foil is parallel and horizontal).
!
! Note that the dimensions of the microstructure file are likely different from those of the output
! data set.  So we will need a kind of interpolation routine between the image/foil reference frame 
! and that of the microstructure (i.e., an independent scale parameter).  We do need to sample the 
! microstructure along the beam direction lines for each image sampling point.
!
 open(unit=dataunit,file=trim(microfile),status='old',form='unformatted')
 read (dataunit) dimx, dimy, dimz
 read (dataunit) xstep, ystep, zstep
 allocate(microstructure(-npix:npix,-npix:npix, dimz ),stat=istat)
 read (dataunit) microstructure
 close(unit=dataunit,status='keep')
 mess = 'Loaded the microstructure file '//trim(microfile)
 call Message("(A)")

! do we allow for translational gamma' variants?  We really should to have meaningful results...
 if (variants) then  
   open(unit=dataunit,file=trim(variantfile),status='old',form='unformatted')
   read (dataunit) vdimx, vdimy, vdimz
   if (sum( abs( (/ dimx, dimy, dimz /) - (/vdimx, vdimy, vdimz /) ) ) .ne. 0) then
     call FatalError('GAMMAImage',' inconsistent dimensions in variant and microstructure files')
   end if
   write (*,*) dimx, dimy, dimz, vdimx, vdimy, vdimz
   allocate(variantnumber(-npix:npix,-npix:npix, dimz ),stat=istat)
   read (dataunit) variantnumber
   close(unit=dataunit,status='keep')
   mess = 'Loaded the variant identification file '//trim(variantfile)
   call Message("(A)")
 end if
! ===================================================

! we're done with the initializations; let's start the image computation.
! we need to first determine how many reflections we're going to store in
! in the output file (to be replaced later with BF/HAADF signals).
! we'll keep all reflections with |g| <= |g_{220}| for the L1_2 phase.
 cell = gammapcell
 gmax = CalcLength( (/ 2.0, 2.0, 0.0 /),'r')
! go through the gammapreflist and count/tag the ones that satisfy this condition   
 rltmpa  => gammapreflist
 rltmpa%famnum = 1	! we'll use this field to identify the reflections that will be in the output file
 numg = 1
 write (*,*) numg, rltmpa%hkl(1),rltmpa%hkl(2),rltmpa%hkl(3)
 rltmpa => rltmpa%next
 do
   if (.not.associated(rltmpa)) EXIT
   gcur = CalcLength( float(rltmpa%hkl), 'r')
   if (gcur.le.gmax) then
     numg = numg+1
     rltmpa%famnum = numg
     write (*,*) numg, rltmpa%hkl(1),rltmpa%hkl(2),rltmpa%hkl(3)
   else
     rltmpa%famnum = 0
   end if
   rltmpa => rltmpa%next 
 end do

! now we can allocate the output image array
  allocate(images(-npix:npix,-npix:npix,1:numg),stat=istat)
  images=0.0
write (*,*) 'shape images', shape(images)

! ===================================================

! print the number of incident wave vectors that need to be considered
  io_int(1)=numk
  call WriteValue('Starting computation for # beam directions = ', io_int, 1, "(I8)")

! time the computation
  cnt = 0
  call system_clock(cnt,count_rate,count_max)

! ================ main image loop =============
reflist => gammapreflist
z0 = zstep ! step size for scattering matrix
frac = 0.05

! point to the first beam direction (this is really the first pixel in the image in the present implementation)
  ktmp => khead

 ! call Compute_GGp_DynMats(gammaLUT, gammapLUT, imh, imk, iml, ktmp%k, ktmp%kt, variants)

  call Compute_GGp_DynMats_Bethe(gammaLUT, gammapLUT, imh, imk, iml, ktmp%k, ktmp%kt, variants)
  sdm = shape(DynMat)
!write (*,*) 'return form DynMat'

! compute the scattering matrices for both phases and for a slice thickness of 1 nm

	if (variants) then  ! allocate scattering matrices for all 4 gamma-prime variants 
	  if (allocated(ScatMat)) deallocate(ScatMat)
	  if (allocated(ScatMat0)) deallocate(ScatMat0)
	  if (allocated(ScatMat1)) deallocate(ScatMat1)
	  if (allocated(ScatMat2)) deallocate(ScatMat2)
	  if (allocated(ScatMat3)) deallocate(ScatMat3)
  	  allocate(ScatMat(sdm(1),sdm(2)),ScatMat0(sdm(1),sdm(2)),ScatMat1(sdm(1),sdm(2)), &
		   ScatMat2(sdm(1),sdm(2)),ScatMat3(sdm(1),sdm(2)))
	  ScatMat = cmplx(0.0,0.0)
	  ScatMat0 = cmplx(0.0,0.0)
	  ScatMat1 = cmplx(0.0,0.0)
	  ScatMat2 = cmplx(0.0,0.0)
	  ScatMat3 = cmplx(0.0,0.0)
	else ! no variants, just two different scattering matrices
	  if (allocated(ScatMat)) deallocate(ScatMat)
	  if (allocated(ScatMat0)) deallocate(ScatMat0)
 	  allocate(ScatMat(sdm(1),sdm(2)),ScatMat0(sdm(1),sdm(2)))
	  ScatMat = cmplx(0.0,0.0)
	  ScatMat0 = cmplx(0.0,0.0)
	end if

! and perform the exponentiations
	call MatrixExponential(DynMat, ScatMat, z0, 'Pade', sdm(1))
	call MatrixExponential(DynMat0, ScatMat0, z0, 'Pade', sdm(1))
	if (variants) then 
	  call MatrixExponential(DynMat1, ScatMat1, z0, 'Pade', sdm(1))
	  call MatrixExponential(DynMat2, ScatMat2, z0, 'Pade', sdm(1))
	  call MatrixExponential(DynMat3, ScatMat3, z0, 'Pade', sdm(1))
	end if


! allocate the intensity array 
	allocate(inten(DynNbeams), amp(DynNbeams))
  	inten = 0.0

! then iterate through the microstructure array, selecting the appropriate scattering matrix 
! for each point.  This requires the equation of the line along the current beam direction. 
! for now, we'll simply go straight down the column; we'll assume that z0 = zstep as well.
     do ip=-npx,npx
      do jp=-npy,npy
	amp = cmplx(0.D0,0.D0)
	amp(1) = cmplx(1.D0,0.D0)
	if (variants) then 
	  do iz=1,dimz 
	    select case (variantnumber(ip,jp,iz))
	      case (0) 
		amp = matmul( ScatMat, amp )
	      case (1) 
		amp = matmul( (1.0-microstructure(ip,jp,iz))*ScatMat + microstructure(ip,jp,iz)*ScatMat0, amp )
	      case (2) 
		amp = matmul( (1.0-microstructure(ip,jp,iz))*ScatMat + microstructure(ip,jp,iz)*ScatMat1, amp )
	      case (3) 
		amp = matmul( (1.0-microstructure(ip,jp,iz))*ScatMat + microstructure(ip,jp,iz)*ScatMat2, amp )
	      case (4) 
		amp = matmul( (1.0-microstructure(ip,jp,iz))*ScatMat + microstructure(ip,jp,iz)*ScatMat3, amp )
	      case default
		i=0
	    end select
 	  end do
	else
	  do iz=1,dimz 
	    amp = matmul( (1.0-microstructure(ip,jp,iz))*ScatMat + microstructure(ip,jp,iz)*ScatMat0, amp )
 	  end do
 	end if
 	inten = abs(amp)**2 ! PGC cabs -> abs

!write (*,*) ik, maxval(cabs(ScatMat)),maxval(cabs(DynMat)), maxval(inten), DynNbeams

! ok, we have all the intensities.  Next we need to copy the relevant intensities into the slots 
! of the disk array, one for each required reflection.
	rltmpa => reflist
	do i=1,DynNbeamsLinked
	  if (BetheParameter%stronglist(i).ne.0) then ! is this a reflection on the current list
 	    if (BetheParameter%reflistindex(i).gt.0) then
              images(ip,jp,BetheParameter%reflistindex(i)) = inten(BetheParameter%stronglist(i))
	    end if
	  end if
	  rltmpa => rltmpa%next
	end do


! update computation progress
           if (float(ip)/float(npx) .gt. frac) then
            io_int(1) = nint(100.0*frac) 
            call WriteValue('       ', io_int, 1, "(1x,I3,' percent completed')") 
            frac = frac + 0.05
            open(unit=dataunit,file=trim(outname),status='unknown',action='write',form='unformatted')
            write (dataunit) dimx, dimy, numg
            write (dataunit) images
            close(UNIT=dataunit,STATUS='keep')
           end if  
           
    end do 
   end do

! stop the clock and report the total time     
  call system_clock(newcount,count_rate,count_max)
  io_real(1)=float(newcount-cnt)/float(count_rate)
  mess = ' Program run completed '; call Message("(/A/)")
  call WriteValue('Total computation time [s] ' , io_real, 1, "(F)")

! the final bit of the program involves dumping all the results into a file,
! binary for now, but HDF5 in the future, for the IDL visualization program 
! to read.
  open(unit=dataunit,file=trim(outname),status='unknown',action='write',form='unformatted')
  write (dataunit) dimx, dimy, numg
  write (dataunit) images
  close(UNIT=dataunit,STATUS='keep')

  mess = ' Data stored in '//outname; call Message("(/A/)") 

end subroutine GAMMAimage


!--------------------------------------------------------------------------
!
! SUBROUTINE:Compute_GGp_DynMats_Bethe
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Computes the dynamical matrices for both phases, including the Bethe potentials
!
!> @details This is a reduced and adapted version of the Compute_DynMat routine. 
!
!> @param gammaLUT reflection lookup table for gamma phase
!> @param gammapLUT reflection lookup table for gamma' phase
!> @param imh h dimension parameter for gammaLUT
!> @param imk k dimension parameter for gammaLUT
!> @param iml l dimension parameter for gammaLUT
!> @param kk incident wave vector
!> @param kt tangential component of incident wave vector
!> @param variants logical to turn on APB variants for gamma' phase
!
!> @date 11/02/13  MDG 1.0 original
!> @date 11/05/13  MDG 1.1 added variant handling
!> @date 11/14/13  MDG 2.0 added Bethe potentials (first try)
!--------------------------------------------------------------------------
subroutine Compute_GGp_DynMats_Bethe(gammaLUT, gammapLUT, imh, imk, iml, kk, kt, variants)


use local
use dynamical
use error
use constants
use crystal
use diffraction
use io
use gvectors

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)		:: gammaLUT(-imh:imh,-imk:imk,-iml:iml)
complex(kind=dbl),INTENT(IN)		:: gammapLUT(-imh:imh,-imk:imk,-iml:iml)
integer(kind=irg),INTENT(IN)		:: imh, imk, iml
real(kind=dbl),INTENT(IN)		:: kk(3),kt(3)		!< incident wave vector and tangential component
logical,INTENT(IN)			:: variants

complex(kind=dbl)  			:: czero,pref, pre2, weaksum, ughp, uhph, ep, em
integer(kind=irg) 		 	:: istat,ir,ic,nn, iweak, istrong, iw, ig, ll(3), gh(3), nnn, nweak, i
real(kind=sgl)     			:: glen,exer,gg(3), kpg(3), gplen, sgp, lUg, cut1, cut2
real(kind=dbl)				:: lsfour, weaksgsum, dgg(3), arg, R1(3), R2(3), R3(3), tpi, dp
logical					:: AddSecondOrder

complex(kind=dbl),allocatable		:: testDynMat(:,:)

! has the list of reflections been allocated ?
if (.not.associated(reflist)) call FatalError('Compute_GGp_DynMats',' reflection list has not been allocated')

! if the dynamical matrices have already been allocated, deallocate them first
! this is partially so that no program will allocate DynMats itself; it must be done
! via this routine only.
if (variants) then
if (allocated(DynMat)) deallocate(DynMat)
if (allocated(DynMat0)) deallocate(DynMat0)
if (allocated(DynMat1)) deallocate(DynMat1)
if (allocated(DynMat2)) deallocate(DynMat2)
if (allocated(DynMat3)) deallocate(DynMat3)
else
  if (allocated(DynMat)) deallocate(DynMat)
  if (allocated(DynMat0)) deallocate(DynMat0)
end if 

! initialize some parameters
pref = cmplx(0.D0,1.D0)*cPi	! i times pi
pre2 = cmplx(0.5D0,0.D0)/cPi	! 1/(2pi)
czero = cmplx(0.0,0.0)	! complex zero

! we don't know yet how many strong reflections there are so we'll need to determine this first
! this number depends on some externally supplied parameters, which we will get from a namelist
! file (which should be read only once by the Set_Bethe_Parameters routine), or from default values
! if there is no namelist file in the folder.
if (BetheParameter%cutoff.eq.0.0) call Set_Bethe_Parameters(.TRUE.)

! reset the value of DynNbeams in case it was modified in a previous call 
DynNbeams = DynNbeamsLinked
  	
if (.not.allocated(BetheParameter%stronglist)) allocate(BetheParameter%stronglist(DynNbeams))
if (.not.allocated(BetheParameter%reflistindex)) allocate(BetheParameter%reflistindex(DynNbeams))
if (.not.allocated(BetheParameter%weaklist)) allocate(BetheParameter%weaklist(DynNbeams))
if (.not.allocated(BetheParameter%weakreflistindex)) allocate(BetheParameter%weakreflistindex(DynNbeams))

BetheParameter%stronglist = 0
BetheParameter%reflistindex = 0
BetheParameter%weaklist = 0
BetheParameter%weakreflistindex = 0

    rltmpa => reflist

!write (*,*) 'Starting reflectionloop'

! deal with the transmitted beam first
    nn = 1		! nn counts all the scattered beams that satisfy the cutoff condition
    rltmpa%sg = 0.D0    
    nnn = 1		! nnn counts only the strong beams
    nweak = 0		! counts only the weak beams
    BetheParameter%stronglist(nn) = 1   ! make sure that the transmitted beam is always a strong beam ...
    BetheParameter%weaklist(nn) = 0
    BetheParameter%reflistindex(nn) = 1

! loop over all reflections in the linked list    
    rltmpa => rltmpa%next
    reflectionloop: do ig=2,DynNbeamsLinked
      gg = float(rltmpa%hkl)        		! this is the reciprocal lattice vector 
      rltmpa%sg = Calcsg(gg,sngl(kk),DynFN)

! use the reflection num entry to indicate whether or not this
! reflection should be used for the dynamical matrix
! We compare |sg| with a multiple of lambda |Ug|
!
!  |sg|>cutoff lambda |Ug|   ->  don't count reflection
!  cutoff lambda |Ug| > |sg|  -> strong reflection
!
      sgp = abs(rltmpa%sg) 
      lUg = abs(rltmpa%Ucg) * cell%mLambda ! PGC cabs -> abs
      cut1 = BetheParameter%cutoff * lUg
      cut2 = BetheParameter%weakcutoff * lUg

      if (sgp.le.cut1) then  ! count this beam
	nn = nn+1
! is this a weak or a strong reflection (in terms of Bethe potentials)? 
             	if (sgp.le.cut2) then ! it's a strong beam
              		nnn = nnn+1
	      		BetheParameter%stronglist(ig) = nnn
	                if (rltmpa%famnum.ne.0) then
	                  BetheParameter%reflistindex(ig) = rltmpa%famnum
	                else
	                  BetheParameter%reflistindex(ig) = -1
	                end if
             	else ! it's a weak beam
              		nweak = nweak+1
              		BetheParameter%weaklist(ig) = 1
              		BetheParameter%weakreflistindex(ig) = nweak
             	end if
      end if
! go to the next beam in the list
      rltmpa => rltmpa%next
    end do reflectionloop
!write (*,*) 'Completed reflectionloop; # strong beams = ',nn

! if we don't have any beams in this list (unlikely, but possible if the cutoff
! parameter has an unreasonable value) then we abort the run
! and we report some numbers to the user 
if (nnn.eq.0) then
   mess = ' no beams found for the following parameters:'; call Message("(A)")
   write (stdout,*) ' wave vector = ',kk,'  -> number of beams = ',nn
   mess =  '   -> check cutoff and weakcutoff parameters for reasonableness'; call Message("(A)")
   call FatalError('Compute_GGp_DynMats','No beams in list')
end if

! next, we define nns to be the number of strong beams.
BetheParameter%nns = nnn
BetheParameter%nnw = sum(BetheParameter%weaklist)

! add nns to the weakreflistindex to offset it; not sure if this will be needed here...
do ig=2,DynNbeamsLinked
  if (BetheParameter%weakreflistindex(ig).ne.0) then
    BetheParameter%weakreflistindex(ig) = BetheParameter%weakreflistindex(ig) + BetheParameter%nns
  end if
end do

! allocate arrays for strong beam information
if (allocated(BetheParameter%stronghkl)) deallocate(BetheParameter%stronghkl)
if (allocated(BetheParameter%strongsg)) deallocate(BetheParameter%strongsg)
if (allocated(BetheParameter%strongID)) deallocate(BetheParameter%strongID)
if (allocated(BetheParameter%weakhkl)) deallocate(BetheParameter%weakhkl)
if (allocated(BetheParameter%weaksg)) deallocate(BetheParameter%weaksg)
allocate(BetheParameter%weakhkl(3,BetheParameter%nnw),BetheParameter%weaksg(BetheParameter%nnw))
allocate(BetheParameter%stronghkl(3,BetheParameter%nns),BetheParameter%strongsg(BetheParameter%nns))
allocate(BetheParameter%strongID(BetheParameter%nns))

BetheParameter%stronghkl = 0
BetheParameter%strongsg = 0.0
BetheParameter%strongID = 0


!write (*,*) 'starting ir loop'
! here's where we extract the relevant information from the linked list (much faster
! than traversing the list each time...)
rltmpa => reflist    ! reset the a list
iweak = 0
istrong = 0
do ir=1,DynNbeamsLinked
     if (BetheParameter%weaklist(ir).eq.1) then
        iweak = iweak+1
        BetheParameter%weakhkl(1:3,iweak) = rltmpa%hkl(1:3)
        BetheParameter%weaksg(iweak) = rltmpa%sg
     end if
     if (BetheParameter%stronglist(ir).gt.0) then
        istrong = istrong+1
        BetheParameter%stronghkl(1:3,istrong) = rltmpa%hkl(1:3)
        BetheParameter%strongsg(istrong) = rltmpa%sg
! make an inverse index list
	BetheParameter%strongID(istrong) = ir		
!	write (*,*) ir, (rltmpa%hkl(i),i=1,3), rltmpa%sg, istrong
     end if
   rltmpa => rltmpa%next
end do

! now we are ready to create the dynamical matrix
DynNbeams = BetheParameter%nns

! allocate DynMat and DynMat0 and set to complex zero
allocate(DynMat(DynNbeams,DynNbeams),stat=istat)
allocate(DynMat0(DynNbeams,DynNbeams),stat=istat)

DynMat = czero	! this is for the disordered fcc phase
DynMat0 = czero	! this is for the ordered gamma' phase

! ic is the column index
do ic=1,BetheParameter%nns
! ir is the row index
  do ir=1,BetheParameter%nns
! compute the Fourier coefficient of the electrostatic lattice potential 
    if (ic.ne.ir) then  ! not a diagonal entry
      ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%stronghkl(1:3,ic)
      DynMat(ir,ic) = gammaLUT(ll(1),ll(2),ll(3)) 
! and subtract from this the total contribution of the weak beams
      weaksum = czero
      do iw=1,BetheParameter%nnw
         ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%weakhkl(1:3,iw)
         ughp = gammaLUT(ll(1),ll(2),ll(3)) 
         ll = BetheParameter%weakhkl(1:3,iw) - BetheParameter%stronghkl(1:3,ic)
         uhph = gammaLUT(ll(1),ll(2),ll(3)) 
         if (abs(ughp*uhph).ne.0.D0) then ! PGC cabs -> abs 
!           weaksum = weaksum +  ughp * uhph *cmplx(1.D0/(BetheParameter%weaksg(iw)-BetheParameter%strongsg(ic)),0.0,dbl)
           weaksum = weaksum +  ughp * uhph *cmplx(1.D0/BetheParameter%weaksg(iw),0.0,dbl)
         end if
      end do
! and correct the dynamical matrix element to become a Bethe potential coefficient
      DynMat(ir,ic) = DynMat(ir,ic) - pre2*weaksum

! next we do the gammap dynamical matrix
      DynMat0(ir,ic) = gammapLUT(ll(1),ll(2),ll(3)) 
! and subtract from this the total contribution of the weak beams
      weaksum = czero
      do iw=1,BetheParameter%nnw
         ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%weakhkl(1:3,iw)
         ughp = gammapLUT(ll(1),ll(2),ll(3)) 
         ll = BetheParameter%weakhkl(1:3,iw) - BetheParameter%stronghkl(1:3,ic)
         uhph = gammapLUT(ll(1),ll(2),ll(3)) 
         if (abs(ughp*uhph).ne.0.D0) then ! PGC cabs -> abs 
!           weaksum = weaksum +  ughp * uhph *cmplx(1.D0/(BetheParameter%weaksg(iw)-BetheParameter%strongsg(ic)),0.0,dbl)
           weaksum = weaksum +  ughp * uhph *cmplx(1.D0/BetheParameter%weaksg(iw),0.0,dbl)
         end if
      end do
! and correct the dynamical matrix element to become a Bethe potential coefficient
      DynMat0(ir,ic) = DynMat0(ir,ic) - pre2*weaksum

    else  ! it is a diagonal entry, so we need the excitation error and the absorption length	
      DynMat(ir,ir) = cmplx(0.0,2.D0*cPi*BetheParameter%strongsg(ir),dbl)+gammaLUT(0,0,0)      
! determine the total contribution of the weak beams
      weaksgsum = 0.D0
      do iw=1,BetheParameter%nnw
         ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%weakhkl(1:3,iw)
         ughp = gammaLUT(ll(1),ll(2),ll(3)) 
         if (abs(ughp).ne.0.D0) then ! PGC cabs -> abs 
!           weaksgsum = weaksgsum +  cabs(ughp)**2/(BetheParameter%weaksg(iw)-BetheParameter%strongsg(ir))
           weaksgsum = weaksgsum +  abs(ughp)**2/BetheParameter%weaksg(iw) ! PGC cabs -> abs
         end if
      end do
      DynMat(ir,ir) = DynMat(ir,ir) - pre2*weaksgsum
      
      DynMat0(ir,ir) = cmplx(0.0,2.D0*cPi*BetheParameter%strongsg(ir),dbl)+gammapLUT(0,0,0)
! determine the total contribution of the weak beams
      weaksgsum = 0.D0
      do iw=1,BetheParameter%nnw
         ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%weakhkl(1:3,iw)
         ughp = gammapLUT(ll(1),ll(2),ll(3)) 
         if (abs(ughp).ne.0.D0) then ! PGC cabs -> abs 
!           weaksgsum = weaksgsum +  cabs(ughp)**2/(BetheParameter%weaksg(iw)-BetheParameter%strongsg(ir))
           weaksgsum = weaksgsum +  abs(ughp)**2/BetheParameter%weaksg(iw) ! PGC cabs -> abs 
         end if
      end do
      DynMat0(ir,ir) = DynMat0(ir,ir) - pre2*weaksgsum

    end if
  end do
end do
! that should do it for the initialization of the dynamical matrices if there are no translation variants

if (variants) then ! allocate and adjust the variant dynamical matrices
  allocate(DynMat1(DynNbeams,DynNbeams),stat=istat)
  allocate(DynMat2(DynNbeams,DynNbeams),stat=istat)
  allocate(DynMat3(DynNbeams,DynNbeams),stat=istat)
  DynMat1 = DynMat0	! this is for the ordered gamma' phase variant 1
  DynMat2 = DynMat0	! this is for the ordered gamma' phase variant 2
  DynMat3 = DynMat0	! this is for the ordered gamma' phase variant 3

! next we need to go through each matrix again and for the off-diagonal elements
! multiply the entry with the correct APB phase factor.  
  R1 = (/ 0.5D0, 0.5D0 ,0.0D0 /)
  R2 = (/ 0.5D0, 0.0D0 ,0.5D0 /)
  R3 = (/ 0.0D0, 0.5D0 ,0.5D0 /)
! for APBs in L1_2 there are only three possibilities for the phase factor: 1, exp(i pi) and exp(-i pi)  
! so we'll pre-compute the possible phase shifts for the superlattice reflections as ep and em:
  ep = cmplx(dcos(cPi),-dsin(cPi))
  em = cmplx(dcos(cPi),dsin(cPi))
! ic is the column index
  do ic=1,BetheParameter%nns
! ir is the row index
    do ir=1,BetheParameter%nns
! compute the Fourier coefficient of the electrostatic lattice potential 
      if (ic.ne.ir) then  ! not a diagonal entry
        dgg = dble(BetheParameter%stronghkl(1:3,ir) - BetheParameter%stronghkl(1:3,ic))
	dp = dot_product(dgg,R1)
	arg = dmod(dp,1.D0)
	if (dabs(arg).ne.0.D0) then
!	  arg = 2.D0*( 1.D0 - dmod(dp,2.D0) )
!	  if (arg.lt.0.D0) then 
	    DynMat1(ir,ic) = - DynMat1(ir,ic) !* ep
!	  else
!	    DynMat1(ir,ic) = DynMat1(ir,ic) * em
!	  end if
	end if
	dp = dot_product(dgg,R2)
	arg = dmod(dp,1.D0)
	if (dabs(arg).ne.0.D0) then
!	  arg = 2.D0*( 1.D0 - dmod(dp,2.D0) )
!	  if (arg.lt.0.D0) then 
	    DynMat2(ir,ic) = - DynMat2(ir,ic) !* ep
!	write (*,*) dgg, dp, arg, ep
!	  else
!	    DynMat2(ir,ic) = DynMat2(ir,ic) * em
!	write (*,*) dgg, dp, arg, em
!	  end if
	end if
	dp = dot_product(dgg,R3)
	arg = dmod(dp,1.D0)
	if (dabs(arg).ne.0.D0) then
!	  arg = 2.D0*( 1.D0 - dmod(dp,2.D0) )
!	  if (arg.lt.0.D0) then 
	    DynMat3(ir,ic) = - DynMat3(ir,ic) !* ep
!	  else
!	    DynMat3(ir,ic) = DynMat3(ir,ic) * em
!	  end if
	end if
      end if
    end do
  end do
end if

write (*,*) ' # of beams (strong/weak) ',BetheParameter%nns,BetheParameter%nnw

!  open(unit=dataunit,file='arrays.data',status='unknown',form='unformatted')
!  write (*,*) 'dimension = ',shape(DynMat)
!  write(dataunit) testDynMat
!  write(dataunit) DynMat
!  write(dataunit) DynMat2
!  close(unit=dataunit,status='keep')
!
end subroutine Compute_GGp_DynMats_Bethe









!--------------------------------------------------------------------------
!
! SUBROUTINE:Compute_GGp_DynMats
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Computes the dynamical matrices for both phases
!
!> @details This is a reduced and adapted version of the Compute_DynMat routine. 
!
!> @param gammaLUT reflection lookup table for gamma phase
!> @param gammapLUT reflection lookup table for gamma' phase
!> @param imh h dimension parameter for gammaLUT
!> @param imk k dimension parameter for gammaLUT
!> @param iml l dimension parameter for gammaLUT
!> @param kk incident wave vector
!> @param kt tangential component of incident wave vector
!> @param variants logical to turn on APB variants for gamma' phase
!
!> @date 11/02/13  MDG 1.0 original
!> @date 11/05/13  MDG 1.1 added variant handling
!--------------------------------------------------------------------------
subroutine Compute_GGp_DynMats(gammaLUT, gammapLUT, imh, imk, iml, kk, kt, variants)


use local
use dynamical
use error
use constants
use crystal
use diffraction
use io
use gvectors

IMPLICIT NONE

complex(kind=dbl),INTENT(IN)		:: gammaLUT(-imh:imh,-imk:imk,-iml:iml)
complex(kind=dbl),INTENT(IN)		:: gammapLUT(-imh:imh,-imk:imk,-iml:iml)
integer(kind=irg),INTENT(IN)		:: imh, imk, iml
real(kind=dbl),INTENT(IN)		:: kk(3),kt(3)		!< incident wave vector and tangential component
logical,INTENT(IN)			:: variants

complex(kind=dbl)  			:: czero,pref, weaksum, ughp, uhph, ep, em
integer(kind=irg) 		 	:: istat,ir,ic,nn, iweak, istrong, iw, ig, ll(3), gh(3), nnn, nweak, i
real(kind=sgl)     			:: glen,exer,gg(3), kpg(3), gplen, sgp, lUg, cut1, cut2
real(kind=dbl)				:: lsfour, weaksgsum, dgg(3), arg, R1(3), R2(3), R3(3), tpi, dp
logical					:: AddSecondOrder

complex(kind=dbl),allocatable		:: testDynMat(:,:)

! has the list of reflections been allocated ?
if (.not.associated(reflist)) call FatalError('Compute_GGp_DynMats',' reflection list has not been allocated')

! if the dynamical matrices have already been allocated, deallocate them first
! this is partially so that no program will allocate DynMats itself; it must be done
! via this routine only.
if (variants) then
if (allocated(DynMat)) deallocate(DynMat)
if (allocated(DynMat0)) deallocate(DynMat0)
if (allocated(DynMat1)) deallocate(DynMat1)
if (allocated(DynMat2)) deallocate(DynMat2)
if (allocated(DynMat3)) deallocate(DynMat3)
else
  if (allocated(DynMat)) deallocate(DynMat)
  if (allocated(DynMat0)) deallocate(DynMat0)
end if 

! initialize some parameters
pref = cmplx(0.0,1.0)*cPi	! i times pi
czero = cmplx(0.0,0.0)	! complex zero

! we don't know yet how many strong reflections there are so we'll need to determine this first
! this number depends on some externally supplied parameters, which we will get from a namelist
! file (which should be read only once by the Set_Bethe_Parameters routine), or from default values
! if there is no namelist file in the folder.
if (BetheParameter%cutoff.eq.0.0) call Set_Bethe_Parameters(.TRUE.)
BetheParameter%weakcutoff = BetheParameter%cutoff ! no Bethe potentials in the current approach

! reset the value of DynNbeams in case it was modified in a previous call 
DynNbeams = DynNbeamsLinked
  	
if (.not.allocated(BetheParameter%stronglist)) allocate(BetheParameter%stronglist(DynNbeams))
if (.not.allocated(BetheParameter%reflistindex)) allocate(BetheParameter%reflistindex(DynNbeams))
BetheParameter%stronglist = 0
BetheParameter%reflistindex = 0

rltmpa => reflist

!write (*,*) 'Starting reflectionloop'

! deal with the transmitted beam first
    nn = 1		! nn counts all the scattered beams that satisfy the cutoff condition
    rltmpa%sg = 0.D0    
    BetheParameter%stronglist(1) = 1
    BetheParameter%reflistindex(1) = 1

! loop over all reflections in the linked list    
    rltmpa => rltmpa%next
    reflectionloop: do ig=2,DynNbeamsLinked
      gg = float(rltmpa%hkl)        		! this is the reciprocal lattice vector 
      rltmpa%sg = Calcsg(gg,sngl(kk),DynFN)

! use the reflection num entry to indicate whether or not this
! reflection should be used for the dynamical matrix
! We compare |sg| with a multiple of lambda |Ug|
!
!  |sg|>cutoff lambda |Ug|   ->  don't count reflection
!  cutoff lambda |Ug| > |sg|  -> strong reflection
!
      sgp = abs(rltmpa%sg) 
      lUg = abs(rltmpa%Ucg) * cell%mLambda ! PGC cabs -> abs
      cut1 = BetheParameter%cutoff * lUg

      if (sgp.le.cut1) then  ! count this beam
	nn = nn+1
	BetheParameter%stronglist(ig) = nn
	if (rltmpa%famnum.ne.0) then
	  BetheParameter%reflistindex(ig) = rltmpa%famnum
	else
	  BetheParameter%reflistindex(ig) = -1
	end if
      end if

! go to the next beam in the list
      rltmpa => rltmpa%next
    end do reflectionloop
!write (*,*) 'Completed reflectionloop; # strong beams = ',nn

! if we don't have any beams in this list (unlikely, but possible if the cutoff
! parameter has an unreasonable value) then we abort the run
! and we report some numbers to the user 
if (nn.eq.0) then
   mess = ' no beams found for the following parameters:'; call Message("(A)")
   write (stdout,*) ' wave vector = ',kk,'  -> number of beams = ',nn
   mess =  '   -> check cutoff and weakcutoff parameters for reasonableness'; call Message("(A)")
   call FatalError('Compute_GGp_DynMats','No beams in list')
end if

! next, we define nns to be the number of strong beams.
BetheParameter%nns = nn
	 	
! allocate arrays for strong beam information
if (allocated(BetheParameter%stronghkl)) deallocate(BetheParameter%stronghkl)
if (allocated(BetheParameter%strongsg)) deallocate(BetheParameter%strongsg)
if (allocated(BetheParameter%strongID)) deallocate(BetheParameter%strongID)
allocate(BetheParameter%stronghkl(3,BetheParameter%nns),BetheParameter%strongsg(BetheParameter%nns))
allocate(BetheParameter%strongID(BetheParameter%nns))

BetheParameter%stronghkl = 0
BetheParameter%strongsg = 0.0
BetheParameter%strongID = 0


!write (*,*) 'starting ir loop'
! here's where we extract the relevant information from the linked list (much faster
! than traversing the list each time...)
rltmpa => reflist    ! reset the a list
istrong = 0
do ir=1,DynNbeamsLinked
     if (BetheParameter%stronglist(ir).gt.0) then
        istrong = istrong+1
        BetheParameter%stronghkl(1:3,istrong) = rltmpa%hkl(1:3)
        BetheParameter%strongsg(istrong) = rltmpa%sg
! make an inverse index list
	BetheParameter%strongID(istrong) = ir		
!	write (*,*) ir, (rltmpa%hkl(i),i=1,3), rltmpa%sg, istrong
     end if
   rltmpa => rltmpa%next
end do

!write (*,*) 'completed ir loop'

! now we are ready to create the dynamical matrix
DynNbeams = BetheParameter%nns

! allocate DynMat and DynMat0 and set to complex zero
allocate(DynMat(DynNbeams,DynNbeams),stat=istat)
allocate(DynMat0(DynNbeams,DynNbeams),stat=istat)

DynMat = czero	! this is for the disordered fcc phase
DynMat0 = czero	! this is for the ordered gamma' phase

! ic is the column index
do ic=1,BetheParameter%nns
! ir is the row index
  do ir=1,BetheParameter%nns
! compute the Fourier coefficient of the electrostatic lattice potential 
    if (ic.ne.ir) then  ! not a diagonal entry
      ll = BetheParameter%stronghkl(1:3,ir) - BetheParameter%stronghkl(1:3,ic)
      DynMat(ir,ic) = gammaLUT(ll(1),ll(2),ll(3)) 
      DynMat0(ir,ic) = gammapLUT(ll(1),ll(2),ll(3)) 
    else  ! it is a diagonal entry, so we need the excitation error and the absorption length	
      DynMat(ir,ir) = cmplx(0.0,2.D0*cPi*BetheParameter%strongsg(ir),dbl)+gammaLUT(0,0,0)
      DynMat0(ir,ir) = cmplx(0.0,2.D0*cPi*BetheParameter%strongsg(ir),dbl)+gammapLUT(0,0,0)
    end if
  end do
end do
! that should do it for the initialization of the dynamical matrices if there are no translation variants

if (variants) then ! allocate and adjust the variant dynamical matrices
  allocate(DynMat1(DynNbeams,DynNbeams),stat=istat)
  allocate(DynMat2(DynNbeams,DynNbeams),stat=istat)
  allocate(DynMat3(DynNbeams,DynNbeams),stat=istat)
  DynMat1 = DynMat0	! this is for the ordered gamma' phase variant 1
  DynMat2 = DynMat0	! this is for the ordered gamma' phase variant 2
  DynMat3 = DynMat0	! this is for the ordered gamma' phase variant 3

! next we need to go through each matrix again and for the off-diagonal elements
! multiply the entry with the correct APB phase factor.  
  R1 = (/ 0.5D0, 0.5D0 ,0.0D0 /)
  R2 = (/ 0.5D0, 0.0D0 ,0.5D0 /)
  R3 = (/ 0.0D0, 0.5D0 ,0.5D0 /)
! for APBs in L1_2 there are only three possibilities for the phase factor: 1, exp(i pi) and exp(-i pi)  
! so we'll pre-compute the possible phase shifts for the superlattice reflections as ep and em:
  ep = cmplx(dcos(cPi),-dsin(cPi))
  em = cmplx(dcos(cPi),dsin(cPi))
! ic is the column index
  do ic=1,BetheParameter%nns
! ir is the row index
    do ir=1,BetheParameter%nns
! compute the Fourier coefficient of the electrostatic lattice potential 
      if (ic.ne.ir) then  ! not a diagonal entry
        dgg = dble(BetheParameter%stronghkl(1:3,ir) - BetheParameter%stronghkl(1:3,ic))
	dp = dot_product(dgg,R1)
	arg = dmod(dp,1.D0)
	if (dabs(arg).ne.0.D0) then
	  arg = 2.D0*( 1.D0 - dmod(dp,2.D0) )
	  if (arg.lt.0.D0) then 
	    DynMat1(ir,ic) = DynMat1(ir,ic) * ep
	  else
	    DynMat1(ir,ic) = DynMat1(ir,ic) * em
	  end if
	end if
	dp = dot_product(dgg,R2)
	arg = dmod(dp,1.D0)
	if (dabs(arg).ne.0.D0) then
	  arg = 2.D0*( 1.D0 - dmod(dp,2.D0) )
	  if (arg.lt.0.D0) then 
	    DynMat2(ir,ic) = DynMat2(ir,ic) * ep
	  else
	    DynMat2(ir,ic) = DynMat2(ir,ic) * em
	  end if
	end if
	dp = dot_product(dgg,R3)
	arg = dmod(dp,1.D0)
	if (dabs(arg).ne.0.D0) then
	  arg = 2.D0*( 1.D0 - dmod(dp,2.D0) )
	  if (arg.lt.0.D0) then 
	    DynMat3(ir,ic) = DynMat3(ir,ic) * ep
	  else
	    DynMat3(ir,ic) = DynMat3(ir,ic) * em
	  end if
	end if
      end if
    end do
  end do
end if


!  open(unit=dataunit,file='arrays.data',status='unknown',form='unformatted')
!  write (*,*) 'dimension = ',shape(DynMat)
!  write(dataunit) testDynMat
!  write(dataunit) DynMat
!  write(dataunit) DynMat2
!  close(unit=dataunit,status='keep')
!
end subroutine Compute_GGp_DynMats





