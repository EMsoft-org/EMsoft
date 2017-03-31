! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMMC.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMMC 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Monte Carlo BSE simulation with Lambert projection for data storage
!
!> @detail Monte Carlo Electron Trajectory Simulation for EBSD
!>	This version uses the modified Lambert projection to store 
!>	the MC output data, so that we are not dependent on a
!>	particular detector geometry.  We store the energy and direction 
!>	cosines of a BSE electron along with depth information in the 
!>	Lambert projection array, which needs to be sufficiently fine in
!>	terms of sampling so that we can deal with a detector that's relatively
!>	far away.
!
!> @todo implement more detailed Monte Carlo scheme in addition to CSDA.
!> Also, switch the actual single_run routine to OpenCL and GPU...
!                
!> @date 11/**/12  PGC 1.0 IDL version
!> @date 12/04/12  MDG 1.1 conversion to Fortran-90
!> @date 12/06/12  MDG 1.2 conversion to OpenMP, with new random number generator
!> @date 12/06/12  MDG 1.3 added energy histogram sampling
!> @date 12/07/12  MDG 1.4 added energy vs. depth sampling
!> @date 03/11/13  MDG 2.0 replaced regular storage by modified Lambert projection
!> @date 07/23/13  MDG 3.0 complete rewrite
!> @date 09/25/13  MDG 3.1 modified output file format
!> @date 03/17/14  MDG 3.2 modified output file format for IDL GUI
!> @date 06/19/14  MDG 4.0 converted to remove all globals and split namelist handling from computation
!> @date 09/24/14  MDG 4.1 test version to compare with Saransh's OpenCL code
!--------------------------------------------------------------------------
program EMMC

use local
use files
use NameListTypedefs
use NameListHandlers
use io

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(MCNameListType)                    :: mcnl

nmldeffile = 'EMMC.nml'
progname = 'EMMC.f90'
progdesc = 'Monte Carlo backscattered electron simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 20 /), progname)

! deal with the namelist stuff
call GetMCNameList(nmldeffile,mcnl)

! perform a Monte Carlo simulation
 call DoMCsimulation(mcnl, progname)
 
end program EMMC 
 
!--------------------------------------------------------------------------
!
! SUBROUTINE:DoMCsimulation
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Perform the MC simulation
!
!> @param nmlfile namelist file name
!
!> @date 11/29/01  MDG 1.0 original
!> @date 04/08/13  MDG 2.0 rewrite
!> @date 05/14/13  MDG 2.1 replaced IO by namelist file
!> @date 07/23/13  MDG 3.0 complete rewrite
!> @date 07/30/13  MDG 3.1 added Patrick's code for double sample tilt (sigma, omega)
!> @date 09/25/13  MDG 3.2 added a few parameters to the output file 
!> @date 03/17/14  MDG 3.3 added a few more for the IDL visualization program
!> @date 06/19/14  MDG 4.0 rewrite with name list handling removed
!--------------------------------------------------------------------------
subroutine DoMCsimulation(mcnl, progname)

use local
use typedefs
use NameListTypedefs
use initializers
use initializersHDF
use crystal
use symmetry
use error
use io
use files
use diffraction, only:CalcWaveLength
use rng
use Lambert
use omp_lib

IMPLICIT NONE

type(MCNameListType),INTENT(IN)         :: mcnl
character(fnlen),INTENT(IN)             :: progname


type(unitcell),pointer  :: cell
type(DynType)           :: Dyn
type(gnode)             :: rlp

integer(kind=irg)       :: numsy        ! number of Lambert map points along y
integer(kind=irg)       :: numEbins     ! number of energy bins
integer(kind=irg)       :: numzbins     ! number of depth bins
integer(kind=irg)       :: nel          ! number of electrons per thread
integer(kind=irg)       :: NUMTHREADS   ! number of allocated threads
integer,parameter       :: k12 = selected_int_kind(15)
real(kind=dbl)          :: Ze           ! average atomic number
real(kind=dbl)          :: density      ! density in g/cm^3
real(kind=dbl)          :: at_wt        ! average atomic weight in g/mole
logical                 :: verbose

! variable passing array
real(kind=dbl)          :: varpas(13) 
integer(kind=irg)       :: i, TID, nx, skip, io_int(1)
real(kind=sgl)          :: dens, avA, avZ, io_real(3), dmin ! used with CalcDensity routine

! variables used for parallel random number generator (based on http://http://jblevins.org/log/openmp)
type(rng_t), allocatable :: rngs(:)

! various allocatable arrays, energy histogram is first index, x,y on scintillator 2nd and 3rd indices
integer(kind=irg),allocatable   :: accum_e(:,:,:), acc_e(:,:,:), accum_z(:,:,:,:), acc_z(:,:,:,:)

! various 
integer(kind=irg)       :: istat

 numsy = mcnl%numsx

 nullify(cell)
 allocate(cell)

 verbose = .TRUE.
 dmin = 0.05
 call Initialize_Cell(cell,Dyn,rlp,mcnl%xtalname, dmin, sngl(mcnl%EkeV), verbose)

! then get the density, average atomic number and average atomic weight
 call CalcDensity(cell, dens, avZ, avA)
 density = dble(dens)
 Ze = dble(avZ)
 at_wt = dble(avA)
 io_real(1:3) = (/ dens, avZ, avA /) 
 call WriteValue('Density, avZ, avA = ',io_real,3,"(2f10.5,',',f10.5)")

! allocate the accumulator arrays for number of electrons and energy
 numEbins =  int((mcnl%EkeV-mcnl%Ehistmin)/mcnl%Ebinsize)+1
 numzbins =  int(mcnl%depthmax/mcnl%depthstep)+1
 nx = (mcnl%numsx-1)/2
 allocate(accum_e(numEbins,-nx:nx,-nx:nx),accum_z(numEbins,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)
 allocate(rngs(mcnl%nthreads),stat=istat)

! now put most of these variables in an array to be passed to the single_run subroutine
 varpas = (/ dble(mcnl%sig), dble(mcnl%numsx), dble(numsy), dble(mcnl%num_el), mcnl%EkeV, &
           Ze, density, at_wt, mcnl%Ehistmin, mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep, dble(mcnl%omega)/)

! set the number of OpenMP threads and allocate the corresponding number of random number streams
 io_int(1) = mcnl%nthreads
 call WriteValue(' Attempting to set number of threads to ',io_int,1,"(I4)")
 call OMP_SET_NUM_THREADS(mcnl%nthreads)

! use OpenMP to run on multiple cores ... 
 nel = mcnl%num_el
!$OMP PARALLEL  PRIVATE(i,TID,acc_e,acc_z,istat) &
!$OMP& SHARED(NUMTHREADS,varpas,accum_e,accum_z,nel,numEbins,numzbins)

 NUMTHREADS = OMP_GET_NUM_THREADS()
 TID = OMP_GET_THREAD_NUM()

! allocate memory for the accumulator arrays in each thread
 allocate(acc_e(numEbins,-nx:nx,-nx:nx),acc_z(numEbins,numzbins,-nx/10:nx/10,-nx/10:nx/10),stat=istat)

! each thread gets to execute the entire single_run function just once
!$OMP DO SCHEDULE(STATIC,1)    

 do i=1,NUMTHREADS
! get a unique seed for this thread  (take the primeseed and add the thread ID)
  call rng_seed(rngs(i), mcnl%primeseed + i)

! do the Monte Carlo run  
  call single_run(varpas,rngs(i),acc_e,acc_z,nx,numEbins,numzbins)

! make sure that only one thread copies its contents into the main accumulator arrays at any given time
!$OMP CRITICAL
  accum_e = accum_e + acc_e
  accum_z = accum_z + acc_z
!$OMP END CRITICAL  

 end do
!$OMP END DO

!$OMP END PARALLEL

! and here we create the output file
 call Message(' ',"(A)")
 call Message(' All threads complete; saving data to file '//trim(mcnl%dataname), frm = "(A)")

 io_int(1) = mcnl%num_el*NUMTHREADS
 call WriteValue(' Total number of electrons generated = ',io_int, 1, "(I15)")
 io_int(1) = sum(accum_e)
 call WriteValue(' Number of electrons on detector       = ',io_int, 1, "(I15)")

 open(dataunit,file=trim(EMsoft_toNativePath(mcnl%dataname)),status='unknown',form='unformatted')
! write the program identifier
 write (dataunit) progname
! write the version number
 write (dataunit) EMsoft_getEMsoftversion()
! then the name of the crystal data file
 write (dataunit) mcnl%xtalname
! energy information etc...
 write (dataunit) numEbins, numzbins, mcnl%numsx, numsy, mcnl%num_el*NUMTHREADS ! , NUMTHREADS
 write (dataunit) mcnl%EkeV, mcnl%Ehistmin, mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep
 write (dataunit) mcnl%sig, mcnl%omega
 write (dataunit) mcnl%MCmode
! and here are the actual results
 write (dataunit) accum_e
 write (dataunit) accum_z
 close (dataunit,status='keep')
! this is the end of the main subroutine


! the routine contains one function called single_run
contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:single_run
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief does a full simulation starting from a given random number seed
!
!> @param varpas variable list
!> @param rngt random number identifier
!> @param accum_e energy accumulator array
!> @param accum_z depth accumulator array
!> @param numEbins number of energy bins
!> @param numzbins number of depth bins
!
!> @date 11/**/12  PGC 1.0 IDL version
!> @date 12/04/12  MDG 1.1 conversion to Fortran-90
!> @date 12/05/12  MDG 1.2 created subroutine to run with OpenMP
!> @date 03/11/13  MDG 2.0 modified for Lambert projection
!> @date 07/23/13  MDG 3.0 complete rewrite, integration with EMsoft libraries
!> @date 07/30/13  MDG 3.1 added Patrick's code for tilted sample surface (sigma, omega)
!> @date 07/31/13  MDG 3.2 corrected off-by-one error in energy binning
!--------------------------------------------------------------------------
recursive subroutine  single_run(varpas,rngt,accum_e,accum_z,nx,numEbins,numzbins)

use local
!use rng
use Lambert

! all geometrical parameters for the scintillator setup
real(kind=dbl)          :: sig          ! TD sample tile angle [degrees]
real(kind=dbl)          :: omega        ! RD sample tile angle [degrees]
integer(kind=irg)       :: numsx        ! number of scintillator points along x
integer(kind=irg)       :: numsy        ! number of scintillator points along y
real(kind=dbl)          :: Ehistmin     ! minimum energy for energy histogram (in keV)
real(kind=dbl)          :: Ebinsize     ! binsize in keV
integer(kind=irg),INTENT(IN)    :: numEbins, numzbins
integer(kind=irg),INTENT(IN)    :: nx
real(kind=dbl)          :: Emin         ! Ehistmin - Ebinsize/2
integer(kind=irg)       :: iE           ! energy bin counter
integer(kind=irg)       :: iz           ! exit depth bin counter
real(kind=dbl)          :: depthmax     ! maximum depth for which to keep track of exit energy statistics [in nm]
real(kind=dbl)          :: depthstep    ! stepsize for depth-energy accumulator array [in nm]

! Monte Carlo related parameters
real(kind=dbl)          :: EkeV, Ec                     ! electron energy in keV
real(kind=dbl)          :: scaled = 1.0D8               ! cm to Angstrom scalefactor
real(kind=dbl)          :: min_energy = 0.D0            ! in keV
real(kind=dbl)          :: presig = 1.5273987D19        ! = 1/( 5.21D-21 * (4*cPi) )
real(kind=dbl)          :: xyz(3), xyzn(3)              ! electron coordinates
real(kind=dbl)          :: cxyz(3), cxstart, czstart    ! direction cosines
real(kind=dbl)          :: alpha, psi                   ! angles
real(kind=dbl)          :: lambda, step, pre, sige, prealpha, predEds, delta, dd, dxy(2)        ! stepsize parameters
real(kind=dbl)          :: J, dEds, dE                  ! energy-related variables
real(kind=dbl)          :: cphi, sphi, cpsi, spsi, tpi  ! cosines and sines and such ... 
real(kind=dbl)          :: cxyzp(3), dsq, edis, dsqi, tano, znmax       ! trajectory parameters

integer(kind=irg)       :: idxy(2),px,py, io_int(2)                ! scintillator coordinates

! auxiliary variables
integer(kind=irg)       :: num                  ! number of scattering events to try
integer(kind=irg)       :: bsct                 ! back-scattered electron counter
integer,parameter       :: k12 = selected_int_kind(15)
integer(kind=k12)       :: num_el               ! total number of electrons to try
integer(kind=k12)       :: el                   ! electron counter
integer(kind=irg)       :: traj                 ! trajectory counter
integer(kind=irg)       :: ierr                 ! Lambert projection error status flag
integer(kind=irg)       :: iran                 ! random number counter

! material parameters
real(kind=dbl)          :: Ze                   ! average atomic number
real(kind=dbl)          :: density              ! density in g/cm^3
real(kind=dbl)          :: at_wt                ! average atomic weight in g/mole

! variable passing arrays
real(kind=dbl),INTENT(IN)       :: varpas(13)
integer(kind=irg),INTENT(OUT)   :: accum_e(numEbins,-nx:nx,-nx:nx), accum_z(numEbins,numzbins,-nx/10:nx/10,-nx/10:nx/10)
integer(kind=irg)               :: TID

! parallel random number variable 
type(rng_t), intent(inout)      :: rngt
real(kind=dbl)                  :: rn(1501)
real(kind=dbl)                  :: rr   ! random number

real(kind=dbl), parameter :: cDtoR = 0.017453293D0

  write (*,*) 'entered single_run routine'

! in this test program, we'll only follow one electron for 500 steps; to do so,
! we'll need 1501 random numbers from the RandomSeeds.data file, which we read here:
open(unit=20,file='randomuniform.txt',status='old',form='formatted')
do i=1,1501
  read(20,"(F14.12)") rn(i)
  write (*,*) rn(i)
end do
close(unit=20,status='keep')


! get the current thread number
 TID = OMP_GET_THREAD_NUM()

! initialize all the variables based on the varpas array
 sig = varpas(1)
 numsx = int(varpas(2),kind=irg)
 numsy = int(varpas(3),kind=irg) 
 num_el = 1 ! int(varpas(4),kind=k12)
 EkeV = varpas(5)
 Ze = varpas(6) 
 density = varpas(7) 
 at_wt = varpas(8)
 Ehistmin = varpas(9)
 Ebinsize = varpas(10)
 depthmax = varpas(11)
 depthstep = varpas(12)
 omega = varpas(13)

 Emin = Ehistmin - Ebinsize/2.D0

 Emin = 0.D0
   
! prefactors for mean free path and other computations
 pre =  at_wt/cAvogadro/density
 prealpha = 3.4D-3 * Ze**(0.67) 
 J = (9.76D0 * Ze+58.5D0 / Ze**(0.19D0) )*1.0D-3 / 1.166
 J = 1.D0/J
 predEds = -78500.0D0 * density * Ze / at_wt 

! initialize max number of scattering events along a single trajectory
 num = 500
 tpi = 2.D0 * cPi
 tano = tan(omega * cDtoR)

! parameter for the Lambert projection scaling
 delta = dble(nx) / LPs%sPio2

! beam direction cosines
 cxstart = dcos( (90.D0-sig) * cDtoR)
 czstart = -dsin( (90.D0-sig) * cDtoR)
 
! random number counter
 iran = 1

open(unit=dataunit,file='singlerun.txt',status='unknown',form='formatted')
open(unit=dataunit2,file='singlerun2.txt',status='unknown',form='formatted')

! and here is the main loop
 mainloop: do el = 1,num_el

! every million steps, print something to the screen
    if ((TID.eq.0).and.(mod(el,1000000_k12).eq.0)) then
        io_int(1) = el
        call WriteValue(' Completed electron # ',io_int, 1, "(I15,$)")
        io_int(1) = sum(accum_e)
        call WriteValue('; BSE hits = ',io_int, 1, frm = "(I15)")
    end if

    Ec = EkeV   ! set the initial energy for this incident electron

! these could in principle be sampled from an area corresponding to the beam size
    xyz = (/ 0.D0, 0.D0, 0.D0 /)        ! initial coordinates

! get the mean free path for this energy and scale it by a random number 
    alpha = prealpha / Ec
    step = Ec*(Ec+1024.D0)/Ze/(Ec+511.D0)               ! step is used here as a dummy variable
    sige =  presig * step * step * alpha * (1.D0+alpha)
    lambda = pre * sige
    rr = rn(iran)
    iran = iran+1
    step = - lambda * log(rr)
 
    cxyz = (/ cxstart, 0.D0, czstart /)         ! direction cosines for beam on tilted sample
 
! advance the coordinates 
    xyz = xyz + step * scaled * cxyz

  write (dataunit,"(I6,':',F10.4,',',5(F12.4,','),F12.4)") iran-2, Ec, cxyz(1:3), xyz(1:3)
  write (dataunit2,"(I6,':',E12.4,',',E12.4,',',E12.4,',',E12.4)") iran-2, alpha, step*scaled, sige, lambda*scaled 

  traj = 0
  trajloop: do while (traj.lt.num)
! Subtract the energy that is lost for path length lambda.
    dEds = predEds * dlog( Ec * J + 1.0D0 ) / Ec
    dE = dEds * step
    Ec = Ec+dE

! here we exit the trajloop (using the f90 EXIT command) if the energy becomes low enough
    if (Ec.lt.min_energy) EXIT trajloop
    
!  Find the angle the electron is deflected through by the scattering event.
    rr = rn(iran)  ! rng_uniform(rngt)
    iran = iran+1
    cphi = 1.D0-2.D0*alpha*rr/(1.D0+alpha-rr)
    sphi = dsin(dacos(cphi)) !  dsqrt(1.D0-cphi*cphi)

! Find the azimuthal scattering angle psi
    rr = rn(iran) ! rng_uniform(rngt)
    iran = iran+1
    psi = tpi * rr
    spsi = dsin(psi)
    cpsi = dcos(psi)

! compute the new direction cosines
! From MCML paper START
    if (dabs(cxyz(3)).gt.0.99999D0) then
      cxyzp = (/ sphi * cpsi, sphi * spsi, (cxyz(3)/dabs(cxyz(3))) * cphi /)
    else 
      dsq = dsqrt(1.D0-cxyz(3)*cxyz(3))
      dsqi = 1.D0/dsq
      cxyzp = (/ sphi * (cxyz(1) * cxyz(3) * cpsi - cxyz(2) * spsi) * dsqi + cxyz(1) * cphi, &
                sphi * (cxyz(2) * cxyz(3) * cpsi + cxyz(1) * spsi) * dsqi + cxyz(2) * cphi, &
                -sphi * cpsi * dsq + cxyz(3) * cphi /)
    end if
!  From MCML paper END

! normalize the direction cosines
    dd = 1.D0/dsqrt(sum(cxyzp*cxyzp))
    cxyzp = cxyzp * dd
    
! get the step size between scattering events (inline code rather than function call)
    alpha = prealpha / Ec
    step = Ec*(Ec+1024.D0)/Ze/(Ec+511.D0)               ! step is used here as a dummy variable
    sige =  presig * step * step * alpha * (1.D0+alpha)
    lambda = pre * sige

    rr = rn(iran) ! rng_uniform(rngt)
    iran = iran+1
    step = - lambda * log(rr)

! apply the step to the next location    
    xyzn = xyz + step * scaled * cxyzp 

! did this electron end up outside the crystal ?  [Replaced with Patrick's code to 
! allow for double tilt of the sample surface (sigma, omega) ]
    znmax = xyzn(2) * tano
    if (xyzn(3).gt.znmax) then
!    if (xyzn(3).gt.0.D0) then    ! old line
        bsct = bsct + 1  ! yes, we have a back-scattered electron

        
! Let's figure out where in the Lambert array this point should be projected ...   
! We know the direction cosines were normalized, so no reason to check the error flag
        dxy = delta * LambertSphereToSquare( cxyzp, ierr )       
        
! and get the nearest pixel [ take into account reversal of coordinate frame (x,y) -> (y,-x) ]
        idxy = (/ nint(dxy(2)), nint(-dxy(1)) /)
        
        if (maxval(abs(idxy)).le.nx) then
! If Ec larger than Emin, then we should count this electron
           if (Ec.gt.Emin) then     
             iE = nint((Ec-Ehistmin)/Ebinsize)+1
! first add this electron to the correct exit distance vs. energy bin (coarser than the angular plot)
             edis = dabs(xyz(3)/cxyzp(3))   ! distance from last scattering point to surface along trajectory
             iz = nint(edis*0.1D0/depthstep) +1
             if ( (iz.gt.0).and.(iz.le.numzbins) ) then
              px = nint(idxy(1)/10.0)
              py = nint(idxy(2)/10.0)
              accum_z(iE,iz,px,py) = accum_z(iE,iz,px,py) + 1
            end if
! then add it to the modified Lambert accumulator array.
            accum_e(iE,idxy(1),idxy(2)) = accum_e(iE,idxy(1),idxy(2)) + 1
           end if
        end if
        
        EXIT trajloop  ! and exit trajloop
    end if  ! xyzn(3).gt.znmax

! update the electron direction cosines, coordinates and the interaction event counter
   cxyz = cxyzp
   xyz = xyzn
   traj = traj + 1
  
   write (dataunit,"(I6,':',F10.4,',',5(F12.4,','),F12.4)") traj, Ec, cxyz(1:3), xyz(1:3)
   write (dataunit2,"(I6,':',E12.4,',',E12.4,',',E12.4,',',E12.4)") traj, alpha, step*scaled, sige, lambda*scaled 

  end do trajloop

end do mainloop

close(unit=dataunit,status='keep')
close(unit=dataunit2,status='keep')


end subroutine single_run




end subroutine DoMCsimulation
