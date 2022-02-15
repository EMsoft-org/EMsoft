! ###################################################################
! Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:PSOmod.f90
!--------------------------------------------------------------------------
!
! Module: PSOmod
!
!> @author Chaoyi Zhu/Marcus Ochsendorf/Marc De Graef, Carnegie Mellon University
!
!> @brief Global Optimization for Pattern Center, Orientation, and 
! Deformation using Dynamical EBSD patterns
!
!> @date  09/15/20  CZ 1.0 
! ###################################################################

module PSO
    implicit none

    ! the object that stores all relevant parameters of optimization
    type particle
        real(kind=4), dimension(:), allocatable :: parameters
        real(kind=4), dimension(:), allocatable :: velocity
        real(kind=4), dimension(:), allocatable :: best_parameters
        real(kind=4) :: best_cost,current_cost
    end type particle 
    
contains

    recursive subroutine particle_init(part, num_parameters,min,max)
        ! initializes the particles 
        use, intrinsic :: ISO_Fortran_env
        implicit none
        type (particle) :: part
        integer ,intent(in) ::  num_parameters
        real(kind=4), dimension(num_parameters), intent(in) :: min,max
        real(kind=4) :: diff
        integer(kind=4) :: i
        real(kind=4), dimension(:), allocatable :: rand

        ! Initialization of the random number generation
        allocate(rand(num_parameters)) 
        call random_number(rand)
        
        do i=1,num_parameters
            diff = max(i) - min(i)
            rand(i) = min(i) + diff*rand(i)
        end do
        
        ! Initialization of the particle object
        allocate(part%parameters(num_parameters))
        allocate(part%velocity(num_parameters))
        allocate(part%best_parameters(num_parameters))

        part%best_cost = HUGE(1.0)
        part%current_cost = HUGE(1.0)
        part%parameters = rand 
        part%best_parameters = rand 

        do i=1,num_parameters
            part%velocity(i) = 0
        end do
    
    end subroutine particle_init

    subroutine swarm_init(swarm, num_param, minimum, maximum)
        ! intializes the swarm by generating the initial guesses for the optimization problem
        implicit none
        type (particle), dimension(:), intent(inout) :: swarm
        integer, intent(in) :: num_param 
        real(kind=4), dimension(num_param), intent(in) :: minimum, maximum
        integer :: i   

        do i=1,size(swarm)
            call particle_init(swarm(i),num_param,min = minimum, max=maximum)
        end do

    end subroutine swarm_init

   subroutine swarm_init_single(swarm, num_param, minimum, maximum)
        ! intializes the swarm by generating the initial guesses for the optimization problem
        implicit none
        type (particle), intent(inout) :: swarm
        integer, intent(in) :: num_param 
        real(kind=4), dimension(num_param), intent(in) :: minimum, maximum
        call particle_init(swarm,num_param,min = minimum, max=maximum)
        
    end subroutine swarm_init_single

    recursive subroutine print_particle(part)
        ! prints all of the variables stored within a particle object
        implicit none
        type (particle),intent(in) :: part
        write(*,*)
        print *, "The current parameters of this particle are: "
        print *, part%parameters
        print *, "The best parameters this particle found are: "
        print *, part%best_parameters
        print *, "The current velocity of this particle is: "
        print *, part%velocity
        print *, "The current cost of the particle is: "
        print *, part%current_cost
        print *, "The best cost that this particle found is: "
        print *, part%best_cost
    
    end subroutine print_particle

    recursive subroutine random_init()
        ! set the seed for random number generation
        implicit none
        integer(kind=4) :: i,n,clock
        integer(kind=4) , dimension(:), allocatable :: seed

        call random_seed(size = n)
        allocate(seed(n))
        call system_clock(count = clock)
        seed = clock + 37*(/(i-1,i=1,n)/)
        call random_seed(put =seed)
        deallocate(seed)
    
    end subroutine random_init



  subroutine find_min(swarm, best, w, w_damp, c1, c2, Dim_XC, st_initial,de,&
     mcnl, mpnl, EBSDMCdata, EBSDMPdata, patterndata, enl, offset3, minimum, maximum)
        use local
        use typedefs
        use NameListTypedefs
        use HDF5
       
        
        IMPLICIT NONE
        type(MCCLNameListType),intent(inout)                           :: mcnl
        type(EBSDMasterNameListType),intent(inout)                     :: mpnl
        type(EBSDMCdataType),intent(inout)                             :: EBSDMCdata
        type(EBSDMPdataType),intent(inout)                             :: EBSDMPdata
        type(EBSDDIpreviewNameListType),INTENT(INOUT)       :: patterndata
        type(EBSDNameListType),INTENT(INOUT)                :: enl
        type(EBSDDENameListType),INTENT(INOUT)              :: de
        integer(kind=4),INTENT(IN)                        :: Dim_XC
        integer(HSIZE_T),intent(in)                         :: offset3(3)
        real(kind=4),dimension(3),INTENT(IN)              ::  st_initial
        type (particle), dimension(:), intent(inout)  :: swarm
        real(kind=4), dimension(Dim_XC), intent(in)  :: minimum, maximum
        type (particle), intent(inout) :: best
        real(kind=4), intent(inout) :: w 
        real(kind=4), intent(in) :: w_damp, c1, c2
        real(kind=4) :: v_ideal, v_ave, v_start, maxVelocity(Dim_XC), minVelocity(Dim_XC)
        real(kind=4) :: value(size(swarm),Dim_XC), rand_step_1, rand_step_2, objval(size(swarm))
        
        integer :: i,j,k,s,iter, iloc
        
        iter=1
        v_start=0.0
        maxVelocity=0.1*(maximum-minimum)
        minVelocity=-maxVelocity

        do i=1, de%itermax
                value=0.0              
            do j=1,size(swarm)
                call random_number(rand_step_1)
                call random_number(rand_step_2)
                swarm(j)%velocity = de%w*swarm(j)%velocity + de%c1*rand_step_1*(best%parameters - swarm(j)%parameters) &
                & + de%c2*rand_step_2*(swarm(j)%best_parameters -swarm(j)%parameters)
                swarm(j)%parameters = swarm(j)%parameters + swarm(j)%velocity
                ! Confine invidual particle velocity in the lower-upper bound
                swarm(j)%velocity =max(min(swarm(j)%velocity,maxVelocity),minVelocity)
                ! Confine invidual particle in the lower-upper bound
                swarm(j)%parameters=max(min(swarm(j)%parameters,maximum),minimum)
                value(j,1:Dim_XC)=swarm(j)%parameters
            end do

            call objective_function(offset3, value, st_initial, objval, Dim_XC, &
            enl, patterndata, size(swarm), de%objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
            
            ! updates the inertia weight
            de%w = de%w*de%w_damp  
          
            do k=1,size(swarm)
              swarm(k)%current_cost=objval(k)
              if (swarm(k)%best_cost > swarm(k)%current_cost) then
                swarm(k)%best_cost = swarm(k)%current_cost
                swarm(k)%best_parameters = swarm(k)%parameters      
              end if
            end do
            iloc=minloc(objval,1)
            best = swarm(iloc)
            iter=iter+1

            if( (de%refresh > 0) .and. (mod(iter,de%refresh)==0)) then
              write(*,*)
              print *,"# Iteration:",iter,": Objective function value:", best%best_cost 
              print *,"# Best Member:", best%best_parameters
            end if

            ! end if best fitness if smaller than expected value to reach
            if ( best%best_cost  <= de%VTR ) then
              print *,"# The best fitness", best%best_cost , "is smaller than VTR at generation #", iter
              print *,"# Best Member:", best%best_parameters
              exit
            end if
            end do
            

    end subroutine find_min
    
subroutine objective_function(offset3, value,st_initial, objval, &
  Dim_XC, enl, patterndata, numangles, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
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
    use detectors
    use EBSDmod
    use Lambert
    use quaternions
    use rotations
    use noise
    use HDF5
    use HDFsupport
    use ISO_C_BINDING
    use omp_lib
    use timing
    use stringconstants
    use math
    use filters
    use patternmod
    use error
    use image
    use FFTW3mod
    use, intrinsic :: iso_fortran_env
    
    IMPLICIT NONE
    
    
    type(EBSDNameListType),INTENT(INOUT)                :: enl
    type(EBSDDIpreviewNameListType),INTENT(INOUT)       :: patterndata
    integer(kind=irg),INTENT(IN)                        :: numangles, Dim_XC, objective
    real(kind=sgl),dimension(3),INTENT(IN)              :: st_initial
    real(kind=sgl), dimension(numangles, Dim_XC), intent(in)   :: value
    real(kind=sgl), dimension(numangles),INTENT(OUT)    :: objval
    integer(HSIZE_T),intent(in)                         :: offset3(3)
    type(MCCLNameListType),intent(inout)                :: mcnl
    type(EBSDMasterNameListType),intent(inout)          :: mpnl
    type(EBSDMCdataType),intent(inout)                  :: EBSDMCdata
    type(EBSDMPdataType),intent(inout)                  :: EBSDMPdata
    
    ! all geometrical parameters and filenames
    real(kind=dbl)                          :: prefactor, qz(3)
    real(kind=sgl), dimension(numangles, Dim_XC)  :: X_value
    ! allocatable arrays
    real(kind=sgl),allocatable              :: EBSDpattern(:,:), binned(:,:)        ! array with EBSD patterns
    real(kind=sgl),allocatable              :: z(:,:)               ! used to store the computed patterns before writing to disk
    real(kind=sgl),allocatable              :: energywf(:), eulerangles(:,:)
    
    ! arrays for each OpenMP thread
    real(kind=sgl),allocatable              :: tmLPNH(:,:,:) , tmLPSH(:,:,:)
    real(kind=sgl),allocatable              :: trgx(:,:), trgy(:,:), trgz(:,:)          ! auxiliary detector arrays needed for interpolation
    real(kind=sgl),allocatable              :: taccum(:,:,:)
    
    ! various items
    integer(kind=irg)                       :: i, j, ii, jj, iang, jang, k, hdferr, dim2, recordsize         ! various counters
    integer(kind=irg)                       :: iunitexpt, istat, istats, ipar(7), L, correctsize
    integer(kind=irg)                       :: nix, niy, binx, biny, nixp, niyp     ! various parameters
    integer(kind=irg)                       :: nthreads,maskradius
    real(kind=sgl)                          :: norm_target,norm_pattern(numangles)
    real(kind=sgl)                          :: ma, mi, tstart, tstop, io_real(3),temp_objval, max_p
    real(kind=sgl),parameter                :: dtor = 0.0174533  ! convert from degrees to radians
    real(kind=dbl),parameter                :: nAmpere = 6.241D+18   ! Coulomb per second
    integer(kind=irg),parameter             :: storemax = 20        ! number of EBSD patterns stored in one output block
    integer(kind=irg)                       :: Emin, Emax      ! various parameters
    real(kind=dbl)                          :: dc(3), scl, nel, emult           ! direction cosine array
    real(kind=dbl)                          :: sx, dx, dxm, dy, dym, rhos, x         ! various parameters
    real(kind=dbl)                          :: ixy(2), tmp
    real(kind=sgl),allocatable              :: mask(:,:), masklin(:), lx(:), ly(:), binnedvec(:), targetpattern(:)
    character(kind=c_char),allocatable      :: batchpatterns(:,:,:), bpat(:,:)
    integer(kind=irg),allocatable           :: batchpatternsint(:,:,:), bpatint(:,:)
    real(kind=sgl),allocatable              :: batchpatterns32(:,:,:), batchpatterns32lin(:,:)
    integer(kind=irg),allocatable           :: acc_array(:,:)
    real(kind=sgl),allocatable              :: master_arrayNH(:,:), master_arraySH(:,:), wf(:) 
    character(len=3)                        :: outputformat
    character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)
    
    ! parameter for random number generator
    integer, parameter                      :: K4B=selected_int_kind(9)      ! used by ran function in math.f90
    integer(K4B)                            :: idum
    
    integer(HSIZE_T)                        :: dims3(3)
    logical                                 :: verbose
    character(5)                            :: bitmode
    integer(kind=irg)                       :: numbits
    real(kind=sgl)                          :: bitrange, q_c(numangles,4)
    
    ! new stuff: deformation tensor
    real(kind=dbl)                          :: Umatrix(3,3), Fmatrix(3,3), Smatrix(3,3), quF(4), Fmatrix_inverse(3,3), &
                                               Gmatrix(3,3)
    logical                                 :: includeFmatrix=.FALSE.
    type(C_PTR)                                         :: HPplanf, HPplanb
    complex(kind=dbl),allocatable                       :: hpmask(:,:)
    complex(C_DOUBLE_COMPLEX),pointer                   :: inp(:,:), outp(:,:)
    type(c_ptr), allocatable                            :: ip, op
    real(kind=dbl),allocatable                          :: rrdata(:,:), ffdata(:,:)


    ! binned pattern array
    binx = enl%numsx/enl%binning
    biny = enl%numsy/enl%binning
    recordsize = 4 * binx * biny
     ! convert the pc back to units of pixels
    do iang=1,numangles
       if (enl%applyDeformation.eq.'y') then
          X_value(iang, 4:12)=value(iang, 4:12)
        else 
          X_value(iang, 4:6)=value(iang, 4:6)
        end if

      if (enl%eulerconvention.eq.'hkl') then
          X_value(iang, 1:3)=(/-enl%numsx*(value(iang,1)-0.5), &
          enl%numsy*(value(iang,2)-0.5), enl%numsx*value(iang,3)*enl%delta/)

          if (patterndata%inputtype.eq.'BrukerHDF') X_value(iang, 1:3)= &
          (/-enl%numsx*(value(iang,1)-0.5),enl%numsy*(0.5-value(iang,2)), enl%numsy*value(iang,3)*enl%delta/)

      elseif (enl%eulerconvention.eq.'tsl') then
          X_value(iang, 1:3)=(/-enl%numsx*(value(iang,1)-0.5), &
          enl%numsx*value(iang,2)-0.5*enl%numsy, enl%numsx*value(iang,3)*enl%delta/)
      else 
        print *, "Undefined Euler Convention"
      end if
    end do
  
    call RotationCorrection(q_c, enl%applyDeformation,enl%delta, enl%thetac, st_initial, X_value, Dim_XC, numangles) 
  
    call h5open_EMsoft(hdferr)
    
    istats = openExpPatternFile(enl%targetfile, patterndata%ipf_wd, binx * biny, &
    patterndata%inputtype, recordsize, iunitexpt, patterndata%HDFstrings, verbose=.FALSE.)
    if (istats.ne.0) then
        call patternmod_errormessage(istats)
        call FatalError("Read Pattern Routine:","Fatal error handling experimental pattern file")
    end if
    ! and read the pattern from target pattern file
    allocate(targetpattern(enl%numsx * enl%numsy))
    dims3 = (/ binx, biny, 1 /)
    call getSingleExpPattern(patterndata%paty, patterndata%ipf_wd, binx * biny, &
    binx * biny, dims3, offset3, iunitexpt, patterndata%inputtype, patterndata%HDFstrings, targetpattern)
    
    ! binning the experimental pattern
    allocate(EBSDpattern(enl%numsx, enl%numsy), binned(binx, biny),stat=istat)
    if (enl%binning.ne.1) then
      EBSDpattern = reshape(targetpattern,(/enl%numsx , enl%numsy/))
      deallocate(targetpattern)
      allocate(targetpattern(binx*biny))
      do ii=1,enl%numsx,enl%binning
        do jj=1,enl%numsy,enl%binning
            binned(ii/enl%binning+1,jj/enl%binning+1) = &
            sum(EBSDpattern(ii:ii+enl%binning-1,jj:jj+enl%binning-1))
        end do
      end do
      targetpattern=reshape(binned,(/binx*biny/))
    end if
    deallocate(EBSDpattern, binned)
    
    if (enl%makedictionary.eq.'y') then
    ! high pass filter and adaptive histogram equalization
    allocate(hpmask(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate hpmask, inp, outp arrays'
    allocate(rrdata(binx,biny),ffdata(binx,biny),stat=istat)
    if (istat .ne. 0) stop 'could not allocate rrdata, ffdata arrays'
    allocate(bpatint(binx,biny),binned(binx,biny),stat=istat)

    ip = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
    call c_f_pointer(ip, inp, [binx,biny])

    op = fftw_alloc_complex(int(binx*biny,C_SIZE_T))
    call c_f_pointer(op, outp, [binx,biny])

    inp = cmplx(0.D0,0D0)
    outp = cmplx(0.D0,0.D0)
    
    call init_HiPassFilter(dble(enl%hipassw), (/binx, biny /), hpmask, inp, outp, HPplanf, HPplanb) 
    
    binned = reshape(targetpattern,(/binx , biny/))
    rrdata = dble(binned)
    ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), dble(enl%hipassw), hpmask, inp, outp, HPplanf, HPplanb)
    binned = sngl(ffdata)

    ma = maxval(binned)
    mi = minval(binned)

    bpatint = nint(((binned - mi) / (ma-mi))*255.0)
   
    binned = float(adhisteq(enl%nregions,binx,biny,bpatint))
    targetpattern = reshape(binned ,(/binx*biny/))

    deallocate(bpatint, binned, rrdata, ffdata, hpmask)
    call fftw_free(ip)
    call fftw_free(op)
    call fftw_cleanup()
    end if
    
    call closeExpPatternFile(patterndata%inputtype, iunitexpt)
    call h5close_EMsoft(hdferr)
    
    !====================================
    !====================================
    ! bit depth and format of output
    
    call get_bit_parameters(enl%bitdepth, numbits, bitrange, bitmode)

    if (enl%makedictionary.eq.'y') bitmode = 'dict'
    ! define some energy-related parameters derived from MC input parameters
    !====================================
    ! make sure the requested energy range is within the range available from the Monte Carlo computation
    if (enl%energymin.lt.mcnl%Ehistmin) enl%energymin = mcnl%Ehistmin
    if (enl%energymax.gt.mcnl%EkeV) enl%energymax = mcnl%EkeV
    
    ! get the indices of the minimum and maximum energy
    Emin = nint((enl%energymin - mcnl%Ehistmin)/mcnl%Ebinsize) +1
    if (Emin.lt.1)  Emin=1
    if (Emin.gt.EBSDMCdata%numEbins)  Emin=EBSDMCdata%numEbins
    
    Emax = nint((enl%energymax - mcnl%Ehistmin)/mcnl%Ebinsize) +1
    if (Emax.lt.1)  Emax=1
    if (Emax.gt.EBSDMCdata%numEbins)  Emax=EBSDMCdata%numEbins
    
    ! modified by MDG, 03/26/18
    nel = float(mcnl%totnum_el) * float(EBSDMCdata%multiplier)
    emult = nAmpere * 1e-9 / nel  ! multiplicative factor to convert MC data to an equivalent incident beam of 1 nanoCoulomb
    !write (*,*) ' multiplicative factor to generate 1 nC of incident electrons ', emult
    
    ! and allocate space to store each batch; this requires some careful analysis
    ! since we are doing things in multiple threads
      nthreads = enl%nthreads
      L = binx*biny
      ! make sure that correctsize is a multiple of 16; if not, make it so
      if (mod(L,16) .ne. 0) then
          correctsize = 16*ceiling(float(L)/16.0)
      else
          correctsize = L
      end if

    ! and allocate the batchpatterns array for hyperslab writing [modified 8/25/17 for different output formats]
    if (trim(bitmode).eq.'char') then 
      allocate(batchpatterns(binx,biny,numangles),stat=istat)
    end if
    if (trim(bitmode).eq.'int') then 
      allocate(batchpatternsint(binx,biny,numangles),stat=istat)
    end if
    if (trim(bitmode).eq.'float') then 
      allocate(batchpatterns32(binx,biny,numangles),stat=istat)
    end if
    if (trim(bitmode).eq.'dict') then 
      allocate(batchpatterns32lin(correctsize,numangles),stat=istat)
    end if
    !====================================
    ! here we also create a mask if necessary
      allocate(mask(binx,biny), masklin(L), stat=istat)
      mask = 1.0
      masklin = 1.0
      if (enl%maskpattern.eq.'y') then
    ! create the circular mask in a potentially rectangular array
        maskradius = (minval( (/ binx, biny /) ) / 2 )**2
        allocate(lx(binx), ly(biny), stat=istat)
        lx = (/ (float(i),i=1,binx) /) - float(binx/2)
        ly = (/ (float(i),i=1,biny) /) - float(biny/2)
        do i=1,binx
          do j=1,biny
            if ((lx(i)**2+ly(j)**2).gt.maskradius) mask(i,j) = 0.0
          end do
        end do
        deallocate(lx, ly)
        if (trim(bitmode).eq.'dict') then
          do j = 1,biny
            do i = 1,binx
              masklin((j-1)*binx+i) = mask(i,j)
            end do
          end do 
        end if
      end if
    ! apply mask on the experimental pattern
    targetpattern(1:L)=masklin(1:L) * targetpattern(1:L)
    !====================================
    ! determine the scale factor for the Lambert interpolation
    scl = dble(mpnl%npx) 
    
    !====================================
    ! define the integer parameter list for the CalcEBSDPatternSingleFull call
    ipar(1) = enl%binning
    ipar(2) = enl%numsx
    ipar(3) = enl%numsy
    ipar(4) = mpnl%npx
    ipar(5) = mpnl%npx
    ipar(6) = EBSDMCdata%numEbins
    ipar(7) = EBSDMCdata%numEbins
    
    !====================================
    ! set the number of OpenMP threads 
    call OMP_SET_NUM_THREADS(nthreads)

    !====================================
    !====================================

    ! use OpenMP to run on multiple cores ... 
    !$OMP PARALLEL default(shared)  PRIVATE(iang,i,j,istat,EBSDpattern,binned,idum,bpat,ma,mi,bpatint)&
    !$OMP& PRIVATE(tmLPNH, tmLPSH, trgx, trgy, trgz, taccum, prefactor)&
    !$OMP& PRIVATE(Fmatrix_inverse, nel, Fmatrix, binnedvec)

    ! each thread needs a private copy of the master and accum arrays; not having
    ! those can produce poor scaling... in addition, they need to be recomputed for each pattern !
      allocate(trgx(enl%numsx,enl%numsy), trgy(enl%numsx,enl%numsy), trgz(enl%numsx,enl%numsy))
      allocate(taccum(EBSDMCdata%numEbins,enl%numsx,enl%numsy))
      allocate(tmLPNH(enl%numsx,enl%numsy,EBSDMCdata%numEbins), tmLPSH(enl%numsx,enl%numsy,EBSDMCdata%numEbins))
    ! and copy the data in
      tmLPNH = EBSDMPdata%mLPNH
      tmLPSH = EBSDMPdata%mLPSH
    
    ! allocate the arrays that will hold the computed pattern
      allocate(binned(binx,biny),stat=istat)
      if (trim(bitmode).eq.'char') then 
        allocate(bpat(binx,biny),stat=istat)
      end if
      if (trim(bitmode).eq.'int') then 
        allocate(bpatint(binx,biny),stat=istat)
      end if
      if (trim(bitmode).eq.'dict') then 
        allocate(bpatint(binx,biny),stat=istat)
        allocate(binnedvec(correctsize),stat=istat)
      end if
     
     !$OMP DO SCHEDULE(DYNAMIC)
      do iang=1,numangles
          if (enl%applyDeformation.eq.'y') then
          includeFmatrix = .TRUE.
    ! invert the transposed deformation tensor for this pattern
          
          Fmatrix = real(transpose(reshape(X_value(iang,4:12),(/ 3,3 /))),kind=dbl)
          call mInvert(Fmatrix, Fmatrix_inverse, .FALSE.)
        end if

    ! for each pattern we need to compute the detector arrays 
        if (enl%includebackground.eq.'y') then
          call GeneratemyEBSDDetector(enl, mcnl, EBSDMCdata, enl%numsx, enl%numsy, EBSDMCdata%numEbins, trgx, trgy, trgz, taccum, &
                                    X_value(iang,1:3),bg=.TRUE.)
    ! intensity prefactor
          prefactor = emult * enl%beamcurrent * enl%dwelltime * 1.0D-6
        else
          call GeneratemyEBSDDetector(enl, mcnl, EBSDMCdata, enl%numsx, enl%numsy, EBSDMCdata%numEbins, trgx, trgy, trgz, taccum, &
                                     X_value(iang,1:3),bg=.FALSE.)
    ! we pick a reasonable value here ...
          prefactor = 3.D0 * enl%beamcurrent * enl%dwelltime * 1.0D-6
        end if

        binned = 0.0
            
        if (includeFmatrix.eqv..TRUE.) then 
         if (enl%includebackground.eq.'y') then
          call CalcEBSDPatternSingleFull(ipar,q_c(iang,1:4),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                         Emin,Emax,mask,prefactor,Fmatrix_inverse)
         else
          call CalcEBSDPatternSingleFull(ipar,q_c(iang,1:4),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                         Emin,Emax,mask,prefactor,Fmatrix_inverse,removebackground='y')
         end if
        else
         if (enl%includebackground.eq.'y') then
          call CalcEBSDPatternSingleFull(ipar,q_c(iang,1:4),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                         Emin,Emax,mask,prefactor)
         else
          call CalcEBSDPatternSingleFull(ipar,q_c(iang,1:4),taccum,tmLPNH,tmLPSH,trgx,trgy,trgz,binned, &
                                         Emin,Emax,mask,prefactor,removebackground='y')
         end if
        end if

        if (enl%scalingmode .eq. 'gam') then
            binned = binned**enl%gammavalue
        end if

        if (trim(bitmode).eq.'dict') then  ! pre-process the patterns for dictionary indexing
          ! this step includes adaptive histogram equalization, masking, and normalization
          
          ! adaptive histogram equalization
                  ma = maxval(binned)
                  mi = minval(binned)
                  bpatint = nint(((binned - mi)/ (ma-mi))*255.0)
                  binned =  float(adhisteq(enl%nregions,binx,biny,bpatint))
          
          ! linearize the array and apply the mask
                  binnedvec = 0.0
                  do j= 1,biny
                    do i = 1,binx
                      binnedvec((j-1)*binx+i) = binned(i,j)
                    end do
                  end do
          ! apply circular mask and normalize
                  binnedvec(1:L) = binnedvec(1:L) * masklin(1:L)
                  binnedvec(1:correctsize) = binnedvec(1:correctsize)/vecnorm(binnedvec(1:correctsize))
          
          ! store in array for hyperslab writing
                  batchpatterns32lin(1:correctsize, iang) = binnedvec
        else
          if (trim(bitmode).eq.'char') then 
            ma = maxval(binned)
            mi = minval(binned)
            binned = mask * ((binned - mi)/ (ma-mi))
            bpat = char(nint(bitrange*binned))
            batchpatterns(1:binx,1:biny, iang) = bpat
          end if
      
          if (trim(bitmode).eq.'int') then 
            ma = maxval(binned)
            mi = minval(binned)
            binned = mask * ((binned - mi)/ (ma-mi))
            bpatint = nint(bitrange*binned)
            batchpatternsint(1:binx,1:biny, iang) = bpatint
          end if

          if (trim(bitmode).eq.'float') then 
            batchpatterns32(1:binx,1:biny, iang) = binned
          end if
        end if

      end do ! end of iang loop
     
      !$OMP END DO
      
      ! deallocate arrays to free memory
      deallocate(tmLPSH)
      deallocate(tmLPNH)
      deallocate(taccum)
      deallocate(trgx)
      deallocate(trgy)
      deallocate(trgz)  
      deallocate(binned)

      if (trim(bitmode).eq.'char') then 
        deallocate(bpat)     
       end if
      
      if (trim(bitmode).eq.'int') then 
        deallocate(bpatint)
      end if

      if (trim(bitmode).eq.'dict') then 
        deallocate(binnedvec)
      end if
    
    !$OMP END PARALLEL


    ! Calculate the objective functions values (normalized dot product and root mean square error)
    ! normalized dot product (NDP)
    if (objective .eq. 1) then
      norm_target=norm2(targetpattern)
      do i=1,numangles
        if (trim(bitmode).eq.'char') then 
          norm_pattern(i)=norm2(float(reshape(ichar(batchpatterns(:,:,i)),(/ L /))))
          objval(i)=-dot_product(targetpattern/norm_target, & 
          float(reshape(ichar(batchpatterns(:,:,i)),(/ L /)))/norm_pattern(i))
        end if
      
        if (trim(bitmode).eq.'int') then 
          norm_pattern(i)=norm2(float(reshape(batchpatternsint(:,:,i),(/ L /))))
          objval(i)=-dot_product(targetpattern/norm_target,& 
          float(reshape(batchpatternsint(:,:,i),(/ L /)))/norm_pattern(i))
        end if
      
        if (trim(bitmode).eq.'float') then 
          norm_pattern(i)=norm2(reshape(batchpatterns32(:,:,i),(/ L /)))
          objval(i)=-dot_product(targetpattern/norm_target,& 
          reshape(batchpatterns32(:,:,i),(/ L /))/norm_pattern(i))
        end if

        if (trim(bitmode).eq.'dict') then 
          norm_pattern(i)=norm2(batchpatterns32lin(:,i))
          objval(i)=-dot_product(targetpattern/norm_target,& 
          reshape(batchpatterns32lin(:,i),(/ L /))/norm_pattern(i))
        end if
      end do

      ! root mean square error (RMSE)
    else if (objective .eq. 2) then
      do i=1,numangles
        if (trim(bitmode).eq.'char') then
          objval(i)=sqrt(sum((float(reshape(ichar(batchpatterns(:,:,i)),(/ L /)))-targetpattern)**2)/L)
        end if

        if (trim(bitmode).eq.'int') then
          objval(i)=sqrt(sum((float(reshape(batchpatternsint(:,:,i),(/ L /)))-targetpattern)**2)/L)
        end if

        if (trim(bitmode).eq.'float') then
          objval(i)=sqrt(sum((reshape(batchpatterns32(:,:,i),(/ L /))-targetpattern)**2)/L)
        end if

        if (trim(bitmode).eq.'dict') then
          objval(i)=sqrt(sum((reshape(batchpatterns32lin(:,i),(/ L /))-targetpattern)**2)/L)
        end if
      end do
    else
      print *,"Undefined Objective Function"
    end if
      
      ! deallocate arrays
      deallocate(targetpattern)

      if (trim(bitmode).eq.'char') then 
        deallocate(batchpatterns) 
      end if
      
      if (trim(bitmode).eq.'int') then 
        deallocate(batchpatternsint)
      end if
      
      if (trim(bitmode).eq.'float') then 
        deallocate(batchpatterns32)
      end if
       
      if (trim(bitmode).eq.'dict') then 
        deallocate(batchpatterns32lin)
      end if
end subroutine objective_function


subroutine RotationCorrection(q_c, Fmatrix,delta, thetac, st_initial, X_value, Dim_XC, numangles) 

use quaternions
use rotations
use constants 

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                        :: numangles, Dim_XC
real(kind=sgl),dimension(3),INTENT(IN)              :: st_initial
real(kind=sgl), dimension(numangles, Dim_XC), intent(in)   :: X_value
real(kind=sgl),intent(in)                           :: delta, thetac
real(kind=sgl), intent(out)                         :: q_c(numangles,4)

integer(kind=irg)                                   :: i
character(len=1)                                    :: Fmatrix
real(kind=dbl)                                      :: qu(4), delta_pc(3),a,rho_c,w,n(3),r(4)


if (Fmatrix.eq.'y') then
  do i=1,numangles
    
    q_c(i,:)=st2qu(st_initial)
    
  end do

else
do i=1,numangles
r=st2qu(X_value(i,4:6))
! delta_pc=(/-(X_value(i,1)-pc_initial(1)),-(X_value(i,2)-pc_initial(2)), X_value(i,3)/)
! ! sample tilt 20 degrees + detector tilt (rad)
! a=((thetac+20.0)*cPi)/180.0
! rho_c=sqrt(delta_pc(1)**2+(delta_pc(2)*cos(2*a))**2)
! n=(/-delta_pc(1)*cos(a),delta_pc(2)*cos(2*a),delta_pc(1)*sin(a)/)/rho_c
! w=dacos((delta_pc(3)+delta_pc(2)*delta*sin(2*a))/sqrt(delta_pc(3)**2+ &
! 2*delta_pc(3)*delta_pc(2)*delta*sin(2*a)+(delta_pc(1)*delta)**2+(delta_pc(2)*delta)**2))
! r=ax2qu((/n(1),n(2),n(3), w/))
! q_c(:,i)=quat_mult(r,qu)
qu=st2qu(st_initial)
q_c(i,:)=quat_mult(r,qu)
end do
end if
end subroutine RotationCorrection

function randperm(num)
    use local
    implicit none
    integer(kind=irg), intent(in) :: num
    integer(kind=irg) :: number, i, j, k
    integer(kind=irg), dimension(num) :: randperm
    real(kind=sgl), dimension(num) :: rand2
    intrinsic random_number
    call random_number(rand2)
    do i=1,num
    number=1
    do j=1,num
    if (rand2(i) > rand2(j)) then
      number=number+1
    end if
    end do
    do k=1,i-1
    if (rand2(i) <= rand2(k) .and. rand2(i) >= rand2(k)) then
      number=number+1
    end if
    end do
    randperm(i)=number
    end do
    return
 end function randperm
 

subroutine DE_Fortran90(Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC, &
CR_XC, strategy, objective, bestmem_XC, bestval, nfeval, F_CR, method, refresh, &
enl,patterndata,st_initial,offset3, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
!.......................................................................
!    
!                     Differential Evolution Algorithm
!
!.......................................................................
!  This part of Fortran 90 program is adapted (with minor modifications) 
!  from Dr. Feng-Sheng Wang's Fortran translation on the original Matlab 
!  version of the differential evolution algorithm.
!
!  Any users new to the DE are encouraged to read the article of Storn and Price. 
!
!  Refences:
!  Storn, R., and Price, K.V., (1996). Minimizing the real function of the 
!  ICEC'96 contest by differential evolution. IEEE conf. on Evolutionary 
!  Comutation, 842-844.
!.........................................................................
!                           List of Parameters
!.........................................................................
!                obj : The user provided file for evlauting the objective function.
!                      subroutine obj(xc,fitness)
!                      where "xc" is the real decision parameter vector.(input)
!                            "fitness" is the fitness value.(output)
!             Dim_XC : Dimension of the real decision parameters.
!      XCmin(Dim_XC) : The lower bound of the real decision parameters.
!      XCmax(Dim_XC) : The upper bound of the real decision parameters.
!                VTR : The expected fitness value to reach.
!                 NP : Population size.
!            itermax : The maximum number of iteration.
!               F_XC : Mutation scaling factor for real decision parameters.
!              CR_XC : Crossover factor for real decision parameters.
!           strategy : The strategy of the mutation operations is used in HDE.
!            refresh : The intermediate output will be produced after "refresh"
!                      iterations. No intermediate output will be produced if
!                      "refresh < 1".
!             iwrite : The unit specfier for writing to an external data file.
! bestmen_XC(Dim_XC) : The best real decision parameters.
!              bestval : The best objective function.
!             nfeval : The number of function call.
!         method(1) = 0, Fixed mutation scaling factors (F_XC)
!                   = 1, Random mutation scaling factors F_XC=[0, 1]
!                   = 2, Random mutation scaling factors F_XC=[-1, 1] 
!         method(2) = 1, Random combined factor (F_CR) used for strategy = 6
!                        in the mutation operation 
!                   = other, fixed combined factor provided by the user 
!         method(3) = 1, Saving results in a data file.
!                   = other, displaying results only.
!.........................................................................

use local
use typedefs
use files
use NameListTypedefs
use NameListHandlers
use JSONsupport
use io
use HDF5
use HDFsupport
use error
use detectors
use EBSDmod
use stringconstants
use omp_lib

IMPLICIT NONE

integer(kind=irg), dimension(3), intent(in)              :: method
type(EBSDNameListType),INTENT(INOUT)                     :: enl
type(EBSDDIpreviewNameListType),INTENT(INOUT)            :: patterndata
integer(kind=irg), intent(in)                            :: NP, Dim_XC, itermax, strategy, objective, refresh
real(kind=sgl), intent(in)                               :: VTR, CR_XC, st_initial(3)
real(kind=sgl), intent(inout)                            :: F_XC, F_CR
real(kind=sgl), dimension(Dim_XC), intent(in)            :: XCmin, XCmax
real(kind=sgl), dimension(Dim_XC), intent(inout)         :: bestmem_XC
real(kind=sgl), intent(out)                              :: bestval
integer(kind=irg), intent(out)                           :: nfeval
integer(HSIZE_T),intent(in)                              :: offset3(3)
type(MCCLNameListType),intent(inout)                     :: mcnl
type(EBSDMasterNameListType),intent(inout)               :: mpnl
type(EBSDMCdataType),intent(inout)                       :: EBSDMCdata
type(EBSDMPdataType),intent(inout)                       :: EBSDMPdata


real(kind=sgl), dimension(NP,Dim_XC)                     :: pop_XC, bm_XC, mui_XC, mpo_XC,   &
                                                         popold_XC, rand_XC, ui_XC, jrand, rand_j
integer(kind=irg)                                        :: i, j, ibest, iter 
integer(kind=irg), dimension(NP)                         :: rot, a1, a2, a3, a4, a5, rt
integer(kind=irg), dimension(4)                          :: ind
real(kind=sgl), dimension(NP)                            :: val, tempval
real(kind=sgl), dimension(Dim_XC)                        :: bestmemit_XC
real(kind=sgl), dimension(Dim_XC)                        :: rand_C1
integer(kind=irg)                                        :: res, error_cnt, hdferr
integer(kind=irg)                                        :: istat
logical                                                  :: verbose
!intrinsic max, min, random_number, mod, abs, any, all, maxloc, floor


!!---------------------Initialize a population -----------------------------!!
! randomly initialize between the bounds
pop_XC=0.0
do i=1,NP
  call random_number(rand_C1)
  pop_XC(i,:)=XCmin+rand_C1*(XCmax-XCmin)
end do

!!--------------------------------------------------------------------------!!

!!------Evaluate fitness functions and find the best member-----------------!!
val=0.0
nfeval=0
ibest=1

! Evaluate the inital population
call objective_function(offset3, pop_XC, st_initial, val, Dim_XC, enl, &
patterndata, NP, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)

bestval=val(1)
nfeval=nfeval+1

do i=2,NP
    nfeval=nfeval+1
if (val(i) < bestval) then
 ibest=i
 bestval=val(i)
end if
end do       
bestmemit_XC=pop_XC(ibest,:)
bestmem_XC=bestmemit_XC

!!--------------------------------------------------------------------------!!

bm_XC=0.0
rot=(/(i,i=0,NP-1)/)
iter=1 
!!------Perform evolutionary computation------------------------------------!! 

do while (iter <= itermax)
popold_XC=pop_XC

!!------Mutation operation--------------------------------------------------!!
! rand permutation of the population size
ind=randperm(4)
a1=randperm(NP)
rt=mod(rot+ind(1),NP)
a2=a1(rt+1)
rt=mod(rot+ind(2),NP)
a3=a2(rt+1)
rt=mod(rot+ind(3),NP)
a4=a3(rt+1)
rt=mod(rot+ind(4),NP)
a5=a4(rt+1)
bm_XC=spread(bestmemit_XC, DIM=1, NCOPIES=NP)

!----- Generating a random scaling factor--------------------------------!
select case (method(1))
  case (1)
    call random_number(F_XC)
  case(2)
    call random_number(F_XC)
    F_XC=2.0*F_XC-1.0
end select

!---- select a mutation strategy-----------------------------------------!
select case (strategy)

case (1) !de/best/1/bin
  ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

case default !de/rand/1/bin
  ui_XC=popold_XC(a3,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

case (3) !de/rand-to-best/1/bin
  ui_XC=popold_XC+F_XC*(bm_XC-popold_XC+popold_XC(a1,:)-popold_XC(a2,:))

case (4) !de/best/2/bin
  ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:)-popold_XC(a4,:))

case (5) !de/rand/2/bin
  ui_XC=popold_XC(a5,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:) &
    -popold_XC(a4,:))

case (6) ! A linear crossover combination of bm_XC and popold_XC
  if (method(2) == 1) call random_number(F_CR) 
  ui_XC=popold_XC+F_CR*(bm_XC-popold_XC)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))
end select
!!--------------------------------------------------------------------------!!
!!---------------------Binomial Crossover operation-------------------------!!
call random_number(rand_XC)
call random_number(rand_j)

jrand=floor(rand_j*Dim_XC+1)
mui_XC=0.0
mpo_XC=0.0

do i=1,NP
do j=1,Dim_XC
if ((rand_XC(i,j) < CR_XC) .or. (j==jrand(i,j))) then
  mui_XC(i,j)=1.0
else
  mpo_XC(i,j)=1.0
end if
end do 
end do
ui_XC=popold_XC*mpo_XC+ui_XC*mui_XC
!!--------------------------------------------------------------------------!!
!!------Evaluate fitness functions and find the best member-----------------!!
do i=1,NP
! Confine each of feasible individuals in the lower-upper bound
 ui_XC(i,:)=max(min(ui_XC(i,:),XCmax),XCmin)
end do

call objective_function(offset3, ui_XC,st_initial, tempval, Dim_XC, enl, &
patterndata, NP, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)

do i=1,NP
  nfeval=nfeval+1

! Population selection based on objectiove function values 
  if (tempval(i) < val(i)) then
     pop_XC(i,:)=ui_XC(i,:)
     val(i)=tempval(i)
     if (tempval(i) < bestval) then
        bestval=tempval(i)
        bestmem_XC=ui_XC(i,:)
      end if
   end if
end do

bestmemit_XC=bestmem_XC

if( (refresh > 0) .and. (mod(iter,refresh)==0)) then
write(*,*)
print *,"# Iteration:",iter,": Objective function value:",bestval
print *,"# Best Member:", bestmem_XC
end if

iter=iter+1
! end if best fitness if smaller than expected value to reach
if ( bestval <= VTR ) then
  print *,"# The best fitness",bestval, "is smaller than VTR at generation #", iter
  print *,"# Best Member:", bestmem_XC
  exit
end if
end do
!!------end the evolutionary computation------------------------------!!
end subroutine DE_Fortran90


subroutine NelderMeadSimplex(offset3, start, ynewlo, st_initial, n, &
enl, patterndata, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata, icount, numres)
  use local
  use typedefs
  use NameListTypedefs
  use HDF5

  implicit none

  integer ( kind = 4 ), intent(in)                    :: n, objective
  real ( kind = 4 ), intent(inout)                    :: start(n)
  type(EBSDNameListType),INTENT(INOUT)                :: enl
  integer(HSIZE_T),intent(in)                         :: offset3(3)
  type(EBSDDIpreviewNameListType),INTENT(INOUT)       :: patterndata
  real(kind=4), intent(in)                            :: st_initial(3)
  type(MCCLNameListType),intent(inout)                :: mcnl
  type(EBSDMasterNameListType),intent(inout)          :: mpnl
  type(EBSDMCdataType),intent(inout)                  :: EBSDMCdata
  type(EBSDMPdataType),intent(inout)                  :: EBSDMPdata
  real ( kind = 4 ), intent(out)                      :: ynewlo
  integer ( kind = 4 ),intent(out)                    :: icount, numres
  real ( kind = 4 )                                   :: xmin(n),ynew(1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) kcount
  integer ( kind = 4 ) konvge
  real ( kind = 4 ) reqmin
  real ( kind = 4 ) step(n)
  
  call OMP_SET_NUM_THREADS(1)
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Apply Nelder-Mead Simplex to the Pattern Optimization'
        
  reqmin = 1.0D-06

  if (enl%applyDeformation.eq.'y') then
    step(1:n) = 0.001*(/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/)
  else
    step(1:n) = 0.001*(/1.0,1.0,1.0,1.0,1.0,1.0/)
  end if

  konvge = 10
  kcount = 300

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Starting point X:'
  write ( *, '(a)' ) ' '

  do i = 1, n
      write ( *, '(2x,g14.6)' ) start(i)
  end do

  call objective_function(offset3, start,st_initial, ynew(1), n, &
  enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)

  if (objective.eq.1) then
    ynew=ynew+1.0
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  F(X) = ', ynew(1)

  call nelmin ( n, start, xmin, ynewlo, reqmin, step, &
    konvge, kcount, icount, numres, ifault,st_initial, &
  enl, patterndata,  objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata, offset3)


  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Return code IFAULT = ', ifault
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Estimate of minimizing value X*:'
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,g14.6)' ) xmin(i)
  end do

  start=xmin

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  F(X*) = ', ynewlo

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of iterations = ', icount
  write ( *, '(a,i8)' ) '  Number of restarts =   ', numres

  return
  end subroutine NelderMeadSimplex

  subroutine nelmin ( n, start, xmin, ynewlo, reqmin, step, konvge, kcount, icount, numres, &
   ifault, st_initial, enl, patterndata,  objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata, offset3)
  
  !*****************************************************************************80
  !
  !! NELMIN minimizes a function using the Nelder-Mead algorithm.
  !
  !  Discussion:
  !
  !    This routine seeks the minimum value of a user-specified function.
  !
  !    Simplex function minimisation procedure due to Nelder and Mead (1965),
  !    as implemented by O'Neill(1971, Appl.Statist. 20, 338-45), with
  !    subsequent comments by Chambers+Ertel(1974, 23, 250-1), Benyon(1976,
  !    25, 97) and Hill(1978, 27, 380-2)
  !
  !    The function to be minimized must be defined by a function of
  !    the form
  !
  !      function fn ( x, f )
  !      real ( kind = 4 ) fn
  !      real ( kind = 4 ) x(*)
  !
  !    and the name of this subroutine must be declared EXTERNAL in the
  !    calling routine and passed as the argument FN.
  !
  !    This routine does not include a termination test using the
  !    fitting of a quadratic surface.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    27 February 2008
  !
  !  Author:
  !
  !    Original FORTRAN77 version by R ONeill.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    John Nelder, Roger Mead,
  !    A simplex method for function minimization,
  !    Computer Journal,
  !    Volume 7, 1965, pages 308-313.
  !
  !    R ONeill,
  !    Algorithm AS 47:
  !    Function Minimization Using a Simplex Procedure,
  !    Applied Statistics,
  !    Volume 20, Number 3, 1971, pages 338-345.
  !
  !  Parameters:
  !
  !    Input, external FN, the name of the function which evaluates
  !    the function to be minimized.
  !
  !    Input, integer ( kind = 4 ) N, the number of variables.
  !    0 < N is required.
  !
  !    Input/output, real ( kind = 4 ) START(N).  On input, a starting point
  !    for the iteration.  On output, this data may have been overwritten.
  !
  !    Output, real ( kind = 4 ) XMIN(N), the coordinates of the point which
  !    is estimated to minimize the function.
  !
  !    Output, real ( kind = 4 ) YNEWLO, the minimum value of the function.
  !
  !    Input, real ( kind = 4 ) REQMIN, the terminating limit for the variance
  !    of the function values.  0 < REQMIN is required.
  !
  !    Input, real ( kind = 4 ) STEP(N), determines the size and shape of the
  !    initial simplex.  The relative magnitudes of its elements should reflect
  !    the units of the variables.
  !
  !    Input, integer ( kind = 4 ) KONVGE, the convergence check is carried out
  !    every KONVGE iterations. 0 < KONVGE is required.
  !
  !    Input, integer ( kind = 4 ) KCOUNT, the maximum number of function
  !    evaluations.
  !
  !    Output, integer ( kind = 4 ) ICOUNT, the number of function evaluations
  !    used.
  !
  !    Output, integer ( kind = 4 ) NUMRES, the number of restarts.
  !
  !    Output, integer ( kind = 4 ) IFAULT, error indicator.
  !    0, no errors detected.
  !    1, REQMIN, N, or KONVGE has an illegal value.
  !    2, iteration terminated because KCOUNT was exceeded without convergence.
  !
    use local
    use typedefs
    use NameListTypedefs
    use HDF5

    implicit none
  
    integer ( kind = 4 ), intent(in)                    :: n, objective
    type(EBSDNameListType),INTENT(INOUT)                :: enl
    type(EBSDDIpreviewNameListType),INTENT(INOUT)       :: patterndata
    real(kind=4), intent(in)                            :: st_initial(3)
    type(MCCLNameListType),intent(inout)                :: mcnl
    type(EBSDMasterNameListType),intent(inout)          :: mpnl
    type(EBSDMCdataType),intent(inout)                  :: EBSDMCdata
    type(EBSDMPdataType),intent(inout)                  :: EBSDMPdata
    integer(HSIZE_T),intent(in)                          :: offset3(3)
    real ( kind = 4 ), intent(inout)  :: ynewlo
    integer ( kind = 4 ):: konvge,numres,kcount,icount,ifault
    real ( kind = 4 ) :: reqmin, step(n)
    real ( kind = 4 ) :: start(n)
    real ( kind = 4 ) :: xmin(n)
    real ( kind = 4 ), parameter :: ccoeff = 0.5D+00
    real ( kind = 4 ), parameter :: ecoeff = 2.0D+00
    real ( kind = 4 ), parameter :: eps = 0.1D+00
    integer ( kind = 4 ) :: i,ihi,ilo,j,jcount,l
    real ( kind = 4 ):: del, p(n,n+1),x, rq, p2star(n), pbar(n), pstar(n), y(n+1),y2star(1),ylo, z(1), ystar(1)
    real ( kind = 4 ), parameter :: rcoeff = 1.0D+00
    
   
  !
  !  Check the input parameters.
  !
    if ( reqmin <= 0.0D+00 ) then
      ifault = 1
      return
    end if
  
    if ( n < 1 ) then
      ifault = 1
      return
    end if
  
    if ( konvge < 1 ) then
      ifault = 1
      return
    end if
  !
  !  Initialization.
  !
    icount = 0
    numres = 0
    jcount = konvge
    del = 1.0D+00
    rq = reqmin * real ( n, kind = 4 )
  !
  !  Initial or restarted loop.
  !
    do
  
      p(1:n,n+1) = start(1:n)
      !y(n+1) = fn ( start )
      call objective_function(offset3, start,st_initial,y(n+1), n, &
      enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)

      if (objective.eq.1) then
      y(n+1)=y(n+1)+1.0
      end if
      icount = icount + 1
  !
  !  Define the initial simplex.
  !
      do j = 1, n
        x = start(j)
        start(j) = start(j) + step(j) * del
        p(1:n,j) = start(1:n)
        call objective_function(offset3, start,st_initial,y(j), n, &
        enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
      if (objective.eq.1) then
        y(j)=y(j)+1.0
      end if
        icount = icount + 1
        start(j) = x
      end do
      
  !  Find highest and lowest Y values.  YNEWLO = Y(IHI) indicates
  !  the vertex of the simplex to be replaced.
  !
      ilo = minloc ( y(1:n+1), 1 )
      ylo = y(ilo)
  !
  !  Inner loop.
  !
      do while ( icount < kcount )
  !
  !  YNEWLO is, of course, the HIGHEST value???
  !
        ihi = maxloc ( y(1:n+1), 1 )
        ynewlo = y(ihi)
  !
  !  Calculate PBAR, the centroid of the simplex vertices
  !  excepting the vertex with Y value YNEWLO.
  !
        do i = 1, n
          pbar(i) = ( sum ( p(i,1:n+1) ) - p(i,ihi) ) / real ( n, kind = 4 )
        end do
  !
  !  Reflection through the centroid.
  !
        pstar(1:n) = pbar(1:n) + rcoeff * ( pbar(1:n) - p(1:n,ihi) )
        call objective_function(offset3, pstar,st_initial,ystar, n, &
        enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
        if (objective.eq.1) then
        ystar=ystar+1.0
        end if
        !ystar = fn ( pstar )
        icount = icount + 1
  !
  !  Successful reflection, so extension.
  !
        if ( ystar(1) < ylo ) then
  
          p2star(1:n) = pbar(1:n) + ecoeff * ( pstar(1:n) - pbar(1:n) )
          call objective_function(offset3, p2star,st_initial,y2star, n, &
          enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
          if (objective.eq.1) then
          y2star=y2star+1.0
          end if
          icount = icount + 1
  !
  !  Retain extension or contraction.
  !
          if ( ystar(1) < y2star(1) ) then
            p(1:n,ihi) = pstar(1:n)
            y(ihi) = ystar(1)
          else
            p(1:n,ihi) = p2star(1:n)
            y(ihi) = y2star(1)
          end if
  !
  !  No extension.
  !
        else
  
          l = 0
          do i = 1, n + 1
            if ( ystar(1) < y(i) ) then
              l = l + 1
            end if
          end do
  
          if ( 1 < l ) then
  
            p(1:n,ihi) = pstar(1:n)
            y(ihi) = ystar(1)
  !
  !  Contraction on the Y(IHI) side of the centroid.
  !
          else if ( l == 0 ) then
  
            p2star(1:n) = pbar(1:n) + ccoeff * ( p(1:n,ihi) - pbar(1:n) )
            call objective_function(offset3, p2star,st_initial,y2star, n,&
            enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
            if (objective .eq. 1) then
              y2star=y2star+1.0
            end if
           ! y2star = fn ( p2star )
            icount = icount + 1
  !
  !  Contract the whole simplex.
  !
            if ( y(ihi) < y2star(1) ) then
  
              do j = 1, n + 1
                p(1:n,j) = ( p(1:n,j) + p(1:n,ilo) ) * 0.5D+00
                xmin(1:n) = p(1:n,j)
                call objective_function(offset3, xmin,st_initial,y(j), n, &
                enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)

              if (objective .eq. 1) then
              y(j)=y(j)+1.0
              end if
  
                icount = icount + 1
              end do
              ilo = minloc ( y(1:n+1), 1 )
              ylo = y(ilo)
  
              cycle
  !
  !  Retain contraction.
  !
            else
              p(1:n,ihi) = p2star(1:n)
              y(ihi) = y2star(1)
            end if
  !
  !  Contraction on the reflection side of the centroid.
  !
          else if ( l == 1 ) then
  
            p2star(1:n) = pbar(1:n) + ccoeff * ( pstar(1:n) - pbar(1:n) )
            call objective_function(offset3, p2star,st_initial,y2star, n, &
            enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
            if (objective.eq.1) then
            y2star=y2star+1.0
            end if
            icount = icount + 1
  !
  !  Retain reflection?
  !
            if ( y2star(1) <= ystar(1) ) then
              p(1:n,ihi) = p2star(1:n)
              y(ihi) = y2star(1)
            else
              p(1:n,ihi) = pstar(1:n)
              y(ihi) = ystar(1)
            end if
  
          end if
  
        end if
  !
  !  Check if YLO improved.
  !
        if ( y(ihi) < ylo ) then
          ylo = y(ihi)
          ilo = ihi
        end if
  
        jcount = jcount - 1
  
        if ( 0 < jcount ) then
          cycle
        end if
  !
  !  Check to see if minimum reached.
  !
        if ( icount <= kcount ) then
  
          jcount = konvge
  
          x = sum ( y(1:n+1) ) / real ( n + 1, kind = 4 )
          z = sum ( ( y(1:n+1) - x )**2 )
  
          if ( z(1) <= rq ) then
            exit
          end if
  
        end if
  
      end do
  !
  !  Factorial tests to check that YNEWLO is a local minimum.
  !
      xmin(1:n) = p(1:n,ilo)
      ynewlo = y(ilo)
      !print *,"ynewlo=",ynewlo
      if ( kcount < icount ) then
        ifault = 2
        exit
      end if
  
      ifault = 0
  
      do i = 1, n
        del = step(i) * eps
        xmin(i) = xmin(i) + del
        call objective_function(offset3, xmin,st_initial,z, n, &
        enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
         if (objective.eq.1) then
          z=z+1.0
         end if
        icount = icount + 1
        if ( z(1) < ynewlo ) then
          ifault = 2
          exit
        end if
        xmin(i) = xmin(i) - del - del
        call objective_function(offset3, xmin,st_initial,z, n, &
        enl, patterndata, 1, objective, mcnl, mpnl, EBSDMCdata, EBSDMPdata)
        if (objective.eq.1) then
          z=z+1.0
        end if
        icount = icount + 1
        if ( z(1) < ynewlo ) then
          ifault = 2
          exit
        end if
        xmin(i) = xmin(i) + del
      end do
  
      if ( ifault == 0 ) then
        exit
      end if
  !
  !  Restart the procedure.
  !
      start(1:n) = xmin(1:n)
      del = eps
      numres = numres + 1
  
    end do
   
    return
  end subroutine nelmin


end module PSO
