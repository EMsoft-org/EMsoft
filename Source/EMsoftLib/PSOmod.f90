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
    

end module PSO
