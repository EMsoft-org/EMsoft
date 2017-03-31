module rng 
implicit none 
private 
public :: rng_t, rng_seed, rng_uniform

! Dimension of the state 
integer, parameter :: ns = 4 
!DEC$ ATTRIBUTES DLLEXPORT :: ns

! Default seed vector 
integer, parameter, dimension(ns) :: default_seed = (/ 521288629, 362436069, 16163801, 1131199299 /) 
!DEC$ ATTRIBUTES DLLEXPORT :: default_seed

! A data type for storing the state of the RNG 
type :: rng_t 
  integer, dimension(ns) :: state = default_seed 
end type rng_t 

contains 



! Seeds the RNG using a single integer and a default seed vector. 
recursive subroutine rng_seed(self, seed) 
!DEC$ ATTRIBUTES DLLEXPORT :: rng_seed
type(rng_t), intent(inout) :: self 
integer, intent(in) :: seed 
self%state(1) = seed 
self%state(2:ns) = default_seed(2:ns) 
end subroutine rng_seed 




! Draws a uniform real number on [0,1]. 
recursive function rng_uniform(self) result(u) 
!DEC$ ATTRIBUTES DLLEXPORT :: rng_uniform
type(rng_t), intent(inout) :: self 
real :: u
integer :: imz 

imz = self%state(1) - self%state(3) 

if (imz < 0) imz = imz + 2147483579 

self%state(1) = self%state(2) 
self%state(2) = self%state(3) 
self%state(3) = imz 
self%state(4) = 69069 * self%state(4) + 1013904243 
imz = imz + self%state(4) 
u = 0.5d0 + 0.23283064d-9 * imz 
end function rng_uniform 




end module rng
