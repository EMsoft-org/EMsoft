
!--------------------------------------------------------------------------
! EMsoft:multibeams.f90
!--------------------------------------------------------------------------
!
! MODULE: multibeams
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Anything related to dynamical diffraction
! 
!> @details these variables are passed back and forth between the main
!> multi beam program and the various subroutines.  
!
!> @todo prepend MB to all the variable names and propagate into other code
!
!> @date   10/13/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date    3/14/02 MDG 2.2 added CalcDynMat routine
!--------------------------------------------------------------------------
module multibeams

use local

integer(kind=irg),parameter    :: numr = 500                    ! max number of families of reflections in zone
integer(kind=irg)              :: family(numr,48,3), numfam(numr) 
integer(kind=irg),allocatable  :: idx(:)
real(kind=sgl)                 :: glen(numr)                    ! length of g-vectors
real(kind=sgl),allocatable     :: gm(:), V(:,:)
logical,allocatable            :: al(:)                         ! array of allowed reflections
!DEC$ ATTRIBUTES DLLEXPORT :: numr
!DEC$ ATTRIBUTES DLLEXPORT :: family
!DEC$ ATTRIBUTES DLLEXPORT :: numfam
!DEC$ ATTRIBUTES DLLEXPORT :: idx
!DEC$ ATTRIBUTES DLLEXPORT :: glen
!DEC$ ATTRIBUTES DLLEXPORT :: gm
!DEC$ ATTRIBUTES DLLEXPORT :: V
!DEC$ ATTRIBUTES DLLEXPORT :: al

end module multibeams

