
module NameListHDFwritersTest

contains 

!====================================
subroutine NameListHDFwritersExecuteTest(result) &
           bind(c, name='NameListHDFwritersExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: NameListHDFwritersExecuteTest

use,INTRINSIC :: ISO_C_BINDING

use local


IMPLICIT NONE

! This variable needs to be declared
integer(C_INT32_T),INTENT(OUT)               :: result



! When you are done with the testing, set this variable to 0=Success, 1=Testing Failed
result = 1

end subroutine NameListHDFwritersExecuteTest


end module NameListHDFwritersTest

