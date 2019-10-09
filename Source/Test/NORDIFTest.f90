! ###################################################################
! Copyright (c) 2018-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:NORDIFTest.f90
!--------------------------------------------------------------------------
!
! module: NORDIFTest
!
!> @author: Håkon Wiik Ånes, Norwegian University of Science and Technology
!
!> @brief: test reading of patterns from a NORDIF binary file
!
!> @date: 10/09/19 HWÅ 1.0 original
!
!--------------------------------------------------------------------------

module NORDIFTest

    implicit none

    character(len=14) :: fname = 'NORDIFTest.dat'

    contains

    subroutine createNORDIFFile()

        implicit none

        integer :: funit
        character, dimension(36) :: patterns

        patterns = char([ &
            32, 236, 126, 127, 255, 248, 197, 1, 0, &
            85, 99, 81, 255, 23, 30, 59, 0, 130, &
            138, 192, 201, 255, 0, 245, 184, 34, 178, &
            167, 40, 255, 155, 233, 70, 0, 217, 152])

        open(newunit=funit, file=fname, access='stream')
        write(funit) patterns
        close(funit, status='keep')

    end subroutine createNORDIFFile

    subroutine deleteNORDIFFile()

        implicit none

        integer :: funit

        open(newunit=funit, file=fname)
        close(funit, status='delete')

    end subroutine deleteNORDIFFile

!    function getSingleExpPatternTest(funit)
!    end function getSingleExpPatternTest
!
!    function getExpPatternRowTest(funit)
!    end function getExpPatternRowTest

!    subroutine NORDIFExecuteTest()
!    end subroutine NORDIFExecuteTest

end module NORDIFTest

program NORDIFExecuteTest

    use NORDIFTest

    implicit none

    integer :: funit
    character, dimension(36) :: patterns

    call createNORDIFFile()

!    open(newunit=funit, file='NORDIFTest.dat', access='stream')
!    rewind(funit)
!    read(funit) patterns
!    close(funit, status='keep')

!    write(*,*) ichar(patterns)

!    call deleteNORDIFFile(funit)

end program NORDIFExecuteTest
