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
! EMsoft:JSONsupport.f90
!--------------------------------------------------------------------------
!
! MODULE: JSONsupport
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines for conversion between json and nml files and reading of json files
!
!> @date  08/11/15 MDG 1.0 original
!> @date  08/12/15 MDG 1.1 added all routines currently also in NameListHDFwriters.f90
!> @date  08/12/15 MDG 1.2 replaced all the json_failed stuff by short routine JSON_failtest
!> @date  11/20/15 MDG 1.3 started defect file format
!--------------------------------------------------------------------------
module JSONsupport

use local
use typedefs
use, intrinsic :: iso_fortran_env , only: error_unit, wp => real64
use NameListTypedefs
use json_module
use stringconstants

IMPLICIT NONE

contains

! these are the jsonfortran public functions from jsonmodule.mod
!
!    public :: json_add                   ! add data to a JSON structure
!    public :: json_check_for_errors      ! check for error and get error message
!    public :: json_clear_exceptions      ! clear exceptions
!    public :: json_count                 ! count the number of children
!    public :: json_create_array          ! allocate a json_value array
!    public :: json_create_double         ! allocate a json_value double
!    public :: json_create_integer        ! allocate a json_value integer
!    public :: json_create_logical        ! allocate a json_value logical
!    public :: json_create_null           ! allocate a json_value null
!    public :: json_create_object         ! allocate a json_value object
!    public :: json_create_string         ! allocate a json_value string
!    public :: json_destroy               ! clear a JSON structure (destructor)
!    public :: json_failed                ! check for error
!    public :: json_get                   ! get data from the JSON structure
!    public :: json_get_child             ! get a child of a json_value
!    public :: json_info                  ! get info about a json_value
!    public :: json_initialize            ! to initialize the module
!    public :: json_parse                 ! read a JSON file and populate the structure
!    public :: json_print                 ! print the JSON structure to a file
!    public :: json_print_to_string       ! write the JSON structure to a string
!    public :: json_remove                ! remove from a JSON structure
!    public :: json_remove_if_present     ! remove from a JSON structure (if it is present)
!    public :: json_update                ! update a value in a JSON structure
!    public :: json_traverse              ! to traverse all elements of a JSON structure
!    public :: json_print_error_message   !
!    public :: to_unicode                 ! Function to convert from 'DEFAULT' to 'ISO_10646' strings

!--------------------------------------------------------------------------
!
! FUNCTION:JSON_minify
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief remove all fortran comment lines from a JSON input file and store file in tmp folder
!
!> @param jsonname json file name
!> @param jsonreturn filename of stripped down JSON input file (in EMsoft tmp folder)
!
!> @date 05/11/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive function JSON_minify(jsonname) result(jsonreturn)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_minify

IMPLICIT NONE

character(fnlen),INTENT(IN)          :: jsonname
character(fnlen)                     :: jsonreturn

! the JSON syntax standard does not allow for comment lines/statements of any kind.
! In the XXX.jtemplate files, comment lines starting with the f90 ! comment character
! are interspersed with the name-value pairs.  Those are present to make it easier for 
! the user to figure out what each variable means.  They need to be removed before the 
! file is sent to the json parser routine, and this is done here.  The stripped down
! file is stored in the EMsoft tmp folder with the same name and must be deleted after 
! the parsing has been completed.

! with the 5.3 version of json-fortran, it is possible to convert a json file to a namelist
! file, so since we are already set up to handle namelist files, this will signficantly 
! simplify the implementation of the JSON format...

! jsonname is 

jsonreturn = ''

end function JSON_minify

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSON_failtest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief executes the json_fail routine; mostly to shorten the remaining code a little
!
!> @param error_cnt error counter
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSON_failtest(error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_failtest

IMPLICIT NONE

integer(kind=irg),INTENT(INOUT)         :: error_cnt
!f2py intent(in,out) ::  error_cnt

if (json_failed().eqv..TRUE.) then
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
end if

end subroutine JSON_failtest

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSON_writeNMLintegers
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a series of integer namelist entries to a json structure 
!
!> @param inp json structure pointer
!> @param io_int list of integers
!> @param intlist list of string descriptors
!> @param n_int number of entries
!> @param error_cnt error counter
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_writeNMLintegers

IMPLICIT NONE

type(json_value),INTENT(INOUT),pointer                :: inp
!f2py intent(in,out) ::  inp
integer(kind=irg),INTENT(IN)                          :: n_int
integer(kind=irg),INTENT(IN)                          :: io_int(n_int)
character(20),INTENT(IN)                              :: intlist(n_int)
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset

do i=1,n_int
  dataset = intlist(i)
  call json_add(inp, dataset, io_int(i)); call JSON_failtest(error_cnt)
end do

end subroutine JSON_writeNMLintegers

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSON_writeNMLreals
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a series of real namelist entries to a json structure
!
!> @param inp pointer to json_value 
!> @param io_real list of reals
!> @param reallist list of string descriptors
!> @param n_real number of entries
!> @param error_cnt error counter
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_writeNMLreals

IMPLICIT NONE

type(json_value),INTENT(INOUT),pointer                :: inp
!f2py intent(in,out) ::  inp
integer(kind=irg),INTENT(IN)                          :: n_real
real(kind=sgl),INTENT(IN)                             :: io_real(n_real)
character(20),INTENT(IN)                              :: reallist(n_real)

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

do i=1,n_real
  dataset = reallist(i)
  call json_add(inp, dataset, dble(io_real(i))); call JSON_failtest(error_cnt)
end do

end subroutine JSON_writeNMLreals

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSON_writeNMLdoubles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a series of double namelist entries to a json structure
!
!> @param inp pointer to json_value 
!> @param io_real list ofadoubles 
!> @param reallist list of string descriptors
!> @param n_real number of entries
!> @param error_cnt error counter
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSON_writeNMLdoubles(inp, io_real, reallist, n_real, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_writeNMLdoubles

IMPLICIT NONE

type(json_value),INTENT(INOUT),pointer                :: inp
!f2py intent(in,out) ::  inp
integer(kind=irg),INTENT(IN)                          :: n_real
real(kind=dbl),INTENT(IN)                             :: io_real(n_real)
character(20),INTENT(IN)                              :: reallist(n_real)

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

do i=1,n_real
  dataset = reallist(i)
  call json_add(inp, dataset, io_real(i)); call JSON_failtest(error_cnt)
end do

end subroutine JSON_writeNMLdoubles

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSON_initpointers
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the necessary pointers to write a namelist json file
!
!> @param inp pointer to json_value 
!> @param io_real list of reals
!> @param reallist list of string descriptors
!> @param n_real number of entries
!> @param error_cnt error counter
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_initpointers

IMPLICIT NONE

type(json_value),INTENT(INOUT),pointer  :: p, inp
!f2py intent(in,out) ::  p, inp
character(fnlen),INTENT(IN)             :: jsonname, namelistname
integer(kind=irg),INTENT(INOUT)         :: error_cnt
!f2py intent(in,out) ::  error_cnt

! initialize the json state variables
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! create the json root pointer
call json_create_object(p,trim(jsonname)); call JSON_failtest(error_cnt)

! we'll use the namelist name to configure the inp structure and add it to p
call json_create_object(inp,trim(namelistname)); call JSON_failtest(error_cnt) 
call json_add(p, inp); call JSON_failtest(error_cnt)

end subroutine JSON_initpointers

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSON_cleanuppointers
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief clean up the pointers and write the json file
!
!> @param p pointer to json_value 
!> @param inp pointer to json_value 
!> @param jsonname json output file name
!> @param error_cnt error counter
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSON_cleanuppointers(p, inp, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSON_cleanuppointers

use io 

IMPLICIT NONE

type(json_value),INTENT(INOUT),pointer  :: p, inp
!f2py intent(in,out) ::  p, inp
character(fnlen),INTENT(IN)             :: jsonname
integer(kind=irg),INTENT(INOUT)         :: error_cnt
!f2py intent(in,out) ::  error_cnt

character(fnlen)                        :: fname

! get rid of inp
nullify(inp)

! write the json file
fname = EMsoft_toNativePath(jsonname)
open(unit=dataunit, file=trim(fname), status='REPLACE')
call json_print(p,dataunit); call JSON_failtest(error_cnt)
close(dataunit)

! final cleanup
call json_destroy(p); call JSON_failtest(error_cnt)

end subroutine JSON_cleanuppointers


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! from here on we have the Namelist->json conversion routines for all the 
! namelists defined in the NameListTypedefs.f90 file.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteKosselNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into json file
!
!> @param knl Kossel name list structure
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteKosselNameList(knl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteKosselNameList

use ISO_C_BINDING

IMPLICIT NONE

type(KosselNameListType),INTENT(IN)                   :: knl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'Kossellist'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! then we need to add all the necessary fields to the inp structure

! write all the single integers
io_int = (/ knl%stdout, knl%numthick, knl%npix, knl%maxHOLZ, knl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'numthick'
intlist(3) = 'npix'
intlist(4) = 'maxHOLZ'
intlist(5) = 'nthreads'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! integer vectors
dataset = SC_k
call json_add(inp, dataset, knl%k); call JSON_failtest(error_cnt)

dataset = SC_fn
call json_add(inp, dataset, knl%fn); call JSON_failtest(error_cnt)

! write all the single reals
io_real = (/ knl%voltage, knl%dmin, knl%convergence, knl%startthick, knl%thickinc, knl%minten /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'convergence'
reallist(4) = 'startthick'
reallist(5) = 'thickinc'
reallist(6) = 'minten'
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

! write all the strings
dataset = SC_xtalname
call json_add(inp, dataset, knl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_outname
call json_add(inp, dataset, knl%outname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteKosselNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteKosselMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into json file
!
!> @param knl Kossel name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteKosselMasterNameList(knl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteKosselMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(KosselMasterNameListType),INTENT(IN)             :: knl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 4, n_real = 5
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'Kosselmasterlist'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! then we need to add all the necessary fields to the inp structure

! write all the single integers
io_int = (/ knl%stdout, knl%numthick, knl%npx, knl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'numthick'
intlist(3) = 'npx'
intlist(4) = 'nthreads' 
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write all the single reals
io_real = (/ knl%voltage, knl%dmin, knl%startthick, knl%thickinc, knl%tfraction /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'startthick'
reallist(4) = 'thickinc'
reallist(5) = 'tfraction' 
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

! write all the strings
dataset = SC_Kosselmode
call json_add(inp, dataset, knl%Kosselmode); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, knl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_outname
call json_add(inp, dataset, knl%outname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteKosselMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteMCNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into JSON file
!
!> @param mcnl Monte Carlo name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/11/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteMCNameList(mcnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteMCNameList

use ISO_C_BINDING

IMPLICIT NONE

type(MCNameListType),INTENT(INOUT)                    :: mcnl
!f2py intent(in,out) ::  mcnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 5, n_real = 7
integer(kind=irg)                                     :: io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, sval(1), namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'MCdata'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%primeseed, mcnl%num_el, mcnl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'primeseed'
intlist(4) = 'num_el'
intlist(5) = 'nthreads'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write all the single doubles
io_real = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep /)
reallist(1) = 'sig'
reallist(2) = 'omega'
reallist(3) = 'EkeV'
reallist(4) = 'Ehistmin'
reallist(5) = 'Ebinsize'
reallist(6) = 'depthmax'
reallist(7) = 'depthstep'
call JSON_writeNMLdoubles(inp, io_real, reallist, n_real, error_cnt)

! write all the strings
dataset = SC_MCmode
call json_add(inp, dataset, mcnl%MCmode); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, mcnl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_dataname
call json_add(inp, dataset, mcnl%dataname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteMCNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteMCCLNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @param mcnl Monte Carlo name list structure
!
!> @date 03/21/15 MDG 1.0 new routine
!> @date 09/09/15 MDG 1.1 added devid
!> @date 10/12/15 SS  1.2 changes to handle new mc program
!--------------------------------------------------------------------------
recursive subroutine JSONwriteMCCLNameList(mcnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteMCCLNameList

use ISO_C_BINDING

IMPLICIT NONE

type(MCCLNameListType),INTENT(INOUT)                  :: mcnl
!f2py intent(in,out) ::  mcnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 8, n_real_bse1 = 9, n_real_full = 7
integer(kind=irg)                                     :: io_int(n_int)
real(kind=dbl)                                        :: io_real_bse1(n_real_bse1), io_real_full(n_real_full)
character(20)                                         :: intlist(n_int), reallist_bse1(n_real_bse1), reallist_full(n_real_full)
character(fnlen)                                      :: dataset, sval(1), namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'MCCLdata'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%globalworkgrpsz, mcnl%num_el, mcnl%totnum_el, mcnl%multiplier, mcnl%devid, mcnl%platid /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'globalworkgrpsz'
intlist(4) = 'num_el'
intlist(5) = 'totnum_el'
intlist(6) = 'multiplier'
intlist(7) = 'devid'
intlist(8) = 'platid'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write all the single doubles for bse1 mode
if (mcnl%mode .eq. 'bse1') then
   io_real_bse1 = (/ mcnl%sigstart, mcnl%sigend, mcnl%sigstep, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, &
             mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep /)
   reallist_bse1(1) = 'sigstart'
   reallist_bse1(2) = 'sigend'
   reallist_bse1(3) = 'sigstep'
   reallist_bse1(4) = 'omega'
   reallist_bse1(5) = 'EkeV'
   reallist_bse1(6) = 'Ehistmin'
   reallist_bse1(7) = 'Ebinsize'
   reallist_bse1(8) = 'depthmax'
   reallist_bse1(9) = 'depthstep'
   call JSON_writeNMLdoubles(inp, io_real_bse1, reallist_bse1, n_real_bse1, error_cnt)
else if (mcnl%mode .eq. 'full') then
   io_real_full = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, &
             mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep /)
   reallist_full(1) = 'sig'
   reallist_full(2) = 'omega'
   reallist_full(3) = 'EkeV'
   reallist_full(4) = 'Ehistmin'
   reallist_full(5) = 'Ebinsize'
   reallist_full(6) = 'depthmax'
   reallist_full(7) = 'depthstep'
   call JSON_writeNMLdoubles(inp, io_real_full, reallist_full, n_real_full, error_cnt)
end if
! write all the strings
dataset = SC_MCmode
call json_add(inp, dataset, mcnl%MCmode); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, mcnl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_dataname
call json_add(inp, dataset, mcnl%dataname); call JSON_failtest(error_cnt)

dataset = SC_mode
call json_add(inp, dataset, mcnl%mode); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteMCCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteMCCLMultiLayerNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param mcnl Monte Carlo name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/11/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteMCCLMultiLayerNameList(mcnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteMCCLMultiLayerNameList

use ISO_C_BINDING

IMPLICIT NONE

type(MCCLMultiLayerNameListType),INTENT(INOUT)        :: mcnl
!f2py intent(in,out) ::  mcnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 5, n_real = 9
integer(kind=irg)                                     :: io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'MCCLdata'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%globalworkgrpsz, mcnl%num_el, mcnl%totnum_el /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'globalworkgrpsz'
intlist(4) = 'num_el'
intlist(5) = 'totnum_el'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write all the single doubles
io_real = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep, &
             mcnl%filmthickness, mcnl%filmstep /)
reallist(1) = 'sig'
reallist(2) = 'omega'
reallist(3) = 'EkeV'
reallist(4) = 'Ehistmin'
reallist(5) = 'Ebinsize'
reallist(6) = 'depthmax'
reallist(7) = 'depthstep'
reallist(8) = 'filmthickness'
reallist(9) = 'filmstep'
call JSON_writeNMLdoubles(inp, io_real, reallist, n_real, error_cnt)

! write all the strings
dataset = SC_MCmode
call json_add(inp, dataset, mcnl%MCmode); call JSON_failtest(error_cnt)

dataset = SC_xtalnamefilm
call json_add(inp, dataset, mcnl%xtalname_film); call JSON_failtest(error_cnt)

dataset = SC_xtalnamesubs
call json_add(inp, dataset, mcnl%xtalname_subs); call JSON_failtest(error_cnt)

dataset = SC_dataname
call json_add(inp, dataset, mcnl%dataname); call JSON_failtest(error_cnt)

dataset = SC_mode
call json_add(inp, dataset, mcnl%mode); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteMCCLMultiLayerNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteEBSDMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param emnl EBSD master name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteEBSDMasterNameList(emnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteEBSDMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(EBSDMasterNameListType),INTENT(INOUT)            :: emnl
!f2py intent(in,out) ::  emnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 6, n_real = 1
integer(kind=irg)                                     :: io_int(n_int), restart, uniform
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'EBSDmastervars'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers

if (emnl%restart.eqv..TRUE.) then
  restart = 1
else
  restart = 0
end if
if (emnl%uniform.eqv..TRUE.) then
  uniform = 1
else
  uniform = 0
end if
io_int = (/ emnl%stdout, emnl%npx, emnl%Esel, emnl%nthreads, restart, uniform /)
intlist(1) = 'stdout'
intlist(2) = 'npx'
intlist(3) = 'Esel'
intlist(4) = 'nthreads'
intlist(5) = 'restart'
intlist(6) = 'uniform'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write a single real
dataset = SC_dmin
call json_add(inp, dataset, dble(emnl%dmin)); call JSON_failtest(error_cnt)

! write all the strings
dataset = SC_outname
call json_add(inp, dataset, emnl%outname); call JSON_failtest(error_cnt)

dataset = SC_energyfile
call json_add(inp, dataset, emnl%energyfile); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteEBSDMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteEBSDclusterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param emnl EBSD cluster name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 12/28/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteEBSDclusterNameList(emnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteEBSDclusterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(EBSDclusterNameListType),INTENT(INOUT)           :: emnl
!f2py intent(in,out) ::  emnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'EBSDclustervars'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ emnl%NClusters, emnl%NIterations, emnl%binfactor /)
intlist(1) = 'NClusters'
intlist(2) = 'NIterations'
intlist(3) = 'binfactor'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write all the strings
dataset = SC_inputfilename
call json_add(inp, dataset, emnl%inputfilename); call JSON_failtest(error_cnt)

dataset = SC_groupname
call json_add(inp, dataset, emnl%groupname); call JSON_failtest(error_cnt)

dataset = SC_datasetname
call json_add(inp, dataset, emnl%datasetname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteEBSDclusterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteECPMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param ecpnl ECP master name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!> @date 09/15/15 SS  1.1 changes after clean up of ECPmasterListType
!--------------------------------------------------------------------------
recursive subroutine JSONwriteECPMasterNameList(ecpnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteECPMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(ECPMasterNameListType),INTENT(INOUT)             :: ecpnl
!f2py intent(in,out) ::  ecpnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 4, n_real = 1
integer(kind=irg)                                     :: io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'ECPmastervars'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ ecpnl%stdout, ecpnl%Esel, ecpnl%npx, ecpnl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'Esel'
intlist(3) = 'npx'
intlist(4) = 'nthreads'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

!dataset = 'distort'
!call json_add(inp, dataset, ecpnl%distort); call JSON_failtest(error_cnt)

! integer vectors
!dataset = 'fn'
!call json_add(inp, dataset, dble(ecpnl%fn)); call JSON_failtest(error_cnt)

! write all the single doubles
io_real = (/ ecpnl%dmin /)
reallist(1) = 'dmin'
!reallist(2) = 'startthick' 
call JSON_writeNMLdoubles(inp, io_real, reallist, n_real, error_cnt)

! 3-vectors (real)
!dataset = 'abcdist'
!call json_add(inp, dataset, dble(ecpnl%abcdist)); call JSON_failtest(error_cnt)

!dataset = 'albegadist'
!call json_add(inp, dataset, dble(ecpnl%albegadist)); call JSON_failtest(error_cnt)

! write all the strings
dataset = SC_energyfile
call json_add(inp, dataset, ecpnl%energyfile); call JSON_failtest(error_cnt)

dataset = SC_compmode
call json_add(inp, dataset, ecpnl%compmode); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteECPMasterNameList
!
!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteEBSDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param enl EBSD name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteEBSDNameList(enl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteEBSDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)                  :: enl
!f2py intent(in,out) ::  enl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 6, n_real = 9
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: t(1)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'EBSDdata'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ enl%stdout, enl%numsx, enl%numsy, enl%binning, enl%nthreads, enl%energyaverage /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'numsy'
intlist(4) = 'binning'
intlist(5) = 'nthreads'
intlist(6) = 'energyaverage'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)


! write all the single reals 
io_real = (/ enl%L, enl%thetac, enl%delta, enl%xpc, enl%ypc, enl%energymin, enl%energymax, enl%gammavalue, enl%alphaBD /)
reallist(1) = 'L'
reallist(2) = 'thetac'
reallist(3) = 'delta'
reallist(4) = 'xpc'
reallist(5) = 'ypc'
reallist(6) = 'energymin'
reallist(7) = 'energymax'
reallist(8) = 'gammavalue'
reallist(9) = 'alphaBD'
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

! a 4-vector
dataset = SC_axisangle
call json_add(inp, dataset, dble(enl%axisangle)); call JSON_failtest(error_cnt)

! a few doubles
dataset = SC_beamcurrent
call json_add(inp, dataset, enl%beamcurrent); call JSON_failtest(error_cnt)

dataset = SC_dwelltime
call json_add(inp, dataset, enl%dwelltime); call JSON_failtest(error_cnt)

! write all the strings
dataset = SC_maskpattern
call json_add(inp, dataset, enl%maskpattern); call JSON_failtest(error_cnt)

dataset = SC_scalingmode
call json_add(inp, dataset, enl%scalingmode); call JSON_failtest(error_cnt)

dataset = SC_eulerconvention
call json_add(inp, dataset, enl%eulerconvention); call JSON_failtest(error_cnt)

dataset = SC_outputformat
call json_add(inp, dataset, enl%outputformat); call JSON_failtest(error_cnt)

dataset = SC_energyfile
call json_add(inp, dataset, enl%energyfile); call JSON_failtest(error_cnt)

dataset = SC_masterfile
call json_add(inp, dataset, enl%masterfile); call JSON_failtest(error_cnt)

dataset = SC_anglefile
call json_add(inp, dataset, enl%anglefile); call JSON_failtest(error_cnt)

dataset = SC_datafile
call json_add(inp, dataset, enl%datafile); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteEBSDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteECPNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param ecpnl ECP namelist structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!> @date 09/15/15 SS  1.1 changes after modification of ECPListType
!> @date 10/15/15 SS  1.2 changes for release
!--------------------------------------------------------------------------
recursive subroutine JSONwriteECPNameList(ecpnl, jsonname, error_cnt, twolayerflag)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteECPNameList

use ISO_C_BINDING
use error

IMPLICIT NONE

type(ECPNameListType),INTENT(INOUT)                   :: ecpnl
!f2py intent(in,out) ::  ecpnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt
logical,INTENT(IN)                                    :: twolayerflag

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 2
integer(kind=irg)                                     :: n_real, istat
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl),allocatable                            :: io_real(:)
character(20),allocatable                             :: reallist(:)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'ECPlist'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ ecpnl%nthreads, ecpnl%npix /)
intlist(1) = 'nthreads'
intlist(2) = 'npix'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

if (twolayerflag) then
! integer vectors
dataset = SC_fnf
    call json_add(inp, dataset, ecpnl%fn_f); call JSON_failtest(error_cnt)

dataset = SC_fns
    call json_add(inp, dataset, ecpnl%fn_s); call JSON_failtest(error_cnt)

dataset = SC_gF
    call json_add(inp, dataset, ecpnl%gF); call JSON_failtest(error_cnt)

dataset = SC_gS
    call json_add(inp, dataset, ecpnl%gS); call JSON_failtest(error_cnt)

dataset = SC_tF
    call json_add(inp, dataset, ecpnl%tF); call JSON_failtest(error_cnt)

dataset = SC_tS
    call json_add(inp, dataset, ecpnl%tS); call JSON_failtest(error_cnt)

   n_real = 8
    allocate(reallist(n_real),io_real(n_real),stat=istat)
    if (istat .ne. 0) then
        call FatalError('HDFwriteECPNameList','Cannot allocate the reallist array')
    end if
! write all the single reals
    io_real = (/ ecpnl%dmin, ecpnl%thetac, sngl(ecpnl%sampletilt), ecpnl%workingdistance, &
             ecpnl%filmthickness, ecpnl%gammavalue, ecpnl%Rin, ecpnl%Rout /)
    reallist(1) = 'dmin'
    reallist(2) = 'thetac'
    reallist(3) = 'sampletilt'
    reallist(4) = 'workingdistance'
    reallist(5) = 'filmthickness'
    reallist(6) = 'gammavalue'
    reallist(7) = 'Rin'
    reallist(8) = 'Rout'

    call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

else

     n_real = 6
    allocate(reallist(n_real),io_real(n_real),stat=istat)
    if (istat .ne. 0) then
        call FatalError('HDFwriteECPNameList','Cannot allocate the reallist array')
    end if
! write all the single reals
    io_real = (/ ecpnl%thetac, sngl(ecpnl%sampletilt), &
                 ecpnl%gammavalue, ecpnl%workingdistance, ecpnl%Rin, ecpnl%Rout /)
    reallist(1) = 'thetac'
    reallist(2) = 'sampletilt'
    reallist(3) = 'gammavalue'
    reallist(4) = 'workingdistance'
    reallist(5) = 'Rin'
    reallist(6) = 'Rout'

    call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

end if
! write all the strings

dataset = SC_energyfile
call json_add(inp, dataset, ecpnl%energyfile); call JSON_failtest(error_cnt)

dataset = SC_masterfile
call json_add(inp, dataset, ecpnl%masterfile); call JSON_failtest(error_cnt)

dataset = SC_datafile
call json_add(inp, dataset, ecpnl%datafile); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, ecpnl%xtalname); call JSON_failtest(error_cnt)

if (twolayerflag) then
dataset = SC_xtalname2
    call json_add(inp, dataset, ecpnl%xtalname2); call JSON_failtest(error_cnt)

dataset = SC_filmfile
    call json_add(inp, dataset, ecpnl%filmfile); call JSON_failtest(error_cnt)

dataset = SC_subsfile
    call json_add(inp, dataset, ecpnl%subsfile); call JSON_failtest(error_cnt)
end if

dataset = SC_maskpattern
call json_add(inp, dataset, ecpnl%maskpattern); call JSON_failtest(error_cnt)

dataset = SC_anglefile
call json_add(inp, dataset, ecpnl%anglefile); call JSON_failtest(error_cnt)

dataset = SC_outputformat
call json_add(inp, dataset, ecpnl%outputformat); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteECPNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteLACBEDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param lacbednl LACBED name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteLACBEDNameList(lacbednl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteLACBEDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(LACBEDNameListType),INTENT(INOUT)                :: lacbednl
!f2py intent(in,out) ::  lacbednl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 4, n_real = 6
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'inputlist'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ lacbednl%maxHOLZ, lacbednl%numthick, lacbednl%npix, lacbednl%nthreads /)
intlist(1) = 'maxHOLZ'
intlist(2) = 'numthick'
intlist(3) = 'npix'
intlist(4) = 'nthreads'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! vectors
dataset = SC_k
call json_add(inp, dataset, lacbednl%k); call JSON_failtest(error_cnt)

dataset = SC_fn
call json_add(inp, dataset, lacbednl%fn); call JSON_failtest(error_cnt)

! write all the single reals
io_real = (/ lacbednl%voltage, lacbednl%dmin, lacbednl%convergence, lacbednl%startthick, lacbednl%thickinc, lacbednl%minten/)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'convergence'
reallist(4) = 'startthick'
reallist(5) = 'thickinc'
reallist(6) = 'minten'
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

! write all the strings
dataset = SC_outname
call json_add(inp, dataset, lacbednl%outname); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, lacbednl%xtalname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteLACBEDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteECPpatternNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist into JSON file
!
!> @param ecpnl ECP name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteECPpatternNameList(ecpnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteECPpatternNameList

use ISO_C_BINDING

IMPLICIT NONE

type(ECPpatternNameListType),INTENT(INOUT)            :: ecpnl
!f2py intent(in,out) ::  ecpnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 2, n_real = 6
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'ECPvars'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ ecpnl%stdout, ecpnl%npix /)
intlist(1) = 'stdout'
intlist(2) = 'npix'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! single real
dataset = SC_thetac
call json_add(inp, dataset, dble(ecpnl%thetac)); call JSON_failtest(error_cnt)

! real vector
dataset = SC_k
call json_add(inp, dataset, dble(ecpnl%k)); call JSON_failtest(error_cnt)

! write all the strings
dataset = SC_outname
call json_add(inp, dataset, ecpnl%outname); call JSON_failtest(error_cnt)

dataset = SC_masterfile
call json_add(inp, dataset, ecpnl%masterfile); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteECPpatternNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwritePEDkinNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param pednl PED name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwritePEDkinNameList(pednl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwritePEDkinNameList

use ISO_C_BINDING

IMPLICIT NONE

type(PEDkinNameListType),INTENT(INOUT)                :: pednl
!f2py intent(in,out) ::  pednl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 3, n_real = 4
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'PEDkinNameList'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/pednl%npix, pednl%ncubochoric, pednl%nthreads /)
intlist(1) = 'npix'
intlist(2) = 'ncubochoric'
intlist(3) = 'nthreads'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! write all the single reals
io_real = (/ pednl%voltage, pednl%thickness, pednl%dmin, pednl%rnmpp /)
reallist(1) = 'voltage'
reallist(2) = 'thickness'
reallist(3) = 'dmin'
reallist(4) = 'rnmpp'
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)

! write all the strings
dataset = SC_outname
call json_add(inp, dataset, pednl%outname); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, pednl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_eulerfile
call json_add(inp, dataset, pednl%eulerfile); call JSON_failtest(error_cnt)

dataset = SC_sampling
call json_add(inp, dataset, pednl%sampling); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwritePEDkinNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwritePEDZANameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param pednl PED name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwritePEDZANameList(pednl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwritePEDZANameList

use ISO_C_BINDING

IMPLICIT NONE

type(PEDZANameListType),INTENT(INOUT)                 :: pednl
!f2py intent(in,out) ::  pednl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'EMPEDZA'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ pednl%stdout, pednl%precsample, pednl%precazimuthal, pednl%npix, pednl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'precsample'
intlist(3) = 'precazimuthal'
intlist(4) = 'npix'
intlist(5) = 'nthreads'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! vectors
dataset = SC_k
call json_add(inp, dataset, pednl%k); call JSON_failtest(error_cnt)

dataset = SC_fn
call json_add(inp, dataset, pednl%fn); call JSON_failtest(error_cnt)

! single reals
io_real = (/ pednl%voltage, pednl%dmin, pednl%precangle, pednl%prechalfwidth, pednl%thickness, pednl%camlen /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'precangle'
reallist(4) = 'prechalfwidth'
reallist(5) = 'thickness'
reallist(6) = 'camlen'
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)


! write all the strings
dataset = SC_outname
call json_add(inp, dataset, pednl%outname); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, pednl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_filemode
call json_add(inp, dataset, pednl%filemode); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwritePEDZANameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteECCINameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param eccinl ECCI name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteECCINameList(eccinl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteECCINameList

use ISO_C_BINDING

IMPLICIT NONE

type(ECCINameListType),INTENT(INOUT)                  :: eccinl
!f2py intent(in,out) ::  eccinl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'ECCIlist'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ eccinl%stdout, eccinl%nthreads, eccinl%nktstep, eccinl%DF_npix, eccinl%DF_npiy /)
intlist(1) = 'stdout'
intlist(2) = 'nthreads'
intlist(3) = 'nktstep'
intlist(4) = 'DF_npix'
intlist(5) = 'DF_npiy'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! vectors
dataset = SC_k
call json_add(inp, dataset, eccinl%k); call JSON_failtest(error_cnt)

! single reals
io_real = (/ eccinl%voltage, eccinl%dkt, eccinl%ktmax, eccinl%dmin, eccinl%DF_L, eccinl%DF_slice /)
reallist(1) = 'voltage'
reallist(2) = 'dkt'
reallist(3) = 'ktmax'
reallist(4) = 'dmin'
reallist(5) = 'DF_L'
reallist(6) = 'DF_slice'
call JSON_writeNMLreals(inp, io_real, reallist, n_real, error_cnt)


! 2-vectors
dataset = SC_lauec
call json_add(inp, dataset, dble(eccinl%lauec)); call JSON_failtest(error_cnt)

dataset = SC_lauec2
call json_add(inp, dataset, dble(eccinl%lauec2)); call JSON_failtest(error_cnt)

! write all the strings
dataset = SC_dispmode
call json_add(inp, dataset, eccinl%dispmode); call JSON_failtest(error_cnt)

dataset = SC_summode
call json_add(inp, dataset, eccinl%summode); call JSON_failtest(error_cnt)

dataset = SC_progmode
call json_add(inp, dataset, eccinl%progmode); call JSON_failtest(error_cnt)

dataset = SC_xtalname
call json_add(inp, dataset, eccinl%xtalname); call JSON_failtest(error_cnt)

dataset = SC_defectfilename
call json_add(inp, dataset, eccinl%defectfilename); call JSON_failtest(error_cnt)

dataset = SC_dispfile
call json_add(inp, dataset, eccinl%dispfile); call JSON_failtest(error_cnt)

dataset = SC_dataname
call json_add(inp, dataset, eccinl%dataname); call JSON_failtest(error_cnt)

dataset = SC_ECPname
call json_add(inp, dataset, eccinl%ECPname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteECCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteRFZNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to an JSON file
!
!> @param rfznl RFZ name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!> @date 08/18/15 MDG 1.1 added other rotation representations
!--------------------------------------------------------------------------
recursive subroutine JSONwriteRFZNameList(rfznl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteRFZNameList

use ISO_C_BINDING

IMPLICIT NONE

type(RFZNameListType),INTENT(INOUT)                   :: rfznl
!f2py intent(in,out) ::  rfznl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'RFZlist'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ rfznl%pgnum, rfznl%nsteps, rfznl%gridtype /)
intlist(1) = 'pgnum'
intlist(2) = 'nsteps'
intlist(3) = 'gridtype'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

! strings
dataset = SC_euoutname
call json_add(inp, dataset, rfznl%euoutname); call JSON_failtest(error_cnt)

dataset = SC_cuoutname
call json_add(inp, dataset, rfznl%cuoutname); call JSON_failtest(error_cnt)

dataset = SC_hooutname
call json_add(inp, dataset, rfznl%hooutname); call JSON_failtest(error_cnt)

dataset = SC_quoutname
call json_add(inp, dataset, rfznl%quoutname); call JSON_failtest(error_cnt)

dataset = SC_rooutname
call json_add(inp, dataset, rfznl%rooutname); call JSON_failtest(error_cnt)

dataset = SC_omoutname
call json_add(inp, dataset, rfznl%omoutname); call JSON_failtest(error_cnt)

dataset = SC_axoutname
call json_add(inp, dataset, rfznl%axoutname); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteRFZNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONwriteDictIndxOpenCLNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to JSON file
!
!> @param rfznl RFZ name list structure
!> @param jsonname output file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONwriteDictIndxOpenCLNameList(dictindxnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONwriteDictIndxOpenCLNameList

use ISO_C_BINDING

use local

IMPLICIT NONE

type(DictIndxOpenCLListType),INTENT(INOUT)            :: dictindxnl
!f2py intent(in,out) ::  dictindxnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_value),pointer                              :: p, inp

integer(kind=irg),parameter                           :: n_int = 7, n_real = 1
integer(kind=irg)                                     :: io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, namelistname
character(fnlen,kind=c_char)                          :: line2(1)

! initialize the json state variables
namelistname = 'DictIndxOpenCLvars'
call JSON_initpointers(p, inp, jsonname, namelistname, error_cnt)

! write all the single integers
io_int = (/ dictindxnl%numexptsingle, dictindxnl%numdictsingle, dictindxnl%totnumdict, dictindxnl%totnumexpt, dictindxnl%imght, &
            dictindxnl%imgwd, dictindxnl%nnk /)
intlist(1) = 'numexptsingle'
intlist(2) = 'numdictsingle'
intlist(3) = 'totnumdict'
intlist(4) = 'totnumexpt'
intlist(5) = 'imght'
intlist(6) = 'imgwd'
intlist(7) = 'nnk'
call JSON_writeNMLintegers(inp, io_int, intlist, n_int, error_cnt)

dataset = SC_MeanSubtraction
call json_add(inp, dataset, dictindxnl%MeanSubtraction); call JSON_failtest(error_cnt)

! strings
dataset = SC_exptfile
call json_add(inp, dataset, dictindxnl%exptfile); call JSON_failtest(error_cnt)

dataset = SC_dictfile
call json_add(inp, dataset, dictindxnl%dictfile); call JSON_failtest(error_cnt)

dataset = SC_eulerfile
call json_add(inp, dataset, dictindxnl%eulerfile); call JSON_failtest(error_cnt)

! and then we write the file and clean up
call JSON_cleanuppointers(p, inp, jsonname, error_cnt)

end subroutine JSONwriteDictIndxOpenCLNameList


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! here we have the json->namelist conversion routines for all the 
! namelists defined in the NameListTypedefs.f90 file.
!
! To convert a json file to a namelist, we first initialize the namelist 
! to the default values, to make sure that any omissions in the json file
! are intercepted.  To do so, we call the NameListHandler routines with 
! the "initonly" optional keyword.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadInteger
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read integer from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param ival integer variable
!> @param dval integer variable (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadInteger(json, ep, ival, dval)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadInteger

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
integer(kind=irg),INTENT(INOUT)         :: ival
!f2py intent(in,out) ::  ival
integer(kind=irg),INTENT(IN)            :: dval

logical                                 :: found

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dval default value
call json%get(ep, ival, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  ival = dval
end if

end subroutine JSONreadInteger

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadIntegerVec
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read integer vector from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param ival integer vector variable
!> @param dval integer vector variable (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadIntegerVec(json, ep, ivec, dvec, n)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadIntegerVec

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
integer(kind=irg),INTENT(IN)            :: n
integer(kind=irg),INTENT(INOUT)         :: ivec(n)
!f2py intent(in,out) ::  ivec
integer(kind=irg),INTENT(IN)            :: dvec(n)

logical                                 :: found
integer(kind=irg),dimension(:),allocatable :: rv

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dval default value
call json%get(ep, rv, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  ivec = dvec
else
  ivec = rv
end if

end subroutine JSONreadIntegerVec

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadReal
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read single precision real from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param rval real variable
!> @param dval real variable (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadReal(json, ep, rval, dval)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadReal

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
real(kind=sgl),INTENT(INOUT)            :: rval
!f2py intent(in,out) ::  rval
real(kind=sgl),INTENT(IN)               :: dval

logical                                 :: found
real(kind=dbl)                          :: rv

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dval default value
call json%get(ep, rv, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  rval = dval
else
  rval = sngl(rv)
end if

end subroutine JSONreadReal

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadRealVec
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read single precision real vector from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param rval real vector variable
!> @param dval real vector variable (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadRealVec(json, ep, rvec, dvec, n)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadRealVec

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
integer(kind=irg),INTENT(IN)            :: n
real(kind=sgl),INTENT(INOUT)            :: rvec(n)
!f2py intent(in,out) ::  rvec
real(kind=sgl),INTENT(IN)               :: dvec(n)

logical                                 :: found
real(kind=dbl),dimension(:),allocatable :: rv

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dvec default value
call json%get(ep, rv, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  rvec = dvec
else
  rvec = sngl(rv)
end if

end subroutine JSONreadRealVec

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read double precision real from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param rval real variable
!> @param dval real variable (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadDouble(json, ep, rval, dval)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadDouble

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
real(kind=dbl),INTENT(INOUT)            :: rval
!f2py intent(in,out) ::  rval
real(kind=dbl),INTENT(IN)               :: dval

logical                                 :: found
real(kind=dbl)                          :: rv

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dval default value
call json%get(ep, rv, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  rval = dval
else
  rval = rv
end if

end subroutine JSONreadDouble

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadDoubleVec
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read double precision real vector from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param rval real vector variable
!> @param dval real vector variable (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadDoubleVec(json, ep, rvec, dvec, n)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadDoubleVec

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
integer(kind=irg),INTENT(IN)            :: n
real(kind=dbl),INTENT(INOUT)            :: rvec(n)
!f2py intent(in,out) ::  rvec
real(kind=dbl),INTENT(IN)               :: dvec(n)

logical                                 :: found
real(kind=dbl),dimension(:),allocatable :: rv

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dvec default value
call json%get(ep, rv, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  rvec = dvec
else
  rvec = rv
end if

end subroutine JSONreadDoubleVec

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadString
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read string from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param sval string
!> @param dval string (default value)
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadString(json, ep, sval, dval)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadString

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
character(fnlen),INTENT(INOUT)          :: sval
!f2py intent(in,out) ::  sval
character(fnlen),INTENT(IN)             :: dval

logical                                 :: found
character(kind=jsonCK,len=:),allocatable    :: cval

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dval default value
call json%get(ep, cval, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  sval = dval
else
  sval = trim(cval)
end if

end subroutine JSONreadString

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadLogical
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read logical from json file into namelist structure (with auto missing detection)
!
!> @param json structure
!> @param ep entry path string
!> @param sval logical 
!> @param dval logical (default value)
!
!> @date 08/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadLogical(json, ep, sval, dval)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadLogical

use ISO_C_BINDING
use io

IMPLICIT NONE

type(json_file),INTENT(INOUT)           :: json
!f2py intent(in,out) ::  json
character(fnlen),INTENT(IN)             :: ep
logical,INTENT(INOUT)                   :: sval
!f2py intent(in,out) ::  sval
logical,INTENT(IN)                      :: dval

logical                                 :: found, cval

! if we find the field 'ep' in the file, then we read its corresponding value
! if it is not there, then we return the dval default value
call json%get(ep, cval, found)
if (.not. found) then
  write(error_unit,'(A)') 'WARNING: field '//trim(ep)//' not found in json file; using default value from namelist template'
  sval = dval
else
  sval = cval
end if

end subroutine JSONreadLogical

!--------------------------------------------------------------------------
!
! FUNCTION:JSONgetDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief get vector from a json_value
!
!> @param child json_value structure
!> @param str text with variable name
!> @param v verbose if 1
!
!> @date 11/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive function JSONgetDouble(child,str,v) result(oval)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONgetDouble

use io
use, intrinsic :: iso_fortran_env, only: wp => real64

IMPLICIT NONE

type(json_value), pointer,INTENT(IN)            :: child
character(fnlen)                                :: str
integer(kind=irg),INTENT(IN)                    :: v
real(kind=dbl)                                  :: oval

real(kind=wp)                                   :: val
real(kind=sgl)                                  :: io_real(1)

call json_get(child, val)
if (v.eq.1) then
  io_real(1) = val
  call WriteValue(str,io_real,1)
end if
oval = val

end function JSONgetDouble

!--------------------------------------------------------------------------
!
! FUNCTION:JSONgetDoubleVector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief get vector from a json_value
!
!> @param child json_value structure
!> @param nc number of items to read
!> @param str text with variable name
!> @param v verbose if 1
!
!> @date 11/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive function JSONgetDoubleVector(child,nc,str,v) result(ovec)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONgetDoubleVector

use io
use, intrinsic :: iso_fortran_env, only: wp => real64

IMPLICIT NONE

type(json_value), pointer,INTENT(IN)            :: child
integer(kind=irg),INTENT(IN)                    :: nc
character(fnlen)                                :: str
integer(kind=irg),INTENT(IN)                    :: v
real(kind=dbl)                                  :: ovec(nc)

real(kind=wp),dimension(:),allocatable          :: vec
real(kind=sgl)                                  :: io_real(nc)

allocate(vec(nc))
call json_get(child, vec)
if (v.eq.1) then
  io_real(1:nc) = vec(1:nc)
  call WriteValue(str,io_real,nc)
end if
ovec(1:nc) = vec(1:nc)
deallocate(vec)

end function JSONgetDoubleVector


!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadFoilData
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief parse json foil file into defect namelist structures
!
!> @param cell unit cell pointer
!> @param defects defect structure, to be filled by this routine
!> @param error_cnt total number of errors encountered by json routines
!> @param verbose [optional] print a lot of output if present and true
!
!> @date 11/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadFoilData(cell, defects, error_cnt, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadFoilData

use ISO_C_BINDING
use NameListHandlers
use io
use error
use crystal
use, intrinsic :: iso_fortran_env, only: wp => real64

IMPLICIT NONE

type(unitcell)        ,INTENT(IN)                     :: cell
type(defecttype),INTENT(INOUT)                        :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt
logical,INTENT(IN),OPTIONAL                           :: verbose

type(json_value),pointer                              :: jval, child, child2, child3
type(json_value), pointer                             :: tmp_json_ptr
character(kind=jsonCK,len=:),allocatable                  :: name
integer(kind=irg)                                     :: v, i, j, jj, vart, nc, nc2, nc3, io_int(3)
real(kind=wp),dimension(:),allocatable                :: vec3
real(kind=wp)                                         :: val
real(kind=sgl)                                        :: io_real(6), x
logical                                               :: found
character(4),parameter                                :: row(6) = (/ 'row1', 'row2', 'row3', 'row4', 'row5', 'row6' /)
character(fnlen)                                      :: str, filename

v = 0
if (PRESENT(verbose)) then
  if (verbose) then
    v = 1
  end if
end if

! set the default values for all entries
 defects%foil%elmo = 0.0                         ! elastic moduli
 defects%foil%F = (/ 0.0,0.0,1.0 /)              ! foil normal in direct space Bravais reference frame 
 defects%foil%q = (/ 1.0,0.0,0.0 /)              ! reciprocal space vector along primary tilt axis towards airlock
 defects%foil%alP = 0.0                          ! primary tilt angle in degrees
 defects%foil%alS = 0.0                          ! secondary tilt angle (for double tilt holder)
 defects%foil%alR = 0.0                          ! secondary tilt angle (for rotation tilt holder)
 defects%foil%beP = 0.0                          ! angle of primary tilt axis w.r.t. image bottom edge
 defects%foil%z0 = 100.0                         ! foil thickness in nm

! the following are not used currently, but need to be initialized properly
 defects%foil%brx = 0.0                          ! parameters to describe the foil shape as a quadratic surface 
 defects%foil%bry = 0.0
 defects%foil%brxy = 0.0
 defects%foil%cpx = 0.0                          ! center of the foil quadratic surface within [-1,1] range in pixel coordinates
 defects%foil%cpy = 0.0

filename = trim(EMsoft_getEMdatapathname())//trim(defects%foilname)
filename = EMsoft_toNativePath(filename)

! json has alrady been initialized, so we should be ok directly reading the data from the file
call json_parse(trim(filename), jval)

if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
  call json_info(jval,vart,nc,name)             ! jval name = filename

! loop over the children (only 1)
  do i=1,nc
    call json_get_child(jval,i,child)
    call json_info(child,vart,nc2,name)         ! child name = FoilDescriptor

    nc2loop: do j=1,nc2
      call json_get_child(child,j,child2)
      call json_info(child2,vart,nc3,name)
! foil normal
        if (name.eq.'foilF') then 
          str = '   Foil normal F = ' 
          defects%foil%F = JSONgetDoubleVector(child2,nc3,str,v)
        end if
! foil q vector
        if (name.eq.'foilq') then
          str = '   Foil q-vector = ' 
          defects%foil%q = JSONgetDoubleVector(child2,nc3,str,v)
        end if
! foil alP tilt
        if (name.eq.'foilalP') then
          str = '   Foil alP tilt = ' 
          defects%foil%alP =JSONgetDouble(child2,str,v)
        end if
! foil alS tilt
        if (name.eq.'foilalS') then
          str = '   Foil alS tilt = ' 
          defects%foil%alS =JSONgetDouble(child2,str,v)
        end if
! foil alR tilt
        if (name.eq.'foilalR') then
          str = '   Foil alR tilt = ' 
          defects%foil%alR =JSONgetDouble(child2,str,v)
        end if
! foil thickness
        if (name.eq.'foilz0') then
          str = '   Foil thickness = ' 
          defects%foil%z0 =JSONgetDouble(child2,str,v)
        end if
! foil shape parameters
        if (name.eq.'brx') then
          str = '   shape brx = ' 
          defects%foil%brx =JSONgetDouble(child2,str,v)
        end if
! foil shape parameters
        if (name.eq.'bry') then
          str = '   shape bry = ' 
          defects%foil%bry =JSONgetDouble(child2,str,v)
        end if
! foil shape parameters
        if (name.eq.'brxy') then
          str = '   shape brxy = ' 
          defects%foil%brxy =JSONgetDouble(child2,str,v)
        end if
! foil shape parameters
        if (name.eq.'cpx') then
          str = '   shape cpx = ' 
          defects%foil%cpx =JSONgetDouble(child2,str,v)
        end if
! foil shape parameters
        if (name.eq.'cpy') then
          str = '   shape cpy = ' 
          defects%foil%cpy =JSONgetDouble(child2,str,v)
        end if
! foil elastic modulus tensor  (6x6 format)
        if (name.eq.'foilelmo') then 
          str = ''
          do jj=1,6
           call json_get_child(child2,jj,child3)
           call json_info(child3,vart,nc3,name)
           if (name.eq.row(jj)) defects%foil%elmo(jj,1:6) = JSONgetDoubleVector(child3,nc3,str,0)
          end do
          if (v.eq.1) then
            call WriteValue('   Elastic moduli tensor (Voigt notation)','')
            do jj=1,6
              io_real(1:6) = defects%foil%elmo(jj,1:6)
              call WriteValue('',io_real,6)
            end do
          end if
        end if
    end do nc2loop
  end do
end if

call JSON_failtest(error_cnt)

! verify that the foil normal (in real space) and q (in reciprocal space) are orthogonal
! in other words, we do a cartesian dot product...
x = CalcDot(cell,defects%foil%F,defects%foil%q,'c')
if (abs(x).gt.0.005) then
  call Message('Foil normal F must be orthogonal to q', frm = "(A)")
!  stop
end if

end subroutine JSONreadFoilData


!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadDefectFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief parse json file into defect namelist structures
!
!> @param cell unit cell pointer
!> @param jsonname input file name
!> @param defects defect structure, to be filled by this routine
!> @param error_cnt total number of errors encountered by json routines
!> @param verbose [optional] print a lot of output if present and true
!
!> @date 11/20/15 MDG 1.0 new routine
!> @date 12/08/15 MDG 1.1 added Einclusion defect type
!--------------------------------------------------------------------------
recursive subroutine JSONreadDefectFile(cell, jsonname, defects, error_cnt,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadDefectFile

use ISO_C_BINDING
use NameListHandlers
use io
use error
use, intrinsic :: iso_fortran_env, only: wp => real64

IMPLICIT NONE

type(unitcell)        ,INTENT(IN)                     :: cell
character(fnlen),INTENT(IN)                           :: jsonname
type(defecttype),INTENT(INOUT)                        :: defects
!f2py intent(in,out) ::  defects
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt
logical,INTENT(IN),OPTIONAL                           :: verbose

type(json_file)                                       :: json    !the JSON structure read from the file:
type(json_value),pointer                              :: jval, child, child2, child3, child4
character(kind=jsonCK,len=:),allocatable                  :: name
integer(kind=irg)                                     :: i, j, jj, kk, v, io_int(3), jskip, ndis
integer(kind=irg)                                     :: vart,nc, nc2, nc3, nc4, nc5
logical                                               :: found
character(fnlen)                                      :: foilfilename, str, filename, dummystr
real(wp)                                              :: v4(4), v5(5), v6(6), v9(9), io_real(6)

v = 0
if (PRESENT(verbose)) then
  if (verbose) then
    v = 1
  end if
end if
dummystr = ''
! first of all, initialize json and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

filename = trim(EMsoft_getEMdatapathname())//trim(jsonname)
filename = EMsoft_toNativePath(filename)

! populate the jval json_value structure
call json_parse(trim(filename), jval)
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! get the top level file descriptor (should be the file name) and the number of its children nc (should really be 1)
  call json_info(jval,vart,nc,name)             ! jval name = filename
  if (v.eq.1) then 
    call WriteValue(' Defect file name : ',name,"(' ',A)")
  end if

! loop over the children
  do i=1,nc
    call json_get_child(jval,i,child)
    call json_info(child,vart,nc2,name)         ! child name = DefectDescriptors

! loop over those children, which are the actual defect descriptors and deal with each of them separately

! the foil date must be the first entry; if it is not, then loop until we find it
    found = .FALSE.
    nc2loop: do j=1,nc2
      call json_get_child(child,j,child2)
      call json_info(child2,vart,nc3,name)
      if (.not.(name.eq.'foil')) CYCLE nc2loop
! name = foil, so read the name of the foil descriptor file
      call json_get(child2,'foilfilename',child3,found)
      jskip = j
      if (found) then
        call json_get(child3, name)
        if (v.eq.1) call WriteValue(' Foil file name = ',trim(name),"(' ',A)")
        defects%foilname = trim(name)
      end if
    end do nc2loop
   
    if (.not.found) then 
     call FatalError('JSONreadDefectFile','JSON file does not contain a foilfilename entry')
    end if

! here we call the foil reading routine to first fill all the foil parameters
    call JSONreadFoilData(cell, defects, error_cnt, verbose)

! then we need to get the total number of defects in the file, so that we can allocate
! the correct array sizes in the defects structure
    ndis = 0
    nc2loop2: do j=1,nc2
      if (j.eq.jskip) CYCLE nc2loop2
      call json_get_child(child,j,child2)
      call json_info(child2,vart,nc3,name)
      if (name.eq.'voids') then 
        allocate(defects%voids(nc3))
        defects%numvoids = nc3
      end if
      if (name.eq.'inclusions') then
        allocate(defects%inclusions(nc3))
        defects%numinc = nc3
      end if
      if (name.eq.'Einclusions') then
        allocate(defects%Einclusions(nc3))
        defects%numEinc = nc3
      end if
      if (name.eq.'Ydislocations') then 
        allocate(defects%YD(nc3))
        defects%numYdisl = nc3
      end if
      if (name.eq.'dislocations') then
        ndis = ndis + nc3
        defects%numdisl = nc3
      end if
      if (name.eq.'stackingfaults') then
        allocate(defects%SF(nc3))
        ndis = ndis + 2*nc3
        defects%numsf = nc3
      end if
    end do nc2loop2
    if (ndis.gt.0) allocate(defects%DL(ndis))

! now loop over all entries at the child level (Except for the foil data) and
! read the individual defect parameters; note that these are nested on level 4...
    ndis = 1
    nc2loop3: do j=1,nc2
      if (j.eq.jskip) CYCLE nc2loop3
      call json_get_child(child,j,child2)
      call json_info(child2,vart,nc3,name)
      
! dislocations
      if (name.eq.'dislocations') then
        do jj=1,nc3
         if (v.eq.1) then 
           io_int(1) = jj
           call WriteValue('   dislocation #  ',io_int,1,"(I4)")
         end if
         call json_get_child(child2,jj,child3)
         call json_info(child3,vart,nc4,name)
         do kk=1,nc4
          call json_get_child(child3,kk,child4)
          call json_info(child4,vart,nc5,name)
          if (name.eq.'id') then
            str = '        x-coordinate  = ' 
            defects%DL(ndis)%id = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'jd') then
            str = '        y-coordinate  = ' 
            defects%DL(ndis)%jd = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'zfrac') then
            str = '        zfrac         = ' 
            defects%DL(ndis)%zfrac = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'u') then
            str = '        u             = ' 
            defects%DL(ndis)%u = JSONgetDoubleVector(child4,nc5,str,v)
          end if
          if (name.eq.'bv') then
            str = '        bv            = ' 
            defects%DL(ndis)%burg = JSONgetDoubleVector(child4,nc5,str,v)
          end if
         end do
         ndis = ndis + 1
        end do
        CYCLE nc2loop3
      end if

! Ydislocations
      if (name.eq.'Ydislocations') then
        do jj=1,nc3
         if (v.eq.1) then 
           io_int(1) = jj
           call WriteValue('   Ydislocation #  ',io_int,1,"(I4)")
         end if
         call json_get_child(child2,jj,child3)
         call json_info(child3,vart,nc4,name)
         do kk=1,nc4
          call json_get_child(child3,kk,child4)
          call json_info(child4,vart,nc5,name)
          if (name.eq.'id') then
            str = '        x-coordinate  = ' 
            defects%YD(jj)%id = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'jd') then
            str = '        y-coordinate  = ' 
            defects%YD(jj)%jd = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'poisson') then
            str = '        poisson       = ' 
            defects%YD(jj)%sig = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'u') then
            str = '        u             = ' 
            defects%YD(jj)%u = JSONgetDoubleVector(child4,nc5,str,v)
          end if
          if (name.eq.'bv') then
            str = '        bv            = ' 
            defects%YD(jj)%burg = JSONgetDoubleVector(child4,nc5,str,v)
          end if
         end do
        end do
        CYCLE nc2loop3
      end if

! stacking faults
      if (name.eq.'stackingfaults') then
        do jj=1,nc3
         if (v.eq.1) then 
           io_int(1) = jj
           call WriteValue('   Stacking Fault #  ',io_int,1,"(I4)")
         end if
         call json_get_child(child2,jj,child3)
         call json_info(child3,vart,nc4,name)
         do kk=1,nc4
          call json_get_child(child3,kk,child4)
          call json_info(child4,vart,nc5,name)
          if (name.eq.'SFi') then
            str = '        x-coordinate  = ' 
            defects%SF(jj)%id = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'SFj') then
            str = '        y-coordinate  = ' 
            defects%SF(jj)%jd = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'SFsep') then
            str = '        SF separation = ' 
            defects%SF(jj)%sep = JSONgetDouble(child4,str,v)
          end if
          if (name.eq.'SFplane') then
            str = '        SF plane      = ' 
            defects%SF(jj)%plane = JSONgetDoubleVector(child4,nc5,str,v)
          end if
          if (name.eq.'SFlpu') then
            str = '        SF lpu        = ' 
            defects%SF(jj)%lpu = JSONgetDoubleVector(child4,nc5,str,v)
          end if
          if (name.eq.'SFtpu') then
            str = '        SF tpu        = ' 
            defects%SF(jj)%tpu = JSONgetDoubleVector(child4,nc5,str,v)
          end if
          if (name.eq.'SFlpb') then
            str = '        SF lpb        = ' 
            defects%SF(jj)%lpb = JSONgetDoubleVector(child4,nc5,str,v)
          end if
          if (name.eq.'SFtpb') then
            str = '        SF tpb        = ' 
            defects%SF(jj)%tpb = JSONgetDoubleVector(child4,nc5,str,v)
          end if
         end do
        end do
        CYCLE nc2loop3
      end if

! voids
      if (name.eq.'voids') then
        do jj=1,nc3
         call json_get_child(child2,jj,child3)
         call json_info(child3,vart,nc4,name)
         v4 = JSONgetDoubleVector(child3,nc4,dummystr,0)
         defects%voids(jj)%xpos = v4(1)
         defects%voids(jj)%ypos = v4(2)
         defects%voids(jj)%zpos = v4(3)
         defects%voids(jj)%radius = v4(4)
         if (v.eq.1) then 
           io_real(1:4) = v4(1:4)
           io_int(1) = jj
           call WriteValue(' void   ',io_int,1,"(I4)",advance="no")
           call WriteValue('',io_real,4)
         end if
        end do
        CYCLE nc2loop3
      end if

! inclusions 
      if (name.eq.'inclusions') then
        do jj=1,nc3
         call json_get_child(child2,jj,child3)
         call json_info(child3,vart,nc4,name)
         v5 = JSONgetDoubleVector(child3,nc4,dummystr,0)
         defects%inclusions(jj)%xpos = v5(1)
         defects%inclusions(jj)%ypos = v5(2)
         defects%inclusions(jj)%zpos = v5(3)
         defects%inclusions(jj)%radius = v5(4)
         defects%inclusions(jj)%C = v5(5)
         if (v.eq.1) then 
           io_real(1:5) = v5(1:5)
           io_int(1) = jj
           call WriteValue(' inclusion    ',io_int,1,"(I4)",advance="no")
           call WriteValue('',io_real,5)
         end if
        end do
        CYCLE nc2loop3
      end if

! Eshelby ellipsoidal inclusions (isotropic)
       if (name.eq.'Einclusions') then
         do jj=1,nc3
          if (v.eq.1) then 
            io_int(1) = jj
            call WriteValue('   Einclusion   #  ',io_int,1,"(I4)")
          end if
          call json_get_child(child2,jj,child3)
          call json_info(child3,vart,nc4,name)
          do kk=1,nc4
           call json_get_child(child3,kk,child4)
           call json_info(child4,vart,nc5,name)
           if (name.eq.'xyz') then
             str = '        xyz           = ' 
             defects%Einclusions(jj)%xyz = JSONgetDoubleVector(child4,nc5,str,v)
           end if
           if (name.eq.'a123') then
             str = '        a123          = ' 
             defects%Einclusions(jj)%a123 = JSONgetDoubleVector(child4,nc5,str,v)
           end if
           if (name.eq.'nu') then
             str = '        nu            = ' 
             defects%Einclusions(jj)%nu = JSONgetDouble(child4,str,v)
           end if
           if (name.eq.'epsstarvoigt') then
             str = '        eps* (Voigt)  = ' 
             v6 = JSONgetDoubleVector(child4,nc5,str,v)
             defects%Einclusions(jj)%epsstar(1,1) = v6(1)
             defects%Einclusions(jj)%epsstar(2,2) = v6(2)
             defects%Einclusions(jj)%epsstar(3,3) = v6(3)
             defects%Einclusions(jj)%epsstar(1,2) = v6(6)*0.5D0
             defects%Einclusions(jj)%epsstar(2,1) = v6(6)*0.5D0
             defects%Einclusions(jj)%epsstar(1,3) = v6(5)*0.5D0
             defects%Einclusions(jj)%epsstar(3,1) = v6(5)*0.5D0
             defects%Einclusions(jj)%epsstar(2,3) = v6(4)*0.5D0
             defects%Einclusions(jj)%epsstar(3,2) = v6(4)*0.5D0
           end if
           if (name.eq.'principalaxes') then
             str = '        principalaxes = ' 
             v9 = JSONgetDoubleVector(child4,nc5,str,v)
             defects%Einclusions(jj)%principalaxes(1,1:3) = v9(1:3)
             defects%Einclusions(jj)%principalaxes(2,1:3) = v9(4:6)
             defects%Einclusions(jj)%principalaxes(3,1:3) = v9(7:9)
           end if
          end do
         end do
         CYCLE nc2loop3
       end if

! other defct types to be added here 

    end do nc2loop3
  end do
end if

call JSON_failtest(error_cnt)

end subroutine JSONreadDefectFile


!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadKosselNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file into namelist structure
!
!> @param knl Kossel name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/12/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadKosselNameList(knl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadKosselNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(KosselNameListType),INTENT(INOUT)                :: knl
!f2py intent(in,out) ::  knl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(KosselNameListType)                              :: defknl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetKosselNameList(nmlfile, defknl, initonly=init)

! then we start reading the values in the json file  
  ep = 'Kossellist.stdout'
  call JSONreadInteger(json, ep, knl%stdout, defknl%stdout)
  ep = 'Kossellist.numthick'
  call JSONreadInteger(json, ep, knl%numthick, defknl%numthick)
  ep = 'Kossellist.npix'
  call JSONreadInteger(json, ep, knl%npix, defknl%npix)
  ep = 'Kossellist.maxHOLZ'
  call JSONreadInteger(json, ep, knl%maxHOLZ, defknl%maxHOLZ)
  ep = 'Kossellist.nthreads'
  call JSONreadInteger(json, ep, knl%nthreads, defknl%nthreads)

  ep = 'Kossellist.k'
  call JSONreadIntegerVec(json, ep, knl%k, defknl%k, size(knl%k))
  ep = 'Kossellist.fn'
  call JSONreadIntegerVec(json, ep, knl%fn, defknl%fn, size(knl%fn))

  ep = 'Kossellist.voltage'
  call JSONreadReal(json, ep, knl%voltage, defknl%voltage)
  ep = 'Kossellist.dmin'
  call JSONreadReal(json, ep, knl%dmin, defknl%dmin)
  ep = 'Kossellist.convergence'
  call JSONreadReal(json, ep, knl%convergence, defknl%convergence)
  ep = 'Kossellist.startthick'
  call JSONreadReal(json, ep, knl%startthick, defknl%startthick)
  ep = 'Kossellist.thickinc'
  call JSONreadReal(json, ep, knl%thickinc, defknl%thickinc)
  ep = 'Kossellist.minten'
  call JSONreadReal(json, ep, knl%minten, defknl%minten)

  ep = 'Kossellist.xtalname'
  call JSONreadString(json, ep, knl%xtalname, defknl%xtalname)
  ep = 'Kossellist.outname'
  call JSONreadString(json, ep, knl%outname, defknl%outname)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadKosselNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadKosselMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill knl structure (used by EMKosselmaster.f90)
!
!> @param knl Kossel name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/19/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadKosselMasterNameList(knl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadKosselMasterNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(KosselMasterNameListType),INTENT(INOUT)          :: knl
!f2py intent(in,out) ::  knl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(KosselMasterNameListType)                        :: defknl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetKosselMasterNameList(nmlfile, defknl, initonly=init)

! then we start reading the values in the json file  
  ep = 'Kosselmasterlist.stdout'
  call JSONreadInteger(json, ep, knl%stdout, defknl%stdout)

  ep = 'Kosselmasterlist.numthick'
  call JSONreadInteger(json, ep, knl%numthick, defknl%numthick)
  ep = 'Kosselmasterlist.npix'
  call JSONreadInteger(json, ep, knl%npx, defknl%npx)
  ep = 'Kosselmasterlist.nthreads'
  call JSONreadInteger(json, ep, knl%nthreads, defknl%nthreads)

  ep = 'Kosselmasterlist.voltage'
  call JSONreadReal(json, ep, knl%voltage, defknl%voltage)
  ep = 'Kosselmasterlist.dmin'
  call JSONreadReal(json, ep, knl%dmin, defknl%dmin)
  ep = 'Kosselmasterlist.startthick'
  call JSONreadReal(json, ep, knl%startthick, defknl%startthick)
  ep = 'Kosselmasterlist.thickinc'
  call JSONreadReal(json, ep, knl%thickinc, defknl%thickinc)
  ep = 'Kosselmasterlist.tfraction'
  call JSONreadReal(json, ep, knl%tfraction, defknl%tfraction)

  ep = 'Kosselmasterlist.Kosselmode'
  s = knl%Kosselmode
  s2 = defknl%Kosselmode
  call JSONreadString(json, ep, s, s2)
  ep = 'Kosselmasterlist.xtalname'
  call JSONreadString(json, ep, knl%xtalname, defknl%xtalname)
  ep = 'Kosselmasterlist.outname'
  call JSONreadString(json, ep, knl%outname, defknl%outname)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadKosselMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadreflectorNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill mcnl structure (used by EMMC.f90)
!
!> @param mcnl Monte Carloname list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/19/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadreflectorNameList(rnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadreflectorNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(reflectorNameListType),INTENT(INOUT)             :: rnl
!f2py intent(in,out) ::  rnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(reflectorNameListType)                           :: defrnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetreflectorNameList(nmlfile, defrnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'EBSDreflectors.increment'
  call JSONreadReal(json, ep, rnl%increment, defrnl%increment)
  ep = 'EBSDreflectors.dmin'
  call JSONreadReal(json, ep, rnl%dmin, defrnl%dmin)

  ep = 'EBSDreflectors.masterfile'
  call JSONreadString(json, ep, rnl%masterfile, defrnl%masterfile)
  ep = 'EBSDreflectors.energyfile'
  call JSONreadString(json, ep, rnl%listfile, defrnl%listfile)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadreflectorNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadMCNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill mcnl structure (used by EMMC.f90)
!
!> @param mcnl Monte Carloname list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/19/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadMCNameList(mcnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadMCNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(MCNameListType),INTENT(INOUT)                    :: mcnl
!f2py intent(in,out) ::  mcnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(MCNameListType)                                  :: defmcnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetMCNameList(nmlfile, defmcnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'MCdata.stdout'
  call JSONreadInteger(json, ep, mcnl%stdout, defmcnl%stdout)
  ep = 'MCdata.numsx'
  call JSONreadInteger(json, ep, mcnl%numsx, defmcnl%numsx)
  ep = 'MCdata.num_el'
  call JSONreadInteger(json, ep, mcnl%num_el, defmcnl%num_el)
  ep = 'MCdata.primeseeds'
  call JSONreadInteger(json, ep, mcnl%primeseed, defmcnl%primeseed)
  ep = 'MCdata.nthreads'
  call JSONreadInteger(json, ep, mcnl%nthreads, defmcnl%nthreads)

  ep = 'MCdata.sig'
  call JSONreadDouble(json, ep, mcnl%sig, defmcnl%sig)
  ep = 'MCdata.omega'
  call JSONreadDouble(json, ep, mcnl%omega, defmcnl%omega)
  ep = 'MCdata.EkeV'
  call JSONreadDouble(json, ep, mcnl%EkeV, defmcnl%EkeV)
  ep = 'MCdata.Ehistmin'
  call JSONreadDouble(json, ep, mcnl%Ehistmin, defmcnl%Ehistmin)
  ep = 'MCdata.Ebinsize'
  call JSONreadDouble(json, ep, mcnl%Ebinsize, defmcnl%Ebinsize)
  ep = 'MCdata.depthmax'
  call JSONreadDouble(json, ep, mcnl%depthmax, defmcnl%depthmax)
  ep = 'MCdata.depthstep'
  call JSONreadDouble(json, ep, mcnl%depthstep, defmcnl%depthstep)

  ep = 'MCdata.MCmode'
  s = mcnl%MCmode
  s2 = defmcnl%MCmode
  call JSONreadString(json, ep, s, s2)
  ep = 'MCdata.xtalname'
  call JSONreadString(json, ep, mcnl%xtalname, defmcnl%xtalname)
  ep = 'MCdata.dataname'
  call JSONreadString(json, ep, mcnl%dataname, defmcnl%dataname)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadMCNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadMCCLNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMCCL.f90)
!
!> @param mcnl Monte Carlo name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/19/15  MDG 1.0 new routine
!> @date 09/09/15  MDG 1.1 added devid
!--------------------------------------------------------------------------
recursive subroutine JSONreadMCCLNameList(mcnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadMCCLNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(MCCLNameListType),INTENT(INOUT)                  :: mcnl
!f2py intent(in,out) ::  mcnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(MCCLNameListType)                                :: defmcnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetMCCLNameList(nmlfile, defmcnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'MCCLdata.stdout'
  call JSONreadInteger(json, ep, mcnl%stdout, defmcnl%stdout)
  ep = 'MCCLdata.numsx'
  call JSONreadInteger(json, ep, mcnl%numsx, defmcnl%numsx)
  ep = 'MCCLdata.globalworkgrpsz'
  call JSONreadInteger(json, ep, mcnl%globalworkgrpsz, defmcnl%globalworkgrpsz)
  ep = 'MCCLdata.num_el'
  call JSONreadInteger(json, ep, mcnl%num_el, defmcnl%num_el)
  ep = 'MCCLdata.totnum_el'
  call JSONreadInteger(json, ep, mcnl%totnum_el, defmcnl%totnum_el)
  ep = 'MCCLdata.multiplier'
  call JSONreadInteger(json, ep, mcnl%multiplier, defmcnl%multiplier)
  ep = 'MCCLdata.devid'
  call JSONreadInteger(json, ep, mcnl%devid, defmcnl%devid)
  ep = 'MCCLdata.platid'
  call JSONreadInteger(json, ep, mcnl%platid, defmcnl%platid)

  ep = 'MCCLdata.sigstart'
  call JSONreadDouble(json, ep, mcnl%sigstart, defmcnl%sigstart)
  ep = 'MCCLdata.sigend'
  call JSONreadDouble(json, ep, mcnl%sigend, defmcnl%sigend)
  ep = 'MCCLdata.sigstep'
  call JSONreadDouble(json, ep, mcnl%sigstep, defmcnl%sigstep)
  ep = 'MCCLdata.omega'
  call JSONreadDouble(json, ep, mcnl%omega, defmcnl%omega)
  ep = 'MCCLdata.EkeV'
  call JSONreadDouble(json, ep, mcnl%EkeV, defmcnl%EkeV)
  ep = 'MCCLdata.Ehistmin'
  call JSONreadDouble(json, ep, mcnl%Ehistmin, defmcnl%Ehistmin)
  ep = 'MCCLdata.Ebinsize'
  call JSONreadDouble(json, ep, mcnl%Ebinsize, defmcnl%Ebinsize)
  ep = 'MCCLdata.depthmax'
  call JSONreadDouble(json, ep, mcnl%depthmax, defmcnl%depthmax)
  ep = 'MCCLdata.depthstep'
  call JSONreadDouble(json, ep, mcnl%depthstep, defmcnl%depthstep)

  ep = 'MCCLdata.MCmode'
  s = mcnl%MCmode
  s2 = defmcnl%MCmode
  call JSONreadString(json, ep, s, s2)
  ep = 'MCCLdata.xtalname'
  call JSONreadString(json, ep, mcnl%xtalname, defmcnl%xtalname)
  ep = 'MCCLdata.dataname'
  call JSONreadString(json, ep, mcnl%dataname, defmcnl%dataname)
  ep = 'MCCLdata.mode'
  call JSONreadString(json, ep, mcnl%mode, defmcnl%mode)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadMCCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadMCCLMultiLayerNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMCCL.f90)
!
!> @param mcnl Monte Carloname list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/19/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadMCCLMultiLayerNameList(mcnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadMCCLMultiLayerNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(MCCLMultiLayerNameListType),INTENT(INOUT)        :: mcnl
!f2py intent(in,out) ::  mcnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(MCCLMultiLayerNameListType)                      :: defmcnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetMCCLMultiLayerNameList(nmlfile, defmcnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'MCCLdata.stdout'
  call JSONreadInteger(json, ep, mcnl%stdout, defmcnl%stdout)
  ep = 'MCCLdata.numsx'
  call JSONreadInteger(json, ep, mcnl%numsx, defmcnl%numsx)
  ep = 'MCCLdata.globalworkgrpsz'
  call JSONreadInteger(json, ep, mcnl%globalworkgrpsz, defmcnl%globalworkgrpsz)
  ep = 'MCCLdata.num_el'
  call JSONreadInteger(json, ep, mcnl%num_el, defmcnl%num_el)
  ep = 'MCCLdata.totnum_el'
  call JSONreadInteger(json, ep, mcnl%totnum_el, defmcnl%totnum_el)

  ep = 'MCCLdata.sig'
  call JSONreadDouble(json, ep, mcnl%sig, defmcnl%sig)
  ep = 'MCCLdata.omega'
  call JSONreadDouble(json, ep, mcnl%omega, defmcnl%omega)
  ep = 'MCCLdata.EkeV'
  call JSONreadDouble(json, ep, mcnl%EkeV, defmcnl%EkeV)
  ep = 'MCCLdata.Ehistmin'
  call JSONreadDouble(json, ep, mcnl%Ehistmin, defmcnl%Ehistmin)
  ep = 'MCCLdata.Ebinsize'
  call JSONreadDouble(json, ep, mcnl%Ebinsize, defmcnl%Ebinsize)
  ep = 'MCCLdata.depthmax'
  call JSONreadDouble(json, ep, mcnl%depthmax, defmcnl%depthmax)
  ep = 'MCCLdata.depthstep'
  call JSONreadDouble(json, ep, mcnl%depthstep, defmcnl%depthstep)
  ep = 'MCCLdata.filmthickness'
  call JSONreadDouble(json, ep, mcnl%filmthickness, defmcnl%filmthickness)
  ep = 'MCCLdata.filmstep'
  call JSONreadDouble(json, ep, mcnl%filmstep, defmcnl%filmstep)

  ep = 'MCCLdata.MCmode'
  s = mcnl%MCmode
  s2 = defmcnl%MCmode
  call JSONreadString(json, ep, s, s2)
  ep = 'MCCLdata.xtalname_film'
  call JSONreadString(json, ep, mcnl%xtalname_film, defmcnl%xtalname_film)
  ep = 'MCCLdata.xtalname_subs'
  call JSONreadString(json, ep, mcnl%xtalname_subs, defmcnl%xtalname_subs)
  ep = 'MCCLdata.dataname'
  call JSONreadString(json, ep, mcnl%dataname, defmcnl%dataname)
  ep = 'MCCLdata.mode'
  call JSONreadString(json, ep, mcnl%mode, defmcnl%mode)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadMCCLMultiLayerNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadEBSDMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill emnl structure (used by EMEBSDmaster.f90)
!
!> @param emnl EBSD master name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/19/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadEBSDMasterNameList(emnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadEBSDMasterNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(EBSDMasterNameListType),INTENT(INOUT)            :: emnl
!f2py intent(in,out) ::  emnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(EBSDMasterNameListType)                          :: defemnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetEBSDMasterNameList(nmlfile, defemnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'EBSDmastervars.stdout'
  call JSONreadInteger(json, ep, emnl%stdout, defemnl%stdout)
  ep = 'EBSDmastervars.npx'
  call JSONreadInteger(json, ep, emnl%npx, defemnl%npx)
  ep = 'EBSDmastervars.Esel'
  call JSONreadInteger(json, ep, emnl%Esel, defemnl%Esel)
  ep = 'EBSDmastervars.nthreads'
  call JSONreadInteger(json, ep, emnl%nthreads, defemnl%nthreads)

  ep = 'EBSDmastervars.dmin'
  call JSONreadReal(json, ep, emnl%dmin, defemnl%dmin)

  ep = 'EBSDmastervars.energyfile'
  call JSONreadString(json, ep, emnl%energyfile, defemnl%energyfile)
  ep = 'EBSDmastervars.outname'
  call JSONreadString(json, ep, emnl%outname, defemnl%outname)

  ep = 'EBSDmastervars.restart'
  call JSONreadLogical(json, ep, emnl%restart, defemnl%restart)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadEBSDMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadEBSDclusterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill emnl structure (used by EMEBSDcluster.f90)
!
!> @param emnl EBSD cluster name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 12/28/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadEBSDclusterNameList(emnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadEBSDclusterNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(EBSDclusterNameListType),INTENT(INOUT)           :: emnl
!f2py intent(in,out) ::  emnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(EBSDclusterNameListType)                         :: defemnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetEBSDclusterNameList(nmlfile, defemnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'EBSDclustervars.NClusters'
  call JSONreadInteger(json, ep, emnl%NClusters, defemnl%NClusters)

  ep = 'EBSDclustervars.NIterations'
  call JSONreadInteger(json, ep, emnl%NIterations, defemnl%NIterations)

  ep = 'EBSDclustervars.inputfilename'
  call JSONreadString(json, ep, emnl%inputfilename, defemnl%inputfilename)

  ep = 'EBSDclustervars.groupname'
  call JSONreadString(json, ep, emnl%groupname, defemnl%groupname)

  ep = 'EBSDclustervars.datasetname'
  call JSONreadString(json, ep, emnl%datasetname, defemnl%datasetname)

end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadEBSDclusterNameList



!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadECPMasterNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill mcnl structure (used by EMECPmaster.f90)
!
!> @param emnl ECP master name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/20/15  MDG 1.0 new routine
!> @date 09/15/15  SS  1.1 changes after clean up of ECPmasterListType
!--------------------------------------------------------------------------
recursive subroutine JSONreadECPMasterNameList(ecpnl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadECPMasterNameList

use ISO_C_BINDING
use NameListHandlers

IMPLICIT NONE

type(ECPMasterNameListType),INTENT(INOUT)             :: ecpnl
!f2py intent(in,out) ::  ecpnl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(ECPMasterNameListType)                           :: defecpnl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetECPMasterNameList(nmlfile, defecpnl, initonly=init)

! then we start reading the values in the json file  
  ep = 'ECPmastervars.stdout'
  call JSONreadInteger(json, ep, ecpnl%stdout, defecpnl%stdout)
  ep = 'ECPmastervars.npx'
  call JSONreadInteger(json, ep, ecpnl%npx, defecpnl%npx)
  ep = 'ECPmastervars.Esel'
  call JSONreadInteger(json, ep, ecpnl%Esel, defecpnl%Esel)
  ep = 'ECPmastervars.nthreads'
  call JSONreadInteger(json, ep, ecpnl%nthreads, defecpnl%nthreads)

  !ep = 'ECPmastervars.startthick'
  !call JSONreadReal(json, ep, ecpnl%startthick, defecpnl%startthick)
  ep = 'ECPmastervars.dmin'
  call JSONreadReal(json, ep, ecpnl%dmin, defecpnl%dmin)

  !ep = 'ECPmastervars.fn'
  !call JSONreadRealVec(json, ep, ecpnl%fn, defecpnl%fn, size(ecpnl%fn))
  !ep = 'ECPmastervars.abcdist'
  !call JSONreadRealVec(json, ep, ecpnl%abcdist, defecpnl%abcdist, size(ecpnl%abcdist))
  !ep = 'ECPmastervars.albegadist'
  !call JSONreadRealVec(json, ep, ecpnl%albegadist, defecpnl%albegadist, size(ecpnl%albegadist))

  ep = 'ECPmastervars.compmode'
  call JSONreadString(json, ep, ecpnl%compmode, defecpnl%compmode)
  ep = 'ECPmastervars.energyfile'
  call JSONreadString(json, ep, ecpnl%energyfile, defecpnl%energyfile)

  !ep = 'ECPmastervars.distort'
  !call JSONreadLogical(json, ep, ecpnl%distort, defecpnl%distort)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadECPMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadEBSDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read json file and fill enl structure (used by EMEBSD.f90)
!
!> @param enl EBSD name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadEBSDNameList(enl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadEBSDNameList

use ISO_C_BINDING
use NameListHandlers
use error

IMPLICIT NONE

type(EBSDNameListType),INTENT(INOUT)                  :: enl
!f2py intent(in,out) ::  enl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(EBSDNameListType)                                :: defenl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetEBSDNameList(nmlfile, defenl, initonly=init)

  ep = 'EBSDdata.stdout'
  call JSONreadInteger(json, ep, enl%stdout, defenl%stdout)
  ep = 'EBSDdata.numsx'
  call JSONreadInteger(json, ep, enl%numsx, defenl%numsx)
  ep = 'EBSDdata.numsy'
  call JSONreadInteger(json, ep, enl%numsy, defenl%numsy)
  ep = 'EBSDdata.binning'
  call JSONreadInteger(json, ep, enl%binning, defenl%binning)
  ep = 'EBSDdata.nthreads'
  call JSONreadInteger(json, ep, enl%nthreads, defenl%nthreads)
  ep = 'EBSDdata.energyaverage'
  call JSONreadInteger(json, ep, enl%energyaverage, defenl%energyaverage)

  ep = 'EBSDdata.L'
  call JSONreadReal(json, ep, enl%L, defenl%L)
  ep = 'EBSDdata.thetac'
  call JSONreadReal(json, ep, enl%thetac, defenl%thetac)
  ep = 'EBSDdata.delta'
  call JSONreadReal(json, ep, enl%delta, defenl%delta)
  ep = 'EBSDdata.xpc'
  call JSONreadReal(json, ep, enl%xpc, defenl%xpc)
  ep = 'EBSDdata.ypc'
  call JSONreadReal(json, ep, enl%ypc, defenl%ypc)
  ep = 'EBSDdata.energymin'
  call JSONreadReal(json, ep, enl%energymin, defenl%energymin)
  ep = 'EBSDdata.energymax'
  call JSONreadReal(json, ep, enl%energymax, defenl%energymax)
  ep = 'EBSDdata.gammavalue'
  call JSONreadReal(json, ep, enl%gammavalue, defenl%gammavalue)

  ep = 'EBSDdata.axisangle'
  call JSONreadRealVec(json, ep, enl%axisangle, defenl%axisangle, size(enl%axisangle))

  ep = 'EBSDdata.beamcurrent'
  call JSONreadDouble(json, ep, enl%beamcurrent, defenl%beamcurrent)
  ep = 'EBSDdata.dwelltime'
  call JSONreadDouble(json, ep, enl%dwelltime, defenl%dwelltime)

  ep = 'EBSDdata.maskpattern'
  s = enl%maskpattern
  s2 = defenl%maskpattern
  call JSONreadString(json, ep, s, s2)
  ep = 'EBSDdata.scalingmode'
  s = enl%scalingmode
  s2 = defenl%scalingmode
  call JSONreadString(json, ep, s, s2)
  ep = 'EBSDdata.eulerconvention'
  s = enl%eulerconvention
  s2 = defenl%eulerconvention
  call JSONreadString(json, ep, s, s2)
  ep = 'EBSDdata.outputformat'
  s = enl%outputformat
  s2 = defenl%outputformat
  call JSONreadString(json, ep, s, s2)

  ep = 'EBSDdata.anglefile'
  call JSONreadString(json, ep, enl%anglefile, defenl%anglefile)
  ep = 'EBSDdata.masterfile'
  call JSONreadString(json, ep, enl%masterfile, defenl%masterfile)
  ep = 'EBSDdata.energyfile'
  call JSONreadString(json, ep, enl%energyfile, defenl%energyfile)
  ep = 'EBSDdata.datafile'
  call JSONreadString(json, ep, enl%datafile, defenl%datafile)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadEBSDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:JSONreadEBSDoverlapNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read jsonfile and fill enl structure (used by EMEBSDoverlap.f90)
!
!> @param enl EBSD name list structure
!> @param jsonname input file name
!> @param error_cnt total number of errors encountered by json routines
!
!> @date 08/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine JSONreadEBSDoverlapNameList(enl, jsonname, error_cnt)
!DEC$ ATTRIBUTES DLLEXPORT :: JSONreadEBSDoverlapNameList

use ISO_C_BINDING
use NameListHandlers
use error

IMPLICIT NONE

type(EBSDoverlapNameListType),INTENT(INOUT)           :: enl
!f2py intent(in,out) ::  enl
character(fnlen),INTENT(IN)                           :: jsonname
integer(kind=irg),INTENT(INOUT)                       :: error_cnt
!f2py intent(in,out) ::  error_cnt

type(json_file)                                       :: json    !the JSON structure read from the file:

type(EBSDoverlapNameListType)                         :: defenl
logical                                               :: init = .TRUE.
character(fnlen)                                      :: nmlfile = '', ep, s, s2
real(kind=wp)                                         :: rval
character(kind=jsonCK,len=:),allocatable                  :: cval
real(wp),dimension(:),allocatable                     :: rvec

! first of all, open the file and return an error message if it does not exist
error_cnt = 0
call json_initialize(); call JSON_failtest(error_cnt)

! populate the json structure
call json%load_file(filename = trim(jsonname))
if (json_failed().eqv..TRUE.) then    !if there was an error reading the file
  call json_print_error_message(error_unit)
  error_cnt = error_cnt + 1
else
! ok, we got here so we need to initialize the namelist first to its default values (set in NameListHandlers)
  call GetEBSDoverlapNameList(nmlfile, defenl, initonly=init)

  ep = 'EBSDdata.stdout'
  call JSONreadInteger(json, ep, enl%stdout, defenl%stdout)

  ep = 'EBSDdata.PatternAxisA'
  call JSONreadIntegerVec(json, ep, enl%PatternAxisA, defenl%PatternAxisA, size(defenl%PatternAxisA))
  ep = 'EBSDdata.HorizontalAxisA'
  call JSONreadIntegerVec(json, ep, enl%HorizontalAxisA, defenl%HorizontalAxisA, size(defenl%HorizontalAxisA))

  ep = 'EBSDdata.tA'
  call JSONreadRealVec(json, ep, enl%tA, defenl%tA, size(enl%tA))
  ep = 'EBSDdata.tB'
  call JSONreadRealVec(json, ep, enl%tB, defenl%tB, size(enl%tB))
  ep = 'EBSDdata.gA'
  call JSONreadRealVec(json, ep, enl%gA, defenl%gA, size(enl%gA))
  ep = 'EBSDdata.gB'
  call JSONreadRealVec(json, ep, enl%gB, defenl%gB, size(enl%gB))

  ep = 'EBSDdata.fracA'
  call JSONreadReal(json, ep, enl%fracB, defenl%fracB)

  ep = 'EBSDdata.masterfileA'
  call JSONreadString(json, ep, enl%masterfileA, defenl%masterfileA)
  ep = 'EBSDdata.masterfileB'
  call JSONreadString(json, ep, enl%masterfileB, defenl%masterfileB)
  ep = 'EBSDdata.datafile'
  call JSONreadString(json, ep, enl%datafile, defenl%datafile)
end if

call json%destroy(); call JSON_failtest(error_cnt)

end subroutine JSONreadEBSDoverlapNameList

! line 870 of NameListHandlers

end module JSONsupport
