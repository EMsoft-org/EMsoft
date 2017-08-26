! ###################################################################
! Copyright (c) 2013-2015, Marc De Graef/Carnegie Mellon University
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
! USE OF THIS SOFTWAG, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

!--------------------------------------------------------------------------
! EMsoft:NameListHDFwriters.f90
!--------------------------------------------------------------------------
!
! PROGRAM: NameListHDFwriters
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines for reading and returning name list type structures
!
!> @date 03/20/15 MDG 1.0 original, completed on 3/23/15
!> @date 03/28/15 MDG 2.0 removing all h5lt calls; replaced with HDFsupport calls
!> @date 04/08/15 MDG 2.1 removed HDF_tail pointer as it was no longer needed
!> @date 05/05/15 MDG 2.2 removed primelist variable from name list files
!> @date 05/19/16 MDG 2.3 inserted groupname variable in all createGroup calls
!--------------------------------------------------------------------------
module NameListHDFwriters

use local
use typedefs
use NameListTypedefs
use HDF5
use HDFsupport

contains

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! first some auxiliary routines to make things easier later on
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_writeNMLintegers
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a series of integer namelist entries to an HDF file
!
!> @param HDF_head top of stack pointer
!> @param io_int list of integers
!> @param intlist list of string descriptors
!> @param n_int number of entries
!
!> @date 03/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeNMLintegers

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
integer(kind=irg),INTENT(IN)                          :: io_int(n_int)
character(20),INTENT(IN)                              :: intlist(n_int)
integer(kind=irg),INTENT(IN)                          :: n_int

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
logical                                               :: g_exists, overwrite=.TRUE.

do i=1,n_int
  dataset = intlist(i)
  call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, io_int(i), HDF_head, overwrite)
   else
    hdferr = HDF_writeDatasetInteger(dataset, io_int(i), HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeNMLintegers: unable to create '//trim(intlist(i))//' dataset',.TRUE.)
end do

end subroutine HDF_writeNMLintegers

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_writeNMLreals
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a series of real namelist entries to an HDF file
!
!> @param HDF_head top of stack pointer
!> @param io_real list of reals
!> @param reallist list of string descriptors
!> @param n_real number of entries
!
!> @date 03/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeNMLreals

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
real(kind=sgl),INTENT(IN)                             :: io_real(n_real)
character(20),INTENT(IN)                              :: reallist(n_real)
integer(kind=irg),INTENT(IN)                          :: n_real

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
logical                                               :: g_exists, overwrite=.TRUE.

do i=1,n_real
  dataset = reallist(i)
  call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloat(dataset, io_real(i), HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloat(dataset, io_real(i), HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeNMLreals: unable to create '//trim(reallist(i))//' dataset',.TRUE.)
end do

end subroutine HDF_writeNMLreals

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDF_writeNMLdbles
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write a series of double precision namelist entries to an HDF file
!
!> @param HDF_head top of stack pointer
!> @param io_real list of doubles
!> @param reallist list of string descriptors
!> @param n_real number of entries
!
!> @date 03/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)
!DEC$ ATTRIBUTES DLLEXPORT :: HDF_writeNMLdbles

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
real(kind=dbl),INTENT(IN)                             :: io_real(n_real)
character(20),INTENT(IN)                              :: reallist(n_real)
integer(kind=irg),INTENT(IN)                          :: n_real

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
logical                                               :: g_exists, overwrite=.TRUE.

do i=1,n_real
  dataset = reallist(i)
  call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetDouble(dataset, io_real(i), HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetDouble(dataset, io_real(i), HDF_head)
  end if
  if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeNMLdbles: unable to create '//trim(reallist(i))//' dataset',.TRUE.)
end do

end subroutine HDF_writeNMLdbles


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! then the actual namelist write routines
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteKosselNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param knl Kossel name list structure
!
!> @date 03/20/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteKosselNameList(HDF_head, knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteKosselNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(KosselNameListType),INTENT(IN)                   :: knl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'KosselNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ knl%stdout, knl%numthick, knl%npix, knl%maxHOLZ, knl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'numthick'
intlist(3) = 'npix'
intlist(4) = 'maxHOLZ'
intlist(5) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! integer vectors
dataset = 'k'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, knl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselNameList: unable to create k dataset',.TRUE.)

dataset = 'fn'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, knl%fn, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselNameList: unable to create fn dataset',.TRUE.)

! write all the single reals
io_real = (/ knl%voltage, knl%dmin, knl%convergence, knl%startthick, knl%thickinc, knl%minten /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'convergence'
reallist(4) = 'startthick'
reallist(5) = 'thickinc'
reallist(6) = 'minten'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'xtalname'
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'outname'
line2(1) = knl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselNameList: unable to create outname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteKosselNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteKosselMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param knl Kossel name list structure
!
!> @date 03/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteKosselMasterNameList(HDF_head, knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteKosselMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(KosselMasterNameListType),INTENT(IN)             :: knl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 5
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'KosselMasterNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ knl%stdout, knl%numthick, knl%npx, knl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'numthick'
intlist(3) = 'npx'
intlist(4) = 'nthreads' 
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ knl%voltage, knl%dmin, knl%startthick, knl%thickinc, knl%tfraction /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'startthick'
reallist(4) = 'thickinc'
reallist(5) = 'tfraction' 
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'Kosselmode'
line2(1) = knl%Kosselmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselMasterNameList: unable to create Kosselmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselMasterNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'outname'
line2(1) = knl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselMasterNameList: unable to create outname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteKosselMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteMCNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param mcnl Monte Carlo name list structure
!
!> @date 03/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteMCNameList(HDF_head, mcnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteMCNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(MCNameListType),INTENT(INOUT)                    :: mcnl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 7
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, sval(1), groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'MCNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%primeseed, mcnl%num_el, mcnl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'primeseed'
intlist(4) = 'num_el'
intlist(5) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single doubles
io_real = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, mcnl%Ebinsize, mcnl%depthmax, mcnl%depthstep /)
reallist(1) = 'sig'
reallist(2) = 'omega'
reallist(3) = 'EkeV'
reallist(4) = 'Ehistmin'
reallist(5) = 'Ebinsize'
reallist(6) = 'depthmax'
reallist(7) = 'depthstep'
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'MCmode'
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create MCmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'dataname'
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create dataname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteMCLIPSSNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param mcnl Monte Carlo name list structure
!
!> @date 12/01/15  PGC 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteMCLIPSSNameList(HDF_head, mcnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteMCLIPSSNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(MCLIPSSNameListType),INTENT(INOUT)                    :: mcnl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 10
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, sval(1), groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'MCLIPSSNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%primeseed, mcnl%num_el, mcnl%nthreads, mcnl%npx /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'primeseed'
intlist(4) = 'num_el'
intlist(5) = 'nthreads'
intlist(6) = 'npx'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single doubles
io_real=(/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, mcnl%Ebinsize, &
        mcnl%depthmax, mcnl%depthstep, mcnl%lipssamp, mcnl%lipsswave, mcnl%scaled/)
reallist(1) = 'sig'
reallist(2) = 'omega'
reallist(3) = 'EkeV'
reallist(4) = 'Ehistmin'
reallist(5) = 'Ebinsize'
reallist(6) = 'depthmax'
reallist(7) = 'depthstep'
reallist(8) = 'LIPSSamp'
reallist(9) = 'LIPSSwave'
reallist(10) = 'scaled'
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'MCmode'
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCLIPSSNameList: unable to create MCmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCLIPSSNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'dataname'
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCLIPSSNameList: unable to create dataname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCLIPSSNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteMCCLNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param mcnl Monte Carlon ame list structure
!
!> @date 03/21/15 MDG 1.0 new routine
!> @date 09/09/15 MDG 1.1 added devid (GPU device id)
!> @date 10/12/15 SS  1.2 changes to handle new mc program
!--------------------------------------------------------------------------
recursive subroutine HDFwriteMCCLNameList(HDF_head, mcnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteMCCLNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(MCCLNameListType),INTENT(INOUT)                  :: mcnl

integer(kind=irg),parameter                           :: n_int = 8, n_real_bse1 = 9, n_real_full = 7
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real_bse1(n_real_bse1), io_real_full(n_real_full)
character(20)                                         :: reallist_bse1(n_real_bse1), reallist_full(n_real_full)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, sval(1),groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'MCCLNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

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
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single doubles
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
   call HDF_writeNMLdbles(HDF_head, io_real_bse1, reallist_bse1, n_real_bse1)
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
   call HDF_writeNMLdbles(HDF_head, io_real_full, reallist_full, n_real_full)
end if

! write all the strings
dataset = 'MCmode'
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create MCmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'dataname'
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create dataname dataset',.TRUE.)

dataset = 'mode'
sval(1) = mcnl%mode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create mode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteMCCLMultiLayerNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param mcnl Monte Carlo name list structure
!
!> @date 03/21/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteMCCLMultiLayerNameList(HDF_head, mcnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteMCCLMultiLayerNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(MCCLMultiLayerNameListType),INTENT(INOUT)        :: mcnl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 9
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'MCCLMultiLayerNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%globalworkgrpsz, mcnl%num_el, mcnl%totnum_el /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'globalworkgrpsz'
intlist(4) = 'num_el'
intlist(5) = 'totnum_el'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

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
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'MCmode'
line2(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create MCmode dataset',.TRUE.)

dataset = 'xtalname_film'
line2(1) = mcnl%xtalname_film
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create xtalname_film dataset',.TRUE.)

dataset = 'xtalname_subs'
line2(1) = mcnl%xtalname_subs
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create xtalname_subs dataset',.TRUE.)

dataset = 'dataname'
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create dataname dataset',.TRUE.)

dataset = 'mode'
line2(1) = mcnl%mode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create mode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCCLMultiLayerNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EBSD master name list structure
!
!> @date 03/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSDMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(EBSDMasterNameListType),INTENT(INOUT)            :: emnl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = 'EBSDMasterNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
if (emnl%restart) then 
  restart = 1
else 
  restart = 0
end if
if (emnl%uniform) then 
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
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write a single real
dataset = 'dmin'
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create energyfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteTKDMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl TKD master name list structure
!
!> @date 03/21/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteTKDMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteTKDMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(TKDMasterNameListType),INTENT(INOUT)            :: emnl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = 'TKDMasterNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
if (emnl%restart) then 
  restart = 1
else 
  restart = 0
end if
if (emnl%uniform) then 
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
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write a single real
dataset = 'dmin'
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDMasterNameList: unable to create energyfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteTKDMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDMasterOpenCLNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EBSD master name list structure
!
!> @date 12/10/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSDMasterOpenCLNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDMasterOpenCLNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(EBSDMasterOpenCLNameListType),INTENT(INOUT)      :: emnl

integer(kind=irg),parameter                           :: n_int = 9, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = 'EBSDMasterOpenCLNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
if (emnl%restart) then 
  restart = 1
else 
  restart = 0
end if
if (emnl%uniform) then 
  uniform = 1
else 
  uniform = 0
end if
io_int = (/ emnl%stdout, emnl%npx, emnl%Esel, emnl%nthreads, restart, uniform, emnl%platid, &
            emnl%devid, emnl%globalworkgrpsz /)
intlist(1) = 'stdout'
intlist(2) = 'npx'
intlist(3) = 'Esel'
intlist(4) = 'nthreads'
intlist(5) = 'restart'
intlist(6) = 'uniform'
intlist(7) = 'platid'
intlist(8) = 'devid'
intlist(9) = 'globalworkgrpsz'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write a single real
dataset = 'dmin'
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create energyfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDMasterOpenCLNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteBetheparameterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param BetheParameters Bethe Parameter name list structure
!
!> @date 09/27/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteBetheparameterNameList(HDF_head, BetheParameters)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteBetheparameterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(BetheParameterType),INTENT(INOUT)                :: BetheParameters

integer(kind=irg),parameter                           :: n_int = 1, n_real = 4
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! We are not writing the complete BetheParameters structure to the name list, only
! the parameters of importance for the dynamical simulations ... 

! create the group for this namelist
groupname = 'BetheList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single doubles
io_real = (/ BetheParameters%c1, BetheParameters%c2, Betheparameters%c3, BetheParameters%sgdbdiff /)
            
reallist(1) = 'c1'
reallist(2) = 'c2'
reallist(3) = 'c3'
reallist(4) = 'sgdbdiff'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteBetheparameterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDclusterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EBSDcluster name list structure
!
!> @date 12/28/15  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSDclusterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDclusterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(EBSDclusterNameListType),INTENT(INOUT)           :: emnl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'EBSDclusterNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

io_int = (/ emnl%NClusters, emnl%NIterations, emnl%binfactor /)
intlist(1) = 'NClusters'
intlist(2) = 'NIterations'
intlist(3) = 'binfactor'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the strings
dataset = 'inputfilename'
line2(1) = emnl%inputfilename
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDclusterNameList: unable to create inputfilename dataset',.TRUE.)

dataset = 'groupname'
line2(1) = emnl%groupname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDclusterNameList: unable to create groupname dataset',.TRUE.)

dataset = 'datasetname'
line2(1) = emnl%datasetname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDclusterNameList: unable to create datasetname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDclusterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param ecpnl ECP master name list structure
!
!> @date 03/22/15 MDG 1.0 new routine
!> @date 09/15/15 SS  1.1 changes after clean up of ECPmasterNameList
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPMasterNameList(HDF_head, ecpnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECPMasterNameListType),INTENT(INOUT)             :: ecpnl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 1
integer(kind=irg)                                     :: hdferr, io_int(n_int), distort
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPMasterNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
! distort is a logical, for which there is no real HDF_NATIVE_anything conversion, so we'll store it as a 1 or 0
!if (ecpnl%distort) then 
!  distort = 1
!else 
!  distort = 0
!end if

io_int = (/ ecpnl%stdout, ecpnl%Esel, ecpnl%npx, ecpnl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'Esel'
intlist(3) = 'npx'
intlist(4) = 'nthreads'
!intlist(5) = 'distort'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)


! write all the single doubles
io_real = (/ ecpnl%dmin /)
reallist(1) = 'dmin'
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'outname'
line2(1) = ecpnl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPMasterNameList: unable to create outname dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPMasterNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'compmode'
line2(1) = ecpnl%compmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPMasterNameList: unable to create compmode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPZANameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param ecpnl ECPZA name list structure
!
!> @date 01/25/17 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPZANameList(HDF_head, ecpnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPZANameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECPZANameListType),INTENT(INOUT)                 :: ecpnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 2
integer(kind=irg)                                     :: hdferr, io_int(n_int), distort
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPZANameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%npix, ecpnl%nthreads/)
intlist(1) = 'npix'
intlist(2) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single floats
io_real = (/ ecpnl%dmin, ecpnl%ktmax /)
reallist(1) = 'dmin'
reallist(2) = 'ktmax'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! 3-vectors
dataset = 'k'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create k dataset',.TRUE.)

dataset = 'fn'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%fn, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create fn dataset',.TRUE.)

! write all the strings
dataset = 'outname'
line2(1) = ecpnl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create outname dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create energyfile dataset',.TRUE.)

dataset = 'maskpattern'
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create maskpattern dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPZANameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param enl EBSD name list structure
!
!> @date 03/22/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSDNameList(HDF_head, enl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(EBSDNameListType),INTENT(INOUT)                  :: enl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 9
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: t(1)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = 'EBSDNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ enl%stdout, enl%numsx, enl%numsy, enl%binning, enl%nthreads, enl%energyaverage /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'numsy'
intlist(4) = 'binning'
intlist(5) = 'nthreads'
intlist(6) = 'energyaverage'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

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
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! a 4-vector
dataset = 'axisangle'
hdferr = HDF_writeDatasetFloatArray1D(dataset, enl%axisangle, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create axisangle dataset',.TRUE.)

! a few doubles
dataset = 'beamcurrent'
hdferr = HDF_writeDatasetDouble(dataset, enl%beamcurrent, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create beamcurrent dataset',.TRUE.)

dataset = 'dwelltime'
hdferr = HDF_writeDatasetDouble(dataset, enl%dwelltime, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create dwelltime dataset',.TRUE.)

! write all the strings
dataset = 'maskpattern'
line2(1) = trim(enl%maskpattern)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create maskpattern dataset',.TRUE.)

dataset = 'scalingmode'
line2(1) = trim(enl%scalingmode)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create scalingmode dataset',.TRUE.)

dataset = 'eulerconvention'
line2(1) = trim(enl%eulerconvention)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create eulerconvention dataset',.TRUE.)

dataset = 'outputformat'
line2(1) = trim(enl%outputformat)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create outputformat dataset',.TRUE.)

dataset = 'bitdepth'
line2(1) = trim(enl%bitdepth)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create bitdepth dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = trim(enl%energyfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'masterfile'
line2(1) = trim(enl%masterfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create masterfile dataset',.TRUE.)

dataset = 'anglefile'
line2(1) = trim(enl%anglefile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create anglefile dataset',.TRUE.)

dataset = 'datafile'
line2(1) = trim(enl%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create datafile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param ecpnl ECP namelist structure
!
!> @date 03/22/15 MDG 1.0 new routine
!> @date 09/15/15 SS  1.1 changes after updating ECPNameListType
!> @date 10/15/15 SS  1.2 changes for release
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPNameList(HDF_head, ecpnl, twolayerflag)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPNameList

use ISO_C_BINDING
use error

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECPNameListType),INTENT(INOUT)                   :: ecpnl
logical,INTENT(IN)                                    :: twolayerflag

integer(kind=irg),parameter                           :: n_int = 2
integer(kind=irg)                                     :: n_real
integer(kind=irg)                                     :: hdferr,  io_int(n_int), istat
real(kind=sgl),allocatable                            :: io_real(:)
character(20)                                         :: intlist(n_int)
character(20),allocatable                             :: reallist(:)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%nthreads, ecpnl%npix /)
intlist(1) = 'nthreads'
intlist(2) = 'npix'

call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)
if (twolayerflag) then
! integer vectors
    dataset = 'fn_f'
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%fn_f, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create k dataset',.TRUE.)

    dataset = 'fn_s'
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%fn_s, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create fn dataset',.TRUE.)

    dataset = 'gF'
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%gF, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create gF dataset',.TRUE.)

    dataset = 'gS'
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%gS, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create gS dataset',.TRUE.)

    dataset = 'tF'
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%tF, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create tF dataset',.TRUE.)

    dataset = 'tS'
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%tS, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create tS dataset',.TRUE.)
    
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

    call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

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

    call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

end if

! write all the strings

dataset = 'energyfile'
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'datafile'
line2(1) = ecpnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create datafile dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = ecpnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname dataset',.TRUE.)

if (twolayerflag) then

    dataset = 'xtalname2'
    line2(1) = ecpnl%xtalname2
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname2 dataset',.TRUE.)

end if

dataset = 'masterfile'
line2(1) = ecpnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create masterfile dataset',.TRUE.)

if (twolayerflag) then

    dataset = 'filmfile'
    line2(1) = ecpnl%filmfile
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create filmfile dataset',.TRUE.)

    dataset = 'subsfile'
    line2(1) = ecpnl%subsfile
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create subsfile dataset',.TRUE.)

end if

dataset = 'maskpattern'
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create mask dataset',.TRUE.)

dataset = 'anglefile'
line2(1) = ecpnl%anglefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create anglefile dataset',.TRUE.)

dataset = 'outputformat'
line2(1) = trim(ecpnl%outputformat)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create outputformat dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPSingleNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param ecpnl ECP namelist structure
!
!> @date 03/22/15 MDG 1.0 new routine
!> @date 09/15/15 SS  1.1 changes after updating ECPNameListType
!> @date 10/15/15 SS  1.2 changes for release
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPSingleNameList(HDF_head, ecpnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPSingleNameList

use ISO_C_BINDING
use error

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECPSingleNameListType),INTENT(INOUT)             :: ecpnl

integer(kind=irg),parameter                           :: n_int = 2
integer(kind=irg)                                     :: n_real
integer(kind=irg)                                     :: hdferr,  io_int(n_int), istat
real(kind=sgl),allocatable                            :: io_real(:)
character(20)                                         :: intlist(n_int)
character(20),allocatable                             :: reallist(:)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%nthreads, ecpnl%npix /)
intlist(1) = 'nthreads'
intlist(2) = 'npix'

call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

n_real = 10
allocate(reallist(n_real),io_real(n_real),stat=istat)
if (istat .ne. 0) then
    call FatalError('HDFwriteECPNameList','Cannot allocate the reallist array')
end if
! write all the single reals
io_real = (/ ecpnl%thetac, sngl(ecpnl%sampletilt),ecpnl%gammavalue, ecpnl%workingdistance, &
             ecpnl%Rin, ecpnl%Rout, sngl(ecpnl%phi1), sngl(ecpnl%phi), sngl(ecpnl%phi2), ecpnl%dmin /)
reallist(1) = 'thetac'
reallist(2) = 'sampletilt'
reallist(3) = 'gammavalue'
reallist(4) = 'workingdistance'
reallist(5) = 'Rin'
reallist(6) = 'Rout'
reallist(7) = 'phi1'
reallist(8) = 'phi'
reallist(9) = 'phi2'
reallist(10) = 'dmin'

call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings

dataset = 'energyfile'
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'datafile'
line2(1) = ecpnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create datafile dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = ecpnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'maskpattern'
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create mask dataset',.TRUE.)

dataset = 'eulerconvention'
line2(1) = ecpnl%eulerconvention
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create eulerconvention dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPSingleNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPSingleNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param ecpnl ECP namelist structure
!
!> @date 03/22/15 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPDictionaryIndexingNameList(HDF_head, ecpnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPDictionaryIndexingNameList

use ISO_C_BINDING
use error

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECPIndexingNameListType),INTENT(INOUT)           :: ecpnl

integer(kind=irg),parameter                           :: n_int = 11
integer(kind=irg),parameter                           :: n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int), istat
real(kind=sgl),allocatable                            :: io_real(:)
character(20)                                         :: intlist(n_int)
character(20),allocatable                             :: reallist(:)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPIndexingNameListType'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%ncubochoric, ecpnl%totnumexpt, ecpnl%nnk, ecpnl%npix, ecpnl%maskradius, ecpnl%nregions,&
          ecpnl%numdictsingle, ecpnl%numexptsingle,ecpnl%nthreads, ecpnl%platid, ecpnl%devid /)
intlist(1) = 'ncubochoric'
intlist(2) = 'totnumexpt'
intlist(3) = 'nnk'
intlist(4) = 'npix'
intlist(5) = 'maskradius'
intlist(6) = 'nregions'
intlist(7) = 'numdictsingle'
intlist(8) = 'numexptsingle'
intlist(9) = 'nthreads'
intlist(10) = 'platid'
intlist(11) = 'devid'

call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

allocate(reallist(n_real),io_real(n_real),stat=istat)
if (istat .ne. 0) then
    call FatalError('HDFwriteECPNameList','Cannot allocate the reallist array')
end if
! write all the single reals
io_real = (/ ecpnl%thetac, sngl(ecpnl%sampletilt),ecpnl%gammavalue, ecpnl%workingdistance, &
             ecpnl%Rin, ecpnl%Rout /)
reallist(1) = 'thetac'
reallist(2) = 'sampletilt'
reallist(3) = 'gammavalue'
reallist(4) = 'workingdistance'
reallist(5) = 'Rin'
reallist(6) = 'Rout'

call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings

dataset = 'energyfile'
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'exptfile'
line2(1) = ecpnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'datafile'
line2(1) = ecpnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create datafile dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = ecpnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'maskpattern'
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create mask dataset',.TRUE.)

dataset = 'masterfile'
line2(1) = ecpnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create masterfile dataset',.TRUE.)

dataset = 'tmpfile'
line2(1) = ecpnl%tmpfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create tmpfile dataset',.TRUE.)

dataset = 'ctffile'
line2(1) = ecpnl%ctffile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create ctffile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPDictionaryIndexingNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteLACBEDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param lacbednl LACBED name list structure
!
!> @date 06/22/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteLACBEDNameList(HDF_head, lacbednl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteLACBEDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(LACBEDNameListType),INTENT(INOUT)                :: lacbednl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'LACBEDNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ lacbednl%stdout, lacbednl%maxHOLZ, lacbednl%numthick, lacbednl%npix, lacbednl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'maxHOLZ'
intlist(3) = 'numthick'
intlist(4) = 'npix'
intlist(5) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! vectors
dataset = 'k'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, lacbednl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create k dataset',.TRUE.)

dataset = 'fn'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, lacbednl%fn, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create fn dataset',.TRUE.)

! write all the single reals
io_real = (/ lacbednl%voltage, lacbednl%dmin, lacbednl%convergence, lacbednl%startthick, lacbednl%thickinc, lacbednl%minten/)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'convergence'
reallist(4) = 'startthick'
reallist(5) = 'thickinc'
reallist(6) = 'minten'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'outname'
line2(1) = lacbednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create outname dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = lacbednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create xtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteLACBEDNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPpatternNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist into HDF file
!
!> @param HDF_head top of push stack
!> @param ecpnl ECP name list structure
!
!> @date 03/22/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPpatternNameList(HDF_head,ecpnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPpatternNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECPpatternNameListType),INTENT(INOUT)            :: ecpnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPpatternNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%stdout, ecpnl%npix /)
intlist(1) = 'stdout'
intlist(2) = 'npix'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! single real
dataset = 'thetac'
hdferr = HDF_writeDatasetFloat(dataset, ecpnl%thetac, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create thetac dataset',.TRUE.)

! real vector
dataset = 'k'
hdferr = HDF_writeDatasetFloatArray1D(dataset, ecpnl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create k dataset',.TRUE.)

! write all the strings
dataset = 'outname'
line2(1) = ecpnl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create outname dataset',.TRUE.)

dataset = 'masterfile'
line2(1) = ecpnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create masterfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPpatternNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwritePEDkinNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param pednl PED name list structure
!
!> @date 03/22/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwritePEDkinNameList(HDF_head,pednl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwritePEDkinNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(PEDkinNameListType),INTENT(INOUT)                :: pednl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 4
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'PEDkinNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ pednl%stdout, pednl%npix, pednl%ncubochoric, pednl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'npix'
intlist(3) = 'ncubochoric'
intlist(4) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ pednl%voltage, pednl%thickness, pednl%dmin, pednl%rnmpp /)
reallist(1) = 'voltage'
reallist(2) = 'thickness'
reallist(3) = 'dmin'
reallist(4) = 'rnmpp'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'outname'
line2(1) = pednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDkinNameList: unable to create outname dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = pednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDkinNameList: unable to create xtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwritePEDkinNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwritekinematicalNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param pednl PED name list structure
!
!> @date 03/22/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwritekinematicalNameList(HDF_head,knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwritekinematicalNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(kinematicalNameListType),INTENT(INOUT)                :: knl

integer(kind=irg),parameter                           :: n_int = 0, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'EMkinematicalNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single reals
io_real = (/ knl%voltage, knl%dmin, knl%thr /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'thr'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'xtalname'
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritekinematicalNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'datafile'
line2(1) = knl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritekinematicalNameList: unable to create datafile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwritekinematicalNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwritePEDZANameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param pednl PED name list structure
!
!> @date 03/23/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwritePEDZANameList(HDF_head,pednl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwritePEDZANameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(PEDZANameListType),INTENT(INOUT)                   :: pednl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr, io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'PEDZANameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ pednl%stdout, pednl%precsample, pednl%precazimuthal, pednl%npix, pednl%nthreads /)
intlist(1) = 'stdout'
intlist(2) = 'precsample'
intlist(3) = 'precazimuthal'
intlist(4) = 'npix'
intlist(5) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! vectors
dataset = 'k'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, pednl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create k dataset',.TRUE.)

dataset = 'fn'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, pednl%fn, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create fn dataset',.TRUE.)

! single reals
io_real = (/ pednl%voltage, pednl%dmin, pednl%precangle, pednl%prechalfwidth, pednl%thickness, pednl%camlen /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'precangle'
reallist(4) = 'prechalfwidth'
reallist(5) = 'thickness'
reallist(6) = 'camlen'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'outname'
line2(1) = pednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create outname dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = pednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create xtalname dataset',.TRUE.)

dataset = 'filemode'
line2(1) = pednl%filemode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create filemode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwritePEDZANameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECCINameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param eccinl ECCI name list structure
!
!> @date 03/23/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECCINameList(HDF_head,eccinl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECCINameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(ECCINameListType),INTENT(INOUT)                  :: eccinl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECCINameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ eccinl%stdout, eccinl%nthreads, eccinl%nktstep, eccinl%DF_npix, eccinl%DF_npiy /)
intlist(1) = 'stdout'
intlist(2) = 'nthreads'
intlist(3) = 'nktstep'
intlist(4) = 'DF_npix'
intlist(5) = 'DF_npiy'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! vectors
dataset = 'k'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, eccinl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create k dataset',.TRUE.)

! single reals
io_real = (/ eccinl%voltage, eccinl%dkt, eccinl%ktmax, eccinl%dmin, eccinl%DF_L, eccinl%DF_slice /)
reallist(1) = 'voltage'
reallist(2) = 'dkt'
reallist(3) = 'ktmax'
reallist(4) = 'dmin'
reallist(5) = 'DF_L'
reallist(6) = 'DF_slice'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! 2-vectors
dataset = 'lauec'
hdferr = HDF_writeDatasetFloatArray1D(dataset, eccinl%lauec, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create lauec dataset',.TRUE.)

dataset = 'lauec2'
hdferr = HDF_writeDatasetFloatArray1D(dataset, eccinl%lauec2, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create lauec2 dataset',.TRUE.)

! write all the strings
dataset = 'dispmode'
line2(1) = eccinl%dispmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create dispmode dataset',.TRUE.)

dataset = 'summode'
line2(1) = eccinl%summode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create summode dataset',.TRUE.)

dataset = 'progmode'
line2(1) = eccinl%progmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create progmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = eccinl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create xtalname dataset',.TRUE.)

dataset = 'defectfilename'
line2(1) = eccinl%defectfilename
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create defectfilename dataset',.TRUE.)

dataset = 'dispfile'
line2(1) = eccinl%dispfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create dispfile dataset',.TRUE.)

dataset = 'dataname'
line2(1) = eccinl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create dataname dataset',.TRUE.)

dataset = 'ECPname'
line2(1) = eccinl%ECPname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create ECPname dataset',.TRUE.)

dataset = 'sgname'
line2(1) = eccinl%sgname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create sgname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

! the HDFwriteDefectData routine will put all the defect variables in the HDF5 file (to be written)

end subroutine HDFwriteECCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteRFZNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to an HDF file
!
!> @param HDF_head top of push stack
!> @param rfznl RFZ name list structure
!
!> @date 03/23/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteRFZNameList(HDF_head,rfznl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteRFZNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(RFZNameListType),INTENT(INOUT)                   :: rfznl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'RFZNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ rfznl%pgnum, rfznl%nsteps, rfznl%gridtype /)
intlist(1) = 'pgnum'
intlist(2) = 'nsteps'
intlist(3) = 'gridtype'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! strings
dataset = 'euoutname'
line2(1) = rfznl%euoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create euler outname dataset',.TRUE.)

dataset = 'cuoutname'
line2(1) = rfznl%cuoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create cubochoric outname dataset',.TRUE.)

dataset = 'hooutname'
line2(1) = rfznl%hooutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create homochoric outname dataset',.TRUE.)

dataset = 'rooutname'
line2(1) = rfznl%rooutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create rodrigues outname dataset',.TRUE.)

dataset = 'quoutname'
line2(1) = rfznl%quoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create quaternion outname dataset',.TRUE.)

dataset = 'omoutname'
line2(1) = rfznl%omoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create orientation matrix outname dataset',.TRUE.)

dataset = 'axoutname'
line2(1) = rfznl%axoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create axis angle pair outname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteRFZNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteDictIndxOpenCLNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param rfznl RFZ name list structure
!
!> @date 03/23/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteDictIndxOpenCLNameList(HDF_head,dictindxnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteDictIndxOpenCLNameList

use ISO_C_BINDING

use local

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(DictIndxOpenCLListType),INTENT(INOUT)            :: dictindxnl

integer(kind=irg),parameter                           :: n_int = 8, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'DictIndxOpenCLNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! logical will be written as an integer 1 or 0
if (dictindxnl%MeanSubtraction) then 
  i = 1
else
  i = 0 
end if
! write all the single integers
io_int = (/ dictindxnl%numexptsingle, dictindxnl%numdictsingle, dictindxnl%totnumdict, dictindxnl%totnumexpt, dictindxnl%imght, &
            dictindxnl%imgwd, dictindxnl%nnk, i /)
intlist(1) = 'numexptsingle'
intlist(2) = 'numdictsingle'
intlist(3) = 'totnumdict'
intlist(4) = 'totnumexpt'
intlist(5) = 'imght'
intlist(6) = 'imgwd'
intlist(7) = 'nnk'
intlist(8) = 'MeanSubtraction'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! strings
dataset = 'exptfile'
line2(1) = dictindxnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteDictIndxOpenCLNameList: unable to create exptfile dataset',.TRUE.)

dataset = 'dictfile'
line2(1) = dictindxnl%dictfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteDictIndxOpenCLNameList: unable to create dictfile dataset',.TRUE.)

dataset = 'eulerfile'
line2(1) = dictindxnl%eulerfile 
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteDictIndxOpenCLNameList: unable to create eulerfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteDictIndxOpenCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDDictionaryIndexingNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param rfznl RFZ name list structure
!
!> @date 03/23/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSDDictionaryIndexingNameList(HDF_head,ebsdnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDDictionaryIndexingNameList

use ISO_C_BINDING

use local

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(EBSDIndexingNameListType),INTENT(INOUT)          :: ebsdnl

integer(kind=irg),parameter                           :: n_int = 17, n_real = 11, n_reald = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: io_reald(n_reald)
character(20)                                         :: intlist(n_int), reallist(n_real), realdlist(n_reald)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'EBSDIndexingNameListType'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ebsdnl%ncubochoric, ebsdnl%numexptsingle, ebsdnl%numdictsingle, ebsdnl%ipf_ht, &
            ebsdnl%ipf_wd, ebsdnl%nnk, ebsdnl%maskradius, ebsdnl%numsx, ebsdnl%numsy, ebsdnl%binning, &
            ebsdnl%nthreads, ebsdnl%energyaverage, ebsdnl%devid, ebsdnl%platid, ebsdnl%nregions, ebsdnl%nnav, &
            ebsdnl%nosm /)
intlist(1) = 'ncubochoric'
intlist(2) = 'numexptsingle'
intlist(3) = 'numdictsingle'
intlist(4) = 'ipf_ht'
intlist(5) = 'ipf_wd '
intlist(6) = 'nnk'
intlist(7) = 'maskradius'
intlist(8) = 'numsx'
intlist(9) = 'numsy'
intlist(10) = 'binning'
intlist(11) = 'nthreads'
intlist(12) = 'energyaverage'
intlist(13) = 'devid'
intlist(14) = 'platid'
intlist(15) = 'nregions'
intlist(16) = 'nnav'
intlist(17) = 'nosm'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

io_real = (/ ebsdnl%L, ebsdnl%thetac, ebsdnl%delta, ebsdnl%omega, ebsdnl%xpc, &
             ebsdnl%ypc, ebsdnl%energymin, ebsdnl%energymax, ebsdnl%gammavalue, ebsdnl%StepX, ebsdnl%StepY /)
reallist(1) = 'L'
reallist(2) = 'thetac'
reallist(3) = 'delta'
reallist(4) = 'omega'
reallist(5) = 'xpc'
reallist(6) = 'ypc'
reallist(7) = 'energymin'
reallist(8) = 'energymax'
reallist(9) = 'gammavalue'
reallist(10) = 'StepX'
reallist(11) = 'StepY'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

io_reald = (/ ebsdnl%beamcurrent, ebsdnl%dwelltime, ebsdnl%hipassw /)
realdlist(1) = 'beamcurrent'
realdlist(2) = 'dwelltime'
realdlist(3) = 'hipassw'
call HDF_writeNMLdbles(HDF_head, io_reald, realdlist, n_reald)

! a 4-vector
dataset = 'axisangle'
hdferr = HDF_writeDatasetFloatArray1D(dataset, ebsdnl%axisangle, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create axisangle dataset',.TRUE.)


! strings
dataset = 'maskpattern'
line2(1) = ebsdnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create maskpattern dataset',.TRUE.)

dataset = 'scalingmode'
line2(1) = ebsdnl%scalingmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create scalingmode dataset',.TRUE.)

!dataset = 'eulerconvention'
!line2(1) = ebsdnl%eulerconvention
!hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create eulerconvention dataset', &
!                                      .TRUE.)

!dataset = 'outputformat'
!line2(1) = ebsdnl%outputformat
!hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create outputformat dataset', &
!                                      .TRUE.)

dataset = 'spatialaverage'
line2(1) = ebsdnl%spatialaverage
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create spatialaverage dataset', &
                                      .TRUE.)

dataset = 'exptfile'
line2(1) = ebsdnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create exptfile dataset',.TRUE.)

dataset = 'masterfile'
line2(1) = ebsdnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create masterfile dataset',.TRUE.)

dataset = 'energyfile'
line2(1) = ebsdnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'datafile'
line2(1) = ebsdnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create datafile dataset',.TRUE.)

dataset = 'tmpfile'
line2(1) = ebsdnl%tmpfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create tmpfile dataset',.TRUE.)

dataset = 'ctffile'
line2(1) = ebsdnl%ctffile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create ctffile dataset',.TRUE.)

dataset = 'angfile'
line2(1) = ebsdnl%angfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create angfile dataset',.TRUE.)

dataset = 'anglefile'
line2(1) = ebsdnl%anglefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create anglefile dataset',.TRUE.)

dataset = 'eulerfile'
line2(1) = ebsdnl%eulerfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create eulerfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDDictionaryIndexingNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteMCCLsphereNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param mcnl Monte Carlon ame list structure
!
!> @date 10/20/16 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteMCCLsphereNameList(HDF_head, mcnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteMCCLsphereNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(MCCLNameListType),INTENT(INOUT)                  :: mcnl

integer(kind=irg),parameter                           :: n_int = 8, n_real_bse1 = 10, n_real_full = 8
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real_bse1(n_real_bse1), io_real_full(n_real_full)
character(20)                                         :: reallist_bse1(n_real_bse1), reallist_full(n_real_full)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, sval(1),groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'MCCLNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

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
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single doubles
if (mcnl%mode .eq. 'bse1') then
   io_real_bse1 = (/ mcnl%sigstart, mcnl%sigend, mcnl%sigstep, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, &
             mcnl%Ebinsize, mcnl%depthstep, mcnl%radius, mcnl%incloc /)
   reallist_bse1(1) = 'sigstart'
   reallist_bse1(2) = 'sigend'
   reallist_bse1(3) = 'sigstep'
   reallist_bse1(4) = 'omega'
   reallist_bse1(5) = 'EkeV'
   reallist_bse1(6) = 'Ehistmin'
   reallist_bse1(7) = 'Ebinsize'
   reallist_bse1(8) = 'depthstep'
   reallist_bse1(9) = 'radius'
   reallist_bse1(10) = 'incloc'

   call HDF_writeNMLdbles(HDF_head, io_real_bse1, reallist_bse1, n_real_bse1)
else if (mcnl%mode .eq. 'full') then
   io_real_full = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, &
             mcnl%Ebinsize, mcnl%depthstep, mcnl%radius, mcnl%incloc /)
   reallist_full(1) = 'sig'
   reallist_full(2) = 'omega'
   reallist_full(3) = 'EkeV'
   reallist_full(4) = 'Ehistmin'
   reallist_full(5) = 'Ebinsize'
   reallist_full(6) = 'depthstep'
   reallist_bse1(7) = 'radius'
   reallist_bse1(8) = 'incloc'

   call HDF_writeNMLdbles(HDF_head, io_real_full, reallist_full, n_real_full)
end if

! write all the strings
dataset = 'MCmode'
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create MCmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'dataname'
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create dataname dataset',.TRUE.)

dataset = 'mode'
sval(1) = mcnl%mode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create mode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCCLsphereNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteMCCLfoilNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param mcnl Monte Carlo name list structure
!
!> @date 01/15/17 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteMCCLfoilNameList(HDF_head, mcnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteMCCLfoilNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT),pointer        :: HDF_head
type(MCCLNameListType),INTENT(INOUT)                  :: mcnl

integer(kind=irg),parameter                           :: n_int = 8, n_real = 8
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: reallist(n_real)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, sval(1),groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'MCCLfoilNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

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
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single doubles
io_real = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, mcnl%Ehistmin, &
   mcnl%Ebinsize, mcnl%depthstep, mcnl%thickness, mcnl%depthmax /)
reallist(1) = 'sig'
reallist(2) = 'omega'
reallist(3) = 'EkeV'
reallist(4) = 'Ehistmin'
reallist(5) = 'Ebinsize'
reallist(6) = 'depthstep'
reallist(7) = 'thickness'
reallist(8) = 'depthmax'
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = 'MCmode'
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create MCmode dataset',.TRUE.)

dataset = 'xtalname'
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'dataname'
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create dataname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCCLfoilNameList




end module NameListHDFwriters
