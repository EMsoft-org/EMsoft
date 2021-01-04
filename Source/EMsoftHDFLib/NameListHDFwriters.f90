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
use stringconstants

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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg),INTENT(IN)                          :: n_int
integer(kind=irg),INTENT(IN)                          :: io_int(n_int)
character(20),INTENT(IN)                              :: intlist(n_int)

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
logical                                               :: g_exists, overwrite=.TRUE.

do i=1,n_int
  dataset = intlist(i)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg),INTENT(IN)                          :: n_real
real(kind=sgl),INTENT(IN)                             :: io_real(n_real)
character(20),INTENT(IN)                              :: reallist(n_real)

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
logical                                               :: g_exists, overwrite=.TRUE.

do i=1,n_real
  dataset = reallist(i)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
integer(kind=irg),INTENT(IN)                          :: n_real
real(kind=dbl),INTENT(IN)                             :: io_real(n_real)
character(20),INTENT(IN)                              :: reallist(n_real)

integer(kind=irg)                                     :: hdferr, i
character(fnlen)                                      :: dataset
logical                                               :: g_exists, overwrite=.TRUE.

do i=1,n_real
  dataset = reallist(i)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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
! SUBROUTINE:HDFwriteLorentzNameList
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
recursive subroutine HDFwriteLorentzNameList(HDF_head, knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteLorentzNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(LorentzNameListType),INTENT(IN)                  :: knl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 8
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_LorentzNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ knl%nthreads, knl%numappos, knl%numdefocus, knl%numtilts /)
intlist(1) = 'nthreads'
intlist(2) = 'numappos'
intlist(3) = 'numdefocus'
intlist(4) = 'numtilts'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ knl%voltage, knl%apertureradius, knl%defocusstart, knl%defocusstep, knl%defocusspread, knl%thetac, &
             knl%tiltstart, knl%tiltstepsize /)
reallist(1) = 'voltage'
reallist(2) = 'apertureradius'
reallist(2) = 'defocusstart'
reallist(2) = 'defocusstep'
reallist(2) = 'defocusspread'
reallist(2) = 'thetac'
reallist(2) = 'tiltstart'
reallist(2) = 'tiltstepsize'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! real vectors
dataset = SC_apertureposition
hdferr = HDF_writeDatasetFloatArray1D(dataset, knl%apertureposition, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create apertureposition dataset',.TRUE.)

dataset = SC_astigmatism
hdferr = HDF_writeDatasetFloatArray1D(dataset, knl%astigmatism, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create astigmatism dataset',.TRUE.)

dataset = SC_beamdc
hdferr = HDF_writeDatasetFloatArray1D(dataset, knl%beamdc, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create beamdc dataset',.TRUE.)

dataset = SC_tiltaxis
hdferr = HDF_writeDatasetFloatArray1D(dataset, knl%tiltaxis, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create tiltaxis dataset',.TRUE.)

! write all the strings
dataset = SC_Magfile
line2(1) = knl%Magfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create Magfile dataset',.TRUE.)

dataset = SC_phasemethod
line2(1) = knl%phasemethod
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create phasemethod dataset',.TRUE.)

dataset = SC_frfo
line2(1) = knl%frfo
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create frfo dataset',.TRUE.)

dataset = SC_outputfile
line2(1) = knl%outputfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create outputfile dataset',.TRUE.)

dataset = SC_phiefile
line2(1) = knl%phiefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create phiefile dataset',.TRUE.)

dataset = SC_phimfile
line2(1) = knl%phimfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create phimfile dataset',.TRUE.)

dataset = SC_intBxfile
line2(1) = knl%intBxfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create intBxfile dataset',.TRUE.)

dataset = SC_intByfile
line2(1) = knl%intByfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create intByfile dataset',.TRUE.)

dataset = SC_colormapfile
line2(1) = knl%colormapfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLorentzNameList: unable to create colormapfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteLorentzNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(KosselNameListType),INTENT(IN)                   :: knl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_KosselNameList
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
dataset = SC_k
hdferr = HDF_writeDatasetIntegerArray1D(dataset, knl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselNameList: unable to create k dataset',.TRUE.)

dataset = SC_fn
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
dataset = SC_xtalname
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_outname
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(KosselMasterNameListType),INTENT(IN)             :: knl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 5
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_KosselMasterNameList
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
dataset = SC_Kosselmode
line2(1) = knl%Kosselmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselMasterNameList: unable to create Kosselmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselMasterNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_outname
line2(1) = knl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteKosselMasterNameList: unable to create outname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteKosselMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteLaueMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param knl name list structure
!
!> @date 09/06/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteLaueMasterNameList(HDF_head, knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteLaueMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(LaueMasterNameListType),INTENT(IN)               :: knl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 2, n_double = 2
integer(kind=irg)                                     :: hdferr,  io_int(n_int), nm, binarize
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: io_double(n_double)
character(20)                                         :: intlist(n_int), reallist(n_real), dbllist(n_double)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_LauemasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

binarize = 0
if (knl%binarize.eqv..TRUE.) binarize = 1

! write all the single integers
io_int = (/ knl%npx, knl%patchw, binarize /)
intlist(1) = 'npx'
intlist(2) = 'patchw'
intlist(3) = 'binarize'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ knl%lambdamin, knl%lambdamax /)
reallist(1) = 'lambdamin'
reallist(2) = 'lambdamax'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the double reals
io_double = (/ knl%kappaVMF, knl%intfactor /)
dbllist(1) = 'kappaVMF'
dbllist(2) = 'intfactor'
call HDF_writeNMLdbles(HDF_head, io_double, dbllist, n_double)

! write all the strings
dataset = SC_xtalname
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueMasterNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'hdfname'
line2(1) = knl%hdfname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueMasterNameList: unable to create hdfname dataset',.TRUE.)

dataset = 'tiffname'
line2(1) = knl%hdfname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueMasterNameList: unable to create tiffname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteLaueMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteLaueNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param lnl name list structure
!
!> @date 07/30/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteLaueNameList(HDF_head, lnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteLaueNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(LaueNameListType),INTENT(IN)                     :: lnl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int), nm
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_LaueNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ lnl%numpx, lnl%numpy, lnl%nthreads, lnl%BPx /)
intlist(1) = 'numpx'
intlist(2) = 'numpy'
intlist(3) = 'nthreads'
intlist(4) = 'BPx'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ lnl%spotw, lnl%pixelsize, lnl%maxVoltage, lnl%minVoltage, lnl%SDdistance, lnl%gammavalue /)
reallist(1) = 'spotw'
reallist(2) = 'pixelsize'
reallist(3) = 'maxVoltage'
reallist(4) = 'minVoltage'
reallist(5) = 'SDdistance'
reallist(6) = 'gammavalue'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = SC_xtalname
line2(1) = lnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'hdfname'
line2(1) = lnl%hdfname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create hdfname dataset',.TRUE.)

dataset = 'tiffprefix'
line2(1) = lnl%tiffprefix
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create tiffprefix dataset',.TRUE.)

dataset = 'Lauemode'
line2(1) = lnl%Lauemode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create Lauemode dataset',.TRUE.)

dataset = 'backprojection'
line2(1) = lnl%backprojection
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create backprojection dataset',.TRUE.)

dataset = 'orientationfile'
line2(1) = lnl%orientationfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create orientationfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteLaueNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteLaueSlitNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param lnl name list structure
!
!> @date 01/30/20  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteLaueSlitNameList(HDF_head, lnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteLaueSlitNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(LaueSlitNameListType),INTENT(IN)                 :: lnl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 3, n_dbl = 17
integer(kind=irg)                                     :: hdferr,  io_int(n_int), nm
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: io_dbl(n_dbl)   
character(20)                                         :: intlist(n_int), reallist(n_real), dbllist(n_dbl)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_LaueNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ lnl%Ny, lnl%Nz, lnl%nthreads, lnl%BPx /)
intlist(1) = 'Ny'
intlist(2) = 'Nz'
intlist(3) = 'nthreads'
intlist(4) = 'BPx'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ lnl%spotw, lnl%gammavalue, lnl%sampletilt/)
reallist(1) = 'spotw'
reallist(2) = 'gammavalue'
reallist(3) = 'sampletilt'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the single reals
io_dbl = (/ lnl%Lw, lnl%Lh, lnl%Lx, lnl%Ly, lnl%Lz, lnl%VoltageH, lnl%VoltageL, lnl%Sx, &
            lnl%sampletodetector, lnl%samplethickness, lnl%ps, lnl%Dy, &
            lnl%Dz, lnl%vs, lnl%absl, lnl%beamstopatf, lnl%intcutoffratio /)
dbllist(1) = 'Lw'
dbllist(2) = 'Lh'
dbllist(3) = 'Lx'
dbllist(4) = 'Ly'
dbllist(5) = 'Lz'
dbllist(6) = 'VoltageH'
dbllist(7) = 'VoltageL'
dbllist(8) = 'Sx'
dbllist(9) = 'sampletodetector'
dbllist(10) = 'samplethickness'
dbllist(11) = 'ps'
dbllist(12) = 'Dy'
dbllist(13) = 'Dz'
dbllist(14) = 'vs'
dbllist(15) = 'absl'
dbllist(16) = 'beamstopatf'
dbllist(17) = 'intcutoffratio'
call HDF_writeNMLdbles(HDF_head, io_dbl, dbllist, n_dbl)

! write all the strings
dataset = SC_xtalname
line2(1) = lnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create xtalname dataset',.TRUE.)

dataset = 'hdfname'
line2(1) = lnl%hdfname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create hdfname dataset',.TRUE.)

dataset = 'tiffprefix'
line2(1) = lnl%tiffprefix
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create tiffprefix dataset',.TRUE.)

dataset = 'orientationfile'
line2(1) = lnl%orientationfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create orientationfile dataset',.TRUE.)

dataset = 'projectionmode'
line2(1) = lnl%projectionmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLaueNameList: unable to create projectionmode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteLaueSlitNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteCPLMmasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param knl name list structure
!
!> @date 09/06/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteCPLMmasterNameList(HDF_head, knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteCPLMmasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(CPLMmasterNameListType),INTENT(IN)               :: knl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int), nm
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_CPLMmasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! convert normalize parameter to integer
nm = 0
if (knl%normalize.eqv..TRUE.) nm = 1

! write all the single integers
io_int = (/ knl%npx, knl%nthreads, nm /)
intlist(1) = 'npx'
intlist(2) = 'nthreads' 
intlist(3) = 'normalize'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ knl%eps1Re, knl%eps1Im, knl%eps2Re, knl%eps2Im, knl%wl, knl%theta /)
reallist(1) = 'eps1Re'
reallist(2) = 'eps1Im'
reallist(3) = 'eps2Re'
reallist(4) = 'eps2Im'
reallist(5) = 'wl' 
reallist(6) = 'theta' 
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
!dataset = SC_Notify
!line2(1) = knl%Notify
!hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMmasterNameList: unable to create Notify dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMmasterNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = knl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMmasterNameList: unable to create masterfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteCPLMmasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteCPLMNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist file into HDF file
!
!> @param HDF_head top of push stack
!> @param knl name list structure
!
!> @date 09/06/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteCPLMNameList(HDF_head, knl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteCPLMNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(CPLMNameListType),INTENT(IN)                     :: knl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), nm
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset,groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_CPLMNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ knl%phinum, knl%numpx, knl%numpy /)
intlist(1) = 'phinum'
intlist(2) = 'numpx'
intlist(3) = 'numpy'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the strings
dataset = SC_masterfile
line2(1) = knl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_outputfile
line2(1) = knl%outputfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMNameList: unable to create outputfile dataset',.TRUE.)

dataset = SC_anglefile
line2(1) = knl%anglefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMNameList: unable to create anglefile dataset',.TRUE.)

dataset = SC_tiffprefix
line2(1) = knl%tiffprefix
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCPLMNameList: unable to create tiffprefix dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteCPLMNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(MCNameListType),INTENT(INOUT)                    :: mcnl
!f2py intent(in,out) ::  mcnl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 7
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, sval(1), groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist   [we're writing this to the same group name as the MCOpenCL program
! to make sure that the EMEBSDmaster program can read the data ...]
groupname = SC_MCCLNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%primeseed, mcnl%num_el, mcnl%nthreads, mcnl%totnum_el /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'primeseed'
intlist(4) = 'num_el'
intlist(5) = 'nthreads'
intlist(6) = 'totnum_el'
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
dataset = SC_MCmode
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create MCmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_dataname
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create dataname dataset',.TRUE.)

dataset = SC_mode
sval(1) = mcnl%mode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCNameList: unable to create mode dataset',.TRUE.)

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(MCLIPSSNameListType),INTENT(INOUT)                    :: mcnl
!f2py intent(in,out) ::  mcnl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 10
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, sval(1), groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_MCLIPSSNameList
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
dataset = SC_MCmode
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCLIPSSNameList: unable to create MCmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCLIPSSNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_dataname
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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(MCCLNameListType),INTENT(INOUT)                  :: mcnl
!f2py intent(in,out) ::  mcnl

integer(kind=irg),parameter                           :: n_int = 11, n_real_bse1 = 9, n_real_full = 7, n_real_ivol= 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real_bse1(n_real_bse1), io_real_full(n_real_full), &
                                                         io_real_ivol(n_real_ivol)
character(20)                                         :: reallist_bse1(n_real_bse1), reallist_full(n_real_full), &
                                                         reallist_ivol(n_real_ivol)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, sval(1),groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_MCCLNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ mcnl%stdout, mcnl%numsx, mcnl%globalworkgrpsz, mcnl%num_el, mcnl%totnum_el, mcnl%multiplier, mcnl%devid, &
            mcnl%platid, mcnl%ivolx, mcnl%ivoly, mcnl%ivolz /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'globalworkgrpsz'
intlist(4) = 'num_el'
intlist(5) = 'totnum_el'
intlist(6) = 'multiplier'
intlist(7) = 'devid'
intlist(8) = 'platid'
intlist(9) = 'ivolx'
intlist(10) = 'ivoly'
intlist(11) = 'ivolz'
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
else if (mcnl%mode .eq. 'Ivol') then
   io_real_ivol = (/ mcnl%sig, mcnl%omega, mcnl%EkeV, dble(mcnl%ivolstepx), dble(mcnl%ivolstepy), dble(mcnl%ivolstepz) /)
   reallist_ivol(1) = 'sig'
   reallist_ivol(2) = 'omega'
   reallist_ivol(3) = 'EkeV'
   reallist_ivol(4) = 'ivolstepx'
   reallist_ivol(5) = 'ivolstepy'
   reallist_ivol(6) = 'ivolstepz'
   call HDF_writeNMLdbles(HDF_head, io_real_ivol, reallist_ivol, n_real_ivol)
end if

! write all the strings
dataset = SC_MCmode
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create MCmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_dataname
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create dataname dataset',.TRUE.)

dataset = SC_mode
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(MCCLMultiLayerNameListType),INTENT(INOUT)        :: mcnl
!f2py intent(in,out) ::  mcnl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 9
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_MCCLMultiLayerNameList
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
dataset = SC_MCmode
line2(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create MCmode dataset',.TRUE.)

dataset = SC_xtalnamefilm
line2(1) = mcnl%xtalname_film
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create xtalname_film dataset',.TRUE.)

dataset = SC_xtalnamesubs
line2(1) = mcnl%xtalname_subs
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create xtalname_subs dataset',.TRUE.)

dataset = SC_dataname
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLMultiLayerNameList: unable to create dataname dataset',.TRUE.)

dataset = SC_mode
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDMasterNameListType),INTENT(INOUT)            :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 8, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites, &
                                                         useEnergyWeighting
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EBSDMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
if (emnl%combinesites) then 
  combinesites = 1
else 
  combinesites = 0
end if
if (emnl%useEnergyWeighting) then 
  useEnergyWeighting = 1
else 
  useEnergyWeighting = 0
end if
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
io_int = (/ emnl%stdout, emnl%npx, emnl%Esel, emnl%nthreads, combinesites, restart, uniform, useEnergyWeighting /)
intlist(1) = 'stdout'
intlist(2) = 'npx'
intlist(3) = 'Esel'
intlist(4) = 'nthreads'
intlist(5) = 'combinesites'
intlist(6) = 'restart'
intlist(7) = 'uniform'
intlist(8) = 'useEnergyWeighting'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write a single real
dataset = SC_dmin
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = SC_latgridtype
line2(1) = 'Lambert'
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create latgridtype dataset',.TRUE.)

dataset = SC_copyfromenergyfile
line2(1) = emnl%copyfromenergyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create copyfromenergyfile dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create energyfile dataset',.TRUE.)

dataset = 'BetheParametersFile'
line2(1) = emnl%BetheParametersFile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create BetheParametersFile dataset',.TRUE.)


! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteISEMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl ISE master name list structure
!
!> @date 12/18/20  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteISEMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteISEMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ISEMasterNameListType),INTENT(INOUT)             :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites, &
                                                         useEnergyWeighting
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_ISEMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the integers
io_int = (/ emnl%npx, emnl%nthreads /)
intlist(1) = 'npx'
intlist(2) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! reals 
io_real = (/ emnl%iscale(1), emnl%iscale(2), emnl%iscale(3) /)
reallist(1) = 'a'
reallist(2) = 'b'
reallist(3) = 'c'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

dataset = SC_Notify
line2(1) = emnl%Notify
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteISEMasterNameList: unable to create latgridtype dataset',.TRUE.)

dataset = SC_outname
line2(1) = emnl%outname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteISEMasterNameList: unable to create outname dataset',.TRUE.)

dataset = SC_tiffname
line2(1) = emnl%tiffname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteISEMasterNameList: unable to create tiffname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = emnl%xtalname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteISEMasterNameList: unable to create the xtalname dataset',.TRUE.)


! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteISEMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEECMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EEC master name list structure
!
!> @date 12/13/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEECMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEECMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EECMasterNameListType),INTENT(INOUT)             :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int) 
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EECMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ emnl%npx, emnl%nthreads /)
intlist(1) = 'npx'
intlist(2) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single floats
io_real = (/ emnl%dmin, emnl%IsotopeEnergy, emnl%mfp /)
reallist(1) = 'dmin'
reallist(2) = 'IsotopeEnergy'
reallist(3) = 'mfp'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! vectors
dataset = 'IsotopeSite'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%IsotopeSite, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEECMasterNameList: unable to create IsotopeSite dataset',.TRUE.)

! strings
dataset = SC_mpfile
line2(1) = emnl%mpfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEECMasterNameList: unable to create mpfile dataset',.TRUE.)

dataset = 'BetheParametersFile'
line2(1) = emnl%BetheParametersFile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEECMasterNameList: unable to create BetheParametersFile dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = emnl%xtalname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEECMasterNameList: unable to create xtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEECMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDoverlapNameList
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
recursive subroutine HDFwriteEBSDoverlapNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDoverlapNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDoverlapNameListType),INTENT(INOUT)           :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EBSDoverlapNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ emnl%stdout, emnl%newpgnum /)
intlist(1) = 'stdout'
intlist(2) = 'newpgnum'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write a single real
dataset = 'fracB'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%fracB, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%fracB, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create fracB dataset',.TRUE.)

! write a single real
dataset = 'fracC'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%fracC, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%fracC, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create fracC dataset',.TRUE.)

! write a single real
dataset = 'fracD'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%fracD, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%fracD, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create fracD dataset',.TRUE.)


! vectors
dataset = 'tA'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%tA, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create tA dataset',.TRUE.)

dataset = 'tA2'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%tA2, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create tA2 dataset',.TRUE.)

dataset = 'tA3'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%tA3, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create tA3 dataset',.TRUE.)

dataset = 'tB'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%tB, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create tB dataset',.TRUE.)

dataset = 'tC'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%tC, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create tC dataset',.TRUE.)

dataset = 'tD'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%tD, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create tD dataset',.TRUE.)


dataset = 'gA'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%gA, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create gA dataset',.TRUE.)

dataset = 'gA2'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%gA2, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create gA2 dataset',.TRUE.)

dataset = 'gA3'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%gA3, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create gA3 dataset',.TRUE.)

dataset = 'gB'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%gB, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create gB dataset',.TRUE.)

dataset = 'gC'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%gC, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create gC dataset',.TRUE.)

dataset = 'gD'
hdferr = HDF_writeDatasetFloatArray1D(dataset, emnl%gD, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create gD dataset',.TRUE.)

dataset = 'PatternAxisA'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, emnl%PatternAxisA, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create PatternAxisA dataset',.TRUE.)

dataset = 'HorizontalAxisA'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, emnl%HorizontalAxisA, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create HorizontalAxisA dataset',.TRUE.)

dataset = 'masterfileA'
line2(1) = emnl%masterfileA
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create masterfileA dataset',.TRUE.)

dataset = 'masterfileB'
line2(1) = emnl%masterfileB
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create masterfileB dataset',.TRUE.)

dataset = 'masterfileC'
line2(1) = emnl%masterfileC
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create masterfileC dataset',.TRUE.)

dataset = 'masterfileD'
line2(1) = emnl%masterfileD
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create masterfileD dataset',.TRUE.)

dataset = 'datafile'
line2(1) = emnl%datafile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create datafile dataset',.TRUE.)


dataset = 'overlapmode'
line2(1) = emnl%overlapmode
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDoverlapNameList: unable to create overlapmode dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDoverlapNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDSingleMasterNameList
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
recursive subroutine HDFwriteEBSDSingleMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDSingleMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDSingleMasterNameListType),INTENT(INOUT)      :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int), combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EBSDSingleMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
if (emnl%combinesites) then 
  combinesites = 1
else 
  combinesites = 0
end if
io_int = (/ emnl%npx, emnl%nthreads, combinesites /)
intlist(1) = 'npx'
intlist(2) = 'nthreads'
intlist(3) = 'combinesites'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write two reals
dataset = SC_dmin
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDSingleMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = SC_kV
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%kV, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%kV, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDSingleMasterNameList: unable to create kV dataset',.TRUE.)

dataset = 'tstep'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%tstep, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%tstep, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDSingleMasterNameList: unable to create tstep dataset',.TRUE.)

! strings
dataset = SC_outname
line2(1) = emnl%outname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDSingleMasterNameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = emnl%xtalname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDSingleMasterNameList: unable to create xtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDSingleMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteRefineMartensiteNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EMRefineMartensite name list structure
!
!> @date 01/04/19 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteRefineMartensiteNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteRefineMartensiteNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(RefineMartensitetype),INTENT(INOUT)              :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = 'RefineMartensite'
hdferr = HDF_createGroup(groupname,HDF_head)

io_int = (/ emnl%numMartensite, emnl%nthreads /)
intlist(1) = 'numMartensite'
intlist(2) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single floats
io_real = (/emnl%step/)
reallist(1) = 'step'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

dataset = 'outputfile'
line2(1) = emnl%outputfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRefineMartensiteNameList: unable to create outputfile dataset',&
                                      .TRUE.)

dataset = 'martensiteMPprefix'
line2(1) = emnl%martensiteMPprefix
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRefineMartensiteNameList: unable to create martensiteMPprefix dataset',&
                                      .TRUE.)

dataset = 'martensiteMPpostfix'
line2(1) = emnl%martensiteMPpostfix
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRefineMartensiteNameList: unable to create martensiteMPpostfix dataset',&
                                      .TRUE.)

dataset = 'ferritedotproductfile'
line2(1) = emnl%ferritedotproductfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRefineMartensiteNameList: unable to create ferritedotproductfile dataset',&
                                      .TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteRefineMartensiteNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDQCMasterNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EBSDQC master name list structure
!
!> @date 03/21/15  SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSDQCMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDQCMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDQCMasterNameListType),INTENT(INOUT)          :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EBSDMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

io_int = (/emnl%npx, emnl%nsamples, emnl%nthreads /)
intlist(1) = 'npx'
intlist(2) = 'nsamples'
intlist(3) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single floats
io_real = (/emnl%dmin/)
reallist(1) = 'dmin'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

dataset = SC_energyfile
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDQCMasterNameList: unable to create energyfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDQCMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSD2DQCMasterNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param emnl EBSD2DQC master name list structure
!
!> @date 05/1/18  SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteEBSD2DQCMasterNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSD2DQCMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSD2DQCMasterNameListType),INTENT(INOUT)        :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 5
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EBSDMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

io_int = (/emnl%npx, emnl%nthreads, emnl%atno /)
intlist(1) = 'npx'
intlist(2) = 'nthreads'
intlist(3) = 'atno'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single floats
io_real = (/emnl%dmin_qc, emnl%dmin_p, emnl%QClatparm_a, emnl%QClatparm_c, emnl%DWF/)
reallist(1) = 'dmin_qc'
reallist(2) = 'dmin_p'
reallist(3) = 'QClatparm_a'
reallist(4) = 'QClatparm_c'
reallist(5) = 'DWF'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

dataset = SC_energyfile
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDQCMasterNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_centering
line2(1) = emnl%centering
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDQCMasterNameList: unable to create centering dataset',.TRUE.)

dataset = SC_QCtype
line2(1) = emnl%QCtype
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDQCMasterNameList: unable to create centering dataset',.TRUE.)
! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSD2DQCMasterNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(TKDMasterNameListType),INTENT(INOUT)            :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 7, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_TKDMasterNameList
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
if (emnl%combinesites) then 
  combinesites = 1
else 
  combinesites = 0
end if
io_int = (/ emnl%stdout, emnl%npx, emnl%Esel, emnl%nthreads, restart, uniform, combinesites /)
intlist(1) = 'stdout'
intlist(2) = 'npx'
intlist(3) = 'Esel'
intlist(4) = 'nthreads'
intlist(5) = 'restart'
intlist(6) = 'uniform'
intlist(7) = 'combinesites'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write a single real
dataset = SC_dmin
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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
! SUBROUTINE:HDFwriteTKDspotsNameList
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
recursive subroutine HDFwriteTKDspotsNameList(HDF_head, emnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteTKDspotsNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(TKDspotsNameListType),INTENT(INOUT)              :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 10
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform, combinesites
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_TKDspotsNML
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ emnl%ncubochoric, emnl%nthreads, emnl%numsx, emnl%numsy /)
intlist(1) = 'ncubochoric'
intlist(2) = 'nthreads'
intlist(3) = 'numsx'
intlist(4) = 'numsy'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single floats
io_real = (/emnl%voltage, emnl%dmin, emnl%thickness, emnl%L, emnl%thetac, emnl%delta, emnl%omega, emnl%xpc, emnl%ypc, emnl%sig/)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'thickness'
reallist(4) = 'L'
reallist(5) = 'thetac'
reallist(6) = 'delta'
reallist(7) = 'omega'
reallist(8) = 'xpc'
reallist(9) = 'ypc'
reallist(10) = 'sig'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

dataset = SC_outname
line2(1) = emnl%outname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDspotsNameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = emnl%xtalname
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDspotsNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_eulerfile
line2(1) = emnl%eulerfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDspotsNameList: unable to create outname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteTKDspotsNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDMasterOpenCLNameListType),INTENT(INOUT)      :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 9, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart, uniform
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)
logical                                               :: g_exists, overwrite=.TRUE.

! create the group for this namelist
groupname = SC_EBSDMasterOpenCLNameList
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
dataset = SC_dmin
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloat(dataset, emnl%dmin, HDF_head)
end if
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDMasterNameList: unable to create dmin dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = emnl%energyfile
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(BetheParameterType),INTENT(INOUT)                :: BetheParameters
!f2py intent(in,out) ::  BetheParameters

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
groupname = SC_BetheList
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDclusterNameListType),INTENT(INOUT)           :: emnl
!f2py intent(in,out) ::  emnl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int), restart
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_EBSDclusterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

io_int = (/ emnl%NClusters, emnl%NIterations, emnl%binfactor /)
intlist(1) = 'NClusters'
intlist(2) = 'NIterations'
intlist(3) = 'binfactor'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the strings
dataset = SC_inputfilename
line2(1) = emnl%inputfilename
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDclusterNameList: unable to create inputfilename dataset',.TRUE.)

dataset = SC_groupname
line2(1) = emnl%groupname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDclusterNameList: unable to create groupname dataset',.TRUE.)

dataset = SC_datasetname
line2(1) = emnl%datasetname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDclusterNameList: unable to create datasetname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDclusterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteECPQCMasterNameList
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
!> @date 01/04/18 MDG 1.2 added to Public repo
!--------------------------------------------------------------------------
recursive subroutine HDFwriteECPQCMasterNameList(HDF_head, ecpnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteECPQCMasterNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPQCMasterNameListType),INTENT(INOUT)           :: ecpnl
!f2py intent(in,out) ::  ecpnl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr, io_int(n_int), distort
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'ECPMasterNameList'
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%npx, ecpnl%nthreads, ecpnl%nsamples /)
intlist(1) = 'npx'
intlist(2) = 'nthreads'
intlist(3) = 'nsamples'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single doubles
io_real = (/ ecpnl%dmin /)
reallist(1) = 'dmin'
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

dataset = 'energyfile'
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPQCMasterNameList: unable to create energyfile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteECPQCMasterNameList




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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPMasterNameListType),INTENT(INOUT)             :: ecpnl
!f2py intent(in,out) ::  ecpnl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 1
integer(kind=irg)                                     :: hdferr, io_int(n_int), distort, combinesites
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_ECPMasterNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
! distort is a logical, for which there is no real HDF_NATIVE_anything conversion, so we'll store it as a 1 or 0
if (ecpnl%combinesites) then 
  combinesites = 1
else 
  combinesites = 0
end if

io_int = (/ ecpnl%stdout, ecpnl%Esel, ecpnl%npx, ecpnl%nthreads, combinesites /)
intlist(1) = 'stdout'
intlist(2) = 'Esel'
intlist(3) = 'npx'
intlist(4) = 'nthreads'
intlist(5) = 'combinesites'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)


! write all the single doubles
io_real = (/ ecpnl%dmin /)
reallist(1) = 'dmin'
call HDF_writeNMLdbles(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = SC_energyfile
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPMasterNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_compmode
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPZANameListType),INTENT(INOUT)                 :: ecpnl
!f2py intent(in,out) ::  ecpnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 2
integer(kind=irg)                                     :: hdferr, io_int(n_int), distort
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_ECPZANameList
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
dataset = SC_k
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create k dataset',.TRUE.)

dataset = SC_fn
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%fn, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create fn dataset',.TRUE.)

! write all the strings
dataset = SC_outname
line2(1) = ecpnl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create outname dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPZANameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_maskpattern
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDNameListType),INTENT(INOUT)                  :: enl
!f2py intent(in,out) ::  enl

integer(kind=irg),parameter                           :: n_int = 8, n_real = 10
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: t(1)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = SC_EBSDNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ enl%stdout, enl%numsx, enl%numsy, enl%binning, enl%nthreads, enl%energyaverage, enl%nregions, enl%maskradius /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'numsy'
intlist(4) = 'binning'
intlist(5) = 'nthreads'
intlist(6) = 'energyaverage'
intlist(7) = 'nregions'
intlist(8) = 'maskradius'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals 
io_real = (/ enl%L, enl%thetac, enl%delta, enl%xpc, enl%ypc, enl%energymin, enl%energymax, enl%gammavalue, &
             enl%alphaBD, enl%hipassw /)
reallist(1) = 'L'
reallist(2) = 'thetac'
reallist(3) = 'delta'
reallist(4) = 'xpc'
reallist(5) = 'ypc'
reallist(6) = 'energymin'
reallist(7) = 'energymax'
reallist(8) = 'gammavalue'
reallist(9) = 'alphaBD'
reallist(10)= 'hipassw'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! a 4-vector
dataset = SC_axisangle
hdferr = HDF_writeDatasetFloatArray1D(dataset, enl%axisangle, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create axisangle dataset',.TRUE.)

! a 3x3 matrix 
dataset = SC_Ftensor
hdferr = HDF_writeDatasetDoubleArray2D(dataset, enl%Ftensor, 3, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create Ftensor dataset',.TRUE.)

! a few doubles
dataset = SC_beamcurrent
hdferr = HDF_writeDatasetDouble(dataset, enl%beamcurrent, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create beamcurrent dataset',.TRUE.)

dataset = SC_dwelltime
hdferr = HDF_writeDatasetDouble(dataset, enl%dwelltime, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create dwelltime dataset',.TRUE.)

! write all the strings
dataset = SC_maskpattern
line2(1) = trim(enl%maskpattern)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create maskpattern dataset',.TRUE.)

dataset = SC_makedictionary
line2(1) = trim(enl%makedictionary)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create makedictionary dataset',.TRUE.)

dataset = SC_poisson
line2(1) = trim(enl%poisson)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create poisson dataset',.TRUE.)

dataset = SC_includebackground
line2(1) = trim(enl%includebackground)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create includebackground dataset',.TRUE.)

dataset = SC_applyDeformation
line2(1) = trim(enl%applyDeformation)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create applyDeformation dataset',.TRUE.)

dataset = SC_scalingmode
line2(1) = trim(enl%scalingmode)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create scalingmode dataset',.TRUE.)

dataset = SC_eulerconvention
line2(1) = trim(enl%eulerconvention)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create eulerconvention dataset',.TRUE.)

dataset = SC_outputformat
line2(1) = trim(enl%outputformat)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create outputformat dataset',.TRUE.)

dataset = SC_bitdepth
line2(1) = trim(enl%bitdepth)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create bitdepth dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = trim(enl%energyfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = trim(enl%masterfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_anglefile
line2(1) = trim(enl%anglefile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create anglefile dataset',.TRUE.)

dataset = SC_anglefiletype
line2(1) = trim(enl%anglefiletype)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create anglefiletype dataset',.TRUE.)

dataset = SC_datafile
line2(1) = trim(enl%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDNameList: unable to create datafile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteEBSDdefectNameList
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
recursive subroutine HDFwriteEBSDdefectNameList(HDF_head, enl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteEBSDdefectNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDdefectNameListType),INTENT(INOUT)            :: enl
!f2py intent(in,out) ::  enl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 4
integer(kind=irg)                                     :: hdferr, sampleIV, io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: t(1)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = SC_EBSDdefectNameList
hdferr = HDF_createGroup(groupname,HDF_head)

sampleIV = 0
if (enl%sampleInteractionVolume.eqv..TRUE.) sampleIV = 1

! write all the single integers
io_int = (/ enl%stdout, enl%numsx, enl%numsy, enl%nthreads, sampleIV /)
intlist(1) = 'stdout'
intlist(2) = 'numsx'
intlist(3) = 'numsy'
intlist(4) = 'nthreads'
intlist(5) = 'sampleInteractionVolume'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals 
io_real = (/ enl%thetac, enl%delta, enl%gammavalue, enl%spotsize /)
reallist(1) = 'thetac'
reallist(2) = 'delta'
reallist(3) = 'gammavalue'
reallist(4) = 'spotsize'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! a few doubles
dataset = SC_beamcurrent
hdferr = HDF_writeDatasetDouble(dataset, enl%beamcurrent, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create beamcurrent dataset',.TRUE.)

dataset = SC_dwelltime
hdferr = HDF_writeDatasetDouble(dataset, enl%dwelltime, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create dwelltime dataset',.TRUE.)

! write all the strings
dataset = SC_scalingmode
line2(1) = trim(enl%scalingmode)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create scalingmode dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = trim(enl%masterfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_deformationfile
line2(1) = trim(enl%deformationfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create deformationfile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = trim(enl%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create datafile dataset',.TRUE.)

dataset = 'ivolfile'
line2(1) = trim(enl%ivolfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create ivolfile dataset',.TRUE.)

dataset = 'tmpfspath'
line2(1) = trim(enl%tmpfspath)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDdefectNameList: unable to create tmpfspath dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDdefectNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteTKDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param enl TKD name list structure
!
!> @date 03/22/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteTKDNameList(HDF_head, enl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteTKDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(TKDNameListType),INTENT(INOUT)                   :: enl
!f2py intent(in,out) ::  enl

integer(kind=irg),parameter                           :: n_int = 6, n_real = 9
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: t(1)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = SC_TKDNameList
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
dataset = SC_axisangle
hdferr = HDF_writeDatasetFloatArray1D(dataset, enl%axisangle, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create axisangle dataset',.TRUE.)

! a few doubles
dataset = SC_beamcurrent
hdferr = HDF_writeDatasetDouble(dataset, enl%beamcurrent, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create beamcurrent dataset',.TRUE.)

dataset = SC_dwelltime
hdferr = HDF_writeDatasetDouble(dataset, enl%dwelltime, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create dwelltime dataset',.TRUE.)

! write all the strings
dataset = SC_maskpattern
line2(1) = trim(enl%maskpattern)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create maskpattern dataset',.TRUE.)

dataset = SC_scalingmode
line2(1) = trim(enl%scalingmode)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create scalingmode dataset',.TRUE.)

dataset = SC_eulerconvention
line2(1) = trim(enl%eulerconvention)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create eulerconvention dataset',.TRUE.)

dataset = SC_outputformat
line2(1) = trim(enl%outputformat)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create outputformat dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = trim(enl%energyfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = trim(enl%masterfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_anglefile
line2(1) = trim(enl%anglefile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create anglefile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = trim(enl%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDNameList: unable to create datafile dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteTKDNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPNameListType),INTENT(INOUT)                   :: ecpnl
!f2py intent(in,out) ::  ecpnl
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
groupname = SC_ECPNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%nthreads, ecpnl%npix /)
intlist(1) = 'nthreads'
intlist(2) = 'npix'

call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)
if (twolayerflag) then
! integer vectors
dataset = SC_fnf
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%fn_f, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create k dataset',.TRUE.)

dataset = SC_fns
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%fn_s, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create fn dataset',.TRUE.)

dataset = SC_gF
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%gF, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create gF dataset',.TRUE.)

dataset = SC_gS
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%gS, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create gS dataset',.TRUE.)

dataset = SC_tF
    hdferr = HDF_writeDatasetIntegerArray1D(dataset, ecpnl%tF, 3, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create tF dataset',.TRUE.)

dataset = SC_tS
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

dataset = SC_energyfile
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = ecpnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create datafile dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = ecpnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname dataset',.TRUE.)

if (twolayerflag) then

dataset = SC_xtalname2
    line2(1) = ecpnl%xtalname2
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname2 dataset',.TRUE.)

end if

dataset = SC_masterfile
line2(1) = ecpnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create masterfile dataset',.TRUE.)

if (twolayerflag) then

dataset = SC_filmfile
    line2(1) = ecpnl%filmfile
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create filmfile dataset',.TRUE.)

dataset = SC_subsfile
    line2(1) = ecpnl%subsfile
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create subsfile dataset',.TRUE.)

end if

dataset = SC_maskpattern
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create mask dataset',.TRUE.)

dataset = SC_anglefile
line2(1) = ecpnl%anglefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create anglefile dataset',.TRUE.)

dataset = SC_outputformat
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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPSingleNameListType),INTENT(INOUT)             :: ecpnl
!f2py intent(in,out) ::  ecpnl

integer(kind=irg),parameter                           :: n_int = 2
integer(kind=irg)                                     :: n_real
integer(kind=irg)                                     :: hdferr,  io_int(n_int), istat
real(kind=sgl),allocatable                            :: io_real(:)
character(20)                                         :: intlist(n_int)
character(20),allocatable                             :: reallist(:)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_ECPNameList
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

dataset = SC_energyfile
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = ecpnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create datafile dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = ecpnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_maskpattern
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create mask dataset',.TRUE.)

dataset = SC_eulerconvention
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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPIndexingNameListType),INTENT(INOUT)           :: ecpnl
!f2py intent(in,out) ::  ecpnl

integer(kind=irg),parameter                           :: n_int = 11
integer(kind=irg),parameter                           :: n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int), istat
real(kind=sgl),allocatable                            :: io_real(:)
character(20)                                         :: intlist(n_int)
character(20),allocatable                             :: reallist(:)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_ECPIndexingNameListType
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%ncubochoric, ecpnl%totnumexpt, ecpnl%nnk, ecpnl%npix, ecpnl%maskradius, ecpnl%nregions,&
          ecpnl%numdictsingle, ecpnl%numexptsingle,ecpnl%nthreads, ecpnl%platid, ecpnl%devid /)
intlist(1) = 'Ncubochoric'
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

dataset = SC_energyfile
line2(1) = ecpnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_exptfile
line2(1) = ecpnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = ecpnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create datafile dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = ecpnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_maskpattern
line2(1) = ecpnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create mask dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = ecpnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_tmpfile
line2(1) = ecpnl%tmpfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPNameList: unable to create tmpfile dataset',.TRUE.)

dataset = SC_ctffile
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(LACBEDNameListType),INTENT(INOUT)                :: lacbednl
!f2py intent(in,out) ::  lacbednl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_LACBEDNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ lacbednl%maxHOLZ, lacbednl%numthick, lacbednl%npix, lacbednl%nthreads /)
intlist(1) = 'maxHOLZ'
intlist(2) = 'numthick'
intlist(3) = 'npix'
intlist(4) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! vectors
dataset = SC_k
hdferr = HDF_writeDatasetIntegerArray1D(dataset, lacbednl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create k dataset',.TRUE.)

dataset = SC_fn
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
dataset = SC_outname
line2(1) = lacbednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = lacbednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteLACBEDNameList: unable to create xtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteLACBEDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteCBEDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param cbednl CBED name list structure
!
!> @date 11/24/18 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteCBEDNameList(HDF_head, cbednl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteCBEDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(CBEDNameListType),INTENT(INOUT)                  :: cbednl
!f2py intent(in,out) ::  cbednl

integer(kind=irg),parameter                           :: n_int = 4, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_CBEDNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ cbednl%maxHOLZ, cbednl%numthick, cbednl%npix, cbednl%nthreads /)
intlist(1) = 'maxHOLZ'
intlist(2) = 'numthick'
intlist(3) = 'npix'
intlist(4) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! vectors
dataset = SC_k
hdferr = HDF_writeDatasetIntegerArray1D(dataset, cbednl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create k dataset',.TRUE.)

dataset = SC_fn
hdferr = HDF_writeDatasetIntegerArray1D(dataset, cbednl%fn, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create fn dataset',.TRUE.)

dataset = SC_klaue
hdferr = HDF_writeDatasetFloatArray1D(dataset, cbednl%klaue, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create klaue dataset',.TRUE.)

! write all the single reals
io_real = (/ cbednl%voltage, cbednl%dmin, cbednl%convergence, cbednl%startthick, cbednl%thickinc, cbednl%camlen /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'convergence'
reallist(4) = 'startthick'
reallist(5) = 'thickinc'
reallist(6) = 'camlen'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = SC_outname
line2(1) = cbednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = cbednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create xtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteCBEDNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECPpatternNameListType),INTENT(INOUT)            :: ecpnl
!f2py intent(in,out) ::  ecpnl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_ECPpatternNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ecpnl%stdout, ecpnl%npix /)
intlist(1) = 'stdout'
intlist(2) = 'npix'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! single real
dataset = SC_thetac
hdferr = HDF_writeDatasetFloat(dataset, ecpnl%thetac, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create thetac dataset',.TRUE.)

! real vector
dataset = SC_k
hdferr = HDF_writeDatasetFloatArray1D(dataset, ecpnl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create k dataset',.TRUE.)

! write all the strings
dataset = SC_outname
line2(1) = ecpnl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECPpatternNameList: unable to create outname dataset',.TRUE.)

dataset = SC_masterfile
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(PEDkinNameListType),INTENT(INOUT)                :: pednl
!f2py intent(in,out) ::  pednl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 4
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_PEDkinNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ pednl%npix, pednl%ncubochoric, pednl%nthreads /)
intlist(1) = 'npix'
intlist(2) = 'Ncubochoric'
intlist(3) = 'nthreads'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real = (/ pednl%voltage, pednl%thickness, pednl%dmin, pednl%rnmpp /)
reallist(1) = 'voltage'
reallist(2) = 'thickness'
reallist(3) = 'dmin'
reallist(4) = 'rnmpp'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = SC_outname
line2(1) = pednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDkinNameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = pednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDkinNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_eulerfile
line2(1) = pednl%eulerfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDkinNameList: unable to create eulerfile dataset',.TRUE.)

dataset = SC_sampling
line2(1) = pednl%sampling
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDkinNameList: unable to create sampling dataset',.TRUE.)

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(kinematicalNameListType),INTENT(INOUT)                :: knl
!f2py intent(in,out) ::  knl

integer(kind=irg),parameter                           :: n_int = 0, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_EMkinematicalNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single reals
io_real = (/ knl%voltage, knl%dmin, knl%thr /)
reallist(1) = 'voltage'
reallist(2) = 'dmin'
reallist(3) = 'thr'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! write all the strings
dataset = SC_xtalname
line2(1) = knl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritekinematicalNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_datafile
line2(1) = knl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritekinematicalNameList: unable to create datafile dataset',.TRUE.)

dataset = 'mode'
line2(1) = knl%mode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritekinematicalNameList: unable to create mode dataset',.TRUE.)

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(PEDZANameListType),INTENT(INOUT)                   :: pednl
!f2py intent(in,out) ::  pednl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr, io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_PEDZANameList
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
dataset = SC_k
hdferr = HDF_writeDatasetIntegerArray1D(dataset, pednl%k, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create k dataset',.TRUE.)

dataset = SC_fn
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
dataset = SC_outname
line2(1) = pednl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = pednl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePEDZANameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_filemode
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(ECCINameListType),INTENT(INOUT)                  :: eccinl
!f2py intent(in,out) ::  eccinl

integer(kind=irg),parameter                           :: n_int = 5, n_real = 6
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_ECCINameList
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
dataset = SC_k
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
dataset = SC_lauec
hdferr = HDF_writeDatasetFloatArray1D(dataset, eccinl%lauec, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create lauec dataset',.TRUE.)

dataset = SC_lauec2
hdferr = HDF_writeDatasetFloatArray1D(dataset, eccinl%lauec2, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create lauec2 dataset',.TRUE.)

! write all the strings
dataset = SC_dispmode
line2(1) = eccinl%dispmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create dispmode dataset',.TRUE.)

dataset = SC_summode
line2(1) = eccinl%summode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create summode dataset',.TRUE.)

dataset = SC_progmode
line2(1) = eccinl%progmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create progmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = eccinl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_montagename
line2(1) = eccinl%montagename
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create montagename dataset',.TRUE.)

dataset = SC_defectfilename
line2(1) = eccinl%defectfilename
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create defectfilename dataset',.TRUE.)

dataset = SC_dispfile
line2(1) = eccinl%dispfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create dispfile dataset',.TRUE.)

dataset = SC_dataname
line2(1) = eccinl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create dataname dataset',.TRUE.)

dataset = SC_ECPname
line2(1) = eccinl%ECPname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteECCINameList: unable to create ECPname dataset',.TRUE.)

dataset = SC_sgname
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(RFZNameListType),INTENT(INOUT)                   :: rfznl
!f2py intent(in,out) ::  rfznl

integer(kind=irg),parameter                           :: n_int = 3, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_RFZNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ rfznl%pgnum, rfznl%nsteps, rfznl%gridtype /)
intlist(1) = 'pgnum'
intlist(2) = 'nsteps'
intlist(3) = 'gridtype'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! strings
dataset = SC_euoutname
line2(1) = rfznl%euoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create euler outname dataset',.TRUE.)

dataset = SC_cuoutname
line2(1) = rfznl%cuoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create cubochoric outname dataset',.TRUE.)

dataset = SC_hooutname
line2(1) = rfznl%hooutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create homochoric outname dataset',.TRUE.)

dataset = SC_rooutname
line2(1) = rfznl%rooutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create rodrigues outname dataset',.TRUE.)

dataset = SC_quoutname
line2(1) = rfznl%quoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create quaternion outname dataset',.TRUE.)

dataset = SC_omoutname
line2(1) = rfznl%omoutname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteRFZNameList: unable to create orientation matrix outname dataset',.TRUE.)

dataset = SC_axoutname
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(DictIndxOpenCLListType),INTENT(INOUT)            :: dictindxnl
!f2py intent(in,out) ::  dictindxnl

integer(kind=irg),parameter                           :: n_int = 8, n_real = 1
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_DictIndxOpenCLNameList
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
dataset = SC_exptfile
line2(1) = dictindxnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteDictIndxOpenCLNameList: unable to create exptfile dataset',.TRUE.)

dataset = SC_dictfile
line2(1) = dictindxnl%dictfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteDictIndxOpenCLNameList: unable to create dictfile dataset',.TRUE.)

dataset = SC_eulerfile
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

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EBSDIndexingNameListType),INTENT(INOUT)          :: ebsdnl
!f2py intent(in,out) ::  ebsdnl

integer(kind=irg),parameter                           :: n_int = 22, n_real = 12, n_reald = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: io_reald(n_reald)
character(20)                                         :: intlist(n_int), reallist(n_real), realdlist(n_reald)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1), line10(10)

! create the group for this namelist
groupname = SC_EBSDIndexingNameListType
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ ebsdnl%ncubochoric, ebsdnl%numexptsingle, ebsdnl%numdictsingle, ebsdnl%ipf_ht, &
            ebsdnl%ipf_wd, ebsdnl%nnk, ebsdnl%maskradius, ebsdnl%numsx, ebsdnl%numsy, ebsdnl%binning, &
            ebsdnl%nthreads, ebsdnl%energyaverage, ebsdnl%devid, ebsdnl%platid, ebsdnl%nregions, ebsdnl%nnav, &
            ebsdnl%nosm, ebsdnl%nlines, ebsdnl%usenumd, ebsdnl%nism, ebsdnl%exptnumsx, ebsdnl%exptnumsy /)
intlist(1) = 'Ncubochoric'
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
intlist(18) = 'nlines'
intlist(19) = 'usenumd'
intlist(20) = 'nism'
intlist(21) = 'exptnumsx'
intlist(22) = 'exptnumsy'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

io_real = (/ ebsdnl%L, ebsdnl%thetac, ebsdnl%delta, ebsdnl%omega, ebsdnl%xpc, &
             ebsdnl%ypc, ebsdnl%energymin, ebsdnl%energymax, ebsdnl%gammavalue, ebsdnl%StepX, &
             ebsdnl%StepY, ebsdnl%isangle /)
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
reallist(12) = 'isangle'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

io_reald = (/ ebsdnl%beamcurrent, ebsdnl%dwelltime, ebsdnl%hipassw /)
realdlist(1) = 'beamcurrent'
realdlist(2) = 'dwelltime'
realdlist(3) = 'hipassw'
call HDF_writeNMLdbles(HDF_head, io_reald, realdlist, n_reald)

! a 4-vector
dataset = SC_axisangle
hdferr = HDF_writeDatasetFloatArray1D(dataset, ebsdnl%axisangle, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create axisangle dataset',.TRUE.)

! an integer 4-vector
dataset = SC_ROI
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ebsdnl%ROI, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create ROI dataset',.TRUE.)

! an integer 8-vector
dataset = 'multidevid'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, ebsdnl%multidevid, 8, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create multidevid dataset',.TRUE.)

! strings
dataset = SC_maskpattern
line2(1) = ebsdnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create maskpattern dataset',.TRUE.)

dataset = SC_scalingmode
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

dataset = SC_spatialaverage
line2(1) = ebsdnl%spatialaverage
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create spatialaverage dataset', &
                                      .TRUE.)

dataset = SC_exptfile
line2(1) = ebsdnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create exptfile dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = ebsdnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = ebsdnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = ebsdnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create datafile dataset',.TRUE.)

dataset = SC_tmpfile
line2(1) = ebsdnl%tmpfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create tmpfile dataset',.TRUE.)

dataset = SC_ctffile
line2(1) = ebsdnl%ctffile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create ctffile dataset',.TRUE.)

dataset = SC_angfile
line2(1) = ebsdnl%angfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create angfile dataset',.TRUE.)

dataset = SC_anglefile
line2(1) = ebsdnl%anglefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create anglefile dataset',.TRUE.)

dataset = SC_eulerfile
line2(1) = ebsdnl%eulerfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create eulerfile dataset',.TRUE.)

dataset = SC_maskfile
line2(1) = ebsdnl%maskfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create maskfile dataset',.TRUE.)

dataset = SC_inputtype
line2(1) = ebsdnl%inputtype
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create inputtype dataset',.TRUE.)

dataset = SC_HDFstrings
line10 = ebsdnl%HDFstrings
hdferr = HDF_writeDatasetStringArray(dataset, line10, 10, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteEBSDDictionaryIndexingNameList: unable to create HDFstrings dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteEBSDDictionaryIndexingNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteTKDDictionaryIndexingNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param rfznl RFZ name list structure
!
!> @date 05/07/17 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteTKDDictionaryIndexingNameList(HDF_head,tkdnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteTKDDictionaryIndexingNameList

use ISO_C_BINDING

use local

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(TKDIndexingNameListType),INTENT(INOUT)           :: tkdnl
!f2py intent(in,out) ::  tkdnl

integer(kind=irg),parameter                           :: n_int = 17, n_real = 11, n_reald = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
real(kind=dbl)                                        :: io_reald(n_reald)
character(20)                                         :: intlist(n_int), reallist(n_real), realdlist(n_reald)
integer(kind=irg)                                     :: i
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1), line10(10)

! create the group for this namelist
groupname = SC_TKDIndexingNameListType
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ tkdnl%ncubochoric, tkdnl%numexptsingle, tkdnl%numdictsingle, tkdnl%ipf_ht, &
            tkdnl%ipf_wd, tkdnl%nnk, tkdnl%maskradius, tkdnl%numsx, tkdnl%numsy, tkdnl%binning, &
            tkdnl%nthreads, tkdnl%energyaverage, tkdnl%devid, tkdnl%platid, tkdnl%nregions, tkdnl%nnav, &
            tkdnl%nosm /)
intlist(1) = 'Ncubochoric'
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

io_real = (/ tkdnl%L, tkdnl%thetac, tkdnl%delta, tkdnl%omega, tkdnl%xpc, &
             tkdnl%ypc, tkdnl%energymin, tkdnl%energymax, tkdnl%gammavalue, tkdnl%StepX, tkdnl%StepY /)
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

io_reald = (/ tkdnl%beamcurrent, tkdnl%dwelltime, tkdnl%hipassw /)
realdlist(1) = 'beamcurrent'
realdlist(2) = 'dwelltime'
realdlist(3) = 'hipassw'
call HDF_writeNMLdbles(HDF_head, io_reald, realdlist, n_reald)

! a 4-vector
dataset = SC_axisangle
hdferr = HDF_writeDatasetFloatArray1D(dataset, tkdnl%axisangle, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create axisangle dataset',.TRUE.)

dataset = SC_ROI
hdferr = HDF_writeDatasetIntegerArray1D(dataset, tkdnl%ROI, 4, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create ROI dataset',.TRUE.)


! strings
dataset = SC_maskpattern
line2(1) = tkdnl%maskpattern
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create maskpattern dataset',.TRUE.)

dataset = SC_scalingmode
line2(1) = tkdnl%scalingmode
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create scalingmode dataset',.TRUE.)

!dataset = 'eulerconvention'
!line2(1) = tkdnl%eulerconvention
!hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create eulerconvention dataset', &
!                                      .TRUE.)

!dataset = 'outputformat'
!line2(1) = tkdnl%outputformat
!hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create outputformat dataset', &
!                                      .TRUE.)

dataset = SC_spatialaverage
line2(1) = tkdnl%spatialaverage
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create spatialaverage dataset', &
                                      .TRUE.)

dataset = SC_maskfile
line2(1) = tkdnl%maskfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create maskfile dataset',.TRUE.)

dataset = SC_exptfile
line2(1) = tkdnl%exptfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create exptfile dataset',.TRUE.)

dataset = SC_masterfile
line2(1) = tkdnl%masterfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create masterfile dataset',.TRUE.)

dataset = SC_energyfile
line2(1) = tkdnl%energyfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create energyfile dataset',.TRUE.)

dataset = SC_datafile
line2(1) = tkdnl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create datafile dataset',.TRUE.)

dataset = SC_tmpfile
line2(1) = tkdnl%tmpfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create tmpfile dataset',.TRUE.)

dataset = SC_ctffile
line2(1) = tkdnl%ctffile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create ctffile dataset',.TRUE.)

dataset = SC_angfile
line2(1) = tkdnl%angfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create angfile dataset',.TRUE.)

dataset = SC_anglefile
line2(1) = tkdnl%anglefile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create anglefile dataset',.TRUE.)

dataset = SC_eulerfile
line2(1) = tkdnl%eulerfile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create eulerfile dataset',.TRUE.)

dataset = SC_inputtype
line2(1) = tkdnl%inputtype
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create inputtype dataset',.TRUE.)

dataset = SC_HDFstrings
line10 = tkdnl%HDFstrings
hdferr = HDF_writeDatasetStringArray(dataset, line10, 10, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTKDDictionaryIndexingNameList: unable to create HDFstrings dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteTKDDictionaryIndexingNameList

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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(MCCLNameListType),INTENT(INOUT)                  :: mcnl
!f2py intent(in,out) ::  mcnl

integer(kind=irg),parameter                           :: n_int = 8, n_real_bse1 = 10, n_real_full = 8
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real_bse1(n_real_bse1), io_real_full(n_real_full)
character(20)                                         :: reallist_bse1(n_real_bse1), reallist_full(n_real_full)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, sval(1),groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_MCCLNameList
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
dataset = SC_MCmode
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create MCmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_dataname
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create dataname dataset',.TRUE.)

dataset = SC_mode
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

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(MCCLNameListType),INTENT(INOUT)                  :: mcnl
!f2py intent(in,out) ::  mcnl

integer(kind=irg),parameter                           :: n_int = 8, n_real = 8
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=dbl)                                        :: io_real(n_real)
character(20)                                         :: reallist(n_real)
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, sval(1),groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_MCCLfoilNameList
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
dataset = SC_MCmode
sval(1) = mcnl%MCmode
hdferr = HDF_writeDatasetStringArray(dataset, sval, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create MCmode dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = mcnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_dataname
line2(1) = mcnl%dataname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteMCCLNameList: unable to create dataname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteMCCLfoilNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwritePFInversionNameList
!
!> @author saransh singh, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param epf PF name list structure
!
!> @date 04/02/17 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwritePFInversionNameList(HDF_head,epf)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwritePFInversionNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(PFInversionNameListType),INTENT(IN)              :: epf


integer(kind=irg),parameter                           :: n_int = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int), ii
character(20)                                         :: intlist(n_int)
character(fnlen)                                      :: dataset, groupname, str
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_PFInversionNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ epf%nLam, epf%ncub, epf%nfiles/)
intlist(1) = 'nLam'
intlist(2) = 'ncub'
intlist(3) = 'nfiles'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the strings
dataset = SC_xtalname
line2(1) = trim(epf%xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePFInversionNameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_datafile
line2(1) = trim(epf%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePFInversionNameList: unable to create datafile dataset',.TRUE.)

dataset = SC_mrcfile
line2(1) = trim(epf%mrcfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePFInversionNameList: unable to create mrcfile dataset',.TRUE.)


do ii = 1,epf%nfiles
    write(str,'(A,I1)')'PFfile',ii
    dataset = str
    line2(1) = trim(epf%flist(ii))
    hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwritePFInversionNameList: unable to create PFfile dataset',.TRUE.)
end do
    
! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwritePFInversionNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteSTEMDCINameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param epf STEMDCI name list structure
!
!> @date 07/02/17 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteSTEMDCINameList(HDF_head,epf)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteSTEMDCINameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(STEMDCINameListType),INTENT(IN)                  :: epf


integer(kind=irg),parameter                           :: n_int = 6, n_real = 4
integer(kind=irg)                                     :: hdferr,  io_int(n_int), ii
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname, str
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = SC_STEMDCINameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ epf%nthreads, epf%output, epf%dinfo, epf%t_interval, epf%DF_npix, epf%DF_npiy /)
intlist(1) = 'nthreads'
intlist(2) = 'output'
intlist(3) = 'dinfo'
intlist(4) = 't_interval'
intlist(5) = 'DF_npix'
intlist(6) = 'DF_npiy'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

io_real = (/ epf%voltage, epf%DF_L, epf%DF_slice, epf%dmin /)
reallist(1) = 'voltage'
reallist(2) = 'DF_L'
reallist(3) = 'DF_slice'
reallist(4) = 'dmin'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! vectors
dataset = SC_kk
hdferr = HDF_writeDatasetIntegerArray1D(dataset, epf%kk, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create kk dataset',.TRUE.)

! a 2-vector
dataset = SC_lauec
hdferr = HDF_writeDatasetFloatArray1D(dataset, epf%lauec, 2, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create lauec dataset',.TRUE.)

! short strings
dataset = SC_progmode
line2(1) = trim(epf%progmode)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create progmode dataset',.TRUE.)

dataset = SC_dispmode
line2(1) = trim(epf%dispmode)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create dispmode dataset',.TRUE.)

! write all the full length strings
dataset = SC_xtalname
line2(1) = trim(epf%xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create xtalname dataset',.TRUE.)

dataset = SC_dataname
line2(1) = trim(epf%dataname)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create dataname dataset',.TRUE.)

dataset = SC_dispfile
line2(1) = trim(epf%dispfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create dispfile dataset',.TRUE.)

dataset = SC_defectfilename
line2(1) = trim(epf%defectfilename)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create defectfilename dataset',.TRUE.)

dataset = SC_STEMnmlfile
line2(1) = trim(epf%STEMnmlfile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMDCINameList: unable to create STEMnmlfile dataset',.TRUE.)
    
! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteSTEMDCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteGammaSTEMDCINameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write gamma-gamma' STEMDCI namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param dcinl STEMDCI name list structure
!
!> @date 01/04/18 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteGammaSTEMDCINameList(HDF_head,dcinl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteGammaSTEMDCINameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EMgammaSTEMNameListType),INTENT(IN)              :: dcinl


integer(kind=irg),parameter                           :: n_int = 2, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int), ii
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname, str
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = SC_STEMDCINameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ dcinl%platid, dcinl%devid /)
intlist(1) = 'platid'
intlist(2) = 'devid'

call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

io_real = (/ dcinl%voltage, dcinl%convergence, dcinl%dmin /)
reallist(1) = 'voltage'
reallist(2) = 'convergence'
reallist(3) = 'dmin'

call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! vectors
dataset = SC_EulerAngles
hdferr = HDF_writeDatasetfloatArray1D(dataset, dcinl%eu, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteGammaSTEMDCINameList: unable to create EulerAngle dataset',.TRUE.)

! write all the full length strings
dataset = SC_xtalname_gamma
line2(1) = trim(dcinl%gammaname)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteGammaSTEMDCINameList: unable to create gammaname dataset',.TRUE.)

dataset = SC_xtalname_gammap
line2(1) = trim(dcinl%gammapname)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteGammaSTEMDCINameList: unable to create gammapname dataset',.TRUE.)

dataset = SC_datafile
line2(1) = trim(dcinl%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteGammaSTEMDCINameList: unable to create datafile dataset',.TRUE.)

dataset = SC_microstructurefile
line2(1) = trim(dcinl%microstructurefile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteGammaSTEMDCINameList: unable to create microstructurefile dataset',.TRUE.)
    
! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteGammaSTEMDCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteTGBSTEMDCINameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write twist grain boundary STEMDCI namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param dcinl STEMDCI name list structure
!
!> @date 01/04/18 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteTGBSTEMDCINameList(HDF_head,dcinl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteTGBSTEMDCINameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EMTGBSTEMNameListType),INTENT(IN)              :: dcinl


integer(kind=irg),parameter                           :: n_int = 2, n_real = 3
integer(kind=irg)                                     :: hdferr,  io_int(n_int), ii
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname, str
character(fnlen,kind=c_char)                          :: line2(1)


! create the group for this namelist
groupname = SC_STEMDCINameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ dcinl%platid, dcinl%usenumd /)
intlist(1) = 'platid'
intlist(2) = 'usenumd'

call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

io_real = (/ dcinl%voltage, dcinl%convergence, dcinl%dmin /)
reallist(1) = 'voltage'
reallist(2) = 'convergence'
reallist(3) = 'dmin'

call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! vectors
dataset = SC_EulerAngles
hdferr = HDF_writeDatasetfloatArray1D(dataset, dcinl%eu, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTGBSTEMDCINameList: unable to create EulerAngle dataset',.TRUE.)

! write all the full length strings
dataset = SC_xtalname
line2(1) = trim(dcinl%xtalname)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTGBSTEMDCINameList: unable to create gammaname dataset',.TRUE.)

dataset = SC_datafile
line2(1) = trim(dcinl%datafile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTGBSTEMDCINameList: unable to create datafile dataset',.TRUE.)

dataset = SC_microstructurefile
line2(1) = trim(dcinl%microstructurefile)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteTGBSTEMDCINameList: unable to create microstructurefile dataset',.TRUE.)
    
! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteTGBSTEMDCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteSTEMGeometryNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param epf STEMDCI name list structure
!
!> @date 07/02/17 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteSTEMGeometryNameList(HDF_head,epf)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteSTEMGeometryNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(STEMGeometryNameListType),INTENT(IN)             :: epf


integer(kind=irg),parameter                           :: n_int = 2, n_real = 7
integer(kind=irg)                                     :: hdferr,  io_int(n_int), ii
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname, str
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_STEMGeometryNameList
hdferr = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int = (/ epf%numberofsvalues, epf%numCL /)
intlist(1) = 'numberofsvalues'
intlist(2) = 'numCL'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

io_real = (/ epf%BFradius, epf%ADFinnerradius, epf%ADFouterradius, epf%kt, epf%beamconvergence, epf%diffaprad, epf%diffapcenter /)
reallist(1) = 'BFradius'
reallist(2) = 'ADFinnerradius'
reallist(3) = 'ADFouterradius'
reallist(4) = 'kt'
reallist(5) = 'beamconvergence'
reallist(6) = 'diffaprad'
reallist(7) = 'diffapcenter'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! a 20-vector
dataset = SC_CLarray
hdferr = HDF_writeDatasetFloatArray1D(dataset, epf%CLarray, 20, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMGeometryNameList: unable to create CLarray dataset',.TRUE.)

! short strings
dataset = SC_geometry
line2(1) = trim(epf%geometry)
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteSTEMGeometryNameList: unable to create geometry dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteSTEMGeometryNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteCBEDQCNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write CBEDQC namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param pednl CBEDQC name list structure
!
!> @date 02/22/18 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteCBEDQCNameList(HDF_head, cbednl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteCBEDQCNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EMCBEDQCNameListType),INTENT(INOUT)              :: cbednl
!f2py intent(in,out) ::  cbednl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 4
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_CBEDQCNameList
hdferr    = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int      = (/ cbednl%nthreads, cbednl%npix /)
intlist(1)  = 'nthreads'
intlist(2)  = 'npix'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real     = (/ cbednl%voltage, cbednl%thickness, cbednl%dmin, cbednl%convergence /)
reallist(1) = 'voltage'
reallist(2) = 'thickness'
reallist(3) = 'dmin'
reallist(4) = 'convergence'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! euler vectors
dataset = SC_Eulertriplet
hdferr = HDF_writeDatasetFloatArray1D(dataset, cbednl%eu, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create euler dataset',.TRUE.)

! write all the strings
dataset = SC_datafile
line2(1) = cbednl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create datafile dataset',.TRUE.)

! write all the strings
dataset = SC_xtalname
line2(1) = cbednl%qxtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create qxtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteCBEDQCNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwrite2DQCCBEDNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param pednl CBEDQC name list structure
!
!> @date 02/22/18 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwrite2DQCCBEDNameList(HDF_head, cbednl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwrite2DQCCBEDNameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                    :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EMCBED2DQCNameListType),INTENT(INOUT)            :: cbednl
!f2py intent(in,out) ::  cbednl

integer(kind=irg),parameter                           :: n_int = 2, n_real = 5
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = SC_CBEDQCNameList
hdferr    = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int      = (/ cbednl%nthreads, cbednl%npix /)
intlist(1)  = 'nthreads'
intlist(2)  = 'npix'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the single reals
io_real     = (/ cbednl%voltage, cbednl%thickness, cbednl%dmin_qc, cbednl%dmin_p, cbednl%convergence /)
reallist(1) = 'voltage'
reallist(2) = 'thickness'
reallist(3) = 'dmin_qc'
reallist(4) = 'dmin_p'
reallist(5) = 'convergence'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! euler vectors
dataset = SC_Eulertriplet
hdferr = HDF_writeDatasetFloatArray1D(dataset, cbednl%eu, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create euler dataset',.TRUE.)

! write all the strings
dataset = SC_datafile
line2(1) = cbednl%datafile
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create datafile dataset',.TRUE.)

! write all the strings
dataset = SC_xtalname
line2(1) = cbednl%qxtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteCBEDNameList: unable to create qxtalname dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwrite2DQCCBEDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:HDFwriteHH4NameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write namelist to HDF file
!
!> @param HDF_head top of push stack
!> @param hhnl EMHH4 name list structure
!
!> @date 08/23/19 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine HDFwriteHH4NameList(HDF_head, hhnl)
!DEC$ ATTRIBUTES DLLEXPORT :: HDFwriteHH4NameList

use ISO_C_BINDING

IMPLICIT NONE

type(HDFobjectStackType),INTENT(INOUT)                :: HDF_head
!f2py intent(in,out) ::  HDF_head
type(EMhh4NameListType),INTENT(INOUT)                 :: hhnl
!f2py intent(in,out) ::  hhnl

integer(kind=irg),parameter                           :: n_int = 11, n_real = 10 
integer(kind=irg)                                     :: hdferr,  io_int(n_int)
real(kind=sgl)                                        :: io_real(n_real)
character(20)                                         :: intlist(n_int), reallist(n_real)
character(fnlen)                                      :: dataset, groupname
character(fnlen,kind=c_char)                          :: line2(1)

! create the group for this namelist
groupname = 'EMHH4NameList'
hdferr    = HDF_createGroup(groupname,HDF_head)

! write all the single integers
io_int      = (/ hhnl%IROW, hhnl%ICOL, hhnl%wnum, hhnl%LTEST, hhnl%LD, hhnl%LD2, &
                 hhnl%LD3, hhnl%LD4, hhnl%LQ1, hhnl%LQ2, hhnl%LQ3 /)
intlist(1)  = 'IROW'
intlist(2)  = 'ICOL'
intlist(3)  = 'wnum'
intlist(4)  = 'LTEST'
intlist(5)  = 'LD'
intlist(6)  = 'LD2'
intlist(7)  = 'LD3'
intlist(8)  = 'LD4'
intlist(9)  = 'LQ1'
intlist(10)  = 'LQ2'
intlist(11)  = 'LQ3'
call HDF_writeNMLintegers(HDF_head, io_int, intlist, n_int)

! write all the integer arrays 
dataset = 'LB'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LB, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LB dataset',.TRUE.)

dataset = 'LB2'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LB2, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LB2 dataset',.TRUE.)

dataset = 'LB3'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LB3, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LB3 dataset',.TRUE.)

dataset = 'LB4'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LB4, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LB4 dataset',.TRUE.)

dataset = 'LU'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LU, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LU dataset',.TRUE.)

dataset = 'LG'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LG, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LG dataset',.TRUE.)

dataset = 'LBM'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LBM, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LBM dataset',.TRUE.)

dataset = 'LFN'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LFN, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LFN dataset',.TRUE.)

dataset = 'LFP1'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LFP1, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LFP1 dataset',.TRUE.)

dataset = 'LFP'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LFP, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LFP dataset',.TRUE.)

dataset = 'LFP3'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LFP3, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LFP3 dataset',.TRUE.)

dataset = 'LS1'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LS1, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LS1 dataset',.TRUE.)

dataset = 'LS2'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LS2, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LS2 dataset',.TRUE.)

dataset = 'LS3'
hdferr = HDF_writeDatasetIntegerArray1D(dataset, hhnl%LS3, 3, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create LS3 dataset',.TRUE.)


! write all the single reals
io_real     = (/ hhnl%kV, hhnl%THICK, hhnl%START, hhnl%FINISH, hhnl%wmin, hhnl%wmax, hhnl%SEP, hhnl%SEP2, hhnl%FAP1, hhnl%FAP3 /)
reallist(1) = 'kV'
reallist(2) = 'THICK'
reallist(3) = 'START'
reallist(4) = 'FINISH'
reallist(5) = 'wmin'
reallist(6) = 'wmax'
reallist(7) = 'SEP'
reallist(8) = 'SEP2'
reallist(9) = 'FAP1'
reallist(10) = 'FAP3'
call HDF_writeNMLreals(HDF_head, io_real, reallist, n_real)

! real arrays
dataset = 'D1row1'
hdferr = HDF_writeDatasetFloatArray1D(dataset, hhnl%D1row1, 6, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create D1row1 dataset',.TRUE.)

dataset = 'D1row2'
hdferr = HDF_writeDatasetFloatArray1D(dataset, hhnl%D1row2, 6, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create D1row2 dataset',.TRUE.)

dataset = 'D1row3'
hdferr = HDF_writeDatasetFloatArray1D(dataset, hhnl%D1row3, 6, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create D1row3 dataset',.TRUE.)

dataset = 'D1row4'
hdferr = HDF_writeDatasetFloatArray1D(dataset, hhnl%D1row4, 6, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create D1row4 dataset',.TRUE.)

dataset = 'D1row5'
hdferr = HDF_writeDatasetFloatArray1D(dataset, hhnl%D1row5, 6, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create D1row5 dataset',.TRUE.)

dataset = 'D1row6'
hdferr = HDF_writeDatasetFloatArray1D(dataset, hhnl%D1row6, 6, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create D1row6 dataset',.TRUE.)

! write all the strings
dataset = 'outname'
line2(1) = hhnl%outname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create outname dataset',.TRUE.)

dataset = SC_xtalname
line2(1) = hhnl%xtalname
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create xtalname dataset',.TRUE.)

dataset = 'imageprefix'
line2(1) = hhnl%imageprefix
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create imageprefix dataset',.TRUE.)

dataset = 'imagetype'
line2(1) = hhnl%imagetype
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDFwriteHH4NameList: unable to create imagetype dataset',.TRUE.)

! and pop this group off the stack
call HDF_pop(HDF_head)

end subroutine HDFwriteHH4NameList



end module NameListHDFwriters
