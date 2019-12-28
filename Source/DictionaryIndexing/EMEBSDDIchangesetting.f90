! ###################################################################
! Copyright (c) 2015-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDDIchangesetting.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDDIchangesetting
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief for orthorhombic systems, change the Euler angle variables to a different space group setting
!
!> @date 07/18/18 MDG 1.0 new program
!--------------------------------------------------------------------------
program EMEBSDDIchangesetting

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use io
use HDF5
use HDFsupport
use rotations
use EBSDmod
use ECPmod
use EBSDiomod
use EBSDDImod
use files
use constants
use error
use omp_lib
use ISO_C_BINDING
use commonmod

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(EBSDIndexingNameListType)              :: dinl
type(ChangeSettingNameListType)             :: csnl
type(EBSDMasterNameListType)                :: mpnl
type(EBSDDIdataType)                        :: EBSDDIdata
type(EBSDMPdataType)                        :: EBSDMPdata

integer(kind=irg)                           :: hdferr, pgnum, sgnum, orthonum, i, TID, ipar(10) 
character(fnlen)                            :: outstring, dataset, infile, groupname, comment
real(kind=dbl)                              :: dtor, qrot(4), qin(4)
real(kind=dbl),allocatable                  :: newEulers(:,:), newAvOr(:,:), oldEulers(:,:), newRefined(:,:)
real(kind=sgl),allocatable                  :: eulers(:,:), ang(:), resultmain(:,:)
logical                                     :: g_exists, readonly, verbose, overwrite = .TRUE., transformRefined
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)

type(HDFobjectStackType)                    :: HDF_head


nullify(HDF_head%next)

nmldeffile = 'EMEBSDDIchangesetting.nml'
progname = 'EMEBSDDIchangesetting.f90'
progdesc = 'Changes the orthorhombic space group setting in an indexing dot product file'
verbose = .TRUE.

dtor = cPi / 180.D0

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 48 /), progname)

! deal with the namelist stuff
call GetChangeSettingNameList(nmldeffile,csnl)

!====================================
! 1. read the relevant fields from the dot product HDF5 file
! open the fortran HDF interface
call h5open_EMsoft(hdferr)

call readEBSDDotProductFile(csnl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                            getCI=.TRUE., &
                            getIQ=.TRUE., & 
                            getOSM=.TRUE., & 
                            getPhi1=.TRUE., &
                            getPhi=.TRUE., &
                            getPhi2=.TRUE., &
                            getRefinedEulerAngles=.TRUE., &
                            getAverageOrientations=.TRUE., &
                            getEulerAngles=.TRUE., &
                            getTopMatchIndices=.TRUE.)

allocate(oldEulers(3,EBSDDIdata%FZcnt))
allocate(newEulers(3,EBSDDIdata%FZcnt))
allocate(newAvOr(3,EBSDDIdata%Nexp))

transformRefined = .FALSE.
if (allocated(EBSDDIdata%RefinedEulerAngles)) then
  transformRefined = .TRUE.
  allocate(newRefined(3,EBSDDIdata%Nexp))
end if

do i=1,EBSDDIdata%FZcnt
  oldEulers(1:3,i) = EBSDDIdata%EulerAngles(1:3,i)
end do
newEulers = oldEulers * dtor 
newAvOr = EBSDDIdata%AverageOrientations * dtor

call Message(' ')
call Message(' -> completed reading of dot product file')

! 2. read EBSD master pattern file (including HDF format)
call readEBSDMasterPatternFile(dinl%masterfile, mpnl, hdferr, EBSDMPdata)

! 3. check to make sure that this structure is actually orthorhombic...
!    (monoclinic settings will be added in a later version)

!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
pgnum = GetPointGroup(EBSDMPdata%xtalname,.FALSE.,sgnumber=sgnum)

if ((pgnum.lt.6).or.(pgnum.gt.8)) then
    call FatalError('EMEBSDDIchangesetting','Crystal structure point group # must be 6, 7, or 8 (orthorhombic')
end if 

! get the sequential space group number within the orthorhombic system
orthonum = sgnum - SGPG(6) + 1

! OK, this is an orthorhombic structure, so we will first convert the EulerAngle representation to 
! rotation matrices, then apply the appropriate permutation matrix, copy the EulerAngles array to a new 
! EulerAnglesOriginal array, overwrite the Phi1, Phi, and Phi2 arrays with the new values, write the 
! new EulerAngles along with a data set attribute to indicate that this is a derived array, and 
! finally generate the appropriate .ctf files

call Message(' The original orthorhombic setting is '//extendedOrthsettings(1)//' for '//SYM_SGname(sgnum))
outstring = ' The requested setting is '//extendedOrthsettings(csnl%orthorhombicSetting)// &
            ' for '//extendedHMOrthsymbols(csnl%orthorhombicSetting,orthonum)
call Message(outstring)
call Message(' ')
call Message(' Starting conversion of orientation data to new setting ')
call Message('  (original EulerAngles array will be renamed to EulerAnglesOriginal)')
call Message(' ')

! select the quaternion that will carry out the transformation to the new setting
select case(csnl%orthorhombicSetting)
  case(1)
    qrot = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
  case(2)
    qrot = eu2qu( (/ cPi*0.5, cPi, 0.D0/) )
  case(3)
    qrot = eu2qu( (/ cPi*0.5D0, cPi*0.5D0, 0.D0/) )
  case(4)
    qrot = eu2qu( (/ cPi*1.5D0, cPi*0.5D0, cPi*0.5D0/) )
  case(5)
    qrot = eu2qu( (/ cPi, cPi*0.5D0, cPi*0.5D0/) )
  case(6)
    qrot = eu2qu( (/ 0.0D0, cPi*0.5D0, 0.D0/) )
  case default
    call FatalError('EMEBSDDIchangesetting','Unknown orthorhombic setting; please check for typos')
end select

qrot = conjg(qrot)

! parallel section starts here
call OMP_SET_NUM_THREADS(csnl%nthreads)
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, i, qin)
TID = OMP_GET_THREAD_NUM()

!$OMP DO SCHEDULE(DYNAMIC)
do i=1,EBSDDIdata%FZcnt
  qin = eu2qu(newEulers(1:3,i))
  qin = quat_mult(qrot,qin)
  newEulers(1:3,i) = qu2eu(qin)
end do 
!$OMP END DO

if (TID.eq.0) call Message('  -> completed transformation of EulerAngles array')

! rotate the average orientations to the new setting 

!$OMP DO SCHEDULE(DYNAMIC)
do i=1,EBSDDIdata%Nexp
  qin = eu2qu(newAvOr(1:3,i))
  qin = quat_mult(qrot,qin)
  newAvOr(1:3,i) = qu2eu(qin)
end do
!$OMP END DO
if (TID.eq.0) call Message('  -> completed transformation of AverageOrientations array')

if (transformRefined.eqv..TRUE.) then
!$OMP DO SCHEDULE(DYNAMIC)
  do i=1,EBSDDIdata%Nexp
    qin = eu2qu(EBSDDIdata%RefinedEulerAngles(1:3,i))
    qin = quat_mult(qrot,qin)
    newRefined(1:3,i) = qu2eu(qin)
  end do
!$OMP END DO
  if (TID.eq.0) call Message('  -> completed transformation of RefinedEulerAngles array')
end if

!$OMP END PARALLEL

newEulers = newEulers / dtor
newAvOr = newAvOr / dtor
! and ends here...

! open the dotproductfile 
infile = trim(EMsoft_getEMdatapathname())//trim(csnl%dotproductfile)
infile = EMsoft_toNativePath(infile)

!===================================================================================
! open the dot product file 
nullify(HDF_head%next)
hdferr =  HDF_openFile(infile, HDF_head)

! open the Scan 1/EBSD/Data group
groupname = 'Scan 1'
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSD
hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
hdferr = HDF_openGroup(groupname, HDF_head)

! next, create a new EulerAnglesOriginal array and write the original angles to its
! same thing for the AverageOrientations array
dataset = 'EulerAnglesOriginal'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray2D(dataset, EBSDDIdata%EulerAngles, 3, EBSDDIdata%FZcnt, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray2D(dataset, EBSDDIdata%EulerAngles, 3, EBSDDIdata%FZcnt, HDF_head)
end if

! then, overwrite the existing EulerAngles data set with the rotated orientations
dataset = SC_EulerAngles
hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(newEulers), 3, EBSDDIdata%FZcnt, HDF_head, overwrite)
! add an explanatory attribute to the data set

! do the same with the AverageOrientations data set
dataset = 'AverageOrientationsOriginal'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetFloatArray2D(dataset, EBSDDIdata%AverageOrientations, 3, EBSDDIdata%Nexp, HDF_head, overwrite)
else
  hdferr = HDF_writeDatasetFloatArray2D(dataset, EBSDDIdata%AverageOrientations, 3, EBSDDIdata%Nexp, HDF_head)
end if

! then, overwrite the existing EulerAngles data set with the rotated orientations
dataset = SC_AverageOrientations
hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(newAvOr), 3, EBSDDIdata%Nexp, HDF_head, overwrite)

if (transformRefined.eqv..TRUE.) then
  ! next, create a new RefinedEulerAnglesOriginal array and write the original angles to its
  dataset = 'RefinedEulerAnglesOriginal'
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray2D(dataset, EBSDDIdata%RefinedEulerAngles, 3, EBSDDIdata%Nexp, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetFloatArray2D(dataset, EBSDDIdata%RefinedEulerAngles, 3, EBSDDIdata%Nexp, HDF_head)
  end if

  ! then, overwrite the existing EulerAngles data set with the rotated orientations
  dataset = SC_RefinedEulerAngles
  hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(newRefined), 3, EBSDDIdata%Nexp, HDF_head, overwrite)
end if

! add an explanatory data set
comment = 'Original orthorhombic setting changed to '//extendedOrthsettings(csnl%orthorhombicSetting)// &
          ', corresponding to space group symbol '//extendedHMOrthsymbols(csnl%orthorhombicSetting,orthonum)
allocate(stringarray(1))
stringarray(1)= trim(comment)
dataset = 'Comment'
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head, overwrite) 
else
  hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head) 
end if
deallocate(stringarray)

! also, update the Phi1, Phi, and Phi2 data sets 
newEulers = newEulers * sngl(dtor)
allocate(eulers(3,EBSDDIdata%Nexp), ang(EBSDDIdata%Nexp))
do i=1,EBSDDIdata%Nexp
  eulers(1:3,i) = newEulers(1:3,EBSDDIdata%TopMatchIndices(1,i))
end do

dataset = SC_Phi1
ang(:) = eulers(1,:)
hdferr = HDF_writeDatasetFloatArray1D(dataset, ang, EBSDDIdata%Nexp, HDF_head, overwrite)

dataset = SC_Phi
ang(:) = eulers(2,:)
hdferr = HDF_writeDatasetFloatArray1D(dataset, ang, EBSDDIdata%Nexp, HDF_head, overwrite)

dataset = SC_Phi2
ang(:) = eulers(3,:)
hdferr = HDF_writeDatasetFloatArray1D(dataset, ang, EBSDDIdata%Nexp, HDF_head, overwrite)

! leave this group and file
call HDF_pop(HDF_head,.TRUE.)

! finally, we need to write a new .ctf file as well... 
dinl%ctffile = trim(csnl%newctffile)

ipar = 0
ipar(1) = 1
ipar(2) = EBSDDIdata%Nexp
ipar(3) = EBSDDIdata%Nexp
ipar(4) = EBSDDIdata%Nexp 
ipar(5) = EBSDDIdata%FZcnt
ipar(6) = pgnum
if (sum(dinl%ROI).ne.0) then
    ipar(7) = dinl%ROI(3)
    ipar(8) = dinl%ROI(4)
else
    ipar(7) = dinl%ipf_wd
    ipar(8) = dinl%ipf_ht
end if

allocate(resultmain(1,ipar(2)))
resultmain(1,:) = EBSDDIdata%CI(:)

eulers = eulers / sngl(dtor)

call ctfebsd_writeFile(dinl,EBSDMPdata%xtalname,ipar,EBSDDIdata%TopMatchIndices, &
                       eulers,resultmain,EBSDDIdata%OSM,EBSDDIdata%IQ,noindex=.TRUE.)
call Message('Data stored in ctf file : '//trim(dinl%ctffile))

call h5close_EMsoft(hdferr)

end program EMEBSDDIchangesetting
