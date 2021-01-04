!--------------------------------------------------------------------------
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
! EMsoft:NameListHandlers.f90
!--------------------------------------------------------------------------
!
! PROGRAM: NameListHandlers
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief routines for reading and returning name list type structures
!
!> @date 06/13/14 MDG 1.0 original
!> @date 05/05/15 MDG 1.1 removed primelist variable from name list files
!> @date 08/12/15 MDG 1.2 added initonly optional keyword to skip reading from file
!--------------------------------------------------------------------------
module NameListHandlers

use local
use NameListTypedefs

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetGrainVizNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill gvnl structure (used by EMgrainviz.f90)
!
!> @param nmlfile namelist file name
!> @param gvnl name list structure
!> @param initonly [optional] logical
!
!> @date 04/22/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetGrainVizNameList(nmlfile, gvnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetGrainVizNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(GrainVizNameListType),INTENT(INOUT)    :: gvnl
!f2py intent(in,out) ::  gvnl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

real(kind=dbl)          :: fraction
real(kind=dbl)          :: pA(4)
integer(kind=irg)       :: numbins
character(6)            :: inside
character(fnlen)        :: qAfilename
character(fnlen)        :: qBfilename
character(fnlen)        :: povname

namelist /GrainVizlist/ fraction, numbins, inside, qAfilename, qBfilename, povname, pA

fraction = 1.D0   ! 0=cube, 1=sphere
pA = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
numbins = 50      ! number of sampling points along semi edge of Lambert square
inside = 'inside' ! 'inside' or 'all'
qAfilename = 'undefined'
qBfilename = 'undefined'
povname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=GrainVizlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(qAfilename).eq.'undefined') then
  call FatalError('EMgrainviz:',' qA output file name is undefined in '//nmlfile)
 end if
 if (trim(qBfilename).eq.'undefined') then
  call FatalError('EMgrainviz:',' qB output file name is undefined in '//nmlfile)
 end if

 if (trim(povname).eq.'undefined') then
  call FatalError('EMgrainviz:',' powray output file name is undefined in '//nmlfile)
 end if
end if

gvnl%fraction = fraction 
gvnl%pA = pA
gvnl%numbins = numbins
gvnl%inside = trim(inside)
gvnl%qAfilename = trim(qAfilename)
gvnl%qBfilename = trim(qBfilename)
gvnl%povname = trim(povname)

end subroutine GetGrainVizNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetChangeSettingNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill gvnl structure (used by EMgrainviz.f90)
!
!> @param nmlfile namelist file name
!> @param gvnl name list structure
!> @param initonly [optional] logical
!
!> @date 07/18/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetChangeSettingNameList(nmlfile, csnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetChangeSettingNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(ChangeSettingNameListType),INTENT(INOUT) :: csnl
!f2py intent(in,out) ::  csnl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

integer(kind=irg)       :: orthorhombicSetting
integer(kind=irg)       :: nthreads
character(fnlen)        :: dotproductfile
character(fnlen)        :: newctffile

namelist /ChangeSettingslist/ nthreads, orthorhombicSetting, dotproductfile, newctffile

dotproductfile = 'undefined'
newctffile = 'undefined'
orthorhombicSetting = 1
nthreads = 1

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=ChangeSettingslist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError('GetChangeSettingNameList:',' dotproductfile name is undefined in '//nmlfile)
 end if

 if (trim(newctffile).eq.'undefined') then
  call FatalError('GetChangeSettingNameList:',' newctffile name is undefined in '//nmlfile)
 end if
end if

csnl%dotproductfile = dotproductfile 
csnl%newctffile = newctffile
csnl%nthreads = nthreads
csnl%orthorhombicSetting = orthorhombicSetting 

end subroutine GetChangeSettingNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetCTFNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill csnl structure (used by EMgetCTF.f90)
!
!> @param nmlfile namelist file name
!> @param csnl name list structure
!> @param initonly [optional] logical
!
!> @date 07/19/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetCTFNameList(nmlfile, csnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetCTFNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(CTFNameListType),INTENT(INOUT)         :: csnl
!f2py intent(in,out) ::  csnl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

character(4)            :: modality
character(8)            :: angledataset   ! 'original' or 'refined'
character(fnlen)        :: xtalname
character(fnlen)        :: newctffile
character(fnlen)        :: dotproductfile


namelist /CTFlist/ modality, xtalname, angledataset, dotproductfile, newctffile

dotproductfile = 'undefined'
newctffile = 'undefined'
xtalname = 'undefined'
modality = 'EBSD'
angledataset = 'original'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=CTFlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError('GetCTFNameList:',' dotproductfile name is undefined in '//nmlfile)
 end if

 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetCTFNameList:',' xtalname name is undefined in '//nmlfile)
 end if

 if (trim(newctffile).eq.'undefined') then
  call FatalError('GetCTFNameList:',' newctffile name is undefined in '//nmlfile)
 end if
end if

csnl%dotproductfile = dotproductfile 
csnl%newctffile = newctffile
csnl%xtalname = xtalname
csnl%modality = trim(modality)
csnl%angledataset = trim(angledataset) 

end subroutine GetCTFNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetANGNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill csnl structure (used by EMgetANG.f90)
!
!> @param nmlfile namelist file name
!> @param csnl name list structure
!> @param initonly [optional] logical
!
!> @date 07/19/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetANGNameList(nmlfile, csnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetANGNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(ANGNameListType),INTENT(INOUT)         :: csnl
!f2py intent(in,out) ::  csnl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

character(4)            :: modality
character(8)            :: angledataset   ! 'original' or 'refined'
character(fnlen)        :: xtalname
character(fnlen)        :: newangfile
character(fnlen)        :: dotproductfile


namelist /ANGlist/ modality, xtalname, angledataset, dotproductfile, newangfile

dotproductfile = 'undefined'
newangfile = 'undefined'
xtalname = 'undefined'
modality = 'EBSD'
angledataset = 'original'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=ANGlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError('GetANGNameList:',' dotproductfile name is undefined in '//nmlfile)
 end if

 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetANGNameList:',' xtalname name is undefined in '//nmlfile)
 end if

 if (trim(newangfile).eq.'undefined') then
  call FatalError('GetANGNameList:',' newangfile name is undefined in '//nmlfile)
 end if
end if

csnl%dotproductfile = dotproductfile 
csnl%newangfile = newangfile
csnl%xtalname = xtalname
csnl%modality = trim(modality)
csnl%angledataset = trim(angledataset) 

end subroutine GetANGNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEulersNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill csnl structure (used by EMgetEulers.f90)
!
!> @param nmlfile namelist file name
!> @param csnl name list structure
!> @param initonly [optional] logical
!
!> @date 05/22/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEulersNameList(nmlfile, csnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEulersNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(EulersNameListType),INTENT(INOUT)      :: csnl
!f2py intent(in,out) ::  csnl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

character(8)            :: angledataset   ! 'original' or 'refined'
character(3)            :: raddeg         ! 'rad' or 'deg'
character(fnlen)        :: txtfile
character(fnlen)        :: datafile
character(fnlen)        :: EMEBSDnmlfile
character(fnlen)        :: dotproductfile


namelist /Eulerslist/ datafile, txtfile, angledataset, dotproductfile, EMEBSDnmlfile, raddeg

dotproductfile = 'undefined'
txtfile = 'undefined'
datafile = 'undefined'
EMEBSDnmlfile = 'undefined'
angledataset = 'original'
raddeg = 'deg'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=Eulerslist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError('GetEulersNameList:',' dotproductfile name is undefined in '//nmlfile)
 end if

 if (trim(txtfile).eq.'undefined') then
  call FatalError('GetEulersNameList:',' txtfile name is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('GetEulersNameList:',' datafile name is undefined in '//nmlfile)
 end if
end if

csnl%dotproductfile = dotproductfile 
csnl%txtfile = txtfile
csnl%datafile = datafile
csnl%EMEBSDnmlfile = EMEBSDnmlfile
csnl%angledataset = trim(angledataset) 
csnl%raddeg = raddeg

end subroutine GetEulersNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetGBONameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill gbonl structure (used by EMGBO.f90)
!
!> @param nmlfile namelist file name
!> @param gbonl name list structure
!> @param initonly [optional] logical
!
!> @date 04/22/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetGBONameList(nmlfile, gbonl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetGBONameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(GBONameListType),INTENT(INOUT)         :: gbonl
!f2py intent(in,out) ::  gbonl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
integer(kind=irg)       :: numsamples
integer(kind=irg)       :: numbins
integer(kind=irg)       :: pgnum
character(3)            :: CSLtype
logical                 :: fixedAB
character(fnlen)        :: outname
character(fnlen)        :: octonions

namelist /GBOlist/ pgnum, numsamples, numbins, outname, nthreads, CSLtype, fixedAB, octonions

nthreads = 1
outname = 'undefined' 
octonions = 'random'
pgnum = 32
numsamples = 100000
numbins = 180
CSLtype = ''
fixedAB = .FALSE.

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=GBOlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(outname).eq.'undefined') then
  call FatalError('EMGBO:',' output file name is undefined in '//nmlfile)
 end if
end if

gbonl%nthreads = nthreads
gbonl%pgnum = pgnum
gbonl%numsamples = numsamples
gbonl%numbins = numbins
gbonl%outname = trim(outname)
gbonl%octonions = trim(octonions)
gbonl%CSLtype = trim(CSLtype)
gbonl%fixedAB = fixedAB

end subroutine GetGBONameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetGBOdmNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill gbonl structure (used by EMGBOdm.f90)
!
!> @param nmlfile namelist file name
!> @param gbonl name list structure
!> @param initonly [optional] logical
!
!> @date 04/22/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetGBOdmNameList(nmlfile, gbonl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetGBOdmNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(GBOdmNameListType),INTENT(INOUT)       :: gbonl
!f2py intent(in,out) ::  gbonl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

integer(kind=irg)       :: pgnum
integer(kind=irg)       :: nthreads
logical                 :: refine
character(fnlen)        :: workmode
character(fnlen)        :: metric
character(fnlen)        :: inname
character(fnlen)        :: outname

namelist /GBOdmlist/ pgnum, outname, nthreads, inname, metric, workmode, refine

nthreads = 0
metric = 'octonion'    ! or 'Olmsted' or 'Riemannian'
refine = .FALSE.
workmode = 'newmatrix' ! or 'addcolumns'
outname = 'undefined' 
inname = 'undefined' 
pgnum = 32

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=GBOdmlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(outname).eq.'undefined') then
  call FatalError('EMGBOdm:',' output file name is undefined in '//nmlfile)
 end if

 if (trim(inname).eq.'undefined') then
  call FatalError('EMGBOdm:',' input file name is undefined in '//nmlfile)
 end if
end if

gbonl%nthreads = nthreads
gbonl%pgnum = pgnum
gbonl%refine = refine
gbonl%outname = outname
gbonl%metric = metric
gbonl%workmode = workmode
gbonl%inname = inname

end subroutine GetGBOdmNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetoSLERPNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill onl structure (used by EMoSLERP.f90)
!
!> @param nmlfile namelist file name
!> @param onl name list structure
!> @param initonly [optional] logical
!
!> @date 05/05/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetoSLERPNameList(nmlfile, onl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetoSLERPNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(oSLERPNameListType),INTENT(INOUT)      :: onl
!f2py intent(in,out) ::  onl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

integer(kind=irg)       :: framesize
real(kind=dbl)          :: qm(4)
real(kind=dbl)          :: mA(3)
real(kind=dbl)          :: mC(3)
real(kind=dbl)          :: o1(8)
real(kind=dbl)          :: o2(8)
real(kind=dbl)          :: dOmega
character(fnlen)        :: GBmode
character(fnlen)        :: rendermode
character(fnlen)        :: xtalname 
character(fnlen)        :: povrayfile
character(fnlen)        :: framefolder
character(fnlen)        :: moviename

namelist /oSLERPlist/ framesize, qm, mA, mC, o1, o2, dOmega, GBmode, xtalname, povrayfile, framefolder, &
                      rendermode, moviename

framesize = 1024
! if GBmode = 'normal'
qm = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
mA = (/ 1.D0, 0.D0, 0.D0 /)
mC = (/ 1.D0, 0.D0, 0.D0 /)
! if GBmode = 'octonion'
o1 = (/ 1.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0 /)   ! normalization will be done by program
o2 = (/ 1.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0 /)   ! normalization will be done by program

dOmega = 0.25D0
GBmode = 'normal'      ! 'normal' for (mA, qm) description; 'octonion' for (qA, qB) description
rendermode = 'cubes'
xtalname = 'undefined'
povrayfile = 'underfined'
framefolder = 'frames'
moviename = 'render.mp4'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=oSLERPlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMoSLERP:',' xtal input file name is undefined in '//nmlfile)
 end if
 if (trim(povrayfile).eq.'undefined') then
  call FatalError('EMoSLERP:',' POVray output file name is undefined in '//nmlfile)
 end if
end if

onl%framesize = framesize
onl%qm = qm
onl%mA = mA
onl%mC = mC
onl%o1 = o1
onl%o2 = o2
onl%qm = qm
onl%dOmega = dOmega
onl%rendermode = rendermode
onl%xtalname = trim(xtalname)
onl%povrayfile = trim(povrayfile) 
onl%framefolder = trim(framefolder)
onl%moviename = trim(moviename)

end subroutine GetoSLERPNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetLorentzNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMLorentz.f90)
!
!> @param nmlfile namelist file name
!> @param enl Lorentz name list structure
!> @param initonly [optional] logical
!
!> @date 06/13/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetLorentzNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetLorentzNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(LorentzNameListType),INTENT(INOUT) :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
integer(kind=irg)       :: numappos
integer(kind=irg)       :: numdefocus
integer(kind=irg)       :: numtilts
real(kind=sgl)          :: voltage
real(kind=sgl)          :: apertureradius
real(kind=sgl)          :: apertureposition(2)
real(kind=sgl)          :: defocusstart
real(kind=sgl)          :: defocusstep
real(kind=sgl)          :: defocusspread
real(kind=sgl)          :: thetac
real(kind=sgl)          :: astigmatism(2)
real(kind=sgl)          :: beamdc(3)
real(kind=sgl)          :: tiltaxis(3)
real(kind=sgl)          :: tiltstart
real(kind=sgl)          :: tiltstepsize
character(fnlen)        :: phasemethod
character(fnlen)        :: frfo
character(fnlen)        :: Magfile
character(fnlen)        :: outputfile
character(fnlen)        :: phiefile
character(fnlen)        :: phimfile
character(fnlen)        :: intBxfile
character(fnlen)        :: intByfile
character(fnlen)        :: colormapfile

namelist /EMLorentz/ nthreads, numappos, numdefocus, numtilts, voltage, apertureradius, apertureposition, &
                     defocusstart, defocusstep, defocusspread, thetac, astigmatism, beamdc, tiltaxis, tiltstepsize, frfo, &
                     tiltstart, phasemethod, Magfile, outputfile, phiefile, phimfile, intByfile, intBxfile, colormapfile

! default values that are set in the name list file
nthreads = 1
numappos = 1
numdefocus = 1
numtilts = 0
voltage = 200.0
apertureradius = 10.0
apertureposition = (/ 0.0, 0.0 /)
defocusstart = 0.0
defocusstep = 10.0
defocusspread = 10.0
thetac = 0.001
astigmatism = (/ 0.0, 0.0 /)
beamdc = (/ 0.0, 0.0, 1.0 /)
tiltaxis = (/ 1.0, 0.0, 0.0 /)
tiltstart = -70.0
tiltstepsize = 2.0
phasemethod = 'mansuripur'
frfo = 'Fresnel'
Magfile = 'undefined'
outputfile = 'undefined'
phiefile = 'undefined'
phimfile = 'undefined'
intBxfile = 'undefined'
intByfile = 'undefined'
colormapfile = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EMLorentz)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(Magfile).eq.'undefined') then
  call FatalError('EMLorentz:',' Mag HDF5 input file name is undefined in '//nmlfile)
 end if
 if (trim(outputfile).eq.'undefined') then
  call FatalError('EMLorentz:',' output HDF5 file name is undefined in '//nmlfile)
 end if
end if

! and assign the values to the namelist structure
enl%nthreads = nthreads
enl%numappos = numappos
enl%numdefocus = numdefocus
enl%numtilts = numtilts
enl%voltage = voltage
enl%apertureradius = apertureradius
enl%apertureposition = apertureposition
enl%defocusstart = defocusstart
enl%defocusstep = defocusstep
enl%defocusspread = defocusspread
enl%thetac = thetac
enl%astigmatism = astigmatism
enl%beamdc = beamdc
enl%tiltaxis = tiltaxis
enl%tiltstart = tiltstart
enl%tiltstepsize = tiltstepsize
enl%phasemethod = phasemethod
enl%frfo = frfo
enl%Magfile = Magfile
enl%outputfile = outputfile
enl%phiefile = phiefile
enl%phimfile = phimfile
enl%intBxfile = intBxfile
enl%intByfile = intByfile
enl%colormapfile = colormapfile

end subroutine GetLorentzNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMultiPhaseNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMMultiphase.f90)
!
!> @param nmlfile namelist file name
!> @param enl multiphase name list structure
!> @param initonly [optional] logical
!
!> @date 02/21/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetMultiPhaseNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMultiPhaseNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(MultiPhaseNameListType),INTENT(INOUT)  :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
character(fnlen)        :: dp1file
character(fnlen)        :: dp2file
character(fnlen)        :: dp3file
character(fnlen)        :: outputfile

namelist /multiphaselist/ dp1file, dp2file, dp3file, outputfile, nthreads

nthreads = 1
dp1file = 'undefined' 
dp2file = 'undefined' 
dp3file = 'undefined' 
outputfile = 'undefined' 

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=multiphaselist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dp1file).eq.'undefined') then
  call FatalError('Emultiphase:',' dp1 file name is undefined in '//nmlfile)
 end if
 if (trim(dp2file).eq.'undefined') then
  call FatalError('Emultiphase:',' dp2 file name is undefined in '//nmlfile)
 end if
 if (trim(dp3file).eq.'undefined') then
  call FatalError('Emultiphase:',' dp3 file name is undefined in '//nmlfile)
 end if
 if (trim(outputfile).eq.'undefined') then
  call FatalError('Emultiphase:',' output file name is undefined in '//nmlfile)
 end if
end if

enl%nthreads = nthreads
enl%dp1file = dp1file
enl%dp2file = dp2file
enl%dp3file = dp3file
enl%outputfile = outputfile

end subroutine GetMultiPhaseNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetKosselNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill knl structure (used by EMKossel.f90)
!
!> @param nmlfile namelist file name
!> @param knl Kossel name list structure
!> @param initonly [optional] logical
!
!> @date 06/13/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetKosselNameList(nmlfile, knl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetKosselNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(KosselNameListType),INTENT(INOUT)  :: knl
!f2py intent(in,out) ::  knl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numthick
integer(kind=irg)       :: npix
integer(kind=irg)       :: maxHOLZ
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: k(3)
integer(kind=irg)       :: fn(3)
real(kind=sgl)          :: voltage
real(kind=sgl)          :: dmin
real(kind=sgl)          :: convergence
real(kind=sgl)          :: startthick
real(kind=sgl)          :: thickinc
real(kind=sgl)          :: minten
character(fnlen)        :: xtalname
character(fnlen)        :: outname


namelist /Kossellist/ stdout, xtalname, voltage, k, fn, dmin, convergence, minten, nthreads, &
                              startthick, thickinc, numthick, outname, npix, maxHOLZ

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6                      ! standard output
numthick = 10                   ! number of increments
npix = 256                      ! output arrays will have size npix x npix
maxHOLZ = 3                     ! output arrays will have size npix x npix
nthreads = 4                    ! default number of threads for OpenMP
k = (/ 0, 0, 1 /)               ! beam direction [direction indices]
fn = (/ 0, 0, 1 /)              ! foil normal [direction indices]
voltage = 200000.0              ! acceleration voltage [V]
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
convergence = 25.0              ! beam convergence angle [mrad]
startthick = 10.0               ! starting thickness [nm]
thickinc = 10.0                 ! thickness increment
minten = 1.0E-5                 ! minimum intensity in diffraction disk to make it into the output file
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
outname = 'Kosselout.data'      ! output filename

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=Kossellist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMKossel:',' structure file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the knl fields
knl%stdout = stdout
knl%numthick = numthick
knl%npix = npix
knl%maxHOLZ = maxHOLZ
knl%nthreads = nthreads
knl%k = k
knl%fn = fn
knl%voltage = voltage
knl%dmin = dmin
knl%convergence = convergence
knl%startthick = startthick
knl%thickinc = thickinc
knl%minten = minten
knl%xtalname = xtalname
knl%outname = outname

end subroutine GetKosselNameList
!--------------------------------------------------------------------------
!
! SUBROUTINE:GetKosselMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill knl structure (used by EMKosselmaster.f90)
!
!> @param nmlfile namelist file name
!> @param knl Kossel name list structure
!
!> @date 09/09/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetKosselMasterNameList(nmlfile, knl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetKosselMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(KosselMasterNameListType),INTENT(INOUT)  :: knl
!f2py intent(in,out) ::  knl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numthick
integer(kind=irg)       :: npx
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: voltage
real(kind=sgl)          :: dmin
real(kind=sgl)          :: startthick
real(kind=sgl)          :: thickinc
real(kind=sgl)          :: tfraction
character(6)            :: Kosselmode
character(fnlen)        :: xtalname
character(fnlen)        :: outname

namelist /Kosselmasterlist/ stdout, xtalname, voltage, dmin,  nthreads, &
                              startthick, thickinc, numthick, tfraction, outname, npx, Kosselmode

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6                      ! standard output
numthick = 10                   ! number of increments
npx = 256                       ! output arrays will have size npix x npix
nthreads = 4                    ! default number of threads for OpenMP
voltage = 200000.0              ! acceleration voltage [V]
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
startthick = 10.0               ! starting thickness [nm]
thickinc = 10.0                 ! thickness increment
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
outname = 'Kosselout.data'      ! output filename
Kosselmode = 'normal'           ! 'thicks' for thickness determination, 'normal' for normal plot
tfraction = 0.1                 ! thickness fraction for 'thicks' mode

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=Kosselmasterlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMKosselMaster:',' structure file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the knl fields
knl%stdout = stdout
knl%numthick = numthick
knl%npx = npx
knl%nthreads = nthreads
knl%voltage = voltage
knl%dmin = dmin
knl%startthick = startthick
knl%thickinc = thickinc
knl%tfraction = tfraction
knl%Kosselmode = Kosselmode
knl%xtalname = xtalname
knl%outname = outname

end subroutine GetKosselMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetCPLMmasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMCPLMmaster.f90)
!
!> @param nmlfile namelist file name
!> @param omnl name list structure
!
!> @date 06/18/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetCPLMmasterNameList(nmlfile, omnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetCPLMmasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(CPLMmasterNameListType),INTENT(INOUT)    :: omnl
!f2py intent(in,out) ::  omnl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                       :: skipread = .FALSE.

integer(kind=irg)                             :: npx
integer(kind=irg)                             :: nthreads
real(kind=sgl)                                :: eps1Re
real(kind=sgl)                                :: eps1Im
real(kind=sgl)                                :: eps2Re
real(kind=sgl)                                :: eps2Im
real(kind=sgl)                                :: wl
real(kind=sgl)                                :: theta 
logical                                       :: normalize
character(3)                                  :: Notify
character(fnlen)                              :: xtalname
character(fnlen)                              :: masterfile

! define the IO namelist to facilitate passing variables to the program.
namelist  / CPLMasterData / npx, nthreads, eps1Re, eps1Im, eps2Re, eps2Im, wl, theta, &
                           Notify, xtalname, masterfile, normalize

xtalname = 'undefined'
theta = 0.0 
wl = 750.0
eps1Re =  1.0
eps1Im =  0.0
eps2Re =  1.0
eps2Im =  0.0
npx = 360
normalize = .FALSE.
nthreads = 1
masterfile = 'undefined'
Notify = 'Off' 

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=CPLMasterData)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetCPLMmasterNameList:',' structure file name is undefined in '//nmlfile)
 end if
 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetCPLMmasterNameList:',' master output file name is undefined in '//nmlfile)
 end if
end if

omnl%npx = npx
omnl%nthreads = nthreads
omnl%eps1Re = eps1Re
omnl%eps1Im = eps1Im
omnl%eps2Re = eps2Re
omnl%eps2Im = eps2Im
omnl%wl = wl
omnl%theta = theta
omnl%normalize = normalize
omnl%Notify = Notify 
omnl%xtalname = xtalname
omnl%masterfile = masterfile

end subroutine GetCPLMmasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetLaueMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMLauemaster.f90)
!
!> @param nmlfile namelist file name
!> @param lmnl name list structure
!
!> @date 03/14/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetLaueMasterNameList(nmlfile, lmnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetLaueMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(LaueMasterNameListType),INTENT(INOUT)    :: lmnl
!f2py intent(in,out) ::  lmnl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                       :: skipread = .FALSE.

integer(kind=irg)       :: npx
integer(kind=irg)       :: patchw
real(kind=sgl)          :: lambdamin
real(kind=sgl)          :: lambdamax
real(kind=dbl)          :: kappaVMF
real(kind=dbl)          :: intfactor
character(3)            :: outformat
logical                 :: binarize
character(fnlen)        :: SHT_folder
character(fnlen)        :: SHT_formula
character(fnlen)        :: SHT_name
character(fnlen)        :: SHT_structuresymbol
character(fnlen)        :: addtoKiltHub
character(fnlen)        :: useDOI
character(fnlen)        :: hdfname
character(fnlen)        :: tiffname
character(fnlen)        :: xtalname

! define the IO namelist to facilitate passing variables to the program.
namelist  / LaueMasterData / npx, lambdamin, lambdamax, kappaVMF, hdfname, xtalname, &
                             intfactor, tiffname, patchw, SHT_folder, SHT_formula, SHT_name, &
                             SHT_structuresymbol, addtoKiltHub, useDOI, outformat, binarize

npx = 500
patchw = 5
lambdamin = 0.10
lambdamax = 0.16
kappaVMF = 50000.D0
intfactor = 0.0001D0
outformat = 'LMP'
SHT_folder = 'undefined'        ! folder to store SHT files, relative to EMDatapathname
SHT_formula = 'undefined'       ! compound chemical formula, e.g., SiO2
SHT_name = 'undefined'          ! compund name (e.g., forsterite)
SHT_structuresymbol = 'undefined' ! StrukturBericht symbol (e.g., D0_22) or Pearson symbol (e.g., hP12), or ...
addtoKiltHub = 'No'             ! file to be added to data base on kilthub.cmu.edu ?
useDOI = 'undefined'            ! if no DOI is entered, then we use the Zenodo DOI for the .sht repository
xtalname = 'undefined'
hdfname = 'undefined'
tiffname = 'undefined'
binarize = .FALSE.

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=LaueMasterData)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetLaueMasterNameList:',' structure file name is undefined in '//nmlfile)
 end if
 if (trim(hdfname).eq.'undefined') then
  call FatalError('GetLaueMasterNameList:',' master output file name is undefined in '//nmlfile)
 end if

 if (outformat.eq.'SHT') then 
 ! for Legendre mode, the SHT_formula parameter MUST be present 
   if (trim(SHT_formula).eq.'undefined') then 
    call FatalError('GetLaueMasterNameList:',' SHT_formula must be defined in '//nmlfile)
   end if

   if (trim(SHT_folder).eq.'undefined') then 
    call FatalError('GetLaueMasterNameList:',' SHT_folder must be defined in '//nmlfile)
   end if
 end if 

end if

lmnl%npx = npx
lmnl%patchw = patchw
lmnl%lambdamin = lambdamin
lmnl%lambdamax = lambdamax
lmnl%kappaVMF = kappaVMF
lmnl%intfactor = intfactor
lmnl%xtalname = xtalname
lmnl%outformat = outformat
lmnl%hdfname = hdfname
lmnl%tiffname = tiffname 
lmnl%addtoKiltHub = addtoKiltHub
lmnl%useDOI = useDOI
lmnl%SHT_formula = SHT_formula
lmnl%SHT_name = SHT_name
lmnl%SHT_structuresymbol = SHT_structuresymbol
lmnl%SHT_folder = trim(SHT_folder)
lmnl%binarize = binarize

end subroutine GetLaueMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetLaueNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill lnl structure (used by EMLaue.f90)
!
!> @param nmlfile namelist file name
!> @param lmnl name list structure
!
!> @date 03/28/19  MDG 1.0 new routine
!> @dete 07/30/19  MDG 1.1 reorganization of namelist
!--------------------------------------------------------------------------
recursive subroutine GetLaueNameList(nmlfile, lnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetLaueNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(LaueNameListType),INTENT(INOUT)          :: lnl
!f2py intent(in,out) ::  lnl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                       :: skipread = .FALSE.

integer(kind=irg)       :: numpx
integer(kind=irg)       :: numpy
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: BPx
real(kind=sgl)          :: spotw
real(kind=sgl)          :: pixelsize
real(kind=sgl)          :: maxVoltage
real(kind=sgl)          :: minVoltage
real(kind=sgl)          :: SDdistance
real(kind=sgl)          :: gammavalue
character(fnlen)        :: backprojection
character(fnlen)        :: Lauemode
character(fnlen)        :: orientationfile
character(fnlen)        :: tiffprefix
character(fnlen)        :: xtalname
character(fnlen)        :: hdfname

! define the IO namelist to facilitate passing variables to the program.
namelist  / LaueData / numpx, numpy, nthreads, spotw, pixelsize, maxVoltage, minVoltage, SDdistance, &
                       gammavalue, Lauemode, orientationfile, tiffprefix, xtalname, hdfname, BPx, &
                       backprojection

numpx = 1024                   ! detector x-size (pixels)
numpy = 768                    ! detector y-size (pixels)
nthreads = 1                   ! number of parallel threads for pattern computation
BPx = 300                      ! semi-edge length for back projection square Lambert maps
pixelsize = 50.0               ! micron
spotw = 0.1                    ! spot size weight factor (1/(2*sigma^2))
maxVoltage = 30.0              ! in kV
minVoltage = 15.0              ! in kV
SDdistance = 100.0             ! mm
gammavalue = 1.0               ! scaling factor for gamma intensity scaling
backprojection = 'No'          ! 'Yes' or 'No'; adds backprojections to output file
Lauemode = 'transmission'      ! 'transmission' or 'reflection'
orientationfile = 'undefined'  ! input file with orientation list 
tiffprefix = 'undefined'       ! prefix for tiff output files with individual patterns
xtalname = 'undefined'         ! structure file name
hdfname = 'undefined'          ! HDF output file name

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=LaueData)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetLaueNameList:',' crystal structure file name is undefined in '//nmlfile)
 end if
 if (trim(hdfname).eq.'undefined') then
  call FatalError('GetLaueNameList:',' output file name is undefined in '//nmlfile)
 end if
 if (trim(orientationfile).eq.'undefined') then
  call FatalError('GetLaueNameList:',' orientation file name is undefined in '//nmlfile)
 end if
end if

lnl%numpx = numpx
lnl%numpy = numpy
lnl%nthreads = nthreads
lnl%pixelsize = pixelsize
lnl%BPx = BPx
lnl%spotw = spotw
lnl%maxVoltage= maxVoltage
lnl%minVoltage= minVoltage
lnl%SDdistance = SDdistance  
lnl%gammavalue = gammavalue
lnl%backprojection = backprojection
lnl%Lauemode = Lauemode
lnl%orientationfile = orientationfile
lnl%xtalname = xtalname
lnl%hdfname = hdfname
lnl%tiffprefix = tiffprefix

end subroutine GetLaueNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetLaueSlitNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill lnl structure (used by EMLaue.f90)
!
!> @param nmlfile namelist file name
!> @param lmnl name list structure
!
!> @date 03/28/19  MDG 1.0 new routine
!> @dete 07/30/19  MDG 1.1 reorganization of namelist
!--------------------------------------------------------------------------
recursive subroutine GetLaueSlitNameList(nmlfile, lnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetLaueSlitNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(LaueSlitNameListType),INTENT(INOUT)      :: lnl
!f2py intent(in,out) ::  lnl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                       :: skipread = .FALSE.

real(kind=dbl)          :: Lw               ! slit width (mm)
real(kind=dbl)          :: Lh               ! slit height (mm)
real(kind=dbl)          :: Lx               ! distance front face of slit to divergent x-ray source (mm)
real(kind=dbl)          :: Ly               ! slit center x position (mm)
real(kind=dbl)          :: Lz               ! slit center y position (mm)
real(kind=dbl)          :: VoltageH         ! highest tube voltage     
real(kind=dbl)          :: VoltageL         ! lowest tube voltage     
real(kind=dbl)          :: Sx               ! distance from source to samplefront (mm)
real(kind=dbl)          :: sampletodetector ! distance sample front to detector face (mm)
real(kind=dbl)          :: samplethickness  ! sample thickness (mm)
real(kind=dbl)          :: ps               ! detector pixel size (mm)
integer(kind=irg)       :: Ny               ! number of detector pixels horizontally
integer(kind=irg)       :: Nz               ! number of detector pixels vertically
real(kind=dbl)          :: DX               ! detector pattern center x coordinate  [mm]
real(kind=dbl)          :: Dy               ! detector pattern center y coordinate  [mm]
real(kind=dbl)          :: Dz               ! detector pattern center z coordinate  [mm]
real(kind=dbl)          :: vs               ! size of the voxels that make up the sample (mm)
real(kind=dbl)          :: absl             ! sample absorption length [mm]
real(kind=dbl)          :: beamstopatf      ! beam stop attenuation factor
real(kind=sgl)          :: spotw
real(kind=sgl)          :: sampletilt
real(kind=sgl)          :: gammavalue
real(kind=dbl)          :: intcutoffratio
integer(kind=irg)       :: BPx
integer(kind=irg)       :: nthreads
logical                 :: binarize
character(1)            :: projectionmode
character(fnlen)        :: backprojection
character(fnlen)        :: orientationfile
character(fnlen)        :: tiffprefix
character(fnlen)        :: hdfname
character(fnlen)        :: xtalname



! define the IO namelist to facilitate passing variables to the program.
namelist  / LaueSlitData / Lw,Lh,Lx,Ly,Lz,VoltageH,VoltageL,Sx,sampletodetector, &
                           samplethickness,ps,Ny,Nz,Dx,Dy,Dz,vs,absl, binarize, sampletilt, &
                           beamstopatf,spotw,BPx,nthreads,backprojection, intcutoffratio, &
                           orientationfile,tiffprefix,hdfname,xtalname, gammavalue, projectionmode

Lw               = 2.D0    ! slit width (mm)
Lh               = 2.D0    ! slit height (mm)
Lx               = 100.D0  ! distance front face of slit to divergent x-ray source (mm)
Ly               = 0.D0    ! slit center x position (mm)
Lz               = 0.D0    ! slit center y position (mm)
VoltageH         = 60.D0   ! highest tube voltage     
VoltageL         = 40.D0   ! lowest tube voltage     
Sx               = 120.D0  ! distance from source to samplefront (mm)
sampletodetector = 120.D0  ! distance sample front to detector face (mm)
samplethickness  = 2.D0    ! sample thickness (mm)
ps               = 0.254D0 ! pixel width (mm)
Ny               = 960     ! number of pixels horizontally
Nz               = 780     ! number of pixels vertically
Dx               = 0.D0    ! pattern center x coordinate 
Dy               = 0.D0    ! pattern center y coordinate 
Dz               = 0.D0    ! pattern center z coordinate 
vs               = 0.10D0  ! size of the voxels that make up the sample (mm)
absl             = 0.5D0   ! absorption length (mm)
beamstopatf      = 0.1D0   ! beam stop attenuation factor
nthreads         = 1       ! number of parallel threads for pattern computation
BPx              = 300     ! semi-edge length for back projection square Lambert maps
spotw            = 0.1     ! spot size weight factor (1/(2*sigma^2))
sampletilt       = 40.D0   ! sample tilt for side-reflection mode 
gammavalue       = 1.0     ! scaling factor for gamma intensity scaling
intcutoffratio   = 0.0001D0! intensity ratio cut off
binarize         = .FALSE.
projectionmode   = 'T'     ! transmission; 'B' for back-reflection, 'S' for side-reflection
backprojection   = 'No'    ! 'Yes' or 'No'; adds backprojections to output file
orientationfile  = 'undefined'  ! input file with orientation list 
tiffprefix       = 'undefined'  ! prefix for tiff output files with individual patterns
xtalname         = 'undefined'  ! structure file name
hdfname          = 'undefined'  ! HDF output file name

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=LaueSlitData)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetLaueNameList:',' crystal structure file name is undefined in '//nmlfile)
 end if
 if (trim(hdfname).eq.'undefined') then
  call FatalError('GetLaueNameList:',' output file name is undefined in '//nmlfile)
 end if
 if (trim(orientationfile).eq.'undefined') then
  call FatalError('GetLaueNameList:',' orientation file name is undefined in '//nmlfile)
 end if
end if

lnl%Lw = Lw               
lnl%Lh = Lh               
lnl%Lx = Lx               
lnl%Ly = Ly               
lnl%Lz = Lz               
lnl%VoltageH = VoltageH         
lnl%VoltageL = VoltageL         
lnl%Sx = Sx               
lnl%sampletodetector = sampletodetector 
lnl%samplethickness  = samplethickness  
lnl%ps = ps               
lnl%Ny = Ny               
lnl%Nz = Nz               
lnl%Dx = Dx               
lnl%Dy = Dy               
lnl%Dz = Dz               
lnl%vs = vs               
lnl%absl = absl             
lnl%beamstopatf = beamstopatf
lnl%spotw = spotw
lnl%sampletilt = sampletilt
lnl%BPx = BPx
lnl%nthreads = nthreads
lnl%intcutoffratio = intcutoffratio
lnl%backprojection = backprojection
lnl%projectionmode = projectionmode
lnl%orientationfile = orientationfile
lnl%tiffprefix = tiffprefix
lnl%hdfname = hdfname
lnl%xtalname = xtalname
lnl%binarize = binarize

end subroutine GetLaueSlitNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetCPLMNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMCPLM.f90)
!
!> @param nmlfile namelist file name
!> @param omnl name list structure
!
!> @date 09/21/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetCPLMNameList(nmlfile, omnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetCPLMNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(CPLMNameListType),INTENT(INOUT)          :: omnl
!f2py intent(in,out) ::  omnl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                       :: skipread = .FALSE.

integer(kind=irg)       :: phinum
integer(kind=irg)       :: numpx
integer(kind=irg)       :: numpy
character(fnlen)        :: masterfile
character(fnlen)        :: anglefile
character(fnlen)        :: outputfile
character(fnlen)        :: tiffprefix

! define the IO namelist to facilitate passing variables to the program.
namelist  / CPLMData / phinum, numpx, numpy, masterfile, outputfile, anglefile, tiffprefix

phinum = 36
numpx = 100
numpy = 100
masterfile = 'undefined'
outputfile = 'undefined'
anglefile = 'undefined'
tiffprefix = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=CPLMdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetCPLMNameList:',' master output file name is undefined in '//nmlfile)
 end if
 if (trim(anglefile).eq.'undefined') then
  call FatalError('GetCPLMNameList:',' angle input file name is undefined in '//nmlfile)
 end if
 if (trim(outputfile).eq.'undefined') then
  call FatalError('GetCPLMNameList:',' intensity output file name is undefined in '//nmlfile)
 end if
end if

omnl%phinum = phinum
omnl%numpx = numpx
omnl%numpy = numpy
omnl%masterfile = masterfile
omnl%outputfile = outputfile
omnl%anglefile = anglefile
omnl%tiffprefix = tiffprefix

end subroutine GetCPLMNameList
!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMCNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMC.f90)
!
!> @param nmlfile namelist file name
!> @param mcnl Monte Carloname list structure
!
!> @date 06/18/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetMCNameList(nmlfile, mcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCNameListType),INTENT(INOUT)      :: mcnl
!f2py intent(in,out) ::  mcnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: num_el
integer(kind=irg)       :: primeseed
integer(kind=irg)       :: nthreads
real(kind=dbl)          :: sig
real(kind=dbl)          :: omega
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
character(4)            :: MCmode
character(fnlen)        :: xtalname
character(fnlen)        :: dataname

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCdata / stdout, xtalname, sig, numsx, num_el, primeseed, EkeV, &
                dataname, nthreads, Ehistmin, Ebinsize, depthmax, depthstep, omega, MCmode

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
numsx = 1501
primeseed = 932117
num_el = 12500000
nthreads = 1
sig = 70.D0
omega = 0.D0
EkeV = 30.D0
Ehistmin = 5.D0
Ebinsize = 0.5D0
depthmax = 100.D0
depthstep = 1.0D0
MCmode = 'CSDA'
xtalname = 'undefined'
dataname = 'MCoutput.data'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=MCdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMMC:',' structure file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the mcnl fields
mcnl%stdout = stdout
mcnl%numsx = numsx
mcnl%primeseed = primeseed
mcnl%num_el = num_el
mcnl%nthreads = nthreads
mcnl%sig = sig
mcnl%omega = omega
mcnl%EkeV = EkeV
mcnl%Ehistmin = Ehistmin
mcnl%Ebinsize = Ebinsize
mcnl%depthmax = depthmax
mcnl%depthstep = depthstep
mcnl%MCmode = MCmode
mcnl%xtalname = xtalname
mcnl%dataname = dataname
mcnl%stdout = stdout

end subroutine GetMCNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMCLIPSSNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMC.f90)
!
!> @param nmlfile namelist file name
!> @param mcnl Monte Carloname list structure
!
!> @date 12/01/15  PGC 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetMCLIPSSNameList(nmlfile, mcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCLIPSSNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCLIPSSNameListType),INTENT(INOUT)      :: mcnl
!f2py intent(in,out) ::  mcnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: num_el
integer(kind=irg)       :: primeseed
integer(kind=irg)       :: nthreads
real(kind=dbl)          :: sig
real(kind=dbl)          :: omega
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
real(kind=dbl)          :: lipssamp ! lipss amplitude
real(kind=dbl)          :: lipsswave ! lipss wavelength
real(kind=dbl)          :: scaled ! scale factor
integer(kind=irg)       :: npx ! array size of trajectory accumulator
integer(kind=irg)       :: vis ! write large trajectory accumulator (0-no, 1-yes)
character(4)            :: MCmode
character(fnlen)        :: xtalname
character(fnlen)        :: dataname

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCdata / stdout, xtalname, sig, numsx, num_el, primeseed, EkeV, &
                dataname, nthreads, Ehistmin, Ebinsize, depthmax, depthstep, &
                lipssamp, lipsswave, scaled, npx, vis, omega, MCmode

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
numsx = 1501
primeseed = 932117
num_el = 12500000
nthreads = 1
sig = 70.D0
omega = 0.D0
EkeV = 30.D0
Ehistmin = 5.D0
Ebinsize = 0.5D0
depthmax = 100.D0
depthstep = 1.0D0
lipssamp = 50.D0
lipsswave = 300.D0
scaled=1.0D7
npx = 500
vis = 0
MCmode = 'CSDA'
xtalname = 'undefined'
dataname = 'MCLIPSSoutput.data'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=MCdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMMCLIPSS:',' structure file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the mcnl fields
mcnl%stdout = stdout
mcnl%numsx = numsx
mcnl%primeseed = primeseed
mcnl%num_el = num_el
mcnl%nthreads = nthreads
mcnl%sig = sig
mcnl%omega = omega
mcnl%EkeV = EkeV
mcnl%Ehistmin = Ehistmin
mcnl%Ebinsize = Ebinsize
mcnl%depthmax = depthmax
mcnl%depthstep = depthstep
mcnl%MCmode = MCmode
mcnl%xtalname = xtalname
mcnl%dataname = dataname
mcnl%stdout = stdout
mcnl%lipssamp = lipssamp
mcnl%lipsswave = lipsswave
mcnl%scaled = scaled
mcnl%npx = npx
mcnl%vis = vis

end subroutine GetMCLIPSSNameList




!--------------------------------------------------------------------------
!
! SUBROUTINE:GetConvertOrientationsNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMConvertOrientations.f90 program)
!
!> @param nmlfile namelist file name
!> @param enl name list structure
!
!> @date 01/31/17 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetConvertOrientationsNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetConvertOrientationsNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(ConvertOrientationsNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.
integer(kind=irg)                       :: cnt

integer(kind=irg)       :: reducetoRFZ
character(fnlen)        :: xtalname
character(fnlen)        :: cubochoric
character(fnlen)        :: homochoric
character(fnlen)        :: rodrigues
character(fnlen)        :: stereographic
character(fnlen)        :: eulerangles
character(fnlen)        :: axisangle
character(fnlen)        :: quaternion
character(fnlen)        :: rotationmatrix
character(fnlen)        :: anglefile

! define the IO namelist to facilitate passing variables to the program.
namelist  / EMConvertOrientations / cubochoric, homochoric, rodrigues, stereographic, eulerangles, &
                              axisangle, quaternion, rotationmatrix, xtalname, anglefile, reducetoRFZ

! initialize
reducetoRFZ = 1
cubochoric = 'undefined'
homochoric = 'undefined'
rodrigues = 'undefined'
stereographic = 'undefined'
eulerangles = 'undefined'
axisangle = 'undefined'
quaternion = 'undefined'
rotationmatrix = 'undefined'
xtalname = 'undefined'
anglefile = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EMConvertOrientations)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetConvertOrientationsNameList:',' structure file name is undefined in '//nmlfile)
 end if
 if (trim(anglefile).eq.'undefined') then
  call FatalError('GetConvertOrientationsNameList:',' angle file name is undefined in '//nmlfile)
 end if
! at least one of the output file strings must be different from 'undefined'
 cnt = 0
 if (cubochoric.eq.'undefined') cnt = cnt+1
 if (homochoric.eq.'undefined') cnt = cnt+1
 if (rodrigues.eq.'undefined') cnt = cnt+1
 if (stereographic.eq.'undefined') cnt = cnt+1
 if (eulerangles.eq.'undefined') cnt = cnt+1
 if (quaternion.eq.'undefined') cnt = cnt+1
 if (axisangle.eq.'undefined') cnt = cnt+1
 if (rotationmatrix.eq.'undefined') cnt = cnt+1
 if (cnt.eq.8) then
   call FatalError('GetConvertOrientationsNameList',' at least one output file name must be defined in '//nmlfile)
 end if
end if

enl%reducetoRFZ = reducetoRFZ
enl%cubochoric = trim(cubochoric)
enl%homochoric = trim(homochoric)
enl%rodrigues = trim(rodrigues)
enl%stereographic = trim(stereographic)
enl%eulerangles = trim(eulerangles)
enl%axisangle = trim(axisangle)
enl%quaternion = trim(quaternion)
enl%rotationmatrix = trim(rotationmatrix)
enl%xtalname = trim(xtalname)
enl%anglefile = trim(anglefile)

end subroutine GetConvertOrientationsNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetOrientationVizNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMOrientationViz.f90 program)
!
!> @param nmlfile namelist file name
!> @param enl name list structure
!
!> @date 01/30/17 MDG 1.0 new routine
!> @date 06/13/17 MDG 1.1 added support for .mrc files
!--------------------------------------------------------------------------
recursive subroutine GetOrientationVizNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOrientationVizNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(OrientationVizNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: cubochoric
integer(kind=irg)       :: homochoric
integer(kind=irg)       :: rodrigues
integer(kind=irg)       :: stereographic
integer(kind=irg)       :: eulerspace
integer(kind=irg)       :: reducetoRFZ
integer(kind=irg)       :: nx
integer(kind=irg)       :: ny
integer(kind=irg)       :: nz
integer(kind=irg)       :: overridepgnum
integer(kind=irg)       :: MacKenzieCell
real(kind=sgl)          :: rgb(3)
real(kind=sgl)          :: sphrad
real(kind=sgl)          :: distance
character(3)            :: scalingmode
character(3)            :: mrcmode
character(fnlen)        :: df3file
character(fnlen)        :: mrcfile
character(fnlen)        :: framemrcfile
character(fnlen)        :: xtalname
character(fnlen)        :: povrayfile
character(fnlen)        :: anglefile

! define the IO namelist to facilitate passing variables to the program.
namelist  / EMOrientationViz / cubochoric, homochoric, rodrigues, stereographic, eulerspace, &
                              xtalname, povrayfile, anglefile, reducetoRFZ, rgb, sphrad, df3file, &
                              mrcfile, framemrcfile, mrcmode, &
                              nx, ny, nz, distance, scalingmode, overridepgnum, MacKenzieCell

! initialize
cubochoric = 0
homochoric = 0
rodrigues = 0
stereographic = 0
eulerspace = 0
reducetoRFZ = 1
overridepgnum = 0
MacKenzieCell = 0
rgb = (/ 0.0, 0.0, 1.0 /)
sphrad = 0.015
distance = 4.0
nx = 64
ny = 64
nz = 64
scalingmode = 'lev'   ! or 'log' or 'lev' (for equi-level contours)
mrcmode = 'off'       ! 'off', 'reg', 'frm'
df3file = 'undefined'
mrcfile = 'undefined'
framemrcfile = 'undefined'
xtalname = 'undefined'
povrayfile = 'undefined'
anglefile = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EMOrientationViz)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMOrientationViz:',' structure file name is undefined in '//nmlfile)
 end if
 if (mrcmode.eq.'off') then
   if (trim(povrayfile).eq.'undefined') then
    call FatalError('EMOrientationViz:',' povray file name is undefined in '//nmlfile)
   end if
 else
  if (mrcmode.eq.'reg') then
   if (trim(mrcfile).eq.'undefined') then
    call FatalError('EMOrientationViz:',' mrc file name is undefined in '//nmlfile)
   end if
  else
   if (trim(framemrcfile).eq.'undefined') then
    call FatalError('EMOrientationViz:',' frame mrc file name is undefined in '//nmlfile)
   end if
  end if
 end if
 if (trim(anglefile).eq.'undefined') then
  call FatalError('EMOrientationViz:',' angle file name is undefined in '//nmlfile)
 end if
end if

enl%cubochoric = cubochoric
enl%homochoric = homochoric
enl%rodrigues = rodrigues
enl%stereographic = stereographic
enl%eulerspace = eulerspace
enl%reducetoRFZ = reducetoRFZ
enl%nx = nx
enl%ny = ny
enl%nz = nz
enl%overridepgnum = overridepgnum
enl%MacKenzieCell = MacKenzieCell
enl%rgb = rgb
enl%sphrad = sphrad
enl%distance = distance
enl%scalingmode = scalingmode
enl%mrcmode = mrcmode
enl%df3file = df3file
enl%mrcfile = mrcfile
enl%framemrcfile = framemrcfile
enl%xtalname = trim(xtalname)
enl%povrayfile = trim(povrayfile)
enl%anglefile = trim(anglefile)

end subroutine GetOrientationVizNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMCCLNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMCCL.f90 and other programs)
!
!> @param nmlfile namelist file name
!> @param mcnl Monte Carloname list structure
!
!> @date 06/18/14  SS 1.0 new routine
!> @date 09/09/15 MDG 1.1 added devid (GPU device id)
!> @date 11/10/19 MDG 1.2 added interaction volume parameters
!--------------------------------------------------------------------------
recursive subroutine GetMCCLNameList(nmlfile, mcnl, initonly, writetofile)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCCLNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
!f2py intent(in,out) ::  mcnl
logical,OPTIONAL,INTENT(IN)             :: initonly
character(fnlen),INTENT(IN),optional    :: writetofile

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: ivolx 
integer(kind=irg)       :: ivoly 
integer(kind=irg)       :: ivolz 
integer(kind=irg)       :: globalworkgrpsz
integer(kind=irg)       :: num_el
integer(kind=irg)       :: totnum_el
integer(kind=irg)       :: multiplier
integer(kind=irg)       :: devid
integer(kind=irg)       :: platid
real(kind=sgl)          :: ivolstepx 
real(kind=sgl)          :: ivolstepy 
real(kind=sgl)          :: ivolstepz 
real(kind=dbl)          :: sig
real(kind=dbl)          :: sigstart
real(kind=dbl)          :: sigend
real(kind=dbl)          :: sigstep
real(kind=dbl)          :: omega
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
character(3)            :: Notify
character(4)            :: MCmode
character(fnlen)        :: xtalname
character(fnlen)        :: dataname
character(fnlen)        :: mode

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCCLdata / stdout, xtalname, sigstart, numsx, num_el, globalworkgrpsz, EkeV, multiplier, &
dataname, totnum_el, Ehistmin, Ebinsize, depthmax, depthstep, omega, MCmode, mode, devid, platid, &
sigend, sigstep, sig, Notify, ivolx, ivoly, ivolz, ivolstepx, ivolstepy, ivolstepz

if (present(writetofile)) then
  if (trim(writetofile).ne.'') then 
    xtalname = trim(mcnl%xtalname)
    mode = mcnl%mode
    ivolx = mcnl%ivolx
    ivoly = mcnl%ivoly
    ivolz = mcnl%ivolz
    ivolstepx = mcnl%ivolstepx
    ivolstepy = mcnl%ivolstepy
    ivolstepz = mcnl%ivolstepz
    globalworkgrpsz = mcnl%globalworkgrpsz
    num_el = mcnl%num_el
    totnum_el = mcnl%totnum_el 
    multiplier = mcnl%multiplier
    devid = mcnl%devid 
    platid = mcnl%platid
    sig = mcnl%sig  
    omega = mcnl%omega
    EkeV = mcnl%EkeV 
    Ehistmin = mcnl%Ehistmin
    Ebinsize = mcnl%Ebinsize
    dataname = mcnl%dataname

    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='unknown')
    write(UNIT=dataunit,NML=MCCLdata)
    close(UNIT=dataunit,STATUS='keep')
    return 
  end if 
end if 

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
ivolx = 1001
ivoly = 1001
ivolz = 101
numsx = 1501
globalworkgrpsz = 100
num_el = 10
totnum_el = 2000000000
multiplier = 1
devid = 1
platid = 1
ivolstepx = 1.0
ivolstepy = 1.0
ivolstepz = 1.0
sig = 70.D0
sigstart = 70.D0
sigend = 70.D0
sigstep = 1.D0
omega = 0.D0
EkeV = 30.D0
Ehistmin = 5.D0
Ebinsize = 0.5D0
depthmax = 100.D0
depthstep = 1.0D0
Notify = 'Off'
MCmode = 'CSDA'
xtalname = 'undefined'
dataname = 'MCoutput.data'
mode = 'full'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if
write(*,*)EMsoft_toNativePath(nmlfile)
if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=MCCLdata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
call FatalError('EMMC:',' structure file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the mcnl fields
mcnl%stdout = stdout
mcnl%numsx = numsx
mcnl%ivolx = ivolx
mcnl%ivoly = ivoly 
mcnl%ivolz = ivolz
mcnl%globalworkgrpsz = globalworkgrpsz
mcnl%num_el = num_el
mcnl%totnum_el = totnum_el
mcnl%multiplier = multiplier
mcnl%devid = devid
mcnl%platid = platid
mcnl%ivolstepx = ivolstepx 
mcnl%ivolstepy = ivolstepy
mcnl%ivolstepz = ivolstepz
mcnl%sigstart = sigstart
mcnl%sigend = sigend
mcnl%sigstep = sigstep
mcnl%sig = sig
mcnl%omega = omega
mcnl%EkeV = EkeV
mcnl%Ehistmin = Ehistmin
mcnl%Ebinsize = Ebinsize
mcnl%depthmax = depthmax
mcnl%depthstep = depthstep
mcnl%Notify= Notify
mcnl%MCmode = MCmode
mcnl%xtalname = xtalname
mcnl%dataname = dataname
mcnl%mode = mode

end subroutine GetMCCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMCCLMultiLayerNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMCCL.f90)
!
!> @param nmlfile namelist file name
!> @param mcnl Monte Carloname list structure
!
!> @date 06/18/14  SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetMCCLMultiLayerNameList(nmlfile, mcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCCLMultiLayerNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCCLMultiLayerNameListType),INTENT(INOUT)      :: mcnl
!f2py intent(in,out) ::  mcnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: globalworkgrpsz
integer(kind=irg)       :: num_el
integer(kind=irg)       :: totnum_el
real(kind=dbl)          :: sig
real(kind=dbl)          :: omega
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
real(kind=dbl)          :: filmthickness
real(kind=dbl)          :: filmstep
character(4)            :: MCmode
character(fnlen)        :: xtalname_film
character(fnlen)        :: xtalname_subs
character(fnlen)        :: dataname
character(fnlen)        :: mode

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCCLdata / stdout, sig, numsx, num_el, globalworkgrpsz, EkeV, &
        dataname, totnum_el, Ehistmin, Ebinsize, depthmax, &
        depthstep, omega, MCmode, mode, xtalname_film, xtalname_subs, &
        filmthickness, filmstep


! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
numsx = 1501
globalworkgrpsz = 100
num_el = 10
totnum_el = 100000
sig = 70.D0
omega = 0.D0
EkeV = 30.D0
Ehistmin = 5.D0
Ebinsize = 0.5D0
depthmax = 100.D0
depthstep = 1.0D0
MCmode = 'CSDA'
xtalname_film = 'undefined'
xtalname_subs = 'undefined'
dataname = 'MCoutput.data'
mode = 'full'
filmthickness = 20.D0
filmstep = 2.D0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=MCCLdata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if ((trim(xtalname_film).eq.'undefined') .or. (trim(xtalname_subs).eq.'undefined')) then
call FatalError('EMMC:',' structure file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the mcnl fields
mcnl%stdout = stdout
mcnl%numsx = numsx
mcnl%globalworkgrpsz = globalworkgrpsz
mcnl%num_el = num_el
mcnl%totnum_el = totnum_el
mcnl%sig = sig
mcnl%omega = omega
mcnl%EkeV = EkeV
mcnl%Ehistmin = Ehistmin
mcnl%Ebinsize = Ebinsize
mcnl%depthmax = depthmax
mcnl%depthstep = depthstep
mcnl%MCmode = MCmode
mcnl%xtalname_film = xtalname_film
mcnl%xtalname_subs = xtalname_subs
mcnl%dataname = dataname
mcnl%mode = mode
mcnl%filmthickness = filmthickness
mcnl%filmstep = filmstep

end subroutine GetMCCLMultiLayerNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetDisorientationsNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMDisorientations.f90)
!
!> @param nmlfile namelist file name
!> @param emnl name list structure
!
!> @date 06/24/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetDisorientationsNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDisorientationsNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(DisorientationsNameListType),INTENT(INOUT)      :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: pgnum
integer(kind=irg)       :: pgnum2
character(fnlen)        :: inputfile
character(fnlen)        :: outputfile

! define the IO namelist to facilitate passing variables to the program.
namelist /Disorientations/ pgnum, pgnum2, inputfile, outputfile

! set the input parameters to default values 
pgnum = 32                  ! 
pgnum2 = 32                 ! 
inputfile = 'undefined'     ! default filename for input file
outputfile = 'undefined'    ! default filename for input file

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=Disorientations)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(inputfile).eq.'undefined') then
  call FatalError(' EMDisorientations',' input file name is undefined in '//nmlfile)
 end if

 if (trim(outputfile).eq.'undefined') then
  call FatalError(' EMDisorientations',' output file name is undefined in '//nmlfile)
 end if

end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%pgnum = pgnum
emnl%pgnum2 = pgnum2
emnl%inputfile = inputfile
emnl%outputfile = outputfile

end subroutine GetDisorientationsNameList



!--------------------------------------------------------------------------
!
! SUBROUTINE:GetAverageOrientationNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMAverageOrient.f90)
!
!> @param nmlfile namelist file name
!> @param emnl name list structure
!
!> @date 06/24/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetAverageOrientationNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetAverageOrientationNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(AverageOrientationNameListType),INTENT(INOUT)      :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: nmuse
integer(kind=irg)       :: reldisx
integer(kind=irg)       :: reldisy
character(1)            :: refined
character(fnlen)        :: dotproductfile
character(fnlen)        :: averagectffile
character(fnlen)        :: averagetxtfile
character(fnlen)        :: disorientationmap

! define the IO namelist to facilitate passing variables to the program.
namelist /AverageOrientation/ nmuse, dotproductfile, averagectffile, averagetxtfile, &
                              reldisx, reldisy, disorientationmap, refined

! set the input parameters to default values (except for xtalname, which must be present)
nmuse = 10                      ! number of near-matches to use
reldisx = 0                     ! x-coordinate for relative disorientation map
reldisy = 0                     ! y-coordinate for relative disorientation map
refined = 'n'                   ! which Euler angle set to be used for disorientation map ...
dotproductfile = 'undefined'    ! default filename for input dotproduct file (HDF5)
averagectffile = 'undefined'    ! default filename for output ctf file
averagetxtfile = 'undefined'    ! default filename for output txt file (only with oldformat=.TRUE.
disorientationmap = 'undefined' ! default filename for relative disorientation map file


if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=AverageOrientation)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError(' EMAverageOrient',' dotproduct file name is undefined in '//nmlfile)
 end if

 if (trim(averagectffile).eq.'undefined') then
  call FatalError(' EMAverageOrient',' ctf output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%nmuse = nmuse
emnl%reldisx = reldisx
emnl%reldisy = reldisy
emnl%refined = refined
emnl%dotproductfile = dotproductfile
emnl%averagectffile = averagectffile
emnl%averagetxtfile = averagetxtfile
emnl%disorientationmap = disorientationmap

end subroutine GetAverageOrientationNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetOrientationSimilarityNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMOrientationSimilarity.f90)
!
!> @param nmlfile namelist file name
!> @param emnl name list structure
!
!> @date 07/29/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetOrientationSimilarityNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOrientationSimilarityNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(OrientationSimilarityNameListType),INTENT(INOUT)      :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: nmuse
character(fnlen)        :: dotproductfile
character(fnlen)        :: osmtiff

! define the IO namelist to facilitate passing variables to the program.
namelist /OrientationSimilarity/ nmuse, dotproductfile, osmtiff

! set the input parameters to default values (except for xtalname, which must be present)
nmuse = 10                      ! number of near-matches to use
dotproductfile = 'undefined'    ! default filename for input dotproduct file (HDF5)
osmtiff = 'undefined'    ! default filename for input dotproduct file (HDF5)

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=OrientationSimilarity)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError(' EMOrientationSimilarity',' dotproduct file name is undefined in '//nmlfile)
 end if

 if (trim(osmtiff).eq.'undefined') then
  call FatalError(' EMOrientationSimilarity',' osm.tiff output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%nmuse = nmuse
emnl%dotproductfile = dotproductfile
emnl%osmtiff = osmtiff

end subroutine GetOrientationSimilarityNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetKAMNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMOrientationSimilarity.f90)
!
!> @param nmlfile namelist file name
!> @param emnl name list structure
!
!> @date 07/29/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetKAMNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetKAMNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(KAMNameListType),INTENT(INOUT)     :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

real(kind=sgl)          :: kamcutoff
integer(kind=irg)       :: orav
character(fnlen)        :: dotproductfile
character(fnlen)        :: kamtiff

! define the IO namelist to facilitate passing variables to the program.
namelist /KAM/ kamcutoff, orav, dotproductfile, kamtiff

! set the input parameters to default values (except for xtalname, which must be present)
kamcutoff = 5.0                 ! number of near-matches to use
orav = 0                        ! perform orientation average first ?
dotproductfile = 'undefined'    ! default filename for input dotproduct file (HDF5)
kamtiff = 'undefined'    ! default filename for input dotproduct file (HDF5)

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=KAM)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dotproductfile).eq.'undefined') then
  call FatalError(' EMKAM',' dotproduct file name is undefined in '//nmlfile)
 end if

 if (trim(kamtiff).eq.'undefined') then
  call FatalError(' EMKAM',' kam.tiff output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%kamcutoff = kamcutoff
emnl%orav = orav
emnl%dotproductfile = dotproductfile
emnl%kamtiff = kamtiff

end subroutine GetKAMNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetDvsDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMOrientationSimilarity.f90)
!
!> @param nmlfile namelist file name
!> @param emnl name list structure
!
!> @date 07/29/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetDvsDNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDvsDNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(DvsDNameListType),INTENT(INOUT)    :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: nmuse
real(kind=sgl)          :: maxdis
real(kind=sgl)          :: minang
real(kind=sgl)          :: maxang
character(fnlen)        :: dotproductfile
character(fnlen)        :: outfile
character(fnlen)        :: povfile
character(fnlen)        :: xtalfile

! define the IO namelist to facilitate passing variables to the program.
namelist /DvsD/ nmuse, maxdis, minang, maxang, dotproductfile, outfile, povfile, xtalfile

! set the input parameters to default values (except for xtalname, which must be present)
nmuse = 10
maxdis = 3.0
minang = 10.0
maxang = 20.0
dotproductfile = 'undefined'    ! default filename for input dotproduct file (HDF5)
outfile = 'undefined'    ! default filename for output file (txt)
povfile = 'undefined'    ! default filename for output file (txt)
xtalfile = 'undefined'    ! default filename for output file (txt)

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=DvsD)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalfile).eq.'undefined') then
  call FatalError(' EMDvsD',' xtal file name is undefined in '//nmlfile)
 end if

 if (trim(dotproductfile).eq.'undefined') then
  call FatalError(' EMDvsD',' dotproduct file name is undefined in '//nmlfile)
 end if

 if (trim(outfile).eq.'undefined') then
  call FatalError(' EMDvsD',' outfile output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%nmuse = nmuse
emnl%maxdis = maxdis
emnl%minang = minang
emnl%maxang = maxang
emnl%dotproductfile = dotproductfile
emnl%outfile = outfile
emnl%povfile = povfile
emnl%xtalfile = xtalfile

end subroutine GetDvsDNameList



!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMEBSDmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl EBSD master name list structure
!
!> @date 06/19/14  MDG 1.0 new routine
!> @date 10/21/19  MDG 2.0 adds support for .sht file format
!> @date 10/25/19  MDG 2.1 remove .sht support; moved to new GetEBSDMasterSHTNameList routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDMasterNameListType),INTENT(INOUT)      :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: npx
integer(kind=irg)       :: Esel
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
character(3)            :: Notify
character(fnlen)        :: copyfromenergyfile
character(fnlen)        :: energyfile
character(fnlen)        :: BetheParametersFile
character(fnlen)        :: h5copypath
logical                 :: useEnergyWeighting
logical                 :: combinesites
logical                 :: restart
logical                 :: uniform

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDmastervars/ dmin,npx,nthreads,copyfromenergyfile,energyfile,Esel,restart,uniform,Notify, &
                          combinesites, h5copypath, BetheParametersFile, stdout, useEnergyWeighting

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
npx = 500                       ! Nx pixels (total = 2Nx+1)
nthreads = 1
Esel = -1                       ! selected energy value for single energy run
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
Notify = 'Off'
copyfromenergyfile = 'undefined'! default filename for z_0(E_e) data from a different Monte Carlo simulation
h5copypath = 'undefined'
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
BetheParametersFile='BetheParameters.nml'
useEnergyWeighting = .FALSE.    ! use the Monte Carlo depth histogram to scale the slice intensities (EMEBSDdepthmaster program)
combinesites = .FALSE.          ! combine all atom sites into one BSE yield or not
restart = .FALSE.               ! when .TRUE. an existing file will be assumed 
uniform = .FALSE.               ! when .TRUE., the output master patterns will contain 1.0 everywhere

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDmastervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(energyfile).eq.'undefined') then
  call FatalError('GetEBSDMasterNameList:',' output (energy) file name is undefined in '//nmlfile)
 end if

end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%stdout = stdout
emnl%npx = npx
emnl%Esel = Esel
emnl%nthreads = nthreads
emnl%dmin = dmin
emnl%copyfromenergyfile = copyfromenergyfile
emnl%h5copypath = h5copypath
emnl%energyfile = energyfile
emnl%BetheParametersFile = BetheParametersFile
emnl%Notify = Notify
emnl%outname = energyfile       ! as off release 3.1, outname must be the same as energyfile
emnl%useEnergyWeighting = useEnergyWeighting
emnl%combinesites = combinesites
emnl%restart = restart
emnl%uniform = uniform

end subroutine GetEBSDMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetISEMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMISEmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl ISE master name list structure
!
!> @date 12/18/20  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetISEMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetISEMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(ISEMasterNameListType),INTENT(INOUT)       :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: npx
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: iscale(3)
character(3)            :: Notify
character(fnlen)        :: outname
character(fnlen)        :: tiffname
character(fnlen)        :: xtalname


! define the IO namelist to facilitate passing variables to the program.
namelist /ISEmastervars/ npx,nthreads,Notify,xtalname,outname,iscale,tiffname 

! set the input parameters to default values (except for xtalname, which must be present)
npx = 500
nthreads = 1
iscale = (/ 3.0, 4.0, 1.0 /)
Notify = 'Off'
tiffname = 'undefined'
outname = 'undefined'
xtalname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=ISEmastervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetISEMasterNameList:',' crystal structure file name is undefined in '//nmlfile)
 end if

 if (trim(outname).eq.'undefined') then
  call FatalError('GetISEMasterNameList:',' output file name is undefined in '//nmlfile)
 end if

  if (trim(tiffname).eq.'undefined') then
  call FatalError('GetISEMasterNameList:',' tiff file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%npx = npx
emnl%nthreads = nthreads
emnl%iscale = iscale
emnl%Notify = Notify
emnl%outname= outname
emnl%tiffname= tiffname
emnl%xtalname = xtalname

end subroutine GetISEMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEECMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill emnl structure (used by EMEECmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl EEC master name list structure
!
!> @date 12/13/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEECMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEECMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EECMasterNameListType),INTENT(INOUT)       :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: npx
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
character(3)            :: Notify
character(fnlen)        :: mpfile
character(fnlen)        :: xtalname
character(fnlen)        :: BetheParametersFile
real(kind=sgl)          :: IsotopeSite(3)        
real(kind=sgl)          :: IsotopeEnergy
real(kind=sgl)          :: mfp

! define the IO namelist to facilitate passing variables to the program.
namelist /EECmastervars/ dmin,npx,nthreads,Notify, BetheParametersFile,mpfile, IsotopeSite, IsotopeEnergy, mfp, xtalname 

! set the input parameters to default values (except for xtalname, which must be present)
npx = 500                       ! Nx pixels (total = 2Nx+1)
nthreads = 1
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
Notify = 'Off'
mpfile = 'undefined'            ! default output HDF5 file name
xtalname = 'undefined'          ! default xtal file name
BetheParametersFile='BetheParameters.nml'
IsotopeSite = (/ 0.0, 0.0, 0.0 /)
IsotopeEnergy = 100.0           ! keV (emitted electron energy)
mfp = 50.0                      ! mean free path [nm] for depth integration

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EECmastervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(mpfile).eq.'undefined') then
  call FatalError('GetEECMasterNameList:',' output file name is undefined in '//nmlfile)
 end if

 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetEECMasterNameList:',' xtalname is undefined in '//nmlfile)
 end if

end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%npx = npx
emnl%nthreads = nthreads
emnl%dmin = dmin
emnl%mpfile = mpfile
emnl%xtalname = xtalname
emnl%BetheParametersFile = BetheParametersFile
emnl%Notify = Notify
emnl%IsotopeEnergy = IsotopeEnergy
emnl%IsotopeSite = IsotopeSite
emnl%mfp = mfp

end subroutine GetEECMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDMasterSHTNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMEBSDmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl EBSD master name list structure
!
!> @date 06/19/14  MDG 1.0 new routine
!> @date 10/21/19  MDG 2.0 adds support for .sht file format
!--------------------------------------------------------------------------
recursive subroutine GetEBSDMasterSHTNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDMasterSHTNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDMasterSHTNameListType),INTENT(INOUT)   :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
character(3)            :: Notify
character(fnlen)        :: energyfile
character(fnlen)        :: BetheParametersFile
character(fnlen)        :: SHT_folder
character(fnlen)        :: SHT_formula
character(fnlen)        :: SHT_name
character(fnlen)        :: SHT_structuresymbol
character(fnlen)        :: addtoKiltHub
character(fnlen)        :: useDOI
logical                 :: combinesites

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDmasterSHTvars/ dmin,nthreads,energyfile,Notify, &
                             combinesites, BetheParametersFile, addtoKiltHub, &
                             useDOI, SHT_formula, SHT_name, SHT_structuresymbol, SHT_folder

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
nthreads = 1
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
Notify = 'Off'
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
BetheParametersFile='BetheParameters.nml'
SHT_folder = 'undefined'        ! folder to store SHT files, relative to EMDatapathname
SHT_formula = 'undefined'       ! compound chemical formula, e.g., SiO2
SHT_name = 'undefined'          ! compund name (e.g., forsterite)
SHT_structuresymbol = 'undefined' ! StrukturBericht symbol (e.g., D0_22) or Pearson symbol (e.g., hP12), or ...
addtoKiltHub = 'No'             ! file to be added to data base on kilthub.cmu.edu ?
useDOI = 'undefined'            ! if no DOI is entered, then we use the Zenodo DOI for the .sht repository
combinesites = .FALSE.          ! combine all atom sites into one BSE yield or not

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDmasterSHTvars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(energyfile).eq.'undefined') then
  call FatalError('GetEBSDMasterSHTNameList:',' output (energy) file name is undefined in '//nmlfile)
 end if

! for Legendre mode, the SHT_formula parameter MUST be present 
 if (trim(SHT_formula).eq.'undefined') then 
  call FatalError('GetEBSDMasterSHTNameList:',' SHT_formula must be defined in '//nmlfile)
 end if

 if (trim(SHT_folder).eq.'undefined') then 
  call FatalError('GetEBSDMasterSHTNameList:',' SHT_folder must be defined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%stdout = stdout
emnl%nthreads = nthreads
emnl%dmin = dmin
emnl%energyfile = energyfile
emnl%BetheParametersFile = BetheParametersFile
emnl%Notify = Notify
emnl%addtoKiltHub = addtoKiltHub
emnl%useDOI = useDOI
emnl%combinesites = combinesites
emnl%SHT_formula = SHT_formula
emnl%SHT_name = SHT_name
emnl%SHT_structuresymbol = SHT_structuresymbol
emnl%SHT_folder = trim(SHT_folder)

end subroutine GetEBSDMasterSHTNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDSingleMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill emnl structure (used by EMEBSDsinglemaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl EBSD master name list structure
!
!> @date 11/13/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDSingleMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDSingleMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDSingleMasterNameListType),INTENT(INOUT):: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: npx
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
real(kind=sgl)          :: kV
real(kind=sgl)          :: tstep
character(3)            :: Notify
character(fnlen)        :: outname
character(fnlen)        :: xtalname
logical                 :: combinesites

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDsinglemastervars/ dmin,npx,nthreads,kV,tstep,xtalname,Notify,outname,combinesites

! set the input parameters to default values (except for xtalname, which must be present)
npx = 500                       ! Nx pixels (total = 2Nx+1)
nthreads = 1
kV = 20.0                       ! microscope accelerating voltage
dmin = 0.05                     ! smallest d-spacing to include in dynamical matrix [nm]
tstep = 0.5                     ! thickness increment in nm
Notify = 'Off'
combinesites = .TRUE.           ! combine all atom sites into one BSE yield or not
xtalname = 'undefined'
outname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDsinglemastervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMEBSDsinglemaster:',' xtal file name is undefined in '//nmlfile)
 end if

 if (trim(outname).eq.'undefined') then
  call FatalError('EMEBSDsinglemaster:',' output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%npx = npx
emnl%kV = kV
emnl%nthreads = nthreads
emnl%dmin = dmin
emnl%tstep = tstep
emnl%xtalname = trim(xtalname)
emnl%Notify = Notify
emnl%outname = trim(outname)
emnl%combinesites = combinesites

end subroutine GetEBSDSingleMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetlocalOSMMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill emnl structure (used by EMgetlocalOSM.f90)
!
!> @param nmlfile namelist file name
!> @param emnl name list structure
!
!> @date 10/22/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetlocalOSMMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetlocalOSMMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(localOSMNameListType),INTENT(INOUT)        :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: numnm
character(fnlen)        :: dpfile
character(fnlen)        :: outfile
character(fnlen)        :: tracemapfile
character(fnlen)        :: determinantmapfile

! define the IO namelist to facilitate passing variables to the program.
namelist /localOSMvars/ numnm, dpfile, outfile, tracemapfile, determinantmapfile 

! set the input parameters to default values (except for xtalname, which must be present)
numnm = 20                      ! number of near matches to use for maps 
dpfile = 'undefined'
outfile = 'undefined'
tracemapfile = 'undefined'
determinantmapfile = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=localOSMvars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(dpfile).eq.'undefined') then
  call FatalError('GetlocalOSMMasterNameList:',' xtal file name is undefined in '//nmlfile)
 end if

 if (trim(outfile).eq.'undefined') then
  call FatalError('GetlocalOSMMasterNameList:',' output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%numnm = numnm
emnl%dpfile = trim(dpfile)
emnl%outfile = trim(outfile)
emnl%tracemapfile = trim(tracemapfile)
emnl%determinantmapfile = trim(determinantmapfile)

end subroutine GetlocalOSMMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetTKDMasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMTKDmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl TKD master name list structure
!
!> @date 01/16/17  MDG 1.0 new routine
!> @date 11/06/17  MDG 1.1 added combinesites parameter
!--------------------------------------------------------------------------
recursive subroutine GetTKDMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetTKDMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(TKDMasterNameListType),INTENT(INOUT)       :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: npx
integer(kind=irg)       :: Esel
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
character(fnlen)        :: energyfile
logical                 :: combinesites
logical                 :: restart
logical                 :: uniform

! define the IO namelist to facilitate passing variables to the program.
namelist /TKDmastervars/ dmin,npx,nthreads,energyfile,Esel,restart,uniform,combinesites

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
npx = 500                       ! Nx pixels (total = 2Nx+1)
nthreads = 1
Esel = -1                       ! selected energy value for single energy run
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
combinesites = .FALSE.          ! keep asymmetric unit sites separate or not
restart = .FALSE.               ! when .TRUE. an existing file will be assumed 
uniform = .FALSE.               ! when .TRUE., the output master patterns will contain 1.0 everywhere

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=TKDmastervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(energyfile).eq.'undefined') then
  call FatalError('EMTKDmaster:',' energy file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%stdout = stdout
emnl%npx = npx
emnl%Esel = Esel
emnl%nthreads = nthreads
emnl%dmin = dmin
emnl%energyfile = energyfile
emnl%outname = energyfile       ! as off release 3.1, outname must be the same as energyfile
emnl%combinesites = combinesites
emnl%restart = restart
emnl%uniform = uniform

end subroutine GetTKDMasterNameList



!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDMasterOpenCLNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMEBSDmasterOpenCL.f90)
!
!> @param nmlfile namelist file name
!> @param emnl EBSD master name list structure
!
!> @date 12/10/16  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDMasterOpenCLNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDMasterOpenCLNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDMasterOpenCLNameListType),INTENT(INOUT):: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: npx
integer(kind=irg)       :: Esel
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: platid
integer(kind=irg)       :: devid
integer(kind=irg)       :: globalworkgrpsz
real(kind=sgl)          :: dmin
character(fnlen)        :: energyfile
logical                 :: restart
logical                 :: uniform

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDmastervars/ stdout,dmin,npx,platid,devid,globalworkgrpsz,energyfile,restart,uniform,Esel,nthreads

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
npx = 500                       ! Nx pixels (total = 2Nx+1)
nthreads = 1
platid = 1
devid = 1
globalworkgrpsz = 150
Esel = -1                       ! selected energy value for single energy run
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
restart = .FALSE.               ! when .TRUE. an existing file will be assumed 
uniform = .FALSE.               ! when .TRUE., the output master patterns will contain 1.0 everywhere

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDmastervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(energyfile).eq.'undefined') then
  call FatalError('EMEBSDmaster:',' energy file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%stdout = stdout
emnl%npx = npx
emnl%Esel = Esel
emnl%nthreads = nthreads
emnl%platid = platid 
emnl%devid = devid 
emnl%globalworkgrpsz = globalworkgrpsz 
emnl%dmin = dmin
emnl%energyfile = energyfile
emnl%restart = restart
emnl%uniform = uniform

end subroutine GetEBSDMasterOpenCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDclusterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill emnl structure (used by EMEBSDcluster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl EBSD master name list structure
!
!> @date 06/19/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDclusterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDclusterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDclusterNameListType),INTENT(INOUT)     :: emnl
!f2py intent(in,out) ::  emnl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: NClusters, NIterations, binfactor
character(fnlen)        :: inputfilename, groupname, datasetname

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDclustervars/ NClusters, NIterations, binfactor, inputfilename, groupname, datasetname

! set the input parameters to default values (except for xtalname, which must be present)
NClusters = 100                 ! initial number of clusters to look for
NIterations = 50                ! number of iterations in K-means algorithm
binfactor = 1                   ! no binning by default
inputfilename = 'undefined'     ! default filename for HDF5 data input file
groupname = 'undefined'         ! default groupname for EBSD data
datasetname = 'undefined'       ! default dataset name 

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDclustervars)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(inputfilename).eq.'undefined') then
  call FatalError('EMEBSDcluster:',' HDF5 input data file name is undefined in '//nmlfile)
 end if
 if (trim(groupname).eq.'undefined') then
  call FatalError('EMEBSDcluster:',' Data group name is undefined in '//nmlfile)
 end if
 if (trim(datasetname).eq.'undefined') then
  call FatalError('EMEBSDcluster:',' Data set name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%NClusters = NClusters
emnl%NIterations = NIterations
emnl%binfactor = binfactor
emnl%inputfilename = inputfilename
emnl%groupname = groupname
emnl%datasetname = datasetname

! parameters that are not in the nml file
emnl%NScanColumns = 0
emnl%NScanRows = 0

end subroutine GetEBSDclusterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPQCMasterNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMECPQCmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl ECP master name list structure
!
!> @date 06/19/14  SS 1.0 new routine
!> @date 08/12/15 MDG 1.1 correction of type for startthick and fn(3)
!> @date 09/15/15  SS 1.2 clean up of the subroutine
!> @date 01/04/18 MDG 1.3 added to Public repo
!--------------------------------------------------------------------------
recursive subroutine GetECPQCMasterNameList(nmlfile, ecpnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPQCMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                    :: nmlfile
type(ECPQCMasterNameListType),INTENT(INOUT)    :: ecpnl
!f2py intent(in,out) ::  ecpnl
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

integer(kind=irg)       :: nsamples
integer(kind=irg)       :: npx
integer(kind=irg)       :: Esel
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: atno
real(kind=sgl)          :: DWF
real(kind=sgl)          :: dmin
real(kind=sgl)          :: gmax_orth
real(kind=sgl)          :: QClatparm
character(1)            :: centering
character(fnlen)        :: energyfile

! define the IO namelist to facilitate passing variables to the program.
namelist /ECPQCmastervars/ nsamples, dmin, energyfile, npx, nthreads

! set the input parameters to default values (except for xtalname, which must be present)
nthreads = 1
dmin = 0.1                     ! smallest d-spacing to include in dynamical matrix [nm]
nsamples = 400
npx = 256
energyfile = 'undefined'       ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=ECPQCmastervars)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(energyfile).eq.'undefined') then
call FatalError('EMECPQCmaster:',' energy file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
ecpnl%nsamples = nsamples
ecpnl%npx = npx
ecpnl%nthreads = nthreads
ecpnl%dmin = dmin
ecpnl%energyfile = energyfile

end subroutine GetECPQCMasterNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetCTEMQCNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMECPQCmaster.f90)
!
!> @param nmlfile namelist file name
!> @param ctemqcnl EMCTEMQC name list structure
!
!> @date 06/19/14  SS 1.0 new routine
!> @date 08/12/15 MDG 1.1 correction of type for startthick and fn(3)
!> @date 09/15/15  SS 1.2 clean up of the subroutine
!> @date 01/04/18 MDG 1.3 added to Public repo
!--------------------------------------------------------------------------
recursive subroutine GetCTEMQCNameList(nmlfile, ctemqcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetCTEMQCNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                    :: nmlfile
type(CTEMQCNameListType),INTENT(INOUT)         :: ctemqcnl
!f2py intent(in,out) ::  ctemqcnl
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
integer(kind=irg)       :: npix
integer(kind=irg)       :: wwmax
real(kind=sgl)          :: kvec(3)
real(kind=sgl)          :: dmin
real(kind=sgl)          :: rnmpp
real(kind=sgl)          :: voltage
character(fnlen)        :: qxtalname
character(fnlen)        :: hdfname
character(fnlen)        :: tiffname

! define the IO namelist to facilitate passing variables to the program.
namelist /CTEMQCvars/ dmin, npix, wwmax, nthreads, kvec, rnmpp, voltage, qxtalname, hdfname, tiffname

! set the input parameters to default values (except for xtalname, which must be present)
nthreads = 1
npix = 512
wwmax = 100
kvec = (/ 0.0, 0.0, 1.0 /)
dmin = 0.25
rnmpp = 200.0
voltage = 200.0
qxtalname = 'undefined'
hdfname = 'undefined'
tiffname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
  open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
  read(UNIT=dataunit,NML=CTEMQCvars)
  close(UNIT=dataunit,STATUS='keep')

  ! check for required entries
  if (trim(qxtalname).eq.'undefined') then
    call FatalError('GetCTEMQCNameList:',' qxtalname file name is undefined in '//nmlfile)
  end if

  if (trim(hdfname).eq.'undefined') then
    call FatalError('GetCTEMQCNameList:',' hdf file name is undefined in '//nmlfile)
  end if

  if (trim(tiffname).eq.'undefined') then
    call FatalError('GetCTEMQCNameList:',' tiff file name is undefined in '//nmlfile)
  end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
ctemqcnl%nthreads = nthreads
ctemqcnl%npix = npix
ctemqcnl%wwmax = wwmax 
ctemqcnl%kvec = kvec
ctemqcnl%dmin = dmin
ctemqcnl%rnmpp = rnmpp 
ctemqcnl%voltage = voltage
ctemqcnl%qxtalname = qxtalname
ctemqcnl%hdfname = hdfname
ctemqcnl%tiffname = tiffname

end subroutine GetCTEMQCNameList



!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPMasterNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMECPmaster.f90)
!
!> @param nmlfile namelist file name
!> @param emnl ECP master name list structure
!
!> @date 06/19/14  SS 1.0 new routine
!> @date 08/12/15 MDG 1.1 correction of type for startthick and fn(3)
!> @date 09/15/15  SS 1.2 clean up of the subroutine
!--------------------------------------------------------------------------
recursive subroutine GetECPMasterNameList(nmlfile, ecpnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                    :: nmlfile
type(ECPMasterNameListType),INTENT(INOUT)      :: ecpnl
!f2py intent(in,out) ::  ecpnl
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: npx
integer(kind=irg)       :: Esel
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
character(3)            :: Notify
character(fnlen)        :: compmode
character(fnlen)        :: copyfromenergyfile
character(fnlen)        :: h5copypath
character(fnlen)        :: energyfile
logical                 :: combinesites

! define the IO namelist to facilitate passing variables to the program.
namelist /ECPmastervars/ stdout, dmin, compmode, Notify, h5copypath, &
    energyfile, Esel, npx, nthreads, copyfromenergyfile, combinesites

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
Esel = -1                       ! selected energy value for single energy run
nthreads = 1
dmin = 0.04                    ! smallest d-spacing to include in dynamical matrix [nm]
npx = 256
Notify = 'Off'
compmode = 'Blochwv'
h5copypath = 'undefined'
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
copyfromenergyfile = 'undefined'
combinesites = .FALSE.

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=ECPmastervars)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(energyfile).eq.'undefined') then
call FatalError('EMECPmaster:',' energy file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
ecpnl%stdout = stdout
ecpnl%Esel = Esel
ecpnl%npx = npx
ecpnl%nthreads = nthreads
ecpnl%dmin = dmin
ecpnl%Notify = Notify
ecpnl%compmode = compmode
ecpnl%h5copypath = h5copypath
ecpnl%copyfromenergyfile = copyfromenergyfile
ecpnl%energyfile = energyfile
ecpnl%combinesites = combinesites

end subroutine GetECPMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetreflectorNameList
!
!> @author Marc De Graef
!
!> @brief read reflector namelist for EMreflectors program
!
!> @param nmlfile namelist file name
!> @param rnl reflect name list structure
!
!> @date 05/31/16 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetreflectorNameList(nmlfile, rnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetreflectorNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                    :: nmlfile
type(reflectorNameListType),INTENT(INOUT)      :: rnl
!f2py intent(in,out) ::  rnl
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

real(kind=sgl)                                 :: increment
real(kind=sgl)                                 :: dmin
integer(kind=irg)                              :: numlist
integer(kind=irg)                              :: nthreads
character(fnlen)                               :: outputformat
character(fnlen)                               :: masterfile
character(fnlen)                               :: listfile
logical                                        :: kinematical

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDreflectors/ increment, dmin, masterfile, listfile, numlist, nthreads, outputformat, kinematical

! set the input parameters to default values (except for xtalname, which must be present)
increment = 0.025               ! angular increment [°]
dmin = 0.05                     ! smallest d-spacing to include in dynamical matrix [nm]
numlist = 20
nthreads = 1
outputformat = 'csv'            ! options: 'latex', 'csv', and 'markdown'
masterfile = 'undefined'        ! master pattern filename
listfile = 'undefined'          ! filename for output (no extension)
kinematical = .FALSE.           ! if .TRUE., a kinematical master pattern will be generated 

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
  open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
  read(UNIT=dataunit,NML=EBSDreflectors)
  close(UNIT=dataunit,STATUS='keep')

! check for required entries
  if (trim(listfile).eq.'undefined') then
    call FatalError('EMreflectors:',' output file name is undefined in '//nmlfile)
  end if
  if (trim(masterfile).eq.'undefined') then
    call FatalError('EMreflectors:',' master file name is undefined in '//nmlfile)
  end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
rnl%increment = increment
rnl%dmin = dmin
rnl%outputformat = trim(outputformat)
rnl%numlist = numlist
rnl%nthreads = nthreads
rnl%masterfile = trim(masterfile)
rnl%listfile = trim(listfile)
rnl%kinematical = kinematical

end subroutine GetreflectorNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetkinematicalNameList
!
!> @author Marc De Graef
!
!> @brief read reflector namelist for EMreflectors program
!
!> @param nmlfile namelist file name
!> @param rnl reflect name list structure
!
!> @date 05/31/16 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetkinematicalNameList(nmlfile, knl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetkinematicalNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                    :: nmlfile
type(kinematicalNameListType),INTENT(INOUT)    :: knl
!f2py intent(in,out) ::  knl
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

real(kind=sgl)                                 :: dmin
real(kind=sgl)                                 :: thr
real(kind=sgl)                                 :: voltage
character(fnlen)                               :: xtalname
character(fnlen)                               :: datafile
character(5)                                   :: mode

! define the IO namelist to facilitate passing variables to the program.
namelist /EMkinematical/ dmin, voltage, thr, xtalname, datafile, mode

! set the input parameters to default values (except for xtalname, which must be present)
dmin = 0.05                    ! smallest d-spacing to include in dynamical matrix [nm]
thr = 1.0                      ! smallest |structurefactor|^2 to include
voltage = 30000.0              ! microscope voltage [V]
datafile = 'undefined'         ! output file name
xtalname = 'undefined'         ! structure file name
mode = 'lines'                 ! default plot mode

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
  open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
  read(UNIT=dataunit,NML=EMkinematical)
  close(UNIT=dataunit,STATUS='keep')

! check for required entries
  if (trim(xtalname).eq.'undefined') then
    call FatalError('EMkinematical:','  crystal structure file name is undefined in '//nmlfile)
  end if
  if (trim(datafile).eq.'undefined') then
    call FatalError('EMkinematical:',' output file name is undefined in '//nmlfile)
  end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
knl%dmin = dmin
knl%thr = thr
knl%voltage = voltage
knl%xtalname = xtalname
knl%datafile = datafile
knl%mode = mode 

end subroutine GetkinematicalNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSD.f90)
!
!> @param nmlfile namelist file name
!> @param enl EBSD name list structure
!
!> @date 06/23/14  MDG 1.0 new routine
!> @date 05/16/19  MDG 1.1 disable energyfile parameter from namelist file
!--------------------------------------------------------------------------
recursive subroutine GetEBSDNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(EBSDNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: binning
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: energyaverage
integer(kind=irg)       :: maskradius
integer(kind=irg)       :: nregions
real(kind=sgl)          :: L
real(kind=sgl)          :: thetac
real(kind=sgl)          :: delta
real(kind=sgl)          :: xpc
real(kind=sgl)          :: ypc
real(kind=sgl)          :: energymin
real(kind=sgl)          :: energymax
real(kind=sgl)          :: gammavalue
real(kind=sgl)          :: alphaBD
real(kind=sgl)          :: axisangle(4)
real(kind=sgl)          :: hipassw
real(kind=dbl)          :: Ftensor(3,3)
real(kind=dbl)          :: beamcurrent
real(kind=dbl)          :: dwelltime
character(1)            :: includebackground
character(1)            :: poisson
character(1)            :: makedictionary
character(1)            :: applyDeformation
character(1)            :: maskpattern
character(1)            :: spatialaverage
character(3)            :: scalingmode
character(3)            :: eulerconvention
character(3)            :: outputformat
character(4)            :: Fframe
character(5)            :: bitdepth
character(fnlen)        :: anglefile
character(fnlen)        :: anglefiletype
character(fnlen)        :: masterfile
character(fnlen)        :: energyfile  ! removed from template file 05/16/19 [MDG]
character(fnlen)        :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDdata / stdout, L, thetac, delta, numsx, numsy, xpc, ypc, anglefile, eulerconvention, masterfile, bitdepth, &
                        energyfile, datafile, beamcurrent, dwelltime, energymin, energymax, binning, gammavalue, alphaBD, &
                        scalingmode, axisangle, nthreads, outputformat, maskpattern, energyaverage, spatialaverage, &
                        applyDeformation, Ftensor, includebackground, anglefiletype, makedictionary, hipassw, nregions, &
                        maskradius, poisson, Fframe

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
numsx           = 0             ! [dimensionless]
numsy           = 0             ! [dimensionless]
binning         = 1             ! binning mode  (1, 2, 4, or 8)
L               = 20000.0       ! [microns]
nthreads        = 1             ! number of OpenMP threads
nregions        = 10            ! number of regions in adaptive histogram equalization
energyaverage   = 0             ! apply energy averaging (1) or not (0); useful for dictionary computations
thetac          = 0.0           ! [degrees]
delta           = 25.0          ! [microns]
xpc             = 0.0           ! [pixels]
ypc             = 0.0           ! [pixels]
energymin       = 15.0          ! minimum energy to consider
energymax       = 30.0          ! maximum energy to consider
gammavalue      = 1.0           ! gamma factor
alphaBD         = 0.0           ! transfer lens barrel distortion parameter
maskradius      = 240           ! mask radius
hipassw         = 0.05          ! hi-pass filter radius
axisangle       = (/0.0, 0.0, 1.0, 0.0/)        ! no additional axis angle rotation
Ftensor         = reshape( (/ 1.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 1.D0 /), (/ 3,3 /) )
beamcurrent     = 14.513D0      ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
makedictionary  = 'y'
poisson         = 'n'           ! apply poisson noise ? 
includebackground = 'y'         ! set to 'n' to remove realistic background intensity profile
applyDeformation = 'n'          ! should we apply a deformation tensor to the unit cell?
Fframe = 'crys'                 ! frame of reference for the Ftensor ('crys':crystal frame; 'samp':sample frame)
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
eulerconvention = 'tsl'         ! convention for the first Euler angle ['tsl' or 'hkl']
outputformat    = 'gui'         ! output format for 'bin' or 'gui' use
bitdepth        = '8bit'        ! format for output; '8char' for [0..255], '##int' for integers, 'float' for floats
! the '##int' notation stands for the actual bitdepth; all values are stored as 32bit integers, but they are scaled
! from the float values to a maximum that is given by the first two digits, which indicate the bit depth; so, valid
! values would be '10int' for a 10-bit integer scale, '16int' for a 16-bit integer scale, and so on.
anglefile       = 'undefined'   ! filename
anglefiletype   = 'orientations'! 'orientations' or 'orpcdef'
masterfile      = 'undefined'   ! filename
energyfile      = 'undefined'   ! name of file that contains energy histograms for all scintillator pixels (output from MC program)
datafile        = 'undefined'   ! output file name
spatialaverage  = 'n'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries

! we no longer require the energyfile parameter, but for backwards compatibility
! we still allow the user to include it (it doesn't do anything though)
! if (trim(energyfile).eq.'undefined') then
!  call FatalError('GetEBSDNameList:',' energy file name is undefined in '//nmlfile)
! end if

 if (trim(anglefile).eq.'undefined') then
  call FatalError('GetEBSDNameList:',' angle file name is undefined in '//nmlfile)
 end if

 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetEBSDNameList:',' master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('GetEBSDNameList:',' output file name is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetEBSDNameList:',' pattern size numsx is zero '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetEBSDNameList:',' pattern size numsy is zero '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
enl%stdout = stdout
enl%numsx = numsx
enl%numsy = numsy
enl%binning = binning
enl%nregions = nregions
enl%maskradius = maskradius
enl%L = L
enl%nthreads = nthreads
enl%energyaverage = energyaverage
enl%thetac = thetac
enl%delta = delta
enl%xpc = xpc
enl%ypc = ypc
enl%energymin = energymin
enl%energymax = energymax
enl%gammavalue = gammavalue
enl%alphaBD = alphaBD
enl%hipassw = hipassw
enl%axisangle = axisangle
enl%Ftensor = Ftensor
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%includebackground = includebackground
enl%makedictionary = makedictionary
enl%poisson = poisson
enl%applyDeformation = applyDeformation
enl%Fframe=Fframe
enl%maskpattern = maskpattern
enl%scalingmode = scalingmode
enl%eulerconvention = eulerconvention
enl%outputformat = outputformat
enl%bitdepth = bitdepth
enl%anglefile = anglefile
enl%anglefiletype = anglefiletype
enl%masterfile = masterfile
! we require energyfile to be identical to masterfile, so the 
! user definition, if any, in the namelist file is overwritten here...
enl%energyfile = enl%masterfile       ! changed on 05/16/19 [MDG]
enl%datafile = datafile
enl%spatialaverage = spatialaverage
end subroutine GetEBSDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetBSENameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMBSE.f90)
!
!> @param nmlfile namelist file name
!> @param enl EMBSE name list structure
!
!> @date 07/28/20  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetBSENameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetBSENameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(BSENameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

real(kind=sgl)          :: energymin
real(kind=sgl)          :: energymax
real(kind=sgl)          :: incidence 
real(kind=sgl)          :: beamcurrent
real(kind=sgl)          :: dwelltime
real(kind=sgl)          :: gammavalue
real(kind=sgl)          :: workingdistance
real(kind=sgl)          :: BSEdistance
real(kind=sgl)          :: rin
real(kind=sgl)          :: rout
integer(kind=irg)       :: NsqL
integer(kind=irg)       :: nthreads
character(fnlen)        :: scalingmode
character(fnlen)        :: useangles
character(fnlen)        :: imagefile
character(fnlen)        :: masterfile
character(fnlen)        :: Kosselmasterfile
character(fnlen)        :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist  / BSEdata / energymin, energymax, beamcurrent, dwelltime, gammavalue, workingdistance, BSEdistance, Kosselmasterfile, &
                      rin, rout, NsqL, nthreads, scalingmode, useangles, imagefile, masterfile, datafile, incidence


! set the input parameters to default values (except for xtalname, which must be present)
energymin = 5.0
energymax = 20.0
incidence = 0.0
beamcurrent = 150.0
dwelltime = 100.0
gammavalue = 1.0
workingdistance = 10.0
BSEdistance = 9.5
rin = 5.0
rout = 12.0
NsqL = 40
nthreads = 1
scalingmode = 'not;'
useangles = 'original'
imagefile = 'undefined'
masterfile = 'undefined'
Kosselmasterfile = 'undefined'
datafile = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=BSEdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries

! we no longer require the energyfile parameter, but for backwards compatibility
! we still allow the user to include it (it doesn't do anything though)
! if (trim(energyfile).eq.'undefined') then
!  call FatalError('GetEBSDNameList:',' energy file name is undefined in '//nmlfile)
! end if

 if (trim(imagefile).eq.'undefined') then
  call FatalError('GetBSENameList:',' image file name is undefined in '//nmlfile)
 end if

 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetBSENameList:',' master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(Kosselmasterfile).eq.'undefined') then
  call FatalError('GetBSENameList:',' Kossel master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('GetBSENameList:',' DI input file name is undefined in '//nmlfile)
 end if
 
end if

! if we get here, then all appears to be ok, and we need to fill in the enl fields
enl%energymin = energymin
enl%energymax = energymax
enl%incidence = incidence
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%gammavalue = gammavalue
enl%workingdistance = workingdistance
enl%BSEdistance = BSEdistance
enl%rin = rin
enl%rout = rout 
enl%NsqL = NsqL
enl%nthreads = nthreads
enl%scalingmode = scalingmode
enl%useangles = useangles
enl%imagefile = trim(imagefile)
enl%masterfile = trim(masterfile)
enl%Kosselmasterfile = trim(Kosselmasterfile)
enl%datafile = trim(datafile)

end subroutine GetBSENameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDdefectNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDdefect.f90)
!
!> @param nmlfile namelist file name
!> @param enl EBSD name list structure
!
!> @date 11/05/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDdefectNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDdefectNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(EBSDdefectNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                       :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: binning
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: thetac
real(kind=sgl)          :: delta
real(kind=sgl)          :: spotsize
real(kind=sgl)          :: omega
real(kind=sgl)          :: gammavalue
real(kind=dbl)          :: beamcurrent
real(kind=dbl)          :: dwelltime
logical                 :: sampleInteractionVolume
character(3)            :: scalingmode
character(fnlen)        :: deformationfile
character(fnlen)        :: ivolfile
character(fnlen)        :: masterfile
character(fnlen)        :: datafile
character(fnlen)        :: tmpfspath

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDdefectdata / stdout, thetac, delta, numsx, numsy, deformationfile, spotsize, &
                             masterfile, datafile, beamcurrent, dwelltime, gammavalue, tmpfspath, &
                             scalingmode, nthreads, omega, ivolfile, sampleInteractionVolume

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
numsx           = 0             ! [dimensionless]
numsy           = 0             ! [dimensionless]
nthreads        = 1             ! number of OpenMP threads
thetac          = 0.0           ! [degrees]
delta           = 25.0          ! [microns]
spotsize        = 2.0           ! [nanometer]
omega           = 0.0
gammavalue      = 1.0           ! gamma factor
beamcurrent     = 14.513D0      ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
sampleInteractionVolume = .FALSE.  ! should we sample an MC-generated interaction volume?
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
ivolfile        = 'undefined'   ! filename
deformationfile = 'undefined'   ! filename
masterfile      = 'undefined'   ! filename
datafile        = 'undefined'   ! output file name
tmpfspath       = 'undefined'   ! path to memory file system, if it exists

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDdefectdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(deformationfile).eq.'undefined') then
  call FatalError('GetEBSDdefectNameList:',' deformationfile file name is undefined in '//nmlfile)
 end if

 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetEBSDdefectNameList:',' master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('GetEBSDdefectNameList:',' output file name is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetEBSDdefectNameList:',' pattern size numsx is zero '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetEBSDdefectNameList:',' pattern size numsy is zero '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the enl fields
enl%stdout = stdout
enl%numsx = numsx
enl%numsy = numsy
enl%nthreads = nthreads
enl%thetac = thetac
enl%delta = delta
enl%spotsize = spotsize
enl%gammavalue = gammavalue
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%scalingmode = scalingmode
enl%sampleInteractionVolume = sampleInteractionVolume
enl%deformationfile = deformationfile
enl%masterfile = masterfile
enl%ivolfile = ivolfile
enl%datafile = datafile
enl%omega = omega
enl%tmpfspath = tmpfspath

end subroutine GetEBSDdefectNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDDENameList
!
!> @author Chaoyi Zhu/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill de and enl structures
!
!> @param nmlfile namelist file name
!> @param enl EBSD name list structure
!> @param de differential evolution name list structure
!> @date 01/30/20  CZ 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDDENameList(nmlfile, enl, de, p,initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDDENameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(EBSDDENameListType),INTENT(INOUT)  :: de
type(EBSDNameListType),INTENT(INOUT)    :: enl
type(EBSDDIpreviewNameListType),INTENT(INOUT)     :: p
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)        :: NP
integer(kind=irg)        :: itermax
integer(kind=irg)        :: strategy 
integer(kind=irg)        :: refresh
integer(kind=irg)        :: iwrite
integer(kind=irg)        :: method(3)
real(kind=sgl)           :: VTR 
real(kind=sgl)           :: CR_XC
real(kind=sgl)           :: F_XC
real(kind=sgl)           :: F_CR
real(kind=sgl)           :: bound(3)
real(kind=sgl)           :: w
real(kind=sgl)           :: w_damp
real(kind=sgl)           :: c1 
real(kind=sgl)           :: c2 
integer(kind=irg)        :: objective
character(fnlen)         :: outputfile
character(1)            :: hybrid
character(2)            :: globalopt
character(1)             :: single_opt
integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: binning
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: energyaverage
integer(kind=irg)       :: maskradius
integer(kind=irg)       :: nregions
real(kind=sgl)          :: L
real(kind=sgl)          :: thetac
real(kind=sgl)          :: delta
real(kind=sgl)          :: xpc
real(kind=sgl)          :: ypc
real(kind=sgl)          :: energymin
real(kind=sgl)          :: energymax
real(kind=sgl)          :: gammavalue
real(kind=sgl)          :: alphaBD
real(kind=sgl)          :: axisangle(4)
real(kind=sgl)          :: hipassw
real(kind=dbl)          :: Ftensor(3,3)
real(kind=dbl)          :: beamcurrent
real(kind=dbl)          :: dwelltime
character(1)            :: includebackground
character(1)            :: poisson
character(1)            :: makedictionary
character(1)            :: applyDeformation
character(1)            :: maskpattern
character(1)            :: spatialaverage
character(4)            :: Fframe
character(3)            :: scalingmode
character(3)            :: eulerconvention
character(3)            :: outputformat
character(5)            :: bitdepth
character(fnlen)        :: anglefile
character(fnlen)        :: anglefiletype
character(fnlen)        :: masterfile
character(fnlen)        :: targetfile
character(fnlen)        :: datafile
character(fnlen)        :: energyfile  ! removed from template file 05/16/19 [MDG]

integer(kind=irg)       :: patx
integer(kind=irg)       :: paty
integer(kind=irg)       :: ipf_wd
integer(kind=irg)       :: ipf_ht
character(fnlen)        :: inputtype
character(fnlen)        :: HDFstrings(10)


! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDDEdata / NP, itermax, strategy, refresh, iwrite, method, VTR, CR_XC, F_XC, F_CR, bound, hybrid, globalopt, &
                         objective, outputfile, stdout, L, thetac, delta, numsx, numsy, binning, xpc, ypc, anglefile, &
                         eulerconvention, masterfile, targetfile, bitdepth, energyfile, beamcurrent, dwelltime, energymin, &
                         energymax, gammavalue, alphaBD, scalingmode, axisangle, nthreads, outputformat, maskpattern, &
                         energyaverage, spatialaverage, applyDeformation, Ftensor, includebackground, anglefiletype, &
                         makedictionary, hipassw, nregions, maskradius, poisson, patx, paty, inputtype, HDFstrings, ipf_wd, &
                         ipf_ht, datafile, w, w_damp, c1, c2, single_opt, Fframe

! set the input parameters to default values (except for xtalname, which must be present)
                        
NP=60
itermax=100
strategy=2
refresh=10
iwrite=7
method=(/0,1,1/)
VTR= -1
CR_XC=0.9
F_XC=0.5
F_CR=0.5
bound=(/0.001,1.0,1.0/)
w=1.0
w_damp=0.99
c1=2
c2=2
hybrid='n'
globalopt='DE'
single_opt='n'
objective=1
outputfile='undefined'
stdout          = 6
numsx           = 0             ! [dimensionless]
numsy           = 0             ! [dimensionless]
binning         = 1             ! binning mode  (1, 2, 4, or 8)
L               = 20000.0       ! [microns]
nthreads        = 1             ! number of OpenMP threads
nregions        = 10            ! number of regions in adaptive histogram equalization
energyaverage   = 0             ! apply energy averaging (1) or not (0); useful for dictionary computations
thetac          = 0.0           ! [degrees]
delta           = 25.0          ! [microns]
xpc             = 0.0           ! [pixels]
ypc             = 0.0           ! [pixels]
energymin       = 10.0          ! minimum energy to consider
energymax       = 20.0          ! maximum energy to consider
gammavalue      = 1.0           ! gamma factor
alphaBD         = 0.0           ! transfer lens barrel distortion parameter
maskradius      = 240           ! mask radius
hipassw         = 0.05          ! hi-pass filter radius
axisangle       = (/0.0, 0.0, 1.0, 0.0/)        ! no additional axis angle rotation
Ftensor         = reshape( (/ 1.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 1.D0 /), (/ 3,3 /) )
beamcurrent     = 150.0D0      ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
makedictionary  = 'n'
poisson         = 'n'           ! apply poisson noise ? 
includebackground = 'n'         ! set to 'n' to remove realistic background intensity profile
applyDeformation = 'n'          ! should we apply a deformation tensor to the unit cell?
Fframe = 'crys'                 ! frame of reference for the Ftensor ('crys':crystal frame; 'samp':sample frame)
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
eulerconvention = 'tsl'         ! convention for the first Euler angle ['tsl' or 'hkl']
outputformat    = 'gui'         ! output format for 'bin' or 'gui' use
bitdepth        = '8bit'        ! format for output; '8char' for [0..255], '##int' for integers, 'float' for floats
! the '##int' notation stands for the actual bitdepth; all values are stored as 32bit integers, but they are scaled
! from the float values to a maximum that is given by the first two digits, which indicate the bit depth; so, valid
! values would be '10int' for a 10-bit integer scale, '16int' for a 16-bit integer scale, and so on.
anglefile       = 'undefined'   ! filename
anglefiletype   = 'orientations'! 'orientations' or 'orpcdef'
masterfile      = 'undefined'   ! filename
targetfile      = 'undefined'   ! filename
datafile        = 'undefined'   ! filename
energyfile      = 'undefined'   ! name of file that contains energy histograms for all scintillator pixels (output from MC program)
spatialaverage  = 'n'
patx = 0
paty = 0
ipf_wd = 100
ipf_ht = 100
inputtype = 'Binary'
HDFstrings = ''

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDDEdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 
 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetEBSDDENameList:',' master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(targetfile).eq.'undefined') then
  call FatalError('GetEBSDDENameList:',' target pattern file name is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetEBSDDENameList:',' pattern size numsx is zero '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetEBSDDENameList:',' pattern size numsy is zero '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
de%outputfile=outputfile
de%NP=NP
de%itermax=itermax
de%strategy=strategy
de%refresh=refresh
de%iwrite=iwrite
de%method=method
de%VTR=VTR
de%CR_XC=CR_XC
de%F_XC=F_XC
de%F_CR=F_CR
de%bound=bound
de%w=w
de%w_damp=w_damp
de%c1=c1
de%c2=c2
de%hybrid=hybrid
de%globalopt=globalopt
de%objective=objective
de%single_opt=single_opt
enl%stdout = stdout
enl%numsx = numsx
enl%numsy = numsy
enl%binning = binning
enl%nregions = nregions
enl%maskradius = maskradius
enl%L = L
enl%nthreads = nthreads
enl%energyaverage = energyaverage
enl%thetac = thetac
enl%delta = delta
enl%xpc = xpc
enl%ypc = ypc
enl%energymin = energymin
enl%energymax = energymax
enl%gammavalue = gammavalue
enl%alphaBD = alphaBD
enl%hipassw = hipassw
enl%axisangle = axisangle
enl%Ftensor = Ftensor
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%includebackground = includebackground
enl%makedictionary = makedictionary
enl%poisson = poisson
enl%applyDeformation = applyDeformation
enl%Fframe = Fframe
enl%maskpattern = maskpattern
enl%scalingmode = scalingmode
enl%eulerconvention = eulerconvention
enl%outputformat = outputformat
enl%bitdepth = bitdepth
enl%anglefile = anglefile
enl%anglefiletype = anglefiletype
enl%masterfile = masterfile
enl%targetfile = targetfile
enl%datafile = datafile
! we require energyfile to be identical to masterfile, so the 
! user definition, if any, in the namelist file is overwritten here...
enl%energyfile = enl%masterfile       ! changed on 05/16/19 [MDG]
enl%spatialaverage = spatialaverage

p%patx = patx
p%paty = paty
p%ipf_wd = ipf_wd
p%ipf_ht = ipf_ht
p%inputtype = inputtype
p%HDFstrings = HDFstrings

end subroutine GetEBSDDENameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetTKDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMTKD.f90)
!
!> @param nmlfile namelist file name
!> @param enl TKD name list structure
!
!> @date 05/09/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetTKDNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetTKDNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(TKDNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: binning
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: energyaverage
real(kind=sgl)          :: L
real(kind=sgl)          :: thetac
real(kind=sgl)          :: delta
real(kind=sgl)          :: xpc
real(kind=sgl)          :: ypc
real(kind=sgl)          :: omega
real(kind=sgl)          :: energymin
real(kind=sgl)          :: energymax
real(kind=sgl)          :: gammavalue
real(kind=sgl)          :: alphaBD
real(kind=sgl)          :: axisangle(4)
real(kind=dbl)          :: beamcurrent
real(kind=dbl)          :: dwelltime
character(1)            :: maskpattern
character(1)            :: spatialaverage
character(3)            :: scalingmode
character(3)            :: eulerconvention
character(3)            :: outputformat
character(fnlen)        :: anglefile
character(fnlen)        :: masterfile
character(fnlen)        :: energyfile 
character(fnlen)        :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist  / TKDdata / stdout, L, thetac, delta, numsx, numsy, xpc, ypc, anglefile, eulerconvention, masterfile, &
                     energyfile, datafile, beamcurrent, dwelltime, energymin, energymax, binning, gammavalue, alphaBD, &
                     scalingmode, axisangle, nthreads, outputformat, maskpattern, energyaverage, omega, spatialaverage

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
numsx           = 0             ! [dimensionless]
numsy           = 0             ! [dimensionless]
binning         = 1             ! binning mode  (1, 2, 4, or 8)
L               = 20000.0       ! [microns]
nthreads        = 1             ! number of OpenMP threads
energyaverage   = 0             ! apply energy averaging (1) or not (0); useful for dictionary computations
thetac          = 0.0           ! [degrees]
delta           = 25.0          ! [microns]
xpc             = 0.0           ! [pixels]
ypc             = 0.0           ! [pixels]
omega           = 0.0
energymin       = 15.0          ! minimum energy to consider
energymax       = 30.0          ! maximum energy to consider
gammavalue      = 1.0           ! gamma factor
alphaBD         = 0.0           ! transfer lens barrel distortion parameter
axisangle       = (/0.0, 0.0, 1.0, 0.0/)        ! no additional axis angle rotation
beamcurrent     = 100.0D0       ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
eulerconvention = 'tsl'         ! convention for the first Euler angle ['tsl' or 'hkl']
outputformat    = 'gui'         ! output format for 'bin' or 'gui' use
anglefile       = 'undefined'   ! filename
masterfile      = 'undefined'   ! filename
energyfile      = 'undefined'   ! name of file that contains energy histograms for all scintillator pixels (output from MC program)
datafile        = 'undefined'   ! output file name
spatialaverage  = 'n'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=TKDdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(energyfile).eq.'undefined') then
  call FatalError('GetTKDNameList:',' energy file name is undefined in '//nmlfile)
 end if

 if (trim(anglefile).eq.'undefined') then
  call FatalError('GetTKDNameList:',' angle file name is undefined in '//nmlfile)
 end if

 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetTKDNameList:',' master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('GetTKDNameList:',' output file name is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetTKDNameList:',' pattern size numsx is zero '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetTKDNameList:',' pattern size numsy is zero '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
enl%stdout = stdout
enl%numsx = numsx
enl%numsy = numsy
enl%binning = binning
enl%L = L
enl%nthreads = nthreads
enl%energyaverage = energyaverage
enl%thetac = thetac
enl%delta = delta
enl%xpc = xpc
enl%ypc = ypc
enl%energymin = energymin
enl%energymax = energymax
enl%gammavalue = gammavalue
enl%alphaBD = alphaBD
enl%axisangle = axisangle
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%maskpattern = maskpattern
enl%scalingmode = scalingmode
enl%eulerconvention = eulerconvention
enl%outputformat = outputformat
enl%anglefile = anglefile
enl%masterfile = masterfile
enl%energyfile = energyfile
enl%datafile = datafile
enl%omega = omega
enl%spatialaverage = spatialaverage
end subroutine GetTKDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDoverlapNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDoverlap.f90)
!
!> @param nmlfile namelist file name
!> @param enl EBSD name list structure
!
!> @date 04/29/15  MDG 1.0 new routine
!> @date 09/24/19  MDG 1.1 expanded for up to three variant phases
!--------------------------------------------------------------------------
recursive subroutine GetEBSDoverlapNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDoverlapNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDoverlapNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: newpgnum
integer(kind=irg)       :: PatternAxisA(3)
integer(kind=irg)       :: HorizontalAxisA(3)
real(kind=sgl)          :: tA(3)
real(kind=sgl)          :: tB(3)
real(kind=sgl)          :: tA2(3)
real(kind=sgl)          :: tC(3)
real(kind=sgl)          :: tA3(3)
real(kind=sgl)          :: tD(3)
real(kind=sgl)          :: gA(3)
real(kind=sgl)          :: gB(3)
real(kind=sgl)          :: gA2(3)
real(kind=sgl)          :: gC(3)
real(kind=sgl)          :: gA3(3)
real(kind=sgl)          :: gD(3)
real(kind=sgl)          :: fracB
real(kind=sgl)          :: fracC
real(kind=sgl)          :: fracD
character(fnlen)        :: masterfileA
character(fnlen)        :: masterfileB
character(fnlen)        :: masterfileC
character(fnlen)        :: masterfileD
character(fnlen)        :: h5copypath
character(fnlen)        :: overlapmode
character(fnlen)        :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDoverlapdata / stdout, PatternAxisA, tA, tB, gA, gB, masterfileA, masterfileB, & 
                              datafile, HorizontalAxisA, overlapmode, newpgnum, tC, gC, tD, gD, fracB, &
                              fracC, fracD, masterfileC, masterfileD, gA2, gA3, tA2, tA3, h5copypath

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
newpgnum        = -1                            ! -1 means 'use the point group of phase A'
PatternAxisA    = (/ 0, 0, 1 /)                 ! center axis for output pattern
HorizontalAxisA = (/ 1, 0, 0 /)                 ! horizontal axis for output pattern
tA              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal A
tB              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal B
tA2             = (/0.0, 0.0, 1.0/)             ! direction vector in crystal A
tC              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal C
tA3             = (/0.0, 0.0, 1.0/)             ! direction vector in crystal A
tD              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal D
gA              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal A
gB              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal B
gA2             = (/1.0, 0.0, 0.0/)             ! plane normal in crystal A
gC              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal C
gA3             = (/1.0, 0.0, 0.0/)             ! plane normal in crystal A
gD              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal D
fracB           = 0.25                          ! volume fraction of phase B 
fracC           = 0.25                          ! volume fraction of phase C 
fracD           = 0.25                          ! volume fraction of phase D 
masterfileA     = 'undefined'   ! filename
masterfileB     = 'undefined'   ! filename
masterfileC     = 'undefined'   ! filename
masterfileD     = 'undefined'   ! filename
h5copypath      = 'undefined'   ! filename
datafile        = 'undefined'   ! output file name
overlapmode     = 'series'      ! options are 'full' or 'series'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDoverlapdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(masterfileA).eq.'undefined') then
  call FatalError('EMEBSDoverlap:',' master pattern file name A is undefined in '//nmlfile)
 end if

 if (trim(masterfileB).eq.'undefined') then
  call FatalError('EMEBSDoverlap:',' master pattern file name B is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('EMEBSDoverlap:',' output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
enl%stdout = stdout
enl%newpgnum = newpgnum
enl%PatternAxisA = PatternAxisA
enl%HorizontalAxisA = HorizontalAxisA
enl%tA = tA
enl%tB = tB
enl%tA2= tA2
enl%tC = tC
enl%tA3= tA3
enl%tD = tD
enl%gA = gA
enl%gB = gB
enl%gA2= gA2
enl%gC = gC
enl%gA3= gA3
enl%gD = gD
enl%fracB = fracB
enl%fracC = fracC
enl%fracD = fracD
enl%masterfileA = masterfileA
enl%masterfileB = masterfileB
enl%masterfileC = masterfileC
enl%masterfileD = masterfileD
enl%h5copypath = h5copypath
enl%datafile = datafile
enl%overlapmode = overlapmode

end subroutine GetEBSDoverlapNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetTKDoverlapNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMTKDoverlap.f90)
!
!> @param nmlfile namelist file name
!> @param enl TKD name list structure
!
!> @date 01/03/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetTKDoverlapNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetTKDoverlapNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(TKDoverlapNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: PatternAxisA(3)
integer(kind=irg)       :: HorizontalAxisA(3)
real(kind=sgl)          :: tA(3)
real(kind=sgl)          :: tB(3)
real(kind=sgl)          :: gA(3)
real(kind=sgl)          :: gB(3)
real(kind=sgl)          :: fracA
character(fnlen)        :: masterfileA
character(fnlen)        :: masterfileB
character(fnlen)        :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist  / TKDoverlapdata / stdout, PatternAxisA, tA, tB, gA, gB, fracA, masterfileA, masterfileB, & 
                              datafile, HorizontalAxisA

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
PatternAxisA    = (/ 0, 0, 1 /)                 ! center axis for output pattern
HorizontalAxisA = (/ 1, 0, 0 /)                 ! horizontal axis for output pattern
tA              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal A
tB              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal B
gA              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal A
gB              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal B
fracA           = -1.0                          ! volume fraction of phase A; 
! if set to -1.0, the output is a simple HDF file with a series of 21 merged patterns for fracA=0..1 
! in steps of 0.05, formatted in Square Lambert, Circular Lambert, and Stereographic Projection formats;
! if positive, then we single merged master pattern is computed for all energies,
! and the datafile will be a copy of the masterfileA file, but with a replaced master
! pattern array.
masterfileA     = 'undefined'   ! filename
masterfileB     = 'undefined'   ! filename
datafile        = 'undefined'   ! output file name

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=TKDoverlapdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(masterfileA).eq.'undefined') then
  call FatalError('EMTKDoverlap:',' master pattern file name A is undefined in '//nmlfile)
 end if

 if (trim(masterfileB).eq.'undefined') then
  call FatalError('EMTKDoverlap:',' master pattern file name B is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('EMTKDoverlap:',' output file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
enl%stdout = stdout
enl%PatternAxisA = PatternAxisA
enl%HorizontalAxisA = HorizontalAxisA
enl%tA = tA
enl%tB = tB
enl%gA = gA
enl%gB = gB
enl%fracA = fracA
enl%masterfileA = masterfileA
enl%masterfileB = masterfileB
enl%datafile = datafile

end subroutine GetTKDoverlapNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetTKDspotsNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMTKDspots.f90)
!
!> @param nmlfile namelist file name
!> @param enl TKD name list structure
!
!> @date 01/03/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetTKDspotsNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetTKDspotsNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(TKDspotsNameListType),INTENT(INOUT):: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: ncubochoric
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
real(kind=sgl)          :: voltage
real(kind=sgl)          :: dmin
real(kind=sgl)          :: thickness
real(kind=sgl)          :: L
real(kind=sgl)          :: thetac
real(kind=sgl)          :: delta
real(kind=sgl)          :: omega
real(kind=sgl)          :: sig
real(kind=sgl)          :: xpc
real(kind=sgl)          :: ypc
character(fnlen)        :: xtalname
character(fnlen)        :: outname
character(fnlen)        :: eulerfile 

! define the IO namelist to facilitate passing variables to the program.
namelist  / TKDspots / ncubochoric, nthreads, numsx, numsy, voltage, dmin, thickness, L, &
                       thetac, delta, omega, sig, xpc, ypc, xtalname, outname, eulerfile

! set the input parameters to default values (except for xtalname, which must be present)
ncubochoric     = 100
nthreads        = 1
numsx           = 0
numsy           = 0
voltage         = 20.0
dmin            = 0.05
thickness       = 50.0
L               = 15000.0
thetac          = 10.0
delta           = 50.0
omega           = 0.0
sig             = 70.0
xpc             = 0.0
ypc             = 0.0
xtalname        = 'undefined'
outname         = 'undefined'
eulerfile       = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=TKDspots)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetTKDspotsNameList:',' xtal input file is undefined in '//nmlfile)
 end if

 if (trim(outname).eq.'undefined') then
  call FatalError('GetTKDspotsNameList:',' output file name B is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then 
  call FatalError('GetTKDspotsNameList:',' pattern size numsx is zero '//nmlfile)
 end if

 if (numsy.eq.0) then 
  call FatalError('GetTKDspotsNameList:',' pattern size numsy is zero '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
enl%ncubochoric = ncubochoric
enl%nthreads = nthreads
enl%numsx = numsx 
enl%numsy = numsy
enl%voltage = voltage
enl%dmin = dmin 
enl%thickness = thickness
enl%L = L 
enl%thetac = thetac
enl%delta = delta
enl%omega = omega
enl%sig = sig
enl%xpc = xpc 
enl%ypc = ypc 
enl%xtalname = xtalname 
enl%outname = outname 
enl%eulerfile = eulerfile

end subroutine GetTKDspotsNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPZANameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill ecpnl structure (used by EMECPZA.f90)
!
!> @param nmlfile namelist file name
!> @param ecpnl name list structure
!
!> @date 01/25/17 MDG 1.0 new structure
!--------------------------------------------------------------------------
recursive subroutine GetECPZANameList(nmlfile, ecpnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPZANameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(ECPZANameListType),INTENT(INOUT)   :: ecpnl
!f2py intent(in,out) ::  ecpnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: fn(3)
integer(kind=irg)       :: k(3)
integer(kind=irg)       :: npix
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: dmin
real(kind=sgl)          :: ktmax
character(1)            :: maskpattern
character(fnlen)        :: energyfile 
character(fnlen)        :: outname

namelist /ECPZAlist/ k, fn, dmin, ktmax, npix, outname, nthreads, maskpattern, energyfile

! default values
k = (/ 0, 0, 1 /)        ! beam direction [direction indices]
fn = (/ 0, 0, 1 /)       ! foil normal [direction indices]
dmin = 0.05              ! smallest d-spacing to include in dynamical matrix [nm]
ktmax = 0.0              ! beam convergence in units of |g_a|
npix = 256               ! output arrays will have size npix x npix
nthreads = 1
maskpattern = 'n'
outname = 'undefined'    ! output filename
energyfile = 'undefined'    ! output filename

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=ECPZAlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(outname).eq.'undefined') then
  call FatalError('EMECPZA:',' output file name is undefined in '//nmlfile)
 end if
 if (trim(energyfile).eq.'undefined') then
  call FatalError('EMECPZA:',' Monte Carlo input file name is undefined in '//nmlfile)
 end if
end if

ecpnl%fn = fn
ecpnl%k = k
ecpnl%npix = npix
ecpnl%dmin = dmin
ecpnl%ktmax = ktmax
ecpnl%outname = outname
ecpnl%energyfile = energyfile
ecpnl%nthreads = nthreads
ecpnl%maskpattern = maskpattern

end subroutine GetECPZANameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill ecpnl structure (used by EMECP.f90)
!
!> @param nmlfile namelist file name
!> @param knl Kossel name list structure
!
!> @date 06/13/14  MDG 1.0 new routine
!> @date 11/25/14  MDG 2.0 added parameters for film on substrate mode
!> @date 10/15/15 SS  1.2 changes for release
!--------------------------------------------------------------------------
recursive subroutine GetECPNameList(nmlfile, ecpnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(ECPNameListType),INTENT(INOUT)     :: ecpnl
!f2py intent(in,out) ::  ecpnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: fn_f(3)
integer(kind=irg)       :: fn_s(3)
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: npix
integer(kind=irg)       :: gF(3)
integer(kind=irg)       :: gS(3)
integer(kind=irg)       :: tF(3)
integer(kind=irg)       :: tS(3)
real(kind=sgl)          :: dmin
real(kind=sgl)          :: thetac
real(kind=sgl)          :: filmthickness
character(fnlen)        :: xtalname
character(fnlen)        :: xtalname2
character(fnlen)        :: energyfile
character(fnlen)        :: filmfile
character(fnlen)        :: subsfile
character(fnlen)        :: masterfile
character(fnlen)        :: datafile
character(1)            :: maskpattern
character(fnlen)        :: anglefile
character(3)            :: eulerconvention
integer(kind=irg)       :: numangle_anglefile
real(kind=sgl)          :: gammavalue
character(3)            :: outputformat
real(kind=dbl)          :: sampletilt
real(kind=sgl)          :: workingdistance
real(kind=sgl)          :: Rin
real(kind=sgl)          :: Rout

! namelist /ECPlist/ stdout, xtalname, voltage, k, fn, dmin, distort, abcdist, albegadist, ktmax, &
namelist /ECPlist/ stdout, xtalname, xtalname2, fn_f, fn_s, dmin, filmthickness, anglefile, &
                   nthreads, thetac, npix, maskpattern, eulerconvention, Rin, Rout, &
                   gF, gS, tF, tS, energyfile, filmfile, subsfile, masterfile, datafile, &
                   numangle_anglefile, gammavalue, outputformat, sampletilt, workingdistance

! default values
stdout = 6                              ! standard output
fn_f = (/ 0, 0, 1 /)                    ! beam direction [direction indices]
fn_s = (/ 0, 0, 1 /)                    ! foil normal [direction indices]
gF = (/ 0, 0, 0 /)                      ! plane normal in film
gS = (/ 0, 0, 0 /)                      ! plane normal in substrate
tF = (/ 0, 0, 0 /)                      ! direction in film
tS = (/ 0, 0, 0 /)                      ! direction in substrate
npix = 200                              ! number of pixels in final image (npix x npix)
nthreads = 1                            ! number of OpenMP threads
dmin = 0.04                             ! smallest d-spacing to include in dynamical matrix [nm]
thetac = 0.0                            ! beam convergence in mrad (either ktmax or thetac must be given)
filmthickness = 0.0                     ! 0.0 if there is no film
xtalname = 'undefined'                  ! initial value to check that the keyword is present in the nml file
xtalname2 = 'undefined'                 ! initial value for substrate structure name
energyfile = 'undefined'
filmfile = 'undefined'
subsfile = 'undefined'
masterfile = 'undefined'
datafile = 'undefined'
maskpattern = 'y'
anglefile = 'undefined'
eulerconvention = 'hkl'
numangle_anglefile = 0
gammavalue = 1.0
outputformat = 'gui'
sampletilt = 0.D0
Workingdistance = 13.0
Rin = 2.0
Rout = 6.0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=ECPlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMEECP:',' crystal file name is undefined in '//nmlfile)
 end if
end if

ecpnl%stdout = stdout
ecpnl%fn_f = fn_f
ecpnl%fn_s = fn_s
ecpnl%gF = gF
ecpnl%gS = gS
ecpnl%tF = tF
ecpnl%tS = tS
ecpnl%npix = npix
ecpnl%nthreads = nthreads
ecpnl%dmin = dmin
ecpnl%thetac = thetac
ecpnl%filmthickness = filmthickness
ecpnl%datafile = datafile
ecpnl%xtalname = xtalname
ecpnl%xtalname2 = xtalname2
ecpnl%energyfile = energyfile
ecpnl%filmfile = filmfile
ecpnl%subsfile = subsfile
ecpnl%masterfile = masterfile
ecpnl%maskpattern = maskpattern
ecpnl%anglefile = anglefile
ecpnl%numangle_anglefile = numangle_anglefile
ecpnl%eulerconvention = eulerconvention
ecpnl%gammavalue = gammavalue
ecpnl%outputformat = outputformat
ecpnl%sampletilt = sampletilt
ecpnl%workingdistance = workingdistance
ecpnl%Rin = Rin
ecpnl%Rout = Rout

end subroutine GetECPNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetLACBEDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill lacbednl structure (used by EMLACBED.f90)
!
!> @param nmlfile namelist file name
!> @param lacbednl LACBED name list structure
!
!> @date 07/01/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetLACBEDNameList(nmlfile, lacbednl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetLACBEDNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(LACBEDNameListType),INTENT(INOUT)  :: lacbednl
!f2py intent(in,out) ::  lacbednl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: k(3)
integer(kind=irg)       :: fn(3)
integer(kind=irg)       :: maxHOLZ
integer(kind=irg)       :: numthick
integer(kind=irg)       :: npix
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: voltage
real(kind=sgl)          :: dmin
real(kind=sgl)          :: convergence
real(kind=sgl)          :: startthick
real(kind=sgl)          :: thickinc
real(kind=sgl)          :: minten
character(fnlen)        :: xtalname
character(fnlen)        :: outname

namelist /LACBEDlist/ xtalname, voltage, k, fn, dmin, convergence, minten, &
                              nthreads, startthick, thickinc, numthick, outname, npix, maxHOLZ

k = (/ 0, 0, 1 /)               ! beam direction [direction indices]
fn = (/ 0, 0, 1 /)              ! foil normal [direction indices]
maxHOLZ = 2                     ! maximum HOLZ layer index to be used for the output file; note that his number
                                ! does not affect the actual computations; it only determines which reflection 
                                ! families will end up in the output file
numthick = 10                   ! number of increments
npix = 256                      ! output arrays will have size npix x npix
nthreads = 1                    ! number of computational threads
voltage = 200000.0              ! acceleration voltage [V]
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
convergence = 25.0              ! beam convergence angle [mrad]
startthick = 10.0               ! starting thickness [nm]
thickinc = 10.0                 ! thickness increment
minten = 1.0E-6                 ! minimum intensity in diffraction disk to make it into the output file
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
outname = 'lacbedout.data'      ! output filename

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=LACBEDlist)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
  call FatalError('EMLACBED:',' structure file name is undefined in '//nmlfile)
end if
end if

lacbednl%k = k
lacbednl%fn = fn
lacbednl%maxHOLZ = maxHOLZ
lacbednl%numthick = numthick
lacbednl%npix = npix
lacbednl%nthreads = nthreads
lacbednl%voltage = voltage
lacbednl%dmin = dmin
lacbednl%convergence = convergence
lacbednl%startthick = startthick
lacbednl%thickinc = thickinc
lacbednl%minten = minten
lacbednl%xtalname = xtalname
lacbednl%outname = outname

end subroutine GetLACBEDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetCBEDNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill cbednl structure (used by EMCBED.f90)
!
!> @param nmlfile namelist file name
!> @param cbednl CBED name list structure
!
!> @date 11/24/18  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetCBEDNameList(nmlfile, cbednl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetCBEDNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(CBEDNameListType),INTENT(INOUT)    :: cbednl
!f2py intent(in,out) ::  cbednl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: k(3)
integer(kind=irg)       :: fn(3)
integer(kind=irg)       :: maxHOLZ
integer(kind=irg)       :: numthick
integer(kind=irg)       :: npix
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: voltage
real(kind=sgl)          :: camlen
real(kind=sgl)          :: klaue(2)
real(kind=sgl)          :: dmin
real(kind=sgl)          :: convergence
real(kind=sgl)          :: startthick
real(kind=sgl)          :: thickinc
character(fnlen)        :: xtalname
character(fnlen)        :: outname

namelist /CBEDlist/ xtalname, voltage, k, fn, dmin, convergence, klaue, camlen, &
                    nthreads, startthick, thickinc, numthick, outname, npix, maxHOLZ

k = (/ 0, 0, 1 /)               ! beam direction [direction indices]
fn = (/ 0, 0, 1 /)              ! foil normal [direction indices]
maxHOLZ = 2                     ! maximum HOLZ layer index to be used for the output file; note that his number
                                ! does not affect the actual computations; it only determines which reflection 
                                ! families will end up in the output file
klaue = (/ 0.0, 0.0 /)          ! Laue center coordinates
numthick = 10                   ! number of increments
npix = 256                      ! output arrays will have size npix x npix
nthreads = 1                    ! number of computational threads
voltage = 200.0                 ! acceleration voltage [kV]
camlen = 1000.0                 ! camera length [mm]
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
convergence = 25.0              ! beam convergence angle [mrad]
startthick = 10.0               ! starting thickness [nm]
thickinc = 10.0                 ! thickness increment
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
outname = 'undefined'           ! output filename

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=CBEDlist)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
  call FatalError('EMCBED:',' structure file name is undefined in '//nmlfile)
end if
end if

cbednl%k = k
cbednl%fn = fn
cbednl%maxHOLZ = maxHOLZ
cbednl%numthick = numthick
cbednl%npix = npix
cbednl%nthreads = nthreads
cbednl%klaue = klaue
cbednl%voltage = voltage
cbednl%camlen = camlen
cbednl%dmin = dmin
cbednl%convergence = convergence
cbednl%startthick = startthick
cbednl%thickinc = thickinc
cbednl%xtalname = xtalname
cbednl%outname = outname

end subroutine GetCBEDNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPpatternNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMECPpattern.f90)
!
!> @param nmlfile namelist file name
!> @param emnl ECP name list structure
!
!> @date 06/19/14  SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetECPpatternNameList(nmlfile,ecpnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPpatternNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(ECPpatternNameListType),INTENT(INOUT)             :: ecpnl
!f2py intent(in,out) ::  ecpnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: npix
real(kind=sgl)          :: thetac
real(kind=sgl)          :: k(3)
character(fnlen)        :: masterfile
character(fnlen)        :: outname

! define the IO namelist to facilitate passing variables to the program.
namelist /ECPvars/ stdout, npix, masterfile, outname, thetac, k

! set the input parameters to default values (except for masterfile, which must be present)
stdout = 6
npix = 256
thetac = 5.0
k = (/0.0,0.0,1.0/)
masterfile = 'undefined'        ! default filename for master data from EMECPmaster
outname = 'ECP.data'  ! default filename for final output

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=ECPvars)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(masterfile).eq.'undefined') then
call FatalError('EMECP:',' master file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
ecpnl%stdout = stdout
ecpnl%npix = npix
ecpnl%thetac = thetac
ecpnl%k = k
ecpnl%masterfile = masterfile
ecpnl%outname = outname

end subroutine GetECPpatternNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetPEDkinNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill pednl structure (used by EMpedKIN.f90)
!
!> @param nmlfile namelist file name
!> @param pednl PED name list structure
!
!> @date 03/02/15 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetPEDkinNameList(nmlfile,pednl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetPEDkinNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(PEDkinNameListType),INTENT(INOUT)             :: pednl
!f2py intent(in,out) ::  pednl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: npix
integer(kind=irg)       :: ncubochoric
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: voltage
real(kind=sgl)          :: thickness
real(kind=sgl)          :: rnmpp
real(kind=sgl)          :: dmin
character(fnlen)        :: xtalname
character(fnlen)        :: outname
character(fnlen)        :: eulerfile
character(4)            :: sampling

! define the IO namelist to facilitate passing variables to the program.
namelist /PEDkinNameList/ xtalname, voltage, npix, rnmpp, ncubochoric, nthreads, &
                              thickness, outname , dmin, sampling, eulerfile

! set the input parameters to default values (except for xtalname, which must be present)
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
voltage = 200.0              ! acceleration voltage [kV]
nthreads = 1                    ! number of OpenMP threads to start
thickness = 10.0                ! sample thickness [nm]
npix = 256                      ! output arrays will have size npix x npix
outname = 'pedout.data'         ! output filename
dmin = 0.04                     ! smallest d-spacing [nm]
ncubochoric = 100               ! number of samples along the cubochoric edge length
rnmpp = 0.2                     ! nm^{-1} per pattern pixel
sampling = 'dict'
eulerfile = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=PEDkinNameList)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(xtalname).eq.'undefined') then
        call FatalError('EMPED:',' crystal structure file name is undefined in '//nmlfile)
    end if

    if (sampling .eq. 'file' .and. trim(eulerfile) .eq. 'undefined') then
        call FatalError('EMPED:',' euler angle file name is undefined in '//nmlfile)
    end if

end if

! if we get here, then all appears to be ok, and we need to fill in the pednl fields
pednl%xtalname = xtalname
pednl%voltage = voltage
pednl%thickness = thickness
pednl%dmin = dmin
pednl%npix = npix
pednl%nthreads = nthreads
pednl%outname = outname
pednl%rnmpp = rnmpp
pednl%ncubochoric = ncubochoric
pednl%sampling = sampling
pednl%eulerfile = eulerfile

end subroutine GetPEDKINNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetPEDZANameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill pednl structure (used by EMPEDZA.f90)
!
!> @param nmlfile namelist file name
!> @param pednl PED name list structure
!
!> @date 07/09/14 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetPEDZANameList(nmlfile,pednl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetPEDZANameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(PEDZANameListType),INTENT(INOUT)   :: pednl
!f2py intent(in,out) ::  pednl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: k(3)
integer(kind=irg)       :: fn(3)
integer(kind=irg)       :: precsample
integer(kind=irg)       :: precazimuthal
integer(kind=irg)       :: npix
integer(kind=irg)       :: nthreads
real(kind=sgl)          :: voltage
real(kind=sgl)          :: dmin
real(kind=sgl)          :: precangle
real(kind=sgl)          :: prechalfwidth
real(kind=sgl)          :: thickness
real(kind=sgl)          :: camlen
character(5)            :: filemode
character(fnlen)        :: xtalname
character(fnlen)        :: outname

! define the IO namelist to facilitate passing variables to the program.
namelist /EMPEDZA/ stdout, xtalname, voltage, k, fn, dmin, precangle, prechalfwidth, precsample, precazimuthal, &
                              thickness,  outname, npix, camlen, filemode, nthreads

! set the input parameters to default values (except for xtalname, which must be present)
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
stdout = 6                      ! standard output
voltage = 200000.0              ! acceleration voltage [V]
k = (/ 0, 0, 1 /)               ! beam direction [direction indices]
fn = (/ 0, 0, 1 /)              ! foil normal [direction indices]
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
precangle = 10.472              ! beam precession angle [mrad]; default = 0.6 degrees
prechalfwidth = 0.25            ! beam half width in the tilt direction [mrad]
nthreads = 1                    ! number of OpenMP threads to start
precsample = 10                 ! number of samples (concentric circles) in beam half width (total = 2*precsample + 1)
precazimuthal = 360             ! number of azimuthal samples for each precession circle
thickness = 10.0                ! sample thickness [nm]
filemode = 'total'              ! 'total' mode or 'eachp'
npix = 256                      ! output arrays will have size npix x npix
outname = 'pedout.data'         ! output filename
camlen = 1000.0                 ! camera length [mm]


if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=EMPEDZA)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
call FatalError('EMPEDZA:',' crystal structure file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the pednl fields
pednl%xtalname = xtalname
pednl%stdout = stdout
pednl%voltage = voltage
pednl%k = k
pednl%fn = fn
pednl%dmin = dmin
pednl%precangle = precangle
pednl%prechalfwidth = prechalfwidth
pednl%precsample = precsample
pednl%precazimuthal = precazimuthal
pednl%thickness = thickness
pednl%filemode = filemode
pednl%npix = npix
pednl%nthreads = nthreads
pednl%outname = outname
pednl%camlen = camlen

end subroutine GetPEDZANameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECCINameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill eccinl structure (used by EMECCI.f90)
!
!> @param nmlfile namelist file name
!> @param eccinl ECCI name list structure
!
!> @date 10/04/14 MDG 1.0 new routine
!> @date 11/24/15 MDG 1.1 adapted for new namelist variables
!--------------------------------------------------------------------------
recursive subroutine GetECCINameList(nmlfile,eccinl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECCINameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(ECCINameListType),INTENT(INOUT)    :: eccinl
!f2py intent(in,out) ::  eccinl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)                       :: i

integer(kind=irg)       :: stdout
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: k(3)
integer(kind=irg)       :: nktstep
integer(kind=irg)       :: DF_npix
integer(kind=irg)       :: DF_npiy
real(kind=sgl)          :: voltage
real(kind=sgl)          :: dkt
real(kind=sgl)          :: ktmax
real(kind=sgl)          :: lauec(2)
real(kind=sgl)          :: lauec2(2)
real(kind=sgl)          :: dmin
real(kind=sgl)          :: DF_L
real(kind=sgl)          :: DF_slice
character(4)            :: dispmode
character(4)            :: summode
character(5)            :: progmode
character(fnlen)        :: xtalname
character(fnlen)        :: montagename
character(fnlen)        :: defectfilename
character(fnlen)        :: DDDfilename
character(fnlen)        :: dispfile
character(fnlen)        :: dataname
character(fnlen)        :: ECPname
character(fnlen)        :: sgname

! define the IO namelist to facilitate passing variables to the program.
namelist / ECCIlist / DF_L, DF_npix, DF_npiy, DF_slice, dmin, sgname, stdout, &
                      progmode, dispfile, ktmax, dkt, ECPname, summode, lauec, lauec2, &
                      dispmode, nthreads, xtalname, voltage, k, nktstep, &
                      dataname, defectfilename, montagename, DDDfilename

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
nthreads = 1
k = (/ 0,0,1 /)
nktstep = 20
DF_npix = 256
DF_npiy = 256
voltage = 30.
dkt = 0.1
ktmax = 5.0
lauec = (/ 0.0, 0.0 /)
lauec2 = (/ 0.0, 0.0 /)
dmin = 0.1
DF_L = 1.0
DF_slice = 1.0
dispmode = 'not'
summode = 'diag'
progmode = 'array'
xtalname = 'undefined'
montagename = 'undefined'
defectfilename = 'undefined'
DDDfilename = 'undefined'
dispfile = 'displacements.data'
dataname = 'ECCIout.data'
ECPname = 'undefined'
sgname = 'nofile'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=ECCIlist)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
  call FatalError('EMECCI:',' crystal structure file name is undefined in '//nmlfile)
end if

! make sure the ECPname variable has been properly defined
if (trim(ECPname).eq.'undefined') then
  call FatalError('EMECCI:',' ECP pattern file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
eccinl%stdout = stdout
eccinl%nthreads = nthreads
eccinl%k = k
eccinl%nktstep = nktstep
eccinl%DF_npix = DF_npix
eccinl%DF_npiy = DF_npiy
eccinl%voltage = voltage
eccinl%dkt = dkt
eccinl%ktmax = ktmax
eccinl%lauec = lauec
eccinl%lauec2 = lauec2
eccinl%dmin = dmin
eccinl%DF_L = DF_L
eccinl%DF_slice = DF_slice
eccinl%dispmode = dispmode
eccinl%summode = summode
eccinl%progmode = progmode
eccinl%xtalname = xtalname
eccinl%montagename = montagename
eccinl%defectfilename = defectfilename
eccinl%DDDfilename = DDDfilename
eccinl%dispfile = dispfile
eccinl%dataname = dataname
eccinl%ECPname = ECPname
eccinl%sgname = sgname

end subroutine GetECCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetRFZNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill rfznl structure (used by EMsampleRFZ.f90)
!
!> @param nmlfile namelist file name
!> @param rfznl RFZ name list structure
!
!> @date 12/09/14 MDG 1.0 new routine
!> @date 08/18/15 MDG 1.1 added options for all seven representations
!> @date 01/17/15 MDG 1.2 added gridtype option
!> @date 12/22/16 MDG 1.3 added new sampling mode
!> @date 02/01/17 MDG 1.4 added conical sampling mode
!--------------------------------------------------------------------------
recursive subroutine GetRFZNameList(nmlfile,rfznl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetRFZNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(RFZNameListType),INTENT(INOUT)             :: rfznl
!f2py intent(in,out) ::  rfznl
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)                               :: pgnum, nsteps, gridtype
real(kind=dbl)                                  :: rodrigues(4), qFZ(4), axFZ(4), maxmisor, conevector(3), semiconeangle
character(fnlen)                                :: samplemode
character(fnlen)                                :: xtalname
character(fnlen)                                :: euoutname
character(fnlen)                                :: cuoutname
character(fnlen)                                :: hooutname
character(fnlen)                                :: rooutname
character(fnlen)                                :: quoutname
character(fnlen)                                :: omoutname
character(fnlen)                                :: axoutname

! namelist components
namelist / RFZlist / pgnum, nsteps, gridtype, euoutname, cuoutname, hooutname, rooutname, quoutname, omoutname, axoutname, &
                     samplemode, rodrigues, maxmisor, conevector, semiconeangle, xtalname, qFZ, axFZ

! initialize to default values
pgnum = 32
nsteps = 50
gridtype = 0
rodrigues = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)  ! initialize as the identity rotation
qFZ= (/ 1.D0, 0.D0, 0.D0, 0.D0 /)         ! initialize as the identity rotation
axFZ= (/ 0.D0, 0.D0, 1.D0, 0.D0 /)         ! initialize as the identity rotation
maxmisor = 5.D0                           ! in degrees
samplemode = 'RFZ'                        ! or 'MIS' for sampling inside a ball with constant misorientation w.r.t. rodrigues
! or 'CON' for conical sampling around a unitvector for a cone with semi opening angle semiconangle
conevector = (/ 0.D0, 0.D0, 1.D0 /)       ! default unit vector for cone axis
semiconeangle = 2.0                       ! default opening semi-angle (in degrees)
euoutname = 'undefined'
xtalname = 'undefined'
cuoutname = 'undefined'
hooutname = 'undefined'
rooutname = 'undefined'
quoutname = 'undefined'
omoutname = 'undefined'
axoutname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=RFZlist)
close(UNIT=dataunit,STATUS='keep')
end if

! and copy the variables to the rfznl variable
rfznl%pgnum  = pgnum
rfznl%nsteps = nsteps
rfznl%gridtype = gridtype
rfznl%rodrigues = rodrigues
rfznl%qFZ = qFZ
rfznl%axFZ = axFZ
rfznl%maxmisor = maxmisor
rfznl%samplemode = samplemode
rfznl%conevector = conevector
rfznl%semiconeangle = semiconeangle
rfznl%xtalname = xtalname
rfznl%euoutname = euoutname
rfznl%cuoutname = cuoutname
rfznl%hooutname = hooutname
rfznl%rooutname = rooutname
rfznl%quoutname = quoutname
rfznl%omoutname = omoutname
rfznl%axoutname = axoutname

end subroutine GetRFZNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetDictIndxOpenCLNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill DictIndxOpenCLListType (used by EMDictIndxOpenCL.f90)
!
!> @param nmlfile namelist file name
!> @param DictIndxOpenCL name list structure
!
!> @date 13/01/15 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetDictIndxOpenCLNameList(nmlfile,dictindxnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetDictIndxOpenCLNameList

use error
use local

IMPLICIT NONE

character(fnlen),INTENT(IN)                                 :: nmlfile
type(DictIndxOpenCLListType),INTENT(INOUT)                  :: dictindxnl
!f2py intent(in,out) ::  dictindxnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)                                           :: numexptsingle
integer(kind=irg)                                           :: numdictsingle
integer(kind=irg)                                           :: totnumexpt
integer(kind=irg)                                           :: totnumdict
integer(kind=irg)                                           :: imght
integer(kind=irg)                                           :: imgwd
integer(kind=irg)                                           :: nnk
character(fnlen)                                            :: exptfile
character(fnlen)                                            :: dictfile
character(fnlen)                                            :: eulerfile
logical                                                     :: MeanSubtraction
logical                                                     :: patternflip

! define the IO namelist to facilitate passing variables to the program.
namelist /DictIndxOpenCLvars/ numexptsingle, numdictsingle, totnumexpt, totnumdict,&
        imght, imgwd, exptfile, dictfile, eulerfile, nnk, MeanSubtraction, patternflip

! set some of the input parameters to default values 
numdictsingle = 1024
numexptsingle = 1024
imght = 0
imgwd = 0
nnk = 40
exptfile = 'undefined'
dictfile = 'undefined'
eulerfile = 'undefined'
totnumdict = 0
totnumexpt = 0
MeanSubtraction = .TRUE.
patternflip = .TRUE.

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=DictIndxOpenCLvars)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(exptfile).eq.'undefined') then
    call FatalError('EMDictIndxOpenCL:',' experimental file name is undefined in '//nmlfile)
end if

if (trim(dictfile).eq.'undefined') then
    call FatalError('EMDictIndxOpenCL:',' dictionary file name is undefined in '//nmlfile)
end if

if (trim(eulerfile).eq.'undefined') then
    call FatalError('EMDictIndxOpenCL:',' euler angle file name is undefined in '//nmlfile)
end if

if (totnumexpt .eq. 0) then
    call FatalError('EMDictIndxOpenCL:',' total number of experimental patterns is undefined in '//nmlfile)
end if

if (totnumdict .eq. 0) then
    call FatalError('EMDictIndxOpenCL:',' total number of dictionary patterns is undefined in '//nmlfile)
end if

if (imght .eq. 0) then
    call FatalError('EMDictIndxOpenCL:',' height of single pattern is undefined in '//nmlfile)
end if

if (imgwd .eq. 0) then
    call FatalError('EMDictIndxOpenCL:',' width of single pattern is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields

dictindxnl%numexptsingle = numexptsingle
dictindxnl%numdictsingle = numdictsingle
dictindxnl%imght = imght
dictindxnl%imgwd = imgwd
dictindxnl%exptfile = exptfile
dictindxnl%dictfile = dictfile
dictindxnl%eulerfile = eulerfile
dictindxnl%totnumdict = totnumdict
dictindxnl%totnumexpt = totnumexpt
dictindxnl%nnk = nnk
dictindxnl%MeanSubtraction = MeanSubtraction
dictindxnl%patternflip = patternflip

end subroutine GetDictIndxOpenCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetPEDIndxNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill PEDKINIndxListType (used by EMPEDIndexing.f90)
!
!> @param nmlfile namelist file name
!> @param pednl PEDKINIndx name list structure
!
!> @date 13/01/15 SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetPEDIndxNameList(nmlfile,pednl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetPEDIndxNameList

use error
use local

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(PEDKINIndxListType),INTENT(INOUT)  :: pednl
!f2py intent(in,out) ::  pednl
logical,OPTIONAL,INTENT(IN)             :: initonly
logical                                 :: skipread = .FALSE.

integer(kind=irg)                       :: npix
integer(kind=irg)                       :: ncubochoric
real(kind=sgl)                          :: voltage
real(kind=sgl)                          :: dmin
real(kind=sgl)                          :: thickness
real(kind=sgl)                          :: rnmpp ! reciprocal nanometers per pixel
character(fnlen)                        :: xtalname
integer(kind=irg)                       :: numexptsingle
integer(kind=irg)                       :: numdictsingle
integer(kind=irg)                       :: ipf_ht
integer(kind=irg)                       :: ipf_wd
integer(kind=irg)                       :: nnk
real(kind=sgl)                          :: sgmax ! maximum sg value for a beam to be considered
real(kind=sgl)                          :: ww ! 2*ww+1 is the size of the spot
real(kind=sgl)                          :: var ! variance of gaussian peak
character(fnlen)                        :: exptfile
character(fnlen)                        :: datafile
character(fnlen)                        :: ctffile
character(fnlen)                        :: tmpfile
integer(kind=irg)                       :: devid
integer(kind=irg)                       :: platid
integer(kind=irg)                       :: nthreads


! define the IO namelist to facilitate passing variables to the program.
namelist /inputlist/ npix, ncubochoric, numexptsingle, numdictsingle, voltage, dmin, thickness, rnmpp, xtalname, &
exptfile, nnk, ipf_ht, ipf_wd, nthreads, sgmax, ww, var, devid, platid, datafile, ctffile, tmpfile

! set some of the input parameters to default values
npix = 0
ncubochoric = 50
voltage = 200000.0
dmin = 0.04
thickness = 50.0
rnmpp = 0.20
xtalname = 'undefined'
numdictsingle = 1024
numexptsingle = 1024
nnk = 40
exptfile = 'undefined'
datafile = 'undefined'
ctffile = 'undefined'
tmpfile = 'EMPEDDI_tmp.data'
sgmax = 0.50
ww = 3
var = 0.0020
ipf_ht = 0
ipf_wd = 0
nthreads = 1
platid = 1
devid = 1

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=inputlist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (npix .eq. 0) then
        call FatalError('EMPEDIndexing:',' size of dictionary pattern not specified or set to 0 in '//nmlfile)
    end if

    if (trim(xtalname) .eq. 'undefined') then
        call FatalError('EMPEDIndexing:',' crystal file undefined in '//nmlfile)
    end if

    if (trim(exptfile).eq.'undefined') then
        call FatalError('EMPEDIndexing:',' experimental file name is undefined in '//nmlfile)
    end if


    if (ipf_ht .eq. 0) then
        call FatalError('EMPEDIndexing:',' total number of experimental patterns is either set to 0 or is undefined in '//nmlfile)
    end if

    if (ipf_wd .eq. 0) then
        call FatalError('EMPEDIndexing:',' total number of experimental patterns is either set to 0 or is undefined in '//nmlfile)
    end if


end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
pednl%npix = npix
pednl%ncubochoric = ncubochoric
pednl%voltage = voltage
pednl%dmin = dmin
pednl%thickness = thickness
pednl%rnmpp = rnmpp
pednl%xtalname = xtalname
pednl%numexptsingle = numexptsingle
pednl%numdictsingle = numdictsingle
pednl%exptfile = exptfile
pednl%datafile = datafile
pednl%ctffile = ctffile 
pednl%nnk = nnk
pednl%sgmax = sgmax
pednl%ww = ww
pednl%var = var
pednl%ipf_ht = ipf_ht
pednl%ipf_wd = ipf_wd
pednl%nthreads = nthreads
pednl%platid = platid
pednl%devid = devid
pednl%tmpfile = tmpfile

end subroutine GetPEDIndxNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDDIpreviewNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDDIpreview.f90)
!
!> @param nmlfile namelist file name
!> @param enl name list structure
!
!> @date 01/24/18 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDDIpreviewNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDDIpreviewNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EBSDDIpreviewNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: hipasswnsteps
integer(kind=irg)       :: nregionsmin
integer(kind=irg)       :: nregionsmax
integer(kind=irg)       :: nregionsstepsize
integer(kind=irg)       :: patx
integer(kind=irg)       :: paty
integer(kind=irg)       :: ipf_wd
integer(kind=irg)       :: ipf_ht
integer(kind=irg)       :: numav
real(kind=sgl)          :: hipasswmax
character(fnlen)        :: patternfile
character(fnlen)        :: tifffile
character(fnlen)        :: exptfile
character(fnlen)        :: inputtype
character(fnlen)        :: hDFstrings(10)

namelist / EBSDDIpreviewdata / numsx, numsy, hipasswmax, hipasswnsteps, nregionsstepsize, &
          nregionsmax, nregionsmin, patx, paty, tifffile, exptfile, inputtype, HDFstrings, ipf_wd, &
          ipf_ht, patternfile, numav

! set the input parameters to default values
numsx = 0
numsy = 0
hipasswmax = 0.5
hipasswnsteps = 10
nregionsmin = 1
nregionsmax = 10
nregionsstepsize = 1
patx = 1
paty = 1
ipf_wd = 100
ipf_ht = 100
numav = 0
patternfile = 'undefined'
tifffile = 'undefined'
exptfile = 'undefined'
inputtype = 'Binary'
HDFstrings = ''

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=EBSDDIpreviewdata)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
        
    if (trim(exptfile).eq.'undefined') then
        call FatalError('GetEBSDDIpreviewNameList:',' experimental file name is undefined in '//nmlfile)
    end if

    if (trim(tifffile).eq.'undefined') then
        call FatalError('GetEBSDDIpreviewNameList:',' TIFF file name is undefined in '//nmlfile)
    end if

    if (numsx.eq.0) then 
        call FatalError('GetEBSDDIpreviewNameList:',' pattern size numsx is zero in '//nmlfile)
    end if

    if (numsy.eq.0) then 
        call FatalError('GetEBSDDIpreviewNameList:',' pattern size numsy is zero in '//nmlfile)
    end if
end if

enl%numsx = numsx
enl%numsy = numsy
enl%hipasswnsteps = hipasswnsteps
enl%nregionsmin = nregionsmin
enl%nregionsmax = nregionsmax
enl%nregionsstepsize = nregionsstepsize
enl%patx = patx
enl%paty = paty
enl%ipf_wd = ipf_wd
enl%ipf_ht = ipf_ht
enl%numav = numav
enl%patternfile = patternfile
enl%hipasswmax = hipasswmax
enl%tifffile = tifffile
enl%exptfile = exptfile
enl%inputtype = inputtype
enl%HDFstrings = HDFstrings

end subroutine GetEBSDDIpreviewNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDIndxNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDI.f90)
!
!> @param nmlfile namelist file name
!> @param enl EBSD indexing name list structure
!
!> @date 06/10/15  SS 1.0 new routine
!> @date 11/19/15  SS 1.1 added new variables
!> @date 01/26/16  SS 1.2 adjusted for EBSDIndexing
!--------------------------------------------------------------------------
recursive subroutine GetEBSDIndexingNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDIndexingNameList

use error
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EBSDIndexingNameListType),INTENT(INOUT)      :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: exptnumsx
integer(kind=irg)                                 :: exptnumsy
integer(kind=irg)                                 :: numsx
integer(kind=irg)                                 :: numsy
integer(kind=irg)                                 :: ROI(4)
integer(kind=irg)                                 :: binning
integer(kind=irg)                                 :: energyaverage
integer(kind=irg)                                 :: devid
integer(kind=irg)                                 :: multidevid(8)
integer(kind=irg)                                 :: usenumd
integer(kind=irg)                                 :: platid
integer(kind=irg)                                 :: nregions
integer(kind=irg)                                 :: nlines
real(kind=sgl)                                    :: L
real(kind=sgl)                                    :: thetac
real(kind=sgl)                                    :: delta
real(kind=sgl)                                    :: xpc
real(kind=sgl)                                    :: ypc
real(kind=sgl)                                    :: isangle
real(kind=sgl)                                    :: gammavalue
real(kind=dbl)                                    :: beamcurrent
real(kind=dbl)                                    :: dwelltime
real(kind=dbl)                                    :: hipassw
real(kind=sgl)                                    :: omega
real(kind=sgl)                                    :: stepX
real(kind=sgl)                                    :: stepY
integer(kind=irg)                                 :: nthreads
character(1)                                      :: maskpattern
character(1)                                      :: keeptmpfile
character(3)                                      :: scalingmode
character(3)                                      :: similaritymetric
character(3)                                      :: Notify
character(fnlen)                                  :: dotproductfile
character(fnlen)                                  :: masterfile
real(kind=sgl)                                    :: energymin
real(kind=sgl)                                    :: energymax
character(1)                                      :: spatialaverage
character(fnlen)                                  :: tmpfile
character(fnlen)                                  :: datafile
character(fnlen)                                  :: ctffile
character(fnlen)                                  :: avctffile
character(fnlen)                                  :: angfile
character(fnlen)                                  :: eulerfile
character(fnlen)                                  :: inputtype
character(fnlen)                                  :: HDFstrings(10)
character(fnlen)                                  :: refinementNMLfile
integer(kind=irg)                                 :: ncubochoric
integer(kind=irg)                                 :: numexptsingle
integer(kind=irg)                                 :: numdictsingle
integer(kind=irg)                                 :: ipf_ht
integer(kind=irg)                                 :: ipf_wd
integer(kind=irg)                                 :: nnk
integer(kind=irg)                                 :: nnav
integer(kind=irg)                                 :: nosm
integer(kind=irg)                                 :: nism
integer(kind=irg)                                 :: maskradius
integer(kind=irg)                                 :: section
character(fnlen)                                  :: exptfile
character(fnlen)                                  :: dictfile
character(fnlen)                                  :: maskfile
character(fnlen)                                  :: indexingmode

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDIndexingdata / thetac, delta, numsx, numsy, xpc, ypc, masterfile, devid, platid, &
beamcurrent, dwelltime, binning, gammavalue, energymin, spatialaverage, nregions, nlines, exptnumsx, exptnumsy, &
scalingmode, maskpattern, energyaverage, L, omega, nthreads, energymax, datafile, angfile, ctffile, &
ncubochoric, numexptsingle, numdictsingle, ipf_ht, ipf_wd, nnk, nnav, exptfile, maskradius, inputtype, &
dictfile, indexingmode, hipassw, stepX, stepY, tmpfile, avctffile, nosm, eulerfile, Notify, maskfile, &
section, HDFstrings, ROI, keeptmpfile, multidevid, usenumd, nism, isangle, refinementNMLfile, similaritymetric

! set the input parameters to default values (except for xtalname, which must be present)
ncubochoric     = 50
numexptsingle   = 1024
numdictsingle   = 1024
platid          = 1
devid           = 1
usenumd         = 1
multidevid      = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)
nregions        = 10
nlines          = 3
nnk             = 50
nnav            = 20
nosm            = 20
nism            = 5
exptfile        = 'undefined'
numsx           = 0             ! [dimensionless] no longer used starting in version 5.0.3
numsy           = 0             ! [dimensionless] no longer used starting in version 5.0.3
exptnumsx       = 0             ! [dimensionless] size for *experimental* patterns in input file
exptnumsy       = 0             ! [dimensionless] 
ROI             = (/ 0, 0, 0, 0 /)  ! Region of interest (/ x0, y0, w, h /)
maskradius      = 240
binning         = 1             ! binning mode  (1, 2, 4, or 8)
L               = 20000.0       ! [microns]
! the following parameter is no longer used but can still be in older namelist files
energyaverage   = -1            ! apply energy averaging (1) or not (0); useful for dictionary computations
thetac          = 0.0           ! [degrees]
delta           = 25.0          ! [microns]
xpc             = 0.0           ! [pixels]
ypc             = 0.0           ! [pixels]
gammavalue      = 1.0           ! gamma factor
isangle         = 1.5
beamcurrent     = 14.513D0      ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
hipassw         = 0.05D0        ! hi pass inverted Gaussian mask parameter
stepX           = 1.0           ! sampling step size along X
stepY           = 1.0           ! sampling step size along Y
keeptmpfile     = 'n'
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
Notify          = 'Off'
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
similaritymetric = 'ndp'
masterfile      = 'undefined'   ! filename
dotproductfile  = 'undefined'
energymin       = 10.0
energymax       = 20.0
ipf_ht          = 100
ipf_wd          = 100
nthreads        = 1
spatialaverage  = 'n'
datafile        = 'undefined'
ctffile         = 'undefined'
avctffile       = 'undefined'
angfile         = 'undefined'
eulerfile       = 'undefined'
omega           = 0.0
tmpfile         = 'EMEBSDDict_tmp.data'
dictfile        = 'undefined'
maskfile        = 'undefined'
refinementNMLfile = 'undefined'
indexingmode    = 'dynamic'
section         = 0
inputtype       = 'Binary'    ! Binary, EMEBSD, TSLHDF, TSLup2, OxfordHDF, OxfordBinary, BrukerHDF 
HDFstrings      = ''

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=EBSDIndexingdata)
    close(UNIT=dataunit,STATUS='keep')

    if (trim(indexingmode) .eq. 'static') then
        if (trim(dictfile) .eq. 'undefined') then
            call FatalError('EMEBSDIndexing:',' dictionary file name is undefined in '//nmlfile)
        end if
    end if
        

! check for required entries
    if (trim(indexingmode) .eq. 'dynamic') then
        
        if (trim(masterfile).eq.'undefined') then
            call FatalError('EMEBSDIndexing:',' master pattern file name is undefined in '//nmlfile)
        end if

    end if

    if (trim(exptfile).eq.'undefined') then
        call FatalError('EMEBSDIndexing:',' experimental file name is undefined in '//nmlfile)
    end if

    if (numsx.eq.0) then 
        call FatalError('EMEBSDIndexing:',' pattern size numsx is zero in '//nmlfile)
    end if

    if (numsy.eq.0) then 
        call FatalError('EMEBSDIndexing:',' pattern size numsy is zero in '//nmlfile)
    end if

    if (energyaverage.ne.-1) then
        call Message('EMEBSDIndexing Warning: energyaverage parameter is no longer used;')
        call Message('   ------> parameter value will be ignored during program run ')
    end if 
end if



! if we get here, then all appears to be ok, and we need to fill in the enl fields

enl%devid         = devid
enl%multidevid    = multidevid
enl%usenumd       = usenumd
enl%platid        = platid
enl%nregions      = nregions
enl%nlines        = nlines
enl%maskpattern   = maskpattern
enl%keeptmpfile   = keeptmpfile
enl%exptfile      = exptfile
enl%nnk           = nnk
enl%nnav          = nnav
enl%nosm          = nosm
enl%nism          = nism
enl%isangle       = isangle
enl%ipf_ht        = ipf_ht
enl%ipf_wd        = ipf_wd
enl%nthreads      = nthreads
enl%datafile      = datafile
enl%tmpfile       = tmpfile
enl%ctffile       = ctffile
enl%avctffile     = avctffile
enl%angfile       = angfile
enl%eulerfile     = eulerfile
enl%maskradius    = maskradius
enl%numdictsingle = numdictsingle
enl%numexptsingle = numexptsingle
enl%hipassw       = hipassw
enl%masterfile    = masterfile
enl%energyfile    = masterfile
enl%maskfile      = maskfile
enl%StepX         = stepX
enl%StepY         = stepY
enl%indexingmode  = trim(indexingmode)
enl%Notify        = Notify
enl%section       = section
enl%inputtype     = inputtype
enl%HDFstrings    = HDFstrings
enl%L             = L
enl%numsx         = numsx
enl%numsy         = numsy
enl%exptnumsx     = exptnumsx
enl%exptnumsy     = exptnumsy
enl%binning       = binning
enl%ROI           = ROI
! following parameter is no longer used but may be present in older nml files.
enl%energyaverage = -1 ! energyaverage
enl%thetac        = thetac
enl%delta         = delta
enl%xpc           = xpc
enl%ypc           = ypc
enl%gammavalue    = gammavalue
enl%beamcurrent   = beamcurrent
enl%dwelltime     = dwelltime
enl%scalingmode   = scalingmode
enl%similaritymetric = similaritymetric
enl%ncubochoric   = ncubochoric
enl%omega         = omega
enl%energymin     = energymin
enl%energymax     = energymax
enl%spatialaverage= spatialaverage
enl%dictfile      = dictfile 
enl%refinementNMLfile = refinementNMLfile

end subroutine GetEBSDIndexingNameList

! !--------------------------------------------------------------------------
! !
! ! SUBROUTINE: GetSphInxNameList
! !
! !> @author Marc De Graef, Carnegie Mellon University
! !
! !> @brief read namelist file and fill enl structure (used by EMSphInx.f90)
! !
! !> @param nmlfile namelist file name
! !> @param enl SphInx indexing name list structure
! !
! !> @date 06/17/19 MDG 1.0 new routine (based on regular EMEBSDDI name list structure)
! !--------------------------------------------------------------------------
! recursive subroutine GetSphInxNameList(nmlfile, enl, initonly)
! !DEC$ ATTRIBUTES DLLEXPORT :: GetSphInxNameList

! use error
! use io

! IMPLICIT NONE

! character(fnlen),INTENT(IN)                       :: nmlfile
! type(SphInxNameListType),INTENT(INOUT)            :: enl
!f2py intent(in,out) ::  enl
! logical,OPTIONAL,INTENT(IN)                       :: initonly

! logical                                           :: skipread = .FALSE.

! integer(kind=irg)       :: numexptsingle
! integer(kind=irg)       :: numdictsingle
! integer(kind=irg)       :: ipf_ht
! integer(kind=irg)       :: ipf_wd
! integer(kind=irg)       :: ROI(4)
! integer(kind=irg)       :: maskradius
! character(fnlen)        :: exptfile
! integer(kind=irg)       :: numsx
! integer(kind=irg)       :: numsy
! integer(kind=irg)       :: binning
! integer(kind=irg)       :: nthreads
! integer(kind=irg)       :: energyaverage
! integer(kind=irg)       :: devid
! integer(kind=irg)       :: usenumd
! integer(kind=irg)       :: multidevid(8)
! integer(kind=irg)       :: platid
! integer(kind=irg)       :: nregions
! real(kind=sgl)          :: L
! real(kind=sgl)          :: thetac
! real(kind=sgl)          :: delta
! real(kind=sgl)          :: omega
! real(kind=sgl)          :: xpc
! real(kind=sgl)          :: ypc
! real(kind=sgl)          :: stepX
! real(kind=sgl)          :: stepY
! real(kind=sgl)          :: energymin
! real(kind=sgl)          :: energymax
! real(kind=sgl)          :: gammavalue
! real(kind=dbl)          :: beamcurrent
! real(kind=dbl)          :: dwelltime
! real(kind=dbl)          :: hipassw
! character(1)            :: maskpattern
! character(3)            :: scalingmode
! character(3)            :: Notify
! character(1)            :: keeptmpfile
! character(fnlen)        :: anglefile
! character(fnlen)        :: masterfile
! character(fnlen)        :: energyfile
! character(fnlen)        :: datafile
! character(fnlen)        :: tmpfile
! character(fnlen)        :: ctffile
! character(fnlen)        :: angfile
! character(fnlen)        :: inputtype
! character(fnlen)        :: HDFstrings(10)

! ! define the IO namelist to facilitate passing variables to the program.
! namelist  / EBSDIndexingdata / thetac, delta, numsx, numsy, xpc, ypc, masterfile, devid, platid, &
!                                beamcurrent, dwelltime, binning, gammavalue, energymin, nregions, &
!                                scalingmode, maskpattern, L, omega, nthreads, energymax, datafile, angfile, ctffile, &
!                                numexptsingle, numdictsingle, ipf_ht, ipf_wd, exptfile, maskradius, inputtype, &
!                                hipassw, stepX, stepY, tmpfile, Notify, &
!                                HDFstrings, ROI,  multidevid, usenumd

! ! set the input parameters to default values (except for xtalname, which must be present)
! numexptsingle   = 1024
! numdictsingle   = 1024
! platid          = 1
! devid           = 1
! usenumd         = 1
! multidevid      = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)
! nregions        = 10
! exptfile        = 'undefined'
! numsx           = 0             ! [dimensionless]
! numsy           = 0             ! [dimensionless]
! ROI             = (/ 0, 0, 0, 0 /)  ! Region of interest (/ x0, y0, w, h /)
! maskradius      = 240
! binning         = 1             ! binning mode  (1, 2, 4, or 8)
! L               = 20000.0       ! [microns]
! thetac          = 0.0           ! [degrees]
! delta           = 25.0          ! [microns]
! xpc             = 0.0           ! [pixels]
! ypc             = 0.0           ! [pixels]
! gammavalue      = 1.0           ! gamma factor
! beamcurrent     = 14.513D0      ! beam current (actually emission current) in nano ampere
! dwelltime       = 100.0D0       ! in microseconds
! hipassw         = 0.05D0        ! hi pass inverted Gaussian mask parameter
! stepX           = 1.0           ! sampling step size along X
! stepY           = 1.0           ! sampling step size along Y
! maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
! Notify          = 'Off'
! scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
! masterfile      = 'undefined'   ! filename
! energymin       = 10.0
! energymax       = 20.0
! ipf_ht          = 100
! ipf_wd          = 100
! nthreads        = 1
! datafile        = 'undefined'
! ctffile         = 'undefined'
! angfile         = 'undefined'
! omega           = 0.0
! inputtype       = 'Binary'    ! Binary, EMEBSD, TSLHDF, TSLup2, OxfordHDF, OxfordBinary, BrukerHDF 
! HDFstrings      = ''

! if (present(initonly)) then
!   if (initonly) skipread = .TRUE.
! end if

! if (.not.skipread) then
! ! read the namelist file
!     open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
!     read(UNIT=dataunit,NML=EBSDIndexingdata)
!     close(UNIT=dataunit,STATUS='keep')

! ! check for required entries
!     if (trim(masterfile).eq.'undefined') then
!         call FatalError('EMEBSDIndexing:',' master pattern file name is undefined in '//nmlfile)
!     end if

!     if (trim(exptfile).eq.'undefined') then
!         call FatalError('EMEBSDIndexing:',' experimental file name is undefined in '//nmlfile)
!     end if

!     if (numsx.eq.0) then 
!         call FatalError('EMEBSDIndexing:',' pattern size numsx is zero in '//nmlfile)
!     end if

!     if (numsy.eq.0) then 
!         call FatalError('EMEBSDIndexing:',' pattern size numsy is zero in '//nmlfile)
!     end if
! end if

! ! if we get here, then all appears to be ok, and we need to fill in the enl fields

! enl%devid         = devid
! enl%multidevid    = multidevid
! enl%usenumd       = usenumd
! enl%platid        = platid
! enl%nregions      = nregions
! enl%maskpattern   = maskpattern
! enl%exptfile      = exptfile
! enl%ipf_ht        = ipf_ht
! enl%ipf_wd        = ipf_wd
! enl%nthreads      = nthreads
! enl%datafile      = datafile
! enl%ctffile       = ctffile
! enl%angfile       = angfile
! enl%maskradius    = maskradius
! enl%numdictsingle = numdictsingle
! enl%numexptsingle = numexptsingle
! enl%hipassw       = hipassw
! enl%masterfile    = masterfile
! enl%energyfile    = masterfile
! enl%stepX         = stepX
! enl%stepY         = stepY
! enl%Notify        = Notify
! enl%inputtype     = inputtype
! enl%HDFstrings    = HDFstrings
! enl%L             = L
! enl%numsx         = numsx
! enl%numsy         = numsy
! enl%ROI           = ROI
! enl%binning       = binning
! enl%thetac        = thetac
! enl%delta         = delta
! enl%xpc           = xpc
! enl%ypc           = ypc
! enl%gammavalue    = gammavalue
! enl%beamcurrent   = beamcurrent
! enl%dwelltime     = dwelltime
! enl%scalingmode   = scalingmode
! enl%omega         = omega
! enl%energymin     = energymin
! enl%energymax     = energymax

! end subroutine GetSphInxNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetADPNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill adpnl structure (used by EMgetADP.f90)
!
!> @param nmlfile namelist file name
!> @param adpnl name list structure
!
!> @date 02/17/18 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetADPNameList(nmlfile, adpnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetADPNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(ADPNameListType),INTENT(INOUT)               :: adpnl
!f2py intent(in,out) ::  adpnl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: ipf_ht
integer(kind=irg)       :: ipf_wd 
integer(kind=irg)       :: maskradius
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: nregions
integer(kind=irg)       :: ROI(4)
real(kind=dbl)          :: hipassw
character(1)            :: maskpattern
character(1)            :: filterpattern
character(1)            :: keeptmpfile
character(1)            :: usetmpfile
character(fnlen)        :: exptfile 
character(fnlen)        :: tmpfile
character(fnlen)        :: tiffname
character(fnlen)        :: maskfile
character(fnlen)        :: inputtype
character(fnlen)        :: HDFstrings(10)

! define the IO namelist to facilitate passing variables to the program.
namelist  / getADP / numsx, numsy, nregions, maskpattern, nthreads, ipf_ht, ipf_wd, exptfile, maskradius, inputtype, &
                     tmpfile, maskfile, HDFstrings, hipassw, tiffname, filterpattern, keeptmpfile, usetmpfile, ROI

! set the input parameters to default values
 ipf_ht = 100
 ipf_wd = 100
 maskfile = 'undefined'
 filterpattern = 'y'
 maskpattern = 'n'
 keeptmpfile = 'n'
 usetmpfile = 'n'
 maskradius = 240
 hipassw = 0.05
 nregions = 10
 numsx = 0
 numsy = 0
 ROI = (/ 0, 0, 0, 0 /)
 exptfile = 'undefined'
 inputtype = 'Binary'
 HDFstrings = ''
 tmpfile = 'EMEBSDDict_tmp.data'
 tiffname = 'undefined'
 nthreads = 1

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=getADP)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(exptfile).eq.'undefined') then
        call FatalError('GetADPNameList:',' experimental file name is undefined in '//nmlfile)
    end if

    if (trim(tiffname).eq.'undefined') then
        call FatalError('GetADPNameList:',' output tiff file name is undefined in '//nmlfile)
    end if

    if (numsx.eq.0) then
        call FatalError('GetADPNameList:',' patterns size numsx is zero in '//nmlfile)
    end if

    if (numsy.eq.0) then
        call FatalError('GetADPNameList:',' patterns size numsy is zero in '//nmlfile)
    end if
 end if

! if we get here, then all appears to be ok, and we need to fill in the enl fields
adpnl%ipf_ht = ipf_ht
adpnl%ipf_wd = ipf_wd
adpnl%maskradius = maskradius
adpnl%numsx = numsx
adpnl%numsy = numsy
adpnl%nthreads = nthreads
adpnl%nregions = nregions
adpnl%ROI = ROI
adpnl%hipassw = hipassw
adpnl%maskpattern = maskpattern
adpnl%filterpattern = filterpattern
adpnl%keeptmpfile = keeptmpfile
adpnl%usetmpfile = usetmpfile
adpnl%exptfile = exptfile
adpnl%tmpfile = tmpfile
adpnl%tiffname = tiffname
adpnl%maskfile = maskfile
adpnl%inputtype = inputtype
adpnl%HDFstrings = HDFstrings

end subroutine GetADPNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetOSMNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill osmnl structure (used by EMgetOSM.f90)
!
!> @param nmlfile namelist file name
!> @param osmnl name list structure
!
!> @date 08/17/19 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetOSMNameList(nmlfile, osmnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOSMNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(OSMNameListType),INTENT(INOUT)               :: osmnl
!f2py intent(in,out) ::  osmnl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: nmatch(5)
character(fnlen)        :: dotproductfile
character(fnlen)        :: tiffname


! define the IO namelist to facilitate passing variables to the program.
namelist  / getOSM / nmatch, dotproductfile, tiffname

! set the input parameters to default values
nmatch = (/ 20, 0, 0, 0, 0 /)
dotproductfile = 'undefined'
tiffname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=getOSM)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(dotproductfile).eq.'undefined') then
        call FatalError('GetOSMNameList:',' dot product file name is undefined in '//nmlfile)
    end if

    if (trim(tiffname).eq.'undefined') then
        call FatalError('GetOSMNameList:',' output tiff file name is undefined in '//nmlfile)
    end if
 end if

! if we get here, then all appears to be ok, and we need to fill in the enl fields
osmnl%nmatch = nmatch
osmnl%dotproductfile = dotproductfile
osmnl%tiffname = tiffname

end subroutine GetOSMNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetdpmergeNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill osmnl structure (used by EMgetOSM.f90)
!
!> @param nmlfile namelist file name
!> @param dpmnl name list structure
!
!> @date 08/17/19 MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetdpmergeNameList(nmlfile, dpmnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetdpmergeNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(dpmergeNameListType),INTENT(INOUT)           :: dpmnl
!f2py intent(in,out) ::  dpmnl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

character(fnlen)        :: dotproductfile(5)
character(fnlen)        :: ctfname
character(fnlen)        :: angname
character(fnlen)        :: phasemapname
integer(kind=irg)       :: phasecolors(5)
character(8)            :: usedp
character(2)            :: indexingmode

! define the IO namelist to facilitate passing variables to the program.
namelist  / dpmerge / dotproductfile, ctfname, angname, usedp, indexingmode, phasemapname, phasecolors

! set the input parameters to default values
dotproductfile = (/ 'undefined','undefined','undefined','undefined','undefined' /)
ctfname = 'undefined'
angname = 'undefined'
phasemapname = 'undefined'
phasecolors = (/ 1, 2, 0, 0, 0 /)
usedp = 'original'
indexingmode = 'DI'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=dpmerge)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if ((trim(dotproductfile(1)).eq.'undefined').or.(trim(dotproductfile(2)).eq.'undefined')) then
        call FatalError('GetdpmergeNameList:',' at least two dot product file names must be defined in '//nmlfile)
    end if

    if ((trim(ctfname).eq.'undefined').and.(trim(angname).eq.'undefined')) then
        call FatalError('GetdpmergeNameList:',' either ctfname or angname must be defined in '//nmlfile)
    end if
 end if

! if we get here, then all appears to be ok, and we need to fill in the dpmnl fields
dpmnl%dotproductfile = dotproductfile
dpmnl%ctfname = ctfname 
dpmnl%angname = angname 
dpmnl%phasemapname = phasemapname 
dpmnl%phasecolors = phasecolors
dpmnl%indexingmode = indexingmode
dpmnl%usedp = usedp

end subroutine GetdpmergeNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetTKDIndexingNameList
!
!> @author Marc De Graef , Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMTKDDI.f90)
!
!> @param nmlfile namelist file name
!> @param enl TKD indexing name list structure
!
!> @date 05/07/17 MDG 1.0 new routine, based on EBSD routine
!--------------------------------------------------------------------------
recursive subroutine GetTKDIndexingNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetTKDIndexingNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(TKDIndexingNameListType),INTENT(INOUT)       :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: numsx
integer(kind=irg)                                 :: numsy
integer(kind=irg)                                 :: binning
integer(kind=irg)                                 :: energyaverage
integer(kind=irg)                                 :: devid
integer(kind=irg)                                 :: platid
integer(kind=irg)                                 :: nregions
real(kind=sgl)                                    :: L
real(kind=sgl)                                    :: thetac
real(kind=sgl)                                    :: delta
real(kind=sgl)                                    :: xpc
real(kind=sgl)                                    :: ypc
real(kind=sgl)                                    :: gammavalue
real(kind=dbl)                                    :: beamcurrent
real(kind=dbl)                                    :: dwelltime
real(kind=dbl)                                    :: hipassw
real(kind=sgl)                                    :: omega
real(kind=sgl)                                    :: stepX
real(kind=sgl)                                    :: stepY
integer(kind=irg)                                 :: nthreads
character(1)                                      :: maskpattern
character(3)                                      :: scalingmode
character(3)                                      :: Notify
character(fnlen)                                  :: dotproductfile
character(fnlen)                                  :: masterfile
real(kind=sgl)                                    :: energymin
real(kind=sgl)                                    :: energymax
character(1)                                      :: spatialaverage
character(fnlen)                                  :: maskfile
character(fnlen)                                  :: tmpfile
character(fnlen)                                  :: datafile
character(fnlen)                                  :: ctffile
character(fnlen)                                  :: avctffile
character(fnlen)                                  :: angfile
character(fnlen)                                  :: eulerfile
character(fnlen)                                  :: inputtype
character(fnlen)                                  :: HDFstrings(10)
integer(kind=irg)                                 :: ncubochoric
integer(kind=irg)                                 :: numexptsingle
integer(kind=irg)                                 :: numdictsingle
integer(kind=irg)                                 :: ipf_ht
integer(kind=irg)                                 :: ipf_wd
integer(kind=irg)                                 :: nnk
integer(kind=irg)                                 :: ROI(4) 
integer(kind=irg)                                 :: nnav
integer(kind=irg)                                 :: nosm
integer(kind=irg)                                 :: maskradius
character(fnlen)                                  :: exptfile
character(fnlen)                                  :: dictfile
character(fnlen)                                  :: indexingmode

! define the IO namelist to facilitate passing variables to the program.
namelist  / TKDIndexingdata / thetac, delta, numsx, numsy, xpc, ypc, masterfile, devid, platid, &
beamcurrent, dwelltime, binning, gammavalue, energymin, spatialaverage, nregions, ROI, inputtype, HDFstrings, &
scalingmode, maskpattern, energyaverage, L, omega, nthreads, energymax, datafile, angfile, ctffile, &
ncubochoric, numexptsingle, numdictsingle, ipf_ht, ipf_wd, nnk, nnav, exptfile, maskradius, Notify, &
dictfile, indexingmode, hipassw, stepX, stepY, tmpfile, avctffile, nosm, eulerfile, maskfile

! set the input parameters to default values (except for xtalname, which must be present)
Notify          = 'Off'
ncubochoric     = 50
numexptsingle   = 1024
numdictsingle   = 1024
platid          = 1
devid           = 1
nregions        = 10
ROI             = (/ 0, 0, 0, 0 /)
nnk             = 50
nnav            = 20
nosm            = 20
exptfile        = 'undefined'
numsx           = 0           ! [dimensionless]
numsy           = 0           ! [dimensionless]
maskradius      = 240
binning         = 1             ! binning mode  (1, 2, 4, or 8)
L               = 20000.0       ! [microns]
energyaverage   = 1             ! apply energy averaging (1) or not (0); useful for dictionary computations
thetac          = 0.0           ! [degrees]
delta           = 25.0          ! [microns]
xpc             = 0.0           ! [pixels]
ypc             = 0.0           ! [pixels]
gammavalue      = 1.0           ! gamma factor
beamcurrent     = 14.513D0      ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
hipassw         = 0.05D0        ! hi pass inverted Gaussian mask parameter
stepX           = 1.0           ! sampling step size along X
stepY           = 1.0           ! sampling step size along Y
maskfile        = 'undefined'
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
masterfile      = 'undefined'   ! filename
dotproductfile  = 'undefined'
energymin       = 10.0
energymax       = 20.0
ipf_ht          = 100
ipf_wd          = 100
nthreads        = 1
spatialaverage  = 'n'
inputtype       = 'Binary'
HDFstrings      = (/ '', '', '', '', '', '', '', '', '', '' /)
datafile        = 'undefined'
ctffile         = 'undefined'
avctffile       = 'undefined'
angfile         = 'undefined'
eulerfile       = 'undefined'
omega           = 0.0
tmpfile         = 'EMEBSDDict_tmp.data'
dictfile        = 'undefined'
indexingmode    = 'dynamic'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=TKDIndexingdata)
    close(UNIT=dataunit,STATUS='keep')

    if (trim(indexingmode) .eq. 'static') then
        if (trim(dictfile) .eq. 'undefined') then
            call FatalError('EMTKDIndexing:',' dictionary file name is undefined in '//nmlfile)
        end if
    end if
        

! check for required entries
    if (trim(indexingmode) .eq. 'dynamic') then
        
        if (trim(masterfile).eq.'undefined') then
            call FatalError('EMTKDIndexing:',' master pattern file name is undefined in '//nmlfile)
        end if

    end if

    if (trim(exptfile).eq.'undefined') then
    end if

    if (numsx.eq.0) then
        call FatalError('EMTKDIndexing:',' pattern size numsx is zero in '//nmlfile)
    end if

    if (numsy.eq.0) then
        call FatalError('EMTKDIndexing:',' pattern size numsy is zero in '//nmlfile)
    end if
end if



! if we get here, then all appears to be ok, and we need to fill in the enl fields

enl%Notify = Notify
enl%devid = devid
enl%platid = platid
enl%nregions = nregions
enl%maskpattern = maskpattern
enl%exptfile = exptfile
enl%ROI = ROI
enl%nnk = nnk
enl%nnav = nnav
enl%nosm = nosm
enl%ipf_ht = ipf_ht
enl%ipf_wd = ipf_wd
enl%nthreads = nthreads
enl%datafile = datafile
enl%tmpfile = tmpfile
enl%maskfile = maskfile
enl%ctffile = ctffile
enl%avctffile = avctffile
enl%angfile = angfile
enl%eulerfile = eulerfile
enl%maskradius = maskradius
enl%numdictsingle = numdictsingle
enl%numexptsingle = numexptsingle
enl%hipassw = hipassw
enl%masterfile = masterfile
enl%energyfile = enl%masterfile
enl%StepX = stepX
enl%StepY = stepY
enl%indexingmode = trim(indexingmode)
enl%inputtype = inputtype
enl%hDFstrings = hDFstrings

if (trim(indexingmode) .eq. 'dynamic') then
    enl%L = L
    enl%numsx = numsx
    enl%numsy = numsy
    enl%binning = binning
    enl%energyaverage = energyaverage
    enl%thetac = thetac
    enl%delta = delta
    enl%xpc = xpc
    enl%ypc = ypc
    enl%gammavalue = gammavalue
    enl%beamcurrent = beamcurrent
    enl%dwelltime = dwelltime
    enl%scalingmode = scalingmode
    enl%ncubochoric = ncubochoric
    enl%omega = omega
    enl%energymin = energymin
    enl%energymax = energymax
    enl%spatialaverage = spatialaverage
    enl%dictfile = 'undefined'
else if (trim(indexingmode) .eq. 'static') then
    enl%dictfile = dictfile
    enl%ncubochoric = 0
else
    call FatalError('EMTKDIndexing:',' indexingmode is not known in '//nmlfile)
end if

end subroutine GetTKDIndexingNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetZAdefectNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill ZAdefect structure (used by CTEMDefect.f90)
!
!> @param nmlfile namelist file name
!> @param ZAdefect Zone Axis defect simulation name list structure
!
!> @date 06/24/15  SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetZAdefectNameList(nmlfile, ZAdefect, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetZAdefectNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(ZAdefectnameListType),INTENT(INOUT)      :: ZAdefect
!f2py intent(in,out) ::  ZAdefect
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.


character(fnlen)    :: xtalname
real(kind=sgl)      :: voltage 
integer(kind=irg)   :: kk(3) 
real(kind=sgl)      :: lauec(2) 
real(kind=sgl)      :: dmin 

! EM or STEM ?
character(fnlen)    :: progmode
character(fnlen)    :: STEMnmlfile 
character(fnlen)    :: foilnmlfile 

! column approximation parameters and image parameters 
real(kind=sgl)      :: DF_L 
real(kind=sgl)      :: DF_npix 
real(kind=sgl)      :: DF_npiy 
real(kind=sgl)      :: DF_slice 

integer(kind=irg)   :: dinfo
character(fnlen)    :: sgname 

! defect parameters
integer(kind=irg)   :: numdisl
integer(kind=irg)   :: numsf
integer(kind=irg)   :: numinc
integer(kind=irg)   :: numvoids
character(fnlen)    :: voidname
character(fnlen)    :: dislname
character(fnlen)    :: sfname
character(fnlen)    :: incname
character(fnlen)    :: dispfile
character(fnlen)    :: dispmode

! output parameters
character(fnlen)    :: dataname
integer(kind=irg)   :: t_interval

! define the IO namelist to facilitate passing variables to the program.
namelist  / rundata / xtalname, voltage, kk, lauec, dmin, progmode, STEMnmlfile, foilnmlfile,&
                      DF_L, DF_npix, DF_npiy, DF_slice, dinfo, sgname, numdisl, numsf, numinc,&
                      numvoids, voidname, dislname, sfname, incname, dispfile, dispmode, dataname, t_interval 

! set the input parameters to default values (except for xtalname, which must be present)
xtalname = 'undefined'
voltage = 200000.0
kk = (/0.0,0.0,1.0/)
lauec = (/0.0,0.0/)
dmin = 0.04
progmode = 'CTEM'
STEMnmlfile = 'STEM_rundata.nml'
foilnmlfile = 'FOIL_rundata.nml'
DF_L = 1.0
DF_npix = 256
DF_npiy = 256
DF_slice = 1.0
dinfo = 0
sgname = 'undefined'
numdisl = 0
numsf = 0
numinc = 0
numvoids = 0
voidname = 'void.nml'
dislname = 'dislocation.nml'
sfname = 'stackingfault.nml'
incname = 'inclusion.nml'
dispfile = 'undefined'
dispmode = 'undefined'
dataname = 'undefined'
t_interval = 10

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=rundata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
call FatalError('CTEMDefect:',' xtal file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
ZAdefect%xtalname = xtalname
ZAdefect%voltage = voltage
ZAdefect%kk = kk
ZAdefect%lauec = lauec
ZAdefect%dmin = dmin
ZAdefect%progmode = progmode
ZAdefect%STEMnmlfile = STEMnmlfile
ZAdefect%foilnmlfile = foilnmlfile
ZAdefect%DF_L = DF_L
ZAdefect%DF_npix = DF_npix
ZAdefect%DF_npiy = DF_npiy
ZAdefect%DF_slice = DF_slice
ZAdefect%dinfo = dinfo
ZAdefect%sgname = sgname
ZAdefect%numdisl = numdisl
ZAdefect%numsf = numsf
ZAdefect%numinc = numinc
ZAdefect%numvoids = numvoids
ZAdefect%voidname = voidname
ZAdefect%dislname = dislname
ZAdefect%sfname = sfname
ZAdefect%incname = incname
ZAdefect%dispfile = dispfile
ZAdefect%dispmode = dispmode
ZAdefect%dataname = dataname
ZAdefect%t_interval = t_interval

end subroutine GetZADefectNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPIndexingNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMDynamicECPIndeixing.f90)
!
!> @param nmlfile namelist file name
!> @param enl ECP indexing name list structure
!
!> @date 11/19/15  SS 1.0 original
!> @date 01/26/16  SS 1.1 modified for EMsoft3.1 indexing code
!--------------------------------------------------------------------------
recursive subroutine GetECPIndexingNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPIndexingNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(ECPIndexingNameListType),INTENT(INOUT)       :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: ncubochoric
integer(kind=irg)                                 :: numexptsingle
integer(kind=irg)                                 :: numdictsingle
integer(kind=irg)                                 :: totnumexpt
integer(kind=irg)                                 :: maskradius
integer(kind=irg)                                 :: nnk
integer(kind=irg)                                 :: platid
integer(kind=irg)                                 :: devid
integer(kind=irg)                                 :: nregions
character(fnlen)                                  :: exptfile 
integer(kind=irg)                                 :: stdout
integer(kind=irg)                                 :: nthreads
integer(kind=irg)                                 :: npix
real(kind=sgl)                                    :: thetac
character(1)                                      :: maskpattern
character(fnlen)                                  :: masterfile
character(fnlen)                                  :: tmpfile
character(fnlen)                                  :: datafile
character(fnlen)                                  :: ctffile
real(kind=sgl)                                    :: gammavalue
real(kind=dbl)                                    :: sampletilt
real(kind=sgl)                                    :: workingdistance
real(kind=sgl)                                    :: Rin
real(kind=sgl)                                    :: Rout

namelist / ECPIndexingdata / ncubochoric, numexptsingle, numdictsingle, totnumexpt, nnk, exptfile, &
stdout, nthreads, npix, thetac, maskpattern, masterfile, datafile, gammavalue, platid, &
sampletilt, workingdistance, Rin, Rout, maskradius, devid, nregions, ctffile, tmpfile

ncubochoric = 100 
numexptsingle = 1024
numdictsingle = 1024 
totnumexpt = 10
nregions = 10
nnk = 2 
platid = 1
devid = 1
exptfile = 'undefined'
nthreads = 1 
npix = 256 
thetac = 10.0 
maskpattern ='y' 
masterfile = 'undefined' 
datafile = 'undefined' 
tmpfile = 'EMECPDict_tmp.data'
ctffile = 'undefined'
gammavalue = 1.0 
sampletilt = 0.0 
workingdistance = 10.0 
Rin = 3.0 
Rout = 5.0
maskradius = 128

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=ECPIndexingdata)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(masterfile).eq.'undefined') then
        call FatalError('EMECPIndexing:',' master pattern file name is undefined in '//nmlfile)
    end if

    if (trim(exptfile).eq.'undefined') then
        call FatalError('EMECPIndexing:',' experimental file name is undefined in '//nmlfile)
    end if

end if

enl%ncubochoric = ncubochoric
enl%numexptsingle = numexptsingle
enl%numdictsingle = numdictsingle
enl%totnumexpt = totnumexpt
enl%nnk = nnk
enl%devid = devid
enl%platid = platid
enl%nregions = nregions
enl%exptfile = exptfile
enl%nthreads = nthreads
enl%npix = npix
enl%thetac = thetac
enl%maskpattern = maskpattern 
enl%masterfile = masterfile 
enl%energyfile = enl%masterfile
enl%datafile = datafile
enl%gammavalue = gammavalue
enl%sampletilt = sampletilt
enl%workingdistance = workingdistance
enl%Rin = Rin
enl%Rout = Rout
enl%maskradius = maskradius 
enl%ctffile = ctffile
enl%tmpfile = tmpfile

end subroutine GetECPIndexingNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMDPFit4NameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMDPFit.f90)
!
!> @param nmlfile namelist file name
!> @param enl DPFit name list structure
!
!> @date 02/22/16  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMDPFit4NameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMDPFit4NameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMDPFit4ListType),INTENT(INOUT)              :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

character(fnlen)                                  :: masterfile
character(fnlen)                                  :: modalityname
character(fnlen)                                  :: exptfile_pat1
character(fnlen)                                  :: exptfile_pat2
character(fnlen)                                  :: exptfile_pat3
character(fnlen)                                  :: exptfile_pat4
real(kind=dbl)                                    :: rhobeg, rhoend
logical                                           :: verbose, mask
real(kind=sgl)                                    :: gammavalue
real(kind=sgl)                                    :: phi_pat1, phi1_pat1, phi2_pat1
real(kind=sgl)                                    :: phi_pat2, phi1_pat2, phi2_pat2
real(kind=sgl)                                    :: phi_pat3, phi1_pat3, phi2_pat3
real(kind=sgl)                                    :: phi_pat4, phi1_pat4, phi2_pat4
real(kind=sgl)                                    :: L
real(kind=sgl)                                    :: thetac
real(kind=sgl)                                    :: delta
real(kind=sgl)                                    :: omega
integer(kind=irg)                                 :: numsx
integer(kind=irg)                                 :: numsy
integer(kind=irg)                                 :: binning
real(kind=sgl)                                    :: xpc
real(kind=sgl)                                    :: ypc
real(kind=sgl)                                    :: beamcurrent
real(kind=sgl)                                    :: dwelltime
integer(kind=irg)                                 :: npix
real(kind=sgl)                                    :: Rin
real(kind=sgl)                                    :: Rout
real(kind=sgl)                                    :: thetacone
real(kind=sgl)                                    :: sampletilt
real(kind=sgl)                                    :: workingdistance
real(kind=sgl)                                    :: maskradius
real(kind=sgl)                                    :: step_xpc
real(kind=sgl)                                    :: step_ypc
real(kind=sgl)                                    :: step_L
real(kind=sgl)                                    :: step_phi1
real(kind=sgl)                                    :: step_phi
real(kind=sgl)                                    :: step_phi2
real(kind=sgl)                                    :: step_thetacone
integer(kind=irg)                                 :: nrun
integer(kind=irg)                                 :: pixx_pat1,pixx_pat2,pixx_pat3,pixx_pat4
integer(kind=irg)                                 :: pixy_pat1,pixy_pat2,pixy_pat3,pixy_pat4
real(kind=sgl)                                    :: stepx
real(kind=sgl)                                    :: stepy



namelist / DPFitdata / masterfile, modalityname, exptfile_pat1, rhobeg, rhoend, verbose, mask, &
         phi1_pat1, phi_pat1, phi2_pat1, L, thetac, delta, omega, numsx, numsy, binning, xpc, ypc, beamcurrent, &
         dwelltime, npix, Rin, Rout, thetacone, sampletilt, workingdistance, gammavalue, maskradius, &
         phi1_pat2, phi_pat2, phi2_pat2, phi1_pat3, phi_pat3, phi2_pat3, phi1_pat4, phi_pat4, phi2_pat4, &
         exptfile_pat2, exptfile_pat3, exptfile_pat4, step_xpc, step_ypc, step_L, step_phi1, step_phi, step_phi2,&
         step_thetacone, nrun, pixx_pat1, pixy_pat1, stepx, stepy,  pixx_pat2, pixy_pat2,  pixx_pat3, pixy_pat3,&
         pixx_pat4, pixy_pat4

masterfile = 'undefined' 
modalityname = 'undefined' 

exptfile_pat1 = 'undefined'
exptfile_pat2 = 'undefined'
exptfile_pat3 = 'undefined'
exptfile_pat4 = 'undefined'

rhobeg = 1.0D-2
rhoend = 1.0D-7
verbose = .TRUE.
mask = .TRUE.
phi1_pat1 = 0.0
phi_pat1 = 0.0
phi2_pat1 = 0.0

phi1_pat2 = 0.0
phi_pat2 = 0.0
phi2_pat2 = 0.0

phi1_pat3 = 0.0
phi_pat3 = 0.0
phi2_pat3 = 0.0

phi1_pat4 = 0.0
phi_pat4 = 0.0
phi2_pat4 = 0.0

step_phi1 = 2.0
step_phi = 2.0
step_phi2 = 2.0

L = 15000.0
thetac = 10.0
delta = 50.0
omega = 0.0
numsx = 0
numsy = 0
binning = 1
xpc = 0.0
ypc = 0.0
beamcurrent = 1000.0
dwelltime = 1000.0
npix = 512
Rin = 2.0
Rout = 5.0
thetacone = 5.0
sampletilt = 0.0
workingdistance = 7.0
gammavalue = 1.0
maskradius = 256.0

step_xpc = 5.0
step_ypc = 5.0
step_L = 5.0
step_thetacone = 1.0

pixx_pat1 = 0
pixy_pat1 = 0
pixx_pat2 = 0
pixy_pat2 = 0
pixx_pat3 = 0
pixy_pat3 = 0
pixx_pat4 = 0
pixy_pat4 = 0

stepx = 5.0
stepy = 5.0

nrun = 2

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=DPFitdata)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(masterfile).eq.'undefined') then
        call FatalError('EMDPFit:',' master pattern file name is undefined in '//nmlfile)
    end if

    if (trim(modalityname).eq.'undefined') then
        call FatalError('EMDPFit:',' modality name is undefined in '//nmlfile)
    end if

    if (trim(exptfile_pat1).eq.'undefined') then
        call FatalError('EMDPFit:',' experimental file name is undefined in '//nmlfile)
    end if

    if (numsx.eq.0) then
        call FatalError('EMDPFit:',' pattern size numsx is zero in '//nmlfile)
    end if

    if (numsy.eq.0) then
        call FatalError('EMDPFit:',' pattern size numsy is zero in '//nmlfile)
    end if
end if

enl%masterfile = masterfile 
enl%modalityname = modalityname
enl%exptfile_pat1 = exptfile_pat1
enl%exptfile_pat2 = exptfile_pat2
enl%exptfile_pat3 = exptfile_pat3
enl%exptfile_pat4 = exptfile_pat4
enl%rhobeg = rhobeg
enl%rhoend = rhoend
enl%verbose = verbose
enl%mask = mask
enl%phi1_pat1 = phi1_pat1
enl%phi_pat1 = phi_pat1
enl%phi2_pat1 = phi2_pat1

enl%phi1_pat2 = phi1_pat2
enl%phi_pat2 = phi_pat2
enl%phi2_pat2 = phi2_pat2

enl%phi1_pat3 = phi1_pat3
enl%phi_pat3 = phi_pat3
enl%phi2_pat3 = phi2_pat3

enl%phi1_pat4 = phi1_pat4
enl%phi_pat4 = phi_pat4
enl%phi2_pat4 = phi2_pat4

enl%L = L
enl%thetac = thetac
enl%omega = omega
enl%numsx = numsx
enl%numsy = numsy
enl%binning = binning
enl%xpc = xpc
enl%ypc = ypc
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%npix = npix
enl%Rin = Rin
enl%Rout = Rout
enl%thetacone = thetacone
enl%sampletilt = sampletilt
enl%workingdistance = workingdistance
enl%gammavalue = gammavalue
enl%maskradius = maskradius
enl%delta = delta

enl%step_xpc = step_xpc
enl%step_ypc = step_ypc
enl%step_L = step_L
enl%step_phi1 = step_phi1
enl%step_phi = step_phi
enl%step_phi2 = step_phi2
enl%step_thetacone = step_thetacone

enl%nrun = nrun

enl%stepx = stepx
enl%stepy = stepy

enl%pixx_pat1 = pixx_pat1
enl%pixy_pat1 = pixy_pat1

enl%pixx_pat2 = pixx_pat2
enl%pixy_pat2 = pixy_pat2

enl%pixx_pat3 = pixx_pat3
enl%pixy_pat3 = pixy_pat3

enl%pixx_pat4 = pixx_pat4
enl%pixy_pat4 = pixy_pat4

end subroutine GetEMDPFit4NameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMDPFitNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMDPFit.f90)
!
!> @param nmlfile namelist file name
!> @param enl DPFit name list structure
!
!> @date 02/22/16  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMDPFitNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMDPFitNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMDPFitListType),INTENT(INOUT)               :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

character(fnlen)                                  :: masterfile
character(fnlen)                                  :: modalityname
character(fnlen)                                  :: exptfile
real(kind=dbl)                                    :: rhobeg, rhoend
logical                                           :: verbose, mask
real(kind=sgl)                                    :: gammavalue
real(kind=sgl)                                    :: phi, phi1, phi2
real(kind=sgl)                                    :: L
real(kind=sgl)                                    :: thetac
real(kind=sgl)                                    :: delta
integer(kind=irg)                                 :: numsx
integer(kind=irg)                                 :: numsy
integer(kind=irg)                                 :: binning
real(kind=sgl)                                    :: xpc
real(kind=sgl)                                    :: ypc
real(kind=sgl)                                    :: beamcurrent
real(kind=sgl)                                    :: dwelltime
integer(kind=irg)                                 :: npix
real(kind=sgl)                                    :: Rin
real(kind=sgl)                                    :: Rout
real(kind=sgl)                                    :: thetacone
real(kind=sgl)                                    :: sampletilt
real(kind=sgl)                                    :: workingdistance
real(kind=sgl)                                    :: maskradius
real(kind=sgl)                                    :: step_xpc
real(kind=sgl)                                    :: step_ypc
real(kind=sgl)                                    :: step_L
real(kind=sgl)                                    :: step_phi1
real(kind=sgl)                                    :: step_phi
real(kind=sgl)                                    :: step_phi2
real(kind=sgl)                                    :: step_thetacone
integer(kind=irg)                                 :: nrun
integer(kind=irg)                                 :: nregions
character(2)                                      :: metric



namelist / DPFitdata / modalityname, masterfile, metric, exptfile, nrun, rhobeg, rhoend, verbose, mask, &
                      maskradius, gammavalue, nregions, phi1, phi, phi2, step_phi1, step_phi, step_phi2, &
                      thetac, delta, numsx, numsy, beamcurrent, dwelltime, binning, L, xpc, ypc, step_L, &
                      step_xpc, step_ypc, npix, Rin, Rout, thetacone, sampletilt, workingdistance, step_thetacone

masterfile    = 'undefined1' 
modalityname  = 'undefined1' 

exptfile      = 'undefined1'

rhobeg        = 1.0D-2
rhoend        = 1.0D-7
verbose       = .TRUE.
mask          = .TRUE.
phi1          = 0.0
phi           = 0.0
phi2          = 0.0

step_phi1     = 2.0
step_phi      = 2.0
step_phi2     = 2.0

L             = 15000.0
thetac        = 10.0
delta         = 50.0
numsx         = 0
numsy         = 0
binning       = 1
xpc           = 0.0
ypc           = 0.0
beamcurrent   = 1000.0
dwelltime     = 1000.0
npix          = 512
Rin           = 2.0
Rout          = 5.0
thetacone     = 5.0
sampletilt    = 0.0
workingdistance = 7.0
gammavalue    = 1.0
maskradius    = 256.0

step_xpc      = 5.0
step_ypc      = 5.0
step_L        = 5.0
step_thetacone = 1.0

nrun          = 2
nregions      = 8
metric        = 'DP'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=DPFitdata)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(masterfile).eq.'undefined') then
        call FatalError('EMDPFit:',' master pattern file name is undefined in '//nmlfile)
    end if

    if (trim(modalityname).eq.'undefined') then
        call FatalError('EMDPFit:',' modality name is undefined in '//nmlfile)
    end if

    if (trim(exptfile).eq.'undefined') then
        call FatalError('EMDPFit:',' experimental file name is undefined in '//nmlfile)
    end if

    if (numsx.eq.0) then 
        call FatalError('EMDPFit:',' pattern size numsx is zero in '//nmlfile)
    end if

    if (numsy.eq.0) then 
        call FatalError('EMDPFit:',' pattern size numsy is zero in '//nmlfile)
    end if
end if

enl%masterfile = masterfile 
enl%modalityname = modalityname
enl%metric = metric
enl%nregions = nregions
enl%exptfile = exptfile
enl%rhobeg = rhobeg
enl%rhoend = rhoend
enl%verbose = verbose
enl%mask = mask
enl%phi1 = phi1
enl%phi = phi
enl%phi2 = phi2

enl%L = L
enl%thetac = thetac
enl%numsx = numsx
enl%numsy = numsy
enl%binning = binning
enl%xpc = xpc
enl%ypc = ypc
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%npix = npix
enl%Rin = Rin
enl%Rout = Rout
enl%thetacone = thetacone
enl%sampletilt = sampletilt
enl%workingdistance = workingdistance
enl%gammavalue = gammavalue
enl%maskradius = maskradius
enl%delta = delta

enl%step_xpc = step_xpc
enl%step_ypc = step_ypc
enl%step_L = step_L
enl%step_phi1 = step_phi1
enl%step_phi = step_phi
enl%step_phi2 = step_phi2
enl%step_thetacone = step_thetacone
enl%nrun = nrun

end subroutine GetEMDPFitNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetECPSingleNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMDPFit.f90)
!
!> @param nmlfile namelist file name
!> @param enl ECP single name list structure
!
!> @date 02/22/16  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetECPSingleNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPSingleNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(ECPSingleNameListType),INTENT(INOUT)         :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
integer(kind=irg)       :: npix
real(kind=sgl)          :: thetac
character(1)            :: maskpattern
character(fnlen)        :: energyfile
character(fnlen)        :: datafile
character(3)            :: eulerconvention
real(kind=sgl)          :: gammavalue
real(kind=dbl)          :: sampletilt
real(kind=sgl)          :: workingdistance
real(kind=sgl)          :: Rin
real(kind=sgl)          :: Rout
real(kind=dbl)          :: phi1, phi, phi2
real(kind=sgl)          :: dmin

namelist / ECPSinglelist / nthreads, npix, thetac, maskpattern, energyfile, datafile, eulerconvention, &
gammavalue, sampletilt, workingdistance, Rin, Rout, phi1, phi, phi2, dmin


nthreads = 1
npix = 512
thetac = 5.0
maskpattern = 'n'
energyfile = 'undefined'
datafile = 'undefined'
eulerconvention = 'tsl'
gammavalue = 1.0
sampletilt = 0.D0
workingdistance = 7.0
Rin = 2.0
Rout = 5.0
phi1 = 0.D0
phi = 0.D0
phi2 = 0.D0
dmin = 0.03


if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=ECPSinglelist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(energyfile).eq.'undefined') then
        call FatalError('EMDPFit:',' energy file name is undefined in '//nmlfile)
    end if

end if

enl%nthreads = nthreads
enl%npix = npix
enl%thetac = thetac
enl%maskpattern = maskpattern
enl%energyfile = energyfile
enl%datafile = datafile
enl%eulerconvention = eulerconvention
enl%gammavalue = gammavalue
enl%sampletilt = sampletilt
enl%workingdistance = workingdistance
enl%Rin = Rin
enl%Rout = Rout
enl%phi1 = phi1
enl%phi = phi
enl%phi2 = phi2
enl%dmin = dmin

end subroutine GetECPSingleNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMSTEMDCINameList
!
!> @author Patrick Callahan
!
!> @brief read namelist file for EMSTEMDCI
!
!> @param nmlfile namelist file name
!
!> @date 11/01/16  PGC 1.0 new routine
!--------------------------------------------------------------------------

recursive SUBROUTINE GetSTEMDCINameList(nmlfile, dcinl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSTEMDCINameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)              :: nmlfile
type(STEMDCINameListType), INTENT(INOUT) :: dcinl
!f2py intent(in,out) ::  dcinl
logical, OPTIONAL, INTENT(IN)            :: initonly

logical                                  :: skipread = .FALSE.

integer(kind=irg)     :: nthreads
real(kind=sgl)        :: voltage
character(4)          :: progmode
character(fnlen)      :: xtalname
integer(kind=irg)     :: kk(3)
real(kind=sgl)        :: lauec(2)
character(fnlen)      :: STEMnmlfile
character(fnlen)      :: dataname
character(fnlen)      :: defectfilename
character(3)          :: dispmode
character(fnlen)      :: dispfile
integer(kind=irg)     :: output
integer(kind=irg)     :: dinfo
integer(kind=irg)     :: t_interval
real(kind=sgl)        :: DF_L
integer(kind=irg)     :: DF_npix
integer(kind=irg)     :: DF_npiy
real(kind=sgl)        :: DF_slice
real(kind=sgl)        :: dmin

! DEFINE THE IO NAMELIST FOR VARIABLE PASSING
namelist / STEMDCIdata / nthreads, voltage, progmode, xtalname, kk, lauec, STEMnmlfile, &
                         dataname, defectfilename, dispmode, dispfile, output, dinfo, t_interval, DF_L, &
                         DF_npix, DF_npiy, DF_slice, dmin

! SET INPUT PARAMETERS TO DEFAULT VALUES (EXCEPT XTALNAME, WHICH MUST BE PRESENT)
nthreads = 6
voltage = 200000
progmode = 'STEM'
xtalname = 'undefined'
kk = (/ 0.0, 0.0, 1.0 /)
lauec = (/ 0.0, 0.0 /)
STEMnmlfile = 'STEM_rundata.nml'
defectfilename = 'EMdefec/dispt.json'
dataname = '/folder/to/trialbinary.data'
dispmode = 'not'
dispfile = 'test_ZA.data'
output = 6 ! screen output
dinfo = 0 ! 1 is verbose
t_interval = 5 ! update every x steps
DF_L = 1.0
DF_npix = 256
DF_npiy = 256
DF_slice = 1.0 ! slice thickness for scattering matrix approach (nmu)
dmin = 0.03

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=STEMDCIdata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
  call FatalError('STEMDCI:',' crystal structure file name is undefined in '//nmlfile)
end if

end if

dcinl%nthreads = nthreads
dcinl%voltage = voltage
dcinl%progmode = progmode
dcinl%xtalname = xtalname
dcinl%kk = kk
dcinl%lauec = lauec
dcinl%STEMnmlfile = STEMnmlfile
dcinl%dataname = dataname
dcinl%defectfilename = defectfilename
dcinl%dispmode = dispmode
dcinl%dispfile = dispfile
dcinl%output = output
dcinl%dinfo = dinfo
dcinl%t_interval = t_interval
dcinl%DF_L = DF_L
dcinl%DF_npix = DF_npix
dcinl%DF_npiy = DF_npiy
dcinl%DF_slice = DF_slice
dcinl%dmin = dmin


END SUBROUTINE GetSTEMDCINameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetSTEMGeometryNameList
!
!> @author Marc De Graef
!
!> @brief read namelist file for STEM geometry
!
!> @param nmlfile namelist file name
!
!> @date 07/02/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive SUBROUTINE GetSTEMGeometryNameList(nmlfile, dcinl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSTEMGeometryNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)              :: nmlfile
type(STEMGeometryNameListType), INTENT(INOUT) :: dcinl
!f2py intent(in,out) ::  dcinl
logical, OPTIONAL, INTENT(IN)            :: initonly

logical                                  :: skipread = .FALSE.

integer(kind=irg)     :: numberofsvalues 
integer(kind=irg)     :: numCL
real(kind=sgl)        :: BFradius
real(kind=sgl)        :: ADFinnerradius
real(kind=sgl)        :: ADFouterradius
real(kind=sgl)        :: kt
real(kind=sgl)        :: beamconvergence
real(kind=sgl)        :: diffaprad
real(kind=sgl)        :: diffapcenter
real(kind=sgl)        :: CLarray(20)
character(2)          :: geometry

! DEFINE THE IO NAMELIST FOR VARIABLE PASSING
namelist / STEMGeometrydata / numberofsvalues, numCL, BFradius, ADFinnerradius, ADFouterradius, kt, &
                              beamconvergence, CLarray, geometry, diffaprad, diffapcenter

! SET INPUT PARAMETERS TO DEFAULT VALUES (EXCEPT XTALNAME, WHICH MUST BE PRESENT)
numberofsvalues = 21
numCL = 2
BFradius = 3.5
ADFinnerradius = 3.5
ADFouterradius = 10.0
kt = 1.0
beamconvergence = 8.2
diffaprad = 0.0
diffapcenter = 0.0
CLarray(20) = 100.0
CLarray(1) = 150.0
CLarray(2) = 250.0
geometry = 'ZA'

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=STEMGeometrydata)
close(UNIT=dataunit,STATUS='keep')

end if

! make sure numberofsvalues is an odd number...
if (mod(numberofsvalues,2).eq.0) numberofsvalues=numberofsvalues+1
dcinl%numberofsvalues = numberofsvalues 
dcinl%numCL = numCL
dcinl%BFradius = BFradius 
dcinl%ADFinnerradius = ADFinnerradius
dcinl%ADFouterradius = ADFouterradius
dcinl%kt = kt 
dcinl%beamconvergence = beamconvergence 
dcinl%diffaprad = diffaprad
dcinl%diffapcenter = diffapcenter
dcinl%CLarray = CLarray 
dcinl%geometry = geometry 

END SUBROUTINE GetSTEMGeometryNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetRefineOrientationNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMFitOrientation.f90)
!
!> @param nmlfile namelist file name
!> @param enl single name list structure
!
!> @date 02/22/16  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetRefineOrientationNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetRefineOrientationNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(RefineOrientationtype),INTENT(INOUT)         :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: nthreads
integer(kind=irg)                                 :: matchdepth
character(fnlen)                                  :: dotproductfile
character(fnlen)                                  :: ctffile
character(fnlen)                                  :: tmpfile
character(fnlen)                                  :: PSvariantfile
character(fnlen)                                  :: method
character(4)                                      :: modality
logical                                           :: inRAM
integer(kind=irg)                                 :: nmis
integer(kind=irg)                                 :: niter
real(kind=sgl)                                    :: step
integer(kind=irg)                                 :: initialx
integer(kind=irg)                                 :: initialy
character(fnlen)                                  :: PCcorrection
real(kind=sgl)                                    :: truedelta


namelist / RefineOrientations / nthreads, dotproductfile, ctffile, modality, nmis, niter, step, inRAM, method, &
                                matchdepth, PSvariantfile, tmpfile, initialx, initialy, PCcorrection, truedelta

nthreads = 1
matchdepth = 1
dotproductfile = 'undefined'
ctffile = 'undefined'
tmpfile = 'undefined'
PSvariantfile = 'undefined'
method = 'FIT'
inRAM = .FALSE.
nmis = 1
niter = 1
step = 1.0
modality = 'EBSD'
initialx = 0
initialy = 0
PCcorrection = 'off'
truedelta = 50.0


if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=RefineOrientations)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(dotproductfile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' dotproduct file name is undefined in '//nmlfile)
    end if

    if (trim(ctffile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' ctf file name is undefined in '//nmlfile)
    end if

    if (trim(tmpfile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' tmp file name is undefined in '//nmlfile)
    end if
end if

enl%nthreads = nthreads
enl%matchdepth = matchdepth
enl%dotproductfile = dotproductfile
enl%ctffile = ctffile
enl%tmpfile = tmpfile
enl%PSvariantfile = PSvariantfile
enl%method = method
enl%inRAM = inRAM
enl%nmis = nmis
enl%niter = niter
enl%step = step
enl%modality = modality
enl%initialx = initialx 
enl%initialy = initialy
enl%PCcorrection = PCcorrection
enl%truedelta = truedelta 

end subroutine GetRefineOrientationNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetFitalphavariantsNameList
!
!> @author Marc De Graef , Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMFitalphavariants.f90)
!
!> @param nmlfile namelist file name
!> @param enl single name list structure
!
!> @date 03/05/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetFitalphavariantsNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetFitalphavariantsNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(Fitalphavarianttype),INTENT(INOUT)           :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
character(fnlen)        :: betadotproductfile
character(fnlen)        :: alphadotproductfile
character(fnlen)        :: outputfile
character(fnlen)        :: variantquaternionfile
real(kind=sgl)          :: step


namelist / Fitalphavariants / nthreads, betadotproductfile, alphadotproductfile, outputfile, &
                              variantquaternionfile, step

nthreads = 1
betadotproductfile = 'undefined'
alphadotproductfile = 'undefined'
outputfile = 'undefined'
variantquaternionfile = 'undefined'
step = 1.0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=Fitalphavariants)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(betadotproductfile).eq.'undefined') then
        call FatalError('GetFitalphavariantsNameList:',' beta phase dotproduct file name is undefined in '//nmlfile)
    end if

    if (trim(alphadotproductfile).eq.'undefined') then
        call FatalError('GetFitalphavariantsNameList:',' alphaa phase dotproduct file name is undefined in '//nmlfile)
    end if

    if (trim(variantquaternionfile).eq.'undefined') then
        call FatalError('GetFitalphavariantsNameList:',' variantquaternion file name is undefined in '//nmlfile)
    end if

    if (trim(outputfile).eq.'undefined') then
        call FatalError('GetFitalphavariantsNameList:',' output file name is undefined in '//nmlfile)
    end if


end if

enl%nthreads = nthreads
enl%betadotproductfile = betadotproductfile
enl%alphadotproductfile = alphadotproductfile
enl%outputfile = outputfile
enl%variantquaternionfile = variantquaternionfile
enl%step = step

end subroutine GetFitalphavariantsNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetFitOrientationPSNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMFitOrientationPS.f90)
!
!> @param nmlfile namelist file name
!> @param enl single name list structure
!
!> @date 02/22/16  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetFitOrientationPSNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetFitOrientationPSNameList

use error
use constants

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(FitOrientationPStype),INTENT(INOUT)          :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: nthreads
integer(kind=irg)                                 :: matchdepth
character(fnlen)                                  :: dotproductfile
character(fnlen)                                  :: ctffile
real(kind=sgl)                                    :: step
character(fnlen)                                  :: PSvariantfile
character(fnlen)                                  :: modality

namelist / FitOrientationPS / nthreads, dotproductfile, ctffile, modality, step, PSvariantfile, matchdepth

nthreads = 1
matchdepth = 1
dotproductfile = 'undefined'
ctffile = 'undefined'
step = 1.0
PSvariantfile = 'undefined'
modality = 'EBSD'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=FitOrientationPS)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(dotproductfile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' dotproduct file name is undefined in '//nmlfile)
    end if

    if (trim(dotproductfile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' ctf file name is undefined in '//nmlfile)
    end if

    if (trim(PSvariantfile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' variant file name is undefined in '//nmlfile)
    end if
end if

enl%nthreads = nthreads
enl%matchdepth = matchdepth
enl%dotproductfile = dotproductfile
enl%ctffile = ctffile
enl%step = step
enl%PSvariantfile = PSvariantfile
enl%modality = modality

end subroutine GetFitOrientationPSNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetRefineMartensiteNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMRefineMartensite.f90)
!
!> @param nmlfile namelist file name
!> @param enl single name list structure
!
!> @date 01/04/19 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetRefineMartensiteNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetRefineMartensiteNameList

use error
use constants

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(RefineMartensitetype),INTENT(INOUT)          :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
integer(kind=irg)       :: numMartensite
real(kind=sgl)          :: step
character(fnlen)        :: martensiteMPprefix
character(fnlen)        :: martensiteMPpostfix
character(fnlen)        :: ferritedotproductfile
character(fnlen)        :: outputfile

namelist / RefineMartensite / nthreads, ferritedotproductfile, step, numMartensite, martensiteMPprefix, &
                              martensiteMPpostfix, outputfile

nthreads = 1
ferritedotproductfile = 'undefined'
step = 1.0
martensiteMPprefix = 'undefined'
martensiteMPpostfix = 'undefined'
outputfile = 'undefined'
numMartensite = 1

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=RefineMartensite)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(ferritedotproductfile).eq.'undefined') then
        call FatalError('GetRefineMartensiteNameList:',' ferrite dotproduct file name is undefined in '//nmlfile)
    end if

    if (trim(martensiteMPprefix).eq.'undefined') then
        call FatalError('GetRefineMartensiteNameList:',' martensiteMPprefix is undefined in '//nmlfile)
    end if

    if (trim(martensiteMPpostfix).eq.'undefined') then
        call FatalError('GetRefineMartensiteNameList:',' martensiteMPpostfix is undefined in '//nmlfile)
    end if

    if (trim(outputfile).eq.'undefined') then
        call FatalError('GetRefineMartensiteNameList:',' outputfile is undefined in '//nmlfile)
    end if
end if

enl%nthreads = nthreads
enl%ferritedotproductfile = ferritedotproductfile 
enl%step = step
enl%martensiteMPprefix = martensiteMPprefix
enl%martensiteMPpostfix = martensiteMPpostfix
enl%outputfile = outputfile
enl%numMartensite = numMartensite

end subroutine GetRefineMartensiteNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMCCLsphereNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMCCL.f90)
!
!> @param nmlfile namelist file name
!> @param mcnl Monte Carloname list structure
!
!> @date 10/20/16  SS 1.0 new routine; adapted from GetMCCLNameList
!--------------------------------------------------------------------------
recursive subroutine GetMCCLsphereNameList(nmlfile, mcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCCLsphereNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
!f2py intent(in,out) ::  mcnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: globalworkgrpsz
integer(kind=irg)       :: num_el
integer(kind=irg)       :: totnum_el
integer(kind=irg)       :: multiplier
integer(kind=irg)       :: devid
integer(kind=irg)       :: platid
real(kind=dbl)          :: sig
real(kind=dbl)          :: sigstart
real(kind=dbl)          :: sigend
real(kind=dbl)          :: sigstep
real(kind=dbl)          :: omega
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
character(4)            :: MCmode
character(fnlen)        :: xtalname
character(fnlen)        :: dataname
character(fnlen)        :: mode
real(kind=dbl)          :: incloc
real(kind=dbl)          :: radius

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCCLdata / stdout, xtalname, sigstart, numsx, num_el, globalworkgrpsz, EkeV, multiplier, &
dataname, totnum_el, Ehistmin, Ebinsize, depthstep, omega, MCmode, mode, devid, platid, &
sigend, sigstep, sig, radius, incloc

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
numsx = 1501
globalworkgrpsz = 100
num_el = 10
totnum_el = 2000000000
multiplier = 1
devid = 1
platid = 1
sig = 70.D0
sigstart = 70.D0
sigend = 70.D0
sigstep = 1.D0
omega = 0.D0
EkeV = 30.D0
Ehistmin = 5.D0
Ebinsize = 0.5D0
depthstep = 1.0D0
MCmode = 'CSDA'
xtalname = 'undefined'
dataname = 'MCoutput.data'
mode = 'full'
radius = 100.D0
incloc = 0.D0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=MCCLdata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
call FatalError('EMMC:',' structure file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the mcnl fields
mcnl%stdout = stdout
mcnl%numsx = numsx
mcnl%globalworkgrpsz = globalworkgrpsz
mcnl%num_el = num_el
mcnl%totnum_el = totnum_el
mcnl%multiplier = multiplier
mcnl%devid = devid
mcnl%platid = platid
mcnl%sigstart = sigstart
mcnl%sigend = sigend
mcnl%sigstep = sigstep
mcnl%sig = sig
mcnl%omega = omega
mcnl%EkeV = EkeV
mcnl%Ehistmin = Ehistmin
mcnl%Ebinsize = Ebinsize
mcnl%depthstep = depthstep
mcnl%MCmode = MCmode
mcnl%xtalname = xtalname
mcnl%dataname = dataname
mcnl%mode = mode
mcnl%radius = radius
mcnl%incloc = incloc

end subroutine GetMCCLsphereNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMCCLfoilNameList
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMCfoil.f90)
!
!> @param nmlfile namelist file name
!> @param mcnl Monte Carloname list structure
!
!> @date 01/15/17 MDG 1.0 new routine; adapted from GetMCCLNameList
!--------------------------------------------------------------------------
recursive subroutine GetMCCLfoilNameList(nmlfile, mcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCCLfoilNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
!f2py intent(in,out) ::  mcnl
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
integer(kind=irg)       :: numsx
integer(kind=irg)       :: globalworkgrpsz
integer(kind=irg)       :: num_el
integer(kind=irg)       :: totnum_el
integer(kind=irg)       :: multiplier
integer(kind=irg)       :: devid
integer(kind=irg)       :: platid
real(kind=dbl)          :: sig
real(kind=dbl)          :: omega
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
real(kind=dbl)          :: thickness
character(4)            :: MCmode
character(fnlen)        :: xtalname
character(fnlen)        :: dataname

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCCLfoildata / stdout, xtalname, numsx, num_el, globalworkgrpsz, EkeV, multiplier, &
dataname, totnum_el, Ehistmin, Ebinsize, depthstep, depthmax, omega, MCmode, devid, platid, &
sig, thickness

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
numsx = 501
globalworkgrpsz = 100
num_el = 10
totnum_el = 2000000000
multiplier = 1
devid = 1
platid = 1
sig = 20.D0
omega = 0.D0
EkeV = 30.D0
Ehistmin = 10.D0
Ebinsize = 1.0D0
depthmax = 100.0D0
depthstep = 1.0D0
MCmode = 'CSDA'
xtalname = 'undefined'
dataname = 'MCoutput.data'
thickness = 200.0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=MCCLfoildata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
call FatalError('EMMC:',' structure file name is undefined in '//nmlfile)
end if
end if

! if we get here, then all appears to be ok, and we need to fill in the mcnl fields
mcnl%stdout = stdout
mcnl%numsx = numsx
mcnl%globalworkgrpsz = globalworkgrpsz
mcnl%num_el = num_el
mcnl%totnum_el = totnum_el
mcnl%multiplier = multiplier
mcnl%devid = devid
mcnl%platid = platid
mcnl%sig = sig
mcnl%omega = omega
mcnl%EkeV = EkeV
mcnl%Ehistmin = Ehistmin
mcnl%Ebinsize = Ebinsize
mcnl%depthmax = depthmax
mcnl%depthstep = depthstep
mcnl%MCmode = MCmode
mcnl%xtalname = xtalname
mcnl%dataname = dataname
mcnl%thickness = thickness

end subroutine GetMCCLfoilNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDFullNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDFull.f90)
!
!> @param nmlfile namelist file name
!> @param enl EBSD name list structure
!
!> @date 01/24/17  SS 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDFullNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDFullNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(EBSDFullNameListType),INTENT(INOUT)      :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                   :: skipread = .FALSE.

character(fnlen)        :: xtalname
real(kind=dbl)          :: dmin
integer(kind=irg)       :: totnum_el
integer(kind=irg)       :: num_el
real(kind=dbl)          :: EkeV
real(kind=dbl)          :: Ehistmin
real(kind=dbl)          :: Ebinsize
real(kind=dbl)          :: depthmax
real(kind=dbl)          :: depthstep
real(kind=dbl)          :: beamcurrent
real(kind=dbl)          :: dwelltime
real(kind=dbl)          :: sig
real(kind=dbl)          :: omega
real(kind=sgl)          :: L
real(kind=sgl)          :: xpc
real(kind=sgl)          :: ypc
real(kind=sgl)          :: thetac
real(kind=sgl)          :: delta
integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: binning
character(3)            :: scalingmode
real(kind=sgl)          :: gammavalue
character(1)            :: maskpattern
integer(kind=irg)       :: nthreads
integer(kind=irg)       :: platid
integer(kind=irg)       :: devid
integer(kind=irg)       :: globalworkgrpsz
character(3)            :: eulerconvention
character(fnlen)        :: anglefile
character(fnlen)        :: datafile
integer(kind=irg)       :: multiplier


! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDFulldata / xtalname, dmin, totnum_el, EkeV, Ehistmin, Ebinsize, depthmax, depthstep, beamcurrent,&
          dwelltime, sig, omega, L, xpc, ypc, thetac, delta, numsx, numsy, binning, scalingmode, gammavalue, maskpattern,&
          nthreads, platid, devid,  globalworkgrpsz, eulerconvention, anglefile, datafile, multiplier, num_el
! set the input parameters to default values (except for xtalname, anglefile and datafile, which must be present)

xtalname = 'undefined'        ! name of xtal
dmin = 0.04D0                 ! maximum g vector used in computation
totnum_el = 2000000000        ! number of electrons for MC run
num_el = 10                   ! number of electrons per work item
EkeV = 20.D0                  ! incident electron energy [kV]
Ehistmin = 10.D0              ! cutoff energy [kV]
Ebinsize = 1.D0               ! energy step size [kV]
depthmax = 100.D0             ! depth cutoff [nm]
depthstep = 1.D0              ! depth bin size [nm]
beamcurrent = 1000.D0         ! [nA]
dwelltime = 1000.D0           ! [micro seconds]
sig = 70.D0                   ! sample tilt angle [degrees]
omega = 0.D0                  ! tilt about RD axis [degrees]
L = 15000.0                   ! scintillator to sample distance [micro m]
xpc = 0.0                     ! units of pixel [dimensionless]
ypc = 0.0                     ! units of pixel [dimensionless] 
thetac = 10.0                 ! camera elevation [degrees]
delta = 59.2                  ! physical size of detector pixel [micro m]
numsx = 0                     ! number of pixel is x direction of scintillator
numsy = 0                     ! number of pixel is y direction of scintillator
binning = 1                   ! detector binning
scalingmode = 'not'           ! intensity scaling in detector
gammavalue = 0.34             ! intensity scaling factor
maskpattern = 'n'             ! circular mask or not
nthreads = 1                  ! number of CPU threads for computation
platid = 2                    ! platform id for GPU
devid = 1                     ! device id for GPU
globalworkgrpsz = 256         ! work group size
eulerconvention = 'tsl'       ! euler angle convention
anglefile = 'undefined'       ! list of euler angles for which simulation is done
datafile = 'undefined'        ! output HDF5 file
multiplier = 1

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDFulldata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries

 if (trim(anglefile).eq.'undefined') then
  call FatalError('EMEBSD:',' angle file name is undefined in '//nmlfile)
 end if

 if (trim(xtalname).eq.'undefined') then
  call FatalError('EMEBSD:',' xtal file is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('EMEBSD:',' output file name is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then
  call FatalError('EMEBSD:',' pattern size numsx is zero in '//nmlfile)
 end if

 if (numsy.eq.0) then
  call FatalError('EMEBSD:',' pattern size numsy is zero in '//nmlfile)
 end if
end if

enl%xtalname = xtalname                       ! name of xtal
enl%dmin = dmin                               ! maximum g vector used in computation
enl%totnum_el = totnum_el                     ! number of electrons for MC run
enl%num_el = num_el                           ! number of electrons per work item
enl%EkeV = EkeV                               ! incident electron energy [kV]
enl%Ehistmin = Ehistmin                       ! cutoff energy [kV]
enl%Ebinsize = Ebinsize                       ! energy step size [kV]
enl%depthmax = depthmax                       ! depth cutoff [nm]
enl%depthstep = depthstep                     ! depth bin size [nm]
enl%beamcurrent = beamcurrent                 ! [nA]
enl%dwelltime = dwelltime                     ! [micro seconds]
enl%sig = sig                                 ! sample tilt angle [degrees]
enl%omega = omega                             ! tilt about RD axis [degrees]
enl%L = L                                     ! scintillator to sample distance [micro m]
enl%xpc = xpc                                 ! units of pixel [dimensionless]
enl%ypc = ypc                                 ! units of pixel [dimensionless] 
enl%thetac = thetac                           ! camera elevation [degrees]
enl%delta = delta                             ! physical size of detector pixel [micro m]
enl%numsx = numsx                             ! number of pixel is x direction of scintillator
enl%numsy = numsy                             ! number of pixel is y direction of scintillator
enl%binning = binning                         ! detector binning
enl%scalingmode = scalingmode                 ! intensity scaling in detector
enl%gammavalue = gammavalue                   ! intensity scaling factor
enl%maskpattern = maskpattern                 ! circular mask or not
enl%nthreads = nthreads                       ! number of CPU threads for computation
enl%platid = platid                           ! platform id for GPU
enl%devid = devid                             ! device id for GPU
enl%globalworkgrpsz = globalworkgrpsz         ! work group size
enl%eulerconvention = eulerconvention         ! euler angle convention
enl%anglefile = anglefile                     ! list of euler angles for which simulation is done
enl%datafile = datafile                       ! output HDF5 file
enl%multiplier = multiplier

end subroutine GetEBSDFullNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetSRdefectNameList
!
!> @author Patrick Callahan
!
!> @brief read namelist file for EMSRdefect
!
!> @param nmlfile namelist file name
!
!> @date 02/10/17  PGC 1.0 new routine
!--------------------------------------------------------------------------

recursive SUBROUTINE GetSRdefectNameList(nmlfile, srdnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetSRdefectNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)              :: nmlfile
type(SRdefectNameListType), INTENT(INOUT) :: srdnl
!f2py intent(in,out) ::  srdnl
logical, OPTIONAL, INTENT(IN)            :: initonly

logical                                  :: skipread = .FALSE.

real(kind=sgl)    :: DF_L
integer(kind=irg) :: DF_npix
integer(kind=irg) :: DF_npiy
real(kind=sgl)    :: DF_slice
real(kind=sgl)    :: dmin
character(4)      :: progmode
integer(kind=irg) :: dinfo
character(3)      :: outputformat
integer(kind=irg) :: output
character(fnlen)  :: dataname
integer(kind=irg) :: t_interval
character(fnlen)  :: dispfile
integer(kind=irg) :: nthreads
character(3)      :: dispmode
character(fnlen)  :: xtalname
real(kind=sgl)    :: voltage
integer(kind=irg) :: SRG(3)
integer(kind=irg) :: Grange
real(kind=sgl)    :: GLaue
character(fnlen)  :: STEMnmlfile
character(fnlen)  :: defectfilename

! DEFINE THE IO NAMELIST FOR VARIABLE PASSING
namelist / SRdefectdata / DF_L, DF_npix, DF_npiy, DF_slice, dmin, progmode,&
                  dinfo, outputformat, output, dataname, t_interval, dispfile, &
                  nthreads, dispmode, xtalname, voltage, SRG, Grange, GLaue, &
                  STEMnmlfile, defectfilename


!nthreads, voltage, progmode, xtalname, SRG, lauec, STEMnmlfile, &
!                         dataname, defectfilename, dispmode, dispfile, output, dinfo, t_interval, DF_L, &
!                         DF_npix, DF_npiy, DF_slice, dmin

! SET INPUT PARAMETERS TO DEFAULT VALUES (EXCEPT XTALNAME, WHICH MUST BE PRESENT)
nthreads = 6
voltage = 200000
progmode = 'STEM'
xtalname = 'undefined'
SRG = (/ 0.0, 0.0, 1.0 /)
Grange = 4
GLaue = 0.5
!lauec = (/ 0.0, 0.0 /)
STEMnmlfile = 'STEM_rundata.nml'
defectfilename = 'EMdefec/dispt.json'
dataname = '/folder/to/trialbinary.data'
dispmode = 'not'
dispfile = 'test_ZA.data'
output = 6 ! screen output
dinfo = 0 ! 1 is verbose
t_interval = 5 ! update every x steps
DF_L = 1.0
DF_npix = 256
DF_npiy = 256
DF_slice = 1.0 ! slice thickness for scattering matrix approach (nmu)
dmin = 0.03

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=SRdefectdata)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
  call FatalError('SRdefect:',' crystal structure file name is undefined in '//nmlfile)
end if

end if

srdnl%DF_L = DF_L
srdnl%DF_npix = DF_npix
srdnl%DF_npiy = DF_npiy
srdnl%DF_slice = DF_slice
srdnl%dmin = dmin
srdnl%progmode = progmode
srdnl%dinfo = dinfo
srdnl%outputformat = outputformat
srdnl%output = output
srdnl%dataname = dataname
srdnl%t_interval = t_interval
srdnl%dispfile = dispfile
srdnl%nthreads = nthreads
srdnl%dispmode = dispmode
srdnl%xtalname = xtalname
srdnl%voltage = voltage
srdnl%SRG = SRG
srdnl%Grange = Grange
srdnl%GLaue = GLaue
srdnl%STEMnmlfile = STEMnmlfile
srdnl%defectfilename = defectfilename

!srdnl%nthreads = nthreads
!srdnl%voltage = voltage
!srdnl%progmode = progmode
!srdnl%xtalname = xtalname
!srdnl%SRG = SRG
!srdnl%lauec = lauec
!srdnl%STEMnmlfile = STEMnmlfile
!srdnl%dataname = dataname
!srdnl%defectfilename = defectfilename
!srdnl%dispmode = dispmode
!srdnl%dispfile = dispfile
!srdnl%output = output
!srdnl%dinfo = dinfo
!srdnl%t_interval = t_interval
!srdnl%DF_L = DF_L
!srdnl%DF_npix = DF_npix
!srdnl%DF_npiy = DF_npiy
!srdnl%DF_slice = DF_slice
!srdnl%dmin = dmin


END SUBROUTINE GetSRdefectNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetPFInversionNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by PFInversionSIRT.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 04/02/17  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetPFInversionNameList(nmlfile, epf, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetPFInversionNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(PFInversionNameListType),INTENT(INOUT)       :: epf
!f2py intent(in,out) ::  epf
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: nLam, nfiles, ncub
character(fnlen)                                  :: xtalname
character(fnlen)                                  :: datafile, flist(10), mrcfile
real(kind=dbl)                                    :: damp

namelist / PFInversion / nLam, xtalname, datafile, nfiles, flist, ncub, mrcfile, damp

nLam = 20
xtalname = 'undefined'
flist(1:10) = 'undefined'
nfiles = 1
datafile = 'undefined' 
mrcfile = 'undefined'
ncub = 40
damp = 1.D0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=PFInversion)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(xtalname).eq.'undefined') then
        call FatalError('PFForwardTest:',' xtal file name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
        call FatalError('PFForwardTest:',' datafile name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
        call Message('PFForwardTest: MRC filename name is undefined in '//nmlfile)
    end if

end if

epf%xtalname = xtalname
epf%nfiles = nfiles
epf%flist(1:nfiles) = flist(1:nfiles)
epf%nLam = nLam
epf%datafile = datafile
epf%ncub = ncub
epf%mrcfile = mrcfile
epf%damp = damp

end subroutine GetPFInversionNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMgammaNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMgamma.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 06/28/17 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMgammaNameList(nmlfile, epf, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMgammaNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMgammaNameListType),INTENT(INOUT)           :: epf
!f2py intent(in,out) ::  epf
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: npix, k(3), fn(3)
real(kind=sgl)      :: voltage, klaue(2), dmin, convergence, thick
logical             :: variants
character(fnlen)    :: gammaname, gammapname, microfile, variantfile, defectfile, outname

namelist /GAMMAlist/ gammaname, gammapname, microfile, voltage, k, fn, klaue, dmin, &
          convergence,thick, npix, outname, variants, variantfile, defectfile

gammaname = 'undefined' ! initial value to check that the keyword is present in the nml file (gamma phase)
gammapname = 'undefined'  ! initial value to check that the keyword is present in the nml file (gamma' phase)
microfile = 'undefined' ! microstructure file name
variantfile = 'undefined' ! variant information file
defectfile = 'undefined'      ! defect data (optional)
outname = 'gammaout.data' ! output filename
variants = .FALSE.    ! do we load variant information ?
voltage = 200.0    ! acceleration voltage [kV]
k = (/ 0, 0, 1 /)   ! beam direction [direction indices]
fn = (/ 0, 0, 1 /)    ! foil normal [direction indices]
klaue = (/ 0.0, 0.0 /)    ! Laue center coordinates
dmin = 0.04     ! smallest d-spacing to include in dynamical matrix [nm]
convergence = 25.0    ! beam convergence angle [mrad]
thick = 100.0     ! thickness increment
npix = 256      ! output arrays will have size (2*npix+1 x 2*npix+1)

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=GAMMAlist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(gammaname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' gamma xtal file name is undefined in '//nmlfile)
    end if

    if (trim(gammapname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' gamma prime xtal file name is undefined in '//nmlfile)
    end if

    if (trim(microfile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' microfile name is undefined in '//nmlfile)
    end if

    if (trim(outname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

epf%gammaname = gammaname 
epf%gammapname = gammapname 
epf%microfile = microfile
epf%variantfile = variantfile 
epf%defectfile = defectfile
epf%outname = outname
epf%variants = variants
epf%voltage = voltage
epf%k = k
epf%fn = fn
epf%klaue = klaue
epf%dmin = dmin
epf%convergence = convergence
epf%thick = thick 
epf%npix = npix

end subroutine GetEMgammaNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMgammaOpenCLNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMgamma.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 06/28/17 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMgammaOpenCLNameList(nmlfile, epf, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMgammaOpenCLNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMgammaOpenCLNameListType),INTENT(INOUT)     :: epf
!f2py intent(in,out) ::  epf
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: npix, platid, devid
real(kind=sgl)      :: voltage, dmin, thick, eu(3)
character(fnlen)    :: gammaname, gammapname, microfile, variantfile, defectfile, datafile

namelist /GAMMAlist/ gammaname, gammapname, microfile, voltage, dmin, &
          thick, npix, datafile, eu, variantfile, defectfile, platid, devid

gammaname = 'undefined' ! initial value to check that the keyword is present in the nml file (gamma phase)
gammapname = 'undefined'  ! initial value to check that the keyword is present in the nml file (gamma' phase)
microfile = 'undefined' ! microstructure file name
variantfile = 'undefined' ! variant information file
defectfile = 'undefined'      ! defect data (optional)
datafile = 'gammaout.data' ! output filename
voltage = 200.0    ! acceleration voltage [kV]
eu = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
dmin = 0.04     ! smallest d-spacing to include in dynamical matrix [nm]
thick = 100.0     ! thickness increment
npix = 256      ! output arrays will have size (2*npix+1 x 2*npix+1)
platid = 1
devid = 1

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=GAMMAlist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(gammaname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' gamma xtal file name is undefined in '//nmlfile)
    end if

    if (trim(gammapname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' gamma prime xtal file name is undefined in '//nmlfile)
    end if

    if (trim(microfile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' microfile name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

epf%gammaname   = gammaname 
epf%gammapname  = gammapname 
epf%microfile   = microfile
epf%variantfile = variantfile 
epf%defectfile  = defectfile
epf%datafile    = datafile
epf%voltage     = voltage
epf%eu          = eu
epf%dmin        = dmin
epf%thick       = thick 
epf%npix        = npix
epf%platid      = platid
epf%devid       = devid

end subroutine GetEMgammaOpenCLNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMTwoPhaseNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMTwoPhase.f90)
!
!> @param nmlfile namelist file name
!> @param enl single name list structure
!
!> @date 06/28/17 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMTwoPhaseNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMTwoPhaseNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMTwoPhaseNameListType),INTENT(INOUT)        :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: npix
real(kind=sgl)      :: voltage, dmin, zf, zfoil
character(fnlen)    :: xtalname_f, xtalname_s, datafile

namelist /TwoPhaselist/ xtalname_f, xtalname_s, datafile, voltage, &
                        dmin, npix, zf, zfoil

xtalname_f  = 'undefined' ! initial value to check that the keyword is present in the nml file (gamma phase)
xtalname_s  = 'undefined' ! initial value to check that the keyword is present in the nml file (gamma' phase)
datafile    = 'output.h5' ! output filename
voltage     = 200.0       ! acceleration voltage [kV]
dmin        = 0.04        ! smallest d-spacing to include in dynamical matrix [nm]
zf          = 0.0         ! film thickness 
zfoil       = 100.0       ! total thickness of foil including film and substrate
npix        = 144         ! output size of square diffraction pattern

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=TwoPhaselist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(xtalname_f).eq.'undefined') then
        call FatalError('GetEMTwoPhaseList:',' film xtal file name is undefined in '//nmlfile)
    end if

    if (trim(xtalname_s).eq.'undefined') then
        call FatalError('GetEMTwoPhaseList:','substrate xtal file name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
        call FatalError('GetEMTwoPhaseList:',' output file name is undefined in '//nmlfile)
    end if

end if

enl%xtalname_f   = xtalname_f
enl%xtalname_s   = xtalname_s
enl%datafile     = datafile
enl%dmin         = dmin
enl%voltage      = voltage
enl%zf           = zf
enl%npix         = npix
enl%zfoil        = zfoil

end subroutine GetEMTwoPhaseNameList


!--------------------------------------------------------------------------
!
! SUBROUTINE:GetMDElectronPropNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMDPFit.f90)
!
!> @param nmlfile namelist file name
!> @param enl ECP single name list structure
!
!> @date 02/22/16  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetMDElectronPropNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMDElectronPropNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(MDElectronPropNameListType),INTENT(INOUT)    :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: nthreads
integer(kind=irg)       :: npix
real(kind=sgl)          :: thetac
character(1)            :: maskpattern
character(fnlen)        :: energyfile
character(fnlen)        :: datafile
character(3)            :: eulerconvention
real(kind=sgl)          :: gammavalue
real(kind=dbl)          :: sampletilt
real(kind=sgl)          :: workingdistance
real(kind=sgl)          :: Rin
real(kind=sgl)          :: Rout
real(kind=dbl)          :: phi1, phi, phi2
real(kind=sgl)          :: dmin

namelist / MDElectronPropList / nthreads, npix, thetac, maskpattern, energyfile, datafile, eulerconvention, &
gammavalue, sampletilt, workingdistance, Rin, Rout, phi1, phi, phi2, dmin


nthreads = 1
npix = 512
thetac = 5.0
maskpattern = 'n'
energyfile = 'undefined'
datafile = 'undefined'
eulerconvention = 'tsl'
gammavalue = 1.0
sampletilt = 0.D0
workingdistance = 7.0
Rin = 2.0
Rout = 5.0
phi1 = 0.D0
phi = 0.D0
phi2 = 0.D0
dmin = 0.03


! if (present(initonly)) then
!   if (initonly) skipread = .TRUE.
! end if

! if (.not.skipread) then
! ! read the namelist file
!     open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
!     read(UNIT=dataunit,NML=ECPSinglelist)
!     close(UNIT=dataunit,STATUS='keep')

! ! check for required entries
!     if (trim(energyfile).eq.'undefined') then
!         call FatalError('EMDPFit:',' energy file name is undefined in '//nmlfile)
!     end if

! end if

enl%nthreads = nthreads
enl%npix = npix
enl%thetac = thetac
enl%maskpattern = maskpattern
enl%energyfile = energyfile
enl%datafile = datafile
enl%eulerconvention = eulerconvention
enl%gammavalue = gammavalue
enl%sampletilt = sampletilt
enl%workingdistance = workingdistance
enl%Rin = Rin
enl%Rout = Rout
enl%phi1 = phi1
enl%phi = phi
enl%phi2 = phi2
enl%dmin = dmin

end subroutine GetMDElectronPropNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMgammaSTEMNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMgammaSTEM.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 11/20/17 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMgammaSTEMNameList(nmlfile, epf, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMgammaSTEMNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMgammaSTEMNameListType),INTENT(INOUT)       :: epf
!f2py intent(in,out) ::  epf
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: platid, devid
real(kind=sgl)      :: voltage, dmin, eu(3), convergence
character(fnlen)    :: gammaname, gammapname, microstructurefile, datafile

namelist /GAMMAlist/ gammaname, gammapname, microstructurefile, voltage, dmin, &
          datafile, eu, platid, devid, convergence

gammaname = 'undefined' ! initial value to check that the keyword is present in the nml file (gamma phase)
gammapname = 'undefined'  ! initial value to check that the keyword is present in the nml file (gamma' phase)
microstructurefile = 'undefined' ! microstructure file name
datafile = 'undefined' ! output filename
voltage = 200.0    ! acceleration voltage [kV]
eu = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
dmin = 0.04     ! smallest d-spacing to include in dynamical matrix [nm]
platid = 1
devid = 1
convergence = 0.0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=GAMMAlist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(gammaname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' gamma xtal file name is undefined in '//nmlfile)
    end if

    if (trim(gammapname).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' gamma prime xtal file name is undefined in '//nmlfile)
    end if

    if (trim(microstructurefile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' microfile name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

epf%gammaname            = gammaname 
epf%gammapname           = gammapname 
epf%microstructurefile   = microstructurefile
epf%datafile             = datafile
epf%voltage              = voltage
epf%eu                   = eu
epf%dmin                 = dmin
epf%platid               = platid
epf%devid                = devid
epf%convergence          = convergence

end subroutine GetEMgammaSTEMNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMTGBSTEMNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMTGBSTEM.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 05/10/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMTGBSTEMNameList(nmlfile, epf, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMTGBSTEMNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMTGBSTEMNameListType),INTENT(INOUT)         :: epf
!f2py intent(in,out) ::  epf
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: platid, usenumd, selnumd(4)
real(kind=sgl)      :: voltage, dmin, eu(3), convergence
character(fnlen)    :: xtalname, microstructurefile, datafile

namelist /TGBSTEMlist/ xtalname, microstructurefile, voltage, dmin, &
          datafile, eu, platid, usenumd, selnumd, convergence

xtalname            = 'undefined' ! initial value to check that the keyword is present in the nml file (gamma phase)
microstructurefile  = 'undefined' ! microstructure file name
datafile            = 'undefined' ! output filename
voltage             = 200.0    ! acceleration voltage [kV]
eu                  = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
dmin                = 0.04     ! smallest d-spacing to include in dynamical matrix [nm]
platid              = 1
usenumd             = 1
selnumd             = (/1, 0, 0, 0/)
convergence         = 5.0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=TGBSTEMlist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(xtalname).eq.'undefined') then
        call FatalError('GetEMTGBSTEMNameList:',' gamma xtal file name is undefined in '//nmlfile)
    end if

    if (trim(microstructurefile).eq.'undefined') then
        call FatalError('GetEMTGBSTEMNameList:',' microfile name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
        call FatalError('GetEMTGBSTEMNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

epf%xtalname             = xtalname
epf%microstructurefile   = microstructurefile
epf%datafile             = datafile
epf%voltage              = voltage
epf%eu                   = eu
epf%dmin                 = dmin
epf%platid               = platid
epf%usenumd              = usenumd
epf%selnumd              = selnumd
epf%convergence          = convergence

end subroutine GetEMTGBSTEMNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMCBEDQCNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMCBEDQC.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 02/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMCBEDQCNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMCBEDQCNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMCBEDQCNameListType),INTENT(INOUT)          :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: nthreads, atno, npix
real(kind=sgl)      :: voltage, dmin, eu(3), convergence, DWF, QClatparm, thickness
character(fnlen)    :: datafile, qxtalname
character(1)        :: centering

namelist /CBEDQC/ qxtalname, voltage, dmin, nthreads, thickness, &
          datafile, eu, convergence, npix

qxtalname   = 'undefined'
datafile    = 'undefined'           ! output filename
voltage     = 200.0                 ! acceleration voltage [kV]
eu          = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
dmin        = 0.25                  ! smallest d-spacing to include in dynamical matrix [nm]
convergence = 10.0                  ! beam convergence angle [mRad]
nthreads    = 1                     ! number of threads
thickness   = 50.0                  ! film thickness
npix        = 50

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=CBEDQC)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries

    if (trim(datafile).eq.'undefined' .or. trim(qxtalname) .eq. 'undefined') then
        call FatalError('GetEMCBEDQCNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

enl%qxtalname         = qxtalname
enl%datafile              = datafile
enl%voltage               = voltage
enl%eu                    = eu
enl%dmin                  = dmin
enl%nthreads              = nthreads
enl%convergence           = convergence
enl%thickness             = thickness
enl%npix                  = npix

end subroutine GetEMCBEDQCNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMCBED2DQCNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMCBEDQC.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 06/28/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEMCBED2DQCNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMCBED2DQCNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EMCBED2DQCNameListType),INTENT(INOUT)        :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: nthreads, npix
real(kind=sgl)      :: voltage, dmin_qc, dmin_p, eu(3), convergence, thickness
character(fnlen)    :: datafile, qxtalname
character(1)        :: centering

namelist /CBEDQC/ qxtalname, voltage, dmin_qc, dmin_p, nthreads, thickness, &
          datafile, eu, convergence, npix

qxtalname   = 'undefined'
datafile    = 'undefined'           ! output filename
voltage     = 200.0                 ! acceleration voltage [kV]
eu          = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
dmin_qc     = 0.25                  ! smallest d-spacing to include in dynamical matrix [nm] for quaiscrystal plane
dmin_p     = 0.05           ! smallest d-spacing to include in dynamical matrix [nm] for periodic direction
convergence = 10.0                  ! beam convergence angle [mRad]
nthreads    = 1                     ! number of threads
thickness   = 50.0                  ! film thickness
npix        = 50

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=CBEDQC)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries

    if (trim(datafile).eq.'undefined' .or. trim(qxtalname) .eq. 'undefined') then
        call FatalError('GetEMCBED2DQCNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

enl%qxtalname         = qxtalname
enl%datafile              = datafile
enl%voltage               = voltage
enl%eu                    = eu
enl%dmin_qc               = dmin_qc
enl%dmin_p                = dmin_p
enl%nthreads              = nthreads
enl%convergence           = convergence
enl%thickness             = thickness
enl%npix                  = npix

end subroutine GetEMCBED2DQCNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDQCMasterNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDQCmaster.f90)
!
!> @param nmlfile namelist file name
!> @param epf single name list structure
!
!> @date 02/21/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEBSDQCMasterNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDQCMasterNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                           :: nmlfile
type(EBSDQCMasterNameListType),INTENT(INOUT)          :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                           :: initonly

logical                                               :: skipread = .FALSE.

integer(kind=irg)                                     :: nthreads, npx, nsamples
real(kind=sgl)                                        :: dmin
character(fnlen)                                      :: energyfile

namelist /EBSDQCmastervars/ dmin, nthreads, &
          energyfile, npx, nsamples

energyfile  = 'undefined'           ! output filename
dmin        = 0.25                  ! smallest d-spacing to include in dynamical matrix [nm]
nthreads    = 1                     ! number of threads
npx         = 500                   ! size of master pattern
nsamples    = 200                   ! number of samples for sampling k vectors

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=EBSDQCmastervars)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries

    if (trim(energyfile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

enl%energyfile            = energyfile
enl%dmin                  = dmin
enl%nthreads              = nthreads
enl%nsamples              = nsamples
enl%npx                   = npx

end subroutine GetEBSDQCMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSD2DQCMasterNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSDQCmaster.f90)
!
!> @param nmlfile namelist file name
!> @param enl single name list structure
!
!> @date 05/1/18 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine GetEBSD2DQCMasterNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSD2DQCMasterNameList

use error
use constants
use io

IMPLICIT NONE

character(fnlen),INTENT(IN)                           :: nmlfile
type(EBSD2DQCMasterNameListType),INTENT(INOUT)        :: enl
!f2py intent(in,out) ::  enl
logical,OPTIONAL,INTENT(IN)                           :: initonly

logical                                               :: skipread = .FALSE.

integer(kind=irg)                                     :: nthreads, atno, npx, nsamples
real(kind=sgl)                                        :: dmin_qc, dmin_p, DWF, QClatparm_a, QClatparm_c
character(fnlen)                                      :: energyfile
character(3)                                          :: QCtype

namelist /EBSD2DQCmastervars/ dmin_qc, dmin_p, nthreads, &
          energyfile, QClatparm_a, QClatparm_c, npx

energyfile  = 'undefined'           ! output filename
dmin_qc     = 0.25                  ! smallest d-spacing to include in dynamical matrix [nm] in quasiperiodic dimensions
dmin_p      = 0.05                  ! smallest d-spacing to include in dynamical matrix [nm] in periodic dimensions
QClatparm_a = 0.50                  ! lattice parameter of hyper-lattice in aperiodic plane
QClatparm_c = 0.50                  ! lattice parameter of hyper-lattice in periodic axial direction
nthreads    = 1                     ! number of threads
npx         = 500                   ! size of master pattern

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=EBSD2DQCmastervars)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries

    if (trim(energyfile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' output file name is undefined in '//nmlfile)
    end if

    if (trim(QCtype).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' type of 2D quasi-crystals is undefined in '//nmlfile)
    end if

end if

enl%energyfile            = energyfile
enl%dmin_qc               = dmin_qc
enl%dmin_p                = dmin_p
enl%nthreads              = nthreads
enl%DWF                   = DWF
enl%atno                  = atno
enl%QClatparm_a           = QClatparm_a
enl%QClatparm_c           = QClatparm_c
enl%QCtype                = QCtype
enl%npx                   = npx

end subroutine GetEBSD2DQCMasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMmdSTEMNameList
!
!> @author JosephTessmer, Carnegie Mellon University
!
!> @brief read namelist file and fill mdstem structure (used by EMgammaSTEM.f90)

!
!> @date 06/28/17 jt 1.0 original
!--------------------------------------------------------------------------

recursive subroutine GetEMmdSTEMNameList(nmlfile, msnml, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMmdSTEMNameList

use error
use constants
use io

IMPLICIT NONE

type(EMmdSTEMNameListType),INTENT(INOUT)          :: msnml
!f2py intent(in,out) ::  msnml
character(fnlen),INTENT(IN)                       :: nmlfile

logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.
character(fnlen)        :: xtalname,datafile,inputfilename
logical                 :: hypslab,dwflag
character(3)            :: eulerconvention
integer(kind=irg)       :: platid, devid, discsize, usenumd, selnumd(4), maxnumincell
real(kind=sgl)          :: voltage, dmin, eu(3), convergence, scalefactor(6), stride
real(kind=dbl)          :: phi1, phi2, phi3, thk 
logical                 :: presorted
integer(kind=irg)       :: subslice
integer(kind=irg)       :: ZAindex(3)
real(kind=sgl)          :: lauec(2)




namelist /MDSTEMlist/ xtalname, datafile, eu, eulerconvention, phi1, phi2, phi3, dmin, &
  voltage, convergence, platid, devid, inputfilename, scalefactor, usenumd, selnumd, &
  discsize, stride, maxnumincell, hypslab, dwflag, thk, presorted, subslice, ZAindex, &
  lauec


datafile            = 'undefined' ! output filename
inputfilename       = 'undefined' ! input filename
voltage             = 200.0    ! acceleration voltage [kV]
eu                  = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
scalefactor         = (/ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /)
dmin                = 0.04 ! smallest d-spacing to include in dynamical matrix [nm]
platid              = 1
devid               = 1
convergence         = 0.001 
usenumd             = 1
selnumd             = (/1, 0, 0, 0/)
discsize            = 10.0   ! keep this
stride              = 0.0001 ! keep this
maxnumincell        = 8
hypslab             = .TRUE. ! keep this
dwflag              = .TRUE. ! keep this
thk                 = 1.0
presorted           = .False.
subslice            = 1 ! how many subslices to use, default 0
ZAindex             = (/ 0, 0, 1 /)
lauec               = (/ 0.0, 0.0 /)


if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=MDSTEMlist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries

    if (trim(datafile).eq.'undefined') then
        call FatalError('GetEMmdSTEMNameList:',' output file name is undefined in '//nmlfile)
    end if

    if (trim(inputfilename).eq.'undefined') then
        call FatalError('GetEMmdSTEMNameList:',' input file name is undefined in '//nmlfile)
    end if
end if


! both 
msnml%thk                  = thk
msnml%subslice             = subslice
msnml%lauec                = lauec
msnml%ZAindex              = ZAindex
msnml%xtalname             = xtalname
msnml%datafile             = datafile
msnml%voltage              = voltage
msnml%eu                   = eu
msnml%dmin                 = dmin
msnml%platid               = platid
msnml%devid                = devid
msnml%convergence          = convergence
msnml%inputfilename        = inputfilename
msnml%usenumd              = usenumd
msnml%selnumd              = selnumd
msnml%discsize             = discsize
msnml%hypslab              = hypslab

! DDD
msnml%presorted            = presorted
msnml%scalefactor          = scalefactor

! MD
msnml%dwflag               = dwflag
msnml%stride               = stride
msnml%maxnumincell         = maxnumincell

end subroutine GetEMmdSTEMNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMIntegrateSTEMNameList
!
!> @author JosephTessmer, Carnegie Mellon University
!
!> @brief read namelist file and fill mdstem structure (used by EMgammaSTEM.f90)

!
!> @date 06/28/17 jt 1.0 original
!--------------------------------------------------------------------------

recursive subroutine GetEMIntegrateSTEMNameList(nmlfile, isnml, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMIntegrateSTEMNameList

use error
use constants
use io

IMPLICIT NONE

type(EMIntegrateSTEMNameListType),INTENT(INOUT)   :: isnml
character(fnlen),INTENT(IN)                       :: nmlfile

logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.
character(fnlen)        :: inputfilename
integer(kind=irg)       :: mode
integer(kind=irg)       :: ref(3)
integer(kind=irg)       :: camlen
integer(kind=irg)       :: pixsize
real(kind=sgl)          :: id 
real(kind=sgl)          :: od
logical                 :: CBED




namelist /IntegrateSTEMNamelist/ inputfilename, mode, ref, camlen, pixsize, id, od, CBED


inputfilename       = 'undefined'    ! input filename
mode                = 2              ! annular detector mode
ref                 = (/ 0, 0, 0 /)  ! default reflection for single ref is the through beam
camlen              = 3              ! camera length in cm 
pixsize             = 9000           ! pixel size of detector in nanometers
id                  = 9              ! id in mm
od                  = 18             ! od in mm
CBED                = .False.        ! generate a cbed pattern

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=IntegrateSTEMNamelist)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(inputfilename).eq.'undefined') then
        call FatalError('GetEMIntegrateSTEMNameList:',' input file name is undefined in '//nmlfile)
    end if
end if


isnml%inputfilename = inputfilename
isnml%mode          = mode
isnml%ref           = ref
isnml%camlen        = camlen
isnml%pixsize       = pixsize
isnml%id            = id
isnml%od            = od
isnml%CBED          = CBED

end subroutine GetEMIntegrateSTEMNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEMhh4NameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill hhnl structure (used by EMhh4.f90)
!
!> @param nmlfile namelist file name
!> @param hhnl name list structure
!> @param initonly [optional] logical
!
!> @date 08/15/19  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEMhh4NameList(nmlfile, hhnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEMhh4NameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                 :: nmlfile
type(EMhh4NameListType),INTENT(INOUT)       :: hhnl
!f2py intent(in,out) ::  hhnl
logical,OPTIONAL,INTENT(IN)                 :: initonly

logical                                     :: skipread = .FALSE.

integer(kind=irg)       :: IROW
integer(kind=irg)       :: ICOL
integer(kind=irg)       :: LB(3), LD 
integer(kind=irg)       :: LB2(3), LD2
integer(kind=irg)       :: LB3(3), LD3
integer(kind=irg)       :: LB4(3), LD4
integer(kind=irg)       :: LU(3)
integer(kind=irg)       :: LG(3)
integer(kind=irg)       :: LBM(3)
integer(kind=irg)       :: LFN(3)
integer(kind=irg)       :: wnum
integer(kind=irg)       :: LFP1(3), LFP(3), LFP3(3)
integer(kind=irg)       :: LS1(3), LQ1 
integer(kind=irg)       :: LS2(3), LQ2 
integer(kind=irg)       :: LS3(3), LQ3 
integer(kind=sgl)       :: LTEST
real(kind=sgl)          :: kV
real(kind=sgl)          :: THICK, START, FINISH
real(kind=sgl)          :: wmin, wmax
real(kind=sgl)          :: SEP, SEP2
real(kind=sgl)          :: FAP1, FAP3
real(kind=sgl)          :: D1row1(6)
real(kind=sgl)          :: D1row2(6)
real(kind=sgl)          :: D1row3(6)
real(kind=sgl)          :: D1row4(6)
real(kind=sgl)          :: D1row5(6)
real(kind=sgl)          :: D1row6(6)
character(fnlen)        :: xtalname
character(fnlen)        :: outname
character(fnlen)        :: imageprefix
character(fnlen)        :: imagetype 

namelist /hhlist/ IROW, ICOL, LB, LD , LB2, LD2, LB3, LD3, LB4, LD4, LU, LG, LBM, LFN, &
                  wnum, LFP1, LFP, LFP3, LS1, LQ1 , LS2, LQ2 , LS3, LQ3 , LTEST, kV, THICK, START, FINISH, &
                  wmin, wmax, SEP, SEP2, FAP1, FAP3, D1row1, D1row2, D1row3, D1row4, D1row5, D1row6,&
                  xtalname, outname, imageprefix, imagetype

 xtalname = 'undefined'
 outname = 'undefined'
 imageprefix = 'undefined'
 imagetype = 'tiff'
 IROW = 160
 ICOL = 256
 kV = 200.0
 LB = (/1, 0, 1/)
 LD = 2
 LB2 = (/0, 0, 0/) 
 LD2 = 1
 LB3 = (/0, 0, 0/)
 LD3 = 1
 LB4 = (/0, 0, 0/)
 LD4 = 1
 LU = (/1, 1, 1/)
 LG = (/2, 0, 0/)
 LBM = (/0, 0, 1/)
 LFN = (/0, 0, 1/)
 THICK = 5.0
 START = 0.0
 FINISH = 6.0
 wmin = -1.0
 wmax =  1.0
 wnum =  5
 LFP1 = (/0, 0, 0/)
 LFP = (/0, 0, 0/)
 LFP3 = (/0, 0, 0/) 
 LS1 = (/0, 0, 0/) 
 LQ1 = 2
 LS2 = (/0, 0, 0/) 
 LQ2 = 2
 LS3 = (/0, 0, 0/) 
 LQ3 = 2      
 SEP = 2.0
 SEP2 = 2.0
 FAP1 = 0.0
 FAP3 = 0.0
 D1row1 = (/100.0, 80.0, 80.0,  0.0,  0.0,  0.0/)
 D1row2 = (/ 80.0,100.0, 80.0,  0.0,  0.0,  0.0/)
 D1row3 = (/ 80.0, 80.0,100.0,  0.0,  0.0,  0.0/)
 D1row4 = (/  0.0,  0.0,  0.0, 50.0,  0.0,  0.0/)
 D1row5 = (/  0.0,  0.0,  0.0,  0.0, 50.0,  0.0/)
 D1row6 = (/  0.0,  0.0,  0.0,  0.0,  0.0, 50.0/)
 LTEST = 0

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=hhlist)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(outname).eq.'undefined') then
  call FatalError('GetEMhh4NameList:',' output HDF file name is undefined in '//nmlfile)
 end if

 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetEMhh4NameList:',' xtalname name is undefined in '//nmlfile)
 end if

end if

hhnl%IROW = IROW
hhnl%ICOL = ICOL
hhnl%LB = LB
hhnl%LD  = LD
hhnl%LB2 = LB2
hhnl%LD2 = LD2
hhnl%LB3 = LB3
hhnl%LD3 = LD3
hhnl%LB4 = LB4
hhnl%LD4 = LD4
hhnl%LU = LU
hhnl%LG = LG
hhnl%LBM = LBM
hhnl%LFN = LFN
hhnl%wnum = wnum
hhnl%LFP1 = LFP1
hhnl%LFP = LFP
hhnl%LFP3 = LFP3
hhnl%LS1 = LS1
hhnl%LQ1  = LQ1
hhnl%LS2 = LS2
hhnl%LQ2  = LQ2
hhnl%LS3 = LS3
hhnl%LQ3  = LQ3
hhnl%LTEST = LTEST
hhnl%kV = kV
hhnl%THICK = THICK
hhnl%START = START
hhnl%FINISH = FINISH
hhnl%wmin = wmin
hhnl%wmax = wmax
hhnl%SEP = SEP
hhnl%SEP2 = SEP2
hhnl%FAP1 = FAP1
hhnl%FAP3 = FAP3
hhnl%D1row1 = D1row1 
hhnl%D1row2 = D1row2 
hhnl%D1row3 = D1row3 
hhnl%D1row4 = D1row4 
hhnl%D1row5 = D1row5 
hhnl%D1row6 = D1row6 
hhnl%xtalname = xtalname
hhnl%outname = outname
hhnl%imageprefix = imageprefix
hhnl%imagetype = imagetype 

end subroutine GetEMhh4NameList


end module NameListHandlers
