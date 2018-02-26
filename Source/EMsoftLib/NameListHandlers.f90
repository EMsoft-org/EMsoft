! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
! SUBROUTINE:GetOMmasterNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMC.f90)
!
!> @param nmlfile namelist file name
!> @param omnl name list structure
!
!> @date 06/18/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetOMmasterNameList(nmlfile, omnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOMmasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(OMmasterNameListType),INTENT(INOUT)      :: omnl
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
namelist  / OMMasterData / npx, nthreads, eps1Re, eps1Im, eps2Re, eps2Im, wl, theta, &
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
 read(UNIT=dataunit,NML=OMMasterdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(xtalname).eq.'undefined') then
  call FatalError('GetOMmasterNameList:',' structure file name is undefined in '//nmlfile)
 end if
 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetOMmasterNameList:',' master output file name is undefined in '//nmlfile)
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

end subroutine GetOMmasterNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetOMNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill mcnl structure (used by EMMC.f90)
!
!> @param nmlfile namelist file name
!> @param omnl name list structure
!
!> @date 09/21/17  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetOMNameList(nmlfile, omnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetOMNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                   :: nmlfile
type(OMNameListType),INTENT(INOUT)            :: omnl
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
namelist  / OMData / phinum, numpx, numpy, masterfile, outputfile, anglefile, tiffprefix

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
 read(UNIT=dataunit,NML=OMdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(masterfile).eq.'undefined') then
  call FatalError('GetOMmasterNameList:',' master output file name is undefined in '//nmlfile)
 end if
 if (trim(anglefile).eq.'undefined') then
  call FatalError('GetOMmasterNameList:',' angle input file name is undefined in '//nmlfile)
 end if
 if (trim(outputfile).eq.'undefined') then
  call FatalError('GetOMmasterNameList:',' intensity output file name is undefined in '//nmlfile)
 end if
end if

omnl%phinum = phinum
omnl%numpx = numpx
omnl%numpy = numpy
omnl%masterfile = masterfile
omnl%outputfile = outputfile
omnl%anglefile = anglefile
omnl%tiffprefix = tiffprefix

end subroutine GetOMNameList
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
!--------------------------------------------------------------------------
recursive subroutine GetMCCLNameList(nmlfile, mcnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetMCCLNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)             :: nmlfile
type(MCCLNameListType),INTENT(INOUT)    :: mcnl
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
character(3)            :: Notify
character(4)            :: MCmode
character(fnlen)        :: xtalname
character(fnlen)        :: dataname
character(fnlen)        :: mode

! define the IO namelist to facilitate passing variables to the program.
namelist  / MCCLdata / stdout, xtalname, sigstart, numsx, num_el, globalworkgrpsz, EkeV, multiplier, &
dataname, totnum_el, Ehistmin, Ebinsize, depthmax, depthstep, omega, MCmode, mode, devid, platid, &
sigend, sigstep, sig, Notify

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
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: nmuse
integer(kind=irg)       :: reldisx
integer(kind=irg)       :: reldisy
logical                 :: oldformat
character(fnlen)        :: dotproductfile
character(fnlen)        :: averagectffile
character(fnlen)        :: averagetxtfile
character(fnlen)        :: disorientationmap

! define the IO namelist to facilitate passing variables to the program.
namelist /AverageOrientation/ nmuse, dotproductfile, averagectffile, oldformat, averagetxtfile, &
                              reldisx, reldisy, disorientationmap

! set the input parameters to default values (except for xtalname, which must be present)
nmuse = 10                      ! number of near-matches to use
reldisx = 0                     ! x-coordinate for relative disorientation map
reldisy = 0                     ! y-coordinate for relative disorientation map
oldformat = .FALSE.             ! switch for older format of dot product files
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

 if (oldformat.eqv..TRUE.) then
   if (trim(averagetxtfile).eq.'undefined') then
    call FatalError(' EMAverageOrient',' txt output file name is undefined in '//nmlfile)
   end if
 else
   if (trim(averagectffile).eq.'undefined') then
    call FatalError(' EMAverageOrient',' ctf output file name is undefined in '//nmlfile)
   end if
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%nmuse = nmuse
emnl%reldisx = reldisx
emnl%reldisy = reldisy
emnl%oldformat = oldformat
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
!--------------------------------------------------------------------------
recursive subroutine GetEBSDMasterNameList(nmlfile, emnl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDMasterNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDMasterNameListType),INTENT(INOUT)      :: emnl
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
logical                 :: combinesites
logical                 :: restart
logical                 :: uniform

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDmastervars/ dmin,npx,nthreads,copyfromenergyfile,energyfile,Esel,restart,uniform,Notify, &
                          combinesites

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
npx = 500                       ! Nx pixels (total = 2Nx+1)
nthreads = 1
Esel = -1                       ! selected energy value for single energy run
dmin = 0.025                    ! smallest d-spacing to include in dynamical matrix [nm]
Notify = 'Off'
copyfromenergyfile = 'undefined'! default filename for z_0(E_e) data from a different Monte Carlo simulation
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
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
  call FatalError('EMEBSDmaster:',' output (energy) file name is undefined in '//nmlfile)
 end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
emnl%stdout = stdout
emnl%npx = npx
emnl%Esel = Esel
emnl%nthreads = nthreads
emnl%dmin = dmin
emnl%copyfromenergyfile = copyfromenergyfile
emnl%energyfile = energyfile
emnl%Notify = Notify
emnl%outname = energyfile       ! as off release 3.1, outname must be the same as energyfile
emnl%combinesites = combinesites
emnl%restart = restart
emnl%uniform = uniform

end subroutine GetEBSDMasterNameList

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
namelist /ECPQCmastervars/ nsamples, DWF, atno, dmin, gmax_orth, energyfile, Esel, npx, nthreads, QClatparm, centering

! set the input parameters to default values (except for xtalname, which must be present)
Esel = -1                      ! selected energy value for single energy run
nthreads = 1
dmin = 0.04                    ! smallest d-spacing to include in dynamical matrix [nm]
gmax_orth = 2.0                ! smallest d-spacing to include in dynamical matrix [nm]
QClatparm = 0.46
atno = 28
DWF = 0.004
nsamples = 400
npx = 256
centering = 'P'
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
ecpnl%Esel = Esel
ecpnl%nsamples = nsamples
ecpnl%npx = npx
ecpnl%nthreads = nthreads
ecpnl%dmin = dmin
ecpnl%atno = atno
ecpnl%DWF = DWF
ecpnl%QClatparm = QClatparm
ecpnl%gmax_orth = gmax_orth
ecpnl%energyfile = energyfile
ecpnl%centering = centering

end subroutine GetECPQCMasterNameList


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
character(fnlen)        :: energyfile
logical                 :: combinesites

! define the IO namelist to facilitate passing variables to the program.
namelist /ECPmastervars/ stdout, dmin, compmode, Notify, &
    energyfile, Esel, npx, nthreads, copyfromenergyfile, combinesites

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
Esel = -1                       ! selected energy value for single energy run
nthreads = 1
dmin = 0.04                    ! smallest d-spacing to include in dynamical matrix [nm]
npx = 256
Notify = 'Off'
compmode = 'Blochwv'
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
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

integer(kind=irg)                              :: numphi
integer(kind=irg)                              :: numtheta
real(kind=sgl)                                 :: dmin
character(fnlen)                               :: masterfile
character(fnlen)                               :: energyfile

! define the IO namelist to facilitate passing variables to the program.
namelist /EBSDreflectors/ numphi, numtheta, dmin, masterfile, energyfile

! set the input parameters to default values (except for xtalname, which must be present)
numphi = 360
numtheta = 10
dmin = 0.05                    ! smallest d-spacing to include in dynamical matrix [nm]
masterfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations
energyfile = 'undefined'        ! default filename for z_0(E_e) data from EMMC Monte Carlo simulations

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
  open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
  read(UNIT=dataunit,NML=EBSDreflectors)
  close(UNIT=dataunit,STATUS='keep')

! check for required entries
  if (trim(energyfile).eq.'undefined') then
    call FatalError('EMreflectors:',' energy file name is undefined in '//nmlfile)
  end if
  if (trim(masterfile).eq.'undefined') then
    call FatalError('EMreflectors:',' master file name is undefined in '//nmlfile)
  end if
end if

! if we get here, then all appears to be ok, and we need to fill in the emnl fields
rnl%numphi = numphi
rnl%numtheta = numtheta
rnl%dmin = dmin
rnl%masterfile = masterfile
rnl%energyfile = energyfile

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
logical,OPTIONAL,INTENT(IN)                    :: initonly

logical                                        :: skipread = .FALSE.

real(kind=sgl)                                 :: dmin
real(kind=sgl)                                 :: thr
real(kind=sgl)                                 :: voltage
character(fnlen)                               :: xtalname
character(fnlen)                               :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist /EMkinematical/ dmin, voltage, thr, xtalname, datafile

! set the input parameters to default values (except for xtalname, which must be present)
dmin = 0.05                    ! smallest d-spacing to include in dynamical matrix [nm]
thr = 1.0                      ! smallest |structurefactor|^2 to include
voltage = 30000.0              ! microscope voltage [V]
datafile = 'undefined'         ! output file name
xtalname = 'undefined'         ! structure file name

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

end subroutine GetkinematicalNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetEBSDdetparmscanNameList
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMEBSD.f90)
!
!> @param nmlfile namelist file name
!> @param enl EBSD name list structure
!
!> @date 06/23/14  MDG 1.0 new routine
!--------------------------------------------------------------------------
recursive subroutine GetEBSDdetparmscanNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDdetparmscanNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)               :: nmlfile
type(EBSDdetparmscanNameListType),INTENT(INOUT)      :: enl
logical,OPTIONAL,INTENT(IN)               :: initonly

logical                                   :: skipread = .FALSE.

integer(kind=irg)       :: numdetparm
integer(kind=irg)       :: numeuler
real(kind=sgl)          :: DetParms(3)
real(kind=sgl)          :: Eulertriplet(3)
real(kind=sgl)          :: DetParmstepsize(3)
real(kind=sgl)          :: Cubochoricstepsize(3)

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDscandata / numdetparm, numeuler, DetParms, Eulertriplet, DetParmstepsize, Cubochoricstepsize

! set the input parameters to default values (except for xtalname, which must be present)
numdetparm      = 10            ! number of detector parameter steps (2x+1)
numeuler        = 10            ! number of Euler angle steps (2x+1)
DetParms        = (/ 0.0, 0.0, 15000.0 /)
Eulertriplet    = (/ 0.0, 0.0, 0.0 /)
DetParmstepsize = (/ 1.0, 1.0, 100.0 /)
Cubochoricstepsize   = (/ 0.01, 0.01, 0.01 /)

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSDscandata)
 close(UNIT=dataunit,STATUS='keep')
end if

! if we get here, then all appears to be ok, and we need to fill in the enl fields
enl%numdetparm = numdetparm
enl%numeuler = numeuler
enl%DetParms = DetParms 
enl%DetParmstepsize = DetParmstepsize
enl%Eulertriplet = Eulertriplet 
enl%Cubochoricstepsize = Cubochoricstepsize 

end subroutine GetEBSDdetparmscanNameList

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
!--------------------------------------------------------------------------
recursive subroutine GetEBSDNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)               :: nmlfile
type(EBSDNameListType),INTENT(INOUT)      :: enl
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
real(kind=dbl)          :: Ftensor(3,3)
real(kind=dbl)          :: beamcurrent
real(kind=dbl)          :: dwelltime
character(1)            :: includebackground
character(1)            :: applyDeformation
character(1)            :: maskpattern
character(1)            :: spatialaverage
character(3)            :: scalingmode
character(3)            :: eulerconvention
character(3)            :: outputformat
character(5)            :: bitdepth
character(fnlen)        :: anglefile
character(fnlen)        :: masterfile
character(fnlen)        :: energyfile 
character(fnlen)        :: datafile

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDdata / stdout, L, thetac, delta, numsx, numsy, xpc, ypc, anglefile, eulerconvention, masterfile, bitdepth, &
                        energyfile, datafile, beamcurrent, dwelltime, energymin, energymax, binning, gammavalue, alphaBD, &
                        scalingmode, axisangle, nthreads, outputformat, maskpattern, energyaverage, omega, spatialaverage, &
                        applyDeformation, Ftensor, includebackground

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
numsx           = 640           ! [dimensionless]
numsy           = 480           ! [dimensionless]
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
Ftensor         = reshape( (/ 1.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 1.D0 /), (/ 3,3 /) )
beamcurrent     = 14.513D0      ! beam current (actually emission current) in nano ampere
dwelltime       = 100.0D0       ! in microseconds
includebackground = 'y'         ! set to 'n' to remove realistic background intensity profile
applyDeformation = 'n'          ! should we apply a deformation tensor to the unit cell?
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
eulerconvention = 'tsl'         ! convention for the first Euler angle ['tsl' or 'hkl']
outputformat    = 'gui'         ! output format for 'bin' or 'gui' use
bitdepth        = '8bit'        ! format for output; '8char' for [0..255], '##int' for integers, 'float' for floats
! the '##int' notation stands for the actual bitdepth; all values are stored as 32bit integers, but they are scaled
! from the float values to a maximum that is given by the first two digits, which indicate the bit depth; so, valid
! values would be '10int' for a 10-bit integer scale, '16int' for a 16-bit integer scale, and so on.
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
 read(UNIT=dataunit,NML=EBSDdata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(energyfile).eq.'undefined') then
  call FatalError('EMEBSD:',' energy file name is undefined in '//nmlfile)
 end if

 if (trim(anglefile).eq.'undefined') then
  call FatalError('EMEBSD:',' angle file name is undefined in '//nmlfile)
 end if

 if (trim(masterfile).eq.'undefined') then
  call FatalError('EMEBSD:',' master pattern file name is undefined in '//nmlfile)
 end if

 if (trim(datafile).eq.'undefined') then
  call FatalError('EMEBSD:',' output file name is undefined in '//nmlfile)
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
enl%Ftensor = Ftensor
enl%beamcurrent = beamcurrent
enl%dwelltime = dwelltime
enl%includebackground = includebackground
enl%applyDeformation = applyDeformation
enl%maskpattern = maskpattern
enl%scalingmode = scalingmode
enl%eulerconvention = eulerconvention
enl%outputformat = outputformat
enl%bitdepth = bitdepth
enl%anglefile = anglefile
enl%masterfile = masterfile
enl%energyfile = energyfile
enl%datafile = datafile
enl%omega = omega
enl%spatialaverage = spatialaverage
end subroutine GetEBSDNameList

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
numsx           = 640           ! [dimensionless]
numsy           = 480           ! [dimensionless]
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
!--------------------------------------------------------------------------
recursive subroutine GetEBSDoverlapNameList(nmlfile, enl, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: GetEBSDoverlapNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                     :: nmlfile
type(EBSDoverlapNameListType),INTENT(INOUT)     :: enl
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
namelist  / EBSDoverlapdata / stdout, PatternAxisA, tA, tB, gA, gB, fracA, masterfileA, masterfileB, & 
                              datafile, HorizontalAxisA

! set the input parameters to default values (except for xtalname, which must be present)
stdout          = 6
PatternAxisA    = (/ 0, 0, 1 /)                 ! center axis for output pattern
HorizontalAxisA = (/ 1, 0, 0 /)                 ! horizontal axis for output pattern
tA              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal A
tB              = (/0.0, 0.0, 1.0/)             ! direction vector in crystal B
gA              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal A
gB              = (/1.0, 0.0, 0.0/)             ! plane normal in crystal B
fracA           = 0.5                           ! volume fraction of phase A 
masterfileA     = 'undefined'   ! filename
masterfileB     = 'undefined'   ! filename
datafile        = 'undefined'   ! output file name

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
numsx           = 640
numsy           = 480
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
  call FatalError('EMTKDspots:',' xtal input file is undefined in '//nmlfile)
 end if

 if (trim(outname).eq.'undefined') then
  call FatalError('EMTKDspots:',' output file name B is undefined in '//nmlfile)
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
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.

integer(kind=irg)       :: stdout
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

namelist /inputlist/ stdout, xtalname, voltage, k, fn, dmin, convergence, minten, &
                              nthreads, startthick, thickinc, numthick, outname, npix, maxHOLZ

stdout = 6                      ! standard output
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
minten = 1.0E-5                 ! minimum intensity in diffraction disk to make it into the output file
xtalname = 'undefined'          ! initial value to check that the keyword is present in the nml file
outname = 'lacbedout.data'      ! output filename

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(nmlfile)),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=inputlist)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(xtalname).eq.'undefined') then
  call FatalError('EMLACBED:',' structure file name is undefined in '//nmlfile)
end if
end if

lacbednl%stdout = stdout
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
character(fnlen)        :: defectfilename
character(fnlen)        :: dispfile
character(fnlen)        :: dataname
character(fnlen)        :: ECPname
character(fnlen)        :: sgname

! define the IO namelist to facilitate passing variables to the program.
namelist / ECCIlist / DF_L, DF_npix, DF_npiy, DF_slice, dmin, sgname, stdout, &
                      progmode, dispfile, ktmax, dkt, ECPname, summode, lauec, lauec2, &
                      dispmode, nthreads, xtalname, voltage, k, nktstep, &
                      dataname, defectfilename

! set the input parameters to default values (except for xtalname, which must be present)
stdout = 6
nthreads = 1
k = (/ 0,0,1 /)
nktstep = 20
DF_npix = 256
DF_npiy = 256
voltage = 30000.
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
defectfilename = 'undefined'
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
eccinl%defectfilename = defectfilename
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
logical,OPTIONAL,INTENT(IN)                     :: initonly

logical                                         :: skipread = .FALSE.

integer(kind=irg)                               :: pgnum, nsteps, gridtype
real(kind=dbl)                                  :: rodrigues(4), maxmisor, conevector(3), semiconeangle
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
                     samplemode, rodrigues, maxmisor, conevector, semiconeangle, xtalname

! initialize to default values
pgnum = 32
nsteps = 50
gridtype = 0
rodrigues = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)  ! initialize as the identity rotation
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
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)       :: numsx
integer(kind=irg)       :: numsy
integer(kind=irg)       :: hipasswnsteps
integer(kind=irg)       :: nregionsmin
integer(kind=irg)       :: nregionsmax
integer(kind=irg)       :: nregionsstepsize
integer(kind=irg)       :: patnum
real(kind=sgl)          :: hipasswmax
character(fnlen)        :: tifffile
character(fnlen)        :: exptfile

namelist / EBSDDIpreviewdata / numsx, numsy, hipasswmax, hipasswnsteps, nregionsstepsize, &
          nregionsmax, nregionsmin, patnum, tifffile, exptfile

! set the input parameters to default values
numsx = 640
numsy = 480
hipasswmax = 0.5
hipasswnsteps = 10
nregionsmin = 1
nregionsmax = 10
nregionsstepsize = 1
patnum = 1
tifffile = 'undefined'
exptfile = 'undefined'

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
end if

enl%numsx = numsx
enl%numsy = numsy
enl%hipasswnsteps = hipasswnsteps
enl%nregionsmin = nregionsmin
enl%nregionsmax = nregionsmax
enl%nregionsstepsize = nregionsstepsize
enl%patnum = patnum
enl%hipasswmax = hipasswmax
enl%tifffile = tifffile
enl%exptfile = exptfile

end subroutine GetEBSDDIpreviewNameList

!--------------------------------------------------------------------------
!
! SUBROUTINE:GetBSDIndxNameList
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief read namelist file and fill enl structure (used by EMDynamicEBSDIndeixing.f90)
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

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(EBSDIndexingNameListType),INTENT(INOUT)      :: enl
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: numsx
integer(kind=irg)                                 :: numsy
integer(kind=irg)                                 :: binning
integer(kind=irg)                                 :: energyaverage
integer(kind=irg)                                 :: devid
integer(kind=irg)                                 :: platid
integer(kind=irg)                                 :: nregions
integer(kind=irg)                                 :: nlines
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
integer(kind=irg)                                 :: nnav
integer(kind=irg)                                 :: nosm
integer(kind=irg)                                 :: maskradius
integer(kind=irg)                                 :: section
character(fnlen)                                  :: exptfile
character(fnlen)                                  :: dictfile
character(fnlen)                                  :: maskfile
character(fnlen)                                  :: indexingmode

! define the IO namelist to facilitate passing variables to the program.
namelist  / EBSDIndexingdata / thetac, delta, numsx, numsy, xpc, ypc, masterfile, devid, platid, &
beamcurrent, dwelltime, binning, gammavalue, energymin, spatialaverage, nregions, nlines, &
scalingmode, maskpattern, energyaverage, L, omega, nthreads, energymax, datafile, angfile, ctffile, &
ncubochoric, numexptsingle, numdictsingle, ipf_ht, ipf_wd, nnk, nnav, exptfile, maskradius, inputtype, &
dictfile, indexingmode, hipassw, stepX, stepY, tmpfile, avctffile, nosm, eulerfile, Notify, maskfile, &
section, HDFstrings

! set the input parameters to default values (except for xtalname, which must be present)
ncubochoric     = 50
numexptsingle   = 1024
numdictsingle   = 1024
platid          = 1
devid           = 1
nregions        = 10
nlines          = 3
nnk             = 50
nnav            = 20
nosm            = 20
exptfile        = 'undefined'
numsx           = 640           ! [dimensionless]
numsy           = 480           ! [dimensionless]
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
maskpattern     = 'n'           ! 'y' or 'n' to include a circular mask
Notify          = 'Off'
scalingmode     = 'not'         ! intensity selector ('lin', 'gam', or 'not')
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


end if



! if we get here, then all appears to be ok, and we need to fill in the enl fields

enl%devid         = devid
enl%platid        = platid
enl%nregions      = nregions
enl%nlines        = nlines
enl%maskpattern   = maskpattern
enl%exptfile      = exptfile
enl%nnk           = nnk
enl%nnav          = nnav
enl%nosm          = nosm
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

if (trim(indexingmode) .eq. 'dynamic') then
    enl%L               = L
    enl%numsx           = numsx
    enl%numsy           = numsy
    enl%binning         = binning
    enl%energyaverage   = energyaverage
    enl%thetac          = thetac
    enl%delta           = delta
    enl%xpc             = xpc
    enl%ypc             = ypc
    enl%gammavalue      = gammavalue
    enl%beamcurrent     = beamcurrent
    enl%dwelltime       = dwelltime
    enl%scalingmode     = scalingmode
    enl%ncubochoric     = ncubochoric
    enl%omega           = omega
    enl%energymin       = energymin
    enl%energymax       = energymax
    enl%spatialaverage  = spatialaverage
    enl%dictfile        = 'undefined'
else if (trim(indexingmode) .eq. 'static') then
    enl%dictfile = dictfile
    enl%ncubochoric = 0
else
    call FatalError('EMEBSDIndexing:',' indexingmode is not known in '//nmlfile)
end if

end subroutine GetEBSDIndexingNameList

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
integer(kind=irg)                                 :: ncubochoric
integer(kind=irg)                                 :: numexptsingle
integer(kind=irg)                                 :: numdictsingle
integer(kind=irg)                                 :: ipf_ht
integer(kind=irg)                                 :: ipf_wd
integer(kind=irg)                                 :: nnk
integer(kind=irg)                                 :: nnav
integer(kind=irg)                                 :: nosm
integer(kind=irg)                                 :: maskradius
character(fnlen)                                  :: exptfile
character(fnlen)                                  :: dictfile
character(fnlen)                                  :: indexingmode

! define the IO namelist to facilitate passing variables to the program.
namelist  / TKDIndexingdata / thetac, delta, numsx, numsy, xpc, ypc, masterfile, devid, platid, &
beamcurrent, dwelltime, binning, gammavalue, energymin, spatialaverage, nregions, &
scalingmode, maskpattern, energyaverage, L, omega, nthreads, energymax, datafile, angfile, ctffile, &
ncubochoric, numexptsingle, numdictsingle, ipf_ht, ipf_wd, nnk, nnav, exptfile, maskradius,&
dictfile, indexingmode, hipassw, stepX, stepY, tmpfile, avctffile, nosm, eulerfile, maskfile

! set the input parameters to default values (except for xtalname, which must be present)
ncubochoric     = 50
numexptsingle   = 1024
numdictsingle   = 1024
platid          = 1
devid           = 1
nregions        = 10
nnk             = 50
nnav            = 20
nosm            = 20
exptfile        = 'undefined'
numsx           = 640           ! [dimensionless]
numsy           = 480           ! [dimensionless]
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
        call FatalError('EMTKDIndexing:',' experimental file name is undefined in '//nmlfile)
    end if


end if



! if we get here, then all appears to be ok, and we need to fill in the enl fields

enl%devid = devid
enl%platid = platid
enl%nregions = nregions
enl%maskpattern = maskpattern
enl%exptfile = exptfile
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
logical,OPTIONAL,INTENT(IN)             :: initonly

logical                                 :: skipread = .FALSE.


	character(fnlen)		:: xtalname
	real(kind=sgl)			:: voltage 
	integer(kind=irg)		:: kk(3) 
	real(kind=sgl)			:: lauec(2) 
	real(kind=sgl)			:: dmin 

! EM or STEM ?
	character(fnlen)		:: progmode
	character(fnlen)		:: STEMnmlfile 
character(fnlen)			:: foilnmlfile 
 
! column approximation parameters and image parameters 
	real(kind=sgl)			:: DF_L 
	real(kind=sgl)			:: DF_npix 
	real(kind=sgl)			:: DF_npiy 
	real(kind=sgl)			:: DF_slice 

	integer(kind=irg)		:: dinfo
	character(fnlen)		:: sgname 

! defect parameters
	integer(kind=irg)		:: numdisl
	integer(kind=irg)		:: numsf
	integer(kind=irg)		:: numinc
	integer(kind=irg)		:: numvoids
	character(fnlen)		:: voidname
	character(fnlen)		:: dislname
	character(fnlen)		:: sfname
	character(fnlen)		:: incname
	character(fnlen)		:: dispfile
	character(fnlen)		:: dispmode

! output parameters
	character(fnlen)		:: dataname
	integer(kind=irg)		:: t_interval

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
numsx = 640
numsy = 480
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
integer(kind=irg)                                 :: nregions
character(2)                                      :: metric



namelist / DPFitdata / masterfile, modalityname, exptfile, rhobeg, rhoend, verbose, mask, &
         phi1, phi, phi2, L, thetac, delta, omega, numsx, numsy, binning, xpc, ypc, beamcurrent, &
         dwelltime, npix, Rin, Rout, thetacone, sampletilt, workingdistance, gammavalue, maskradius, &
         step_xpc, step_ypc, step_L, step_phi1, step_phi, step_phi2, step_thetacone, nrun, nregions, metric

masterfile = 'undefined' 
modalityname = 'undefined' 

exptfile = 'undefined'

rhobeg = 1.0D-2
rhoend = 1.0D-7
verbose = .TRUE.
mask = .TRUE.
phi1 = 0.0
phi = 0.0
phi2 = 0.0

step_phi1 = 2.0
step_phi = 2.0
step_phi2 = 2.0

L = 15000.0
thetac = 10.0
delta = 50.0
omega = 0.0
numsx = 640
numsy = 480
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

nrun = 2
nregions = 8
metric = 'DP'

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
                              beamconvergence, CLarray, geometry

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
!> @brief read namelist file and fill enl structure (used by EMRefineOrientation.f90)
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
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: nthreads
character(fnlen)                                  :: dotproductfile
character(fnlen)                                  :: ctffile
integer(kind=irg)                                 :: nmis
integer(kind=irg)                                 :: niter
real(kind=sgl)                                    :: step


namelist / RefineOrientations / nthreads, dotproductfile, ctffile, nmis, niter, step

nthreads = 1
dotproductfile = 'undefined'
ctffile = 'undefined'
nmis = 1
niter = 1
step = 1.0

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

    if (trim(dotproductfile).eq.'undefined') then
        call FatalError('EMRefineOrientation:',' ctf file name is undefined in '//nmlfile)
    end if


end if

enl%nthreads = nthreads
enl%dotproductfile = dotproductfile
enl%ctffile = ctffile
enl%nmis = nmis
enl%niter = niter
enl%step = step

end subroutine GetRefineOrientationNameList

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
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)                                 :: nthreads
character(fnlen)                                  :: dotproductfile
character(fnlen)                                  :: ctffile
real(kind=sgl)                                    :: step
real(kind=sgl)                                    :: angleaxis(4)

namelist / FitOrientationPS / nthreads, dotproductfile, ctffile, step, angleaxis

nthreads = 1
dotproductfile = 'undefined'
ctffile = 'undefined'
step = 1.0
angleaxis = (/1.0,1.0,1.0,120.0/)

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

    if(NORM2(angleaxis(1:3)) .eq. 0.0) then
        call FatalError('GetFitOrientationPSNameList:','The angle axis for pseudosymmetric variant has norm 0')
    end if

end if

enl%nthreads = nthreads
enl%dotproductfile = dotproductfile
enl%ctffile = ctffile
enl%step = step
enl%angleaxis = angleaxis

end subroutine GetFitOrientationPSNameList


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

write(*,NML=MCCLfoildata)

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
type(EBSDNameListType),INTENT(INOUT)          :: enl
logical,OPTIONAL,INTENT(IN)                   :: initonly

logical                                   :: skipread = .FALSE.

character(fnlen)        :: xtalname
real(kind=dbl)          :: dmin
integer(kind=irg)       :: totnum_el
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
          nthreads, platid, devid,  globalworkgrpsz, eulerconvention, anglefile, datafile, multiplier
! set the input parameters to default values (except for xtalname, anglefile and datafile, which must be present)

xtalname = 'undefined'        ! name of xtal
dmin = 0.04D0                 ! maximum g vector used in computation
totnum_el = 2000000000        ! number of electrons for MC run
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
numsx = 480                   ! number of pixel is x direction of scintillator
numsy = 480                   ! number of pixel is y direction of scintillator
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
end if

enl%MCxtalname = xtalname                     ! name of xtal
enl%dmin = dmin                               ! maximum g vector used in computation
enl%totnum_el = totnum_el                     ! number of electrons for MC run
enl%EkeV = EkeV                               ! incident electron energy [kV]
enl%Ehistmin = Ehistmin                       ! cutoff energy [kV]
enl%Ebinsize = Ebinsize                       ! energy step size [kV]
enl%depthmax = depthmax                       ! depth cutoff [nm]
enl%depthstep = depthstep                     ! depth bin size [nm]
enl%beamcurrent = beamcurrent                 ! [nA]
enl%dwelltime = dwelltime                     ! [micro seconds]
enl%MCsig = sig                               ! sample tilt angle [degrees]
enl%MComega = omega                           ! tilt about RD axis [degrees]
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

! fill other namelist variables in the ebsd namelist; will be used to write the HDF5 files etc.
enl%energyaverage = 0
enl%spatialaverage = 'n'
enl%alphaBD = 0.0
enl%energyfile = 'undefined'
enl%masterfile = 'undefined'
enl%nsx = enl%numsx
enl%nsy = enl%numsy
enl%num_el = 10 ! this is variable in the MCOpenCL program, here we keep it to a fixed value
enl%MCnthreads = enl%nthreads
enl%npx = enl%numsx
enl%npy = enl%numsy
enl%MCmode = 'full'
enl%numEbins =  int((enl%EkeV-enl%Ehistmin)/enl%Ebinsize)+1
enl%numzbins =  int(enl%depthmax/enl%depthstep)+1

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
!DEC$ ATTRIBUTES DLLEXPORT :: GetECPSingleNameList

use error

IMPLICIT NONE

character(fnlen),INTENT(IN)                       :: nmlfile
type(MDElectronPropNameListType),INTENT(INOUT)    :: enl
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
logical,OPTIONAL,INTENT(IN)                       :: initonly

logical                                           :: skipread = .FALSE.

integer(kind=irg)   :: nthreads, atno, npix
real(kind=sgl)      :: voltage, dmin, eu(3), convergence, DWF, QClatparm, thickness
character(fnlen)    :: datafile
character(1)        :: centering

namelist /CBEDQC/ voltage, dmin, nthreads, DWF, atno, thickness, &
          datafile, eu, convergence, QClatparm,centering, npix

datafile    = 'undefined'           ! output filename
voltage     = 200.0                 ! acceleration voltage [kV]
eu          = (/ 0.0, 0.0, 0.0 /)   ! beam direction [direction indices]
dmin        = 0.25                  ! smallest d-spacing to include in dynamical matrix [nm]
convergence = 10.0                  ! beam convergence angle [mRad]
QClatparm   = 0.50                  ! lattice parameter of hyper-cube [nm]
DWF         = 0.0033                ! Debye-Waller factor [nm^-2]
atno        = 12                    ! atomin cnumber
nthreads    = 1                     ! number of threads
centering   = 'P'                   ! hyper-cube lattice centering
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

    if (trim(datafile).eq.'undefined') then
        call FatalError('GetEMgammaNameList:',' output file name is undefined in '//nmlfile)
    end if

end if

enl%datafile              = datafile
enl%voltage               = voltage
enl%eu                    = eu
enl%dmin                  = dmin
enl%nthreads              = nthreads
enl%DWF                   = DWF
enl%atno                  = atno
enl%QClatparm             = QClatparm
enl%convergence           = convergence
enl%centering             = centering
enl%thickness             = thickness
enl%npix                  = npix

end subroutine GetEMCBEDQCNameList

end module NameListHandlers