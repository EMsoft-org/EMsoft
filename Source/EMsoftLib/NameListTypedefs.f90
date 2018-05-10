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
! EMsoft:NameListTypedefs.f90
!--------------------------------------------------------------------------
!
! PROGRAM: NameListTypedefs
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief collection of namelist type declarations
!
!> @date 06/13/14 MDG 1.0 initial version
!> @date 05/05/15 MDG 1.1 removed primelist variable from name list files
!> @date 11/24/15 MDG 1.2 significant changes to ECCINameListType
!--------------------------------------------------------------------------
module NameListTypedefs

use local

IMPLICIT NONE

! namelist for EMGBO program
type GBONameListType
        integer(kind=irg)       :: pgnum
        integer(kind=irg)       :: numsamples
        integer(kind=irg)       :: numbins
        integer(kind=irg)       :: nthreads
        character(3)            :: CSLtype
        logical                 :: fixedAB
        character(fnlen)        :: outname
end type GBONameListType

! namelist for EMoSLERP program
type oSLERPNameListType
        integer(kind=irg)       :: framesize
        real(kind=dbl)          :: qm(4)
        real(kind=dbl)          :: mA(3)
        real(kind=dbl)          :: mC(3)
        real(kind=dbl)          :: o1(8)
        real(kind=dbl)          :: o2(8)
        real(kind=dbl)          :: dOmega
        character(fnlen)        :: rendermode
        character(fnlen)        :: GBmode
        character(fnlen)        :: xtalname 
        character(fnlen)        :: povrayfile
        character(fnlen)        :: framefolder
        character(fnlen)        :: moviename
end type oSLERPNameListType

! namelist for the EMLorentz program
type LorentzNameListType
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
end type LorentzNameListType

! namelist for EMmultiphases program
type MultiPhaseNameListType
        integer(kind=irg)       :: nthreads
        character(fnlen)        :: dp1file
        character(fnlen)        :: dp2file
        character(fnlen)        :: dp3file
        character(fnlen)        :: outputfile
end type MultiPhaseNameListType

! namelist for the EMKossel program
type KosselNameListType
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
end type KosselNameListType

! namelist for the EMKosselmaster program
type KosselMasterNameListType
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
end type KosselMasterNameListType

! namelist for the EMMC program
type MCNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: numsx
        integer(kind=irg)       :: primeseed
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: totnum_el
        integer(kind=irg)       :: nthreads
        real(kind=dbl)          :: sig
        real(kind=dbl)          :: omega
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin
        real(kind=dbl)          :: Ebinsize
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        character(4)            :: MCmode
        character(fnlen)        :: mode
        character(fnlen)        :: xtalname
        character(fnlen)        :: dataname
end type MCNameListType

! namelist for the EMOMmaster program
type OMmasterNameListType
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: nthreads
        real(kind=sgl)          :: eps1Re
        real(kind=sgl)          :: eps1Im
        real(kind=sgl)          :: eps2Re
        real(kind=sgl)          :: eps2Im
        real(kind=sgl)          :: wl
        real(kind=sgl)          :: theta 
        logical                 :: normalize
        character(3)            :: Notify
        character(fnlen)        :: xtalname
        character(fnlen)        :: masterfile
end type OMmasterNameListType

! namelist for the EMOMmaster program
type OMNameListType
        integer(kind=irg)       :: phinum
        integer(kind=irg)       :: numpx
        integer(kind=irg)       :: numpy
        character(fnlen)        :: masterfile
        character(fnlen)        :: anglefile
        character(fnlen)        :: outputfile
        character(fnlen)        :: tiffprefix
end type OMNameListType


! namelist for the EMMCLIPSS program ! PGC added 12/01/15
type MCLIPSSNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: numsx
        integer(kind=irg)       :: primeseed
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: nthreads
        real(kind=dbl)          :: sig
        real(kind=dbl)          :: omega
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin
        real(kind=dbl)          :: Ebinsize
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        real(kind=dbl)          :: lipssamp
        real(kind=dbl)          :: lipsswave
        real(kind=dbl)          :: scaled
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: vis
        character(4)            :: MCmode
        character(fnlen)        :: xtalname
        character(fnlen)        :: dataname
end type MCLIPSSNameListType

! namelist for the EMreflectors program
type reflectorNameListType
        integer(kind=irg)       :: numphi
        integer(kind=irg)       :: numtheta
        real(kind=sgl)          :: dmin
        character(fnlen)        :: masterfile
        character(fnlen)        :: energyfile
end type reflectorNameListType

! namelist for the EMreflectors program
type kinematicalNameListType
        real(kind=sgl)          :: dmin
        real(kind=sgl)          :: thr
        real(kind=sgl)          :: voltage 
        character(fnlen)        :: xtalname
        character(fnlen)        :: datafile
end type kinematicalNameListType

type OrientationVizNameListType
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
end type OrientationVizNameListType 

type ConvertOrientationsNameListType
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
end type ConvertOrientationsNameListType

! namelist for the EMMCOpenCL program
type MCCLNameListType
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
        real(kind=dbl)          :: thickness
        real(kind=dbl)          :: radius
        real(kind=dbl)          :: incloc
        character(3)            :: Notify
        character(4)            :: MCmode
        character(fnlen)        :: xtalname
        character(fnlen)        :: dataname
        character(fnlen)        :: mode
end type MCCLNameListType

! namelist for the MCCLMultiLayer program

type MCCLMultiLayerNameListType
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
end type MCCLMultiLayerNameListType

! namelist for the EMEBSDmaster program
type EBSDMasterNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: Esel
        integer(kind=irg)       :: nthreads
        real(kind=sgl)          :: dmin
        character(3)            :: Notify
        character(fnlen)        :: copyfromenergyfile
        character(fnlen)        :: energyfile
        character(fnlen)        :: outname
        logical                 :: combinesites
        logical                 :: restart
        logical                 :: uniform
end type EBSDMasterNameListType

! namelist for the EMTKDmaster program
type TKDMasterNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: Esel
        integer(kind=irg)       :: nthreads
        real(kind=sgl)          :: dmin
        character(fnlen)        :: energyfile
        character(fnlen)        :: outname
        logical                 :: combinesites
        logical                 :: restart
        logical                 :: uniform
end type TKDMasterNameListType

! namelist for the EMEBSDmasterOpenCL program
type EBSDMasterOpenCLNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: Esel
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: devid
        integer(kind=irg)       :: globalworkgrpsz
        real(kind=sgl)          :: dmin
        character(fnlen)        :: energyfile
        character(fnlen)        :: outname
        logical                 :: restart
        logical                 :: uniform
end type EBSDMasterOpenCLNameListType

! namelist for the EMEBSD program
! note that not all of these are actually entered via a namelist file
! some of them are used to facilitate passing of subroutine arguments in EBSDmod.f90
type EBSDNameListType
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
        real(kind=sgl)          :: omega
        real(kind=sgl)          :: xpc
        real(kind=sgl)          :: ypc
        real(kind=sgl)          :: energymin
        real(kind=sgl)          :: energymax
        real(kind=sgl)          :: gammavalue
        real(kind=sgl)          :: axisangle(4)
        real(kind=sgl)          :: alphaBD
        real(kind=sgl)          :: hipassw
        real(kind=dbl)          :: Ftensor(3,3)
        real(kind=dbl)          :: beamcurrent
        real(kind=dbl)          :: dwelltime
        character(1)            :: makedictionary
        character(1)            :: poisson
        character(1)            :: includebackground
        character(1)            :: applyDeformation
        character(1)            :: maskpattern
        character(3)            :: scalingmode
        character(3)            :: eulerconvention
        character(3)            :: outputformat
        character(1)            :: spatialaverage
        character(5)            :: bitdepth
        character(fnlen)        :: anglefile
        character(fnlen)        :: anglefiletype
        character(fnlen)        :: masterfile
        character(fnlen)        :: energyfile 
        character(fnlen)        :: datafile
end type EBSDNameListType

! namelist for the EMEBSDFull program
type EBSDFullNameListType
        real(kind=dbl)          :: dmin
        integer(kind=irg)       :: totnum_el
        integer(kind=irg)       :: multiplier
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
        real(kind=sgl)          :: thetac
        real(kind=sgl)          :: delta
        integer(kind=irg)       :: numsx
        integer(kind=irg)       :: numsy
        real(kind=sgl)          :: xpc
        real(kind=sgl)          :: ypc
        integer(kind=irg)       :: binning
        character(3)            :: scalingmode
        real(kind=sgl)          :: gammavalue
        character(1)            :: maskpattern
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: devid
        integer(kind=irg)       :: globalworkgrpsz
        integer(kind=irg)       :: num_el
        character(3)            :: eulerconvention
        character(fnlen)        :: anglefile
        character(fnlen)        :: datafile
        character(fnlen)        :: xtalname 
end type EBSDFullNameListType

! namelist for the EMTKD program
! note that not all of these are actually entered via a namelist file
! some of them are used to facilitate passing of subroutine arguments in EBSDmod.f90
type TKDNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: numsx
        integer(kind=irg)       :: numsy
        integer(kind=irg)       :: binning
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: energyaverage
        real(kind=sgl)          :: L
        real(kind=sgl)          :: thetac
        real(kind=sgl)          :: delta
        real(kind=sgl)          :: omega
        real(kind=sgl)          :: xpc
        real(kind=sgl)          :: ypc
        real(kind=sgl)          :: energymin
        real(kind=sgl)          :: energymax
        real(kind=sgl)          :: gammavalue
        real(kind=sgl)          :: axisangle(4)
        real(kind=sgl)          :: alphaBD
        real(kind=dbl)          :: beamcurrent
        real(kind=dbl)          :: dwelltime
        character(1)            :: maskpattern
        character(3)            :: scalingmode
        character(3)            :: eulerconvention
        character(3)            :: outputformat
        character(1)            :: spatialaverage
        character(fnlen)        :: anglefile
        character(fnlen)        :: masterfile
        character(fnlen)        :: energyfile 
        character(fnlen)        :: datafile
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numangles
        integer(kind=irg)       :: numEbins
        integer(kind=irg)       :: numzbins 
        integer(kind=irg)       :: nsx
        integer(kind=irg)       :: nsy
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: MCnthreads
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        integer(kind=irg)       :: numset
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin 
        real(kind=dbl)          :: Ebinsize 
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        real(kind=dbl)          :: MCsig
        real(kind=dbl)          :: MComega
        character(4)            :: MCmode       ! Monte Carlo mode
        character(5)            :: anglemode    ! 'quats' or 'euler' for angular input
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: MCscversion
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: Masterenergyfile
        character(fnlen)        :: MCprogname 
        character(fnlen)        :: MCxtalname
        real(kind=dbl)          :: dmin
        integer(kind=irg)       :: totnum_el
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: devid
        integer(kind=irg)       :: globalworkgrpsz
        integer(kind=irg)       :: multiplier

end type TKDNameListType

! namelist for the EMEBSDoverlap program
! note that not all of these are actually entered via a namelist file
! some of them are used to facilitate passing of subroutine arguments in EBSDmod.f90
type EBSDoverlapNameListType
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
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numset
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: masterfile
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: xtalnameA
        character(fnlen)        :: xtalnameB
        character(fnlen)        :: Masterenergyfile
end type EBSDoverlapNameListType

! namelist for the EMTKDoverlap program
! note that not all of these are actually entered via a namelist file
! some of them are used to facilitate passing of subroutine arguments in EBSDmod.f90
type TKDoverlapNameListType
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
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numset
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: masterfile
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: xtalnameA
        character(fnlen)        :: xtalnameB
        character(fnlen)        :: Masterenergyfile
end type TKDoverlapNameListType

! TKDspots name list type
type TKDspotsNameListType
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
        real(kind=sgl)          :: sig
        real(kind=sgl)          :: omega
        real(kind=sgl)          :: xpc
        real(kind=sgl)          :: ypc
        character(fnlen)        :: xtalname
        character(fnlen)        :: outname
        character(fnlen)        :: eulerfile 
end type 

! EMEBSDcluster name list
type EBSDclusterNameListType
        integer(kind=irg)       :: NClusters
        integer(kind=irg)       :: NIterations
        integer(kind=irg)       :: NScanColumns
        integer(kind=irg)       :: NScanRows
        integer(kind=irg)       :: binfactor
        character(fnlen)        :: inputfilename
        character(fnlen)        :: groupname
        character(fnlen)        :: datasetname
end type EBSDclusterNameListType


! ECP structure; note that cell distortions are disabled for now
type ECPNameListType
        integer(kind=irg)       :: stdout
        integer(kind=irg)       :: fn_f(3)
        integer(kind=irg)       :: fn_s(3)
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: npix
        integer(kind=irg)       :: gF(3)
        integer(kind=irg)       :: gS(3)
        integer(kind=irg)       :: tF(3)
        integer(kind=irg)       :: tS(3)
        real(kind=sgl)          :: thetac
        real(kind=sgl)          :: filmthickness
        character(1)            :: maskpattern
        character(fnlen)        :: xtalname
        character(fnlen)        :: xtalname2
        character(fnlen)        :: energyfile
        character(fnlen)        :: filmfile
        character(fnlen)        :: subsfile
        character(fnlen)        :: masterfile
        character(fnlen)        :: datafile
        character(fnlen)        :: anglefile
        character(3)            :: eulerconvention
        real(kind=sgl)          :: gammavalue
        character(3)            :: outputformat
        real(kind=dbl)          :: sampletilt
        real(kind=sgl)          :: workingdistance
        real(kind=sgl)          :: Rin
        real(kind=sgl)          :: Rout
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numangle
        integer(kind=irg)       :: numangle_anglefile
        integer(kind=irg)       :: numEbins
        integer(kind=irg)       :: numzbins 
        integer(kind=irg)       :: nsx
        integer(kind=irg)       :: nsy
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: MCnthreads
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        integer(kind=irg)       :: numset
        integer(kind=irg)       :: npolar
        integer(kind=irg)       :: nazimuth
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin 
        real(kind=dbl)          :: Ebinsize 
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        real(kind=sgl)          :: dmin
        real(kind=dbl)          :: MCsigstart
        real(kind=dbl)          :: MCsigend
        real(kind=dbl)          :: MCsigstep
        real(kind=dbl)          :: MComega
        character(4)            :: MCmode       ! Monte Carlo mode
        character(5)            :: anglemode    ! 'quats' or 'euler' for angular input
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: MCscversion
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: Masterenergyfile
        character(fnlen)        :: MCprogname 
        character(fnlen)        :: MCxtalname
end type ECPNameListType


! LACBED structure
type LACBEDNameListType
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
end type LACBEDNameListType

! namelist for the EMECPmaster program
type ECPMasterNameListType
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
end type ECPMasterNameListType

! namelist for the EMECPQCmaster program
type ECPQCMasterNameListType
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
end type ECPQCMasterNameListType

! namelist for the EMEBSDQCmaster program
type EBSDQCMasterNameListType
    integer(kind=irg)       :: nsamples
    integer(kind=irg)       :: npx
    integer(kind=irg)       :: nthreads
    integer(kind=irg)       :: atno
    real(kind=sgl)          :: DWF
    real(kind=sgl)          :: dmin
    real(kind=sgl)          :: QClatparm 
    character(1)            :: centering
    character(fnlen)        :: energyfile
! parameters from MC simulation
    real(kind=sgl)          :: depthstep 
    real(kind=sgl)          :: Ehistmin
    real(kind=sgl)          :: Ebinsize
    real(kind=sgl)          :: Ekev
    real(kind=sgl)          :: depthmax
    integer(kind=irg)       :: numsx
    integer(kind=irg)       :: num_el
    integer(kind=irg)       :: numEbins
    integer(kind=irg)       :: numzbins
    integer(kind=irg)       :: multiplier
    real(kind=sgl)          :: sig
    character(4)            :: mode
end type EBSDQCMasterNameListType

! namelist for the EMEBSDQCmaster program
type EBSD2DQCMasterNameListType
    integer(kind=irg)       :: npx
    integer(kind=irg)       :: nthreads
    integer(kind=irg)       :: atno
    real(kind=sgl)          :: DWF
    real(kind=sgl)          :: dmin
    real(kind=sgl)          :: QClatparm_a
    real(kind=sgl)          :: QClatparm_c 
    character(1)            :: centering
    character(fnlen)        :: energyfile
    character(3)            :: QCtype
! parameters from MC simulation
    real(kind=sgl)          :: depthstep 
    real(kind=sgl)          :: Ehistmin
    real(kind=sgl)          :: Ebinsize
    real(kind=sgl)          :: Ekev
    real(kind=sgl)          :: depthmax
    integer(kind=irg)       :: numsx
    integer(kind=irg)       :: num_el
    integer(kind=irg)       :: numEbins
    integer(kind=irg)       :: numzbins
    integer(kind=irg)       :: multiplier
    real(kind=sgl)          :: sig
    character(4)            :: mode
end type EBSD2DQCMasterNameListType

!namelist for the EMECP program
type ECPpatternNameListType
    integer(kind=irg)       :: stdout
    integer(kind=irg)       :: npix
    real(kind=sgl)          :: thetac
    real(kind=sgl)          :: k(3)
    character(fnlen)        :: masterfile
    character(fnlen)        :: outname
end type ECPpatternNameListType

!namelist for the EMECPZA program
type ECPZANameListType
    integer(kind=irg)       :: fn(3)
    integer(kind=irg)       :: k(3)
    integer(kind=irg)       :: npix
    integer(kind=irg)       :: nthreads
    real(kind=sgl)          :: dmin
    real(kind=sgl)          :: ktmax
    character(1)            :: maskpattern
    character(fnlen)        :: energyfile 
    character(fnlen)        :: outname
end type ECPZANameListType

!namelist for the EMPED program
type PEDZANameListType
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
end type PEDZANameListType

!namelist for the EMPEDkin program
type PEDkinNameListType
    integer(kind=irg)       :: npix
    integer(kind=irg)       :: ncubochoric
    integer(kind=irg)       :: nthreads
    real(kind=sgl)          :: voltage
    real(kind=sgl)          :: dmin
    real(kind=sgl)          :: thickness
    real(kind=sgl)          :: rnmpp
    character(fnlen)        :: xtalname
    character(fnlen)        :: outname
    character(4)            :: sampling
    character(fnlen)        :: eulerfile
end type PEDkinNameListType

! namelist for the EMECCI program
type ECCINameListType
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
end type ECCINameListType


! namelist for the EMsampleRFZ program
type RFZNameListType
    integer(kind=irg)       :: pgnum
    integer(kind=irg)       :: nsteps
    integer(kind=irg)       :: gridtype
    real(kind=dbl)          :: rodrigues(4)
    real(kind=dbl)          :: maxmisor
    real(kind=dbl)          :: conevector(3)
    real(kind=dbl)          :: semiconeangle
    character(fnlen)        :: xtalname
    character(fnlen)        :: samplemode
    character(fnlen)        :: euoutname
    character(fnlen)        :: cuoutname
    character(fnlen)        :: hooutname
    character(fnlen)        :: rooutname
    character(fnlen)        :: quoutname
    character(fnlen)        :: omoutname
    character(fnlen)        :: axoutname
end type RFZNameListType

type DictIndxOpenCLListType
    integer(kind=irg)           :: numexptsingle
    integer(kind=irg)           :: numdictsingle
    integer(kind=irg)           :: totnumexpt
    integer(kind=irg)           :: totnumdict
    integer(kind=irg)           :: imght
    integer(kind=irg)           :: imgwd
    integer(kind=irg)           :: nnk
    character(fnlen)            :: exptfile
    character(fnlen)            :: dictfile
    character(fnlen)            :: eulerfile
    logical                     :: MeanSubtraction
    logical                     :: patternflip
end type DictIndxOpenCLListType


type PEDKINIndxListType
    integer(kind=irg)           :: npix
    integer(kind=irg)           :: ncubochoric
    real(kind=sgl)              :: voltage
    real(kind=sgl)              :: dmin
    real(kind=sgl)              :: thickness
    real(kind=sgl)              :: rnmpp ! reciprocal nanometers per pixel
    character(fnlen)            :: xtalname
    integer(kind=irg)           :: numexptsingle
    integer(kind=irg)           :: numdictsingle
    integer(kind=irg)           :: ipf_ht
    integer(kind=irg)           :: ipf_wd
    integer(kind=irg)           :: nnk
    real(kind=sgl)              :: sgmax ! maximum sg value for a beam to be considered
    real(kind=sgl)              :: ww ! 2*ww+1 is the size of the spot
    real(kind=sgl)              :: var ! variance of gaussian peak
    character(fnlen)            :: exptfile
    character(fnlen)            :: datafile
    character(fnlen)            :: ctffile
    character(fnlen)            :: tmpfile
    integer(kind=irg)           :: devid
    integer(kind=irg)           :: platid
    integer(kind=irg)           :: nthreads
! all variables below are used to pass values to the subroutine and not read from the namelist file
    real(kind=sgl)              :: Igmax

end type PEDKINIndxListType

type DisorientationsNameListType
        integer(kind=irg)       :: pgnum
        integer(kind=irg)       :: pgnum2
        character(fnlen)        :: inputfile
        character(fnlen)        :: outputfile
end type DisorientationsNameListType

type AverageOrientationNameListType
        integer(kind=irg)       :: nmuse
        integer(kind=irg)       :: reldisx
        integer(kind=irg)       :: reldisy
        character(1)            :: refined
        character(fnlen)        :: dotproductfile
        character(fnlen)        :: averagectffile
        character(fnlen)        :: averagetxtfile
        character(fnlen)        :: disorientationmap
end type AverageOrientationNameListType

type OrientationSimilarityNameListType
        integer(kind=irg)       :: nmuse
        character(fnlen)        :: dotproductfile
        character(fnlen)        :: osmtiff
end type OrientationSimilarityNameListType

type KAMNameListType
        real(kind=sgl)          :: kamcutoff
        integer(kind=irg)       :: orav
        character(fnlen)        :: dotproductfile
        character(fnlen)        :: kamtiff
end type KAMNameListType

type DvsDNameListType
        integer(kind=irg)       :: nmuse
        real(kind=sgl)          :: maxdis
        real(kind=sgl)          :: minang
        real(kind=sgl)          :: maxang
        character(fnlen)        :: dotproductfile
        character(fnlen)        :: outfile
        character(fnlen)        :: povfile
        character(fnlen)        :: xtalfile
end type DvsDNameListType

type EBSDDIpreviewNameListType
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
        real(kind=sgl)          :: hipasswmax
        character(fnlen)        :: tifffile
        character(fnlen)        :: exptfile
        character(fnlen)        :: inputtype
        character(fnlen)        :: HDFstrings(10)
end type EBSDDIpreviewNameListType 

type EBSDIndexingNameListType
        integer(kind=irg)       :: ncubochoric
        integer(kind=irg)       :: numexptsingle
        integer(kind=irg)       :: numdictsingle
        integer(kind=irg)       :: ipf_ht
        integer(kind=irg)       :: ipf_wd 
        integer(kind=irg)       :: ROI(4)
        integer(kind=irg)       :: nnk
        integer(kind=irg)       :: nnav
        integer(kind=irg)       :: nosm
        integer(kind=irg)       :: maskradius
        character(fnlen)        :: exptfile 
        integer(kind=irg)       :: numsx
        integer(kind=irg)       :: numsy
        integer(kind=irg)       :: binning
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: energyaverage
        integer(kind=irg)       :: devid
        integer(kind=irg)       :: usenumd
        integer(kind=irg)       :: multidevid(8)
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: nregions
        integer(kind=irg)       :: nlines
        real(kind=sgl)          :: L
        real(kind=sgl)          :: thetac
        real(kind=sgl)          :: delta
        real(kind=sgl)          :: omega
        real(kind=sgl)          :: xpc
        real(kind=sgl)          :: ypc
        real(kind=sgl)          :: energymin
        real(kind=sgl)          :: energymax
        real(kind=sgl)          :: gammavalue
        real(kind=sgl)          :: axisangle(4)
        real(kind=dbl)          :: beamcurrent
        real(kind=dbl)          :: dwelltime
        real(kind=dbl)          :: hipassw
        character(1)            :: maskpattern
        character(3)            :: scalingmode
        character(3)            :: Notify
        !character(3)            :: eulerconvention
        !character(3)            :: outputformat
        character(1)            :: keeptmpfile
        character(1)            :: spatialaverage
        character(fnlen)        :: anglefile
        !character(fnlen)        :: dotproductfile
        character(fnlen)        :: masterfile
        character(fnlen)        :: energyfile 
        character(fnlen)        :: datafile
        character(fnlen)        :: tmpfile
        character(fnlen)        :: ctffile
        character(fnlen)        :: avctffile
        character(fnlen)        :: angfile
        character(fnlen)        :: eulerfile
        character(fnlen)        :: dictfile
        character(fnlen)        :: maskfile
        character(fnlen)        :: indexingmode
        character(fnlen)        :: inputtype
        character(fnlen)        :: HDFstrings(10)
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numangles
        integer(kind=irg)       :: numEbins
        integer(kind=irg)       :: numzbins 
        integer(kind=irg)       :: nsx
        integer(kind=irg)       :: nsy
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: MCnthreads
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        integer(kind=irg)       :: numset
        integer(kind=irg)       :: section
        integer(kind=irg)       :: multiplier
        integer(kind=irg)       :: totnum_el
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin 
        real(kind=dbl)          :: Ebinsize 
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        real(kind=dbl)          :: MCsig
        real(kind=dbl)          :: MComega
        real(kind=sgl)          :: dmin
        real(kind=sgl)          :: StepX
        real(kind=sgl)          :: StepY
        real(kind=sgl)          :: WD
        character(4)            :: MCmode       ! Monte Carlo mode
        character(5)            :: anglemode    ! 'quats' or 'euler' for angular input
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: MCscversion
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: Masterenergyfile
        character(fnlen)        :: MCprogname 
        character(fnlen)        :: MCxtalname
end type EBSDIndexingNameListType

type ADPNameListType
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
end type ADPNameListType

type TKDIndexingNameListType
        integer(kind=irg)       :: ncubochoric
        integer(kind=irg)       :: numexptsingle
        integer(kind=irg)       :: numdictsingle
        integer(kind=irg)       :: ipf_ht
        integer(kind=irg)       :: ipf_wd 
        integer(kind=irg)       :: nnk
        integer(kind=irg)       :: nnav
        integer(kind=irg)       :: nosm
        integer(kind=irg)       :: maskradius
        character(fnlen)        :: exptfile 
        integer(kind=irg)       :: numsx
        integer(kind=irg)       :: numsy
        integer(kind=irg)       :: binning
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: energyaverage
        integer(kind=irg)       :: devid
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: nregions
        real(kind=sgl)          :: L
        real(kind=sgl)          :: thetac
        real(kind=sgl)          :: delta
        real(kind=sgl)          :: omega
        real(kind=sgl)          :: xpc
        real(kind=sgl)          :: ypc
        real(kind=sgl)          :: energymin
        real(kind=sgl)          :: energymax
        real(kind=sgl)          :: gammavalue
        real(kind=sgl)          :: axisangle(4)
        real(kind=dbl)          :: beamcurrent
        real(kind=dbl)          :: dwelltime
        real(kind=dbl)          :: hipassw
        character(1)            :: maskpattern
        character(3)            :: scalingmode
        !character(3)            :: eulerconvention
        !character(3)            :: outputformat
        character(1)            :: spatialaverage
        character(fnlen)        :: anglefile
        !character(fnlen)        :: dotproductfile
        character(fnlen)        :: maskfile
        character(fnlen)        :: masterfile
        character(fnlen)        :: energyfile 
        character(fnlen)        :: datafile
        character(fnlen)        :: tmpfile
        character(fnlen)        :: ctffile
        character(fnlen)        :: avctffile
        character(fnlen)        :: angfile
        character(fnlen)        :: eulerfile
        character(fnlen)        :: dictfile
        character(fnlen)        :: indexingmode
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numangles
        integer(kind=irg)       :: numEbins
        integer(kind=irg)       :: numzbins 
        integer(kind=irg)       :: nsx
        integer(kind=irg)       :: nsy
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: MCnthreads
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        integer(kind=irg)       :: numset
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin 
        real(kind=dbl)          :: Ebinsize 
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        real(kind=dbl)          :: MCsig
        real(kind=dbl)          :: MComega
        real(kind=sgl)          :: dmin
        real(kind=sgl)          :: StepX
        real(kind=sgl)          :: StepY
        real(kind=sgl)          :: WD
        character(4)            :: MCmode       ! Monte Carlo mode
        character(5)            :: anglemode    ! 'quats' or 'euler' for angular input
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: MCscversion
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: Masterenergyfile
        character(fnlen)        :: MCprogname 
        character(fnlen)        :: MCxtalname
end type TKDIndexingNameListType

type ECPIndexingNameListType

        integer(kind=irg)       :: ncubochoric
        integer(kind=irg)       :: numexptsingle
        integer(kind=irg)       :: numdictsingle
        integer(kind=irg)       :: totnumexpt
        integer(kind=irg)       :: maskradius
        integer(kind=irg)       :: nnk
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: devid
        integer(kind=irg)       :: nregions
        character(fnlen)        :: exptfile 
        integer(kind=irg)       :: fn_f(3)
        integer(kind=irg)       :: fn_s(3)
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: npix
        integer(kind=irg)       :: gF(3)
        integer(kind=irg)       :: gS(3)
        integer(kind=irg)       :: tF(3)
        integer(kind=irg)       :: tS(3)
        real(kind=sgl)          :: thetac
        real(kind=sgl)          :: filmthickness
        character(1)            :: maskpattern
        character(fnlen)        :: xtalname
        character(fnlen)        :: xtalname2
        character(fnlen)        :: energyfile
        character(fnlen)        :: filmfile
        character(fnlen)        :: subsfile
        character(fnlen)        :: masterfile
        character(fnlen)        :: datafile
        character(fnlen)        :: tmpfile
        character(fnlen)        :: ctffile
        character(fnlen)        :: anglefile
        character(3)            :: eulerconvention
        real(kind=sgl)          :: gammavalue
        character(3)            :: outputformat
        real(kind=dbl)          :: sampletilt
        real(kind=sgl)          :: workingdistance
        real(kind=sgl)          :: Rin
        real(kind=sgl)          :: Rout
! everything below here is not part of the namelist input structure, but is used to pass arguments to subroutines
        integer(kind=irg)       :: numangle
        integer(kind=irg)       :: numangle_anglefile
        integer(kind=irg)       :: numEbins
        integer(kind=irg)       :: numzbins 
        integer(kind=irg)       :: nsx
        integer(kind=irg)       :: nsy
        integer(kind=irg)       :: num_el
        integer(kind=irg)       :: MCnthreads
        integer(kind=irg)       :: npx
        integer(kind=irg)       :: npy
        integer(kind=irg)       :: nE
        integer(kind=irg)       :: numset
        integer(kind=irg)       :: npolar
        integer(kind=irg)       :: nazimuth
        real(kind=dbl)          :: EkeV
        real(kind=dbl)          :: Ehistmin 
        real(kind=dbl)          :: Ebinsize 
        real(kind=dbl)          :: depthmax
        real(kind=dbl)          :: depthstep
        real(kind=sgl)          :: dmin
        real(kind=dbl)          :: MCsigstart
        real(kind=dbl)          :: MCsigend
        real(kind=dbl)          :: MCsigstep
        real(kind=dbl)          :: MComega
        character(4)            :: MCmode       ! Monte Carlo mode
        character(5)            :: anglemode    ! 'quats' or 'euler' for angular input
        character(6)            :: sqorhe       ! from Master file, square or hexagonal Lambert projection
        character(8)            :: MCscversion
        character(8)            :: Masterscversion
        character(fnlen)        :: Masterprogname
        character(fnlen)        :: Masterxtalname 
        character(fnlen)        :: Masterenergyfile
        character(fnlen)        :: MCprogname 
        character(fnlen)        :: MCxtalname

end type ECPIndexingNameListType

type ZAdefectnameListType
 
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
	character(fnlen)		::dislname
	character(fnlen)		::sfname
	character(fnlen)		::incname
	character(fnlen)		::dispfile
	character(fnlen)		::dispmode

! output parameters
	character(fnlen)		:: dataname
	integer(kind=irg)		:: t_interval


end type ZAdefectnameListType

type EMDPFitListType

    character(fnlen)               :: modalityname
    character(fnlen)               :: masterfile
    character(fnlen)               :: exptfile
    real(kind=dbl)                 :: rhobeg
    real(kind=dbl)                 :: rhoend
    logical                        :: verbose
    logical                        :: mask
    real(kind=irg)                 :: maskradius
    real(kind=sgl)                 :: gammavalue
    real(kind=sgl)                 :: phi, phi1, phi2
    real(kind=sgl)                 :: L
    real(kind=sgl)                 :: thetac
    real(kind=sgl)                 :: delta
    real(kind=sgl)                 :: omega
    integer(kind=irg)              :: numsx
    integer(kind=irg)              :: numsy
    integer(kind=irg)              :: binning
    real(kind=sgl)                 :: xpc
    real(kind=sgl)                 :: ypc
    real(kind=sgl)                 :: beamcurrent
    real(kind=sgl)                 :: dwelltime
    integer(kind=irg)              :: npix
    real(kind=sgl)                 :: Rin
    real(kind=sgl)                 :: Rout
    real(kind=sgl)                 :: thetacone
    real(kind=sgl)                 :: sampletilt
    real(kind=sgl)                 :: workingdistance
    real(kind=sgl)                 :: step_xpc
    real(kind=sgl)                 :: step_ypc
    real(kind=sgl)                 :: step_L
    real(kind=sgl)                 :: step_phi1
    real(kind=sgl)                 :: step_phi
    real(kind=sgl)                 :: step_phi2
    real(kind=sgl)                 :: step_thetacone
    integer(kind=irg)              :: nrun
    integer(kind=irg)              :: nregions
    character(2)                   :: metric
    
end type EMDPFitListType


type EMDPFit4ListType

    character(fnlen)               :: modalityname
    character(fnlen)               :: masterfile
    character(fnlen)               :: exptfile_pat1
    character(fnlen)               :: exptfile_pat2
    character(fnlen)               :: exptfile_pat3
    character(fnlen)               :: exptfile_pat4
    real(kind=dbl)                 :: rhobeg
    real(kind=dbl)                 :: rhoend
    logical                        :: verbose
    logical                        :: mask
    real(kind=irg)                 :: maskradius
    real(kind=sgl)                 :: gammavalue
    real(kind=sgl)                 :: phi_pat1, phi1_pat1, phi2_pat1
    real(kind=sgl)                 :: phi_pat2, phi1_pat2, phi2_pat2
    real(kind=sgl)                 :: phi_pat3, phi1_pat3, phi2_pat3
    real(kind=sgl)                 :: phi_pat4, phi1_pat4, phi2_pat4
    real(kind=sgl)                 :: L
    real(kind=sgl)                 :: thetac
    real(kind=sgl)                 :: delta
    real(kind=sgl)                 :: omega
    integer(kind=irg)              :: numsx
    integer(kind=irg)              :: numsy
    integer(kind=irg)              :: binning
    real(kind=sgl)                 :: xpc
    real(kind=sgl)                 :: ypc
    real(kind=sgl)                 :: beamcurrent
    real(kind=sgl)                 :: dwelltime
    integer(kind=irg)              :: npix
    real(kind=sgl)                 :: Rin
    real(kind=sgl)                 :: Rout
    real(kind=sgl)                 :: thetacone
    real(kind=sgl)                 :: sampletilt
    real(kind=sgl)                 :: workingdistance
    real(kind=sgl)                 :: step_xpc
    real(kind=sgl)                 :: step_ypc
    real(kind=sgl)                 :: step_L
    real(kind=sgl)                 :: step_phi1
    real(kind=sgl)                 :: step_phi
    real(kind=sgl)                 :: step_phi2
    real(kind=sgl)                 :: step_thetacone
    integer(kind=irg)              :: nrun
    integer(kind=irg)              :: pixx_pat1
    integer(kind=irg)              :: pixy_pat1
    integer(kind=irg)              :: pixx_pat2
    integer(kind=irg)              :: pixy_pat2
    integer(kind=irg)              :: pixx_pat3
    integer(kind=irg)              :: pixy_pat3
    integer(kind=irg)              :: pixx_pat4
    integer(kind=irg)              :: pixy_pat4
    real(kind=sgl)                 :: stepx
    real(kind=sgl)                 :: stepy
    integer(kind=irg)              :: nregions
    character(2)                   :: metric

end type EMDPFit4ListType

! ECP structure; note that cell distortions are disabled for now
type ECPSingleNameListType
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: npix
        real(kind=sgl)          :: thetac
        character(1)            :: maskpattern
        character(fnlen)        :: xtalname
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

end type ECPSingleNameListType

! STEM DCI type !PGC 11/02/16
type STEMDCINameListType
    integer(kind=irg)     :: nthreads
    integer(kind=irg)     :: kk(3)
    integer(kind=irg)     :: output
    integer(kind=irg)     :: dinfo
    integer(kind=irg)     :: t_interval
    integer(kind=irg)     :: DF_npix
    integer(kind=irg)     :: DF_npiy
    real(kind=sgl)        :: voltage
    real(kind=sgl)        :: lauec(2)
    real(kind=sgl)        :: DF_L
    real(kind=sgl)        :: DF_slice
    real(kind=sgl)        :: dmin
    character(4)          :: progmode
    character(3)          :: dispmode
    character(fnlen)      :: xtalname
    character(fnlen)      :: STEMnmlfile
    character(fnlen)      :: dataname
    character(fnlen)      :: defectfilename
    character(fnlen)      :: dispfile
end type STEMDCINameListType


! STEM geometry name list type (MDG 07/02/17)
type STEMGeometryNameListType
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
end type STEMGeometryNameListType

! SRdefect type PGC 02/09/2017
type SRdefectNameListType
  integer(kind=irg) :: DF_npix
  integer(kind=irg) :: DF_npiy
  integer(kind=irg) :: dinfo
  integer(kind=irg) :: output
  integer(kind=irg) :: t_interval
  integer(kind=irg) :: nthreads
  integer(kind=irg) :: SRG(3)
  integer(kind=irg) :: Grange
  real(kind=sgl)    :: voltage
  real(kind=sgl)    :: GLaue
  real(kind=sgl)    :: DF_L
  real(kind=sgl)    :: DF_slice
  real(kind=sgl)    :: dmin
  character(4)      :: progmode
  character(3)      :: outputformat
  character(3)      :: dispmode
  character(fnlen)  :: dataname
  character(fnlen)  :: dispfile
  character(fnlen)  :: xtalname
  character(fnlen)  :: STEMnmlfile
  character(fnlen)  :: defectfilename
  !! The following are now in ZAdefectnameListType
  ! :: foilnmlfile ! moved to ZAdefect type
  ! :: numvoids
  ! :: incname
  ! :: voidname
  ! :: numdisl
  ! :: dislname
  ! :: numsf
  ! :: sfname
end type SRdefectNameListType

! typedef for special EMgamma program
type EMgammaNameListType
    integer(kind=irg)   :: npix
    integer(kind=irg)   :: k(3)
    integer(kind=irg)   :: fn(3)
    real(kind=sgl)      :: voltage 
    real(kind=sgl)      :: klaue(2)
    real(kind=sgl)      :: dmin
    real(kind=sgl)      :: convergence
    real(kind=sgl)      :: thick
    logical             :: variants
    character(fnlen)    :: gammaname
    character(fnlen)    :: gammapname
    character(fnlen)    :: microfile
    character(fnlen)    :: variantfile
    character(fnlen)    :: defectfile
    character(fnlen)    :: outname
end type EMgammaNameListType

type RefineOrientationtype
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: matchdepth
        character(fnlen)        :: dotproductfile
        character(fnlen)        :: ctffile
        character(fnlen)        :: PSvariantfile
        character(fnlen)        :: method
        character(4)            :: modality
        logical                 :: inRAM
        real(kind=sgl)          :: step
        integer(kind=irg)       :: nmis
        integer(kind=irg)       :: niter
end type RefineOrientationtype

type FitOrientationPStype
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: matchdepth
        character(fnlen)        :: dotproductfile
        character(fnlen)        :: ctffile
        real(kind=sgl)          :: step
        character(fnlen)        :: PSvariantfile
        character(fnlen)        :: modality
end type FitOrientationPStype

type Fitalphavarianttype
        integer(kind=irg)       :: nthreads
        character(fnlen)        :: betadotproductfile
        character(fnlen)        :: alphadotproductfile
        character(fnlen)        :: outputfile
        character(fnlen)        :: variantquaternionfile
        real(kind=sgl)          :: step
end type Fitalphavarianttype

type PFInversionNameListType
        integer(kind=irg)       :: nLam
        integer(kind=irg)       :: ncub
        integer(kind=irg)       :: nnz
        integer(kind=irg)       :: nfiles
        real(kind=dbl)          :: damp
        character(fnlen)        :: xtalname
        character(fnlen)        :: flist(10)
        character(fnlen)        :: datafile
        character(fnlen)        :: mrcfile
end type PFInversionNameListType

! typedef for special EMgammaOpenCL program
type EMgammaOpenCLNameListType
    integer(kind=irg)   :: npix
    real(kind=sgl)      :: voltage
    real(kind=sgl)      :: dmin
    real(kind=sgl)      :: thick
    real(kind=sgl)      :: eu(3)
    integer(kind=irg)   :: platid
    integer(kind=irg)   :: devid
    character(fnlen)    :: gammaname
    character(fnlen)    :: gammapname
    character(fnlen)    :: microfile
    character(fnlen)    :: variantfile
    character(fnlen)    :: defectfile
    character(fnlen)    :: datafile
end type EMgammaOpenCLNameListType

type EMTwoPhaseNameListType
    character(fnlen)    :: xtalname_f       ! crystal structure of film
    character(fnlen)    :: xtalname_s       ! crystal structure of substrate
    character(fnlen)    :: datafile         ! HDF5 data file
    real(kind=sgl)      :: zf               ! film thickness
    real(kind=sgl)      :: zfoil               ! total foil thickness
    real(kind=sgl)      :: voltage          ! acceleration voltage
    real(kind=sgl)      :: dmin             ! minimum d-spacing between planes
    integer(kind=irg)   :: npix             ! size of output diffraction pattern

end type EMTwoPhaseNameListType

type MDElectronPropNameListType
        integer(kind=irg)       :: nthreads
        integer(kind=irg)       :: npix
        real(kind=sgl)          :: thetac
        character(1)            :: maskpattern
        character(fnlen)        :: xtalname
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

end type MDElectronPropNameListType

! typedef for special EMgammaSTEM program
type EMgammaSTEMNameListType
    real(kind=sgl)      :: voltage
    real(kind=sgl)      :: dmin
    real(kind=sgl)      :: eu(3)
    real(kind=sgl)      :: convergence
    integer(kind=irg)   :: platid
    integer(kind=irg)   :: devid
    character(fnlen)    :: gammaname
    character(fnlen)    :: gammapname
    character(fnlen)    :: microstructurefile
    character(fnlen)    :: datafile
end type EMgammaSTEMNameListType

type EMCBEDQCNameListType
    real(kind=sgl)      :: dmin
    real(kind=sgl)      :: QClatparm
    integer(kind=irg)   :: atno
    real(kind=sgl)      :: DWF
    real(kind=sgl)      :: voltage
    real(kind=sgl)      :: convergence
    real(kind=sgl)      :: eu(3)
    real(kind=sgl)      :: thickness
    character(fnlen)    :: datafile
    integer(kind=irg)   :: nthreads
    integer(kind=irg)   :: npix
    character(1)        :: centering
end type EMCBEDQCNameListType

!typedef for EMmdSTEM program
type EMmdSTEMNameListType
        character(fnlen)        :: xtalname
        character(fnlen)        :: datafile
        real(kind=sgl)          :: eu(3)
        character(3)            :: eulerconvention
        real(kind=dbl)          :: phi1, phi, phi2
        real(kind=sgl)          :: dmin
        real(kind=sgl)          :: voltage
        real(kind=sgl)          :: convergence
        integer(kind=irg)       :: platid
        integer(kind=irg)       :: devid
end type EMmdSTEMNameListType

end module NameListTypedefs
