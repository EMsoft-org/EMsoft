! ###################################################################
! Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:diffraction.f90
!--------------------------------------------------------------------------
!
! MODULE: diffraction
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Anything related to dynamical diffraction
!
!> @todo general cleanup; merge with dynamical module?; add Private/Public 
! 
!> @date   10/13/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date    3/14/02 MDG 2.2 added CalcDynMat routine
!> @date   01/10/14 MDG 3.0 update with new cell type etc...
!> @date   12/01/14 MDG 3.1 removal of all global variables, including mLambda etc...
!--------------------------------------------------------------------------
module diffraction

use local
use typedefs

! atomic scattering factor parametrization (Doyle-Turner, Smith-Burge)
! used only if absorption is not taken into account;  otherwise
! the Weickenmeier-Kohl routine is used.
real(kind=sgl),parameter,private   :: scatfac(8,98) = reshape( (/ &
        0.202,0.244,0.082,0.000,0.30868,0.08544,0.01273,0.00000, &
        0.091,0.181,0.110,0.036,0.18183,0.06212,0.01803,0.00284, &
        1.611,1.246,0.326,0.099,1.07638,0.30480,0.04533,0.00495, &
        1.250,1.334,0.360,0.106,0.60804,0.18591,0.03653,0.00416, &
        0.945,1.312,0.419,0.116,0.46444,0.14178,0.03223,0.00377, &
        0.731,1.195,0.456,0.125,0.36995,0.11297,0.02814,0.00346, &
        0.572,1.043,0.465,0.131,0.28847,0.09054,0.02421,0.00317, &
        0.455,0.917,0.472,0.138,0.23780,0.07622,0.02144,0.00296, &
        0.387,0.811,0.475,0.146,0.20239,0.06609,0.01931,0.00279, &
        0.303,0.720,0.475,0.153,0.17640,0.05860,0.01762,0.00266, &
        2.241,1.333,0.907,0.286,1.08004,0.24505,0.03391,0.00435, &
        2.268,1.803,0.839,0.289,0.73670,0.20175,0.03013,0.00405, &
        2.276,2.428,0.858,0.317,0.72322,0.19773,0.03080,0.00408, &
        2.129,2.533,0.835,0.322,0.57775,0.16476,0.02880,0.00386, &
        1.888,2.469,0.805,0.320,0.44876,0.13538,0.02642,0.00361, &
        1.659,2.386,0.790,0.321,0.36650,0.11488,0.02469,0.00340, &
        1.452,2.292,0.787,0.322,0.30935,0.09980,0.02234,0.00323, &
        1.274,2.190,0.793,0.326,0.26682,0.08813,0.02219,0.00307, &
        3.951,2.545,1.980,0.482,1.37075,0.22402,0.04532,0.00434, &
        4.470,2.971,1.970,0.482,0.99523,0.22696,0.04195,0.00417, &
        3.966,2.917,1.925,0.480,0.88960,0.20606,0.03856,0.00399, &
        3.565,2.818,1.893,0.483,0.81982,0.19049,0.03590,0.00386, &
        3.245,2.698,1.860,0.486,0.76379,0.17726,0.03363,0.00374, &
        2.307,2.334,1.823,0.490,0.78405,0.15785,0.03157,0.00364, &
        2.747,2.456,1.792,0.498,0.67786,0.15674,0.03000,0.00357, &
        2.544,2.343,1.759,0.506,0.64424,0.14880,0.02854,0.00350, &
        2.367,2.236,1.724,0.515,0.61431,0.14180,0.02725,0.00344, &
        2.210,2.134,1.689,0.524,0.58727,0.13553,0.02609,0.00339, &
        1.579,1.820,1.658,0.532,0.62940,0.12453,0.02504,0.00333, &
        1.942,1.950,1.619,0.543,0.54162,0.12518,0.02416,0.00330, &
        2.321,2.486,1.688,0.599,0.65602,0.15458,0.02581,0.00351, &
        2.447,2.702,1.616,0.601,0.55893,0.14393,0.02446,0.00342, &
        2.399,2.790,1.529,0.594,0.45718,0.12817,0.02280,0.00328, &
        2.298,2.854,1.456,0.590,0.38830,0.11536,0.02146,0.00316, &
        2.166,2.904,1.395,0.589,0.33899,0.10497,0.02041,0.00307, &
        2.034,2.927,1.342,0.589,0.29999,0.09598,0.01952,0.00299, &
        4.776,3.859,2.234,0.868,1.40782,0.18991,0.03701,0.00419, &
        5.848,4.003,2.342,0.880,1.04972,0.19367,0.03737,0.00414, &
        4.129,3.012,1.179,0.000,0.27548,0.05088,0.00591,0.000,   &
        4.105,3.144,1.229,0.000,0.28492,0.05277,0.00601,0.000,   &
        4.237,3.105,1.234,0.000,0.27415,0.05074,0.00593,0.000,   &
        3.120,3.906,2.361,0.850,0.72464,0.14642,0.03237,0.00366, &
        4.318,3.270,1.287,0.000,0.28246,0.05148,0.00590,0.000,   &
        4.358,3.298,1.323,0.000,0.27881,0.05179,0.00594,0.000,   &
        4.431,3.343,1.345,0.000,0.27911,0.05153,0.00592,0.000,   &
        4.436,3.454,1.383,0.000,0.28670,0.05269,0.00595,0.000,   &
        2.036,3.272,2.511,0.837,0.61497,0.11824,0.02846,0.00327, &
        2.574,3.259,2.547,0.838,0.55675,0.11838,0.02784,0.00322, &
        3.153,3.557,2.818,0.884,0.66649,0.14449,0.02976,0.00335, &
        3.450,3.735,2.118,0.877,0.59104,0.14179,0.02855,0.00327, &
        3.564,3.844,2.687,0.864,0.50487,0.13316,0.02691,0.00316, &
        4.785,3.688,1.500,0.000,0.27999,0.05083,0.00581,0.000,   &
        3.473,4.060,2.522,0.840,0.39441,0.11816,0.02415,0.00298, &
        3.366,4.147,2.443,0.829,0.35509,0.11117,0.02294,0.00289, &
        6.062,5.986,3.303,1.096,1.55837,0.19695,0.03335,0.00379, &
        7.821,6.004,3.280,1.103,1.17657,0.18778,0.03263,0.00376, &
        4.940,3.968,1.663,0.000,0.28716,0.05245,0.00594,0.000,   &
        5.007,3.980,1.678,0.000,0.28283,0.05183,0.00589,0.000,   &
        5.085,4.043,1.684,0.000,0.28588,0.05143,0.00581,0.000,   &
        5.151,4.075,1.683,0.000,0.28304,0.05073,0.00571,0.000,   &
        5.201,4.094,1.719,0.000,0.28079,0.05081,0.00576,0.000,   &
        5.255,4.113,1.743,0.000,0.28016,0.05037,0.00577,0.000,   &
        6.267,4.844,3.202,1.200,1.00298,0.16066,0.02980,0.00367, &
        5.225,4.314,1.827,0.000,0.29158,0.05259,0.00586,0.000,   &
        5.272,4.347,1.844,0.000,0.29046,0.05226,0.00585,0.000,   &
        5.332,4.370,1.863,0.000,0.28888,0.05198,0.00581,0.000,   &
        5.376,4.403,1.884,0.000,0.28773,0.05174,0.00582,0.000,   &
        5.436,4.437,1.891,0.000,0.28655,0.05117,0.00577,0.000,   &
        5.441,4.510,1.956,0.000,0.29149,0.05264,0.00590,0.000,   &
        5.529,4.533,1.945,0.000,0.28927,0.05144,0.00578,0.000,   &
        5.553,4.580,1.969,0.000,0.28907,0.05160,0.00577,0.000,   &
        5.588,4.619,1.997,0.000,0.29001,0.05164,0.00579,0.000,   &
        5.659,4.630,2.014,0.000,0.28807,0.05114,0.00578,0.000,   &
        5.709,4.677,2.019,0.000,0.28782,0.05084,0.00572,0.000,   &
        5.695,4.740,2.064,0.000,0.28968,0.05156,0.00575,0.000,   &
        5.750,4.773,2.079,0.000,0.28933,0.05139,0.00573,0.000,   &
        5.754,4.851,2.096,0.000,0.29159,0.05152,0.00570,0.000,   &
        5.803,4.870,2.127,0.000,0.29016,0.05150,0.00572,0.000,   &
        2.388,4.226,2.689,1.255,0.42866,0.09743,0.02264,0.00307, &
        2.682,4.241,2.755,1.270,0.42822,0.09856,0.02295,0.00307, &
        5.932,4.972,2.195,0.000,0.29086,0.05126,0.00572,0.000,   &
        3.510,4.552,3.154,1.359,0.52914,0.11884,0.02571,0.00321, &
        3.841,4.679,3.192,1.363,0.50261,0.11999,0.02560,0.00318, &
        6.070,4.997,2.232,0.000,0.28075,0.04999,0.00563,0.000,   &
        6.133,5.031,2.239,0.000,0.28047,0.04957,0.00558,0.000,   &
        4.078,4.978,3.096,1.326,0.38406,0.11020,0.02355,0.00299, &
        6.201,5.121,2.275,0.000,0.28200,0.04954,0.00556,0.000,   &
        6.215,5.170,2.316,0.000,0.28382,0.05002,0.00562,0.000,   &
        6.278,5.195,2.321,0.000,0.28323,0.04949,0.00557,0.000,   &
        6.264,5.263,2.367,0.000,0.28651,0.05030,0.00563,0.000,   &
        6.306,5.303,2.386,0.000,0.28688,0.05026,0.00561,0.000,   &
        6.767,6.729,4.014,1.561,0.85951,0.15642,0.02936,0.00335, &
        6.323,5.414,2.453,0.000,0.29142,0.05096,0.00568,0.000,   &
        6.415,5.419,2.449,0.000,0.28836,0.05022,0.00561,0.000,   &
        6.378,5.495,2.495,0.000,0.29156,0.05102,0.00565,0.000,   &
        6.460,5.469,2.471,0.000,0.28396,0.04970,0.00554,0.000,   &
        6.502,5.478,2.510,0.000,0.28375,0.04975,0.00561,0.000,   &
        6.548,5.526,2.520,0.000,0.28461,0.04965,0.00557,0.000/), (/8,98/))


! the following variables used to be globals, but they are now entries in the cell pointer structure...
!
! mLambda       = electron wavelength [nm]
! mRelcor       = relativistic correction factor gamma [dimensionless]
! mSigma        = interaction constant [ ]
! mPsihat       = relativistic acceleration potential

! interface statements
interface Calcsg
        module procedure CalcsgSingle
        module procedure CalcsgDouble
end interface

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetVoltage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief ask for accelerating voltage, then call CalcWaveLength
! 
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!> @date   12/02/14 MDG 3.1 added voltage as argument
!--------------------------------------------------------------------------
recursive subroutine GetVoltage(cell, rlp, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: GetVoltage

use io

IMPLICIT NONE

type(unitcell)            :: cell
type(gnode),INTENT(INOUT) :: rlp
!f2py intent(in,out) ::  rlp
logical,INTENT(IN),OPTIONAL             :: verbose
real(kind=sgl)            :: io_real(1)

call ReadValue('Enter the microscope accelerating voltage [kV, R] : ', io_real, 1)
cell%voltage = dble(io_real(1))

if (present(verbose)) then
  if (verbose) then
     write (*,*) 'cell%voltage      : ', cell%voltage
  end if 
end if 
call CalcWaveLength(cell,rlp,verbose=.TRUE.)
end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcWaveLength
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the electron wavelength and select scattering data set
! 
!> @details computes the relativistic electron wavelength
!>  These quantities are computed in double precision because of the 
!>  wide range of magnitudes.  If a crystal structure has been defined
!>  then the gamma*V_0 term is added to correct for refraction.
!
!> @param cell unit cell pointer
!> @param skip scattering set identifier (optional)
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!> @date   12/02/14 MDG 3.1 removed mAccvol as global variable
!--------------------------------------------------------------------------
recursive subroutine CalcWaveLength(cell,rlp,skip,verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcWaveLength

use constants
use symmetry
use io

IMPLICIT NONE

type(unitcell)                          :: cell
type(gnode),INTENT(INOUT)               :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN),OPTIONAL   :: skip                 !< scattering set identifier
logical,INTENT(IN),OPTIONAL             :: verbose

real(kind=dbl)                          :: temp1,temp2, oi_real(1)
integer(kind=irg)                       :: hkl(3), io_int(1)

  temp1 = 1.0D+9*cPlanck/dsqrt(2.D0*cRestmass*cCharge)
  temp2 = cCharge*0.5D0*cell%voltage*1000.D0/cRestmass/(cLight**2)

! relativistic correction factor (known as gamma)      
  cell%mRelcor = 1.0D0+2.0D0*temp2

! relativistic acceleration voltage
  cell%mPsihat = cell%voltage*(1.D0+temp2)*1000.D0

! compute the electron wavelength in nm
! compute V_0 and add it to mPsihat (corrected by mRelcor)
  call CalcPositions(cell,'v')

! which scattering factors should be used ?
  if (present(skip)) then
   select case (skip) 
    case(1); rlp%method='DT'; 
    case(2); rlp%method='WK'; 
    case(3); rlp%method='WK'; rlp%absorption=.TRUE.
   end select
  else
   call Message(' The following scattering factor sets are available :', frm = "(/A/)")
   call Message('  [1] Doyle-Turner/Smith-Burge (no absorption) ', frm = "(A)")
   call Message('  [2] Weickenmeier-Kohl (no absorption) ', frm = "(A)")
   call Message('  [3] Weickenmeier-Kohl (with absorption) ', frm = "(A/)")
   call ReadValue(' Which set do you want to use [1/2/3] ? ', io_int,1)
   rlp%absorption = .FALSE.
   select case (io_int(1)) 
    case(1); rlp%method='DT'; 
    case(2); rlp%method='WK'; 
    case(3); rlp%method='WK'; rlp%absorption=.TRUE.
   end select
  end if

 hkl=(/0,0,0/)
 call CalcUcg(cell,rlp,hkl) 
 cell%mPsihat = cell%mPsihat + dble(rlp%Vmod)
 cell%mLambda = temp1/dsqrt(cell%mPsihat)
! interaction constant sigma
 cell%mSigma = 2.D0*cPi*cRestmass*cell%mRelcor*cCharge*cell%mLambda
 cell%mSigma = 1.0D-18*cell%mSigma/cPlanck**2

 if (present(verbose)) then
  if (verbose) then
    oi_real(1) = rlp%Vmod
    call WriteValue(' Mean inner potential [V] ', oi_real, 1,"(' ',E10.4)")
    call Message(' Wavelength corrected for refraction', frm = "(A)")
    oi_real(1) = cell%mRelcor
    call WriteValue(' Relativistic correction factor [gamma]  ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = cell%mPsihat
    call WriteValue(' Relativistic Accelerating Potential [V] ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = cell%mLambda
    call WriteValue(' Electron Wavelength [nm]                ', oi_real, 1,"(' ',E10.4)")
    oi_real(1) = cell%mSigma
    call WriteValue(' Interaction constant [V nm]^(-1)        ', oi_real, 1,"(' ',E10.4)")
  end if
 end if

 
end subroutine

!--------------------------------------------------------------------------
!
! FUNCTION: CalcDiffAngle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the diffraction angle 2theta in radians
!
!> @param h  Miller index
!> @param k  Miller index
!> @param l  Miller index
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive function CalcDiffAngle(cell,h,k,l) result(tt)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcDiffAngle

use crystal

IMPLICIT NONE

type(unitcell)                  :: cell
integer(kind=irg),INTENT(IN)    :: h,k,l                !< Miller indices

real(kind=sgl)                  :: tt

tt = 2.0*asin(0.50*sngl(cell%mLambda)*CalcLength( cell, float( (/h,k,l/) ), 'r') )

end function

!--------------------------------------------------------------------------
!
! FUNCTION: LorentzPF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the Lorentz Polarization Factor Lp
!
!> @param theta scattering angle  
!> @param HEDM optional string to indicate HEDM mode
!
!> @date  03/26/13  MDG  1.0 added for HEDM project
!--------------------------------------------------------------------------
recursive function LorentzPF(theta,HEDM) result(tt)
!DEC$ ATTRIBUTES DLLEXPORT :: LorentzPF

use crystal

IMPLICIT NONE

real(kind=sgl),INTENT(IN)                       :: theta                !< scattering angle
character(*),INTENT(IN),OPTIONAL                :: HEDM         !< for HEDM we have a different polarization factor
real(kind=sgl)                                  :: tt

if (present(HEDM)) then
  tt = (1.0+cos(2.0*theta)**2) / sin(theta)**2 / cos(theta)
else
  tt = (1.0+cos(2.0*theta)**2) / sin(theta)**2 / cos(theta)
end if

end function

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcUcg
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the complex Structure Factor for a given g
!
!> @details includes computation of extinction distance, absorption length, etc...
!> This routine is probably the most important one in all the dynamical routines,
!> because it computes all possible relevant parameters and stores them in the rlp variable.
!> We've added the XR rlp%method parameter so that the same routine can be used for the
!> computation of kinematical x-ray scattering; this was needed for the HEDM package.
!
!> @param cell unit cell pointer
!> @param rlp reciprocal lattice point
!> @param hkl  Miller indices
!> @param applyqgshift [optional] multiply qg by exp[i theta_g] if present and .TRUE.
!
!> @note CalcPositions must be called before calling this routine
!
!> @date  10/20/98 MDG 1.0 original
!> @date   5/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  03/26/13 MDG 3.0 updated IO
!> @date  03/26/13 MDG 3.1 added XRD support
!> @date  01/10/14 MDG 4.0 new cell type
!> @date  06/09/14 MDG 4.1 added cell as argument
!> @date  12/02/14 MDG 4.2 added voltage as argument
!> @date  09/11/15 MDG 4.3 added optional argument
!> @date  08/09/18 MDG 5.0 added option to use precomputed FSCATT values stored in cell structure
!--------------------------------------------------------------------------
recursive subroutine CalcUcg(cell,rlp,hkl,applyqgshift,interpolate)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcUcg

use crystal
use symmetry
use constants
use others

IMPLICIT NONE

type(unitcell)                  :: cell
type(gnode),INTENT(INOUT)       :: rlp
!f2py intent(in,out) ::  rlp
integer(kind=irg),INTENT(IN)    :: hkl(3)               !< Miller indices
logical,OPTIONAL,INTENT(IN)     :: applyqgshift
logical,OPTIONAL,INTENT(IN)     :: interpolate          ! requires rlp%mode = 'IP'

integer(kind=irg)               :: j,absflg,m,ii
real(kind=sgl)                  :: s,twopi,arg,swk,dwwk,pref,ul,pre,sct,fs,fsp
real(kind=sgl),parameter        :: preg = 0.664840340614319 ! = 2.0 * sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18
complex(kind=sgl)               :: ff,gg,sf,p1
complex(kind=sgl)               :: czero
complex(kind=sgl),allocatable   :: sfarray(:)
logical                         :: accflg, dwflg, interp
character(2)                    :: smb

! interpolatiobn is only used for the WK mode to pre-compute the scattering factor array
interp = .FALSE.
if (present(interpolate)) then
  if (interpolate.eqv..TRUE.) interp=.TRUE.
end if

twopi = sngl(2.D0*cPi)
czero = cmplx(0.0,0.0)
rlp%hkl = hkl

! compute the scattering parameter s^2=(g/2)^2
 if (sum(hkl**2).eq.0) then 
  s = 0.0
  rlp%g = 0.0
 else
  rlp%g = sngl(CalcLength(cell,dble(hkl),'r'))
  s = (0.50*rlp%g)**2
 end if

!----------------------------------
! first the simplest case: kinematical X-ray scattering factors
! this option was added to accomodate the HEDM forward projector needs
if (rlp%method.eq.'XR') then 

! set the prefactor for the Doyle-Turner summation
  pref = 0.4178214

! initialize the real and imaginary parts of the structure factor
  sf = czero
  
! loop over all atoms in the asymmetric unit
 do m=1,cell % ATOM_ntype

! get the atomic scattering factor for this atom
  sct=0.0
  j=cell % ATOM_type(m)
  do ii=1,4
   sct=sct+scatfac(ii,j)*exp(-scatfac(ii+4,j)*s)
  end do

! scale and include Debye-Waller factor and site occupation parameter
  fsp = pref * s * sct 
  fs = (float(cell % ATOM_type(m)) - fsp ) * cell % ATOM_pos(m,4) * exp(-cell % ATOM_pos(m,5)*s)
!  write (*,*) 'atom type ',cell % ATOM_type(m),'; s = ',sqrt(s),'; fs = ',float(cell % ATOM_type(m))-fsp, &
!      '; occ. ',cell % ATOM_pos(m,4),'; DW ',exp(-cell % ATOM_pos(m,5)*s),'; fs = ',fs,'; sct = ',sct

! loop over all atoms in the orbit
  do j=1,cell%numat(m)
   arg=twopi * sum(hkl(1:3)*cell%apos(m,j,1:3))
   sf = sf + fs * cmplx(cos(arg),-sin(arg))
  end do

 end do ! m loop

! and fill in just two entries of the rlp variable
 rlp%hkl = hkl
 rlp%Ucg = sf
 
end if

!----------------------------------
! for Doyle-Turner scattering factors
if (rlp%method.eq.'DT') then 
! initialize the real and imaginary parts of the structure factor
 sf = czero

! compute the prefactor (this also scales from Angstrom to nm)
 pref = 0.04787801/cell % vol

! loop over all atoms in the asymmetric unit
 do m=1,cell % ATOM_ntype

! get the atomic scattering factor for this atom
  sct=0.0
  j=cell % ATOM_type(m)
  do ii=1,4
   sct=sct+scatfac(ii,j)*exp(-scatfac(ii+4,j)*s)
  end do

! scale and include Debye-Waller factor
! and site occupation parameter
  fs=pref*sct*exp(-cell % ATOM_pos(m,5)*s)*cell % ATOM_pos(m,4)

! loop over all atoms in the orbit
  do j=1,cell%numat(m)
   arg=twopi*sum(hkl(1:3)*cell%apos(m,j,1:3))
   sf = sf + fs*exp(cmplx(0.0,-arg))
  end do
 end do ! m loop

! and fill in the entries of the rlp variable
 pre = 2.0*sngl(cRestmass*cCharge/cPlanck**2)*1.0E-18
 rlp%hkl = hkl
 rlp%Vmod = cabs(sf)*cell%mRelcor
 rlp%Vphase = atan2(aimag(sf),real(sf))
 rlp%Vpmod = 0.0
 rlp%Vpphase = 0.0
 if (rlp%Vmod.gt.0.0) then
  rlp%xg = 1.0/(pre*rlp%Vmod*cell%mLambda)
 else
  rlp%xg = 1.0E+8
 end if
 rlp%xgp = 0.0
 rlp%ar = 0.0
 rlp%Vg = rlp%Vmod * exp(cmplx(0.0,rlp%Vphase))
 rlp%Ucg = pre*rlp%Vg
 rlp%qg = cmplx(1.0/rlp%xg,0.0)
end if


!----------------------------------
if (rlp%method.eq.'WK') then 
! The Weickenmeier-Kohl (WK) subroutine works in Angstrom, and also 
! scales reciprocal space by a factor of 2*pi;  this scaling
! is accomplished by changing the g-value in nm^{-1} by a 
! scaling factor swk = 2*pi/10, to go from book units to WK units.
!
! A similar scaling must be performed on the Debye Waller factor;
! the book defines it as exp(-Bs^2), with s in [nm^2]; WK define
! it in A^2, and with a scaled reciprocal space.  The conversion
! factor dwwk = 100.0/8*pi^2
!
! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
 swk = 0.1*twopi
 dwwk = 100.0/(8.0*cPi**2)
  
! properly scale the scattering parameter
 s = rlp%g*swk

! let fscatt perform the relativistic corrections for f_g and fprime_g
 accflg = .TRUE.

! include absorption ?
 absflg = 0
 if (rlp%absorption.eqv..TRUE.) absflg = 3  ! include phonon and core contributions

! always include Debye-Waller factor
 dwflg  = .TRUE.

! compute the scaling prefactors
! pref contains A to nm conversion, and divides by 4pi
 pref = 0.04787801/cell % vol/(4.0*cPi) 

! preg is used to go from V to U, remembering that gamma is already
! included in the output from fscatt
 pre = pref * preg

! initialize the real and imaginary parts of the structure factor
 ff=czero
 gg=czero

! loop over all atoms in the asymmetric unit
 do m=1,cell % ATOM_ntype
! get the atomic scattering factor for this atom
! scale and include Debye-Waller factor and site occupation parameter
  ul = sqrt(cell % ATOM_pos(m,5)*dwwk)
  j = cell % ATOM_type(m)
  sf = FSCATT(s,ul,j,smb,sngl(cell%voltage),absflg,accflg,dwflg)*cmplx(cell%ATOM_pos(m,4),0.0)

! loop over all atoms in the orbit
  p1 = czero
  do j=1,cell%numat(m)
   arg=twopi*sum(float(hkl(1:3))*cell%apos(m,j,1:3))
   p1 = p1 + exp(cmplx(0.0,-arg))
  end do

  ff = ff + p1*real(sf)
  gg = gg + p1*aimag(sf)

 end do
!
! fill in the entries of the rlp variable
 rlp%hkl = hkl

! these are the modulus and phase of the real part of Vg
 rlp%Vmod = pref * cabs(ff)
 rlp%Vphase = atan2(aimag(ff),real(ff))

! modulus of U_g
 rlp%Umod = preg*rlp%Vmod

! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g
 if (rlp%absorption.eqv..TRUE.) then 
  rlp%Vpmod = pref * cabs(gg)
  rlp%Vpphase = atan2(aimag(gg),real(gg))

! modulus of Uprime_g
  rlp%Upmod = preg*rlp%Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
  rlp%Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))
 else ! set absorption parameters to zero
  rlp%Vpmod = 0.0
  rlp%Vpphase = 0.0
! Ucg = U_g (complex number)
  rlp%Ucg = pre * ff
 end if

! complex Vg 
 rlp%Vg = rlp%Ucg/preg
 if (abs(rlp%Umod).gt.0.0) then 
  rlp%xg = 1.0/abs(rlp%Umod)/cell%mLambda
 else
  rlp%xg = 1.0E+8
 end if 

 if (abs(rlp%Upmod).gt.0.0) then 
  rlp%xgp = 1.0/abs(rlp%Upmod)/cell%mLambda
 else
  rlp%xgp = 1.0E+8
 end if 

 if (rlp%absorption.eqv..TRUE.) then 
  rlp%ar = rlp%xgp/rlp%xg
  if (present(applyqgshift)) then
    if (applyqgshift.eqv..TRUE.) then
      rlp%qg = cmplx(cos(rlp%Vphase)/rlp%xg-sin(rlp%Vpphase)/rlp%xgp,cos(rlp%Vpphase)/rlp%xgp+sin(rlp%Vphase)/rlp%xg)
    end if
  else
    arg = rlp%Vpphase-rlp%Vphase
    rlp%qg = cmplx(1.0/rlp%xg-sin(arg)/rlp%xgp,cos(arg)/rlp%xgp)
  end if
 else
  rlp%ar = 0.0
  rlp%qg = cmplx(1.0/rlp%xg,0.0)
 end if

end if

!----------------------------------
if (rlp%method.eq.'IP') then 
 allocate(sfarray(cell%ATOM_ntype))

! The Weickenmeier-Kohl (WK) scattering parameters have been pre-calculated 
! so all we need to do is linear interpolation to get the correct value
 swk = 0.1*twopi
  
! properly scale the scattering parameter
 s = rlp%g*swk

! get the atomic scattering factors for all atom types by linear interpolation
 call getScatfac(cell, s, sfarray, cell%ATOM_ntype)

! compute the scaling prefactors
! pref contains A to nm conversion, and divides by 4pi
 pref = 0.04787801/cell % vol/(4.0*cPi) 

! preg is used to go from V to U, remembering that gamma is already
! included in the output from fscatt
 pre = pref * preg

! initialize the real and imaginary parts of the structure factor
 ff=czero
 gg=czero

! loop over all atoms in the asymmetric unit
 do m=1,cell % ATOM_ntype
  sf = sfarray(m)

! loop over all atoms in the orbit
  p1 = czero
  do j=1,cell%numat(m)
   arg=twopi*sum(float(hkl(1:3))*cell%apos(m,j,1:3))
   p1 = p1 + exp(cmplx(0.0,-arg))
  end do

  ff = ff + p1*real(sf)
  gg = gg + p1*aimag(sf)

 end do
!
! fill in the entries of the rlp variable
 rlp%hkl = hkl

! these are the modulus and phase of the real part of Vg
 rlp%Vmod = pref * cabs(ff)
 rlp%Vphase = atan2(aimag(ff),real(ff))

! modulus of U_g
 rlp%Umod = preg*rlp%Vmod

! if absorption is included, also compute the imaginary part of Vg, i.e., Vprime_g
 if (rlp%absorption.eqv..TRUE.) then 
  rlp%Vpmod = pref * cabs(gg)
  rlp%Vpphase = atan2(aimag(gg),real(gg))

! modulus of Uprime_g
  rlp%Upmod = preg*rlp%Vpmod

! complex Ucg = U_g + i Uprime_g = U_g,r-Uprime_g,i + i(U_g,i+Uprime_g,r)
  rlp%Ucg = pre * cmplx(real(ff)-aimag(gg),aimag(ff)+real(gg))
 else ! set absorption parameters to zero
  rlp%Vpmod = 0.0
  rlp%Vpphase = 0.0
! Ucg = U_g (complex number)
  rlp%Ucg = pre * ff
 end if

! complex Vg 
 rlp%Vg = rlp%Ucg/preg
 if (abs(rlp%Umod).gt.0.0) then 
  rlp%xg = 1.0/abs(rlp%Umod)/cell%mLambda
 else
  rlp%xg = 1.0E+8
 end if 

 if (abs(rlp%Upmod).gt.0.0) then 
  rlp%xgp = 1.0/abs(rlp%Upmod)/cell%mLambda
 else
  rlp%xgp = 1.0E+8
 end if 

 if (rlp%absorption.eqv..TRUE.) then 
  rlp%ar = rlp%xgp/rlp%xg
  if (present(applyqgshift)) then
    if (applyqgshift.eqv..TRUE.) then
      rlp%qg = cmplx(cos(rlp%Vphase)/rlp%xg-sin(rlp%Vpphase)/rlp%xgp,cos(rlp%Vpphase)/rlp%xgp+sin(rlp%Vphase)/rlp%xg)
    end if
  else
    arg = rlp%Vpphase-rlp%Vphase
    rlp%qg = cmplx(1.0/rlp%xg-sin(arg)/rlp%xgp,cos(arg)/rlp%xgp)
  end if
 else
  rlp%ar = 0.0
  rlp%qg = cmplx(1.0/rlp%xg,0.0)
 end if

 deallocate(sfarray)
end if


end subroutine CalcUcg


!--------------------------------------------------------------------------
!
! SUBROUTINE: PreCalcFSCATT
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief precompute the FSCATT values for interpolation purposes, to speed up STEM-DCI and other codes
!
!> @param cell unit cell pointer
!> @param dmin smallest d-spacing to consider
!> @param gstep step size in the cell%scatfacg array
!
!> @date   08/09/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PreCalcFSCATT(cell, dmin, gstep)
!DEC$ ATTRIBUTES DLLEXPORT :: PreCalcFSCATT

use crystal
!use symmetry
use constants
use others

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: dmin
real(kind=sgl),INTENT(IN)       :: gstep

integer(kind=irg)               :: j,m,ii,i
real(kind=sgl)                  :: s,ul
real(kind=sgl),parameter        :: swk = 0.628318530717959
real(kind=sgl),parameter        :: dwwk = 1.26651479552922
integer(kind=irg),parameter     :: absflg = 3
logical                         :: accflg=.TRUE., dwflg=.TRUE.
character(2)                    :: smb

! first generate the array of s-values for which the scattering factors need to be computed
s = 2.0/dmin   ! maximum range in reciprocal space
cell%numscatfac = nint(s/gstep) + 2
allocate(cell%scatfacg(cell%numscatfac))
cell%scatfacg = (/ (gstep * float(i-1),i=1,cell%numscatfac) /)
cell%scatfacg = cell%scatfacg * swk

! allocate the scattering factor interpolation array
allocate( cell%scatfac(cell%numscatfac, cell % ATOM_ntype) )

! The Weickenmeier-Kohl (WK) subroutine works in Angstrom, and also 
! scales reciprocal space by a factor of 2*pi;  this scaling
! is accomplished by changing the g-value in nm^{-1} by a 
! scaling factor swk = 2*pi/10, to go from book units to WK units.
!
! A similar scaling must be performed on the Debye Waller factor;
! the book defines it as exp(-Bs^2), with s in [nm^2]; WK define
! it in A^2, and with a scaled reciprocal space.  The conversion
! factor dwwk = 100.0/8*pi^2
!
! To go from the standard B factor in [nm^2] to ul^2 in A^2,
! ul = sqrt(B*dwwk)
  
do i=1,cell%numscatfac
! properly scale the scattering parameter
 s = cell%scatfacg(i)

! loop over all atoms in the asymmetric unit
 do m=1,cell % ATOM_ntype
! get the atomic scattering factor for this atom
! scale and include Debye-Waller factor and site occupation parameter
  ul = sqrt(cell % ATOM_pos(m,5)*dwwk)
  j = cell % ATOM_type(m)
  cell%scatfac(i,m) = FSCATT(s,ul,j,smb,sngl(cell%voltage),absflg,accflg,dwflg)*cell%ATOM_pos(m,4)
 end do 
end do 

end subroutine PreCalcFSCATT

!--------------------------------------------------------------------------
!
! SUBROUTINE: getScatfac
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief interpolate the precomputed FSCATT values 
!
!> @param cell unit cell pointer
!> @param s reciprocal distance value
!> @param sfarray returned scattering factor values
!
!> @date   08/09/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine getScatfac(cell, s, sfarray, ntypes)
!DEC$ ATTRIBUTES DLLEXPORT :: getScatfac

use crystal
!use symmetry
use constants
use others

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: s
integer(kind=irg),INTENT(IN)    :: ntypes
complex(kind=sgl),INTENT(OUT)   :: sfarray(ntypes)

integer(kind=irg)               :: jj
real(kind=sgl)                  :: dx

if (s.eq.0.0) then 
    sfarray(1:ntypes) = cell%scatfac(1,1:ntypes)
else
    jj = ifix(s/cell%scatfacg(2))
    if (jj.ge.cell%numscatfac) then
        sfarray(1:ntypes) = cell%scatfac(cell%numscatfac,1:ntypes)
    else
        dx = s/cell%scatfacg(2) - float(jj)
        sfarray(1:ntypes) = cell%scatfac(jj,1:ntypes)*(1.0-dx) + &
                                     cell%scatfac(jj+1,1:ntypes)*dx
    end if
end if

end subroutine getScatfac


!--------------------------------------------------------------------------
!
! FUNCTION: CalcsgSingle
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the excitation error for a given reflection
!
!> @param cell unit cell pointer
!> @param gg reciprocal lattice point indices
!> @param kk wave vector components
!> @param FN foil normal
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!> @date   06/09/14 MDG 4.0 added cell as argument 
!--------------------------------------------------------------------------
recursive function CalcsgSingle(cell,gg,kk,FN) result(sg)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcsgSingle

use crystal

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: gg(3)                !< reciprocal lattice point
real(kind=sgl),INTENT(IN)       :: kk(3)                !< wave vector
real(kind=sgl),INTENT(IN)       :: FN(3)                !< foil normal

real(kind=sgl)                  :: kpg(3),tkpg(3),xnom,xden,q1,q2,sg


 kpg=kk+gg
 tkpg=2.0*kk+gg

! use equation of Ewald sphere
 xnom = -CalcDot(cell,gg,tkpg,'r')

! 2|k0+g|cos(alpha) = 2(k0+g).Foilnormal
 q1 = CalcLength(cell,kpg,'r')
 q2 = CalcAngle(cell,kpg,FN,'r')
 xden = 2.0*q1*cos(q2)
 sg = xnom/xden

end function CalcsgSingle

!--------------------------------------------------------------------------
!
! FUNCTION: CalcsgDouble
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute the excitation error for a given reflection (double precision)
!
!> @param cell unit cell pointer
!> @param gg reciprocal lattice point indices
!> @param kk wave vector components
!> @param FN foil normal
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!> @date   06/09/14 MDG 4.0 added cell as argument
!--------------------------------------------------------------------------
recursive function CalcsgDouble(cell,gg,kk,FN) result(sg)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcsgDouble

use crystal

IMPLICIT NONE

type(unitcell)                  :: cell
real(kind=dbl),INTENT(IN)       :: gg(3)                !< reciprocal lattice point
real(kind=dbl),INTENT(IN)       :: kk(3)                !< wave vector
real(kind=dbl),INTENT(IN)       :: FN(3)                !< foil normal

real(kind=dbl)                  :: kpg(3),tkpg(3),xnom,xden,q1,q2,sg

 kpg=kk+gg
 tkpg=2.D0*kk+gg

! use equation of Ewald sphere
 xnom = -CalcDot(cell,gg,tkpg,'r')

! 2|k0+g|cos(alpha) = 2(k0+g).Foilnormal
 q1 = CalcLength(cell,kpg,'r')
 q2 = CalcAngle(cell,kpg,FN,'r')
 xden = 2.D0*q1*dcos(q2)
 sg = xnom/xden

end function CalcsgDouble


!--------------------------------------------------------------------------
!
! SUBROUTINE: TBCalcSM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief 2-beam scattering matrix implementation
!
!> @details compute crystal scattering matrix (real and imaginary
!>  part) for a given set of parameters. Optimized to minimize the 
!>  total number of function evaluations and multiplications/divisions
!
!> @param Ar real part of dynamical matrix
!> @param Ai imaginary part 
!> @param sg excitation error
!> @param z thickness
!> @param xig extinction distance
!> @param xigp anomalous absorption length
!> @param xizero normal absorption length
!> @param betag phase factor
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine TBCalcSM(Ar,Ai,sg,z,xig,xigp,xizero,betag)
!DEC$ ATTRIBUTES DLLEXPORT :: TBCalcSM

use constants

IMPLICIT NONE

real(kind=sgl),INTENT(IN)       :: sg                   !< excitation error
real(kind=sgl),INTENT(IN)       :: z                    !< thickness
real(kind=sgl),INTENT(IN)       :: xig          !< extinction distance
real(kind=sgl),INTENT(IN)       :: xigp                 !< anomalous absorption length
real(kind=sgl),INTENT(IN)       :: xizero               !< normal absorption length
real(kind=sgl),INTENT(IN)       :: betag                !< phase parameter
real(kind=sgl),INTENT(OUT)      :: Ar(2,2)              !< real part of result 
real(kind=sgl),INTENT(OUT)      :: Ai(2,2)              !< imaginary part of result 

real(kind=sgl)  :: pr, pi, cs, ss, ch, sh, q, q1, q2, sgs, sr, si, o , p, sb, cb, e, r, sq, xigi,xigpi

! setup auxiliary variables 
 xigi = 1.00/xig
 xigpi = 1.00/xigp

! sigma squared
 cb = cos(betag)
 sb = sin(betag)
 q = sg**2+xigi**2-xigpi**2
 r = cb*xigi*xigpi
 sq = sqrt(q**2+4.0*r**2)

! real part of sigma
 sr = sqrt(0.5*(q+sq))

! imaginary part of sigma
 si = r/sr
 sq = 1.0/sq

! s_g divided by sigma squared
 sgs = sg*sq

! arguments of trigonometric and hyperbolic functions
 e = cPi*z
 pr = e*sr
 pi = e*si
 e = exp(-e/xizero)

! trigonometric and hyperbolic functions
 cs = cos(pr)
 ss = sin(pr)
 ch = cosh(pi)
 sh = sinh(pi)
 o = ss*ch
 p = cs*sh

! transmitted amplitude T including normal absorption
 q = e*sgs
 q1 = q*(si*o-sr*p)
 q2 = q*(sr*o+si*p)
 Ar(1,1) = e*cs*ch
 Ai(1,1) = -e*ss*sh
 Ar(2,2) = Ar(1,1)+q1
 Ai(2,2) = Ai(1,1)+q2
 Ar(1,1) = Ar(1,1)-q1
 Ai(1,1) = Ai(1,1)-q2

! scattered amplitude S including normal absorption
 q1 = e*sq*(si*xigi-(sr*cb+si*sb)*xigpi)
 q2 = e*sq*(sr*xigi-(sr*sb-si*cb)*xigpi)
 Ar(1,2) = q1*o-q2*p
 Ai(1,2) = q2*o+q1*p
 Ar(2,1) = Ar(1,2)
 Ai(2,1) = Ai(1,2)

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: TBCalcInten
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief 2-beam transmitted and scattered intensities
!
!> @details compute transmitted and scattered intensities for
!>  the perfect crystal case.   This routine does not make use of 
!>  complex number arithmetic, but instead uses the analytical 
!>  expressions for the two-beam intensities derived in section 
!>  6.3.3.4 on page 356-357.
!
!> @param It transmitted intensity
!> @param Is scattered intensity 
!> @param sg excitation error
!> @param z thickness
!> @param xig extinction distance
!> @param xigp anomalous absorption length
!> @param xizero normal absorption length
!> @param betag phase factor
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine TBCalcInten(It,Is,sg,z,xig,xigp,xizero,betag)
!DEC$ ATTRIBUTES DLLEXPORT :: TBCalcInten

use constants

IMPLICIT NONE

real(kind=sgl),INTENT(IN)       :: sg                   !< excitation error
real(kind=sgl),INTENT(IN)       :: z                    !< thickness
real(kind=sgl),INTENT(IN)       :: xig          !< extinction distance
real(kind=sgl),INTENT(IN)       :: xigp                 !< anomalous absorption length
real(kind=sgl),INTENT(IN)       :: xizero               !< normal absorption length
real(kind=sgl),INTENT(IN)       :: betag                !< phase parameter
real(kind=sgl),INTENT(OUT)      :: It                   !< real part of result 
real(kind=sgl),INTENT(OUT)      :: Is                   !< imaginary part of result 

real(kind=sgl) :: q, r, sq, qgsi, e, sr, si, cp, ch, pr, pi, xigi, xigpi, sgs
     
! setup auxiliary variables 
 xigi = 1.0/xig
 xigpi = 1.0/xigp

! sigma squared
 q = sg**2+xigi**2-xigpi**2
 r = cos(betag)*xigi*xigpi
 sq = sqrt(q**2+4.0*r**2)

! real part of sigma
 sr = sqrt(0.5*(q+sq))

! imaginary part of sigma
 si = r/sr
 sq = 1.0/sq

! reciprocal of q_g squared
 qgsi = xigi**2+xigpi**2-2.0*sin(betag)*xigi*xigpi

! s_g squared divided by sigma squared
 sgs = sg**2*sq

! arguments of trigonometric and hyperbolic functions
 e = 2.0*cPi*z
 pr = e*sr
 pi = e*si
 e = exp(-e/xizero)

! trigonometric functions 
 cp = cos(pr)
 ch = cosh(pi)

! transmitted intensity It
 It = 0.5*ch*(1.0+sgs)+sg*sq*(sr*sinh(pi)-si*sin(pr))+0.5*cp*(1.0-sgs)
 It = It*e

! scattered intensity Is
 Is = 0.5*qgsi*sq*e*(ch-cp)

end subroutine


!--------------------------------------------------------------------------
!
! SUBROUTINE: DiffPage
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw kinematical zone axis electron diffraction patterns
!
!> @param cell unit cell pointer
!> @param PS Postscript structure
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!> @date   01/10/14 MDG 4.0 update for new cell type
!> @date   06/09/14 MDG 4.1 added cell, PS as arguments
!> @date   12/02/14 MDG 4.2 added camlen as argument; modified Vg and Vgsave arrays
!--------------------------------------------------------------------------
recursive subroutine DiffPage(PS,cell,rlp,camlen)
!DEC$ ATTRIBUTES DLLEXPORT :: DiffPage

use postscript
use crystal
use symmetry
use math
use io
use constants

IMPLICIT NONE

type(unitcell)                  :: cell
type(postscript_type),INTENT(INOUT) :: PS
!f2py intent(in,out) ::  PS
type(gnode),INTENT(INOUT)       :: rlp
!f2py intent(in,out) ::  rlp
real(kind=sgl),INTENT(IN)       :: camlen

integer(kind=irg),parameter     :: inm = 5
character(1)                    :: list(256)
logical                         :: first,np,ppat,a
logical,allocatable             :: z(:,:,:),zr(:,:,:)
integer(kind=irg)               :: i,j,h,k,l,m,totfam,hh,ll,fmax,inmhkl(3),ricnt,icnt,ind(3),uu,vv,ww,slect(256), &
                                   js,ii,num,hc,hhcc,iinm,dpcnt,imo,ih,ik,il,ier,iref, io_int(4)
integer(kind=irg),allocatable   :: idx(:)
integer(kind=irg),allocatable   :: family(:,:),numfam(:)
real(kind=sgl)                  :: twopi,ggl,g(3),Vmax,laL,gmax,RR,thr, oi_real(1)
real(kind=sgl),allocatable      :: gg(:)
real(kind=sgl),parameter        :: xoff(0:5)=(/0.0,3.3125,0.0,3.3125,0.0,3.3125/),yoff(0:5)=(/6.0,6.0,3.0,3.0,0.0,0.0/), &
                                   eps = 1.0E-3
logical,allocatable             :: dbdiff(:)
integer(kind=irg)               :: itmp(48,3)   !< array used for family computations etc

real(kind=sgl),allocatable      :: Vg(:),rg(:),Vgsave(:)
integer(kind=irg),allocatable   :: rfamily(:,:,:),rnumfam(:)


! set some parameters
 cell % SG % SYM_reduce=.TRUE.
 thr = 1.E-4 
 twopi = 2.0*cPi
 Vmax = 0.0

! gmax is the radius of the sphere whose intersection with the 
! back focal plane is the circle on the output zone axis patterns
 laL = sngl(cell%mLambda) * camlen
 RR = 1.375 * 25.4
 gmax = RR/laL

 oi_real(1) = sngl(cell%mLambda)
 call WriteValue('wavelength [nm] = ', oi_real, 1)
 oi_real(1) = camlen
 call WriteValue(' L         [mm] = ', oi_real, 1)
 oi_real(1) = laL
 call WriteValue('camera length lambda*L [mm nm] = ', oi_real, 1) 

! determine the families of reciprocal lattice points
! in a region of reciprocal space.
! set the index boundaries
 do i=1,3
  inmhkl(i) = 2*int(gmax/sqrt(cell % rmt(i,i)))
 end do
 hc = maxval(inmhkl)

! allocate all the necessary arrays
 allocate(zr(-hc:hc,-hc:hc,-hc:hc))
 allocate(z(-inm:inm,-inm:inm,-inm:inm))
 hhcc = (2*hc+1)**3
 allocate(Vg(hhcc))
 allocate(Vgsave(hhcc))
 allocate(rfamily(hhcc,48,3))
 allocate(rnumfam(hhcc))
 allocate(rg(hhcc))
 iinm = (2*inm+1)**3
 allocate(family(iinm,3))
 allocate(numfam(iinm))

! if this is a non-symmorphic space group, then also
! allocate the dbdiff array to tag potential double 
! diffraction reflections
 if (cell%nonsymmorphic) then
   allocate(dbdiff(hhcc))
   dbdiff(1:hhcc) = .FALSE.
 endif

! and initialize the ones that need to be initialized
 zr(-hc:hc,-hc:hc,-hc:hc) = .FALSE.
 z(-inm:inm,-inm:inm,-inm:inm) = .FALSE.

! here we go:
 first = .TRUE.
 icnt = 1
 totfam=0
 do h=-inmhkl(1),inmhkl(1)
  ind(1)=h
  do k=-inmhkl(2),inmhkl(2)
   ind(2)=k
   do l=-inmhkl(3),inmhkl(3)
    ind(3)=l
! make sure we have not already done this one in another family
    if (.not.zr(h,k,l)) then
! check the length, to make sure it lies within the sphere gmax
     ggl=CalcLength(cell,float(ind),'r')
! if it is larger than gmax, then compute the entire family
     if (ggl.ge.gmax) then
      call CalcFamily(cell,ind,num,'r',itmp)
! and label the family members in the zr array so that we 
! do not include those points later on in the loop
      do i=1,num
       zr(itmp(i,1),itmp(i,2),itmp(i,3))=.TRUE.
      end do
     else 
! if the length is smaller than gmax, then compute the 
! Fourier coefficient Vg and determine the entire family
! [recall that all members in a family have the same Vg]
! Do this only for those reflections that are allowed by
! the lattice centering !
      a = IsGAllowed(cell,(/h,k,l/))
      if (a) then
       call CalcUcg(cell,rlp,ind)
! check for nonsymmorphic systematic absences
       if ((cell%nonsymmorphic).and.(rlp%Vmod.lt.eps)) then
        io_int = (/ h, k, l, 0 /)
        call WriteValue(' potential double diffraction family :', io_int,3,"('{',I3,I3,I3,'}')")
        dbdiff(icnt) = .TRUE.
        rlp%Vmod = 0.0
       endif
! compute the entire family
       call CalcFamily(cell,ind,num,'r',itmp)
       rg(icnt)=ggl
! copy family in array
       do i=1,num
        rfamily(icnt,i,1:3)=itmp(i,1:3)
        zr(itmp(i,1),itmp(i,2),itmp(i,3))=.TRUE.
       end do
! and take the modulus squared for the intensity
       Vg(icnt)=rlp%Vmod**2
       Vgsave(icnt)=Vg(icnt)
! update the maximum value 
       Vmax = max(Vg(icnt),Vmax)
      else
! remove the equivalent systematic absences
       call CalcFamily(cell,ind,num,'r',itmp)
       rg(icnt)=ggl
       do i=1,num
        rfamily(icnt,i,1:3)=itmp(i,1:3)
        zr(itmp(i,1),itmp(i,2),itmp(i,3))=.TRUE.
       end do
! and put the intensity to a negative value
! that way we will know whether or not to draw them
       Vg(icnt)=-100.0
       Vgsave(icnt)=Vg(icnt)
      end if
! and increment the family counter
      rnumfam(icnt)=num
      totfam=totfam+num-1
      icnt=icnt+1
     end if 
    end if 
   end do
  end do
 end do

 icnt=icnt-1
! normalize potential coefficients to largest one
! and scale in a non-linear way to mimic density on 
! an electron micrograph [Gonzalez & Windtz]
! Use the where operator to avoid the negative intensities
 call ReadValue('logarithmic[0] or exponential[1] intensity scale ', io_int, 1)
 ll = io_int(1)
 if (ll.eq.0) then
  where(Vg.gt.0.0) Vg=0.01*alog(1.0+0.1*Vg)
 else
  where(Vg.gt.0.0) Vg=0.05*(Vg/Vmax)**0.2
 end if
 ricnt=icnt

! determine families of directions
 first = .TRUE.
 icnt = 1
 totfam=0
 do uu=-inm,inm
  do vv=-inm,inm
   do ww=-inm,inm
    if ((uu**2+vv**2+ww**2).ne.0) then
! make sure we have not already done this one in another family
     ind= (/ -uu, -vv, -ww /)
     call IndexReduce(ind)
     if (.not.z(ind(1),ind(2),ind(3))) then
! determine the family <uvw>
      call CalcFamily(cell,ind,num,'d',itmp)
! and keep only one family member, namely the one with the
! largest sum of the three integers, i.e. u+v+w
! [this is a simple way to get mostly positive indices as
! the zone axis indices]
      js = -100
      ii = 0
      do i=1,num
       hh = itmp(i,1)+itmp(i,2)+itmp(i,3)
       if (hh.gt.js) then 
        ii = i
        js = hh
       end if
! then remove the multiples of those direction indices from list 
       do m=-inm,inm
        ih=itmp(i,1)*m
        ik=itmp(i,2)*m
        il=itmp(i,3)*m
        if (((abs(ih).le.inm).and.(abs(ik).le.inm)).and.(abs(il).le.inm)) then 
         z(ih,ik,il)=.TRUE.
        end if
       end do
      end do
      family(icnt,1:3)=itmp(ii,1:3)
! increment family counter
      numfam(icnt)=num
      totfam=totfam+num-1
      icnt=icnt+1
     end if
    end if
   end do
  end do
 end do
 icnt=icnt-1
 io_int(1) = icnt
 call WriteValue('->Total number of direction families = ', io_int, 1)

! compute length of direction vectors and rank by increasing length
 allocate(idx(icnt))
 allocate(gg(icnt))
 gg(1:icnt) = 0.0
 do k=1,icnt
  g(1:3)=float(family(k,1:3))
  gg(k)=CalcLength(cell,g,'d')
 end do

! rank by increasing value of gg (use SLATEC routine)
 call SPSORT(gg,icnt,idx,1,ier)

! ask for number to be included in output
 call Message('List of available zone axis patterns', frm = "(A)")
 do i=1,icnt
  j=idx(i)
  io_int(1)=i
  do k=1,3
   io_int(k+1) = family(j,k)
  end do
  if (mod(i,4).eq.0) then 
   call WriteValue('', io_int,4,"(I3,' [',3I3,'];')")
  else
   call WriteValue('', io_int,4,"(I3,' [',3I3,'];')",advance="no")
  endif
 end do
 call Message('Enter selection (e.g. 4,10-20, ... ) ', frm = "(//,A)")
 call Message('[Include 0 to also draw a powder pattern] ', frm = "(A)")
 list = (/ (' ',j=1,256) /)
 call Message(' -> ', frm = "(A,' ')",advance="no")
 read (5,"(256A)") list
 call studylist(list,slect,fmax,ppat)

 if (cell%nonsymmorphic) then
  call Message('Potential double diffraction reflections will be indicated by open squares.', frm = "(A,/)")
 end if
 call ReadValue('No indices (0), labels (1), extinctions (2), labels + extinctions (3): ', io_int, 1)
 iref = io_int(1)

! and create output in 2 columns, 3 rows 
 do i=1,fmax  
  dpcnt=dpcnt+1
  j=idx(slect(i))
  imo = mod(i-1,6)
  if (imo.eq.0) then 
   np=.TRUE.
  else
   np=.FALSE.
  endif
  if (i.eq.1) then
   first=.TRUE.
  else
   first=.FALSE.
  endif
  if (slect(i).eq.0) then
   call Message('Creating Powder Pattern ', frm = "(A)")
   call DumpPP(PS,cell,xoff(imo),yoff(imo),np,laL,ricnt,Vgsave,rg,rnumfam)
   ppat=.FALSE.
  else
   io_int(1:3) = family(j,1:3)
   call WriteValue('Creating ZAP ', io_int,3, "('[',3i3,'] : ')",advance="no")
   call DumpZAP(PS,cell,xoff(imo),yoff(imo),family(j,1),family(j,2),family(j,3),numfam(j),np,first,iref,laL,ricnt,dbdiff, &
        Vg, Vgsave, rg, rfamily, rnumfam, hhcc)
  endif
 end do

! and clean up all variables
 deallocate(zr,z,Vg, Vgsave, rfamily, rnumfam, rg, family, numfam, idx, gg)
 if (cell%nonsymmorphic) deallocate(dbdiff)

end subroutine DiffPage

!--------------------------------------------------------------------------
!
! SUBROUTINE: DumpZAP
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a single zone axis diffraction pattern
!
!> @param PS Postscript structure
!> @param cell unit cell pointer
!> @param xo lower left x position
!> @param yo lower left y position
!> @param u direction index u
!> @param v direction index v
!> @param w direction index w
!> @param p ??
!> @param np logical (is this a new page?)
!> @param first logical
!> @param indi 
!> @param laL camera length
!> @param icnt counter
!> @param dbdiff double diffraction logical array
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date  03/26/13  MDG 3.0 updated IO
!> @date  06/09/14  MDG 4.0 added PS argument
!--------------------------------------------------------------------------
recursive subroutine DumpZAP(PS,cell,xo,yo,u,v,w,p,np,first,indi,laL,icnt,dbdiff,Vg,Vgsave,rg,rfamily,rnumfam,hhcc)
!DEC$ ATTRIBUTES DLLEXPORT :: DumpZAP

use io
use postscript
use crystal
use error

IMPLICIT NONE

type(postscript_type),INTENT(INOUT) :: PS
!f2py intent(in,out) ::  PS
type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: xo, yo               !< lower left position
integer(kind=irg),INTENT(IN)    :: u, v, w              !< zone axis components
integer(kind=irg),INTENT(IN)    :: p                    !< ??
logical,INTENT(IN)              :: np                   !< logical for new page
logical,INTENT(IN)              :: first                !< logical
integer(kind=irg),INTENT(IN)    :: indi                 !< ??
real(kind=sgl),INTENT(IN)       :: laL                  !< camera length
integer(kind=irg),INTENT(IN)    :: icnt                 !< counter
logical,INTENT(IN)              :: dbdiff(icnt)         !< array to deal with double diffraction spots
real(kind=sgl),INTENT(IN)       :: Vg(*),rg(*),Vgsave(*)
integer(kind=irg),INTENT(IN)    :: hhcc
integer(kind=irg),INTENT(IN)    :: rfamily(hhcc,48,3),rnumfam(*)

! nref is the anticipated maximum number of reflections per pattern
integer(kind=irg),parameter     :: nref = 2000
integer(kind=irg)               :: dp,i,j,jcnt,ui,vi,wi,pp,locg(nref,3),ier, io_int(1)
integer(kind=irg),allocatable   :: idx(:)
real(kind=sgl)                  :: sc,gmax,leng(nref),PX,PY,qx,qy,locv(nref),locvsave(nref),t(3),c(3),gg(3),gx(3),gy(3)
real(kind=sgl),allocatable      :: lng(:)
real(kind=sgl),parameter        :: le=3.25,he=2.9375,thr=1.0E-4
logical                         :: dbd(nref)

! do page preamble stuff if this is a new page
! [This assumes that the PostScript file has already been opened]
 if (np) then
  call PS_newpage(PS,.FALSE.,'Kinematical Zone Axis Patterns')
  call PS_text(5.25,-0.05,'scale bar in reciprocal nm')
  gmax = laL
  call PS_textvar(5.25,PS % psfigheight+0.02,'Camera Constant [nm mm]',gmax)
  call PS_setfont(PSfonts(2),0.15)
  call PS_text(-0.25,PS % psfigheight+0.02,'Structure File : '//cell % fname)
 end if

! draw frame and related stuff
 call PS_setlinewidth(0.012)
 call PS_balloon(xo,yo,le,he,0.0312)
 call PS_setlinewidth(0.001)
 PX = xo+1.8125
 PY = yo+1.0+15.0/32.0
 call PS_circle(PX,PY,1.375)

! zone axis
 call PS_setfont(PSfonts(2),0.12)
 call PS_text(xo+0.05,yo+he-0.15,'Zone axis ')
 ui=u
 vi=v
 wi=w
 call PrintIndices('d',cell%hexset,ui,vi,wi,xo+0.6,yo+he-0.15)

! multiplicity
 call PS_setfont(PSfonts(2),0.12)
 pp=p
 call PS_textint(xo+0.05,yo+he-0.30,'Multiplicity ',pp)

! scale bar (sc is the conversion factor from nm-1 to inches)
 sc = laL/25.4
 call PS_setlinewidth(0.020)
 call PS_line(xo+0.05,yo+0.06,xo+0.05+5.0*sc,yo+0.06)
 call PS_setfont(PSfonts(2),0.15)
 call PS_text(xo+0.05+2.5*sc,yo+0.10,'5 ')

! select all reflections belonging to this zone axis pattern
 leng(1:nref)=0.0
 jcnt=0
 do i=1,icnt
  do j=1,rnumfam(i)
   dp=u*rfamily(i,j,1)+v*rfamily(i,j,2)+w*rfamily(i,j,3)
   if (dp.eq.0) then
    jcnt=jcnt+1
    if (jcnt.gt.nref) call FatalError('DumpZAP ',' too many reflections (<2000)' )
    locg(jcnt,1:3)=rfamily(i,j,1:3)
    leng(jcnt)=rg(i)
    locv(jcnt)=Vg(i)
    locvsave(jcnt)=Vgsave(i)
    dbd(jcnt)=.FALSE.
! take care of potential double diffraction reflections
    if ((cell%nonsymmorphic).and.(dbdiff(i))) dbd(jcnt) = .TRUE.
   end if
  end do
 end do

! rank them by length (use SLATEC routine)
 allocate(idx(jcnt))
 allocate(lng(jcnt))
 lng(1:jcnt) = leng(1:jcnt)
 call SPSORT(lng,jcnt,idx,1,ier)
 io_int(1) = jcnt
 call WriteValue(' Number of reflections : ', io_int, 1)

! normalize the zone axis in cartesian components; this is the z-axis
 t(1)=-float(u)
 t(2)=-float(v)
 t(3)=-float(w)
 call TransSpace(cell,t,c,'d','c')
 call NormVec(cell,c,'c')

! take the first reflection in the list and make that the x-axis
! skip the zero reflection !!
 j=idx(2)

! normalize the first reciprocal vector in cartesian components
! this will be the x-axis of the diffraction pattern
 gg(1:3)=float(locg(j,1:3))
 call TransSpace(cell,gg,gx,'r','c')
 call NormVec(cell,gx,'c')

! then get the cross product between t and g; this is the y-axis
 call CalcCross(cell,c,gx,gy,'c','c',0)

! plot origin of reciprocal space 
 call PS_filledcircle(PX,PY,0.05,0.0)

! then plot the remaining reflections
 do i=1,jcnt
  j=idx(i)
  gg(1:3)=float(locg(j,1:3))
  call TransSpace(cell,gg,c,'r','c')
  qx=PX-CalcDot(cell,c,gx,'c')*sc
  qy=PY+CalcDot(cell,c,gy,'c')*sc

! first check for systematic absence due to lattice centering
  if ((locvsave(j).eq.-100.0).and.(indi.ge.2)) then
    call PS_cross(qx,qy,0.03,0.001)
  end if

! could it be a double diffraction spot ?
  if ((cell%nonsymmorphic).and.(dbd(j))) call PS_square(qx,qy,0.04)

! is it a regular reflection ?
  if (locv(j).ge.thr) then
   call PS_filledcircle(qx,qy,locv(j),0.0)
   if ((indi.eq.1).or.(indi.eq.3)) then 
    call Printhkl(qx,qy,locg(j,1),locg(j,2),locg(j,3))
   end if
  end if

 end do

 deallocate(idx, lng)
 
end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: DumpPP
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief draw a kinematical powder pattern
!
!> @param PS Postscript structure
!> @param cell unit cell pointer
!> @param xo lower left x position
!> @param yo lower left y position
!> @param np logical (is this a new page?)
!> @param laL camera length
!> @param icnt counter
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date  03/26/13  MDG 3.0 updated IO
!> @date  06/09/14  MDG 4.0 added PS, cell as arguments
!--------------------------------------------------------------------------
recursive subroutine DumpPP(PS,cell,xo,yo,np,laL,icnt,Vgsave,rg,rnumfam)
!DEC$ ATTRIBUTES DLLEXPORT :: DumpPP

use postscript

IMPLICIT NONE 

type(postscript_type),INTENT(INOUT) :: PS
!f2py intent(in,out) ::  PS
type(unitcell)                  :: cell
real(kind=sgl),INTENT(IN)       :: xo, yo               !< lower left position
logical,INTENT(IN)              :: np                   !< logical for new page
real(kind=sgl),INTENT(IN)       :: laL                  !< camera length
integer(kind=irg),INTENT(IN)    :: icnt         !< counter
real(kind=sgl),INTENT(IN)       :: rg(*),Vgsave(*)
integer(kind=irg),INTENT(IN)    :: rnumfam(*)

! nref = max number of rings
integer(kind=irg),parameter     :: nref = 500
integer(kind=irg)               :: i,j
real(kind=sgl)                  :: sc,gmax,leng(nref),PX,PY,locv(nref),grad,w,Vmax
real(kind=sgl),parameter        :: le=3.25,he=2.9375,thr=1.0E-4

! do page preamble stuff if this is a new page
! [This assumes that the PostScript file has already been opened]
 if (np) then
  call PS_newpage(PS,.FALSE.,'Kinematical Zone Axis Patterns')
  call PS_text(5.25,-0.05,'scale bar in reciprocal nm')
  gmax = laL
  call PS_textvar(5.25,PS % psfigheight+0.02,'Camera Constant [nm mm]',gmax)
  call PS_setfont(PSfonts(2),0.15)
  call PS_text(-0.25,PS % psfigheight+0.02,'Structure File : '//cell % fname)
 end if

! draw frame and related stuff
 call PS_setlinewidth(0.012)
 call PS_balloon(xo,yo,le,he,0.0312)
 call PS_setlinewidth(0.001)
 PX = xo+1.8125
 PY = yo+1.0+15.0/32.0
 call PS_circle(PX,PY,1.375)

! frame title
 call PS_setfont(PSfonts(2),0.12)
 call PS_text(xo+0.05,yo+he-0.15,'Powder Pattern')

! scale bar (sc is the conversion factor from nm-1 to inches)
 sc = laL/25.4
 call PS_setlinewidth(0.020)
 call PS_line(xo+0.05,yo+0.06,xo+0.05+5.0*sc,yo+0.06)
 call PS_setfont(PSfonts(2),0.15)
 call PS_text(xo+0.05+2.5*sc,yo+0.10,'5 ')

! scale all reflection intensities by the multiplicity
 leng(1:nref)=0.0
 Vmax = 0.0
 do i=1,icnt-1
  leng(i)=rg(i)
  locv(i)=Vgsave(i)*rnumfam(i)
  if (locv(i).gt.Vmax) Vmax=locv(i)
 end do

! plot origin of reciprocal space 
 call PS_filledcircle(PX,PY,0.05,0.0)

! then plot the diffraction circles 
 do i=1,icnt
  j=icnt+1-i
! get the circle radius and intensity
  grad = leng(j)*sc
  w = locv(j)*0.03/Vmax
! draw circle if radius fits in drawing frame and intensity large enough
  if ((w.gt.0.0001).AND.(grad.le.1.375)) then 
   call PS_setlinewidth(w)
   call PS_circle(PX,PY,grad)
  end if
 end do

end subroutine

!--------------------------------------------------------------------------
!
! SUBROUTINE: studylist
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief analyze reflection input list
!
!> @details determine which zone axis pattern to draw from user input
!> the parameter ppat is returned as true if the user also requested a powder pattern
!
!> @param list input string
!> @param slect list of patterns
!> @param np number of patterns
!> @param ppat draw a powder pattern?
!
!> @date   10/20/98 MDG 1.0 original
!> @date    5/22/01 MDG 2.0 f90
!> @date   11/27/01 MDG 2.1 added kind support
!> @date   03/26/13 MDG 3.0 updated IO
!--------------------------------------------------------------------------
recursive subroutine studylist(list,slect,np,ppat)
!DEC$ ATTRIBUTES DLLEXPORT :: studylist

IMPLICIT NONE

character(1),INTENT(IN)                 :: list(256)            !< input string
integer(kind=irg),INTENT(OUT)           :: slect(256)           !< list of patterns to be drawn
integer(kind=irg),INTENT(OUT)           :: np                           !< number of patterns
logical,INTENT(INOUT)                   :: ppat                 !< powder pattern included ?
!f2py intent(in,out) ::  ppat                 !< powder pattern included ?

integer(kind=irg)                       :: comma(100),hyphen(100),ccnt,hcnt,i,j,k,ip,icnt,nd,n,istart,istop
integer(kind=irg),parameter             :: nmb(48:57)=(/0,1,2,3,4,5,6,7,8,9/)

! initialize a few parameters
 ccnt = 0
 hcnt = 0
 ppat = .FALSE.
 slect = 0
 comma = 0
 hyphen= 0
 j = 0

! count characters and search for , and -
 do i=1,256
  if (list(i)(1:1).ne.' ') j=j+1
  if (list(i)(1:1).eq.',') then 
   ccnt = ccnt+1
   comma(ccnt)=i
  end if
  if (list(i)(1:1).eq.'-') then 
   hcnt = hcnt+1
   hyphen(hcnt)=i
  end if
 end do 
 ccnt = ccnt+1
 comma(ccnt) = j+1

! interpret the string
 j = 1
 ip = 1
 icnt = 0
 do i=1,ccnt
! is it a range ?
  if (((hyphen(j).lt.comma(i)).and.(hcnt.gt.0)).and.(j.le.hcnt)) then

! yes, it is;  get the first number
   nd = hyphen(j)-ip
   n = 0
   do k=0,nd-1
    n = 10*n+nmb(ichar(list(ip+k)(1:1)))
   end do
   istart = n
   ip = hyphen(j)+1

! and then the second number
   nd = comma(i)-ip
   n = 0
   do k=0,nd-1
    n = 10*n+nmb(ichar(list(ip+k)(1:1)))
   end do
   istop = n

! and fill in the entire range
   do k=istart,istop
    icnt=icnt+1
    slect(icnt)=k
    if (k.eq.0) then
     ppat = .TRUE.
    end if
   end do
   ip = comma(i)+1
   j=j+1
  else

! no, it is not a range; determine number of digits
   nd = comma(i)-ip
   n = 0
   do k=0,nd-1
    n = 10*n+nmb(ichar(list(ip+k)(1:1)))
   end do
   icnt=icnt+1
   slect(icnt)=n
   if (n.eq.0) then
    ppat = .TRUE.
   end if
   ip = comma(i)+1
  end if
 end do 
 np = icnt
 
end subroutine



!--------------------------------------------------------------------------
!
! SUBROUTINE: BWsolve
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Bloch wave solver routine
!
!> @details Solve the Bloch wave eigenvalue equation for the 
!> N-beam case, using the ZGEEV LAPACK 3.0 routine
!
!> @param M dynamical matrix
!> @param W eigenvalues
!> @param CGG eigenvectors
!> @param CGinv inverse of eigenvector matrix
!> @param nn number of beams
!> @param IPIV pivot array, currently unused
!> @param keeporder optional legacy parameter; should be removed
!
!> @note This routine has seen a lot of modifications and is probably not in
!> its final version yet...  A lot of the original comments have been removed
!> as they were starting to be confusing.
!
!> @todo This routine needs to be thoroughly debugged and validated ...
!
!> @date  10/20/98 MDG 1.0 original
!> @date  05/22/01 MDG 2.0 f90
!> @date  11/27/01 MDG 2.1 added kind support
!> @date  08/09/10 MDG 2.2  rewritten to include both ZGEEV and ZGEES/ZTREVC
!> @date  03/26/13 MDG 3.0 clean up of old comments
!> @date  06/15/14 MDG 4.0 updated for removal of all globals
!--------------------------------------------------------------------------
recursive subroutine BWsolve(M,W,CGG,CGinv,nn,IPIV)
!DEC$ ATTRIBUTES DLLEXPORT :: BWsolve

use local
use error
use io

IMPLICIT NONE

integer(kind=irg),INTENT(IN)    :: nn           !< number of beams
complex(kind=dbl),INTENT(IN)    :: M(nn,nn)     !< input dynamical matrix
complex(kind=dbl),INTENT(OUT)   :: W(nn)        !< Bloch eigenvalues
complex(kind=dbl),INTENT(OUT)   :: CGG(nn,nn)   !< Bloch eigenvectors
complex(kind=dbl),INTENT(OUT)   :: CGinv(nn,nn) !< inverse of eigenvector array
integer(kind=irg),INTENT(IN)    :: IPIV(nn)     !< pivot array, currently unused
!logical,INTENT(IN),OPTIONAL     :: keeporder    !< optional legacy parameter, should be removed

integer(kind=irg)               :: INFO, LDA, LDVR, LDVL, LWORK, JPIV(nn),MILWORK, i, io_int(1)
integer(kind=irg),parameter     :: LWMAX = 5000 
complex(kind=dbl)               :: VL(nn,nn),  WORK(LWMAX), normsum
real(kind=dbl)                  :: RWORK(2*nn), io_real(1)
character                       :: JOBVL, JOBVR
complex(kind=dbl),allocatable   :: MIWORK(:)

! set some initial LAPACK variables 
 LDA = nn
 LDVL = nn
 LDVR = nn
 INFO = 0
 
! first initialize the parameters for the LAPACK ZGEEV, CGETRF, and CGETRI routines
 JOBVL = 'N'   ! do not compute the left eigenvectors
 JOBVR = 'V'   ! do compute the right eigenvectors
 LWORK = -1    ! so that we can ask the routine for the actually needed value

! call the routine to determine the optimal workspace size
  call zgeev(JOBVL,JOBVR,nn,M,LDA,W,VL,LDVL,CGG,LDVR,WORK,LWORK,RWORK,INFO)
  LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )

! then call the eigenvalue solver
  call zgeev(JOBVL,JOBVR,nn,M,LDA,W,VL,LDVL,CGG,LDVR,WORK,LWORK,RWORK,INFO)
  if (INFO.ne.0) call FatalError('Error in BWsolve: ','ZGEEV return not zero')

! it appears that the eigenvectors may not always be normalized ...
! so we renormalize them here...
! do i=1,nn
!   normsum = sum(abs(CGG(1:nn,i))**2)
!   normsum = cmplx(1.0,0.0,dbl)/sqrt(normsum)
!   CGG(1:nn,i) = CGG(1:nn,i)*normsum
! end do

! make a copy of CG for the matrix inversion routines
 CGinv = CGG

! invert CGinv to get the Bloch wave excitation amplitudes 
 LDA = nn
 call zgetrf(nn,nn,CGinv,LDA,JPIV,INFO)
 if (INFO.ne.0) then
  io_int(1) = INFO
  call WriteValue('zgetrf error code: ',io_int,1,frm="(I5)")
  call FatalError('Error in BWsolve: ','ZGETRF return not zero')
 end if

 MILWORK = 64*nn 
 allocate(MIWORK(MILWORK))

 MIWORK = cmplx(0.0_dbl,0.0_dbl)
 call zgetri(nn,CGinv,LDA,JPIV,MIWORK,MILWORK,INFO)
 if (INFO.ne.0) then
  io_int(1) = INFO
  call WriteValue('zgetri error code: ',io_int,1,frm="(I5)")
  call FatalError('Error in BWsolve: ','ZGETRI return not zero')
 end if

! if ((abs(sum(matmul(CGG,CGinv)))-dble(nn)).gt.1.E-8) then
!  call Message('Error in matrix inversion; continuing', frm = "(A)")
!  io_real(1) = abs(sum(matmul(CGG,CGinv)))-dble(nn)
!  call WriteValue('   Matrix inversion error; this number should be zero: ',io_real,1,"(F)")
! endif
  
 deallocate(MIWORK)
 
end subroutine BWsolve

!
!
!
!subroutine BWsolve_test(M,W,CG,CGinv,nn,IPIV)
!
!use local
!use error
!
!IMPLICIT NONE
!
!integer    :: INFO, LDA, LDVR, LDVL, LWORK, nn, IPIV(nn), JPIV(nn), i, j, MILWORK, IERR
!complex    :: M(nn,nn), CG(nn,nn), W(nn),  &
!              MIWORK(nn), CGinv(nn,nn), cW(nn)
!
!real,allocatable :: rW(:),WR(:),WI(:),CCR(:,:),CCI(:,:)
!character  :: JOBVL, JOBVR, BALANC, SENSE
!
!intent(IN)  :: nn,M
!intent(OUT) :: W,CG,CGinv,IPIV
!
! first initialize the parameters for the LAPACK CGEEV, CGETRF,
! and CGETRI routines
 !BALANC = 'B'  ! balance and diagonal scaling
 !JOBVL = 'V'   ! compute the left eigenvectors
 !JOBVR = 'V'   ! compute the right eigenvectors
 !SENSE = 'B'   ! reciprocal condition numbers
! LDA = nn
! LWORK = 2*(nn**2+2*nn)
! LDVL = nn
! LDVR = nn
! MILWORK = nn
! then call the eispack eigenvalue solver
! call CGEEVX(BALANC,JOBVL,JOBVR,SENSE,nn,M,LDA,W,VL,LDVL,CG,LDVR, &
!             ILO,IHI,SCLE,ABNRM,RCONDE,RCONDV,WORK,LWORK,RWORK,INFO)
! allocate(WR(nn),WI(nn),CCR(nn,nn),CCI(nn,nn)) 
! CALL EISPACK(nn,nn,real(M),aimag(M),WR,WI,CCR,CCI,IERR)
! W = cmplx(WR,WI)
! CG = cmplx(CCR,CCI)
!
!if (INFO.ne.0) call FatalError('Error in BWsolve: ','CGEEV return not zero')
!  do i=1,nn
!    write (15,*) W(i)
!  end do
!  write (15,*) '----'
!  do i=1,nn
!   do j=1,nn
!    write (15,*) CG(i,j)
!   end do
!  write (15,*) '--'
! end do
!
!e = slamch('E')
!write (*,*) 'SLAMCH(E) = ',e
!do i=1,nn
! write (*,*) i,real(W(i)),e*ABNRM/RCONDE(i),e*ABNRM/RCONDV(i)
!end do
!
! rank the eigenvalues from largest (most positive) to smallest
! (most negative) (use the real part for the ranking); 
! cW = W
! allocate(rW(nn))
! rW = real(W)
! IPIV = 0
! JPIV = 0
! call SPSORT(rW,nn,IPIV,-1,INFO)
! do i=1,nn
!  W(i) = cW(IPIV(i))
! end do
! CGinv = CG
! do i=1,nn  ! row index
!  do j=1,nn ! column index
!   CG(i,j) = CGinv(i,IPIV(j))
!  end do
! end do 
! make a new copy of CG for the inversion routines
! CGinv = CG
! invert CGinv to get the Bloch wave excitation amplitudes 
! call CGETRF(nn,nn,CGinv,LDA,JPIV,INFO)
! if (INFO.ne.0) call FatalError('Error in BWsolve: ','CGETRF return not zero')
!write (*,*) 'CGETRF INFO = ',INFO
! call CGETRI(nn,CGinv,LDA,JPIV,MIWORK,MILWORK,INFO)
! if (INFO.ne.0) call FatalError('Error in BWsolve: ','CGETRI return not zero')
!write (*,*) 'CGETRI INFO = ',INFO
!end subroutine
!


!
! ###################################################################
! 
!  subroutine CalcFresnelPropagator
!
!                                    created: 4/16/97
!  Author: Marc De Graef
!  
!  Description: compute the Fresnel propagator (for possibly inclined
!               illumination) and store it in a file
! 
!  History
! 
!  modified by  rev reason
!  -------- --- --- -----------
!   4/16/97 MDG 1.0 original
!   9/29/01 MDG 2.0 f90
!  11/27/01 MDG 2.1 added kind support
! ###################################################################
recursive subroutine CalcFresnelPropagator(beam,dimi,dimj,dz,scl,propname,lambda)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcFresnelPropagator

use constants
use io
use files

IMPLICIT NONE

real(kind=sgl)                  :: beam(3),b,bm(2),dz,fidim,fjdim,prefac,scl, oi_real(2), lambda
real(kind=sgl),allocatable      :: idimi(:),jdimj(:)
complex(kind=sgl),allocatable   :: fr(:,:)
integer(kind=irg)               :: dimi,dimj,i,ix,iy
character(fnlen)                   :: propname

INTENT(IN) :: beam,dimi,dimj,dz

  fidim = 1.0/float(dimi)
  fjdim = 1.0/float(dimj)
  prefac = scl*cPi*lambda*dz
  call Message('Computing Fresnel propagator', frm = "(A)")
! normalize the incident beam direction and rescale to the wavevector
  b = sqrt(sum(beam**2))
  bm= beam(1:2)/b/lambda
  oi_real(1:2) = bm(1:2) 
  call WriteValue(' Laue center at ', oi_real, 2, "(F8.4,',',F8.4)")
! allocate variables
  allocate(fr(dimi,dimj))
  allocate(idimi(dimi),jdimj(dimj))
  idimi = float((/ (i, i=0,dimi-1) /))
  jdimj = float((/ (i, i=0,dimj-1) /))
!
  where(idimi.ge.dimi/2) idimi = idimi-float(dimi)
  where(jdimj.ge.dimj/2) jdimj = jdimj-float(dimj)
!
  idimi = prefac*idimi*fidim
  jdimj = prefac*jdimj*fjdim
!
  idimi = idimi*(idimi + 2.0*bm(1))
  jdimj = jdimj*(jdimj + 2.0*bm(2))
! loop over y axis  
  do iy=1,dimj
! loop over x-axis
    do ix=1,dimi
      fr(ix,iy)=cmplx(cos(idimi(ix)+jdimj(iy)),sin(idimi(ix)+jdimj(iy)))
    end do
  end do
  deallocate(idimi,jdimj)
! and store it in a file
  open(unit=dataunit,file=trim(EMsoft_toNativePath(propname)),form='unformatted')
  write (dataunit) dimi,dimj
  write (dataunit) fr
  close(unit=dataunit,status='keep')
  deallocate(fr)
end subroutine

end module diffraction
