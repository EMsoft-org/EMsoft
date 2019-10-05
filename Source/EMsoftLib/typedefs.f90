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
! EMsoft:typedefs.f90
!--------------------------------------------------------------------------
!
! MODULE: typedefs
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Definition of all variables and types for crystallographic computations
!
!> @details  Defines the unitcell type and the orientation type, as well as 
!> the main cell variable used by all crystallographic computations
! 
!> @note mod 5.3: f90wrap does not like arrays of parameters (constants), so we removed the 'parameter' attribute
!>  in all those cases... we should trust that the user will not modify the values in these arrays...
!
!> @date     1/5/99 MDG 1.0 original
!> @date    7/16/99 MDG 1.1 added error handling and TransCoor
!> @date    4/ 5/00 MDG 1.2 modified TransCoor to include new mInvert
!> @date    5/19/01 MDG 2.0 f90 version
!> @date   03/19/13 MDG 3.0 update to new version
!> @date   10/17/13 MDG 3.1 added HOLZentries type
!> @date    1/10/14 MDG 4.0 new version, many new entries in unitcell type
!> @date    6/ 5/14 MDG 4.1 removed variable declaration for cell
!> @date    6/ 9/14 MDG 4.2 added all defect type declarations
!> @date    8/11/14 MDG 4.3 modified Rodrigues vector to 4 components
!> @date   12/02/14 MDG 4.4 added a few entries to unitcell pointer
!> @date   08/25/15 MDG 4.5 added PGSamplingType conversion array for master pattern computations
!> @date   08/27/15 MDG 4.6 added component to kvectorlist
!> @date   08/30/15 MDG 4.7 added trigmat to unitcell type
!> @date   09/08/15 MDG 4.8 added LUTqg to cell
!> @date   12/11/15 MDG 4.9 added Einclusion (Eshelby isotropic inclusion) defect type
!> @date   01/11/15 MDG 5.0 moved HDFobjectStackType to HDFSupport module
!> @date   05/02/16 MDG 5.1 added SghLUT to unitcell type
!> @date   05/07/18 MDG 5.2 added CSL types
!> @date   09/01/19 MDG 5.3 changed parameter arrays to regular array for compatibility with f90wrap python wrapping
!--------------------------------------------------------------------------
module typedefs

use local
use, intrinsic :: iso_c_binding


! following are used to define the quaternion symmetry operators
real(kind=dbl),private,parameter        :: sq22=0.7071067811865475244D0 ! sqrt(2)/2
real(kind=dbl),private,parameter        :: sq32=0.8660254037844386467D0 ! sqrt(3)/2
real(kind=dbl),private,parameter        :: half=0.5D0                   ! 1/2
!DEC$ ATTRIBUTES DLLEXPORT :: sq22
!DEC$ ATTRIBUTES DLLEXPORT :: sq32
!DEC$ ATTRIBUTES DLLEXPORT :: half

!> Maximum number of positions in asymmetric unit
  integer(kind=irg), parameter          :: maxpasym = 250   
!DEC$ ATTRIBUTES DLLEXPORT :: maxpasym
!> Maximum number of defects of any given type
  integer(kind=irg), parameter          :: maxdefects = 250
!DEC$ ATTRIBUTES DLLEXPORT :: maxdefects

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! this used to be the symmetryvars module 
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! MODULE: symmetryvars
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief all symmetry and related variable definitions
!
!> @details  contains a list of space group names and generator strings,
!> numbering according to International Tables for Crystallography
!> [hexagonal groups in rhombohedral setting
!> are described in locations 231-237]
!> 
!> The space group information is encoded in the following way:
!> From the International Crystallographic Tables one finds that
!> the point symmetry parts of the space group generators are 
!> represented by 14 out of 71 different matrices.  When expressed 
!> in the crystallographic reference frame, those matrices contain 
!> only the entries 1, 0, and -1.  
!> 
!> The 14 matrices are represented by the lower case 
!> letters a through n. (see program and Appendix A3, page 666, for details)
!> The translational parts of the space group generators are limited
!> to the following set (encoded with upper-case letters):
!> 
!> A             1/6
!> B             1/4
!> C             1/3
!> D             1/2
!> E             2/3
!> F             3/4
!> G             5/6
!> O             0
!> X             -3/8
!> Y             -1/4
!> Z             -1/8
!>
!> In encoding the space groups we have selected the [unique axis b, 
!> cell choice 1] settings for the monoclinic point groups.
!> The first regular space group with a multiple origin choice is 
!> No. 48 (Pnnn).  
!>
!> <b> WARNING FOR  THE USER: </b> This is a very tricky file!  Make sure that you 
!> understand all the details before you attempt to change anything!!!
! 
!> @note  9/25/2011: corrected the generators for space groups 39, 90, 107, 108, 131, 214, and 222.\n
!> [Thank you Matthew O'Brien for pointing out the errors !]\n
!> [Thank you also to Marco Schowalter for pointing out an error in space group 222; corrected on 9/24/2012]

!> @date  1/5/99  MDG 1.0 original
!> @date  5/19/01 MDG 2.0 f90
!> @date 11/27/01 MDG 2.1 added kind support
!> @date 03/19/13 MDG 3.0 updated file
!> @date 01/10/14 MDG 4.0 new version
!--------------------------------------------------------------------------

!>  SYM_SGname all space group names
! TRICLINIC SPACE GROUPS
character(11),dimension(237) :: SYM_SGname= (/" P  1      " ," P -1      ", & ! MONOCLINIC SPACE GROUPS
        " P 2       " ," P 21      " ," C 2       " ," P m       ", &
        " P c       " ," C m       " ," C c       " ," P 2/m     ", &
        " P 21/m    " ," C 2/m     " ," P 2/c     " ," P 21/c    ", &
        " C 2/c     ", &                                              ! ORTHORHOMBIC SPACE GROUPS
        " P 2 2 2   " ," P 2 2 21  " ," P 21 21 2 " ," P 21 21 21", &
        " C 2 2 21  " ," C 2 2 2   " ," F 2 2 2   " ," I 2 2 2   ", &
        " I 21 21 21" ," P m m 2   " ," P m c 21  " ," P c c 2   ", &
        " P m a 2   " ," P c a 21  " ," P n c 2   " ," P m n 21  ", &
        " P b a 2   " ," P n a 21  " ," P n n 2   " ," C m m 2   ", &
        " C m c 21  " ," C c c 2   " ," A m m 2   " ," A b m 2   ", &
        " A m a 2   " ," A b a 2   " ," F m m 2   " ," F d d 2   ", &
        " I m m 2   " ," I b a 2   " ," I m a 2   " ," P m m m   ", &
        " P n n n   " ," P c c m   " ," P b a n   " ," P m m a   ", &
        " P n n a   " ," P m n a   " ," P c c a   " ," P b a m   ", &
        " P c c n   " ," P b c m   " ," P n n m   " ," P m m n   ", &
        " P b c n   " ," P b c a   " ," P n m a   " ," C m c m   ", &
        " C m c a   " ," C m m m   " ," C c c m   " ," C m m a   ", &
        " C c c a   " ," F m m m   " ," F d d d   " ," I m m m   ", &
        " I b a m   " ," I b c a   " ," I m m a   ", &                ! TETRAGONAL SPACE GROUPS  
        " P 4       " ," P 41      " ," P 42      " ," P 43      ", &
        " I 4       " ," I 41      " ," P -4      " ," I -4      ", &
        " P 4/m     " ," P 42/m    " ," P 4/n     " ," P 42/n    ", &
        " I 4/m     " ," I 41/a    " ," P 4 2 2   " ," P 4 21 2  ", &
        " P 41 2 2  " ," P 41 21 2 " ," P 42 2 2  " ," P 42 21 2 ", &
        " P 43 2 2  " ," P 43 21 2 " ," I 4 2 2   " ," I 41 2 2  ", &
        " P 4 m m   " ," P 4 b m   " ," P 42 c m  " ," P 42 n m  ", &
        " P 4 c c   " ," P 4 n c   " ," P 42 m c  " ," P 42 b c  ", &
        " I 4 m m   " ," I 4 c m   " ," I 41 m d  " ," I 41 c d  ", &
        " P -4 2 m  " ," P -4 2 c  " ," P -4 21 m " ," P -4 21 c ", &
        " P -4 m 2  " ," P -4 c 2  " ," P -4 b 2  " ," P -4 n 2  ", &
        " I -4 m 2  " ," I -4 c 2  " ," I -4 2 m  " ," I -4 2 d  ", &
        " P 4/m m m " ," P 4/m c c " ," P 4/n b m " ," P 4/n n c ", &
        " P 4/m b m " ," P 4/m n c " ," P 4/n m m " ," P 4/n c c ", &
        " P 42/m m c" ," P 42/m c m" ," P 42/n b c" ," P 42/n n m", &
        " P 42/m b c" ," P 42/m n m" ," P 42/n m c" ," P 42/n c m", &
        " I 4/m m m " ," I 4/m c m " ," I 41/a m d" ," I 41/a c d", & ! RHOMBOHEDRAL SPACE GROUPS  
        " P 3       " ," P 31      " ," P 32      " ," R 3       ", &
        " P -3      " ," R -3      " ," P 3 1 2   " ," P 3 2 1   ", &
        " P 31 1 2  " ," P 31 2 1  " ," P 32 1 2  " ," P 32 2 1  ", &
        " R 3 2     " ," P 3 m 1   " ," P 3 1 m   " ," P 3 c 1   ", &
        " P 3 1 c   " ," R 3 m     " ," R 3 c     " ," P -3 1 m  ", &
        " P -3 1 c  " ," P -3 m 1  " ," P -3 c 1  " ," R -3 m    ", &
        " R -3 c    ", &                                              ! HEXAGONAL SPACE GROUPS   
        " P 6       " ," P 61      " ," P 65      " ," P 62      ", &
        " P 64      " ," P 63      " ," P -6      " ," P 6/m     ", &
        " P 63/m    " ," P 6 2 2   " ," P 61 2 2  " ," P 65 2 2  ", &
        " P 62 2 2  " ," P 64 2 2  " ," P 63 2 2  " ," P 6 m m   ", &
        " P 6 c c   " ," P 63 c m  " ," P 63 m c  " ," P -6 m 2  ", &
        " P -6 c 2  " ," P -6 2 m  " ," P -6 2 c  " ," P 6/m m m ", &
        " P 6/m c c " ," P 63/m c m" ," P 63/m m c", &                ! CUBIC SPACE GROUPS
        " P 2 3     " ," F 2 3     " ," I 2 3     " ," P 21 3    ", &
        " I 21 3    " ," P m 3     " ," P n 3     " ," F m 3     ", &
        " F d 3     " ," I m 3     " ," P a 3     " ," I a 3     ", &
        " P 4 3 2   " ," P 42 3 2  " ," F 4 3 2   " ," F 41 3 2  ", &
        " I 4 3 2   " ," P 43 3 2  " ," P 41 3 2  " ," I 41 3 2  ", &
        " P -4 3 m  " ," F -4 3 m  " ," I -4 3 m  " ," P -4 3 n  ", &
        " F -4 3 c  " ," I -4 3 d  " ," P m 3 m   " ," P n 3 n   ", &
        " P m 3 n   " ," P n 3 m   " ," F m 3 m   " ," F m 3 c   ", &
        " F d 3 m   " ," F d 3 c   " ," I m 3 m   " ," I a 3 d   ", & ! TRIGONAL GROUPS RHOMBOHEDRAL SETTING
        " R 3   |146" ," R -3  |148" ," R 3 2 |155" ," R 3 m |160", &
        " R 3 c |161" ," R -3 m|166" ," R -3 c|167"/)
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_SGname

!> extended Hermann-Mauguin symbols for the orthorhombic space groups in the following settings:
character(8), dimension(6):: extendedOrthsettings = (/ &
    " a  b  c", " b  a -c", " c  a  b", "-c  b  a", " b  c  a", " a -c  b"  /)
!DEC$ ATTRIBUTES DLLEXPORT :: extendedOrthsettings

character(11), dimension(6,59) :: extendedHMOrthsymbols = reshape( (/ &
    " P 2 2 2   ", " P 2 2 2   ", " P 2 2 2   ", " P 2 2 2   ", " P 2 2 2   ", " P 2 2 2   ", &
    " P 2 2 21  ", " P 2 2 21  ", " P 21 2 2  ", " P 21 2 2  ", " P 2 21 2  ", " P 2 21 2  ", &
    " P 21 21 2 ", " P 21 21 2 ", " P 2  21 21", " P 2  21 21", " P 21 2  21", " P 21 2  21", &
    " P 21 21 21", " P 21 21 21", " P 21 21 21", " P 21 21 21", " P 21 21 21", " P 21 21 21", &
    " C 2 2 21  ", " C 2 2 21  ", " A 21 2 2  ", " A 21 2 2  ", " B 2 21 2  ", " B 2 21 2  ", &
    " C 2 2 2   ", " C 2 2 2   ", " A 2 2 2   ", " A 2 2 2   ", " B 2 2 2   ", " B 2 2 2   ", &
    " F 2 2 2   ", " F 2 2 2   ", " F 2 2 2   ", " F 2 2 2   ", " F 2 2 2   ", " F 2 2 2   ", &
    " I 2 2 2   ", " I 2 2 2   ", " I 2 2 2   ", " I 2 2 2   ", " I 2 2 2   ", " I 2 2 2   ", &
    " I 21 21 21", " I 21 21 21", " I 21 21 21", " I 21 21 21", " I 21 21 21", " I 21 21 21", &
    " P m m 2   ", " P m m 2   ", " P 2 m m   ", " P 2 m m   ", " P m 2 m   ", " P m 2 m   ", &
    " P m c 21  ", " P c m 21  ", " P 21 m a  ", " P 21 a m  ", " P b 21 m  ", " P m 21 b  ", &
    " P c c 2   ", " P c c 2   ", " P 2 a a   ", " P 2 a a   ", " P b 2 b   ", " P b 2 b   ", &
    " P m a 2   ", " P b m 2   ", " P 2 m b   ", " P 2 c m   ", " P c 2 m   ", " P c 2 a   ", &
    " P c a 21  ", " P b c 21  ", " P 21 a b  ", " P 21 c a  ", " P c 21 b  ", " P b 21 a  ", &
    " P n c 2   ", " P c n 2   ", " P 2 n a   ", " P 2 a n   ", " P b 2 n   ", " P n 2 b   ", &
    " P m n 21  ", " P n m 21  ", " P 21 m n  ", " P 21 n m  ", " P n 21 m  ", " P m 21 n  ", &
    " P b a 2   ", " P b a 2   ", " P 2 c b   ", " P 2 c b   ", " P c 2 a   ", " P c 2 a   ", &
    " P n a 21  ", " P b n 21  ", " P 21 n b  ", " P 21 c n  ", " P c 21 n  ", " P n 21 a  ", &
    " P n n 2   ", " P n n 2   ", " P 2 n n   ", " P 2 n n   ", " P n 2 n   ", " P n 2 n   ", &
    " C m m 2   ", " C m m 2   ", " A 2 m m   ", " A 2 m m   ", " B m 2 m   ", " B m 2 m   ", &
    " C m c 21  ", " C c m 21  ", " A 21 m a  ", " A 21 a m  ", " B b 21 m  ", " B m 21 b  ", &
    " C c c 2   ", " C c c 2   ", " A 2 a a   ", " A 2 a a   ", " B b 2 b   ", " B b 2 b   ", &
    " A m m 2   ", " B m m 2   ", " B 2 m m   ", " C 2 m m   ", " C m 2 m   ", " A m 2 m   ", &
    " A b m 2   ", " B m a 2   ", " B 2 c m   ", " C 2 m b   ", " C m 2 a   ", " A c 2 m   ", &
    " A m a 2   ", " B b m 2   ", " B 2 m b   ", " C 2 c m   ", " C c 2 m   ", " A m 2 a   ", &
    " A b a 2   ", " B b a 2   ", " B 2 c b   ", " C 2 c b   ", " C c 2 a   ", " A c 2 a   ", &
    " F m m 2   ", " F m m 2   ", " F 2 m m   ", " F 2 m m   ", " F m 2 m   ", " F m 2 m   ", &
    " F d d 2   ", " F d d 2   ", " F 2 d d   ", " F 2 d d   ", " F d 2 d   ", " F d 2 d   ", &
    " I m m 2   ", " I m m 2   ", " I 2 m m   ", " I 2 m m   ", " I m 2 m   ", " I m 2 m   ", &
    " I b a 2   ", " I b a 2   ", " I 2 c b   ", " I 2 c b   ", " I c 2 a   ", " I c 2 a   ", &
    " I m a 2   ", " I b m 2   ", " I 2 m b   ", " I 2 c m   ", " I c 2 m   ", " I m 2 a   ", &
    " P m m m   ", " P m m m   ", " P m m m   ", " P m m m   ", " P m m m   ", " P m m m   ", &
    " P n n n   ", " P n n n   ", " P n n n   ", " P n n n   ", " P n n n   ", " P n n n   ", &
    " P c c m   ", " P c c m   ", " P m a a   ", " P m a a   ", " P b m b   ", " P b m b   ", &
    " P b a n   ", " P b a n   ", " P n c b   ", " P n c b   ", " P c n a   ", " P c n a   ", &
    " P m m a   ", " P m m b   ", " P b m m   ", " P c m m   ", " P m c m   ", " P m a m   ", &
    " P n n a   ", " P n n b   ", " P b n n   ", " P c n n   ", " P n c n   ", " P n a n   ", &
    " P m n a   ", " P n m b   ", " P b m n   ", " P c n m   ", " P n c m   ", " P m a n   ", &
    " P c c a   ", " P c c b   ", " P b a a   ", " P c a a   ", " P b c b   ", " P b a b   ", &
    " P b a m   ", " P b a m   ", " P m c b   ", " P m c b   ", " P c m a   ", " P c m a   ", &
    " P c c n   ", " P c c n   ", " P n a a   ", " P n a a   ", " P b n b   ", " P b n b   ", &
    " P b c m   ", " P c a m   ", " P m c a   ", " P m a b   ", " P b m a   ", " P c m b   ", &
    " P n n m   ", " P n n m   ", " P m n n   ", " P m n n   ", " P n m n   ", " P n m n   ", &
    " P m m n   ", " P m m n   ", " P n m m   ", " P n m m   ", " P m n m   ", " P m n m   ", &
    " P b c n   ", " P c a n   ", " P n c a   ", " P n a b   ", " P b n a   ", " P c n b   ", &
    " P b c a   ", " P c a b   ", " P b c a   ", " P c a b   ", " P b c a   ", " P c a b   ", &
    " P n m a   ", " P m n b   ", " P b n m   ", " P c m n   ", " P m c n   ", " P n a m   ", &
    " C m c m   ", " C c m m   ", " A m m a   ", " A m a m   ", " B b m m   ", " B m m b   ", &
    " C m c a   ", " C c m b   ", " A b m a   ", " A c a m   ", " B b c m   ", " B m a b   ", &
    " C m m m   ", " C m m m   ", " A m m m   ", " A m m m   ", " B m m m   ", " B m m m   ", &
    " C c c m   ", " C c c m   ", " A m a a   ", " A m m a   ", " B b m b   ", " B b m b   ", &
    " C m m a   ", " C m m b   ", " A b m m   ", " A c m m   ", " B m c m   ", " B m a m   ", &
    " C c c a   ", " C c c b   ", " A b a a   ", " A c a a   ", " B b c b   ", " B b a b   ", &
    " F m m m   ", " F m m m   ", " F m m m   ", " F m m m   ", " F m m m   ", " F m m m   ", &
    " F d d d   ", " F d d d   ", " F d d d   ", " F d d d   ", " F d d d   ", " F d d d   ", &
    " I m m m   ", " I m m m   ", " I m m m   ", " I m m m   ", " I m m m   ", " I m m m   ", &
    " I b a m   ", " I b a m   ", " I m c b   ", " I m c b   ", " I c m a   ", " I c m a   ", &
    " I b c a   ", " I c a b   ", " I b c a   ", " I c a b   ", " I b c a   ", " I c a b   ", &
    " I m m a   ", " I m m b   ", " I b m m   ", " I c m m   ", " I m c m   ", " I m a m   " /), (/6, 59/) )
!DEC$ ATTRIBUTES DLLEXPORT :: extendedHMOrthsymbols


!>  SYM_GL  encoded generator strings
character(40),dimension(237) :: SYM_GL= (/  &
"000                                     ","100                                     ","01cOOO0                                 ", &
"01cODO0                                 ","02aDDOcOOO0                             ","01jOOO0                                 ", &
"01jOOD0                                 ","02aDDOjOOO0                             ","02aDDOjOOD0                             ", &
"11cOOO0                                 ","11cODO0                                 ","12aDDOcOOO0                             ", &
"11cOOD0                                 ","11cODD0                                 ","12aDDOcOOD0                             ", &
"02bOOOcOOO0                             ","02bOODcOOD0                             ","02bOOOcDDO0                             ", &
"02bDODcODD0                             ","03aDDObOODcOOD0                         ","03aDDObOOOcOOO0                         ", &
"04aODDaDODbOOOcOOO0                     ","03aDDDbOOOcOOO0                         ","03aDDDbDODcODD0                         ", &
"02bOOOjOOO0                             ","02bOODjOOD0                             ","02bOOOjOOD0                             ", &
"02bOOOjDOO0                             ","02bOODjDOO0                             ","02bOOOjODD0                             ", &
"02bDODjDOD0                             ","02bOOOjDDO0                             ","02bOODjDDO0                             ", &
"02bOOOjDDD0                             ","03aDDObOOOjOOO0                         ","03aDDObOODjOOD0                         ", &
"03aDDObOOOjOOD0                         ","03aODDbOOOjOOO0                         ","03aODDbOOOjODO0                         ", &
"03aODDbOOOjDOO0                         ","03aODDbOOOjDDO0                         ","04aODDaDODbOOOjOOO0                     ", &
"04aODDaDODbOOOjBBB0                     ","03aDDDbOOOjOOO0                         ","03aDDDbOOOjDDO0                         ", &
"03aDDDbOOOjDOO0                         ","12bOOOcOOO0                             ","03bOOOcOOOhDDD1BBB                      ", &
"12bOOOcOOD0                             ","03bOOOcOOOhDDO1BBO                      ","12bDOOcOOO0                             ", &
"12bDOOcDDD0                             ","12bDODcDOD0                             ","12bDOOcOOD0                             ", &
"12bOOOcDDO0                             ","12bDDOcODD0                             ","12bOODcODD0                             ", &
"12bOOOcDDD0                             ","03bOOOcDDOhDDO1BBO                      ","12bDDDcOOD0                             ", &
"12bDODcODD0                             ","12bDODcODO0                             ","13aDDObOODcOOD0                         ", &
"13aDDObODDcODD0                         ","13aDDObOOOcOOO0                         ","13aDDObOOOcOOD0                         ", &
"13aDDObODOcODO0                         ","04aDDObDDOcOOOhODD1OBB                  ","14aODDaDODbOOOcOOO0                     ", &
"05aODDaDODbOOOcOOOhBBB1ZZZ              ","13aDDDbOOOcOOO0                         ","13aDDDbOOOcDDO0                         ", &
"13aDDDbDODcODD0                         ","13aDDDbODOcODO0                         ","02bOOOgOOO0                             ", &
"02bOODgOOB0                             ","02bOOOgOOD0                             ","02bOODgOOF0                             ", &
"03aDDDbOOOgOOO0                         ","03aDDDbDDDgODB0                         ","02bOOOmOOO0                             ", &
"03aDDDbOOOmOOO0                         ","12bOOOgOOO0                             ","12bOOOgOOD0                             ", &
"03bOOOgDDOhDDO1YBO                      ","03bOOOgDDDhDDD1YYY                      ","13aDDDbOOOgOOO0                         ", &
"04aDDDbDDDgODBhODB1OYZ                  ","03bOOOgOOOcOOO0                         ","03bOOOgDDOcDDO0                         ", &
"03bOODgOOBcOOO0                         ","03bOODgDDBcDDB0                         ","03bOOOgOODcOOO0                         ", &
"03bOOOgDDDcDDD0                         ","03bOODgOOFcOOO0                         ","03bOODgDDFcDDF0                         ", &
"04aDDDbOOOgOOOcOOO0                     ","04aDDDbDDDgODBcDOF0                     ","03bOOOgOOOjOOO0                         ", &
"03bOOOgOOOjDDO0                         ","03bOOOgOODjOOD0                         ","03bOOOgDDDjDDD0                         ", &
"03bOOOgOOOjOOD0                         ","03bOOOgOOOjDDD0                         ","03bOOOgOODjOOO0                         ", &
"03bOOOgOODjDDO0                         ","04aDDDbOOOgOOOjOOO0                     ","04aDDDbOOOgOOOjOOD0                     ", &
"04aDDDbDDDgODBjOOO0                     ","04aDDDbDDDgODBjOOD0                     ","03bOOOmOOOcOOO0                         ", &
"03bOOOmOOOcOOD0                         ","03bOOOmOOOcDDO0                         ","03bOOOmOOOcDDD0                         ", &
"03bOOOmOOOjOOO0                         ","03bOOOmOOOjOOD0                         ","03bOOOmOOOjDDO0                         ", &
"03bOOOmOOOjDDD0                         ","04aDDDbOOOmOOOjOOO0                     ","04aDDDbOOOmOOOjOOD0                     ", &
"04aDDDbOOOmOOOcOOO0                     ","04aDDDbOOOmOOOcDOF0                     ","13bOOOgOOOcOOO0                         ", &
"13bOOOgOOOcOOD0                         ","04bOOOgOOOcOOOhDDO1YYO                  ","04bOOOgOOOcOOOhDDD1YYY                  ", &
"13bOOOgOOOcDDO0                         ","13bOOOgOOOcDDD0                         ","04bOOOgDDOcDDOhDDO1YBO                  ", &
"04bOOOgDDOcDDDhDDO1YBO                  ","13bOOOgOODcOOO0                         ","13bOOOgOODcOOD0                         ", &
"04bOOOgDDDcOODhDDD1YBY                  ","04bOOOgDDDcOOOhDDD1YBY                  ","13bOOOgOODcDDO0                         ", &
"13bOOOgDDDcDDD0                         ","04bOOOgDDDcDDDhDDD1YBY                  ","04bOOOgDDDcDDOhDDD1YBY                  ", &
"14aDDDbOOOgOOOcOOO0                     ","14aDDDbOOOgOOOcOOD0                     ","05aDDDbDDDgODBcDOFhODB1OBZ              ", &
"05aDDDbDDDgODBcDOBhODB1OBZ              ","01nOOO0                                 ","01nOOC0                                 ", &
"01nOOE0                                 ","02aECCnOOO0                             ","11nOOO0                                 ", &
"12aECCnOOO0                             ","02nOOOfOOO0                             ","02nOOOeOOO0                             ", &
"02nOOCfOOE0                             ","02nOOCeOOO0                             ","02nOOEfOOC0                             ", &
"02nOOEeOOO0                             ","03aECCnOOOeOOO0                         ","02nOOOkOOO0                             ", &
"02nOOOlOOO0                             ","02nOOOkOOD0                             ","02nOOOlOOD0                             ", &
"03aECCnOOOkOOO0                         ","03aECCnOOOkOOD0                         ","12nOOOfOOO0                             ", &
"12nOOOfOOD0                             ","12nOOOeOOO0                             ","12nOOOeOOD0                             ", &
"13aECCnOOOeOOO0                         ","13aECCnOOOeOOD0                         ","02nOOObOOO0                             ", &
"02nOOCbOOD0                             ","02nOOEbOOD0                             ","02nOOEbOOO0                             ", &
"02nOOCbOOO0                             ","02nOOObOOD0                             ","02nOOOiOOO0                             ", &
"12nOOObOOO0                             ","12nOOObOOD0                             ","03nOOObOOOeOOO0                         ", &
"03nOOCbOODeOOC0                         ","03nOOEbOODeOOE0                         ","03nOOEbOOOeOOE0                         ", &
"03nOOCbOOOeOOC0                         ","03nOOObOODeOOO0                         ","03nOOObOOOkOOO0                         ", &
"03nOOObOOOkOOD0                         ","03nOOObOODkOOD0                         ","03nOOObOODkOOO0                         ", &
"03nOOOiOOOkOOO0                         ","03nOOOiOODkOOD0                         ","03nOOOiOOOeOOO0                         ", &
"03nOOOiOODeOOO0                         ","13nOOObOOOeOOO0                         ","13nOOObOOOeOOD0                         ", &
"13nOOObOODeOOD0                         ","13nOOObOODeOOO0                         ","03bOOOcOOOdOOO0                         ", &
"05aODDaDODbOOOcOOOdOOO0                 ","04aDDDbOOOcOOOdOOO0                     ","03bDODcODDdOOO0                         ", &
"04aDDDbDODcODDdOOO0                     ","13bOOOcOOOdOOO0                         ","04bOOOcOOOdOOOhDDD1YYY                  ", &
"15aODDaDODbOOOcOOOdOOO0                 ","06aODDaDODbOOOcOOOdOOOhBBB1ZZZ          ","14aDDDbOOOcOOOdOOO0                     ", &
"13bDODcODDdOOO0                         ","14aDDDbDODcODDdOOO0                     ","04bOOOcOOOdOOOeOOO0                     ", &
"04bOOOcOOOdOOOeDDD0                     ","06aODDaDODbOOOcOOOdOOOeOOO0             ","06aODDaDODbODDcDDOdOOOeFBF0             ", &
"05aDDDbOOOcOOOdOOOeOOO0                 ","04bDODcODDdOOOeBFF0                     ","04bDODcODDdOOOeFBB0                     ", &
"05aDDDbDODcODDdOOOeFBB0                 ","04bOOOcOOOdOOOlOOO0                     ","06aODDaDODbOOOcOOOdOOOlOOO0             ", &
"05aDDDbOOOcOOOdOOOlOOO0                 ","04bOOOcOOOdOOOlDDD0                     ","06aODDaDODbOOOcOOOdOOOlDDD0             ", &
"05aDDDbDODcODDdOOOlBBB0                 ","14bOOOcOOOdOOOeOOO0                     ","05bOOOcOOOdOOOeOOOhDDD1YYY              ", &
"14bOOOcOOOdOOOeDDD0                     ","05bOOOcOOOdOOOeDDDhDDD1YYY              ","16aODDaDODbOOOcOOOdOOOeOOO0             ", &
"16aODDaDODbOOOcOOOdOOOeDDD0             ","07aODDaDODbODDcDDOdOOOeFBFhBBB1ZZZ      ","07aODDaDODbODDcDDOdOOOeFBFhFFF1XXX      ", &
"15aDDDbOOOcOOOdOOOeOOO0                 ","15aDDDbDODcODDdOOOeFBB0                 ","01dOOO0                                 ", &
"11dOOO0                                 ","02dOOOfOOO0                             ","02dOOOlOOO0                             ", &
"02dOOOlDDD0                             ","12dOOOfOOO0                             ","12dOOOfDDD0                             "/) 
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_GL

!> SGXsym contains the first space group of each crystal system
integer(kind=irg),dimension(7) :: SGXsym = (/ 1, 3, 16, 75, 143, 168, 195 /)
!DEC$ ATTRIBUTES DLLEXPORT :: SGXsym

!>  SGPG contains the first space group # for a given point group
integer(kind=irg),dimension(32):: SGPG =(/1,2,3,6,10,16,25,47,75,81,83,89,99,111,123,143, &
                                          147,149,156,162,168,174,175,177,183,187,191,195, &
                                          200,207,215,221/)
!DEC$ ATTRIBUTES DLLEXPORT :: SGPG

!>  SGsym contains the numbers of all the symmorphic space groups
integer(kind=irg),dimension(73) :: SGsym =(/1,2,3,5,6,8,10,12,16,21,22,23,25,35,38,42,44,47, &
                                            65,69,71,75,79,81,82,83,87,89,97,99,107,111,115, &
                                            119,121,123,139,143,146,147,148,149,150,155,156, &
                                            157,160,162,164,166,168,174,175,177,183,187,189, &
                                            191,195,196,197,200,202,204,207,209,211,215,216, &
                                            217,221,225,229/)
!DEC$ ATTRIBUTES DLLEXPORT :: SGsym

!> SGsymnum contains the number of the symmorphic space group with the same point group symmetry
!>     this is necessary because sometimes the numbering of the space groups is not continuous in
!>     terms of the underlying point group ... e.g. 158 has the same point group as 156, not 157.
integer(kind=irg),dimension(230) :: SGsymnum(230) = (/ &
                                           1,   2,   3,   3,   3,   6,   6,   6,   6,  10, &
                                          10,  10,  10,  10,  10,  16,  16,  16,  16,  16, &
                                          16,  16,  16,  16,  25,  25,  25,  25,  25,  25, &
                                          25,  25,  25,  25,  25,  25,  25,  25,  25,  25, &
                                          25,  25,  25,  25,  25,  25,  47,  47,  47,  47, &
                                          47,  47,  47,  47,  47,  47,  47,  47,  47,  47, &
                                          47,  47,  47,  47,  47,  47,  47,  47,  47,  47, &
                                          47,  47,  47,  47,  75,  75,  75,  75,  75,  75, &
                                          81,  81,  83,  83,  83,  83,  83,  83,  89,  89, &
                                          89,  89,  89,  89,  89,  89,  89,  89,  99,  99, &
                                          99,  99,  99,  99,  99,  99,  99,  99,  99,  99, &
                                         111, 111, 111, 111, 115, 115, 115, 115, 115, 115, &
                                         111, 111, 123, 123, 123, 123, 123, 123, 123, 123, &
                                         123, 123, 123, 123, 123, 123, 123, 123, 123, 123, &
                                         123, 123, 143, 143, 143, 143, 147, 147, 149, 150, &
                                         149, 150, 149, 150, 150, 156, 157, 156, 157, 156, &
                                         156, 162, 162, 164, 164, 164, 164, 168, 168, 168, &
                                         168, 168, 168, 174, 175, 175, 177, 177, 177, 177, &
                                         177, 177, 183, 183, 183, 183, 187, 187, 189, 189, &
                                         191, 191, 191, 191, 195, 195, 195, 195, 195, 200, &
                                         200, 200, 200, 200, 200, 200, 207, 207, 207, 207, &
                                         207, 207, 207, 207, 215, 215, 215, 215, 215, 215, &
                                         221, 221, 221, 221, 221, 221, 221, 221, 221, 221 /)
!DEC$ ATTRIBUTES DLLEXPORT :: SGsymnum

! these parameters implement the diffraction group
! formalism described in the BESR paper.

!> 10 2D point group symbols in International Tables order
character(10),dimension(0:11)  :: PGTWD = (/ ' none     ','    1     ','    2     ','    m     ','  2mm     ','    4     ', &
                                             '  4mm     ','    3     ','   3m1    ','    6     ','  6mm     ','   31m    ' /)
!DEC$ ATTRIBUTES DLLEXPORT :: PGTWD

!> 10 2D point group orders in International Tables order
integer(kind=irg),dimension(0:11)       :: PGTWDorder = (/0,1,2,2,4,4,8,3,6,6,12,6/)
!DEC$ ATTRIBUTES DLLEXPORT :: PGTWDorder

!> inverse table for 2D point groups; this essentially implements the inverse of Table 4 in BESR paper for the Bright Field symmetry.
integer(kind=irg),dimension(12,11) :: PGTWDinverse = reshape((/ & 
                                   1,0,0,0,0,0,0,0,0,0,0,0,  1,2,0,0,0,0,0,0,0,0,0,0, &
                                   1,3,0,4,0,0,0,0,0,0,0,0,  1,3,0,5,0,0,0,0,0,0,0,0, &
                                   1,3,0,4,0,0,0,6,0,0,0,0,  1,0,7,0,0,0,0,0,0,0,0,0, &
                                   1,2,0,0,0,8,0,0,0,0,0,0,  1,3,0,0,0,9,0,0,0,0,0,0, &
                                   1,3,0,4,0,0,0,0,0,0,0,10, 1,3,7,4,0,0,0,0,0,0,0,0, &
                                   1,3,0,4,0,8,0,6,0,0,0,0 /), (/ 12,11 /))
!DEC$ ATTRIBUTES DLLEXPORT :: PGTWDinverse


!> 32 3D point group symbols in International Tables order; additional quasi-crystal rotational
!> groups are added at the end of the list
character(5),dimension(36):: PGTHD =(/'    1','   -1','    2','    m','  2/m','  222', &
                                        '  mm2','  mmm','    4','   -4','  4/m','  422', &
                                        '  4mm',' -42m','4/mmm','    3','   -3','   32', &
                                        '   3m','  -3m','    6','   -6','  6/m','  622', &
                                        '  6mm',' -6m2','6/mmm','   23','   m3','  432', &
                                        ' -43m',' m-3m','  532','  822',' 1022',' 1222' /)
!DEC$ ATTRIBUTES DLLEXPORT :: PGTHD

!> 32 3D point group orders in International Tables order
integer(kind=irg),dimension(32)       :: PGTHDorder = (/ 1, 2, 2, 2, 4, 4, 4, 8, 4, 8, &
                                                         8, 8, 8, 8,16, 3, 6, 6, 6,12, &
                                                         6,12,12,12,12,12,24,12,24,24, &
                                                        24,32 /)
!DEC$ ATTRIBUTES DLLEXPORT :: PGTHDorder

!> 3D point groups : purely rotational point groups corresponding to each point group
integer(kind=irg),dimension(36)   :: PGrot = (/1,1,3,1,3,6,3,6,9,3,9,12,9,6,12,16,16, &
                                              18,16,18,21,16,21,24,21,18,24,28,28,30,28,30,33,34,35,36/)
!DEC$ ATTRIBUTES DLLEXPORT :: PGrot

!> 3D point groups : Laue group number
integer(kind=irg),dimension(36)   :: PGLaue =(/2,2,5,5,5,8,8,8,11,11,11,15,15,15,15,17,17, &
                                              20,20,20,23,23,23,27,27,27,27,29,29,32,32,32,33,34,35,36/)
!DEC$ ATTRIBUTES DLLEXPORT :: PGLaue

!> 3D point groups : inverted Laue group number
integer(kind=irg),dimension(36)   :: PGLaueinv = (/1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6, &
                                                   7,7,7,8,8,8,9,9,9,9,10,10,11,11,11,12,13,14,15/)
!DEC$ ATTRIBUTES DLLEXPORT :: PGLaueinv

!> 3D point groups mapped onto kvector sampling type (used for master pattern computations) [-1 for special cases]
integer(kind=irg),dimension(36)   :: PGSamplingType = (/1, 2, 3, 4, 5, 5, 5, 6, 5, 5, &
                                                        6, 6, 7,-1, 9,-1,-1,-1,-1,-1, &
                                                       15,12,17,16,18,-1,19, 3, 6, 6, &
                                                        8, 9, -1, -1, -1, -1 /)
!DEC$ ATTRIBUTES DLLEXPORT :: PGSamplingType

!> 31 diffraction group symbols in BESR order
character(5),dimension(31)  :: DG =(/'    1','   1R','    2','   2R','  21R','   mR', &
                                     '    m','  m1R','2mRmR','  2mm','2RmmR','2mm1R', &
                                     '    4','   4R','  41R','4mRmR','  4mm','4RmmR', &
                                     '4mm1R','    3','   6R','  3mR','   3m','6RmmR', &
                                     '    6','  31R','  61R','6mRmR','  6mm',' 3m1R', &
                                     '6mm1R'/)
!DEC$ ATTRIBUTES DLLEXPORT :: DG

!> 31 diffraction group orders in BESR order
integer(kind=irg),dimension(31) :: DGorder =(/1, 2, 2, 2, 4, 2, 2, 4, 4, 4, 4, 8, &
                                              4, 4, 8, 8, 8, 8,16, 3, 6, 6, 6,12, &
                                              6, 6,12,12,12,12,24/)
!DEC$ ATTRIBUTES DLLEXPORT :: DGorder

!> Bright Field planar point group for 31 diffraction groups (Table 2, column 2, BESR, with change in row ordering)
integer(kind=irg),dimension(31) :: BFPG =(/1,2,2,1,2,3,3,4,4,4,3,4,5,5,5,6,6,6,6,7,7,8,8,8,9,9,9,10,10,10,10/)
!DEC$ ATTRIBUTES DLLEXPORT :: BFPG

!> Whole Pattern planar point group for 31 diffraction groups (Table 2, column 3, BESR, with change in row ordering)
integer(kind=irg),dimension(31) :: WPPG =(/1,1,2,1,2,1,3,3,2,4,3,4,5,2,5,5,6,4,6,7,7,7,8,8,9,7,9,9,10,8,10/)
!DEC$ ATTRIBUTES DLLEXPORT :: WPPG

!> Dark Field planar point group for 31 diffraction groups (Table 2, column 4, BESR, with change in row ordering)
integer(kind=irg),dimension(31) :: DFGN = (/1,2,1,1,2,1,1,2,1,1,1,2,1,1,2,1,1,1,2,1,1,1,1,1,1,2,2,1,1,2,2/)
!DEC$ ATTRIBUTES DLLEXPORT :: DFGN

!> Dark Field planar point group for 31 diffraction groups (Table 2, column 5, BESR, with change in row ordering)
integer(kind=irg),dimension(31) :: DFSP = (/0,0,0,0,0,3,3,4,3,3,3,4,0,0,0,3,3,3,4,0,0,3,3,3,0,0,0,3,3,4,4/)
!DEC$ ATTRIBUTES DLLEXPORT :: DFSP

!> 10 projection diffraction groups in BESR order (Table 2, column 8, BESR, with change in row ordering)
integer(kind=irg),dimension(31) :: PDG = (/2,2,5,5,5,8,8,8,12,12,12,12,15,15,15,19,19,19,19,26,27,30,30, &
                                          31,27,26,27,31,31,30,31/)
!DEC$ ATTRIBUTES DLLEXPORT :: PDG

!> short hand for .FALSE. logical parameter
logical,parameter,private :: FF=.FALSE.
!DEC$ ATTRIBUTES DLLEXPORT :: FF

!> short hand for .TRUE. logical parameter
logical,parameter,private :: TT=.TRUE.
!DEC$ ATTRIBUTES DLLEXPORT :: TT

!> Table 3 from BESR paper
logical,dimension(32,31)  :: DGPG = reshape((/ &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,TT,FF,FF,TT, &
     FF,FF,FF,FF,TT,FF,FF,TT,FF,FF,TT,FF,FF,FF,TT,FF,FF,FF,FF,TT,FF,FF,TT,FF,FF,FF,TT,FF,TT,FF,FF,TT, &
     FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,TT,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,TT,FF,TT,FF,FF, &
     FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,TT,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,TT,FF,FF,FF,FF,TT,FF, &
     FF,FF,FF,TT,FF,FF,TT,FF,FF,FF,FF,FF,TT,TT,FF,FF,FF,FF,TT,FF,FF,TT,FF,FF,TT,TT,FF,FF,FF,FF,TT,FF, &
     FF,FF,TT,FF,FF,TT,TT,FF,TT,TT,FF,TT,TT,TT,FF,FF,FF,TT,FF,FF,TT,FF,FF,TT,TT,TT,FF,TT,FF,TT,TT,FF, &
     FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,TT,FF,FF,TT,FF,FF,TT,FF,FF,TT,FF,FF,FF,TT,FF,TT,FF,FF,TT,FF,FF,TT,FF,FF,FF,TT,FF,TT,FF,FF,TT, &
     FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,TT,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF,FF, &
     TT,FF,TT,TT,FF,TT,TT,FF,TT,TT,FF,TT,TT,TT,FF,TT,FF,TT,TT,FF,TT,TT,FF,TT,TT,TT,FF,TT,FF,TT,TT,FF/), (/32,31/))
!DEC$ ATTRIBUTES DLLEXPORT :: DGPG

! the following arrays are used for the symmetry compression step in 
! the spherical indexing (EMSphInx) package
integer(kind=irg),dimension(230) :: SHT_ZRot = (/ &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
        4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
        3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 3, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, &
        6, 6, 3, 3, 3, 3, 6, 6, 6, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, &
        4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 /)
!DEC$ ATTRIBUTES DLLEXPORT :: SHT_ZRot

integer(kind=irg),dimension(230) :: SHT_mirInv = (/ &
        0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
        3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
        3, 3, 3, 3, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 2, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 2, 2, 2, 2, 3, 3, 3, 3, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 0, &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 /) 
!DEC$ ATTRIBUTES DLLEXPORT :: SHT_mirInv


! these lines are from an older version; not sure if they are still needed...
! SYM_SGtworig  = point symmetries for two origin choices
! SYM_NUMtworig = number of space groups with two origin settings


! declare user-defined types
!> symdata type definition (for 3D symmetry operations with space groups)
type symdata
  integer(kind=irg)     :: SYM_GENnum                   !< number of generator matrices
  integer(kind=irg)     :: SYM_MATnum                   !< number of non-zero symmetry matrices
  integer(kind=irg)     :: SYM_NUMpt                    !< number of point group operators
  logical               :: SYM_reduce                   !< switch to enable/disable reduction to fundamental cell
  logical               :: SYM_trigonal                 !< switch for hexagonal vs. rhombohedral settings
  logical               :: SYM_second                   !< switch for second setting of spacegroup (if any)
  logical               :: SYM_centrosym                !< switch for presence of centrosymmetry
  real(kind=dbl)        :: SYM_data(192,4,4)            !< all symmetry matrices for a given spacegroup
  real(kind=dbl)        :: SYM_direc(48,3,3)            !< direct space point group matrices
  real(kind=dbl)        :: SYM_recip(48,3,3)            !< reciprocal space point group matrices
  real(kind=dbl)        :: SYM_c(4,4)                   !< dummy 4x4 matrix used for various computations
  character(11)         :: SYM_name                     !< current space group name
end type symdata

! in Release 3.0 and beyond, there are no more global variables
! declare global variables
!> @param SG entire space group structure, moved to cell in crystalvars.f90
! type (symdata)        :: SG


! for many diffraction calculations we need the 2D planar point groups; 
! the maximum order of such a group is 12, and there are only 10 of them, with
! two settings for one of them (3m1 and 31m).
type symdata2D
  integer(kind=irg)     :: SYM_pgnum                    !< 2D point group number
  integer(kind=irg)     :: SYM_MATnum                   !< number of non-zero symmetry matrices (order)
  integer(kind=irg)     :: SYM_direc(12,2,2)            !< point group matrices (filled in by Generate2DSymmetry)
end type symdata2D

! finally, we define the rotational crystal symmetry operators in terms of quaternions (q0, q1,q2,q3) with q0 the scalar part;
! these are used in the dictmod EBSD dictionary indexing module, and are defined with respect to the standard cartesian reference frame
real(kind=dbl),dimension(4,152) :: SYM_Qsymop = reshape( (/ &
                                1.D0, 0.D0, 0.D0, 0.D0, &       ! 1: identity operator
                                0.D0, 1.D0, 0.D0, 0.D0, &       ! 2: 180@[100]
                                0.D0, 0.D0, 1.D0, 0.D0, &       ! 3: 180@[010]
                                0.D0, 0.D0, 0.D0, 1.D0, &       ! 4: 180@[001]
                                sq22, sq22, 0.D0, 0.D0, &       ! 5: 90@[100]
                                sq22, 0.D0, sq22, 0.D0, &       ! 6: 90@[010]
                                sq22, 0.D0, 0.D0, sq22, &       ! 7: 90@[001]
                                sq22,-sq22, 0.D0, 0.D0, &       ! 8: 270@[100]
                                sq22, 0.D0,-sq22, 0.D0, &       ! 9: 270@[010]
                                sq22, 0.D0, 0.D0,-sq22, &       !10: 270@[001]
                                0.D0, sq22, sq22, 0.D0, &       !11: 180@[110]
                                0.D0,-sq22, sq22, 0.D0, &       !12: 180@[-110]
                                0.D0, 0.D0, sq22, sq22, &       !13: 180@[011]
                                0.D0, 0.D0,-sq22, sq22, &       !14: 180@[0-11]
                                0.D0, sq22, 0.D0, sq22, &       !15: 180@[101]
                                0.D0,-sq22, 0.D0, sq22, &       !16: 180@[-101]
                                half, half, half, half, &       !17: 120@[111]
                                half,-half,-half,-half, &       !18: 120@[-1-1-1]
                                half, half,-half, half, &       !19: 120@[1-11]
                                half,-half, half,-half, &       !20: 120@[-11-1]
                                half,-half, half, half, &       !21: 120@[-111]
                                half, half,-half,-half, &       !22: 120@[1-1-1]
                                half,-half,-half, half, &       !23: 120@[-1-11]
                                half, half, half,-half, &       !24: 120@[11-1]
                                sq32, 0.D0, 0.D0, half, &       !25:  60@[001]   (hexagonal/trigonal operators start here)
                                half, 0.D0, 0.D0, sq32, &       !26: 120@[001]
                                0.D0, 0.D0, 0.D0, 1.D0, &       !27: 180@[001]  (duplicate from above, but useful to keep it here)
                               -half, 0.D0, 0.D0, sq32, &       !28: 240@[001]
                               -sq32, 0.D0, 0.D0, half, &       !29: 300@[001]
                                0.D0, 1.D0, 0.D0, 0.D0, &       !30: 180@[100]
                                0.D0, sq32, half, 0.D0, &       !31: 180@[xxx]
                                0.D0, half, sq32, 0.D0, &       !32: 180@[xxx]
                                0.D0, 0.D0, 1.D0, 0.D0, &       !33: 180@[010]
                                0.D0,-half, sq32, 0.D0, &       !34: 180@[xxx]
                                0.D0,-sq32, half, 0.D0, &       !35: 180@[xxx]
                                1.0000000000000000D0, 0.00000000000000000D0, 0.00000000000000000D0, 0.00000000000000000D0, & ! icosahedral operators
                                0.0000000000000000D0, 0.68819093704223555D0, 0.50000000000000000D0, 0.52573108673095648D0, &
                                0.0000000000000000D0,-0.26286554336547824D0, 0.80901700258254916D0, 0.52573108673095648D0, &
                                0.0000000000000000D0,-0.85065078735351463D0, 0.00000000000000000D0, 0.52573108673095648D0, &
                                0.0000000000000000D0,-0.26286554336547824D0,-0.80901700258254916D0, 0.52573108673095648D0, &
                                0.0000000000000000D0, 0.68819093704223555D0,-0.50000000000000000D0, 0.52573108673095648D0, &
                                0.0000000000000000D0, 0.52573108673095648D0, 0.00000000000000000D0, 0.85065078735351463D0, &
                                0.0000000000000000D0, 0.16245985031127913D0, 0.50000000000000000D0, 0.85065078735351463D0, &
                                0.0000000000000000D0,-0.42532539367675731D0, 0.30901700258254972D0, 0.85065078735351463D0, &
                                0.0000000000000000D0,-0.42532539367675731D0,-0.30901700258254972D0, 0.85065078735351463D0, &
                                0.0000000000000000D0, 0.16245985031127913D0,-0.50000000000000000D0, 0.85065078735351463D0, &
                                0.0000000000000000D0, 0.95105654001235851D0,-0.30901700258254972D0, 0.00000000000000000D0, &
                                0.0000000000000000D0, 0.95105654001235851D0, 0.30901700258254972D0, 0.00000000000000000D0, &
                                0.0000000000000000D0, 0.58778524398803644D0, 0.80901700258254916D0, 0.00000000000000000D0, &
                                0.0000000000000000D0, 0.00000000000000000D0, 1.00000000000000000D0, 0.00000000000000000D0, &
                                0.0000000000000000D0,-0.58778524398803644D0, 0.80901700258254916D0, 0.00000000000000000D0, &
                               0.50000000000000000D0, 0.42532540417601997D0, 0.30901699437494742D0, 0.68819096193209561D0, &
                               0.50000000000000000D0,-0.42532540417601997D0,-0.30901699437494742D0,-0.68819096193209561D0, &
                               0.50000000000000000D0,-0.16245984737382999D0, 0.50000000000000000D0, 0.68819096193209561D0, &
                               0.50000000000000000D0, 0.16245984737382999D0,-0.50000000000000000D0,-0.68819096193209561D0, &
                               0.50000000000000000D0,-0.52573108874869778D0, 0.00000000000000000D0, 0.68819096193209561D0, &
                               0.50000000000000000D0, 0.52573108874869778D0, 0.00000000000000000D0,-0.68819096193209561D0, &
                               0.50000000000000000D0,-0.16245984737382999D0,-0.50000000000000000D0, 0.68819096193209561D0, &
                               0.50000000000000000D0, 0.16245984737382999D0, 0.50000000000000000D0,-0.68819096193209561D0, &
                               0.50000000000000000D0, 0.42532539174817890D0,-0.30901700049082287D0, 0.68819096193209561D0, &
                               0.50000000000000000D0,-0.42532539174817890D0, 0.30901700049082287D0,-0.68819096193209561D0, &
                               0.50000000000000000D0,-0.85065078349635781D0, 0.00000000000000000D0, 0.16245984737382999D0, &
                               0.50000000000000000D0, 0.85065078349635781D0, 0.00000000000000000D0,-0.16245984737382999D0, &
                               0.50000000000000000D0,-0.68819096193209561D0,-0.50000000000000000D0,-0.16245984737382999D0, &
                               0.50000000000000000D0, 0.68819096193209561D0, 0.50000000000000000D0, 0.16245984737382999D0, &
                               0.50000000000000000D0,-0.26286554437434889D0,-0.80901695670145712D0, 0.16245984737382999D0, &
                               0.50000000000000000D0, 0.26286554437434889D0, 0.80901695670145712D0,-0.16245984737382999D0, &
                               0.50000000000000000D0, 0.26286554437434889D0,-0.80901695670145712D0,-0.16245984737382999D0, &
                               0.50000000000000000D0,-0.26286554437434889D0, 0.80901695670145712D0, 0.16245984737382999D0, &
                               0.50000000000000000D0, 0.68819096193209561D0,-0.50000000000000000D0, 0.16245984737382999D0, &
                               0.50000000000000000D0,-0.68819096193209561D0, 0.50000000000000000D0,-0.16245984737382999D0, &
                               0.80901700537708732D0, 0.00000000000000000D0, 0.00000000000000000D0, 0.58778523714932640D0, &
                               0.30901702997862029D0, 0.00000000000000000D0, 0.00000000000000000D0, 0.95105650472681824D0, &
                               0.80901700537708732D0, 0.00000000000000000D0, 0.00000000000000000D0,-0.58778523714932640D0, &
                               0.30901702997862029D0, 0.00000000000000000D0, 0.00000000000000000D0,-0.95105650472681824D0, &
                               0.80901700537708732D0, 0.52573109227969150D0, 0.00000000000000000D0, 0.26286554613984575D0, &
                               0.30901702997862029D0, 0.85065078781948245D0, 0.00000000000000000D0, 0.42532539390974122D0, &
                               0.80901700537708732D0,-0.52573109227969150D0, 0.00000000000000000D0,-0.26286554613984575D0, &
                               0.30901702997862029D0,-0.85065078781948245D0, 0.00000000000000000D0,-0.42532539390974122D0, &
                               0.80901700537708732D0, 0.16245984550474032D0, 0.50000000000000000D0, 0.26286554613984575D0, &
                               0.30901702997862029D0, 0.26286555540853851D0, 0.80901696456355054D0, 0.42532539390974122D0, &
                               0.80901700537708732D0,-0.16245984550474032D0,-0.50000000000000000D0,-0.26286554613984575D0, &
                               0.30901702997862029D0,-0.26286555540853851D0,-0.80901696456355054D0,-0.42532539390974122D0, &
                               0.80901700537708732D0,-0.42532540916195122D0, 0.30901697149092866D0, 0.26286554613984575D0, &
                               0.30901702997862029D0,-0.68819097766197224D0, 0.50000000000000000D0, 0.42532539390974122D0, &
                               0.80901700537708732D0, 0.42532540916195122D0,-0.30901697149092866D0,-0.26286554613984575D0, &
                               0.30901702997862029D0, 0.68819097766197224D0,-0.50000000000000000D0,-0.42532539390974122D0, &
                               0.80901700537708732D0,-0.42532540916195122D0,-0.30901697149092866D0, 0.26286554613984575D0, &
                               0.30901702997862029D0,-0.68819097766197224D0,-0.50000000000000000D0, 0.42532539390974122D0, &
                               0.80901700537708732D0, 0.42532540916195122D0, 0.30901697149092866D0,-0.26286554613984575D0, &
                               0.30901702997862029D0, 0.68819097766197224D0, 0.50000000000000000D0,-0.42532539390974122D0, &
                               0.80901700537708732D0, 0.16245984550474032D0,-0.50000000000000000D0, 0.26286554613984575D0, &
                               0.30901702997862029D0, 0.26286555540853851D0,-0.80901696456355054D0, 0.42532539390974122D0, &
                               0.80901700537708732D0,-0.16245984550474032D0, 0.50000000000000000D0,-0.26286554613984575D0, &
                               0.30901702997862029D0,-0.26286555540853851D0, 0.80901696456355054D0,-0.42532539390974122D0, & 

                               ! octagonal QC group 822                              
                               0.923879532511287D0, 0.D0, 0.D0, 0.38268343236509D0, &       ! 45@[001]
                               0.707106781186547D0, 0.D0, 0.D0, 0.707106781186547D0, &      ! 90@[001]
                               0.38268343236509D0, 0.D0, 0.D0, 0.923879532511287D0, &       ! 135@[001]
                               0.D0, 0.D0, 0.D0, 1.D0, &                                    ! 180@[001]
                              -0.382683432365090D0, 0.D0, 0.D0, 0.923879532511287D0, &     ! 225@[001]
                              -0.707106781186547D0, 0.0D0, 0.0D0, 0.707106781186548D0, &   ! 270@[001]
                              -0.923879532511287D0, 0.0D0, 0.0D0, 0.382683432365090D0, &   ! 315@[001]
                               ! all 2 fold rotation axes
                               0.D0, 1.D0, 0.D0, 0.D0, &                                    ! 180@[100]
                               0.D0, 0.923879532511287D0, 0.38268343236509D0, 0.D0, &       ! 180@[cos(pi/8) sin(pi/8) 0]
                               0.0D0, 0.923879532511287D0, -0.382683432365090D0, 0.0D0, &
                               0.0D0, 0.707106781186548D0, -0.707106781186547D0, 0.0D0, &
                               0.0D0, 0.382683432365090D0, -0.923879532511287D0, 0.0D0, &
                               0.0D0, 0.0D0, -1.0D0, 0.0D0, &
                               0.0D0, -0.382683432365090D0, -0.923879532511287D0, 0.0D0, &
                               0.0D0, -0.707106781186547D0, -0.707106781186548D0, 0.0D0, &
                               
                               ! decagonal QC group 1022
                               0.951056516295154D0, 0.0D0, 0.0D0, 0.309016994374947D0, &    ! 36@[001]
                               0.809016994374947D0, 0.0D0, 0.0D0, 0.587785252292473D0, &    ! 72@[001]
                               0.587785252292473D0, 0.0D0, 0.0D0, 0.809016994374947D0, &    ! 108@[001]
                               0.309016994374947D0, 0.0D0, 0.0D0, 0.951056516295154D0, &    ! 144@[001]
                               0.0D0, 0.0D0, 0.0D0, 1.0D0, &                                ! 180@[001]
                              -0.309016994374947D0, 0.0D0, 0.0D0, 0.951056516295154D0, &    ! 216@[001]
                              -0.587785252292473D0, 0.0D0, 0.0D0, 0.809016994374947D0, &    ! 252@[001]
                              -0.809016994374947D0, 0.0D0, 0.0D0, 0.587785252292473D0, &    ! 288@[001]
                              -0.951056516295154D0, 0.0D0, 0.0D0, 0.309016994374948D0, &    ! 324@[001]  
                              ! all 2-fold rotation axis
                               0.D0, 1.D0, 0.D0, 0.D0, &                                    ! 180@[100]
                               0.D0, 0.951056516295154D0, 0.309016994374947D0, 0.D0,   &    ! 180@[cos(pi/10) sin(pi/10) 0]
                               0.0D0, 0.951056516295154D0, -0.309016994374947D0, 0.0D0, &
                               0.0D0, 0.809016994374947D0, -0.587785252292473D0, 0.0D0, &
                               0.0D0, 0.587785252292473D0, -0.809016994374947D0, 0.0D0, &
                               0.0D0, 0.309016994374947D0, -0.951056516295154D0, 0.0D0, &
                               0.0D0, 0.0D0, -1.0D0, 0.0D0, &
                               0.0D0, -0.309016994374947D0, -0.951056516295154D0, 0.0D0, &
                               0.0D0, -0.587785252292473D0, -0.809016994374947D0, 0.0D0, &
                               0.0D0, -0.809016994374947D0, -0.587785252292473D0, 0.0D0, &

                               ! dodecagonal QC group 1222
                               0.965925826289068D0, 0.0D0, 0.0D0, 0.258819045102521D0, &    ! 30@[001] 
                               0.866025403784439D0, 0.0D0, 0.0D0, 0.5D0, &                  ! 60@[001] 
                               0.707106781186548D0, 0.0D0, 0.0D0, 0.707106781186547D0, &    ! 90@[001] 
                               0.5D0, 0.0D0, 0.0D0, 0.866025403784439D0, &                  ! 120@[001] 
                               0.258819045102521D0, 0.0D0, 0.0D0, 0.965925826289068D0, &    ! 150@[001] 
                               0.0D0, 0.0D0, 0.0D0, 1.0D0, &                                    ! 180@[001] 
                              -0.258819045102521D0, 0.0D0, 0.0D0, 0.965925826289068D0, &      ! 210@[001] 
                              -0.5D0, 0.0D0, 0.0D0, 0.866025403784439D0, &                  ! 240@[001] 
                              -0.707106781186547D0, 0.0D0, 0.0D0, 0.707106781186548D0, &    ! 270@[001] 
                              -0.866025403784439D0, 0.0D0, 0.0D0, 0.5D0, &                  ! 300@[001] 
                              -0.965925826289068D0, 0.0D0, 0.0D0, 0.258819045102521D0, &    ! 330@[001]
                              ! all 2-fold rotation axes
                              0.0D0, 1.0D0, 0.0D0, 0.0D0, &                                    ! 180@[100]
                              0.0D0, 0.965925826289068D0, 0.258819045102521D0, 0.0D0, &      ! 180@[cos(pi/12) sin(pi/12) 0]
                              0.0D0, 0.965925826289068D0, -0.258819045102521D0, 0.0D0, &
                              0.0D0, 0.866025403784439D0, -0.5D0, 0.0D0, &
                              0.0D0, 0.707106781186548D0, -0.707106781186547D0, 0.0D0, &
                              0.0D0, 0.5D0, -0.866025403784439D0, 0.0D0, &
                              0.0D0, 0.258819045102521D0, -0.965925826289068D0, 0.0D0, &
                              0.0D0, 0.0D0, -1.0D0, 0.0D0, &
                              0.0D0, -0.258819045102521D0, -0.965925826289068D0, 0.0D0, &
                              0.0D0, -0.5D0, -0.866025403784439D0, 0.0D0, &
                              0.0D0, -0.707106781186547D0, -0.707106781186548D0, 0.0D0, &
                              0.0D0, -0.866025403784439D0, -0.5D0, 0.0D0 &
                              /), (/4,152/) )
!DEC$ ATTRIBUTES DLLEXPORT :: SYM_Qsymop


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! The EBSDiomod module needs a conversion table from a point group number to 
! a 2 character string describing the TSL symmetry convention for that point group
!
! TSL LAUE Symmetry Identifiers (taken from DREAM3D/Source/EbsdLib/TSL/AngConstants.h in DREAM.3D source code)

! #define OH  43        // cubic            Oh         a=b=c     a=b=g=90
! #define TH  23        // tetrahedral      Th         a=b=c     a=b=g=90

! #define D4H 42        // ditetragonal     D4h        a=b!=c    a=b=g=90
! #define C4H 4         // tetragonal       C4h        a=b!=c    a=b=g=90

! #define D2H 22        // orthrohombic     D2h        a!=b!=c   a=b=g=90

! #define C2H_c 2       // monoclinic       C2h        a!=b!=c   a=b=90!=g
! #define C2H_b 20      // monoclinic       C2h        a!=b!=c   a=g=90!=b
! #define C2H_a 21      // monoclinic       C2h        a!=b!=c   b=g=90!=a

! #define D6H 62        // dihexagonal      D6h        a=b!=c    a=b=90 g=120
! #define C6H 6         // hexagonal        C6h        a=b! =c   a=b=90 g=120

! #define D3D 32        // ditrigonal       D3d        a=b=c     a=b=g!=90
! #define C3I 3         // trigonal         C3i        a=b=c     a=b=g!=90

! #define CIs 1         // triclinic        Ci         a!=b!=c  a!=b!=g!=90
!--------------------------------------------------------------------------
character(2),dimension(32) :: TSLsymtype = (/' 1',' 1',' 2',' 2',' 2','22','22','22', &
                                             ' 4',' 4',' 4','42','42','42','42',' 3', &
                                             ' 3','32','32','32',' 6',' 6',' 6','62', &
                                             '62','62','62','23','23','43','43','43'/)
!DEC$ ATTRIBUTES DLLEXPORT :: TSLsymtype
                                             
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! the following table is used for two-phase disorientation fundamental zones
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! this table encodes Figure 1 of the paper  "Representation of Orientation and
! Disorientation data for Cubic, Hexagonal, Tetragonal, and Orthorhombic Crystals", A. Heinz
! and P. Neumann, Acta Cryst. A47, 780-789 (1991)
! The following conversions are used
! 0 -> x  (no symmetry)
! 1 -> a  mixed cubic-hexagonal FZ
! 2 -> b  mixed FZ
! 3 -> c  octahedral FZ
! 4 -> d  tetrahedral FZ
! 5 -> e  24-sided prismatic FZ
! 6 -> f  622 hexagonal dihedral FZ
! 7 -> g  422 octagonal dihedral FZ
! 8 -> h  32 trigonal dihedral FZ
! 9 -> i  222 dihedral FZ
! This table is used in the so3.f90 module to figure out which FZ should be used for a single phase
! or two phase FZ computation; all FZs are also available in the povray.f90 module for 3D visualization.
! The new routine getFZtypeandorder in so3.f90 will take two point group numbers, possibly identical,
! and return the FZtype and FZorder parameters that are currently used already in other routines.  
integer(kind=irg), dimension(32,32) :: FZtypeTable = reshape( (/ &
 0, 0, 0, 0, 0, 9, 0, 9, 0, 0, 0, 7, 0, 9, 7, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 8, 6, 4, 4, 3, 4, 3, &
 0, 0, 0, 0, 0, 9, 0, 9, 0, 0, 0, 7, 0, 9, 7, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 8, 6, 4, 4, 3, 4, 3, &
 0, 0, 0, 0, 0, 9, 9, 9, 7, 9, 7, 7, 7, 7, 7, 8, 8, 6, 8, 6, 6, 8, 6, 6, 6, 6, 6, 4, 4, 3, 4, 3, &
 0, 0, 0, 0, 0, 9, 0, 9, 0, 0, 0, 7, 0, 9, 7, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 8, 6, 4, 4, 3, 4, 3, &
 0, 0, 0, 0, 0, 9, 9, 9, 7, 9, 7, 7, 7, 7, 7, 8, 8, 6, 8, 6, 6, 8, 6, 6, 6, 6, 6, 4, 4, 3, 4, 3, &
 9, 9, 9, 9, 9, 9, 9, 9, 7, 9, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 4, 4, 3, 4, 3, &
 0, 0, 9, 0, 9, 9, 0, 9, 0, 0, 0, 7, 0, 9, 7, 0, 0, 6, 0, 6, 0, 0, 0, 6, 0, 6, 6, 4, 4, 3, 4, 3, &
 9, 9, 9, 9, 9, 9, 9, 9, 7, 9, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 4, 4, 3, 4, 3, &
 0, 0, 7, 0, 7, 7, 0, 7, 0, 0, 0, 7, 0, 7, 7, 0, 0, 5, 0, 5, 0, 0, 0, 5, 0, 5, 5, 3, 3, 3, 3, 3, &
 0, 0, 9, 0, 9, 9, 0, 9, 0, 0, 0, 7, 0, 9, 7, 0, 0, 6, 0, 6, 0, 0, 0, 6, 0, 6, 6, 4, 4, 3, 4, 3, &
 0, 0, 7, 0, 7, 7, 0, 7, 0, 0, 0, 7, 0, 7, 7, 0, 0, 5, 0, 5, 0, 0, 0, 5, 0, 5, 5, 3, 3, 3, 3, 3, &
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, &
 0, 0, 7, 0, 7, 7, 0, 7, 0, 0, 0, 7, 0, 7, 7, 0, 0, 5, 0, 5, 0, 0, 0, 5, 0, 5, 5, 3, 3, 3, 3, 3, &
 9, 9, 7, 9, 7, 7, 9, 7, 7, 9, 7, 7, 7, 9, 7, 6, 6, 5, 6, 5, 6, 6, 6, 5, 6, 5, 5, 3, 3, 3, 3, 3, &
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, &
 0, 0, 8, 0, 8, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 8, 6, 2, 2, 1, 2, 1, &
 0, 0, 8, 0, 8, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 8, 6, 2, 2, 1, 2, 1, &
 8, 8, 6, 8, 6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 5, 8, 8, 8, 8, 8, 6, 8, 6, 6, 6, 8, 6, 2, 2, 1, 2, 1, &
 0, 0, 8, 0, 8, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 8, 6, 2, 2, 1, 2, 1, &
 8, 8, 6, 8, 6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 5, 8, 8, 8, 8, 8, 6, 8, 6, 6, 6, 8, 6, 2, 2, 1, 2, 1, &
 0, 0, 6, 0, 6, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 6, 0, 6, 0, 0, 0, 6, 0, 6, 6, 2, 2, 1, 2, 1, &
 0, 0, 8, 0, 8, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 8, 0, 8, 0, 0, 0, 6, 0, 6, 6, 2, 2, 1, 2, 1, &
 0, 0, 6, 0, 6, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 6, 0, 6, 0, 0, 0, 6, 0, 6, 6, 2, 2, 1, 2, 1, &
 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 2, 2, 1, 2, 1, &
 0, 0, 6, 0, 6, 6, 0, 6, 0, 0, 0, 5, 0, 6, 5, 0, 0, 6, 0, 6, 0, 0, 0, 6, 0, 6, 6, 2, 2, 1, 2, 1, &
 8, 8, 6, 8, 6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 5, 8, 8, 8, 8, 8, 6, 6, 6, 6, 6, 8, 6, 2, 2, 1, 2, 1, &
 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 2, 2, 1, 2, 1, &
 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 3, 4, 3, &
 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 3, 4, 3, &
 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, &
 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 3, 4, 3, &
 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3 &
 /), (/ 32, 32/) )


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! and this used to be the crystalvars module 
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!> [note added on 1/10/14]
!> first we define the reflisttype (all the information needed for a given reciprocal lattice point or rlp).
!> This used to be in the gvectors module, but the type definitions make more sense here.
!> In this linked list, we want to keep everything that might be needed to perform rlp-related 
!> simulations, except for the Fourier coefficient of the lattice potential, wich is kept in 
!> a lookup table.  Anything that can easily be derived from the LUT does not need to be stored.
!> [end note]
!
! linked list of reflections
type reflisttype  
  integer(kind=irg)             :: num, &               ! sequential number
                                   hkl(3),&             ! Miller indices
                                   famhkl(3),&          ! family representative Miller indices
                                   HOLZN,&              ! belongs to this HOLZ layer
                                   strongnum,&          ! sequential number for strong beams
                                   weaknum,&            ! sequential number for weak beams
                                   famnum,&             ! family number
                                   nab(2)               ! decomposition with respect to ga and gb
  logical                       :: dbdiff               ! double diffraction reflection ?
  real(kind=dbl)                :: sg, &               ! excitation error
                                   xg, &              ! extinction distance
! removed 1/10/14                 Ucgmod, &          ! modulus of Fourier coefficient
                                   sangle, &          ! scattering angle (mrad)
                                   thetag             ! phase angle, needed for ECCI simulations
  logical                       :: strong, weak       ! is this a strong beam or not; both .FALSE. means 'do not consider'
  integer(kind=irg)             :: variant            ! one of the four variants of a superlattice reflection in fcc
  complex(kind=dbl)             :: Ucg                  ! Fourier coefficient, copied from cell%LUT
  complex(kind=dbl)             :: qg                  ! scaled Fourier coefficient, copied from cell%LUTqg
  type(reflisttype),pointer     :: next                 ! connection to next entry in master linked list
  type(reflisttype),pointer     :: nexts                ! connection to next strong entry in linked list
  type(reflisttype),pointer     :: nextw                ! connection to next weak entry in linked list
end type reflisttype

! linked list of quasi-crystal reflections [03/15/17, MDG]
type QCreflisttype  
  integer(kind=irg)             :: num, &               ! sequential number
                                   hkl(6),&             ! Miller indices
                                   strongnum,&          ! sequential number for strong beams
                                   weaknum              ! sequential number for weak beams
  real(kind=dbl)                :: sg, &                ! excitation error
                                   xg, &                ! extinction distance
                                   glen                 ! length of reciprocal lattice vector
  logical                       :: strong, weak         ! is this a strong beam or not; both .FALSE. means 'do not consider'
  complex(kind=dbl)             :: Ucg                  ! Fourier coefficient
  complex(kind=dbl)             :: qg                   ! scaled Fourier coefficient
  type(QCreflisttype),pointer   :: next                 ! connection to next entry in master linked list
  type(QCreflisttype),pointer   :: nexts                ! connection to next strong entry in linked list
  type(QCreflisttype),pointer   :: nextw                ! connection to next weak entry in linked list
end type QCreflisttype

! linked list of quasi-crystal reflections [03/23/18, SS]
type TDQCreflisttype  
  integer(kind=irg)                 ::  num, &               ! sequential number
                                        hkl(5),&             ! Miller indices
                                        strongnum,&          ! sequential number for strong beams
                                        weaknum              ! sequential number for weak beams
  real(kind=dbl)                    ::  sg, &                ! excitation error
                                        xg, &                ! extinction distance
                                        glen                 ! length of reciprocal lattice vector
  logical                           ::  strong, weak         ! is this a strong beam or not; both .FALSE. means 'do not consider'
  complex(kind=dbl)                 ::  Ucg                  ! Fourier coefficient
  complex(kind=dbl)                 ::  qg                   ! scaled Fourier coefficient
  type(TDQCreflisttype),pointer     ::  next                 ! connection to next entry in master linked list
  type(TDQCreflisttype),pointer     ::  nexts                ! connection to next strong entry in linked list
  type(TDQCreflisttype),pointer     ::  nextw                ! connection to next weak entry in linked list
end type TDQCreflisttype

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!> [note added on 11/28/14]
!> we define the refliststrongsubstype for the purpose of the film+substrate system. This linked list
!> stores the list of beams diffracted by the film which are incident on the substrate.
!>
!> @todo: we will need to figure out which beams to neglect and which beams to consider while 
!> doing the calculations. Right now we consider all the beams.
!
! linked list of incident beams to the substrate
type refliststrongsubstype
    real(kind=dbl),allocatable              :: hlist(:,:)
    complex(kind=dbl),allocatable           :: DynMat(:,:)
    real(kind=dbl)                          :: kg(3)
    real(kind=dbl)                          :: g(3) ! the g vector corresponding to kg
    integer(kind=irg)                       :: nns 
    type(refliststrongsubstype),pointer     :: next ! only strong beams are considered
end type refliststrongsubstype



!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! linked list for Laue XRD computations.
type Laue_g_list  
  integer(kind=irg)   :: hkl(3)       ! Miller indices
  real(kind=dbl)      :: xyz(3)       ! Cartesian components of the plane normal
  real(kind=dbl)      :: tt           ! 2theta value
  real(kind=dbl)      :: polar        ! polarization factor
  real(kind=dbl)      :: sfs          ! |structure factor|^2
  type(Laue_g_list),pointer :: next   ! connection to next reflector
end type Laue_g_list

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


! a structure that contains all the relevant HOLZ geometry information;
! it can be filled by calling the GetHOLZGeometry subroutine in crystal.f90
type HOLZentries
  real(kind=sgl)        :: g1(3),g2(3),g3(3),gx(3),gy(3),LC1,LC2,H,FNr(3),FNg(3),gp(2),gtoc(2,2),gshort(3)
  integer(kind=irg)     :: uvw(3),FN(3)
end type HOLZentries

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!> [note added on 1/10/14]
!> To make this package functional for multi-phase materials, we must have
!> a way to load the structural information for multiple crystal structures
!> simultaneously.  And we must also be able to perform scattering computations
!> for any given phase. This means that it is probably best if we define a 
!> pointer type to a large collection of unit cell related parameters, so that
!> we can easily switch from one to the other.  As a consequence, we need to 
!> pass the cell pointer along with the other arguments for every single routine
!> that performs simulations that require cell information.  So, first we define a 
!> pointer to a unitcell, and then we can fill in all the information needed,
!> including symmetry operators, atom coordinates, the potential coefficient lookup
!> table, pointers to the linked g-vector list, etc. In order to reduce the number
!> of changes to be made to the source code, we will keep the current variable names
!> as much as possible.
!> [end note]  
!
!> The following are the components of the unitcell type:
!
!> lattice parameters
!> a        = a parameter [nm]
!> b        = b parameter [nm]
!> c        = c parameter [nm]
!> alpha    = alpha angle [deg]
!> beta         = beta angle [deg]
!> gamma    = gamma angle [deg]
!> vol      = unit cell volume [nm^3]
!
!> metric information
!> dmt          = direct space metric tensor
!> rmt      = reciprocal space metric tensor
!> dsm          = direct space structure matrix
!> rsm          = reciprocal space structure matrix
!> trigmat      = direct structure matrix for the trigonal/rhombohedral case, used for Lambert projections
!> [removed on 1/10/14] krdel   = Kronecker delta (unit matrix)
!
!> asymmetric unit contents
!> ATOM_ntype   = actual number of occupied positions in asymmetric unit
!> ATOM_type    = atomic number for each atom in asymmetric unit
!> ATOM_pos = fractional coordinates (x,y,z), occupation, Debye-Waller factor for each atom in asymmetric unit
!
!> the structure file name
!> fname    = crystal structure file name
!
!> use hexagonal or regular indices (comes from old local.f90 module)
!> hexset   = logical to determine whether to use 3(FALSE) or 4(TRUE) index notation 
!
!> atom coordinate array
!> apos        = allocatable array for atom coordinates
!
!> storage space for the potential Fourier coefficient lookup table
!> LUT          = lookup table (allocatable)
!
!> double diffraction logical array
!> dbdiff   = indicates whether a reflection could be due to double diffraction
!
!> is this space group non-symmorphic or not ?
!> nonsymmorphic = logical .TRUE. or .FALSE.
!
!> space group symmetry
!> SG          = space group symmetry structure defined in symmetryvars.f90
!
!> linked g-vector list (used to be in gvectors.f90)
!> reflist     = starting point of linked list of g-vectors
!
!> firstw   = pointer to first weak beam entry in list
        
!> number of beams in linked list (used to be in dynamical.f90)
!> DynNbeams   = current number being considered
!> DynNbeamsLinked = total number
!> nns  = number of strong beams
!> nnw = number of weak beams
!
!> the following entries used to be globals in diffraction.f90 [MDG, 12/02/14]
!>  voltage,mLambda,mRelcor,mSigma,mPsihat
!>
!> added source string on 07/19/18 [MDG]
!>
!> added scatfac(s) arrays to store pre-computed FSCATT values on 08/09/18 [MDG]
!>


type unitcell
  real(kind=dbl)                       :: a,b,c,alpha,beta,gamma
  real(kind=dbl)                       :: dmt(3,3),rmt(3,3),dsm(3,3),rsm(3,3),trigmat(3,3),vol
  integer(kind=irg)                    :: ATOM_type(maxpasym),ATOM_ntype,SYM_SGnum,xtal_system,SYM_SGset
  real(kind=sgl)                       :: ATOM_pos(maxpasym,5)
  integer(kind=irg)                    :: numat(maxpasym)      !< number of atoms of each type in the asymmetric unit
  character(fnlen)                     :: fname
  character(fnlen)                     :: source
  logical                              :: hexset
  real(kind=dbl),allocatable           :: apos(:,:,:)
  real(kind=sgl),allocatable           :: scatfacg(:)
  complex(kind=sgl),allocatable        :: scatfac(:,:) 
  complex(kind=dbl),allocatable        :: LUT(:,:,:), SghLUT(:,:,:,:)
  complex(kind=dbl),allocatable        :: LUTqg(:,:,:)
  logical,allocatable                  :: dbdiff(:,:,:)
  logical                              :: nonsymmorphic
  type(symdata)                        :: SG
  type(reflisttype),pointer            :: reflist
  type(reflisttype),pointer            :: firstw                ! connection to first weak entry in linked list
  integer(kind=irg)                    :: DynNbeams, DynNbeamsLinked, nns, nnw, numscatfac
  real(kind=dbl)                       :: voltage, mLambda, mRelcor, mSigma, mPsihat   ! voltage always in keV !
end type unitcell

! used to hold an array of pointers to multiple cell objects
! type multicell
!     class(unitcell), pointer :: cell
! end type multicell

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!> this type is used to define an orientation relation, i.e., two parallel
!> directions and two parallel planes
type orientation
  real(kind=sgl)                        :: tA(3), tB(3), gA(3), gB(3)
end type orientation

!> cell is a pointer to the generic unit cell variable used in all programs.  
! This pointer is allocated by the InitializeCell routine in the initializers.f90 module
! 
! in Release 2.0, the cell variable was a global variable.  In Release 3 and beyond,
! we aim to have no global variables at all and, instead, pass all variables as 
! function and subroutine arguments.
! type(unitcell) :: cell

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! all variables related to the foil orientation, normal, thickness, etc...
! also, transformation quaternions from various reference frames to the foil and back
! material properties are also stored here, such as the elastic moduli
type foiltype
  real(kind=dbl)                :: F(3), q(3),Fn(3),qn(3),brx,bry,brxy,cpx,cpy, & 
                                   alP,alS,alR,beP,elmo(6,6),z0,zb,B(3),Bn(3),Bm(3)
  real(kind=dbl)                :: a_fc(4), a_fm(4), a_mi(4), a_ic(4), a_mc(4), a_fi(4)
  integer(kind=irg)             :: npix,npiy
  real(kind=sgl),allocatable    :: sg(:,:)
end type foiltype

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! define all defect types
type dislocationtype
  real(kind=dbl)                :: burg(3),burgd(3),u(3),un(3),g(3),gn(3),id,jd, zfrac, zu
  real(kind=dbl)                :: top(3), bottom(3)
  real(kind=dbl)                :: a_dc(4), a_id(4), a_di(4), a_df(4)
  complex(kind=dbl)             :: dismat(3,3),pa(3)
end type dislocationtype

type inclusiontype
        real(kind=sgl)       ::  xpos,ypos,zpos,radius,C
end type inclusiontype

! variables to be read from JSON file
! xyz(3), a123(3), principalaxes(3,3), nu, epsstarvoight(6)

type Einclusiontype
        real(kind=dbl)                  :: xyz(3), a123(3)
        real(kind=dbl)                  :: a1, a2, a3, principalaxes(3,3), permut(3,3), rotell(3,3), epsstar(3,3)
        real(kind=dbl)                  :: nu, omnu, pre, V, a12, a22, a32, asq(3), eta, ss1, svec(3), qs1, qvec1(3), & 
                                           qvec2(3), Deltaij(3,3), kEl, preI1, preI3, s3, c1, c2, mith, math, thpre, &
                                           IIinside(3), IIJinside(3,3), xpos, ypos, zpos, ESV(6,6), EshelbyS(3,3,3,3)
        real(kind=dbl),allocatable      :: EFLUT(:), EELUT(:)
        integer(kind=irg)               :: nLUT
end type Einclusiontype

type stackingfaulttype
  real(kind=sgl)             :: lpu(3),tpu(3),lpb(3),lpbc(3),tpb(3),plane(3),sep,id,jd, &
                                lptop(3),lpbot(3),tptop(3),tpbot(3),thetan,a_if(3,3), &
                                lpr(3),tpr(3), Rdisp(3), poisson
  real(kind=sgl),allocatable     :: zpos(:,:)
end type stackingfaulttype

type voidtype
        real(kind=sgl)       ::  xpos,ypos,zpos,radius
end type voidtype

type YDtype
  real(kind=dbl)             :: burg(3), burgd(3), u(3), un(3), g(3), gn(3), id, jd, zu, bs, be, bx, beta
  real(kind=dbl)             :: alpha, ca, sa, ta, cota,  top(3), bottom(3), sig
  real(kind=dbl)             :: a_dc(4), a_id(4), a_di(4)
end type YDtype

type apbtype
        real(kind=sgl)       ::  xpos,ypos,zpos,radius,w,Rdisp(3)
end type apbtype


! here is a new type definition that simplifies defect handling quite a bit...
! instead of passing many arrays to the defect routines, now we only need to 
! pass a single master defect variable "defects", which must be defined by
! the calling program as type(defecttype) :: defects
! we've also added a few other variables here for lack of a better place to do so...
type defecttype
  integer(kind=irg)                        :: numvoids,numdisl,numYdisl,numsf,numinc,numEinc,numapb
  character(fnlen)                         :: foilname
  integer(kind=irg)                        :: Nmat,DF_g(3),DF_npix,DF_npiy,DF_nums,DF_numinclusion,DF_numvoid
  real(kind=sgl)                           :: DF_slice,DF_L,DF_gc(3),DF_gstar(3), DF_gf(3)
  type (foiltype)                          :: foil
  real(kind=sgl),allocatable               :: DF_foilsg(:,:),DF_R(:,:)
  type (dislocationtype), allocatable      :: DL(:)
  type (inclusiontype), allocatable        :: inclusions(:)
  type (Einclusiontype), allocatable       :: Einclusions(:)
  type (stackingfaulttype), allocatable    :: SF(:)
  type (voidtype), allocatable             :: voids(:)
  type (YDtype), allocatable               :: YD(:)    
  type (apbtype), allocatable              :: apbs(:)
end type defecttype


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! The parameters in gnode are computed by CalcUcg 
type gnode
  character(2)          :: method   ! computation method (WK = Weickenmeier-Kohl, DT = Doyle-Turner/Smith-Burge, XR for XRD)
  logical               :: absorption ! is absorption included or not ?
  integer(kind=irg)     :: hkl(3)   ! Miller indices
  real(kind=sgl)        :: xg, &    ! extinction distance [nm]
                           xgp, &   ! absorption length [nm]
                           ar, &    ! aborption ratio
                           g, &     ! length of reciprocal lattice vectors [nm^-1]
                           Vmod,Vpmod, & ! modulus of Vg and Vgprime [V]
                           Umod,Upmod, & ! modulus of Ug and Ugprime [nm^-2]
                           Vphase,Vpphase ! phase factors of Vg and Vgprime [rad]
  complex(kind=sgl)     :: Ucg, &   ! scaled potential Fourier coefficient [nm^-2]
                           Vg, &    ! potential Fourier coefficient [V]
                           qg       ! interaction parameter for Darwin-Howie-Whelan equations [nm^-1]
end type gnode

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! we'll also need to replace a bunch of variables that have to do with dynamical simulations
type DynType
  real(kind=sgl)                            :: WV(3)                  ! wave vector expressed in reciprocal frame
  real(kind=sgl)                            :: FN(3)                  ! Foil normal in reciprocal frame
  real(kind=sgl)                            :: Upz                    ! U'_0 normal absorption parameter
! complex(kind=dbl),allocatable            :: W(:), &           ! eigenvalue vector for Bloch wave method
!                                             CG(:,:), &        ! eigenvector matrix
!                                             alpha(:), &       ! excitation amplitude vector
!                                             DHWMz(:,:),&      ! Darwin-Howie-Whelan matrix
  complex(kind=dbl),allocatable           :: DynMat(:,:)    ! dynamical matrix
!                                             DynMat0(:,:), &   ! dynamical matrix (for programs that need two or more of them)
!                                             DynMat1(:,:), &   ! dynamical matrix (for programs that need two or more of them)
!                                             DynMat2(:,:), &   ! dynamical matrix (for programs that need two or more of them)
!                                             DynMat3(:,:), &   ! dynamical matrix (for programs that need two or more of them)
!                                             phiz(:),Az(:,:)   ! used for Taylor expansion of scattering matrix
end type DynType



!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! define the cutoff parameters for the Bethe potential approach 
type BetheParameterType
        real(kind=sgl)                 :: c1 = 8.0_sgl         ! changed from 8 and 12 for a test on 8/14/15
        real(kind=sgl)                 :: c2 = 50.0_sgl
        real(kind=sgl)                 :: c3 = 100.0_sgl
        real(kind=sgl)                 :: sgdbdiff = 1.00_sgl    ! changed from 0.05 on 08/14/15 by MDG
        real(kind=sgl)                 :: weakcutoff = 0.0_sgl
        real(kind=sgl)                 :: cutoff = 0.0_sgl
        real(kind=sgl)                 :: sgcutoff = 0.0_sgl
        integer(kind=irg)              :: nns
        integer(kind=irg)              :: nnw
        integer(kind=irg)              :: minweak
        integer(kind=irg)              :: minstrong
        integer(kind=irg)              :: maxweak
        integer(kind=irg)              :: maxstrong
        integer(kind=irg)              :: totweak
        integer(kind=irg)              :: totstrong
        integer(kind=irg),allocatable  :: weaklist(:) 
        integer(kind=irg),allocatable  :: stronglist(:)
        integer(kind=irg),allocatable  :: weakhkl(:,:)
        integer(kind=irg),allocatable  :: stronghkl(:,:)
        real(kind=sgl),allocatable     :: weaksg(:)
        real(kind=sgl),allocatable     :: strongsg(:)
        integer(kind=irg),allocatable  :: strongID(:)
        integer(kind=sgl),allocatable  :: reflistindex(:)              ! used to map strong reflections onto the original reflist
        integer(kind=sgl),allocatable  :: weakreflistindex(:)          ! used to map weak reflections onto the original reflist
end type BetheParameterType


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

type STEMtype
        integer(kind=irg)               :: numk
        real(kind=sgl)                  :: BFmrad,ADFimrad,ADFomrad, diffapmrad, diffapmcenter
        logical,allocatable             :: ZABFweightsarray(:,:,:),ZAADFweightsarray(:,:,:)       ! only used for the zone axis case
        real(kind=sgl),allocatable      :: sgarray(:,:),BFweightsarray(:,:,:),ADFweightsarray(:,:,:)   ! only used for the systematic row case
end type STEMtype

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! linked list of wave vectors (used by all diffraction programs)
type kvectorlist  
  integer(kind=irg)             :: i,j,hs       ! image coordinates
  real(kind=dbl)                :: kt(3)        ! tangential component of wavevector
  real(kind=dbl)                :: kn           ! normal component
  real(kind=dbl)                :: k(3)         ! full wave vector
  type(kvectorlist),pointer     :: next         ! connection to next wave vector
end type kvectorlist

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! collection of formatting parameters
type postscript_type
 integer(kind=irg)      :: pspage
 real(kind=sgl)         :: psdash(20),psfigwidth,psfigheight,psscale
 character(fnlen)       :: psname
end type postscript_type

! used by axonometry-related routines
type axonotype
 integer(kind=irg)      :: xi,yi,beta,xmod,ymod,countx,county
 real(kind=sgl)         :: grid,scle,vscle,xstart,ystart
 logical                :: visibility
end type axonotype

! used by axis and its routines
type axistype
 real(kind=sgl)         :: axw,xll,yll
end type axistype

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

type timetype
  real(kind=sgl)        :: TIME_t_count
  real(kind=sgl)        :: TIME_unit_count
  real(kind=sgl)        :: TIME_interval
  real(kind=sgl)        :: TIME_fraction
  integer(kind=irg)     :: TIME_newcount
  integer(kind=irg)     :: TIME_count_rate
  integer(kind=irg)     :: TIME_count_max
  integer(kind=irg)     :: TIME_count
  integer(kind=irg)     :: TIME_old
  integer(kind=irg)     :: TIME_loops
end type timetype

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! the "orientation" type contains entries for all rotation and orientation representations
type orientationtype
  real(kind=sgl)        :: eulang(3)            ! Bunge Euler angles in radians
  real(kind=sgl)        :: om(3,3)              ! 3x3 matrix
  real(kind=sgl)        :: axang(4)             ! axis-angle pair (angle in rad, component 4; axis in direction cosines)
  real(kind=sgl)        :: rodrigues(4)         ! Rodrigues vector (stored as direction cosines and length, to allow for Infinity)
  real(kind=sgl)        :: quat(4)              ! quaternion representation (q(1) is scalar part, q(2:4) vector part)
  real(kind=sgl)        :: homochoric(3)        ! homochoric representation according to Frank's paper  
  real(kind=sgl)        :: cubochoric(3)        ! cubic grid representation (derived from homochoric)
  real(kind=sgl)        :: stereographic(3)     ! 3D stereographic  [added 10/05/17]
end type orientationtype


! double precision version
type orientationtyped
  real(kind=dbl)        :: eulang(3)            ! Bunge Euler angles in radians
  real(kind=dbl)        :: om(3,3)              ! 3x3 matrix
  real(kind=dbl)        :: axang(4)             ! axis-angle pair (angle in rad, component 4; axis in direction cosines)
  real(kind=dbl)        :: rodrigues(4)         ! Rodrigues vector (stored as direction cosines and length, to allow for Infinity)
  real(kind=dbl)        :: quat(4)              ! quaternion representation (q(1) is scalar part, q(2:4) vector part)
  real(kind=dbl)        :: homochoric(3)        ! homochoric representation according to Frank's paper  
  real(kind=dbl)        :: cubochoric(3)        ! cubic grid representation (derived from homochoric)
  real(kind=dbl)        :: stereographic(3)     ! 3D stereographic  [added 10/05/17]
end type orientationtyped

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! type definition for linked list of Rodrigues-Frank vectors (used in so3.module)
type FZpointd
        real(kind=dbl)          :: rod(4)        ! Rodrigues-Frank vector [nx, ny, nz, tan(omega/2) ]
        real(kind=dbl)          :: trod(4)       ! second Rodrigues-Frank vector; can be used for coordinate transformations
        integer(kind=irg)       :: gridpt(3)     ! coordinates of grid point ! added on 06/19/18 by SS
        type(FZpointd),pointer  :: next          ! link to next point
end type FZpointd


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! type definition for linked list in substrate Bloch wave calculations
type substrateBW
        integer(kind=irg)               :: NSg          ! number of strong substrate reflections for this film g-vector
        real(kind=sgl)                  :: kg(3)        ! incident reciprocal wave vector for particular g in substrate reference frame
        integer(kind=irg),allocatable   :: hg(:,:)      ! reciprocal lattice point list for substrate (hg(3,NSg))
        complex(kind=dbl),allocatable   :: Gammam(:)    ! Gamma eigenvalues of dynamical matrix
        complex(kind=dbl),allocatable   :: Dmg(:,:)     ! Bloch wave coefficients (they already include the beta^(m) excitation amplitudes)
        type(substrateBW),pointer       :: nextg        ! pointer to next entry in list
end type substrateBW


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! type definition for dictionary-based indexing of EBSD patterns
type dicttype
        integer(kind=irg)               :: Nqsym        ! number of quaternion symmetry operators for current crystal system 
        real(kind=dbl)                  :: Pm(4,60)     ! array for quaternion symmetry operators
        integer(kind=irg)               :: pgnum        ! point group number
        integer(kind=irg)               :: prot         ! rotational point group number
        real(kind=dbl),allocatable      :: xAp(:)       ! kappa array
        real(kind=dbl),allocatable      :: yAp(:)       ! A_4(u) lookup table
        integer(kind=irg)               :: Apnum        ! number of entries in lookup table
        integer(kind=irg)               :: Num_of_init  ! number of times that the EM algorithm needs to be carried out (set by user)
        integer(kind=irg)               :: Num_of_iterations    ! number of iterations inside each EM call (set by user)
end type dicttype


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

type kikuchireflection
  integer(kind=irg)                     :: hkl(3),rnum
  logical                               :: drawh,drawk,drawc
  real(kind=sgl)                        :: hlx(2),hly(2),klx(2),kly(2),clx(2),cly(2),beta,theta,Ig
  type(kikuchireflection),pointer       :: next
end type kikuchireflection

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! type definitions for kinematical HOLZ patterns
type holzreflection
  integer(kind=irg)                     :: hkl(3),n1,n2,N
  logical                               :: draw,dbdiff
  real(kind=sgl)                        :: hlphi,hlx(2),hly(2),sg,Ig,pxy(2)
  type(holzreflection),pointer          :: next
end type holzreflection

type HOLZvartype
  real(kind=sgl)                        :: g1(3),g2(3),g3(3),H,FNg(3),FNr(3),gshort(3),gp(3),LC1,LC2,thickness,rectangle(2), &
                                           PX,PY,thetac,laL,Gmax,Imax,gtoc(2,2),glen,phi,CBEDrad,CBEDsc
  integer(kind=irg)                     :: uvw(3),FN(3) 
end type HOLZvartype

! typedef for simulated annealing minimization routine
TYPE COMP_WKS_TYP
        DOUBLE PRECISION         :: FOB,ALFAMAX,ALFANR2,FSTOP
        DOUBLE PRECISION,POINTER :: DOLDALFA(:)
        DOUBLE PRECISION,POINTER :: X(:)
        DOUBLE PRECISION,POINTER :: D(:)
        INTEGER                  :: NUMCOST
END TYPE COMP_WKS_TYP


!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

type MuellerMatrixType
  real(kind=dbl)                        :: M(4,4)
  character(fnlen)                      :: descriptor
end type MuellerMatrixType

type StokesVectorType
  real(kind=dbl)                        :: S(0:3)
  character(fnlen)                      :: descriptor
end type StokesVectorType

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! Quasi-Crystal data structures

type QCsymdata
  integer(kind=irg)                 :: SYM_GENnum                   !< number of generator matrices
  integer(kind=irg)                 :: SYM_MATnum                   !< number of non-zero symmetry matrices
  integer(kind=irg)                 :: SYM_NUMpt                    !< number of point group operators
  logical                           :: SYM_reduce                   !< switch to enable/disable reduction to fundamental cell
  real(kind=dbl)                    :: SYM_data(5000,7,7)            !< all symmetry matrices for a given spacegroup
  real(kind=dbl)                    :: SYM_direc(5000,6,6)           !< direct space point group matrices
  real(kind=dbl)                    :: SYM_recip(5000,6,6)           !< reciprocal space point group matrices
  real(kind=dbl)                    :: SYM_c(7,7)                   !< dummy 6x6 matrix used for various computations
  character(11)                     :: SYM_name
  character(100)                    :: SYM_GL(11)
end type QCsymdata

type QCStructureType
  integer(kind=irg)                     :: atno
  integer(kind=irg)                     :: imax
  integer(kind=irg)                     :: numindices
  integer(kind=irg),allocatable         :: facts(:,:)
  integer(kind=irg),allocatable         :: Ucgindex(:)
  logical,allocatable                   :: Ucgcalc(:)
  integer(kind=irg),allocatable         :: inverseIndex(:,:)
  type(QCsymdata)                       :: SG
  real(kind=dbl)                        :: epvec(3,6), epar(6,3)
  real(kind=dbl)                        :: eovec(3,6), eperp(6,3)
  real(kind=dbl)                        :: Mp(6,6), Picos(6,6)
  real(kind=dbl)                        :: Mo(6,6), Qicos(6,6)
  real(kind=dbl)                        :: dsm(6,6), rsm(6,6)
  real(kind=dbl)                        :: dmt(6,6), rmt(6,6)
  real(kind=dbl)                        :: scaling(6,6)
  real(kind=dbl)                        :: SYM_icos(6,6,120)              ! 532 rotational group in matrix representation
  real(kind=dbl)                        :: QClatparm, alphaij, alphastarij
  real(kind=dbl)                        :: dmin
  real(kind=dbl)                        :: vol
  real(kind=dbl)                        :: gmax_orth
  real(kind=dbl)                        :: DWF
  real(kind=dbl)                        :: voltage
  real(kind=dbl)                        :: mRelCor
  real(kind=dbl)                        :: mSigma
  real(kind=dbl)                        :: mPsihat
  real(kind=dbl)                        :: mLambda
  real(kind=dbl)                        :: Upzero
  real(kind=dbl)                        :: xizerop
  real(kind=dbl)                        :: multiplicity
  character(1)                          :: centering   ! 'P','I','F'
  complex(kind=dbl),allocatable         :: LUT(:)
  complex(kind=dbl),allocatable         :: LUTqg(:)
  logical, allocatable                  :: dbdiff(:)
  character(fnlen)                      :: SGname(11), QCtype, fname
  integer(kind=irg)                     :: SYM_SGnum, ATOM_ntype, ATOM_type(maxpasym), numat(maxpasym)
  real(kind=sgl),allocatable            :: apos(:,:,:)
  real(kind=sgl)                        :: ATOM_pos(maxpasym,10)

end type QCStructureType

type TDQCsymdata
  integer(kind=irg)                 :: SYM_GENnum                   !< number of generator matrices
  integer(kind=irg)                 :: SYM_MATnum                   !< number of non-zero symmetry matrices
  integer(kind=irg)                 :: SYM_NUMpt                    !< number of point group operators
  logical                           :: SYM_reduce                   !< switch to enable/disable reduction to fundamental cell
  real(kind=dbl)                    :: SYM_data(200,6,6)            !< all symmetry matrices for a given spacegroup
  real(kind=dbl)                    :: SYM_direc(100,5,5)           !< direct space point group matrices
  real(kind=dbl)                    :: SYM_recip(100,5,5)           !< reciprocal space point group matrices
  real(kind=dbl)                    :: SYM_c(6,6)                   !< dummy 6x6 matrix used for various computations
  character(11)                     :: SYM_name
  integer(kind=irg)                 :: N_Axial
  character(40),allocatable         :: SYM_GL(:)
end type TDQCsymdata

! 2-D Quasi-Crystal data structures
type TDQCStructureType
  integer(kind=irg)                     :: atno
  integer(kind=irg)                     :: imax_qc, imax_p
  integer(kind=irg)                     :: imaxz
  integer(kind=irg)                     :: numindices
  integer(kind=irg)                     :: nsym
  integer(kind=irg),allocatable         :: facts(:,:)
  integer(kind=irg),allocatable         :: Ucgindex(:)
  logical,allocatable                   :: Ucgcalc(:)
  integer(kind=irg),allocatable         :: inverseIndex(:,:)
  type(TDQCsymdata)                     :: SG
  real(kind=dbl)                        :: epvec(3,5), epar(5,3), scaling(5,5), scalingfact
  real(kind=dbl)                        :: dsm(5,5), rsm(5,5)
  real(kind=dbl)                        :: rmt(5,5), dmt(5,5)
  real(kind=dbl)                        :: SYM_icos(5,5,40)
  real(kind=dbl)                        :: QClatparm_a
  real(kind=dbl)                        :: QClatparm_c
  real(kind=dbl)                        :: alphaij, alphai5, alphastarij
  real(kind=dbl)                        :: dmin_qc, dmin_p
  real(kind=dbl)                        :: vol
  real(kind=dbl)                        :: gmax_orth
  real(kind=dbl)                        :: DWF
  real(kind=dbl)                        :: voltage
  real(kind=dbl)                        :: mRelCor
  real(kind=dbl)                        :: mSigma
  real(kind=dbl)                        :: mPsihat
  real(kind=dbl)                        :: mLambda
  real(kind=dbl)                        :: Upzero
  real(kind=dbl)                        :: xizerop
  real(kind=dbl)                        :: multiplicity
  character(fnlen)                      :: QCtype, fname
  character(1)                          :: centering   ! 'P','I','F'
  complex(kind=dbl),allocatable         :: LUT(:)
  complex(kind=dbl),allocatable         :: LUTqg(:)
  logical, allocatable                  :: dbdiff(:)
  integer(kind=irg)                     :: ATOM_ntype, ATOM_type(maxpasym), SYM_SGnum, numat(maxpasym)
  character(fnlen),allocatable          :: SGname(:)
  real(kind=sgl)                        :: ATOM_pos(maxpasym,10)
  real(kind=sgl),allocatable            :: apos(:,:,:)
end type TDQCStructureType

type PoleFigures
  integer(kind=irg),allocatable         :: hkl(:,:)
  real(kind=dbl),allocatable            :: PFhkl(:,:,:)
  complex(kind=dbl),allocatable         :: xraysf(:)  
  real(kind=dbl),allocatable            :: wf(:)  
end type PoleFigures

type sparse_ll
    integer(kind=ill)               :: idcol, idrow, idlin, idrow_inc, idlin_inc
    real(kind=dbl)                  :: val
    type(sparse_ll),pointer         :: next, next_inc
end type sparse_ll

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

! type definitions for mrc file format writing
type FEIstruct
    real(kind=sgl)      :: a_tilt = 0.0
    real(kind=sgl)      :: b_tilt = 0.0
    real(kind=sgl)      :: x_stage = 0.0
    real(kind=sgl)      :: y_stage = 0.0
    real(kind=sgl)      :: z_stage = 0.0
    real(kind=sgl)      :: x_shift = 0.0
    real(kind=sgl)      :: y_shift = 0.0
    real(kind=sgl)      :: defocus = 0.0
    real(kind=sgl)      :: exp_time = 0.0
    real(kind=sgl)      :: mean_int = 0.0
    real(kind=sgl)      :: tiltaxis = 0.0
    real(kind=sgl)      :: pixelsize = 0.0
    real(kind=sgl)      :: magnification = 0.0
    real(kind=sgl)      :: voltage = 0.0
    character(72)       :: unused
end type FEIstruct

type MRCstruct
    integer(kind=irg)       :: nx = 0 ! number of columns
    integer(kind=irg)       :: ny = 0 ! number of rows
    integer(kind=irg)       :: nz = 0 ! number of sections
    integer(kind=irg)       :: mode = 1 ! type of image pixel 
    integer(kind=irg)       :: nxstart = 0 ! starting point of subimage
    integer(kind=irg)       :: nystart = 0 !  
    integer(kind=irg)       :: nzstart = 0 ! 
    integer(kind=irg)       :: mx = 0 ! grid size in x
    integer(kind=irg)       :: my = 0 ! grid size in y
    integer(kind=irg)       :: mz = 0 ! grid size in z
    real(kind=sgl)          :: xlen = 0 ! cell size; pixel spacing = xlen/mx, ylen/my, zlen/mz
    real(kind=sgl)          :: ylen = 0 !
    real(kind=sgl)          :: zlen = 0 !
    real(kind=sgl)          :: alpha = 90.0 ! cell angles - ignored by IMOD
    real(kind=sgl)          :: beta =90.0 !
    real(kind=sgl)          :: gamma = 90.0 !
    integer(kind=irg)       :: mapc = 1 ! map column  1=x,2=y,3=z
    integer(kind=irg)       :: mapr = 2 ! map row     1=x,2=y,3=z
    integer(kind=irg)       :: maps =3 ! map section 1=x,2=y,3=z
    real(kind=sgl)          :: amin = 0.0 ! minimum pixel value (needs to be set for proper scaling of data)
    real(kind=sgl)          :: amax = 0.0 ! maximum pixel value
    real(kind=sgl)          :: amean = 0.0 ! mean pixel value
    integer(kind=ish)       :: ispg = 0 ! space group number
    integer(kind=ish)       :: nsymbt = 0 ! NOT SURE WHAT THIS IS
    integer(kind=irg)       :: next = 131072 ! number of bytes in extended header (1024 * 128 for FEI)
    integer(kind=ish)       :: creatid = 0 ! used to be an ID number, is 0 as of IMOD 4.2.23
    character(30)           :: extra_data = '00                            ' ! string(' ',format='(A30)'), not used, first two bytes should be 0
    integer(kind=ish)       :: numint = 0 ! number of bytes per section (SerialEM interpretation) [renamed from nint to numin]
    integer(kind=ish)       :: nreal = 32 ! bit flags for short data type
    character(20)           :: extra_data_2= '                    ' ! string(' ',format='(A20)'), $ ; not used
    integer(kind=irg)       :: imodStamp = 0 ! 
    integer(kind=irg)       :: imodFlags = 0 !
    integer(kind=ish)       :: idtype = 0 !  ( 0 = mono, 1 = tilt, 2 = tilts, 3 = lina, 4 = lins)
    integer(kind=ish)       :: lens = 0 !
    integer(kind=ish)       :: nd1 = 0 ! for idtype = 1, nd1 = axis (1, 2, or 3)
    integer(kind=ish)       :: nd2 = 0 ! 
    integer(kind=ish)       :: vd1 = 0 ! vd1 = 100. * tilt increment
    integer(kind=ish)       :: vd2 = 0 ! vd2 = 100. * starting angle
    real(kind=sgl)          :: tiltangles(6) !  0,1,2 = original:  3,4,5 = current
    real(kind=sgl)          :: xorg = 0.0 ! origin of image
    real(kind=sgl)          :: yorg = 0.0 !
    real(kind=sgl)          :: zorg = 0.0 !
    character(4)            :: cmap = 'MAP '
    character(4)            :: stamp = 'DA  ' ! First two bytes have 17 and 17 for big-endian or 68 and 65 for little-endian
    real(kind=sgl)          :: rms = 0.0 ! RMS deviation of densities from mean density
    integer(kind=irg)       :: nlabels =0 ! Number of labels with useful data
    character(800)          :: labels ! string(' ',format='(A800)') $ ; 10 labels of 80 characters each
end type MRCstruct

type sggamma
    real(kind=dbl)          :: sg     ! excitation error for a g vector
    integer(kind=irg)       :: hkl(3) ! g vector associated with sg
    complex(kind=dbl)       :: expsg  ! exp(2*cPi*sg + q0)
    type(sggamma),pointer   :: next   ! pointer to next element
end type sggamma

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

type LTEMstruct
    real(kind=dbl),allocatable      :: kx(:,:,:)
    real(kind=dbl),allocatable      :: ky(:,:,:)
    real(kind=dbl),allocatable      :: kz(:,:,:)
    real(C_DOUBLE),pointer          :: Mx(:,:,:)
    real(C_DOUBLE),pointer          :: My(:,:,:)
    real(C_DOUBLE),pointer          :: Mz(:,:,:)
    real(C_DOUBLE),pointer          :: Bx(:,:,:)
    real(C_DOUBLE),pointer          :: By(:,:,:)
    real(C_DOUBLE),pointer          :: Bz(:,:,:)
    real(C_DOUBLE),pointer          :: Ax(:,:,:)
    real(C_DOUBLE),pointer          :: Ay(:,:,:)
    real(C_DOUBLE),pointer          :: Az(:,:,:)
    real(kind=dbl),allocatable      :: kmag(:,:,:)
end type LTEMstruct

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

integer(kind=irg),parameter         :: CSLnumberdefined = 29
!DEC$ ATTRIBUTES DLLEXPORT :: CSLnumberdefined

character(3), dimension(CSLnumberdefined) :: CSLlabels = &
                                       (/ 'I  ', '3  ', '5  ', '7  ', '9  ', '11 ', '13a', '13b', '15 ', '17a', '17b', &
                                          '19a', '19b', '21a', '21b', '23 ', '25a', '25b', '27a', '27b', '29a', &
                                          '29b', '31a', '31b', '33a', '33b', '33c', '35a', '35b' /)
!DEC$ ATTRIBUTES DLLEXPORT :: CSLlabels

integer(kind=irg),dimension(6,CSLnumberdefined)         :: CSLintegers = reshape((/ 0,1,0,1,0,1, &
                                                                                    1,3,1,3,1,3, &
                                                                                    1,3,0,1,0,1, &
                                                                                    1,5,1,5,1,5, &
                                                                                    1,4,1,4,0,1, &
                                                                                    1,3,1,3,0,1, &
                                                                                    1,5,0,1,0,1, &
                                                                                    1,7,1,7,1,7, &
                                                                                    2,5,1,5,0,1, &
                                                                                    1,4,0,1,0,1, &
                                                                                    2,5,2,5,1,5, &
                                                                                    1,6,1,6,0,1, &
                                                                                    1,4,1,4,1,4, &
                                                                                    1,9,1,9,1,9, &
                                                                                    1,3,1,6,1,6, &
                                                                                    1,3,1,9,1,9, &
                                                                                    1,7,0,1,0,1, &
                                                                                    1,3,1,3,1,9, &
                                                                                    1,5,1,5,0,1, &
                                                                                    2,7,1,7,0,1, &
                                                                                    2,5,0,1,0,1, &
                                                                                    2,7,2,7,1,7, &
                                                                                    1,11,1,11,1,11, &
                                                                                    2,5,1,5,1,5, &
                                                                                    1,8,1,8,0,1, &
                                                                                    3,11,1,11,1,11, &
                                                                                    2,5,2,5,0,1, &
                                                                                    1,4,1,8,1,8, &
                                                                                    3,11,3,11,1,11 /), (/ 6, CSLnumberdefined /))
!DEC$ ATTRIBUTES DLLEXPORT :: CSLintegers

type AngleType
        real(kind=sgl),allocatable      :: quatang(:,:)
end type AngleType

!=======================================
!=======================================
!=======================================
! below are type definitions for the f90 EMsoft version of the original
! Head&Humble hh4.f90 program, now called EMhh4.f90. The comment line
! before each type def is the original COMMON block definition
!
! COMMON/MAPN/NEW,ZR,ZI,QR(9),QI(9),KRASH 
type MAPN_block
  integer(kind=irg)        :: KRASH, NEW
  real(kind=sgl)           :: ZR, ZI, QR(9), QI(9)
end type MAPN_block

! COMMON/MA/PR(4),PI(4),AR(4,4),AI(4,4),EMR(4,4),EMI(4,4),H(4,4) 
type MA_block
  real(kind=sgl)           :: PR(4), PI(4), AR(4,4), AI(4,4), EMR(4,4), EMI(4,4), H(4,4) 
end type MA_block

! COMMON/MKAP/D1(6,6),EP(3,6),EA(3,3) 
type MKAP_block
 real(kind=sgl)            :: D1(6,6), EP(3,6), EA(3,3) 
end type MKAP_block

! COMMON/MRD/CN(61),X,X1,Y(8),ERROR,Q,KOUNT,D(8),YT(8),DT(8,4),ANO,SKIP 
type MRD_block
 real(kind=sgl)            :: CN(61), X, X1, Y(8), ERROR, Q, D(8), YT(8), DT(8,4), ANO, SKIP 
 integer(kind=irg)         :: KOUNT
end type MRD_block

! COMMON/MT/LU(3),LG(3),LBM(3),LFN(3),LB(3),LB2(3),LB3(3),LB4(3), 
!           LFP(3),LFP1(3),LFP3(3),LS1(3),LS2(3),LS3(3),TLU(3),TLG(3),
!           TLBM(3),TLFN(3),TLB(3),TLB2(3),TLB3(3),TLB4(3),TLFP(3),
!           TLFP1(3),TLFP3(3),TLS1(3),TLS2(3),TLS3(3),LF1(3),
!           LF2(3),LF3(3),LF4(3),TLF1(3),TLF2(3),TLF3(3),TLF4(3) 
type MT_block
 real(kind=sgl)            :: TLU(3), TLG(3), TLBM(3), TLFN(3), TLB(3), TLB2(3), TLB3(3), TLB4(3), TLFP(3), &
                              TLFP1(3), TLFP3(3), TLS1(3), TLS2(3), TLS3(3), TLF1(3), TLF2(3), TLF3(3), TLF4(3) 
 integer(kind=irg)         :: LU(3), LG(3), LBM(3), LFN(3), LB(3), LB2(3), LB3(3), LB4(3), LD, LD2, LD3, LD4, &
                              LFP(3), LFP1(3), LFP3(3), LS1(3), LS2(3), LS3(3), LF1(3), LF2(3), LF3(3), LF4(3), &
                              LQ1, LQ2, LQ3
end type MT_block

! COMMON/MKT/AT(3,3),ATR(3,3)
type MKT_block
 real(kind=sgl)            :: AT(3,3), ATR(3,3)
end type MKT_block

! COMMON/SCALE30/LTEST
type SCALE30_block
 integer(kind=irg)         :: LTEST
end type SCALE30_block

! COMMON/MP/PC(4),AS(4,4),EL(4,4) 
type MP_block
 complex(kind=sgl)         :: PC(4), AS(4,4), EL(4,4) 
end type MP_block

! COMMON/MAP/DC(3,3)
type MAP_block
 real(kind=sgl)            :: DC(3,3)
end type MAP_block

!=======================================
!=======================================
!=======================================

! the following is a type definition for a 3D magnetization state 
type LTEM_Magnetization
  integer(kind=irg)                 :: nx
  integer(kind=irg)                 :: ny
  integer(kind=irg)                 :: nz
  real(kind=dbl)                    :: dx
  real(kind=dbl)                    :: dy
  real(kind=dbl)                    :: dz
  real(kind=dbl),allocatable        :: originalMag(:,:,:)
  real(kind=dbl),allocatable        :: resampledMag(:,:,:)
  real(kind=dbl)                    :: Mmagnitude
  real(kind=dbl)                    :: Bzero
  real(kind=dbl)                    :: thick
  character(fnlen)                  :: origin 
end type LTEM_Magnetization

!=======================================
!=======================================
!=======================================
! these are used by the detectors module and various programs
type EBSDPixel
        real(kind=sgl),allocatable      :: lambdaEZ(:,:)
        real(kind=dbl)                  :: dc(3) ! direction cosine in sample frame
        real(kind=dbl)                  :: cfactor
end type EBSDPixel

type EBSDMCdataType
        integer(kind=irg)               :: multiplier
        integer(kind=irg)               :: numEbins
        integer(kind=irg)               :: numzbins
        integer(kind=irg)               :: totnum_el
        integer(kind=irg),allocatable   :: accum_e(:,:,:)
        integer(kind=irg),allocatable   :: accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accumSP(:,:,:)
end type EBSDMCdataType

type EBSDMPdataType
        integer(kind=irg)               :: lastEnergy
        integer(kind=irg)               :: numEbins
        integer(kind=irg)               :: numset
        integer(kind=irg)               :: newPGnumber
        logical                         :: AveragedMP
        character(fnlen)                :: xtalname
        real(kind=sgl),allocatable      :: BetheParameters(:)
        real(kind=sgl),allocatable      :: keVs(:)
        real(kind=sgl),allocatable      :: mLPNH4(:,:,:,:)
        real(kind=sgl),allocatable      :: mLPSH4(:,:,:,:)
        real(kind=sgl),allocatable      :: mLPNH(:,:,:)
        real(kind=sgl),allocatable      :: mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: masterSPNH(:,:,:)
        real(kind=sgl),allocatable      :: masterSPSH(:,:,:)
end type EBSDMPdataType

type EBSDDetectorType
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)  ! auxiliary detector arrays needed for interpolation
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
        type(EBSDPixel),allocatable     :: detector(:,:) 
end type EBSDDetectorType




end module typedefs
