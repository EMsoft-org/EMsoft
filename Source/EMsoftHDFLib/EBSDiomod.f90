! ###################################################################
! Copyright (c) 2016-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EBSDiomod.f90
!--------------------------------------------------------------------------
!
! MODULE: support routines for EBSD output files in .ang (TSL) and .ctf (HKL) formats
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @date 3/10/16 MDG 1.0 reorganization of a few existing routines
!--------------------------------------------------------------------------
module EBSDiomod

use local
use stringconstants
use utilities

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:ctfebsd_writeFile
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Write a *.ctf output file with EBSD data (HKL format)
!
!> @param ebsdnl namelist
!> @param ipar  series of integer dimensions
!> @param indexmain indices into the main Euler array
!> @param eulerarray array of Euler angle triplets
!> @param resultmain dot product array
!
!> @date 02/07/15  SS 1.0 original
!> @date 03/10/16 MDG 1.1 moved from program to module and updated [TO BE COMPLETED]
!> @date 06/05/16 MDG 1.2 added reading of xtal file for correct crystallography output; corrected Euler angles for hkl convention
!> @date 06/05/16 MDG 1.3 added sampling step sizes
!> @date 06/25/16 MDG 1.4 added noindex optional keyword
!> @date 07/10/16 MDG 1.5 swapped Error, MAD, and BC columns
!> @date 02/18/18 MDG 1.6 made sure that Euler angles are ALWAYS positive
!> @date 03/05/18 MDG 1.7 replaced BC=OSMmap, BC=IQmap, BANDS=pattern index columns
!--------------------------------------------------------------------------
recursive subroutine ctfebsd_writeFile(ebsdnl,xtalname,ipar,indexmain,eulerarray,resultmain,OSMmap,IQmap,noindex)
!DEC$ ATTRIBUTES DLLEXPORT :: ctfebsd_writeFile

use NameListTypedefs
use typedefs
use symmetry
use error

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
character(fnlen),INTENT(IN)                         :: xtalname
integer(kind=irg),INTENT(IN)                        :: ipar(10)
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: eulerarray(3,ipar(4))
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: OSMmap(ipar(7),ipar(8))
real(kind=sgl),INTENT(IN)                           :: IQmap(ipar(3))
logical,INTENT(IN),OPTIONAL                         :: noindex

integer(kind=irg)                                   :: ierr, i, ii, indx, hdferr, SGnum, LaueGroup, BCval, BSval
character(fnlen)                                    :: ctfname
character                                           :: TAB = CHAR(9)
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
real(kind=sgl)                                      :: euler(3), eu, mi, ma
logical                                             :: donotuseindexarray
real(kind=dbl)                                      :: cellparams(6)
integer(kind=irg),allocatable                       :: osm(:), iq(:)

donotuseindexarray = .FALSE.
if (present(noindex)) then
  if (noindex.eqv..TRUE.) then 
    donotuseindexarray = .TRUE.
  end if
end if

! get the OSMmap into 1D format and scale to the range [0..255]
allocate(osm(ipar(3)))
mi = minval(OSMmap)
ma = maxval(OSMmap)
if (mi.eq.ma) then
  osm = 0
else
  indx = 1
  do i=1,ipar(8)
    do ii=1,ipar(7)
      osm(indx) = nint(255.0 * (OSMmap(ii,i)-mi)/(ma-mi))
      indx = indx+1
    end do 
  end do
end if
  
! scale the IQmap to the range [0..255]
allocate(iq(ipar(3)))
iq = nint(255.0 * IQmap)

! open the file (overwrite old one if it exists)
ctfname = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%ctffile)
ctfname = EMsoft_toNativePath(ctfname)
open(unit=dataunit2,file=trim(ctfname),status='unknown',action='write',iostat=ierr)

write(dataunit2,'(A)') 'Channel Text File'
write(dataunit2,'(A)') 'EMsoft v. '//trim(EMsoft_getEMsoftversion())//'; BANDS=pattern index, MAD=CI, BC=OSM, BS=IQ'
write(dataunit2,'(A)') 'Author	'//trim(EMsoft_getUsername())
write(dataunit2,'(A)') 'JobMode	Grid'
write(dataunit2,'(2A,I5)') 'XCells',TAB, ipar(7)
write(dataunit2,'(2A,I5)') 'YCells',TAB, ipar(8)
write(dataunit2,'(2A,F6.2)') 'XStep',TAB, ebsdnl%StepX
write(dataunit2,'(2A,F6.2)') 'YStep',TAB, ebsdnl%StepY
write(dataunit2,'(A)') 'AcqE1'//TAB//'0'
write(dataunit2,'(A)') 'AcqE2'//TAB//'0'
write(dataunit2,'(A)') 'AcqE3'//TAB//'0'
write(dataunit2,'(A,A,$)') 'Euler angles refer to Sample Coordinate system (CS0)!',TAB
str1 = 'Mag'//TAB//'30'//TAB//'Coverage'//TAB//'100'//TAB//'Device'//TAB//'0'//TAB//'KV'
write(str2,'(F4.1)') ebsdnl%EkeV
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAngle'
write(str2,'(F5.2)') ebsdnl%MCsig
str2 = adjustl(str2)
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAxis'//TAB//'0'
write(dataunit2,'(A)') trim(str1)
write(dataunit2,'(A)') 'Phases'//TAB//'1'

! here we need to read the .xtal file and extract the lattice parameters, Laue group and space group numbers
! test to make sure the input file exists and is HDF5 format
call getXtalData(xtalname, cellparams, SGnum)

! unit cell size
cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
write(str1,'(F8.3)') cellparams(1)
write(str2,'(F8.3)') cellparams(2)
write(str3,'(F8.3)') cellparams(3)
str1 = adjustl(str1)
str2 = adjustl(str2)
str3 = adjustl(str3)
str1 = trim(str1)//';'//trim(str2)//';'//trim(str3)

! unit cell angles
write(str4,'(F8.3)') cellparams(4)
write(str5,'(F8.3)') cellparams(5)
write(str6,'(F8.3)') cellparams(6)
str4 = adjustl(str5)
str5 = adjustl(str5)
str6 = adjustl(str6)
str1 = trim(str1)//TAB//trim(str4)//';'//trim(str5)//';'//trim(str6)

! structure name
str3 = ''
ii = len(trim(xtalname))-5
do i=1,ii
  str3(i:i) = xtalname(i:i)
end do
str1 = trim(str1)//TAB//trim(str3)

! rotational symmetry group
str4 = ''
LaueGroup = getLaueGroupNumber(SGnum)
write(str4,'(I2)') LaueGroup
str1 = trim(str1)//TAB//trim(adjustl(str4))

! space group
str2 = ''
write(str2,'(I3)') SGnum
str1 = trim(str1)//TAB//trim(adjustl(str2))

! and now collect them all into a single string
write(dataunit2,'(A)') trim(str1)

! this is the table header
write(dataunit2,'(A)') 'Phase'//TAB//'X'//TAB//'Y'//TAB//'Bands'//TAB//'Error'//TAB//'Euler1'//TAB//'Euler2'//TAB//'Euler3' &
                      //TAB//'MAD'//TAB//'BC'//TAB//'BS'

! go through the entire array and write one line per sampling point
do ii = 1,ipar(3)
    BCval = osm(ii)
    BSval = iq(ii)
    if (donotuseindexarray.eqv..TRUE.) then
      indx = 0
      euler = eulerarray(1:3,ii)
    else
      indx = indexmain(1,ii)
      euler = eulerarray(1:3,indx)
    end if
! changed order of coordinates to conform with ctf standard
    if (sum(ebsdnl%ROI).ne.0) then
      write(str2,'(F12.3)') float(floor(float(ii-1)/float(ebsdnl%ROI(3))))*ebsdnl%StepY
      write(str1,'(F12.3)') float(MODULO(ii-1,ebsdnl%ROI(3)))*ebsdnl%StepX
    else
      write(str2,'(F12.3)') float(floor(float(ii-1)/float(ebsdnl%ipf_wd)))*ebsdnl%StepY
      write(str1,'(F12.3)') float(MODULO(ii-1,ebsdnl%ipf_wd))*ebsdnl%StepX
    end if 

    write(str3,'(I8)') indx  ! pattern index into dictionary list of discrete orientations
    write(str8,'(I8)') 0 ! integer zero error; was indx, which is now moved to BANDS
    eu = euler(1) - 90.0 ! conversion from TSL to Oxford convention
    if (eu.lt.0) eu = eu + 360.0
    write(str5,'(F12.3)') eu  
    eu = euler(2)
    if (eu.lt.0) eu = eu + 360.0
    write(str6,'(F12.3)') eu
! intercept the hexagonal case, for which we need to subtract 30° from the third Euler angle
! Note: after working with Lionel Germain, we concluded that we do not need to subtract 30° 
! in the ctf file, because the fundamental zone is already oriented according to the Oxford
! convention... That means that we need to subtract the angle for the .ang file (to be implemented)
! [modified by MDG on 3/5/18]
    if ((LaueGroup.eq.8).or.(LaueGroup.eq.9)) euler(3) = euler(3) - 30.0
    eu = euler(3)
    if (eu.lt.0) eu = eu + 360.0
    write(str7,'(F12.3)') eu
    write(str4,'(F12.6)') resultmain(1,ii)   ! this replaces MAD
! the following two parameters need to be modified to contain more meaningful information
    write(str9,'(I8)') BCval   ! OSM value in range [0..255]
    write(str10,'(I8)') BSval  !  IQ value in range [0..255]
! Oxford 3D files have four additional integer columns;
! GrainIndex
! GrainRandomColourR
! GrainRandomColourG
! GrainRandomColourB
!
    write(dataunit2,'(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)')'1',TAB,trim(adjustl(str1)),TAB,&
    trim(adjustl(str2)),TAB,trim(adjustl(str3)),TAB,trim(adjustl(str8)),TAB,trim(adjustl(str5)),&
    TAB,trim(adjustl(str6)),TAB,trim(adjustl(str7)),TAB,trim(adjustl(str4)),TAB,trim(adjustl(str9)),&
    TAB,trim(adjustl(str10))
end do

close(dataunit2,status='keep')

end subroutine ctfebsd_writeFile


!--------------------------------------------------------------------------
!
! SUBROUTINE:ctftkd_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Write a *.ctf output file with TKD data (HKL format)
!
!> @param tkdnl namelist
!> @param ipar  series of integer dimensions
!> @param indexmain indices into the main Euler array
!> @param eulerarray array of Euler angle triplets
!> @param resultmain dot product array
!
!> @date 05/07/17 MDG 1.0 original, based on EBSD routine
!> @date 05/09/17 MDG 1.1 minor adjustments of TABs (DREAM.3D could not read ctf file)
!> @date 12/01/18 MDG 1.2 replaced BC=OSMmap, BC=IQmap, BANDS=pattern index columns
!--------------------------------------------------------------------------
recursive subroutine ctftkd_writeFile(tkdnl,ipar,indexmain,eulerarray,resultmain,OSMmap,IQmap,noindex)
!DEC$ ATTRIBUTES DLLEXPORT :: ctftkd_writeFile

use NameListTypedefs
use typedefs
use error
use symmetry

IMPLICIT NONE

type(TKDIndexingNameListType),INTENT(INOUT)         :: tkdnl
!f2py intent(in,out) ::  tkdnl
integer(kind=irg),INTENT(IN)                        :: ipar(10)
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: eulerarray(3,ipar(4))
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: OSMmap(ipar(7),ipar(8))
real(kind=sgl),INTENT(IN)                           :: IQmap(ipar(3))
logical,INTENT(IN),OPTIONAL                         :: noindex

integer(kind=irg)                                   :: ierr, i, ii, indx, SGnum, LaueGroup, BCval, BSval
character(fnlen)                                    :: ctfname
character                                           :: TAB = CHAR(9)
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
real(kind=sgl)                                      :: euler(3), eu, mi, ma
logical                                             :: donotuseindexarray
real(kind=dbl)                                      :: cellparams(6)
integer(kind=irg),allocatable                       :: osm(:), iq(:)

donotuseindexarray = .FALSE.
if (present(noindex)) then
  if (noindex.eqv..TRUE.) then 
    donotuseindexarray = .TRUE.
  end if
end if

! get the OSMmap into 1D format and scale to the range [0..255]
allocate(osm(ipar(3)))
mi = minval(OSMmap)
ma = maxval(OSMmap)
if (mi.eq.ma) then
  osm = 0
else
  indx = 1
  do i=1,ipar(8)
    do ii=1,ipar(7)
      osm(indx) = nint(255.0 * (OSMmap(ii,i)-mi)/(ma-mi))
      indx = indx+1
    end do 
  end do
end if

! scale the IQmap to the range [0..255]
allocate(iq(ipar(3)))
iq = nint(255.0 * IQmap)

! open the file (overwrite old one if it exists)
ctfname = trim(EMsoft_getEMdatapathname())//trim(tkdnl%ctffile)
ctfname = EMsoft_toNativePath(ctfname)
open(unit=dataunit2,file=trim(ctfname),status='unknown',action='write',iostat=ierr)

write(dataunit2,'(A)') 'Channel Text File'
write(dataunit2,'(A)') 'EMsoft v. '//trim(EMsoft_getEMsoftversion())//'; BANDS=pattern index, MAD=CI, BC=OSM, BS=IQ'
write(dataunit2,'(A)') 'Author  '//trim(EMsoft_getUsername())//'EMsoft'
write(dataunit2,'(A)') 'JobMode Grid'
write(dataunit2,'(2A,I5)') 'XCells',TAB, ipar(7)
write(dataunit2,'(2A,I5)') 'YCells',TAB, ipar(8)
write(dataunit2,'(2A,F6.2)') 'XStep',TAB, tkdnl%StepX
write(dataunit2,'(2A,F6.2)') 'YStep',TAB, tkdnl%StepY
write(dataunit2,'(A)') 'AcqE1'//TAB//'0'
write(dataunit2,'(A)') 'AcqE2'//TAB//'0'
write(dataunit2,'(A)') 'AcqE3'//TAB//'0'
write(dataunit2,'(A,A,$)') 'Euler angles refer to Sample Coordinate system (CS0)!',TAB
str1 = 'Mag'//TAB//'30'//TAB//'Coverage'//TAB//'100'//TAB//'Device'//TAB//'0'//TAB//'KV'
write(str2,'(F4.1)') tkdnl%EkeV
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAngle'
write(str2,'(F6.2)') tkdnl%MCsig
str2 = adjustl(str2)
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAxis'//TAB//'0'
write(dataunit2,'(A)') trim(str1)
write(dataunit2,'(A)') 'Phases'//TAB//'1'

! here we need to read the .xtal file and extract the lattice parameters, Laue group and space group numbers
! test to make sure the input file exists and is HDF5 format
call getXtalData(tkdnl%MCxtalname, cellparams, SGnum)

! unit cell size
cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
write(str1,'(F8.3)') cellparams(1)
write(str2,'(F8.3)') cellparams(2)
write(str3,'(F8.3)') cellparams(3)
str1 = adjustl(str1)
str2 = adjustl(str2)
str3 = adjustl(str3)
str1 = trim(str1)//';'//trim(str2)//';'//trim(str3)

! unit cell angles
write(str4,'(F8.3)') cellparams(4)
write(str5,'(F8.3)') cellparams(5)
write(str6,'(F8.3)') cellparams(6)
str4 = adjustl(str5)
str5 = adjustl(str5)
str6 = adjustl(str6)
str1 = trim(str1)//TAB//trim(str4)//';'//trim(str5)//';'//trim(str6)

! structure name
str3 = ''
ii = len(trim(tkdnl%MCxtalname))-5
do i=1,ii
  str3(i:i) = tkdnl%MCxtalname(i:i)
end do
str1 = trim(str1)//TAB//trim(str3)

! rotational symmetry group
str4 = ''
LaueGroup = getLaueGroupNumber(SGnum)
write(str4,'(I2)') LaueGroup
str1 = trim(str1)//TAB//trim(adjustl(str4))

! space group
str2 = ''
write(str2,'(I3)') SGnum
str1 = trim(str1)//TAB//trim(adjustl(str2))

! and now collect them all into a single string
write(dataunit2,'(A)') str1

! this is the table header
write(dataunit2,'(A)') 'Phase'//TAB//'X'//TAB//'Y'//TAB//'Bands'//TAB//'Error'//TAB//'Euler1'//TAB//'Euler2'//TAB//'Euler3' &
                      //TAB//'MAD'//TAB//'BC'//TAB//'BS'

! go through the entire array and write one line per sampling point
do ii = 1,ipar(3)
    BCval = osm(ii)
    BSval = iq(ii)
    if (donotuseindexarray.eqv..TRUE.) then
      indx = 0
      euler = eulerarray(1:3,ii)
    else
      indx = indexmain(1,ii)
      euler = eulerarray(1:3,indx)
    end if
! changed order of coordinates to conform with ctf standard
    if (sum(tkdnl%ROI).ne.0) then
      write(str2,'(F12.3)') float(floor(float(ii-1)/float(tkdnl%ROI(3))))*tkdnl%stepX
      write(str1,'(F12.3)') float(MODULO(ii-1,tkdnl%ROI(3)))*tkdnl%stepY
    else
      write(str2,'(F12.3)') float(floor(float(ii-1)/float(tkdnl%ipf_wd)))*tkdnl%stepX
      write(str1,'(F12.3)') float(MODULO(ii-1,tkdnl%ipf_wd))*tkdnl%stepY
    end if
    write(str3,'(I2)') indx ! pattern index into dictionary list of discrete orientations
    write(str8,'(I8)') 0 ! integer zero error; was indx, which is now moved to BANDS
    eu = euler(1) - 90.0 ! conversion from TSL to Oxford convention
    if (eu.lt.0) eu = eu + 360.0
    write(str5,'(F12.3)') eu  
    eu = euler(2)
    if (eu.lt.0) eu = eu + 360.0
    write(str6,'(F12.3)') eu
! intercept the hexagonal case, for which we need to subtract 30° from the third Euler angle
    if ((LaueGroup.eq.8).or.(LaueGroup.eq.9)) euler(3) = euler(3) - 30.0
    eu = euler(3)
    if (eu.lt.0) eu = eu + 360.0
    write(str7,'(F12.3)') eu
    write(str4,'(F12.6)') resultmain(1,ii)   ! this replaces MAD
! the following two parameters need to be modified to contain more meaningful information
    write(str9,'(I8)') BCval    ! OSM value in range [0..255]
    write(str10,'(I8)') BSval   ! IQ value in range [0..255]
! Oxford 3D files have four additional integer columns;
! GrainIndex
! GrainRandomColourR
! GrainRandomColourG
! GrainRandomColourB
!
    write(dataunit2,'(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)')'1',TAB,trim(adjustl(str1)),TAB,&
    trim(adjustl(str2)),TAB,trim(adjustl(str3)),TAB,trim(adjustl(str8)),TAB,trim(adjustl(str5)),&
    TAB,trim(adjustl(str6)),TAB,trim(adjustl(str7)),TAB,trim(adjustl(str4)),TAB,trim(adjustl(str9)),&
    TAB,trim(adjustl(str10))
end do

close(dataunit2,status='keep')

end subroutine ctftkd_writeFile

!--------------------------------------------------------------------------
!
! SUBROUTINE:angebsd_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Write a *.ang output file with EBSD data (TSL format)
!
!> @param ebsdnl namelist
!> @param ipar  series of integer dimensions
!> @param indexmain indices into the main Euler array
!> @param eulerarray array of Euler angle triplets
!> @param resultmain dot product array
!
!> @date 02/07/15  SS 1.0 original
!> @date 03/10/16 MDG 1.1 moved from program to module and updated [TO BE COMPLETED]
!> @date 11/08/18 MDG 2.0 rewrite and testing
!--------------------------------------------------------------------------
recursive subroutine angebsd_writeFile(ebsdnl,xtalname,ipar,indexmain,eulerarray,resultmain,IQmap,noindex)
!DEC$ ATTRIBUTES DLLEXPORT :: angebsd_writeFile

use NameListTypedefs
use constants 


IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
character(fnlen),INTENT(IN)                         :: xtalname
integer(kind=irg),INTENT(IN)                        :: ipar(10)
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: eulerarray(3,ipar(4))
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: IQmap(ipar(3))
logical,INTENT(IN),OPTIONAL                         :: noindex

integer(kind=irg)                                   :: ierr, ii, indx, SGnum
character(fnlen)                                    :: angname
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
character                                           :: TAB = CHAR(9)
character(2)                                        :: TSLsymmetry
real(kind=sgl)                                      :: euler(3), s, BSval
real(kind=dbl)                                      :: cellparams(6), dtor
logical                                             :: donotuseindexarray

dtor = cPi/180.D0

donotuseindexarray = .FALSE.
if (present(noindex)) then
  if (noindex.eqv..TRUE.) then 
    donotuseindexarray = .TRUE.
  end if
end if

! open the file (overwrite old one if it exists)
angname = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%angfile)
angname = EMsoft_toNativePath(angname)
open(unit=dataunit2,file=trim(angname),status='unknown',action='write',iostat=ierr)

! this requires a lot of information...
write(dataunit2,'(A)') '# TEM_PIXperUM          1.000000'
s = ( float(ebsdnl%numsx)*0.5 + ebsdnl%xpc ) / float(ebsdnl%numsx)      ! x-star
write(dataunit2,'(A,F9.6)') '# x-star                ', s
s = ( float(ebsdnl%numsy)*0.5 + ebsdnl%ypc ) / float(ebsdnl%numsy)      ! y-star
write(dataunit2,'(A,F9.6)') '# y-star                ', s
s = ebsdnl%L / ( ebsdnl%delta * float(ebsdnl%numsx) )                   ! z-star
write(dataunit2,'(A,F9.6)') '# z-star                ', s 
write(dataunit2,'(A,F9.6)') '# WorkingDistance       ', ebsdnl%WD       ! this quantity is not used in EMsoft
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# Phase 1'

ii = scan(trim(xtalname),'.')
angname = xtalname(1:ii-1)
write(dataunit2,'(A)') '# MaterialName  	'//trim(angname)
write(dataunit2,'(A)') '# Formula     	'//trim(angname)
write(dataunit2,'(A)') '# Info          patterns indexed using EMsoft::EMEBSDDI'

!==========================
! get space group, lattice parameters, and TSL symmetry string
call getXtalData(xtalname,cellparams,SGnum,TSLsymmetry)

! symmetry string
write(dataunit2,'(A)') '# Symmetry              '//TSLsymmetry

! lattice parameters
cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
write(str1,'(F8.3)') cellparams(1)
write(str2,'(F8.3)') cellparams(2)
write(str3,'(F8.3)') cellparams(3)
str1 = adjustl(str1)
str2 = adjustl(str2)
str3 = adjustl(str3)
str1 = trim(str1)//' '//trim(str2)//' '//trim(str3)

! unit cell angles
write(str4,'(F8.3)') cellparams(4)
write(str5,'(F8.3)') cellparams(5)
write(str6,'(F8.3)') cellparams(6)
str4 = adjustl(str5)
str5 = adjustl(str5)
str6 = adjustl(str6)
str1 = trim(str1)//TAB//trim(str4)//' '//trim(str5)//' '//trim(str6)

write(dataunit2,'(A)') '# LatticeConstants      '//trim(str1)
!==========================

! next we need to get the hklFamilies ranked by kinematical intensity, going out to some value
! this is probably not necessary [based on Stuart's feedback], so we comment it all out
write(dataunit2,'(A)') '# NumberFamilies        0'
! write(dataunit2,'(A)') '# NumberFamilies        4'
! write(dataunit2,'(A)') '# hklFamilies   	 1  1  1 1 0.000000'
! write(dataunit2,'(A)') '# hklFamilies   	 2  0  0 1 0.000000'
! write(dataunit2,'(A)') '# hklFamilies   	 2  2  0 1 0.000000'
! write(dataunit2,'(A)') '# hklFamilies   	 3  1  1 1 0.000000'

!==========================
write(dataunit2,'(A)') '# Categories 0 0 0 0 0'
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# GRID: SqrGrid'
write(dataunit2,'(A,F9.6)') '# XSTEP: ', ebsdnl%StepX
write(dataunit2,'(A,F9.6)') '# YSTEP: ', ebsdnl%StepY
write(dataunit2,'(A,I5)') '# NCOLS_ODD: ',ipar(7)
write(dataunit2,'(A,I5)') '# NCOLS_EVEN: ',ipar(7)
write(dataunit2,'(A,I5)') '# NROWS: ', ipar(8)
write(dataunit2,'(A)') '#'
write(dataunit2,'(A,A)') '# OPERATOR: 	', trim(EMsoft_getUsername())
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# SAMPLEID:'
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# SCANID:'
write(dataunit2,'(A)') '#'

! ok, next we have the actual data, which is in the following order
! * phi1                      -> Phi1
! * phi                       -> Phi
! * phi2                      -> Phi2
! * x pos                     -> pixel position
! * y pos                     -> pixel position
! * image quality             -> iq
! * confidence index          -> resultmain
! * phase                     -> 1 (since there is only one phase in each indexing run)
! the second entry after the arrow is the EMsoft parameter that we write into that location
! these 8 entries must be present...

! go through the entire array and write one line per sampling point
do ii = 1,ipar(3)
    BSval = 255.0 * IQmap(ii)
! should we use the index array or not?
    if (donotuseindexarray.eqv..TRUE.) then
      indx = 0
      euler = eulerarray(1:3,ii)
    else
      indx = indexmain(1,ii)
      euler = eulerarray(1:3,indx)
    end if
    write(str1,'(A,F8.5)') ' ',euler(1)*dtor
    write(str2,'(A,F8.5)') ' ',euler(2)*dtor
    write(str3,'(A,F8.5)') ' ',euler(3)*dtor
! sampling coordinates [interchanged x and y on 05/28/19, MDG] 
    if (sum(ebsdnl%ROI).ne.0) then
      write(str4,'(A,F12.5)') ' ',float(MODULO(ii-1,ebsdnl%ROI(3)))*ebsdnl%StepX
      write(str5,'(A,F12.5)') ' ',float(floor(float(ii-1)/float(ebsdnl%ROI(3))))*ebsdnl%StepY
    else
      write(str4,'(A,F12.5)') ' ',float(MODULO(ii-1,ebsdnl%ipf_wd))*ebsdnl%StepX
      write(str5,'(A,F12.5)') ' ',float(floor(float(ii-1)/float(ebsdnl%ipf_wd)))*ebsdnl%StepY
    end if 
! Image Quality (using the Krieger Lassen pattern sharpness parameter iq)
    write(str6,'(A,F6.1)') ' ',BSval  !  IQ value in range [0.0 .. 255.0]
    write(str7,'(A,F6.3)') ' ',resultmain(1,ii)   ! this replaces MAD
    write(str8,'(A,I1)') '  ',1 
!
    write(dataunit2,"(A,' ',A,' ',A,' ',A,' ',A,' ',A,' ',A,' ',A)") trim(adjustl(str1)),trim(adjustl(str2)),&
                                            trim(adjustl(str3)),trim(adjustl(str4)),trim(adjustl(str5)),&
                                            trim(adjustl(str6)),trim(adjustl(str7)),trim(adjustl(str8))
end do

close(dataunit2,status='keep')

end subroutine angebsd_writeFile



!--------------------------------------------------------------------------
!
! SUBROUTINE:angtkd_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Write a *.ang output file with EBSD data (TSL format)
!
!> @param tkdnl namelist
!> @param ipar  series of integer dimensions
!> @param indexmain indices into the main Euler array
!> @param eulerarray array of Euler angle triplets
!> @param resultmain dot product array
!
!> @date 02/07/15  SS 1.0 original
!> @date 03/10/16 MDG 1.1 moved from program to module and updated [TO BE COMPLETED]
!> @date 11/08/18 MDG 2.0 rewrite and testing
!--------------------------------------------------------------------------
recursive subroutine angtkd_writeFile(tkdnl,ipar,indexmain,eulerarray,resultmain,IQmap,noindex)
!DEC$ ATTRIBUTES DLLEXPORT :: angtkd_writeFile

use NameListTypedefs
use constants 


IMPLICIT NONE

type(TKDIndexingNameListType),INTENT(INOUT)         :: tkdnl
!f2py intent(in,out) ::  tkdnl
integer(kind=irg),INTENT(IN)                        :: ipar(10)
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: eulerarray(3,ipar(4))
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: IQmap(ipar(3))
logical,INTENT(IN),OPTIONAL                         :: noindex

integer(kind=irg)                                   :: ierr, ii, indx, SGnum
character(fnlen)                                    :: angname
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
character                                           :: TAB = CHAR(9)
character(2)                                        :: TSLsymmetry
real(kind=sgl)                                      :: euler(3), s, BSval
real(kind=dbl)                                      :: cellparams(6), dtor
logical                                             :: donotuseindexarray

dtor = cPi/180.D0

donotuseindexarray = .FALSE.
if (present(noindex)) then
  if (noindex.eqv..TRUE.) then 
    donotuseindexarray = .TRUE.
  end if
end if

! open the file (overwrite old one if it exists)
angname = trim(EMsoft_getEMdatapathname())//trim(tkdnl%angfile)
angname = EMsoft_toNativePath(angname)
open(unit=dataunit2,file=trim(angname),status='unknown',action='write',iostat=ierr)

! this requires a lot of information...
write(dataunit2,'(A)') '# TEM_PIXperUM          1.000000'
s = ( float(tkdnl%numsx)*0.5 + tkdnl%xpc ) / float(tkdnl%numsx)      ! x-star
write(dataunit2,'(A,F9.6)') '# x-star                ', s
s = ( float(tkdnl%numsy)*0.5 + tkdnl%ypc ) / float(tkdnl%numsy)      ! y-star
write(dataunit2,'(A,F9.6)') '# y-star                ', s
s = tkdnl%L / ( tkdnl%delta * float(tkdnl%numsx) )                   ! z-star
write(dataunit2,'(A,F9.6)') '# z-star                ', s 
write(dataunit2,'(A,F9.6)') '# WorkingDistance       ', tkdnl%WD       ! this quantity is not used in EMsoft
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# Phase 1'

ii = scan(trim(tkdnl%MCxtalname),'.')
angname = tkdnl%MCxtalname(1:ii-1)
write(dataunit2,'(A)') '# MaterialName    '//trim(angname)
write(dataunit2,'(A)') '# Formula       '//trim(angname)
write(dataunit2,'(A)') '# Info          patterns indexed using EMsoft::EMTKDDI'

!==========================
! get space group, lattice parameters, and TSL symmetry string
call getXtalData(tkdnl%MCxtalname,cellparams,SGnum,TSLsymmetry)

! symmetry string
write(dataunit2,'(A)') '# Symmetry              '//TSLsymmetry

! lattice parameters
cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
write(str1,'(F8.3)') cellparams(1)
write(str2,'(F8.3)') cellparams(2)
write(str3,'(F8.3)') cellparams(3)
str1 = adjustl(str1)
str2 = adjustl(str2)
str3 = adjustl(str3)
str1 = trim(str1)//' '//trim(str2)//' '//trim(str3)

! unit cell angles
write(str4,'(F8.3)') cellparams(4)
write(str5,'(F8.3)') cellparams(5)
write(str6,'(F8.3)') cellparams(6)
str4 = adjustl(str5)
str5 = adjustl(str5)
str6 = adjustl(str6)
str1 = trim(str1)//TAB//trim(str4)//' '//trim(str5)//' '//trim(str6)

write(dataunit2,'(A)') '# LatticeConstants      '//trim(str1)
!==========================

! next we need to get the hklFamilies ranked by kinematical intensity, going out to some value
! this is probably not necessary [based on Stuart's feedback], so we comment it all out
write(dataunit2,'(A)') '# NumberFamilies        0'
! write(dataunit2,'(A)') '# NumberFamilies        4'
! write(dataunit2,'(A)') '# hklFamilies      1  1  1 1 0.000000'
! write(dataunit2,'(A)') '# hklFamilies      2  0  0 1 0.000000'
! write(dataunit2,'(A)') '# hklFamilies      2  2  0 1 0.000000'
! write(dataunit2,'(A)') '# hklFamilies      3  1  1 1 0.000000'

!==========================
write(dataunit2,'(A)') '# Categories 0 0 0 0 0'
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# GRID: SqrGrid'
write(dataunit2,'(A,F9.6)') '# XSTEP: ', tkdnl%StepX
write(dataunit2,'(A,F9.6)') '# YSTEP: ', tkdnl%StepY
write(dataunit2,'(A,I5)') '# NCOLS_ODD: ',ipar(7)
write(dataunit2,'(A,I5)') '# NCOLS_EVEN: ',ipar(7)
write(dataunit2,'(A,I5)') '# NROWS: ', ipar(8)
write(dataunit2,'(A)') '#'
write(dataunit2,'(A,A)') '# OPERATOR:   ', trim(EMsoft_getUsername())
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# SAMPLEID:'
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# SCANID:'
write(dataunit2,'(A)') '#'

! ok, next we have the actual data, which is in the following order
! * phi1                      -> Phi1
! * phi                       -> Phi
! * phi2                      -> Phi2
! * x pos                     -> pixel position
! * y pos                     -> pixel position
! * image quality             -> iq
! * confidence index          -> resultmain
! * phase                     -> 1 (since there is only one phase in each indexing run)
! the second entry after the arrow is the EMsoft parameter that we write into that location
! these 8 entries must be present...

! go through the entire array and write one line per sampling point
do ii = 1,ipar(3)
    BSval = 255.0 * IQmap(ii)
! should we use the index array or not?
    if (donotuseindexarray.eqv..TRUE.) then
      indx = 0
      euler = eulerarray(1:3,ii)
    else
      indx = indexmain(1,ii)
      euler = eulerarray(1:3,indx)
    end if
    write(str1,'(A,F8.5)') ' ',euler(1)*dtor
    write(str2,'(A,F8.5)') ' ',euler(2)*dtor
    write(str3,'(A,F8.5)') ' ',euler(3)*dtor
! sampling coordinates
    if (sum(tkdnl%ROI).ne.0) then
      write(str4,'(A,F12.5)') ' ',float(floor(float(ii-1)/float(tkdnl%ROI(3))))*tkdnl%StepY
      write(str5,'(A,F12.5)') ' ',float(MODULO(ii-1,tkdnl%ROI(3)))*tkdnl%StepX
    else
      write(str4,'(A,F12.5)') ' ',float(floor(float(ii-1)/float(tkdnl%ipf_wd)))*tkdnl%StepY
      write(str5,'(A,F12.5)') ' ',float(MODULO(ii-1,tkdnl%ipf_wd))*tkdnl%StepX
    end if 
! Image Quality (using the Krieger Lassen pattern sharpness parameter iq)
    write(str6,'(A,F6.1)') ' ',BSval  !  IQ value in range [0.0 .. 255.0]
    write(str7,'(A,F6.3)') ' ',resultmain(1,ii)   ! this replaces MAD
    write(str8,'(A,I1)') '  ',1 
!
    write(dataunit2,"(A,' ',A,' ',A,' ',A,' ',A,' ',A,' ',A,' ',A)") trim(adjustl(str1)),trim(adjustl(str2)),&
                                            trim(adjustl(str3)),trim(adjustl(str4)),trim(adjustl(str5)),&
                                            trim(adjustl(str6)),trim(adjustl(str7)),trim(adjustl(str8))
end do

close(dataunit2,status='keep')

end subroutine angtkd_writeFile


!--------------------------------------------------------------------------
!
! SUBROUTINE:ctfmerge_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Write a merged *.ctf output file with EBSD data (HKL format)
!
!> @param ebsdnl namelist
!> @param ipar  series of integer dimensions
!> @param indexmain indices into the main Euler array
!> @param eulerarray array of Euler angle triplets
!> @param resultmain dot product array
!
!> @date 08/18/19 MDG 1.0 original, based on ctfebsd_writeFile routine 
!--------------------------------------------------------------------------
recursive subroutine ctfmerge_writeFile(ebsdnl,xtalname,ipar,eangles,phaseID,dplist,OSMlist,IQmap)
!DEC$ ATTRIBUTES DLLEXPORT :: ctfmerge_writeFile

use NameListTypedefs
use typedefs
use symmetry
use constants
use error

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
character(fnlen),INTENT(IN)                         :: xtalname(5)
integer(kind=irg),INTENT(IN)                        :: ipar(4)
real(kind=sgl),INTENT(INOUT)                        :: eangles(3,ipar(1),ipar(2))
!f2py intent(in,out) ::  eangles
integer(kind=irg),INTENT(IN)                        :: phaseID(ipar(1))
real(kind=sgl),INTENT(IN)                           :: dplist(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: OSMlist(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: IQmap(ipar(1))

integer(kind=irg)                                   :: ierr, i, ii, indx, hdferr, SGnum, LaueGroup(5), BCval, BSval, iph
character(fnlen)                                    :: ctfname, xtn
character                                           :: TAB = CHAR(9)
character(1)                                        :: np
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
real(kind=sgl)                                      :: euler(3), eu, mi, ma
logical                                             :: donotuseindexarray
real(kind=dbl)                                      :: cellparams(6)
integer(kind=irg),allocatable                       :: osm(:), iq(:)
real(kind=sgl),allocatable                          :: osmr(:)

! get the OSMmap into 1D format and scale to the range [0..255]
allocate(osm(ipar(1)), osmr(ipar(1)))
do i=1,ipar(1)
  osmr(i) = OSMlist(i,phaseID(i))
end do
mi = minval(osmr)
ma = maxval(osmr)
osm = nint(255.0 * (osmr-mi)/(ma-mi))
deallocate(osmr)

! scale the IQmap to the range [0..255]
allocate(iq(ipar(1)))
iq = nint(255.0 * IQmap)

! open the file (overwrite old one if it exists)
ctfname = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%ctffile)
ctfname = EMsoft_toNativePath(ctfname)
open(unit=dataunit2,file=trim(ctfname),status='unknown',action='write',iostat=ierr)

write(dataunit2,'(A)') 'Channel Text File'
write(dataunit2,'(A)') 'EMsoft v. '//trim(EMsoft_getEMsoftversion())//'; BANDS=pattern index, MAD=CI, BC=OSM, BS=IQ'
write(dataunit2,'(A)') 'Author  '//trim(EMsoft_getUsername())
write(dataunit2,'(A)') 'JobMode Grid'
write(dataunit2,'(2A,I5)') 'XCells',TAB, ipar(3)
write(dataunit2,'(2A,I5)') 'YCells',TAB, ipar(4)
write(dataunit2,'(2A,F6.2)') 'XStep',TAB, ebsdnl%StepX
write(dataunit2,'(2A,F6.2)') 'YStep',TAB, ebsdnl%StepY
write(dataunit2,'(A)') 'AcqE1'//TAB//'0'
write(dataunit2,'(A)') 'AcqE2'//TAB//'0'
write(dataunit2,'(A)') 'AcqE3'//TAB//'0'
write(dataunit2,'(A,A,$)') 'Euler angles refer to Sample Coordinate system (CS0)!',TAB
str1 = 'Mag'//TAB//'30'//TAB//'Coverage'//TAB//'100'//TAB//'Device'//TAB//'0'//TAB//'KV'
write(str2,'(F4.1)') ebsdnl%EkeV
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAngle'
write(str2,'(F5.2)') ebsdnl%MCsig
str2 = adjustl(str2)
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAxis'//TAB//'0'
write(dataunit2,'(A)') trim(str1)
write(np,"(I1)") ipar(2)
write(dataunit2,'(A)') 'Phases'//TAB//np

do iph=1,ipar(2)
! here we need to read each .xtal file and extract the lattice parameters, Laue group and space group numbers
! test to make sure the input file exists and is HDF5 format
  call getXtalData(xtalname(iph), cellparams, SGnum)

! unit cell size
  cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
  write(str1,'(F8.3)') cellparams(1)
  write(str2,'(F8.3)') cellparams(2)
  write(str3,'(F8.3)') cellparams(3)
  str1 = adjustl(str1)
  str2 = adjustl(str2)
  str3 = adjustl(str3)
  str1 = trim(str1)//';'//trim(str2)//';'//trim(str3)

! unit cell angles
  write(str4,'(F8.3)') cellparams(4)
  write(str5,'(F8.3)') cellparams(5)
  write(str6,'(F8.3)') cellparams(6)
  str4 = adjustl(str5)
  str5 = adjustl(str5)
  str6 = adjustl(str6)
  str1 = trim(str1)//TAB//trim(str4)//';'//trim(str5)//';'//trim(str6)

! structure name
  str3 = ''
  xtn = trim(xtalname(iph))
  ii = len(trim(xtn))-5
  do i=1,ii
    str3(i:i) = xtn(i:i)
  end do
  str1 = trim(str1)//TAB//trim(str3)

! rotational symmetry group
  str4 = ''
  LaueGroup(iph) = getLaueGroupNumber(SGnum)
  write(str4,'(I2)') LaueGroup(iph)
  str1 = trim(str1)//TAB//trim(adjustl(str4))

! space group
  str2 = ''
  write(str2,'(I3)') SGnum
  str1 = trim(str1)//TAB//trim(adjustl(str2))

! and now collect them all into a single string
  write(dataunit2,'(A)') trim(str1)
end do 

! this is the table header
write(dataunit2,'(A)') 'Phase'//TAB//'X'//TAB//'Y'//TAB//'Bands'//TAB//'Error'//TAB//'Euler1'//TAB//'Euler2'//TAB//'Euler3' &
                      //TAB//'MAD'//TAB//'BC'//TAB//'BS'

! Euler angles are always in degrees 
eangles = eangles * 180.0/sngl(cPi)

! go through the entire array and write one line per sampling point
do ii = 1,ipar(1)
    BCval = osm(ii)
    BSval = iq(ii)
    euler = eangles(1:3,ii,phaseID(ii))

! changed order of coordinates to conform with ctf standard
    if (sum(ebsdnl%ROI).ne.0) then
      write(str2,'(F12.3)') float(floor(float(ii-1)/float(ebsdnl%ROI(3))))*ebsdnl%StepY
      write(str1,'(F12.3)') float(MODULO(ii-1,ebsdnl%ROI(3)))*ebsdnl%StepX
    else
      write(str2,'(F12.3)') float(floor(float(ii-1)/float(ebsdnl%ipf_wd)))*ebsdnl%StepY
      write(str1,'(F12.3)') float(MODULO(ii-1,ebsdnl%ipf_wd))*ebsdnl%StepX
    end if 

    write(str3,'(I8)') 0
    write(str8,'(I8)') 0 ! integer zero error; was indx, which is now moved to BANDS
    eu = euler(1) - 90.0 ! conversion from TSL to Oxford convention
    if (eu.lt.0) eu = eu + 360.0
    write(str5,'(F12.3)') eu  
    eu = euler(2)
    if (eu.lt.0) eu = eu + 360.0
    write(str6,'(F12.3)') eu
! intercept the hexagonal case, for which we need to subtract 30° from the third Euler angle
! Note: after working with Lionel Germain, we concluded that we do not need to subtract 30° 
! in the ctf file, because the fundamental zone is already oriented according to the Oxford
! convention... That means that we need to subtract the angle for the .ang file (to be implemented)
! [modified by MDG on 3/5/18]
    if ((LaueGroup(phaseID(ii)).eq.8).or.(LaueGroup(phaseID(ii)).eq.9)) euler(3) = euler(3) - 30.0
    eu = euler(3)
    if (eu.lt.0) eu = eu + 360.0
    write(str7,'(F12.3)') eu
    write(str4,'(F12.6)') dplist(ii,phaseID(ii))   ! this replaces MAD
! the following two parameters need to be modified to contain more meaningful information
    write(str9,'(I8)') BCval   ! OSM value in range [0..255]
    write(str10,'(I8)') BSval  !  IQ value in range [0..255]
! Oxford 3D files have four additional integer columns;
! GrainIndex
! GrainRandomColourR
! GrainRandomColourG
! GrainRandomColourB
!
    write(np,"(I1)") phaseID(ii)
    write(dataunit2,'(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)') np,TAB,trim(adjustl(str1)),TAB,&
    trim(adjustl(str2)),TAB,trim(adjustl(str3)),TAB,trim(adjustl(str8)),TAB,trim(adjustl(str5)),&
    TAB,trim(adjustl(str6)),TAB,trim(adjustl(str7)),TAB,trim(adjustl(str4)),TAB,trim(adjustl(str9)),&
    TAB,trim(adjustl(str10))
end do

close(dataunit2,status='keep')

! reset the Euler angles to radians
eangles = eangles * sngl(cPi)/180.0

end subroutine ctfmerge_writeFile

!--------------------------------------------------------------------------
!
! SUBROUTINE:angmerge_writeFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Write a merged *.ang output file with EBSD data (TSL format)
!
!> @param ebsdnl namelist
!> @param ipar  series of integer dimensions
!> @param eangles of Euler angle triplets
!> @param phaseID phase identifier array
!> @param dplistdot product array
!> @param IQmap pattern quality array
!
!> @date 08/18/19 MDG 1.0 original based opn angebsd_writeFile
!--------------------------------------------------------------------------
recursive subroutine angmerge_writeFile(ebsdnl,xtalname,ipar,eangles,phaseID,dplist,IQmap)
!DEC$ ATTRIBUTES DLLEXPORT :: angmerge_writeFile

use NameListTypedefs
use constants 


IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
!f2py intent(in,out) ::  ebsdnl
character(fnlen),INTENT(IN)                         :: xtalname(5)
integer(kind=irg),INTENT(IN)                        :: ipar(4)
real(kind=sgl),INTENT(IN)                           :: eangles(3,ipar(1),ipar(2))
integer(kind=irg),INTENT(IN)                        :: phaseID(ipar(1))
real(kind=sgl),INTENT(IN)                           :: dplist(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: IQmap(ipar(1))

integer(kind=irg)                                   :: ierr, ii, indx, SGnum, iph
character(fnlen)                                    :: angname, xtn
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
character(1)                                        :: np
character                                           :: TAB = CHAR(9)
character(2)                                        :: TSLsymmetry
real(kind=sgl)                                      :: euler(3), s, BSval
real(kind=dbl)                                      :: cellparams(6), dtor
logical                                             :: donotuseindexarray

dtor = cPi/180.D0

! open the file (overwrite old one if it exists)
angname = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%angfile)
angname = EMsoft_toNativePath(angname)
open(unit=dataunit2,file=trim(angname),status='unknown',action='write',iostat=ierr)

! this requires a lot of information...
write(dataunit2,'(A)') '# TEM_PIXperUM          1.000000'
s = ( float(ebsdnl%numsx)*0.5 + ebsdnl%xpc ) / float(ebsdnl%numsx)      ! x-star
write(dataunit2,'(A,F9.6)') '# x-star                ', s
s = ( float(ebsdnl%numsy)*0.5 + ebsdnl%ypc ) / float(ebsdnl%numsy)      ! y-star
write(dataunit2,'(A,F9.6)') '# y-star                ', s
s = ebsdnl%L / ( ebsdnl%delta * float(ebsdnl%numsx) )                   ! z-star
write(dataunit2,'(A,F9.6)') '# z-star                ', s 
write(dataunit2,'(A,F9.6)') '# WorkingDistance       ', ebsdnl%WD       ! this quantity is not used in EMsoft
write(dataunit2,'(A)') '#'

do iph=1,ipar(2)
  write (np,"(I1)") iph
  write(dataunit2,'(A)') '# Phase '//np

  xtn = trim(xtalname(iph))
  ii = scan(xtn,'.')
  angname = xtn(1:ii-1)
  write(dataunit2,'(A)') '# MaterialName    '//trim(angname)
  write(dataunit2,'(A)') '# Formula       '//trim(angname)
  write(dataunit2,'(A)') '# Info          patterns indexed using EMsoft::EMEBSDDI'

  !==========================
  ! get space group, lattice parameters, and TSL symmetry string
  call getXtalData(xtalname(iph),cellparams,SGnum,TSLsymmetry)

  ! symmetry string
  write(dataunit2,'(A)') '# Symmetry              '//TSLsymmetry

  ! lattice parameters
  cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
  write(str1,'(F8.3)') cellparams(1)
  write(str2,'(F8.3)') cellparams(2)
  write(str3,'(F8.3)') cellparams(3)
  str1 = adjustl(str1)
  str2 = adjustl(str2)
  str3 = adjustl(str3)
  str1 = trim(str1)//' '//trim(str2)//' '//trim(str3)

  ! unit cell angles
  write(str4,'(F8.3)') cellparams(4)
  write(str5,'(F8.3)') cellparams(5)
  write(str6,'(F8.3)') cellparams(6)
  str4 = adjustl(str5)
  str5 = adjustl(str5)
  str6 = adjustl(str6)
  str1 = trim(str1)//TAB//trim(str4)//' '//trim(str5)//' '//trim(str6)

  write(dataunit2,'(A)') '# LatticeConstants      '//trim(str1)
  !==========================

  ! next we need to get the hklFamilies ranked by kinematical intensity, going out to some value
  ! this is probably not necessary [based on Stuart's feedback], so we comment it all out
  write(dataunit2,'(A)') '# NumberFamilies        0'
  ! write(dataunit2,'(A)') '# hklFamilies      3  1  1 1 0.000000'

end do 

!==========================
! write(dataunit2,'(A)') '# Categories 0 0 0 0 0'
! write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# GRID: SqrGrid'
write(dataunit2,'(A,F9.6)') '# XSTEP: ', ebsdnl%StepX
write(dataunit2,'(A,F9.6)') '# YSTEP: ', ebsdnl%StepY
write(dataunit2,'(A,I5)') '# NCOLS_ODD: ',ipar(3)
write(dataunit2,'(A,I5)') '# NCOLS_EVEN: ',ipar(3)
write(dataunit2,'(A,I5)') '# NROWS: ', ipar(4)
write(dataunit2,'(A)') '#'
write(dataunit2,'(A,A)') '# OPERATOR:   ', trim(EMsoft_getUsername())
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# SAMPLEID:'
write(dataunit2,'(A)') '#'
write(dataunit2,'(A)') '# SCANID:'
write(dataunit2,'(A)') '#'

! ok, next we have the actual data, which is in the following order
! * phi1                      -> Phi1
! * phi                       -> Phi
! * phi2                      -> Phi2
! * x pos                     -> pixel position
! * y pos                     -> pixel position
! * image quality             -> iq
! * confidence index          -> resultmain
! * phase                     -> 1 (since there is only one phase in each indexing run)
! the second entry after the arrow is the EMsoft parameter that we write into that location
! these 8 entries must be present...

! go through the entire array and write one line per sampling point
do ii = 1,ipar(1)
    write (np,"(I1)") phaseID(ii)
    BSval = 255.0 * IQmap(ii)
! should we use the index array or not?
    euler = eangles(1:3,ii,phaseID(ii))
    write(str1,'(A,F8.5)') ' ',euler(1)
    write(str2,'(A,F8.5)') ' ',euler(2)
    write(str3,'(A,F8.5)') ' ',euler(3)
! sampling coordinates [interchanged x and y on 05/28/19, MDG] 
    if (sum(ebsdnl%ROI).ne.0) then
      write(str4,'(A,F12.5)') ' ',float(MODULO(ii-1,ebsdnl%ROI(3)))*ebsdnl%StepX
      write(str5,'(A,F12.5)') ' ',float(floor(float(ii-1)/float(ebsdnl%ROI(3))))*ebsdnl%StepY
    else
      write(str4,'(A,F12.5)') ' ',float(MODULO(ii-1,ebsdnl%ipf_wd))*ebsdnl%StepX
      write(str5,'(A,F12.5)') ' ',float(floor(float(ii-1)/float(ebsdnl%ipf_wd)))*ebsdnl%StepY
    end if 
! Image Quality (using the Krieger Lassen pattern sharpness parameter iq)
    write(str6,'(A,F6.1)') ' ',BSval  !  IQ value in range [0.0 .. 255.0]
    write(str7,'(A,F6.3)') ' ',dplist(ii,phaseID(ii))   ! this replaces MAD
    write(str8,'(A)') '  '//np 
!
    write(dataunit2,"(A,' ',A,' ',A,' ',A,' ',A,' ',A,' ',A,' ',A)") trim(adjustl(str1)),trim(adjustl(str2)),&
                                            trim(adjustl(str3)),trim(adjustl(str4)),trim(adjustl(str5)),&
                                            trim(adjustl(str6)),trim(adjustl(str7)),trim(adjustl(str8))
end do

close(dataunit2,status='keep')

end subroutine angmerge_writeFile






end module EBSDiomod
