! ###################################################################
! Copyright (c) 2016-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:povray.f90
!--------------------------------------------------------------------------
!
! MODULE: povray
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief all povray-related routines
!
!> @details routines to generate output for PoVray file creation; this is mostly
!> used for the visualization of orientation data sets in one of the many
!> representations and fundamental zones...
! 
!> @date  09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------

module povray

use io
use local
use files
use rotations
use constants
use Lambert
use quaternions

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_openFile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add the camera command to the current PoVRay file
! 
!> @param dunit output unit number
!> @param locationline string that defines the camera position
!
!> @date    09/08/16 MDG 1.0 original
!> @date    01/30/17 MDG 1.1 added optional name list file variable
!--------------------------------------------------------------------------
recursive subroutine PoVRay_openFile(dunit, povray_filename, nmlfile)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_openFile

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
character(fnlen),INTENT(IN)           :: povray_filename
character(fnlen),INTENT(IN),OPTIONAL  :: nmlfile

character(fnlen)                      :: fname, line, cwd
integer(kind=irg)                     :: io

open(unit=dunit,file=trim(povray_filename),form='formatted',status='unknown')

write (dunit,"(A)") "//Persistence of Vision Ray Tracer Scene Description File"
write (dunit,"(A)") "//Created by EMsoft package"

! should we copy the complete namelist file as a comment at this point ?
if (PRESENT(nmlfile)) then
  call getcwd(cwd)
  fname = trim(cwd)//'/'//trim(nmlfile)
  fname = EMsoft_toNativePath(fname)
  write(dunit,"(A)") "// "
  write(dunit,"(A)") "// contents of the namelist used to create this file "//trim(fname)
  open(unit=88,file=trim(fname),status='unknown',form='formatted')
  do
    read(88,"(A)",iostat=io) line
    if (io.eq.0) then
      write(dunit,"(A)") "// "//trim(line)
    else 
      EXIT 
    end if
  end do
  close(unit=88,status='keep')
  write(dunit,"(A)") "// "
end if 

! and write the include statements to the file
write (dunit,"(A)") "#include ""colors.inc"""
write (dunit,"(A)") "#include ""textures.inc"""
write (dunit,"(A)") "#include ""glass.inc"""
write(dunit,"(A)") "// "
write (dunit,"(A)") "global_settings"
write (dunit,"(A)") "{  ambient_light <1,1,1>"
write (dunit,"(A)") "   assumed_gamma 1"
write (dunit,"(A)") "}"

end subroutine PoVRay_openFile

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_setCamera
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add the camera command to the current PoVRay file
! 
!> @param dunit output unit number
!> @param locationline string that defines the camera position
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_setCamera(dunit, locationline, skyline)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_setCamera

IMPLICIT NONE

integer(kind=irg),INTENT(IN)         :: dunit
character(fnlen),INTENT(IN)          :: locationline
character(fnlen),INTENT(IN),OPTIONAL :: skyline

write (dunit,"(A)") " "
write (dunit,"(A)") "camera {"
write (dunit,"(A)") "perspective "
write (dunit,"(A)") trim(locationline)
if (PRESENT(skyline)) then
  write (dunit,"(A)") trim(skyline)
else
  write (dunit,"(A)") "sky < 0.0, 0.0, 1.0>"
end if
write (dunit,"(A)") "right y * 1"
write (dunit,"(A)") "up z"
write (dunit,"(A)") "angle 50"
write (dunit,"(A)") "look_at < 0.0, 0.0, 0.0>"
write (dunit,"(A)") "}"


end subroutine PoVRay_setCamera

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_setLightSource
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a light command to the current PoVRay file
! 
!> @param dunit output unit number
!> @param lightline string that defines the camera position
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_setLightSource(dunit, lightline, nobackground)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_setLightSource

IMPLICIT NONE

integer(kind=irg),INTENT(IN)         :: dunit
character(fnlen),INTENT(IN)          :: lightline
logical,OPTIONAL,INTENT(IN)          :: nobackground

write (dunit,"(A)") " "
write (dunit,"(A)") "light_source {"
write (dunit,"(A)") trim(lightline)
write (dunit,"(A)") "color White"
write (dunit,"(A)") "media_interaction on"
write (dunit,"(A)") "media_attenuation on"
write (dunit,"(A)") "shadowless"
write (dunit,"(A)") "}"
if (.not.PRESENT(nobackground)) then
  write (dunit,"(A)") "background { color White }"
  write (dunit,"(A)") " "
end if

end subroutine PoVRay_setLightSource

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_addEulerBox
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a wireframe Euler Box to the current PoVRay file
! 
!> @param dunit output unit number
!> @param sphereRadius radius of sphere
!
!> @date    11/23/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_addEulerBox(dunit)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_addEulerBox

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit

write (dunit,"(A)") "cylinder {<-3.141593,-1.570796,-3.141593>,<-3.141593, 1.570796,-3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {<-3.141593,-1.570796, 3.141593>,<-3.141593, 1.570796, 3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {< 3.141593,-1.570796,-3.141593>,< 3.141593, 1.570796,-3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {< 3.141593,-1.570796, 3.141593>,< 3.141593, 1.570796, 3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {<-3.141593,-1.570796,-3.141593>,<-3.141593,-1.570796, 3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {<-3.141593,-1.570796, 3.141593>,< 3.141593,-1.570796, 3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {< 3.141593,-1.570796, 3.141593>,< 3.141593,-1.570796,-3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {< 3.141593,-1.570796,-3.141593>,<-3.141593,-1.570796,-3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {<-3.141593, 1.570796,-3.141593>,<-3.141593, 1.570796, 3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {<-3.141593, 1.570796, 3.141593>,< 3.141593, 1.570796, 3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {< 3.141593, 1.570796, 3.141593>,< 3.141593, 1.570796,-3.141593>, 0.005 pigment {color Green*0.7}}"
write (dunit,"(A)") "cylinder {< 3.141593, 1.570796,-3.141593>,<-3.141593, 1.570796,-3.141593>, 0.005 pigment {color Green*0.7}}"

end subroutine PoVRay_addEulerBox

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_declare_DF3file
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief set up the code to include a 3D density field (df3) file
! 
!> @param dunit output unit number
!> @param df3 file name
!> @param levelset if TRUE, then different colors are used for level contours
!
!> @date    11/23/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_declare_DF3file(dunit, df3name, levelset)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_declare_DF3file

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
character(fnlen),INTENT(IN)           :: df3name
logical,OPTIONAL,INTENT(IN)           :: levelset

logical         :: levels

write (dunit,"(A)") ''
write (dunit,"(A)") '#declare DENS = interior'
write (dunit,"(A)") '  {  media'
write (dunit,"(A)") '    {  intervals 100                   // number of ray-intervals'
write (dunit,"(A)") '      ratio 0.5'
write (dunit,"(A)") '      samples 4,4                     // maximum,minimum of samples per voxel'
write (dunit,"(A)") '      method 2                        // 1, 2 or 3 (3 requires samples 3,3)'
write (dunit,"(A)") '      emission 20*<1,1,1>'
write (dunit,"(A)") '      absorption <1,1,1>'
write (dunit,"(A)") '      scattering { 1, <0,0,0> }'
write (dunit,"(A)") '      confidence 0.9                // default: 0.9'
write (dunit,"(A)") '      variance 1/100                // default: 1/128'
write (dunit,"(A)") '      density'
     write (dunit,"('      {  density_file df3 ""',A,'""')") trim(df3name)
write (dunit,"(A)") '        interpolate 1'
write (dunit,"(A)") '        color_map                    // colour map with (smooth) linear transition(s)'

levels = .FALSE.
if (PRESENT(levelset)) then
  if (levelset.eqv..TRUE.) then
    levels = .TRUE.
  end if
end if

if (levels.eqv..TRUE.) then 
  write (dunit,"(A)") '        {  [0.0 rgb <0.0,0.0,0.0>] '
  write (dunit,"(A)") '           [0.1 rgb <0.6,0.0,0.0>]'
  write (dunit,"(A)") '           [0.2 rgb <0.0,0.6,0.0>]'
  write (dunit,"(A)") '           [0.4 rgb <0.2,0.3,0.8>]'
  write (dunit,"(A)") '           [0.6 rgb <0.6,0.5,0.8>]'
  write (dunit,"(A)") '           [1.0 rgb <1.0,1.0,1.0>]'
else
  write (dunit,"(A)") '        {  [0.1 rgb <0.0,0.0,0.0>] '
  write (dunit,"(A)") '           [0.2 rgb <0.5,0.0,0.0>]'
  write (dunit,"(A)") '           [0.3 rgb <1.0,0.0,0.0>]'
  write (dunit,"(A)") '           [0.4 rgb <1.0,0.3,0.0>]'
  write (dunit,"(A)") '           [0.5 rgb <1.0,0.6,0.0>]'
  write (dunit,"(A)") '           [0.6 rgb <1.0,1.0,0.0>]'
  write (dunit,"(A)") '           [0.7 rgb <1.0,1.0,0.3>]'
  write (dunit,"(A)") '           [0.8 rgb <1.0,1.0,0.6>]'
  write (dunit,"(A)") '           [0.9 rgb <1.0,1.0,1.0>]'
end if
write (dunit,"(A)") '        }'
write (dunit,"(A)") '      }'
write (dunit,"(A)") '    }'
write (dunit,"(A)") '  }'
write (dunit,"(A)") ''
write (dunit,"(A)") '#declare renderbox = box'
write (dunit,"(A)") '  {  <0,0,0>, <1,1,1>'
write (dunit,"(A)") '     pigment { rgbt <0,0,0,1> }'
write (dunit,"(A)") '     hollow'
write (dunit,"(A)") '     interior { DENS }'
write (dunit,"(A)") '  }'

end subroutine PoVRay_declare_DF3file

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_write_DF3file
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief set up the code to include a 3D density field (df3) file
! 
!> @param dunit output unit number
!> @param df3 file name
!> @param volume the volume array to be written to the file
!> @param ndims dimensions of the volume array
!> @param scalingmode  'lin' or 'log' or 'lev'
!
!> @date    11/23/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_write_DF3file(dunit, df3name, volume, ndims, scalingmode)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_write_DF3file

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
character(fnlen),INTENT(IN)           :: df3name
integer(kind=irg),INTENT(IN)          :: ndims(3)
real(kind=sgl),INTENT(INOUT)          :: volume(-ndims(1):ndims(1),-ndims(2):ndims(2),-ndims(3):ndims(3))
!f2py intent(in,out) ::  volume
character(3),INTENT(IN)               :: scalingmode

integer(kind=ish)                     :: ivol(-ndims(1):ndims(1),-ndims(2):ndims(2),-ndims(3):ndims(3))
integer(kind=ish)                     :: idims(3), mval
real(kind=sgl)                        :: mi, ma, levels(6)
integer(kind=irg)                     :: recno, i, j, k

! This format is described on the following web pages:
! http://wwwmpa.mpa-garching.mpg.de/~mselig/povray/povray.html
! http://www.povray.org/documentation/view/3.6.1/374/
!
! Essentially, this is a binary file with first 3 2-byte integers
! with the array dimensions, and then the array itself as 4-byte integers.
! Note that this file *must* be written in big-endian format (most significant
! byte first).  This can be achieved by using the CONVERT='BIG_ENDIAN' qualifier
! when the file is opened.

! we will write this as a file of 16 bit integers, so we need to rescale them all
! to the correct range
mval = int(2**15-1,kind=ish)
if ((scalingmode.eq.'log').or.(scalingmode.eq.'lev')) volume = alog10(volume+1.0)
mi = minval(volume)
ma = maxval(volume)
volume = (volume - mi) / (ma - mi)

write (*,*) 'volume range ', mi, ma

if (scalingmode.eq.'lev') then
! we discretize the logarithmic values into bins (levels) which are then displayed
! as nested contour surfaces (ideally) by PoVRay's Density_field routine...
! Since we are testing this capability, we'll only include 5 different contour levels
! for now; this can be modified later on...
  levels = (/ 0.0, 0.4, 0.6, 0.8, 0.9, 1.0 /)
! levels = (/ 0.1, 0.2, 0.4, 0.6, 0.8, 1.0 /)
  do i=2,6
    where ((volume.ge.levels(i-1)).and.(volume.lt.levels(i))) volume = levels(i-1)
  end do
endif

volume = volume * mval
ivol = int(volume, kind=ish)

! open the file and write the rescaled integer data
open(unit=dunit,file=trim(df3name),status='unknown',access="DIRECT",action="WRITE", &
     recl=2,form='unformatted',convert='big_endian')
idims = 2*ndims+1
recno = 1
do i=1,3
  write (dunit,rec=recno) idims(i)
  recno = recno+1
end do
do k=-ndims(3),ndims(3)
  do j=-ndims(2),ndims(2)
    do i=-ndims(1),ndims(1)
      write (dunit,rec=recno) ivol(i,j,k)
      recno = recno+1
    end do
  end do
end do
close(dunit,status='keep')

end subroutine PoVRay_write_DF3file

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_addWireFrameSphere
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a wireframe sphere to the current PoVRay file
! 
!> @param dunit output unit number
!> @param sphereRadius radius of sphere
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_addWireFrameSphere(dunit, sphereRadius)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_addWireFrameSphere

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
real(kind=dbl),INTENT(IN)             :: sphereRadius

character(12)                         :: px

write (dunit,"(A)") "#macro WireFrameSphere(NrLongitudes, NrLatitudes, Rmaj, Rmin)"
write (dunit,"(A)") ""
write (dunit,"(A)") "  #local dLongitude = 360/NrLongitudes;"
write (dunit,"(A)") "  #local dLatitude = 180/NrLatitudes;"
write (dunit,"(A)") "  #local Cnt = 0;"
write (dunit,"(A)") "  #while (Cnt < NrLongitudes)"
write (dunit,"(A)") "    #local Longitude = Cnt*dLongitude;"
write (dunit,"(A)") "    difference {"
write (dunit,"(A)") "      torus { Rmaj, Rmin }"
write (dunit,"(A)") "      plane { -z, 0 }"
write (dunit,"(A)") "      rotate -90*z"
write (dunit,"(A)") "      rotate Longitude*y"
write (dunit,"(A)") "    }"
write (dunit,"(A)") "    #local Cnt = Cnt + 1;"
write (dunit,"(A)") "  #end // while"
write (dunit,"(A)") ""
write (dunit,"(A)") "  #local Cnt = 1;"
write (dunit,"(A)") "  #while (Cnt < NrLatitudes)"
write (dunit,"(A)") "    #local Latitude = radians(Cnt*dLatitude - 90);"
write (dunit,"(A)") "    torus {"
write (dunit,"(A)") "      Rmaj*cos(Latitude), Rmin"
write (dunit,"(A)") "      translate Rmaj*sin(Latitude)*y"
write (dunit,"(A)") "    }"
write (dunit,"(A)") "    #local Cnt = Cnt + 1;"
write (dunit,"(A)") "  #end // while"
write (dunit,"(A)") ""
write (dunit,"(A)") "#end // macro WireFrameSphere"
write (dunit,"(A)") ""

write (px,"(F12.6)") sphereRadius

write (dunit,"(A)") "#declare Rglobe = "//px//";"
write (dunit,"(A)") "#declare Rwireframe = 0.0033;"
write (dunit,"(A)") "#declare Rspheres = Rwireframe*2;"
write (dunit,"(A)") ""
write (dunit,"(A)") "// Number of longitude intervals"
write (dunit,"(A)") "#declare Longitudes = 12;"
write (dunit,"(A)") ""
write (dunit,"(A)") "// Number of latitude intervals"
write (dunit,"(A)") "#declare Latitudes = 6;"
write (dunit,"(A)") ""
write (dunit,"(A)") ""
write (dunit,"(A)") "union {"
write (dunit,"(A)") "  WireFrameSphere(Longitudes, Latitudes, Rglobe, Rwireframe)"
write (dunit,"(A)") "  rotate 90.0*x"
write (dunit,"(A)") "  pigment { color Black*0.7 }"
write (dunit,"(A)") "}"

end subroutine PoVRay_addWireFrameSphere


!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_addReferenceFrame
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a reference frame to the current PoVRay file
! 
!> @param dunit output unit number
!> @param ac maximum semi-length of axis
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_addReferenceFrame(dunit, ac, cylr)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_addReferenceFrame

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
real(kind=dbl),INTENT(IN)             :: ac
real(kind=dbl),OPTIONAL,INTENT(IN)    :: cylr

real(kind=dbl)                        :: cc

if (present(cylr).eqv..TRUE.) then
  cc = cylr
else
  cc = 0.005D0
end if


write (dunit,"('cylinder { <',2(F9.6,','),F9.6,'>,<',2(F9.6,','),F9.6,'>,',F9.6,' pigment { color Red*0.7 } }')") &
      -ac, 0.0, 0.0,  ac, 0.0, 0.0, cc
write (dunit,"('cylinder { <',2(F9.6,','),F9.6,'>,<',2(F9.6,','),F9.6,'>,',F9.6,' pigment { color Green*0.7 } }')") &
      0.0, -ac, 0.0, 0.0,  ac, 0.0, cc
write (dunit,"('cylinder { <',2(F9.6,','),F9.6,'>,<',2(F9.6,','),F9.6,'>,',F9.6,' pigment { color Blue*0.7 } }')") &
      0.0, 0.0, -ac, 0.0, 0.0,  ac, cc

end subroutine PoVRay_addReferenceFrame

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_addSphere
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a sphere to the current PoVRay file
! 
!> @param dunit output unit number
!> @param ctr center coordinates of the sphere
!> @param radius sphere radius
!> @param rgb color triplet (RGB)
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_addSphere(dunit, ctr, radius, rgb)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_addSphere

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
real(kind=dbl),INTENT(IN)             :: ctr(3)
real(kind=dbl),INTENT(IN)             :: radius 
real(kind=sgl),INTENT(IN)             :: rgb(3)

write (dataunit,"('sphere { <',2(F9.6,','),F9.6,'>,',F9.6,' material { texture { pigment { rgb <', &
                &2(F9.6,','),F9.6,'>}}}}')") ctr(1:3), radius, rgb(1:3)

end subroutine PoVRay_addSphere


!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_addCylinder
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a cylinder to the current PoVRay file
! 
!> @param dunit output unit number
!> @param p1 starting point of cylinder (on axis)
!> @param p2 end point of cylinder (on axis)
!> @param radius cylinder radius
!> @param rgb color triplet (RGB)
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_addCylinder(dunit, p1, p2, radius, rgb)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_addCylinder

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
real(kind=dbl),INTENT(IN)             :: p1(3), p2(3)
real(kind=dbl),INTENT(IN)             :: radius 
real(kind=sgl),INTENT(IN)             :: rgb(3)

write (dunit,"('cylinder { <',2(F9.6,','),F9.6,'>,<',2(F9.6,','),F9.6,'>,', F9.6,' pigment { ', &
             &'rgb <',2(F9.6,','),F9.6,'>}}')") p1(1:3), p2(1:3), radius, rgb(1:3)

end subroutine PoVRay_addCylinder


!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_addCubochoricCube
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  add a reference frame to the current PoVRay file
! 
!> @param dunit output unit number
!
!> @date    09/08/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_addCubochoricCube(dunit)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_addCubochoricCube

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit

real(kind=dbl)                        :: ac

! create the cubochoric cube
ac = 0.5 * LPs%ap
call PoVRay_addCylinder(dunit,(/ -ac,  ac,  ac /), (/ ac,  ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac,  ac, -ac /), (/ ac,  ac, -ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac, -ac,  ac /), (/ ac, -ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac, -ac, -ac /), (/ ac, -ac, -ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))

call PoVRay_addCylinder(dunit,(/  ac, -ac,  ac /), (/ ac,  ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac, -ac,  ac /), (/-ac,  ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/  ac, -ac, -ac /), (/ ac,  ac, -ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac, -ac, -ac /), (/-ac,  ac, -ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))

call PoVRay_addCylinder(dunit,(/  ac,  ac, -ac /), (/ ac,  ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac,  ac, -ac /), (/-ac,  ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/  ac, -ac, -ac /), (/ ac, -ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))
call PoVRay_addCylinder(dunit,(/ -ac, -ac, -ac /), (/-ac, -ac,  ac /), 0.005D0, (/ 0.7, 0.7, 0.7 /))

end subroutine PoVRay_addCubochoricCube

!--------------------------------------------------------------------------
!
! function: PoVRay_fliprotationmatrix
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief  change a rotation matrix to the POVray axes convention (left handed)
! 
!> @param mat input matrix
!
!> @date    05/05/18 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function PoVRay_fliprotationmatrix(M) result(O)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_fliprotationmatrix

IMPLICIT NONE

real(kind=dbl),INTENT(IN)         :: M(3,3)
real(kind=dbl)                    :: O(3,3)

O = reshape( (/ M(1,1), M(1,3), M(1,2), M(3,1), M(3,3), M(3,2), M(2,1), M(2,3), M(2,2) /), (/ 3,3 /) )

end function PoVRay_fliprotationmatrix

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! below this line are a number of routines for different rotation representations
! and different crystallographic fundamental zones... below this line, there
! should be no direct writing to the dunit file; all writes should pass 
! through the routines above.
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_getpos_FZ432
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 432
! 
!> @param dims dimensions for subsequent arrays
!> @param cpos vertex coordinates
!> @param s_edge first set of edge connectivities
!> @param t_edge second set of edge connectivities
!> @param ns, d, nt  auxiliary parameters
!> @param MFZ (optional) return coordinates for Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_getpos_FZ432(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_getpos_FZ432

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dims(3)
real(kind=dbl),INTENT(INOUT)          :: cpos(3,dims(1))
!f2py intent(in,out) ::  cpos
integer(kind=irg),INTENT(INOUT)       :: s_edge(2,dims(2))
!f2py intent(in,out) ::  s_edge
integer(kind=irg),INTENT(INOUT)       :: t_edge(2,dims(3))
!f2py intent(in,out) ::  t_edge
integer(kind=irg),INTENT(OUT)         :: ns
real(kind=dbl),INTENT(OUT)            :: d
integer(kind=irg),INTENT(OUT)         :: nt
logical,OPTIONAL,INTENT(IN)           :: MFZ

real(kind=dbl)          :: a = 0.41421356237D0, c = 0.17157287525D0, dt = 0.34314575050D0, ds = 0.6340506711D0, &
                           dd, e = 0.29289323D0, f = 0.333333333D0

d = 0.610395774912

if (present(MFZ)) then
  if (MFZ) then 

! define the coordinates of the cubic Mackenzie FZ in Rodrigues Space
    cpos(1:3, 1) = (/  0.D0,  0.D0,  0.D0 /)
    cpos(1:3, 2) = (/  a,  0.D0,  0.D0 /)
    cpos(1:3, 3) = (/  a,  a,  0.D0 /)

    cpos(1:3, 4) = (/  a,  a,  c /)
    cpos(1:3, 5) = (/  f,  f,  f /)
    cpos(1:3, 6) = (/  a,  e,  e /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1, 2 /)
    s_edge(1:2, 2) = (/  1, 3 /)
    s_edge(1:2, 3) = (/  1, 5 /)
    s_edge(1:2, 4) = (/  2, 3 /)
    s_edge(1:2, 5) = (/  2, 6 /)
    s_edge(1:2, 6) = (/  3, 4 /)
    s_edge(1:2, 7) = (/  4, 5 /)
    s_edge(1:2, 8) = (/  4, 6 /)
    s_edge(1:2, 9) = (/  5, 6 /)
   end if 
! define the coordinates of the cubic FZ in Rodrigues Space
 else
    cpos(1:3, 1) = (/  a,  a,  c /)
    cpos(1:3, 2) = (/  c,  a,  a /)
    cpos(1:3, 3) = (/  a,  c,  a /)

    cpos(1:3, 4) = (/ -a,  a,  c /)
    cpos(1:3, 5) = (/ -c,  a,  a /)
    cpos(1:3, 6) = (/ -a,  c,  a /)

    cpos(1:3, 7) = (/ -a, -a,  c /)
    cpos(1:3, 8) = (/ -c, -a,  a /)
    cpos(1:3, 9) = (/ -a, -c,  a /)

    cpos(1:3,10) = (/  a, -a,  c /)
    cpos(1:3,11) = (/  c, -a,  a /)
    cpos(1:3,12) = (/  a, -c,  a /)

    cpos(1:3,13) = (/  a,  a, -c /)
    cpos(1:3,14) = (/  a,  c, -a /)
    cpos(1:3,15) = (/  c,  a, -a /)

    cpos(1:3,16) = (/ -a,  a, -c /)
    cpos(1:3,17) = (/ -a,  c, -a /)
    cpos(1:3,18) = (/ -c,  a, -a /)

    cpos(1:3,19) = (/ -a, -a, -c /)
    cpos(1:3,20) = (/ -a, -c, -a /)
    cpos(1:3,21) = (/ -c, -a, -a /)

    cpos(1:3,22) = (/  a, -a, -c /)
    cpos(1:3,23) = (/  a, -c, -a /)
    cpos(1:3,24) = (/  c, -a, -a /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

    ! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  3, 12 /)
    s_edge(1:2, 2) = (/ 10, 22 /)
    s_edge(1:2, 3) = (/ 14, 23 /)
    s_edge(1:2, 4) = (/  1, 13 /)
    s_edge(1:2, 5) = (/  2,  5 /)
    s_edge(1:2, 6) = (/  8, 11 /)
    s_edge(1:2, 7) = (/ 21, 24 /)
    s_edge(1:2, 8) = (/ 15, 18 /)
    s_edge(1:2, 9) = (/  6,  9 /)
    s_edge(1:2,10) = (/  7, 19 /)
    s_edge(1:2,11) = (/ 17, 20 /)
    s_edge(1:2,12) = (/  4, 16 /)

    t_edge(1:2, 1) = (/  1,  2 /)
    t_edge(1:2, 2) = (/  2,  3 /)
    t_edge(1:2, 3) = (/  3,  1 /)

    t_edge(1:2, 4) = (/  4,  5 /)
    t_edge(1:2, 5) = (/  5,  6 /)
    t_edge(1:2, 6) = (/  6,  4 /)

    t_edge(1:2, 7) = (/  7,  8 /)
    t_edge(1:2, 8) = (/  8,  9 /)
    t_edge(1:2, 9) = (/  9,  7 /)

    t_edge(1:2,10) = (/ 10, 11 /)
    t_edge(1:2,11) = (/ 11, 12 /)
    t_edge(1:2,12) = (/ 12, 10 /)

    t_edge(1:2,13) = (/ 13, 14 /)
    t_edge(1:2,14) = (/ 14, 15 /)
    t_edge(1:2,15) = (/ 15, 13 /)

    t_edge(1:2,16) = (/ 16, 17 /)
    t_edge(1:2,17) = (/ 17, 18 /)
    t_edge(1:2,18) = (/ 18, 16 /)

    t_edge(1:2,19) = (/ 19, 20 /)
    t_edge(1:2,20) = (/ 20, 21 /)
    t_edge(1:2,21) = (/ 21, 19 /)

    t_edge(1:2,22) = (/ 22, 23 /)
    t_edge(1:2,23) = (/ 23, 24 /)
    t_edge(1:2,24) = (/ 24, 22 /)
end if

end subroutine PoVRay_getpos_FZ432

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_getpos_FZ23
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 23
! 
!> @param dims dimensions for subsequent arrays
!> @param cpos vertex coordinates
!> @param s_edge first set of edge connectivities
!> @param t_edge second set of edge connectivities
!> @param ns, d, nt  auxiliary parameters
!> @param MFZ (optional) return coordinates for Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_getpos_FZ23(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_getpos_FZ23

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dims(3)
real(kind=dbl),INTENT(INOUT)          :: cpos(3,dims(1))
!f2py intent(in,out) ::  cpos
integer(kind=irg),INTENT(INOUT)       :: s_edge(2,dims(2))
!f2py intent(in,out) ::  s_edge
integer(kind=irg),INTENT(INOUT)       :: t_edge(2,dims(3))
!f2py intent(in,out) ::  t_edge
integer(kind=irg),INTENT(OUT)         :: ns
real(kind=dbl),INTENT(OUT)            :: d
integer(kind=irg),INTENT(OUT)         :: nt
logical,OPTIONAL,INTENT(IN)           :: MFZ

real(kind=dbl)          :: a = 1.D0, b = 0.0D0, c = 0.5773502692D0, e = 0.333333333D0, &
                           ds = 0.6340506711D0, dt = 1.4142135623730D0, dd, zz = 0.D0, oo = 1.D0

d = 1.0

if (present(MFZ)) then
  if (MFZ) then 

! define the coordinates of the cubic Mackenzie FZ in Rodrigues Space
    cpos(1:3, 1) = (/  b,  b,  b /)
    cpos(1:3, 2) = (/  a,  b,  b /)
    cpos(1:3, 3) = (/  b,  a,  b /)
    cpos(1:3, 4) = (/  e,  e,  e /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1, 2 /)
    s_edge(1:2, 2) = (/  1, 3 /)
    s_edge(1:2, 3) = (/  1, 4 /)
    s_edge(1:2, 4) = (/  2, 3 /)
    s_edge(1:2, 5) = (/  2, 4 /)
    s_edge(1:2, 6) = (/  3, 4 /)
   end if 
! define the coordinates of the cubic FZ in Rodrigues Space
 else
  ! define the coordinates of the cubic D3 FZ in Rodrigues Space
    cpos(1:3, 1) = (/  a,  b,  b /)
    cpos(1:3, 2) = (/  b,  a,  b /)
    cpos(1:3, 3) = (/ -a,  b,  b /)
    cpos(1:3, 4) = (/  b, -a,  b /)
    cpos(1:3, 5) = (/  b,  b,  a /)
    cpos(1:3, 6) = (/  b,  b, -a /)

    cpos = cpos / d

    ns = 200
    nt = nint( ns * dt/ds )

    ! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1,  2 /)
    s_edge(1:2, 2) = (/  2,  3 /)
    s_edge(1:2, 3) = (/  3,  4 /)
    s_edge(1:2, 4) = (/  4,  1 /)
    s_edge(1:2, 5) = (/  1,  5 /)
    s_edge(1:2, 6) = (/  2,  5 /)
    s_edge(1:2, 7) = (/  3,  5 /)
    s_edge(1:2, 8) = (/  4,  5 /)
    s_edge(1:2, 9) = (/  1,  6 /)
    s_edge(1:2,10) = (/  2,  6 /)
    s_edge(1:2,11) = (/  3,  6 /)
    s_edge(1:2,12) = (/  4,  6 /)
 
end if

end subroutine PoVRay_getpos_FZ23

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_getpos_FZ622
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 622
! 
!> @param dims dimensions for subsequent arrays
!> @param cpos vertex coordinates
!> @param s_edge first set of edge connectivities
!> @param t_edge second set of edge connectivities
!> @param ns, d, nt  auxiliary parameters
!> @param MFZ (optional) return coordinates for Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!> @date    02/05/17 MDG 1.1 correction of d parameter
!--------------------------------------------------------------------------
recursive subroutine PoVRay_getpos_FZ622(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_getpos_FZ622

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dims(3)
real(kind=dbl),INTENT(INOUT)          :: cpos(3,dims(1))
!f2py intent(in,out) ::  cpos
integer(kind=irg),INTENT(INOUT)       :: s_edge(2,dims(2))
!f2py intent(in,out) ::  s_edge
integer(kind=irg),INTENT(INOUT)       :: t_edge(2,dims(3))
!f2py intent(in,out) ::  t_edge
integer(kind=irg),INTENT(OUT)         :: ns
real(kind=dbl),INTENT(OUT)            :: d
integer(kind=irg),INTENT(OUT)         :: nt
logical,OPTIONAL,INTENT(IN)           :: MFZ

real(kind=dbl)          :: a = 1.0D0, b = 0.267949192431D0, c = 0.732050807569D0, &
                           dt = 0.5358983848622454D0, ds = 0.5358983848622454D0, di =1.069389330154823D0, dd, &
                           z = 0.D0, o = 0.86602540378443D0, p = 0.5D0


d = 1.0693893290743279D0
if (present(MFZ)) then
  if (MFZ) then 

! define the coordinates of the hexagonal Mackenzie FZ in Rodrigues Space
    cpos(1:3, 1) = (/  z,  z,  z /)
    cpos(1:3, 2) = (/  z,  z,  b /)
    cpos(1:3, 3) = (/  a,  z,  z /)
    cpos(1:3, 4) = (/  a,  z,  b /)
    cpos(1:3, 5) = (/  a,  b,  z /)
    cpos(1:3, 6) = (/  a,  b,  b /)
    cpos(1:3, 7) = (/  o,  p,  z /)
    cpos(1:3, 8) = (/  o,  p,  b /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1, 2 /)
    s_edge(1:2, 2) = (/  1, 3 /)
    s_edge(1:2, 3) = (/  1, 7 /)
    s_edge(1:2, 4) = (/  2, 4 /)
    s_edge(1:2, 5) = (/  2, 8 /)
    s_edge(1:2, 6) = (/  3, 4 /)
    s_edge(1:2, 7) = (/  3, 5 /)
    s_edge(1:2, 8) = (/  4, 6 /)
    s_edge(1:2, 9) = (/  5, 6 /)
    s_edge(1:2, 10) = (/  5, 7 /)
    s_edge(1:2, 11) = (/  6, 8 /)
    s_edge(1:2, 12) = (/  7, 8 /)
   end if 

 else


    ! define the coordinates of the hexagonal FZ in Rodrigues Space
    cpos(1:3, 1) = (/  a,  b,  b /)
    cpos(1:3, 2) = (/  c,  c,  b /)
    cpos(1:3, 3) = (/  b,  a,  b /)

    cpos(1:3, 4) = (/ -b,  a,  b /)
    cpos(1:3, 5) = (/ -c,  c,  b /)
    cpos(1:3, 6) = (/ -a,  b,  b /)

    cpos(1:3, 7) = (/ -a, -b,  b /)
    cpos(1:3, 8) = (/ -c, -c,  b /)
    cpos(1:3, 9) = (/ -b, -a,  b /)

    cpos(1:3,10) = (/  b, -a,  b /)
    cpos(1:3,11) = (/  c, -c,  b /)
    cpos(1:3,12) = (/  a, -b,  b /)

    cpos(1:3,13) = (/  a,  b, -b /)
    cpos(1:3,14) = (/  c,  c, -b /)
    cpos(1:3,15) = (/  b,  a, -b /)

    cpos(1:3,16) = (/ -b,  a, -b /)
    cpos(1:3,17) = (/ -c,  c, -b /)
    cpos(1:3,18) = (/ -a,  b, -b /)

    cpos(1:3,19) = (/ -a, -b, -b /)
    cpos(1:3,20) = (/ -c, -c, -b /)
    cpos(1:3,21) = (/ -b, -a, -b /)

    cpos(1:3,22) = (/  b, -a, -b /)
    cpos(1:3,23) = (/  c, -c, -b /)
    cpos(1:3,24) = (/  a, -b, -b /)

    cpos = cpos / d

    ns = 200
    nt = nint( ns * dt/ds )

    ! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1,  2 /)
    s_edge(1:2, 2) = (/  2,  3 /)
    s_edge(1:2, 3) = (/  3,  4 /)
    s_edge(1:2, 4) = (/  4,  5 /)
    s_edge(1:2, 5) = (/  5,  6 /)
    s_edge(1:2, 6) = (/  6,  7 /)
    s_edge(1:2, 7) = (/  7,  8 /)
    s_edge(1:2, 8) = (/  8,  9 /)
    s_edge(1:2, 9) = (/  9, 10 /)
    s_edge(1:2,10) = (/ 10, 11 /)
    s_edge(1:2,11) = (/ 11, 12 /)
    s_edge(1:2,12) = (/ 12,  1 /)
    s_edge(1:2,13) = (/ 13, 14 /)
    s_edge(1:2,14) = (/ 14, 15 /)
    s_edge(1:2,15) = (/ 15, 16 /)
    s_edge(1:2,16) = (/ 16, 17 /)
    s_edge(1:2,17) = (/ 17, 18 /)
    s_edge(1:2,18) = (/ 18, 19 /)
    s_edge(1:2,19) = (/ 19, 20 /)
    s_edge(1:2,20) = (/ 20, 21 /)
    s_edge(1:2,21) = (/ 21, 22 /)
    s_edge(1:2,22) = (/ 22, 23 /)
    s_edge(1:2,23) = (/ 23, 24 /)
    s_edge(1:2,24) = (/ 24, 13 /)

    t_edge(1:2, 1) = (/  1, 13 /)
    t_edge(1:2, 2) = (/  2, 14 /)
    t_edge(1:2, 3) = (/  3, 15 /)
    t_edge(1:2, 4) = (/  4, 16 /)
    t_edge(1:2, 5) = (/  5, 17 /)
    t_edge(1:2, 6) = (/  6, 18 /)
    t_edge(1:2, 7) = (/  7, 19 /)
    t_edge(1:2, 8) = (/  8, 20 /)
    t_edge(1:2, 9) = (/  9, 21 /)
    t_edge(1:2,10) = (/ 10, 22 /)
    t_edge(1:2,11) = (/ 11, 23 /)
    t_edge(1:2,12) = (/ 12, 24 /)


end if

end subroutine PoVRay_getpos_FZ622

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_getpos_FZ422
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 422
! 
!> @param dims dimensions for subsequent arrays
!> @param cpos vertex coordinates
!> @param s_edge first set of edge connectivities
!> @param t_edge second set of edge connectivities
!> @param ns, d, nt  auxiliary parameters
!> @param MFZ (optional) return coordinates for Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_getpos_FZ422(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_getpos_FZ422

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dims(3)
real(kind=dbl),INTENT(INOUT)          :: cpos(3,dims(1))
!f2py intent(in,out) ::  cpos
integer(kind=irg),INTENT(INOUT)       :: s_edge(2,dims(2))
!f2py intent(in,out) ::  s_edge
integer(kind=irg),INTENT(INOUT)       :: t_edge(2,dims(3))
!f2py intent(in,out) ::  t_edge
integer(kind=irg),INTENT(OUT)         :: ns
real(kind=dbl),INTENT(OUT)            :: d
integer(kind=irg),INTENT(OUT)         :: nt
logical,OPTIONAL,INTENT(IN)           :: MFZ

real(kind=dbl)          :: a = 1.0D0, b = 0.41421354D0, c = 0.41421354D0, dt = 0.8284270763397216D0, &
                           ds = 0.8284270763397216D0, dd, z = 0.D0, o = 0.70710678118654746D0

d = 1.158941651036677D0
if (present(MFZ)) then
  if (MFZ) then 

! define the coordinates of the tetragonal Mackenzie FZ in Rodrigues Space
    cpos(1:3, 1) = (/  z,  z,  z /)
    cpos(1:3, 2) = (/  z,  z,  c /)
    cpos(1:3, 3) = (/  a,  z,  z /)
    cpos(1:3, 4) = (/  a,  z,  c /)
    cpos(1:3, 5) = (/  a,  b,  z /)
    cpos(1:3, 6) = (/  a,  b,  c /)
    cpos(1:3, 7) = (/  o,  o,  z /)
    cpos(1:3, 8) = (/  o,  o,  c /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1, 2 /)
    s_edge(1:2, 2) = (/  1, 3 /)
    s_edge(1:2, 3) = (/  1, 7 /)
    s_edge(1:2, 4) = (/  2, 4 /)
    s_edge(1:2, 5) = (/  2, 8 /)
    s_edge(1:2, 6) = (/  3, 4 /)
    s_edge(1:2, 7) = (/  3, 5 /)
    s_edge(1:2, 8) = (/  4, 6 /)
    s_edge(1:2, 9) = (/  5, 6 /)
    s_edge(1:2, 10) = (/  5, 7 /)
    s_edge(1:2, 11) = (/  6, 8 /)
    s_edge(1:2, 12) = (/  7, 8 /)
   end if 
! define the coordinates of the cubic FZ in Rodrigues Space
 else
! define the coordinates of the tetragonal 422 FZ in Rodrigues Space
    cpos(1:3, 1) = (/  a,  b,  c /)
    cpos(1:3, 2) = (/  b,  a,  c /)
    cpos(1:3, 3) = (/ -b,  a,  c /)
    cpos(1:3, 4) = (/ -a,  b,  c /)
    cpos(1:3, 5) = (/ -a, -b,  c /)
    cpos(1:3, 6) = (/ -b, -a,  c /)
    cpos(1:3, 7) = (/  b, -a,  c /)
    cpos(1:3, 8) = (/  a, -b,  c /)
    cpos(1:3, 9) = (/  a,  b, -c /)
    cpos(1:3,10) = (/  b,  a, -c /)
    cpos(1:3,11) = (/ -b,  a, -c /)
    cpos(1:3,12) = (/ -a,  b, -c /)
    cpos(1:3,13) = (/ -a, -b, -c /)
    cpos(1:3,14) = (/ -b, -a, -c /)
    cpos(1:3,15) = (/  b, -a, -c /)
    cpos(1:3,16) = (/  a, -b, -c /)

    cpos = cpos / d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1,  2 /)
    s_edge(1:2, 2) = (/  2,  3 /)
    s_edge(1:2, 3) = (/  3,  4 /)
    s_edge(1:2, 4) = (/  4,  5 /)
    s_edge(1:2, 5) = (/  5,  6 /)
    s_edge(1:2, 6) = (/  6,  7 /)
    s_edge(1:2, 7) = (/  7,  8 /)
    s_edge(1:2, 8) = (/  8,  1 /)
    s_edge(1:2, 9) = (/  9, 10 /)
    s_edge(1:2,10) = (/ 10, 11 /)
    s_edge(1:2,11) = (/ 11, 12 /)
    s_edge(1:2,12) = (/ 12, 13 /)
    s_edge(1:2,13) = (/ 13, 14 /)
    s_edge(1:2,14) = (/ 14, 15 /)
    s_edge(1:2,15) = (/ 15, 16 /)
    s_edge(1:2,16) = (/ 16,  9 /)

    t_edge(1:2, 1) = (/  1,  9 /)
    t_edge(1:2, 2) = (/  2, 10 /)
    t_edge(1:2, 3) = (/  3, 11 /)
    t_edge(1:2, 4) = (/  4, 12 /)
    t_edge(1:2, 5) = (/  5, 13 /)
    t_edge(1:2, 6) = (/  6, 14 /)
    t_edge(1:2, 7) = (/  7, 15 /)
    t_edge(1:2, 8) = (/  8, 16 /)

end if

end subroutine PoVRay_getpos_FZ422

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_getpos_FZ32
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 32
! 
!> @param dims dimensions for subsequent arrays
!> @param cpos vertex coordinates
!> @param s_edge first set of edge connectivities
!> @param t_edge second set of edge connectivities
!> @param ns, d, nt  auxiliary parameters
!> @param MFZ (optional) return coordinates for Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_getpos_FZ32(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_getpos_FZ32

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dims(3)
real(kind=dbl),INTENT(INOUT)          :: cpos(3,dims(1))
!f2py intent(in,out) ::  cpos
integer(kind=irg),INTENT(INOUT)       :: s_edge(2,dims(2))
!f2py intent(in,out) ::  s_edge
integer(kind=irg),INTENT(INOUT)       :: t_edge(2,dims(3))
!f2py intent(in,out) ::  t_edge
integer(kind=irg),INTENT(OUT)         :: ns
real(kind=dbl),INTENT(OUT)            :: d
integer(kind=irg),INTENT(OUT)         :: nt
logical,OPTIONAL,INTENT(IN)           :: MFZ


real(kind=dbl)          :: a = 0.8660254038D0, b = 0.5D0, c = 0.5773502692D0, dt = 0.34314575050D0, &
                           ds = 0.6340506711D0, dd, z = 0.D0, oo = 1.D0, o = 0.86602540378443D0, p = 0.5D0

d = 1.1547005384D0
if (present(MFZ)) then
  if (MFZ) then 

! define the coordinates of the tetragonal Mackenzie FZ in Rodrigues Space
    cpos(1:3, 1) = (/  z,  z,  z /)
    cpos(1:3, 2) = (/  z,  z,  c /)
    cpos(1:3, 3) = (/  a, -p,  z /)
    cpos(1:3, 4) = (/  a, -p,  c /)
    cpos(1:3, 5) = (/  a,  p,  z /)
    cpos(1:3, 6) = (/  a,  p,  c /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1, 2 /)
    s_edge(1:2, 2) = (/  1, 3 /)
    s_edge(1:2, 3) = (/  1, 5 /)
    s_edge(1:2, 4) = (/  2, 4 /)
    s_edge(1:2, 5) = (/  2, 6 /)
    s_edge(1:2, 6) = (/  3, 4 /)
    s_edge(1:2, 7) = (/  3, 5 /)
    s_edge(1:2, 8) = (/  4, 6 /)
    s_edge(1:2, 9) = (/  5, 6 /)
   end if 
 else
! define the coordinates of the cubic FZ in Rodrigues Space
    cpos(1:3, 1) = (/  a,  b,  c /)
    cpos(1:3, 2) = (/  z, oo,  c /)
    cpos(1:3, 3) = (/ -a,  b,  c /)

    cpos(1:3, 4) = (/ -a, -b,  c /)
    cpos(1:3, 5) = (/  z,-oo,  c /)
    cpos(1:3, 6) = (/  a, -b,  c /)

    cpos(1:3, 7) = (/  a,  b, -c /)
    cpos(1:3, 8) = (/  z, oo, -c /)
    cpos(1:3, 9) = (/ -a,  b, -c /)

    cpos(1:3,10) = (/ -a, -b, -c /)
    cpos(1:3,11) = (/  z,-oo, -c /)
    cpos(1:3,12) = (/  a, -b, -c /)

    cpos = cpos / d

    ns = 200
    nt = nint( ns * dt/ds )

    ! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1,  2 /)
    s_edge(1:2, 2) = (/  2,  3 /)
    s_edge(1:2, 3) = (/  3,  4 /)
    s_edge(1:2, 4) = (/  4,  5 /)
    s_edge(1:2, 5) = (/  5,  6 /)
    s_edge(1:2, 6) = (/  6,  1 /)
    s_edge(1:2, 7) = (/  7,  8 /)
    s_edge(1:2, 8) = (/  8,  9 /)
    s_edge(1:2, 9) = (/  9, 10 /)
    s_edge(1:2,10) = (/ 10, 11 /)
    s_edge(1:2,11) = (/ 11, 12 /)
    s_edge(1:2,12) = (/ 12,  7 /)

    t_edge(1:2, 1) = (/  1,  7 /)
    t_edge(1:2, 2) = (/  2,  8 /)
    t_edge(1:2, 3) = (/  3,  9 /)
    t_edge(1:2, 4) = (/  4, 10 /)
    t_edge(1:2, 5) = (/  5, 11 /)
    t_edge(1:2, 6) = (/  6, 12 /)

end if

end subroutine PoVRay_getpos_FZ32

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_getpos_FZ222
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 222
! 
!> @param dims dimensions for subsequent arrays
!> @param cpos vertex coordinates
!> @param s_edge first set of edge connectivities
!> @param t_edge second set of edge connectivities
!> @param ns, d, nt  auxiliary parameters
!> @param MFZ (optional) return coordinates for Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_getpos_FZ222(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_getpos_FZ222

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dims(3)
real(kind=dbl),INTENT(INOUT)          :: cpos(3,dims(1))
!f2py intent(in,out) ::  cpos
integer(kind=irg),INTENT(INOUT)       :: s_edge(2,dims(2))
!f2py intent(in,out) ::  s_edge
integer(kind=irg),INTENT(INOUT)       :: t_edge(2,dims(3))
!f2py intent(in,out) ::  t_edge
integer(kind=irg),INTENT(OUT)         :: ns
real(kind=dbl),INTENT(OUT)            :: d
integer(kind=irg),INTENT(OUT)         :: nt
logical,OPTIONAL,INTENT(IN)           :: MFZ

real(kind=dbl)          :: a = 1.0D0, b = 1.0D0, c = 1D0, dt = 2.0D0, &
                           ds = 2.0D0, dd, z = 0.D0, oo = 1.D0

d = 1.7320508075688772D0
if (present(MFZ)) then
  if (MFZ) then 

! define the coordinates of the tetragonal Mackenzie FZ in Rodrigues Space
    cpos(1:3, 1) = (/  z, -a,  z /)
    cpos(1:3, 2) = (/  z, -a,  c /)
    cpos(1:3, 3) = (/  a, -a,  z /)
    cpos(1:3, 4) = (/  a, -a,  c /)
    cpos(1:3, 5) = (/  a,  a,  z /)
    cpos(1:3, 6) = (/  a,  a,  c /)
    cpos(1:3, 7) = (/  z,  a,  z /)
    cpos(1:3, 8) = (/  z,  a,  c /)

    cpos = cpos/d

    ns = 200
    nt = nint( ns * dt/ds )

! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1, 2 /)
    s_edge(1:2, 2) = (/  1, 3 /)
    s_edge(1:2, 3) = (/  1, 7 /)
    s_edge(1:2, 4) = (/  2, 4 /)
    s_edge(1:2, 5) = (/  2, 8 /)
    s_edge(1:2, 6) = (/  3, 4 /)
    s_edge(1:2, 7) = (/  3, 5 /)
    s_edge(1:2, 8) = (/  4, 6 /)
    s_edge(1:2, 9) = (/  5, 6 /)
    s_edge(1:2, 10) = (/  5, 7 /)
    s_edge(1:2, 11) = (/  6, 8 /)
    s_edge(1:2, 12) = (/  7, 8 /)
   end if 
! define the coordinates of the cubic FZ in Rodrigues Space
 else

    ! define the coordinates of the FZ in Rodrigues Space
    cpos(1:3, 1) = (/  a,  b,  c /)
    cpos(1:3, 2) = (/ -a,  b,  c /)
    cpos(1:3, 3) = (/ -a, -b,  c /)
    cpos(1:3, 4) = (/  a, -b,  c /)
    cpos(1:3, 5) = (/  a,  b, -c /)
    cpos(1:3, 6) = (/ -a,  b, -c /)
    cpos(1:3, 7) = (/ -a, -b, -c /)
    cpos(1:3, 8) = (/  a, -b, -c /)

    cpos = cpos / d

    ns = 200
    nt = nint( ns * dt/ds )

    ! define the connectivity of all the edges
    s_edge(1:2, 1) = (/  1,  2 /)
    s_edge(1:2, 2) = (/  2,  3 /)
    s_edge(1:2, 3) = (/  3,  4 /)
    s_edge(1:2, 4) = (/  4,  1 /)
    s_edge(1:2, 5) = (/  5,  6 /)
    s_edge(1:2, 6) = (/  6,  7 /)
    s_edge(1:2, 7) = (/  7,  8 /)
    s_edge(1:2, 8) = (/  8,  5 /)

    t_edge(1:2, 1) = (/  1,  5 /)
    t_edge(1:2, 2) = (/  2,  6 /)
    t_edge(1:2, 3) = (/  3,  7 /)
    t_edge(1:2, 4) = (/  4,  8 /)
end if

end subroutine PoVRay_getpos_FZ222

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_drawFZ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for any rotation group
! 
!> @details This routine draws the outline of either the Rodrigues Fundamental
!> zone, or the Mackenzie Fundamental Zone (if MFZ is set and true).
!
!> @param dunit output unit number
!> @param rmode  1(cubochoric)|2(homochoric)|3(stereographic)|4(Rodrigues)|5(Euler)
!> @param cylr cylinder radius
!> @param FZtype  Fundamental zone type 
!> @param FZorder order of the FZ point group
!> @param MFZ (optional) Mackenzie FZ instead of regular FZ
!
!> @date    09/10/16 MDG 1.0 original
!> @date    11/23/16 MDG 1.1 added Euler representation
!--------------------------------------------------------------------------
recursive subroutine PoVRay_drawFZ(dunit, rmode, cylr, FZtype, FZorder, MFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_drawFZ

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
integer(kind=irg),INTENT(IN)          :: rmode
real(kind=dbl),INTENT(IN)             :: cylr
integer(kind=irg),INTENT(IN)          :: FZtype
integer(kind=irg),INTENT(IN)          :: FZorder
logical,OPTIONAL,INTENT(IN)           :: MFZ

real(kind=dbl)          :: eul(3), axang(4), rv(3), qu(4), h(3), rmax, dx, r, xmax, x, y, z, ho(3), cu(3), zsmall, &
                           eyepos(3), ac, sh(3), ron(4)
real(kind=dbl)          :: euld(3), om(3,3), ro1(4), ro2(4), culast(3), holast(3), ro(4), rolast(4), splast(3), sp(3), &
                           eu(3), eulast(3), xx 
type(orientationtype)   :: ot
type(orientationtyped)  :: otd

integer(kind=irg),allocatable :: s_edge(:,:), t_edge(:,:)
real(kind=dbl),allocatable    :: cpos(:,:)

logical                 :: doMFZ, twostep
integer(kind=irg)       :: i,j,k, icnt, imax, nt, ns, dims(3) 
real(kind=dbl)          :: d, dd, tpi, hpi

tpi = 2.D0 * cPi
hpi = 0.5D0 * cPi
doMFZ = .FALSE.
if(present(MFZ)) doMFZ = .TRUE.

write (*,*) 'inside PoVRay_drawFZ; MFZ = ', doMFZ, dunit, rmode, cylr, FZtype, FZorder

if (FZtype.eq.2) then
    if (FZorder.eq.6) then
    if (doMFZ) then
      twostep = .FALSE.
      dims = (/ 8, 12, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ622(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
    else
      twostep = .TRUE.
      dims = (/ 24, 24, 12 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ622(dims, cpos, s_edge, t_edge, ns, d, nt)
    end if
  end if
  if (FZorder.eq.4) then
    if (doMFZ) then
      twostep = .FALSE.
      dims = (/ 8, 12, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ422(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
    else
      twostep = .TRUE.
      dims = (/ 16, 16, 8 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ422(dims, cpos, s_edge, t_edge, ns, d, nt)
    end if
  end if
  if (FZorder.eq.3) then
    if (doMFZ) then
      twostep = .FALSE.
      dims = (/ 6, 9, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ32(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
    else
      twostep = .TRUE.
      dims = (/ 12, 12, 6 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ32(dims, cpos, s_edge, t_edge, ns, d, nt)
    end if
  end if
  if (FZorder.eq.2) then
    if (doMFZ) then
      twostep = .FALSE.
      dims = (/ 8, 12, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ222(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
    else
      twostep = .TRUE.
      dims = (/ 8, 8, 4 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ222(dims, cpos, s_edge, t_edge, ns, d, nt)
    end if
  end if
end if

if (FZtype.eq.3) then
! rotational group 23
    if (doMFZ) then
      twostep = .FALSE.
      dims = (/ 4, 6, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ23(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
    else
      twostep = .FALSE.
      dims = (/ 6, 12, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ23(dims, cpos, s_edge, t_edge, ns, d, nt)
    end if
end if

if (FZtype.eq.4) then
! rotational group 432
    if (doMFZ) then
      twostep = .FALSE.
      dims = (/ 6, 9, 1 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ432(dims, cpos, s_edge, t_edge, ns, d, nt, MFZ)
    else
      twostep = .TRUE.
      dims = (/ 24, 12, 24 /)
      allocate(cpos(3,dims(1)), s_edge(2,dims(2)), t_edge(2,dims(3)))
      call PoVRay_getpos_FZ432(dims, cpos, s_edge, t_edge, ns, d, nt)
    end if
end if

! add the reference frame and any necessary wireframes 
if (rmode.eq.1) then
  ac = 0.5D0 * LPs%ap
  call PoVRay_addReferenceFrame(dunit, ac, cylr)
  call PoVRay_addCubochoricCube(dunit)
end if
if (rmode.eq.2) then
  ac = 1.33067D0
  call PoVRay_addReferenceFrame(dunit, ac, cylr)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.3) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac, cylr)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.4) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac, cylr)
end if
if (rmode.eq.5) then 
  call PoVRay_addEulerBox(dunit)
end if

! and next, draw the outline of the FZ or MFZ
if ((rmode.eq.1).or.(rmode.eq.2)) then
! create the square edges first
 dx = 1.D0/dble(ns)
 do i=1,dims(2)
  ro1(1:3) = cpos(1:3,s_edge(1,i))
  ro1(4) = d
  ro2(1:3) = cpos(1:3,s_edge(2,i))
  ro2(4) = d
  culast = ro2cu(ro1)
  holast = ro2ho(ro1)
  do j=1,ns+1
    ro(1:3) = d*ro1(1:3) + d*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    cu = ro2cu(ro)
    ho = ro2ho(ro)
! and create a cylinder with these points
    if (rmode.eq.1) then
      call PoVRay_addCylinder(dunit, culast(1:3),cu(1:3),cylr,(/ 0.0, 0.0, 1.0 /))
    else
      call PoVRay_addCylinder(dunit, holast(1:3),ho(1:3),cylr,(/ 0.0, 0.0, 1.0 /))
    end if
    culast = cu
    holast = ho
  end do 
 end do

 if (twostep) then
   dx = 1.D0/dble(nt)
   do i=1,dims(3)
    ro1(1:3) = cpos(1:3,t_edge(1,i))
    ro1(4) = d
    ro2(1:3) = cpos(1:3,t_edge(2,i))
    ro2(4) = d
    culast = ro2cu(ro1)
    holast = ro2ho(ro1)
    do j=1,nt+1
      ro(1:3) = d*ro1(1:3) + d*(ro2(1:3) - ro1(1:3)) * j * dx
      xx = dsqrt( sum (ro(1:3)**2) )
      ro(1:3) = ro(1:3)/xx
      ro(4) = xx
      cu = ro2cu(ro)
      ho = ro2ho(ro)
  ! and create a cylinder with these points
      if (rmode.eq.1) then
        call PoVRay_addCylinder(dunit, culast(1:3),cu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      else
        call PoVRay_addCylinder(dunit, holast(1:3),ho(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      end if
      culast = cu
      holast = ho
    end do 
   end do
  end if 
end if

if ((rmode.eq.3).or.(rmode.eq.4)) then
 dx = 1.D0/dble(ns)
 do i=1,dims(2)
  ro1(1:3) = cpos(1:3,s_edge(1,i))
  ro1(4) = d
  ro2(1:3) = cpos(1:3,s_edge(2,i))
  ro2(4) = d
  rolast = ro1
  qu = ro2qu(ro1)
  splast(1:3) = qu(2:4)/ (1.D0 + qu(1))
  do j=1,ns+1
    ro(1:3) = d*ro1(1:3) + d*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    qu = ro2qu(ro)
    sp(1:3) = qu(2:4)/ (1.D0 + qu(1))
! and create a cylinder with these points
    if (rmode.eq.3) then
       call PoVRay_addCylinder(dunit, splast(1:3),sp(1:3),cylr,(/ 0.0, 0.0, 1.0 /))
    else
       call PoVRay_addCylinder(dunit, rolast(1:3)*rolast(4),ro(1:3)*ro(4),cylr,(/ 0.0, 0.0, 1.0 /))
    end if
    rolast = ro
    splast = sp
  end do 
 end do

 if (twostep) then
   dx = 1.D0/dble(nt)
   do i=1,dims(3)
    ro1(1:3) = cpos(1:3,t_edge(1,i))
    ro1(4) = d
    ro2(1:3) = cpos(1:3,t_edge(2,i))
    ro2(4) = d
    rolast = ro1
    qu = ro2qu(ro1)
    splast(1:3) = qu(2:4)/ (1.D0 + qu(1))
    do j=1,nt+1
      ro(1:3) = d*ro1(1:3) + d*(ro2(1:3) - ro1(1:3)) * j * dx
      xx = dsqrt( sum (ro(1:3)**2) )
      ro(1:3) = ro(1:3)/xx
      ro(4) = xx
      qu = ro2qu(ro)
      sp(1:3) = qu(2:4)/ (1.D0 + qu(1))
  ! and create a cylinder with these points
      if (rmode.eq.3) then
         call PoVRay_addCylinder(dunit, splast(1:3),sp(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      else
         call PoVRay_addCylinder(dunit, rolast(1:3)*rolast(4),ro(1:3)*ro(4),cylr,(/ 1.0, 0.0, 0.0 /))
      end if
      rolast = ro
      splast = sp
    end do 
   end do
 end if
end if
  
! and next, draw the outline of the FZ or MFZ
if (rmode.eq.5) then
  sh = (/ cPi, cPi/2.D0, cPi /)
! create the square edges first
 dx = 1.D0/dble(ns)
 do i=1,dims(2)
  ro1(1:3) = cpos(1:3,s_edge(1,i))
  ro1(4) = d
  ro2(1:3) = cpos(1:3,s_edge(2,i))
  ro2(4) = d
  eulast = ro2eu(ro1)
  eulast(1) = mod(eulast(1)+10.D0*cPi,2.D0*cPi)
  eulast(2) = mod(eulast(2)+10.D0*cPi,cPi)
  eulast(3) = mod(eulast(3)+10.D0*cPi,2.D0*cPi)
  eulast = eulast - sh
  do j=1,ns+1
    ro(1:3) = d*ro1(1:3) + d*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    eu = ro2eu(ro)
    eu(1) = mod(eu(1)+10.D0*cPi,2.D0*cPi)
    eu(2) = mod(eu(2)+10.D0*cPi,cPi)
    eu(3) = mod(eu(3)+10.D0*cPi,2.D0*cPi)
    eu = eu - sh
! and create a cylinder with these points
    if (maxval(abs(eulast-eu)).lt.cPi) then 
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 0.0, 0.0, 1.0 /))
    end if
    eulast = eu
  end do 
 end do

 if (twostep) then
   dx = 1.D0/dble(nt)
   do i=1,dims(3)
    ro1(1:3) = cpos(1:3,t_edge(1,i))
    ro1(4) = d
    ro2(1:3) = cpos(1:3,t_edge(2,i))
    ro2(4) = d
    eulast = ro2eu(ro1)
    eulast(1) = mod(eulast(1)+10.D0*cPi,2.D0*cPi)
    eulast(2) = mod(eulast(2)+10.D0*cPi,cPi)
    eulast(3) = mod(eulast(3)+10.D0*cPi,2.D0*cPi)
    eulast = eulast - sh
    do j=1,nt+1
      ro(1:3) = d*ro1(1:3) + d*(ro2(1:3) - ro1(1:3)) * j * dx
      xx = dsqrt( sum (ro(1:3)**2) )
      ro(1:3) = ro(1:3)/xx
      ro(4) = xx
      eu = ro2eu(ro)
      eu(1) = mod(eu(1)+10.D0*cPi,2.D0*cPi)
      eu(2) = mod(eu(2)+10.D0*cPi,cPi)
      eu(3) = mod(eu(3)+10.D0*cPi,2.D0*cPi)
      eu = eu - sh
  ! and create a cylinder with these points
      if (maxval(abs(eulast-eu)).lt.cPi) then 
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      end if
      eulast = eu
    end do 
   end do
  end if 
! for the Euler representation of the Rodrigues fundamental zones we also need to draw
! a few additional lines to complete the volume appearance of the FZ; this depends on
! the order of the FZ, naturally, so we have a couple of possible cases...

! the diagonal lines in the Phi=0 plane are the most important lines; they should be drawn
! for all the rotation groups except for the identity
  if ((FZtype.ge.1).and.(FZtype.le.2)) then
    if (FZorder.ne.0) then  ! we have cyclic or dihedral
      xx = cPi/dble(FZorder)
! draw four diagonal lines
      eu(1:3) = (/ xx, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, 0.D0, xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi-xx, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, 0.D0, tpi-xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, 0.D0, xx /) - sh
      eulast(1:3) = (/ xx, 0.D0, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, 0.D0, tpi-xx /) - sh
      eulast(1:3) = (/ tpi-xx, 0.D0, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
! for cyclic groups we also need to draw the diagonals in the top surface
! and the vertical lines connecting bottom and top planes
      if (FZtype.eq.1) then
! top plane
        eu(1:3) = (/ xx, cPi, 0.D0 /) - sh
        eulast(1:3) = (/ 0.D0, cPi, xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi-xx, cPi, 0.D0 /) - sh
        eulast(1:3) = (/ 0.D0, cPi, tpi-xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi, cPi, xx /) - sh
        eulast(1:3) = (/ xx, cPi, tpi /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi, cPi, tpi-xx /) - sh
        eulast(1:3) = (/ tpi-xx, cPi, tpi /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
! verticals
        eu(1:3) = (/ xx, 0.D0, 0.D0 /) - sh
        eulast(1:3) = (/ xx, cPi, 0.D0 /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ 0.D0, 0.D0, xx /) - sh
        eulast(1:3) = (/ 0.D0, cPi, xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi-xx, 0.D0, 0.D0 /) - sh
        eulast(1:3) = (/ tpi-xx, cPi, 0.D0 /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ 0.D0, 0.D0, tpi-xx /) - sh
        eulast(1:3) = (/ 0.D0, cPi, tpi-xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi, 0.D0, xx /) - sh
        eulast(1:3) = (/ tpi, cPi, xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ xx, 0.D0, tpi /) - sh
        eulast(1:3) = (/ xx, cPi, tpi /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi, 0.D0, tpi-xx /) - sh
        eulast(1:3) = (/ tpi, cPi, tpi-xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi-xx, 0.D0, tpi /) - sh
        eulast(1:3) = (/ tpi-xx, cPi, tpi /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      end if
      if (FZtype.eq.2) then
! in this case, the verticals need to be drawn but only up to the level of the FZ surface
        eu(1:3) = (/ xx, 0.D0, 0.D0 /) - sh
        eulast(1:3) = (/ xx, hPi, 0.D0 /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ 0.D0, 0.D0, xx /) - sh
        eulast(1:3) = (/ 0.D0, hPi, xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi-xx, 0.D0, 0.D0 /) - sh
        eulast(1:3) = (/ tpi-xx, hPi, 0.D0 /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ 0.D0, 0.D0, tpi-xx /) - sh
        eulast(1:3) = (/ 0.D0, hPi, tpi-xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi, 0.D0, xx /) - sh
        eulast(1:3) = (/ tpi, hPi, xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ xx, 0.D0, tpi /) - sh
        eulast(1:3) = (/ xx, hPi, tpi /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi, 0.D0, tpi-xx /) - sh
        eulast(1:3) = (/ tpi, hPi, tpi-xx /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
        eu(1:3) = (/ tpi-xx, 0.D0, tpi /) - sh
        eulast(1:3) = (/ tpi-xx, hPi, tpi /) - sh
        call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      end if
    end if
  end if

  if (FZtype.eq.4) then
      xx = cPi/dble(4)
! draw four diagonal lines
      eu(1:3) = (/ xx, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, 0.D0, xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi-xx, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, 0.D0, tpi-xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, 0.D0, xx /) - sh
      eulast(1:3) = (/ xx, 0.D0, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, 0.D0, tpi-xx /) - sh
      eulast(1:3) = (/ tpi-xx, 0.D0, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
! and verticals
      hPi = hPi * 0.5D0
      eu(1:3) = (/ xx, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ xx, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ 0.D0, 0.D0, xx /) - sh
      eulast(1:3) = (/ 0.D0, hPi, xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi-xx, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ tpi-xx, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ 0.D0, 0.D0, tpi-xx /) - sh
      eulast(1:3) = (/ 0.D0, hPi, tpi-xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, 0.D0, xx /) - sh
      eulast(1:3) = (/ tpi, hPi, xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ xx, 0.D0, tpi /) - sh
      eulast(1:3) = (/ xx, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, 0.D0, tpi-xx /) - sh
      eulast(1:3) = (/ tpi, hPi, tpi-xx /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi-xx, 0.D0, tpi /) - sh
      eulast(1:3) = (/ tpi-xx, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
 ! and the closing segments
      eu(1:3) = (/ xx, hPi, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ 0.D0, hPi, xx /) - sh
      eulast(1:3) = (/ 0.D0, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi-xx, hPi, 0.D0 /) - sh
      eulast(1:3) = (/ tpi, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ 0.D0, hPi, tpi-xx /) - sh
      eulast(1:3) = (/ 0.D0, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, hPi, xx /) - sh
      eulast(1:3) = (/ tpi, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ xx, hPi, tpi /) - sh
      eulast(1:3) = (/ 0.D0, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi, hPi, tpi-xx /) - sh
      eulast(1:3) = (/ tpi, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
      eu(1:3) = (/ tpi-xx, hPi, tpi /) - sh
      eulast(1:3) = (/ tpi, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /))
! and finally the corner posts
      eu(1:3) = (/ 0.D0, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ hPi, 0.D0, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ 0.D0, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ 0.D0, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ 0.D0, 0.D0, hPi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 

      eu(1:3) = (/ tpi, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ tpi - hPi, 0.D0, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ tpi, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ tpi, hPi, 0.D0 /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ tpi, 0.D0, 0.D0 /) - sh
      eulast(1:3) = (/ tpi, 0.D0, hPi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 

      eu(1:3) = (/ tpi, 0.D0, tpi /) - sh
      eulast(1:3) = (/ tpi - hPi, 0.D0, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ tpi, 0.D0, tpi /) - sh
      eulast(1:3) = (/ tpi, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ tpi, 0.D0, tpi /) - sh
      eulast(1:3) = (/ tpi, 0.D0, tpi-hPi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 

      eu(1:3) = (/ 0.D0, 0.D0, tpi /) - sh
      eulast(1:3) = (/ 0.D0 + hPi, 0.D0, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ 0.D0, 0.D0, tpi /) - sh
      eulast(1:3) = (/ 0.D0, hPi, tpi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
      eu(1:3) = (/ 0.D0, 0.D0, tpi /) - sh
      eulast(1:3) = (/ 0.D0, 0.D0, tpi-hPi /) - sh
      call PoVRay_addCylinder(dunit,eulast(1:3),eu(1:3),cylr,(/ 1.0, 0.0, 0.0 /)) 
  end if
end if


end subroutine PoVRay_drawFZ

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_initFZ2
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 2
! 
!> @param dunit output unit number
!> @param rmode  1(cubochoric)|2(homochoric)|3(stereographic)|4(Rodrigues)
!
!> @date    09/09/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_initFZ2(dunit, rmode)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_initFZ2

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
integer(kind=irg),INTENT(IN)          :: rmode

real(kind=dbl)          :: eul(3), axang(4), rv(3), qu(4), h(3), rmax, dx, r, xmax, x, y, z, ho(3), cu(3), zsmall, &
                           eyepos(3), ac, dpos(200), tmp
real(kind=dbl)          :: euld(3), om(3,3), ro1(4), ro2(4), culast(3), holast(3), ro(4), rolast(4), splast(3), sp(3), xx 
type(orientationtype)   :: ot
type(orientationtyped)  :: otd
integer(kind=irg)       :: i,j,k, icnt, imax, nt, ns, h_edge(2,100)
! the parameter a represents infinity as tan(178°/2)
real(kind=dbl)          :: a = 570.289922125538D0, b = 1.0D0, c = 1.0D0, cpos(3,200), dt = 114.57984425107713D0, &
                           ds = 2.0D0, d = 1.7320508075688772D0, dd, zz = 0.D0, oo = 1.D0, dtor =  0.017453292D0


! define the coordinates of the monoclinic C2 (2) FZ in Rodrigues Space
do i=-12,12
  if (abs(i).ne.12) then 
    cpos(1:3, 13+i) = (/ -a, dtan(dble(i)*15.D0*dtor*0.5D0) ,  c /)
  else
    cpos(1:3, 13+i) = (/ -a, a ,  c /)
    if (i.lt.0) cpos(2,13+i) = -cpos(2,13+i)
  end if
end do

do i=1,25
  cpos(1:3,25+i) = cpos(1:3,i)
  cpos(1,25+i) = -cpos(1,25+i)
end do

do i=1,50
  cpos(1:3,50+i) = cpos(1:3,i)
  tmp = cpos(1,50+i)
  cpos(1,50+i) = cpos(2,50+i)
  cpos(2,50+i) = tmp
end do

do i=1,100
  cpos(1:3,100+i) = cpos(1:3,i)
  cpos(3,100+i) = -cpos(3,100+i)
end do

! and normalize
do i=1,200
  dpos(i) = dsqrt(sum(cpos(1:3,i)*cpos(1:3,i)))
end do

ns = 2000
dx = dt/float(ns-1)

! define the connectivity of all the edges
do i=1,25
  h_edge(1:2,i)    = (/    i, 25+i /)
  h_edge(1:2,25+i) = (/ 50+i, 75+i /)
  h_edge(1:2,50+i) = (/100+i,125+i /)
  h_edge(1:2,75+i) = (/150+i,175+i /)
end do

! first add the reference frame and any necessary wireframe diagrams
if (rmode.eq.1) then
  ac = 0.5D0 * LPs%ap
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addCubochoricCube(dunit)
end if
if (rmode.eq.2) then
  ac = 1.33067D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.3) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.4) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
end if

if ((rmode.eq.1).or.(rmode.eq.2)) then
! create the top and bottom planes first
 dx = 1.D0/dble(ns)
 do i=1,100
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  culast = ro2cu(ro1)
  holast = ro2ho(ro1)
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    cu = ro2cu(ro)
    ho = ro2ho(ro)
! and create a cylinder with these points
    if (rmode.eq.1) then
      call PoVRay_addCylinder(dunit, culast(1:3),cu(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
      call PoVRay_addCylinder(dunit, holast(1:3),ho(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    culast = cu
    holast = ho
  end do 
 end do
end if
  
if ((rmode.eq.3).or.(rmode.eq.4)) then
! create the square edges first
 dx = 1.D0/dble(ns)
 do i=1,100
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  rolast = ro1
  qu = ro2qu(ro1)
  splast(1:3) = qu(2:4)/ (1.D0 + qu(1))
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    qu = ro2qu(ro)
    sp(1:3) = qu(2:4)/ (1.D0 + qu(1))
! and create a cylinder with these points
    if (rmode.eq.3) then
       call PoVRay_addCylinder(dunit, splast(1:3),sp(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
       call PoVRay_addCylinder(dunit, rolast(1:3)*rolast(4),ro(1:3)*ro(4),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    rolast = ro
    splast = sp
  end do 
 end do
end if
  
end subroutine PoVRay_initFZ2

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_initFZ3
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 3
! 
!> @param dunit output unit number
!> @param rmode  1(cubochoric)|2(homochoric)|3(stereographic)|4(Rodrigues)
!
!> @date    09/09/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_initFZ3(dunit, rmode)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_initFZ3

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
integer(kind=irg),INTENT(IN)          :: rmode

real(kind=dbl)          :: eul(3), axang(4), rv(3), qu(4), h(3), rmax, dx, r, xmax, x, y, z, ho(3), cu(3), zsmall, &
                           eyepos(3), ac, dpos(208), tmp
real(kind=dbl)          :: euld(3), om(3,3), ro1(4), ro2(4), culast(3), holast(3), ro(4), rolast(4), splast(3), sp(3), xx 
type(orientationtype)   :: ot
type(orientationtyped)  :: otd
integer(kind=irg)       :: i,j,k, icnt, imax, nt, ns, h_edge(2,104)
! the parameter a represents infinity as tan(178°/2)
real(kind=dbl)          :: a = 57.289922125538D0, b = 1.0D0, c = 0.577350269120D0, cpos(3,208), dt = 114.57984425107713D0, &
                           ds = 2.0D0, d = 1.7320508075688772D0, dd, zz = 0.D0, oo = 1.D0, dtor =  0.017453292D0, &
                           c2 = 1.7320508075688767D0


! define the coordinates of the trigonal C3 (3) FZ in Rodrigues Space
do i=-6,6 
  if (abs(i).ne.6) then 
    cpos(1:3, 7+i) = (/ -a, dtan(dble(i)*30.D0*dtor*0.5D0) ,  c /)
  else
    cpos(1:3, 7+i) = (/ -a, a ,  c /)
    if (i.lt.0) cpos(2,7+i) = -cpos(2,7+i)
  end if
end do
do i=1,13
  cpos(1:3,13+i) = cpos(1:3,i)
  cpos(1,13+i) = -cpos(1,13+i)
end do

do i=1,26
  cpos(1:3,26+i) = cpos(1:3,i)
  tmp = cpos(1,26+i)
  cpos(1,26+i) = cpos(2,26+i)
  cpos(2,26+i) = tmp
end do

do i=1,52
  cpos(1:3,52+i) = cpos(1:3,i)
  cpos(3,52+i) = -cpos(3,52+i)
end do

do i=1,104
  cpos(1:3,104+i) = cpos(1:3,i)
  if (cpos(3,104+i).lt.0.D0) then
    cpos(3,104+i) = -c2
  else 
    cpos(3,104+i) = c2
  end if
end do

! and normalize
do i=1,208
  dpos(i) = dsqrt(sum(cpos(1:3,i)*cpos(1:3,i)))
end do

ns = 200
dx = dt/float(ns-1)

! define the connectivity of all the edges
do i=1,13
  h_edge(1:2,i)    = (/    i, 13+i /)
  h_edge(1:2,13+i) = (/ 26+i, 39+i /)
  h_edge(1:2,26+i) = (/ 52+i, 65+i /)
  h_edge(1:2,39+i) = (/ 78+i, 91+i /)

  h_edge(1:2,52+i) = (/104+i,117+i /)
  h_edge(1:2,65+i) = (/130+i,143+i /)
  h_edge(1:2,78+i) = (/156+i,169+i /)
  h_edge(1:2,91+i) = (/182+i,195+i /)
end do

! first add the reference frame and any necessary wireframe diagrams
if (rmode.eq.1) then
  ac = 0.5D0 * LPs%ap
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addCubochoricCube(dunit)
end if
if (rmode.eq.2) then
  ac = 1.33067D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.3) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.4) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
end if

if ((rmode.eq.1).or.(rmode.eq.2)) then
! create the top and bottom planes first
 dx = 1.D0/dble(ns)
 do i=1,104
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  culast = ro2cu(ro1)
  holast = ro2ho(ro1)
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    cu = ro2cu(ro)
    ho = ro2ho(ro)
! and create a cylinder with these points
    if (rmode.eq.1) then
      call PoVRay_addCylinder(dunit, culast(1:3),cu(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
      call PoVRay_addCylinder(dunit, holast(1:3),ho(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    culast = cu
    holast = ho
  end do 
 end do
end if
  
if ((rmode.eq.3).or.(rmode.eq.4)) then
! create the square edges first
 dx = 1.D0/dble(ns)
 do i=1,104
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  rolast = ro1
  qu = ro2qu(ro1)
  splast(1:3) = qu(2:4)/ (1.D0 + qu(1))
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    qu = ro2qu(ro)
    sp(1:3) = qu(2:4)/ (1.D0 + qu(1))
! and create a cylinder with these points
    if (rmode.eq.3) then
       call PoVRay_addCylinder(dunit, splast(1:3),sp(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
       call PoVRay_addCylinder(dunit, rolast(1:3)*rolast(4),ro(1:3)*ro(4),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    rolast = ro
    splast = sp
  end do 
 end do
end if
  
end subroutine PoVRay_initFZ3

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_initFZ4
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 4
! 
!> @param dunit output unit number
!> @param rmode  1(cubochoric)|2(homochoric)|3(stereographic)|4(Rodrigues)
!
!> @date    09/09/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_initFZ4(dunit, rmode)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_initFZ4

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
integer(kind=irg),INTENT(IN)          :: rmode

real(kind=dbl)          :: eul(3), axang(4), rv(3), qu(4), h(3), rmax, dx, r, xmax, x, y, z, ho(3), cu(3), zsmall, &
                           eyepos(3), ac, dpos(104), tmp
real(kind=dbl)          :: euld(3), om(3,3), ro1(4), ro2(4), culast(3), holast(3), ro(4), rolast(4), splast(3), sp(3), xx 
type(orientationtype)   :: ot
type(orientationtyped)  :: otd
integer(kind=irg)       :: i,j,k, icnt, imax, nt, ns, h_edge(2,52)
! the parameter a represents infinity as tan(178°/2)
real(kind=dbl)          :: a = 57.289922125538D0, b = 1.0D0, c = 1.0D0, cpos(3,104), dt = 114.57984425107713D0, &
                           ds = 2.0D0, d = 1.7320508075688772D0, dd, zz = 0.D0, oo = 1.D0, dtor =  0.017453292D0


! define the coordinates of the tetragonal C4 (4) FZ in Rodrigues Space
do i=-6,6 
  if (abs(i).ne.6) then 
    cpos(1:3, 7+i) = (/ -a, dtan(dble(i)*30.D0*dtor*0.5D0) ,  c /)
  else
    cpos(1:3, 7+i) = (/ -a, a ,  c /)
    if (i.lt.0) cpos(2,7+i) = -cpos(2,7+i)
  end if
end do
do i=1,13
  cpos(1:3,13+i) = cpos(1:3,i)
  cpos(1,13+i) = -cpos(1,13+i)
end do

do i=1,26
  cpos(1:3,26+i) = cpos(1:3,i)
  tmp = cpos(1,26+i)
  cpos(1,26+i) = cpos(2,26+i)
  cpos(2,26+i) = tmp
end do

do i=1,52
  cpos(1:3,52+i) = cpos(1:3,i)
  cpos(3,52+i) = -cpos(3,52+i)
end do

! and normalize
do i=1,104
  dpos(i) = dsqrt(sum(cpos(1:3,i)*cpos(1:3,i)))
end do

ns = 200
dx = dt/float(ns-1)

! define the connectivity of all the edges
do i=1,13
  h_edge(1:2,i)    = (/    i, 13+i /)
  h_edge(1:2,13+i) = (/ 26+i, 39+i /)
  h_edge(1:2,26+i) = (/ 52+i, 65+i /)
  h_edge(1:2,39+i) = (/ 78+i, 91+i /)
end do

! first add the reference frame and any necessary wireframe diagrams
if (rmode.eq.1) then
  ac = 0.5D0 * LPs%ap
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addCubochoricCube(dunit)
end if
if (rmode.eq.2) then
  ac = 1.33067D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.3) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.4) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
end if

if ((rmode.eq.1).or.(rmode.eq.2)) then
! create the top and bottom planes first
 dx = 1.D0/dble(ns)
 do i=1,52
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  culast = ro2cu(ro1)
  holast = ro2ho(ro1)
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    cu = ro2cu(ro)
    ho = ro2ho(ro)
! and create a cylinder with these points
    if (rmode.eq.1) then
      call PoVRay_addCylinder(dunit, culast(1:3),cu(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
      call PoVRay_addCylinder(dunit, holast(1:3),ho(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    culast = cu
    holast = ho
  end do 
 end do
end if
  
if ((rmode.eq.3).or.(rmode.eq.4)) then
! create the square edges first
 dx = 1.D0/dble(ns)
 do i=1,52
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  rolast = ro1
  qu = ro2qu(ro1)
  splast(1:3) = qu(2:4)/ (1.D0 + qu(1))
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    qu = ro2qu(ro)
    sp(1:3) = qu(2:4)/ (1.D0 + qu(1))
! and create a cylinder with these points
    if (rmode.eq.3) then
       call PoVRay_addCylinder(dunit, splast(1:3),sp(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
       call PoVRay_addCylinder(dunit, rolast(1:3)*rolast(4),ro(1:3)*ro(4),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    rolast = ro
    splast = sp
  end do 
 end do
end if
  
end subroutine PoVRay_initFZ4

!--------------------------------------------------------------------------
!
! SUBROUTINE: PoVRay_initFZ6
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initialize the PoVRay output for rotational group 6
! 
!> @param dunit output unit number
!> @param rmode  1(cubochoric)|2(homochoric)|3(stereographic)|4(Rodrigues)
!
!> @date    09/09/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine PoVRay_initFZ6(dunit, rmode)
!DEC$ ATTRIBUTES DLLEXPORT :: PoVRay_initFZ6

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: dunit
integer(kind=irg),INTENT(IN)          :: rmode

real(kind=dbl)          :: eul(3), axang(4), rv(3), qu(4), h(3), rmax, dx, r, xmax, x, y, z, ho(3), cu(3), zsmall, &
                           eyepos(3), ac, dpos(104), tmp
real(kind=dbl)          :: euld(3), om(3,3), ro1(4), ro2(4), culast(3), holast(3), ro(4), rolast(4), splast(3), sp(3), xx 
type(orientationtype)   :: ot
type(orientationtyped)  :: otd
integer(kind=irg)       :: i,j,k, icnt, imax, nt, ns, h_edge(2,52)
! the parameter a represents infinity as tan(178°/2)
real(kind=dbl)          :: a = 57.289922125538D0, b = 1.0D0, c = 1.0D0, cpos(3,104), dt = 114.57984425107713D0, &
                           ds = 2.0D0, d = 1.7320508075688772D0, dd, zz = 0.D0, oo = 1.D0, dtor =  0.017453292D0


! define the coordinates of the hexagonal C6 (6) FZ in Rodrigues Space
do i=-6,6 
  if (abs(i).ne.6) then 
    cpos(1:3, 7+i) = (/ -a, dtan(dble(i)*30.D0*dtor*0.5D0) ,  c /)
  else
    cpos(1:3, 7+i) = (/ -a, a ,  c /)
    if (i.lt.0) cpos(2,7+i) = -cpos(2,7+i)
  end if
end do
do i=1,13
  cpos(1:3,13+i) = cpos(1:3,i)
  cpos(1,13+i) = -cpos(1,13+i)
end do

do i=1,26
  cpos(1:3,26+i) = cpos(1:3,i)
  tmp = cpos(1,26+i)
  cpos(1,26+i) = cpos(2,26+i)
  cpos(2,26+i) = tmp
end do

do i=1,52
  cpos(1:3,52+i) = cpos(1:3,i)
  cpos(3,52+i) = -cpos(3,52+i)
end do

! and normalize
do i=1,104
  dpos(i) = dsqrt(sum(cpos(1:3,i)*cpos(1:3,i)))
end do

ns = 200
dx = dt/float(ns-1)

! define the connectivity of all the edges
do i=1,13
  h_edge(1:2,i)    = (/    i, 13+i /)
  h_edge(1:2,13+i) = (/ 26+i, 39+i /)
  h_edge(1:2,26+i) = (/ 52+i, 65+i /)
  h_edge(1:2,39+i) = (/ 78+i, 91+i /)
end do

! first add the reference frame and any necessary wireframe diagrams
if (rmode.eq.1) then
  ac = 0.5D0 * LPs%ap
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addCubochoricCube(dunit)
end if
if (rmode.eq.2) then
  ac = 1.33067D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.3) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
  call PoVRay_addWireFrameSphere(dunit, ac)
end if
if (rmode.eq.4) then 
  ac = 1.0D0
  call PoVRay_addReferenceFrame(dunit, ac)
end if

if ((rmode.eq.1).or.(rmode.eq.2)) then
! create the top and bottom planes first
 dx = 1.D0/dble(ns)
 do i=1,52
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  culast = ro2cu(ro1)
  holast = ro2ho(ro1)
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    cu = ro2cu(ro)
    ho = ro2ho(ro)
! and create a cylinder with these points
    if (rmode.eq.1) then
      call PoVRay_addCylinder(dunit, culast(1:3),cu(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
      call PoVRay_addCylinder(dunit, holast(1:3),ho(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    culast = cu
    holast = ho
  end do 
 end do
end if
  
if ((rmode.eq.3).or.(rmode.eq.4)) then
! create the square edges first
 dx = 1.D0/dble(ns)
 do i=1,52
  ro1(1:3) = cpos(1:3,h_edge(1,i))/dpos(i)
  ro1(4) = dpos(i)
  ro2(1:3) = cpos(1:3,h_edge(2,i))/dpos(i)
  ro2(4) = dpos(i)
  rolast = ro1
  qu = ro2qu(ro1)
  splast(1:3) = qu(2:4)/ (1.D0 + qu(1))
  do j=1,ns+1
    ro(1:3) = dpos(i)*ro1(1:3) + dpos(i)*(ro2(1:3) - ro1(1:3)) * j * dx
    xx = dsqrt( sum (ro(1:3)**2) )
    ro(1:3) = ro(1:3)/xx
    ro(4) = xx
    qu = ro2qu(ro)
    sp(1:3) = qu(2:4)/ (1.D0 + qu(1))
! and create a cylinder with these points
    if (rmode.eq.3) then
       call PoVRay_addCylinder(dunit, splast(1:3),sp(1:3),0.005D0,(/ 0.0, 0.0, 1.0 /))
    else
       call PoVRay_addCylinder(dunit, rolast(1:3)*rolast(4),ro(1:3)*ro(4),0.005D0,(/ 0.0, 0.0, 1.0 /))
    end if
    rolast = ro
    splast = sp
  end do 
 end do
end if
  
end subroutine PoVRay_initFZ6








end module povray

