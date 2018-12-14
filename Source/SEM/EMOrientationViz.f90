! ###################################################################
! Copyright (c) 2016-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:EMOrientationViz.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMOrientationViz
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a PoVray file with a visualization of orientation data
!
!> @date 01/30/17 MDG 1.0 original
!> @date 02/05/17 MDG 1.1 added 3D Density File (DF3) option
!> @date 06/13/17 MDG 1.2 added 3D .mrc output file option as alternative to .pov
!> @date 06/20/17 MDG 1.3 added trilinear "splatting" to volume output
!--------------------------------------------------------------------------
program EMOrientationViz
! 
! this program draws a data set of orientations in the selected Fundamental Zone
! the output is in .pov format for rendering with PoVRay, or as a data volume in
! an .mrc file for rendering with Chimera or Fiji.

use io
use local
use files
use rotations
use constants
use Lambert
use quaternions
use povray
use so3
use dictmod
use MRCmod
use math
use NameListTypedefs
use NameListHandlers
use ECPmod
use error

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(OrientationVizNameListType)        :: enl

real(kind=dbl)          :: rod(4), cu(3), ho(3), qu(4), eu(3), sh(3), xyz(3), eyepos(3), XY(2), euFZ(3), rstep, ac, dd, qur(4), &
                           rodrot(3), newrod(3), rodcor(3), quri(4), Mu(4)
type(orientationtype)   :: ot
type(orientationtyped)  :: otd
integer(kind=irg)       :: i,j,k, icnt, imax, nt, npx, ngroups, groups(10), dataunit4=25, dataunit5=40, ierr, ig, ix, iy, iz, num

real(kind=dbl)          :: delta, eps = 1.0D-2
character(fnlen)        :: locationline, fname, dataname, outname, lightline, skyline, rgbstring, colorstring, df3name, mrcname
character(11)           :: p0
character(21)           :: p1, p2
character(5)            :: px, py, pz, pd
character(7)            :: pd2
character(3)            :: gid(10)
character(2)            :: angleformat
integer(kind=irg)       :: FZtype, FZorder, numpoints, seed, istat, ro(3), FZtype_override, FZorder_override
logical                 :: levelset, skipthis, drawMK
type(dicttype),pointer  :: dict
real(kind=dbl),allocatable :: samples(:,:)
real(kind=dbl)          :: muhat(4), kappahat
! rendering volumes
real(kind=sgl),allocatable :: rovol(:,:,:), spvol(:,:,:), euvol(:,:,:), cuvol(:,:,:), hovol(:,:,:)
type(MRCstruct)         :: MRCheader
type(FEIstruct)         :: FEIheaders(1024)
real(kind=dbl),allocatable :: psum(:)
real(kind=dbl),allocatable :: volume(:,:,:)  ! you'll need to fill this array with values ... 
integer(kind=irg)       :: numx, numy, numz       ! set these to the size of the volume array
real(kind=sgl)          :: maxRFZdis(5), rodx, rody, rodz, eudx, eudy, eudz, spdx, spdy, spdz, cudx, cudy, cudz, &
                           hodx, hody, hodz, scalefactors(3,5), acubo, ahomo, grid3(3,3,3)

nmldeffile = 'EMOrientationViz.nml'
progname = 'EMOrientationViz.f90'
progdesc = 'Generate a povray scene file for visualization of an orientation data set'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 86 /), progname)

! deal with the namelist stuff
call GetOrientationVizNameList(nmldeffile,enl)

grid3 = trilinear_splat( (/ 0.0, 0.0, 0.0/), (/ 0.0, 0.0, 0.0/), init=.TRUE.)

drawMK = .FALSE.
if (enl%MacKenzieCell.eq.1) drawMK = .TRUE.

! define some parameters 
sh = (/ sngl(cPi), sngl(cPi/2.D0), sngl(cPi) /)   ! offset parameter for primary Euler cell

! define the eye position so that we can create a movie in povray (use the clock variable)
eyepos(1:3) = (/ 1.5D0, 3.2D0, 1.6D0 /)
dd = enl%distance  ! used to be dsqrt( sum( eyepos*eyepos))
eyepos = eyepos/dsqrt( sum( eyepos*eyepos))

! write the parameters into short strings
p0 = "location < "
p1 = "*cos(clock*0.0174533)"
p2 = "*sin(clock*0.0174533)"
write (px,"(F5.3)") eyepos(1)
write (py,"(F5.3)") eyepos(2)
write (pz,"(F5.3)") eyepos(3)
write (pd,"(F5.3)") dd

! create the location and light lines
locationline = p0//px//p1//"-"//py//p2//","//px//p2//"+"//py//p1//","//pz//">*"//pd
lightline =  '<1, 2, -2>*50'

! generate the symmetry quaternions
nullify(dict)
allocate(dict)
dict%Num_of_init = 3 
dict%Num_of_iterations = 30
! get the point group number from the xtal file
dict%pgnum = GetPointGroup(enl%xtalname)
call DI_Init(dict,'WAT')  ! generate the quaternions; 'WAT' does not matter, could also be 'VMF'
num = dict%Nqsym

if (enl%overridepgnum.ne.0) then
  FZtype_override = FZtarray(enl%overridepgnum)
  FZorder_override = FZoarray(enl%overridepgnum)
end if

! set the basic FZ type; this sets the symmetry operators as well... 
FZtype = FZtarray(dict%pgnum)
FZorder = FZoarray(dict%pgnum)

write (*,*) 'FZtype, FZorder = ',dict%pgnum, FZtype, FZorder

! define some FZ dimensional parameters (used for volume rendering)
maxRFZdis = (/ 5.0, 2.5, 1.0, 1.0/3.0, sqrt(2.0)-1.0 /)
scalefactors(1:3,1) = (/ 1.0, 1.0, 1.0 /)
if (enl%overridepgnum.eq.0) then 
  if (FZorder.ne.0) then
    scalefactors(1:3,2) = (/ 2.0, 2.0, 2.0*tan(sngl(cPi)*0.5/float(FZorder)) /)
    scalefactors(1:3,3) = (/ 2.0, 2.0, 2.0*tan(sngl(cPi)*0.5/float(FZorder)) /)
  end if
else
  if (FZorder_override.ne.0) then
    scalefactors(1:3,2) = (/ 2.0, 2.0, 2.0*tan(sngl(cPi)*0.5/float(FZorder_override)) /)
    scalefactors(1:3,3) = (/ 2.0, 2.0, 2.0*tan(sngl(cPi)*0.5/float(FZorder_override)) /)
  end if
end if
scalefactors(1:3,4) = (/ 2.0/3.0, 2.0/3.0, 2.0/3.0 /)
scalefactors(1:3,5) = (/ (sqrt(2.0)-1.0)/0.5, (sqrt(2.0)-1.0)/0.5, (sqrt(2.0)-1.0)/0.5 /)


! data file of orientations
dataname = trim(enl%anglefile)

! output file name 
outname = trim(enl%povrayfile)
! make sure that the outname does not have an extension (no . in the string)
if ((index(trim(outname),'.').ne.0).and.(index(trim(outname),'.').gt.20)) then
  call FatalError('EMOrientationViz.f90','the output filename may not contain any periods')
end if 
outname = trim(EMsoft_getEMdatapathname())//trim(enl%povrayfile)
outname = EMsoft_toNativePath(outname)

! first we open all the requested .pov files and initalize the scenes
if (enl%cubochoric.ne.0) then
 if (enl%mrcmode.eq.'off') then
  fname = trim(outname)//'-cu.pov'
  call Message('opening '//trim(fname))

  call PoVRay_openFile(dataunit, fname, nmldeffile)
  call PoVRay_setCamera(dataunit, locationline)
  call PoVRay_setLightSource(dataunit, lightline)
  if (trim(enl%df3file).eq.'undefined') then 
! we're just going to draw a bunch of spheres, so put them together in a PoVRay union
    if (enl%overridepgnum.eq.0) then
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype_override, FZorder_override)
      end if
   end if
    write (dataunit,"('union { ')")
  else
 ! insert code to read in a 3D Density File (df3) containing the object to be rendered
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-cu.df3'
    df3name = EMsoft_toNativePath(df3name)
    if (enl%scalingmode.eq.'lev') then
      call PoVRay_declare_DF3file(dataunit, df3name,.TRUE.)
    else
      call PoVRay_declare_DF3file(dataunit, df3name)
    end if
    if (enl%overridepgnum.eq.0) then
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit, 1, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
  end if
 end if
! create the rendering volume
  allocate(cuvol(-enl%nx:enl%nx,-enl%ny:enl%ny,-enl%nz:enl%nz),stat=istat)
  cuvol = 0.0
! generate the x/y/z scaling factors
! these should be set so that the render box has the outer dimensions of the cubochoric cube,
  acubo = sngl(cPi)**0.6666666 * 0.5
  cudx = float(enl%nx) / acubo
  cudy = float(enl%nx) / acubo
  cudz = float(enl%nz) / acubo
!   write (*,*) 'scaling factors ', cudx, cudy, cudz
end if

if (enl%homochoric.ne.0) then
 if (enl%mrcmode.eq.'off') then
  fname = trim(outname)//'-ho.pov'
  call Message('opening '//trim(fname))

  call PoVRay_openFile(dataunit2, fname, nmldeffile)
  call PoVRay_setCamera(dataunit2, locationline)
  call PoVRay_setLightSource(dataunit2, lightline)
  if (trim(enl%df3file).eq.'undefined') then 
! we're just going to draw a bunch of spheres, so put them together in a PoVRay union  
    if (enl%overridepgnum.eq.0) then
       if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
    write (dataunit2,"('union { ')")
  else
 ! insert code to read in a 3D Density File (df3) containing the object to be rendered
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-ho.df3'
    df3name = EMsoft_toNativePath(df3name)
    if (enl%scalingmode.eq.'lev') then
      call PoVRay_declare_DF3file(dataunit2, df3name,.TRUE.)
    else
      call PoVRay_declare_DF3file(dataunit2, df3name)
    end if
    if (enl%overridepgnum.eq.0) then
       if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit2, 2, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
   end if
  end if
! create the rendering volume
  allocate(hovol(-enl%nx:enl%nx,-enl%ny:enl%ny,-enl%nz:enl%nz),stat=istat)
  hovol = 0.0
! generate the x/y/z scaling factors
! these should be set so that the render box has the outer dimensions of the cubochoric cube,
  ahomo = (3.0*sngl(cPi)/4.0)**0.3333333
  hodx = float(enl%nx) / ahomo
  hody = float(enl%nx) / ahomo
  hodz = float(enl%nz) / ahomo
!   write (*,*) 'scaling factors ', hodx, hody, hodz
end if

if (enl%rodrigues.ne.0) then
 if (enl%mrcmode.eq.'off') then
  fname = trim(outname)//'-ro.pov'
  call Message('opening '//trim(fname))

  call PoVRay_openFile(dataunit3, fname, nmldeffile)
  call PoVRay_setCamera(dataunit3, locationline)
  call PoVRay_setLightSource(dataunit3, lightline, .TRUE.)
  if (trim(enl%df3file).eq.'undefined') then 
! we're just going to draw a bunch of spheres, so put them together in a PoVRay union
    if (enl%overridepgnum.eq.0) then
      if (drawMK.eqv..TRUE.) then
        write (*,*) 'drawing MFZ 1', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        write (*,*) 'drawing MFZ 2', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        write (*,*) 'drawing MFZ 3', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        write (*,*) 'drawing MFZ 4', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
    write (dataunit3,"('union { ')")
  else
! insert code to read in a 3D Density File (df3) containing the object to be rendered
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-ro.df3'
    df3name = EMsoft_toNativePath(df3name)
    if (enl%scalingmode.eq.'lev') then
      levelset = .TRUE.
      call PoVRay_declare_DF3file(dataunit3, df3name,levelset)
    else
      call PoVRay_declare_DF3file(dataunit3, df3name)
    end if
    if (enl%overridepgnum.eq.0) then
      if (drawMK.eqv..TRUE.) then
        write (*,*) 'drawing MFZ 5', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        write (*,*) 'drawing MFZ 6', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        write (*,*) 'drawing MFZ 7', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        write (*,*) 'drawing MFZ 8', drawMK
        call PoVRay_drawFZ(dataunit3, 4, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
   end if
  end if
! create the rendering volume
  allocate(rovol(-enl%nx:enl%nx,-enl%ny:enl%ny,-enl%nz:enl%nz),stat=istat)
  rovol = 0.0
! generate the x/y/z scaling factors
! these should be set so that the render box has the outer dimensions of the RFZ,
! with the exception of the cyclic groups for which we pick some value that is 
! reasonable.  For the dihedral groups the box should have an edge length of 2,
! for tetrahedral 1/3, and for octahedral sqrt(2)-1.
  if (enl%overridepgnum.eq.0) then
    rodx = float(enl%nx) / maxRFZdis(FZtype+1)
    rody = float(enl%nx) / maxRFZdis(FZtype+1)
    if ((FZtype.eq.1).or.(FZtype.eq.2)) then
      rodz = float(enl%nz) / tan(cPi*0.5/float(FZorder))
    else
      rodz = float(enl%nz) / maxRFZdis(FZtype+1) 
    end if
  else
    rodx = float(enl%nx) / maxRFZdis(FZtype_override+1)
    rody = float(enl%nx) / maxRFZdis(FZtype_override+1)
    if ((FZtype_override.eq.1).or.(FZtype_override.eq.2)) then
      rodz = float(enl%nz) / tan(cPi*0.5/float(FZorder_override))
    else
      rodz = float(enl%nz) / maxRFZdis(FZtype_override+1) 
    end if
  end if
end if

if (enl%stereographic.ne.0) then
 if (enl%mrcmode.eq.'off') then
  fname = trim(outname)//'-sp.pov'
  call Message('opening '//trim(fname))

  call PoVRay_openFile(dataunit4, fname, nmldeffile)
  call PoVRay_setCamera(dataunit4, locationline)
  call PoVRay_setLightSource(dataunit4, lightline)
! we're just going to draw a bunch of spheres, so put them together in a PoVRay union
  if (trim(enl%df3file).eq.'undefined') then 
    if (enl%overridepgnum.eq.0) then
       if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
    write (dataunit4,"('union { ')")
  else
! insert code to read in a 3D Density File (df3) containing the object to be rendered
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-sp.df3'
    df3name = EMsoft_toNativePath(df3name)
    if (enl%scalingmode.eq.'lev') then
      call PoVRay_declare_DF3file(dataunit4, df3name,.TRUE.)
    else
      call PoVRay_declare_DF3file(dataunit4, df3name)
    end if
    if (enl%overridepgnum.eq.0) then
       if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit4, 3, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
   end if
  end if
! create the rendering volume
  allocate(spvol(-enl%nx:enl%nx,-enl%ny:enl%ny,-enl%nz:enl%nz),stat=istat)
  spvol = 0.0
! generate the x/y/z scaling factors
! these should be set so that the render box has the outer dimensions of the stereographic sphere,
! which has unit radius.
  spdx = float(enl%nx)
  spdy = float(enl%nx)
  spdz = float(enl%nz)
end if

if (enl%eulerspace.ne.0) then
 if (enl%mrcmode.eq.'off') then
  fname = trim(outname)//'-eu.pov'
  call Message('opening '//trim(fname))

! Euler space has a slightly different eye position so we need to redefine the locationline and skyline strings
  eyepos(1:3) = (/ 1.5D0, 1.6D0, 1.6D0 /)
  dd = enl%distance   !  dsqrt( sum( eyepos*eyepos))
  eyepos = eyepos/dsqrt( sum( eyepos*eyepos))

  p0 = "location < "
  p1 = "*cos(clock*0.0174533)"
  p2 = "*sin(clock*0.0174533)"
  write (px,"(F5.3)") eyepos(1)
  write (py,"(F5.3)") eyepos(2)
  write (pz,"(F5.3)") eyepos(3)
  write (pd2,"(F7.3)") 10.0*dd
  locationline = p0//px//p1//"-"//py//p2//","//pz//","//px//p2//"+"//py//p1//">*"//pd2

  call PoVRay_openFile(dataunit5, fname, nmldeffile)
  skyline = 'sky < 0.0, 1.0, 0.0>'
  call PoVRay_setCamera(dataunit5, locationline, skyline)
  call PoVRay_setLightSource(dataunit5, lightline)
! we're just going to draw a bunch of spheres, so put them together in a PoVRay union
  if (trim(enl%df3file).eq.'undefined') then 
    if (enl%overridepgnum.eq.0) then
       if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
    write (dataunit5,"('union { ')")
  else
   ! insert code to read in a 3D Density File (df3) containing the object to be rendered
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-eu.df3'
    df3name = EMsoft_toNativePath(df3name)
    if (enl%scalingmode.eq.'lev') then
      call PoVRay_declare_DF3file(dataunit5, df3name,.TRUE.)
    else
      call PoVRay_declare_DF3file(dataunit5, df3name)
    end if
    if (enl%overridepgnum.eq.0) then
       if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype, FZorder, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype, FZorder)
      end if
    else
      if (drawMK.eqv..TRUE.) then
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype_override, FZorder_override, MFZ=drawMK)
      else
        call PoVRay_drawFZ(dataunit5, 5, 0.005D0, FZtype_override, FZorder_override)
      end if
    end if
   end if
  end if
! create the rendering volume
  allocate(euvol(1:2*enl%nx+1,1:2*enl%ny+1,1:2*enl%nz+1),stat=istat)
  euvol = 0.0
! generate the x/y/z scaling factors
! these should be set so that the render box has the outer dimensions of the primary Euler cell
! which has unit radius.
  eudx = float(2*enl%nx+1) / (2.0*cPi)
  eudy = float(2*enl%ny+1) / cPi
  eudz = float(2*enl%nz+1) / (2.0*cPi)
end if

! open the input data file and read the orientation format and the number of orientations
fname = trim(EMsoft_getEMdatapathname())//trim(enl%anglefile)
fname = EMsoft_toNativePath(fname)
open(unit=53,file=trim(fname),status='old',action='read')
read (53,*) angleformat
read (53,*) numpoints

if (enl%reducetoRFZ.eq.1) then 
  num = 1   ! we only display the point inside the RFZ
endif 

! here is the loop over all points
do ix = 1,numpoints

! we'll convert them first to Euler angle triplets, regardless of what the angleformat is...
  if (angleformat.eq.'ho') then
    read (53,*) ho(1:3)
    eu = ho2eu(ho)
  end if
  if (angleformat.eq.'qu') then
    read (53,*) qu(1:4)
    eu = qu2eu(qu)
  end if
  if (angleformat.eq.'ro') then
    read (53,*) rod(1:4)
    eu = ro2eu(rod)
  end if
  if (angleformat.eq.'cu') then
    read (53,*) cu(1:3)
    eu = cu2eu(cu)
  end if
  if (angleformat.eq.'eu') then
    read (53,*) eu(1:3)
    eu = eu * cPi/180.D0
  end if

! first, make sure the point lies inside the Rodrigues FZ or the relevant MacKenzie cell
  if (drawMK.eqv..TRUE.) then
    call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, euFZ, MFZ=.TRUE.)
  else
    call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, euFZ)
  end if
  eu = euFZ
  Mu = eu2qu(euFZ)
  if (Mu(1).lt.0.D0) Mu = -Mu

! then, if reducetoRFZ = 0, we must generate all the equivalent points as well

  do j=1,num
    if (j.gt.1) then
      qu = quat_mult(dict%Pm(1:4,j),Mu)
      if (qu(1).lt.0.D0) qu = -qu
      eu = qu2eu(qu)
    end if 

    skipthis = .FALSE.
    if (enl%overridepgnum.ne.0) then
! we need to check whether or not this point lies in the FZ of the override symmetry
      rod = eu2ro(eu)
      if (IsInsideFZ(rod,FZtype_override,FZorder_override).eqv..FALSE.) skipthis = .TRUE.
    end if

! are we drawing spheres ?
  if (skipthis.eqv..FALSE.) then
   if ((trim(enl%df3file).eq.'undefined').and.(enl%mrcmode.eq.'off')) then 

! we have the point, so now transform it to all alternative representations and draw the 
! corresponding spheres and their opposites
! cubochoric
      if (enl%cubochoric.ne.0) then
        cu = eu2cu(eu)
        write (dataunit,"('sphere { <',2(F14.6,','),F14.6,'>,',F6.4,'  }')") cu(1:3), enl%sphrad
      end if
! homochoric
      if (enl%homochoric.ne.0) then
        ho = eu2ho(eu)
        write (dataunit2,"('sphere { <',2(F14.6,','),F14.6,'>,',F6.4,' }')") ho(1:3), enl%sphrad
      end if
! rodrigues
      if (enl%rodrigues.ne.0) then
        rod = eu2ro(eu)
        if (rod(4).le.1000.0) then 
          write (dataunit3,"('sphere { <',2(F20.6,','),F20.6,'>,',F6.4,' }')") rod(1:3)*rod(4), enl%sphrad
        end if
      end if
! stereographic
      if (enl%stereographic.ne.0) then
        qu = eu2qu(eu)
        xyz = qu(2:4)/(1.D0+qu(1))
        write (dataunit4,"('sphere { <',2(F14.6,','),F14.6,'>,',F6.4,' }')") xyz(1:3), enl%sphrad
      end if
! Euler box
      if (enl%eulerspace.ne.0) then
        write (dataunit5,"('sphere { <',2(F14.6,','),F14.6,'>,',F6.4,' }')") eu(1:3) - sh(1:3), enl%sphrad
      end if
    else
! no, we're not drawing spheres, but we're filling up the rendering volume

! cubochoric rendering volume
      if (enl%cubochoric.ne.0) then
        cu = eu2cu(eu)
        grid3 = trilinear_splat( sngl(cu(1:3)), (/ cudx, cudy, cudz /), init=.FALSE.)
        ro(1:3) = nint(cu(1:3) * (/ cudx, cudy, cudz /))
        if ((abs(ro(1)).lt.enl%nx).and.(abs(ro(2)).lt.enl%ny).and.(abs(ro(3)).lt.enl%nz) ) then
          cuvol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) = &
            cuvol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) + grid3
        end if
      end if

! homochoric rendering volume
      if (enl%homochoric.ne.0) then
        ho = eu2ho(eu)
        grid3 = trilinear_splat( sngl(ho(1:3)), (/ hodx, hody, hodz /), init=.FALSE.)
        ro(1:3) = nint(ho(1:3) * (/ hodx, hody, hodz /))
        if ((abs(ro(1)).lt.enl%nx).and.(abs(ro(2)).lt.enl%ny).and.(abs(ro(3)).lt.enl%nz) ) then
          hovol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) = &
            hovol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) + grid3
        end if
      end if

! rodrigues rendering volume
      if (enl%rodrigues.ne.0) then
        rod = eu2ro(eu)
        grid3 = trilinear_splat( sngl(rod(1:3)*rod(4)), (/ rodx, rody, rodz /), init=.FALSE.)
        ro(1:3) = nint((rod(1:3)*rod(4)) * (/ rodx, rody, rodz /))
        if ((abs(ro(1)).lt.enl%nx).and.(abs(ro(2)).lt.enl%ny).and.(abs(ro(3)).lt.enl%nz) ) then
          rovol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) = &
            rovol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) + grid3
        end if
      end if

! stereographic rendering volume
      if (enl%stereographic.ne.0) then
        qu = eu2qu(eu)
        xyz = qu(2:4)/(1.D0+qu(1))
        grid3 = trilinear_splat( sngl(xyz), (/ spdx, spdy, spdz /), init=.FALSE.)
        ro(1:3) = nint(xyz(1:3) * (/ spdx, spdy, spdz /))
        ! in the following test, we are potentially eliminating a few points that lie on
        ! the surface of the volume array...
        if ((abs(ro(1)).lt.enl%nx).and.(abs(ro(2)).lt.enl%ny).and.(abs(ro(3)).lt.enl%nz) ) then
          spvol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) = &
            spvol(ro(1)-1:ro(1)+1,ro(2)-1:ro(2)+1,ro(3)-1:ro(3)+1) + grid3
        end if
      end if

! Euler primary cell rendering volume  [needs to be updated with trilinear splatting]
      if (enl%eulerspace.ne.0) then
        eu(1) = mod(eu(1)+10.0*cPi,2.0*cPi)
        eu(2) = mod(eu(2)+5.0*cPi,cPi)
        eu(3) = mod(eu(3)+10.0*cPi,2.0*cPi)
        ro(1:3) = nint(eu(1:3) * (/ eudx, eudy, eudz /))
        if ((ro(1).le.2*enl%nx).and.(ro(2).le.2*enl%ny).and.(ro(3).le.2*enl%nz) ) then
          euvol(ro(1)+1,ro(2)+1,ro(3)+1) = euvol(ro(1)+1,ro(2)+1,ro(3)+1) + 1.0
        end if
      end if
    end if  ! df3 mode ?
   end if ! skipthis = .FALSE.
  end do ! loop over equivalent points
end do

if (enl%mrcmode.eq.'off') then 
 if (trim(enl%df3file).eq.'undefined') then ! we're drawing spheres, so close the union and add color information
  write(rgbstring,"(F8.6,',',F8.6,',',F8.6)") enl%rgb(1:3)
  colorstring = 'material { texture { pigment { rgb <'//trim(rgbstring)//'> filter 0.95 }'
  if (enl%cubochoric.ne.0) then
    write (dataunit,"(A)") trim(colorstring)
    write (dataunit,"(' finish { diffuse 0.6, 0.6 brilliance 1.0 } } } } ')")
    write (dataunit,"(A)") 'background { color rgb <0.9, 0.9, 0.9> }'
    close(UNIT=dataunit,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-cu.pov')
  end if

  if (enl%homochoric.ne.0) then
    write (dataunit2,"(A)") trim(colorstring)
    write (dataunit2,"(' finish { diffuse 0.6, 0.6 brilliance 1.0 }  } } }')")
    write (dataunit2,"(A)") 'background { color rgb <0.9, 0.9, 0.9> }'
    close(UNIT=dataunit2,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-ho.pov')
  end if

  if (enl%rodrigues.ne.0) then
    write (dataunit3,"(A)") trim(colorstring)
    write (dataunit3,"('finish { diffuse 0.6, 0.6 brilliance 1.0 }  } } }')")
    write (dataunit3,"(A)") 'background { color rgb <0.9, 0.9, 0.9> }'
    close(UNIT=dataunit3,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-ro.pov')
  end if

  if (enl%stereographic.ne.0) then
    write (dataunit4,"(A)") trim(colorstring)
    write (dataunit4,"(' finish { diffuse 0.6, 0.6 brilliance 1.0 }  } } }')")
    write (dataunit4,"(A)") 'background { color rgb <0.9, 0.9, 0.9> }'
    close(UNIT=dataunit4,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-sp.pov')
  end if

  if (enl%eulerspace.ne.0) then
    write (dataunit5,"(A)") trim(colorstring)
    write (dataunit5,"(' finish { diffuse 0.6, 0.6 brilliance 1.0 }  } } }')")
    write (dataunit5,"(A)") 'background { color rgb <0.9, 0.9, 0.9> }'
    close(UNIT=dataunit5,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-eu.pov')
  end if
 else ! we're writing the rendering volume to a DF3 file and closing the povray file

  if (enl%cubochoric.ne.0) then
! output the final rendering commands
    write (dataunit,"(A)") 'background { color rgb <0.2, 0.2, 0.2> }'
    write (dataunit,"('object { renderbox translate <-0.5, -0.5, -0.5>')")
    write (dataunit,"(' scale <',F10.6,',',F10.6,',',F10.6,' > }')") 2.0 * (/ acubo, acubo, acubo /)
! and close the file
    close(UNIT=dataunit,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-cu.pov')
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-cu.df3'
    df3name = EMsoft_toNativePath(df3name)
    call PoVRay_write_DF3file(dataunit, df3name, cuvol, (/ enl%nx, enl%ny, enl%nz /), enl%scalingmode)
  end if

  if (enl%homochoric.ne.0) then
! output the final rendering commands
    write (dataunit2,"(A)") 'background { color rgb <0.2, 0.2, 0.2> }'
    write (dataunit2,"('object { renderbox translate <-0.5, -0.5, -0.5>')")
    write (dataunit2,"(' scale <',F10.6,',',F10.6,',',F10.6,' > }')") 2.0 * (/ ahomo, ahomo, ahomo /)
! and close the file
    close(UNIT=dataunit2,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-ho.pov')
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-ho.df3'
    df3name = EMsoft_toNativePath(df3name)
    call PoVRay_write_DF3file(dataunit2, df3name, hovol, (/ enl%nx, enl%ny, enl%nz /), enl%scalingmode)
  end if

  if (enl%rodrigues.ne.0) then
! output the final rendering commands
    write (dataunit3,"(A)") 'background { color rgb <0.2, 0.2, 0.2> }'
    write (dataunit3,"('object { renderbox translate <-0.5, -0.5, -0.5>')")
    if (enl%overridepgnum.eq.0) then
      write (dataunit3,"(' scale <',F10.6,',',F10.6,',',F10.6,' > }')") scalefactors(1:3,FZtype+1)
    else
      write (dataunit3,"(' scale <',F10.6,',',F10.6,',',F10.6,' > }')") scalefactors(1:3,FZtype_override+1)
    end if
! and close the file
    close(UNIT=dataunit3,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-ro.pov')
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-ro.df3'
    df3name = EMsoft_toNativePath(df3name)
    call PoVRay_write_DF3file(dataunit3, df3name, rovol, (/ enl%nx, enl%ny, enl%nz /), enl%scalingmode)
  end if

  if (enl%stereographic.ne.0) then
! output the final rendering commands
    write (dataunit4,"(A)") 'background { color rgb <0.2, 0.2, 0.2> }'
    write (dataunit4,"('object { renderbox translate <-0.5, -0.5, -0.5>')")
    write (dataunit4,"(' scale <',F10.6,',',F10.6,',',F10.6,' > }')") (/ 2.0, 2.0, 2.0 /)
! and close the file
    close(UNIT=dataunit4,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-sp.pov')
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-sp.df3'
    df3name = EMsoft_toNativePath(df3name)

    write (*,*) 'total number of entries in spvol : ',sum(spvol)

    call PoVRay_write_DF3file(dataunit4, df3name, spvol, (/ enl%nx, enl%ny, enl%nz /), enl%scalingmode)
  end if

  if (enl%eulerspace.ne.0) then
! output the final rendering commands
    write (dataunit5,"(A)") 'background { color rgb <0.2, 0.2, 0.2> }'
    write (dataunit5,"('object { renderbox scale < 6.2831855, 3.1415927, 6.2831855 > ')")
    write (dataunit5,"(' translate <  -3.1415927, -1.5707964, -3.1415927 > } ')")
! and close the file
    close(UNIT=dataunit5,STATUS='keep')
    call Message('PoVray rendering script stored in '//trim(outname)//'-eu.pov')
    df3name = trim(EMsoft_getEMdatapathname())//trim(enl%df3file)//'-eu.df3'
    df3name = EMsoft_toNativePath(df3name)
    call PoVRay_write_DF3file(dataunit5, df3name, euvol, (/ enl%nx, enl%ny, enl%nz /), enl%scalingmode)
  end if
 end if
else  ! we're creating an .mrc file, so we do not need any of the povray commands...
 !write(*,*) 'starting creation of .mrc file'
! parameters that are generic to all the volumes
  numx = 2*enl%nx+1
  numy = 2*enl%ny+1
  numz = 2*enl%nz+1
  allocate(volume(numx,numy,numz))
  MRCheader%nx = numx
  MRCheader%ny = numy
  MRCheader%nz = numz
  MRCheader%mode = 2    ! for floating point output
  MRCheader%mx = numx
  MRCheader%my = numy
  MRCheader%mz = numz
  MRCheader%xlen = numx
  MRCheader%ylen = numy
  MRCheader%zlen = numz
  do i=1,numz
    FEIheaders(i)%b_tilt = 0.0
    FEIheaders(i)%defocus = 0.0
    FEIheaders(i)%pixelsize = 1.0e-9
    FEIheaders(i)%magnification = 1000.0
    FEIheaders(i)%voltage = 0.0
  end do
  allocate(psum(numz))
! write (*,*) 'dimensions :',numx, numy, numz

  if (enl%cubochoric.ne.0) then
! copy the cubochoric array into the volume array
    do ix = -enl%nx,enl%nx
     do iy = -enl%ny,enl%ny
      do iz = -enl%nz,enl%nz
        volume(ix+enl%nx+1, iy+enl%ny+1, iz+enl%nz+1) = dble(cuvol(ix,iy,iz))
      end do
     end do 
    end do    
! parameters specific to this volume
    psum = sum(sum(volume,1),1)
    do iz=1,numz
      FEIheaders(iz)%mean_int = psum(iz)/float(numx)/float(numy)
    end do
    MRCheader%amin = minval(volume)
    MRCheader%amax = maxval(volume)
    MRCheader%amean = sum(volume)/float(numx)/float(numy)/float(numz)
! set the filename
    mrcname = trim(EMsoft_getEMdatapathname())//trim(enl%mrcfile)//'-cu.mrc'
    mrcname = EMsoft_toNativePath(mrcname)
! and write the volume to file
    call MRC_write_3Dvolume(MRCheader,FEIheaders,mrcname,numx,numy,numz,volume,verbose=.TRUE.) 
  end if


  if (enl%homochoric.ne.0) then
! copy the homochoric array into the volume array
    do ix = -enl%nx,enl%nx
     do iy = -enl%ny,enl%ny
      do iz = -enl%nz,enl%nz
        volume(ix+enl%nx+1, iy+enl%ny+1, iz+enl%nz+1) = dble(hovol(ix,iy,iz))
      end do
     end do 
    end do    
! parameters specific to this volume
    psum = sum(sum(volume,1),1)
    do iz=0,numz-1
      FEIheaders(iz)%mean_int = psum(iz)/float(numx)/float(numy)
    end do
    MRCheader%amin = minval(volume)
    MRCheader%amax = maxval(volume)
    MRCheader%amean = sum(volume)/float(numx)/float(numy)/float(numz)
! set the filename
    mrcname = trim(EMsoft_getEMdatapathname())//trim(enl%mrcfile)//'-ho.mrc'
    mrcname = EMsoft_toNativePath(mrcname)
! and write the volume to file
    call MRC_write_3Dvolume(MRCheader,FEIheaders,mrcname,numx,numy,numz,volume,verbose=.TRUE.) 
  end if


  if (enl%rodrigues.ne.0) then
! copy the rodrigues array into the volume array
    do ix = -enl%nx,enl%nx
     do iy = -enl%ny,enl%ny
      do iz = -enl%nz,enl%nz
        volume(ix+enl%nx+1, iy+enl%ny+1, iz+enl%nz+1) = dble(rovol(ix,iy,iz))
      end do
     end do 
    end do    
! parameters specific to this volume
    psum = sum(sum(volume,1),1)
    do iz=1,numz
      FEIheaders(iz)%mean_int = psum(iz)/float(numx)/float(numy)
    end do
    MRCheader%amin = minval(volume)
    MRCheader%amax = maxval(volume)
    MRCheader%amean = sum(volume)/float(numx)/float(numy)/float(numz)
! set the filename
    mrcname = trim(EMsoft_getEMdatapathname())//trim(enl%mrcfile)//'-ro.mrc'
    mrcname = EMsoft_toNativePath(mrcname)
! and write the volume to file
    call MRC_write_3Dvolume(MRCheader,FEIheaders,mrcname,numx,numy,numz,volume,verbose=.TRUE.) 
  end if


  if (enl%stereographic.ne.0) then
  ! write (*,*) 'starting volume array copy'
! copy the stereographic array into the volume array
    do ix = -enl%nx,enl%nx
     do iy = -enl%ny,enl%ny
      do iz = -enl%nz,enl%nz
        volume(ix+enl%nx+1, iy+enl%ny+1, iz+enl%nz+1) = dble(spvol(ix,iy,iz))
      end do
     end do 
    end do    
  ! write (*,*) '  --> done'
! parameters specific to this volume
    psum = sum(sum(volume,1),1)
    do iz=1,numz
      FEIheaders(iz)%mean_int = psum(iz)/float(numx)/float(numy)
    end do
    MRCheader%amin = minval(volume)
    MRCheader%amax = maxval(volume)
    MRCheader%amean = sum(volume)/float(numx)/float(numy)/float(numz)
  ! write(*,*) MRCheader%amin, MRCheader%amax, MRCheader%amean
! set the filename
    mrcname = trim(EMsoft_getEMdatapathname())//trim(enl%mrcfile)//'-sp.mrc'
    mrcname = EMsoft_toNativePath(mrcname)
  ! write (*,*) 'mrcfile : ',trim(mrcname)
! and write the volume to file
    call MRC_write_3Dvolume(MRCheader,FEIheaders,mrcname,numx,numy,numz,volume,verbose=.TRUE.) 
  end if


  if (enl%eulerspace.ne.0) then
! copy the eulerspace array into the volume array
    volume = dble(euvol)
! parameters specific to this volume
    psum = sum(sum(volume,1),1)
    do iz=1,numz
      FEIheaders(iz)%mean_int = psum(iz)/float(numx)/float(numy)
    end do
    MRCheader%amin = minval(volume)
    MRCheader%amax = maxval(volume)
    MRCheader%amean = sum(volume)/float(numx)/float(numy)/float(numz)
! set the filename
    mrcname = trim(EMsoft_getEMdatapathname())//trim(enl%mrcfile)//'-eu.mrc'
    mrcname = EMsoft_toNativePath(mrcname)
! and write the volume to file
    call MRC_write_3Dvolume(MRCheader,FEIheaders,mrcname,numx,numy,numz,volume,verbose=.TRUE.) 
  end if

end if

end program EMOrientationViz
