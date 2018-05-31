! ###################################################################
! Copyright (c) 2014-2018, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMoSLERP.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMoSLERP
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @todo replace the cubes currently used in one of the rendering modes by parallellepipeds 
!> that have the actual shape of the unit cell for the crystal structure used to define the 
!> symmetry...
!
!> @brief generate POVray files to create a grain boundary interpolation movie
!
!> @date 05/05/18 MDG 1.0 original 
!--------------------------------------------------------------------------
program EMoSLERP

use local
use io
use files
use typedefs
use NameListTypedefs
use NameListHandlers

IMPLICIT NONE

character(fnlen)                  :: nmldeffile, progname, progdesc
type(oSLERPNameListType)          :: onl

nmldeffile = 'EMoSLERP.nml'
progname = 'EMoSLERP.f90'
progdesc = 'Generate POVray input files for an octonion SLERP interpolation between grain boundaries'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 100 /), progname)

! deal with the namelist stuff
call GetoSLERPNameList(nmldeffile,onl)

! and call the main routine
call oSLERPrender(onl)

end program EMoSLERP


!--------------------------------------------------------------------------
!
! SUBROUTINE: oSLERPrender
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief performs the interpolation and creates all the required POVray files for rendering
!
!> @date 05/05/18 MDG 1.0 original 
!--------------------------------------------------------------------------
subroutine oSLERPrender(onl)

use local
use NameListTypedefs
use dictmod
use ECPmod
use io
use files
use rotations
use Lambert
use rng
use omp_lib
use constants
use quaternions
use GBmod
use povray
use HDF5
use HDFsupport

IMPLICIT NONE

type(oSLERPNameListType),INTENT(IN)         :: onl

real(kind=dbl)                              :: oct1(8), oct2(8), Omega, dOmega, qn1(4), qn2(4), qinter(8), qint1(4), &
                                               qint2(4), qint(4), qmat(3,3), OB(3,3), ON(3,3), p, pA(4), pB(4), pC(4), &
                                               pD(4), qq(3), phiA, phiC, rhoA(4), rhoC(4), qmc(4), msA(3), msC(3), pp(3)
type(dicttype),pointer                      :: dict
integer(kind=irg)                           :: pgnum, numf, io_int(1), ios, i, hdferr
real(kind=dbl),allocatable                  :: tval(:)
character(4)                                :: number4
character(3)                                :: number3
character(2)                                :: number2
character(1)                                :: number1
character(fnlen)                            :: povname, subfolder, dirstring, respath, fname, outname, line, pvcmd
logical                                     :: fexists, frames_generated, dexists

nullify(dict)

! copy all the necessary PoVRay definition files from the EMsoft resources folder 
! there are two files to be copied: one contains a set of PoVRay macros by F. Lohmueller,
! the other contains specific macros for the oSLERP rendering
respath = trim(EMsoft_getResourcepathname())
fname = trim(respath)//'analytical_g.inc'
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname),exist=fexists)
if (fexists.eqv..FALSE.) then
    call Message('WARNING: the analytical_g.inc file is missing from the EMsoft resource folder...')
    call Message('         you will not be able to render the scene files.')
else
    outname = 'analytical_g.inc'
    open(UNIT=dataunit,FILE=trim(fname), STATUS='old', FORM='formatted',ACCESS='sequential')
    open(UNIT=dataunit2,FILE=trim(outname), STATUS='unknown', FORM='formatted',ACCESS='sequential')
    do
        read(dataunit,'(A)',iostat=ios) line
        if (ios.ne.0) then 
          exit
        end if
        write(dataunit2,'(A)') trim(line)
    end do
    close(UNIT=dataunit, STATUS='keep')
    close(UNIT=dataunit2, STATUS='keep')
    call Message(' --> copied analytical_g.inc file from resource folder')
end if

! repeat this for the octonionSLERP.inc include file 
fname = trim(respath)//'octonionSLERP.inc'
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname),exist=fexists)
if (fexists.eqv..FALSE.) then
    call Message('WARNING: the octonionSLERP.inc file is missing from the EMsoft resource folder...')
    call Message('         you will not be able to render the scene files.')
else
    outname = 'octonionSLERP.inc'
    open(UNIT=dataunit,FILE=trim(fname), STATUS='old', FORM='formatted',ACCESS='sequential')
    open(UNIT=dataunit2,FILE=trim(outname), STATUS='unknown', FORM='formatted',ACCESS='sequential')
    do
        read(dataunit,'(A)',iostat=ios) line
        if (ios.ne.0) then 
          exit
        end if
        write(dataunit2,'(A)') trim(line)
    end do
    close(UNIT=dataunit, STATUS='keep')
    close(UNIT=dataunit2, STATUS='keep')
    call Message(' --> copied octonionSLERP.inc file from resource folder')
end if

! get the point group number for this crystal structure
call h5open_EMsoft(hdferr)
pgnum = GetPointGroup(onl%xtalname,.FALSE.) 
call h5close_EMsoft(hdferr)

! if GBmode = 'normal', we must compute the relevant octonions
if (trim(onl%GBmode).eq.'normal') then 
    ! grains B and D are unrotated
    ! so A and C are rotated by the misorientation quaternion qm
    qmc = conjg(onl%qm)
    ! determine the boundary normals in the sample reference frame
    msA = quat_LP(onl%qm,onl%mA)
    msC = quat_LP(onl%qm,onl%mC)
    ! get the rotations to bring the normals to the z-axis
    phiA = acos(abs(msA(3))) * 0.5D0
    pp = (/ msA(2), -msA(1), 0.D0 /)
    pp = sin(phiA) * pp / NORM2(pp)
    rhoA = (/ cos(phiA), pp(1), pp(2), pp(3) /)    
    phiC = acos(abs(msC(3))) * 0.5D0
    pp = (/ msC(2), -msC(1), 0.D0 /)
    pp = sin(phiC) * pp / NORM2(pp)
    rhoC = (/ cos(phiC), pp(1), pp(2), pp(3) /)
    ! transform the quaternions and form the octonions
    oct1 = (/ quat_mult(qmc, rhoA), rhoA /)
    oct2 = (/ quat_mult(qmc, rhoC), rhoC /)
else
    oct1 = onl%o1
    oct2 = onl%o2
end if

! make sure the octonions are properly normalized, as are the component quaternions
! i.e., normalize each quaternion separately 
! we need to have for o = (u,v):  ||u||=||v|| and ||o||=2 
p = NORM2(oct1(1:4))
oct1(1:4) = oct1(1:4)/p
p = NORM2(oct1(5:8))
oct1(5:8) = oct1(5:8)/p
p = NORM2(oct2(1:4))
oct2(1:4) = oct2(1:4)/p
p = NORM2(oct2(5:8))
oct2(5:8) = oct2(5:8)/p

! define the subfolder name for the frames
subfolder = trim(EMsoft_getEMdatapathname())//trim(onl%framefolder)
subfolder = EMsoft_toNativePath(subfolder) 

! make sure that the frame folder exists
pvcmd = trim(subfolder)
inquire(file=trim(pvcmd),exist=dexists)
if (dexists.eqv..FALSE.) then 
    call Message(' --> creating frames folder')
    call system('mkdir '//trim(subfolder))
else
    call Message(' --> frames folder exists; removing existing files ')
    call system('rm '//trim(subfolder)//'/*')
end if

! get the misorientation angle
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum
call DI_Init(dict,'nil') 
Omega = GBO_Omega_Symmetric(oct1(1:4), oct1(5:8), oct2(1:4), oct2(5:8), dict)
deallocate(dict)
write (*,*) '--> octonion misorientation angle (degrees) ', Omega * 180.D0 / cPi

if (Omega.eq.0.D0) then 
    call Message('The misorientation angles between the octonions is zero.')
    call Message('No movie will be generated.')
    stop 'Program run aborted'
end if

! determine the number of movie frames
dOmega = onl%dOmega * cPi/180.D0
numf = nint(Omega/dOmega/2.0)
io_int(1) = numf
call WriteValue('Number of movie frames = ',io_int,1)

! construct the t values array for the interpolation parameter
allocate(tval(numf+1))
do i=1,numf+1
    tval(i) = float(i-1)/float(numf)
end do

! convert the octonions to the correct parameters for POVray visualization
qn1 = conjg(oct1(1:4))
qn1 = qn1/NORM2(qn1)
qn2 = conjg(oct2(1:4))
qn2 = qn2/NORM2(qn2)

do i=1,numf+1
    ! interpolate the octonions
    qinter = GBO_SLERP(oct1, oct2, Omega, tval(i), 8)
    qint1 = conjg(qinter(1:4))
    qint2 = qinter(5:8)
    qint = quat_mult(qint2,qint1)
    qmat = qu2om(qint/sqrt(sum(qint*qint)))
    OB = transpose(PoVRay_flipRotationMatrix(qmat))

    ! interpolate quaternions to get the grain boundary normal
    qint = GBO_SLERP(qn1, qn2, Omega, tval(i), 4) 
    qmat = qu2om(qint)
    ON = transpose(PoVRay_flipRotationMatrix(qmat))

    ! create the filename prefix
    if (numf.gt.1000) then 
        write (number4,"(I4.4)") i
        povname = trim(subfolder)//'/parameter'//number4//'.pov'
    else 
        write (number3,"(I3.3)") i
        povname = trim(subfolder)//'/parameter'//number3//'.pov'
    end if
        
    open(unit=dataunit,file=trim(povname),status='unknown',form='formatted')
    write(dataunit,"('// this file contains the orientations for grain B and for the interface normal')")
    write(dataunit,"('// this file can be generated by any script or code and must contain two orientation')")
    write(dataunit,"('// in the form of 4x4 matrices')")
    write(dataunit,"(' ')")
    write(dataunit,"('#macro GrainMatrix()  ')")
    write(dataunit,"('matrix < ',3(F10.6,','))") OB(1,1), OB(1,2), OB(1,3)
    write(dataunit,"(3(F10.6,','))") OB(2,1), OB(2,2), OB(2,3)
    write(dataunit,"(3(F10.6,','))") OB(3,1), OB(3,2), OB(3,3)
    write(dataunit,"('0.0, 0.0, 0.0 > ')")
    write(dataunit,"('#end')")
    write(dataunit,"(' ')")
    write(dataunit,"('#macro NormalMatrix()')")
    write(dataunit,"('matrix < ',3(F10.6,','))") ON(1,1), ON(1,2), ON(1,3)
    write(dataunit,"(3(F10.6,','))") ON(2,1), ON(2,2), ON(2,3)
    write(dataunit,"(3(F10.6,','))") ON(3,1), ON(3,2), ON(3,3)
    write(dataunit,"('0, 0, 0  > ')")
    write(dataunit,"('#end')")
    close(unit=dataunit,status='keep')
end do
call Message(' --> All PoVRay frame parameter files created ')

! generate the main POVray scene file with the correct SubFolder variable
if (trim(onl%rendermode).eq.'spheres') then
    open(unit=dataunit,file='sphere-scene.pov',status='unknown',form='formatted')
else
    open(unit=dataunit,file='cube-scene.pov',status='unknown',form='formatted')
end if
write(dataunit,"('// POV-Ray 3.7 Scene File for visualization of grain boundary configurations')")
write(dataunit,"('// produced by EMsoft 4.0')")
write(dataunit,"('// email: degraef@cmu.edu')")
write(dataunit,"('// homepage: http://materials.cmu.edu/degraef')")
write(dataunit,"('//')")
write(dataunit,"('')")
write(dataunit,"('// load all the definitions')")
write(dataunit,"('#include ""octonionSLERP.inc""')")
write(dataunit,"('')")
write(dataunit,"('#declare SubFolder = ""',A,'/""')") trim(subfolder)
write(dataunit,"('')")
write(dataunit,"('#declare scl = 2.0;')")
write(dataunit,"('#declare he =  2.8;')")
write(dataunit,"('#declare FileName = concat(SubFolder,""/parameter"",str(clock,-3,0),"".pov"")')")
write(dataunit,"('#include FileName')")
write(dataunit,"('')")
write(dataunit,"('// the fixed grain A reference frame')")
write(dataunit,"('#declare scl = 3.0;')")
write(dataunit,"('#declare sclA = 3.0;')")
write(dataunit,"('#declare sclB = 2.0;')")

if (trim(onl%rendermode).eq.'spheres') then
    write(dataunit,"('// external coordinate axes (sample reference frame)')")
    write(dataunit,"('object{ AxisXYZ( scl, scl, scl, Texture_A_Red, Texture_A_Blue, Texture_A_Green) ')")
    write(dataunit,"('scale <0.5,0.5,0.5>  translate <-he, -he, he> }')")
    write(dataunit,"('')")
    write(dataunit,"('object{ AxisXYZgrainA( scl, scl, scl, Texture_A_Red, Texture_A_Blue,Texture_A_Green)  }')")
    write(dataunit,"('')")
    write(dataunit,"('// the rotatable grain B reference frame')")
    write(dataunit,"('#declare scl = 2.0;')")
    write(dataunit,"('object{ AxisXYZgrainB( 1.5*scl, 1.5*scl, 1.5*scl, Texture_A_Red, Texture_A_Blue, Texture_A_Green, scl)')") 
    write(dataunit,"('GrainMatrix() }')")
    write(dataunit,"('')")
    write(dataunit,"('// and the grain boundary plane normal')")
    write(dataunit,"('object { normals translate <0, R1, 0> NormalMatrix() }')")
else
    write(dataunit,"('#declare grainB = ')")
    write(dataunit,"('union {')")
    write(dataunit,"('difference {')")
    write(dataunit,"('object { AxisXYZcubeB( 1.5*sclB, 1.5*sclB, 1.5*sclB,Texture_A_Red,Texture_A_Blue,Texture_A_Green, sclB)')") 
    write(dataunit,"('GrainMatrix() }')")
    write(dataunit,"('object { TruncationB() NormalMatrix() }')")
    write(dataunit,"('}')")
    write(dataunit,"('object { normals NormalMatrix() }')")
    write(dataunit,"('} ')")
    write(dataunit,"('')")
    write(dataunit,"('#declare grainA = ')")
    write(dataunit,"('difference {')")
    write(dataunit,"('object { AxisXYZcubeA( sclA, sclA, sclA, Texture_A_Red, Texture_A_Blue,Texture_A_Green) }')")
    write(dataunit,"('object { TruncationA() NormalMatrix() }')")
    write(dataunit,"('}')")
    write(dataunit,"('')")
    write(dataunit,"('object { grainA }')")
    write(dataunit,"('object { grainB }')")
end if
close(unit=dataunit,status='keep')
call Message(' --> PoVRay scene file sphere-scene.pov created ')

! next we generate the PoVRay.ini file with the rendering instructions
open(unit=dataunit,file='povray.ini',status='unknown',form='formatted')
if (trim(onl%rendermode).eq.'spheres') then
    write(dataunit,"('Input_File_Name=sphere-scene.pov')")
else
    write(dataunit,"('Input_File_Name=cube-scene.pov')")
end if
write(dataunit,"('Output_File_Name=',A,'/frame')") trim(subfolder)
write(dataunit,"('+L/Users/mdg/Applications/PovrayCommandLineMacV2/include')")
if (onl%framesize.ge.1000) then
   write(dataunit,"('+W',I4,' +H',I4)") onl%framesize, onl%framesize
else if (onl%framesize.ge.100) then 
   write(dataunit,"('+W',I3,' +H',I3)") onl%framesize, onl%framesize
else 
   write(dataunit,"('+W',I2,' +H',I2)") onl%framesize, onl%framesize
end if
write(dataunit,"('Initial_Clock=1')")
write(dataunit,"('Initial_Frame=1')")
if (numf.gt.1000) then 
    write(dataunit,"('Final_Clock=',I4)") numf+1
    write(dataunit,"('Final_Frame=',I4)") numf+1
else if (numf.gt.100) then
    write(dataunit,"('Final_Clock=',I3)") numf+1
    write(dataunit,"('Final_Frame=',I3)") numf+1
else if (numf.gt.10) then
    write(dataunit,"('Final_Clock=',I2)") numf+1
    write(dataunit,"('Final_Frame=',I2)") numf+1
else 
    write(dataunit,"('Final_Clock=',I1)") numf+1
    write(dataunit,"('Final_Frame=',I1)") numf+1
end if
write(dataunit,"('Work_Threads=6')")
close(unit=dataunit,status='keep')
call Message(' --> povray.ini file created ')


! finally, execute the rendering programs if they can be found; 
! otherwise, print out some suggestions on how to proceed with the scene files
dirstring = EMsoft_getUserHomePath()
pvcmd = trim(dirstring)//'/Applications/PovrayCommandLineMacV2/Povray37UnofficialMacCmd'
inquire(file=trim(pvcmd),exist=fexists)
frames_generated = .FALSE.

if (fexists.eqv..TRUE.) then
    call Message('')
    call Message('Found PovRay command line executable; rendering frames in silent mode (may take a while)')
    call Message('Executing '//trim(pvcmd)//' povray >/dev/null 2>/dev/null')
    call system(trim(pvcmd)//' povray >/dev/null 2>/dev/null')
    frames_generated = .TRUE.
else
    call Message(' =============================== ')
    call Message('')
    call Message('To render the individual frames, please run the command line PoVRay executable')
    call Message('on the povray.ini file.  On the Mac platform, this can be accomplished as follows:')
    call Message('')
    call Message('shell> ...path.../PovrayCommandLineMacV2/Povray37UnofficialMacCmd povray >/dev/null 2>/dev/null')
    call Message('')
    call Message('Make sure that the second line of the povray.ini file has the correct path to the include folder.')
    call Message('')
    if (trim(onl%rendermode).eq.'spheres') then
        call Message('You can also simply start the interactive PoVRay program and render the sphere-scene.pov file.')
    else
        call Message('You can also simply start the interactive PoVRay program and render the cube-scene.pov file.')
    end if
call Message('')
end if

! if the frames have been generated, then look for the ffmpeg program; if found, try to execute it, otherwise print
! an explanation on what to do next 
pvcmd = trim(dirstring)//'/packages/ffmpeg'
inquire(file=trim(pvcmd),exist=fexists)

if ((frames_generated.eqv..FALSE.).OR.(fexists.eqv..FALSE.)) then
    call Message(' =============================== ')
    call Message('')
    call Message('The individual frames can be turned into a movie in a number of different ways;  if you have the ')
    call Message('ffmpeg program installed, you can run it as follows to generate an mp4 file:')
    call Message('')
    if (numf.gt.1000) then 
     call Message('shell> ...path.../ffmpeg -i frames/frame%04d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename))
    else if (numf.gt.100) then  
     call Message('shell> ...path.../ffmpeg -i frames/frame%03d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename))
    else if (numf.gt.10) then  
     call Message('shell> ...path.../ffmpeg -i frames/frame%02d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename))
    else 
     call Message('shell> ...path.../ffmpeg -i frames/frame%01d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename))
    end if
    call Message('')
    call Message('This should generate an mp4 file in your current folder.')
    call Message('')
else if (fexists.eqv..TRUE.) then 
    if (numf.gt.1000) then 
     pvcmd = trim(pvcmd)//' -i frames/frame%04d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename)
    else if (numf.gt.100) then  
     pvcmd = trim(pvcmd)//' -i frames/frame%03d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename)
    else if (numf.gt.10) then  
     pvcmd = trim(pvcmd)//' -i frames/frame%02d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename)
    else 
     pvcmd = trim(pvcmd)//' -i frames/frame%01d.png -y -c:v libx264 -pix_fmt yuv420p '//trim(onl%moviename)
    end if
    call Message('')
    call Message('Found ffmpeg executable; silently converting frames into mp4 file...')
    call Message('Executing '//trim(pvcmd)//' >/dev/null 2>/dev/null')
    call system(trim(pvcmd)//' >/dev/null 2>/dev/null')
end if

end subroutine oSLERPrender
