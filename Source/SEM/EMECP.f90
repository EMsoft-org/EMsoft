! ###################################################################
! Copyright (c) 2013-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMECP.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMECP
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Electron channeling patterns from master pattern
!
!> @date 08/26/14 SS 1.0 f90
!> @date 13/10/15 SS 2.0 added detector model+new GetVectorCone routine+OpenMP
!> @date 05/21/16 MDG 2.1 changes for new HDF internal file organization
!--------------------------------------------------------------------------

program EMECP

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(ECPNameListType)                   :: ecpnl

nmldeffile = 'EMECP.nml'
progname = 'EMECP.f90'
progdesc = 'Electron channeling patterns from master pattern'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 40 /), progname)

! deal with the namelist stuff
call GetECPNameList(nmldeffile,ecpnl)

! perform the zone axis computations
call ECpattern(ecpnl, progname, nmldeffile)

end program EMECP

!-----------------------------------------------------------------------------------
!
! SUBROUTINE: ECpattern
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Electron channeling patterns from master pattern
!
!> @date 08/27/14 SS 1.0 f90
!> @date 13/10/15 SS 2.0 added detector model+new GetVectorCone routine+OpenMP+hdf5
!> @date 09/13/18 MDG 2.1 fix off-by-one error in writing of pattern byte arrays (fixes issue 26)
!> @date 02/19/19 MDG 2.2 updates HDF_FileVersion to 4.1
!> @date 09/18/19 MDG 2.3 removes unnecessary copying of large master pattern arrays
!-------------------------------------------------------------------------------------
subroutine ECpattern(ecpnl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use crystal
use constants
use symmetry
use Lambert
use initializers
use constants
use error
use io
use files
use ECPmod
use rotations
use quaternions
use HDF5
use HDFsupport
use NameListHDFwriters
use ISO_C_BINDING
use omp_lib
use distortion
use filters 
use stringconstants

IMPLICIT NONE

type(ECPNameListType),INTENT(INOUT)     :: ecpnl
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile

type(ECPLargeAccumType),pointer         :: acc
type(ECPMasterType),pointer             :: master
type(IncidentListECP),pointer           :: khead, ktmp
type(ECPAngleType),pointer              :: angles
real(kind=dbl),allocatable              :: klist(:,:)

integer(kind=irg)                       :: npx,npy,numset,istat,val
integer(kind=irg),allocatable           :: ATOM_type(:)
real(kind=dbl)                          :: EkeV
real(kind=sgl)                          :: dmin, FN(3)
real(kind=sgl),allocatable              :: mask(:,:), lx(:), ly(:)
integer(kind=irg)                       :: maskradius, io_int(1), hdferr
logical                                 :: verbose
real(kind=sgl),allocatable              :: anglewf(:)
integer(kind=irg)                       :: nsig, isig, isigp
integer(kind=irg)                       :: numk, nix, niy, nixp, niyp, i, j, ierr, &
                                           ipx, ipy, iang, idir
real(kind=dbl)                          :: scl, x, dx, dy, dxm, dym, wf
real(kind=dbl)                          :: dc(3), ixy(2), qu(4)
integer(kind=irg),allocatable           :: kij(:,:)
real(kind=sgl),allocatable              :: ECPpattern(:,:), ECPpatternintd(:,:)
integer(kind=irg),allocatable           :: ECPpatterninteger(:,:), ECPpatternad(:,:)
real(kind=sgl)                          :: time_start, time_end, ma, mi
character(len=1),allocatable            :: bpat(:,:)
integer(kind=irg)                       :: TID, nthreads
real(kind=dbl)                          :: dp, MCangle
real(kind=dbl),parameter                :: Rtod = 57.2957795131D0
real(kind=dbl),parameter                :: dtoR = 0.01745329251D0
logical                                 :: switchwfoff = .FALSE.
!complex(kind=dbl)                       :: D

type(HDFobjectStackType)                :: HDF_head
integer(HSIZE_T), dimension(1:3)        :: hdims, offset 
integer(HSIZE_T)                        :: dims3(3)
character(fnlen,kind=c_char)            :: line2(1)
character(fnlen)                        :: groupname, dataset, attributename, HDF_FileVersion
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
character(fnlen)                        :: datafile
logical                                 :: overwrite = .TRUE., insert = .TRUE.

!=================================================================
! read Monte Carlo output file and extract necessary parameters
! first, we need to load the data from the output of EMMCOpenCL
! used in the bse1 mode
!=================================================================
! open the fortran HDF interface
call h5open_EMsoft(hdferr)

call Message(' -> opening '//trim(ecpnl%energyfile), frm = "(A)" )
call  ECPreadMCfile(ecpnl, acc, verbose=.TRUE.)

!=================================================================
! read Master pattern output file and extract necessary parameters
! first, we need to load the data from the ECP master program
!=================================================================

call Message(' -> opening '//trim(ecpnl%masterfile), frm = "(A)" )
call ECPreadMasterfile(ecpnl, master, verbose=.TRUE.)

!=================================================================
! reading the angle file as euler angles or quaternions
!=================================================================

call Message(' -> opening '//trim(ecpnl%anglefile), frm = "(A)" )
call ECPreadangles(ecpnl, angles, verbose=.TRUE.)

!================================================================================
! generating the detector arrays; should be always called after ECPreadMasterfile
!================================================================================

call Message(' -> Generating detector arrays', frm = "(A)" )
call ECPGenerateDetector(ecpnl, master, verbose=.TRUE.)

!================================================================================
! get the weight factors here
!================================================================================

nsig = nint((ecpnl%thetac) + abs(ecpnl%sampletilt)) + 1
allocate(anglewf(1:nsig),stat=istat)

call Message(' -> Calculating weight factors', frm = "(A)" )
call ECPGetWeightFactors(ecpnl, master, acc, anglewf, nsig, verbose=.TRUE.)

!=================================================================
! check if there are enough angles in MC for detector geometry
!=================================================================
if (ecpnl%MCsigend .lt. abs(ecpnl%sampletilt) + ecpnl%thetac) then
        call Message('Not enough angles in Monte carlo file...interpolation will be done without &
        appropriate weight factors',frm = "(A)")
        switchwfoff = .TRUE.
end if

if ((-ecpnl%MCsigend .gt. ecpnl%thetac - abs(ecpnl%sampletilt)) .and. (switchwfoff .eqv. .FALSE.)) then
    call Message('Not enough angles in Monte carlo file...interpolation will be done without &
    appropriate weight factors',frm = "(A)")
    switchwfoff = .TRUE.
end if


!=================================================================
! completed reading the file; generating list of incident vectors
!=================================================================

numk = 0
call GetVectorsCone(ecpnl, khead, numk)
allocate(kij(2,numk),klist(3,numk),stat=istat)

io_int(1) = numk
call WriteValue('Number of beams for which interpolation will be done = ',io_int,1) 
!=============================================================================================
! ------ and open the output file for IDL visualization (only thread 0 can write to this file)
!=============================================================================================
! we need to write the image dimensions, and also how many of those there are...

call timestamp(datestring=dstr, timestring=tstrb)
tstre = tstrb
call CPU_TIME(time_start)

! Create a new file using the default properties.
datafile = trim(EMsoft_getEMdatapathname())//trim(ecpnl%datafile)
datafile = EMsoft_toNativePath(datafile)
hdferr =  HDF_createFile(datafile, HDF_head)

! write the EMheader to the file
groupname = SC_ECP
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EMECPNML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

call HDF_pop(HDF_head)

! create a NMLparameters group to write all the namelist entries into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)

call HDFwriteECPNameList(HDF_head, ecpnl, .FALSE.)

! and leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

! create the ECP group and add a HDF_FileVersion attribbute to it 
groupname = SC_ECP
  hdferr = HDF_createGroup(groupname, HDF_head)
! before Feb. 19, 2019, an undetected error caused all patterns to be upside down in the Kikuchi bands only,
! not in the background intensity profile.  This was compensated by a pattern flip of all experimental 
! patterns in the dictionary indexing program, but when taking individual patterns from this program, they
! are actually upside down in all versions through HDF_FileVersion 4.0.  As of 4.1, the patterns are in the
! correct orientation.  This was detected by manually indexing a simulated pattern.
  HDF_FileVersion = '4.1'
  attributename = SC_HDFFileVersion
  hdferr = HDF_addStringAttributeToGroup(attributename, HDF_FileVersion, HDF_head)

! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0 and above
! =====================================================
! we need to write the image dimensions
dataset = SC_npix
hdferr = HDF_writeDatasetInteger(dataset, ecpnl%npix, HDF_head) 

dataset = SC_numangledictionary
hdferr = HDF_writeDatasetInteger(dataset, ecpnl%numangle_anglefile, HDF_head) 
! =====================================================
! end of HDF_FileVersion = 4.0 and above write statements
! =====================================================


! and we leave this group open for further data output ... 

ktmp => khead
! converting to array for OpenMP parallelization
do i = 1,numk
   klist(1:3,i) = ktmp%k(1:3)
   kij(1:2,i) = (/ktmp%i,ktmp%j/)
   ktmp => ktmp%next
end do

allocate(mask(1:ecpnl%npix, 1:ecpnl%npix),stat=istat)
if (istat .ne. 0) then
   call FatalError('ECpattern','could not allocate mask array')
end if

mask = 1.0
if (ecpnl%maskpattern .eq. 'y') then

! create the circular mask
  maskradius = (float(ecpnl%npix)/2.0)**2
  allocate(lx(1:ecpnl%npix), ly(1:ecpnl%npix), stat=istat)
  lx = (/ (float(i),i=1,ecpnl%npix) /) - float(ecpnl%npix+1)/2.0
  ly = (/ (float(i),i=1,ecpnl%npix) /) - float(ecpnl%npix+1)/2.0
  do i= 1,ecpnl%npix
    do j= 1,ecpnl%npix
      if ((lx(i)**2+ly(j)**2).gt.maskradius) mask(i,j) = 0.0
    end do
  end do
  deallocate(lx, ly)
end if

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
! This has been changed on 09/01/15 to accommodate the new Lambert module]

!scl = float(npx)/LPs%sPio2
scl = dble(ecpnl%npx)

allocate(ECPpattern(1:ecpnl%npix, 1:ecpnl%npix),&
ECPpatternintd(1:ecpnl%npix,1:ecpnl%npix),&
ECPpatterninteger(1:ecpnl%npix,1:ecpnl%npix),&
ECPpatternad(1:ecpnl%npix,1:ecpnl%npix),stat=istat)

ECPpattern = 0.0
ECPpatternintd = 0.0
ECPpatterninteger = 0
ECPpatternad = 0

dataset = SC_ECpatterns

if (ecpnl%outputformat .eq. 'bin') then
    allocate(bpat(1:ecpnl%npix,1:ecpnl%npix),stat=istat)
    if (istat .ne. 0) call FatalError('ECpatter','cannot allocate bpat array')
    bpat = char(nint(255.0*ECPpattern))

! write dictionary pattern to h5 file
    offset = (/ 0, 0, 0 /)
    hdims = (/ ecpnl%npix, ecpnl%npix, ecpnl%numangle_anglefile /)
    dims3 = (/ ecpnl%npix, ecpnl%npix, 1 /)
    hdferr = HDF_writeHyperslabCharArray3D(dataset, bpat, hdims, offset, dims3, HDF_head)
end if

if (ecpnl%outputformat .eq. 'gui') then
    offset = (/ 0, 0, 0 /)
    hdims = (/ ecpnl%npix, ecpnl%npix, ecpnl%numangle_anglefile /)
    dims3 = (/ ecpnl%npix, ecpnl%npix, 1 /)
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, ECPpattern, hdims, offset, dims3, HDF_head)
end if

!D = cmplx(0.000001,-0.0000014)

! set the number of OpenMP threads
io_int(1) = ecpnl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int,1,"(I4)")
call OMP_SET_NUM_THREADS(ecpnl%nthreads)

! use OpenMP to run on multiple cores
!$OMP PARALLEL DEFAULT(SHARED) &
!$OMP PRIVATE(TID,nthreads,dc,ixy,istat,nix,niy,nixp,niyp,dx,dy,dxm,dym,MCangle,isig,dp,isigp) &
!$OMP& PRIVATE(ipx,ipy,ECPpattern,bpat,ECPpatternintd,ma,mi,offset,hdims,dims3,hdferr,qu,idir,wf)

TID = OMP_GET_THREAD_NUM()
nthreads = OMP_GET_NUM_THREADS()

!$OMP DO SCHEDULE(DYNAMIC)
angleloop: do iang = 1,ecpnl%numangle_anglefile
    qu(1:4) = angles%quatang(1:4,iang)

    imageloop: do idir = 1,numk

! do the active coordinate transformation for this euler angle

        dc = klist(1:3,idir)
        dp = DOT_PRODUCT(dc(1:3),(/dsin(ecpnl%sampletilt*dtoR),0.D0,dcos(ecpnl%sampletilt*dtoR)/))        
      
        MCangle = acos(dp)*Rtod
! find index closest to the list of MC runs we already have and interpolate the weight factor
        isig = int(MCangle) + 1
        if (isig .gt. nsig) isig = nsig

        isigp = isig + 1
        if (isigp .gt. nsig) isigp = nsig

        dx = MCangle - int(MCangle)
        dxm =  1.0 - dx
 
        wf = anglewf(isig) * dxm + anglewf(isigp) * dx
        
        dc = quat_LP(qu,dc)
        dc = dc/dsqrt(sum(dc*dc))

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, ecpnl%npx, ecpnl%npy, nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
        ipx = kij(1,idir)
        ipy = kij(2,idir)
        
! including the detector model with some sample tilt
        if (switchwfoff .eqv. .FALSE.) then
            if (dc(3) .ge. 0.0) then 

                ECPpattern(ipx,ipy) = wf * ( master%mLPNH(nix,niy) * dxm * dym + &
                             master%mLPNH(nixp,niy) * dx * dym + &
                             master%mLPNH(nix,niyp) * dxm * dy + &
                             master%mLPNH(nixp,niyp) * dx * dy )

            else

                 ECPpattern(ipx,ipy) =  wf * ( master%mLPSH(nix,niy) * dxm * dym + &
                             master%mLPSH(nixp,niy) * dx * dym + &
                             master%mLPSH(nix,niyp) * dxm * dy + &
                             master%mLPSH(nixp,niyp) * dx * dy )

            end if

        else
            if (dc(3) .ge. 0.0) then 
                ECPpattern(ipx,ipy) =   master%mLPNH(nix,niy) * dxm * dym + &
                         master%mLPNH(nixp,niy) * dx * dym + &
                         master%mLPNH(nix,niyp) * dxm * dy + &
                         master%mLPNH(nixp,niyp) * dx * dy 
            else
                 ECPpattern(ipx,ipy) =   master%mLPSH(nix,niy) * dxm * dym + &
                         master%mLPSH(nixp,niy) * dx * dym + &
                         master%mLPSH(nix,niyp) * dxm * dy + &
                         master%mLPSH(nixp,niyp) * dx * dy 

            end if
 
        end if

    end do imageloop

! added by MDG, 06/16/20
    ECPpattern = ECPpattern**ecpnl%gammavalue
    
    !call BarrelDistortion(D,ECPpattern,ecpnl%npix,ecpnl%npix)
    !ma = maxval(ECPpattern)
    !mi = minval(ECPpattern)
    !ECPpatternintd = ((ECPpattern - mi)/ (ma-mi))
    !ECPpatterninteger = nint(ECPpatternintd*255.0)
    !ECPpatternad =  adhisteq(10,ecpnl%npix,ecpnl%npix,ECPpatterninteger)
    !ECPpattern = float(ECPpatternad)
! =====================================================
! The following write commands constitute HDF_FileVersion = 4.0
! =====================================================

    if (mod(iang,2500) .eq. 0) then
        io_int(1) = iang
        call WriteValue(' completed pattern # ',io_int,1)
    end if
    
!$OMP CRITICAL
    if (ecpnl%outputformat .eq. 'bin') then
        ma = maxval(ECPpattern)
        mi = minval(ECPpattern)
        ECPpatternintd = ((ECPpattern - mi)/ (ma-mi))
        if (ecpnl%maskpattern.eq.'y')  ECPpatternintd = ECPpatternintd * mask 
        bpat = char(nint(255.0*ECPpatternintd))

! write dictionary pattern to h5 file
        offset = (/ 0, 0, iang-1 /)
        hdims = (/ ecpnl%npix, ecpnl%npix, ecpnl%numangle_anglefile /)
        dims3 = (/ ecpnl%npix, ecpnl%npix, 1 /)
        hdferr = HDF_writeHyperslabCharArray3D(dataset, bpat, hdims, offset, dims3, HDF_head, insert)
 
    end if

    if (ecpnl%outputformat .eq. 'gui') then
          if (ecpnl%maskpattern.eq.'y')  ECPpattern = ECPpattern * mask
          offset = (/ 0, 0, iang-1 /)
          hdims = (/ ecpnl%npix, ecpnl%npix, ecpnl%numangle_anglefile /)
          dims3 = (/ ecpnl%npix, ecpnl%npix, 1 /)
          hdferr = HDF_writeHyperslabFloatArray3D(dataset, ECPpattern, hdims, offset, dims3, HDF_head, insert)
! =====================================================
! end of HDF_FileVersion = 4.0 write statements
! =====================================================

    end if
!$OMP END CRITICAL

end do angleloop 
!$OMP END DO
!$OMP END PARALLEL

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! and update the end time
call timestamp(datestring=dstr, timestring=tstre)
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_ECP
hdferr = HDF_openGroup(groupname, HDF_head)

! stop time /EMheader/StopTime 'character'
dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

call CPU_TIME(time_end)
dataset = SC_Duration
time_end = time_end - time_start
hdferr = HDF_writeDatasetFloat(dataset, time_end, HDF_head)

! close all groups and the file
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! close the Fortran interface
call h5close_EMsoft(hdferr)

call Message(' -> Execution done...quitting now',frm='(A)')

end subroutine ECpattern
