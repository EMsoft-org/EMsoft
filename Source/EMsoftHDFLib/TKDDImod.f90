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
! EMsoft:TKDDImod.f90
!--------------------------------------------------------------------------
!
! MODULE: TKDDImod
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMTKD dictionary indexing helper routines
!
!> @date  06/24/14  MDG 1.0 original, lifted from EMEBSD.f90 to simplify code
!> @date  09/01/15  MDG 1.1 modified EBSDMasterDIType definition to accommodate multiple Lambert maps
!> @date  09/15/15  SS  1.2 added accum_z to EBSDLargeAccumDIType
!> @date  05/07/17  MDG 1.3 added support for TKD indexing routines
!--------------------------------------------------------------------------
module TKDDImod

use local
use typedefs

IMPLICIT NONE

type TKDAngleDIType
        real(kind=sgl),allocatable      :: quatang(:,:)
end type TKDAngleDIType

type TKDLargeAccumDIType
        integer(kind=irg),allocatable   :: accum_e(:,:,:),accum_z(:,:,:,:)
        real(kind=sgl),allocatable      :: accum_e_detector(:,:,:)
end type TKDLargeAccumDIType

type TKDMasterDIType
        real(kind=sgl),allocatable      :: mLPNH(:,:,:) , mLPSH(:,:,:)
        real(kind=sgl),allocatable      :: rgx(:,:), rgy(:,:), rgz(:,:)          ! auxiliary detector arrays needed for interpolation
end type TKDMasterDIType

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDIndexingreadMasterfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read TKD master pattern from file
!
!> @param enl TKD name list structure
!> @param 
!
!> @date 05/07/17  MDG 1.0 original based on EBSD version
!--------------------------------------------------------------------------
recursive subroutine TKDIndexingreadMasterfile(enl, master, mfile, verbose, NoHDFInterfaceOpen)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDIndexingreadMasterfile

use local
use typedefs
use NameListTypedefs
use files
use io
use error
use HDF5
use HDFsupport
use stringconstants


IMPLICIT NONE

type(TKDIndexingNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDMasterDIType),pointer                   :: master
character(fnlen),INTENT(IN),OPTIONAL            :: mfile
logical,INTENT(IN),OPTIONAL                     :: verbose
logical,OPTIONAL,INTENT(IN)                     :: NoHDFInterfaceOpen

real(kind=sgl),allocatable                      :: mLPNH(:,:,:) 
real(kind=sgl),allocatable                      :: mLPSH(:,:,:) 
real(kind=sgl),allocatable                      :: EkeVs(:) 
integer(kind=irg),allocatable                   :: atomtype(:)

real(kind=sgl),allocatable                      :: srtmp(:,:,:,:)
integer(kind=irg)                               :: istat

logical                                         :: stat, readonly, HDFopen 
integer(kind=irg)                               :: hdferr, nlines
integer(HSIZE_T)                                :: dims(1), dims4(4)
character(fnlen)                                :: groupname, dataset, masterfile
character(fnlen),allocatable                    :: stringarray(:)

type(HDFobjectStackType)                        :: HDF_head

HDFopen = .TRUE.
if (present(NoHDFInterfaceOpen)) then
  if (NoHDFInterfaceOpen.eqv..FALSE.) HDFopen = .FALSE.
end if 

! open the fortran HDF interface
if (HDFopen.eqv..TRUE.) call h5open_EMsoft(hdferr)

nullify(HDF_head%next)

! is the mfile parameter present? If so, use it as the filename, otherwise use the enl%masterfile parameter
if (PRESENT(mfile)) then
  masterfile = mfile
else
  masterfile = trim(EMsoft_getEMdatapathname())//trim(enl%masterfile)
end if
masterfile = EMsoft_toNativePath(masterfile)

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(masterfile), stat, hdferr)

if (stat) then 
! open the master file 
  readonly = .TRUE.
  hdferr =  HDF_openFile(masterfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDMasterNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_energyfile
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterenergyfile = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_npx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%npx)
  enl%npy = enl%npx

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nE)
! make sure that MC and Master results are compatible
  if ((enl%numEbins.ne.enl%nE).and.(.not.PRESENT(mfile))) then
    call Message('Energy histogram and Lambert stack have different energy dimension; aborting program', frm = "(A)")
    call HDF_pop(HDF_head,.TRUE.)
    stop
  end if

dataset = SC_numset
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numset)

! dataset = 'squhex'
! call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
! enl%sqorhe = trim(stringarray(1))
! deallocate(stringarray)

dataset = SC_mLPNH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPNH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPNH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_mLPSH
  call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, srtmp)
  allocate(master%mLPSH(-enl%npx:enl%npx,-enl%npy:enl%npy,enl%nE),stat=istat)
  master%mLPSH = sum(srtmp,4)
  deallocate(srtmp)

dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterxtalname = trim(stringarray(1))
  deallocate(stringarray)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_TKDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_ProgramName
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterprogname = trim(stringarray(1))
  deallocate(stringarray)
  
dataset = SC_Version
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%Masterscversion = trim(stringarray(1))
  deallocate(stringarray)
  
  call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
if (HDFopen.eqv..TRUE.)  call h5close_EMsoft(hdferr)

else
  masterfile = 'File '//trim(masterfile)//' is not an HDF5 file'
  call FatalError('TKDIndexingreadMasterfile',masterfile)
end if
!====================================

if (present(verbose)) call Message(' -> completed reading dynamical scattering data from &
'//trim(enl%masterfile), frm = "(A)")

end subroutine TKDIndexingreadMasterfile

!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDIndexingGenerateDetector
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate the detector arrays
!
!> @param enl TKD name list structure
!
!> @date 06/24/14  MDG 1.0 original
!> @date 07/01/15   SS  1.1 added omega as the second tilt angle
!> @date 07/07/15   SS  1.2 correction to the omega tilt parameter; old version in the comments
!> @date 01/26/16   SS  1.3 adjusted for EBSDIndexing
!> @date 06/12/16  MDG  1.4 added correction for effetive detector pixel size w.r.t. equal area mapping
!> @date 05/07/17  MDG  2.0 forked from EBSD version for TKD indexing program
!--------------------------------------------------------------------------
recursive subroutine TKDIndexingGenerateDetector(enl, acc, master, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDIndexingGenerateDetector

use local
use typedefs
use NameListTypedefs
use files
use constants
use io
use Lambert
use stringconstants

IMPLICIT NONE

type(TKDIndexingNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDLargeAccumDIType),pointer               :: acc
type(TKDMasterDIType),pointer                   :: master
logical,INTENT(IN),OPTIONAL                     :: verbose

real(kind=sgl),allocatable                      :: scin_x(:), scin_y(:)                 ! scintillator coordinate ararays [microns]
real(kind=sgl),parameter                        :: dtor = 0.0174533  ! convert from degrees to radians
real(kind=sgl)                                  :: alp, ca, sa, cw, sw
real(kind=sgl)                                  :: L2, Ls, Lc     ! distances
real(kind=sgl),allocatable                      :: z(:,:)           
integer(kind=irg)                               :: nix, niy, binx, biny , i, j, Emin, Emax, istat, k, ipx, ipy, nixp, niyp, elp      ! various parameters
real(kind=sgl)                                  :: dc(3), scl, pcvec(3), alpha, theta, gam, dp           ! direction cosine array
real(kind=sgl)                                  :: sx, dx, dxm, dy, dym, rhos, x, bindx         ! various parameters
real(kind=sgl)                                  :: ixy(2)


!====================================
! ------ generate the detector arrays
!====================================
! This needs to be done only once for a given detector geometry
allocate(scin_x(enl%numsx),scin_y(enl%numsy),stat=istat)
! if (istat.ne.0) then ...
scin_x = - ( enl%xpc - ( 1.0 - enl%numsx ) * 0.5 - (/ (i-1, i=1,enl%numsx) /) ) * enl%delta
scin_y = ( enl%ypc - ( 1.0 - enl%numsy ) * 0.5 - (/ (i-1, i=1,enl%numsy) /) ) * enl%delta

! auxiliary angle to rotate between reference frames
alp = 0.5 * cPi - (enl%MCsig - enl%thetac) * dtor
ca = cos(alp)
sa = sin(alp)

cw = cos(enl%omega * dtor)
sw = sin(enl%omega * dtor)

! we will need to incorporate a series of possible distortions 
! here as well, as described in Gert nolze's paper; for now we 
! just leave this place holder comment instead

! compute auxilliary interpolation arrays
! if (istat.ne.0) then ...
elp = enl%numsy + 1
L2 = enl%L * enl%L
do j=1,enl%numsx
  sx = L2 + scin_x(j) * scin_x(j)
  Ls = -sw * scin_x(j) + enl%L*cw
  Lc = cw * scin_x(j) + enl%L*sw
  do i=1,enl%numsy
   rhos = 1.0/sqrt(sx + scin_y(i)**2)
   master%rgx(j,elp-i) = (scin_y(i) * ca + sa * Ls) * rhos!Ls * rhos
   master%rgy(j,elp-i) = Lc * rhos!(scin_x(i) * cw + Lc * sw) * rhos
   master%rgz(j,elp-i) = (-sa * scin_y(i) + ca * Ls) * rhos!(-sw * scin_x(i) + Lc * cw) * rhos
  end do
end do
deallocate(scin_x, scin_y)

! normalize the direction cosines.
allocate(z(enl%numsx,enl%numsy))
  z = 1.0/sqrt(master%rgx*master%rgx+master%rgy*master%rgy+master%rgz*master%rgz)
  master%rgx = master%rgx*z
  master%rgy = master%rgy*z
  master%rgz = master%rgz*z
deallocate(z)
!====================================

!====================================
! ------ create the equivalent detector energy array
!====================================
! from the Monte Carlo energy data, we need to extract the relevant
! entries for the detector geometry defined above.  Once that is 
! done, we can get rid of the larger energy array
!
! in the old version, we either computed the background model here, or 
! we would load a background pattern from file.  In this version, we are
! using the background that was computed by the MC program, and has 
! an energy histogram embedded in it, so we need to interpolate this 
! histogram to the pixels of the scintillator.  In other words, we need
! to initialize a new accum_e array for the detector by interpolating
! from the Lambert projection of the MC results.
!

! determine the scale factor for the Lambert interpolation; the square has
! an edge length of 2 x sqrt(pi/2)
  scl = float(enl%nsx) !  / LPs%sPio2  [removed on 09/01/15 by MDG for new Lambert routines]

! get the indices of the minimum and maximum energy
  Emin = nint((enl%energymin - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emin.lt.1)  Emin=1
  if (Emin.gt.enl%numEbins)  Emin=enl%numEbins

  Emax = nint((enl%energymax - enl%Ehistmin)/enl%Ebinsize) +1
  if (Emax.lt.1)  Emax=1
  if (Emax.gt.enl%numEbins)  Emax=enl%numEbins

! get an estimate of the cone opening angle for which the projected area at the pattern
! center is the same as delta**2
  alpha = atan(enl%delta/enl%L/sqrt(sngl(cPi)))

! then get the direction cosines for the pattern center, keeping in mind that for TKD, 
! the pattern center need not lie on the detector

! this needs to be verified since we have flipped the pattern upside down ... [MDG, 02/19/2019]
  ipx = enl%numsx/2+nint(enl%xpc)
  ipy = enl%numsy/2+nint(enl%ypc)
  if (abs(ipy).gt.enl%numsy) then 
    pcvec = (/enl%ypc*enl%delta*ca + enl%xpc*enl%delta*sa*sw + enl%L*cw*sa, &
             enl%L*sw - enl%xpc*enl%delta*cw,&
             enl%L*ca*cw + enl%xpc*enl%delta*ca*sw - enl%ypc*enl%delta*sa/)
    pcvec = pcvec/NORM2(pcvec)
  else
    pcvec = (/ master%rgx(ipx,ipy), master%rgy(ipx,ipy), master%rgz(ipx,ipy) /)
  end if

  do i=1,enl%numsx
    do j=1,enl%numsy
! do the coordinate transformation for this detector pixel
       dc = (/ master%rgx(i,j),master%rgy(i,j),master%rgz(i,j) /)

! make sure the third one is positive; if not, switch all 
       if (dc(3).lt.0.0) dc = -dc

! convert these direction cosines to coordinates in the Rosca-Lambert projection
        call LambertgetInterpolation(dc, scl, enl%nsx, enl%nsy, nix, niy, nixp, niyp, dx, dy, dxm, dym, swap=.TRUE.)

! do the area correction for this detector pixel
        dp = dot_product(pcvec,dc)
        theta = acos(dp)
        if ((i.eq.ipx).and.(j.eq.ipy)) then
          gam = 0.25 
        else
          gam = 2.0 * tan(alpha) * dp / ( tan(theta+alpha) - tan(theta-alpha) ) * 0.25
        end if
! interpolate the intensity 
        do k=Emin,Emax 
          acc%accum_e_detector(k,i,elp-j) = gam * ( acc%accum_e(k,nix,niy) * dxm * dym + &
                                                    acc%accum_e(k,nixp,niy) * dx * dym + &
                                                    acc%accum_e(k,nix,niyp) * dxm * dy + &
                                                    acc%accum_e(k,nixp,niyp) * dx * dy )
        end do
    end do
  end do 

! and finally, get rid of the original accum_e array which is no longer needed
! [we'll do that in the calling program ]
!  deallocate(accum_e)

!====================================
end subroutine TKDIndexingGenerateDetector

!--------------------------------------------------------------------------
!
! SUBROUTINE:TKDIndexingreadMCfile
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read data from TKD Monte Carlo file
!
!> @param enl TKD name list structure
!> @param acc energy structure
!
!> @date 05/07/17  MDG 1.0 original based on similar EBSD routine
!--------------------------------------------------------------------------
recursive subroutine TKDIndexingreadMCfile(enl,acc,efile,verbose,NoHDFInterfaceOpen)
!DEC$ ATTRIBUTES DLLEXPORT :: TKDIndexingreadMCfile

use local
use typedefs
use NameListTypedefs
use files
use io
use HDF5
use HDFsupport
use error
use stringconstants

IMPLICIT NONE

type(TKDIndexingNameListType),INTENT(INOUT)     :: enl
!f2py intent(in,out) ::  enl
type(TKDLargeAccumDIType),pointer               :: acc
character(fnlen),INTENT(IN),OPTIONAL            :: efile
logical,INTENT(IN),OPTIONAL                     :: verbose
logical,INTENT(IN),OPTIONAL                     :: NoHDFInterfaceOpen

integer(kind=irg)                               :: istat, hdferr, nlines, nx
logical                                         :: stat, readonly, HDFopen
integer(HSIZE_T)                                :: dims3(3),dims4(4)
character(fnlen)                                :: groupname, dataset, energyfile 
character(fnlen),allocatable                    :: stringarray(:)

integer(kind=irg),allocatable                   :: acc_e(:,:,:),acc_z(:,:,:,:)

type(HDFobjectStackType)                        :: HDF_head


! is the efile parameter present? If so, use it as the filename, otherwise use the enl%energyfile parameter
if (PRESENT(efile)) then
  energyfile = efile
else
  energyfile = trim(EMsoft_getEMdatapathname())//trim(enl%energyfile)
end if
energyfile = EMsoft_toNativePath(energyfile)

HDFopen = .TRUE.
if (present(NoHDFInterfaceOpen)) then
  if (NoHDFInterfaceOpen.eqv..FALSE.) HDFopen = .FALSE.
end if 

allocate(acc)

! first, we need to check whether or not the input file is of the HDF5 format type; if
! it is, we read it accordingly, otherwise we use the old binary format.
!
call h5fis_hdf5_f(energyfile, stat, hdferr)

if (stat) then
! open the fortran HDF interface
  if (HDFopen.eqv..TRUE.) call h5open_EMsoft(hdferr)

  nullify(HDF_head%next)

! open the MC file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(energyfile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLfoilNameList
  hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_xtalname
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCxtalname = trim(stringarray(1))
  deallocate(stringarray)

! dataset = 'mode'
! call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
! enl%MCmode = trim(stringarray(1))
! deallocate(stringarray)
  enl%MCmode = 'full'
  if (enl%MCmode .ne. 'full') then
      call FatalError('TKDIndexingreadMCfile','This file is not in full mode. Please input correct HDF5 file')
  end if
dataset = SC_numsx
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nsx)
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%nsx)
  enl%nsx = (enl%nsx - 1)/2
  enl%nsy = enl%nsx

dataset = SC_EkeV
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%EkeV)

dataset = SC_Ehistmin
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%Ehistmin)

dataset = SC_Ebinsize
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%Ebinsize)

dataset = SC_depthmax
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%depthmax)

dataset = SC_depthstep
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%depthstep)

dataset = SC_sig
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MCsig)

dataset = SC_omega
  call HDF_readDatasetDouble(dataset, HDF_head, hdferr, enl%MComega)

! close the name list group
  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

! read from the EMheader
groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCfoil
  hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_ProgramName
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCprogname = trim(stringarray(1))
  deallocate(stringarray)

dataset = SC_Version
  call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
  enl%MCscversion = trim(stringarray(1))
  deallocate(stringarray)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

! open the Data group
groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCfoil
  hdferr = HDF_openGroup(groupname, HDF_head)

! read data items 
dataset = SC_numEbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numEbins)

dataset = SC_numzbins
  call HDF_readDatasetInteger(dataset, HDF_head, hdferr, enl%numzbins)

dataset = SC_accume
  call HDF_readDatasetIntegerArray3D(dataset, dims3, HDF_head, hdferr, acc_e)
  enl%num_el = sum(acc_e)
  nx = (dims3(2)-1)/2
  allocate(acc%accum_e(1:dims3(1),-nx:nx,-nx:nx))
  acc%accum_e = acc_e
  deallocate(acc_e)

dataset = SC_accumz
  call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)
  allocate(acc%accum_z(1:dims4(1),1:dims4(2),1:dims4(3),1:dims4(4)))
  acc%accum_z = acc_z
  deallocate(acc_z)

! and close everything
  call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
  if (HDFopen.eqv..TRUE.) call h5close_EMsoft(hdferr)

else
  call FatalError('TKDIndexingreadMCfile','Could not find MC file')
end if

if (present(verbose)) call Message(' -> completed reading Monte Carlo data from '&
//trim(enl%energyfile), frm = "(A)")

end subroutine TKDIndexingreadMCfile

end module TKDDImod
