! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
module PFInversionHDF

use local
use constants

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE: BackProjectionCwrapper
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief wrapper function for C to avoid passing cell variable to C
!
!> @param ODF         orientation distribution function
!> @param ncub        number of bins in semi-edge in x,y and z directions
!> @param PFhkl       hkl indices of the specific pole figure
!> @param nLam        bins in the output along semi-edge in Lambert projection
!
!> @out PFLam         Pole figure in Lambert projection
!
!> @date 11/07/16     SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine BackProjectionCwrapper(ncub, ODF, PFhkl, nLam, PFLam, xtalname) &
bind(c, name = 'BackProjectionCwrapper')
!DEC$ ATTRIBUTES DLLEXPORT :: BackProjectionCwrapper

use symmetry
use HDFsupport
use typedefs
use crystal
use PFInversionmod
use ECPmod
use HDFsupport
use InitializersHDF
use stringconstants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                    :: ncub
real(kind=dbl),INTENT(OUT)                      :: ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub)
integer(kind=irg),INTENT(IN)                    :: PFhkl(3)
integer(kind=irg),INTENT(IN)                    :: nLam
real(kind=dbl),INTENT(IN)                       :: PFLam(-nLam:nLam,-nLam:nLam)
character(1),dimension(fnlen),INTENT(IN)        :: xtalname

character(fnlen)                                :: xtalname2, fname
character(1)                                    :: rchar
type(unitcell)                                  :: cell
integer(kind=irg)                               :: eqvplanes(48,3), ii, pgnum, hdferr
type(HDFobjectStackType),pointer                :: HDF_head_cell

type(gnode)                                     :: rlp
type(DynType)                                   :: Dyn
real(kind=sgl)                                  :: dmin, voltage
logical                                         :: verbose2

xtalname2 = ''
!nullify(cell)        
!allocate(cell)        

! just some arbitrary values
dmin = 0.04
voltage = 30.0
verbose2 = .FALSE.

call ResetCell(cell)

do ii = 1,fnlen
    rchar = xtalname(ii)
    xtalname2 = trim(xtalname2)//rchar
end do

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays

!cell%SG%SYM_reduce=.TRUE.
!cell%fname = xtalname2

nullify(HDF_head_cell)
fname = trim(EMsoft_getXtalpathname())//trim(xtalname2)
fname = EMsoft_toNativePath(fname)
hdferr =  HDF_openFile(fname, HDF_head_cell)
call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)

call Initialize_Cell(cell, Dyn, rlp, xtalname2, dmin, voltage, verbose2, HDF_head_cell)

!call CrystalData(cell, verbose= .FALSE., existingHDFhead = HDF_head_cell)

pgnum = GetPointGroup(xtalname2,.FALSE.)

call BackProjection(ncub, ODF, PFhkl, nLam, PFLam, pgnum, cell)

end subroutine BackProjectionCwrapper

!--------------------------------------------------------------------------
!
! SUBROUTINE: ForwardProjectionCwrapper
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief wrapper function for C to avoid passing cell variable to C
!
!> @param ODF         orientation distribution function
!> @param ncub        number of bins in semi-edge in x,y and z directions
!> @param PFhkl       hkl indices of the specific pole figure
!> @param nLam        bins in the output along semi-edge in Lambert projection
!
!> @out PFLam         Pole figure in Lambert projection
!
!> @date 11/07/16     SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine ForwardProjectionCwrapper(ncub, ODF, PFhkl, nLam, PFLam, xtalname) &
bind(c, name = 'ForwardProjectionCwrapper')
!DEC$ ATTRIBUTES DLLEXPORT :: ForwardProjectionCwrapper

use symmetry
use HDFsupport
use typedefs
use crystal
use ECPmod
use PFInversionmod
use HDFsupport 
use InitializersHDF
use stringconstants

IMPLICIT NONE

integer(kind=irg),INTENT(IN)                  :: ncub
real(kind=dbl),INTENT(IN)                     :: ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub)
integer(kind=irg),INTENT(IN)                  :: PFhkl(3)
integer(kind=irg),INTENT(IN)                  :: nLam
real(kind=dbl),INTENT(OUT)                    :: PFLam(-nLam:nLam,-nLam:nLam)
character(1),dimension(fnlen),INTENT(IN)      :: xtalname

character(fnlen)                              :: xtalname2, fname
character(1)                                  :: rchar
type(unitcell)                                :: cell
integer(kind=irg)                             :: eqvplanes(48,3), num, ii, pgnum, hdferr
integer(kind=irg),allocatable                 :: PFhkl_eqv(:,:)
type(HDFobjectStackType),pointer              :: HDF_head_cell

type(gnode)                                   :: rlp
type(DynType)                                 :: Dyn
real(kind=sgl)                                :: dmin, voltage
logical                                       :: verbose2


! just some arbitrary values
dmin = 0.04
voltage = 30.0
verbose2 = .FALSE.

xtalname2 = ''
!nullify(cell)        
!allocate(cell)        

call ResetCell(cell)

do ii = 1,fnlen
    rchar = xtalname(ii)
    xtalname2 = trim(xtalname2)//rchar
end do

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays

!cell%SG%SYM_reduce=.TRUE.
!cell%fname = xtalname2

nullify(HDF_head_cell)
fname = trim(EMsoft_getXtalpathname())//trim(xtalname2)
fname = EMsoft_toNativePath(fname)
hdferr =  HDF_openFile(fname, HDF_head_cell)
call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)

call Initialize_Cell(cell, Dyn, rlp, xtalname2, dmin, voltage, verbose2, HDF_head_cell)
!call CrystalData(cell, verbose= .FALSE., existingHDFhead = HDF_head_cell)

call CalcFamily(cell,PFhkl,num,'r',eqvplanes)

pgnum = GetPointGroup(xtalname2,.FALSE.)
num = 1
allocate(PFhkl_eqv(num,3))

PFhkl_eqv(1:num,1:3) = eqvplanes(1:num,1:3)

call ForwardProjection(ncub, ODF, PFhkl_eqv, num, nLam, PFLam, pgnum, cell)

deallocate(PFhkl_eqv)

end subroutine ForwardProjectionCwrapper

!-----------------------------------------------------------------------------------------
!
! SUBROUTINE: WritePFInversionH5Data
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write out HDF5 file for pole figure inversion
!
!> @date 04/02/17     SS 1.0 original
!-----------------------------------------------------------------------------------------
subroutine WritePFInversionH5Data(epf, dstr, tstrb, progname, nmldeffile, ODF, PF, PFrecon)
!DEC$ ATTRIBUTES DLLEXPORT :: WritePFInversionH5Data

use NameListTypedefs
use io
use HDFsupport
use NameListHDFwriters
use typedefs
use PFInversionmod
use stringconstants

type(PFInversionNameListType),INTENT(IN)            :: epf
type(PoleFigures),pointer                           :: PF
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile
real(kind=dbl),INTENT(IN)                           :: ODF(-epf%ncub:epf%ncub,-epf%ncub:epf%ncub,-epf%ncub:epf%ncub)!val(epf%nnz)
!integer(kind=irg),INTENT(IN)                        :: row(epf%nnz), col(epf%nnz)
real(kind=dbl),INTENT(IN)                           :: PFrecon(-epf%nLam:epf%nLam,-epf%nLam:epf%nLam,epf%nfiles)

type(HDFobjectStackType),pointer                    :: HDF_head

character(15)                                       :: tstre
character(fnlen)                                    :: str
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, nmlname, h5file
integer(kind=irg)                                   :: hdferr, nODF, ii, nPF
real(kind=dbl)                                      :: PFstereo(-epf%nLam:epf%nLam,-epf%nLam:epf%nLam)

nmlname = 'PFInversionNML'

allocate(stringarray(1))

nullify(HDF_head)
call timestamp(timestring=tstre)

! Create a new file using the default properties.
h5file = trim(EMsoft_getEMdatapathname())//trim(epf%datafile)
h5file = EMsoft_toNativePath(h5file)
hdferr =  HDF_createFile(h5file, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening file')

dataset = SC_Version
stringarray(1)= 'EMsoft '//EMsoft_getEMsoftversion()
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! add the EMsoft header group
! write the EMheader to the file
groupname = SC_h5PFInversion
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! and write the nml file for this program to the HDF5 file
! read the text file and write the array to the file
dataset = trim(nmlname)
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwritePFInversionNameList(HDF_head, epf)

! leave this group
call HDF_pop(HDF_head)

groupname = SC_PFInversionData
hdferr = HDF_createGroup(groupname, HDF_head)

groupname = SC_Reconstructed
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_ODF
nODF = 2*epf%ncub + 1
hdferr = HDF_writeDatasetDoubleArray3D(dataset, ODF, nODF, nODF, nODF, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing non zero reconstructed ODF values')

!dataset = 'nnzrow'
!nODF = epf%nnz
!hdferr = HDF_writeDatasetIntegerArray1D(dataset, row, nODF, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing non zero reconstructed ODF rows')

!dataset = 'nnzcol'
!nODF = epf%nnz
!hdferr = HDF_writeDatasetIntegerArray1D(dataset, col, nODF, HDF_head)
!if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing non zero reconstructed ODF columns')

nPF = 2*epf%nLam + 1
do ii = 1,epf%nfiles
    write(str,'(A,3I1)')'LambertPF',PF%hkl(1:3,ii)
    dataset = str
    hdferr = HDF_writeDatasetDoubleArray2D(dataset, PFrecon(:,:,ii), nPF, nPF, HDF_head)

    write(str,'(A,3I1)')'StereoPF',PF%hkl(1:3,ii)
    dataset = str
    PFstereo = 0.D0
    PFstereo = PFLamToStereo(epf%nLam, PFrecon(:,:,ii))
    hdferr = HDF_writeDatasetDoubleArray2D(dataset, PFstereo, nPF, nPF, HDF_head)

end do

! leave this group
call HDF_pop(HDF_head)

groupname = "Input Pole Figures"
hdferr = HDF_createGroup(groupname, HDF_head)

do ii = 1,epf%nfiles
    write(str,'(A,3I1)')'LambertPF',PF%hkl(1:3,ii)
    dataset = str
    hdferr = HDF_writeDatasetDoubleArray2D(dataset, PF%PFhkl(:,:,ii), nPF, nPF, HDF_head)

    write(str,'(A,3I1)')'StereoPF',PF%hkl(1:3,ii)
    dataset = str
    PFstereo = 0.D0
    PFstereo = PFLamToStereo(epf%nLam, PF%PFhkl(:,:,ii))
    hdferr = HDF_writeDatasetDoubleArray2D(dataset, PFstereo, nPF, nPF, HDF_head)

end do

! leave this group
call HDF_pop(HDF_head,.TRUE.)

call Message('Data stored in file:'//trim(h5file))

end subroutine WritePFInversionH5Data

!-----------------------------------------------------------------------------------------
!
! SUBROUTINE: WriteMTEXFiles
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write out MTEX text file for pole figure inversion
!
!> @date 04/02/17     SS 1.0 original
!-----------------------------------------------------------------------------------------
subroutine WriteMTEXFiles(epf, PF, PFrecon, fname, incomplete)
!DEC$ ATTRIBUTES DLLEXPORT :: WriteMTEXFiles

use local
use io
use typedefs
use Lambert
use NameListTypedefs
use stringconstants

type(PFInversionNameListType),INTENT(IN)            :: epf
type(PoleFigures),pointer                           :: PF
real(kind=dbl),INTENT(IN)                           :: PFrecon(-epf%nLam:epf%nLam,-epf%nLam:epf%nLam,epf%nfiles)
character(fnlen),INTENT(IN)                         :: fname
logical,INTENT(IN),optional                         :: incomplete

integer(kind=irg)                                   :: ii, jj, kk, ierr
real(kind=dbl)                                      :: xy(2), xyz(3), pol, azi, delta
character(fnlen)                                    :: fname2, str
logical                                             :: inc
integer(kind=irg),parameter                         :: iunitf = 46

if(present(incomplete)) then
    if(incomplete) then
        inc = .TRUE.
    end if
end if

do kk = 1,epf%nfiles

    write(str,'(A,3I1,A)')trim(fname),PF%hkl(1:3,kk),'.txt'
    fname2 = trim(str)
    fname2 = trim(EMsoft_getEMdatapathname())//trim(fname2)
    fname2 = EMsoft_toNativePath(fname2)
    open(unit=iunitf,file=trim(fname2),form='formatted',status='unknown')

    do ii = -epf%nLam,epf%nLam
        do jj = -epf%nLam,epf%nLam
            if(.not. inc) then
                xy = (/dble(ii), dble(jj)/)/dble(epf%nLam)
            else
                xy = (/dble(ii), dble(jj)/)/dble(epf%nLam+2)
            end if
                 
            xyz = LambertSquareToSphere(xy,ierr)
            
            if(ierr .eq. 0) then
                xyz = xyz/NORM2(xyz)
                pol = acos(xyz(3))*180.D0/cPi
                azi = atan2(xyz(2),xyz(1))*180.D0/cPi
                write(iunitf,'(3F15.6)')pol, azi, PFrecon(ii,jj,kk)
            end if
        end do
    end do
    close(iunitf)
end do


call Message('Finished writing reconstructed PFs for MTEX visualization in folder:'//trim(EMsoft_getEMdatapathname()))

end subroutine WriteMTEXFiles

!--------------------------------------------------------------------------
!
! SUBROUTINE: GetPoleFigureData
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Read all pole figure data from the specified files
!
!> @param epf      epf pole figure namelist file
!> @param PF       pole figure structure
!
!
!> @date 03/31/17  SS 1.0 original
!> @date 04/04/17  SS 1.1 added x-ray scattering factors as weight factors
!--------------------------------------------------------------------------
recursive subroutine GetPoleFigureData(epf, PF, verbose)
!DEC$ ATTRIBUTES DLLEXPORT :: GetPoleFigureData

use local
use typedefs
use NameListTypedefs
use error
use io
use InitializersHDF
use HDFsupport
use diffraction
use stringconstants

type(PFInversionNameListType), INTENT(IN)      :: epf
type(PoleFigures),pointer                      :: PF
logical, optional                              :: verbose
type(unitcell)                                 :: cell
type(HDFobjectStackType),pointer               :: HDF_head_cell

logical                                        :: pout, f_exists
character(fnlen)                               :: ename, fname
integer(kind=irg)                              :: lx, ly, nfiles, ii, jj, kk, hdferr
type(gnode)                                    :: rlp
type(DynType)                                  :: Dyn
real(kind=sgl)                                 :: dmin, voltage
logical                                        :: verbose2

nLam = epf%nLam
nfiles = epf%nfiles

! just some arbitrary values
dmin = 0.04
voltage = 30.0
verbose2 = .FALSE.

! load the crystal structure file, which also computes all the important 
! matrices as well as all the symmetry arrays
!nullify(cell)        
!allocate(cell)        
cell%SG%SYM_reduce=.TRUE.
cell%fname = trim(epf%xtalname)

nullify(HDF_head_cell)
fname = trim(EMsoft_getXtalpathname())//trim(cell%fname)
fname = EMsoft_toNativePath(fname)
hdferr =  HDF_openFile(fname, HDF_head_cell)
call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)

call Initialize_Cell(cell, Dyn, rlp, epf%xtalname, dmin, voltage, verbose2, HDF_head_cell)

pout = .FALSE.
if(present(verbose)) then
    if(verbose) then
        pout = .TRUE.
    end if
end if

if(nfiles .gt. 0) then
    allocate(PF%hkl(3,nfiles),PF%PFhkl(-nLam:nLam,-nLam:nLam,nfiles), PF%xraysf(nfiles), PF%wf(nfiles))
else
    call FatalError('GetPoleFigureData:','None of the pole figures have been specified')
end if

PF%xraysf = cmplx(0.D0,0.D0)
PF%wf = 0.D0
rlp%method = 'XR'

do ii = 1,nfiles
    ename = trim(EMsoft_getEMdatapathname())//trim(epf%flist(ii))
    ename = EMsoft_toNativePath(ename)
    inquire(file=trim(ename), exist=f_exists)

    if(.not.f_exists) call FatalError('GetPoleFigureData:','Cannot find file: '//trim(ename))
    open(unit=dataunit,file=trim(ename),form='formatted',status='old')

    read(dataunit,*) PF%hkl(1:3,ii)
    
    do jj = -nLam,nLam
        do kk = -nLam,nLam
            read(dataunit,*)lx,ly,PF%PFhkl(jj,kk,ii)
        end do
    end do

    if(pout) call Message('Finished reading Pole figure file'//trim(ename))
    PF%PFhkl(:,:,ii) = PF%PFhkl(:,:,ii)/sum(PF%PFhkl(:,:,ii))

    call CalcUcg(cell,rlp,PF%hkl(1:3,ii))
    PF%xraysf(ii) = rlp%Ucg
    PF%wf(ii) = 1.D0
    !PF%wf(ii) = abs(PF%xraysf(ii))**2
end do

PF%wf = PF%wf/sum(PF%wf)
do ii = 1,nfiles
    PF%PFhkl(:,:,ii) = PF%wf(ii)*PF%PFhkl(:,:,ii)
end do

end subroutine GetPoleFigureData

!--------------------------------------------------------------------------
!
! SUBROUTINE: CalcBigA
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate forward model matrix
!> adapted from PFMatrix program

!> @date 06/04/17  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcBigA(nmldeffile2, mm, nn, nnz, nnzcolidp, nnzcolp, nnzvalssortedp)&
bind(c, name = 'CalcBigA')
!DEC$ ATTRIBUTES DLLEXPORT :: CalcBigA

use PFInversionmod
use local
use files
use constants
use rotations
use error
use io
use dictmod
use ECPmod, only:GetPointGroup
use typedefs,only:dicttype
use Lambert
use NameListTypedefs
use NameListHandlers
use HDF5
use HDFsupport
use InitializersHDF
use symmetry
use crystal
use others
use ISO_C_BINDING
use stringconstants

IMPLICIT NONE

character(1),dimension(fnlen),INTENT(IN)         :: nmldeffile2
integer(kind=irg),target,INTENT(INOUT)           :: mm, nn, nnz
!f2py intent(in,out) ::  mm, nn, nnz
integer(kind=irg),INTENT(OUT)                    :: nnzcolp(nn+1), nnzcolidp(nnz)
real(kind=dbl),INTENT(OUT)                       :: nnzvalssortedp(nnz)

character(fnlen)                   :: progname, progdesc, nmldeffile
logical                            :: verbose
type(PFInversionNameListType)      :: epf
type(PoleFigures),pointer          :: PF

type(unitcell)                     :: cell
type(DynType)                      :: Dyn
type(gnode)                        :: rlp
integer(kind=irg)                  :: eqvplanes(48,3)

integer(kind=irg)                  :: ncub, nLam
real(kind=dbl),allocatable,target  :: PFstereo(:,:), PFrecon(:,:,:),&
                                      wfLP(:,:), b(:), b2(:), dth(:)
real(kind=dbl),allocatable,target,save  :: nnzvals(:), nnzvalssorted(:)
real(kind=sgl),allocatable         :: nnzidcolreal(:), nnzidrowreal(:), nnzidlinreal(:)
integer(kind=ill),allocatable      :: nnzidlinsorted(:)
integer(kind=irg),allocatable      :: nnzidcolsorted(:), nnzidrowsorted(:)
integer(kind=irg),allocatable,target,save ::  nnzcol(:), nnzcolid(:)

integer(kind=ill)                  :: idlam, idlin, idcub
integer(kind=irg),target           :: mmm, nnn, nonz
integer(kind=irg),allocatable,target      :: indexlistlin(:), nnzcol2(:)
integer(kind=irg)                  :: hkl1(3), hkl2(3), hkl3(3), hkl4(3), istat, ntex
integer(kind=irg)                  :: ii, jj, kk, ierr, npts
integer(kind=irg)                  :: nixp, niyp, nizp, nix, niy, niz, i, j, k, ll,io_int(4), PFhkl(3)
real(kind=dbl)                     :: dx, dy, dz, dxm, dym, dzm, nbx, nby, nbz, val(4), io_real(2), wval
character(fnlen)                   :: fname
real(kind=dbl)                     :: eu(3),cu(3),dtor,qu(4),qui(4), xy(2),xyz(3),res, normfact, delta
real(kind=dbl)                     :: hkl_r(3), hkl_f(3), hcrossy(3), cth, qu2(4), qures(4), quresi(4), eu2(3), ro(4), mcu, dv
real(kind=dbl)                     :: pol, azi, LPapn, qures2(4), hpy(3), rocomp(3), tindex, tstrgth
type(dicttype),pointer             :: dict
integer(kind=irg)                  :: hdferr, Pmdims, FZtype, FZorder, pgnum, numeqv, nth
character(fnlen)                   :: xtalname, ename
character(11)                      :: dstr
character(15)                      :: tstrb
character(1)                       :: rchar

integer(kind=irg)                  :: kwidth
real(kind=dbl),allocatable,target  :: X(:)

type(HDFobjectStackType),pointer   :: HDF_head_cell
type(sparse_ll),pointer            :: sparseA, sparseA2, tailA, tailA2

type(MRCstruct)                    :: MRCheader
type(FEIstruct)                    :: FEIheaders(1024)
real(kind=dbl),allocatable         :: psum(:)
real(kind=dbl),allocatable         :: volume(:,:,:)          ! you'll need to fill this array with values ... 
integer(kind=irg)                  :: numx, numy, numz       ! set these to the size of the volume array

! name list file definition
nmldeffile = ''
do ii = 1,fnlen
    rchar       = nmldeffile2(ii)
    nmldeffile  = trim(nmldeffile)//rchar
end do

progname    = 'PFForwardTest.f90'
progdesc    = 'Program to invert pole figures using cubochoric representation and MBIR'
verbose     = .TRUE.

call timestamp(datestring=dstr, timestring=tstrb)

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 94 /), progname)

call GetPFInversionNameList(nmldeffile, epf)

! Initialize HDF5 FORTRAN interface.
call h5open_EMsoft(hdferr)

! initialize some parameters
xtalname    = trim(epf%xtalname)
pgnum       = GetPointGroup(xtalname,.FALSE.)
nLam        = epf%nLam
ncub        = epf%ncub
dtor        = cPi/180.D0
delta       = 1.D0/dble(nLam)

! allocate cell
!nullify(cell)        
!allocate(cell)        

nullify(HDF_head_cell)
fname   = trim(EMsoft_getXtalpathname())//trim(xtalname)
fname   = EMsoft_toNativePath(fname)
hdferr  =  HDF_openFile(fname, HDF_head_cell)
call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)

! initialize unitcell including symmetry etc.
call Initialize_Cell(cell,Dyn,rlp,xtalname, 0.05, 10.0, verbose, HDF_head_cell)

! allocate the dict structure
allocate(dict)
dict%Num_of_init        = 3
dict%Num_of_iterations  = 30
dict%pgnum              = pgnum
numeqv                  = 0
pgnum                   = dict%pgnum

! initialize the symmetry matrices
call DI_Init(dict,'nil') 
Pmdims  = dict%Nqsym

FZtype  = FZtarray(pgnum)
FZorder = FZoarray(pgnum)

! write some information

call Message('Xtalname :'//trim(xtalname))

io_int(1)   = pgnum
call WriteValue('Point group number = ',io_int,1,"(I8)")

io_int(1:2) = (/FZtype, FZorder/)
call WriteValue('Fundamental Zone type and order: ',io_int,2,"(2I8)")

io_int(1)   = Pmdims
call WriteValue('Number of symmetry operators: ',io_int,1,"(I8)")

!==================================================================
!============READ POLE FIGURE DATA FROM FILE=======================
!==================================================================

allocate(PF)

call GetPoleFigureData(epf, PF, verbose) 
do ii = 1,epf%nfiles
    print*,'(hkl), wf = ',PF%hkl(1:3,ii),PF%wf(ii)
end do

!#############################################################################################
!==============================================================================================
! GENERATING FORWARD MODEL IN SPARSE FORMAT 
!==============================================================================================
!#############################################################################################

mm = (epf%nfiles)*(2*nLam+1)**2
nn = (2*ncub+1)**3

io_int(1:2) = (/mm,nn/)
call WriteValue('Dimensions of the forward model = ',io_int,2,'(2I10)')

nth = 101
allocate(dth(1:nth))
dth = 0.D0

dth = (/(2.D0*cPi*float(ii-1)/(float(nth-1)),ii=1,nth)/)

call Message('Initializing A matrix ...')

LPapn = LPs%ap/2.D0

! initializing the matrix using the fundamental equation
! relating PF intensity and ODF (line integral)
! using linked list sparse matrix representation

nullify(sparseA,tailA)
allocate(sparseA)
tailA   =>  sparseA

nonz    =   0

! line integrals

do k = 1,epf%nfiles ! each pole figure

    eqvplanes   = 0
    numeqv      = 0
    call CalcFamily(cell,PF%hkl(1:3,k),numeqv,'r',eqvplanes)

    ! multiplicity of plane
    io_int(1:4) = (/eqvplanes(1,1:3), numeqv/)
    call WriteValue('Plane = ',io_int,4,'(3I6," multiplicity : ",I3)')

    do ll = 1,numeqv ! each equivalent hkl using symmetry

    hkl_r(1:3) = dble(eqvplanes(ll,1:3))

    ! convert to cartesian reference frame and normalize
    call TransSpace(cell, hkl_r, hkl_f, 'r', 'c')
    call NormVec(cell, hkl_f, 'c')
 
    ! each sample direction
    do i = -nLam,nLam ! each direction in pole figure
        do j = -nLam,nLam

            idlam = (k-1)*(2*nLam+1)**2 + (i+nLam)*(2*nLam+1) + j + nLam + 1

            xy  = (/dble(i)/dble(nLam), dble(j)/dble(nLam)/)
            xyz = LambertSquareToSphere(xy,ierr)

            if (ierr .eq. 0) then
               xyz = xyz/NORM2(xyz)
            else
                call FatalError('LambertSquareToSphere:','Coulnd not convert lambert square to sphere')
            end if

!       hcrossy =  PFhkl_f x xyz; 
            hcrossy =  (/ hkl_f(2)*xyz(3) - hkl_f(3)*xyz(2),&
                          hkl_f(3)*xyz(1) - hkl_f(1)*xyz(3),&
                          hkl_f(1)*xyz(2) - hkl_f(2)*xyz(1)/)
  
            if(sum(abs(hcrossy)) .ne. 0.D0) then
                hcrossy = hcrossy/NORM2(hcrossy)
            else
                hcrossy = (/1.D0, 0.D0, 0.D0/)
            end if

            cth = DOT_PRODUCT(hkl_f,xyz)

            qu = ax2qu((/hcrossy(1:3),acos(cth)/))
            if(qu(1) .lt. 0.D0) qu = -qu

            do kk = 1,nth
            
                qu2     = ax2qu((/xyz(1),xyz(2),xyz(3),dth(kk)/))
                if(qu2(1) .lt. 0.D0) qu2 = -qu2

                qures   = quat_mult(qu2,qu)
                if(qures(1) .lt. 0.D0) qures = -qures 
                    
                eu      = qu2eu(qures)

                ! only points in RFZ are kept
                call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)
 
                cu      = eu2cu(eu2)

                nbx     = cu(1)*dble(ncub)/LPapn
                nby     = cu(2)*dble(ncub)/LPapn
                nbz     = cu(3)*dble(ncub)/LPapn

                nix     = floor(nbx)
                niy     = floor(nby)
                niz     = floor(nbz)
 
                if(nix < -ncub) nix = -ncub
                if(niy < -ncub) niy = -ncub
                if(niz < -ncub) niz = -ncub
 
                nixp    = nix + 1
                niyp    = niy + 1
                nizp    = niz + 1
 
                if(nixp > ncub) nixp = ncub
                if(niyp > ncub) niyp = ncub
                if(nizp > ncub) nizp = ncub

                dx      = nbx - nix
                dy      = nby - niy
                dz      = nbz - niz

                dxm     = 1.D0 - dx
                dym     = 1.D0 - dy
                dzm     = 1.D0 - dz
            
                nix     = nix + ncub + 1
                niy     = niy + ncub + 1
                niz     = niz + ncub + 1

                nixp    = nixp + ncub + 1
                niyp    = niyp + ncub + 1
                nizp    = nizp + ncub + 1

                idcub   = (niz - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nix
                idlin   = (idcub - 1)*mm + idlam
                wval    = dxm*dym*dzm/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val   = wval
                    nonz        = nonz + 1

                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (niz - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nixp
                idlin = (idcub - 1)*mm + idlam
                wval  = dx*dym*dzm/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val   = wval
                    nonz        = nonz + 1

                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (niz - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nix
                idlin = (idcub - 1)*mm + idlam
                wval  = dxm*dy*dzm/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val   = wval
                    nonz        = nonz + 1

                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (niz - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nixp
                idlin = (idcub - 1)*mm + idlam
                wval  = dx*dy*dzm/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val = wval
                    nonz = nonz + 1
                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (nizp - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nix
                idlin = (idcub - 1)*mm + idlam
                wval  = dxm*dym*dz/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val = wval
                    nonz = nonz + 1
                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (nizp - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nixp
                idlin = (idcub - 1)*mm + idlam
                wval  = dx*dym*dz/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val = wval
                    nonz = nonz + 1
                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (nizp - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nix
                idlin = (idcub - 1)*mm + idlam
                wval  = dxm*dy*dz/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val = wval
                    nonz = nonz + 1
                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                idcub = (nizp - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nixp
                idlin = (idcub - 1)*mm + idlam
                wval  = dx*dy*dz/dble(numeqv)
                if(wval .gt. 1.0D-10) then
                    tailA%idcol = idcub
                    tailA%idrow = idlam
                    tailA%idlin = idlin
                    tailA%val = wval
                    nonz = nonz + 1
                    allocate(tailA%next)
                    tailA => tailA%next
                    nullify(tailA%next)
                end if

                end do ! theta loop

            end do ! lamy loop
        end do  ! lamx loop

    end do ! equivalent planes

end do ! Pole figure loop

call Message('Initialization complete ...')

allocate(nnzidcolreal(nonz),nnzidrowreal(nonz),nnzvals(nonz),indexlistlin(nonz),nnzidlinreal(nonz))
nnzidcolreal    = 0.D0
nnzidrowreal    = 0.D0
nnzidlinreal    = 0.D0
nnzvals         = 0.D0
indexlistlin    = 0

allocate(nnzidlinsorted(nonz),nnzidcolsorted(nonz),nnzidrowsorted(nonz),nnzvalssorted(nonz))
nnzidcolsorted  = 0
nnzidrowsorted  = 0
nnzidlinsorted  = 0
nnzvalssorted   = 0.D0

! extracting non-zero values from linked list for
! further processing
! sorting the values in column major order
nullify(tailA)
tailA => sparseA

do ii = 1,nonz
    nnzidlinreal(ii)    =   float(tailA%idlin)
    nnzidcolreal(ii)    =   float(tailA%idcol)
    nnzidrowreal(ii)    =   float(tailA%idrow)
    nnzvals(ii)         =   tailA%val
    indexlistlin(ii)    =   ii
    tailA               =>  tailA%next 
end do

call Message('--> deleting linked list of sparse matrix')

tailA   => sparseA
tailA2  => tailA%next

do ii = 1,nonz
    nullify(tailA)
    tailA  => tailA2
    tailA2 => tailA%next
end do

nullify(tailA,tailA2)

call Message('--> sorting values')

!call qsortd(nnzidlinreal, indexlistlin, nonz)
call SSORT(nnzidlinreal, indexlistlin, nonz, 2)

nnzidlinsorted = nnzidlinreal
!deallocate(nnzidlinreal)

do ii = 1,nonz
    nnzidlinsorted(ii) = nnzidlinreal(indexlistlin(ii))
    nnzvalssorted(ii)  = nnzvals(indexlistlin(ii))
    nnzidcolsorted(ii) = nnzidcolreal(indexlistlin(ii))
    nnzidrowsorted(ii) = nnzidrowreal(indexlistlin(ii))
end do

deallocate(nnzidcolreal,nnzidrowreal,nnzvals,nnzidlinreal)

call Message('removing repeated indices in sorted array')

! removing repeated indices from the arrays and storing in
! linked list sparseA2

nullify(sparseA2)
allocate(sparseA2)

tailA => sparseA2
nullify(tailA%next)

tailA%idcol = nnzidcolsorted(1)
tailA%idrow = nnzidrowsorted(1)
tailA%idlin = nnzidlinsorted(1)
tailA%val   = nnzvalssorted(1)
nnz = 1

do ii = 2,nonz
    if(nnzidlinsorted(ii) .eq. nnzidlinsorted(ii-1)) then
        tailA%val   = tailA%val + nnzvalssorted(ii)
    else if(nnzidlinsorted(ii) .ne. nnzidlinsorted(ii-1)) then
        allocate(tailA%next)
        tailA => tailA%next
        nullify(tailA%next)
        tailA%idrow = nnzidrowsorted(ii)
        tailA%idcol = nnzidcolsorted(ii)
        tailA%idlin = nnzidlinsorted(ii)
        tailA%val   = nnzvalssorted(ii) 
        nnz         = nnz + 1        
    end if
end do

! print some info about sparseness
io_int(1) = nnz
call WriteValue('Number of non-zero elements in forward model matrix =  ',io_int,1,'(I10)')

io_real(1) = dble(nnz)/dble(mm)/dble(nn)
call WriteValue('Sparsity in Forward model = ',io_real,1,'(F12.10)')

! create new list with no repeated indices
! create array with number of non zero entries in each column

allocate(nnzcol(nn+1),nnzcol2(nn+1))
nnzcol = 0
nnzcol2 = 0

deallocate(nnzidrowsorted,nnzidcolsorted,nnzvalssorted)

allocate(nnzcolid(nnz),nnzvalssorted(nnz))
nnzcolid = 0
nnzvalssorted = 0.D0

call Message('generating final list of non zero array')

nullify(tailA)
tailA => sparseA2

! set up the arrays for bclsf90 subroutine
do ii = 1,nnz
    nonz                =   tailA%idcol
    nnzcolid(ii)        =   tailA%idrow - 1
    nnzcol2(nonz)       =   nnzcol2(nonz) + 1
    nnzvalssorted(ii)   =   tailA%val
    tailA               =>  tailA%next 
end do

nnzcol(1) = 0
do ii = 2,nn+1
    nnzcol(ii) = nnzcol(ii-1) + nnzcol2(ii)
end do

call Message('--> deleting linked list of sparse matrix')

tailA   => sparseA2
tailA2  => tailA%next

do ii = 1,nnz-1
    nullify(tailA)
    tailA  => tailA2
    tailA2 => tailA%next
end do

nullify(tailA,tailA2)

nnzvalssortedp = nnzvalssorted
nnzcolidp      = nnzcolid
nnzcolp        = nnzcol

call Message('Done ...')

!#############################################################################################
!==============================================================================================
! DONE CREATING FORWARD MODEL
!==============================================================================================
!#############################################################################################

end subroutine CalcBigA


end module PFInversionHDF
