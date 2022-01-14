! ###################################################################
! Copyright (c) 2017-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMCBEDQC.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMCBEDQC
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Convergent Beam Electron Diffraction Pattern for Quasicrystals
!
!> @date 02/21/18 SS 1.0 original
!--------------------------------------------------------------------------
program EMCBEDQC

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(EMCBEDQCNameListType)              :: cbednl

nmldeffile  = 'EMCBEDQC.nml'
progname    = 'EMCBEDQC.f90'
progdesc    = 'Convergent beam Electron Diffraction Pattern for quasi-crystals'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 96, 0 /), progname)

! deal with the namelist stuff
call GetEMCBEDQCNameList(nmldeffile, cbednl)

! perform the zone axis computations
call CalcCBEDQCPattern(cbednl, progname, nmldeffile)

end program EMCBEDQC

!--------------------------------------------------------------------------
!
! SUBROUTINE:CalcCBEDQCPattern
!
!> @author Marc De Graef/Saransh Singh, Carnegie Mellon University
!
!> @brief compute a quasi-crystal CBED pattern
!
!> @date 02/21/18 MDG  1.0 original 
!--------------------------------------------------------------------------
subroutine CalcCBEDQCPattern(cbednl, progname, nmldeffile)

use local
use typedefs
use files
use QCmod
use io
use gvectors
use math
use HDFsupport
use HDF5
use NameListTypedefs
use NameListHandlers
use NameListHDFWriters
use quaternions
use constants
use rotations
use initializersQCHDF
use gvectorsQC
use kvectorsQC
use MBmoduleQC
use omp_lib

IMPLICIT NONE

type(EMCBEDQCNameListType),INTENT(INOUT)  :: cbednl
character(fnlen),INTENT(IN)               :: progname
character(fnlen),INTENT(IN)               :: nmldeffile

type(QCStructureType),pointer             :: QCcell
logical                                   :: verbose
integer(kind=irg)						  :: sLUT, ii, jj, ik, ithick, parity, hkl(6,23), Pmdims, h(6), gindex, io_int(3)
complex(kind=dbl)						  :: Ucg, qg, Ucg2, qg0
real(kind=dbl)           				  :: Vmod, Vpmod, xig, xgp, sg
real(kind=dbl)							  :: lgpar, lgperp, st, nfact

type(QCreflisttype),pointer               :: reflist, firstw, nexts
type(BetheParameterType)                  :: BetheParameters
type(gnode),save                          :: rlp
type(DynType),save                        :: Dyn

real(kind=sgl)							  :: FN(3), kk(3), dmin, kp(3), ku(3), io_real(3), qu(4)
real(kind=dbl)                            :: epar(6,3), mLambda
integer(kind=irg)						  :: nref, nns, nnw, counter, numk, thick, ierr
type(IncidentListCBED),pointer            :: klist, ktmp
real(kind=sgl),allocatable                :: klistarray(:,:)
integer(kind=irg),allocatable             :: kpix(:,:), hklarray(:,:)

complex(kind=dbl),allocatable             :: DynMat(:,:), DynMatMaster(:,:), wave(:), S(:,:), wave2(:)

logical                                   :: f_exists
real(kind=sgl),allocatable                :: intensity(:,:,:)
integer(kind=irg)                         :: nthreads, TID
real(kind=sgl),parameter                  :: DtoR = cPi/180.0
type(HDFobjectStackType)                  :: HDF_head
character(fnlen)                          :: dataset, instring, outname, groupname
character(fnlen)                          :: mode, qxtalname
integer(HSIZE_T)                          :: dims3(3), cnt3(3)
integer(kind=irg)                         :: hdferr
integer(kind=irg)                         :: tstart, tstop, clock_rate
real(kind=sgl)                            :: exec_time
character(11)                             :: dstr
character(15)                             :: tstrb
character(15)                             :: tstre

call timestamp(datestring=dstr, timestring=tstrb)
call system_clock(tstart, clock_rate)

nullify(QCcell)
allocate(QCcell)

QCcell%voltage  = cbednl%voltage
QCcell%mLambda  = -1.0
QCcell%dmin     = cbednl%dmin
dmin            = QCcell%dmin
qxtalname 	    = cbednl%qxtalname
nthreads 		= cbednl%nthreads
!===========================================================================================
! QUASI CRYSTAL CRYSTALLOGRAPHY
!===========================================================================================

call Initialize_QCcell(QCcell, qxtalname, dmin, sngl(QCcell%voltage), nthreads, verbose=.TRUE.)

!===========================================================================================
! GET LIST OF k VECTORS
!===========================================================================================
! get list of k-vectors in a cone with z-direction as the cone axis
nullify(klist)

call GetVectorsConeCBEDQC(cbednl, klist, numk)

allocate(klistarray(3,numk), kpix(2,numk))
klistarray = 0.0
kpix       = 0

ktmp => klist
do ii = 1,numk
  klistarray(1:3,ii)  =   ktmp%k(1:3)
  kpix(1:2,ii)        =   (/ktmp%i, ktmp%j/)
  ktmp                =>  ktmp%next
end do

io_int(1) = numk
call WriteValue('total # of k vectors = ',io_int,1,'(I6)')

!===========================================================================================
! GET LIST OF REFLECTIONS AND CATEGORIZE AS WEAK/STRONG
!===========================================================================================
! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters)

nullify(reflist)
qu = eu2qu(cbednl%eu(1:3)*DtoR)
kk = (/0.0, 0.0, 1.0/)/QCcell%mLambda
FN = (/0.0, 0.0, 1.0/)

FN = quat_LP(conjg(qu),FN)
kk = quat_LP(conjg(qu),kk)

!FN = quat_LP(conjg(qu),FN)
!call Initialize_QCReflectionList(QCcell, reflist, BetheParameters, FN, kk, dmin, nref)
call Initialize_QCReflectionList(QCcell, reflist, BetheParameters, FN, kk, nref)

io_int(1) = nref
call WriteValue('total number of reflections = ', io_int, 1)

allocate(DynMatMaster(nref,nref), DynMat(nref,nref), wave(nref), wave2(nref), S(nref,nref))
DynMat      = cmplx(0.D0, 0.D0)
S           = cmplx(0.D0, 0.D0)

call QC_GetDynMatMaster(QCcell, reflist, DynMatMaster, nref)

! determine strong and weak reflections
! nullify(firstw)
! nns = 0
! nnw = 0
!call QC_Apply_BethePotentials(QCcell, reflist, firstw, BetheParameters, nref, nns, nnw)
!call QC_Apply_BethePotentials(QCcell, reflist, firstw, BetheParameters, nref, nns, nnw)

!io_int(1) = nns
!call WriteValue('number of strong reflections = ', io_int, 1)

!io_int(1) = nnw
!call WriteValue('number of weak reflections = ', io_int, 1)

allocate(hklarray(6,nref))
hklarray = 0

nexts => reflist%next
do ii = 1,nref
  hklarray(1:6,ii)  =   nexts%hkl
  nexts             =>  nexts%next 
end do

! some parameters required for simulation
gindex  = QC_getindex(QCcell, (/0,0,0,0,0,0/))
qg0     = QCcell%LUTqg(gindex)
thick   = nint(cbednl%thickness)
epar    = QCcell%epar
mLambda = QCcell%mLambda

allocate(intensity(nref,numk,thick))
intensity  = 0.0

!===========================================================================================
! GET DYNAMICAL MATRIX AND PROPAGATE BEAM FOR EACH k VECTOR
! CAN BE PARALLELIZED EASILY BY DISTRIBUTING OVER MANY THREADS
!===========================================================================================

call OMP_SET_NUM_THREADS(cbednl%nthreads)
io_int(1) = cbednl%nthreads
call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")

!$OMP  PARALLEL DEFAULT(PRIVATE) SHARED(numk, klistarray, epar, qu, DynMatMaster) &
!$OMP& SHARED(hklarray, qg0, nref, thick, intensity, mLambda, FN, QCcell)

!$OMP DO SCHEDULE(DYNAMIC)
do ik = 1,numk

  kk  = klistarray(1:3,ik)/mLambda
  kk  = quat_LP(conjg(qu), kk) 
  !klistarray(1:3,ik)  = kk(1:3)

! use interaction coefficients from master list and calculate sg for each
! beam direction
  DynMat(1:nref,1:nref) = DynMatMaster(1:nref,1:nref)

  do ii = 1,nref
    h(1:6)        = hklarray(1:6,ii)
    sg            = QC_Calcsg(QCcell,h,kk,FN)
    DynMat(ii,ii) = (cmplx(2.D0*sg,0.D0) + qg0) * cmplx(cPi, 0.D0)
  end do

  DynMat  = DynMat * cmplx(0.D0,1.D0)

  call MatrixExponential(DynMat, S, 1.D0, 'Pade', nref)

  wave    = cmplx(0.D0, 0.D0)
  wave(1) = cmplx(1.D0,0.D0)

  do ithick = 1,thick
	 wave = matmul(S,wave)
   intensity(1:nref,ik,ithick) = abs(wave(1:nref))
  end do

  if(mod(ik,500) .eq. 0) then
    io_real(1) = 100.D0 * dble(ik)/dble(numk)
    call WriteValue('completed ',io_real,1,'(F10.2,"% of beams")')
  end if
end do
!$OMP END DO
!$OMP END PARALLEL

call timestamp(datestring=dstr, timestring=tstre)

! HDF5 I/O
! write out the data to the file
nullify(HDF_head%next)

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
outname = trim(EMsoft_getEMdatapathname())//trim(cbednl%datafile)
outname = EMsoft_toNativePath(outname)

inquire(file=trim(outname), exist=f_exists)
! if file already exists, then delete it
if (f_exists) then
  open(unit=dataunit,file=trim(outname),&
      status='unknown',form='unformatted',access='direct',recl=1,iostat=ierr)
  close(unit=dataunit,status='delete')
end if

hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
groupname = SC_CBEDQC
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_CBEDQCNameList
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteCBEDQCNameList(HDF_head, cbednl)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

dataset     = SC_klist  
klistarray  = klistarray/QCcell%QClatparm  
hdferr      = HDF_writeDatasetFloatArray2D(dataset, klistarray, 3, numk, HDF_head)

dataset = SC_Intensities
hdferr  = HDF_writeDatasetFloatArray3D(dataset, intensity, nref, numk, thick, HDF_head)

dataset = SC_hkl
hdferr  = HDF_writeDatasetIntegerArray2D(dataset, hklarray, 6, nref, HDF_head)

dataset = SC_PixelLocation
hdferr  = HDF_writeDatasetIntegerArray2D(dataset, kpix, 2, numk, HDF_head)

call system_clock(tstop, clock_rate)
exec_time   = real(tstop - tstart)/real(clock_rate)
io_real(1)  = exec_time
call WriteValue('Execution time = ',io_real,1,'(F8.2,"sec")')

end subroutine CalcCBEDQCPattern
