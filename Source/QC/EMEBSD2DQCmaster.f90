! ###################################################################
! Copyright (c) 2017-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDQCmaster.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSD2DQCmaster
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief EBSD master pattern for quasi-crystals
!
!> @date 02/26/18 SS 1.0 original based on EMEBSDQCmaster program
!--------------------------------------------------------------------------

program EMEBSD2DQCmaster

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                        	:: nmldeffile, progname, progdesc
type(EBSD2DQCMasterNameListType)          :: ebsdnl

nmldeffile = 'EMEBSD2DQCmaster.nml'
progname = 'EMEBSD2DQCmaster.f90'
progdesc = 'Master pattern generation for 2-D quasi-crystal electron backscatter diffraction pattern'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 99, 0 /), progname)

! deal with the namelist stuff
call GetEBSD2DQCMasterNameList(nmldeffile,ebsdnl)

! perform the zone axis computations
call EBSD2DQCmasterpattern(ebsdnl, progname, nmldeffile)

end program EMEBSD2DQCmaster

!--------------------------------------------------------------------------
!
! SUBROUTINE:EBSDQCmasterpattern
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute a quasi-crystal master electron backscatter diffraction
!
!> @date 02/27/18 SS 1.0 original 
!--------------------------------------------------------------------------
recursive subroutine EBSD2DQCmasterpattern(ebsdnl, progname, nmldeffile)

use local
use error
use typedefs
use io
use NameListTypedefs
use NameListHDFwriters
use HDF5
use HDFsupport
use QCmod
! use QCmod, only:InterpolateLambert   ! use the routine from the Lambert module ...
use QCmodHDF
use gvectorsQC
use kvectorsQC
use MBmoduleQC
use MBmodule, only:CalcLgh
use kvectors, only:Delete_kvectorlist
use gvectors, only:Set_Bethe_Parameters
use Lambert
use omp_lib
use dictmod
use InitializersQCHDF
use timing
use ISO_C_BINDING
use stringconstants

IMPLICIT NONE

! subroutine arguments
type(EBSD2DQCMasterNameListType),INTENT(INOUT)    :: ebsdnl
character(fnlen),INTENT(IN)							          :: progname
character(fnlen),INTENT(IN)                      	:: nmldeffile

! quasicrystal unitcell type
type(TDQCStructureType),pointer                   :: QCcell
character(fnlen)                                  :: QCtype, xtalname
type(dicttype),pointer            :: dict
! terminal output variables
integer(kind=irg)									:: io_int(3), tickstart
integer(kind=ill)									:: io_int_ill(1)
real(kind=sgl)										:: io_real(3)

! timing variables
real(kind=sgl)										:: tstop
real(kind=sgl)										:: exec_time

! date and time for HDF output file
character(11)           							:: dstr
character(15)           							:: tstrb
character(15)           							:: tstre

! HDF variables
type(HDFobjectStackType)            				:: HDF_head
integer(kind=irg)									:: hdferr
character(fnlen)									:: datagroupname, groupname, dataset
character(fnlen, KIND=c_char),allocatable,TARGET 	:: stringarray(:)
character(fnlen,kind=c_char)                     	:: line2(1)

! variables for HDF5 i/o
character(fnlen)									:: energyfile
logical												    :: f_exists, g_exists, readonly
integer(kind=irg)									:: numsx, numsy, numEbins, numzbins, num_el, multiplier
real(kind=dbl)										:: Ekev, Ehistmin, Ebinsize, depthmax, depthstep
character(4)									    :: mode
real(kind=dbl)										:: etotal
integer(kind=irg),allocatable			:: acc_z(:,:,:,:), accum_z(:,:,:,:)
integer(HSIZE_T)									:: dims4(4)

! variables for weight factors for thickness integration
integer(kind=irg),allocatable			:: thick(:)
real(kind=sgl),allocatable      	:: EkeVs(:), lambdaE(:,:)
integer(kind=irg)									:: izzmax, iE, ix, iy, iz, istat

! variables for list of incident beams
type(kvectorlist), pointer        :: khead, ktmp
real(kind=sgl), allocatable 			:: lambdaZ(:), klist(:,:), kij(:,:)
integer(kind=irg)									:: npy, numk, tots, totw, numset, izz, ik

! variables for list of reflections
type(BetheParameterType)          :: BetheParameters
type(TDQCreflisttype),pointer     :: reflist, firstw, rltmp
real(kind=sgl)										:: dmin_qc, dmin_p, selE, kk(3), FN(3), kn
integer(kind=irg)									:: gzero

! variables for dynamical diffraction calculation
real(kind=sgl),allocatable				:: svals(:), Iarray(:,:,:), Iarrayout(:,:,:)
complex(kind=dbl),allocatable			:: DynMat(:,:), Lgh(:,:), Sghtmp(:,:,:), Sgh(:,:)
integer(kind=irg)									:: nat(maxpasym), ipx, ipy, nns, nnw, nref
real(kind=sgl),allocatable				:: mLPNH(:,:,:,:), mLPSH(:,:,:,:), masterSPNH(:,:,:), masterSPSH(:,:,:)

! opnemp variables
integer(kind=irg)									:: nthreads, TID

! variables for HDF5 I/O
integer(HSIZE_T)									:: cnt4(4), cnt3(3), dims3(3), offset3(3), offset4(4)
logical												    :: insert = .TRUE., overwrite = .TRUE.
real(kind=sgl)										:: bp(4)
integer(kind=irg)									:: one, nlines

! variables for lambert to stereographic conversion
real(kind=dbl)										:: Radius, xy(2), xyz(3)
integer(kind=irg)									:: ii, jj, ierr

! auxiliary variables
complex(kind=dbl)									:: czero
logical												    :: verbose
integer(kind=irg)									:: numthreads

! complex(kind=dbl)         :: qg
! real(kind=dbl)            :: Vmod
! real(kind=dbl)            :: Vpmod
! real(kind=dbl)            :: xig
! real(kind=dbl)            :: xgp
! complex(kind=dbl)         :: Ucg 

call timestamp(datestring=dstr, timestring=tstrb)
call Time_tick(tickstart)

nullify(HDF_head%next)

!=============================================================
! READ THE NECESSARY INFORMATION FROM THE MC FILE
!=============================================================
!read Monte Carlo output file and extract necessary parameters
! first, we need to load the data from the MC program.
call Message('opening '//trim(ebsdnl%energyfile), frm = "(A)" )

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

energyfile = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%energyfile)
energyfile = EMsoft_toNativePath(energyfile)
inquire(file=trim(energyfile), exist=f_exists)

if (.not.f_exists) then
    call FatalError('ComputeMasterPattern','Monte Carlo input file does not exist')
end if

! open the MC file using the default properties.
readonly = .FALSE.
hdferr =  HDF_openFile(energyfile, HDF_head)

! next we need to make sure that this EM file actually contains a Monte Carlo 
! data set; if it does, then we can open the file and read all the information
datagroupname = '/EMData/MCOpenCL'
call H5Lexists_f(HDF_head%next%objectID,trim(datagroupname),g_exists, hdferr)
if (.not.g_exists) then
  call FatalError('EBSDQCmasterpattern','This HDF file does not contain any Monte Carlo data')
end if

! open the namelist group
groupname = SC_NMLparameters
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_MCCLNameList
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_numsx
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%numsx)
ebsdnl%numsx = (ebsdnl%numsx - 1)/2
numsx = ebsdnl%numsx
numsy = numsx

dataset = SC_EkeV
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ekev)
ebsdnl%Ekev = Ekev

dataset = SC_Ehistmin
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ehistmin)
ebsdnl%Ehistmin = Ehistmin

dataset = SC_Ebinsize
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, Ebinsize)
ebsdnl%Ebinsize = Ebinsize

dataset = SC_depthmax
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, depthmax)
ebsdnl%depthmax = depthmax

dataset = SC_depthstep
call HDF_readDatasetDouble(dataset, HDF_head, hdferr, depthstep)
ebsdnl%depthstep = depthstep

dataset = SC_mode
call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
mode 		= trim(stringarray(1))
ebsdnl%mode = mode 
deallocate(stringarray)

if (trim(ebsdnl%mode) .ne. 'full') then
    call FatalError('EBSDQCmasterpattern','The MC mode is not correct. Please select the correct monte carlo file')
end if

! close the name list group
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! open the Data group
groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)

datagroupname = 'MCOpenCL'
hdferr = HDF_openGroup(datagroupname, HDF_head)

! read data items 
dataset = SC_numEbins
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numEbins)

dataset = SC_numzbins
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numzbins)

dataset = SC_totnumel
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, num_el)

dataset = SC_multiplier
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, multiplier)

dataset = SC_accumz

call HDF_readDatasetIntegerArray4D(dataset, dims4, HDF_head, hdferr, acc_z)

allocate(accum_z(numEbins,numzbins,-numsx/10:numsx/10,-numsy/10:numsy/10),stat=istat)
accum_z = acc_z
deallocate(acc_z)

! and close everything
call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_EMsoft(hdferr)

io_int_ill(1) 	= num_el * multiplier
etotal			= float(num_el) * float(multiplier)

call WriteValue('Total number of electrons in MC file = ',io_int_ill,1,'(I15)')
call Message(' -> completed reading '//trim(ebsdnl%energyfile), frm = "(A/)")

!===========================================================================================
! CALCULATE WEIGHT FACTORS FOR DEPTH INTEGRATION
!===========================================================================================
! this is where we determine the value for the thickness integration limit for the CalcLgh3 routine...
allocate(EkeVs(numEbins),thick(numEbins))

do iE=1,numEbins
  EkeVs(iE) = Ehistmin + float(iE-1) * Ebinsize
end do

! then, for each energy determine the 95% histogram thickness
izzmax = 0
do iE = 1,numEbins
	do ix=-numsx/10,numsx/10
		do iy=-numsy/10,numsy/10
   			istat = sum(accum_z(iE,:,ix,iy))
   			izz = 1
   			do while (sum(accum_z(iE,1:izz,ix,iy)).lt.(0.95*istat)) 
    			izz = izz+1
   			end do
   			if (izz.gt.izzmax) izzmax = izz
  		end do
 	end do
 	thick(iE) = dble(izzmax) * depthstep
end do

izz = nint(maxval(thick)/depthstep)
allocate(lambdaE(1:numEbins,1:izz),stat=istat)
lambdaE = 0.0
do iE=1,numEbins
 do iz=1,izz
  lambdaE(iE,iz) = float(sum(accum_z(iE,iz,-numsx/10:numsx/10,-numsy/10:numsy/10)))/etotal
 end do
end do

! and get rid of the accum_z array
deallocate(accum_z)

!===========================================================================================
! QUASI CRYSTAL CRYSTALLOGRAPHY
!===========================================================================================
nullify(QCcell)
allocate(QCcell)

QCcell%voltage    = ebsdnl%Ekev
QCcell%mLambda    = -1.0
QCcell%dmin_qc    = ebsdnl%dmin_qc
QCcell%dmin_p     = ebsdnl%dmin_p
dmin_qc           = QCcell%dmin_qc
dmin_p            = QCcell%dmin_p
QCcell%centering  = 'P'
QCcell%QCtype     = trim(ebsdnl%QCtype)

! define the unit cell etc...
!call Initialize_QCCell(QCcell, dble(ebsdnl%QClatparm_a), dble(ebsdnl%QClatparm_c), QCcell%QCtype, verbose=.TRUE.)
xtalname =  'AlNiCo.qxtal'
nthreads = ebsdnl%nthreads

call Initialize_QCcell(QCcell, xtalname, dmin_qc, dmin_p, sngl(QCcell%voltage), nthreads, verbose=.TRUE.)
!===========================================================================================
! ARRAY AND PARAMETER INITIALIZATIONS
!===========================================================================================
! force dynamical matrix routine to read new Bethe parameters from file
call Set_Bethe_Parameters(BetheParameters)

nns   	=	0
nnw 	  = 0
tots 	  = 0
totw 	  = 0

numset 	= QCcell%ATOM_ntype  ! number of special positions in the unit cell

!izz 	  = numzbins
gzero	  = 1
! The computed intensities are stored in a smaller array of dimensions (ecpnl%nsamples+2) x (2*npy+1)
! This array encompasses two unit triangles of the icosahedral group and implements the mirror
! symmetry across the x-z plane, so that the final symmetry step only needs to apply the 60 rotational
! symmetry elements, not the full 120-member group 
allocate(svals(numset))

! allocate some arrays
allocate(mLPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1,numset))
allocate(mLPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1,numset))
allocate(masterSPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1))
allocate(masterSPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1))

mLPNH 		= 0.0
mLPSH 		= 0.0
masterSPNH	= 0.0
masterSPSH 	= 0.0

!===========================================================================================
! HDF5 I/O
!===========================================================================================

nullify(HDF_head%next)
! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

! open the existing file using the default properties.
hdferr =  HDF_openFile(energyfile, HDF_head)

! ===========================
! ===========================
! write the EMheader to the file
datagroupname = 'EBSDmaster'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, datagroupname)

! open or create a namelist group to write all the namelist files into
groupname   = SC_NMLfiles
hdferr    = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset   = SC_EBSDmasterNML
hdferr    = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname   = SC_NMLparameters
hdferr    = HDF_createGroup(groupname, HDF_head)

call HDFwriteEBSD2DQCMasterNameList(HDF_head, ebsdnl)
call HDFwriteBetheparameterNameList(HDF_head, BetheParameters)

! leave this group
call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)
  hdferr = HDF_createGroup(datagroupname, HDF_head)

dataset = SC_xtalname
  allocate(stringarray(1))
  stringarray(1)= trim(xtalname)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head, overwrite)
  else
    hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)
  end if

dataset = SC_numset
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head, overwrite)
else
    hdferr = HDF_writeDatasetInteger(dataset, numset, HDF_head)
end if

dataset = SC_BetheParameters
bp = (/ BetheParameters%c1, BetheParameters%c2, BetheParameters%c3, BetheParameters%sgdbdiff /)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head, overwrite)
else
    hdferr = HDF_writeDatasetFloatArray1D(dataset, bp, 4, HDF_head)
end if

dataset = SC_numEbins
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeDatasetInteger(dataset, numEbins, HDF_head, overwrite)
else
    hdferr = HDF_writeDatasetInteger(dataset, numEbins, HDF_head)
end if

dataset = SC_EkeVs
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, numEbins, HDF_head, overwrite)
else
    hdferr = HDF_writeDatasetFloatArray1D(dataset, EkeVs, numEbins, HDF_head)
end if

! create the hyperslabs and write zeroes to them for now
dataset = SC_mLPNH
dims4 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins, numset /)
cnt4 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1, numset /)
offset4 = (/ 0, 0, 0, 0 /)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
	hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head, insert)
else
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head)
end if

dataset = SC_mLPSH
dims4 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins, numset /)
cnt4 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1, numset /)
offset4 = (/ 0, 0, 0, 0 /)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head, insert)
else
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head)
end if

dataset = SC_masterSPNH
dims3 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins /)
cnt3 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1 /)
offset3 = (/ 0, 0, 0 /)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)
else
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head)
end if

dataset = SC_masterSPSH
dims3 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins /)
cnt3 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1 /)
offset3 = (/ 0, 0, 0 /)
call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)
else
    hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head)
end if

  call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
 call h5close_EMsoft(hdferr)

! the symmetry operators have all been defined in the dictmod module, so we only need to
! initialize them here...
allocate(dict)
dict%Num_of_init = 1
dict%Num_of_iterations = 1

if(trim(QCcell%QCtype) .eq. 'Oct') then
  dict%pgnum  = 34

else if(trim(QCcell%QCtype) .eq. 'Dec') then
  dict%pgnum  = 35

else if(trim(QCcell%QCtype) .eq. 'DoD') then
  dict%pgnum  = 36

else
  call FatalError('EMEBSD2DQCmaster:','unknown 2D quasi-crystal type.')

end if

call DI_Init(dict,'VMF')

call Time_tick(tickstart)

!===========================================================================================
! MAIN COMPUTATIONAL LOOP
!===========================================================================================
energyloop: do iE = numEbins,1,-1
	io_int(1)=iE
   	io_int(2)=numEbins
   	call Message('Starting computation for energy bin (in reverse order)', frm = "(/A)",advance="no")
   	call WriteValue(' ',io_int,2,"(I4,' of ',I4)",advance="no")
   	io_real(1) = EkeVs(iE)
   	call WriteValue('; energy [keV] = ',io_real,1,"(F6.2/)")
   	selE = EkeVs(iE)

   	! set the accelerating voltage
   	QCcell%voltage = dble(EkeVs(iE))
   	!call QC_CalcWaveLength(QCcell,verbose=.TRUE.)
    if(iE .ne. numEbins) then
    	nthreads = ebsdnl%nthreads
      	call Initialize_QCcell(QCcell, xtalname, dmin_qc, dmin_p, sngl(QCcell%voltage), nthreads, verbose=.TRUE., initLUT=.TRUE.)
    end if

!===========================================================================================
! GENERATING LIST OF k VECTORS
!===========================================================================================

! determine all independent incident beam directions inside the icosahedral unit triangle (use a linked list starting at khead)
! numk is the total number of k-vectors to be included in this computation
	nullify(khead)
	call TDQC_Calckvectors(khead,QCcell,ebsdnl%npx,numk)
	io_int(1)=numk
	call WriteValue('# independent beam directions to be considered = ', io_int, 1, "(I8)")

	allocate(Iarray(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1:numset),&
           Iarrayout(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1:numset))
	czero 	= cmplx(0.D0, 0.D0)

	! copy to array for OpenMP
	allocate(klist(4,numk), kij(3,numk),stat =istat)
	ktmp 	=> khead
	do ik = 1,numk
		klist(1:3,ik)  = 	ktmp%k(1:3)
		klist(4,ik)		 =	ktmp%kn
		kij(1:3,ik)		 =	(/ktmp%i, ktmp%j, ktmp%hs/)
		ktmp			     =>	ktmp%next
	end do

	! and remove the linked list
 	call Delete_kvectorlist(khead)

 	verbose = .FALSE.
  	tots 	= 0
  	totw 	= 0

 	! set the number of OpenMP threads 
  	call OMP_SET_NUM_THREADS(ebsdnl%nthreads)
  	io_int(1) = ebsdnl%nthreads
  	call WriteValue(' Attempting to set number of threads to ',io_int, 1, frm = "(I4)")
    nthreads = ebsdnl%nthreads
	! use OpenMP to run on multiple cores ... 
	!$OMP PARALLEL DEFAULT(PRIVATE) &
	!$OMP& SHARED(QCcell,numk,klist,kij, BetheParameters,thick,depthstep,lambdaE,numset) &
	!$OMP& SHARED(Iarray,tots,totw,iE,nthreads,gzero,ebsdnl)

  	numthreads = OMP_GET_NUM_THREADS()
  	TID = OMP_GET_THREAD_NUM()

	!$OMP DO SCHEDULE(DYNAMIC,100)
	beamloop: do ik = 1, numk

    	kk = klist(1:3,ik)

    	FN = kk/vecnorm(kk)

    	nullify(reflist)

    	call Initialize_QCReflectionList(QCcell, reflist, BetheParameters, FN, kk, nref, verbose=.FALSE.)

		! determine strong and weak reflections
    	nullify(firstw)
    	nns = 0
    	nnw = 0
    	call QC_Apply_BethePotentials(QCcell, reflist, firstw, BetheParameters, nref, nns, nnw)
!print*,nref,nns,nnw
    	allocate(DynMat(nns,nns))
    	call QC_GetDynMat(QCcell, reflist, firstw, DynMat, nns, nnw)
    	DynMat = DynMat * cmplx(1.D0/cPi/QCcell%mLambda,0.D0)

		! then we need to initialize the Sgh and Lgh arrays
    	if (allocated(Lgh)) deallocate(Lgh)
    	if (allocated(Sghtmp)) deallocate(Sghtmp)

    	allocate(Sghtmp(nns,nns,numset),Lgh(nns,nns))

    	Lgh 	= czero
    	Sghtmp 	= czero
    	nat 	= 0
    	call TDQC_CalcSgh(QCcell, reflist, nns, numset, Sghtmp, nat)

! solve the dynamical eigenvalue equation; this remains unchanged for QCs
    	kn 		= klist(4,ik)
        izz = nint(thick(iE)/depthstep)

    	call CalcLgh(DynMat, Lgh, dble(thick(iE)), dble(kn), nns, gzero, depthstep, lambdaE(iE,:), izz)
    	deallocate(DynMat)

! dynamical contributions
     	svals = 0.0
     	do ix = 1, numset
       		svals(ix) = real(sum(Lgh(1:nns,1:nns)*Sghtmp(1:nns,1:nns,ix)))
     	end do

     	svals = svals/float(sum(nat(1:numset)))

		! and store the resulting values in all the symmetryically equivalent positions
		! for now, we're only using inversion symmetry
    	ipx = kij(1,ik)
        ipy = kij(2,ik)
    	Iarray(ipx,ipy,1:numset) = svals(1:numset)

    	totw = totw + nnw
    	tots = tots + nns

    	if (mod(ik,5000).eq.0) then
        io_int(1) = ik
        io_int(2) = numk
        call WriteValue('  completed beam direction ',io_int, 2, "(I8,' of ',I8)")
      end if

    	call Delete_TDQCgvectorlist(reflist)
    	!if(associated(firstw)) call Delete_TDQCweakgvectorlist(firstw)

	end do beamloop
	! end of OpenMP portion
	!$OMP END DO
	!$OMP END PARALLEL

	io_int(1) = nint(float(tots)/float(numk))
	call WriteValue(' -> Average number of strong reflections = ',io_int, 1, "(I5)")
	io_int(1) = nint(float(totw)/float(numk))
	call WriteValue(' -> Average number of weak reflections   = ',io_int, 1, "(I5)")

	! next we need to apply 2D rotational symmetry to the triangle of intensity values
	! in the Iarray to generate the full mLPNH and mLPSH arrays
  Iarrayout = 0.0
  
	call TDQC_applySymmetry(dict, ebsdnl%npx, numset, Iarrayout, Iarray, QCcell%QCtype)

  mLPNH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1,1:numset) = &
  Iarrayout(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1:numset)

	mLPSH(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1,1:numset) = &
  Iarrayout(-ebsdnl%npx:ebsdnl%npx,-ebsdnl%npx:ebsdnl%npx,1:numset)

! get stereographic projections (summed over the atomic positions)
  	Radius = 1.0
  	do ii = -ebsdnl%npx,ebsdnl%npx 
    	do jj = -ebsdnl%npx,ebsdnl%npx 
      		xy 	= (/ float(ii), float(jj) /) / float(ebsdnl%npx)
      		xyz = StereoGraphicInverse( xy, ierr, Radius )
      		xyz = xyz/vecnorm(xyz)
      		if (ierr.ne.0) then 
        		masterSPNH(ii,jj,1) = 0.0
        		masterSPSH(ii,jj,1) = 0.0
      		else
        		masterSPNH(ii,jj,1) = InterpolateLambert(xyz, mLPNH, ebsdnl%npx, numset)
        		masterSPSH(ii,jj,1) = InterpolateLambert(xyz, mLPSH, ebsdnl%npx, numset)
      		end if
    	end do
  	end do

 	deallocate(klist, kij, Iarray, Iarrayout)

  datagroupname = 'EBSDmaster'

  nullify(HDF_head%next)
! Initialize FORTRAN HDF interface.
  call h5open_EMsoft(hdferr)

  ! open the existing file using the default properties.
  hdferr =  HDF_openFile(energyfile, HDF_head)

! update the time string
  groupname = SC_EMheader
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

  dataset = SC_StopTime
  line2(1) = dstr//', '//tstre
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

  tstop = Time_tock(tickstart)
  io_real(1) = tstop
  call WriteValue('Execution time: ',io_real,1,'(F10.2," sec")')

  dataset = SC_Duration
  if (iE.eq.numEbins) then 
    call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
    if (g_exists) then     
      hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
    else
      hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
    end if
  else
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head, overwrite)
  end if

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)
  
  groupname = SC_EMData
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

 	! add data to the hyperslab
	dataset = SC_mLPNH
  	dims4 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins, numset /)
  	cnt4 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1, numset /)
  	offset4 = (/ 0, 0, iE-1, 0 /)
  	hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPNH, dims4, offset4, cnt4, HDF_head, insert)

	dataset = SC_mLPSH
  	dims4 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins, numset /)
  	cnt4 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1, numset /)
  	offset4 = (/ 0, 0, iE-1, 0 /)
  	hdferr = HDF_writeHyperslabFloatArray4D(dataset, mLPSH, dims4, offset4, cnt4, HDF_head, insert)

	dataset = SC_masterSPNH
  	dims3 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins /)
  	cnt3 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1 /)
  	offset3 = (/ 0, 0, iE-1 /)
  	hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPNH, dims3, offset3, cnt3, HDF_head, insert)

	dataset = SC_masterSPSH
  	dims3 = (/  2*ebsdnl%npx+1, 2*ebsdnl%npx+1, numEbins /)
  	cnt3 = (/ 2*ebsdnl%npx+1, 2*ebsdnl%npx+1, 1 /)
  	offset3 = (/ 0, 0, iE-1 /)
  	hdferr = HDF_writeHyperslabFloatArray3D(dataset, masterSPSH, dims3, offset3, cnt3, HDF_head, insert)

    call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
    call h5close_EMsoft(hdferr)

end do energyloop

io_real(1)  = tstop/60.0
call WriteValue("Execution time = ",io_real,1,'(F10.2," min")')

end subroutine EBSD2DQCmasterpattern
