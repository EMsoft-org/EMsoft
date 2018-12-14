! ###################################################################
! Copyright (c) 2015-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:EMECPDictionaryIndexing.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMECPDictionaryIndexing
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Indexing of ECP patterns using the dictionary approach. 
!
!> @date 12/28/15 SS  1.0 original
!> @date 01/25/17 MDG 1.1 corrected template file numbering
!--------------------------------------------------------------------------

program ECPIndexing

use local 
use NameListTypedefs
use NameListHandlers
use ECPmod
use json_module
use files
use error
use io

IMPLICIT NONE

character(fnlen)                                   :: nmldeffile, progname, progdesc
type(ECPIndexingNameListType)                      :: ecpnl
type(ECPLargeAccumType),pointer                    :: acc
type(ECPMasterType),pointer                        :: master
logical                                            :: verbose
integer(kind=irg)                                  :: istat, nsig, numk, i, res
integer(kind=irg),allocatable                      :: kij(:,:)
real(kind=sgl),allocatable                         :: anglewf(:), klist(:,:)
type(IncidentListECP),pointer                      :: khead, ktmp


interface
        subroutine MasterSubroutine(ecpnl,acc,master,kij,klist,anglewf,progname,nmldeffile)

        use local
        use typedefs
        use NameListTypedefs
        use NameListHandlers
        use files
        use dictmod
        use others, only: SSORT
        use crystal
        use initializers
        use gvectors
        use io
        use error
        use diffraction
        use symmetry
        use quaternions
        use constants
        use rotations
        use so3
        use math
        use ECPmod
        use clfortran
        use CLsupport
        use omp_lib
        use Indexingmod
        use ISO_C_BINDING

        IMPLICIT NONE

        type(ECPIndexingNameListType),INTENT(INOUT)        :: ecpnl
        type(ECPLargeAccumType),pointer,INTENT(IN)         :: acc
        type(ECPMasterType),pointer,Intent(IN)             :: master
        integer(kind=irg),INTENT(IN)                       :: kij(2,ecpnl%npix**2)
        real(kind=sgl),INTENT(IN)                          :: klist(3,ecpnl%npix**2)
        real(kind=sgl),INTENT(IN)                          :: anglewf(nint((ecpnl%thetac)+abs(ecpnl%sampletilt)) + 1)
        character(fnlen),INTENT(IN)                        :: progname
        character(fnlen),INTENT(IN)                        :: nmldeffile

        end subroutine MasterSubroutine
end interface

nmldeffile = 'EMECPDI.nml'
progname = 'EMECPDI.f90'
progdesc = 'Program to index channeling patterns using the dynamically calculated dictionary'
verbose = .TRUE.

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 72 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMECPIndexing','JSON input not yet implemented')
!  call JSONreadECPIndexingNameList(ecpnl, nmldeffile, error_cnt)
else
  call GetECPIndexingNameList(nmldeffile,ecpnl)
end if

! 1. read the Monte Carlo data file
allocate(acc)
call ECPIndexingreadMCfile(ecpnl, acc, verbose=.TRUE.)

! 2. read EBSD master pattern file
allocate(master)
call ECPIndexingreadMasterfile(ecpnl, master)

! 3. generate detector arrays and weight factors
call ECPIndexingGenerateDetector(ecpnl, master, verbose)

nsig = nint((ecpnl%thetac) + abs(ecpnl%sampletilt)) + 1
allocate(anglewf(1:nsig),stat=istat)

call ECPIndexingGetWeightFactors(ecpnl, master, acc, anglewf, nsig, verbose)

! 4. generate list of incident vectors and put them in array for OpenMP
numk = 0
call GetVectorsConeIndexing(ecpnl, khead, numk)
allocate(kij(2,numk),klist(3,numk),stat=istat)

ktmp => khead
! converting to array for OpenMP parallelization
do i = 1,numk
   klist(1:3,i) = ktmp%k(1:3)
   kij(1:2,i) = (/ktmp%i,ktmp%j/)
   ktmp => ktmp%next
end do

! perform the dictionary indexing computations
call MasterSubroutine(ecpnl,acc,master,kij,klist,anglewf,progname, nmldeffile)

deallocate(master, acc, klist, kij, anglewf)

end program ECPIndexing 

!--------------------------------------------------------------------------------------------------------
!
! SUBROUTINE:MasterSubroutine
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Master subroutine to control dictionary generation, inner products computations, sorting values
!> and indexing of points, all in parallel using OpenCL/openMP
!
!> @param ecpnl
!> @param master
!> @param acc
!> @param 
!
!> @date 12/28/15  SS 1.0 original
!> @date 01/29/16 MDG 1.1 moved exp. pattern reading out of major loop for speedup
!> @date 01/30/16 MDG 1.2 no need to flip the patterns so removed some arrays
!> @date 01/30/16 MDG 1.3 use master thread to control GPU; loop simplifications to avoid code duplication
!> @date 01/31/16 MDG 1.4 added devid parameter to select the opencl device
!> @date 03/08/16 MDG 1.5 converted OpenCL calls to clfortran library
!> @date 03/16/17 SS  1.6 adding tmpfile as argument; adding h5 file as output; improving ctf write routine
!> @date 11/13/17 MDG 2.0 moved OpenCL code from InnerProdGPU routine to main code
!----------------------------------------------------------------------------------------------------------
subroutine MasterSubroutine(ecpnl, acc, master, kij, klist, anglewf, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use dictmod
use others, only: SSORT
use crystal
use initializers
use gvectors
use io
use error
use diffraction
use symmetry
use quaternions
use constants
use rotations
use so3
use math
use filters
use ECPmod
use clfortran
use CLsupport
use distortion
use omp_lib
use Indexingmod
use HDF5
use HDFsupport
use ECPiomod
use ISO_C_BINDING


IMPLICIT NONE

character(fnlen),INTENT(IN)                        :: progname
character(fnlen),INTENT(IN)                        :: nmldeffile
type(ECPIndexingNameListType),INTENT(INOUT)        :: ecpnl
type(ECPLargeAccumType),pointer,INTENT(IN)         :: acc
type(ECPMasterType),pointer,INTENT(IN)             :: master
integer(kind=irg),INTENT(IN)                       :: kij(2,ecpnl%npix**2)
real(kind=sgl),INTENT(IN)                          :: klist(3,ecpnl%npix**2)
real(kind=sgl),INTENT(IN)                          :: anglewf(nint((ecpnl%thetac)+abs(ecpnl%sampletilt))+1)


integer(c_intptr_t),allocatable, target            :: platform(:)
integer(c_intptr_t),allocatable, target            :: device(:)
integer(c_intptr_t),target                         :: context
integer(c_intptr_t),target                         :: command_queue
integer(c_intptr_t),target                         :: cl_expt,cl_dict
character(len = 50000), target                     :: source
integer(kind=irg), parameter                       :: source_length = 50000
integer(kind=irg), target                          :: source_l
character(len=source_length, KIND=c_char),TARGET   :: csource
type(c_ptr), target                                :: psource
integer(c_int32_t)                                 :: ierr, ierr2, pcnt
integer(c_intptr_t),target                         :: prog
integer(c_intptr_t),target                         :: kernel
integer(c_size_t)                                  :: cnum
character(9),target                                :: kernelname
character(10, KIND=c_char),target                  :: ckernelname

integer(kind=irg)                                  :: i, j, ii, iii, jj, pp, kk, ll ! loop variables
integer(kind=irg)                                  :: FZcnt, pgnum, io_int(3), ncubochoric, istat, itmpexpt = 43, cratio, &
                                                      fratio, cratioE, fratioE
integer(kind=irg)                                  :: Ne, Nd, L, totnumexpt, imght, imgwd, nnk, recordsize, correctsize, curdict,&
                                                      recordsize_correct
integer(kind=8)                                    :: size_in_bytes_dict, size_in_bytes_expt
real(kind=sgl)                                     :: dmin, voltage, projweight, qu(4), mi, ma, io_real(1), ratio, ratioE
real(kind=sgl),allocatable                         :: FZarray(:,:)
logical                                            :: verbose, f_exists
character                                          :: TAB
type(FZpointd),pointer                             :: FZlist, FZtmp
real(kind=sgl),pointer                             :: dict(:), T0dict(:)
real(kind=sgl),allocatable,TARGET                  :: dict1(:), dict2(:)
real(kind=sgl),allocatable,TARGET                  :: expt(:), dicttranspose(:), results(:), resultarray(:), eulerarray(:,:), &
                                                      resultmain(:,:), resulttmp(:,:)
real(kind=sgl),allocatable                         :: mask(:,:),masklin(:)
real(kind=sgl),allocatable                         :: imageexpt(:), tmpimageexpt(:), imagedict(:)
real(kind=sgl),allocatable                         :: meandict(:), meanexpt(:), ECPattern(:,:)
integer(kind=irg),allocatable                      :: indexlist(:), indexarray(:), indexmain(:,:), indextmp(:,:),  &
                                                      ppend(:), ppendE(:)
real(kind=sgl),allocatable                         :: ECPpattern(:,:), ECPpatternintd(:,:),ECP1D(:)
integer(kind=irg),allocatable                      :: ECPpatterninteger(:,:), ECPpatternad(:,:)
integer(c_size_t),target                           :: slength
integer(c_int)                                     :: numd, nump

complex(kind=dbl)                                  :: D

character(fnlen)                                   :: info ! info about the GPU
integer(kind=irg)                                  :: irec, iunit, num

integer,parameter                                  :: iunitexpt = 42

character(fnlen)                                   :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10, clname, ename, sourcefile
real(kind=sgl)                                     :: euler(3)
integer(kind=irg)                                  :: index, TID
integer(kind=irg)                                  :: ipar(10), hdferr

character(11)                                      :: dstr
character(15)                                      :: tstrb
character(15)                                      :: tstre
real(kind=sgl)                                     :: tstart


interface
     recursive subroutine CalcECPatternSingle(ecpnl, qu, anglewf, master, kij, klist, ECPattern)

       use error
       use typedefs
       use NameListTypedefs
       use ECPmod
       use quaternions
       use Lambert

       IMPLICIT NONE

       type(ECPIndexingNameListType),INTENT(IN)        :: ecpnl
       real(kind=sgl),INTENT(IN)                       :: qu(4)
       real(kind=sgl),INTENT(IN)                       :: anglewf(nint((ecpnl%thetac) + abs(ecpnl%sampletilt)) + 1)
       type(ECPMasterType),pointer                     :: master
       integer(kind=irg),INTENT(IN)                    :: kij(2,ecpnl%npix**2)
       real(kind=sgl),INTENT(IN)                       :: klist(3,ecpnl%npix**2)
       real(kind=sgl),INTENT(OUT)                      :: ECPattern(1:ecpnl%npix,1:ecpnl%npix)
        
     end subroutine CalcECPatternSingle
end interface


call timestamp(datestring=dstr, timestring=tstrb)
call cpu_time(tstart)

!=====================================================
! INITIALIZATION OF VARIOUS PARAMETERS
!=====================================================
TAB = CHAR(9)
iunit = 20
verbose =  .FALSE. ! .TRUE.
Ne = ecpnl%numexptsingle
Nd = ecpnl%numdictsingle
totnumexpt = ecpnl%totnumexpt
imght = ecpnl%npix
imgwd = ecpnl%npix
L = imght*imgwd
nnk = ecpnl%nnk
dmin = ecpnl%dmin
voltage = ecpnl%EkeV
ncubochoric = ecpnl%ncubochoric
recordsize = L*4
source_l = source_length


! nullify the dict  and T0dict pointers
nullify(dict,T0dict)

!D = dcmplx(0.000001,-0.00000185)

if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if

size_in_bytes_dict = Nd*correctsize*sizeof(dict(1))
size_in_bytes_expt = Ne*correctsize*sizeof(expt(1))
recordsize_correct = correctsize*4


!=====================================================
! EXTRACT POINT GROUP NUMBER FROM CRYSTAL STRUCTURE FILE 
!=====================================================
pgnum = GetPointGroup(ecpnl%MCxtalname,.FALSE.)

!=====================================================
! SAMPLING OF RODRIGUES FUNDAMENTAL ZONE
!=====================================================
nullify(FZlist)
FZcnt = 0
io_int(1) = pgnum
io_int(2) = ncubochoric
call WriteValue('Point group number and number of cubochoric sampling points : ',io_int,2,"(I4,',',I5)")

call sampleRFZ(ncubochoric, pgnum, 0, FZcnt, FZlist)

! allocate and fill FZarray for OpenMP parallelization
allocate(FZarray(4,FZcnt),stat=istat)
FZarray = 0.0

FZtmp => FZlist

do i = 1,FZcnt
    FZarray(1:4,i) = FZtmp%rod(1:4)
    FZtmp => FZtmp%next
end do

io_int(1) = FZcnt
call WriteValue(' Number of dictionary patterns to consider : ', io_int, 1, "(I8)")

!=========================================
! ALLOCATION AND INITIALIZATION OF ARRAYS
!=========================================

allocate(expt(Ne*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for experimental patterns'
expt = 0.0

! allocate the two dict arrays dict1 and dict2 and point dict to dict1
allocate(dict1(Nd*correctsize),dict2(Nd*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate dict1 and dict2 arrays for dictionary patterns'
dict1 = 0.0
dict2 = 0.0
dict => dict1
curdict = 1

allocate(dicttranspose(Nd*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for dictionary patterns'
dicttranspose = 0.0

allocate(results(Ne*Nd),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for results of dot product'
results = 0.0

allocate(imageexpt(L),tmpimageexpt(correctsize),imagedict(correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for reading experimental image patterns'
imageexpt = 0.0
tmpimageexpt = 0.0
imagedict = 0.0

allocate(meandict(correctsize),meanexpt(correctsize),mask(imght,imgwd),masklin(L),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for mean dictionary and experimental patterns'
meandict = 0.0
meanexpt = 0.0
mask = 1.0
masklin = 0.0

allocate(ECPattern(1:ecpnl%npix,1:ecpnl%npix))
if (istat .ne. 0) stop 'Could not allocate array for channeling pattern'
ECPattern = 0.0

allocate(resultarray(Nd),stat=istat)
if (istat .ne. 0) stop 'Could not allocate result arrays'
resultarray = 0.0

allocate(indexarray(1:Nd),stat=istat)
if (istat .ne. 0) stop 'Could not allocate index arrays'
indexarray = 0

allocate(indexlist(1:Nd*(ceiling(float(FZcnt)/float(Nd)))),stat=istat)
if (istat .ne. 0) stop 'Could not allocate indexlist arrays'
indexlist = (/ (i,i = 1,Nd*ceiling(float(FZcnt)/float(Nd))) /)

allocate(resultmain(nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'Could not allocate main result array'
resultmain = 0.0

allocate(indexmain(nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'Could not allocate main index array'
indexmain = 0

allocate(resulttmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'Could not allocate temporary result array'
resulttmp = 0.0

allocate(indextmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'Could not allocate temporary index array'
indextmp = 0

allocate(eulerarray(1:3,Nd*ceiling(float(FZcnt)/float(Nd))),stat=istat)
if (istat .ne. 0) stop 'Could not allocate euler array'
eulerarray = 0.0

allocate(ECPpatternintd(1:ecpnl%npix,1:ecpnl%npix),&
ECP1D(ecpnl%npix*ecpnl%npix),&
ECPpattern(1:ecpnl%npix,1:ecpnl%npix),&
ECPpatterninteger(1:ecpnl%npix,1:ecpnl%npix),&
ECPpatternad(1:ecpnl%npix,1:ecpnl%npix),stat=istat)

ECPpatternintd = 0.0
ECPpatterninteger = 0
ECPpatternad = 0

!==========================
! INITIALIZATION OF DEVICE
!==========================
call CLinit_PDCCQ(platform, nump, ecpnl%platid, device, numd, ecpnl%devid, info, context, command_queue)

! read the source file
sourcefile = 'DictIndx.cl'
call CLread_source_file(sourcefile, csource, slength)

! allocate device memory
cl_expt = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_expt, C_NULL_PTR, ierr)
if(ierr /= CL_SUCCESS) stop 'Error: cannot allocate device memory for experimental data.'

cl_dict = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_dict, C_NULL_PTR, ierr)
if(ierr /= CL_SUCCESS) stop 'Error: cannot allocate device memory for dictionary data.'

!================================
! the following lines were originally in the InnerProdGPU routine, but there is no need
! to execute them each time that routine is called so we move them here...
!================================
! create the program
pcnt = 1
psource = C_LOC(csource)
!prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(source_l), ierr)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('InnerProdGPU:clCreateProgramWithSource', ierr)

! build the program
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(ecpnl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
! if(cnum > 1) call Message(trim(source(1:cnum))//'test',frm='(A)')
call CLerror_check('InnerProdGPU:clBuildProgram', ierr)
call CLerror_check('InnerProdGPU:clGetProgramBuildInfo', ierr2)

! finally get the kernel and release the program
kernelname = 'InnerProd'
ckernelname = kernelname
ckernelname(10:10) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('InnerProdGPU:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('InnerProdGPU:clReleaseProgram', ierr)

! the remainder is done in the InnerProdGPU routine
!=========================================

!====================================================================================
! I/O FOR EXPERIMENTAL DATA SET AND APPLICATION OF MASK IF ANY
!====================================================================================

call Message(' -> opened experimental file for I/O', frm = "(A)" )

ename = trim(EMsoft_getEMdatapathname())//trim(ecpnl%exptfile)
ename = EMsoft_toNativePath(ename)
open(unit=iunitexpt,file=trim(ename),&
    status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

if (ecpnl%maskpattern.eq.'y') then
  do ii = 1,imght
      do jj = 1,imgwd
          if((ii-imght/2)**2 + (jj-imgwd/2)**2 .ge. ecpnl%maskradius**2) then
              mask(ii,jj) = 0.0
          end if
      end do
  end do
end if
  
do ii = 1,imght
    do jj = 1,imgwd
        masklin((ii-1)*imgwd+jj) = mask(ii,jj)
    end do
end do

!=========================================
! determine loop variables to avoid having to duplicate large sections of mostly identical code
!=========================================
ratio = float(FZcnt)/float(Nd)
cratio = ceiling(ratio)
fratio = floor(ratio)

ratioE = float(totnumexpt)/float(Ne)
cratioE = ceiling(ratioE)
fratioE = floor(ratioE)

allocate(ppend(cratio),ppendE(cratioE))
ppend = (/ (Nd, i=1,cratio) /)
if (fratio.lt.cratio) then
  ppend(cratio) = MODULO(FZcnt,Nd)
end if

ppendE = (/ (Ne, i=1,cratioE) /)
if (fratioE.lt.cratioE) then
  ppendE(cratioE) = MODULO(totnumexpt,Ne)
end if


!=========================================
! PREPARE THE EXPERIMENTAL PATTERNS
!=========================================
! in this section, we read all the experimental patterns
! and apply adaptive histogram equalization, masking, and
! whatever other filters we like to apply; then we store
! those patterns in a temporary file from which they will 
! be read when needed.

! first, make sure that this file does not already exist
f_exists = .FALSE.
ename = trim(EMsoft_getEMdatapathname())//trim(ecpnl%tmpfile)
ename = EMsoft_toNativePath(ename)
inquire(file=trim(ename), exist=f_exists)

if (f_exists) then
  open(unit=itmpexpt,file=trim(ename),&
      status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
  close(unit=itmpexpt,status='delete')
end if

! open the temporary file
open(unit=itmpexpt,file=trim(ename),&
    status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)

prepexperimentalloop: do jj = 1,totnumexpt
    tmpimageexpt = 0.0
    read(iunitexpt,rec=jj) imageexpt

! convert imageexpt to 2D ECPattern array
    do kk=1,ecpnl%npix
      ECPattern(kk,1:ecpnl%npix) = imageexpt((kk-1)*ecpnl%npix+1:kk*ecpnl%npix) 
    end do

! adaptive histogram equalization
    ma = maxval(imageexpt)
    mi = minval(imageexpt)

    ECPpatternintd = ((ECPattern - mi)/ (ma-mi))
    ECPpatterninteger = nint(ECPpatternintd*255.0)
    ECPpatternad =  adhisteq(ecpnl%nregions,ecpnl%npix,ecpnl%npix,ECPpatterninteger)
    ECPattern = float(ECPpatternad)

! convert back to 1D vector
    do kk=1,ecpnl%npix
      imageexpt((kk-1)*ecpnl%npix+1:kk*ecpnl%npix) = ECPattern(kk,1:ecpnl%npix)
    end do

    imageexpt(1:L) = imageexpt(1:L)*masklin(1:L)
    tmpimageexpt(1:L) = imageexpt(1:L)
!   projweight = DOT_PRODUCT(imageexpt,meanexpt)
!   tmpimageexpt(1:L) = imageexpt - projweight*meanexpt(1:L)
    tmpimageexpt = tmpimageexpt/NORM2(tmpimageexpt)

! and write this pattern into the temporary file
    write(itmpexpt,rec=jj) tmpimageexpt
end do prepexperimentalloop

close(unit=iunitexpt,status='keep')
! we keep the temporary file open since we will be reading from it...

!=========================================
!=========================================
!=========================================
! MAIN COMPUTATION LOOP
! this is a complicated loop since we want to have the GPU computations 
! to occur at the same time as the next batch of dictionary patterns is computed.
! We can do this if we use two dict arrays in an alternating fashion, and we 
! flip pointers back and forth so that the GPU works on one dict array while
! the threads compute the other one.  So we make dict a pointer to the targets
! dict1 and dict2 and simply alternate at the start of the main loop. The other
! pointer, T0dict (dict for thread 0) points to the other array.  We also put
! OMP barrier statements in to ensure that the threads wait for each other at
! the end of each cycle.  Finally, we must remember to add one more GPU cycle
! at the end of the dictionaryloop to make sure that we process all the dict patterns.
!=========================================
!=========================================
!=========================================

call timestamp()


! set the correct number of threads
call OMP_SET_NUM_THREADS(ecpnl%nthreads)
io_int(1) = ecpnl%nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

! start the main dictionary loop
dictionaryloop: do ii = 1,cratio+1      ! +1 is needed to make sure that the final GPU run is carried out
    results = 0.0
! if ii is odd, then we use dict1 for the dictionary computation, and dict2 for the GPU
! (assuming ii>1); when ii is even we switch the two pointers 
    if (mod(ii,2).eq.1) then
      dict => dict1
      dict1 = 0.0
      T0dict => dict2   ! these are the patterns to be sent to the GPU
      curdict = 2
      if (verbose.eqv..TRUE.) call WriteValue('','dict => dict1; T0dict => dict2')
    else
      dict => dict2
      dict2 = 0.0
      T0dict => dict1   ! these are the patterns to be sent to the GPU
      curdict = 1
      if (verbose.eqv..TRUE.) call WriteValue('','dict => dict2; T0dict => dict1')
    end if

    if (verbose.eqv..TRUE.) then
      io_int(1) = ii
      call WriteValue('Dictionaryloop index = ',io_int,1)
    end if

!$OMP PARALLEL DEFAULT(PRIVATE) &
!$OMP& SHARED(FZarray,ecpnl,anglewf,master,kij,klist,ii,meandict,Nd,imght,imgwd,correctsize,D,eulerarray,masklin) &
!$OMP& SHARED(dict1,dict2,T0dict,dict,curdict,expt,dicttranspose,command_queue,cl_dict,size_in_bytes_dict,ierr,Ne) &
!$OMP& SHARED(itmpexpt,totnumexpt,tmpimageexpt,cl_expt,size_in_bytes_expt,results,source,slength,platform,device) &
!$OMP& SHARED(context,io_real,resultarray,indexarray,resulttmp,indextmp,resultmain,indexmain,nnk,indexlist) &
!$OMP& SHARED(cratio,fratio,ppend,verbose,cratioE,ppendE,nump,numd,csource)

    TID = OMP_GET_THREAD_NUM()

! the master thread should be the one working on the GPU computation
!$OMP MASTER
    if (ii.gt.1) then
      iii = ii-1        ! the index ii is already one ahead, since the GPU thread lags one cycle behind the others...
      if (verbose.eqv..TRUE.) then 
        if (associated(T0dict,dict1)) then 
          write(*,"('   GPU thread is working on dict1')")
        else
          write(*,"('   GPU thread is working on dict2')")
        end if
      end if
!--CHECK THIS !!!--------------------------------
! Saransh, we do not really need to transpose this, since the cl routine then transposes again...

      dicttranspose = 0.0

      do kk = 1,correctsize
        do ll = 1,Nd
            dicttranspose((kk-1)*Nd+ll) = T0dict((ll-1)*correctsize+kk)
        end do
      end do
!------------------------------------------------

      ierr = clEnqueueWriteBuffer(command_queue, cl_dict, CL_TRUE, 0_8, size_in_bytes_dict, C_LOC(dicttranspose(1)), &
                                  0, C_NULL_PTR, C_NULL_PTR)
      if(ierr /= CL_SUCCESS) call FatalError('clEnqueueWriteBuffer:','cannot write to dicttranspose buffer.')

!------------------------------------------------
! this loop is now faster since we have stored the modified experimental patterns in a temporary file
      experimentalloop: do jj = 1,cratioE
        
        expt = 0.0 
        tmpimageexpt = 0.0

        do pp = 1,ppendE(jj)
          read(itmpexpt,rec=(jj-1)*Ne+pp) tmpimageexpt
          expt((pp-1)*correctsize+1:pp*correctsize) = tmpimageexpt
        end do

        ierr = clEnqueueWriteBuffer(command_queue, cl_expt, CL_TRUE, 0_8, size_in_bytes_expt, C_LOC(expt(1)), &
                                    0, C_NULL_PTR, C_NULL_PTR)
        if(ierr /= CL_SUCCESS) call FatalError('clEnqueueWriteBuffer','cannot write to expt buffer.')

        call InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,ecpnl%devid,kernel,context,command_queue)

        do pp = 1,ppendE(jj)

            resultarray(1:Nd) = results((pp-1)*Nd+1:pp*Nd)
            indexarray(1:Nd) = indexlist((iii-1)*Nd+1:iii*Nd)

            call SSORT(resultarray,indexarray,Nd,-2)
            resulttmp(nnk+1:2*nnk,(jj-1)*Ne+pp) = resultarray(1:nnk)
            indextmp(nnk+1:2*nnk,(jj-1)*Ne+pp) = indexarray(1:nnk)

            call SSORT(resulttmp(:,(jj-1)*Ne+pp),indextmp(:,(jj-1)*Ne+pp),2*nnk,-2)

            resultmain(1:nnk,(jj-1)*Ne+pp) = resulttmp(1:nnk,(jj-1)*Ne+pp)

            indexmain(1:nnk,(jj-1)*Ne+pp) = indextmp(1:nnk,(jj-1)*Ne+pp)

        end do
        io_real(1) = maxval(results)
        call WriteValue('Max(results) = ',io_real,1)

      end do experimentalloop
   
      if (mod(iii,25) .eq. 0) then
        io_int(1:2) = (/iii,cratio/)
        call WriteValue('',io_int,2,"(' -> Completed cycle ',I5,' out of ',I5)")
      end if
    else
       if (verbose.eqv..TRUE.) call WriteValue('','        GPU thread is idling')
    end if  ! ii.gt.1
!$OMP END MASTER 

! here we carry out the dictionary pattern computation, unless we are in the ii=cratio+1 step
    if (ii.lt.cratio+1) then
     if (verbose.eqv..TRUE.) then
       if (associated(dict,dict1)) then 
         write(*,"('    Thread ',I2,' is working on dict1')") TID
       else
         write(*,"('    Thread ',I2,' is working on dict2')") TID
       end if
     end if

!$OMP DO SCHEDULE(DYNAMIC)
        do pp = 1, ppend(ii)
            qu = ro2qu(FZarray(:,(ii-1)*Nd+pp))
            call CalcECPatternSingle(ecpnl, qu, anglewf, master, kij, klist, ECPattern)

            !call BarrelDistortion(D,ECPattern,imght,imgwd)

! adaptive histogram equalization
            ma = maxval(ECPattern)
            mi = minval(ECPattern)

            ECPpatternintd = ((ECPattern - mi)/ (ma-mi))
            ECPpatterninteger = nint(ECPpatternintd*255.0)
            ECPpatternad =  adhisteq(ecpnl%nregions,ecpnl%npix,ecpnl%npix,ECPpatterninteger)
            ECPattern = float(ECPpatternad)

            do kk = 1, imght
                do ll = 1, imgwd
                    imagedict((kk-1)*imgwd+ll) = ECPattern(ll,kk)
                end do
            end do

            imagedict(1:L) = imagedict(1:L) * masklin(1:L)

! normalize pattern vector
            dict((pp-1)*correctsize+1:pp*correctsize) = imagedict(1:correctsize)/NORM2(imagedict(1:correctsize))
! dot product with mean dictionary pattern (not used at the moment)
!           projweight = DOT_PRODUCT(meandict,dict((pp-1)*correctsize+1:pp*correctsize))
! project out the mean dictionary pattern
!           dict((pp-1)*correctsize+1:pp*correctsize) = dict((pp-1)*correctsize+1:pp*correctsize) - projweight*meandict
! normalize the resulting pattern
!           dict((pp-1)*correctsize+1:pp*correctsize) = dict((pp-1)*correctsize+1:pp*correctsize)&
!           /NORM2(dict((pp-1)*correctsize+1:pp*correctsize))    

            eulerarray(1:3,(ii-1)*Nd+pp) = 180.0/cPi*ro2eu(FZarray(1:4,(ii-1)*Nd+pp))
        end do
!$OMP END DO 
        if (verbose.eqv..TRUE.) then
          io_int(1) = TID
          call WriteValue('',io_int,1,"('       Thread ',I2,' is done')")
        end if
     else
        if (verbose.eqv..TRUE.) then
          io_int(1) = TID
          call WriteValue('',io_int,1,"('       Thread ',I2,' idling')")
        end if
     end if
! and we end the parallel section here (all threads will synchronize).
!$OMP END PARALLEL

end do dictionaryloop

call timestamp()

! remove the temporary file
close(unit=itmpexpt,status='delete')

! release the OpenCL kernel
ierr = clReleaseKernel(kernel)
call CLerror_check('InnerProdGPU:clReleaseKernel', ierr)


! ===================
! MAIN OUTPUT SECTION
! ===================

! fill the ipar array with integer parameters that are needed to write the h5ebsd file
! (anything other than what is already in the ebsdnl structure)
ipar = 0
ipar(1) = nnk
ipar(2) = Ne*ceiling(float(totnumexpt)/float(Ne))
ipar(3) = totnumexpt
ipar(4) = Nd*ceiling(float(FZcnt)/float(Nd))
ipar(5) = FZcnt
ipar(6) = pgnum

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

if (ecpnl%datafile.ne.'undefined') then 
  call h5ecp_writeFile(ecpnl, dstr, tstrb, ipar, resultmain, indexmain, eulerarray, &
                       progname, nmldeffile)
  call Message('Data stored in h5ebsd file : '//trim(ecpnl%datafile))
end if


if (ecpnl%ctffile.ne.'undefined') then 
    call ctfecp_writeFile(ecpnl,ipar,indexmain,eulerarray,resultmain)
    call Message('Data stored in ctf file : '//trim(ecpnl%ctffile))
end if

end subroutine MasterSubroutine

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!
! SUBROUTINE:CalcECPatternSingle
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Calculate a single EC pattern for a given euler angle
!
!> @param ecpnl namelist file for ebsd
!> @param qu quaternion for the pattern
!> @param acc accumulator array from MC simulation
!> @param master master pattern
!> @param ECPattern output array
!
!> @date 05/07/15  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CalcECPatternSingle(ecpnl, qu, anglewf, master, kij, klist, ECPattern)

use error
use typedefs
use NameListTypedefs
use ECPmod
use quaternions
use Lambert

IMPLICIT NONE

type(ECPIndexingNameListType),INTENT(IN)        :: ecpnl
real(kind=sgl),INTENT(IN)                       :: qu(4)
real(kind=sgl),INTENT(IN)                       :: anglewf(nint((ecpnl%thetac) + abs(ecpnl%sampletilt)) + 1)
type(ECPMasterType),pointer                     :: master
integer(kind=irg),INTENT(IN)                    :: kij(2,ecpnl%npix**2)
real(kind=sgl),INTENT(IN)                       :: klist(3,ecpnl%npix**2)
real(kind=sgl),INTENT(OUT)                      :: ECPattern(1:ecpnl%npix,1:ecpnl%npix)

real(kind=dbl),parameter                        :: Rtod = 57.2957795131D0
real(kind=dbl),parameter                        :: dtoR = 0.01745329251D0
integer(kind=irg)                               :: numk, idir, isig, isigp, nsig, istat
real(kind=sgl)                                  :: dc(3), dp, MCangle, scl, ixy(2)
real(kind=sgl)                                  :: wf, dx, dy, dxm, dym
integer(kind=irg)                               :: nix, niy, nixp, niyp, ipx, ipy

numk = ecpnl%npix**2
nsig = nint((ecpnl%thetac) + abs(ecpnl%sampletilt)) + 1
scl = dble(ecpnl%npx)

do idir = 1,numk

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
    dc = dc/sqrt(sum(dc*dc))

! convert these direction cosines to coordinates in the Rosca-Lambert projection
    call LambertgetInterpolation(dc, scl, ecpnl%npx, ecpnl%npy, nix, niy, nixp, niyp, dx, dy, dxm, dym)

! interpolate the intensity
    ipx = kij(1,idir)
    ipy = kij(2,idir)
        
! including the detector model with some sample tilt
    if (dc(3) .gt. 0.0) then 

        ECPattern(ipx,ipy) = wf * (master%mLPNH(nix,niy) * dxm * dym + &
                          master%mLPNH(nixp,niy) * dx * dym + &
                          master%mLPNH(nix,niyp) * dxm * dy + &
                          master%mLPNH(nixp,niyp) * dx * dy)

    else

        ECPattern(ipx,ipy) =  wf * (master%mLPSH(nix,niy) * dxm * dym + &
                          master%mLPSH(nixp,niy) * dx * dym + &
                          master%mLPSH(nix,niyp) * dxm * dy + &
                          master%mLPSH(nixp,niyp) * dx * dy)

    end if

end do 


end subroutine CalcECPatternSingle

