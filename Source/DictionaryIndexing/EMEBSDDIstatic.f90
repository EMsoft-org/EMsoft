! ###################################################################
! Copyright (c) 2015-2016, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDDIstatic.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDDIstatic
!
!> @author Saransh Singh/Marc De Graef, Carnegie Mellon University
!
!> @brief Indexing of EBSD patterns using a pre-calculated dictionary. 
!
!> @date 03/15/16 MDG 1.0 original/ forked from EMEBSDDI.f90
!> @date 11/14/16 MDG 1.1 continued reworking of code
!--------------------------------------------------------------------------

program EMEBSDDIstatic

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error
use initializers
use EBSDmod
use stringconstants

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(EBSDIndexingNameListType)              :: ebsdnl
logical                                     :: verbose
integer(kind=irg)                           :: istat, res

nmldeffile = 'EMEBSDDI.nml'
progname = 'EMEBSDDIstatic.f90'
progdesc = 'Program to index EBSD patterns using a pre-calculated pattern dictionary'
verbose = .TRUE.

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 80 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMEBSDDIstatic','JSON input not yet implemented')
!  call JSONreadEBSDIndexingNameList(ebsdnl, nmldeffile, error_cnt)
else
  call GetEBSDIndexingNameList(nmldeffile,ebsdnl)
end if

! perform the dictionary indexing computations
call MasterSubroutine(ebsdnl,progname, nmldeffile)

end program EMEBSDDIstatic

!--------------------------------------------------------------------------
!
! SUBROUTINE:MasterSubroutine
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Master subroutine to control inner product computations, sorting values
!> and indexing of points, all in parallel using OpenCL/openMP
!
!> @param ebsdnl ped indexing namelist pointer
!> @param progname name of the program
!> @param nmldeffile namelist filename
!
!> @date 03/30/15  SS 1.0 original
!> @date 05/05/15 MDG 1.1 removed getenv() call; replaced by global path strings
!> @date 02/07/16 MDG 1.2 added image quality computation
!> @date 02/08/16 MDG 1.3 added confidence index output to HDF5 file
!> @date 02/10/16 MDG 1.4 added average dot product map to HDF5 file
!> @date 03/15/16 MDG 1.5 converted from EMEBSDDI.f90 code; removed all pattern calculation parts
!> @date 11/14/16 MDG 2.0 reworked code to enable multiple GPU indexing
!> @date 11/13/17 MDG 2.1 moved OpenCL code from InnerProdGPU routine to main code
!--------------------------------------------------------------------------

subroutine MasterSubroutine(ebsdnl,progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use dictmod
use Lambert
use others
use crystal
use initializersHDF
use gvectors
use filters
use error
use io
use diffraction
use symmetry
use quaternions
use constants
use rotations
use so3
use math
use EBSDmod
use EBSDDImod
use clfortran
use CLsupport
use omp_lib
use HDF5
use h5im
use h5lt
use HDFsupport
use EMh5ebsd
use EBSDiomod
use NameListHDFwriters
use ECPmod, only: GetPointGroup
use Indexingmod
use ISO_C_BINDING
use stringconstants

IMPLICIT NONE

type(EBSDIndexingNameListType),INTENT(INOUT)        :: ebsdnl
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

type(EBSDNameListType)                              :: ebsdnl2
type(unitcell),pointer                              :: cell
type(DynType)                                       :: Dyn
type(gnode)                                         :: rlp
logical                                             :: verbose

integer(c_intptr_t),allocatable, target             :: platform(:)
integer(c_intptr_t),allocatable, target             :: device(:)
integer(c_intptr_t),target                          :: context
integer(c_intptr_t),target                          :: command_queue
integer(c_intptr_t),target                          :: cl_expt,cl_dict
character(len = 50000), target                      :: source
integer(kind=irg), parameter                        :: source_length = 50000
integer(kind=irg), target                           :: source_l
character(len=source_length, KIND=c_char),TARGET    :: csource
type(c_ptr), target                                 :: psource
integer(c_int32_t)                                  :: ierr2, pcnt
integer(c_intptr_t),target                          :: prog
integer(c_intptr_t),target                          :: kernel
integer(c_size_t)                                   :: cnum
character(9),target                                 :: kernelname
character(10, KIND=c_char),target                   :: ckernelname

integer(kind=irg)                                   :: num,ierr,irec,istat
integer(kind=irg),parameter                         :: iunit = 40
integer(kind=irg),parameter                         :: iunitexpt = 41
integer(kind=irg),parameter                         :: iunitdict = 42
character(fnlen)                                    :: info ! info about the GPU
real(kind=dbl),parameter                            :: nAmpere = 6.241D+18   ! Coulomb per second

integer(kind=irg)                                   :: Ne,Nd,L,totnumexpt,numdictsingle,numexptsingle,imght,imgwd,nnk, &
                                                       recordsize, fratio, cratio, fratioE, cratioE, iii, itmpexpt, hdferr,&
                                                       recordsize_correct
integer(kind=8)                                     :: size_in_bytes_dict,size_in_bytes_expt
real(kind=sgl),pointer                              :: dict(:), T0dict(:)
real(kind=sgl),allocatable,TARGET                   :: dict1(:), dict2(:)
!integer(kind=1),allocatable                         :: imageexpt(:),imagedict(:)
real(kind=sgl),allocatable                          :: imageexpt(:),imagedict(:), mask(:,:),masklin(:), exptIQ(:), &
                                                       exptCI(:), exptFit(:)
real(kind=sgl),allocatable                          :: imageexptflt(:),binned(:,:),imagedictflt(:),imagedictfltflip(:), &
                                                       tmpimageexpt(:)
real(kind=sgl),allocatable, target                  :: results(:),expt(:),dicttranspose(:),resultarray(:),&
                                                       eulerarray(:,:),eulerarray2(:,:),resultmain(:,:),resulttmp(:,:)
integer(kind=irg),allocatable                       :: accum_e_MC(:,:,:),acc_array(:,:), ppend(:), ppendE(:) 
integer*4,allocatable                               :: idpmap(:),iexptCI(:,:), iexptIQ(:,:)
real(kind=sgl),allocatable                          :: meandict(:),meanexpt(:),wf(:),mLPNH(:,:,:),mLPSH(:,:,:)
real(kind=sgl),allocatable                          :: mLPNH_simple(:,:), mLPSH_simple(:,:), eangle(:)
real(kind=sgl),allocatable                          :: EBSDpattern(:,:), FZarray(:,:), dpmap(:), lstore(:,:), pstore(:,:)
real(kind=sgl),allocatable                          :: EBSDpatternintd(:,:), lp(:), cp(:)
integer(kind=irg),allocatable                       :: EBSDpatterninteger(:,:), EBSDpatternad(:,:)

character(kind=c_char),allocatable                  :: EBSDdictpat(:,:,:)

real(kind=dbl),allocatable                          :: rdata(:,:), fdata(:,:)
integer(kind=irg)                                   :: dims(2), w
character(11)                                       :: dstr
character(15)                                       :: tstrb
character(15)                                       :: tstre
character(3)                                        :: vendor
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, fname, clname, ename, sourcefile, dictfile, outline, &
                                                        datagroupname
integer(hsize_t)                                    :: expwidth, expheight
integer(hsize_t),allocatable                        :: iPhase(:), iValid(:)
integer(c_size_t),target                            :: slength
integer(c_int)                                      :: numd, nump
integer(HSIZE_T)                                    :: dims2(2), dims3(3), offset3(3)

integer(kind=irg)                                   :: i,j,ii,jj,kk,ll,mm,pp,qq
integer(kind=irg)                                   :: FZcnt, pgnum, io_int(3), ncubochoric, nlines
type(FZpointd),pointer                              :: FZlist, FZtmp
integer(kind=irg),allocatable                       :: indexlist(:),indexarray(:),indexmain(:,:),indextmp(:,:)
real(kind=sgl)                                      :: dmin,voltage,scl,prefactor,ratio, mi, ma, ratioE, io_real(2), tstart, &
                                                       tstop, tmp
character(fnlen)                                    :: xtalname
integer(kind=irg)                                   :: binx,biny,TID,totnum_el,nthreads,Emin,Emax
real(kind=sgl)                                      :: sx,dx,dxm,dy,dym,rhos,x,projweight, dp
real(kind=sgl)                                      :: dc(3),quat(4),ixy(2),bindx
integer(kind=irg)                                   :: nix,niy,nixp,niyp
real(kind=sgl)                                      :: euler(3)
integer(kind=irg)                                   :: indx
integer(kind=irg)                                   :: correctsize
logical                                             :: f_exists, init, holdexpt, holddict

integer(kind=irg)                                   :: ipar(10)

type(HDFobjectStackType),pointer                    :: HDF_head

call timestamp(datestring=dstr, timestring=tstrb)
call cpu_time(tstart)
source_l = source_length

!=====================================================
!=====================================================
! in a later version, we will need to do things this way...
!
! setting up the variables will depend on the available 
! RAM for this run, described in the ebsdnl%sysmem parameter.
! First, we determine the sizes of the experimental and 
! dictionary pattern arrays and figure out if we can keep
! it all in memory (fastest); if not, we'll need to make a 
! choice on which datasets to keep in memory (slower) or none
! at all (slowest).  
! call get_EBSDDI_memory_pattern(ebsdnl, holdexpt, holddict)

! then we also need to figure out how much memory the GPU has,
! so that we can determine how many patterns to send in each step.
!=====================================================
!=====================================================

!
! Initialize FORTRAN interface.
CALL h5open_EMsoft(hdferr)

! fills out the EBSD Indexing name list by read dictionary H5 file
call FillEBSDIndexingNameList(ebsdnl,hdferr)

call Message('-->  '//'Finished filling the namelist structure')

! get the full filename
dictfile = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%dictfile)
dictfile = EMsoft_toNativePath(dictfile)

call Message('-->  '//'Opening HDF5 dictionary file '//trim(ebsdnl%dictfile))
nullify(HDF_head)


hdferr =  HDF_openFile(dictfile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openFile ')
   
! then read some more data from the EMData group
groupname = SC_EMData
hdferr = HDF_openGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup:EMData')

datagroupname = 'EBSD'
hdferr = HDF_openGroup(datagroupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_openGroup:EBSD')

! number of Eulerangles numangles
dataset = SC_numangles
call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetInteger:numangles')

! euler angle list Eulerangles
dataset = SC_Eulerangles
call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, eulerarray2)
if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_readDatasetFloatArray2D:Eulerangles')

! we leave this file open since we still need to read all the patterns...
!=====================================================

!=====================================================
! copy namelist variables and compute array dimensions
verbose = .FALSE.
init = .TRUE.
Ne = ebsdnl%numexptsingle
Nd = ebsdnl%numdictsingle
L = ebsdnl%numsx*ebsdnl%numsy/ebsdnl%binning**2
totnumexpt = ebsdnl%ipf_wd*ebsdnl%ipf_ht
imght = ebsdnl%numsx/ebsdnl%binning
imgwd =  ebsdnl%numsy/ebsdnl%binning
binx = imght
biny = imgwd
nnk = ebsdnl%nnk
xtalname = ebsdnl%MCxtalname
! ncubochoric = ebsdnl%ncubochoric  (we're reading euler angles from file, so no need to create a new list)
recordsize = L*4
itmpexpt = 43
dims = (/imght, imgwd/)
w = 2
pgnum = GetPointGroup(xtalname,.FALSE.)

! these will need to be read from an experimental data file but we''l set
! defaults values here.
ebsdnl%WD = 10.0


! nullify the dict  and T0dict pointers
nullify(dict,T0dict)

! make sure that correctsize is a multiple of 16; if not, make it so
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if

! determine the experimental and dictionary sizes in bytes
size_in_bytes_dict = Nd*correctsize*sizeof(correctsize)
size_in_bytes_expt = Ne*correctsize*sizeof(correctsize)
recordsize_correct = correctsize*4

!================================
! INITIALIZATION OF OpenCL DEVICE
!================================
call CLinit_PDCCQ(platform, nump, ebsdnl%platid, device, numd, ebsdnl%devid, info, context, command_queue)

! read the cl source file
sourcefile = 'DictIndx.cl'
call CLread_source_file(sourcefile, csource, slength)

! allocate device memory for experimental and dictionary patterns
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
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(source_l), ierr)
call CLerror_check('InnerProdGPU:clCreateProgramWithSource', ierr)

! build the program
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(ebsdnl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
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

!=========================================
! ALLOCATION AND INITIALIZATION OF ARRAYS
!=========================================

allocate(expt(Ne*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for experimental patterns'
expt = 0.0

allocate(dict1(Nd*correctsize),dict2(Nd*correctsize),dicttranspose(Nd*correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for dictionary patterns'
dict1 = 0.0
dict2 = 0.0
dict => dict1
dicttranspose = 0.0

allocate(results(Ne*Nd),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for results'
results = 0.0

allocate(mask(binx,biny),masklin(L),stat=istat)
if (istat .ne. 0) stop 'Could not allocate arrays for masks'
mask = 1.0
masklin = 0.0

allocate(imageexpt(L),imageexptflt(correctsize),imagedictflt(correctsize),imagedictfltflip(correctsize),stat=istat)
allocate(tmpimageexpt(correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for reading experimental image patterns'
imageexpt = 0.0
imageexptflt = 0.0

allocate(meandict(correctsize),meanexpt(correctsize),imagedict(correctsize),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for mean dictionary and experimental patterns'
meandict = 0.0
meanexpt = 0.0

! allocate(EBSDpattern(ebsdnl%numsx,ebsdnl%numsy),binned(binx,biny),stat=istat)
allocate(EBSDpattern(binx,biny),binned(binx,biny),stat=istat)
if (istat .ne. 0) stop 'Could not allocate array for EBSD pattern'
EBSDpattern = 0.0

! all AHE arrays
allocate(EBSDpatternintd(binx,biny),EBSDpatterninteger(binx,biny),EBSDpatternad(binx,biny),stat=istat)
if(istat .ne. 0) stop 'Could not allocate arrays for AHE'
EBSDpatternintd = 0.0
EBSDpatterninteger = 0
EBSDpatternad = 0.0

allocate(resultarray(1:Nd),stat=istat)
if (istat .ne. 0) stop 'could not allocate result arrays'

resultarray = 0.0

allocate(indexarray(1:Nd),stat=istat)
if (istat .ne. 0) stop 'could not allocate index arrays'

indexarray = 0

allocate(indexlist(1:Nd*(ceiling(float(FZcnt)/float(Nd)))),stat=istat)
if (istat .ne. 0) stop 'could not allocate indexlist arrays'

indexlist = 0

do ii = 1,Nd*ceiling(float(FZcnt)/float(Nd))
    indexlist(ii) = ii
end do

allocate(resultmain(nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate main result array'

resultmain = -2.0

allocate(indexmain(nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate main index array'

indexmain = 0

allocate(resulttmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate temporary result array'

resulttmp = -2.0

allocate(indextmp(2*nnk,Ne*ceiling(float(totnumexpt)/float(Ne))),stat=istat)
if (istat .ne. 0) stop 'could not allocate temporary index array'

indextmp = 0

! eulerarray was read from the dictionary file so we comment out these lines
allocate(eulerarray(1:3,Nd*ceiling(float(FZcnt)/float(Nd))),stat=istat)
if (istat .ne. 0) stop 'could not allocate euler array'
eulerarray = 0.0
eulerarray(1:3,1:FZcnt) = eulerarray2(1:3,1:FZcnt)
deallocate(eulerarray2)

allocate(exptIQ(totnumexpt), exptCI(totnumexpt), exptFit(totnumexpt), stat=istat)
if (istat .ne. 0) stop 'could not allocate exptIQ array'

allocate(rdata(binx,biny),fdata(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate arrays for Hi-Pass filter'
rdata = 0.D0
fdata = 0.D0

!=====================================================
! determine loop variables to avoid having to duplicate 
! large sections of mostly identical code
!=====================================================
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


!=====================================================
! define the circular mask if necessary and convert to 1D vector
!=====================================================
if (ebsdnl%maskpattern.eq.'y') then
  do ii = 1,biny
      do jj = 1,binx
          if((ii-biny/2)**2 + (jj-binx/2)**2 .ge. ebsdnl%maskradius**2) then
              mask(ii,jj) = 0.0
          end if
      end do
  end do
end if
  
do ii = 1,biny
    do jj = 1,binx
        masklin((ii-1)*binx+jj) = mask(jj,ii)
    end do
end do
!=====================================================


!=====================================================
! Preprocess all the experimental patterns and store
! them in a temporary file as vectors; also, create 
! an average dot product map to be stored in the h5ebsd output file
!
! this could become a separate routine in the EMEBSDmod module ...
!=====================================================

! first, make sure that this file does not already exist
f_exists = .FALSE.
fname = trim(EMsoft_getEMtmppathname())//trim(ebsdnl%tmpfile)
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname), exist=f_exists)

call WriteValue('Creating temporary file :',trim(fname))

if (f_exists) then
  open(unit=itmpexpt,file=trim(fname),&
      status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)
  close(unit=itmpexpt,status='delete')
end if

! open the temporary file
open(unit=itmpexpt,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize_correct,iostat=ierr)

write (*,*) 'input file ',trim(EMsoft_getEMdatapathname())//trim(ebsdnl%exptfile)

ename = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%exptfile)
ename = EMsoft_toNativePath(ename)

f_exists = .FALSE.
inquire(file=trim(fname), exist=f_exists)
if (f_exists) then
    open(unit=iunitexpt,file=trim(ename),&
    status='old',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
else
    call FatalError('EMEBSDDIstatic:','The experimental pattern file does not exist') 
end if

! prepare the fftw plan for this pattern size
EBSDPattern = 0.0
tmp = sngl(getEBSDIQ(binx, biny, EBSDPattern, init))

! also, allocate the arrays used to create the dot product map; this will require 
! reading the actual EBSD HDF5 file to figure out how many rows and columns there
! are in the region of interest.  For now we get those from the nml until we actually 
! implement the HDF5 reading bit
! this portion of code was first tested in IDL.
allocate(lstore(L,ebsdnl%ipf_wd),pstore(L,ebsdnl%ipf_wd),dpmap(totnumexpt), &
         idpmap(totnumexpt),lp(L),cp(L))
dpmap= 0.0
pstore = 0.0
lstore = 0.0
lp = 0.0
cp = 0.0

! initialize the HiPassFilter routine
rdata = dble(EBSDPattern)
fdata = HiPassFilter(rdata,dims,ebsdnl%hipassw,init=.TRUE.)

call Message('Starting processing of experimental patterns')
call cpu_time(tstart)


prepexperimentalloop: do iii = 1,totnumexpt
    tmpimageexpt = 0.0
    read(iunitexpt,rec=iii) imageexpt

! convert imageexpt to 2D EBS Pattern array
    do kk=1,binx
      EBSDPattern(kk,1:biny) = imageexpt((kk-1)*biny+1:kk*biny)
    end do

! compute the pattern Image Quality 
    exptIQ(iii) = sngl(getEBSDIQ(binx, biny, EBSDPattern))

! Hi-Pass filter
    rdata = dble(EBSDPattern)
    fdata = HiPassFilter(rdata,dims,ebsdnl%hipassw)
    EBSDPattern = sngl(fdata)

! adaptive histogram equalization
    ma = maxval(EBSDPattern)
    mi = minval(EBSDPattern)
    
    EBSDpatternintd = ((EBSDPattern - mi)/ (ma-mi))
    EBSDpatterninteger = nint(EBSDpatternintd*255.0)
    EBSDpatternad =  adhisteq(ebsdnl%nregions,binx,biny,EBSDpatterninteger)
    EBSDPattern = float(EBSDpatternad)

! convert back to 1D vector
    do kk=1,binx
      imageexpt((kk-1)*biny+1:kk*biny) = EBSDPattern(kk,1:biny)
    end do

! normalize and apply circular mask
    imageexpt(1:L) = imageexpt(1:L) * masklin(1:L)
    imageexpt(1:L) = imageexpt(1:L)/NORM2(imageexpt(1:L))
    tmpimageexpt(1:L) = imageexpt(1:L)
! and write this pattern into the temporary file
    write(itmpexpt,rec=iii) tmpimageexpt

! finally, handle the average dot product map stuff
    ii = mod(iii,ebsdnl%ipf_wd)
    if (ii.eq.0) ii = ebsdnl%ipf_wd
    jj = iii/ebsdnl%ipf_wd+1
! do we need to copy pstore into lstore ?
    if ((ii.eq.1).and.(jj.gt.1)) lstore = pstore
! determine to which dpmap entries we need to add the dot product
    if (ii.eq.1) then
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
    else
      lp = cp
      cp(1:L) = imageexpt(1:L)
      pstore(1:L,ii) = cp(1:L)
      dp = sum(lp(1:L)*cp(1:L))
      dpmap(iii-1) = dpmap(iii-1) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
    if (jj.gt.1) then
      dp = sum(lstore(1:L,ii)*cp(1:L))
      dpmap(iii-ebsdnl%ipf_wd) = dpmap(iii-ebsdnl%ipf_wd) + dp
      dpmap(iii) = dpmap(iii) + dp
    end if
end do prepexperimentalloop

call WriteValue('','experimental patterns stored in tmp file')

! print some timing information

call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(totnumexpt)/tstop
call WriteValue('Number of experimental patterns processed per second : ',io_real,1,"(F10.1,//)")


close(unit=iunitexpt,status='keep')
! we keep the temporary file open since we will be reading from it...

! correct the dot product map values depending on inside, edge, or corner pixels
! divide by 4
dpmap = dpmap*0.25

! correct the straight segments
dpmap(2:ebsdnl%ipf_wd-1) = dpmap(2:ebsdnl%ipf_wd-1) * 4.0/3.0
dpmap(totnumexpt-ebsdnl%ipf_wd+2:totnumexpt-1) = dpmap(totnumexpt-ebsdnl%ipf_wd+2:totnumexpt-1) * 4.0/3.0
do jj=1,ebsdnl%ipf_ht-2
  dpmap(ebsdnl%ipf_wd*jj+1) = dpmap(ebsdnl%ipf_wd*jj+1) * 4.0/3.0
end do
do jj=2,ebsdnl%ipf_ht-1
  dpmap(ebsdnl%ipf_wd*jj) = dpmap(ebsdnl%ipf_wd*jj) * 4.0/3.0
end do

! and the corners
dpmap(1) = dpmap(1) * 2.0
dpmap(ebsdnl%ipf_wd) = dpmap(ebsdnl%ipf_wd) * 2.0
dpmap(totnumexpt) = dpmap(totnumexpt) * 2.0
dpmap(totnumexpt-ebsdnl%ipf_wd+1) = dpmap(totnumexpt-ebsdnl%ipf_wd+1) * 2.0

! and we deallocate the auxiliary variables for the average dot product map
deallocate(lstore,pstore,lp,cp)
!=====================================================


!=====================================================
! MAIN COMPUTATIONAL LOOP
!
! this particular implementation uses only one thread and one GPU.
!
! there is a double loop over experimental and dictionary patterns,
! and there is a choice of holding all patterns in memory or reading
! them from file(s) as we go along.
!=====================================================

verbose = .FALSE.

call timestamp()
call OMP_SET_NUM_THREADS(2)
io_int(1) = 2 ! ebsdnl%nthreads
call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

call cpu_time(tstart)

call timestamp()

dictionaryloop: do ii = 1,cratio+1
    results = 0.0

! if ii is odd, then we use dict1 for the dictionary computation, and dict2 for the GPU
! (assuming ii>1); when ii is even we switch the two pointers 
    if (mod(ii,2).eq.1) then
      dict => dict1
      dict1 = 0.0
      T0dict => dict2   ! these are the patterns to be sent to the GPU
      if (verbose.eqv..TRUE.) call WriteValue('','dict => dict1; T0dict => dict2')
    else
      dict => dict2
      dict2 = 0.0
      T0dict => dict1   ! these are the patterns to be sent to the GPU
      if (verbose.eqv..TRUE.) call WriteValue('','dict => dict2; T0dict => dict1')
    end if

    if (verbose.eqv..TRUE.) then
      io_int(1) = ii
      call WriteValue('Dictionaryloop index = ',io_int,1)
    end if

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,iii,jj,ll,mm,pp,ierr,io_int) &
!$OMP& PRIVATE(binned, ma, mi, EBSDpatternintd, EBSDpatterninteger, EBSDpatternad, quat, imagedictflt,imagedictfltflip)

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

      dicttranspose = 0.0

      do ll = 1,correctsize
        do mm = 1,Nd
            dicttranspose((ll-1)*Nd+mm) = T0dict((mm-1)*correctsize+ll)
        end do
      end do

      ierr = clEnqueueWriteBuffer(command_queue, cl_dict, CL_TRUE, 0_8, size_in_bytes_dict, C_LOC(dicttranspose(1)), &
                                  0, C_NULL_PTR, C_NULL_PTR)
      if(ierr /= CL_SUCCESS) call FatalError('clEnqueueWriteBuffer: ','cannot Enqueue write buffer.')

      experimentalloop: do jj = 1,cratioE

        expt = 0.0

        do pp = 1,ppendE(jj)   ! Ne or MODULO(totnumexpt,Ne)
          read(itmpexpt,rec=(jj-1)*Ne+pp) tmpimageexpt
          expt((pp-1)*correctsize+1:pp*correctsize) = tmpimageexpt
        end do

        ierr = clEnqueueWriteBuffer(command_queue, cl_expt, CL_TRUE, 0_8, size_in_bytes_expt, C_LOC(expt(1)), &
                                    0, C_NULL_PTR, C_NULL_PTR)
        if(ierr /= CL_SUCCESS) call FatalError('clEnqueueWriteBuffer: ','cannot Enqueue write buffer.')

        call InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,ebsdnl%devid,kernel,context,command_queue)

! this might be simplified later for the remainder of the patterns
        do qq = 1,ppend(jj)
            resultarray(1:Nd) = results((qq-1)*Nd+1:qq*Nd)
            indexarray(1:Nd) = indexlist((iii-1)*Nd+1:iii*Nd)

            call SSORT(resultarray,indexarray,Nd,-2)
            resulttmp(nnk+1:2*nnk,(jj-1)*Ne+qq) = resultarray(1:nnk)
            indextmp(nnk+1:2*nnk,(jj-1)*Ne+qq) = indexarray(1:nnk)

            call SSORT(resulttmp(:,(jj-1)*Ne+qq),indextmp(:,(jj-1)*Ne+qq),2*nnk,-2)

            resultmain(1:nnk,(jj-1)*Ne+qq) = resulttmp(1:nnk,(jj-1)*Ne+qq)
            indexmain(1:nnk,(jj-1)*Ne+qq) = indextmp(1:nnk,(jj-1)*Ne+qq)
        end do
      end do experimentalloop

      io_real(1) = maxval(results)
      io_real(2) = float(iii)/float(cratio)*100.0
      call WriteValue('',io_real,2,"(' max. dot product = ',F10.6,';',F6.1,'% complete')")


      if (mod(iii,10) .eq. 0) then
        io_int(1:2) = (/iii,cratio/)
        call WriteValue('',io_int,2,"(' -> Completed cycle ',I5,' out of ',I5)")
      end if
    else
       if (verbose.eqv..TRUE.) call WriteValue('','        GPU thread is idling')
    end if  ! ii.gt.1

!$OMP END MASTER

! and here is the other thread, which reads a series of patterns from an hdf5 file and processes them
! before handing them off to thread 0
    if (ii.lt.cratio+1) then
     if (verbose.eqv..TRUE.) then
       if (associated(dict,dict1)) then 
         write(*,"('    Thread ',I2,' is working on dict1')") TID
       else
         write(*,"('    Thread ',I2,' is working on dict2')") TID
       end if
     end if

! get a set of patterns from the precomputed dictionary file... then perform the 
! standard image processing steps on them before passing them on to thread 0.
! we'll use a hyperslab to read a block from file and then process them one at a timestamp
! in the following loop

   if (TID .ne. 0) then
! read data from the hyperslab
dataset = SC_EBSDpatterns
     dims3 = (/ binx, biny, ppend(ii) /)
     offset3 = (/ 0, 0, (ii-1)*Nd /)

     if(allocated(EBSDdictpat)) deallocate(EBSDdictpat)
     EBSDdictpat = HDF_readHyperslabCharArray3D(dataset, offset3, dims3, HDF_head)
      
     do pp = 1,ppend(ii)  !Nd or MODULO(FZcnt,Nd)

       do ll = 1,biny
           do mm = 1,binx
               binned(mm,ll) = float(ichar(EBSDdictpat(mm,ll,pp)))
           end do
       end do

       !binned(:,:) = float(EBSDdictpat(:,:,pp))

! adaptive histogram equalization
       ma = maxval(binned)
       mi = minval(binned)

       EBSDpatternintd = ((binned - mi)/ (ma-mi))
       EBSDpatterninteger = nint(EBSDpatternintd*255.0)
       EBSDpatternad =  adhisteq(ebsdnl%nregions,binx,biny,EBSDpatterninteger)
       binned = float(EBSDpatternad)

       imagedictflt = 0.0
       imagedictfltflip = 0.0

       do ll = 1,biny
         do mm = 1,binx
           imagedictflt((ll-1)*binx+mm) = binned(mm,ll)
         end do
       end do

! normalize and apply circular mask
       imagedictflt(1:L) = imagedictflt(1:L) * masklin(1:L)

       imagedictflt(1:correctsize) = imagedictflt(1:correctsize)/NORM2(imagedictflt(1:correctsize))
       dict((pp-1)*correctsize+1:pp*correctsize) = imagedictflt(1:correctsize)
     end do
   end if   

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

close(itmpexpt,status='delete')

! release the OpenCL kernel
ierr = clReleaseKernel(kernel)
call CLerror_check('InnerProdGPU:clReleaseKernel', ierr)


! close file and nullify pointer
call HDF_pop(HDF_head,.TRUE.)

! perform some timing stuff
call CPU_TIME(tstop)
tstop = tstop - tstart
io_real(1) = float(totnumexpt)*float(FZcnt) / tstop
call WriteValue('Number of pattern comparisons per second : ',io_real,1,"(/,F10.1)")
io_real(1) = float(totnumexpt) / tstop
call WriteValue('Number of experimental patterns indexed per second : ',io_real,1,"(/,F10.1,/)")

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

if (ebsdnl%datafile.ne.'undefined') then 
  vendor = 'TSL'
  eulerarray = eulerarray*180.0/sngl(cPi)
  call h5ebsd_writeFile(vendor, ebsdnl, dstr, tstrb, ipar, resultmain, exptIQ, indexmain, eulerarray, &
                        dpmap, progname, nmldeffile)
  call Message('Data stored in h5ebsd file : '//trim(ebsdnl%datafile))
end if

if (ebsdnl%ctffile.ne.'undefined') then 
  call ctfebsd_writeFile(ebsdnl,ipar,indexmain,eulerarray,resultmain)
  call Message('Data stored in ctf file : '//trim(ebsdnl%ctffile))
end if

if (ebsdnl%angfile.ne.'undefined') then 
  write (*,*) 'ang format currently in development [March 2016]'
  !call angebsd_writeFile(ebsdnl,ipar,indexmain,eulerarray,resultmain)
  !call Message('Data stored in ang file : '//trim(ebsdnl%angfile))
end if

! close the Fortran interface
call h5close_EMsoft(hdferr)


end subroutine MasterSubroutine

