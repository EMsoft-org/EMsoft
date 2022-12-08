! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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
program EMDispFieldSTEM

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                             :: nmldeffile, progname, progdesc
type(EMmdSTEMNameListType)                   :: msnml

nmldeffile = 'EMDispFieldSTEM.nml'
progname = 'EMDispFieldSTEM.f90'
progdesc = 'Calculation of STEM images for discrete dislocation dynamics data'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any 
call Interpret_Program_Arguments(nmldeffile,1,(/ 214 /), progname)


write (*,*) 'read program arguments '

! deal with the namelist stuff
call GetEMmdSTEMNameList(nmldeffile,msnml)
write (*,*) 'read namelist file '

write (*,*) 'calling dddSTEMcalc routine '
! perform the zone axis computations
call DDDSTEMcalc(msnml, progname, nmldeffile)

end program EMDispFieldSTEM




!--------------------------------------------------------------------------
!
! SUBROUTINE: DDDSTEMcalc
!
!> @author Joseph Tessmer, Carnegie Mellon University
!
!> @brief Simulate 4D-STEM images for a material represented by a displacement field
!
!> @date 02/02/18  JT  1.0 original
!--------------------------------------------------------------------------
subroutine DDDSTEMcalc(msnml, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use crystal
use symmetry
use initializersHDF
use initializers
use constants
use gvectors
use kvectors
use error
use io
use files
use TIFF_F90
use diffraction
use omp_lib
use MBModule
use HDF5
use NameListHDFwriters
use HDFsupport
use rotations!, only:eu2om
use ISO_C_BINDING
use MDsubroutines
use math
use quaternions
use clsupport
use clfortran 
use Lambert


IMPLICIT NONE

!inputs
type(EMmdSTEMNameListType),INTENT(INOUT)              :: msnml
character(fnlen),INTENT(IN)                           :: progname
character(fnlen),INTENT(IN)                           :: nmldeffile


real(kind=4)                        :: start, finish, looptimes, looptimef, pixstart, pixfinish
integer(kind=irg)                   :: cc, catom, currentAtom, symBoxX, symBoxY, symBoxZ, NTHR, NUMTHREADS, io_int(6) 
integer(kind=irg)                   :: xmin, xmax, ymin, ymax, zmin, zmax, ixy, beamcount 
integer(kind=irg)                   :: currentx, currenty, currentz, cz, hklvec(3), currentzmod
real(kind=dbl)                      :: dimsp(6), dimsn(6)
integer(kind=irg)                   :: maxnumreflections, discsize
integer(kind=irg),allocatable       :: hklarray(:,:)
real(kind=sgl),allocatable          :: sgarray(:,:), tiltshift


! image variables
real(kind=sgl),allocatable          :: ImageArray(:,:,:)

!variables for input data preparation
real(kind=sgl),allocatable          :: dispfieldraw(:,:),dispfield(:,:,:,:)
integer(kind=irg)                   :: kji(3)

integer(kind=8)                     :: atomArraySize(2), atomArraySize4D(4)
integer(kind=irg)                   :: numRows, numCells
real(kind=sgl), allocatable         :: relData(:,:)

integer(kind=irg)                   :: ii, jj, ll, mm
integer(kind=irg)                   :: hkl(3)
real(kind=sgl)                      :: hkltest1(3), hkltest2(3)

integer(kind=irg)                   :: i, j, x, y, ix, jy, ipos, jpos, isym, pgnum, SamplingType ! variables for point group and Laue group
integer(kind=irg),parameter         :: LaueTest(11) = (/ 149, 151, 153, 156, 158, 160, 161, 164, 165, 166, 167 /)  ! space groups with 2 or mirror at 30 degrees
integer(kind=irg)                   :: npyhex, ijmax, numk, skip ! parameters for calckvectors and calcwavelength subroutine

complex(kind=dbl),allocatable       :: DynMat(:,:), mscatt(:,:), DynMat_diag(:,:), DynMat_off_diag(:,:)
complex(kind=dbl)                   :: czero, cone
real(kind=sgl),allocatable,target   :: gx(:), gy(:), gz(:)

real(kind=sgl)                      :: FN(3), kk(3), kkk(3), fnat, kn, Radius, xy(2), tstart, tstop
real(kind=sgl)                      :: k(3), kp(3), ku(3), kin(3), eu(3), qu(4), gin(3), gout(3)
!integer(kind=irg)                   :: numset, ipx, ipy, ipz, iequiv(3,48), nequiv, ip, jp, izz, IE, iz, one, gg(3)
integer(kind=irg),allocatable       :: kij(:,:), nat(:), gvex(:,:), gvexpres(:)
real(kind=dbl)                      :: res(2), xyz(3), ind, om(3,3), sg
complex(kind=sgl),target            :: qg0, cmplxvar, pq0
real(kind=dbl)                      :: ktmax, delta, thk

complex(kind=sgl),allocatable,target:: a_off_diag(:,:)
character(fnlen)                    :: xtalname, groupname, DDataFile, fstring, datafile, outname
logical                             :: f_exists, readonly, overwrite=.TRUE., insert=.TRUE.
logical                             :: verbose, usehex, switchmirror
complex(kind=sgl),allocatable,target:: sgmaster(:)

type(unitcell), pointer             :: cell

! type(multicell),allocatable,dimension(:) :: cells
type(gnode),save                    :: rlp
type(DynType),save                  :: Dyn
type(BetheParameterType)            :: BetheParameters
type(reflisttype),pointer,save      :: reflist
type(reflisttype),pointer           :: firstw,rltmp, tmpreflist, tmpreflist2, reflist_tmp
integer(kind=irg)                   :: nthreads,TID,hdferr,num_el,etotal, nlines,nsx,nsy,SelE
type(HDFobjectStackType)            :: HDF_head
character(fnlen)                    :: dataset, instring, blochmode, datagroupname, dataname
character(fnlen)                    :: mode
integer(HSIZE_T)                    :: dims4(4), cnt4(4), offset4(4), dims2(2), cnt2(2), offset2(2)


! OpenCL variables
integer(c_intptr_t),allocatable, target         ::  platform(:)
integer(c_intptr_t),allocatable, target         ::  device(:)
integer(c_intptr_t),target                      ::  context
integer(c_intptr_t),target                      ::  command_queue
integer(c_intptr_t),target                      ::  prog
integer(c_intptr_t),target                      ::  kernel, kernel2, kernel3
integer(c_intptr_t),target                      ::  event
character(fnlen)                                ::  sourcefile, info
integer, parameter                              ::  source_length = 50000
character(len=source_length, KIND=c_char),TARGET::  csource
integer(c_size_t),TARGET                        ::  slength
integer(c_int)                                  ::  numd, nump, correctsize
integer(kind=8),target                          ::  globalsize(2), localsize(2), globalsize2(2), localsize2(2)
type(c_ptr), TARGET                             ::  psource
integer(c_size_t)                               ::  cnum
character(len=source_length),TARGET             ::  source
integer(c_int32_t)                              ::  ierr, ierr2, pcnt, qcnt
character(13),TARGET                            ::  kernelname, kernelname2
character(14, KIND=c_char),target               ::  ckernelname, ckernelname2
integer(kind=irg)                               ::  irec

real(kind=sgl),allocatable,target               ::  glist(:,:), gglist(:,:), glistarr(:), klist(:,:)
real(kind=sgl),target                           ::  mlambda, conv, ma, tend
integer(kind=irg),target                        ::  nsam, nref, nref_tmp, nsam_correct
complex(kind=sgl),target                        ::  cvals(9), coef(9)
type(sggamma),pointer                           ::  sglist, tmpsglist
complex(kind=sgl),allocatable,target            ::  wavecoeffin(:)

complex(kind=sgl),allocatable,target            ::  resGPU(:), a_off_test(:)
complex(kind=sgl),allocatable,TARGET            ::  dynmattest(:)
real(kind=sgl),allocatable                      ::  results(:,:,:,:), resultsrow(:,:,:,:)
integer(c_size_t),target                        ::  start_time, end_time, exec_time, texec_time
integer(c_size_t),target                        ::  return_bytes
integer(c_int64_t)                              ::  profile_props

integer(c_intptr_t),target                      ::  cl_glist, cl_FN, cl_om, cl_A_off, cl_coef, cl_SMf
integer(c_intptr_t),target                      ::  cl_A2, cl_A3, cl_TT1, cl_TT2, cl_TT3, cl_tmp, cl_SM,&
                                                    cl_wavecoeffin, cl_wavecoeffout, cl_gx, cl_gy, cl_gz
integer(c_intptr_t),target                      ::  cl_A_g, cl_A_dia, cl_wavecoefftmp
integer(c_size_t)                               ::  size_in_bytes_glist, size_in_bytes_FN, size_in_bytes_om
integer(c_size_t)                               ::  size_in_bytes_S_off, size_in_bytes_sgmaster,&
                                                    size_in_bytes_coef, size_in_bytes_A_dia, size_in_bytes_SMf,&
                                                    size_in_bytes_gminusgp

complex(kind=sgl),allocatable,target            :: snglpreccomp(:,:)
real(kind=dbl)                                  :: dblprecreal
real(kind=sgl)                                  :: DtoR, RtoD

complex(kind=sgl),allocatable,target            :: mscatttest(:)

character(11)                                   :: dstr
character(15)                                   :: tstrb
character(15)                                   :: tstre
character(fnlen,kind=c_char)                    :: line2(1)


real(kind=sgl),allocatable                :: klistarray(:,:)
integer(kind=irg),allocatable             :: kpix(:,:)
integer(kind=sgl)                         :: imh, imk, iml, gp(3)
complex(kind=dbl),allocatable             :: LUTqg(:,:,:)
character(12)                             :: stringone
character(3)                              :: stringtwo
character(18)                             :: stringthree
character(35)                             :: stringfour
character(54),TARGET                      :: progoptions

integer(kind=irg)                         :: xpos, ypos, zpos
real(kind=sgl),target                     :: dx, dy, dz

real(kind=sgl)                            :: dispfieldscalefactor(6)
logical                                   :: g_exists
real(kind=sgl)                            :: ytemp
integer(kind=irg)                         :: boxdims(3)
real(kind=sgl)                            :: tiltangle

integer(kind=irg)                         :: atomtypesANsin
integer(kind=irg),allocatable             :: atomtypesAN(:)
integer(kind=sgl)                         :: numAT
integer(kind=irg),allocatable             :: atomtypes(:)
integer(kind=irg)                         :: atomtypessin
real(kind=sgl),allocatable                :: dwfs(:)
real(kind=sgl)                            :: dwfsSin
integer(kind=8)                           :: dwfsSize(1), atomtypesANsize(1), lpabcSize(1)

real(kind=sgl)                            :: xyzcirc(3), xyLamb(2), beamstep, numbeams, beamsize
real(kind=sgl), allocatable               :: xyzBeams(:,:,:), xyLambs(:,:)
REAL(kind=sgl), PARAMETER                 :: Pi = 3.1415927
logical                                   :: preprocessedflag
integer(kind=irg)                         :: subslice, count
logical                                   :: EVEN, ODD, ZEVEN, ZODD
integer(kind=irg)                         :: ccount, ccounttot

! beam tilt parameters:
real(kind=sgl)                            :: lauec(2), lpg(3), glen, gplen, FNr(3), g3(3), exer, H, LC3, sgdenom, tt(3)
integer(kind=irg)                         :: ga(3), gb(3), gab(2,3), ir, ZAindex(3)



interface
            subroutine FindUnion(reflist_master, reflist_in, nref_master, nref_new)
                use typedefs

                IMPLICIT NONE

                type(reflisttype),pointer               :: reflist_master
                type(reflisttype),pointer               :: reflist_in
                integer(kind=irg),INTENT(INOUT)         :: nref_master
                integer(kind=irg),INTENT(IN)            :: nref_new

            end subroutine FindUnion

end interface

interface
            subroutine MarkStrong(reflist, nref)
                use typedefs

                IMPLICIT NONE

                type(reflisttype),pointer               :: reflist
                integer(kind=irg),INTENT(IN)            :: nref
            end subroutine MarkStrong

end interface


nullify(HDF_head%next)

! write NML values to the  variables used in the code

xtalname = msnml%xtalname
subslice = msnml%subslice
thk = msnml%thk
thk = thk/subslice

ZAindex = msnml%ZAindex
lauec   = msnml%lauec


! ccounttot = 10


! BEAMSIZE MUST!!! BE AN ODD INTEGER
beamsize = msnml%discsize
numbeams = beamsize ** 2
if (mod(int(beamsize), 2) /= 1) then
    write (*,*) 'Discsize must be an odd integer'
    STOP 
end if


preprocessedflag = msnml%presorted
DDataFile = msnml%inputfilename
outname = msnml%datafile
dispfieldscalefactor = msnml%scalefactor

DtoR = cPi/180.0
RtoD = 1.0/DtoR

verbose = .TRUE.

czero = cmplx(0.D0, 0.D0)
cone = cmplx(1.0,0.0,dbl)

write (*,*) 'starting computation'

!=============================================================
!read DDD data and get necessary files
!=============================================================

! read in the displacement field data

call h5open_EMsoft(hdferr)

! open the MD file using the default properties.
readonly = .TRUE.
hdferr =  HDF_openFile(DDataFile, HDF_head, readonly)

! ! open the data group
! ! !groupname = 'Data'
! groupname = 'Data'
! hdferr = HDF_openGroup(groupname, HDF_head)
! read in displacement field

dataset = 'Data'
if (preprocessedflag) then
    ! this was already processed into a 4D array
    call HDF_readDatasetFloatArray4D(dataset, atomArraySize4D, HDF_head, hdferr, dispfield)
    print *, atomArraySize4D
end if 
if (.NOT. preprocessedflag) then
    dataset = 'data'
    call HDF_readDatasetFloatArray2D(dataset, atomArraySize, HDF_head, hdferr, dispfieldraw)
end if 

! ! read in the number of atom types in the sample
! dataset = 'NumAT'
! call HDF_readDatasetInteger(dataset, HDF_head, hdferr, numAT)


! if (numAT .eq. 1) then
!     ! there's only 1 atom type and these should be single values
!     ! read debye waller factors for each atom:
!     dataset = 'dwfs'
!     call HDF_readDatasetFloat(dataset, HDF_head, hdferr, dwfsSin)
!     allocate(dwfs(1))
!     dwfs(1) = dwfsSin

!     ! read number of atom types present in simulation volume
!     dataset = 'atomtypesAN'
!     call HDF_readDatasetInteger(dataset, HDF_head, hdferr, atomtypesANsin)
!     allocate(atomtypesAN(1))
!     atomtypesAN(1) = atomtypesANsin

! else if (numAT /= 1) then
!     ! read debye waller factors for each atom:
!     dataset = 'dwfs'
!     call HDF_readDatasetFloatArray1D(dataset, dwfsSize, HDF_head, hdferr, dwfs)


!     ! read number of atom types present in simulation volume
!     dataset = 'atomtypesAN'
!     call HDF_readDatasetIntegerArray1D(dataset, atomtypesANsize, HDF_head, hdferr, atomtypesAN)
! end if



! close the datafile
call HDF_pop(HDF_head,.TRUE.)

! close the Fortran interface
call h5close_EMsoft(hdferr)

call Message('--> Read data from HDF5 file and closed file.')




! call Set_Bethe_Parameters(BetheParameters)
! don't want to have to generate a seperate xtal file with different site occupations: 
! new routine Initialize_Multicell

! allocate(cell)
! we need a cell for 
! allocate(cells(NumAT))
! do ii = 1, NumAT
!     call Initialize_Multicell(cells(ii)%cell, Dyn, rlp, xtalname, msnml%dmin, sngl(msnml%voltage), verbose)
!     call Message('--> Finished initializing cell.')
! end do
allocate(cell)
call Initialize_Cell(cell, Dyn, rlp, xtalname, msnml%dmin, sngl(msnml%voltage), verbose)
call Message('--> Finished initializing cell.')



!=============================================
! Prepare DDD displacement field data 
!=============================================
! ONLY DO THIS IF THE DATA WAS NOT PREPROCESSED
if (.NOT. preprocessedflag) then
    ! read in as a 2D array ->  x, y, z, dx, dy, dz
    ! this is in dispfieldraw
    ! output as a 4D array -> (d,x,y,z) 
    ! dispfield(:,:,:,:)
    ! dimensions (5,xpix,ypix,zpix)
    ! 5 = dx, dy, dz, in sample flag, idstr

    numRows = size(dispfieldraw,2)

    ! scale the dispfieldraw by the scale factor if it was not saved directly in nm

    do i = 1, numRows
        dispfieldraw(1,i) = dispfieldraw(1,i) * dispfieldscalefactor(1)
        dispfieldraw(2,i) = dispfieldraw(2,i) * dispfieldscalefactor(2)
        dispfieldraw(3,i) = dispfieldraw(3,i) * dispfieldscalefactor(3)
        dispfieldraw(4,i) = dispfieldraw(4,i) * dispfieldscalefactor(4)
        dispfieldraw(5,i) = dispfieldraw(5,i) * dispfieldscalefactor(5)
        dispfieldraw(6,i) = dispfieldraw(6,i) * dispfieldscalefactor(6)
    end do

    ! need to find the maximum and minimum of the field in each direction

    dimsp = MAXVAL(dispfieldraw,2)
    dimsn = MINVAL(dispfieldraw,2)
    xmin = dimsn(1)
    xmax = dimsp(1)
    ymin = dimsn(2)
    ymax = dimsp(2)
    zmin = dimsn(3)
    zmax = dimsp(3)

    symboxx = xmax-xmin + 1
    symboxy = ymax-ymin + 1
    kji(3) = symBoxX
    kji(2) = symBoxY
    kji(1) = zmax - zmin + 1

    ! the grid is known to be evenly spaced so we can simply allocate an array
    ! we use the additional value in the first dimension to indicate whether
    ! the beam should be propagated through this cell, e.g. if 
    ! dispfield(5,x,y,z) = 0 this is outside the cell and should not be propagated

    allocate(dispfield(5,(xmax-xmin+1),(ymax-ymin+1),(zmax-zmin+1)))
    print *, shape(dispfield)
    dispfield = 0

    boxdims(1) = (xmax-xmin+1)
    boxdims(2) = (ymax-ymin+1)
    boxdims(3) = (zmax-zmin+1)


    do i = 1, numRows
        dispfieldraw(1,i) = dispfieldraw(1,i) - xmin
        dispfieldraw(2,i) = dispfieldraw(2,i) - ymin
        dispfieldraw(3,i) = dispfieldraw(3,i) - zmin
        dispfield(1,NINT(dispfieldraw(1,i))+1,NINT(dispfieldraw(2,i))+1,NINT(dispfieldraw(3,i))+1) = dispfieldraw(4,i)
        dispfield(2,NINT(dispfieldraw(1,i))+1,NINT(dispfieldraw(2,i))+1,NINT(dispfieldraw(3,i))+1) = dispfieldraw(5,i)
        dispfield(3,NINT(dispfieldraw(1,i))+1,NINT(dispfieldraw(2,i))+1,NINT(dispfieldraw(3,i))+1) = dispfieldraw(6,i)
        dispfield(4,NINT(dispfieldraw(1,i))+1,NINT(dispfieldraw(2,i))+1,NINT(dispfieldraw(3,i))+1) = 1
    end do
else if (preprocessedflag) then
    symboxx = atomArraySize4D(2)
    symboxy = atomArraySize4D(3)
    kji(3) = symBoxX
    kji(2) = symBoxY
    kji(1) = atomArraySize4D(4)
end if 

print *, 'Simulation cell size: ', kji(1:3)
!=============================================
! finished preparing data
!=============================================


!=============================================
! calculating the list of g vectors
!=============================================

! convert euler angle to quaternion
eu(1:3) = msnml%eu(1:3)*DtoR
qu = eu2qu(eu)

k = quat_LP(conjg(qu),(/0.0,0.0,1.0/))
FN = k/cell%mlambda
call TransSpace(cell,FN,ku,'c','r')
call NormVec(cell,ku,'r')
FN = ku

nullify(reflist, reflist_tmp)
allocate(reflist)


! ! convert wave vector to reciprocal space, normalize and scale by electron wavelength.
! call TransSpace(cell,kk,kkk,'c','r')
! call NormVec(cell,kkk,'r')
! kk = kkk/cell%mLambda

!=======================================================================
! the list of excited reflections for the different incident beam 
! directions will not be the same. however, for using the approximations
! (details further in the code), the number of beams for each of those
! beam directions needs to be the same. we work around this by finding
! the list of reflections for a bunch of the beam directions and taking
! the union of them as the final reflection list. 
!=======================================================================
call Set_Bethe_Parameters(BetheParameters,.TRUE.)
nref = 0

do ll = 0,5
    do mm = 0,8

        nullify(reflist_tmp)

        nref_tmp = 0
        k           =  quat_LP(conjg(qu),(/0.0,0.0,1.0/))
        kp          =  (/(ll/3.D0) * cos(mm*cPi/4.D0), (ll/3.D0) * sin(mm*cPi/4.D0), 0.D0/)
        kp          =  quat_LP(conjg(qu),kp)
        k           =  k + tan(msnml%convergence/1000.D0) * kp 

        call NormVec(cell,k,'c')

        k           =  k/cell%mlambda

        call TransSpace(cell,k,kk,'c','r')

        call Initialize_ReflectionList(cell, reflist_tmp, BetheParameters, FN, kk, msnml%dmin, nref_tmp)

        call FindUnion(reflist, reflist_tmp, nref, nref_tmp)

        if(ll+mm .eq. 0) EXIT

    end do
end do


call Message('--> Finished initializing reflection list ')
io_int(1) = nref
call WriteValue('--> Number of reflections = ',io_int,1,'(I3)')

call MarkStrong(reflist, nref)

! tmpreflist => reflist%next
! do i = 1, nref
!     !print *, tmpreflist%hkl
!     tmpreflist => tmpreflist%next
! end do

! this size will be the actual size. the array is padded with zeros to 
! improve performance of the kernel code
correctsize = ceiling(float(nref)/16.0) * 16

!store glist for writing to the HDF
allocate(glist(3,nref))
allocate(gglist(4,nref))
glist = 0
! print the list of reflections
tmpreflist => reflist%next
do i = 1, nref
    glist(1,i) = tmpreflist%hkl(1)
    glist(2,i) = tmpreflist%hkl(2)
    glist(3,i) = tmpreflist%hkl(3)
    gglist(1,i) = tmpreflist%hkl(1)
    gglist(2,i) = tmpreflist%hkl(2)
    gglist(3,i) = tmpreflist%hkl(3)
    gglist(4,i) = tmpreflist%sg
    tmpreflist => tmpreflist%next
end do

 


allocate(hklarray(3,nref))
hklarray = 0

tmpreflist  =>  reflist%next
do ii = 1,nref
    hklarray(1:3,ii)    =  tmpreflist%hkl
    tmpreflist          => tmpreflist%next
end do


! Get the dynamical matrix
allocate(DynMat(nref,nref))
allocate(DynMat_diag(nref,nref),DynMat_off_diag(nref,nref))
allocate(a_off_diag(correctsize,correctsize))
DynMat_off_diag   =   cmplx(0.D0, 0.D0)
A_off_diag        =   cmplx(0.0, 0.0)
DynMat            =   cmplx(0.D0,0.D0)

call GetDynMatMaster(cell, reflist, DynMat, nref)


! account for beam tilt here, if present: 
j=0
do i=1,32
    if (SGPG(i).le.cell%SYM_SGnum) j=i
end do

call BFsymmetry(cell,ZAindex,j,isym,ir)
call ShortestG(cell,ZAindex,ga,gb,isym) ! outputs ga gb
io_int(1:3) = ZAindex(1:3)

call WriteValue('', io_int, 3,  "(//,' ','[',3I2,'] has Bright Field symmetry ')",advance="no")
! call Message(PGTWD(isym),"(' ',A,', ')",advance="no")
io_int(1) = ir
call WriteValue(' order = ', io_int, 1, "(I4/)")
io_int(1:3) = ga(1:3)
io_int(4:6) = gb(1:3)
call WriteValue(' Reciprocal lattice vectors : ', io_int, 6, "('(',3I3,') and (',3I3,')',/)")


tt = lauec(1)*ga + lauec(2)*gb
 io_int(1:3) = tt(1:3)
call WriteValue(' Laue shift: ', io_int, 3, "(' ',3I2)")
! normalization parameter
LC3 = sqrt(1.0-cell%mLambda**2*(CalcLength(cell,tt,'r')**2))   ! to ensure proper normalization of wave vector

tiltshift = CalcDiffAngle(cell,int(tt(1)),int(tt(2)),int(tt(3)))
tiltshift = cos(tiltshift)



DynMat = DynMat * cmplx(0.D0,1.D0)

do ii = 1,nref
    do jj = 1,nref
        if(ii .ne. jj) then
            DynMat_off_diag(ii,jj)   = tiltshift * DynMat(ii,jj) * thk !* 0.5
        end if
    end do
end do
A_off_diag(1:nref,1:nref) = DynMat_off_diag(:,:)


!=============================================
! done calculating the list of g vectors and dynmat
!=============================================

!=============================================
! Initialization the OpenCL device
!=============================================

call CLinit_PDCCQ(platform, nump, msnml%platid, device, numd, msnml%devid, info, context, command_queue)
call Message('--> Finished initializing the OPENCL devices.')
call Message('Device selected : '//trim(info))

! read the cl source file
sourcefile = 'dddSTEM.cl'
call CLread_source_file(sourcefile, csource, slength)

!=============================================
! Finished initialization of the OpenCL device
!=============================================

! building the progoptions string
stringone   = '-D TILE_BIG='
write(stringtwo,'(I3)') correctsize
stringtwo = adjustl(stringtwo)

stringthree = trim(stringone)//trim(stringtwo)

stringfour  = ' -cl-no-signed-zeros -cl-mad-enable'

progoptions = trim(stringthree)//trim(stringfour)//CHAR(0)

!print *, progoptions

 
!=============================================
! BUILD THE KERNEL
!=============================================

! create the program
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('DDD_STEMDCI:clCreateProgramWithSource', ierr)

! build the program
! progoptions = '-cl-no-signed-zeros -cl-mad-enable'//CHAR(0)
ierr = clBuildProgram(prog, numd, C_LOC(device), C_LOC(progoptions), C_NULL_FUNPTR, C_NULL_PTR)
!ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(msnml%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
if(len(trim(source)) > 0) call Message(trim(source(1:cnum)),frm='(A)')
call CLerror_check('DDD_STEMDCI:clBuildProgram', ierr)
call CLerror_check('DDD_STEMDCI:clGetProgramBuildInfo', ierr2)

! finally get the kernel and release the program
kernelname = 'dddSTEMDCI'
ckernelname = kernelname
ckernelname(11:11) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('DDD_STEMDCI:clCreateKernel', ierr)

! finally get the kernel and release the program
kernelname2 = 'PropagateBeam'
ckernelname2 = kernelname2
ckernelname2(14:14) = C_NULL_CHAR
kernel2 = clCreateKernel(prog, C_LOC(ckernelname2), ierr)
call CLerror_check('PropagateBeam:clCreateKernel', ierr)

! build a second copy of the prop. kernel for improved speed
kernel3 = clCreateKernel(prog, C_LOC(ckernelname2), ierr)
call CLerror_check('PropagateBeam:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('DDD_STEMDCI:clReleaseProgram', ierr)

call Message('--> Done creating OpenCL executable.')

!=============================================
! Finished building kernel 
!=============================================

!=============================================
! Initialization of OpenCL variables 
!=============================================

! checking saransh's old code for this to see if my algorithm is the issue or what:

! global and local work group size for kernel 1
! globalsize   =   (/correctsize, correctsize/)
! localsize    =   (/16, 16/)

! ! global and local work group size for kernel 2
! globalsize2 =   (/correctsize, correctsize*5/)
! localsize2  =   (/correctsize, 1/)

! ! probe convergence angle [mRad]
! conv     = msnml%convergence
! ktmax = tan(conv/1000)

! ! qg0
! qg0      = cell%LUTqg(0,0,0)

! ! master lsit of diagonal terms in the STEM-DCI calculation
! ! these values are already exponentiated, so should not be
! ! repeated on the GPU device

! nullify(sglist, tmpsglist)
! allocate(sglist)
! tmpsglist  =>  sglist


! beamsize = 20
! ! allocate(klist(3,21*21))
! allocate(klist(3,100*100))
! klist = 0.0
! call CalcUcg(cell, rlp, (/0,0,0/),applyqgshift=.TRUE. )
! pq0 = cmplx(0.D0,1.D0/rlp%xgp,dbl)
! pcnt = 1
! qcnt = 1
! do ll = -int(beamsize),int(beamsize)
!     do mm = -int(beamsize),int(beamsize)

!         if(ll**2 + mm**2 .le. int(beamsize)*int(beamsize)) then

!             kin             =  (/ktmax * float(ll)/beamsize, ktmax * float(mm)/beamsize, 0.D0/) + (/0.D0, 0.D0, 1.D0/)
!             call NormVec(cell,kin,'c')
!             kin             =  quat_LP(conjg(qu),kin)
!             kin             =  kin/cell%mlambda
!             call TransSpace(cell,kin,ku,'c','r')
!             klist(1:3,qcnt) =  ku

!             tmpreflist => reflist%next
!             do ii = 1,nref

!                 sg              =   calcsg(cell,float(tmpreflist%hkl),ku,FN)
!                 cmplxvar        =   0.5D0 * thk * cPi * (2.D0 * sg + qg0) ! + pq0 the 0.5 factor is because of e^Bt/2 e^At e^Bt/2
!                 cmplxvar        =   cmplxvar * cmplx(0.D0, 1.D0)

!                 tmpsglist%sg    =   sg
!                 tmpsglist%expsg =   exp(cmplxvar)
!                 tmpsglist%hkl   =   tmpreflist%hkl

!                 allocate(tmpsglist%next)
!                 tmpsglist       =>  tmpsglist%next
!                 nullify(tmpsglist%next)

!                 tmpreflist      =>  tmpreflist%next
!                 pcnt = pcnt + 1
!             end do
!             qcnt = qcnt + 1
!         end if
!     end do
! end do



! ! sampling density (total number of plane waves in STEM calculation)
! nsam            =   pcnt / nref
! nsam_correct    =   ceiling(float(nsam)/16.0) * 16

! allocate(kpix(2,nsam))
! kpix     = 0
! pcnt     = 1 
! do ll = -int(beamsize),int(beamsize)
!     do mm = -int(beamsize),int(beamsize)
!         if(ll**2 + mm**2 .le. int(beamsize)*int(beamsize)) then
!             kpix(1:2,pcnt) = (/ll, mm/)
!             pcnt = pcnt + 1
!         end if
!     end do
! end do

! allocate(sgmaster(correctsize * nsam_correct))
! sgmaster = cmplx(0.0,0.0)
! allocate(sgarray(nsam,nref))

! tmpsglist  =>  sglist
! do ii = 1, nsam
!     do jj = 1, nref
!         sgmaster((ii-1)*correctsize + jj) =  tmpsglist%expsg
!         tmpsglist   =>  tmpsglist%next
!         sgarray(ii,jj) = abs(tmpsglist%sg)
!     end do
! end do

! call Message('--> Done creating master excitation error list')
! end saransh's old algorithm.






! Some of these can be pre-computed externally 
! we can also tell the kernel where these will be in memory 
! and simply overwrite them at each loop.

! global and local work group size for kernel 1
globalsize   =   (/correctsize, correctsize/)
localsize    =   (/16, 16/)
! global and local work group size for kernel 2
globalsize2 =   (/correctsize, correctsize*5/)
localsize2  =   (/correctsize, 1/)
! probe convergence angle [mRad]
conv     = msnml%convergence
! qg0
qg0      = cell%LUTqg(0,0,0)

! get the Sg values for each beam in the cone:
nullify(sglist, tmpsglist)
allocate(sglist)
tmpsglist  =>  sglist

! radius 1 vector (on the surface of the unit sphere) that is conv away from the north pole in x
xyzcirc = (/ cos((Pi/2) - (conv/1000)), 0.0, sin((Pi/2) - (conv/1000)) /)

! print *, xyzcirc

xyLamb = LambertSphereToSquare(xyzcirc,ierr)

! now xyLamb(1) holds the min/max value we want to tile in lambert space
! we need to evenly disperse discsize points in this space:

allocate(xyzBeams(-INT((beamsize-1)/2):INT((beamsize-1)/2),-INT((beamsize-1)/2):INT((beamsize-1)/2),3))
allocate(xyLambs(INT(numbeams),2))
beamstep = 2*xyLamb(1)/beamsize


! print *, beamstep

numk = 0
pcnt = 0
xyLamb(1:2) = 0
do ll = -INT((beamsize-1)/2), INT((beamsize-1)/2)
    do mm = -INT((beamsize-1)/2), INT((beamsize-1)/2)
        ! get the coordinates on the sphere for each beam in the square
        xyLamb(1:2) = (/ ll*beamstep, mm*beamstep/) 
        xyLambs(numk+1,1:2) = xyLamb(1:2)
        xyzBeams(ll,mm,1:3) = LambertSquareToSphere( xyLamb(1:2), ierr)
        kin = xyzBeams(ll,mm,1:3)

        ! now we have unit vectors that are equal-area projected onto the surface of a sphere
        ! rotate to  crystal frame
        call NormVec(cell,kin,'c')
        kin             =  quat_LP(conjg(qu),kin)
        !scaling factor
        kin             =  kin/cell%mlambda
        ! ! go to reciprocal space (hkl)
        call TransSpace(cell,kin,ku,'c','r')


        ! ku is the new incident beam vector
        !klist(1:3,qcnt) =  ku

        ! holz line position
        H = 1.0/CalcLength(cell,kin,'d')
        ! g3 basis vector, properly scaled
        call CalcCross(cell,float(ga),float(gb),g3,'r','r',1)
        call NormVec(cell,g3,'r')
        g3 = H * g3

        !now we need to compute the sg values for this incident beam vec.
        tmpreflist => reflist%next
        do ii = 1,nref
            ! print *, tmpreflist%hkl
            ! loop over reflections
            glen = CalcLength(cell,float(tmpreflist%hkl),'r')
            if (glen.eq.0.0) then
             ! through-beam has 0 excitation error by definition.
             sg = czero
            else
             ! shifted hkl values 
             lpg = tt + tmpreflist%hkl
             gplen = CalcLength(cell,lpg,'r')
             if (gplen.eq.0.0) then
              ! print *, 'a'
              sg =-cell%mLambda*CalcDot(cell,2*ku+float(tmpreflist%hkl),lpg,'r')/2.0*&
              LC3*cos(CalcAngle(cell,dble(ku),dble(FN),'r'))        
             else
              ! print *, 'b'
              sgdenom=2.0*LC3*cos(CalcAngle(cell,dble(ku),dble(FN),'r'))-2.0*cell%mLambda*gplen*cos(CalcAngle(cell,lpg,FN,'r'))
              sg =-(cell%mLambda*CalcDot(cell,2*ku+float(tmpreflist%hkl),lpg,'r')&
                -2.0*LC3*CalcDot(cell,g3,lpg,'r'))/sgdenom
             end if
            end if
            ! print *, sg

            ! this was old code to compute the sg

            ! there should be a way to account for the laue shift here 
            ! which will give us the modified incident beam direction kin


            ! ! beam components from cone
            ! xyLamb(1:2) = (/ ll*beamstep, mm*beamstep/) 
            ! xyLambs(numk+1,1:2) = xyLamb(1:2)
            ! xyzBeams(ll,mm,1:3) = LambertSquareToSphere( xyLamb(1:2), ierr)
            ! kin = xyzBeams(ll,mm,1:3)

            ! ! kin is a VECTOR in CARTESIAN SPACE

            ! ! there should be something we can add or multiply by here to get to the shifted incident beam:
            ! ! now we have unit vectors that are equal-area projected onto the surface of a sphere
            ! ! rotate to  crystal frame

            ! call NormVec(cell,kin,'c')
            ! kin             =  quat_LP(conjg(qu),kin)
            ! !scaling factor
            ! kin             =  kin/cell%mlambda
            ! ! go to reciprocal space (hkl)
            ! call TransSpace(cell,kin,ku,'c','r')
            ! ! ku is the new incident beam vector in RECIPROCAL space
            ! !klist(1:3,qcnt) =  ku

            ! ! tilt should happen in reciprocal space


            ! sg              =   calcsg(cell,float(tmpreflist%hkl),ku,FN)
            ! print *, sg
            ! print *, cPi * (2.D0 * sg + qg0)
            ! testing no 0.5 for thickness
            ! cmplxvar        =   thk * cPi * (2.D0 * sg + qg0) * 0.5D0! + pq0 the 0.5 factor is because of e^Bt/2 e^At e^Bt/2
            ! cmplxvar        =   cmplxvar * cmplx(0.D0, 1.D0)
            ! old SG computation method; instead:

            cmplxvar = cPi * (2.D0 * sg + qg0) * cmplx(0.D0, 1.D0)
            cmplxvar = cmplxvar * thk/2

            tmpsglist%sg    =   sg
            tmpsglist%expsg =   exp(cmplxvar)
            tmpsglist%hkl   =   tmpreflist%hkl

            allocate(tmpsglist%next)
            tmpsglist       =>  tmpsglist%next
            nullify(tmpsglist%next)

            tmpreflist      =>  tmpreflist%next
            pcnt = pcnt + 1
        end do
        numk = numk + 1
    end do
end do

! sampling density (total number of plane waves in STEM calculation)
!nsam            =   pcnt / nref
nsam            =   numbeams
nsam_correct    =   ceiling(float(nsam)/16.0) * 16

io_int(1) = nsam
call WriteValue('--> number of plane waves in STEM calculation = ',io_int,1,'(I6)')

! print *, numbeams, nsam, nsam_correct, correctsize

allocate(sgmaster(correctsize * nsam_correct))
sgmaster = cmplx(0.0,0.0)
allocate(sgarray(nsam,nref))

tmpsglist  =>  sglist
do ii = 1, nsam
    do jj = 1, nref
        sgarray(ii,jj) = abs(tmpsglist%sg)
        sgmaster((ii-1)*correctsize + jj) =  tmpsglist%expsg
        tmpsglist   =>  tmpsglist%next
    end do
end do

call Message('--> Done creating master excitation error list')

!=============================================
! Prepping data for input to the gpu
!=============================================

! complex coefficients for matrix exponential
! details can be found in the following paper:
!
! Robert S. Pennington n, Feng Wang, Christoph T. Koch,
! Stacked-Bloch-wave electron diffraction simulations using GPU acceleration,
! Ultramicroscopy, 141 (2014), 32–37

cvals(1) = cmplx(-3.3335514852690488032942739163345055,0.0)
cvals(2) = cmplx(-3.0386480729366970892124687564926859,-1.5868011957588383288038677051222921)
cvals(3) = cmplx(-3.0386480729366970892124687564926859,+1.5868011957588383288038677051222921)
cvals(4) = cmplx(-2.1108398003026547374987047865183922,-3.0899109287255009227777015426228801)
cvals(5) = cmplx(-2.1108398003026547374987047865183922,+3.0899109287255009227777015426228801)
cvals(6) = cmplx(-0.38106984566311299903129424501333242,-4.3846445331453979503692027283066828)
cvals(7) = cmplx(-0.38106984566311299903129424501333242,+4.3846445331453979503692027283066828)
cvals(8) = cmplx(2.6973334615369892273896047461916633,-5.1841620626494141778340870727109629)
cvals(9) = cmplx(2.6973334615369892273896047461916633,+5.1841620626494141778340870727109629)

coef(1) = cvals(1)+cvals(2)+cvals(3)
coef(2) = cvals(1)*cvals(2)+cvals(2)*cvals(3)+cvals(3)*cvals(1)
coef(3) = cvals(1)*cvals(2)*cvals(3)

coef(4) = cvals(4)+cvals(5)+cvals(6)
coef(5) = cvals(4)*cvals(5)+cvals(5)*cvals(6)+cvals(6)*cvals(4)
coef(6) = cvals(4)*cvals(5)*cvals(6)

coef(7) = cvals(7)+cvals(8)+cvals(9)
coef(8) = cvals(7)*cvals(8)+cvals(8)*cvals(9)+cvals(9)*cvals(7)
coef(9) = cvals(7)*cvals(8)*cvals(9)


! wave function coefficients
allocate(wavecoeffin(nsam * correctsize))
wavecoeffin     =   cmplx(0.0,0.0)
do ii = 1,nsam
    wavecoeffin((ii - 1)*correctsize + 1)   =   cmplx(1.0,0.0)
end do
! g - g'
allocate(gx(correctsize**2),gy(correctsize**2),gz(correctsize**2))
gx = 0.0
gy = 0.0
gz = 0.0

! all the (g - g') reciprocal lattice vectors are computed and copied 
! to the GPU here
tmpreflist => reflist%next
do ii = 1,nref
    tmpreflist2 => reflist%next
    do jj = 1,nref            
        ! print *, (ii-1)*nref + jj
        if(ii .ne. jj) then
            hkl = tmpreflist%hkl - tmpreflist2%hkl
            gx((ii-1)*correctsize + jj) = 2.D0 * cPi * float(hkl(1))
            gy((ii-1)*correctsize + jj) = 2.D0 * cPi * float(hkl(2))
            gz((ii-1)*correctsize + jj) = 2.D0 * cPi * float(hkl(3))

            ! gx((ii-1)*nref + jj) = 2.D0 * cPi * float(hkl(1))
            ! gy((ii-1)*nref + jj) = 2.D0 * cPi * float(hkl(2))
            ! gz((ii-1)*nref + jj) = 2.D0 * cPi * float(hkl(3))
        end if
        tmpreflist2 => tmpreflist2%next
    end do
    tmpreflist => tmpreflist%next
end do

! size of the different variables
! this is what we need to allocate the buffers
allocate(snglpreccomp(1,1))
allocate(glistarr(1))

! this is being size checked before being allocated
size_in_bytes_glist     =   nref * 3 * sizeof(glistarr(1))
size_in_bytes_FN        =   3 * sizeof(FN(1))

! this is also being size checked before being allocated
size_in_bytes_om        =   9 * sizeof(dblprecreal)
size_in_bytes_coef      =   9 * sizeof(coef(1))
size_in_bytes_S_off     =   correctsize * correctsize * sizeof(snglpreccomp(1,1))
size_in_bytes_sgmaster  =   correctsize * nsam_correct* sizeof(sgmaster(1)) 
size_in_bytes_SMf       =   correctsize * correctsize * nsam_correct * sizeof(sgmaster(1))
size_in_bytes_gminusgp  =   correctsize * correctsize * sizeof(gx(1))

!=============================================
! Finished initialization of OpenCL variables
!=============================================

!============================================================
!GENERATE ALL THE BUFFERS
!============================================================

cl_A_off    =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_A_off', ierr)

cl_A_g      =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_A_g', ierr)

cl_coef     =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_coef, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_coef', ierr)

cl_A2       =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_A2', ierr)

cl_A3       =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_A3', ierr)

cl_TT1      =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_TT1', ierr)

cl_TT2      =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_TT2', ierr)

cl_TT3      =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_TT3', ierr)

cl_tmp      =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_tmp', ierr)

cl_SM       =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_SM', ierr)

cl_SMf      =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_SMf, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_SMf', ierr)

cl_wavecoeffin =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_sgmaster, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_wavecoeffin', ierr)

cl_wavecoeffout =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_sgmaster, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_wavecoeffout', ierr)

cl_wavecoefftmp =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_SMf, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_wavecoefftmp', ierr)

cl_A_dia    =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_sgmaster, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_A_dia', ierr)

cl_gx       =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_gx', ierr)

cl_gy       =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_gy', ierr)

cl_gz       =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('DDD_STEMDCI:clCreateBuffer:cl_gz', ierr)

call Message('--> Done allocating all GPU buffers.')

!=============================================
! Finished allocating buffers
!=============================================

!=============================================
! Put some data in some buffers
!=============================================

! These quantities do not change per-loop so we can simply leave them in place.

ierr = clEnqueueWriteBuffer(command_queue, cl_coef, CL_TRUE, 0_8, size_in_bytes_coef, &
                          C_LOC(coef(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_coef', ierr)

! g - g' variables
ierr = clEnqueueWriteBuffer(command_queue, cl_gx, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
                          C_LOC(gx(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_gx', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_gy, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
                          C_LOC(gy(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_gy', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_gz, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
                          C_LOC(gz(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_gz', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_A_dia, CL_TRUE, 0_8, size_in_bytes_sgmaster, &
                          C_LOC(sgmaster(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_A_dia', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_A_off, CL_TRUE, 0_8, size_in_bytes_S_off, &
                  C_LOC(A_off_diag(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_A_off', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_A_g, CL_TRUE, 0_8, size_in_bytes_S_off, &
                  C_LOC(A_off_diag(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_A_g', ierr)

ierr = clFinish(command_queue)
call CLerror_check('DDD_STEMDCI:clFinish', ierr)

call Message('--> Constant values inserted into buffers.')

!=============================================
! Done with buffers for now
!=============================================

!=============================================
! Set some kernel arguments
!=============================================

ierr =  clSetKernelArg(kernel, 0, sizeof(nsam_correct), C_LOC(nsam_correct))
call CLerror_check('DDD_STEMDCI:clSetKernelArg:nsam', ierr)

ierr =  clSetKernelArg(kernel, 1, sizeof(nref), C_LOC(nref))
call CLerror_check('DDD_STEMDCI:clSetKernelArg:nref', ierr)

ierr =  clSetKernelArg(kernel, 2, sizeof(cl_A_off), C_LOC(cl_A_off))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 3, sizeof(cl_coef), C_LOC(cl_coef))
call CLerror_check('DDD_STEMDCI:clSetKernelArg:nref', ierr)

ierr =  clSetKernelArg(kernel, 4, sizeof(cl_A2), C_LOC(cl_A2))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:A2', ierr)

ierr =  clSetKernelArg(kernel, 5, sizeof(cl_A3), C_LOC(cl_A3))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:A3', ierr)

ierr =  clSetKernelArg(kernel, 6, sizeof(cl_TT1), C_LOC(cl_TT1))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:TT1', ierr)

ierr =  clSetKernelArg(kernel, 7, sizeof(cl_TT2), C_LOC(cl_TT2))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:TT2', ierr)

ierr =  clSetKernelArg(kernel, 8, sizeof(cl_TT3), C_LOC(cl_TT3))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:TT3', ierr)

ierr =  clSetKernelArg(kernel, 9, sizeof(cl_tmp), C_LOC(cl_tmp))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:tmp', ierr)

ierr =  clSetKernelArg(kernel, 10, sizeof(cl_SM), C_LOC(cl_SM))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:SM', ierr)

ierr =  clSetKernelArg(kernel, 11, sizeof(cl_A_dia), C_LOC(cl_A_dia))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:A_dia', ierr)

ierr =  clSetKernelArg(kernel, 12, sizeof(cl_SMf), C_LOC(cl_SMf))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:SMf', ierr)

ierr =  clSetKernelArg(kernel, 13, sizeof(cl_gx), C_LOC(cl_gx))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:gx', ierr)

ierr =  clSetKernelArg(kernel, 14, sizeof(cl_gy), C_LOC(cl_gy))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:gy', ierr)

ierr =  clSetKernelArg(kernel, 15, sizeof(cl_gz), C_LOC(cl_gz))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:gz', ierr)

ierr =  clSetKernelArg(kernel, 16, sizeof(cl_A_g), C_LOC(cl_A_G))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:A_g', ierr)

ierr =  clSetKernelArg(kernel2, 0, sizeof(cl_SMf), C_LOC(cl_SMf))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:SMf', ierr)

ierr =  clSetKernelArg(kernel2, 1, sizeof(cl_wavecoeffin), C_LOC(cl_wavecoeffin))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoeffin', ierr)

ierr =  clSetKernelArg(kernel2, 2, sizeof(cl_wavecoeffout), C_LOC(cl_wavecoeffout))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

ierr =  clSetKernelArg(kernel2, 3, sizeof(cl_wavecoefftmp), C_LOC(cl_wavecoefftmp))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoefftmp', ierr)

ierr =  clSetKernelArg(kernel2, 4, sizeof(nsam_correct), C_LOC(nsam_correct))
call CLerror_check('DDD_STEMDCI:clSetKernelArg:nsam', ierr)

ierr =  clSetKernelArg(kernel3, 0, sizeof(cl_SMf), C_LOC(cl_SMf))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:SMf', ierr)

ierr =  clSetKernelArg(kernel3, 1, sizeof(cl_wavecoeffout), C_LOC(cl_wavecoeffout))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

ierr =  clSetKernelArg(kernel3, 2, sizeof(cl_wavecoeffin), C_LOC(cl_wavecoeffin))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoeffin', ierr)

ierr =  clSetKernelArg(kernel3, 3, sizeof(cl_wavecoefftmp), C_LOC(cl_wavecoefftmp))
call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoefftmp', ierr)

ierr =  clSetKernelArg(kernel3, 4, sizeof(nsam_correct), C_LOC(nsam_correct))
call CLerror_check('DDD_STEMDCI:clSetKernelArg:nsam', ierr)

!=============================================
! Done with kernel arguments for now
!=============================================

allocate(resultsrow(1,kji(2),nsam,nref))
resultsrow = 0.0

!=============================================
! Prepare for h5 file to save the data
!=============================================

call Message('Preparing output h5 file')

nullify(HDF_head%next)
! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

datafile = outname

datafile = EMsoft_toNativePath(datafile)
inquire(file=trim(datafile), exist=f_exists)

if (f_exists) then
  call Message(' --> deleting old data file with the same name')
  open(unit=dataunit, file=trim(datafile), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

nullify(HDF_head%next)

hdferr =  HDF_createFile(datafile, HDF_head)

!write the EMheader to the file
dataset = 'DDDdata'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, dataset)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the nml and json files and write them as string arrays to the file
dataset = SC_STEMDCINML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! ! create a namelist group to write all the namelist files into
! groupname = SC_NMLparameters
! hdferr = HDF_createGroup(groupname, HDF_head)

! !call HDFwriteGammaSTEMDCINameList(HDF_head, enl)

! leave this group
! call HDF_pop(HDF_head)

! dataset   =  SC_StopTime
! line2(1)  =  dstr//', '//tstre
! hdferr    =  HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

! call HDF_pop(HDF_head)
! call HDF_pop(HDF_head)

!create a group to write all the data and parameters into

groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

! write final intensities
groupname = SC_STEMDCI
hdferr = HDF_createGroup(groupname, HDF_head)

!dataset = SC_BraggAngle
!hdferr = HDF_writeDatasetFloat(dataset, CalcDiffAngle(cell,ga(1),ga(2),ga(3))*0.5, HDF_head)

dataset = SC_wavelength
hdferr  = HDF_writeDatasetFloat(dataset, sngl(cell%mLambda), HDF_head)

dataset = SC_pixelsize
hdferr  = HDF_writeDatasetFloat(dataset, 1.0, HDF_head)

dataset = SC_numreflections
hdferr  = HDF_writeDatasetInteger(dataset, nref, HDF_head)

dataset = SC_numk
hdferr  = HDF_writeDatasetInteger(dataset, nsam, HDF_head)

dataset = SC_hkl
hdferr  = HDF_writeDatasetIntegerArray2D(dataset, hklarray, 3, nref, HDF_head)

dataset = SC_sg
hdferr  = HDF_writeDatasetFloatArray2D(dataset, sgarray, nsam, nref, HDF_head)
 

! FOR SARANSH'S ALGORITHM

! dataset = SC_PixelLocation
! hdferr  = HDF_writeDatasetIntegerArray2D(dataset, kpix, 2, nsam, HDF_head)
! dataset = SC_klist
! hdferr  = HDF_writeDatasetFloatArray2D(dataset, klist, 3, nsam, HDF_head)


! This holds the x,y coordinates for 
dataset = SC_PixelLocation
hdferr  = HDF_writeDatasetFloatArray2D(dataset, xyLambs, INT(numbeams), 2, HDF_head)

dataset = SC_convergenceangle
hdferr  = HDF_writeDatasetFloat(dataset, conv, HDF_head)

! write the orientation as a quaternion
dataset = SC_orientation
hdferr  = HDF_writeDatasetFloatArray1D(dataset, qu, 4, HDF_head)


! write the laue center shift
dataset = SC_lauec
hdferr  = HDF_writeDatasetFloatArray1D(dataset, lauec, 2, HDF_head)


! write ga + gb
gab(1,:) = ga
gab(2,:) = gb
! gab(1,:) = (/ 0, 1, 2 /)
! gab(2,:) = (/ 3, 4, 5 /)
dataset = SC_ga
hdferr  = HDF_writeDatasetIntegerArray2D(dataset, gab, 2,3, HDF_head)


! print *, '9'
! print *, shape(results)
! dataset = SC_Intensities
! hdferr = HDF_writeDatasetFloatArray4D(dataset, results, symboxx, symboxy, nsam, nref, HDF_head)

print *, 'creating hyperslab array'

dataset = SC_Intensities
  dims4 = (/  kji(3), kji(2), nsam, nref /)
  cnt4 = (/ 1, kji(2), nsam, nref /)
  offset4 = (/ 0, 0, 0, 0 /)
  call H5Lexists_f(HDF_head%next%objectID,trim(dataset),g_exists, hdferr)
  if (g_exists) then 
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, resultsrow, dims4, offset4, cnt4, HDF_head, insert)
  else
    hdferr = HDF_writeHyperslabFloatArray4D(dataset, resultsrow, dims4, offset4, cnt4, HDF_head)
  end if


call HDF_pop(HDF_head,.TRUE.)

! and close the fortran hdf interface
call h5close_f(hdferr)

!=============================================
! Finished output file prep
!=============================================



!=============================================
! main calculation loop
!=============================================

allocate(resGPU(nsam_correct * correctsize))

resultsrow = 0.0

call Message('--> Starting main computation loop.')

wavecoeffin     =   cmplx(0.0,0.0)
do ii = 1,nsam
    wavecoeffin((ii - 1)*correctsize + 1)   =   cmplx(1.0,0.0)
end do


! Should we start in A or B?
! this is a stupid way to do this
if (mod(kji(1),2) == 0) then
    ! even number of z slices
    ZEVEN = .TRUE.
    ZODD = .FALSE.
else if (mod(kji(1),2) == 1) then
    ! odd number of z slices
    ZEVEN = .FALSE.
    ZODD = .TRUE.
end if


! Even or odd number of subslices
if (MOD(subslice,2) == 0) then
    EVEN = .True.
    ODD  = .False.
else if (MOD(subslice,2) == 1) then
    ODD  = .True.
    EVEN = .False.
end if


! allocate(a_off_test(correctsize*correctsize))
allocate(dynmattest(correctsize * correctsize * nsam_correct))

print *, "Slice thickness: ", msnml%thk
print *, "Subslice thickness: ", thk
print *, "Total foil thickness: ", msnml%thk*kji(1)

do ixy = 0, (symboxx)*(symboxy) - 1
    ! extract x and y values 
    currenty = mod(ixy,symboxy)
    currentx = (ixy - currenty)/symboxy

    currentx = currentx + 1
    currenty = currenty + 1
    
    ! set the initial wave state to 1 0 ... 0 for each column

    ierr = clEnqueueWriteBuffer(command_queue, cl_wavecoeffin, CL_TRUE, 0_8, size_in_bytes_sgmaster, &
                  C_LOC(wavecoeffin(1)), 0, C_NULL_PTR, C_LOC(event))
    call CLerror_check('DDD_STEMDCI:clEnqueueWriteBuffer:cl_wavecoeffin', ierr)

    ! ierr = clFinish(command_queue)
    ! call CLerror_check('DDD_STEMDCI:clFinish', ierr)

    ! go down the column




    ! REMOVE THIS!!!!
    ! ccount = 0


    do currentz = kji(1), 1, -1
        if (dispfield(4,currentx,currenty,currentz) == 0.0) then
            CYCLE
        end if 


        ! ! REMOVE THIS!!!!
        ! ccount = ccount + 1
        ! if (ccount > ccounttot) then
        !     CYCLE
        ! end if


        ! print *, currentx, currenty, currentz

        resGPU = 0.0
        dx      =   dispfield(1,currentx,currenty,currentz) 
        dy      =   dispfield(2,currentx,currenty,currentz) 
        dz      =   dispfield(3,currentx,currenty,currentz) 
        ! dx = currentz * 0.001
        ! dy = 0
        ! dz = 0

        ! print *, currentx, currenty, currentz
        ! print *, dx, dy, dz

        ! if this cell is outside the sample, don't propagate the beam 
        ! we also need to consider if this 

        ierr    =  clSetKernelArg(kernel, 17, sizeof(dx), C_LOC(dx))
        call CLerror_check('DDD_STEMDCI:clSetKernelArg:dx', ierr)

        ierr    =  clSetKernelArg(kernel, 18, sizeof(dy), C_LOC(dy))
        call CLerror_check('DDD_STEMDCI:clSetKernelArg:dy', ierr)

        ierr    =  clSetKernelArg(kernel, 19, sizeof(dz), C_LOC(dz))
        call CLerror_check('DDD_STEMDCI:clSetKernelArg:dz', ierr)

        ! call cpu_time(start)

        !=====================================
        ! EXECUTE THE EXPONENTIATION KERNEL
        !=====================================

        ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), &
                          0, C_NULL_PTR, C_LOC(event))
        call CLerror_check('DDD_STEMDCI:clEnqueueNDRangeKernel', ierr)

        ierr = clFinish(command_queue)
        call CLerror_check('DDD_STEMDCI:clFinish', ierr)

        ! ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_QUEUED, sizeof(start_time), C_LOC(start_time), return_bytes)

        ! ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end_time), C_LOC(end_time), return_bytes)

        ! exec_time  = end_time  -  start_time

        ! texec_time = texec_time + exec_time

        ! print *, 'kernel 1'
        ! print *, exec_time

        ierr = clReleaseEvent(event)
        call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)




        !! DEBUGGING: READ OUT sm to check values against control 

        ! if (currentz== kji(1) .and. currentx==kji(2) .and. currenty==kji(2)) then
        !     ierr = clEnqueueReadBuffer(command_queue, cl_SMf, CL_TRUE, 0_8, size_in_bytes_SMf,&
        !        C_LOC(dynmattest(1)), 0, C_NULL_PTR, C_LOC(event))
        !     call CLerror_check('MD_STEMDCI:clEnqueueReadBuffer:cl_wavecoeffout', ierr)

        !     ! the dynmat is supposed to be DynMat_off_diag

        !     ! print *, DynMat_off_diag
        !     ! print *, '----------'
        !     print *, dynmattest(1:9*16)
        ! end if



        ! !===================================================
        ! ! SET ARGUMENTS FOR BEAM PROPAGATION STEP
        ! !===================================================

        ! ierr =  clSetKernelArg(kernel2, 0, sizeof(cl_SMf), C_LOC(cl_SMf))
        ! call CLerror_check('DDD_STEMDCI:clsetKernelArg:SMf', ierr)

        ! ierr =  clSetKernelArg(kernel2, 1, sizeof(cl_wavecoeffin), C_LOC(cl_wavecoeffin))
        ! call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoeffin', ierr)

        ! ierr =  clSetKernelArg(kernel2, 2, sizeof(cl_wavecoeffout), C_LOC(cl_wavecoeffout))
        ! call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

        ! ierr =  clSetKernelArg(kernel2, 3, sizeof(cl_wavecoefftmp), C_LOC(cl_wavecoefftmp))
        ! call CLerror_check('DDD_STEMDCI:clsetKernelArg:wavecoefftmp', ierr)

        ! ierr =  clSetKernelArg(kernel2, 4, sizeof(nsam_correct), C_LOC(nsam_correct))
        ! call CLerror_check('DDD_STEMDCI:clSetKernelArg:nsam', ierr)

        ! !===================================
        ! ! EXECUTE THE PROPAGATION KERNEL
        ! !===================================

        ! ierr = clEnqueueNDRangeKernel(command_queue, kernel2, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
        !                   0, C_NULL_PTR, C_LOC(event))
        ! call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)




        ! new subslice code:

        ! There are 2 cases - even or odd number of subslices
        ! even - each slice will always start in the same kernel
        ! odd  - the starting kernel will alternate 

        ! how many times this cell has been propagated
        count = 0
        if (EVEN) then
            ! even number of subslices, always start in kernel 1
            do 
                if (mod(count,2) == 0) then
                    ierr = clEnqueueNDRangeKernel(command_queue, kernel2, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                                      0, C_NULL_PTR, C_LOC(event))
                    call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

                    ierr = clReleaseEvent(event)
                    call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)
                else 
                    ierr = clEnqueueNDRangeKernel(command_queue, kernel3, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                                      0, C_NULL_PTR, C_LOC(event))
                    call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

                    ierr = clReleaseEvent(event)
                    call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)
                end if 
                count = count + 1

                if (count == subslice) then
                    EXIT
                end if 
            end do
        else if (ODD) then
            ! this is more complicated because we need to alternate the starting point
            currentzmod = MOD((kji(1) - currentz),2)
            IF (CURRENTZMOD == 0) THEN
            ! 0 or even number of interations - the incident beam starts in 1
                do 
                    if (mod(count,2) == 0) then
                        ierr = clEnqueueNDRangeKernel(command_queue, kernel2, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                                          0, C_NULL_PTR, C_LOC(event))
                        call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

                        ierr = clReleaseEvent(event)
                        call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)
                    else 
                        ierr = clEnqueueNDRangeKernel(command_queue, kernel3, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                                          0, C_NULL_PTR, C_LOC(event))
                        call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

                        ierr = clReleaseEvent(event)
                        call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)
                    end if 
                    count = count + 1

                    if (count == subslice) then
                        EXIT
                    end if 
                end do
            ELSE IF (CURRENTZMOD == 1) THEN
                do 
                    if (mod(count,2) == 0) then
                        ierr = clEnqueueNDRangeKernel(command_queue, kernel3, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                                          0, C_NULL_PTR, C_LOC(event))
                        call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

                        ierr = clReleaseEvent(event)
                        call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)
                    else 
                        ierr = clEnqueueNDRangeKernel(command_queue, kernel2, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                                          0, C_NULL_PTR, C_LOC(event))
                        call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

                        ierr = clReleaseEvent(event)
                        call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)
                    end if 
                    count = count + 1

                    if (count == subslice) then
                        EXIT
                    end if 
                end do
            END IF
        ! even/odd subslice if loop
        end if


        ! --------------------------------------------------------------------------------
        ! OLD SINGLE PROP CODE
        ! --------------------------------------------------------------------------------

        ! currentzmod = MOD((kji(1) - currentz),2)

        ! ! if it has been 0 or an even number of iterations, the current state is in "in"
        ! ! and we want to put it in "out"

        ! ! if it has been an odd number of iterations, the current state is in "out" and we 
        ! ! want to put it in "in"

        ! IF (CURRENTZMOD == 0) THEN

        !     ierr = clEnqueueNDRangeKernel(command_queue, kernel2, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
        !                       0, C_NULL_PTR, C_LOC(event))
        !     call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

        ! ELSE IF (CURRENTZMOD == 1) THEN

        !     ierr = clEnqueueNDRangeKernel(command_queue, kernel3, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
        !                       0, C_NULL_PTR, C_LOC(event))
        !     call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

        ! END IF

        ! ierr = clFinish(command_queue)
        ! call CLerror_check('PropagateBeam:clFinish', ierr)

        ! ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_QUEUED, sizeof(start_time), C_LOC(start_time), return_bytes)

        ! ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end_time), C_LOC(end_time), return_bytes)

        ! exec_time  = end_time  -  start_time

        ! texec_time = texec_time + exec_time


        ! ierr = clReleaseEvent(event)
        ! call CLerror_check('DDD_STEMDCI:clReleaseEvent', ierr)

        ! --------------------------------------------------------------------------------
        ! OLD SINGLE PROP CODE
        ! --------------------------------------------------------------------------------


    ! !z loop
    end do
    resGPU = 0.0

    if (ZEVEN) then
        ! even # of slices so final state is in "in"
        ierr = clEnqueueReadBuffer(command_queue, cl_wavecoeffin, CL_TRUE, 0_8, size_in_bytes_sgmaster,&
                   C_LOC(resGPU(1)), 0, C_NULL_PTR, C_LOC(event))
        call CLerror_check('DDD_STEMDCI:clEnqueueReadBuffer:cl_wavecoeffout', ierr)
    else if (ZODD) then
        ! odd # so final state is in "out"
        ierr = clEnqueueReadBuffer(command_queue, cl_wavecoeffout, CL_TRUE, 0_8, size_in_bytes_sgmaster,&
                   C_LOC(resGPU(1)), 0, C_NULL_PTR, C_LOC(event))
        call CLerror_check('DDD_STEMDCI:clEnqueueReadBuffer:cl_wavecoeffout', ierr)
    end if


    ! ierr = clEnqueueReadBuffer(command_queue, cl_wavecoeffout, CL_TRUE, 0_8, size_in_bytes_sgmaster,&
    !        C_LOC(resGPU(1)), 0, C_NULL_PTR, C_LOC(event))
    ! call CLerror_check('MD_STEMDCI:clEnqueueReadBuffer:cl_wavecoeffout', ierr)

    ! print *, abs(resGPU(1:5))

    ierr = clFinish(command_queue)
    call CLerror_check('DDD_STEMDCI:clFinish', ierr)
    

    ! ! write the results to the results array after each column
    do ii = 1,nsam
        resultsrow(1,mod(currenty,kji(2))+1,ii,1:nref) = abs(resGPU((ii-1)*correctsize+1:(ii-1)*correctsize+nref))
    end do

    if (mod(ixy,100).eq.0) print *, 'Completed pixel:', ixy+1, 'out of', symboxy * symboxx
    
    ! if mod(currenty,kji(2)) = 0 then write this hyperslab to the array in the HDF folder

    if (mod(currenty,kji(2)) == 0 .AND. currenty /= 0) then

        ! writept = 0
        nullify(HDF_head%next)
        ! Initialize FORTRAN HDF interface.
        call h5open_EMsoft(hdferr)

        ! open the existing file using the default properties.
        hdferr =  HDF_openFile(datafile, HDF_head)

        groupname = SC_EMData
        hdferr = HDF_openGroup(groupname, HDF_head)
        
        groupname = SC_STEMDCI
        hdferr = HDF_openGroup(groupname, HDF_head)

        dataset = SC_Intensities
        dims4 = (/  kji(3), kji(2), nsam, nref /)
        cnt4 = (/ 1, kji(2), nsam, nref /)
        offset4 = (/ currentx - 1, 0, 0, 0 /)
        hdferr = HDF_writeHyperslabFloatArray4D(dataset, resultsrow, dims4, offset4, cnt4, HDF_head, insert)

        call HDF_pop(HDF_head,.TRUE.)

        ! and close the fortran hdf interface
        call h5close_EMsoft(hdferr)

        call Message('Intermediate data stored in output file.')

        resultsrow = 0.0
    end if

!xy loop
end do 


call Message('--> Finished computation.')


end subroutine DDDSTEMcalc


!--------------------------------------------------------------------------
!
! SUBROUTINE:MarkStrong
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief mark all reflection in reflist pointer as strong
!
!> @param reflist reflectionlist type
!
!> @date 07/11/17  SS  1.0 original
!-------------------------------------------------------------------------
recursive subroutine MarkStrong(reflist, nref)

use typedefs

IMPLICIT NONE

type(reflisttype),pointer                       :: reflist
integer(kind=irg),INTENT(IN)                    :: nref

type(reflisttype),pointer                       :: tmpreflist, tmpreflist2
integer(kind=irg)                               :: ii

tmpreflist  => reflist%next
tmpreflist%strong =.TRUE.
tmpreflist%weak   = .FALSE.
tmpreflist2 => tmpreflist
nullify(tmpreflist2%nextw)

do ii = 2,nref
    tmpreflist => tmpreflist%next
    tmpreflist2%nexts => tmpreflist
    tmpreflist%strong =.TRUE.
    tmpreflist%weak   = .FALSE.
    tmpreflist2 => tmpreflist
end do
nullify(tmpreflist2%nexts)

end subroutine MarkStrong

!--------------------------------------------------------------------------
!
! SUBROUTINE:FindUnion
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Find union of two reflection list. add it to the master list
!
!> @param reflist_master master reflection list
!> @param reflist_new    new input reflection list
!
!> @date 12/17/17  SS  1.0 original
!-------------------------------------------------------------------------
recursive subroutine FindUnion(reflist_master, reflist_new, nref_master, nref_new)

use local
use typedefs
use gvectors

IMPLICIT NONE

type(reflisttype),pointer               :: reflist_master
type(reflisttype),pointer               :: reflist_new
integer(kind=irg),INTENT(INOUT)         :: nref_master
integer(kind=irg),INTENT(IN)            :: nref_new

type(reflisttype),pointer               :: tmp1, tmp2
integer(kind=irg)                       :: ii, jj, hkl1(3), hkl2(3)
logical                                 :: isnew

if(nref_master .eq. 0) then
    reflist_master  =>  reflist_new
    nref_master     =   nref_new
else
    tmp1  =>  reflist_new%next
    do 

        hkl1  =   tmp1%hkl

        tmp2  =>  reflist_master%next
        isnew = .TRUE.
        do 

            hkl2  =  tmp2%hkl
            if(sum(abs(hkl1 - hkl2)) .lt. 1.E-6) then
                isnew = .FALSE.
                EXIT
            end if

            if(.not. associated(tmp2%next)) exit
            tmp2  =>  tmp2%next
        end do

        if(isnew) then
            allocate(tmp2%next)
            tmp2            => tmp2%next
            nullify(tmp2%next)
            tmp2%hkl        = tmp1%hkl           ! store Miller indices
            nref_master     =  nref_master + 1
        end if

        if(.not. associated(tmp1%next)) exit
        tmp1  =>  tmp1%next
    end do

end if


end subroutine FindUnion
