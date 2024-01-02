! ###################################################################
! Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:PFInversion.f90
!--------------------------------------------------------------------------
!
! PROGRAM: PFMain 
!
!> @author Saransh Singh, Carnegie Mellon University
!
!--------------------------------------------------------------------------
program PFMain

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
use PFInversionHDF
use Lambert
use NameListTypedefs
use NameListHandlers
use HDF5
use HDFsupport
use InitializersHDF
use symmetry
use crystal
use others
use MRCmod

IMPLICIT NONE

character(fnlen)                   :: progname, progdesc, nmldeffile
logical                            :: verbose
type(PFInversionNameListType)      :: epf
type(PoleFigures),pointer          :: PF

type(unitcell)                     :: cell
type(DynType)                      :: Dyn
type(gnode)                        :: rlp
integer(kind=irg)                  :: eqvplanes(48,3)

integer(kind=irg)                  :: ncub, nLam, pcnt
real(kind=dbl),allocatable,target  :: ODF(:,:,:), PFLam(:,:), PFstereo(:,:), ODFcalc(:,:,:), PFrecon(:,:,:), PFLam2(:,:),&
                                      wfLP(:,:), BigA(:,:), b(:), b2(:), dth(:), odflin(:), Xmodel(:), BigA2(:,:), BigAlin(:)
real(kind=dbl),allocatable,target  :: Xl(:), Xu(:), C(:), bb(:), nnzvals(:), nnzvalssorted(:)
real(kind=sgl),allocatable         :: nnzidcolreal(:), nnzidrowreal(:), nnzidlinreal(:)
real(kind=dbl),allocatable         :: nnzidlindbl(:)
integer(kind=ill),allocatable      :: nnzidlinsorted(:)
integer(kind=irg),allocatable      :: nnzidcolsorted(:), nnzidrowsorted(:)
integer(kind=ill)                  :: idlam, idlin, idcub
real(kind=dbl),target              :: damp, mu(9), nu(5), L2recon, L2phantom
integer(kind=irg),target           :: mmm, nnn, nonz, nnz
integer(kind=irg),allocatable,target      :: indexlistlin(:), nnzcol(:), nnzcol2(:), nnzcolid(:)
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

integer(kind=irg)                  :: mm, nn, kwidth
real(kind=dbl),allocatable,target  :: X(:)

type(HDFobjectStackType)           :: HDF_head_cell
type(sparse_ll),pointer            :: sparseA, sparseA2, tailA, tailA2

type(MRCstruct)                    :: MRCheader
type(FEIstruct)                    :: FEIheaders(1024)
real(kind=dbl),allocatable         :: psum(:)
real(kind=dbl),allocatable         :: volume(:,:,:),volumeout(:,:,:)          ! you'll need to fill this array with values ... 
integer(kind=irg)                  :: numx, numy, numz                        ! set these to the size of the volume array

interface
    recursive subroutine bclsf90(m ,n, nonz, ids, vals, nnzcols, b, c, mu, bl, bu, X)

    use local
    use ISO_C_BINDING
    
    integer(C_INT),target,INTENT(IN)             :: m, n, nonz, nnzcols(n+1)
    integer(C_INT),target,INTENT(IN)             :: ids(nonz)
    real(C_DOUBLE),target,INTENT(IN)             :: vals(nonz), b(m), c(n), bl(n), bu(n)
    real(C_DOUBLE),target,INTENT(IN)             :: mu
    real(C_DOUBLE),target,INTENT(INOUT)          :: X(n)

    type(C_PTR)                                  :: out_ptr
    real(C_DOUBLE),pointer                       :: Xintd(:)

    end subroutine bclsf90
end interface

! name list file definition
nmldeffile = 'PFInversion.nml'
progname = 'PFInversion.f90'
progdesc = 'Program to invert pole figures using cubochoric representation and MBIR'
verbose = .TRUE.

call timestamp(datestring=dstr, timestring=tstrb)

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 94 /), progname)

call GetPFInversionNameList(nmldeffile, epf)

! Initialize HDF5 FORTRAN interface.
call h5open_EMsoft(hdferr)

! initialize some parameters
xtalname = trim(epf%xtalname)
pgnum = GetPointGroup(xtalname,.FALSE.)
nLam = epf%nLam
ncub = epf%ncub
dtor = cPi/180.D0
delta = 1.D0/dble(nLam)

! allocate cell
!nullify(cell)        
!allocate(cell)        

nullify(HDF_head_cell%next)
fname = trim(EMsoft_getXtalpathname())//trim(xtalname)
fname = EMsoft_toNativePath(fname)
hdferr =  HDF_openFile(fname, HDF_head_cell)
call HDFerror_check('ReadDataHDF:HDF_openFile:'//trim(fname), hdferr)

! initialize unitcell including symmetry etc.
call Initialize_Cell(cell,Dyn,rlp,xtalname, 0.05, 10.0, verbose, HDF_head_cell)

! allocate the dict structure
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum
numeqv = 0
pgnum = dict%pgnum

! initialize the symmetry matrices
call DI_Init(dict,'nil') 
Pmdims = dict%Nqsym

FZtype = FZtarray(pgnum)
FZorder = FZoarray(pgnum)

! write some information

call Message('Xtalname :'//trim(xtalname))

io_int(1) = pgnum
call WriteValue('Point group number = ',io_int,1,"(I8)")

io_int(1:2) = (/FZtype, FZorder/)
call WriteValue('Fundamental Zone type and order: ',io_int,2,"(2I8)")

io_int(1) = Pmdims
call WriteValue('Number of symmetry operators: ',io_int,1,"(I8)")

!==================================================================
!============READ POLE FIGURE DATA FROM FILE=======================
!==================================================================
allocate(PFLam(-nLam:nLam,-nLam:nLam))
PFLam = 0.D0

allocate(PF)

call GetPoleFigureData(epf, PF, verbose) 
do ii = 1,epf%nfiles
    print*,'(hkl), wf = ',PF%hkl(1:3,ii),PF%wf(ii)
end do

!==================================================================
!============ALLOCATE 	AND INITIALIZE MODEL ODF===================
!==================================================================
allocate(b(epf%nfiles*(2*nLam+1)**2))
b = 0.D0

!#############################################################################################
!==============================================================================================
! GENERATING FORWARD MODEL IN SPARSE FORMAT 
!==============================================================================================
!#############################################################################################

mm = (epf%nfiles)*(2*nLam+1)**2
nn = (2*ncub+1)**3

io_int(1:2) = (/mm,nn/)
call WriteValue('Dimensions of the forward model = ',io_int,2,'(2I10)')

nth = 181
allocate(dth(1:nth))
dth = 0.D0

dth = (/(2.D0*cPi*float(ii-1)/(float(nth-1)),ii=1,nth)/)

call Message('Initializing forward model ...')

LPapn = LPs%ap/2.D0

! initializing the matrix using the fundamental equation
! relating PF intensity and ODF (line integral)
! using linked list sparse matrix representation

nullify(sparseA,tailA)
allocate(sparseA)
tailA => sparseA

nonz = 0

! line integrals

do k = 1,epf%nfiles ! each pole figure

    eqvplanes = 0
    numeqv = 0
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

            xy = (/dble(i)/dble(nLam), dble(j)/dble(nLam)/)
            xyz = LambertSquareToSphere(xy,ierr)

            if (ierr .eq. 0) then
               xyz = xyz/vecnorm(xyz)
            else
                call FatalError('LambertSquareToSphere:','Coulnd not convert lambert square to sphere')
            end if

!       hcrossy =  PFhkl_f x xyz; 
            hcrossy =  (/ hkl_f(2)*xyz(3) - hkl_f(3)*xyz(2),&
                          hkl_f(3)*xyz(1) - hkl_f(1)*xyz(3),&
                          hkl_f(1)*xyz(2) - hkl_f(2)*xyz(1)/)
  
            if(sum(abs(hcrossy)) .ne. 0.D0) then
                hcrossy = hcrossy/vecnorm(hcrossy)
            else
                hcrossy = (/1.D0, 0.D0, 0.D0/)
            end if

            cth = DOT_PRODUCT(hkl_f,xyz)

            qu = ax2qu((/hcrossy(1:3),-acos(cth)/))
            if(qu(1) .lt. 0.D0) qu = -qu

            do kk = 1,nth
            
                qu2 = ax2qu((/xyz(1),xyz(2),xyz(3),dth(kk)/))
                if(qu2(1) .lt. 0.D0) qu2 = -qu2

                qures  = quat_mult(qu,qu2)
                if(qures(1) .lt. 0.D0) qures = -qures 
                    
                eu = qu2eu(qures)

                ! only points in RFZ are kept
                call ReduceOrientationtoRFZ(eu, dict, FZtype, FZorder, eu2)
 
                cu = eu2cu(eu2)

                nbx = cu(1)*dble(ncub)/LPapn
                nby = cu(2)*dble(ncub)/LPapn
                nbz = cu(3)*dble(ncub)/LPapn

                nix = floor(nbx)
                niy = floor(nby)
                niz = floor(nbz)
 
                if(nix < -ncub) nix = -ncub
                if(niy < -ncub) niy = -ncub
                if(niz < -ncub) niz = -ncub
 
                nixp = nix + 1
                niyp = niy + 1
                nizp = niz + 1
 
                if(nixp > ncub) nixp = ncub
                if(niyp > ncub) niyp = ncub
                if(nizp > ncub) nizp = ncub

                dx = nbx - nix
                dy = nby - niy
                dz = nbz - niz

                dxm = 1.D0 - dx
                dym = 1.D0 - dy
                dzm = 1.D0 - dz
            
                nix = nix + ncub + 1
                niy = niy + ncub + 1
                niz = niz + ncub + 1

                nixp = nixp + ncub + 1
                niyp = niyp + ncub + 1
                nizp = nizp + ncub + 1

                idcub = (niz - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nix
                idlin = (idcub - 1)*mm + idlam
                wval  = dxm*dym*dzm/dble(numeqv)
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

                idcub = (niz - 1)*(2*ncub + 1)**2 + (niy - 1)*(2*ncub + 1) + nixp
                idlin = (idcub - 1)*mm + idlam
                wval  = dx*dym*dzm/dble(numeqv)
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


                idcub = (niz - 1)*(2*ncub + 1)**2 + (niyp - 1)*(2*ncub + 1) + nix
                idlin = (idcub - 1)*mm + idlam
                wval  = dxm*dy*dzm/dble(numeqv)
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

allocate(nnzidcolsorted(nonz),nnzidrowsorted(nonz),nnzvalssorted(nonz))
nnzidcolsorted = 0
nnzidrowsorted = 0
nnzvalssorted = 0.D0

!=============================================================================================
!=========================================SORTING VALUES======================================
!=============================================================================================

! extracting non-zero values from linked list for
! further processing
! sorting the values in column major order
allocate(nnzidlinreal(nonz), nnzidlindbl(nonz))
nnzidlinreal = 0.D0
nnzidlindbl  = 0.D0

nullify(tailA)
tailA => sparseA

do ii = 1,nonz
    nnzidlinreal(ii) = float(tailA%idlin)
    tailA => tailA%next 
end do

! extracting non-zero values from linked list for
! further processing
! sorting the values in column major order
allocate(indexlistlin(nonz))
indexlistlin = 0

do ii = 1,nonz
    indexlistlin(ii) = ii
end do

call Message('--> sorting values')

nnzidlindbl = nnzidlinreal
call qsortd(nnzidlindbl, indexlistlin, nonz)

!call SSORT(nnzidlinreal, indexlistlin, nonz, 2)

!=============================================================================================
!====================================DONE SORTING VALUES======================================
!=============================================================================================

allocate(nnzidlinsorted(nonz))
nnzidlinsorted = 0


! extracting non-zero values from linked list for
! further processing
! sorting the values in column major order
allocate(nnzidcolreal(nonz))
nnzidcolreal = 0.D0

nullify(tailA)
tailA => sparseA

do ii = 1,nonz
    nnzidcolreal(ii) = float(tailA%idcol)
    tailA => tailA%next 
end do

! extracting non-zero values from linked list for
! further processing
! sorting the values in column major order
allocate(nnzidrowreal(nonz))
nnzidrowreal = 0.D0

nullify(tailA)
tailA => sparseA

do ii = 1,nonz
    nnzidrowreal(ii) = float(tailA%idrow)
    tailA => tailA%next 
end do

! extracting non-zero values from linked list for
! further processing
! sorting the values in column major order
allocate(nnzvals(nonz))
nnzvals = 0.D0

nullify(tailA)
tailA => sparseA

do ii = 1,nonz
    nnzvals(ii) = tailA%val
    tailA => tailA%next 
end do

call Message('--> deleting linked list of sparse matrix')

tailA => sparseA
tailA2 => tailA%next

do ii = 1,nonz
    nullify(tailA)
    tailA  => tailA2
    tailA2 => tailA%next
end do

nullify(tailA,tailA2)

do ii = 1,nonz
    nnzidlinsorted(ii) = nnzidlinreal(indexlistlin(ii))
    nnzvalssorted(ii)  = nnzvals(indexlistlin(ii))
    nnzidcolsorted(ii) = nnzidcolreal(indexlistlin(ii))
    nnzidrowsorted(ii) = nnzidrowreal(indexlistlin(ii))
end do

deallocate(nnzidcolreal,nnzidrowreal,nnzvals,nnzidlinreal,indexlistlin, nnzidlindbl)

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
tailA%val = nnzvalssorted(1)
nnz = 1

do ii = 2,nonz
    if(nnzidlinsorted(ii) .eq. nnzidlinsorted(ii-1)) then
        tailA%val = tailA%val + nnzvalssorted(ii)
    else if(nnzidlinsorted(ii) .ne. nnzidlinsorted(ii-1)) then
        allocate(tailA%next)
        tailA => tailA%next
        nullify(tailA%next)
        tailA%idrow = nnzidrowsorted(ii)
        tailA%idcol = nnzidcolsorted(ii)
        tailA%idlin = nnzidlinsorted(ii)
        tailA%val = nnzvalssorted(ii) 
        nnz = nnz + 1        
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
nnzcol  = 0
nnzcol2 = 0

deallocate(nnzidrowsorted,nnzidcolsorted,nnzvalssorted)
deallocate(nnzidlinsorted)

allocate(nnzcolid(nnz),nnzvalssorted(nnz))
nnzcolid = 0
nnzvalssorted = 0

call Message('generating final list of non zero array')

nullify(tailA)
tailA => sparseA2

! set up the arrays for bclsf90 subroutine
do ii = 1,nnz
    nonz = tailA%idcol
    nnzcolid(ii) = tailA%idrow - 1
    nnzcol2(nonz) = nnzcol2(nonz) + 1
    nnzvalssorted(ii) = tailA%val
    tailA => tailA%next 
end do

do ii = 2,nn+1
    nnzcol(ii) = nnzcol(ii-1) + nnzcol2(ii)
end do

call Message('--> deleting linked list of sparse matrix')

tailA => sparseA2
tailA2 => tailA%next

do ii = 1,nnz-1
    nullify(tailA)
    tailA  => tailA2
    tailA2 => tailA%next
end do

nullify(tailA,tailA2)
deallocate(nnzcol2)

call Message('Done ...')

!#############################################################################################
!==============================================================================================
! DONE CREATING FORWARD MODEL
!==============================================================================================
!#############################################################################################

! final set of allocations

allocate(X(nn),bb(mm))
X = 0.D0

allocate(Xl(nn), Xu(nn))

mmm = mm
nnn = nn

! lower and upper bound respectively
Xl = 0.D0
Xu = 1.0D5
X = 0.D0

!#############################################################################################
!==============================================================================================
! RECONSTRUCTING CUBE TEXTURE TO MATCH WITH INITIAL PHANTOM
!==============================================================================================
!#############################################################################################

! initialize bb, the set of experimental measurements
! the pole figure intensities are normalized in the final step
! to add to a total intensity of 2*PI
!call Message('--> initializing experimental pole figure values')

do k = 1,epf%nfiles
    do i = -nLam,nLam
        do j = -nLam,nLam
            idlam = (k-1)*(2*nLam+1)**2 + (i+nLam)*(2*nLam+1) + j + nLam + 1
            bb(idlam) = PF%PFhkl(i,j,k)
        end do
    end do
    bb((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2) = bb((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2)/&
    sum(bb((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2))
end do

bb = bb*(2*nLam+1)**2

!#############################################################################################
!==============================================================================================
! DONE RECONSTRUCTING CUBE TEXTURE TO MATCH WITH INITIAL PHANTOM
!==============================================================================================
!#############################################################################################


! main call to bound constrained regularized least square routine
! the rouitne takes the matrix A (forward model) and vector bb
! (experimental observation) and computes the vector X such as to
! minimize 1/2*||AX - bb||^2 + damp*||X||^2 + C^T*X s.t. Xl<=X<=Xu

call Message('--> Starting optimization routines')

damp = epf%damp

X = 0.D0
call bclsf90(mmm, nnn, nnz, nnzcolid, nnzvalssorted, nnzcol, bb, C, damp, Xl, Xu, X)

k = 0
j = 0
do i = 1,nn
    if(X(i) .lt. 0.D0) k = k + 1
    if(X(i) .ne. 0.D0) j = j + 1
end do

! normalize the ODF to sum to volume of orientation space
! in case of cubochoric, it is equal to PI*PI/Pmdims

X = X/sum(X)
X = X*nn

io_int(1) = j
call WriteValue('Number of non-zero values in ODF = ',io_int,1,'(I10)')

io_real(1) = dble(j)/dble(nn)
call WriteValue('sparsity of ODF = ',io_real,1,'(F15.6)')

tindex = sum(X*X*cPi**4/dble(Pmdims)/dble(Pmdims))/(2*ncub+1)**3
tstrgth = sqrt(tindex)
io_real(1:2) = (/tindex,tstrgth/)
call WriteValue('Texture Index/strength = ',io_real,2,'(2F15.6)')

!==================================================================
!============calculate the forward projection of solution==========
!==================================================================

call Message('--> Calculating reconstructed pole figures')

! recalculated pole figures
! sparse matrix multiplication is used
b = 0.D0
call sparse_matmul(mm, nn, nnz, nnzcolid, nnzvalssorted, nnzcol, X, b)

do k = 1,epf%nfiles
    b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2) = b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2)/&
    sum(b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2))
    b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2) = b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2)*(2*nLam+1)**2
end do

! reformat new pole figure in matrix from column vector
allocate(PFrecon(-nLam:nLam,-nLam:nLam,1:epf%nfiles))
PFrecon = 0.D0

do k = 1,epf%nfiles
    do i = -nLam,nLam
        do j = -nLam,nLam 
            idlam = (k-1)*(2*nLam+1)**2 + (i+nLam)*(2*nLam+1) + j + nLam + 1
            PFrecon(i,j,k) = b(idlam)
        end do
    end do
end do

do k = 1,epf%nfiles
    b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2) = b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2)/&
    sum(b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2))
    b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2) = b((k-1)*(2*nLam+1)**2+1:k*(2*nLam+1)**2)*(2*nLam+1)**2
end do

allocate(ODF(-ncub:ncub,-ncub:ncub,-ncub:ncub))
ODF = 0.D0

numx = 2*ncub + 1
numy = numx
numz = numx

allocate(volume(numx,numy,numz),volumeout(numx,numy,numz))
volume = 0.D0
volumeout = 0.D0

do i = -ncub,ncub
    do j = -ncub,ncub
        do k = -ncub,ncub
            idcub = (i + ncub)*(2*ncub + 1)**2 + (j + ncub)*(2*ncub + 1) + k + ncub + 1
            ODF(i,j,k) = X(idcub)
            volume(k+ncub+1,j+ncub+1,i+ncub+1) = X(idcub)
        end do
    end do
end do

! smooth the volume ODF data for MRC file
! this step is optional
! this step can be replaced by putting gaussians at each intensity point as well
! for now we'll use the boxcar or the moving average algorithm
! the width of the smoothing will be a user specified parameter

kwidth = 2
call smooth3dreconstruction(volume,numx, numy, numz, kwidth)

volumeout = 0.D0
call ConvertCubochoric2Stereo3Dvol(dict,volume,volumeout,numx,numy,numz)

! ! define the relevant entries in the MRCheader structure (other entries are not important and 
! ! will be set to their default values)
MRCheader%nx = numx
MRCheader%ny = numy
MRCheader%nz = numz
MRCheader%mode = 2    ! for floating point output
MRCheader%mx = numx
MRCheader%my = numy
MRCheader%mz = numz
MRCheader%amin = minval(volumeout)
MRCheader%amax = maxval(volumeout)
MRCheader%amean = sum(volumeout)/float(numx)/float(numy)/float(numz)
MRCheader%xlen = numx
MRCheader%ylen = numy
MRCheader%zlen = numz

! !----------------------------------------------------------------
! ! fill the relevant entries in the FEIheaders; none of these values are 
! ! actually used, except for mean_int
allocate(psum(0:numz-1))
psum = sum(sum(volumeout,1),1)
do i=0,numz-1
    FEIheaders(i+1)%b_tilt = 0.0
    FEIheaders(i+1)%defocus = 0.0
    FEIheaders(i+1)%pixelsize = 1.0e-9
    FEIheaders(i+1)%magnification = 1000.0
    FEIheaders(i+1)%voltage = 0.0
    FEIheaders(i+1)%mean_int = psum(i)/float(numx)/float(numy)
end do

!==================================================================
!============WRITE OUTPUT FILES====================================
!==================================================================

fname = 'Reconstructed'
! textfiles for MTEX in the format theta rho intnsity; theta and rho are spherical angles
call WriteMTEXFiles(epf, PF, PFrecon, fname)

! HDF5 output file
call WritePFInversionH5Data(epf, dstr, tstrb, progname, nmldeffile, ODF, PF, PFrecon)

! MRC file I/O for chimera/fiji visualization
fname = trim(epf%mrcfile)
fname = trim(EMsoft_getEMdatapathname())//trim(fname)//'.mrc'
fname = EMsoft_toNativePath(fname)
call MRC_write_3Dvolume(MRCheader,FEIheaders,fname,numx,numy,numz,volumeout,verbose=.TRUE.)

fname = trim(epf%mrcfile)
fname = trim(EMsoft_getEMdatapathname())//trim(fname)//'-outline'//'.mrc'
fname = EMsoft_toNativePath(fname)
call WriteStereoOutline(MRCheader,FEIheaders,fname,numx,numy,numz)

! close the fortran HDF5 interface
call h5close_EMsoft(hdferr)

end program PFMain

!=================================

subroutine bclsf90(m ,n, nonz, ids, vals, nnzcols, b, c, mu, bl, bu, X)
!DEC$ ATTRIBUTES DLLEXPORT :: bclsf90

use local
use ISO_C_BINDING

interface
    recursive subroutine bclsf90wrapper (m ,n, nonz, ids, vals, nnzcols, b, c, mu, &
    bl, bu, X) bind(C, name = 'bclsf90wrapper')

    use ISO_C_BINDING

    IMPLICIT NONE
    
    integer(C_INT)    :: m
    integer(C_INT)    :: n
    integer(C_INT)    :: nonz
    integer(C_INT)    :: nnzcols(n+1) 
    integer(C_INT)    :: ids(nonz)
    real(C_DOUBLE)    :: vals(nonz)
    real(C_DOUBLE)    :: b(m)
    real(C_DOUBLE)    :: c(n)
    real(C_DOUBLE)    :: mu
    real(C_DOUBLE)    :: bl(n)
    real(C_DOUBLE)    :: bu(n)
    real(C_DOUBLE)    :: X(n)
    
    end subroutine bclsf90wrapper
end interface
    
integer(C_INT),target,INTENT(IN)             :: m, n, nonz, nnzcols(n+1)
integer(C_INT),target,INTENT(IN)             :: ids(nonz)
real(C_DOUBLE),target,INTENT(IN)             :: vals(nonz), b(m), c(n), bl(n), bu(n)
real(C_DOUBLE),target,INTENT(IN)             :: mu
real(C_DOUBLE),target,INTENT(INOUT)          :: X(n)

type(C_PTR)                                  :: out_ptr
real(C_DOUBLE),pointer                       :: Xintd(:)

!out_ptr = bclsf90wrapper(m, n, nonz, ids(1), vals(1), nnzcols(1), b(1), c(1), mu, bl(1), bu(1))
call bclsf90wrapper(m, n, nonz, ids(1), vals(1), nnzcols(1), b(1), c(1), mu, bl(1), bu(1), X(1))

!call C_F_POINTER(out_ptr, Xintd, (/n/))

!X = Xintd

end subroutine bclsf90

recursive subroutine sparse_matmul(mm, nn, nnz, colid, nnzvals, nnzcol, X, b)

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)            :: mm, nn, nnz
integer(kind=irg),INTENT(IN)            :: colid(nnz), nnzcol(nn+1)
real(kind=dbl),INTENT(IN)               :: nnzvals(nnz), X(nn)
real(kind=dbl),INTENT(OUT)              :: b(mm)

integer(kind=irg)                       :: ii, jj, kk
real(kind=dbl)                          :: xj, aij

b = 0.D0

do ii = 1,nn
    xj = X(ii)
    if(xj .eq. 0.D0) then
    ! relax
    else
        do jj =  nnzcol(ii)+1, nnzcol(ii+1)
            kk    =  colid(jj)+1
            aij   =  nnzvals(jj)
            b(kk) =  b(kk) + aij*xj
        end do
    end if
end do

end subroutine sparse_matmul

recursive subroutine smooth3dreconstruction(volume,numx, numy, numz, kwidth)

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)          :: numx, numy, numz
real(kind=dbl),INTENT(INOUT)          :: volume(numx,numy,numz)
integer(kind=irg),INTENT(IN)          :: kwidth

integer(kind=irg)                     :: ii, jj, kk

do ii = 1+kwidth,numx-kwidth
    do jj = 1+kwidth,numy-kwidth
        do kk = 1+kwidth,numz-kwidth
            volume(ii,jj,kk) = sum(volume(ii-kwidth:ii+kwidth,jj-kwidth:jj+kwidth,kk-kwidth:kk+kwidth))
            volume(ii,jj,kk) = volume(ii,jj,kk)/(2*kwidth+1)**3
        end do
    end do
end do

end subroutine smooth3dreconstruction
