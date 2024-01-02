! ###################################################################
! Copyright (c) 2015-2024, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMgammaSTEM.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgammaOpenCL
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief STEM-DCI image simulation for mix of gamma and gamma' phases for arbitraty 
!> orientation
!
!> @details Based on a conversation with Mike Mills on 11/01/13; question we 
!> try to answer is the following: how can we perform a dynamical simulation
!> of a two-phase microstructure, in which the precipitates have a structure
!> that has many reflections in common with the disordered matrix.  In this
!> particular case, obviously we have fcc Ni solid solution, and L1_2 ordered
!> gamma phase.  
!>
!> we need to load two crystal structures, compute the dynamical matrix for the 
!> ordered phase, then copy that for the disordered phase and recompute the 
!> scattering factors; to do this efficiently, we may want to re-order the 
!> reflections into two groups (fundamental and superlattice), although that is
!> not essential to the simulation. The most efficient way would be to arrange the
!> reflection list to have all fundamental reflecton as one block and the 
!> independent superlattice reflections for 3 fcc translation variants 1/2[110]
!> as three blocks. The microstructure can be loaded from a file, and has simple
!> integers on each voxel, indicating which phase the voxel belongs to.  Once
!> we have the scattering matrices for both phases, it is just a matter of running
!> down each integration column.  We can then also implement a bent foil, which
!> is equivalent to a beam tilt, so we can likely reuse some of the lacbed code. 
!>
!> Starting late June 2017:
!> program incorporated in EMsoftPrivate repo in preparation for collaboration
!> with OSU group; since this is a special "one-off" program, we keep it in the
!> OLIO folder...this is a variant of the original program to perform the propagation
!> of the electron beam on the GPU.
!
!
!> This program also incorporates the idea of fast STEM-DCI computations.
!> (conversation with Marc at ICOTOM 2017). We will validate the results
!> for a single column before we write a full scale image simulation
!> The idea states that A = A_diag + A_off-diag; only the A_diag elements change 
!> different beam directions
!>
!> @date 11/02/13 MDG 1.0 original (took a few days to get it right...)
!> @date 06/28/17 MDG 2.0 complete rewrite to current EMsoft libraries.
!> @date 07/05/17 SS  3.0 rewrite for the GPU
!> @date 11/20/17 SS  4.0 STEM-DCI mode on the GPU
!--------------------------------------------------------------------------
program EMgammaSTEM

use local
use files
use io
use NameListTypedefs
use NameListHandlers
use JSONsupport
use stringconstants

IMPLICIT NONE

character(fnlen)                   :: nmldeffile, progname, progdesc
type(EMgammaSTEMNameListType)      :: enl
integer(kind=irg)                  :: res

nmldeffile = 'EMgammaSTEM.nml'
progname = 'EMgammaSTEM.f90'
progdesc = 'Gamma-gamma'' microstructure dynamical image simulation in the STEM-DCI mode'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 0, 32 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
! call JSONreadEMgammaNameList(enl, nmldeffile, error_cnt)
  call Message('JSON input not yet implemented')
  STOP
else
  call GetEMgammaSTEMNameList(nmldeffile,enl)
end if

! perform the (near) zone axis computations
call ComputeGAMMASTEM(enl, progname, nmldeffile)

end program EMgammaSTEM

!--------------------------------------------------------------------------
!
! SUBROUTINE: computeGAMMASTEM
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute the N beam STEM-DCI modality for two phase 
!> gamma-gamma' microstructure with defects and translational variants
!
!> @param enl namelist
!> @param progname program name (for HDF5 file)
!> @param nmldeffile namelist file name
!
!> @date 12/19/17 SS 1.0 original working
!--------------------------------------------------------------------------
subroutine computeGAMMASTEM(enl, progname, nmldeffile)

use local
use constants
use initializers
use initializersHDF
use typedefs
use NameListTypedefs
use NameListHandlers
use HDFsupport
use NameListHDFWriters
use error
use io
use rotations
use crystal
use MBmodule
use diffraction
use gvectors
use math
use clsupport
use clfortran 

use ISO_C_BINDING

IMPLICIT NONE

type(EMgammaSTEMNameListType)					::  enl
character(fnlen)								::  nmldeffile, progname

type(unitcell)        							::  cell_g, cell_gp
type(DynType)									::  Dyn_g, Dyn_gp
type(gnode)										::  rlp_g, rlp_gp
logical											::  verbose

character(fnlen)								::  xtalname_g, xtalname_gp
real(kind=sgl),target							::  k(3), kk(3), kp(3), ku(3), kin(3), eu(3), qu(4), FN(3), gin(3), gout(3)
real(kind=sgl)									::  DtoR, RtoD, io_real(3), alpha, cdot
integer(kind=irg)								::  io_int(3), nvar(0:3), ncts(0:3)
integer(kind=irg),allocatable 					::  hklarray(:,:)

type(reflisttype),pointer						::  reflist_gp_tmp, reflist_gp, reflist_gp_rearr, tmpreflist, tmpreflist2
type(reflisttype),pointer						::  reflist_v0, reflist_v1, reflist_v2, reflist_v3
type(reflisttype),pointer						::  tmpreflist_v0, tmpreflist_v1, tmpreflist_v2, tmpreflist_v3
type(reflisttype),pointer						::  reflist_rearr, firstw_rearr, firstw_gp

type(BetheParameterType)						::  BetheParameters
integer(kind=irg),target						::  nref_gp_tmp, nref_gp, npx, npy, nz, nns_gp, nnw_gp
integer(kind=irg)								::  ii, jj, ll, mm, pp, qq ! loop variables
logical											::  isFun

real(kind=sgl),allocatable 						:: 	eta1(:,:,:), eta2(:,:,:), eta3(:,:,:), eta4(:,:,:)
real(kind=sgl),allocatable 						:: 	orderparam(:,:,:,:),dispfield(:,:,:,:), klist(:,:)


complex(kind=dbl),allocatable					::  DynMat_gp(:,:)
complex(kind=dbl),allocatable					::  DynMat_diag_gp(:,:), DynMat_off_diag_gp(:,:)
complex(kind=dbl),allocatable					::  DynMat_diag_g(:,:), DynMat_off_diag_g(:,:)
complex(kind=dbl),allocatable 					::	DynMat_g(:,:)
complex(kind=sgl),allocatable,target 			::  A_off_diag_gp(:,:), sgmaster(:), wavecoeffin(:), A_off_diag_g(:,:),&
													A_off_diag_gp_v(:,:)
real(kind=sgl),allocatable,target 				::	gx(:), gy(:), gz(:)

complex(kind=sgl),allocatable 					::	Var(:,:,:)
complex(kind=sgl),target 						::  qg0, cmplxvar, pq0
real(kind=sgl),target 							::	pf1, pf2, pf3, pf4, dx, dy, dz
real(kind=dbl)									::  ktmax, delta, thk, sg

real(kind=dbl),allocatable						::  image(:,:), image_approx(:,:)

integer(c_intptr_t),allocatable, target 		::  platform(:)
integer(c_intptr_t),allocatable, target 		::  device(:)
integer(c_intptr_t),target 						::  context
integer(c_intptr_t),target 						::  command_queue
integer(c_intptr_t),target 						::  prog
integer(c_intptr_t),target 						::  kernel, kernel2
integer(c_intptr_t),target						::  event
character(fnlen)								::  sourcefile, info
integer, parameter 								::  source_length = 50000
character(len=source_length, KIND=c_char),TARGET::  csource
integer(c_size_t),TARGET						::  slength
integer(c_int)									::  numd, nump, correctsize
integer(kind=8),target 							::  globalsize(2), localsize(2), globalsize2(2), localsize2(2)
type(c_ptr), TARGET 							::  psource
character(35),TARGET 							::  progoptions
integer(c_size_t)								::  cnum
character(len=source_length),TARGET 			::  source
integer(c_int32_t)								::  ierr, ierr2, pcnt, qcnt
character(13),TARGET 							::  kernelname, kernelname2
character(14, KIND=c_char),target 				::  ckernelname, ckernelname2
integer(kind=irg)								::  irec

real(kind=sgl),allocatable,target 				::  glist(:), om(:,:)
real(kind=sgl),target 							::  mlambda, conv, ma, tstart, tend
integer(kind=irg),target 						::  nsam, nref, nsam_correct
complex(kind=sgl),target 						::  cvals(9), coef(9)
type(sggamma),pointer 							::  sglist, tmpsglist

integer(c_intptr_t),target 						::  cl_glist, cl_FN, cl_om, cl_A_off, cl_coef, cl_SMf
integer(c_intptr_t),target 						::  cl_A2, cl_A3, cl_TT1, cl_TT2, cl_TT3, cl_tmp, cl_SM,&
													cl_wavecoeffin, cl_wavecoeffout, cl_A_g, cl_A_gp0, cl_A_gp1,&
													cl_gx, cl_gy, cl_gz
integer(c_intptr_t),target 						::  cl_A_gp2, cl_A_gp3, cl_A_dia, cl_wavecoefftmp
integer(c_size_t)								::  size_in_bytes_glist, size_in_bytes_FN, size_in_bytes_om
integer(c_size_t)								::  size_in_bytes_S_off, size_in_bytes_sgmaster,&
													size_in_bytes_coef, size_in_bytes_A_dia, size_in_bytes_SMf,&
													size_in_bytes_gminusgp

complex(kind=sgl),allocatable,target			::  resGPU(:), resGPU2(:)
real(kind=sgl),allocatable 						::  results(:,:,:,:)
integer(c_size_t),target 						::  start_time, end_time, exec_time, texec_time
integer(c_size_t),target 						::  return_bytes
integer(c_int64_t)								::  profile_props

type(HDFobjectStackType)        				::  HDF_head
integer(kind=irg)								:: 	hdferr
integer(HSIZE_T)								:: 	dims3(3), dims4(4), hkl(3)
character(fnlen)								:: 	microfile, datafile, groupname, dataset
logical											:: 	f_exists, readonly

complex(kind=dbl),allocatable					:: Aeff(:,:), Seff(:,:), wave(:), wave2(:)
complex(kind=dbl),allocatable					:: A_off(:,:), S_off(:,:), A_diag(:)

character(11)           						:: dstr
character(15)           						:: tstrb
character(15)           						:: tstre
character(fnlen,kind=c_char)                    :: line2(1)
logical 										:: overwrite = .TRUE.
interface
			subroutine FindUnion(reflist_master, reflist_in, nref_master, nref_new)
				use typedefs

				IMPLICIT NONE

				type(reflisttype),pointer				:: reflist_master
				type(reflisttype),pointer				:: reflist_in
				integer(kind=irg),INTENT(INOUT)			:: nref_master
				integer(kind=irg),INTENT(IN)			:: nref_new

			end subroutine FindUnion

end interface

interface
			subroutine chooseVariant(reflist)
				use typedefs

				IMPLICIT NONE

				type(reflisttype),pointer				:: reflist

			end subroutine chooseVariant

end interface

interface
			subroutine MarkStrong(reflist, nref)
				use typedefs

				IMPLICIT NONE

				type(reflisttype),pointer				:: reflist
				integer(kind=irg),INTENT(IN)			:: nref
			end subroutine MarkStrong

end interface

interface
			subroutine CalcVarPhase(reflist, Varout, nref)
				use typedefs

				IMPLICIT NONE

				type(reflisttype),pointer				:: reflist
				complex(kind=sgl),INTENT(OUT)			:: Varout(nref,nref,4)
				integer(kind=irg),INTENT(IN)			:: nref
			end subroutine CalcVarPhase

end interface

nullify(HDF_head%next)

call timestamp(datestring=dstr, timestring=tstrb)

! degree to radians and vice versa
DtoR = cPi/180.0
RtoD = 1.0/DtoR

RtoD = 1.0/DtoR
!====================================================================================
! 1. INITIALIZE UNIT CELL FOR TWO PHASES 
!====================================================================================
! nullify(cell_g,cell_gp)
! allocate(cell_g,cell_gp)

xtalname_g = trim(enl%gammaname)
xtalname_gp = trim(enl%gammapname)

verbose = .FALSE.

! initializing gamma phase 
call Initialize_Cell(cell_g, Dyn_g, rlp_g, xtalname_g, enl%dmin, enl%voltage, verbose)
call Message('--> Finished initializing gamma phase')

! initializing gamma prime phase
call Initialize_Cell(cell_gp, Dyn_gp, rlp_gp, xtalname_gp, enl%dmin, enl%voltage, verbose)
call Message('--> Finished initializing gamma prime phase')

! the old program considered parallel illumination i.e. in the CTEM mode.
! in this program, we will consider a converged probe, which is the STEM mode.
! Most of the imaging for these superalloy systems have been done in the
! STEM-DCI mode, so that is what we will implement. The computation time for
! a 256x256x128 voxel phase field simulation for a parallel illumination took
! a few hours. The STEM-DCI mode involves the same computation but for a whole
! list of such incident wave vectors. The only way to make this computationally
! tractable is to introduce some approximations. For this program, we will
! consider that the change in incidence direction of a few mrad will not change
! the off-diagonal components i.e. the beam coupling. However, the diagonal 
! components, which is the geometry of the diffraction will change. Thus for the
! various incident beam direction, the sg for various reciprocal reflections will
! be calculated. The structure matrix will be split into a sum of a diagonal and 
! an off-diagonal term. The final exponential of the sum will be computed using the
! Zassenhaus theorem. exp{A_diag + A_off-diag} = exp{A_diag/2}*exp{A_off-diag}*exp{A_diag/2}
! Furthermore, the exponential of a diagonal matrix is a diagonal matrix with exponential
! of individual terms, which makes the computation even faster. see:
!
! Christoph T Koch and John C H Spence, useful expansion of the exponential of the sum of
! two non-commuting matrices, one of which is diagonal, J. Phys. A: Math. Gen. 36 (2003) 803–816

! one important thing to note is that we need to move away from the Bethe approximation if
! we use this approximation. The Bethe potentials modifies both the diagonal and off-diagonal
! components of the dynamical matrix based on the diffraction geometry and therefore does not
! allow for the splitting of the problem in the form described above. We will compromise with 
! a larger size of the matrix for easier computation for different beam directions.
!====================================================================================
! 2. CALCULATE MASTER REFLECTION LIST
!====================================================================================

! convert euler angle to quaternion
eu(1:3) = enl%eu(1:3)*DtoR
qu = eu2qu(eu)

!=======================================================================
! the list of excited reflections for the different incident beam 
! directions will not be the same. however, for using the approximations
! (details further in the code), the number of beams for each of those
! beam directions needs to be the same. we work around this by finding
! the list of reflections for a bunch of the beam directions and taking
! the union of them as the final reflection list. 
!=======================================================================

! scale k by the inverse wavelength and convert to reciprocal frame
k = quat_LP(conjg(qu),(/0.0,0.0,1.0/))
FN = k/cell_g%mlambda
call TransSpace(cell_gp,FN,ku,'c','r')
call NormVec(cell_gp,FN,'c')
FN = k

nullify(reflist_gp, reflist_gp_tmp)
allocate(reflist_gp)

call Set_Bethe_Parameters(BetheParameters,.TRUE.)

nref_gp = 0

do ll = 1,3
	do mm = 0,7

		nullify(reflist_gp_tmp)

		nref_gp_tmp = 0
		k 			=  quat_LP(conjg(qu),(/0.0,0.0,1.0/))
		kp 			=  (/(ll/3.D0) * cos(mm*cPi/4.D0), (ll/3.D0) * sin(mm*cPi/4.D0), 0.D0/)
		kp 			=  quat_LP(conjg(qu),kp)
		k 			=  k + tan(enl%convergence/1000.D0) * kp 

		call NormVec(cell_gp,k,'c')

		k 			=  k/cell_g%mlambda

		call TransSpace(cell_g,k,kk,'c','r')

		call Initialize_ReflectionList(cell_gp, reflist_gp_tmp, BetheParameters, FN, kk, enl%dmin, nref_gp_tmp)

		call FindUnion(reflist_gp, reflist_gp_tmp, nref_gp, nref_gp_tmp)

	end do
end do

call Message('--> Finished initializing reflection list for gamma prime phase')
io_int(1) = nref_gp
call WriteValue('--> Number of reflections in gamma prime phase = ',io_int,1,'(I3)')


! this size will be the actual size. the array is padded with zeros to 
! improve performance of the kernel code
correctsize = ceiling(float(nref_gp)/16.0) * 16

! arrange the reflection list such that the fundamental reflections are separated
! from superlattice reflections. the superlattice reflections are further classified 
! into one of the three variats which only scatter among themselves. this will involve
! a new subroutine

! choosing which variant of the superlattice reflections 
tmpreflist => reflist_gp%next
do ii = 1,nref_gp
	tmpreflist%variant = 0
	call isFundamental(tmpreflist%hkl,isfun)
	if(.not. isfun) then
		call chooseVariant(tmpreflist)
	end if
	tmpreflist => tmpreflist%next
end do

! in this block of code we will take the variant type and arrange the reflection list such
! that each reflection type is arranged separately. this way the final dynamical matrix will 
! consist of 4 distict dynamical matrix potentially leading to a  speedup in the final
! computation, but that remains to be seen

nullify(reflist_v0,reflist_v1,reflist_v2,reflist_v3)
allocate(reflist_v0,reflist_v1,reflist_v2,reflist_v3)

tmpreflist => reflist_gp%next
ncts = 0
do ii = 1,nref_gp
	jj = tmpreflist%variant
	select case(jj)
	case(0)
		if(ncts(0) .eq. 0) then
			reflist_v0%next    => tmpreflist
			tmpreflist_v0      => tmpreflist
			ncts(0) = ncts(0) + 1
		else
			tmpreflist_v0%next => tmpreflist
			tmpreflist_v0      => tmpreflist
			ncts(0) = ncts(0) + 1
		end if
	case(1)
		if(ncts(1) .eq. 0) then
			reflist_v1%next    => tmpreflist
			tmpreflist_v1      => tmpreflist
			ncts(1) = ncts(1) + 1
		else
			tmpreflist_v1%next => tmpreflist
			tmpreflist_v1      => tmpreflist
			ncts(1) = ncts(1) + 1
		end if
	case(2)
		if(ncts(2) .eq. 0) then
			reflist_v2%next    => tmpreflist
			tmpreflist_v2      => tmpreflist
			ncts(2) = ncts(2) + 1
		else
			tmpreflist_v2%next => tmpreflist
			tmpreflist_v2      => tmpreflist
			ncts(2) = ncts(2) + 1
		end if
	case(3)
		if(ncts(3) .eq. 0) then
			reflist_v3%next    => tmpreflist
			tmpreflist_v3      => tmpreflist
			ncts(3) = ncts(3) + 1
		else
			tmpreflist_v3%next => tmpreflist
			tmpreflist_v3      => tmpreflist
			ncts(3) = ncts(3) + 1
		end if
	case DEFAULT
		call FatalError('EMgammSTEM:','reflection in none of the 4 variants.')
	end select
	tmpreflist => tmpreflist%next
end do

nullify(tmpreflist_v0%next,tmpreflist_v1%next,tmpreflist_v2%next,tmpreflist_v3%next)

! print information about these separate reflection types
io_int(1) = ncts(0)
call WriteValue('--> number of fundamental reflections = ',io_int,1,'(I3)')

io_int(1) = ncts(1)
call WriteValue('--> number of (1 0 0) + fundamental fcc type reflections = ',io_int,1,'(I3)')

io_int(1) = ncts(2)
call WriteValue('--> number of (0 1 0) + fundamental fcc type reflections = ',io_int,1,'(I3)')

io_int(1) = ncts(3)
call WriteValue('--> number of (1 1 0) + fundamental fcc type reflections = ',io_int,1,'(I3)')


! now all reflections have been sorted in their respective 
! linked lists. we will skip using the bethe approximation 
! since we want to explicitly take each super lattice reflection
! in account

! calculate dynamical matrix for each of these reflection lists
allocate(DynMat_off_diag_g(nref_gp, nref_gp), A_off_diag_g(correctsize,correctsize))
DynMat_off_diag_g 	= 	cmplx(0.D0, 0.D0)
A_off_diag_g 		=	cmplx(0.0, 0.0)

allocate(DynMat_g(ncts(0), ncts(0)))
DynMat_g 	=	cmplx(0.D0, 0.D0)
call GetDynMatMaster(cell_g, reflist_v0, DynMat_g, ncts(0))
DynMat_g 	= 	DynMat_g * cmplx(0.D0,1.D0)
DynMat_off_diag_g(1:ncts(0),1:ncts(0))	=	DynMat_g(1:ncts(0),1:ncts(0))
deallocate(DynMat_g)

allocate(DynMat_g(ncts(1), ncts(1)))
DynMat_g 	=	cmplx(0.D0, 0.D0)
call GetDynMatMaster(cell_g, reflist_v1, DynMat_g, ncts(1))
DynMat_g 	= 	DynMat_g * cmplx(0.D0,1.D0)
DynMat_off_diag_g(ncts(0)+1:sum(ncts(0:1)),ncts(0)+1:sum(ncts(0:1))) = DynMat_g(1:ncts(1),1:ncts(1))
deallocate(DynMat_g)

allocate(DynMat_g(ncts(2), ncts(2)))
DynMat_g 	=	cmplx(0.D0, 0.D0)
call GetDynMatMaster(cell_g, reflist_v2, DynMat_g, ncts(2))
DynMat_g 	= 	DynMat_g * cmplx(0.D0,1.D0)
DynMat_off_diag_g(sum(ncts(0:1))+1:sum(ncts(0:2)),sum(ncts(0:1))+1:sum(ncts(0:2))) = DynMat_g(1:ncts(2),1:ncts(2))
deallocate(DynMat_g)

allocate(DynMat_g(ncts(3), ncts(3)))
DynMat_g 	=	cmplx(0.D0, 0.D0)
call GetDynMatMaster(cell_g, reflist_v3, DynMat_g, ncts(3))
DynMat_g 	= 	DynMat_g * cmplx(0.D0,1.D0)
DynMat_off_diag_g(sum(ncts(0:2))+1:sum(ncts(0:3)),sum(ncts(0:2))+1:sum(ncts(0:3))) = DynMat_g(1:ncts(3),1:ncts(3))
deallocate(DynMat_g)

call Message('--> Done generating dynamical matrices for gamma phase')

! before we proceed to calculating the dynamical matrix for the gamma prime phase
! we need to make sure that the reflection list is arranged in the same fashion as
! we will arrange them for the gamma matrix. the fundamental reflections first, 
! followed by the [1 0 0] + fcc superlattice reflection and so on. this will involve
! creating a new linked list which will be passed onto the GetDynMat routine

nullify(reflist_gp_rearr)
allocate(reflist_gp_rearr)
nullify(reflist_gp_rearr%next)

tmpreflist            => reflist_v0%next
reflist_gp_rearr%next => tmpreflist

do
	if(.not. associated(tmpreflist%next)) exit
	tmpreflist  => tmpreflist%next
end do

tmpreflist%next => reflist_v1%next

do
	if(.not. associated(tmpreflist%next)) exit
	tmpreflist  => tmpreflist%next
end do

tmpreflist%next => reflist_v2%next

do
	if(.not. associated(tmpreflist%next)) exit
	tmpreflist  => tmpreflist%next
end do

tmpreflist%next => reflist_v3%next

! store the reflection list in array for HDF5 I/O
allocate(hklarray(3,nref_gp))
hklarray = 0

tmpreflist  =>  reflist_gp_rearr%next
do ii = 1,nref_gp
	hklarray(1:3,ii) 	=  tmpreflist%hkl
	tmpreflist 			=> tmpreflist%next
end do


allocate(DynMat_gp(nref_gp,nref_gp))
DynMat_gp = cmplx(0.D0,0.D0)

call GetDynMatMaster(cell_gp, reflist_gp_rearr, DynMat_gp, nref_gp)

DynMat_gp = DynMat_gp * cmplx(0.D0,1.D0)
call Message('--> Done generating dynamical matrices for gamma'' phase')

! calculate the phase shift due to the translational variants

! main array
allocate(Var(nref_gp,nref_gp,4))
Var = cmplx(1.D0,0.D0)

call CalcVarPhase(reflist_gp_rearr, Var, nref_gp)

allocate(DynMat_diag_gp(nref_gp,nref_gp), DynMat_off_diag_gp(nref_gp,nref_gp))
DynMat_diag_gp     = cmplx(0.D0,0.D0)
DynMat_off_diag_gp = cmplx(0.D0,0.D0)

do ii = 1,nref_gp
	do jj = 1,nref_gp
		if(ii .eq. jj) then
			DynMat_diag_gp(ii,jj)      	= DynMat_gp(ii,jj)
		else if(ii .ne. jj) then
			DynMat_off_diag_gp(ii,jj)  	= DynMat_gp(ii,jj)
			A_off_diag_g(ii,jj) 		= DynMat_off_diag_g(ii,jj)
		end if
	end do
end do

allocate(A_off_diag_gp(correctsize,correctsize), A_off_diag_gp_v(correctsize,correctsize))
A_off_diag_gp	= cmplx(0.0,0.0)
A_off_diag_gp_v	= cmplx(0.D0, 0.D0)
thk = 1.D0

ktmax = tan(enl%convergence/1000.0)
delta = 2*ktmax/20.0

call Message('--> Done generating dynamical matrices for gamma prime phase')

!=========================================================
! HDF5 I/O
!=========================================================
! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

microfile = trim(EMsoft_getEMdatapathname())//trim(enl%microstructurefile)
microfile = EMsoft_toNativePath(microfile)
inquire(file=trim(microfile), exist=f_exists)

if(f_exists) then
	readonly = .TRUE.
	call Message('--> opening '//trim(enl%microstructurefile), frm = "(A)" )
	hdferr =  HDF_openFile(microfile, HDF_head, readonly)
else
	call FatalError('EMgammaSTEM:','microstructure file not found')
end if

! open the MicrostructureData group
groupname = SC_MicrostructureData
hdferr = HDF_openGroup(groupname, HDF_head)

!==================================================================================

! as of 10/23/2017, we will follow the new data structure which has
! one file which contains the order parameter along with the displacement
! fields etc.
!==================================================================================

dataset = SC_OrderParameter ! order parameter
call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, orderparam)

dataset = SC_DisplacementField ! displacement field
call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, dispfield)

npx = dims4(2)
npy = dims4(3)
nz  = dims4(4)

io_int(1:3) = dims4(2:4)
call WriteValue('--> size of phase field microstructure (in voxels) = ',io_int,3,' (I3," X ",I3," X ",I3)')

allocate(eta1(npx,npy,nz),eta2(npx,npy,nz),eta3(npx,npy,nz),eta4(npx,npy,nz))

eta1 = orderparam(1,1:npx,1:npy,1:nz)
eta2 = orderparam(2,1:npx,1:npy,1:nz)
eta3 = orderparam(3,1:npx,1:npy,1:nz)
eta4 = orderparam(4,1:npx,1:npy,1:nz)

deallocate(orderparam)

call HDF_pop(HDF_head,.TRUE.)

!=========================================================================================================================
!OPENCL PORTION OF THE CODE BEGINS HERE
!=========================================================================================================================

!================================
! INITIALIZATION OF OpenCL DEVICE
!================================

call CLinit_PDCCQ(platform, nump, enl%platid, device, numd, enl%devid, info, context, command_queue)
call Message('--> Finished initializing the OPENCL devices.')
call Message('Device selected : '//trim(info))

! read the cl source file
sourcefile = 'gammaSTEM.cl'
call CLread_source_file(sourcefile, csource, slength)

!====================================
! INITIALIZATION OF OpenCL VARIABLES
!====================================
! global and local work group size for kernel 1
globalsize   =   (/correctsize, correctsize/)
localsize    =   (/16, 16/)

! global and local work group size for kernel 2
globalsize2	=	(/correctsize, correctsize*5/)
localsize2	=	(/correctsize, 1/)

! probe convergence angle [mRad]
conv     = enl%convergence

! number of reflections
nref     = nref_gp

! qg0
qg0      = cell_gp%LUTqg(0,0,0)

! master lsit of diagonal terms in the STEM-DCI calculation
! these values are already exponentiated, so should not be
! repeated on the GPU device

nullify(sglist, tmpsglist)
allocate(sglist)
tmpsglist  =>  sglist

allocate(klist(3,21*21))
klist = 0.0

call CalcUcg(cell_g, rlp_g, (/0,0,0/),applyqgshift=.TRUE. )
pq0 = cmplx(0.D0,1.D0/rlp_g%xgp,dbl)

pcnt = 1
qcnt = 1
do ll = -10,10
	do mm = -10,10

		if(ll**2 + mm**2 .le. 100) then

			kin 			=  (/ktmax * float(ll)/10.D0, ktmax * float(mm)/10.D0, 0.D0/) + (/0.D0, 0.D0, 1.D0/)
			call NormVec(cell_gp,kin,'c')
			kin 			=  quat_LP(conjg(qu),kin)
			kin 			=  kin/cell_gp%mlambda
			call TransSpace(cell_gp,kin,ku,'c','r')
			klist(1:3,qcnt) =  ku

			tmpreflist => reflist_gp_rearr%next
			do ii = 1,nref_gp

				sg              =	calcsg(cell_gp,float(tmpreflist%hkl),ku,FN)
				cmplxvar        =  	0.5D0 * thk * cPi * (2.D0 * sg + qg0) ! + pq0 the 0.5 factor is because of e^Bt/2 e^At e^Bt/2
				cmplxvar		=	cmplxvar * cmplx(0.D0, 1.D0)

				tmpsglist%sg  	=	sg
				tmpsglist%expsg	=	exp(cmplxvar)
				tmpsglist%hkl 	=	tmpreflist%hkl

				allocate(tmpsglist%next)
				tmpsglist		=>	tmpsglist%next
				nullify(tmpsglist%next)

				tmpreflist 		=>  tmpreflist%next
				pcnt = pcnt + 1
			end do
			qcnt = qcnt + 1
		end if
	end do
end do

! sampling density (total number of plane waves in STEM calculation)
nsam     		= 	pcnt / nref_gp
nsam_correct	=	ceiling(float(nsam)/16.0) * 16

allocate(sgmaster(correctsize * nsam_correct))
sgmaster = cmplx(0.0,0.0)

tmpsglist  =>  sglist
do ii = 1, nsam
	do jj = 1, nref_gp
		sgmaster((ii-1)*correctsize + jj) =  tmpsglist%expsg
		tmpsglist	=>	tmpsglist%next
	end do
end do

call Message('--> Done creating master excitation error list')

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

! A_off_diag single precision
A_off_diag_gp(1:nref_gp,1:nref_gp)  =   thk * DynMat_off_diag_gp(:,:)
A_off_diag_g 					 	=	thk * A_off_diag_g


! wave function coefficients
allocate(wavecoeffin(nsam * correctsize))
wavecoeffin 	=	cmplx(0.0,0.0)
do ii = 1,nsam
	wavecoeffin((ii - 1)*correctsize + 1)	=	cmplx(1.0,0.0)
end do

! g - g'
allocate(gx(correctsize**2),gy(correctsize**2),gz(correctsize**2))
gx = 0.0
gy = 0.0
gz = 0.0

! all the (g - g') reciprocal lattice vectors are computed and copied 
! to the GPU here
tmpreflist => reflist_gp_rearr%next
do ii = 1,nref_gp
	tmpreflist2 => reflist_gp_rearr%next
	do jj = 1,nref_gp
		if(ii .ne. jj) then
			hkl = tmpreflist%hkl - tmpreflist2%hkl
			gx((ii-1)*nref_gp + jj) = float(hkl(1))
			gy((ii-1)*nref_gp + jj) = float(hkl(2))
			gz((ii-1)*nref_gp + jj) = float(hkl(3))
		end if
		tmpreflist2 => tmpreflist2%next
	end do
	tmpreflist => tmpreflist%next
end do

! size of the different variables
size_in_bytes_glist     =   nref_gp * 3 * sizeof(glist(1))
size_in_bytes_FN        =   3 * sizeof(FN(1))
size_in_bytes_om        =   9 * sizeof(om(1,1))
size_in_bytes_coef      =   9 * sizeof(coef(1))
size_in_bytes_S_off     =   correctsize * correctsize * sizeof(A_off_diag_gp(1,1))
size_in_bytes_sgmaster  =   correctsize * nsam_correct* sizeof(sgmaster(1)) 
size_in_bytes_SMf		=	correctsize * correctsize * nsam_correct * sizeof(sgmaster(1))
size_in_bytes_gminusgp  = 	correctsize * correctsize * sizeof(gx(1))
call Message('--> All quantities for kernel code calculated. Launching OpenCL part of code...')

!============================================================
!GENERATE ALL THE BUFFERS AND ALLOCATE RESULT ARRAY
!============================================================

cl_A_off   	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_off', ierr)

cl_A_g   	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_g', ierr)

cl_A_gp0   	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_gp', ierr)

cl_A_gp1   	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_gp', ierr)

cl_A_gp2   	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_gp', ierr)

cl_A_gp3   	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_gp', ierr)

cl_coef    	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_coef, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_coef', ierr)

cl_A2      	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A2', ierr)

cl_A3      	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A3', ierr)

cl_TT1    	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_TT1', ierr)

cl_TT2     	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_TT2', ierr)

cl_TT3     	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_TT3', ierr)

cl_tmp     	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_tmp', ierr)

cl_SM      	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_S_off, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_SM', ierr)

cl_SMf     	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_SMf, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_SMf', ierr)

cl_wavecoeffin =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_sgmaster, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_wavecoeffin', ierr)

cl_wavecoeffout =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_sgmaster, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_wavecoeffout', ierr)

cl_wavecoefftmp =   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_SMf, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_wavecoefftmp', ierr)

cl_A_dia	=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_sgmaster, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_A_dia', ierr)

cl_gx		=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_gx', ierr)

cl_gy		=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_gy', ierr)

cl_gz		=   clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('gamma_STEMDCI:clCreateBuffer:cl_gz', ierr)

call Message('--> Done allocating all GPU buffers.')

!=====================================
!COPY ALL THE DATA TO THE BUFFERS
!=====================================
A_off_diag_gp_v(1:nref_gp,1:nref_gp)	=	A_off_diag_gp(1:nref_gp,1:nref_gp) * Var(1:nref_gp,1:nref_gp,1)
ierr = clEnqueueWriteBuffer(command_queue, cl_A_gp0, CL_TRUE, 0_8, size_in_bytes_S_off, &
						  C_LOC(A_off_diag_gp_v(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_S_off', ierr)

A_off_diag_gp_v(1:nref_gp,1:nref_gp)	=	A_off_diag_gp(1:nref_gp,1:nref_gp) * Var(1:nref_gp,1:nref_gp,2)
ierr = clEnqueueWriteBuffer(command_queue, cl_A_gp1, CL_TRUE, 0_8, size_in_bytes_S_off, &
						  C_LOC(A_off_diag_gp_v(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_S_off', ierr)

A_off_diag_gp_v(1:nref_gp,1:nref_gp)	=	A_off_diag_gp(1:nref_gp,1:nref_gp) * Var(1:nref_gp,1:nref_gp,3)
ierr = clEnqueueWriteBuffer(command_queue, cl_A_gp2, CL_TRUE, 0_8, size_in_bytes_S_off, &
						  C_LOC(A_off_diag_gp_v(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_S_off', ierr)

A_off_diag_gp_v(1:nref_gp,1:nref_gp)	=	A_off_diag_gp(1:nref_gp,1:nref_gp) * Var(1:nref_gp,1:nref_gp,4)
ierr = clEnqueueWriteBuffer(command_queue, cl_A_gp3, CL_TRUE, 0_8, size_in_bytes_S_off, &
						  C_LOC(A_off_diag_gp_v(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_S_off', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_A_g, CL_TRUE, 0_8, size_in_bytes_S_off, &
						  C_LOC(A_off_diag_g(1,1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_S_off', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_coef, CL_TRUE, 0_8, size_in_bytes_coef, &
						  C_LOC(coef(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_coef', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_A_dia, CL_TRUE, 0_8, size_in_bytes_sgmaster, &
						  C_LOC(sgmaster(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_A_dia', ierr)

! g - g' variables
ierr = clEnqueueWriteBuffer(command_queue, cl_gx, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
						  C_LOC(gx(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_gx', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_gy, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
						  C_LOC(gy(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_gy', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_gz, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
						  C_LOC(gz(1)), 0, C_NULL_PTR, C_LOC(event))
call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_gz', ierr)

ierr = clFinish(command_queue)
call CLerror_check('gamma_STEMDCI:clFinish', ierr)

call Message('--> Done data transfer to all GPU buffers.')

!======================
! BUILD THE KERNEL
!======================

! create the program
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('gamma_STEMDCI:clCreateProgramWithSource', ierr)

! build the program
progoptions = '-cl-no-signed-zeros -cl-mad-enable'//CHAR(0)
ierr = clBuildProgram(prog, numd, C_LOC(device), C_LOC(progoptions), C_NULL_FUNPTR, C_NULL_PTR)
!ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(enl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
if(len(trim(source)) > 0) call Message(trim(source(1:cnum)),frm='(A)')
call CLerror_check('gamma_STEMDCI:clBuildProgram', ierr)
call CLerror_check('gamma_STEMDCI:clGetProgramBuildInfo', ierr2)

! finally get the kernel and release the program
kernelname = 'gamma_STEMDCI'
ckernelname = kernelname
ckernelname(14:14) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('gamma_STEMDCI:clCreateKernel', ierr)

! finally get the kernel and release the program
kernelname2 = 'PropagateBeam'
ckernelname2 = kernelname2
ckernelname2(14:14) = C_NULL_CHAR
kernel2 = clCreateKernel(prog, C_LOC(ckernelname2), ierr)
call CLerror_check('PropagateBeam:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('gamma_STEMDCI:clReleaseProgram', ierr)

call Message('--> Done creating OpenCL executable.')

!===========================
! SET KERNEL ARGUMENTS
!===========================

ierr =  clSetKernelArg(kernel, 0, sizeof(nsam_correct), C_LOC(nsam_correct))
call CLerror_check('gamma_STEMDCI:clSetKernelArg:nsam', ierr)

ierr =  clSetKernelArg(kernel, 1, sizeof(nref), C_LOC(nref))
call CLerror_check('gamma_STEMDCI:clSetKernelArg:nref', ierr)

ierr =  clSetKernelArg(kernel, 2, sizeof(cl_A_g), C_LOC(cl_A_g))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 3, sizeof(cl_A_gp0), C_LOC(cl_A_gp0))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 4, sizeof(cl_A_gp1), C_LOC(cl_A_gp0))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 5, sizeof(cl_A_gp2), C_LOC(cl_A_gp0))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 6, sizeof(cl_A_gp3), C_LOC(cl_A_gp0))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 7, sizeof(cl_A_off), C_LOC(cl_A_off))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:S_off', ierr)

ierr =  clSetKernelArg(kernel, 8, sizeof(cl_coef), C_LOC(cl_coef))
call CLerror_check('gamma_STEMDCI:clSetKernelArg:nref', ierr)

ierr =  clSetKernelArg(kernel, 9, sizeof(cl_A2), C_LOC(cl_A2))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:A2', ierr)

ierr =  clSetKernelArg(kernel, 10, sizeof(cl_A3), C_LOC(cl_A3))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:A3', ierr)

ierr =  clSetKernelArg(kernel, 11, sizeof(cl_TT1), C_LOC(cl_TT1))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:TT1', ierr)

ierr =  clSetKernelArg(kernel, 12, sizeof(cl_TT2), C_LOC(cl_TT2))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:TT2', ierr)

ierr =  clSetKernelArg(kernel, 13, sizeof(cl_TT3), C_LOC(cl_TT3))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:TT3', ierr)

ierr =  clSetKernelArg(kernel, 14, sizeof(cl_tmp), C_LOC(cl_tmp))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:tmp', ierr)

ierr =  clSetKernelArg(kernel, 15, sizeof(cl_SM), C_LOC(cl_SM))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:SM', ierr)

ierr =  clSetKernelArg(kernel, 16, sizeof(cl_A_dia), C_LOC(cl_A_dia))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:A_dia', ierr)

ierr =  clSetKernelArg(kernel, 17, sizeof(cl_SMf), C_LOC(cl_SMf))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:SMf', ierr)

ierr =  clSetKernelArg(kernel, 22, sizeof(cl_gx), C_LOC(cl_gx))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:gx', ierr)

ierr =  clSetKernelArg(kernel, 23, sizeof(cl_gy), C_LOC(cl_gy))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:gy', ierr)

ierr =  clSetKernelArg(kernel, 24, sizeof(cl_gz), C_LOC(cl_gz))
call CLerror_check('gamma_STEMDCI:clsetKernelArg:gz', ierr)

texec_time = 0.0

allocate(resGPU(nsam_correct * correctsize))  !,resGPU2(correctsize**2))
pcnt	=	1

!==============================================================
! FOR TESTING PURPOSE ONLY
!==============================================================
!allocate(Aeff(nref_gp,nref_gp),Seff(nref_gp,nref_gp),wave(nref_gp),wave2(nref_gp))
!allocate(A_off(nref_gp,nref_gp),S_off(nref_gp,nref_gp),A_diag(nref_gp))

!Aeff 		= cmplx(0.D0,0.D0)
!Seff 		= cmplx(0.D0,0.D0)
!wave 		= cmplx(0.D0,0.D0)
!wave(1) 	= cmplx(1.D0,0.D0)

!open(unit=13,file='/Users/saranshsingh/Desktop/CPU.txt',action='write')

!kin 			=  (/0.D0,0.D0,0.D0/) + (/0.D0,0.D0,1.D0/)
!call NormVec(cell_gp,kin,'c')
!kin 			=  quat_LP(conjg(qu),kin)
!kin 			=  kin/cell_gp%mlambda
!call TransSpace(cell_gp,kin,ku,'c','r')

!do ii = 1,128

!	pf1 	=	eta1(1,1,ii)
!	pf2 	=	eta2(1,1,ii)
!	pf3 	=	eta3(1,1,ii)
!	pf4 	=	eta4(1,1,ii)

!	Aeff	=	pf1 * DynMat_gp * Var(1:nref_gp,1:nref_gp,1)
!	Aeff	=	Aeff + pf2 * DynMat_gp * Var(1:nref_gp,1:nref_gp,2)
!	Aeff	=	Aeff + pf3 * DynMat_gp * Var(1:nref_gp,1:nref_gp,3)
!	Aeff	=	Aeff + pf4 * DynMat_gp * Var(1:nref_gp,1:nref_gp,4)
!	Aeff 	=	Aeff + (1.D0 - pf1 - pf2 - pf3 - pf4) * DynMat_off_diag_g(1:nref_gp,1:nref_gp)

!	do ll = 1,nref_gp
!		do mm = 1,nref_gp
!			if(ll .eq. mm) then
				!sg              =	calcsg(cell_gp,float(hklarray(1:3,ll)),kin,FN)
				!cmplxvar        =  	thk * cPi * (2.D0 * sg + qg0) ! the 0.5 factor is because of e^Bt/2 e^At e^Bt/2
				!cmplxvar		=	cmplxvar * cmplx(0.D0, 1.D0)
!				Aeff(ll,mm) 	=	(0.D0,0.D0)!cmplxvar
!			end if
!		end do
!	end do

!	call MatrixExponential(Aeff, Seff, thk, 'Tayl', nref_gp)
!if(ii .eq. 1) print*,Seff(1,1:5)

!	do ll = 1,nref_gp
!		do mm = 1,nref_gp
!			Seff(ll,mm) = Seff(ll,mm) * sgmaster(158*correctsize+ll) * sgmaster(158*correctsize+mm)
!		end do
!	end do

!	!do ll = 1,4
!	wave 	= 	matmul(Seff,wave)
!		!wave 	= 	wave2
!	!end do

!	write(13,*) realpart(wave(1)), imagpart(wave(1))
!end do
!close(13)

!open(unit=13,file='/Users/saranshsingh/Desktop/GPU.txt',action='write')
!open(unit=14,file='/Users/saranshsingh/Desktop/GPU2.txt',action='write')

!==============================================================
! END SECTION
!==============================================================

!============================================
! WRITE EVERYTHING TO HDF5 FILE
!============================================
datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
datafile = EMsoft_toNativePath(datafile)
inquire(file=trim(datafile), exist=f_exists)

if (f_exists) then
  call Message(' --> deleting old data file with the same name')
  open(unit=dataunit, file=trim(datafile), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

nullify(HDF_head%next)

hdferr =  HDF_createFile(datafile, HDF_head)

! write the EMheader to the file
dataset = 'STEMDCI'
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, dataset)

! add the CrystalData group at the top level of the file
call SaveDataHDF(cell_g, HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! read the nml and json files and write them as string arrays to the file
dataset = SC_STEMDCINML
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)

call HDFwriteGammaSTEMDCINameList(HDF_head, enl)

! leave this group
call HDF_pop(HDF_head)

!==============================================================
! MAIN COMPUTATIONAL LOOP
!==============================================================

ierr       = clReleaseEvent(event)
call CLerror_check('gamma_STEMDCI:clReleaseEvent', ierr)

! allocate final results array
allocate(results(npx,npy,nsam,nref_gp))
results = cmplx(0.D0,0.D0)

do ll = 1,npx
	do mm = 1,npy

! each scan point starts with [1 0 0 ...]
		ierr = clEnqueueWriteBuffer(command_queue, cl_wavecoeffin, CL_TRUE, 0_8, size_in_bytes_sgmaster, &
						  C_LOC(wavecoeffin(1)), 0, C_NULL_PTR, C_LOC(event))
		call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_wavecoeffin', ierr)

		do ii = 1,nz

			pf1 	=	eta1(ll,mm,ii)
			pf2 	=	eta2(ll,mm,ii)
			pf3 	=	eta3(ll,mm,ii)
			pf4 	=	eta4(ll,mm,ii)

			dx 		=	dispfield(1,ll,mm,ii)
			dy 		=	dispfield(2,ll,mm,ii)
			dz 		=	dispfield(3,ll,mm,ii)

			ierr 	=  clSetKernelArg(kernel, 18, sizeof(pf1), C_LOC(pf1))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:eta1', ierr)

			ierr 	=  clSetKernelArg(kernel, 19, sizeof(pf2), C_LOC(pf2))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:eta2', ierr)

			ierr 	=  clSetKernelArg(kernel, 20, sizeof(pf3), C_LOC(pf3))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:eta3', ierr)

			ierr 	=  clSetKernelArg(kernel, 21, sizeof(pf4), C_LOC(pf4))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:eta4', ierr)

			ierr 	=  clSetKernelArg(kernel, 25, sizeof(dx), C_LOC(dx))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:dx', ierr)

			ierr 	=  clSetKernelArg(kernel, 26, sizeof(dy), C_LOC(dy))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:dy', ierr)

			ierr 	=  clSetKernelArg(kernel, 27, sizeof(dz), C_LOC(dz))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:dz', ierr)

!=====================================
! EXECUTE THE EXPONENTIATION KERNEL
!=====================================
			ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), &
                              0, C_NULL_PTR, C_LOC(event))
			call CLerror_check('gamma_STEMDCI:clEnqueueNDRangeKernel', ierr)

			ierr = clFinish(command_queue)
			call CLerror_check('gamma_STEMDCI:clFinish', ierr)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_QUEUED, sizeof(start_time), C_LOC(start_time), return_bytes)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end_time), C_LOC(end_time), return_bytes)

			exec_time  = end_time  -  start_time

			texec_time = texec_time + exec_time

			ierr       = clReleaseEvent(event)
			call CLerror_check('gamma_STEMDCI:clReleaseEvent', ierr)

!===================================================
! SET ARGUMENTS FOR BEAM PROPAGATION STEP
!===================================================
			ierr =  clSetKernelArg(kernel2, 0, sizeof(cl_SMf), C_LOC(cl_SMf))
			call CLerror_check('gamma_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

			ierr =  clSetKernelArg(kernel2, 1, sizeof(cl_wavecoeffin), C_LOC(cl_wavecoeffin))
			call CLerror_check('gamma_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

			ierr =  clSetKernelArg(kernel2, 2, sizeof(cl_wavecoeffout), C_LOC(cl_wavecoeffout))
			call CLerror_check('gamma_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

			ierr =  clSetKernelArg(kernel2, 3, sizeof(cl_wavecoefftmp), C_LOC(cl_wavecoefftmp))
			call CLerror_check('gamma_STEMDCI:clsetKernelArg:wavecoeffout', ierr)

			ierr =  clSetKernelArg(kernel2, 4, sizeof(nsam_correct), C_LOC(nsam_correct))
			call CLerror_check('gamma_STEMDCI:clSetKernelArg:nsam', ierr)

!===================================
! EXECUTE THE PROPAGATION KERNEL
!===================================

			ierr = clEnqueueNDRangeKernel(command_queue, kernel2, 2, C_NULL_PTR, C_LOC(globalsize2), C_NULL_PTR, &
                              0, C_NULL_PTR, C_LOC(event))
			call CLerror_check('PropagateBeam:clEnqueueNDRangeKernel', ierr)

			ierr = clFinish(command_queue)
			call CLerror_check('PropagateBeam:clFinish', ierr)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_QUEUED, sizeof(start_time), C_LOC(start_time), return_bytes)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end_time), C_LOC(end_time), return_bytes)

			exec_time  = end_time  -  start_time

			texec_time = texec_time + exec_time

			ierr       = clReleaseEvent(event)
			call CLerror_check('gamma_STEMDCI:clReleaseEvent', ierr)

!=======================================================
! COPY THE RESULTS AND MAKE IT INPUT FOR NEXT ITERATION
!=======================================================
! read the resulting vector from device memory
			ierr    =  clEnqueueReadBuffer(command_queue,cl_wavecoeffout,CL_TRUE,0_8,size_in_bytes_sgmaster,&
					   C_LOC(resGPU(1)),0,C_NULL_PTR,C_LOC(event))
			call CLerror_check('gamma_STEMDCI:clEnqueueReadBuffer:cl_wavecoeffout', ierr)

			ierr = clFinish(command_queue)
			call CLerror_check('gamma_STEMDCI:clFinish', ierr)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_QUEUED, sizeof(start_time), C_LOC(start_time), return_bytes)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end_time), C_LOC(end_time), return_bytes)

			exec_time  = end_time  -  start_time

			texec_time = texec_time + exec_time

			ierr       = clReleaseEvent(event)
			call CLerror_check('gamma_STEMDCI:clReleaseEvent', ierr)

! write the resulting wave function coefficients to the input matrix for next iteration
			ierr = clEnqueueWriteBuffer(command_queue, cl_wavecoeffin, CL_TRUE, 0_8, size_in_bytes_sgmaster, &
						  C_LOC(resGPU(1)), 0, C_NULL_PTR, C_LOC(event))
			call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_wavecoeffin', ierr)

			ierr = clFinish(command_queue)
			call CLerror_check('gamma_STEMDCI:clFinish', ierr)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_QUEUED, sizeof(start_time), C_LOC(start_time), return_bytes)

			ierr = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end_time), C_LOC(end_time), return_bytes)

			exec_time  = end_time  -  start_time

			texec_time = texec_time + exec_time

			ierr       = clReleaseEvent(event)
			call CLerror_check('gamma_STEMDCI:clReleaseEvent', ierr)

			!ierr = clEnqueueReadBuffer(command_queue, cl_SM, CL_TRUE, 0_8, size_in_bytes_S_off, &
			!			  C_LOC(resGPU2(1)), 0, C_NULL_PTR, C_LOC(event))
			!call CLerror_check('gamma_STEMDCI:clEnqueueWriteBuffer:cl_SM', ierr)

			!ierr = clFinish(command_queue)
			!call CLerror_check('gamma_STEMDCI:clFinish', ierr)

			!if(ii .eq. 1) print*,resGPU2(1:5)
			!if(ll.eq. 1 .and. mm .eq. 1) then
			!	write(13,*) realpart(resGPU(158*correctsize+1)),imagpart(resGPU(158*correctsize+1))
			!end if
			!write(14,*) resGPU(158*correctsize+1)
		end do

! write the results to the results array
		do ii = 1,nsam
			results(ll,mm,ii,1:nref_gp) = abs(resGPU((ii-1)*correctsize+1:(ii-1)*correctsize+nref_gp))
		end do

		! print some results at fixed intervals
		if(float((ll - 1)*npy*nz + (mm - 1)*nz + nz)*1000.0/float(npx*npy*nz) .ge. float(pcnt)) then

			io_real(1)	=	float(pcnt)/10.0
			call WriteValue('completed ',io_real,1,'(F8.2, " %" )')

			io_real(1) = dble(texec_time) * 1.D-9 / 60.0
			call WriteValue('OpenCL execution time (min) = ',io_real,1,'(F15.2)')
			pcnt 	=	pcnt + 1

		end if

	end do

end do

call Message("Finished executing kernel. Writing data to:"//trim(datafile))

call timestamp(datestring=dstr, timestring=tstre)

dataset   =  'STEMDCI'

! update the time string
groupname =  SC_EMheader
hdferr    =  HDF_openGroup(groupname, HDF_head)
hdferr    =  HDF_openGroup(dataset, HDF_head)

dataset   =  SC_StopTime
line2(1)  =  dstr//', '//tstre
hdferr    =  HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! create a group to write all the data and parameters into
groupname = SC_EMData
hdferr = HDF_createGroup(groupname, HDF_head)

! write final intensities
groupname = SC_STEMDCI
hdferr = HDF_createGroup(groupname, HDF_head)

!dataset = SC_BraggAngle
!hdferr = HDF_writeDatasetFloat(dataset, CalcDiffAngle(cell,ga(1),ga(2),ga(3))*0.5, HDF_head)

dataset = SC_wavelength
hdferr  = HDF_writeDatasetFloat(dataset, sngl(cell_g%mLambda), HDF_head)

dataset = SC_pixelsize
hdferr  = HDF_writeDatasetFloat(dataset, 1.0, HDF_head)

dataset = SC_numreflections
hdferr  = HDF_writeDatasetInteger(dataset, nref_gp, HDF_head)

dataset = SC_numk
hdferr  = HDF_writeDatasetInteger(dataset, nsam, HDF_head)

dataset = SC_hkl
hdferr  = HDF_writeDatasetIntegerArray2D(dataset, hklarray, 3, nref_gp, HDF_head)

dataset = SC_klist
hdferr  = HDF_writeDatasetFloatArray2D(dataset, klist, 3, nsam, HDF_head)

dataset = SC_Intensities
hdferr = HDF_writeDatasetFloatArray4D(dataset, results, npx, npy, nsam, nref_gp, HDF_head)

call HDF_pop(HDF_head)

! and close the fortran hdf interface
call h5close_f(hdferr)

!=========================
! RELEASE EVERYTHING
!=========================
call Message('--> Releasing device memory.')

ierr = clReleaseKernel(kernel)
call CLerror_check('gamma_STEMDCI:clReleaseKernel', ierr)

ierr = clReleaseCommandQueue(command_queue)
call CLerror_check('gamma_STEMDCI:clReleaseCommandQueue', ierr)

ierr = clReleaseContext(context)
call CLerror_check('gamma_STEMDCI:clReleaseContext', ierr)

ierr = clReleaseMemObject(cl_A_dia)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_A_dia', ierr)

ierr = clReleaseMemObject(cl_A_off)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_A_off', ierr)

ierr = clReleaseMemObject(cl_A2)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_A2', ierr)

ierr = clReleaseMemObject(cl_A3)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_A3', ierr)

ierr = clReleaseMemObject(cl_TT1)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_TT1', ierr)

ierr = clReleaseMemObject(cl_TT2)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_TT2', ierr)

ierr = clReleaseMemObject(cl_TT3)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_TT3', ierr)

ierr = clReleaseMemObject(cl_tmp)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_tmp', ierr)

ierr = clReleaseMemObject(cl_SM)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_SM', ierr)

ierr = clReleaseMemObject(cl_SMf)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_SMf', ierr)

ierr = clReleaseMemObject(cl_wavecoeffin)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_wavecoeffin', ierr)

ierr = clReleaseMemObject(cl_wavecoeffout)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_wavecoeffout', ierr)

ierr = clReleaseMemObject(cl_wavecoefftmp)
call CLerror_check('gamma_STEMDCI:clReleaseMemObject:cl_wavecoefftmp', ierr)

!===============================================================================
!FINISHED OPENCL PORTION OF THE CODE 
!===============================================================================

end subroutine computeGAMMASTEM

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

type(reflisttype),pointer				:: reflist_master
type(reflisttype),pointer				:: reflist_new
integer(kind=irg),INTENT(INOUT)			:: nref_master
integer(kind=irg),INTENT(IN)			:: nref_new

type(reflisttype),pointer				:: tmp1, tmp2
integer(kind=irg)						:: ii, jj, hkl1(3), hkl2(3)
logical									:: isnew

if(nref_master .eq. 0) then
	reflist_master  => 	reflist_new
	nref_master		=	nref_new
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
			tmp2 			=> tmp2%next
			nullify(tmp2%next)
			tmp2%hkl 		= tmp1%hkl           ! store Miller indices
			nref_master  	=  nref_master + 1
		end if

		if(.not. associated(tmp1%next)) exit
		tmp1  =>  tmp1%next
	end do

end if


end subroutine FindUnion


!--------------------------------------------------------------------------
!
! SUBROUTINE:isFundamental
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief check if reflection is fundamental fcc reflection or not
!> done by checking parity of indices
!
!> @param hkl reflection index
!
!> @date 07/05/17  SS  1.0 original
!-------------------------------------------------------------------------
recursive subroutine isFundamental(hkl,res)

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)				:: hkl(3)
logical,INTENT(OUT)							:: res

integer(kind=irg)							:: sum2

sum2 = 0
res = .FALSE.

if(mod(abs(hkl(1)),2) .eq. 0 ) then
	sum2 = sum2 + 1
else
	sum2 = sum2 - 1
end if

if(mod(abs(hkl(2)),2) .eq. 0 ) then
	sum2 = sum2 + 1
else
	sum2 = sum2 - 1
end if

if(mod(abs(hkl(3)),2) .eq. 0 ) then
	sum2 = sum2 + 1
else
	sum2 = sum2 - 1
end if

if(abs(sum2) .eq. 3) res = .TRUE.

end subroutine isFundamental

!--------------------------------------------------------------------------
!
! SUBROUTINE:chooseVariant
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief assign a superlattice reflection to one of the four variants
!> (0 0 0) + fcc ===> variant 0
!> (1 0 0) + fcc ===> variant 1
!> (0 1 0) + fcc ===> variant 2
!> (1 1 0) + fcc ===> variant 3
!
!> @param reflist reflectionlist type
!
!> @date 07/05/17  SS  1.0 original
!-------------------------------------------------------------------------
recursive subroutine chooseVariant(reflist)

use typedefs

IMPLICIT NONE

type(reflisttype),pointer						:: reflist

integer(kind=irg)								:: hkl(3), hkl2(3), trans(3,3), ii
logical											:: isfun

hkl(1:3) = reflist%hkl(1:3)

trans(1,1:3) = (/1,0,0/)
trans(2,1:3) = (/0,1,0/)
trans(3,1:3) = (/1,1,0/)

do ii = 1,3
	hkl2(1:3) = hkl(1:3) + trans(ii,1:3)
	call isFundamental(hkl2,isfun)
	if(isfun) then
		reflist%variant = ii
		exit
	end if
	hkl2(1:3) = hkl(1:3) - trans(ii,1:3)
	call isFundamental(hkl2,isfun)
	if(isfun) then
		reflist%variant = ii
		exit
	end if
end do

end subroutine chooseVariant

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

type(reflisttype),pointer						:: reflist
integer(kind=irg),INTENT(IN)					:: nref

type(reflisttype),pointer						:: tmpreflist, tmpreflist2
integer(kind=irg)								:: ii

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
! SUBROUTINE:CalcVarPhase
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate the phase factors for the variants
!
!> @param reflist reflectionlist type
!
!> @date 07/11/17  SS  1.0 original
!-------------------------------------------------------------------------
recursive subroutine CalcVarPhase(reflist, Varout, nref)

use typedefs
use constants

IMPLICIT NONE

type(reflisttype),pointer							:: reflist
complex(kind=sgl),INTENT(OUT)						:: Varout(nref, nref, 4)
integer(kind=irg),INTENT(IN)						:: nref

type(reflisttype),pointer							:: tmpreflist, tmpreflist2
real(kind=dbl)										:: cdot
integer(kind=irg)									:: ii, jj

tmpreflist  => reflist%next
do ii = 1,nref
	tmpreflist2  => reflist%next
	do jj = 1,nref
		if(ii .ne. jj) then
			cdot = dot_product(float(tmpreflist%hkl - tmpreflist2%hkl),(/1.0,1.0,0.0/))
			Varout(ii,jj,1) = exp(-cmplx(0.D0,1.D0)*cPi*cdot)

			cdot = dot_product(float(tmpreflist%hkl - tmpreflist2%hkl),(/1.0,0.0,1.0/))
			Varout(ii,jj,2) = exp(-cmplx(0.D0,1.D0)*cPi*cdot)

			cdot = dot_product(float(tmpreflist%hkl - tmpreflist2%hkl),(/0.0,1.0,1.0/))
			Varout(ii,jj,3) = exp(-cmplx(0.D0,1.D0)*cPi*cdot)
		end if
		tmpreflist2 => tmpreflist2%next
	end do
	tmpreflist => tmpreflist%next
end do

end subroutine CalcVarPhase
