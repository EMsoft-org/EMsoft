!###################################################################
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

!--------------------------------------------------------------------------
! EMsoft:EMgammaOpenCL.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgammaOpenCL
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief image simulation for mix of gamma and gamma' phases for arbitraty 
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
!> @date 11/02/13 MDG 1.0 original (took a few days to get it right...)
!> @date 06/28/17 MDG 2.0 complete rewrite to current EMsoft libraries.
!> @date 07/05/17 SS  3.0 rewrite for the GPU
!--------------------------------------------------------------------------
program EMgammaOpenCL

use local
use files
use io
use NameListTypedefs
use NameListHandlers
use JSONsupport
use stringconstants

IMPLICIT NONE

character(fnlen)                   :: nmldeffile, progname, progdesc
type(EMgammaOpenCLNameListType)    :: enl
integer(kind=irg)                  :: res

nmldeffile = 'EMgammaOpenCL.nml'
progname = 'EMgammaOpenCL.f90'
progdesc = 'Gamma-gamma'' microstructure dynamical image simulation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,2,(/ 0, 31 /), progname)

! deal with the namelist stuff, either .nml or .json format
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
! call JSONreadEMgammaNameList(enl, nmldeffile, error_cnt)
  call Message('JSON input not yet implemented')
  STOP
else
  call GetEMgammaOpenCLNameList(nmldeffile,enl)
end if

! perform the (near) zone axis computations
call ComputeGAMMAimage(enl, progname, nmldeffile)

end program EMgammaOpenCL

!--------------------------------------------------------------------------
!
! SUBROUTINE:ComputeGAMMAimage
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief compute the bright field and dark field images for a bent foil
!> with a gamma-gamma' microstructure, using full dynamical theory
!
!> @param enl name list
!> @param progname program name
!> @param nmldeffile namelist file name
!
!> @date 11/02/13  MDG 1.0 original
!> @date 11/13/13  MDG 1.1 moved MatrixExponential into math.f90 library file
!> @date 11/14/13  MDG 1.2 addition of Bethe potentials for scattering matrix formalism
!> @date 06/28/17  MDG 2.0 complete rewrite with modern EMsoft libraries
!> @date 07/05/17  SS  3.0 OpenCL 
!-------------------------------------------------------------------------
subroutine ComputeGAMMAimage(enl, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use NameListHDFwriters
use InitializersHDF
use symmetry
use crystal
use constants
use io
use files
use diffraction
use noise
use HDF5
use HDFsupport
use ISO_C_BINDING
use omp_lib
use timing
use CLsupport
use clfortran
use rotations
use quaternions
use MBmodule
use gvectors
use initializers
use error
use math
use stringconstants

IMPLICIT NONE

type(EMgammaOpenCLNameListType),INTENT(IN)		:: enl
character(fnlen),INTENT(IN)						:: progname
character(fnlen),INTENT(IN)						:: nmldeffile

type(unitcell)        							:: cell_g, cell_gp
type(DynType)									:: Dyn_g, Dyn_gp
type(gnode)										:: rlp_g, rlp_gp
logical											:: verbose

character(fnlen)								:: xtalname_g, xtalname_gp
real(kind=sgl)									:: k(3), kk(3), kp(3), ku(3), eu(3), qu(4), FN(3)
real(kind=sgl)									:: DtoR, RtoD, io_real(3), alpha, cdot

type(reflisttype),pointer						:: reflist_g, reflist_gp, reflist_gp_rearr, tmpreflist, tmpreflist2
type(BetheParameterType)						:: BetheParameters
integer(kind=irg),target						:: nref_g, nref_gp, npx, npy, nz
integer(kind=8),target 							:: globalsize(2), localsize(2)
integer(kind=irg)								:: io_int(3), nvar(0:3), ncts(0:3)
complex(kind=dbl),allocatable					:: ScatMat_v0(:,:), ScatMat_v1(:,:), ScatMat_v2(:,:), ScatMat_v3(:,:)
complex(kind=dbl),allocatable					:: A_g(:,:), A_gp(:,:,:), A_tmp(:,:), ampl(:)
complex(kind=dbl),allocatable					:: Var(:,:,:), Var_tmp(:,:,:)
type(reflisttype),pointer						:: reflist_v0, reflist_v1, reflist_v2, reflist_v3
type(reflisttype),pointer						:: tmpreflist_v0, tmpreflist_v1, tmpreflist_v2, tmpreflist_v3
type(reflisttype),pointer						:: firstw
integer(kind=irg)								:: nns_v0, nns_v1, nns_v2, nns_v3
integer(kind=irg)								:: nnw_v0, nnw_v1, nnw_v2, nnw_v3
logical											:: isfun
integer(kind=irg)								:: ii, jj, ll, mm, nix, nixp, ivar
real(kind=sgl),allocatable						:: image(:,:,:)
integer(kind=irg),allocatable					:: list(:)
real(kind=dbl)									:: thk, dx, dxm
type(reflisttype),pointer						:: reflist_rearr, firstw_rearr, firstw_gp
complex(kind=dbl),allocatable					:: ScatMat_rearr(:,:), ScatMat_g(:,:), ScatMat_gp(:,:), ScatMat_tmp(:,:)
integer(kind=irg)								:: nns_rearr, nnw_rearr
complex(kind=dbl),allocatable					:: master_g(:,:,:), master_gp(:,:,:,:)
complex(kind=sgl),allocatable,TARGET			:: tmparray(:,:), tmparraylin(:), wavefncoeff(:)
type(HDFobjectStackType)        				:: HDF_head
integer(kind=irg)								:: hdferr, hkl(3)
integer(HSIZE_T)								:: dims3(3), dims4(4)
character(fnlen)								:: microfile, datafile, groupname, dataset
logical											:: f_exists, readonly
real(kind=sgl),allocatable						:: microstructure(:,:,:)
real(kind=sgl),allocatable						:: eta1(:,:,:),eta2(:,:,:),eta3(:,:,:),eta4(:,:,:),orderparam(:,:,:,:),dispfield(:,:,:,:)
real(kind=sgl),allocatable,target				:: tmpphase(:), gx(:), gy(:), gz(:)
integer(kind=irg)								:: ww, tdp, nsize, npix, sx, sy
real(kind=dbl)									:: rnmpp, maxint, Igmax, Ig, xs, ys, sig
real(kind=dbl),allocatable						:: xx(:,:), yy(:,:), line(:), dot(:,:), dpattern(:,:), dp(:,:,:,:)
integer, parameter 								:: source_length = 50000
character(len=source_length, KIND=c_char),TARGET:: csource
integer(c_size_t),TARGET						:: slength
integer(c_int)									:: numd, nump

integer(c_intptr_t),allocatable, target 		:: platform(:)
integer(c_intptr_t),allocatable, target 		:: device(:)
integer(c_intptr_t),target 						:: context
integer(c_intptr_t),target 						:: command_queue
integer(c_intptr_t),target 						:: prog
integer(c_intptr_t),target 						:: kernel
character(fnlen)								:: sourcefile, info
integer(c_intptr_t),target 						:: cl_ScatMat_g,cl_ScatMat_gp_v1,cl_ScatMat_gp_v2, cl_ScatMat_gp_v3, &
												   cl_Ain, cl_expA, cl_AA, cl_AAA, cl_T1, cl_T2, cl_wavefncoeff, &
												   cl_wavefncoeffintd, cl_pf_v1, cl_coeff, cl_ScatMat_gp_v4, cl_pf_v2, &
												   cl_pf_v3, cl_pf_v4
integer(c_intptr_t),target 						:: cl_dfx, cl_dfy, cl_dfz, cl_gminusgp_x, cl_gminusgp_y, cl_gminusgp_z
integer(c_size_t)								:: size_in_bytes_matrix, size_in_bytes_vector, size_in_bytes_phase, &
												   size_in_bytes_coeff, size_in_bytes_scale, size_in_bytes_scat_matrix,&
												   size_in_bytes_gminusgp
complex(kind=sgl),target						:: coef(9),cvals(9)

type(c_ptr), TARGET 							:: psource
character(19),TARGET 							:: progoptions
integer(c_size_t)								:: cnum
character(len=source_length),TARGET 			:: source
integer(c_int32_t)								:: ierr, ierr2, pcnt
character(12),TARGET 							:: kernelname
character(13, KIND=c_char),target 				:: ckernelname
integer(kind=irg)								:: irec


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
				complex(kind=dbl),INTENT(OUT)			:: Varout(nref,nref,4)
				integer(kind=irg),INTENT(IN)			:: nref
			end subroutine CalcVarPhase

end interface

nullify(HDF_head%next)

! degree to radians and vice versa

DtoR = cPi/180.0
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

! this is where the old and the new program starts to diverge; the old program
! only considers near zone axis orientations; however, in this program, we will 
! relax that constratint to include any arbitrary orientation. The workflow will 
! be as follows:
! 1. Initialize cell (done just before this comment)
! 2. calculate the master list of reflections for both phases depending on voltage etc.
! 3. go through the list and mark fundamental and super-lattice reflections
! 4. calculate the dynamical matrix with these reflections as separate
! 5. pass the dynamical matrix and 3D microstructure on to the GPU
! 6. calculate exponentaial of matrix and propagate for different image pixels
!   on different GPU threads
! 7. save results in HDF5 file

!====================================================================================
! 2. CALCULATE MASTER REFLECTION LIST
!====================================================================================

! convert euler angle to quaternion
eu(1:3) = enl%eu(1:3)*DtoR
qu = eu2qu(eu)

! scale k by the inverse wavelength and convert to reciprocal frame
k = quat_LP(conjg(qu),(/0.0,0.0,1.0/))
ku = k
call NormVec(cell_g,k,'c')
k = k/cell_g%mlambda
call TransSpace(cell_g,k,kk,'c','r')
FN = kk

! force dynamical matrix routine to read new Bethe parameters from file
!call Set_Bethe_Parameters(BetheParameters)

nullify(reflist_g,reflist_gp)

call Initialize_ReflectionList(cell_g, reflist_g, BetheParameters, FN, kk, enl%dmin, nref_g)
call Message('--> Finished initializing reflection list for gamma phase')
io_int(1) = nref_g
call WriteValue('--> Number of reflections in gamma phase = ',io_int,1,'(I3)')

call Initialize_ReflectionList(cell_gp, reflist_gp, BetheParameters, FN, kk, enl%dmin, nref_gp)
call Message('--> Finished initializing reflection list for gamma prime phase')
io_int(1) = nref_gp
call WriteValue('--> Number of reflections in gamma prime phase = ',io_int,1,'(I3)')

! we will calculate the dynamical matrix for the gammpa prime phase here
! bethe approximation will not be used since we want to take all superlattice
! reflections into account

! mark all reflections as strong
tmpreflist  => reflist_gp%next
tmpreflist%strong =.TRUE.
tmpreflist%weak   = .FALSE.
tmpreflist2 => tmpreflist
nullify(tmpreflist2%nextw)

do ii = 2,nref_gp
	tmpreflist => tmpreflist%next
	tmpreflist2%nexts => tmpreflist
	tmpreflist%strong =.TRUE.
	tmpreflist%weak   = .FALSE.
	tmpreflist2 => tmpreflist
end do

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
		call FatalError('EMgammaOpenCL:','reflection in none of the 4 variants.')
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
	tmpreflist => tmpreflist%next
end do

tmpreflist%next => reflist_v1%next

do
	if(.not. associated(tmpreflist%next)) exit
	tmpreflist => tmpreflist%next
end do

tmpreflist%next => reflist_v2%next

do
	if(.not. associated(tmpreflist%next)) exit
	tmpreflist => tmpreflist%next
end do

tmpreflist%next => reflist_v3%next

! mark all beams as strong in the gamma prime phase too
call MarkStrong(reflist_gp_rearr, nref_gp)

allocate(ScatMat_gp(nref_gp,nref_gp))
ScatMat_gp = cmplx(0.D0,0.D0)
call GetDynMat(cell_gp, reflist_gp_rearr, firstw_gp, rlp_gp, ScatMat_gp, nref_gp, 0, BlochMode = 'Struc')

call Message('--> Done generating dynamical matrices for gamma prime phase')

! now all reflections have been sorted in their respective 
! linked lists. we will skip using the bethe approximation 
! since we want to explicitly take each super lattice reflection
! in account

! mark all reflections in the four partitioned linked list 
! as strong

call MarkStrong(reflist_v0, ncts(0))

call MarkStrong(reflist_v1, ncts(1))

call MarkStrong(reflist_v2, ncts(2))

call MarkStrong(reflist_v3, ncts(3))

call Message('--> All beams set as strong reflection')

! for the gamma prime phase there are four distinct variants depending on 
! the position where the Aluminum atom is placed. these correspond to no 
! translation [0 0 0], 1/2[1 1 0], 1/2[1 0 1] and 1/2[0 1 1]. these variants
! manifest themselves as phase factors in the lattice potential. in this code
! block, the phase factors for the three non-zero translations are computed
! these are given by -2.pi.(g - g').R, where R are the translation vectors
! noted above

allocate(list(25))
list = 0
list = (/1,9,10,11,15,16,20,21,22,33,34,35,36,37,38,43,44,47,48,51,52,57,&
		58, 61,62/)

! main array
allocate(Var(nref_gp,nref_gp,4))
Var = cmplx(1.D0,0.D0)

call CalcVarPhase(reflist_gp_rearr, Var, nref_gp)

call Message('--> Done calculating phase factors for the four translational variants')

nullify(firstw)
! calculate dynamical matrix for each of these reflection lists
allocate(ScatMat_v0(ncts(0),ncts(0)))
ScatMat_v0 = cmplx(0.D0,0.D0)
call GetDynMat(cell_g, reflist_v0, firstw, rlp_g, ScatMat_v0, ncts(0), 0, BlochMode = 'Struc')

allocate(ScatMat_v1(ncts(1),ncts(1)))
ScatMat_v1 = cmplx(0.D0,0.D0)
call GetDynMat(cell_g, reflist_v1, firstw, rlp_g, ScatMat_v1, ncts(1), 0, BlochMode = 'Struc')

allocate(ScatMat_v2(ncts(2),ncts(2)))
ScatMat_v2 = cmplx(0.D0,0.D0)
call GetDynMat(cell_g, reflist_v2, firstw, rlp_g, ScatMat_v2, ncts(2), 0, BlochMode = 'Struc')

allocate(ScatMat_v3(ncts(3),ncts(3)))
ScatMat_v3 = cmplx(0.D0,0.D0)
call GetDynMat(cell_g, reflist_v3, firstw, rlp_g, ScatMat_v3, ncts(3), 0, BlochMode = 'Struc')

call Message('--> Done generating dynamical matrices for all four variants of gamma phase')

! save these in the scatmat_g array
allocate(ScatMat_g(nref_gp,nref_gp))
ScatMat_g = cmplx(0.D0,0.D0)
ScatMat_g(1:ncts(0),1:ncts(0)) = ScatMat_v0(1:ncts(0),1:ncts(0))
ScatMat_g(ncts(0)+1:sum(ncts(0:1)),ncts(0)+1:sum(ncts(0:1))) = ScatMat_v1(1:ncts(1),1:ncts(1))
ScatMat_g(sum(ncts(0:1))+1:sum(ncts(0:2)),sum(ncts(0:1))+1:sum(ncts(0:2))) = ScatMat_v2(1:ncts(2),1:ncts(2))
ScatMat_g(sum(ncts(0:2))+1:sum(ncts(0:3)),sum(ncts(0:2))+1:sum(ncts(0:3))) = ScatMat_v3(1:ncts(3),1:ncts(3))

! ScatMat_g and ScatMat_gp now have the respective structure matrices for the two 
! phases. What we have to do now is to calculate a "master" scattering matrix list
! for these two respective phases. note that the master list for the gamma' phase
! will have all the 4 translational variants in them. the step size for the thickness
! will be set to 0.05 nm

allocate(master_g(nref_gp,nref_gp,21),master_gp(nref_gp,nref_gp,21,4))
master_g  = cmplx(0.D0,0.D0)
master_gp = cmplx(0.D0,0.D0)

ScatMat_g = ScatMat_g   * cmplx(0.D0, 1.D0)
ScatMat_gp = ScatMat_gp * cmplx(0.D0, 1.D0)

call Message('--> Calculating master list of scattering matrices...')
do ii = 1,21
	! slice thickness
	thk = dble((ii-1))*0.05D0
	! for the gamma phase
	call MatrixExponential(ScatMat_g, master_g(:,:,ii),thk,'Pade',nref_gp)

	! for the gamma prime phase
	call MatrixExponential(ScatMat_gp*Var(:,:,1), master_gp(:,:,ii,1), thk, 'Pade', nref_gp)
	call MatrixExponential(ScatMat_gp*Var(:,:,2), master_gp(:,:,ii,2), thk, 'Pade', nref_gp)
	call MatrixExponential(ScatMat_gp*Var(:,:,3), master_gp(:,:,ii,3), thk, 'Pade', nref_gp)
	call MatrixExponential(ScatMat_gp*Var(:,:,4), master_gp(:,:,ii,4), thk, 'Pade', nref_gp)

end do

call Message('--> Completed calculating scattering matrices...')

! all the dynamical diffraction calculation is done here
! anything that follows will be mere book-keeping now
! if only the microstructure simulation needs to be performed,
! then these dynamical matrices can be used to do a linear interpolation
! on the other hand if defect simulation needs to be preformed, then 
! the displacement vectors would need to be converted to phase factors

! Initialize FORTRAN interface.
call h5open_EMsoft(hdferr)

microfile = trim(EMsoft_getEMdatapathname())//trim(enl%microfile)
microfile = EMsoft_toNativePath(microfile)
inquire(file=trim(microfile), exist=f_exists)

if(f_exists) then
	readonly = .TRUE.
	call Message('--> opening '//trim(enl%microfile), frm = "(A)" )
	hdferr =  HDF_openFile(microfile, HDF_head, readonly)
else
	call FatalError('EMgammaOpenCL:','microstructure file not found')
end if

! open the MicrostructureData group
groupname = SC_MicrostructureData
hdferr = HDF_openGroup(groupname, HDF_head)

! read the variables from the group
!dataset = SC_Microstructure
!call HDF_readDatasetFloatArray3D(dataset, dims3, HDF_head, hdferr, microstructure)

! print data about microstructure file

!==================================================================================

! as of 10/23/2017, we will follow the new data structure which has
! one file which contains the order parameter along with the displacement
! fields etc.
!==================================================================================

dataset = 'OP'!SC_Variants
call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, orderparam)

dataset = 'DF' ! SC_DF displacement field
call HDF_readDatasetFloatArray4D(dataset, dims4, HDF_head, hdferr, dispfield)

npx = dims4(2)
npy = dims4(3)
nz  = dims4(4)

io_int(1:3) = dims4(2:4)
call WriteValue('--> size of phase field microstructure (in voxels) = ',io_int,3,'(I3," X ",I3," X ",I3)')

allocate(eta1(npx,npy,nz),eta2(npx,npy,nz),eta3(npx,npy,nz),eta4(npx,npy,nz))

eta1 = orderparam(1,1:npx,1:npy,1:nz)
eta2 = orderparam(2,1:npx,1:npy,1:nz)
eta3 = orderparam(3,1:npx,1:npy,1:nz)
eta4 = orderparam(4,1:npx,1:npy,1:nz)

deallocate(orderparam)

call HDF_pop(HDF_head,.TRUE.)
! we have two options from here: either perform the simulation with the phase
! field data where the values are 0 for gamma 1 for gamma' and fractional for 
! something in between. In this case we have to use the data as weight factor
! for the dynamical matrix, or we could use the variant (phase data) which is
! 0 for gamma and 1/2/3 for gamma'. The second case is computationally simpler,
! while the first case should be more accurate. We use variant data for testing


!=============================================
!=============================================
! we will also be writing out spot diffraction patterns at four special
! column positions as a function of thickness. this will help us visualize
! how the intensities gets transmitted between different beams.

! create the coordinate arrays for the Gaussian peaks that will represent the diffraction spots
rnmpp = 0.120/3
rnmpp = 1.0/rnmpp
ww = 6*3
tdp = 2*ww+1
sig = 0.00009D0
allocate(xx(-ww:ww,-ww:ww), yy(-ww:ww,-ww:ww), line(-ww:ww), dot(-ww:ww,-ww:ww))
line = (/ (float(ii),ii=-ww,ww) /) * rnmpp
xx = spread(line,dim=1,ncopies=2*ww+1)
yy = transpose(xx)


!=============================================
!=============================================
! create the output array
npix = 144*3
nsize = npix/2 + ww 
allocate(dpattern(-nsize:nsize,-nsize:nsize), dp(npix, npix, nz, 5))

! this is the final image
! we will be calculating the bright field image to begin with
allocate(image(npx,npy,25))
image = 0.0

! inital intensity in the incident beam
allocate(ampl(nref_gp))

ampl = cmplx(0.D0,0.D0)
ampl(1) = cmplx(1.D0,0.D0)

if(1 .eq. 0) then

do ii = 1, npx
	do jj = 1, npy
		ampl = cmplx(0.D0, 0.D0)
		ampl(1) = cmplx(1.D0,0.D0)
		do ll = 1,nz

			alpha = 1.D0 - (eta1(ii,jj,ll) + eta2(ii,jj,ll) + eta3(ii,jj,ll) + eta4(ii,jj,ll))
			! propagation through gamma phase
			nix = floor(alpha/0.05D0) + 1
			nixp = nix + 1
			if(nix  .lt. 1)  nix  = 1
			if(nix  .gt. 21) nix  = 21
			if(nixp .lt. 1)  nixp = 1
			if(nixp .gt. 21) nixp = 21
			dx = alpha/0.05D0 - nix + 1
			dxm = 1.D0 - dx
			A_tmp = master_g(:,:,nix)*dxm + master_g(:,:,nixp)*dx
			ampl = matmul(A_tmp,ampl)

			! propagation through gamma' phase (variant 1)
			nix = floor(eta1(ii,jj,ll)/0.05D0) + 1
			nixp = nix + 1
			if(nix  .lt. 1)  nix  = 1
			if(nix  .gt. 21) nix  = 21
			if(nixp .lt. 1)  nixp = 1
			if(nixp .gt. 21) nixp = 21
			dx = eta1(ii,jj,ll)/0.05D0 - nix + 1
			dxm = 1.D0 - dx
			A_tmp = master_gp(:,:,nix,1)*dxm + master_gp(:,:,nixp,1)*dx
			ampl = matmul(A_tmp,ampl)

			! propagation through gamma' phase (variant 2)
			nix = floor(eta2(ii,jj,ll)/0.05D0) + 1
			nixp = nix + 1
			if(nix  .lt. 1)  nix  = 1
			if(nix  .gt. 21) nix  = 21
			if(nixp .lt. 1)  nixp = 1
			if(nixp .gt. 21) nixp = 21
			dx = eta2(ii,jj,ll)/0.05D0 - nix + 1
			dxm = 1.D0 - dx
			A_tmp = master_gp(:,:,nix,2)*dxm + master_gp(:,:,nixp,2)*dx
			ampl = matmul(A_tmp,ampl)

			! propagation through gamma' phase (variant 3)
			nix = floor(eta3(ii,jj,ll)/0.05D0) + 1
			nixp = nix + 1
			if(nix  .lt. 1)  nix  = 1
			if(nix  .gt. 21) nix  = 21
			if(nixp .lt. 1)  nixp = 1
			if(nixp .gt. 21) nixp = 21
			dx = eta3(ii,jj,ll)/0.05D0 - nix + 1
			dxm = 1.D0 - dx
			A_tmp = master_gp(:,:,nix,3)*dxm + master_gp(:,:,nixp,3)*dx
			ampl = matmul(A_tmp,ampl)

			! propagation through gamma' phase (variant 4)
			nix = floor(eta4(ii,jj,ll)/0.05D0) + 1
			nixp = nix + 1
			if(nix  .lt. 1)  nix  = 1
			if(nix  .gt. 21) nix  = 21
			if(nixp .lt. 1)  nixp = 1
			if(nixp .gt. 21) nixp = 21
			dx = eta4(ii,jj,ll)/0.05D0 - nix + 1
			dxm = 1.D0 - dx
			A_tmp = master_gp(:,:,nix,4)*dxm + master_gp(:,:,nixp,4)*dx
			ampl = matmul(A_tmp,ampl)
				if(ii .eq. 50 .and. jj .eq. 50) then

					tmpreflist => reflist_gp_rearr%next
					dpattern = 0.D0

					do mm = 1,nref_gp
						kp = k + float(tmpreflist%hkl) + tmpreflist%sg*ku
						kp = quat_LP(qu,kp)
						Ig = abs(ampl(mm))**2

						! determine the spot coordinates on the detector
						xs = rnmpp * kp(1)
						ys = rnmpp * kp(2)

						! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
						if ((abs(xs).le.nsize-ww).and.(abs(ys).le.nsize-ww)) then
							sx = nint(xs)
							sy = nint(ys)
							dx = xs - sx
							dxm = ys - sy ! dy
							dot = (Ig)**0.2 * exp(-((xx - dx)**2+(yy - dxm)**2)*sig)
							dpattern(sx-ww:sx+ww,sy-ww:sy+ww) = dpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
							tmpreflist => tmpreflist%next
						end if
					end do
					dp(1:npix,1:npix,ll,1) = dpattern(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)
				end if
				if(ii .eq. 50 .and. jj .eq. 80) then

					tmpreflist => reflist_gp_rearr%next
					dpattern = 0.D0

					do mm = 1,nref_gp
						kp = k + float(tmpreflist%hkl) + tmpreflist%sg*ku
						kp = quat_LP(qu,kp)
						Ig = abs(ampl(mm))**2

!						! determine the spot coordinates on the detector
						xs = rnmpp * kp(1)
						ys = rnmpp * kp(2)

!						! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
						if ((abs(xs).le.nsize-ww).and.(abs(ys).le.nsize-ww)) then
							sx = nint(xs)
							sy = nint(ys)
							dx = xs - sx
							dxm = ys - sy ! dy
							dot = (Ig)**0.2 * exp(-((xx - dx)**2+(yy - dxm)**2)*sig)
							dpattern(sx-ww:sx+ww,sy-ww:sy+ww) = dpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
							tmpreflist => tmpreflist%next
						end if
					end do
					dp(1:npix,1:npix,ll,2) = dpattern(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)
				end if
				if(ii .eq. 80 .and. jj .eq. 50) then

					tmpreflist => reflist_gp_rearr%next
					dpattern = 0.D0

					do mm = 1,nref_gp
						kp = k + float(tmpreflist%hkl) + tmpreflist%sg*ku
						kp = quat_LP(qu,kp)
						Ig = abs(ampl(mm))**2

						! determine the spot coordinates on the detector
						xs = rnmpp * kp(1)
						ys = rnmpp * kp(2)

						! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
						if ((abs(xs).le.nsize-ww).and.(abs(ys).le.nsize-ww)) then
							sx = nint(xs)
							sy = nint(ys)
							dx = xs - sx
							dxm = ys - sy ! dy
							dot = (Ig)**0.2 * exp(-((xx - dx)**2+(yy - dxm)**2)*sig)
							dpattern(sx-ww:sx+ww,sy-ww:sy+ww) = dpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
							tmpreflist => tmpreflist%next
						end if
					end do
					dp(1:npix,1:npix,ll,3) = dpattern(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)
				end if
				if(ii .eq. 80 .and. jj .eq. 80) then

					tmpreflist => reflist_gp_rearr%next
					dpattern = 0.D0

					do mm = 1,nref_gp
						kp = k + float(tmpreflist%hkl) + tmpreflist%sg*ku
						kp = quat_LP(qu,kp)
						Ig = abs(ampl(mm))**2

						! determine the spot coordinates on the detector
						xs = rnmpp * kp(1)
						ys = rnmpp * kp(2)

!						! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
						if ((abs(xs).le.nsize-ww).and.(abs(ys).le.nsize-ww)) then
							sx = nint(xs)
							sy = nint(ys)
							dx = xs - sx
							dxm = ys - sy ! dy
							dot = (Ig)**0.2 * exp(-((xx - dx)**2+(yy - dxm)**2)*sig)
							dpattern(sx-ww:sx+ww,sy-ww:sy+ww) = dpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
							tmpreflist => tmpreflist%next
						end if
					end do
					dp(1:npix,1:npix,ll,4) = dpattern(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)
				end if
				if(ii .eq. 65 .and. jj .eq. 65) then

					tmpreflist => reflist_gp_rearr%next
					dpattern = 0.D0

					do mm = 1,nref_gp
						kp = k + float(tmpreflist%hkl) + tmpreflist%sg*ku
						kp = quat_LP(qu,kp)
						Ig = abs(ampl(mm))**2

						! determine the spot coordinates on the detector
						xs = rnmpp * kp(1)
						ys = rnmpp * kp(2)

!						! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
						if ((abs(xs).le.nsize-ww).and.(abs(ys).le.nsize-ww)) then
							sx = nint(xs)
							sy = nint(ys)
							dx = xs - sx
							dxm = ys - sy ! dy
							dot = (Ig)**0.2 * exp(-((xx - dx)**2+(yy - dxm)**2)*sig)
							dpattern(sx-ww:sx+ww,sy-ww:sy+ww) = dpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
							tmpreflist => tmpreflist%next
						end if
					end do
					dp(1:npix,1:npix,ll,5) = dpattern(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)
				end if
		end do
		do ll = 1,25
		    image(ii,jj,ll) = abs(ampl(list(ll)))
		end do
	end do
	io_real(1) = 100.0*float((ii-1)*npy+jj)/float(npx*npy)
	if(mod(ii,10) .eq. 0) call WriteValue('completed    ',io_real,1,'(F8.2," %")')
end do

datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
datafile = EMsoft_toNativePath(datafile)
inquire(file=trim(datafile), exist=f_exists)

if (f_exists) then
  open(unit=dataunit, file=trim(datafile), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

nullify(HDF_head%next)

!if(.not. f_exists) then
! Create a new file using the default properties.
hdferr =  HDF_createFile(datafile, HDF_head)
!else
!	hdferr = HDF_openFile(datafile, HDF_head)
!end if

groupname = 'BFDF'
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_BFImage
hdferr = HDF_writeDatasetFloatArray3D(dataset, image, npx, npy, 25, HDF_head)

dataset = SC_DiffractionPattern
hdferr = HDF_writeDatasetDoubleArray4D(dataset, dp, npix, npix, nz, 5, HDF_head)

call HDF_pop(HDF_head)

! and close the fortran hdf interface
call h5close_f(hdferr)

stop
end if
!do ii = 1, npx
!	do jj = 1, npy
!		ampl = cmplx(0.D0, 0.D0)
!		ampl(1) = cmplx(1.D0,0.D0)
!		do ll = 1,nz
!			alpha = microstructure(ii,jj,ll)
!			A_tmp = alpha*A_g + (1 - alpha)*A_gp
!			ampl = matmul(A_tmp,ampl)
!		end do
!		image(ii,jj) = abs(ampl(1))
!	end do
!	io_real(1) = 100.0*float((ii-1)*npy+jj)/float(npx*npy)
!	if(mod(ii,10) .eq. 0) call WriteValue('completed    ',io_real,1,'(F8.2," %")')
!end do

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

!================================
! INITIALIZATION OF OpenCL DEVICE
!================================
call CLinit_PDCCQ(platform, nump, enl%platid, device, numd, enl%devid, info, context, command_queue)

! read the cl source file
sourcefile = 'gamma.cl'
call CLread_source_file(sourcefile, csource, slength)

!============================================================
!SET SIZE OF ALL OPENCL VARIABLES
!============================================================

allocate(wavefncoeff(npx * npy * nref_gp))
wavefncoeff = cmplx(0.0,0.0)

allocate(tmpphase(npx*npy*nz))
tmpphase = 0.0

allocate(gx(nref_gp * nref_gp),gy(nref_gp * nref_gp),gz(nref_gp * nref_gp))
gx = 0.0
gy = 0.0
gz = 0.0

size_in_bytes_scat_matrix = nref_gp * nref_gp * sizeof(wavefncoeff(1))

size_in_bytes_gminusgp     = nref_gp * nref_gp * sizeof(gx(1))

size_in_bytes_matrix      = npx * npy * nref_gp * nref_gp * sizeof(wavefncoeff(1))

size_in_bytes_vector      = npx * npy * nref_gp * sizeof(wavefncoeff(1))

size_in_bytes_coeff       = 9 * sizeof(coef(1))

size_in_bytes_phase       = npx * npy * nz * sizeof(tmpphase(1))

!============================================================
!GENERATE ALL THE BUFFERS AND ALLOCATE RESULT ARRAY
!============================================================

cl_ScatMat_g = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_scat_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_ScatMat_g', ierr)

cl_ScatMat_gp_v1 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_scat_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_ScatMat_gp_v1', ierr)

cl_ScatMat_gp_v2 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_scat_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_ScatMat_gp_v2', ierr)

cl_ScatMat_gp_v3 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_scat_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_ScatMat_gp_v3', ierr)

cl_ScatMat_gp_v4 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_scat_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_ScatMat_gp_v4', ierr)

! allocate device memory for experimental and dictionary patterns
cl_Ain = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_A', ierr)

cl_expA = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_expA', ierr)

cl_AA = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_AA', ierr)

cl_AAA = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_AAA', ierr)

cl_T1 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_T1', ierr)

cl_T2 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_matrix, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_T2', ierr)

cl_wavefncoeff = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_vector, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_wavefncoeff', ierr)

cl_wavefncoeffintd = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_vector, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_wavefncoeffintd', ierr)

cl_pf_v1 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_pf_v1', ierr)

cl_pf_v2 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_pf_v2', ierr)

cl_pf_v3 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_pf_v3', ierr)

cl_pf_v4 = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_pf_v4', ierr)

cl_coeff = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_coeff, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_coeff', ierr)

cl_dfx = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_dfx', ierr)

cl_dfy = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_dfy', ierr)

cl_dfz = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_phase, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_dfz', ierr)

cl_gminusgp_x = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_gminusgp_x', ierr)

cl_gminusgp_y = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_gminusgp_y', ierr)

cl_gminusgp_z = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_gminusgp, C_NULL_PTR, ierr)
call CLerror_check('Gamma-Gammap:clCreateBuffer:cl_gminusgp_y', ierr)

call Message('--> Done allocating all GPU buffers.')
!==========================DONE===============================================================

allocate(tmparray(nref_gp,nref_gp),tmparraylin(nref_gp**2))
tmparray    = cmplx(0.0,0.0)
tmparraylin = cmplx(0.0,0.0)

!============================================================
!COPY ALL THE DATA TO THE BUFFERS
!============================================================

do ii = 1,nref_gp
	do jj = 1,nref_gp
		tmparraylin((ii-1)*nref_gp+jj) = ScatMat_g(jj,ii)
	end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_ScatMat_g, CL_TRUE, 0_8, size_in_bytes_scat_matrix, &
						  C_LOC(tmparraylin(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

tmparray = ScatMat_gp * Var(:,:,1)
do ii = 1,nref_gp
	do jj = 1,nref_gp
		tmparraylin((ii-1)*nref_gp+jj) = tmparray(jj,ii)
	end do
end do

ierr = clEnqueueWriteBuffer(command_queue, cl_ScatMat_gp_v1, CL_TRUE, 0_8, size_in_bytes_scat_matrix, &
						  C_LOC(tmparraylin(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)


tmparray = ScatMat_gp * Var(:,:,2)
do ii = 1,nref_gp
	do jj = 1,nref_gp
		tmparraylin((ii-1)*nref_gp+jj) = tmparray(jj,ii)
	end do
end do

ierr = clEnqueueWriteBuffer(command_queue, cl_ScatMat_gp_v2, CL_TRUE, 0_8, size_in_bytes_scat_matrix, &
						  C_LOC(tmparraylin(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)


tmparray = ScatMat_gp * Var(:,:,3)
do ii = 1,nref_gp
	do jj = 1,nref_gp
		tmparraylin((ii-1)*nref_gp+jj) = tmparray(jj,ii)
	end do
end do

ierr = clEnqueueWriteBuffer(command_queue, cl_ScatMat_gp_v3, CL_TRUE, 0_8, size_in_bytes_scat_matrix, &
						  C_LOC(tmparraylin(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

tmparray = ScatMat_gp * Var(:,:,4)
do ii = 1,nref_gp
	do jj = 1,nref_gp
		tmparraylin((ii-1)*nref_gp+jj) = tmparray(jj,ii)
	end do
end do

ierr = clEnqueueWriteBuffer(command_queue, cl_ScatMat_gp_v4, CL_TRUE, 0_8, size_in_bytes_scat_matrix, &
						  C_LOC(tmparraylin(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)


ierr = clEnqueueWriteBuffer(command_queue, cl_coeff, CL_TRUE, 0_8, size_in_bytes_coeff, &
						  C_LOC(coef(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

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

ierr = clEnqueueWriteBuffer(command_queue, cl_gminusgp_x, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
						  C_LOC(gx(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_gminusgp_y, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
						  C_LOC(gy(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

ierr = clEnqueueWriteBuffer(command_queue, cl_gminusgp_z, CL_TRUE, 0_8, size_in_bytes_gminusgp, &
						  C_LOC(gz(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

! here all the phase fraction of each of the four variants is copied
! on to the GPU device. the phase fraction of the matrix is computed
! as the difference of the sum of the variant fractions from unity.

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = eta1(ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_pf_v1, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = eta2(ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_pf_v2, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = eta3(ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_pf_v3, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = eta4(ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_pf_v4, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = dispfield(1,ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_dfx, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = dispfield(2,ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_dfy, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

do ii = 1,npx
    do jj = 1,npy
        do ll = 1,nz
            tmpphase((ii-1)*npy*nz + (jj-1)*nz + ll) = dispfield(3,ii,jj,ll)
        end do
    end do
end do
ierr = clEnqueueWriteBuffer(command_queue, cl_dfz, CL_TRUE, 0_8, size_in_bytes_phase, &
						  C_LOC(tmpphase(1)), 0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('MasterSubroutine:clEnqueueWriteBuffer', ierr)

call Message('--> Done data transfer to all GPU buffers.')

!==========================DONE===============================================================

!=========================================================================
! BUILD THE KERNEL
!=========================================================================

! create the program
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(slength), ierr)
call CLerror_check('gammaGPU:clCreateProgramWithSource', ierr)

! build the program
progoptions = '-cl-no-signed-zeros'
! ierr = clBuildProgram(prog, numd, C_LOC(device), C_LOC(progoptions), C_NULL_FUNPTR, C_NULL_PTR)
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(enl%devid), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
if(len(trim(source)) > 0) call Message(trim(source(1:cnum)),frm='(A)')
call CLerror_check('gammaGPU:clBuildProgram', ierr)
call CLerror_check('gammaGPU:clGetProgramBuildInfo', ierr2)

! finally get the kernel and release the program
kernelname = 'gamma_gammap'
ckernelname = kernelname
ckernelname(13:13) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('gammaGPU:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('gammaGPU:clReleaseProgram', ierr)

call Message('--> Done creating OpenCL executable.')

!==========================DONE===========================================

!=========================================================================
! SET KERNEL ARGUMENTS
!=========================================================================

ierr =  clSetKernelArg(kernel, 0, sizeof(cl_ScatMat_g), C_LOC(cl_ScatMat_g))
call CLerror_check('gammaGPU:clSetKernelArg:cl_ScatMat_g', ierr)

ierr =  clSetKernelArg(kernel, 1, sizeof(cl_ScatMat_gp_v1), C_LOC(cl_ScatMat_gp_v1))
call CLerror_check('gammaGPU:clSetKernelArg:cl_ScatMat_gp_v1', ierr)

ierr =  clSetKernelArg(kernel, 2, sizeof(cl_ScatMat_gp_v2), C_LOC(cl_ScatMat_gp_v2))
call CLerror_check('gammaGPU:clSetKernelArg:cl_ScatMat_gp_v2', ierr)

ierr =  clSetKernelArg(kernel, 3, sizeof(cl_ScatMat_gp_v3), C_LOC(cl_ScatMat_gp_v3))
call CLerror_check('gammaGPU:clSetKernelArg:cl_ScatMat_gp_v3', ierr)

ierr =  clSetKernelArg(kernel, 4, sizeof(cl_ScatMat_gp_v4), C_LOC(cl_ScatMat_gp_v4))
call CLerror_check('gammaGPU:clSetKernelArg:cl_ScatMat_gp_v4', ierr)

ierr =  clSetKernelArg(kernel, 5, sizeof(cl_expA), C_LOC(cl_expA))
call CLerror_check('gammaGPU:clSetKernelArg:cl_expA', ierr)

ierr =  clSetKernelArg(kernel, 6, sizeof(cl_Ain), C_LOC(cl_Ain))
call CLerror_check('gammaGPU:clSetKernelArg:cl_A', ierr)

ierr =  clSetKernelArg(kernel, 7, sizeof(cl_AA), C_LOC(cl_AA))
call CLerror_check('gammaGPU:clSetKernelArg:cl_AA', ierr)

ierr =  clSetKernelArg(kernel, 8, sizeof(cl_AAA), C_LOC(cl_AAA))
call CLerror_check('gammaGPU:clSetKernelArg:cl_AAA', ierr)

ierr =  clSetKernelArg(kernel, 9, sizeof(cl_coeff), C_LOC(cl_coeff))
call CLerror_check('gammaGPU:clSetKernelArg:cl_coeff', ierr)

ierr =  clSetKernelArg(kernel, 10, sizeof(cl_T1), C_LOC(cl_T1))
call CLerror_check('gammaGPU:clSetKernelArg:cl_T1', ierr)

ierr =  clSetKernelArg(kernel, 11, sizeof(cl_T2), C_LOC(cl_T2))
call CLerror_check('gammaGPU:clSetKernelArg:cl_T2', ierr)

ierr =  clSetKernelArg(kernel, 12, sizeof(cl_wavefncoeff), C_LOC(cl_wavefncoeff))
call CLerror_check('gammaGPU:clSetKernelArg:cl_wavefncoeff', ierr)

ierr =  clSetKernelArg(kernel, 13, sizeof(cl_wavefncoeffintd), C_LOC(cl_wavefncoeffintd))
call CLerror_check('gammaGPU:clSetKernelArg:cl_wavefncoeffintd', ierr)

ierr =  clSetKernelArg(kernel, 14, sizeof(cl_pf_v1), C_LOC(cl_pf_v1))
call CLerror_check('gammaGPU:clSetKernelArg:cl_pf_v1', ierr)

ierr =  clSetKernelArg(kernel, 15, sizeof(cl_pf_v2), C_LOC(cl_pf_v2))
call CLerror_check('gammaGPU:clSetKernelArg:cl_pf_v1', ierr)

ierr =  clSetKernelArg(kernel, 16, sizeof(cl_pf_v3), C_LOC(cl_pf_v3))
call CLerror_check('gammaGPU:clSetKernelArg:cl_pf_v1', ierr)

ierr =  clSetKernelArg(kernel, 17, sizeof(cl_pf_v4), C_LOC(cl_pf_v4))
call CLerror_check('gammaGPU:clSetKernelArg:cl_pf_v1', ierr)

ierr =  clSetKernelArg(kernel, 18, sizeof(nref_gp), C_LOC(nref_gp))
call CLerror_check('gammaGPU:clSetKernelArg:nref_gp', ierr)

ierr =  clSetKernelArg(kernel, 19, sizeof(nz), C_LOC(nz))
call CLerror_check('gammaGPU:clSetKernelArg:nz', ierr)

ierr =  clSetKernelArg(kernel, 20, sizeof(cl_dfx), C_LOC(cl_dfx))
call CLerror_check('gammaGPU:clSetKernelArg:cl_dfx', ierr)

ierr =  clSetKernelArg(kernel, 21, sizeof(cl_dfy), C_LOC(cl_dfy))
call CLerror_check('gammaGPU:clSetKernelArg:cl_dfy', ierr)

ierr =  clSetKernelArg(kernel, 22, sizeof(cl_dfz), C_LOC(cl_dfz))
call CLerror_check('gammaGPU:clSetKernelArg:cl_dfz', ierr)

ierr =  clSetKernelArg(kernel, 23, sizeof(cl_gminusgp_x), C_LOC(cl_gminusgp_x))
call CLerror_check('gammaGPU:clSetKernelArg:cl_gminusgp_x', ierr)

ierr =  clSetKernelArg(kernel, 24, sizeof(cl_gminusgp_y), C_LOC(cl_gminusgp_y))
call CLerror_check('gammaGPU:clSetKernelArg:cl_gminusgp_y', ierr)

ierr =  clSetKernelArg(kernel, 25, sizeof(cl_gminusgp_z), C_LOC(cl_gminusgp_z))
call CLerror_check('gammaGPU:clSetKernelArg:cl_gminusgp_z', ierr)

call Message('--> Done setting all kernel arguments.')

!==========================DONE===========================================

!=========================================================================
! EXECUTE THE KERNEL
!=========================================================================

globalsize = (/npx, npy/)
localsize  = (/16,16/)

!execute the kernel
!ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), &
!                              0, C_NULL_PTR, C_NULL_PTR)
ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_NULL_PTR, &
                              0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('gammaGPU:clEnqueueNDRangeKernel', ierr)

ierr = clFinish(command_queue)
call CLerror_check('gammaGPU:clFinish', ierr)

call Message('--> kernel finished run. copying data now.')

!==========================DONE===========================================

!=========================================================================
! COPY THE RESULTS
!=========================================================================

! wait for the commands to finish
ierr = clFinish(command_queue)
call CLerror_check('gammaGPU:clFinish', ierr)

! read the resulting vector from device memory
ierr = clEnqueueReadBuffer(command_queue,cl_wavefncoeff,CL_TRUE,0_8,size_in_bytes_vector,&
						   C_LOC(wavefncoeff(1)),0,C_NULL_PTR,C_NULL_PTR)
call CLerror_check('gammaGPU:clEnqueueReadBuffer', ierr)

! calculate image from the wavefunction coefficients
! for now we will only be looking at bright field image

do ii = 1,npx
    do jj = 1,npy
        do mm = 1,25
            ll = ((ii-1)*npy + jj - 1)*nref_gp + mm
            image(ii,jj,mm) = abs(wavefncoeff(ll))
        end do
    end do
end do

ierr = clFinish(command_queue)
call CLerror_check('gammaGPU:clFinish', ierr)

call Message('--> Finished.')

!=========================================================================
! RELEASE EVERYTHING
!=========================================================================

ierr = clReleaseKernel(kernel)
call CLerror_check('gammaGPU:clReleaseKernel', ierr)

ierr = clReleaseCommandQueue(command_queue)
call CLerror_check('gammaGPU:clReleaseCommandQueue', ierr)

ierr = clReleaseContext(context)
call CLerror_check('gammaGPU:clReleaseContext', ierr)

ierr = clReleaseMemObject(cl_ScatMat_g)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_ScatMat_g', ierr)

ierr = clReleaseMemObject(cl_ScatMat_gp_v1)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_ScatMat_gp_v1', ierr)

ierr = clReleaseMemObject(cl_ScatMat_gp_v2)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_ScatMat_gp_v1', ierr)

ierr = clReleaseMemObject(cl_ScatMat_gp_v3)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_ScatMat_gp_v1', ierr)

ierr = clReleaseMemObject(cl_ScatMat_gp_v4)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_ScatMat_gp_v1', ierr)

ierr = clReleaseMemObject(cl_expA)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_expA', ierr)

ierr = clReleaseMemObject(cl_Ain)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_Ain', ierr)

ierr = clReleaseMemObject(cl_AA)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_AA', ierr)

ierr = clReleaseMemObject(cl_AAA)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_AAA', ierr)

ierr = clReleaseMemObject(cl_T1)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_T1', ierr)

ierr = clReleaseMemObject(cl_T2)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_T2', ierr)

ierr = clReleaseMemObject(cl_wavefncoeff)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_wavefncoeff', ierr)

ierr = clReleaseMemObject(cl_wavefncoeffintd)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_wavefncoeffintd', ierr)

ierr = clReleaseMemObject(cl_pf_v1)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_pf_v1', ierr)

ierr = clReleaseMemObject(cl_pf_v2)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_pf_v2', ierr)

ierr = clReleaseMemObject(cl_pf_v3)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_pf_v3', ierr)

ierr = clReleaseMemObject(cl_pf_v4)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_pf_v4', ierr)

ierr = clReleaseMemObject(cl_dfx)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_dfx', ierr)

ierr = clReleaseMemObject(cl_dfy)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_dfy', ierr)

ierr = clReleaseMemObject(cl_dfz)
call CLerror_check('gammaGPU:clReleaseMemObject:cl_dfz', ierr)
!==========================DONE===========================================

datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
datafile = EMsoft_toNativePath(datafile)
inquire(file=trim(datafile), exist=f_exists)

if (f_exists) then
  open(unit=dataunit, file=trim(datafile), status='old',form='unformatted')
  close(unit=dataunit, status='delete')
end if

nullify(HDF_head%next)

!if(.not. f_exists) then
! Create a new file using the default properties.
hdferr =  HDF_createFile(datafile, HDF_head)
!else
!	hdferr = HDF_openFile(datafile, HDF_head)
!end if

groupname = 'Bright Field'
hdferr = HDF_createGroup(groupname, HDF_head)

dataset = SC_BFImage
hdferr = HDF_writeDatasetFloatArray3D(dataset, image, npx, npy, 25, HDF_head)

!dataset = SC_DiffractionPattern
!hdferr = HDF_writeDatasetDoubleArray4D(dataset, dp, npix, npix, nz, 4, HDF_head)

call HDF_pop(HDF_head)

! and close the fortran hdf interface
call h5close_f(hdferr)

end subroutine ComputeGAMMAimage

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
complex(kind=dbl),INTENT(OUT)						:: Varout(nref, nref, 4)
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
