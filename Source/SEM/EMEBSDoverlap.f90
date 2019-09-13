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
!--------------------------------------------------------------------------
! EMsoft:EMEBSDoverlap.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDoverlap
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief EMEBSDoverlap merges master pattern data for a given orientation relation
!>
!> @note This program takes a pair of master pattern files, and 
!> merges them together for a given orientation relation between the two phases; they
!> could be the same phase for twins, for instance, or two different phases, for instance
!> a mixture of hcp and fcc with a given OR.  There are some conditions on the array sizes
!> that have to be met in order for this to work. This program has two different modes: 
!> in 'series' mode, a series of mixture master patterns is computed but only for the 
!> highest energy bin; these are then stored in a simple HDF5 file in square and circular 
!> Lambert formats as well as stereographic projection format.  In the other mode ('full'), a
!> complete new master pattern is computed for a given (fracA) volume fraction of the two 
!> phases.  The master pattern arrays are averaged over all atom positions in both phases, 
!> and are stored in square Lambert and stereographic formats in a standard master pattern HDF5 file.
!> This file has a few additional entries to make it clear that it contains an averaged master
!> pattern, and the crystal struture space group is set to #2 (-1) to reflect the fact that the
!> averaged symmetry is in general very low (but kept centrosymmetric).
!>
!> The averaged master pattern can be used in dictionary indexing as well as spherical indexing.
!
!> @date  08/01/12  MDG 1.0 EBSD extraction program for fundamental zone patterns
!> @date  08/17/12  MDG 1.1 generalized fundamental zone to other symmetries
!> @date  09/20/12  MDG 1.2 adapted for Lambert projection
!> @date  09/25/12  MDG 1.3 prepared for multithreaded version by separating computation steps
!> @date  12/11/12  MDG 2.0 new branch with energy-dependent Lambert projections (cubic only for now)
!> @date  02/26/14  MDG 3.0 incorporation into git and adapted to new libraries
!> @date  03/26/14  MDG 3.1 modification of file formats; made compatible with IDL visualization interface
!> @date  06/24/14  MDG 4.0 removal of all global variables; separation of nml from computation; OpenMP
!> @date  03/10/15  MDG 4.1 added output format selector
!> @date  04/02/15  MDG 5.0 changed program input & output to HDF format
!> @date  05/03/15  MDG 6.0 branched EMEBSD.f90 into this program; tested square-square case; others to be implemented
!> @date  05/04/15  MDG 6.1 added fracA volume fraction parameter; added stereographic projection and circular Lambert output
!> @date  01/03/18  MDG 7.0 update to make sure that this also works for TKD master patterns...
!> @date  04/04/18  MDG 8.0 updated for new use of name lists and data structures
!> @date  06/18/19  MDG 8.1 correction of overall pattern rotation matrix after error discovery for hexagonal master patterns
!> @date  06/18/19  MDG 8.2 added option to perform a complete merger of two master pattern files into a single new file
! ###################################################################

program EMEBSDoverlap

use local
use files
use crystal
use NameListTypedefs
use NameListHandlers
use NameListHDFwriters
use io
use EBSDmod
use error
use gvectors
use symmetry
use rotations
use quaternions
use constants
use Lambert
use HDFsupport
use initializersHDF
use initializers


IMPLICIT NONE

character(fnlen)                       :: nmldeffile, progname, progdesc, dataset
type(EBSDoverlapNameListType)          :: enl
type(MCCLNameListType)                 :: mcnlA, mcnlB
type(EBSDMasterNameListType)           :: mpnlA, mpnlB
type(EBSDMCdataType)                   :: EBSDMCdataA, EBSDMCdataB
type(EBSDMPdataType)                   :: EBSDMPdataA, EBSDMPdataB

integer(kind=irg)                      :: istat, ierr, i, j , hdferr, npx, npy
integer(kind=irg),allocatable          :: sA(:), sB(:)
integer(kind=irg),parameter            :: numfrac = 21
logical                                :: verbose, iv, overwrite
character(6)                           :: sqorheA, sqorheB
character(fnlen)                       :: outstr, datafile, xtalnameA, xtalnameB
type(unitcell)                         :: cellA, cellB
type(DynType),save                     :: DynA, DynB
type(gnode),save                       :: rlpA, rlpB
real(kind=sgl)                         :: dmin, voltage, TTAB(3,3), TT(3,3), io_real(3), cA, cB, scl, fA(numfrac), om(3,3), &
                                          PP(3), HH(3), CC(3)
real(kind=dbl)                         :: edge, xy(2), xyz(3), txyz(3), txy(2), Radius, dc(3)
type(orientation)                      :: orel    
real(kind=sgl),allocatable             :: master(:,:,:), masterLC(:,:,:), masterSP(:,:,:), masterNH(:,:,:,:), & 
                                          masterSH(:,:,:,:), ccA(:), ccB(:), SPNH(:,:,:), SPSH(:,:,:)
type(HDFobjectStackType)               :: HDF_head
character(fnlen,kind=c_char)           :: line2(1)
integer(HSIZE_T)                       :: dims4(4), cnt4(4), offset4(4)
integer(HSIZE_T)                       :: dims3(3), cnt3(3), offset3(3)
character(fnlen)                       :: groupname, datagroupname

interface
  function InterpolateMaster(dc, EBSDMPdata, s) result(res)

  use local
  use Lambert
  use EBSDmod
  use constants

  IMPLICIT NONE

  real(kind=dbl),INTENT(INOUT)            :: dc(3)
  type(EBSDMPdataType)                    :: EBSDMPdata
  integer(kind=irg),INTENT(IN)            :: s(3)
  real(kind=sgl)                          :: res
  
  end function InterpolateMaster

  function InterpolateCompleteMaster(dc, EBSDMPdata, s) result(res)

  use local
  use Lambert
  use EBSDmod
  use constants

  IMPLICIT NONE

  real(kind=dbl),INTENT(INOUT)            :: dc(3)
  type(EBSDMPdataType)                    :: EBSDMPdata
  integer(kind=irg),INTENT(IN)            :: s(4)
  real(kind=sgl)                          :: res(s(3),s(4))

  end function InterpolateCompleteMaster

  function OverlapInterpolateLambert(dc, master, npx, nf) result(res)

  use local
  use Lambert
  use EBSDmod
  use constants
  
  IMPLICIT NONE
  
  real(kind=dbl),INTENT(INOUT)            :: dc(3)
  real(kind=sgl),INTENT(IN)               :: master(-npx:npx,-npx:npx, 1:nf)
  integer(kind=irg),INTENT(IN)            :: npx 
  integer(kind=irg),INTENT(IN)            :: nf
  real(kind=sgl)                          :: res(nf)
  end function OverlapInterpolateLambert

end interface

verbose = .FALSE.

nmldeffile = 'EMEBSDoverlap.nml'
progname = 'EMEBSDoverlap.f90'
progdesc = 'Merge EBSD master patterns for a particular orientation relation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 23 /), progname)

! deal with the namelist stuff
call GetEBSDoverlapNameList(nmldeffile,enl)

! read EBSD master pattern files 
call h5open_EMsoft(hdferr)
call readEBSDMonteCarloFile(enl%masterfileA, mcnlA, hdferr, EBSDMCdataA)
xtalnameA = trim(mcnlA%xtalname)
call readEBSDMonteCarloFile(enl%masterfileB, mcnlB, hdferr, EBSDMCdataB)
xtalnameB = trim(mcnlB%xtalname)

if (trim(enl%overlapmode).eq.'series') then 
  call readEBSDMasterPatternFile(enl%masterfileA, mpnlA, hdferr, EBSDMPdataA, getmLPNH=.TRUE.)
  call readEBSDMasterPatternFile(enl%masterfileB, mpnlB, hdferr, EBSDMPdataB, getmLPNH=.TRUE.)
  allocate(sA(3), sB(3))
  sA = shape(EBSDMPdataA%mLPNH)
  sB = shape(EBSDMPdataB%mLPNH)
else
  ! we must keep the 4D master arrays in this case, since we want to create a correct new master pattern file
  call readEBSDMasterPatternFile(enl%masterfileA, mpnlA, hdferr, EBSDMPdataA, getmLPNH=.TRUE., getmLPSH=.TRUE., keep4=.TRUE.)
  call readEBSDMasterPatternFile(enl%masterfileB, mpnlB, hdferr, EBSDMPdataB, getmLPNH=.TRUE., getmLPSH=.TRUE., keep4=.TRUE.)
  allocate(sA(4), sB(4))
  sA = shape(EBSDMPdataA%mLPNH4)
  sB = shape(EBSDMPdataB%mLPNH4)
end if

call h5close_EMsoft(hdferr)

! make sure that the master pattern arrays have the same dimensions
write (*,*) 'array sizes : '
write (*,*) sA
write (*,*) sB
if (sum(sA(1:3)-sB(1:3)).ne.0) then
  call FatalError('EMEBSDoverlap','master patterns have different dimensions')
end if

! if the overlapmode is 'full', then we need to copy the phase A master pattern file in its entirety,
! and replace the master pattern arrays by overlap arrays; this can be useful for dictionary or spherical
! indexing runs when the majority of the patterns are overlap patterns between phases with a known OR.
! In that case we will also need to replace the crystal structure by a low symmetry one, possibly triclinic,
! since the symmetry of the overlap master pattern will generally be really low.
if (trim(enl%overlapmode).eq.'full') then
  call h5open_EMsoft(hdferr)
  if (enl%newpgnum.eq.-1) then
    call EBSDcopyMPdata(enl%masterfileA, enl%datafile, enl%h5copypath)
  else
    call EBSDcopyMPdata(enl%masterfileA, enl%datafile, enl%h5copypath, skipCrystalData=.TRUE.)
  end if
  call h5close_EMsoft(hdferr)
end if

!=============================
!=============================
! ok, we're in business... let's initialize the crystal structures so that we
! can compute the orientation relation matrix
! nullify(cellA,cellB)
! allocate(cellA,cellB)
dmin=0.05
voltage = 30000.0

call Initialize_Cell(cellA,DynA,rlpA,xtalnameA, dmin, voltage, verbose)
call Initialize_Cell(cellB,DynB,rlpB,xtalnameB, dmin, voltage, verbose)

!=============================
!=============================
orel%gA = enl%gA
orel%gB = enl%gB
orel%tA = enl%tA
orel%tB = enl%tB

! check the OR for orthogonality
if (sum(orel%gA*orel%tA).ne.0.0) then
  call FatalError('EMEBSDoverlap','gA and tA must be orthogonal !!!')
end if

if (sum(orel%gB*orel%tB).ne.0.0) then
  call FatalError('EMEBSDoverlap','gB and tB must be orthogonal !!!')
end if

! compute the rotation matrix for this OR
TTAB = ComputeOR(orel, cellA, cellB, 'AB')
TT = transpose(matmul( cellA%rsm, matmul( TTAB, transpose(cellB%dsm))))

! apply the overall rotation to the requested reference frame
! defined by the PatternAxisA and HorizontalAxisA vectors
! make sure that these two directions are orthogonal
if (abs(CalcDot(cellA,float(enl%PatternAxisA),float(enl%HorizontalAxisA),'d')).gt.1.e-6) then 
  call FatalError('EMEBSDoverlap','PatternAxisA and HorizontalAxisA must be orthogonal !!!')
end if

! convert the vectors to the standard cartesian frame for structure A
call TransSpaceSingle(cellA, float(enl%PatternAxisA), PP, 'd', 'c')
call TransSpaceSingle(cellA, float(enl%HorizontalAxisA), HH, 'd', 'c')
iv = .FALSE.
call CalcCross(cellA, PP, HH, CC, 'c', 'c', 0)

call NormVec(cellA, PP, 'c')
call NormVec(cellA, HH, 'c')
call NormVec(cellA, CC, 'c')

! place these normalized vectors in the om array
om(1,1:3) = HH(1:3)
om(2,1:3) = CC(1:3)
om(3,1:3) = PP(1:3)
om = transpose(om)

outstr = ''
call WriteValue('Overall Transformation Matrix : ',outstr)
do i=1,3
  io_real(1:3) = om(i,1:3)
  call WriteValue('',io_real,3,"(3f10.6)")
end do

! output
outstr = ' '//trim(xtalnameA)//' --> '//trim(xtalnameB)
call WriteValue('Transformation Matrix : ',outstr)
do i=1,3
  io_real(1:3) = TT(i,1:3)
  call WriteValue('',io_real,3,"(3f10.6)")
end do

!=============================
!=============================
!=============================
!=============================
! next, if we are in 'series' overlapmode, allocate a new master array into which we'll write the superimposed patterns
! we discard all energies except for the highest energy
if (trim(enl%overlapmode).eq.'series') then
  npx = (sA(2)-1)/2
  npy = npx
  allocate(master(-npx:npx,-npy:npy,numfrac))

  fA = (/ (float(i)*0.05,i=0,numfrac-1) /)

  call WriteValue('','Each master pattern has its own intensity range.',"(/A)")
  call WriteValue('','This means that one pattern may dominate over another')
  call WriteValue('','even when the volume fractions of A and B are equal. ',"(A/)")
  io_real(1) = maxval(EBSDMPdataA%mLPNH)
  call WriteValue('maximum intensity in master A: ',io_real, 1)
  io_real(1) = maxval(EBSDMPdataB%mLPNH)
  call WriteValue('maximum intensity in master B: ',io_real, 1)

!=============================
!=============================
  edge = 1.0D0 / dble(npx)
  do i=-npx,npx
    do j=-npy,npy
! determine the spherical direction for this point
      xy = (/ dble(i), dble(j) /) * edge
      xyz = LambertSquareToSphere(xy, ierr)
! apply the overall pattern rotation with rotation matrix om
      xyz = matmul(om, xyz)
      call NormVec(cellA, xyz, 'c')
! since A is already square Lambert, all we need to do is compute the 
! beam orientation in crystal B, and sample the master pattern for that
! location. 
      txyz = matmul(TT, xyz)
! normalize these direction cosines (they are already in a cartesian reference frame!)
      txyz = txyz/sqrt(sum(txyz*txyz))
! and interpolate the masterB pattern
      cA = InterpolateMaster(xyz, EBSDMPdataA, sA)
      cB = InterpolateMaster(txyz, EBSDMPdataB, sB)
      master(i,j,1:numfrac) = fA(1:numfrac)*cA+(1.0-fA(1:numfrac))*cB
    end do
  end do
  call Message(' completed interpolation ')

  ! free up the master pattern memory
  deallocate(EBSDMPdataA%mLPNH, EBSDMPdataB%mLPNH)

  !=============================
  !=============================
  ! convert the square Lambert projection to a stereographic projection
  ! with the PatternAxis of structure A at the center
  allocate(masterSP(-npx:npx,-npy:npy,numfrac))
  Radius = 1.0
  do i=-npx,npx 
    do j=-npy,npy
      xy = (/ float(i), float(j) /) / float(npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      if (ierr.ne.0) then 
        masterSP(i,j,1:numfrac) = 0.0
      else
        masterSP(i,j,1:numfrac) = OverlapInterpolateLambert(xyz, master, npx, numfrac)
      end if
    end do
  end do

  call Message('completed SP conversion')

  ! convert the square Lambert projection to a circular Lambert projection
  ! with the PatternAxis of structure A at the center
  allocate(masterLC(-npx:npx,-npy:npy,numfrac))
  Radius = 1.0
  do i=-npx,npx 
    do j=-npy,npy
      xy = sqrt(2.0) * (/ float(i), float(j) /) / float(npx)
      if (sum(xy*xy).gt.2.0) then 
        masterLC(i,j,1:numfrac) = 0.0
      else
        xyz = LambertInverse( xy, ierr, Radius )
        masterLC(i,j,1:numfrac) = OverlapInterpolateLambert(xyz, master, npx, numfrac)
      end if
    end do
  end do

  call Message('completed LC conversion')

  ! finally, create simple HDF5 file with only the overlap master array in it
  nullify(HDF_head%next)
  ! Initialize FORTRAN interface.
  call h5open_f(hdferr)

  ! Create a new file using the default properties.
  datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
  hdferr =  HDF_createFile(datafile, HDF_head)

  ! create datasets 
  dataset = 'MasterLambertSquare'
  hdferr = HDF_writeDatasetFloatArray3D(dataset, master, 2*npx+1, 2*npx+1, numfrac, HDF_head)
   
  dataset = 'MasterLambertCircle'
  hdferr = HDF_writeDatasetFloatArray3D(dataset, masterLC, 2*npx+1, 2*npx+1, numfrac, HDF_head)
   
  dataset = 'MasterStereographic'
  hdferr = HDF_writeDatasetFloatArray3D(dataset, masterSP, 2*npx+1, 2*npx+1, numfrac, HDF_head)
   
  call HDF_pop(HDF_head,.TRUE.)

  ! and close the fortran hdf interface
  call h5close_f(hdferr)

  call WriteValue('Output data stored in '//trim(datafile),'',"(//A/)")
else    ! overlapmode = 'full'
! in this mode, we compute the complete master pattern array for a fracA volume fraction of pattern A,
! and store it in the output file which was generated earlier by an h5copy command.
  npx = (sA(2)-1)/2
  npy = npx
  allocate(masterNH(-npx:npx,-npy:npy,1:sA(3),1:sA(4)), masterSH(-npx:npx,-npy:npy,1:sA(3),1:sA(4)))
  masterNH = 0.0
  masterSH = 0.0
  allocate(ccA(sA(3)), ccB(sA(3)))

!=============================
!=============================
  edge = 1.0D0 / dble(npx)
  do i=-npx,npx
    do j=-npy,npy
! determine the spherical direction for this point
      xy = (/ dble(i), dble(j) /) * edge
      xyz = LambertSquareToSphere(xy, ierr)
! apply the overall pattern rotation with rotation matrix om
      xyz = matmul(om, xyz)
      call NormVec(cellA, xyz, 'c')
! since A is already square Lambert, all we need to do is compute the 
! beam orientation in crystal B, and sample the master pattern for that
! location. 
      txyz = matmul(TT, xyz)
! normalize these direction cosines (they are already in a cartesian reference frame!)
      txyz = txyz/sqrt(sum(txyz*txyz))
! and interpolate the master patterns
      ccA = sum(InterpolateCompleteMaster(xyz, EBSDMPdataA, sA), 2)
      ccB = sum(InterpolateCompleteMaster(txyz, EBSDMPdataB, sB), 2)
      masterNH(i,j,1:sA(3),1) = enl%fracA*ccA(1:sA(3))+(1.0-enl%fracA)*ccB(1:sA(3))
      masterSH(-i,-j,1:sA(3),1) = masterNH(i,j,1:sA(3),1) 
    end do
  end do
  call Message(' completed interpolation ')

! free up the original master pattern memory
  deallocate(EBSDMPdataA%mLPNH4, EBSDMPdataB%mLPNH4, EBSDMPdataA%mLPSH4, EBSDMPdataB%mLPSH4)

! recompute the stereographic projection arrays as well 
  allocate(SPNH(-npx:npx,-npy:npy,1:sA(3)), SPSH(-npx:npx,-npy:npy,1:sA(3)))
  SPNH = 0.0
  SPSH = 0.0
  allocate(master(-npx:npx,-npy:npy,1:sA(3)))
  master = sum(masterNH,4)   ! sum over all the atom positions

  Radius = 1.0
  do i=-npx,npx 
    do j=-npy,npy
      xy = (/ float(i), float(j) /) / float(npx)
      xyz = StereoGraphicInverse( xy, ierr, Radius )
      if (ierr.ne.0) then 
        SPNH(i,j,1:sA(3)) = 0.0
      else
        SPNH(i,j,1:sA(3)) = OverlapInterpolateLambert(xyz, master, npx, sA(3))
        SPSH(-i,-j,1:sA(3)) = SPNH(i,j,1:sA(3))
      end if
    end do
  end do

  deallocate(master)

  call Message(' completed SP conversion')

! overwrite the existing arrays in the output file 
  nullify(HDF_head%next)

! Initialize FORTRAN HDF interface.
  call h5open_EMsoft(hdferr)
  overwrite = .TRUE.

! open the existing file using the default properties.
  datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
  datafile = EMsoft_toNativePath(datafile)
  hdferr =  HDF_openFile(datafile, HDF_head)

! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
dataset = SC_EBSDoverlapNML
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)

! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)

  call HDFwriteEBSDoverlapNameList(HDF_head, enl)

! leave this group
  call HDF_pop(HDF_head)

! if the CrystalData group was not written to the output file by h5copy, 
! then we need to create it here with only the new SpaceGroupNumber 
! and the PointGroupNumber as data sets (since the merged pattern may have a different 
! symmetry than either of the member phases).
if (enl%newpgnum.ne.-1) then 
  groupname = SC_CrystalData
    hdferr = HDF_createGroup(groupname, HDF_head)

! write the PointGroupNumber data set 
  dataset = 'PointGroupNumber'
    hdferr = HDF_writeDataSetInteger(dataset, enl%newpgnum, HDF_head)

! write the SpaceGroupNumber data set 
  dataset = SC_SpaceGroupNumber
    hdferr = HDF_writeDataSetInteger(dataset, SGPG(enl%newpgnum), HDF_head)

  call HDF_pop(HDF_head)
end if

groupname = SC_EMData
datagroupname = SC_EBSDmaster
  hdferr = HDF_openGroup(groupname, HDF_head)
  hdferr = HDF_openGroup(datagroupname, HDF_head)

! add data to the hyperslab
dataset = SC_mLPNH
  dims4 = (/  2*npx+1, 2*npx+1, sA(3), sA(4) /)
  cnt4 = (/ 2*npx+1, 2*npx+1, sA(3), sA(4) /)
  offset4 = (/ 0, 0, 0, 0 /)
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, masterNH, dims4, offset4, cnt4(1), cnt4(2), cnt4(3), cnt4(4), &
                                          HDF_head, overwrite)

dataset = SC_mLPSH
  hdferr = HDF_writeHyperslabFloatArray4D(dataset, masterSH, dims4, offset4, cnt4(1), cnt4(2), cnt4(3), cnt4(4), &
                                          HDF_head, overwrite)

  deallocate(masterNH, masterSH)

dataset = SC_masterSPNH
  dims3 = (/  2*npx+1, 2*npx+1, sA(3) /)
  cnt3 = (/ 2*npx+1, 2*npx+1, sA(3) /)
  offset3 = (/ 0, 0, 0 /)
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, SPNH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, overwrite)

dataset = SC_masterSPSH
  hdferr = HDF_writeHyperslabFloatArray3D(dataset, SPSH, dims3, offset3, cnt3(1), cnt3(2), cnt3(3), HDF_head, overwrite)

  deallocate(SPNH, SPSH)

  call HDF_pop(HDF_head)
  call HDF_pop(HDF_head)

! and, at the top level of the file, add a string that states that this is a modified master pattern file 
dataset = SC_READMEFIRST
  line2(1) = 'Caution: This master pattern file was generated by the EMEBSDoverlap program!  See the NMLparameters group. '
  hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head)

  call HDF_pop(HDF_head, .TRUE.)

! and close the fortran hdf interface
  call h5close_EMsoft(hdferr)

  call Message(' merged patterns written to output file.')

end if

end program EMEBSDoverlap


function OverlapInterpolateLambert(dc, master, npx, nf) result(res)

use local
use Lambert
use EBSDmod
use constants

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)            :: dc(3)
real(kind=sgl),INTENT(IN)               :: master(-npx:npx,-npx:npx, 1:nf)
integer(kind=irg),INTENT(IN)            :: npx 
integer(kind=irg),INTENT(IN)            :: nf
real(kind=sgl)                          :: res(nf)

integer(kind=irg)                       :: nix, niy, nixp, niyp, istat
real(kind=sgl)                          :: xy(2), dx, dy, dxm, dym, scl

scl = float(npx) !/ LPs%sPio2

if (dc(3).lt.0.0) dc = -dc

! convert direction cosines to lambert projections
xy = scl * LambertSphereToSquare( dc, istat )
res = 0.0

if (istat.eq.0) then 
! interpolate intensity from the neighboring points
  nix = floor(xy(1))
  niy = floor(xy(2))
  nixp = nix+1
  niyp = niy+1
  if (nixp.gt.npx) nixp = nix
  if (niyp.gt.npx) niyp = niy
  dx = xy(1) - nix
  dy = xy(2) - niy
  dxm = 1.0 - dx
  dym = 1.0 - dy
  
  res(1:nf) = master(nix,niy,1:nf)*dxm*dym + master(nixp,niy,1:nf)*dx*dym + &
        master(nix,niyp,1:nf)*dxm*dy + master(nixp,niyp,1:nf)*dx*dy
end if

end function OverlapInterpolateLambert


function InterpolateMaster(dc, EBSDMPdata, s) result(res)

use local
use Lambert
use EBSDmod
use constants

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)            :: dc(3)
type(EBSDMPdataType)                    :: EBSDMPdata
integer(kind=irg),INTENT(IN)            :: s(3)
real(kind=sgl)                          :: res

integer(kind=irg)                       :: nix, niy, nixp, niyp, istat, npx
real(kind=sgl)                          :: xy(2), dx, dy, dxm, dym, scl, tmp

npx = (s(1)-1)/2
if (dc(3).lt.0.0) dc = -dc

! convert direction cosines to lambert projections
scl = float(npx) !/ LPs%sPio2
xy = scl * LambertSphereToSquare( dc, istat )
res = 0.0

if (istat.eq.0) then 
! interpolate intensity from the neighboring points
  nix = floor(xy(1))
  niy = floor(xy(2))
  nixp = nix+1
  niyp = niy+1
  if (nixp.gt.npx) nixp = nix
  if (niyp.gt.npx) niyp = niy
  dx = xy(1) - nix
  dy = xy(2) - niy
  dxm = 1.0 - dx
  dym = 1.0 - dy
  
  res = EBSDMPdata%mLPNH(nix,niy,s(3))*dxm*dym + EBSDMPdata%mLPNH(nixp,niy,s(3))*dx*dym + &
        EBSDMPdata%mLPNH(nix,niyp,s(3))*dxm*dy + EBSDMPdata%mLPNH(nixp,niyp,s(3))*dx*dy
end if

end function InterpolateMaster

function InterpolateCompleteMaster(dc, EBSDMPdata, s) result(res)

use local
use Lambert
use EBSDmod
use constants

IMPLICIT NONE

real(kind=dbl),INTENT(INOUT)            :: dc(3)
type(EBSDMPdataType)                    :: EBSDMPdata
integer(kind=irg),INTENT(IN)            :: s(4)
real(kind=sgl)                          :: res(s(3),s(4))

integer(kind=irg)                       :: nix, niy, nixp, niyp, istat, npx
real(kind=sgl)                          :: xy(2), dx, dy, dxm, dym, scl, tmp

npx = (s(1)-1)/2
if (dc(3).lt.0.0) dc = -dc

! convert direction cosines to lambert projections
scl = float(npx) !/ LPs%sPio2
xy = scl * LambertSphereToSquare( dc, istat )
res = 0.0

if (istat.eq.0) then 
! interpolate intensity from the neighboring points
  nix = floor(xy(1))
  niy = floor(xy(2))
  nixp = nix+1
  niyp = niy+1
  if (nixp.gt.npx) nixp = nix
  if (niyp.gt.npx) niyp = niy
  dx = xy(1) - nix
  dy = xy(2) - niy
  dxm = 1.0 - dx
  dym = 1.0 - dy
  
  res(1:s(3),1:s(4)) = EBSDMPdata%mLPNH4(nix,niy ,1:s(3),1:s(4))*dxm*dym + EBSDMPdata%mLPNH4(nixp, niy,1:s(3),1:s(4))*dx*dym+&
                       EBSDMPdata%mLPNH4(nix,niyp,1:s(3),1:s(4))*dxm*dy  + EBSDMPdata%mLPNH4(nixp,niyp,1:s(3),1:s(4))*dx*dy
end if

end function InterpolateCompleteMaster

