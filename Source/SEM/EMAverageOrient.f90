! ###################################################################
! Copyright (c) 2013-2015, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMAverageOrient.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMAverageOrient
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a ctf file with averaged orientations based on a dot product output file
!
!> @date 06/24/16 MDG 1.0 original
!> @date 07/06/16 MDG 1.1 replaced computation with call to EBSDgetAverageOrientations
!--------------------------------------------------------------------------
program EMAverageOrient

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use constants
use dictmod
use io
use error
use ECPmod
use EBSDiomod
use EBSDmod
use EBSDDImod
use HDF5
use HDFsupport
use rotations
use Lambert
use quaternions
use stringconstants

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(AverageOrientationNameListType)    :: enl
type(EBSDIndexingNameListType)          :: ebsdnl

logical                                 :: stat, readonly, noindex
integer(kind=irg)                       :: hdferr, nlines, FZcnt, Nexp, nnm, nnk, Pmdims, i, j, k, olabel, Nd, Ne, ipar(10), &
                                           ipar2(6), pgnum, ipat
character(fnlen)                        :: groupname, dataset, dpfile, energyfile, masterfile, efile, fname
integer(HSIZE_T)                        :: dims2(2),dims(1),dims2D(2)
real(kind=sgl)                          :: q1(4), q2(4), qus(4), a, oldmo, p(4), qsmall(4), theta, vec(3)
type(dicttype)                          :: dict

character(fnlen),allocatable            :: stringarray(:)
real(kind=sgl),allocatable              :: Eulers(:,:), dplist(:,:), Eulerstmp(:,:), dplisttmp(:,:), avEuler(:,:), & 
                                           resultmain(:,:), disor(:), disorient(:,:), OSMmap(:,:), IQmap(:)
integer(kind=irg),allocatable           :: tmi(:,:), tmitmp(:,:), indexmain(:,:)

type(HDFobjectStackType),pointer        :: HDF_head

nullify(HDF_head)


nmldeffile = 'EMAverageOrient.nml'
progname = 'EMAverageOrient.f90'
progdesc = 'Generate a ctf file with averaged orientations'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 81 /), progname)

! deal with the namelist stuff
call GetAverageOrientationNameList(nmldeffile,enl)

!====================================
! read the relevant fields from the dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

dpfile = trim(EMsoft_getEMdatapathname())//trim(enl%dotproductfile)
dpfile = EMsoft_toNativePath(dpfile)

! is this a proper HDF5 file ?
call h5fis_hdf5_f(trim(dpfile), stat, hdferr)

if (stat) then 
! open the dot product file 
  readonly = .TRUE.
  hdferr =  HDF_openFile(dpfile, HDF_head, readonly)

  if (enl%oldformat.eqv..TRUE.) then

dataset = SC_FZcnt
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)

dataset = SC_NumExptPatterns
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, Nexp)

dataset = SC_PointGroupNumber
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, pgnum)

dataset = SC_EulerAngles
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, Eulerstmp)
    allocate(Eulers(3,FZcnt))
    do i=1,FZcnt
      Eulers(1:3,i) = Eulerstmp(1:3,i)
    end do
    deallocate(Eulerstmp)
    Eulers = Eulers * sngl(cPi)/180.0

dataset = SC_DotProducts
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, dplisttmp)

    !allocate(dplist(dims2(1),Nexp))
    allocate(dplist(enl%nmuse,Nexp))
    do i=1,Nexp
      dplist(1:enl%nmuse,i) = dplisttmp(1:enl%nmuse,i)
    end do
    deallocate(dplisttmp)
    nnm = enl%nmuse
  
dataset = SC_Indices
    call HDF_readDatasetIntegerArray2D(dataset, dims2, HDF_head, hdferr, tmitmp)
    allocate(tmi(nnm,Nexp))
    do i=1,Nexp
      tmi(1:nnm,i) = tmitmp(1:nnm,i)
    end do
    deallocate(tmitmp)

  else ! this file has the new internal format (after ~ April 2016)
! get the energyfile and masterfile parameters from NMLParameters
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSDIndexingNameListType
    hdferr = HDF_openGroup(groupname, HDF_head)

dataset = SC_energyfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    energyfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_masterfile
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    masterfile = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_nnk
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, nnk)

dataset = SC_ipfwd
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ipf_wd)
dataset = SC_ipfht
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ebsdnl%ipf_ht)

!   dataset = 'StepX'
!   call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepX)
!   dataset = 'StepY'
!   call HDF_readDatasetFloat(dataset, HDF_head, hdferr, ebsdnl%StepY)


! and close the NMLparameters group
    call HDF_pop(HDF_head)
    call HDF_pop(HDF_head)

! open the Scan 1/EBSD/Data group
    groupname = SC_Scan1
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_EBSD
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_Data
    hdferr = HDF_openGroup(groupname, HDF_head)

! integers
dataset = SC_FZcnt
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)

dataset = SC_NumExptPatterns
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, Nexp)

! arrays
dataset = SC_OSM
    call HDF_readDatasetFloatArray2D(dataset, dims2D, HDF_head, hdferr, OSMmap)

dataset = SC_IQ
    call HDF_readDatasetFloatArray1D(dataset, dims, HDF_head, hdferr, IQmap)

dataset = SC_EulerAngles
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, Eulerstmp)
    allocate(Eulers(3,FZcnt))
    do i=1,FZcnt
      Eulers(1:3,i) = Eulerstmp(1:3,i)
    end do
    deallocate(Eulerstmp)
    Eulers = Eulers * sngl(cPi)/180.0


dataset = SC_TopDotProductList
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, dplisttmp)
    allocate(dplist(dims2(1),Nexp))
    do i=1,Nexp
      dplist(1:dims2(1),i) = dplisttmp(1:dims2(1),i)
    end do
    deallocate(dplisttmp)
    nnm = dims2(1)
  
dataset = SC_TopMatchIndices
    call HDF_readDatasetIntegerArray2D(dataset, dims2, HDF_head, hdferr, tmitmp)
    allocate(tmi(nnm,Nexp))
    do i=1,Nexp
      tmi(1:nnm,i) = tmitmp(1:nnm,i)
    end do
    deallocate(tmitmp)
  end if
  call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
  call h5close_EMsoft(hdferr)

  call Message('dot product HDF5 file read')
else
  dpfile = 'File '//trim(dpfile)//' is not an HDF5 file'
  call FatalError('EMAverageOrient',dpfile)
end if

if (enl%oldformat.eqv..FALSE.) then
!===================================
! we will also need some of the crystallographic data, so that requires
! extracting the xtalname from the energyfile
  efile = trim(EMsoft_getEMdatapathname())//trim(energyfile)
  efile = EMsoft_toNativePath(efile)


! first, we need to check whether or not the input file is of the HDF5 format type; if
! it is, we read it accordingly, otherwise we use the old binary format.
!
  call h5fis_hdf5_f(efile, stat, hdferr)

  if (stat) then
! open the fortran HDF interface
    call h5open_EMsoft(hdferr)

    nullify(HDF_head)

! open the MC file using the default properties.
    readonly = .TRUE.
    hdferr =  HDF_openFile(efile, HDF_head, readonly)

! open the namelist group
groupname = SC_NMLparameters
    hdferr = HDF_openGroup(groupname, HDF_head)
groupname = SC_MCCLNameList
    hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
dataset = SC_xtalname
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%MCxtalname = trim(stringarray(1))
    deallocate(stringarray)

dataset = SC_EkeV
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%EkeV)

dataset = SC_sig
    call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%MCsig)

    call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
    call h5close_EMsoft(hdferr)

    call Message('energy file read')
  else
    efile = 'File '//trim(efile)//' is not an HDF5 file'
    call FatalError('EMAverageOrient',efile)
  end if
  pgnum = GetPointGroup(ebsdnl%MCxtalname)
end if

! do the actual orientational averaging
allocate(avEuler(3,Nexp),disorient(Nexp,enl%nmuse))
ipar2(1) = pgnum
ipar2(2) = FZcnt
ipar2(3) = Nexp
if (enl%oldformat) then
  ipar2(4) = enl%nmuse
  ipar2(5) = Nexp
else
  ipar2(4) = nnk
  ipar2(5) = Nexp*ceiling(float(ebsdnl%ipf_wd*ebsdnl%ipf_ht)/float(Nexp))
end if
ipar2(6) = enl%nmuse
call EBSDgetAverageOrientations(ipar2, Eulers, tmi, dplist, avEuler,disorient)

open(unit=dataunit,file='disorient.data',status='unknown',form='unformatted')
write(dataunit) nnm, Nexp, enl%nmuse
write(dataunit) dplist
write(dataunit) disorient
close(unit=dataunit,status='keep')


if (enl%oldformat.eqv..FALSE.) then
! ok, so we have our list; next we need to store this in a ctf file, along with 
! all the other crystallographic information, so we need to fill in the proper 
! fields in the ebsdnl structure and ipar array.
  ipar = 0
  ipar(1) = 1
  ipar(2) = Nexp
  ipar(3) = Nexp
  ipar(4) = Nexp
  ipar(5) = FZcnt
  ipar(6) = pgnum
  ipar(7) = ebsdnl%ipf_wd
  ipar(8) = ebsdnl%ipf_ht
  ebsdnl%ctffile = enl%averagectffile
  
  allocate(indexmain(1,Nexp), resultmain(1,Nexp))
  indexmain = 0
  resultmain(1,1:Nexp) = dplist(1,1:Nexp)
  noindex = .TRUE.
  call h5open_EMsoft(hdferr)
  call ctfebsd_writeFile(ebsdnl,ipar,indexmain,avEuler,resultmain,OSMmap,IQmap,noindex)
  call h5close_EMsoft(hdferr)
  call Message('Data stored in ctf file : '//trim(enl%averagectffile))
else
! we need to store the result in a simple text file...
  fname = trim(EMsoft_getEMdatapathname())//trim(enl%averagetxtfile)
  fname = EMsoft_toNativePath(fname)

  open(unit=dataunit,file=trim(fname),status='unknown',form='formatted')
  write (dataunit,"(A2)") 'eu'
  write (dataunit,"(I10)") Nexp
  do i=1,Nexp
    write(dataunit,"(3F13.6)") avEuler(1:3,i)
  end do
  close(unit=dataunit,status='keep')
  call Message('Data stored in txt file : '//trim(enl%averagetxtfile))
end if

! do we need to produce a relative disorientation map ?
if (trim(enl%disorientationmap).ne.'undefined') then
!===================================
! set up the symmetry quaternions for this rotational symmetry
! allocate the dict structure
  dict%Num_of_init = 3
  dict%Num_of_iterations = 30
  dict%pgnum = pgnum
! initialize the symmetry matrices
  call DI_Init(dict,'nil') 

! put the Euler angles back in radians
  avEuler = avEuler * sngl(cPi)/180.0
! get the 1D point coordinate
  ipat = ebsdnl%ipf_wd * enl%reldisy + enl%reldisx
write (*,*) 'position of fixed point = ',enl%reldisx,enl%reldisy
  allocate(disor(Nexp))
! and get the disorientations w.r.t. all the others
  do j=1,Nexp   
    call getDisorientationAngle(avEuler(1:3,ipat),avEuler(1:3,j),dict,oldmo)
    disor(j) = oldmo
  end do
  disor = disor*180.0/sngl(cPi)

  open(unit=dataunit,file=trim(enl%disorientationmap),status='unknown',form='formatted')
  write (dataunit,"(3I10)") Nexp, ebsdnl%ipf_wd, ebsdnl%ipf_ht
  do i=1,Nexp
    write(dataunit,"(F13.6)") disor(i)
  end do
  close(unit=dataunit,status='keep')
end if

! average dot-product vs. average disorientation plot?




end program EMAverageOrient
