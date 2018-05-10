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
!> @date 03/12/18 MDG 1.2 replaced reading of dot product file by new subroutine
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
type(EBSDDIdataType)                    :: EBSDDIdata

logical                                 :: stat, readonly, noindex
integer(kind=irg)                       :: hdferr, nlines, FZcnt, Nexp, nnm, nnk, Pmdims, i, j, k, olabel, Nd, Ne, ipar(10), &
                                           ipar2(6), pgnum, ipat
character(fnlen)                        :: groupname, dataset, dpfile, energyfile, masterfile, efile, fname
integer(HSIZE_T)                        :: dims2(2),dims(1),dims2D(2)
real(kind=sgl)                          :: q1(4), q2(4), qus(4), a, oldmo, p(4), qsmall(4), theta, vec(3)
type(dicttype)                          :: dict

character(fnlen),allocatable            :: stringarray(:)
real(kind=sgl),allocatable              :: Eulers(:,:), dplist(:,:), Eulerstmp(:,:), dplisttmp(:,:), avEuler(:,:), & 
                                           resultmain(:,:), disor(:), disorient(:,:), OSMmap(:,:), IQmap(:), ADMap(:,:)
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
! read the relevant fields from the EBSD dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

if (enl%refined.eq.'n') then 
  call readEBSDDotProductFile(enl%dotproductfile, ebsdnl, hdferr, EBSDDIdata, &
                              getOSM=.TRUE., & 
                              getIQ=.TRUE., & 
                              getEulerAngles=.TRUE., &
                              getTopDotProductList=.TRUE., &
                              getTopMatchIndices=.TRUE.) 
else
  call readEBSDDotProductFile(enl%dotproductfile, ebsdnl, hdferr, EBSDDIdata, &
                              getOSM=.TRUE., & 
                              getIQ=.TRUE., & 
                              getRefinedEulerAngles=.TRUE.)
end if 

Nexp = EBSDDIdata%Nexp
FZcnt = EBSDDIdata%FZcnt
dims2(1) = ebsdnl%nnk
if (enl%refined.eq.'n') then
  allocate(Eulers(3,FZcnt))
  do i=1,FZcnt
    Eulers(1:3,i) = EBSDDIdata%EulerAngles(1:3,i)
  end do
  deallocate(EBSDDIdata%EulerAngles)
  Eulers = Eulers * sngl(cPi)/180.0


  allocate(dplist(dims2(1),Nexp))
  do i=1,Nexp
    dplist(1:dims2(1),i) = EBSDDIdata%TopDotProductList(1:dims2(1),i)
  end do
  deallocate(EBSDDIdata%TopDotProductList)
  nnm = dims2(1)

  allocate(tmi(nnm,Nexp))
  do i=1,Nexp
    tmi(1:nnm,i) = EBSDDIdata%TopMatchIndices(1:nnm,i)
  end do
  deallocate(EBSDDIdata%TopMatchIndices)
else
  allocate(Eulers(3,Nexp))
  do i=1,Nexp
    Eulers(1:3,i) = EBSDDIdata%RefinedEulerAngles(1:3,i)
  end do
  deallocate(EBSDDIdata%RefinedEulerAngles)
end if

! the following arrays are kept in the EBSDDIdata structure
!   OSMmap = EBSDDIdata%OSM
!   IQmap = EBSDDIdata%IQ

call Message('  --> dot product EBSD HDF5 file read')

if (enl%refined.eq.'n') then
!===================================
! we will also need some of the crystallographic data, so that requires
! extracting the xtalname from the energyfile
write (*,*) 'energyfile = ', trim(ebsdnl%energyfile)
  efile = trim(EMsoft_getEMdatapathname())//trim(ebsdnl%energyfile)
  efile = EMsoft_toNativePath(efile)
write (*,*) 'efile = ', trim(efile)

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

write (*,*) 'xtalname  = ', ebsdnl%MCxtalname

! dataset = SC_EkeV
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%EkeV)

! dataset = SC_sig
!   call HDF_readDatasetDouble(dataset, HDF_head, hdferr, ebsdnl%MCsig)

    call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
    call h5close_EMsoft(hdferr)
  end if

! do the actual orientational averaging
  allocate(avEuler(3,Nexp),disorient(Nexp,enl%nmuse))
  ipar2(1) = EBSDDIdata%pgnum
  ipar2(2) = FZcnt
  ipar2(3) = Nexp
  ipar2(4) = ebsdnl%nnk
  ipar2(5) = Nexp*ceiling(float(ebsdnl%ipf_wd*ebsdnl%ipf_ht)/float(Nexp))
  ipar2(6) = enl%nmuse
  call EBSDgetAverageOrientations(ipar2, Eulers, tmi, dplist, avEuler, disorient)

  ! ok, so we have our list; next we need to store this in a ctf file, along with 
  ! all the other crystallographic information, so we need to fill in the proper 
  ! fields in the ebsdnl structure and ipar array.
  ipar = 0
  ipar(1) = 1
  ipar(2) = Nexp
  ipar(3) = Nexp
  ipar(4) = Nexp
  ipar(5) = FZcnt
  ipar(6) = EBSDDIdata%pgnum
  ipar(7) = ebsdnl%ipf_wd
  ipar(8) = ebsdnl%ipf_ht
  ebsdnl%ctffile = enl%averagectffile

  allocate(indexmain(1,Nexp), resultmain(1,Nexp))
  indexmain = 0
  resultmain(1,1:Nexp) = dplist(1,1:Nexp)
  noindex = .TRUE.
  call h5open_EMsoft(hdferr)
  call ctfebsd_writeFile(ebsdnl,ebsdnl%MCxtalname,ipar,indexmain,avEuler,resultmain,EBSDDIdata%OSM,EBSDDIdata%IQ,noindex)
  call h5close_EMsoft(hdferr)
  call Message('Data stored in ctf file : '//trim(enl%averagectffile))
end if 

! do we need to produce a relative disorientation map ?
if (trim(enl%disorientationmap).ne.'undefined') then
!===================================
! set up the symmetry quaternions for this rotational symmetry
! allocate the dict structure
  dict%Num_of_init = 3
  dict%Num_of_iterations = 30
  dict%pgnum = EBSDDIdata%pgnum
! initialize the symmetry matrices
  call DI_Init(dict,'nil') 

  if (enl%refined.eq.'n') then 
! put the Euler angles back in radians
    avEuler = avEuler * sngl(cPi)/180.0
! get the 1D point coordinate
    ipat = ebsdnl%ipf_wd * enl%reldisy + enl%reldisx
    allocate(disor(Nexp))
! and get the disorientations w.r.t. all the others
    do j=1,Nexp   
      call getDisorientationAngle(avEuler(1:3,ipat),avEuler(1:3,j),dict,oldmo)
      disor(j) = oldmo
    end do
    disor = disor*180.0/sngl(cPi)
else
! get the 1D point coordinate
    ipat = ebsdnl%ipf_wd * enl%reldisy + enl%reldisx
    allocate(disor(Nexp))
! and get the disorientations w.r.t. all the others
    do j=1,Nexp   
      call getDisorientationAngle(Eulers(1:3,ipat),Eulers(1:3,j),dict,oldmo)
      disor(j) = oldmo
    end do
    disor = disor*180.0/sngl(cPi)

! next, we compute the average disorientation map (ADM) by computing the disorientations between each 
! point and its four neighbors and averaging them.
    ! allocate (ADMap(ebsdnl%ipf_wd, ebsdnl%ipf_ht))
    ! call getAverageDisorientationMap(Eulers, dict, ebsdnl%ipf_wd, ebsdnl%ipf_ht, ADMap)
    ! open(unit=dataunit,file='ADMap.data',status='unknown',form='unformatted')
    ! write (dataunit) ebsdnl%ipf_wd, ebsdnl%ipf_ht
    ! write(dataunit) ADMap
    ! close(unit=dataunit,status='keep')
end if

! for now, we export the data in a simple text file; this needs to be modified in the future.
  open(unit=dataunit,file=trim(EMsoft_getEMdatapathname())//trim(enl%disorientationmap),status='unknown',form='formatted')
  write (dataunit,"(3I10)") Nexp, ebsdnl%ipf_wd, ebsdnl%ipf_ht
  do i=1,Nexp
    write(dataunit,"(F13.6)") disor(i)
  end do
  close(unit=dataunit,status='keep')
end if

end program EMAverageOrient
