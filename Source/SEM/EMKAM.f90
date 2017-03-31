! ###################################################################
! Copyright (c) 2013-2016, Marc De Graef/Carnegie Mellon University
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
! EMsoft:EMKAM.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMKAM
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a tiff file with an OSM map
!
!> @date 07/29/16 MDG 1.0 original
!--------------------------------------------------------------------------
program EMKAM

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
use TIFF_f90
use rotations


IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(KAMNameListType)                   :: enl
type(EBSDIndexingNameListType)          :: ebsdnl

logical                                 :: stat, readonly, noindex
integer(kind=irg)                       :: hdferr, nlines, FZcnt, Nexp, nnm, nnk, Pmdims, i, j, k, olabel, Nd, Ne, ipar(10), &
                                           ipar2(6), pgnum, ipat, ipf_wd, ipf_ht, idims2(2)
character(fnlen)                        :: groupname, dataset, dpfile, energyfile, masterfile, efile, fname
integer(HSIZE_T)                        :: dims2(2)
!type(dicttype),pointer                  :: dict
type(dicttype)                          :: dict
real(kind=sgl)                          :: testeu(3), eu1(3), eu2(3), ro1(4), ro2(4), s, da

character(fnlen),allocatable            :: stringarray(:)
integer(kind=irg),allocatable           :: tmi(:,:), tmitmp(:,:), indexmain(:,:)
real(kind=sgl),allocatable              :: kam(:,:), eulers(:,:), Eulerstmp(:,:), Eulervals(:,:), avEuler(:,:), &
                                           dplist(:,:), dplisttmp(:,:)

type(HDFobjectStackType),pointer        :: HDF_head

nullify(HDF_head)

nmldeffile = 'EMKAM.nml'
progname = 'EMKAM.f90'
progdesc = 'Generate a nearest-neighbor Kernel Average Misorientation (KAM) map'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 84 /), progname)

! deal with the namelist stuff
call GetKAMNameList(nmldeffile,enl)

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

! get the energyfile and masterfile parameters from NMLParameters
    groupname = 'NMLparameters'
    hdferr = HDF_openGroup(groupname, HDF_head)
    groupname = 'EBSDIndexingNameListType'
    hdferr = HDF_openGroup(groupname, HDF_head)

    dataset = 'energyfile'
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    energyfile = trim(stringarray(1))
    deallocate(stringarray)

    dataset = 'nnk'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, nnk)

    dataset = 'ipf_wd'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ipf_wd)
    dataset = 'ipf_ht'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, ipf_ht)

! and close the NMLparameters group
    call HDF_pop(HDF_head)
    call HDF_pop(HDF_head)


! open the Scan 1/EBSD/Data group
    groupname = 'Scan 1'
    hdferr = HDF_openGroup(groupname, HDF_head)
    groupname = 'EBSD'
    hdferr = HDF_openGroup(groupname, HDF_head)
    groupname = 'Data'
    hdferr = HDF_openGroup(groupname, HDF_head)

! integers
    dataset = 'FZcnt'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, FZcnt)

    dataset = 'NumExptPatterns'
    call HDF_readDatasetInteger(dataset, HDF_head, hdferr, Nexp)


    dataset = 'TopDotProductList'
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, dplisttmp)
    allocate(dplist(dims2(1),Nexp))
    do i=1,Nexp
      dplist(1:dims2(1),i) = dplisttmp(1:dims2(1),i)
    end do
    deallocate(dplisttmp)
    nnm = dims2(1)

    dataset = 'EulerAngles'
    call HDF_readDatasetFloatArray2D(dataset, dims2, HDF_head, hdferr, Eulerstmp)
    allocate(Eulervals(3,FZcnt))
    do i=1,FZcnt
      Eulervals(1:3,i) = Eulerstmp(1:3,i)
    end do
    deallocate(Eulerstmp)
    Eulervals = Eulervals * sngl(cPi)/180.0

! arrays
    dataset = 'TopMatchIndices'
    call HDF_readDatasetIntegerArray2D(dataset, dims2, HDF_head, hdferr, tmitmp)
    allocate(tmi(nnk,Nexp))
    do i=1,Nexp
      tmi(1:nnk,i) = tmitmp(1:nnk,i)
    end do
    idims2 = (/ nnk, Nexp /)
    deallocate(tmitmp)
  call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
  call h5close_EMsoft(hdferr)

  call Message('dot product HDF5 file read')
end if

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
    groupname = 'NMLparameters'
    hdferr = HDF_openGroup(groupname, HDF_head)
    groupname = 'MCCLNameList'
    hdferr = HDF_openGroup(groupname, HDF_head)

! read all the necessary variables from the namelist group
    dataset = 'xtalname'
    call HDF_readDatasetStringArray(dataset, nlines, HDF_head, hdferr, stringarray)
    ebsdnl%MCxtalname = trim(stringarray(1))
    deallocate(stringarray)

    call HDF_pop(HDF_head,.TRUE.)

! close the fortran HDF interface
    call h5close_EMsoft(hdferr)

    call Message('energy file read')
  else
    efile = 'File '//trim(efile)//' is not an HDF5 file'
    call FatalError('EMAverageOrient',efile)
  end if
  pgnum = GetPointGroup(ebsdnl%MCxtalname)

! and next we compute the KAM map (kam)
allocate(kam(ipf_wd,ipf_ht),eulers(3,Nexp))

! do we need to do an orientation average first ?
if (enl%orav.ne.0) then
  ipar2(1) = pgnum
  ipar2(2) = FZcnt
  ipar2(3) = Nexp
  ipar2(4) = nnk
  ipar2(5) = Nexp*ceiling(float(ipf_wd*ipf_ht)/float(Nexp))
  ipar2(6) = enl%orav
  call Message('Computing orientation averages ... ')
  call EBSDgetAverageOrientations(ipar2, Eulervals, tmi, dplist, eulers)
  eulers = eulers*sngl(cPi)/180.0
else
  do i=1,Nexp
    eulers(1:3,i) = Eulervals(1:3,tmi(1,i))
  end do
end if

! compute the Kernel Average Misorientation map
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = pgnum
call DI_Init(dict,'nil') 

call Message('Computing KAM map... ')
call EBSDgetKAMMap(Nexp, eulers, ipf_wd, ipf_ht, dict, kam)
kam = kam*180.0/sngl(cPi)

write (*,*) 'KAM range = ',minval(kam), maxval(kam)

where (kam.gt.enl%kamcutoff) kam = enl%kamcutoff
where (kam.lt.0.0) kam = 0.0
kam = kam/maxval(kam)
kam = kam*255.0

TIFF_filename = trim(EMsoft_getEMdatapathname())//trim(enl%kamtiff)
TIFF_filename = EMsoft_toNativePath(TIFF_filename)

TIFF_nx = ipf_wd
TIFF_ny = ipf_ht
! allocate memory for image
allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))
! fill the image with whatever data you have (between 0 and 255)
 do i=0,TIFF_nx-1
  do j=0,TIFF_ny-1
   TIFF_image(i,j) = kam(i+1,ipf_ht-j)
  end do
 end do
! create the file
 call TIFF_Write_File

call Message('KAM map written to '//trim(TIFF_filename))

end program EMKAM
