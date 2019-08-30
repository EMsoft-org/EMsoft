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
! EMsoft:EMOrientationSimilarity.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMOrientationSimilarity
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief create a tiff file with an OSM map
!
!> @date 07/29/16 MDG 1.0 original
!> @date 03/12/18 MDG 1.1 replaced reading of dot product file by new subroutine
!--------------------------------------------------------------------------
program EMOrientationSimilarity

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
use stringconstants
use commonmod


IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(OrientationSimilarityNameListType) :: enl
type(EBSDIndexingNameListType)          :: ebsdnl
type(EBSDDIdataType)                    :: EBSDDIdata

logical                                 :: stat, readonly, noindex
integer(kind=irg)                       :: hdferr, nlines, FZcnt, Nexp, nnm, nnk, Pmdims, i, j, k, olabel, Nd, Ne, ipar(10), &
                                           ipar2(6), pgnum, ipat, ipf_wd, ipf_ht, idims2(2)
character(fnlen)                        :: groupname, dataset, dpfile, energyfile, masterfile, efile, fname
integer(HSIZE_T)                        :: dims2(2)

integer(kind=irg),allocatable           :: tmi(:,:), tmitmp(:,:)
real(kind=sgl),allocatable              :: osm(:,:)

type(HDFobjectStackType),pointer        :: HDF_head

nullify(HDF_head%next)

nmldeffile = 'EMOrientationSimilarity.nml'
progname = 'EMOrientationSimilarity.f90'
progdesc = 'Generate an orientation similarity map'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 83 /), progname)

! deal with the namelist stuff
call GetOrientationSimilarityNameList(nmldeffile,enl)

!====================================
! read the relevant fields from the dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

call readEBSDDotProductFile(enl%dotproductfile, ebsdnl, hdferr, EBSDDIdata, &
                            getTopMatchIndices=.TRUE.) 

Nexp = EBSDDIdata%Nexp
FZcnt = EBSDDIdata%FZcnt

allocate(tmi(ebsdnl%nnk,Nexp))
do i=1,Nexp
  tmi(1:ebsdnl%nnk,i) = EBSDDIdata%TopMatchIndices(1:ebsdnl%nnk,i)
end do
idims2 = (/ ebsdnl%nnk, Nexp /)
deallocate(EBSDDIdata%TopMatchIndices)

! and next we compute the orientation similarity map (osm)

allocate(osm(ebsdnl%ipf_wd,ebsdnl%ipf_ht))
call EBSDgetOrientationSimilarityMap(idims2, tmi, enl%nmuse, ebsdnl%ipf_wd, ebsdnl%ipf_ht, osm)

! and write everything to a tiff file
osm = osm/maxval(osm)
osm = osm*255

TIFF_filename = trim(EMsoft_getEMdatapathname())//trim(enl%osmtiff)
TIFF_filename = EMsoft_toNativePath(TIFF_filename)

TIFF_nx = ebsdnl%ipf_wd
TIFF_ny = ebsdnl%ipf_ht
! allocate memory for image
allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))
! fill the image with whatever data you have (between 0 and 255)
 do i=0,TIFF_nx-1
  do j=0,TIFF_ny-1
   TIFF_image(i,j) = osm(i+1,ebsdnl%ipf_ht-j)
  end do
 end do
! create the file
 call TIFF_Write_File

call Message('OSM map written to '//trim(TIFF_filename))

end program EMOrientationSimilarity
