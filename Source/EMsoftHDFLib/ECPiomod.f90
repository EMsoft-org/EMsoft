! ###################################################################
! Copyright (c) 2013-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:ECPiomod.f90
!--------------------------------------------------------------------------
!
! MODULE: support routines for ECP output files in .ang (TSL) and .ctf (HKL) formats
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @date 3/16/17 SS 1.0 original
!--------------------------------------------------------------------------

module ECPiomod

use local
use stringconstants

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:ctfebsd_writeFile
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Write a *.ctf output file with EBSD data (HKL format)
!
!> @param ecpnl namelist
!> @param ipar  series of integer dimensions
!> @param indexmain indices into the main Euler array
!> @param eulerarray array of Euler angle triplets
!> @param resultmain dot product array
!
!> @date 02/07/15  SS 1.0 original
!> @date 03/10/16 MDG 1.1 moved from program to module and updated [TO BE COMPLETED]
!> @date 06/05/16 MDG 1.2 added reading of xtal file for correct crystallography output; corrected Euler angles for hkl convention
!> @date 06/05/16 MDG 1.3 added sampling step sizes
!> @date 06/25/16 MDG 1.4 added noindex optional keyword
!> @date 07/10/16 MDG 1.5 swapped Error, MAD, and BC columns
!--------------------------------------------------------------------------
recursive subroutine ctfecp_writeFile(ecpnl,ipar,indexmain,eulerarray,resultmain,noindex)
!DEC$ ATTRIBUTES DLLEXPORT :: ctfecp_writeFile

use NameListTypedefs
use HDF5
use HDFsupport
use typedefs
use error

IMPLICIT NONE

type(ECPIndexingNameListType),INTENT(INOUT)         :: ecpnl
!f2py intent(in,out) ::  ecpnl
integer(kind=irg),INTENT(IN)                        :: ipar(10)
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: eulerarray(3,ipar(4))
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
logical,INTENT(IN),OPTIONAL                         :: noindex

integer(kind=irg)                                   :: ierr, i, ii, indx, hdferr, SGnum, LaueGroup
character(fnlen)                                    :: ctfname
character                                           :: TAB = CHAR(9)
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10,filename,grname,dataset
real(kind=sgl)                                      :: euler(3)
logical                                             :: stat, readonly, donotuseindexarray
integer(HSIZE_T)                                    :: dims(1)
real(kind=dbl),allocatable                          :: cellparams(:)

type(HDFobjectStackType)                            :: HDF_head_local

donotuseindexarray = .FALSE.
if (present(noindex)) then
  if (noindex.eqv..TRUE.) then 
    donotuseindexarray = .TRUE.
  end if
end if

! open the file (overwrite old one if it exists)
ctfname = trim(EMsoft_getEMdatapathname())//trim(ecpnl%ctffile)
ctfname = EMsoft_toNativePath(ctfname)
open(unit=dataunit2,file=trim(ctfname),status='unknown',action='write',iostat=ierr)

write(dataunit2,'(A)') 'Channel Text File'
write(dataunit2,'(A)') 'Prj Test'
write(dataunit2,'(A)') 'Author	'//trim(EMsoft_getUsername())
write(dataunit2,'(A)') 'JobMode	Grid'
write(dataunit2,'(2A,I5)') 'XCells',TAB, ecpnl%totnumexpt
write(dataunit2,'(2A,I5)') 'YCells',TAB, 1
write(dataunit2,'(2A,F6.2)') 'XStep',TAB, 1.0
write(dataunit2,'(2A,F6.2)') 'YStep',TAB, 1.0
write(dataunit2,'(A)') 'AcqE1	0'
write(dataunit2,'(A)') 'AcqE2	0'
write(dataunit2,'(A)') 'AcqE3	0'
write(dataunit2,'(A,A)',ADVANCE='No') 'Euler angles refer to Sample Coordinate system (CS0)!',TAB
str1 = 'Mag	30	Coverage	100	Device	0	KV'
write(str2,'(F4.1)') ecpnl%EkeV
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAngle'
write(str2,'(F5.2)') ecpnl%sampletilt
str2 = adjustl(str2)
str1 = trim(str1)//TAB//trim(str2)//TAB//'TiltAxis	0'
write(dataunit2,'(A)') trim(str1)
!write(dataunit2,'(A)')'Mag	30	Coverage	100	Device	0	KV	288.9	TiltAngle	-1	TiltAxis	0'
write(dataunit2,'(A)') 'Phases	1'

! here we need to read the .xtal file and extract the lattice parameters, Laue group and space group numbers
! test to make sure the input file exists and is HDF5 format
filename = trim(EMsoft_getXtalpathname())//trim(ecpnl%MCxtalname)
filename = EMsoft_toNativePath(filename)

stat = .FALSE.

call h5fis_hdf5_f(filename, stat, hdferr)
nullify(HDF_head_local%next)


if (stat) then

! open the xtal file using the default properties.
  readonly = .TRUE.
  hdferr =  HDF_openFile(filename, HDF_head_local, readonly)

! open the namelist group
  grname = 'CrystalData'
  hdferr = HDF_openGroup(grname, HDF_head_local)

! get the spacegroupnumber
dataset = SC_SpaceGroupNumber
  call HDF_readDatasetInteger(dataset, HDF_head_local, hdferr, SGnum)

! get the lattice parameters
dataset = SC_LatticeParameters
  call HDF_readDatasetDoubleArray1D(dataset, dims, HDF_head_local, hdferr, cellparams) 

! and close the xtal file
  call HDF_pop(HDF_head_local,.TRUE.)
else
  call FatalError('ctfebsd_writeFile','Error reading xtal file '//trim(filename))
end if

! unit cell size
cellparams(1:3) = cellparams(1:3)*10.0  ! convert to Angstrom
write(str1,'(F8.3)') cellparams(1)
write(str2,'(F8.3)') cellparams(2)
write(str3,'(F8.3)') cellparams(3)
str1 = adjustl(str1)
str2 = adjustl(str2)
str3 = adjustl(str3)
str1 = trim(str1)//';'//trim(str2)//';'//trim(str3)

! unit cell angles
write(str4,'(F8.3)') cellparams(4)
write(str5,'(F8.3)') cellparams(5)
write(str6,'(F8.3)') cellparams(6)
str4 = adjustl(str5)
str5 = adjustl(str5)
str6 = adjustl(str6)
str1 = trim(str1)//TAB//trim(str4)//';'//trim(str5)//';'//trim(str6)

! structure name
str3 = ''
ii = len(trim(ecpnl%MCxtalname))-5
do i=1,ii
  str3(i:i) = ecpnl%MCxtalname(i:i)
end do
str1 = trim(str1)//TAB//trim(str3)

! rotational symmetry group
if (SGnum.ge.221) then
  i = 32
else
  i=1
  do while (SGPG(i).lt.SGnum) 
    i = i+1
  end do
end if
str4 = ''
LaueGroup = PGLaueinv(i)
write(str4,'(I2)') LaueGroup
str1 = trim(str1)//TAB//trim(adjustl(str4))

! space group
str2 = ''
write(str2,'(I3)') SGnum
str1 = trim(str1)//TAB//trim(adjustl(str2))

! and now collect them all into a single string
write(dataunit2,'(A)') str1

! write(dataunit2,'(A)'),'3.524;3.524;3.524	90;90;90	Nickel	11	225'

! this is the table header
write(dataunit2,'(A)') 'Phase	X	Y	Bands	Error	Euler1	Euler2	Euler3	MAD	BC	BS'

! go through the entire array and write one line per sampling point
do ii = 1,ipar(3)
    if (donotuseindexarray.eqv..TRUE.) then
      indx = 0
      euler = eulerarray(1:3,ii)
    else
      indx = indexmain(1,ii)
      euler = eulerarray(1:3,indx)
    end if
! changed order of coordinates to conform with ctf standard
    write(str2,'(F12.3)') float(ii-1)
    write(str1,'(F12.3)') 0.0
    write(str3,'(I2)') 10
    write(str8,'(I8)') 0 ! integer zero error; was indx, which is now moved to BC
    write(str5,'(F12.3)') euler(1) - 90.0  ! conversion from TSL to Oxford convention
    write(str6,'(F12.3)') euler(2)
! intercept the hexagonal case, for which we need to subtract 30° from the third Euler angle
    if ((LaueGroup.eq.8).or.(LaueGroup.eq.9)) euler(3) = euler(3) - 30.0
    write(str7,'(F12.3)') euler(3)
    write(str4,'(F12.6)') resultmain(1,ii)   ! this replaces MAD
! the following two parameters need to be modified to contain more meaningful information
    write(str9,'(I8)') indx   ! index into the dictionary list
    write(str10,'(I8)') 255
! Oxford 3D files have four additional integer columns;
! GrainIndex
! GrainRandomColourR
! GrainRandomColourG
! GrainRandomColourB
!
    write(dataunit2,'(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)')'1',TAB,trim(adjustl(str1)),TAB,&
    trim(adjustl(str2)),TAB,trim(adjustl(str3)),TAB,trim(adjustl(str8)),TAB,trim(adjustl(str5)),&
    TAB,trim(adjustl(str6)),TAB,trim(adjustl(str7)),TAB,trim(adjustl(str4)),TAB,trim(adjustl(str9)),&
    TAB,trim(adjustl(str10))
end do

close(dataunit2,status='keep')

end subroutine ctfecp_writeFile

!--------------------------------------------------------------------------
!
! SUBROUTINE: h5ecp_writeFile
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write an .h5ecp file. There is no industry standard yet, so we need to make our 
!> own standard file. This format is STILL UNDER DEVELOPEMENT and will be CHANGED FREQUENTLY
! 
!> @param ecpnl      input name list
!> @param dstr       date string
!> @param tstrb      timestamp 
!> @param ipar       integer dimensions of various arrays
!> @param resultmain main result array
!> @param indexmain  main array with index of best nnk matches
!> @param eulerarray euler array of all RFZ sampled points 
!> @param progname   name of program
!> @param nmldeffile namelist file      
!
!> @date 03/16/17 SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine h5ecp_writeFile(ecpnl, dstr, tstrb, ipar, resultmain, &
                            indexmain, eulerarray, progname, nmldeffile)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ecp_writeFile

use NameListTypedefs
use io
use constants
use ecpmod
use HDF5
use HDFsupport
use ISO_C_BINDING

IMPLICIT NONE

type(ECPIndexingNameListType),INTENT(INOUT)         :: ecpnl
!f2py intent(in,out) ::  ecpnl
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
integer(kind=irg),INTENT(INOUT)                     :: ipar(10)
!f2py intent(in,out) ::  ipar
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(INOUT)                        :: eulerarray(3,ipar(4))
!f2py intent(in,out) ::  eulerarray
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

character(15)                                       :: tstre
character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
integer(kind=irg)                                   :: hdferr, filetype, i, ii, jj,indx, istat, ipar2(6), L
character(fnlen)                                    :: groupname, dataset, h5ecpfile, savefile
logical                                             :: noindex
type(dicttype)                                      :: dict


real(kind=sgl),allocatable                          :: exptCI(:), eangle(:), results(:), avEuler(:,:), &
                                                       lresultmain(:,:), eulers(:,:) 
integer(kind=1),allocatable                         :: iPhase(:), valid(:)
integer(kind=irg),allocatable                       :: SEMsignal(:), lindexmain(:,:)

type(HDFobjectStackType)                            :: HDF_head

!=============================================================
! write the output in the format of an h5ecp file
!!!! THIS PART IS STILL UNDER DEVELOPMENT !!!!
! NO STANDARD EXISTS, SO THIS CODE WILL BE CHANGED FREQUENTLY
!=============================================================

allocate(stringarray(1))

nullify(HDF_head%next)
call timestamp(timestring=tstre)

! Create a new file using the default properties.
h5ecpfile = trim(EMsoft_getEMdatapathname())//trim(ecpnl%datafile)
h5ecpfile = EMsoft_toNativePath(h5ecpfile)
hdferr =  HDF_createFile(h5ecpfile, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening file')

call h5ecp_writeInfo(dstr, tstrb, tstre, progname, ecpnl, nmldeffile, HDF_head)

! here we start with the h5ecp-specific stuff
groupname = 'Scan 1'
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group Scan 1')

groupname = SC_ECP
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group EBSD')

!=====================================================
!=====================================================
groupname = SC_Data
hdferr = HDF_createGroup(groupname, HDF_head)
if (hdferr.ne.0) call HDF_handleError(hdferr,'Error opening group Data')
!=====================================================
! CI Confidence Index: real(kind=sgl), one for each pattern... we take this
! to be the largest dot product
dataset = SC_CI
  allocate(exptCI(ipar(3)))
  exptCI = resultmain(1,1:ipar(3))
  hdferr = HDF_writeDatasetFloatArray1D(dataset, exptCI, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset CI')

!=====================================================
! Phase: Phase identifier (all zero for now)
dataset = SC_Phase
  allocate(iPhase(ipar(3)),stat=istat)
  iPhase = 0
  hdferr = HDF_writeDatasetInteger1byteArray1D(dataset, iPhase, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phase')
  deallocate(iPhase)

!=====================================================
! Euler angles: Phi 
dataset = SC_Phi
  allocate(eangle(ipar(3)),stat=istat)
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(2,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi')

!=====================================================
! Euler angles: Phi1
dataset = SC_Phi1
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(1,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi1')

!=====================================================
! Euler angles: Phi2 
dataset = SC_Phi2
  do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    eangle(ii) = eulerarray(3,indx)
  end do
  eangle = eangle * sngl(cPi)/180.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, eangle, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Phi2')
  deallocate(eangle)

! number of samples in dictionary
dataset = SC_FZcnt
  hdferr = HDF_writeDatasetInteger(dataset, ipar(5), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset FZcnt')

! point group number
dataset = SC_PointGroupNumber
  hdferr = HDF_writeDatasetInteger(dataset, ipar(6), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset PointGroupNumber')

! Ncubochoric
dataset = SC_Ncubochoric
  hdferr = HDF_writeDatasetInteger(dataset, ecpnl%ncubochoric, HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Ncubochoric')

! write the list of sampled Euler angles
dataset = SC_EulerAngles
  hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerarray, 3, ipar(4), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset EulerAngles')

! number of experimental patterns 
dataset = SC_NumExptPatterns
  hdferr = HDF_writeDatasetInteger(dataset, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset NumExptPatterns')

! list of top nnk dot product values
dataset = SC_TopDotProductList
  hdferr = HDF_writeDatasetFloatArray2D(dataset, resultmain, ipar(1), ipar(2), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopDotProductList')

! indices of top matches into Euler angle list
dataset = SC_TopMatchIndices
  hdferr = HDF_writeDatasetIntegerArray2D(dataset, indexmain, ipar(1), ipar(2), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset TopMatchIndices')

!=====================================================
! X Position: list of x positions for sampling points; requires knowledge of step size
! from Header
  dataset = 'X Position'
  allocate(results(ipar(3)),stat=istat)
  do jj=1,ipar(3)
      results(jj) = float(jj-1) 
  end do
  hdferr = HDF_writeDatasetFloatArray1D(dataset, results, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset X Position')

 
!=====================================================
! Y Position: list of y positions for sampling points; requires knowledge of step size
! from Header
  dataset = 'Y Position'
  results = 0.0
  hdferr = HDF_writeDatasetFloatArray1D(dataset, results, ipar(3), HDF_head)
  if (hdferr.ne.0) call HDF_handleError(hdferr,'Error writing dataset Y position')
  deallocate(results)


! leave this group
  call HDF_pop(HDF_head)


end subroutine h5ecp_writeFile

!--------------------------------------------------------------------------
!
! SUBROUTINE:h5ecp_writeInfo
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief write general information fields to the h5ecp file, including EMsoft specific fields
!
!> @param dstr date string
!> @param tstrb begin time string
!> @param tstre end time string
!> @param progname name of the calling program
!
!> @date 02/11/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine h5ecp_writeInfo(dstr, tstrb, tstre, progname, ecpnl, nmldeffile, HDF_head)
!DEC$ ATTRIBUTES DLLEXPORT :: h5ecp_writeInfo

use NameListTypedefs
use NameListHandlers
use NameListHDFwriters

IMPLICIT NONE

type(ECPIndexingNameListType),INTENT(INOUT)         :: ecpnl
!f2py intent(in,out) ::  ecpnl
character(11),INTENT(INOUT)                         :: dstr
!f2py intent(in,out) ::  dstr
character(15),INTENT(IN)                            :: tstrb
character(15),INTENT(IN)                            :: tstre
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile
type(HDFobjectStackType)                            :: HDF_head

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                                    :: groupname, dataset, nmlname, manufacturer
integer(kind=irg)                                   :: hdferr

manufacturer = 'EMECPDictionaryIndexing.f90'
nmlname = 'EMECPDictionaryIndexingNML'

allocate(stringarray(1))

! set the Manufacturer and Version data sets
dataset = SC_Manufacturer
stringarray(1)= trim(manufacturer)
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

dataset = SC_Version
stringarray(1)= 'EMsoft '//EMsoft_getEMsoftversion()
hdferr = HDF_writeDatasetStringArray(dataset, stringarray, 1, HDF_head)

! add the EMsoft header group
! write the EMheader to the file
groupname = SC_h5EBSD
call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname)

! create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
hdferr = HDF_createGroup(groupname, HDF_head)

! and write the nml file for this program to the HDF5 file
! read the text file and write the array to the file
dataset = trim(nmlname)
hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
groupname = SC_NMLparameters
hdferr = HDF_createGroup(groupname, HDF_head)
call HDFwriteECPDictionaryIndexingNameList(HDF_head, ecpnl)

! leave this group
call HDF_pop(HDF_head)

end subroutine h5ecp_writeInfo


end module ECPiomod
