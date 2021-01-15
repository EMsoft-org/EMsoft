! ###################################################################
! Copyright (c) 2015-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMgetCTF.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMgetCTF
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Extract a .ctf file from a dot product HDF5 file; uses the same template as EMFitOrientation
!
!> @date 03/07/18 MDG 1.0 original
!> @date 03/12/18 MDG 1.1 replaced dot product file reading with call to subroutine
!> @date 07/19/18 MDG 1.2 add option to extract refined Euler angles instead of regular array (for now only for EBSD modality)
!--------------------------------------------------------------------------
program EMgetCTF

use local
use typedefs 
use NameListTypedefs
use NameListHandlers
use initializersHDF
use HDF5
use h5im
use h5lt
use EMh5ebsd
use HDFsupport
use constants
use EBSDmod
use ECPiomod
use EBSDDImod
use ECPmod
use filters
use timing
use error
use io
use EBSDiomod
use files
use FitOrientations
use stringconstants
use patternmod

use ISO_C_BINDING

IMPLICIT NONE

character(fnlen)                        :: nmldeffile, progname, progdesc
type(CTFNameListType)                   :: enl
type(EBSDIndexingNameListType)          :: dinl
type(ECPIndexingNameListType)           :: ecpnl
type(EBSDDIdataType)                    :: EBSDDIdata
type(ECPDIdataType)                     :: ECPDIdata
type(MCCLNameListType)                  :: mcnl
type(EBSDMCdataType)                    :: EBSDMCdata


logical                                 :: stat, readonly, noindex, g_exists
character(fnlen)                        :: dpfile, masterfile, energyfile
integer(kind=irg)                       :: hdferr, ii, jj, kk, iii, istat

real(kind=sgl),allocatable              :: euler_best(:,:), CIlist(:)
integer(kind=irg),allocatable           :: indexmain(:,:) 
real(kind=sgl),allocatable              :: resultmain(:,:)                                         
integer(HSIZE_T)                        :: dims(1) 

integer(kind=irg)                       :: numk, numdictsingle, numexptsingle

character(fnlen, KIND=c_char),allocatable,TARGET    :: stringarray(:)
character(fnlen)                        :: dataset, groupname  
character(fnlen)                        :: ename, fname    

logical                                 :: verbose

logical                                 :: f_exists, init, overwrite =.TRUE., refined
real(kind=sgl)                          :: quat(4), ma, mi, dp, tstart, tstop, io_real(1), tmp, totnum_el, genfloat, vlen
integer(kind=irg)                       :: ipar(10), Emin, Emax, nthreads, TID, io_int(2), tick, tock, ierr, L 
integer(kind=irg)                       :: ll, mm, jpar(7), Nexp, pgnum, FZcnt, nlines, dims2(2)
real(kind=dbl)                          :: prefactor, F

character(fnlen)                        :: modalityname


init = .TRUE.

nmldeffile = 'EMgetCTF.nml'
progname = 'EMgetCTF.f90'
progdesc = 'Extract a regular or refined .ctf file from a dot product HDF5 file'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile, 1, (/ 18 /), progname)

! deal with the namelist stuff
call GetCTFNameList(nmldeffile,enl)

modalityname = trim(enl%modality)

!====================================
! read the relevant fields from the dot product HDF5 file

! open the fortran HDF interface
call h5open_EMsoft(hdferr)

if (trim(modalityname) .eq. 'EBSD') then
    refined = .FALSE.
    if (trim(enl%angledataset).eq.'refined') then 
        call readEBSDDotProductFile(enl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                                    getCI=.TRUE., &
                                    getIQ=.TRUE., & 
                                    getOSM=.TRUE., & 
                                    getRefinedEulerAngles=.TRUE., &
                                    getPhi1=.TRUE., &
                                    getPhi=.TRUE., &
                                    getPhi2=.TRUE.) 

    else
        call readEBSDDotProductFile(enl%dotproductfile, dinl, hdferr, EBSDDIdata, &
                                getCI=.TRUE., &
                                getIQ=.TRUE., & 
                                getOSM=.TRUE., & 
                                getPhi1=.TRUE., &
                                getPhi=.TRUE., &
                                getPhi2=.TRUE.) 
    end if

    Nexp = EBSDDIdata%Nexp
    allocate(euler_best(3,Nexp),CIlist(Nexp),stat=istat)
    if (istat .ne. 0) then
        dpfile = 'Failed to allocate CIlist_new and/or euler_bestmatch array'
        call FatalError('EMAverageOrient',dpfile)
    end if 
    euler_best = 0.0
    CIlist = 0.0
    if (refined.eqv..FALSE.) then
        euler_best(1,1:Nexp) = EBSDDIdata%Phi1(1:Nexp)*180.0/cPi
        euler_best(2,1:Nexp) = EBSDDIdata%Phi(1:Nexp)*180.0/cPi
        euler_best(3,1:Nexp) = EBSDDIdata%Phi2(1:Nexp)*180.0/cPi
        deallocate(EBSDDIdata%Phi1,EBSDDIdata%Phi,EBSDDIdata%Phi2)
        call Message(' Using original Euler angles from dot product file')
    else
        euler_best(1,1:Nexp) = EBSDDIdata%RefinedEulerAngles(1,1:Nexp)*180.0/cPi
        euler_best(2,1:Nexp) = EBSDDIdata%RefinedEulerAngles(2,1:Nexp)*180.0/cPi
        euler_best(3,1:Nexp) = EBSDDIdata%RefinedEulerAngles(3,1:Nexp)*180.0/cPi
        deallocate(EBSDDIdata%RefinedEulerAngles)
        call Message(' Using refined Euler angles from dot product file')
    end if 
    CIlist(1:Nexp) = EBSDDIdata%CI(1:Nexp)
    deallocate(EBSDDIdata%CI)

! the following arrays are kept in the EBSDDIdata structure
!   OSMmap = EBSDDIdata%OSM
!   IQmap = EBSDDIdata%IQ

    call Message('  --> dot product EBSD HDF5 file read')

else if (trim(modalityname) .eq. 'ECP') then
! fill ecpnl namelist structure here
    call readECPDotProductFile(enl%dotproductfile, ecpnl, hdferr, ECPDIdata, &
                               getCI=.TRUE., &
                               getPhi1=.TRUE., &
                               getPhi=.TRUE., &
                               getPhi2=.TRUE.) 

    Nexp = ECPDIdata%Nexp
    allocate(euler_best(3,Nexp),CIlist(Nexp),stat=istat)
    if (istat .ne. 0) then
        dpfile = 'Failed to allocate CIlist_new and/or euler_bestmatch array'
        call FatalError('EMAverageOrient',dpfile)
    end if 
    euler_best = 0.0
    CIlist = 0.0
    euler_best(1,1:Nexp) = ECPDIdata%Phi1(1:Nexp)*180.0/cPi
    euler_best(2,1:Nexp) = ECPDIdata%Phi(1:Nexp)*180.0/cPi
    euler_best(3,1:Nexp) = ECPDIdata%Phi2(1:Nexp)*180.0/cPi
    deallocate(ECPDIdata%Phi1,ECPDIdata%Phi,ECPDIdata%Phi2)
    CIlist(1:Nexp) = ECPDIdata%CI(1:Nexp)
    deallocate(ECPDIdata%CI)

    call Message('  --> dot product ECP HDF5 file read')

else
    dpfile = 'File '//trim(dpfile)//' is not an HDF5 file'
    call FatalError('EMgetCTF:',dpfile)
end if

write (*,*) 'crystal structure file name = ', trim(enl%xtalname)

    pgnum = GetPointGroup(enl%xtalname,NoHDFInterfaceOpen=.FALSE.)

! and prepare the .ctf output file 
if(modalityname .eq. 'EBSD') then
    dinl%ctffile = enl%newctffile
    
    ipar = 0
    ipar(1) = 1
    ipar(2) = Nexp
    ipar(3) = Nexp
    ipar(4) = Nexp
    ipar(5) = FZcnt
    ipar(6) = pgnum
    if (sum(dinl%ROI).ne.0) then
        ipar(7) = dinl%ROI(3)
        ipar(8) = dinl%ROI(4)
    else
        ipar(7) = dinl%ipf_wd
        ipar(8) = dinl%ipf_ht
    end if

    allocate(indexmain(ipar(1),1:ipar(2)),resultmain(ipar(1),1:ipar(2)))
    indexmain = 0
    resultmain(1,1:ipar(2)) = CIlist(1:Nexp)

    if (dinl%ctffile.ne.'undefined') then 
      call ctfebsd_writeFile(dinl,enl%xtalname,ipar,indexmain,euler_best,resultmain,EBSDDIdata%OSM, &
                             EBSDDIdata%IQ,noindex=.TRUE.)
      call Message('Data stored in ctf file : '//trim(enl%newctffile))
    end if
else if(modalityname .eq. 'ECP') then

    ecpnl%ctffile = enl%newctffile

    ipar = 0
    ipar(1) = 1
    ipar(2) = Nexp
    ipar(3) = Nexp
    ipar(4) = Nexp
    ipar(5) = FZcnt
    ipar(6) = pgnum

    allocate(indexmain(ipar(1),1:ipar(2)),resultmain(ipar(1),1:ipar(2)))
    indexmain = 0
    resultmain(1,1:ipar(2)) = CIlist(1:Nexp)

    if (ecpnl%ctffile.ne.'undefined') then 
      call ctfecp_writeFile(ecpnl,ipar,indexmain,euler_best,resultmain,noindex=.TRUE.)
      call Message('Data stored in ctf file : '//trim(enl%newctffile))
    end if

end if

! close the fortran HDF interface
call h5close_EMsoft(hdferr)

end program EMgetCTF


