! ###################################################################
! Copyright (c) 2015-2018, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMEBSDDIpreview.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMEBSDDIpreview
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Simple program to preview what the filtering parameters will result in 
!
!> @date 01/24/18 MDG 1.0 original, 
!--------------------------------------------------------------------------

program EMEBSDDIpreview

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io
use error

IMPLICIT NONE

character(fnlen)                            :: nmldeffile, progname, progdesc
type(EBSDDIpreviewNameListType)             :: enl
logical                                     :: verbose
integer(kind=irg)                           :: res

nmldeffile = 'EMEBSDDIpreview.nml'
progname = 'EMEBSDDIpreview.f90'
progdesc = 'Program to preview which EBSD pattern filtering parameters work best'
verbose = .TRUE.

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 47 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMEBSDDIpreview','JSON input not yet implemented')
!  call JSONreadTKDIndexingNameList(tkdnl, nmldeffile, error_cnt)
else
  call GetEBSDDIpreviewNameList(nmldeffile,enl)
end if

call MasterSubroutine(enl, progname, nmldeffile)

end program EMEBSDDIpreview

!--------------------------------------------------------------------------
!
! SUBROUTINE:MasterSubroutine
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Master subroutine to generate tiff file with matrix of pre-processed patterns
!
!> @param enl namelist pointer
!> @param progname name of the program
!> @param nmldeffile namelist filename
!
!> @date 01/24/18 MDG 1.0 original
!> @date 02/19/18 MDG 1.1 modified input to new patternmod module options
!--------------------------------------------------------------------------

subroutine MasterSubroutine(enl, progname, nmldeffile)

use local
use error
use TIFF_f90
use io
use filters
use patternmod
use NameListTypedefs
use HDF5

IMPLICIT NONE

type(EBSDDIpreviewNameListType),INTENT(INOUT)       :: enl
character(fnlen),INTENT(IN)                         :: progname
character(fnlen),INTENT(IN)                         :: nmldeffile

character(fnlen)                                    :: ename
integer(kind=irg)                                   :: iunitexpt, recordsize, ierr, kk, ii, jj, i, j, numr, numw, binx, biny, &
                                                       xoffset, yoffset, io_int(2), istat, L, patsz , hdferr
integer(HSIZE_T)                                    :: dims3(3), offset3(3)
logical                                             :: f_exists
real(kind=sgl)                                      :: mi, ma, io_real(1)
real(kind=dbl)                                      :: x, y, val, v2
real(kind=sgl),allocatable                          :: expt(:), pattern(:,:), pcopy(:,:), hpvals(:)
integer(kind=irg),allocatable                       :: nrvals(:), pint(:,:), ppp(:,:)
type(C_PTR)                                         :: HPplanf, HPplanb
complex(kind=dbl),allocatable                       :: hpmask(:,:)
complex(C_DOUBLE_COMPLEX),allocatable               :: inp(:,:), outp(:,:)
real(kind=dbl),allocatable                          :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)

call h5open_EMsoft(hdferr)

binx = enl%numsx
biny = enl%numsy
L = binx * biny
recordsize = 4 * L
patsz = L

! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a 
! pattern file produced by EMEBSD.f90; or a vendor binary or HDF5 file... in each case we need to 
! open the file and leave it open, then use the getSingleExpPattern() routine to read a 
! pattern into the expt variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
istat = openExpPatternFile(enl%exptfile, enl%ipf_wd, L, enl%inputtype, recordsize, iunitexpt, enl%HDFstrings)
if (istat.ne.0) then
    call patternmod_errormessage(istat)
    call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
end if

allocate(expt(patsz))
dims3 = (/ binx, biny, 1 /)
offset3 = (/ 0, 0, enl%paty * enl%ipf_wd + enl%patx /)
call getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, enl%inputtype, enl%HDFstrings, expt)

! and close the pattern file
call closeExpPatternFile(enl%inputtype, iunitexpt)

! turn it into a 2D pattern
allocate(pattern(binx, biny), pcopy(binx, biny), pint(binx,biny), ppp(binx,biny), stat=ierr)
do kk=1,biny
  pcopy(1:binx,kk) = expt((kk-1)*binx+1:kk*binx)
end do

! define the high-pass filter width array and the nregions array
numr = (enl%nregionsmax - enl%nregionsmin) / enl%nregionsstepsize + 1
allocate(nrvals(numr))
nrvals = enl%nregionsmin + (/ ((i-1)*enl%nregionsstepsize, i=1,numr) /)

! the array for the hi pass filter parameter is a non-linear progression
numw = enl%hipasswnsteps
allocate(hpvals(numw))
do ii=1,numw
    hpvals(ii) = 2.0**(float(ii-1-numw)) * 2.0 * enl%hipasswmax
end do

! allocate a byte array for the final output TIFF image that will contain all individual images
TIFF_nx = numw * binx
TIFF_ny = numr * biny
allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))
TIFF_filename = trim(EMsoft_getEMdatapathname())//trim(enl%tifffile)
TIFF_filename = EMsoft_toNativePath(TIFF_filename)

! next we need to set up the high-pass filter fftw plans
allocate(hpmask(binx,biny),inp(binx,biny),outp(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate hpmask, inp, outp arrays'
allocate(rrdata(binx,biny),ffdata(binx,biny),stat=istat)
if (istat .ne. 0) stop 'could not allocate rrdata, ffdata arrays'
call init_HiPassFilter(dble(hpvals(1)), (/enl%numsx, enl%numsy /), hpmask, inp, outp, HPplanf, HPplanb) 

! the outer loop goes over the hipass filter width and is displayed horizontally in the final image
do ii=1,numw
! Hi-Pass filter
    pattern = pcopy
    rrdata = dble(pattern)
    ffdata = applyHiPassFilter(rrdata, (/ binx, biny /), dble(hpvals(ii)), hpmask, inp, outp, HPplanf, HPplanb)
    pattern = sngl(ffdata)

    ma = maxval(pattern)
    mi = minval(pattern)

    pint = nint(((pattern - mi) / (ma-mi))*255.0)
    xoffset = (ii-1) * binx 
    do jj=1,numr
! adaptive histogram equalization
        if (nrvals(jj).eq.0) then
            ppp = pint
        else
            ppp = adhisteq(nrvals(jj),binx,biny,pint)
        end if 

! and store the pattern in the correct spot in the TIFF_image array
        yoffset =  (jj-1) * biny 
        TIFF_image(xoffset:xoffset+binx-1, yoffset:yoffset+biny-1) = ppp(1:binx, 1:biny)
    end do

! regenerate the complex inverted Gaussian mask with the next value of the mask width
    hpmask = cmplx(1.D0,0.D0)
    do i=1,binx/2 
      x = dble(i)**2
      do j=1,biny/2
        y = dble(j)**2
        v2 = hpvals(ii) * ( x+y )
        if (v2.lt.30.D0) then
          val = 1.D0-dexp(-v2)
          hpmask(i,j) = cmplx(val, 0.D0)
          hpmask(binx+1-i,j) = cmplx(val, 0.D0)
          hpmask(i,biny+1-j) = cmplx(val, 0.D0)
          hpmask(binx+1-i,biny+1-j) = cmplx(val, 0.D0)
        end if
      end do
    end do
end do

call TIFF_Write_File
call Message(' --> final images written to output file '//trim(TIFF_filename))
io_int(1) = TIFF_nx
io_int(2) = TIFF_ny
call WriteValue(' Tiff image dimensions : ',io_int,2)

call Message('')
call Message(' High-pass filter parameter values along horizontal axis (L to R) :')
do ii=1,numw
    io_real(1) = hpvals(ii)
    call WriteValue('',io_real,1,"(F10.6)")
end do

call Message('')
call Message(' nregions values along vertical axis (B to T):')
do ii=1,numr
    io_int(1) = nrvals(ii)
    call WriteValue('',io_int,1)
end do

call h5close_EMsoft(hdferr)

end subroutine MasterSubroutine
