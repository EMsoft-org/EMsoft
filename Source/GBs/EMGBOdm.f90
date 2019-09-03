! ###################################################################
! Copyright (c) 2014-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMGBOdm.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMGBOdm
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a GBOM distance matrix for a set of N octonions
!
!> @date 08/25/19 MDG 1.0 original 
!--------------------------------------------------------------------------
program EMGBOdm

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use dictmod
use io
use files
use rotations
use Lambert
use rng
use omp_lib
use constants
use quaternions
use GBmod
use error

IMPLICIT NONE

character(fnlen)                  :: nmldeffile, progname, progdesc
type(GBOdmNameListType)           :: gbonl

integer(kind=irg)                 :: numoct, numd, TID, i, j, io_int(2), ival, ic, ir 
real(kind=dbl),allocatable        :: octarray(:,:), distancematrix(:,:)
real(kind=dbl)                    :: qa(4) ,qb(4), qc(4), qd(4), is2, io_real(8), tt
type(dicttype),pointer            :: dict
character(fnlen)                  :: fname
logical                           :: f_exists


nmldeffile = 'EMGBOdm.nml'
progname = 'EMGBOdm.f90'
progdesc = 'Generate a distance matrix for a set of octonion grain boundary pairs'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 270 /), progname)

! deal with the namelist stuff
call GetGBOdmNameList(nmldeffile,gbonl)

!============================================================
! read the input file with N octonions 
fname = trim(EMsoft_getEMdatapathname())//trim(gbonl%inname)
fname = EMsoft_toNativePath(fname)

! does this file exist ?
inquire(file=trim(fname), exist=f_exists)
if (.not.f_exists) then 
  call FatalError('EMGBOdm','input octonion file does not exist')
end if

! get the number of octonions and allocate the array
open(dataunit,file=trim(fname),status='old',form='formatted')
read(dataunit,*) numoct

allocate( octarray(8,numoct) )

do i=1,numoct
  read(dataunit,*) octarray(1:8,i)
end do
close(dataunit,status='keep')

!============================================================
! allocate the necessary symmetry operators in quaternion form
nullify(dict)
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = gbonl%pgnum
call DI_Init(dict,'nil') 

io_int(1) = dict%Nqsym 

call WriteValue('Number of symmetry elements to consider : ', io_int,1)
call Message('Symmetry quaternions :')
do i=1,dict%Nqsym
  io_real(1:4) = dict%PM(1:4,i)
  call WriteValue('',io_real,4)
end do

!============================================================
! get the number of entries in the upper diagonal part of the 
! distance matrix which we will store as a 1D array of consecutive rows
numd = numoct * (numoct + 1) / 2
allocate( distancematrix(numoct,numoct) )
distancematrix = 0.D0

if (gbonl%nthreads.eq.0) then 
  call OMP_SET_NUM_THREADS(OMP_GET_MAX_THREADS())
  io_int(1) = OMP_GET_MAX_THREADS()
  call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")
else
  call OMP_SET_NUM_THREADS(gbonl%nthreads)
  io_int(1) = gbonl%nthreads
  call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")
end if 

!============================================================
! compute the pairwise distance matrix using parallel threads
call Message(' Computing distance matrix ',"(/A/)")

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, qa, qb, qc, qd, ic, tt, io_int)
TID = OMP_GET_THREAD_NUM()

! generate nquats random quartets of quaternions
!$OMP DO SCHEDULE(DYNAMIC)
do ir = 1,numoct
!$OMP CRITICAL
  io_int = (/ ir, numoct /) 
  call WriteValue(' Starting row ',io_int,2, "(I4,' of ',I4)")
!$OMP END CRITICAL
  qa = octarray(1:4,ir)
  qb = octarray(5:8,ir)
! make sure they are unit quaternions
  qa = qa / NORM2(qa)
  qb = qb / NORM2(qb)
  do ic = ir,numoct
    if (ic.ne.ir) then 
      qc = octarray(1:4,ic)
      qd = octarray(5:8,ic)
! make sure they are unit quaternions
      qc = qc / NORM2(qc)
      qd = qd / NORM2(qd)
      if (trim(gbonl%metric).eq.'octonion') then 
        tt = GBO_Omega_symmetric(qa,qb,qc,qd,dict)
      end if
      if (trim(gbonl%metric).eq.'Olmsted') then 
        tt = GBO_Olmsted_symmetric(qa,qb,qc,qd,dict)
      end if
      ! if (trim(gbonl%metric).eq.'Morawiec') then 
      !   tt = GBO_Morawiec_symmetric(qa,qb,qc,qd,dict)
      ! end if
      distancematrix(ir,ic) = tt
    end if
  end do
end do 
!$OMP END DO 
!$OMP END PARALLEL 

fname = trim(EMsoft_getEMdatapathname())//trim(gbonl%outname)
fname = EMsoft_toNativePath(fname)

open(dataunit,file=trim(fname),status='unknown',form='formatted')
! first write the dimensions of the matrix, then the total number of entries in the file
write(dataunit,"(I10,I10)") numoct, numd 

! then the distances row by row 
do ir=1,numoct
 do ic=ir,numoct
  write(dataunit,"(F14.10)") distancematrix(ir,ic)
 end do
end do 
close(dataunit,status='keep')

call Message(' Distance matrix stored in '//trim(fname))


end program EMGBOdm
