! ###################################################################
! Copyright (c) 2014-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMGBO.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMGBO
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief generate text files with grain boundary octonion geodesic arc length histograms
!
!> @date 04/21/18 MDG 1.0 original 
!--------------------------------------------------------------------------
program EMGBO

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use dictmod
use io
use files
use error
use rotations
use Lambert
use rng
use omp_lib
use constants
use quaternions
use GBmod

IMPLICIT NONE

character(fnlen)                  :: nmldeffile, progname, progdesc
type(GBONameListType)             :: gbonl

integer(kind=irg)                 :: numb, ip, TID, i, j, io_int(2), CSLnumber, numoct
integer(kind=irg),allocatable     :: histogramSYM(:), histogramNBSYM(:)
type(rng_t)                       :: seed 
real(kind=dbl)                    :: aa(3),bb(3),cc(3),dd(3),qa(4),qb(4),qc(4),qd(4),pp,tt , acube, qq(4), &
                                     x1,x2,y1,y2,s1,s2, eu1(3), eu2(3), scale, Sqa(4), Sqc(4), Sqb(4), Sqd(4), &
                                     oac, obd
real(kind=dbl),allocatable        :: octarray(:,:)
type(dicttype),pointer            :: dict
character(fnlen)                  :: fname 
character(4)                      :: mode
logical                           :: f_exists  

nmldeffile = 'EMGBO.nml'
progname = 'EMGBO.f90'
progdesc = 'Generate geodesic angle distributions for grain boundary pairs'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 98 /), progname)

! deal with the namelist stuff
call GetGBONameList(nmldeffile,gbonl)

! what is the octonion generator mode?  'random' or a user-provided text file
if (trim(gbonl%octonions).eq.'random') then 
  mode = 'rand'
  numoct = gbonl%numsamples
  call Message(' Octonion mode:  random generation based on cubochoric sampling')
else
  mode = 'file'
  fname = trim(EMsoft_getEMdatapathname())//trim(gbonl%octonions)
  fname = EMsoft_toNativePath(fname)
! does this file exist ?
  inquire(file=trim(fname), exist=f_exists)
  if (.not.f_exists) then 
    call FatalError('EMGBO','input octonion file does not exist')
  end if
  call Message(' Octonion mode:  reading octonion pairs from '//trim(fname))
  open(unit=dataunit,file=trim(fname),status='old',form='formatted')
  read(dataunit,*) numoct
  allocate(octarray(8,2*numoct))
  do i=1,2*numoct
    read(dataunit,*) octarray(1:8,i)
  end do
  close(dataunit,status='keep')
  if (trim(gbonl%CSLtype).ne.'') numoct = 2*numoct
end if

! set the number of bins for the interval [0,180]
numb = gbonl%numbins
scale = (180.D0/cPi) * (float(numb)/180.D0)

! generate the histograms
allocate(histogramSYM(0:numb), histogramNBSYM(0:numb))
histogramSYM = 0
histogramNBSYM = 0

acube = 0.5D0 * cPi**0.666666666

nullify(dict)
allocate(dict)
dict%Num_of_init = 3
dict%Num_of_iterations = 30
dict%pgnum = gbonl%pgnum

! initialize the symmetry matrices and print some information
call DI_Init(dict,'nil') 

write (*,*) 'Number of symmetry elements to consider : ', dict%Nqsym
write (*,*) 'Symmetry quaternions:'
do i=1,dict%Nqsym
  write (*,*) i, dict%PM(1:4,i)
end do

if (trim(gbonl%CSLtype).ne.'') then
    call Message('Available CSL Boundary quaternions : ')
    do i=1,CSLnumberdefined
      qb = ro2qu( GB_getCSLrod( CSLlabels(i) , CSLnumber) )
      write (*,"(I3,'  ',A3,'  ',4F10.6)") CSLnumber, CSLlabels(i), qb(1:4)
    end do
end if

! initialize the random number generator
call rng_seed(seed,54532543)

if (trim(gbonl%CSLtype).eq.'') then
!open(unit=20,file='testresults.txt',status='unknown',form='formatted')
  call OMP_SET_NUM_THREADS(gbonl%nthreads)
  io_int(1) = gbonl%nthreads
  call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

  !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, aa, bb, cc, dd, pp, qa, qb, qc, qd, qq, tt, ip, i,j,x1,x2,y1,y2,s1,s2) 

  TID = OMP_GET_THREAD_NUM()

  !$OMP DO SCHEDULE(DYNAMIC)
  do i=1,numoct
    if (mode.eq.'rand') then 
! get four random cubochoric points
      aa = acube * (/ 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0 /)
      bb = acube * (/ 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0 /)
      cc = acube * (/ 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0 /)
      dd = acube * (/ 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0 /)
      qa = cu2qu(aa)
      qb = cu2qu(bb)
      qc = cu2qu(cc)
      qd = cu2qu(dd)
    else
! get 2 octonions from the octarray
      j = 2*(i-1)+1
      qa = octarray(1:4,j) 
      qb = octarray(5:8,j)
      qc = octarray(1:4,j+1) 
      qd = octarray(5:8,j+1)
    end if
! generate the unit quaternions with positive scalar part
    qa = qa/norm2(qa)
    qb = qb/norm2(qb)
    qc = qc/norm2(qc)
    qd = qd/norm2(qd)

! GBO geodesic distance with symmetry
    if (gbonl%fixedAB.eqv..TRUE.) then
      tt = GBO_Omega_symmetric(qa,qb,qc,qd,dict,single=.TRUE.)
    else
      tt = GBO_Omega_symmetric(qa,qb,qc,qd,dict)
    end if
    ip = nint(tt*scale)
    if (ip.lt.numb) histogramSYM(ip) = histogramSYM(ip) + 1

! No-Boundary GBO geodesic distance
    tt = GBO_Omega_symmetric_NB(qa,qc,dict)
    ip = nint(tt*scale)
    if (ip.lt.numb) histogramNBSYM(ip) = histogramNBSYM(ip) + 1

    if (mod(i,100000).eq.0) then
      io_int(1) = i 
      io_int(2) = gbonl%numsamples
      call WriteValue('completed ',io_int,2,"(I14,' out of ',I14)")
    end if
  end do
  !$OMP END DO
  !$OMP END PARALLEL
else
! we're doing a CSL boundary so let's get the correct quaternion for it ... 
  qa = (/ 1.D0, 0.D0, 0.D0, 0.D0 /)
  qb = ro2qu( GB_getCSLrod( gbonl%CSLtype, CSLnumber) )
  qb = qb/norm2(qb)

  call Message('')
  write (*,*) '  CSL quaternion : ', qb

  call OMP_SET_NUM_THREADS(gbonl%nthreads)
  io_int(1) = gbonl%nthreads
  call WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

  !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, cc, dd, pp, qc, qd, qq, tt, ip, i,j, x1,x2,y1,y2,s1,s2) 

  TID = OMP_GET_THREAD_NUM()

  ! generate random quartets of quaternions
  !$OMP DO SCHEDULE(DYNAMIC)
  do i=1,numoct
    if (mode.eq.'rand') then 
! get four random cubochoric points
      cc = acube * (/ 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0 /)
      dd = acube * (/ 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0, 2.D0*rng_uniform(seed)-1.D0 /)
! generate the unit quaternions with positive scalar part
      qc = cu2qu(cc)
      qd = cu2qu(dd)
    else 
      qc = octarray(1:4,i) 
      qd = octarray(5:8,i)
    end if 
    qc = qc/norm2(qc)
    qd = qd/norm2(qd)

  ! GBO geodesic distance with symmetry
    if (gbonl%fixedAB.eqv..TRUE.) then
      tt = GBO_Omega_symmetric(qa,qb,qc,qd,dict,single=.TRUE.)
    else
      tt = GBO_Omega_symmetric(qa,qb,qc,qd,dict)
    end if
    ip = nint(tt*scale)
    if (ip.lt.numb) histogramSYM(ip) = histogramSYM(ip) + 1

  ! No-Boundary GBO geodesic distance
    tt = GBO_Omega_symmetric_NB(qa,qc,dict)
    ip = nint(tt*scale)
    if (ip.lt.numb) histogramNBSYM(ip) = histogramNBSYM(ip) + 1

    if (mod(i,100000).eq.0) then
      io_int(1) = i 
      io_int(2) = gbonl%numsamples
      call WriteValue('completed ',io_int,2,"(I14,' out of ',I14)")
    end if
  end do
  !$OMP END DO
  !$OMP END PARALLEL
end if

! output results
fname = trim(EMsoft_getEMdatapathname())//trim(gbonl%outname)
fname = EMsoft_toNativePath(fname)

open(unit=dataunit,file=trim(fname),status='unknown',form='formatted')
write (dataunit,"(I6)") numb
do i=0,numb
  write (dataunit,"(3I10)") i, histogramSYM(i), histogramNBSYM(i)
end do 
close(unit=dataunit,status='keep')

end program EMGBO
