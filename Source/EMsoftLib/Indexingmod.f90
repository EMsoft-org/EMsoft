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
! EMsoft:Indexingmod.f90
!--------------------------------------------------------------------------
!
! MODULE: Indexingmod
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief everything to do with indexing and such
!
!> @date 11/17/15 SS 1.0 original
!---------------------------------------------------------------------------
module Indexingmod

use local
use NameListTypedefs

IMPLICIT NONE

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:InnerProdGPU
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Perform the inner product computations for the dictionary approach
!
!> @param expt vector with list of observed patterns
!> @param dict vector with list of calculated patterns
!> @param Ne number of patterns in the expt vector
!> @param Nd number of patterns in the dict vector
!> @param L size of one single pattern
!> @param result result of the matrix multiplication
!> @param kernel opencl kernel pointer
!> @param context opencl context type
!> @param command_queue opencl command queue
!
!> @date 12/09/14  SS 1.0 original
!> @date 27/01/15  SS 1.1 modified to call the subroutine from mastersubroutine
!> @date 02/24/16 MDG 1.2 converted OpenCL calls to clfortran from fortrancl
!> @date 03/03/16 MDG 1.3 added C_NULL_CHAR to kernelname
!> @date 06/07/17 MDG 1.4 removed progoptions from Build Program call; caused some issues on Linux in Release mode
!> @date 11/13/17 MDG 2.0 moved several OpenCL init statements to main calling program
!--------------------------------------------------------------------------
recursive subroutine InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,selnumd,kernel,context,command_queue)
!DEC$ ATTRIBUTES DLLEXPORT :: InnerProdGPU

use local
use clfortran
use CLsupport
use ISO_C_BINDING
use io
use error

IMPLICIT NONE

integer(kind=4),INTENT(IN)                          :: Ne
integer(kind=4),INTENT(IN)                          :: Nd
real(kind=4),INTENT(OUT),target                     :: results(Ne*Nd)
integer(c_intptr_t),target,INTENT(INOUT)            :: cl_expt
!f2py intent(in,out) ::  cl_expt
integer(c_intptr_t),target,INTENT(INOUT)            :: cl_dict
!f2py intent(in,out) ::  cl_dict


integer(kind=4),INTENT(IN)                          :: correctsize
integer(kind=irg),INTENT(IN)                        :: numd, selnumd
integer(c_intptr_t),target,INTENT(INOUT)            :: context
!f2py intent(in,out) ::  context
integer(c_intptr_t),target,INTENT(INOUT)            :: kernel
!f2py intent(in,out) ::  kernel
integer(c_intptr_t),target,INTENT(INOUT)            :: command_queue
!f2py intent(in,out) ::  command_queue

integer(c_int32_t)                                  :: ierr, ierr2, pcnt
integer(c_intptr_t),target                          :: cl_result

real(kind=4)                                        :: dicttranspose(Nd*correctsize)
integer(kind=4),parameter                           :: iunit = 40
character(fnlen)                                    :: info ! info about the GPU
integer(kind=8),target                              :: globalsize(2),localsize(2)
integer(kind=4)                                     :: num,istat,i,j,ii,jj,kk, io_int(1)
integer(kind=4),target                              :: Wexp,Wdict
integer(kind=8)                                     :: size_in_bytes_expt,size_in_bytes_dict,size_in_bytes_result
integer(kind=irg)                                   :: irec

size_in_bytes_result = Ne*Nd*sizeof(results(1))
Wexp = correctsize
Wdict = Nd
localsize = (/16,16/)
globalsize = (/Ne,Nd/)

!=====================
! INITIALIZATION [mostly performed in the calling program]
!=====================

! create buffer
cl_result = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_result, C_NULL_PTR, ierr)
call CLerror_check('InnerProdGPU:clCreateBuffer', ierr)

! ---- 

! set kernel arguments
ierr =  clSetKernelArg(kernel, 0, sizeof(cl_expt), C_LOC(cl_expt))
call CLerror_check('InnerProdGPU:clSetKernelArg:cl_expt', ierr)

ierr = clSetKernelArg(kernel, 1, sizeof(cl_dict), C_LOC(cl_dict))
call CLerror_check('InnerProdGPU:clSetKernelArg:cl_dict', ierr)

ierr = clSetKernelArg(kernel, 2, sizeof(Wexp), C_LOC(Wexp))
call CLerror_check('InnerProdGPU:clSetKernelArg:Wexp', ierr)

ierr = clSetKernelArg(kernel, 3, sizeof(Wdict), C_LOC(Wdict))
call CLerror_check('InnerProdGPU:clSetKernelArg:Wdict', ierr)

ierr = clSetKernelArg(kernel, 4, sizeof(cl_result), C_LOC(cl_result))
call CLerror_check('InnerProdGPU:clSetKernelArg:cl_result', ierr)

!execute the kernel
ierr = clEnqueueNDRangeKernel(command_queue, kernel, 2, C_NULL_PTR, C_LOC(globalsize), C_LOC(localsize), &
                              0, C_NULL_PTR, C_NULL_PTR)
call CLerror_check('InnerProdGPU:clEnqueueNDRangeKernel', ierr)

! wait for the commands to finish
ierr = clFinish(command_queue)
call CLerror_check('InnerProdGPU:clFinish', ierr)

! read the resulting vector from device memory
ierr = clEnqueueReadBuffer(command_queue,cl_result,CL_TRUE,0_8,size_in_bytes_result,C_LOC(results(1)),0,C_NULL_PTR,C_NULL_PTR)
call CLerror_check('InnerProdGPU:clEnqueueReadBuffer', ierr)

ierr = clReleaseMemObject(cl_result)
call CLerror_check('InnerProdGPU:clReleaseMemObject:cl_result', ierr)

! ---
end subroutine InnerProdGPU
!--------------------------------------------------------------------------

recursive function Jaccard_Distance(img1,img2,nn,mutualinformation) result(JD)
!DEC$ ATTRIBUTES DLLEXPORT :: Jaccard_Distance

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)      :: nn
integer(kind=irg),INTENT(IN)      :: img1(nn)
integer(kind=irg),INTENT(IN)      :: img2(nn)
logical,INTENT(IN),OPTIONAL       :: mutualinformation

real(kind=dbl)                    :: JD
real(kind=sgl)                    :: hist1(256), hist2(256), jhist(256,256), H1, H2, H12
integer(kind=irg)                 :: ii, jj, kk

hist1 = 0.D0
hist2 = 0.D0
jhist = 0.D0

H1 = 0.D0
H2 = 0.D0
H12 = 0.D0

do ii = 1,nn
    jj = img1(ii)+1
    kk = img2(ii)+1
    jhist(jj,kk) =  jhist(jj,kk) + 1.D0
end do

jhist = jhist/nn

hist1(1:256) = sum(jhist,2)
hist2(1:256) = sum(jhist,1)

!open(unit=13,file='/Users/saranshsingh/Desktop/jh.txt',form='formatted')

do ii = 0,255

    if (hist1(ii+1) .ne. 0.0) then
        H1 = H1 + hist1(ii+1)*log(hist1(ii+1))
    end if
    if (hist2(ii+1) .ne. 0.0) then
        H2 = H2 + hist2(ii+1)*log(hist2(ii+1))
    end if

    do jj = 0,255
        !write(13,'(F15.6)',advance='no')jhist(ii+1,jj+1)
        if (jhist(ii+1,jj+1) .ne. 0.0) then
            H12 = H12 + jhist(ii+1,jj+1)*log(jhist(ii+1,jj+1))
        end if
    end do
    !write(13,*)''
end do

if (present(mutualinformation)) then
  if (mutualinformation.eqv..TRUE.) then 
    JD = (H1 + H2) - H12
  else
    JD = 2.D0 - (H1 + H2)/H12  
  end if
else
    JD = 2.D0 - (H1 + H2)/H12  
end if

end function Jaccard_Distance

end module
