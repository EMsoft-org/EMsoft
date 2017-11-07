! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
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
!> @param source the opencl kernel as a character array
!> @param length of character array
!> @param platform opencl platform type
!> @param device opencl device type
!> @param context opencl context type
!> @param command_queue opencl command queue
!
!> @date 12/09/14  SS 1.0 original
!> @date 27/01/15  SS 1.1 modified to call the subroutine from mastersubroutine
!> @date 02/24/16 MDG 1.2 converted OpenCL calls to clfortran from fortrancl
!> @date 03/03/16 MDG 1.3 added C_NULL_CHAR to kernelname
!> @date 06/07/17 MDG 1.4 removed progoptions from Build Program call; caused some issues on Linux in Release mode
!--------------------------------------------------------------------------
recursive subroutine InnerProdGPU(cl_expt,cl_dict,Ne,Nd,correctsize,results,numd,selnumd,csource,source_length, &
                        platform,device,context,command_queue)
!DEC$ ATTRIBUTES DLLEXPORT :: InnerProdGPU

use local
use clfortran
use CLsupport
use ISO_C_BINDING
use io
use error

IMPLICIT NONE

real(kind=4),INTENT(OUT),target                     :: results(Ne*Nd)
integer(c_intptr_t),target,INTENT(INOUT)            :: cl_expt
integer(c_intptr_t),target,INTENT(INOUT)            :: cl_dict

integer(kind=4),INTENT(IN)                          :: Ne
integer(kind=4),INTENT(IN)                          :: Nd
integer(kind=4),INTENT(IN)                          :: correctsize
integer(kind=irg),INTENT(IN)                        :: numd, selnumd
integer(c_size_t),INTENT(IN),target                 :: source_length
character(len=source_length, KIND=c_char),TARGET,INTENT(IN)      :: csource
integer(c_intptr_t),allocatable,target,INTENT(IN)   :: platform(:)
integer(c_intptr_t),allocatable,target,INTENT(INOUT):: device(:)
integer(c_intptr_t),target,INTENT(INOUT)            :: context
integer(c_intptr_t),target,INTENT(INOUT)            :: command_queue

type(c_ptr), target                                 :: psource
integer(c_int32_t)                                  :: ierr, ierr2, pcnt
integer(c_intptr_t),target                          :: prog
integer(c_intptr_t),target                          :: kernel
integer(c_intptr_t),target                          :: cl_result
character(19),target                                :: progoptions
integer(c_size_t)                                   :: cnum
character(len=source_length),target                 :: source

real(kind=4)                                        :: dicttranspose(Nd*correctsize)
integer(kind=4),parameter                           :: iunit = 40
character(fnlen)                                    :: info ! info about the GPU
integer(kind=8),target                              :: globalsize(2),localsize(2)
integer, parameter                                  :: source_length_build_info = 10000
character(len = source_length)                      :: source_build_info
integer(kind=4)                                     :: num,istat,i,j,ii,jj,kk, io_int(1)
integer(kind=4),target                              :: Wexp,Wdict
integer(kind=8)                                     :: size_in_bytes_expt,size_in_bytes_dict,size_in_bytes_result
character(9),target                                 :: kernelname
character(10, KIND=c_char),target                   :: ckernelname
integer(kind=irg)                                   :: irec

size_in_bytes_result = Ne*Nd*sizeof(results(1))
Wexp = correctsize
Wdict = Nd
localsize = (/16,16/)
globalsize = (/Ne,Nd/)

!=====================
! INITIALIZATION
!=====================
! was performed in the calling program

!=====================
! BUILD THE KERNEL
!=====================

! create the program
pcnt = 1
psource = C_LOC(csource)
prog = clCreateProgramWithSource(context, pcnt, C_LOC(psource), C_LOC(source_length), ierr)
call CLerror_check('InnerProdGPU:clCreateProgramWithSource', ierr)

! build the program
progoptions = '-cl-no-signed-zeros'
! ierr = clBuildProgram(prog, numd, C_LOC(device), C_LOC(progoptions), C_NULL_FUNPTR, C_NULL_PTR)
ierr = clBuildProgram(prog, numd, C_LOC(device), C_NULL_PTR, C_NULL_FUNPTR, C_NULL_PTR)

! get the compilation log
ierr2 = clGetProgramBuildInfo(prog, device(selnumd), CL_PROGRAM_BUILD_LOG, sizeof(source), C_LOC(source), cnum)
! if(cnum > 1) call Message(trim(source(1:cnum))//'test',frm='(A)')
call CLerror_check('InnerProdGPU:clBuildProgram', ierr)
call CLerror_check('InnerProdGPU:clGetProgramBuildInfo', ierr2)

! finally get the kernel and release the program
kernelname = 'InnerProd'
ckernelname = kernelname
ckernelname(10:10) = C_NULL_CHAR
kernel = clCreateKernel(prog, C_LOC(ckernelname), ierr)
call CLerror_check('InnerProdGPU:clCreateKernel', ierr)

ierr = clReleaseProgram(prog)
call CLerror_check('InnerProdGPU:clReleaseProgram', ierr)

! create buffer
cl_result = clCreateBuffer(context, CL_MEM_READ_WRITE, size_in_bytes_result, C_NULL_PTR, ierr)
call CLerror_check('InnerProdGPU:clCreateBuffer', ierr)

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

ierr = clReleaseKernel(kernel)
call CLerror_check('InnerProdGPU:clReleaseKernel', ierr)
ierr = clReleaseMemObject(cl_result)
call CLerror_check('InnerProdGPU:clReleaseMemObject:cl_result', ierr)

end subroutine InnerProdGPU
!--------------------------------------------------------------------------

recursive function Jaccard_Distance(img1,img2,nn) result(JD)
!DEC$ ATTRIBUTES DLLEXPORT :: Jaccard_Distance

use local

IMPLICIT NONE

integer(kind=irg),INTENT(IN)      :: img1(nn)
integer(kind=irg),INTENT(IN)      :: img2(nn)
integer(kind=irg),INTENT(IN)      :: nn

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

!JD = -H12 + (H1 + H2)
JD = 2.D0 - (H1 + H2)/H12  

end function Jaccard_Distance

end module
