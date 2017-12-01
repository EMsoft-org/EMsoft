! ###################################################################
! Copyright (c) 2013-2016, Marc De Graef Research Group/Carnegie Mellon University
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
module CLsupport

use local
use clfortran

IMPLICIT NONE


contains


!--------------------------------------------------------------------------
!
! SUBROUTINE:CLquery_platform_info
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief display information about an OpenCL device (based on (clfortran's query_platforms_devices.f90)
!
!> @param platform_id id number of platform
!
!> @date 02/18/16 MDG 1.0 modification of clfortran's original routine
!> @date 05/21/16 MDG 1.1 split CPU and GPU device information into separate blocks
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! original Copyright information (clfortran's query_platforms_devices.f90)
! -----------------------------------------------------------------------------
!
! Copyright (C) 2013-2014 Company for Advanced Supercomputing Solutions LTD
! Bosmat 2a St.
! Shoham
! Israel 60850
! http://www.cass-hpc.com
!
! Author: Mordechai Butrashvily <support@cass-hpc.com>
!
! -----------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
! -----------------------------------------------------------------------------
recursive subroutine CLquery_platform_info(platform_id)
!DEC$ ATTRIBUTES DLLEXPORT :: CLquery_platform_info

use ISO_C_BINDING

IMPLICIT NONE

! Input variable.
integer(c_intptr_t), INTENT(IN):: platform_id

! Helper variables to work with OpenCL API.
integer(c_int32_t)             :: err
integer(c_size_t)              :: zero_size = 0
integer(c_size_t)              :: temp_size
! For quering devices.
integer(c_int64_t)             :: device_type
integer(c_int32_t)             :: num_devices
integer(c_int)                 :: i
integer(c_intptr_t), allocatable, target :: device_ids(:)

! String arrays for holding platform details.
character, allocatable, target :: platform_profile(:)
character, allocatable, target :: platform_version(:)
character, allocatable, target :: platform_name(:)
character, allocatable, target :: platform_vendor(:)
character, allocatable, target :: platform_extensions(:)

! String array for holding device name.
character, allocatable, target :: device_name(:)
! Maximum compute units for device.
integer(c_size_t), target      :: device_mwgs, device_mwis(3), device_maxalloc
integer(c_int32_t), target     :: device_cu

integer(c_int64_t), target     :: device_gms, device_mmas


! Profile.
err = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, zero_size, C_NULL_PTR, temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
allocate(platform_profile(temp_size))
err = clGetPlatformInfo(platform_id, CL_PLATFORM_PROFILE, temp_size, C_LOC(platform_profile), temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
print *, 'Profile: ', platform_profile
deallocate(platform_profile)

! Version.
err = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, zero_size, C_NULL_PTR, temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
allocate(platform_version(temp_size))
err = clGetPlatformInfo(platform_id, CL_PLATFORM_VERSION, temp_size, C_LOC(platform_version), temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
print *, 'Version: ', platform_version
deallocate(platform_version)

! Name.
err = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, zero_size, C_NULL_PTR, temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
allocate(platform_name(temp_size))
err = clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, temp_size, C_LOC(platform_name), temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
print *, 'Name: ', platform_name
deallocate(platform_name)

! Vendor.
err = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, zero_size, C_NULL_PTR, temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
allocate(platform_vendor(temp_size))
err = clGetPlatformInfo(platform_id, CL_PLATFORM_VENDOR, temp_size, C_LOC(platform_vendor), temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
print *, 'Vendor: ', platform_vendor
deallocate(platform_vendor)

! Extensions.
err = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, zero_size, C_NULL_PTR, temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
allocate(platform_extensions(temp_size))
err = clGetPlatformInfo(platform_id, CL_PLATFORM_EXTENSIONS, temp_size, C_LOC(platform_extensions), temp_size)
call CLerror_check('CLquery_platform_info:clGetPlatformInfo',err)
print *, 'Extensions: ', platform_extensions
deallocate(platform_extensions)

!
! Print device information for this platform.
!
! Get device count.
print *
! device_type = CL_DEVICE_TYPE_CPU
err = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_CPU, 0, C_NULL_PTR, num_devices)
call CLerror_check('CLquery_platform_info:clGetDeviceIDs',err,.TRUE.)

if (err /= CL_SUCCESS .or. num_devices < 1) then
  print *, 'No CPU devices found on this platform'
else
  print *
  print '(A, I2)', 'Num CPU Devices: ', num_devices

! Allocate an array to hold device handles.
  allocate(device_ids(num_devices))

! Get device IDs.
  err = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_CPU, num_devices, C_LOC(device_ids), num_devices)
  call CLerror_check('CLquery_platform_info:clGetDeviceIDs',err)
  if (err /= CL_SUCCESS) then
    print *, 'Error quering CPU devices: ', err
    return
  end if

! Loop over devices and print information.
  do i = 1, num_devices
! Maximum compute units.
    temp_size = 8
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_COMPUTE_UNITS, temp_size, C_LOC(device_cu), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)
  
    temp_size = 8
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_GLOBAL_MEM_SIZE, temp_size, C_LOC(device_gms), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)
    device_gms = device_gms/1024/1024/1024

! temp_size = 8
! err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_MEM_ALLOC_SIZE, temp_size, C_LOC(device_gms), temp_size)
!write (*,*) 'mmas : ', device_mmas

! CL_DEVICE_MAX_WORK_GROUP_SIZE
    temp_size = 8 
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_WORK_GROUP_SIZE, temp_size, C_LOC(device_mwgs), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)

! CL_DEVICE_MAX_WORK_ITEM_SIZES
    temp_size = 8 * 3
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_WORK_ITEM_SIZES, temp_size, C_LOC(device_mwis), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)

! Name.
    temp_size = 4
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_NAME, zero_size, C_NULL_PTR, temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)
    allocate(device_name(temp_size))
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_NAME, temp_size, C_LOC(device_name), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)

! Print brief device details. Since this routine can be used by the user to determine GPU device IDs,
! we subtract 1 from the device ID to make sure that the CPU gets number 0...
    write (*, '(A,I2,A,I4,A,I4,A,I4,A,I4,A,I4,A,I3,A,$)') ' Device (#', i, ', CU/MWGS/MWIS/GMS: ',device_cu,'/',device_mwgs,'/',&
                                                   device_mwis(1),',',device_mwis(2),',',device_mwis(3),'/',device_gms,') - '
    print *, device_name
    deallocate(device_name)
  end do

end if

! Get device count.
!device_type = CL_DEVICE_TYPE_ALL
print *
err = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_GPU, 0, C_NULL_PTR, num_devices)
call CLerror_check('CLquery_platform_info:clGetDeviceIDs',err,.TRUE.)

if (err /= CL_SUCCESS .or. num_devices < 1) then
  print *, 'No GPU devices found on this platform '
else
  print *
  print '(A, I2)', 'Num GPU Devices: ', num_devices

! Allocate an array to hold device handles.
  if (allocated(device_ids)) deallocate(device_ids)
  allocate(device_ids(num_devices))

! Get device IDs.
  err = clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_GPU, num_devices, C_LOC(device_ids), num_devices)
  call CLerror_check('CLquery_platform_info:clGetDeviceIDs',err)
  if (err /= CL_SUCCESS) then
    print *, 'Error quering devices: ', err
    return
  end if


! Loop over devices and print information.
  do i = 1, num_devices
! Maximum compute units.
    temp_size = 8
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_COMPUTE_UNITS, temp_size, C_LOC(device_cu), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)

    temp_size = 8
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_GLOBAL_MEM_SIZE, temp_size, C_LOC(device_gms), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)
    device_gms = device_gms/1024/1024/1024

! temp_size = 8
! err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_MEM_ALLOC_SIZE, temp_size, C_LOC(device_gms), temp_size)
!write (*,*) 'mmas : ', device_mmas

! CL_DEVICE_MAX_WORK_GROUP_SIZE
    temp_size = 8 
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_WORK_GROUP_SIZE, temp_size, C_LOC(device_mwgs), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)


! CL_DEVICE_MAX_WORK_ITEM_SIZES
    temp_size = 8 * 3
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_WORK_ITEM_SIZES, temp_size, C_LOC(device_mwis), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)

! Name.
    temp_size = 4
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_NAME, zero_size, C_NULL_PTR, temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)
    allocate(device_name(temp_size))
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_NAME, temp_size, C_LOC(device_name), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)

! CL_DEVICE_MAX_MEM_ALLOC_SIZE
    temp_size = 8 
    err = clGetDeviceInfo(device_ids(i), CL_DEVICE_MAX_MEM_ALLOC_SIZE, temp_size, C_LOC(device_maxalloc), temp_size)
    call CLerror_check('CLquery_platform_info:clGetDeviceInfo',err)
    device_maxalloc = device_maxalloc/1024/1024

! Print brief device details. Since this routine can be used by the user to determine GPU device IDs,
! we subtract 1 from the device ID to make sure that the CPU gets number 0...
    write (*, '(A,I2,A,I4,A,I4,A,I4,A,I4,A,I4,A,I3,A,I4,A,$)') ' Device (#', i, ', CU/MWGS/MWIS/GMS/MAS: ',device_cu,'/',&
          device_mwgs,'/',device_mwis(1),',',device_mwis(2),',',device_mwis(3),'/',device_gms,',',device_maxalloc,') - '
    print *, device_name
    deallocate(device_name)
  end do
end if

print *,' '
write (*,*) '[CU = Compute Units; MWGS = Maximum Work Group Size; MWIS = Maximum Work Item Sizes (3D); '// &
            'GMS = Global Memory Size (Gb); MAS = Maximum Allocatable Memory Size (Mb)]'

end subroutine CLquery_platform_info


!--------------------------------------------------------------------------
!
! SUBROUTINE:CLread_source_file
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief read an OpenCL source file and return the source properly formatted
!
!> @param sourcefile filename for the OpenCL source code
!> @param source c_str containing the source, NULL-terminated
!> @param slength source string length
!
!> @date 02/18/16  MDG 1.0 original
!> @date 01/15/17  MDG 1.1 added functionality for second opencl folder for developers...
!--------------------------------------------------------------------------
recursive subroutine CLread_source_file(sourcefile, csource, slength)
!DEC$ ATTRIBUTES DLLEXPORT :: CLread_source_file

use local
use error
use ISO_C_BINDING

IMPLICIT NONE

integer, parameter                      :: source_length = 50000

character(fnlen), INTENT(IN)            :: sourcefile
character(len=source_length, KIND=c_char),INTENT(OUT) :: csource
integer(c_size_t),INTENT(OUT)           :: slength


character(len=source_length),target     :: source
character(fnlen)                        :: fname, clpath, clpath2, tcf
integer(kind=irg)                       :: irec, ierr, ipos, i, j
logical                                 :: develop, fexist
character(1)                            :: EMsoftnativedelimiter
integer(kind=irg)                       :: idx


! find the cl file in the main opencl folder or the private folder if the Develop mode equals Yes...
clpath = trim(EMsoft_getOpenCLpathname())

! then, determine whether or not the user is working in develop mode by checking for the 
! Develop keyword in the EMsoftconfig.json file... Regular users will only have a single
! opencl folder, but developers have two, so we need to make sure we check both
! locations.  The second location is the private folder...
develop = EMsoft_getEMdevelop()
clpath2 = ''
if (develop.eqv..TRUE.) then
  ipos = index(clpath,'Public')
  do i=1,ipos-1
    clpath2(i:i) = clpath(i:i)
  end do
  tcf = 'Private/opencl/'
  do i=ipos,ipos+15
    j = i-ipos+1
    clpath2(i:i) = tcf(j:j)
  end do
end if

if (trim(EMsoft_getEMsoftplatform()).eq.SC_Windows) then
  EMsoftnativedelimiter = ':'
  idx = 2
else
  EMsoftnativedelimiter = '/'
  idx = 1
end if

if (sourcefile(idx:idx).ne.EMsoftnativedelimiter) then
fname = trim(clpath)//trim(sourcefile)
else
fname = trim(sourcefile)
endif
fname = EMsoft_toNativePath(fname)
inquire(file=trim(fname),exist=fexist)
if (.not.fexist) then 
  if (develop.eqv..TRUE.) then
   fname = trim(clpath2)//trim(sourcefile)
   fname = EMsoft_toNativePath(fname)
   inquire(file=trim(fname),exist=fexist)
   if (.not.fexist) then 
     call FatalError('CLread_source_file','opencl source file '//trim(sourcefile)//' not found in either opencl folder.'&
     //trim(fname))
   end if
  else
   call FatalError('CLread_source_file','opencl source  file '//trim(fname)//' not found')
  end if
 end if

! read the source file from the opencl folder
open(unit = dataunit, file = trim(fname), access='direct', status = 'old', &
     action = 'read', iostat = ierr, recl = 1)
if (ierr /= 0) call FatalError("CLread_source_file: ",'Cannot open file '//fname)

source = ''
irec = 1
do
  read(unit = dataunit, rec = irec, iostat = ierr) source(irec:irec)
  if (ierr /= 0) exit
  if(irec == source_length) call FatalError("CLread_source_file: ",'Error: CL source file is too big')
  irec = irec + 1
end do
close(unit=dataunit)

csource = trim(source)
csource(irec:irec) = C_NULL_CHAR
slength = irec

end subroutine CLread_source_file


!--------------------------------------------------------------------------
!
! SUBROUTINE:CLinit_PDCCQ
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief initalize a CL platform, device, context, and command queue
!
!> @param 
!
!> @date 02/23/16  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CLinit_PDCCQ(platform, nump, selnump, device, numd, selnumd, devinfo, context, command_queue)
!DEC$ ATTRIBUTES DLLEXPORT :: CLinit_PDCCQ

use ISO_C_BINDING
use error
use io

IMPLICIT NONE

integer(c_intptr_t),allocatable, target  :: platform(:)
integer(kind=irg), INTENT(OUT)           :: nump
integer(kind=irg), INTENT(IN)            :: selnump
integer(c_intptr_t),allocatable, target  :: device(:)
integer(kind=irg), INTENT(OUT)           :: numd
integer(kind=irg), INTENT(IN)            :: selnumd
character(fnlen),INTENT(OUT)             :: devinfo
integer(c_intptr_t),target               :: context
integer(c_intptr_t),target               :: command_queue

integer(c_int32_t)                       :: ierr
integer(c_size_t)                        :: cnuminfo
character(fnlen),target                  :: info 
integer(c_intptr_t),target               :: ctx_props(3)
integer(c_int64_t)                       :: cmd_queue_props

! get the platform ID
ierr = clGetPlatformIDs(0, C_NULL_PTR, nump)
call CLerror_check('CLinit_PDCCQ:clGetPlatformIDs',ierr)
allocate(platform(nump))
ierr = clGetPlatformIDs(nump, C_LOC(platform), nump)
call CLerror_check('CLinit_PDCCQ:clGetPlatformIDs',ierr)

if (selnump.gt.nump) then
  call FatalError("CLinit_PDCCQ","non-existing platform id requested")
end if

! get the device ID
ierr =  clGetDeviceIDs(platform(selnump), CL_DEVICE_TYPE_GPU, 0, C_NULL_PTR, numd)
call CLerror_check('CLinit_PDCCQ:clGetDeviceIDs',ierr)
allocate(device(numd))
ierr =  clGetDeviceIDs(platform(selnump), CL_DEVICE_TYPE_GPU, numd, C_LOC(device), numd)
call CLerror_check('CLinit_PDCCQ:clGetDeviceIDs',ierr)

if (selnumd.gt.numd) then
  call FatalError("CLinit_PDCCQ","non-existing device id requested")
end if

! get the device name and return it as devinfo
ierr = clGetDeviceInfo(device(selnumd), CL_DEVICE_NAME, sizeof(info), C_LOC(info), cnuminfo)
call CLerror_check('CLinit_PDCCQ:clGetDeviceInfo',ierr)

if (cnuminfo.gt.fnlen) then 
  call WriteValue("CLinit_PDCCQ","device info string truncated to 132 characters")
  devinfo = trim(info(1:132))
else
  devinfo = trim(info(1:cnuminfo))
end if

! create the context and the command queue
ctx_props(1) = CL_CONTEXT_PLATFORM
ctx_props(2) = platform(selnump)
ctx_props(3) = 0
context = clCreateContext(C_LOC(ctx_props), numd, C_LOC(device),C_NULL_FUNPTR, C_NULL_PTR, ierr)
call CLerror_check('CLinit_PDCCQ:clCreateContext',ierr)

cmd_queue_props = CL_QUEUE_PROFILING_ENABLE
command_queue = clCreateCommandQueue(context, device(selnumd), cmd_queue_props, ierr)
call CLerror_check('CLinit_PDCCQ:clCreateCommandQueue',ierr)

end subroutine CLinit_PDCCQ


!--------------------------------------------------------------------------
!
! SUBROUTINE:CLerror_check
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief checks whether or not there was a CL 1.2 error and returns the error message.
!
!> @param ierr error number (0 is no error) 
!
!> @date 06/06/16  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine CLerror_check(routine, ierr, nonfatal)
!DEC$ ATTRIBUTES DLLEXPORT :: CLerror_check

use ISO_C_BINDING
use error 
use io

IMPLICIT NONE

character(*),INTENT(IN)                 :: routine
integer(kind=c_int32_t),INTENT(IN)      :: ierr
logical,INTENT(IN),OPTIONAL             :: nonfatal

character(fnlen)                        :: estr
integer(kind=irg)                       :: iout(1)

if (ierr.ne.0) then

  select case(ierr)
   case(-1) 
        estr = 'Error: CL_DEVICE_NOT_FOUND' !                       = -1
   case(-2) 
        estr = 'Error: CL_DEVICE_NOT_AVAILABLE' !                   = -2
   case(-3) 
        estr = 'Error: CL_COMPILER_NOT_AVAILABLE' !                 = -3
   case(-4) 
        estr = 'Error: CL_MEM_OBJECT_ALLOCATION_FAILURE' !          = -4
   case(-5) 
        estr = 'Error: CL_OUT_OF_RESOURCES' !                       = -5
   case(-6) 
        estr = 'Error: CL_OUT_OF_HOST_MEMORY' !                     = -6
   case(-7) 
        estr = 'Error: CL_PROFILING_INFO_NOT_AVAILABLE' !           = -7
   case(-8) 
        estr = 'Error: CL_MEM_COPY_OVERLAP' !                       = -8
   case(-9) 
        estr = 'Error: CL_IMAGE_FORMAT_MISMATCH' !                  = -9
   case(-10) 
        estr = 'Error: CL_IMAGE_FORMAT_NOT_SUPPORTED' !             = -10
   case(-11) 
        estr = 'Error: CL_BUILD_PROGRAM_FAILURE' !                  = -11
   case(-12) 
        estr = 'Error: CL_MAP_FAILURE' !                            = -12
   case(-13) 
        estr = 'Error: CL_MISALIGNED_SUB_BUFFER_OFFSET' !           = -13
   case(-14) 
        estr = 'Error: CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST' ! 14
   case(-15) 
        estr = 'Error: CL_COMPILE_PROGRAM_FAILURE' !                = -15
   case(-16) 
        estr = 'Error: CL_LINKER_NOT_AVAILABLE' !                   = -16
   case(-17) 
        estr = 'Error: CL_LINK_PROGRAM_FAILURE' !                   = -17
   case(-18) 
        estr = 'Error: CL_DEVICE_PARTITION_FAILED' !                = -18
   case(-19) 
        estr = 'Error: CL_KERNEL_ARG_INFO_NOT_AVAILABLE' !          = -19

   case(-30) 
        estr = 'Error: CL_INVALID_VALUE' !                          = -30
   case(-31) 
        estr = 'Error: CL_INVALID_DEVICE_TYPE' !                    = -31
   case(-32) 
        estr = 'Error: CL_INVALID_PLATFORM' !                       = -32
   case(-33) 
        estr = 'Error: CL_INVALID_DEVICE' !                         = -33
   case(-34) 
        estr = 'Error: CL_INVALID_CONTEXT' !                        = -34
   case(-35) 
        estr = 'Error: CL_INVALID_QUEUE_PROPERTIES' !               = -35
   case(-36) 
        estr = 'Error: CL_INVALID_COMMAND_QUEUE' !                  = -36
   case(-37) 
        estr = 'Error: CL_INVALID_HOST_PTR' !                       = -37
   case(-38) 
        estr = 'Error: CL_INVALID_MEM_OBJECT' !                     = -38
   case(-39) 
        estr = 'Error: CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' !        = -39
   case(-40) 
        estr = 'Error: CL_INVALID_IMAGE_SIZE' !                     = -40
   case(-41) 
        estr = 'Error: CL_INVALID_SAMPLER' !                        = -41
   case(-42) 
        estr = 'Error: CL_INVALID_BINARY' !                         = -42
   case(-43) 
        estr = 'Error: CL_INVALID_BUILD_OPTION' !                   = -43
   case(-44) 
        estr = 'Error: CL_INVALID_PROGRAM' !                        = -44
   case(-45) 
        estr = 'Error: CL_INVALID_PROGRAM_EXECUTABLE' !             = -45
   case(-46) 
        estr = 'Error: CL_INVALID_KERNEL_NAME' !                    = -46
   case(-47) 
        estr = 'Error: CL_INVALID_KERNEL_DEFINITION' !              = -47
   case(-48) 
        estr = 'Error: CL_INVALID_KERNEL' !                         = -48
   case(-49) 
        estr = 'Error: CL_INVALID_ARG_INDEX' !                      = -49
   case(-50) 
        estr = 'Error: CL_INVALID_ARG_VALUE' !                      = -50
   case(-51) 
        estr = 'Error: CL_INVALID_ARG_SIZE' !                       = -51
   case(-52) 
        estr = 'Error: CL_INVALID_KERNEL_ARGS' !                    = -52
   case(-53) 
        estr = 'Error: CL_INVALID_WORK_DIMENSION' !                 = -53
   case(-54) 
        estr = 'Error: CL_INVALID_WORK_GROUP_SIZE' !                = -54
   case(-55) 
        estr = 'Error: CL_INVALID_WORK_ITEM_SIZE' !                 = -55
   case(-56) 
        estr = 'Error: CL_INVALID_GLOBAL_OFFSET' !                  = -56
   case(-57) 
        estr = 'Error: CL_INVALID_EVENT_WAIT_LIST' !                = -57
   case(-58) 
        estr = 'Error: CL_INVALID_EVENT' !                          = -58
   case(-59) 
        estr = 'Error: CL_INVALID_OPERATION' !                      = -59
   case(-60) 
        estr = 'Error: CL_INVALID_GL_OBJECT' !                      = -60
   case(-61) 
        estr = 'Error: CL_INVALID_BUFFER_SIZE' !                    = -61
   case(-62) 
        estr = 'Error: CL_INVALID_MIP_LEVEL' !                      = -62
   case(-63) 
        estr = 'Error: CL_INVALID_GLOBAL_WORK_SIZE' !               = -63
   case(-64) 
        estr = 'Error: CL_INVALID_PROPERTY' !                       = -64
   case(-65) 
        estr = 'Error: CL_INVALID_IMAGE_DESCRIPTOR' !               = -65
   case(-66) 
        estr = 'Error: CL_INVALID_COMPILER_OPTIONS' !               = -66
   case(-67) 
        estr = 'Error: CL_INVALID_LINKER_OPTIONS' !                 = -67
   case(-68) 
        estr = 'Error: CL_INVALID_DEVICE_PARTITION_COUNT' !         = -68
   case default  
        estr = 'Error: Unknown CL error code'
        iout(1) = ierr
        call WriteValue('Unknown CL error code : ', iout, 1)
  end select

  if (present(nonfatal)) then
    if (nonfatal.eqv..TRUE.) then
      print *, ' Non-fatal error: '//trim(estr)
    end if
  else
    call FatalError(trim(routine),trim(estr))
  end if

end if

end subroutine CLerror_check


end module CLsupport
