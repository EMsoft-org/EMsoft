!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!! Copyright (c) 2018, De Graef Group, Carnegie Mellon University                    !!
!! All rights reserved.                                                              !!
!!                                                                                   !!
!! Redistribution and use in source and binary forms, with or without                !!
!! modification, are permitted provided that the following conditions are met:       !!
!!                                                                                   !!
!!     - Redistributions of source code must retain the above copyright notice, this !!
!!       list of conditions and the following disclaimer.                            !!
!!     - Redistributions in binary form must reproduce the above copyright notice,   !!
!!       this list of conditions and the following disclaimer in the documentation   !!
!!       and/or other materials provided with the distribution.                      !!
!!     - Neither the copyright holder nor the names of its                           !!
!!       contributors may be used to endorse or promote products derived from        !!
!!       this software without specific prior written permission.                    !!
!!                                                                                   !!
!! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"       !!
!! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE         !!
!! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE    !!
!! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE      !!
!! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL        !!
!! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR        !!
!! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        !!
!! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,     !!
!! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE         !!
!! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.          !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-----------------------------------------------------------------------
! EMsoft:image.f90
!-----------------------------------------------------------------------
!
! modules         : image, image_base
! public constants: pix_i8, pix_i16, pix_i32, pix_i64, pix_r32, pix_r64, pix_unk
! public types    : image_t
!
!> @author: W.C. Lenthe, Carnegie Mellon University
!
!> @brief: image object with io routines and conversion from/to fortran arrays
!
!> @example: image_t construction and methods
!>
!> program image_test_driver
!>   use image
!>   use, intrinsic :: iso_fortran_env
!> implicit none
!>   ! declare variables
!>   integer                     :: iostat,i,j
!>   character(len=128)          :: iomsg
!>   logical                     :: isInteger
!>   type(image_t)               :: im
!>   integer(int8)               :: i8 (3,4)
!>   integer(int16), allocatable :: i16(:,:)
!> 
!>   ! fill array with test data and convert to image_t
!>   i8 = reshape([10,20,30,40,50,60,70,80,90,100,110,120], shape(i8))
!>   im = image_t(i8)
!>   if(im%empty()) write(*,*) "failed to convert array to image"
!> 
!>   ! write image to file with and without error checking
!>   call im%write("test.tif", iostat, iomsg) ! format automatically detected from extension
!>   if(0.ne.iostat) write(*,*) "failed to write image to file : " // iomsg
!>   call im%write("test.bmp") ! iostat / iomsg are optional but writing can fail silently
!> 
!>   ! clear image (free buffer) and read back from file
!>   call im%clear()
!>   im = image_t("test.tif") ! format automatically detected, iostat/iomsg are optional
!>   if(im%empty()) write(*,*) "failed to read image from file" ! empty image_t on failure
!> 
!>   ! print print image information
!>   write(*,*) "rank: "             , size(im%dims)
!>   write(*,*) "dims: "             , im%dims
!>   write(*,*) "samples per pixel: ", im%samplesPerPixel
!>   write(*,*) "total pixels: "     , im%size()
!>   select case(im%pixelType)
!>     case(pix_i8 )
!>       write(*,*) "pixel type: 8  bit integer"
!>     case(pix_i16)
!>       write(*,*) "pixel type: 16 bit integer"
!>     case(pix_i32)
!>       write(*,*) "pixel type: 32 bit integer"
!>     case(pix_i64)
!>       write(*,*) "pixel type: 64 bit integer"
!>     case(pix_r32)
!>       write(*,*) "pixel type: float"
!>     case(pix_r64)
!>       write(*,*) "pixel type: double"
!>     case(pix_unk)
!>       write(*,*) "pixel type: unknown"
!>   end select
!> 
!>   ! convert image buffer to fortran array and print data
!>   call im%getData(i16) ! safely casts to passed type if possible (no precision loss/overflow and right rank)
!>   if(.not.allocated(i16)) write(*,*) "failed to convert to fortran array"
!>   write(*,*) i16
!>   deallocate(i16) ! image_t handles its own memory but you're responsible for arrays allocated by getData
!> end program image_test_driver
!
!> @date: 03/14/18 WCL 1.0 original
!
!-----------------------------------------------------------------------

module image
  use, intrinsic :: iso_fortran_env ! fixed width integers
  use, intrinsic :: iso_c_binding   ! enum
  implicit none

  private
  public :: image_t

  ! enumeration of allowed pixel data types
  enum, bind(c)
    enumerator :: pix_i8  ! integer(int8 )
    enumerator :: pix_i16 ! integer(int16)
    enumerator :: pix_i32 ! integer(int32)
    enumerator :: pix_i64 ! integer(int64)
    enumerator :: pix_r32 ! real(real32)
    enumerator :: pix_r64 ! real(real64)
    enumerator :: pix_unk ! unknown
  end enum
  public pix_i8, pix_i16, pix_i32, pix_i64, pix_r32, pix_r64, pix_unk

  ! enumeration of allowed image file types (and expected file extensions)
  enum, bind(c)
    enumerator :: im_ext_tif ! tiff   (tif/tiff)
    enumerator :: im_ext_bmp ! bitmap (bmp)
    enumerator :: im_ext_unk ! unknown
  end enum

  ! overloaded functions to construct an image_t (simulates a constructor)
  interface image_t
    ! construct image_t from 2d/3d scalar array: image_t(array)
    ! vector (rgb, etc) images require new functions (w/ handling of 3d vs 2d vector images)
    module procedure image_build_i8
    module procedure image_build_i16
    module procedure image_build_i32
    module procedure image_build_i64
    module procedure image_build_r32
    module procedure image_build_r64
    module procedure image_build_i8_3
    module procedure image_build_i16_3
    module procedure image_build_i32_3
    module procedure image_build_i64_3
    module procedure image_build_r32_3
    module procedure image_build_r64_3

    ! construct image_t from file: image_t(filename, iostat, iomsg): iostat/iomsg optional, empty image on failure
    module procedure image_read
  end interface

  type image_t
    integer(int32), allocatable :: dims(:)         ! image dimensions (x,y,z,...)
    integer(int16)              :: samplesPerPixel ! 1 for scalar images, 3 for rgb, etc
    integer                     :: pixelType       ! one of enumerated pixel types, all samples of same type
    integer(int8) , allocatable :: buff(:)         ! image data
    logical                     :: unsigned        ! true if intger values should be interpreted as unsigned
  contains
    final     ::          image_destroy ! automatically deallocate memory
    procedure :: clear => image_clear   ! initialize image
    procedure :: size  => image_size    ! number of pixels
    procedure :: empty => image_empty   ! logical true if no image in object
    procedure :: write => image_write   ! write(filename, unit, iostat): iostat/iomsg optional, empty image on failure

    ! functions to copy data into passed 2d/3d array (casted up to wider type if needed, array unallocated on failure)
    procedure :: image_get2_i8, image_get2_i16, image_get2_i32, image_get2_i64, image_get2_r32, image_get2_r64, & ! 2d
                 image_get3_i8, image_get3_i16, image_get3_i32, image_get3_i64, image_get3_r32, image_get3_r64    ! 3d
    generic   :: getData  =>  image_get2_i8, image_get2_i16, image_get2_i32, image_get2_i64, image_get2_r32, image_get2_r64, &
                              image_get3_i8, image_get3_i16, image_get3_i32, image_get3_i64, image_get3_r32, image_get3_r64
  end type image_t

  ! function definitions need to be in submodules to avoid circular references with file format specific classes
  interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!            defined in this file (image_base submodule)             !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    module subroutine image_destroy(this)
      type(image_t), intent(inout) :: this ! image data structure to clean up
    end subroutine image_destroy

    module subroutine image_clear(this)
      class(image_t), intent(inout) :: this ! image data structure to clean up
    end subroutine image_clear

    module function image_size(this) result(pixels)
      class(image_t), intent(in) :: this   ! image data structure to compute size of
      integer                    :: pixels ! size of image in pixel
    end function image_size

    module function image_empty(this) result(empty)
      class(image_t), intent(in) :: this  ! image data structure to check
      logical                    :: empty ! true if the object doesn't contain an image
    end function image_empty

    module function image_build_i8 (data2) result(im)
      integer(int8) , intent(in) :: data2(:,:) ! image data
      type(image_t)              :: im
    end function image_build_i8

    module function image_build_i16(data2) result(im)
      integer(int16), intent(in) :: data2(:,:) ! image data
      type(image_t)              :: im
    end function image_build_i16

    module function image_build_i32(data2) result(im)
      integer(int32), intent(in) :: data2(:,:) ! image data
      type(image_t)              :: im
    end function image_build_i32

    module function image_build_i64(data2) result(im)
      integer(int64), intent(in) :: data2(:,:) ! image data
      type(image_t)              :: im
    end function image_build_i64

    module function image_build_r32(data2) result(im)
      real(real32)  , intent(in) :: data2(:,:) ! image data
      type(image_t)              :: im
    end function image_build_r32

    module function image_build_r64(data2) result(im)
      real(real64)  , intent(in) :: data2(:,:) ! image data
      type(image_t)              :: im
    end function image_build_r64

    ! functions to build image from 3d array
    module function image_build_i8_3 (data3) result(im)
      integer(int8) , intent(in) :: data3(:,:,:) ! image data
      type(image_t)              :: im
    end function image_build_i8_3

    module function image_build_i16_3(data3) result(im)
      integer(int16), intent(in) :: data3(:,:,:) ! image data
      type(image_t)              :: im
    end function image_build_i16_3

    module function image_build_i32_3(data3) result(im)
      integer(int32), intent(in) :: data3(:,:,:) ! image data
      type(image_t)              :: im
    end function image_build_i32_3

    module function image_build_i64_3(data3) result(im)
      integer(int64), intent(in) :: data3(:,:,:) ! image data
      type(image_t)              :: im
    end function image_build_i64_3

    module function image_build_r32_3(data3) result(im)
      real(real32)  , intent(in) :: data3(:,:,:) ! image data
      type(image_t)              :: im
    end function image_build_r32_3

    module function image_build_r64_3(data3) result(im)
      real(real64)  , intent(in) :: data3(:,:,:) ! image data
      type(image_t)              :: im
    end function image_build_r64_3

    ! functions to get data as appropriate type (casting up if needed)
    module function image_get_i8 (this) result(data)
      class(image_t), intent(in)  :: this    ! image data structure to copy data from
      integer(int8) , allocatable :: data(:)
     end function image_get_i8

    module function image_get_i16(this) result(data)
      class(image_t), intent(in)  :: this    ! image data structure to copy data from
      integer(int16), allocatable :: data(:)
     end function image_get_i16

    module function image_get_i32(this) result(data)
      class(image_t), intent(in)  :: this    ! image data structure to copy data from
      integer(int32), allocatable :: data(:)
     end function image_get_i32

    module function image_get_i64(this) result(data)
      class(image_t), intent(in)  :: this    ! image data structure to copy data from
      integer(int64), allocatable :: data(:)
     end function image_get_i64

    module function image_get_r32(this) result(data)
      class(image_t), intent(in)  :: this    ! image data structure to copy data from
      real(real32)  , allocatable :: data(:)
     end function image_get_r32

    module function image_get_r64(this) result(data)
      class(image_t), intent(in)  :: this    ! image data structure to copy data from
      real(real64)  , allocatable :: data(:)
     end function image_get_r64

    ! routines to get data as 2d array
    module subroutine image_get2_i8 (this, data2)
      class(image_t), intent(in )              :: this       ! image data structure to copy data from
      integer(int8) , intent(out), allocatable :: data2(:,:) ! array to copy data into
     end subroutine image_get2_i8

    module subroutine image_get2_i16(this, data2)
      class(image_t), intent(in )              :: this       ! image data structure to copy data from
      integer(int16), intent(out), allocatable :: data2(:,:) ! array to copy data into
     end subroutine image_get2_i16

    module subroutine image_get2_i32(this, data2)
      class(image_t), intent(in )              :: this       ! image data structure to copy data from
      integer(int32), intent(out), allocatable :: data2(:,:) ! array to copy data into
     end subroutine image_get2_i32

    module subroutine image_get2_i64(this, data2)
      class(image_t), intent(in )              :: this       ! image data structure to copy data from
      integer(int64), intent(out), allocatable :: data2(:,:) ! array to copy data into
     end subroutine image_get2_i64

    module subroutine image_get2_r32(this, data2)
      class(image_t), intent(in )              :: this       ! image data structure to copy data from
      real(real32)  , intent(out), allocatable :: data2(:,:) ! array to copy data into
     end subroutine image_get2_r32

    module subroutine image_get2_r64(this, data2)
      class(image_t), intent(in )              :: this       ! image data structure to copy data from
      real(real64)  , intent(out), allocatable :: data2(:,:) ! array to copy data into
     end subroutine image_get2_r64

    ! routines to get data as 3d array
    module subroutine image_get3_i8 (this, data3)
      class(image_t), intent(in )              :: this         ! image data structure to copy data from
      integer(int8) , intent(out), allocatable :: data3(:,:,:) ! array to copy data into
    end subroutine image_get3_i8

    module subroutine image_get3_i16(this, data3)
      class(image_t), intent(in )              :: this         ! image data structure to copy data from
      integer(int16), intent(out), allocatable :: data3(:,:,:) ! array to copy data into
    end subroutine image_get3_i16

    module subroutine image_get3_i32(this, data3)
      class(image_t), intent(in )              :: this         ! image data structure to copy data from
      integer(int32), intent(out), allocatable :: data3(:,:,:) ! array to copy data into
    end subroutine image_get3_i32

    module subroutine image_get3_i64(this, data3)
      class(image_t), intent(in )              :: this         ! image data structure to copy data from
      integer(int64), intent(out), allocatable :: data3(:,:,:) ! array to copy data into
    end subroutine image_get3_i64

    module subroutine image_get3_r32(this, data3)
      class(image_t), intent(in )              :: this         ! image data structure to copy data from
      real(real32)  , intent(out), allocatable :: data3(:,:,:) ! array to copy data into
    end subroutine image_get3_r32

    module subroutine image_get3_r64(this, data3)
      class(image_t), intent(in )              :: this         ! image data structure to copy data from
      real(real64)  , intent(out), allocatable :: data3(:,:,:) ! array to copy data into
    end subroutine image_get3_r64

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!         defined in image_io (to break circular references)         !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    module function image_get_extension(filename) result(ext)
      character(len=*) , intent(in) :: filename ! filename to parse
      integer                       :: ext      ! one of im_ext_*
    end function image_get_extension

    module subroutine image_flatten_rgba(this)
      type(image_t), intent(inout) :: this
    end subroutine image_flatten_rgba

    module function image_read(filename, iostat, iomsg) result(im)
      character(len=*), intent(in )           :: filename ! filename to read image from
      integer         , intent(out), optional :: iostat   ! error flag
      character(len=*), intent(out), optional :: iomsg    ! error message
      type(image_t)                           :: im
    end function image_read

    module subroutine image_write(this, filename, iostat, iomsg)
      class(image_t)  , intent(in )           :: this     ! image to write to file
      character(len=*), intent(in )           :: filename ! filename to read image from
      integer         , intent(out), optional :: iostat   ! error flag
      character(len=*), intent(out), optional :: iomsg    ! error message
    end subroutine image_write
  end interface
end module image

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                        image_base submodule                        !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
submodule (image) image_base
  implicit none
contains
  ! @brief: clean up memory alloted by an image_t
  ! @param this: image_t entry to clean up
  ! @signature: subroutine image_destroy(this)
  module procedure image_destroy
    if(allocated(this%dims)) deallocate(this%dims)
    if(allocated(this%buff)) deallocate(this%buff)
  end procedure image_destroy

  ! @brief: initialize an image_t
  ! @param this: image_t initialize
  ! @signature: subroutine image_clear(this)
  module procedure image_clear
    if(allocated(this%dims)) deallocate(this%dims)
    this%samplesPerPixel = 0
    this%pixelType = pix_unk
    if(allocated(this%buff)) deallocate(this%buff)
    this%unsigned = .false.
  end procedure image_clear

  ! @brief: determine the number of pixels in an image
  ! @param this: image_t to compute pixels in
  ! @return: number of pixels
  ! @signature: function image_size(this) result(pixels)
  module procedure image_size
    integer :: i
    if(allocated(this%buff)) then
      pixels = 1
      do i = 1, size(this%dims)
        pixels = pixels * this%dims(i)
      end do
    else
      pixels = 0
    end if
  end procedure image_size

  ! @brief: determine if an image_t contains data
  ! @param this: image_t to check
  ! @return: true/false if the image doesn't / does contain data
  ! @signature: function image_empty(this) result(empty)
  module procedure image_empty
    empty = (.not.allocated(this%dims)).or.(.not.allocated(this%buff))
  end procedure image_empty

  ! functions to build image from 2d array

  ! @brief: create an image_t from a 2d int8 array
  ! @param data2: array to create image from
  ! @return: created image
  ! @signature: function image_build_i8 (data2) result(im)
  module procedure image_build_i8
    allocate(im%dims(2))
    allocate(im%buff(1*size(data2)))
    im%dims            = shape(data2)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i8
    im%buff            = transfer(data2, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i8

  ! @brief: create an image_t from a 2d int16 array
  ! @param data2: array to create image from
  ! @return: created image
  ! @signature: function image_build_i16(data2) result(im)
  module procedure image_build_i16
    allocate(im%dims(2))
    allocate(im%buff(2*size(data2)))
    im%dims            = shape(data2)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i16
    im%buff            = transfer(data2, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i16

  ! @brief: create an image_t from a 2d int32 array
  ! @param data2: array to create image from
  ! @return: created image
  ! @signature: function image_build_i32(data2) result(im)
  module procedure image_build_i32
    allocate(im%dims(2))
    allocate(im%buff(4*size(data2)))
    im%dims            = shape(data2)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i32
    im%buff            = transfer(data2, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i32

  ! @brief: create an image_t from a 2d int64 array
  ! @param data2: array to create image from
  ! @return: created image
  ! @signature: function image_build_i64(data2) result(im)
  module procedure image_build_i64
    allocate(im%dims(2))
    allocate(im%buff(8*size(data2)))
    im%dims            = shape(data2)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i64
    im%buff            = transfer(data2, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i64

  ! @brief: create an image_t from a 2d real32 array
  ! @param data2: array to create image from
  ! @return: created image
  ! @signature: function image_build_r32(data2) result(im)
  module procedure image_build_r32
    allocate(im%dims(2))
    allocate(im%buff(4*size(data2)))
    im%dims            = shape(data2)
    im%samplesPerPixel = 1
    im%pixelType       = pix_r32
    im%buff            = transfer(data2, im%buff)
    im%unsigned        = .false.
  end procedure image_build_r32

  ! @brief: create an image_t from a 2d real64 array
  ! @param data2: array to create image from
  ! @return: created image
  ! @signature: function image_build_r64(data2) result(im)
  module procedure image_build_r64
    allocate(im%dims(2))
    allocate(im%buff(8*size(data2)))
    im%dims            = shape(data2)
    im%samplesPerPixel = 1
    im%pixelType       = pix_r64
    im%buff            = transfer(data2, im%buff)
    im%unsigned        = .false.
  end procedure image_build_r64

  ! functions to build image from 3d array

  ! @brief: create an image_t from a 3d int8 array
  ! @param data3: array to create image from
  ! @return: created image
  ! @signature: function image_build_i8_3 (data3) result(im)
  module procedure image_build_i8_3 
    allocate(im%dims(3))
    allocate(im%buff(1*size(data3)))
    im%dims            = shape(data3)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i8
    im%buff            = transfer(data3, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i8_3

  ! @brief: create an image_t from a 3d int16 array
  ! @param data3: array to create image from
  ! @return: created image
  ! @signature: function image_build_i16_3(data3) result(im)
  module procedure image_build_i16_3
    allocate(im%dims(3))
    allocate(im%buff(2*size(data3)))
    im%dims            = shape(data3)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i16
    im%buff            = transfer(data3, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i16_3

  ! @brief: create an image_t from a 3d int32 array
  ! @param data3: array to create image from
  ! @return: created image
  ! @signature: function image_build_i32_3(data3) result(im)
  module procedure image_build_i32_3
    allocate(im%dims(3))
    allocate(im%buff(4*size(data3)))
    im%dims            = shape(data3)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i32
    im%buff            = transfer(data3, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i32_3

  ! @brief: create an image_t from a 3d int64 array
  ! @param data3: array to create image from
  ! @return: created image
  ! @signature: function image_build_i64_3(data3) result(im)
  module procedure image_build_i64_3
    allocate(im%dims(3))
    allocate(im%buff(8*size(data3)))
    im%dims            = shape(data3)
    im%samplesPerPixel = 1
    im%pixelType       = pix_i64
    im%buff            = transfer(data3, im%buff)
    im%unsigned        = .false.
  end procedure image_build_i64_3

  ! @brief: create an image_t from a 3d real32 array
  ! @param data3: array to create image from
  ! @return: created image
  ! @signature: function image_build_r32_3(data3) result(im)
  module procedure image_build_r32_3
    allocate(im%dims(3))
    allocate(im%buff(4*size(data3)))
    im%dims            = shape(data3)
    im%samplesPerPixel = 1
    im%pixelType       = pix_r32
    im%buff            = transfer(data3, im%buff)
    im%unsigned        = .false.
  end procedure image_build_r32_3

  ! @brief: create an image_t from a 3d real64 array
  ! @param data3: array to create image from
  ! @return: created image
  ! @signature: function image_build_r64_3(data3) result(im)
  module procedure image_build_r64_3
    allocate(im%dims(3))
    allocate(im%buff(8*size(data3)))
    im%dims            = shape(data3)
    im%samplesPerPixel = 1
    im%pixelType       = pix_r64
    im%buff            = transfer(data3, im%buff)
    im%unsigned        = .false.
  end procedure image_build_r64_3

  ! functions to get data as appropriate type (casting up if needed)

  ! @brief: create a 1d int8 array from an image_t
  ! @param this: image to extract array from
  ! @return: extracted array
  ! @note: x increments fastest, followed by y (followed by z etc for higher dimension images)
  ! @note: an empty array is returned (allocated to size 0) if the pixel type can't be safely cast to int8
  ! @note: user is responsible for deallocating the created array
  ! @signature: function image_get_i8 (this) result(data)
  module procedure image_get_i8 
    select case(this%pixelType)
      case(pix_i8 )
        allocate(data(this%size()))
        data = transfer(this%buff, data)
      case default
        allocate(data(0))
    end select
  end procedure image_get_i8

  ! @brief: create a 1d int16 array from an image_t
  ! @param this: image to extract array from
  ! @return: extracted array
  ! @note: x increments fastest, followed by y (followed by z etc for higher dimension images)
  ! @note: an empty array is returned (allocated to size 0) if the pixel type can't be safely cast to int16
  ! @note: user is responsible for deallocating the created array
  ! @signature: function image_get_i16(this) result(data)
  module procedure image_get_i16
    select case(this%pixelType)
      case(pix_i8 )
        allocate(data(this%size()))
        data = image_get_i8 (this) ! cast from i8  => i16
        if(this%unsigned) where(data.lt.0) data = data+z'0100' ! handle unsigned => signed
      case(pix_i16)
        allocate(data(this%size()))
        data = transfer(this%buff, data)
      case default
        allocate(data(0))
    end select
  end procedure image_get_i16

  ! @brief: create a 1d int32 array from an image_t
  ! @param this: image to extract array from
  ! @return: extracted array
  ! @note: x increments fastest, followed by y (followed by z etc for higher dimension images)
  ! @note: an empty array is returned (allocated to size 0) if the pixel type can't be safely cast to int32
  ! @note: user is responsible for deallocating the created array
  ! @signature: function image_get_i32(this) result(data)
  module procedure image_get_i32
    select case(this%pixelType)
      case(pix_i8 )
        allocate(data(this%size()))
        data = image_get_i8 (this) ! cast from i8  => i32
        if(this%unsigned) where(data.lt.0) data = data+z'00000100' ! handle unsigned => signed
      case(pix_i16)
        allocate(data(this%size()))
        data = image_get_i16(this) ! cast from i16 => i32
        if(this%unsigned) where(data.lt.0) data = data+z'00010000' ! handle unsigned => signed
      case(pix_i32)
        allocate(data(this%size()))
        data = transfer(this%buff, data)
      case default
        allocate(data(0))
    end select
  end procedure image_get_i32

  ! @brief: create a 1d int64 array from an image_t
  ! @param this: image to extract array from
  ! @return: extracted array
  ! @note: x increments fastest, followed by y (followed by z etc for higher dimension images)
  ! @note: an empty array is returned (allocated to size 0) if the pixel type can't be safely cast to int64
  ! @note: user is responsible for deallocating the created array
  ! @signature: function image_get_i64(this) result(data)
  module procedure image_get_i64
    select case(this%pixelType)
      case(pix_i8 )
        allocate(data(this%size()))
        data = image_get_i8 (this) ! cast from i8  => i64
        if(this%unsigned) where(data.lt.0) data = data+z'0000000000000100' ! handle unsigned => signed
      case(pix_i16)
        allocate(data(this%size()))
        data = image_get_i16(this) ! cast from i16 => i64
        if(this%unsigned) where(data.lt.0) data = data+z'0000000000010000' ! handle unsigned => signed
      case(pix_i32)
        allocate(data(this%size()))
        data = image_get_i32(this) ! cast from i32 => i64
        if(this%unsigned) where(data.lt.0) data = data+z'0000000100000000' ! handle unsigned => signed
      case(pix_i64)
        allocate(data(this%size()))
        data = transfer(this%buff, data)
      case default
        allocate(data(0))
    end select
  end procedure image_get_i64

  ! @brief: create a 1d real32 array from an image_t
  ! @param this: image to extract array from
  ! @return: extracted array
  ! @note: x increments fastest, followed by y (followed by z etc for higher dimension images)
  ! @note: an empty array is returned (allocated to size 0) if the pixel type can't be safely cast to real32
  ! @note: user is responsible for deallocating the created array
  ! @signature: function image_get_r32(this) result(data)
  module procedure image_get_r32
    select case(this%pixelType)
      case(pix_i8 )
        allocate(data(this%size()))
        data = image_get_i8 (this) ! cast from i8  => r32
        if(this%unsigned) where(data.lt.0) data = data+z'00000100' ! handle unsigned => signed
      case(pix_i16)
        allocate(data(this%size()))
        data = image_get_i16(this) ! cast from i16 => r32
        if(this%unsigned) where(data.lt.0) data = data+z'00010000' ! handle unsigned => signed
      case(pix_r32)
        allocate(data(this%size()))
        data = transfer(this%buff, data)
      case default
        allocate(data(0))
    end select
  end procedure image_get_r32

  ! @brief: create a 1d real64 array from an image_t
  ! @param this: image to extract array from
  ! @return: extracted array
  ! @note: x increments fastest, followed by y (followed by z etc for higher dimension images)
  ! @note: an empty array is returned (allocated to size 0) if the pixel type can't be safely cast to real64
  ! @note: user is responsible for deallocating the created array
  ! @signature: function image_get_r64(this) result(data)
  module procedure image_get_r64
    select case(this%pixelType)
      case(pix_i8 )
        allocate(data(this%size()))
        data = image_get_i8 (this) ! cast from i8  => r64
        if(this%unsigned) where(data.lt.0) data = data+z'0000000000010000' ! handle unsigned => signed
      case(pix_i16)
        allocate(data(this%size()))
        data = image_get_i16(this) ! cast from i16 => r64
        if(this%unsigned) where(data.lt.0) data = data+z'0000000100000000' ! handle unsigned => signed
      case(pix_i32)
        allocate(data(this%size()))
        data = image_get_i32(this) ! cast from i32 => r64
        ! if(this%unsigned) where(data.lt.0) data = data+real(4294967296,real64) ! handle unsigned => signed
      case(pix_r32)
        allocate(data(this%size()))
        data = image_get_r32(this) ! cast from r32 => r64
      case(pix_r64)
        allocate(data(this%size()))
        data = transfer(this%buff, data)
      case default
        allocate(data(0))
    end select
  end procedure image_get_r64

  ! routines to get data as 2d array

  ! @brief: create a 2d int8 array from an image_t
  ! @param this: image to extract array from
  ! @param data2: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int8
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get2_i8 (this, data2)
  module procedure image_get2_i8 
    integer(int8) , allocatable :: data (:)
    if(allocated(data2)) deallocate(data2)
    if(size(this%dims).eq.2) then
      data = image_get_i8 (this)
      if(size(data).ne.0) then
        allocate(data2( this%dims(1), this%dims(2) ))
        data2 = reshape(data, shape(data2))
      end if
    end if
   end procedure image_get2_i8

  ! @brief: create a 2d int16 array from an image_t
  ! @param this: image to extract array from
  ! @param data2: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int16
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get2_i16(this, data2)
  module procedure image_get2_i16
    integer(int16), allocatable :: data (:)
    if(allocated(data2)) deallocate(data2)
    if(size(this%dims).eq.2) then
      data = image_get_i16(this)
      if(size(data).ne.0) then
        allocate(data2( this%dims(1), this%dims(2) ))
        data2 = reshape(data, shape(data2))
      end if
    end if
   end procedure image_get2_i16

  ! @brief: create a 2d int32 array from an image_t
  ! @param this: image to extract array from
  ! @param data2: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int32
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get2_i32(this, data2)
  module procedure image_get2_i32
    integer(int32), allocatable :: data (:)
    if(allocated(data2)) deallocate(data2)
    if(size(this%dims).eq.2) then
      data = image_get_i32(this)
      if(size(data).ne.0) then
        allocate(data2( this%dims(1), this%dims(2) ))
        data2 = reshape(data, shape(data2))
      end if
    end if
   end procedure image_get2_i32

  ! @brief: create a 2d int64 array from an image_t
  ! @param this: image to extract array from
  ! @param data2: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int64
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get2_i64(this, data2)
  module procedure image_get2_i64
    integer(int64), allocatable :: data (:)
    if(allocated(data2)) deallocate(data2)
    if(size(this%dims).eq.2) then
      data = image_get_i64(this)
      if(size(data).ne.0) then
        allocate(data2( this%dims(1), this%dims(2) ))
        data2 = reshape(data, shape(data2))
      end if
    end if
   end procedure image_get2_i64

  ! @brief: create a 2d real32 array from an image_t
  ! @param this: image to extract array from
  ! @param data2: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to real32
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get2_r32(this, data2)
  module procedure image_get2_r32
    real(real32)  , allocatable :: data (:)
    if(allocated(data2)) deallocate(data2)
    if(size(this%dims).eq.2) then
      data = image_get_r32(this)
      if(size(data).ne.0) then
        allocate(data2( this%dims(1), this%dims(2) ))
        data2 = reshape(data, shape(data2))
      end if
    end if
   end procedure image_get2_r32

  ! @brief: create a 2d real64 array from an image_t
  ! @param this: image to extract array from
  ! @param data2: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to real64
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get2_r64(this, data2)
  module procedure image_get2_r64
    real(real64)  , allocatable :: data (:)
    if(allocated(data2)) deallocate(data2)
    if(size(this%dims).eq.2) then
      data = image_get_r64(this)
      if(size(data).ne.0) then
        allocate(data2( this%dims(1), this%dims(2) ))
        data2 = reshape(data, shape(data2))
      end if
    end if
   end procedure image_get2_r64

  ! routines to get data as 3d array

  ! @brief: create a 3d int8 array from an image_t
  ! @param this: image to extract array from
  ! @param data3: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int8
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get3_i8 (this, data3)
  module procedure image_get3_i8 
    integer(int8) , allocatable :: data(:)
    if(size(this%dims).eq.2.or.size(this%dims).eq.3) then
      data = image_get_i8 (this)
      if(size(data).ne.0) then
        if(size(this%dims).eq.2) then
          allocate(data3( this%dims(1), this%dims(2), 1            ))
          data3 = reshape(data, shape(data3))
        else
          allocate(data3( this%dims(1), this%dims(2), this%dims(3) ))
          data3 = reshape(data, shape(data3))
        endif
      end if
    end if
  end procedure image_get3_i8

  ! @brief: create a 3d int16 array from an image_t
  ! @param this: image to extract array from
  ! @param data3: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int16
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get3_i16(this, data3)
  module procedure image_get3_i16
    integer(int16), allocatable :: data(:)
    if(size(this%dims).eq.2.or.size(this%dims).eq.3) then
      data = image_get_i16(this)
      if(size(data).ne.0) then
        if(size(this%dims).eq.2) then
          allocate(data3( this%dims(1), this%dims(2), 1            ))
          data3 = reshape(data, shape(data3))
        else
          allocate(data3( this%dims(1), this%dims(2), this%dims(3) ))
          data3 = reshape(data, shape(data3))
        endif
      end if
    end if
  end procedure image_get3_i16

  ! @brief: create a 3d int32 array from an image_t
  ! @param this: image to extract array from
  ! @param data3: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int32
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get3_i32(this, data3)
  module procedure image_get3_i32
    integer(int32), allocatable :: data(:)
    if(size(this%dims).eq.2.or.size(this%dims).eq.3) then
      data = image_get_i32(this)
      if(size(data).ne.0) then
        if(size(this%dims).eq.2) then
          allocate(data3( this%dims(1), this%dims(2), 1            ))
          data3 = reshape(data, shape(data3))
        else
          allocate(data3( this%dims(1), this%dims(2), this%dims(3) ))
          data3 = reshape(data, shape(data3))
        endif
      end if
    end if
  end procedure image_get3_i32

  ! @brief: create a 3d int64 array from an image_t
  ! @param this: image to extract array from
  ! @param data3: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to int64
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get3_i64(this, data3)
  module procedure image_get3_i64
    integer(int64), allocatable :: data(:)
    if(size(this%dims).eq.2.or.size(this%dims).eq.3) then
      data = image_get_i64(this)
      if(size(data).ne.0) then
        if(size(this%dims).eq.2) then
          allocate(data3( this%dims(1), this%dims(2), 1            ))
          data3 = reshape(data, shape(data3))
        else
          allocate(data3( this%dims(1), this%dims(2), this%dims(3) ))
          data3 = reshape(data, shape(data3))
        endif
      end if
    end if
  end procedure image_get3_i64

  ! @brief: create a 3d real32 array from an image_t
  ! @param this: image to extract array from
  ! @param data3: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to real32
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get3_r32(this, data3)
  module procedure image_get3_r32
    real(real32)  , allocatable :: data(:)
    if(size(this%dims).eq.2.or.size(this%dims).eq.3) then
      data = image_get_r32(this)
      if(size(data).ne.0) then
        if(size(this%dims).eq.2) then
          allocate(data3( this%dims(1), this%dims(2), 1            ))
          data3 = reshape(data, shape(data3))
        else
          allocate(data3( this%dims(1), this%dims(2), this%dims(3) ))
          data3 = reshape(data, shape(data3))
        endif
      end if
    end if
  end procedure image_get3_r32

  ! @brief: create a 3d real64 array from an image_t
  ! @param this: image to extract array from
  ! @param data3: array to extract image into
  ! @return: extracted array
  ! @note: an empty array is returned (not allocated) if the pixel type can't be safely cast to real64
  ! @note: user is responsible for deallocating the created array
  ! @signature: subroutine image_get3_r64(this, data3)
  module procedure image_get3_r64
    real(real64)  , allocatable :: data(:)
    if(size(this%dims).eq.2.or.size(this%dims).eq.3) then
      data = image_get_r64(this)
      if(size(data).ne.0) then
        if(size(this%dims).eq.2) then
          allocate(data3( this%dims(1), this%dims(2), 1            ))
          data3 = reshape(data, shape(data3))
        else
          allocate(data3( this%dims(1), this%dims(2), this%dims(3) ))
          data3 = reshape(data, shape(data3))
        endif
      end if
    end if
  end procedure image_get3_r64
end submodule image_base
