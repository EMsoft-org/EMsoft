!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (c) 2018-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:bmp.f90
!-----------------------------------------------------------------------
!
! module: bmp
!
!> @author: W.C. Lenthe, Carnegie Mellon University
!
!> @brief: io routines for bitmap files
!
!> @note: should be used through the image_t object instead of directly
!
!> @date: 03/14/18 WCL 1.0 original
!
!-----------------------------------------------------------------------

module bmp
  use, intrinsic :: iso_fortran_env ! fixed width integers
  use, intrinsic :: iso_c_binding   ! enum and packed derived types
  use image
  implicit none

  private
  public :: bmp_read, bmp_write ! only expose the read/write methods

  ! type to hold bitmap file header (14 bytes)
  type, bind(c) :: bmp_header ! bind(c) ensures memory packing
    integer(int16) :: signature  ! magic bytes
    integer(int32) :: size       ! file size in bytes
    integer(int16) :: res1, res2 ! 4 reserved bytes as 2 shorts
    integer(int32) :: offset     ! start of image data
  end type bmp_header

  ! type to hold bitmap information header (12 to 124 bytes)
  ! there are 8 versions but only subsets of BITMAPV5HEADER are used in practice
  type, bind(c) :: bmp_info ! bind(c) ensures memory packing
    ! modified BITMAPCOREHEADER (16 bytes) (actual BITMAPCOREHEADER is 12 bytes)
    integer(int32) :: size        ! size of this structrue in bytes
    integer(int32) :: width       ! bitmap width in pixels  ! only int16 in bitmapcoreheader only
    integer(int32) :: height      ! bitmap height in pixels ! only int16 in bitmapcoreheader only
    integer(int16) :: planes      ! must be 1
    integer(int16) :: bitCount    ! bits per pixel (1, 4, 8, or 24)

    ! BITMAPINFOHEADER (40 bytes) adds these fields and extends bitCount to include (0, 16, and 32)
    integer(int32) :: compression ! type of compression (0:none,1:8bit rle,2:4bit rle,3:indexed,4:jpg,5:png)
    integer(int32) :: sizeImage   ! size of image in bytes
    integer(int32) :: xRes        ! x resoultion in pixels per meter
    integer(int32) :: yRes        ! y resoultion in pixels per meter
    integer(int32) :: colorsUsed  ! number of lut values used
    integer(int32) :: colorsImprt ! number of lut values needed to display the image

    ! BITMAPV4HEADER (108 bytes) adds these fields
    integer(int32) :: redMask    ! red   bits in rgb image
    integer(int32) :: greenMask  ! green bits in rgb image
    integer(int32) :: blueMask   ! blue  bits in rgb image
    integer(int32) :: alphaMask  ! alpha bits in rgba image
    integer(int32) :: colorSpace ! color space (flag ofor if cie enpoints are given)
    integer(int32) :: redX       ! x coordinate of red in cie
    integer(int32) :: redY       ! y '                      '
    integer(int32) :: redZ       ! z '                      '
    integer(int32) :: greenX     ! '
    integer(int32) :: greenY     !                green
    integer(int32) :: greenZ     !                          '
    integer(int32) :: blueX      ! '
    integer(int32) :: blueY      !                 blue
    integer(int32) :: blueZ      !                          '
    integer(int32) :: gammaRed   ! red   gamma curve value
    integer(int32) :: gammaGreen ! green gamma curve value
    integer(int32) :: gammaBlue  ! blue  gamma curve value

    ! BITMAPV5HEADER (124 bytes) adds these fields and support for additional colorSpace types
    integer(int32) :: intent      ! rendering intent
    integer(int32) :: profileData ! offset in bytes from beginning of header to start of profile data
    integer(int32) :: profileSize ! size in byets of profile data
    integer(int32) :: reserved    ! should be 0
  end type bmp_info

contains

  ! @brief: read a bitmap file into an image_t object
  ! @param filename: name of bitmap file to open
  ! @param iostat: error code (0 on success)
  ! @param iomsg: error message (only filled if iostat.ne.0)
  ! @return: image contained in file (as image_t)
  function bmp_read(filename, iostat, iomsg) result(im)
!DEC$ ATTRIBUTES DLLEXPORT :: bmp_read
    character(len=*), intent(in ) :: filename ! name of bitmap file to read
    integer         , intent(out) :: iostat   ! error flag
    character(len=*), intent(out) :: iomsg    ! error message
    type(image_t)                 :: im       ! image data structure to hold result
    integer                       :: i, unit, rowBytes, fileRowBytes, offset
    type(bmp_header)              :: header
    type(bmp_info)                :: info
    integer(int32)                :: size
    integer(int8)                 :: buff(124)
    integer(int8)   , allocatable :: rowBuff(:)
    logical                       :: isGray
    character(len=2)              :: magicBytes

    ! open file and read header (header components need to be read individually for ifort)
    inquire(file=filename, size=size) ! get size of file in bytes
    open(newunit = unit, file = filename, status = 'old', access = 'stream') ! open file for reading in stream mode
    read(unit, iostat=iostat, iomsg=iomsg) magicBytes
    if(iostat.ne.0) return
    read(unit, iostat=iostat, iomsg=iomsg) header%size
    if(iostat.ne.0) return
    read(unit, iostat=iostat, iomsg=iomsg) header%res1
    if(iostat.ne.0) return
    read(unit, iostat=iostat, iomsg=iomsg) header%res2
    if(iostat.ne.0) return
    read(unit, iostat=iostat, iomsg=iomsg) header%offset
    if(iostat.ne.0) return

    ! check if file is valid ('BM' signature and file size matches size listed in header)
    if(magicBytes.ne.'BM'.or.size.ne.header%size) then
      iostat = 1
      iomsg = "\'" // trim(filename) // "\' is not a bitmap file"
      return
    endif

    ! bitmaps actually use unsigned integers, check for overflow in image data offset (this is almost impossible)
    if(header%offset.lt.0) then
      iostat = 1
      iomsg = "signed/unsigned overflow reading bmp file \'" // trim(filename) // "\'"
      return
    endif

    ! get size of header and read type base on size
    buff = 0
    read(unit, iostat=iostat, iomsg=iomsg) size
    if(size.eq.12.or.size.eq.40.or.size.eq.108.or.size.eq.124) then ! BITMAPCOREHEADER,BITMAPINFOHEADER,BITMAPV4HEADER, or BITMAPV5HEADER
      rewind(unit) ! go back to file start
      read(unit, pos=15, iostat=iostat, iomsg=iomsg) buff(1:size) ! read into buffer
    else
      iostat = 1
      iomsg = "unsupported bitmap info size in \'" // trim(filename) // "\'"
      return
    endif

    ! convert buffer to header data structure and check for problems
    if(size.eq.12) then
      info%size     = transfer(buff( 1: 4), info%size    )
      info%width    = transfer(buff( 5: 6), info%planes  ) ! 16 instead of 32 bit
      info%height   = transfer(buff( 7: 8), info%planes  ) ! 16 instead of 32 bit
      info%planes   = transfer(buff( 9:10), info%planes  )
      info%bitCount = transfer(buff(11:12), info%bitCount)
    else
      info = transfer(buff, info)
    endif

    ! check for valid values (only simple bitmaps are supported)
    if(info%planes.ne.1) then ! analogous to tiff directories but not used in practice
      iostat = 1
      iomsg = "unsupported dimensions in \'" // trim(filename) // "\'"
      return
    endif
    if(info%compression.ne.0) then ! compressed bitmaps aren't supported
      iostat = 1
      iomsg = "unsupported bitmap compression in \'" // trim(filename) // "\'"
      return
    endif
    if(info%width.lt.0.or.info%height.lt.0.or.info%sizeImage.lt.0) then ! this is very unlikely but possible (> 2^31)
      iostat = 1
      iomsg = "signed/unsigned overflow reading bmp info in \'" // trim(filename) // "\'"
      return
    endif

    ! determine pixel type
    im%pixelType = pix_unk ! initially the pixel type is unknown
    select case(info%bitCount)
      case(8) ! 8 bit gray
        im%pixelType = pix_i8
        im%samplesPerPixel = 1
        rowBytes = 1 ! bytes per sample
      case(24) ! 8 bit rgb
        im%pixelType = pix_i8
        im%samplesPerPixel = 3
        rowBytes = 1 ! bytes per sample
      case(32) ! 8 bit rgba
        im%pixelType = pix_i8
        im%samplesPerPixel = 4
        rowBytes = 1 ! bytes per sample
    end select

    ! make sure we found a suitable pixel type
    if(im%pixelType.eq.pix_unk) then
      iostat = 1
      iomsg = "unsupported bitmap pixel type in \'" // trim(filename) // "\'"
      return
    endif

    ! allocate image buffer
    allocate(im%dims(2))
    im%dims(1) = info%width
    im%dims(2) = info%height
    rowBytes = rowBytes * im%samplesPerPixel * im%dims(1)
    allocate(im%buff(rowBytes*im%dims(2)))

    ! compute size of rows on disk (each row is padded to a multiple of 32 bits)
    fileRowBytes = 4 - modulo(rowBytes, 4)
    if(fileRowBytes.eq.4) fileRowBytes = 0
    fileRowBytes = fileRowBytes + rowBytes
    allocate(rowBuff(fileRowBytes))

    ! read image from file row by row
    do i = 1, im%dims(2)
      read(unit, pos=header%offset+(i-1)*fileRowBytes+1, iostat=iostat, iomsg=iomsg) rowBuff
      if(iostat.ne.0) return ! make sure we haven't run out of file or encountered another error
      offset = (im%dims(2)-i) * rowBytes ! rows are stored from bottom --> top
      im%buff(offset+1:offset+rowBytes) = rowBuff(1:rowBytes) ! copy row to image buffer
    enddo

    ! clean up
    deallocate(rowBuff)
  end function bmp_read

  ! @brief: write a bitmap file from an image_t object
  ! @param filename: name of file to write to
  ! @param im: image to write to file (as image_t)
  ! @param iostat: error code (0 on success)
  ! @param iomsg: error message (only filled if iostat.ne.0)
  subroutine bmp_write(filename, im, iostat, iomsg)
!DEC$ ATTRIBUTES DLLEXPORT :: bmp_write
    character(len=*), intent(in ) :: filename ! name of bitmap file to read
    integer         , intent(out) :: iostat   ! error flag
    character(len=*), intent(out) :: iomsg    ! error message
    type(image_t)   , intent(in ) :: im       ! image data structure to hold result
    integer                       :: i, unit, rowBytes, fileRowBytes, offset
    type(bmp_header)              :: header
    type(bmp_info)                :: info
    integer(int8)   , allocatable :: padBytes(:)
    integer(int8)                 :: infoBuff(40)
    integer(int8)                 :: palette(4*256)

    ! check that image has supported dimensions (2d only)
    if(size(im%dims).ne.2) then
      iostat = 1
      iomsg = "bitmap only supports 2d images"
      return
    endif

    ! check that image has a supported pixel type (8 bit only)
    if(im%pixelType.ne.pix_i8) then
      iostat = 1
      iomsg = "bitmap only supports 8 bit images"
      return
    endif

    ! check that image has a supported pixel type (grayscale, color, or color + alpha)
    if(.not.(im%samplesPerPixel.eq.1.or.&
             im%samplesPerPixel.eq.3.or.&
             im%samplesPerPixel.eq.4)) then
      iostat = 1
      iomsg = "bitmap only supports scalar, rgb, or rgba images"
      return
    endif

    ! compute padded row size (bitmap rows are padded to multiple of 32 bits)
    rowBytes = im%dims(1) * im%samplesPerPixel
    fileRowBytes = 4 - modulo(rowBytes, 4)
    if(fileRowBytes.eq.4) fileRowBytes = 0
    fileRowBytes = fileRowBytes + rowBytes
    allocate(padBytes(fileRowBytes-rowBytes))
    padBytes = 0

    ! fill header structure
    header%signature = 19778 ! 'BM'
    header%size      = 14 + 40 + fileRowBytes * im%dims(2) ! size of file = header + info + palette + data
    header%res1      = 0 ! reserved (always 0)
    header%res2      = 0 ! reserved (always 0)
    header%offset    = 14 + 40 ! 40 byte header (BITMAPCOREHEADER)

    ! fill info header (40 byte BITMAPCOREHEADER is most widely supported type)
    info%size        = 40
    info%width       = im%dims(1)
    info%height      = im%dims(2)
    info%planes      = 1
    info%bitCount    = 8 * im%samplesPerPixel
    info%compression = 0
    info%sizeImage   = im%dims(1) * im%dims(2) * im%samplesPerPixel
    info%xRes        = 0
    info%yRes        = 0
    info%colorsUsed  = 0 ! 0 => 2^n inferred
    info%colorsImprt = 0
    infoBuff = transfer(info, infoBuff) ! transfer required bytes to buffer

    ! build pallete for grayscale images (shouldn't be required but some software (e.g. imagej) wants it)
    if(im%samplesPerPixel.eq.1) then
      do i = 0, 255 ! 8 bit lut
        palette(4*i+1) = i
        palette(4*i+2) = i
        palette(4*i+3) = i
        palette(4*i+4) = 0
      enddo
      ! update file size and image data offset
      header%size   = header%size   + size(palette)
      header%offset = header%offset + size(palette)
    endif

    ! open file and write headers (header components need to be written individually for ifort)
    open(newunit = unit, file = filename, access = 'stream')
    write(unit, iostat=iostat, iomsg=iomsg) 'B'
    if(iostat.ne.0) return
    write(unit, iostat=iostat, iomsg=iomsg) 'M'
    if(iostat.ne.0) return
    write(unit, iostat=iostat, iomsg=iomsg) header%size
    if(iostat.ne.0) return
    write(unit, iostat=iostat, iomsg=iomsg) header%res1
    if(iostat.ne.0) return
    write(unit, iostat=iostat, iomsg=iomsg) header%res2
    if(iostat.ne.0) return
    write(unit, iostat=iostat, iomsg=iomsg) header%offset
    if(iostat.ne.0) return
    write(unit, iostat=iostat, iomsg=iomsg) infoBuff
    if(iostat.ne.0) return

    ! write palette if needed
    if(im%samplesPerPixel.eq.1) write(unit, iostat=iostat, iomsg=iomsg) palette
    if(iostat.ne.0) return

    ! write data row by row
    do i = 1, im%dims(2)
      offset = (im%dims(2)-i) * rowBytes + 1 ! rows written from bottom to top
      write(unit, iostat=iostat, iomsg=iomsg) im%buff(offset:offset+rowBytes-1)
      if(iostat.ne.0) return
      write(unit, iostat=iostat, iomsg=iomsg) padBytes
      if(iostat.ne.0) return
    enddo
    close(unit)

    ! clean up
    deallocate(padBytes)
  end subroutine bmp_write
end module bmp
