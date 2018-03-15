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
! EMsoft:tif.f90
!-----------------------------------------------------------------------
!
! module: bmp
!
!> @author: W.C. Lenthe, Carnegie Mellon University
!
!> @brief: types to hold and read/write TIFF files
!
!> @note: should be used through the image_t object instead of directly
!
!> @date: 03/14/18 WCL 1.0 original
!
!-----------------------------------------------------------------------

module tif
  use, intrinsic :: iso_fortran_env ! fixed width integers
  use lzw   ! TIFF files can be compressed, currently only LZW (the most common algorithm) is supported
  use image ! read to / write from shared image_t class
  implicit none
  private
  public :: tif_t ! only expose the tiff object

  ! type to hold a single TIFF image file directory (ifd) entry
  type entry
    integer(int16)             :: tag          ! TIFF tag (one of tif tags enumerated below)
    integer(int16)             :: type         ! TIFF data type (one of tif types enumerated below)
    integer(int32)             :: valueCount   ! number of values
    integer(int8), allocatable :: valueBuff(:) ! buffer to hold entry data
  contains
    final     ::                entry_destroy
    procedure :: read        => entry_read
    procedure :: write       => entry_write
    procedure ::                entry_from_short , entry_from_long, entry_from_shorts, entry_from_longs
    generic   :: build       => entry_from_short , entry_from_long, entry_from_shorts, entry_from_longs
  end type entry

  ! type to hold a single TIFF (ifd)
  type ifd
    ! baseline TIFF values, these are all actually unsigned
    integer(int16)                :: compression, photometricInterpretation, threshholding, cellWidth, cellLength, fillOrder, &
                                     orientation, samplesPerPixel, planarConfiguration, grayResponseUnit, resolutionUnit, predictor
    integer(int32)                :: newSubfileType, subfileType, imageWidth, imageLength, rowsPerStrip
    integer(int16)  , allocatable :: bitsPerSample(:), minSampleValue(:), maxSampleValue(:),&
                                     grayResponseCurve(:), colorMap(:), extraSamples(:), sampleFormat(:)
    integer(int32)  , allocatable :: stripOffsets(:), stripByteCounts(:), freeOffsets(:), freeByteCounts(:)
    character(len=:), allocatable :: imageDescription, make, model, software, dateTime, &
                                     artist, hostComputer, copyright!datetime as YYYY:MM:DD HH:MM:SS, others unspecified
    integer(int32)                :: xResolution(2), yResolution(2) ! real number as numerator / denominator pair
    integer(int8)   , allocatable :: data(:) ! image data buffer
    type(entry)     , allocatable :: entries(:)
    logical                       :: configured = .false.
  contains
    final     ::                ifd_destroy
    procedure :: init        => ifd_init
    procedure :: readHeader  => ifd_read_header
    procedure :: readData    => ifd_read_image_data
    procedure :: writeHeader => ifd_write_header
    procedure :: size        => ifd_size
  end type ifd

  ! type to hold a TIFF image
  type tif_t
    type(ifd), allocatable :: directories(:)
  contains
    final     ::              tif_destroy

    ! read/write TIFF files (filename, iostat, iomsg)
    procedure :: read      => tif_read
    procedure :: write     => tif_write

    ! convert to/from image_t
    procedure :: getImage  => tif_get_image
    procedure :: fromImage => tif_from_image
  end type tif_t

  ! baseline TIFF tags
  integer(int16), parameter :: NewSubfileType            = int(z'00FE', kind=int16) ! A general indication of the kind of data contained in this
  integer(int16), parameter :: SubfileType               = int(Z'00FF', kind=int16) ! A general indication of the kind of data contained in this
  integer(int16), parameter :: ImageWidth                = int(Z'0100', kind=int16) ! The number of columns in the image, i.e., the number of pi
  integer(int16), parameter :: ImageLength               = int(Z'0101', kind=int16) ! The number of rows of pixels in the image.
  integer(int16), parameter :: BitsPerSample             = int(Z'0102', kind=int16) ! Number of bits per component.
  integer(int16), parameter :: Compression               = int(Z'0103', kind=int16) ! Compression scheme used on the image data.
  integer(int16), parameter :: PhotometricInterpretation = int(Z'0106', kind=int16) ! The color space of the image data.
  integer(int16), parameter :: Threshholding             = int(Z'0107', kind=int16) ! For black and white TIFF files that represent shades of gr
  integer(int16), parameter :: CellWidth                 = int(Z'0108', kind=int16) ! The width of the dithering or halftoning matrix used to cr
  integer(int16), parameter :: CellLength                = int(Z'0109', kind=int16) ! The length of the dithering or halftoning matrix used to c
  integer(int16), parameter :: FillOrder                 = int(Z'010A', kind=int16) ! The logical order of bits within a byte.
  integer(int16), parameter :: ImageDescription          = int(Z'010E', kind=int16) ! A string that describes the subject of the image.
  integer(int16), parameter :: Make                      = int(Z'010F', kind=int16) ! The scanner manufacturer.
  integer(int16), parameter :: Model                     = int(Z'0110', kind=int16) ! The scanner model name or number.
  integer(int16), parameter :: StripOffsets              = int(Z'0111', kind=int16) ! For each strip, the byte offset of that strip.
  integer(int16), parameter :: Orientation               = int(Z'0112', kind=int16) ! The orientation of the image with respect to the rows and 
  integer(int16), parameter :: SamplesPerPixel           = int(Z'0115', kind=int16) ! The number of components per pixel.
  integer(int16), parameter :: RowsPerStrip              = int(Z'0116', kind=int16) ! The number of rows per strip.
  integer(int16), parameter :: StripByteCounts           = int(Z'0117', kind=int16) ! For each strip, the number of bytes in the strip after com
  integer(int16), parameter :: MinSampleValue            = int(Z'0118', kind=int16) ! The minimum component value used.
  integer(int16), parameter :: MaxSampleValue            = int(Z'0119', kind=int16) ! The maximum component value used.
  integer(int16), parameter :: XResolution               = int(Z'011A', kind=int16) ! The number of pixels per ResolutionUnit in the ImageWidth 
  integer(int16), parameter :: YResolution               = int(Z'011B', kind=int16) ! The number of pixels per ResolutionUnit in the ImageLength
  integer(int16), parameter :: PlanarConfiguration       = int(Z'011C', kind=int16) ! How the components of each pixel are stored.
  integer(int16), parameter :: FreeOffsets               = int(Z'0120', kind=int16) ! For each string of contiguous unused bytes in a TIFF file,
  integer(int16), parameter :: FreeByteCounts            = int(Z'0121', kind=int16) ! For each string of contiguous unused bytes in a TIFF file,
  integer(int16), parameter :: GrayResponseUnit          = int(Z'0122', kind=int16) ! The precision of the information contained in the GrayResp
  integer(int16), parameter :: GrayResponseCurve         = int(Z'0123', kind=int16) ! For grayscale data, the optical density of each possible p
  integer(int16), parameter :: ResolutionUnit            = int(Z'0128', kind=int16) ! The unit of measurement for XResolution and YResolution.
  integer(int16), parameter :: Software                  = int(Z'0131', kind=int16) ! Name and version number of the software package(s) used to
  integer(int16), parameter :: DateTime                  = int(Z'0132', kind=int16) ! Date and time of image creation.
  integer(int16), parameter :: Artist                    = int(Z'013B', kind=int16) ! Person who created the image.
  integer(int16), parameter :: HostComputer              = int(Z'013C', kind=int16) ! The computer and/or operating system in use at the time of
  integer(int16), parameter :: ColorMap                  = int(Z'0140', kind=int16) ! A color map for palette color images.
  integer(int16), parameter :: ExtraSamples              = int(Z'0152', kind=int16) ! Description of extra components.
  integer(int16), parameter :: Copyright                 =-int(Z'7D68', kind=int16) ! Copyright notice. (actually 0x8298 as uint16)

  ! extension tags
  integer(int16), parameter :: SampleFormat              = int(Z'0153', kind=int16) ! Specifies how to interpret each data sample in a pixel.
  integer(int16), parameter :: Predictor                 = int(Z'013D', kind=int16) ! Specifies how to interpret each data sample in a pixel.

  ! enumeration of tif data types
  integer(int16), parameter :: tiff_byte      = int(Z'0001', kind=int16) ! uint8
  integer(int16), parameter :: tiff_ascii     = int(Z'0002', kind=int16) ! null terminated ascii string (count field includes null terminator but not padding byte if needed)
  integer(int16), parameter :: tiff_short     = int(Z'0003', kind=int16) ! uint16
  integer(int16), parameter :: tiff_long      = int(Z'0004', kind=int16) ! uint32
  integer(int16), parameter :: tiff_rational  = int(Z'0005', kind=int16) ! fractional real number (2 sequential uint32: numerator, denominator)
  integer(int16), parameter :: tiff_sbyte     = int(Z'0006', kind=int16) ! int8
  integer(int16), parameter :: tiff_undefined = int(Z'0007', kind=int16) ! arbitrary 8 bit byte
  integer(int16), parameter :: tiff_sshort    = int(Z'0008', kind=int16) ! int16
  integer(int16), parameter :: tiff_slong     = int(Z'0009', kind=int16) ! int32
  integer(int16), parameter :: tiff_srational = int(Z'000A', kind=int16) ! as rational but with int32_t
  integer(int16), parameter :: tiff_float     = int(Z'000B', kind=int16) ! IEEE float
  integer(int16), parameter :: tiff_double    = int(Z'000C', kind=int16) ! IEEE double

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                         helper functions                           !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! @brief: check if the system is big or little endian
  ! @param iostat: error flag (0 on success)
  ! @param iomessage: error message (filled if iostat.ne.0)
  ! @return: true if big endian, false if little endian
  function sys_big_endian(iostat, iomsg) result(sysBig)
 !DEC$ ATTRIBUTES DLLEXPORT :: sys_big_endian
   integer         , intent(out) :: iostat ! error flag
    character(len=*), intent(out) :: iomsg  ! error message
    logical                       :: sysBig ! true/false for little/big ended system
    integer(int8) , parameter     :: i8x4(4) = [1,2,3,4]
    integer(int32), parameter     :: i32 = 0
    integer(int32), parameter     :: i32x1 = transfer(i8x4, i32)
    iostat = 0
    if(67305985.eq.i32x1) then
      sysBig = .false.
    else if(16909060.eq.i32x1) then
      sysBig = .true.
    else
      iostat = 1
      iomsg = "couldn't determine system endedness"
    endif
  end function sys_big_endian

  ! @brief: map from enumerated tiff data type to size of corresponding fortran type
  ! @param type: one of enumerated tiff data types (e.g. tiff_byte)
  ! @return: width of fortran type in bytes
  pure elemental function type_bytes(type) result(bytes)
 !DEC$ ATTRIBUTES DLLEXPORT :: type_bytes
   integer(int16), intent(in) :: type  ! tiff data type id
    integer                    :: bytes ! size of data type in bytes
    select case(type)
      case(tiff_byte     )
        bytes = 1
      case(tiff_ascii    )
        bytes = 1
      case(tiff_short    )
        bytes = 2
      case(tiff_long     )
        bytes = 4
      case(tiff_rational )
        bytes = 8
      case(tiff_sbyte    )
        bytes = 1
      case(tiff_undefined)
        bytes = 1
      case(tiff_sshort   )
        bytes = 2
      case(tiff_slong    )
        bytes = 4
      case(tiff_srational)
        bytes = 8
      case(tiff_float    )
        bytes = 4
      case(tiff_double   )
        bytes = 8
      case default
        bytes = 1
    end select
  end function type_bytes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                          Entry Functions                           !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! @brief: clean up memory alloted by an entry
  ! @param this: ifd entry to clean up
  subroutine entry_destroy(this)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_destroy
    type(entry), intent(inout) :: this ! ifd entry clean up
    if(allocated(this%valueBuff)) deallocate(this%valueBuff)
  end subroutine entry_destroy

  ! @brief: read an ifd entry from a file
  ! @param this: ifd entry to read data into
  ! @param unit: file handle to read from
  ! @param wrongEndian: flag for files not in native ended-ness
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message
  ! @param parse: flag for if appended data should be read
  subroutine entry_read(this, unit, wrongEndian, iostat, iomsg, parse)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_read
    class(entry)    , intent(inout)           :: this        ! ifd entry to read data into
    integer         , intent(in   )           :: unit        ! file handle to read from
    integer(int8)                             :: buff(12)    ! ifd entry header is always 12 bytes
    logical         , intent(in   )           :: wrongEndian ! flag for files not in native ended-ness
    integer         , intent(out  )           :: iostat      ! error flag
    character(len=*), intent(out  )           :: iomsg       ! error message
    logical         , intent(in   ), optional :: parse       ! flag for if extended data should be read
    integer                                   :: i, typeBytes
    integer(int32)                            :: offset
    logical                                   :: doParse
    doParse = .true.
    if(present(parse)) doParse = parse

    ! read ifd header and byteswap if needed
    read(unit, iostat=iostat, iomsg=iomsg) buff
    if(0.ne.iostat) return
    if(wrongEndian) then
      buff(1:2) = buff(2:1:-1) ! tag is first 2 bytes
      buff(3:4) = buff(4:3:-1) ! data type is first 2 bytes
      buff(5:8) = buff(8:5:-1) ! value count is first 2 bytes
    endif

    ! parse header and allocate storage
    this%tag        = transfer(buff(1:2), this%tag       ) ! tag is first 2 bytes
    this%type       = transfer(buff(3:4), this%type      ) ! type is next 2 bytes
    this%valueCount = transfer(buff(5:8), this%valueCount) ! valueCount is next 4 bytes

    if(this%valueCount.lt.0) then
      iostat = 1
      iomsg = "signed/unsigned overflow reading value count for tif entry"
      return
    endif
    if(allocated(this%valueBuff)) deallocate(this%valueBuff)
    typeBytes = type_bytes(this%type)
    allocate(this%valueBuff(typeBytes * this%valueCount))

    if(size(this%valueBuff).le.4) then ! next 4 bytes is actual data
      this%valueBuff = buff(9:8+size(this%valueBuff))
    else ! next 4 bytes is offset to data
      if(wrongEndian) then
        buff(9:12) = buff(12:9:-1)
      endif
      if(doParse) then
        ! seek to data and read, then return to current position
        inquire(unit, pos=i) ! get current file position
        read(unit, pos=transfer(buff(9:12), this%valueCount)+1, iostat=iostat, iomsg=iomsg) this%valueBuff
        if(0.ne.iostat) return
        read(unit, pos=i-12, iostat=iostat, iomsg=iomsg) buff ! return to end of entry
        if(0.ne.iostat) return
      endif
    endif

    ! byte swap actual data if needed
    if(wrongEndian.and.(typeBytes.gt.1)) then
      do i = 1, this%valueCount
        this%valueBuff(typeBytes*(i-1)+1:typeBytes*i) = this%valueBuff(typeBytes*i:typeBytes*(i-1)+1:-1)
      enddo
    endif
  end subroutine entry_read

  ! @brief: write an ifd entry to a file
  ! @param this: ifd entry to write data from
  ! @param unit: file handle to write to
  ! @param dataOffset: position to write appended data
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message
  ! @param return: bytes of appended data written
  function entry_write(this, unit, dataOffset, iostat, iomsg) result(dataWidth)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_write
    class(entry)    , intent(in ) :: this       ! ifd entry to read data into
    integer         , intent(in ) :: unit       ! file handle to write to
    integer(int32)  , intent(in ) :: dataOffset ! position of to write appended data
    integer         , intent(out) :: iostat     ! error flag
    character(len=*), intent(out) :: iomsg      ! error message
    integer(int32)                :: dataWidth  ! bytes of appended data written
    integer(int8)                 :: buff(12)
    integer(int8)   , parameter   :: i8 = 0
    integer                       :: typeBytes, pos
    typeBytes = type_bytes(this%type)

    ! fill front of buffer
    buff(1:2)  = transfer(this%tag       , buff(1:2) ) ! tag is first 2 bytes
    buff(3:4)  = transfer(this%type      , buff(3:4) ) ! type is next 2 bytes
    buff(5:8)  = transfer(this%valueCount, buff(5:8) ) ! valueCount is next 4 bytes
    buff(9:12) = 0

    ! complete buffer and write appneded data if needed
    inquire(unit, pos=pos) ! get current file position
    if(typeBytes*this%valueCount.le.4) then ! next 4 bytes is actual data
      buff(9:9+size(this%valueBuff)-1) = this%valueBuff
      dataWidth = 0
    else ! next 4 bytes is offset to data
      buff(9:12) = transfer(dataOffset-1, buff(9:12)) ! next 4 bytes is offset to data
      dataWidth = this%valueCount * type_bytes(this%type)
      write(unit, pos=dataOffset, iostat=iostat, iomsg=iomsg) this%valueBuff
      if(0.ne.iostat) return
      if(1.eq.typeBytes.and.(1.eq.modulo(dataWidth, 2))) then
        write(unit, pos=dataOffset+dataWidth, iostat=iostat, iomsg=iomsg) i8 ! maintain 16 bit word boundaries
        if(0.ne.iostat) return
        dataWidth = dataWidth + 1
      endif
    endif

    ! write buffer
    write(unit, pos=pos, iostat=iostat, iomsg=iomsg) buff
  end function entry_write

  ! @brief: build an ifd entry from an int16
  ! @param this: ifd entry to build
  ! @param tag: tiff tag to store
  ! @param value: int16 value
  subroutine entry_from_short(this, tag, value)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_from_short
    class(entry)  , intent(inout)   :: this  ! ifd entry to format
    integer(int16), intent(in   )   :: tag   ! entry tag
    integer(int16), intent(in   )   :: value ! entry value
    if(allocated(this%valueBuff)) deallocate(this%valueBuff)
    allocate(this%valueBuff(2))
    this%tag            = tag
    this%type           = tiff_short
    this%valueCount     = 1
    this%valueBuff = transfer(value, this%valueBuff)
  end subroutine entry_from_short

  ! @brief: build an ifd entry from an int32
  ! @param this: ifd entry to build
  ! @param tag: tiff tag to store
  ! @param value: int32 value
  subroutine entry_from_long(this, tag, value)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_from_long
    class(entry)  , intent(inout)   :: this  ! ifd entry to format
    integer(int16), intent(in   )   :: tag   ! entry tag
    integer(int32), intent(in   )   :: value ! entry value
    if(allocated(this%valueBuff)) deallocate(this%valueBuff)
    allocate(this%valueBuff(4))
    this%tag            = tag
    this%type           = tiff_long
    this%valueCount     = 1
    this%valueBuff = transfer(value, this%valueBuff)
  end subroutine entry_from_long

  ! @brief: build an ifd entry from an array of int16s
  ! @param this: ifd entry to build
  ! @param tag: tiff tag to store
  ! @param value: int16 values
  subroutine entry_from_shorts(this, tag, values)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_from_shorts
    class(entry)  , intent(inout) :: this   ! ifd entry to format
    integer(int16), intent(in   ) :: tag    ! entry tag
    integer(int16), intent(in   ) :: values(:) ! entry values
    if(allocated(this%valueBuff)) deallocate(this%valueBuff)
    allocate(this%valueBuff(2*size(values)))
    this%tag            = tag
    this%type           = tiff_short
    this%valueCount     = size(values)
    this%valueBuff = transfer(values, this%valueBuff)
  end subroutine entry_from_shorts

  ! @brief: build an ifd entry from an array of int32s
  ! @param this: ifd entry to build
  ! @param tag: tiff tag to store
  ! @param value: int32 values
  subroutine entry_from_longs(this, tag, values)
!DEC$ ATTRIBUTES DLLEXPORT :: entry_from_longs
    class(entry)  , intent(inout) :: this   ! ifd entry to format
    integer(int16), intent(in   ) :: tag    ! entry tag
    integer(int32), intent(in   ) :: values(:) ! entry values
    if(allocated(this%valueBuff)) deallocate(this%valueBuff)
    allocate(this%valueBuff(4*size(values)))
    this%tag            = tag
    this%type           = tiff_long
    this%valueCount     = size(values)
    this%valueBuff = transfer(values, this%valueBuff)
  end subroutine entry_from_longs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                        Directory Functions                         !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! @brief: clean up memory alloted by an ifd
  ! @param this: ifd to clean up
  subroutine ifd_destroy(this)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_destroy
    type(ifd), intent(inout) :: this ! ifd to clean up
    if(allocated(this%bitsPerSample    )) deallocate(this%bitsPerSample    )
    if(allocated(this%minSampleValue   )) deallocate(this%minSampleValue   )
    if(allocated(this%maxSampleValue   )) deallocate(this%maxSampleValue   )
    if(allocated(this%grayResponseCurve)) deallocate(this%grayResponseCurve)
    if(allocated(this%colorMap         )) deallocate(this%colorMap         )
    if(allocated(this%extraSamples     )) deallocate(this%extraSamples     )
    if(allocated(this%sampleFormat     )) deallocate(this%sampleFormat     )
    if(allocated(this%stripOffsets     )) deallocate(this%stripOffsets     )
    if(allocated(this%stripByteCounts  )) deallocate(this%stripByteCounts  )
    if(allocated(this%freeOffsets      )) deallocate(this%freeOffsets      )
    if(allocated(this%freeByteCounts   )) deallocate(this%freeByteCounts   )
    if(allocated(this%imageDescription )) deallocate(this%imageDescription )
    if(allocated(this%make             )) deallocate(this%make             )
    if(allocated(this%model            )) deallocate(this%model            )
    if(allocated(this%software         )) deallocate(this%software         )
    if(allocated(this%dateTime         )) deallocate(this%dateTime         )
    if(allocated(this%artist           )) deallocate(this%artist           )
    if(allocated(this%hostComputer     )) deallocate(this%hostComputer     )
    if(allocated(this%copyright        )) deallocate(this%copyright        )
    if(allocated(this%data             )) deallocate(this%data             )
    if(allocated(this%entries          )) deallocate(this%entries          )
  end subroutine ifd_destroy

  ! @brief: initialize an ifd with default values
  ! @param this: ifd to initialize
  subroutine ifd_init(this)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_init
    class(ifd), intent(inout) :: this ! ifd to initialize
    if(allocated(this%minSampleValue)) deallocate(this%minSampleValue)
    if(allocated(this%maxSampleValue)) deallocate(this%maxSampleValue)
    if(allocated(this%sampleFormat  )) deallocate(this%sampleFormat  )
    allocate(this%minSampleValue(1), this%maxSampleValue(1), this%sampleFormat(1))
    this%compression               = 1 ! none
    this%photometricInterpretation = 1 ! black is 0
    this%threshholding             = 1
    this%cellWidth                 = 0
    this%cellLength                = 0
    this%fillOrder                 = 1
    this%orientation               = 1
    this%samplesPerPixel           = 1
    this%planarConfiguration       = 1
    this%resolutionUnit            = 1
    this%predictor                 = 1
    this%newSubfileType            = 0
    this%subfileType               = 0
    this%imageWidth                = 0
    this%imageLength               = 0
    this%rowsPerStrip              = huge(rowsPerStrip)
    this%minSampleValue(1)         = 0
    this%maxSampleValue(1)         = huge(maxSampleValue)
    this%sampleFormat(1)           = 1
  end subroutine ifd_init

  ! @brief: parse entry and copy data to correct ifd value
  ! @param e: entry to parse
  pure subroutine ifd_parse_entry(this, e)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_parse_entry
    type(ifd)  , intent(inout) :: this ! ifd to parse into
    type(entry), intent(in   ) :: e    ! entry to parse
    integer                    :: i
    select case(e%tag)
      case(NewSubfileType           )
        this%newSubfileType            = transfer(e%valueBuff(1:4), newSubfileType)
      case(SubfileType              )
        this%subfileType               = transfer(e%valueBuff(1:2), subfileType)
      case(ImageWidth               )
        this%imageWidth                = transfer(e%valueBuff(1:size(e%valueBuff)), imageWidth) ! short or long
      case(ImageLength              )
        this%imageLength               = transfer(e%valueBuff(1:size(e%valueBuff)), imageLength) ! short or long

      case(BitsPerSample            )
        if(allocated(this%bitsPerSample)) deallocate(this%bitsPerSample)
        allocate(this%bitsPerSample(size(e%valueBuff)))
        this%bitsPerSample = transfer(e%valueBuff, this%bitsPerSample)

      case(Compression              )
        this%compression              = transfer(e%valueBuff(1:2), compression)
      case(PhotometricInterpretation)
        this%photometricInterpretation = transfer(e%valueBuff(1:2), photometricInterpretation)
      case(Threshholding            )
        this%threshholding             = transfer(e%valueBuff(1:2), threshholding)
      case(CellWidth                )
        this%cellWidth                 = transfer(e%valueBuff(1:2), cellWidth)
      case(CellLength               )
        this%cellLength                = transfer(e%valueBuff(1:2), cellLength)
      case(FillOrder                )
        this%fillOrder                 = transfer(e%valueBuff(1:2), fillOrder)

      case(ImageDescription         )
        if(allocated(this%imageDescription)) deallocate(this%imageDescription)
        allocate(character(e%valueCount) :: this%imageDescription)
        do i = 1, e%valueCount
          this%imageDescription(i:i) = char(e%valueBuff(i))
        enddo

      case(Make                     )
        if(allocated(this%make)) deallocate(this%make)
        allocate(character(e%valueCount) :: this%make)
        do i = 1, e%valueCount
          this%make(i:i) = char(e%valueBuff(i))
        enddo

      case(Model                    )
        if(allocated(this%model)) deallocate(this%model)
        allocate(character(e%valueCount) :: this%model)
        do i = 1, e%valueCount
          this%model(i:i) = char(e%valueBuff(i))
        enddo

      case(StripOffsets             )
        if(allocated(this%stripOffsets)) deallocate(this%stripOffsets)
        allocate(this%stripOffsets(e%valueCount))
        this%stripOffsets = transfer(e%valueBuff, this%stripOffsets)

      case(Orientation              )
        this%orientation               = transfer(e%valueBuff(1:2), orientation)
      case(SamplesPerPixel          )
        this%samplesPerPixel           = transfer(e%valueBuff(1:2), samplesPerPixel)
      case(RowsPerStrip             )
        this%rowsPerStrip              = transfer(e%valueBuff(1:2), rowsPerStrip)

      case(StripByteCounts          )
        if(allocated(this%stripByteCounts)) deallocate(this%stripByteCounts)
        allocate(this%stripByteCounts(e%valueCount))
        this%stripByteCounts = transfer(e%valueBuff, this%stripByteCounts)

      case(MinSampleValue           )
        if(allocated(this%minSampleValue)) deallocate(this%minSampleValue)
        allocate(this%minSampleValue(e%valueCount))
        this%minSampleValue = transfer(e%valueBuff, this%minSampleValue)

      case(MaxSampleValue           )
        if(allocated(this%maxSampleValue)) deallocate(this%maxSampleValue)
        allocate(this%maxSampleValue(e%valueCount))
        this%maxSampleValue = transfer(e%valueBuff, this%maxSampleValue)

      case(XResolution              )
        this%xResolution               = transfer(e%valueBuff(1:2), xResolution)
      case(YResolution              )
        this%yResolution               = transfer(e%valueBuff(1:2), yResolution)
      case(PlanarConfiguration      )
        this%planarConfiguration       = transfer(e%valueBuff(1:2), planarConfiguration)

      case(FreeOffsets              )
        if(allocated(this%freeOffsets)) deallocate(this%freeOffsets)
        allocate(this%freeOffsets(e%valueCount))
        this%freeOffsets = transfer(e%valueBuff, this%freeOffsets)

      case(FreeByteCounts           )
        if(allocated(this%freeByteCounts)) deallocate(this%freeByteCounts)
        allocate(this%freeByteCounts(e%valueCount))
        this%freeByteCounts = transfer(e%valueBuff, this%freeByteCounts)

      case(GrayResponseUnit         )
        this%grayResponseUnit          = transfer(e%valueBuff(1:2), grayResponseUnit)

      case(GrayResponseCurve        )
        if(allocated(this%grayResponseCurve)) deallocate(this%grayResponseCurve)
        allocate(this%grayResponseCurve(e%valueCount))
        this%grayResponseCurve = transfer(e%valueBuff, this%grayResponseCurve)

      case(ResolutionUnit           )
        this%resolutionUnit            = transfer(e%valueBuff(1:2), resolutionUnit)

      case(Software                 )
        if(allocated(this%software)) deallocate(this%software)
        allocate(character(e%valueCount) :: this%software)
        do i = 1, e%valueCount
          this%software(i:i) = char(e%valueBuff(i))
        enddo

      case(DateTime                 )
        if(allocated(this%dateTime)) deallocate(this%dateTime)
        allocate(character(e%valueCount) :: this%dateTime)
        do i = 1, e%valueCount
          this%dateTime(i:i) = char(e%valueBuff(i))
        enddo

      case(Artist                   )
        if(allocated(this%artist)) deallocate(this%artist)
        allocate(character(e%valueCount) :: this%artist)
        do i = 1, e%valueCount
          this%artist(i:i) = char(e%valueBuff(i))
        enddo

      case(HostComputer             )
        if(allocated(this%hostComputer)) deallocate(this%hostComputer)
        allocate(character(e%valueCount) :: this%hostComputer)
        do i = 1, e%valueCount
          this%hostComputer(i:i) = char(e%valueBuff(i))
        enddo

      case(ColorMap                 )
        if(allocated(this%colorMap)) deallocate(this%colorMap)
        allocate(this%colorMap(e%valueCount))
        this%colorMap = transfer(e%valueBuff, this%colorMap)

      case(ExtraSamples             )
        if(allocated(this%extraSamples)) deallocate(this%extraSamples)
        allocate(this%extraSamples(e%valueCount))
        this%extraSamples = transfer(e%valueBuff, this%extraSamples)

      case(Copyright                )
        if(allocated(this%copyright)) deallocate(this%copyright)
        allocate(character(e%valueCount) :: this%copyright)
        do i = 1, e%valueCount
          this%copyright(i:i) = char(e%valueBuff(i))
        enddo

      case(SampleFormat              )
        if(allocated(this%sampleFormat)) deallocate(this%sampleFormat)
        allocate(this%sampleFormat(e%valueCount))
        this%sampleFormat = transfer(e%valueBuff, this%sampleFormat)

      case(Predictor                 )
        this%predictor                 = transfer(e%valueBuff(1:2), predictor)
    end select
  end subroutine ifd_parse_entry

  ! @brief: read an ifd header
  ! @param this: ifd to read header data into
  ! @param unit: file handle to read from
  ! @param pos: position of ifd in stream
  ! @param wrongEndian: flag for files not in native ended-ness
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message (filled if iostat.ne.0)
  ! @param parse: flag for if data should actually be parsed (defaults to true)
  ! @return: offset to next ifd header (0 if this is the last ifd)
  function ifd_read_header(this, unit, pos, wrongEndian, iostat, iomsg, parse) result(nextOffset)
 !DEC$ ATTRIBUTES DLLEXPORT :: ifd_read_header
   class(ifd)      , intent(inout)           :: this        ! ifd entry to read data into
    integer         , intent(in   )           :: unit        ! file handle to read from
    integer(int32)  , intent(in   )           :: pos         ! position of ifd in stream
    logical         , intent(in   )           :: wrongEndian ! flag for files not in native ended-ness
    integer         , intent(out  )           :: iostat      ! error flag
    character(len=*), intent(out  )           :: iomsg       ! error message
    logical         , intent(in   ), optional :: parse       ! flag for if data should actually be parsed (defaults to true)
    integer(int32)                            :: nextOffset  ! offset to next ifd header
    integer(int16)                            :: numEntries, i
    integer(int8 )                            :: buff(4)
    logical                                   :: overflow, doParse
    overflow = .false.
    doParse = .true.
    if(present(parse)) doParse = parse
    call ifd_init(this)

    ! get number of entries and allocate memory
    rewind(unit=unit)
    read(unit, pos=pos+1, iostat=iostat, iomsg=iomsg) buff(1:2)
    if(0.ne.iostat) return
    if(wrongEndian) then
      buff(1:2) = buff(2:1:-1)
    endif
    numEntries = transfer(buff(1:2), numEntries)
    if(allocated(this%entries)) deallocate(this%entries)
    allocate(this%entries(numEntries))

    ! read and parse entries
    do i = 1, numEntries
      call this%entries(i)%read(unit, wrongEndian, iostat, iomsg, doParse)
      if(0.ne.iostat) return
      if(doParse) call ifd_parse_entry(this, this%entries(i))
    enddo

    if(doParse) then
      if(this%imageWidth.lt.int(0).or.this%imageLength.lt.0) overflow = .true.
      if(this%cellWidth.lt.0.or.this%cellLength.lt.0) overflow = .true.
      do i = 1, size(this%stripByteCounts)
        if(this%stripOffsets(i).lt.0.or.this%stripByteCounts(i).lt.0) overflow = .true.
      enddo
      do i = 1, size(this%maxSampleValue)
        if(this%maxSampleValue(i).lt.0) overflow = .true.
      enddo
    endif

    ! read offset to next ifd
    read(unit, iostat=iostat, iomsg=iomsg) buff
    if(0.ne.iostat) return
    if(wrongEndian) then
      buff(1:4) = buff(4:1:-1)
    endif
    nextOffset = transfer(buff, nextOffset)
    if(nextOffset.lt.0) overflow = .true.
    if(overflow) then
      iostat = 1
      iomsg = "signed/unsigned overflow reading offset to next tif ifd"
    endif
  end function ifd_read_header

  ! @brief: read image data for an ifd
  ! @param this: ifd to read header data into
  ! @param unit: file handle to read from
  ! @param wrongEndian: flag for files not in native ended-ness
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message (filled if iostat.ne.0)
  subroutine ifd_read_image_data(this, unit, wrongEndian, iostat, iomsg)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_read_image_data
    class(ifd)      , intent(inout)                       :: this        ! ifd entry to read data into
    integer         , intent(in   )                       :: unit        ! file handle to read from
    logical         , intent(in   )                       :: wrongEndian ! flag for files not in native ended-ness
    integer         , intent(out  )                       :: iostat      ! error flag
    character(len=*), intent(out  )                       :: iomsg       ! error message
    integer(int32)                                        :: buffOffsets(size(this%stripByteCounts))
    integer         , dimension(size(this%bitsPerSample)) :: bytesPerSample, sampleOffsets
    integer                                               :: i, j, bytesPerPix, offset, pixelsPerStrip, bytesDecoded
    logical                                               :: wide
    integer(int8 )  , allocatable                         :: encoded(:), decoded(:) ! temp buffers for compressed images

    ! some writers specify only a single sample format for multi sample images
    if(this%samplesPerPixel.gt.1.and.allocated(this%sampleFormat)) then
      if(size(this%sampleFormat).eq.1) then
        j = this%sampleFormat(1)
        deallocate(this%sampleFormat)
        allocate(this%sampleFormat(this%samplesPerPixel))
        do i = 1, this%samplesPerPixel
          this%sampleFormat(i) = j
        enddo
      endif
    endif

    ! sanity check offset/bytecounts
    if(size(this%stripOffsets).ne.size(this%stripByteCounts)) then
      iostat = 1
      iomsg = "mismatch strip offset / strip byte count sizes"
      return
    endif

    ! compute offsets
    buffOffsets(1) = 0
    do i = 2, size(buffOffsets)
      buffOffsets(i) = buffOffsets(i-1) + this%stripByteCounts(i-1)
    enddo
    if(allocated(this%data)) deallocate(this%data)

    if(this%compression.eq.1) then ! uncompressed
      ! allocate buffer for data
      allocate(this%data(this%stripByteCounts(size(this%stripByteCounts)) + buffOffsets(size(buffOffsets))))

      ! read data
      do i = 1, size(this%stripOffsets)
        read(unit, pos=this%stripOffsets(i)+1, iostat=iostat, iomsg=iomsg)&
        this%data(buffOffsets(i)+1:buffOffsets(i)+this%stripByteCounts(i))
      enddo
    else if(this%compression.eq.5) then ! lzw
      ! allocate buffer for data
      bytesPerPix = 0
      do j = 1, this%samplesPerPixel
        bytesPerPix = bytesPerPix + this%bitsPerSample(j) / 8
      enddo
      allocate(this%data(this%imageWidth*this%imageLength*bytesPerPix))

      ! read data
      offset = 0
      allocate(encoded(maxval(this%stripByteCounts)))
      pixelsPerStrip = this%imageWidth*this%rowsPerStrip
      allocate(decoded(pixelsPerStrip*bytesPerPix))
      do i = 1, size(this%stripOffsets)
        read(unit, pos=this%stripOffsets(i)+1, iostat=iostat, iomsg=iomsg) encoded(1:this%stripByteCounts(i))
        bytesDecoded = lzw_decode(encoded, decoded)
        if(bytesDecoded.gt.0) then
          if(this%predictor.eq.2) then ! undo horizontal predictor modification
            do j = 2, pixelsPerStrip
              if(modulo(j, this%imageWidth).ne.1) decoded((j-1)*bytesPerPix+1:j*bytesPerPix) = &
               decoded((j-1)*bytesPerPix+1: j   *bytesPerPix) + decoded((j-2)*bytesPerPix+1:(j-1)*bytesPerPix)
            enddo
          endif
          this%data(offset+1:offset+bytesDecoded) = decoded(1:bytesDecoded)
          offset = offset + bytesDecoded
        endif
      enddo
      deallocate(encoded)
      deallocate(decoded)
    endif

    ! handle edge cases and check for data types wider than 8 bits
    if(.not.(1.eq.this%compression.or.5.eq.this%compression)) then
      iostat = 1
      select case(this%compression)
        case(1)
          iostat = 0
        case(2)
          iomsg = "unsupported compression (run length encoded)"
        case(-32763)
          iomsg = "unsupported compression (pack bits)"
        case(3)
          iomsg = "unsupported compression (group 3 fax encoding)"
        case(4)
          iomsg = "unsupported compression (group 4 fax encoding)"
        case(5)
          iostat = 0
        case(6)
          iomsg = "unsupported compression (old sytle jpeg)"
        case(7)
          iomsg = "unsupported compression (new style jpeg)"
        case(8)
          iomsg = "unsupported compression (deflate)"
        case(9)
          iomsg = "unsupported compression (JBIG b&w)"
        case(10)
          iomsg = "unsupported compression (JBIG color)"
        case default
          iomsg = "unknown compression"
      end select
      return
    endif
    wide = .false.
    do i = 1, size(this%bitsPerSample)
      if(0.ne.modulo(this%bitsPerSample(i), int(8, kind=int16))) then
        iostat = 1
        iomsg = "unsupported sample bitdepth (not a multiple of 8)"
        return
      endif
      bytesPerSample(i) = this%bitsPerSample(i) / 8 ! assume 8 bits per byte
      if(bytesPerSample(i).gt.1) wide = .true.
    enddo

    ! byteswap data if needed
    if(wrongEndian.and.wide) then
      sampleOffsets(1) = 0
      do i = 2, size(sampleOffsets)
        sampleOffsets(i) = sampleOffsets(i-1) + bytesPerSample(i-1)
      enddo
      bytesPerPix = sampleOffsets(size(sampleOffsets)) + bytesPerSample(size(bytesPerSample))
      do i = 1, this%imageWidth * this%imageLength
        offset = (i-1) * bytesPerPix
        do j = 1, size(bytesPerSample)
          this%data(offset+sampleOffsets(j)+1:offset+bytesPerSample(j)) = &
           this%data(offset+bytesPerSample(j):offset+sampleOffsets(j)+1:-1)
        enddo
      enddo
    endif
  end subroutine ifd_read_image_data

  ! @brief: write an ifd's header to a file
  ! @param this: ifd to write header data from
  ! @param unit: file handle to write to
  ! @param lastEntry: true/false if this is/is not the last ifd
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message (filled if iostat.ne.0)
  subroutine ifd_write_header(this, unit, lastEntry, iostat, iomsg)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_write_header
    class(ifd)      , intent(in ) :: this      ! ifd entry to write data from
    integer         , intent(in ) :: unit      ! file handle to write into
    logical         , intent(in ) :: lastEntry ! flag for if this is the last ifd
    integer         , intent(out) :: iostat    ! error flag
    character(len=*), intent(out) :: iomsg     ! error message
    integer(int32)                :: dataOffset
    integer(int16)                :: numEntries, i

    ! write number of entries and calculate offset to end of ifd
    numEntries = size(this%entries)
    write(unit, iostat=iostat, iomsg=iomsg) numEntries
    if(0.ne.iostat) return
    inquire(unit, pos=dataOffset) ! get current file position
    dataOffset = dataOffset + 12 * size(this%entries) + 4 ! 12 bytes per entry + 4 bytes for offset to next ifd
    
    ! write actual entries
    do i = 1, numEntries
      dataOffset = dataOffset + this%entries(i)%write(unit, dataOffset, iostat, iomsg)
      if(0.ne.iostat) return
    enddo

    ! write offset to next entry
    if(lastEntry) dataOffset = 1
    write(unit, iostat=iostat, iomsg=iomsg) dataOffset-1
  end subroutine ifd_write_header

  ! @brief: compute the size required to write an ifd header (everything but the image data) to a file
  ! @param this: ifd to compute size requirement for
  ! @return: bytes required to write ifd header
  function ifd_size(this) result(byteCount)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_size
    class(ifd)    , intent(in ) :: this      ! ifd entry to compute size of
    integer(int32)              :: byteCount ! size in bytes of ifd header on disk
    integer(int16)              :: numEntries, i, entryBytes

    ! compute size of entries without appended data
    numEntries = size(this%entries)
    byteCount = 2 + 12 * numEntries + 4 ! 2 bytes for entry count, 12 bytes per entry, 4 bytes for offset to next ifd

    ! compute size of appnded data
    do i = 1, numEntries
      entryBytes = size(this%entries(i)%valueBuff)

      if(entryBytes.gt.4) then
        if(1.eq.modulo(entryBytes, int(2, kind=int16))) entryBytes = entryBytes + 1 ! 16 bit word boundaries
        byteCount = byteCount + entryBytes
      endif
    enddo
  end function ifd_size

  subroutine ifd_set_type_and_dims(this, type, typeBytes, count, width, length)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_set_type_and_dims
    class(ifd)    , intent(inout) :: this             ! ifd data structure to fill
    integer       , intent(in   ) :: type             ! 1,2,3,5,6 for uint,int,fp,complex int,complex fp
    integer(int16), intent(in   ) :: typeBytes, count ! sample bytes + count
    integer       , intent(in   ) :: width, length    ! image dimension
    
    ! allocate arrays
    if(allocated(this%bitsPerSample  )) deallocate(this%bitsPerSample  )
    if(allocated(this%stripOffsets   )) deallocate(this%stripOffsets   )
    if(allocated(this%stripByteCounts)) deallocate(this%stripByteCounts)
    if(allocated(this%sampleFormat   )) deallocate(this%sampleFormat   )
    if(allocated(this%data           )) deallocate(this%data           )
    if(allocated(this%entries        )) deallocate(this%entries        )
    allocate(this%bitsPerSample  (count                       ))
    allocate(this%stripOffsets   (1                           ))
    allocate(this%stripByteCounts(1                           ))
    allocate(this%sampleFormat   (count                       ))
    allocate(this%data           (width*length*count*typeBytes))
    allocate(this%entries        (11                          ))

    ! fill ifd data fields
    this%compression               = 1 ! none
    this%photometricInterpretation = 1 ! black is zero
    this%samplesPerPixel           = count
    this%planarConfiguration       = 1 ! rgbrgb instead of rrr ggg bbb
    this%imageWidth                = width
    this%imageLength               = length
    this%rowsPerStrip              = length
    this%bitsPerSample             = typeBytes * 8
    this%stripOffsets              = 0
    this%stripByteCounts           = width*length*count*typeBytes
    this%sampleFormat              = type
    if(3.eq.count.and.1.eq.typeBytes) this%photometricInterpretation = 2 ! rgb
    if(4.eq.count.and.1.eq.typeBytes) then
      this%photometricInterpretation = 2 ! rgba
      deallocate(this%entries)
      allocate(this%entries(12))
      allocate(this%extraSamples(1))
      this%extraSamples(1) = 2 ! unassociated alpha
    endif

    ! build ifd entries (sorted by tag value)
    call this%entries( 1)%build(ImageWidth                , this%imageWidth               ) ! 0100
    call this%entries( 2)%build(ImageLength               , this%imageLength              ) ! 0101
    call this%entries( 3)%build(BitsPerSample             , this%bitsPerSample            ) ! 0102
    call this%entries( 4)%build(Compression               , this%compression              ) ! 0103
    call this%entries( 5)%build(PhotometricInterpretation , this%photometricInterpretation) ! 0106
    call this%entries( 6)%build(StripOffsets              , this%stripOffsets             ) ! 0111
    call this%entries( 7)%build(SamplesPerPixel           , this%samplesPerPixel          ) ! 0115
    call this%entries( 8)%build(RowsPerStrip              , this%rowsPerStrip             ) ! 0116
    call this%entries( 9)%build(StripByteCounts           , this%stripByteCounts          ) ! 0117
    call this%entries(10)%build(PlanarConfiguration       , this%planarConfiguration      ) ! 011C
    call this%entries(11)%build(SampleFormat              , this%sampleFormat             ) ! 0153
    if(allocated(this%extraSamples)) then
      call this%entries(11)%build(ExtraSamples            , this%extraSamples             ) ! 0152
      call this%entries(12)%build(SampleFormat            , this%sampleFormat             ) ! 0153
    endif
    this%configured = .true.
  end subroutine ifd_set_type_and_dims

  ! @brief: check if the image in this ifd has a simple pixel type (scalar or vector of a single data type)
  ! @param this: ifd to check
  ! @return: true/false if ifd contains a simple/complex pixel type
  function ifd_is_simple(this) result(simple)
!DEC$ ATTRIBUTES DLLEXPORT :: ifd_is_simple
    class(ifd), intent(in) :: this   ! ifd data structure to check
    logical                :: simple ! true if all samples are same type
    integer                :: i, bits, type
    simple = .false.
    if(allocated(this%bitsPerSample).and.allocated(this%sampleFormat)) then
      simple = .true.
      bits = this%bitsPerSample(1)
      type = this%sampleFormat (1)
      do i = 2, this%samplesPerPixel
        if(bits.ne.this%bitsPerSample(i)) simple = .false.
        if(type.ne.this%sampleFormat (i)) simple = .false.
      enddo
    endif
  end function ifd_is_simple

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                          tif_t Functions                           !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! @brief: clean up memory alloted by a tif
  ! @param this: tif to clean up
  subroutine tif_destroy(this)
!DEC$ ATTRIBUTES DLLEXPORT :: tif_destroy
    type(tif_t), intent(inout) :: this ! tif data structure to clean up
    if(allocated(this%directories)) deallocate(this%directories)
  end subroutine tif_destroy
  
  ! @brief: read a tif from a file
  ! @param this: tif object to read into
  ! @param filename: name of file to read from
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message (filled if iostat.ne.0)
  subroutine tif_read(this, filename, iostat, iomsg)
!DEC$ ATTRIBUTES DLLEXPORT :: tif_read
    class(tif_t)    , intent(inout) :: this     ! tif data structure to read file into
    character(len=*), intent(in   ) :: filename ! name of tiff file to read
    integer         , intent(out  ) :: iostat   ! error flag
    character(len=*), intent(out  ) :: iomsg    ! error message
    integer(int8)                   :: buff(4)
    integer(int32)                  :: offset, start
    logical                         :: matchBig, matchLit, sysBig, wrongEndian, overflow
    integer                         :: unit, ifdCount, i
    type(ifd)                       :: dir
    matchBig = .false.
    matchLit = .false.
    overflow = .false.
    iostat = 0

    ! determine system endedness
    sysBig = sys_big_endian(iostat, iomsg)
    if(0.ne.iostat) return

    ! open file, read header, and check for magic character sequence
    open(newunit = unit, file = filename, status = 'old', access = 'stream')
    read(unit, iostat=iostat, iomsg=iomsg) buff
    if(0.ne.iostat) return
    if((77.eq.buff(1)).and.(77.eq.buff(2)).and.(0.eq.buff(3)).and.(42.eq.buff(4))) then
      matchBig = .true.
    else if((73.eq.buff(1)).and.(73.eq.buff(2)).and.(42.eq.buff(3)).and.(0.eq.buff(4))) then
      matchLit = .true.
    endif

    if(matchBig.or.matchLit) then
      ! check if file matches endedness
      if((sysBig.and.matchLit).or.((.not.sysBig).and.matchBig)) then
        wrongEndian = .true.
      else
        wrongEndian = .false.
      endif

      ! get offset to first ifd
      read(unit, iostat=iostat, iomsg=iomsg) buff
      if(0.ne.iostat) return
      if(wrongEndian) then
        buff(1:4) = buff(4:1:-1)
      endif
      start = transfer(buff(1:4), start)
      if(start.lt.0) then
        iostat = 1
        iomsg = "signed/unsigned overflow reading ifd offset"
        return
      endif

      ! count number of ifds without parsing (offset to next ifd is 0 for last ifd)
      ifdCount = 0
      offset = start
      do while(0.ne.offset)
        offset = dir%readHeader(unit, offset, wrongEndian, iostat, iomsg, .false.) ! read headers without parsing
        if(0.ne.iostat) return
        ifdCount = ifdCount + 1
      enddo

      ! allocate ifds and read
      offset = start
      allocate(this%directories(ifdCount))
      do i = 1, ifdCount
        offset = this%directories(i)%readHeader(unit, offset, wrongEndian, iostat, iomsg)
        if(iostat.ne.0) return
        call this%directories(i)%readData(unit, wrongEndian, iostat, iomsg)
        if(0.ne.iostat) return
      enddo
    else
      iostat = 1
      iomsg = "not a tif file"
    endif
    close(unit)
  end subroutine tif_read

  ! @brief: write a tif to a file
  ! @param this: tif object to write from
  ! @param filename: name of file to write to
  ! @param iostat: error flag (0 on success)
  ! @param iomsg: error message (filled if iostat.ne.0)
  subroutine tif_write(this, filename, iostat, iomsg)
 !DEC$ ATTRIBUTES DLLEXPORT :: tif_write
   class(tif_t)    , intent(inout) :: this     ! tif data structure to write to file
    character(len=*), intent(in   ) :: filename ! name of tiff file to write
    integer         , intent(out  ) :: iostat   ! error flag
    character(len=*), intent(out  ) :: iomsg    ! error message
    integer(int8)                   :: header(8)
    integer(int32)                  :: offset
    logical                         :: sysBig
    integer                         :: unit, ifdCount, i
    type(ifd)                       :: dir
    iostat = 0

    ! make sure the file has been properly formatted
    ifdCount = size(this%directories)
    do i = 1, ifdCount
      if(.not.this%directories(i)%configured) then
        iostat = 1
        iomsg = "not all tif directories have been configured"
        return
      endif
    enddo

    ! determine system endedness and write tiff header
    sysBig = sys_big_endian(iostat, iomsg)
    if(0.ne.iostat) return
    if(sysBig) then
      header = [77,77,0,42,0,0,0,8] ! MM, 0x002A, offset to first ifd
    else
      header = [73,73,42,0,8,0,0,0] ! II, 0x002A, offset to first ifd
    endif
    open(newunit = unit, file = filename, access = 'stream')
    write(unit, iostat=iostat, iomsg=iomsg) header
    if(0.ne.iostat) return

    ! compute total size of ifds and determine offset to data
    offset = 8
    do i = 1, ifdCount
      offset = offset + this%directories(i)%size()
    enddo

    ! set up ifd offsets and other required header components
    do i = 1, ifdCount
      ! single strip per ifd
      if(allocated(this%directories(i)%stripOffsets)) deallocate(this%directories(i)%stripOffsets)
      allocate(this%directories(i)%stripOffsets(1))
      
      ! all ifds then all data in order
      if(1.eq.i) then
        this%directories(i)%stripOffsets(1) = offset
      else
        this%directories(i)%stripOffsets(1) = this%directories(i-1)%stripOffsets   (1) &
                                            + this%directories(i-1)%stripByteCounts(1)
      endif
      call this%directories(i)%entries(6)%build(StripOffsets, this%directories(i)%stripOffsets)
    enddo

    ! write ifd headers
    do i = 1, ifdCount
      call this%directories(i)%writeHeader(unit, i.eq.ifdCount, iostat, iomsg)
      if(0.ne.iostat) return
    enddo

    ! write image data
    do i = 1, ifdCount
      write(unit, pos=this%directories(i)%stripOffsets(1)+1, iostat=iostat, iomsg=iomsg) this%directories(i)%data
      if(0.ne.iostat) return
    enddo
    close(unit)
  end subroutine tif_write

  ! @brief: check that the pixel type of this image is simple (scalar or vector of a single scalar type). If the tif has multiple ifds checks that it is a 3d stack (all images are same pixel type and size)
  ! @param this: tif to check for simplicity
  ! @return: true (false) if the tif is (not) simple
  function tif_is_simple(this) result(simple)
!DEC$ ATTRIBUTES DLLEXPORT :: tif_is_simple
    class(tif_t), intent(in) :: this   ! tif data structure to check
    logical                  :: simple ! true if all samples are same type and slices are same dimensions
    integer                  :: i, bits, type, width, length, count
    simple = .false.
    if(allocated(this%directories)) then
      simple = ifd_is_simple(this%directories(1))
      bits   = this%directories(1)%bitsPerSample(1)
      type   = this%directories(1)%sampleFormat(1)
      width  = this%directories(1)%imageWidth
      length = this%directories(1)%imageLength
      count  = this%directories(1)%samplesPerPixel
      do i = 2, size(this%directories)
        if(.not.ifd_is_simple(this%directories(i))) simple = .false.
        if(  bits.ne.this%directories(i)%bitsPerSample(1)) simple = .false.
        if(  type.ne.this%directories(i)%sampleFormat(1) ) simple = .false.
        if( width.ne.this%directories(i)%imageWidth      ) simple = .false.
        if(length.ne.this%directories(i)%imageLength     ) simple = .false.
        if( count.ne.this%directories(i)%samplesPerPixel ) simple = .false.
      enddo
    endif
  end function tif_is_simple

  ! @brief: extract the data from a tif object into an image_t
  ! @param this: tif to extract image from
  ! @return: image_t (empty if the tif couldn't be converted)
  function tif_get_image(this) result(im)
!DEC$ ATTRIBUTES DLLEXPORT :: tif_get_image
    class(tif_t) , intent(in) :: this ! tif data structure to extract image from
    type(image_t)             :: im
    integer                   :: i, sliceBytes
    call im%clear()
    if(tif_is_simple(this)) then ! potentially convertible, check pixel type
      im%pixelType = pix_unk
      if(this%directories(1)%sampleFormat(1).eq.1.or.this%directories(1)%sampleFormat(1).eq.2) then ! int/uint
        if(this%directories(1)%sampleFormat(1).eq.1) im%unsigned = .true.
        select case(this%directories(1)%bitsPerSample(1))
          case(8 )
            im%pixelType = pix_i8
          case(16)
            im%pixelType = pix_i16
          case(32)
            im%pixelType = pix_i32
          case(64)
            im%pixelType = pix_i64
        end select
      else if(this%directories(1)%sampleFormat(1).eq.3) then ! floating point
        select case(this%directories(1)%bitsPerSample(1))
          case(32)
            im%pixelType = pix_r32
          case(64)
            im%pixelType = pix_r64
        end select
      endif

      if(im%pixelType.ne.pix_unk) then ! convertible
        ! fill image dimensions
        if(size(this%directories).eq.1) then ! 2d image
          allocate(im%dims(2))
        else ! 3d image
          allocate(im%dims(3))
          im%dims(3) = size(this%directories)
        endif
        im%dims(1) = this%directories(1)%imageWidth
        im%dims(2) = this%directories(1)%imageLength
      
        ! set sample count and allocate buffer
        im%samplesPerPixel = this%directories(1)%samplesPerPixel
        sliceBytes = this%directories(1)%imageWidth * this%directories(1)%imageLength &
                   * (this%directories(1)%bitsPerSample(1)/8) * this%directories(1)%samplesPerPixel
        allocate(im%buff(sliceBytes*size(this%directories)))

        ! copy data to image buffer
        do i = 1, size(this%directories)
          im%buff((i-1)*sliceBytes+1:i*sliceBytes) = this%directories(i)%data
        enddo
      endif
    end if
  end function tif_get_image

  ! @brief: extract the data from an image_t into a tif object
  ! @param this: tif to extract image into
  ! @param im: image_t to extract image from
  subroutine tif_from_image(this, im)
 !DEC$ ATTRIBUTES DLLEXPORT :: tif_from_image
   class(tif_t) , intent(inout) :: this ! tif structure to fill
    type(image_t), intent(in   ) :: im   ! imnage to build tif from
    integer                      :: i, type, width, length, sliceBytes
    integer(int16)               :: typeBytes
    if(allocated(this%directories)) deallocate(this%directories)
    if(allocated(im%dims)) then
      ! parse dimensions
      if(im%samplesPerPixel.gt.1.and.size(im%dims).gt.2) then ! vector image
        width  = im%dims(2)
        length = im%dims(3)
        if(size(im%dims).eq.3) then ! 2d vector image
          allocate(this%directories(1         ))
        else if(size(im%dims).eq.4) then ! 3d vector image
          allocate(this%directories(im%dims(4)))
        endif
      else if(size(im%dims).gt.1) then ! scalar image
        width  = im%dims(1)
        length = im%dims(2)
        if(size(im%dims).eq.2) then ! 2d scalar image
          allocate(this%directories(1         ))
        else if(size(im%dims).eq.3) then ! 3d scalar image
          allocate(this%directories(im%dims(3)))
        endif
      endif

      if(allocated(this%directories)) then ! valid tif dimensions
        select case(im%pixelType)
          case(pix_i8 )
            type      = 2 ! int
            typeBytes = 1
          case(pix_i16)
            type      = 2 ! int
            typeBytes = 2
          case(pix_i32)
            type      = 2 ! int
            typeBytes = 4
          case(pix_i64)
            type      = 2 ! int
            typeBytes = 8
          case(pix_r32)
            type      = 3 ! fp
            typeBytes = 4
          case(pix_r64)
            type      = 3 ! fp
            typeBytes = 8
        end select
        if(type.eq.2.and.im%unsigned) type = 1 ! uint

        ! build ifds
        sliceBytes = width * length * typeBytes * im%samplesPerPixel
        do i = 1, size(this%directories)
          call ifd_set_type_and_dims(this%directories(i), type, typeBytes, im%samplesPerPixel, width, length)
          this%directories(i)%data = im%buff((i-1)*sliceBytes+1:i*sliceBytes)
        enddo
      endif
    endif
  end subroutine tif_from_image
end module tif