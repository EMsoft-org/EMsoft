!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (c) 2018-2020, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:lzw.f90
!-----------------------------------------------------------------------
!
! module: lzw
!
!> @author: W.C. Lenthe, Carnegie Mellon University
!
!> @brief: routine to decompress an lzw encoded buffer
!
!> @date: 03/14/18 WCL 1.0 original
!
!-----------------------------------------------------------------------

! simple lzw decoded class to support reading of compressed tiff files
module lzw
  use, intrinsic :: iso_fortran_env ! fixed width integers
  implicit none

  private
  public :: lzw_decode ! only expose the actual decode function

  ! 12 bit lzw string table
  type dict
    integer(int8 ) :: finalChar(0:4095) ! last character of string at position in table
    integer(int16) :: prevChar (0:4095) ! position of previous character in table
    integer(int16) :: size              ! number of entries
    integer(int16) :: bits              ! current width of codes
  contains
    procedure :: init   => dict_init
    procedure :: decode => dict_decode
    procedure :: length => dict_code_string_length
    procedure :: string => dict_build_code_string
  end type dict

  ! class to hold stream during decoding
  type stream
    integer(int8), pointer :: encoded(:) ! encoded array of bytes
    integer                :: index      ! index of current byte in encoded array
    integer                :: bitsLeft   ! usable bits at index in array (1-8)
    type(dict)             :: dict       ! string table
  contains
    procedure :: init    => stream_init
    procedure :: extract => stream_extract
  end type stream

contains
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!                       dict member functions                        !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! @brief: initialize an lzw string table (dictionary)
  ! @param this: dict to initialize
  subroutine dict_init(this)
 !DEC$ ATTRIBUTES DLLEXPORT :: dict_init
   class(dict) , intent(inout) :: this ! dict to initialize
    integer                     :: i
    this%prevChar(:) = -1
    this%bits = 9
    do i = 0,255
      this%finalChar(i) = i
    enddo
    this%size = 257 ! 256 + clear + eoi
  end subroutine dict_init

  ! @brief: compute the length of a string table code
  ! @param this: string table to look up code in
  ! @param code: index of string to look up
  ! @return: length of string
  function dict_code_string_length(this, code) result(length)
 !DEC$ ATTRIBUTES DLLEXPORT :: dict_code_string_length
   class(dict)   , intent(in) :: this
    integer(int16), intent(in) :: code
    integer(int16)             :: length, i
    length = 1
    i = code
    do while(this%prevChar(i).ne.-1)
      i = this%prevChar(i)
      length = length+1
    enddo
  end function dict_code_string_length

  ! @brief: build a code string from a string table
  ! @param this: string table to look up code in
  ! @param code: index of string to look up
  ! @param string: array to write string into
  ! @param endindex: index of final character position in string
  subroutine dict_build_code_string(this, code, string, endindex)
 !DEC$ ATTRIBUTES DLLEXPORT :: dict_build_code_string
   class(dict)   , intent(in   ) :: this
    integer(int16), intent(in   ) :: code
    integer(int8 ), intent(inout) :: string(:)
    integer       , intent(in   ) :: endindex
    integer(int16)                :: i, j
    i = code
    j = endindex
    do while(this%prevChar(i).ne.-1)
      string(j) = this%finalChar(i)
      i = this%prevChar(i)
      j = j-1
    enddo
    string(j) = this%finalChar(i)
  end subroutine dict_build_code_string

  ! @brief: decode a string from the dictionary and update dictionary
  ! @param this: string table to look up code in
  ! @param code: index of string to look up
  ! @param oldCode: index of previous string in stream
  ! @param string: string table value for code
  ! @return: bytes extracted
  function dict_decode(this, code, oldCode, string) result(count)
 !DEC$ ATTRIBUTES DLLEXPORT :: dict_decode
   class(dict)   , intent(inout)              :: this
    integer(int16), intent(in   )              :: code, oldCode
    integer(int8 ), intent(inout), allocatable :: string(:)
    integer                                    :: count
    integer                                    :: codeLength
    logical                                    :: codeInTable
    ! find out how much space is required and make sure enough is allocated
    codeInTable = code.le.this%size
    if(codeInTable) then ! already in dictionary
      count = this%length(code)
    else ! code needs to be added to dictionary
      count = this%length(oldCode)+1
    endif
    if(allocated(string)) then
      if(size(string).lt.count) then
        deallocate(string)
        allocate(string(2*count))
      endif
    else
      allocate(string(count))
    endif

    ! build string / update dictionary
    if(codeInTable) then ! already in dictionary
      call this%string(code   , string, count)
    else ! code needs to be added to dictionary
      call this%string(oldCode, string, count-1)
      string(count) = string(1)
    endif
    this%size = this%size+1
    this%finalChar(this%size) = string(1)
    this%prevChar( this%size) = oldCode

    ! update code bitwidth
    select case(this%bits)
      case( 9)
        if(this%size.eq.510 ) this%bits = 10
      case(10)
        if(this%size.eq.1022) this%bits = 11
      case(11)
        if(this%size.eq.2046) this%bits = 12
      case(12)
        if(this%size.eq.4094) call this%init()
    end select
  end function dict_decode

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!                      stream member functions                       !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! @brief: initialize an lzw stream
  ! @param this: stream to initialize
  ! @param encoded: encoded stream to read from
  subroutine stream_init(this, encoded)
!DEC$ ATTRIBUTES DLLEXPORT :: stream_init
    class(stream) , intent(inout)         :: this ! stream to initialize
    integer(int8 ), intent(in   ), target :: encoded(:)
    this%index = 1
    this%bitsLeft = 8
    this%encoded => encoded
    call this%dict%init()
  end subroutine stream_init

  ! @brief: get the next 9, 10, 11, or 12 bit code from the encoded stream
  ! @param this: stream to extract bits from
  ! @return: extracted bits padded to 16 bits wide
  function stream_extract(this) result(code)
 !DEC$ ATTRIBUTES DLLEXPORT :: image_flatten_rgba
   class(stream) , intent(inout) :: this ! stream to extract bits from
    integer(int16)                :: code ! extracted bits padded to 16 wide
    integer(int16)                :: b0, b1, b2
    integer(int16), parameter     :: magicBytes (19)  = [   1,  3,  7, 15, 31, 63,127,255,&
                                                          128,192,224,240,248,252,254,255,&
                                                          255,255,255]
    b0 = ishft(iand(int(this%encoded(this%index  ),int16),magicBytes(                 this%bitsLeft)),&
               this%dict%bits-this%bitsLeft  ) ! mask off leading bits and shift
    b1 = ishft(iand(int(this%encoded(this%index+1),int16),magicBytes(8+this%dict%bits-this%bitsLeft)),&
               this%dict%bits-this%bitsLeft-8) ! mask off middle/trailing bits and shift
    code = ior(b0,b1)
    if(this%bitsLeft.le.(this%dict%bits-9)) then ! code is split across 3 instead of 2 bytes
      b2 = ishft(iand(int(this%encoded(this%index+2),int16),magicBytes(8+this%dict%bits-this%bitsLeft)),&
                 this%dict%bits-this%bitsLeft-16) ! mask off middle/trailing bits and shift
      code = ior(code,b2)
      this%index = this%index + 1
      this%bitsLeft = 16 - this%dict%bits + this%bitsLeft
    else
      this%bitsLeft = 8 - this%dict%bits + this%bitsLeft
      if(this%bitsLeft.eq.0) then
        this%index = this%index + 1
        this%bitsLeft = 8
      endif
    endif
    this%index = this%index + 1
  end function stream_extract

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!                   actual decompression algorithm                   !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! @brief: decode an lzw encoded buffer
  ! @param encoded: encoded buffer
  ! @param decoded: decoded buffer (must already be allocated with enough space to hold the decompressed result)
  ! @result bytesRead: bytes read into the the decoded array
  function lzw_decode(encoded, decoded) result(bytesRead)
!DEC$ ATTRIBUTES DLLEXPORT :: lzw_decode
    integer(int8 ), intent(in   ), target :: encoded(:)
    integer(int16)                        :: code, oldCode, i
    integer(int64)                        :: bytesRead
    integer(int8 ), intent(inout)         :: decoded(:)
    integer(int8 ), allocatable           :: string(:)
    type(stream)                          :: strm

    ! initialize, extract first code, and make sure it first code is valid
    call strm%init(encoded)
    bytesRead = 0
    allocate(string(1))
    code = strm%extract()
    if(code.ne.256) return

    ! extract/decode codes until end of information code (257) is received
    do while(code.ne.257)
      if(code.eq.256) then ! clear code
        call strm%dict%init() ! empty dictionary
        code = strm%extract()
        if(code.eq.257) return
        i = 1
        string(1) = code
        oldCode = code
      else
        i = strm%dict%decode(code, oldCode, string)
      endif
      decoded(bytesRead+1:bytesRead+i) = string(1:i)
      bytesRead = bytesRead + i
      oldCode = code
      code = strm%extract()
      if((code.eq.514).and.(&
          (strm%dict%size.eq.1022).or.&
          (strm%dict%size.eq.2046).or.&
          (strm%dict%size.eq.4094))) code = 257
    enddo

    ! clean up
    if(allocated(string)) deallocate(string)
  end function lzw_decode
end module lzw
