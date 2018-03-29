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
! EMsoft:ImageTest.f90
!-----------------------------------------------------------------------
!
! module: ImageTest
!
!> @author: W.C. Lenthe, Carnegie Mellon University
!
!> @brief: test function for image_t
!
!> @date: 03/14/18 WCL 1.0 original
!
!-----------------------------------------------------------------------

module ImageTest
  use image
  use, intrinsic :: iso_fortran_env
  implicit none

contains
  ! @brief: helper function to check if an image is the expected shape and pixel type
  ! @param im: image_t object to check
  ! @param type: expected pixel type
  ! @param dims: expected shape
  ! @return: 0 if image matches expectation, >0 otherwise
  function check_image(im, type, dims) result(code)
    class(image_t)  , intent(in) :: im      ! image to check
    integer         , intent(in) :: type    ! expected pixel type
    integer         , intent(in) :: dims(:) ! expected dimensions
    integer                      :: i, code
    code = 0

    ! check pixel type
    if(im%pixelType.ne.type) then
      write(*,*) "incorrect pixel type"
      code = 1
      return
    endif
    if(im%samplesPerPixel.ne.1) then
      write(*,*) "incorrect pixel samples. Expected 1 got",im%samplesPerPixel
      code = 2
      return
    endif

    ! check dims
    if(size(im%dims).ne.size(dims)) then
      write(*,*) "incorrect dimensionallity"
      code = 3
      return
    endif
    do i = 1,size(dims)
      if(im%dims(i).ne.dims(i)) then
        write(*,*) "incorrect shape"
        code = 4
        return
      endif
    enddo
  end function check_image

  subroutine ImageExecuteTest(result) &
  bind(c, name='ImageExecuteTest') ! this routine is callable from a C/C++ program
  !DEC$ ATTRIBUTES DLLEXPORT :: ImageExecuteTest

    ! output is code that is 0 on success >0 otherwise
    use, intrinsic :: iso_c_binding
    integer(c_int32_t),intent(out) :: result

    ! declare variables
    integer                        :: i
    type(image_t)                  :: im
    integer(int8 )                 :: c
    integer(int16)                 :: s

    ! test data
    integer(int8  )                :: i8 (3*5*7)
    integer(int16 )                :: i16(3*5*7)
    integer(int32 )                :: i32(3*5*7)
    integer(int64 )                :: i64(3*5*7)
    real   (real32)                :: r32(3*5*7)
    real   (real64)                :: r64(3*5*7)
    integer(int8 )                 :: shape2(3,5), shape3(3,5,7)
    integer                        :: dim2(2), dim3(3)

    ! allocatable arrays for reading data into
    integer(int8  ), allocatable   :: i8_1 (:), i8_2 (:,:), i8_3 (:,:,:)
    integer(int16 ), allocatable   :: i16_1(:), i16_2(:,:), i16_3(:,:,:)
    integer(int32 ), allocatable   :: i32_1(:), i32_2(:,:), i32_3(:,:,:)
    integer(int64 ), allocatable   :: i64_1(:), i64_2(:,:), i64_3(:,:,:)
    real   (real32), allocatable   :: r32_1(:), r32_2(:,:), r32_3(:,:,:)
    real   (real64), allocatable   :: r64_1(:), r64_2(:,:), r64_3(:,:,:)

    ! arrays to hold binary tiff test data
    integer(int8 )                 :: lzwBuff(2281), bgBuff(200), ltBuff(200), rgbBuff(346), rgbaBuff(377)

    ! fill test data
    do i = 1,105
      i8 (i) = i
      i16(i) = i * 256
      i32(i) = i * 65536
      i64(i) = i * 429496729
      r32(i) = i
      r64(i) = i
    enddo
    dim2 = [3,5]
    dim3 = [3,5,7]

    allocate(i8_1(105))
    allocate(i16_1(105))
    allocate(i32_1(105))
    allocate(i64_1(105))
    allocate(r32_1(105))
    allocate(r64_1(105))

    ! fill lzw tiff buffer
    lzwBuff = [+Z'49',+Z'49',+Z'2A',+Z'00',-Z'34',+Z'07',+Z'00',+Z'00',-Z'80',+Z'00',+Z'20',+Z'50',+Z'38',+Z'24',+Z'14',+Z'00',&
               +Z'01',-Z'7C',+Z'42',+Z'61',+Z'50',-Z'50',+Z'08',+Z'0A',+Z'1D',+Z'0F',-Z'78',+Z'44',+Z'40',+Z'40',+Z'38',-Z'5C',&
               +Z'56',+Z'2D',+Z'17',+Z'01',-Z'7F',+Z'23',+Z'51',-Z'48',-Z'1C',+Z'74',+Z'08',+Z'05',-Z'70',+Z'48',+Z'64',+Z'52',&
               +Z'30',+Z'28',+Z'1A',+Z'4D',+Z'27',-Z'6C',+Z'4A',+Z'40',-Z'40',+Z'79',+Z'64',-Z'4A',+Z'5D',+Z'2F',+Z'03',-Z'7E',&
               +Z'26',+Z'53',+Z'39',-Z'5C',-Z'2C',+Z'10',+Z'09',-Z'64',+Z'4E',+Z'67',+Z'53',-Z'50',+Z'48',+Z'2A',+Z'7D',+Z'3F',&
               -Z'60',+Z'50',+Z'41',+Z'40',-Z'46',+Z'25',+Z'16',-Z'73',+Z'47',+Z'05',-Z'7D',+Z'29',+Z'54',-Z'46',+Z'65',+Z'34',&
               +Z'18',+Z'0D',-Z'58',+Z'54',+Z'6A',+Z'55',+Z'30',+Z'68',+Z'3A',-Z'53',+Z'57',-Z'54',+Z'56',+Z'41',-Z'40',-Z'06',&
               -Z'1B',+Z'76',-Z'43',+Z'5F',+Z'07',-Z'7C',+Z'2C',+Z'56',+Z'3B',+Z'25',-Z'6C',+Z'20',+Z'11',-Z'4C',+Z'5A',+Z'6D',&
               +Z'56',-Z'50',-Z'78',+Z'4A',-Z'23',+Z'6F',-Z'48',+Z'5C',+Z'42',+Z'41',+Z'3B',-Z'5B',-Z'2A',-Z'13',+Z'77',+Z'09',&
               -Z'7B',+Z'2F',+Z'57',-Z'45',-Z'1B',-Z'0C',+Z'28',+Z'15',-Z'40',+Z'60',+Z'70',+Z'58',+Z'30',-Z'58',+Z'5B',+Z'0D',&
               -Z'79',-Z'3C',+Z'62',+Z'42',-Z'3F',+Z'7C',+Z'66',+Z'37',+Z'1D',-Z'71',+Z'0B',-Z'7A',+Z'32',+Z'59',+Z'3C',-Z'5A',&
               +Z'54',+Z'30',+Z'19',-Z'34',+Z'66',+Z'73',+Z'59',-Z'50',-Z'38',+Z'6B',+Z'3D',-Z'61',-Z'30',+Z'68',+Z'43',+Z'41',&
               -Z'43',+Z'26',-Z'69',+Z'4D',-Z'59',+Z'0D',-Z'79',+Z'35',+Z'5A',-Z'43',+Z'66',-Z'4C',+Z'38',+Z'1D',-Z'28',+Z'6C',&
               +Z'76',+Z'5B',+Z'30',-Z'18',+Z'7B',+Z'6D',-Z'49',-Z'24',+Z'6E',+Z'43',-Z'3F',-Z'03',-Z'1A',-Z'09',+Z'7D',-Z'41',&
               +Z'0F',-Z'78',+Z'38',+Z'5C',+Z'3E',+Z'27',+Z'14',+Z'40',+Z'21',-Z'1C',+Z'72',+Z'79',+Z'5C',-Z'4F',+Z'08',-Z'75',&
               -Z'63',-Z'31',-Z'18',+Z'74',+Z'44',+Z'42',+Z'3E',-Z'59',+Z'57',-Z'53',-Z'29',+Z'11',-Z'77',+Z'3B',+Z'5D',-Z'42',&
               -Z'19',+Z'74',+Z'48',+Z'25',-Z'10',+Z'78',+Z'7C',+Z'5E',+Z'31',+Z'28',-Z'65',-Z'33',-Z'19',-Z'0C',+Z'7A',+Z'44',&
               -Z'3E',+Z'7F',+Z'67',-Z'49',-Z'23',-Z'11',+Z'13',-Z'76',+Z'3E',+Z'5F',+Z'3F',-Z'59',-Z'2C',+Z'50',+Z'29',-Z'04',&
               +Z'7E',+Z'7F',+Z'5F',-Z'4F',+Z'48',-Z'55',-Z'04',-Z'01',-Z'40',+Z'10',+Z'08',+Z'54',+Z'15',-Z'40',-Z'70',+Z'2C',&
               +Z'0D',+Z'03',-Z'7B',+Z'61',+Z'64',+Z'15',+Z'05',-Z'3F',-Z'70',+Z'68',+Z'58',+Z'16',-Z'3E',+Z'10',-Z'74',+Z'25',&
               +Z'09',-Z'7B',-Z'5F',+Z'74',+Z'2D',+Z'0B',-Z'3D',+Z'10',-Z'38',+Z'5C',+Z'17',-Z'3D',-Z'70',-Z'14',+Z'3D',+Z'0F',&
               -Z'7B',-Z'1F',-Z'7C',+Z'45',+Z'11',-Z'3C',-Z'6F',+Z'28',+Z'60',+Z'18',-Z'3B',+Z'11',+Z'4C',+Z'55',+Z'15',-Z'7A',&
               +Z'21',-Z'6C',+Z'5D',+Z'17',-Z'3A',+Z'11',-Z'78',+Z'64',+Z'19',-Z'3A',-Z'6F',-Z'54',+Z'6D',+Z'1B',-Z'7A',+Z'61',&
               -Z'5C',+Z'75',+Z'1D',-Z'39',-Z'6F',-Z'18',+Z'68',+Z'1A',-Z'38',+Z'12',+Z'0C',-Z'7B',+Z'21',-Z'7A',-Z'5F',-Z'4C',&
               -Z'73',+Z'23',-Z'37',+Z'12',+Z'48',+Z'6C',+Z'1B',-Z'37',-Z'6E',+Z'6C',-Z'63',+Z'27',-Z'7A',-Z'1F',-Z'3C',-Z'5B',&
               +Z'29',-Z'36',-Z'6E',-Z'58',+Z'70',+Z'1C',-Z'35',+Z'12',-Z'34',-Z'4B',+Z'2D',-Z'79',+Z'21',-Z'2C',-Z'43',+Z'2F',&
               -Z'34',+Z'13',+Z'08',+Z'74',+Z'1D',-Z'34',-Z'6D',+Z'2C',-Z'33',+Z'33',-Z'79',+Z'61',-Z'1C',-Z'2B',+Z'35',-Z'33',&
               -Z'6D',+Z'68',+Z'78',+Z'1E',-Z'32',+Z'13',-Z'74',-Z'1B',+Z'39',-Z'79',-Z'5F',-Z'0C',-Z'13',+Z'3B',-Z'31',+Z'13',&
               -Z'38',+Z'7C',+Z'1F',-Z'31',-Z'6D',-Z'14',-Z'03',+Z'3F',-Z'79',-Z'1E',+Z'05',+Z'05',+Z'41',-Z'30',-Z'6C',+Z'28',&
               -Z'80',+Z'20',-Z'2F',+Z'14',+Z'4D',+Z'15',+Z'45',-Z'78',+Z'22',+Z'15',+Z'1D',+Z'47',-Z'2E',+Z'14',-Z'78',-Z'7C',&
               +Z'21',-Z'2E',-Z'6C',-Z'53',+Z'2D',+Z'4B',-Z'78',+Z'62',+Z'25',+Z'35',+Z'4D',-Z'2D',-Z'6C',-Z'18',-Z'78',+Z'22',&
               -Z'2C',+Z'15',+Z'0D',+Z'45',+Z'51',-Z'78',-Z'5E',+Z'35',+Z'4D',+Z'53',-Z'2B',+Z'15',+Z'48',-Z'74',+Z'23',-Z'2B',&
               -Z'6B',+Z'6D',+Z'5D',+Z'57',-Z'78',-Z'1E',+Z'45',+Z'65',+Z'59',-Z'2A',-Z'6B',-Z'58',-Z'70',+Z'24',-Z'29',+Z'15',&
               -Z'33',+Z'75',+Z'5D',-Z'77',+Z'22',+Z'55',+Z'7D',+Z'5F',-Z'28',+Z'16',+Z'08',-Z'6C',+Z'25',-Z'28',-Z'6A',+Z'2D',&
               -Z'73',+Z'63',-Z'77',+Z'62',+Z'65',-Z'6B',+Z'65',-Z'27',-Z'6A',+Z'68',-Z'68',+Z'26',-Z'26',+Z'16',-Z'73',-Z'5B',&
               +Z'69',-Z'77',-Z'5E',+Z'75',-Z'53',+Z'6B',-Z'25',+Z'16',-Z'38',-Z'64',+Z'27',-Z'25',-Z'6A',-Z'13',-Z'43',+Z'6F',&
               -Z'77',-Z'1E',-Z'7B',-Z'3B',+Z'71',-Z'24',-Z'69',+Z'28',-Z'60',+Z'28',-Z'23',+Z'17',+Z'4D',-Z'2B',+Z'75',-Z'76',&
               +Z'22',-Z'6B',-Z'23',+Z'77',-Z'22',+Z'17',-Z'78',-Z'5C',+Z'29',-Z'22',-Z'69',-Z'53',-Z'13',+Z'7B',-Z'76',+Z'62',&
               -Z'5B',-Z'0B',+Z'7D',-Z'21',-Z'69',-Z'18',-Z'58',+Z'2A',-Z'20',+Z'18',+Z'0E',+Z'05',-Z'7F',-Z'76',-Z'5E',-Z'4A',&
               +Z'0D',-Z'7D',-Z'1F',+Z'18',+Z'48',-Z'54',+Z'2B',-Z'1F',-Z'68',+Z'6E',+Z'1D',-Z'79',-Z'76',-Z'1E',-Z'3A',+Z'25',&
               -Z'77',-Z'1E',-Z'68',-Z'58',-Z'50',+Z'2C',-Z'1D',+Z'18',-Z'32',+Z'35',-Z'73',-Z'75',+Z'22',-Z'2A',+Z'3D',-Z'71',&
               -Z'1C',+Z'19',+Z'08',-Z'4C',+Z'2D',-Z'1C',-Z'67',+Z'2E',+Z'4D',-Z'6D',-Z'75',+Z'62',-Z'1A',+Z'55',-Z'6B',-Z'1B',&
               -Z'67',+Z'68',-Z'48',+Z'2E',-Z'1A',+Z'19',-Z'72',+Z'65',-Z'67',-Z'75',-Z'5E',-Z'0A',+Z'6D',-Z'65',-Z'19',+Z'19',&
               -Z'38',-Z'44',+Z'2F',-Z'19',-Z'67',-Z'12',+Z'7D',-Z'61',-Z'75',-Z'1D',+Z'06',-Z'7B',-Z'5F',-Z'18',-Z'66',+Z'28',&
               -Z'40',+Z'30',-Z'17',+Z'1A',+Z'4E',-Z'6B',-Z'5B',-Z'74',+Z'23',+Z'16',-Z'63',-Z'59',-Z'16',+Z'1A',-Z'78',-Z'3C',&
               +Z'31',-Z'16',-Z'66',-Z'52',-Z'53',-Z'55',-Z'74',+Z'63',+Z'26',-Z'4B',-Z'53',-Z'15',-Z'66',-Z'18',-Z'38',+Z'32',&
               -Z'14',+Z'1B',+Z'0E',-Z'3B',-Z'4F',-Z'74',-Z'5D',+Z'36',-Z'33',-Z'4D',-Z'13',+Z'1B',+Z'48',-Z'34',+Z'33',-Z'13',&
               -Z'65',+Z'6E',-Z'23',-Z'49',-Z'74',-Z'1D',+Z'46',-Z'1B',-Z'47',-Z'12',-Z'65',-Z'58',-Z'30',+Z'34',-Z'11',+Z'1B',&
               -Z'32',-Z'0B',-Z'43',-Z'73',+Z'23',+Z'56',-Z'03',-Z'41',-Z'10',+Z'1C',+Z'08',-Z'2C',+Z'35',-Z'10',-Z'64',+Z'2F',&
               +Z'0D',-Z'3D',-Z'73',+Z'63',+Z'67',+Z'15',-Z'3B',-Z'0F',-Z'64',+Z'68',-Z'28',+Z'36',-Z'0E',+Z'1C',-Z'71',+Z'25',&
               -Z'37',-Z'73',-Z'5D',+Z'77',+Z'2D',-Z'35',-Z'0D',+Z'1C',-Z'38',-Z'24',+Z'37',-Z'0D',-Z'64',-Z'11',+Z'3D',-Z'31',&
               -Z'73',-Z'1D',-Z'79',+Z'45',-Z'2F',-Z'0C',-Z'63',+Z'28',-Z'20',+Z'38',-Z'0B',+Z'1D',+Z'4F',+Z'55',-Z'2B',-Z'72',&
               +Z'23',-Z'69',+Z'5D',-Z'29',-Z'0A',+Z'1D',-Z'78',-Z'1C',+Z'39',-Z'0A',-Z'63',-Z'51',+Z'6D',-Z'25',-Z'72',+Z'63',&
               -Z'59',+Z'75',-Z'23',-Z'09',-Z'63',-Z'18',-Z'18',+Z'3A',-Z'08',+Z'1E',+Z'0F',-Z'7B',-Z'1F',-Z'72',-Z'5D',-Z'49',&
               -Z'73',-Z'1D',-Z'07',+Z'1E',+Z'48',-Z'14',+Z'3B',-Z'07',-Z'62',+Z'6F',-Z'63',-Z'19',-Z'72',-Z'1D',-Z'39',-Z'5B',&
               -Z'17',-Z'06',-Z'62',-Z'58',-Z'10',+Z'3C',-Z'05',+Z'1E',-Z'31',-Z'4B',-Z'13',-Z'71',+Z'23',-Z'29',-Z'43',-Z'11',&
               -Z'04',+Z'1F',+Z'08',-Z'0C',+Z'3D',-Z'04',-Z'61',+Z'2F',-Z'33',-Z'0D',-Z'71',+Z'63',-Z'19',-Z'2B',-Z'0B',-Z'03',&
               -Z'61',+Z'68',-Z'08',+Z'3E',-Z'02',+Z'1F',-Z'71',-Z'1B',-Z'07',-Z'71',-Z'5D',-Z'09',-Z'13',-Z'05',-Z'01',+Z'1F',&
               -Z'38',-Z'04',+Z'3F',-Z'01',-Z'61',-Z'11',-Z'04',-Z'01',-Z'3D',-Z'08',-Z'80',-Z'80',+Z'50',+Z'0E',+Z'02',+Z'40',&
               +Z'51',+Z'00',+Z'20',+Z'60',+Z'44',+Z'09',-Z'7F',+Z'50',+Z'2C',+Z'40',-Z'78',+Z'28',+Z'1D',+Z'03',-Z'20',-Z'7C',&
               +Z'11',+Z'10',+Z'42',+Z'0E',+Z'0A',+Z'41',+Z'58',+Z'2D',+Z'05',-Z'3C',+Z'18',-Z'7C',-Z'7D',+Z'50',+Z'6E',+Z'0E',&
               +Z'41',-Z'2F',+Z'08',+Z'21',+Z'61',+Z'04',+Z'21',-Z'7C',+Z'50',-Z'74',+Z'42',-Z'78',+Z'68',+Z'4D',+Z'09',-Z'1F',&
               +Z'44',+Z'29',+Z'10',-Z'3E',+Z'1E',+Z'16',+Z'42',-Z'28',+Z'5D',+Z'0B',-Z'3C',+Z'38',-Z'78',-Z'7A',+Z'50',-Z'32',&
               +Z'1A',+Z'43',+Z'51',+Z'10',+Z'22',+Z'61',-Z'3C',+Z'39',-Z'79',+Z'50',-Z'14',+Z'44',-Z'78',-Z'58',+Z'7D',+Z'0F',&
               -Z'1E',+Z'04',+Z'41',+Z'11',+Z'42',+Z'2E',+Z'22',+Z'44',+Z'58',-Z'73',+Z'11',-Z'3C',+Z'58',-Z'74',-Z'77',+Z'51',&
               +Z'2E',+Z'26',+Z'44',-Z'2F',+Z'18',+Z'23',+Z'62',-Z'7C',+Z'51',-Z'76',+Z'51',+Z'4C',+Z'46',-Z'78',-Z'18',-Z'53',&
               +Z'15',-Z'1E',-Z'3C',+Z'59',+Z'11',-Z'3E',+Z'3E',+Z'2E',+Z'45',-Z'28',-Z'43',+Z'17',-Z'3C',+Z'78',-Z'70',-Z'74',&
               +Z'51',-Z'72',+Z'32',+Z'46',+Z'51',+Z'20',+Z'24',+Z'63',+Z'44',+Z'69',-Z'73',+Z'51',-Z'54',+Z'48',-Z'77',+Z'28',&
               -Z'23',+Z'1B',-Z'1D',-Z'7C',+Z'71',+Z'12',+Z'42',+Z'4E',+Z'3A',+Z'47',+Z'58',-Z'13',+Z'1D',-Z'3C',-Z'68',-Z'6C',&
               -Z'71',+Z'51',-Z'12',+Z'3E',+Z'47',-Z'2F',+Z'28',+Z'25',+Z'64',+Z'04',-Z'7F',-Z'70',+Z'52',+Z'0C',+Z'4A',-Z'77',&
               +Z'69',+Z'0D',+Z'21',-Z'1C',+Z'44',-Z'77',+Z'12',-Z'3E',+Z'5E',+Z'46',+Z'48',-Z'27',+Z'1D',+Z'23',-Z'3C',-Z'48',&
               -Z'68',-Z'6E',+Z'52',+Z'4E',+Z'4A',+Z'49',+Z'51',+Z'30',+Z'26',+Z'64',-Z'3C',-Z'67',-Z'6D',+Z'52',+Z'6C',+Z'4C',&
               -Z'77',-Z'57',+Z'3D',+Z'27',-Z'1B',+Z'04',-Z'5F',+Z'13',+Z'42',+Z'6E',+Z'52',+Z'4A',+Z'59',+Z'4D',+Z'29',-Z'3C',&
               -Z'28',-Z'64',-Z'6B',+Z'52',-Z'52',+Z'56',+Z'4A',-Z'2F',+Z'38',+Z'27',+Z'65',-Z'7C',-Z'4F',-Z'6A',+Z'52',-Z'34',&
               +Z'4E',-Z'77',-Z'17',+Z'6D',+Z'2D',-Z'1B',-Z'3C',-Z'47',+Z'13',-Z'3E',+Z'7E',+Z'5E',+Z'4B',-Z'27',+Z'7D',+Z'2F',&
               -Z'3C',-Z'08',-Z'60',-Z'68',+Z'53',+Z'0E',+Z'62',+Z'4C',+Z'51',+Z'40',+Z'28',+Z'66',+Z'44',-Z'37',-Z'67',+Z'53',&
               +Z'2C',+Z'50',-Z'76',+Z'29',-Z'63',+Z'33',-Z'1A',-Z'7C',-Z'2F',+Z'14',+Z'42',-Z'72',+Z'6A',+Z'4D',+Z'59',-Z'53',&
               +Z'35',-Z'3B',+Z'18',-Z'5C',-Z'65',+Z'53',+Z'6E',+Z'6E',+Z'4D',-Z'2F',+Z'48',+Z'29',+Z'67',+Z'04',-Z'1F',-Z'64',&
               +Z'53',-Z'74',+Z'52',-Z'76',+Z'69',-Z'33',+Z'39',-Z'19',+Z'44',-Z'17',+Z'14',-Z'3E',-Z'62',+Z'76',+Z'4E',-Z'27',&
               -Z'23',+Z'3B',-Z'3B',+Z'38',-Z'58',-Z'62',+Z'53',-Z'32',+Z'7A',+Z'4F',+Z'51',+Z'50',+Z'2A',+Z'67',-Z'3C',-Z'07',&
               -Z'61',+Z'53',-Z'14',+Z'54',-Z'76',-Z'57',-Z'03',+Z'3F',-Z'18',+Z'05',+Z'01',+Z'15',+Z'42',-Z'52',-Z'7E',+Z'50',&
               +Z'5A',+Z'0D',+Z'41',-Z'3B',+Z'58',-Z'54',-Z'5F',+Z'54',+Z'2E',-Z'7A',+Z'50',-Z'2F',+Z'58',+Z'2B',+Z'68',-Z'7B',&
               +Z'11',-Z'5E',+Z'54',+Z'4C',+Z'56',-Z'76',-Z'16',+Z'2D',+Z'45',-Z'18',-Z'3B',+Z'19',+Z'15',-Z'3E',-Z'42',-Z'72',&
               +Z'51',-Z'26',+Z'3D',+Z'47',-Z'3B',+Z'78',-Z'50',-Z'5C',+Z'54',-Z'72',-Z'6E',+Z'52',+Z'51',+Z'60',+Z'2C',+Z'69',&
               +Z'45',+Z'29',-Z'5B',+Z'54',-Z'54',+Z'58',-Z'75',+Z'2A',+Z'5D',+Z'4B',-Z'17',-Z'7B',+Z'31',+Z'16',+Z'42',-Z'32',&
               -Z'66',+Z'53',+Z'5A',+Z'6D',+Z'4D',-Z'3B',-Z'68',-Z'4C',-Z'59',+Z'54',-Z'12',-Z'62',+Z'53',-Z'2F',+Z'68',+Z'2D',&
               +Z'6A',+Z'05',+Z'41',-Z'58',+Z'55',+Z'0C',+Z'5A',-Z'75',+Z'6A',-Z'73',+Z'51',-Z'16',+Z'45',+Z'49',+Z'16',-Z'3E',&
               -Z'22',-Z'5A',+Z'54',-Z'26',-Z'63',+Z'53',-Z'3B',-Z'48',-Z'48',-Z'56',+Z'55',+Z'4E',-Z'56',+Z'55',+Z'51',+Z'70',&
               +Z'2E',+Z'6A',-Z'3B',+Z'59',-Z'55',+Z'55',+Z'6C',+Z'5C',-Z'75',-Z'56',-Z'43',+Z'57',-Z'15',+Z'05',+Z'61',+Z'17',&
               +Z'42',-Z'12',-Z'4E',+Z'56',+Z'5A',-Z'33',+Z'59',-Z'3B',-Z'28',-Z'44',-Z'53',+Z'55',-Z'52',-Z'4A',+Z'56',-Z'2F',&
               +Z'78',+Z'2F',+Z'6B',-Z'7B',+Z'71',-Z'52',+Z'55',-Z'34',+Z'5E',-Z'75',-Z'16',-Z'13',+Z'5D',-Z'15',-Z'3B',+Z'79',&
               +Z'17',-Z'3E',-Z'02',-Z'42',+Z'57',-Z'26',-Z'03',+Z'5F',-Z'3B',-Z'08',-Z'40',-Z'50',+Z'56',+Z'0E',-Z'3E',+Z'58',&
               +Z'51',-Z'80',+Z'30',+Z'6C',+Z'45',-Z'77',-Z'4F',+Z'56',+Z'2C',+Z'60',-Z'74',+Z'2B',+Z'1D',+Z'63',-Z'14',-Z'7B',&
               -Z'6F',+Z'18',+Z'43',+Z'0E',-Z'36',+Z'59',+Z'5B',+Z'2D',+Z'65',-Z'3A',+Z'18',-Z'3C',-Z'4D',+Z'56',+Z'6E',-Z'32',&
               +Z'59',-Z'2F',-Z'78',+Z'31',+Z'6D',+Z'05',-Z'5F',-Z'4C',+Z'56',-Z'74',+Z'62',-Z'74',+Z'6B',+Z'4D',+Z'69',-Z'13',&
               +Z'45',-Z'57',+Z'18',-Z'3D',+Z'1E',-Z'2A',+Z'5A',-Z'25',+Z'5D',+Z'6B',-Z'3A',+Z'38',-Z'38',-Z'4A',+Z'56',-Z'32',&
               -Z'26',+Z'5B',+Z'51',-Z'70',+Z'32',+Z'6D',-Z'3B',-Z'47',-Z'49',+Z'56',-Z'14',+Z'64',-Z'74',-Z'55',+Z'7D',+Z'6F',&
               -Z'12',+Z'05',-Z'3F',+Z'19',+Z'43',+Z'2E',-Z'1E',+Z'5C',+Z'5B',-Z'73',+Z'71',-Z'3A',+Z'58',-Z'34',-Z'47',+Z'57',&
               +Z'2E',-Z'1A',+Z'5C',-Z'2F',-Z'68',+Z'33',+Z'6E',-Z'7B',-Z'2F',-Z'46',+Z'57',+Z'4C',+Z'66',-Z'74',-Z'15',-Z'53',&
               +Z'75',-Z'12',-Z'3B',-Z'27',+Z'19',-Z'3D',+Z'3E',-Z'12',+Z'5D',-Z'25',-Z'43',+Z'77',-Z'3A',+Z'78',-Z'30',-Z'44',&
               +Z'57',-Z'72',-Z'0E',+Z'5E',+Z'51',-Z'60',+Z'34',+Z'6F',+Z'45',-Z'17',-Z'43',+Z'57',-Z'54',+Z'68',-Z'73',+Z'2B',&
               -Z'23',+Z'7B',-Z'11',-Z'7B',-Z'0F',+Z'1A',+Z'43',+Z'4E',-Z'06',+Z'5F',+Z'5B',-Z'13',+Z'7D',-Z'3A',-Z'68',-Z'2C',&
               -Z'41',+Z'57',-Z'12',-Z'02',+Z'5F',-Z'2F',-Z'58',+Z'35',+Z'70',+Z'06',+Z'01',-Z'40',+Z'58',+Z'0C',+Z'6A',-Z'73',&
               +Z'6C',+Z'0D',-Z'7F',-Z'10',+Z'46',+Z'09',+Z'1A',-Z'3D',+Z'5F',+Z'06',+Z'60',-Z'24',+Z'1D',-Z'7D',-Z'3A',-Z'48',&
               -Z'28',-Z'3E',+Z'58',+Z'4F',+Z'0A',+Z'61',+Z'51',-Z'50',+Z'36',+Z'70',-Z'3A',+Z'19',-Z'3D',+Z'58',+Z'6C',+Z'6C',&
               -Z'73',-Z'54',+Z'3D',-Z'79',-Z'0F',+Z'06',+Z'21',+Z'1B',+Z'43',+Z'6F',+Z'12',+Z'62',+Z'5C',+Z'4D',-Z'77',-Z'3A',&
               -Z'28',-Z'24',-Z'3B',+Z'58',-Z'51',+Z'16',+Z'62',-Z'2F',-Z'48',+Z'37',+Z'71',-Z'7A',+Z'31',-Z'3A',+Z'58',-Z'34',&
               +Z'6E',-Z'73',-Z'14',+Z'6D',-Z'73',-Z'0F',-Z'3A',+Z'39',+Z'1B',-Z'3D',+Z'7F',+Z'1E',+Z'63',-Z'24',+Z'7D',-Z'71',&
               -Z'3A',-Z'08',-Z'20',-Z'38',+Z'59',+Z'0F',+Z'22',+Z'64',+Z'51',-Z'40',+Z'38',+Z'72',+Z'46',+Z'49',-Z'37',+Z'59',&
               +Z'2C',+Z'70',-Z'72',+Z'2C',-Z'63',-Z'6D',-Z'0E',-Z'7A',+Z'51',+Z'1C',+Z'43',-Z'71',+Z'2A',+Z'65',+Z'5C',-Z'53',&
               -Z'6B',-Z'39',+Z'18',-Z'1C',-Z'35',+Z'59',+Z'6F',+Z'2E',+Z'65',-Z'2F',-Z'38',+Z'39',+Z'73',+Z'06',+Z'61',-Z'34',&
               +Z'59',-Z'74',+Z'72',-Z'72',+Z'6C',-Z'33',-Z'67',-Z'0D',+Z'46',+Z'69',+Z'1C',-Z'3D',-Z'61',+Z'36',+Z'66',-Z'24',&
               -Z'23',-Z'65',-Z'39',+Z'38',-Z'18',-Z'32',+Z'59',-Z'31',+Z'3A',+Z'67',+Z'51',-Z'30',+Z'3A',+Z'73',-Z'3A',+Z'79',&
               -Z'31',+Z'59',-Z'14',+Z'74',-Z'72',-Z'54',-Z'03',-Z'61',-Z'0C',+Z'06',-Z'7F',+Z'1D',+Z'43',-Z'51',+Z'42',+Z'68',&
               +Z'5D',+Z'0D',-Z'5F',-Z'39',+Z'58',-Z'14',-Z'2F',+Z'5A',+Z'2F',+Z'46',+Z'68',-Z'2F',-Z'28',+Z'3B',+Z'74',-Z'7A',&
               -Z'6F',-Z'2E',+Z'5A',+Z'4C',+Z'76',-Z'72',-Z'13',+Z'2D',-Z'5B',-Z'0C',-Z'3A',-Z'67',+Z'1D',-Z'3D',-Z'41',+Z'4E',&
               +Z'69',-Z'23',+Z'3D',-Z'59',-Z'39',+Z'78',-Z'10',-Z'2C',+Z'5A',-Z'71',+Z'52',+Z'6A',+Z'51',-Z'20',+Z'3C',+Z'75',&
               +Z'46',-Z'57',-Z'2B',+Z'5A',-Z'54',+Z'78',-Z'71',+Z'2D',+Z'5D',-Z'55',-Z'0B',-Z'7A',-Z'4F',+Z'1E',+Z'43',-Z'31',&
               +Z'5A',+Z'6B',+Z'5D',+Z'6D',-Z'53',-Z'39',-Z'68',-Z'0C',-Z'29',+Z'5A',-Z'11',+Z'5E',+Z'6B',-Z'2F',-Z'18',+Z'3D',&
               +Z'76',+Z'06',-Z'3F',-Z'28',+Z'5B',+Z'0C',+Z'7A',-Z'71',+Z'6D',-Z'73',-Z'4F',-Z'0A',+Z'46',-Z'37',+Z'1E',-Z'3D',&
               -Z'21',+Z'66',+Z'6C',-Z'23',-Z'63',-Z'4D',-Z'39',-Z'48',-Z'08',-Z'26',+Z'5B',+Z'4F',+Z'6A',+Z'6D',+Z'51',-Z'10',&
               +Z'3E',+Z'76',-Z'3A',-Z'27',-Z'25',+Z'5B',+Z'6C',+Z'7C',-Z'71',-Z'53',-Z'43',-Z'49',-Z'09',+Z'06',-Z'1F',+Z'1F',&
               +Z'43',-Z'11',+Z'72',+Z'6E',+Z'5D',-Z'33',-Z'47',-Z'39',-Z'28',-Z'04',-Z'23',+Z'5B',-Z'51',+Z'76',+Z'6E',-Z'2F',&
               -Z'08',+Z'3F',+Z'77',-Z'7A',-Z'0F',-Z'22',+Z'5B',-Z'34',+Z'7E',-Z'71',-Z'13',-Z'13',-Z'43',-Z'09',-Z'3A',-Z'07',&
               +Z'1F',-Z'3D',-Z'01',+Z'7E',+Z'6F',-Z'23',-Z'03',-Z'41',-Z'39',-Z'07',+Z'01',+Z'00',+Z'10',+Z'00',-Z'02',+Z'00',&
               +Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'00',+Z'10',+Z'00',+Z'00',+Z'01',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'02',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',+Z'00',+Z'00',+Z'03',+Z'01',&
               +Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'05',+Z'00',+Z'00',+Z'00',+Z'06',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'0D',+Z'01',+Z'02',+Z'00',+Z'47',+Z'00',+Z'00',+Z'00',-Z'5E',+Z'08',&
               +Z'00',+Z'00',+Z'11',+Z'01',+Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',+Z'00',+Z'00',+Z'12',+Z'01',&
               +Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'15',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'16',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'40',+Z'00',&
               +Z'00',+Z'00',+Z'17',+Z'01',+Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',-Z'3D',+Z'07',+Z'00',+Z'00',+Z'1A',+Z'01',&
               +Z'05',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',-Z'6E',+Z'08',+Z'00',+Z'00',+Z'1B',+Z'01',+Z'05',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',-Z'66',+Z'08',+Z'00',+Z'00',+Z'1C',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'28',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',&
               +Z'00',+Z'00',+Z'48',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'48',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'43',+Z'3A',+Z'5C',+Z'55',+Z'73',+Z'65',+Z'72',+Z'73',+Z'5C',+Z'57',+Z'69',+Z'6C',+Z'6C',+Z'5C',&
               +Z'44',+Z'6F',+Z'63',+Z'75',+Z'6D',+Z'65',+Z'6E',+Z'74',+Z'73',+Z'5C',+Z'63',+Z'72',+Z'79',+Z'73',+Z'74',+Z'61',&
               +Z'6C',+Z'6C',+Z'6F',+Z'67',+Z'72',+Z'61',+Z'70',+Z'68',+Z'79',+Z'5F',+Z'62',+Z'75',+Z'69',+Z'6C',+Z'64',+Z'32',&
               +Z'5C',+Z'70',+Z'79',+Z'74',+Z'68',+Z'6F',+Z'6E',+Z'5C',+Z'52',+Z'65',+Z'6C',+Z'65',+Z'61',+Z'73',+Z'65',+Z'5C',&
               +Z'72',+Z'61',+Z'6D',+Z'70',+Z'2E',+Z'74',+Z'69',+Z'66',+Z'00']

    ! fill big endian tiff buffer
    bgBuff = [+Z'4D',+Z'4D',+Z'00',+Z'2A',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',+Z'0A',+Z'00',-Z'02',+Z'00',+Z'04',+Z'00',+Z'00',&
              +Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',&
              +Z'00',+Z'04',+Z'01',+Z'01',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'04',+Z'01',+Z'02',&
              +Z'00',+Z'03',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'10',+Z'00',+Z'00',+Z'01',+Z'06',+Z'00',+Z'03',+Z'00',+Z'00',&
              +Z'00',+Z'01',+Z'00',+Z'01',+Z'00',+Z'00',+Z'01',+Z'0E',+Z'00',+Z'02',+Z'00',+Z'00',+Z'00',+Z'22',+Z'00',+Z'00',&
              +Z'00',-Z'7A',+Z'01',+Z'11',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',-Z'58',+Z'01',+Z'15',&
              +Z'00',+Z'03',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'01',+Z'00',+Z'00',+Z'01',+Z'16',+Z'00',+Z'03',+Z'00',+Z'00',&
              +Z'00',+Z'01',+Z'00',+Z'04',+Z'00',+Z'00',+Z'01',+Z'17',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',&
              +Z'00',+Z'20',+Z'00',+Z'00',+Z'00',+Z'00',+Z'49',+Z'6D',+Z'61',+Z'67',+Z'65',+Z'4A',+Z'3D',+Z'31',+Z'2E',+Z'35',&
              +Z'30',+Z'69',+Z'0A',+Z'6D',+Z'69',+Z'6E',+Z'3D',+Z'30',+Z'2E',+Z'30',+Z'0A',+Z'6D',+Z'61',+Z'78',+Z'3D',+Z'31',&
              +Z'35',+Z'33',+Z'30',+Z'30',+Z'2E',+Z'30',+Z'0A',+Z'00',+Z'00',+Z'00',+Z'04',+Z'00',+Z'08',+Z'00',+Z'0C',+Z'00',&
              +Z'10',+Z'00',+Z'14',+Z'00',+Z'18',+Z'00',+Z'1C',+Z'00',+Z'20',+Z'00',+Z'24',+Z'00',+Z'28',+Z'00',+Z'2C',+Z'00',&
              +Z'30',+Z'00',+Z'34',+Z'00',+Z'38',+Z'00',+Z'3C',+Z'00']

    ! fill little endian tiff buffer
    ltBuff = [+Z'49',+Z'49',+Z'2A',+Z'00',+Z'08',+Z'00',+Z'00',+Z'00',+Z'0A',+Z'00',-Z'02',+Z'00',+Z'04',+Z'00',+Z'01',+Z'00',&
              +Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'01',+Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'04',+Z'00',&
              +Z'00',+Z'00',+Z'01',+Z'01',+Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'02',+Z'01',&
              +Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'10',+Z'00',+Z'00',+Z'00',+Z'06',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
              +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'0E',+Z'01',+Z'02',+Z'00',+Z'22',+Z'00',+Z'00',+Z'00',-Z'7A',+Z'00',&
              +Z'00',+Z'00',+Z'11',+Z'01',+Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',-Z'58',+Z'00',+Z'00',+Z'00',+Z'15',+Z'01',&
              +Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'16',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
              +Z'00',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'17',+Z'01',+Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'20',+Z'00',&
              +Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'49',+Z'6D',+Z'61',+Z'67',+Z'65',+Z'4A',+Z'3D',+Z'31',+Z'2E',+Z'35',&
              +Z'30',+Z'69',+Z'0A',+Z'6D',+Z'69',+Z'6E',+Z'3D',+Z'30',+Z'2E',+Z'30',+Z'0A',+Z'6D',+Z'61',+Z'78',+Z'3D',+Z'31',&
              +Z'35',+Z'33',+Z'30',+Z'30',+Z'2E',+Z'30',+Z'0A',+Z'00',+Z'00',+Z'00',+Z'00',+Z'04',+Z'00',+Z'08',+Z'00',+Z'0C',&
              +Z'00',+Z'10',+Z'00',+Z'14',+Z'00',+Z'18',+Z'00',+Z'1C',+Z'00',+Z'20',+Z'00',+Z'24',+Z'00',+Z'28',+Z'00',+Z'2C',&
              +Z'00',+Z'30',+Z'00',+Z'34',+Z'00',+Z'38',+Z'00',+Z'3C']

    ! rgb grayscale tif buffer
    rgbBuff = [+Z'49',+Z'49',+Z'2A',+Z'00',+Z'38',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'01',+Z'01',+Z'01',+Z'02',+Z'02',&
               +Z'02',+Z'03',+Z'03',+Z'03',+Z'04',+Z'04',+Z'04',+Z'05',+Z'05',+Z'05',+Z'06',+Z'06',+Z'06',+Z'07',+Z'07',+Z'07',&
               +Z'08',+Z'08',+Z'08',+Z'09',+Z'09',+Z'09',+Z'0A',+Z'0A',+Z'0A',+Z'0B',+Z'0B',+Z'0B',+Z'0C',+Z'0C',+Z'0C',+Z'0D',&
               +Z'0D',+Z'0D',+Z'0E',+Z'0E',+Z'0E',+Z'0F',+Z'0F',+Z'0F',+Z'10',+Z'00',-Z'02',+Z'00',+Z'04',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'10',+Z'00',&
               +Z'00',+Z'00',+Z'01',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'01',&
               +Z'03',+Z'00',+Z'03',+Z'00',+Z'00',+Z'00',+Z'0E',+Z'01',+Z'00',+Z'00',+Z'03',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'06',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'00',&
               +Z'00',+Z'00',+Z'0D',+Z'01',+Z'02',+Z'00',+Z'46',+Z'00',+Z'00',+Z'00',+Z'14',+Z'01',+Z'00',+Z'00',+Z'11',+Z'01',&
               +Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',+Z'00',+Z'00',+Z'12',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'15',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'03',+Z'00',&
               +Z'00',+Z'00',+Z'16',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'40',+Z'00',+Z'00',+Z'00',+Z'17',+Z'01',&
               +Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'30',+Z'00',+Z'00',+Z'00',+Z'1A',+Z'01',+Z'05',+Z'00',+Z'01',+Z'00',&
               +Z'00',+Z'00',-Z'02',+Z'00',+Z'00',+Z'00',+Z'1B',+Z'01',+Z'05',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'06',+Z'01',&
               +Z'00',+Z'00',+Z'1C',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'28',+Z'01',&
               +Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'48',+Z'00',&
               +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'48',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',&
               +Z'08',+Z'00',+Z'08',+Z'00',+Z'43',+Z'3A',+Z'5C',+Z'55',+Z'73',+Z'65',+Z'72',+Z'73',+Z'5C',+Z'57',+Z'69',+Z'6C',&
               +Z'6C',+Z'5C',+Z'44',+Z'6F',+Z'63',+Z'75',+Z'6D',+Z'65',+Z'6E',+Z'74',+Z'73',+Z'5C',+Z'63',+Z'72',+Z'79',+Z'73',&
               +Z'74',+Z'61',+Z'6C',+Z'6C',+Z'6F',+Z'67',+Z'72',+Z'61',+Z'70',+Z'68',+Z'79',+Z'5F',+Z'62',+Z'75',+Z'69',+Z'6C',&
               +Z'64',+Z'32',+Z'5C',+Z'70',+Z'79',+Z'74',+Z'68',+Z'6F',+Z'6E',+Z'5C',+Z'52',+Z'65',+Z'6C',+Z'65',+Z'61',+Z'73',&
               +Z'65',+Z'5C',+Z'72',+Z'67',+Z'62',+Z'2E',+Z'74',+Z'69',+Z'66',+Z'00']

    !rgba grayscale tif buffer
    rgbaBuff = [+Z'49',+Z'49',+Z'2A',+Z'00',+Z'48',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',-Z'01',+Z'01',+Z'01',+Z'01',-Z'01',&
                +Z'02',+Z'02',+Z'02',-Z'01',+Z'03',+Z'03',+Z'03',-Z'01',+Z'04',+Z'04',+Z'04',-Z'01',+Z'05',+Z'05',+Z'05',-Z'01',&
                +Z'06',+Z'06',+Z'06',-Z'01',+Z'07',+Z'07',+Z'07',-Z'01',+Z'08',+Z'08',+Z'08',-Z'01',+Z'09',+Z'09',+Z'09',-Z'01',&
                +Z'0A',+Z'0A',+Z'0A',-Z'01',+Z'0B',+Z'0B',+Z'0B',-Z'01',+Z'0C',+Z'0C',+Z'0C',-Z'01',+Z'0D',+Z'0D',+Z'0D',-Z'01',&
                +Z'0E',+Z'0E',+Z'0E',-Z'01',+Z'0F',+Z'0F',+Z'0F',-Z'01',+Z'11',+Z'00',-Z'02',+Z'00',+Z'04',+Z'00',+Z'01',+Z'00',&
                +Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'10',+Z'00',&
                +Z'00',+Z'00',+Z'01',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'01',&
                +Z'03',+Z'00',+Z'04',+Z'00',+Z'00',+Z'00',+Z'2A',+Z'01',+Z'00',+Z'00',+Z'03',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
                +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'06',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'00',&
                +Z'00',+Z'00',+Z'0D',+Z'01',+Z'02',+Z'00',+Z'47',+Z'00',+Z'00',+Z'00',+Z'32',+Z'01',+Z'00',+Z'00',+Z'11',+Z'01',&
                +Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',+Z'00',+Z'00',+Z'12',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
                +Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'15',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'04',+Z'00',&
                +Z'00',+Z'00',+Z'16',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'40',+Z'00',+Z'00',+Z'00',+Z'17',+Z'01',&
                +Z'04',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'40',+Z'00',+Z'00',+Z'00',+Z'1A',+Z'01',+Z'05',+Z'00',+Z'01',+Z'00',&
                +Z'00',+Z'00',+Z'1A',+Z'01',+Z'00',+Z'00',+Z'1B',+Z'01',+Z'05',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'22',+Z'01',&
                +Z'00',+Z'00',+Z'1C',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'28',+Z'01',&
                +Z'03',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'02',+Z'00',+Z'00',+Z'00',+Z'52',+Z'01',+Z'03',+Z'00',+Z'01',+Z'00',&
                +Z'00',+Z'00',+Z'02',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'00',+Z'48',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',&
                +Z'00',+Z'00',+Z'48',+Z'00',+Z'00',+Z'00',+Z'01',+Z'00',+Z'00',+Z'00',+Z'08',+Z'00',+Z'08',+Z'00',+Z'08',+Z'00',&
                +Z'08',+Z'00',+Z'43',+Z'3A',+Z'5C',+Z'55',+Z'73',+Z'65',+Z'72',+Z'73',+Z'5C',+Z'57',+Z'69',+Z'6C',+Z'6C',+Z'5C',&
                +Z'44',+Z'6F',+Z'63',+Z'75',+Z'6D',+Z'65',+Z'6E',+Z'74',+Z'73',+Z'5C',+Z'63',+Z'72',+Z'79',+Z'73',+Z'74',+Z'61',&
                +Z'6C',+Z'6C',+Z'6F',+Z'67',+Z'72',+Z'61',+Z'70',+Z'68',+Z'79',+Z'5F',+Z'62',+Z'75',+Z'69',+Z'6C',+Z'64',+Z'32',&
                +Z'5C',+Z'70',+Z'79',+Z'74',+Z'68',+Z'6F',+Z'6E',+Z'5C',+Z'52',+Z'65',+Z'6C',+Z'65',+Z'61',+Z'73',+Z'65',+Z'5C',&
                +Z'72',+Z'67',+Z'62',+Z'61',+Z'2E',+Z'74',+Z'69',+Z'66',+Z'00']

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      2d tif io and conversion                      !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    write(*,*) "testing 2d scalar TIFF conversion and io"
    ! test 2d int8 conversion,read, and write
    write(*,*) "  i8 "
    im = image_t(reshape(i8 , shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_i8 , dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i8 , dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i8_2) ! extract data
    i8_1  = reshape(i8_2, [15])
    do i = 1,15
      if(i8_1 (i).ne.i8 (i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 2d int16 conversion,read, and write
    write(*,*) "  i16"
    im = image_t(reshape(i16, shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_i16, dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i16, dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i16_2) ! extract data
    i16_1 = reshape(i16_2, [15])
    do i = 1,15
      if(i16_1(i).ne.i16(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 2d int32 conversion,read, and write
    write(*,*) "  i32"
    im = image_t(reshape(i32, shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_i32, dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i32, dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i32_2) ! extract data
    i32_1 = reshape(i32_2, [15])
    do i = 1,15
      if(i32_1(i).ne.i32(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 2d int64 conversion,read, and write
    write(*,*) "  i64"
    im = image_t(reshape(i64, shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_i64, dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i64, dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i64_2) ! extract data
    i64_1 = reshape(i64_2, [15])
    do i = 1,15
      if(i64_1(i).ne.i64(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 2d real32 conversion,read, and write
    write(*,*) "  r32"
    im = image_t(reshape(r32, shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_r32, dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_r32, dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(r32_2) ! extract data
    r32_1 = reshape(r32_2, [15])
    do i = 1,15
      if(r32_1(i).ne.r32(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 2d real64 conversion,read, and write
    write(*,*) "  r64"
    im = image_t(reshape(r64, shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_r64, dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_r64, dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(r64_2) ! extract data
    r64_1 = reshape(r64_2, [15])
    do i = 1,15
      if(r64_1(i).ne.r64(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      3d tif io and conversion                      !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    write(*,*) "testing 3d scalar TIFF conversion and io"
    ! test 3d int8 conversion,read, and write
    write(*,*) "  i8 "
    im = image_t(reshape(i8 , shape(shape3))) ! convert test data to image_t
    result = check_image(im, pix_i8 , dim3) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i8 , dim3) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i8_3 ) ! extract data
    i8_1  = reshape(i8_3 , [15])
    do i = 1,15
      if(i8_1 (i).ne.i8 (i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 3d int16 conversion,read, and write
    write(*,*) "  i16"
    im = image_t(reshape(i16, shape(shape3))) ! convert test data to image_t
    result = check_image(im, pix_i16, dim3) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i16, dim3) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i16_3) ! extract data
    i16_1 = reshape(i16_3, [15])
    do i = 1,15
      if(i16_1(i).ne.i16(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 3d int32 conversion,read, and write
    write(*,*) "  i32"
    im = image_t(reshape(i32, shape(shape3))) ! convert test data to image_t
    result = check_image(im, pix_i32, dim3) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i32, dim3) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i32_3) ! extract data
    i32_1 = reshape(i32_3, [15])
    do i = 1,15
      if(i32_1(i).ne.i32(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 3d int64 conversion,read, and write
    write(*,*) "  i64"
    im = image_t(reshape(i64, shape(shape3))) ! convert test data to image_t
    result = check_image(im, pix_i64, dim3) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_i64, dim3) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i64_3) ! extract data
    i64_1 = reshape(i64_3, [15])
    do i = 1,15
      if(i64_1(i).ne.i64(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 3d real32 conversion,read, and write
    write(*,*) "  r32"
    im = image_t(reshape(r32, shape(shape3))) ! convert test data to image_t
    result = check_image(im, pix_r32, dim3) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_r32, dim3) ! check reading metadata
    if(result.ne.0) return
    call im%getData(r32_3) ! extract data
    r32_1 = reshape(r32_3, [15])
    do i = 1,15
      if(r32_1(i).ne.r32(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    ! test 3d real64 conversion,read, and write
    write(*,*) "  r64"
    im = image_t(reshape(r64, shape(shape3))) ! convert test data to image_t
    result = check_image(im, pix_r64, dim3) ! check conversion
    if(result.ne.0) return
    call im%write("test.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.tif") ! read back from file
    result = check_image(im, pix_r64, dim3) ! check reading metadata
    if(result.ne.0) return
    call im%getData(r64_3) ! extract data
    r64_1 = reshape(r64_3, [15])
    do i = 1,15
      if(r64_1(i).ne.r64(i)) then
        write(*,*) "incorrect image data"
        result = 5
        return
      endif
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      other tiff functionality                      !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! lzw
    write(*,*) "testing lzw decompression"
    open(newunit = i, file = 'test.tif', access = 'stream') ! open file
    write(i) lzwBuff ! write lzw compressed tiff to file
    close(i) ! close file
    im = image_t("test.tif") ! read back lzw compressed tif from file
    call im%getData(i8_2) ! extract data
    c = -1
    do i = 1,4096 ! tiff is 4096x1 ramp from 0 -> 255
      if(mod(i-1,16).eq.0) c = c+1
      if(c.ne.i8_2(i,1)) then
        write(*,*) "incorrect lzw decompressed data"
        result = 6
        return
      endif
    enddo

    ! big endian
    write(*,*) "testing tif big endian compatibility"
    open(newunit = i, file = 'test.tif', access = 'stream') ! open file
    write(i) bgBuff ! write big endian tiff to file
    close(i) ! close file
    im = image_t("test.tif") ! read back big endian tif from file
    call im%getData(i16_2) ! extract data
    i16_1 = reshape(i16_2, [16])
    s = 0
    do i = 1, 16 ! tif is 4x4 ramp from 0->15360 in 1024 increments
      if(i16_1(i).ne.s) then
        write(*,*) "error reading big endian tif"
        result = 6
        return
      endif
      s = s + 1024
    enddo

    ! little endian
    write(*,*) "testing tif little endian compatibility"
    open(newunit = i, file = 'test.tif', access = 'stream') ! open file
    write(i) ltBuff ! write little endian tiff to file
    close(i) ! close file
    im = image_t("test.tif") ! read back little endian tif from file
    call im%getData(i16_2) ! extract data
    i16_1 = reshape(i16_2, [16])
    s = 0
    do i = 1, 16 ! tif is 4x4 ramp from 0->15360 in 1024 increments
      if(i16_1(i).ne.s) then
        write(*,*) "error reading little endian tif"
        result = 6
        return
      endif
      s = s + 1024
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                    other image_t functionality                     !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! check clear and empty
    write(*,*) "testing image_t clear(), empty(), and size()"
    im = image_t(reshape(i8 , shape(shape3)))
    if(im%empty()) then
      write(*,*) "allocated image shouldn't be empty"
      result = 7
      return
    endif
    call im%clear()
    if(.not.im%empty()) then
      write(*,*) "cleared image should be empty"
      result = 7
      return
    endif

    ! check size
    im = image_t(reshape(i8 , shape(shape2)))
    if(15.ne.im%size()) then
      write(*,*) "incorrect 2d size"
      result = 8
      return
    endif
    im = image_t(reshape(i8 , shape(shape3)))
    if(105.ne.im%size()) then
      write(*,*) "incorrect 2d size"
      result = 8
      return
    endif

    ! rgb -> gray
    write(*,*) "testing rgb -> gray conversion"
    open(newunit = i, file = 'test.tif', access = 'stream') ! open file
    write(i) rgbBuff ! write false color rgb tiff
    close(i) ! close file
    im = image_t("test.tif") ! read back little endian tif from file
    call im%getData(i8_2) ! extract data
    do i = 1, 16 ! tif is 16x1 ramp from 0->15
      if(i8_2(i,1).ne.(i-1)) then
        write(*,*) i, i8_2(i,1)
        write(*,*) "error reading little endian tif"
        result = 9
        return
      endif
      s = s + 1024
    enddo

    ! rgba -> gray
    write(*,*) "testing rgba -> gray conversion"
    open(newunit = i, file = 'test.tif', access = 'stream') ! open file
    write(i) rgbaBuff ! write false color rgb tiff
    close(i) ! close file
    im = image_t("test.tif") ! read back little endian tif from file
    call im%getData(i8_2) ! extract data
    do i = 1, 16 ! tif is 16x1 ramp from 0->15
      if(i8_2(i,1).ne.(i-1)) then
        write(*,*) i, i8_2(i,1)
        write(*,*) "error reading little endian tif"
        result = 9
        return
      endif
      s = s + 1024
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                        2d safe type casting                        !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    write(*,*) "testing 2d image_t casting"

    ! int8->
    write(*,*) "  int8 -> ___"
    im = image_t(reshape(i8 , shape(shape2)))
    call im%getData(i8_2 )
    if(.not.allocated(i8_2 )) then
      write(*,*) "couldn't cast int8 ->int8 "
      result = 10
      return
    endif
    call im%getData(i16_2)
    if(.not.allocated(i16_2)) then
      write(*,*) "couldn't cast int8 ->int16"
      result = 10
      return
    endif
    call im%getData(i32_2)
    if(.not.allocated(i32_2)) then
      write(*,*) "couldn't cast int8 ->int32"
      result = 10
      return
    endif
    call im%getData(i64_2)
    if(.not.allocated(i64_2)) then
      write(*,*) "couldn't cast int8 ->int64"
      result = 10
      return
    endif
    call im%getData(r32_2)
    if(.not.allocated(r32_2)) then
      write(*,*) "couldn't cast int8 ->real32"
      result = 10
      return
    endif
    call im%getData(r64_2)
    if(.not.allocated(r64_2)) then
      write(*,*) "couldn't cast int8 ->real64"
      result = 10
      return
    endif

    ! int16->
    write(*,*) "  int16-> ___"
    im = image_t(reshape(i16, shape(shape2)))
    call im%getData(i8_2 )
    if(     allocated(i8_2 )) then
      write(*,*) "shouldn't cast int16->int8 "
      result = 10
      return
    endif
    call im%getData(i16_2)
    if(.not.allocated(i16_2)) then
      write(*,*) "couldn't cast int16->int16"
      result = 10
      return
    endif
    call im%getData(i32_2)
    if(.not.allocated(i32_2)) then
      write(*,*) "couldn't cast int16->int32"
      result = 10
      return
    endif
    call im%getData(i64_2)
    if(.not.allocated(i64_2)) then
      write(*,*) "couldn't cast int16->int64"
      result = 10
      return
    endif
    call im%getData(r32_2)
    if(.not.allocated(r32_2)) then
      write(*,*) "couldn't cast int16->real32"
      result = 10
      return
    endif
    call im%getData(r64_2)
    if(.not.allocated(r64_2)) then
      write(*,*) "couldn't cast int16->real64"
      result = 10
      return
    endif

    ! int32->
    write(*,*) "  int32-> ___"
    im = image_t(reshape(i32, shape(shape2)))
    call im%getData(i8_2 )
    if(     allocated(i8_2 )) then
      write(*,*) "shouldn't cast int32->int8 "
      result = 10
      return
    endif
    call im%getData(i16_2)
    if(     allocated(i16_2)) then
      write(*,*) "shouldn't cast int32->int16"
      result = 10
      return
    endif
    call im%getData(i32_2)
    if(.not.allocated(i32_2)) then
      write(*,*) "couldn't cast int32->int32"
      result = 10
      return
    endif
    call im%getData(i64_2)
    if(.not.allocated(i64_2)) then
      write(*,*) "couldn't cast int32->int64"
      result = 10
      return
    endif
    call im%getData(r32_2)
    if(     allocated(r32_2)) then
      write(*,*) "shouldn't cast int32->real32"
      result = 10
      return
    endif
    call im%getData(r64_2)
    if(.not.allocated(r64_2)) then
      write(*,*) "couldn't cast int32->real64"
      result = 10
      return
    endif

    ! int64->
    write(*,*) "  int64-> ___"
    im = image_t(reshape(i64, shape(shape2)))
    call im%getData(i8_2 )
    if(     allocated(i8_2 )) then
      write(*,*) "shouldn't cast int64->int8 "
      result = 10
      return
    endif
    call im%getData(i16_2)
    if(     allocated(i16_2)) then
      write(*,*) "shouldn't cast int64->int16"
      result = 10
      return
    endif
    call im%getData(i32_2)
    if(     allocated(i32_2)) then
      write(*,*) "shouldn't cast int64->int32"
      result = 10
      return
    endif
    call im%getData(i64_2)
    if(.not.allocated(i64_2)) then
      write(*,*) "couldn't cast int64->int64"
      result = 10
      return
    endif
    call im%getData(r32_2)
    if(     allocated(r32_2)) then
      write(*,*) "shouldn't cast int64->real32"
      result = 10
      return
    endif
    call im%getData(r64_2)
    if(     allocated(r64_2)) then
      write(*,*) "shouldn't cast int64->real64"
      result = 10
      return
    endif

    ! real32->
    write(*,*) "  real32-> ___"
    im = image_t(reshape(r32, shape(shape2)))
    call im%getData(i8_2 )
    if(     allocated(i8_2 )) then
      write(*,*) "shouldn't cast real32->int8 "
      result = 10
      return
    endif
    call im%getData(i16_2)
    if(     allocated(i16_2)) then
      write(*,*) "shouldn't cast real32->int16"
      result = 10
      return
    endif
    call im%getData(i32_2)
    if(     allocated(i32_2)) then
      write(*,*) "shouldn't cast real32->int32"
      result = 10
      return
    endif
    call im%getData(i64_2)
    if(     allocated(i64_2)) then
      write(*,*) "shouldn't cast real32->int64"
      result = 10
      return
    endif
    call im%getData(r32_2)
    if(.not.allocated(r32_2)) then
      write(*,*) "couldn't cast real32->real32"
      result = 10
      return
    endif
    call im%getData(r64_2)
    if(.not.allocated(r64_2)) then
      write(*,*) "couldn't cast real32->real64"
      result = 10
      return
    endif

    ! real64->
    write(*,*) "  real64-> ___"
    im = image_t(reshape(r64, shape(shape2)))
    call im%getData(i8_2 )
    if(     allocated(i8_2 )) then
      write(*,*) "shouldn't cast real64->int8 "
      result = 10
      return
    endif
    call im%getData(i16_2)
    if(     allocated(i16_2)) then
      write(*,*) "shouldn't cast real64->int16"
      result = 10
      return
    endif
    call im%getData(i32_2)
    if(     allocated(i32_2)) then
      write(*,*) "shouldn't cast real64->int32"
      result = 10
      return
    endif
    call im%getData(i64_2)
    if(     allocated(i64_2)) then
      write(*,*) "shouldn't cast real64->int64"
      result = 10
      return
    endif
    call im%getData(r32_2)
    if(     allocated(r32_2)) then
      write(*,*) "couldn't cast real64->real32"
      result = 10
      return
    endif
    call im%getData(r64_2)
    if(.not.allocated(r64_2)) then
      write(*,*) "couldn't cast real64->real64"
      result = 10
      return
    endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                             bitmap io                              !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    write(*,*) "testing BMP io"
    im = image_t(reshape(i8 , shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_i8 , dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test.bmp") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test.bmp") ! read back from file
    result = check_image(im, pix_i8 , dim2) ! check reading metadata
    if(result.ne.0) return
    call im%getData(i8_2) ! extract data (16x1 ramp from 0->15)
    i8_1  = reshape(i8_2, [15])
    do i = 1,15
      if(i8_1 (i).ne.i8 (i)) then
        write(*,*) "incorrect image data"
        result = 11
        return
      endif
    enddo



  end subroutine ImageExecuteTest

end module ImageTest