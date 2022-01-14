!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (c) 2018-2022, Marc De Graef Research Group/Carnegie Mellon University
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
    lzwBuff = [73,73,42,0,-52,7,0,0,-128,0,32,80,56,36,20,&
                0,1,-124,66,97,80,-80,8,10,29,15,-120,68,64,64,56,&
                -92,86,45,23,1,-127,35,81,-72,-28,116,8,5,-112,72,100,&
                82,48,40,26,77,39,-108,74,64,-64,121,100,-74,93,47,3,&
                -126,38,83,57,-92,-44,16,9,-100,78,103,83,-80,72,42,125,&
                63,-96,80,65,64,-70,37,22,-115,71,5,-125,41,84,-70,101,&
                52,24,13,-88,84,106,85,48,104,58,-83,87,-84,86,65,-64,&
                -6,-27,118,-67,95,7,-124,44,86,59,37,-108,32,17,-76,90,&
                109,86,-80,-120,74,-35,111,-72,92,66,65,59,-91,-42,-19,119,&
                9,-123,47,87,-69,-27,-12,40,21,-64,96,112,88,48,-88,91,&
                13,-121,-60,98,66,-63,124,102,55,29,-113,11,-122,50,89,60,&
                -90,84,48,25,-52,102,115,89,-80,-56,107,61,-97,-48,104,67,&
                65,-67,38,-105,77,-89,13,-121,53,90,-67,102,-76,56,29,-40,&
                108,118,91,48,-24,123,109,-73,-36,110,67,-63,-3,-26,-9,125,&
                -65,15,-120,56,92,62,39,20,64,33,-28,114,121,92,-79,8,&
                -117,-99,-49,-24,116,68,66,62,-89,87,-83,-41,17,-119,59,93,&
                -66,-25,116,72,37,-16,120,124,94,49,40,-101,-51,-25,-12,122,&
                68,-62,127,103,-73,-35,-17,19,-118,62,95,63,-89,-44,80,41,&
                -4,126,127,95,-79,72,-85,-4,-1,-64,16,8,84,21,-64,-112,&
                44,13,3,-123,97,100,21,5,-63,-112,104,88,22,-62,16,-116,&
                37,9,-123,-95,116,45,11,-61,16,-56,92,23,-61,-112,-20,61,&
                15,-123,-31,-124,69,17,-60,-111,40,96,24,-59,17,76,85,21,&
                -122,33,-108,93,23,-58,17,-120,100,25,-58,-111,-84,109,27,-122,&
                97,-92,117,29,-57,-111,-24,104,26,-56,18,12,-123,33,-122,-95,&
                -76,-115,35,-55,18,72,108,27,-55,-110,108,-99,39,-122,-31,-60,&
                -91,41,-54,-110,-88,112,28,-53,18,-52,-75,45,-121,33,-44,-67,&
                47,-52,19,8,116,29,-52,-109,44,-51,51,-121,97,-28,-43,53,&
                -51,-109,104,120,30,-50,19,-116,-27,57,-121,-95,-12,-19,59,-49,&
                19,-56,124,31,-49,-109,-20,-3,63,-121,-30,5,5,65,-48,-108,&
                40,-128,32,-47,20,77,21,69,-120,34,21,29,71,-46,20,-120,&
                -124,33,-46,-108,-83,45,75,-120,98,37,53,77,-45,-108,-24,-120,&
                34,-44,21,13,69,81,-120,-94,53,77,83,-43,21,72,-116,35,&
                -43,-107,109,93,87,-120,-30,69,101,89,-42,-107,-88,-112,36,-41,&
                21,-51,117,93,-119,34,85,125,95,-40,22,8,-108,37,-40,-106,&
                45,-115,99,-119,98,101,-107,101,-39,-106,104,-104,38,-38,22,-115,&
                -91,105,-119,-94,117,-83,107,-37,22,-56,-100,39,-37,-106,-19,-67,&
                111,-119,-30,-123,-59,113,-36,-105,40,-96,40,-35,23,77,-43,117,&
                -118,34,-107,-35,119,-34,23,-120,-92,41,-34,-105,-83,-19,123,-118,&
                98,-91,-11,125,-33,-105,-24,-88,42,-32,24,14,5,-127,-118,-94,&
                -74,13,-125,-31,24,72,-84,43,-31,-104,110,29,-121,-118,-30,-58,&
                37,-119,-30,-104,-88,-80,44,-29,24,-50,53,-115,-117,34,-42,61,&
                -113,-28,25,8,-76,45,-28,-103,46,77,-109,-117,98,-26,85,-107,&
                -27,-103,104,-72,46,-26,25,-114,101,-103,-117,-94,-10,109,-101,-25,&
                25,-56,-68,47,-25,-103,-18,125,-97,-117,-29,6,-123,-95,-24,-102,&
                40,-64,48,-23,26,78,-107,-91,-116,35,22,-99,-89,-22,26,-120,&
                -60,49,-22,-102,-82,-83,-85,-116,99,38,-75,-83,-21,-102,-24,-56,&
                50,-20,27,14,-59,-79,-116,-93,54,-51,-77,-19,27,72,-52,51,&
                -19,-101,110,-35,-73,-116,-29,70,-27,-71,-18,-101,-88,-48,52,-17,&
                27,-50,-11,-67,-115,35,86,-3,-65,-16,28,8,-44,53,-16,-100,&
                47,13,-61,-115,99,103,21,-59,-15,-100,104,-40,54,-14,28,-113,&
                37,-55,-115,-93,119,45,-53,-13,28,-56,-36,55,-13,-100,-17,61,&
                -49,-115,-29,-121,69,-47,-12,-99,40,-32,56,-11,29,79,85,-43,&
                -114,35,-105,93,-41,-10,29,-120,-28,57,-10,-99,-81,109,-37,-114,&
                99,-89,117,-35,-9,-99,-24,-24,58,-8,30,15,-123,-31,-114,-93,&
                -73,-115,-29,-7,30,72,-20,59,-7,-98,111,-99,-25,-114,-29,-57,&
                -91,-23,-6,-98,-88,-16,60,-5,30,-49,-75,-19,-113,35,-41,-67,&
                -17,-4,31,8,-12,61,-4,-97,47,-51,-13,-113,99,-25,-43,-11,&
                -3,-97,104,-8,62,-2,31,-113,-27,-7,-113,-93,-9,-19,-5,-1,&
                31,-56,-4,63,-1,-97,-17,-4,-1,-61,-8,-128,-128,80,14,2,&
                64,81,0,32,96,68,9,-127,80,44,64,-120,40,29,3,-32,&
                -124,17,16,66,14,10,65,88,45,5,-60,24,-124,-125,80,110,&
                14,65,-47,8,33,97,4,33,-124,80,-116,66,-120,104,77,9,&
                -31,68,41,16,-62,30,22,66,-40,93,11,-60,56,-120,-122,80,&
                -50,26,67,81,16,34,97,-60,57,-121,80,-20,68,-120,-88,125,&
                15,-30,4,65,17,66,46,34,68,88,-115,17,-60,88,-116,-119,&
                81,46,38,68,-47,24,35,98,-124,81,-118,81,76,70,-120,-24,&
                -83,21,-30,-60,89,17,-62,62,46,69,-40,-67,23,-60,120,-112,&
                -116,81,-114,50,70,81,32,36,99,68,105,-115,81,-84,72,-119,&
                40,-35,27,-29,-124,113,18,66,78,58,71,88,-19,29,-60,-104,&
                -108,-113,81,-18,62,71,-47,40,37,100,4,-127,-112,82,12,74,&
                -119,105,13,33,-28,68,-119,18,-62,94,70,72,-39,29,35,-60,&
                -72,-104,-110,82,78,74,73,81,48,38,100,-60,-103,-109,82,108,&
                76,-119,-87,61,39,-27,4,-95,19,66,110,82,74,89,77,41,&
                -60,-40,-100,-107,82,-82,86,74,-47,56,39,101,-124,-79,-106,82,&
                -52,78,-119,-23,109,45,-27,-60,-71,19,-62,126,94,75,-39,125,&
                47,-60,-8,-96,-104,83,14,98,76,81,64,40,102,68,-55,-103,&
                83,44,80,-118,41,-99,51,-26,-124,-47,20,66,-114,106,77,89,&
                -83,53,-59,24,-92,-101,83,110,110,77,-47,72,41,103,4,-31,&
                -100,83,-116,82,-118,105,-51,57,-25,68,-23,20,-62,-98,118,78,&
                -39,-35,59,-59,56,-88,-98,83,-50,122,79,81,80,42,103,-60,&
                -7,-97,83,-20,84,-118,-87,-3,63,-24,5,1,21,66,-82,-126,&
                80,90,13,65,-59,88,-84,-95,84,46,-122,80,-47,88,43,104,&
                -123,17,-94,84,76,86,-118,-22,45,69,-24,-59,25,21,-62,-66,&
                -114,81,-38,61,71,-59,120,-80,-92,84,-114,-110,82,81,96,44,&
                105,69,41,-91,84,-84,88,-117,42,93,75,-23,-123,49,22,66,&
                -50,-102,83,90,109,77,-59,-104,-76,-89,84,-18,-98,83,-47,104,&
                45,106,5,65,-88,85,12,90,-117,106,-115,81,-22,69,73,22,&
                -62,-34,-90,84,-38,-99,83,-59,-72,-72,-86,85,78,-86,85,81,&
                112,46,106,-59,89,-85,85,108,92,-117,-86,-67,87,-21,5,97,&
                23,66,-18,-78,86,90,-51,89,-59,-40,-68,-83,85,-82,-74,86,&
                -47,120,47,107,-123,113,-82,85,-52,94,-117,-22,-19,93,-21,-59,&
                121,23,-62,-2,-66,87,-38,-3,95,-59,-8,-64,-80,86,14,-62,&
                88,81,-128,48,108,69,-119,-79,86,44,96,-116,43,29,99,-20,&
                -123,-111,24,67,14,-54,89,91,45,101,-58,24,-60,-77,86,110,&
                -50,89,-47,-120,49,109,5,-95,-76,86,-116,98,-116,107,77,105,&
                -19,69,-87,24,-61,30,-42,90,-37,93,107,-58,56,-56,-74,86,&
                -50,-38,91,81,-112,50,109,-59,-71,-73,86,-20,100,-116,-85,125,&
                111,-18,5,-63,25,67,46,-30,92,91,-115,113,-58,88,-52,-71,&
                87,46,-26,92,-47,-104,51,110,-123,-47,-70,87,76,102,-116,-21,&
                -83,117,-18,-59,-39,25,-61,62,-18,93,-37,-67,119,-58,120,-48,&
                -68,87,-114,-14,94,81,-96,52,111,69,-23,-67,87,-84,104,-115,&
                43,-35,123,-17,-123,-15,26,67,78,-6,95,91,-19,125,-58,-104,&
                -44,-65,87,-18,-2,95,-47,-88,53,112,6,1,-64,88,12,106,&
                -115,108,13,-127,-16,70,9,26,-61,95,6,96,-36,29,-125,-58,&
                -72,-40,-62,88,79,10,97,81,-80,54,112,-58,25,-61,88,108,&
                108,-115,-84,61,-121,-15,6,33,27,67,111,18,98,92,77,-119,&
                -58,-40,-36,-59,88,-81,22,98,-47,-72,55,113,-122,49,-58,88,&
                -52,110,-115,-20,109,-115,-15,-58,57,27,-61,127,30,99,-36,125,&
                -113,-58,-8,-32,-56,89,15,34,100,81,-64,56,114,70,73,-55,&
                89,44,112,-114,44,-99,-109,-14,-122,81,28,67,-113,42,101,92,&
                -83,-107,-57,24,-28,-53,89,111,46,101,-47,-56,57,115,6,97,&
                -52,89,-116,114,-114,108,-51,-103,-13,70,105,28,-61,-97,54,102,&
                -36,-35,-101,-57,56,-24,-50,89,-49,58,103,81,-48,58,115,-58,&
                121,-49,89,-20,116,-114,-84,-3,-97,-12,6,-127,29,67,-81,66,&
                104,93,13,-95,-57,88,-20,-47,90,47,70,104,-47,-40,59,116,&
                -122,-111,-46,90,76,118,-114,-19,45,-91,-12,-58,-103,29,-61,-65,&
                78,105,-35,61,-89,-57,120,-16,-44,90,-113,82,106,81,-32,60,&
                117,70,-87,-43,90,-84,120,-113,45,93,-85,-11,-122,-79,30,67,&
                -49,90,107,93,109,-83,-57,-104,-12,-41,90,-17,94,107,-47,-24,&
                61,118,6,-63,-40,91,12,122,-113,109,-115,-79,-10,70,-55,30,&
                -61,-33,102,108,-35,-99,-77,-57,-72,-8,-38,91,79,106,109,81,&
                -16,62,118,-58,-39,-37,91,108,124,-113,-83,-67,-73,-9,6,-31,&
                31,67,-17,114,110,93,-51,-71,-57,-40,-4,-35,91,-81,118,110,&
                -47,-8,63,119,-122,-15,-34,91,-52,126,-113,-19,-19,-67,-9,-58,&
                -7,31,-61,-1,126,111,-35,-3,-65,-57,-7,1,0,16,0,-2,&
                0,4,0,1,0,0,0,0,0,0,0,0,1,3,0,1,&
                0,0,0,0,16,0,0,1,1,3,0,1,0,0,0,1,&
                0,0,0,2,1,3,0,1,0,0,0,8,0,0,0,3,&
                1,3,0,1,0,0,0,5,0,0,0,6,1,3,0,1,&
                0,0,0,1,0,0,0,13,1,2,0,71,0,0,0,-94,&
                8,0,0,17,1,4,0,1,0,0,0,8,0,0,0,18,&
                1,3,0,1,0,0,0,1,0,0,0,21,1,3,0,1,&
                0,0,0,1,0,0,0,22,1,3,0,1,0,0,0,64,&
                0,0,0,23,1,4,0,1,0,0,0,-61,7,0,0,26,&
                1,5,0,1,0,0,0,-110,8,0,0,27,1,5,0,1,&
                0,0,0,-102,8,0,0,28,1,3,0,1,0,0,0,1,&
                0,0,0,40,1,3,0,1,0,0,0,2,0,0,0,0,&
                0,0,0,72,0,0,0,1,0,0,0,72,0,0,0,1,&
                0,0,0,67,58,92,85,115,101,114,115,92,109,97,114,99,&
                92,68,111,99,117,109,101,110,116,115,92,99,114,121,115,116,&
                97,108,108,111,103,114,97,112,104,121,95,98,117,105,108,100,&
                50,92,112,121,116,104,111,110,92,82,101,108,101,97,115,101,&
                92,114,97,109,112,46,116,105,102,0]

    ! fill big endian tiff buffer
    bgBuff = [77,77,0,42,0,0,0,8,0,10,0,-2,0,4,0,&
              0,0,1,0,0,0,0,1,0,0,4,0,0,0,1,0,&
              0,0,4,1,1,0,4,0,0,0,1,0,0,0,4,1,&
              2,0,3,0,0,0,1,0,16,0,0,1,6,0,3,0,&
              0,0,1,0,1,0,0,1,14,0,2,0,0,0,34,0,&
              0,0,-122,1,17,0,4,0,0,0,1,0,0,0,-88,1,&
              21,0,3,0,0,0,1,0,1,0,0,1,22,0,3,0,&
              0,0,1,0,4,0,0,1,23,0,4,0,0,0,1,0,&
              0,0,32,0,0,0,0,73,109,97,103,101,74,61,49,46,&
              53,48,105,10,109,105,110,61,48,46,48,10,109,97,120,61,&
              49,53,51,48,48,46,48,10,0,0,0,4,0,8,0,12,&
              0,16,0,20,0,24,0,28,0,32,0,36,0,40,0,44,&
              0,48,0,52,0,56,0,60,0]

    ! fill little endian tiff buffer
    ltBuff = [73,73,42,0,8,0,0,0,10,0,-2,0,4,0,1,&
              0,0,0,0,0,0,0,0,1,4,0,1,0,0,0,4,&
              0,0,0,1,1,4,0,1,0,0,0,4,0,0,0,2,&
              1,3,0,1,0,0,0,16,0,0,0,6,1,3,0,1,&
              0,0,0,1,0,0,0,14,1,2,0,34,0,0,0,-122,&
              0,0,0,17,1,4,0,1,0,0,0,-88,0,0,0,21,&
              1,3,0,1,0,0,0,1,0,0,0,22,1,3,0,1,&
              0,0,0,4,0,0,0,23,1,4,0,1,0,0,0,32,&
              0,0,0,0,0,0,0,73,109,97,103,101,74,61,49,46,&
              53,48,105,10,109,105,110,61,48,46,48,10,109,97,120,61,&
              49,53,51,48,48,46,48,10,0,0,0,0,4,0,8,0,&
              12,0,16,0,20,0,24,0,28,0,32,0,36,0,40,0,&
              44,0,48,0,52,0,56,0,60]

    ! rgb grayscale tif buffer
    rgbBuff = [73,73,42,0,56,0,0,0,0,0,0,1,1,1,2,&
                2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,&
                7,8,8,8,9,9,9,10,10,10,11,11,11,12,12,12,&
                13,13,13,14,14,14,15,15,15,16,0,-2,0,4,0,1,&
                0,0,0,0,0,0,0,0,1,3,0,1,0,0,0,16,&
                0,0,0,1,1,3,0,1,0,0,0,1,0,0,0,2,&
                1,3,0,3,0,0,0,14,1,0,0,3,1,3,0,1,&
                0,0,0,1,0,0,0,6,1,3,0,1,0,0,0,2,&
                0,0,0,13,1,2,0,70,0,0,0,20,1,0,0,17,&
                1,4,0,1,0,0,0,8,0,0,0,18,1,3,0,1,&
                0,0,0,1,0,0,0,21,1,3,0,1,0,0,0,3,&
                0,0,0,22,1,3,0,1,0,0,0,64,0,0,0,23,&
                1,4,0,1,0,0,0,48,0,0,0,26,1,5,0,1,&
                0,0,0,-2,0,0,0,27,1,5,0,1,0,0,0,6,&
                1,0,0,28,1,3,0,1,0,0,0,1,0,0,0,40,&
                1,3,0,1,0,0,0,2,0,0,0,0,0,0,0,72,&
                0,0,0,1,0,0,0,72,0,0,0,1,0,0,0,8,&
                0,8,0,8,0,67,58,92,85,115,101,114,115,92,87,105,&
                108,108,92,68,111,99,117,109,101,110,116,115,92,99,114,121,&
                115,116,97,108,108,111,103,114,97,112,104,121,95,98,117,105,&
                108,100,50,92,112,121,116,104,111,110,92,82,101,108,101,97,&
                115,101,92,114,103,98,46,116,105,102,0]

    !rgba grayscale tif buffer
    rgbaBuff = [73,73,42,0,72,0,0,0,0,0,0,-1,1,1,1,&
                -1,2,2,2,-1,3,3,3,-1,4,4,4,-1,5,5,5,&
                -1,6,6,6,-1,7,7,7,-1,8,8,8,-1,9,9,9,&
                -1,10,10,10,-1,11,11,11,-1,12,12,12,-1,13,13,13,&
                -1,14,14,14,-1,15,15,15,-1,17,0,-2,0,4,0,1,&
                0,0,0,0,0,0,0,0,1,3,0,1,0,0,0,16,&
                0,0,0,1,1,3,0,1,0,0,0,1,0,0,0,2,&
                1,3,0,4,0,0,0,42,1,0,0,3,1,3,0,1,&
                0,0,0,1,0,0,0,6,1,3,0,1,0,0,0,2,&
                0,0,0,13,1,2,0,71,0,0,0,50,1,0,0,17,&
                1,4,0,1,0,0,0,8,0,0,0,18,1,3,0,1,&
                0,0,0,1,0,0,0,21,1,3,0,1,0,0,0,4,&
                0,0,0,22,1,3,0,1,0,0,0,64,0,0,0,23,&
                1,4,0,1,0,0,0,64,0,0,0,26,1,5,0,1,&
                0,0,0,26,1,0,0,27,1,5,0,1,0,0,0,34,&
                1,0,0,28,1,3,0,1,0,0,0,1,0,0,0,40,&
                1,3,0,1,0,0,0,2,0,0,0,82,1,3,0,1,&
                0,0,0,2,0,0,0,0,0,0,0,72,0,0,0,1,&
                0,0,0,72,0,0,0,1,0,0,0,8,0,8,0,8,&
                0,8,0,67,58,92,85,115,101,114,115,92,109,97,114,99,&
                92,68,111,99,117,109,101,110,116,115,92,99,114,121,115,116,&
                97,108,108,111,103,114,97,112,104,121,95,98,117,105,108,100,&
                50,92,112,121,116,104,111,110,92,82,101,108,101,97,115,101,&
                92,114,103,98,97,46,116,105,102,0]

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      2d tif io and conversion                      !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    write(*,*) "testing 2d scalar TIFF conversion and io"
    ! test 2d int8 conversion,read, and write
    write(*,*) "  i8 "
    im = image_t(reshape(i8 , shape(shape2))) ! convert test data to image_t
    result = check_image(im, pix_i8 , dim2) ! check conversion
    if(result.ne.0) return
    call im%write("test_io.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_io.tif") ! read back from file
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
    call im%write("test_2d_int16.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_2d_int16.tif") ! read back from file
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
    call im%write("test_i32.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_i32.tif") ! read back from file
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
    call im%write("test_i64.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_i64.tif") ! read back from file
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
    call im%write("test_r32.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_r32.tif") ! read back from file
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
    call im%write("test_r64.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_r64.tif") ! read back from file
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
    call im%write("test_3d_i8.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_3d_i8.tif") ! read back from file
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
    call im%write("test_3d_i16.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_3d_i16.tif") ! read back from file
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
    call im%write("test_3d_i32.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_3d_i32.tif") ! read back from file
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
    call im%write("test_3D_i64.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_3D_i64.tif") ! read back from file
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
    call im%write("test_3d_r32.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_3d_r32.tif") ! read back from file
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
    call im%write("test_3d_r64.tif") ! write test data to file
    call im%clear() ! erase image
    im = image_t("test_3d_r64.tif") ! read back from file
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
    open(newunit = i, file = 'test_lzw.tif', access = 'stream') ! open file
    write(i) lzwBuff ! write lzw compressed tiff to file
    close(i) ! close file
    im = image_t("test_lzw.tif") ! read back lzw compressed tif from file
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
    open(newunit = i, file = 'test_big.tif', access = 'stream') ! open file
    write(i) bgBuff ! write big endian tiff to file
    close(i) ! close file
    im = image_t("test_big.tif") ! read back big endian tif from file
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
    open(newunit = i, file = 'test_little.tif', access = 'stream') ! open file
    write(i) ltBuff ! write little endian tiff to file
    close(i) ! close file
    im = image_t("test_little.tif") ! read back little endian tif from file
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
    open(newunit = i, file = 'test_rgb_gray.tif', access = 'stream') ! open file
    write(i) rgbBuff ! write false color rgb tiff
    close(i) ! close file
    im = image_t("test_rgb_gray.tif") ! read back little endian tif from file
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
    open(newunit = i, file = 'test_rgba_gray.tif', access = 'stream') ! open file
    write(i) rgbaBuff ! write false color rgb tiff
    close(i) ! close file
    im = image_t("test_rgba_gray.tif") ! read back little endian tif from file
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
