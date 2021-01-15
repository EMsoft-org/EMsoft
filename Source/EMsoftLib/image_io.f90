!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (c) 2018-2021, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:image_io.f90
!-----------------------------------------------------------------------
!
! module: image_io (submodule of image)
!
!> @author: W.C. Lenthe, Carnegie Mellon University
!
!> @brief: io routines for image_t object (separate file needed to break circular dependencies)
!
!> @date: 03/14/18 WCL 1.0 original
!
!-----------------------------------------------------------------------

submodule (image) image_io
  use tif
  use bmp
  implicit none
contains
  ! @brief: extract file time from a file name based on the extension
  ! @param filename: file name to determine type of
  ! @return: one of the file types enumerated in the image module
  ! @signature: function image_get_extension(filename) result(ext)
  module procedure image_get_extension
!DEC$ ATTRIBUTES DLLEXPORT :: image_get_extension
    integer                      :: i, c
    character(len=32)            :: s
    integer          , parameter :: cA = iachar('A')
    integer          , parameter :: cZ = iachar('Z')
    integer          , parameter :: ul = iachar('a') - iachar('A') 
    i = scan(filename, '.', back=.true.) ! find last '.'
    ext = im_ext_unk
    if(i > 0) then
      s = trim(filename(i+1:len(filename))) ! extract extension
      do i = 1, len(s) ! convert to lower case
        c = iachar(s(i:i))
        if(c.ge.cA.and.c.le.cZ) s(i:i) = achar(c + ul)
      enddo
      if(s.eq.'tif'.or.s.eq.'tiff') then
        ext = im_ext_tif
      else if(s.eq.'bmp') then
        ext = im_ext_bmp
      endif
    endif
  end procedure image_get_extension

  ! @brief: convert an 8 bit rgb or rgba image to grayscale if the extra channels are identical (some software saves grayscale to color images)
  ! @param this: image_t to flatten
  ! @signature: subroutine image_flatten_rgba(this)
  module procedure image_flatten_rgba
!DEC$ ATTRIBUTES DLLEXPORT :: image_flatten_rgba
   integer                     :: i
    integer(int8) , allocatable :: tempBuff(:)

    if(pix_i8.eq.this%pixelType) then ! only flatten 8 bit images
      if(this%samplesPerPixel.eq.4) then ! rgba, check if alpha channel actually is used
        i = size(this%buff)
        if(all(this%buff(4:i:4).eq.this%buff(4))) then ! all alpha values are the same
          allocate(tempBuff(i*3/4))
          tempBuff(1:size(tempBuff):3) = this%buff(1:i:4) ! copy r
          tempBuff(2:size(tempBuff):3) = this%buff(2:i:4) ! copy g
          tempBuff(3:size(tempBuff):3) = this%buff(3:i:4) ! copy b
          deallocate(this%buff)
          allocate(this%buff(size(tempBuff)))
          this%buff = tempBuff
          deallocate(tempBuff)
          this%samplesPerPixel = 3
        endif
      endif

      if(this%samplesPerPixel.eq.3) then ! rgb, check if actually color
        i = size(this%buff)
        if(all(this%buff(1:i:3).eq.this%buff(2:i:3)).and.all(this%buff(2:i:3).eq.this%buff(3:i:3))) then ! r==g==b
          ! copy image data flattening to 1 channel
          allocate(tempBuff(i/3))
          tempBuff = this%buff(1:i:3)
          deallocate(this%buff)
          allocate(this%buff(size(tempBuff)))
          this%buff = tempBuff
          deallocate(tempBuff)
          this%samplesPerPixel = 1
        endif
      endif
    endif
  end procedure image_flatten_rgba

  ! @brief: read the data from a file into an image_t
  ! @param filename: the name of the file to read data from
  ! @param (optional) iostat: error flag (0 on success)
  ! @param (optional) iomsg: error message (filled if 0.ne.iostat)
  ! @return: image_t with file data (empty on failure)
  ! @signature: function image_read(filename, iostat, iomsg) result(im)
  module procedure image_read
!DEC$ ATTRIBUTES DLLEXPORT :: image_read
  integer            :: stat
    character(len=128) :: msg
    integer            :: ext
    type(tif_t)        :: tif
    logical            :: isFile

! initialize variables
    stat = 0
    msg = ""
    if(present(iostat)) iostat = 0
    if(present(iomsg)) iomsg = ""
    call im%clear()

    ! make sure file exists
    inquire(file=filename, exist=isFile)
    if(.not.isFile) then
      if(present(iostat)) iostat = 1
      if(present(iomsg )) iomsg  = "file '" // trim(filename) // "' does not exist"
      return
    endif

    ! determine file type from extension and read
    ext = image_get_extension(filename)
    if(im_ext_unk.eq.ext) then
      if(present(iostat)) iostat = 1
      if(present(iomsg )) iomsg  = "no image reader available for file type"
    else
      select case(ext)
        case(im_ext_tif) ! tiff
          call tif%read(filename, stat, msg)
          if(stat.ne.0) then
            if(present(iostat)) iostat = stat
            if(present(iomsg )) iomsg  = msg
          else
            im = tif%getImage()
            if(.not.allocated(im%buff)) then
              if(present(iostat)) iostat = 1
              if(present(iomsg )) iomsg  = "unable to convert tif '" // filename // "' to image_t"
            endif
          endif

        case(im_ext_bmp) ! bitmap
          im = bmp_read(filename, stat, msg)
          if(stat.ne.0) then
            if(present(iostat)) iostat = stat
            if(present(iomsg )) iomsg  = msg
          endif
      end select
      if(.not.im%empty()) call image_flatten_rgba(im) ! remove extra channels if possible
    endif
  end procedure image_read

  ! @brief: write the data in an image_t to a file
  ! @param this: the image_t to read data from
  ! @param filename: the name of the file to write data to
  ! @param (optional) iostat: error flag (0 on success)
  ! @param (optional) iomsg: error message (filled if 0.ne.iostat)
  ! @signature: subroutine image_write(this, filename, iostat, iomsg)
  module procedure image_write
!DEC$ ATTRIBUTES DLLEXPORT :: image_write
   integer            :: stat
    character(len=128) :: msg
    integer            :: ext
    type(tif_t)        :: tif

! initialize variables
    stat = 0
    msg = ""
    if(present(iostat)) iostat = 0
    if(present(iomsg)) iomsg = ""

! select writer based on the file extention
    ext = image_get_extension(filename)
    if(im_ext_unk.eq.ext) then
      if(present(iostat)) iostat = 1
      if(present(iomsg )) iomsg  = "no image writer available for file type"
    else
      select case(ext)
        ! tiff
        case(im_ext_tif)
          call tif%fromImage(this)
          if(.not.allocated(tif%directories)) then
            if(present(iostat)) iostat = 1
            if(present(iomsg )) iomsg  = "unable to convert image to tif"
          else
            call tif%write(filename, stat, msg)
            if(stat.ne.0) then
              if(present(iostat)) iostat = stat
              if(present(iomsg )) iomsg  = msg
            endif
          endif

        ! bitmap
        case(im_ext_bmp)
          call bmp_write(filename, this, stat, msg)
          if(stat.ne.0) then
            if(present(iostat)) iostat = stat
            if(present(iomsg )) iomsg  = msg
          endif
      end select
    endif
  end procedure image_write
end submodule image_io
