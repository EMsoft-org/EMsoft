module imageOPs

  use local

implicit none

  private
  public :: ImageRescaler

  !@brief                                 : rescale an image (craetes a single use ImageRescaler)
  !@param in  [IN   ] real(1:wIn , 1:hIn ): input image
  !@param out [INOUT] real(1:wOut, 1:hOut): locatino to write output image (scale to size of out)
  !@note                                  : wOut/wIn must == hOut/hIn (to the nearest int)
  public :: RescaleImage 

  !@brief: a type to hold everything needed to rescale an image
  type ImageRescaler
    integer(kind=irg)             :: wIn  , hIn   ! input  image dimensions
    integer(kind=irg)             :: wOut , hOut  ! output image dimensions
    type   (c_ptr   )             :: pFwd , pRev  ! forward and reverse 2D DCT plans
    type   (c_ptr   )             :: pBuf1, pBuf2 ! c pointers for allocation of FFTW work arrays
    real   (c_double),pointer     :: pImIn  (:,:) ! fortran pointer for input  to pFwd
    real   (c_double),pointer     :: pImOut (:,:) ! fortran pointer for output of pFwd
    real   (c_double),pointer     :: pDctIn (:,:) ! fortran pointer for input  to pRev
    real   (c_double),pointer     :: pDctOut(:,:) ! fortran pointer for output of pRev
  contains
    !@brief               : initialize an image rescaler
    !@param w [IN] integer: input image width
    !@param h [IN] integer: input image height
    !@param s [IN] real   : scale factor such that output size is rounded from s * (w, h)
    procedure :: init    => ImageRescaler_Init

    !@brief: clean up an image rescaler
    procedure :: destroy => ImageRescaler_Destroy

    !@brief: clean up resources automatically
    final     ::            ImageRescaler_Finalize ! just calls destroy w/ polymorphism

    ! this is required for overloaded 'generic' type bound procedure
    procedure ::            ImageRescaler_Rescale8, ImageRescaler_Rescale16, ImageRescaler_Rescale32, ImageRescaler_Rescale64

    !@brief                              : rescale an image (this could be modified to include a filter)
    !@param in  [IN ] real(1:wIn ,1:hIn ): image to rescale
    !@param out [OUT] real(1:wOut,1:hOut): location to write rescaled image
    !@note                               : input can be (8/16)bit int or (32/64)bit real, output will always be 64bit real
    generic   :: rescale => ImageRescaler_Rescale8, ImageRescaler_Rescale16, ImageRescaler_Rescale32, ImageRescaler_Rescale64
  end type ImageRescaler

contains
  !@brief    : rescale an image (craetes a single use ImageRescaler)
  !@param in : input image
  !@param out: locatino to write output image (scale to size of out)
  !@note     : wOut/wIn must == hOut/hIn (to the nearest int)
  subroutine RescaleImage(in, out)
    use error
  implicit none
    real   (kind=dbl     ),INTENT(IN   ) :: in (1:,:)
    real   (kind=dbl     ),INTENT(INOUT) :: out(1:,:)

    real   (kind=dbl     )               :: s
    integer(kind=irg     )               :: iIn(2), iOut(2)
    type   (ImageRescaler)               :: scaler

    ! get shape of arrays
    iIn  = shape(in )
    iOut = shape(out)

    ! check that there is a single scale factor that captures both dimensions
    s = sum(real(iOut) / real(iIn)) / 2 ! get average of x/y scale factor to rescale in to out
    if(all(iOut.ne.nint(real(iIn) * s))) then ! make sure s * iIn == iOut
      call FatalError('RescaleImage', 'could not find single scale factor to rescale both directions')
    endif

    ! if a single scale factor works build a rescaler and apply
    call scaler%init(iIn(1), iIn(2), s)
    call scaler%rescale(in, out)

  end subroutine RescaleImage

  !@brief     : initialize an image rescaler
  !@param this: structure to clean up
  !@param w   : input image width
  !@param h   : input image height
  !@param s   : scale factor such that output size is rounded from s * (w, h)
  subroutine ImageRescaler_Init(this, w, h, s)
    use FFTW3MOD
    use error
  implicit none
    class  (ImageRescaler),INTENT(INOUT) :: this ! structure to initialize
    integer(kind=irg     ),INTENT(IN   ) :: w    ! input image width
    integer(kind=irg     ),INTENT(IN   ) :: h    ! image height
    real   (kind=dbl     ),INTENT(IN   ) :: s    ! scale factor such that output size is rounded from s * (w, h)

    integer(kind=irg     )               :: wMax, hMax

    ! sanity check scale factor
    if(s.le.0.D0) call FatalError('ImageRescaler_Init', 'scale factor must be non-negative')

    ! determine rescaled size
    this%wIn  = w
    this%hIn  = h
    this%wOut = nint(s * real(w)) ! round output to nearest int
    this%hOut = nint(s * real(h)) ! round output to nearest int

    ! prevent rescale to nothing and compute work space needed
    if(this%wOut.eq.0) this%wOut = 1
    if(this%hOut.eq.0) this%hOut = 1
    wMax = max(this%wIn, this%wOut)
    hMax = max(this%hIn, this%hOut)

    ! allocate arrays
    this%pBuf1 = fftw_alloc_real(int(wMax * hMax, C_SIZE_T)) ! allocate
    this%pBuf2 = fftw_alloc_real(int(wMax * hMax, C_SIZE_T)) ! allocate

    ! pFwd transforms from buff1 -> buff2
    call c_f_pointer(this%pBuf1, this%pImIn  , [this%wIn , this%hIn ]) ! get fortran pointer
    call c_f_pointer(this%pBuf2, this%pImOut , [this%wIn , this%hIn ]) ! get fortran pointer

    ! after transforming image is cropped/padded into buff1

    ! pRev transforms from buff1 -> buff2
    call c_f_pointer(this%pBuf1, this%pDctIn , [this%wOut, this%hOut]) ! get fortran pointer
    call c_f_pointer(this%pBuf2, this%pDctOut, [this%wOut, this%hOut]) ! get fortran pointer

    ! build fft plans
    ! call FFTW_loadWisdom() ! load wisdom from file !!! this requires sphfft
    this%pFwd = fftw_plan_r2r_2d(this%hIn , this%wIn , this%pImIn , this%pImOut , FFTW_REDFT10, FFTW_REDFT10, FFTW_MEASURE)
    this%pRev = fftw_plan_r2r_2d(this%hOut, this%wOut, this%pDctIn, this%pDctOut, FFTW_REDFT01, FFTW_REDFT01, FFTW_MEASURE)
    ! call FFTW_saveWisdom() ! save any new wisdom to file !!! this requires sphfft
  end subroutine ImageRescaler_Init

  !@brief     : clean up an image rescaler
  !@param this: structure to clean up
  subroutine ImageRescaler_Destroy(this)
    use FFTW3MOD
  implicit none
    class(ImageRescaler),INTENT(INOUT) :: this ! structure to clean up
    call fftw_destroy_plan(this%pFwd ) ! free plans
    call fftw_destroy_plan(this%pRev ) ! free plans
    call fftw_free        (this%pBuf1) ! free work arrays
    call fftw_free        (this%pBuf2) ! free work arrays
  end subroutine ImageRescaler_Destroy

  !@brief     : clean up resources automatically
  !@param this: structure to clean up
  subroutine ImageRescaler_Finalize(this)
    use FFTW3MOD
  implicit none
    type(ImageRescaler),INTENT(INOUT) :: this ! structure to clean up
    call this%destroy()
  end subroutine ImageRescaler_Finalize

  !@brief     : rescale whatever image is currently in the pDctIn buffer
  !@param this: structure to use for rescaling
  !@param in  : image to rescale
  !@param out : location to write rescaled image
  subroutine ImageRescaler_Rescale(this, out)
    use FFTW3MOD
  implicit none
    class(ImageRescaler),INTENT(INOUT) :: this ! structure to use for rescaling
    real (kind=dbl     ),INTENT(INOUT) :: out(1:this%wOut, 1:this%hOut) ! structure to use for rescaling

    ! first compute DCT of input image
    call fftw_execute_r2r(this%pFwd, this%pImIn, this%pImOut) ! do forward transformation

    ! copy output to input of reverse transform
    ! we could also do filtering here
    if(this%wIn.lt.this%wOut.and.this%hIn.lt.this%hOut) then ! up scaling
      this%pDctIn = 0.D0 ! zero out (zero pad)
      this%pDctIn(1:this%wIn, 1:this%hIn) = this%pImOut ! copy entire dct
    else
      this%pDctIn = this%pImOut(1:this%wOut, 1:this%hOut) ! copy entire dct
    endif

    ! do inverse DCT
    call fftw_execute_r2r(this%pRev, this%pDctIn, this%pDctOut)
    out = this%pDctOut ! copy out of fftw allocated array

  end subroutine ImageRescaler_Rescale

  !@brief     : rescale an image (this could be modified to include a filter)
  !@param this: structure to use for rescaling
  !@param in  : 8 bit int image to rescale
  !@param out : location to write rescaled image
  subroutine ImageRescaler_Rescale8(this, in, out)
  implicit none
    class  (ImageRescaler),INTENT(INOUT) :: this ! structure to use for rescaling
    integer(kind=1       ),INTENT(IN   ) :: in (1:this%wIn , 1:this%hIn ) ! structure to use for rescaling
    real   (kind=dbl     ),INTENT(INOUT) :: out(1:this%wOut, 1:this%hOut) ! structure to use for rescaling
    this%pImIn = in ! copy input to fftw allocated array
    call ImageRescaler_Rescale(this, out) ! do rescaling
  end subroutine ImageRescaler_Rescale8

  !@brief     : rescale an image (this could be modified to include a filter)
  !@param this: structure to use for rescaling
  !@param in  : 16 bit int image to rescale
  !@param out : location to write rescaled image
  subroutine ImageRescaler_Rescale16(this, in, out)
  implicit none
    class  (ImageRescaler),INTENT(INOUT) :: this ! structure to use for rescaling
    integer(kind=2       ),INTENT(IN   ) :: in (1:this%wIn , 1:this%hIn ) ! structure to use for rescaling
    real   (kind=dbl     ),INTENT(INOUT) :: out(1:this%wOut, 1:this%hOut) ! structure to use for rescaling
    this%pImIn = in ! copy input to fftw allocated array
    call ImageRescaler_Rescale(this, out) ! do rescaling
  end subroutine ImageRescaler_Rescale16

  !@brief     : rescale an image (this could be modified to include a filter)
  !@param this: structure to use for rescaling
  !@param in  : 32 bit real image to rescale
  !@param out : location to write rescaled image
  subroutine ImageRescaler_Rescale32(this, in, out)
  implicit none
    class(ImageRescaler),INTENT(INOUT) :: this ! structure to use for rescaling
    real (kind=sgl     ),INTENT(IN   ) :: in (1:this%wIn , 1:this%hIn ) ! structure to use for rescaling
    real (kind=dbl     ),INTENT(INOUT) :: out(1:this%wOut, 1:this%hOut) ! structure to use for rescaling
    this%pImIn = in ! copy input to fftw allocated array
    call ImageRescaler_Rescale(this, out) ! do rescaling
  end subroutine ImageRescaler_Rescale32

  !@brief     : rescale an image (this could be modified to include a filter)
  !@param this: structure to use for rescaling
  !@param in  : 64 bit real image to rescale
  !@param out : location to write rescaled image
  subroutine ImageRescaler_Rescale64(this, in, out)
  implicit none
    class(ImageRescaler),INTENT(INOUT) :: this ! structure to use for rescaling
    real (kind=dbl     ),INTENT(IN   ) :: in (1:this%wIn , 1:this%hIn ) ! structure to use for rescaling
    real (kind=dbl     ),INTENT(INOUT) :: out(1:this%wOut, 1:this%hOut) ! structure to use for rescaling
    this%pImIn = in ! copy input to fftw allocated array
    call ImageRescaler_Rescale(this, out) ! do rescaling
  end subroutine ImageRescaler_Rescale64

end module imageOPs