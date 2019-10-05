!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!*                                                                     *
!* Copyright (c) 2019-2019, De Graef Group, Carnegie Mellon University *
!* All rights reserved.                                                *
!*                                                                     *
!* Author: William C. Lenthe                                           *
!*                                                                     *
!* EMSphInx is available for academic or non-profit non-commercial     *
!* research use. Please, see the license.txt file in this distribution *
!* for further details.                                                *
!*                                                                     *
!* Interested in a commercial license? Contact:                        *
!*                                                                     *
!* Center for Technology Transfer and Enterprise Creation              *
!* 4615 Forbes Avenue, Suite 302                                       *
!* Pittsburgh, PA 15213                                                *
!*                                                                     *
!* phone. : 412.268.7393                                               *
!* email  : innovation@cmu.edu                                         *
!* website: https://www.cmu.edu/cttec/                                 *
!*                                                                     *
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

!--------------------------------------------------------------------------
! EMsoft:fft_wrap.f90
!--------------------------------------------------------------------------
!
! MODULE: fft_wrap
!
!> @author William C Lenthe
!
!> @brief abstraction layer for FFTW (or potentially alternate fft library)
! 
!> @date 08/06/19 WCL 1.0 original, based on Will Lenthe's C++ routines
!--------------------------------------------------------------------------
module fft_wrap

  use FFTW3MOD

implicit none
  
  !@brief: wrapper class to handle memory allocation for FFTs
  !@note : this shouldn't be instantiated directly (use the single declared instance 'FFTWisdom' below)
  type FFTWisdomType
    logical :: loaded = .false. ! has load() been called
  contains
    !@brief                                           : get the wisdom file name (with null terminator)
    !@param exists [OUT] logical                      : optional location to write true/false if the file exists/doesn't
    !@return             character(c_char),allocatable: wisdom file name (you are responsible for deallocating)
    procedure, nopass :: name => FFTWisdomType_Name

    !@brief: (re)allocate a buffer
    procedure         :: load => FFTWisdomType_Load

    !@brief: (re)allocate a buffer
    procedure         :: save => FFTWisdomType_Save

    !@brief: automatically save wisdom on destruction
    ! final             ::         FFTWisdomType_Finalize
  end type FFTWisdomType



  ! a global instance of the FFT wisdom type
  ! this should make it easier to manage automatically
  type(FFTWisdomType) :: FFTWisdom ! may call FFTWisdom%load() before planning and FFTWisdom%save() afterwards (auto save doesn't work yet)
!DEC$ ATTRIBUTES DLLEXPORT :: FFTWisdom


  !@brief: wrapper class to handle memory allocation for FFTs
  type FFTBuffer
    type(c_ptr), allocatable :: ptr ! = c_null_ptr ! fftw allocated memory pointer, allocatable since =c_null_ptr doesn't carry to nested types
  contains
    !@brief                : (re)allocate a buffer
    !@param sz [IN] integer: size of array to allocate (number of doubles)
    procedure :: allocReal => FFTBuffer_AllocReal

    !@brief                : (re)allocate a buffer
    !@param sz [IN] integer: size of array to allocate (number of complex doubles)
    procedure :: allocCplx => FFTBuffer_AllocCplx

    !@brief: deallocate a buffer (if it is allocated)
    procedure :: free  => FFTBuffer_Free

    !@brief: automatically clean up on destruction
    final     ::          FFTBuffer_Finalize
  end type FFTBuffer



  !@brief: an FFT plans
  type FFTPlan
    type(c_ptr), allocatable :: ptr ! = c_null_ptr ! fftw plan, allocatable since =c_null_ptr doesn't carry to nested types
  contains
    !@brief: allocate space for the ptr and zero
    procedure :: init     => FFTPlan_Init

    !@brief: destroy a plan (if it is allocated)
    procedure :: destroy  => FFTPlan_Destroy

    !@brief: automatically clean up on destruction
    final     ::             FFTPlan_Finalize
  end type FFTPlan



  ! next enumerate planning flags
  integer, parameter :: FFT_PLAN_ESTIMATE = FFTW_ESTIMATE
  integer, parameter :: FFT_PLAN_MEASURE  = FFTW_MEASURE


  ! finally we can implement specific plans to abstract FFTW

  !@brief: wrapper for 1D real <--> complex transform
  type RealFFT
    type(FFTPlan  ) :: pFwd, pRev ! forward and reverse plans (clean up automatically via final)
  contains
    !@brief                 : (re)initialize a real to complex transformer
    !@param sz  [IN] integer: length of transform (length of real data)
    !@param flg [IN] integer: planning flags
    procedure :: init    => RealFFT_Init

    !@brief                                                  : execute a real to half complex transform
    !@param signal  [IN   ] real   (c_double        ),pointer: real data to compute DFT of
    !@param spectra [INOUT] complex(c_double_complex),pointer: location to write half complex DFT
    procedure :: forward  => RealFFT_Fwd

    !@brief                                                  : execute a half complex to real transform
    !@param spectra [IN   ] complex(c_double_complex),pointer: half complex DFT to compute inverse of
    !@param signal  [INOUT] real   (c_double        ),pointer: location to write real signal
    procedure :: inverse  => RealFFT_Inv
  end type RealFFT

  !@brief: wrapper for 3D complex --> real transform
  type Real3DFFT
    type(FFTPlan  ) :: pRev ! reverse plan only (clean up automatically via final)
  contains
    !@brief                 : (re)initialize a complex to real transformer
    !@param sz  [IN] integer: cube size lenth length of transform (sz*sz*sz cube)
    !@param flg [IN] integer: planning flags
    procedure :: init    => Real3DFFT_Init

    !@brief                                                  : execute a half complex to real transform
    !@param spectra [IN   ] complex(c_double_complex),pointer: half complex DFT to compute inverse of
    !@param signal  [INOUT] real   (c_double        ),pointer: location to write real signal
    procedure :: inverse  => Real3DFFT_Inv
  end type Real3DFFT

  !@brief: wrapper for symmetry leveraging 3D complex --> real transform [special case for euler angle cube]
  type RealSep3DFFT
    type   (FFTPlan         )         :: pX         ! reverse plan for X direction
    type   (FFTPlan         )         :: pY         ! reverse plan for Y direction
    type   (FFTPlan         )         :: pZ         ! reverse plan for Z direction
    type   (FFTBuffer       )         :: pWrk       ! fft allocated work space for intermediate transform
    complex(c_double_complex),pointer :: wrk(:,:,:) ! fortran wrapped work space
    integer                              vN, vH     ! cube is vN^3 with only the first vH z slices needed in the output
  contains
    !@brief                 : (re)initialize a complex to real transformer
    !@param sz  [IN] integer: cube size lenth length of transform (sz*sz*sz cube)
    !@param flg [IN] integer: planning flags
    procedure :: init    => RealSep3DFFT_Init

    !@brief                                                  : execute a half complex to real transform
    !@param spectra [IN   ] complex(c_double_complex),pointer: half complex DFT to compute inverse of
    !@param signal  [INOUT] real   (c_double        ),pointer: location to write real signal
    !@param dx      [IN   ] integer                          : spacing of non zero YZ planes (at 1, 1+dx, 1+dx*2, 1+dx*3, ...)
    procedure :: inverse  => RealSep3DFFT_Inv
  end type RealSep3DFFT

contains

  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  !                                                                   !
  !                member functions for FFTWisdomType                 !
  !                                                                   !
  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: FFTWisdomType_Name
  !
  !> @author Will Lenthe
  !
  !> @brief get the FFTW wisdom file name
  !> @param 
  !
  !> @date 08/06/19 WCL 1.0 original
  !--------------------------------------------------------------------------
  recursive function FFTWisdomType_Name(fileExists) result(name)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTWisdomType_Name
    use local
  implicit none
    logical            ,intent(out),optional    :: fileExists ! boolean for if the file exists
    character(c_char  )            ,allocatable :: name(:)       ! c string for file name
    character(fnlen   )                         :: wisdomFile ! fortran string for file name
    integer  (c_size_t)                         :: slen       ! length of s
    integer                                     :: i          ! loop index
    integer  (c_int   )                         :: status     ! fftw return code

    ! get the wisdom file name and convert to a path
    wisdomFile = EMsoft_getfftwWisdomfilename()
    wisdomFile = EMsoft_toNativePath(wisdomFile)

    ! check if the file exists if needed
    if(present(fileExists)) inquire(file=trim(wisdomFile), exist=fileExists)

write (*,*) 'wisdom file '//trim(wisdomFile), fileExists

    ! convert from fortran character array to c string
    slen = len(trim(wisdomFile)) ! determine length of string
    allocate(name(slen+1)) ! add extra space for null terminator
    do i=1,slen ! loop over array copying to c string
      name(i) = wisdomFile(i:i)
    end do
    name(slen+1) = C_NULL_CHAR ! add null terminator
  end function FFTWisdomType_Name

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: FFTWisdomType_Load
  !
  !> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
  !
  !> @brief load FFTW wisdom from a file
  !
  !> @date 05/30/19 WCL 1.0 original, based on MDG code in DSHT.f90
  !> @date 08/06/19 WCL converted to object oriented
  !--------------------------------------------------------------------------
  recursive subroutine FFTWisdomType_Load(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTWisdomType_Load
    use FFTW3MOD
    use error
  implicit none
    class    (FFTWisdomType), intent(inout) :: this
    character(c_char       ), allocatable   :: name(:)    ! fftw file name
    logical                                 :: fileExists ! boolean for if the file exists
    integer  (c_int        )                :: status     ! fftw return code

    if(.not.this%loaded) then ! if we've already loaded the file we're done
      name = FFTWisdomType_Name(fileExists) ! get the wisdom file name and check if it exists
      if (fileExists) then ! make sure there is a file to read
        status = fftw_import_wisdom_from_filename(name) ! attempt to import and check for error
        if (status .eq. 0) call FatalError('FFTWisdomType_Load','error reading wisdom from file') ! handle error
      endif
      deallocate(name) ! clean up string
      this%loaded = .true. ! flag wisdom as loaded
    endif
  end subroutine FFTWisdomType_Load

  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: FFTWisdomType_Save
  !
  !> @author Will Lenthe/Marc De Graef, Carnegie Mellon University
  !
  !> @brief save FFTW wisdom to a file
  !
  !> @date 05/30/19 WCL 1.0 original, based on MDG code in DSHT.f90
  !> @date 08/06/19 WCL converted to object oriented
  !--------------------------------------------------------------------------
  recursive subroutine FFTWisdomType_Save(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTWisdomType_Save
    use FFTW3MOD
    use error
  implicit none
    class    (FFTWisdomType), intent(in)  :: this
    character(c_char       ), allocatable :: name(:)! fftw file name
    integer  (c_int        )              :: status ! fftw return code

    if(this%loaded) then
      name = FFTWisdomType_Name() ! get the wisdom file name
      status = fftw_export_wisdom_to_filename(name) ! write accumulated wisdom to file
      if (status .eq. 0) call FatalError('FFTWisdomType_Save','error writing wisdom to file') ! handle error
      deallocate(name) ! clean up string
      ! call fftw_cleanup() ! free up any extra fftw memory usage (outside of existing plans)
    endif

  end subroutine FFTWisdomType_Save

  !@brief: automatically clean up on destruction
  recursive subroutine FFTWisdomType_Finalize(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTWisdomType_Finalize
  implicit none
    type(FFTWisdomType),intent(inout) :: this
    call this%save()
  end subroutine FFTWisdomType_Finalize


  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  !                                                                   !
  !                  member functions for FFTBuffer                   !
  !                                                                   !
  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

  !@brief   : (re)allocate a buffer
  !@param sz: size of array to allocate (number of doubles)
  recursive subroutine FFTBuffer_AllocReal(this, sz)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTBuffer_AllocReal
  implicit none
    class  (FFTBuffer),INTENT(INOUT) :: this 
    integer           ,INTENT(IN   ) :: sz
    call this%free() ! free memory if it is already allocated
    allocate(this%ptr) ! allocate a new pointer
    this%ptr = fftw_alloc_real   (int(sz, C_SIZE_T)) ! allocate memory
  end subroutine FFTBuffer_AllocReal

  !@brief   : (re)allocate a buffer
  !@param sz: size of array to allocate (number of complex doubles)
  recursive subroutine FFTBuffer_AllocCplx(this, sz)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTBuffer_AllocCplx
  implicit none
    class  (FFTBuffer),INTENT(INOUT) :: this 
    integer           ,INTENT(IN   ) :: sz
    call this%free() ! free memory if it is already allocated
    allocate(this%ptr) ! allocate a new pointer
    this%ptr = fftw_alloc_complex(int(sz, C_SIZE_T)) ! allocate memory
  end subroutine FFTBuffer_AllocCplx

  !@brief: deallocate a buffer (if it is allocated)
  recursive subroutine FFTBuffer_Free(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTBuffer_Free
  implicit none
    class(FFTBuffer),INTENT(INOUT) :: this 

    ! the extra check is needed here since nested types wont initialize this%ptr = c_null_ptr 
    if(allocated(this%ptr)) then ! we have allocated a pointer
      if(c_associated(this%ptr)) call fftw_free(this%ptr) ! free if it allocated
      this%ptr = c_null_ptr ! zero
      deallocate(this%ptr) ! deallocate pointer
    endif
  end subroutine FFTBuffer_Free

  !@brief: automatically clean up on destruction
  recursive subroutine FFTBuffer_Finalize(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTBuffer_Finalize
  implicit none
    type(FFTBuffer),INTENT(INOUT) :: this
    call this%free()
  end subroutine FFTBuffer_Finalize

  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  !                                                                   !
  !                   member functions for FFTPlan                    !
  !                                                                   !
  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

  !@brief: allocate space for the ptr and zero
  recursive subroutine FFTPlan_Init(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTPlan_Init
  implicit none
    class(FFTPlan),INTENT(INOUT) :: this
    call this%destroy() ! clean out any existing plan
    allocate(this%ptr) ! allocate a fresh pointer
    this%ptr = c_null_ptr ! zero
  end subroutine FFTPlan_Init

  !@brief: deallocate a buffer (if it is allocated)
  recursive subroutine FFTPlan_Destroy(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTPlan_Destroy
  implicit none
    class(FFTPlan),INTENT(INOUT) :: this
    if(allocated(this%ptr)) then ! there may be a plan stored here
      if(c_associated(this%ptr)) call fftw_destroy_plan(this%ptr) ! free plan if it exists
      this%ptr = c_null_ptr ! zero
      deallocate(this%ptr) ! deallocate pointer
    endif
  end subroutine FFTPlan_Destroy

  !@brief: automatically clean up on destruction
  recursive subroutine FFTPlan_Finalize(this)
  !DEC$ ATTRIBUTES DLLEXPORT :: FFTPlan_Finalize
  implicit none
    type(FFTPlan),INTENT(INOUT) :: this
    call this%destroy()
  end subroutine FFTPlan_Finalize

  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  !                                                                   !
  !                   member functions for RealFFT                    !
  !                                                                   !
  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

  !@brief                 : (re)initialize a real to complex transformer
  !@param sz  [IN] integer: length of transform (length of real data)
  !@param flg [IN] integer: planning flags
  recursive subroutine RealFFT_Init(this, sz, flg)
  !DEC$ ATTRIBUTES DLLEXPORT :: RealFFT_Init
  implicit none
    class  (RealFFT         ),INTENT(INOUT)          :: this 
    integer                  ,INTENT(IN   )          :: sz
    integer                  ,INTENT(IN   ),optional :: flg
    type   (c_ptr           )                        :: pr, pc
    real   (c_double        )              ,pointer  :: in (:)
    complex(c_double_complex)              ,pointer  :: out(:)
    integer                                          :: vFlg

    ! clear plans if needed
    call this%pFwd%init()
    call this%pFwd%init()

    ! allocate some space to plan in
    pr = fftw_alloc_real   (int(sz,C_SIZE_T))
    pc = fftw_alloc_complex(int(sz,C_SIZE_T))
    call c_f_pointer(pr, in , [sz])
    call c_f_pointer(pc, out, [sz])
    in  = 0.D0
    out = cmplx(0.D0,0.D0)

    ! do the planning
    vFlg = FFTW_ESTIMATE
    if(present(flg)) vFlg = flg
    this%pFwd%ptr = fftw_plan_dft_r2c_1d(sz, in , out, FFTW_FORWARD  + vFlg)
    this%pRev%ptr = fftw_plan_dft_c2r_1d(sz, out, in , FFTW_BACKWARD + vFlg)
      
    ! clean up memory
    call fftw_free(pr)
    call fftw_free(pc)

  end subroutine RealFFT_Init

  !@brief        : execute a real to half complex transform
  !@param signal : real data to compute DFT of
  !@param spectra: location to write half complex DFT
  recursive subroutine RealFFT_Fwd(this, signal, spectra)
  !DEC$ ATTRIBUTES DLLEXPORT :: RealFFT_Fwd
  implicit none
    class  (RealFFT         ),INTENT(IN   )         :: this
    real   (c_double        ),INTENT(IN   ),pointer :: signal (:)
    complex(c_double_complex),INTENT(INOUT),pointer :: spectra(:)
    call fftw_execute_dft_r2c(this%pFwd%ptr, signal, spectra) ! do fft
  end subroutine RealFFT_Fwd

  !@brief        : execute a half complex to real transform
  !@param spectra: half complex DFT to compute inverse of
  !@param signal : location to write real signal
  recursive subroutine RealFFT_Inv(this, spectra, signal)
  !DEC$ ATTRIBUTES DLLEXPORT :: RealFFT_Inv
  implicit none
    class  (RealFFT         ),INTENT(IN   )         :: this
    complex(c_double_complex),INTENT(IN   ),pointer :: spectra(:)
    real   (c_double        ),INTENT(INOUT),pointer :: signal (:)
    call fftw_execute_dft_c2r(this%pRev%ptr, spectra, signal) ! do inverse fft
  end subroutine RealFFT_Inv


  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  !                                                                   !
  !                  member functions for Real3DFFT                   !
  !                                                                   !
  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

  !@brief                 : (re)initialize a complex to real transformer
  !@param sz  [IN] integer: cube size lenth length of transform (sz*sz*sz cube)
  !@param flg [IN] integer: planning flags
  recursive subroutine Real3DFFT_Init(this, sz, flg)
  !DEC$ ATTRIBUTES DLLEXPORT :: Real3DFFT_Init
  implicit none
    class  (Real3DFFT       ),INTENT(INOUT)          :: this 
    integer                  ,INTENT(IN   )          :: sz
    integer                  ,INTENT(IN   ),optional :: flg
    type   (c_ptr           )                        :: pr, pc
    real   (c_double        )              ,pointer  :: in (:,:,:)
    complex(c_double_complex)              ,pointer  :: out(:,:,:)
    integer                                          :: vFlg, hz

    ! clear plan if needed
    call this%pRev%init()

    ! allocate some space to plan in
    hz = sz / 2 + 1
    pr = fftw_alloc_real   (int(sz * sz * sz,C_SIZE_T))
    pc = fftw_alloc_complex(int(sz * sz * hz,C_SIZE_T))
    call c_f_pointer(pr, in , [sz, sz, sz])
    call c_f_pointer(pc, out, [sz, sz, hz])
    in  = 0.D0
    out = cmplx(0.D0,0.D0)

    ! do the planning
    vFlg = FFTW_ESTIMATE
    if(present(flg)) vFlg = flg
    this%pRev%ptr = fftw_plan_dft_c2r_3d(sz, sz, sz, out, in , FFTW_BACKWARD + vFlg)

    ! clean up memory
    call fftw_free(pr)
    call fftw_free(pc)

  end subroutine Real3DFFT_Init

  !@brief        : execute a half complex to real transform
  !@param spectra: half complex DFT to compute inverse of
  !@param signal : location to write real signal
  recursive subroutine Real3DFFT_Inv(this, spectra, signal)
  !DEC$ ATTRIBUTES DLLEXPORT :: Real3DFFT_Inv
  implicit none
    class  (Real3DFFT       ),INTENT(IN   )         :: this
    complex(c_double_complex),INTENT(IN   ),pointer :: spectra(:,:,:)
    real   (c_double        ),INTENT(INOUT),pointer :: signal (:,:,:)
    call fftw_execute_dft_c2r(this%pRev%ptr, spectra, signal) ! do inverse fft
  end subroutine Real3DFFT_Inv

  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
  !                                                                   !
  !                 member functions for RealSep3DFFT                 !
  !                                                                   !
  ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !


    ! type   (FFTPlan         )         :: pX       ! reverse plan for X direction
    ! type   (FFTPlan         )         :: pY       ! reverse plan for Y direction
    ! type   (FFTPlan         )         :: pZ       ! reverse plan for Z direction
    ! type   (FFTBuffer       )         :: pWrk     ! fft allocated work space for intermediate transform
    ! complex(c_double_complex),pointer :: wrk(:,:,:) ! fortran wrapped work space (holds a single YZ slice)
    ! integer                              vN, vH   ! cube is vN^3 with only the first vH z slices needed in the output


  !@brief                 : (re)initialize a complex to real transformer
  !@param sz  [IN] integer: cube size lenth length of transform (sz*sz*sz cube)
  !@param flg [IN] integer: planning flags
  recursive subroutine RealSep3DFFT_Init(this, sz, flg)
  !DEC$ ATTRIBUTES DLLEXPORT :: RealSep3DFFT_Init
  implicit none
    class  (RealSep3DFFT    ),INTENT(INOUT)          :: this 
    integer                  ,INTENT(IN   )          :: sz
    integer                  ,INTENT(IN   ),optional :: flg
    type   (c_ptr           )                        :: pr, pc
    real   (c_double        )              ,pointer  :: out(:,:,:)
    complex(c_double_complex)              ,pointer  :: in (:,:,:)
    integer                                          :: vFlg, hz

    integer                                          :: rank, nn(1), howmany, sign
    integer                                          :: inembed(1), istride, idist
    integer                                          :: onembed(1), ostride, odist

    ! clear plan if needed
    call this%pX%init()
    call this%pY%init()
    call this%pZ%init()

    ! save dimensions
    this%vN = sz
    this%vH = sz/2+1

    ! allocate + wrap work space
    call this%pWrk%allocCplx(sz * sz)
    call c_f_pointer(this%pWrk%ptr, this%wrk, [sz, sz, sz])

    ! now allocate temporary space to plan in
    hz = sz / 2 + 1
    pr = fftw_alloc_real   (int(sz * sz * sz,C_SIZE_T))
    pc = fftw_alloc_complex(int(sz * sz * hz,C_SIZE_T))
    call c_f_pointer(pc, in , [sz, sz, hz])
    call c_f_pointer(pr, out, [sz, sz, sz])
    in  = cmplx(0.D0,0.D0)
    out = 0.D0

    ! get planning flags
    vFlg = FFTW_ESTIMATE
    if(present(flg)) vFlg = flg
    
    ! do the planning
    ! this%pRev%ptr = fftw_plan_dft_c2r_3d(sz, sz, sz, out, in , FFTW_BACKWARD + vFlg)

    rank           = 1                                   !individual transforms are all 1D
    nn(1)          = this%vN                             !individual transforms are all of length n
    howmany        = this%vN                             !how many transformations will be performed (one down each z for each y at a single x)
    inembed(1)     = 0                                   !dimensions of super array that input  is row major subarray of (null for not a subarray)
    istride        = this%vN * this%vH                   !stride between sequential elements (z spacing)
    idist          = this%vH                             !kth fft input at in + k * idist (y spacing)
    onembed(1)     = 0                                   !dimensions of super array that output is row major subarray of (null for not a subarray)
    ostride        = 1                                   !output stride
    odist          = this%vN                             !kth fft outputs to out + k * odist
    sign           = FFTW_BACKWARD                       !inverse transform

    ! unsigned flags     = FFTW_DESTROY_INPUT | (unsigned)pFlag;//planning flags
    this%pZ%ptr = fftw_plan_many_dft    (rank, nn, howmany, in      , inembed, istride, idist, this%wrk,&
                                         onembed, ostride, odist  , sign, vFlg)!1st: transform down z for all y at a single x (into work array)
    this%pY%ptr = fftw_plan_many_dft    (rank, nn, this%vH, this%wrk, inembed, this%vN, 1    , in      ,&
                                         onembed, this%vH, istride, sign, vFlg)!2nd: transform down y for all z at a single x (into original 3d input from work)
    this%pX%ptr = fftw_plan_many_dft_c2r(rank, nn, howmany, in      , inembed, 1      , idist, out     ,&
                                         onembed, ostride, odist  ,       vFlg)!3rd: transform down x for all y at a single z (into work array)

    ! clean temporary space
    call fftw_free(pr)
    call fftw_free(pc)

  end subroutine RealSep3DFFT_Init

  !@brief        : execute a half complex to real transform
  !@param spectra: half complex DFT to compute inverse of
  !@param signal : location to write real signal
  !@param dx     : spacing of non zero YZ planes (at 1, 1+dx, 1+dx*2, 1+dx*3, ...)
  recursive subroutine RealSep3DFFT_Inv(this, spectra, signal, dx)
  !DEC$ ATTRIBUTES DLLEXPORT :: RealSep3DFFT_Inv
  implicit none
    class  (RealSep3DFFT    ),INTENT(IN   )         :: this
    complex(c_double_complex),INTENT(IN   ),pointer :: spectra(:,:,:)
    real   (c_double        ),INTENT(INOUT),pointer :: signal (:,:,:)
    integer                  ,INTENT(IN   )         :: dx
    integer                                         :: i

    do i = 1, this%vH, dx ! loop over yz planes doing 2d transforms
      call fftw_execute_dft (this%pZ%ptr, spectra (i,:,:), this%wrk(:,:,1)) ! do Z transform into first slice of work array
      call fftw_execute_dft (this%pY%ptr, this%wrk(:,:,1), spectra (i,:,:)) ! do Y transform back overtop of input
    enddo
    ! do i = 1, this%vH ! loop up xy planes doing batches of 1d c2r transforms, does 1 extra plane for extraction of 3x3x3 neighborhood at upper glide
    do i = 1, this%vN ! loop up xy planes doing batches of 1d c2r transforms, does entire cube
      call fftw_execute_dft_c2r (this%pX%ptr, spectra(:,:,i), signal(:,:,i))
    enddo

  end subroutine RealSep3DFFT_Inv

end module fft_wrap
