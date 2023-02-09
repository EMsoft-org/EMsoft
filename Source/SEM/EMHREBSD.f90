program EMHREBSD
    use local
    use files
    use NameListTypedefs
    use NameListHandlers
    use JSONsupport
    use io
    use HDF5
    use HDFsupport
    use ISO_C_BINDING
    use error
    use detectors
    use EBSDmod
    use stringconstants
    use rotations
    use patternmod
    use NameListHDFwriters
    use omp_lib
    use PSO
    use timing
    use quaternions
  
    IMPLICIT NONE
    
    character(fnlen)                       :: nmldeffile, progname, progdesc
    type(HREBSDNameListType)               :: enl
    type(EBSDAngleType),pointer            :: angles
    type(EBSDSEMArray)                     :: SEM
    type(MCCLNameListType)                 :: mcnl
    type(EBSDMasterNameListType)           :: mpnl
    type(EBSDMCdataType)                   :: EBSDMCdata
    type(EBSDMPdataType)                   :: EBSDMPdata
  
    real(kind=sgl),allocatable             :: eulerangles(:,:), pc(:,:),cost(:), Ftensor(:,:)
    real(kind=sgl),allocatable             :: eulerangles_NMS(:,:), pc_NMS(:,:),cost_NMS(:), Ftensor_NMS(:,:)
    integer(kind=irg)                      :: Dim_XC, numangles,iunitexpt, maxthreads
    real(kind=sgl),parameter               :: dtor = 0.0174533  ! convert from degrees to radians
    integer(HSIZE_T)                       :: offset3(3), offset3new(3), newspot
    integer(kind=ill)                      :: ispot
    integer(kind=irg)                      :: i, j, res, hdferr, numangle, nfeval, icount, numres
    integer(kind=irg)                      :: istat, istats,ipf_wd,ipf_ht,binx,biny,recordsize
    real(kind=sgl),allocatable             :: XCminV(:), XCmaxV(:), bestmem_XC(:)
    real(kind=sgl)                         :: bestval,st_initial(3),pc_initial(3), q_c(4), tstart, tstop
    character(1)                           :: single_grain
    type (particle), dimension(:), allocatable :: swarm
    type (particle), allocatable           :: best_particle
    real(kind=sgl)                         :: w, w_damp,c1,c2
    integer(kind=irg)                      :: dims(2)
    real(kind=dbl),allocatable             :: a(:,:), c(:,:), Euler_Angle(:,:)

    nmldeffile = 'EMHREBSD.nml'
    progname   = 'EMHREBSD.f90'
    progdesc   = 'High angular resolution EBSD'
    
    ! print some information
  call EMsoft(progname, progdesc)
  
  ! deal with the command line arguments (using EMHREBSD template file code = 287)
  call Interpret_Program_Arguments(nmldeffile,1,(/ 287 /), progname)
  
  ! deal with the namelist stuff
  res = index(nmldeffile,'.nml',kind=irg)
  if (res.eq.0) then
    call FatalError('EMHREBSD','JSON input not yet implemented')
  else
    call GetHREBSDNameList(nmldeffile,enl)
  end if
  
  ! TO DO: HREBSD of multiple grains
  single_grain = 'n'

  nullify(angles)
  allocate(angles)
  ! number of Euler angles
  numangles = enl%ipf_wd*enl%ipf_ht

  call h5open_EMsoft(hdferr)

  call GetHREBSDData(enl, SEM, angles, numangles, single_grain, iunitexpt, &
  enl%HDFstrings, istats, verbose=.TRUE.)

  call h5close_EMsoft(hdferr)
  allocate(Euler_Angle(3,enl%ipf_wd*enl%ipf_ht))

  do i=1,enl%ipf_wd*enl%ipf_ht
  Euler_Angle(:,i) = qu2eu(angles%quatang(1:4,i))
  end do 
  !print *, Euler_Angle(:,1)
  deallocate(angles)
  call MasterSubroutine(enl, progname, numangles, Euler_Angle, nmldeffile)
 
  end program EMHREBSD
  
  subroutine MasterSubroutine(enl, progname, numangles, Euler_Angle, nmldeffile)
    use local
    use error
    use image
    use, intrinsic :: iso_fortran_env
    use io
    use filters
    use patternmod
    use NameListTypedefs
    use HDF5
    use FFTW3mod
    use rotations
    use HDFsupport
    use timing

    integer(kind=irg), INTENT(IN)                       :: numangles
    real(kind=dbl), INTENT(IN)                          :: Euler_Angle(3, numangles)
    type(HREBSDNameListType),INTENT(INOUT)              :: enl
    character(fnlen),INTENT(IN)                         :: progname
    character(fnlen),INTENT(IN)                         :: nmldeffile
  
    character(fnlen)                                    :: ename, image_filenam, datafile, groupname, dataset, datagroupname
    integer(kind=irg)                                   :: ROI_size, iunitexpt, recordsize, ierr, kk, ii, jj, i, j, numr, numw, &
                                                        binx, biny, xoffset, yoffset, io_int(2), istat, L, patsz , hdferr, nx, ny, &
                                                        interp_size, interp_grid, N_pattern, tick, tock, tickstart
    integer(HSIZE_T)                                    :: dims2(2), dims3(3), offset3(3), offset(3)
    logical                                             :: f_exists
    real(kind=sgl)                                      :: mi, ma, io_real(1), ave, std, q(2), interp_step, C(6,6), Distance(2), &
                                                          R_x, R_y
    real(kind=sgl),parameter                            :: dtor = 0.0174533  ! convert from degrees to radians
    real(kind=dbl)                                      :: x, y, val, Ftensor(9), R_sample(3,3), Smatrix(3,3), w(3,3), &
                                                          R_tilt(3,3), F_sample(3,3), R_detector(3,3), strain_sample(3,3), &
                                                          strain(3,3,numangles), rotation(3,3,numangles), minf(numangles), &
                                                          shift_data(3,21,numangles), beta_sample(3,3)
    real(kind=sgl),allocatable                          :: window(:,:), expt(:), expt_ref(:), pattern(:,:), pattern_test(:,:), &
                                                        hpvals(:), lpvals(:), sumexpt(:), pcopy_ROI(:,:), q_shift(:,:), &
                                                        ref_p(:,:), pcopy_ROI_test(:,:), interp_ngrid(:), ngrid(:), &
                                                        z_peak(:,:), test_p(:,:), r(:,:)
    real(kind=dbl),allocatable                          :: XCF(:,:), ref_rotated(:,:)
    integer(kind=irg),allocatable                       :: nrvals(:), pint(:,:), ppp(:,:), pint_test(:,:), XCFint(:,:), &
                                                          roi_centre(:,:)
    type(C_PTR)                                         :: planf, planb
    real(kind=sgl),allocatable                          :: hpmask_shifted(:,:), lpmask_shifted(:,:)
    complex(C_DOUBLE_COMPLEX),pointer                   :: inp(:,:), outp(:,:)
    type(c_ptr), allocatable                            :: ip, op
    type(HDFobjectStackType)                            :: HDF_head
    real(kind=dbl),allocatable                          :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)
    real(kind=dbl),allocatable                          :: rrdata_test(:,:), ffdata_test(:,:)
    integer(kind=irg)                                   :: max_pos(2), size_interp
    ! declare variables for use in object oriented image module
    integer                                             :: iostat
    character(len=128)                                  :: iomsg
    character(11)                                       :: dstr
    character(15)                                       :: tstrb
    character(15)                                       :: tstre
    logical                                             :: isInteger
    type(image_t)                                       :: im, im2
    integer(int8)                                       :: i8 (3,4), int8val
    integer(int8), allocatable                          :: output_image(:,:), output_image_XCF(:,:)
  
  
    ! start the clock
    call Time_tick(tickstart)
    call Time_tick(tick)

    call h5open_EMsoft(hdferr)
  
    binx = enl%numsx
    biny = enl%numsy
    L = binx * biny
    recordsize = 4 * L
    patsz = L
    
    ! size of region of interest
    ROI_size=2**enl%size_ROI
    
    allocate(expt(patsz), expt_ref(patsz))
    dims3 = (/ binx, biny, 1 /)

    ! open the file with reference experimental pattern
    istat = openExpPatternFile(enl%exptfile, enl%ipf_wd, L, enl%inputtype, recordsize, iunitexpt, enl%HDFstrings)
  
    if (istat.ne.0) then
        call patternmod_errormessage(istat)
        call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
    end if
  
  ! and read the pattern (again)
    offset = (/ 0, 0, enl%paty * enl%ipf_wd + enl%patx /)
  
    call getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset, iunitexpt, enl%inputtype, enl%HDFstrings, expt_ref)
    
    ! and close the pattern file
    call closeExpPatternFile(enl%inputtype, iunitexpt)

    dims2=(/ROI_size, ROI_size/)

  ! use the center of the diffraction pattern to get ROI and turn it into a 2D pattern
    allocate(pattern(dims2(1),dims2(2)), pattern_test(dims2(1),dims2(2)), XCF(2*dims2(1)-1,2*dims2(1)-1), stat=ierr)
    allocate(test_p(biny, binx), ref_p(biny, binx), stat=ierr)
    allocate(ref_rotated(biny, binx), stat=ierr)
    allocate(pint(dims2(1),dims2(2)), pint_test(dims2(1),dims2(2)), ppp(dims2(1),dims2(2)), stat=ierr)
    allocate(pcopy_ROI(dims2(1),dims2(2)), XCFint(2*dims2(1)+1,2*dims2(1)+1), stat=ierr)
  
  ! turn the expt 1D array to 2D patterns
    do kk=1,biny
      ref_p(kk, 1:binx) = expt_ref((kk-1)*binx+1:kk*binx)
    end do
  
  ! image intensity normalization with standard deviation and zero mean
  ! reference pattern
    ave=sum(ref_p)/size(ref_p)
    std=sqrt(sum((ref_p-ave)**2)/size(ref_p))
    ref_p=(ref_p-ave)/std

  ! define the interpolation grid and parameters  
    interp_grid = 4
  
    allocate(ngrid(interp_grid+1))
    allocate(z_peak(interp_grid+1,interp_grid+1))
    allocate(q_shift(3, enl%N_ROI), r(3,enl%N_ROI), roi_centre(enl%N_ROI, 2) )

    ngrid =  (/ ((i-interp_grid/2-1.0), i=1,(interp_grid+1))/)
    interp_step= 0.01
    interp_size= interp_grid/interp_step+1;
    interp_ngrid =  (/ (-interp_grid/2+(i-1)*interp_step, i=1,interp_size)/)
  
  
    ! next we need to set up the high-pass filter fftw plans
    allocate(hpmask_shifted(ROI_size, ROI_size), lpmask_shifted(ROI_size, ROI_size), stat=istat)  
  
    if (istat .ne. 0) stop 'could not allocate hpmask, inp, outp arrays'
  
    allocate(rrdata(ROI_size, ROI_size),ffdata(ROI_size, ROI_size),stat=istat)
    if (istat .ne. 0) stop 'could not allocate rrdata, ffdata arrays'
    
    ! use the fftw_alloc routine to create the inp and outp arrays
    ip = fftw_alloc_complex(int(ROI_size**2,C_SIZE_T))
    call c_f_pointer(ip, inp, [ROI_size, ROI_size])
    
    op = fftw_alloc_complex(int(ROI_size**2,C_SIZE_T))
    call c_f_pointer(op, outp, [ROI_size, ROI_size])
    
    inp = cmplx(0.D0,0D0)
    outp = cmplx(0.D0,0.D0)
  
    ! allocate the Hann windowing function array
    allocate(window(ROI_size,ROI_size))
    window=0.0

    ! determine the values of the windowing function
    call HannWindow(ROI_size, window)

    ! determine the coordinates of the ROIs used for cross-correlation
    call setROI(enl, roi_centre, r)

    ! stiffness matrix in the crystal frame
    C = 0.0  
    C(1,1)=enl%C11
    C(2,2)=enl%C11
    C(1,2)=enl%C12
    C(2,1)=enl%C12
    C(4,4)=enl%C44
    C(5,5)=enl%C44
    
    ! cubic crystal
    if (enl%crystal.eq.'cub') then
      C(3,3)=enl%C11    
      C(1,3)=enl%C12 
      C(2,3)=enl%C12
      C(3,1)=enl%C12
      C(3,2)=enl%C12 
      C(6,6)=enl%C44
    ! hexagonal closed pack crystal
    else if (enl%crystal.eq.'hex') then
      C(3,3)=enl%C33    
      C(1,3)=enl%C13 
      C(2,3)=enl%C13
      C(3,1)=enl%C13
      C(3,2)=enl%C13
      C(6,6)=(enl%C11-enl%C12)/2  
    else
      write(*,*) "Undefined crystal structure for HREBSD"
    end if
    write(*,*)
    write(*,*) "Stiffness tensor (unit:GPa) = "

    do i = 1, ubound(C, 1)
      write(*,*) C(i, :)
    end do

    ! loop through patterns
    do j = 1, numangles  
      write(*,*)
      write(*,*) "Number of pattern = ", j
      ! position of the pattern to be used
      offset3 = (/ 0, 0, j-1 /)
      
      ! open the file with reference experimental pattern
      istat = openExpPatternFile(enl%exptfile, enl%ipf_wd, L, enl%inputtype, recordsize, iunitexpt, enl%HDFstrings)
    
      if (istat.ne.0) then
          call patternmod_errormessage(istat)
          call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
      end if

      call getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, enl%inputtype, enl%HDFstrings, expt)
      
      ! and close the pattern file
      call closeExpPatternFile(enl%inputtype, iunitexpt)
      
      ! test pattern (intensity normalization)
      do kk=1,biny
        test_p(kk, 1:binx) = expt((kk-1)*binx+1:kk*binx)
      end do
      ave=sum(test_p)/size(test_p)
      std=sqrt(sum((test_p-ave)**2)/size(test_p))
      test_p=(test_p-ave)/std

      do i = 1, enl%N_ROI  ! loop through all the ROIs

        ! region of interest of reference pattern
        pcopy_ROI=ref_p(roi_centre(i,2)-ROI_size/2:roi_centre(i,2)+ROI_size/2-1,&
        roi_centre(i,1)-ROI_size/2:roi_centre(i,1)+ROI_size/2-1)
        
        ! region of interest of test pattern
        pcopy_ROI_test=test_p(roi_centre(i,2)-ROI_size/2:roi_centre(i,2)+ROI_size/2-1,&
        roi_centre(i,1)-ROI_size/2:roi_centre(i,1)+ROI_size/2-1)
        
        ! initialize band pass fitlers (low pass and high pass)
        call init_BandPassFilter((/ROI_size, ROI_size/), enl%highpass, enl%lowpass, hpmask_shifted, &
        lpmask_shifted, inp, outp, planf, planb) 
        
        ! apply the windowing function on the reference ROI
        pattern = window*pcopy_ROI
        rrdata = dble(pattern)

        ! apply the windowing function on the test ROI
        pattern_test = window*pcopy_ROI_test
        rrdata_test = dble(pattern_test)

        ! apply the band pass fitlers on the reference ROI
        ffdata = applyBandPassFilter(rrdata, (/ ROI_size, ROI_size/), dble(hpmask_shifted), &
          dble(lpmask_shifted), inp, outp, planf, planb)

        ! apply the band pass fitlers on the test ROI
        ffdata_test = applyBandPassFilter(rrdata_test, (/ ROI_size, ROI_size/), dble(hpmask_shifted), &
        dble(lpmask_shifted), inp, outp, planf, planb)

        ! convert to single precision 
        pattern = (sngl(ffdata))
        pattern_test = (sngl(ffdata_test))

        ! compute the cross correlation function in the Fourier space
        call cross_correlation_function((/ ROI_size, ROI_size/), dble(pattern), dble(pattern_test), XCF, max_pos) 
       
        ! now crop out a small region around the peak of xcf
        z_peak=XCF(max_pos(1)-interp_grid/2:max_pos(1)+interp_grid/2,max_pos(2)-interp_grid/2:max_pos(2)+interp_grid/2)
      
        ! we do interpolation on thie small region
        call peak_interpolation(max_pos, z_peak, 2*ROI_size-1, interp_step, interp_grid+1, ngrid, interp_size, interp_ngrid, q)
        
        ! we can then find the shift vectors associated with the ROI with subpixel accuracy
        q_shift(:,i) = (/-q(1), -q(2), 0.0/)
        
        ! pattern center refinement (geometrically corrected, this assumes that the titlt is perfect 70 degree)
        ! reference: Britton et al, 2011, Ultramicroscopy
        if (enl%PCrefine.eq.'y') then
          ! shift of the beam position on the sample
          Distance(1) = (0.000001*enl%step_size)*(mod(j,enl%ipf_wd)-(enl%patx+1)) ! step size in unit of micron
          Distance(2) = (0.000001*enl%step_size)*((1+j/enl%ipf_wd)-(enl%paty+1))  ! step size in unit of micron
          ! ROI positions
          R_x = roi_centre(i,1)
          R_y = enl%numsy-roi_centre(i,2)
          ! diffraction pattern shift
          cosang = cos(70.0*dtor)  ! cosd is an intel compiler extension
          sinang = sin(70.0*dtor)  ! sind is an intel compiler extension
          q_shift(2,i) = q_shift(2,i)-Distance(2)/(0.000001*enl%delta)*(sinang-(enl%numsy/2-R_y)*cosang/enl%PC(3))
          q_shift(1,i) = q_shift(1,i)-1/(0.000001*enl%delta)*(-Distance(1)+(enl%numsx/2-R_x)*Distance(2)*cosang/enl%PC(3))    
        end if
      end do

      ! rotation matrix to sample frame
      R_tilt = eu2om((/0.D0, real(-enl%totaltilt*dtor,8), 0.D0/))
   
      ! optimization routine
      call main_minf(enl%N_ROI, real(r,8), real(q_shift,8), Euler_Angle(:,j), real(C,8), &
      Ftensor, minf(j), reshape(R_tilt,(/9/)))

      ! polar decomposition of the deformation tensor
      call getPolarDecomposition(reshape(Ftensor,(/3,3/)), R_detector, Smatrix)

      ! deformation tensor in to sample frame
      F_sample= matmul(matmul(R_tilt, reshape(Ftensor,(/3,3/))), transpose(R_tilt))

      ! polar decomposition of the deformation tensor
      call getPolarDecomposition(F_sample, R_sample, Smatrix)

      ! lattice rotation matrix in the sample frame
      w = 0.D0
      call Rot2LatRot(R_sample, w)

      ! distortion tensor
      beta_sample = F_sample-reshape((/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/),(/3,3/))
      strain_sample = 0.5*(transpose(beta_sample)+beta_sample)

      ! populate the data matrix
      strain(:,:,j) = strain_sample
      rotation(:,:,j) = w
      shift_data(:,:,j) = q_shift
      
      ! print rotation and strain tensor 
      write(*,*)
      write(*,*) 'Lattice Rotation Matrix (w) = '
      do i = 1, ubound(w, 1)
        write(*,*) w(i, :)
      end do
      write(*,*)
      write(*,*) 'Strain Tensor (e) = '
      do i = 1, ubound(strain_sample, 1)
        write(*,*) strain_sample(i, :)
      end do
      
      ! if (enl%Remap.eq.'y') then
      !   call fRemapbicubic(binx, biny, R_detector, real(enl%PC,8), real(ref_p,8), ref_rotated)
      ! end if

      
    end do

    ! save output data
    tstop = Time_tock(tickstart) 
    tock = Time_tock(tick)
    tstop = tstop - tstart

    ! ouptput data file in .h5 format
    call h5open_EMsoft(hdferr)

    call timestamp(datestring=dstr, timestring=tstrb)
    tstre = tstrb
    
    ! Create a new file using the default properties.
    datafile = trim(EMsoft_getEMdatapathname())//trim(enl%datafile)
    datafile = EMsoft_toNativePath(datafile)
    
    hdferr =  HDF_createFile(datafile, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createFile ')

    ! write the EMheader to the file
    groupname = 'HREBSDdata'
    hdferr = HDF_createGroup(groupname, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_createGroup HREBSDdata')

    ! execution time [s]
    dataset = SC_Duration
    hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloat Duration')

    dataset = 'EulerAngles'
    hdferr = HDF_writeDatasetFloatArray2D(dataset, sngl(Euler_Angle), 3, numangles, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetArray2D euler angles data')

    dataset = 'Shift'
    hdferr = HDF_writeDatasetFloatArray3D(dataset, sngl(shift_data), 3, 21, numangles, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetArray3D pattern shift data')

    dataset = 'Strain'
    hdferr = HDF_writeDatasetFloatArray3D(dataset, sngl(strain), 3, 3, numangles, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetArray3D strain data')
    
    dataset = 'Rotation'
    hdferr = HDF_writeDatasetFloatArray3D(dataset, sngl(rotation), 3, 3, numangles, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetArray3D rotation data')
    
    dataset = SC_numangles
    hdferr = HDF_writeDatasetInteger(dataset, numangles, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger numangles')

    dataset = 'PC'
    hdferr = HDF_writeDatasetFloatArray1D(dataset, enl%PC, 3, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetArray1D PC')

    dataset = 'minf'
    hdferr = HDF_writeDatasetFloatArray1D(dataset, sngl(minf), numangles, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetArray1D minf')

    dataset = 'PixelSize'
    hdferr = HDF_writeDatasetFloat(dataset, enl%delta, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetFloat PixelSize')

    dataset = 'RefPosition_X'
    hdferr = HDF_writeDatasetInteger(dataset, enl%patx, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger Reference Pattern Position X')

    dataset = 'RefPosition_Y'
    hdferr = HDF_writeDatasetInteger(dataset, enl%paty, HDF_head) 
    if (hdferr.ne.0) call HDF_handleError(hdferr,'HDF_writeDatasetInteger Reference Pattern Position Y')

    call HDF_pop(HDF_head)
    call h5close_EMsoft(hdferr)
    call fftw_free(ip)
    call fftw_free(op)
    call fftw_cleanup()
  end subroutine
  
  
  subroutine cross_correlation_function(dims,a,b,c,max_pos) 
  ! this routine assumes that a and b have the same input dimension
  ! INPUT
  ! dims: dimension of the array to be cross-correlated 
  ! a(dims): reference image array
  ! b(dims): test image  array  
  ! OUTPUT
  ! c(2*dims-1): cross-correlation function of a and b
  ! max_pos(2): location of the maximum value in the cross-correlation function
  use local   
  use FFTW3mod
  
  IMPLICIT NONE
  
  integer(kind=irg),intent(in)                                :: dims(2)
  real(kind=dbl),intent(in)                                   :: a(dims(1),dims(2)), b(dims(1),dims(2))
  real(kind=dbl),intent(out)                                  :: c(2*dims(1)-1,2*dims(1)-1)
  integer(kind=irg),intent(out)                               :: max_pos(2)
  type(C_PTR)                                                 :: plan, cplan
  complex(C_DOUBLE_COMPLEX),pointer                           :: inp(:,:), outp(:,:)
  type(c_ptr), allocatable                                    :: ip, op
  complex(C_DOUBLE_COMPLEX), allocatable                      :: ffta(:,:), fftb(:,:), fftc(:,:)
  integer(kind=irg)                                           :: cdims(2), i, j
  real(kind=dbl), allocatable                                 :: apad(:,:), bpad(:,:) 
  
  ! matrix dimensions
  cdims=2*dims-1
  
  ! allocate arrays for cross-correlation in Fourier space
  allocate(apad(cdims(1),cdims(2)),bpad(cdims(1),cdims(2)))
  apad = 0.D0
  bpad = 0.D0
  apad(1:dims(1),1:dims(2)) = a
  bpad(1:dims(1),1:dims(2)) = b(dims(1):1:-1,dims(2):1:-1)
  
  ! set up the fftw
  ip = fftw_alloc_complex(int(cdims(1)*cdims(2),C_SIZE_T))
  call c_f_pointer(ip, inp, [cdims(1),cdims(2)])
  
  op = fftw_alloc_complex(int(cdims(1)*cdims(2),C_SIZE_T))
  call c_f_pointer(op, outp, [cdims(1),cdims(2)])
  
  allocate(ffta(dims(1),dims(2)),fftb(dims(1),dims(2)),fftc(cdims(1),cdims(2)))
  
  inp = cmplx(0.D0,0D0)
  outp = cmplx(0.D0,0.D0)
  ! create plan for forward Fourier transform
  plan = fftw_plan_dft_2d(cdims(1),cdims(2), inp, outp, FFTW_FORWARD, FFTW_ESTIMATE)
  do j=1,cdims(1)
      do i=1, cdims(2)
       inp(j,i) = cmplx(apad(j,i),0.D0)    
      end do
  end do
  
  ! compute the Forward Fourier transform of a
  call fftw_execute_dft(plan, inp, outp)
  ffta=outp
  
  ! compute the Forward Fourier transform of b
  inp = cmplx(0.D0,0D0)
  outp = cmplx(0.D0,0.D0)
  do j=1,cdims(1)
      do i=1, cdims(2)
       inp(j,i) = cmplx(bpad(j,i),0.D0)    
      end do
  end do
  call fftw_execute_dft(plan, inp, outp)
  fftb=outp
  
  ! compute the inverse Fourier transform of invF(F(a)*F(b))
  fftc = ffta*fftb
  inp = cmplx(0.D0,0D0)
  outp = cmplx(0.D0,0.D0)
  
  cplan = fftw_plan_dft_2d(cdims(1),cdims(2), inp, outp, FFTW_BACKWARD, FFTW_ESTIMATE)
  
  call fftw_execute_dft(cplan, fftc, outp)
  c=real(outp)
  max_pos=maxloc(c)
  
  deallocate(ffta, fftb, fftc, apad, bpad)
  call fftw_free(ip)
  call fftw_free(op)
  call fftw_cleanup()
  end subroutine
  
  subroutine peak_interpolation(max_pos, z, z_size, interp_step, interp_size, ngrid, size_interp, interp_ngrid, q)
    use local 
    use Grid_Interpolation
    
    IMPLICIT NONE
    
    integer(kind=irg),intent(in) :: interp_size, size_interp, z_size, max_pos(2)
    real(kind=sgl),intent(in)    :: ngrid(interp_size), interp_step
    real(kind=sgl),intent(inout) :: interp_ngrid(size_interp), z(interp_size,interp_size)                       
    real(kind=sgl),intent(out)   :: q(2)
    integer(kind=irg)            :: md, ixi, iyi, ier, max_pos_interp(2), interp_half
    real(kind=sgl)               :: zi(size_interp,size_interp)
  
    do  iyi = 1, size_interp
      do  ixi = 1, size_interp
        if (ixi == 1.AND.iyi == 1) then
          md = 1
        else
          md = 2
        end if
        ! Rectangular-grid bivariate interpolation
        call rgbi3p(md, interp_size, interp_size, ngrid, ngrid, z, 1, interp_ngrid(ixi), interp_ngrid(iyi), zi(ixi,iyi), ier)
        if (ier > 0) stop
      end do
    end do
    ! location of the maximum value on the interpolated surface
    max_pos_interp = maxloc(zi)
  
    interp_half=(size_interp+1)/2
    ! shift vector
    q(1)=(max_pos(2)-(z_size+1)/2)+((max_pos_interp(2)-interp_half)*interp_step);
    q(2)=((z_size+1)/2-max_pos(1))+((interp_half-max_pos_interp(1))*interp_step);
      
  end subroutine

  ! set the number and location of region of interests 
  ! the center of the ROI ring is the pattern center of the reference pattern
  subroutine setROI(enl, roi_centre, r)
    use local
    use constants
    use NameListTypedefs

    implicit NONE

    type(HREBSDNameListType),INTENT(inout)          :: enl
    integer(kind=irg),intent(inout)                 :: roi_centre(enl%N_ROI,2)
    real(kind=sgl),intent(inout)                    :: r(3, enl%N_ROI)
    integer(kind=irg)                               :: Lx, Ly, i
    real(kind=sgl)                                  :: PC_x, PC_y, DD

    Lx = enl%numsx
    Ly = enl%numsy
    PC_x = enl%PC(1)
    PC_y = enl%PC(2)
    DD = enl%PC(3)

    do i=1,enl%N_ROI
        if (i.lt.enl%N_ROI) then
            roi_centre(i,1:2)=floor((/Lx/2+enl%roi_distance*cos(i*2*cPi/(enl%N_ROI-1)),&
            ((Ly/2)+enl%roi_distance*sin(i*2*cPi/(enl%N_ROI-1)))/))
        else
            roi_centre(i,1:2)=(/Lx/2,Ly/2/)
        end if
        r(1:3, i)=(/real(roi_centre(i,1))-PC_x,real(Ly-PC_y)-roi_centre(i,2), DD/)
    end do
  end subroutine

  ! the main subroutine for computing the bouned constrained optimization
  subroutine main_minf(N, r, q, Euler_Angle, C_c, Ftensor, minf, R_tilt)
    use local
    use math
    implicit NONE
    real(kind=dbl), INTENT(out)   :: Ftensor(9), minf
    integer(kind=irg), INTENT(in) :: N
    real(kind=dbl), INTENT(in)  :: r(3, N), q(3, N), Euler_Angle(3), C_c(6,6), R_tilt(9)
    external myfunc, myconstraint
    real(kind=dbl)    :: lb(9), ub(9), f(6,N), x(9), tol
    integer*8 opt
    integer(kind=irg) ::  ires, i
    real(kind=dbl) :: gs2c(3,3), gc2s(3,3), RM(6,6), RN(6,6), RM_inv(6,6), C_s(6,6), C(9,9)
    include 'nlopt.f'
    
    ! The nlopt_opt type corresponds to integer*8. 
    ! (Technically, we could use any type that is big enough to hold a pointer on all platforms;
    ! integer*8 is big enough for pointers on both 32-bit and 64-bit machines.)
    opt = 0

    ! Optimization algorithms that support nonlinear equality constraints
    ! NLOPT_LD_SLSQP: Sequential Quadratic Programming (SQP) algorithm 
    ! need all the partial derivatives 
    ! Local derivative based optimization method with nonlinear equality constraint
    
    ! The COBYLA might hang on several instances 
    ! Reported issue: https://github.com/stevengj/nlopt/issues/118
    ! NLOPT_LN_COBYLA: COBYLA (Constrained Optimization BY Linear Approximations)
    ! Local derivative free method with nonlinear equality constraint

    ! create the nlopt_opt (opt) C type object (a pointer)
    ! If the constructor succeeds, opt will be nonzero after nlo_create, 
    ! so you can check for an error by checking whether opt is zero 
    ! nlo_create(nlopt object, algorithm, number of variables)
    call nlo_create(opt, NLOPT_LN_COBYLA, 9)
  
    ! get the default lower/upper bounds (+-infinity) 
    ! integer: ires (positive: sucess, negative: failed)
    call nlo_get_upper_bounds(ires, opt, ub)
    call nlo_get_lower_bounds(ires, opt, lb)
  
    ! set the lower/upper bounds (bounded by +-0.05)
    lb = (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/)-0.03D0
    ub = (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/)+0.03D0
    call nlo_set_lower_bounds(ires, opt, lb)
    call nlo_set_upper_bounds(ires, opt, ub)

    ! myfunc defines the objective function to be used          
    f(1:3,:) = r
    f(4:6,:) = q
    ! write(*,*) 'ROI positions = '
    ! do i = 1, ubound(r, 2)
    !   write(*,*) r(:, i)
    ! end do

    ! write(*,*) 'refined shift vectors = '
    ! do i = 1, ubound(q, 2)
    !   write(*,*) q(:, i)
    ! end do
    ! set objective function
    call nlo_set_min_objective(ires, opt, myfunc, f)
    ! determine the orientation matrices and rotation matrices for 
    ! stiffness tensor coordinate transformation

    call StiffnessRotation(Euler_Angle, gs2c, gc2s, RM, RN) 
    call inv(6, 6, RM, RM_inv)

    ! Rotate stiffness tensor from crystal frame into sample frame
    C_s = matmul(matmul(RM_inv,C_c),RN)
    C = 0.D0
    C(1:6,1:6) = C_s
    C(7,1:9) = R_tilt

    ! add equality constraint (partial contraction boundary condition)
    call nlo_add_equality_constraint(ires, opt, myconstraint, C, 1.0D-6)

    ! step tolerance (|change in x|/|xtl_abs|<xtol_rel)
    call nlo_set_xtol_rel(ires, opt, 1.0D-6)

    ! function tolerance (|change in f(x)|/|f_abs|<ftol_abs)
    tol = 1.0D-6
    call nlo_set_ftol_rel(ires, opt, tol)

    ! set maximum number of evalutions
    call nlo_set_maxeval(ires, opt, 800)

    ! intial value for the deformation gradient tensor (unit matrix)
    x = (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/)
   
    ! initiate the optimization
    call nlo_optimize(ires, opt, x, minf)
     
    Ftensor = x
    if (ires.lt.0) then
      write(*,*) 'nlopt failed!'
      stop 1
    else

      write(*,*) 'found min at x = ', x
      write(*,*) 'min |f(x)| = ', minf
    endif
  
    call nlo_destroy(opt)
  
end subroutine

! my objective function
subroutine myfunc(val, n, x, grad, need_gradient, f)
  double precision val, x(n), grad(n), DD, f(6,21)
  integer n, need_gradient
  double precision, allocatable :: r1(:,:), r2(:,:), r3(:,:)
  double precision, allocatable :: q1(:,:), q2(:,:), q3(:,:)
  double precision, allocatable :: row1(:,:), row2(:,:), row3(:,:)
  integer dims2(2)
  
  !TODO: the number of region of interest (ROIs) default to 21
  DD = f(3,1)
  dims2 = shape(f)
  
  ! allocate the variables
  allocate(r1(3,dims2(2)),r2(3,dims2(2)),r3(3,dims2(2)))
  allocate(q1(3,dims2(2)),q2(3,dims2(2)),q3(3,dims2(2)))
  allocate(row1(3,dims2(2)),row2(3,dims2(2)),row3(3,dims2(2)))
  
  ! extract the cooridates of ROIs
  r1=spread(f(1,:),1,3)
  r2=spread(f(2,:),1,3)
  r3=spread(f(3,:),1,3)

  ! extract the shift vectors
  q1=spread(f(4,:),1,3)
  q2=spread(f(5,:),1,3)
  q3=spread(f(6,:),1,3)
  
  ! used for the objective function
  row1=0.D0
  row1(1,:)=1.D0
  row2=0.D0
  row2(2,:)=1.D0 
  row3=0.D0
  row3(3,:)=1.D0 

  ! for gradient based algorithms only
  if (need_gradient.ne.0) then 
      ! partial derivatives of the objective function (not used for derivative free algorithm)      
      grad(1)= 0.5*sum(norm2((DD*r1*row1)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
      grad(2)= 0.5*sum(norm2((DD*r1*row2)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
      grad(3)= 0.5*sum(norm2((DD*r1*row3)/(x(3)*r1 + x(6)*r2 + x(9)*r3) - (DD*r1*(x(1)*r1*row1 + x(2)*r1*row2 + &
       x(3)*r1*row3 + x(4)*r2*row1 + x(5)*r2*row2 + x(6)*r2*row3 + x(7)*r3*row1 + x(8)*r3*row2 + x(9)*r3*row3))/(x(3)*r1 + &
       x(6)*r2 + x(9)*r3)**2,1)**2)
      grad(4)= 0.5*sum(norm2((DD*r2*row1)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
      grad(5)= 0.5*sum(norm2((DD*r2*row2)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
      grad(6)= 0.5*sum(norm2((DD*r2*row3)/(x(3)*r1 + x(6)*r2 + x(9)*r3) - (DD*r2*(x(1)*r1*row1 + &
      x(2)*r1*row2 + x(3)*r1*row3 + x(4)*r2*row1 + x(5)*r2*row2 + x(6)*r2*row3 + &
      x(7)*r3*row1 + x(8)*r3*row2 + x(9)*r3*row3))/(x(3)*r1 + x(6)*r2 + x(9)*r3)**2,1)**2)
      grad(7)= 0.5*sum(norm2((DD*r3*row1)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
      grad(8)= 0.5*sum(norm2((DD*r3*row2)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
      grad(9)= 0.5*sum(norm2((DD*r3*row3)/(x(3)*r1 + x(6)*r2 + x(9)*r3) - (DD*r3*(x(1)*r1*row1 + &
       x(2)*r1*row2 + x(3)*r1*row3 + x(4)*r2*row1 + x(5)*r2*row2 + x(6)*r2*row3 + &
       x(7)*r3*row1 + x(8)*r3*row2 + x(9)*r3*row3))/(x(3)*r1 + x(6)*r2 + x(9)*r3)**2,1)**2)   
       print *, "objective function gradients = ", grad  
  endif
  ! value of the objective function 
  val = 0.5*sum(norm2((DD/(x(3)*r1+x(6)*r2+x(9)*r3))*(x(1)*(r1*row1)+x(4)*(r2*row1)+x(7)*(r3*row1)+&
  x(2)*(r1*row2)+x(5)*(r2*row2)+x(8)*(r3*row2)+x(3)*(r1*row3)+x(6)*(r2*row3)+x(9)*(r3*row3))-&
  ((r1+q1)*row1+(r2+q2)*row2+(r3+q3)*row3),1)**2)

end subroutine

! my traction free boundary constraint
subroutine myconstraint(val, n, x, grad, need_gradient, C)
  integer need_gradient
  double precision val, x(n), grad(n), C(9,9)
  double precision F_s(3,3), beta(3,3), e(3,3), C_s(6,6), e_s(6,1), s_s(6,1), R(3,3)

  ! extract the rotation matrix for sample tilt
  R = reshape(C(7,:),(/3,3/))
  ! extract the stiffness matrix components
  C_s = C(1:6,1:6)

  if (need_gradient.ne.0) then
    ! partial derivatives of the objective function (not used for derivative free algorithm)      
    grad(1)=abs(C(3,1))
    grad(2)=abs(C(3,6))
    grad(3)=abs(C(3,5))
    grad(4)=abs(C(3,6))
    grad(5)=abs(C(3,2))
    grad(6)=abs(C(3,4))
    grad(7)=abs(C(3,5))
    grad(8)=abs(C(3,4))
    grad(9)=abs(C(3,3))
    print *, "constraits gradients = ", grad
  endif
    
  ! partial traction boundary condition (sigma_33=0)
  F_s= matmul(matmul(R, reshape(x, (/3,3/))),transpose(R))
  beta= F_s-reshape((/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/),(/3,3/))
  e = 0.5*(beta+transpose(beta))
  e_s = reshape((/ e(1,1), e(2,2), e(3,3), 2*e(2,3), 2*e(1,3), 2*e(1,2)/), (/6,1/))
  s_s = matmul(C_s,e_s)
  val = s_s(3,1)

  end

  subroutine StiffnessRotation(Euler_Angle, gs2c, gc2s, RM, RN)
    ! Reference:
    ! Salvati, E., Sui, T. and Korsunsky, A.M., 2016. 
    ! Uncertainty quantification of residual stress evaluation by the FIB–DIC 
    ! ring-core method due to elastic anisotropy effects. 
    ! International Journal of Solids and Structures, 87, pp.61-69

    use local 
    use rotations
    use math
    
    IMPLICIT NONE
    real(kind=dbl), intent(in)  :: Euler_Angle(3)
    real(kind=dbl), intent(out) :: gs2c(3,3), gc2s(3,3), RM(6,6), RN(6,6) 
    real(kind=dbl), parameter   :: dtor = 0.0174533 ! degree to radians
    real(kind=dbl)              :: R(3,3)
    integer(kind=irg)           :: i

    ! Orientation matrix (sample to crystal) 
    gs2c = eu2om(Euler_Angle)
    ! Orientation matrix (crystal to sample)
    gc2s = transpose(gs2c)
 
! % T_sigma =
! %  
! % [   R11^2,   R12^2,   R13^2,         2*R12*R13,         2*R11*R13,         2*R11*R12]
! % [   R21^2,   R22^2,   R23^2,         2*R22*R23,         2*R21*R23,         2*R21*R22]
! % [   R31^2,   R32^2,   R33^2,         2*R32*R33,         2*R31*R33,         2*R31*R32]
! % [ R21*R31, R22*R32, R23*R33, R22*R33 + R23*R32, R21*R33 + R23*R31, R21*R32 + R22*R31]
! % [ R11*R31, R12*R32, R13*R33, R12*R33 + R13*R32, R11*R33 + R13*R31, R11*R32 + R12*R31]
! % [ R11*R21, R12*R22, R13*R23, R12*R23 + R13*R22, R11*R23 + R13*R21, R11*R22 + R12*R21]
    R=gs2c
    RM=reshape((/R(1,1)**2, R(1,2)**2, R(1,3)**2, 2*R(1,2)*R(1,3), 2*R(1,3)*R(1,1), 2*R(1,1)*R(1,2),&
    R(2,1)**2, R(2,2)**2, R(2,3)**2, 2*R(2,2)*R(2,3), 2*R(2,3)*R(2,1), 2*R(2,1)*R(2,2),&
    R(3,1)**2, R(3,2)**2, R(3,3)**2, 2*R(3,2)*R(3,3), 2*R(3,3)*R(3,1), 2*R(3,1)*R(3,2),&
    R(2,1)*R(3,1), R(2,2)*R(3,2), R(2,3)*R(3,3), R(2,2)*R(3,3)+R(2,3)*R(3,2), R(2,1)*R(3,3)+R(2,3)*R(3,1), &
    R(2,2)*R(3,1)+R(2,1)*R(3,2),R(1,1)*R(3,1), R(1,2)*R(3,2), R(1,3)*R(3,3), R(1,2)*R(3,3)+R(1,3)*R(3,2), &
    R(1,3)*R(3,1)+R(1,1)*R(3,3), R(1,1)*R(3,2)+R(1,2)*R(3,1),R(1,1)*R(2,1), R(1,2)*R(2,2), R(1,3)*R(2,3), &
    R(1,2)*R(2,3)+R(1,3)*R(2,2), R(1,3)*R(2,1)+R(1,1)*R(2,3), R(1,1)*R(2,2)+R(1,2)*R(2,1)/),(/6,6/))
    RM=transpose(RM)
! % T_epsilon =
! %  
! % [     R11^2,     R12^2,     R13^2,           R12*R13,           R11*R13,           R11*R12]
! % [     R21^2,     R22^2,     R23^2,           R22*R23,           R21*R23,           R21*R22]
! % [     R31^2,     R32^2,     R33^2,           R32*R33,           R31*R33,           R31*R32]
! % [ 2*R21*R31, 2*R22*R32, 2*R23*R33, R22*R33 + R23*R32, R21*R33 + R23*R31, R21*R32 + R22*R31]
! % [ 2*R11*R31, 2*R12*R32, 2*R13*R33, R12*R33 + R13*R32, R11*R33 + R13*R31, R11*R32 + R12*R31]
! % [ 2*R11*R21, 2*R12*R22, 2*R13*R23, R12*R23 + R13*R22, R11*R23 + R13*R21, R11*R22 + R12*R21]
    RN=reshape((/R(1,1)**2, R(1,2)**2, R(1,3)**2, R(1,2)*R(1,3), R(1,3)*R(1,1), R(1,1)*R(1,2),&
    R(2,1)**2, R(2,2)**2, R(2,3)**2, R(2,2)*R(2,3), R(2,3)*R(2,1), R(2,1)*R(2,2), &
    R(3,1)**2, R(3,2)**2, R(3,3)**2, R(3,2)*R(3,3), R(3,3)*R(3,1), R(3,1)*R(3,2), &
    2*R(2,1)*R(3,1), 2*R(2,2)*R(3,2), 2*R(2,3)*R(3,3), R(2,2)*R(3,3)+R(2,3)*R(3,2), &
    R(2,1)*R(3,3)+R(2,3)*R(3,1), R(2,2)*R(3,1)+R(2,1)*R(3,2), 2*R(1,1)*R(3,1), &
    2*R(1,2)*R(3,2), 2*R(1,3)*R(3,3), R(1,2)*R(3,3)+R(1,3)*R(3,2), &
    R(1,3)*R(3,1)+R(1,1)*R(3,3), R(1,1)*R(3,2)+R(1,2)*R(3,1), 2*R(1,1)*R(2,1), 2*R(1,2)*R(2,2), &
    2*R(1,3)*R(2,3), R(1,2)*R(2,3)+R(1,3)*R(2,2), R(1,3)*R(2,1)+R(1,1)*R(2,3),&
     R(1,1)*R(2,2)+R(1,2)*R(2,1)/),(/6,6/))
    RN=transpose(RN)
  
  
  end subroutine 

  subroutine Rot2LatRot(R_finite, w)
    use local 
    use rotations

    implicit NONE
    real(kind=dbl),intent(in)    :: R_finite(3,3)
    real(kind=dbl),intent(inout) :: w(3,3)
    real(kind=dbl)               :: v(4), rotation_vector(3)
    
    ! convert the rotation matrix to axis-angle pair
    v = om2ax(R_finite)
    rotation_vector=v(1:3)*v(4)

    ! population the lattice rotation matrix
    w(1,2) = -rotation_vector(3)
    w(1,3) = rotation_vector(2)
    w(2,3) = -rotation_vector(1)
    w(2,1) = -w(1,2)
    w(3,1) = -w(1,3)
    w(3,2) = -w(2,3)
    ! sample frame rotation
    w = transpose(w)
 end subroutine

! interpolation based remapping of diffraction pattern 
  subroutine fRemapbicubic(binx, biny, R, PC, image, image_rotated)
    use local 
    use rotations
    use math
    
    IMPLICIT NONE

    integer(kind=irg),intent(in)    :: binx, biny
    real(kind=dbl),intent(in)       :: R(3,3), PC(3), image(biny, binx)
    real(kind=dbl),intent(out)       :: image_rotated(biny, binx)
    real(kind=dbl)       :: yy, xx, P(4,4), rp(3,1), r_rot(3), row1, col1, &
                           dx, dy, a, b, intensity
    integer(kind=irg)   :: row2, col2, x1, y1
    real(kind=dbl) :: bicubicInterpolate
    do row2=1,biny
      do col2=1,binx
        yy = -(row2-PC(2))
        xx = col2-PC(1)

        rp=reshape((/xx,yy,PC(3)/),(/3,1/))
        r_rot = reshape(matmul((PC(3) / DOT_PRODUCT(reshape(matmul(R,rp),(/3/)),&
        (/0.D0, 0.D0, 1.D0/)))*R, rp),(/3/))
        row1 = -r_rot(2) + PC(2)
        col1 = r_rot(1) + PC(1)
        
        b = row1
        a = col1
        x1 = floor(a)
        y1 = floor(b)

        if ((x1.ge.2).and.(y1.ge.2).and.(x1.le.binx-2).and.(y1.le.biny-2)) then
            P = image(y1 -1:y1 +2, x1-1:x1+2)
            ! interpolation weights
            dx = a-x1
            dy = b-y1
            intensity = bicubicInterpolate(P, dx, dy)
        else
            intensity = 0.D0
        end if
        image_rotated(row2,col2) = intensity
      end do

    end do
  end subroutine  


  function bicubicInterpolate(p, x, y) result (q)
    use local 
    implicit NONE
    real(kind=dbl), intent(in) :: p(4,4), x, y
    real(kind=dbl)  :: q, q1, q2, q3, q4
    real(kind=dbl) :: cubicInterpolate
    q1 = cubicInterpolate(p(1,:), x)
    q2 = cubicInterpolate(p(2,:), x)
    q3 = cubicInterpolate(p(3,:), x)
    q4 = cubicInterpolate(p(4,:), x)
    q = cubicInterpolate((/q1, q2, q3, q4/), y)
  end
  
  function cubicInterpolate(p, x) result (q)
    use local 
    implicit NONE
    real(kind=dbl), intent(in) :: p(4), x
    real(kind=dbl)  :: q

    q = p(2) + 0.5 * x*(p(3) - p(1) + x*(2.0*p(1) - 5.0*p(2) + 4.0*p(3) - p(4) + x*(3.0*(p(2) - p(3)) + p(4) - p(1))))
  end
  
  ! Returns the inverse of a matrix A(nn,mm) calculated by finding the LU
  ! decomposition.  Depends on LAPACK.
  subroutine inv(nn, mm, A, Ainv)
    use local 
    implicit NONE
    integer(kind=irg), intent(in) :: nn, mm
    real(kind=dbl), dimension(nn,mm), intent(in) :: A
    real(kind=dbl), dimension(nn,mm), intent(out) :: Ainv
  
    real(kind=dbl), dimension(nn) :: work  ! work array for LAPACK
    integer(kind=irg), dimension(nn) :: ipiv   ! pivot indices
    integer(kind=irg) :: n, info
  
    ! External procedures defined in LAPACK
    external DGETRF
    external DGETRI
  
    ! Store A in Ainv to prevent it from being overwritten by LAPACK
    Ainv = A
    n = size(A,1)
  
    ! DGETRF computes an LU factorization of a general M-by-N matrix A
    ! using partial pivoting with row interchanges.
    call DGETRF(n, n, Ainv, n, ipiv, info)
  
    if (info /= 0) then
       stop 'Matrix is numerically singular!'
    end if
  
    ! DGETRI computes the inverse of a matrix using the LU factorization
    ! computed by DGETRF.
    call DGETRI(n, Ainv, n, ipiv, work, n, info)
  
    if (info /= 0) then
       stop 'Matrix inversion failed!'
    end if
  end subroutine