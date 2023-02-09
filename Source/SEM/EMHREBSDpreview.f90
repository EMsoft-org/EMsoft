program EMHREBSDpreview
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
  type(HREBSDpreviewNameListType)        :: enl
  type(EBSDAngleType),pointer            :: angles
  type(EBSDSEMArray)                     :: SEM
  type(MCCLNameListType)                 :: mcnl
  type(EBSDMasterNameListType)           :: mpnl
  type(EBSDMCdataType)                   :: EBSDMCdata
  type(EBSDMPdataType)                   :: EBSDMPdata

  real(kind=sgl),allocatable             :: eulerangles(:,:), pc(:,:),cost(:), Ftensor(:,:)
  real(kind=sgl),allocatable             :: eulerangles_NMS(:,:), pc_NMS(:,:),cost_NMS(:), Ftensor_NMS(:,:)
  integer(kind=irg)                      :: Dim_XC, numangles,iunitexpt, maxthreads, tick, tock, tickstart
  real(kind=sgl),parameter               :: dtor = 0.0174533  ! convert from degrees to radians
  integer(HSIZE_T)                       :: offset3(3), offset3new(3), newspot
  integer(kind=ill)                      :: ispot
  integer(kind=irg)                      :: i, j, res, hdferr, numangle, nfeval, icount, numres
  integer(kind=irg)                      :: istat, istats,ipf_wd,ipf_ht,binx,biny,recordsize
  real(kind=sgl),allocatable             :: XCminV(:), XCmaxV(:), bestmem_XC(:)
  real(kind=sgl)                         :: bestval,st_initial(3),pc_initial(3), q_c(4), tstart, tstop
  character(11)                          :: dstr
  character(15)                          :: tstrb
  character(15)                          :: tstre
  character(fnlen)                       :: datafile, groupname, dataset, datagroupname
  type(HDFobjectStackType)               :: HDF_head
  type (particle), dimension(:), allocatable :: swarm
  type (particle), allocatable           :: best_particle
  real(kind=sgl)                         :: w, w_damp,c1,c2
  integer(kind=irg)                 :: dims(2)
  real(kind=dbl),allocatable        :: a(:,:)
  real(kind=dbl),allocatable        :: c(:,:)

  nmldeffile = 'EMHREBSDpreview.nml'
  progname   = 'EMHREBSDpreview.f90'
  progdesc   = 'High angular resolution EBSD preview'
  
  ! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments (using EMHREBSDpreview template file code = 286)
call Interpret_Program_Arguments(nmldeffile,1,(/ 286 /), progname)

! deal with the namelist stuff
res = index(nmldeffile,'.nml',kind=irg)
if (res.eq.0) then
  call FatalError('EMHREBSDpreview','JSON input not yet implemented')
else
  call GetHREBSDpreviewNameList(nmldeffile,enl)
end if

call MasterSubroutine(enl, progname, nmldeffile)
 
end program EMHREBSDpreview

subroutine MasterSubroutine(enl, progname, nmldeffile)
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

  
  type(HREBSDpreviewNameListType),INTENT(INOUT)       :: enl
  character(fnlen),INTENT(IN)                         :: progname
  character(fnlen),INTENT(IN)                         :: nmldeffile

  character(fnlen)                                    :: ename, image_filename
  integer(kind=irg)                                   :: ROI_size, iunitexpt, recordsize, ierr, kk, ii, jj, i, j, numr, numw, &
                                                      binx, biny, xoffset, yoffset, io_int(2), istat, L, patsz , hdferr, nx, ny, &
                                                      interp_size, interp_grid
  integer(HSIZE_T)                                    :: dims2(2), dims3(3), offset3(3)
  logical                                             :: f_exists
  real(kind=sgl)                                      :: mi, ma, io_real(1), ave, std, q(2), interp_step
  real(kind=dbl)                                      :: x, y, val, v2
  real(kind=sgl),allocatable                          :: window(:,:), expt(:), pattern(:,:), pattern_test(:,:), pcopy(:,:), &
                                                      hpvals(:), lpvals(:), sumexpt(:), pcopy_ROI(:,:), &
                                                      pcopy_ROI_test(:,:), interp_ngrid(:), ngrid(:), z_peak(:,:)
  real(kind=dbl),allocatable                          :: XCF(:,:)
  integer(kind=irg),allocatable                       :: nrvals(:), pint(:,:), ppp(:,:), pint_test(:,:), XCFint(:,:)
  type(C_PTR)                                         :: planf, planb
  real(kind=sgl),allocatable                          :: hpmask_shifted(:,:), lpmask_shifted(:,:)
  complex(C_DOUBLE_COMPLEX),pointer                   :: inp(:,:), outp(:,:)
  type(c_ptr), allocatable                            :: ip, op
  real(kind=dbl),allocatable                          :: rrdata(:,:), ffdata(:,:), ksqarray(:,:)
  real(kind=dbl),allocatable                          :: rrdata_test(:,:), ffdata_test(:,:)
  integer(kind=irg)                                   :: max_pos(2), size_interp
  ! declare variables for use in object oriented image module
  integer                                             :: iostat
  character(len=128)                                  :: iomsg
  logical                                             :: isInteger
  type(image_t)                                       :: im, im2
  integer(int8)                                       :: i8 (3,4), int8val
  integer(int8), allocatable                          :: output_image(:,:), output_image_XCF(:,:)


  call h5open_EMsoft(hdferr)

  binx = enl%numsx
  biny = enl%numsy
  L = binx * biny
  recordsize = 4 * L
  patsz = L

  ! size of region of interest
  ROI_size=2**enl%dimROI
  
  allocate(expt(patsz))
  dims3 = (/ binx, biny, 1 /)
  ! open the file with experimental patterns; depending on the inputtype parameter
  istat = openExpPatternFile(enl%exptfile, enl%ipf_wd, L, enl%inputtype, recordsize, iunitexpt, enl%HDFstrings)

  if (istat.ne.0) then
      call patternmod_errormessage(istat)
      call FatalError("MasterSubroutine:", "Fatal error handling experimental pattern file")
  end if

! and read the pattern (again)
  offset3 = (/ 0, 0, enl%paty * enl%ipf_wd + enl%patx /)

  call getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, iunitexpt, enl%inputtype, enl%HDFstrings, expt)
  
  ! and close the pattern file
  call closeExpPatternFile(enl%inputtype, iunitexpt)
  dims2=(/ROI_size, ROI_size/)
! use the center of the diffraction pattern to get ROI and turn it into a 2D pattern
  allocate(pattern(dims2(1),dims2(2)), pattern_test(dims2(1),dims2(2)), XCF(2*dims2(1)-1,2*dims2(1)-1), stat=ierr)
  allocate(pcopy(binx, biny), pint(dims2(1),dims2(2)), pint_test(dims2(1),dims2(2)), ppp(dims2(1),dims2(2)), stat=ierr)
  allocate(pcopy_ROI(dims2(1),dims2(2)), XCFint(2*dims2(1)+1,2*dims2(1)+1), stat=ierr)

  do kk=1,biny
    pcopy(1:binx,kk) = expt((kk-1)*binx+1:kk*binx)
  end do
  
! image intensity normalization with standard deviation and zero mean
  ave=sum(pcopy)/size(pcopy)
  std=sqrt(sum((pcopy-ave)**2)/size(pcopy))
  pcopy=(pcopy-ave)/std

! region of interest
  pcopy_ROI=pcopy((binx-ROI_size)/2:(binx-ROI_size)/2+ROI_size-1,(biny-ROI_size)/2:(biny-ROI_size)/2+ROI_size-1)
! shifted region of interest
  pcopy_ROI_test=pcopy((binx-ROI_size)/2+20:(binx-ROI_size)/2+ROI_size-1+20,(biny-ROI_size)/2+20:(biny-ROI_size)/2+ROI_size-1+20)

  ! do we need to extract this pattern from the file and store it as an image file ?
  if (trim(enl%patternfile).ne.'undefined') then
  ! allocate a byte array for the final output TIFF image that will contain all individual images
    allocate(output_image(binx,biny))
    image_filename = trim(EMsoft_getEMdatapathname())//trim(enl%patternfile)
    image_filename = EMsoft_toNativePath(image_filename)
  
    ma = maxval(pcopy)
    mi = minval(pcopy)
  
    do i=1,binx
      do j=1,biny
       int8val = int(255.0*(pcopy(i,biny-j+1)-mi)/(ma-mi))
       output_image(i,j) = int8val
      end do
    end do
  
   ! set up the image_t structure
    im = image_t(output_image)
    if(im%empty()) call Message("EMHREBSDpreview","failed to convert array to image")
  
   ! create the file
    call im%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
    if(0.ne.iostat) then
      call Message("failed to write image to file : "//iomsg)
    else  
      call Message('  Selected pattern written to '//trim(image_filename))
    end if 
    deallocate(output_image)
  end if
  
  
  ! define the nregions array
  numr = enl%nsteps 
  allocate(nrvals(numr))
  nrvals = enl%nregionsmin  + (/ ((i-1), i=1,numr) /)
  numw = enl%nsteps

  ! the array for the high pass filter parameter 
  allocate(hpvals(numw))
  hpvals = (/ ((i-1)*(enl%hipasswmax/numw), i=1,numw) /)

  ! the array for the low pass filter parameter 
  allocate(lpvals(numw))
  lpvals = (/ (0.1+(i-1)*((enl%lowpasswmax-0.1)/numw), i=1,numw) /)
  interp_grid = 4

  allocate(ngrid(interp_grid+1))
  allocate(z_peak(interp_grid+1,interp_grid+1))
 
  ngrid =  (/ ((i-interp_grid/2-1.0), i=1,(interp_grid+1))/)
  interp_step= 0.01
  interp_size= interp_grid/interp_step+1;
  interp_ngrid =  (/ (-interp_grid/2+(i-1)*interp_step, i=1,interp_size)/)

  ! allocate a byte array for the final output TIFF image that will contain all individual images
  nx = numw * ROI_size
  ny = numw * ROI_size
  allocate(output_image(nx,ny))
  allocate(output_image_XCF(nx,ny))
  
  image_filename = trim(EMsoft_getEMdatapathname())//trim(enl%tifffile)
  image_filename = EMsoft_toNativePath(image_filename)

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

  ! define the Hann windowing function
  allocate(window(ROI_size,ROI_size))
  window=0.0
  call HannWindow(ROI_size, window)

  do kk=1,numw ! low pass
    print *, "when low pass filter =", lpvals(kk)
  do jj=1,numw  ! high pass

      call init_BandPassFilter((/ROI_size, ROI_size/), hpvals(jj), lpvals(kk), hpmask_shifted, &
      lpmask_shifted, inp, outp, planf, planb) 
 
      pattern = window*pcopy_ROI
      rrdata = dble(pattern)

      pattern_test = window*pcopy_ROI_test
      rrdata_test = dble(pattern_test)

      ffdata = applyBandPassFilter(rrdata, (/ ROI_size, ROI_size/), dble(hpmask_shifted), &
        dble(lpmask_shifted), inp, outp, planf, planb)

      ffdata_test = applyBandPassFilter(rrdata_test, (/ ROI_size, ROI_size/), dble(hpmask_shifted), &
      dble(lpmask_shifted), inp, outp, planf, planb)

      pattern = sngl(ffdata)
      pattern_test = sngl(ffdata_test)

      ma = maxval(pattern)
      mi = minval(pattern)      
      pint = nint(((pattern - mi) / (ma-mi))*255.0)

      ma = maxval(pattern_test)
      mi = minval(pattern_test)      
      pint_test = nint(((pattern_test - mi) / (ma-mi))*255.0)


      xoffset = (kk-1) * ROI_size + 1
  ! adaptive histogram equalization (only applied on diagonal boxes)
      if (kk.eq.jj) then 
          if (nrvals(jj).eq.0) then
              ppp = pint
          else
              ppp = adhisteq(nrvals(jj),ROI_size, ROI_size, pint)
          end if 
      else
         ppp=pint
      end if

  ! compute the cross correlation function in the Fourier space
    call cross_correlation_function((/ ROI_size, ROI_size/), dble(pattern), dble(pattern_test), XCF, max_pos) 
    z_peak=XCF(max_pos(1)-interp_grid/2:max_pos(1)+interp_grid/2,max_pos(2)-interp_grid/2:max_pos(2)+interp_grid/2)

    call peak_interpolation(max_pos, z_peak, 2*ROI_size-1, interp_step, interp_grid+1, ngrid, interp_size, interp_ngrid, q)
    print *, "XCF error", sum(abs(q-(/20,-20/))), "with high pass filter = ", hpvals(jj)

    ma = maxval(XCF)
    mi = minval(XCF)      
    XCFint = nint(((XCF - mi) / (ma-mi))*255.0)

  ! and store the pattern in the correct spot in the output_image array 
          yoffset =  (jj-1) * ROI_size + 1
         !print *,xoffset,yoffset
          do i=1,ROI_size
            do j=1,ROI_size
             output_image(xoffset+i-1, yoffset+j-1) = ppp(i,j)
             output_image_XCF(xoffset+i-1, yoffset+j-1) = XCFint(i+ROI_size/2,j+ROI_size/2)
            end do
          end do
  end do 
  end do

  call fftw_free(ip)
  call fftw_free(op)
  call fftw_cleanup()
  
  ! save the processed pattern panel
  ! set up the image_t structure
  im2 = image_t(output_image)
  if(im2%empty()) call Message("EMHREBSDpreview","failed to convert array to image")
  
  ! create the file
  call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
  if(0.ne.iostat) then
    call Message(" failed to write image to file : "//iomsg)
  else  
    call Message('  Preprocessed pattern array written to '//trim(image_filename))
  end if 
  deallocate(output_image)
  
  call Message('')
  call Message(' High-pass filter parameter values along vertical axis (T to B) :')
  do ii=1,numw
      io_real(1) = hpvals(ii)
      call WriteValue('',io_real,1,"(F10.6)")
  end do

  call Message('')
  call Message(' Low-pass filter parameter values along horizontal axis (L to R) :')
  do ii=1,numw
      io_real(1) = lpvals(ii)
      call WriteValue('',io_real,1,"(F10.6)")
  end do

  call Message('')
  call Message(' nregions values along vertical axis (diagonal from top left to bottom right):')
  do ii=1,numr
      io_int(1) = nrvals(ii)
      call WriteValue('',io_int,1)
  end do

  ! save the image panel for cross correlation function 
  image_filename = trim(EMsoft_getEMdatapathname())//trim(enl%xcffile)
  image_filename = EMsoft_toNativePath(image_filename)

  ! set up the image_t structure
  im2 = image_t(output_image_XCF)
  if(im2%empty()) call Message("EMHREBSDpreview","failed to convert array to image")
  
  ! create the file
  call im2%write(trim(image_filename), iostat, iomsg) ! format automatically detected from extension
  if(0.ne.iostat) then
    call Message(" failed to write image to file : "//iomsg)
  else  
    call Message(' Preprocessed pattern array written to '//trim(image_filename))
  end if 
  deallocate(output_image_XCF)

  call h5close_EMsoft(hdferr)

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
  q(2)=((z_size+1)/2-max_pos(2))+((interp_half-max_pos_interp(1))*interp_step);
    
end subroutine