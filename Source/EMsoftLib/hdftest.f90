
program hdftest

use hdf5
use h5lt

IMPLICIT NONE

     CHARACTER(LEN=5), PARAMETER :: filename = "atest"    !File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=9), PARAMETER :: dsetname = "atestdset"        !Dataset name
     CHARACTER(LEN=11), PARAMETER :: aname = "attr_string"   !String Attribute name
     CHARACTER(LEN=14), PARAMETER :: aname2 = "attr_character"!Character Attribute name
     CHARACTER(LEN=11), PARAMETER :: aname3 = "attr_double"   !DOuble Attribute name
     CHARACTER(LEN=9), PARAMETER :: aname4 = "attr_real"      !Real Attribute name
     CHARACTER(LEN=12), PARAMETER :: aname5 = "attr_integer"  !Integer Attribute name
     CHARACTER(LEN=9), PARAMETER :: aname6 = "attr_null"     !Null Attribute name

     !
     !data space rank and dimensions
     !
     INTEGER, PARAMETER :: RANK = 2
     INTEGER, PARAMETER :: NX = 4
     INTEGER, PARAMETER :: NY = 5



     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier for dataset

     INTEGER(HID_T) :: attr_id        !String Attribute identifier
     INTEGER(HID_T) :: attr2_id       !Character Attribute identifier
     INTEGER(HID_T) :: attr3_id       !Double Attribute identifier
     INTEGER(HID_T) :: attr4_id       !Real Attribute identifier
     INTEGER(HID_T) :: attr5_id       !Integer Attribute identifier
     INTEGER(HID_T) :: attr6_id       !Null Attribute identifier
     INTEGER(HID_T) :: aspace_id      !String Attribute Dataspace identifier
     INTEGER(HID_T) :: aspace2_id     !Character Attribute Dataspace identifier
     INTEGER(HID_T) :: aspace6_id     !Null Attribute Dataspace identifier
     INTEGER(HID_T) :: dtype_id       !
     INTEGER(HID_T) :: atype_id       !String Attribute Datatype identifier
     INTEGER(HID_T) :: atype2_id      !Character Attribute Datatype identifier
     INTEGER(HID_T) :: atype3_id      !Double Attribute Datatype identifier
     INTEGER(HID_T) :: atype4_id      !Real Attribute Datatype identifier
     INTEGER(HID_T) :: atype5_id      !Integer Attribute Datatype identifier
     INTEGER(HSIZE_T), DIMENSION(1) :: adims = (/2/) ! Attribute dimension
     INTEGER(HSIZE_T), DIMENSION(1) :: adims2 = (/1/) ! Attribute dimension
     INTEGER     ::   arank = 1                      ! Attribure rank
     INTEGER(SIZE_T) :: attrlen    ! Length of the attribute string

     INTEGER(HID_T) :: attr_space     !Returned String Attribute Space identifier
     INTEGER(HID_T) :: attr2_space    !Returned other Attribute Space identifier
     INTEGER(HID_T) :: attr_type      !Returned Attribute Datatype identifier
     INTEGER(HID_T) :: attr2_type      !Returned CHARACTER Attribute Datatype identifier
     INTEGER(HID_T) :: attr3_type      !Returned DOUBLE Attribute Datatype identifier
     INTEGER(HID_T) :: attr4_type      !Returned REAL Attribute Datatype identifier
     INTEGER(HID_T) :: attr5_type      !Returned INTEGER Attribute Datatype identifier
     INTEGER(HID_T) :: attr6_type      !Returned NULL Attribute Datatype identifier
     INTEGER        :: num_attrs      !number of attributes
     INTEGER(HSIZE_T) :: attr_storage   ! attributes storage requirements .MSB.
     CHARACTER(LEN=256) :: attr_name    !buffer to put attr_name
     INTEGER(SIZE_T)    ::  name_size = 80 !attribute name length

     CHARACTER(LEN=35), DIMENSION(2) ::  attr_data  ! String attribute data
     CHARACTER(LEN=35), DIMENSION(2) ::  aread_data ! Buffer to put read back
                                               ! string attr data
     CHARACTER ::  attr_character_data = 'A'
     DOUBLE PRECISION,  DIMENSION(1) ::  attr_double_data = 3.459
     REAL,         DIMENSION(1) ::  attr_real_data = 4.0
     INTEGER,      DIMENSION(1) ::  attr_integer_data = 5


     CHARACTER :: aread_character_data ! variable to put read back Character attr data
     INTEGER, DIMENSION(1)  :: aread_integer_data ! variable to put read back integer attr data
     INTEGER, DIMENSION(1)  :: aread_null_data = 7 ! variable to put read back null attr data
     DOUBLE PRECISION, DIMENSION(1)   :: aread_double_data ! variable to put read back double attr data
     REAL, DIMENSION(1)  :: aread_real_data ! variable to put read back real attr data

     character(len=80)          :: fname, datapath,g1,g2,g3,s
     integer                    :: error, total_error, i ! Error flag
     INTEGER(HSIZE_T),allocatable :: data_dims(:)
     INTEGER(HSIZE_T)           :: npoints
     integer                    :: rnk, type_class
     integer(SIZE_T)            :: type_size
     integer(HID_T)             :: grp1_id, grp2_id, grp3_id
     real(kind=8),allocatable   :: buf_dbl1(:), buf_dbl2(:,:), buf_dbl3(:,:,:)
     real(kind=4),allocatable   :: buf_flt1(:)

     character(8)               :: scversion
     character(11)              :: dstring
     character(15)              :: tstring

call timestamp(datestring=dstring, timestring=tstring)

fname = 'Nidata.h5'

scversion = '3.x.x'

write (*,*) 'Date = '//dstring 
write (*,*) 'Time = '//tstring 

!
! Initialize FORTRAN interface.
!
CALL h5open_EMsoft(error)
write (*,*) 'Initialize Fortran interface error   = ',error


write (*,*) 'calling h5fopen_f to open file '//fname
     CALL h5fopen_f(fname, H5F_ACC_RDWR_F, file_id, error)
 
write (*,*) 'file_id = ',file_id
write (*,*) 'error   = ',error

! 
datapath = "/Scan 1/EBSD/Data/CI"

call h5ltget_dataset_ndims_f(file_id, datapath, rnk, error)
write (*,*) ' data dimensionality = ',rnk
allocate(data_dims(rnk))

call h5ltget_dataset_info_f(file_id, datapath, data_dims, type_class, type_size, error)
write(*,*) ' data set info : ',data_dims(1), type_class, type_size, error
allocate(buf_flt1(data_dims(1)))

call h5ltread_dataset_f(file_id, datapath, H5T_NATIVE_REAL, buf_flt1, data_dims, error)

do i=1,10
  write (*,*) i, buf_flt1(i)
end do


write (*,*) 'calling h5fclose_f to close file '
     CALL h5fclose_f(file_id, error)
write (*,*) 'error   = ',error

! now let's do a test an create a new file with this dataset written to it in a different location
fname = 'test.h5'
call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, error)

g1 = 'EMheader'
call h5gcreate_f(file_id,g1,grp1_id,error)
write (*,*) 'open group ',trim(g1),' ',error

! version number /EMheader/Version 'character'
call h5ltmake_dataset_string_f(grp1_id, 'Version', scversion, error)

! execution data /EMheader/Date 'character'
call h5ltmake_dataset_string_f(grp1_id, 'Date', dstring, error)

! start time /EMheader/StartTime 'character'
call h5ltmake_dataset_string_f(grp1_id, 'StartTime', tstring, error)

! stop time /EMheader/StopTime 'character'
call h5ltmake_dataset_string_f(grp1_id, 'StopTime', tstring, error)

call h5gclose_f(grp1_id,error)


g1 = 'level1'
g2 = 'level2'
g3 = 'CI'
s = '/'


call h5gcreate_f(file_id,g1,grp1_id,error)
write (*,*) 'open group ',trim(g1),' ',error

call h5gcreate_f(grp1_id,g2,grp2_id,error)
write (*,*) 'open group ',trim(g2),' ',error

datapath = trim(s)//trim(g1)//trim(s)//trim(g2)//trim(s)//trim(g3)
write (*,*) 'datapath = ',datapath

call h5ltmake_dataset_f(file_id, datapath, rnk, data_dims, H5T_NATIVE_REAL, buf_flt1, error)

call h5gclose_f(grp2_id,error)
call h5gclose_f(grp1_id,error)

call h5fclose_f(file_id,error)


!
! Close FORTRAN interface.
!
CALL h5close_EMsoft(error)
write (*,*) 'close fortran interface error   = ',error


end program hdftest


subroutine timestamp (stdout, timestring, datestring)

  IMPLICIT NONE
  
  integer(kind=irg),INTENT(IN),OPTIONAL    :: stdout
  character(len = 11),INTENT(OUT),OPTIONAL :: datestring
  character(len = 15),INTENT(OUT),OPTIONAL :: timestring

  integer(kind=4)      :: std
  character ( len = 8 )  :: ampm
  integer ( kind = 4 ) :: d
  character ( len = 8 )  :: date
  integer ( kind = 4 ) :: h
  integer ( kind = 4 ) :: mo
  integer ( kind = 4 ) :: mm
  character ( len = 3 ), parameter, dimension(12) :: month = (/ &
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
  integer ( kind = 4 ) :: n
  integer ( kind = 4 ) :: s
  character ( len = 10 ) :: time
  integer ( kind = 4 ) :: values(8)
  integer ( kind = 4 ) :: y
  character ( len = 5 )  :: zone

  std = 6
  if (PRESENT(stdout)) std=stdout

  call date_and_time ( date, time, zone, values )

  y = values(1)
  mo = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  if ((.not.PRESENT(datestring)).and.(.not.PRESENT(timestring))) then
    write ( std, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
      month(mo), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )
  else
    if (PRESENT(datestring)) write (datestring, '(a,1x,i2,1x,i4)' ) month(mo), d, y
    if (PRESENT(timestring)) then
      write (timestring, '(i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) h,':',n,':',s,'.',mm,trim(ampm)
    end if
  end if

end subroutine timestamp





