program EMIntegrateSTEM

use local
use typedefs
use NameListTypedefs
use NameListHandlers
use files
use io

IMPLICIT NONE

character(fnlen)                             :: nmldeffile, progname, progdesc
type(EMIntegrateSTEMNameListType)              :: isnml

nmldeffile = 'EMIntegrateSTEM.nml'
progname = 'EMIntegrateSTEM.f90'
progdesc = 'Integration of STEM images from a 4D array produced by EMdddSTEM or EMmdSTEM'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any 
! value probably needs to be changed here
call Interpret_Program_Arguments(nmldeffile,1,(/ 213 /), progname)


write (*,*) 'read program arguments '

! deal with the namelist stuff
call GetEMIntegrateSTEMnameList(nmldeffile,isnml)
write (*,*) 'read namelist file '

write (*,*) 'calling STEMintegrate routine '
! perform the zone axis computations
call STEMintegrate(isnml, progname, nmldeffile)

end program EMIntegrateSTEM




!--------------------------------------------------------------------------
!
! SUBROUTINE: STEMintegrate
!
!> @author Joseph Tessmer, Carnegie Mellon University
!
!> @brief Integrate STEM-DCI images from arrays produced by EMmdSTEM or EMdddSTEM
!
!> @date 08/30/2019  JT  1.0 original
!--------------------------------------------------------------------------
subroutine STEMintegrate(isnml, progname, nmldeffile)

use local
use typedefs
use NameListTypedefs
use crystal
use symmetry
use initializersHDF
use initializers
use constants
use gvectors
use kvectors
use error
use io
use files
use TIFF_F90
use diffraction
use omp_lib
use MBModule
use HDF5
use NameListHDFwriters
use HDFsupport
use rotations!, only:eu2om
use ISO_C_BINDING
use MDsubroutines
use math
use quaternions
use clsupport
use clfortran 
use Lambert

IMPLICIT NONE

!inputs
type(EMIntegrateSTEMNameListType),INTENT(INOUT)         :: isnml
character(fnlen),INTENT(IN)                           :: progname
character(fnlen),INTENT(IN)                           :: nmldeffile

! parameters
REAL(kind=sgl), PARAMETER                 :: Pi = 3.1415927

! H5 files
type(HDFobjectStackType)            :: HDF_head
!type(HDFobjectStackType),pointer    :: HDF_head

integer(kind=irg)                   :: hdferr
character(fnlen)                    :: dataset, groupname, inputfile
integer(HSIZE_T)                    :: IntensityArraySize(4), PixelLocationArraySize(2), hklArraySize(2)
integer(HSIZE_T)                    :: orientArraySize(1), basisArraySize(2), lauecArraySize(1), LParraysize(1)
real(kind=sgl),allocatable          :: intensities(:,:,:,:), pixelloc(:,:), orient(:), lauec(:), LParray(:)
integer(kind=irg),allocatable       :: hklarr(:,:), basis(:,:)
real(kind=sgl)                      :: lpabc(3)
logical                             :: readonly
integer(c_int32_t)                  ::  ierr

! looping variables 
integer(kind=irg)                   :: ii, jj, kk, ll, i, j, k, ix, jy

! detector + integration variables
integer(kind=irg)                   :: mode, reflection(3), ga(3), gb(3), nref, nsam, xpix, ypix, absHKLa(3), absHKLb(3), numbeams
real(kind=sgl)                      :: innerrad, outerrad, innerrad2, outerrad2, wavelength, om(3,3), discrad, theta
real(kind=sgl)                      :: convangle
integer(kind=irg)                   :: pixsize, camlen, npx
logical                             :: CBED, useBeam, ondetector
real(kind=sgl)                      :: x, y, z, xx, yy, zz
integer(kind=irg),allocatable       :: mask(:,:), refmask(:,:), pixinten(:,:)
integer(kind=irg)                   :: hklh, hklk, hkll, inpx, jnpx, inpx2, jnpx2, r, r2, rr
integer(kind=irg)                   :: indX, indY
real(kind=sgl)                      :: xyzlamb(3)
real(kind=sgl),allocatable          :: xyscaled(:,:)
integer(kind=irg)                   :: iro, oro

! Image variables
real(kind=sgl),allocatable          :: ImageArray(:,:)
real(kind=sgl),allocatable          :: CBEDimage(:,:)
real(kind=sgl)                      :: ma, mi
character(fnlen)                    :: fstring





! some user-set variables from the H5 file that were not determined at diffraction 
! runtime

inputfile     = isnml%inputfilename ! input filename
mode          = isnml%mode       ! single reflection or annular mode 
reflection    = isnml%ref        ! which reflection to use for refmode 
innerrad      = isnml%id         ! annular detector inner radius
outerrad      = isnml%od         ! annular detector outer radius
pixsize       = isnml%pixsize    ! pixel size of detector in nm
camlen        = isnml%camlen     ! camera length in nm
CBED          = isnml%CBED       ! generate a CBED pattern or not

! stuff
camlen = camlen * 1e7
iro = int(innerrad)
oro = int(outerrad)
innerrad = innerrad * 1e6 / pixsize ! calculation is done in units of pixels
outerrad = outerrad * 1e6 / pixsize ! calculation is done in units of pixels


! compute these now to save time
innerrad2 = innerrad * innerrad
outerrad2 = outerrad * outerrad

!nullify(HDF_head)
nullify(HDF_head%next)




! =================================================================
! Read data from diffraction simulation H5 output file
! =================================================================


print *, inputfile
! read in the 4D array that holds the beam intensities + other data for image integration
call Message('--> Opening h5 data file.')
call h5open_EMsoft(hdferr)

! open the diffraction file using the default properties.
readonly = .TRUE.
hdferr =  HDF_openFile(inputfile, HDF_head, readonly)


! Open the group containing the necessary info for image formation
groupname = 'EMData'
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = 'STEMDCI'
hdferr = HDF_openGroup(groupname, HDF_head)

! We need the intensity data, pixellocation, hkl, orientation, laue shift, laue basis vectors,
! electron wavelength, and STEM convergence angle
dataset = 'Intensities'
call HDF_readDatasetFloatArray4D(dataset, IntensityArraySize, HDF_head, hdferr, intensities)

dataset = 'PixelLocation'
call HDF_readDatasetFloatArray2D(dataset, PixelLocationArraySize, HDF_head, hdferr, pixelloc)
 
dataset = 'hkl'
call HDF_readDatasetIntegerArray2D(dataset, hklArraySize, HDF_head, hdferr, hklarr)

dataset = 'orientation'
call HDF_readDatasetFloatArray1D(dataset,orientArraySize, HDF_head, hdferr, orient) 

dataset = 'lauec'
call HDF_readDatasetFloatArray1D(dataset, lauecArraySize, HDF_head, hdferr, lauec) 

dataset = 'ga'
call HDF_readDatasetIntegerArray2D(dataset, basisArraySize, HDF_head, hdferr, basis)

dataset = 'wavelength'
call HDF_readDatasetFloat(dataset, HDF_head, hdferr, wavelength)

dataset = 'convergenceangle'
call HDF_readDatasetFloat(dataset, HDF_head, hdferr, convangle)

! go up 2 levels
call HDF_pop(HDF_head)
call HDF_pop(HDF_head)

! Open the CrystalData group

call Message('--> Changing group.')

groupname = 'CrystalData'
hdferr = HDF_openGroup(groupname, HDF_head)

dataset = 'LatticeParameters'
call HDF_readDatasetFloatArray1D(dataset,LParraySize, HDF_head, hdferr, LParray) 

lpabc(1) = LParray(1)
lpabc(2) = LParray(2)
lpabc(3) = LParray(3)

! close the datafile
call HDF_pop(HDF_head,.TRUE.)

! close the Fortran interface
call h5close_EMsoft(hdferr)

call Message('--> Read data from HDF5 file and closed file.')

! =================================================================
! Done reading data from H5
! =================================================================

! some variables that will be used in the computation

xpix  = IntensityArraySize(1) ! number of reflections in the diffraction calc
ypix  = IntensityArraySize(2) ! number of incident beams in the cone
nsam  = IntensityArraySize(3) ! number of x pixels in the image space
nref  = IntensityArraySize(4) ! number of y pixels in the image space
ga(:) = basis(1,:)           ! 1st laue basis vector
gb(:) = basis(2,:)           ! 2nd laue basis vector 


discrad = nint((tan(convangle) * camlen)/pixsize)
npx     = 2 * nint((outerrad+2*discrad)/2)

! select operation mode:
! 1 = single reflection
! 2 = all reflections of given type 
! 3 = annular detector 


! get the orientation matrix for this xtal

om = qu2om(orient)

! allocate the reflection-space mask
allocate(refmask(nsam,nref))
allocate(ImageArray(xpix,ypix))
if (cbed) then
    allocate(CBEDimage(npx,npx))
end if

refmask = 0


select case(mode)
    case(1) ! single reflection
        call Message('Operating in single reflection mode.')
        print *, "Reflection: ", reflection

        ! just integrate the beams that have correct hkl:

        do jj = 1, nref

            useBeam = (hklarr(1,jj).eq.reflection(1)) .and. (hklarr(2,jj) .eq. reflection(2))&
             .and. (hklarr(3,jj) .eq. reflection(3))

            if (useBeam) then
                do kk = 1, nsam
                    refmask(kk,jj) = 1
                end do 
            end if 
        end do 

        call Message('Mask generated, building image.')


        ! loop over each reflection & beam to integrate:
        do ii = 1, nsam
            do jj = 1, nref
                if (refmask(ii,jj) > 0) then
                    ! we need to add the value of this beam to every pixel 
                    ImageArray(:,:) = ImageArray(:,:) + intensities(:,:,ii,jj)
                end if 
            end do 
        end do

        ! output normal image & cbed image

        TIFF_nx = xpix
        TIFF_ny = ypix

        ! allocate memory for image
        allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))


        ! fill the image with whatever data you have (between 0 and 255)
        ! write (fstring,"(I2,I2,I2)") int(reflection(1)), int(reflection(2)), int(reflection(3))
        ! TIFF_filename = trim(fstring)//".tiff"
        TIFF_filename = "output.tiff"
        ! get the min and max value for the current image and rescale the intensities between 0 and 255
        mi = minval(ImageArray(:,:))
        ma = maxval(ImageArray(:,:))
        do ix=0,TIFF_nx-1
          do jy=0,TIFF_ny-1
            TIFF_image(ix,jy) = int(255.0*(ImageArray(ix+1,jy+1)-mi)/(ma-mi))
          end do
        end do
        ! create the file
        call TIFF_Write_File





    ! case(2) ! all reflections of a single type 
    !     call Message('Operating in reflection type mode.')
    !     print *, "Reflection type: ", reflection

    !     ! no mask, just integrate only the beams with the correct hkl type


    !     do jj = 0, nref

    !         absHKLa = abs(hkl(jj,:))
    !         absHKLb = abs(reflection)



    !         useBeam = hkl(jj,1) == reflection(1) .and. hkl(jj,2) == reflection(2) .and. hkl(jj,3) == reflection(3)

    !         if (useBeam) then
    !             do kk = 0, nsam
    !                 refmask(kk,jj) = 1
    !             end do 
    !         end if 
    !     end do 

    !     ! loop over each reflection & beam to integrate:
    !     do ii = 0, nsam
    !         do jj = 0, nref
    !             if (refmask(ii,jj) > 0) then
    !                 ! we need to add the value of this beam to every pixel 
    !                 ImageArray(:,:) = ImageArray(:,:) + intensities(:,:,ii,jj)
    !             end if 
    !         end do 
    !     end do

    !     ! output normal image & cbed image

    !     TIFF_nx = xpix
    !     TIFF_ny = ypix

    !     ! allocate memory for image
    !     allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))

    !     ! fill the image with whatever data you have (between 0 and 255)
    !     write (fstring,"(I2,I2,I2)") hklarray(1,i), hklarray(2,i), hklarray(3,i)
    !     TIFF_filename = trim(fstring)//".tiff"
    !     ! get the min and max value for the current image and rescale the intensities between 0 and 255
    !     mi = minval(ImageArray(:,:))
    !     ma = maxval(ImageArray(:,:))
    !     do ix=0,TIFF_nx-1
    !       do jy=0,TIFF_ny-1
    !         TIFF_image(ix,jy) = int(255.0*(ImageArray(ix+1,jy+1)-mi)/(ma-mi))
    !       end do
    !     end do
    !     ! create the file
    !     call TIFF_Write_File


    case(2) ! annular detector 
        ! generate the detector mask:
        allocate(mask(npx,npx))
        do jj = 1, npx
            jnpx = jj-npx/2 
            jnpx2 = jnpx * jnpx
            do ii = 1, npx
                inpx = ii-npx/2
                inpx2 = inpx * inpx
                r2 = jnpx2 + inpx2
                if (r2 <= outerrad2 .and. r2 >= innerrad2) then
                    mask(ii,jj) = 1
                end if              
            end do 
        end do 

        ! get the xyscaled positions of the beams in a disc:
        allocate(xyscaled(nsam,2))

        do ii = 1, nsam
            xyzlamb = LambertSquareToSphere((/ pixelloc(ii,1), pixelloc(ii,2) /),ierr)
            xyscaled(ii,1) = xyzlamb(1)/(discrad*2)
            xyscaled(ii,2) = xyzlamb(2)/(discrad*2)
        end do 



        ! to figure out if a beam should be integrated we need to see if it falls on the detector
        do ii = 1, nref 
            hklh = hklarr(1,ii)
            hklk = hklarr(2,ii)
            hkll = hklarr(3,ii)

            x = hklh*om(1,1) + hklk*om(1,2) + hkll*om(1,3);
            y = hklh*om(2,1) + hklk*om(2,2) + hkll*om(2,3);
            z = hklh*om(3,1) + hklk*om(3,2) + hkll*om(3,3);

            theta = atan2(y,x)
            if (theta < 0) then
                theta = theta + 2 * pi
            end if 

            if (x == 0 .and. y == 0 .and. z == 0) then 
                xx = npx/2
                yy = npx/2
            else 
                x = x/lpabc(1)
                y = y/lpabc(2)
                z = z/lpabc(3)

                rr = wavelength * camlen * sqrt((x*x)+ (y*y))/pixsize

                ! position of the disc center for this reflection
                xx = nint((rr*cos(theta))+npx/2)
                yy = nint((rr*sin(theta))+npx/2)
            end if  

            ondetector = xx < npx .and. yy < npx .and. xx > 0 .and. yy > 0
            if (ondetector) then
                do ll = 1, nsam 
                    if (xx > discrad .and. xx < (npx-discrad) .and. yy > discrad .and. yy < (npx-discrad)) then 
                        ! add on the shifts for each cone
                        indY = yy + xyscaled(ll,1)
                        indX = xx + xyscaled(ll,2)

                        ! generate CBED pattern
                        if (CBED) then
                            CBEDimage(indX,indY) = intensities(xpix/2,ypix/2,ll,kk)
                        end if 

                        ! generate the reflection mask
                        if (mask(indX,indY) > 0) then 
                            refmask(ll,ii) = 1
                        end if 
                    end if 
                end do 
            end if 
        end do 

        numbeams = sum(refmask)

        call Message('Mask generated, building image.')
        print *, 'Number of beams:', numbeams


        ! now we have a mask in reflection space, need to iterate over every reflection/beam
        do ii = 1, nsam
            do jj = 1, nref
                if (refmask(ii,jj) > 0) then
                    ! we need to add the value of this beam to every pixel 
                    ImageArray(:,:) = ImageArray(:,:) + intensities(:,:,ii,jj)
                end if 
            end do 
        end do

        ! output normal image & cbed image

        TIFF_nx = xpix
        TIFF_ny = ypix

        ! allocate memory for image
        allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))

        ! fill the image with whatever data you have (between 0 and 255)
        write (fstring,"(I2,I2,I2)") iro, oro, int(camlen/1e7)
        TIFF_filename = trim(fstring)//".tiff"
        ! get the min and max value for the current image and rescale the intensities between 0 and 255
        mi = minval(ImageArray(:,:))
        ma = maxval(ImageArray(:,:))
        do ix=0,TIFF_nx-1
          do jy=0,TIFF_ny-1
            TIFF_image(ix,jy) = int(255.0*(ImageArray(ix+1,jy+1)-mi)/(ma-mi))
          end do
        end do
        ! create the file
        call TIFF_Write_File

        if (CBED) then
            ! generate a cbed pattern
            deallocate(TIFF_image)

            TIFF_nx = npx
            TIFF_ny = npx

            ! allocate memory for image
            allocate(TIFF_image(0:TIFF_nx-1,0:TIFF_ny-1))

            ! fill the image with whatever data you have (between 0 and 255)
            TIFF_filename = "CBED.tiff"
            ! get the min and max value for the current image and rescale the intensities between 0 and 255
            mi = minval(CBEDimage(:,:))
            ma = maxval(CBEDimage(:,:))
            do ix=0,TIFF_nx-1
              do jy=0,TIFF_ny-1
                TIFF_image(ix,jy) = int(255.0*(CBEDimage(ix+1,jy+1)-mi)/(ma-mi))
              end do
            end do
            ! create the file
            call TIFF_Write_File
        end if 

    case default ! you didn't provide a valid case
        ! call Message('Mode value must be 1: single reflection, 2: single type, or 3: annular detector.')
        call Message('Mode value must be 1: single reflection or 2: annular detector.')

        stop 'Program halted.'
end select




end subroutine STEMintegrate