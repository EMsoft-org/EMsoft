! ###################################################################
! Copyright (c) 2015-2019, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

!--------------------------------------------------------------------------
! EMsoft:EMPEDkin.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMPEDkin 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Kinematical precession electron diffraction dictionary creation
!
!> @date 03/01/15 MDG 1.0 original
!> @date 03/03/15 MDG 1.1 first tests with realistic parameters; reasonable results
!> @date 11/14/15 MDG 1.2 minor name change; verification; HDF5 output
!> @date 05/21/16 MDG 1.3 change for HDF internal file reorganization
!> @date 08/15/19 MDG 1.4 program moved into public repository
!--------------------------------------------------------------------------
program EMPEDkin

use local
use NameListTypedefs
use NameListHandlers
use files
use io
use stringconstants

IMPLICIT NONE

character(fnlen)                  :: nmldeffile, progname, progdesc
type(PEDkinNameListType)          :: pednl

nmldeffile = 'EMPEDkin.nml'
progname = 'EMPEDkin.f90'
progdesc = 'Kinematical Precession Electron Diffraction Dictionary Generation'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(nmldeffile,1,(/ 15 /), progname)

! deal with the namelist stuff
call GetPEDkinNameList(nmldeffile,pednl)

! generate a set of kinematical PED patterns
 call PEDkin_dictionary(pednl,progname, nmldeffile)

end program EMPEDkin

!--------------------------------------------------------------------------
!
! SUBROUTINE:PEDkin_dictionary
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief compute a kinematical PED dictionary
!
!> @param nmlfile namelist file name
!
!> @date 03/02/15 MDG 1.0 original
!> @date 03/06/15 MDG 1.1 testing complete; added EulerAngles.txt output
!> @date 11/14/15 MDG 1.2 added HDF5 support; write patterns using hyperslabbing
!--------------------------------------------------------------------------
subroutine PEDkin_dictionary(pednl,progname, nmldeffile)

use local
use typedefs
use dictmod
use crystal
use initializersHDF
use initializers
use gvectors
use io
use diffraction
use symmetry
use quaternions
use NameListTypedefs
use constants
use rotations
use so3
use math
use HDF5
use NameListHDFwriters
use HDFsupport
use stringconstants
use error

IMPLICIT NONE

type(PEDkinNameListType),INTENT(INOUT)  :: pednl
character(fnlen),INTENT(IN)             :: progname
character(fnlen),INTENT(IN)             :: nmldeffile

integer(kind=irg)               :: FZcnt, pgnum, ival, i, j, l
type(FZpointd),pointer          :: FZlist, FZtmp, FZtmp2
real(kind=sgl)                  :: la, dval, dmin, glen, gmax, io_real(3), om(3,3), k(3), sgmax, FN(3), xgmin, Ig, Igmax, & 
                                   maxint, w, ku(3), kp(3), rnmpp, dx, dy, eu(3), tstart, tstop, x, y, ma, mi
integer(kind=irg)               :: gp(3), imh, imk, iml, nref, gg(3), ix, iy, iz, io_int(5), ww, nsize, tdp, sx, sy, hdferr, &
                                   ninbatch, nbatches, nremainder,ibatch,istat, gridtype 
integer(HSIZE_T)                :: dim0, dim1, dim2, hdims(3), offset(3)
logical                         :: verbose, insert=.TRUE., overwrite=.TRUE., exists
character(fnlen)                :: groupname, dataset, outname
character(11)                   :: dstr
character(15)                   :: tstrb
character(15)                   :: tstre
character(fnlen, KIND=c_char),allocatable,TARGET :: stringarray(:)
character(fnlen,kind=c_char)                     :: line2(1)
character(fnlen)                :: fname, atype

real(kind=sgl),allocatable      :: pedpattern(:,:), xx(:,:), yy(:,:), line(:), dot(:,:), eulerarray(:,:), sglread(:)
character(len=1),allocatable    :: pedp(:,:,:), pedpat(:,:)
integer(kind=irg),allocatable   :: patinbatch(:)
integer(kind=irg)               :: totnumberbatch, nextra
type(unitcell)                  :: cell
type(DynType),save              :: Dyn
type(gnode),save                :: rlp
type(reflisttype),pointer       :: reflist, nexts, rltmpa

type(HDFobjectStackType),pointer  :: HDF_head


sgmax = 0.50

call timestamp(datestring=dstr, timestring=tstrb)
call cpu_time(tstart)

!=============================================
!=============================================
! crystallography section
!nullify(cell)        
allocate(cell)

verbose = .TRUE.

call Initialize_Cell(cell,Dyn,rlp,pednl%xtalname, pednl%dmin, pednl%voltage, verbose)

! determine the point group number
j=0
do i=1,32
 if (SGPG(i).le.cell % SYM_SGnum) j=i
end do
pgnum = j
io_int(1) = pgnum
call WriteValue(' Point group number                       : ',io_int, 1, "(I3)")

!=============================================
!=============================================
! generation of all potential reflections inside a reciprocal space sphere
! computed from the camera length and the detector size ...

! first set the maximum |g| value that can possibly give rise to a diffracted beam on the detector (diagonal)
  gmax = sqrt(2.0) * float(pednl%npix) * pednl%rnmpp
  io_real(1) = gmax
  call WriteValue(' Length of longest g-vector               : ', io_real, 1, "(F8.4)")

! this code is taken from the Initialize_ReflectionList routine, but we do not
! need everything from that routine; first get the size of the lookup table
  gp = shape(cell%LUT)
  imh = (gp(1)-1)/4
  imk = (gp(2)-1)/4
  iml = (gp(3)-1)/4

! initialize the reflection list
  nullify(reflist)
  nullify(rltmpa)
  nref = 0
 
! transmitted beam always has excitation error zero
  gg = (/ 0,0,0 /)
  call AddReflection(rltmpa, reflist, cell, nref, gg)   ! this guarantees that 000 is always the first reflection
  rltmpa%xg = 0.0
  xgmin = 100000.0
  Igmax = 0.0

! now compute |U_g|^2 for all allowed reflections; 
ixl: do ix=-imh,imh
iyl:  do iy=-imk,imk
izl:   do iz=-iml,iml
        if ((abs(ix)+abs(iy)+abs(iz)).ne.0) then  ! avoid double counting the origin
         gg = (/ ix, iy, iz /)
         glen = CalcLength(cell, float(gg), 'r' )

! find all reflections, ignoring double diffraction spots
         if ((IsGAllowed(cell,gg)).and.(glen.le.gmax)) then ! allowed by the lattice centering, if any
            call AddReflection(rltmpa, reflist, cell, nref, gg )
! we'll use the sangle field of the rltail structure to store |Ug|^2; we will also need the extinction distance
            rltmpa%sangle = cdabs(cell%LUT(ix, iy, iz))**2
            if (rltmpa%sangle.gt.Igmax) Igmax = rltmpa%sangle
            rltmpa%xg = 1.0/(cdabs(cell%LUT(ix,iy,iz))*cell%mLambda)
            if (rltmpa%xg.lt.xgmin) xgmin = rltmpa%xg
         end if ! IsGAllowed
        end if
       end do izl
      end do iyl
    end do ixl
    
io_int(1) = nref
call WriteValue(' Length of the master list of reflections : ', io_int, 1, "(I8)")


!=============================================
!=============================================
! create the coordinate arrays for the Gaussian peaks that will represent the diffraction spots
rnmpp = 1.0/pednl%rnmpp
ww = 6
tdp = 2*ww+1
allocate(xx(-ww:ww,-ww:ww), yy(-ww:ww,-ww:ww), line(-ww:ww), dot(-ww:ww,-ww:ww))
line = (/ (float(i),i=-ww,ww) /) * rnmpp
xx = spread(line,dim=1,ncopies=2*ww+1)
yy = transpose(xx)


!=============================================
!=============================================
! create the output array
nsize = pednl%npix/2 + ww 
allocate(pedpattern(-nsize:nsize,-nsize:nsize))
maxint = Igmax 
write (*,*) 'Maximum diffracted intensity : ',maxint

!=============================================
!=============================================
! open the HDF5 output file; in the old code, the output was written into two separate binary files
! here, we do just one output file in the standard EMsoft style
  nullify(HDF_head)

! Initialize FORTRAN interface.
  call h5open_EMsoft(hdferr)

! Create a new file using the default properties.
  outname = trim(EMsoft_getEMdatapathname())//trim(pednl%outname)
  outname = EMsoft_toNativePath(outname)
  hdferr =  HDF_createFile(outname, HDF_head)

! write the EMheader to the file
  groupname = SC_PEDkin
  call HDF_writeEMheader(HDF_head, dstr, tstrb, tstre, progname, groupname)

! create a namelist group to write all the namelist files into
  groupname = SC_NMLfiles
  hdferr = HDF_createGroup(groupname, HDF_head)

! read the text file and write the array to the file
  dataset = SC_PEDkinNameList
  hdferr = HDF_writeDatasetTextFile(dataset, nmldeffile, HDF_head)

! leave this group
  call HDF_pop(HDF_head)
  
! create a namelist group to write all the namelist files into
  groupname = SC_NMLparameters
  hdferr = HDF_createGroup(groupname, HDF_head)
  call HDFwritePEDkinNameList(HDF_head, pednl)

! leave this group
  call HDF_pop(HDF_head)

! then the remainder of the data in a EMData group
  groupname = SC_EMData
  hdferr = HDF_createGroup(groupname, HDF_head)

!=============================================
!=============================================
! rotation sampling section
! we need to get a sampling of orientation space, starting from the 
! cubochoric representation or read from file depending on input

if(pednl%sampling .eq. 'dict') then
    nullify(FZlist)
    FZcnt = 0
    gridtype = 0
    call sampleRFZ(pednl%ncubochoric, pgnum, gridtype, FZcnt, FZlist)

! convert the Rodrigues vectors to Euler angle triplets and delete the entries
    allocate(eulerarray(3,FZcnt))
    FZtmp => FZlist                        ! point to the top of the list
    do i=1,FZcnt
        eulerarray(1:3,i) = ro2eu(FZtmp%rod)
        FZtmp => FZtmp%next                  ! point to the next entry
    end do
! delete list
    FZtmp => FZlist%next
    FZtmp2 => FZlist
    do
        deallocate(FZtmp2)  
        if (.not. associated(FZtmp) ) EXIT
        FZtmp2 => FZtmp
        FZtmp => FZtmp%next
    end do
    nullify(FZlist)
else if(pednl%sampling .eq. 'file') then
    fname = trim(EMsoft_getEMdatapathname())//trim(pednl%eulerfile)
    inquire(file=fname,exist=exists)
    if(.not.exists) call FatalError('PEDkin:','eulerfile not found : '//trim(fname))
    open(unit=dataunit,file=fname,action='read',status='unknown')

    read(dataunit,*) atype
    read(dataunit,*)FZcnt
    allocate(eulerarray(3,FZcnt))

    if(atype .eq. 'eu') then
        allocate(sglread(3))
        do i = 1,FZcnt
            read(dataunit,*) sglread
            eulerarray(1:3,i) = sglread*cPi/180.0
        end do
    else if(atype .eq. 'qu') then
        allocate(sglread(4))
        do i = 1,FZcnt
            read(dataunit,*) sglread
            eulerarray(1:3,i) = qu2eu(sglread)
        end do
    else
        call FatalError('PEDkin:','Unknown orientation format in eulerfile :'//trim(fname)//&
             '. (only euler angles or quaternions allowed)')
    end if
else
    call FatalError('PEDkin:','unknown sampling type in namelist file :'//trim(nmldeffile))
end if

call Message(' Done creating Euler angle array.')

io_int(1) = FZcnt
call WriteValue(' Number of orientation in dictionary       : ', io_int, 1, "(I8)")

! Euler angle array size
dataset = SC_FZcnt
hdferr = HDF_writeDatasetInteger(dataset, FZcnt, HDF_head)

! and write them to the HDF5 file
dataset = SC_EulerAngles
hdferr = HDF_writeDatasetFloatArray2D(dataset, eulerarray*180.0/sngl(cPi), 3, FZcnt, HDF_head)

! keep the datafile open for writing; we'll collect 1024 patterns in an array,
! and then use hyperslab writing to put them into the HDF5 file.

!=============================================
!=============================================
! and loop over all orientations

! allocate the hyperslab array that will hold the batches of PED patterns 
ninbatch = 1024
nbatches = floor(float(FZcnt)/float(ninbatch))
nremainder = mod(FZcnt,ninbatch)

nextra = 0
if(nremainder .gt. 0) nextra = 1
totnumberbatch = nbatches + nextra

allocate(patinbatch(totnumberbatch))
patinbatch = ninbatch
if(nextra .eq. 1) patinbatch(totnumberbatch) = nremainder
allocate(pedpat(-nsize:nsize,-nsize:nsize),stat=istat)

call Message(' Starting main computation loop.')
call Message(' ')
batchloop: do i = 1, totnumberbatch       ! loop over all batches in the eulerarray
! allocate and set the individual output pattern to zero
    if(allocated(pedp)) deallocate(pedp)
    allocate(pedp(pednl%npix,pednl%npix,patinbatch(i)),stat=istat)
    pedp = ' '
    pedpattern = 0.0
    pedpat = ' '
    orientationloop: do l = 1,patinbatch(i)
        pedpattern = 0.0
        pedpat = ' '
        ival = (i-1)*patinbatch(i) + l

! convert the rodrigues vector to a passive rotation matrix.
        om = eu2om(eulerarray(1:3,ival))

! multiplication with (0,0,1) produces the normalized beam direction in a
! cartesian reference frame; so now we can compute the excitation errors 
! for every reflection and keep only the ones that are sufficiently small
        k = (/ 0.0, 0.0, 1.0 /)
        ku = matmul(om,k)
        FN = ku
        k = ku/sngl(cell%mLambda)

! first we go through the entire reflection list and compute the excitation errors
! those points that satisfy the cutoff are linked via the nexts pointers
        rltmpa => reflist%next
        nexts => rltmpa
        do j=1,nref
          gg = rltmpa%hkl
          rltmpa%sg = Calcsg(cell,float(gg),k,FN)
! should we consider this point any further ? If so, add it to the strong reflection linked list
          if (abs(rltmpa%sg).le.sgmax) then 
            nexts%nexts => rltmpa
            nexts => rltmpa
          end if
          rltmpa => rltmpa%next
        end do

! then, for each point in the nexts list, we compute the components of k' = k+g+s
! and place them in the proper reference frame; we skip the incident beam since it is 
! meaningless in the kinematical approximation
      nexts => reflist%next%nexts
      do 
! determine the vector k'
        kp = k + float(nexts%hkl) + nexts%sg*ku
        kp = matmul(transpose(om),kp)

! get the intensity for each point
        w = sngl(cPi)*nexts%sg*pednl%thickness
        if (abs(w).lt.1.0e-6) then
          Ig = nexts%sangle  ! * (sngl(cPi)*pednl%thickness/nexts%xg)**2
        else 
          Ig = nexts%sangle * (sin(w)/w)**2 ! * (sngl(cPi)*pednl%thickness/nexts%xg)**2
        end if

! determine the spot coordinates on the detector
        x = rnmpp * kp(1)
        y = rnmpp * kp(2)

! and plot that spot as a small Gaussian in the pedpattern array, assuming it falls on the detector.
        if ((abs(x).le.nsize-ww).and.(abs(y).le.nsize-ww)) then
          sx = nint(x)
          sy = nint(y)
          dx = x-sx
          dy = y-sy
          dot = (Ig/Igmax)**0.2 * exp(-((xx-dx)**2+(yy-dy)**2)*0.0025)
          pedpattern(sx-ww:sx+ww,sy-ww:sy+ww) = pedpattern(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
        end if

! and repeat this until the end of the list
        if (.not. associated(nexts%nexts)) EXIT
        nexts => nexts%nexts
      end do

      ma = maxval(pedpattern)
      mi = minval(pedpattern)
      pedpattern = ((pedpattern - mi)/ (ma-mi))
      pedpat = char(nint(255.0*pedpattern))

! save the pedpattern to the pedp array
      pedp(1:pednl%npix,1:pednl%npix,l) = pedpat(-nsize+ww+1:nsize-ww,-nsize+ww+1:nsize-ww)

! reset the nexts linked list and start over
      nexts => reflist%next
      rltmpa => nexts%nexts
      do 
        nullify(nexts%nexts)
        if (.not. associated(rltmpa%nexts)) EXIT
        nexts => rltmpa
        rltmpa => rltmpa%nexts
      end do
    end do orientationloop

    offset = (/ 0, 0, (i-1)*patinbatch(i) /)
    hdims = (/ pednl%npix, pednl%npix, FZcnt /)
    dim0 = pednl%npix
    dim1 = pednl%npix
    dim2 = patinbatch(i)

    if (i.eq.1) then
        dataset = SC_PEDpatterns
        hdferr = HDF_writeHyperslabCharArray3D(dataset, pedp, hdims, offset, dim0, dim1, dim2, &
                 HDF_head)
    else
        dataset = SC_PEDpatterns
        hdferr = HDF_writeHyperslabCharArray3D(dataset, pedp, hdims, offset, dim0, dim1, dim2, &
                 HDF_head, insert)
    end if

    io_real(1) = float(sum(patinbatch(1:i)))*100/float(sum(patinbatch))
    call WriteValue(' Completed ',io_real,1,'(F10.2," % ")')

end do batchloop

call HDF_pop(HDF_head)

! and update the end time
call timestamp(datestring=dstr, timestring=tstre)
groupname = SC_EMheader
hdferr = HDF_openGroup(groupname, HDF_head)

groupname = SC_PEDkin
hdferr = HDF_openGroup(groupname, HDF_head)

! stop time /EMheader/StopTime 'character'
dataset = SC_StopTime
line2(1) = dstr//', '//tstre
hdferr = HDF_writeDatasetStringArray(dataset, line2, 1, HDF_head, overwrite)

call CPU_TIME(tstop)
dataset = SC_Duration
tstop = tstop - tstart
hdferr = HDF_writeDatasetFloat(dataset, tstop, HDF_head)

! close the datafile
call HDF_pop(HDF_head,.TRUE.)

! close the Fortran interface
call h5close_EMsoft(hdferr)





end subroutine PEDkin_dictionary



