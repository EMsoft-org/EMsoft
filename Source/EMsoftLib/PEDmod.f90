! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:PEDmod.f90
!--------------------------------------------------------------------------
!
! MODULE: PEDmod
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief all things related to precession electron diffraction
!
!> @date 04/18/16 SS 1.0 original
!--------------------------------------------------------------------------
module PEDmod

use local
use typedefs

contains

!--------------------------------------------------------------------------
!
! SUBROUTINE:CalcKINPEDPatternSingle
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief calculate single PED pattern; used in the EMPEDDI program
!
!> @param pednl PED name list structure
!> @param cell cell structure
!> @param reflist list of possible reflections
!> @param nref number of reflections in master list
!> @param qu orientation of xtal
!> @param pedpattern calculated kinematic ped pattern
!
!> @date 06/24/14  SS 1.0 original
!> @date 04/28/16  SS 1.1 corrected array bound error
!--------------------------------------------------------------------------
recursive subroutine CalcKINPEDPatternSingle(pednl, cell, reflistarray, sanglearray, nref, qu, pedpattern)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcKINPEDPatternSingle

use NameListTypedefs
use typedefs
use quaternions
use constants
use diffraction

IMPLICIT NONE

type(PEDKINIndxListType), INTENT(IN)         :: pednl
type(unitcell)                               :: cell
integer(kind=irg), INTENT(IN)                :: nref
integer(kind=irg),INTENT(IN)                 :: reflistarray(3,nref)
real(kind=sgl),INTENT(IN)                    :: sanglearray(nref)

!type(reflisttype), pointer                   :: reflist, rltmpa, nexts
real(kind=sgl), INTENT(IN)                   :: qu(4)
real(kind=sgl), INTENT(OUT)                  :: pedpattern(1:pednl%npix, 1:pednl%npix)

integer(kind=irg)                            :: io_int(6), i, j, sx, sy, ww, tdp, nsize, gg(3), nns, counter
real(kind=sgl)                               :: sgmax, k(3), ku(3), FN(3), kp(3), w, x, y
real(kind=sgl)                               :: dx, dy, rnmpp, Ig, Igmax
real(kind=sgl), allocatable                  :: xx(:,:), yy(:,:), line(:), dot(:,:), image(:,:)
real(kind=sgl),allocatable                   :: sgarray(:),sanglearraystrong(:),sgarraystrong(:)
integer(kind=irg),allocatable                :: reflistarraystrong(:,:)

sgmax = pednl%sgmax
Igmax = pednl%Igmax
rnmpp = 1.0/pednl%rnmpp
ww = pednl%ww
tdp = 2*ww + 1
nsize = pednl%npix/2 +ww

allocate(image(-nsize:nsize,-nsize:nsize),xx(-ww:ww,-ww:ww), yy(-ww:ww,-ww:ww), line(-ww:ww), dot(-ww:ww,-ww:ww))

line = (/ (float(i),i=-ww,ww) /) * rnmpp
xx = spread(line,dim=1,ncopies=2*ww+1)
yy = transpose(xx)
image = 0.0

! multiplication with (0,0,1) produces the normalized beam direction in a
! cartesian reference frame; so now we can compute the excitation errors
! for every reflection and keep only the ones that are sufficiently small

k = (/ 0.0, 0.0, 1.0 /)
ku = quat_LP(qu,k)
FN = ku
k = ku/sngl(cell%mLambda)

! first we go through the entire reflection list and compute the excitation errors
! those points that satisfy the cutoff are linked via the nexts pointers

allocate(sgarray(nref))
sgarray = 0.0

! counter for number of strong beams
nns = 0

do j=1,nref
    gg = reflistarray(1:3,j)
    sgarray(j) = Calcsg(cell,float(gg),k,FN)
! should we consider this point any further ? If so, increase strong reflection list counter
    if (abs(sgarray(j)).le.sgmax) then
        nns = nns + 1
    end if
end do

allocate(sgarraystrong(nns),sanglearraystrong(nns),reflistarraystrong(3,nns))
counter = 1

do j = 1,nref
    if (abs(sgarray(j)) .le. sgmax) then
        reflistarraystrong(1:3,counter) = reflistarray(1:3,j)
        sgarraystrong(counter) = sgarray(j)
        sanglearraystrong(counter) = sanglearray(j)
        counter = counter + 1
    end if
end do

! then, for each point in the nexts list, we compute the components of k' = k+g+s
! and place them in the proper reference frame; we skip the incident beam since it is
! meaningless in the kinematical approximation

! strat from 2 to exclude the transmitted beam
do j = 2,nns
! determine the vector k'

    kp = k + reflistarraystrong(1:3,j) + sgarraystrong(j)*ku
    kp = quat_LP(conjg(qu),kp)

! get the intensity for each point

    w = sngl(cPi)*sgarraystrong(j)*pednl%thickness
    if (abs(w).lt.1.0e-6) then
        Ig = sanglearraystrong(j)  ! * (sngl(cPi)*pednl%thickness/nexts%xg)**2
    else
        Ig = sanglearraystrong(j) * (sin(w)/w)**2 ! * (sngl(cPi)*pednl%thickness/nexts%xg)**2
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
        dot = (Ig/Igmax)**0.2 * exp(-((xx-dx)**2+(yy-dy)**2)*0.0022)
        image(sx-ww:sx+ww,sy-ww:sy+ww) = image(sx-ww:sx+ww,sy-ww:sy+ww) + dot(-ww:ww,-ww:ww)
    end if

end do

pedpattern(1:pednl%npix,1:pednl%npix) = image(-nsize+ww:nsize-ww-1,-nsize+ww:nsize-ww-1)

deallocate(image, xx, yy, line, dot)
deallocate(sgarray,sgarraystrong,sanglearraystrong,reflistarraystrong)

end subroutine CalcKINPEDPatternSingle

!--------------------------------------------------------------------------
!
! SUBROUTINE:Denoise_PED
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Denoise a kinematical PED pattern
!
!> @param pednl namelist file for PED
!
!> @date 11/23/15  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Denoise_PED(pednl, img_in, img_out)
!DEC$ ATTRIBUTES DLLEXPORT :: Denoise_PED

use local
use ISO_C_BINDING
use NameListTypedefs

interface
    type(C_PTR) recursive function denoise (lx, ly, pf, powp, sigma_w, prinf, noisinf, &
    icd, mbike) bind(C, name = 'denoise')

use ISO_C_BINDING

IMPLICIT NONE
 
        integer(C_INT)                      :: lx
        integer(C_INT)                      :: ly
        real(C_FLOAT)                       :: pf
        real(C_FLOAT)                       :: powp
        real(C_FLOAT)                       :: sigma_w
        real(C_FLOAT)                       :: prinf
        real(C_FLOAT)                       :: noisinf
        integer(C_INT)                      :: icd
        real(C_DOUBLE)                      :: mbike
    end function denoise
end interface

type(PEDKINIndxListType),INTENT(IN)        :: pednl
real(kind=sgl), INTENT(IN), target         :: img_in(1:pednl%npix**2)
real(kind=sgl), INTENT(OUT)                :: img_out(1:pednl%npix**2)


integer(C_INT)                             :: retval
real(C_FLOAT),target                       :: pf, powp, sigma_w, prinf, noisinf
integer(C_INT),target                      :: lx, ly, icd, length
integer(kind=irg)                          :: istat
real(C_DOUBLE), pointer                    :: output(:)
type(C_PTR)                                :: out_ptr
real(C_DOUBLE), allocatable, target        :: aux_img(:)

lx = pednl%npix
ly = pednl%npix

! these values will remain fixed
pf = 1.0
powp = 1.2
sigma_w = 15.0
prinf = 1.0
noisinf = 0.2
icd = 15

allocate(aux_img(1:lx*ly),stat=istat)

aux_img(1:lx*ly) = img_in(1:lx*ly)

out_ptr = denoise(lx, ly, pf, powp, sigma_w, prinf, noisinf, icd, aux_img(1))

call C_F_POINTER(out_ptr, output, (/lx*ly/))

img_out(1:lx*ly) = output(1:lx*ly)

deallocate(aux_img)
deallocate(output)

end subroutine Denoise_PED

!--------------------------------------------------------------------------
!
! SUBROUTINE:Denoise_Pattern
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief Denoise an arbitrary pattern
!
!> @param pednl namelist file for PED
!
!> @date 11/23/15  SS 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Denoise_Pattern(flx, fly, fpf, fpowp, fsigma_w, fprinf, fnoisinf, ficd, img_in, &
                     img_out) bind(C, name = 'Denoise_Pattern')
!DEC$ ATTRIBUTES DLLEXPORT :: Denoise_Pattern

use local
use,INTRINSIC :: ISO_C_BINDING

interface
    type(C_PTR) recursive function denoise (lx, ly, pf, powp, sigma_w, prinf, noisinf, &
    icd, mbike) bind(C, name = 'denoise')

use ISO_C_BINDING

IMPLICIT NONE
 
        integer(C_INT)                      :: lx
        integer(C_INT)                      :: ly
        real(C_FLOAT)                       :: pf
        real(C_FLOAT)                       :: powp
        real(C_FLOAT)                       :: sigma_w
        real(C_FLOAT)                       :: prinf
        real(C_FLOAT)                       :: noisinf
        integer(C_INT)                      :: icd
        real(C_DOUBLE)                      :: mbike
    end function denoise
end interface

integer(kind=irg),INTENT(IN)               :: flx
integer(kind=irg),INTENT(IN)               :: fly
real(kind=sgl),INTENT(IN)                  :: fpf, fpowp, fsigma_w, fprinf, fnoisinf
integer(kind=irg),INTENT(IN)               :: ficd
real(kind=sgl), INTENT(IN), target         :: img_in(1:flx*fly)
real(kind=sgl), INTENT(OUT)                :: img_out(1:flx*fly)


integer(C_INT)                             :: retval
real(C_FLOAT),target                       :: pf, powp, sigma_w, prinf, noisinf
integer(C_INT),target                      :: lx, ly, icd, length
integer(kind=irg)                          :: istat
real(C_DOUBLE), pointer                    :: output(:)
type(C_PTR)                                :: out_ptr
real(C_DOUBLE), allocatable, target        :: aux_img(:)

lx = flx
ly = fly

! these values will remain fixed
pf = fpf ! = 1.0
powp = fpowp ! = 1.2
sigma_w = fsigma_w ! = 15.0
prinf = fprinf ! = 1.0
noisinf =fnoisinf ! = 0.2
icd =ficd ! = 15

allocate(aux_img(1:lx*ly),stat=istat)

aux_img(1:lx*ly) = img_in(1:lx*ly)

out_ptr = denoise(lx, ly, pf, powp, sigma_w, prinf, noisinf, icd, aux_img(1))

call C_F_POINTER(out_ptr, output, (/lx*ly/))

img_out(1:lx*ly) = output(1:lx*ly)

deallocate(aux_img)
deallocate(output)

end subroutine Denoise_Pattern

!--------------------------------------------------------------------------
!
! SUBROUTINE:ctfped_writeFile
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief subroutine to write ctf file for ped indexing program
!
!> @param pednl namelist file for PED
!> @param ipar  some integer parameters passed to the subroutine
!> @param indexmain indices for top nnk match
!> @param eulerarray main euler angle list for the RFZ sampling
!> @param resultmain list of dot products for all nnk matches 
!
!> @date 04/27/16  SS 1.0 original
!--------------------------------------------------------------------------

recursive subroutine ctfped_writeFile(pednl,ipar,indexmain,eulerarray,resultmain)
!DEC$ ATTRIBUTES DLLEXPORT :: ctfped_writeFile

use local
use NameListTypedefs

IMPLICIT NONE

type(PEDKINIndxListType),INTENT(INOUT)              :: pednl
!f2py intent(in,out) ::  pednl
integer(kind=irg),INTENT(IN)                        :: ipar(10)
integer(kind=irg),INTENT(IN)                        :: indexmain(ipar(1),ipar(2))
real(kind=sgl),INTENT(IN)                           :: eulerarray(3,ipar(4))
real(kind=sgl),INTENT(IN)                           :: resultmain(ipar(1),ipar(2))

integer(kind=irg)                                   :: ierr, ii, indx
character(fnlen)                                    :: ctfname
character                                           :: TAB = CHAR(9)
character(fnlen)                                    :: str1,str2,str3,str4,str5,str6,str7,str8,str9,str10
real(kind=sgl)                                      :: euler(3)

! open the file (overwrite old one if it exists)
ctfname = trim(EMsoft_getEMdatapathname())//trim(pednl%ctffile)
ctfname = EMsoft_toNativePath(ctfname)
open(unit=dataunit2,file=trim(ctfname),status='unknown',action='write',iostat=ierr)

write(dataunit2,'(A)') 'Channel Text File'
write(dataunit2,'(A)') 'Prj Test'
write(dataunit2,'(A)') 'Author	'//trim(EMsoft_getUsername())
write(dataunit2,'(A)') 'JobMode	Grid'
write(dataunit2,'(2A,I5)') 'XCells',TAB, pednl%ipf_wd
write(dataunit2,'(2A,I5)') 'YCells',TAB, pednl%ipf_ht
write(dataunit2,'(3A)') 'XStep',TAB,'1.0' 
write(dataunit2,'(3A)') 'YStep',TAB,'1.0'
write(dataunit2,'(A)') 'AcqE1	0'
write(dataunit2,'(A)') 'AcqE2	0'
write(dataunit2,'(A)') 'AcqE3	0'
write(dataunit2,'(A,A,$)') 'Euler angles refer to Sample Coordinate system (CS0)!',TAB
! the following line would need some work to do it properly...
write(dataunit2,'(A)') 'Mag	30	Coverage	100	Device	0	KV	288.9	TiltAngle	-1	TiltAxis	0'
write(dataunit2,'(A)') 'Phases	1'

! here we need to read the .xtal file and extract the lattice parameters, Laue group and space group numbers
write(dataunit2,'(A)') '3.524;3.524;3.524	90;90;90	Nickel	11	225'

! this is the table header
write(dataunit2,'(A)') 'Phase	X	Y	Bands	Error	Euler1	Euler2	Euler3	MAD	BC	BS'

! go through the entire array and write one line per sampling point
do ii = 1,ipar(3)
    indx = indexmain(1,ii)
    euler = eulerarray(1:3,indx)
    write(str1,'(F12.3)') float(MODULO(ii-1,pednl%ipf_wd))
    write(str2,'(F12.3)') float(floor(float(ii-1)/float(pednl%ipf_wd)))
    write(str3,'(I2)') 10
    write(str4,'(F12.6)') resultmain(1,ii)
    write(str5,'(F12.3)') euler(1) - 90.0 ! TSL to HKL convention
    write(str6,'(F12.3)') euler(2)
    write(str7,'(F12.3)') euler(3)
    write(str8,'(I8)') indx
    write(str9,'(I3)') 255
    write(str10,'(I3)') 255
    write(dataunit2,'(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)')'1',TAB,trim(adjustl(str1)),TAB,&
    trim(adjustl(str2)),TAB,trim(adjustl(str3)),TAB,trim(adjustl(str4)),TAB,trim(adjustl(str5)),&
    TAB,trim(adjustl(str6)),TAB,trim(adjustl(str7)),TAB,trim(adjustl(str8)),TAB,trim(adjustl(str9)),&
    TAB,trim(adjustl(str10))
end do

close(dataunit2,status='keep')


end subroutine ctfped_writeFile

end module PEDmod


