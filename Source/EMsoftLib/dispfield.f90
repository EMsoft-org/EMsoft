! ###################################################################
! Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
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

module YSHModule

use local

IMPLICIT NONE

type YDtype
  real(kind=sgl)     :: burg(3), burgd(3), u(3), un(3), g(3), gn(3), id, jd, zu, bs, be, bx, beta
  real(kind=sgl)     :: alpha, ca, sa, ta, cota, a_dc(3,3),a_id(3,3),a_di(3,3), top(3), bottom(3), sig
end type YDtype

type (YDtype), allocatable  :: YD(:)    
!DEC$ ATTRIBUTES DLLEXPORT :: YD


contains




recursive function YSHDisp(x,y,z,ii)
!DEC$ ATTRIBUTES DLLEXPORT :: YSHDisp
!
! compute the displacement field of an inclined dislocation intersection the top surface of 
! the foil, taking into account surface relaxations for the isotropic elastic case (cubic only) ... 
!
! equations are based on the Shaibani&Hazzledine 1981 paper, along with special limits for 
! the alpha->0 case, which were derived by MDG using Mathematica.

use local
use constants

IMPLICIT NONE

integer(kind=irg)  ::  ii
real(kind=dbl)    :: eta, zeta, etap, zetap, r, oms, omts, xx, sgn, om, omp, AA, BB, BBp, th, &
                             k, lam, alA, alB, u, v, w, ms, S, du, dv, dw, qe, me, De, qx, mx, Dx, rr, eps
real(kind=dbl) :: x,y,z
real(kind=dbl) :: YSHDisp(3) 

! initialize the geometrical parameters
eta  = y*YD(ii)%ca - z*YD(ii)%sa
zeta = y*YD(ii)%sa + z*YD(ii)%ca
etap  = -y*YD(ii)%ca - z*YD(ii)%sa
zetap = y*YD(ii)%sa - z*YD(ii)%ca
r = sqrt(x**2+y**2+z**2) 
oms = 1.D0-YD(ii)%sig
omts = 1.D0-2.D0*YD(ii)%sig

! cover the special case of negative x values (based on IDL tests)
xx = x
if (xx.lt.0.D0) then
  x = dabs(x)
  sgn = -1.D0
else 
  sgn = 1.D0
end if

! more parameters
om =  (datan2(y,x)-datan2(eta,x)+datan2(x*r*YD(ii)%sa,eta*y+x**2*YD(ii)%ca))
omp= (datan2(y,x)-datan2(etap,x)+datan2(x*r*YD(ii)%sa,etap*y-x**2*YD(ii)%ca))

AA = r-z
BB  = r-zeta
BBp = r-zetap 
th = 2.D0*oms*(omp-om)
k = 4.D0*oms*omts*YD(ii)%cota**2
lam = omts*dlog(BBp/BB)
alA = dlog(AA)
alB = dlog(BB)


u = 0.D0
v = 0.D0
w = 0.D0
eps = 1.0D-6

! screw component first
if (abs(YD(ii)%bs).gt.eps) then 
  ms = x*sin(2.D0*YD(ii)%alpha)/r/BB
  S = YD(ii)%bs/(4.D0*cPi)
  if (YD(ii)%alpha.gt.0.01) then 
    du = x*ms+2.D0*eta*YD(ii)%ca**2/BB+2.D0*omts*YD(ii)%cota*(-1.D0+YD(ii)%ca+&
            YD(ii)%ca*alA-y*YD(ii)%sa/AA-alB)-sin(2.D0*YD(ii)%alpha)
    dv = y*ms-2.D0*x*YD(ii)%ca/BB-YD(ii)%sa*(omp-om)+2.D0*omts*YD(ii)%cota*(x*YD(ii)%sa/AA-om*YD(ii)%ca)
    dw = z*ms+YD(ii)%ca*(omp-om)-2.D0*omts*om*YD(ii)%ca
  else 
    du = 2.D0*y/(r-z)
    dv = 2.D0*x/(r-z)
    dw = cPi + datan2(y,x) - datan2(-y,x)
  end if
  u = u+du*S
  v = v-sgn*dv*S
  w = w+sgn*dw*S
end if

! then the edge component in the y-z plane
if (abs(YD(ii)%be).gt.eps) then 
  qe = x*(1.D0/BBp-1.D0/BB+2.D0*z*YD(ii)%ca/BB**2)
  me = -qe/r-4.D0*oms*x*YD(ii)%ca**2/r/BB
  De = YD(ii)%be/(8.D0*cPi*oms)
  if (YD(ii)%alpha.gt.0.01) then 
    du = x*me+lam+2.D0*YD(ii)%ca*(z+2.D0*oms*eta*YD(ii)%sa)/BB-4.D0*oms*YD(ii)%sa**2+k*(1.D0-YD(ii)%ca-YD(ii)%ca*alA+&
             y*YD(ii)%sa/AA+alB)
    dv = y*me+qe*YD(ii)%sa+th*YD(ii)%ca+k*(-x*YD(ii)%sa/AA+om*YD(ii)%ca)
    dw = z*me+qe*YD(ii)%ca+th*YD(ii)%sa-2.D0*x*YD(ii)%ca*(1.D0/BBp+omts/BB)+k*om*YD(ii)%sa
!    write (*,*) du,dv,dw
  else 
    rr = x**2+y**2
    du = 2.D0*z/(r-z)+4.D0*x**2*(YD(ii)%sig*rr-r**2)/r/AA**2/(r+z)+2.D0*omts*oms*((x**2+z*(z+r))/AA**2+alA)+omts*dlog((r+z)/AA)
    dv = 4.D0*x*y*(rr*YD(ii)%sig-r**2)/r/AA**2/(r+z)+2.D0*x*y*(rr+2.D0*z*(r+z))*oms*omts/rr**2+&
            2.D0*oms*(cPi + datan2(y,x) - datan2(-y,x))
    dw = 4.D0*x*rr*YD(ii)%sig*(z-2.D0*r*oms)/r/AA**2/(r+z)
  end if
  u = u+du*De
  v = v+sgn*dv*De
  w = w+sgn*dw*De
end if

! and finally the bx edge component
if (abs(YD(ii)%bx).gt.eps) then 
  qx = etap/BBp-eta/BB-2.D0*z*eta*YD(ii)%ca/BB**2
  mx = -qx/r+2.D0*omts*y*YD(ii)%ca/r/BB
  Dx = YD(ii)%bx/(8.D0*cPi*oms)
  du = x*mx+th+k*(x*YD(ii)%ta/AA-om)
  dv = y*mx+qx*YD(ii)%sa-lam*YD(ii)%ca-2.D0*YD(ii)%ca*(z*YD(ii)%ca+omts*y*YD(ii)%sa)/BB+k*(-1.D0+YD(ii)%ca-alA+y*YD(ii)%ta/AA+ &
          YD(ii)%ca*alB)
  dw = z*mx+qx*YD(ii)%ca-lam*YD(ii)%sa-2.D0*etap*YD(ii)%ca/BBp+4.D0*YD(ii)%ca*(oms*y*YD(ii)%ca-omts*z*YD(ii)%sa)/BB+ &
           k*YD(ii)%ta*(YD(ii)%ca-alA+YD(ii)%ca*alB)+4.D0*oms*YD(ii)%ca*YD(ii)%cota
  u = u+sgn*du*Dx
  v = v+dv*Dx
  w = w+dw*Dx
end if

! and return the displacement components
!write (*,*) u,v,w
YSHDisp = (/ u,v,w /)

end function YSHDisp


recursive subroutine makeYSHdislocation(i,dinfo, L)    
!DEC$ ATTRIBUTES DLLEXPORT :: makeYSHdislocation
! 
! this routine pre-computes a number of parameters related to the 
! geometry of the Yoffe&Shaibani&Hazzledine (YSH) surface-relaxed dislocation in an elastically
! isotropic matrix.  These parameters are then used in the CalcR routine.
!
! We implemented the YSH expressions instead of Yoffe's 
! since the former are more easily handled for numerical computations.

! SH have redefined the x-y-z reference frame used by Yoffe to fall along
! the dislocation line itself.  As a result, the Burgers vector must be decomposed
! into a screw component and two edge components, one in the plane of the 
! discontinuity, the other normal to that plane (which is by definition the x-axis).
! Check the SH paper for more details.

use local
use foilmodule
use constants
use crystal

IMPLICIT NONE

real(kind=sgl)  :: alpha, beta, L, tu(3), tx(3), ty(3), te(3), tb(3), bl
integer(kind=irg) :: i,dinfo

! first, determine the alpha angle between the 
! negative z-axis, which is really the negative foil normal, and the line direction
! (make sure to reduce the angle to [0,90] interval).
alpha = CalcAngle(-foil%F,YD(i)%u,'d')*180.0/cPi
if (alpha.gt.90.0) then 
  alpha = 180.0-alpha
  YD(i)%u = -YD(i)%u   ! the u-direction should point down into the foil
end if
write (*,*) 'Foil normal = ',foil%F
write (*,*) 'line direction = ',YD(i)%u
write (*,*) '  --> alpha angle = ',alpha
alpha = alpha*cPi/180.0

call TransSpace(YD(i)%u,tu,'d','c')
call NormVec(tu,'c')
call TransSpace(foil%F,ty,'d','c')
call NormVec(ty,'c')
call CalcCross(tu,ty,tx,'c','c',0)       ! x = u x F
call NormVec(tx,'c')
call CalcCross(tx,tu,te,'c','c',0)             ! e = x x u
call NormVec(te,'c')
call CalcCross(ty,tx,ty,'c','c',0)
call NormVec(ty,'c')
bl = CalcLength(YD(i)%burg,'d')
write (*,*) ' tx = ',tx
write (*,*) ' te = ',te
write (*,*) ' tu = ',tu
write (*,*) ' ty = ',ty
write (*,*) ' bl = ',bl
call TransSpace(YD(i)%burg,tb,'d','c')
call NormVec(tb,'c')
YD(i)%bx = bl * CalcDot(tb,tx,'c')   ! edge component normal to cut plane
YD(i)%be = bl * CalcDot(tb,te,'c')   ! edge component in cut plane
YD(i)%bs = bl * CalcDot(tb,tu,'c')  ! screw component

write (*,*) 'Burgers vector components (bx,be,bs) ',YD(i)%bx,YD(i)%be,YD(i)%bs

! we will also need to know the rotation matrix between the dislocation reference frame 
! and the foil reference frame, so that we can transform the foil coordinates to defect 
! coordinates...  We need the angle beta between the defect x axis (tx) and the foil x axis,
! which is the first column of the foil%a_fc matrix ...
ty(1:3) = foil%a_fc(1:3,1)
write (*,*) ' tx = ',tx
YD(i)%beta = CalcAngle(tx,ty,'d')
write (*,*) ' ty = ',ty
write (*,*) ' beta = ',YD(i)%beta
beta = YD(i)%beta
YD(i)%a_di(1,1:3) = (/ cos(beta), -sin(beta), 0.0 /)
YD(i)%a_di(2,1:3) = (/ sin(beta), cos(beta), 0.0 /)
YD(i)%a_di(3,1:3) = (/ 0.0, 0.0, 1.0 /)
YD(i)%a_id = transpose(YD(i)%a_di)
write (*,*) YD(i)%a_di

! finally some geometrical parameters needed for the displacement field computation...
YD(i)%alpha =  alpha
YD(i)%ca = cos(alpha)
YD(i)%sa = sin(alpha)
YD(i)%ta = tan(alpha)
YD(i)%cota = 1.0/YD(i)%ta

write (*,*) 'angulars = ',cos(alpha),sin(alpha),tan(alpha)

! that's it! the rest is handled in the CalcR routine.

end subroutine makeYSHdislocation



recursive subroutine read_YSH_dislocation_data(dislYname,numYdisl,DF_npix,DF_npiy,DF_gf,L,dinfo)
!DEC$ ATTRIBUTES DLLEXPORT :: read_YSH_dislocation_data

use local
use io
use files
use typedefs

IMPLICIT NONE

integer(kind=irg) :: i,numYdisl,dinfo,DF_npix,DF_npiy
real(kind=sgl) :: id,jd,u(3),bv(3),DF_gf(3),L,poisson
character(50) :: dislYname(3*maxdefects)

namelist / dislocationdata / id, jd, u, bv, poisson

! allocate the memory for the dislocation parameters
  allocate(YD(numYdisl))

! these are just the individual dislocations; the ones that belong to 
! stacking faults are handled separately
   do i=1,numYdisl
    mess = 'opening '//dislYname(i); call Message("(/A)")
    open(UNIT=dataunit,FILE=trim(EMsoft_toNativePath(dislYname(i))),DELIM='APOSTROPHE')
    read(UNIT=dataunit,NML=dislocationdata)
    close(UNIT=dataunit)
    
! top-of-the-foil intersection of dislocation line is transformed to foil coordinates [nm] with DL(i)%kd=0 (center of foil) [verified 4/23/11]
! the point (0,0) is at the center of the image ... hence the factor of 0.5
    YD(i)%id = id * 0.5 * float(DF_npix) ! * L   scaling (zooming) is done in the image reference frame...
    YD(i)%jd = jd * 0.5 * float(DF_npiy) ! * L
    YD(i)%u = u
    YD(i)%burg = bv
    YD(i)%g = DF_gf
    YD(i)%sig = poisson
    
! and pre-compute the dislocation displacement field parameters
       call makeYSHdislocation(i,dinfo, L)    
  end do
  
end subroutine read_YSH_dislocation_data





end module YSHModule




program dispfield 
! 
use local
use typedefs
use crystalvars 
use crystal
use symmetryvars
use symmetry
use postscript
use constants
use diffraction
use dynamical
use files
use io
use foilmodule
use stacking_fault
use dislocation
use void
use inclusion
use defectmodule     
use TIFF_global
use TIFF_f90
use pgm
use timing
use STEMmodule
use YSHModule

IMPLICIT NONE

integer(kind=irg)       :: ira,nn,izero,i,j,k,n,nsl,numi,npix,npiy,ii,jj,numvoids,numdisl,numsf, &
                                  numinc,dinfo,t_interval,outputfirst,outputlast,DF_nums_new, numYdisl, &
                                  DF_npix_new,DF_npiy_new, numstart,numstop, isg, TID, NTHR, jcnt, numCL, iCL
!                                  OMP_GET_THREAD_NUM,OMP_GET_NUM_THREADS
real(kind=sgl)          :: ind(3),hkl(3),exerg,cosom,glen,exer,dgr,sl,thr,arg,zmax,thick,mi,ma, &
                                  att,xgp,DF_gf(3),DF_gd(3,maxdefects)
character(50)           :: fname,sgname,voidname,dislname(3*maxdefects),sfname(maxdefects),outputroot, &
                                  incname,dispfile, dislYname(3*maxdefects)
character(100)          :: imname
character(4)            :: outputformat, illumination_mode, dispmode
character(5)            :: fnumber(-10:10)
character(len=10)       :: ci

complex(kind=dbl)                :: czero,cone
real(kind=sgl),allocatable    :: disparray(:,:,:,:)


namelist / rundata / DF_L, DF_npix, DF_npiy, DF_slice, Nmat, sgname, numvoids, incname, &
                                 voidname, numdisl, dislname, numYdisl, dislYname, numsf, sfname, dinfo, outputformat, &
				 outputroot,t_interval,illumination_mode,outputfirst,outputlast, dispfile, &
				 dispmode

! display the standard program info
 progname = 'dispfield.f90'
 progdesc = 'Defect displacement field simulation program'
 call EMsoft
 
! get the crystal data and microscope voltage
 SG % SYM_reduce=.TRUE.
 call CrystalData             ! read crystal structure
 hexset = .FALSE.
 
 czero=cmplx(0.0,0.0,dbl)
 cone=cmplx(1.0,0.0,dbl)
  
! here we read the general simulation information from a namelist file SRdef_rundata.nml
! first we define the default values
 DF_L = 1.0             ! edge length of column in nanometers
 DF_npix = 256       ! number of image pixels along x
 DF_npiy = 256       ! number of image pixels along y 
 DF_slice = 1.0       ! slice thickness in nanometers
 Nmat = 10000       ! number of precomputed A matrices to be stored
 dinfo = 0               ! switch to make makedislocation verbose
 sgname = 'nofile'   ! if this variable is different from 'nofile', then an external sg array is read (to be implemented)
 numdisl = 0           ! number of dislocation files
 numYdisl = 0           ! number of Yoffe dislocation files
 numsf = 0             ! number of stacking fault files
 numinc = 0           ! number of inclusions
 numvoids = 0       ! number of voids
 voidname = 'none' ! filename for void data
 dislname = ''         ! filenames for dislocation data
 dislYname = ''         ! filenames for Yoffe dislocation data
 sfname = ''            ! filenames for stacking fault data
 incname = 'none'   ! filename for inclusion data
 outputformat = 'data'  ! format for output data, can be 'data', 'pgm', or 'tiff'
 outputroot = 'image'  ! default root name for output files
 outputfirst = 1       ! first image number to be written to file (will be set to first image in SR)
 outputlast = nn       ! last image number to be written to file (will be set to last image in SR)
 t_interval = 10       ! default timing interval (output every t_interval image columns)
 dispfile = 'none'     ! name of the displacement field output file (will be created if different from none)
 dispmode = 'not'  ! should a diplacement file be written ('new') or read ('old') or neither ('not')?
 illumination_mode = 'EM'  ! default illumination mode (can be 'EM' or 'STEM')
 
 
! then we read the rundata namelist, which may override some of these defaults  
 OPEN(UNIT=dataunit,FILE='SRdef_rundata.nml',DELIM='APOSTROPHE')
 READ(UNIT=dataunit,NML=rundata)
 CLOSE(UNIT=dataunit)



! next, we read the foildata namelist from the SRdef_foildata.nml file
! this includes material property data, in this case the elastic moduli
! in particular, the foilmodule MUST compute the beam direction from
! the sample tilt angles !!!
  call read_foil_data(DF_npix,DF_npiy,DF_L)
  
  
! define the foil thickness, attenuation, and number slices per column
thick = foil%zb    ! this is the same everywhere for this version; needs to be updated in the next version
DF_nums = nint(thick/DF_slice)  ! this is the number of slices for this particular column


! next, deal with all the defects
!
! if there is a diplacement field file entered in the STEM_rundata.nml file,  
! then we simply read that file in; otherwise, we read all the defect descriptor files

! is there a void data filename? If so, then read it  
   if (voidname.ne.'none') call read_void_data(numvoids,voidname,DF_L,DF_npix,DF_npiy,dinfo)

! read namelist files for all dislocations, if any
   if (numdisl.gt.0) call read_dislocation_data(dislname,numdisl,numsf,DF_npix,DF_npiy,DF_gf,DF_L,dinfo)

! read namelist files for all Yoffe dislocations, if any
   if (numYdisl.gt.0) call read_YSH_dislocation_data(dislYname,numYdisl,DF_npix,DF_npiy,DF_gf,DF_L,dinfo)

! read namelist files for all stacking faults, if any
   if (numsf.gt.0) call read_stacking_fault_data(numsf,numdisl,sfname,DF_L,DF_npix,DF_npiy,DF_g,dinfo)

! is there an inclusion data file? if so, then read it
   if (incname.ne.'none') call read_inclusion_data(numinc,incname,DF_L,DF_npix,DF_npiy,dinfo)

! transform the g-vector to the defect reference frames (needed for all dislocations in CalcR).
! this can only be done AFTER all dislocations and stacking faults have been created.
   do i=1,numdisl
     DF_gd(0:2,i) = matmul(DL(i)%a_dc,DF_gc)
   end do
   
! precompute ALL the defect columns and, if needed, store them in dispfile
! this portion should be carried out in multi-threaded mode as much as possible
  allocate(disparray(DF_nums,DF_npix,DF_npiy,3))
  disparray = 0.0
  mess = ' Starting Displacement Field Computation '; call Message("(A/)")

  call CalcRLocal(numvoids,numdisl,numYdisl,numsf,numinc,DF_nums,DF_npix,DF_npiy,t_interval,disparray)
   
   write (*,*) maxval(disparray),minval(disparray)
   
! and, if needed, store the defect displacement field for re-runs
    mess = 'Displacement field data stored in file '//dispfile; call Message("(/A/)")
    open(unit=dataunit,file=trim(EMsoft_toNativePath(dispfile)),status='new',action='write',form='unformatted')
    i=3
    write (dataunit) DF_nums,DF_npix,DF_npiy,i
    write (dataunit) disparray
    call SafeCloseFile('d1','keep',dispfile,.FALSE.)

end program dispfield



recursive subroutine CalcRLocal(numvoids,numdisl,numYdisl,numsf,numinc,lDFnums,lDFnpix,lDFnpiy,t_interval,disparray)

! this routine returns the total displacement field (multithreaded with OPENMP)

use local
use constants
use crystal
use crystalvars
use dislocation
use foilmodule
use void
use stacking_fault
use inclusion
use defectmodule
use timing
use YSHModule

IMPLICIT NONE

integer(kind=irg)     :: i,j,k,ii,islice,numvoids,numdisl,numYdisl,numsf,numinc,lDFnums,lDFnpix,lDFnpiy, &
                                  TID,NTHR,imat,t_interval,jcnt
real(kind=sgl)          :: disparray(lDFnums,lDFnpix,lDFnpiy,3)
real(kind=sgl)          :: rx,ry,rz,dis,xpos,ypos,zpos,RR(3),sumR(3),thick,tmp(3),tmpf(3),u(3),zaamp,zaphase,zar,zai,zr(3),zi(3), &
                                 zt,fx,fy,fz,z0,gdotR,nunit(3)    !,&
!                                nu,x,y,z,zn,t,pre,r1,r2,r3,th,rn 
                         
real(kind=dbl)            :: afi(3,3), afc(3,3), lDFR(3)
complex(kind=dbl)     :: za(3)
complex(kind=sgl)     :: zero
logical               :: void
type (voidtype), allocatable  :: lvoids(:)
real(kind=sgl),allocatable :: lsg(:,:)
type (dislocationtype), allocatable  :: lDL(:)    
type (YDtype), allocatable  :: YDL(:)    
type (stackingfaulttype), allocatable  :: lSF(:)
type (inclusiontype), allocatable  :: linclusions(:)

! before we start the threads, we need to copy data from various modules
! into local variables that can then be accessed by the threads ...
 Nmat = 10000
! foil unit normal in microscope frame 
 nunit = matmul(foil%Fn,transpose(foil%a_mc))
! foil normal parameters for zpos computation
 fx = -nunit(1)/nunit(3)
 fy = -nunit(2)/nunit(3)
 fz = foil%zb*0.5
! other parameters
 z0 = foil%z0
 thick = foil%zb
 zero = cmplx(0.0,0.0)
 afi = foil%a_fi
 afc = foil%a_fc
 if (allocated(voids)) then
   allocate(lvoids(numvoids))
   lvoids = voids
 endif
 allocate(lsg(foil%npix,foil%npiy))
 lsg = foil%sg
 if (allocated(DL)) then 
   allocate(lDL(numdisl+2*numsf))
   lDL = DL
 endif
 if (allocated(YD)) then 
   allocate(YDL(numYdisl))
   YDL = YD
 endif
 if (allocated(SF)) then 
   allocate(lSF(numsf))
   do ii=1,numsf 
     allocate(lSF(ii)%zpos(foil%npix,foil%npiy))
   end do
   lSF = SF
 endif
 if (allocated(inclusions)) then
   allocate(linclusions(numinc))
   linclusions = inclusions
 end if
 
! ok, we've copied all the necessary variables into local structures
! now we can perform the multi-threaded loop

! initiate multi-threaded segment
!$OMP     PARALLEL PRIVATE(TID,lDFR,gdotR,i,j,k,imat,zt,xpos,ypos,zpos,islice,dis,sumR,tmp,tmpf, &
!$OMP&   ii,void,za,zar,zai,zaamp,zaphase,zr,zi,u,jcnt) &
!$OMP&   SHARED(NTHR,lDFnpix,lDFnpiy,lDFnums,DF_L,numvoids,numdisl,numsf,numinc,disparray,t_interval, &
!$OMP&    fx,fy,fz,z0,thick,zero,afi,afc,lvoids,lsg,lSF,linclusions,lDL,Nmat,YDL)
!  NTHR = OMP_GET_NUM_THREADS()
!  TID = OMP_GET_THREAD_NUM()
TID = 0
NTHR = 1
  if (TID.eq.0) then
! do time reporting only in the master thread
    write (*,*) 'Message from master thread ',TID,': splitting into ',NTHR,' threads '
    call Time_reset
    call Time_report(0.01*t_interval)
    call Time_start
    jcnt = 0
  end if

!$OMP barrier
!$OMP DO SCHEDULE (GUIDED)
  do i=1,lDFnpix  
    do j=1,lDFnpiy
! compute the displacement vectors lDFR for all points in this column
     
! scale the image coordinates with respect to the origin at the center of the image;
! this is where we need to include the zoom factor ...
      xpos = float(i-lDFnpix/2) * DF_L
      ypos = float(j-lDFnpiy/2) * DF_L
      zt =  (xpos*fx+ypos*fy+fz)
       
! loop over all slices (this is the main loop)
 sliceloop: do islice = 1,lDFnums 
      lDFR = 0.0

! zpos is the position down the column, starting at zt (in image coordinates)
      zpos = zt - float(islice)*DF_slice
      
    
! set the displacements to zero
       sumR = 0.0
    
! convert image point (xpos,ypos,zpos) to tmpf in the foil reference frame
!       tmpf = matmul(matmul( (/ xpos, ypos, zpos /),transpose(afi)),afc)
       tmpf = matmul( (/ xpos, ypos, zpos /),transpose(afi))
    
! let's put a few dislocations in ... (see section 8.4.2)

       do ii=1,numdisl
! convert the defect location from untilted image space to the tilted foil reference frame, and subtract it from the current
! column and slice position
         tmp =  (/ xpos, ypos, zpos /) - matmul( (/ DF_L*lDL(ii)%id, DF_L*lDL(ii)%jd, lDL(ii)%zfrac*z0 /), transpose(afi) )
! then convert the difference vector to the defect reference frame for this dislocation (we will only need the x and y coordinates)
         tmp = matmul(tmp,lDL(ii)%a_id)
! check the z-coordinate; if it falls beyond the dislocation line that is inside the foil, then skip
! the displacement computation... the top and bottom coordinates of the dislocation intersections
! measured along the dislocation line were pre-computed when the dislocations were first read from
! the namelist files...
!         if (abs(tmp(3)).le.lDL(ii)%zu) then
! compute x1 + p_alpha x2  (eq. 8.38)
          za(1:3) = tmp(1) + lDL(ii)%pa(1:3)*tmp(2)
! compute the displacement vector u (eq. 8.38) [this expands the log of a complex number and takes the real part only] 
          if (tmp(1).gt.0.0) then
           do k=1,3
            zar =  real(za(k))
            zai = aimag(za(k))
            zaamp = abs(za(k))
            zaphase = abs(zai/zar)
            zr(k) = log(zaamp)
            zi(k) = atan(zaphase)
            if (zar.le.0.0) then
              if (zai.lt.0.0) zi(k) = -cPi+zi(k)
              if (zai.eq.0.0) zi(k) = cPi
              if (zai.gt.0.0) zi(k) = cPi-zi(k)
            else
              if (zai.lt.0.0) zi(k) = -zi(k)
            end if
           end do
          else
           do k=1,3
            zar =  real(za(k))
            zai = aimag(za(k))
            zaamp = abs(za(k))
            zaphase = abs(zai/zar)
            zr(k) = log(zaamp)
            zi(k) = atan(zaphase)
            if (zar.le.0.0) then
              if (zai.gt.0.0) zi(k) = cPi-zi(k)
              if (zai.eq.0.0) zi(k) = cPi
              if (zai.lt.0.0) zi(k) = cPi+zi(k)
            else
              if (zai.lt.0.0) zi(k) = 2.0*cPi-zi(k)
              if (zai.eq.0.0) zi(k) = 0.0
            end if
           end do  
          end if
          u = 2.0*real(matmul(lDL(ii)%dismat,cmplx(zr,zi)))
! transform displacement vector u to the Cartesian crystal reference frame and then to the foil frame
          u = matmul(matmul(u,lDL(ii)%a_dc),transpose(afc))
          sumR = sumR + u
!         end if 
       end do

! do we have any dislocations with surface relaxations ?  YSH model
       if (numYdisl.gt.0) then 
        do ii=1,numYdisl
! first, figure out what the coordinates are in the YSH reference frame for this dislocation ... 
! translate to the defect origin
         tmp =  (/ xpos, ypos, zpos /) -  (/ DF_L*YDL(ii)%id, DF_L*YDL(ii)%jd, z0*0.5 /)
! rotate into the defect reference frame
         tmp = matmul(tmp,YDL(ii)%a_id)
! compute the displacement vector
         u = sngl(YSHDisp(dble(tmp(1)),dble(tmp(2)),dble(tmp(3)),ii))
!         write (*,*) u
! and rotate back to the image reference frame
         u = matmul(u,YDL(ii)%a_di)
! that should do it !
         sumR = sumR + u
       end do
      end if


! stacking faults (this is easy because we've already done all the work in the stacking_fault module)
!       do ii=1,numsf
!         if ((zpos.lt.lSF(ii)%zpos(i,j)).and.(lSF(ii)%zpos(i,j).ne.-10000.0)) then 
!           sumR = sumR + lSF(ii)%lpbc
!         end if
!       end do

! write(*,*) sumR(1:3)
      disparray(islice,i,j,1:3) = sumR(1:3)
!      if (i.eq.10) write (*,*) sumR(1:3)
      
    end do sliceloop
    
   end do  ! j-loop
  if (TID.eq.0) then 
    jcnt = jcnt+1
    if (mod(jcnt,t_interval).eq.0) call Time_remaining(jcnt,lDFnpix/NTHR)
  endif
end do  ! i-loop
!$OMP END DO 
 if (TID.eq.0) call Time_stop(DF_npix*DF_npiy)
!$OMP END PARALLEL

end subroutine CalcRLocal




       
