! ###################################################################
! Copyright (c) 2013-2019, Marc De Graef Research Group\/Carnegie Mellon University
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
! EMsoft:EMfamily.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMfamily 
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Draw stereographic projection of a family of planes or directions
!
!> @details  This program draws a stereographic projection of either real or reciprocal space for an arbitrary
!>  crystal system and viewing direction.  The program is different from stereo.f in that it only draws
!>  the requested families <uvw> or {hkl}.  Multiple output files can be generated.
!>  
! 
!> @date 10/16/98 MDG 1.0 original
!> @date 5/21/01 MDG 2.0 f90
!> @date 4/8/13 MDG 3.0 revised version
!--------------------------------------------------------------------------
program EMfamily

use local
use typedefs
use crystal
use HDFsupport
use symmetry
use postscript
use io
use math
use graphics
use files

IMPLICIT NONE

character(1)         		:: sp
logical              		:: nn,topbot
real(kind=sgl)       		:: rr(3),g(3),r(3),M(3,3), CX, CY, CRad, negthresh,xst,yst
integer(kind=irg)    		:: h,k,l,hkl(3),iview(3),cr,ans,sgn,i,num, io_int(1), imanum
character(fnlen)                :: progname, progdesc, gname
character(200)                  :: parta
type(unitcell),pointer	        :: cell
logical                         :: loadingfile
type(postscript_type)           :: PS
integer(kind=irg)           	:: itmp(48,3)

 progname = 'EMfamily.f90'
 progdesc = 'Stereographic projection of a family of directions/planes'
 call EMsoft(progname, progdesc)
 
  allocate(cell)
 cell % SG % SYM_reduce=.TRUE.
 topbot=.TRUE.

! 20cm radius projection circle [inches]
 CRad = 3.937
 CX = 3.25
 CY = 3.5
 negthresh=-0.0001
 imanum = 1

! read crystal information
 call ReadValue(' Enter xtal file name : ', gname,"(A)")
 cell%fname = trim(gname)
 call CrystalData(cell)
 sgn = 1 

! main loop 
 do while (sgn.eq.1) 

! real space or reciprocal space?
  call GetDrawingSpace(sp)

! viewing direction (watch for hexagonal indices !)
  call GetViewingDirection(cell%hexset,iview)

! create transformation matrix
  call ProjectionMatrix(cell,iview,M)

! open PostScript file
  loadingfile = .FALSE.
  call PS_openfile(PS, progdesc, imanum)

! write text and draw projection circle
  call DrawSPFrame(PS, cell, CX, CY, CRad, iview, sp)
  ans = 1

! loop over families
  do while (ans.eq.1)

! loop over all points and draw projection+label
   call GetIndex(cell%hexset,hkl,sp)
   call CalcFamily(cell,hkl,num,sp,itmp)
   io_int(:1) = num
   call WriteValue('  Multiplicity = ', io_int, 1, "(I3)")

   do i=1,num
    h=itmp(i,1)
    k=itmp(i,2)
    l=itmp(i,3)
    hkl(1)=h
    hkl(2)=k
    hkl(3)=l

! reduce to smallest integers to avoid overlap
! of indices, such as (111) and (222)
    call IndexReduce(hkl)
    g(1)=float(h)
    g(2)=float(k)
    g(3)=float(l)
    h=hkl(1)
    k=hkl(2)
    l=hkl(3)
    call TransSpace(cell,g,r,sp,'c')
    call NormVec(cell,r,'c')

! apply viewing tansformation
    rr = matmul(M,r)

! compute stereographic projection coordinates
    xst=CX+CRad*rr(1)/(1.0+abs(rr(3)))
    yst=CY+CRad*rr(2)/(1.0+abs(rr(3)))

! and draw the projection point along with its label
    cr=1
    if (rr(3).gt.negthresh) then
     call PS_filledcircle(xst,yst,0.015/PS % psscale,0.0)
     nn = .TRUE.
     call DumpIndices(PS,cell%hexset,sp,h,k,l,cr,xst,yst,nn)
    else if (topbot) then
     call PS_circle(xst,yst,0.035/PS % psscale)
     nn = .FALSE.
     call DumpIndices(PS,cell%hexset,sp,h,k,l,cr,xst,yst,nn)
    endif
   end do

! another family on the same drawing ?
   call ReadValue(' Another family (1/0) ? ', io_int,1)
   ans = io_int(1)
  end do

! close Postscript file
  call PS_closefile(PS)

! loop for another drawing ?
   call ReadValue(' Another pattern (1/0) ? ', io_int,1)
   sgn = io_int(1)
 end do

end program
