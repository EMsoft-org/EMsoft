! ###################################################################
! Copyright (c) 2015-2022, Marc De Graef Research Group/Carnegie Mellon University
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
! EMsoft:EMcuboMK.f90
!--------------------------------------------------------------------------
!
! PROGRAM: EMcuboMK
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate MacKenzie histogram for a given rotational symmetry based on cubochoric sampling
!
!> @note This is a simple utility program to generate a histogram of a MacKenzie misorientation plot 
!> for a given rotational symmetry group, based on a concentric sampling of cubochoric space.
!
!> @date 02/21/19 MDG 1.0 original
!--------------------------------------------------------------------------
program EMcuboMK

use local 
use constants
use io 
use error 
use rotations
use so3
use symmetry
use files

IMPLICIT NONE 

character(fnlen)				:: progname, progdesc
integer(kind=irg)				:: pgnum, FZorder, FZtype, nsteps, n, i, j, k, io_int(2), ntot, pgrotOrder
real(kind=dbl)					:: sedge, delta, x, y, z, rod(4), tot, rho, ho2(3), ho1(3), vol
real(kind=dbl),allocatable		:: misor(:), histogram(:), e(:), mk(:)
logical 						:: b


progname = 'EMcuboMK.f90'
progdesc = 'Generate MacKenzie histogram for a given rotational symmetry based on cubochoric sampling'

! print some information
call EMsoft(progname, progdesc)

! deal with the command line arguments, if any
call Interpret_Program_Arguments(1,(/ 906 /), progname)

! first ask for the rotational point group number
call ListPointGroups
call ReadValue('Enter the desired point group number: ',io_int)
pgnum = io_int(1)
pgrotOrder = PGTHDorder(PGrot(pgnum))

! determine the Laue symmetry group and print it 
call Message(' ')
call Message(' Corresponding pure rotational group is '//trim(PGTHD(PGrot(pgnum))) )

! get the fundamental zone parameters
call getFZtypeandorder(pgnum, FZtype, FZorder)
io_int(1) = FZtype
io_int(2) = FZorder 
call WriteValue(' FZ type and order :',io_int,2)

! ask the user for the number of steps along the cubochoric semi edge length
call Message(' ')
call ReadValue('Enter the desired number of steps along the cubochoric semi-edge length: ',io_int)
nsteps = io_int(1)

! allocate output arrays 
allocate(misor(0:nsteps), histogram(0:nsteps), e(0:nsteps), mk(0:nsteps))
histogram = 0.D0

! set some auxiliary parameters
sedge = 0.5D0 * LPs%ap
delta = sedge / dble(nsteps)

! start the sampling, working in concentric cubes from the center outward
do n = 0, nsteps 
	if (n.eq.0) then 
		misor(0) = 0.D0
		e(0) = 0.D0
		histogram(0) = 0.D0 
	else ! loop over concentric cubes 
		e(n) = dble(n) * delta   ! semi-edge length of sub-cube
		! get the angle first 
		if (n.lt.nsteps) then 
			rod = cu2ro( (/ e(n), 0.D0, 0.D0 /) )
			misor(n) = 2.D0 * atan(rod(4))
		else
			misor(n) = cPi
		end if
		! cover the +/- x faces with a complete grid
		do j = -n, n
			y = dble(j) * delta
			do k = -n, n
				z = dble(k) * delta
				rod = cu2ro( (/ e(n), y, z/) )
		        b = IsinsideFZ(rod,FZtype,FZorder)
		        if (b) histogram(n) = histogram(n) + 1.D0
				rod = cu2ro( (/ -e(n), y, z/) )
		        b = IsinsideFZ(rod,FZtype,FZorder)
		        if (b) histogram(n) = histogram(n) + 1.D0
			end do 
		end do 
		! then cover the +/- y faces with a grid, omitting the already covered points 
		do i = -n+1, n-1
			x = dble(i) * delta
			do k = -n, n
				z = dble(k) * delta
				rod = cu2ro( (/ x, e(n), z/) )
		        b = IsinsideFZ(rod,FZtype,FZorder)
		        if (b) histogram(n) = histogram(n) + 1.D0
				rod = cu2ro( (/ x, -e(n), z/) )
		        b = IsinsideFZ(rod,FZtype,FZorder)
		        if (b) histogram(n) = histogram(n) + 1.D0
			end do 
		end do 
		! finally cover the +/- z faces with a grid, omitting the already covered points 
		do i = -n+1, n-1
			x = dble(i) * delta
			do j = -n+1, n-1
				y = dble(j) * delta
				rod = cu2ro( (/ x, y, e(n)/) )
		        b = IsinsideFZ(rod,FZtype,FZorder)
		        if (b) histogram(n) = histogram(n) + 1.D0
				rod = cu2ro( (/ x, y, -e(n)/) )
		        b = IsinsideFZ(rod,FZtype,FZorder)
		        if (b) histogram(n) = histogram(n) + 1.D0
			end do 
		end do 
		histogram(n) = histogram(n) * sin(misor(n)*0.5D0)**2 / dble( 2.D0 + 24.D0 * n**2 )
	end if
end do

call getMacKenzieDistribution(pgnum, nsteps, misor, mk)

! normalize 
histogram =  pgrotOrder * (4.D0 * cPi) * (cPi / 180.D0) * histogram / ( 2.D0 * cPi**2 )
misor = misor * 180.D0 / cPi

! and print the results to a text file
open(unit=20,file='EMcuboMK.csv',status='unknown',form='formatted')
write (20,"(A)") 'angle, sampled, theoretical'
write (20,"(I5,',',I5,',',I5)") nsteps, nsteps, nsteps
do n=0,nsteps
  write (20,"(F12.8,',',F12.8,',',F12.8)") misor(n), histogram(n), mk(n)
end do 
close(unit=20,status='keep')
call Message('Results stored in EMcuboMK.csv file...')

end program EMcuboMK
