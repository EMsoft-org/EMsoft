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
! EMsoft:timing.f90
!--------------------------------------------------------------------------
!
! MODULE: timing
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Provides a few timing routines
! 
!> @date   11/19/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 replaced globals by timetype argument
!--------------------------------------------------------------------------
module timing

use local
use typedefs
use io

IMPLICIT NONE

contains



!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_tick
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief start time recording using system_clock
!
!> @date   03/17/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine Time_tick(t)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_tick

IMPLICIT NONE

integer(kind=irg), intent(OUT) :: t

call system_clock(t)

end subroutine Time_tick


!--------------------------------------------------------------------------
!
! FUNCTION: Time_tock
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief stop time recording using system_clock
!
!> @date   03/17/16 MDG 1.0 original
!--------------------------------------------------------------------------
recursive function Time_tock(t) result(tock)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_tock

IMPLICIT NONE

integer(kind=irg), intent(in)   :: t
integer(kind=irg)               :: now, clock_rate
integer(kind=irg)               :: tock

call system_clock(now,clock_rate)
tock = real(now - t)/real(clock_rate)

end function Time_tock



!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_reset
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief reset time recording
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 added TT as argument
!--------------------------------------------------------------------------
recursive subroutine Time_reset(TT)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_reset

IMPLICIT NONE

type(timetype),INTENT(INOUT)	:: TT
!f2py intent(in,out) ::  TT

TT%TIME_t_count = 0.0
TT%TIME_unit_count = 0.0
TT%TIME_count = 0
TT%TIME_newcount = 0
TT%TIME_count_rate = 0
TT%TIME_count_max = HUGE(0)
TT%TIME_old = 0
TT%TIME_loops = 0

end subroutine Time_reset

!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_report
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief report time recording
!
!> @param TT time structure
!> @param interval interval for reporting
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 added TT as argument
!--------------------------------------------------------------------------
recursive subroutine Time_report(TT, interval)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_report

IMPLICIT NONE

type(timetype),INTENT(INOUT)		:: TT
!f2py intent(in,out) ::  TT
real(kind=sgl),intent(IN)   		:: interval

 TT%TIME_interval = interval
 TT%TIME_fraction = TT%TIME_interval
 call Message('Starting computation', frm = "(/A)")

end subroutine Time_report

!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_start
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief start time recording
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 added TT as argument
!--------------------------------------------------------------------------
recursive subroutine Time_start(TT)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_start

IMPLICIT NONE

type(timetype),INTENT(INOUT)		:: TT
!f2py intent(in,out) ::  TT

! start the timing of the computation
 call Time_reset(TT)
 call system_clock(TT%TIME_count,TT%TIME_count_rate,TT%TIME_count_max)

end subroutine Time_start

!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_estimate
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief estimare remaining time
!
!> @param TT time structure
!> @param numk number of idividual computations
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 added TT as argument
!--------------------------------------------------------------------------
recursive subroutine Time_estimate(TT, numk)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_estimate

IMPLICIT NONE

type(timetype),INTENT(INOUT)		:: TT
!f2py intent(in,out) ::  TT
integer(kind=irg),intent(IN)     	:: numk

integer(kind=irg)      		:: TIME_nc
real(kind=sgl)				:: io_real(1)

! get the current time
 call system_clock(TIME_nc, TT%TIME_count_rate, TT%TIME_count_max)
 TT%TIME_newcount = TIME_nc
 TT%TIME_t_count = float(TT%TIME_newcount-TT%TIME_count)/float(TT%TIME_count_rate)
 TT%TIME_unit_count = TT%TIME_t_count
 io_real(1) = TT%TIME_unit_count
 call WriteValue(' Time for first computation step [s, typically overestimate] :', io_real, 1, frm = "(F10.5)")
 call Message('  Anticipated total computation time :', frm = "(A$)")
 call PrintTime(TT%TIME_unit_count*float(numk))
 
end subroutine Time_estimate

!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_estimate
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief estimate remaining time
!
!> @param TT time structure
!> @param ik current computation
!> @param numk number of idividual computations
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 added TT as argument
!--------------------------------------------------------------------------
recursive subroutine Time_remaining(TT, ik, numk)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_remaining

IMPLICIT NONE

type(timetype),INTENT(INOUT)		:: TT
!f2py intent(in,out) ::  TT
integer(kind=irg),intent(IN)   	:: ik
integer(kind=irg),intent(IN)   	:: numk

integer(kind=irg)    			:: TIME_nc, io_int(1)
real(kind=sgl)				:: io_real(1)


 TT%TIME_fraction = TT%TIME_fraction + TT%TIME_interval

! get the current time
 call system_clock(TIME_nc, TT%TIME_count_rate, TT%TIME_count_max)

! correct for the resetting of TIME_nc when TIME_count_max is reached
 if (TIME_nc.lt.TT%TIME_newcount) then     ! we've looped through the entire cycle
   TT%TIME_loops = TT%TIME_loops+1
   TT%TIME_count = 0
 end if 
 TT%TIME_newcount = TIME_nc

! and print it
 TT%TIME_t_count = (float(TT%TIME_loops)*float(TT%TIME_count_max)+float(TT%TIME_newcount-TT%TIME_count))/float(TT%TIME_count_rate)

! reset the time per unit
 TT%TIME_unit_count = TT%TIME_t_count/float(ik)

! print estimated remaining time
 io_int(1) = nint(100.0*TT%TIME_t_count/(TT%TIME_t_count+TT%TIME_unit_count*(float(numk)-float(ik))))
 call WriteValue (' ',io_int, 1, frm = "(1x,I3,' % completed; '$)") 
 io_real(1) = TT%TIME_t_count
 call WriteValue(' Total computation time [s] ', io_real, 1, frm = "(F$)")
 call Message(';  Estimated remaining time : ', frm = "(A$)")
 call PrintTime(TT%TIME_unit_count*(float(numk)-float(ik)))
!
end subroutine Time_remaining

!--------------------------------------------------------------------------
!
! SUBROUTINE: PrintTime
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief print  time
!
!> @param tm time variable
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 changed IO
!--------------------------------------------------------------------------
recursive subroutine PrintTime(tm)
!DEC$ ATTRIBUTES DLLEXPORT :: PrintTime

IMPLICIT NONE

real(kind=sgl),INTENT(IN)		:: tm

integer(kind=irg)    			:: days, hours, minutes, seconds, io_int(4)
real(kind=sgl)       			:: secs

  secs = tm
  days = 0
  hours = 0
  minutes = 0
  if (secs.gt.86400.0) then
    days = int(secs)/86400
    secs = mod(secs,86400.0)
  end if
  if (secs.gt.3600.0) then
    hours = int(secs)/3600
    secs = mod(secs,3600.0)
  end if
  if (secs.gt.60.0) then
    minutes = int(secs)/60
    secs = mod(secs,60.0)
  end if
  seconds = int(secs)

  io_int(1:4) = (/ days, hours, minutes, seconds /)
  call WriteValue(' ',io_int, 4, frm = "(1x,I3,' d,',I3,' h,',I3,' m,',I3,' s')")

end subroutine PrintTime

!--------------------------------------------------------------------------
!
! SUBROUTINE: Time_stop
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief stop time recording
!
!> @param TT time structure
!> @param numk total number of computations
!
!> @date   06/04/01 MDG 1.0 original
!> @date   06/04/13 MDG 2.0 rewrite
!> @date   06/05/14 MDG 3.0 added TT; changed IO
!--------------------------------------------------------------------------
recursive subroutine Time_stop(TT, numk)
!DEC$ ATTRIBUTES DLLEXPORT :: Time_stop

IMPLICIT NONE

type(timetype),INTENT(INOUT)		:: TT
!f2py intent(in,out) ::  TT
integer(kind=irg),INTENT(IN)  		:: numk

real(kind=sgl)				:: io_real(1)


  call system_clock(TT%TIME_newcount, TT%TIME_count_rate, TT%TIME_count_max)
  call Message('  Total computation time [s] ', frm = "(A$)")
  call PrintTime((float(TT%TIME_loops)*float(TT%TIME_count_max)+float(TT%TIME_newcount-TT%TIME_count))/float(TT%TIME_count_rate))
  io_real(1)= float(TT%TIME_loops)*float(TT%TIME_count_max)+float(TT%TIME_newcount-TT%TIME_count)
  io_real(1) = io_real(1)/float(TT%TIME_count_rate)/float(numk)
  call WriteValue(' Time per step/pixel [s] ', io_real, 1, frm = "(F)")
end subroutine Time_stop


end module timing
