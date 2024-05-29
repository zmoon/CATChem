module testing_mod
   use iso_fortran_env, only: rk => real32
   implicit none

   private
   public :: assert, assert_close

contains

   !> Error out if `cond` is false, with optional `msg`.
   subroutine assert(cond, msg)
      logical, intent(in) :: cond  !< A conditional expression or variable
      character(len=*), intent(in), optional :: msg  !< Brief description of the assertion

      character(len=:), allocatable :: msg_

      if (.not. present(msg)) then
         msg_ = "Assertion failed"
      else
         msg_ = "Assertion '"//trim(adjustl(msg))//"' failed"
      end if

      if (.not. cond) then
         print "(a)", msg_
         stop 1
      end if
   end subroutine assert

   !> Error out if `a` and `b` are not within `tol` of each other.
   subroutine assert_close(a, b, tol)
      real(rk), intent(in) :: a, b
      real(rk), intent(in), optional :: tol  !< Absolute tolerance, defaults to TINY

      real(rk) :: diff, tol_

      if (.not. present(tol)) then
         tol_ = tiny(1.0_rk)
      else
         tol_ = tol
      end if

      diff = abs(a - b)
      if (diff > tol_) then
         print '("Closeness assertion failed: ", g9.4, " != ", g9.4)', a, b
         stop 1
      end if
   end subroutine assert_close

end module testing_mod
