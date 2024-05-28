module testing_mod
   use iso_fortran_env, only: rk => real32
   implicit none

   private
   public :: assert, assert_close

contains

   subroutine assert(cond, msg)
      logical, intent(in) :: cond
      character(len=*), intent(in), optional :: msg

      character(len=:), allocatable :: msg_

      if (.not. present(msg)) then
         msg_ = 'Condition is false'
      else
         msg_ = msg
      end if

      if (.not. cond) then
         print '("Error: ", a)', msg
         stop 1
      end if
   end subroutine assert

   subroutine assert_close(a, b, tol)
      real(rk), intent(in) :: a, b
      real(rk), intent(in), optional :: tol  !> Absolute tolerance, defaults to TINY

      real(rk) :: diff, tol_

      if (.not. present(tol)) then
         tol_ = tiny(1.0_rk)
      else
         tol_ = tol
      end if

      diff = abs(a - b)
      if (diff > tol) then
         print '("Error: ", g8.3, " != ", g8.3)', a, b
         stop 1
      end if
   end subroutine assert_close

end module testing_mod
