module testing_mod
   use iso_fortran_env, only: rk => real32
   implicit none

   private
   public :: assert
   public :: assert_close
   public :: load_column_data

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

   subroutine load_column_data(filename, &
      HFLUX,   &
      PBLH,    &
      PMID,    &
      PS,      &
      QV,      &
      T,       &
      T2M,     &
      U,       &
      USTAR,   &
      V,       &
      Z,       &
      ZMID)

      implicit none

      character(len=*), intent(in) :: filename
      real, intent(inout) :: HFLUX
      real, intent(inout) :: PBLH
      real, intent(inout), ALLOCATABLE :: PMID(:)
      real, intent(inout) :: PS
      real, intent(inout), ALLOCATABLE :: QV(:)
      real, intent(inout), ALLOCATABLE :: T(:)
      real, intent(inout) :: T2M
      real, intent(inout), ALLOCATABLE :: U(:)
      real, intent(inout) :: USTAR
      real, intent(inout), ALLOCATABLE :: V(:)
      real, intent(inout), ALLOCATABLE :: Z(:)
      real, intent(inout), ALLOCATABLE :: ZMID(:)

      ! Local Variables
      integer, parameter :: unum = 99  !< Unit number for loading column data
      integer :: i
      integer :: ios  !< File I/O status
      integer :: n  !< Size of data to load
      character(len=255) :: vn  !< Variable name

      i = 0
      vn = ""
      n = 0

      ! Load the data
      open(unit=unum, file=TRIM(filename), iostat=ios)
      if (ios /= 0) error stop
      do
         if (mod(i, 2) == 0) then
            ! Variable info
            read(unum, *, iostat=ios) vn, n
            print*,  vn, n
            if (ios /= 0) exit
         else
            ! Data
            print *, "Reading: " // trim(vn)
            select case (vn)
             case ("hflux")
               read(unum, *, iostat=ios) HFLUX
               if (ios /= 0) exit
             case ("pblh")
               read(unum, *, iostat=ios) PBLH
               if (ios /= 0) exit
             case ("pmid")
               allocate(PMID(n))
               read(unum, *, iostat=ios) PMID
               if (ios /= 0) exit
             case ("ps")
               read(unum, *, iostat=ios) PS
               if (ios /= 0) exit
             case ("qv")
               allocate(QV(n))
               read(unum, *, iostat=ios) QV
               if (ios /= 0) exit
             case ("t")
               allocate(T(n))
               read(unum, *, iostat=ios) T
               if (ios /= 0) exit
             case ("t2m")
               read(unum, *, iostat=ios) T2M
               if (ios /= 0) exit
             case ("u")
               allocate(U(n))
               read(unum, *, iostat=ios) U
               if (ios /= 0) exit
             case ("ustar")
               read(unum, *, iostat=ios) USTAR
               if (ios /= 0) exit
             case ("v")
               allocate(V(n))
               read(unum, *, iostat=ios) V
               if (ios /= 0) exit
             case ("z")
               allocate(Z(n))
               read(unum, *, iostat=ios) Z
               if (ios /= 0) exit
             case ("zmid")
               allocate(ZMID(n))
               read(unum, *, iostat=ios) ZMID
               if (ios /= 0) exit
             case default
               print *, "Unknown variable name: " // trim(vn)
               error stop
            end select
         end if
         i = i + 1
      end do
      print *, '==================================================='
      print *, "Column data loaded"
      print *, "==================================================="
      print *, "HFLUX:", HFLUX
      print *, "PBLH:", PBLH
      print *, "PMID:", PMID
      print *, "PS:", PS
      print *, "QV:", QV
      print *, "T:", T
      print *, "T2M:", T2M
      print *, "U:", U
      print *, "USTAR:", USTAR
      print *, "V:", V
      print *, "Z:", Z
      print *, "ZMID:", ZMID

   end subroutine load_column_data

end module testing_mod
