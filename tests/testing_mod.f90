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

   subroutine load_column_data(filename, MetState, rc)
      use MetState_Mod, only: MetStateType

      implicit none

      type(MetStateType), intent(inout) :: MetState

      character(len=*), intent(in) :: filename

      integer, intent(inout) :: rc

      ! Local Variables
      integer, parameter :: unum = 99  !< Unit number for loading column data
      integer :: i
      integer :: ios  !< File I/O status
      character(len=255) :: isomsg
      integer :: n  !< Size of data to load
      real :: tmpReal !< Temporary integer
      character(len=10) :: tmpStr
      character(len=255) :: vn  !< Variable name
      character(len=8) :: fmt ! format descriptor
      character(len=3) :: tmp
      fmt = '(I3.3)'

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
            write(tmp, fmt) n
            print*, "Reading: " // trim(vn) // " | size: " // trim(tmp)
            if (ios /= 0) return
         else
            ! Data
            ! print *, "Reading: " // trim(vn)
            select case (vn)
             case ("hflux")
               read(unum, *, iostat=ios) MetState%HFLUX
               if (ios /= 0) then
                  print *, "Error reading HFLUX: ", ios
                  return
               end if
             case ("pblh")
               read(unum, *, iostat=ios) MetState%PBLH
               if (ios /= 0) then
                  print *, "Error reading PBLH: ", ios
                  return
               end if
             case ("pmid")
               read(unum, *, iostat=ios) MetState%PMID
               if (ios /= 0) then
                  print *, "Error reading PMID: ", ios
                  return
               end if
             case ("ps")
               read(unum, *, iostat=ios) MetState%PS
               if (ios /= 0) then
                  print *, "Error reading PS: ", ios
                  return
               end if
             case ("qv")
               read(unum, *, iostat=ios) MetState%QV
               if (ios /= 0) then
                  print *, "Error reading QV: ", ios
                  return
               end if
             case ("t")
               read(unum, *, iostat=ios) MetState%T
               if (ios /= 0) then
                  print *, "Error reading T: ", ios
                  return
               end if
             case ("t2m")
               read(unum, *, iostat=ios) MetState%T2M
               if (ios /= 0) then
                  print *, "Error reading T2M: ", ios
                  return
               end if
             case ("u")
               read(unum, *, iostat=ios) MetState%U
               if (ios /= 0) then
                  print *, "Error reading U: ", ios
                  return
               end if
             case ("ustar")
               read(unum, *, iostat=ios) MetState%USTAR
               if (ios /= 0) then
                  print *, "Error reading USTAR: ", ios
                  return
               end if
             case ("v")
               read(unum, *, iostat=ios) MetState%V
               if (ios /= 0) then
                  print *, "Error reading V: ", ios
                  return
               end if
             case ("z")
               read(unum, *, iostat=ios) MetState%Z
               if (ios /= 0) then
                  print *, "Error reading Z: ", ios
                  return
               end if
             case ("zmid")
               read(unum, *, iostat=ios) MetState%ZMID
               if (ios /= 0) then
                  print *, "Error reading ZMID: ", ios
                  return
               end if
             case ("wilt")
               read(unum, *, iostat=ios) MetState%WILT
               if (ios /= 0) then
                  print *, "Error reading WILT: ", ios
                  return
               end if
             case ("weasd")
               read(unum, *, iostat=ios) MetState%SNOMAS
               if (ios /= 0) then
                  print *, "Error reading SNOMAS: ", ios
                  return
               end if
             case ("lwi")
               read(unum, *, iostat=ios) tmpReal
               MetState%LWI = int(tmpReal)
               if (ios /= 0) then
                  print *, "Error reading DLUSE: ", ios
                  return
               end if
             case ("vtype")
               read(unum, *, iostat=ios) tmpReal
               MetState%DLUSE = int(tmpReal)
               if (ios /= 0) then
                  print *, "Error reading DLUSE: ", ios
                  return
               end if
             case ("vfrac")
               read(unum, *, iostat=ios) MetState%GVF
               if (ios /= 0) then
                  print *, "Error reading GVF: ", ios
                  return
               end if
             case ("v10m")
               read(unum, *, iostat=ios) MetState%V10M
               if (ios /= 0) then
                  print *, "Error reading V10M: ", ios
                  return
               end if
             case ("u10m")
               read(unum, *, iostat=ios) MetState%U10M
               if (ios /= 0) then
                  print *, "Error reading U10M: ", ios
                  return
               end if
             case ("lhtfl")
               read(unum, *, iostat=ios) MetState%EFLUX ! [W/m^2]
               if (ios /= 0) then
                  print *, "Error reading EFLUX: ", ios
                  return
               end if
             case ("z0")
               read(unum, *, iostat=ios) MetState%Z0
               if (ios /= 0) then
                  print *, "Error reading Z0: ", ios
                  return
               end if
             case ("soilw4")
               read(unum, *, iostat=ios) MetState%SOILM(4)
               if (ios /= 0) then
                  print *, "Error reading SOILM: ", ios
                  return
               end if
             case ("soilw3")
               read(unum, *, iostat=ios) MetState%SOILM(3)
               if (ios /= 0) then
                  print *, "Error reading SOILM: ", ios
                  return
               end if
             case ("soilw2")
               read(unum, *, iostat=ios) MetState%SOILM(2)
               if (ios /= 0) then
                  print *, "Error reading SOILM: ", ios
                  return
               end if
             case ("soilw1")
               read(unum, *, iostat=ios) MetState%SOILM(1)
               if (ios /= 0) then
                  print *, "Error reading SOILM: ", ios
                  return
               end if
             case ("cldfra")
               read(unum, *, iostat=ios) MetState%CLDFRC
               if (ios /= 0) then
                  print *, "Error reading CLDFRC: ", ios
                  return
               end if
             case ("tmpsfc")
               read(unum, *, iostat=ios) MetState%TS
               if (ios /= 0) then
                  print *, "Error reading TS: ", ios
                  return
               end if
             case ("sotyp")
               read(unum, *, iostat=ios) tmpReal
               MetState%DSOILTYPE = int(tmpReal)
               if (ios /= 0) then
                  print *, "Error reading DSOILTYPE: ", ios
                  return
               end if
             case ("snowc")
               read(unum, *, iostat=ios) MetState%FRSNO
               if (ios /= 0) then
                  print *, "Error reading FRSNO: ", ios
                  return
               end if
             case ("pwat")
               read(unum, *, iostat=ios) MetState%PRECTOT
               if (ios /= 0) then
                  print *, "Error reading PRECTOT: ", ios
                  return
               end if
             case ("sphu")
               read(unum, *, iostat=ios) MetState%SPHU
               if (ios /= 0) then
                  print *, "Error reading SPHU: ", ios
                  return
               end if
             case ("delp")
               read(unum, *, iostat=ios) MetState%DELP
               if (ios /= 0) then
                  print *, "Error reading DELP: ", ios
                  return
               end if
             case ("delz")
               read(unum, *, iostat=ios) MetState%BXHEIGHT
               if (ios /= 0) then
                  print *, "Error reading BXHEIGHT: ", ios
                  return
               end if
             case ("snowd")
               read(unum, iostat=ios) tmpStr
               write(*,*) tmpReal
               MetState%SNODP = tmpReal
               if (ios /= 0) then
                  print *, "Error reading SNODP: ", ios
                  return
               end if
             case default
               print *, "Variable from file not used: " // trim(vn)
               ! error stop
            end select
         end if
         i = i + 1
      end do
      ! print *, '==================================================='
      ! print *, "Column data loaded"
      ! print *, "==================================================="
      ! print *, "HFLUX:", MetState%HFLUX
      ! print *, "PBLH:", MetState%PBLH
      ! print *, "PMID:", MetState%PMID
      ! print *, "PS:", MetState%PS
      ! print *, "QV:", MetState%QV
      ! print *, "T:", MetState%T
      ! print *, "T2M:", MetState%T2M
      ! print *, "U:", MetState%U
      ! print *, "USTAR:", MetState%USTAR
      ! print *, "V:", MetState%V
      ! print *, "Z:", MetState%Z
      ! print *, "ZMID:", MetState%ZMID
      ! print *, "WILT:", MetState%WILT
      ! print *, "SNOMAS:", MetState%SNOMAS
      ! print *, "VTYPE:", MetState%DLUSE
      ! print *, "VFRAC:", MetState%GVF
      ! print *, "V10M:", MetState%V10M
      ! print *, "U10M:", MetState%U10M
      ! print *, "PARDF:", MetState%PARDF
      ! print *, "PARDR:", MetState%PARDR
      ! print *, "EFLUX:", MetState%EFLUX
      ! print *, "Z0:", MetState%Z0
      ! print *, "SOILM:", MetState%SOILM
      ! print *, "CLDFRC:", MetState%CLDFRC
      ! print *, "TS:", MetState%TS
      ! print *, "SOILTYP:", MetState%DSOILTYPE
      ! print *, "FRSNO:", MetState%FRSNO
      ! print *, "PRECTOT:", MetState%PRECTOT
      ! print *, "==================================================="

   end subroutine load_column_data

end module testing_mod
