program test_plumerise
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   use state_mod

   implicit none

   integer, parameter :: u = 99  !< Unit number for loading column data

   integer :: i
   integer :: ios  !< File I/O status
   integer :: n  !< Size of data to load
   character(len=255) :: vn  !< Variable name

   i = 0
   vn = ""
   n = 0

   ! Load the data
   open(unit=u, file="col.csv", iostat=ios)
   if (ios /= 0) error stop
   do
      if (mod(i, 2) == 0) then
         ! Variable info
         read(u, *, iostat=ios) vn, n
         if (ios /= 0) exit
      else
         ! Data
         print *, "Reading: " // trim(vn)
         select case (vn)
          case ("gwetroot")
            read(u, *, iostat=ios) MetState%GWETROOT
            if (ios /= 0) exit
          case ("gwettop")
            read(u, *, iostat=ios) MetState%GWETTOP
            if (ios /= 0) exit
          case ("hflux")
            read(u, *, iostat=ios) MetState%HFLUX
            if (ios /= 0) exit
          case ("pblh")
            read(u, *, iostat=ios) MetState%PBLH
            if (ios /= 0) exit
          case ("pmid")
            allocate(MetState%PMID(n))
            read(u, *, iostat=ios) MetState%PMID
            if (ios /= 0) exit
          case ("ps")
            read(u, *, iostat=ios) MetState%PS
            if (ios /= 0) exit
          case ("qv")
            allocate(MetState%QV(n))
            read(u, *, iostat=ios) MetState%QV
            if (ios /= 0) exit
          case ("swgdn")
            read(u, *, iostat=ios) MetState%SWGDN
            if (ios /= 0) exit
          case ("t")
            allocate(MetState%T(n))
            read(u, *, iostat=ios) MetState%T
            if (ios /= 0) exit
          case ("t2m")
            read(u, *, iostat=ios) MetState%T2M
            if (ios /= 0) exit
          case ("u")
            allocate(MetState%U(n))
            read(u, *, iostat=ios) MetState%U
            if (ios /= 0) exit
          case ("u10m")
            read(u, *, iostat=ios) MetState%U10M
            if (ios /= 0) exit
          case ("ustar")
            read(u, *, iostat=ios) MetState%USTAR
            if (ios /= 0) exit
          case ("v")
            allocate(MetState%V(n))
            read(u, *, iostat=ios) MetState%V
            if (ios /= 0) exit
          case ("v10m")
            read(u, *, iostat=ios) MetState%V10M
            if (ios /= 0) exit
          case ("z")
            allocate(MetState%Z(n))
            read(u, *, iostat=ios) MetState%Z
            if (ios /= 0) exit
          case ("zmid")
            allocate(MetState%ZMID(n))
            read(u, *, iostat=ios) MetState%ZMID
            if (ios /= 0) exit
          case default
            print *, "Unknown variable name: " // trim(vn)
            error stop
         end select
      end if
      i = i + 1
   end do
   print *, "GWETROOT:", MetState%GWETROOT
   print *, "GWETTOP:", MetState%GWETTOP
   print *, "HFLUX:", MetState%HFLUX
   print *, "PBLH:", MetState%PBLH
   print *, "PMID:", MetState%PMID
   print *, "PS:", MetState%PS
   print *, "QV:", MetState%QV
   print *, "SWGDN:", MetState%SWGDN
   print *, "T:", MetState%T
   print *, "T2M:", MetState%T2M
   print *, "U:", MetState%U
   print *, "U10M:", MetState%U10M
   print *, "USTAR:", MetState%USTAR
   print *, "V:", MetState%V
   print *, "V10M:", MetState%V10M
   print *, "Z:", MetState%Z
   print *, "ZMID:", MetState%ZMID

end program test_plumerise
