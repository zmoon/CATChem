program test_plumerise
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   use state_mod

   implicit none

   integer, parameter :: u = 99

   integer :: i, ios, n
   character(len=255) :: vn

   i = 0
   vn = ""
   n = 0

   ! Load the data
   open(unit=u, file="col.csv", iostat=ios)
   if (ios /= 0) error stop
   do
      if (mod(i, 2) == 0) then
         ! Var info
         read(u, *, iostat=ios) vn, n
         if (ios /= 0) exit
      else
         ! Data
         print *, "Reading: " // trim(vn)
         select case (vn)
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
          case ("ustar")
            read(u, *, iostat=ios) MetState%USTAR
            if (ios /= 0) exit
          case ("v")
            allocate(MetState%V(n))
            read(u, *, iostat=ios) MetState%V
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
   print *, MetState%HFLUX
   print *, MetState%PBLH
   print *, MetState%PMID
   print *, MetState%PS
   print *, MetState%QV
   print *, MetState%T
   print *, MetState%T2M
   print *, MetState%U
   print *, MetState%USTAR
   print *, MetState%V
   print *, MetState%Z
   print *, MetState%ZMID

end program test_plumerise
