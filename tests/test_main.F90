!> \\file test_main.F90


program test_main
   USE init_mod
   IMPLICIT NONE

   ! Integers
   INTEGER:: RC          ! Success or failure

   call base_config_yaml_read(RC)

   if (RC /= 0) then
      stop 1
   end if

end program test_main
