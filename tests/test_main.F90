!> \\file test_main.F90


program test_main
   USE init_mod
   USE Error_Mod
   Use State_Mod
   USE Config_Mod

   IMPLICIT NONE

   ! Integers
   INTEGER:: RC          ! Success or failure

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
   ! set thisLoc
   thisLoc = 'test_main::test_main() -> at read CATChem_Conifg.yml'
   errMsg = ''
   RC = 0

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC    EEEE  M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   ! Read input file and initialize grid
   call Read_Input_File(Config_Opt, GRID, RC)
   if (RC /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call CC_Error( errMsg, RC , thisLoc)
   endif

   ! write grid info
   write(*,*) 'Grid info:'
   write(*,*) 'Number of grid nx = ', GRID%NX
   write(*,*) 'Number of grid ny = ', GRID%NY
   write(*,*) 'Number of grid levels = ', GRID%number_of_levels

   ! initialize met
   call Init_Met(GRID, MET, RC)
   if (RC /= CC_success) then
      errMsg = 'Error initializing meteorology'
      call CC_Error( errMsg, RC , thisLoc)
   endif

   write(*,*) 'Finished CATChem'
   ! call base_config_yaml_read(RC)

   ! read yaml file
   ! if (RC /= CC_success) then
   !    errMsg = 'Error reading configuration file: ' // TRIM( configFile )
   !    call CC_Error( errMsg, RC , thisLoc)
   !    return
   ! endif

   ! allocate arrays for current state
   ! call Init_State(Config_Opt, State_Grid, State_Met, RC)


end program test_main
