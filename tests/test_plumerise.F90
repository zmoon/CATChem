program test_plumerise
   use CATChem, fp => cc_rk
   use testing_mod, only: assert, load_column_data
   use state_mod

   implicit none

   type(PlumeriseStateType) :: PlumeriseState

   integer :: rc          ! Success or failure
   integer :: c, s, l

   CHARACTER(LEN=255), PARAMETER :: configFile ='Configs/Plumerise/CATChem_config.yml'

   ! Error handling
   CHARACTER(LEN=255) :: errMsg
   CHARACTER(LEN=255) :: thisLoc

   thisLoc = 'test_plumerise -> at read CATChem_Config.yml'
   errMsg = ''
   rc = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC   EEEE   M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) '  PLUMERISE TEST'

   ! Read input file and initialize grid
   call cc_read_config(Config, GridState, EmisState, ChemState, rc, configFile)
   if (rc /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Allocate MetState
   MetState%nSOIL = 4
   print*, 'Allocating MetState'
   call cc_allocate_metstate(GridState, MetState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_allocate_metstate"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif
   print*, 'Allocated MetState'

   ! Meteorological State
   call load_column_data("MetProfiles/Profile_NCWCP.csv", MetState, rc)

   ! Allocate EmisState with FRP
   do c = 1, EmisState%nCats
      if (EmisState%Cats(c)%nPlumerise /= 0) then
         do s = 1, EmisState%Cats(c)%nSpecies
            if (EmisState%Cats(c)%Species(s)%plumerise == 1) then

               EmisState%Cats(c)%Species(s)%nPlmSrc = 4
               allocate(EmisState%Cats(c)%Species(s)%PlmSrcFlx(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%FRP(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               EmisState%Cats(c)%Species(s)%FRP = (/10.0e6, 100.0e6, 500.0e6, 1000.0e6/)
               EmisState%Cats(c)%Species(s)%PlmSrcFlx = 0.0

            else if (EmisState%Cats(c)%Species(s)%plumerise == 2) then

               EmisState%Cats(c)%Species(s)%nPlmSrc = 1
               allocate(EmisState%Cats(c)%Species(s)%PlmSrcFlx(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKDM(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKHT(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKTK(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               allocate(EmisState%Cats(c)%Species(s)%STKVE(EmisState%Cats(c)%Species(s)%nPlmSrc), stat=rc)
               if (rc /= CC_success) then
                  errMsg = 'Error in "cc_allocate_emisstate"'
                  call cc_emit_error(errMsg, rc, thisLoc)
                  stop 1
               endif
               EmisState%Cats(c)%Species(s)%STKDM = (/30.0/)
               EmisState%Cats(c)%Species(s)%STKHT = (/10.0/)
               EmisState%Cats(c)%Species(s)%STKTK = (/273.0/)
               EmisState%Cats(c)%Species(s)%STKVE = (/10.0/)

            endif
         enddo
      endif
   enddo

   ! Plumerise init
   call cc_plumerise_init(PlumeriseState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_plumerise_init"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   ! Plumerise run
   call cc_plumerise_run(PlumeriseState, rc, verbose_opt=.true.)
   if (rc /= CC_success) then
      errMsg = 'Error in "cc_plumerise_run"'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

end program test_plumerise
