program test_drydep
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(drydepStateType) :: DryDepState
   type(GridStateType) :: GridState

   ! Integers
   INTEGER:: rc          ! Success or failure

   character(len=:), allocatable :: title

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=255), PARAMETER :: configFile ='Configs/Plumerise/CATChem_config.yml'


   thisLoc = 'test_drydep -> at read CATChem_Config.yml'
   errMsg = ''
   rc = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC   EEEE   M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   !----------------------------
   ! Test 1
   !----------------------------

   ! Read input file and initialize grid
   call cc_read_config(Config, GridState, rc)
   if (rc /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   title = 'drydep Test 1 | Read Config'
   ! call print_info(Config, drydepState, MetState, title)


   !----------------------------
   ! Test 2
   !----------------------------
   ! Set number of drydep species to zero for now
   ChemState%nSpeciesAerodrydep = 0

   ! Meteorological State
   MetState%LWI = 1.0_fp
   MetState%USTAR = 0.5_fp
   MetState%PBLH = 1000.0_fp
   MetState%HFLUX = 0.5_fp
   MetState%Z0H = 10.0_fp
   allocate(MetState%MAIRDEN(1))
   Metstate%NLEVS = 1
   Metstate%TSTEP = 60

   do i = 1, gridstate%number_of_levels
      metstate%T(i)=273.15_fp          ! K
      MetState%MAIRDEN(i) = 1.2_fp  ! kg/m3
      MetState%PHIS(i) = I*100   ! m   !this is technically supposed to be PHIT???
   end do


   ! Turn off resuspension
   DryDepState%RESUSPENSION = .FALSE.

   title = "DryDep Test 2 | Test GOCART DryDep defaults"

   call cc_drydep_init(Config, drydepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_init'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call cc_drydep_run(MetState, DiagState, drydepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, drydepState, MetState, title)
   call assert(DiagState%drydepf > 0.0_fp, "Test GOCART DryDep Scheme (no resuspension)")


   !----------------------------
   ! Test 3
   !----------------------------
   title = "drydep Test 3 | resuspension is .TRUE. "
   ! Turn on resuspension
   DryDepState%RESUSPENSION = .TRUE.
   DryDepState%particleradius = 0.0001_fp
   DryDepState%particledensity = 1.00
   MetState%FRLAKE = 0
   MetState%GWETTOP = 0.01_fp
   metstate%U10m = 1.0_fp
   metstate%V10m = 1.0_fp


   call cc_drydep_run(MetState, DiagState, drydepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, drydepState, MetState, title)
   call assert(DiagState%drydepf .eq. 0.0_fp, "Test 2 GOCART drydep Scheme (resuspension activated)")



contains

   subroutine print_info(Config_, drydepState_, MetState_, title_)
      type(ConfigType), intent(in) :: Config_
      type(MetStateType), intent(in) :: MetState_
      type(drydepStateType), intent(in) :: drydepState_
      character(len=*), intent(in) :: title_

      write(*,*) '======================================='
      write(*,*) title_
      write(*,*) '======================================='
      write(*,*) '*************'
      write(*,*) 'Configuration '
      write(*,*) '*************'
      write(*,*) 'drydepState%activate = ', drydepState_%activate
      write(*,*) 'drydepState%drydep_scheme = ', drydepState_%SchemeOpt
      write(*,*) 'MetState%GWETTOP =', MetState_%GWETTOP
      write(*,*) 'MetState%USTAR =', MetState_%USTAR
      write(*,*) 'MetState%MAIRDEN =', MetState_%MAIRDEN
      write(*,*) 'drydepState%DryDepf = ', drydepState_%drydepf

   end subroutine print_info

end program test_drydep
