program test_drydep
   use CATChem, fp => cc_rk
   use testing_mod, only: assert
   use precision_mod, only: rae
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(drydepStateType) :: DryDepState
   type(GridStateType) :: GridState
   type(EmisStateType) :: EmisState

   ! Integers
   INTEGER:: rc          ! Success or failure

   character(len=:), allocatable :: title
   integer :: i ! loop counter

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=255), PARAMETER :: configFile ='Configs/Default/CATChem_config.yml'


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
   call cc_read_config(Config, GridState, EmisState, ChemState, rc, configFile)
   if (rc /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   endif

   title = 'drydep Test 1 | Read Config'
    call print_info(Config, drydepState, MetState, title)


   !----------------------------
   ! Test 2
   !----------------------------
   ! Set number of drydep species to one for now
   ChemState%nSpeciesAerodrydep = 1

   ! Meteorological State
   MetState%LWI = 1
   MetState%USTAR = 0.5_fp
   MetState%PBLH = 1000.0_fp
   MetState%HFLUX = 0.5_fp
   MetState%Z0H = 0.01
   Metstate%NLEVS = 1
   Metstate%TSTEP = 60
   allocate(MetState%MAIRDEN(MetState%NLEVS))
   allocate(MetState%T(MetState%NLEVS))
   allocate(MetState%ZMID(MetState%NLEVS))
   gridstate%number_of_levels = MetState%NLEVS

   do i = 1, MetState%NLEVS
      MetState%T(i)=273.15         ! K
      MetState%MAIRDEN(i) = 1.12   ! kg/m3
      MetState%ZMID(i) = I*100   ! m
   end do

   DryDepState%SchemeOpt = 1
   ! Turn off resuspension
   DryDepState%Resuspension = .FALSE.

   title = "DryDep Test 2 | Test GOCART DryDep defaults"

   call cc_drydep_init(Config, DryDepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_init'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call cc_drydep_run(MetState, DiagState, DryDepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, DryDepState, MetState, title)
   call assert(DiagState%drydep_frequency(1) > 0.0_fp, "Test GOCART DryDep Scheme (no resuspension)")
   

   !----------------------------
   ! Test 3
   !----------------------------
   title = "drydep Test 3 | resuspension is .TRUE. "
   ChemState%nSpeciesAerodrydep = 1
   ! Turn on resuspension
   DryDepState%Resuspension = .TRUE.
   DryDepState%particleradius = 3.00
   DryDepState%particledensity = 2500.
   MetState%FRLAKE = 0
   MetState%GWETTOP = 0.01_fp
   MetState%U10m = 1.0_fp
   MetState%V10m = 1.0_fp


   call cc_drydep_run(MetState, DiagState, DryDepState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      errMsg = 'Error in cc_drydep_run'
      call cc_emit_error(errMsg, rc, thisLoc)
      stop 1
   end if

   call print_info(Config, DryDepState, MetState, title)
   call assert(rae(DiagState%drydep_frequency(1), 0.0_fp), "Test 2 GOCART drydep Scheme (resuspension activated)")



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
      write(*,*) 'Config%drydep_activate = ', Config_%drydep_activate
      write(*,*) 'drydepState%Activate = ', drydepState_%Activate
      write(*,*) 'drydepState%SchemeOpt = ', drydepState_%SchemeOpt
      write(*,*) 'MetState%GWETTOP =', MetState_%GWETTOP
      write(*,*) 'MetState%USTAR =', MetState_%USTAR
      write(*,*) 'MetState%MAIRDEN =', MetState_%MAIRDEN
      write(*,*) 'drydepState%drydepf = ', drydepState_%drydep_frequency

   end subroutine print_info

end program test_drydep
