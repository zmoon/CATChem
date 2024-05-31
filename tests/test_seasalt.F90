program test_dust
   use precision_mod, only: fp
   use Config_Opt_Mod, only: ConfigType
   use ChemState_Mod, only: ChemStateType
   use MetState_Mod, only: MetStateType
   use DiagState_Mod, only: DiagStateType
   use CCPr_SeaSalt_Common_Mod, only: SeaSaltStateType
   use CCPr_Scheme_GEOS12_Mod, only: CCPr_Scheme_GEOS12
   use CCPr_Scheme_Gong03_Mod, only: CCPr_Scheme_Gong03
   use CCPr_Scheme_Gong97_Mod, only: CCPr_Scheme_Gong97
   use CCPr_SeaSalt_mod, only: CCPr_SeaSalt_Init, CCPr_SeaSalt_Run, CCPr_SeaSalt_Finalize
   use Error_Mod, only: CC_Error, CC_SUCCESS
   use testing_mod, only: assert
   use GridState_Mod, only: GridStateType
   use Config_Mod
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(SeaSaltStateType) :: SeaSaltState
   type(GridStateType) :: GridState

   ! Integers
   INTEGER:: RC          ! Success or failure

   character(len=:), allocatable :: title

   ! Error handling
   CHARACTER(LEN=512) :: errMsg
   CHARACTER(LEN=255) :: thisLoc
   CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'

   ! set thisLoc
   thisLoc = 'test_dust -> at read CATChem_Conifg.yml'
   errMsg = ''
   RC = CC_SUCCESS

   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC   EEEEE  M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''

   !----------------------------
   ! Test 1
   !----------------------------

   ! Read input file and initialize grid
   call Read_Input_File(Config, GridState, RC)
   if (RC /= CC_success) then
      errMsg = 'Error reading configuration file: ' // TRIM( configFile )
      call CC_Error( errMsg, RC , thisLoc)
      stop 1
   endif
   title = 'SeaSalt Test 1 | Read Config'
   ! call print_info(Config, SeaSaltState, MetState, title)


   !----------------------------
   ! Test 2
   !----------------------------
   ! Set number of dust species to zero for now
   ChemState%nSpeciesSeaSalt = 0

   ! Meteorological State
   MetState%IsLand = .False.
   MetState%IsIce = .False.
   MetState%IsSnow = .False.
   MetState%U10M = 10.0_fp
   MetState%V10M = 10.0_fp
   MetState%USTAR = 5.0_fp
   MetState%USTAR_THRESHOLD = 0.1_fp
   allocate(MetState%AIRDEN(1))
   MetState%AIRDEN = 1.2_fp  ! kg/m3

   title = "SeaSalt Test 2 | Test GEOS12 defaults"
   SeaSaltState%SchemeOpt=3

   call CCPr_SeaSalt_Init(Config, SeaSaltState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_SeaSalt_Init'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call CCPr_SeaSalt_Run(MetState, DiagState, SeaSaltState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_SeaSalt_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call print_info(Config, SeaSaltState, MetState, title)
   call assert(DiagState%sea_salt_total_flux > 50.0_fp, "Test Fengsha SeaSalt Scheme")

contains

   subroutine print_info(Config_, SeaSaltState_, MetState_, title_)
      type(ConfigType), intent(in) :: Config_
      type(MetStateType), intent(in) :: MetState_
      type(SeaSaltStateType), intent(in) :: SeaSaltState_
      character(len=*), intent(in) :: title_

      write(*,*) '======================================='
      write(*,*) title_
      write(*,*) '======================================='
      write(*,*) '*************'
      write(*,*) 'Configuration '
      write(*,*) '*************'
      write(*,*) 'SeaSaltState%activate = ', SeaSaltState_%activate
      write(*,*) 'SeaSaltState%SchemeOpt = ', SeaSaltState_%SchemeOpt
      write(*,*) 'SeaSaltState%SeaSaltScaleFactor = ', SeaSaltState_%SeaSaltScaleFactor
      write(*,*) 'MetState%IsLand = ', MetState_%IsLand
      write(*,*) 'MetState%IsIce = ', MetState_%IsIce
      write(*,*) 'MetState%IsSnow = ', MetState_%IsSnow
      write(*,*) 'MetState%U10M =', MetState_%U10M
      write(*,*) 'MetState%V10M =', MetState_%V10M
      write(*,*) 'MetState%USTAR =', MetState_%USTAR
      write(*,*) 'MetState%AIRDEN =', MetState_%AIRDEN
      write(*,*) 'SeaSaltState%TotalEmission = ', SeaSaltState_%TotalEmission

   end subroutine print_info

end program test_dust
