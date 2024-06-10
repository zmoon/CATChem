program test_dust
   use precision_mod, only: fp
   use Config_Opt_Mod, only: ConfigType
   use ChemState_Mod, only: ChemStateType
   use MetState_Mod, only: MetStateType
   use DiagState_Mod, only: DiagStateType
   use CCPr_Dust_Common_Mod, only: DustStateType
   use CCPr_Scheme_Fengsha_Mod, only: CCPr_Scheme_Fengsha
   use CCPr_Dust_mod, only: CCPr_Dust_Init, CCPr_Dust_Run, CCPr_Dust_Finalize
   use Error_Mod, only: CC_Error, CC_SUCCESS
   use testing_mod, only: assert
   use GridState_Mod, only: GridStateType
   use Config_Mod
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(DustStateType) :: DustState
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
   title = 'Dust Test 1 | Read Config'
   ! call print_info(Config, DustState, MetState, title)


   !----------------------------
   ! Test 2
   !----------------------------
   ! Set number of dust species to zero for now
   ChemState%nSpeciesDust = 0

   ! Meteorological State
   MetState%DSOILTYPE = 1
   MetState%SSM = 0.5_fp
   MetState%RDRAG = 0.8_fp
   MetState%TSKIN = 300._fp
   MetState%CLAYFRAC = 0.5_fp
   MetState%SANDFRAC = 0.5_fp
   MetState%GWETTOP = 0.01_fp
   MetState%USTAR = 0.5_fp
   MetState%USTAR_THRESHOLD = 0.1_fp
   allocate(MetState%AIRDEN(1))
   MetState%AIRDEN = 1.2_fp  ! kg/m3

   title = "Dust Test 2 | Test Fengsha defaults"

   call CCPr_Dust_Init(Config, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Init'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call print_info(Config, DustState, MetState, title)
   call assert(DiagState%dust_total_flux > 0.0_fp, "Test Fengsha Dust Scheme")


   !----------------------------
   ! Test 3
   !----------------------------
   title = "Dust Test 3 | ustar == ustar_threshold"
   MetState%USTAR = 0.1_fp

   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call print_info(Config, DustState, MetState, title)

   call assert(DiagState%dust_total_flux .eq. 0.0_fp, "Test 2 FENGSHA Dust Scheme (no Dust)")

   !------------------------------------------------------
   ! TEST 4
   !------------------------------------------------------
   title = "Dust Test 4 | test horizontal flux change"
   MetState%z0 = .001_fp
   DustState%HorizFluxOpt = 2
   MetState%USTAR = 0.5_fp
   MetState%U10M = 5.0_fp
   MetState%V10M = 5.0_fp
   DiagState%dust_total_flux = 0.0_fp

   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   call print_info(Config, DustState, MetState, title)

   call assert(DiagState%dust_total_flux >500.0_fp, "Test different horizontal flux")
   write(*,*) 'Test 3 Success!!!!!'

   call CCPr_Dust_Finalize(DustState, RC)
   if (RC /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Finalize'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   endif

   !----------------------------
   ! Test 5
   !----------------------------
   title = "Dust Test 5 | Test Ginoux Scheme"
   Config%dust_activate = .true.
   Config%dust_scheme = 2
   Config%dust_drag_opt = 1
   Config%dust_moist_opt = 1
   Config%dust_horizflux_opt = 1
   Config%dust_alpha = 1
   MetState%U10M = 5.0_fp
   MetState%V10M = 5.0_fp

   call CCPr_Dust_Init(Config, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Init'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if



   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if
   call print_info(Config, DustState, MetState, title)
   call assert(DiagState%dust_total_flux > 700.0_fp, "Test Ginoux Dust Scheme Success")

contains

   subroutine print_info(Config_, DustState_, MetState_, title_)
      type(ConfigType), intent(in) :: Config_
      type(MetStateType), intent(in) :: MetState_
      type(DustStateType), intent(in) :: DustState_
      character(len=*), intent(in) :: title_

      write(*,*) '======================================='
      write(*,*) title_
      write(*,*) '======================================='
      write(*,*) '*************'
      write(*,*) 'Configuration '
      write(*,*) '*************'
      write(*,*) 'DustState%activate = ', DustState_%activate
      write(*,*) 'DustState%dust_scheme = ', DustState_%SchemeOpt
      write(*,*) 'DustState%dust_moist_opt = ', DustState_%DragOpt
      write(*,*) 'DustState%dust_horizflux_opt = ', DustState_%MoistOpt
      write(*,*) 'DustState%AlphaScaleFactor = ', DustState_%AlphaScaleFactor
      write(*,*) 'ChemState%nSpeciesDust = ', DustState_%HorizFluxOpt
      write(*,*) 'MetState%DSOILTYPE = ', MetState_%DSOILTYPE
      write(*,*) 'MetState%SSM = ', MetState_%SSM
      write(*,*) 'MetState%RDRAG = ', MetState_%RDRAG
      write(*,*) 'MetState%TSKIN =', MetState_%TSKIN
      write(*,*) 'MetState%CLAYFRAC =', MetState_%CLAYFRAC
      write(*,*) 'MetState%SANDFRAC =', MetState_%SANDFRAC
      write(*,*) 'MetState%GWETTOP =', MetState_%GWETTOP
      write(*,*) 'MetState%USTAR =', MetState_%USTAR
      write(*,*) 'MetState%USTAR_THRESHOLD =', MetState_%USTAR_THRESHOLD
      write(*,*) 'MetState%AIRDEN =', MetState_%AIRDEN
      write(*,*) 'DustState%TotalEmission = ', DustState_%TotalEmission

   end subroutine print_info

end program test_dust
