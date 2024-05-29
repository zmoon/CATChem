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
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(DustStateType) :: DustState

   ! Error Handling variables
   integer :: rc
   CHARACTER(LEN=255) :: ErrMsg, thisLoc


   !error handling
   rc = CC_SUCCESS
   ErrMsg = ''
   thisLoc = ' -> at main (in tests/test_dust.f90)'


   write(*,*) '   CCCCC      A     TTTTTTT   CCCCC  H'
   write(*,*) '  C          A A       T     C       H       CCCC    EEEE  M       M'
   write(*,*) '  C         AAAAA      T     C       HHHHH  C      E    E  M M   M M'
   write(*,*) '  C        A     A     T     C       H   H  C      E EE    M   M   M'
   write(*,*) '   CCCCC  A       A    T      CCCCC  H   H   CCCC   EEEEE  M       M'
   write(*,*) ''
   write(*,*) ''
   ! Dust Test 1
   !------------

   ! Configuration Options
   !----------------------
   Config%dust_activate = .true.
   Config%dust_scheme = 1
   Config%dust_drag_opt = 1
   Config%dust_moist_opt = 1
   Config%dust_horizflux_opt = 1

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

   write(*,*) '======================================='
   write(*,*) '        Dust Test 1'
   write(*,*) '======================================='
   write(*,*)
   write(*,*) '*************'
   write(*,*) 'Configuration '
   write(*,*) '*************'
   write(*,*) 'DustState%activate = ', Config%dust_activate
   write(*,*) 'DustState%dust_scheme = ', Config%dust_scheme
   write(*,*) 'DustState%dust_moist_opt = ', Config%dust_drag_opt
   write(*,*) 'DustState%dust_horizflux_opt = ', Config%dust_moist_opt
   write(*,*) 'ChemState%nSpeciesDust = ', Config%dust_horizflux_opt
   write(*,*) 'MetState%DSOILTYPE = ', MetState%DSOILTYPE
   write(*,*) 'MetState%SSM = ', MetState%SSM
   write(*,*) 'MetState%RDRAG = ', MetState%RDRAG
   write(*,*) 'MetState%TSKIN =', MetState%TSKIN
   write(*,*) 'MetState%CLAYFRAC =', MetState%CLAYFRAC
   write(*,*) 'MetState%SANDFRAC =', MetState%SANDFRAC
   write(*,*) 'MetState%GWETTOP =', MetState%GWETTOP
   write(*,*) 'MetState%USTAR =', MetState%USTAR
   write(*,*) 'MetState%USTAR_THRESHOLD =', MetState%USTAR_THRESHOLD
   write(*,*) 'MetState%AIRDEN =', MetState%AIRDEN


   call CCPr_Dust_Init(Config, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Init'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if
   DustState%AlphaScaleFactor = 1.0_fp

   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if
   ! Expected Result = 2.78159837E-06
   write(*,*) 'dust_total_flux = ', DiagState%dust_total_flux
   write(*,*) 'Test 1 Success!!!!!'

   ! Dust Test 2
   !------------
   MetState%USTAR = 0.1_fp
   write(*,*) '======================================='
   write(*,*) '        Dust Test 2'
   write(*,*) '======================================='
   write(*,*)
   write(*,*) '*************'
   write(*,*) 'Configuration '
   write(*,*) '*************'
   write(*,*) 'DustState%activate = ', Config%dust_activate
   write(*,*) 'DustState%dust_scheme = ', Config%dust_scheme
   write(*,*) 'DustState%dust_moist_opt = ', Config%dust_drag_opt
   write(*,*) 'DustState%dust_horizflux_opt = ', Config%dust_moist_opt
   write(*,*) 'ChemState%nSpeciesDust = ', Config%dust_horizflux_opt
   write(*,*) 'MetState%DSOILTYPE = ', MetState%DSOILTYPE
   write(*,*) 'MetState%SSM = ', MetState%SSM
   write(*,*) 'MetState%RDRAG = ', MetState%RDRAG
   write(*,*) 'MetState%TSKIN =', MetState%TSKIN
   write(*,*) 'MetState%CLAYFRAC =', MetState%CLAYFRAC
   write(*,*) 'MetState%SANDFRAC =', MetState%SANDFRAC
   write(*,*) 'MetState%GWETTOP =', MetState%GWETTOP
   write(*,*) 'MetState%USTAR =', MetState%USTAR
   write(*,*) 'MetState%USTAR_THRESHOLD =', MetState%USTAR_THRESHOLD
   write(*,*) 'MetState%AIRDEN =', MetState%AIRDEN
   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   ! Expected Result = 0
   write(*,*) 'dust_total_flux = ', DiagState%dust_total_flux
   write(*,*) 'Test 2 Success!!!!!'

   ! Dust Test 3 - Test horizontal flux change
   MetState%z0 = .001_fp
   DustState%HorizFluxOpt = 2
   MetState%USTAR = 0.5_fp
   MetState%U10M = 5.0_fp
   MetState%V10M = 5.0_fp
   DiagState%dust_total_flux = 0.0_fp
   write(*,*) '======================================='
   write(*,*) '        Dust Test 3'
   write(*,*) '======================================='
   write(*,*)
   write(*,*) '*************'
   write(*,*) 'Configuration '
   write(*,*) '*************'
   write(*,*) 'DustState%activate = ', Config%dust_activate
   write(*,*) 'DustState%dust_scheme = ', Config%dust_scheme
   write(*,*) 'DustState%dust_moist_opt = ', Config%dust_drag_opt
   write(*,*) 'DustState%dust_horizflux_opt = ', Config%dust_moist_opt
   write(*,*) 'ChemState%nSpeciesDust = ', Config%dust_horizflux_opt
   write(*,*) 'MetState%DSOILTYPE = ', MetState%DSOILTYPE
   write(*,*) 'MetState%SSM = ', MetState%SSM
   write(*,*) 'MetState%RDRAG = ', MetState%RDRAG
   write(*,*) 'MetState%TSKIN =', MetState%TSKIN
   write(*,*) 'MetState%CLAYFRAC =', MetState%CLAYFRAC
   write(*,*) 'MetState%SANDFRAC =', MetState%SANDFRAC
   write(*,*) 'MetState%GWETTOP =', MetState%GWETTOP
   write(*,*) 'MetState%USTAR =', MetState%USTAR
   write(*,*) 'MetState%USTAR_THRESHOLD =', MetState%USTAR_THRESHOLD
   write(*,*) 'MetState%AIRDEN =', MetState%AIRDEN
   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)
   if (rc /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Run'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   end if

   write(*,*) 'dust_total_flux = ', DiagState%dust_total_flux
   write(*,*) 'Test 3 Success!!!!!'

   call CCPr_Dust_Finalize(DustState, RC)
   if (RC /= CC_SUCCESS) then
      ErrMsg = 'Error in CCPr_Dust_Finalize'
      call CC_Error( ErrMsg, rc, thisLoc )
      stop 1
   endif

   ! Change Dust scheme
   ! ------------------
   Config%dust_activate = .true.
   Config%dust_scheme = 2
   Config%dust_drag_opt = 1
   Config%dust_moist_opt = 1
   Config%dust_horizflux_opt = 1
   MetState%U10M = 5.0_fp
   MetState%V10M = 5.0_fp
   write(*,*) '======================================='
   write(*,*) '        Dust Test 4'
   write(*,*) '======================================='
   write(*,*)
   write(*,*) '*************'
   write(*,*) 'Configuration '
   write(*,*) '*************'
   write(*,*) 'DustState%activate = ', Config%dust_activate
   write(*,*) 'DustState%dust_scheme = ', Config%dust_scheme
   write(*,*) 'DustState%dust_moist_opt = ', Config%dust_drag_opt
   write(*,*) 'DustState%dust_horizflux_opt = ', Config%dust_moist_opt
   write(*,*) 'ChemState%nSpeciesDust = ', Config%dust_horizflux_opt
   write(*,*) 'MetState%DSOILTYPE = ', MetState%DSOILTYPE
   write(*,*) 'MetState%SSM = ', MetState%SSM
   write(*,*) 'MetState%RDRAG = ', MetState%RDRAG
   write(*,*) 'MetState%TSKIN =', MetState%TSKIN
   write(*,*) 'MetState%CLAYFRAC =', MetState%CLAYFRAC
   write(*,*) 'MetState%SANDFRAC =', MetState%SANDFRAC
   write(*,*) 'MetState%GWETTOP =', MetState%GWETTOP
   write(*,*) 'MetState%USTAR =', MetState%USTAR
   write(*,*) 'MetState%USTAR_THRESHOLD =', MetState%USTAR_THRESHOLD
   write(*,*) 'MetState%AIRDEN =', MetState%AIRDEN

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

   write(*,*) 'dust_total_flux = ', DiagState%dust_total_flux
   write(*,*) 'Test 4 Success!!!!!'



end program
