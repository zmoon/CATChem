program test_fengha
   use precision_mod, only: fp
   use Config_Opt_Mod, only: ConfigType
   use ChemState_Mod, only: ChemStateType
   use MetState_Mod, only: MetStateType
   use DiagState_Mod, only: DiagStateType
   use CCPr_Dust_Common_Mod, only: DustStateType
   use CCPr_Scheme_Fengsha_Mod, only: CCPr_Scheme_Fengsha
   use CCPr_Dust_mod, only: CCPr_Dust_Init, CCPr_Dust_Run
   use testing_mod, only: assert
   implicit none

   type(ConfigType) :: Config
   type(ChemStateType) :: ChemState
   type(MetStateType) :: MetState
   type(DiagStateType) :: DiagState
   type(DustStateType) :: DustState

   integer :: rc

   Config%dust_activate = .true.
   Config%dust_scheme = 1
   Config%dust_drag_opt = 1
   Config%dust_moist_opt = 1
   Config%dust_horizflux_opt = 1

   ChemState%nSpeciesDust = 0

   MetState%DSOILTYPE = 1
   MetState%SSM = 0.5_fp
   MetState%RDRAG = 0.5_fp
   MetState%TSKIN = 300._fp
   MetState%CLAYFRAC = 0.5_fp
   MetState%SANDFRAC = 0.5_fp
   MetState%GWETTOP = 0.5_fp
   MetState%USTAR = 0.25_fp
   MetState%USTAR_THRESHOLD = 0.1_fp
   allocate(MetState%AIRDEN(1))
   MetState%AIRDEN = 1.2_fp  ! kg/m3

   call CCPr_Dust_Init(Config, DustState, ChemState, rc)
   DustState%AlphaScaleFactor = 1.0_fp

   call CCPr_Dust_Run(MetState, DiagState, DustState, ChemState, rc)

   call assert(DiagState%dust_total_flux == 0.0_fp, "no flux since ustar < u_ts")

end program
