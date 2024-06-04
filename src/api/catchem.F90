!> \file
!! CATChem core data types and routines.
module CATChem
   !
   use ChemState_Mod, only: ChemStateType
   !
   use Config_Opt_Mod, only: ConfigType
   !
   use Config_Mod, only: cc_read_config => Read_Input_File
   !
   use DiagState_Mod, only: DiagStateType
   !
   use Error_Mod, only: cc_check_var => CC_CheckVar
   use Error_Mod, only: cc_error
   use Error_Mod, only: CC_FAILURE
   use Error_Mod, only: CC_SUCCESS
   use Error_Mod, only: cc_warn => CC_Warning
   !
   use GridState_Mod, only: GridStateType
   !
   use init_mod, only: cc_init_diag => Init_Diag
   use init_mod, only: cc_init_met => Init_Met
   !
   use MetState_Mod, only: MetStateType
   !
   use precision_mod, only: cc_rk => fp
   !
   use species_mod, only: SpeciesType
   !
   use CCPr_Dust_Common_Mod, only: DustStateType
   use CCPr_Dust_mod, only: cc_dust_init => CCPr_Dust_Init
   use CCPr_Dust_mod, only: cc_dust_run => CCPr_Dust_Run
   use CCPr_Dust_mod, only: cc_dust_finalize => CCPr_Dust_Finalize
   !
   implicit none

   public

end module CATChem
