!> \file
!! CATChem core data types and routines.
module CATChem_core
   !
   use ChemState_Mod, only: ChemStateType
   !
   use Config_Opt_Mod, only: ConfigType
   !
   use Config_Mod, only: CC_Read_Input_File => Read_Input_File
   !
   use DiagState_Mod, only: DiagStateType
   !
   use Error_Mod, only: CC_CheckVar
   use Error_Mod, only: CC_Error
   use Error_Mod, only: CC_FAILURE
   use Error_Mod, only: CC_SUCCESS
   use Error_Mod, only: CC_Warning
   !
   use GridState_Mod, only: GridStateType
   !
   use init_mod, only: CC_Init_Diag => Init_Diag
   use init_mod, only: CC_Init_Met => Init_Met
   !
   use MetState_Mod, only: MetStateType
   use MetState_Mod
   !
   use species_mod, only: SpeciesType
   !
   implicit none

   public

end module CATChem_core
