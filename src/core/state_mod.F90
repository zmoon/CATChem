module state_mod
   use precision_mod
   use Config_Opt_Mod, only : ConfigType
   use GridState_Mod, only : GridStateType
   use MetState_Mod, only : MetStateType
   use ChemState_Mod, only : ChemStateType

   IMPLICIT NONE

   type(GridStateType) :: GridState
   type(MetStateType)  :: MetState
   type(ChemStateType) :: ChemState
   type(ConfigType)  :: Config

   ! PUBLIC :: Init_State
   PUBLIC :: GridState
   PUBLIC :: MetState
   PUBLIC :: ChemState
   PUBLIC :: Config

end module state_mod
