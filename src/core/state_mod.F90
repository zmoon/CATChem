module state_mod
   use precision_mod
   use Config_Opt_Mod, only : OptConfig
   use GridState_Mod, only : GrdState
   use MetState_Mod, only : MetState

   IMPLICIT NONE

   type(GrdState) :: GRID
   type(MetState)  :: MET
   type(OptConfig)  :: Config_Opt

   ! PUBLIC :: Init_State
   PUBLIC :: GRID
   PUBLIC :: MET
   PUBLIC :: Config_Opt

end module state_mod
