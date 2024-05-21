module state_mod
   use precision_mod
   use Config_Opt_Mod, only : OptConfig
   use GridState_Mod, only : GridStateType
   use MetState_Mod, only : MetStateType

   IMPLICIT NONE

   type(GridStateType) :: GRID
   type(MetStateType)  :: MET
   type(OptConfig)  :: Config_Opt

   ! PUBLIC :: Init_State
   PUBLIC :: GRID
   PUBLIC :: MET
   PUBLIC :: Config_Opt

end module state_mod
