module init_mod

   implicit none

   PUBLIC :: Init_Met
   PUBLIC :: Init_Diag

contains

   subroutine Init_Met(State_Grid, State_Met, RC)

      use GridState_Mod, Only : GridStateType
      use MetState_Mod
      USE Error_Mod
      implicit none

      ! Arguments
      TYPE(GridStateType), INTENT(IN)  :: State_Grid
      TYPE(MetStateType), INTENT(INOUT) :: State_Met
      INTEGER,        INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = 0
      ErrMsg = ''
      thisLoc = ' -> at Init_Met (in core/state_mod.F90)'

      call Zero_State_Met(State_Met, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error Nullifying met state'
         call CC_Error( errMsg, RC , thisLoc)
      endif

      call Met_Allocate(State_Grid, State_Met, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error allocating met state'
         call CC_Error( errMsg, RC , thisLoc)
      endif
   end subroutine Init_Met

   subroutine Init_Diag(Config_Opt, State_Grid, State_Diag, RC)
      use DiagState_Mod
      use Config_Opt_Mod, Only : OptConfig
      use GridState_Mod, Only : GridStateType
      use Error_Mod

      implicit none

      ! Arguments
      TYPE(OptConfig), INTENT(IN)  :: Config_Opt
      TYPE(GridStateType),  INTENT(IN)  :: State_Grid
      TYPE(DiagStateType), INTENT(INOUT) :: State_Diag
      INTEGER,         INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Init_Diag (in core/init_mod.F90)'

      call Diag_Allocate(Config_Opt, State_Grid, State_Diag, RC)
      if (RC /= CC_SUCCESS) then
         errMsg = 'Error allocating diag state'
         call CC_Error( errMsg, RC , thisLoc)
      endif

   end subroutine Init_Diag

end module init_mod
