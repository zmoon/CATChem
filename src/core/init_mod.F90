module init_mod

   implicit none

   PUBLIC :: base_config_yaml_read
   PUBLIC :: Init_Met

contains

   subroutine Init_Met(State_Grid, State_Met, RC)
      use GridState_Mod, Only : GrdState
      use MetState_Mod
      USE Error_Mod
      implicit none

      ! Arguments
      TYPE(GrdState), INTENT(IN)  :: State_Grid
      TYPE(MetState), INTENT(INOUT) :: State_Met
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

   subroutine base_config_yaml_read(Config_Opt, State_Grid, RC)
      use QfYaml_Mod
      use Error_Mod
      use Config_Opt_Mod
      use GridState_Mod, Only : GrdState

      implicit none

      ! INOUT Params
      type(OptConfig), intent(inout) :: Config_Opt ! Input Options object
      type(OptConfig), intent(inout) :: State_Grid ! Input Options object
      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      ! Local Params
      !-------------

      ! Characters
      CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=255) :: thisLoc

      ! QFYAML_t type
      TYPE(QFYAML_t)     :: Config, ConfigAnchored

      ! set thisLoc
      thisLoc = 'init_mod::base_config_yaml_read() -> at read CATChem_Conifg.yml'
      errMsg = ''
      RC = 0

      WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
      WRITE( 6, 100   ) TRIM( configFile )

100   FORMAT( 'READ_INPUT_FILE: Opening ', a )

      !========================================================================
      ! Read the YAML file into the Config object
      !========================================================================

      print *, 'QFYAML_Init(', configFile, 'Config, ConfigAnchored, RC  )'
      CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )

      IF ( RC /= 0 ) THEN
         errMsg = 'Error reading configuration file: ' // TRIM( configFile )
         CALL CC_Error( errMsg, RC , thisLoc)
         RETURN
         print *, errMsg

      ENDIF
   end subroutine base_config_yaml_read

end module init_mod
