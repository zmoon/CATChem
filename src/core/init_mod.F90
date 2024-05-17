module init_mod

    implicit none
        
    PUBLIC :: base_config_yaml_read

    contains

        subroutine base_config_yaml_read( RC)
        use QfYaml_Mod
        use Error_Mod
        implicit none

        ! Local Params
        !-------------

        ! Characters
        CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
        CHARACTER(LEN=512) :: errMsg

        ! QFYAML_t type 
        TYPE(QFYAML_t)     :: Config, ConfigAnchored

        ! Integers
        INTEGER :: RC          ! Success or failure

        WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
        WRITE( 6, 100   ) TRIM( configFile )

100     FORMAT( 'READ_INPUT_FILE: Opening ', a )



        ! Assume success
        !    RC      = CC_SUCCESS
        !    errMsg  = ''
        !    thisLoc = ' -> at Read_Input_File (in module GeosCore/input_mod.F90)'
        !========================================================================
        ! Read the YAML file into the Config object
        !========================================================================

        print *, 'QFYAML_Init(', configFile, 'Config, ConfigAnchored, RC  )'
        CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )

        IF ( RC /= 0 ) THEN
          errMsg = 'Error reading configuration file: ' // TRIM( configFile )
          CALL CC_Error( errMsg, RC )
          RETURN
        print *, errMsg

        ENDIF
    end subroutine base_config_yaml_read

end module init_mod

