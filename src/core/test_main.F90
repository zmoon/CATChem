!> \\file test_main.F90


program testmain

  use QfYaml_Mod
!  use ErrCode_Mod
  implicit none


  CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
  TYPE(QFYAML_t)     :: Config, ConfigAnchored

!  CHARACTER(LEN=255) :: thisLoc
  CHARACTER(LEN=512) :: errMsg

!  INTEGER :: GC_SUCCESS=1


!  INTEGER, PARAMETER :: GC_SUCCESS =  0   ! Routine returns success
!  INTEGER, PUBLIC, PARAMETER :: GC_FAILURE = -1   ! Routine returns failure

  INTEGER         :: RC=1          ! Success or failure


         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
         WRITE( 6, 100   ) TRIM( configFile )

100    FORMAT( 'READ_INPUT_FILE: Opening ', a )



! Assume success
!    RC      = GC_SUCCESS
!    errMsg  = ''
!    thisLoc = ' -> at Read_Input_File (in module GeosCore/input_mod.F90)'
!========================================================================
! Read the YAML file into the Config object
!========================================================================

    print *, 'QFYAML_Init(', configFile, 'Config, ConfigAnchored, RC  )'
    CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )


!    print *, "configFile = ", configFile
!    print *, "Config = ", config
!    print *, "configAnchored = ", ConfigAnchored
!    print *, 'RC = ',  RC


    IF ( RC /= 0 ) THEN
        errMsg = 'Error reading configuration file: ' // TRIM( configFile )
!        CALL GC_Error( errMsg, RC, thisLoc )
!        RETURN
    print *, errMsg
    ENDIF





end program testmain





