!> \\file test_main.F90


program testmain

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CC_Initialize
   PUBLIC :: CC_GET_CONFIG
   PUBLIC :: CC_RUN
   PUBLIC :: CC_FINALIZE

CONTAINS

   subroutine CC_GET_CONFIG(RC)

      ! USES
      use QfYaml_Mod
      ! TODO: Add Error checking

      implicit none

      ! Local Parameters
      CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
      TYPE(QFYAML_t) :: Config, ConfigAnchored


      subroutine CC_Initialize(configFile, RC)

         ! USES
         use
         use QfYaml_Mod
         !  use ErrCode_Mod
         implicit none

         ! Local Params
         !-------------

         ! Characters
         CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml'
         CHARACTER(LEN=512) :: errMsg

         ! QFYAML_t type
         TYPE(QFYAML_t)     :: Config, ConfigAnchored

         ! Integers
         INTEGER         :: RC=1          ! Success or failure

!  INTEGER :: CC_SUCCESS=1


!  INTEGER, PARAMETER :: CC_SUCCESS =  0   ! Routine returns success
!  INTEGER, PUBLIC, PARAMETER :: CC_FAILURE = -1   ! Routine returns failure

         INTEGER         :: RC=1          ! Success or failure


         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
         WRITE( 6, 100   ) TRIM( configFile )

100      FORMAT( 'READ_INPUT_FILE: Opening ', a )



! Assume success
!    RC      = CC_SUCCESS
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
!        CALL CC_Error( errMsg, RC, thisLoc )
!        RETURN
            print *, errMsg
         ENDIF





      end program testmain
