!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: input_mod.F90
!
! !DESCRIPTION: Contains routines that read the CATChem configuration file at
!  the start of the run and pass the information to Config_Opt.
!\\
!\\
! !INTERFACE:
!
MODULE Config_Mod
!
! !USES:
!
   USE CharPak_Mod, ONLY : MaxDim  => MaxStrLen
   USE QfYaml_Mod
   USE Precision_Mod

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC  :: Read_Input_File
   ! PUBLIC  :: Do_Error_Checks
   ! PUBLIC  :: Validate_Directories
!
! !REVISION HISTORY:
!  20 Jul 2004 - R. Yantosca - Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !DEFINED PARAMETERS:
!
   ! YAML configuration file name to be read
   CHARACTER(LEN=21), PARAMETER, PRIVATE :: configFile ='./CATChem_config.yml'

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_input_file
!
! !DESCRIPTION: Driver program for reading the CATChem input file.
!\\
!\\
! In an ESMF environment, all time steps (chemistry, convection, emissions,
! dynamics) must be specified externally before calling this routine.
! The time steps specified in the CATChem configuration file are ignored.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Read_Input_File( Config_Opt, State_Grid, RC )
!
! !USES:
!
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : OptConfig
      USE GridState_Mod, ONLY : GridStateType
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
      TYPE(GridStateType), INTENT(INOUT) :: State_Grid  ! Grid State object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  20 Jul 2004 - R. Yantosca - Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Strings
      CHARACTER(LEN=18), PARAMETER :: configFile ='CATChem_config.yml' ! base configuration file
      ! Objects
      TYPE(QFYAML_t)     :: Config, ConfigAnchored

      ! Error handling
      CHARACTER(LEN=255) :: thisLoc ! where am i
      CHARACTER(LEN=512) :: errMsg  ! error message

      !========================================================================
      ! Read_Input_File begins here!
      !========================================================================

      ! Echo output
      IF ( Config_Opt%amIRoot ) THEN
         WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
         WRITE( 6, '(a,/)' ) 'CATChem Initialization'
         WRITE( 6, 100   ) TRIM( configFile )
100      FORMAT( 'READ_INPUT_FILE: Opening ', a )
      ENDIF

      ! Assume success
      RC      = CC_SUCCESS
      errMsg  = ''
      thisLoc = ' -> at Read_Input_File (in module CATChem/src/core/input_mod.F90)'

      !========================================================================
      ! Read the YAML file into the Config object
      !========================================================================
      CALL QFYAML_Init( configFile, Config, ConfigAnchored, RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error reading configuration file: ' // TRIM( configFile )
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF

      !========================================================================
      ! Get basic simulation settings from the YAML Config object
      !========================================================================

      ! Simulation config settings
      CALL Config_Simulation( Config, Config_Opt, RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error in "Config_Simulation"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( Config         )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      ENDIF

      !========================================================================
      ! Get grid settings from the YAML Config object
      !========================================================================

      ! Grid config settings
      CALL Config_Grid( Config, Config_Opt, State_Grid, RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error in "Config_Grid"!'
         CALL CC_Error( errMsg, RC, thisLoc  )
         CALL QFYAML_CleanUp( Config         )
         CALL QFYAML_CleanUp( ConfigAnchored )
         RETURN
      ENDIF

      !========================================================================
      ! Further error-checking and initialization
      !========================================================================
      CALL QFYAML_CleanUp( Config         )
      CALL QFYAML_CleanUp( ConfigAnchored )

   END SUBROUTINE Read_Input_File
!EOC
!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: config_simulation
!
! !DESCRIPTION: Copies simulation information from the Config object
!  to Config_Opt, and does necessary checks.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Config_Simulation( Config, Config_Opt, RC )
!
! !USES:
!
      USE Charpak_Mod,   ONLY : To_UpperCase
      USE Error_Mod
      USE Config_Opt_Mod, ONLY : OptConfig
      ! USE Time_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Scalars
      LOGICAL                      :: v_bool
      INTEGER                      :: N,                C
      REAL(fp)                     :: JulianDateStart,  JulianDateEnd

      ! Arrays
      INTEGER                      :: a_int(2)

      ! Strings
      CHARACTER(LEN=6)             :: timeStr
      CHARACTER(LEN=8)             :: dateStr
      CHARACTER(LEN=12)            :: met
      CHARACTER(LEN=15)            :: verboseMsg
      CHARACTER(LEN=24)            :: sim
      CHARACTER(LEN=255)           :: thisLoc
      CHARACTER(LEN=512)           :: errMsg
      CHARACTER(LEN=QFYAML_NamLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str

      !========================================================================
      ! Config_Simulation begins here!
      !========================================================================

      ! Initialize
      RC      = CC_SUCCESS
      errMsg  = ''
      thisLoc = &
         ' -> at Config_Simulation (in module CATChem/src/core/input_mod.F90)'

      !------------------------------------------------------------------------
      ! Simulation type
      !------------------------------------------------------------------------
      key   = "simulation%name"
      v_str = MISSING_STR
      CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      Config_Opt%SimulationName = TRIM( v_str )

      ! Return success
      RC = CC_SUCCESS

      !========================================================================
      ! Print to screen
      !========================================================================
      IF ( Config_Opt%amIRoot ) THEN
         WRITE( 6, 90  ) 'SIMULATION SETTINGS'
         WRITE( 6, 95  ) '-------------------'
         WRITE( 6, 110 ) 'Simulation name             : ',                     &
            TRIM( Config_Opt%SimulationName )
         ! WRITE( 6, 110 ) 'CHEM_INPUTS directory       : ',                     &
         !    TRIM( Config_Opt%CHEM_INPUTS_DIR )
         ! WRITE( 6, 110 ) 'Species database file       : ',                     &
         !    TRIM( Config_Opt%SpcDatabaseFile )
         WRITE( 6, 120 ) 'Turn on verbose output      : ',                     &
            Config_Opt%Verbose
         WRITE( 6, 110 ) 'Verbose output printed on   : ',                     &
            TRIM( verboseMsg )
      ENDIF

      ! Format statements
90    FORMAT( /, A              )
95    FORMAT( A                 )
100   FORMAT( A, I8.8, 1X, I6.6 )
110   FORMAT( A, A              )
120   FORMAT( A, L5             )

   END SUBROUTINE Config_Simulation
!EOC
! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: config_grid
!
! !DESCRIPTION: Copies grid information from the Config object
!  to Config_Opt, and does necessary checks.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Config_Grid( Config, Config_Opt, State_Grid, RC )
!
! !USES:
!
      USE CharPak_Mod,    ONLY : StrSplit
      USE Error_Mod
      USE Config_Opt_Mod,  ONLY : OptConfig
      USE GridState_Mod, ONLY : GridStateType
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(QFYAML_t),      INTENT(INOUT) :: Config      ! YAML Config object
      TYPE(OptConfig),     INTENT(INOUT) :: Config_Opt   ! Input options
      TYPE(GridStateType), INTENT(INOUT) :: State_Grid  ! Grid State
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  20 Oct 2018 - M. Sulprizio- Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Scalars
      LOGICAL                      :: v_bool
      INTEGER                      :: v_int
      INTEGER                      :: nSubStrs
      INTEGER                      :: N
      INTEGER                      :: C

      ! Arrays
      INTEGER                      :: a_int(4)

      ! Strings
      CHARACTER(LEN=10)            :: xMin_Str, xMax_Str
      CHARACTER(LEN=10)            :: yMin_Str, yMax_Str
      CHARACTER(LEN=255)           :: thisLoc,  nLev
      CHARACTER(LEN=512)           :: errMsg
      CHARACTER(LEN=QFYAML_StrLen) :: key
      CHARACTER(LEN=QFYAML_StrLen) :: v_str

      ! String arrays
      CHARACTER(LEN=255)           :: subStrs(MAXDIM)
      CHARACTER(LEN=QFYAML_StrLen) :: a_str(2)

      !========================================================================
      ! Config_Grid begins here!
      !========================================================================

      ! Initialize
      RC      = CC_SUCCESS
      errMsg  = ''
      thisLoc = ' -> at Config_Grid (in CATChem/src/core/input_mod.F90)'

      !------------------------------------------------------------------------
      ! Level range
      !------------------------------------------------------------------------
      key   = "grid%number_of_levels"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      State_Grid%number_of_levels = v_int

      !------------------------------------------------------------------------
      ! number of x and y dimensions (nx and ny)
      !------------------------------------------------------------------------
      key   = "grid%nx"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      State_Grid%NX = v_int

      key   = "grid%ny"
      v_int = MISSING_INT
      CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
      IF ( RC /= CC_SUCCESS ) THEN
         errMsg = 'Error parsing ' // TRIM( key ) // '!'
         CALL CC_Error( errMsg, RC, thisLoc )
         RETURN
      ENDIF
      State_Grid%NY = v_int

      ! Return success
      RC = CC_SUCCESS

!       !========================================================================
!       ! Print to screen
!       !========================================================================
!       IF ( Config_Opt%amIRoot ) THEN
!          WRITE( 6, 90  ) 'GRID SETTINGS'
!          WRITE( 6, 95  )  '------------'
!          WRITE( 6, 100 ) 'Grid resolution             : ',                     &
!             TRIM( State_Grid%GridRes )
!          WRITE( 6, 110 ) 'Min/max longitude           : ',                     &
!             State_Grid%XMin, State_Grid%XMax
!          WRITE( 6, 110 ) 'Min/max latitude            : ',                     &
!             State_Grid%YMin, State_Grid%YMax
!          WRITE( 6, 120 ) 'X grid dimension            : ',                     &
!             State_Grid%NX
!          WRITE( 6, 120 ) 'Y grid dimension            : ',                     &
!             State_Grid%NY
!          WRITE( 6, 120 ) 'Z grid dimension            : ',                     &
!             State_Grid%NZ
!          WRITE( 6, 130 ) 'Use half-sized polar boxes? : ',                     &
!             State_Grid%HalfPolar
!          WRITE( 6, 130 ) 'Center on Intl Date Line?   : ',                     &
!             State_Grid%Center180
!          WRITE( 6, 130 ) 'Is this a nested-grid sim?  : ',                     &
!             State_Grid%NestedGrid
!          WRITE( 6, 140 ) ' --> Buffer zone (N S E W ) : ',                     &
!             State_Grid%NorthBuffer,                              &
!             State_Grid%SouthBuffer,                              &
!             State_Grid%EastBuffer,                               &
!             State_Grid%WestBuffer
!       ENDIF

!       ! Format statements
! 90    FORMAT( /, A                )
! 95    FORMAT( A                   )
! 100   FORMAT( A, A                )
! 110   FORMAT( A, F10.4, 1X, F10.4 )
! 120   FORMAT( A, I5               )
! 130   FORMAT( A, L5               )
! 140   FORMAT( A, 4I5              )

   END SUBROUTINE Config_Grid

END MODULE config_mod
