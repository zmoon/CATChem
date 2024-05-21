!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
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
!                  CATChem Global Chemical Transport Model                  !
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
      USE GridState_Mod, ONLY : GrdState
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
      TYPE(GrdState), INTENT(INOUT) :: State_Grid  ! Grid State object
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

! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
!       ! Grid config settings
!       ! Skip if we are gettomg the grid from an external model
!       CALL Config_Grid( Config, Config_Opt, State_Grid, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_Grid"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF
! #endif

      ! ! Timesteps config settings
      ! CALL Config_Timesteps( Config, Config_Opt, State_Grid, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_Timesteps"!'
      !    CALL CC_Error( errMsg, RC, thisLoc  )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      !========================================================================
      ! Get settings for CATChem operations from the YAML Config object
      !========================================================================

      ! ! Transport settings
      ! CALL Config_Transport( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_Transport"!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      ! ! Convection and PBL mixing settings
      ! CALL Config_Convection_Mixing( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_Convection_Mixing"!'
      !    CALL CC_Error( errMsg, RC, thisLoc  )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      ! ! Aerosol settings
      ! CALL Config_Aerosol( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_Aerosol"!'
      !    CALL CC_Error( errMsg, RC, thisLoc  )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      ! ! Dry deposition and wet deposition settings
      ! CALL Config_DryDep_WetDep( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_DryDep_WetDep"!'
      !    CALL CC_Error( errMsg, RC, thisLoc  )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      ! ! Chemistry settings
      ! CALL Config_Chemistry( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_Chemistry"!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! ! Photolysis settings
      ! CALL Config_Photolysis( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_Photolysis"!'
      !    CALL CC_Error( errMsg, RC, thisLoc  )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      ! ! RRTMG (radiative transfer model) settings
      ! CALL Config_RRTMG( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_RRTMG"!'
      !    CALL CC_Error( errMsg, RC, thisLoc  )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

      !========================================================================
      ! Get settings for specialty simulations from the YAML Config object
      !========================================================================

      ! CH4/carbon simulation settings
      ! IF ( Config_Opt%Its_A_CH4_Sim .or. Config_Opt%Its_A_TagCH4_Sim .or. &
      !    Config_Opt%Its_A_Carbon_Sim ) THEN
      !    CALL Config_CH4( Config, Config_Opt, RC )
      !    IF ( RC /= CC_SUCCESS ) THEN
      !       errMsg = 'Error in "Config_CH4"!'
      !       CALL CC_Error( errMsg, RC, thisLoc  )
      !       CALL QFYAML_CleanUp( Config         )
      !       CALL QFYAML_CleanUp( ConfigAnchored )
      !       RETURN
      !    ENDIF
      ! ENDIF

      ! ! CO simulation settings
      ! IF ( Config_Opt%Its_A_TagCO_Sim .or. Config_Opt%Its_A_Carbon_Sim ) THEN
      !    CALL Config_CO( Config, Config_Opt, RC )
      !    IF ( RC /= CC_SUCCESS ) THEN
      !       errMsg = 'Error in "Config_CO"!'
      !       CALL CC_Error( errMsg, RC, thisLoc  )
      !       CALL QFYAML_CleanUp( Config         )
      !       CALL QFYAML_CleanUp( ConfigAnchored )
      !       RETURN
      !    ENDIF
      ! ENDIF


      ! ! CO2/carbon simulation settings
      ! IF ( Config_Opt%Its_A_CO2_Sim .or. Config_Opt%Its_A_Carbon_Sim ) THEN
      !    CALL Config_CO2( Config, Config_Opt, RC )
      !    IF ( RC /= CC_SUCCESS ) THEN
      !       errMsg = 'Error in "Config_CO2"!'
      !       CALL CC_Error( errMsg, RC, thisLoc  )
      !       CALL QFYAML_CleanUp( Config         )
      !       CALL QFYAML_CleanUp( ConfigAnchored )
      !       RETURN
      !    ENDIF
      ! ENDIF

      ! ! Hg simulation settings
      ! IF ( Config_Opt%Its_A_Mercury_Sim ) THEN
      !    CALL Config_Hg( Config, Config_Opt, RC )
      !    IF ( RC /= CC_SUCCESS ) THEN
      !       errMsg = 'Error in "Config_Hg"!'
      !       CALL CC_Error( errMsg, RC, thisLoc  )
      !       CALL QFYAML_CleanUp( Config         )
      !       CALL QFYAML_CleanUp( ConfigAnchored )
      !       RETURN
      !    ENDIF
      ! ENDIF

      ! ! POPs simulation settings
      ! IF ( Config_Opt%Its_A_POPs_Sim ) THEN
      !    CALL Config_POPs( Config, Config_Opt, RC )
      !    IF ( RC /= CC_SUCCESS ) THEN
      !       errMsg = 'Error in "Config_POPs"!'
      !       CALL CC_Error( errMsg, RC, thisLoc  )
      !       CALL QFYAML_CleanUp( Config         )
      !       CALL QFYAML_CleanUp( ConfigAnchored )
      !       RETURN
      !    ENDIF
      ! ENDIF

      !========================================================================
      ! Get settings for extra diagnostics from the YAML Config object
      !========================================================================

      ! Obspack diagnostic settings
      ! CALL Config_ObsPack( Config, Config_Opt, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error in "Config_ObsPack"!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    CALL QFYAML_CleanUp( Config         )
      !    CALL QFYAML_CleanUp( ConfigAnchored )
      !    RETURN
      ! ENDIF

! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))

!       ! Planeflight diagnostic settings
!       ! (Skip if we are connecting to an external model)
!       CALL Config_PlaneFlight( Config, Config_Opt, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_PlaneFlight"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF

! #ifdef BPCH_DIAG
!       ! GAMAP metadata files
!       ! (Skip if we are connecting to an external model)
!       CALL Config_Gamap( Config, Config_Opt, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_Gamap"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF

!       ! ND51 timeseries
!       ! (Skip if we are connecting to an external model)
!       CALL Config_ND51( Config, Config_Opt, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_ND51"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF

!       ! ND51b timeseries
!       ! (Skip if we are connecting to an external model)
!       CALL Config_ND51b( Config, Config_Opt, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_ND51b"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF

!       ! Bpch diagnostic output
!       ! (Skip if we are connecting to an external model)
!       CALL Config_Bpch_Output( Config, Config_Opt, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_Bpch_Output"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF

! #ifdef TOMAS
!       ! Bpch prod & loss -- only needed for TOMAS
!       ! (Skip if we are connecting to an external model)
!       CALL Config_Bpch_ProdLoss( Config, Config_Opt, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Config_Bpch_ProdLoss"!'
!          CALL CC_Error( errMsg, RC, thisLoc  )
!          CALL QFYAML_CleanUp( Config         )
!          CALL QFYAML_CleanUp( ConfigAnchored )
!          RETURN
!       ENDIF
! #endif
! #endif

!       !========================================================================
!       ! Check CATChem timesteps
!       ! NOTE: Skip for GCHP/GEOS, as this is called from GCHP_Chunk_Run
!       !========================================================================
!       CALL Check_Time_Steps( Config_Opt, State_Grid, RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error in "Check_Time_Steps"!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
! #endif

      !========================================================================
      ! Further error-checking and initialization
      !========================================================================
      CALL QFYAML_CleanUp( Config         )
      CALL QFYAML_CleanUp( ConfigAnchored )

   END SUBROUTINE Read_Input_File
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
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
! #if defined( ESMF_ ) || defined( MODEL_ )
!       INTEGER                      :: H,       M,       S
!       REAL(f4)                     :: init_UTC
! #endif

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

      ! Error check simulation name
      ! Sim = To_UpperCase( TRIM( Config_Opt%SimulationName ) )
      ! IF ( TRIM(Sim) /= 'AEROSOL'                                        .and. &
      !    TRIM(Sim) /= 'CARBON'                                         .and. &
      !    TRIM(Sim) /= 'CH4'                                            .and. &
      !    TRIM(Sim) /= 'CO2'                                            .and. &
      !    TRIM(Sim) /= 'FULLCHEM'                                       .and. &
      !    TRIM(Sim) /= 'HG'                                             .and. &
      !    TRIM(Sim) /= 'METALS'                                         .and. &
      !    TRIM(Sim) /= 'POPS'                                           .and. &
      !    TRIM(Sim) /= 'TAGCH4'                                         .and. &
      !    TRIM(Sim) /= 'TAGCO'                                          .and. &
      !    TRIM(Sim) /= 'TAGO3'                                          .and. &
      !    TRIM(Sim) /= 'TRANSPORTTRACERS' ) THEN

      !    errMsg = Trim( Config_Opt%SimulationName) // ' is not a'            // &
      !       ' valid simulation. Supported simulations are:'           // &
      !       ' aerosol, carbon, CH4, CO2, fullchem, Hg, Metals, POPs,' // &
      !       ' TransportTracers, TagCH4, TagCO, or TagO3.'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! ! Set simulation type flags in Config_Opt
      ! Config_Opt%ITS_AN_AEROSOL_SIM   = ( TRIM(Sim) == 'AEROSOL'               )
      ! Config_Opt%ITS_A_CARBON_SIM     = ( TRIM(Sim) == 'CARBON'                )
      ! Config_Opt%ITS_A_CH4_SIM        = ( TRIM(Sim) == 'CH4'                   )
      ! Config_Opt%ITS_A_CO2_SIM        = ( TRIM(Sim) == 'CO2'                   )
      ! Config_Opt%ITS_A_FULLCHEM_SIM   = ( TRIM(Sim) == 'FULLCHEM'              )
      ! Config_Opt%ITS_A_MERCURY_SIM    = ( TRIM(Sim) == 'HG'                    )
      ! Config_Opt%ITS_A_TRACEMETAL_SIM = ( TRIM(Sim) == 'METALS'                )
      ! Config_Opt%ITS_A_POPS_SIM       = ( TRIM(Sim) == 'POPS'                  )
      ! Config_Opt%ITS_A_TAGCH4_SIM     = ( TRIM(Sim) == 'TAGCH4'                )
      ! Config_Opt%ITS_A_TAGCO_SIM      = ( TRIM(Sim) == 'TAGCO'                 )
      ! Config_Opt%ITS_A_TAGO3_SIM      = ( TRIM(Sim) == 'TAGO3'                 )
      ! Config_Opt%ITS_A_TRACER_SIM     = ( TRIM(Sim) == 'TRANSPORTTRACERS'      )

      ! TODO: Add support for species files later on
      ! !------------------------------------------------------------------------
      ! ! Species database file
      ! !------------------------------------------------------------------------
      ! key   = "simulation%species_database_file"
      ! v_str = MISSING_STR
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%SpcDataBaseFile = TRIM( v_str )

      ! !------------------------------------------------------------------------
      ! ! Species metadata output file
      ! !------------------------------------------------------------------------
      ! key   = "simulation%species_metadata_output_file"
      ! v_str = MISSING_STR
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%SpcMetaDataOutFile = TRIM( v_str )

      ! !------------------------------------------------------------------------
      ! ! Turn on debug output
      ! !------------------------------------------------------------------------
      ! key    = "simulation%verbose%activate"
      ! v_bool = MISSING_BOOL
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%VerboseRequested = v_bool

      ! !------------------------------------------------------------------------
      ! ! Which cores for verbose output: root or all?
      ! !------------------------------------------------------------------------
      ! key  = "simulation%verbose%on_cores"
      ! v_str = MISSING_STR
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%VerboseOnCores = To_UpperCase( v_str )

      ! Should verbose output be printed only on root or on all cores?
      ! SELECT CASE ( TRIM( Config_Opt%VerboseOnCores ) )
      !  CASE( 'ROOT' )
      !    verboseMsg = 'root core only'
      !    Config_Opt%Verbose =                                                &
      !       ( Config_Opt%VerboseRequested .and. Config_Opt%amIRoot )
      !  CASE( 'ALL' )
      !    verboseMsg = 'all cores'
      !    Config_Opt%Verbose = Config_Opt%VerboseRequested
      !  CASE DEFAULT
      !    errMsg = 'Invalid selection!' // NEW_LINE( 'a' ) //                &
      !       'simulation:verbose:on_cores must be either "root" or "all"'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! END SELECT

      ! !========================================================================
      ! !             %%%%%%% CATChem CLASSIC (with OpenMP) %%%%%%%
      ! !
      ! ! If we aren't using ESMF, read extra settings from geoschem_config.yml
      ! !========================================================================

      ! !------------------------------------------------------------------------
      ! ! Simulation start date
      ! !------------------------------------------------------------------------
      ! key   = "simulation%start_date"
      ! a_int = MISSING_INT
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), a_int, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%NYMDb = a_int(1)
      ! Config_Opt%NHMSb = a_int(2)

      ! ! Make sure the starting date NYMDb is valid
      ! IF ( .not. Valid_Date( Config_Opt%NYMDb ) ) THEN
      !    WRITE( DateStr, '(i8.8)' ) Config_Opt%NYMDb
      !    errMsg = 'Input%Opt%NYMDb = ' // DateStr        // &
      !       ' is not a valid calendar date!'       // &
      !       ' Please check your "geoschem_config.yml" file.'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! ! Make sure the starting time NHMSb is valid
      ! IF ( .not. Valid_Time( Config_Opt%NHMSb ) ) THEN
      !    WRITE( TimeStr, '(i6.6)' ) Config_Opt%NHMSb
      !    errMsg = 'Input%Opt%NHMSb = ' // TimeStr        // &
      !       ' is not a valid clock time!'          // &
      !       ' Please check your "geoschem_config.yml" file.'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! !------------------------------------------------------------------------
      ! ! Simulation end date
      ! !------------------------------------------------------------------------
      ! key   = "simulation%end_date"
      ! a_int = MISSING_INT
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), a_int, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%NYMDe = a_int(1)
      ! Config_Opt%NHMSe = a_int(2)

      ! ! Make sure the starting date NYMDb is valid
      ! IF ( .not. Valid_Date( Config_Opt%NYMDe ) ) THEN
      !    WRITE( DateStr, '(i8.8)' ) Config_Opt%NYMDe
      !    errMsg = 'Input%Opt%NYMDe = ' // DateStr        // &
      !       ' is not a valid calendar date!'       // &
      !       ' Please check your "geoschem_config.yml" file.'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! ! Make sure the ending time NHMSe is valid
      ! IF ( .not. Valid_Time( Config_Opt%NHMSe ) ) THEN
      !    WRITE( TimeStr, '(i6.6)' ) Config_Opt%NHMSe
      !    errMsg = 'Input%Opt%NHMSe = ' // TimeStr        // &
      !       ' is not a valid clock time!'          // &
      !       ' Please check your "geoschem_config.yml" file.'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! !------------------------------------------------------------------------
      ! ! Root data directory
      ! !------------------------------------------------------------------------
      ! key   = "simulation%root_data_dir"
      ! v_str = MISSING_STR
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%Data_Dir = TRIM( v_str )

      ! ! Make sure DATA-DIR ends with a "/" character
      ! C = LEN_TRIM( Config_Opt%DATA_DIR )
      ! IF ( Config_Opt%DATA_DIR(C:C) /= '/' ) THEN
      !    Config_Opt%DATA_DIR = TRIM( Config_Opt%DATA_DIR ) // '/'
      ! ENDIF

      ! ! Create CHEM_INPUTS directory
      ! Config_Opt%CHEM_INPUTS_DIR = TRIM( Config_Opt%DATA_DIR ) // &
      !    'CHEM_INPUTS/'

      ! !------------------------------------------------------------------------
      ! ! Meteorology field
      ! !------------------------------------------------------------------------
      ! key   = "simulation%met_field"
      ! v_str = MISSING_STR
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%MetField = TRIM( v_str )

! #if !defined( MODEL_CESM )
!       ! Make sure a valid met field is specified
!       Met = To_UpperCase( TRIM( Config_Opt%MetField ) )
!       SELECT CASE( TRIM( Met ) )
!        CASE( 'GEOS-FP', 'GEOSFP' )
!          Config_Opt%MetField = 'GEOSFP'
!        CASE( 'MERRA-2', 'MERRA2' )
!          Config_Opt%MetField = 'MERRA2'
!        CASE( 'GEOS-IT', 'GEOSIT' )
!          Config_Opt%MetField = 'GEOSIT'
!        CASE( 'MODELE2.1' )
!          Config_Opt%MetField = 'MODELE2.1'
!        CASE( 'MODELE2.2' )
!          Config_Opt%MetField = 'MODELE2.2'
!        CASE DEFAULT
!          errMsg = Trim( Config_Opt%MetField ) // ' is not a valid '       // &
!             ' met field. Supported met fields are GEOS-FP, '          // &
!             ' MERRA-2 and ModelE2.1. Please check your '              // &
!             '"geoschem_config.ymls" file.'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       END SELECT
! #endif

      ! !------------------------------------------------------------------------
      ! ! Turn on timers
      ! !------------------------------------------------------------------------
      ! key    = "simulation%use_gcclassic_timers"
      ! v_bool = MISSING_BOOL
      ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF
      ! Config_Opt%UseTimers = v_bool

      ! !------------------------------------------------------------------------
      ! ! Set start time of run in "time_mod.F90"
      ! !------------------------------------------------------------------------
      ! CALL Set_Begin_Time( Config_Opt%NYMDb, Config_Opt%NHMSb, RC  )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    errMsg = 'Error encountered in "Set_Begin_Time"!'
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! !------------------------------------------------------------------------
      ! ! Set end time of run in "time_mod.F90"
      ! !------------------------------------------------------------------------
      ! errMsg = 'Error encountered in "Set_Begin_Time"!'
      ! CALL Set_End_Time( Config_Opt%NYMDe, Config_Opt%NHMSe, RC )
      ! IF ( RC /= CC_SUCCESS ) THEN
      !    CALL CC_Error( errMsg, RC, thisLoc )
      !    RETURN
      ! ENDIF

      ! ! Set the current time
      ! CALL Set_Current_Time()

      !========================================================================
      ! Compute the length of the simulation, in elapsed seconds
      !========================================================================
      ! JulianDateStart        = GET_JD( Config_Opt%NymdB, Config_Opt%NhmsB )
      ! JulianDateEnd          = GET_JD( Config_Opt%NymdE, Config_Opt%NhmsE )
      ! Config_Opt%SimLengthSec = NINT( ( JulianDateEnd - JulianDateStart  )      &
      !    * 86400_f8)

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
!                  CATChem Global Chemical Transport Model                  !
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
      ! USE RoundOff_Mod,   ONLY : 0.  !_and_RoundOff
      USE GridState_Mod, ONLY : GrdState
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
      TYPE(GrdState), INTENT(INOUT) :: State_Grid  ! Grid State
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

!       !------------------------------------------------------------------------
!       ! Grid resolution
!       !------------------------------------------------------------------------
!       key   = "grid%resolution"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%GridRes = TRIM( v_str )

!       ! Split into two values, separated by 'x'
!       CALL StrSplit( TRIM( State_Grid%GridRes ) , 'x', SubStrs, nSubStrs )

!       ! Stop with error if there are more than two substrings
!       IF ( nSubStrs /= 2 ) THEN
!          errMsg = 'Error in extracting delta X and Y values from'    // &
!             ' State_Grid%GridRes. Values must be separated by' // &
!             ' an x. Please check your "geoschem_config.yml" file.'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF

!       ! Save the delta X and Y values
!       State_Grid%DY = 0.  !_and_RoundOff( subStrs(1), places=4 )
!       State_Grid%DX = 0.  !_and_RoundOff( subStrs(2), places=4 )

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
!       !------------------------------------------------------------------------
!       ! Longitude range
!       !------------------------------------------------------------------------
!       key   = "grid%longitude%range"
!       a_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%XMin = 0.  !_and_RoundOff( a_str(1), places=4 )
!       State_Grid%XMax = 0.  !_and_RoundOff( a_str(2), places=4 )

!       ! Make sure values are in valid rangre
!       IF ( State_Grid%XMin >= State_Grid%XMax ) THEN
!          WRITE( XMin_Str, '(i10)' ) State_Grid%XMin
!          WRITE( XMax_Str, '(i10)' ) State_Grid%XMax
!          errMsg = 'Lower lon must be smaller than upper lon: ' // &
!             TRIM( XMin_Str ) // ' ' // TRIM( XMax_Str )
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF

!       !------------------------------------------------------------------------
!       ! Center longitude on International Date Line?Longitude range
!       !------------------------------------------------------------------------
!       key    = "grid%longitude%center_at_180"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%Center180 = v_bool

!       !------------------------------------------------------------------------
!       ! Latitude range
!       !------------------------------------------------------------------------
!       key   = "grid%latitude%range"
!       a_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%YMin = 0.  !_and_RoundOff( a_str(1), places=4 )
!       State_Grid%YMax = 0.  !_and_RoundOff( a_str(2), places=4 )

!       ! Make sure values are in valid range
!       IF ( State_Grid%YMin >= State_Grid%YMax ) THEN
!          WRITE( YMin_Str, '(i10)' ) State_Grid%YMin
!          WRITE( YMax_Str, '(i10)' ) State_Grid%YMax
!          errMsg = 'Lower lat must be smaller than upper lat: ' // &
!             TRIM( YMin_Str ) // ' ' // TRIM( YMax_Str )
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF

!       ! Restrict latitude values to -90.0 and 90.0
!       IF ( State_Grid%YMin < -90.0_fp ) THEN
!          WRITE( YMin_Str, '(i10)' ) State_Grid%YMin
!          errMsg = 'Lower latitude must be between -90 and 90 degN: ' // &
!             TRIM( YMin_Str )
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       IF ( State_Grid%YMax > 90.0_fp ) THEN
!          WRITE( YMax_Str, '(i10)' ) State_Grid%YMax
!          errMsg = 'Upper latitude must be between -90 and 90 degN: ' // &
!             TRIM( YMax_Str )
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF

!       !------------------------------------------------------------------------
!       ! Use half-sized polar boxes in latitude?
!       !------------------------------------------------------------------------
!       key    = "grid%latitude%half_size_polar_boxes"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%HalfPolar = v_bool

!       !------------------------------------------------------------------------
!       ! Nested grid settings
!       !------------------------------------------------------------------------
!       key    = "grid%nested_grid_simulation%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%NestedGrid = v_bool

!       IF ( State_Grid%NestedGrid ) THEN
!          ! Increase NX by 1
!          State_Grid%NX        = State_Grid%NX + 1

!          ! For now hardcode HalfPolar to false when using a nested grid
!          State_Grid%HalfPolar = .FALSE.
!       ENDIF

!       !------------------------------------------------------------------------
!       ! Nested grid transport offsets
!       !------------------------------------------------------------------------
!       key   = "grid%nested_grid_simulation%buffer_zone_NSEW"
!       a_int = MISSING_INT
!       CALL QFYAML_Add_Get( Config, TRIM( key ), a_int, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       State_Grid%NorthBuffer = a_int(1)
!       State_Grid%SouthBuffer = a_int(2)
!       State_Grid%EastBuffer  = a_int(3)
!       State_Grid%WestBuffer  = a_int(4)

!       ! Set buffers to zero for global grids
!       IF ( .not. State_Grid%NestedGrid ) THEN
!          State_Grid%NorthBuffer = 0
!          State_Grid%SouthBuffer = 0
!          State_Grid%EastBuffer  = 0
!          State_Grid%WestBuffer  = 0
!       ENDIF

!       ! Compute grid horizontal dimensions
!       State_Grid%NX =                                                          &
!          FLOOR( ( State_Grid%XMax - State_Grid%XMin ) / State_Grid%DX )
!       IF ( State_Grid%HalfPolar .and. .not. State_Grid%NestedGrid ) THEN
!          State_Grid%NY =                                                       &
!             FLOOR( ( State_Grid%YMax - State_Grid%YMin ) / State_Grid%DY ) + 1
!       ELSE
!          State_Grid%NY =                                                       &
!             FLOOR( ( State_Grid%YMax - State_Grid%YMin ) / State_Grid%DY )
!       ENDIF

!       ! Return success
!       RC = CC_SUCCESS

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
! !EOC
! #endif
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: config_timesteps
! !
! ! !DESCRIPTION: Copies timestep information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_Timesteps( Config, Config_Opt, State_Grid, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod,  ONLY : OptConfig
!       USE GridState_Mod, ONLY : GrdState
! !
! ! !INPUT PARAMETERS:
! !
!       TYPE(GrdState), INTENT(IN)    :: State_Grid  ! Grid State object
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       INTEGER                      :: v_int

!       ! Strings
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=512)           :: errMsg
!       CHARACTER(LEN=QFYAML_StrLen) :: key

!       !========================================================================
!       ! Config_Timesteps begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = 'Error reading the "geoschem_config.yml" file!'
!       thisLoc = ' -> at Config_Timestep (in module CATChem/src/core/input_mod.F90)'

! ! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
! !       !------------------------------------------------------------------------
! !       ! Transport/convection timestep
! !       !------------------------------------------------------------------------
! !       key   = "timesteps%transport_timestep_in_s"
! !       v_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%TS_DYN  = v_int
! !       Config_Opt%TS_CONV = v_int

! !       !------------------------------------------------------------------------
! !       ! Chemistry/emissions timestep
! !       !------------------------------------------------------------------------
! !       key   = "timesteps%chemistry_timestep_in_s"
! !       v_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%TS_CHEM = v_int
! !       Config_Opt%TS_EMIS = v_int

! !       !------------------------------------------------------------------------
! !       ! RRTMG radiation timestep
! !       !------------------------------------------------------------------------
! !       key   = "timesteps%radiation_timestep_in_s"
! !       v_int = 0
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%TS_RAD = v_int
! ! #endif

!       !========================================================================
!       ! Error checks
!       !========================================================================

! ! #ifdef MODEL_CLASSIC
! !       IF ( Config_Opt%SimLengthSec < Config_Opt%TS_DYN                      .or. &
! !          Config_Opt%SimLengthSec < Config_Opt%TS_CHEM )                   THEN
! !          IF ( Config_Opt%amIRoot )                                         THEN
! !             WRITE( 6,'(a)' ) ''
! !             WRITE( 6,'(a)' ) 'The length of the simulation is shorter '
! !             WRITE( 6,'(a)' ) 'than the transport and/or chemistry '
! !             WRITE( 6,'(a)' ) 'timesteps. Check the settings in '
! !             WRITE( 6,'(a)' ) 'the "geoschem_config.yml" file.'
! !             WRITE( 6,'(a)' ) ''
! !             WRITE( 6,100 ) 'Transport/Convection  [sec] : ', Config_Opt%TS_DYN
! !             WRITE( 6,100 ) 'Chemistry/Emissions   [sec] : ', Config_Opt%TS_CHEM
! !             WRITE( 6,100 ) 'Simulation duration   [sec] : ',                   &
! !                Config_Opt%SimLengthSec
! !             CALL CC_Error( errMsg, RC, thisLoc )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       IF ( TRIM( Config_Opt%MetField ) == 'MERRA2'                        .and. &
! !          TRIM( State_Grid%GridRes ) == '0.5x0.625' )                   THEN
! !          IF ( Config_Opt%ITS_A_CH4_SIM     .or. &
! !             Config_Opt%ITS_A_TAGCH4_SIM  .or. &
! !             Config_Opt%ITS_A_CO2_SIM )   THEN
! !             IF ( Config_Opt%TS_DYN > 300 .or. Config_Opt%TS_CHEM > 600 )   THEN
! !                IF ( Config_Opt%amIRoot ) THEN
! !                   WRITE( 6,'(a)' ) ''
! !                   WRITE( 6,'(a)' ) 'It has been noted that MERRA-2 nested grid'
! !                   WRITE( 6,'(a)' ) ' simulations can have very high species'
! !                   WRITE( 6,'(a)' ) ' concentrations in the stratosphere caused'
! !                   WRITE( 6,'(a)' ) ' by a violation of the CFL condition due to'
! !                   WRITE( 6,'(a)' ) ' strong stratospheric winds. This is'
! !                   WRITE( 6,'(a)' ) ' especially problematic when using total'
! !                   WRITE( 6,'(a)' ) ' column concentrations. To avoid the issue,'
! !                   WRITE( 6,'(a)' ) ' a timestep of 5/10 instead of 10/20 is'
! !                   WRITE( 6,'(a)' ) ' recommended for CH4 and CO2 simulations.'
! !                   WRITE( 6,'(a)' ) ''
! !                   WRITE( 6,'(a)' ) 'You may remove this trap at your own peril,'
! !                   WRITE( 6,'(a)' ) ' by commenting out the call to CC_ERROR in'
! !                   WRITE( 6,'(a)' ) ' CATChem/src/core/input_mod.F90. '
! !                   WRITE( 6,'(a)' ) ''
! !                   WRITE( 6,'(a)' ) 'See the MERRA-2 implementation details page'
! !                   WRITE( 6,'(a)' ) ' on the CATChem wiki for details'
! !                   CALL CC_Error( errMsg, RC, thisLoc )
! !                   RETURN
! !                ENDIF
! !             ENDIF
! !          ENDIF
! !       ENDIF
! ! #endif

!       ! Return success
!       RC = CC_SUCCESS

!       !========================================================================
!       ! Print to screen
!       !========================================================================
!       ! IF ( Config_Opt%amIRoot ) THEN
!       !    WRITE( 6, 90  ) 'TIMESTEP SETTINGS'
!       !    WRITE( 6, 95  ) '-----------------'
!       !    WRITE( 6, 100 ) 'Transport/Convection [sec]  : ', Config_Opt%TS_DYN
!       !    WRITE( 6, 100 ) 'Chemistry/Emissions  [sec]  : ', Config_Opt%TS_CHEM
!       !    WRITE( 6, 100 ) 'RRTMG rad transfer   [sec]  : ', Config_Opt%TS_RAD
!       ! ENDIF

!       ! Format statements
! 90    FORMAT( /, A   )
! 95    FORMAT( A      )
! 100   FORMAT( A, I5  )

!    END SUBROUTINE Config_Timesteps
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: config_aerosol
! !
! ! !DESCRIPTION: Copies aerosol information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_Aerosol( Config, Config_Opt, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod, ONLY : OptConfig
!       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !
! ! !REMARKS:
! !  Move error checks that depend on species indices to the subroutine
! !  DO_ERROR_CHECKS.  This is now called from CC_INIT_EXTRA, after the
! !  initialization of the species database.
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       INTEGER                      :: N, T
!       INTEGER                      :: v_int
!       LOGICAL                      :: v_bool
!       REAL(yp)                     :: v_real

!       ! Strings
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=255)           :: errMsg
!       CHARACTER(LEN=QFYAML_NamLen) :: key
!       CHARACTER(LEN=QFYAML_StrLen) :: v_str
!       CHARACTER(LEN=QFYAML_StrLen) :: a_str(2)

!       !========================================================================
!       ! Config_Aerosol begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = ''
!       thisLoc = ' -> at Config_Aerosol (in module CATChem/src/core/input_mod.F90)'

!       !------------------------------------------------------------------------
!       ! Use online carbon aerosols?
!       !------------------------------------------------------------------------
!       key    = "aerosols%carbon%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LCARB = v_bool

!       !------------------------------------------------------------------------
!       ! Use brown carbon aerosols?
!       !------------------------------------------------------------------------
!       key    = "aerosols%carbon%use_brown_carbon"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LBRC = v_bool

!       !------------------------------------------------------------------------
!       ! Include BC absorption enhancement due to coating?
!       !------------------------------------------------------------------------
!       key    = "aerosols%carbon%enhance_black_carbon_absorption%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LBCAE = v_bool

!       !------------------------------------------------------------------------
!       ! Define BC absorption enhancement
!       !------------------------------------------------------------------------
!       key   = "aerosols%carbon%enhance_black_carbon_absorption%hydrophilic"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%BCAE_1 = 0.  !_and_RoundOff( v_str, places=2 )

!       !------------------------------------------------------------------------
!       ! Define BC absorption enhancement (xnw, 8/24/15)
!       !------------------------------------------------------------------------
!       key   = "aerosols%carbon%enhance_black_carbon_absorption%hydrophobic"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%BCAE_2 = 0.  !_and_RoundOff( v_str, places=2 )

!       !------------------------------------------------------------------------
!       ! Use secondary organic aerosols?
!       !------------------------------------------------------------------------
!       key    = "aerosols%complex_SOA%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSOA = v_bool

!       !------------------------------------------------------------------------
!       ! Use semi-volatile POA?
!       !------------------------------------------------------------------------
!       key    = "aerosols%complex_SOA%semivolatile_POA"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSVPOA = v_bool

!       !------------------------------------------------------------------------
!       ! Use online dust aerosols ?
!       !------------------------------------------------------------------------
!       key    = "aerosols%dust%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LDUST = v_bool

!       !------------------------------------------------------------------------
!       ! Use SO2 and HNO3 uptake on dust aerosols
!       !------------------------------------------------------------------------
!       key    = "aerosols%dust%acid_uptake_on_dust"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LDSTUP = v_bool

!       !------------------------------------------------------------------------
!       ! Use online sea-salt aerosols?
!       !------------------------------------------------------------------------
!       key    = "aerosols%sea_salt%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSSALT = v_bool

!       !------------------------------------------------------------------------
!       ! Accum mode seasalt radii bin edges [um]
!       !------------------------------------------------------------------------
!       key   = "aerosols%sea_salt%SALA_radius_bin_in_um"
!       a_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%SALA_Redge_um(1) = 0.  !_and_RoundOff( a_str(1), places=2 )
!       Config_Opt%SALA_Redge_um(2) = 0.  !_and_RoundOff( a_str(2), places=2 )

!       !------------------------------------------------------------------------
!       ! Coarse mode seasalt radii bin edges [um]
!       !------------------------------------------------------------------------
!       key   = "aerosols%sea_salt%SALC_radius_bin_in_um"
!       a_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%SALC_Redge_um(1) = 0.  !_and_RoundOff( a_str(1), places=2 )
!       Config_Opt%SALC_Redge_um(2) = 0.  !_and_RoundOff( a_str(2), places=2 )

!       !------------------------------------------------------------------------
!       ! Use marine organic aerosols?
!       !------------------------------------------------------------------------
!       key    = "aerosols%sea_salt%marine_organic_aerosols"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LMPOA = v_bool

!       !------------------------------------------------------------------------
!       ! Apply gravitational settling in stratosphere?
!       !------------------------------------------------------------------------
!       key    = "aerosols%stratosphere%settle_strat_aerosol"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LGRAVSTRAT = v_bool

!       !------------------------------------------------------------------------
!       ! Use solid polar stratospheric clouds (PSCs)?
!       !------------------------------------------------------------------------
!       key    = "aerosols%stratosphere%polar_strat_clouds%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSOLIDPSC = v_bool

!       !------------------------------------------------------------------------
!       ! Perform heterogeneous chemistry on PSCs?
!       !------------------------------------------------------------------------
!       key    = "aerosols%stratosphere%polar_strat_clouds%het_chem"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LPSCCHEM = v_bool

!       !------------------------------------------------------------------------
!       ! Allow homogeneous NAT?
!       !------------------------------------------------------------------------
!       key    = "aerosols%stratosphere%allow_homogeneous_NAT"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LHOMNUCNAT = v_bool

!       !------------------------------------------------------------------------
!       ! NAT supercooling requirement (K)
!       !------------------------------------------------------------------------
!       key   = "aerosols%stratosphere%NAT_supercooling_req_in_K"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%T_NAT_SUPERCOOL = 0.  !_and_RoundOff( v_str, places=2 )

!       !------------------------------------------------------------------------
!       ! Ice supersaturation ratio requirement
!       !------------------------------------------------------------------------
!       key   = "aerosols%stratosphere%supersat_factor_req_for_ice_nucl"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%P_ICE_SUPERSAT = 0.  !_and_RoundOff( v_str, places=2 )

!       !------------------------------------------------------------------------
!       ! Include stratospheric aerosols optical depths?
!       !------------------------------------------------------------------------
!       key    = "aerosols%stratosphere%calc_strat_aod"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSTRATOD = v_bool

!       !------------------------------------------------------------------------
!       ! Use online sulfate aerosols?
!       !------------------------------------------------------------------------
!       key    = "aerosols%sulfate%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSULF = v_bool

!       !------------------------------------------------------------------------
!       ! Use metal catalyzed oxidation of SO2?
!       !------------------------------------------------------------------------
!       key    = "aerosols%sulfate%metal_cat_SO2_oxidation"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LMETALCATSO2 = v_bool

!       !=================================================================
!       ! Error checks
!       !=================================================================

!       ! Make sure that SALA, SALC bins are contiguous
!       IF ( Config_Opt%SALA_REDGE_um(2) /= &
!          Config_Opt%SALC_REDGE_um(1)     ) THEN
!          errMsg = 'SALA and SALC bin edges are not contiguous!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF

!       ! Turn off switches for simulations that don't use aerosols
!       IF ( ( .not. Config_Opt%ITS_A_FULLCHEM_SIM )  .and. &
!          ( .not. Config_Opt%ITS_AN_AEROSOL_SIM ) ) THEN
!          Config_Opt%LSULF        = .FALSE.
!          Config_Opt%LMETALCATSO2 = .FALSE.
!          Config_Opt%LCARB        = .FALSE.
!          Config_Opt%LBRC         = .FALSE.
!          Config_Opt%LSOA         = .FALSE.
!          Config_Opt%LDUST        = .FALSE.
!          Config_Opt%LSSALT       = .FALSE.
!          Config_Opt%LMPOA        = .FALSE.
!          Config_Opt%LSVPOA       = .FALSE.
!          Config_Opt%LBCAE        = .FALSE.
!       ENDIF

!       ! Return success
!       RC = CC_SUCCESS

!       !========================================================================
!       ! Print to screen
!       !========================================================================
!       IF ( Config_Opt%amIRoot ) THEN
!          WRITE( 6, 90  ) 'AEROSOL SETTINGS'
!          WRITE( 6, 95  ) '----------------'
!          WRITE( 6, 100 ) 'Online SULFATE AEROSOLS?    : ', Config_Opt%LSULF
!          WRITE( 6, 100 ) 'Metal catalyzed SO2 ox.?    : ', Config_Opt%LMETALCATSO2
!          WRITE( 6, 100 ) 'Online CARBON AEROSOLS?     : ', Config_Opt%LCARB
!          WRITE( 6, 100 ) 'Brown Carbon Aerosol?       : ', Config_Opt%LBRC
!          WRITE( 6, 100 ) 'BC Absorption Enhancement?  : ', Config_Opt%LBCAE
!          WRITE( 6, 105 ) 'Hydrophilic BC AE factor    : ', Config_Opt%BCAE_1
!          WRITE( 6, 105 ) 'Hydrophobic BC AE factor    : ', Config_Opt%BCAE_2
!          WRITE( 6, 100 ) 'Online COMPLEX SOA?         : ', Config_Opt%LSOA
!          WRITE( 6, 100 ) 'Semivolatile POA?           : ', Config_Opt%LSVPOA
!          WRITE( 6, 100 ) 'Online DUST AEROSOLS?       : ', Config_Opt%LDUST
!          WRITE( 6, 100 ) 'Acid uptake on dust?        : ', Config_Opt%LDSTUP
!          WRITE( 6, 100 ) 'Online SEA SALT AEROSOLS?   : ', Config_Opt%LSSALT
!          WRITE( 6, 110 ) 'Accum  SEA SALT radii [um]  : ',                     &
!             Config_Opt%SALA_REDGE_um(1), &
!             Config_Opt%SALA_REDGE_um(2)
!          WRITE( 6, 110 ) 'Coarse SEA SALT radii [um]  : ',                     &
!             Config_Opt%SALC_REDGE_um(1), &
!             Config_Opt%SALC_REDGE_um(2)
!          WRITE( 6, 100 ) 'MARINE ORGANIC AEROSOLS?    : ', Config_Opt%LMPOA
!          WRITE( 6, 100 ) 'Settle strat. aerosols?     : ', Config_Opt%LGRAVSTRAT
!          WRITE( 6, 100 ) 'Online SOLID PSC aerosols?  : ', Config_Opt%LSOLIDPSC
!          WRITE( 6, 100 ) 'Allow hom. NAT nucleation?  : ', Config_Opt%LHOMNUCNAT
!          WRITE( 6, 120 ) 'NAT supercooling requirement: ',                     &
!             Config_Opt%T_NAT_SUPERCOOL
!          WRITE( 6, 120 ) 'Ice supersaturation req.    : ',                     &
!             ((Config_Opt%P_ICE_SUPERSAT-1)*1.e+2_fp)
!          WRITE( 6, 100 ) 'Perform PSC het. chemistry? : ', Config_Opt%LPSCCHEM
!          WRITE( 6, 100 ) 'Use strat. aerosol OD?      : ', Config_Opt%LSTRATOD
!       ENDIF

! 90    FORMAT( /, A                 )
! 95    FORMAT( A                    )
! 100   FORMAT( A, L5                )
! 105   FORMAT( A, f8.2              )
! 110   FORMAT( A, f8.2, ' - ', f8.2 )
! 120   FORMAT( A, f8.2, 'K'         )

!    END SUBROUTINE Config_Aerosol
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: config_co
! !
! ! !DESCRIPTION: Copies CO simulation information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_CO( Config, Config_Opt, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod,      ONLY : OptConfig
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       LOGICAL                      :: v_bool

!       ! Strings
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=512)           :: errMsg
!       CHARACTER(LEN=QFYAML_NamLen) :: key

!       !========================================================================
!       ! Config_CO begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = ''
!       thisLoc = ' -> at Config_CO2 (in module CATChem/src/core/input_mod.F90)'

!       !------------------------------------------------------------------------
!       ! Use P(CO) from CH4 (archived from a fullchem simulation)?
!       !------------------------------------------------------------------------
!       key    = "CO_simulation_options%use_fullchem_PCO_from_CH4"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LPCO_CH4 = v_bool

!       !------------------------------------------------------------------------
!       ! Use P(CO) from NMVOC (archived from a fullchem simulation)?
!       !------------------------------------------------------------------------
!       key    = "CO_simulation_options%use_fullchem_PCO_from_NMVOC"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LPCO_NMVOC = v_bool

!       ! !========================================================================
!       ! ! Print to screen
!       ! !========================================================================
!       ! IF ( Config_Opt%ITS_A_TAGCO_SIM .and. Config_Opt%amIRoot ) THEN
!       !    WRITE(6,90 ) 'TAGGED CO SIMULATION SETTINGS'
!       !    WRITE(6,95 ) '(overwrites any other settings related to CO)'
!       !    WRITE(6,95 ) '---------------------------------------------'
!       !    WRITE(6,100) 'Use full chem. P(CO) from CH4?   :', Config_Opt%LPCO_CH4
!       !    WRITE(6,100) 'Use full chem. P(CO) from NMVOC? :', Config_Opt%LPCO_NMVOC
!       ! ENDIF

!       ! FORMAT statements
! 90    FORMAT( /, A   )
! 95    FORMAT( A      )
! ! 100   FORMAT( A, L5  )

!    END SUBROUTINE Config_CO
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: config_co2
! !
! ! !DESCRIPTION: Copies CO2 simulation information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_CO2( Config, Config_Opt, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod,      ONLY : OptConfig
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       LOGICAL                      :: v_bool

!       ! Strings
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=512)           :: errMsg
!       CHARACTER(LEN=QFYAML_NamLen) :: key

!       !========================================================================
!       ! Config_CO2 begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = ''
!       thisLoc = ' -> at Config_CO2 (in module CATChem/src/core/input_mod.F90)'

!       !------------------------------------------------------------------------
!       ! Use Fossil Fuel emissions?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%fossil_fuel_emissions"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LFOSSIL = v_bool

!       !------------------------------------------------------------------------
!       ! Use Ocean Exchange?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%ocean_exchange"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LOCEAN = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on (balanced) biosphere with diurnal cycle?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%balanced_biosphere_exchange"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LBIODIURNAL = v_bool

!       !------------------------------------------------------------------------
!       ! Use Net Terrestrial Exchange Climatology?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%net_terrestrial_exchange"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LBIONETCLIM = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on Ship emissions?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%ship_emissions"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSHIP = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on Aviation emissions?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%aviation_emissions"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LPLANE = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on CO2 3D chemical source and surface correction?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%sources%3D_chemical_oxidation_source"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LCHEMCO2 = v_bool

!       !------------------------------------------------------------------------
!       ! Background CO2 (no emissions or exchange) for Tagged-CO2 runs
!       !------------------------------------------------------------------------
!       key = "CO2_simulation_options%tagged_species%save_fossil_fuel_in_background"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LFFBKGRD = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on biosphere and ocean exchange region tagged species?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%tagged_species%tag_bio_and_ocean_CO2"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LBIOSPHTAG = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on fossil fuel emission region tagged species?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%tagged_species%tag_land_fossil_fuel_CO2"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LFOSSILTAG = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on global ship emissions tagged species?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%tagged_species%tag_global_ship_CO2"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSHIPTAG = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on global aircraft emissions tagged species?
!       !------------------------------------------------------------------------
!       key    = "CO2_simulation_options%tagged_species%tag_global_aircraft_CO2"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LPLANETAG = v_bool

!       ! !=================================================================
!       ! ! Print to screen
!       ! !=================================================================
!       ! IF ( Config_Opt%ITS_A_CO2_SIM .and. Config_Opt%amIRoot ) THEN
!          WRITE( 6,90  ) 'CO2 SIMULATION SETTINGS'
!          WRITE( 6,95  ) '(overwrites any other settings related to CO2)'
!          WRITE( 6,95  ) '----------------------------------------------'
!          WRITE( 6,100 ) 'National Fossil Fuel Emission :', Config_Opt%LFOSSIL
!          WRITE( 6,100 ) 'Ocean CO2 Uptake/Emission     :', Config_Opt%LOCEAN
!          WRITE( 6,100 ) 'Biosphere seas/diurnal cycle  :', Config_Opt%LBIODIURNAL
!          WRITE( 6,100 ) 'Net Terr Exch - Climatology   :', Config_Opt%LBIONETCLIM
!          WRITE( 6,100 ) 'Intl/Domestic Ship emissions  :', Config_Opt%LSHIP
!          WRITE( 6,100 ) 'Intl/Domestic Aviation emiss  :', Config_Opt%LPLANE
!          WRITE( 6,100 ) 'CO2 from oxidation (CO,CH4,..):', Config_Opt%LCHEMCO2
!          WRITE( 6, 95 ) 'Tagged CO2 settings'
!          WRITE( 6,100 ) '  Save Fossil CO2 in Bckgrnd  :', Config_Opt%LFFBKGRD
!          WRITE( 6,100 ) '  Tag Biosphere/Ocean CO2     :', Config_Opt%LBIOSPHTAG
!          WRITE( 6,100 ) '  Tag Fossil Fuel CO2         :', Config_Opt%LFOSSILTAG
!          WRITE( 6,100 ) '  Tag Global Ship CO2         :', Config_Opt%LSHIPTAG
!          WRITE( 6,100 ) '  Tag Global Aviation CO2     :', Config_Opt%LPLANETAG
!       ! ENDIF

!       ! FORMAT statements
! 90    FORMAT( /, A  )
! 95    FORMAT( A     )
! 100   FORMAT( A, L5 )

!    END SUBROUTINE Config_CO2
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: config_chemistry
! !
! ! !DESCRIPTION: Copies chemistry information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_Chemistry( Config, Config_Opt, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod, ONLY : OptConfig
!       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       INTEGER                      :: N
!       LOGICAL                      :: v_bool

!       ! Strings
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=512)           :: errMsg
!       CHARACTER(LEN=QFYAML_NamLen) :: key
!       CHARACTER(LEN=QFYAML_StrLen) :: v_str

!       !========================================================================
!       ! Config_Chemistry begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = ''
!       thisLoc = ' -> at Config_Chemistry (in module CATChem/src/core/input_mod.F90)'

!       !------------------------------------------------------------------------
!       ! Turn on chemistry?
!       !------------------------------------------------------------------------
!       key    = "operations%chemistry%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LCHEM = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on linearized chemistry above chemistry grid?
!       !------------------------------------------------------------------------
!       key    = "operations%chemistry%linear_chemistry_aloft%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LINEAR_CHEM = v_bool

!       !------------------------------------------------------------------------
!       ! Use Linoz for ozone above chemistry grid? (Otherwise, Synoz is used)
!       !------------------------------------------------------------------------
!       key    = "operations%chemistry%linear_chemistry_aloft%use_linoz_for_O3"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LLINOZ = v_bool
!       IF ( .not. Config_Opt%LLINOZ ) Config_Opt%LSYNOZ = .TRUE.

!       !------------------------------------------------------------------------
!       ! Turn on online stratospheric H2O?
!       !------------------------------------------------------------------------
!       key    = "operations%chemistry%active_strat_H2O%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LACTIVEH2O = v_bool

!       !------------------------------------------------------------------------
!       ! Use a more conservative boundary condition for strat. H2O?
!       !------------------------------------------------------------------------
!       key    = "operations%chemistry%active_strat_H2O%use_static_bnd_cond"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LStaticH2OBC = v_bool

!       !------------------------------------------------------------------------
!       ! GAMMA HO2 ?
!       !------------------------------------------------------------------------
!       key   = "operations%chemistry%gamma_HO2"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%GAMMA_HO2 = 0.  !_and_RoundOff( v_str, places=2 )

!       !------------------------------------------------------------------------
!       ! Auto-reduce solver options (hplin, 10/3/22)
!       ! autoreduce_solver:
!       !   activate: false
!       !   use_target_threshold:
!       !     activate: true
!       !     oh_tuning_factor: 0.00005
!       !     no2_tuning_factor: 0.0001
!       !   use_absolute_threshold:
!       !     scale_by_pressure: true
!       !     absolute_threshold: 100.0
!       !   keep_halogens_active: false
!       !   append_in_internal_timestep: false
!       !------------------------------------------------------------------------
!       key   = "operations%chemistry%autoreduce_solver%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%USE_AUTOREDUCE = v_bool

!       ! Use target species (OH, NO2) based threshold?
!       key   = "operations%chemistry%autoreduce_solver%use_target_threshold%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_IS_KEY_THRESHOLD = v_bool

!       ! ... OH and NO2 tuning factors?
!       key   = "operations%chemistry%autoreduce_solver%use_target_threshold%oh_tuning_factor"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_TUNING_OH = 0.  !_and_RoundOff( v_str, places=0 )

!       key   = "operations%chemistry%autoreduce_solver%use_target_threshold%no2_tuning_factor"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_TUNING_NO2 = 0.  !_and_RoundOff( v_str, places=0 )

!       ! If not target species, absolute rate threshold
!       key   = "operations%chemistry%autoreduce_solver%use_absolute_threshold%absolute_threshold"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_THRESHOLD = 0.  !_and_RoundOff( v_str, places=0 )

!       ! Would this absolute threshold be scaled by pressure?
!       key   = "operations%chemistry%autoreduce_solver%use_absolute_threshold%scale_by_pressure"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_IS_PRS_THRESHOLD = v_bool

!       ! Keep halogens active?
!       key   = "operations%chemistry%autoreduce_solver%keep_halogens_active"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_IS_KEEPACTIVE = v_bool

!       ! Append species over the course of the external time step
!       ! (aka. in internal timesteps?)
!       key   = "operations%chemistry%autoreduce_solver%append_in_internal_timestep"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%AUTOREDUCE_IS_APPEND = v_bool

!       ! Return success
!       RC = CC_SUCCESS

!       !========================================================================
!       ! Print to screen
!       !========================================================================
!       IF ( Config_Opt%amIRoot ) THEN
!          WRITE( 6,90  ) 'CHEMISTRY SETTINGS'
!          WRITE( 6,95  ) '------------------'
!          WRITE( 6,100 ) 'Turn on chemistry?          : ', Config_Opt%LCHEM
!          WRITE( 6,100 ) 'Use linear. mesospheric chem: ', Config_Opt%LINEAR_CHEM
!          WRITE( 6,100 ) ' => Use Linoz for O3?       : ', Config_Opt%LLINOZ
!          WRITE( 6,100 ) 'Online strat. H2O?          : ', Config_Opt%LACTIVEH2O
!          WRITE( 6,100 ) 'Use robust strat H2O BC?    : ', Config_Opt%LStaticH2OBC
!          WRITE( 6,110 ) 'GAMMA HO2                   : ', Config_Opt%GAMMA_HO2
!          WRITE( 6,100 ) 'Use auto-reduce solver?     : ', Config_Opt%USE_AUTOREDUCE
!          IF ( Config_Opt%AUTOREDUCE_IS_KEY_THRESHOLD ) THEN
!             WRITE( 6,100 ) 'Use target species threshold: ', Config_Opt%AUTOREDUCE_IS_KEY_THRESHOLD
!             WRITE( 6,130 ) 'OH tuning factor:             ', Config_Opt%AUTOREDUCE_TUNING_OH
!             WRITE( 6,130 ) 'NO2 tuning factor:            ', Config_Opt%AUTOREDUCE_TUNING_NO2
!          ELSE
!             WRITE( 6,120 ) 'Absolute AR threshold       : ', Config_Opt%AUTOREDUCE_THRESHOLD
!             WRITE( 6,100 ) 'Use prs. dependent thres?   : ', Config_Opt%AUTOREDUCE_IS_PRS_THRESHOLD
!          ENDIF
!          WRITE( 6,100 ) 'Keep halogen spec. active?  : ', Config_Opt%AUTOREDUCE_IS_KEEPACTIVE
!          WRITE( 6,100 ) 'Use append in auto-reduce?  : ', Config_Opt%AUTOREDUCE_IS_APPEND
!       ENDIF

!       ! FORMAT statements
! 90    FORMAT( /, A    )
! 95    FORMAT( A       )
! 100   FORMAT( A, L5   )
! 110   FORMAT( A, F4.2 )
! 120   FORMAT( A, F5.1 )
! 130   FORMAT( A, ES7.1 )

!    END SUBROUTINE Config_Chemistry
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: Config_RRTMG
! !
! ! !DESCRIPTION: Copies RRTMG information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_RRTMG( Config, Config_Opt, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod, ONLY : OptConfig
!       ! ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !
! ! !REMARKS:
! !  Flux outputs are now scheduled in the HISTORY.rc file, and the relevant
! !  fields of Config_Opt will be populated in the RRTMG module routine
! !  Init_RRTMG_Indices (called at startup).
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       INTEGER                      :: I
!       INTEGER                      :: N
!       LOGICAL                      :: v_bool

!       ! Strings
!       CHARACTER(LEN=20)            :: str
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=512)           :: errMsg
!       CHARACTER(LEN=QFYAML_NamLen) :: key
!       CHARACTER(LEN=QFYAML_NamLen) :: a_str(3)
!       CHARACTER(LEN=QFYAML_StrLen) :: v_str

!       !========================================================================
!       ! Config_RRTMG begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = ''
!       thisLoc = ' -> at Config_RRTMG (in module CATChem/src/core/input_mod.F90)'

!       !------------------------------------------------------------------------
!       ! Turn on RRTMG?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LRAD = v_bool

!       !------------------------------------------------------------------------
!       ! AOD wavelength selection? (You can have up to 3)
!       !------------------------------------------------------------------------
!       key   = "operations%rrtmg_rad_transfer_model%aod_wavelengths_in_nm"
!       a_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str,                          &
!          "",     RC,          dynamic_size=.TRUE.            )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF

!       ! ! Copy values into Config_Opt
!       ! I = 0
!       ! DO N = 1, SIZE( a_str )
!       !    IF ( a_str(N) == MISSING_STR ) EXIT
!       !    I = I + 1
!       !    Config_Opt%nWvSelect      = I
!       !    Config_Opt%StrWvSelect(I) = TRIM( ADJUSTL( a_str(N) ) )
!       !    Config_Opt%WvSelect(I)    = 0.  !_and_RoundOff( a_str(N), places=2 )
!       ! ENDDO

!       !------------------------------------------------------------------------
!       ! Turn on LW radiation calculation?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%longwave_fluxes"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LLWRAD = v_bool

!       !------------------------------------------------------------------------
!       ! Turn on SW radiation calculation?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%shortwave_fluxes"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSWRAD = v_bool

!       !------------------------------------------------------------------------
!       ! Calculate for clear-sky?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%clear_sky_flux"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSKYRAD(1) = v_bool

!       !------------------------------------------------------------------------
!       ! Calculate for all-sky?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%all_sky_flux"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%LSKYRAD(2) = v_bool

!       ! !------------------------------------------------------------------------
!       ! ! Value to use (in ppmv) for CO2?
!       ! !------------------------------------------------------------------------
!       ! key    = "operations%rrtmg_rad_transfer_model%co2_ppmv"
!       ! v_str = MISSING_STR
!       ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       ! IF ( RC /= CC_SUCCESS ) THEN
!       !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
!       !    CALL CC_Error( errMsg, RC, thisLoc )
!       !    RETURN
!       ! ENDIF
!       ! Config_Opt%RRTMG_CO2_ppmv = 0.  !_and_Roundoff( v_str, places=2 )

!       !------------------------------------------------------------------------
!       ! Use the fixed dynamical heating assumption?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%fixed_dyn_heating"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%RRTMG_FDH = v_bool

!       !------------------------------------------------------------------------
!       ! Allow seasonal adjustment?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%seasonal_fdh"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%RRTMG_SEFDH = v_bool

!       !------------------------------------------------------------------------
!       ! Extend dynamical heating adjustment to TOA?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%fdh_to_toa"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%RRTMG_SA_TOA = v_bool

!       !------------------------------------------------------------------------
!       ! Read in dynamical heating data?
!       !------------------------------------------------------------------------
!       key    = "operations%rrtmg_rad_transfer_model%read_dyn_heating"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%Read_Dyn_Heating = v_bool

!       !========================================================================
!       ! Error check settings
!       !========================================================================
! #ifndef RRTMG
!       ! Use of RRTMG necessitates recompilation
!       IF ( Config_Opt%LRAD ) THEN
!          errMsg = 'LRAD=T but RRTMG is not defined at compile time!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
! #endif

!       ! Make sure radiation switches are turned off if RRTMG is off
!       IF ( .not. Config_Opt%LRAD ) THEN

!          IF ( Config_Opt%LLWRAD ) THEN
!             errMsg = 'Cannot have LW fluxes turned on without RRTMG!'
!             CALL CC_Error( errMsg, RC, thisLoc )
!             RETURN
!          ENDIF

!          IF ( Config_Opt%LSWRAD ) THEN
!             errMsg = 'Cannot have SW fluxes turned on without RRTMG!'
!             CALL CC_Error( errMsg, RC, thisLoc )
!             RETURN
!          ENDIF

!          IF ( Config_Opt%LSKYRAD(1) ) THEN
!             errMsg = 'Cannot have clear-sky flux turned on without RRTMG!'
!             CALL CC_Error( errMsg, RC, thisLoc )
!             RETURN
!          ENDIF

!          IF ( Config_Opt%LSKYRAD(2) ) THEN
!             errMsg = 'Cannot have all-sky flux turned on without RRTMG!'
!             CALL CC_Error( errMsg, RC, thisLoc )
!          ENDIF
!       ENDIF

! #ifndef MODEL_GCHPCTM
!       If (Config_Opt%RRTMG_FDH) Then
!          errMsg = 'Fixed dynamical heating in RRTMG is currently only available in GCHP'
!          CALL CC_Error( errMsg, RC, thisLoc )
!       End If
! #endif

!       If (Config_Opt%RRTMG_SEFDH.and.(.not.Config_Opt%RRTMG_FDH)) Then
!          errMsg = 'Cannot have seasonally evolving FDH without enabling FDH!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!       End If

!       !========================================================================
!       ! Print to screen
!       !========================================================================
!       IF ( Config_Opt%amIRoot ) THEN
!          WRITE( 6, 90 ) 'RRTMG SETTINGS'
!          WRITE( 6, 95 ) '--------------'
!          DO N=1, Config_Opt%NWVSELECT
!             WRITE( 6, 115     ) 'AOD output wavelength (nm)  : ',              &
!                Config_Opt%WVSELECT(N)
!          ENDDO
!          WRITE( 6, 100 ) 'Turn on radiation?          : ', Config_Opt%LRAD
!          WRITE( 6, 100 ) 'Consider longwave?          : ', Config_Opt%LLWRAD
!          WRITE( 6, 100 ) 'Consider shortwave?         : ', Config_Opt%LSWRAD
!          WRITE( 6, 100 ) 'Clear-sky flux?             : ', Config_Opt%LSKYRAD(1)
!          WRITE( 6, 100 ) 'All-sky flux?               : ', Config_Opt%LSKYRAD(2)
!          WRITE( 6, 115 ) 'CO2 VMR in ppmv             : ', Config_Opt%RRTMG_CO2_ppmv
!          WRITE( 6, 100 ) 'Fixed dyn. heat. assumption?: ', Config_Opt%RRTMG_FDH
!          WRITE( 6, 100 ) ' --> Seasonal evolution?    : ', Config_Opt%RRTMG_SEFDH
!          WRITE( 6, 100 ) ' --> Extend to TOA?         : ', Config_Opt%RRTMG_SA_TOA
!          WRITE( 6, 100 ) ' --> Read in dyn. heating?  : ', Config_Opt%Read_Dyn_Heating
!       ENDIF

!       ! FORMAT statements
! 90    FORMAT( /, A    )
! 95    FORMAT( A       )
! 100   FORMAT( A, L5   )
! 110   FORMAT( A, I5   )
! 115   FORMAT( A, F7.1 )
! ! 120   FORMAT( A, 11I1 )

!    END SUBROUTINE Config_RRTMG
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: Config_Photolysis
! !
! ! !DESCRIPTION: Copies photolysis information from the Config object
! !  to Config_Opt, and does necessary checks.
! !\\
! !\\
! ! !INTERFACE:
! !
!    SUBROUTINE Config_Photolysis( Config, Config_Opt, RC )
! !
! ! !USES:
! !
!       USE Error_Mod
!       USE Config_Opt_Mod, ONLY : OptConfig
!       ! ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! !
! ! !INPUT/OUTPUT PARAMETERS:
! !
!       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
!       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! !
! ! !OUTPUT PARAMETERS:
! !
!       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       ! Scalars
!       LOGICAL                      :: v_bool
!       REAL(yp)                     :: v_real

!       ! Strings
!       CHARACTER(LEN=255)           :: thisLoc
!       CHARACTER(LEN=512)           :: errMsg
!       CHARACTER(LEN=QFYAML_NamLen) :: key
!       CHARACTER(LEN=QFYAML_StrLen) :: v_str

!       !========================================================================
!       ! Config_Photolysis begins here!
!       !========================================================================

!       ! Initialize
!       RC      = CC_SUCCESS
!       errMsg  = ''
!       thisLoc = ' -> at Config_Photolysis (in module CATChem/src/core/input_mod.F90)'

!       !------------------------------------------------------------------------
!       ! Turn on photolysis
!       !------------------------------------------------------------------------

!       key    = "operations%photolysis%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%Do_Photolysis = v_bool

!       ! !------------------------------------------------------------------------
!       ! ! Directories with photolysis input files
!       ! !------------------------------------------------------------------------

!       ! key   = "operations%photolysis%input_directories%fastjx_input_dir"
!       ! v_str = MISSING_STR
!       ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       ! IF ( RC /= CC_SUCCESS ) THEN
!       !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
!       !    CALL CC_Error( errMsg, RC, thisLoc )
!       !    RETURN
!       ! ENDIF
!       ! Config_Opt%FAST_JX_DIR = TRIM( v_str )

!       ! key   = "operations%photolysis%input_directories%cloudj_input_dir"
!       ! v_str = MISSING_STR
!       ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       ! IF ( RC /= CC_SUCCESS ) THEN
!       !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
!       !    CALL CC_Error( errMsg, RC, thisLoc )
!       !    RETURN
!       ! ENDIF
!       ! Config_Opt%CloudJ_DIR = TRIM( v_str )

!       ! !------------------------------------------------------------------------
!       ! ! Use online ozone in extinction calculations for FAST-JX?
!       ! !------------------------------------------------------------------------
!       ! key    = "operations%photolysis%overhead_O3%use_online_O3_from_model"
!       ! v_bool = MISSING_BOOL
!       ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       ! IF ( RC /= CC_SUCCESS ) THEN
!       !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
!       !    CALL CC_Error( errMsg, RC, thisLoc )
!       !    RETURN
!       ! ENDIF
!       ! Config_Opt%USE_ONLINE_O3 = v_bool

!       !------------------------------------------------------------------------
!       ! Use ozone columns from met fields?
!       !------------------------------------------------------------------------
!       key    = "operations%photolysis%overhead_O3%use_column_O3_from_met"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%USE_O3_FROM_MET = v_bool

!       !------------------------------------------------------------------------
!       ! Use ozone columns from TOMS?
!       !------------------------------------------------------------------------
!       key    = "operations%photolysis%overhead_O3%use_TOMS_SBUV_O3"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%USE_TOMS_O3 = v_bool

!       !------------------------------------------------------------------------
!       ! Photoylse nitrate aerosol?
!       !------------------------------------------------------------------------
!       key    = "operations%photolysis%photolyze_nitrate_aerosol%activate"
!       v_bool = MISSING_BOOL
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%hvAerNIT = v_bool

!       !------------------------------------------------------------------------
!       ! Scalar for JHNO3 for photoylsing NITs aerosol
!       !------------------------------------------------------------------------
!       key    = &
!          "operations%photolysis%photolyze_nitrate_aerosol%NITs_Jscale_JHNO3"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%hvAerNIT_JNITs = 0. !0.  !_and_RoundOff( v_str, places=3 )

!       !------------------------------------------------------------------------
!       ! scalar for JHNO3 for photoylsing NIT aerosol (TMS, 23/08/18)
!       !------------------------------------------------------------------------
!       key    = &
!          "operations%photolysis%photolyze_nitrate_aerosol%NIT_Jscale_JHNO2"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%hvAerNIT_JNIT = 0. !0.  !_and_RoundOff( v_str, places=3 )

!       !------------------------------------------------------------------------
!       ! Fraction for JNITS/NIT channel A (HNO2) for NITs photoylsis
!       !------------------------------------------------------------------------
!       key   = &
!          "operations%photolysis%photolyze_nitrate_aerosol%percent_channel_A_HONO"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%JNITChanA = 0.  !_and_RoundOff( v_str, places=3 )

!       !------------------------------------------------------------------------
!       ! Fraction for JNITs/NIT channel B (NO2) for NITs photoylsis
!       !------------------------------------------------------------------------
!       key    = &
!          "operations%photolysis%photolyze_nitrate_aerosol%percent_channel_B_NO2"
!       v_str = MISSING_STR
!       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
!       IF ( RC /= CC_SUCCESS ) THEN
!          errMsg = 'Error parsing ' // TRIM( key ) // '!'
!          CALL CC_Error( errMsg, RC, thisLoc )
!          RETURN
!       ENDIF
!       Config_Opt%JNITChanB = 0.  !_and_RoundOff( v_str, places=3 )

!       !========================================================================
!       ! Error check settings
!       !========================================================================

! ! #ifndef MODEL_GEOS
! !       ! Cannot use Synoz with linearized mesospheric chemistry
! !       IF ( Config_Opt%ITS_A_FULLCHEM_SIM .and. Config_Opt%LINEAR_CHEM ) THEN
! !          IF (.not.Config_Opt%LLINOZ) THEN
! !             errMsg = 'Cannot use Synoz with linearized mesospheric chem.!'
! !             CALL CC_Error( errMsg, RC, thisLoc )
! !             RETURN
! !          ENDIF
! !       ENDIF
! ! #endif

!       ! ! FAST-JX is only used for fullchem and mercury
!       ! IF ( Config_Opt%ITS_A_FULLCHEM_SIM   .or.                                 &
!       !    Config_Opt%ITS_AN_AEROSOL_SIM   .or.                                 &
!       !    Config_Opt%ITS_A_MERCURY_SIM  ) THEN

!       !    ! Make sure either O3 from met or TOMS is selected
!       !    IF ( .not. Config_Opt%USE_O3_FROM_MET   .and.                          &
!       !       .not. Config_Opt%USE_TOMS_O3     ) THEN
!       !       errMsg = 'Must select either O3 from met or TOMS/SBUV O3'          &
!       !          // 'for O3 values above the chemistry grid!'
!       !       CALL CC_Error( errMsg, RC, thisLoc )
!       !       RETURN
!       !    ENDIF
!       !    IF ( Config_Opt%USE_O3_FROM_MET .and. &
!       !       Config_Opt%USE_TOMS_O3 ) THEN
!       !       errMsg = 'Must select either O3 from met or TOMS/SBUV O3'          &
!       !          // 'for O3 values above the chemistry grid!'
!       !       CALL CC_Error( errMsg, RC, thisLoc )
!       !       RETURN
!       !    ENDIF

!       !    ! Make sure the aerosol-only simulation gets O3 from met or TOMS
!       !    IF ( Config_Opt%ITS_AN_AEROSOL_SIM ) THEN
!       !       IF ( Config_Opt%USE_ONLINE_O3 ) THEN
!       !          errMsg= 'Cannot use online O3 for specialty simulations! '      &
!       !             // 'Select O3 from met or TOMS O3 instead.'
!       !          CALL CC_Error( errMsg, RC, thisLoc )
!       !          RETURN
!       !       ENDIF
!       !    ENDIF

!       ! ELSE

!       !    ! If not a simulation that uses photolysis, set options to FALSE
!       !    Config_Opt%USE_ONLINE_O3   = .FALSE.
!       !    Config_Opt%USE_O3_FROM_MET = .FALSE.
!       !    Config_Opt%USE_TOMS_O3     = .FALSE.

!       ! ENDIF

!       ! ! Turn off switches for simulations that don't use aerosols
!       ! IF ( ( .not. Config_Opt%ITS_A_FULLCHEM_SIM )                        .and. &
!       !    ( .not. Config_Opt%ITS_AN_AEROSOL_SIM ) ) THEN
!       !    Config_Opt%hvAerNIT       = .FALSE.
!       !    Config_Opt%hvAerNIT_JNITs = MISSING_REAL
!       !    Config_Opt%hvAerNIT_JNIT  = MISSING_REAL
!       !    Config_Opt%JNITChanA      = MISSING_REAL
!       !    Config_Opt%JNITChanB      = MISSING_REAL
!       ! ENDIF

!       ! Return success
!       RC = CC_SUCCESS

!       !========================================================================
!       ! Print to screen
!       !========================================================================
!       IF ( Config_Opt%amIRoot ) THEN
!          WRITE( 6,90  ) 'PHOTOLYSIS SETTINGS'
!          WRITE( 6,95  ) '-------------------'
!          WRITE( 6,100 ) 'Turn on photolysis?         : ', Config_Opt%Do_Photolysis
!          ! WRITE( 6,120 ) 'FAST-JX input directory     : ',                      &
!          !    TRIM( Config_Opt%FAST_JX_DIR )
!          ! WRITE( 6,120 ) 'Cloud-J input directory     : ',                      &
!          !    TRIM( Config_Opt%CloudJ_Dir )
!          ! WRITE( 6,100 ) 'Use online ozone?           : ', Config_Opt%USE_ONLINE_O3
!          WRITE( 6,100 ) 'Use ozone from met?         : ',                      &
!             Config_Opt%USE_O3_FROM_MET
!          WRITE( 6,100 ) 'Use TOMS/SBUV ozone?        : ', Config_Opt%USE_TOMS_O3
!          WRITE( 6,100 ) 'Photolyse nitrate aerosol?  : ', Config_Opt%hvAerNIT
!          WRITE( 6,105 ) 'JNITs scaling of JHNO3      : ', Config_Opt%hvAerNIT_JNITs
!          WRITE( 6,105 ) 'JNIT scaling of JHNO3       : ', Config_Opt%hvAerNIT_JNIT
!          WRITE( 6,105 ) 'JNIT(s) channel A (HONO)    : ', Config_Opt%JNITChanA
!          WRITE( 6,105 ) 'JNIT(s) channel B (NO2)     : ', Config_Opt%JNITChanB
!          ! Write more info
!          IF ( Config_Opt%USE_ONLINE_O3 ) THEN
!             WRITE( 6, 95 ) ''
!             WRITE( 6, 95 ) 'NOTE ABOUT OVERHEAD O3 FOR FAST-JX:'
!             WRITE( 6, 95 ) ' Online O3 from CATChem will be used'
!             WRITE( 6, 95 ) ' to weight the O3 column within the'
!             WRITE( 6, 95 ) ' chemistry grid and O3 from met or TOMS'
!             WRITE( 6, 95 ) ' will be used outside the chemistry grid.'
!          ENDIF
!       ENDIF

!       ! FORMAT statements
! 90    FORMAT ( /, A    )
! 95    FORMAT( A       )
! 100   FORMAT( A, L5   )
! 105   FORMAT( A, F8.3 )
! ! 110   FORMAT( A, F4.2 )
! ! 120   FORMAT( A, A    )

!    END SUBROUTINE Config_Photolysis
! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_convection_mixing
! ! !
! ! ! !DESCRIPTION: Copies convection & PBL mixing information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_Convection_Mixing( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAl                      :: v_bool

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=512)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key

! !       !========================================================================
! !       ! Config_Convection_Mixing begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ""
! !       thisLoc = &
! !          ' -> at Config_Convection_Mixing (in module CATChem/src/core/input_mod.F90)'

! !       ! !------------------------------------------------------------------------
! !       ! ! Turn on convection?
! !       ! !------------------------------------------------------------------------
! !       ! key    = "operations%convection%activate"
! !       ! v_bool = MISSING_BOOL
! !       ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       ! IF ( RC /= CC_SUCCESS ) THEN
! !       !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !       !    CALL CC_Error( errMsg, RC, thisLoc )
! !       !    RETURN
! !       ! ENDIF
! !       ! Config_Opt%LCONV = v_bool

! !       ! !------------------------------------------------------------------------
! !       ! ! Turn on PBL mixing
! !       ! !------------------------------------------------------------------------
! !       ! key    = "operations%pbl_mixing%activate"
! !       ! v_bool = MISSING_BOOL
! !       ! CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       ! IF ( RC /= CC_SUCCESS ) THEN
! !       !    errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !       !    CALL CC_Error( errMsg, RC, thisLoc )
! !       !    RETURN
! !       ! ENDIF
! !       ! Config_Opt%LTURB = v_bool

! !       !------------------------------------------------------------------------
! !       ! Use non-local PBL mixing?
! !       !------------------------------------------------------------------------
! !       key    = "operations%pbl_mixing%use_non_local_pbl"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LNLPBL = v_bool

! !       ! Set the PBL drydep flag. This determines if dry deposition is
! !       ! applied (and drydep frequencies are calculated) over the entire
! !       ! PBL or the first model layer only. For now, set this value
! !       ! automatically based upon the selected PBL scheme: 1st model layer
! !       ! for the non-local PBL scheme, full PBL for the full-mixing scheme.
! !       Config_Opt%PBL_DRYDEP = ( .not. Config_Opt%LNLPBL )

! !       ! Return success
! !       RC = CC_SUCCESS

! !       !=========================================================================
! !       ! Print to screen
! !       !=========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, 90  ) 'CONVECTION SETTINGS'
! !          WRITE( 6, 95  ) '-------------------'
! !          WRITE( 6, 100 ) 'Turn on cloud convection?   : ', Config_Opt%LCONV

! !          WRITE( 6, 90  ) 'PBL MIXING SETTINGS'
! !          WRITE( 6, 95  ) '-------------------'
! !          WRITE( 6, 100 ) 'Turn on PBL mixing?         : ', Config_Opt%LTURB
! !          WRITE( 6, 100 ) 'Turn on non-local PBL?      : ', Config_Opt%LNLPBL
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A  )
! ! 95    FORMAT( A     )
! ! 100   FORMAT( A, L5 )

! !    END SUBROUTINE Config_Convection_Mixing
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_drydep_wetdep
! ! !
! ! ! !DESCRIPTION: Copies drydep and wetdep information from the Config object
! ! !  to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_DryDep_WetDep( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL                      :: v_bool
! !       INTEGER                      :: v_int

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=512)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       !========================================================================
! !       ! Config_DryDep_WetDep begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_DryDep_WetDep (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Turn on drydep?
! !       !------------------------------------------------------------------------
! !       key    = "operations%dry_deposition%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LDRYD = v_bool

! !       !------------------------------------------------------------------------
! !       ! Turn on CO2 effect on drydep?
! !       !------------------------------------------------------------------------
! !       key    = "operations%dry_deposition%CO2_effect%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%CO2_EFFECT = v_bool

! !       !------------------------------------------------------------------------
! !       ! CO2 level at simulation
! !       !------------------------------------------------------------------------
! !       key   = "operations%dry_deposition%CO2_effect%CO2_level"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%CO2_LEVEL = 0.  !_and_RoundOff( v_str, places=2 )

! !       !------------------------------------------------------------------------
! !       ! Reference CO2 level
! !       !------------------------------------------------------------------------
! !       key   = "operations%dry_deposition%CO2_effect%reference_CO2_level"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%CO2_REF = 0.  !_and_RoundOff( v_str, places=2 )

! !       !------------------------------------------------------------------------
! !       ! Diag for RA_alt above surface in meters
! !       !------------------------------------------------------------------------
! !       key   = "operations%dry_deposition%diag_alt_above_sfc_in_m"
! !       v_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_int, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%RA_Alt_Above_Sfc = v_int

! !       !------------------------------------------------------------------------
! !       ! Turn on wetdep?
! !       !------------------------------------------------------------------------
! !       key    = "operations%wet_deposition%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LWETD = v_bool

! !       !========================================================================
! !       ! Error check settings
! !       !========================================================================

! !       ! Turn off drydep for simulations that don't need it
! !       IF ( Config_Opt%ITS_A_TAGCO_SIM   ) Config_Opt%LDRYD = .FALSE.

! !       ! Turn off wetdep for simulations that don't need it
! !       IF ( Config_Opt%ITS_A_CH4_SIM     ) Config_Opt%LWETD = .FALSE.
! !       IF ( Config_Opt%ITS_A_TAGCH4_SIM  ) Config_Opt%LWETD = .FALSE.
! !       IF ( Config_Opt%ITS_A_TAGCO_SIM   ) Config_Opt%LWETD = .FALSE.
! !       IF ( Config_Opt%ITS_A_TAGO3_SIM   ) Config_Opt%LWETD = .FALSE.

! !       ! If CO2 effect on RS in turned on, calculate the scaling factor
! !       ! on Rs based on Franks et al. (2013) (ayhwong, 6/25/2019)
! !       If (Config_Opt%CO2_EFFECT) THEN
! !          Config_Opt%RS_SCALE = Config_Opt%CO2_LEVEL / Config_Opt%CO2_REF * &
! !             (Config_Opt%CO2_LEVEL + 80.0_fp) *          &
! !             (Config_Opt%CO2_REF   - 40.0_fp) /          &
! !             (Config_Opt%CO2_LEVEL - 40.0_fp) /          &
! !             (Config_Opt%CO2_REF   + 80.0_fp)
! !       ENDIF

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, 90  ) 'DRY DEPOSITION SETTINGS'
! !          WRITE( 6, 95  ) '-----------------------'
! !          WRITE( 6, 100 ) 'Turn on dry deposition?     : ', Config_Opt%LDRYD
! !          WRITE( 6, 100 ) 'Dry dep over full PBL?      : ', Config_Opt%PBL_DRYDEP
! !          WRITE( 6, 100 ) 'Turn on CO2 effect?         : ', Config_Opt%CO2_EFFECT
! !          WRITE( 6, 110 ) 'CO2 level                   : ', Config_Opt%CO2_LEVEL
! !          WRITE( 6, 110 ) 'CO2 reference level         : ', Config_Opt%CO2_REF
! !          WRITE( 6, 110 ) 'RIX scaling factor          : ', Config_Opt%RS_SCALE


! !          WRITE( 6, 90  ) 'WET DEPOSITION SETTINGS'
! !          WRITE( 6, 95  ) '-----------------------'
! !          WRITE( 6, 100 ) 'Turn on wet deposition?     : ', Config_Opt%LWETD
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A    )
! ! 95    FORMAT( A       )
! ! 100   FORMAT( A, L5   )
! ! 110   FORMAT( A, f8.2 )

! !    END SUBROUTINE Config_DryDep_WetDep
! ! !!EOC
! ! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
! ! #ifdef BPCH_DIAG
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_gamap
! ! !
! ! ! !DESCRIPTION: Copies GAMAP information from the Config object
! ! !  to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_Gamap( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !
! ! ! !REVISION HISTORY:
! ! !  25 Apr 2005 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=512)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       !========================================================================
! !       ! Config_Gamap begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_Gamap (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! diaginfo.dat
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%legacy_bpch%gamap%diaginfo_dat_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%GAMAP_DIAGINFO = TRIM( ADJUSTL( v_str ) )

! !       !------------------------------------------------------------------------
! !       ! tracerinfo.dat
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%legacy_bpch%gamap%tracerinfo_dat_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%GAMAP_TRACERINFO = TRIM( ADJUSTL( v_str ) )

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, '(/,a)' ) 'GAMAP SETTINGS (when -DBPCH_DIAG=y)'
! !          WRITE( 6, '(  a)' ) '-----------------------------------'
! !          WRITE( 6, '(a,a)' ) 'GAMAP "diaginfo.dat"   file : ',                 &
! !             TRIM( Config_Opt%GAMAP_DIAGINFO   )
! !          WRITE( 6, '(a,a)' ) 'GAMAP "tracerinfo.dat" file : ',                 &
! !             TRIM( Config_Opt%GAMAP_TRACERINFO )
! !       ENDIF

! !    END SUBROUTINE Config_Gamap
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_bpch_output

! ! ! !DESCRIPTION: Copies bpch output information from the Config object
! ! !  to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_Bpch_Output( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod,  ONLY : OptConfig
! !       USE QFYAML_Mod,     ONLY : QFYAML_t
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !
! ! ! !REVISION HISTORY:
! ! !  20 Jul 2004 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       INTEGER                      :: N
! !       INTEGER                      :: v_int

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=512)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       ! String arrays
! !       CHARACTER(LEN=3)             :: mon(12)=  (/'JAN', 'FEB', 'MAR', 'APR',  &
! !          'MAY', 'JUN', 'JUL', 'AUG',  &
! !          'SEP', 'OCT', 'NOV', 'DEC'/)

! !       !========================================================================
! !       ! Config_Bpch_Output begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_Bpch_Output (in module CATChem/src/core/input_mod.F90)'

! !       !========================================================================
! !       ! Read data into the Config_Opt%NJDAY array
! !       !========================================================================
! !       DO N = 1, 12

! !          key = "extra_diagnostics%legacy_bpch%output_menu%" // &
! !             "schedule_output_for_"                       // mon(N)
! !          v_str = MISSING_STR
! !          CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !          IF ( RC /= CC_SUCCESS ) THEN
! !             errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !             CALL CC_Error( errMsg, RC, thisLoc )
! !             RETURN
! !          ENDIF

! !          ! Parse string into NJDAY array by month
! !          SELECT CASE( N )
! !           CASE( 1 )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(1:31)
! !           CASE( 2  )
! !             READ( v_str, '(29i1)' ) Config_Opt%NJDAY(32:60)
! !           CASE( 3 )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(61:91)
! !           CASE( 4  )
! !             READ( v_str, '(30i1)' ) Config_Opt%NJDAY(92:121)
! !           CASE( 5  )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(122:152)
! !           CASE( 6  )
! !             READ( v_str, '(30i1)' ) Config_Opt%NJDAY(153:182)
! !           CASE( 7  )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(183:213)
! !           CASE( 8  )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(214:244)
! !           CASE( 9  )
! !             READ( v_str, '(30i1)' ) Config_Opt%NJDAY(245:274)
! !           CASE( 10 )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(275:305)
! !           CASE( 11 )
! !             READ( v_str, '(30i1)' ) Config_Opt%NJDAY(306:335)
! !           CASE( 12 )
! !             READ( v_str, '(31i1)' ) Config_Opt%NJDAY(336:366)
! !          END SELECT
! !       ENDDO

! !       !=================================================================
! !       ! Print to screen
! !       !=================================================================
! !       IF( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, '(/,a)' ) 'BPCH OUTPUT SETTINGS'
! !          WRITE( 6, '(  a)' ) '--------------------'
! !          WRITE( 6, 110     )
! !          WRITE( 6, 120     )
! !          WRITE( 6, 130     )
! !          WRITE( 6, 140     ) Config_Opt%NJDAY
! !       ENDIF

! !       ! FORMAT statements
! ! 110   FORMAT( '              1111111111222222222233' )
! ! 120   FORMAT( '     1234567890123456789012345678901' )
! ! 130   FORMAT( '     -------------------------------' )
! ! 140   FORMAT( 'JAN--', 31i1, /, 'FEB--', 29i1, /, 'MAR--', 31i1, /, &
! !          'APR--', 30i1, /, 'MAY--', 31i1, /, 'JUN--', 30i1, /, &
! !          'JUL--', 31i1, /, 'AUG--', 31i1, /, 'SEP--', 30i1, /, &
! !          'OCT--', 31i1, /, 'NOV--', 30i1, /, 'DEC--', 31i1 )

! !       ! Make sure we have output at end of run
! !       CALL IS_LAST_DAY_GOOD( Config_Opt, RC )

! !       ! Trap potential errors
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !    END SUBROUTINE Config_Bpch_Output
! ! !EOC
! ! #endif
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_planeflight
! ! !
! ! ! !DESCRIPTION: Copies PlaneFlight diagnostic information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_PlaneFlight( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod,   ONLY : OptConfig
! !       USE PlaneFlight_Mod, ONLY : SET_PLANEFLIGHT
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options Object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL                      :: v_bool

! !       ! Strings
! !       CHARACTER(LEN=255)           :: key
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       !========================================================================
! !       ! Config_PlaneFlight begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_PlaneFlight (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Turn on planeflight diagnostic?
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%planeflight%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%Do_Planeflight = v_bool

! !       !------------------------------------------------------------------------
! !       ! Input file name (w/ flight track data points)
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%planeflight%flight_track_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%Planeflight_InFile = TRIM( v_str )


! !       !------------------------------------------------------------------------
! !       ! Output file name
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%planeflight%output_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )

! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%Planeflight_OutFile = TRIM( v_str )

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, 90  ) 'PLANEFLIGHT DIAGNOSTIC SETTINGS'
! !          WRITE( 6, 95  ) '-------------------------------'
! !          WRITE( 6, 100 ) 'Turn on planeflight diag?   : ',                     &
! !             Config_Opt%Do_Planeflight
! !          WRITE( 6, 110 ) 'Flight track input file     : ',                     &
! !             TRIM( Config_Opt%Planeflight_InFile )
! !          WRITE( 6, 110 ) 'Output file name            : ',                     &
! !             TRIM( Config_Opt%Planeflight_OutFile )
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A   )
! ! 95    FORMAT( A      )
! ! 100   FORMAT( A, L5  )
! ! 110   FORMAT( A, A   )

! !       !========================================================================
! !       ! Call setup routines from other F90 modules
! !       !========================================================================

! !       ! Pass variables to "planeflight_mod.F90"
! !       CALL Set_PlaneFlight( PF       = Config_Opt%Do_Planeflight,               &
! !          In_File  = Config_Opt%Planeflight_InFile,           &
! !          Out_File = Config_Opt%Planeflight_OutFile          )

! !    END SUBROUTINE Config_PlaneFlight
! ! #endif
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_obspack
! ! !
! ! ! !DESCRIPTION: Copies Obspack diagnostic information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_ObsPack( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       INTEGER                      :: N
! !       LOGICAL                      :: v_bool

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       ! String arrays
! !       CHARACTER(LEN=QFYAML_StrLen) :: a_str(QFYAML_MaxArr)

! !       !========================================================================
! !       ! Config_ObsPack begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = 'Error reading the "geoschem_config.yml" file!'
! !       thisLoc = ' -> at Config_ObsPack (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Turn on ObsPack diagnostic?
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%obspack%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%Do_ObsPack = v_bool

! !       !------------------------------------------------------------------------
! !       ! ObsPack quiet output?
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%obspack%quiet_logfile_output"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ObsPack_Quiet = v_bool

! !       !------------------------------------------------------------------------
! !       ! Input file name (w/ coordinates and sampling strategy)
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%obspack%input_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ObsPack_InputFile = TRIM( v_str )

! !       !------------------------------------------------------------------------
! !       ! Output file name
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%obspack%output_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ObsPack_OutputFile = TRIM( v_str )

! !       !------------------------------------------------------------------------
! !       ! Species names
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%obspack%output_species"
! !       a_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str,                         &
! !          "",     RC,          dynamic_size=.TRUE.           )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !       !------------------------------------------------------------------------
! !       ! Copy species names into Config_Opt
! !       !------------------------------------------------------------------------
! !       Config_Opt%ObsPack_nSpc = 0
! !       DO N = 1, SIZE( a_str )

! !          ! Stop iterationg when we find a missing value
! !          IF ( TRIM( a_str(N) ) == MISSING_STR ) EXIT

! !          ! If wildcard for all species is requested then update the
! !          ! list of species to track to be the list of advected species
! !          ! and exit from further
! !          IF ( N==1 .AND. INDEX( a_str(1) , '?ALL' ) >  0)  THEN
! !             Config_Opt%ObsPack_SpcName = Config_Opt%AdvectSpc_Name
! !             Config_Opt%ObsPack_nSpc    = Config_Opt%N_Advect
! !             EXIT
! !          ENDIF

! !          ! Otherwise, increment the count and copy the Obspack species
! !          ! name into the Config_Opt object
! !          Config_Opt%ObsPack_nSpc = Config_Opt%ObsPack_nSpc + 1
! !          Config_Opt%ObsPack_SpcName(Config_Opt%ObsPack_nSpc) = TRIM( a_str(N) )
! !       ENDDO

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, 90  ) 'OBSPACK SETTINGS'
! !          WRITE( 6, 95  ) '----------------'
! !          WRITE( 6, 100 ) 'Turn on ObsPack diagnostic? : ',                     &
! !             Config_Opt%Do_ObsPack
! !          WRITE( 6, 100 ) 'Suppress logfile output?    : ',                     &
! !             Config_Opt%ObsPack_Quiet
! !          WRITE( 6, 110 ) 'ObsPack input file          : ',                     &
! !             TRIM( Config_Opt%ObsPack_InputFile  )
! !          WRITE( 6, 110 ) 'ObsPack output file         : ',                     &
! !             TRIM( Config_Opt%ObsPack_OutputFile )
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A   )
! ! 95    FORMAT( A      )
! ! 100   FORMAT( A, L5  )
! ! 110   FORMAT( A, A   )

! !    END SUBROUTINE Config_ObsPack
! ! !EOC
! ! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
! ! #ifdef BPCH_DIAG
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_nd51
! ! !
! ! ! !DESCRIPTION: Copies ND51 satellite diagnostic information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_ND51( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL                      :: v_bool
! !       INTEGER                      :: N

! !       ! Arrays
! !       INTEGER                      :: a_int(QFYAML_MaxArr)

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       ! String arrays
! !       CHARACTER(LEN=QFYAML_StrLen) :: a_str(QFYAML_MaxArr)


! !       !========================================================================
! !       ! Config_ND51 begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_ND51 (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Turn on ND51 diagnostic
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%legacy_bpch%ND51_satellite%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%DO_ND51 = v_bool

! !       !------------------------------------------------------------------------
! !       ! Instantaneous 3-D timeseries file
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%legacy_bpch%ND51_satellite%output_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51_FILE = TRIM( v_str )

! !       !------------------------------------------------------------------------
! !       ! Tracers to include
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%legacy_bpch%ND51_satellite%tracers"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       DO N = 1, SIZE( a_int )
! !          IF ( a_int(N) == MISSING_INT ) EXIT
! !          Config_Opt%ND51_TRACERS(N) = a_int(N)
! !       ENDDO
! !       Config_Opt%N_ND51 = N - 1

! !       !------------------------------------------------------------------------
! !       ! NHMS_WRITE
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%legacy_bpch%ND51_satellite%UTC_hour_for_write"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51_HR_WRITE = 0.  !_and_RoundOff( v_str, places=2 )

! !       ! Make sure ND51_HR_WRITE is in the range 0-23.999 hrs
! !       Config_Opt%ND51_HR_WRITE = MOD( Config_Opt%ND51_HR_WRITE, 24.0_fp )

! !       !------------------------------------------------------------------------
! !       ! HR1, HR2
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51_satellite%averaging_period_in_LT"
! !       a_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, a_str, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51_HR1 = 0.  !_and_RoundOff( a_str(1), places=2 )
! !       Config_Opt%ND51_HR2 = 0.  !_and_RoundOff( a_str(2), places=2 )

! !       !------------------------------------------------------------------------
! !       ! IMIN, IMAX
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51_satellite%IMIN_and_IMAX_of_region"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51_IMIN = a_int(1)
! !       Config_Opt%ND51_IMAX = a_int(2)

! !       !------------------------------------------------------------------------
! !       ! JMIN, JMAX
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51_satellite%JMIN_and_JMAX_of_region"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51_JMIN = a_int(1)
! !       Config_Opt%ND51_JMAX = a_int(2)

! !       !------------------------------------------------------------------------
! !       ! LMIN, LMAX
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51_satellite%LMIN_and_LMAX_of_region"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51_LMIN = a_int(1)
! !       Config_Opt%ND51_LMAX = a_int(2)

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6,90  ) 'ND51 TIMESERIES SETTINGS (when -DBPCH_DIAG=y)'
! !          WRITE( 6,95  ) '---------------------------------------------'
! !          WRITE( 6,100 ) 'Turn on ND51 timeseries?    : ', Config_Opt%DO_ND51
! !          WRITE( 6,110 ) 'ND51 timeseries file name   : ',                      &
! !             TRIM( Config_Opt%ND51_FILE )
! !          WRITE( 6,120 ) 'ND51 timeseries tracers     : ',                      &
! !             ( Config_Opt%ND51_TRACERS(N), N=1, Config_Opt%N_ND51 )
! !          WRITE( 6,140 ) 'ND51 hour to write to disk  : ', Config_Opt%ND51_HR_WRITE
! !          WRITE( 6,140 ) 'ND51 averaging period [GMT] : ', Config_Opt%ND51_HR1,  &
! !             Config_Opt%ND51_HR2
! !          WRITE( 6,130 ) 'ND51 longitude limits       : ', Config_Opt%ND51_IMIN, &
! !             Config_Opt%ND51_IMAX
! !          WRITE( 6,130 ) 'ND51 latitude  limits       : ', Config_Opt%ND51_JMIN, &
! !             Config_Opt%ND51_JMAX
! !          WRITE( 6,130 ) 'ND51 altitude  limits       : ', Config_Opt%ND51_LMIN, &
! !             Config_Opt%ND51_LMAX
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A     )
! ! 95    FORMAT( A        )
! ! 100   FORMAT( A, L5    )
! ! 110   FORMAT( A, A     )
! ! 120   FORMAT( A, 100I4 )
! ! 130   FORMAT( A, 2I5   )
! ! 140   FORMAT( A, 2F5.1 )

! !    END SUBROUTINE Config_Nd51
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_nd51b
! ! !
! ! ! !DESCRIPTION: Copies ND51b satellite diagnostic information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_ND51b( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL                      :: v_bool
! !       INTEGER                      :: N

! !       ! Arrays
! !       INTEGER                      :: a_int(QFYAML_MaxArr)

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       ! String arrays
! !       CHARACTER(LEN=QFYAML_StrLen) :: a_str(QFYAML_MaxArr)

! !       !========================================================================
! !       ! Config_ND51b begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_ND51b (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Turn on ND51b diagnostic
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%legacy_bpch%ND51b_satellite%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%DO_ND51b = v_bool

! !       !------------------------------------------------------------------------
! !       ! Instantaneous 3-D timeseries file
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%legacy_bpch%ND51b_satellite%output_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51b_FILE = TRIM( v_str )

! !       !------------------------------------------------------------------------
! !       ! Tracers to include
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%legacy_bpch%ND51b_satellite%tracers"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       DO N = 1, SIZE( a_int )
! !          IF ( a_int(N) == MISSING_INT ) EXIT
! !          Config_Opt%ND51b_TRACERS(N) = a_int(N)
! !       ENDDO
! !       Config_Opt%N_ND51b = N - 1

! !       !------------------------------------------------------------------------
! !       ! NHMS_WRITE
! !       !------------------------------------------------------------------------
! !       key   = "extra_diagnostics%legacy_bpch%ND51b_satellite%UTC_hour_for_write"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51b_HR_WRITE = 0.  !_and_RoundOff( v_str, places=2 )

! !       ! Make sure ND51b_HR_WRITE is in the range 0-23.999 hrs
! !       Config_Opt%ND51b_HR_WRITE = MOD( Config_Opt%ND51b_HR_WRITE, 24.0_fp )

! !       !------------------------------------------------------------------------
! !       ! HR1, HR2
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51b_satellite%averaging_period_in_LT"
! !       a_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, a_str, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51b_HR1 = 0.  !_and_RoundOff( a_str(1), places=2 )
! !       Config_Opt%ND51b_HR2 = 0.  !_and_RoundOff( a_str(2), places=2 )

! !       !------------------------------------------------------------------------
! !       ! IMIN, IMAX
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51b_satellite%IMIN_and_IMAX_of_region"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51b_IMIN = a_int(1)
! !       Config_Opt%ND51b_IMAX = a_int(2)

! !       !------------------------------------------------------------------------
! !       ! JMIN, JMAX
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51b_satellite%JMIN_and_JMAX_of_region"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51b_JMIN = a_int(1)
! !       Config_Opt%ND51b_JMAX = a_int(2)

! !       !------------------------------------------------------------------------
! !       ! LMIN, LMAX
! !       !------------------------------------------------------------------------
! !       key = "extra_diagnostics%legacy_bpch%ND51b_satellite%LMIN_and_LMAX_of_region"
! !       a_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, a_int, "", RC, dynamic_size=.TRUE. )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND51b_LMIN = a_int(1)
! !       Config_Opt%ND51b_LMAX = a_int(2)

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6,90  ) 'ND51b TIMESERIES SETTINGS (when -DBPCH_DIAG=y)'
! !          WRITE( 6,95  ) '----------------------------------------------'
! !          WRITE( 6,100 ) 'Turn on ND51b timeseries?   : ', Config_Opt%DO_ND51b
! !          WRITE( 6,110 ) 'ND51b timeseries file name  : ',                      &
! !             TRIM( Config_Opt%ND51b_FILE )
! !          WRITE( 6,120 ) 'ND51b timeseries tracers    : ',                      &
! !             ( Config_Opt%ND51b_TRACERS(N), N=1, Config_Opt%N_ND51b )
! !          WRITE( 6,140 ) 'ND51b hour to write to disk : ', Config_Opt%ND51b_HR_WRITE
! !          WRITE( 6,140 ) 'ND51b averaging period [GMT]: ', Config_Opt%ND51b_HR1, &
! !             Config_Opt%ND51b_HR2
! !          WRITE( 6,130 ) 'ND51b longitude limits      : ', Config_Opt%ND51b_IMIN,&
! !             Config_Opt%ND51b_IMAX
! !          WRITE( 6,130 ) 'ND51b latitude  limits      : ', Config_Opt%ND51b_JMIN,&
! !             Config_Opt%ND51b_JMAX
! !          WRITE( 6,130 ) 'ND51b altitude  limits      : ', Config_Opt%ND51b_LMIN,&
! !             Config_Opt%ND51b_LMAX
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A     )
! ! 95    FORMAT( A        )
! ! 100   FORMAT( A, L5    )
! ! 110   FORMAT( A, A     )
! ! 120   FORMAT( A, 100I4 )
! ! 130   FORMAT( A, 2I5   )
! ! 140   FORMAT( A, 2F5.1 )

! !    END SUBROUTINE Config_Nd51b
! ! #ifdef TOMAS
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_bpch_prodloss

! ! ! !DESCRIPTION: Copies bpch prodloss information from the Config object
! ! !  to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_Bpch_ProdLoss( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE CMN_DIAG_MOD,     ONLY : ND65
! !       USE gckpp_Monitor,    ONLY : Fam_Names
! !       USE gckpp_Parameters, ONLY : nFam
! !       USE Config_Opt_Mod,    ONLY : OptConfig
! !       USE QFYAML_Mod,       ONLY : QFYAML_t
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !
! ! ! !REVISION HISTORY:
! ! !  20 Jul 2004 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL                      :: v_bool
! !       INTEGER                      :: v_int, F, N, n_Advect

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=512)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       ! Arrays
! ! !    CHARACTER(LEN=255) :: SUBSTRS(MAXDIM)

! !       !=========================================================================
! !       ! Config_Bpch_ProdLoss begins here!
! !       !=========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = 'Error reading the "geoschem_config.yml" file!'
! !       thisLoc = ' -> at Config_Bpch_ProdLoss (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Activate bpch ND65 diagnostic?
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%legacy_bpch%bpch_diagnostics%"            // &
! !          "ND65_prodloss%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%DO_SAVE_PL = v_bool

! !       !------------------------------------------------------------------------
! !       ! Number of levels for ND65 bpch diagnostic
! !       !------------------------------------------------------------------------
! !       key    = "extra_diagnostics%legacy_bpch%bpch_diagnostics%"            // &
! !          "ND65_prodloss%number_of_levels"
! !       v_int = MISSING_INT
! !       CALL QFYAML_Add_Get( Config, key, v_int, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%ND65 = v_int

! !       ! Copy field to variable in CMN_DIAG
! !       ND65 = Config_Opt%ND65

! !       !=================================================================
! !       ! Error check families for certain types of simulations
! !       !=================================================================

! !       ! Offline aerosol -- turn off DO_SAVE_PL
! !       IF ( Config_Opt%ITS_AN_AEROSOL_SIM ) THEN
! !          Config_Opt%DO_SAVE_PL    = .FALSE.
! !          Config_Opt%ND65          = 0
! !       ENDIF

! !       !=================================================================
! !       ! Set fields of Input Options object
! !       !=================================================================

! !       ! Number of advected species
! !       N_ADVECT = Config_Opt%N_ADVECT

! !       IF ( Config_Opt%DO_SAVE_PL ) THEN
! !          IF ( Config_Opt%ITS_A_FULLCHEM_SIM ) THEN
! !             ! Fullchem - Obtain NFAM from KPP
! !             Config_Opt%NFAM = NFAM
! !          ELSEIF ( Config_Opt%ITS_A_TAGO3_SIM ) THEN
! !             ! Tagged O3
! !             Config_Opt%NFAM = 2*N_ADVECT
! !          ELSEIF ( Config_Opt%ITS_A_TAGCO_SIM ) THEN
! !             ! Tagged CO
! !             IF ( Config_Opt%LPCO_NMVOC ) THEN
! !                Config_Opt%NFAM = N_ADVECT+2
! !             ELSE
! !                Config_Opt%NFAM = N_ADVECT+6
! !             ENDIF
! !          ENDIF
! !       ENDIF

! !       ! Return if there are no prod/loss families
! !       ! or if we have turned off this diagnostic
! !       IF ( .not. ( Config_Opt%DO_SAVE_PL .and. Config_Opt%NFAM > 0 )) THEN
! !          Config_Opt%DO_SAVE_PL = .FALSE.
! !          Config_Opt%ND65       = 0
! !       ENDIF

! !       ! Loop over families
! !       DO F = 1, Config_Opt%NFAM

! !          IF ( Config_Opt%ITS_A_FULLCHEM_SIM ) THEN

! !             ! Fullchem - Obtain FAM_NAME from KPP
! !             Config_Opt%FAM_NAME(F) = FAM_NAMES(F)

! !          ELSEIF ( Config_Opt%ITS_A_TAGO3_SIM ) THEN

! !             ! Tagged O3
! !             IF ( F <= N_ADVECT ) THEN
! !                Config_Opt%FAM_NAME(F) = &
! !                   'P' // TRIM(Config_Opt%AdvectSpc_Name(F))
! !             ELSE
! !                Config_Opt%FAM_NAME(F) = &
! !                   'L' // TRIM(Config_Opt%AdvectSpc_Name(F-N_ADVECT))
! !             ENDIF

! !          ELSEIF ( Config_Opt%ITS_A_TAGCO_SIM ) THEN

! !             ! Tagged CO
! !             IF ( F <= N_ADVECT ) THEN
! !                Config_Opt%FAM_NAME(F) = 'L'//Config_Opt%AdvectSpc_Name(F)
! !             ELSEIF ( F == N_ADVECT+1 ) THEN
! !                Config_Opt%FAM_NAME(F) = 'PCO_CH4'
! !             ELSEIF ( F == N_ADVECT+2 ) THEN
! !                Config_Opt%FAM_NAME(F) = 'PCO_NMVOC'
! !             ELSEIF ( F == N_ADVECT+3 ) THEN
! !                Config_Opt%FAM_NAME(F) = 'PCO_ISOP'
! !             ELSEIF ( F == N_ADVECT+4 ) THEN
! !                Config_Opt%FAM_NAME(F) = 'PCO_CH3OH'
! !             ELSEIF ( F == N_ADVECT+5 ) THEN
! !                Config_Opt%FAM_NAME(F) = 'PCO_MONO'
! !             ELSEIF ( F == N_ADVECT+6 ) THEN
! !                Config_Opt%FAM_NAME(F) = 'PCO_ACET'
! !             ENDIF

! !          ENDIF

! !          ! Get family type as prod or loss
! !          IF ( Config_Opt%FAM_NAME(F)(1:1) == 'P'   .or. &
! !             Config_Opt%FAM_NAME(F)(1:1) == 'p' ) THEN
! !             Config_Opt%FAM_TYPE(F) = 'prod'
! !          ELSE
! !             Config_Opt%FAM_TYPE(F) = 'loss'
! !          ENDIF

! !       ENDDO

! !       !=================================================================
! !       ! Print to screen
! !       !=================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, '(/,a)' ) 'PROD & LOSS DIAGNOSTIC SETTINGS'
! !          WRITE( 6, '(  a)' ) '-------------------------------'
! !          WRITE( 6, 100 ) 'Turn on prod & loss diag?   : ', &
! !             Config_Opt%DO_SAVE_PL
! !          WRITE( 6, 110 ) '# of levels for P/L diag    : ', &
! !             Config_Opt%ND65

! !          ! Loop over families
! !          DO F = 1, Config_Opt%NFAM

! !             ! Write family name and type
! !             WRITE( 6, 120 ) TRIM(Config_Opt%FAM_NAME(F)), &
! !                TRIM(Config_Opt%FAM_TYPE(F))

! !          ENDDO

! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A                             )
! ! 95    FORMAT( A                                )
! ! 100   FORMAT( A, L5                            )
! ! 110   FORMAT( A, I5                            )
! ! 120   FORMAT( /, 'Family=', A10, '  Type=', A4 )

! !    END SUBROUTINE Config_Bpch_ProdLoss
! ! !EOC
! ! #endif
! ! #endif
! ! #endif
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_Hg
! ! !
! ! ! !DESCRIPTION: Copies Hg simulation information from the Config object
! ! !  to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_Hg( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       INTEGER                      :: N
! !       LOGICAL                      :: v_bool

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       !=================================================================
! !       ! Config_Hg begins here!
! !       !=================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_Hg (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Use dynamic ocean Hg?
! !       !------------------------------------------------------------------------
! !       key    = "Hg_simulation_options%sources%use_dynamic_ocean_Hg"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LDYNOCEAN = v_bool

! !       !------------------------------------------------------------------------
! !       ! Use preindustrial Hg?
! !       !------------------------------------------------------------------------
! !       key    = "Hg_simulation_options%sources%use_preindustrial_Hg"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LPREINDHG = v_bool

! !       !------------------------------------------------------------------------
! !       ! Use arctic river Hg?
! !       !------------------------------------------------------------------------
! !       key    = "Hg_simulation_options%sources%use_arctic_river_Hg"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LARCTICRIV = v_bool

! !       !------------------------------------------------------------------------
! !       ! Tie Hg2(aq) reduction to UV-B radiation?
! !       !------------------------------------------------------------------------
! !       key    = "Hg_simulation_options%chemistry%tie_HgIIaq_reduction_to_UVB"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LKRedUV = v_bool

! !       !------------------------------------------------------------------------
! !       ! Use GTMM soil model
! !       !
! !       ! NOTE: As of April 2022, GTMM is broken.  We look to the community
! !       ! to take the lead in restoring it.  Until that happens, these options
! !       ! will have no effect. -- Bob Yantosca (04 Apr 2022)
! !       !------------------------------------------------------------------------
! !       key    = "Hg_simulation_options%GTMM_soil_model%activate"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%LGTMM = v_bool

! !       !------------------------------------------------------------------------
! !       ! GTMM restart file name
! !       !
! !       ! NOTE: As of April 2022, GTMM is broken.  We look to the community
! !       ! to take the lead in restoring it.  Until that happens, these options
! !       ! will have no effect. -- Bob Yantosca (04 Apr 2022)
! !       !------------------------------------------------------------------------
! !       key   = "Hg_simulation_options%GTMM_soil_model%restart_file"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%GTMM_RST_FILE = TRIM( v_str )

! !       !------------------------------------------------------------------------
! !       ! Sanity checks
! !       !------------------------------------------------------------------------
! !       IF ( .not. Config_Opt%ITS_A_MERCURY_SIM ) THEN
! !          Config_Opt%LGTMM      = .FALSE.
! !          Config_Opt%LDYNOCEAN  = .FALSE.
! !          Config_Opt%LARCTICRIV = .FALSE.
! !          Config_Opt%LKRedUV    = .FALSE.
! !       ENDIF

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, 90  ) 'MERCURY SIMULATION SETTINGS'
! !          WRITE( 6, 95  ) '---------------------------'
! !          WRITE( 6, 110 ) 'Use dynamic ocean Hg model? : ', Config_Opt%LDYNOCEAN
! !          WRITE( 6, 110 ) 'Preindustrial simulation?   : ', Config_Opt%LPREINDHG
! !          WRITE( 6, 110 ) 'Use Arctic river Hg ?       : ', Config_Opt%LARCTICRIV
! !          WRITE( 6, 110 ) 'Tie HgII(aq) red. to UV-B?  : ', Config_Opt%LKRedUV
! !          WRITE( 6, 110 ) 'Use GTMM ?                  : ', Config_Opt%LGTMM
! !          WRITE( 6, 120 ) '=> GTMM restart file        : ',                      &
! !             TRIM( Config_Opt%GTMM_RST_FILE )
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A  )
! ! 95    FORMAT( A     )
! ! 100   FORMAT( A, I4 )
! ! 110   FORMAT( A, L5 )
! ! 120   FORMAT( A, A  )

! !    END SUBROUTINE Config_Hg
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_ch4
! ! !
! ! ! !DESCRIPTION: Copies CH4 simulation information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_CH4( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:

! !       ! Scalars
! !       INTEGER                      :: N
! !       INTEGER                      :: v_int
! !       LOGICAL                      :: v_bool

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       ! String arrays
! !       CHARACTER(LEN=QFYAML_NamLen) :: a_str(4)

! !       !========================================================================
! !       ! Config_CH4 begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = 'Error reading the "geoschem_config.yml" file!'
! !       thisLoc = ' -> at Config_CH4 (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! Use AIRS observational operator?
! !       !------------------------------------------------------------------------
! !       key    = "CH4_simulation_options%use_observational_operators%AIRS"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%AIRS_CH4_OBS = v_bool

! !       !------------------------------------------------------------------------
! !       ! Use GOSAT observational operator?
! !       !------------------------------------------------------------------------
! !       key    = "CH4_simulation_options%use_observational_operators%GOSAT"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%GOSAT_CH4_OBS =  v_bool

! !       !------------------------------------------------------------------------
! !       ! Use TCCON observational operator?
! !       !------------------------------------------------------------------------
! !       key    = "CH4_simulation_options%use_observational_operators%TCCON"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%TCCON_CH4_OBS = v_bool

! !       !------------------------------------------------------------------------
! !       ! Perturb CH4 boundary conditions?
! !       !------------------------------------------------------------------------
! !       key    = "CH4_simulation_options%analytical_inversion%perturb_CH4_boundary_conditions"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM ( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%DoPerturbCH4BoundaryConditions = v_bool

! !       !------------------------------------------------------------------------
! !       ! How much to perturb CH4 boundary conditions by?
! !       !------------------------------------------------------------------------
! !       key    = "CH4_simulation_options%analytical_inversion%CH4_boundary_condition_ppb_increase_NSEW"
! !       a_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, TRIM( key ), a_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM ( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%CH4BoundaryConditionIncreaseNorth = 0.  !_and_RoundOff( a_str(1), places=4 )
! !       Config_Opt%CH4BoundaryConditionIncreaseSouth = 0.  !_and_RoundOff( a_str(2), places=4 )
! !       Config_Opt%CH4BoundaryConditionIncreaseEast  = 0.  !_and_RoundOff( a_str(3), places=4 )
! !       Config_Opt%CH4BoundaryConditionIncreaseWest  = 0.  !_and_RoundOff( a_str(4), places=4 )

! !       !========================================================================
! !       ! Print to screen
! !       !========================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE(6,90 ) 'CH4 SIMULATION SETTINGS'
! !          WRITE(6,95 ) '-----------------------'
! !          WRITE(6,100) 'Use AIRS obs operator?   : ', Config_Opt%AIRS_CH4_OBS
! !          WRITE(6,100) 'Use GOSAT obs operator?  : ', Config_Opt%GOSAT_CH4_OBS
! !          WRITE(6,100) 'Use TCCON obs operator?  : ', Config_Opt%TCCON_CH4_OBS
! !          WRITE(6,100) 'Perturb CH4 BCs?         : ', Config_Opt%DoPerturbCH4BoundaryConditions
! !          WRITE(6,130) 'CH4 BC ppb increase NSEW : ', Config_Opt%CH4BoundaryConditionIncreaseNorth,&
! !             Config_Opt%CH4BoundaryConditionIncreaseSouth,&
! !             Config_Opt%CH4BoundaryConditionIncreaseEast,&
! !             Config_Opt%CH4BoundaryConditionIncreaseWest
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A    )
! ! 95    FORMAT( A       )
! ! 100   FORMAT( A, L5   )
! ! 130   FORMAT( A, F10.4, 1X, F10.4, 1X, F10.4, 1X, F10.4)

! !    END SUBROUTINE Config_CH4
! ! !EOC
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: config_pops
! ! !
! ! ! !DESCRIPTION: Copies POPs simulation information from the Config
! ! !  object to Config_Opt, and does necessary checks.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Config_POPs( Config, Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE RoundOff_Mod,  ONLY : 0.  !_and_RoundOff
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(QFYAML_t), INTENT(INOUT) :: Config      ! YAML Config object
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input options
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL                      :: v_bool

! !       ! Strings
! !       CHARACTER(LEN=255)           :: thisLoc
! !       CHARACTER(LEN=255)           :: errMsg
! !       CHARACTER(LEN=QFYAML_NamLen) :: key
! !       CHARACTER(LEN=QFYAML_StrLen) :: v_str

! !       !========================================================================
! !       ! Config_POPs begins here!
! !       !========================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS

! !       ! Continue initializing
! !       errMsg  = ''
! !       thisLoc = ' -> at Config_POPs (in module CATChem/src/core/input_mod.F90)'

! !       !------------------------------------------------------------------------
! !       ! POP species
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_type"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_TYPE = TRIM( v_str )

! !       !------------------------------------------------------------------------
! !       ! Dummy for future process logical switches
! !       !------------------------------------------------------------------------
! !       key    = "POPs_simulation_options%chemistry_processing"
! !       v_bool = MISSING_BOOL
! !       CALL QFYAML_Add_Get( Config, key, v_bool, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%CHEM_PROCESS = v_bool

! !       !------------------------------------------------------------------------
! !       ! Molecular weight
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_XMW"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_XMW = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! KOA
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_KOA"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_KOA = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! KBC
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_KBC"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_KBC = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! OH oxidation
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_K_POPG_OH"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_K_POPG_OH = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! O3 oxidation 1
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_K_POPP_O3A"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_K_POPP_O3A = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! O3 oxidation 2
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_K_POPP_O3B"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_K_POPP_O3B = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! H*
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_HSTAR"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_HSTAR = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! DEL_H
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_DEL_H"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_DEL_H = 0.  !_and_RoundOff( v_str, places=0 )

! !       !------------------------------------------------------------------------
! !       ! DEL_Hw
! !       !------------------------------------------------------------------------
! !       key   = "POPs_simulation_options%POP_DEL_Hw"
! !       v_str = MISSING_STR
! !       CALL QFYAML_Add_Get( Config, key, v_str, "", RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          errMsg = 'Error parsing ' // TRIM( key ) // '!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! !       Config_Opt%POP_DEL_Hw = 0.  !_and_RoundOff( v_str, places=0 )

! !       !=================================================================
! !       ! Print to screen
! !       !=================================================================
! !       IF ( Config_Opt%amIRoot ) THEN
! !          WRITE( 6, 90  ) 'POPs SIMULATION SETTINGS'
! !          WRITE( 6, 95  ) '------------------------'
! !          WRITE( 6, 120 ) 'Species of POP        : ', Config_Opt%POP_TYPE
! !          WRITE( 6, 110 ) 'Chemistry on?         : ', Config_Opt%CHEM_PROCESS
! !          WRITE( 6, 130 ) 'POP_XMW               : ', Config_Opt%POP_XMW
! !          WRITE( 6, 130 ) 'POP_KOA               : ', Config_Opt%POP_KOA
! !          WRITE( 6, 130 ) 'POP_KBC               : ', Config_Opt%POP_KBC
! !          WRITE( 6, 130 ) 'POP_K_POPG_OH         : ', Config_Opt%POP_K_POPG_OH
! !          WRITE( 6, 130 ) 'POP_K_POPP_O3A        : ', Config_Opt%POP_K_POPP_O3A
! !          WRITE( 6, 130 ) 'POP_K_POPP_O3B        : ', Config_Opt%POP_K_POPP_O3B
! !          WRITE( 6, 130 ) 'POP_HSTAR             : ', Config_Opt%POP_HSTAR
! !          WRITE( 6, 130 ) 'POP_DEL_H             : ', Config_Opt%POP_DEL_H
! !          WRITE( 6, 130 ) 'POP_DEL_Hw            : ', Config_Opt%POP_DEL_Hw
! !       ENDIF

! !       ! FORMAT statements
! ! 90    FORMAT( /, A      )
! ! 95    FORMAT( A         )
! ! 110   FORMAT( A, L5     )
! ! 120   FORMAT( A, A      )
! ! 130   FORMAT( A, ES10.2 )

! !       ! Return success
! !       RC = CC_SUCCESS

! !    END SUBROUTINE Config_POPs
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: validate_directories
! !
! ! !DESCRIPTION: Makes sure that each of the directories that we have read from
! !  the CATChem input file are valid. Also, trailing separator characters will
! !  be added.
! !\\
! !\\
! ! !INTERFACE:
! !
! !    SUBROUTINE VALIDATE_DIRECTORIES( Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE Time_Mod,      ONLY : Expand_Date
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
! ! !
! ! ! !REVISION HISTORY:
! ! !  20 Jul 2004 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Strings
! !       CHARACTER(LEN=255) :: errMsg, thisLoc, Dir

! !       !=================================================================
! !       ! Validate_Directories begins here!
! !       !=================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = 'Invalid directory encountered!'
! !       thisLoc = ' -> at Validate_Directories (in module CATChem/src/core/input_mod.F90)'

! !       ! Skip for dry-runs
! !       IF ( Config_Opt%DryRun ) RETURN

! ! #if !defined( MODEL_CESM )
! !       ! Check directories
! !       CALL Check_Directory( Config_Opt, Config_Opt%DATA_DIR, RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! ! #endif

! !       CALL Check_Directory( Config_Opt, Config_Opt%CHEM_INPUTS_DIR, RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! ! #if !defined( MODEL_CESM )
! !       CALL Check_Directory( Config_Opt, Config_Opt%RUN_DIR, RC )
! !       IF ( RC /= CC_SUCCESS ) THEN
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF
! ! #endif

! !    END SUBROUTINE Validate_Directories
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: check_directory
! !
! ! !DESCRIPTION: Makes sure that the given directory is valid.  Also a trailing
! !  slash character will be added if necessary.
! !\\
! !\\
! ! !INTERFACE:
! !
! !    SUBROUTINE Check_Directory( Config_Opt, dir, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE FILE_MOD,      ONLY : File_Exists
! !       USE Config_Opt_Mod, ONLY : OptConfig
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(OptConfig),   INTENT(INOUT) :: Config_Opt   ! Input Options object
! !       CHARACTER(LEN=*), INTENT(INOUT) :: dir         ! Dir to be checked
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,          INTENT(OUT)   :: RC          ! Success or failure
! ! !
! ! ! !REVISION HISTORY:
! ! !  20 Mar 2003 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       INTEGER            :: C

! !       ! Strings
! !       CHARACTER(LEN=255) :: errMsg, thisLoc

! !       !=================================================================
! !       ! Check_Directory begins here!
! !       !=================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Check_Directory (in module CATChem/src/core/input_mod.F90)'

! !       ! Locate the last non-white-space character of NEWDIR
! !       C = LEN_TRIM( dir )

! !       ! Add the trailing directory separator if it is not present
! !       IF ( dir(C:C) /= '/' ) THEN
! !          dir(C+1:C+1) = TRIM( '/' )
! !       ENDIF

! !       !=================================================================
! !       ! Test if the directory actually exists
! !       !=================================================================

! !       ! If the directory does not exist then stop w/ an error message
! !       IF ( .not. File_Exists( dir ) ) THEN
! !          errMsg = 'Invalid directory: ' // TRIM( dir )
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !    END SUBROUTINE Check_Directory
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: check_time_steps
! !
! ! !DESCRIPTION: Computes the smallest dynamic time step for the model, based on
! !  which operation are turned on.  This is called from routine
! !  Read_Input_File, after all of the timesteps and logical flags have been
! !  read from the configuration file.
! !\\
! !\\
! ! !INTERFACE:
! !
! !    SUBROUTINE Check_Time_Steps( Config_Opt, State_Grid, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod,  ONLY : OptConfig
! !       USE State_Grid_Mod, ONLY : GrdState
! !       USE Time_Mod,       ONLY : Set_TimeSteps
! ! !
! ! ! !INPUT PARAMETERS:
! ! !
! !       TYPE(OptConfig), INTENT(IN)  :: Config_Opt   ! Input Options object
! !       TYPE(GrdState), INTENT(IN)  :: State_Grid  ! Grid State object
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT) :: RC          ! Success or failure?
! ! !
! ! ! !REVISION HISTORY:
! ! !  20 Jul 2004 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       LOGICAL            :: LCONV, LCHEM,       LDRYD
! !       LOGICAL            :: LTRAN, LTURB
! !       INTEGER            :: I,     J,           K
! !       INTEGER            :: L,     TS_SMALLEST, TS_DIAG
! !       INTEGER            :: TS_CHEM, TS_EMIS, TS_CONV, TS_DYN
! !       INTEGER            :: TS_UNIT, TS_RAD,  MAX_DYN

! !       ! Strings
! !       CHARACTER(LEN=255) :: errMsg, thisLoc

! !       !=================================================================
! !       ! Check_Time_Steps begins here!
! !       !=================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Check_Time_Steps (in module CATChem/src/core/input_mod.F90)'

! !       ! Copy fields from Config_Opt
! !       LCONV = Config_Opt%LCONV
! !       LCHEM = Config_Opt%LCHEM
! !       LDRYD = Config_Opt%LDRYD
! !       LTRAN = Config_Opt%LTRAN
! !       LTURB = Config_Opt%LTURB

! !       TS_CHEM = Config_Opt%TS_CHEM
! !       TS_EMIS = Config_Opt%TS_EMIS
! !       TS_CONV = Config_Opt%TS_CONV
! !       TS_DYN  = Config_Opt%TS_DYN
! !       TS_RAD  = Config_Opt%TS_RAD

! !       ! If we're doing the reverse integration
! !       ! multiply all the timesteps by -1 here
! !       if (TS_DYN < 0) THEN
! !          TS_CHEM = TS_CHEM * -1
! !          TS_EMIS = TS_EMIS * -1
! !          TS_CONV = TS_CONV * -1
! !          TS_DYN  = TS_DYN  * -1
! !          TS_RAD  = TS_RAD  * -1
! !       endif


! !       ! NUNIT is time step in minutes for unit conversion
! !       TS_UNIT = -1

! !       ! Define maximum timestep for transport
! !       IF ( TRIM(State_Grid%GridRes) == '4.0x5.0') THEN
! !          MAX_DYN = 1800
! !       ELSE IF ( TRIM(State_Grid%GridRes) == '2.0x2.5' ) THEN
! !          MAX_DYN = 900
! !       ELSE IF ( TRIM(State_Grid%GridRes) == '0.5x0.625' ) THEN
! !          MAX_DYN = 600
! !       ELSE IF ( TRIM(State_Grid%GridRes) == '0.25x0.3125' ) THEN
! !          MAX_DYN = 300
! !       ELSE
! !          MAX_DYN = 3600
! !       ENDIF

! !       ! If TS_DYN is greater than MAX_DYN, then stop w/ error
! !       IF ( .not. Config_Opt%isMPI ) THEN
! !          IF ( Config_Opt%TS_DYN > MAX_DYN .and. LTRAN ) THEN
! !             WRITE( errMsg, 300 ) 'Transport timestep exceeds max:', &
! !                Config_Opt%TS_DYN, MAX_DYN
! ! 300         FORMAT( a, i8, ' >', i8 )
! !             CALL CC_Error( errMsg, RC, thisLoc )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       ! Only do unit conversion if necessary
! !       IF ( LTRAN .or. LCONV .or. LTURB ) THEN
! !          TS_UNIT = MAX( TS_DYN, TS_CONV )
! !       ENDIF

! !       ! Compute NSMALLEST as the minimum of NDYN, NCONV, NSRCE, NCHEM
! !       I = TS_DYN
! !       J = TS_CONV
! !       K = TS_EMIS
! !       L = TS_CHEM

! !       ! SDE 2017-02-24: Always use LTRAN on the assumption that it will
! !       ! be used as a "heartbeat". This ensures that chemistry always
! !       ! takes place at the same time, regardless of whether or not
! !       ! transport is enabled.
! !       !IF ( .not. LTRAN                  ) I = 999999
! !       IF ( .not. LCONV .and..not. LTURB ) J = 999999
! !       IF ( .not. LDRYD                  ) K = 999999
! !       IF ( .not. LCHEM                  ) L = 999999

! !       ! Get the smallest of all of the above
! !       TS_SMALLEST = MIN( I, J, K, L )

! !       ! If all of the operators above are turned off,
! !       ! then set TS_SMALLEST to TS_DYN.
! !       IF ( TS_SMALLEST == 999999 ) THEN
! !          TS_SMALLEST = TS_DYN
! !       ENDIF

! !       IF ( LTRAN .and. TS_DYN /= TS_SMALLEST ) THEN
! !          errMsg = 'The transport time step should be the smallest one'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !       ! If TS_DYN is smaller than TS_SMALLEST, reset TS_DYN
! !       ! to TS_SMALLEST.
! !       ! This is useful for runs where transport is turned off,
! !       ! but where chemistry is turned on.
! !       IF ( TS_DYN < TS_SMALLEST ) THEN
! !          TS_DYN = TS_SMALLEST
! !       ENDIF

! !       ! Define the largest time step, TS_DIAG, for diagnostics.
! !       ! Diagnostics should be incremented at the end of multiples of
! !       ! TS_DIAG, so that the system is at a physical state.
! !       ! (ccc, 5/13/09)
! !       IF ( .not. LTRAN                  ) I = -999999
! !       IF ( .not. LCONV .and..not. LTURB ) J = -999999
! !       IF ( .not. LDRYD                  ) K = -999999
! !       IF ( .not. LCHEM                  ) L = -999999

! !       TS_DIAG = MAX( I, J, K, L )

! !       ! If all the operators are turned off, then set TS_DIAG to TS_CHEM
! !       ! Usually the chemistry time step is large. (ccc, 5/13/09)
! !       IF ( TS_DIAG == -999999 ) THEN
! !          TS_DIAG = TS_CHEM
! !       ENDIF

! !       ! Check if all time steps are multiples of the smallest.
! !       ! (ccc, 5/13/09)
! !       IF ( L /= -999999 .and. MOD( TS_CHEM, TS_SMALLEST ) /= 0 ) THEN
! !          WRITE( errMsg, 100 ) 'Chemistry', TS_CHEM, TS_SMALLEST
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !       IF ( K /= -999999 .and. MOD( TS_EMIS, TS_SMALLEST ) /= 0 ) THEN
! !          WRITE( ErrMSg, 100 ) 'Emission', TS_EMIS, TS_SMALLEST
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !       IF ( J /= -999999 .and. MOD( TS_CONV, TS_SMALLEST ) /= 0 ) THEN
! !          WRITE( errMsg, 100 ) 'Convection', TS_CONV, TS_SMALLEST
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !       IF ( I /= -999999 .and. MOD( TS_DYN, TS_SMALLEST ) /= 0 ) THEN
! !          WRITE( errMsg, 100 ) 'Transport', TS_DYN, TS_SMALLEST
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !       ! Initialize timesteps in "time_mod.F90"
! !       CALL Set_Timesteps( Config_Opt,                                           &
! !          CHEMISTRY  = TS_CHEM,                                &
! !          EMISSION   = TS_EMIS,                                &
! !          DYNAMICS   = TS_DYN,                                 &
! !          UNIT_CONV  = TS_UNIT,                                &
! !          CONVECTION = TS_CONV,                                &
! !          DIAGNOS    = TS_DIAG,                                &
! !          RADIATION  = TS_RAD )

! ! 100   FORMAT( A, ' time step must be a multiple of the smallest one:', i5, i5 )

! !    END SUBROUTINE Check_Time_Steps
! ! !EOC
! ! #if !(defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING ))
! ! #ifdef BPCH_DIAG
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: is_last_day_good
! !
! ! !DESCRIPTION: Tests to see if there is output scheduled on the last day of
! !  the run.
! !\\
! !\\
! ! !INTERFACE:
! !
! !    SUBROUTINE IS_LAST_DAY_GOOD( Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       USE JULDAY_MOD,    ONLY : JULDAY
! !       USE TIME_MOD,      ONLY : GET_NYMDe, ITS_A_LEAPYEAR, YMD_EXTRACT
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt  ! Input options
! !       INTEGER,        INTENT(OUT)   :: RC         ! Success or failure?
! ! !
! ! ! !REVISION HISTORY:
! ! !  20 Jul 2004 - R. Yantosca - Initial version
! ! !  See https://github.com/geoschem/CATChem for complete history
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       LOGICAL            :: IS_LEAPYEAR
! !       INTEGER            :: NYMDe, Y, M, D, LASTDAY
! !       REAL(fp)           :: JD, JD0

! !       ! Strings
! !       CHARACTER(LEN=255) :: errMsg, thisLoc

! !       !=================================================================
! !       ! Is_Last_Day_Good begins here!
! !       !=================================================================

! !       ! Initialize
! !       RC      = CC_SUCCESS
! !       errMsg  = ''
! !       thisLoc = ' -> at Is_Last_Day_Good (in module CATChem/src/core/input_mod.F90)'

! !       ! Astronomical Julian Day corresponding to NYMDe
! !       NYMDe = GET_NYMDe()
! !       CALL YMD_EXTRACT( NYMDe, Y, M, D )
! !       JD = JULDAY( Y, M, DBLE( D ) )

! !       ! Astronomical Julian Day corresponding to the 1st of the year
! !       JD0 = JULDAY( Y, 1, 0d0 )

! !       ! LASTDAY is the day of year corresponding to NYMDe
! !       LASTDAY = JD - JD0

! !       ! Skip past the element of NJDAY for Feb 29, if necessary
! !       IF ( .not. ITS_A_LEAPYEAR( Y, .TRUE. ) .and. LASTDAY > 59 ) THEN
! !          LASTDAY = LASTDAY + 1
! !       ENDIF

! !       ! Exit w/ error if THIS_NJDAY = 0
! !       IF ( Config_Opt%NJDAY(LASTDAY) == 0 ) THEN
! !          errMsg = 'No output scheduled on last day of run!'
! !          CALL CC_Error( errMsg, RC, thisLoc )
! !          RETURN
! !       ENDIF

! !    END SUBROUTINE IS_LAST_DAY_GOOD
! ! !EOC
! ! #endif
! ! #endif
! ! !------------------------------------------------------------------------------
! ! !                  CATChem Global Chemical Transport Model                  !
! ! !------------------------------------------------------------------------------
! ! !BOP
! ! !
! ! ! !IROUTINE: do_error_checks
! ! !
! ! ! !DESCRIPTION: Makes sure that certain species are defined in order to
! ! !  proceed with a certain option.  Halts the simulation with an error message
! ! !  if incorrect inputs  would have caused  a simulation to crash.
! ! !\\
! ! !\\
! ! ! !INTERFACE:
! ! !
! !    SUBROUTINE Do_Error_Checks( Config_Opt, RC )
! ! !
! ! ! !USES:
! ! !
! !       USE Error_Mod
! !       USE Config_Opt_Mod, ONLY : OptConfig
! !       ! USE State_Chm_Mod, ONLY : Ind_
! ! !
! ! ! !INPUT/OUTPUT PARAMETERS:
! ! !
! !       TYPE(OptConfig), INTENT(INOUT) :: Config_Opt
! ! !
! ! ! !OUTPUT PARAMETERS:
! ! !
! !       INTEGER,        INTENT(OUT)   :: RC
! ! !
! ! ! !REMARKS:
! ! !  The Ind_() function now defines all species ID's.  It returns -1 if
! ! !  a species cannot be found.  The prior behavior was to return 0 if a
! ! !  species wasn't found.  Therefore, in order to preserve the logic of the
! ! !  error checks, we must force any -1's returned by Ind_() to 0's in
! ! !  this subroutine.
! ! !EOP
! ! !------------------------------------------------------------------------------
! ! !BOC
! ! !
! ! ! !LOCAL VARIABLES:
! ! !
! !       ! Scalars
! !       INTEGER            :: I

! !       ! Strings
! !       CHARACTER(LEN=255) :: MSG, LOCATION

! !       !=================================================================
! !       ! Initialization
! !       !=================================================================

! !       ! Assume success
! !       RC       = CC_SUCCESS

! !       ! Define location string
! !       LOCATION = '-> at Do_Error_Checks (in CATChem/src/core/input_mod.F90)'

! !       !=================================================================
! !       ! Error check SEASALT AEROSOLS
! !       !=================================================================
! !       I = MAX( Ind_('SALA','A'), 0 ) + MAX( Ind_('SALC','A'), 0 )

! !       IF ( Config_Opt%LSSALT ) THEN
! !          IF ( I == 0 ) THEN
! !             MSG = 'LSSALT=T but ONLINE SEASALT AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use ONLINE SEASALT AEROSOLS if LSSALT=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check MARINE ORGANIC AEROSOLS
! !       !=================================================================
! !       I = MAX( Ind_('MOPO','A'), 0 ) + MAX( Ind_('MOPI','A'), 0 )

! !       IF ( Config_Opt%LMPOA ) THEN
! !          IF ( .not. Config_Opt%LSSALT ) THEN
! !             MSG = 'LMPOA=T but LSSALT=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !          IF ( I == 0 ) THEN
! !             MSG = 'LMPOA=T but MARINE ORGANIC AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use MARINE ORGANIC AEROSOLS if LMPOA=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check SULFUR AEROSOLS
! !       !=================================================================
! !       I = MAX( Ind_('DMS' ,'A'), 0 ) + &
! !          MAX( Ind_('SO2' ,'A'), 0 ) + &
! !          MAX( Ind_('SO4' ,'A'), 0 ) + &
! !          MAX( Ind_('SO4s','A'), 0 ) + &
! !          MAX( Ind_('HMS' ,'A'), 0 ) + &! (jmm, 07/2/18)
! !          MAX( Ind_('MSA' ,'A'), 0 ) + &
! !          MAX( Ind_('NH3' ,'A'), 0 ) + &
! !          MAX( Ind_('NH4' ,'A'), 0 ) + &
! !          MAX( Ind_('NITs','A'), 0 )

! !       IF ( Config_Opt%LSULF ) THEN

! !          ! We now compute the production of SO4s and NITs, so when
! !          ! LSULF=T, then we must also have LSSALT=T (bec, bmy, 4/13/05)
! !          IF ( .not. Config_Opt%LSSALT ) THEN
! !             MSG = 'LSULF=T now also requires LSSALT=T!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF

! !          ! Stop w/ error if everything is undefined
! !          IF ( I == 0 ) THEN
! !             MSG = 'LSULF=T but ONLINE SULFUR AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF

! !       ELSE

! !          ! If LSULF=F but we have defined species, stop w/ error
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use ONLINE SULFUR AEROSOLS if LSULF=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF

! !       ENDIF

! !       !=================================================================
! !       ! Error check CARBON AEROSOLS
! !       !=================================================================

! !       ! SOAupdate: Add POA (hotp 10/11/09)
! !       I = MAX( Ind_('BCPO','A'), 0 ) + &
! !          MAX( Ind_('BCPI','A'), 0 ) + &
! !          MAX( Ind_('OCPO','A'), 0 ) + &
! !          MAX( Ind_('OCPI','A'), 0 ) + &
! !          MAX( Ind_('POA1','A'), 0 )

! !       IF ( Config_Opt%LCARB ) THEN
! !          IF ( I == 0 ) THEN
! !             MSG = 'LCARB=T but ONLINE CARBON AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use ONLINE CARBON AEROSOLS if LCARB=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       IF ( Config_Opt%LSVPOA .and. ( .NOT. Config_Opt%LSOA ) ) THEN
! !          MSG = 'Semivolatile POA requires COMPLEX SOA (LSOA=T)'
! !          CALL CC_Error( Msg, RC, Location )
! !          RETURN
! !       ENDIF

! !       ! SOAupdate: Error check (hotp 8/24/09)
! !       ! OCPI and OCPO are the non-volatile POA species
! !       ! POA (along w/ POG, OPOA, and OPOG) are the semivol POA species
! !       ! You can't have both!
! !       I = MAX( Ind_('OCPI','A'), 0 ) + MAX( Ind_('OCPO','A'), 0 )

! !       IF ( Ind_('POA1') > 0 ) THEN
! !          IF ( I > 0 ) THEN
! !             MSG = 'Semivolatile POA species is defined in addition to ' // &
! !                'Nonvolatile POA'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !          IF ( ( .NOT. Config_Opt%LSOA   ) .or. &
! !             ( .NOT. Config_Opt%LSVPOA ) ) THEN
! !             MSG = 'Semivolatile POA requires LSOA=T and LSVPOA=T'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       ! SOAupdate
! !       ! Options for organic aerosol species:
! !       ! IF LSOA = F: only OCPI and OCPO
! !       ! IF LSOA = T:
! !       !   OCPI OCPO SOA (non-vol + original traditional)
! !       !   POA POG OPOA OPOG SOA BTX NAP (semivol + orig trad + IVOC )
! !       ! NAP emissions are set in HEMCO_Config.rc
! !       ! LSVPOA is just a check (doesn't do anything hotp 7/21/10)
! !       I = MAX( Ind_('POA1' ,'A'), 0 ) + &
! !          MAX( Ind_('POA2' ,'A'), 0 ) + &
! !          MAX( Ind_('POG1' ,'A'), 0 ) + &
! !          MAX( Ind_('POG2' ,'A'), 0 ) + &
! !          MAX( Ind_('OPOA1','A'), 0 ) + &
! !          MAX( Ind_('OPOA2','A'), 0 ) + &
! !          MAX( Ind_('OPOG1','A'), 0 ) + &
! !          MAX( Ind_('OPOG2','A'), 0 )

! !       IF ( Config_Opt%LSVPOA ) THEN
! !          IF ( I < 8 ) THEN
! !             MSG = 'Not enough semivolatile POA species!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !          IF ( Ind_('NAP','A') < 0 ) THEN
! !             MSG = 'Semivolatile POA requires IVOCs/NAP!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check SECONDARY ORGANIC AEROSOLS
! !       !=================================================================

! !       ! Check for complex SOA species
! !       I = MAX( Ind_('TSOA1','A'), 0 ) + &
! !          MAX( Ind_('TSOA2','A'), 0 ) + &
! !          MAX( Ind_('TSOA3','A'), 0 ) + &
! !          MAX( Ind_('ASOA1','A'), 0 ) + &
! !          MAX( Ind_('ASOA2','A'), 0 ) + &
! !          MAX( Ind_('ASOA3','A'), 0 ) + &
! !          MAX( Ind_('ASOAN','A'), 0 ) + &
! !          MAX( Ind_('ASOG1','A'), 0 ) + &
! !          MAX( Ind_('ASOG2','A'), 0 ) + &
! !          MAX( Ind_('ASOG3','A'), 0 ) + &
! !          MAX( Ind_('TSOG0','A'), 0 ) + &
! !          MAX( Ind_('TSOG1','A'), 0 ) + &
! !          MAX( Ind_('TSOG2','A'), 0 ) + &
! !          MAX( Ind_('TSOG3','A'), 0 )

! !       IF ( Config_Opt%LSOA ) THEN
! !          IF ( I == 0 ) THEN
! !             MSG = 'LSOA=T but COMPLEX SOA species are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use COMPLEX SOA species if LSOA=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check DUST AEROSOLS
! !       !=================================================================

! ! #ifdef TOMAS
! !       ! For TOMAS only: If DUST01 is present, the other dust species are too
! !       I = MAX( Ind_('DUST01','A'), 0 )
! ! #else
! !       ! Non-TOMAS simulations: Need all DST1-DST4 species
! !       I = MAX( Ind_('DST1','A'), 0 ) + &
! !          MAX( Ind_('DST2','A'), 0 ) + &
! !          MAX( Ind_('DST3','A'), 0 ) + &
! !          MAX( Ind_('DST4','A'), 0 )
! ! #endif

! !       IF ( Config_Opt%LDUST ) THEN
! !          IF ( I == 0 ) THEN
! !             MSG = 'LDUST=T but ONLINE DUST AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use ONLINE DUST AEROSOLS if LDUST=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check DUST NITRATE    AEROSOLS
! !       !             DUST SULFATE    AEROSOLS
! !       !             DUST ALKALINITY AEROSOLS
! !       !=================================================================
! !       I = MAX( Ind_('NITd1'  ,'A'), 0 ) + &
! !          MAX( Ind_('NITd2'  ,'A'), 0 ) + &
! !          MAX( Ind_('NITd3'  ,'A'), 0 ) + &
! !          MAX( Ind_('NITd4'  ,'A'), 0 ) + &
! !          MAX( Ind_('SO4d1'  ,'A'), 0 ) + &
! !          MAX( Ind_('SO4d2'  ,'A'), 0 ) + &
! !          MAX( Ind_('SO4d3'  ,'A'), 0 ) + &
! !          MAX( Ind_('SO4d4'  ,'A'), 0 ) + &
! !          MAX( Ind_('DSTAL1' ,'A'), 0 ) + &
! !          MAX( Ind_('DSTAL2' ,'A'), 0 ) + &
! !          MAX( Ind_('DSTAL3' ,'A'), 0 ) + &
! !          MAX( Ind_('DSTAL4' ,'A'), 0 )

! !       IF ( Config_Opt%LDSTUP ) THEN
! !          IF ( I < 12 ) THEN
! !             MSG = 'LDSTUP=T but COATED DUST AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use COATED DUST AEROSOLS if LDSTUP=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check SEASALT AEROSOLS
! !       !=================================================================
! !       I = MAX( Ind_('SALA','A'), 0 ) + MAX( Ind_('SALC','A'), 0 )

! !       IF ( Config_Opt%LSSALT ) THEN
! !          IF ( I == 0 ) THEN
! !             MSG = 'LSSALT=T but ONLINE SEASALT AEROSOLS are undefined!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ELSE
! !          IF ( I > 0 ) THEN
! !             MSG = 'Cannot use ONLINE SEASALT AEROSOLS if LSSALT=F!'
! !             CALL CC_Error( Msg, RC, Location )
! !             RETURN
! !          ENDIF
! !       ENDIF

! !       !=================================================================
! !       ! Error check stratospheric H2O
! !       !=================================================================
! !       IF ( Config_Opt%LSETH2O .and. Ind_('H2O') < 0 ) THEN
! !          WRITE( 6, '(a)'     ) REPEAT( '=', 79 )
! !          WRITE( 6, '(/,a,/)' ) 'Warning in input_mod.F90: ' &
! !             // 'H2O is set but H2O species is undefined.'
! !          Config_Opt%LSETH2O = .FALSE.
! !          WRITE( 6, '(a)'     ) REPEAT( '=', 79 )
! !       ENDIF

! !    END SUBROUTINE Do_Error_Checks
! !EOC
! !------------------------------------------------------------------------------
! !                  CATChem Global Chemical Transport Model                  !
! !------------------------------------------------------------------------------
! !BOP
! !
! ! !IROUTINE: Find_Number_of_Species
! !
! ! !DESCRIPTION: Searches a string array containing species names and returns
! !  the number of valid species (i.e. species that do not match MISSING_STR).
! !  Assumes all the valid species will be listed contiguously at the front
! !  of the array
! !\\
! !\\
! ! !INTERFACE:
! !
!    FUNCTION Get_Number_of_Species( a_str ) RESULT( n_valid )
! !
! ! !INPUT PARAMETERS:
! !
!       CHARACTER(LEN=*), INTENT(IN) :: a_str(:)
! !
! ! !RETURN VALUE:
! !
!       INTEGER                      :: n_valid
! !EOP
! !------------------------------------------------------------------------------
! !BOC
! !
! ! !LOCAL VARIABLES:
! !
!       INTEGER :: N

!       ! Return the number of valid species
!       n_valid = 0
!       DO N = 1, SIZE( a_str )
!          IF ( TRIM( a_str(N) ) == MISSING_STR ) EXIT
!          n_valid = n_valid + 1
!       ENDDO

!    END FUNCTION Get_Number_of_Species
!EOC
END MODULE config_mod
