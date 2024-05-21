!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: Config_Opt_mod.F90
!
! !DESCRIPTION: Module INPUT\_OPT\_MOD contains the derived type for CATChem
!  options and logical switches.
!\\
!\\
! !INTERFACE:
!
MODULE Config_Opt_Mod
!
! !USES:
!
   USE PRECISION_MOD    ! For CATChem Precision (fp)

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC :: Set_Config_Opt
   PUBLIC :: Set_Config_Opt_Advect
   PUBLIC :: Cleanup_Config_Opt
!
! !PUBLIC DATA MEMBERS:
!
   !=========================================================================
   ! Derived type for Input Options
   !=========================================================================
   TYPE, PUBLIC :: OptConfig

      !----------------------------------------
      ! General Runtime & Distributed Comp Info
      !----------------------------------------
      INTEGER                     :: numCPUs    ! Number of MPI procs
      INTEGER                     :: thisCPU    ! Local MPI process handle
      INTEGER                     :: MPIComm    ! MPI Communicator Handle
      LOGICAL                     :: isMPI      ! Is this an MPI sim?
      LOGICAL                     :: amIRoot    ! Is this the root cpu?

      !----------------------------------------
      ! Dry run info (print out file names)
      !----------------------------------------
      LOGICAL                     :: DryRun     ! Is this a dry run?

      !----------------------------------------
      ! SIZE PARAMETER fields
      !----------------------------------------
      ! INTEGER                     :: Max_BPCH_Diag
      ! INTEGER                     :: Max_Families
      ! INTEGER                     :: Max_AdvectSpc

      !----------------------------------------
      ! SIMULATION MENU fields
      !----------------------------------------
      INTEGER                     :: NYMDb
      INTEGER                     :: NHMSb
      INTEGER                     :: NYMDe
      INTEGER                     :: NHMSe
      INTEGER                     :: SimLengthSec
      CHARACTER(LEN=255)          :: RUN_DIR
      CHARACTER(LEN=255)          :: DATA_DIR
      ! CHARACTER(LEN=255)          :: CHEM_INPUTS_DIR
      ! CHARACTER(LEN=255)          :: MetField
      CHARACTER(LEN=255)          :: SimulationName
      CHARACTER(LEN=255)          :: SpcDatabaseFile
      ! CHARACTER(LEN=255)          :: SpcMetaDataOutFile
      !  LOGICAL                     :: ITS_AN_AEROSOL_SIM
      !  LOGICAL                     :: ITS_A_CARBON_SIM
      !  LOGICAL                     :: ITS_A_CH4_SIM
      !  LOGICAL                     :: ITS_A_CO2_SIM
      !  LOGICAL                     :: ITS_A_FULLCHEM_SIM
      !  LOGICAL                     :: ITS_A_MERCURY_SIM
      !  LOGICAL                     :: ITS_A_POPS_SIM
      !  LOGICAL                     :: ITS_A_TAGCH4_SIM
      !  LOGICAL                     :: ITS_A_TAGCO_SIM
      !  LOGICAL                     :: ITS_A_TAGO3_SIM
      !  LOGICAL                     :: ITS_A_TRACEMETAL_SIM
      !  LOGICAL                     :: ITS_A_TRACER_SIM
      LOGICAL                     :: VerboseRequested
      CHARACTER(LEN=10)           :: VerboseOnCores
      LOGICAL                     :: Verbose
      ! LOGICAL                     :: useTimers

      !----------------------------------------
      ! ADVECTED SPECIES MENU fields
      !----------------------------------------
      INTEGER                     :: N_ADVECT
      CHARACTER(LEN=255), POINTER :: AdvectSpc_Name(:)
      LOGICAL                     :: LSPLIT

      !----------------------------------------
      ! AEROSOL MENU fields
      !----------------------------------------
      LOGICAL                     :: LSULF
      LOGICAL                     :: LMETALCATSO2
      LOGICAL                     :: LCARB
      LOGICAL                     :: LBRC
      LOGICAL                     :: LSOA
      LOGICAL                     :: LMPOA
      LOGICAL                     :: LSVPOA
      LOGICAL                     :: LDUST
      LOGICAL                     :: LDEAD
      LOGICAL                     :: LSSALT
      LOGICAL                     :: LDSTUP
      REAL(fp),           POINTER :: SALA_REDGE_um(:)
      REAL(fp),           POINTER :: SALC_REDGE_um(:)
      LOGICAL                     :: LGRAVSTRAT
      LOGICAL                     :: LSOLIDPSC
      LOGICAL                     :: LHOMNUCNAT
      REAL(fp)                    :: T_NAT_SUPERCOOL
      REAL(fp)                    :: P_ICE_SUPERSAT
      LOGICAL                     :: LPSCCHEM
      LOGICAL                     :: LSTRATOD
      !for BC absorption enhancement, (xnw, 8/24/15)
      LOGICAL                     :: LBCAE
      REAL(fp)                    :: BCAE_1
      REAL(fp)                    :: BCAE_2
      ! for nitrate aerosol photolysis (TMS, 23/08/2018)
      LOGICAL                     :: hvAerNIT
      REAL(fp)                    :: hvAerNIT_JNIT
      REAL(fp)                    :: hvAerNIT_JNITs
      REAL(fp)                    :: JNITChanA
      REAL(fp)                    :: JNITChanB

      !----------------------------------------
      ! EMISSIONS fields
      !----------------------------------------
      LOGICAL                     :: DoEmissions
      INTEGER                     :: TS_EMIS
      LOGICAL                     :: LBIOFUEL
      LOGICAL                     :: LOTDLOC
      LOGICAL                     :: LSOILNOX
      LOGICAL                     :: LCH4SBC
      LOGICAL                     :: LSETH2O
      LOGICAL                     :: LStaticH2OBC
      LOGICAL                     :: LHCodedOrgHal
      LOGICAL                     :: LCMIP6OrgHal
      LOGICAL                     :: DoLightNOx ! Shadow for LightNOX extension

      !----------------------------------------
      ! CO MENU fields
      !----------------------------------------
      LOGICAL                     :: LPCO_CH4
      LOGICAL                     :: LPCO_NMVOC

      !----------------------------------------
      ! CO2 MENU fields
      !----------------------------------------
      LOGICAL                     :: LFOSSIL
      LOGICAL                     :: LCHEMCO2
      LOGICAL                     :: LBIODIURNAL
      LOGICAL                     :: LBIONETCLIM
      LOGICAL                     :: LOCEAN
      LOGICAL                     :: LSHIP
      LOGICAL                     :: LPLANE
      LOGICAL                     :: LFFBKGRD
      LOGICAL                     :: LBIOSPHTAG
      LOGICAL                     :: LFOSSILTAG
      LOGICAL                     :: LSHIPTAG
      LOGICAL                     :: LPLANETAG

      !----------------------------------------
      ! CHEMISTRY MENU fields
      !----------------------------------------
      LOGICAL                     :: LCHEM
      LOGICAL                     :: LINEAR_CHEM
      LOGICAL                     :: LLINOZ
      LOGICAL                     :: LSYNOZ
      INTEGER                     :: TS_CHEM
      REAL(fp)                    :: GAMMA_HO2
      LOGICAL                     :: LACTIVEH2O
      LOGICAL                     :: LINITSPEC
      LOGICAL                     :: USE_ONLINE_O3
      LOGICAL                     :: USE_O3_FROM_MET
      LOGICAL                     :: USE_TOMS_O3
      LOGICAL                     :: USE_AUTOREDUCE
      LOGICAL                     :: AUTOREDUCE_IS_KEEPACTIVE
      LOGICAL                     :: AUTOREDUCE_IS_KEY_THRESHOLD
      LOGICAL                     :: AUTOREDUCE_IS_PRS_THRESHOLD
      LOGICAL                     :: AUTOREDUCE_IS_APPEND
      REAL(f8)                    :: AUTOREDUCE_THRESHOLD
      REAL(f8)                    :: AUTOREDUCE_TUNING_OH
      REAL(f8)                    :: AUTOREDUCE_TUNING_NO2
#ifdef MODEL_GEOS
      LOGICAL                     :: LGMIOZ
#endif

      !----------------------------------------
      ! PHOTOLYSIS MENU fields
      !----------------------------------------
      LOGICAL                     :: Do_Photolysis
      CHARACTER(LEN=255)          :: CloudJ_Dir

      !----------------------------------------
      ! RADIATION MENU fields
      !----------------------------------------
      LOGICAL                     :: LRAD
      LOGICAL                     :: LLWRAD
      LOGICAL                     :: LSWRAD
      LOGICAL,            POINTER :: LSKYRAD(:)
      INTEGER                     :: TS_RAD
      INTEGER                     :: NWVSELECT
      REAL(8),            POINTER :: WVSELECT(:)
      CHARACTER(LEN=5),   POINTER :: STRWVSELECT(:)
      INTEGER                     :: NSPECRADMENU
      INTEGER,            POINTER :: LSPECRADMENU(:)
      REAL(8)                     :: RRTMG_CO2_ppmv
      LOGICAL                     :: RRTMG_FDH
      LOGICAL                     :: RRTMG_SEFDH
      LOGICAL                     :: RRTMG_SA_TOA
      LOGICAL                     :: Read_Dyn_Heating

      !----------------------------------------
      ! DEPOSITION MENU fields
      !----------------------------------------
      LOGICAL                     :: LDRYD
      LOGICAL                     :: LWETD
      REAL(fp)                    :: WETD_CONV_SCAL
      LOGICAL                     :: PBL_DRYDEP
      LOGICAL                     :: CO2_EFFECT
      REAL(fp)                    :: CO2_LEVEL
      REAL(fp)                    :: CO2_REF
      REAL(fp)                    :: RS_SCALE
      INTEGER                     :: RA_Alt_Above_Sfc

      !----------------------------------------
      ! GAMAP MENU fields
      !----------------------------------------
      CHARACTER(LEN=255)          :: GAMAP_DIAGINFO
      CHARACTER(LEN=255)          :: GAMAP_TRACERINFO

      !----------------------------------------
      ! OUTPUT MENU fields
      !----------------------------------------
      INTEGER,            POINTER :: NJDAY(:)

      !----------------------------------------
      ! DIAGNOSTIC MENU fields
      !----------------------------------------
      CHARACTER(LEN=255)          :: HistoryInputFile
      INTEGER                     :: ND03   ! Hg
      INTEGER                     :: ND06   ! TOMAS
      INTEGER                     :: ND44   ! TOMAS
      INTEGER                     :: ND53   ! POPs
      INTEGER                     :: ND59   ! TOMAS
      INTEGER                     :: ND60   ! TOMAS
      INTEGER                     :: ND61   ! TOMAS

      INTEGER                     :: TS_DIAG
      INTEGER,            POINTER :: TINDEX(:,:)
      INTEGER,            POINTER :: TCOUNT(:)
      INTEGER,            POINTER :: TMAX(:)
      LOGICAL                     :: DO_DIAG_WRITE

      ! Collection ids
      INTEGER                     :: DIAG_COLLECTION
      INTEGER                     :: CC_RST_COLLECTION ! Used only for NetCDF

      !  !----------------------------------------
      !  ! PLANEFLIGHT MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: Do_Planeflight
      !  CHARACTER(LEN=255)          :: Planeflight_InFile
      !  CHARACTER(LEN=255)          :: Planeflight_OutFile

      !  !----------------------------------------
      !  ! OBSPACK MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: Do_ObsPack
      !  LOGICAL                     :: ObsPack_Quiet
      !  CHARACTER(LEN=255)          :: ObsPack_InputFile
      !  CHARACTER(LEN=255)          :: ObsPack_OutputFile
      !  INTEGER                     :: ObsPack_nSpc
      !  CHARACTER(LEN=255), POINTER :: ObsPack_SpcName(:)

      !  !----------------------------------------
      !  ! ND51 MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: DO_ND51
      !  INTEGER                     :: N_ND51
      !  CHARACTER(LEN=255)          :: ND51_FILE
      !  INTEGER,            POINTER :: ND51_TRACERS(:)
      !  REAL(fp)                    :: ND51_HR_WRITE
      !  REAL(fp)                    :: ND51_HR1
      !  REAL(fp)                    :: ND51_HR2
      !  INTEGER                     :: ND51_IMIN
      !  INTEGER                     :: ND51_IMAX
      !  INTEGER                     :: ND51_JMIN
      !  INTEGER                     :: ND51_JMAX
      !  INTEGER                     :: ND51_LMIN
      !  INTEGER                     :: ND51_LMAX

      !  !----------------------------------------
      !  ! ND51b MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: DO_ND51b
      !  INTEGER                     :: N_ND51b
      !  CHARACTER(LEN=255)          :: ND51b_FILE
      !  INTEGER,            POINTER :: ND51b_TRACERS(:)
      !  REAL(fp)                    :: ND51b_HR_WRITE
      !  REAL(fp)                    :: ND51b_HR1
      !  REAL(fp)                    :: ND51b_HR2
      !  INTEGER                     :: ND51b_IMIN
      !  INTEGER                     :: ND51b_IMAX
      !  INTEGER                     :: ND51b_JMIN
      !  INTEGER                     :: ND51b_JMAX
      !  INTEGER                     :: ND51b_LMIN
      !  INTEGER                     :: ND51b_LMAX

      !  !----------------------------------------
      !  ! PROD LOSS MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: DO_SAVE_PL
      !  INTEGER                     :: ND65, LD65
      !  INTEGER                     :: NFAM
      !  CHARACTER(LEN=255), POINTER :: FAM_NAME(:)
      !  CHARACTER(LEN=255), POINTER :: FAM_TYPE(:)

      !  !----------------------------------------
      !  ! BENCHMARK MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: LSTDRUN
      !  CHARACTER(LEN=255)          :: STDRUN_INIT_FILE
      !  CHARACTER(LEN=255)          :: STDRUN_FINAL_FILE

      !  !----------------------------------------
      !  ! MERCURY MENU fields
      !  !----------------------------------------
      !  INTEGER                     :: ANTHRO_Hg_YEAR
      !  CHARACTER(LEN=255)          :: HG_SCENARIO
      !  LOGICAL                     :: USE_CHECKS
      !  LOGICAL                     :: LDYNOCEAN
      !  LOGICAL                     :: LPREINDHG
      !  LOGICAL                     :: LGTMM
      !  CHARACTER(LEN=255)          :: GTMM_RST_FILE
      !  LOGICAL                     :: LARCTICRIV
      !  LOGICAL                     :: LKRedUV

      !  !----------------------------------------
      !  ! CH4 MENU fields
      !  !----------------------------------------
      !  LOGICAL                     :: GOSAT_CH4_OBS
      !  LOGICAL                     :: AIRS_CH4_OBS
      !  LOGICAL                     :: TCCON_CH4_OBS
      !  LOGICAL                     :: DoAnalyticalInv
      !  INTEGER                     :: StateVectorElement
      !  REAL(fp)                    :: EmisPerturbFactor
      !  LOGICAL                     :: DoPerturbCH4BoundaryConditions
      !  REAL(fp)                    :: CH4BoundaryConditionIncreaseNorth
      !  REAL(fp)                    :: CH4BoundaryConditionIncreaseSouth
      !  REAL(fp)                    :: CH4BoundaryConditionIncreaseEast
      !  REAL(fp)                    :: CH4BoundaryConditionIncreaseWest
      !  LOGICAL                     :: UseEmisSF
      !  LOGICAL                     :: UseOHSF

      !  !----------------------------------------
      !  ! POPS MENU fields
      !  !----------------------------------------
      !  CHARACTER(LEN=3)            :: POP_TYPE
      !  LOGICAL                     :: CHEM_PROCESS
      !  REAL(fp)                    :: POP_XMW
      !  REAL(fp)                    :: POP_KOA
      !  REAL(fp)                    :: POP_KBC
      !  REAL(fp)                    :: POP_K_POPG_OH
      !  REAL(fp)                    :: POP_K_POPP_O3A
      !  REAL(fp)                    :: POP_K_POPP_O3B
      !  REAL(fp)                    :: POP_HSTAR
      !  REAL(fp)                    :: POP_DEL_H
      !  REAL(fp)                    :: POP_DEL_Hw

      !----------------------------------------
      ! Fields for interface to GEOS-5 GCM
      !----------------------------------------
#ifdef MODEL_GEOS
      LOGICAL                     :: LCAPTROP     = .FALSE.
      !REAL(fp)                    :: OZONOPAUSE   = -999.0
      LOGICAL                     :: haveImpRst   = .FALSE.
      LOGICAL                     :: AlwaysSetH2O = .TRUE.
      LOGICAL                     :: UseOnlineVUD = .FALSE.
      INTEGER                     :: LLFASTJX     = 601
      INTEGER                     :: NN_RxnRates             ! # of diagnosed reaction rates
      INTEGER, POINTER            :: RxnRates_IDs(:)         ! Reaction rate numbers to be diagnosed
      INTEGER                     :: NN_RxnRconst            ! # of diagnosed reaction rates
      INTEGER, POINTER            :: RxnRconst_IDs(:)        ! Reaction rate numbers to be diagnosed
      INTEGER                     :: NN_Jvals                ! # of diagnosed Jvalues
      INTEGER, POINTER            :: Jval_IDs(:)             ! J-values to be diagnosed
      INTEGER                     :: FJX_EXTRAL_ITERMAX = 5
      LOGICAL                     :: FJX_EXTRAL_ERR     = .TRUE.
      ! Toggle for het rates. If true, turns off three Cl producing het reactions
      ! in the stratosphere. In MODEL_GEOS, this flag is set in GEOSCHEMchem_GridComp.rc
      LOGICAL                     :: TurnOffHetRates    = .TRUE.
      INTEGER                     :: KppCheckNegatives  = -1      ! Check for negatives after KPP integration
      REAL(fp)                    :: KppTolScale        = 1.0_fp  ! Tolerance scale factor for 2nd KPP integration
      LOGICAL                     :: applyQtend         = .FALSE. ! Apply water vapor tendency
#else
      LOGICAL                     :: AlwaysSetH2O
      LOGICAL                     :: TurnOffHetRates
#endif

#if defined( MODEL_GEOS ) || defined( MODEL_WRF ) || defined( MODEL_CESM )
      LOGICAL                     :: KppStop            = .TRUE. ! Stop KPP if integration fails twice
#endif

#if defined( MODEL_CESM )
      ! Use albedo from land model
      LOGICAL                     :: onlineAlbedo       = .TRUE.
      ! Apply water vapor tendency to specific humidity
      LOGICAL                     :: applyQtend         = .TRUE.
      ! Apply photolytic correction for convective scavenging of soluble tracers?
      LOGICAL                     :: correctConvUTLS    = .TRUE.
#endif

#ifdef ADJOINT
      !----------------------------------------
      ! GCHP adjoint fields
      !---------------------------------------
      LOGICAL                     :: IS_ADJOINT
      LOGICAL                     :: IS_FD_SPOT, IS_FD_GLOBAL
      INTEGER                     :: FD_STEP
      LOGICAL                     :: IS_FD_SPOT_THIS_PET
      INTEGER                     :: IFD, JFD, NFD, LFD, NFD_ADJ
      INTEGER                     :: CF_IMIN, CF_IMAX
      INTEGER                     :: CF_JMIN, CF_JMAX
      INTEGER                     :: CF_LMIN, CF_LMAX
#endif

      !----------------------------------------
      ! Fields for LINOZ strat chem
      !----------------------------------------
      INTEGER                     :: LINOZ_NLEVELS
      INTEGER                     :: LINOZ_NLAT
      INTEGER                     :: LINOZ_NMONTHS
      INTEGER                     :: LINOZ_NFIELDS
      REAL(fp),           POINTER :: LINOZ_TPARM(:,:,:,:)

#if defined( ESMF_ )
      ! ESMF logger
      class(Logger), pointer      :: lgr
      Character(Len=255)          :: compname
#endif

   END TYPE OptConfig
!
! !REMARKS:
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set_Config_Opt
!
! !DESCRIPTION: Subroutine SET\_INPUT\_OPT intializes all CATChem
!  options carried in Input Options derived type object.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Set_Config_Opt( am_I_Root, Config_Opt, RC )
!
! !USES:
!
      USE Error_Mod
!
! !INPUT PARAMETERS:
!
      LOGICAL,        INTENT(IN)    :: am_I_Root   ! Are we on the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  01 Nov 2012 - R. Yantosca - Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Strings
      CHARACTER(LEN=30) :: arrayId

      !----------------------------------------
      ! Initialize
      ! Set pointers to NULL for safety's sake
      !----------------------------------------
      RC                               =  CC_SUCCESS
      Config_Opt%AdvectSpc_Name         => NULL()
      Config_Opt%SALA_REDGE_um          => NULL()
      Config_Opt%SALC_REDGE_um          => NULL()
      Config_Opt%LSKYRAD                => NULL()
      Config_Opt%LSPECRADMENU           => NULL()
      Config_Opt%NJDAY                  => NULL()
      Config_Opt%TINDEX                 => NULL()
      Config_Opt%TCOUNT                 => NULL()
      Config_Opt%TMAX                   => NULL()
      ! Config_Opt%ND51_TRACERS           => NULL()
      ! ! Config_Opt%ND51b_TRACERS          => NULL()
      ! Config_Opt%FAM_NAME               => NULL()
      ! Config_Opt%FAM_TYPE               => NULL()
      ! Config_Opt%LINOZ_TPARM            => NULL()

      !----------------------------------------
      ! General Runtime & Distributed Comp Info
      !----------------------------------------
      Config_Opt%amIRoot                = am_I_Root
      Config_Opt%isMPI                  = .FALSE.
      Config_Opt%numCPUs                = 1
      Config_Opt%thisCPU                = -1
      Config_Opt%MPIComm                = -1

      !----------------------------------------
      ! Dry run info (print out file names)
      !----------------------------------------
      Config_Opt%DryRun                 = .FALSE.

      !----------------------------------------
      ! SIZE PARAMETER fields
      !
      ! Set to large placeholder values
      !----------------------------------------
! #ifdef RRTMG
!       Config_Opt%Max_BPCH_Diag          = 187 ! Mirror MAX_DIAG in CMN_DIAG_mod.F90
! #else
!       Config_Opt%Max_BPCH_Diag          = 80  ! Mirror MAX_DIAG in CMN_DIAG_mod.F90
! #endif
!       Config_Opt%Max_Families           = 250
!       Config_Opt%Max_AdvectSpc          = 600

      ! !----------------------------------------
      ! ! SIMULATION MENU fields
      ! !----------------------------------------
      ! Config_Opt%NYMDb                  = 0
      ! Config_Opt%NHMSb                  = 0
      ! Config_Opt%NYMDe                  = 0
      ! Config_Opt%NHMSe                  = 0
      ! Config_Opt%SimLengthSec           = 0
      ! Config_Opt%RUN_DIR                = './'
      ! Config_Opt%DATA_DIR               = './'
      ! Config_Opt%CHEM_INPUTS_DIR        = './'
      ! Config_Opt%MetField               = ''
      ! Config_Opt%SimulationName         = ''
      ! Config_Opt%SpcDatabaseFile        = ''
      ! Config_Opt%SpcMetaDataOutFile     = ''
      ! Config_Opt%ITS_AN_AEROSOL_SIM     = .FALSE.
      ! Config_Opt%ITS_A_CARBON_SIM       = .FALSE.
      ! Config_Opt%ITS_A_CH4_SIM          = .FALSE.
      ! Config_Opt%ITS_A_CO2_SIM          = .FALSE.
      ! Config_Opt%ITS_A_FULLCHEM_SIM     = .FALSE.
      ! Config_Opt%ITS_A_MERCURY_SIM      = .FALSE.
      ! Config_Opt%ITS_A_POPS_SIM         = .FALSE.
      ! Config_Opt%ITS_A_TAGCH4_SIM       = .FALSE.
      ! Config_Opt%ITS_A_TAGCO_SIM        = .FALSE.
      ! Config_Opt%ITS_A_TAGO3_SIM        = .FALSE.
      ! Config_Opt%ITS_A_TRACEMETAL_SIM   = .FALSE.
      ! Config_Opt%ITS_A_TRACER_SIM       = .FALSE.
      ! Config_Opt%VerboseRequested       = .FALSE.
      ! Config_Opt%VerboseOnCores         = ''
      ! Config_Opt%Verbose                = .FALSE.
      ! Config_Opt%useTimers              = .FALSE.

      ! !----------------------------------------
      ! ! ADVECTED SPECIES MENU fields
      ! !----------------------------------------
      ! arrayId = 'Config_Opt%AdvectSpc_Name'
      ! ALLOCATE( Config_Opt%AdvectSpc_Name( Config_Opt%Max_AdvectSpc ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      Config_Opt%N_ADVECT               = 0
      Config_Opt%AdvectSpc_Name         = ''
      Config_Opt%LSPLIT                 = .FALSE.

      !----------------------------------------
      ! AEROSOL MENU fields
      !----------------------------------------
      arrayId = 'Config_Opt%SALA_REDGE_um'
      ALLOCATE( Config_Opt%SALA_REDGE_um( 2 ), STAT=RC )
      CALL CC_CheckVar( arrayId, 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN

      arrayId = 'Config_Opt%SALC_REDGE_um'
      ALLOCATE( Config_Opt%SALC_REDGE_um( 2 ), STAT=RC )
      CALL CC_CheckVar( arrayId, 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN

      Config_Opt%LSULF                  = .FALSE.
      Config_Opt%LMETALCATSO2           = .FALSE.
      Config_Opt%LCARB                  = .FALSE.
      Config_Opt%LBRC                   = .FALSE.
      Config_Opt%LSOA                   = .FALSE.
      Config_Opt%LMPOA                  = .FALSE.
      Config_Opt%LSVPOA                 = .FALSE.
      Config_Opt%LDUST                  = .FALSE.
      Config_Opt%LDEAD                  = .FALSE.
      Config_Opt%LDSTUP                 = .FALSE.
      Config_Opt%LSSALT                 = .FALSE.
      Config_Opt%SALA_REDGE_um          = 0.0_fp
      Config_Opt%SALC_REDGE_um          = 0.0_fp
      Config_Opt%LGRAVSTRAT             = .FALSE.
      Config_Opt%LSOLIDPSC              = .FALSE.
      Config_Opt%LHOMNUCNAT             = .FALSE.
      Config_Opt%T_NAT_SUPERCOOL        = 0.0_fp
      Config_Opt%P_ICE_SUPERSAT         = 0.0_fp
      Config_Opt%LPSCCHEM               = .FALSE.
      Config_Opt%LSTRATOD               = .FALSE.
      Config_Opt%hvAerNIT               = .FALSE.
      Config_Opt%hvAerNIT_JNIT          = 0.0_fp
      Config_Opt%hvAerNIT_JNITs         = 0.0_fp
      Config_Opt%JNITChanA              = 0.0_fp
      Config_Opt%JNITChanB              = 0.0_fp

      ! !----------------------------------------
      ! ! EMISSIONS MENU fields
      ! !----------------------------------------
      ! Config_Opt%DoEmissions            = .TRUE. ! On by default
      ! Config_Opt%TS_EMIS                = 0
      ! Config_Opt%LSOILNOX               = .FALSE.
      ! Config_Opt%LCH4SBC                = .FALSE.
      ! Config_Opt%LSETH2O                = .FALSE.
      ! Config_Opt%LStaticH2OBC           = .FALSE.
      ! Config_Opt%LHCodedOrgHal          = .FALSE.
      ! Config_Opt%LCMIP6OrgHal           = .FALSE.
      ! Config_Opt%DoLightNOx             = .FALSE.
      ! Config_Opt%LIMGRID                = .FALSE.
      ! Config_Opt%IMGRID_XSCALE          = 1
      ! Config_Opt%IMGRID_YSCALE          = 1

      ! !----------------------------------------
      ! ! CO MENU fields
      ! !----------------------------------------
      ! Config_Opt%LPCO_CH4               = .FALSE.
      ! Config_Opt%LPCO_NMVOC             = .FALSE.

      ! !----------------------------------------
      ! ! CO2 MENU fields
      ! !----------------------------------------
      ! Config_Opt%LFOSSIL                = .FALSE.
      ! Config_Opt%LCHEMCO2               = .FALSE.
      ! Config_Opt%LBIOFUEL               = .FALSE.
      ! Config_Opt%LBIODIURNAL            = .FALSE.
      ! Config_Opt%LBIONETCLIM            = .FALSE.
      ! Config_Opt%LOCEAN                 = .FALSE.
      ! Config_Opt%LSHIP                  = .FALSE.
      ! Config_Opt%LPLANE                 = .FALSE.
      ! Config_Opt%LFFBKGRD               = .FALSE.
      ! Config_Opt%LBIOSPHTAG             = .FALSE.
      ! Config_Opt%LFOSSILTAG             = .FALSE.
      ! Config_Opt%LSHIPTAG               = .FALSE.
      ! Config_Opt%LPLANETAG              = .FALSE.

!     !----------------------------------------
!     ! CHEMISTRY MENU fields
!     !----------------------------------------
!     Config_Opt%LCHEM                  = .FALSE.
!     Config_Opt%LINEAR_CHEM            = .FALSE.
!     Config_Opt%LLINOZ                 = .FALSE.
!     Config_Opt%LSYNOZ                 = .FALSE.
! ! #ifdef MODEL_GEOS
! !     Config_Opt%LGMIOZ                 = .FALSE.
! ! #endif
!     Config_Opt%TS_CHEM                = 0
!     Config_Opt%GAMMA_HO2              = 0.0_fp
!     Config_Opt%LACTIVEH2O             = .FALSE.
!     Config_Opt%LINITSPEC              = .FALSE.
!     Config_Opt%USE_ONLINE_O3          = .FALSE.
!     Config_Opt%USE_O3_FROM_MET        = .FALSE.
!     Config_Opt%USE_TOMS_O3            = .FALSE.

!     Config_Opt%USE_AUTOREDUCE                = .FALSE.
!     Config_Opt%AUTOREDUCE_IS_KEY_THRESHOLD   = .TRUE.
!     Config_Opt%AUTOREDUCE_TUNING_OH          = 5e-5_fp
!     Config_Opt%AUTOREDUCE_TUNING_NO2         = 1e-4_fp
!     Config_Opt%AUTOREDUCE_IS_PRS_THRESHOLD   = .TRUE.
!     Config_Opt%AUTOREDUCE_IS_KEEPACTIVE      = .FALSE.
!     Config_Opt%AUTOREDUCE_IS_APPEND          = .FALSE.

      !----------------------------------------
      ! PHOTOLYSIS MENU fields
      !----------------------------------------
      ! Config_Opt%Do_Photolysis         = .FALSE.
      ! Config_Opt%FAST_JX_DIR           = ''
      ! Config_Opt%CloudJ_Dir            = ''

      ! !----------------------------------------
      ! ! RADIATION MENU fields (for RRTMG only)
      ! !----------------------------------------
      ! arrayId = 'Config_Opt%LSKYRAD'
      ! ALLOCATE( Config_Opt%LSKYRAD( 2 ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! arrayId = 'Config_Opt%WVSELECT'
      ! ALLOCATE( Config_Opt%WVSELECT( 3 ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! arrayId = 'Config_Opt%STRWVSELECT'
      ! ALLOCATE( Config_Opt%STRWVSELECT( 3 ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! Number of RRTMG outputs (change as necessary)
      Config_Opt%NSpecRadMenu           = 17

      arrayId = 'Config_Opt%LSPECRADMENU'
      ALLOCATE( Config_Opt%LSPECRADMENU( Config_Opt%NSpecRadMenu ), STAT=RC )
      CALL CC_CheckVar( arrayId, 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      Config_Opt%LSpecRadMenu           = 0

      Config_Opt%LRAD                   = .FALSE.
      Config_Opt%LLWRAD                 = .FALSE.
      Config_Opt%LSWRAD                 = .FALSE.
      Config_Opt%LSKYRAD                = .FALSE.
      Config_Opt%TS_RAD                 = 0
      Config_Opt%NWVSELECT              = 0
      Config_Opt%WVSELECT               = 0.0_fp
      Config_Opt%STRWVSELECT            = ''
      Config_Opt%RRTMG_CO2_ppmv         = 3.90e-4_fp
      Config_Opt%RRTMG_FDH              = .FALSE.
      Config_Opt%RRTMG_SEFDH            = .FALSE.
      Config_Opt%RRTMG_SA_TOA           = .FALSE.
      Config_Opt%Read_Dyn_Heating       = .FALSE.

      ! !----------------------------------------
      ! ! TRANSPORT MENU fields
      ! !----------------------------------------
      ! Config_Opt%LTRAN                  = .FALSE.
      ! Config_Opt%LFILL                  = .FALSE.
      ! Config_Opt%TPCORE_IORD            = 0
      ! Config_Opt%TPCORE_JORD            = 0
      ! Config_Opt%TPCORE_KORD            = 0
      ! Config_Opt%TS_DYN                 = 0

      ! !----------------------------------------
      ! ! CONVECTION MENU fields
      ! !----------------------------------------
      ! Config_Opt%LCONV                  = .FALSE.
      ! Config_Opt%LTURB                  = .FALSE.
      ! Config_Opt%LNLPBL                 = .FALSE.
      ! Config_Opt%TS_CONV                = 0

      !----------------------------------------
      ! DEPOSITION MENU fields
      !----------------------------------------
      Config_Opt%LDRYD                  = .FALSE.
      Config_Opt%LWETD                  = .FALSE.
      Config_Opt%WETD_CONV_SCAL         = 1.0_fp
      Config_Opt%PBL_DRYDEP             = .FALSE.
      Config_Opt%CO2_LEVEL              = 390.0_fp
      Config_Opt%CO2_REF                = 390.0_fp
      Config_Opt%CO2_EFFECT             = .FALSE.
      Config_Opt%RS_SCALE               = 1.0_fp
      Config_Opt%RA_Alt_Above_Sfc       = 10       ! default height


      !----------------------------------------
      ! GAMAP_MENU fields
      !----------------------------------------
      Config_Opt%GAMAP_DIAGINFO         = ''
      Config_Opt%GAMAP_TRACERINFO       = ''

      ! !----------------------------------------
      ! ! OUTPUT MENU fields
      ! !----------------------------------------
      ! arrayId = 'Config_Opt%NJDAY'
      ! ALLOCATE( Config_Opt%NJDAY( 366 ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! Config_Opt%NJDAY                  = 0

      ! !----------------------------------------
      ! ! DIAGNOSTIC MENU fields
      ! !----------------------------------------
      ! Config_Opt%HistoryInputFile       = ''
      ! Config_Opt%DIAG_COLLECTION        = -999
      ! Config_Opt%TS_DIAG                = 0
      ! ALLOCATE( Config_Opt%TCOUNT( Config_Opt%Max_BPCH_Diag ), STAT=RC )
      ! ALLOCATE( Config_Opt%TMAX  ( Config_Opt%Max_BPCH_Diag ), STAT=RC )

      ! Config_Opt%ND03                   = 0
      ! Config_Opt%ND06                   = 0
      ! Config_Opt%ND44                   = 0
      ! Config_Opt%ND53                   = 0
      ! Config_Opt%ND59                   = 0
      ! Config_Opt%ND60                   = 0
      ! Config_Opt%ND61                   = 0
      ! Config_Opt%ND65                   = 0
      ! Config_Opt%TCOUNT(:)              = 0
      ! Config_Opt%TMAX(:)	             = 0
! #if defined( ESMF_ ) || defined( EXTERNAL_GRID ) || defined( EXTERNAL_FORCING )
!     ! Need to shut off G-C diagnostics when
!     ! connecting to an external GCM (bmy, 3/29/13)
!     Config_Opt%DO_DIAG_WRITE          = .FALSE.
! #else
!     ! For traditional G-C runs, always write diags (bmy, 3/29/13)
!     Config_Opt%DO_DIAG_WRITE          = .TRUE.
! #endif

      ! !----------------------------------------
      ! ! PLANEFLIGHT MENU fields
      ! !----------------------------------------
      ! Config_Opt%Do_Planeflight         = .FALSE.
      ! Config_Opt%Planeflight_InFile     = ''
      ! Config_Opt%Planeflight_OutFile    = ''

      ! !----------------------------------------
      ! ! PLANEFLIGHT MENU fields
      ! !----------------------------------------
      ! ALLOCATE( Config_Opt%ObsPack_SpcName( 1000 ), STAT=RC )

      ! Config_Opt%Do_ObsPack             = .FALSE.
      ! Config_Opt%ObsPack_Quiet          = .FALSE.
      ! Config_Opt%ObsPack_InputFile      = ''
      ! Config_Opt%ObsPack_OutputFile     = ''
      ! Config_Opt%ObsPack_nSpc           = 0
      ! Config_Opt%ObsPack_SpcName        = ''

      ! !----------------------------------------
      ! ! ND51 MENU fields
      ! !----------------------------------------
      ! Config_Opt%DO_ND51                = .FALSE.
      ! Config_Opt%N_ND51                 = 0
      ! Config_Opt%ND51_FILE              = ''
      ! Config_Opt%ND51_HR_WRITE          = 0.0_fp
      ! Config_Opt%ND51_HR1               = 0.0_fp
      ! Config_Opt%ND51_HR2               = 0.0_fp
      ! Config_Opt%ND51_IMIN              = 0
      ! Config_Opt%ND51_IMAX              = 0
      ! Config_Opt%ND51_JMIN              = 0
      ! Config_Opt%ND51_JMAX              = 0
      ! Config_Opt%ND51_LMIN              = 0

      ! !----------------------------------------
      ! ! ND51b MENU fields
      ! !----------------------------------------
      ! Config_Opt%DO_ND51b               = .FALSE.
      ! Config_Opt%N_ND51b                = 0
      ! Config_Opt%ND51b_FILE             = ''
      ! Config_Opt%ND51b_HR_WRITE         = 0.0_fp
      ! Config_Opt%ND51b_HR1              = 0.0_fp
      ! Config_Opt%ND51b_HR2              = 0.0_fp
      ! Config_Opt%ND51b_IMIN             = 0
      ! Config_Opt%ND51b_IMAX             = 0
      ! Config_Opt%ND51b_JMIN             = 0
      ! Config_Opt%ND51b_JMAX             = 0
      ! Config_Opt%ND51b_LMIN             = 0

      ! !----------------------------------------
      ! ! PROD LOSS MENU fields
      ! !---------------------------------------

      ! arrayId = 'Config_Opt%FAM_NAME'
      ! ALLOCATE( Config_Opt%FAM_NAME( Config_Opt%Max_Families ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! arrayId = 'Config_Opt%FAM_TYPE'
      ! ALLOCATE( Config_Opt%FAM_TYPE( Config_Opt%Max_Families ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! Config_Opt%DO_SAVE_PL             = .FALSE.
      ! Config_Opt%ND65                   = 0
      ! Config_Opt%NFAM                   = 0
      ! Config_Opt%FAM_NAME               = ''
      ! Config_Opt%FAM_TYPE               = ''

      ! !----------------------------------------
      ! ! MERCURY MENU fields
      ! !----------------------------------------
      ! Config_Opt%ANTHRO_Hg_YEAR         = 0
      ! Config_Opt%HG_SCENARIO            = ''
      ! Config_Opt%USE_CHECKS             = .FALSE.
      ! Config_Opt%LDYNOCEAN              = .FALSE.
      ! Config_Opt%LPREINDHG              = .FALSE.
      ! Config_Opt%LGTMM                  = .FALSE.
      ! Config_Opt%GTMM_RST_FILE          = ''

      ! !----------------------------------------
      ! ! CH4 MENU fields
      ! !----------------------------------------
      ! Config_Opt%GOSAT_CH4_OBS                     = .FALSE.
      ! Config_Opt%AIRS_CH4_OBS                      = .FALSE.
      ! Config_Opt%TCCON_CH4_OBS                     = .FALSE.
      ! Config_Opt%DoAnalyticalInv                   = .FALSE.
      ! Config_Opt%StateVectorElement                = 0
      ! Config_Opt%EmisPerturbFactor                 = 1.0
      ! Config_Opt%DoPerturbCH4BoundaryConditions    = .FALSE.
      ! Config_Opt%CH4BoundaryConditionIncreaseNorth = 0.0_fp
      ! Config_Opt%CH4BoundaryConditionIncreaseSouth = 0.0_fp
      ! Config_Opt%CH4BoundaryConditionIncreaseEast  = 0.0_fp
      ! Config_Opt%CH4BoundaryConditionIncreaseWest  = 0.0_fp
      ! Config_Opt%UseEmisSF                         = .FALSE.
      ! Config_Opt%UseOHSF                           = .FALSE.

      ! !----------------------------------------
      ! ! POPS MENU fields
      ! !----------------------------------------
      ! Config_Opt%POP_TYPE               = ''
      ! Config_Opt%CHEM_PROCESS           = .FALSE.
      ! Config_Opt%POP_XMW                = 0.0_fp
      ! Config_Opt%POP_KOA                = 0.0_fp
      ! Config_Opt%POP_KBC                = 0.0_fp
      ! Config_Opt%POP_K_POPG_OH          = 0.0_fp
      ! Config_Opt%POP_K_POPP_O3A         = 0.0_fp
      ! Config_Opt%POP_K_POPP_O3B         = 0.0_fp
      ! Config_Opt%POP_HSTAR              = 0.0_fp
      ! Config_Opt%POP_DEL_H              = 0.0_fp
      ! Config_Opt%POP_DEL_Hw             = 0.0_fp

      !----------------------------------------
      ! Fields for interface to GEOS-5 GCM
      !----------------------------------------
! #ifdef MODEL_GEOS
! !    Config_Opt%OZONOPAUSE             = -999.0
! !    Config_Opt%haveImpRst             = .FALSE.
! !    Config_Opt%AlwaysSetH2O           = .FALSE.
! !    Config_Opt%LLFASTJX               = -999
!     Config_Opt%NN_RxnRates            = -999
!     Config_Opt%RxnRates_IDs           => NULL()
!     Config_Opt%NN_RxnRconst           = -999
!     Config_Opt%RxnRconst_IDs          => NULL()
!     Config_Opt%NN_Jvals               = -999
!     Config_Opt%Jval_IDs               => NULL()
! #else
!     Config_Opt%AlwaysSetH2O           = .FALSE.
!     Config_Opt%TurnOffHetRates        = .FALSE.
! #endif

! #ifdef ADJOINT
!     !----------------------------------------
!     ! Fields for adoint
!     !---------------------------------------
!     Config_Opt%IS_ADJOINT             = .FALSE.
!     Config_Opt%IS_FD_SPOT             = .FALSE.
!     Config_Opt%IS_FD_GLOBAL           = .FALSE.
!     Config_Opt%IS_FD_SPOT_THIS_PET    = .FALSE.
!     Config_Opt%FD_STEP                = -999
!     Config_Opt%IFD                    = -999
!     Config_Opt%JFD                    = -999
!     Config_Opt%NFD                    = -999
!     Config_Opt%LFD                    = -999
! #endif

      ! !----------------------------------------
      ! ! Fields for LINOZ strat chem
      ! !----------------------------------------
      ! Config_Opt%LINOZ_NLEVELS          = 25
      ! Config_Opt%LINOZ_NLAT             = 18
      ! Config_Opt%LINOZ_NMONTHS          = 12
      ! Config_Opt%LINOZ_NFIELDS          = 7

      ! arrayId = 'Config_Opt%LINOZ_TPARM'
      ! ALLOCATE( Config_Opt%LINOZ_TPARM( Config_Opt%LINOZ_NLEVELS,            &
      !                                  Config_Opt%LINOZ_NLAT,               &
      !                                  Config_Opt%LINOZ_NMONTHS,            &
      !                                  Config_Opt%LINOZ_NFIELDS ), STAT=RC )
      ! CALL CC_CheckVar( arrayId, 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN

      ! Config_Opt%LINOZ_TPARM            = 0.0_fp

! #if defined( ESMF_ )
!     ! Logger handle is set up by Chem_GridCompMod
!     Config_Opt%lgr => NULL()
!     ! Component name is acquired externally - this is a placeholder
!     Config_Opt%compname = 'GC'
! #endif

   END SUBROUTINE Set_Config_Opt
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set_Config_Opt_Advect
!
! !DESCRIPTION: Subroutine SET\_INPUT\_OPT\_ADVECT intializes all CATChem
!  options carried in Input Options derived type object that depend on
!  the number of advected species (Input\_Opt%N_ADVECT).
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Set_Config_Opt_Advect( Config_Opt, RC )
!
! !USES:
!
      USE Error_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!  NOTE: These arrays are all for bpch diagnostics, and will eventually
!  be removed from CATChem.

! !REVISION HISTORY:
!  26 Jan 2018 - M. Sulprizio- Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Initialize
      RC = CC_SUCCESS

      !=======================================================================
      ! Allocate arrays
      !=======================================================================

      ! ALLOCATE( Config_Opt%TINDEX(Config_Opt%Max_BPCH_Diag,Config_Opt%N_ADVECT), &
      !    STAT=RC )
      ! CALL CC_CheckVar( 'Config_Opt%TINDEX', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! Config_Opt%TINDEX = 0

      ! ALLOCATE( Config_Opt%ND51_TRACERS (Config_Opt%N_ADVECT+Config_Opt%Max_BPCH_Diag),&
      !    STAT=RC )
      ! CALL CC_CheckVar( 'Config_Opt%ND51_TRACERS', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! Config_Opt%ND51_TRACERS = 0

      ! ALLOCATE( Config_Opt%ND51b_TRACERS(Config_Opt%N_ADVECT+Config_Opt%Max_BPCH_Diag),&
      !    STAT=RC )
      ! CALL CC_CheckVar( 'Config_Opt%ND51b_TRACERS', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! Config_Opt%ND51b_TRACERS = 0

   END SUBROUTINE Set_Config_Opt_Advect
!EOC
!------------------------------------------------------------------------------
!                  CATChem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleanup_Config_Opt
!
! !DESCRIPTION: Subroutine CLEANUP\_INPUT\_OPT deallocates all
!  allocatable fields of the Input Options object.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Cleanup_Config_Opt( Config_Opt, RC )
!
! !USES:
!
      USE Error_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  02 Nov 2012 - R. Yantosca - Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

      ! Assume success
      RC = CC_SUCCESS

      !======================================================================
      ! Deallocate fields of the Input Options object
      !======================================================================
      IF ( ASSOCIATED( Config_Opt%AdvectSpc_Name ) ) THEN
         DEALLOCATE( Config_Opt%AdvectSpc_Name, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%AdvectSpcName', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%AdvectSpc_Name => NULL()
      ENDIF

      IF ( ASSOCIATED( Config_Opt%SALA_REDGE_um ) ) THEN
         DEALLOCATE( Config_Opt%SALA_REDGE_um, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%SALA_REDGE_um', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%SALA_REDGE_um => NULL()
      ENDIF

      IF ( ASSOCIATED( Config_Opt%SALC_REDGE_um ) ) THEN
         DEALLOCATE( Config_Opt%SALC_REDGE_um, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%SALC_REDGE_um', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%SALC_REDGE_um => NULL()
      ENDIF

      IF ( ASSOCIATED( Config_Opt%NJDAY ) ) THEN
         DEALLOCATE( Config_Opt%NJDAY, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%NJDAY', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%NJDAY => NULL()
      ENDIF

      IF ( ASSOCIATED( Config_Opt%TINDEX ) ) THEN
         DEALLOCATE( Config_Opt%TINDEX, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%TINDEX', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%TINDEX => NULL()
      ENDIF

      IF ( ASSOCIATED( Config_Opt%TCOUNT ) ) THEN
         DEALLOCATE( Config_Opt%TCOUNT, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%TCOUNT', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%TCOUNT => NULL()
      ENDIF

      IF ( ASSOCIATED( Config_Opt%TMAX ) ) THEN
         DEALLOCATE( Config_Opt%TMAX, STAT=RC )
         CALL CC_CheckVar( 'Config_Opt%TMAX', 2, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         Config_Opt%TMAX => NULL()
      ENDIF

      ! IF ( ASSOCIATED( Config_Opt%ND51_TRACERS ) ) THEN
      !    DEALLOCATE( Config_Opt%ND51_TRACERS, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%ND51_TRACERS', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%ND51_TRACERS => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%ND51b_TRACERS ) ) THEN
      !    DEALLOCATE( Config_Opt%ND51b_TRACERS, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%ND51b_TRACERS', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%ND51b_TRACERS => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%FAM_NAME ) ) THEN
      !    DEALLOCATE( Config_Opt%FAM_NAME, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%FAM_NAME', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%FAM_NAME => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%FAM_TYPE ) ) THEN
      !    DEALLOCATE( Config_Opt%FAM_TYPE, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%FAM_TYPE', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%FAM_TYPE => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%LINOZ_TPARM ) ) THEN
      !    DEALLOCATE( Config_Opt%LINOZ_TPARM, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%LINOZ_TPARM', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%LINOZ_TPARM => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%LSPECRADMENU ) ) THEN
      !    DEALLOCATE( Config_Opt%LSPECRADMENU, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%LSPECRADMENU', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%LSPECRADMENU => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%LSKYRAD ) ) THEN
      !    DEALLOCATE( Config_Opt%LSKYRAD, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%LSKYRAD', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%LSKYRAD => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%WVSELECT ) ) THEN
      !    DEALLOCATE( Config_Opt%WVSELECT, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%WVSELECT', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%WVSELECT => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%STRWVSELECT ) ) THEN
      !    DEALLOCATE( Config_Opt%STRWVSELECT, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%STRWVSELECT', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%STRWVSELECT => NULL()
      ! ENDIF

      ! IF ( ASSOCIATED( Config_Opt%ObsPack_SpcName ) ) THEN
      !    DEALLOCATE( Config_Opt%ObsPack_SpcName, STAT=RC )
      !    CALL CC_CheckVar( 'Config_Opt%ObsPack_SpcName', 2, RC )
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !    Config_Opt%ObsPack_SpcName => NULL()
      ! ENDIF

! #ifdef MODEL_GEOS
!       !=======================================================================
!       ! These fields of Config_Opt are only finalized when
!       ! CATChem is coupled to the online NASA/GEOS ESM
!       !=======================================================================
!       IF ( ASSOCIATED( Config_Opt%RxnRconst_IDs ) ) THEN
!          DEALLOCATE( Config_Opt%RxnRconst_IDs, STAT=RC )
!          CALL CC_CheckVar( 'Config_Opt%RxnRconst_IDs', 2, RC )
!          IF ( RC /= CC_SUCCESS ) RETURN
!          Config_Opt%RxnRconst_IDs => NULL()
!       ENDIF

!       IF ( ASSOCIATED( Config_Opt%RxnRates_IDs ) ) THEN
!          DEALLOCATE( Config_Opt%RxnRates_IDs, STAT=RC )
!          CALL CC_CheckVar( 'Config_Opt%RxnRates_IDs', 2, RC )
!          IF ( RC /= CC_SUCCESS ) RETURN
!          Config_Opt%RxnRates_IDs => NULL()
!       ENDIF

!       IF ( ASSOCIATED( Config_Opt%Jval_IDs ) ) THEN
!          DEALLOCATE( Config_Opt%Jval_IDs, STAT=RC )
!          CALL CC_CheckVar( 'Config_Opt%Jval_Ids', 2, RC )
!          IF ( RC /= CC_SUCCESS ) RETURN
!          Config_Opt%Jval_Ids => NULL()
!       ENDIF
! #endif

! #if defined( ESMF_ )
!       If (Associated(Config_Opt%lgr)) Config_Opt%lgr => NULL()
! #endif

   END SUBROUTINE Cleanup_Config_Opt
!EOC
END MODULE Config_Opt_Mod
