!> \file metstate_mod.F90
!! \brief Module for meteorology state variables
!!
!! This module contains subroutines and functions related to the MetStateType instance of CATChem.
!! It includes subroutines for initializing of the MetStateType.
!!
!! \ingroup core_modules
!!!>
MODULE MetState_Mod
   !
   ! USES:
   !
   USE Cmn_Size_Mod, ONLY : NSURFTYPE
   ! USE Dictionary_M, ONLY : dictionary_t
   USE Error_Mod
   USE Precision_Mod
   ! USE Registry_Mod


   IMPLICIT NONE
   PRIVATE
   !
   ! !PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Zero_MetState
   PUBLIC :: Met_Allocate
   !
   ! !PUBLIC DATA MEMBERS:
   !
   !=========================================================================
   ! Derived type for Meteorology State
   !=========================================================================

   !> \brief Derived type for Meteorology State
   !!
   !! \ingroup core_modules
   !!!>
   TYPE, PUBLIC :: MetStateType

      !----------------------------------------------------------------------
      ! Surface fields
      !----------------------------------------------------------------------

      ! Logicals
      !---------
      LOGICAL            :: IsLand         !< Is this a land  grid box?
      LOGICAL            :: IsWater        !< Is this a water grid box?
      LOGICAL            :: IsIce          !< Is this a ice   grid box?
      LOGICAL            :: IsSnow         !< Is this a snow  grid box?

      ! Land Specific Fields
      !---------------------
      REAL(fp)           :: AREA_M2         !< Grid box surface area [m2]
      REAL(fp)           :: CLAYFRAC        !< Fraction of clay [1]
      INTEGER            :: DSOILTYPE       !< Dominant soil type
      REAL(fp)           :: FRLAKE          !< Fraction of lake [1]
      REAL(fp)           :: FRLAND          !< Fraction of land [1]
      REAL(fp)           :: FRLANDIC        !< Fraction of land ice [1]
      REAL(fp)           :: FROCEAN         !< Fraction of ocean [1]
      REAL(fp)           :: FRSEAICE        !< Sfc sea ice fraction
      REAL(fp)           :: FRSNO           !< Sfc snow fraction
      REAL(fp)           :: LAI             !< Leaf area index [m2/m2] (online)
      REAL(fp)           :: RDRAG           !< Drag Partition [1]
      REAL(fp)           :: SANDFRAC        !< Fraction of sand [1]
      REAL(fp)           :: SEAICE00        !< Sea ice coverage 00-10%
      REAL(fp)           :: SEAICE10        !< Sea ice coverage 10-20%
      REAL(fp)           :: SEAICE20        !< Sea ice coverage 20-30%
      REAL(fp)           :: SEAICE30        !< Sea ice coverage 30-40%
      REAL(fp)           :: SEAICE40        !< Sea ice coverage 40-50%
      REAL(fp)           :: SEAICE50        !< Sea ice coverage 50-60%
      REAL(fp)           :: SEAICE60        !< Sea ice coverage 60-70%
      REAL(fp)           :: SEAICE70        !< Sea ice coverage 70-80%
      REAL(fp)           :: SEAICE80        !< Sea ice coverage 80-90%
      REAL(fp)           :: SEAICE90        !< Sea ice coverage 90-100%
      REAL(fp)           :: SNODP           !< Snow depth [m]
      REAL(fp)           :: SNOMAS          !< Snow mass [kg/m2]
      REAL(fp)           :: SSM             !< Sediment Supply Map [1]
      REAL(fp)           :: USTAR_THRESHOLD !< Threshold friction velocity [m/s]


      ! Radiation Related Surface Fields
      !---------------------------------
      REAL(fp)           :: ALBD_VIS       !< Visible surface albedo [1]
      REAL(fp)           :: ALBD_NIR       !< Near-IR surface albedo [1]
      REAL(fp)           :: ALBD_UV        !< UV surface albedo [1]
      REAL(fp)           :: PARDR          !< Direct photsynthetically active radiation [W/m2]
      REAL(fp)           :: PARDF          !< Diffuse photsynthetically active radiation [W/m2]
      REAL(fp)           :: SUNCOS         !< COS(solar zenith angle) at current time
      REAL(fp)           :: SUNCOSmid      !< COS(solar zenith angle) at midpoint of chem timestep
      REAL(fp)           :: SUNCOSsum      !< Sum of COS(SZA) for HEMCO OH diurnal variability
      REAL(fp)           :: SZAFACT        !< Diurnal scale factor for HEMCO OH diurnal variability (computed) [1]
      REAL(fp)           :: SWGDN          !< Incident radiation @ ground [W/m2]

      ! Soil Related Fields
      !--------------------
      REAL(fp)           :: GWETTOP        !< Top soil moisture [1]
      REAL(fp)           :: GWETROOT       !< Root Zone soil moisture [1]

      ! Flux Related Fields
      !--------------------
      REAL(fp)           :: EFLUX          !< Latent heat flux [W/m2]
      REAL(fp)           :: HFLUX          !< Sensible heat flux [W/m2]
      REAL(fp)           :: U10M           !< E/W wind speed @ 10m ht [m/s]
      REAL(fp)           :: USTAR          !< Friction velocity [m/s]
      REAL(fp)           :: V10M           !< N/S wind speed @ 10m ht [m/s]
      REAL(fp)           :: Z0             !< Surface roughness height [m]

      ! Cloud Related Fields
      !---------------------
      REAL(fp)           :: CLDFRC         !< Column cloud fraction [1]
      REAL(fp)           :: CONV_DEPTH     !< Convective cloud depth [m]
      REAL(fp)           :: FLASH_DENS     !< Lightning flash density [#/km2/s]






      REAL(fp)           :: PBLH           !< PBL height [m]
      REAL(fp)           :: PBL_TOP_hPa    !< PBL top [hPa]
      REAL(fp)           :: PBL_TOP_L      !< PBL top [level]
      REAL(fp)           :: PBL_TOP_m      !< PBL top [m]
      REAL(fp)           :: PBL_THICK      !< PBL thickness [hPa]
      REAL(fp)           :: PHIS           !< Surface geopotential height [m2/s2]
      REAL(fp)           :: PRECANV        !< Anvil previp @ ground [kg/m2/s] -> [mm/day]
      REAL(fp)           :: PRECCON        !< Conv  precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp)           :: PRECLSC        !< Large-scale precip @ ground kg/m2/s] -> [mm/day]
      REAL(fp)           :: PRECTOT        !< Total precip @ ground [kg/m2/s] -> [mm/day]
      REAL(fp)           :: PS_WET         !< Wet surface pressure at start of timestep [hPa]
      REAL(fp)           :: PS_DRY         !< Dry surface pressure at start of timestep [hPa]
      REAL(fp)           :: QV2M           !< Specific Humidity at 2m [kg/kg]
      REAL(fp)           :: T2M
      REAL(fp)           :: SST            !< Sea surface temperature [K]

      REAL(fp)           :: SLP            !< Sea level pressure [hPa]

      REAL(fp)           :: PS             !< Surface Pressure [hPa]


      REAL(fp)           :: TO3            !< Total overhead O3 column [DU]
      REAL(fp)           :: TROPP          !< Tropopause pressure [hPa]
      INTEGER            :: TropLev        !< Tropopause level [1]
      REAL(fp)           :: TropHt         !< Tropopause height [km]
      REAL(fp)           :: TS             !< Surface temperature [K]
      REAL(fp)           :: TSKIN          !< Surface skin temperature [K]

      REAL(fp)           :: CNV_FRC        !< Convective fraction [1]

      !----------------------------------------------------------------------
      ! Column Fields
      !----------------------------------------------------------------------
      REAL(fp), POINTER :: CLDF          (:) !< 3-D cloud fraction [1]
      REAL(fp), POINTER :: CMFMC         (:) !< Cloud mass flux [kg/m2/s]
      REAL(fp), POINTER :: DQRCU         (:) !< Conv precip production rate [kg/kg/s] (assume per dry air)
      REAL(fp), POINTER :: DQRLSAN       (:) !< LS precip prod rate [kg/kg/s] (assume per dry air)
      REAL(fp), POINTER :: DTRAIN        (:) !< Detrainment flux [kg/m2/s]
      REAL(fp), POINTER :: F_OF_PBL      (:) !< Fraction of box within PBL [1]
      REAL(fp), POINTER :: F_UNDER_PBLTOP(:) !< Fraction of box under PBL top
      REAL(fp), POINTER :: OMEGA         (:) !< Updraft velocity [Pa/s]
      REAL(fp), POINTER :: OPTD          (:) !< Visible optical depth [1]
      REAL(fp), POINTER :: PEDGE         (:) !< Wet air press @ level edges [hPa]
      REAL(fp), POINTER :: PFICU         (:) !< Dwn flux ice prec:conv [kg/m2/s]
      REAL(fp), POINTER :: PFILSAN       (:) !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), POINTER :: PFLCU         (:) !< Dwn flux liq prec:conv [kg/m2/s]
      REAL(fp), POINTER :: PFLLSAN       (:) !< Dwn flux ice prec:LS+anv [kg/m2/s]
      REAL(fp), POINTER :: QI            (:) !< Mass fraction of cloud ice water [kg/kg dry air]
      REAL(fp), POINTER :: QL            (:) !< Mass fraction of cloud liquid water [kg/kg dry air]
      REAL(fp), POINTER :: RH            (:) !< Relative humidity [%]
      REAL(fp), POINTER :: SPHU          (:) !< Specific humidity [g H2O/kg tot air]
      REAL(fp), POINTER :: SPHU1         (:) !< Specific humidity at start of timestep [g/kg]
      REAL(fp), POINTER :: SPHU2         (:) !< Specific humidity at end of timestep [g/kg]
      REAL(fp), POINTER :: T             (:) !< Temperature [K]
      REAL(fp), POINTER :: TAUCLI        (:) !< Opt depth of ice clouds [1]
      REAL(fp), POINTER :: TAUCLW        (:) !< Opt depth of H2O clouds [1]
      REAL(fp), POINTER :: TMPU1         (:) !< Temperature at start of timestep [K]
      REAL(fp), POINTER :: TMPU2         (:) !< Temperature at end of timestep [K]
      REAL(fp), POINTER :: U             (:) !< E/W component of wind [m s-1]
      REAL(fp), POINTER :: UPDVVEL       (:) !< Updraft vertical velocity [hPa/s]
      REAL(fp), POINTER :: V             (:) !< N/S component of wind [m s-1]

      !----------------------------------------------------------------------
      ! Air quantities assigned in AIRQNT
      !----------------------------------------------------------------------
      ! Note on pressures: PMID is calculated from PEDGE,
      ! and dry air pressures assume constant RH and T across grid box
      REAL(fp), POINTER :: PEDGE_DRY     (:) !< Dry air partial pressure @ level edges [hPa]
      REAL(fp), POINTER :: PMID          (:) !< Average wet air pressure [hPa] defined as arithmetic average of edge pressures
      REAL(fp), POINTER :: PMID_DRY      (:) !< Dry air partial pressure [hPa] defined as arithmetic avg of edge pressures
      REAL(fp), POINTER :: THETA         (:) !< Potential temperature [K]
      REAL(fp), POINTER :: TV            (:) !< Virtual temperature [K]
      REAL(fp), POINTER :: MAIRDEN       (:) !< Moist air density [kg/m3]
      REAL(fp), POINTER :: AIRDEN        (:) !< Dry air density [kg/m3]
      REAL(fp), POINTER :: AIRNUMDEN     (:) !< Dry air density [molec/cm3]
      REAL(fp), POINTER :: AVGW          (:) !< Water vapor volume mixing ratio [vol H2O/vol dry air]
      REAL(fp), POINTER :: BXHEIGHT      (:) !< Grid box height [m] (dry air)
      REAL(fp), POINTER :: DELP          (:) !< Delta-P (wet) across box [hPa]
      REAL(fp), POINTER :: DELP_DRY      (:) !< Delta-P (dry) across box [hPa]
      REAL(fp), POINTER :: DAIRMASS      (:) !< Dry air mass [kg] in grid box
      REAL(fp), POINTER :: AIRVOL        (:) !< Grid box volume [m3] (dry air)
      REAL(fp), POINTER :: DP_DRY_PREV   (:) !< Previous MetState%DELP_DRY
      REAL(fp), POINTER :: SPHU_PREV     (:) !< Previous MetState%SPHU


      !  !----------------------------------------------------------------------
      !  ! Fields read in from a previous GC run
      !  !----------------------------------------------------------------------
      !  REAL(fp), POINTER :: DynHeating    (:) ! Dynamical heating (K/day)

      !----------------------------------------------------------------------
      ! Offline land type, leaf area index, and chlorophyll fields
      !----------------------------------------------------------------------
      INTEGER,  POINTER :: IREG           !< # of landtypes in box (I,J)
      INTEGER,  POINTER :: ILAND         (:) !< Land type at (I,J); 1..IREG(I,J)
      INTEGER,  POINTER :: IUSE          (:) !< Fraction (per mil) of box (I,J) occupied by each land
      REAL(fp), POINTER :: MODISLAI       !< Daily LAI computed from monthly offline MODIS [m2/m2]
      REAL(fp), POINTER :: XLAI          (:) !< MODIS LAI per land type, for this month
      REAL(fp), POINTER :: LandTypeFrac  (:) !< Olson frac per type (I,J,type)
      REAL(fp), POINTER :: XLAI_NATIVE   (:) !< avg LAI per type (I,J,type)
      REAL(fp), POINTER :: XLAI2         (:) !< MODIS LAI per land type,
      !  for next month

      !----------------------------------------------------------------------
      ! Fields for querying in which vertical regime a grid box is in
      ! or if a grid box is near local noon solar time
      !----------------------------------------------------------------------
      LOGICAL,  POINTER :: InChemGrid    (:) ! Are we in the chemistry grid?
      LOGICAL,  POINTER :: InPbl         (:) ! Are we in the PBL?
      LOGICAL,  POINTER :: InStratMeso   (:) ! Are we in the stratosphere
      !            or mesosphere?
      LOGICAL,  POINTER :: InStratosphere(:) ! Are we in the stratosphere?
      LOGICAL,  POINTER :: InTroposphere (:) ! Are we in the troposphere?
      REAL(fp), POINTER :: LocalSolarTime ! Local solar time
      LOGICAL,  POINTER :: IsLocalNoon    ! Is it local noon (between 11
      !  and 13 local solar time?

      !----------------------------------------------------------------------
      ! Fields for boundary layer mixing
      !----------------------------------------------------------------------
      INTEGER,  POINTER :: IMIX           ! Integer and fractional level
      REAL(fp), POINTER :: FPBL           !  where PBL top occurs
      INTEGER           :: PBL_MAX_L      ! Max level where PBL top occurs

      !----------------------------------------------------------------------
      ! Registry of variables contained within MetState
      !----------------------------------------------------------------------
      CHARACTER(LEN=3)             :: State     = 'MET'    ! Name of this state

   END TYPE MetStateType

CONTAINS

   !---------------------------------------------------------------------------
   ! PUBLIC MEMBER FUNCTIONS
   !---------------------------------------------------------------------------
   !
   SUBROUTINE Zero_MetState( MetState, RC )
      !
      ! !INPUT/OUTPUT PARAMETERS:
      !
      TYPE(MetStateType), INTENT(INOUT) :: MetState
      !
      ! !OUTPUT PARAMETERS:
      !
      INTEGER,        INTENT(OUT)   :: RC
      !
      ! !REVISION HISTORY:
      !  21 Sep 2020 - R. Yantosca - Initial version
      !  See the subsequent Git history with the gitk browser!
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! Initialize
      RC = CC_SUCCESS

   END SUBROUTINE Zero_MetState

   !>
   !! \brief Allocate the MetState object
   !!
   !! \ingroup core_modules
   !!
   !! \param GridState   CATCHem grid state
   !! \param MetState    CATCHem met state
   !! \param RC          Error return code
   !!!>
   SUBROUTINE Met_Allocate( GridState, MetState, RC)
      ! USES
      USE GridState_Mod, Only : GridStateType

      IMPLICIT NONE

      ! Arguments
      TYPE(GridStateType), INTENT(IN)  :: GridState !< Grid state
      TYPE(MetStateType), INTENT(INOUT) :: MetState !< Meteorological state
      INTEGER,            INTENT(OUT)   :: RC       !< Return code

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Met_Allocate (in core/metstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values


      !--------------------------------------------------
      ! Initialize fields
      !--------------------------------------------------

      ! Visible Surface Albedo
      !-----------------------
      MetState%ALBD_VIS = ZERO
      MetState%ALBD_NIR = ZERO
      MetState%ALBD_UV = ZERO
      MetState%AREA_M2 = ZERO
      MetState%CLDFRC = ZERO
      MetState%CONV_DEPTH = ZERO
      MetState%EFLUX = ZERO
      MetState%FRLAKE = ZERO
      MetState%FRLAND = ZERO
      MetState%FRLANDIC = ZERO
      MetState%FROCEAN = ZERO
      MetState%FRSEAICE = ZERO
      MetState%FRSNO = ZERO
      MetState%GWETROOT = ZERO
      MetState%GWETTOP = ZERO
      MetState%HFLUX = ZERO
      MetState%IsLand = .false.
      MetState%IsWater = .false.
      MetState%IsIce = .false.
      MetState%IsSnow = .false.
      MetState%LAI = ZERO
      MetState%PARDR = ZERO
      MetState%PARDF = ZERO
      MetState%PBLH = ZERO
      MetState%PBL_TOP_hPa = ZERO
      MetState%PBL_TOP_L = ZERO
      MetState%PS = ZERO
      MetState%QV2M = ZERO
      MetState%T2M = ZERO
      MetState%TSKIN = ZERO
      MetState%U10M = ZERO
      MetState%V10M = ZERO
      MetState%z0 = ZERO
      MetState%USTAR_THRESHOLD = ZERO
      MetState%RDRAG = ZERO
      MetState%SSM = ZERO
      MetState%CLAYFRAC = ZERO
      MetSTate%SANDFRAC = ZERO
      MetState%SST = ZERO

      ! Allocate Column Fields
      !-----------------------

   end subroutine Met_Allocate

END MODULE MetState_Mod
