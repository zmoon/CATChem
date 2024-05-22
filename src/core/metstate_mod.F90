!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: MetState_mod.F90
!
! !DESCRIPTION: Module STATE\_MET\_MOD contains the derived type
!  used to define the Meteorology State object for CATChem.
!\\
!\\
!  This module also contains the routines that allocate and deallocate memory
!  to the Meteorology State object.  The Meteorology State object is not
!  defined in this module.  It must be be declared as variable in the top-level
!  driver routine, and then passed to lower-level routines as an argument.
!\\
!\\
! !INTERFACE:
!
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
   TYPE, PUBLIC :: MetStateType

      !----------------------------------------------------------------------
      ! Surface fields
      !----------------------------------------------------------------------
      REAL(fp), POINTER :: ALBD_VIS      (:,:) ! Visible surface albedo [1]
      REAL(fp), POINTER :: ALBD_NIR      (:,:) ! Near-IR surface albedo [1]
      REAL(fp), POINTER :: ALBD_UV       (:,:) ! UV surface albedo [1]
      REAL(fp), POINTER :: AREA_M2       (:,:) ! Grid box surface area [m2]
      REAL(fp), POINTER :: CLDFRC        (:,:) ! Column cloud fraction [1]
      REAL(fp), POINTER :: CONV_DEPTH    (:,:) ! Convective cloud depth [m]
      REAL(fp), POINTER :: EFLUX         (:,:) ! Latent heat flux [W/m2]
      REAL(fp), POINTER :: FLASH_DENS    (:,:) ! Lightning flash density [#/km2/s]
      REAL(fp), POINTER :: FRLAKE        (:,:) ! Fraction of lake [1]
      REAL(fp), POINTER :: FRLAND        (:,:) ! Fraction of land [1]
      REAL(fp), POINTER :: FRLANDIC      (:,:) ! Fraction of land ice [1]
      REAL(fp), POINTER :: FROCEAN       (:,:) ! Fraction of ocean [1]
      REAL(fp), POINTER :: FRSEAICE      (:,:) ! Sfc sea ice fraction
      REAL(fp), POINTER :: FRSNO         (:,:) ! Sfc snow fraction
      REAL(fp), POINTER :: GWETTOP       (:,:) ! Top soil moisture [1]
      REAL(fp), POINTER :: GWETROOT      (:,:) ! Root Zone soil moisture [1]
      REAL(fp), POINTER :: HFLUX         (:,:) ! Sensible heat flux [W/m2]
      LOGICAL,  POINTER :: IsLand        (:,:) ! Is this a land  grid box?
      LOGICAL,  POINTER :: IsWater       (:,:) ! Is this a water grid box?
      LOGICAL,  POINTER :: IsIce         (:,:) ! Is this a ice   grid box?
      LOGICAL,  POINTER :: IsSnow        (:,:) ! Is this a snow  grid box?
      REAL(fp), POINTER :: LAI           (:,:) ! Leaf area index [m2/m2]
      !  (online)
      REAL(fp), POINTER :: PARDR         (:,:) ! Direct photsynthetically
      !  active radiation [W/m2]
      REAL(fp), POINTER :: PARDF         (:,:) ! Diffuse photsynthetically
      !  active radiation [W/m2]
      REAL(fp), POINTER :: PBLH          (:,:) ! PBL height [m]
      REAL(fp), POINTER :: PBL_TOP_hPa   (:,:) ! PBL top [hPa]
      REAL(fp), POINTER :: PBL_TOP_L     (:,:) ! PBL top [level]
      REAL(fp), POINTER :: PBL_TOP_m     (:,:) ! PBL top [m]
      REAL(fp), POINTER :: PBL_THICK     (:,:) ! PBL thickness [hPa]
      REAL(fp), POINTER :: PHIS          (:,:) ! Surface geopotential height
      !  [m2/s2]
      REAL(fp), POINTER :: PRECANV       (:,:) ! Anvil previp @ ground
      !  [kg/m2/s] -> [mm/day]
      REAL(fp), POINTER :: PRECCON       (:,:) ! Conv  precip @ ground
      !  [kg/m2/s] -> [mm/day]
      REAL(fp), POINTER :: PRECLSC       (:,:) ! Large-scale precip @ ground
      !  [kg/m2/s] -> [mm/day]
      REAL(fp), POINTER :: PRECTOT       (:,:) ! Total precip @ ground
      !  [kg/m2/s] -> [mm/day]
      REAL(fp), POINTER :: PS_WET        (:,:) ! Wet surface pressure at
      !  start of timestep [hPa]
      REAL(fp), POINTER :: PS_DRY        (:,:) ! Dry surface pressure at
      !  start of timestep [hPa]
      REAL(fp), POINTER :: QV2M          (:,:) ! Specific Humidity at 2m [kg/kg]
      REAL(fp), POINTER :: T2M           (:,:)
      REAL(fp), POINTER :: SEAICE00      (:,:) ! Sea ice coverage 00-10%
      REAL(fp), POINTER :: SEAICE10      (:,:) ! Sea ice coverage 10-20%
      REAL(fp), POINTER :: SEAICE20      (:,:) ! Sea ice coverage 20-30%
      REAL(fp), POINTER :: SEAICE30      (:,:) ! Sea ice coverage 30-40%
      REAL(fp), POINTER :: SEAICE40      (:,:) ! Sea ice coverage 40-50%
      REAL(fp), POINTER :: SEAICE50      (:,:) ! Sea ice coverage 50-60%
      REAL(fp), POINTER :: SEAICE60      (:,:) ! Sea ice coverage 60-70%
      REAL(fp), POINTER :: SEAICE70      (:,:) ! Sea ice coverage 70-80%
      REAL(fp), POINTER :: SEAICE80      (:,:) ! Sea ice coverage 80-90%
      REAL(fp), POINTER :: SEAICE90      (:,:) ! Sea ice coverage 90-100%
      REAL(fp), POINTER :: SLP           (:,:) ! Sea level pressure [hPa]
      REAL(fp), POINTER :: SNODP         (:,:) ! Snow depth [m]
      REAL(fp), POINTER :: PS            (:,:) ! Surface Pressure [hPa]
      REAL(fp), POINTER :: SNOMAS        (:,:) ! Snow mass [kg/m2]
      REAL(fp), POINTER :: SUNCOS        (:,:) ! COS(solar zenith angle) at
      !   current time
      REAL(fp), POINTER :: SUNCOSmid     (:,:) ! COS(solar zenith angle) at
      !  midpoint of chem timestep
      REAL(fp), POINTER :: SUNCOSsum     (:,:) ! Sum of COS(SZA) for HEMCO OH
      !  diurnal variability
      REAL(fp), POINTER :: SZAFACT       (:,:) ! Diurnal scale factor for HEMCO OH
      !  diurnal variability (computed) [1]
      REAL(fp), POINTER :: SWGDN         (:,:) ! Incident radiation @ ground
      !  [W/m2]
      REAL(fp), POINTER :: TO3           (:,:) ! Total overhead O3 column [DU]
      REAL(fp), POINTER :: TROPP         (:,:) ! Tropopause pressure [hPa]
      INTEGER,  POINTER :: TropLev       (:,:) ! Tropopause level [1]
      REAL(fp), POINTER :: TropHt        (:,:) ! Tropopause height [km]
      REAL(fp), POINTER :: TS            (:,:) ! Surface temperature [K]
      REAL(fp), POINTER :: TSKIN         (:,:) ! Surface skin temperature [K]
      REAL(fp), POINTER :: U10M          (:,:) ! E/W wind speed @ 10m ht [m/s]
      REAL(fp), POINTER :: USTAR         (:,:) ! Friction velocity [m/s]
      REAL(fp), POINTER :: V10M          (:,:) ! N/S wind speed @ 10m ht [m/s]
      REAL(fp), POINTER :: Z0            (:,:) ! Surface roughness height [m]
      REAL(fp), POINTER :: CNV_FRC       (:,:) ! Convective fraction [1]

      !----------------------------------------------------------------------
      ! 3-D Fields
      !----------------------------------------------------------------------
      REAL(fp), POINTER :: CLDF          (:,:,:) ! 3-D cloud fraction [1]
      REAL(fp), POINTER :: CMFMC         (:,:,:) ! Cloud mass flux [kg/m2/s]
      REAL(fp), POINTER :: DQRCU         (:,:,:) ! Conv precip production rate
      !  [kg/kg/s] (assume per
      !  dry air)
      REAL(fp), POINTER :: DQRLSAN       (:,:,:) ! LS precip prod rate [kg/kg/s]
      !  (assume per dry air)
      REAL(fp), POINTER :: DTRAIN        (:,:,:) ! Detrainment flux [kg/m2/s]
      REAL(fp), POINTER :: F_OF_PBL      (:,:,:) ! Fraction of box within PBL [1]
      REAL(fp), POINTER :: F_UNDER_PBLTOP(:,:,:) ! Fraction of box under PBL top
      REAL(fp), POINTER :: OMEGA         (:,:,:) ! Updraft velocity [Pa/s]
      REAL(fp), POINTER :: OPTD          (:,:,:) ! Visible optical depth [1]
      REAL(fp), POINTER :: PEDGE         (:,:,:) ! Wet air press @ level
      !  edges [hPa]
      REAL(fp), POINTER :: PFICU         (:,:,:) ! Dwn flux ice prec:conv
      !  [kg/m2/s]
      REAL(fp), POINTER :: PFILSAN       (:,:,:) ! Dwn flux ice prec:LS+anv
      !  [kg/m2/s]
      REAL(fp), POINTER :: PFLCU         (:,:,:) ! Dwn flux liq prec:conv
      !  [kg/m2/s]
      REAL(fp), POINTER :: PFLLSAN       (:,:,:) ! Dwn flux ice prec:LS+anv
      !  [kg/m2/s]
      REAL(fp), POINTER :: QI            (:,:,:) ! Mass fraction of cloud ice water
      !  [kg/kg dry air]
      REAL(fp), POINTER :: QL            (:,:,:) ! Mass fraction of cloud liquid water
      !  [kg/kg dry air]
      REAL(fp), POINTER :: RH            (:,:,:) ! Relative humidity [%]
      REAL(fp), POINTER :: SPHU          (:,:,:) ! Specific humidity
      !  [g H2O/kg tot air]
      REAL(fp), POINTER :: SPHU1         (:,:,:) ! Specific humidity at start
      !  of timestep [g/kg]
      REAL(fp), POINTER :: SPHU2         (:,:,:) ! Specific humidity at end
      !  of timestep [g/kg]
      REAL(fp), POINTER :: T             (:,:,:) ! Temperature [K]
      REAL(fp), POINTER :: TAUCLI        (:,:,:) ! Opt depth of ice clouds [1]
      REAL(fp), POINTER :: TAUCLW        (:,:,:) ! Opt depth of H2O clouds [1]
      REAL(fp), POINTER :: TMPU1         (:,:,:) ! Temperature at start of
      !  timestep [K]
      REAL(fp), POINTER :: TMPU2         (:,:,:) ! Temperature at end of
      !  timestep [K]
      REAL(fp), POINTER :: U             (:,:,:) ! E/W component of wind [m s-1]
      REAL(fp), POINTER :: UPDVVEL       (:,:,:) ! Updraft vertical velocity
      !  [hPa/s]
      REAL(fp), POINTER :: V             (:,:,:) ! N/S component of wind [m s-1]

      !----------------------------------------------------------------------
      ! Air quantities assigned in AIRQNT
      !----------------------------------------------------------------------
      ! Note on pressures: PMID is calculated from PEDGE,
      ! and dry air pressures assume constant RH and T across grid box
      REAL(fp), POINTER :: PEDGE_DRY     (:,:,:) ! Dry air partial pressure
      !  @ level edges [hPa]
      REAL(fp), POINTER :: PMID          (:,:,:) ! Average wet air pressure [hPa]
      !  defined as arithmetic
      !  average of edge pressures
      REAL(fp), POINTER :: PMID_DRY      (:,:,:) ! Dry air partial pressure [hPa]
      !  defined as arithmetic avg
      !  of edge pressures
      REAL(fp), POINTER :: THETA         (:,:,:) ! Potential temperature [K]
      REAL(fp), POINTER :: TV            (:,:,:) ! Virtual temperature [K]
      REAL(fp), POINTER :: MAIRDEN       (:,:,:) ! Moist air density [kg/m3]
      REAL(fp), POINTER :: AIRDEN        (:,:,:) ! Dry air density [kg/m3]
      REAL(fp), POINTER :: AIRNUMDEN     (:,:,:) ! Dry air density [molec/cm3]
      REAL(fp), POINTER :: AVGW          (:,:,:) ! Water vapor volume mixing
      !  ratio [vol H2O/vol dry air]
      REAL(fp), POINTER :: BXHEIGHT      (:,:,:) ! Grid box height [m] (dry air)
      REAL(fp), POINTER :: DELP          (:,:,:) ! Delta-P (wet) across box [hPa]
      REAL(fp), POINTER :: DELP_DRY      (:,:,:) ! Delta-P (dry) across box [hPa]
      REAL(fp), POINTER :: AD            (:,:,:) ! Dry air mass [kg] in grid box
      REAL(fp), POINTER :: AIRVOL        (:,:,:) ! Grid box volume [m3] (dry air)
      REAL(fp), POINTER :: DP_DRY_PREV   (:,:,:) ! Previous MetState%DELP_DRY
      REAL(fp), POINTER :: SPHU_PREV     (:,:,:) ! Previous MetState%SPHU

      !  !----------------------------------------------------------------------
      !  ! Fields read in from a previous GC run
      !  !----------------------------------------------------------------------
      !  REAL(fp), POINTER :: DynHeating    (:,:,:) ! Dynamical heating (K/day)

      !----------------------------------------------------------------------
      ! Offline land type, leaf area index, and chlorophyll fields
      !----------------------------------------------------------------------
      INTEGER,  POINTER :: IREG          (:,:) ! # of landtypes in box (I,J)
      INTEGER,  POINTER :: ILAND         (:,:,:) ! Land type at (I,J);
      !  1..IREG(I,J)
      INTEGER,  POINTER :: IUSE          (:,:,:) ! Fraction (per mil) of box
      !  (I,J) occupied by each land
      REAL(fp), POINTER :: MODISLAI      (:,:) ! Daily LAI computed from
      !  monthly offline MODIS [m2/m2]
      REAL(fp), POINTER :: XLAI          (:,:,:) ! MODIS LAI per land type,
      !  for this month
      REAL(fp), POINTER :: LandTypeFrac  (:,:,:) ! Olson frac per type (I,J,type)
      REAL(fp), POINTER :: XLAI_NATIVE   (:,:,:) ! avg LAI per type (I,J,type)
      REAL(fp), POINTER :: XLAI2         (:,:,:) ! MODIS LAI per land type,
      !  for next month

      !----------------------------------------------------------------------
      ! Fields for querying in which vertical regime a grid box is in
      ! or if a grid box is near local noon solar time
      !----------------------------------------------------------------------
      LOGICAL,  POINTER :: InChemGrid    (:,:,:) ! Are we in the chemistry grid?
      LOGICAL,  POINTER :: InPbl         (:,:,:) ! Are we in the PBL?
      LOGICAL,  POINTER :: InStratMeso   (:,:,:) ! Are we in the stratosphere
      !            or mesosphere?
      LOGICAL,  POINTER :: InStratosphere(:,:,:) ! Are we in the stratosphere?
      LOGICAL,  POINTER :: InTroposphere (:,:,:) ! Are we in the troposphere?
      REAL(fp), POINTER :: LocalSolarTime(:,:) ! Local solar time
      LOGICAL,  POINTER :: IsLocalNoon   (:,:) ! Is it local noon (between 11
      !  and 13 local solar time?

      !----------------------------------------------------------------------
      ! Fields for boundary layer mixing
      !----------------------------------------------------------------------
      INTEGER,  POINTER :: IMIX          (:,:) ! Integer and fractional level
      REAL(fp), POINTER :: FPBL          (:,:) !  where PBL top occurs
      INTEGER           :: PBL_MAX_L             ! Max level where PBL top occurs

      !----------------------------------------------------------------------
      ! Registry of variables contained within MetState
      !----------------------------------------------------------------------
      CHARACTER(LEN=3)             :: State     = 'MET'    ! Name of this state
      ! TYPE(MetaRegItem), POINTER   :: Registry  => NULL()  ! Registry object
      ! TYPE(dictionary_t)           :: RegDict              ! Reg. lookup table

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

      !=======================================================================
      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      !=======================================================================
      MetState%ALBD_VIS       => NULL() ! Visible albedo
      MetState%ALBD_NIR       => NULL() ! Near-IR albedo
      MetState%ALBD_UV        => NULL() ! UV albedo
      MetState%AREA_M2        => NULL() ! Area of grid box
      MetState%CLDFRC         => NULL() ! Cloud fraction
      ! MetState%CLDTOPS        => NULL() ! Cloud top level
      MetState%CONV_DEPTH     => NULL() ! Convective depth
      MetState%EFLUX          => NULL() ! Surface flux
      MetState%FLASH_DENS     => NULL() ! Flash density
      MetState%FRLAKE         => NULL() ! Fraction Lake of grid box
      MetState%FRLAND         => NULL() ! Fraction Land of grid box
      MetState%FRLANDIC       => NULL() ! Fraction LandIce of grid box
      MetState%FROCEAN        => NULL() ! Fraction Ocean of grid box
      MetState%FRSEAICE       => NULL() ! Fraction SeaIce of grid box
      MetState%FRSNO          => NULL() ! Fraction Snow of grid box
      MetState%GWETROOT       => NULL() ! Soil Moisture Root Zone
      MetState%GWETTOP        => NULL() ! Soil Moisture Top Level
      MetState%HFLUX          => NULL() ! Surface flux
      MetState%IsLand         => NULL() ! Is this grid box land?
      MetState%IsWater        => NULL() ! Is this grid box water?
      MetState%IsIce          => NULL() ! Is this grid box ice?
      MetState%IsSnow         => NULL() ! Is this grid box snow?
      MetState%LAI            => NULL() ! Leaf Area Index
      MetState%PARDR          => NULL() ! Direct  downward PAR
      MetState%PARDF          => NULL() ! Diffuse downward PAR
      MetState%PBLH           => NULL() ! PBL height
      MetState%PBL_TOP_hPa    => NULL() ! PBL top [hPa]
      MetState%PBL_TOP_L      => NULL() ! PBL top [levels]
      MetState%PBL_TOP_m      => NULL() ! PBL top [m]
      MetState%PBL_THICK      => NULL() ! PBL thickness
      MetState%PHIS           => NULL() ! Surface geopotential
      MetState%PRECANV        => NULL() ! Canopy evaporation
      MetState%PRECCON        => NULL() ! Convective precipitation
      MetState%PRECLSC        => NULL() ! Large-scale precipitation
      MetState%PRECTOT        => NULL() ! Total precipitation
      ! MetState%PS1_WET        => NULL()
      ! MetState%PS2_WET        => NULL()
      ! MetState%PSC2_WET       => NULL()
      ! MetState%PS1_DRY        => NULL()
      ! MetState%PS2_DRY        => NULL()
      ! MetState%PSC2_DRY       => NULL()
      MetState%QV2M           => NULL() ! 2-m specific humidity
      MetState%SEAICE00       => NULL() ! Sea ice fraction
      MetState%SEAICE10       => NULL() ! Sea ice fraction
      MetState%SEAICE20       => NULL() ! Sea ice fraction
      MetState%SEAICE30       => NULL() ! Sea ice fraction
      MetState%SEAICE40       => NULL() ! Sea ice fraction
      MetState%SEAICE50       => NULL() ! Sea ice fraction
      MetState%SEAICE60       => NULL() ! Sea ice fraction
      MetState%SEAICE70       => NULL() ! Sea ice fraction
      MetState%SEAICE80       => NULL() ! Sea ice fraction
      MetState%SEAICE90       => NULL() ! Sea ice fraction
      MetState%SLP            => NULL() ! Sea level pressure
      MetState%SNODP          => NULL() ! Snow depth
      MetState%SNOMAS         => NULL() ! Snow mass
      MetState%SUNCOS         => NULL() ! Cosine of solar zenith angle
      MetState%SUNCOSmid      => NULL() ! Cosine of solar zenith angle
      MetState%SUNCOSsum      => NULL() ! Cosine of solar zenith angle
      MetState%SZAFACT        => NULL() ! Cosine of solar zenith angle
      MetState%SWGDN          => NULL() ! Surface downward SW radiation
      MetState%TO3            => NULL() ! Total ozone
      MetState%TROPP          => NULL() ! Topopause level pressure
      MetState%TropLev        => NULL() ! Topopause level
      MetState%TropHt         => NULL() ! Top level height
      MetState%TS             => NULL() ! Surface temperature
      MetState%TSKIN          => NULL() ! Skin temperature
      MetState%U10M           => NULL() ! 10-m zonal wind
      MetState%USTAR          => NULL() ! Friction velocity
      MetState%V10M           => NULL() ! 10-m meridional wind
      MetState%Z0             => NULL() ! Surface roughness
      MetState%CNV_FRC        => NULL() ! Convective fraction
      MetState%CLDF           => NULL() ! Cloud fraction
      MetState%CMFMC          => NULL() ! Convective mass flux
      MetState%DQRCU          => NULL()
      MetState%DQRLSAN        => NULL()
      MetState%DTRAIN         => NULL()
      MetState%F_OF_PBL       => NULL()
      MetState%F_UNDER_PBLTOP => NULL()
      MetState%OMEGA          => NULL()
      MetState%OPTD           => NULL()
      MetState%PEDGE          => NULL()
      MetState%PFICU          => NULL()
      MetState%PFILSAN        => NULL()
      MetState%PFLCU          => NULL()
      MetState%PFLLSAN        => NULL()
      MetState%QI             => NULL()
      MetState%QL             => NULL()
      MetState%RH             => NULL()
      MetState%SPHU           => NULL()
      MetState%SPHU1          => NULL()
      MetState%SPHU2          => NULL()
      MetState%T              => NULL()
      MetState%TAUCLI         => NULL()
      MetState%TAUCLW         => NULL()
      MetState%TMPU1          => NULL()
      MetState%TMPU2          => NULL()
      MetState%U              => NULL()
      MetState%UPDVVEL        => NULL()
      MetState%V              => NULL()
      MetState%PEDGE_DRY      => NULL()
      MetState%PMID           => NULL()
      MetState%PMID_DRY       => NULL()
      MetState%THETA          => NULL()
      MetState%TV             => NULL()
      MetState%MAIRDEN        => NULL()
      MetState%AIRDEN         => NULL()
      MetState%AIRNUMDEN      => NULL()
      MetState%AVGW           => NULL()
      MetState%BXHEIGHT       => NULL()
      MetState%DELP           => NULL()
      MetState%DELP_DRY       => NULL()
      MetState%AD             => NULL()
      MetState%PS             => NULL()
      MetState%AIRVOL         => NULL()
      MetState%DP_DRY_PREV    => NULL()
      MetState%SPHU_PREV      => NULL()
      MetState%IREG           => NULL()
      MetState%ILAND          => NULL()
      MetState%IUSE           => NULL()
      MetState%MODISLAI       => NULL()
      MetState%XLAI           => NULL()
      MetState%LandTypeFrac   => NULL()
      MetState%XLAI_NATIVE    => NULL()
      MetState%XLAI2          => NULL()
      MetState%InChemGrid     => NULL()
      MetState%InPbl          => NULL()
      MetState%InStratMeso    => NULL()
      MetState%InStratosphere => NULL()
      MetState%InTroposphere  => NULL()
      MetState%LocalSolarTime => NULL()
      MetState%IsLocalNoon    => NULL()
      MetState%IMIX           => NULL()
      MetState%FPBL           => NULL()
      MetState%PBL_MAX_L      = 0

   END SUBROUTINE Zero_MetState

   SUBROUTINE Met_Allocate( GridState, MetState, RC)
      ! USES
      USE GridState_Mod, Only : GridStateType

      IMPLICIT NONE

      ! Arguments
      TYPE(GridStateType), INTENT(IN)  :: GridState
      TYPE(MetStateType),  INTENT(OUT) :: MetState
      INTEGER,             INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Met_Allocate (in core/metstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      MetState%ALBD_VIS       => NULL() ! Visible albedo
      MetState%ALBD_NIR       => NULL() ! Near-IR albedo
      MetState%ALBD_UV        => NULL() ! UV albedo
      MetState%AREA_M2        => NULL() ! Area of grid box
      ! MetState%CLDFRA         => NULL() ! Cloud fraction
      MetState%CONV_DEPTH     => NULL() ! Convective depth
      MetState%EFLUX          => NULL() ! Latent heat flux
      MetState%FLASH_DENS     => NULL() ! Flash density
      MetState%FRLAKE         => NULL() ! Lake fraction
      MetState%FROCEAN        => NULL() ! Ocean fraction
      MetState%FRLAND         => NULL() ! Land fraction
      MetState%FRLANDIC       => NULL() ! Land ice fraction
      MetState%FROCEAN        => NULL() ! Ocean fraction
      MetState%FRSNO          => NULL() ! Snow fraction
      MetState%FRSEAICE       => NULL() ! Sea ice fraction
      MetState%GWETROOT       => NULL() ! Root zone wetted area
      MetState%GWETTOP        => NULL() ! Top level wetted area
      MetState%HFLUX          => NULL() ! Surface flux
      MetState%IsLand         => NULL() ! Is this grid box land?
      MetState%IsWater        => NULL() ! Is this grid box water?
      MetState%IsIce          => NULL() ! Is this grid box ice?
      MetState%IsSnow         => NULL() ! Is this grid box snow?
      MetState%LAI            => NULL() ! Leaf area index
      MetState%PARDR          => NULL() ! Direct  downward PAR
      MetState%PARDF          => NULL() ! Diffuse downward PAR
      MetState%PBLH           => NULL() ! PBL height
      MetState%PBL_TOP_hPa    => NULL() ! PBL top [hPa]
      MetState%PBL_TOP_L      => NULL() ! PBL top [levels]
      MetState%PBL_TOP_m      => NULL() ! PBL top [m]
      MetState%PS             => NULL() ! Surface pressure
      MetState%QV2M           => NULL() ! 2m specific humidity
      MetState%T2M            => NULL() ! 2m temperature
      MetState%TSKIN          => NULL() ! Skin temperature
      MetState%U10M           => NULL() ! 10m U wind
      MetState%V10M           => NULL() ! 10m V wind
      MetState%z0             => NULL() ! Surface roughness
      MetState%USTAR          => NULL() ! Friction velocity

      !--------------------------------------------------
      ! Allocate fields
      !--------------------------------------------------

      ! Visible Surface Albedo
      ALLOCATE( MetState%ALBD_VIS( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%ALBD_VIS', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%ALBD_VIS = 0e+0_fp

      ! Near-IR Surface Albedo
      ALLOCATE( MetState%ALBD_NIR( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%ALBD_NIR', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%ALBD_NIR = 0e+0_fp

      ! UV Surface Albedo
      ALLOCATE( MetState%ALBD_UV( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%ALBD_UV', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%ALBD_UV = 0e+0_fp

      ! Grid Box Area
      ALLOCATE( MetState%AREA_M2( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%AREA_M2', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%AREA_M2 = 0e+0_fp

      ! Cloud Fraction
      ALLOCATE( MetState%CLDFRC( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%CLDFRC', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%CLDFRC = 0e+0_fp

      ! Convective Depth
      ALLOCATE( MetState%CONV_DEPTH( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%CONV_DEPTH', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%CONV_DEPTH = 0e+0_fp

      ! Latent Heaf Flux
      ALLOCATE( MetState%EFLUX( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%EFLUX', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%EFLUX = 0e+0_fp

      ! Flash Density
      ALLOCATE( MetState%FLASH_DENS( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FLASH_DENS', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN

      ! Fraction of Lake
      ALLOCATE( MetState%FRLAKE( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FRLAKE', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%FRLAKE = 0e+0_fp

      ! Fraction of Land
      ALLOCATE( MetState%FRLAND( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FRLAND', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%FRLAND = 0e+0_fp

      ! Fraction of Land
      ALLOCATE( MetState%FRLANDIC( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FRLANDIC', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%FRLANDIC = 0e+0_fp

      ! Fraction of Ocean
      ALLOCATE( MetState%FROCEAN( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FROCEAN', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%FROCEAN = 0e+0_fp

      ! Fraction of Sea Ice
      ALLOCATE( MetState%FRSEAICE( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FRSEAICE', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%FRSEAICE = 0e+0_fp

      ! Fraction of Snow
      ALLOCATE( MetState%FRSNO( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%FRSNO', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%FRSNO = 0e+0_fp

      ! GWETROOT
      ALLOCATE( MetState%GWETROOT( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%GWETROOT', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%GWETROOT = 0e+0_fp

      ! GWETTOP
      ALLOCATE( MetState%GWETTOP( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%GWETTOP', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%GWETTOP = 0e+0_fp

      ! Heat Flux
      ALLOCATE( MetState%HFLUX( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%HFLUX', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%HFLUX = 0e+0_fp

      ! Is Land grid box
      ALLOCATE( MetState%IsLand( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%IsLand', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%IsLand = .false.

      ! Is water grid box
      ALLOCATE( MetState%IsWater( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%IsWater', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%IsWater = .false.

      ! Is Ice grid box
      ALLOCATE( MetState%IsIce( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%IsIce', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%IsIce = .false.

      ! Is Snow grid box
      ALLOCATE( MetState%IsSnow( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%IsSnow', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%IsSnow = .false.

      ! Leaf Area Index
      ALLOCATE( MetState%LAI( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%LAI', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%LAI = 0e+0_fp

      ! Direct Downwelling PAR
      ALLOCATE( MetState%PARDR( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%PARDR', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%PARDR = 0e+0_fp

      ! Diffuse Downwelling PAR
      ALLOCATE( MetState%PARDF( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%PARDF', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%PARDF = 0e+0_fp

      ! PBL Height
      ALLOCATE( MetState%PBLH( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%PBLH', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%PBLH = 0e+0_fp

      ! PBL Top in hPa
      ALLOCATE( MetState%PBL_TOP_hPa( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%PBL_TOP_hPa', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%PBL_TOP_hPa = 0e+0_fp

      ! PBL Top level
      ALLOCATE( MetState%PBL_TOP_L( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%PBL_TOP_L', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%PBL_TOP_L = 0e+0_fp

      ! Surface Pressure
      ALLOCATE( MetState%PS( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%PS', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%PS = 0e+0_fp

      ! ! Rain
      ! ALLOCATE( MetState%QRAIN( GridState%NX, GridState%NY ), STAT=RC )
      ! CALL CC_CheckVar( 'MetState%QRAIN', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! MetState%QRAIN = 0e+0_fp

      ! ! Snow
      ! ALLOCATE( MetState%QSNOW( GridState%NX, GridState%NY ), STAT=RC )
      ! CALL CC_CheckVar( 'MetState%QSNOW', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! MetState%QSNOW = 0e+0_fp

      ! Specific Humidity 2M
      ALLOCATE( MetState%QV2M( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%QV2M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%QV2M = 0e+0_fp

      ! Temperature 2M
      ALLOCATE( MetState%T2M( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%T2M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%T2M = 0e+0_fp

      ! Skin Temperature
      ALLOCATE( MetState%TSKIN( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%TSKIN', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%TSKIN = 0e+0_fp

      ! U Wind Speed 10M
      ALLOCATE( MetState%U10M( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%U10M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%U10M = 0e+0_fp

      ! V Wind Speed 10M
      ALLOCATE( MetState%V10M( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%V10M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%V10M = 0e+0_fp

      ! Surface Roughness
      ALLOCATE( MetState%z0( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%z0', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%z0 = 0e+0_fp

      ! Surface Roughness
      ALLOCATE( MetState%USTAR( GridState%NX, GridState%NY ), STAT=RC )
      CALL CC_CheckVar( 'MetState%USTAR', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      MetState%USTAR = 0e+0_fp
   end subroutine Met_Allocate

END MODULE MetState_Mod
