!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: state_met_mod.F90
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
   PUBLIC :: Zero_State_Met
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
      REAL(fp), POINTER :: DP_DRY_PREV   (:,:,:) ! Previous State_Met%DELP_DRY
      REAL(fp), POINTER :: SPHU_PREV     (:,:,:) ! Previous State_Met%SPHU

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
      ! Registry of variables contained within State_Met
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
   SUBROUTINE Zero_State_Met( State_Met, RC )
      !
      ! !INPUT/OUTPUT PARAMETERS:
      !
      TYPE(MetStateType), INTENT(INOUT) :: State_Met
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
      State_Met%ALBD_VIS       => NULL() ! Visible albedo
      State_Met%ALBD_NIR       => NULL() ! Near-IR albedo
      State_Met%ALBD_UV        => NULL() ! UV albedo
      State_Met%AREA_M2        => NULL() ! Area of grid box
      State_Met%CLDFRC         => NULL() ! Cloud fraction
      ! State_Met%CLDTOPS        => NULL() ! Cloud top level
      State_Met%CONV_DEPTH     => NULL() ! Convective depth
      State_Met%EFLUX          => NULL() ! Surface flux
      State_Met%FLASH_DENS     => NULL() ! Flash density
      State_Met%FRLAKE         => NULL() ! Fraction Lake of grid box
      State_Met%FRLAND         => NULL() ! Fraction Land of grid box
      State_Met%FRLANDIC       => NULL() ! Fraction LandIce of grid box
      State_Met%FROCEAN        => NULL() ! Fraction Ocean of grid box
      State_Met%FRSEAICE       => NULL() ! Fraction SeaIce of grid box
      State_Met%FRSNO          => NULL() ! Fraction Snow of grid box
      State_Met%GWETROOT       => NULL() ! Soil Moisture Root Zone
      State_Met%GWETTOP        => NULL() ! Soil Moisture Top Level
      State_Met%HFLUX          => NULL() ! Surface flux
      State_Met%IsLand         => NULL() ! Is this grid box land?
      State_Met%IsWater        => NULL() ! Is this grid box water?
      State_Met%IsIce          => NULL() ! Is this grid box ice?
      State_Met%IsSnow         => NULL() ! Is this grid box snow?
      State_Met%LAI            => NULL() ! Leaf Area Index
      State_Met%PARDR          => NULL() ! Direct  downward PAR
      State_Met%PARDF          => NULL() ! Diffuse downward PAR
      State_Met%PBLH           => NULL() ! PBL height
      State_Met%PBL_TOP_hPa    => NULL() ! PBL top [hPa]
      State_Met%PBL_TOP_L      => NULL() ! PBL top [levels]
      State_Met%PBL_TOP_m      => NULL() ! PBL top [m]
      State_Met%PBL_THICK      => NULL() ! PBL thickness
      State_Met%PHIS           => NULL() ! Surface geopotential
      State_Met%PRECANV        => NULL() ! Canopy evaporation
      State_Met%PRECCON        => NULL() ! Convective precipitation
      State_Met%PRECLSC        => NULL() ! Large-scale precipitation
      State_Met%PRECTOT        => NULL() ! Total precipitation
      ! State_Met%PS1_WET        => NULL()
      ! State_Met%PS2_WET        => NULL()
      ! State_Met%PSC2_WET       => NULL()
      ! State_Met%PS1_DRY        => NULL()
      ! State_Met%PS2_DRY        => NULL()
      ! State_Met%PSC2_DRY       => NULL()
      State_Met%QV2M           => NULL() ! 2-m specific humidity
      State_Met%SEAICE00       => NULL() ! Sea ice fraction
      State_Met%SEAICE10       => NULL() ! Sea ice fraction
      State_Met%SEAICE20       => NULL() ! Sea ice fraction
      State_Met%SEAICE30       => NULL() ! Sea ice fraction
      State_Met%SEAICE40       => NULL() ! Sea ice fraction
      State_Met%SEAICE50       => NULL() ! Sea ice fraction
      State_Met%SEAICE60       => NULL() ! Sea ice fraction
      State_Met%SEAICE70       => NULL() ! Sea ice fraction
      State_Met%SEAICE80       => NULL() ! Sea ice fraction
      State_Met%SEAICE90       => NULL() ! Sea ice fraction
      State_Met%SLP            => NULL() ! Sea level pressure
      State_Met%SNODP          => NULL() ! Snow depth
      State_Met%SNOMAS         => NULL() ! Snow mass
      State_Met%SUNCOS         => NULL() ! Cosine of solar zenith angle
      State_Met%SUNCOSmid      => NULL() ! Cosine of solar zenith angle
      State_Met%SUNCOSsum      => NULL() ! Cosine of solar zenith angle
      State_Met%SZAFACT        => NULL() ! Cosine of solar zenith angle
      State_Met%SWGDN          => NULL() ! Surface downward SW radiation
      State_Met%TO3            => NULL() ! Total ozone
      State_Met%TROPP          => NULL() ! Topopause level pressure
      State_Met%TropLev        => NULL() ! Topopause level
      State_Met%TropHt         => NULL() ! Top level height
      State_Met%TS             => NULL() ! Surface temperature
      State_Met%TSKIN          => NULL() ! Skin temperature
      State_Met%U10M           => NULL() ! 10-m zonal wind
      State_Met%USTAR          => NULL() ! Friction velocity
      State_Met%V10M           => NULL() ! 10-m meridional wind
      State_Met%Z0             => NULL() ! Surface roughness
      State_Met%CNV_FRC        => NULL() ! Convective fraction
      State_Met%CLDF           => NULL() ! Cloud fraction
      State_Met%CMFMC          => NULL() ! Convective mass flux
      State_Met%DQRCU          => NULL()
      State_Met%DQRLSAN        => NULL()
      State_Met%DTRAIN         => NULL()
      State_Met%F_OF_PBL       => NULL()
      State_Met%F_UNDER_PBLTOP => NULL()
      State_Met%OMEGA          => NULL()
      State_Met%OPTD           => NULL()
      State_Met%PEDGE          => NULL()
      State_Met%PFICU          => NULL()
      State_Met%PFILSAN        => NULL()
      State_Met%PFLCU          => NULL()
      State_Met%PFLLSAN        => NULL()
      State_Met%QI             => NULL()
      State_Met%QL             => NULL()
      State_Met%RH             => NULL()
      State_Met%SPHU           => NULL()
      State_Met%SPHU1          => NULL()
      State_Met%SPHU2          => NULL()
      State_Met%T              => NULL()
      State_Met%TAUCLI         => NULL()
      State_Met%TAUCLW         => NULL()
      State_Met%TMPU1          => NULL()
      State_Met%TMPU2          => NULL()
      State_Met%U              => NULL()
      State_Met%UPDVVEL        => NULL()
      State_Met%V              => NULL()
      State_Met%PEDGE_DRY      => NULL()
      State_Met%PMID           => NULL()
      State_Met%PMID_DRY       => NULL()
      State_Met%THETA          => NULL()
      State_Met%TV             => NULL()
      State_Met%MAIRDEN        => NULL()
      State_Met%AIRDEN         => NULL()
      State_Met%AIRNUMDEN      => NULL()
      State_Met%AVGW           => NULL()
      State_Met%BXHEIGHT       => NULL()
      State_Met%DELP           => NULL()
      State_Met%DELP_DRY       => NULL()
      State_Met%AD             => NULL()
      State_Met%PS             => NULL()
      State_Met%AIRVOL         => NULL()
      State_Met%DP_DRY_PREV    => NULL()
      State_Met%SPHU_PREV      => NULL()
      State_Met%IREG           => NULL()
      State_Met%ILAND          => NULL()
      State_Met%IUSE           => NULL()
      State_Met%MODISLAI       => NULL()
      State_Met%XLAI           => NULL()
      State_Met%LandTypeFrac   => NULL()
      State_Met%XLAI_NATIVE    => NULL()
      State_Met%XLAI2          => NULL()
      State_Met%InChemGrid     => NULL()
      State_Met%InPbl          => NULL()
      State_Met%InStratMeso    => NULL()
      State_Met%InStratosphere => NULL()
      State_Met%InTroposphere  => NULL()
      State_Met%LocalSolarTime => NULL()
      State_Met%IsLocalNoon    => NULL()
      State_Met%IMIX           => NULL()
      State_Met%FPBL           => NULL()
      State_Met%PBL_MAX_L      = 0

   END SUBROUTINE Zero_State_Met

   SUBROUTINE Met_Allocate( State_Grid, State_Met, RC)
      ! USES
      USE GridState_Mod, Only : GridStateType

      IMPLICIT NONE

      ! Arguments
      TYPE(GridStateType), INTENT(IN)  :: State_Grid
      TYPE(MetStateType),  INTENT(OUT) :: State_Met
      INTEGER,             INTENT(OUT) :: RC

      ! Local variables
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Met_Allocate (in core/metstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      State_Met%ALBD_VIS       => NULL() ! Visible albedo
      State_Met%ALBD_NIR       => NULL() ! Near-IR albedo
      State_Met%ALBD_UV        => NULL() ! UV albedo
      State_Met%AREA_M2        => NULL() ! Area of grid box
      ! State_Met%CLDFRA         => NULL() ! Cloud fraction
      State_Met%CONV_DEPTH     => NULL() ! Convective depth
      State_Met%EFLUX          => NULL() ! Latent heat flux
      State_Met%FLASH_DENS     => NULL() ! Flash density
      State_Met%FRLAKE         => NULL() ! Lake fraction
      State_Met%FROCEAN        => NULL() ! Ocean fraction
      State_Met%FRLAND         => NULL() ! Land fraction
      State_Met%FRLANDIC       => NULL() ! Land ice fraction
      State_Met%FROCEAN        => NULL() ! Ocean fraction
      State_Met%FRSNO          => NULL() ! Snow fraction
      State_Met%FRSEAICE       => NULL() ! Sea ice fraction
      State_Met%GWETROOT       => NULL() ! Root zone wetted area
      State_Met%GWETTOP        => NULL() ! Top level wetted area
      State_Met%HFLUX          => NULL() ! Surface flux
      State_Met%IsLand         => NULL() ! Is this grid box land?
      State_Met%IsWater        => NULL() ! Is this grid box water?
      State_Met%IsIce          => NULL() ! Is this grid box ice?
      State_Met%IsSnow         => NULL() ! Is this grid box snow?
      State_Met%LAI            => NULL() ! Leaf area index
      State_Met%PARDR          => NULL() ! Direct  downward PAR
      State_Met%PARDF          => NULL() ! Diffuse downward PAR
      State_Met%PBLH           => NULL() ! PBL height
      State_Met%PBL_TOP_hPa    => NULL() ! PBL top [hPa]
      State_Met%PBL_TOP_L      => NULL() ! PBL top [levels]
      State_Met%PBL_TOP_m      => NULL() ! PBL top [m]
      State_Met%PS             => NULL() ! Surface pressure
      State_Met%QV2M           => NULL() ! 2m specific humidity
      State_Met%T2M            => NULL() ! 2m temperature
      State_Met%TSKIN          => NULL() ! Skin temperature
      State_Met%U10M           => NULL() ! 10m U wind
      State_Met%V10M           => NULL() ! 10m V wind
      State_Met%z0             => NULL() ! Surface roughness
      State_Met%USTAR          => NULL() ! Friction velocity

      !--------------------------------------------------
      ! Allocate fields
      !--------------------------------------------------

      ! Visible Surface Albedo
      ALLOCATE( State_Met%ALBD_VIS( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%ALBD_VIS', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%ALBD_VIS = 0e+0_fp

      ! Near-IR Surface Albedo
      ALLOCATE( State_Met%ALBD_NIR( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%ALBD_NIR', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%ALBD_NIR = 0e+0_fp

      ! UV Surface Albedo
      ALLOCATE( State_Met%ALBD_UV( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%ALBD_UV', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%ALBD_UV = 0e+0_fp

      ! Grid Box Area
      ALLOCATE( State_Met%AREA_M2( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%AREA_M2', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%AREA_M2 = 0e+0_fp

      ! Cloud Fraction
      ALLOCATE( State_Met%CLDFRC( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%CLDFRC', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%CLDFRC = 0e+0_fp

      ! Convective Depth
      ALLOCATE( State_Met%CONV_DEPTH( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%CONV_DEPTH', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%CONV_DEPTH = 0e+0_fp

      ! Latent Heaf Flux
      ALLOCATE( State_Met%EFLUX( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%EFLUX', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%EFLUX = 0e+0_fp

      ! Flash Density
      ALLOCATE( State_Met%FLASH_DENS( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FLASH_DENS', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN

      ! Fraction of Lake
      ALLOCATE( State_Met%FRLAKE( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FRLAKE', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%FRLAKE = 0e+0_fp

      ! Fraction of Land
      ALLOCATE( State_Met%FRLAND( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FRLAND', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%FRLAND = 0e+0_fp

      ! Fraction of Land
      ALLOCATE( State_Met%FRLANDIC( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FRLANDIC', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%FRLANDIC = 0e+0_fp

      ! Fraction of Ocean
      ALLOCATE( State_Met%FROCEAN( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FROCEAN', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%FROCEAN = 0e+0_fp

      ! Fraction of Sea Ice
      ALLOCATE( State_Met%FRSEAICE( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FRSEAICE', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%FRSEAICE = 0e+0_fp

      ! Fraction of Snow
      ALLOCATE( State_Met%FRSNO( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%FRSNO', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%FRSNO = 0e+0_fp

      ! GWETROOT
      ALLOCATE( State_Met%GWETROOT( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%GWETROOT', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%GWETROOT = 0e+0_fp

      ! GWETTOP
      ALLOCATE( State_Met%GWETTOP( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%GWETTOP', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%GWETTOP = 0e+0_fp

      ! Heat Flux
      ALLOCATE( State_Met%HFLUX( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%HFLUX', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%HFLUX = 0e+0_fp

      ! Is Land grid box
      ALLOCATE( State_Met%IsLand( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%IsLand', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%IsLand = .false.

      ! Is water grid box
      ALLOCATE( State_Met%IsWater( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%IsWater', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%IsWater = .false.

      ! Is Ice grid box
      ALLOCATE( State_Met%IsIce( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%IsIce', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%IsIce = .false.

      ! Is Snow grid box
      ALLOCATE( State_Met%IsSnow( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%IsSnow', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%IsSnow = .false.

      ! Leaf Area Index
      ALLOCATE( State_Met%LAI( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%LAI', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%LAI = 0e+0_fp

      ! Direct Downwelling PAR
      ALLOCATE( State_Met%PARDR( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%PARDR', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%PARDR = 0e+0_fp

      ! Diffuse Downwelling PAR
      ALLOCATE( State_Met%PARDF( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%PARDF', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%PARDF = 0e+0_fp

      ! PBL Height
      ALLOCATE( State_Met%PBLH( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%PBLH', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%PBLH = 0e+0_fp

      ! PBL Top in hPa
      ALLOCATE( State_Met%PBL_TOP_hPa( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%PBL_TOP_hPa', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%PBL_TOP_hPa = 0e+0_fp

      ! PBL Top level
      ALLOCATE( State_Met%PBL_TOP_L( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%PBL_TOP_L', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%PBL_TOP_L = 0e+0_fp

      ! Surface Pressure
      ALLOCATE( State_Met%PS( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%PS', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%PS = 0e+0_fp

      ! ! Rain
      ! ALLOCATE( State_Met%QRAIN( State_Grid%NX, State_Grid%NY ), STAT=RC )
      ! CALL CC_CheckVar( 'State_Met%QRAIN', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! State_Met%QRAIN = 0e+0_fp

      ! ! Snow
      ! ALLOCATE( State_Met%QSNOW( State_Grid%NX, State_Grid%NY ), STAT=RC )
      ! CALL CC_CheckVar( 'State_Met%QSNOW', 0, RC )
      ! IF ( RC /= CC_SUCCESS ) RETURN
      ! State_Met%QSNOW = 0e+0_fp

      ! Specific Humidity 2M
      ALLOCATE( State_Met%QV2M( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%QV2M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%QV2M = 0e+0_fp

      ! Temperature 2M
      ALLOCATE( State_Met%T2M( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%T2M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%T2M = 0e+0_fp

      ! Skin Temperature
      ALLOCATE( State_Met%TSKIN( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%TSKIN', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%TSKIN = 0e+0_fp

      ! U Wind Speed 10M
      ALLOCATE( State_Met%U10M( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%U10M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%U10M = 0e+0_fp

      ! V Wind Speed 10M
      ALLOCATE( State_Met%V10M( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%V10M', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%V10M = 0e+0_fp

      ! Surface Roughness
      ALLOCATE( State_Met%z0( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%z0', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%z0 = 0e+0_fp

      ! Surface Roughness
      ALLOCATE( State_Met%USTAR( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'State_Met%USTAR', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Met%USTAR = 0e+0_fp
   end subroutine Met_Allocate

END MODULE MetState_Mod
