!> \brief Template for a new process driver
!!
!!
!! More in-depth description here
!!
!! \author Barry baker
!! \date 05/2024
!!!>
MODULE CCPR_DryDep_mod
   USE Precision_mod
   USE Error_Mod
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod,  Only : MetStateType
   USE ChemState_Mod, Only : ChemStateType
   USE Config_Mod,    Only : ConfigType

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CCPR_DryDep_Init
   PUBLIC :: CCPR_DryDep_Run
   PUBLIC :: CCPR_DryDep_Final


   !> \brief DryDepStateType
   !!
   !! DryDepStateType is the process-specific derived type. It should hold all module
   !! variables and arrays that are required to compute the emissions.
   !! For instance, if the process relies on an input field read through the
   !! CATChem configuration file (e.g. MY_INPUT_FIELD), the data array pointer
   !! to that field should be listed within the instance and NOT outside of it.
   !! This ensures that the same process can be invoked in various instances,
   !! all of them potentially pointing to different data fields.
   !!
   !! \param Activate Activate Process (True/False)
   !! \param SchemeOpt Scheme Option
   !! \param DryDepSpeciesIndex Effected Chemical Species from DryDep
   !! \param nSpc # of species
   !! \param SpcIDs CATChem species IDs
   !! \param ScaleFactor Scale Factor
   !!!>
   TYPE :: DryDepStateType
      LOGICAL                         :: Activate              ! Activate Process (True/False)
      INTEGER                         :: SchemeOpt             ! Scheme Option (if there is only one SchemeOpt always = 1)


      ! Process Specific Parameters

      ! Namelist parameters for specific DryDep goes here as well
      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================
      integer                         :: DryDepGOCARTOpt       ! Scheme Option


   END TYPE DryDepStateType


CONTAINS

   !>
   !! \brief Initialize the CATChem DryDep module
   !!
   !! \param Config       CATCHem configuration options
   !! \param DryDepState   CATCHem PROCESS state
   !! \param ChmState         CATCHem chemical state
   !! \param RC               Error return code
   !!
   !!!>

   SUBROUTINE CCPR_DryDep_Init( Config, ChemState, DryDepState, RC )
      ! USE

      IMPLICIT NONE
      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType), POINTER       :: Config    ! Module options
      TYPE(ChemStateType), POINTER    :: ChemState ! Chemical state

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DryDepStateType), POINTER :: DryDepState ! DryDep state
      INTEGER,         INTENT(INOUT)    :: RC         ! Success or failure

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! LOCAL VARIABLES
      !----------------


      ! Put any local variables here

      !=================================================================
      ! CCPR_DryDep_Init begins here!
      !=================================================================
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_DryDep_INIT (in process/drydep/ccpr_drydep_mod.F90)'

      ! First check if process is activated in config | if not don't allocate arrays or pointers
      if (Config%DryDep_activate) then

         ! Activate Process
         !------------------
         DryDepState%Activate = .true.


         ! Set scheme option
         !------------------
         DryDepState%SchemeOpt = config%DryDepGOCARTOpt

         ! Set particle radius, density,
         ! and other terms for a resuspension term
         !------------------
         DryDepState%particleradius = config%DryDepParticleRadius
         DryDepState%particledensity = config%DryDepParticleDensity
         DryDepState%u10m = config%DryDepu10m
         DryDepState%v10m = config%DryDepv10m
         DryDepState%fraclake = config%DryDepfraclake
         DryDepState%gwettop = config%DryDepgwettop


      else

         DryDepState%Activate = .false.

      endif

   end subroutine CCPR_DryDep_Init

   !>
   !! \brief Run the DryDep
   !!
   !! \param [IN] MetState - The MetState object
   !! \param [INOUT] DiagState - The DiagState object
   !! \param [INOUT] DryDepState - The DryDepState object
   !! \param [INOUT] ChemState - The ChemState object
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_DryDep_Run( MetState, DiagState, DryDepState, ChemState, RC )

      ! USE
      USE DryDeposition
      USE GridState_Mod
      USE constants

      IMPLICIT NONE
      ! INPUT PARAMETERS
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DiagStateType), INTENT(INOUT)      :: DiagState       ! DiagState Instance
      TYPE(DryDepStateType), INTENT(INOUT)    :: DryDepState     ! DryDepState Instance
      TYPE(ChemStateType),  INTENT(INOUT)     :: ChemState       ! ChemState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                  ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      INTEGER :: km = Gridstate%number_of_levels          ! Number of levels in the model
      REAL, DIMENSION(1,1,km) :: tmpu = reshape(metstate%T, (/1, 1, km/))         ! temperature [K]
      REAL, DIMENSION(1,1,km) :: rhoa   = reshape(metstate%mairden, (/1, 1, km/)) ! air density [kg/m^3]
      REAL, DIMENSION(1,1,km) :: hghte  = reshape(metstate%phit, (/1, 1, km/))     ! top of layer geopotential height [m]
      REAL, DIMENSION(1,1) :: oro    = metstate%LandTypeFrac       ! orography flag; see
      ! https://wiki.seas.harvard.edu/geos-chem/index.php/Olson_land_map
      REAL, DIMENSION(1,1) :: ustar  = metstate%ustar     ! friction speed [m/sec]
      REAL, DIMENSION(1,1) :: pblh   = metstate%pblh      ! PBL height [m]
      REAL, DIMENSION(1,1) :: shflux = metstate%hflux     ! sfc. sens. heat flux [W m-2]
      REAL, DIMENSION(1,1) :: z0h    = metstate%z0        ! rough height, sens. heat [m]
      REAL, DIMENSION(1,1) :: drydepf                     ! Deposition frequency [1/sec]


      REAL :: radius = DryDepState%particleradius   ! particle radius [m]
      REAL :: rhop   = DryDepState%particledensity  ! particle density [kg/m^3]
      REAL, DIMENSION(1,1) :: u10m   = reshape(DryDepState%U10m, (/1, 1/))   ! 10-m u-wind component [m/sec]
      REAL, DIMENSION(1,1) :: v10m   = reshape(DryDepState%V10m, (/1, 1/))   ! 10-m v-wind component [m/sec]
      REAL, DIMENSION(1,1) :: fraclake=reshape(metstate%FRLAKE, (/1, 1/))    ! fraction covered by water [1]
      REAL, DIMENSION(1,1) :: gwettop= reshape(metstate%GWETTOP, (/1, 1/))   ! fraction soil moisture [1]


      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_DryDep_Run (in process/drydep/ccpr_DryDep_mod.F90)'

      ! Run the DryDep Scheme
      !-------------------------
      if (DryDepState%Activate) then
         ! Run the DryDep Scheme
         !-------------------------
         if (DryDepState%SchemeOpt == 1) then
            ! Run the DryDep Scheme
            !-------------------------

            DryDeposition ( km, tmpu, rhoa, hghte, oro, ustar, pblh, shflux, &
               von_karman, cp, g0, z0h, drydepf, rc, &
               radius, rhop, u10m, v10m, fraclake, gwettop )

         endif

      endif

   end subroutine CCPr_DryDep_Run

   !>
   !! \brief Finalize the DryDep
   !!
   !! \param [INOUT] DryDepState
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_DryDep_Final( DryDepState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DryDepStateType), INTENT(INOUT) :: DryDepState  ! DryDepState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                  ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_DryDep_Final (in process/drydep/ccpr_DryDep_mod.F90)'



   end subroutine CCPr_DryDep_Final


END MODULE CCPR_DryDep_Mod
