!> \brief CCPR drydep state types
!!
!!
!!
!! \author Lacey Holland
!! \date 07/2024
!!!>
MODULE CCPR_DryDep_mod
   USE Precision_mod
   USE Error_Mod
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod,  Only : MetStateType
   USE ChemState_Mod, Only : ChemStateType
   USE Config_Opt_Mod, Only : ConfigType

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CCPR_DryDep_Init
   PUBLIC :: CCPR_DryDep_Run
   PUBLIC :: CCPR_DryDep_Finalize


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
      LOGICAL                         :: Resuspension          ! Activate resuspension  (True/False)
      INTEGER                         :: SchemeOpt             ! Scheme Option (if there is only one SchemeOpt always = 1)
      INTEGER                         :: Number_of_levels      ! number of vertical levels
      REAL                            :: ParticleRadius
      REAL                            :: ParticleDensity
      REAL                            :: U10M
      REAL                            :: V10M
      REAL                            :: FRACLAKE
      REAL                            :: GWETTOP

      ! Process Specific Parameters

      ! Namelist parameters for specific DryDep goes here as well
      !=================================================================
      ! Module specific variables/arrays/data pointers come below
      !=================================================================



   END TYPE DryDepStateType


CONTAINS

   !>
   !! \brief Initialize the CATChem DryDep module
   !!
   !! \param Config       CATCHem configuration options
   !! \param DryDepState   CATCHem PROCESS state
   !! \param ChemState         CATCHem chemical state
   !! \param RC               Error return code
   !!
   !!!>

   SUBROUTINE CCPR_DryDep_Init( Config, DryDepState, ChemState, RC )
      ! USE


      IMPLICIT NONE
      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType), POINTER       :: Config    ! Module options
      TYPE(ChemStateType), POINTER    :: ChemState ! Chemical state

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DryDepStateType), POINTER  :: DryDepState ! DryDep state
      INTEGER,         INTENT(INOUT)    :: RC       ! Success or failure

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
         DryDepState%SchemeOpt = config%DryDep_Scheme


         if (Config%DryDep_Resuspension) then

            ! Set particle radius, density,
            ! and other terms for a resuspension term
            !------------------

            !DryDepState%particleradius = config%DryDep_ParticleRadius
            !DryDepState%particledensity = config%DryDep_ParticleDensity
            DryDepState%u10m = config%DryDep_u10m
            DryDepState%v10m = config%DryDep_v10m
            DryDepState%fraclake = config%DryDep_fraclake
            DryDepState%gwettop = config%DryDep_gwettop
            DryDepState%number_of_levels = config%number_of_levels

         endif

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
      USE GridState_Mod
      USE MetState_Mod
      USE constants
      USE precision_MOD

      IMPLICIT NONE
      ! INPUT PARAMETERS
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance
      !TYPE(GridStateType), INTENT(IN) :: GridState      ! Grid state

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DiagStateType), INTENT(INOUT)      :: DiagState       ! DiagState Instance
      TYPE(DryDepStateType), INTENT(INOUT)    :: DryDepState     ! DryDepState Instance
      TYPE(ChemStateType),  INTENT(INOUT)     :: ChemState       ! ChemState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                 ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      INTEGER :: km
      REAL, DIMENSION(1,1) :: drydepf                     ! Deposition frequency [1/sec]
      REAL, allocatable, DIMENSION(:,:,:) :: tmpu
      REAL, allocatable, DIMENSION(:,:,:) :: rhoa
      REAL, allocatable, DIMENSION(:,:,:) :: hghte
      REAL :: radius    ! particle radius [m]
      REAL :: rhop      ! particle density [kg/m^3]
      INTEGER :: oro       ! orography flag; Land, ocean, ice mask
      REAL :: ustar     ! friction speed [m/sec]
      REAL :: pblh      ! PBL height [m]
      REAL :: shflux    ! sfc. sens. heat flux [W m-2]
      REAL :: z0h       ! rough height, sens. heat [m]
      REAL, DIMENSION(1,1) :: u10m       ! 10-m u-wind component [m/sec]
      REAL, DIMENSION(1,1) :: v10m       ! 10-m v-wind component [m/sec]
      REAL, DIMENSION(1,1) :: fraclake   ! fraction covered by water [1]
      REAL, DIMENSION(1,1) :: gwettop    ! fraction soil moisture [1]


!      z0h    = call(metstate%z0h)       ! rough height, sens. heat [m]

      ! FIXME: These are pointers (next 3)
      km  = DryDepstate%number_of_levels          ! Number of levels in the model - this should go somewhere else later
      allocate(tmpu(1,1, km), rhoa(1,1,km), hghte(1,1,km))
      tmpu = reshape(metstate%T, (/1, 1, km/))         ! temperature [K]
      rhoa = reshape(metstate%mairden, (/1, 1, km/)) ! air density [kg/m^3]
      hghte = reshape(metstate%phit, (/1, 1, km/))    ! top of layer geopotential height [m]
      oro    = metstate%LWI       ! orography flag; Land, ocean, ice mask
      ustar  = metstate%ustar     ! friction speed [m/sec]
      pblh   = metstate%pblh      ! PBL height [m]
      shflux = metstate%hflux     ! sfc. sens. heat flux [W m-2]
      z0h    = metstate%z0h       ! rough height, sens. heat [m]
      


      radius = DryDepState%particleradius   ! particle radius [m]
      rhop   = DryDepState%particledensity  ! particle density [kg/m^3]


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

            if (DryDepState%resuspension) then
               u10m(1,1)   = metstate%U10m     ! 10-m u-wind component [m/sec]
               v10m(1,1)   = metstate%V10m     ! 10-m v-wind component [m/sec]
               fraclake(1,1)=metstate%FRLAKE   ! fraction covered by water [1]
               gwettop(1,1)= metstate%GWETTOP  ! fraction soil moisture [1]

            call DryDeposition( km, tmpu, rhoa, hghte, oro, ustar, pblh, shflux, &
               von_karman, cp, g0, z0h, drydepf, rc, &
               radius, rhop, u10m, v10m, fraclake, gwettop )

            else

            call DryDeposition( km, tmpu, rhoa, hghte, oro, ustar, pblh, shflux, &
               von_karman, cp, g0, z0h, drydepf, rc)


            endif

         endif

         ! TO DO:  apply dry dep velocities/freq to chem species
         write(*,*) 'TODO: Need to figure out how to add back to the chemical species state '

      endif



   end subroutine CCPr_DryDep_Run

   !>
   !! \brief Finalize the DryDep
   !!
   !! \param [INOUT] DryDepState
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_DryDep_Finalize( DryDepState, RC )

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
      thisLoc = ' -> at CCPr_DryDep_Finalize (in process/drydep/ccpr_DryDep_mod.F90)'


   end subroutine CCPr_DryDep_Finalize


END MODULE CCPR_DryDep_Mod
