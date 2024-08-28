!> \brief CCPR drydep state types
!!
!! \defgroup catchem_drydep_process
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
   !! DryDepStateType is the process-specific derived type.
   !!
   !! \param Activate Activate Process (True/False)
   !! \param Scheme Scheme Option
   !! \param DryDepSpeciesIndex Effected Chemical Species from DryDep
   !! \param nSpc # of species
   !! \param SpcIDs CATChem species IDs
   !! \param ScaleFactor Scale Factor
   !! \param Resuspension Activate resuspension  (True/False)
   !!
   !! \ingroup core_modules
   !!!>
   TYPE :: DryDepStateType
      LOGICAL                         :: Activate              ! Activate Process (True/False)
      LOGICAL                         :: Resuspension          ! Activate resuspension  (True/False)
      INTEGER                         :: SchemeOpt             ! Scheme Option (if there is only one SchemeOpt always = 1)

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
   !! \ingroup catchem_drydep_process
   !!
   !!!>
   SUBROUTINE CCPR_DryDep_Init( Config, DryDepState, ChemState, RC )
      ! USE
      USE ChemState_Mod, ONLY : ChemStateType
      use Config_Opt_Mod, only : ConfigType
      use DiagState_Mod, only : DiagStateType


      IMPLICIT NONE
      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType), POINTER       :: Config    ! Module options
      TYPE(ChemStateType), POINTER    :: ChemState ! Chemical state

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DryDepStateType), POINTER  :: DryDepState ! DryDep state
      INTEGER,         INTENT(INOUT)  :: RC       ! Success or failure

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

         if (Config%DryDep_resuspension) then
            ! Activate resuspension
            !------------------
            DryDepState%Resuspension = .true.
         else
            DryDepState%Resuspension = .false.
         end if
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
   !!
   !! \ingroup catchem_drydep_process
   !!!>
   SUBROUTINE CCPr_DryDep_Run( MetState, DiagState, DryDepState, ChemState, RC )

      ! USE
      USE constants
      USE precision_MOD
      use MetState_Mod, only : MetStateType
      use DiagState_Mod, only : DiagStateType
      use ChemState_Mod, only : ChemStateType

      IMPLICIT NONE
      ! INPUT PARAMETERS
      TYPE(MetStateType),  INTENT(IN) :: MetState       !< MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DiagStateType), INTENT(INOUT)      :: DiagState       !< DiagState Instance
      TYPE(DryDepStateType), INTENT(INOUT)    :: DryDepState     !< DryDepState Instance
      TYPE(ChemStateType),  INTENT(INOUT)     :: ChemState       !< ChemState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                 ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      INTEGER :: km
      INTEGER :: i !< counter
      INTEGER :: lwi                                    ! orography flag; Land, ocean, ice mask
      REAL(fp), DIMENSION(1,1) :: drydepf               ! Deposition frequency [1/sec]
      REAL(fp), allocatable, DIMENSION(:,:,:) :: tmpu   ! Temperature [K]
      REAL(fp), allocatable, DIMENSION(:,:,:) :: rhoa   ! Air density [kg/m^3]
      REAL(fp), allocatable, DIMENSION(:,:,:) :: hghte  ! Height [m]
      REAL(fp) :: radius                                ! particle radius [m]
      REAL(fp) :: rhop                                  ! particle density [kg/m^3]
      REAL(fp) :: ustar                                 ! friction speed [m/sec]
      REAL(fp) :: pblh                                  ! PBL height [m]
      REAL(fp) :: hflux                                ! sfc. sens. heat flux [W m-2]
      REAL(fp) :: z0h                                   ! rough height, sens. heat [m]
      REAL :: ddfreq
      REAL(fp), DIMENSION(1,1) :: u10m                   ! 10-m u-wind component [m/sec]
      REAL(fp), DIMENSION(1,1) :: v10m                   ! 10-m v-wind component [m/sec]
      REAL(fp), DIMENSION(1,1) :: fraclake               ! fraction covered by water [1]
      REAL(fp), DIMENSION(1,1) :: gwettop                ! fraction soil moisture [1]
      REAL(fp) :: dqa                                    ! Change in Species due to drydep
      REAL(fp) :: SpecConc                               ! Temporary Species concentration

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_DryDep_Run (in process/drydep/ccpr_DryDep_mod.F90)'

      km = MetState%NLEVS

      ! Run the DryDep Scheme
      !-------------------------
      if (DryDepState%Activate) then
         ! Run the DryDep Scheme
         !-------------------------
         if (DryDepState%SchemeOpt == 1) then
            ! Run the DryDep Scheme - Only Applicable to AEROSOL species
            !-------------------------
            if (ChemState%nSpeciesAeroDryDep > 0) then

               call PrepMetVarsForGOCART(MetState, tmpu, rhoa, hghte, lwi, ustar, &
                  pblh, hflux, z0h, u10m, v10m, fraclake, gwettop, rc)

         ! loop through aerosol species
         do i = 1, ChemState%nSpeciesAeroDryDep

            if (DryDepState%resuspension) then

               radius = ChemState%chemSpecies(ChemState%DryDepIndex(i))%radius
               rhop = ChemState%chemSpecies(ChemState%DryDepIndex(i))%density

               call CCPr_Scheme_GOCART_DryDep( km,          &
                  tmpu,        &
                  rhoa,        &
                  hghte,       &
                  lwi,         &
                  ustar,       &
                  pblh,        &
                  hflux,      &
                  von_karman,  &
                  cp,          &
                  g0,          &
                  z0h,         &
                  drydepf,     &
                  rc,          &
                  radius,      &
                  rhop,        &
                  u10m,        &
                  v10m,        &
                  fraclake,    &
                  gwettop )
               if (RC /= CC_SUCCESS) then
                  errMsg = 'Error in GOCART DryDeposition'
                  CALL CC_Error( errMsg, RC, thisLoc )
               endif  !if (RC /= CC_SUCCESS) 
            else
               call CCPr_Scheme_GOCART_DryDep( km,  &
                  tmpu,        &
                  rhoa,        &
                  hghte,       &
                  lwi,         &
                  ustar,       &
                  pblh,        &
                  hflux,      &
                  von_karman,  &
                  cp,          &
                  g0,          &
                  z0h,         &
                  drydepf,     &
                  rc)
               if (RC /= CC_SUCCESS) then
                  errMsg = 'Error in GOCART DryDeposition'
                  CALL CC_Error( errMsg, RC, thisLoc )
               endif  !if (RC /= CC_SUCCESS) 

            endif  ! if (DryDepState%resuspension)

            ! Fill Diagnostic Variables
            !--------------------------
            !!!!FIXME: COME BACK TO THIS LATER
            !DiagState%drydep_frequency(ChemState%DryDepIndex(i)) = drydepf(1,1)
            !DiagState%drydep_vel(ChemState%DryDepIndex(i)) = MetState%ZMID(1) * drydepf(1,1)

            ! apply drydep velocities/freq to chem species
            dqa = 0.
            dqa = MAX(0.0_fp, ChemState%chemSpecies(ChemState%DryDepIndex(i))%conc(1)   &
               * (1.-exp(-drydepf(1,1) * MetState%TSTEP)))
            ChemState%chemSpecies(ChemState%DryDepIndex(i))%conc(1) =     &
               ChemState%chemSpecies(ChemState%DryDepIndex(i))%conc(1) - dqa

         end do ! do i = 1, ChemState%nSpeciesAeroDryDep

      endif  ! if (ChemState%nSpeciesAeroDryDep > 0)

   endif  ! if (DryDepState%SchemeOpt == 1)

   ! TO DO:  apply dry dep velocities/freq to chem species
   write(*,*) 'TODO: Need to figure out how to add back to the chemical species state '

endif   !  if (DryDepState%Activate)


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
