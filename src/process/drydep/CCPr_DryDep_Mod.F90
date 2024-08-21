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
      INTEGER                         :: Scheme             ! Scheme Option (if there is only one SchemeOpt always = 1)
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
         DryDepState%Scheme = config%DryDep_scheme

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
         if (DryDepState%Scheme == 1) then
            ! Run the DryDep Scheme - Only Applicable to AEROSOL species
            !-------------------------
            if (ChemState%nSpeciesAero > 0) then

               call PrepMetVarsForGOCART(MetState, tmpu, rhoa, hghte, lwi, ustar, &
                  pblh, shflux, z0h, u10m, v10m, fraclake, gwettop, rc)

               ! loop through aerosol species
               do i = 1, ChemState%nSpeciesAero
                  ! cycle if not a drydep species
                  if (ChemState%ChemSpecies(ChemState%AeroIndex(i))%is_drydep .eqv. .false.) cycle

                  radius = ChemState%chemSpecies(ChemState%AeroIndex(i))%radius
                  rhop = ChemState%chemSpecies(ChemState%AeroIndex(i))%density
                  if (DryDepState%resuspension) then
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
                     endif
                  else
                     call CCPr_Scheme_GOCART_DryDep( km,          &
                        tmpu,        &
                        rhoa,        &
                        hghte,       &
                        lwi,         &
                        ustar,       &
                        pblh,        &
                        shflux,      &
                        von_karman,  &
                        cp,          &
                        g0,          &
                        z0h,         &
                        drydepf,     &
                        rc)
                     if (RC /= CC_SUCCESS) then
                        errMsg = 'Error in GOCART DryDeposition'
                        CALL CC_Error( errMsg, RC, thisLoc )
                     endif

                  endif

                  ! Fill Diagnostic Variables
                  !--------------------------
                  !!!!FIXME: COME BACK TO THIS LATER
                  !DiagState%drydep_frequency(ChemState%AeroIndex(i)) = drydepf(1,1)
                  !DiagState%drydep_vel(ChemState%AeroIndex(i)) = MetState%ZMID(1) * drydepf(1,1)

                  ! apply drydep velocities/freq to chem species
                  dqa = 0.
                  dqa = MAX(0.0_fp, ChemState%chemSpecies(ChemState%AeroIndex(i))%conc(1)   &
                     * (1.-exp(-drydepf(1,1) * MetState%TSTEP)))
                  ChemState%chemSpecies(ChemState%AeroIndex(i))%conc(1) =     &
                     ChemState%chemSpecies(ChemState%AeroIndex(i))%conc(1) - dqa
               end do ! do i = 1, ChemState%nSpeciesAero

            endif ! if (ChemState%nSpeciesAero > 0)

         endif ! if (DryDepState%Scheme == 1)

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

   !>
   !! \brief PrepMetVarsForGOCART - Prep the meteorological variables for GOCART DryDeposition scheme
   !!
   !! \param [INOUT] metstate
   !! \param [INOUT] tmpu
   !! \param [INOUT] rhoa
   !! \param [INOUT] hghte
   !! \param [INOUT] oro
   !! \param [INOUT] ustar
   !! \param [INOUT] pblh
   !! \param [INOUT] shflux
   !! \param [INOUT] z0h
   !! \param [INOUT] u10m
   !! \param [INOUT] v10m
   !! \param [INOUT] fraclake
   !! \param [INOUT] gwettop
   !! \param [OUT] rc
   !!
   !! \ingroup core_modules
   !!!>
   subroutine PrepMetVarsForGOCART(metstate, tmpu, rhoa, hghte, oro, ustar, pblh, shflux, z0h, u10m, v10m, fraclake, gwettop, rc)
      use MetState_Mod, only: MetStateType

      IMPLICIT NONE

      ! INPUTS
      type(MetStateType), intent(in) :: MetState                   !< Meteorological state

      ! INPUT/OUTPUTS
      REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: tmpu   !< temperature [K]
      REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: rhoa   !< air density [kg/m^3]
      REAL, intent(inout), allocatable, DIMENSION(:,:,:) :: hghte  !< geometric height [m]
      REAL, DIMENSION(1,1), intent(inout) :: u10m                  !< 10-m u-wind component [m/sec]
      REAL, DIMENSION(1,1), intent(inout) :: v10m                  !< 10-m v-wind component [m/sec]
      REAL, DIMENSION(1,1), intent(inout) :: fraclake              !< fraction covered by water [1]
      REAL, DIMENSION(1,1), intent(inout) :: gwettop               !< fraction soil moisture [1]
      INTEGER, intent(inout) :: lwi                                !< orography flag; Land, ocean, ice mask
      REAL,    intent(inout) :: ustar                              !< friction speed [m/sec]
      REAL,    intent(inout) :: pblh                               !< PBL height [m]
      REAL,    intent(inout) :: hflux                             !< sfc. sens. heat flux [W m-2]
      REAL,    intent(inout) :: z0h                                !< rough height, sens. heat [m]

      ! OUTPUTS
      INTEGER :: rc !< Return code


      ! LOCAL VARIABLES
      INTEGER :: km

      ! Error handling
      character(len=255) :: errMsg
      character(len=255) :: thisloc

      ! Initialize
      rc = CC_SUCCESS
      errMsg = ''
      thisloc = ' -> at PrepMetVarsForGOCART (in process/drydep/ccpr_DryDep_mod.F90)'

      km  = MetState%NLEVS

      if (.not. allocated(tmpu)) then
         allocate(tmpu(1,1, km), stat=rc)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Could not allocate tmpu'
            CALL CC_Error( errMsg, RC, thisLoc )
         endif
      endif

      if (.not. allocated(rhoa)) then
         allocate(rhoa(1,1,km), stat=rc)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Could not allocate rhoa'
            CALL CC_Error( errMsg, RC, thisLoc )
         endif
      endif

      if (.not. allocated(hghte)) then
         allocate(hghte(1,1,km), stat=rc)
         if (RC /= CC_SUCCESS) then
            errMsg = 'Could not allocate hghte'
            CALL CC_Error( errMsg, RC, thisLoc )
         endif
      endif

      tmpu = reshape(metstate%T, (/1, 1, km/))         ! temperature [K]
      rhoa = reshape(metstate%AIRDEN, (/1, 1, km/)) ! air density [kg/m^3]
      hghte = reshape(metstate%ZMID, (/1, 1, km/))    ! top of layer geopotential height [m]
      lwi    = metstate%LWI       ! orography flag; Land, ocean, ice mask
      ustar  = metstate%ustar     ! friction speed [m/sec]
      pblh   = metstate%pblh      ! PBL height [m]
      hflux = metstate%hflux     ! sfc. sens. heat flux [W m-2]
      z0h    = metstate%z0h       ! rough height, sens. heat [m]

   end subroutine PrepMetVarsForGOCART


END MODULE CCPR_DryDep_Mod
