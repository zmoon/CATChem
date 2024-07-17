!> \brief Template for a new process driver
!!
!! To use:
!! - replace <PROCESS> by an identifier for the process group (e.g. 'Dust')
!!
!! More in-depth description here
!!
!! \author Barry baker
!! \date 05/2024
!!!>
MODULE CCPR_Megan_mod
   USE Precision_mod
   USE Error_Mod
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod,  Only : MetStateType
   USE ChemState_Mod, Only : ChemStateType
   USE Config_Mod,    Only : ConfigType
   USE CCPr_Megan_Common_Mod, Only : MeganStateType

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: CCPR_Megan_Init
   PUBLIC :: CCPR_Megan_Run
   PUBLIC :: CCPR_Megan_Final

CONTAINS

   !>
   !! \brief Initialize the CATChem <PROCESS> module
   !!
   !! \param Config_Opt       CATCHem configuration options
   !! \param <PROCESS>State   CATCHem PROCESS state
   !! \param ChmState         CATCHem chemical state
   !! \param RC               Error return code
   !!
   !!!>
   SUBROUTINE CCPR_Megan_Init( Config, ChemState, MeganState, RC )
      ! USE

      IMPLICIT NONE
      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigOptType), POINTER    :: Config    ! Module options
      TYPE(ChemStateType), POINTER    :: ChemState ! Chemical state

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(MeganStateType), POINTER   :: MeganState ! <PROCESS> state
      INTEGER,         INTENT(INOUT)  :: RC         ! Success or failure

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! LOCAL VARIABLES
      !----------------

      ! Put any local variables here

      !=================================================================
      ! CCPR_Megan_Init begins here!
      !=================================================================
      RC = CC_SUCCESS
      ThisLoc = ' -> at CCPR_Megan_INIT (in process/megan/ccpr_megan_mod.F90)'

      ! First check if process is activated in config | if not don't allocate arrays or pointers
      if (Config%megan_activate) then

         ! Activate Process
         !------------------
         MeganState%Activate = .true.

         ! Set number of species
         !----------------------
         MeganState%nMeganSpecies = 21

         ! CO2 inhibition option
         !------------------
         MeganState%CO2Inhib = config%CO2_Inhib_Opt

         ! Set CO2 concentration (ppm)
         !----------------------------
         if (Config%CO2_conc < 0) then ! not listed in config
            MeganState%CO2conc = 390.0_fp
         else
            MeganState%CO2conc = Config%CO2_conc_ppm
         endif

         ! Check GLOBCO2 if CO2 inhibition is turned on (LISOPCO2 = .TRUE.)
         ! GLOBCO2 should be between 150-1250 ppmv. Isoprene response to
         ! CO2 outside this range has no empirical basis.
         if ( MeganState%CO2Inhib ) then
            if ( MeganState%CO2conc <  150.0_fp .or. &
               MeganState%CO2conc > 1250.0_fp     ) then
               RC = CC_FAILURE
               ErrMsg = 'Global CO2 outside valid range of 150-1250 ppmv!'
               call CC_Error( errMsg, RC, thisLoc )
               return
            endif
         endif

         ! Allocate any arrays here for scheme to run
         ALLOCATE( MeganState%MeganSpeciesIndex(MeganState%nMeganSpecies) )
         CALL CC_CheckVar('MeganState%MeganSpeciesIndex', 0, RC)  
         IF (RC /= CC_SUCCESS) RETURN
         
         ! Allocate any arrays here for scheme to run
         ALLOCATE( MeganState%MeganSpeciesName(MeganState%nMeganSpecies) )
         CALL CC_CheckVar('MeganState%MeganSpeciesName', 0, RC)  
         IF (RC /= CC_SUCCESS) RETURN

         ! Allocate any arrays here for scheme to run
         ALLOCATE( MeganState%EmissionPerSpecies(MeganState%nMeganSpecies) )
         CALL CC_CheckVar('MeganState%EmissionPerSpecies', 0, RC)  
         IF (RC /= CC_SUCCESS) RETURN

         !TODO: emission species name and ID should read from a namelist. Give them values for now
         MeganState%MeganSpeciesIndex = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21/)
         MeganState%MeganSpeciesName(/'ISOP','APIN','BPIN','LIMO','SAIB','MYRC','CARE',
                                      'OCIM','OMON','ALD2','MOH', 'EOH', 'MBOX','FAXX',
                                      'AAXX','ACET','PRPE','C2H4','FARN','BCAR','OSQT' /)

      else

         MeganState%Activate = .false.

      endif

   end subroutine CCPR_Megan_Init

   !>
   !! \brief Run the <Process>
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] DustState The DustState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_Dust_Run( MetState, DiagState, <PROCESS>State, ChemState, RC )

      ! USE

      IMPLICIT NONE
      ! INPUT PARAMETERS
      TYPE(MetState_type),  INTENT(IN) :: MetState       ! MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      TYPE(DiagState_type), INTENT(INOUT)      :: DiagState       ! DiagState Instance
      TYPE(<PROCESS>State_type), INTENT(INOUT) :: <PROCESS>State  ! <PROCESS>State Instance
      TYPE(ChemState_type),  INTENT(INOUT)     :: ChemState       ! ChemState Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                  ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_<PROCESS>_Run (in process/<PROCESS>/ccpr_<PROCESS>_mod.F90)'

      ! Run the <PROCESS> Scheme
      !-------------------------
      if (<PROCESS>State%Activate) then
         ! Run the <PROCESS> Scheme
         !-------------------------
         if (<PROCESS>State%SchemeOpt == 1) then
            ! Run the <PROCESS> Scheme
            !-------------------------
            CCPr_<Process>_Scheme_<SCHEME_NAME>( MetState, DiagState, <PROCESS>State, ChemState, RC)

         elseif (<PROCESS>State%SchemeOpt == 2) then
            ! Run the <PROCESS> Scheme
            !-------------------------
            CCPr_<Process>_Scheme_<2ndSCHEME_NAME>( MetState, DiagState, <PROCESS>State, ChemState, RC)
         endif

      endif

   end subroutine CCPr_<PROCESS>_Run

   !>
   !! \brief Finalize the <Process>
   !!
   !! \param [INOUT] <Process>State
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_<PROCESS>_Final( <PROCESS>State, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      TYPE(<PROCESS>State_type), INTENT(INOUT) :: <PROCESS>State  ! <PROCESS>State Instance

      ! OUTPUT PARAMETERS
      INTEGER, INTENT(OUT) :: RC                                  ! Return Code

      ! LOCAL VARIABLES
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_<PROCESS>_Final (in process/<PROCESS>/ccpr_<PROCESS>_mod.F90)'

      ! Deallocate any arrays here
      DELLOCATE( <PROCESS>State%SpcIDs, STAT=RC )
      CALL CC_CheckVar('<PROCESS>State%SpcIDs', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN
      <PROCESS>State%SpcIDs => NULL()

   end subroutine CCPr_<PROCESS>_Final

END MODULE CCPR_Megan_Mod
