!> \file CCPr_SeaSalt_mod.F90
!! \brief Driver for the CATCHem Process: SeaSalt
!!
!!!>
MODULE CCPr_SeaSalt_mod

   ! USES:
   USE Precision_Mod
   USE Error_MOD
   USE DiagState_Mod, Only : DiagStateType
   USE MetState_Mod, Only : MetStateType
   USE Config_Opt_Mod, Only : ConfigType
   USE ChemState_Mod, Only : ChemStateType
   USE CCPr_SeaSalt_Common_Mod, Only : SeaSaltStateType

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CCPR_SeaSalt_Run
   PUBLIC :: CCPR_SeaSalt_Init
   PUBLIC :: CCPR_SeaSalt_Finalize

CONTAINS

   !>
   !! \brief Initialize the CATCHem SeaSalt Process
   !!
   !! \param Config_Opt  CATCHem configuration options
   !! \param SeaSaltState   CATCHem SeaSalt state
   !! \param ChmState    CATCHem chemical state
   !! \param RC          Error return code
   !!
   !!!>
   SUBROUTINE CCPR_SeaSalt_Init( Config, SeaSaltState, ChemState, RC)
      ! USES

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType),    intent(in)    :: Config     ! Config options
      TYPE(SeaSaltStateType), intent(inout) :: SeaSaltState  ! Nullify SeaSalt State During INIT
      TYPE(ChemStateType), intent(in)    :: ChemState  ! Chemical State

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      INTEGER,          INTENT(INOUT) :: RC

      ! LOCAL VARIABLES
      !----------------
      Integer, parameter :: nSeaSaltBinsDefault = 5
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultSeaSaltDensity  = (/ 2200., &
         2200., &
         2200., &
         2200., &
         2200. /)
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultEffectiveRadius = (/ 0.079e-6, &
         0.316e-6, &
         1.119e-6, &
         2.818e-6, &
         7.772e-6 /)
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultLowerBinRadius  = (/ 0.03e-6, &
         0.1e-6,  &
         0.5e-6,  &
         1.5e-6,  &
         5.0e-6  /)
      REAL(fp), DIMENSION(nSeaSaltBinsDefault), Parameter :: DefaultUpperBinRadius  = (/ 0.1e-6, &
         0.5e-6, &
         1.5e-6, &
         5.0e-6, &
         10.0e-6 /)

      INTEGER :: k ! Loop Counter

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! Initialize Error handling
      !--------------------------
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_SeaSalt_INIT (in process/SeaSalt/ccpr_SeaSalt_mod.F90)'

      ! Initialize
      !-----------
      if (Config%seasalt_activate) then

         ! Activate SeaSalt Process
         !----------------------
         SeaSaltState%Activate = .true.

         ! Set number of seasalt species
         !---------------------------
         SeaSaltState%nSeaSaltSpecies = ChemState%nSpeciesSeaSalt

         ! Set Scheme Options
         !-------------------
         SeaSaltState%SchemeOpt = Config%seasalt_scheme

         ! Set Tuning Scale Factor | Default = 1 if not set
         !-------------------------------------------------
         SeaSaltState%SeaSaltScaleFactor = Config%seasalt_scalefactor

         if (SeaSaltState%nSeaSaltSpecies == 0) then

            ! Set default bin properties for schemes that need them
            !------------------------------------------------------
            ALLOCATE(SeaSaltState%LowerBinRadius(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%LowerBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%LowerBinRadius(k) = DefaultLowerBinRadius(k)
            end do

            ALLOCATE(SeaSaltState%UpperBinRadius(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%UpperBinRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%UpperBinRadius(k) = DefaultUpperBinRadius(k)
            enddo

            ALLOCATE(SeaSaltState%EffectiveRadius(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%EffectiveRadius', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%EffectiveRadius(k) = DefaultEffectiveRadius(k)
            end do

            ALLOCATE(SeaSaltState%SeaSaltDensity(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%SeaSaltDensity', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%SeaSaltDensity(k) = DefaultSeaSaltDensity(k)
            end do

            ALLOCATE(SeaSaltState%EmissionPerSpecies(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('EmissionPerSpecies', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%EmissionPerSpecies(k) = ZERO
            end do

            ALLOCATE(SeaSaltState%NumberEmissionBin(nSeaSaltBinsDefault), STAT=RC)
            CALL CC_CheckVar('SeaSaltState%NumberEmissionBin', 0, RC)
            IF (RC /= CC_SUCCESS) RETURN
            do k = 1, nSeaSaltBinsDefault
               SeaSaltState%NumberEmissionBin(k) = ZERO
            end do

         else

            ! seasalt Aerosols are present in ChmState
            !--------------------------------------

            !TODO: Need to figure out how exactly to do this at the moment
            write(*,*) 'TODO: Need to figure out how to add back to the chemical species state '

         endif

      else

         SeaSaltState%Activate = .false.

      endif


   END SUBROUTINE CCPR_SeaSalt_INIT

   !>
   !! \brief Run the seasalt scheme
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] SeaSaltState The SeaSaltState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_SeaSalt_Run( MetState, DiagState, SeaSaltState, ChemState, RC )

      ! USE
      USE CCPr_Scheme_Gong03_mod,  ONLY: CCPr_Scheme_Gong03   ! Ginoux SeaSalt Scheme

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(DiagStateType), INTENT(INOUT)    :: DiagState   ! DiagState Instance
      TYPE(SeaSaltStateType), INTENT(INOUT) :: SeaSaltState   ! SeaSaltState Instance
      TYPE(ChemStateType), INTENT(INOUT)    :: ChemState  ! ChemState Instance

      ! OUTPUT PARAMETERS
      !------------------
      INTEGER, INTENT(OUT) :: RC                         ! Return Code


      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_SeaSalt_Run (in process/seasalt/ccpr_seasalt_mod.F90)'

      if (SeaSaltState%Activate) then

         ! Run the SeaSalt Scheme
         !--------------------
         if (SeaSaltState%SchemeOpt == 1) then ! FENGSHA
            call CCPr_Scheme_Gong03( MetState, DiagState, SeaSaltState, RC )
            if (RC /= CC_SUCCESS) then
               errMsg = 'Error in CCPr_Scheme_Gong03'
               CALL CC_Error( errMsg, RC, thisLoc )
            endif
            !   else if (SeaSaltState%SchemeOpt == 2) then ! GINOUX
            !      call CCPr_Scheme_Ginoux( MetState, DiagState, SeaSaltState, RC )
            !      if (RC /= CC_SUCCESS) then
            !         errMsg = 'Error in CCPr_Scheme_Ginoux'
            !         CALL CC_Error( errMsg, RC, thisLoc )
            !      endif
         else
            write(*,*) 'ERROR: Unknown seasalt scheme option'
         endif
      endif



   END SUBROUTINE CCPr_SeaSalt_Run

   !>
   !! \brief Finalize the seasalt scheme
   !!
   !! \param [INOUT] SeaSaltState The SeaSaltState object
   !! \param [OUT] RC Return code
   !!!>
   SUBROUTINE CCPr_SeaSalt_Finalize( SeaSaltState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(SeaSaltStateType), INTENT(INOUT) :: SeaSaltState ! SeaSaltState Instance
      INTEGER, INTENT(OUT) :: RC                       ! Return Code

      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_SeaSalt_Finalize (in process/seasalt/ccpr_SeaSalt.F90)'

      DEALLOCATE( SeaSaltState%LowerBinRadius, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%LowerBinRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%UpperBinRadius, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%UpperBinRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%EffectiveRadius, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%EffectiveRadius', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%SeaSaltDensity, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%SeaSaltDensity', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

      DEALLOCATE( SeaSaltState%EmissionPerSpecies, STAT=RC )
      CALL CC_CheckVar('SeaSaltState%EmissionPerSpecies', 0, RC)
      IF (RC /= CC_SUCCESS) RETURN

   END SUBROUTINE CCPr_SeaSalt_Finalize

END MODULE CCPr_SeaSalt_Mod
