!> \file CCPr_Plumerise_mod.F90
!! \brief Driver for the CATCHem Process: Plumerise
!!
!! \defgroup catchem_plumerise_process
!!
!! The CATChem Plumerise Process group holds all the CATCHem Plumerise processes.
!!!>
MODULE CCPr_Plumerise_mod

   ! USES:
   USE State_Mod
   USE Error_MOD

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CCPR_Plumerise_Run
   PUBLIC :: CCPR_Plumerise_Init
   PUBLIC :: CCPR_Plumerise_Finalize
   PUBLIC :: PlumeRiseStateType

   TYPE :: PlumeRiseStateType
      Logical :: activate      !< Activate Plume Rise
   END TYPE PlumeRiseStateType

CONTAINS

   !>
   !! \brief Initialize the CATCHem Plumerise Process
   !!
   !! \param Config_Opt  CATCHem configuration options
   !! \param PlumeriseState   CATCHem Plumerise state
   !! \param ChmState    CATCHem chemical state
   !! \param RC          Error return code
   !!
   !! \ingroup catchem_plumerise_process
   !!!>
   SUBROUTINE CCPR_Plumerise_Init( Config, PlumeriseState, RC)
      ! USES

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(ConfigType),    intent(in)    :: Config     ! Config options
      TYPE(PlumeriseStateType), intent(inout) :: PlumeriseState  ! Nullify Plumerise State During INIT

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      INTEGER,          INTENT(INOUT) :: RC

      ! LOCAL VARIABLES
      !----------------
      INTEGER :: k ! Loop Counter

      ! Error handling
      !---------------
      CHARACTER(LEN=255)    :: ErrMsg
      CHARACTER(LEN=255)    :: ThisLoc

      ! Initialize Error handling
      !--------------------------
      ErrMsg = ''
      ThisLoc = ' -> at CCPR_Plumerise_INIT (in process/Plumerise/ccpr_Plumerise_mod.F90)'

      ! Initialize
      !-----------
      if (Config%Plumerise_activate) then

         ! Activate Plumerise Process
         !----------------------
         Plumerisestate%Activate = .true.

      else

         PlumeriseState%Activate = .false.

      endif


   END SUBROUTINE CCPR_Plumerise_INIT

   !>
   !! \brief Run the Plumerise scheme
   !!
   !! \param [IN] MetState The MetState object
   !! \param [INOUT] DiagState The DiagState object
   !! \param [INOUT] PlumeriseState The PlumeriseState object
   !! \param [INOUT] ChemState The ChemState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup catchem_plumerise_process
   !!!>
   SUBROUTINE CCPr_Plumerise_Run( MetState, GridState, PlumeriseState, ChemState, RC )

      ! USE
      USE ccpr_scheme_sofiev_mod, ONLY : CCPr_Sofiev_Plmrise
      use CCPR_Scheme_Briggs_Mod, only : CCPr_Briggs_Plumerise

      IMPLICIT NONE

      ! INPUT PARAMETERS
      !-----------------
      TYPE(MetStateType),  INTENT(IN) :: MetState       ! MetState Instance
      TYPE(GridStateType), INTENT(IN) :: GridState      ! GridState Instance

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(PlumeriseStateType), INTENT(INOUT) :: PlumeriseState   ! PlumeriseState Instance
      TYPE(ChemStateType), INTENT(INOUT) :: ChemState  ! ChemState Instance

      ! OUTPUT PARAMETERS
      !------------------
      INTEGER, INTENT(OUT) :: RC                         ! Return Code

      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc
      Integer :: c ! Emission Categories
      Integer :: s ! Emitted Species
      Integer :: p ! Plumerise source length counter
      Integer :: z ! Vertical counter


      REAL(fp) :: plmHGT                              ! Plumerise Height [m]
      REAL(fp) :: EFRAC(GridState%number_of_levels)   ! Emission fraction
      real(fp) :: ColEmis(GridState%number_of_levels) ! Column emission rate [kg/m2/s]

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_Plumerise_Run (in process/Plumerise/ccpr_Plumerise_mod.F90)'

      if (PlumeriseState%Activate) then

         if (EmisState%nEmisTotalPlumerise == 0) RETURN  ! no plumerise species listed in CATCHem_emission.yml

         do c = 1, EmisState%nEmisCategories

            ! EmisState%EmisCats(c) plumerise options activated
            if (EmisState%EmisCats(c)%nPlumerise /= 0) then  ! in EmisState%EmisCats(c) plumerise options activated
               do s = 1, EmisState%EmisCats(c)%nSpecies ! loop over emitted species
                  do p = 1, size(EmisState%EmisCats(c)%Species(s)%frp) ! loop over plume sources
                     if (EmisState%EmisCats(c)%Species(s)%plumerise == 1) then ! Sofiev Plumerise

                        call CCPr_Sofiev_PlmriseHgt(MetState%Z,      &
                           MetState%T,                               &
                           MetState%PMID,                            &
                           MetState%PBLH,                            &
                           MetState%PS,                              &
                           EmisState%EmisCats(c)%Species(s)%frp(p),  &
                           plmHGT,                                   &
                           EFRAC)
                        if (RC /= CC_SUCCESS) then
                           errMsg = 'Error in CCPr_Sofiev_PlmriseHgt'
                           CALL CC_Error( errMsg, RC, thisLoc )
                        endif

                        ! Add emission to ColEmis for total Emission in grid cell due to plumerise
                        ! Will Speciate out afterwards to concentration at end
                        do z = 1, GridState%number_of_levels
                           ColEmis(z) = ColEmis(z) + EmisState%EmisCats(c)%Species(s)%PlmSrcFlx(p) * EFRAC(z)
                        enddo

                     else if (EmisState%EmisCats(c)%Species(s)%plumerise == 2) then ! Brigg's Plumerise
                        call CCPr_Briggs_Plumerise(MetState%Z,          &
                           MetState%ZMID,                               &
                           MetState%T,                                  &
                           MetState%QV,                                 &
                           MetState%U,                                  &
                           MetState%V,                                  &
                           MetState%PMID,                               &
                           MetState%HFLUX,                              &
                           MetState%PBLH,                               &
                           MetState%USTAR,                              &
                           MetState%T2M,                                &
                           MetState%PS,                                 &
                           SIZE(MetState%T),                            &
                           EmisState%EmisCats(c)%Species(s)%STKDM(p),   &
                           EmisState%EmisCats(c)%Species(s)%STKHT(p),   &
                           EmisState%EmisCats(c)%Species(s)%STKTK(p),   &
                           EmisState%EmisCats(c)%Species(s)%STKVE(p),   &
                           plmHGT,                                      &
                           EFRAC)
                        if (RC /= CC_SUCCESS) then
                           errMsg = 'Error in CCPr_Briggs_Plumerise'
                           CALL CC_Error( errMsg, RC, thisLoc )
                        endif

                        do z = 1, GridState%number_of_levels
                           ColEmis(z) = ColEmis(z) + EmisState%EmisCats(c)%Species(s)%PlmSrcFlx(p) * EFRAC(z)
                        enddo

                     end if ! plume source loop
                  end do ! p - Plumerise Source Loop

                  ! Add emission to ColEmis to Species total flux in grid cell
                  EmisState%EmisCats(c)%Species(s)%EmisFlux = ColEmis

               enddo ! s - Species List Loop
            end if ! End Plumerise Condition within EmisState%EmisCats(c)
         enddo ! c - Category List Loop

      endif ! Activate

   END SUBROUTINE CCPr_Plumerise_Run

   !>
   !! \brief Finalize the Plumerise scheme
   !!
   !! \param [INOUT] PlumeriseState The PlumeriseState object
   !! \param [OUT] RC Return code
   !!
   !! \ingroup catchem_plumerise_process
   !!!>
   SUBROUTINE CCPr_Plumerise_Finalize( PlumeriseState, RC )

      ! USE
      !----

      IMPLICIT NONE

      ! INPUT/OUTPUT PARAMETERS
      !------------------------
      TYPE(PlumeriseStateType), INTENT(INOUT) :: PlumeriseState ! PlumeriseState Instance
      INTEGER, INTENT(OUT) :: RC                       ! Return Code

      ! LOCAL VARIABLES
      !----------------
      CHARACTER(LEN=255) :: ErrMsg, thisLoc

      ! Initialize
      !-----------
      RC = CC_SUCCESS
      errMsg = ''
      thisLoc = ' -> at CCPr_Plumerise_Finalize (in process/Plumerise/ccpr_Plumerise.F90)'

      ! Nothing to do yet

      RETURN

   END SUBROUTINE CCPr_Plumerise_Finalize

END MODULE CCPr_Plumerise_Mod
