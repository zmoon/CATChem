!> \file CCPr_Dust_mod.F90
!! \brief Driver for the CATCHem Process: Dust
!!
!!!>
MODULE CCPr_Dust_mod
    
      ! USES:
      USE Precision_Mod   
      USE Error_MOD
      USE DiagState_Mod, Only : DiagStateType
      USE MetState_Mod, Only : MetStateType
      USE Config_Opt_Mod, Only : ConfigType

      IMPLICIT NONE
      PRIVATE
    !
    ! !PUBLIC MEMBER FUNCTIONS:
    !
      PUBLIC :: CCPR_<yourmame>_Run
      PUBLIC :: CCPR_<yourname>_Init
      PUBLIC :: CCPR_<yourname>_Final
    
      !> \brief Type for CATCHem Dust Process
      !!
      !! \details Contains all the information needed to run the CATCHem Dust Process
      !! 
      !! This type contains the following variables:
      !! - Activate : Activate Process (True/False)
      !! - nDustSpecies : Number of dust processes
      !! - SchemeOpt : Scheme Option
      !! - DustSpeciesIndex : Index of dust species
      !! - SpcIDs : CATChem species IDs
      !! - LowerBinRadius : Lower bin radius        [m]
      !! - UpperBinRadius : Upper bin radius        [m]
      !! - EffectiveRadius : Effective radius        [m]
      !! - DustDensity : Dust density            [kg/m^3]
      !! - BetaScaleFactor : Beta Scaling Parameter  [1]
      !! - AlphaScaleFactor : Alpha Scaling Parameter [1]
      !! - TotalEmission : Total emission          [kg/m^2/s]
      !! - EmissionRate : Emission rate            [kg/m^2/s]
      !! - FengshaMoistureOpt : Fengsha-Moisture Calculation Option
      !! - FengshaDragOpt : Fengsha-Drag Calculation Option
      !!!>
      TYPE :: DustStateType
       ! Generic Variables for Every Process
       Logical                         :: Activate            ! Activate Process (True/False)
       INTEGER                         :: nDustSpecies        ! Number of dust processes
       INTEGER                         :: SchemeOpt           ! Scheme Option
       INTEGER                         :: DustSpeciesIndex(:) ! Index of dust species
       INTEGER,  ALLOCATABLE           :: SpcIDs(:)           ! CATChem species IDs

       ! Process Specific Parameters
       REAL(fp), ALLOCATABLE           :: LowerBinRadius(:)         ! Lower bin radius        [m]
       REAL(fp), ALLOCATABLE           :: UpperBinRadius(:)         ! Upper bin radius        [m]
       REAL(fp), ALLOCATABLE           :: EffectiveRadius(:)        ! Effective radius        [m]
       REAL(fp), ALLOCATABLE           :: DustDensity(:)            ! Dust density            [kg/m^3]
       REAL(fp)                        :: BetaScaleFactor           ! Beta Scaling Parameter  [1]
       REAL(fp)                        :: AlphaScaleFactor          ! Alpha Scaling Parameter [1]
       REAL(fp), ALLOCATABLE           :: TotalEmission             ! Total emission          [kg/m^2/s]
       REAL(fp), ALLOCATABLE           :: EmissionPerSpecies(:)     ! Emission per species    [kg/m^2/s]

       ! Scheme Options 
       INTEGER                         :: FengshaMoistureOpt  ! Fengsha-Moisture Calculation Option
       INTEGER                         :: FengshaDragOpt      ! Fengsha-Drag Calculation Option

        !=================================================================
        ! Module specific variables/arrays/data pointers come below
        !=================================================================

      END TYPE DustStateType

    CONTAINS
    
      !>
      !! \brief Initialize the CATCHem Dust Process
      !!
      !! \param Config_Opt  CATCHem configuration options
      !! \param DustState   CATCHem dust state
      !! \param ChmState    CATCHem chemical state
      !! \param RC          Error return code
      !!
      !!!>
      SUBROUTINE CCPR_Dust_Init( Config, DustState, ChmState, RC)
        ! USES

        IMPLICIT NONE

        ! INPUT PARAMETERS
        !-----------------
        TYPE(ConfigType),    POINTER :: Config  ! Config options
        TYPE(ChmStateType),  POINTER :: ChmState    ! Chemical State
        TYPE(DustStateType), POINTER :: DustState => NULL()
    
        ! INPUT/OUTPUT PARAMETERS
        !------------------------
        INTEGER,          INTENT(INOUT) :: RC
    
        ! LOCAL VARIABLES
        !----------------
        Integer, parameter :: nDustBinsDefault = 5
        REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultDustDensity     = (/ 2500., 2650., 2650., 2650., 2650. /)
        REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultEffectiveRadius = (/ 0.73D-6, 1.4D-6, 2.4D-6, 4.5D-6, 8.0D-6 /)
        REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultLowerBinRadius  = (/ 0.1D-6, 1.0D-6, 1.8D-6, 3.0D-6, 6.0D-6  /)
        REAL(fp), DIMENSION(nDustBinsDefault), Parameter :: DefaultUpperBinRadius  = (/ 1.0D-6, 1.8D-6, 3.0D-6, 6.0D-6,10.0D-6  /)

        ! Error handling
        !---------------
        CHARACTER(LEN=255)    :: ErrMsg
        CHARACTER(LEN=255)    :: ThisLoc

        ! Initialize Error handling
        !--------------------------
        ErrMsg = ''
        ThisLoc = ' -> at CCPR_DUST_INIT (in process/dust/ccpr_dust_mod.F90)'

        ! Initialize
        !-----------
        if (Config%dust_activate) then 

            ! Activate Dust Process
            !----------------------
            duststate%Activate = .true.

            ! Set number of dust species
            !---------------------------
            DustState%nDustSpecies = ChmState%nDust

            ! Set Scheme Options
            !-------------------
            duststate%SchemeOpt = Config%dust_scheme

            ! Set Drag Calculation Option
            !----------------------------
            DustState%DragOpt = Config%dust_drag

            ! Set Moisture Calculation Option
            !--------------------------------
            DustState%MoistOpt = Config%dust_moisture

            ! Set Horizontal Flux Calculation Option
            !---------------------------------------
            DustState%HorizFluxOpt = Config%dust_horizflux

            if (DustState%nDustSpecies == 0) then

                ! Set default bin properties for schemes that need them 
                !------------------------------------------------------
                ALLOCATE(DustState%LowerBinRadius(nDustBinsDefault), STAT=RC)
                CALL CC_CheckVar('DustState%LowerBinRadius', 0, RC)
                IF (RC /= CC_SUCCESS) RETURN
                do k in 1, nDustBinsDefault 
                    DustState%LowerBinRadius(k) = DefaultLowerBinRadius(k)
                end do  

                ALLOCATE(DustState%UpperBinRadius(nDustBinsDefault), STAT=RC)
                CALL CC_CheckVar('DustState%UpperBinRadius', 0, RC)
                IF (RC /= CC_SUCCESS) RETURN
                do k in 1, nDustBinsDefault 
                    DustState%UpperBinRadius(k) = DefaultUpperBinRadius(k)
                end do

                ALLOCATE(DustState%EffectiveRadius(nDustBinsDefault), STAT=RC)
                CALL CC_CheckVar('DustState%EffectiveRadius', 0, RC)
                IF (RC /= CC_SUCCESS) RETURN
                do k in 1, nDustBinsDefault 
                    DustState%EffectiveRadius(k) = DefaultEffectiveRadius(k)
                end do  

                ALLOCATE(DustState%DustDensity(nDustBinsDefault), STAT=RC)
                CALL CC_CheckVar('DustState%DustDensity', 0, RC)
                IF (RC /= CC_SUCCESS) RETURN
                do k in 1, nDustBinsDefault 
                    DustState%DustDensity(k) = DefaultDustDensity(k)
                end do

                ALLOCATE(DustEmissionPerSpecies(nDustBinsDefault), STAT=RC)
                CALL CC_CheckVar('DustEmissionPerSpecies', 0, RC)
                IF (RC /= CC_SUCCESS) RETURN
                do k in 1, nDustBinsDefault 
                    DustState%EmissionPerSpecies(k) = 0.0_fp
                end do

            else 

                ! Dust Aerosols are present in ChmState
                !--------------------------------------

                !TODO: Need to figure out how exactly to do this at the moment
                write(*,*) 'TODO: Need to figure out how to add back to the chemical species state '

            endif                

        else

            DustState%Activate = .false.

        endif


    END SUBROUTINE CCPR_DUST_INIT

    !>
    !! \brief Run the dust scheme
    !!
    !! \param [IN] MetState The MetState object
    !! \param [INOUT] DiagState The DiagState object
    !! \param [INOUT] DustState The DustState object
    !! \param [INOUT] ChemState The ChemState object
    !! \param [OUT] RC Return code
    !!!>
    SUBROUTINE CCPr_Dust_Run( MetState, DiagState, DustState, ChemState, RC )

        ! USE
        USE CCPr_Scheme_Fengsha_Mod, ONLY: CCPr_Dust_Scheme_Fengsha  ! Fengsha Dust Scheme

        IMPLICIT NONE

        ! INPUT PARAMETERS
        !-----------------
        TYPE(MetState_type),  INTENT(IN) :: MetState       ! MetState Instance
        
        ! INPUT/OUTPUT PARAMETERS
        !------------------------
        TYPE(DiagState_type), INTENT(INOUT) :: DiagState   ! DiagState Instance
        TYPE(DustState_type), INTENT(INOUT) :: DustState   ! DustState Instance
        TYPE(ChemState_type),  INTENT(INOUT) :: ChemState  ! ChemState Instance

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
        thisLoc = ' -> at CCPr_Dust_Run (in process/dust/ccpr_dust_mod.F90)'

        if (DustState%Activate) then

            ! Run the Dust Scheme
            !--------------------
            if (DustState%SchemeOpt == 1) then
                CCPr_Dust_Scheme_Fengsha( MetState, DiagState, DustState, ChemState, RC)
            else
                write(*,*) 'TODO: Add other dust schemes'
            endif
        endif 

    END SUBROUTINE CCPr_Dust_Run

    !>
    !! \brief Finalize the dust scheme
    !!
    !! \param [INOUT] DustState The DustState object
    !! \param [OUT] RC Return code
    !!!>
    SUBROUTINE CCPr_Dust_Finalize( DustState, RC )

        ! USE
        !----
        
        IMPLICIT NONE
        
        ! INPUT/OUTPUT PARAMETERS
        !------------------------
        TYPE(DustState_type), INTENT(INOUT) :: DustState ! DustState Instance
        INTEGER, INTENT(OUT) :: RC                       ! Return Code

        ! LOCAL VARIABLES
        !----------------
        CHARACTER(LEN=255) :: ErrMsg, thisLoc

        ! Initialize
        !-----------
        RC = CC_SUCCESS
        errMsg = ''
        thisLoc = ' -> at CCPr_Dust_Finalize (in process/dust/ccpr_dust.F90)'

        DELLOCATE( DustState%LowerBinRadius, STAT=RC )
        CALL CC_CheckVar('DustState%LowerBinRadius', 0, RC)
        IF (RC /= CC_SUCCESS) RETURN
        DustState%LowerBinRadius => NULL()

        DELLOCATE( DustState%UpperBinRadius, STAT=RC )
        CALL CC_CheckVar('DustState%UpperBinRadius', 0, RC)
        IF (RC /= CC_SUCCESS) RETURN
        DustState%UpperBinRadius => NULL()

        DELLOCATE( DustState%EffectiveRadius, STAT=RC )
        CALL CC_CheckVar('DustState%EffectiveRadius', 0, RC)
        IF (RC /= CC_SUCCESS) RETURN
        DustState%EffectiveRadius => NULL()

        DELLOCATE( DustState%DustDensity, STAT=RC )
        CALL CC_CheckVar('DustState%DustDensity', 0, RC)
        IF (RC /= CC_SUCCESS) RETURN
        DustState%DustDensity => NULL()

        DEALLOCATE( DustState%EmissionPerSpecies, STAT=RC )
        CALL CC_CheckVar('DustState%EmissionPerSpecies', 0, RC)
        IF (RC /= CC_SUCCESS) RETURN
        DustState%EmissionPerSpecies => NULL()

    END SUBROUTINE CCPr_Dust_Finalize

END MODULE CCPr_Dust_Mod    
