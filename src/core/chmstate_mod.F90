module ChmState_Mod
    !
    ! USES:
    !
    USE Error_Mod
    USE Precision_Mod

    IMPLICIT NONE
    PRIVATE
    !
    ! !PUBLIC MEMBER FUNCTIONS:
    PUBLIC :: Chm_Allocate
    !
    ! !Private DATA MEMBERS:
    !
    !=========================================================================
    !

    type, public :: ChmStateType
        !---------------------------------------------------------------------
        ! Name of variables containing chemistry information
        !---------------------------------------------------------------------
        CHARACTER(LEN=4)  :: State     = 'Chem'    ! Name of this state

        !---------------------------------------------------------------------
        ! Integers
        !---------------------------------------------------------------------
        INTEGER           :: nSpecies
        INTEGER           :: nSpeciesGas
        INTEGER           :: nSpeciesAero
        INTEGER           :: nSpeciesDust
        INTEGER           :: NSpeicesSeaSalt
        INTEGER, POINTER  :: AeroIndex(:)
        INTEGER, POINTER  :: GasIndex(:)
        INTEGER, POINTER  :: DustIndex(:)
        INTEGER, POINTER  :: SeaSaltIndex(:)

        !---------------------------------------------------------------------
        ! Reals
        !---------------------------------------------------------------------
        REAL(fp), POINTER :: ChmSpecies(:,:,:,:)

    end type ChmStateType

    CONTAINS
    !
    !=========================================================================
    !
    subroutine Chm_Allocate(Config_Opt, State_Grid, State_Chm, RC)

        ! USES
        USE Config_Opt_Mod, ONLY : OptConfig
        USE GridState_Mod,  ONLY : GridStateType
        USE ChmState_Mod,   ONLY : ChmStateType
