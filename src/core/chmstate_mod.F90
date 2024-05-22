!> \defgroup ChmState_Mod ChmState Module
!! \brief Contains the ChmStateType data type and related subroutines and functions.
!!
!! The ChmState_Mod module contains the ChmStateType data type and related subroutines and functions for managing the state of the chemical model.
!!
!! \author Barry baker
!! \date 05/2023

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
      INTEGER           :: nSpecies          ! Total Number of Species
      INTEGER           :: nSpeciesGas       ! Number of Gas Species
      INTEGER           :: nSpeciesAero      ! Number of Aerosol Species
      INTEGER           :: nSpeciesDust      ! Number of Dust Species
      INTEGER           :: NSpeicesSeaSalt   ! Number of SeaSalt Species
      Integer, POINTER  :: SpeciesIndex(:)   ! Total Species Index
      INTEGER, POINTER  :: AeroIndex(:)      ! Aerosol Species Index
      INTEGER, POINTER  :: GasIndex(:)       ! Gas Species Index
      INTEGER, POINTER  :: DustIndex(:)      ! Dust Species Index
      INTEGER, POINTER  :: SeaSaltIndex(:)   ! SeaSalt Species Index

      !---------------------------------------------------------------------
      ! Reals
      !---------------------------------------------------------------------
      REAL(fp), POINTER :: ChmSpecies(:,:,:,:)

      ! TODO: Add properties for species
   end type ChmStateType

CONTAINS
   !
   !=========================================================================
   !
   subroutine Chm_Allocate(Config_Opt, GridState, State_Species,State_Chm, RC)

      ! USES
      USE Config_Opt_Mod, ONLY : OptConfig
      USE GridState_Mod,  ONLY : GridStateType
      USE Species_Mod,    Only : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(OptConfig),     INTENT(in)    :: Config_Opt    ! Input Options object
      type(GridStateType), INTENT(in)    :: GridState    ! Grid State object
      type(ChmStateType),  INTENT(inout) :: State_Chm     ! Chm State object
      type(SpeciesType),   POINTER       :: State_Species ! Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Chm_Allocate (in core/chmstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      State_Chm%ChmSpecies => NULL()

      ! Allocate
      ALLOCATE( State_Chm%ChmSpecies( GridState%NX, GridState%NY, &
         GridState%number_of_levels, State_Chm%nSpecies ), &
         STAT=RC )
      CALL CC_CheckVar( 'ChmState%ChmSpecies', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Chm%ChmSpecies = 1e-31_fp

      ! do other checks etc

   end subroutine Chm_Allocate
   !
   !=========================================================================
   !
   subroutine Find_Number_of_Species(State_Chm, State_Species, RC)
      ! USES
      USE Species_Mod,  ONLY : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ChmStateType),  INTENT(inout) :: State_Chm     ! Chm State object
      type(SpeciesType),   POINTER       :: State_Species ! Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Find_Number_of_Species (in core/chmstate_mod.F90)'

      ! do nothing yet
      ! loop through Species etc and find the number of species


   end subroutine Find_Number_of_Species

   !
   !=========================================================================
   !
   subroutine Find_Index_of_Species(State_Chm, State_Species, RC)
      ! USES
      USE Species_Mod,  ONLY : SpeciesType

      IMPLICIT NONE

      ! INOUT Params
      type(ChmStateType),  INTENT(inout) :: State_Chm     ! Chm State object
      type(SpeciesType),   POINTER       :: State_Species ! Species object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC            ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = CC_SUCCESS
      ErrMsg = ''
      thisLoc = ' -> at Find_Index_of_Species (in core/chmstate_mod.F90)'

      ! do nothing yet
      ! loop through Species etc and find the number of species


   end subroutine Find_Index_of_Species

end module ChmState_Mod
