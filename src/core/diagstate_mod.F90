!> \brief Contains the DiagStateType and Diag_Allocate subroutine
module DiagState_Mod
   ! Uses
   USE Precision_Mod
   USE Error_Mod

   IMPLICIT NONE
   private

   ! PUBLIC :: Zero_DiagState
   PUBLIC :: Diag_Allocate

   !> \brief Data type for storing diagnostic state variables
   !!
   !! This type contains the following real variables:
   !! - `dust_total_flux` : Total flux of dust particles
   !! - `sea_salt_total_flux` : Total flux of sea salt particles
   type, public :: DiagStateType

      ! Reals
      REAL(fp), POINTER :: dust_total_flux(:,:)
      REAL(fp), POINTER :: sea_salt_total_flux(:,:)

   end type DiagStateType

CONTAINS

   !> \brief Allocate memory for the diagnostic state variables
   !!
   !! This subroutine allocates memory for the diagnostic state variables.
   !!
   !! \param Config The configuration options
   !! \param GridState The grid state containing information about the grid
   !! \param DiagState The diagnostic state to be allocated
   !! \param RC The return code
   subroutine Diag_Allocate(Config, GridState, DiagState, RC)
      ! USES
      USE GridState_Mod, ONLY : GridStateType
      USE Config_Opt_Mod, ONLY : ConfigType

      ! Arguments
      type(ConfigType),    INTENT(IN)    :: Config
      type(GridStateType), INTENT(IN)    :: GridState ! Grid State object
      type(DiagStateType), INTENT(INOUT) :: DiagState ! Diag State object
      ! OUTPUT Params
      INTEGER,             INTENT(OUT)   :: RC          ! Success or failure

      ! Error handling
      CHARACTER(LEN=255) :: ErrMsg
      CHARACTER(LEN=255) :: thisLoc

      ! Initialize
      RC = 0
      ErrMsg = ''
      thisLoc = ' -> at Diag_Allocate (in core/diagstate_mod.F90)'

      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      ! Nullify all fields for safety's sake before allocating them
      ! This can prevent compilation errors caused by uninitialized values
      DiagState%dust_total_flux => NULL()
      DiagState%sea_salt_total_flux => NULL()

      ! If dust process is activated then allocate dust related diagnostics
      if (Config%dust_activate) then
         ALLOCATE( DiagState%dust_total_flux( GridState%NX, GridState%NY ), STAT=RC )
         CALL CC_CheckVar( 'DiagState%dust_total_flux', 0, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         DiagState%dust_total_flux = 0._fp
      endif

      ! If sea salt process is activated then allocate sea salt related diagnostics
      if (Config%seasalt_activate) then
         ALLOCATE( DiagState%sea_salt_total_flux( GridState%NX, GridState%NY ), STAT=RC )
         CALL CC_CheckVar( 'DiagState%sea_salt_total_flux', 0, RC )
         IF ( RC /= CC_SUCCESS ) RETURN
         DiagState%sea_salt_total_flux = 0e+0_fp
      endif

   end subroutine Diag_Allocate

end module DiagState_Mod
