!!! coments for GridState_Mod.F90


module GridState_Mod

   USE Error_Mod
   USE precision_mod

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: Grid_Init_State
   PUBLIC :: Grid_State_Allocate
   PUBLIC :: Grid_Cleanup

   type, public :: GridStateType

      ! Integers
      integer :: nx
      integer :: ny
      integer :: number_of_levels

      ! Reals
      real(fp), POINTER :: area(:,:)

      !-------------------------------------------------------------------------
      ! Name of variables containing grid information
      !-------------------------------------------------------------------------
      CHARACTER(LEN=4)             :: State     = 'Grid'    ! Name of this state
   end type GridStateType


contains

   subroutine Grid_Init_State(Config_Opt, State_Grid, RC)
      ! USES
      USE Config_Opt_Mod, ONLY : OptConfig
      use Error_Mod, only : CC_SUCCESS

      implicit none

      ! Input Params
      type(OptConfig),  intent(in)    :: Config_Opt ! Input Options object

      ! INOUT Params
      type(GridStateType), intent(inout) :: State_Grid ! Grid State object


      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=255) :: thisLoc

      ! set error handling defaults
      RC = 0
      errMsg = ''
      thisLoc = 'Grid_Init_State() -> at initializing GridState'

      ! initialize GridState
      State_Grid%nx=0
      State_Grid%ny=0
      State_Grid%number_of_levels=0
      State_Grid%area => NULL()

   end subroutine Grid_Init_State

   subroutine Grid_Init(Config_Opt, State_Grid, RC)

      USE Config_Opt_Mod, ONLY : OptConfig

      implicit none

      type(OptConfig),  intent(in)    :: Config_Opt ! Input Options object
      type(GridStateType), intent(inout) :: State_Grid ! Grid State object


      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=512) :: thisLoc

      RC=0

      ! allocate GridState
      ! ------------------

      ! start with integers
      State_Grid%nx = 0
      State_Grid%ny = 0
      State_Grid%number_of_levels = 0

      ! nullify arrays
      State_Grid%area   => NULL()

   end subroutine Grid_Init

   subroutine Grid_State_Allocate(State_Grid, RC) !Config_Opt, State_Grid, RC)

      use Error_Mod
      ! use Config_Opt_Mod, only : OptConfig

      implicit none

      ! input Params
      ! type(OptConfig),  intent(in)    :: Config_Opt ! Input Options object

      ! INOUT Params
      type(GridStateType), intent(inout) :: State_Grid ! Grid State object

      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=512) :: thisLoc

      ! set error handling defaults
      thisLoc = 'Grid_Allocate() -> at allocating GridState'
      errMsg = ''
      RC = 0

      ALLOCATE( State_Grid%Area( State_Grid%NX, State_Grid%NY ), STAT=RC )
      CALL CC_CheckVar( 'GridState%Area', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      State_Grid%Area = 0e+0_fp

   end subroutine Grid_State_Allocate

   subroutine Grid_Cleanup(GridState, RC)

      ! INOUT Params
      type(GridStateType), intent(inout) :: GridState ! Grid State object

      ! OUTPUT Params
      INTEGER,         INTENT(OUT)   :: RC          ! Success or failure

      CHARACTER(LEN=512) :: errMsg
      CHARACTER(LEN=512) :: thisLoc

      ! set error handling defaults
      thisLoc = 'Grid_Cleanup() -> at deallocating GridState'
      errMsg = ''
      RC = 0

      DEALLOCATE( GridState%Area, STAT=RC )
      CALL CC_CheckVar( 'GridState%Area', 0, RC )
      IF ( RC /= CC_SUCCESS ) RETURN
      GridState%Area => NULL()

   end subroutine Grid_Cleanup

end module GridState_Mod
