module DiagState_Mod
    ! Uses
    USE Precision_Mod
    USE Error_Mod
    
    IMPLICIT NONE
    private

    ! PUBLIC :: Zero_State_Diag
    PUBLIC :: Diag_Allocate

    type, public :: DiagStateType

        ! Reals
        REAL(fp), POINTER :: dust_total_flux(:,:)
        REAL(fp), POINTER :: sea_salt_total_flux(:,:)

    end type DiagStateType

    CONTAINS

    subroutine Diag_Allocate(Config_Opt, State_Grid, State_Diag, RC)
        ! USES
        USE GridState_Mod, ONLY : GridStateType
        USE Config_Opt_Mod, ONLY : OptConfig

        IMPLICIT NONE

        ! INOUT Params
        type(OptConfig),     INTENT(in)    :: Config_Opt ! Input Options object
        type(GridStateType), INTENT(in)    :: State_Grid ! Grid State object
        type(DiagStateType), INTENT(inout) :: State_Diag ! Diag State object
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
        State_Diag%dust_total_flux => NULL()
        State_Diag%sea_salt_total_flux => NULL()

        ! If dust process is activated then allocate dust related diagnostics
        if (Config_Opt%dust_activate) then
            ALLOCATE( State_Diag%dust_total_flux( State_Grid%NX, State_Grid%NY ), STAT=RC )
            CALL CC_CheckVar( 'State_Diag%dust_total_flux', 0, RC )
            IF ( RC /= CC_SUCCESS ) RETURN
            State_Diag%dust_total_flux = 0e+0_fp
        endif

        ! If sea salt process is activated then allocate sea salt related diagnostics
        if (Config_Opt%seasalt_activate) then
            ALLOCATE( State_Diag%sea_salt_total_flux( State_Grid%NX, State_Grid%NY ), STAT=RC )
            CALL CC_CheckVar( 'State_Diag%sea_salt_total_flux', 0, RC )
            IF ( RC /= CC_SUCCESS ) RETURN
            State_Diag%sea_salt_total_flux = 0e+0_fp
        endif

    end subroutine Diag_Allocate

end module DiagState_Mod
