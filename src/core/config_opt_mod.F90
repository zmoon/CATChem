!------------------------------------------------------------------------------
!                  CATChem  Model
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: Config_mod.F90
!
! !DESCRIPTION: Module INPUT\_OPT\_MOD contains the derived type for CATChem
!  options and logical switches.
!\\
!\\
! !INTERFACE:
!
MODULE Config_Opt_Mod
!
! !USES:
!
   USE PRECISION_MOD    ! For CATChem Precision (fp)

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
   PUBLIC :: Set_Config
   PUBLIC :: Cleanup_Config
!
! !PUBLIC DATA MEMBERS:
!
   !=========================================================================
   ! Derived type for Input Options
   !=========================================================================
   TYPE, PUBLIC :: ConfigType

      !----------------------------------------
      ! General Runtime & Distributed Comp Info
      !----------------------------------------
      INTEGER                     :: numCPUs    ! Number of MPI procs
      INTEGER                     :: thisCPU    ! Local MPI process handle
      INTEGER                     :: MPIComm    ! MPI Communicator Handle
      LOGICAL                     :: isMPI      ! Is this an MPI sim?
      LOGICAL                     :: amIRoot    ! Is this the root cpu?

      !----------------------------------------
      ! Dry run info (print out file names)
      !----------------------------------------
      LOGICAL                     :: DryRun     ! Is this a dry run?

      !----------------------------------------
      ! SIMULATION MENU fields
      !----------------------------------------
      CHARACTER(LEN=255)          :: SimulationName
      CHARACTER(LEN=255)          :: SpcDatabaseFile
      LOGICAL                     :: VerboseRequested
      CHARACTER(LEN=10)           :: VerboseOnCores
      LOGICAL                     :: Verbose

      !-----------------------------------------
      ! PROCESSING MENU fields
      !-----------------------------------------

      ! Dust Process
      LOGICAL                     :: dust_activate
      INTEGER                     :: dust_scheme
      INTEGER                     :: dust_fengsha_drag_opt  ! Fengsha Option for drag Paramertization (1 MB95; 2) Input Value)
      INTEGER                     :: dust_fengsha_moist_opt ! Fengsha Option for moisture Paramertization (1 Fecan; 2 Shao)

      ! SeaSalt Process
      LOGICAL                     :: seasalt_activate
      INTEGER                     :: seasalt_scheme


   END TYPE ConfigType
!
! !REMARKS:
!
! !REVISION HISTORY:
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  CATChem  Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Set_Config
!
! !DESCRIPTION: Subroutine SET\_INPUT\_OPT intializes all CATChem
!  options carried in Input Options derived type object.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Set_Config( am_I_Root, Config, RC )
!
! !USES:
!
      USE Error_Mod
!
! !INPUT PARAMETERS:
!
      LOGICAL,        INTENT(IN)    :: am_I_Root   ! Are we on the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(ConfigType), INTENT(INOUT) :: Config   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!
! !REVISION HISTORY:
!  01 Nov 2012 - R. Yantosca - Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Strings
      CHARACTER(LEN=30) :: arrayId

      !----------------------------------------
      ! Initialize
      ! Set pointers to NULL for safety's sake
      !----------------------------------------
      RC                               =  CC_SUCCESS

      !----------------------------------------
      ! General Runtime & Distributed Comp Info
      !----------------------------------------
      Config%amIRoot                = am_I_Root
      Config%isMPI                  = .FALSE.
      Config%numCPUs                = 1
      Config%thisCPU                = -1
      Config%MPIComm                = -1

      !----------------------------------------
      ! Dry run info (print out file names)
      !----------------------------------------
      Config%DryRun                 = .FALSE.

      !-----------------------------------------
      ! PROCESSING MENU fields
      !-----------------------------------------
      ! Dust Process
      Config%dust_activate = .FALSE.
      Config%dust_scheme = 1
      Config%dust_fengsha_drag_opt = 1
      Config%dust_fengsha_moist_opt = 1

      ! SeaSalt Process
      Config%seasalt_activate = .FALSE.
      Config%seasalt_scheme = 1



   END SUBROUTINE Set_Config
!EOC
!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleanup_Config
!
! !DESCRIPTION: Subroutine CLEANUP\_INPUT\_OPT deallocates all
!  allocatable fields of the Input Options object.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Cleanup_Config( Config, RC )
!
! !USES:
!
      USE Error_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(ConfigType), INTENT(INOUT) :: Config   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
      INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REVISION HISTORY:
!  02 Nov 2012 - R. Yantosca - Initial version
!  See https://github.com/geoschem/CATChem for complete history
!EOP
!------------------------------------------------------------------------------
!BOC

      ! Assume success
      RC = CC_SUCCESS

      !======================================================================
      ! Deallocate fields of the Input Options object
      !======================================================================

      ! Nothing to do yet

   END SUBROUTINE Cleanup_Config
!EOC
END MODULE Config_Opt_Mod
