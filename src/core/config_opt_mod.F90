!------------------------------------------------------------------------------
!                  CATChem  Model  
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: Config_Opt_mod.F90     
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
   PUBLIC :: Set_Config_Opt
   PUBLIC :: Cleanup_Config_Opt
!
! !PUBLIC DATA MEMBERS:
!
   !=========================================================================
   ! Derived type for Input Options
   !=========================================================================
   TYPE, PUBLIC :: OptConfig

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
      LOGICAL                     :: dust_activate
      INTEGER                     :: dust_scheme
      LOGICAL                     :: seasalt_activate
      INTEGER                     :: seasalt_scheme


   END TYPE OptConfig
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
! !IROUTINE: Set_Config_Opt
!
! !DESCRIPTION: Subroutine SET\_INPUT\_OPT intializes all CATChem
!  options carried in Input Options derived type object.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Set_Config_Opt( am_I_Root, Config_Opt, RC )
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
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
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
      Config_Opt%amIRoot                = am_I_Root
      Config_Opt%isMPI                  = .FALSE.
      Config_Opt%numCPUs                = 1
      Config_Opt%thisCPU                = -1
      Config_Opt%MPIComm                = -1

      !----------------------------------------
      ! Dry run info (print out file names)
      !----------------------------------------
      Config_Opt%DryRun                 = .FALSE.

      !-----------------------------------------
      ! PROCESSING MENU fields
      !-----------------------------------------
      Config_Opt%dust_activate = .FALSE.
      Config_Opt%dust_scheme = 1
      Config_Opt%seasalt_activate = .FALSE.
      Config_Opt%seasalt_scheme = 1



   END SUBROUTINE Set_Config_Opt
!EOC
!------------------------------------------------------------------------------
!                  CATChem Model                                              !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Cleanup_Config_Opt
!
! !DESCRIPTION: Subroutine CLEANUP\_INPUT\_OPT deallocates all
!  allocatable fields of the Input Options object.
!\\
!\\
! !INTERFACE:
!
   SUBROUTINE Cleanup_Config_Opt( Config_Opt, RC )
!
! !USES:
!
      USE Error_Mod
!
! !INPUT/OUTPUT PARAMETERS:
!
      TYPE(OptConfig), INTENT(INOUT) :: Config_Opt   ! Input Options object
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

   END SUBROUTINE Cleanup_Config_Opt
!EOC
END MODULE Config_Opt_Mod
