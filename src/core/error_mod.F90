!<
!! \file error_mod.F90
!! \brief This file contains error handling routines for CATChem
!! \author Barry Baker
!!
!! This file contains error handling routines for CATChem

!------------------------------------------------------------------------------
!                  CATChem Error Handling Module                           !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: errcode_mod.F90
!
! !DESCRIPTION: Module ERRCODE\_MOD contains the error codes (i.e. that
!  report success or failure) returned by CATChem routines.
!\\
!\\
! !INTERFACE:
!
MODULE ErrCode_Mod
   !
   ! !USES:
   !
   IMPLICIT NONE
   PRIVATE
   !
   ! !PUBLIC MEMBER FUNCTIONS:
   !
   PUBLIC :: CC_Error
   PUBLIC :: CC_Warning
   PUBLIC :: CC_CheckVar
   !
   ! !DEFINED PARAMETERS:
   !
   INTEGER, PUBLIC, PARAMETER :: CC_SUCCESS =  0   ! Routine returns success
   INTEGER, PUBLIC, PARAMETER :: CC_FAILURE = -1   ! Routine returns failure
   !
   ! !REMARKS:
   !  The error codes are returned by routines at various levels of CATChem.
   !
   ! !REVISION HISTORY:
   !  19 Oct 2012 - R. Yantosca - Initial version
   !  See https://github.com/ufs-community/CATchem for complete history
   !EOP
   !------------------------------------------------------------------------------
   !BOC
CONTAINS
   !EOC
   !------------------------------------------------------------------------------
   !                  CATChem                                                     !
   !------------------------------------------------------------------------------
   !BOP
   !
   ! !IROUTINE: CC_Error
   !
   ! !DESCRIPTION: Subroutine CC\_Error prints an error message and sets RC to
   !  CC\_FAILURE. Note that this routine does not stop a run, but it will cause
   !  a stop at a higher level if you add a catch for RC /= CC\_SUCCESS.
   !\\
   !\\
   ! !INTERFACE:
   !
   SUBROUTINE CC_Error( ErrMsg, RC, ThisLoc, Instr )
      !
      ! !USES:
      !
      USE Charpak_Mod,    ONLY : WordWrapPrint
      !
      ! !INPUT PARAMETERS:
      !
      CHARACTER(LEN=*), INTENT(IN)            :: ErrMsg  ! Message to display
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: ThisLoc ! Location of error
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: Instr   ! Other instructions
      !
      ! !INPUT/OUTPUT PARAMETERS:
      !
      INTEGER,          INTENT(INOUT)            :: RC      ! Error code
      !
      ! !REVISION HISTORY:
      !  See https://github.com/ufs-community/CATchem for complete history
      !EOP
      !------------------------------------------------------------------------------
      !BOC

      CHARACTER(LEN=1000) :: Message
      !=======================================================================
      ! CC_ERROR begins here
      !=======================================================================

      ! Construct error message

      ! Separator
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )

      ! Print error message to log
      Message =  'CATChem ERROR: ' // TRIM( ErrMsg )
      CALL WordWrapPrint( Message, 78 )

      ! Print error location to log
      IF ( PRESENT( ThisLoc ) ) THEN
         Message = 'ERROR LOCATION: ' // TRIM( ThisLoc )
         WRITE( 6, '(a)' ) TRIM( ThisLoc )
      ENDIF

      ! Print additional instructions to log
      IF ( PRESENT( Instr ) ) THEN
         WRITE( 6, '(a)' )
         CALL WordWrapPrint( Instr, 78 )
      ENDIF

      ! Separators
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      WRITE( 6, '(a)' ) ''


      ! Force the message to be flushed to the log file
      CALL Flush( 6 )

      ! Return with failure, but preserve existing error code
      IF ( RC == CC_SUCCESS ) THEN
         RC = CC_FAILURE
      ENDIF

   END SUBROUTINE CC_Error
   !EOC
   !------------------------------------------------------------------------------
   !                  CATChem Global Chemical Transport Model                  !
   !------------------------------------------------------------------------------
   !BOP
   !
   ! !IROUTINE: CC_Warning
   !
   ! !DESCRIPTION: Subroutine CC\_Warning prints an warning (i.e. non-fatal
   !  error message) and sets RC to CC\_SUCCESS.
   !\\
   !\\
   ! !INTERFACE:
   !
   SUBROUTINE CC_Warning( WarnMsg, RC, ThisLoc, Instr )
      !
      ! !USES:
      !
      USE Charpak_Mod, ONLY : WordWrapPrint
      !!
      ! !INPUT PARAMETERS:
      !
      CHARACTER(LEN=*), INTENT(IN   )            :: WarnMsg ! Message to display
      CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: ThisLoc ! Location of warning
      CHARACTER(LEN=*), INTENT(IN   ), OPTIONAL  :: Instr   ! Other instructions
      !
      ! !INPUT/OUTPUT PARAMETERS:
      !
      INTEGER,          INTENT(INOUT)            :: RC
      !
      ! !REVISION HISTORY:
      !  See https://github.com/ufs-community/CATchem for complete history
      !EOP
      !------------------------------------------------------------------------------
      !BOC

      CHARACTER(LEN=1000) :: Message

      !=======================================================================
      ! CC_ERROR begins here
      !=======================================================================

      ! Separator
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )

      ! Print error message to log
      Message =  'CATChem WARNING: ' // TRIM( WarnMsg )
      CALL WordWrapPrint( Message, 78 )

      ! Print error location to log
      IF ( PRESENT( ThisLoc ) ) THEN
         Message = 'WARNING LOCATION: ' // TRIM( ThisLoc )
         WRITE( 6, '(a)' ) TRIM( ThisLoc )
      ENDIF

      ! Print additional instructions to log
      IF ( PRESENT( Instr ) ) THEN
         WRITE( 6, '(a)' )
         CALL WordWrapPrint( Instr, 78 )
      ENDIF

      ! Separators
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      WRITE( 6, '(a)' ) ''

      ! Force the message to be flushed to the log file
      CALL Flush( 6 )

      ! Return with success, since this is only a warning message
      RC = CC_SUCCESS

   END SUBROUTINE CC_Warning
   !EOC
   !------------------------------------------------------------------------------
   !                  CATChem Global Chemical Transport Model                  !
   !------------------------------------------------------------------------------
   !BOP
   !
   ! !IROUTINE: CC_CheckVar
   !
   ! !DESCRIPTION: Wrapper routine for CC\_Error.  Prints an error message
   !  if there is an allocation or registration error.  This is intended to
   !  be called from the state initialization method (e.g. Init\_State\_Met).
   !\\
   !\\
   ! !INTERFACE:
   !
   SUBROUTINE CC_CheckVar( Variable, Operation, RC )
      !
      ! !INPUT PARAMETERS:
      !
      CHARACTER(LEN=*), INTENT(IN)    :: Variable   ! Name of variable to check
      INTEGER,          INTENT(IN)    :: Operation  ! 0=Allocate
      ! 1=Register
      ! 2=Deallocate
      !
      ! !OUTPUT PARAMETERS:
      !
      INTEGER,          INTENT(INOUT) :: RC         ! Success or failure
      !
      ! !REMARKS:
      !  You also need to add an
      !    IF ( RC /= CC_SUCCESS ) RETURN
      !  from the calling routine for proper error handling.
      !
      ! !REVISION HISTORY:
      !  See https://github.com/ufs-community/CATchem for complete history
      !EOP
      !------------------------------------------------------------------------------
      !BOC
      !
      ! !LOCAL VARIABLES:
      !
      ! Strings
      CHARACTER(LEN=255) :: ErrMsg, ThisLoc

      !=========================================================================
      ! Initialize
      !=========================================================================

      ! Define error message
      SELECT CASE( Operation )
       CASE( 1 )
         ErrMsg = 'Could not register '   // TRIM( Variable ) // '!'
       CASE( 2 )
         ErrMsg = 'Could not deallocate ' // TRIM( Variable ) // '!'
       CASE DEFAULT
         ErrMsg = 'Could not allocate '   // TRIM( Variable ) // '!'
      END SELECT

      ! Define location string
      ThisLoc   = ' -> at CC_CheckVar (in Headers/errcode_mod.F90)'

      !=========================================================================
      ! Display error message if necessary
      !=========================================================================
      IF ( RC /= CC_SUCCESS ) THEN
         CALL CC_Error( ErrMsg, RC, ThisLoc )
      ENDIF

   END SUBROUTINE CC_CheckVar
   !EOC
END MODULE ErrCode_Mod
