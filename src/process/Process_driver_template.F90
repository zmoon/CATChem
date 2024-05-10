!------------------------------------------------------------------------------
!                   Configurable Atmosperhic Chemistry Component (CATChem)
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: ccpr_template_mod.F90
!
! !DESCRIPTION: Module ccpr\_template\_mod.F90 is a CATChem process template.
! It serves as a starting point for a new emission process within CATChem.
! Specifically, it provides the framework to use multiple 'instances' of the
! process at the same time.
!\\
!\\
! !INTERFACE:
!
MODULE CCPR_<yourname>_mod
    !
    ! !USES:
    !
      USE Error_MOD
      USE Diagn_MOD
    !   USE TOOLS_MOD
      USE State_MOD, ONLY : Ext_State
      USE State_MOD,  ONLY : State

      IMPLICIT NONE
      PRIVATE
    !
    ! !PUBLIC MEMBER FUNCTIONS:
    !
      PUBLIC :: CCPR_<yourmame>_Run
      PUBLIC :: CCPR_<yourname>_Init
      PUBLIC :: CCPR_<yourname>_Final
    !
    ! !REVISION HISTORY:
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
    !
    ! !MODULE VARIABLES:
    !
      ! MyInst is the process-specific derived type. It should hold all module
      ! variables and arrays that are required to compute the emissions.
      ! For instance, if the process relies on an input field read through the
      ! CATChem configuration file (e.g. MY_INPUT_FIELD), the data array pointer
      ! to that field should be listed within the instance and NOT outside of it.
      ! This ensures that the same process can be invoked in various instances,
      ! all of them potentially pointing to different data fields.
      TYPE :: MyInst
       INTEGER                         :: Instance
       INTEGER                         :: ExtNr     = -1   ! process number
       INTEGER                         :: nSpc      =  0   ! # of species
       INTEGER,  ALLOCATABLE           :: SpcIDs(:)        ! CATChem species IDs
       REAL(sp), ALLOCATABLE           :: SpcScl(:)        ! Species scale factors
       CHARACTER(LEN=31), ALLOCATABLE  :: SpcNames(:)
       CHARACTER(LEN=61), ALLOCATABLE  :: SpcScalFldNme(:) ! Names of scale factor fields
       TYPE(MyInst), POINTER           :: NextInst => NULL()

        !=================================================================
        ! Module specific variables/arrays/data pointers come below
        !=================================================================

      END TYPE MyInst

      ! Pointer to all instances
      TYPE(MyInst), POINTER            :: AllInst => NULL()

    CONTAINS
    !EOC
    !------------------------------------------------------------------------------
    !                   Configurable Atmosperhic Chemistry Component (CATChem)
    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: CCPR_<yourname>_Run
    !
    ! !DESCRIPTION: Write a description here
    !\\
    !\\
    ! !INTERFACE:
    !
      SUBROUTINE CCPR_<yourname>_Run( ExtState, HcoState, RC )
    !
    ! !USES:
    !
        USE FluxArr_Mod,  ONLY : EmisAdd
    !
    ! !INPUT PARAMETERS:
    !
        TYPE(Ext_State), POINTER       :: ExtState    ! Module options
    !
    ! !INPUT/OUTPUT PARAMETERS:
    !
        TYPE(State), POINTER       :: HcoState    ! CATChem state
        INTEGER,         INTENT(INOUT) :: RC          ! Success or failure
    !
    ! !REMARKS:
    !
    !
    ! !REVISION HISTORY:
    !  19 Feb 2016 - C. Keller   - Initial version
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
    !
    ! !LOCAL VARIABLES:
    !
        TYPE(MyInst), POINTER :: Inst => NULL()
        CHARACTER(LEN=255)    :: MSG, LOC

        !=================================================================
        ! CCPR_<yourname>_RUN begins here!
        !=================================================================
        LOC = 'CCPR_<yourname>_RUN (CCPR_<yourname>_MOD.F90)'

        ! Enter
        CALL ENTER( HcoState%Config%Err, LOC, RC )
        IF ( RC /= SUCCESS ) THEN
            CALL ERROR( 'ERROR 0', RC, THISLOC=LOC )
            RETURN
        ENDIF

        ! Get pointer to this instance. Varible Inst contains all module
        ! variables for the current instance. The instance number is
        ! ExtState%<yourname>.
        CALL InstGet ( ExtState%<yourname>, Inst, RC )
        IF ( RC /= SUCCESS ) THEN
           WRITE(MSG,*) 'Cannot find <yourname> instance Nr. ', ExtState%<yourname>
           CALL ERROR(MSG,RC)
           RETURN
        ENDIF

        !=================================================================
        ! Module code comes below
        !=================================================================


        ! Cleanup
        Inst => NULL()

        ! Return w/ success
        CALL LEAVE( HcoState%Config%Err, RC )

      END SUBROUTINE CCPR_AeroCom_Run
    !EOC
    !------------------------------------------------------------------------------
    !                   Configurable Atmosperhic Chemistry Component (CATChem)
    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: CCPR_<yourname>_Init
    !
    ! !DESCRIPTION: Write a description here
    !\\
    !\\
    ! !INTERFACE:
    !
      SUBROUTINE CCPR_<yourname>_Init( HcoState, ExtName, ExtState, RC )
    !
    ! !USES:
    !
        USE ExtList_Mod,    ONLY : GetExtNr
        USE ExtList_Mod,    ONLY : GetExtOpt
        USE ExtList_Mod,    ONLY : GetExtSpcVal
        USE STATE_MOD,      ONLY : GetExtHcoID
    !
    ! !INPUT PARAMETERS:
    !
        CHARACTER(LEN=*), INTENT(IN   ) :: ExtName    ! process name
        TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
    !
    ! !INPUT/OUTPUT PARAMETERS:
    !
        TYPE(State),  POINTER       :: HcoState   ! CATChem state
        INTEGER,          INTENT(INOUT) :: RC

    ! !REVISION HISTORY:
    !  04 Jun 2015 - C. Keller   - Initial version
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
    !
    ! !LOCAL VARIABLES:
    !
        TYPE(MyInst), POINTER          :: Inst => NULL()
        INTEGER                        :: ExtNr, N
        CHARACTER(LEN=255)             :: MSG, LOC

        !=================================================================
        ! CCPR_<yourname>_INIT begins here!
        !=================================================================
        LOC = 'CCPR_<yourname>_INIT (CCPR_<yourname>_MOD.F90)'

        ! process Nr.
        ExtNr = GetExtNr( HcoState%Config%ExtList, TRIM(ExtName) )
        IF ( ExtNr <= 0 ) RETURN

        ! Enter
        CALL ENTER( HcoState%Config%Err, LOC, RC )
        IF ( RC /= SUCCESS ) THEN
            CALL ERROR( 'ERROR 1', RC, THISLOC=LOC )
            RETURN
        ENDIF

        ! Create instance for this simulation. Link instance number to the ExtState object
        ! for future reference to the instance. See InstCreate for more details.
        CALL InstCreate ( ExtNr, ExtState%<yourname>, Inst, RC )
        IF ( RC /= SUCCESS ) THEN
           CALL ERROR ( 'Cannot create <yourname> instance', RC )
           RETURN
        ENDIF

        ! Get species IDs.
        CALL GetExtHcoID( HcoState, ExtNr, Inst%SpcIDs, Inst%SpcNames, Inst%nSpc, RC )
        IF ( RC /= SUCCESS ) THEN
            CALL ERROR( 'ERROR 2', RC, THISLOC=LOC )
            RETURN
        ENDIF

        ! There must be at least one species
        IF ( Inst%nSpc == 0 ) THEN
           CALL ERROR ( 'No <yourname> species specified', RC )
           RETURN
        ENDIF

        ! Determine scale factor to be applied to each species. This is 1.00
        ! by default, but can be set in the CATChem configuration file via setting
        ! Scaling_<SpcName>.
        CALL GetExtSpcVal( HcoState%Config, ExtNr, Inst%nSpc, &
                           Inst%SpcNames, 'Scaling', 1.0_sp, Inst%SpcScl, RC )
        IF ( RC /= SUCCESS ) THEN
            CALL ERROR( 'ERROR 3', RC, THISLOC=LOC )
            RETURN
        ENDIF

        ! Get species mask fields
        CALL GetExtSpcVal( HcoState%Config, ExtNr, Inst%nSpc, &
                           Inst%SpcNames, 'ScaleField', CCPR_NOSCALE, Inst%SpcScalFldNme, RC )
        IF ( RC /= SUCCESS ) THEN
            CALL ERROR( 'ERROR 4', RC, THISLOC=LOC )
            RETURN
        ENDIF

        ! Add conversion factor from kg S to kg of emitted species
        DO N = 1, Inst%nSpc
           Inst%SpcScl(N) = Inst%SpcScl(N) * HcoState%Spc(Inst%SpcIDs(N))%MW_g &
                          * HcoState%Spc(Inst%SpcIDs(N))%MolecRatio / MW_S
        ENDDO

        ! Verbose mode
        IF ( HcoState%amIRoot ) THEN
           MSG = 'Use emissions process <yourname>:'
           CALL MSG( HcoState%Config%Err,  MSG )

           MSG = ' - use the following species (Name, HcoID, Scaling):'
           CALL MSG( HcoState%Config%Err, MSG)
           DO N = 1, Inst%nSpc
              WRITE(MSG,*) TRIM(Inst%SpcNames(N)), ', ', Inst%SpcIDs(N), ', ', Inst%SpcScl(N)
              CALL MSG( HcoState%Config%Err, MSG)
              WRITE(MSG,*) 'Apply scale field: ', TRIM(Inst%SpcScalFldNme(N))
              CALL MSG( HcoState%Config%Err, MSG)
           ENDDO
        ENDIF

        !=================================================================
        ! Module code comes below
        !=================================================================


        ! Cleanup
        Inst => NULL()
        CALL LEAVE( HcoState%Config%Err, RC )

      END SUBROUTINE CCPR_<yourname>_Init
    !EOC
    !------------------------------------------------------------------------------
    !                   Configurable Atmosperhic Chemistry Component (CATChem)
    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: CCPR_<yourname>_Final
    !
    ! !DESCRIPTION: Write a description here
    !\\
    !\\
    ! !INTERFACE:
    !
      SUBROUTINE CCPR_AeroCom_Final ( ExtState )
    !
    ! !INPUT PARAMETERS:
    !
        TYPE(Ext_State),  POINTER       :: ExtState   ! Module options
    !
    ! !REVISION HISTORY:
    !  04 Jun 2015 - C. Keller   - Initial version
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
        !=================================================================
        ! CCPR_<yourname>_FINAL begins here!
        !=================================================================
        CALL InstRemove ( ExtState%<yourname> )

      END SUBROUTINE CCPR_<yourname>_Final
    !EOC
    !------------------------------------------------------------------------------
    !                   Configurable Atmosperhic Chemistry Component (CATChem)
    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: InstGet
    !
    ! !DESCRIPTION: Subroutine InstGet returns a pointer to the desired instance.
    !\\
    !\\
    ! !INTERFACE:
    !
      SUBROUTINE InstGet ( Instance, Inst, RC, PrevInst )
    !
    ! !INPUT PARAMETERS:
    !
        INTEGER                             :: Instance
        TYPE(MyInst),     POINTER           :: Inst
        INTEGER                             :: RC
        TYPE(MyInst),     POINTER, OPTIONAL :: PrevInst
    !
    ! !REVISION HISTORY:
    !  18 Feb 2016 - C. Keller   - Initial version
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
        TYPE(MyInst),     POINTER    :: PrvInst

        !=================================================================
        ! InstGet begins here!
        !=================================================================

        ! Get instance. Also archive previous instance.
        PrvInst => NULL()
        Inst    => AllInst
        DO WHILE ( ASSOCIATED(Inst) )
           IF ( Inst%Instance == Instance ) EXIT
           PrvInst => Inst
           Inst    => Inst%NextInst
        END DO
        IF ( .NOT. ASSOCIATED( Inst ) ) THEN
           RC = FAIL
           RETURN
        ENDIF

        ! Pass output arguments
        IF ( PRESENT(PrevInst) ) PrevInst => PrvInst

        ! Cleanup & Return
        PrvInst => NULL()
        RC = SUCCESS

      END SUBROUTINE InstGet
    !EOC
    !------------------------------------------------------------------------------
    !                   Configurable Atmosperhic Chemistry Component (CATChem)
    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: InstCreate
    !
    ! !DESCRIPTION: Subroutine InstCreate adds a new instance to the list of
    !  instances, assigns a unique instance number to this new instance, and
    !  archives this instance number to output argument Instance.
    !\\
    !\\
    ! !INTERFACE:
    !
      SUBROUTINE InstCreate ( ExtNr, Instance, Inst, RC )
    !
    ! !INPUT PARAMETERS:
    !
        INTEGER,       INTENT(IN)       :: ExtNr
    !
    ! !OUTPUT PARAMETERS:
    !
        INTEGER,       INTENT(  OUT)    :: Instance
        TYPE(MyInst),  POINTER          :: Inst
    !
    ! !INPUT/OUTPUT PARAMETERS:
    !
        INTEGER,       INTENT(INOUT)    :: RC
    !
    ! !REVISION HISTORY:
    !  18 Feb 2016 - C. Keller   - Initial version
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
        TYPE(MyInst), POINTER          :: TmpInst  => NULL()
        INTEGER                        :: nnInst

        !=================================================================
        ! InstCreate begins here!
        !=================================================================

        ! ----------------------------------------------------------------
        ! Generic instance initialization
        ! ----------------------------------------------------------------

        ! Initialize
        Inst => NULL()

        ! Get number of already existing instances
        TmpInst => AllInst
        nnInst = 0
        DO WHILE ( ASSOCIATED(TmpInst) )
           nnInst  =  nnInst + 1
           TmpInst => TmpInst%NextInst
        END DO

        ! Create new instance
        ALLOCATE(Inst)
        Inst%Instance = nnInst + 1
        Inst%ExtNr    = ExtNr

        ! Attach to instance list
        Inst%NextInst => AllInst
        AllInst       => Inst

        ! Update output instance
        Instance = Inst%Instance

        ! ----------------------------------------------------------------
        ! Type specific initialization statements follow below
        ! ----------------------------------------------------------------

        ! Return w/ success
        RC = SUCCESS

      END SUBROUTINE InstCreate
    !EOC
    !------------------------------------------------------------------------------
    !                   Configurable Atmosperhic Chemistry Component (CATChem)
    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: InstRemove
    !
    ! !DESCRIPTION: Subroutine InstRemove removes an instance from the list of
    ! instances.
    !\\
    !\\
    ! !INTERFACE:
    !
      SUBROUTINE InstRemove ( Instance )
    !
    ! !INPUT PARAMETERS:
    !
        INTEGER                         :: Instance
    !
    ! !REVISION HISTORY:
    !  18 Feb 2016 - C. Keller   - Initial version
    !  See https://github.com/ufs-community/CATChem for complete history
    !EOP
    !------------------------------------------------------------------------------
    !BOC
        INTEGER                     :: RC
        TYPE(MyInst), POINTER       :: PrevInst
        TYPE(MyInst), POINTER       :: Inst

        !=================================================================
        ! InstRemove begins here!
        !=================================================================

        ! Initialize
        PrevInst => NULL()
        Inst     => NULL()

        ! Get instance. Also archive previous instance.
        CALL InstGet ( Instance, Inst, RC, PrevInst=PrevInst )

        ! Instance-specific deallocation
        IF ( ASSOCIATED(Inst) ) THEN

           !---------------------------------------------------------------------
           ! Deallocate fields of Inst before popping off from the list
           ! in order to avoid memory leaks (Bob Yantosca (17 Aug 2022)
           !---------------------------------------------------------------------
           IF ( ALLOCATED( Inst%SpcIDs ) ) THEN
              DEALLOCATE ( Inst%SpcIDs )
           ENDIF

           IF ( ALLOCATED( Inst%SpcScl ) ) THEN
              DEALLOCATE ( Inst%SpcScl )
           ENDIF

           IF ( ALLOCATED( Inst%SpcNames ) ) THEN
              DEALLOCATE ( Inst%SpcNames )
           ENDIF

           IF ( ALLOCATED( Inst%SpcScalFldNme ) ) THEN
              DEALLOCATE( Inst%SpcScalFldNme  )
           ENDIF

           ! ----------------------------------------------------------------
           ! Type specific initialization statements follow below
           ! ----------------------------------------------------------------


           ! ----------------------------------------------------------------
           ! Pop off instance from list
           ! ----------------------------------------------------------------
           IF ( ASSOCIATED(PrevInst) ) THEN
              PrevInst%NextInst => Inst%NextInst
           ELSE
              AllInst => Inst%NextInst
           ENDIF
           DEALLOCATE(Inst)
        ENDIF

        ! Free pointers before exiting
        PrevInst => NULL()
        Inst     => NULL()

       END SUBROUTINE InstRemove
    !EOC
    END MODULE CCPR_yourname_Mod