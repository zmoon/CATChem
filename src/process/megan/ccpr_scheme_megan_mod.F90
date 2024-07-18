!>
!! \file
!! \brief Contains MEGAN2.1 biogenic VOC emission Scheme based on HEMCO and Sam Silva's canopy edits
!!
!! Reference: 
!! (1) Guenther et al, (GMD 2012) and associated MEGANv2.1 source code
!!     https://doi.org/10.5194/gmd-5-1471-2012
!! (2) Sam Silva's simplified canopy edits (GMD 2020)
!!     https://doi.org/10.5194/gmd-13-2569-2020
!!
!! \author Wei Li
!! \date 07/2024
!! \ingroup catchem_megan_process
!!!>

module CCPr_Scheme_Megan_Mod

   implicit none

   private

   public :: CCPr_Scheme_Megan

contains

   !> \brief Brief description of the subroutine
   !!
   !! \param MetState     Meteorological Variables
   !! \param DiagState    Diagnostic Variables !TODO: seems not ued now
   !! \param DustState    Dust Variables
   !! \param RC           Success or Failure
   !!
   !! Note that other state types may be required, e.g. one specific to the process group.
   !!!>
   subroutine CCPr_Scheme_Megan(MetState, DiagState, MeganState, RC)

      ! Uses
      USE Constants,     Only : g0 ! Example to pull in a constant from the CONSTANTS MODULE < Modify as needed >
      use precision_mod, only : fp, ZERO  ! Example to pull in a precision from the PRECISION MODULE < Modify as needed >
      !TODO: need to comment out the 'Only' to avoid errors, but not in fengsha scheme
      Use MetState_Mod !,  Only : MetStateType  ! Needed to access Meteorological Variables
      Use DiagState_Mod!, Only : DiagStateType ! Diagnostic Variables are added through DiagState below
      Use Error_Mod,     Only : CC_SUCCESS    ! Error Check Success
      Use CCPr_Megan_Common_mod !, Only : MeganStateType  ! Overall PROCESS State Type - Controlling PROCESS
      
      IMPLICIT NONE

      ! Arguments
      type(MetStateType),   intent(inout)    :: MetState    ! Meteorological Variables
      type(DiagStateType),  intent(inout) :: DiagState   ! Diagnostic Variables
      type(MeganStateType), intent(inout) :: MeganState  ! Megan Variables
      integer,              intent(out)   :: RC          ! Success or Failure

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      real(fp)            :: MEGAN_EMIS ! emission for each species [kg/m2/s]
      character(len=256)  :: CMPD      ! Compound name 
      REAL(fp)            :: GAMMA_LAI
      REAL(fp)            :: GAMMA_AGE
      REAL(fp)            :: GAMMA_TP  !canopy add
      REAL(fp)            :: CDEA(5)   !canopy add
      REAL(fp)            :: VPGWT(5)  !canopy add
      REAL(fp)            :: GAMMA_PAR_Sun, GAMMA_PAR_Shade !canopy add
      REAL(fp)            :: GAMMA_T_LD_Sun, GAMMA_T_LD_Shade !canopy add
      REAL(fp)            :: GAMMA_T_LI_Sun, GAMMA_T_LI_Shade !canopy add
      !REAL(fp)            :: WINDSP !canopy add
      REAL(fp)            :: GAMMA_PAR
      REAL(fp)            :: GAMMA_T_LD
      REAL(fp)            :: GAMMA_T_LI
      REAL(fp)            :: GAMMA_SM
      REAL(fp)            :: GAMMA_CO2  
      REAL(fp)            :: AEF
      !REAL(fp)            :: D_BTW_M
      !REAL(fp)            :: TS, SUNCOS
      !REAL(fp)            :: Q_DIR_2, Q_DIFF_2
      REAL(fp)            :: BETA, LDF, CT1, CEO
      REAL(fp)            :: ANEW, AGRO, AMAT, AOLD
      !REAL(fp)            :: ISOLAI, PMISOLAI, MISOLAI
      REAL(fp)            :: PFTSUM
      REAL(fp), parameter :: LAI_MAX = 6.0_fp !Maximum LAI value [cm2/cm2]
      !REAL(fp)            :: LAT, LocalHour !canopy add
      REAL(fp)            :: PSTD
      REAL(fp)            :: Ea1L, Ea2L, SINbeta, SunF !canopy add
      LOGICAL             :: BIDIR
      INTEGER             :: K, S !,DOY  !canopy add and below
      REAL(fp)            :: T_Leaf_Int_Sun(5)
      REAL(fp)            :: T_Leaf_Int_Shade(5)
      REAL(fp)            :: T_Leaf_Temp_Sun(5)
      REAL(fp)            :: T_Leaf_Temp_Shade(5)
      !REAL(fp)            :: T_Leaf_Wind_Sun(5)
      !REAL(fp)            :: T_Leaf_Wind_Shade(5)
      REAL(fp)            :: P_Leaf_Int_Sun(5)
      REAL(fp)            :: P_Leaf_Int_Shade(5)
      REAL(fp)            :: P_Leaf_LAI_Sun(5)
      REAL(fp)            :: P_Leaf_LAI_Shade(5)
      REAL(fp)            :: Distgauss(5)

      ! Initialize parameters, gamma values, and return value
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_Megan (in CCPr_Scheme_Megan_mod.F90)'
      RC = CC_SUCCESS

      CDEA       = 0.0_fp  !canopy add
      GAMMA_TP   = 0.0_fp  !canopy add
      MEGAN_EMIS = 0.0_fp
      GAMMA_LAI  = 0.0_fp
      GAMMA_AGE  = 0.0_fp
      GAMMA_T_LD = 0.0_fp
      GAMMA_T_LI = 0.0_fp
      GAMMA_PAR  = 0.0_fp
      GAMMA_SM   = 0.0_fp
      GAMMA_CO2  = 0.0_fp
      BETA       = 0.0_fp
      AEF        = 0.0_fp
      LDF        = 0.0_fp
      CT1        = 0.0_fp
      CEO        = 0.0_fp
      ANEW       = 0.0_fp
      AGRO       = 0.0_fp
      AMAT       = 0.0_fp
      AOLD       = 0.0_fp
      BIDIR      = .FALSE.

      !----------------------------------
      ! Begin SchemeCCPr_Scheme_Megan
      !----------------------------------

      MeganState%EmissionPerSpecies = 0.0_fp

      !-----------------------------------------------------
      ! Only interested in terrestrial biosphere
      ! If ( local LAI > 0 ) replace the zeros assigned above
      !-----------------------------------------------------
      if ( MetState%LAI > 0.0_fp ) then

         !-----------------normalize LAI by total PFT fractions
         PFTSUM = SUM( MetState%PFT_16(2:16) )
         MetState%MISOLAI  = min(MetState%LAI/PFTSUM, LAI_MAX)
         MetState%PMISOLAI = min(MetState%PMISOLAI, LAI_MAX)

         !----------------- %%gamma values not related to compound%% ------------------

         ! --------------------------------------------------
         ! GAMMA_par (light activity factor)
         ! --------------------------------------------------

         ! Calculate GAMMA PAR only during day
         IF ( MetState%SUNCOS > 0.0_fp ) THEN

            call GET_GAMMA_PAR_PCEEA( MetState%Q_DIR_2,             &
                                    MetState%Q_DIFF_2,              &
                                    MetState%PARDR_LASTXDAYS,       &
                                    MetState%PARDF_LASTXDAYS,       &
                                    MetState%LAT, MetState%DOY,     &
                                    MetState%LocalHour,             &
                                    MetState%D2RAD, MetState%RAD2D, &
                                    GAMMA_PAR)
         ELSE

            ! If night
            GAMMA_PAR = 0.0_fp
         ENDIF

         ! --------------------------------------------------
         ! CO2 inhibition of isoprene (Tai, Jan 2013)
         ! --------------------------------------------------
         IF ( MeganState%CO2conc ) THEN
            call GET_GAMMA_CO2( MeganState%CO2conc, GAMMA_CO2 )
         ELSE
            GAMMA_CO2 = 1.0_fp
         ENDIF

         !Sam Silva's canopy related coefficients
         T_Leaf_Int_Sun  = (/-13.891_fp, -12.322_fp, -1.032_fp, -5.172_fp, -5.589_fp/)
         T_Leaf_Int_Shade = (/-12.846_fp, -11.343_fp, -1.068_fp,-5.551_fp, -5.955_fp/)
         T_Leaf_Temp_Sun = (/1.064_fp, 1.057_fp, 1.031_fp,  1.050_fp, 1.051_fp/)
         T_Leaf_Temp_Shade = (/1.060_fp, 1.053_fp, 1.031_fp,1.051_fp, 1.052_fp/)
         P_Leaf_Int_Sun  = (/1.0831_fp, 1.0964_fp, 1.1036_fp, 1.0985_fp, 1.0901_fp/)
         P_Leaf_Int_Shade = (/0.8706_fp, 0.8895_fp, 0.9160_fp,0.9407_fp, 0.9564_fp/)
         P_Leaf_LAI_Sun = (/0.0018_fp, -0.1281_fp, -0.2977_fp, -0.4448_fp, -0.5352_fp/)
         P_Leaf_LAI_Shade = (/0.0148_fp, -0.1414_fp, -0.3681_fp,-0.5918_fp, -0.7425_fp/)
         VPGWT = (/0.1184635, 0.2393144, 0.284444444, 0.2393144, 0.1184635/)
         Distgauss = (/0.0469101, 0.2307534, 0.5, 0.7692465, 0.9530899/)      
         
         call  SOLAR_ANGLE(MetState%DOY, MetState%LocalHour, &
                                MetState%LAT, MetState%D2RAD, SINbeta)
         call GET_CDEA( MetState%MISOLAI, CDEA  )

         !--------------------- %%gamma values related to compound%% ----------------------

         DO S=1, MeganState%nMeganSpecies

            CMPD = MeganState%MeganSpeciesName(S)

            ! --------------------------------------------
            ! Get MEGAN parameters for this compound
            ! --------------------------------------------
            CALL GET_MEGAN_PARAMS ( CMPD, BETA, LDF,  CT1,  CEO,      &
                                    ANEW, AGRO, AMAT, AOLD, BIDIR, RC )

            ! --------------------------------------------------
            ! GAMMA_LAI (leaf area index activity factor)
            ! --------------------------------------------------
            call GET_GAMMA_LAI( MetState%MISOLAI, BIDIR, GAMMA_LAI )

            ! --------------------------------------------------
            ! GAMMA_AGE (leaf age activity factor)
            ! --------------------------------------------------
            call GET_GAMMA_AGE(  MetState%MISOLAI,           &
                                 MetState%PMISOLAI,          &
                                 MetState%D_BTW_M,           &
                                 MetState%T_LASTXDAYS,       &
                                 ANEW, AGRO, AMAT, AOLD,     &
                                 GAMMA_AGE)
            
            ! --------------------------------------------------
            ! GAMMA_T_LI (temperature activity factor for
            ! light-independent fraction)
            ! --------------------------------------------------
            !GAMMA_T_LI = GET_GAMMA_T_LI( TS, BETA ) 

            ! --------------------------------------------------
            ! GAMMA_T_LD (temperature activity factor for
            ! light-dependent fraction)
            ! --------------------------------------------------
            !GAMMA_T_LD = GET_GAMMA_T_LD( TS, Inst%T_LASTXDAYS(I,J), &
            !                             Inst%T_LAST24H(I,J), CT1, CEO )
            
            ! --------------------------------------------------
            ! Sam Silva's edits to replace GAMMA_T_LD 
            !  and GAMMA_T_LI above
            ! --------------------------------------------------
            GAMMA_TP = 0.0_fp

            DO K = 1, 5

               call Calc_Sun_Frac(MetState%MISOLAI,SINbeta,Distgauss(K), SunF)
   
               PSTD = 200_fp
               call GET_GAMMA_PAR_C(MetState%Q_DIR_2,             &
                                    MetState%Q_DIFF_2,            &
                                    MetState%PARDR_LASTXDAYS,     &
                                    MetState%PARDF_LASTXDAYS,     &
                                    P_Leaf_LAI_Sun(K),            &
                                    P_Leaf_Int_Sun(K),            &
                                    MetState%MISOLAI, PSTD,       &
                                    GAMMA_PAR_Sun)
   
               PSTD = 50_fp
               call GET_GAMMA_PAR_C(MetState%Q_DIR_2,             &
                                    MetState%Q_DIFF_2,            &
                                    MetState%PARDR_LASTXDAYS,     &
                                    MetState%PARDF_LASTXDAYS,     &
                                    P_Leaf_LAI_Shade(K),          &
                                    P_Leaf_Int_Shade(K),          &
                                    MetState%MISOLAI, PSTD,       &
                                    GAMMA_PAR_Shade)
   
               call GET_GAMMA_T_LD_C( MetState%TS,                &
                                    MetState%T_LASTXDAYS,         &
                                    MetState%T_LAST24H,           &
                                    CT1, CEO,                     &
                                    T_Leaf_Int_Sun(K),            &
                                    T_Leaf_Temp_Sun(K),           &
                                    GAMMA_T_LD_Sun )
   
               call GET_GAMMA_T_LD_C(MetState%TS,                 &
                                    MetState%T_LASTXDAYS,         &
                                    MetState%T_LAST24H,           &
                                    CT1, CEO,                     &
                                    T_Leaf_Int_Shade(K),          &
                                    T_Leaf_Temp_Shade(K),         &
                                    GAMMA_T_LD_Sun )
   
               call GET_GAMMA_T_LI( MetState%TS, BETA,            &
                                    T_Leaf_Int_Sun(K),            &
                                    T_Leaf_Temp_Sun(K),           &
                                    GAMMA_T_LI_Sun )
   
               call GET_GAMMA_T_LI( MetState%TS, BETA,            &
                                    T_Leaf_Int_Shade(K),          &
                                    T_Leaf_Temp_Shade(K),         &
                                    GAMMA_T_LI_Sun )   
   
               Ea1L  =  CDEA(K) * GAMMA_PAR_Sun * GAMMA_T_LD_Sun * SunF +   &
                           GAMMA_PAR_Shade * GAMMA_T_LD_Shade * (1-SunF)
   
               Ea2L =  GAMMA_T_LI_Sun * SunF +                              &
                           GAMMA_T_LI_Shade * (1-SunF)
   
               GAMMA_TP  = GAMMA_TP +                                       &
                           (Ea1L*LDF + Ea2L*(1-LDF))* VPGWT(K)
         
            ENDDO
   
            ! --------------------------------------------------
            ! GAMMA_SM (soil moisture activity factor)
            ! --------------------------------------------------
            call GET_GAMMA_SM( MetState%GWETROOT, CMPD, GAMMA_SM )

            ! --------------------------------------------------
            ! emission factor (TODO: AE of these seven species are from file reading)
            ! --------------------------------------------------
            select case ( TRIM(CMPD) )
               case ('ISOP')
                  AEF = MetState%AEF_ISOP
               case ('MBOX')
                  AEF = MetState%AEF_MBOX
               case ('BPIN')
                  AEF = MetState%AEF_BPIN
               case ('CARE')
                  AEF = MetState%AEF_CARE
               case ('LIMO')
                  AEF = MetState%AEF_LIMO
               case ('OCIM')
                  AEF = MetState%AEF_OCIM
               case ('SABI')
                  AEF = MetState%AEF_SABI
               case default !others are calcualted inline
                  call CALC_AEF(MetState%PFT_16, CMPD, AEF, RC)
            end select
            
            ! --------------------------------------------------
            ! calculate emission
            ! --------------------------------------------------
            ! Emission is the product of all of these in kg/m2/s.
            ! Normalization factor ensures product of GAMMA values is 1.0 under
            !  standard conditions. Norm_FAC = 0.21. canopy add
            IF ( TRIM(CMPD) == 'ISOP' ) THEN
            ! Only apply CO2 inhibition to isoprene
            ! MEGAN_EMIS = Inst%NORM_FAC(1) * AEF * GAMMA_AGE * GAMMA_SM * &
            !              GAMMA_LAI * ((1.0_fp - LDF) * GAMMA_T_LI +      &
            !              (LDF * GAMMA_PAR * GAMMA_T_LD)) * GAMMA_CO2
               MEGAN_EMIS = MetState%MISOLAI * AEF * GAMMA_AGE * GAMMA_SM *  &
                              GAMMA_TP*GAMMA_CO2*GAMMA_LAI*0.21_fp
            ELSE
            ! MEGAN_EMIS = Inst%NORM_FAC(1) * AEF * GAMMA_AGE * GAMMA_SM * &
            !              GAMMA_LAI * ((1.0_fp - LDF) * GAMMA_T_LI +      &
            !              (LDF * GAMMA_PAR * GAMMA_T_LD))
               MEGAN_EMIS = MetState%MISOLAI * AEF * GAMMA_AGE * GAMMA_SM *  &
                           GAMMA_TP * GAMMA_LAI * 0.21_fp

            ENDIF
            
            MeganState%EmissionPerSpecies(S) = MEGAN_EMIS
         
         ENDDO !each species

      endif

      return

   end subroutine CCPr_Scheme_Megan

end module CCPr_Scheme_Megan_Mod
