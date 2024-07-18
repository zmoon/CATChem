!>
!! \file
!! \brief Contains a Template for any given CCPr Scheme
!!
!! To use:
!! - replace SCHEME by an identifier for the scheme (e.g. 'fengsha')
!! - replace PROCESS by an identifier for the process group (e.g. 'Dust')
!!
!! Reference: Please include citation here
!!
!! \author Barry baker
!! \date 05/2024
!!!>
module CCPr_Scheme_Megan_Mod

   implicit none

   private

   public :: CCPr_Scheme_Megan

contains

   !> \brief Brief description of the subroutine
   !!
   !! \param MetState     Meteorological Variables
   !! \param DiagState    Diagnostic Variables
   !! \param DustState    Dust Variables
   !! \param RC           Success or Failure
   !!
   !! Note that other state types may be required, e.g. one specific to the process group.
   !!!>
   subroutine CCPr_Scheme_Megan(MetState, DiagState, MeganState, RC)

      ! Uses
      USE Constants,     Only : g0 ! Example to pull in a constant from the CONSTANTS MODULE < Modify as needed >
      use precision_mod, only : fp, ZERO  ! Example to pull in a precision from the PRECISION MODULE < Modify as needed >
      Use MetState_Mod!,  Only : MetStateType  ! Needed to access Meteorological Variables
      Use DiagState_Mod!, Only : DiagStateType ! Diagnostic Variables are added through DiagState below
      Use Error_Mod,     Only : CC_SUCCESS    ! Error Check Success
      Use CCPr_Megan_Common_mod !, Only : MeganStateType  ! Overall PROCESS State Type - Controlling PROCESS
      
      IMPLICIT NONE

      ! Arguments
      type(MetStateType),  intent(in) :: MetState     ! Meteorological Variables
      type(DiagStateType), intent(inout) :: DiagState    ! Diagnostic Variables
      type(MeganStateType), intent(inout) :: MeganState  ! PROCESS Variables
      integer, intent(out) :: RC                      ! Success or Failure

      ! Local Variables
      character(len=256) :: errMsg
      character(len=256) :: thisLoc

      !integer :: SCHEMEInt   ! Add Local Scheme Specific Variables < Modify as needed >
      real(fp) :: MEGAN_EMIS ! emission for each species [kg/m2/s]
      character(len=256) :: CMPD      ! Compound name 
      REAL(hp)            :: GAMMA_LAI
      REAL(hp)            :: GAMMA_AGE
      REAL(hp)            :: GAMMA_TP  !canopy add
      REAL(hp)            :: CDEA(5)   !canopy add
      REAL(hp)            :: VPGWT(5)  !canopy add
      REAL(hp)            :: GAMMA_PAR_Sun, GAMMA_PAR_Shade !canopy add
      REAL(hp)            :: GAMMA_T_LD_Sun, GAMMA_T_LD_Shade !canopy add
      REAL(hp)            :: GAMMA_T_LI_Sun, GAMMA_T_LI_Shade !canopy add
      !REAL(hp)            :: WINDSP !canopy add
      REAL(hp)            :: GAMMA_PAR
      REAL(hp)            :: GAMMA_T_LD
      REAL(hp)            :: GAMMA_T_LI
      REAL(hp)            :: GAMMA_SM
      REAL(hp)            :: GAMMA_CO2  ! (Tai, Jan 2013)
      REAL(hp)            :: AEF
      !REAL(hp)            :: D_BTW_M
      !REAL(hp)            :: TS, SUNCOS
      !REAL(hp)            :: Q_DIR_2, Q_DIFF_2
      REAL(hp)            :: BETA, LDF, CT1, CEO
      REAL(hp)            :: ANEW, AGRO, AMAT, AOLD
      !REAL(hp)            :: ISOLAI, PMISOLAI, MISOLAI
      !REAL(hp)            :: PFTSUM
      !REAL(hp)            :: LAT, LocalHour !canopy add
      REAL(hp)            :: PSTD
      REAL(hp)            :: Ea1L, Ea2L, SINbeta, SunF !canopy add
      LOGICAL             :: BIDIR
      INTEGER             :: K  !, DOY  !canopy add and below
      REAL(hp)            :: T_Leaf_Int_Sun(5)
      REAL(hp)            :: T_Leaf_Int_Shade(5)
      REAL(hp)            :: T_Leaf_Temp_Sun(5)
      REAL(hp)            :: T_Leaf_Temp_Shade(5)
      !REAL(hp)            :: T_Leaf_Wind_Sun(5)
      !REAL(hp)            :: T_Leaf_Wind_Shade(5)
      REAL(hp)            :: P_Leaf_Int_Sun(5)
      REAL(hp)            :: P_Leaf_Int_Shade(5)
      REAL(hp)            :: P_Leaf_LAI_Sun(5)
      REAL(hp)            :: P_Leaf_LAI_Shade(5)
      REAL(hp)            :: Distgauss(5)

      ! Initialize parameters, gamma values, and return value
      errMsg = ''
      thisLoc = ' -> at CCPr_Scheme_Megan (in CCPr_Scheme_Megan_mod.F90)'
      RC = CC_SUCCESS

      CDEA       = 0.0_hp  !canopy add
      GAMMA_TP   = 0.0_hp  !canopy add
      MEGAN_EMIS = 0.0_hp
      GAMMA_LAI  = 0.0_hp
      GAMMA_AGE  = 0.0_hp
      GAMMA_T_LD = 0.0_hp
      GAMMA_T_LI = 0.0_hp
      GAMMA_PAR  = 0.0_hp
      GAMMA_SM   = 0.0_hp
      GAMMA_CO2  = 0.0_hp
      BETA       = 0.0_hp
      AEF        = 0.0_hp
      LDF        = 0.0_hp
      CT1        = 0.0_hp
      CEO        = 0.0_hp
      ANEW       = 0.0_hp
      AGRO       = 0.0_hp
      AMAT       = 0.0_hp
      AOLD       = 0.0_hp
      BIDIR      = .FALSE.

      !----------------------------------
      ! Begin SchemeCCPr_Scheme_Megan
      !----------------------------------

      MeganState%EmissionPerSpecies = 0.0_fp

      !-----------------------------------------------------
      ! Only interested in terrestrial biosphere
      ! If (local LAI != 0 .AND. baseline emission !=0 )
      !-----------------------------------------------------
      if ( LocalMet%ISOLAI > 0.0_fp ) then

         !----------------- gamma values not related to compound ------------------

         ! --------------------------------------------------
         ! GAMMA_par (light activity factor)
         ! --------------------------------------------------

         ! Calculate GAMMA PAR only during day
         IF ( LocalMet%SUNCOS > 0.0_hp ) THEN

            GAMMA_PAR = GET_GAMMA_PAR_PCEEA( LocalMet%Q_DIR_2,           &
                                             LocalMet%Q_DIFF_2,          &
                                             LocalMet%PARDR_LASTXDAYS,   &
                                             LocalMet%PARDF_LASTXDAYS,   &
                                             LocalMet%LAT, LocalMet%DOY, &
                                             LocalMet%LocalHour,         &
                                             LocalMet%D2RAD, LocalMet%RAD2D)
         ELSE

            ! If night
            GAMMA_PAR = 0.0_hp
         ENDIF

         ! --------------------------------------------------
         ! GAMMA_LAI (leaf area index activity factor)
         ! --------------------------------------------------
         GAMMA_LAI = GET_GAMMA_LAI( LocalMet%MISOLAI, BIDIR )

         ! --------------------------------------------------
         ! GAMMA_AGE (leaf age activity factor)
         ! --------------------------------------------------
         GAMMA_AGE = GET_GAMMA_AGE( LocalMet%MISOLAI,           &
                                    LocalMet%PMISOLAI,          &
                                    LocalMet%D_BTW_M,           &
                                    LocalMet%T_LASTXDAYS,       &
                                    ANEW, AGRO, AMAT, AOLD )
         
         ! --------------------------------------------------
         ! CO2 inhibition of isoprene (Tai, Jan 2013)
         ! --------------------------------------------------
         IF ( LocalMet%LISOPCO2 ) THEN
            GAMMA_CO2 = GET_GAMMA_CO2( LocalMet%GLOBCO2 )
         ELSE
            GAMMA_CO2 = 1.0_hp
         ENDIF

         !canopy add main
         T_Leaf_Int_Sun  = (/-13.891_hp, -12.322_hp, -1.032_hp,        &
                     -5.172_hp, -5.589_hp/)
         T_Leaf_Int_Shade = (/-12.846_hp, -11.343_hp, -1.068_hp,  &
                     -5.551_hp, -5.955_hp/)

         T_Leaf_Temp_Sun = (/1.064_hp, 1.057_hp, 1.031_hp,        &
                  1.050_hp, 1.051_hp/)
         T_Leaf_Temp_Shade = (/1.060_hp, 1.053_hp, 1.031_hp,      &
                     1.051_hp, 1.052_hp/)

         P_Leaf_Int_Sun  = (/1.0831_hp, 1.0964_hp, 1.1036_hp,     &
                  1.0985_hp, 1.0901_hp/)
         P_Leaf_Int_Shade = (/0.8706_hp, 0.8895_hp, 0.9160_hp,    &
                     0.9407_hp, 0.9564_hp/)

         P_Leaf_LAI_Sun = (/0.0018_hp, -0.1281_hp, -0.2977_hp,    &
                  -0.4448_hp, -0.5352_hp/)
         P_Leaf_LAI_Shade = (/0.0148_hp, -0.1414_hp, -0.3681_hp,  &
                  -0.5918_hp, -0.7425_hp/)


         VPGWT = (/0.1184635, 0.2393144, 0.284444444,                &
         0.2393144, 0.1184635/)

         Distgauss = (/0.0469101, 0.2307534, 0.5, 0.7692465,         &
         0.9530899/)
      
         SINbeta =  SOLAR_ANGLE(LocalMet%DOY, LocalMet%LocalHour,     &
                                LocalMet%LAT, LocalMet%D2RAD)

         CDEA = GET_CDEA( LocalMet%MISOLAI )
         GAMMA_TP = 0.0_hp

         DO K = 1, 5

            SunF = Calc_Sun_Frac(LocalMet%MISOLAI,SINbeta,Distgauss(K))
 
            PSTD = 200_hp
            GAMMA_PAR_Sun = GET_GAMMA_PAR_C(LocalMet%Q_DIR_2,            &
                                            LocalMet%Q_DIFF_2,           &
                                            LocalMet%PARDR_LASTXDAYS,    &
                                            LocalMet%PARDF_LASTXDAYS,    &
                                            P_Leaf_LAI_Sun(K),           &
                                            P_Leaf_Int_Sun(K),           &
                                            LocalMet%MISOLAI, PSTD)
 
            PSTD = 50_hp
            GAMMA_PAR_Shade = GET_GAMMA_PAR_C(LocalMet%Q_DIR_2,          &
                                              LocalMet%Q_DIFF_2,         &
                                          LocalMet%PARDR_LASTXDAYS,      &
                                          LocalMet%PARDF_LASTXDAYS,      &
                                          P_Leaf_LAI_Shade(K),           &
                                          P_Leaf_Int_Shade(K),           &
                                          LocalMet%MISOLAI, PSTD)
 
            GAMMA_T_LD_Sun = GET_GAMMA_T_LD_C( LocalMet%TS,                &
                                             LocalMet%T_LASTXDAYS,       &
                                             LocalMet%T_LAST24H,         &
                                             CT1, CEO,                   &
                                             T_Leaf_Int_Sun(K),          &
                                             T_Leaf_Temp_Sun(K) )
 
            GAMMA_T_LD_Shade = GET_GAMMA_T_LD_C(LocalMet%TS,               &
                                             LocalMet%T_LASTXDAYS,       &
                                             LocalMet%T_LAST24H,         &
                                             CT1, CEO,                   &
                                             T_Leaf_Int_Shade(K),        &
                                             T_Leaf_Temp_Shade(K) )
 
            GAMMA_T_LI_Sun =  GET_GAMMA_T_LI( LocalMet%TS, BETA,           &
                                            T_Leaf_Int_Sun(K),           &
                                            T_Leaf_Temp_Sun(K) )
 
            GAMMA_T_LI_Shade =  GET_GAMMA_T_LI( LocalMet%TS, BETA,         &
                                            T_Leaf_Int_Shade(K),         &
                                            T_Leaf_Temp_Shade(K) )
 
 
            Ea1L  =  CDEA(K) * GAMMA_PAR_Sun * GAMMA_T_LD_Sun * SunF +   &
                         GAMMA_PAR_Shade * GAMMA_T_LD_Shade * (1-SunF)
 
            Ea2L =  GAMMA_T_LI_Sun * SunF +                              &
                         GAMMA_T_LI_Shade * (1-SunF)
 
            GAMMA_TP  = GAMMA_TP +                                       &
                         (Ea1L*LDF + Ea2L*(1-LDF))* VPGWT(K)
      
         ENDDO
 
         !--------------------- gamma values related to compound ----------------------

         DO K=1, MeganState%nMeganSpecies

            CMPD = MeganState%MeganSpeciesName(K)
            ! --------------------------------------------------
            ! GAMMA_SM (soil moisture activity factor)
            ! --------------------------------------------------
            GAMMA_SM = GET_GAMMA_SM( LocalMet%GWETROOT, CMPD )

            ! --------------------------------------------------
            ! emission factor
            ! --------------------------------------------------
            select case ( TRIM(CMPD) )
               case ('ISOP')
                  AEF = MetState%AEF_ISOP
               case ('ISOP')
                  AEF = MetState%AEF_ISOP

               case default 
                  call CALC_AEF(MetState%PFT_16, CMPD, AEF, RC)
            end select
            
            ! --------------------------------------------------
            ! calculate emission
            ! --------------------------------------------------
            ! Emission is the product of all of these in kg/m2/s.
            ! Normalization factor ensures product of GAMMA values is 1.0 under
            !  standard conditions. Norm_FAC = 0.21. canopy add
            IF ( TRIM(CMPD) == 'ISOP' ) THEN
               ! Only apply CO2 inhibition to isoprene (mps, 9/15/15)
               ! Amos Tai wrote:
               !  In my opinion, we should not apply the CO2 inhibition factor on
               !  other monoterpene species yet, because the empirical data I've used
               !  are for isoprene emissions only. But we generally agree that CO2
               !  inhibition should affect monoterpenes too, so we'll leave room for
               !  future incorporation when new data arise.
            ! MEGAN_EMIS = Inst%NORM_FAC(1) * AEF * GAMMA_AGE * GAMMA_SM * &
            !              GAMMA_LAI * ((1.0_hp - LDF) * GAMMA_T_LI +      &
            !              (LDF * GAMMA_PAR * GAMMA_T_LD)) * GAMMA_CO2
               MEGAN_EMIS = LocalMet%MISOLAI * AEF * GAMMA_AGE * GAMMA_SM *  &
                              GAMMA_TP*GAMMA_CO2*GAMMA_LAI*0.21_hp
            ELSE
            ! MEGAN_EMIS = Inst%NORM_FAC(1) * AEF * GAMMA_AGE * GAMMA_SM * &
            !              GAMMA_LAI * ((1.0_hp - LDF) * GAMMA_T_LI +      &
            !              (LDF * GAMMA_PAR * GAMMA_T_LD))
               MEGAN_EMIS = LocalMet%MISOLAI * AEF * GAMMA_AGE * GAMMA_SM *  &
                           GAMMA_TP * GAMMA_LAI * 0.21_hp

            ENDIF
            
            MeganState%EmissionPerSpecies(K) = MEGAN_EMIS
         
         ENDDO

      endif

      return

   end subroutine CCPr_Scheme_Megan

end module CCPr_Scheme_Megan_Mod
